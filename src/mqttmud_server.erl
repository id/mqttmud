-module(mqttmud_server).
-behaviour(gen_server).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
    start_link/0,
    stop/0
]).

-define(BROKER, "mqtt://localhost:1883").
-define(CLIENT_ID, "mqttmud").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

%% gen_server callbacks
init([]) ->
    Host = application:get_env(mqttmud, broker_host, "localhost"),
    Port = application:get_env(mqttmud, broker_port, 1883),
    ClientId = application:get_env(mqttmud, client_id, "mqttmud"),
    Username = application:get_env(mqttmud, mqtt_username, <<"mqttmud">>),
    Password = application:get_env(mqttmud, mqtt_password, <<"mqttmud">>),
    ClientOpts = [
        {owner, self()},
        {client_id, ClientId},
        {host, Host},
        {port, Port},
        {username, Username},
        {password, Password}
    ],
    {ok, Client} = emqtt:start_link(ClientOpts),
    {ok, _Props} = emqtt:connect(Client),
    pong = emqtt:ping(Client),
    SubOpts = [{qos, 1}],
    {ok, _, _} =
        emqtt:subscribe(
            Client,
            #{},
            [
                {<<"game/#">>, SubOpts},
                {<<"connected">>, SubOpts},
                {<<"disconnected">>, SubOpts}
            ]
        ),
    {ok, #{client => Client}}.

handle_call(stop, _From, #{client := Client}) ->
    emqtt:disconnect(Client),
    emqtt:stop(Client),
    {stop, normal, ok, #{}};
handle_call(_Request, _From, State) ->
    {reply, not_implemented, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({publish, Msg}, #{client := Client} = State) ->
    handle_messsage(Client, Msg),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal functions
handle_messsage(Client, #{topic := <<"connected">>, payload := Payload}) ->
    logger:info("Connected: ~p", [Payload]),
    #{<<"username">> := Username, <<"client_id">> := ClientId} = jsone:decode(Payload),
    ok = mqttmud_db:upsert_session(Username, ClientId),
    Room = mqttmud_db:player_room(Username),
    Message = jsone:encode(#{
        <<"type">> => <<"notification">>,
        <<"from">> => <<"system">>,
        <<"message">> => <<Username/binary, " entered the room.">>
    }),
    emqtt:publish(Client, <<"rooms/", Room/binary>>, Message, 1),
    mqttmud_emqx_api:subscribe(ClientId, <<"users/", Username/binary>>),
    mqttmud_emqx_api:subscribe(ClientId, <<"rooms/", Room/binary>>);
handle_messsage(Client, #{topic := <<"disconnected">>, payload := Payload}) ->
    logger:info("Disconnected: ~p", [Payload]),
    #{<<"username">> := Username, <<"client_id">> := _ClientId} = jsone:decode(Payload),
    Room = mqttmud_db:player_room(Username),
    ok = mqttmud_db:delete_session(Username),
    Message = jsone:encode(#{
        <<"type">> => <<"notification">>,
        <<"from">> => <<"system">>,
        <<"message">> => <<Username/binary, " has left the game.">>
    }),
    emqtt:publish(Client, <<"rooms/", Room/binary>>, Message, 1);
handle_messsage(Client, #{topic := Topic, payload := Payload}) ->
    [<<"game">>, Username] = binary:split(Topic, <<"/">>, [global]),
    do(Payload, Client, Username).

do(<<"help">>, Client, Username) ->
    Message = jsone:encode(#{
        <<"type">> => <<"message">>,
        <<"from">> => <<"system">>,
        <<"message">> => <<"You can use 'look', 'go <exit>', and 'say <something>'.">>
    }),
    emqtt:publish(Client, <<"users/", Username/binary>>, Message, 1);
do(<<"look">>, Client, Username) ->
    RoomPlayers = mqttmud_db:room_players(Username),
    RoomExits = mqttmud_db:room_exits(Username),
    Message = jsone:encode(#{
        <<"type">> => <<"look">>,
        <<"from">> => <<"system">>,
        <<"players">> => RoomPlayers,
        <<"exits">> => RoomExits
    }),
    emqtt:publish(Client, <<"users/", Username/binary>>, Message, 1);
do(<<"go ", Exit/binary>>, Client, Username) ->
    OldRoomId = mqttmud_db:player_room(Username),
    {NewRoomId, NewRoomName} = mqttmud_db:get_room_by_exit(Exit),
    ClientId = mqttmud_db:player_client_id(Username),
    mqttmud_db:move_player_to(Username, NewRoomId),
    mqttmud_emqx_api:unsubscribe(ClientId, <<"rooms/", OldRoomId/binary>>),
    Message1 = jsone:encode(#{
        <<"type">> => <<"notification">>,
        <<"from">> => <<"system">>,
        <<"message">> => <<Username/binary, " has left the room.">>
    }),
    emqtt:publish(Client, <<"rooms/", OldRoomId/binary>>, Message1, 1),

    Message2 = jsone:encode(#{
        <<"type">> => <<"notification">>,
        <<"from">> => <<"system">>,
        <<"message">> => <<Username/binary, " has entered the room.">>
    }),
    emqtt:publish(Client, <<"rooms/", NewRoomId/binary>>, Message2, 1),
    Message3 = jsone:encode(#{
        <<"type">> => <<"move">>,
        <<"from">> => <<"system">>,
        <<"message">> => NewRoomName
    }),
    emqtt:publish(Client, <<"users/", Username/binary>>, Message3, 1),
    mqttmud_emqx_api:subscribe(ClientId, <<"rooms/", NewRoomId/binary>>);
do(<<"say ", Whatever/binary>>, Client, Username) ->
    Room = mqttmud_db:player_room(Username),
    Message = jsone:encode(#{
        <<"type">> => <<"voice">>,
        <<"from">> => Username,
        <<"message">> => Whatever
    }),
    emqtt:publish(Client, <<"rooms/", Room/binary>>, Message, 1);
do(Bin, Client, Username) ->
    logger:warning("Unknown command: ~p", [Bin]),
    Message = jsone:encode(#{
        <<"type">> => <<"message">>,
        <<"from">> => <<"system">>,
        <<"message">> => <<"What was that?">>
    }),
    emqtt:publish(Client, <<"users/", Username/binary>>, Message, 1).
