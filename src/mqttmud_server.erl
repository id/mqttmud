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
    stop/0,
    send_welcome_message/1
]).

-define(BROKER, "mqtt://localhost:1883").
-define(CLIENT_ID, "mqttmud").

-define(DM, <<"DM">>).

-define(notification, notification).
-define(message, message).
-define(cmd, command).
-define(cmd_move, move).
-define(data, data).
-define(look, look).
-define(voice, voice).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

send_welcome_message(Username) ->
    gen_server:cast(?MODULE, {send_welcome_message, Username}).

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

handle_cast({send_welcome_message, Username}, #{client := Client} = State) ->
    Message = jsone:encode(#{
        type => message,
        from => ?DM,
        message =>
            <<"Welcome to the game, ", Username/binary,
                "! Type 'help' for a list of commands. Have fun!">>
    }),
    emqtt:publish(Client, <<"users/", Username/binary>>, Message, [{qos, 1}, {retain, true}]),
    {noreply, State};
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
    init_session(Client, Username, ClientId);
handle_messsage(Client, #{topic := <<"disconnected">>, payload := Payload}) ->
    logger:info("Disconnected: ~p", [Payload]),
    #{<<"username">> := Username, <<"client_id">> := ClientId} = jsone:decode(Payload),
    RoomId = mqttmud_db:player_room(Username),
    mqttmud_emqx_api:unsubscribe(ClientId, <<"rooms/", RoomId/binary>>),
    ok = mqttmud_db:delete_session(Username),
    send_notification(
        Client, <<"rooms/", RoomId/binary>>, ?DM, <<Username/binary, " has left the game.">>
    );
handle_messsage(Client, #{topic := Topic, payload := Payload}) ->
    [<<"game">>, Username] = binary:split(Topic, <<"/">>, [global]),
    do(Payload, Client, Username).

do(<<"help">>, Client, Username) ->
    Message = <<
        "Commands:"
        "<ul>"
        "<li>look           - examine the surroundings, e.g. 'look'</li>"
        "<li>go <em>exit</em>      - move through the exit specified, e.g. 'go outside'</li>"
        "<li>say <em>message</em>  - say something out loud to everyone in the room, e.g. 'say Hello'</li>"
        "<li>whisper <em>username</em> <em>message</em> - whisper something to someone privately, e.g. 'whisper MyFriend a secret'. You both need to be in the same room.</li>"
        "<li>shout <em>message</em>  - shout something to everyone, e.g. 'shout do you hear me?'</li>"
        "<li>help           - show this help message</li>"
    "</ul>"
    >>,
    send_message(Client, <<"users/", Username/binary>>, ?DM, Message);
do(<<"look">>, Client, Username) ->
    RoomPlayers = mqttmud_db:room_players(Username),
    RoomExits = mqttmud_db:room_exits(Username),
    send_data(Client, <<"users/", Username/binary>>, ?DM, #{
        dataType => ?look,
        players => RoomPlayers,
        exits => RoomExits
    });
do(<<"go ", Exit/binary>>, Client, Username) ->
    OldRoomId = mqttmud_db:player_room(Username),
    {NewRoomId, NewRoomName} = mqttmud_db:get_room_by_exit(Exit),
    ClientId = mqttmud_db:player_client_id(Username),
    mqttmud_db:move_player_to(Username, NewRoomId),
    mqttmud_emqx_api:unsubscribe(ClientId, <<"rooms/", OldRoomId/binary>>),
    send_notification(
        Client, <<"rooms/", OldRoomId/binary>>, ?DM, <<Username/binary, " has left the room.">>
    ),
    send_notification(
        Client, <<"rooms/", NewRoomId/binary>>, ?DM, <<Username/binary, " has entered the room.">>
    ),
    send_cmd(Client, <<"users/", Username/binary>>, ?cmd_move, NewRoomName),
    mqttmud_emqx_api:subscribe(ClientId, <<"rooms/", NewRoomId/binary>>);
do(<<"say ", Whatever/binary>>, Client, Username) ->
    Room = mqttmud_db:player_room(Username),
    send_say(Client, <<"rooms/", Room/binary>>, Username, Whatever);
do(<<"whisper ", Whatever/binary>>, Client, Username) ->
    [To, Message] = binary:split(Whatever, <<" ">>),
    FromRoom = mqttmud_db:player_room(Username),
    ToRoom = mqttmud_db:player_room(To),
    case FromRoom =:= ToRoom of
        true ->
            send_whisper(Client, <<"users/", To/binary, "/inbox">>, Username, Message);
        false ->
            send_message(
                Client,
                <<"users/", Username/binary>>,
                ?DM,
                <<"You can't whisper to someone in another room.">>
            )
    end;
do(<<"shout ", Whatever/binary>>, Client, Username) ->
    Players = mqttmud_db:active_players(),
    lists:foreach(
        fun(Player) ->
            send_shout(Client, <<"users/", Player/binary>>, Username, Whatever)
        end,
        Players
    );
do(Bin, Client, Username) ->
    logger:warning("Unknown command: ~p", [Bin]),
    send_message(Client, <<"users/", Username/binary>>, ?DM, <<"Unknown command.">>).

init_session(_Client, ?DM, _ClientId) ->
    ok;
init_session(Client, Username, ClientId) ->
    ok = mqttmud_db:upsert_session(Username, ClientId),
    Room = mqttmud_db:player_room(Username),
    Message = jsone:encode(#{
        type => notification,
        from => ?DM,
        message => <<Username/binary, " entered the room.">>
    }),
    emqtt:publish(Client, <<"rooms/", Room/binary>>, Message, 1),
    mqttmud_emqx_api:subscribe(ClientId, <<"users/", Username/binary>>),
    mqttmud_emqx_api:subscribe(ClientId, <<"users/", Username/binary, "/inbox">>),
    mqttmud_emqx_api:subscribe(ClientId, <<"rooms/", Room/binary>>).

send_notification(Client, Topic, From, Message) ->
    publish(Client, Topic, From, ?notification, Message).

send_message(Client, Topic, From, Message) ->
    publish(Client, Topic, From, ?message, Message).

send_cmd(Client, Topic, CmdType, Message) ->
    publish(Client, Topic, ?DM, ?cmd, #{command => CmdType, message => Message}).

send_data(Client, Topic, From, Message) ->
    publish(Client, Topic, From, ?data, Message).

send_say(Client, Topic, From, Message) ->
    publish(Client, Topic, From, ?voice, #{voiceType => say, message => Message}).

send_whisper(Client, Topic, From, Message) ->
    publish(Client, Topic, From, ?voice, #{voiceType => whisper, message => Message}).

send_shout(Client, Topic, From, Message) ->
    publish(Client, Topic, From, ?voice, #{voiceType => shout, message => Message}).

publish(Client, Topic, From, Type, Message) ->
    Payload = jsone:encode(#{type => Type, from => From, message => Message}),
    emqtt:publish(Client, Topic, Payload, 1).
