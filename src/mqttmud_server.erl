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
    check_roll/2,
    start_link/0,
    stop/0,
    send_welcome_message/1,
    send_cmd/4
]).

-define(BROKER, "mqtt://localhost:1883").
-define(CLIENT_ID, "mqttmud").

-define(int2bin(__X), (list_to_binary(integer_to_list(__X)))).

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
    #{<<"username">> := Username, <<"client_id">> := ClientId} = jsone:decode(Payload),
    logger:info("~s connected", [Username]),
    init_session(Client, Username, ClientId);
handle_messsage(Client, #{topic := <<"disconnected">>, payload := Payload}) ->
    #{<<"username">> := Username, <<"client_id">> := ClientId} = jsone:decode(Payload),
    logger:info("~s disconnected", [Username]),
    #{room_id := RoomId} = mqttmud_db:player_room(Username),
    delete_session(ClientId, Username, RoomId),
    send_notification(
        Client, <<"rooms/", RoomId/binary>>, ?DM, <<Username/binary, " has left the game.">>
    );
handle_messsage(Client, #{topic := Topic, payload := Payload}) ->
    [<<"game">>, Username] = binary:split(Topic, <<"/">>, [global]),
    Player = mqttmud_db:get_player(Username),
    case Player of
        #{alive := true} ->
            Session = mqttmud_db:get_session(Username),
            [Cmd | Rest] = binary:split(Payload, <<" ">>),
            do(string:lowercase(Cmd), iolist_to_binary(Rest), Client, Player, Session);
        _ ->
            ok
    end.

do(<<"help">>, _, Client, #{name := Username}, #{status := normal}) ->
    Message = <<
        "Commands:"
        "<ul>"
        "<li>look - examine the surroundings</li>"
        "<li>go <em>exit</em> - go through the exit specified</li>"
        "<li>say <em>message</em> - say something out loud to everyone in the room</li>"
        "<li>whisper <em>username</em> <em>message</em> - whisper something privately</li>"
        "<li>shout <em>message</em> - shout something to everyone in the game</li>"
        "<li>fight <em>monster</em> - fight a monster that you see</li>"
        "<li>help - show this help message</li>"
        "</ul>"
    >>,
    send_message(Client, <<"users/", Username/binary>>, ?DM, Message);
do(<<"help">>, _, Client, #{name := Username}, #{status := {fight, _}}) ->
    Message = <<
        "Commands:"
        "<ul>"
        "<li>roll <em>d4|d6|d8|d10|d20|d100</em> - roll a dice</li>"
        "<li>attack - attack the monster</li>"
        "<li>run - run away from the fight</li>"
        "<li>help - show this help message</li>"
        "</ul>"
    >>,
    send_message(Client, <<"users/", Username/binary>>, ?DM, Message);
do(<<"look">>, _, Client, #{room_id := RoomId, name := Username}, #{status := normal}) ->
    RoomPlayers = mqttmud_db:room_players(RoomId) -- [Username],
    RoomExits = mqttmud_db:room_exits(RoomId),
    RoomMonsters = mqttmud_db:room_monsters(RoomId),
    send_data(Client, <<"users/", Username/binary>>, ?DM, #{
        dataType => ?look,
        players => RoomPlayers,
        exits => RoomExits,
        monsters => [MonsterName || #{name := MonsterName} <- RoomMonsters]
    });
do(<<"look">>, _, Client, #{name := Username}, #{status := {fight, _}}) ->
    Msg = <<"You cannot use this command while fighting.">>,
    send_message(Client, <<"users/", Username/binary>>, ?DM, Msg);
do(<<"look">>, _, _Client, Player, Session) ->
    logger:warning("Look: ~p, ~p", [Player, Session]),
    {ok, Player, Session};
do(<<"go">>, Exit, Client, #{room_id := RoomId, name := Username}, #{status := normal} = Session) ->
    {NewRoomId, NewRoomName} = mqttmud_db:get_room_by_exit(Exit, RoomId),
    #{client_id := ClientId} = Session,
    mqttmud_db:move_player_to(Username, NewRoomId),
    mqttmud_emqx_api:unsubscribe(ClientId, <<"rooms/", RoomId/binary>>),
    send_notification(
        Client, <<"rooms/", RoomId/binary>>, ?DM, <<Username/binary, " has left the room.">>
    ),
    send_notification(
        Client, <<"rooms/", NewRoomId/binary>>, ?DM, <<Username/binary, " has entered the room.">>
    ),
    send_cmd(Client, <<"users/", Username/binary>>, ?cmd_move, NewRoomName),
    Msg = <<"You entered ", NewRoomName/binary, ".">>,
    send_message(Client, <<"users/", Username/binary>>, ?DM, Msg),
    mqttmud_emqx_api:subscribe(ClientId, <<"rooms/", NewRoomId/binary>>);
do(<<"go">>, _, Client, #{name := Username}, #{status := {fight, _}}) ->
    Msg = <<"You cannot use this command while fighting.">>,
    send_message(Client, <<"users/", Username/binary>>, ?DM, Msg);
do(<<"say">>, Whatever, Client, #{room_id := RoomId, name := Username}, _) ->
    send_say(Client, <<"rooms/", RoomId/binary>>, Username, Whatever);
do(<<"whisper">>, Whatever, Client, #{room_id := RoomId, name := Username}, _) ->
    [To, Message] = binary:split(Whatever, <<" ">>),
    #{room_id := ToRoomId} = mqttmud_db:player_room(To),
    case RoomId =:= ToRoomId of
        true ->
            send_whisper(Client, <<"users/", To/binary, "/inbox">>, Username, Message);
        false ->
            Msg = <<"You can only whisper with someone in the same room.">>,
            send_message(Client, <<"users/", Username/binary>>, ?DM, Msg)
    end;
do(<<"shout">>, Whatever, Client, #{name := Username}, _) ->
    Players = mqttmud_db:active_players(),
    lists:foreach(
        fun(Player) ->
            send_shout(Client, <<"users/", Player/binary>>, Username, Whatever)
        end,
        Players
    );
do(<<"roll">>, Dice, Client, #{name := Username} = Player, #{status := {fight, MonsterId}} = Session) ->
    #{dmg := Dmg} = Player,
    case roll_dice(Dice) of
        {ok, {1, 20, Result}} ->
            %% hit roll
            handle_hit_roll(Client, Player, Session, MonsterId, Result);
        {ok, {N, S, Result}} when Dice =:= Dmg ->
            %% attack roll
            handle_dmg_roll(Client, Player, Session, MonsterId, N, S, Result);
        Res ->
            logger:warning("Invalid roll: ~p", [Res]),
            send_message(Client, <<"users/", Username/binary, "/fight">>, ?DM, <<"Invalid roll.">>)
    end;
do(<<"roll">>, Dice, Client, #{name := Username}, #{status := normal}) ->
    case roll_dice(Dice) of
        {ok, {_, _, Result}} ->
            Msg = list_to_binary(integer_to_list(Result)),
            send_message(Client, <<"users/", Username/binary>>, ?DM, Msg);
        _ ->
            send_message(Client, <<"users/", Username/binary>>, ?DM, <<"Invalid roll.">>)
    end;
do(<<"fight">>, MonsterName, Client, #{name := Username, room_id := RoomId}, #{status := normal}) ->
    Monsters = mqttmud_db:room_monsters(RoomId),
    SearchFun = fun
        (#{name := Name}) when Name =:= MonsterName -> true;
        (_) -> false
    end,
    case lists:search(SearchFun, Monsters) of
        false ->
            Msg = <<"There is no ", MonsterName/binary, " nearby.">>,
            send_message(Client, <<"users/", Username/binary>>, ?DM, Msg);
        {value, #{id := MonsterId}} ->
            mqttmud_db:set_status(Username, {fight, MonsterId}),
            send_message(Client, <<"users/", Username/binary, "/fight">>, ?DM, <<"on">>),
            Msg1 = <<"You are fighting ", MonsterName/binary, ".">>,
            send_message(Client, <<"users/", Username/binary>>, ?DM, Msg1),
            Msg2 = <<"Roll d20 to check for a hit.">>,
            send_message(Client, <<"users/", Username/binary>>, ?DM, Msg2)
    end;
do(<<"fight">>, _, Client, #{name := Username}, #{status := {fight, _}}) ->
    Msg = <<"You are already fighting.">>,
    send_message(Client, <<"users/", Username/binary>>, ?DM, Msg);
do(<<"run">>, _, Client, #{name := Username}, #{status := normal}) ->
    Msg = <<"You can only run from monsters.">>,
    send_message(Client, <<"users/", Username/binary>>, ?DM, Msg);
do(<<"run">>, _, Client, #{name := Username}, #{status := {fight, _}}) ->
    mqttmud_db:set_status(Username, normal),
    send_message(Client, <<"users/", Username/binary>>, ?DM, <<"You cowardly ran from a fight.">>),
    send_message(Client, <<"users/", Username/binary, "/fight">>, ?DM, <<"off">>);
do(Cmd, _, Client, #{name := Username}, _) ->
    logger:warning("Unknown command: ~s", [Cmd]),
    send_message(Client, <<"users/", Username/binary>>, ?DM, <<"Unknown command.">>).

init_session(_Client, ?DM, _ClientId) ->
    ok;
init_session(Client, Username, ClientId) ->
    #{alive := Alive} = mqttmud_db:get_player(Username),
    case Alive of
        true ->
            ok = mqttmud_db:upsert_session(Username, ClientId),
            #{room_id := RoomId, room_name := RoomName} = mqttmud_db:player_room(Username),
            mqttmud_emqx_api:subscribe(ClientId, <<"users/", Username/binary>>),
            mqttmud_emqx_api:subscribe(ClientId, <<"users/", Username/binary, "/inbox">>),
            mqttmud_emqx_api:subscribe(ClientId, <<"users/", Username/binary, "/fight">>),
            Message = jsone:encode(#{
                                     type => notification,
                                     from => ?DM,
                                     message => <<Username/binary, " entered the room.">>
                                    }),
            emqtt:publish(Client, <<"rooms/", RoomId/binary>>, Message, 1),
            timer:apply_after(1000, ?MODULE, send_cmd, [Client, <<"users/", Username/binary>>, ?cmd_move, RoomName]),
            mqttmud_emqx_api:subscribe(ClientId, <<"rooms/", RoomId/binary>>);
        false ->
            mqttmud_emqx_api:subscribe(ClientId, <<"users/", Username/binary, "/inbox">>),
            Message = jsone:encode(#{
                                     type => notification,
                                     from => ?DM,
                                     message => <<"You are dead.">>
                                    }),
            emqtt:publish(Client, <<"users/", Username/binary, "/inbox">>, Message, 1)
    end.

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

roll_dice(Dice) ->
    case binary:split(string:lowercase(Dice), <<"d">>) of
        [BinN, BinS] ->
            case check_roll(BinN, BinS) of
                {ok, {N, S}} ->
                    Result = lists:sum(lists:map(fun(_) -> rand:uniform(S) end, lists:seq(1, N))),
                    {ok, {N, S, Result}};
                {error, invalid_roll} ->
                    {error, invalid_roll}
            end;
        Res ->
            logger:warning("Invalid dice: ~p", [Res]),
            {error, invalid_dice}
    end.

check_roll(<<>>, Sides) when is_binary(Sides) -> check_roll(<<"1">>, Sides);
check_roll(BinN, BinSides) when is_binary(BinN), is_binary(BinSides) ->
    try
        N = binary_to_integer(BinN),
        Sides = binary_to_integer(BinSides),
        check_roll(N, Sides)
    catch
        error:badarg ->
            logger:warning("Failed to parse integer: ~p, ~p", [BinN, BinSides]),
            {error, invalid_roll}
    end;
check_roll(N, Sides) when is_integer(N), is_integer(Sides) ->
    ValidN = N >= 1 andalso N =< 100,
    ValidSides = lists:member(Sides, [4, 6, 8, 10, 12, 20, 100]),
    case {ValidN, ValidSides} of
        {true, true} -> {ok, {N, Sides}};
        _ -> {error, invalid_roll}
    end;
check_roll(N, Sides) ->
    logger:warning("Invalid roll: ~p d~p", [N, Sides]),
    {error, invalid_roll}.

handle_hit_roll(Client, Player, Session, MonsterId, Result) ->
    Monster = mqttmud_db:get_monster(MonsterId),
    #{ac := MonsterAC} = Monster,
    #{name := Username, dmg := PlayerDMG} = Player,
    case Result >= MonsterAC of
        true ->
            Msg = <<?int2bin(Result)/binary, ". It's a hit!">>,
            send_message(Client, <<"users/", Username/binary, "/fight">>, ?DM, Msg),
            Msg2 = <<"Now roll ", PlayerDMG/binary, " for damage.">>,
            send_message(Client, <<"users/", Username/binary, "/fight">>, ?DM, Msg2);
        false ->
            Msg = <<?int2bin(Result)/binary, ". You missed! Now it's Goblin's turn.">>,
            send_message(Client, <<"users/", Username/binary, "/fight">>, ?DM, Msg),
            monster_turn(Client, Player, Session, Monster)
    end.

handle_dmg_roll(Client, Player, Session, MonsterId, _N, _S, Result) ->
    #{name := Username, room_id := RoomId, dmg_mod := PlayerDmgMod} = Player,
    Monster = mqttmud_db:get_monster(MonsterId),
    #{current_hp := HP} = Monster,
    Dmg = Result + PlayerDmgMod,
    NewHP = HP - Dmg,
    Msg = <<"You deal ", ?int2bin(Dmg)/binary, " damage to the Goblin.">>,
    mqttmud_db:update_monster(Monster#{current_hp := NewHP}),
    send_message(Client, <<"users/", Username/binary, "/fight">>, ?DM, Msg),
    case NewHP =< 0 of
        true ->
            Msg2 = <<"You have killed the Goblin!">>,
            send_message(Client, <<"users/", Username/binary, "/fight">>, ?DM, Msg2),
            Msg3 = <<Username/binary, " has killed the Goblin!">>,
            send_notification(Client, <<"rooms/", RoomId/binary>>, ?DM, Msg3),
            mqttmud_db:update_monster(Monster#{alive := false}),
            mqttmud_db:set_status(Username, normal),
            send_message(Client, <<"users/", Username/binary, "/fight">>, ?DM, <<"off">>);
        false ->
            Msg2 = <<"Goblin's turn.">>,
            send_message(Client, <<"users/", Username/binary, "/fight">>, ?DM, Msg2),
            monster_turn(Client, Player, Session, Monster)
    end.

apply_damage(Client, Player, _Session, Dmg) ->
    #{name := Username, current_hp := HP} = Player,
    NewHP = HP - Dmg,
    case NewHP =< 0 of
        true ->
            mqttmud_db:update_player(Player#{current_hp := NewHP, alive := false}),
            Msg = <<"You have been killed by the Goblin.">>,
            send_message(Client, <<"users/", Username/binary, "/fight">>, ?DM, Msg),
            send_message(Client, <<"users/", Username/binary, "/fight">>, ?DM, <<"off">>);
        false ->
            mqttmud_db:update_player(Player#{current_hp := NewHP}),
            Msg = <<"You have ", ?int2bin(NewHP)/binary, " HP left. Your turn.">>,
            send_message(Client, <<"users/", Username/binary, "/fight">>, ?DM, Msg)
    end.

monster_turn(Client, Player, Session, Monster) ->
    #{name := Username, ac := PlayerAC} = Player,
    #{atk_mod := MosnterAtkMod, dmg := MonsterDmg, dmg_mod := MonsterDmgMod} = Monster,
    case roll_dice(<<"1d20">>) of
        {ok, {1, 20, Result}} when (Result + MosnterAtkMod) >= PlayerAC ->
            Msg = <<?int2bin(Result)/binary, ". Goblin has hit you!">>,
            send_message(Client, <<"users/", Username/binary, "/fight">>, ?DM, Msg),
            {ok, {_, _, Result2}} = roll_dice(MonsterDmg),
            Dmg = Result2 + MonsterDmgMod,
            Msg3 = <<"Goblin deals ", ?int2bin(Dmg)/binary, " damage to you.">>,
            send_message(Client, <<"users/", Username/binary, "/fight">>, ?DM, Msg3),
            apply_damage(Client, Player, Session, Dmg);
        {ok, {1, 20, Result2}} ->
            Msg2 = <<?int2bin(Result2)/binary, ". Goblin missed! Your turn.">>,
            send_message(Client, <<"users/", Username/binary, "/fight">>, ?DM, Msg2)
    end.

delete_session(ClientId, Username, RoomId) ->
    mqttmud_emqx_api:unsubscribe(ClientId, <<"rooms/", RoomId/binary>>),
    mqttmud_emqx_api:unsubscribe(ClientId, <<"users/", Username/binary>>),
    mqttmud_emqx_api:unsubscribe(ClientId, <<"users/", Username/binary, "/inbox">>),
    mqttmud_emqx_api:unsubscribe(ClientId, <<"users/", Username/binary, "/fight">>),
    ok = mqttmud_db:delete_session(Username).
