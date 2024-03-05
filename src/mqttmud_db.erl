-module(mqttmud_db).

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
    apply_migrations/0,
    create_player/1,
    get_player/1,
    get_session/1,
    upsert_session/2,
    delete_session/1,
    set_status/2,
    player_client_id/1,
    player_room/1,
    get_room_by_exit/2,
    room_players/1,
    room_exits/1,
    room_monsters/1,
    move_player_to/2,
    active_players/0,
    update_player/1,
    get_monster/1,
    update_monster/1,
    start_link/0,
    stop/0
]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

apply_migrations() ->
    gen_server:call(?MODULE, apply_migrations).

create_player(Name) ->
    gen_server:call(?MODULE, {create_player, Name}).

get_player(Name) ->
    gen_server:call(?MODULE, {get_player, Name}).

get_session(Username) ->
    gen_server:call(?MODULE, {get_session, Username}).

upsert_session(Username, ClientId) ->
    gen_server:call(?MODULE, {upsert_session, Username, ClientId}).

player_client_id(Username) ->
    gen_server:call(?MODULE, {player_client_id, Username}).

delete_session(Username) ->
    gen_server:call(?MODULE, {delete_session, Username}).

set_status(Username, Status) ->
    gen_server:call(?MODULE, {set_status, Username, Status}).

player_room(Player) ->
    gen_server:call(?MODULE, {player_room, Player}).

get_room_by_exit(Exit, OldRoomId) ->
    gen_server:call(?MODULE, {get_room_by_exit, Exit, OldRoomId}).

room_players(RoomId) ->
    gen_server:call(?MODULE, {room_players, RoomId}).

room_exits(RoomId) ->
    gen_server:call(?MODULE, {room_exits, RoomId}).

room_monsters(RoomId) ->
    gen_server:call(?MODULE, {room_monsters, RoomId}).

move_player_to(Player, RoomId) ->
    gen_server:call(?MODULE, {move_player_to, Player, RoomId}).

active_players() ->
    gen_server:call(?MODULE, active_players).

update_player(Player) ->
    gen_server:call(?MODULE, {update_player, Player}).

get_monster(Id) ->
    gen_server:call(?MODULE, {get_monster, Id}).

update_monster(Monster) ->
    gen_server:call(?MODULE, {update_monster, Monster}).

%% gen_server callbacks
init([]) ->
    Host = application:get_env(mqttmud, postgres_host, "localhost"),
    Port = application:get_env(mqttmud, postgres_port, 5432),
    Username = application:get_env(mqttmud, postgres_username, "mqttmud"),
    Password = application:get_env(mqttmud, postgres_password, "mqttmud"),
    Database = application:get_env(mqttmud, postgres_database, "mqttmud"),
    ConnectOpts = #{
        host => Host,
        port => Port,
        username => Username,
        password => Password,
        database => Database,
        codecs => [{epgsql_codec_json, jsone}]
    },
    {ok, Conn} = epgsql:connect(ConnectOpts),
    apply_migrations(Conn),
    {ok, #{conn => Conn}}.

handle_call(apply_migrations, _From, #{conn := Conn} = State) ->
    apply_migrations(Conn),
    {reply, ok, State};
handle_call({create_player, Name}, _From, #{conn := Conn} = State) ->
    {ok, 1, _Cols, [{UserId}]} = epgsql:equery(
        Conn, "INSERT INTO players (name) VALUES ($1) RETURNING id", [Name]
    ),
    {ok, 1} = epgsql:equery(
        Conn, "INSERT INTO room_players (room_id, player_id) VALUES ('tavern', $1)", [UserId]
    ),
    {reply, ok, State};
handle_call({get_player, Name}, _From, #{conn := Conn} = State) ->
    {ok, _, [{RoomId, Name, HP, CurrentHP, Alive, AC, Dmg, DmgMod, Level}]} = epgsql:equery(
        Conn,
        "SELECT r.id,p.name,p.hp,p.current_hp,p.alive,p.ac,p.dmg,p.dmg_mod,p.level FROM rooms r JOIN room_players rp ON r.id = rp.room_id JOIN players p ON rp.player_id = p.id WHERE p.name = $1",
        [Name]
    ),
    {reply,
        #{
            room_id => RoomId,
            name => Name,
            hp => HP,
            current_hp => CurrentHP,
            alive => Alive,
            ac => AC,
            dmg => Dmg,
            dmg_mod => DmgMod,
            level => Level
        },
        State};
handle_call({get_session, Username}, _From, #{conn := Conn} = State) ->
    {ok, _, [{ClientId, Status0, MonsterId}]} = epgsql:equery(
        Conn,
        "SELECT client_id, status, monster_id FROM sessions WHERE player_id = (SELECT id FROM players WHERE name = $1)",
        [Username]
    ),
    Status = case Status0 of
        <<"fight">> -> {fight, MonsterId};
        <<"normal">> -> normal
    end,
    {reply, #{client_id => ClientId, status => Status, monster_id => MonsterId}, State};
handle_call({upsert_session, Username, ClientId}, _From, #{conn := Conn} = State) ->
    {ok, 1} = epgsql:equery(
        Conn,
        "INSERT INTO sessions (player_id, client_id) VALUES ((SELECT id FROM players WHERE name = $1), $2) ON CONFLICT (player_id) DO UPDATE SET client_id = $2",
        [Username, ClientId]
    ),
    {reply, ok, State};
handle_call({delete_session, Username}, _From, #{conn := Conn} = State) ->
    {ok, _} = epgsql:equery(
        Conn,
        "DELETE FROM sessions WHERE player_id = (SELECT id FROM players WHERE name = $1)",
        [Username]
    ),
    {reply, ok, State};
handle_call({set_status, Username, {fight, MonsterId}}, _From, #{conn := Conn} = State) ->
    {ok, 1} = epgsql:equery(
        Conn,
        "UPDATE sessions SET status = 'fight', monster_id = $1 WHERE player_id = (SELECT id FROM players WHERE name = $2)",
        [MonsterId, Username]
    ),
    {reply, ok, State};
handle_call({set_status, Username, normal}, _From, #{conn := Conn} = State) ->
    {ok, 1} = epgsql:equery(
        Conn,
        "UPDATE sessions SET status = 'normal', monster_id = null WHERE player_id = (SELECT id FROM players WHERE name = $1)",
        [Username]
    ),
    {reply, ok, State};
handle_call({player_client_id, Username}, _From, #{conn := Conn} = State) ->
    {ok, _, [{ClientId}]} = epgsql:equery(
        Conn,
        "SELECT client_id FROM sessions WHERE player_id = (SELECT id FROM players WHERE name = $1)",
        [Username]
    ),
    {reply, ClientId, State};
handle_call({player_room, Player}, _From, #{conn := Conn} = State) ->
    case
        epgsql:equery(
            Conn,
            "SELECT r.id, r.name FROM rooms r JOIN room_players rp ON r.id = rp.room_id JOIN players p ON rp.player_id = p.id WHERE p.name = $1;",
            [Player]
        )
    of
        {ok, _, [{RoomId, RoomName}]} -> {reply, #{room_id => RoomId, room_name => RoomName}, State};
        {ok, _, []} -> {reply, undefined, State}
    end;
handle_call({get_room_by_exit, Exit, OldRoomId}, _From, #{conn := Conn} = State) ->
    {ok, _, [Result]} = epgsql:equery(
        Conn,
        "SELECT to_id, name FROM exits WHERE from_id = $1 AND name ILIKE $2;",
        [OldRoomId, <<Exit/binary, "%">>]
    ),
    {reply, Result, State};
handle_call({room_players, RoomId}, _From, #{conn := Conn} = State) ->
    {ok, _, Result} = epgsql:equery(
        Conn,
        "SELECT p.name FROM players p "
        "JOIN room_players rp ON p.id = rp.player_id "
        "JOIN sessions s ON p.id = s.player_id "
        "WHERE rp.room_id = $1;",
        [RoomId]
    ),
    {reply, [P || {P} <- Result], State};
handle_call({room_exits, RoomId}, _From, #{conn := Conn} = State) ->
    {ok, _, Result} = epgsql:equery(
        Conn,
        "SELECT e.name FROM exits e JOIN rooms r ON e.from_id = r.id WHERE r.id = $1;",
        [RoomId]
    ),
    {reply, [E || {E} <- Result], State};
handle_call({room_monsters, RoomId}, _From, #{conn := Conn} = State) ->
    {ok, _, Result} = epgsql:equery(
        Conn,
        "SELECT id, name, hp, ac, xp, atk_mod, dmg, dmg_mod, current_hp, alive, respawn_interval_seconds FROM monsters WHERE room_id = $1 and alive is true;",
        [RoomId]
    ),
    {reply,
        [
            #{
                id => Id,
                name => Name,
                hp => HP,
                ac => AC,
                xp => XP,
                atk_mod => AttackMod,
                dmg => Dmg,
                dmg_mod => DmgMod,
                current_hp => CurrentHP,
                alive => Alive,
                respawn_interval_seconds => RespawnInterval
            }
         || {Id, Name, HP, AC, XP, AttackMod, Dmg, DmgMod, CurrentHP, Alive, RespawnInterval} <-
                Result
        ],
        State};
handle_call({get_monster, Id}, _From, #{conn := Conn} = State) ->
    {ok, _, [{Name, HP, AC, XP, AttackMod, Dmg, DmgMod, CurrentHP, Alive, RespawnInterval}]} = epgsql:equery(
        Conn,
        "SELECT name, hp, ac, xp, atk_mod, dmg, dmg_mod, current_hp, alive, respawn_interval_seconds FROM monsters WHERE id = $1;",
        [Id]
    ),
    {reply,
        #{
            id => Id,
            name => Name,
            hp => HP,
            ac => AC,
            xp => XP,
            atk_mod => AttackMod,
            dmg => Dmg,
            dmg_mod => DmgMod,
            current_hp => CurrentHP,
            alive => Alive,
            respawn_interval_seconds => RespawnInterval
        },
        State};
handle_call({update_monster, Monster}, _From, #{conn := Conn} = State) ->
    #{id := Id, current_hp := CurrentHP, alive := Alive} = Monster,
    {ok, 1} = epgsql:equery(
        Conn,
        "UPDATE monsters SET current_hp = $1, alive = $2 WHERE id = $3;",
        [CurrentHP, Alive, Id]
    ),
    {reply, ok, State};
handle_call({move_player_to, Player, RoomId}, _From, #{conn := Conn} = State) ->
    {ok, 1} = epgsql:equery(
        Conn,
        "UPDATE room_players SET room_id = $1 WHERE player_id = (SELECT id FROM players WHERE name = $2);",
        [RoomId, Player]
    ),
    {reply, ok, State};
handle_call(active_players, _From, #{conn := Conn} = State) ->
    {ok, _, Result} = epgsql:equery(
        Conn,
        "SELECT p.name FROM players p JOIN sessions s ON p.id = s.player_id;",
        []
    ),
    {reply, [P || {P} <- Result], State};
handle_call({update_player, #{name := Name, current_hp := HP, alive := Alive}},
            _From, #{conn := Conn} = State) ->
    {ok, 1} = epgsql:equery(
        Conn,
        "UPDATE players SET current_hp = $1, alive = $2 WHERE name = $3;",
        [HP, Alive, Name]
    ),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, not_implemented, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #{conn := C}) ->
    epgsql:close(C),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------

apply_migrations(Conn) ->
    Res = epgsql:equery(Conn, "SELECT schema_version FROM game ORDER BY schema_version DESC LIMIT 1", []),
    CurrentVersion = case Res of
                         {ok, _, [{V}]} -> V;
                         {error, _} -> 0
                     end,
    logger:info("Current schema version: ~p", [CurrentVersion]),
    PrivDir = code:priv_dir(mqttmud),
    Migrations0 = filelib:wildcard(filename:join([PrivDir, "migrations", "*.sql"])),
    Migrations1 = lists:sort(
                    fun(A, B) -> filename:basename(A) < filename:basename(B) end, Migrations0
                   ),
    Migrations = lists:map(
                   fun(F) ->
                           [Version, _] = string:split(filename:basename(F), "_"),
                           {list_to_integer(Version), F}
                   end,
                   Migrations1
                  ),
    F = fun(Migration) -> apply_migration(Conn, CurrentVersion, Migration) end,
    lists:foreach(F, Migrations).

apply_migration(Conn, CurrentVersion, {Version, File}) when CurrentVersion < Version ->
    {ok, Sql} = file:read_file(File),
    ok = epgsql:with_transaction(
        Conn,
        fun(C) ->
            Res = epgsql:squery(C, binary_to_list(Sql)),
            ok = to_simple_res(Res),
            ok
        end,
        #{reraise => true}
    );
apply_migration(_, _, _) ->
    ok.

to_simple_res([]) -> ok;
to_simple_res([{ok, _} | Rest]) -> to_simple_res(Rest);
to_simple_res([{ok, _, _} | Rest]) -> to_simple_res(Rest);
to_simple_res([{error, Error} | _]) -> {error, Error};
to_simple_res({ok, _}) -> ok;
to_simple_res({ok, _, _}) -> ok;
to_simple_res({error, _}) -> error.
