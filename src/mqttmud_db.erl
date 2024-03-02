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
    create_player/1,
    upsert_session/2,
    delete_session/1,
    player_client_id/1,
    player_room/1,
    get_room_by_exit/1,
    room_players/1,
    room_exits/1,
    move_player_to/2,
    active_players/0,
    start_link/0,
    stop/0
]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

create_player(Name) ->
    gen_server:call(?MODULE, {create_player, Name}).

upsert_session(Username, ClientId) ->
    gen_server:call(?MODULE, {upsert_session, Username, ClientId}).

player_client_id(Username) ->
    gen_server:call(?MODULE, {player_client_id, Username}).

delete_session(Username) ->
    gen_server:call(?MODULE, {delete_session, Username}).

player_room(Player) ->
    gen_server:call(?MODULE, {player_room, Player}).

get_room_by_exit(Exit) ->
    gen_server:call(?MODULE, {get_room_by_exit, Exit}).

room_players(Player) ->
    gen_server:call(?MODULE, {room_players, Player}).

room_exits(Player) ->
    gen_server:call(?MODULE, {room_exits, Player}).

move_player_to(Player, RoomId) ->
    gen_server:call(?MODULE, {move_player_to, Player, RoomId}).

active_players() ->
    gen_server:call(?MODULE, active_players).

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
    ensure_schema(Conn),
    init_world(Conn),
    {ok, #{conn => Conn}}.

handle_call({create_player, Name}, _From, #{conn := Conn} = State) ->
    {ok, 1, _Cols, [{UserId}]} = epgsql:equery(
        Conn, "INSERT INTO players (name) VALUES ($1) RETURNING id", [Name]
    ),
    {ok, 1} = epgsql:equery(
        Conn, "INSERT INTO room_players (room_id, player_id) VALUES ('tavern', $1)", [UserId]
    ),
    {reply, ok, State};
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
handle_call({player_client_id, Username}, _From, #{conn := Conn} = State) ->
    {ok, _, [{ClientId}]} = epgsql:equery(
        Conn,
        "SELECT client_id FROM sessions WHERE player_id = (SELECT id FROM players WHERE name = $1)",
        [Username]
    ),
    {reply, ClientId, State};
handle_call({player_room, Player}, _From, #{conn := Conn} = State) ->
    case epgsql:equery(
        Conn,
        "SELECT r.id FROM rooms r JOIN room_players rp ON r.id = rp.room_id JOIN players p ON rp.player_id = p.id WHERE p.name = $1;",
        [Player]
    ) of
        {ok, _, [{RoomId}]} -> {reply, RoomId, State};
        {ok, _, []} -> {reply, undefined, State}
    end;
handle_call({get_room_by_exit, Exit}, _From, #{conn := Conn} = State) ->
    {ok, _, [Result]} = epgsql:equery(
        Conn,
        "SELECT r.id, r.name FROM exits e JOIN rooms r ON e.to_id = r.id WHERE e.name = $1;",
        [Exit]
    ),
    {reply, Result, State};
handle_call({room_players, Player}, _From, #{conn := Conn} = State) ->
    {ok, _, Result} = epgsql:equery(
        Conn,
        "SELECT p2.name "
        "FROM players p "
        "JOIN room_players rp ON p.id = rp.player_id "
        "JOIN rooms r ON rp.room_id = r.id "
        "JOIN room_players rp2 ON r.id = rp2.room_id "
        "JOIN players p2 ON rp2.player_id = p2.id "
        "JOIN sessions s ON p2.id = s.player_id "
        "WHERE p.name = $1 AND p2.name != $1;",
        [Player]
    ),
    {reply, [P || {P} <- Result], State};
handle_call({room_exits, Player}, _From, #{conn := Conn} = State) ->
    {ok, _, Result} = epgsql:equery(
        Conn,
        "SELECT e.name FROM exits e JOIN rooms r ON e.from_id = r.id JOIN room_players rp ON r.id = rp.room_id JOIN players p ON rp.player_id = p.id WHERE p.name = $1;",
        [Player]
    ),
    {reply, [E || {E} <- Result], State};
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

ensure_schema(Conn) ->
    PrivDir = code:priv_dir(mqttmud),
    {ok, SchemaSql} = file:read_file(filename:join(PrivDir, "schema.sql")),
    Results = epgsql:squery(Conn, binary_to_list(SchemaSql)),
    [{ok, _, _} = R || R <- Results].

init_world(Conn) ->
    PrivDir = code:priv_dir(mqttmud),
    {ok, WorldSql} = file:read_file(filename:join(PrivDir, "world.sql")),
    Results = epgsql:squery(Conn, binary_to_list(WorldSql)),
    [{ok, _} = R || R <- Results].
