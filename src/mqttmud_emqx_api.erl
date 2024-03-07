-module(mqttmud_emqx_api).
-behavior(gen_server).

-export([
    start_link/0,
    create_user/2,
    subscribe/2,
    unsubscribe/2,
    kick/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_user(Username, Password) ->
    gen_server:call(?MODULE, {create_user, Username, Password}).

subscribe(ClientId, Topic) ->
    gen_server:cast(?MODULE, {subscribe, ClientId, Topic}).

unsubscribe(ClientId, Topic) ->
    gen_server:cast(?MODULE, {unsubscribe, ClientId, Topic}).

kick(ClientId) ->
    gen_server:cast(?MODULE, {kick, ClientId}).

%% gen_server callbacks
init([]) ->
    EmqxAPI = application:get_env(mqttmud, emqx_api_url, "http://localhost:18083"),
    APIKey = application:get_env(mqttmud, emqx_api_key, "mqttmud"),
    APISecret = application:get_env(mqttmud, emqx_api_secret, "secret"),
    Headers = [
        {<<"Content-Type">>, <<"application/json">>},
        {<<"Accept">>, <<"application/json">>}
    ],
    Opts = [{basic_auth, {APIKey, APISecret}}],
    {ok, #{api_url => EmqxAPI, headers => Headers, opts => Opts}}.

handle_call({create_user, Username, Password}, _From, State) ->
    ApiURL = maps:get(api_url, State),
    EmqxUsersAPI = iolist_to_binary([
        ApiURL, "/api/v5/authentication/password_based:built_in_database/users"
    ]),
    Headers = maps:get(headers, State),
    Opts = maps:get(opts, State),
    Payload = jsone:encode(#{user_id => Username, password => Password}),
    case hackney:request(post, EmqxUsersAPI, Headers, Payload, Opts) of
        {ok, 201, _, _} ->
            mqttmud_db:create_player(Username),
            mqttmud_server:send_welcome_message(Username),
            {reply, ok, State};
        {ok, 409, _RespHeaders, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            Response = jsone:decode(Body),
            logger:warning("Failed to register user ~p: ~p", [Username, Response]),
            case Response of
                #{<<"code">> := <<"ALREADY_EXISTS">>} ->
                    {reply, {error, already_exists}, State};
                _ ->
                    {reply, {error, {409, Response}}, State}
            end;
        {ok, StatusCode, RespHeaders, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            logger:warning("Failed to register user ~p: ~p", [Username, jsone:decode(Body)]),
            {reply, {error, {StatusCode, RespHeaders, jsone:decode(Body)}}, State};
        Other ->
            logger:warning("Failed to register user ~p: ~p", [Username, Other]),
            {reply, {error, Other}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, not_implemented, State}.

handle_cast({subscribe, ClientId, Topic}, State) ->
    ApiURL = maps:get(api_url, State),
    SubscribeAPI = iolist_to_binary([
        ApiURL, "/api/v5/clients/", ClientId, "/subscribe"
    ]),
    Headers = maps:get(headers, State),
    Opts = maps:get(opts, State),
    Payload = jsone:encode(#{topic => Topic, qos => 1}),
    case hackney:request(post, SubscribeAPI, Headers, Payload, Opts) of
        {ok, 200, _, _} ->
            {noreply, State};
        {ok, 401, _, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            logger:warning("Failed to subscribe client ~p to ~p: ~p", [ClientId, Topic, Body]),
            {noreply, State};
        Other ->
            logger:error("Failed to subscribe client ~p to ~p: ~p", [ClientId, Topic, Other]),
            {noreply, State}
    end;
handle_cast({unsubscribe, ClientId, Topic}, State) ->
    ApiURL = maps:get(api_url, State),
    UnsubscribeAPI = iolist_to_binary([
        ApiURL, "/api/v5/clients/", ClientId, "/unsubscribe"
    ]),
    Headers = maps:get(headers, State),
    Opts = maps:get(opts, State),
    Payload = jsone:encode(#{topic => Topic}),
    case hackney:request(post, UnsubscribeAPI, Headers, Payload, Opts) of
        {ok, 204, _, _} ->
            {noreply, State};
        {ok, _StatusCode, _RespHeaders, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            JsonBody = jsone:decode(Body),
            logger:warning("Failed to unsubscribe client ~p from ~p: ~p", [ClientId, Topic, JsonBody]),
            {noreply, State};
        Other ->
            logger:warning("Failed to unsubscribe client ~p from ~p: ~p", [ClientId, Topic, Other]),
            {noreply, State}
    end;
handle_cast({kick, ClientId}, State) ->
    ApiURL = maps:get(api_url, State),
    KickAPI = iolist_to_binary([
        ApiURL, "/api/v5/clients/", ClientId
    ]),
    Headers = maps:get(headers, State),
    Opts = maps:get(opts, State),
    case hackney:request(delete, KickAPI, Headers, <<>>, Opts) of
        {ok, 204, _, _} ->
            {noreply, State};
        {ok, _StatusCode, _RespHeaders, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            JsonBody = jsone:decode(Body),
            logger:warning("Failed to kick client ~p: ~p", [ClientId, JsonBody]),
            {noreply, State};
        Other ->
            logger:warning("Failed to kick client ~p: ~p", [ClientId, Other]),
            {noreply, State}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
