-module(mqttmud_http_users).

-export([
    init/2,
    init/3,
    descr/0,
    handle_request/2,
    content_types_accepted/2,
    allowed_methods/2
]).

descr() ->
    "Users.".

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {
        [
            {<<"application/json">>, handle_request}
        ],
        Req,
        State
    }.

handle_request(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_urlencoded_body(Req0),
    Headers = #{<<"content-type">> => <<"application/json">>},
    case parse_body(Body) of
        {ok, Username, Password} ->
            case mqttmud_emqx_api:create_user(Username, Password) of
                ok ->
                    Req = cowboy_req:reply(201, Headers, <<>>, Req1),
                    {true, Req, State};
                {error, already_exists} ->
                    Message = #{<<"error">> => <<"User already exists.">>},
                    Req = cowboy_req:reply(409, Headers, jsone:encode(Message), Req1),
                    {false, Req, State};
                {error, _} ->
                    Message = #{<<"error">> => <<"Failed to register user.">>},
                    Req = cowboy_req:reply(400, Headers, jsone:encode(Message), Req1),
                    {false, Req, State}
            end;
        {error, Code, Message} ->
            Req = cowboy_req:reply(Code, Headers, jsone:encode(Message), Req1),
            {false, Req, State}
    end.

parse_body([{Input, true}]) ->
    try jsone:decode(Input) of
        #{<<"username">> := Username, <<"password">> := Password} ->
            {ok, Username, Password}
    catch
        _:_ ->
            {error, 400, #{<<"error">> => <<"Invalid json.">>}}
    end;
parse_body([]) ->
    {error, 400, #{<<"error">> => <<"Missing body.">>}};
parse_body(_) ->
    {error, 400, #{<<"error">> => <<"Bad request.">>}}.
