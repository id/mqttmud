-module(mqttmud_http).

%% API:
-export([start_link/0]).

-include_lib("kernel/include/logger.hrl").

start_link() ->
    Port = application:get_env(mqttmud, http_port, 8080),
    TransportOpts = #{port => Port},
    Env = #{dispatch => dispatch()},
    ProtocolOpts = #{env => Env},
    cowboy:start_clear(?MODULE, maps:to_list(TransportOpts), ProtocolOpts).

dispatch() ->
    cowboy_router:compile([{'_', routes()}]).

routes() ->
    [
        {"/api/v1/users", mqttmud_http_users, []},
        {"/", cowboy_static, {priv_file, mqttmud, "static/index.html"}},
        {"/[...]", cowboy_static, {priv_dir, mqttmud, "static"}}
    ].
