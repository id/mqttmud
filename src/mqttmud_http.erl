-module(mqttmud_http).

%% API:
-export([start_link/0]).

-include_lib("kernel/include/logger.hrl").

start_link() ->
    Port = application:get_env(mqttmud, http_port, 8080),
    TransportOpts = #{port => Port},
    Env = #{dispatch => dispatch()},
    ProtocolOpts = #{env => Env},
    cowboy:start_clear(rest_api, maps:to_list(TransportOpts), ProtocolOpts).

dispatch() ->
    cowboy_router:compile([{'_', routes()}]).

routes() ->
    [{"/users", mqttmud_http_users, []}].
