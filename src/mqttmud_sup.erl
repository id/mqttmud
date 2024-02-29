-module(mqttmud_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 1
    },
    ChildSpecs = [
        #{
            id => mqttmud_server,
            start => {mqttmud_server, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [mqttmud_server]
        },
        #{
            id => mqttmud_emqx_api,
            start => {mqttmud_emqx_api, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [mqttmud_emqx_api]
        },
        #{
            id => mqttmud_http,
            start => {mqttmud_http, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [mqttmud_http]
        },
        #{
            id => mqttmud_db,
            start => {mqttmud_db, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [mqttmud_db]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
