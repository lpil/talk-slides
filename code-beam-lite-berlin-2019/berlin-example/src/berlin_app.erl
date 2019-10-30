-module(berlin_app).

-behaviour(application).
-behaviour(supervisor).

-export([start/2, stop/1, init/1]).

-define(SERVER, ?MODULE).

start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

stop(_State) ->
    ok.

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => berlin,
                    start => {elli, start_link, [[{callback, berlin}, {port, 8080}]]}}],
    {ok, {SupFlags, ChildSpecs}}.
