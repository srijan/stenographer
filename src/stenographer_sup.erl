-module(stenographer_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ChildSpec = case application:get_env(stenographer, protocol, udp) of
                     udp ->
                         ?CHILD(stenographer_udp, worker);
                     tcp ->
                         ?CHILD(stenographer_tcp, worker)
                 end,
    SupFlags = {one_for_one, 5000, 1},
    {ok, { SupFlags, [ChildSpec]} }.
