-module(snp_cache_item_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SHUTDOWN_POLICY, {one_for_one, 5, 5}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs=[],
    {ok, {?SHUTDOWN_POLICY, Procs}}.
