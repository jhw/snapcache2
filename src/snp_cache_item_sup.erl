-module(snp_cache_item_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-import(snp_registry, [registry/2]).

-define(SHUTDOWN_POLICY, {one_for_one, 5, 5}).

start_link(Id) ->
    supervisor:start_link({local, registry(?MODULE, Id)}, ?MODULE, []).

init([]) ->
    Procs=[],
    {ok, {?SHUTDOWN_POLICY, Procs}}.
