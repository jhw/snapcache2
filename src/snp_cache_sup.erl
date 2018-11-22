-module(snp_cache_sup).

-behaviour(supervisor).

-export([start_link/2]).

-export([init/1]).

-import(snp_registry, [registry/2]).

-import(wol_procs, [init_permanent_worker/3,
		    init_permanent_sup/3]).

-define(SHUTDOWN_POLICY, {one_for_one, 5, 5}).

start_link(Id, Callback) ->
    supervisor:start_link({local, registry(?MODULE, Id)}, ?MODULE, [Id, Callback]).

init([Id, Callback]) ->
    Procs=[init_permanent_worker(registry(snp_cache, Id), snp_cache, [Id, Callback]),
	   init_permanent_sup(registry(snp_cache_item_sup, Id), snp_cache_item_sup, [Id])],
    {ok, {?SHUTDOWN_POLICY, Procs}}.
