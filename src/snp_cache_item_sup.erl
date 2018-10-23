-module(snp_cache_item_sup).

-behaviour(supervisor).

-export([start_link/1,
	 spawn/3]).

-export([init/1]).

-import(snp_registry, [registry/2]).

-import(wol_procs, [init_transient_worker/3,
		    random_id/1]).

-define(SHUTDOWN_POLICY, {one_for_one, 5, 5}).

start_link(Id) ->
    supervisor:start_link({local, registry(?MODULE, Id)}, ?MODULE, [Id]).

spawn(Id, Value, Expiry) ->
    Proc=init_transient_worker(random_id(32), snp_cache_item, [Value, Expiry]),
    supervisor:start_child(registry(?MODULE, Id), Proc),
    ok.

init([Id]) ->
    lager:info("~s started", [Id]),
    Procs=[],
    {ok, {?SHUTDOWN_POLICY, Procs}}.
