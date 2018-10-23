-module(snp_sup).

-behaviour(supervisor).

-export([start_link/0,
	 spawn/1]).

-export([init/1]).

-import(snp_registry, [registry/2]).

-import(wol_procs, [init_permanent_sup/3]).

-define(SHUTDOWN_POLICY, {one_for_one, 5, 5}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

spawn(Id) ->
    Proc=init_permanent_sup(registry(snp_cache_sup, Id), snp_cache_sup, [Id]),
    supervisor:start_child(?MODULE, Proc),
    ok.

init([]) ->
    Procs=[],
    {ok, {?SHUTDOWN_POLICY, Procs}}.
