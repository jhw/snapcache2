-module(snp_demo).

-export([run/0]).

-import(wol_procs, [random_id/1]).

run() ->
    snp_sup:spawn(random_id(32)),
    ok.
