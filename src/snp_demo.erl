-module(snp_demo).

-export([run/0]).

run() ->
    snp_sup:spawn(<<"ENG.1">>),
    ok.
