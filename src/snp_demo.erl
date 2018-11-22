-module(snp_demo).

-export([run/0]).

-import(wol_datetime, [now_utc/0,
		       add_seconds/2]).

-define(ID, <<"ENG.1">>).

sleep(Secs) ->
    timer:sleep(timer:seconds(Secs)).    

test_add(Id) ->
    io:format("--- test_add ---~n"),
    snp:add(Id, foo, bar, add_seconds(now_utc(), 1)),
    FooBar=#{foo => bar},
    FooBar=snp:items(Id),
    snp:add(Id, foo, bar, add_seconds(now_utc(), 2)),
    FooBar=snp:items(Id),    
    sleep(2),
    #{}=snp:items(Id),
    ok.

test_get(Id) ->
    io:format("--- test_get ---~n"),
    snp:add(Id, foo, bar, add_seconds(now_utc(), 1)),
    bar=snp:get(Id, foo),
    sleep(2),
    #{}=snp:items(Id),
    ok.

test_set(Id) ->
    io:format("--- test_set ---~n"),
    snp:add(Id, foo, bar, add_seconds(now_utc(), 1)),
    bar=snp:get(Id, foo),
    snp:set(Id, foo, car, add_seconds(now_utc(), 1)),
    car=snp:get(Id, foo),
    sleep(2),
    #{}=snp:items(Id),
    ok.

test_delete(Id) ->
    io:format("--- test_delete ---~n"),
    snp:add(Id, foo, bar, add_seconds(now_utc(), 1)),
    FooBar=#{foo => bar},
    FooBar=snp:items(Id),
    ok=snp:delete(Id, foo),
    #{}=snp:items(Id),
    ok.

run() ->
    CallbackFn=fun(K, V) ->  io:format("~p => ~p~n", [K, V]) end,
    snp:spawn(?ID, CallbackFn),
    sleep(1),
    ok=test_add(?ID),
    ok=test_get(?ID),
    ok=test_set(?ID),
    ok=test_delete(?ID),
    ok.
