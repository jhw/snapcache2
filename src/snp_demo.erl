-module(snp_demo).

-export([run/0]).

-import(wol_datetime, [now_utc/0,
		       add_seconds/2]).

-define(ID, <<"ENG.1">>).

dump(Stuff) ->
    io:format("~p~n", [Stuff]).    

list(Id) ->
    dump(snp_cache:items(Id)).

get(Id, Key) ->
    dump(snp_cache:get(Id, Key)).

delete(Id, Key) ->
    dump(snp_cache:delete(Id, Key)).

sleep(Secs) ->
    timer:sleep(timer:seconds(Secs)).    

run() ->
    snp_sup:spawn(?ID),
    list(?ID),
    %% add/expire
    snp_cache:add(?ID, <<"foo">>, <<"bar">>, add_seconds(now_utc(), 3)),
    sleep(1),
    list(?ID),    
    get(?ID, <<"foo">>),
    sleep(3),
    list(?ID),
    %% delete
    snp_cache:add(?ID, <<"foo">>, <<"bar">>, add_seconds(now_utc(), 3)),
    list(?ID),
    sleep(1),
    delete(?ID, <<"foo">>),
    list(?ID),
    %% add twice
    snp_cache:add(?ID, <<"foo">>, <<"bar">>, add_seconds(now_utc(), 3)),
    list(?ID),
    sleep(1),
    snp_cache:add(?ID, <<"foo">>, <<"bar">>, add_seconds(now_utc(), 3)),
    list(?ID),
    sleep(3),
    list(?ID),
    ok.
