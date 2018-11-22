-module(snp).

-compile({no_auto_import,[spawn/2]}).

-export([start/0,	 
	 spawn/1,
	 spawn/2,
	 items/1,
	 add/4,
	 get/2,
	 set/4,
	 delete/2]).

start() ->
    application:ensure_all_started(?MODULE).

spawn(Id) ->
    spawn(Id, fun(_K, _V) -> void end).
			    
spawn(Id, Callback) ->
    snp_sup:spawn(Id, Callback).

items(Id) ->
    snp_cache:items(Id).

add(Id, Key, Value, Expiry) ->
    snp_cache:add(Id, Key, Value, Expiry).

get(Id, Key) ->
    snp_cache:get(Id, Key).  

set(Id, Key, Value, Expiry) ->
    snp_cache:set(Id, Key, Value, Expiry).

delete(Id, Key) ->
    snp_cache:delete(Id, Key).

