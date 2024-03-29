-module(snp_cache).

-behaviour(gen_server).

%% API.

-export([start_link/2,
	 items/1,
	 add/4,
	 get/2,
	 set/4,
	 delete/2]).

%% gen_server.

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-import(snp_registry, [registry/2]).

-import(wol_datetime, [timedelta/2,
		       now_utc/0]).

-record(state, {id, callback, pids}).

%% API.

start_link(Id, Callback) ->
    gen_server:start_link({local, registry(?MODULE, Id)}, ?MODULE, [Id, Callback], []).

items(Id) ->
    gen_server:call(registry(?MODULE, Id), items).

add(Id, Key, Value, Expiry) ->
    gen_server:call(registry(?MODULE, Id), {add, Key, Value, Expiry}).

get(Id, Key) ->
    gen_server:call(registry(?MODULE, Id), {get, Key}).

set(Id, Key, Value, Expiry) ->
    gen_server:call(registry(?MODULE, Id), {set, Key, Value, Expiry}).

delete(Id, Key) ->
    gen_server:call(registry(?MODULE, Id), {delete, Key}).

%% gen_server.

init([Id, Callback]) ->
    lager:info("~s started", [Id]),
    {ok, #state{id=Id,
		callback=Callback,
		pids=#{}}}.

handle_call(items, _From, #state{pids=Pids}=State) ->
    Items=maps:from_list([{Key, snp_cache_item:value(Pid)} || {Pid, Key} <- maps:to_list(Pids)]),
    {reply, Items, State};
handle_call({add, Key, Value, Expiry}, _From, #state{id=Id, pids=Pids}=State) ->
    case lookup_pid(Key, Pids) of
	undefined ->
	    case spawn_item(Id, Key, Value, Expiry, Pids) of
		{ok, NewPids} ->
		    {reply, ok, State#state{pids=NewPids}};
		Other ->
		    {reply, Other, State}
	    end;
	_Pid ->
	    {reply, {error, <<"pid already exists">>}, State}
    end;
handle_call({get, Key}, _From, #state{pids=Pids}=State) ->
    case lookup_pid(Key, Pids) of
	undefined ->
	    {reply, undefined, State};
	Pid ->
	    Value=snp_cache_item:value(Pid),
	    {reply, Value, State}
    end;
handle_call({set, Key, Value, Expiry}, _From, #state{id=Id, pids=Pids}=State) ->
    case lookup_pid(Key, Pids) of
	undefined ->
	    case spawn_item(Id, Key, Value, Expiry, Pids) of
		{ok, NewPids} ->
		    {reply, ok, State#state{pids=NewPids}};
		Other ->
		    {reply, Other, State}
	    end;
	Pid ->
	    snp_cache_item:stop(Pid),
	    case spawn_item(Id, Key, Value, Expiry, Pids) of
		{ok, NewPids} ->
		    {reply, ok, State#state{pids=NewPids}};
		Other ->
		    {reply, Other, State}
	    end
    end;
handle_call({delete, Key}, _From, #state{pids=Pids}=State) ->
    case lookup_pid(Key, Pids) of
	undefined ->
	    {reply, {error, <<"pid not found">>}, State};
	Pid ->
	    snp_cache_item:stop(Pid),
	    {reply, ok, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% NB callback only called by DOWN not EXIT

handle_info({'DOWN', _Ref, process, Pid, _Reason}, #state{id=Id, callback=Callback, pids=Pids}=State) -> 
    case maps:is_key(Pid, Pids) of
	true -> 
	    Key=maps:get(Pid, Pids),
	    Callback(Key, Pid),
	    lager:info("~s: ~s is DOWN", [Id, Key]),
	    NewPids=maps:remove(Pid, Pids),
	    {noreply, State#state{pids=NewPids}};
	false ->
	    {noreply, State}
    end;
handle_info({'EXIT', _Ref, process, Pid, _Reason}, #state{id=Id, pids=Pids}=State) ->
    case maps:is_key(Pid, Pids) of
	true -> 
	    Key=maps:get(Pid, Pids),
	    lager:info("~s: ~s EXIT message", [Id, Key]),
	    NewPids=maps:remove(Pid, Pids),
	    {noreply, State#state{pids=NewPids}};
	false ->
	    {noreply, State}
    end; 
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal functions

lookup_pid(Key, Pids) ->    
    Keys=maps:from_list([{K, Pid} || {Pid, K} <- maps:to_list(Pids)]),
    case maps:is_key(Key, Keys) of
	true ->
	    maps:get(Key, Keys);
	false ->
	    undefined
    end.

spawn_item(Id, Key, Value, Expiry, Pids) ->
    SecsToExpiry=secs_to_expiry(Expiry),
    case SecsToExpiry > 0 of
	true ->
	    {ok, Pid}=snp_cache_item_sup:spawn(Id, Value, SecsToExpiry),
	    lager:info("~s spawning ~s:~p [~p]", [Id, Key, Value, Expiry]),
	    erlang:monitor(process, Pid),
	    NewPids=maps:put(Pid, Key, Pids),
	    {ok, NewPids};
	false ->
	    {error, <<"expiry < now">>}
    end.
    
secs_to_expiry(Secs) when is_integer(Secs) ->
    Secs;
secs_to_expiry(Expiry) ->
    timedelta(now_utc(), Expiry).


