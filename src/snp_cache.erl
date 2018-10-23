-module(snp_cache).

-behaviour(gen_server).

%% API.

-export([start_link/1,
	 items/1,
	 add/3,
	 get/2,
	 set/3,
	 delete/2]).

%% gen_server.

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-import(snp_registry, [registry/2]).

-record(state, {id, pids}).

%% API.

start_link(Id) ->
    gen_server:start_link({local, registry(?MODULE, Id)}, ?MODULE, [Id], []).

items(Id) ->
    gen_server:call(registry(?MODULE, Id), items).

add(Id, Key, Value) ->
    gen_server:call(registry(?MODULE, Id), {add, Key, Value}).

get(Id, Key) ->
    gen_server:call(registry(?MODULE, Id), {get, Key}).

set(Id, Key, Value) ->
    gen_server:call(registry(?MODULE, Id), {set, Key, Value}).

delete(Id, Key) ->
    gen_server:call(registry(?MODULE, Id), {delete, Key}).

%% gen_server.

init([Id]) ->
    lager:info("~s started", [Id]),
    {ok, #state{id=Id,
		pids=#{}}}.

handle_call(items, _From, #state{pids=Pids}=State) ->
    Items=maps:from_list([{Key, snp_cache_item:value(Pid)} || {Pid, Key} <- maps:to_list(Pids)]),
    {reply, Items, State};
handle_call({add, Key, Value}, _From, #state{id=Id, pids=Pids}=State) ->
    Keys=maps:from_list([{K, Pid} || {Pid, K} <- Pids]),
    case maps:is_key(Key, Keys) of
	true ->
	    {reply, {error, <<"pid already exists">>}, State};
	false ->
	    {ok, Pid}=snp_cache_item_sup:spawn(Id, Key, Value),
	    erlang:monitor(process, Pid),
	    NewPids=maps:put(Pid, Key, Pids),
	    {reply, ok, State#state{pids=NewPids}}
    end;
handle_call({get, Key}, _From, #state{pids=Pids}=State) ->
    Keys=maps:from_list([{K, Pid} || {Pid, K} <- Pids]),
    case maps:is_key(Key, Keys) of
	true ->
	    Pid=maps:get(Key, Keys),
	    Value=snp_cache_item:value(Pid),
	    {reply, {ok, Value}, State};
	false ->
	    {reply, {error, <<"pid not found">>}, State}
    end;
handle_call({set, Key, Value}, _From, #state{id=Id, pids=Pids}=State) ->
    Keys=maps:from_list([{K, Pid} || {Pid, K} <- Pids]),
    case maps:is_key(Key, Keys) of
	true ->
	    OldPid=maps:get(Key, Keys),
	    snp_cache_item:stop(OldPid),
	    {ok, Pid}=snp_cache_item_sup:spawn(Id, Key, Value),
	    erlang:monitor(process, Pid),
	    NewPids=maps:put(Pid, Key, Pids),
	    {reply, ok, State#state{pids=NewPids}};
	false ->
	    {ok, Pid}=snp_cache_item_sup:spawn(Id, Key, Value),
	    erlang:monitor(process, Pid),
	    NewPids=maps:put(Pid, Key, Pids),
	    {reply, ok, State#state{pids=NewPids}}
    end;
handle_call({delete, Key}, _From, #state{pids=Pids}=State) ->
    Keys=maps:from_list([{K, Pid} || {Pid, K} <- Pids]),
    case maps:is_key(Key, Keys) of
	true ->
	    Pid=maps:get(Key, Keys),
	    snp_cache_item:stop(Pid),
	    {reply, ok, State};
	false ->
	    {reply, {error, <<"pid not found">>}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, #state{pids=Pids}=State) -> 
    case maps:is_key(Pid, Pids) of
	true -> 
	    {noreply, State};
	false ->
	    {noreply, State}
    end;
handle_info({'EXIT', _Ref, process, Pid, _Reason}, #state{pids=Pids}=State) ->
    case maps:is_key(Pid, Pids) of
	true -> 
	    {noreply, State};
	false ->
	    {noreply, State}
    end; 
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
