-module(snp_cache).

-behaviour(gen_server).

%% API.

-export([start_link/0,
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

-record(state, {items}).

%% API.

start_link() ->
    gen_server:start_link(?MODULE, [], []).

items(Pid) ->
    gen_server:call(Pid, items).

add(Pid, Key, Value) ->
    gen_server:call(Pid, {add, Key, Value}).

get(Pid, Key) ->
    gen_server:call(Pid, {get, Key}).

set(Pid, Key, Value) ->
    gen_server:call(Pid, {set, Key, Value}).

delete(Pid, Key) ->
    gen_server:call(Pid, {delete, Key}).

%% gen_server.

init([]) ->
    {ok, #state{items=#{}}}.

%% need to return values for each pid

handle_call(items, _From, #state{items=Items}=State) ->
    {reply, Items, State};
handle_call({add, _Key, _Value}, _From, #state{items=_Items}=State) ->
    %% check if pid exists
    %% spawn pid
    %% monitor pid
    {reply, ok, State};
handle_call({get, _Key}, _From, #state{items=_Items}=State) ->
    %% find pid
    %% return value
    {reply, ok, State};
handle_call({set, _Key, _Value}, _From, #state{items=_Items}=State) ->
    %% check if pid exists
    %% spawn pid
    %% monitor pid
    {reply, ok, State};
handle_call({delete, _Key}, _From, #state{items=_Items}=State) ->
    %% find pid
    %% send stop messages
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% handle DOWN, EXIT
%% remove from items
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
