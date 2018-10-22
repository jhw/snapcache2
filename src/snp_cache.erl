-module(snp_cache).

-behaviour(gen_server).

%% API.

-export([start_link/0,
	 items/1]).

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

%% gen_server.

init([]) ->
    {ok, #state{items=#{}}}.

%% need to return values for each pid

handle_call(items, _From, #state{items=Items}=State) ->
    {reply, Items, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
