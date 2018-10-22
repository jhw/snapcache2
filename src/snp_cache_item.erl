-module(snp_cache_item).

-behaviour(gen_server).

%% API.

-export([start_link/2,
	 value/1,
	 expiry/1]).

%% gen_server.

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-import(wol_datetime, [timedelta/2,
		       now_utc/0]).

-record(state, {value, expiry}).

%% API.

start_link(Value, Expiry) ->
    gen_server:start_link(?MODULE, [Value, Expiry], []).

%% add GS_GLOBAL_TIMEOUT ?

value(Pid) ->
    gen_server:call(Pid, value).

expiry(Pid) ->
    gen_server:call(Pid, expiry).

%% gen_server.

init([Value, Expiry]) ->
    SecsToExpiry=secs_to_expiry(Expiry),
    erlang:start_timer(timer:seconds(SecsToExpiry), self(), stop),
    {ok, #state{value=Value,
		expiry=Expiry}}.

handle_call(value, _From, #state{value=Value}=State) ->
    {reply, Value, State};
handle_call(expiry, _From, #state{expiry=Expiry}=State) ->
    {reply, Expiry, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, _, stop}, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal functions

secs_to_expiry(Secs) when is_integer(Secs) ->
    Secs;
secs_to_expiry(Expiry) ->
    max(0, timedelta(now_utc(), Expiry)).


