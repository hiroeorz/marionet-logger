%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2014, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created :  1 May 2014 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(riak_pool).

-behaviour(gen_server).

-include_lib("riakc/include/riakc.hrl").

%% API
-export([start_link/1,
	 search/2,
	 search_logs/2,
	 get_analog_history/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {pid :: pid()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Args) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%--------------------------------------------------------------------
%% @doc Search data from riak.
%% @end
%%--------------------------------------------------------------------
-spec search(Index, Query) -> {ok, riakc_pb_socket:search_result()} | 
			      {error, term()} when
      Index :: binary(),
      Query :: binary().
search(Index, SearchQuery) ->
    call_pool({search, Index, SearchQuery}).

search_logs(SearchQuery, Filter) ->
    call_pool({search_logs, SearchQuery, Filter}).

get_analog_history(Id, No) ->
    call_pool({get_analog_history, Id, No}).

call_pool(Req) ->
    F = fun(Pid) -> gen_server:call(Pid, Req) end,
    poolboy:transaction(riak_pool, F).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Args) ->
    process_flag(trap_exit, true),
    Host = proplists:get_value(host, Args),
    Port = proplists:get_value(port, Args),
    {ok, Pid} = riakc_pb_socket:start_link(Host, Port),
    {ok, #state{pid = Pid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get_analog_history, Id, No}, From, State) ->    
    Query = query("type:ai AND id:~s AND no:~w", [Id, No]),
    F = fun(E) ->
		ObjId = proplists:get_value(<<"id">>, E),
		ObjNo = proplists:get_value(<<"no">>, E),
		(ObjId =:= Id andalso ObjNo =:= No)
	end,
    handle_call({search_logs, Query, F}, From, State);

handle_call({search, Index, SearchQuery}, _From, State = #state{pid = Pid}) ->
    Reply = riakc_pb_socket:search(Pid, Index, SearchQuery),
    {reply, Reply, State};

handle_call({search_logs, SearchQuery, Filter}, _From,
	    State = #state{pid = Pid}) ->
    R = case riakc_pb_socket:search(Pid, <<"fluentlog_index">>, SearchQuery) of
	    {ok, #search_results{docs = KeyObjList}} ->
		L = [get_objects(Pid, Obj, Filter) || Obj <- KeyObjList],
		{ok, lists:merge(L)};
	    {error, Reason} ->
		{error, Reason}
	end,
    {reply, R, State}.

query(Str, Params) ->
	list_to_binary(io_lib:format(Str, Params)).

get_objects(Pid, {_Index, Params}, Filter) ->
    Type = proplists:get_value(<<"_yz_rt">>, Params),
    Bucket = proplists:get_value(<<"_yz_rb">>, Params),
    Key = proplists:get_value(<<"_yz_rk">>, Params),
    {ok, RObj} = riakc_pb_socket:get(Pid, {Type, Bucket}, Key),	    
    [{_MD, Val}] = riakc_obj:get_contents(RObj),
    List = jsx:decode(Val),
    lists:filter(Filter, List).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
