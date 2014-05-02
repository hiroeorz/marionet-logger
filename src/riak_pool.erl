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
	 get_analog_logs/4,
	 get_digital_logs/4]).

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

%%--------------------------------------------------------------------
%% @doc Search data from riak.
%% @end
%%--------------------------------------------------------------------
-spec search_logs(SearchQuery, Filter) -> {ok, KeyObjects} |
					  {error, Reason} when
      SearchQuery :: binary(),
      Filter :: function(),
      KeyObjects :: #search_results{},
      Reason :: term().
search_logs(SearchQuery, Filter) when is_binary(SearchQuery),
				      is_function(Filter) ->
    call_pool({search_logs, SearchQuery, Filter}).

%%--------------------------------------------------------------------
%% @doc Get analog history data from Riak.
%% @end
%%--------------------------------------------------------------------
-spec get_analog_logs(Id, No, Start, End) -> [tuple()] when
      Id :: binary(),
      No :: non_neg_integer(),
      Start :: binary(), %% <<"20140501190620000">>
      End :: binary().   %% <<"20140501200620000">>
get_analog_logs(Id, No, Start, End) when is_binary(Id),
					 is_integer(No),
					 is_binary(Start),
					 is_binary(End) ->
    get_logs(Id, <<"ai">>, No, Start, End).

%%--------------------------------------------------------------------
%% @doc Get digital history data from Riak.
%% @end
%%--------------------------------------------------------------------
-spec get_digital_logs(Id, No, Start, End) -> [tuple()] when
      Id :: binary(),
      No :: non_neg_integer(),
      Start :: binary(), %% <<"20140501190620000">>
      End :: binary().   %% <<"20140501200620000">>
get_digital_logs(Id, No, Start, End) when is_binary(Id),
					  is_integer(No),
					  is_binary(Start),
					  is_binary(End) ->
    get_logs(Id, <<"di">>, No, Start, End).

%%--------------------------------------------------------------------
%% @private
%% @doc Get log data from Riak.
%% @end
%%--------------------------------------------------------------------
-define(MAX_LOG_COUNT, 1000).
-spec get_logs(Id, Type, No, Start, End) -> [tuple()] when
      Id :: binary(),
      Type :: binary(),
      No :: non_neg_integer(),
      Start :: binary(), %% <<"20140501190620000">>
      End :: binary().   %% <<"20140501200620000">>
get_logs(Id, Type, No, Start, End) when is_binary(Id),
					is_binary(Type),
					is_integer(No),
					is_binary(Start),
					is_binary(End) ->
    Query = query("type:~s AND id:~s AND no:~w AND datetime:[~s TO ~s]",
		  [Type, Id, No, Start, End]),
    io:format("Query: ~p~n", [Query]),
    Options = [{rows, ?MAX_LOG_COUNT}],

    Fun = fun(E) ->
		  ObjId = proplists:get_value(<<"id">>, E),
		  ObjNo = proplists:get_value(<<"no">>, E),
		  ObjType = proplists:get_value(<<"type">>, E),
		  ObjTime = proplists:get_value(<<"datetime">>, E),
		  (ObjType =:= Type   andalso 
		   ObjId   =:= Id     andalso 
		   ObjNo   =:= No     andalso
		   ObjTime >=  Start  andalso
		   ObjTime =<  End)
	  end,
    call_pool({search_logs, Query, Options, Fun}).

%%--------------------------------------------------------------------
%% @private
%% @doc Call to process in pool.
%% @end
%%--------------------------------------------------------------------
-spec call_pool(atom | tuple()) -> term().
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
handle_call({search, Index, SearchQuery}, _From, State = #state{pid = Pid}) ->
    Reply = riakc_pb_socket:search(Pid, Index, SearchQuery),
    {reply, Reply, State};

handle_call({search_logs, SearchQuery, Filter}, From, State) ->
    handle_call({search_logs, SearchQuery, [], Filter}, From, State);

handle_call({search_logs, SearchQuery, Options, Filter}, _From,
	    State = #state{pid = Pid}) ->
    SearchResult = riakc_pb_socket:search(Pid, <<"fluentlog_index">>, 
					  SearchQuery, Options),
    R = case SearchResult of
	    {ok, #search_results{docs = KeyObjList}} ->
		L = [get_objects(Pid, Obj, Filter) || Obj <- KeyObjList],
		{ok, sorted_logs(lists:merge(L))};
	    {error, Reason} ->
		{error, Reason}
	end,

    {reply, R, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Sort log list (by datetime).
%% @end
%%--------------------------------------------------------------------
-spec sorted_logs(list()) -> list().
sorted_logs(List) ->
    lists:sort(fun(Obj1, Obj2) ->
		       Time1 = proplists:get_value(<<"datetime">>, Obj1),
		       Time2 = proplists:get_value(<<"datetime">>, Obj2),
		       Time1 =< Time2
	       end, List).

%%--------------------------------------------------------------------
%% @private
%% @doc Create query for Solr.
%% @end
%%--------------------------------------------------------------------
-spec query(string(), list()) -> binary().
query(Str, Params) ->
	list_to_binary(io_lib:format(Str, Params)).

%%--------------------------------------------------------------------
%% @private
%% @doc Get sotred objects from Riak.
%% @end
%%--------------------------------------------------------------------
-spec get_objects(pid(), tuple(), function()) -> [tuple()].
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
