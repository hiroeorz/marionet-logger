%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2013, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 30 Dec 2013 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(ml_event).

-behaviour(gen_event).

%% API
-export([start_link/1, add_handler/3, notify_io/2]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates an event manager
%% @end
%%--------------------------------------------------------------------
-spec start_link(Handlers) -> {ok, pid()} | {error, term()} when
      Handlers :: [{atom(), list()}].
start_link(Handlers) ->
    {ok, Pid} = gen_event:start_link(),
    Handlers2 = Handlers ++ [{?MODULE, []}],
    [add_handler(Pid, Handler, Arg) || {Handler, Arg} <- Handlers2],
    {ok, Pid}.

%%--------------------------------------------------------------------
%% @doc Adds an event handler
%% @end
%%--------------------------------------------------------------------
-spec add_handler(Pid, Handler, Arg) -> ok | {'EXIT', term()} | term() when
      Pid :: pid(),
      Handler :: atom(),
      Arg :: [term()].
add_handler(Pid, Handler, Arg) ->
    gen_event:add_handler(Pid, Handler, Arg).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================
-spec notify_io(Facility, Obj) -> ok when
      Facility :: binary(),
      Obj :: [ {Name, Data} ],
      Name :: atom() | binary(),
      Data :: term().
notify_io(Facility, Obj) when is_binary(Facility),
			      is_list(Obj) ->
    F = fun(Worker) -> gen_event:notify(Worker, {Facility, Obj}) end,
    ok = poolboy:transaction(fluent_logger, F).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
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
