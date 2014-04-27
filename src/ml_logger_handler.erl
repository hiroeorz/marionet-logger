%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2014, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 28 Apr 2014 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(ml_logger_handler).

-behaviour(poolcat_worker).

%% gen_server callbacks
-export([init/1, handle_pop/2, terminate/2]).

-record(state, {fluent_logger_pid :: pid()}).

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
init(_Any) ->
    io:format("poolcat handler init~n"),
    {ok, Pid} = gen_event:start_link(),
    ok = gen_event:add_handler(Pid, fluent_event, io),
    {ok,  #state{fluent_logger_pid = Pid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling task messages
%% @end
%%--------------------------------------------------------------------
handle_pop(Task, State = #state{fluent_logger_pid = Pid}) ->
    %% process your task using Task and State
    io:format("TASK: ~p~n", [Task]),
    ok = gen_event:notify(Pid, {digital, [{<<"agent">>,<<"foo">>}]}),
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

%%%===================================================================
%%% Internal functions
%%%===================================================================
