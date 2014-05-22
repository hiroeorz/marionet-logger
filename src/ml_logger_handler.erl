%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2014, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 28 Apr 2014 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(ml_logger_handler).

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
    lager:info("ml_logger init."),
    {ok, Pid} = gen_event:start_link(),
    ok = gen_event:add_handler(Pid, fluent_event, io),
    {ok,  #state{fluent_logger_pid = Pid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling task messages
%% @end
%%--------------------------------------------------------------------
handle_pop({publish, Topic, Payload}, 
	   State = #state{fluent_logger_pid = Pid}) ->
    Obj = marionet_data:unpack(Payload),
    Obj1 = [{<<"topic">>, Topic} | Obj],

    case proplists:get_value(<<"type">>, Obj) of
	<<"di">> ->
	    ok = gen_event:notify(Pid, {<<"digital">>, Obj1});
	<<"ai">> ->
	    ok = gen_event:notify(Pid, {<<"analog">>, Obj1});
	Type ->
	    lager:warning("unknown io log type: ~p", [Type])
    end,

    {ok, State};

handle_pop(Task, State) ->
    io:format("unknown task: ~p~n", [Task]),
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
