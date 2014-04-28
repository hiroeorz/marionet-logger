%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2014, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 21 Jan 2014 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(marionet_data).

%% API
-export([pack_io/5, 
	 unpack_io/1,
	 unpack_command/1]).

-export([pack/1, unpack/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Format analog status data.
%% @end
%%--------------------------------------------------------------------
-spec pack_io(Type, DeviceId, AnalogNo, Val, Opts) -> Payload when
      Type :: binary(),
      DeviceId :: binary(),
      AnalogNo :: non_neg_integer(),
      Val :: non_neg_integer(),
      Opts :: [tuple()],
      Payload :: binary().
pack_io(Type, DeviceId, AnalogNo, Val, Opts) ->
    Obj = [{<<"type">>, Type},
	   {<<"id">>, DeviceId},
	   {<<"no">>, AnalogNo},
	   {<<"val">>, Val},
	   {<<"opts">>, Opts}
	  ],
    pack(Obj).

%%--------------------------------------------------------------------
%% @doc Parse analog status data.
%% @end
%%--------------------------------------------------------------------
-spec unpack_io(Payload) -> {Type, DeviceId, No, Val, Opts} when
      Payload :: binary(),
      Type :: binary(),
      DeviceId :: binary(),
      No :: non_neg_integer(),
      Val :: non_neg_integer(),
      Opts :: [tuple()].
unpack_io(Payload) ->
    Obj = unpack(Payload),
    Type = proplists:get_value(<<"type">>, Obj),
    DeviceId = proplists:get_value(<<"id">>, Obj),
    No = proplists:get_value(<<"no">>, Obj),
    Val = proplists:get_value(<<"val">>, Obj),
    Opts = proplists:get_value(<<"opts">>, Obj),
    {Type, DeviceId, No, Val, Opts}.

%%--------------------------------------------------------------------
%% @doc Parse command that send from other application.
%% @end
%%--------------------------------------------------------------------
-spec unpack_command(Payload) -> {UUID, Command, Args} when
      Payload :: binary(),
      UUID :: binary(),
      Command :: binary(),
      Args :: [term()].
unpack_command(Payload) ->
    Obj = unpack(Payload),
    Command = proplists:get_value(<<"command">>, Obj),
    Args = proplists:get_value(<<"args">>, Obj),
    UUID = proplists:get_value(<<"uuid">>, Obj),
    {UUID, Command, Args}.

%%--------------------------------------------------------------------
%% @doc format using msgpack
%% @end
%%--------------------------------------------------------------------
-spec pack(term()) -> binary().
pack(Obj) ->
    msgpack:pack(Obj, [{format, jsx}]).
    %%jsx:encode(Obj).

-spec unpack(binary()) -> term().
unpack(Bin) ->
    case msgpack:unpack(Bin, [jsx]) of
	{ok, Val} ->
	    Val;
	{error, {badarg, name}} ->
	    lager:error("Invalid msgpack data received: ~p", [Bin]),
	    []
    end.
    %%jsx:decode(Bin).

%%%===================================================================
%%% Internal functions
%%%===================================================================
