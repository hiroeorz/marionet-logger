%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2014, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 26 Apr 2014 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(marionet_logger).

%% API
-export([start/0,
	 start_client/1,
	 start_client/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Start application
%% @end
%%--------------------------------------------------------------------
start() ->
    application:start(emqttc),
    application:start(marionet_logger).

%%--------------------------------------------------------------------
%% @doc Create client and connect to MQTT broker.
%% @end
%%--------------------------------------------------------------------
-spec start_client(Host) -> {ok, pid()} when
      Host :: string().
start_client(Host) ->
    start_client(Host, 1883).

-spec start_client(Host, Port) -> {ok, pid()} when
      Host :: string(),
      Port :: inet:port_number().
start_client(Host, Port) ->
    marionet_logger_client_sup:start_client(Host, Port).

%%%===================================================================
%%% Internal functions
%%%===================================================================
