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
	 start_subscribe/2,
	 start_subscribe/3,
	 get_analog_logs/4,
	 get_digital_logs/4]).

-export([start_dev/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Start application
%% @end
%%--------------------------------------------------------------------
start() ->
    application:start(fluent),
    application:start(emqttc),
    application:start(gen_queue),
    application:start(poolcat),
    application:start(poolboy),
    application:start(marionet_logger).

%%--------------------------------------------------------------------
%% @doc Create client and connect to MQTT broker.
%% @end
%%--------------------------------------------------------------------
-spec start_subscribe(Host, Topics) -> {ok, pid()} when
      Host :: string(),
      Topics :: [{binary(), non_neg_integer()}].
start_subscribe(Host, Topics) when is_list(Topics) ->
    start_subscribe(Host, 1883, Topics).

-spec start_subscribe(Host, Port, Topics) -> {ok, pid()} when
      Host :: string(),
      Port :: inet:port_number(),
      Topics :: [{binary(), non_neg_integer()}].
start_subscribe(Host, Port, Topics) when is_list(Topics) ->
    ml_client_sup:start_client(Host, Port, Topics).

%%%===================================================================
%%% Query functions
%%%===================================================================

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
    riak_pool:get_analog_logs(Id, No, Start, End).

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
    riak_pool:get_digital_logs(Id, No, Start, End).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% Development functions
%%%===================================================================

%% set mqtt.marionet.org of ipaddress in /etc/hosts .
start_dev() ->
    ?MODULE:start(),
    ?MODULE:start_subscribe("mqtt.marionet.org", 
			    [{<<"/demo/galileo/analog/#">>, 0},
			     {<<"/demo/pi001/analog/#">>,   0}]),

    ?MODULE:start_subscribe("mqtt.marionet.org", 
			    [{<<"/demo/galileo/digital/#">>, 0},
			     {<<"/demo/pi001/digital/#">>,   0}]),
    ok.
    
