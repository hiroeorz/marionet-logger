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
	 start_subscribe/3]).

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
    
