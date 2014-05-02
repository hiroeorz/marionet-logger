

# Module marionet_logger #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.
Copyright (c) (C) 2014, HIROE Shin

__Authors:__ HIROE Shin ([`shin@HIROE-no-MacBook-Pro.local`](mailto:shin@HIROE-no-MacBook-Pro.local)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_analog_logs-4">get_analog_logs/4</a></td><td>Get analog history data from Riak.</td></tr><tr><td valign="top"><a href="#get_digital_logs-4">get_digital_logs/4</a></td><td>Get digital history data from Riak.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>Start application.</td></tr><tr><td valign="top"><a href="#start_dev-0">start_dev/0</a></td><td></td></tr><tr><td valign="top"><a href="#start_subscribe-2">start_subscribe/2</a></td><td>Create client and connect to MQTT broker.</td></tr><tr><td valign="top"><a href="#start_subscribe-3">start_subscribe/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_analog_logs-4"></a>

### get_analog_logs/4 ###


<pre><code>
get_analog_logs(Id, No, Start, End) -&gt; [tuple()]
</code></pre>

<ul class="definitions"><li><code>Id = binary()</code></li><li><code>No = non_neg_integer()</code></li><li><code>Start = binary()</code></li><li><code>End = binary()</code></li></ul>

Get analog history data from Riak.
<a name="get_digital_logs-4"></a>

### get_digital_logs/4 ###


<pre><code>
get_digital_logs(Id, No, Start, End) -&gt; [tuple()]
</code></pre>

<ul class="definitions"><li><code>Id = binary()</code></li><li><code>No = non_neg_integer()</code></li><li><code>Start = binary()</code></li><li><code>End = binary()</code></li></ul>

Get digital history data from Riak.
<a name="start-0"></a>

### start/0 ###

`start() -> any()`

Start application
<a name="start_dev-0"></a>

### start_dev/0 ###

`start_dev() -> any()`


<a name="start_subscribe-2"></a>

### start_subscribe/2 ###


<pre><code>
start_subscribe(Host, Topics) -&gt; {ok, pid()}
</code></pre>

<ul class="definitions"><li><code>Host = string()</code></li><li><code>Topics = [{binary(), non_neg_integer()}]</code></li></ul>

Create client and connect to MQTT broker.
<a name="start_subscribe-3"></a>

### start_subscribe/3 ###


<pre><code>
start_subscribe(Host, Port, Topics) -&gt; {ok, pid()}
</code></pre>

<ul class="definitions"><li><code>Host = string()</code></li><li><code>Port = <a href="inet.md#type-port_number">inet:port_number()</a></code></li><li><code>Topics = [{binary(), non_neg_integer()}]</code></li></ul>


