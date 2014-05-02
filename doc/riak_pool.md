

# Module riak_pool #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.
Copyright (c) (C) 2014, HIROE Shin

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ HIROE Shin ([`shin@HIROE-no-MacBook-Pro.local`](mailto:shin@HIROE-no-MacBook-Pro.local)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_analog_history-3">get_analog_history/3</a></td><td>Get analog history data from Riak.</td></tr><tr><td valign="top"><a href="#get_digital_history-3">get_digital_history/3</a></td><td>Get analog history data from Riak.</td></tr><tr><td valign="top"><a href="#search-2">search/2</a></td><td>Search data from riak.</td></tr><tr><td valign="top"><a href="#search_logs-2">search_logs/2</a></td><td>Search data from riak.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>
Starts the server.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_analog_history-3"></a>

### get_analog_history/3 ###


<pre><code>
get_analog_history(Id, No, Count) -&gt; [tuple()]
</code></pre>

<ul class="definitions"><li><code>Count = non_neg_integer()</code></li><li><code>Id = binary()</code></li><li><code>No = non_neg_integer()</code></li></ul>

Get analog history data from Riak.
<a name="get_digital_history-3"></a>

### get_digital_history/3 ###


<pre><code>
get_digital_history(Id, PortNo, Count) -&gt; [tuple()]
</code></pre>

<ul class="definitions"><li><code>Count = non_neg_integer()</code></li><li><code>Id = binary()</code></li><li><code>PortNo = non_neg_integer()</code></li></ul>

Get analog history data from Riak.
<a name="search-2"></a>

### search/2 ###


<pre><code>
search(Index, Query) -&gt; {ok, <a href="riakc_pb_socket.md#type-search_result">riakc_pb_socket:search_result()</a>} | {error, term()}
</code></pre>

<ul class="definitions"><li><code>Index = binary()</code></li><li><code>Query = binary()</code></li></ul>

Search data from riak.
<a name="search_logs-2"></a>

### search_logs/2 ###


<pre><code>
search_logs(SearchQuery, Filter) -&gt; {ok, KeyObjects} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>SearchQuery = binary()</code></li><li><code>Filter = function()</code></li><li><code>KeyObjects = #search_results{}</code></li><li><code>Reason = term()</code></li></ul>

Search data from riak.
<a name="start_link-1"></a>

### start_link/1 ###


<pre><code>
start_link(Args) -&gt; {ok, Pid} | ignore | {error, Error}
</code></pre>

<br></br>



Starts the server

