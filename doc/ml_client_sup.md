

# Module ml_client_sup #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.
Copyright (c) (C) 2014, HIROE Shin

__Behaviours:__ [`supervisor`](supervisor.md).

__Authors:__ HIROE Shin ([`shin@HIROE-no-MacBook-Pro.local`](mailto:shin@HIROE-no-MacBook-Pro.local)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#start_client-1">start_client/1</a></td><td>Create subscribe client process.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>
Starts the supervisor.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="start_client-1"></a>

### start_client/1 ###


<pre><code>
start_client(SubOpts) -&gt; {ok, pid()}
</code></pre>

<ul class="definitions"><li><code>SubOpts = [{atom(), binary() | <a href="inet.md#type-port_number">inet:port_number()</a>}]</code></li></ul>

Create subscribe client process.
<a name="start_link-0"></a>

### start_link/0 ###


<pre><code>
start_link() -&gt; {ok, Pid} | ignore | {error, Error}
</code></pre>

<br></br>



Starts the supervisor

