

# Module ml_event #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.
Copyright (c) (C) 2013, HIROE Shin

__Behaviours:__ [`gen_event`](gen_event.md).

__Authors:__ HIROE Shin ([`shin@HIROE-no-MacBook-Pro.local`](mailto:shin@HIROE-no-MacBook-Pro.local)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_handler-3">add_handler/3</a></td><td>Adds an event handler.</td></tr><tr><td valign="top"><a href="#notify_io-2">notify_io/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>Creates an event manager.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_handler-3"></a>

### add_handler/3 ###


<pre><code>
add_handler(Pid, Handler, Arg) -&gt; ok | {'EXIT', term()} | term()
</code></pre>

<ul class="definitions"><li><code>Pid = pid()</code></li><li><code>Handler = atom()</code></li><li><code>Arg = [term()]</code></li></ul>

Adds an event handler
<a name="notify_io-2"></a>

### notify_io/2 ###


<pre><code>
notify_io(Facility, Obj) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Facility = binary()</code></li><li><code>Obj = [{Name, Data}]</code></li><li><code>Name = atom() | binary()</code></li><li><code>Data = term()</code></li></ul>


<a name="start_link-1"></a>

### start_link/1 ###


<pre><code>
start_link(Handlers) -&gt; {ok, pid()} | {error, term()}
</code></pre>

<ul class="definitions"><li><code>Handlers = [{atom(), list()}]</code></li></ul>

Creates an event manager
