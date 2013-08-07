


Clients and Servers
-------------------


Callback to the Future
~~~~~~~~~~~~~~~~~~~~~~

The first OTP behaviour we'll see is one of the most used ones. Its
name is `gen_server` and it has an interface a bit similar to the one
we've written with `my_server` in `last chapter`_; it gives you a few
functions to use it and in exchange, your module has to already have a
few functions `gen_server` will use.



init
````

The first one is an `init/1` function. It is similar to the one we've
used with `my_server` in that it is used to initialize the server's
state and do all of these one-time tasks that it will depend on. The
function can return `{ok, State}`, `{ok, State, TimeOut}`, `{ok,
State, hibernate}`, `{stop, Reason}` or `ignore`.

The normal `{ok, State}` return value doesn't really need explaining
past saying that State will be passed directly to the main loop of the
process as the state to keep later on. The TimeOut variable is meant
to be added to the tuple whenever you need a deadline before which you
expect the server to receive a message. If no message is received
before the deadline, a special one (the atom `timeout`) is sent to the
server, which should be handled with `handle_info/2` (more on this
later.)

On the other hand, if you do expect the process to take a long time
before getting a reply and are worried about memory, you can add the
`hibernate` atom to the tuple. Hibernation basically reduces the size
of the process' state until it gets a message, at the cost of some
processing power. If you are in doubt about using hibernation, you
probably don't need it.

Returning `{stop, Reason}` should be done when something went wrong
during the initialization.

Note: here's a more technical definition of process hibernation. It's
no big deal if some readers do not understand or care about it. When
the BIF erlang:hibernate(M,F,A) is called, the call stack for the
currently running process is discarded (the function never returns).
The garbage collection then kicks in, and what's left is one
continuous heap that is shrunken to the size of the data in the
process. This basically compacts all the data so the process takes
less place.

Once the process receives a message, the function `M:F` with A as
arguments is called and the execution resumes.

Note: while `init/1` is running, execution is blocked in the process
that spawned the server. This is because it is waiting for a 'ready'
message sent automatically by the `gen_server` module to make sure
everything went fine.



handle_call
```````````

The function `handle_call/3` is used to work with synchronous messages
(we'll see how to send them soon). It takes 3 arguments: Request ,
From , and State . It's pretty similar to how we programmed our own
`handle_call/3` in `my_server`. The biggest difference is how you
reply to messages. In our own abstraction of a server, it was
necessary to use `my_server:reply/2` to talk back to the process. In
the case of `gen_server`s, there are 8 different return values
possible, taking the form of tuples.

Because there are many of them, here's a simple list instead:


::

    
    {reply,Reply,NewState}
    {reply,Reply,NewState,Timeout}
    {reply,Reply,NewState,hibernate}
    {noreply,NewState}
    {noreply,NewState,Timeout}
    {noreply,NewState,hibernate}
    {stop,Reason,Reply,NewState}
    {stop,Reason,NewState}


For all of these, Timeout and `hibernate` work the same way as for
`init/1`. Whatever is in Reply will be sent back to whoever called the
server in the first place. Notice that there are three possible
`noreply` options. When you use `noreply`, the generic part of the
server will assume you're taking care of sending the reply back
yourself. This can be done with `gen_server:reply/2`, which can be
used in the same way as `my_server:reply/2`.

Most of the time, you'll only need the `reply` tuples. There are still
a few valid reasons to use `noreply`: whenever you want another
process to send the reply for you or when you want to send an
acknowledgement ('hey! I received the message!') but still process it
afterwards (without replying this time), etc. If this is what you
choose to do, it is absolutely necessary to use `gen_server:reply/2`
because otherwise the call will time out and cause a crash.



handle_cast
```````````

The `handle_cast/2` callback works a lot like the one we had in
`my_server`: it takes the parameters Message and State and is used to
handle asynchronous calls. You do whatever you want in there, in a
manner quite similar to what's doable with `handle_call/3`. On the
other hand, only tuples without replies are valid return values:


::

    
    {noreply,NewState}
    {noreply,NewState,Timeout}
    {noreply,NewState,hibernate}
    {stop,Reason,NewState}




handle_info
```````````

You know how I mentioned our own server didn't really deal with
messages that do not fit our interface, right? Well `handle_info/2` is
the solution. It's very similar to `handle_cast/2` and in fact returns
the same tuples. The difference is that this callback is only there
for messages that were sent directly with the `!` operator and special
ones like `init/1`'s `timeout`, monitors' notifications and `'EXIT'`
signals.



terminate
`````````

The callback `terminate/2` is called whenever one of the three
`handle_Something` functions returns a tuple of the form `{stop,
Reason, NewState}` or `{stop, Reason, Reply, NewState}`. It takes two
parameters, Reason and State , corresponding to the same values from
the `stop` tuples.

`terminate/2` will also be called when its parent (the process that
spawned it) dies, if and only if the `gen_server` is trapping exits.

Note: if any reason other than `normal`, `shutdown` or `{shutdown,
Term}` is used when `terminate/2` is called, the OTP framework will
see this as a failure and start logging a bunch of stuff here and
there for you.

This function is pretty much the direct opposite of `init/1` so
whatever was done in there should have its opposite in `terminate/2`.
It's your server's janitor, the function in charge of locking the door
after making sure everyone's gone. Of course, the function is helped
by the VM itself, which should usually delete all ETS tables, close
all ports, etc. for you. Note that the return value of this function
doesn't really matter, because the code stops executing after it's
been called.



code_change
```````````

The function `code_change/3` is there to let you upgrade code. It
takes the form `code_change(PreviousVersion, State, Extra)`. Here, the
variable PreviousVersion is either the version term itself in the case
of an upgrade (read `More About Modules`_ again if you forget what
this is), or `{down, Version}` in the case of a downgrade (just
reloading older code). The State variable holds all of the current's
server state so you can convert it.

Imagine for a moment that we used an orddict to store all of our data.
However, as time goes on, the orddict becomes too slow and we decide
to change it for a regular dict. In order to avoid the process
crashing on the next function call, the conversion from one data
structure to the other can be done in there, safely. All you have to
do is return the new state with `{ok, NewState}`.

The Extra variable isn't something we'll worry about for now. It's
mostly used in larger OTP deployment, where specific tools exist to
upgrade entire releases on a VM. We're not there yet.

So now we've got all the callbacks defined. Don't worry if you're a
bit lost: the OTP framework is a bit circular sometimes, where to
understand part A of the framework you have to understand part B , but
then part B requires to see part A to be useful. The best way to get
over that confusion is to actually implement a gen_server.



.BEAM me up, Scotty!
~~~~~~~~~~~~~~~~~~~~

This is going to be the `kitty_gen_server`. It's going to be mostly
similar to `kitty_server2`, with only minimal API changes. First start
a new module with the following lines in it:


::

    
    -module(kitty_gen_server).
    -behaviour(gen_server).


And try to compile it. You should get something like this:


::

    
    1> c(kitty_gen_server).
    ./kitty_gen_server.erl:2: Warning: undefined callback function code_change/3 (behaviour 'gen_server')
    ./kitty_gen_server.erl:2: Warning: undefined callback function handle_call/3 (behaviour 'gen_server')
    ./kitty_gen_server.erl:2: Warning: undefined callback function handle_cast/2 (behaviour 'gen_server')
    ./kitty_gen_server.erl:2: Warning: undefined callback function handle_info/2 (behaviour 'gen_server')
    ./kitty_gen_server.erl:2: Warning: undefined callback function init/1 (behaviour 'gen_server')
    ./kitty_gen_server.erl:2: Warning: undefined callback function terminate/2 (behaviour 'gen_server')
    {ok,kitty_gen_server}


The compilation worked, but there are warnings about missing
callbacks. This is because of the `gen_server` behaviour. A behaviour
is basically a way for a module to specify functions it expects
another module to have. The behaviour is the contract sealing the deal
between the well-behaved generic part of the code and the specific,
error-prone part of the code (yours).

Note: both 'behavior' and 'behaviour' are accepted by the Erlang
compiler.

Defining your own behaviours is really simple. You just need to export
a function called `behaviour_info/1` implemented as follows:


::

    
    -module(my_behaviour).
    -export([behaviour_info/1]).
    
    %% init/1, some_fun/0 and other/3 are now expected callbacks
    behaviour_info(callbacks) -> [{init,1}, {some_fun, 0}, {other, 3}];
    behaviour_info(_) -> undefined.


And that's about it for behaviours. You can just use
`-behaviour(my_behaviour).` in a module implementing them to get
compiler warnings if you forgot a function. Anyway, back to our third
kitty server.

The first function we had was `start_link/0`. This one can be changed
to the following:


::

    
    start_link() -> gen_server:start_link(?MODULE, [], []).


The first parameter is the callback module, the second one is the list
of parameters to pass to `init/1` and the third one is about debugging
options that won't be covered right now. You could add a fourth
parameter in the first position, which would be the name to register
the server with. Note that while the previous version of the function
simply returned a pid, this one instead returns `{ok, Pid}`.

Next functions now:


::

    
    %% Synchronous call
    order_cat(Pid, Name, Color, Description) ->
       gen_server:call(Pid, {order, Name, Color, Description}).
    
    %% This call is asynchronous
    return_cat(Pid, Cat = #cat{}) ->
        gen_server:cast(Pid, {return, Cat}).
    
    %% Synchronous call
    close_shop(Pid) ->
        gen_server:call(Pid, terminate).


All of these calls are a one-to-one change. Note that a third
parameter can be passed to `gen_server:call/2-3` to give a timeout. If
you don't give a timeout to the function (or the atom `infinity`), the
default is set to 5 seconds. If no reply is received before time is
up, the call crashes.

Now we'll be able to add the gen_server callbacks. The following table
shows the relationship we have between calls and callbacks:
gen_server YourModule `start/3-4` `init/1` `start_link/3-4` `init/1`
`call/2-3` `handle_call/3` `cast/2` `handle_cast/2`
And then you have the other callbacks, those that are more about
special cases:


+ `handle_info/2`
+ `terminate/2`
+ `code_change/3`


Let's begin by changing those we already have to fit the model:
`init/1`, `handle_call/3` and `handle_cast/2`.


::

    
    %%% Server functions
    init([]) -> {ok, []}. %% no treatment of info here!
    
    handle_call({order, Name, Color, Description}, _From, Cats) ->
        if Cats =:= [] ->
            {reply, make_cat(Name, Color, Description), Cats};
           Cats =/= [] ->
            {reply, hd(Cats), tl(Cats)}
        end;
    handle_call(terminate, _From, Cats) ->
        {stop, normal, ok, Cats}.
    
    handle_cast({return, Cat = #cat{}}, Cats) ->
        {noreply, [Cat|Cats]}.


Again, very little has changed there. In fact, the code is now
shorter, thanks to smarter abstractions. Now we get to the new
callbacks. The first one is `handle_info/2`. Given this is a toy
module and we have no logging system pre-defined, just outputting the
unexpected messages will be enough:


::

    
    handle_info(Msg, Cats) ->
        io:format("Unexpected message: ~p~n",[Msg]),
        {noreply, Cats}.


The next one is the `terminate/2` callback. It will be very similar to
the `terminate/1` private function we had:


::

    
    terminate(normal, Cats) ->
        [io:format("~p was set free.~n",[C#cat.name]) || C <- Cats],
        ok.


And then the last callback, `code_change/3`:


::

    
    code_change(_OldVsn, State, _Extra) ->
        %% No change planned. The function is there for the behaviour,
        %% but will not be used. Only a version on the next
        {ok, State}. 


Just remember to keep in the `make_cat/3` private function:


::

    
    %%% Private functions
    make_cat(Name, Col, Desc) ->
        #cat{name=Name, color=Col, description=Desc}.


And we can now try the brand new code:


::

    
    1> c(kitty_gen_server).
    {ok,kitty_gen_server}
    2> rr(kitty_gen_server).
    [cat]
    3> {ok, Pid} = kitty_gen_server:start_link().
    {ok,<0.253.0>}
    4> Pid ! <<"Test handle_info">>.
    Unexpected message: <<"Test handle_info">>
    <<"Test handle_info">>
    5> Cat = kitty_gen_server:order_cat(Pid, "Cat Stevens", white, "not actually a cat").
    #cat{name = "Cat Stevens",color = white,
         description = "not actually a cat"}
    6> kitty_gen_server:return_cat(Pid, Cat).
    ok
    7> kitty_gen_server:order_cat(Pid, "Kitten Mittens", black, "look at them little paws!").
    #cat{name = "Cat Stevens",color = white,
         description = "not actually a cat"}
    8> kitty_gen_server:order_cat(Pid, "Kitten Mittens", black, "look at them little paws!").
    #cat{name = "Kitten Mittens",color = black,
         description = "look at them little paws!"}
    9> kitty_gen_server:return_cat(Pid, Cat).
    ok       
    10> kitty_gen_server:close_shop(Pid).
    "Cat Stevens" was set free.
    ok


Oh and hot damn, it works!

So what can we say about this generic adventure? Probably the same
generic stuff as before: separating the generic from the specific is a
great idea on every point. Maintenance is simpler, complexity is
reduced, the code is safer, easier to test and less prone to bugs. If
there are bugs, they are easier to fix. Generic servers are only one
of the many available abstractions, but they're certainly one of the
most used ones. We'll see more of these abstractions and behaviours in
the next chapters.

.. _last chapter: what-is-otp.html#the-basic-server
.. _More About Modules: modules.html#more-about-modules


