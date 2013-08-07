


Errors and Processes
--------------------



Links
~~~~~

A link is a specific kind of relationship that can be created between
two processes. When that relationship is set up and one of the
processes dies from an unexpected throw, error or exit (see `Errors
and Exceptions`_), the other linked process also dies.

This is a useful concept from the perspective of failing as soon as
possible to stop errors: if the process that has an error crashes but
those that depend on it don't, then all these depending processes now
have to deal with a dependency disappearing. Letting them die and then
restarting the whole group is usually an acceptable alternative. Links
let us do exactly this.

To set a link between two processes, Erlang has the primitive function
link/1, which takes a Pid as an argument. When called, the function
will create a link between the current process and the one identified
by Pid . To get rid of a link, use unlink/1. When one of the linked
processes crashes, a special kind of message is sent, with information
relative to what happened. No such message is sent if the process dies
of natural causes (read: is done running its functions.) I'll first
introduce this new function as part of linkmon.erl:


::

    
    myproc() ->
        timer:sleep(5000),
        exit(reason).


If you try the next following calls (and wait 5 seconds between each
spawn command), you should see the shell crashing for 'reason' only
when a link has been set between the two processes.


::

    
    1> c(linkmon).
    {ok,linkmon}
    2> spawn(fun linkmon:myproc/0).
    <0.52.0>
    3> link(spawn(fun linkmon:myproc/0)).
    true
    ** exception error: reason


Or, to put it in picture:


.. image:: ../images/link-exit.png
    :alt: A process receiving an exit signal


However, this `{'EXIT', B, Reason}` message can not be caught with a
`try ... catch` as usual. Other mechanisms need to be used to do this.
We'll see them later.

It's important to note that links are used to establish larger groups
of processes that should all die together:


::

    
    chain(0) ->
        receive
            _ -> ok
        after 2000 ->
            exit("chain dies here")
        end;
    chain(N) ->
        Pid = spawn(fun() -> chain(N-1) end),
        link(Pid),
        receive
            _ -> ok
        end.


This function will take an integer N , start N processes linked one to
the other. In order to be able to pass the N-1 argument to the next
'chain' process (which calls `spawn/1`), I wrap the call inside an
anonymous function so it doesn't need arguments anymore. Calling
`spawn(?MODULE, chain, [N-1])` would have done a similar job.

Here, I'll have many processes linked together, dying as each of their
successors exits:


::

    
    4> c(linkmon).               
    {ok,linkmon}
    5> link(spawn(linkmon, chain, [3])).
    true
    ** exception error: "chain dies here"


And as you can see, the shell does receive the death signal from some
other process. Here's a drawn representation of the spawned processes
and links going down:


::

    
    [shell] == [3] == [2] == [1] == [0]
    [shell] == [3] == [2] == [1] == *dead*
    [shell] == [3] == [2] == *dead*
    [shell] == [3] == *dead*
    [shell] == *dead*
    *dead, error message shown*
    [shell] <-- restarted


After the process running `linkmon:chain(0)` dies, the error is
propagated down the chain of links until the shell process itself dies
because of it. The crash could have happened in any of the linked
processes; because links are bidirectional, you only need one of them
to die for the others to follow suit.

Note: If you wanted to kill another process from the shell, you could
use the function exit/2, which is called this way: `exit(Pid,
Reason)`. Try it if you wish.

Note: Links can not be stacked. If you call `link/1` 15 times for the
same two processes, only one link will still exist between them and a
single call to `unlink/1` will be enough to tear it down.

Its important to note that `link(spawn(Function))` or
`link(spawn(M,F,A))` happens in more than one step. In some cases, it
is possible for a process to die before the link has been set up and
then provoke unexpected behavior. For this reason, the function
spawn_link/1-3 has been added to the language. It takes the same
arguments as `spawn/1-3`, creates a process and links it as if
`link/1` had been there, except it's all done as an atomic operation
(the operations are combined as a single one, which can either fail or
succeed, but nothing else). This is generally considered safer and you
save a set of parentheses too.



It's a Trap!
~~~~~~~~~~~~


.. image:: ../images/ackbar.jpg
    :alt: Admiral Ackbar


Now to get back to links and processes dying. Error propagation across
processes is done through a process similar to message passing, but
with a special type of message called signals. Exit signals are
'secret' messages that automatically act on processes, killing them in
the action.

I have mentioned many times already that in order to be reliable, an
application needs to be able to both kill and restart a process
quickly. Right now, links are alright to do the killing part. What's
missing is the restarting.

In order to restart a process, we need a way to first know that it
died. This can be done by adding a layer on top of links (the
delicious frosting on the cake) with a concept called *system
processes*. System processes are basically normal processes, except
they can convert exit signals to regular messages. This is done by
calling `process_flag(trap_exit, true)` in a running process. Nothing
speaks as much as an example, so we'll go with that. I'll just redo
the chain example with a system process at the beginning:


::

    
    1> process_flag(trap_exit, true).
    true
    2> spawn_link(fun() -> linkmon:chain(3) end).
    <0.49.0>
    3> receive X -> X end.
    {'EXIT',<0.49.0>,"chain dies here"}


Ah! Now things get interesting. To get back to our drawings, what
happens is now more like this:


::

    
    [shell] == [3] == [2] == [1] == [0]
    [shell] == [3] == [2] == [1] == *dead*
    [shell] == [3] == [2] == *dead*
    [shell] == [3] == *dead*
    [shell] <-- {'EXIT,Pid,"chain dies here"} -- *dead*
    [shell] <-- still alive!


And this is the mechanism allowing for a quick restart of processes.
By writing programs using system processes, it is easy to create a
process whose only role is to check if something dies and then restart
it whenever it fails. We'll cover more of this in the next chapter,
when we really apply these techniques.

For now, I want to come back to the exception functions seen in the
`exceptions chapter`_ and show how they behave around processes that
trap exits. Let's first set the bases to experiment without a system
process. I'll successively show the results of uncaught throws, errors
and exits in neighboring processes:

:Exception source: `spawn_link(fun() -> ok end)`: Untrapped Result : -
  nothing -
Trapped Result : {'EXIT', <0.61.0>, normal}
The process exited normally, without a problem. Note that this looks a
  bit like the result of `catch exit(normal)`, except a PID is added to
  the tuple to know what processed failed.
:Exception source: `spawn_link(fun() -> exit(reason) end)`: Untrapped
  Result : ** exception exit: reason
Trapped Result : {'EXIT', <0.55.0>, reason}
The process has terminated for a custom reason. In this case, if there
  is no trapped exit, the process crashes. Otherwise, you get the above
  message.
:Exception source: `spawn_link(fun() -> exit(normal) end)`: Untrapped
  Result : - nothing -
Trapped Result : {'EXIT', <0.58.0>, normal}
This successfully emulates a process terminating normally. In some
  cases, you might want to kill a process as part of the normal flow of
  a program, without anything exceptional going on. This is the way to
  do it.
:Exception source: `spawn_link(fun() -> 1/0 end)`: Untrapped Result :
  Error in process <0.44.0> with exit value: {badarith, [{erlang, '/',
  [1,0]}]}
Trapped Result : {'EXIT', <0.52.0>, {badarith, [{erlang, '/',
  [1,0]}]}}
The error ( `{badarith, Reason}`) is never caught by a `try ... catch`
  block and bubbles up into an 'EXIT' . At this point, it behaves
  exactly the same as `exit(reason)` did, but with a stack trace giving
  more details about what happened.
:Exception source: `spawn_link(fun() -> erlang:error(reason) end)`:
  Untrapped Result : Error in process <0.47.0> with exit value: {reason,
  [{erlang, apply, 2}]}
Trapped Result : {'EXIT', <0.74.0>, {reason, [{erlang, apply, 2}]}}
Pretty much the same as with `1/0`. That's normal, `erlang:error/1` is
  meant to allow you to do just that.
:Exception source: `spawn_link(fun() -> throw(rocks) end)`: Untrapped
  Result : Error in process <0.51.0> with exit value: {{nocatch, rocks},
  [{erlang, apply, 2}]}
Trapped Result : {'EXIT', <0.79.0>, {{nocatch, rocks}, [{erlang,
  apply, 2}]}}
Because the `throw` is never caught by a `try ... catch`, it bubbles
  up into an error, which in turn bubbles up into an EXIT . Without
  trapping exit, the process fails. Otherwise it deals with it fine.


And that's about it for usual exceptions. Things are normal:
everything goes fine. Exceptional stuff happens: processes die,
different signals are sent around.

Then there's `exit/2`. This one is the Erlang process equivalent of a
gun. It allows a process to kill another one from a distance, safely.
Here are some of the possible calls:

:Exception source: `exit(self(), normal)`: Untrapped Result : **
  exception exit: normal
Trapped Result : {'EXIT', <0.31.0>, normal}
When not trapping exits, `exit(self(), normal)` acts the same as
  `exit(normal)`. Otherwise, you receive a message with the same format
  you would have had by listening to links from foreign processes dying.
:Exception source: `exit(spawn_link(fun() -> timer:sleep(50000) end),
  normal)`: Untrapped Result : - nothing -
Trapped Result : - nothing -
This basically is a call to `exit(Pid, normal)`. This command doesn't
  do anything useful, because a process can not be remotely killed with
  the reason `normal` as an argument.
:Exception source: `exit(spawn_link(fun() -> timer:sleep(50000) end),
  reason)`: Untrapped Result : ** exception exit: reason
Trapped Result : {'EXIT', <0.52.0>, reason}
This is the foreign process terminating for reason itself. Looks the
  same as if the foreign process called `exit(reason)` on itself.
:Exception source: `exit(spawn_link(fun() -> timer:sleep(50000) end),
  kill)`: Untrapped Result : ** exception exit: killed
Trapped Result : {'EXIT', <0.58.0>, killed}
Surprisingly, the message gets changed from the dying process to the
  spawner. The spawner now receives `killed` instead of `kill`. That's
  because `kill` is a special exit signal. More details on this later.
:Exception source: `exit(self(), kill)`: Untrapped Result : **
  exception exit: killed
Trapped Result : ** exception exit: killed
Oops, look at that. It seems like this one is actually impossible to
  trap. Let's check something.
:Exception source: `spawn_link(fun() -> exit(kill) end)`: Untrapped
  Result : ** exception exit: killed
Trapped Result : {'EXIT', <0.67.0>, kill}
Now that's getting confusing. When another process kills itself with
  `exit(kill)` and we don't trap exits, our own process dies with the
  reason `killed`. However, when we trap exits, things don't happen that
  way.


While you can trap most exit reasons, there are situations where you
might want to brutally murder a process: maybe one of them is trapping
exits but is also stuck in an infinite loop, never reading any
message. The `kill` reason acts as a special signal that can't be
trapped. This ensures any process you terminate with it will really be
dead. Usually, `kill` is a bit of a last resort, when everything else
has failed.


.. image:: ../images/trap.png
    :alt: A mouse trap with a beige laptop on top


As the `kill` reason can never be trapped, it needs to be changed to
`killed` when other processes receive the message. If it weren't
changed in that manner, every other process linked to it would in turn
die for the same `kill` reason and would in turn kill its neighbors,
and so on. A death cascade would ensue.

This also explains why `exit(kill)` looks like `killed` when received
from another linked process (the signal is modified so it doesn't
cascade), but still looks like `kill` when trapped locally.

If you find this all confusing, don't worry. Many programmers feel the
same. Exit signals are a bit of a funny beast. Luckily there aren't
many more special cases than the ones described above. Once you
understand those, you can understand most of Erlang's concurrent error
management without a problem.



Monitors
~~~~~~~~

So yeah. Maybe murdering processes isn't what you want. Maybe you
don't feel like taking the world down with you once you're gone. Maybe
you're more of a stalker. In that case, monitors might be what you
want.

More seriously, monitors are a special type of link with two
differences:


+ they are unidirectional;
+ they can be stacked.



.. image:: ../images/homer.png
    :alt: Ugly Homer Simpson parody


Monitors are what you want when a process wants to know what's going
on with a second process, but neither of them really are vital to each
other.

Another reason, as listed above, is stacking the references. Now this
might seem useless from a quick look, but it is great for writing
libraries which need to know what's going on with other processes.

You see, links are more of an organizational construct. When you
design the architecture of your application, you determine which
process will do which jobs, and what will depend on what. Some
processes will supervise others, some couldn't live without a twin
process, etc. This structure is usually something fixed, known in
advance. Links are useful for that and should not necessarily be used
outside of it.

But what happens if you have 2 or 3 different libraries that you call
and they all need to know whether a process is alive or not? If you
were to use links for this, you would quickly hit a problem whenever
you needed to unlink a process. Now, links aren't stackable, so the
moment you unlink one, you unlink them all and mess up all the
assumptions put up by the other libraries. That's pretty bad. So you
need stackable links, and monitors are your solution. They can be
removed individually. Plus, being unidirectional is handy in libraries
because other processes shouldn't have to be aware of said libraries.

So what does a monitor look like? Easy enough, let's set one up. The
function is erlang:monitor/2, where the first argument is the atom
process and the second one is the pid:


::

    
    1> erlang:monitor(process, spawn(fun() -> timer:sleep(500) end)).
    #Ref<0.0.0.77>
    2> flush().
    Shell got {'DOWN',#Ref<0.0.0.77>,process,<0.63.0>,normal}
    ok


Every time a process you monitor goes down, you will receive such a
message. The message is `{'DOWN', MonitorReference, process, Pid,
Reason}`. The reference is there to allow you to demonitor the
process. Remember, monitors are stackable, so it's possible to take
more than one down. References allow you to track each of them in a
unique manner. Also note that as with links, there is an atomic
function to spawn a process while monitoring it, spawn_monitor/1-3:


::

    
    3> {Pid, Ref} = spawn_monitor(fun() -> receive _ -> exit(boom) end end).
    {<0.73.0>,#Ref<0.0.0.100>}
    4> erlang:demonitor(Ref).
    true
    5> Pid ! die.
    die
    6> flush().
    ok


In this case, we demonitored the other process before it crashed and
as such we had no trace of it dying. The function demonitor/2 also
exists and gives a little bit more information. The second parameter
can be a list of options. Only two exist, `info` and `flush`:


::

    
    7> f().
    ok
    8> {Pid, Ref} = spawn_monitor(fun() -> receive _ -> exit(boom) end end). 
    {<0.35.0>,#Ref<0.0.0.35>}
    9> Pid ! die.
    die
    10> erlang:demonitor(Ref, [flush, info]).
    false
    11> flush().
    ok


The `info` option tells you if a monitor existed or not when you tried
to remove it. This is why the expression 10 returned `false`. Using
`flush` as an option will remove the `DOWN` message from the mailbox
if it existed, resulting in `flush()` finding nothing in the current
process' mailbox.



Naming Processes
~~~~~~~~~~~~~~~~

With links and monitors understood, there is another problem still
left to be solved. Let's use the following functions of the
linkmon.erl module:


::

    
    start_critic() ->
        spawn(?MODULE, critic, []).
    
    judge(Pid, Band, Album) ->
        Pid ! {self(), {Band, Album}},
        receive
            {Pid, Criticism} -> Criticism
        after 2000 ->
            timeout
        end.
    
    critic() ->
        receive
            {From, {"Rage Against the Turing Machine", "Unit Testify"}} ->
                From ! {self(), "They are great!"};
            {From, {"System of a Downtime", "Memoize"}} ->
                From ! {self(), "They're not Johnny Crash but they're good."};
            {From, {"Johnny Crash", "The Token Ring of Fire"}} ->
                From ! {self(), "Simply incredible."};
            {From, {_Band, _Album}} ->
                From ! {self(), "They are terrible!"}
        end,
        critic().


Now we'll just pretend we're going around stores, shopping for music.
There are a few albums that sound interesting, but we're never quite
sure. You decide to call your friend, the critic.


::

    
    1> c(linkmon).                         
    {ok,linkmon}
    2> Critic = linkmon:start_critic().
    <0.47.0>
    3> linkmon:judge(Critic, "Genesis", "The Lambda Lies Down on Broadway").
    "They are terrible!"


Because of a solar storm (I'm trying to find something realistic
here), the connection is dropped:


::

    
    4> exit(Critic, solar_storm).
    true
    5> linkmon:judge(Critic, "Genesis", "A trick of the Tail Recursion").
    timeout


Annoying. We can no longer get criticism for the albums. To keep the
critic alive, we'll write a basic 'supervisor' process whose only role
is to restart it when it goes down:


::

    
    start_critic2() ->
        spawn(?MODULE, restarter, []).
    
    restarter() ->
        process_flag(trap_exit, true),
        Pid = spawn_link(?MODULE, critic, []),
        receive
            {'EXIT', Pid, normal} -> % not a crash
                ok;
            {'EXIT', Pid, shutdown} -> % manual termination, not a crash
                ok;
            {'EXIT', Pid, _} ->
                restarter()
        end.


Here, the restarter will be its own process. It will in turn start the
critic's process and if it ever dies of abnormal cause, `restarter/0`
will loop and create a new critic. Note that I added a clause for
`{'EXIT', Pid, shutdown}` as a way to manually kill the critic if we
ever need to.

The problem with our approach is that there is no way to find the Pid
of the critic, and thus we can't call him to have his opinion. One of
the solutions Erlang has to solve this is to give names to processes.

The act of giving a name to a process allows you to replace the
unpredictable pid by an atom. This atom can then be used exactly as a
Pid when sending messages. To give a process a name, the function
erlang:register/2 is used. If the process dies, it will automatically
lose its name or you can also use unregister/1 to do it manually. You
can get a list of all registered processes with registered/0 or a more
detailed one with the shell command `regs()`. Here we can rewrite the
`restarter/0` function as follows:


::

    
    restarter() ->
        process_flag(trap_exit, true),
        Pid = spawn_link(?MODULE, critic, []),
        register(critic, Pid),
        receive
            {'EXIT', Pid, normal} -> % not a crash
                ok;
            {'EXIT', Pid, shutdown} -> % manual termination, not a crash
                ok;
            {'EXIT', Pid, _} ->
                restarter()
        end. 


So as you can see, `register/2` will always give our critic the name
'critic', no matter what the Pid is. What we need to do is then remove
the need to pass in a Pid from the abstraction functions. Let's try
this one:


::

    
    judge2(Band, Album) ->
        critic ! {self(), {Band, Album}},
        Pid = whereis(critic),
        receive
            {Pid, Criticism} -> Criticism
        after 2000 ->
            timeout
        end.


Here, the line `Pid = whereis(critic)` is used to find the critic's
process identifier in order to pattern match against it in the
`receive` expression. We want to match with this pid, because it makes
sure we will match on the right message (there could be 500 of them in
the mailbox as we speak!) This can be the source of a problem though.
The code above assumes that the critic's pid will remain the same
between the first two lines of the function. However, it is completely
plausible the following will happen:


::

    
      1. critic ! Message
                            2. critic receives
                            3. critic replies
                            4. critic dies
      5. whereis fails
                            6. critic is restarted
      7. code crashes


Or yet, this is also a possibility:


::

    
      1. critic ! Message
                               2. critic receives
                               3. critic replies
                               4. critic dies
                               5. critic is restarted
      6. whereis picks up
         wrong pid
      7. message never matches


The possibility that things go wrong in a different process can make
another one go wrong if we don't do things right. In this case, the
value of the critic atom can be seen from multiple processes. This is
known as *shared state*. The problem here is that the value of critic
can be accessed *and* modified by different processes at virtually the
same time, resulting in inconsistent information and software errors.
The common term for such things is a *race condition*. Race conditions
are particularly dangerous because they depend on the timing of
events. In pretty much every concurrent and parallel language out
there, this timing depends on unpredictable factors such as how busy
the processor is, where the processes go, and what data is being
processed by your program.

Don't drink too much kool-aid:
You might have heard that Erlang is usually free of race conditions or
deadlocks and makes parallel code safe. This is true in many
circumstances, but never assume your code is really that safe. Named
processes are only one example of the multiple ways in which parallel
code can go wrong.

Other examples include access to files on the computer (to modify
them), updating the same database records from many different
processes, etc.

Luckily for us, it's relatively easy to fix the code above if we don't
assume the named process remains the same. Instead, we'll use
references (created with `make_ref()`) as unique values to identify
messages. We'll need to rewrite the `critic/0` function into
`critic2/0` and `judge/3` into `judge2/2`:


::

    
    judge2(Band, Album) ->
        Ref = make_ref(),
        critic ! {self(), Ref, {Band, Album}},
        receive
            {Ref, Criticism} -> Criticism
        after 2000 ->
            timeout
        end.
    
    critic2() ->
        receive
            {From, Ref, {"Rage Against the Turing Machine", "Unit Testify"}} ->
                From ! {Ref, "They are great!"};
            {From, Ref, {"System of a Downtime", "Memoize"}} ->
                From ! {Ref, "They're not Johnny Crash but they're good."};
            {From, Ref, {"Johnny Crash", "The Token Ring of Fire"}} ->
                From ! {Ref, "Simply incredible."};
            {From, Ref, {_Band, _Album}} ->
                From ! {Ref, "They are terrible!"}
        end,
        critic2().


And then change `restarter/0` to fit by making it spawn `critic2/0`
rather than `critic/0`. Now the other functions should keep working
fine. The user won't see a difference. Well, they will because we
renamed functions and changed the number of parameters, but they won't
know what implementation details were changed and why it was
important. All they'll see is that their code got simpler and they no
longer need to send a pid around function calls:


::

    
    6> c(linkmon).
    {ok,linkmon}
    7> linkmon:start_critic2().
    <0.55.0>
    8> linkmon:judge2("The Doors", "Light my Firewall").
    "They are terrible!"
    9> exit(whereis(critic), kill).
    true
    10> linkmon:judge2("Rage Against the Turing Machine", "Unit Testify").     
    "They are great!"


And now, even though we killed the critic, a new one instantly came
back to solve our problems. That's the usefulness of named processes.
Had you tried to call `linkmon:judge/2` without a registered process,
a bad argument error would have been thrown by the `!` operator inside
the function, making sure that processes that depend on named ones
can't run without them.

Note: If you remember earlier texts, atoms can be used in a limited
(though high) number. You shouldn't ever create dynamic atoms. This
means naming processes should be reserved to important services unique
to an instance of the VM and processes that should be there for the
whole time your application runs.

If you need named processes but they are transient or there isn't any
of them which can be unique to the VM, it may mean they need to be
represented as a group instead. Linking and restarting them together
if they crash might be the sane option, rather than trying to use
dynamic names.

In the next chapter, we'll put the recent knowledge we gained on
concurrent programming with Erlang to practice by writing a real
application.





.. _exceptions chapter: errors-and-exceptions.html


