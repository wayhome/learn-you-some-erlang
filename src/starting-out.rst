


Starting Out
------------



The Shell
~~~~~~~~~

In Erlang, you can test most of your stuff in an emulator; it will run
your scripts when compiled and deployed, but it will also let you edit
stuff live. To start the shell in Linux, open a terminal and then type
in `$ erl`. If you've set up everything fine, you should see text like
this:

::

    
    Erlang R13B01 (erts-5.7.2) [source] [smp:2:2] [rq:2] [async-threads:0] [hipe] [kernel-poll:false]
    
    Eshell V5.7.2  (abort with ^G)


Congratulations, you're running the Erlang shell!

For Windows users, you can still run the `erl.exe` shell, but it's
recommended you instead use `werl.exe`, which can be found in your
start menu ( `programs > Erlang`). Werl is a windows-only
implementation of the Erlang shell, having its own window with
scrollbars and supporting command-line editing (like copy-pasting,
which got to be a pain with the standard `cmd.exe` shell in Windows).
The erl shell is still required if you want to redirect standard input
or output, or use pipelines.

We'll be able to enter and run code in the emulator, but first, let's
see how we can get around in it.



Shell Commands
~~~~~~~~~~~~~~

The Erlang shell has a built-in line editor based on a subset of
Emacs, a popular text editor that's been in use since the 70s. If you
know Emacs, you should be fine. For the others, you'll do fine anyway.


.. image:: ../images/shell.png
    :alt: super turtle


First of all, if you type some text and then go `^A` (Ctrl+A), you
should see your cursor moving to the beginning of the line. `^E`
(Ctrl+E) gets you to the end. You can use arrow keys to go forward,
backwards, show previous or next lines so you can repeat code.

If you type something like `li` and then press "tab", the shell will
have completed the terms for you to `lists:`. Press tab again, and the
shell will suggest you many functions to use after. This is Erlang
completing the module `lists` and then suggesting functions from it.
You may find the notation weird, but don't worry, you'll get familiar
with it soon enough.

I think we've seen enough of shell functionality to be alright, except
for one thing: we don't know how to leave! There's a fast way to find
how. Just type in `help().` and you should get information on a bunch
of commands you can use in the shell (do not forget the full stop (
`.`) as it is necessary for the command to run). We'll use some of
them at a later point, but the only line of concern to us in order to
get out is
`q() -- quit - shorthand for init:stop()`
So this is one way to do it (in fact, two ways). But this won't help
us if the shell freezes! If you were paying attention, when you
started the shell, there was a comment about 'aborting with `^G`'.
Let's do that, and then press `h` to get help!

::

    
    User switch command
     --> h
      c [nn]            - connect to job
      i [nn]            - interrupt job
      k [nn]            - kill job
      j                 - list all jobs
      s [shell]         - start local shell
      r [node [shell]]  - start remote shell
      q        - quit erlang
      ? | h             - this message
     -->


If you type in `i` then `c`, Erlang should stop the currently running
code and bring you back to a responsive shell. `j` will give you a
list of processes running (a star after a number indicates this is the
job you are currently running), which you can then interrupt with `i`
followed by the number. If you use `k`, you will kill the shell as it
is instead of just interrupting it. Press `s` to start a new one.


::

    
     Eshell V5.7.2  (abort with ^G)
    1> "OH NO THIS SHELL IS UNRESPONSIVE!!! *hits ctrl+G*"
    User switch command
     --> k
     --> c
    Unknown job
     --> s
     --> j
       2* {shell,start,[]}
     --> c 2
    Eshell V5.7.2  (abort with ^G)
    1> "YESS!"


If you read back the help text, you'll notice we can start remote
shells. I won't get into details right now, but this should give you
an idea of what the Erlang VM can do apart from running code. For now,
let's get things started (for real).




