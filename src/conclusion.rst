


Conclusion
----------



A Few Words
~~~~~~~~~~~

I see you chose to read the conclusion after all. Good for you. Before
I get to point you to a bunch of interesting topics that you could
explore, were you to pick Erlang as a development language you want to
learn more about, I'd like to take a few lines to say writing Learn
You Some Erlang has been one hell of a ride. It took me three years of
hard work while studying and working full time, and juggling every
day's life needs (if I had children, they'd have died of neglect a
while ago now).

This site, coupled with some luck and some more work, allowed me to
get jobs, both as an Erlang trainer, course material writer, and
developer. It allowed me to travel around the world and meet a
crapload of interesting people. It drained a lot of energy, cost me a
decent chunk of money and time to run, but it paid back tenfold in
most ways imaginable.

I have to give a lot of thanks to the Erlang community in general.
They helped me learn stuff, reviewed pages and pages of material for
free, fixed my typoes, helped me get better at writing English and
writing in General. There's been dozen of people helping in many ways.
The biggest contributors in terms of time, advice, and general
resources are all in this site's FAQ. If you've ever written me an
e-mail telling me you'd buy me a beer, buy it to one of these guys
instead; they deserve it as their participation was way more thankless
than mine.

The Erlang community as a whole has been very welcoming to the work
I've been doing with LYSE, helped make it known to readers (it's even
on the official Erlang documentation and website!). Without the
concerted efforts of everyone around Erlang, this site would probably
have died after four or five chapters, left to be yet another useless
website clogging the Internet's tubes. So, hey, thanks.



Other Topics
~~~~~~~~~~~~

There's only so many topics I could cover without going over the top.
This site, if it were to be turned in dead tree form, would probably
yield around 600 pages now. It's taken three years to bring it there,
and I'm tired and glad it's over (what am I gonna do with all that
free time, now?), but there are still plenty of topics I would have
*loved* to cover. Here's a quick list:



Tracing BIFs and DBG
````````````````````

The Erlang VM is traceable inside and out. Got a bug or some stack
trace you can't make sense of? Turn on a few trace flags and the VM
opens up to you. DBG takes these BIFs and builds an app on top of
them. Messages, function calls, function returns, garbage collections,
process spawning and dying, etc. It's all traceable and observable. It
also tends to work much better than any debugger for a concurrent
language like Erlang. The best about it? It's traceable within Erlang,
so you can make Erlang programs that trace themselves! If you look
into them and find them a bit hard to digest, you might be okay
staying with the `sys` module's tracing functions. They work only on
OTP behaviourised processes, but they're often good enough to get
going.



Profiling
`````````

Erlang comes with a good bunch of different profiling tools to analyze
your programs and find all kinds of bottlenecks. The fprof and eprof
tools can be used for time profiling, cprof for function calls, lcnt
for locks, percept for concurrency, and cover for code coverage. Most
of them are built using the tracing BIFs of the language, funnily
enough.



More Introspection
``````````````````

Top-like tools exist for Erlang, such as pman and etop. You can also
use the Erlang debugger, but I do recommend DBG instead of that one.
To explore entire supervision trees for your nodes, appmon is your
app.



Documentation
`````````````

EDoc is a tool that lets you turn your Erlang modules into HTML
documentation. It supports annotations and ways to declare specific
pages that allow you to build small websites to document your code.
It's similar to Javadoc, if you've heard of it.



GUIs
````

The Wx application is the new standard for multiplatform GUI writing
with Erlang. I'm terrible at GUI stuff, so it's probably better for
everyone I actually didn't cover that one.



Other Useful Libraries
``````````````````````

There are plenty of nice libraries coming by default with Erlang not
mentioned here. Cryptography tools, web servers, web clients, all
kinds of protocol implementations, and so on. You can get a general
list of them at http://www.erlang.org/doc/applications.html.



Community libraries
```````````````````

There is a crapload of them. I didn't want to cover them because they
can tend to change and I didn't want to favor one over the other, but
here's a quick list: Rebar and Sinan if you want build systems, redbug
for a friendlier approach to tracing, gproc for a very powerful and
flexible process registry, mochiweb, cowboy and yaws if you need web
servers, riak_core for a very powerful distribution library for
Erlang, lhttpc as a web client, PropEr, Quickcheck and Triq for kick-
ass property-based testing tools (you *need* to try one of them),
entop for a top-like tool, a billion JSON libraries (mochijson2, jsx,
ejson, etc.), UX for Unicode handling and common algorithm pending
R16B, Seresye and exat for some AI tools, database client libraries,
lager as very robust logging system that binds itself to Erlang's
error_logger, poolboy for some generic message-based pool, and a whole
lot more stuff. Community libraries could easily get their own book.



I heard LYSE is also a book?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You heard right.Thanks to No Starch Press, Learn You Some Erlang is
available both as a dead tree book and an ebook! At a large 600 black
and white pages, including images (in color for ebook copies), you can
now have the largest Erlang-themed paperweight and bookcase decoration
printed to date (as far as I know). This should ease the sharp pain of
reading hundreds of pages on a computer screen.



Your ideas are intriguing to me and I wish to subscribe to your
newsletter
~~~~~~~~~~

I have a blog at ferd.ca where I discuss all kinds of stuff (or at
least I want to), but inevitably come back to Erlang topics, due to
using it all the time.



Is that it?
~~~~~~~~~~~

Yes, it is. Have a nice day!



