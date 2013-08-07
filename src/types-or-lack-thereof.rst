


Types (or lack thereof)
-----------------------



Dynamite-strong Typing
~~~~~~~~~~~~~~~~~~~~~~

As you might have noticed when typing in examples from `Starting Out
(for real)`_, and then modules and functions from `Modules`_ and
`Syntax in Functions`_, we never needed to write the type of a
variable or the type of a function. When pattern matching, the code we
had written didn't have to know what it would be matched against. The
tuple `{ X , Y }` could be matched with `{atom, 123}` as well as `{"A
string", <<"binary stuff!">>}`, `{2.0, ["strings","and",atoms]}` or
really anything at all.

When it didn't work, an error was thrown in your face, but only once
you ran the code. This is because Erlang is *dynamically typed*: every
error is caught at runtime and the compiler won't always yell at you
when compiling modules where things may result in failure, like in
`Starting Out (for real)`_'s `"llama + 5"` example.


.. image:: ../images/ham.png
    :alt: A knife slicing ham.


One classic friction point between proponents of static and dynamic
typing has to do with the safety of the software being written. A
frequently suggested idea is that good static type systems with
compilers enforcing them with fervor will catch most errors waiting to
happen before you can even execute the code. As such, statically typed
languages are to be seen as safer than their dynamic counterparts.
While this might be true when comparing with many dynamic languages,
Erlang begs to differ and certainly has a track record to prove it.
The best example is the often reported *nine nines* (99.9999999%) of
availability offered on the Ericsson AXD 301 ATM switches, consisting
of over 1 million lines of Erlang code. Please note that this is not
an indication that none of the components in an Erlang-based system
failed, but that a general switch system was available 99.9999999% of
the time, planned outages included. This is partially because Erlang
is built on the notion that a failure in one of the components should
not affect the whole system. Errors coming from the programmer,
hardware failures or [some] network failures are accounted for: the
language includes features which will allow you to distribute a
program over to different nodes, handle unexpected errors, and *never*
stop running.

To make it short, while most languages and type systems aim to make a
program error-free, Erlang uses a strategy where it is assumed that
errors will happen anyway and makes sure to cover these cases:
Erlang's dynamic type system is not a barrier to reliability and
safety of programs. This sounds like a lot of prophetic talking, but
you'll see how it's done in the later chapters.

Note: Dynamic typing was historically chosen for simple reasons; those
who implemented Erlang at first mostly came from dynamically typed
languages, and as such, having Erlang dynamic was the most natural
option to them.

Erlang is also strongly typed. A weakly typed language would do
implicit type conversions between terms. If Erlang were to be weakly
typed we could possibly do the operation `6 = 5 + "1".` while in
practice, an exception for bad arguments will be thrown:


::

    
    1> 6 + "1".
    ** exception error: bad argument in an arithmetic expression
         in operator  +/2
            called as 6 + "1"


Of course, there are times when you could want to convert one kind of
data to another one: changing regular strings into bit strings to
store them or an integer to a floating point number. The Erlang
standard library provides a number of functions to do it.



Type conversions
~~~~~~~~~~~~~~~~

Erlang, like many languages, changes the type of a term by casting it
into another one. This is done with the help of built-in functions, as
many of the conversions could not be implemented in Erlang itself.
Each of these functions take the form <type>_to_<type> and are
implemented in the `erlang` module. Here are a few of them:


::

    
    1> erlang:list_to_integer("54").
    54
    2> erlang:integer_to_list(54).
    "54"
    3> erlang:list_to_integer("54.32").
    ** exception error: bad argument
         in function  list_to_integer/1
            called as list_to_integer("54.32")
    4> erlang:list_to_float("54.32").
    54.32
    5> erlang:atom_to_list(true).
    "true"
    6> erlang:list_to_bitstring("hi there").
    <<"hi there">>
    7> erlang:bitstring_to_list(<<"hi there">>).
    "hi there"


And so on. We're hitting on a language wart here: because the scheme
<type>_to_<type> is used, every time a new type is added to the
language, a whole lot of conversion BIFs need to be added! Here's the
whole list already there:

`atom_to_binary/2, atom_to_list/1, binary_to_atom/2,
binary_to_existing_atom/2, binary_to_list/1, bitstring_to_list/1,
binary_to_term/1, float_to_list/1, fun_to_list/1, integer_to_list/1,
integer_to_list/2, iolist_to_binary/1, iolist_to_atom/1,
list_to_atom/1, list_to_binary/1, list_to_bitstring/1,
list_to_existing_atom/1, list_to_float/1, list_to_integer/2,
list_to_pid/1, list_to_tuple/1, pid_to_list/1, port_to_list/1,
ref_to_list/1, term_to_binary/1, term_to_binary/2 and
tuple_to_list/1.`

That's a lot of conversion functions. We'll see most if not all of
these types through this book, although we probably won't need all of
these functions.



To Guard a Data Type
~~~~~~~~~~~~~~~~~~~~

Erlang basic data types are easy to spot, visually: tuples have the
curly brackets, lists the square brackets, strings are enclosed in
double quotation marks, etc. Enforcing a certain data type has thus
been possible with pattern matching: a function `head/1` taking a list
could only accept lists because otherwise, the matching ( `[H|_]`)
would have failed.


.. image:: ../images/my-name-is.png
    :alt: Hi, My name is Tuple


However, we've had a problem with numeric values because we couldn't
specify ranges. Consequently, we used guards in functions about
temperature, the age to drive, etc. We're hitting another roadblock
now. How could we write a guard that ensures that patterns match
against data of a single specific type, like numbers, atoms or
bitstrings?

There are functions dedicated to this task. They will take a single
argument and return true if the type is right, false otherwise. They
are part of the few functions allowed in guard expressions and are
named the *type test BIFs*:


::

    
    is_atom/1           is_binary/1         
    is_bitstring/1      is_boolean/1        is_builtin/3        
    is_float/1          is_function/1       is_function/2       
    is_integer/1        is_list/1           is_number/1         
    is_pid/1            is_port/1           is_record/2         
    is_record/3         is_reference/1      is_tuple/1          


They can be used like any other guard expression, wherever guard
expressions are allowed. You might be wondering why there is no
function just giving the type of the term being evaluated (something
akin to `type_of(X) -> Type`). The answer is pretty simple. Erlang is
about programming for the right cases: you only program for what you
know will happen and what you expect. Everything else should cause
errors as soon as possible. Although this might sound insane, the
explanations you'll get in `Errors and Exceptions`_ will hopefully
make things clearer. Until then, just trust me on that.

Note: type test BIFs constitute more than half of the functions
allowed in guard expressions. The rest are also BIFs, but do not
represent type tests. These are:
`abs(Number), bit_size(Bitstring), byte_size(Bitstring), element(N,
Tuple), float(Term), hd(List), length(List), node(),
node(Pid|Ref|Port), round(Number), self(), size(Tuple|Bitstring),
tl(List), trunc(Number), tuple_size(Tuple).`

The functions `node/1` and `self/0` are related to distributed Erlang
and processes/actors. We'll eventually use them, but we've still got
other topics to cover before then.

It may seem like Erlang data structures are relatively limited, but
lists and tuples are usually enough to build other complex structures
without worrying about anything. As an example the basic node of a
binary tree could be represented as `{node, Value, Left, Right}`,
where Left and Right are either similar nodes or empty tuples. I could
also represent myself as:


::

    
    {person, {name, <<"Fred T-H">>},
             {qualities, ["handsome", "smart", "honest", "objective"]},
             {faults, ["liar"]},
             {skills, ["programming", "bass guitar", "underwater breakdancing"]}}.


Which shows that by nesting tuples and list and filling them with
data, we can obtain complex data structures and build functions to
operate on them.

Update:
The release R13B04 saw the addition of the BIF `binary_to_term/2`,
which lets you unserialize data the same way `binary_to_term/1` would,
except the second argument is an option list. If you pass in `[safe]`,
the binary won't be decoded if it contains unknown atoms or `anonymous
functions`_, which could exhaust memory.



For Type Junkies
~~~~~~~~~~~~~~~~


.. image:: ../images/type-dance.png
    :alt: A sign for homeless people: 'Will dance for types'


This section is meant to be read by programmers who can not live
without a static type system for one reason or another. It will
include a little bit more advanced theory and everything may not be
understood by everyone. I will briefly describe tools used to do
static type analysis in Erlang, defining custom types and getting more
safety that way. These tools will be described for anyone to
understand much later in the book, given that it is not necessary to
use any of them to write reliable Erlang programs. Because we'll show
them later, I'll give very little details about installing, running
them, etc. Again, this section is for those who really can't live
without advanced type systems.

Through the years, there were some attempts to build type systems on
top of Erlang. One such attempt happened back in 1997, conducted by
Simon Marlow, one of the lead developers of the Glasgow Haskell
Compiler, and Philip Wadler, who worked on Haskell's design and has
contributed to the theory behind monads (Read the paper on said type
system). Joe Armstrong later commented on the paper:

One day Phil phoned me up and announced that a) Erlang needed a type
system, b) he had written a small prototype of a type system and c) he
had a one year’s sabbatical and was going to write a type system for
Erlang and “were we interested?” Answer —“Yes.”

Phil Wadler and Simon Marlow worked on a type system for over a year
and the results were published in [20]. The results of the project
were somewhat disappointing. To start with, only a subset of the
language was type-checkable, the major omission being the lack of
process types and of type checking inter-process messages.

Processes and messages both being one of the core features of Erlang,
it may explain why the system was never added to the language. Other
attempts at typing Erlang failed. The efforts of the HiPE project
(attempts to make Erlang's performances much better) produced
Dialyzer, a static analysis tool still in use today, with its very own
type inference mechanism.

The type system that came out of it is based on success typings, a
concept different from Hindley-Milner or soft-typing type systems.
Success types are simple in concept: the type-inference will not try
to find the exact type of every expression, but it will guarantee that
the types it infers are right, and that the type errors it finds are
really errors.

The best example would come from the implementation of the function
`and`, which will usually take two Boolean values and return 'true' if
they're both true, 'false' otherwise. In Haskell's type system, this
would be written `and :: bool -> bool -> bool`. If the `and` function
had to be implemented in Erlang, it could be done the following way:


::

    
    and(false, _) -> false;
    and(_, false) -> false;
    and(true,true) -> true.


Under success typing, the inferred type of the function would be
`and(_,_) -> bool()`, where _ means 'anything'. The reason for this is
simple: when running an Erlang program and calling this function with
the arguments `false` and `42`, the result would still be 'false'. The
use of the `_` wildcard in pattern matching made it that in practice,
any argument can be passed as long as one of them is 'false' for the
function to work. ML types would have thrown a fit (and its users had
a heart attack) if you had called the function this way. Not Erlang.
It might make more sense to you if you decide to read the paper on the
implementation of success types, which explains the rationale behind
the behavior. I really encourage any type junkies out there to read
it, it's an interesting and practical implementation definition.

The details about type definitions and function annotations are
described in the Erlang Enhancement Proposal 8 (EEP 8). If you're
interested in using success typings in Erlang, check out the TypEr
application and Dialyzer, both part of the standard distribution. To
use them, type in `$ typer --help` and `$ dialyzer --help` (
`typer.exe --help` and `dialyzer.exe --help` for Windows, if they're
accessible from the directory you are currently in).

TypEr will be used to generate type annotations for functions. Used on
this small FIFO implementation, it spits the following type
annotations:


::

    
    %% File: fifo.erl
    %% --------------
    -spec new() -> {'fifo',[],[]}.
    -spec push({'fifo',_,_},_) -> {'fifo',nonempty_maybe_improper_list(),_}.
    -spec pop({'fifo',_,maybe_improper_list()}) -> {_,{'fifo',_,_}}.
    -spec empty({'fifo',_,_}) -> bool().



.. image:: ../images/fifo.png
    :alt: Implementation of fifo (queues): made out of two stacks (last-in first-out).


Which is pretty much right. Improper lists should be avoided because
`lists:reverse/1` doesn't support them, but someone bypassing the
module's interface would be able to get through it and submit one. In
this case, the functions `push/2` and `pop/2` might still succeed for
a few calls before they cause an exception. This either tells us to
add guards or refine our type definitions manually. Suppose we add the
signature `-spec push({fifo,list(),list()},_) ->
{fifo,nonempty_list(),list()}.` and a function that passes an improper
list to `push/2` to the module: when scanning it in Dialyzer (which
checks and matches the types), the error message "The call
fifo:push({fifo,[1|2],[]},3) breaks the contract '<Type definition
here>' is output.

Dialyzer will complain only when code will break other code, and if it
does, it'll usually be right (it will complain about more stuff too,
like clauses that will never match or general discrepancies).
Polymorphic data types are also possible to write and analyze with
Dialyzer: the `hd()` function could be annotated with `-spec([A]) ->
A.` and be analyzed correctly, although Erlang programmers seem to
rarely use this type syntax.

Don't drink too much Kool-Aid:
Some of the things you can't expect Dialyzer and TypEr to do is type
classes with constructors, first order types and recursive types. The
types of Erlang are only annotations without effects or restrictions
on actual compiling unless you enforce them yourself. The type checker
will never tell you a program that can run right now (or has run for
two years) has a type bug when it effectively causes no error when
running (although you could have buggy code running correctly...)

While recursive types are something that would be really interesting
to have, they're unlikely to ever appear in the current forms of TypEr
and Dialyzer (the paper above explains why). Defining your own types
to simulate recursive types by adding one or two levels manually is
the best you can do at the moment.

It's certainly not a full-blown type system, not as strict or powerful
as what languages like Scala, Haskell or Ocaml propose. Its warning
and error messages are also usually a bit cryptic and not really user
friendly. However, it's still a very good compromise if you really
can't live in a dynamic world or wish for additional safety; just
expect it to be a tool in your arsenal, not too much more.

Update:
Since version R13B04, recursive types are now available as an
experimental feature for Dialyzer. This makes the previous *Don't
drink too much Kool-aid* partially wrong. Shame on me.

Note that the type documentation has also become official (although it
remains subject to change) and is more complete than what can be found
in EEP8.





.. _Syntax in Functions: syntax-in-functions.html
.. _Errors and Exceptions: errors-and-exceptions.html
.. _Modules: modules.html
.. _anonymous functions: higher-order-functions.html
.. _Starting Out (for real): starting-out-for-real.html
.. _Starting Out (for real): starting-out-for-real.html#bool-and-compare


