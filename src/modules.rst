


Modules
-------


What are modules
~~~~~~~~~~~~~~~~

Working with the interactive shell is often considered a vital part of
using dynamic programming languages. It is useful to test all kinds of
code and programs. Most of the basic data types of Erlang were used
without even needing to open a text editor or saving files. You could
drop your keyboard, go play ball outside and call it a day, but you
would be a terrible Erlang programmer if you stopped right there. Code
needs to be saved somewhere to be used!

This is what modules are for. Modules are a bunch of functions
regrouped in a single file, under a single name. Additionally, all
functions in Erlang must be defined in modules. You have already used
modules, perhaps without realizing it. The BIFs mentioned in the
previous chapter, like `hd` or `tl`, actually belong to the `erlang`
module, as well as all of the arithmetic, logic and Boolean operators.
BIFs from the `erlang` module differ from other functions as they are
automatically imported when you use Erlang. Every other function
defined in a module you will ever use needs to be called with the form
`Module:Function(Arguments)`.

You can see for yourself:


::

    
    1> erlang:element(2, {a,b,c}).
    b
    2> element(2, {a,b,c}).
    b
    3> lists:seq(1,4).
    [1,2,3,4]
    4> seq(1,4).
    ** exception error: undefined shell command seq/2


Here, the `seq` function from the list module was not automatically
imported, while `element` was. The error 'undefined shell command'
comes from the shell looking for a shell command like `f()` and not
being able to find it. There are some functions from the `erlang`
module which are not automatically imported, but they're not used too
frequently.

Logically, you should put functions about similar things inside a
single module. Common operations on lists are kept in the `lists`
module, while functions to do input and output (such as writing to the
terminal or in a file) are regrouped in the `io` module. One of the
only modules you will encounter which doesn't respect that pattern is
the aforementioned `erlang` module that has functions which do math,
conversions, deal with multiprocessing, fiddle with the virtual
machine's settings, etc. They have no point in common except being
built-in functions. You should avoid creating modules like `erlang`
and instead focus on clean logical separations.



Module Declaration
~~~~~~~~~~~~~~~~~~

When writing a module, you can declare two kinds of things:
*functions* and *attributes*. Attributes are metadata describing the
module itself such as its name, the functions that should be visible
to the outside world, the author of the code, and so on. This kind of
metadata is useful because it gives hints to the compiler on how it
should do its job, and also because it lets people retrieve useful
information from compiled code without having to consult the source.

There is a large variety of module attributes currently used in Erlang
code across the world; as a matter of fact, you can even declare your
own attributes for whatever you please. There are some pre-defined
attributes that will appear more frequently than others in your code.
All module attributes follow the form `-Name(Attribute).`. Only one of
them is necessary for your module to be compilable:

:-module(Name).: This is always the first attribute (and statement) of
  a file, and for good reason: it's the name of the current module,
  where Name is an `atom`_. This is the name you'll use to call
  functions from other modules. The calls are made with the `M:F(A)`
  form, where M is the module name, F the function, and A the arguments.


It's time to code already! Our first module will be very simple and
useless. Open your text editor and type in the following, then save it
under `useless.erl`:


::

    
    -module(useless).


This line of text is a valid module. Really! Of course it's useless
without functions. Let's first decide what functions will be exported
from our 'useless' module. To do this, we will use another attribute:

:-export([Function1/Arity, Function2/Arity, ..., FunctionN/Arity]).:
  This is used to define what functions of a module can be called by the
  outside world. It takes a list of functions with their respective
  arity. The arity of a function is an integer representing how many
  arguments can be passed to the function. This is critical information,
  because different functions defined within a module can share the same
  name if and only if they have a different arity. The functions
  `add(X,Y)` and `add(X,Y,Z)` would thus be considered different and
  written in the form `add/2` and `add/3` respectively.


Note: Exported functions represent a module's interface. It is
important to define an interface revealing strictly what is necessary
for it to be used and nothing more. Doing so lets you fiddle with all
the other [hidden] details of your implementation without breaking
code that might depend on your module.

Our useless module will first export a useful function named 'add',
which will take two arguments. The following `-export` attribute can
be added after the module declaration:


::

    
    -export([add/2]).


And now write the function:


::

    
    add(A,B) ->
        A + B.


The syntax of a function follows the form `Name(Args) -> Body.`, where
Name has to be an atom and Body can be one or more Erlang expressions
separated by commas. The function is ended with a period. Note that
Erlang doesn't use the 'return' keyword. 'Return' is useless! Instead,
the last logical expression of a function to be executed will have its
value returned to the caller automatically without you having to
mention it.

Add the following function (why yes, every tutorial needs a 'Hello
world' example! Even at the fourth chapter!), without forgetting to
add it to the `-export` attribute.


::

    
    %% Shows greetings.
    %% io:format/1 is the standard function used to output text.
    hello() ->
        io:format("Hello, world!~n").


What we see from this function is that comments are single-line only
and begin with a `%` sign (using `%%` is purely a question of style.)
The `hello/0` function also demonstrates how to call functions from
foreign modules inside yours. In this case, `io:format/1` is the
standard function to output text, as written in the comments.

A last function will be added to the module, using both functions
`add/2` and `hello/0`:


::

    
    greet_and_add_two(X) ->
    	hello(),
    	add(X,2).


Do not forget to add `greet_and_add_two/1` to the exported function
list. The calls to `hello/0` and `add/2` don't need to have the module
name prepended to them because they were declared in the module
itself.

Had you wanted to be able to call `io:format/1` in the same manner as
`add/2` or any other function defined within the module, you could
have added the following module attribute at the beginning of the
file: `-import(io, [format/1]).`. Then you could have called
`format("Hello, World!~n").` directly. More generally, the `-import`
attribute follows this recipe:


::

    
    -import(Module, [Function1/Arity, ..., FunctionN/Arity]).


Importing a function is not much more than a shortcut for programmers
when writing their code. Erlang programmers are often discouraged from
using the `-import` attribute as some people find it reduces the
readability of code. In the case of `io:format/2`, the function
`io_lib:format/2` also exists. Finding which one is used means going
to the top of the file to see from which module it was imported.
Consequently, leaving the module name in is considered good practice.
Usually, the only functions you'll see imported come from the lists
module: its functions are used with a higher frequency than those from
most other modules.

Your `useless` module should now look like the following file:


::

    
    -module(useless).
    -export([add/2, hello/0, greet_and_add_two/1]).
    
    add(A,B) ->
        A + B.
    
    %% Shows greetings.
    %% io:format/1 is the standard function used to output text.
    hello() ->
        io:format("Hello, world!~n").
    
    greet_and_add_two(X) ->
        hello(),
        add(X,2).


We are done with the "useless" module. You can save the file under the
name `useless.erl`. The file name should be the module name as defined
in the `-module` attribute, followed by '.erl', which is the standard
Erlang source extension.

Before showing how to compile the module and finally try all its
exciting functions, we will see how to define and use macros. Erlang
macros are really similar to C's '#define' statements, mainly used to
define short functions and constants. They are simple expressions
represented by text that will be replaced before the code is compiled
for the VM. Such macros are mainly useful to avoid having magic values
floating around your modules. A macro is defined as a module attribute
of the form: `-define(MACRO, some_value).` and is used as `?MACRO`
inside any function defined in the module. A 'function' macro could be
written as `-define(sub(X,Y), X-Y).` and used like `?sub(23,47)`,
later replaced by `23-47` by the compiler. Some people will use more
complex macros, but the basic syntax stays the same.



Compiling the code
~~~~~~~~~~~~~~~~~~

Erlang code is compiled to bytecode in order to be used by the virtual
machine. You can call the compiler from many places: `$ erlc flags
file.erl` when in the command line, `compile:file(FileName)` when in
the shell or in a module, `c()` when in the shell, etc.

It's time to compile our useless module and try it. Open the Erlang
shell, type in:


::

    
    1> cd("/path/to/where/you/saved/the-module/").
    "Path Name to the directory you are in"
    ok


By default, the shell will only look for files in the same directory
it was started in and the standard library: `cd/1` is a function
defined exclusively for the Erlang shell, telling it to change the
directory to a new one so it's less annoying to browse for our files.
Windows users should remember to use forward slashes. When this is
done, do the following:


::

    
    2> c(useless).
    {ok,useless}


If you have another message, make sure the file is named correctly,
that you are in the right directory and that you've made no mistake in
your module. Once you successfully compile code, you'll notice that a
`useless.beam` file was added next to `useless.erl` in your directory.
This is the compiled module. Let's try our first functions ever:


::

    
    3> useless:add(7,2).
    9
    4> useless:hello().
    Hello, world!
    ok
    5> useless:greet_and_add_two(-3).
    Hello, world!
    -1
    6> useless:not_a_real_function().
    ** exception error: undefined function useless:not_a_real_function/0


The functions work as expected: `add/2` adds numbers, `hello/0`
outputs "Hello, world!", and `greet_and_add_two/1` does both! Of
course, you might be asking why `hello/0` returns the atom 'ok' after
outputting text. This is because Erlang functions and expressions must
always return something, even if they would not need to in other
languages. As such, `io:format/1` returns 'ok' to denote a normal
condition, the absence of errors.

Expression 6 shows an error being thrown because a function doesn't
exist. If you have forgotten to export a function, this is the kind of
error message you will have when trying it out.

Note: If you were ever wondering, '.beam' stands for *Bogdan/Björn's
Erlang Abstract Machine*, which is the VM itself. Other virtual
machines for Erlang exist, but they're not really used anymore and are
history: JAM (Joe's Abstract Machine, inspired by Prolog's WAM and old
BEAM, which attempted to compile Erlang to C, then to native code.
Benchmarks demonstrated little benefits in this practice and the
concept was given up.

There are a whole lot of compilation flags existing to get more
control over how a module is compiled. You can get a list of all of
them in the Erlang documentation. The most common flags are:

:-debug_info: Erlang tools such as debuggers, code coverage and static
  analysis tools will use the debug information of a module in order to
  do their work.
:-{outdir,Dir}: By default, the Erlang compiler will create the 'beam'
  files in the current directory. This will let you choose where to put
  the compiled file.
:-export_all: Will ignore the `-export` module attribute and will
  instead export all functions defined. This is mainly useful when
  testing and developing new code, but should not be used in production.
:-{d,Macro} or {d,Macro,Value}: Defines a macro to be used in the
  module, where Macro is an atom. This is more frequently used when
  dealing when unit-testing, ensuring that a module will only have its
  testing functions created and exported when they are explicitly
  wanted. By default, Value is 'true' if it's not defined as the third
  element of the tuple.


To compile our `useless` module with some flags, we could do one of
the following:


::

    
    7> compile:file(useless, [debug_info, export_all]).
    {ok,useless}
    8> c(useless, [debug_info, export_all]).
    {ok,useless}


You can also be sneaky and define compile flags from within a module,
with a module attribute. To get the same results as from expressions 7
and 8, the following line could be added to the module:


::

    
    -compile([debug_info, export_all]).


Then just compile and you'll get the same results as if you manually
passed flags. Now that we're able to write down functions, compile
them and execute them, it's time to see how far we can take them!

Note: another option is to compile your Erlang module to native code.
Native code compiling is not available for every platform and OS, but
on those that support it, it can make your programs go faster (about
20% faster, based on anecdotal evidence). To compile to native code,
you need to use the `hipe` module and call it the following way:
`hipe:c(Module,OptionsList).` You could also use
`c(Module,[{hipe,o3}]).` when in the shell to achieve similar results.
Note that the .beam file generated will no longer be portable across
platforms like regular ones.



More About Modules
~~~~~~~~~~~~~~~~~~

Before moving on to learning more about writing functions and barely
useful snippets of code, there are a few other miscellaneous bits of
information that might be useful to you in the future that I'd like to
discuss.

The first one concerns metadata about modules. I mentioned in the
beginning of this chapter that module attributes are metadata
describing the module itself. Where can we find this metadata when we
don't have an access to the source? Well the compiler plays nice with
us: when compiling a module, it will pick up most module attributes
and store them (along with other information) in a `module_info/0`
function. You can see the metadata of the `useless` module the
following way:


::

    
    9> useless:module_info().
    [{exports,[{add,2},
               {hello,0},
               {greet_and_add_two,1},
               {module_info,0},
               {module_info,1}]},
     {imports,[]},
     {attributes,[{vsn,[174839656007867314473085021121413256129]}]},
     {compile,[{options,[]},
               {version,"4.6.2"},
               {time,{2009,9,9,22,15,50}},
               {source,"/home/ferd/learn-you-some-erlang/useless.erl"}]}]
    10> useless:module_info(attributes).
    [{vsn,[174839656007867314473085021121413256129]}]


The snippet above also shows an additional function, `module_info/1`
which will let you grab one specific piece of information. You can see
exported functions, imported functions (none in this case!),
attributes (this is where your custom metadata would go), and compile
options and information. Had you decided to add `-author("An Erlang
Champ").` to your module, it would have ended up in the same section
as `vsn`. There are limited uses to module attributes when it comes to
production stuff, but they can be nice when doing little tricks to
help yourself out: I'm using them in my testing script for this book
to annotate functions for which unit tests could be better; the script
looks up module attributes, finds the annotated functions and shows a
warning about them.

Note: `vsn` is an automatically generated unique value differentiating
each version of your code, excluding comments. It is used in code hot-
loading (upgrading an application while it runs, without stopping it)
and by some tools related to release handling. You can also specify a
`vsn` value yourself if you want: just add `-vsn(VersionNumber)` to
your module.

Another point that would be nice to approach regards general module
design: avoid circular dependencies! A module A should not call a
module B that also calls module A . Such dependencies usually end up
making code maintenance difficult. In fact, depending on too many
modules even if they're not in a circular dependency can make
maintenance harder. The last thing you want is to wake up in the
middle of the night only to find a maniac software engineer or
computer scientist trying to gouge your eyes out because of terrible
code you have written.

For similar reasons (maintenance and fear for your eyes), it is
usually considered a good practice to regroup functions that have
similar roles close together. Starting and stopping an application or
creating and deleting a record in some database are examples of such a
scenario.

Well, that's enough for the pedantic moralizations. How about we
explore Erlang a little more?

.. _atom: starting-out-for-real.html#atoms


