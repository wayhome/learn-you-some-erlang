


A Short Visit to Common Data Structures
---------------------------------------



Won't be too long, promised!
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Chances are you now understand the functional subset of Erlang pretty
well and could read many programs without a problem. However, I could
bet it's still a bit hard to think about how to build a real useful
program even though the last chapter was about solving problems in a
functional manner. I'm saying this because it's how I felt like at
about that point in my learning, but if you're doing better,
congratulations!

Anyway, the point I'm coming to is that we've seen a bunch of things:
most basic data types, the shell, how to write modules and functions
(with recursion), different ways to compile, control the flow of the
program, handle exceptions, abstract away some common operations, etc.
We've also seen how to store data with tuples, lists and an incomplete
implementation of a binary search tree. What we haven't seen is the
other data structures provided to the programmer in the Erlang
standard library.



Records
~~~~~~~


.. image:: ../images/record-player.png
    :alt: a phonograph


Records are, first of all, a hack. They are more or less an
afterthought to the language and can have their share of
inconveniences. I'll cover that later. They're still pretty useful
whenever you have a small data structure where you want to access the
attributes by name directly. As such, Erlang records are a lot like
structs in C (if you know C.)

They're declared as module attributes in the following manner:


::

    
    -module(records).
    -compile(export_all).
    
    -record(robot, {name,
                    type=industrial,
                    hobbies,
                    details=[]}).


So here we have a record representing robots with 4 fields: name,
type, hobbies and details. There is also a default value for type and
details, ``industrial`` and ``[]``, respectively. Here's how to
declare a record in the module records:


::

    
    first_robot() ->
        #robot{name="Mechatron",
               type=handmade, 
               details=["Moved by a small man inside"]}.


And running the code:


::

    
    1> c(records).
    {ok,records}
    2> records:first_robot().
    {robot,"Mechatron",handmade,undefined,
           ["Moved by a small man inside"]}


Woops! Here comes the hack! Erlang records are just syntactic sugar on
top of tuples. Fortunately, there's a way to make it better. The
Erlang shell has a command ``rr(Module)`` that lets you load record
definitions from Module :


::

    
    3> rr(records).
    [robot]
    4> records:first_robot().         
    #robot{name = "Mechatron",type = handmade,
           hobbies = undefined,
           details = ["Moved by a small man inside"]}


Ah there! This makes it much easier to work with records that way.
You'll notice that in ``first_robot/0``, we had not defined the
``hobbies`` field and it had no default value in its declaration.
Erlang, by defaults, sets the value to undefined for you.

To see the behavior of the defaults we set in the ``robot``
definition, let's compile the following function:


::

    
    car_factory(CorpName) ->
        #robot{name=CorpName, hobbies="building cars"}.


And run it:


::

    
    5> c(records).
    {ok,records}
    6> records:car_factory("Jokeswagen").
    #robot{name = "Jokeswagen",type = industrial,
           hobbies = "building cars",details = []}


And we have an industrial robot that likes to spend time building
cars.

Note: The function ``rr()`` can take more than a module name: it can
take a wildcard (like ``rr("*")``) and also a list as a second
argument to specify which records to load.

There are a few other functions to deal with records in the shell:
``rd(Name, Definition)`` lets you define a record in a manner similar
to the ``-record(Name, Definition)`` used in our module. You can use
``rf()`` to 'unload' all records, or ``rf(Name)`` or ``rf([Names])``
to get rid of specific definitions.

You can use ``rl()`` to print all record definitions in a way you
could copy-paste into the module or use ``rl(Name)`` or
``rl([Names])`` to restrict it to specific records.

Finally, ``rp(Term)`` lets you convert a tuple to a record (given the
definition exists).

Writing records alone won't do much. We need a way to extract values
from them. There are basically two ways to do this. The first one is
with a special 'dot syntax'. Assuming you have the record definition
for robots loaded:


::

    
    5> Crusher = #robot{name="Crusher", hobbies=["Crushing people","petting cats"]}. 
    #robot{name = "Crusher",type = industrial,
           hobbies = ["Crushing people","petting cats"],
           details = []}
    6> Crusher#robot.hobbies.
    ["Crushing people","petting cats"]


Ugh, not a pretty syntax. This is due to the nature of records as
tuples. Because they're just some kind of compiler trick, you have to
keep keywords around defining what record goes with what variable,
hence the ``#robot`` part of ``Crusher#robot.hobbies``. It's sad, but
there's no way out of it. Worse than that, nested records get pretty
ugly:


::

    
    7> NestedBot = #robot{details=#robot{name="erNest"}}.
    #robot{name = undefined,type = industrial,
           hobbies = undefined,
           details = #robot{name = "erNest",type = industrial,
                            hobbies = undefined,details = []}}
    8> (NestedBot#robot.details)#robot.name. 
    "erNest"


And yes, the parentheses are mandatory.

Update:
Starting with revision R14A, it is now possible to nest records
without the parentheses. The NestedBot example above could also be
written as ``NestedRobot#robot.details#robot.name`` and work the same.

To further show the dependence of records on tuples, see the
following:


::

    
    9> #robot.type.
    3


What this outputs is which element of the underlying tuple it is.

One saving feature of records is the possibility to use them in
function heads to pattern match and also in guards. Declare a new
record as follows on top of the file, and then add the functions
under:


::

    
    -record(user, {id, name, group, age}).
    
    %% use pattern matching to filter
    admin_panel(#user{name=Name, group=admin}) ->
        Name ++ " is allowed!";
    admin_panel(#user{name=Name}) ->
        Name ++ " is not allowed".
    
    %% can extend user without problem
    adult_section(U = #user{}) when U#user.age >= 18 ->
        %% Show stuff that can't be written in such a text
        allowed;
    adult_section(_) ->
        %% redirect to sesame street site
        forbidden.


The syntax to bind a variable to any field of a record is demonstrated
in the ``admin_panel/1`` function (it's possible to bind variables to
more than one field). An important thing to note about the
``adult_section/1`` function is that you need to do ``SomeVar =
#some_record{}`` in order to bind the whole record to a variable. Then
we do the compiling as usual:


::

    
    10> c(records).
    {ok,records}
    11> rr(records).
    [robot,user]
    12> records:admin_panel(#user{id=1, name="ferd", group=admin, age=96}).
    "ferd is allowed!"
    13> records:admin_panel(#user{id=2, name="you", group=users, age=66}). 
    "you is not allowed"
    14> records:adult_section(#user{id=21, name="Bill", group=users, age=72}).
    allowed
    15> records:adult_section(#user{id=22, name="Noah", group=users, age=13}).
    forbidden


What this lets us see is how it is not necessary to match on all parts
of the tuple or even know how many there are when writing the
function: we can only match on the age or the group if that's what's
needed and forget about all the rest of the structure. If we were to
use a normal tuple, the function definition might need to look a bit
like ``function({record, _, _, ICareAboutThis, _, _}) -> ...``. Then,
whenever someone decides to add an element to the tuple, someone else
(probably angry about it all) would need to go around and update all
functions where that tuple is used.

The following function illustrates how to update a record (they
wouldn't be very useful otherwise):


::

    
    repairman(Rob) ->
        Details = Rob#robot.details,
        NewRob = Rob#robot{details=["Repaired by repairman"|Details]},
        {repaired, NewRob}.


And then:


::

    
    16> c(records).
    {ok,records}
    17> records:repairman(#robot{name="Ulbert", hobbies=["trying to have feelings"]}).
    {repaired,#robot{name = "Ulbert",type = industrial,
                     hobbies = ["trying to have feelings"],
                     details = ["Repaired by repairman"]}}


And you can see my robot has been repaired. The syntax to update
records is a bit special here. It looks like we're updating the record
in place ( ``Rob#robot{Field=NewValue}``) but it's all compiler
trickery to call the underlying ``erlang:setelement/3`` function.

One last thing about records. Because they're pretty useful and code
duplication is annoying, Erlang programmers frequently share records
across modules with the help of *header files*. Erlang header files
are pretty similar to their C counter-part: they're nothing but a
snippet of code that gets added to the module as if it were written
there in the first place. Create a file named records.hrl with the
following content:


::

    
    %% this is a .hrl (header) file.
    -record(included, {some_field,
                       some_default = "yeah!",
                       unimaginative_name}).


To include it in records.erl, just add the following line to the
module:


::

    
    -include("records.hrl").


And then the following function to try it:


::

    
    included() -> #included{some_field="Some value"}.


Now, try it as usual:


::

    
    18> c(records).
    {ok,records}
    19> rr(records).
    [included,robot,user]
    20> records:included().
    #included{some_field = "Some value",some_default = "yeah!",
              unimaginative_name = undefined}


Hooray! That's about it for records; they're ugly but useful. Their
syntax is not pretty, they're not much but a hack, but they're
relatively important for the maintainability of your code.

Note: You will often see open source software using the method shown
here of having a project-wide ``.hrl`` file for records that are
shared across all modules. While I felt obligated to document this
use, I strongly recommend that you keep all record definitions local,
within one module. If you want some other module to look at a record's
innards, write functions to access its fields and keep its details as
private as possible. This helps prevent name clashes, avoids problems
when upgrading code, and just generally improves the readability and
maintainability of your code.



Key-Value Stores
~~~~~~~~~~~~~~~~


.. image:: ../images/key.png
    :alt: key and keyhole, another terrible pun


I've had you build a tree back a few chapters, and the use was to use
it as a key-value store for an address book. That book sucked: we
couldn't delete or convert it to anything useful. It was a good
demonstration of recursion, but not much more. Now is the time to
introduce you to a bunch of useful data structures and modules to
store data under a certain key. I won't define what every function
does nor go through all the modules. I will simply link to the doc
pages. Consider me as someone responsible about 'raising awareness
about key-value stores in Erlang' or something. Sounds like a good
title. I just need one of these ribbons.

For small amounts of data, there are basically two data structures
that can be used. The first one is called a *proplist*. A proplist is
any list of tuples of the form ``[{Key,Value}]``. They're a weird kind
of structure because there is no other rule than that. In fact the
rules are so relaxed that the list can also contain boolean values,
integers and whatever you want. We're rather interested by the idea of
a tuple with a key and a value in a list here, though. To work with
proplists, you can use the proplists module. It contains functions
such as ``proplists:delete/2``, ``proplists:get_value/2``,
``proplists:get_all_values/2``, ``proplists:lookup/2`` and
``proplists:lookup_all/2``.

You'll notice there is no function to add or update an element of the
list. This shows how loosely defined proplists are as a data
structure. To get these functionalities, you must cons your element
manually ( ``[NewElement|OldList]``) and use functions such as
``lists:keyreplace/4``. Using two modules for one small data structure
is not the cleanest thing, but because proplists are so loosely
defined, they're often used to deal with configuration lists, and
general description of a given item. Proplists are not exactly
complete data structures. They're more of a common pattern that
appears when using lists and tuples to represent some object or item;
the proplists module is a bit of a toolbox over such a pattern.

If you do want a more complete key-value store for small amounts of
data, the orddict module is what you need. Orddicts (ordered
dictionaries) are proplists with a taste for formality. Each key can
be there once, the whole list is sorted for faster average lookup,
etc. Common functions for the CRUD usage include ``orddict:store/3``,
``orddict:find/2`` (when you do not know whether the key is in the
dictionaries), ``orddict:fetch/2`` (when you know it is there or that
it must be there) and ``orddict:erase/2``.


.. image:: ../images/dict.png
    :alt: A dictionary with the definition of 'Awesome' being 'it's you!'


Orddicts are a generally good compromise between complexity and
efficiency up to about 75 elements (see my benchmark). After that
amount, you should switch to different key-value stores.

There are basically two key-value structures/modules to deal with
larger amounts of data: dicts and gb_trees. Dictionaries have the same
interface as orddicts: ``dict:store/3``, ``dict:find/2``,
``dict:fetch/2``, ``dict:erase/2`` and every other function, such as
``dict:map/2`` and ``dict:fold/2`` (pretty useful to work on the whole
data structure!) Dicts are thus very good choices to scale orddicts up
whenever it is needed.

General Balanced Trees, on the other hand, have a bunch more functions
leaving you more direct control over how the structure is to be used.
There are basically two modes for gb_trees: the mode where you know
your structure in and out (I call this the 'smart mode'), and the mode
where you can't assume much about it (I call this one the 'naive
mode'). In naive mode, the functions are ``gb_trees:enter/2``,
``gb_trees:lookup/2`` and ``gb_trees:delete_any/2``. The related smart
functions are ``gb_trees:insert/3``, ``gb_trees:get/2``,
``gb_trees:update/3`` and ``gb_trees:delete/2``. There is also
``gb_trees:map/2``, which is always a nice thing when you need it.

The disadvantage of 'naive' functions over 'smart' ones is that
because gb_trees are balanced trees, whenever you insert a new element
(or delete a bunch), it might be possible that the tree will need to
balance itself. This can take time and memory (even in useless checks
just to make sure). The 'smart' function all assume that the key is
present in the tree: this lets you skip all the safety checks and
results in faster times.

When should you use gb_trees over dicts? Well, it's not a clear
decision. As the benchmark module I have written will show, gb_trees
and dicts have somewhat similar performances in many respects.
However, the benchmark demonstrates that dicts have the best read
speeds while the gb_trees tend to be a little quicker on other
operations. You can judge based on your own needs which one would be
the best.

Oh and also note that while dicts have a fold function, gb_trees
don't: they instead have an *iterator* function, which returns a bit
of the tree on which you can call ``gb_trees:next(Iterator)`` to get
the following values in order. What this means is that you need to
write your own recursive functions on top of gb_trees rather than use
a generic fold. On the other hand, gb_trees let you have quick access
to the smallest and largest elements of the structure with
``gb_trees:smallest/1`` and ``gb_trees:largest/1``.

I would therefore say that your application's needs is what should
govern which key-value store to choose. Different factors such as how
much data you've got to store, what you need to do with it and whatnot
all have their importance. Measure, profile and benchmark to make
sure.

Note: some special key-value stores exist to deal with resources of
different size. Such stores are ETS tables, DETS tables and the mnesia
database. However, their use is strongly related to the concepts of
multiple processes and distribution. Because of this, they'll only be
approached later on. I'm leaving this as a reference to pique your
curiosity and for those interested.



Arrays
~~~~~~

But what about code that requires data structures with nothing but
numeric keys? Well for that, there are arrays. They allow you to
access elements with numerical indices and to fold over the whole
structure while possibly ignoring undefined slots.

Don't drink too much kool-aid:
Erlang arrays, at the opposite of their imperative counterparts, are
not able to have such things as constant-time insertion or lookup.
Because they're usually slower than those in languages which support
destructive assignment and that the style of programming done with
Erlang doesn't necessary lend itself too well to arrays and matrices,
they are rarely used in practice.

Generally, Erlang programmers who need to do matrix manipulations and
other uses requiring arrays tend to use concepts called Ports to let
other languages do the heavy lifting, or C-Nodes, Linked in drivers
and NIFs (Experimental, R13B03+).

Arrays are also weird in the sense that they're one of the few data
structures to be 0-indexed (at the opposite of tuples or lists), along
with indexing in the regular expressions module. Be careful with them.



A Set of Sets
~~~~~~~~~~~~~


.. image:: ../images/swingset.png
    :alt: a swingSET


If you've ever studied set theory in whatever mathematics class you
have an idea about what sets can do. If you haven't, you might want to
skip over this. However, I'll just say that sets are groups of unique
elements that you can compare and operate on: find which elements are
in two groups, in none of them, only in one or the other, etc. There
are more advanced operations letting you define relations and operate
on these relations and much more. I'm not going to dive into the
theory (again, it's out of the scope of this book) so I'll just
describe them as it is.

There are 4 main modules to deal with sets in Erlang. This is a bit
weird at first, but it makes more sense once you realize that it's
because it was agreed by implementers that there was no 'best' way to
build a set. The four modules are ordsets, sets, gb_sets and sofs
(sets of sets):

:ordsets: Ordsets are implemented as a sorted list. They're mainly
  useful for small sets, are the slowest kind of set, but they have the
  simplest and most readable representation of all sets. There are
  standard functions for them such as ``ordsets:new/0``,
  ``ordsets:is_element/2``, ``ordsets:add_element/2``,
  ``ordsets:del_element/2``, ``ordsets:union/1``,
  ``ordsets:intersection/1``, and a bunch more.
:sets: Sets (the module) is implemented on top of a structure really
  similar to the one used in ``dict``. They implement the same interface
  as ordsets, but they're going to scale much better. Like dictionaries,
  they're especially good for read-intensive manipulations, like
  checking whether some element is part of the set or not.
:gb_sets: Gb_sets themselves are constructed above a General Balanced
  Tree structure similar to the one used in the gb_trees module. gb_sets
  are to sets what gb_tree is to dict; an implementation that is faster
  when considering operations different than reading, leaving you with
  more control. While gb_sets implement the same interface as sets and
  ordsets, they also add more functions. Like gb_trees, you have smart
  vs. naive functions, iterators, quick access to the smallest and
  largest values, etc.
:sofs: Sets of sets (sofs) are implemented with sorted lists, stuck
  inside a tuple with some metadata. They're the module to use if you
  want to have full control over relationships between sets, families,
  enforce set types, etc. They're really what you want if you need
  mathematics concept rather than 'just' groups of unique elements.


Don't drink too much kool-aid:
While such a variety can be seen as something great, some
implementation details can be downright frustrating. As an example,
gb_sets, ordsets and sofs all use the ``==`` operator to compare
values: if you have the numbers 2 and 2.0 , they'll both end up seen
as the same one.

However, sets (the module) uses the ``=:=`` operator, which means you
can't necessarily switch over every implementation as you wish. There
are cases where you need one precise behavior and at that point, you
might lose the benefit of having multiple implementations.

It's a bit confusing to have that many options available. Björn
Gustavsson, from the Erlang/OTP team and programmer of Wings3D mainly
suggests using gb_sets in most circumstances, using ordset when you
need a clear representation that you want to process with your own
code and 'sets' when you need the ``=:=`` operator (source.)

In any case, like for key-value stores, the best solution is usually
to benchmark and see what fits your application better.



Directed Graphs
~~~~~~~~~~~~~~~

There is one other data structure that I want to mention here (not
that there are not more than what's mentioned in this chapter, on the
contrary): directed graphs. Again, this data structure is more for
readers who already know the mathematical theory that goes with it.

Directed graphs in Erlang are implemented as two modules, digraph and
digraph_utils. The digraph module basically allows the construction
and modification of a directed graph: manipulating edges and vertices,
finding paths and cycles, etc. On the other hand, digraph_utils allows
you to navigate a graph (postorder, preorder), testing for cycles,
arborescences or trees, finding neighbors, and so on.

Because directed graphs are closely related to set theory, the 'sofs'
module contains a few functions letting you convert families to
digraphs and digraphs to families.



Queues
~~~~~~

The queue module implements a double-ended FIFO (First In, First Out)
queue:


.. image:: ../images/fifo.png
    :alt: Drawing representing the implementation of a functional queue


They're implemented a bit as illustrated above: two lists (in this
context, stacks) that allow to both append and prepend elements
rapidly.

The queue module basically has different functions in a mental
separation into 3 interfaces (or APIs) of varying complexity, called
'Original API', 'Extended API' and 'Okasaki API':

:Original API: The original API contains the functions at the base of
  the queue concept, including: ``new/0``, for creating empty queues,
  ``in/2``, for inserting new elements, ``out/1``, for removing
  elements, and then functions to convert to lists, reverse the queue,
  look if a particular value is part of it, etc.
:Extended API: The extended API mainly adds some introspection power
  and flexibility: it lets you do things such as looking at the front of
  the queue without removing the first element (see ``get/1`` or
  ``peek/1``), removing elements without caring about them (
  ``drop/1``), etc. These functions are not essential to the concept of
  queues, but they're still useful in general.
:Okasaki API: The Okasaki API is a bit weird. It's derived from Chris
  Okasaki's *Purely Functional Data Structures*. The API provides
  operations similar to what was available in the two previous APIs, but
  some of the function names are written backwards and the whole thing
  is relatively peculiar. Unless you do know you want this API, I
  wouldn't bother with it.


You'll generally want to use queues when you'll need to ensure that
the first item ordered is indeed the first one processed. So far, the
examples I've shown mainly used lists as a accumulators that would
then be reversed. In cases where you can't just do all the reversing
at once and elements are frequently added, the queue module is what
you want (well, you should test and measure first! Always test and
measure first!)



End of the short visit
~~~~~~~~~~~~~~~~~~~~~~

That's about it for the data structures trip of Erlang. Thank you for
having kept your arms inside the vehicles the whole time. Of course,
there are a few more data structures available than that to solve
different problems. I've only covered those that you're likely to
encounter or need the most given the strengths of general use cases of
Erlang. I encourage you to explore the standard library and the
extended one too to find more information.

You might be glad to learn that this completes our trip into
sequential (functional) Erlang. I know a lot of people get in Erlang
to see all the concurrency and processes and whatnot. It's
understandable, given it's really where Erlang shines. Supervision
trees, fancy error management, distribution, and more. I know I've
been very impatient to write about these subjects, so I guess some
readers were very impatient to read about them.

However, I judged it made more sense to be comfortable with functional
Erlang before moving on to concurrent Erlang. It will be easier to
move on afterwards and focus on all the new concepts. Here we go!


.. image:: ../images/squid-concurrency.png
    :alt: The splash screen's squid riding a rocket towards concurrency










