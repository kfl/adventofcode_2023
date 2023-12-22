Day 1: Trebuchet?!
------------------

**Language used:** SML.

**Parsing strategy:** Basic string and list manipulation.

**Thoughts on the puzzle:** A relatively straightforward puzzle mostly
about parsing. Part 2 had a slight crinkle where the specification was
unclear about whether natural language digits are allowed to
overlap. I started by implementing the wrong interpretation.

Day 2: Cube Conundrum
---------------------

**Language used:** Haskell

**Parsing strategy:** `ReadP` parser combinators.

**Thoughts on the puzzle:** All done with list manipulation.


Day 3: Gear Ratios
------------------

**Language used:** Haskell

**Parsing strategy:** List manipulation.

**Thoughts on the puzzle:** The parser builds a map representing a
schematics, which means that the parser needs to do a fair amount of
bookkeeping for indices and so on. The code could possibly be made
easier to read using a state monad to keep track of `i`, `j`, the
current parsed number, the columns of the parsed number and the
schematic being constructed.

Once the schematics is constructed it's fairly straightforward to write
the various queries for the two parts.


Day 4: Scratchcards
-------------------

**Language used:** Haskell

**Parsing strategy:** String splitting

**Thoughts on the puzzle:** Nice puzzle. For once recursion over
lists seemed just right and couldn't be easily done with higher order
functions (at least not without a more involved accumulating
parameter).

Added an alternative version made with `foldl`, it's not as bad I
thought it would be. Also ported the solution to F#.


Day 5: If You Give A Seed A Fertilizer
--------------------------------------

**Language used:** Haskell

**Parsing strategy:** String splitting

**Thoughts on the puzzle:** Nice puzzle. Used far too much time
getting the interval logic right. I'm fairly sure it can be done more
elegant than the code I ended up with.

Rather than tracing the seed ranges through the mappings I would have
liked to compose the mappings (as they are piecewise linear functions)
alas I couldn't get it to work. However, I'm not sure it would have
helped in the end (actually, I made a back-of-the-envelope estimate
that suggested that it wouldn't have helped).


Day 6: Wait For It
------------------

**Language used:** Rust

**Parsing strategy:** String splitting

**Thoughts on the puzzle:** Brute force worked :broken_heart:


Day 7: Camel Cards
------------------

**Language used:** Haskell

**Parsing strategy:** String splitting (barely)

**Thoughts on the puzzle:** Brute force worked again, this time for
finding the best possible replacement for jokers.

Added an alternative to brute force for finding the best replacement.


Day 8: Haunted Wasteland
------------------------

**Language used:** Haskell

**Parsing strategy:** String splitting

**Thoughts on the puzzle:** Part 1 was a straightforward automaton
run. Part 2 was a bit more interesting, since brute-force worked the
last two days, I expected it to **not** work today. I started by
implementing the general solution where you kept track of all states
at the same time. While that was running, I thought about making
simplifying assumptions. One is that once a ghost hits an end-node it
enters a cycle, of the same or shorter length than it took to arrive
at the end-node, it least that was the case for the given test
example. Thus you just have to find the least common multiplier for
all cycle-length. I implemented that and to found a working solution
instantly (while the general brute force solution was still
working). However, I find it an unsatisfying solution. Because it is
possible to construct networks where this solution does not work. I
guess the proper general solution is to view the network as multiple
[Büchi automata](https://en.wikipedia.org/wiki/B%C3%BCchi_automaton)
and then find cycle length in their composed language or something
like that, or at least check that the simplifying assumption holds.


Day 9: Mirage Maintenance
-------------------------

**Language used:** Haskell

**Parsing strategy:** String splitting (barely)

**Thoughts on the puzzle:** No twists. Got to exercise the standard
list functions.

Added a Rust version to exercise some iterators.


Day 10: Pipe Maze
-----------------

**Language used:** Haskell

**Parsing strategy:** String and list manipulation. Builds a graph
during parsing.

**Thoughts on the puzzle:** Part 1 is straightforward path following
and then counting the length of the path.

Path 2 was more interesting. Made some visualisation functions to
better understand. Was stuck until I realised that the problem is in
essence a points inside polygon problem. Implemented a simple scan-line
algorithm, but used far to long getting corner-points right (more than
the allotted time-box).


Day 11: Cosmic Expansion
------------------------

**Language used:** Haskell

**Parsing strategy:** List comprehension

**Thoughts on the puzzle:** It's always a pleasure to work in [taxicap
geometry](https://en.wikipedia.org/wiki/Taxicab_geometry). Could make
my code shorter by refactor the more general solution for part 2 to
solve part 1, but decided to leave the code as it was during the
event. Likewise, the current implementation could be made more
efficient (at least wrt big-O complexity) by using sets instead of
lists, but it's fast enough to not bother.


Day 12: Hot Springs
-------------------

**Language used:** Haskell

**Parsing strategy:** String splitting and `read` abuse

**Pre-coding thoughts on the puzzle:** This looks like the solution
from day 4 can be used. But not entirely sure how to convert this days
puzzle format into the format from day 4.

**Thoughts on the puzzle:** Part 1 solved by recursively computing all
possible arrangements. Part 2, Q: what do you do when have a recursive
solution, but it is computing the same thing over and over? A: You
slap some ugly memoisation on it.

Part 2 is still too slow for my taste, however the current solution
runs in well under 5s which is acceptable. To get real speedups from
here we need either a better data structure where we divide the
condition and checksum into independent groups that can solved
individually, and possibly also use a more specialised memoisation.

**Maybe some day:** Instead of checking one element at a time and
keeping track of a running sum, try to match as many `#` (and `?`) at
a time as possible.


Day 13: Point of Incidence
--------------------------

**Language used:** Haskell

**Parsing strategy:** String splitting and List comprehension

**Thoughts on the puzzle:** Decided to use bit-vectors for keeping
track of rocks. Partly because I wanted to exercise using bit-vectors
and partly because I had an inkling that it would come use for part 2,
and it did :smile: Given the size of the sets, I don't think it would
have mattered much if I had used another set representation neither
performance-wise nor code elegance/size. However there is something
satisfying about using `xor` and `countBits` to find the number of
differences between two sets.


Day 14: Parabolic Reflector Dish
--------------------------------

**Language used:** Haskell

**Parsing strategy:** List comprehension.

**Thoughts on the puzzle:** Less elegant choices where made with the
data representation.


Day 15: Lens Library
--------------------

**Language used:** Haskell

**Parsing strategy:** String splitting for both parts.

**Thoughts on the puzzle:** Part 2 is fun. Implementing a small VM of
sorts is always appreciated. Used to opportunity to play with
Haskell's `vector` library. Liked that I was able to encapsulate the
mutation of the box vector nicely.


Day 16: The Floor Will Be Lava
------------------------------

**Language used:** Haskell

**Parsing strategy:** List comprehension.

**Thoughts on the puzzle:** Made a straightforward implementation with
no smarts aside from using maps and sets, it worked. It was a bit
slow, so I used parallelism to speed it up and the running time
dropped below 1s. Another performance improvement is to use s set
implementation based on hashing instead of search trees. A more
elegant solution to improve the running time would be to use
memoisation across the different start position, didn't do that.


Day 17: Clumsy Crucible
-----------------------

**Language used:** Haskell

**Parsing strategy:** List comprehension to array construction.

**Thoughts on the puzzle:** Both parts was path finding problems. In
both path the interesting feature is that the graph is context
dependant, thus it best to unfold the graph dynamically when
traversing it. I implemented my own Dijkstra's shortest path
algorithm, my implementation is four times faster than the one from
the `search-algorithms` package. I compared `dijkstraAssoc` and
`aStarAssoc` to see if it would be worth to implement the A*
algorithm, but without a better heuristic than Manhattan distance,
it's not an improvement to use A*.

Added an alternative way to compute the states of the search space,
this brings the time below 1s.


Day 18: Lavaduct Lagoon
-----------------------

**Language used:** Haskell

**Parsing strategy:** String splitting and a hand-written hex parser

**Thoughts on the puzzle:** Both parts was about finding the area of a
polygon given as a sequence of points. Good that we where introduced
to the same problem earlier this year (on day 10). Back then I didn't
get to use the [Shoelace
formula](https://en.wikipedia.org/wiki/Shoelace_formula) (found out
too late), worked like a charm for today's problem.


Day 19: Aplenty
---------------

**Language used:** Haskell

**Parsing strategy:** String splitting and `read` abuse

**Thoughts on the puzzle:** Good puzzle, but I think I over-engineered
my solution a bit. Both parts was fast an correct on first
run. "Wasted" time making a graph visualiser, to determine if I had to
worry about loops. Alas, I couldn't determine if there was any
loops. However, it looked there might be many shared paths. Hence, I
concluded that I better use some memoisation from the get go (the
`seen` argument in `loop`). I also ended up making my own faux lenses
library (the `view` and `over` functions), even though I'm not a fan
on lenses.


Day 20: Pulse Propagation
-------------------------

**Language used:** Haskell

**Parsing strategy:**

**Pre-coding analysis:** A *module* consists of: some mutable memory,
an immutable *transfer function*. A transfer function takes two
arguments the memory and an a pulse, and returns the new memory and a
sequence of pulses. A pulse consists of a sender module, a value
(`low` or `high`) and a destination module. There are three kinds of
transfer functions: `Flip-flop`, `Conjunction` and `Special`.

Hunch: I'm guessing that we'll be doing cycle detection in part 2, as
the button pushing seem suitable for caching.

**Thoughts on the puzzle:** Only completed part 1 during the event. It
seems that we must do some kind of cycle detection for part 2. Because the
solution requires more than 100M steps. A quick inspection of the
input (I hate when that's necessary) reveals that my `rx` module is
the destination for a single conjunction module, which in turn is the
single destination for four other conjunction modules. Thus, my guess
is that we figure out some gear solution like on day 8. I don't have
the energy for completing this during event time.

Next step: make a visualiser to see if some pattern appears.


Day 21: Step Counter
--------------------

**Language used:** Haskell

**Parsing strategy:** List comprehensions.

**Thoughts on the puzzle:** Only completed part 1 during the event. It
is obvious that brute force isn't going to work (I had it running for
~8h). I tried to use the method from day 9 to predict the value, it
seems that there must be some for sort of recurrence, as (1) the
starting position is right in the middle of the map, and (2) there are
"corridors" straight from the starting position to the edges in both
axis. Alas, I couldn't get it to work.


Day 22: Sand Slabs
------------------

**Language used:** Haskell

**Parsing strategy:** List comprehensions.

**Thoughts on the puzzle:** Not completed during the event.
