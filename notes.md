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
individually, and possibly also use a more specialised memorisation.

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
