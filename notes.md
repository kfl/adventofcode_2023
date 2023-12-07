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

**Thoughts on the puzzle:** Brute force worked again for finding the
best possible replacement for jokers.
