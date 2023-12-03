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

**Thoughts on the puzzle:** The parser ends up building a map
representing a schematics, which means that the parser ends up with a
fair amount of booking for indices. The code could possible be made
easier to read using a state monad to keep track of `i`, `j`, the
current parsed number, the columns of the parsed number and schematic
being constructed.
