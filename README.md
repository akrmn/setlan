## CI3725 Traductores e interpretadores
# setlan

### Members
11-10005 - Moisés Ackerman

11-10323 - Carlos Ferreira

### Implementation decisions
We chose Haskell for its powerful functional notation, and because of the
ease of use of the tools Alex and Happy.

### Current Status
Everything is working as expected.

### Problems?
None yet.

### Other comments
The original double meaning for the map operators (<+>, <->, <*>, </>, <%>) was
kept, meaning that both `3 <-> {4}` and `{3} <-> 4` are valid, (and give
different results).

When a boolean value is scanned, it must be capitalized (Haskell).
