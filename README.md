# haskell-C89-interpreter
Interpreter for C89 in Haskell

Run the interpreter by giving the source file path in the first positon of the arguments.

Eg.

    $ runhaskell Main.hs test.c

Unsupported (yet) features

1. `goto` (Not in current plan)
2. Function pointer (In plan)
3. Default parameters (In plan)
4. `do..while` (Coming)
5. `#include` (Coming)
6. `struct` (Coming)
7. Access Containment. (Will never be implemented)
8. Distinguish between `int`, `long int` and `short int` (Not in current plan)
9. "Real world" `int` (one that can over/underflow). (Not in current plan)

... and the list goes on

And, uhm, the memory allocated for arrays wouldn't be recycled ... yet, due to some ... lapses.
