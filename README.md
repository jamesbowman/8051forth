# 8051forth
Forth for the 8051

To rebuild:

    make

To run the Forth interactively in the emulator:

    $ emu8051/emu generic camel51.hex
    8051 CamelForth v1.6  18 Aug 1999
    1 2 + .
    1 2 + . 3 ok

To run the ANS regression suite:

    $ ./confirm generic camel51.hex
    Cycles: 491549891, 17555.353 ms

Same for the CC1110 Forth:

    ./confirm cc1110  cc0.hex
