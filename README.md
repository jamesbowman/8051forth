# 8051forth - Forth for the 8051

[![Build Status](https://travis-ci.org/jamesbowman/8051forth.svg?branch=master)](https://travis-ci.org/jamesbowman/8051forth)

This is based on CamelForth for the Intel 8051 (c) 1994,1997,1999 Bradford J. Rodriguez:

http://www.camelforth.com/page.php?4

Brad's 1999 version uses a DOS assembler, which is a little difficult to find now.
The changes to this project are:

1. now uses a modified version PJRC's `as31` assembler, included in this project
2. minor changes to Brad's code to build with `as31`
3. a software emulator based on emu8051, included in this project
4. regression test using the ANS test suite, CI by Travis
5. a version tuned for the TI CC1110 SoCs

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
