\ PMOD pin:         3=GRN   2=YEL   1=ORA           9=GRN   8=YEL   7=ORA
\ port $000 bit:    2       1       0               6       5       4     
\                   ResetN  P2_2    P2_1            ResetN  P2_2    P2_1
new hex

: input             $66 2 io! ;
: output            $77 2 io! ;
: out               1 io! ;
: clock ( x -- )    dup $22 + out out ;

output $44 out 0 out 0 clock 0 clock $44 out \ init sequence
input

: send
    output 8 lshift
    8 0 do
        dup 0< $11 and          \ extract MS bit as 00 or 11
        $44 + clock 2*          \ clock with ResetN high
    loop
    drop ;

: recv
    input
    0
    8 0 do
        $44 clock
        2* 1 io@ dup 2* 2* 2* 2* or $101 and +
    loop ;

: ignore                        recv drop ;
: split         ( x -- lo hi )  dup 8 rshift ;
: send16        ( x -- )        split send send ;

: READ_STATUS   ( -- x )        $34 send recv ;
: WR_CONFIG     ( x -- )        $1d send send ignore  ;
: RD_CONFIG     ( -- x )        $24 send recv ;
: GET_CHIP_ID   ( -- id rev )   $68 send recv recv ;
: RESUME        ( -- )          $4c send ignore  ;
: op                            $55 send send ;
: 1op                           $56 send send ;
: 2op                           $57 send send ;

GET_CHIP_ID .s

: MOV_DPTR#     ( d8 -- )       $90 2op send16 ignore  ;
: >XDATA        ( x -- )
    $74 1op send ignore         \ MOV A,#x
    $f0 op ignore               \ MOVX @DPTR,A
    $a3 op ignore ;             \ INC DPTR
: SET_PC        ( d16 - )       $02 2op send16 ignore  ;

: hh ( a u -- x a' ) 0. 2swap >number drop nip ;
: 2h ( a   -- x a' ) 2 hh ;
: 4h ( a   -- x a' ) 4 hh ;
: ramload                       \ copy Intel HEX records to DPTR
    begin
        pad dup 80 accept drop
        1+ 2h 4h 2h             ( count addr fin a )
        swap 0=
    while
                                ( count addr a )
        nip swap                ( a count )
        0 ?do
            2h swap >XDATA
        loop
        drop
    repeat
    2drop drop ;

$ff40 MOV_DPTR# ramload
#include flashcopy.hex

GET_CHIP_ID drop                    \ xx01: lo present  01xx: hi present
Dup  $00ff and $0001 = $00ff and    \ 00ff: lo present
Swap $ff00 and $0100 = $ff00 and    \                   ff00: hi present
Or constant present                 \ present is 0000, 00ff, ff00, or ffff
: yesno ( f ) if ." YES" else ." NO" then ;
Cr ." port 0 connected: " present $0001 and yesno
Cr ." port 1 connected: " present $0100 and yesno

: halted ( -- f ) \ are all present CPUs halted?
    begin
        READ_STATUS present invert or
        $2020 and $2020 =
    until ;

Variable fpage 0 fpage !

: flush ( -- )
    fpage @ MOV_DPTR#
    $ff40 SET_PC RESUME
    1 ms
    halted ;

: flashload
    begin
        pad dup 80 accept drop
        1+ 2h 4h 2h         ( count addr fin a )
        swap 0=
    while
                            ( count addr a )
        over $fc00 and fpage @ <> if
            flush
            over fpage !
        then
        swap fpage @ - $f000 + MOV_DPTR#
        swap
                            ( a count )
        0 ?do
            2h swap >XDATA
        loop
        drop
    repeat
    flush
    2drop drop ;

Flashload
#include cc0f.hex
0 SET_PC RESUME
#bye
