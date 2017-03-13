\ PMOD pin:         3=GRN   2=YEL   1=ORA
\ port $000 bit:    2       1       0     
\                   ResetN  P2_2    P2_1
new

: input             6 2 io! ;
: output            7 2 io! ;
: out               1 io! ;
: clock ( x -- )    dup 2 + out out ;

: sel               input 4 out 0 out 0 clock 0 clock 4 out ;

: send
    output
    8 0 do
        2* dup 8 rshift 1 and   \ extract MS bit
        4 + clock               \ clock with ResetN high
    loop
    drop ;

: recv
    input
    0
    8 0 do
        4 clock
        2* 1 io@ 1 and +
    loop ;

: discard recv drop ;
: split         ( x -- lo hi )  dup 8 rshift ;
: send16        ( x -- )        split send send ;

: READ_STATUS   ( -- x )        $34 send  recv ;
: WR_CONFIG     ( x -- )        $1d send  send discard ;
: RD_CONFIG     ( -- x )        $24 send  recv ;
: GET_CHIP_ID   ( -- id rev )   $68 send  recv recv ;
: RESUME        ( -- )          $4c send discard ;
: op                            $55 send send ;
: 1op                           $56 send send ;
: 2op                           $57 send send ;
: DEBUG_INSTR1  ( op -- a )     $55 send  send        recv ;
: DEBUG_INSTR2  ( a8 op -- a )  $56 send  send send   recv ;
: DEBUG_INSTR3  ( a16 op -- a ) 2op send16 recv ;

: MOV_A#        ( d16 -- )      $74 1op send discard ;
: MOV_DPTR#     ( d8 -- )       $90 DEBUG_INSTR3 drop ;
: INC_DPTR      ( -- )          $a3 DEBUG_INSTR1 drop ;
: s!            ( v r -- )      $75 2op send send discard ;
: s@            ( r -- x )      $e5 1op send recv ;
: >XDATA        ( x -- )
    MOV_A#
    $f0 op discard              \ MOVX @DPTR,A
    INC_DPTR ;
: SET_PC        ( d16 - )       $02 2op send16 discard ;

$ac constant FADDRL
$ad constant FADDRH
$ae constant FLC
$af constant FWDATA

: READ_CODE_MEMORY ( a u -- )
    swap MOV_DPTR#
    0 do
        i 16 mod 0= if cr then
        $e4 DEBUG_INSTR1 drop           \ CLR A
        $93 DEBUG_INSTR1 .x2            \ MOVC A,@A+DPTR
        INC_DPTR
    loop ;

: READ_XDATA_MEMORY ( a u -- )
    swap MOV_DPTR#
    0 do
        i 16 mod 0= if cr then
        $e0 DEBUG_INSTR1 .x2            \ MOVX A,@DPTR
        INC_DPTR
    loop ;

: ?SFR $df80 $80 READ_XDATA_MEMORY ;

: FADDR! ( a16 -- )             split FADDRH s! FADDRL s! ;

: x sel begin READ_STATUS .x cr 1000 ms again ;
: hh ( a u -- x a' ) 0. 2swap >number drop nip ;
: 2h ( a   -- x a' ) 2 hh ;
: 4h ( a   -- x a' ) 4 hh ;
: ramload
    hex
    begin
        pad dup 80 accept drop
        1+ 2h 4h 2h         ( count addr fin a )
        swap 0=
    while
                            ( count addr a )
        nip swap
                            ( a count )
        0 ?do
            2h swap >XDATA
        loop
        drop
    repeat
    2drop drop ;

sel $ff40 MOV_DPTR# ramload
#include flashcopy.hex

variable fpage 0 fpage !

: flush ( -- )
    fpage @ MOV_DPTR#
    $ff40 SET_PC RESUME
    1 ms
    BEGIN READ_STATUS $20 and until
;

: flashload
    hex
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

flashload
#include cc0f.hex
0 SET_PC RESUME

\ Load ram part at $f000 and jump to it
\ sel $f000 MOV_DPTR# ramload
\ #include cc0.hex
\ $f000 SET_PC RESUME

#bye
