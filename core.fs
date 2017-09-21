: immediate 1 latest @ 1- c! ;
: \ 0 parse 2drop ; immediate
: ( ')' parse 2drop ; immediate

: char parse-name drop c@ ;

: [char]
    char postpone literal
; immediate

: align ;
: aligned ;

: chars ;
: char+ 1+ ;

: cells 2* ;
: cell+ 2 + ;

: bounds ( a n -- a+n a ) over + swap ;

: erase 0 fill ;

: s,
    dup ic,
    bounds
    begin
        2dup xor
    while
        count ic,
    repeat
    2drop
;

: (sliteral)
    r>
    count
    2dup +
    >r
;

: sliteral
    ['] (sliteral) compile,
    s,
; immediate

: (.s)
    depth if
        >r recurse r> dup .
    then ;

create _ 80 allot  \ The "temporary buffer" in ANS: A.11.6.1.2165

: s"
    [char] " parse
    state @ if
        postpone sliteral
    else
        tuck _ swap cmove
        _ swap
    then
; immediate

: ." postpone s" postpone type ; immediate

: (c")
    r>
    dup
    count +
    >r
;

: c"
    [char] " parse
    ['] (c") compile,
    s,
; immediate

: roll
    ?dup if
        swap >r
        1- recurse
        r> swap
    then
;

: pick
    ?dup if
        swap >r
        1- recurse
        r> swap
    else
        dup
    then
;

: unused $ff00 here - ;

: source-id 'source-id @ ;

: refill
    source-id 0= dup if
        tib dup 128 accept
        0 source!
    then
;

: evaluate
    >in @ >r
    source 2>r
    source-id >r true 'source-id !
    interpret
    r> 'source-id !
    2r> r> source!
;

\ #######   DEFERRED WORDS    #################################

: defer ( "name" -- )
  create ['] abort ,
does> ( ... -- ... )
  @ execute ;

: defer@ ( xt1 -- xt2 )
  >body @ ;

: defer! ( xt2 xt1 -- )
  >body ! ;

: is
  state @ if
    POSTPONE ['] POSTPONE defer!
  else
    ' defer!
  then ; immediate

: action-of
 state @ if
   POSTPONE ['] POSTPONE defer@
 else
   ' defer@
then ; immediate

include string0.fs
include tools-ext.fs

\ preserve DP, IDP, LATEST
: marker
    latest @ idp @ dp @
    create
        , , ,
    does>
         dp 6 move
;

include value.fs

( CASE                                       JCB 09:15 07/18/14)
\ From ANS specification A.3.2.3.2

0 constant case immediate  ( init count of ofs )

: of  ( #of -- orig #of+1 / x -- )
    1+    ( count ofs )
    postpone over  postpone = ( copy and test case value)
    postpone if    ( add orig to control flow stack )
    postpone drop  ( discards case value if = )
    swap           ( bring count back now )
; immediate

: endof ( orig1 #of -- orig2 #of )
    >r   ( move off the stack in case the control-flow )
         ( stack is the data stack. )
    postpone else
    r>   ( we can bring count back now )
; immediate

: endcase  ( orig1..orign #of -- )
    postpone drop  ( discard case value )
    begin
        dup
    while
        swap postpone then 1-
    repeat drop
; immediate

\ From ANS specification A.6.2.0970
: convert   1+ 511 >number drop ;

include forth2012.fs
include double0.fs
include double.fs
include exception.fs

.( Forth system is ) ihere . .( bytes) cr
