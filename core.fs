: abort true throw ;

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

: decimal #10 base ! ;
: hex     $10 base ! ;

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
    source>r
    true 'source-id !
    interpret
    r>source ;

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

include double0.fs
include double.fs
include exception.fs

( Pictured numeric output                    JCB 08:06 07/18/14)
\ Adapted from hForth

\ "The size of the pictured numeric output string buffer shall
\ be at least (2*n) + 2 characters, where n is the number of
\ bits in a cell."
\
\ The size of the region identified by WORD shall be at least
\ 33 characters.

create BUF0
16 cells 2 + 33 max allot
here constant BUF

variable hld

: <#
    BUF hld !
;

: hold
    -1 hld +! hld @ c!
;

: sign
    0< if
        [char] - hold
    then
;

: #
    0 base @ um/mod >r base @ um/mod swap
    9 over < [ char A char 9 1 + - ] literal and +
    [char] 0 + hold r>
;

: #s
    begin
        #
        2dup d0=
    until
;

: #>
    2drop hld @ BUF over -
;

: d.r  ( d n -- )
    >r
    dup >r dabs <# #s r> sign #>
    r> over - spaces type
;

: d.  ( d -- )
    0 d.r space
;

: .  ( n -- )
    s>d d.
;

: u.  ( u -- )
    0 d.
;

: .r  ( n1 n2 -- )
    >r s>d r> d.r
;

: u.r  ( u n -- )
    0 swap d.r
;

include tools-ext.fs
include forth2012.fs

: new
    s" | marker |" evaluate
    ." Forth system is " ihere . ." bytes" cr
;
marker |
\ #flash dump.bin
