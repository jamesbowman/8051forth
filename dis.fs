: table
    create cells allot
    does> swap cells + ;

256 table distab

: default ( a opcode -- a )
    2 .r
;

: op16 ( a -- a+2 ) count >< >r count r> + ;
: x8 ( u -- n )     dup $7f and swap $80 and - ;
: rel ( a -- a+1 )  count x8 over + . ;

marker scratch
:noname 
    256 0 do
        ['] default i distab !
    loop
; execute
scratch

:noname ( a opcode -- a+2 )
    drop op16
    dup ['] LIT = if
        drop ." LIT " op16 >< .
    else
        ." lcall " .
    then
; $12 distab !
:noname drop ." ret" ; $22 distab !
:noname drop ." jz " rel ; $60 distab !
:noname drop ." sjmp " rel ; $80 distab !

: 1dis ( a -- a' )
    dup 4 .r space
    count dup distab @ execute
    cr
;

: dis
    base @ >r hex
    25 0 do
        1dis
    loop
    drop r> base ! ;

: see ' dis ;
