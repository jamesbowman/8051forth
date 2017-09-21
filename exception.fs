variable abortmsg

: (abort")  ( x1 caddr u -- )
    swap if
        abortmsg ! -2 throw
    else
        drop
    then
;

: abort"
    postpone c"
    postpone (abort")
; immediate

:noname
    source-id if
        cr source type
    then
    \ cr >inwas @ spaces [char] ^ emit
    cr ." error: "
    case
    -1  of ." aborted" endof
    -2  of abortmsg @ count type endof
    -4  of ." stack underflow" endof
    -9  of ." invalid memory address" endof
    -13 of ." undefined word " $ff30 count type endof
    -14 of ." interpreting a compile-only word" endof
    -28 of ." user interrupt" endof
    dup .
    endcase
    cr
; 'fault !
