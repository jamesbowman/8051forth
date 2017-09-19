: align ;
: aligned ;

: chars ;
: char+ 1+ ;

: cells 2* ;
: cell+ 2 + ;

: (.s)
    depth if
        >r recurse r> dup .
    then ;

: .s
    [char] [ emit space (.s) [char] ] emit
;


: ?     @ . ;

: dump
    ?dup
    if
        1- 16 / 1+
        0 do
            cr dup dup .x space
            16 0 do
                dup c@ .x2 1+
            loop
            space swap
            16 0 do
                dup c@
                dup bl 127 within invert
                if drop [char] . then
                emit 1+
            loop
            drop
        loop
    then
    drop ;

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
