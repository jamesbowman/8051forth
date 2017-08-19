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

