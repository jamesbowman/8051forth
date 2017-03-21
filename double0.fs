\ #######   DOUBLE low-level primitives   ####################

: 2ROT
    >R >R 2SWAP R> R> 2SWAP
;

: 2LITERAL
    SWAP POSTPONE LITERAL POSTPONE LITERAL
; IMMEDIATE

: DABS
    DUP 0< IF DNEGATE THEN
;

: D>S   DROP ;

: D=                        \ A B C D -- F ) 
    >R                      \ A B C 
    ROT =                   \ B A=C 
    SWAP R> =               \ A=C B=D 
    AND
; 

: D<            \ ( AL AH BL BH -- FLAG ) 
    ROT         \ AL BL BH AH 
    2DUP = 
    IF 
        2DROP U< 
    ELSE 
        > NIP NIP
    THEN 
; 

: DU<           \ ( AL AH BL BH -- FLAG ) 
    ROT         \ AL BL BH AH 
    2DUP = 
    IF 
        2DROP U< 
    ELSE 
        U> NIP NIP
    THEN 
; 

: D-
    DNEGATE D+
;

: D0<
    NIP 0<
;

: D0=
    OR 0=
;

: D2*
    2DUP D+
; 

: D2/
    >R 1 RSHIFT R@
    [ 8 CELLS 1- ] LITERAL LSHIFT
    OR R> 2/
;
