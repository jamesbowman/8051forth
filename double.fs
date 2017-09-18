\ #######   DOUBLE   ##########################################

: 2VARIABLE
    CREATE 2 CELLS ALLOT ;

: 2CONSTANT
    : POSTPONE 2LITERAL POSTPONE ; ;

: DMAX
    2OVER 2OVER D< IF
        2SWAP
    THEN
    2DROP
;

: DMIN
    2OVER 2OVER D< INVERT IF
        2SWAP
    THEN
    2DROP
;

: M+    S>D D+ ;

: M*
    2DUP XOR >R
    ABS SWAP ABS UM*
    R> 0< IF DNEGATE THEN
;

\ FROM WIL BADEN'S "FPH POPULAR EXTENSIONS"
\ HTTP://WWW.WILBADEN.COM/NEIL_BAWD/FPHPOP.TXT

: TNEGATE                           ( T . . -- -T . . )
    >R  2DUP OR DUP IF DROP  DNEGATE 1  THEN
    R> +  NEGATE ;

: T*                                ( D . N -- T . . )
                                    ( D0 D1 N)
    2DUP XOR >R                     ( R: SIGN)
    >R DABS R> ABS
    2>R                             ( D0)( R: SIGN D1 N)
    R@ UM* 0                        ( T0 D1 0)
    2R> UM*                         ( T0 D1 0 D1*N .)( R: SIGN)
    D+                              ( T0 T1 T2)
    R> 0< IF TNEGATE THEN ;

: T/                                ( T . . U -- D . )
                                    ( T0 T1 T2 U)
    OVER >R >R                      ( T0 T1 T2)( R: T2 U)
    DUP 0< IF TNEGATE THEN
    R@ UM/MOD                       ( T0 REM D1)
    ROT ROT                         ( D1 T0 REM)
    R> UM/MOD                       ( D1 REM' D0)( R: T2)
    NIP SWAP                        ( D0 D1)
    R> 0< IF DNEGATE THEN ;

: M*/  ( D . N U -- D . )  >R T*  R> T/ ;

