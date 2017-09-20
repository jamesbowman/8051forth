\ #######   TOOLS   ###########################################

: ?
    @ .
;

: (.s)
    depth if
        >r recurse r>
        dup .
    then
;

: .s
    [char] < emit depth 0 .r [char] > emit space
    (.s)
;

: dump
    ?dup
    if
        base @ >r hex
        1- 4 rshift 1+
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
        r> base !
    then
    drop
;

\ #######   TOOLS EXT   #######################################

\ From ANS specification A.15.6.2.2533
\ Using PARSE-NAME instead of "BL WORD COUNT"

: ite ( level adr len -- level' )
    2dup  s" [IF]"  compare 0=
    if                                \ level adr len
      2drop 1+                        \ level'
    else                              \ level adr len
      2dup  s" [ELSE]"
      compare 0= if                   \ level adr len
         2drop 1- dup 0<> -           \ level'
      else                            \ level adr len
        s" [THEN]" compare 0= +       \ level
      then
    then
;

: [ELSE]  ( -- )
    1 begin                               \ level
      begin
        parse-name dup
      while                               \ level adr len
        ite
        ?dup 0=  if exit then             \ level'
      repeat  2drop                       \ level
    refill 0= until                       \ level
    drop
;  immediate

: [IF]  ( flag -- )
0= if postpone [ELSE] then ;  immediate

: [THEN]  ( -- )  ;  immediate

: cs-pick   pick ;
: cs-roll   roll ;

: [defined] bl word find nip 0<> ; immediate
: [undefined] postpone [defined] 0= ; immediate
