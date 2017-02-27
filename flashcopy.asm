; ===============================================

        .org 0xf400

        .equ DPS,0x92
        .equ MPAGE,0x93

        .equ FWT,0xab
        .equ FADDRL,0xac
        .equ FADDRH,0xad
        .equ FLC,0xae
        .equ FWDATA,0xaf

;        mov r6,#0
;        mov FLC,#2      ; write
;writeloop:
;        movx a,@dptr
;        inc dptr
;        mov FWDATA,a
;
;        movx a,@dptr
;        inc dptr
;        mov FWDATA,a
;
;wait:   mov a,FLC
;        jb acc.6,wait
;
;        djnz r6,writeloop
;        .db 0xa5

        .equ U0CSR,0x86
        .equ U0UCR,0xc4
        .equ U0GCR,0xc5
        .equ U0DBUF,0xc1
        .equ U0BAUD,0xc2

        .equ P0DIR,0xfd
        .equ P0SEL,0xf3

        .equ CLKCON,0xc6

        ; On entry DPTR is the flash destination byte address
        ; This is 1K page aligned, so can only be:
        ; 0x0000
        ; 0x0400
        ; 0x8000 etc.

        mov FADDRL,#0
        mov a,dph
        rr a
        mov FADDRH,a

        ;             76543210
        mov CLKCON,#0b10001000                  ; external crystal
        mov FWT,#0x2a                           ; flash write timer
        mov P0SEL, #0b00001100                  ; UART0
        mov U0CSR, #0b11000000                  ; 8N1
        mov U0UCR, #0b10000010
        mov U0GCR, #12                          ; 115200
        mov U0BAUD,#34

        acall cr
        mov a,FADDRH
        acall x2

                                                ; DPTR is flash addr
                                                ; MPAGE:r0 is RAM address
        mov MPAGE,#0xf0
        mov r0,#0
        mov r2,#4
compare:
        clr a
        movc a,@a+dptr
        mov r1,a
        movx a,@r0
        clr c
        subb a,r1
        jnz mismatch

        inc dptr
        inc r0
        mov a,r0
        jnz compare
        inc MPAGE
        djnz r2,compare

        mov a,#0x3d
        acall tx
        .db 0xa5

mismatch:
        mov a,#0x2a
        acall tx

        ; Erase page
        ; mov FADDRL,#0
        ; mov FADDRH,#0
erasew0:mov a,FLC
        jb acc.7,erasew0
        mov FLC,#1      ; erase
        nop
        nop
erasew: mov a,FLC
        jb acc.7,erasew

        mov dptr,#0xf000
        mov r1,#2
        mov r0,#0
        mov FLC,#2      ; write
        nop
        nop
write:
        movx a,@dptr
        inc dptr
        mov FWDATA,a
        movx a,@dptr
        inc dptr
        mov FWDATA,a
writew: mov a,FLC
        jb acc.6,writew
        djnz r0,write
        djnz r1,write
   .db 0xa5

cr:
        mov a,#0x0a
        acall tx
        mov a,#0x0d
        sjmp tx
x2:
        push acc
        rr a
        rr a
        rr a
        rr a
        acall x1
        pop acc
        acall x1
space:
        mov a,#32
        sjmp tx
x1:
        anl a,#15
        clr c
        subb a,#0x0a
        jc numeric
        add a,#7
numeric: add a,#0x3a
tx:
        mov U0DBUF,a
w:      mov a,U0CSR
        jb acc.0,w
        ret
