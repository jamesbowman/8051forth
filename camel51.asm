; ===============================================
; CamelForth for the Intel 8051
; (c) 1994,1997,1999 Bradford J. Rodriguez
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

; Commercial inquiries should be directed to the author at 
; 115 First St., #105, Collingwood, Ontario L9Y 4W3 Canada
; or via email to bj@camelforth.com
;
; ===============================================
; CAMEL51.ASM: Code Primitives
;   Source code is for the A51 assembler.
;   Forth words are documented as follows:
;x   NAME     stack -- stack    description
;   where x=C for ANS Forth Core words, X for ANS
;   Extensions, Z for internal or private words.
;
; Subroutine-Threaded Forth model for Intel 8051
; 16 bit cell, 8 bit char, 8 bit (byte) adrs unit
; split Code & Data spaces
;    8051 PC = Forth IP  Interpreter Pointer
;         SP =       RSP Return Stack Pointer low
;                    RSP high byte = 0
;         R0 =       PSP Parameter Stack Ptr low
;                    PSP high = UP
;     reg 08 = P2 =  UP  User area Pointer high
;                    (and PSP high), UP low = 0
;       DPTR =       TOS (top Param. Stack item)
;  A,B,R1-R5 =       temporaries
;                    (no W register is defined)
;      R6,R7 =       loop index
;  reg 09-7F =       return stack
; ===============================================
; REVISION HISTORY
; $Log: Camel51.asm,v $
; v1.6  18 Aug 99  Fixed FM/MOD (again).
;
; Revision 1.5  1997/05/28 23:03:52  brad
; v1.5  Added multitasker words.       28 Mar 97
;       Corrected memory map comments.
;       Fixed UM* bug.
;       Fixed FM/MOD bug, per Ed Smeda (thanks!)
;       Fixed >BODY to return Data adrs, per ANS.
;       Moved SWAP from REPEAT to WHILE, per ANS.
;	ABORT" ?ABORT now use IS" and ITYPE.
;       Fixed WORDS to ignore smudge bit.
;       Fixed IWORD to return Code address.
;       Renamed ROMDICT,RAMDICT to CODERAM,DATARAM
; v1.4  changed QUIT to type CR even    9 Nov 96
;       if STATE<>0, to support downloading.
;       Added example I/O.
; v1.3  changed PLUSLOOPSENSE to       17 Mar 96
;       PLUSLPSENSE (for some A51 assemblers)
; v1.2  fixed names of KEY? and S>D
; v1.1  bug fixes for split I & D mem  14 Mar 95
;       in COLD: changed CMOVE to I->D
;       in XISQUOTE: changed COUNT to ICOUNT
;       in LIT: uses MOVC instead of MOVX
;       in FIND:  changed NFA>LFA @ to NFA>LFA I@
;       in QUIT:  changed TYPE to ITYPE
; v1.0  alpha test version, 12 Dec 94
; ===============================================
; Forth linkage
        .equ link,0
        .equ IMMED,1    ; flag for Immediate word

; 8051 EQUATES
        .equ dr2,0x02   ; r2-r5 accessed as
        .equ dr3,0x03   ; direct registers;
        .equ dr4,0x04   ; required for PUSH and
        .equ dr5,0x05   ; POP instructions.
        .equ dr6,0x06   ; Assumes register bank 0
        .equ dr7,0x07   ; is selected.
        .equ UP,0x08

; FORTH MEMORY MAP EQUATES
; Memory map:
;   regs 8-7Fh  Return stack, 120 bytes, grows up
;   0000h       Forth kernel
;   E000h       Forth dictionary (program)
;   F000h       Forth dictionary (user RAM)
;   UAREA-100h  Task Save Area, 128 bytes
;   UAREA-80h   Terminal Input Buffer, 128 bytes
;   UAREA=FE00h User area, 128 bytes
;   UAREA+80h   Parameter stack, 128B, grows down
;   UAREA+100h  HOLD area, 40 bytes, grows down
;   UAREA+128h  PAD buffer, 88 bytes
;   UAREA+180h  Leave stack, 128 bytes, grows up
; See also the definitions of U0, S0, and R0
; in the "system variables & constants" area.
; A task w/o terminal input requires 200h bytes.
; Double all except TIB and PAD for 32-bit CPUs.

; Initial RAM & ROM pointers for CamelForth.
    .equ coderam,0x0e000    ; where new code goes
    .equ dataram,0x0f000    ; where data goes
    .equ UPHI,0xFE          ; Uarea at FE00 hex

; RESET AND INTERRUPT VECTORS ===================
        ljmp reset
;         ljmp ie0
; ie0:    reti
;         .skip 4
;         ljmp clock ; ljmp tf0
; tf0:    reti
;         .skip 4
;         ljmp ie1
; ie1:    reti
;         .skip 4
;         ljmp tf1
; tf1:    reti
;         .skip 4
;         ljmp riti
; riti:   reti
;         .skip 4
;         ljmp tf2
; tf2:    reti
; 
reset:  mov ie,#0       ; disable all irpts
        mov pcon,#0     ; T1 baudrate not doubled
        mov tmod,#0x20  ; T1 mode 2, T0 mode 0
        mov th1,#0xfd   ; 9600 baud @ 11.0592 MHz
        setb tcon.6     ; enable timer 1
        mov scon,#0x52  ; UART mode 1 (8-bit)

        mov UP,#UPHI
        mov p2,UP       ; user area at FE00,
        mov r0,#0xff    ; param stack at FEFF
        mov sp,#0x8     ; ret stack at bottom
        ljmp COLD       ; enter Forth interpreter

; SERIAL I/O ====================================
;C EMIT     c --      output character to console
        .drw link
        .set link,*+1
        .db  0,4,"EMIT"
EMIT:   ; jnb scon.1,EMIT ; await Tx interrupt flag
        ; clr scon.1      ; clear flag
        mov sbuf,dpl    ; output TOS char to UART
        ajmp poptos     ; pop new TOS

;C KEY      -- c      get character from keyboard
        .drw link
        .set link,*+1
        .db  0,3,"KEY"
KEY:    ; jnb scon.0,KEY  ; await Rx interrupt flag
        ; clr scon.0
        ; dec r0          ; push old TOS
        ; mov a,dph
        ; movx @r0,a
        ; dec r0
        ; mov a,dpl
        ; movx @r0,a
        ; mov dpl,sbuf    ; get new char in TOS
        ; mov dph,#0

        acall   DUP
        mov dpl,sbuf    ; get new char in TOS
        mov dph,#0
        ret

;X KEY?      -- f      return true if char waiting
        .drw link
        .set link,*+1
        .db  0,4,"KEY?"
QUERYKEY: dec r0          ; push old TOS
        mov a,dph
        movx @r0,a
        dec r0
        mov a,dpl
        movx @r0,a
        mov a,scon      ; get rx flag in carry
        rrc a
        ajmp cyprop     ; propagate that thru TOS

; INTERPRETER LOGIC =============================

; NEXT and ENTER are not needed for Subroutine
; Threading. EXIT may be used in high level code.

;C EXIT     --            exit a colon definition
        .drw link
        .set link,*+1
        .db  0,4,"EXIT"
EXIT:   dec sp      ; discard ret adrs in caller
        dec sp
        ret         ; return to caller's caller

;Z LIT      -- x    fetch inline literal to stack
        .drw link
        .set link,*+1
        .db  0,3,"LIT"
LIT:    dec r0          ; push old TOS
        mov a,dph
        movx @r0,a
        dec r0
        mov a,dpl
        movx @r0,a
        pop dph         ; get return address
        pop dpl
        clr a
        movc a,@a+dptr  ; get literal low byte
        inc dptr
        mov r2,a
        clr a
        movc a,@a+dptr  ; get literal high byte
        inc dptr
        push dpl        ; restore updated ret adr
        push dph
        mov dph,a       ; put literal in TOS
        mov dpl,r2
        ret

;C EXECUTE  i*x xt -- j*x      execute Forth word
;C                             at 'xt'
        .drw link
        .set link,*+1
        .db  0,7,"EXECUTE"
EXECUTE: push dpl       ; push addr onto r.stack,
        push dph        ; then pop new TOS->DPTR
        ; 'ret' in poptos will then execute
        ; desired word; its 'ret' will return to
        ; EXECUTE's caller.
        ajmp poptos

; DEFINING WORDS ================================

;C VARIABLE --            define a Forth VARIABLE
;   CREATE CELL ALLOT ;
; Action of ROMable variable is that of CONSTANT;
; the constant holds the RAM address.
        .drw link
        .set link,*+1
        .db  0,8,"VARIABLE"
VARIABLE: lcall CREATE
        acall CELL
        ljmp ALLOT

;C CONSTANT --            define a Forth constant
;   CREATE CELL NEGATE IALLOT  I,   Harvard model
;   DOES> (machine code fragment)
; Note that the constant is stored in Code space.
        .drw link
        .set link,*+1
        .db  0,8,"CONSTANT"
CONSTANT: lcall CREATE
        lcall CELL
        lcall NEGATE
        lcall IALLOT
        lcall ICOMMA
        lcall XDOES
; DOCON, code action of CONSTANT,
; entered by CALL DOCON
docon:  ; -- x         exec action of constant
dovar:  ; -- a-addr    exec action of ROMable var
docreate: ; -- a-addr  exec action of Harv.CREATE
        dec r0          ; push old TOS
        mov a,dph
        movx @r0,a
        dec r0
        mov a,dpl
        movx @r0,a
        pop dph         ; get addr of param field
        pop dpl         ;     (in Code memory!)
        ajmp IFETCH     ; go fetch its contents

;Z USER     n --         define user variable 'n'
;   CONSTANT DOES> (machine code fragment)
; Note that this version allows a full 16-bit
; offset from the user pointer.
        .drw link
        .set link,*+1
        .db  0,4,"USER"
USER:   acall CONSTANT
        lcall XDOES
; DOUSER, code action of USER,
; entered by CALL DOUSER
douser: acall pushtos   ; push old TOS
        pop dph         ; get addr of param field
        pop dpl         ;     (in Code memory!)
        acall IFETCH    ; go fetch its contents
        add a,UP        ; add UP:00 to offset
        mov dph,a       ; NB. IFETCH leaves A=DPH
        ret

; DOCREATE's action is for a table in RAM.
; DOROM is the code action for a table in ROM;
; it returns the address of the parameter field.
; Entered by CALL DOROM
dorom:  acall pushtos   ; push old TOS
        pop dph         ; param field adrs -> TOS
        pop dpl
        ret

; DODOES, code action of DOES> clause
; (internal code fragment, not a Forth word)
; entered by       LCALL fragment
;                  address of data
;                       ...
;        fragment: LCALL DODOES
;                  high-level thread
; Enters high-level thread with address of
; data on top of stack.  HARVARD MODEL: the data
; (in Data space) does NOT follow LCALL fragment
; (in Code space); instead, the address of the
; data is appended after LCALL fragment.
dodoes: ; -- a-addr     support routine for DOES>
        dec r0          ; push old TOS
        mov a,dph
        movx @r0,a
        dec r0
        mov a,dpl
        movx @r0,a
        pop dr5         ; addr of DOES> clause
        pop dr4         ;   Forth code
        pop dph         ; addr of defined word's
        pop dpl         ;   Param. field
        push dr4        ; restore Forth code addr
        push dr5
        ajmp IFETCH     ; fetch adrs from P.field
                        ;  & go do the DOES> code

; STACK OPERATIONS ==============================

;C DUP      x -- x x       duplicate top of stack
        .drw link
        .set link,*+1
        .db  0,3,"DUP"
DUP:
pushtos: dec r0         ; push hi byte of TOS
        mov a,dph
        movx @r0,a
        dec r0          ; push lo byte of TOS
        mov a,dpl
        movx @r0,a
        ret

;C ?DUP     x -- 0 | x x           DUP if nonzero
        .drw link
        .set link,*+1
        .db  0,4,"?DUP"
QDUP:   mov a,dph
        orl a,dpl
        jnz pushtos
        ret

;C DROP     x --                drop top of stack
        .drw link
        .set link,*+1
        .db  0,4,"DROP"
DROP:
poptos: movx a,@r0      ; pop lo byte -> TOS
        mov dpl,a
        inc r0
        movx a,@r0      ; pop hi byte -> TOS
        mov dph,a
        inc r0
        ret

;C SWAP     x1 x2 -- x2 x1     swap top two items
        .drw link
        .set link,*+1
        .db  0,4,"SWAP"
;; SWOP:   movx a,@r0      ; pop lo byte -> X
;;         mov r2,a
;;         inc r0
;;         movx a,@r0      ; pop hi byte -> X
;;         mov r3,a
;;         ; inc r0
;;         ; dec r0        ; push hi byte of TOS
SWOP:
        movx a,@r0
        xch a,dpl
        movx @r0,a
        inc r0
        movx a,@r0
        xch a,dph
        movx @r0,a
        dec r0
        ret
halfover: mov a,dph     
        movx @r0,a
        dec r0          ; push lo byte of TOS
        mov a,dpl
        movx @r0,a
        mov dph,r3      ; old 2nd item -> TOS
        mov dpl,r2
        ret

;C OVER     x1 x2 -- x1 x2 x1   per stack diagram
        .drw link
        .set link,*+1
        .db  0,4,"OVER"
OVER:
        ; movx a,@r0      ; pop lo byte -> X
        ; mov r2,a
        ; inc r0
        ; movx a,@r0      ; pop hi byte -> X
        ; mov r3,a
        ; dec r0          ; restore stack pointer
        ; dec r0          ; predecrement for 'halfover'
        ; sjmp halfover   ; push TOS, then copy X to TOS
        movx a,@r0      ; r2:r3 is x1
        mov r3,a
        inc r0
        movx a,@r0
        mov r2,a
        dec r0

        dec r0          ; push hi byte of TOS
        mov a,dph
        movx @r0,a
        dec r0          ; push lo byte of TOS
        mov a,dpl
        movx @r0,a

        mov dph,r2
        mov dpl,r3
        ret

;C ROT     x1 x2 x3 -- x2 x3 x1 per stack diagram
        .drw link
        .set link,*+1
        .db  0,3,"ROT"
ROT:    ; x3 is in TOS
        movx a,@r0      ; pop x2 -> r5:r4
        mov r4,a
        inc r0
        movx a,@r0
        mov r5,a
        inc r0
        movx a,@r0      ; pop x1 -> r3:r2
        mov r2,a
        inc r0
        movx a,@r0
        mov r3,a
        ; inc r0
        ; dec r0        ; push x2
        mov a,r5        
        movx @r0,a
        dec r0
        mov a,r4
        movx @r0,a
        dec r0          ; predecr. for 'halfover'
        sjmp halfover   ; push x3 (TOS), then
                        ;    copy x1 to TOS

;C >R       x --   R: -- x   push to return stack
        .drw link
        .set link,*+1
        .db  0,2,">R"
TOR:    pop dr3         ; save ret addr in r3:r2
        pop dr2
        push dpl        ; push lo byte*
        push dph        ; push hi byte*
        push dr2        ; restore ret addr
        push dr3
        sjmp poptos     ; pop new TOS
;* NB. stored lo:hi in regs because SP increments

;C R>       -- x   R: x --  pop from return stack
        .drw link
        .set link,*+1
        .db  0,2,"R>"
RFROM:  dec r0          ; push old TOS
        mov a,dph
        movx @r0,a
        dec r0
        mov a,dpl
        movx @r0,a
        pop dr3         ; save ret addr in r3:r2
        pop dr2
        pop dph         ; pop hi byte
        pop dpl         ; pop lo byte
        push dr2        ; restore return address
        push dr3
        ret

;C R@       -- x  R: x -- x  fetch from rtn stack
        .drw link
        .set link,*+1
        .db  0,2,"R@"
RFETCH: dec r0          ; push old TOS
        mov a,dph
        movx @r0,a
        dec r0
        mov a,dpl
        movx @r0,a
        mov r1,sp       ; get copy of SP
        dec r1          ; skip return address
        dec r1
        mov dph,@r1     ; fetch 2nd return stack item
        dec r1
        mov dpl,@r1
        ret

;Z SP@      -- a-addr      get data stack pointer
        .drw link
        .set link,*+1
        .db  0,3,"SP@"
SPFETCH: dec r0          ; push old TOS
        mov a,dph
        movx @r0,a
        dec r0
        mov a,dpl
        movx @r0,a
        mov dph,UP       ; 16-bit pointer P2:R0
        mov dpl,r0
        ret

;Z SP!      a-addr --      set data stack pointer
; Note: only the low 8 bits are affected!
        .drw link
        .set link,*+1
        .db  0,3,"SP!"
SPSTORE: mov r0,dpl       ; set stack pointer
         ajmp poptos      ; get new TOS

;Z RP@      -- a-addr    get return stack pointer
        .drw link
        .set link,*+1
        .db  0,3,"RP@"
RPFETCH: dec r0          ; push old TOS
        mov a,dph
        movx @r0,a
        dec r0
        mov a,dpl
        movx @r0,a
        mov dph,#0       ; 16-bit pointer 00:SP
        mov dpl,sp
        ret

;Z RP!      a-addr --    set return stack pointer
; Note: only the low 8 bits are significant!
        .drw link
        .set link,*+1
        .db  0,3,"RP!"
RPSTORE: pop dr3         ; save ret addr in r3:r2
        pop dr2
        mov sp,dpl       ; set new stack pointer
        push dr2         ; restore ret addr
        push dr3
        ajmp poptos      ; get new TOS

;X NIP    x1 x2 -- x2           per stack diagram
        .drw link
        .set link,*+1
        .db  0,3,"NIP"
NIP:    inc r0
        inc r0
        ret

;X TUCK   x1 x2 -- x2 x1 x2     per stack diagram
        .drw link
        .set link,*+1
        .db  0,4,"TUCK"
TUCK:   acall SWOP
        ajmp OVER

; MEMORY OPERATIONS =============================

;C !        x a-addr --    store cell in Data mem
; Byte order is lo,hi.
        .drw link
        .set link,*+1
        .db  0,1,"!"
STORE:  movx a,@r0      ; low byte of X
        inc r0
        movx @dptr,a
        inc dptr
        movx a,@r0      ; high byte of X
        inc r0
        movx @dptr,a
        ajmp poptos     ; pop new TOS

;C C!       c c-addr --    store char in Data mem
        .drw link
        .set link,*+1
        .db  0,2,"C!"
CSTORE: movx a,@r0      ; low byte is char
        inc r0
        movx @dptr,a
        inc r0          ; skip high byte
        ajmp poptos     ; pop new TOS

;C @        a-addr -- x  fetch cell from Data mem
; Byte order is lo,hi.
        .drw link
        .set link,*+1
        .db  0,1,"@"
FETCH:  movx a,@dptr    ; low byte
        mov r2,a        ; ..temporary stash
        inc dptr
        movx a,@dptr    ; high byte
        mov dpl,r2      ; copy to TOS (DPTR)
        mov dph,a
        ret

;C C@       c-addr -- c  fetch char from Data mem
        .drw link
        .set link,*+1
        .db  0,2,"C@"
CFETCH: movx a,@dptr
        mov dpl,a
        mov dph,#0
        ret

;C I!       x a-addr --    store cell in Code mem
; On 8051, the only way to store to Code memory
; is to have it also appear in Data space.
; So, I! is identical to !, and IC! to C!.
        .drw link
        .set link,*+1
        .db  0,2,"I!"
ISTORE: sjmp STORE

;C IC!      c c-addr --    store char in Code mem
        .drw link
        .set link,*+1
        .db  0,3,"IC!"
ICSTORE: sjmp CSTORE

;Z I@       a-addr -- x  fetch cell from Code mem
; Byte order is lo,hi.
        .drw link
        .set link,*+1
        .db  0,2,"I@"
IFETCH: clr a
        movc a,@a+dptr  ; low byte
        mov r2,a        ; ..temporary stash
        mov a,#1
        movc a,@a+dptr  ; high byte
        mov dpl,r2      ; copy to TOS (DPTR)
        mov dph,a
        ret
; Note! USER expects IFETCH to leave A=DPH!

;Z IC@      a-addr -- x  fetch char from Code mem
        .drw link
        .set link,*+1
        .db  0,2,"IC@"
ICFETCH: clr a
        movc a,@a+dptr  ; low byte
        mov dpl,a
        mov dph,#0
        ret

; ARITHMETIC AND LOGICAL OPERATIONS =============

;C +        n1/u1 n2/u2 -- n3/u3        add n1+n2
        .drw link
        .set link,*+1
        .db  0,1,"+"
PLUS:   movx a,@r0      ; low byte
        inc r0
        add a,dpl
        mov dpl,a
        movx a,@r0      ; high byte
        inc r0
        addc a,dph
        mov dph,a
        ret

;Z M+       d n -- d         add single to double
        .drw link
        .set link,*+1
        .db  0,2,"M+"
MPLUS:  movx a,@r0      ; pop d.high -> r3:r2
        mov r2,a
        inc r0
        movx a,@r0
        mov r3,a
        inc r0
        movx a,@r0      ; d.low, low byte
        add a,dpl
        movx @r0,a
        inc r0
        movx a,@r0      ; d.low, high byte
        addc a,dph
        movx @r0,a
        dec r0
        clr a
        addc a,r2       ; d.high, low byte
        mov dpl,a
        clr a
        addc a,r3       ; d.high, high byte
        mov dph,a
        ret

;C -        n1/u1 n2/u2 -- n3/u3   subtract n1-n2
        .drw link
        .set link,*+1
        .db  0,1,"-"
MINUS:  movx a,@r0      ; low byte
        inc r0
        clr c
        subb a,dpl
        mov dpl,a
        movx a,@r0      ; high byte
        inc r0
        subb a,dph
        mov dph,a
        ret

;C AND      x1 x2 -- x3               logical AND
        .drw link
        .set link,*+1
        .db  0,3,"AND"
AND:    movx a,@r0      ; low byte
        inc r0
        anl a,dpl
        mov dpl,a
        movx a,@r0      ; high byte
        inc r0
        anl a,dph
        mov dph,a
        ret

;C OR       x1 x2 -- x3                logical OR
        .drw link
        .set link,*+1
        .db  0,2,"OR"
OR:     movx a,@r0      ; low byte
        inc r0
        orl a,dpl
        mov dpl,a
        movx a,@r0      ; high byte
        inc r0
        orl a,dph
        mov dph,a
        ret

;C XOR      x1 x2 -- x3               logical XOR
        .drw link
        .set link,*+1
        .db  0,3,"XOR"
XOR:    movx a,@r0      ; low byte
        inc r0
        xrl a,dpl
        mov dpl,a
        movx a,@r0      ; high byte
        inc r0
        xrl a,dph
        mov dph,a
        ret

;C INVERT   x1 -- x2            bitwise inversion
        .drw link
        .set link,*+1
        .db  0,6,"INVERT"
INVERT: xrl dpl,#0xff
        xrl dph,#0xff
        ret

;C NEGATE   x1 -- x2             two's complement
        .drw link
        .set link,*+1
        .db  0,6,"NEGATE"
NEGATE: xrl dpl,#0xff
        xrl dph,#0xff
        inc dptr
        ret

;C 1+       n1/u1 -- n2/u2           add 1 to TOS
        .drw link
        .set link,*+1
        .db  0,2,"1+"
ONEPLUS: inc dptr
        ret

;C 1-       n1/u1 -- n2/u2    subtract 1 from TOS
        .drw link
        .set link,*+1
        .db  0,2,"1-"
ONEMINUS: mov a,dpl
        jnz dphok
        dec dph     ; if dpl=0, decr. affects dph
dphok:  dec dpl
        ret

;Z ><       x1 -- x2        swap bytes (not ANSI)
        .drw link
        .set link,*+1
        .db  0,2,"><"
swapbytes: mov a,dpl
        mov dpl,dph
        mov dph,a
        ret

;C 2*       x1 -- x2        arithmetic left shift
        .drw link
        .set link,*+1
        .db  0,2,"2*"
TWOSTAR: mov a,dpl      ; lo byte, left shift
        add a,dpl
        mov dpl,a
        mov a,dph       ; hi byte, left rot w/cy
        rlc a
        mov dph,a
        ret

;C 2/       x1 -- x2       arithmetic right shift
        .drw link
        .set link,*+1
        .db  0,2,"2/"
TWOSLASH: mov a,dph     ; get msb of TOS into cy
        rlc a
        mov a,dph       ; high byte, right rotate
        rrc a
        mov dph,a
        mov a,dpl       ; low byte, right rotate
        rrc a
        mov dpl,a
        ret
        
;C LSHIFT   x1 u -- x2         logical left shift
        .drw link
        .set link,*+1
        .db  0,6,"LSHIFT"
LSHIFT: mov r4,dpl      ; r4 = loop counter
        movx a,@r0      ; pop x1 -> DPTR
        mov dpl,a
        inc r0
        movx a,@r0
        mov dph,a
        inc r0
        inc r4          ; test for r4=0 case
        sjmp lshtest
lshloop:  mov a,dpl       ; shift left
          add a,dpl
          mov dpl,a
          mov a,dph
          rlc a
          mov dph,a
lshtest:  djnz r4,lshloop
        ret

;C RSHIFT   x1 u -- x2        logical right shift
        .drw link
        .set link,*+1
        .db  0,6,"RSHIFT"
RSHIFT: mov r4,dpl      ; r4 = loop counter
        movx a,@r0      ; pop x1 -> DPTR
        mov dpl,a
        inc r0
        movx a,@r0
        mov dph,a
        inc r0
        inc r4          ; test for r4=0 case
        sjmp rshtest
rshloop:  clr c           ; clear carry
          mov a,dph       ; shift right
          rrc a
          mov dph,a
          mov a,dpl
          rrc a
          mov dpl,a
rshtest:  djnz r4,rshloop
        ret

;C +!       n/u a-addr --    add cell to Data mem
        .drw link
        .set link,*+1
        .db  0,2,"+!"
PLUSSTORE: movx a,@r0      ; low byte of n
        inc r0
        mov r2,a
        movx a,@dptr    ; low byte of memory
        add a,r2
        movx @dptr,a
        inc dptr
        movx a,@r0      ; high byte of n
        inc r0
        mov r2,a
        movx a,@dptr    ; high byte of memory
        addc a,r2
        movx @dptr,a
        ajmp poptos     ; pop new TOS

; COMPARISON OPERATIONS =========================

;X <>     x1 x2 -- flag            test not equal
        .drw link
        .set link,*+1
        .db  0,2,"<>"
NOTEQUAL: acall EQUAL
          sjmp ZEROEQUAL

;C 0=       n/u -- flag      return true if TOS=0
        .drw link
        .set link,*+1
        .db  0,2,"0="
ZEROEQUAL: mov a,dph
zequ1:  orl a,dpl       ; A = z or nz, per DPTR
        clr c
        subb a,#1       ; cy set if A was 0
cyprop: subb a,acc      ; -1 if A was 0, else 0
        mov dph,a
        mov dpl,a
        ret             ; NB! A=0 iff TOS=0

;C 0<       n -- flag        true if TOS negative
        .drw link
        .set link,*+1
        .db  0,2,"0<"
ZEROLESS: mov a,dph
        rlc a           ; cy set if A negative
        sjmp cyprop     ; propagate cy thru TOS

;C =        x1 x2 -- flag              test x1=x2
        .drw link
        .set link,*+1
        .db  0,1,"="
EQUAL:  acall MINUS     ; x1-x2 in TOS, A=DPH
        sjmp zequ1

;C <        n1 n2 -- flag      test n1<n2, signed
        .drw link
        .set link,*+1
        .db  0,1,"<"
LESS:   acall MINUS     ; n1-n2 in TOS, A=DPH,
                        ; CY and OV valid
 ; if result negative (MSB=1) & not OV, n1<n2
 ; neg. & OV => n1 +ve, n2 -ve, result -ve, n1>n2
 ; if result positive (MSB=0) & not OV, n1>=n2
 ; pos. & OV => n1 -ve, n2 +ve, result +ve, n1<n2
 ; thus OV reverses the sense of the sign bit
        jnb psw.2,msbok ; jump if overflow clear
        cpl a           ; OV set: invert msb
msbok:  rlc a           ; put msb (sign) in cy
        sjmp cyprop     ; & propagate thru TOS

;C >        n1 n2 -- flag      test n1>n2, signed
        .drw link
        .set link,*+1
        .db  0,1,">"
GREATER: acall SWOP
        sjmp LESS


;C U<       u1 u2 -- flag    test n1<n2, unsigned
        .drw link
        .set link,*+1
        .db  0,2,"U<"
ULESS:  acall MINUS  ; TOS=u1-u2, cy set if u1<u2
        sjmp cyprop  ; propagate cy thru TOS

;X U>    u1 u2 -- flag       test u1>u2, unsigned
        .drw link
        .set link,*+1
        .db  0,2,"U>"
UGREATER: acall SWOP
          sjmp ULESS

; LOOP AND BRANCH OPERATIONS ====================

; branch and ?branch are done with sjmp and jz,
; respectively, using the following routines
; which leave a value in A.  Typical use:
;   lcall zerosense, jz destadr
;   lcall loopsense, jz destadr, lcall unloop
; LEAVE may exit loop by branching ^--here

        .drw link
        .set link,*+1
        .db  0,7,"?BRANCH"
qbranch:
zerosense: ; n --     leave zero in A if TOS=0
        movx a,@r0    ; new TOS in a:r2
        mov r2,a
        inc r0
        movx a,@r0
        inc r0
        xch a,dph     ; DPH=new TOS hi, A=old DPH
        orl a,dpl     ; A=0 if old TOS was zero
        mov dpl,r2    ; new TOS lo in DPL
        ret

; LOOP and +LOOP are done with jz, using the
; following routines which leave a value in A.
; If the loop terminates, (index crosses 8000h),
; a nonzero value is left in A.  A=0 to loop.
; Typical use:
;   lcall loopsense, jz destadr, lcall unloop
; LEAVE may exit loop by branching ^-here
; The topmost loop index is in regs r7:r6.

        .drw link
        .set link,*+1
        .db  0,6,"(LOOP)"
xloop:
loopsense:  ; --        leave 0 in A if 'loop'
        mov a,r6    ; add 1 to loop index
        add a,#1    ;  ...leaves OV flag set if
        mov r6,a    ;  loop terminates
        mov a,r7
        addc a,#0
        mov r7,a
        jb psw.2,termloop ; jump if OV set
takeloop: clr a     ; OV clear, make A zero
        ret         ;   to take loop branch

        .drw link
        .set link,*+1
        .db  0,7,"(+LOOP)"
xplusloop:
pluslpsense: ; n --    leave 0 in A if '+loop'
        mov a,r6    ; add TOS to loop index
        add a,dpl   ;  ...leaves OV flag set if
        mov r6,a    ;  loop terminates
        mov a,r7
        addc a,dph
        mov r7,a
        movx a,@r0  ; pop new TOS, OV unaffected
        mov dpl,a
        inc r0
        movx a,@r0
        mov dph,a
        inc r0
        jnb psw.2,takeloop ; jump if OV clear
termloop: clr a     ; OV set, make A nonzero
        cpl a       ;  to force loop termination
        ret

;Z (do)    n1|u1 n2|u2 --  R: -- sys1 sys2
;Z                           run-time code for DO
; '83 and ANSI standard loops terminate when the
; boundary of limit-1 and limit is crossed, in
; either direction.  This can be conveniently
; implemented by making the limit 8000h, so that
; arithmetic overflow logic can detect crossing.
; I learned this trick from Laxen & Perry F83.
; fudge factor = 8000h-limit, to be added to
; the start value.
        .drw link
        .set link,*+1
        .db  0,4,"(DO)"
XDO:    ; limit index --
        pop dr3     ; get return adrs in r3:r2
        pop dr2
        push dr6    ; push previous index
        push dr7
        movx a,@r0  ; get (-limit) + 8000h
        inc r0      ;   = (~limit) + 8001h
        cpl a       ;   in r5:r4
        add a,#01
        mov r4,a
        movx a,@r0
        inc r0
        cpl a
        addc a,#0x80
        mov r5,a
        push dr4    ; push this fudge factor
        push dr5
        mov a,r4    ; fudge+index -> r7:r6
        add a,dpl
        mov r6,a
        mov a,r5
        addc a,dph
        mov r7,a
        push dr2    ; restore return addr
        push dr3
        ajmp poptos ; go pop new TOS

;C I        -- n   R: sys1 sys2 -- sys1 sys2
;C                   get the innermost loop index
        .drw link
        .set link,*+1
        .db  0,1,"I"
II:     dec r0      ; push old TOS
        mov a,dph
        movx @r0,a
        dec r0
        mov a,dpl
        movx @r0,a
        mov r1,sp   ; get copy of SP
        dec r1      ; skip return address
        dec r1
        dec r1      ; skip hi byte of fudge
        clr c
        mov a,r6    ; index-fudge = true index
        subb a,@r1
        mov dpl,a
        inc r1
        mov a,r7
        subb a,@r1
        mov dph,a   ; leaves true index on TOS
        ret

;C J        -- n   R: 4*sys -- 4*sys
;C                      get the second loop index
        .drw link
        .set link,*+1
        .db  0,1,"J"
JJ:     dec r0      ; push old TOS
        mov a,dph
        movx @r0,a
        dec r0
        mov a,dpl
        movx @r0,a
        mov r1,sp   ; get copy of SP
        dec r1      ; skip return address
        dec r1
        dec r1      ; skip inner fudge factor
        dec r1
        mov dr3,@r1  ; outer index hi
        dec r1
        mov dr2,@r1  ; outer index lo
        dec r1
        mov b,@r1   ; outer fudge hi
        dec r1
        clr c
        mov a,r2    ; index-fudge = true index
        subb a,@r1
        mov dpl,a
        mov a,r3
        subb a,b
        mov dph,a   ; leaves true index on TOS
        ret

;C UNLOOP   --   R: sys1 sys2 --  drop loop parms
        .drw link
        .set link,*+1
        .db  0,6,"UNLOOP"
UNLOOP: pop dr3     ; get return adrs in r3:r2
        pop dr2
        dec sp      ; discard fudge factor
        dec sp
        pop dr7     ; restore previous loop index
        pop dr6
        push dr2    ; restore return addr
        push dr3
        ret

; MULTIPLY AND DIVIDE ===========================

;C UM*     u1 u2 -- ud   unsigned 16x16->32 mult.
        .drw link
        .set link,*+1
        .db  0,3,"UM*"
UMSTAR: movx a,@r0      ; u1 Lo in r1
        mov r1,a
        inc r0          ; u1 Hi in mem!

        mov a,r1        ; u1L*u2L -> B:A -> r3:r2
        mov b,dpl
        mul ab
        mov r2,a
        mov r3,b

        mov a,r1        ; u1L*u2H -> B:A
        mov b,dph       ;       add into r4:r3
        mul ab
        add a,r3
        mov r3,a
        clr a
        addc a,b
        mov r4,a

        movx a,@r0      ; u1H*u2L -> B:A
        mov b,dpl       ;       add into r4:r3
        mul ab
        add a,r3
        mov r3,a
        mov a,r4
        addc a,b
        mov r4,a
        clr a           ;       w/possible cy->r5
        addc a,#0
        mov r5,a

        movx a,@r0      ; u1H*u2H -> B:A
        mov b,dph       ;       add into r5:r4
        mul ab
        add a,r4
        mov r4,a
        mov a,r5
        addc a,b
        mov dph,a       ; result in dph:r4:r3:r2
        mov dpl,r4
        mov a,r3
        movx @r0,a
        dec r0
        mov a,r2
        movx @r0,a
        ret

;C UM/MOD   ud u1 -- u2 u3     unsigned 32/16->16
        .drw link
        .set link,*+1
        .db  0,6,"UM/MOD"
UMSLASHMOD:             ; DPH:DPL = divisor
        movx a,@r0      ; r2:r3:r4:r5 = dividend
        mov r3,a        ; note stack order:
        inc r0          ;  ^      xxxx
        movx a,@r0      ;  |      xxxx
        mov r2,a        ; high   hi byte \ low
        inc r0          ; adrs   lo byte /  cell
        movx a,@r0      ;        hi byte \ high
        mov r5,a        ;  R0--> lo byte /  cell
        inc r0          ;  on    -------
        movx a,@r0      ;  entry
        mov r4,a        ;
        mov r1,#17      ; loop counter
        clr c
        sjmp div2
div1:   ; division loop
        mov a,r3
        rlc a
        mov r3,a
        mov a,r2
        rlc a
        mov r2,a
        jnc div3
        ; here cy=1, cy:r2:r3 is a 17 bit value,
        ; we know we can subtract divisor
        clr c
        mov a,r3
        subb a,dpl
        mov r3,a
        mov a,r2
        subb a,dph
        mov r2,a
        clr c
        sjmp div4
div3:   ; here cy=0, r2:r3 is a 16 bit value
        clr c
        mov a,r3
        subb a,dpl
        mov r3,a
        mov a,r2
        subb a,dph
        mov r2,a
        jnc div4
        ; borrow occurred -- undo the subtract
        mov a,r3
        add a,dpl
        mov r3,a
        mov a,r2
        addc a,dph
        mov r2,a
        setb c
div4:   ; here cy=0 if subtracted, cy=1 if not
        cpl c
div2:   mov a,r5
        rlc a
        mov r5,a
        mov a,r4
        rlc a
        mov r4,a
        djnz r1,div1
        mov dpl,r5      ; put quotient in TOS
        mov dph,r4
        mov a,r2        ; push remainder on stack
        movx @r0,a
        dec r0
        mov a,r3
        movx @r0,a
        ret

; BLOCK AND STRING OPERATIONS ===================

;C FILL    c-addr u char --  fill Data mem w/char
        .drw link
        .set link,*+1
        .db  0,4,"FILL"
FILL:   mov r4,dpl      ; stash char temporarily
        movx a,@r0      ; get count in r3:r2
        mov r2,a
        inc r0
        movx a,@r0
        mov r3,a
        inc r0
        movx a,@r0      ; get addr in DPTR
        mov dpl,a
        inc r0
        movx a,@r0
        mov dph,a
        inc r0
        mov a,r4        ; get char in A
        inc r3          ; adjust r3,r2 for djnz loop
        inc r2
        sjmp filltest
fillloop: movx @dptr,a
          inc dptr
filltest: djnz r2,fillloop
          djnz r3,fillloop
        ajmp poptos     ; pop new TOS

;Z I->D     c-addr1 c-addr2 u --  move Code->Data
; Block move from Code space to Data space.
;   ?DUP IF
;     OVER + SWAP DO
;       DUP IC@ I C! 1+
;     LOOP DUP
;   THEN 2DROP ;
        .drw link
        .set link,*+1
        .db  0,4,"I->D"
ITOD:   acall QDUP
        acall zerosense
        jz itod2
        acall OVER
        acall PLUS
        acall SWOP
        acall XDO
itod1:  acall DUP
        acall ICFETCH
        acall II
        acall CSTORE
        acall ONEPLUS
        acall loopsense
        jz itod1
        acall UNLOOP
        acall DUP
itod2:  acall DROP
        ajmp DROP

;Z D->I     c-addr1 c-addr2 u --  move Data->Code
; Block move from Data space to Code space.
; On the 8051 this is identical to CMOVE.
        .drw link
        .set link,*+1
        .db  0,4,"D->I"
DTOI:   sjmp CMOVE

;X CMOVE   c-addr1 c-addr2 u --  move from bottom
; as defined in the ANSI optional String word set
; On byte machines, CMOVE and CMOVE> are logical
; factors of MOVE.  They are easy to implement on
; CPUs which have a block-move instruction.
;   ?DUP IF
;     OVER + SWAP DO
;       DUP C@ I C! 1+
;     LOOP DUP
;   THEN 2DROP ;
        .drw link
        .set link,*+1
        .db  0,5,"CMOVE"
CMOVE:  acall QDUP
        acall zerosense
        jz cmove2
        acall OVER
        acall PLUS
        acall SWOP
        acall XDO
cmove1: acall DUP
        acall CFETCH
        acall II
        acall CSTORE
        acall ONEPLUS
        acall loopsense
        jz cmove1
        acall UNLOOP
        acall DUP
cmove2: acall DROP
        ajmp DROP

;X CMOVE>  c-addr1 c-addr2 u --  move from top
; as defined in the ANSI optional String word set
;   ?DUP IF
;     1- ROT OVER +   \ addr2 u-1 addr1+u-1
;     ROT ROT OVER +  \ addr1+u-1 addr2 addr2+u-1
;     DO
;       DUP C@ I C! 1-
;     -1 +LOOP DUP
;   THEN 2DROP ;
        .drw link
        .set link,*+1
        .db  0,6,"CMOVE>"
CMOVEUP: acall QDUP
        acall zerosense
        jz cmovu2
        acall ONEMINUS
        acall ROT
        acall OVER
        acall PLUS
        acall ROT
        acall ROT
        acall OVER
        acall PLUS
        acall XDO
cmovu1: acall DUP
        acall CFETCH
        acall II
        acall CSTORE
        acall ONEMINUS
        acall LIT
        .drw -1
        acall pluslpsense
        jz cmovu1
        acall UNLOOP
        acall DUP
cmovu2: acall DROP
        ajmp DROP

;Z SKIP   c-addr u c -- c-addr' u'
;Z                          skip matching chars
; Although SKIP, SCAN, and S= are perhaps not the
; ideal factors of WORD and FIND, they closely
; follow the string operations available on many
; CPUs, and so are easy to implement and fast.
        .drw link
        .set link,*+1
        .db  0,4,"SKIP"
XSKIP:  mov r4,dpl      ; stash char temporarily
        movx a,@r0      ; get count in r3:r2
        mov r2,a
        inc r0
        movx a,@r0
        mov r3,a
        inc r0
        movx a,@r0      ; get addr in DPTR
        mov dpl,a
        inc r0
        movx a,@r0
        mov dph,a
        inc r3          ; adj r3,r2 for djnz loop
        inc r2
        sjmp skiptest
skiploop: movx a,@dptr  ; get char
          xrl a,r4      ; compare with desired
          jnz skipmis   ; exit if mismatch
          inc dptr
skiptest: djnz r2,skiploop
          djnz r3,skiploop
        ; count exhausted; r3:r2=0000,
        ;    adrs points past last char
skipmis: ; either mismatch, or count exhausted
        mov a,dph      ; push updated addr
        movx @r0,a
        dec r0
        mov a,dpl
        movx @r0,a
        dec r2          ; adjust r3,r2 back
        dec r3          ;   to a normal count,
        mov dph,r3      ; put in TOS,
        mov dpl,r2
        inc dptr        ; adjust for extra decr
        ret

;Z SCAN    c-addr u c -- c-addr' u'
;Z                      find matching char
        .drw link
        .set link,*+1
        .db  0,4,"SCAN"
SCAN:   mov r4,dpl      ; stash char temporarily
        movx a,@r0      ; get count in r3:r2
        mov r2,a
        inc r0
        movx a,@r0
        mov r3,a
        inc r0
        movx a,@r0      ; get addr in DPTR
        mov dpl,a
        inc r0
        movx a,@r0
        mov dph,a
        inc r3          ; adj r3,r2 for djnz loop
        inc r2
        sjmp scantest
scanloop: movx a,@dptr  ; get char
          xrl a,r4      ; compare with desired
          jz scanmis    ; exit if match
          inc dptr
scantest: djnz r2,scanloop
          djnz r3,scanloop
        ; count exhausted; r3:r2=0000
        ;   adrs points past last char
scanmis: ; either match, or count exhausted
        mov a,dph      ; push updated addr
        movx @r0,a
        dec r0
        mov a,dpl
        movx @r0,a
        dec r2          ; adjust r3,r2 back
        dec r3          ;   to a normal count,
        mov dph,r3      ; put in TOS,
        mov dpl,r2
        inc dptr        ; adjust for extra decr
        ret

;Z S=    c-addr1 c-addr2 u -- n   string compare
;Z             n<0: s1<s2, n=0: s1=s2, n>0: s1>s2
; Omitted in 8051 version.

;Z N=    c-addr1 c-addr2 u -- n   string:name cmp
;Z             n<0: s1<s2, n=0: s1=s2, n>0: s1>s2
;   ?DUP IF
;       OVER + SWAP DO
;           DUP C@ I IC@ -
;           ?DUP IF NIP UNLOOP EXIT THEN
;       1+ LOOP DUP
;   THEN 2DROP 0 ;
; Harvard model: c-addr1=>Data, c-addr2=>Code.
        .drw link
        .set link,*+1
        .db  0,2,"N="
NEQUAL: push dr7
        push dr6
        mov r2,dpl      ; count
        mov r3,dph
        movx a,@r0      ; get Code addr in r5:r4
        mov r4,a
        inc r0
        movx a,@r0
        mov r5,a
        inc r0
        movx a,@r0      ; get Data addr in r7:r6
        mov r6,a
        inc r0
        movx a,@r0
        mov r7,a
        inc r0
        inc r3          ; adjust for djnz loop
        inc r2
        sjmp Nequtest
Nequloop: mov dph,r5    ; get Code char
        mov dpl,r4
        clr a
        movc a,@a+dptr
        mov r1,a
        inc dptr
        mov r5,dph
        mov r4,dpl
        mov dph,r7      ; get Data char
        mov dpl,r6
        movx a,@dptr
        inc dptr
        mov r7,dph
        mov r6,dpl
        clr c           ; Data-Code
        subb a,r1
        jnz Nequfail
Nequtest: djnz r2,Nequloop
        djnz r3,Nequloop
        mov dph,r3      ; strings match, r3=0,
        mov dpl,r3      ;  so make TOS=0
        sjmp Nequdone
Nequfail: subb a,acc    ; -1 if cy set, 0 if clr
        mov dph,a       ; (Data<Code) (Data>Code)
        orl a,#1        ; TOS = FFFF or 0001
        mov dpl,a
Nequdone: pop dr6
        pop dr7
        ret

;         acall QDUP
;         acall zerosense
;         jz sequ3
;         acall OVER
;         acall PLUS
;         acall SWOP
;         acall XDO
; sequ1:  acall DUP
;         acall CFETCH
;         acall II
;         acall ICFETCH
;         acall MINUS
;         acall QDUP
;         acall zerosense
;         jz sequ2
;         acall NIP
;         acall UNLOOP
;         ret
; sequ2:  acall ONEPLUS
;         acall loopsense
;         jz sequ1
;         acall UNLOOP
;         acall DUP
; sequ3:  acall DROP
;         acall DROP
;         acall LIT
;         .drw 0
;         ret

; ===============================================
; CamelForth for the Intel 8051
; (c) 1994 Bradford J. Rodriguez
; Permission is granted to freely copy, modify,
; and distribute this program for personal or
; educational use.  Commercial inquiries should
; be directed to the author at 221 King St. E.,
; #32, Hamilton, Ontario L8N 1B5 Canada
;
; CAMEL51D.AZM: CPU and Model Dependencies
;   Source code is for the A51 assembler.
;   Forth words are documented as follows:
;*   NAME     stack -- stack    description
;   Word names in upper case are from the ANS
;   Forth Core word set.  Names in lower case are
;   "internal" implementation words & extensions.
;
; Subroutine-Threaded Forth model for Intel 8051
;   cell size is   16 bits (2 bytes)
;   char size is    8 bits (1 byte)
;   address unit is 8 bits (1 byte), i.e.,
;       addresses are byte-aligned.
; ===============================================

; ALIGNMENT AND PORTABILITY OPERATORS ===========
; Many of these are synonyms for other words,
; and so are defined as CODE words.

;C ALIGN    --                         align HERE
        .drw link
        .set link,*+1
        .db  0,5,"ALIGN"
ALIGN:  ret                     ; noop!

;C ALIGNED  addr -- a-addr       align given addr
        .drw link
        .set link,*+1
        .db  0,7,"ALIGNED"
ALIGNED: ret                    ; noop!

;Z CELL     -- n                 size of one cell
        .drw link
        .set link,*+1
        .db  0,4,"CELL"
CELL:   acall DOCON
        .drw 2

;C CELL+    a-addr1 -- a-addr2      add cell size
;   2 + ;
        .drw link
        .set link,*+1
        .db  0,5,"CELL+"
CELLPLUS: inc dptr
          inc dptr
          ret

;C CELLS    n1 -- n2            cells->adrs units
        .drw link
        .set link,*+1
        .db  0,5,"CELLS"
CELLS:  ajmp twostar

;C CHAR+    c-addr1 -- c-addr2   add char size
        .drw link
        .set link,*+1
        .db  0,5,"CHAR+"
CHARPLUS: inc dptr
        ret

;C CHARS    n1 -- n2            chars->adrs units
        .drw link
        .set link,*+1
        .db  0,5,"CHARS"
CHARS:  ret

;C >BODY    xt -- a-addr      adrs of CREATE data
;   3 + I@ ;                   8051 (3 byte CALL)
        .drw link
        .set link,*+1
        .db  0,5,">BODY"
TOBODY: inc dptr
        inc dptr
        inc dptr
        ajmp IFETCH

; Note that I@ and I! use lo,hi byte order (same
; as 8086 and Z80), but the 8051 LCALL and LJMP
; addresses are stored hi,lo.  This difference
; is encapsulated within ,XT !CF and ,CF .

;X COMPILE,  xt --         append execution token
; I called this word ,XT before I discovered that
; it is defined in the ANSI standard as COMPILE,.
; On a DTC Forth this simply appends xt (like , )
; but on an STC Forth this must append 'CALL xt'.
;   012 IC, >< I, ;  12h = 8051 Lcall instruction
        .drw link
        .set link,*+1
        .db  0,8,"COMPILE,"
COMMAXT: acall LIT
        .drw 0x12
        lcall ICCOMMA
        acall SWAPBYTES
        ljmp ICOMMA

;Z !CF    adrs cfa --   set code action of a word
;   012 OVER IC!         store 'LCALL adrs' instr
;   1+ SWAP >< SWAP I! ;     8051 Harvard VERSION
; Depending on the implementation this could
; append CALL adrs or JUMP adrs.
        .drw link
        .set link,*+1
        .db  0,3,"!CF"
STORECF: acall LIT
        .drw 0x12
        acall OVER
        acall ICSTORE
        acall ONEPLUS
        acall SWOP
        acall SWAPBYTES
        acall SWOP
        ajmp ISTORE

;Z ,CF    adrs --             append a code field
;   012 IC, >< I, ;          8051 Harvard VERSION
        .drw link
        .set link,*+1
        .db  0,3,",CF"
COMMACF: sjmp COMMAXT

;Z !COLON   --       change code field to docolon
;   -5 IALLOT ;           8051 Harvard VERSION
; This should be used immediately after CREATE.
; This is made a distinct word, because on an STC
; Forth, colon definitions have no code field.
        .drw link
        .set link,*+1
        .db  0,6,"!COLON"
STORCOLON: acall LIT
        .drw -5
        ljmp IALLOT

; ***
; This is approximately the end of the first 2K
; block.  CALLs and JMPs crossing this boundary
; must use the Long form.
; ***

;Z ,EXIT    --      append hi-level EXIT action
;   022 IC, ;               8051 VERSION
; This is made a distinct word, because on an STC
; Forth, it appends a RET instruction, not an xt.
        .drw link
        .set link,*+1
        .db  0,5,",EXIT"
CEXIT:  lcall LIT
        .drw 0x22
        ljmp ICCOMMA

; CONTROL STRUCTURES ============================
; These words allow Forth control structure words
; to be defined portably.

;Z ,BRANCH   xt --    append a branch instruction
; xt is the branch operator to use, e.g. qbranch
; or (loop).  It does NOT append the destination
; address.  On the 8051 this compiles
;         LCALL xt   jz-opcode
; unless xt=0 in which case the LCALL is omitted
; and an 'sjmp' instruction is compiled.
;   ?DUP IF ,XT 060 ELSE 080 THEN IC, ;
        .drw link
        .set link,*+1
        .db  0,7,",BRANCH"
COMMABRANCH: lcall QDUP
        lcall zerosense
        jz combr1
        lcall COMMAXT   ; LCALL sense-routine
        lcall LIT
        .drw 0x60       ; jz opcode
        ljmp ICCOMMA
combr1: lcall LIT
        .drw 0x80       ; sjmp opcode
        ljmp ICCOMMA

; high level code may use 'branc0x as an argument
; to ,BRANCH:
        .equ branch,0

; ;Z ,DEST   dest --        append a branch address
; This appends the given destination address to
; the branch instruction.  On the 8051 this is a
; one-byte relative address.
;   IHERE 1+ - IC, ;
        .drw link
        .set link,*+1
        .db  0,5,",DEST"
COMMADEST: lcall IHERE
        lcall ONEPLUS
        lcall MINUS
        ajmp ICCOMMA

;Z !DEST   dest adrs --    change a branch dest'n
; Changes the destination address found at 'adrs'
; to the given 'dest'.  On the 8051 this is a
; one-byte relative address.
;   TUCK 1+ - SWAP IC! ;
        .drw link
        .set link,*+1
        .db  0,5,"!DEST"
STOREDEST: lcall TUCK
        lcall ONEPLUS
        lcall MINUS
        lcall SWOP
        ljmp ICSTORE

;Z ,UNLOOP  --       append an UNloop instruction
; Used after a LOOP or +LOOP is compiled.
; Required on the 8051 because the loop branch
; must be followed by UNLOOP.  No-op on Z80.
;   ['] UNLOOP ,XT ;
        .drw link
        .set link,*+1
        .db  0,7,",UNLOOP"
COMMAUNLOOP: lcall LIT
        .drw UNLOOP
        ljmp COMMAXT

; HEADER STRUCTURE ==============================
; The structure of the Forth dictionary headers
; (name, link, immediate flag, and "smudge" bit)
; does not necessarily differ across CPUs.  This
; structure is not easily factored into distinct
; "portable" words; instead, it is implicit in
; the definitions of FIND and CREATE, and also in
; NFA>LFA, NFA>CFA, IMMED?, IMMEDIATE, HIDE, and
; REVEAL.  These words must be (substantially)
; rewritten if either the header structure or its
; inherent assumptions are changed.

; ===============================================
; CamelForth for the Intel 8051
; (c) 1994 Bradford J. Rodriguez
; Permission is granted to freely copy, modify,
; and distribute this program for personal or
; educational use.  Commercial inquiries should
; be directed to the author at 221 King St. E.,
; #32, Hamilton, Ontario L8N 1B5 Canada
;
; CAMEL51H.AZM: High Level Words
;   Source code is for the A51 assembler.
;   Forth words are documented as follows:
;x   NAME     stack -- stack    description
; ===============================================

; SYSTEM VARIABLES & CONSTANTS ==================

;C BL      -- char                 an ASCII space
        .drw link
        .set link,*+1
        .db  0,2,"BL"
BL:     lcall docon
        .drw 0x20

;Z TIBSIZE  -- n                      size of TIB
        .drw link
        .set link,*+1
        .db  0,7,"TIBSIZE"
TIBSIZE: lcall docon
        .drw 126          ; 2 chars safety zone

;X TIB     -- a-addr        Terminal Input Buffer
;  HEX -80 USER TIB         below user area
        .drw link
        .set link,*+1
        .db  0,3,"TIB"
TIB:    lcall douser
        .drw -128

;Z u0      -- a-addr       current user area adrs
;  0 USER U0
        .drw link
        .set link,*+1
        .db  0,2,"U0"
U0:     lcall douser
        .drw 0

;C >IN     -- a-addr        holds offset into TIB
;  2 USER >IN
        .drw link
        .set link,*+1
        .db  0,3,">IN"
TOIN:   lcall douser
        .drw 2

;C BASE    -- a-addr       holds conversion radix
;  4 USER BASE
        .drw link
        .set link,*+1
        .db  0,4,"BASE"
BASE:   lcall douser
        .drw 4

;C STATE   -- a-addr         holds compiler state
;  6 USER STATE
        .drw link
        .set link,*+1
        .db  0,5,"STATE"
STATE:  lcall douser
        .drw 6

;Z DP      -- a-addr         holds dictionary ptr
;  8 USER DP
        .drw link
        .set link,*+1
        .db  0,2,"DP"
DP:     lcall douser
        .drw 8

;Z 'SOURCE  -- a-addr        two cells: len, adrs
; 10 USER 'SOURCE
        .drw link
        .set link,*+1
        .db  0,7,0x27,"SOURCE"  ; 27h = '
TICKSOURCE: lcall douser
        .drw 10

;Z LATEST    -- a-addr         last word in dict.
;   14 USER LATEST
        .drw link
        .set link,*+1
        .db  0,6,"LATEST"
LATEST: lcall douser
        .drw 14

;Z HP       -- a-addr                HOLD pointer
;   16 USER HP
        .drw link
        .set link,*+1
        .db  0,2,"HP"
HP:     lcall douser
        .drw 16

;Z LP       -- a-addr         Leave-stack pointer
;   18 USER LP
        .drw link
        .set link,*+1
        .db  0,2,"LP"
LP:     lcall douser
        .drw 18

;Z IDP    -- a-addr        ROM dictionary pointer
;   20 USER IDP
        .drw link
        .set link,*+1
        .db  0,3,"IDP"
IDP:  lcall douser
        .drw 20

;Z S0       -- a-addr      end of parameter stack
        .drw link
        .set link,*+1
        .db  0,2,"S0"
S0:     lcall douser
        .drw 0x0FF

;X PAD       -- a-addr            user PAD buffer
;                         = end of hold area!
        .drw link
        .set link,*+1
        .db  0,3,"PAD"
PAD:    lcall douser
        .drw 0x128

;Z L0       -- a-addr       bottom of Leave stack
        .drw link
        .set link,*+1
        .db  0,2,"L0"
L0:     lcall douser
        .drw 0x180

;Z R0       -- a-addr         end of return stack
; on the 8051 this is the init value of RSP (SP).
        .drw link
        .set link,*+1
        .db  0,2,"R0"
RP0:    lcall docon     ; note R0 is an 8051 reg!
        .drw 8

;Z uinit    -- addr  initial values for user area
        .drw link
        .set link,*+1
        .db  0,5,"UINIT"
UINIT:  lcall dorom
        .drw 0,0,10,0   ; reserved,>IN,BASE,STATE
        .drw dataram    ; DP
        .drw 0,0        ; SOURCE init'd elsewhere
        .drw lastword   ; LATEST
        .drw 0,0        ; HP,LP init'd elsewhere
        .drw coderam    ; IDP

;Z #init    -- n    #bytes of user area init data
        .drw link
        .set link,*+1
        .db  0,5,"#INIT"
NINIT:  lcall docon
        .drw 22

; ARITHMETIC OPERATORS ==========================

;C S>D    n -- d           single -> double prec.
;   DUP 0< ;
        .drw link
        .set link,*+1
        .db  0,3,"S>D"
STOD:   lcall DUP
        ljmp ZEROLESS

;Z ?NEGATE  n1 n2 -- n3  negate n1 if n2 negative
;   0< IF NEGATE THEN ;        ...a common factor
        .drw link
        .set link,*+1
        .db  0,7,"?NEGATE"
QNEGATE: lcall ZEROLESS
        lcall zerosense
        jz qneg1
        lcall NEGATE
qneg1:  ret

;C ABS     n1 -- +n2     absolute value
;   DUP ?NEGATE ;
        .drw link
        .set link,*+1
        .db  0,3,"ABS"
ABS:    lcall DUP
        sjmp QNEGATE

;X DNEGATE   d1 -- d2     negate double precision
;   SWAP INVERT SWAP INVERT 1 M+ ;
        .drw link
        .set link,*+1
        .db  0,7,"DNEGATE"
DNEGATE: lcall SWOP
        lcall INVERT
        lcall SWOP
        lcall INVERT
        lcall LIT
        .drw 1
        ljmp MPLUS

;Z ?DNEGATE  d1 n -- d2   negate d1 if n negative
;   0< IF DNEGATE THEN ;       ...a common factor
        .drw link
        .set link,*+1
        .db  0,8,"?DNEGATE"
QDNEGATE: lcall ZEROLESS
        lcall zerosense
        jz dneg1
        acall DNEGATE
DNEG1:  ret

;X DABS     d1 -- +d2    absolute value dbl.prec.
;   DUP ?DNEGATE ;
        .drw link
        .set link,*+1
        .db  0,4,"DABS"
DABS:   lcall DUP
        sjmp QDNEGATE

;C M*     n1 n2 -- d    signed 16*16->32 multiply
;   2DUP XOR >R        carries sign of the result
;   SWAP ABS SWAP ABS UM*
;   R> ?DNEGATE ;
        .drw link
        .set link,*+1
        .db  0,2,"M*"
MSTAR:  acall TWODUP
        lcall XOR
        lcall TOR
        lcall SWOP
        acall ABS
        lcall SWOP
        acall ABS
        lcall UMSTAR
        lcall RFROM
        sjmp QDNEGATE

;C SM/REM   d1 n1 -- n2 n3   symmetric signed div
;   2DUP XOR >R              sign of quotient
;   OVER >R                  sign of remainder
;   ABS >R DABS R> UM/MOD
;   SWAP R> ?NEGATE
;   SWAP R> ?NEGATE ;
; Ref. dpANS-6 section 3.2.2.1.
        .drw link
        .set link,*+1
        .db  0,6,"SM/REM"
SMSLASHREM: acall TWODUP
        lcall XOR
        lcall TOR
        lcall OVER
        lcall TOR
        acall ABS
        lcall TOR
        acall DABS
        lcall RFROM
        lcall UMSLASHMOD
        lcall SWOP
        lcall RFROM
        acall QNEGATE
        lcall SWOP
        lcall RFROM
        ajmp  QNEGATE

;C FM/MOD   d1 n1 -- n2 n3   floored signed div'n
;   DUP >R              divisor
;   2DUP XOR >R         sign of quotient
;   >R                  divisor
;   DABS R@ ABS UM/MOD
;   SWAP R> ?NEGATE SWAP  apply sign to remainder
;   R> 0< IF              if quotient negative,
;       NEGATE
;       OVER IF             if remainder nonzero,
;         R@ ROT -  SWAP 1-     adjust rem,quot
;       THEN
;   THEN  R> DROP ;
; Ref. dpANS-6 section 3.2.2.1.
        .drw link
        .set link,*+1
        .db  0,6,"FM/MOD"
FMSLASHMOD: lcall DUP
        lcall TOR
        acall TWODUP
        lcall XOR
        lcall TOR
        lcall TOR
        acall DABS
        lcall RFETCH
        acall ABS
        lcall UMSLASHMOD
        lcall SWOP
        lcall RFROM
        acall QNEGATE
        lcall SWOP
        lcall RFROM
	lcall ZEROLESS
        lcall zerosense
        jz fmmod1
        lcall NEGATE
        lcall OVER
        lcall zerosense
        jz fmmod1
        lcall RFETCH
        lcall ROT
        lcall MINUS
        lcall SWOP
        lcall ONEMINUS
fmmod1: lcall RFROM
        lcall DROP
	ret

;C *      n1 n2 -- n3             signed multiply
;   M* DROP ;
        .drw link
        .set link,*+1
        .db  0,1,"*"
STAR:   acall MSTAR
        ljmp DROP

;C /MOD   n1 n2 -- n3 n4     signed divide/rem'dr
;   >R S>D R> FM/MOD ;
        .drw link
        .set link,*+1
        .db  0,4,"/MOD"
SLASHMOD: lcall TOR
        acall STOD
        lcall RFROM
        ajmp FMSLASHMOD

;C /      n1 n2 -- n3               signed divide
;   /MOD nip ;
        .drw link
        .set link,*+1
        .db  0,1,"/"
SLASH:  acall SLASHMOD
        ljmp NIP

;C MOD    n1 n2 -- n3            signed remainder
;   /MOD DROP ;
        .drw link
        .set link,*+1
        .db  0,3,"MOD"
MOD:    acall SLASHMOD
        ljmp DROP

;C */MOD  n1 n2 n3 -- n4 n5    n1*n2/n3, rem&quot
;   >R M* R> FM/MOD ;
        .drw link
        .set link,*+1
        .db  0,5,"*/MOD"
SSMOD:  lcall TOR
        acall MSTAR
        lcall RFROM
        ajmp FMSLASHMOD

;C */     n1 n2 n3 -- n4                 n1*n2/n3
;   */MOD nip ;
        .drw link
        .set link,*+1
        .db  0,2,"*/"
STARSLASH: acall SSMOD
        ljmp NIP

;C MAX    n1 n2 -- n3              signed maximum
;   2DUP < IF SWAP THEN DROP ;
        .drw link
        .set link,*+1
        .db  0,3,"MAX"
MAX:    acall TWODUP
        lcall LESS
        lcall zerosense
        jz max1
        lcall SWOP
max1:   ljmp DROP

;C MIN    n1 n2 -- n3              signed minimum
;   2DUP > IF SWAP THEN DROP ;
        .drw link
        .set link,*+1
        .db  0,3,"MIN"
MIN:    acall TWODUP
        lcall GREATER
        lcall zerosense
        jz min1
        lcall SWOP
min1:   ljmp DROP

; DOUBLE OPERATORS ==============================

;C 2@    a-addr -- x1 x2            fetch 2 cells
;   DUP CELL+ @ SWAP @ ;
;   the lower address will appear on top of stack
        .drw link
        .set link,*+1
        .db  0,2,"2@"
TWOFETCH: lcall DUP
        lcall CELLPLUS
        lcall FETCH
        lcall SWOP
        ljmp FETCH

;C 2!    x1 x2 a-addr --            store 2 cells
;   SWAP OVER ! CELL+ ! ;
;   the top of stack is stored at the lower adrs
        .drw link
        .set link,*+1
        .db  0,2,"2!"
TWOSTORE: lcall SWOP
        lcall OVER
        lcall STORE
        lcall CELLPLUS
        ljmp STORE

;C 2DROP  x1 x2 --                   drop 2 cells
;   DROP DROP ;
        .drw link
        .set link,*+1
        .db  0,5,"2DROP"
TWODROP: lcall DROP
        ljmp DROP

;C 2DUP   x1 x2 -- x1 x2 x1 x2    dup top 2 cells
;   OVER OVER ;
        .drw link
        .set link,*+1
        .db  0,4,"2DUP"
TWODUP: lcall OVER
        ljmp OVER

;C 2SWAP  x1 x2 x3 x4 -- x3 x4 x1 x2  per diagram
;   ROT >R ROT R> ;
        .drw link
        .set link,*+1
        .db  0,5,"2SWAP"
TWOSWAP: lcall ROT
        lcall TOR
        lcall ROT
        lcall RFROM     ; can't ljmp RFROM!
        ret

;C 2OVER  x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2
;   >R >R 2DUP R> R> 2SWAP ;
        .drw link
        .set link,*+1
        .db  0,5,"2OVER"
TWOOVER: lcall TOR
        lcall TOR
        acall TWODUP
        lcall RFROM
        lcall RFROM
        ajmp TWOSWAP

; INPUT/OUTPUT ==================================

;C COUNT   c-addr1 -- c-addr2 u  counted->adr/len
;   DUP CHAR+ SWAP C@ ;
        .drw link
        .set link,*+1
        .db  0,5,"COUNT"
COUNT:  lcall DUP
        lcall CHARPLUS
        lcall SWOP
        ljmp CFETCH

;C CR      --                      output newline
;   0D EMIT 0A EMIT ;
        .drw link
        .set link,*+1
        .db  0,2,"CR"
CR:     lcall lit
        .drw 0x0d
        lcall EMIT
        lcall lit
        .drw 0x0a
        ljmp EMIT

;C SPACE   --                      output a space
;   BL EMIT ;
        .drw link
        .set link,*+1
        .db  0,5,"SPACE"
SPACE:  lcall BL
        ljmp EMIT

;C SPACES   n --                  output n spaces
;   BEGIN DUP WHILE SPACE 1- REPEAT DROP ;
        .drw link
        .set link,*+1
        .db 0,6,"SPACES"
SPACES:
SPCS1:  lcall DUP
        lcall zerosense
        jz SPCS2
        acall SPACE
        lcall ONEMINUS
        sjmp SPCS1
SPCS2:  ljmp DROP

;Z umin     u1 u2 -- u           unsigned minimum
;   2DUP U> IF SWAP THEN DROP ;
        .drw link
        .set link,*+1
        .db 0,4,"UMIN"
UMIN:   acall TWODUP
        lcall UGREATER
        lcall zerosense
        jz UMIN1
        lcall SWOP
UMIN1:  ljmp DROP

;Z umax    u1 u2 -- u            unsigned maximum
;   2DUP U< IF SWAP THEN DROP ;
        .drw link
        .set link,*+1
        .db 0,4,"UMAX"
UMAX:   acall TWODUP
        lcall ULESS
        lcall zerosense
        jz UMAX1
        lcall SWOP
UMAX1:  ljmp DROP

;C ACCEPT  c-addr +n -- +n'  get line from term'l
;   OVER + 1- OVER      -- sa ea a
;   BEGIN KEY           -- sa ea a c
;   DUP 0D <> WHILE
;       DUP EMIT        -- sa ea a c
;       DUP 8 = IF  DROP 1-    >R OVER R> UMAX
;             ELSE  OVER C! 1+ OVER UMIN
;       THEN            -- sa ea a
;   REPEAT              -- sa ea a c
;   DROP NIP SWAP - ;
        .drw link
        .set link,*+1
        .db 0,6,"ACCEPT"
ACCEPT: lcall OVER
        lcall PLUS
        lcall ONEMINUS
        lcall OVER
ACC1:   lcall KEY
        lcall DUP
        lcall LIT
        .drw 0x0D
        lcall NOTEQUAL
        lcall zerosense
        jz ACC5
        lcall DUP
        lcall EMIT
        lcall DUP
        lcall LIT
        .drw 8
        lcall EQUAL
        lcall zerosense
        jz ACC3
        lcall DROP
        lcall ONEMINUS
        lcall TOR
        lcall OVER
        lcall RFROM
        acall UMAX
        sjmp ACC4
ACC3:   lcall OVER
        lcall CSTORE
        lcall ONEPLUS
        lcall OVER
        acall UMIN
ACC4:   sjmp ACC1
ACC5:   lcall DROP
        lcall NIP
        lcall SWOP
        ljmp MINUS

;C TYPE    c-addr +n --       type line to term'l
;   ?DUP IF
;     OVER + SWAP DO I C@ EMIT LOOP
;   ELSE DROP THEN ;
        .drw link
        .set link,*+1
        .db 0,4,"TYPE"
TYPE:   lcall QDUP
        lcall zerosense
        jz TYP4
        lcall OVER
        lcall PLUS
        lcall SWOP
        lcall XDO
TYP3:   lcall II
        lcall CFETCH
        lcall EMIT
        lcall loopsense
        jz TYP3
        lcall UNLOOP
        sjmp TYP5
TYP4:   lcall DROP
TYP5:   ret

;Z (IS")    -- c-addr u      run-time code for S"
;   R> ICOUNT 2DUP + ALIGNED >R  ;
; Harvard model, for string stored in Code space
; e.g. as used by ."
        .drw link
        .set link,*+1
        .db 0,5,"(IS",0x22,")"
XISQUOTE: lcall RFROM
        acall ICOUNT
        acall TWODUP
        lcall PLUS
        ; lcall ALIGNED
        lcall TOR       ; do NOT ljmp TOR!
        ret

;Z (S")    -- c-addr u       run-time code for S"
;   R@ I@                     get Data address
;   R> CELL+ DUP IC@ CHAR+    -- Dadr Radr+2 n+1
;   2DUP + ALIGNED >R         -- Dadr Iadr n+1
;   >R OVER R> I>D            -- Dadr
;   COUNT ;
; Harvard model, for string stored in Code space
; which is copied to Data space.
        .drw link
        .set link,*+1
        .db 0,4,"(S",0x22,")"
XSQUOTE: lcall RFETCH
        lcall IFETCH
        lcall RFROM
        lcall CELLPLUS
        lcall DUP
        lcall ICFETCH
        lcall CHARPLUS
        acall TWODUP
        lcall PLUS
        ; lcall ALIGNED
        lcall TOR
        lcall TOR
        lcall OVER
        lcall RFROM
        lcall ITOD
        ajmp COUNT

;C IS"      --            compile in-line string
;   COMPILE (IS")  [ HEX ]
;   22 IWORD
;   IC@ 1+ ALIGNED IALLOT ; IMMEDIATE
; Harvard model: string is stored in Code space
        .drw link
        .set link,*+1
        .db IMMED,3,"IS",0x22
ISQUOTE: lcall LIT
        .drw XISQUOTE
        lcall COMMAXT
        lcall LIT
        .drw 0x22
        acall IWORD
        lcall ICFETCH
        lcall ONEPLUS
        ; lcall ALIGNED
        ajmp IALLOT

;C S"       --             compile in-line string
;   COMPILE (S")  [ HEX ]
;   HERE I,                     data address
;   22 IWORD
;   IC@ 1+ ALIGNED
;   DUP ALLOT IALLOT ; IMMEDIATE
; Harvard model: string is stored in Code space
        .drw link
        .set link,*+1
        .db IMMED,2,"S",0x22
SQUOTE: lcall LIT
        .drw XSQUOTE
        lcall COMMAXT
        acall HERE
        acall ICOMMA
        lcall LIT
        .drw 0x22
        acall IWORD
        lcall ICFETCH
        lcall ONEPLUS
        ; lcall ALIGNED
        lcall DUP
        acall ALLOT
        ajmp IALLOT

;C ."       --            compile string to print
;   POSTPONE IS"  POSTPONE ITYPE ; IMMEDIATE
        .drw link
        .set link,*+1
        .db IMMED,2,".",0x22
DOTQUOTE: acall ISQUOTE
        lcall LIT
        .drw ITYPE
        ljmp COMMAXT

;Z ICOUNT  c-addr1 -- c-addr2 u  counted->adr/len
;   DUP CHAR+ SWAP IC@ ;          from Code space
        .drw link
        .set link,*+1
        .db  0,6,"ICOUNT"
ICOUNT: lcall DUP
        lcall CHARPLUS
        lcall SWOP
        ljmp ICFETCH

;Z ITYPE   c-addr +n --       type line to term'l
;   ?DUP IF                       from Code space
;     OVER + SWAP DO I IC@ EMIT LOOP
;   ELSE DROP THEN ;
        .drw link
        .set link,*+1
        .db 0,5,"ITYPE"
ITYPE:  lcall QDUP
        lcall zerosense
        jz ITYP4
        lcall OVER
        lcall PLUS
        lcall SWOP
        lcall XDO
ITYP3:  lcall II
        lcall ICFETCH
        lcall EMIT
        lcall loopsense
        jz ITYP3
        lcall UNLOOP
        sjmp ITYP5
ITYP4:  lcall DROP
ITYP5:  ret


;Z IWORD     c -- c-addr       WORD to Code space
;   WORD
;   IHERE TUCK OVER C@ CHAR+ D->I ;
        .drw link
        .set link,*+1
        .db 0,5,"IWORD"
IWORD:  lcall XWORD
        acall IHERE
        lcall TUCK
        lcall OVER
        lcall CFETCH
        lcall CHARPLUS
        ljmp DTOI

; NUMERIC OUTPUT ================================
; Numeric conversion is done l.s.digit first, so
; the output buffer is built backwards in memory.

; Some double-precision arithmetic operators are
; needed to implement ANSI numeric conversion.

;Z UD/MOD   ud1 u2 -- u3 ud4     32/16->32 divide
;   >R 0 R@ UM/MOD  ROT ROT R> UM/MOD ROT ;
        .drw link
        .set link,*+1
        .db 0,6,"UD/MOD"
UDSLASHMOD: lcall TOR
        lcall LIT
        .drw 0
        lcall RFETCH
        lcall UMSLASHMOD
        lcall ROT
        lcall ROT
        lcall RFROM
        lcall UMSLASHMOD
        ljmp ROT

;Z UD*      ud1 d2 -- ud3      32*16->32 multiply
;   DUP >R UM* DROP  SWAP R> UM* ROT + ;
        .drw link
        .set link,*+1
        .db 0,3,"UD*"
UDSTAR: lcall DUP
        lcall TOR
        lcall UMSTAR
        lcall DROP
        lcall SWOP
        lcall RFROM
        lcall UMSTAR
        lcall ROT
        ljmp PLUS

;C HOLD  char --        add char to output string
;   -1 HP +!  HP @ C! ;
        .drw link
        .set link,*+1
        .db 0,4,"HOLD"
HOLD:   lcall LIT
        .drw -1
        acall HP
        lcall PLUSSTORE
        acall HP
        lcall FETCH
        ljmp CSTORE

;C <#    --              begin numeric conversion
;   PAD HP ! ;          (initialize Hold Pointer)
        .drw link
        .set link,*+1
        .db 0,2,"<#"
LESSNUM: acall PAD
        acall HP
        ljmp STORE

;Z >digit   n -- c            convert to 0..9A..Z
;   [ HEX ] DUP 9 > 7 AND + 30 + ;
        .drw link
        .set link,*+1
        .db 0,6,">DIGIT"
TODIGIT: lcall DUP
        lcall LIT
        .drw 9
        lcall GREATER
        lcall LIT
        .drw 7
        lcall AND
        lcall PLUS
        lcall LIT
        .drw 0x30
        ljmp PLUS

;C #     ud1 -- ud2     convert 1 digit of output
;   BASE @ UD/MOD ROT >digit HOLD ;
        .drw link
        .set link,*+1
        .db 0,1,"#"
NUM:    acall BASE
        lcall FETCH
        acall UDSLASHMOD
        lcall ROT
        acall TODIGIT
        sjmp HOLD

;C #S    ud1 -- ud2      convert remaining digits
;   BEGIN # 2DUP OR 0= UNTIL ;
        .drw link
        .set link,*+1
        .db 0,2,"#S"
NUMS:
NUMS1:
        acall NUM
        acall TWODUP
        lcall OR
        lcall ZEROEQUAL
        lcall zerosense
        jz NUMS1
        ret

;C #>    ud1 -- c-addr u    end conv., get string
;   2DROP HP @ PAD OVER - ;
        .drw link
        .set link,*+1
        .db 0,2,"#>"
NUMGREATER: acall TWODROP
        acall HP
        lcall FETCH
        acall PAD
        lcall OVER
        ljmp MINUS

;C SIGN  n --               add minus sign if n<0
;   0< IF 2D HOLD THEN ;
        .drw link
        .set link,*+1
        .db 0,4,"SIGN"
SIGN:   lcall ZEROLESS
        lcall zerosense
        jz SIGN1
        lcall LIT
        .drw 0x2D
        acall HOLD
SIGN1:  ret

;C U.    u --                  display u unsigned
;   <# 0 #S #> TYPE SPACE ;
        .drw link
        .set link,*+1
        .db 0,2,"U."
UDOT:   acall LESSNUM
        lcall LIT
        .drw 0
        acall NUMS
        acall NUMGREATER
        acall TYPE
        ajmp SPACE

;C .     n --                    display n signed
;   <# DUP ABS 0 #S ROT SIGN #> TYPE SPACE ;
        .drw link
        .set link,*+1
        .db 0,1,"."
DOT:    acall LESSNUM
        lcall DUP
        lcall ABS
        lcall LIT
        .drw 0
        acall NUMS
        lcall ROT
        acall SIGN
        acall NUMGREATER
        acall TYPE
        ajmp SPACE

;C DECIMAL  --         set number base to decimal
;   10 BASE ! ;
        .drw link
        .set link,*+1
        .db 0,7,"DECIMAL"
DECIMAL: lcall LIT
        .drw 10
        acall BASE
        ljmp STORE

;X HEX     --              set number base to hex
;   16 BASE ! ;
        .drw link
        .set link,*+1
        .db 0,3,"HEX"
HEX:    lcall LIT
        .drw 16
        acall BASE
        ljmp STORE

; DICTIONARY MANAGEMENT =========================

;C HERE    -- addr         returns dictionary ptr
;   DP @ ;
        .drw link
        .set link,*+1
        .db 0,4,"HERE"
HERE:   acall DP
        ljmp FETCH

;C ALLOT   n --          allocate n bytes in dict
;   DP +! ;
        .drw link
        .set link,*+1
        .db 0,5,"ALLOT"
ALLOT:  acall DP
        ljmp PLUSSTORE

;C ,    x --                  append cell to dict
;   HERE ! 1 CELLS ALLOT ;
        .drw link
        .set link,*+1
        .db 0,1,","
COMMA:  acall HERE
        lcall STORE
        lcall lit
        .drw 2
        ajmp ALLOT

;C C,   char --               append char to dict
;   HERE C! 1 CHARS ALLOT ;
        .drw link
        .set link,*+1
        .db 0,2,"C,"
CCOMMA: acall HERE
        lcall CSTORE
        lcall lit
        .drw 1
        ljmp ALLOT

; The following additional words support the
; "Harvard" model, with separate address spaces
; for Instructions (Code) and Data.  ANSI
; requires DP to manage the Data space, so a
; separate Instruction Dictionary Pointer, IDP,
; is added to manage the Code space.  Also added:
;   I@ IC@ I! IC!        (in the primitives)
;   IHERE IALLOT I, IC,
;   ITYPE ICOUNT WORD>I
; It should be possible to convert the Harvard
; implementation to a combined-code-and-data
; system, by equating these words to their
; Data-space counterparts.

;Z IHERE    -- addr    return Code dictionary ptr
;   IDP @ ;
        .drw link
        .set link,*+1
        .db 0,5,"IHERE"
IHERE:  acall IDP
        ljmp FETCH

;Z IALLOT   n --    allocate n bytes in Code dict
;   IDP +! ;
        .drw link
        .set link,*+1
        .db 0,6,"IALLOT"
IALLOT: acall IDP
        ljmp PLUSSTORE

;Z I,   x --             append cell to Code dict
;   IHERE I! 1 CELLS IALLOT ;
        .drw link
        .set link,*+1
        .db 0,2,"I,"
ICOMMA: acall IHERE
        lcall ISTORE
        lcall lit
        .drw 2
        sjmp IALLOT

;Z IC,  x --             append char to Code dict
;   IHERE IC! 1 CHARS IALLOT ;
        .drw link
        .set link,*+1
        .db 0,3,"IC,"
ICCOMMA: acall IHERE
        lcall ICSTORE
        lcall lit
        .drw 1
        sjmp IALLOT

; INTERPRETER ===================================
; Note that NFA>LFA, NFA>CFA, IMMED?, and FIND
; are dependent on the structure of the Forth
; header.  This may be common across many CPUs,
; or it may be different.

;C SOURCE   -- adr n         current input buffer
;   'SOURCE 2@ ;        length is at lower adrs
        .drw link
        .set link,*+1
        .db 0,6,"SOURCE"
SOURCE: acall TICKSOURCE
        ajmp TWOFETCH

;X /STRING  a u n -- a+n u-n          trim string
;   ROT OVER + ROT ROT - ;
        .drw link
        .set link,*+1
        .db 0,7,"/STRING"
SLASHSTRING: lcall ROT
        lcall OVER
        lcall PLUS
        lcall ROT
        lcall ROT
        ljmp MINUS

;Z >counted  src n dst --     copy to counted str
;   2DUP C! CHAR+ SWAP CMOVE ;
        .drw link
        .set link,*+1
        .db 0,8,">COUNTED"
TOCOUNTED: acall TWODUP
        lcall CSTORE
        lcall CHARPLUS
        lcall SWOP
        ljmp CMOVE

;C WORD   char -- c-addr n   word delim'd by char
;   DUP  SOURCE >IN @ /STRING   -- c c adr n
;   DUP >R   ROT SKIP           -- c adr' n'
;   OVER >R  ROT SCAN           -- adr" n"
;   DUP IF CHAR- THEN        skip trailing delim.
;   R> R> ROT -   >IN +!        update >IN offset
;   TUCK -                      -- adr' N
;   HERE >counted               --
;   HERE                        -- a
;   BL OVER COUNT + C! ;    append trailing blank
        .drw link
        .set link,*+1
        .db 0,4,"WORD"
XWORD:  lcall DUP
        acall SOURCE
        acall TOIN
        lcall FETCH
        acall SLASHSTRING
        lcall DUP
        lcall TOR
        lcall ROT
        lcall XSKIP
        lcall OVER
        lcall TOR
        lcall ROT
        lcall SCAN
        lcall DUP
        lcall zerosense
        jz WORD1
        lcall ONEMINUS  ; char-
WORD1:  lcall RFROM
        lcall RFROM
        lcall ROT
        lcall MINUS
        lcall TOIN
        lcall PLUSSTORE
        lcall TUCK
        lcall MINUS
        lcall HERE
        lcall TOCOUNTED
        lcall HERE
        lcall BL
        lcall OVER
        lcall COUNT
        lcall PLUS
        ljmp CSTORE

; ***
; This is approximately the end of the second 2K
; block.  CALLs and JMPs crossing this boundary
; must use the Long form.
; ***

;Z NFA>LFA   nfa -- lfa    name adr -> link field
;   3 - ;
        .drw link
        .set link,*+1
        .db 0,7,"NFA>LFA"
NFATOLFA: lcall LIT
        .drw 3
        ljmp MINUS

;Z NFA>CFA   nfa -- cfa    name adr -> code field
;   ICOUNT 7F AND + ;       mask off 'smudge' bit
; Harvard model.
        .drw link
        .set link,*+1
        .db 0,7,"NFA>CFA"
NFATOCFA: lcall ICOUNT
        lcall LIT
        .drw 0x07F
        lcall AND
        ljmp PLUS

;Z IMMED?    nfa -- f        fetch immediate flag
;   1- IC@ ;                     nonzero if immed
; Harvard model.
        .drw link
        .set link,*+1
        .db 0,6,"IMMED?"
IMMEDQ: lcall ONEMINUS
        ljmp ICFETCH

;C FIND   c-addr -- c-addr 0   if not found
;C                  xt  1      if immediate
;C                  xt -1      if "normal"
;   LATEST @ BEGIN             -- a nfa
;       2DUP OVER C@ CHAR+     -- a nfa a nfa n+1
;       N=                     -- a nfa f
;       DUP IF
;           DROP
;           NFA>LFA I@ DUP     -- a link link
;       THEN
;   0= UNTIL                   -- a nfa  OR  a 0
;   DUP IF
;       NIP DUP NFA>CFA        -- nfa xt
;       SWAP IMMED?            -- xt iflag
;       0= 1 OR                -- xt 1/-1
;   THEN ;
        .drw link
        .set link,*+1
        .db 0,4,"FIND"
FIND:   lcall LATEST
        lcall FETCH
FIND1:  lcall TWODUP
        lcall OVER
        lcall CFETCH
        inc dptr
        lcall NEQUAL
        mov a,dpl
        jz FIND2
        lcall DROP
        lcall NFATOLFA
        lcall IFETCH
        lcall DUP
FIND2:  lcall ZEROEQUAL
        lcall zerosense
        jz FIND1
        lcall DUP
        lcall zerosense
        jz FIND3
        lcall NIP
        lcall DUP
        acall NFATOCFA
        lcall SWOP
        acall IMMEDQ
        lcall ZEROEQUAL
        lcall LIT
        .drw 1
        lcall OR
FIND3:  ret

;C LITERAL  x --           append numeric literal
;   STATE @ IF ['] LIT ,XT I, THEN ; IMMEDIATE
; This tests STATE so that it can also be used
; interpretively.  (ANSI doesn't require this.)
        .drw link
        .set link,*+1
        .db IMMED,7,"LITERAL"
LITERAL: lcall STATE
        lcall FETCH
        lcall zerosense
        jz LITER1
        lcall LIT
        .drw LIT
        lcall COMMAXT
        lcall ICOMMA
LITER1: ret

;Z DIGIT?   c -- n -1   if c is a valid digit
;Z            -- x  0   otherwise
;   [ HEX ] DUP 39 > 100 AND +     silly looking
;   DUP 140 > 107 AND -   30 -     but it works!
;   DUP BASE @ U< ;
        .drw link
        .set link,*+1
        .db 0,6,"DIGIT?"
DIGITQ: lcall DUP
        lcall LIT
        .drw 0x39
        lcall GREATER
        lcall LIT
        .drw 0x100
        lcall AND
        lcall PLUS
        lcall DUP
        lcall LIT
        .drw 0x140
        lcall GREATER
        lcall LIT
        .drw 0x107
        lcall AND
        lcall MINUS
        lcall LIT
        .drw 0x30
        lcall MINUS
        lcall DUP
        lcall BASE
        lcall FETCH
        ljmp ULESS

;Z ?SIGN   adr n -- adr' n' f   get optional sign
;Z  advance adr/n if sign; return NZ if negative
;   OVER C@                 -- adr n c
;   2C - DUP ABS 1 = AND    -- +=-1, -=+1, else 0
;   DUP IF 1+               -- +=0, -=+2
;       >R 1 /STRING R>     -- adr' n' f
;   THEN ;
        .drw link
        .set link,*+1
        .db 0,5,"?SIGN"
QSIGN:  lcall OVER
        lcall CFETCH
        lcall LIT
        .drw 0x2C
        lcall MINUS
        lcall DUP
        lcall ABS
        lcall LIT
        .drw 0x1
        lcall EQUAL
        lcall AND
        lcall DUP
        lcall zerosense
        jz QSIGN1
        lcall ONEPLUS
        lcall TOR
        lcall LIT
        .drw 0x1
        lcall SLASHSTRING
        lcall RFROM
QSIGN1: ret

;C >NUMBER  ud adr u -- ud' adr' u'
;C                       convert string to number
;   BEGIN
;   DUP WHILE
;       OVER C@ DIGIT?
;       0= IF DROP EXIT THEN
;       >R 2SWAP BASE @ UD*
;       R> M+ 2SWAP
;       1 /STRING
;   REPEAT ;
        .drw link
        .set link,*+1
        .db 0,7,">NUMBER"
TONUMBER:
TONUM1: lcall DUP
        lcall zerosense
        jz TONUM3
        lcall OVER
        lcall CFETCH
        lcall DIGITQ
        lcall ZEROEQUAL
        lcall zerosense
        jz TONUM2
        ljmp DROP
TONUM2: lcall TOR
        lcall TWOSWAP
        lcall BASE
        lcall FETCH
        lcall UDSTAR
        lcall RFROM
        lcall MPLUS
        lcall TWOSWAP
        lcall LIT
        .drw 0x1
        lcall SLASHSTRING
        sjmp TONUM1
TONUM3: ret

;Z ?NUMBER  c-addr -- n -1      string->number
;Z                 -- c-addr 0  if convert error
;   DUP  0 0 ROT COUNT      -- ca ud adr n
;   ?SIGN >R  >NUMBER       -- ca ud adr' n'
;   IF   R> 2DROP 2DROP 0   -- ca 0   (error)
;   ELSE 2DROP NIP R>
;       IF NEGATE THEN  -1  -- n -1   (ok)
;   THEN ;
        .drw link
        .set link,*+1
        .db 0,7,"?NUMBER"
QNUMBER:
        lcall DUP
        lcall LIT
        .drw 0x0
        lcall DUP
        lcall ROT
        lcall COUNT
        lcall QSIGN
        lcall TOR
        lcall TONUMBER
        lcall zerosense
        jz QNUM1
        lcall RFROM
        lcall TWODROP
        lcall TWODROP
        lcall LIT
        .drw 0x0
        sjmp QNUM3
QNUM1:  lcall TWODROP
        lcall NIP
        lcall RFROM
        lcall zerosense
        jz QNUM2
        lcall NEGATE
QNUM2:  lcall LIT
        .drw -1
QNUM3:  ret

;Z INTERPRET    i*x c-addr u -- j*x
;Z                         interpret given buffer
; This is a common factor of EVALUATE and QUIT.
; ref. dpANS-6, 3.4 The Forth Text Interpreter
;   'SOURCE 2!  0 >IN !
;   BEGIN
;   BL WORD DUP C@ WHILE        -- textadr
;       FIND                    -- a 0/1/-1
;       ?DUP IF                 -- xt 1/-1
;           1+ STATE @ 0= OR    immed or interp?
;           IF EXECUTE ELSE ,XT THEN
;       ELSE                    -- textadr
;           ?NUMBER
;           IF POSTPONE LITERAL     converted ok
;           ELSE COUNT TYPE 3F EMIT CR ABORT  err
;           THEN
;       THEN
;   REPEAT DROP ;
        .drw link
        .set link,*+1
        .db 0,9,"INTERPRET"
INTERPRET: lcall TICKSOURCE
        lcall TWOSTORE
        lcall LIT
        .drw 0x0
        lcall TOIN
        lcall STORE
INTER1: lcall BL
        lcall XWORD
        lcall DUP
        lcall CFETCH
        lcall zerosense
        jz INTER9
        lcall FIND
        lcall QDUP
        lcall zerosense
        jz INTER4
        lcall ONEPLUS
        lcall STATE
        lcall FETCH
        lcall ZEROEQUAL
        lcall OR
        lcall zerosense
        jz INTER2
        lcall EXECUTE
        sjmp INTER3
INTER2: lcall COMMAXT
INTER3: sjmp INTER8
INTER4: acall QNUMBER
        lcall zerosense
        jz INTER5
        lcall LITERAL
        sjmp INTER6
INTER5: lcall COUNT
        lcall TYPE
        lcall LIT
        .drw 0x3F
        lcall EMIT
        lcall CR
        lcall ABORT
INTER6:
INTER8: sjmp INTER1
INTER9: ljmp DROP

;C EVALUATE  i*x c-addr u -- j*x  interprt string
;   'SOURCE 2@ >R >R  >IN @ >R
;   INTERPRET
;   R> >IN !  R> R> 'SOURCE 2! ;
        .drw link
        .set link,*+1
        .db 0,8,"EVALUATE"
EVALUATE: lcall TICKSOURCE
        lcall TWOFETCH
        lcall TOR
        lcall TOR
        lcall TOIN
        lcall FETCH
        lcall TOR
        acall INTERPRET
        lcall RFROM
        lcall TOIN
        lcall STORE
        lcall RFROM
        lcall RFROM
        lcall TICKSOURCE
        ljmp TWOSTORE

;C QUIT     --    R: i*x --    interpret from kbd
;   L0 LP !  R0 RP!   0 STATE !
;   BEGIN
;       TIB DUP TIBSIZE ACCEPT  SPACE
;       INTERPRET
;       STATE @ 0= IF ." OK" THEN CR
;   AGAIN ;
        .drw link
        .set link,*+1
        .db 0,4,"QUIT"
QUIT:   lcall L0
        lcall LP
        lcall STORE
        lcall RP0
        lcall RPSTORE
        lcall LIT
        .drw 0x0
        lcall STATE
        lcall STORE
QUIT1:  lcall TIB
        lcall DUP
        lcall TIBSIZE
        lcall ACCEPT
        lcall SPACE
        acall INTERPRET
        lcall STATE
        lcall FETCH
        lcall ZEROEQUAL
        lcall zerosense
        jz QUIT2
        lcall XISQUOTE
        .db 2,"ok"
        lcall ITYPE
QUIT2:  lcall CR
        sjmp QUIT1

;C ABORT    i*x --   R: j*x --   clear stk & QUIT
;   S0 SP!  QUIT ;
        .drw link
        .set link,*+1
        .db 0,5,"ABORT"
ABORT:  lcall S0
        lcall SPSTORE
        sjmp QUIT   ; QUIT never returns

;Z ?ABORT   f c-addr u --       abort & print msg
;   ROT IF ITYPE ABORT THEN 2DROP ;
        .drw link
        .set link,*+1
        .db 0,6,"?ABORT"
QABORT: lcall ROT
        lcall zerosense
        jz QABO1
        lcall ITYPE
        sjmp ABORT  ; ABORT never returns
QABO1:  ljmp TWODROP

;C ABORT"  i*x 0  -- i*x   R: j*x -- j*x  x1=0
;C         i*x x1 --       R: j*x --      x1<>0
;   POSTPONE IS" POSTPONE ?ABORT ; IMMEDIATE
        .drw link
        .set link,*+1
        .db IMMED,6,"ABORT",0x22
ABORTQUOTE: lcall ISQUOTE
        lcall LIT
        .drw QABORT
        ljmp COMMAXT

;C '    -- xt             find word in dictionary
;   BL WORD FIND
;   0= ABORT" ?" ;
        .drw link
        .set link,*+1
        .db 0,1,0x27    ; 27h = '
TICK:   lcall BL
        lcall XWORD
        lcall FIND
        lcall ZEROEQUAL
        lcall XISQUOTE
        .db 1,"?"
        sjmp QABORT

;C CHAR   -- char           parse ASCII character
;   BL WORD 1+ C@ ;
        .drw link
        .set link,*+1
        .db 0,4,"CHAR"
CHAR:   lcall BL
        lcall XWORD
        lcall ONEPLUS
        ljmp CFETCH

;C [CHAR]   --          compile character literal
;   CHAR  ['] LIT ,XT  I, ; IMMEDIATE
        .drw link
        .set link,*+1
        .db IMMED,6,"[CHAR]"
BRACCHAR: acall CHAR
        lcall LIT
        .drw LIT
        lcall COMMAXT
        ljmp ICOMMA

;C (    --                     skip input until )
;   [ HEX ] 29 WORD DROP ; IMMEDIATE
        .drw link
        .set link,*+1
        .db IMMED,1,"("
PAREN:  lcall LIT
        .drw 0x29
        lcall XWORD
        ljmp DROP

;C \    --                     skip rest of line
;   [ HEX ] 29 WORD DROP ; IMMEDIATE
        .drw link
        .set link,*+1
        .db IMMED,1,"\\"
BACKSLASH:  lcall LIT
        .drw 0
        lcall XWORD
        ljmp DROP

; COMPILER ======================================

;C CREATE   --         create an empty definition
;   LATEST @ I, 0 IC,          link & immed field
;   IHERE LATEST !             new "latest" link
;   BL IWORD IC@ 1+ IALLOT            name field
;   docreate ,CF                      code field
;   HERE I, ;             <-- Harvard model only!
; Harvard model, separate Code and Data spaces.
        .drw link
        .set link,*+1
        .db 0,6,"CREATE"
CREATE: lcall LATEST
        lcall FETCH
        lcall ICOMMA
        lcall LIT
        .drw 0x0
        lcall ICCOMMA
        lcall IHERE
        lcall LATEST
        lcall STORE
        lcall BL
        lcall IWORD
        lcall ICFETCH
        lcall ONEPLUS
        lcall IALLOT
        lcall LIT
        .drw docreate
        lcall COMMACF
        lcall HERE
        ljmp ICOMMA
        
;Z (DOES>)  --      run-time action of DOES>
;   R>              adrs of headless DOES> def'n
;   LATEST @ NFA>CFA    code field to fix up
;   !CF ;
        .drw link
        .set link,*+1
        .db 0,7,"(DOES>)"
XDOES:  lcall RFROM
        lcall LATEST
        lcall FETCH
        acall NFATOCFA
        ljmp STORECF

;C DOES>    --      change action of latest def'n
;   ['] (DOES>) ,XT
;   dodoes ,CF ; IMMEDIATE
        .drw link
        .set link,*+1
        .db IMMED,5,"DOES>"
DOES:   lcall LIT
        .drw XDOES
        lcall COMMAXT
        lcall LIT
        .drw dodoes
        ljmp COMMACF

;C RECURSE  --      recurse current definition
;   LATEST @ NFA>CFA ,XT ; IMMEDIATE
        .drw link
        .set link,*+1
        .db IMMED,7,"RECURSE"
RECURSE: lcall LATEST
        lcall FETCH
        acall NFATOCFA
        ljmp COMMAXT

;C [        --      enter interpretive state
;   0 STATE ! ; IMMEDIATE
        .drw link
        .set link,*+1
        .db IMMED,1,"["
LEFTBRACKET: lcall LIT
        .drw 0x0
        lcall STATE
        ljmp STORE

;C ]        --      enter compiling state
;   -1 STATE ! ;
        .drw link
        .set link,*+1
        .db 0,1,"]"
RIGHTBRACKET: lcall LIT
        .drw -1
        lcall STATE
        ljmp STORE

;Z HIDE     --      "hide" latest definition
;   LATEST @ DUP IC@ 80 OR SWAP IC! ;
; Harvard model.
        .drw link
        .set link,*+1
        .db 0,4,"HIDE"
HIDE:   lcall LATEST
        lcall FETCH
        lcall DUP
        lcall ICFETCH
        lcall LIT
        .drw 0x80
        lcall OR
        lcall SWOP
        ljmp ICSTORE

;Z REVEAL   --      "reveal" latest definition
;   LATEST @ DUP IC@ 7F AND SWAP IC! ;
; Harvard model.
        .drw link
        .set link,*+1
        .db 0,6,"REVEAL"
REVEAL: lcall LATEST
        lcall FETCH
        lcall DUP
        lcall ICFETCH
        lcall LIT
        .drw 0x7F
        lcall AND
        lcall SWOP
        ljmp ICSTORE

;C IMMEDIATE   --   make last def'n immediate
;   1 LATEST @ 1- IC! ;   set immediate flag
; Harvard model.
        .drw link
        .set link,*+1
        .db 0,9,"IMMEDIATE"
IMMEDIATE: lcall LIT
        .drw 0x1
        lcall LATEST
        lcall FETCH
        lcall ONEMINUS
        ljmp ICSTORE

;C :        --           begin a colon definition
;   CREATE HIDE ] !COLON ;
        .drw link
        .set link,*+1
        .db 0,1,":"
COLON:  acall CREATE
        acall HIDE
        acall RIGHTBRACKET
        ljmp STORCOLON

;C ;        --             end a colon definition
;   REVEAL  ,EXIT
;   POSTPONE [  ; IMMEDIATE
        .drw link
        .set link,*+1
        .db IMMED,1,";"
SEMICOLON: acall REVEAL
        lcall CEXIT
        ajmp LEFTBRACKET

;C [']  --         find word & compile as literal
;   '  ['] LIT ,XT  I, ; IMMEDIATE
; When encountered in a colon definition, the
; phrase  ['] xxx  will cause   LIT,xxt  to be
; compiled into the colon definition (where
; xxt is the execution token of word xxx).
; When the colon definition executes, xxt will
; be put on the stack.  (All xt's are one cell.)
        .drw link
        .set link,*+1
        .db IMMED,3,"[']"
BRACTICK: acall TICK       ; get xt of 'xxx'
        lcall LIT
        .drw LIT
        lcall COMMAXT    ; append LIT action
        ljmp ICOMMA      ; append xt literal

;C POSTPONE  --   postpone compile action of word
;   BL WORD FIND
;   DUP 0= ABORT" ?"
;   0< IF   -- xt  non immed: add code to current
;                  def'n to compile xt later.
;       ['] LIT ,XT  I,     add "LIT,xt,COMMAXT"
;       ['] ,XT ,XT         to current definition
;   ELSE  ,XT      immed: compile into cur. def'n
;   THEN ; IMMEDIATE
        .drw link
        .set link,*+1
        .db IMMED,8,"POSTPONE"
POSTPONE: lcall BL
        lcall XWORD
        lcall FIND
        lcall DUP
        lcall ZEROEQUAL
        lcall XISQUOTE
        .db 1,"?"
        lcall QABORT
        lcall ZEROLESS
        lcall zerosense
        jz POST1
        lcall LIT
        .drw LIT
        lcall COMMAXT
        lcall ICOMMA
        lcall LIT
        .drw COMMAXT
        lcall COMMAXT
        sjmp POST2
POST1:  lcall COMMAXT
POST2:  ret
               
;Z COMPILE   --   append inline execution token
;   R> DUP CELL+ >R @ ,XT ;
; The phrase ['] xxx ,XT appears so often that
; this word was created to combine the actions
; of LIT and ,XT.  It takes an inline literal
; execution token and appends it to the dict.
;    head COMPILE,7,COMPILE,docolon
;        DW RFROM,DUP,CELLPLUS,TOR
;        DW FETCH,COMMAXT,EXIT
; N.B.: not used in the current implementation

; CONTROL STRUCTURES ============================

;C IF       -- adrs    conditional forward branch
;   ['] qbranch ,BRANCH  IHERE DUP ,DEST ;
;   IMMEDIATE
        .drw link
        .set link,*+1
        .db IMMED,2,"IF"
IF:     lcall LIT
        .drw qbranch
        lcall COMMABRANCH
        lcall IHERE
        lcall DUP
        ljmp COMMADEST

;C THEN     adrs --        resolve forward branch
;   IHERE SWAP !DEST ; IMMEDIATE
        .drw link
        .set link,*+1
        .db IMMED,4,"THEN"
THEN:   lcall IHERE
        lcall SWOP
        ljmp STOREDEST

;C ELSE     adrs1 -- adrs2    branch for IF..ELSE
;   ['] branch ,BRANCH  IHERE DUP ,DEST
;   SWAP  POSTPONE THEN ; IMMEDIATE
        .drw link
        .set link,*+1
        .db IMMED,4,"ELSE"
ELSE:   lcall LIT
        .drw branch
        lcall COMMABRANCH
        lcall IHERE
        lcall DUP
        lcall COMMADEST
        lcall SWOP
        sjmp THEN

;C BEGIN    -- adrs        target for bwd. branch
;   IHERE ; IMMEDIATE
        .drw link
        .set link,*+1
        .db IMMED,5,"BEGIN"
BEGIN:  ljmp IHERE

;C UNTIL    adrs --   conditional backward branch
;   ['] qbranch ,BRANCH  ,DEST ; IMMEDIATE
;   conditional backward branch
        .drw link
        .set link,*+1
        .db IMMED,5,"UNTIL"
UNTIL:  lcall LIT
        .drw qbranch
        lcall COMMABRANCH
        ljmp COMMADEST

;X AGAIN    adrs --      uncond'l backward branch
;   ['] branch ,BRANCH  ,DEST ; IMMEDIATE
;   unconditional backward branch
        .drw link
        .set link,*+1
        .db IMMED,5,"AGAIN"
AGAIN:  lcall LIT
        .drw branch
        lcall COMMABRANCH
        ljmp COMMADEST

;C WHILE    -- adrs         branch for WHILE loop
;   POSTPONE IF SWAP ; IMMEDIATE
        .drw link
        .set link,*+1
        .db IMMED,5,"WHILE"
WHILE:  acall IF
        ljmp SWOP

;C REPEAT   adrs1 adrs2 --     resolve WHILE loop
;   POSTPONE AGAIN POSTPONE THEN ; IMMEDIATE
        .drw link
        .set link,*+1
        .db IMMED,6,"REPEAT"
REPEAT: acall AGAIN
        sjmp THEN

;Z >L   x --   L: -- x        move to leave stack
;   CELL LP +!  LP @ ! ;      (L stack grows up)
        .drw link
        .set link,*+1
        .db 0,2,">L"
TOL:    lcall CELL
        lcall LP
        lcall PLUSSTORE
        lcall LP
        lcall FETCH
        ljmp STORE

;Z L>   -- x   L: x --      move from leave stack
;   LP @ @  CELL NEGATE LP +! ;
        .drw link
        .set link,*+1
        .db 0,2,"L>"
LFROM:  lcall LP
        lcall FETCH
        lcall FETCH
        lcall CELL
        lcall NEGATE
        lcall LP
        ljmp PLUSSTORE

;C DO       -- adrs   L: -- 0
;   ['] xdo ,XT  IHERE     target for bwd branch
;   0 >L ; IMMEDIATE           marker for LEAVEs
        .drw link
        .set link,*+1
        .db IMMED,2,"DO"
DO:     lcall LIT
        .drw xdo
        lcall COMMAXT
        lcall IHERE
        lcall LIT
        .drw 0x0
        ajmp TOL

;Z ENDLOOP   adrs xt --   L: 0 a1 a2 .. aN --
;   ,BRANCH  ,DEST  ,UNLOOP       backward loop
;   BEGIN L> ?DUP WHILE POSTPONE THEN REPEAT ;
;                                 resolve LEAVEs
; This is a common factor of LOOP and +LOOP.
        .drw link
        .set link,*+1
        .db 0,7,"ENDLOOP"
ENDLOOP: lcall COMMABRANCH
        lcall COMMADEST
        lcall COMMAUNLOOP       ; new on 8051!
LOOP1:  acall LFROM
        lcall QDUP
        lcall zerosense
        jz LOOP2
        acall THEN
        sjmp LOOP1
LOOP2:  ret

;C LOOP    adrs --   L: 0 a1 a2 .. aN --
;   ['] xloop ENDLOOP ;  IMMEDIATE
        .drw link
        .set link,*+1
        .db IMMED,4,"LOOP"
LOOP:   lcall LIT
        .drw xloop
        sjmp ENDLOOP

;C +LOOP   adrs --   L: 0 a1 a2 .. aN --
;   ['] xplusloop ENDLOOP ;  IMMEDIATE
        .drw link
        .set link,*+1
        .db IMMED,5,"+LOOP"
PLUSLOOP: lcall LIT
        .drw xplusloop
        sjmp ENDLOOP

;C LEAVE    --    L: -- adrs
;   ['] UNLOOP ,XT
;   ['] branch ,BRANCH   IHERE DUP ,DEST  >L
;   ; IMMEDIATE      unconditional forward branch
        .drw link
        .set link,*+1
        .db IMMED,5,"LEAVE"
LEAVE:  lcall LIT
        .drw unloop
        lcall COMMAXT
        lcall LIT
        .drw branch
        lcall COMMABRANCH
        lcall IHERE
        lcall DUP
        lcall COMMADEST
        ajmp TOL

; OTHER OPERATIONS ==============================

;X WITHIN   n1|u1 n2|u2 n3|u3 -- f   n2<=n1<n3?
;  OVER - >R - R> U< ;          per ANS document
        .drw link
        .set link,*+1
        .db 0,6,"WITHIN"
WITHIN: lcall OVER
        lcall MINUS
        lcall TOR
        lcall MINUS
        lcall RFROM
        ljmp ULESS

;C MOVE    addr1 addr2 u --     smart move
;             VERSION FOR 1 ADDRESS UNIT = 1 CHAR
;  >R 2DUP SWAP DUP R@ +     -- ... dst src src+n
;  WITHIN IF  R> CMOVE>        src <= dst < src+n
;       ELSE  R> CMOVE  THEN ;          otherwise
        .drw link
        .set link,*+1
        .db 0,4,"MOVE"
MOVE:   lcall TOR
        lcall TWODUP
        lcall SWOP
        lcall DUP
        lcall RFETCH
        lcall PLUS
        lcall WITHIN
        lcall zerosense
        jz MOVE1
        lcall RFROM
        lcall CMOVEUP
        sjmp MOVE2
MOVE1:  lcall RFROM
        lcall CMOVE
MOVE2:  ret

;C DEPTH    -- +n        number of items on stack
;   SP@ S0 SWAP - 2/ ;   16-BIT VERSION!
        .drw link
        .set link,*+1
        .db 0,5,"DEPTH"
DEPTH:  lcall SPFETCH
        lcall S0
        lcall SWOP
        lcall MINUS
        ljmp TWOSLASH

;C ENVIRONMENT?  c-addr u -- false   system query
;                         -- i*x true
;   2DROP 0 ;       the minimal definition!
        .drw link
        .set link,*+1
        .db 0,12,"ENVIRONMENT?"
ENVIRONMENTQ: lcall TWODROP
        lcall LIT
        .drw 0x0
        ret

; UTILITY WORDS AND STARTUP =====================

;X WORDS    --          list all words in dict.
;   LATEST @ BEGIN
;       DUP ICOUNT 7F AND ITYPE SPACE
;       NFA>LFA I@
;   DUP 0= UNTIL
;   DROP ;
        .drw link
        .set link,*+1
        .db 0,5,"WORDS"
WORDS:  lcall LATEST
        lcall FETCH
WDS1:   lcall DUP
        lcall ICOUNT
        lcall LIT
        .drw 0x7f
        lcall AND
        lcall ITYPE
        lcall SPACE
        lcall NFATOLFA
        lcall IFETCH
        lcall DUP
        lcall ZEROEQUAL
        lcall zerosense
        jz WDS1
        ljmp DROP

;X .S      --           print stack contents
;   SP@ S0 - IF
;       SP@ S0 2 - DO I @ U. -2 +LOOP
;   THEN ;
        .drw link
        .set link,*+1
        .db 0,2,".S"
DOTS:   lcall SPFETCH
        lcall S0
        lcall MINUS
        lcall zerosense
        jz DOTS2
        lcall SPFETCH
        lcall S0
        lcall LIT
        .drw 0x2
        lcall MINUS
        lcall XDO
DOTS1:  lcall II
        lcall FETCH
        lcall UDOT
        lcall LIT
        .drw -2
        lcall pluslpsense
        jz DOTS1
        lcall UNLOOP
DOTS2:  ret

;Z COLD     --      cold start Forth system
;   UINIT U0 #INIT I->D      init user area
;   ." 8051 CamelForth etc."
;   ABORT ;
        .drw link
        .set link,*+1
        .db 0,4,"COLD"
COLD:   lcall UINIT
        lcall U0
        lcall NINIT
        lcall ITOD
        lcall XISQUOTE
       .DB 35,"8051 CamelForth v1.6  18 Aug 1999"
       .DB 0x0d,0x0a
        lcall ITYPE
        ljmp ABORT       ; ABORT never returns

; ===============================================
; CamelForth for the Intel 8051
; Primitive testing code
;
; This is the "minimal" test of the CamelForth
; kernel.  It verifies the threading and nesting
; mechanisms, the stacks, and the primitives
;   DUP EMIT EXIT lit branch ONEPLUS.
; It is particularly useful because it does not
; use the DO..LOOP, multiply, or divide words,
; and because it can be used on embedded CPUs.
; The numeric display word .A is also useful
; for testing the rest of the Core wordset.
;
; Much of this code has been retained because it
; supports DUMP.  It can be deleted without
; affecting the CamelForth kernel.  Be careful
; not to delete the equates at the end of file.
; ===============================================
;
;       Extra primitives for the testing code.
;
SWAB:   mov a,dph
        mov dph,dpl
        mov dpl,a
        ret

LO:     anl dpl,#0x0f
        mov dph,#0
        ret

HI:     anl dpl,#0xf0
        mov a,dpl
        rr a
        rr a
        rr a
        rr a
        mov dpl,a
        mov dph,#0
        ret

DOTHEX: mov a,dpl
        clr c
        subb a,#0x0a
        jc numeric
        add a,#7
numeric: add a,#0x3a
        mov dpl,a
        lcall EMIT
        ret

DOTHH:  lcall DUP
        lcall HI
        lcall DOTHEX
        lcall LO
        lcall DOTHEX
        ret
;
;       : .B ( a - a+1)  DUP C@ .HH 20 EMIT 1+ ;
;
DOTB:   lcall DUP
        lcall CFETCH
        lcall DOTHH
        lcall lit
        .drw 0x20
        lcall EMIT
        lcall ONEPLUS
        ret
;
;       : .A ( a)  DUP SWAB .HH .HH 20 EMIT ;
;
DOTA:   lcall DUP
        lcall SWAB
        lcall DOTHH
        lcall DOTHH
        lcall lit
        .drw 0x20
        lcall EMIT
        ret

;
;       : DUMP  ( a n -- )
;               0 DO
;                  CR DUP .A SPACE
;                  .B .B .B .B  .B .B .B .B
;                  .B .B .B .B  .B .B .B .B
;               16 +LOOP DROP ;
;
        .drw link
        .set link,*+1
        .db 0,4,"DUMP"
DUMP:   lcall LIT
        .drw 0
        lcall XDO
dump1:  lcall CR
        lcall DUP
        lcall DOTA
        lcall SPACE
        lcall DOTB
        lcall DOTB
        lcall DOTB
        lcall DOTB
        lcall DOTB
        lcall DOTB
        lcall DOTB
        lcall DOTB
        lcall DOTB
        lcall DOTB
        lcall DOTB
        lcall DOTB
        lcall DOTB
        lcall DOTB
        lcall DOTB
        lcall DOTB
        lcall LIT
        .drw 16
        lcall xplusloop
        jz dump1
        lcall UNLOOP
        lcall DROP
        ret

;
;       : XQUIT
;               0 BEGIN  0D EMIT 0A EMIT  DUP .A
;                       .B .B .B .B .B .B .B .B
;                       .B .B .B .B .B .B .B .B
;               AGAIN ;
;
; XQUIT:  lcall LIT
;         .drw 0
; xquit1: lcall LIT
;         .drw 0x0D
;         lcall EMIT
;         lcall LIT
;         .drw 0x0A
;         lcall EMIT
;         lcall DUP
;         lcall DOTA
;         lcall DOTB
;         lcall DOTB
;         lcall DOTB
;         lcall DOTB
;         lcall DOTB
;         lcall DOTB
;         lcall DOTB
;         lcall DOTB
;         lcall DOTB
;         lcall DOTB
;         lcall DOTB
;         lcall DOTB
;         lcall DOTB
;         lcall DOTB
;         lcall DOTB
;         lcall DOTB
;         ajmp xquit1

; EXAMPLE I/O ===================================
; !P1     c --      output byte to port 1
        .drw link
        .set link,*+1
        .db  0,3,"!P1"
STOREP1: mov p1,dpl     ; output TOS char to P1
        ljmp poptos     ; pop new TOS

; @P1      -- c      get byte from port 1
        .drw link
        .set link,*+1
        .db  0,3,"@P1"
FETCHP1: lcall pushtos  ; push old TOS
        mov dpl,p1      ; get P1 byte in TOS
        mov dph,#0
        ret

; !TCON     c --      output byte to TCON reg
        .drw link
        .set link,*+1
        .db  0,5,"!TCON"
STORETCON: mov tcon,dpl ; output TOS char to TCON
        ljmp poptos     ; pop new TOS

; @TCON      -- c      get byte from TCON reg
        .drw link
        .set link,*+1
        .db  0,5,"@TCON"
FETCHTCON: lcall pushtos ; push old TOS
        mov dpl,tcon    ; get TCON byte in TOS
        mov dph,#0
        ret

; Other I/O ports are left as an exercise for the
; student.

; ===============================================
; CamelForth Multitasker for the Intel 8051
; (c) 1996 Bradford J. Rodriguez
; Permission is granted to freely copy, modify,
; and distribute this program for personal or
; educational use.  Commercial inquiries should
; be directed to the author at 115 First St.,
; #105, Collingwood, Ontario L9Y 4W3 Canada
; ===============================================

        .equ dr1,0x01   ; r1 as direct register

; The key word of the multitasker is SWITCH.
; It saves the working registers AND the return
; stack of the currently executing task to a
; storage area in external RAM.  Then it gets
; the saved registers and return stack of the
; new task, restores them, and continues
; execution wherever the new task left off.
;
; Registers as they are saved:
; 01 (R1): saved Parameter Stack pointer.
; 02 (R2): future use
; 03 (R3):     "
; 04 (R4):     "
; 05 (R5):     "
; 06 (R6): loop index
; 07 (R7):     "
; 08:      P2, User Area pointer high
; 09...N:  return stack (N is given by SP)
;
; DPTR is not saved, since it is consumed by
; SWITCH.  (It is the address of the new task's
; save area, UAREA-100h.)
;
; Note that these are stored backwards in
; external RAM, starting at address UAREA-100h.
; Thus the save area of a newly created task
; should look like:
; SP:     0Ah
; 0A,09:  init'l Program Counter, hi byte first
; 08:     task's User Pointer high (stack page)
; 07,06:  xxx
; 05,04:  xxx
; 03,02:  xxx
; 01:     0FDh, initial stack pointer
; The initial stack pointer must be FDh because
; of the poptos at the end of SWITCH.

; SWITCH     a --      switch to new task
        .drw link
        .set link,*+1
        .db  0,6,"SWITCH"
SWITCH: mov r2,dph      ; stash new task adrs
        mov r3,dpl
        mov dph,UP      ; save me at UAREA-100h
        dec dph
        mov dpl,#0x0
        mov dr1,r0      ; save my Pstack pointer
; This loop copies internal RAM, from location
; (SP) down to 01, to external RAM.  6+7n cycles.
; The length is saved as the first byte.
        mov a,sp        ; sp=high address,
        movx @dptr,a    ;   =length.
        inc dptr
        mov r0,a        ; 00 won't be moved
saveregs: mov a,@r0      ; 1 cycle
        movx @dptr,a     ; 2 cycles
        inc dptr         ; 2 cycles
        djnz r0,saveregs ; 2 cycles

        mov dph,r2      ; now get new task
        mov dpl,r3
; This loop copies external RAM to internal RAM,
; and restores SP accordingly.  6+7n cycles.
        movx a,@dptr    ; get high address
        inc dptr
        mov sp,a        ; restore Rstack pointer
        mov r0,a
getregs: movx a,@dptr
        inc dptr
        mov @r0,a
        djnz r0,getregs

; The top of this restored return stack contains
; a return address in the new task.  DPTR no
; longer contains its top-of-stack; so pop the
; new top of stack from RAM.
        mov r0,dr1      ; restore Pstack pointer
        mov p2,UP       ; set new stack page
        ljmp poptos     ; pop TOS and return

; -----------------------------------------------
; INITTASK   xt a --     initialize a task area
; Given the xt (code address) of a Forth word to
; execute, and the address of a task's save area,
; fill in that save area so the given word will
; execute when that task is started.
        .drw link
        .set link,*+1
        .db  0,8,"INITTASK"
INITTASK: mov a,#0x0a   ; length
        movx @dptr,a
        inc dptr
        movx a,@r0      ; low byte of xt
        inc r0
        mov r2,a
        movx a,@r0      ; high byte of xt
        inc r0
        movx @dptr,a    ; store high byte first
        inc dptr
        mov a,r2
        movx @dptr,a
        inc dptr
        mov a,dph       ; UAREA=SaveArea+100h, so
        inc a           ; DPH+1 = UAREA high byte
        movx @dptr,a
        inc dptr
        inc dptr        ; skip 6 don't-cares
        inc dptr
        inc dptr
        inc dptr
        inc dptr
        inc dptr
        mov a,#0xfd     ; initial Pstack pointer
        movx @dptr,a
        ljmp poptos

; -----------------------------------------------
; PREEMPT		      force a task switch
; If entered from an interrupt, this will cause
; a switch to the next task in the round robin.
; The task link must be in the first cell of the
; user area (user variable U0).  Note that this
; is an assembler subroutine and must not be
; called as a Forth word.
PREEMPT: push psw     ; save regs used by SWITCH
        push acc
        push b
        push dr1
        push dr2
        push dr3
        lcall pushtos ; DPTR saved on Data stack!
        mov dph,UP    ; fetch task link...
        mov dpl,#0
        movx a,@dptr
        mov r2,a
        inc dptr
        movx a,@dptr
        mov dpl,r2
        mov dph,a     ; ...to DPTR
        acall SWITCH  ; switch to next task
; Execution will resume here when the round-robin
; comes back to this task.  Note that the last
; action of SWITCH is to restore DPH:DPL from the
; Data stack, with "poptos".
        pop dr3       ; restore regs
        pop dr2
        pop dr1
        pop b
        pop acc
        pop psw
        reti

; -----------------------------------------------
; Sample timer 0 interrupt, entered when timer 0
; rolls over from FFFF to 0000.  The interrupt
; flag is automatically cleared when the
; interrupt service routine is entered.
CLOCK:	sjmp PREEMPT

; CLOCKON starts timer 0 & enables the interrupt
        .drw link
        .set link,*+1
        .db  0,7,"CLOCKON"
CLOCKON: mov tmod,#0x21  ; T1 mode 2, T0 mode 1
        mov th0,#0x0
        mov tl0,#0x0
        setb tcon.4      ; enable timer 0
	mov ie,#0x82     ; enable timer 0 irpt
        ret

; CLOCKOFF stops timer 0 & disables the interrupt
        .drw link
        .set link,*+1
        .db  0,8,"CLOCKOFF"
CLOCKOFF: clr tcon.4    ; disable timer 0
	clr ie.1        ; enable timer 0 irpt
        ret


; ===============================================
; Initial dictionary pointer for CamelForth.
; DO NOT delete!
    .equ lastword,link      ; NFA of final word

