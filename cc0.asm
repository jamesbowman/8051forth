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
; FORTH EQUATES
        .equ    UP      ,0x08   ; user pointer
        .equ    _DP     ,0x0a   ; data pointer
        .equ    _IDP    ,0x0c   ; code data pointer
        .equ    _LATEST ,0x0e   ; last word defined
        .equ    _HANDLER,0x10   ; last exception handler
        .equ    _BASE   ,0x12   ; BASE
        .equ    _MS     ,0x14   ; millisecond timer (32-bit)
        .equ    _STATE  ,0x18   ; STATE
        .equ    _TTH    ,0x1a   ; TTH (tethered)
        .equ    _TOIN   ,0x1c   ; >IN
        .equ    _SOURCE ,0x1e   ; TICKSOURCE
        .equ    _HP     ,0x22   ; HP
        .equ    _LP     ,0x24   ; LP
        .equ    _THISXT ,0x26   ; THISXT
        .equ    _SRCID,  0x28   ; 'SOURCE-ID
        .equ    _FAULT,  0x2a   ; 'FAULT

        .equ    WORDBUF ,0x30   ; scratch for FIND
        .equ    RSP0    ,0x50   ; start of R stack
        .equ    S0      ,0xff   ; start of D stack

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
    .equ dataram,0x0f000    ; where data goes
    .equ UPHI,0xFD          ; Uarea at FE00 hex

        .org 0x0000
; RESET AND INTERRUPT VECTORS ===================
        ljmp reset

        .org 0x0003             ; 0     RFTXRX
        ljmp nullintr

        .org 0x000b             ; 1     ADC
        ljmp nullintr

        .org 0x0013             ; 2     USART0
        ljmp nullintr

        .org 0x001b             ; 3     USART1
        ljmp nullintr

        .org 0x0023             ; 4     ENC
        ljmp nullintr

        .org 0x002b             ; 5     ST
        ljmp int_sleep

        .org 0x0033             ; 6     P2INT
        ljmp nullintr

        .org 0x003b             ; 7     UTX0
        ljmp nullintr

        .org 0x0043             ; 8     DMA
        ljmp nullintr

        .org 0x004b             ; 9     T1
        ljmp khz1

        .org 0x0053             ; 10    T2
        ljmp nullintr

        .org 0x005b             ; 11    T3
        ljmp nullintr

        .org 0x0063             ; 12    T4
        ljmp nullintr

        .org 0x006b             ; 13    P0INT
        ljmp nullintr

        .org 0x0073             ; 14    UTX1
        ljmp nullintr

        .org 0x007b             ; 15    P1INT
        ljmp nullintr

        .org 0x0083             ; 16    RF
        ljmp nullintr

        .org 0x008b             ; 17    WDT
        ljmp nullintr

nullintr:
        reti

        .equ IRCON,0xc0
        .equ T1CTL,0xe4

int_sleep:
        clr IRCON.7             ; STIF
        reti

khz1:
        push acc

        inc _MS+2
        mov a,_MS+2
        jnz donekhz

        inc _MS+3
        mov a,_MS+3
        jnz donekhz

        inc _MS+0
        mov a,_MS+0
        jnz donekhz

        inc _MS+1

donekhz:
        mov a,T1CTL             ; Clear T1CTL.OVFIF
        anl a,#0xef
        mov T1CTL,a
        ; mov IRCON,#2
        clr IRCON.1
        pop acc
        reti

        
reset:  mov ie,#0x00       ; disable all irpts

        .equ DPS,0x92
        .equ FWT,0xab
        .equ FADDRL,0xac
        .equ FADDRH,0xad
        .equ FLC,0xae
        .equ FWDATA,0xaf

        .equ W0RTIME0,0xa5

        .equ U0CSR,0x86
        .equ U0UCR,0xc4
        .equ U0GCR,0xc5
        .equ U0DBUF,0xc1
        .equ U0BAUD,0xc2

        .equ SLEEP, 0xbe
        .equ IRCON2,0xe8
        .equ P0DIR,0xfd
        .equ P0SEL,0xf3

        .equ CLKCON,0xc6

        ;             76543210
        mov CLKCON,#0b10000000                  ; external crystal
        mov FWT,#0x2a                           ; flash write timer
        mov P0SEL, #0b00001100                  ; UART0
        mov U0CSR, #0b11000000                  ; 8N1
        mov U0UCR, #0b10000010
        mov U0GCR, #12                          ; 115200
        mov U0BAUD,#34

        mov UP,#UPHI
        mov r0,#S0      ; param stack at FEFF
        mov sp,#RSP0
        ljmp COLD       ; enter Forth interpreter

; SERIAL I/O ====================================

; See TI DN112: Using UART in CC111x

;C EMIT     c --      output character to console
        .drw link
        .set link,*+1
        .db  0,4,"EMIT"
EMIT:
        clr IRCON2.1
        mov U0DBUF,dpl  ; output TOS char to UART
w:      jnb IRCON2.1,w
        clr IRCON2.1
        ajmp poptos     ; pop new TOS

;C KEY      -- c      get character from keyboard
        .drw link
        .set link,*+1
        .db  0,3,"KEY"
KEY:
        mov a,U0CSR
        jnb acc.2,KEY
        acall DUP
        mov dpl,U0DBUF    ; get new char in TOS
        mov dph,#0
        ret

;X KEY?      -- f      return true if char waiting
        .drw link
        .set link,*+1
        .db  0,4,"KEY?"
QUERYKEY:
        acall DUP
        mov a,scon      ; get rx flag in carry
        rrc a
        ajmp cyprop     ; propagate that thru TOS

; EXCEPTIONS ====================================

        .drw link
        .set link,*+1
        .db  0,5,"CATCH"
CATCH:
        lcall SPFETCH   ; SP@ >R         ( xt ) \ save data stack pointer
        lcall TOR
        lcall HANDLER   ; HANDLER @ >R   ( xt ) \ and previous handler
        lcall FETCH
        lcall TOR
        lcall RPFETCH   ; RP@ HANDLER !  ( xt ) \ set current handler
        lcall HANDLER
        lcall STORE
    
        lcall EXECUTE   ; EXECUTE        ( )    \ execute returns if no THROW
        lcall RFROM     ; R> HANDLER !   ( )    \ restore previous handler
        lcall HANDLER
        lcall STORE
        lcall RFROM     ; R> DROP        ( )    \ discard saved stack ptr
        lcall DROP
        ljmp FALSE      ; 0              ( 0 )  \ normal completion

        .drw link
        .set link,*+1
        .db  0,5,"THROW"
THROW:
        mov a,dpl
        orl a,dph
        jnz THROW1
        ljmp DROP
THROW1:
        lcall HANDLER   ; HANDLER @ RP!  ( exc# ) \ restore prev return stack
        lcall FETCH
        lcall RPSTORE
        lcall RFROM     ; R> HANDLER !   ( exc# ) \ restore prev handler
        lcall HANDLER
        lcall STORE
        lcall RFROM     ; R> SWAP >R     ( saved-sp ) \ exc# on return stack
        lcall SWOP
        lcall TOR
        lcall SPSTORE   ; SP! DROP R>    ( exc# ) \ restore stack
        lcall DROP
        lcall RFROM
        ret

; LOOP FACTORS ==================================

bounds: acall OVER
        acall PLUS
        ajmp SWOP

break:  mov a,@r0
        cjne a,dpl,exit1
        inc r0
        mov a,@r0
        dec r0
        cjne a,dph,exit1
        acall TWODROP
        sjmp EXIT

; INTERPRETER LOGIC =============================

; NEXT and ENTER are not needed for Subroutine
; Threading. EXIT may be used in high level code.

;C EXIT     --            exit a colon definition
        .drw link
        .set link,*+1
        .db  0,4,"EXIT"
EXIT:   dec sp      ; discard ret adrs in caller
        dec sp
exit1:  ret         ; return to caller's caller

;Z LIT      -- x    fetch inline literal to stack
        .drw link
        .set link,*+1
        .db  0,3,"LIT"
LIT:    dec r0          ; push old TOS
        mov @r0,dph
        dec r0
        mov @r0,dpl
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

XISQUOTE: acall RFROM
        acall COUNT
        lcall TWODUP
        acall PLUS
        ajmp EXECUTE

; DEFINING WORDS ================================

;Z CELL     -- n                 size of one cell
        .drw link
        .set link,*+1
        .db  0,4,"CELL"
CELL:   acall DOCON
        .drw 2

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
        mov @r0,dph
        dec r0
        mov @r0,dpl
        pop dph         ; get addr of param field
        pop dpl         ;     (in Code memory!)
        ajmp FETCH      ; go fetch its contents

; DOUSER, code action of USER,
; entered by CALL DOUSER
douser: acall pushtos   ; push old TOS
        pop dph         ; get addr of param field
        pop dpl         ;     (in Code memory!)
        acall FETCH     ; go fetch its contents
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
        mov @r0,dph
        dec r0
        mov @r0,dpl
        pop dr5         ; addr of DOES> clause
        pop dr4         ;   Forth code
        pop dph         ; addr of defined word's
        pop dpl         ;   Param. field
        push dr4        ; restore Forth code addr
        push dr5
        ajmp FETCH      ; fetch adrs from P.field
                        ;  & go do the DOES> code

; STACK OPERATIONS ==============================

;C DUP      x -- x x       duplicate top of stack
        .drw link
        .set link,*+1
        .db  0,3,"DUP"
DUP:
pushtos: dec r0         ; push hi byte of TOS
        mov @r0,dph
        dec r0          ; push lo byte of TOS
        mov @r0,dpl
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
poptos: mov dpl,@r0     ; pop lo byte -> TOS
        inc r0
        mov dph,@r0      ; pop hi byte -> TOS
        inc r0
        ret

;C SWAP     x1 x2 -- x2 x1     swap top two items
        .drw link
        .set link,*+1
        .db  0,4,"SWAP"
SWOP:   mov a,@r0
        xch a,dpl
        mov @r0,a
        inc r0
        mov a,@r0
        xch a,dph
        mov @r0,a
        dec r0
        ret

;C OVER     x1 x2 -- x1 x2 x1   per stack diagram
        .drw link
        .set link,*+1
        .db  0,4,"OVER"
OVER:
        mov dr3,@r0     ; a:r3 is x1
        inc r0
        mov a,@r0
        dec r0

        dec r0          ; push hi byte of TOS
        mov @r0,dph
        dec r0          ; push lo byte of TOS
        mov @r0,dpl

        mov dph,a
        mov dpl,r3
        ret

;C ROT     x1 x2 x3 -- x2 x3 x1 per stack diagram
        .drw link
        .set link,*+1
        .db  0,3,"ROT"
ROT:    ; x3 is in TOS
        mov dr4,@r0     ; pop x2 -> r5:r4
        inc r0
        mov dr5,@r0
        inc r0
        mov dr2,@r0     ; pop x1 -> a:r2
        inc r0
        mov a,@r0

        mov @r0,dr5     ; push x2
        dec r0
        mov @r0,dr4
        dec r0
        mov @r0,dph     ; push x3
        dec r0
        mov @r0,dpl
        mov dph,a       ; TOS now x1
        mov dpl,r2
        ret

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
        mov @r0,dph
        dec r0
        mov @r0,dpl
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
        mov @r0,dph
        dec r0
        mov @r0,dpl
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
SPFETCH: lcall DUP
        mov dph,0xff     ; 16-bit pointer P2:R0
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
RPFETCH: lcall DUP
        mov dph,#0       ; 16-bit pointer 00:SP
        mov acc,sp
        add a,#-2
        mov dpl,a
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
STORE:  mov a,@r0       ; low byte of X
        inc r0
        movx @dptr,a
        inc dptr
        mov a,@r0       ; high byte of X
        inc r0
        movx @dptr,a
        ajmp poptos     ; pop new TOS

;C C!       c c-addr --    store char in Data mem
        .drw link
        .set link,*+1
        .db  0,2,"C!"
CSTORE: mov a,@r0       ; low byte is char
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

; ARITHMETIC AND LOGICAL OPERATIONS =============

;C FALSE  -- false
        .drw link
        .set link,*+1
        .db  0,5,"FALSE"
FALSE:  acall DUP
        mov dptr,#0
        ret

;C TRUE  -- true
        .drw link
        .set link,*+1
        .db  0,4,"TRUE"
TRUE :  acall DUP
        mov dptr,#-1
        ret

;C +        n1/u1 n2/u2 -- n3/u3        add n1+n2
        .drw link
        .set link,*+1
        .db  0,1,"+"
PLUS:   mov a,dpl       ; low byte
        add a,@r0
        inc r0
        mov dpl,a
        mov a,dph       ; high byte
        addc a,@r0
        inc r0
        mov dph,a
        ret

;Z M+       d n -- d         add single to double
        .drw link
        .set link,*+1
        .db  0,2,"M+"
MPLUS:  lcall STOD
        ljmp DPLUS

;C -        n1/u1 n2/u2 -- n3/u3   subtract n1-n2
        .drw link
        .set link,*+1
        .db  0,1,"-"
MINUS:  mov a,@r0       ; low byte
        inc r0
        clr c
        subb a,dpl
        mov dpl,a
        mov a,@r0       ; high byte
        inc r0
        subb a,dph
        mov dph,a
        ret

;C AND      x1 x2 -- x3               logical AND
        .drw link
        .set link,*+1
        .db  0,3,"AND"
AND:    mov a,@r0       ; low byte
        inc r0
        anl a,dpl
        mov dpl,a
        mov a,@r0       ; high byte
        inc r0
        anl a,dph
        mov dph,a
        ret

;C OR       x1 x2 -- x3                logical OR
        .drw link
        .set link,*+1
        .db  0,2,"OR"
OR:     mov a,@r0       ; low byte
        inc r0
        orl a,dpl
        mov dpl,a
        mov a,@r0       ; high byte
        inc r0
        orl a,dph
        mov dph,a
        ret

;C XOR      x1 x2 -- x3               logical XOR
        .drw link
        .set link,*+1
        .db  0,3,"XOR"
XOR:    mov a,@r0       ; low byte
        inc r0
        xrl a,dpl
        mov dpl,a
        mov a,@r0       ; high byte
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
        acall DROP
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
        acall DROP
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
PLUSSTORE:
        movx a,@dptr    ; low byte of memory
        add a,@r0
        inc r0
        movx @dptr,a
        inc dptr
        movx a,@dptr    ; high byte of memory
        addc a,@r0
        inc r0
        movx @dptr,a
        ajmp poptos     ; pop new TOS

; COMPARISON OPERATIONS =========================

;X <>     x1 x2 -- flag            test not equal
        .drw link
        .set link,*+1
        .db  0,2,"<>"
NOTEQUAL: mov a,@r0
        inc r0
        cjne a,dpl,NOTEQUAL1
        mov a,@r0
        inc r0
        cjne a,dph,NOTEQUAL2
        mov dptr,#0
        ret
NOTEQUAL1: inc r0
NOTEQUAL2: mov dptr,#-1
        ret

;C 0=       n/u -- flag      return true if TOS=0
        .drw link
        .set link,*+1
        .db  0,2,"0="
ZEROEQUAL: mov a,dph
zequ1:  orl a,dpl       ; A = z or nz, per DPTR
        jnz is1
        mov dptr,#-1
        ret
is1:
        mov dptr,#0
        ret

;C 0<>      n -- flag        true if TOS nonzero
        .drw link
        .set link,*+1
        .db  0,3,"0<>"
ZERONOTEQUAL:
        lcall FALSE
        ljmp NOTEQUAL

;C 0<       n -- flag        true if TOS negative
        .drw link
        .set link,*+1
        .db  0,2,"0<"
ZEROLESS: mov a,dph
        rlc a           ; cy set if A negative
                        ; propagate cy thru TOS
cyprop: subb a,acc      ; -1 if A was 0, else 0
        mov dph,a
        mov dpl,a
        ret             ; NB! A=0 iff TOS=0

;C 0>       n -- flag        true if TOS > 0
        .drw link
        .set link,*+1
        .db  0,2,"0>"
ZEROGREATER:
        lcall FALSE
        ljmp GREATER

;C =        x1 x2 -- flag              test x1=x2
        .drw link
        .set link,*+1
        .db  0,1,"="
EQUAL:  mov a,@r0
        inc r0
        cjne a,dpl,EQUAL1
        mov a,@r0
        inc r0
        cjne a,dph,EQUAL2
        mov dptr,#-1
        ret
EQUAL1: inc r0
EQUAL2: mov dptr,#0
        ret

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
ULESS:  mov a,@r0       ; low byte
        inc r0
        clr c
        subb a,dpl
        mov a,@r0       ; high byte
        inc r0
        subb a,dph
        sjmp cyprop     ; propagate cy thru TOS

;X U>    u1 u2 -- flag       test u1>u2, unsigned
        .drw link
        .set link,*+1
        .db  0,2,"U>"
; UGREATER: acall SWOP
;           sjmp ULESS
UGREATER: mov a,dpl      ; low byte
        clr c
        subb a,@r0
        inc r0
        mov a,dph       ; high byte
        subb a,@r0
        inc r0
        sjmp cyprop     ; propagate cy thru TOS

; DOUBLE OPERATORS ==============================

;C 2@    a-addr -- x1 x2            fetch 2 cells
;   DUP CELL+ @ SWAP @ ;
;   the lower address will appear on top of stack
        .drw link
        .set link,*+1
        .db  0,2,"2@"
TWOFETCH: acall DUP
        inc dptr
        inc dptr
        acall FETCH
        acall SWOP
        ajmp FETCH

;C 2!    x1 x2 a-addr --            store 2 cells
;   SWAP OVER ! CELL+ ! ;
;   the top of stack is stored at the lower adrs
        .drw link
        .set link,*+1
        .db  0,2,"2!"
TWOSTORE: acall SWOP
        acall OVER
        acall STORE
        inc dptr
        inc dptr
        ajmp STORE

;C 2DROP  x1 x2 --                   drop 2 cells
;   DROP DROP ;
        .drw link
        .set link,*+1
        .db  0,5,"2DROP"
TWODROP: inc r0
        inc r0
        ajmp DROP

;C 2DUP   x1 x2 -- x1 x2 x1 x2    dup top 2 cells
;   OVER OVER ;
        .drw link
        .set link,*+1
        .db  0,4,"2DUP"
TWODUP: acall OVER
        ajmp OVER

;C 2SWAP  x1 x2 x3 x4 -- x3 x4 x1 x2  per diagram
;   ROT >R ROT R> ;
        .drw link
        .set link,*+1
        .db  0,5,"2SWAP"
TWOSWAP: acall ROT
        acall TOR
        acall ROT
        acall RFROM     ; can't ljmp RFROM!
        ret

;C 2OVER  x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2
;   >R >R 2DUP R> R> 2SWAP ;
        .drw link
        .set link,*+1
        .db  0,5,"2OVER"
TWOOVER: acall TOR
        acall TOR
        lcall TWODUP
        acall RFROM
        acall RFROM
        ajmp TWOSWAP

;C 2>R       x y --   R: -- x y   push to return stack
        .drw link
        .set link,*+1
        .db  0,3,"2>R"
TWOTOR: lcall SWOP
        pop dr3         ; save ret addr in r3:r2
        pop dr2
        push dpl
        push dph
        lcall DROP
        push dpl
        push dph
        push dr2        ; restore ret addr
        push dr3
        ljmp poptos     ; pop new TOS

;C 2R>      -- x   R: x --  pop from return stack
        .drw link
        .set link,*+1
        .db  0,3,"2R>"
TWORFROM: pop dr3         ; save ret addr in r3:r2
        pop dr2
        lcall DUP
        pop dph         ; pop hi byte
        pop dpl         ; pop lo byte
        lcall DUP
        pop dph         ; pop hi byte
        pop dpl         ; pop lo byte
        lcall SWOP
        push dr2        ; restore return address
        push dr3
        ret

;C 2R@      -- x   R: x -- x
        .drw link
        .set link,*+1
        .db  0,3,"2R@"
TWORFETCH:
        lcall DUP
        mov a,sp
        add a,#-5
        mov dpl,a
        mov dph,#0xff
        lcall TWOFETCH
        ljmp SWOP

;Z D+       d d -- d         add double
        .drw link
        .set link,*+1
        .db  0,2,"D+"
DPLUS:  mov a,@r0       ; pop d.low -> r3:a
        inc r0
        mov dr3,@r0
        inc r0

        mov dr4,@r0     ; pop d.high-> r5:r4
        inc r0
        mov dr5,@r0
        inc r0

        add a,@r0
        mov @r0,a
        inc r0
        mov a,@r0       ; d.low, high byte
        addc a,r3
        mov @r0,a
        dec r0

        mov a,dpl
        addc a,r4
        mov dpl,a
        mov a,dph
        addc a,r5
        mov dph,a
        ret

; INPUT/OUTPUT ==================================

;C BL      -- char                 an ASCII space
        .drw link
        .set link,*+1
        .db  0,2,"BL"
BL:     acall docon
        .drw 0x20

;C COUNT   c-addr1 -- c-addr2 u  counted->adr/len
;   DUP CHAR+ SWAP C@ ;
        .drw link
        .set link,*+1
        .db  0,5,"COUNT"
COUNT:  movx a,@dptr
        inc dptr
PUSHA:  acall DUP
        mov dpl,a
        mov dph,#0
        ret

;C CR      --                      output newline
;   0D EMIT 0A EMIT ;
        .drw link
        .set link,*+1
        .db  0,2,"CR"
CR:     acall lit
        .drw 0x0d
        acall EMIT
        acall lit
        .drw 0x0a
        ljmp EMIT

;C SPACE   --                      output a space
;   BL EMIT ;
        .drw link
        .set link,*+1
        .db  0,5,"SPACE"
SPACE:  acall BL
        ljmp EMIT

;C SPACES   n --                  output n spaces
;   BEGIN DUP WHILE SPACE 1- REPEAT DROP ;
        .drw link
        .set link,*+1
        .db 0,6,"SPACES"
SPACES: acall NEGATE
        sjmp SPCS2
SPCS1:  acall SPACE
        inc dptr
SPCS2:  mov a,dph
        jb acc.2,SPCS1
        ljmp DROP

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
        mov a,dpl
        orl a,dph
        mov dpl,@r0
        inc r0
        mov dph,@r0      ; pop hi byte -> TOS
        inc r0
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
        acall DROP
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

        mov a,@r0   ; get (-limit) + 8000h
        inc r0      ;   = (~limit) + 8001h
        cpl a       ;   in r5:r4
        add a,#01
        mov r4,a
        mov a,@r0
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
        mov a,#-1
        ajmp poptos ; go pop new TOS

XQDO:
        lcall TWODUP
        lcall XOR
        lcall zerosense
        jnz XDO     
                    ; A is zero
        ljmp TWODROP

;C I        -- n   R: sys1 sys2 -- sys1 sys2
;C                   get the innermost loop index
        .drw link
        .set link,*+1
        .db  0,1,"I"
II:     dec r0      ; push old TOS
        mov @r0,dph
        dec r0
        mov @r0,dpl
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
JJ:     lcall DUP
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
UMSTAR: mov a,@r0       ; u1 Lo in r1
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

        mov a,@r0       ; u1H*u2L -> B:A
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

        mov a,@r0       ; u1H*u2H -> B:A
        mov b,dph       ;       add into r5:r4
        mul ab
        add a,r4
        mov r4,a
        mov a,r5
        addc a,b
        mov dph,a       ; result in dph:r4:r3:r2
        mov dpl,r4
        mov @r0,dr3
        dec r0
        mov @r0,dr2
        ret

;C UM/MOD   ud u1 -- u2 u3     unsigned 32/16->16
        .drw link
        .set link,*+1
        .db  0,6,"UM/MOD"
UMSLASHMOD:             ; DPH:DPL = divisor
        mov a,@r0       ; r2:r3:r4:r5 = dividend
        mov r3,a        ; note stack order:
        inc r0          ;  ^      xxxx
        mov a,@r0       ;  |      xxxx
        mov r2,a        ; high   hi byte \ low
        inc r0          ; adrs   lo byte /  cell
        mov a,@r0       ;        hi byte \ high
        mov r5,a        ;  R0--> lo byte /  cell
        inc r0          ;  on    -------
        mov a,@r0       ;  entry
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
        mov @r0,dr2
        dec r0
        mov @r0,dr3
        ret

; BLOCK AND STRING OPERATIONS ===================

;C FILL    c-addr u char --  fill Data mem w/char
        .drw link
        .set link,*+1
        .db  0,4,"FILL"
FILL:   mov a,dpl       ; get char in A
        mov dr2,@r0     ; get count in r3:r2
        inc r0
        mov dr3,@r0
        inc r0
        lcall DROP
        inc r3          ; adjust r3,r2 for djnz loop
        inc r2
        sjmp filltest
fillloop: movx @dptr,a
          inc dptr
filltest: djnz r2,fillloop
          djnz r3,fillloop
        ljmp poptos     ; pop new TOS

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
CMOVE:  lcall QDUP
        lcall zerosense
        jz cmove2
        lcall OVER
        lcall PLUS
        lcall SWOP
        lcall XDO
cmove1: lcall DUP
        lcall CFETCH
        lcall II
        lcall CSTORE
        lcall ONEPLUS
        lcall loopsense
        jz cmove1
        lcall UNLOOP
        lcall DUP
cmove2: lcall DROP
        ljmp DROP

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
CMOVEUP: lcall QDUP
        lcall zerosense
        jz cmovu2
        lcall ONEMINUS
        lcall ROT
        lcall OVER
        lcall PLUS
        lcall ROT
        lcall ROT
        lcall OVER
        lcall PLUS
        lcall XDO
cmovu1: lcall DUP
        lcall CFETCH
        lcall II
        lcall CSTORE
        lcall ONEMINUS
        lcall LIT
        .drw -1
        lcall pluslpsense
        jz cmovu1
        lcall UNLOOP
        lcall DUP
cmovu2: lcall DROP
        ljmp DROP

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
        mov dr2,@r0      ; get count in r3:r2
        inc r0
        mov dr3,@r0
        inc r0
        mov dpl,@r0      ; get addr in DPTR
        inc r0
        mov dph,@r0
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
        mov @r0,dph     ; push updated addr
        dec r0
        mov @r0,dpl
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
        mov dr2,@r0      ; get count in r3:r2
        inc r0
        mov dr3,@r0
        inc r0
        mov dpl,@r0      ; get addr in DPTR
        inc r0
        mov dph,@r0
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
        mov @r0,dph     ; push updated addr
        dec r0
        mov @r0,dpl
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
NEQUAL: push dr7
        push dr6
        mov r2,dpl      ; count
        mov r3,dph
        lcall drop
        mov dr4,dpl      ; get Code addr in r5:r4
        mov dr5,dph
        lcall drop
        mov dr6,dpl      ; get Data addr in r7:r6
        mov dr7,dph
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
        xrl a,r1
        jnz Nequfail
Nequtest: djnz r2,Nequloop
        djnz r3,Nequloop
        mov dph,r3      ; strings match, r3=0,
        mov dpl,r3      ;  so make TOS=0
        sjmp Nequdone
Nequfail: mov dptr,#-1
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

;C >BODY    xt -- a-addr      adrs of CREATE data
;   3 + I@ ;                   8051 (3 byte CALL)
        .drw link
        .set link,*+1
        .db  0,5,">BODY"
TOBODY: inc dptr
        inc dptr
        inc dptr
        ljmp FETCH

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
COMMAXT: lcall LIT
        .drw 0x12
        lcall ICCOMMA
        lcall SWAPBYTES
        ljmp ICOMMA

;Z !CF    adrs cfa --   set code action of a word
;   012 OVER IC!         store 'LCALL adrs' instr
;   1+ SWAP >< SWAP I! ;     8051 Harvard VERSION
; Depending on the implementation this could
; append CALL adrs or JUMP adrs.
        .drw link
        .set link,*+1
        .db  0,3,"!CF"
STORECF: lcall LIT
        .drw 0x12
        lcall OVER
        lcall CSTORE
        lcall ONEPLUS
        lcall SWOP
        lcall SWAPBYTES
        lcall SWOP
        ljmp STORE

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
STORCOLON: lcall LIT
        .drw -5
        ljmp IALLOT

NFATOLFA: lcall DUP
        mov dptr,#3
        ljmp MINUS

;C TYPE    c-addr +n --       type line to term'l
;   ?DUP IF
;     OVER + SWAP DO I C@ EMIT LOOP
;   ELSE DROP THEN ;
        .drw link
        .set link,*+1
        .db 0,4,"TYPE"
TYPE:   lcall bounds
TYPE1:  lcall break
        lcall DUP
        lcall CFETCH
        lcall EMIT
        inc dptr
        sjmp TYPE1

; ; UTILITY WORDS AND STARTUP =====================
; 
; ;X WORDS    --          list all words in dict.
; ;   LATEST @ BEGIN
; ;       DUP ICOUNT 7F AND ITYPE SPACE
; ;       NFA>LFA I@
; ;   DUP 0= UNTIL
; ;   DROP ;
;         .drw link
;         .set link,*+1
;         .db 0,5,"WORDS"
        .drw link
        .set link,*+1
        .db 0,5,"WORDS"

WORDS:  lcall FALSE
        lcall LATEST
WORDS1: lcall FETCH
        lcall break
        lcall DUP
        lcall COUNT
        lcall LIT
        .drw 0x7f
        lcall AND
        lcall TYPE
        lcall SPACE
        lcall NFATOLFA
        sjmp WORDS1

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

; high level code may use 'branch' as an argument
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
        ljmp ICCOMMA

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
        ljmp CSTORE

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
TOIN:   lcall docon
        .drw 0xff00 + _TOIN

;C BASE    -- a-addr       holds conversion radix
;  4 USER BASE
        .drw link
        .set link,*+1
        .db  0,4,"BASE"
BASE:   lcall docon
       .drw 0xff00 + _BASE

;C STATE   -- a-addr         holds compiler state
;  6 USER STATE
        .drw link
        .set link,*+1
        .db  0,5,"STATE"
STATE:  lcall docon
        .drw 0xff00 + _STATE

;Z DP      -- a-addr         holds dictionary ptr
;  8 USER DP
        .drw link
        .set link,*+1
        .db  0,2,"DP"
DP:     lcall docon
        .drw 0xff00 + _DP

;Z 'SOURCE  -- a-addr        two cells: len, adrs
; 10 USER 'SOURCE
        .drw link
        .set link,*+1
        .db  0,7,0x27,"SOURCE"  ; 27h = '
TICKSOURCE: lcall docon
        .drw 0xff00 + _SOURCE

;Z 'SOURCE-ID  -- a-addr
        .drw link
        .set link,*+1
        .db  0,10,0x27,"SOURCE-ID"
TICKSOURCEID: lcall docon
        .drw 0xff00 + _SRCID

;Z 'FAULT
        .drw link
        .set link,*+1
        .db  0,6,0x27,"FAULT"
TICKFAULT: lcall docon
        .drw 0xff00 + _FAULT

;Z LATEST    -- a-addr         last word in dict.
;   14 USER LATEST
        .drw link
        .set link,*+1
        .db  0,6,"LATEST"
LATEST: lcall docon
        .drw 0xff00 + _LATEST

THISXT: lcall docon
        .drw 0xff00 + _THISXT

;Z HP       -- a-addr                HOLD pointer
;   16 USER HP
        .drw link
        .set link,*+1
        .db  0,2,"HP"
HP:     lcall docon
        .drw 0xff00 + _HP

;Z LP       -- a-addr         Leave-stack pointer
;   18 USER LP
        .drw link
        .set link,*+1
        .db  0,2,"LP"
LP:     lcall docon
        .drw 0xff00 + _LP

;Z IDP    -- a-addr        ROM dictionary pointer
;   20 USER IDP
        .drw link
        .set link,*+1
        .db  0,3,"IDP"
IDP:
        mov a,_IDP
        orl a,_IDP+1
        jz DP
        lcall docon
        .drw 0xff00 + _IDP

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

;Z HANDLER   -- a-addr         last exception handler
        .drw link
        .set link,*+1
        .db  0,7,"HANDLER"
HANDLER: lcall docon
        .drw 0xff00 + _HANDLER

; TIME ==========================================

        .drw link
        .set link,*+1
        .db  0,3,"MS@"
MSFETCH:
        lcall LIT
        .drw 0xff00 + _MS
        ljmp TWOFETCH

        .drw link
        .set link,*+1
        .db  0,3,"TTH"
TTH:    lcall docon
        .drw 0xff00 + _TTH

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
        jnz  DNEGATE
        ret

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
MSTAR:  lcall TWODUP
        lcall XOR
        lcall TOR
        lcall SWOP
        lcall ABS
        lcall SWOP
        lcall ABS
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
SMSLASHREM: lcall TWODUP
        lcall XOR
        lcall TOR
        lcall OVER
        lcall TOR
        lcall ABS
        lcall TOR
        acall DABS
        lcall RFROM
        lcall UMSLASHMOD
        lcall SWOP
        lcall RFROM
        lcall QNEGATE
        lcall SWOP
        lcall RFROM
        ljmp  QNEGATE

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
        lcall TWODUP
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
        ajmp SMSLASHREM

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
        ajmp SMSLASHREM

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
MAX:    lcall TWODUP
        lcall LESS
        lcall zerosense
        jz max1
        ljmp NIP
max1:   ljmp DROP

;C MIN    n1 n2 -- n3              signed minimum
;   2DUP > IF SWAP THEN DROP ;
        .drw link
        .set link,*+1
        .db  0,3,"MIN"
MIN:    lcall TWODUP
        lcall GREATER
        lcall zerosense
        jz min1
        ljmp NIP
min1:   ljmp DROP

;Z umin     u1 u2 -- u           unsigned minimum
;   2DUP U> IF SWAP THEN DROP ;
        .drw link
        .set link,*+1
        .db 0,4,"UMIN"
; UMIN:   lcall TWODUP
;         lcall UGREATER
;         lcall zerosense
;         jz UMIN1
;         lcall SWOP
; UMIN1:  ljmp DROP
UMIN:   mov a,dpl      ; low byte
        clr c
        subb a,@r0
        inc r0
        mov a,dph       ; high byte
        subb a,@r0
        jc UMIN1
        mov dph,@r0
        dec r0
        mov dpl,@r0
        inc r0
UMIN1:  inc r0
        ret

;Z umax    u1 u2 -- u            unsigned maximum
;   2DUP U< IF SWAP THEN DROP ;
        .drw link
        .set link,*+1
        .db 0,4,"UMAX"
UMAX:   lcall TWODUP
        lcall ULESS
        lcall zerosense
        jz UMAX1
        ljmp NIP
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
ACCEPT:
        mov a,_TTH
        jz ACC0
        lcall LIT                       ; xxx tethered
        .drw 30
        lcall EMIT
ACC0:
        lcall OVER
        lcall PLUS
        lcall ONEMINUS
        lcall OVER
ACC1:   lcall KEY
        mov a,dpl
        cjne a,#0x0a,notNL
        lcall DROP
        sjmp ACC1
notNL:  cjne a,#0x0d,ACC5
        lcall DROP
        lcall NIP
        lcall SWOP
        ljmp MINUS
ACC5:
        mov a,_TTH
        jnz ACC6
        lcall DUP
        lcall EMIT
ACC6:
        mov a,dpl
        cjne a,#8,ACC3
        lcall DROP
        lcall ONEMINUS
        lcall TOR
        lcall OVER
        lcall RFROM
        acall UMAX
        sjmp ACC1
ACC3:   lcall SWOP
        mov a,@r0
        movx @dptr,a
        inc r0
        inc r0
        inc dptr
        lcall OVER
        acall UMIN
ACC4:   sjmp ACC1

; From Forth200x - public domain

isspace:                        ; ( c -- f )
        lcall LIT
        .drw 0x21
        ljmp ULESS

isnotspace:                     ; ( c -- f )
        lcall isspace
        ljmp ZEROEQUAL

; : xt-skip   ( addr1 n1 xt -- addr2 n2 ) \ gforth
;     \ skip all characters satisfying xt ( c -- f )
;     >r
;     BEGIN
;         over c@ r@ execute
;         overand
;     WHILE
;         1/string
;     REPEAT
;     rdrop ;

xt_skip:
        lcall TOR
xt_skip0:
        lcall OVER
        lcall CFETCH
        lcall RFETCH
        lcall EXECUTE
        lcall OVER
        lcall AND
        lcall zerosense
        jz xt_skip1

        lcall LIT
        .drw 1
        lcall SLASHSTRING
        ljmp xt_skip0

xt_skip1:
        lcall RFROM
        ljmp DROP

; header parse-name
; : parse-name ( "name" -- c-addr u )
;     source >in @i /string
;     ['] isspace? xt-skip over >r
;     ['] isnotspace?
; : _parse
;     xt-skip ( end-word restlen r: start-word )
;     2dup 0<> - sourceA @i - >in!
;     drop r>
;     tuck -
; ;
        .drw link
        .set link,*+1
        .db 0,10,"PARSE-NAME"

PARSE_NAME:
        lcall SOURCE
        lcall TOIN
        lcall FETCH
        lcall SLASHSTRING

        lcall LIT
        .drw isspace
        lcall xt_skip
        lcall OVER
        lcall TOR

        lcall LIT
        .drw isnotspace

_parse:
        lcall xt_skip

        lcall TWODUP
        lcall ZERONOTEQUAL
        lcall MINUS
        lcall SOURCE
        lcall DROP
        lcall MINUS
        lcall TOIN
        lcall STORE

        lcall DROP
        lcall RFROM

        lcall TUCK
        ljmp MINUS

; : isnotdelim
;     scratch @i <>
; ;

isnotdelim:
        lcall DUP
        mov dpl,b
        mov dph,#0
        ljmp NOTEQUAL

; header parse
; : parse ( "ccc<char" -- c-addr u )
;     scratch _!
;     source >in @i /string
;     over >r
;     ['] isnotdelim
;     _parse
; ;

        .drw link
        .set link,*+1
        .db 0,5,"PARSE"
PARSE:
        mov b,dpl
        lcall DROP

        lcall SOURCE
        lcall TOIN
        lcall FETCH
        lcall SLASHSTRING

        lcall OVER
        lcall TOR

        lcall LIT
        .drw isnotdelim

        ljmp _parse

        .drw link
        .set link,*+1
        .db IMMED,2,".("
        lcall LIT
        .drw 0x29
        lcall PARSE
        ljmp TYPE

;Z ICOUNT  c-addr1 -- c-addr2 u  counted->adr/len
;   DUP CHAR+ SWAP IC@ ;          from Code space
        .drw link
        .set link,*+1
        .db  0,6,"ICOUNT"
ICOUNT: lcall DUP
        inc dptr
        lcall SWOP
        ljmp CFETCH

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
        lcall CFETCH
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
        ; lcall towordbuf
        ; lcall DROP
        ; lcall LIT
        ; .drw 0xff00+WORDBUF
        lcall IHERE
        lcall TUCK
        lcall OVER
        lcall CFETCH
        inc dptr
        ljmp CMOVE

IWORD_W:
        lcall XWORD
        lcall towordbuf
        lcall DROP
        lcall LIT
        .drw 0xff00+WORDBUF
        lcall IHERE
        lcall TUCK
        lcall OVER
        lcall CFETCH
        inc dptr
        ljmp CMOVE

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
        lcall HP
        lcall PLUSSTORE
        lcall HP
        lcall FETCH
        ljmp CSTORE

;C <#    --              begin numeric conversion
;   PAD HP ! ;          (initialize Hold Pointer)
        .drw link
        .set link,*+1
        .db 0,2,"<#"
LESSNUM: lcall PAD
        lcall HP
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
NUM:    lcall BASE
        lcall FETCH
        lcall UDSLASHMOD
        lcall ROT
        lcall TODIGIT
        sjmp HOLD

;C #S    ud1 -- ud2      convert remaining digits
;   BEGIN # 2DUP OR 0= UNTIL ;
        .drw link
        .set link,*+1
        .db 0,2,"#S"
NUMS:
NUMS1:
        lcall NUM
        lcall TWODUP
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
NUMGREATER: lcall TWODROP
        lcall HP
        lcall FETCH
        lcall PAD
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
        lcall HOLD
SIGN1:  ret

;C D.R  d n --
;   >R DUP >R DABS <# #S R> SIGN #> R> OVER - SPACES TYPE
        .drw link
        .set link,*+1
        .db 0,3,"D.R"
DDOTR:  lcall TOR
        lcall DUP
        lcall TOR
        lcall DABS
        lcall LESSNUM
        lcall NUMS
        lcall RFROM
        lcall SIGN
        lcall NUMGREATER
        lcall RFROM
        lcall OVER
        lcall MINUS
        lcall SPACES
        ljmp  TYPE

;C d.  ( d -- )
;   0 D.R SPACE
        .drw link
        .set link,*+1
        .db 0,2,"D."
DDOT:   lcall FALSE
        acall DDOTR
        ljmp SPACE

;C .  ( n -- )
;   S>D D.
        .drw link
        .set link,*+1
        .db 0,1,"."
        lcall STOD
        sjmp DDOT

;C u.  ( u -- )
;   0 D.
        .drw link
        .set link,*+1
        .db 0,2,"U."
UDOT:   lcall FALSE
        sjmp DDOT

;C .r  ( n1 n2 -- )
;   >R S>D R> D.R
        .drw link
        .set link,*+1
        .db 0,2,".R"
DOTR:   lcall TOR
        lcall STOD
        lcall RFROM
        sjmp DDOTR

;C u.r  ( u n -- )
;   0 SWAP D.R
        .drw link
        .set link,*+1
        .db 0,3,"U.R"
UDOTR:  lcall FALSE
        lcall SWOP
        sjmp DDOTR

;C DECIMAL  --         set number base to decimal
;   10 BASE ! ;
        .drw link
        .set link,*+1
        .db 0,7,"DECIMAL"
DECIMAL: lcall LIT
        .drw 10
        lcall BASE
        ljmp STORE

;X HEX     --              set number base to hex
;   16 BASE ! ;
        .drw link
        .set link,*+1
        .db 0,3,"HEX"
HEX:    lcall LIT
        .drw 16
        lcall BASE
        ljmp STORE

; DICTIONARY MANAGEMENT =========================

;C HERE    -- addr         returns dictionary ptr
;   DP @ ;
        .drw link
        .set link,*+1
        .db 0,4,"HERE"
HERE:   lcall DP
        ljmp FETCH

;C ALLOT   n --          allocate n bytes in dict
;   DP +! ;
        .drw link
        .set link,*+1
        .db 0,5,"ALLOT"
ALLOT:  lcall DP
        ljmp PLUSSTORE

;C ,    x --                  append cell to dict
;   HERE ! 1 CELLS ALLOT ;
        .drw link
        .set link,*+1
        .db 0,1,","
COMMA:  lcall HERE
        lcall STORE
        lcall lit
        .drw 2
        ljmp ALLOT

;C C,   char --               append char to dict
;   HERE C! 1 CHARS ALLOT ;
        .drw link
        .set link,*+1
        .db 0,2,"C,"
CCOMMA: lcall HERE
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
IHERE:  lcall IDP
        ljmp FETCH

;Z IALLOT   n --    allocate n bytes in Code dict
;   IDP +! ;
        .drw link
        .set link,*+1
        .db 0,6,"IALLOT"
IALLOT: lcall IDP
        ljmp PLUSSTORE

;Z I,   x --             append cell to Code dict
;   IHERE I! 1 CELLS IALLOT ;
        .drw link
        .set link,*+1
        .db 0,2,"I,"
ICOMMA: lcall IHERE
        lcall STORE
        lcall lit
        .drw 2
        sjmp IALLOT

;Z IC,  x --             append char to Code dict
;   IHERE IC! 1 CHARS IALLOT ;
        .drw link
        .set link,*+1
        .db 0,3,"IC,"
ICCOMMA: lcall IHERE
        lcall CSTORE
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
SOURCE: lcall TICKSOURCE
        ljmp TWOFETCH

;X /STRING  a u n -- a+n u-n          trim string
; ;   ROT OVER + ROT ROT - ;
;         .drw link
;         .set link,*+1
;         .db 0,7,"/STRING"
; SLASHSTRING: lcall ROT
;         lcall OVER
;         lcall PLUS
;         lcall ROT
;         lcall ROT
;         ljmp MINUS

;   DUP >R - SWAP R> + SWAP
        .drw link
        .set link,*+1
        .db 0,7,"/STRING"
SLASHSTRING: push dpl
        push dph
        lcall MINUS
        lcall SWOP
        lcall RFROM
        lcall PLUS
        ljmp SWOP

;Z >counted  src n dst --     copy to counted str
;   2DUP C! CHAR+ SWAP CMOVE ;
        .drw link
        .set link,*+1
        .db 0,8,">COUNTED"
TOCOUNTED: lcall TWODUP
        lcall CSTORE
        inc dptr
        lcall SWOP
        ljmp CMOVE

; ***
; This is approximately the end of the second 2K
; block.  CALLs and JMPs crossing this boundary
; must use the Long form.
; ***

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
XWORD:  
        lcall DUP
        lcall SOURCE
        lcall TOIN
        lcall FETCH
        lcall SLASHSTRING
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
        ljmp CFETCH

toupper:
        add a,#-0x61
        add a,#-26
        jc toupper1
        add a,#(0x61 + 26 - 32)
        ret
toupper1:
        add a,#(0x61 + 26)
        ret

        .drw link
        .set link,*+1
        .db 0,1,"U"
        mov a,dpl
        lcall toupper
        mov dpl,a
        ret

; towordbuf ( c-addr -- c-addr )
; Copy counted string to WORDBUF, folding case

towordbuf:
        movx a,@dptr
        anl a,#31
        mov WORDBUF,a
        jz towordbuf2
        mov r2,a
        add a,#WORDBUF
        mov r1,a
copyloop:
        mov a,r2
        movc a,@a+dptr
        lcall toupper
        mov @r1,a
        dec r1
        djnz r2,copyloop
towordbuf2:
        ret

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
FIND:
        lcall towordbuf
        lcall LATEST
        sjmp FIND3
FIND1:
        clr a
        movc a,@a+dptr
        cjne a,WORDBUF,FIND2

        mov r2,a
        add a,#WORDBUF
        mov r1,a
cmploop:
        mov a,r2
        movc a,@a+dptr
        xrl a,@r1
        jnz FIND2
        dec r1
        djnz r2,cmploop
        ;; Match found!
        lcall NIP
        lcall DUP
        lcall NFATOCFA
        lcall SWOP
        lcall IMMEDQ
        lcall ZEROEQUAL
        lcall LIT
        .drw 1
        ljmp  OR
FIND2:
        mov a,dpl
        add a,#-3
        mov dpl,a
        jc FIND3
        dec dph
FIND3:
        movx a,@dptr    ; low byte
        mov r2,a        ; ..temporary stash
        inc dptr
        movx a,@dptr    ; high byte
        mov dpl,r2      ; copy to TOS (DPTR)
        mov dph,a
        jnz FIND1
        orl a,r2
        jnz FIND1
        ret

;C LITERAL  x --           append numeric literal
;   STATE @ IF ['] LIT ,XT I, THEN ; IMMEDIATE
        .drw link
        .set link,*+1
        .db IMMED,7,"LITERAL"
LITERAL:
        lcall LIT
        .drw LIT
        lcall COMMAXT
        ljmp ICOMMA

;C 2LITERAL d --           append numeric literal
        .drw link
        .set link,*+1
        .db IMMED,8,"2LITERAL"
TWOLITERAL:
        lcall SWOP
        lcall LITERAL
        ljmp LITERAL

;Z DIGIT?   c -- n -1   if c is a valid digit
;Z            -- x  0   otherwise
;   [ HEX ] DUP 39 > 100 AND +     silly looking
;   DUP 140 > 107 AND -   30 -     but it works!
;   DUP BASE @ U< ;
        .drw link
        .set link,*+1
        .db 0,6,"DIGIT?"
DIGITQ: 
        mov a,dpl
        lcall toupper
        mov dpl,a
        lcall DUP
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

;  : consume1 ( caddr u ch -- caddr' u' f )
;      >r over c@ r> =
;      over 0<> and
;      dup >r negate /string r>
;  ;

CONSUME1:
        lcall TOR
        lcall OVER
        lcall CFETCH
        lcall RFROM
        lcall EQUAL
        lcall OVER
        lcall ZERONOTEQUAL
        lcall AND
        lcall DUP
        lcall TOR
        lcall NEGATE
        lcall SLASHSTRING
        lcall RFROM
        ret
;  
;  : doublenumber       ( caddr u -- n 0 | d. 1 )
;      0. 2swap
;      [char] - consume1 >r
;      >number
;      [char] . consume1 >r            \ 0 is single, -1 double
;      nip ?error                      \ any chars remain: abort
;      r> ?dneg                        \ is negative
;      r> ?dup and                     \ if single, remove high cell
;  ;

BASEDOUBLENUMBER:
        lcall LIT
        .drw '$'
        lcall CONSUME1
        lcall zerosense
        jz BASEDOUBLENUMBER1
        mov a,#16
BASED:
        push _BASE
        mov _BASE,a
        acall DOUBLENUMBER
        pop _BASE
        ret

BASEDOUBLENUMBER1:
        lcall LIT
        .drw '%'
        lcall CONSUME1
        lcall zerosense
        jz BASEDOUBLENUMBER2
        mov a,#2
        sjmp BASED

BASEDOUBLENUMBER2:
        lcall LIT
        .drw '#'
        lcall CONSUME1
        lcall zerosense
        jz BASEDOUBLENUMBER3
        mov a,#10
        sjmp BASED

BASEDOUBLENUMBER3:
        lcall LIT
        .drw 0x27
        lcall CONSUME1
        lcall zerosense
        jz DOUBLENUMBER
        lcall DROP
        lcall CFETCH
        ljmp FALSE
        
DOUBLENUMBER:
        lcall FALSE
        lcall FALSE
        lcall TWOSWAP
        lcall LIT
        .drw '-'
        lcall CONSUME1
        lcall TOR                       ; sign
        lcall TONUMBER
        lcall LIT
        .drw '.'
        lcall CONSUME1
        mov b,dpl                       ; double
        lcall DROP
        lcall NIP
        lcall ZERONOTEQUAL
        lcall THROW13
        lcall RFROM                     ;
        lcall QDNEGATE
        lcall DUP
        mov dpl,b
        mov dph,b
        lcall QDUP
        lcall AND
        ret

        .drw link
        .set link,*+1
        .db 0,7,"SOURCE!"
SOURCESTORE:
        lcall TOIN
        lcall STORE
        lcall TICKSOURCE
        ljmp TWOSTORE

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
INTERPRET:
        lcall FALSE
        lcall SOURCESTORE
INTER1: lcall BL
        lcall XWORD
        lcall DUP
        lcall CFETCH
        lcall zerosense
        jz INTER9
        lcall FIND
        mov a,dpl
        lcall DROP
        jz INTER4
        dec a
        anl a,_STATE
        jnz INTER2
        lcall EXECUTE
        sjmp INTER1
INTER2: lcall COMMAXT
        sjmp INTER1

INTER4:
        lcall COUNT
        lcall BASEDOUBLENUMBER
        mov a,dpl
        lcall DROP
        mov r1,_state
        cjne r1,#-1,INTER1
        jz INTER5

        lcall TWOLITERAL
        sjmp INTER1
INTER5:
        lcall LITERAL
        sjmp INTER1

INTER9: ljmp DROP

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
QUIT:
        lcall L0
        lcall LP
        lcall STORE
        mov sp,#RSP0
        clr a
        mov _SRCID,a
        mov _SRCID+1,a
        mov _STATE,a
        mov _STATE+1,a
QUIT1:  lcall LIT
        .drw repl
        lcall CATCH
        lcall QDUP
        lcall zerosense
        jnz QUIT3

        lcall STATE
        lcall FETCH
        lcall ZEROEQUAL
        lcall zerosense
        jz QUIT2
        lcall XISQUOTE
        .db 2,"ok"
        lcall ITYPE
QUIT2:
        mov a,_TTH
        jnz QUIT1
        lcall CR
        sjmp QUIT1

QUIT3:
        lcall TICKFAULT
        lcall FETCH
        lcall EXECUTE
        sjmp QUIT

repl:   lcall TIB
        lcall DUP
        lcall TIBSIZE
        lcall ACCEPT
        lcall SPACE
        ljmp INTERPRET

FAULT:  lcall XISQUOTE
        .db 7,"error: "
        ljmp TYPE
        ljmp DOTX

;C ABORT    i*x --   R: j*x --   clear stk & QUIT
;   S0 SP!  QUIT ;
        .drw link
        .set link,*+1
        .db 0,5,"ABORT"
ABORT:  lcall TRUE
        ljmp THROW

;C '    -- xt             find word in dictionary
;   BL WORD FIND
;   0= ABORT" ?" ;
        .drw link
        .set link,*+1
        .db 0,1,0x27    ; 27h = '
TICK:   lcall BL
        lcall XWORD
        lcall FIND
WASFOUND:
        lcall ZEROEQUAL
THROW13:
        lcall LIT
        .drw -13
        lcall AND
        ljmp THROW

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
        lcall IWORD_W
        lcall CFETCH
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
        lcall NFATOCFA
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
RECURSE: lcall THISXT
        lcall FETCH
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
        lcall CFETCH
        lcall LIT
        .drw 0x80
        lcall OR
        lcall SWOP
        ljmp CSTORE

;Z REVEAL   --      "reveal" latest definition
;   LATEST @ DUP IC@ 7F AND SWAP IC! ;
; Harvard model.
        .drw link
        .set link,*+1
        .db 0,6,"REVEAL"
REVEAL: lcall LATEST
        lcall FETCH
        lcall DUP
        lcall CFETCH
        lcall LIT
        .drw 0x7F
        lcall AND
        lcall SWOP
        ljmp CSTORE

;C :        --           begin a colon definition
;   CREATE HIDE ] !COLON ;
        .drw link
        .set link,*+1
        .db 0,1,":"
COLON:  acall CREATE
        acall HIDE
        acall RIGHTBRACKET
        lcall STORCOLON
        lcall IHERE
        lcall THISXT
        lcall STORE
        ret

        .drw link
        .set link,*+1
        .db 0,7,":NONAME"
NONAME: lcall IHERE
        lcall DUP
        lcall THISXT
        lcall STORE
        ljmp RIGHTBRACKET

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
        lcall WASFOUND
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
WHILE:  lcall IF
        ljmp SWOP

;C REPEAT   adrs1 adrs2 --     resolve WHILE loop
;   POSTPONE AGAIN POSTPONE THEN ; IMMEDIATE
        .drw link
        .set link,*+1
        .db IMMED,6,"REPEAT"
REPEAT: lcall AGAIN
        sjmp THEN

;Z >L   x --   L: -- x        move to leave stack
;   CELL LP +!  LP @ ! ;      (L stack grows up)
        .drw link
        .set link,*+1
        .db 0,2,">L"
TOL:    lcall LIT
        .drw 2
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
        lcall LIT
        .drw -2
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
        ljmp TOL

;C ?DO      -- adrs   L: -- 0
;   ['] qbranch ,BRANCH  IHERE DUP ,DEST ;
;   IMMEDIATE
        .drw link
        .set link,*+1
        .db IMMED,3,"?DO"
QDO:    
        lcall LIT
        .drw 0x0
        lcall TOL

        lcall LIT
        .drw xqdo
        lcall COMMABRANCH
        lcall IHERE
        lcall DUP
        lcall TOL
        lcall COMMADEST
        lcall IHERE
        ret

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
LOOP1:  lcall LFROM
        lcall QDUP
        lcall zerosense
        jz LOOP2
        lcall THEN
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
        ljmp TOL

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
DEPTH:  mov a,#S0
        clr c
        subb a,r0
        rr a
        ljmp PUSHA

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

HEX2:   lcall DUP
        mov a,dpl
        swap a
        acall HEX1
        mov a,dpl
HEX1:   anl a,#0x0f
        add a,#-0x0a
        jnc numeric
        add a,#7
numeric: add a,#0x3a
        mov dpl,a
        ljmp EMIT

        .drw link
        .set link,*+1
        .db 0,3,".X2"
DOTX2:  lcall HEX2
        ljmp SPACE

        .drw link
        .set link,*+1
        .db 0,2,".X"
DOTX:   lcall DUP
        lcall swapbytes
        lcall HEX2
        lcall HEX2
        ljmp SPACE

; regspec is a register specifier
; All registers are in page DFxx
;
; +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
; |        offset         |     | hibit  | lobit  |
; +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;

        .drw link
        .set link,*+1
        .db 0,2,"B@"
BFETCH:
        push dpl
        mov a,dph
        jb acc.7,BFETCH_1
        mov dpl,a
        mov dph,#0xdf
        movx a,@dptr
        mov b,a
        mov a,#b
BFETCH_1:
        mov dptr,#bfetch_p0
        movx @dptr,a
        pop acc
        mov r1,a                ; r1 is 6-bit hi:lo
        anl a,#7
        mov r2,a                ; r2 is 3-bit lo
        ljmp bfetch_

        .drw link
        .set link,*+1
        .db 0,2,"B!"
BSTORE:
        mov r1,dpl
        mov a,dph
        jb acc.7,BSTORE_1
        mov dpl,a
        mov dph,#0xdf
        push dph
        push dpl
        movx a,@dptr
        mov b,a
        mov a,#b
        acall BSTORE_1
        lcall DUP
        pop dpl
        pop dph
        mov a,b
        movx @dptr,a
        ljmp DROP

BSTORE_1:
        mov dptr,#bstore_p0
        movx @dptr,a
        mov dptr,#bstore_p1
        movx @dptr,a
        ljmp bstore_

        ; The PHANTOM code block here is not actually executed
        ; in flash. Instead it is copied into RAM, and executed
        ; there. So it must avoid external relative jumps,
        ; i.e. acall and ajmp.

PHANTOM:
        .equ bfetch_,dataram + (* - PHANTOM)
        mov a,P0
        .equ bfetch_p0,dataram + (* - 1 - PHANTOM)
        mov r3,a                ; r3 is fetched value
 
        mov dptr,#bmasks        ; mask r3
        mov a,r1
        movc a,@a+dptr
        anl a,r3
 
        inc r2
        sjmp bfetch1
bfetch0:
        rr a
bfetch1:
        djnz r2,bfetch0
        mov dph,#0
        mov dpl,a
        ret

        .equ bstore_,dataram + (* - PHANTOM)
        mov a,r1                ; r1 is 6-bit hi:lo
        anl a,#7
        mov r2,a                ; r2 is 3-bit lo
        mov a,P0
        .equ bstore_p0,dataram + (* - 1 - PHANTOM)
        mov r3,a                ; r3 is old value
        
        mov a,@r0               ; r5 is new value, shifted
        inc r2
        sjmp bstore1
bstore0:
        rl a
bstore1:
        djnz r2,bstore0
        mov r5,a

        mov DPS,#1              ; {
        mov dptr,#bmasks        ; mask into r4
        mov a,r1
        movc a,@a+dptr
        mov r4,a
        mov DPS,#0              ; }

        mov a,r5
        xrl a,r3
        anl a,r4
        xrl a,r3

        mov P0,a
        .equ bstore_p1,dataram + (* - 1 - PHANTOM)
        ljmp TWODROP
        .equ PHANTOM_SIZE,*-PHANTOM

bmasks:
        .db     1,0,0,0,0,0,0,0
        .db     3,2,0,0,0,0,0,0
        .db     7,6,4,0,0,0,0,0
        .db     15,14,12,8,0,0,0,0
        .db     31,30,28,24,16,0,0,0
        .db     63,62,60,56,48,32,0,0
        .db     127,126,124,120,112,96,64,0
        .db     255,254,252,248,240,224,192,128

; RFST (0xE1) - RF Strobe Commands
; RFTXRXIF is TCON.1

        .equ    RFST,   0xe1
        .equ    RFD,    0xd9

radio_tx:
C1:     jnb TCON.1,C1
        clr TCON.1
        mov RFD,a
        ret

        .drw link
        .set link,*+1
        .db 0,3,"ATX"
ATX:
        mov RFST,#03H ; Start TX with STX command strobe
        mov a,#0x02
        lcall radio_tx
        mov a,dpl
        lcall radio_tx
        mov a,dph
        lcall radio_tx
        ljmp DROP

        .drw link
        .set link,*+1
        .db 0,6,">RADIO"
TO_RADIO:
        mov RFST,#03H ; Start TX with STX command strobe
        mov a,dpl
        mov r1,a
        lcall radio_tx
        lcall DROP
radioloop:
        movx a,@dptr
        lcall radio_tx
        inc dptr
        djnz r1,radioloop
        ljmp DROP

radio_rx:
        jnb TCON.1,radio_rx
        clr TCON.1
        mov a,RFD
        movx @dptr,a
        inc dptr
        ret

        .drw link
        .set link,*+1
        .db 0,6,"RADIO>"
RADIOFROM:
        lcall DUP
        mov RFST,#02h ; RFST_SRX
        lcall radio_rx
        mov r1,a
ARX1:
        lcall radio_rx
        djnz r1,ARX1

        mov RFST,#04h ; RFST_IDLE

        lcall DROP
        ljmp COUNT

INITBLK:
        .drw    dataram+80      ; DP
        .drw    CODEHERE        ; IDP
        .drw    lastword        ; LATEST
        .drw    0               ; HANDLER
        .drw    10              ; BASE
        .drw    0,0             ; MS
        .drw    0               ; STATE
        .drw    0               ; TTH
        .drw    0               ; >IN
        .drw    0,0             ; 'SOURCE
        .drw    0               ; HP
        .drw    0               ; LP
        .drw    0               ; THISXT
        .drw    0               ; SOURCEID
        .drw    FAULT           ; 'FAULT

        .equ    INITBLKSIZE,*-INITBLK
INITB:                          ; ( -- a u ) \ return init block
        lcall lit
        .drw INITBLK
        lcall LIT
        .drw INITBLKSIZE
        ret

;C COMMIT    --
        .drw link
        .set link,*+1
        .db 0,6,"COMMIT"
COMMIT: lcall DP
        lcall INITB
        ljmp MOVE

        .drw link
        .set link,*+1
        .db 0,9,"SLEEPMODE"     ; sleepmode ( m -- ) \ go into mode m
SLEEPMODE:
        mov ie,#0xa0
        mov a,W0RTIME0
        cjne a,W0RTIME0,sleepmode1
sleepmode1:
        mov a,dpl
        orl a,#4
        mov SLEEP,a
        nop
        nop
        nop
        mov PCON,#1
        nop
        ljmp DROP

;Z COLD     --      cold start Forth system
;   UINIT U0 #INIT I->D      init user area
;   ." 8051 CamelForth etc."
;   ABORT ;
        .drw link
        .set link,*+1
        .db 0,4,"COLD"
COLD:
        lcall XISQUOTE
        .db 22
        .drw 0,0,10,0   ; reserved,>IN,BASE,STATE
        .drw dataram    ; DP
        .drw 0,0        ; SOURCE init'd elsewhere
        .drw lastword   ; LATEST
        .drw 0,0        ; HP,LP init'd elsewhere
        .drw 0          ; unused
        lcall U0
        lcall SWOP
        lcall CMOVE

        lcall LIT
        .drw PHANTOM
        lcall LIT
        .drw dataram
        lcall LIT
        .drw PHANTOM_SIZE
        lcall MOVE

        lcall INITB
        lcall DP
        lcall SWOP
        lcall MOVE

        lcall XISQUOTE
       .DB 35,"8051 CamelForth v1.6  18 Aug 1999"
       .DB 0x0d,0x0a
        lcall ITYPE

        mov r0,#S0
        ljmp QUIT

CODEHERE:
; ===============================================
; Initial dictionary pointer for CamelForth.
; DO NOT delete!
    .equ lastword,link      ; NFA of final word
