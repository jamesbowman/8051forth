ACALL addr      11
ADD A #8        24
ADD A iram      25
ADD A @Rn       26
ADD A Rn        28
ADDC A #8       34
ADDC A iram     35
ADDC A @Rn      36
ADDC A Rn       38
AJMP addr       01
ANL iram A      52
ANL iram #8     53
ANL A #8        54
ANL A iram      55
ANL A @Rn       56
ANL A Rn        58
ANL C bitaddr   82
ANL C / bitaddr B0
CJNE A #8 rel   B4
CJNE A iram rel B5
CJNE @Rn #8 rel B6
CJNE Rn #8 rel  B8
CLR bitaddr     C2
CLR C           C3
CLR A           E4
CPL A           F4
CPL C           B3
CPL bitaddr     B2
DA              D4
DEC A           14
DEC iram        15
DEC @Rn         16
DEC Rn          18
DIV AB          84
DJNZ iram rel   D5
DJNZ Rn rel     D8
INC A           04
INC iram        05
INC @Rn         06
INC Rn          08
INC DPTR        A3
JB bitaddr rel  20
JBC bitaddr rel 10
JC rel          40
JMP @A+DPTR     73
JNB bitaddr rel 30
JNC rel         50
JNZ rel         70
JZ rel          60
LCALL addr      12
LJMP addr       02
MOV @Rn #8      76
MOV @Rn A       F6
MOV @Rn iram    A6
MOV A #8        74
MOV A @Rn       E6
MOV A Rn        E8
MOV A iram      E5
MOV C bitaddr   A2
MOV DPTR #16    90
MOV Rn #8       78
MOV Rn A        F8
MOV Rn iram     A8
MOV bitaddr C   92
MOV iram #8     75
MOV iram @Rn    86
MOV iram Rn     88
MOV iram A      F5
MOV iram iram   85
MOVC A @A+DPTR  93
MOVC A @A+PC    83
MOVX @DPTR A    F0
MOVX @Rn A      F2
MOVX A @DPTR    E0
MOVX A @Rn      E2
MUL AB          A4
NOP             00
ORL iram A      42
ORL iram #8     43
ORL A #8        44
ORL A iram      45
ORL A @Rn       46
ORL A Rn        48
ORL C bitaddr   72
ORL C / bitaddr A0
POP iram        D0
PUSH iram       C0
RET             22
RETI            32
RL A            23
RLC A           33
RR A            03
RRC A           13
SETB C          D3
SETB bitaddr    D2
SJMP rel        80
SUBB A #8       94
SUBB A iram     95
SUBB A @Rn      96
SUBB A Rn       98
SWAP A          C4
XCH A @Rn       C6
XCH A Rn        C8
XCH A iram      C5
XCHD A @Rn      D6
XRL iram A      62
XRL iram #8     63
XRL A #8        64
XRL A iram      65
XRL A @Rn       66
XRL A Rn        68
