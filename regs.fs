\ These definitions are for register addresses.
\
\ All registers are in page DFxx, occupy adjacent bits
\ in a byte. So can use a 16-bit number to specify their
\ position.
\ X@ fetches, X! stores.
\
\ +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
\ |        offset         |     | hibit  | lobit  |
\ +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
\

: bits ( addr bits -- addr ) over 8 lshift + constant ;
8 base !
: 0:0 00 bits drop ;
: 1:0 10 bits drop ;
: 1:1 11 bits ;
: 2:0 20 bits drop ;
: 2:1 21 bits ;
: 2:2 22 bits ;
: 3:0 30 bits drop ;
: 3:1 31 bits ;
: 3:2 32 bits ;
: 3:3 33 bits ;
: 4:0 40 bits drop ;
: 4:1 41 bits ;
: 4:2 42 bits ;
: 4:3 43 bits ;
: 4:4 44 bits ;
: 5:0 50 bits drop ;
: 5:1 51 bits ;
: 5:2 52 bits ;
: 5:3 53 bits ;
: 5:4 54 bits ;
: 5:5 55 bits ;
: 6:0 60 bits drop ;
: 6:1 61 bits ;
: 6:2 62 bits ;
: 6:3 63 bits ;
: 6:4 64 bits ;
: 6:5 65 bits ;
: 6:6 66 bits ;
: 7:0 70 bits drop ;
: 7:1 71 bits ;
: 7:2 72 bits ;
: 7:3 73 bits ;
: 7:4 74 bits ;
: 7:5 75 bits ;
: 7:6 76 bits ;
: 7:7 77 bits ;
hex

DF2F                \ IOCFG2 - Radio Test Signal Configuration (P1_7)
    6:6 GDO2_INV    \ Invert output, i.e. select active low (1) / high (0)
    5:0 GDO2_CFG    \ Debug output on P1_7 pin. See Table 73 for a description
                    \ of internal signals which can be output on this pin for
                    \ debug purpose

DF30                \ IOCFG1 - Radio Test Signal Configuration (P1_6)
    7:7 GDO_DS      \
    6:6 GDO1_INV    \
    5:0 GDO1_CFG    \


DF31                \ IOCFG0 - Radio Test Signal Configuration (P1_5)
    6:6 GDO0_INV
    5:0 GDO0_CFG

DF00 7:0 SYNCH      \ Sync word, high byte
DF01 7:0 SYNCL      \ Sync word, low bye
DF02 7:0 PKTLEN     \ Packet length

DF03                \ PKTCTRL1 - Packet Automation Control
    7:5 PQT
    2:2 APPEND_STATUS
    1:0 ADR_CHK

DF04                \ PKTCTRL0 - Packet Automation Control
    6:6 WHITE_DATA
    5:4 PKT_FORMAT
    2:2 CRC_EN
    1:0 LENGTH_CONFIG

DF05                \ ADDR - Device Address
    7:0 DEVICE_ADDR

DF06                \ CHANNR - Channel Number
    7:0 CHAN

DF07                \ FSCTRL1 - Frequency Synthesizer Control
    4:0 FREQ_IF

DF08                \ FSCTRL0 - Frequency Synthesizer Control
    7:0 FREQOFF

DF09                \ FREQ2 - Frequency Control Word, High Byte
    7:0 FREQH

DF0A                \ FREQ1 - Frequency Control Word, Middle Byte
    7:0 FREQM

DF0B                \ FREQ0 - Frequency Control Word, Low Byte
    7:0 FREQL

DF0C                \ MDMCFG4 - Modem configuration
    7:6 CHANBW_E
    5:4 CHANBW_M
    3:0 DRATE_E

DF0D                \ MDMCFG3 - Modem Configuration
    7:0 DRATE_M

DF0E                \ MDMCFG2 - Modem Configuration
    7:7 DEM_DCFILT_OFF
    6:4 MOD_FORMAT
    3:3 MANCHESTER_EN
    2:0 SYNC_MODE

DF0F                \ MDMCG1 - Modem Configuration
    7:7 FEC_EN
    6:4 NUM_PREAMBLE
    1:0 CHANSPC_E

DF10                \ MDMCFG0 - Modem Configuration
    7:0 CHANSPC_M

DF11                \ DEVIATN - Modem Deviation Setting
    6:4 DEVIATION_E
    2:0 DEVIATION_M

DF12                \ MCSM2 - Main Radio Control State Machine Configuration
    4:4 RX_TIME_RSSI
    3:3 RX_TIME_QUAL
    2:0 RX_TIME

DF13                \ MCSM1 - Main Radio Control State Machine Configuration
    5:4 CCA_MODE
    3:2 RXOFF_MODE
    1:0 TXOFF_MODE

DF14                \ MCSM0 - Main Radio Control State Machine Configuration
    5:4 FS_AUTOCAL
    1:0 CLOSE_IN_RX

DF15                \ FOCCFG - Frequency Offset Compensation Configuration
    5:5 FOC_BS_CS_GATE
    4:3 FOC_PRE_K
    2:2 FOC_POST_K
    1:0 FOC_LIMIT

DF16                \ BSCFG - Bit Synchronization Configuration
    7:6 BS_PRE_KI
    5:4 BS_PRE_KP
    3:3 BS_POST_KI
    2:2 BS_POST_KP
    1:0 BS_LIMIT

DF17                \ AGCCTRL2 - AGC Control
    7:6 MAX_DVGA_GAIN
    5:3 MAX_LNA_GAIN
    2:0 MAGN_TARGET

DF18                \ AGCCTRL1 - AGC Control
    6:6 AGC_LNA_PRIORITY
    5:4 CARRIER_SENSE_REL_THR
    3:0 CARRIER_SENSE_ABS_THR

DF19                \ AGCCTRL0 - AGC Control
    7:6 HYST_LEVEL
    5:4 WAIT_TIME
    3:2 AGC_FREEZE
    1:0 FILTER_LENGTH

DF1A                \ FREND1 - Front End RX Configuration
    7:6 LNA_CURRENT
    5:4 LNA2MIX_CURRENT
    3:2 LODIV_BUF_CURRENT_RX
    1:0 MIX_CURRENT

DF1B                \ FREND0 - Front End TX Configuration
    5:4 LODIV_BUF_CURRENT_TX
    2:0 PA_POWER

DF1C                \ FSCAL3 - Frequency Synthesizer Calibration
    7:6 FSCAL3
    5:4 CHP_CURR_CAL_EN
    3:0 FSCAL3L

DF1D                \ FSCAL2 - Frequency Synthesizer Calibration
    5:5 VCO_CORE_H_EN
    4:0 FSCAL2

DF1E                \ FSCAL1 - Frequency Synthesizer Calibration
    5:0 FSCAL1

DF1F                \ FSCAL0 - Frequency Synthesizer Calibration
    6:0 FSCAL0

DF25                \ TEST0 - Various Test Settings
    7:0 TEST0

DF27 constant PA_TABLE

DF3B                \ MARCSTATE - Main Radio Control State Machine State
    4:0 MARCSTATE

DF90                \ port 1
    7:7 P1_7
    6:6 P1_6
    5:5 P1_5
    4:4 P1_4
    3:3 P1_3
    2:2 P1_2
    1:1 P1_1
    0:0 P1_0

DFBA constant ADC

DFBB                \ ADCH
    7:0 ADCH

DFB4                \ ADCCON1
    7:7 EOC
    6:6 ST
    5:4 STSEL
    3:2 RCTRL
    drop

DFB5                \ ADCCON2
    7:6 SREF        \ reference (1.25V, AIN7, AVDD, AIN6-7)
    5:4 SDIV        \ resolution (7, 9, 10, 12)
    3:0 SCH         \ channel select

DFB6                \ ADCCON3
    7:6 EREF        \ reference (1.25V, AIN7, AVDD, AIN6-7)
    5:4 EDIV        \ resolution (7, 9, 10, 12)
    3:0 ECH         \ channel select

DFC6                \ CLKCON
    7:7 OSC32K      \ 32 KHz
    6:6 OSC         \ osc sel (crystal, RC)
    5:3 TICKSPD     \
    2:0 CLKSPD      \

DFBE                \ SLEEP
    6:6 XOSC_STB    \
    5:5 HFRC_STB    \ RC oscillator stable status
    4:3 RST         \ last reset cause (power on, external, watchdog)
    2:2 OSC_PD      \ XOSC and HS RCOSC power down
    1:0 MODE        \ power mode setting

DFA1                \ WORIRQ
    4:4 EVENT0_MASK
    0:0 EVENT0_FLAG

DFA2                \ WORCTRL - Sleep Timer Control
    2:2 WOR_RESET
    1:0 WOR_RES     \ resolution (15:0, 20:5, 25:10, 30:15)

DFA6 constant WOREVT

DFA8                \ IEN0 - Interrupt Enable 0 register
    7:7 EA
    5:5 STIE
    4:4 ENCIE
    3:3 URX1IE
    2:2 URX0IE
    1:1 ADCIE
    0:0 RFTXRXIE

DFB8                \ IEN1 - Interrupt Enable 1 Register
    5:5 P0IE
    4:4 T4IE
    3:3 T3IE
    2:2 T2IE
    1:1 T1IE
    0:0 DMAIE

DFC5                \ USART 0 Generic control
    7:7 CPOL        \ SPI clock polarity
    6:6 CPHA        \ SPI clock phase
    5:5 ORDER       \ bit order (LSB first, MSB first)
    4:0 BAUD_E      \ baud rate exponent

DFE2 constant T1CNT \ Timer 1 Counter (16-bit)

DFE4                \ T1CTL - Timer 1 Control and Status
    7:7 CH2IF       \ Timer 1 channel 2 interrupt flag
    6:6 CH1IF       \ Timer 1 channel 1 interrupt flag
    5:5 CH0IF       \ Timer 1 channel 0 interrupt flag
    4:4 OVFIF       \ Timer 1 counter overflow interrupt flag
    3:2 DIV         \ Prescaler divider (/1, /8, /32, /128)
    1:0 T1MODE      \ Timer 1 mode (off, free, modulo, up/down)

DFD8                \ Timers 1/3/4 Interrupt Mask/Flag
    6:6 OVFIM       \ Timer 1 overflow interrupt mask
    5:5 T4CH1IF
    4:4 T4CH0IF
    3:3 T4OVFIF
    2:2 T3CH1IF
    1:1 T3CH0IF
    0:0 T3OVFIF

DFDA constant T1CC0 \ Timer 1 channel 0 capture/compare

DFC2
    7:0 BAUD_M      \ baud rate mantissa

DFF6                \ P1INP - port 1 input mode
    7:7 MDP1_7      \ input mode (pull up/down, tristate)
    6:6 MDP1_6      \ 
    5:5 MDP1_5      \ 
    4:4 MDP1_4      \ 
    3:3 MDP1_3      \ 
    2:2 MDP1_2      \ 
    drop

DFFE                \ P1DIR - port 1 direction
    7:7 P1D7        \ direction (input, output)
    6:6 P1D6        \ 
    5:5 P1D5        \ 
    4:4 P1D4        \ 
    3:3 P1D3        \ 
    2:2 P1D2        \ 
    1:1 P1D1        \ 
    0:0 P1D0        \ 

\ RFST (0xE1) - RF Strobe Commands
FFE1 constant RFST
00 constant RFST_SFSTXON
01 constant RFST_SCAL
02 constant RFST_SRX
03 constant RFST_STX
04 constant RFST_SIDLE
05 constant RFST_SNOP

: b? ( baddr )      b@ . ;
: b+ ( baddr )      true swap b! ;
: b- ( baddr )      false swap b! ;

: rcosc  0 OSC_PD b! begin HFRC_STB b@ until 1 CLKSPD b! 1 OSC_PD b! ;
: xtosc  0 OSC_PD b! begin XOSC_STB b@ until 1 CLKSPD b! 1 OSC_PD b! ;

decimal
\ : xt 64 0 do i specmask . loop ;
\ Address Config = No address check
\ Base Frequency = 433.999969
\ CRC Enable = true
\ Carrier Frequency = 433.999969
\ Channel Number = 0
\ Channel Spacing = 199.951172
\ Data Rate = 2.39897
\ Deviation = 5.157471
\ Device Address = 0
\ Manchester Enable = false
\ Modulated = true
\ Modulation Format = GFSK
\ PA Ramping = false
\ Packet Length = 255
\ Packet Length Mode = Variable packet length mode. Packet length configured by the first byte after sync word
\ Preamble Count = 4
\ RX Filter BW = 58.035714
\ Sync Word Qualifier Mode = 30/32 sync word bits detected
\ TX Power = 0
\ Whitening = false
\ Register settings for CC1110
\     - generated by SmartRF Studio
hex
: /434mhz
    05 DF04 c! ( PKTCTRL0 )
    06 DF07 c! ( FSCTRL1 )
    10 DF09 c! ( FREQ2 )
    B1 DF0A c! ( FREQ1 )
    3B DF0B c! ( FREQ0 )
    F6 DF0C c! ( MDMCFG4 )
    83 DF0D c! ( MDMCFG3 )
    13 DF0E c! ( MDMCFG2 )
    15 DF11 c! ( DEVIATN )
    18 DF14 c! ( MCSM0 )
    17 DF15 c! ( FOCCFG )
    E9 DF1C c! ( FSCAL3 )
    2A DF1D c! ( FSCAL2 )
    00 DF1E c! ( FSCAL1 )
    1F DF1F c! ( FSCAL0 )
    81 DF23 c! ( TEST2 )
    35 DF24 c! ( TEST1 )
    09 DF25 c! ( TEST0 )
    60 DF2E c! ( PA_TABLE0 )
;
BA80 18C 2CONSTANT fref  \ 26000000

: emb ( reg_E reg_M bias -- x )
    swap b@ + swap b@ lshift ;

: t2/
    >r
    d2/ 7FFF and r@ 0F lshift or
    r> 2/ ;

: m*s  ( d1 u s -- d2 ) \ d2 is  d1 * s / 2**n
    >r t* r>
    ?dup if 0 do
        t2/
    loop then
    drop
;

decimal

: ΔfCHANNEL \ ( -- d ) channel spacing in Hz
    fref
    CHANSPC_M b@ 256 +
    18 CHANSPC_E b@ - m*s ;

: RDATA     \ ( -- d ) symbol rate in Baud
    fref
    DRATE_M b@ 256 +
    28 DRATE_E b@ - m*s ;

: BWCHANNEL \ ( -- d ) Channel bandwith in Hz
    fref 1
    CHANBW_E b@ CHANBW_M b@ 4 emb 8 * m*/ ;

: fcarrier \ ( -- d ) base frequency of frequency synthesizer
    fREF FREQL b@ 16 m*s
    fREF FREQM b@ 8 m*s d+
    fREF FREQH b@ 0 m*s d+
;

: fIF \ ( -- d ) desired IF frequency to employ in RX
    fRef FREQ_IF b@ 1024 m*/ ;

: ?x
    cr ." ΔfCHANNEL " ΔfCHANNEL d.
    cr ." RDATA     " RDATA d.
    cr ." BWCHANNEL " BWCHANNEL d.
    cr ." fcarrier  " fcarrier d.
    cr ." fIF       " fIF d.
;

hex

F800 constant buf
4041 constant slug

: completion
    begin MARCSTATE b@ 13 <> until ;

decimal
: tx
    /434mhz
    RFST_SIDLE RFST c!
    cr 60 0 do MARCSTATE b@ . loop
    \ 10 0 do
        slug ATX
        cr 600 0 do MARCSTATE b@ . loop
        \ completion
    \ loop
;

: idle begin MARCSTATE b@ 1 = until ;

: say
    bl parse
    idle >radio ;

\ create buf0 128 allot
\ create buf 256 allot
buf 256 0 fill

: rx
    /434mhz
    buf RADIO>
;

: minidump
    dup . [char] [ emit type [char] ] emit cr ;

: rxloop
    /434mhz
    begin
        buf RADIO>
        minidump
    again
;

: echo
    /434mhz
    begin
        buf RADIO>
        2dup minidump
        over 1 swap +!
        idle >radio idle
    again ;

: txrx
    /434mhz
    bl parse
    idle >radio idle
    buf radio> minidump
;

: raw ( -- t ) \ read temperature ADC
    14 ECH b!
    begin
        EOC b@
    until
    ADC @ ;

256 constant OVERSAMPLE
755 32767 1250 */ constant T0       \ 0C as a reading
247 32767 1250 */ constant T+100    \ +100C as a reading

: mean
    0 0
    OVERSAMPLE 0 do
        raw m+
    loop
    OVERSAMPLE T0 m* d-
    1000 T+100 m*/
    OVERSAMPLE um/mod nip ;

: x
    begin
        cr mean .
    again
;

: bench
    50000 0 do
        i 7 mod drop
    loop ;

2variable freq

: dlog ( d. -- m e ) \ such that (16384 <= m < 32768)
    0 >r
    begin
        32767 0 2over d<
    while
        over 1 and >r d2/ r> m+
        r> 1+ >r
    repeat
    drop r>
;

: drshift ( d. x -- d. )
    0 do d2/ loop
;

: boot
    26000 1000 um* freq 2!

    26000 1- T1CC0 !        \ Timer 1 modulus, for 1KHz
    2 T1MODE b!             \ modulo mode
    OVFIM b+                \ enable T1 overflow intr
    T1IE b+                 \ enable T1 intr

    EA b+                   \ all intr on
;

: getbaud
    freq 2@ 256 BAUD_M b@ + 256 m*/
    20 BAUD_E b@ - drshift
    ;

: setbaud ( d. -- m e )
    5 drshift drop
    0 128 rot freq 2@ 10 drshift drop m*/

    \ get into range 256-511
    0 >r
    begin
        511 0 2over d<
    while
        over 1 and >r d2/ r> m+
        r> 1+ >r
    repeat
    drop 256 - r>
    ;

\ : baud ( d -- m e )

: x boot getbaud d. ;
\ : x boot 49664 1 setbaud .s ;

\ : frob p1d0 b+ p1_0 begin dup b- dup b+ again ;

\ WWVB receiver
: /wwvb
    P1D0 b+  P1D1 b+
    P1_0 b+  P1_1 b-
    MDP1_2 b+ ;

: recv p1_2 b@ ;

: ms@ ms@ drop ;

: ticker
    ms@
    begin
        begin recv 0= until
        begin recv until
        ms@ tuck swap - . cr
    again
;

: decode
    begin
        begin recv 0= until
        begin recv until
        ms@ [char] t emit space
        begin recv 0= until
        ms@ swap - . cr
    again
;

: pulse
    dup 100 300 within if [char] 0 emit then
    dup 400 600 within if [char] 1 emit then
        700 900 within if cr            then ;

: watch
    begin
        begin recv 0= until
        begin recv until
        ms@
        begin recv 0= until
        ms@ swap - pulse
    again
;

0 IDP !
COMMIT
