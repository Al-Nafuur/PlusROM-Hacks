; Disassembly of ~\Documents\Atari\Games\Solar Storm (1983) (Imagic).bin
; Disassembled 06/21/25 09:58:53
; Using Stella 8.0_pre
;
; Original disassembly and clever optimizations by Thomas Jentzsch
; Gently twisted further and extended with PlusROM functions by Wolfgang Stubig
;
; ROM properties name : Solar Storm (1983) (Imagic)
; ROM properties MD5  : 97842fe847e8eb71263d6f92f7e122bd
; Bankswitch type     : 4K* (4K)
;
; Legend: *  = CODE not yet run (tentative code)
;         D  = DATA directive (referenced in some way)
;         G  = GFX directive, shown as '#' (stored in player, missile, ball)
;         P  = PGFX directive, shown as '*' (stored in playfield)
;         C  = COL directive, shown as color constants (stored in player color)
;         CP = PCOL directive, shown as color constants (stored in playfield color)
;         CB = BCOL directive, shown as color constants (stored in background color)
;         A  = AUD directive (stored in audio registers)
;         i  = indexed accessed only
;         c  = used by code executed in RAM
;         s  = used by stack
;         !  = page crossed, 1 cycle penalty

    processor 6502


;===============================================================================
; A S S E M B L E R - S W I T C H E S
;===============================================================================

ORIGINAL    = 0
PAL         = 1

  IF ORIGINAL
OPTIMIZE    = 0
FILL_OPT    = 0
PLUSROM     = 0
  ELSE ;}
OPTIMIZE    = 1
FILL_OPT    = 0
PLUSROM     = 1
  ENDIF


;-----------------------------------------------------------
;      Color constants
;-----------------------------------------------------------
BLACK            = $00
  IF PAL
YELLOW           = $20
GREEN_BEIGE      = $30
BROWN            = $40
ORANGE           = $40
BEIGE            = $40
GREEN            = $50
RED              = $60
CYAN_GREEN       = $70
MAUVE            = $80
CYAN             = $90
VIOLET           = $c0
BLUE             = $b0
PURPLE           = $b0
BLUE_CYAN        = $d0
  ELSE
YELLOW           = $10
BROWN            = $20
ORANGE           = $30
RED              = $40
MAUVE            = $50
VIOLET           = $60
PURPLE           = $70
BLUE             = $80
BLUE_CYAN        = $90
CYAN             = $a0
CYAN_GREEN       = $b0
GREEN            = $c0
GREEN_BEIGE      = $e0
BEIGE            = $f0
  ENDIF


;-----------------------------------------------------------
;      TIA and IO constants accessed
;-----------------------------------------------------------

CXPPMM          = $07  ; (R)
INPT0           = $08  ; (R)
;INPT1          = $09  ; (Ri)

VSYNC           = $00  ; (W)
VBLANK          = $01  ; (W)
WSYNC           = $02  ; (W)
NUSIZ0          = $04  ; (W)
NUSIZ1          = $05  ; (W)
COLUP0          = $06  ; (W)
COLUP1          = $07  ; (W)
COLUPF          = $08  ; (W)
COLUBK          = $09  ; (W)
CTRLPF          = $0a  ; (W)
REFP0           = $0b  ; (W)
REFP1           = $0c  ; (W)
PF0             = $0d  ; (W)
RESP0           = $10  ; (W)
RESP1           = $11  ; (W)
RESM0           = $12  ; (W)
RESM1           = $13  ; (W)
RESBL           = $14  ; (W)
AUDC0           = $15  ; (W)
AUDC1           = $16  ; (W)
AUDF0           = $17  ; (W)
AUDF1           = $18  ; (W)
AUDV0           = $19  ; (W)
AUDV1           = $1a  ; (W)
GRP0            = $1b  ; (W)
GRP1            = $1c  ; (W)
ENAM0           = $1d  ; (W)
ENAM1           = $1e  ; (W)
ENABL           = $1f  ; (W)
HMP0            = $20  ; (W)
HMP1            = $21  ; (W)
HMM0            = $22  ; (W)
HMM1            = $23  ; (W)
HMBL            = $24  ; (W)
VDELP0          = $25  ; (W)
VDELP1          = $26  ; (W)
HMOVE           = $2a  ; (W)
CXCLR           = $2c  ; (W)

SWCHA           = $0280
SWCHB           = $0282
TIMINT          = $0285
TIM64T          = $0296


;-----------------------------------------------------------
;      RIOT RAM (zero-page) labels
;-----------------------------------------------------------

ram_80          = $80
ram_81          = $81
ram_82          = $82
ram_83          = $83
ram_84          = $84
ram_85          = $85
;                 $86  (i)
ram_87          = $87
;                 $88  (i)
ram_89          = $89
;                 $8a  (i)
ram_8B          = $8b
;                 $8c  (i)
ram_8D          = $8d
;                 $8e  (i)
ram_8F          = $8f
ram_90          = $90
ram_91          = $91
ram_92          = $92
player          = $93
ram_94          = $94
ram_95          = $95
ram_96          = $96
ram_97          = $97

ram_99          = $99
ram_9A          = $9a
ram_9B          = $9b
ram_9C          = $9c
ram_9D          = $9d
ram_9E          = $9e
ram_9F          = $9f
ram_A0          = $a0

ram_A2          = $a2
ram_A3          = $a3
ram_A4          = $a4
ram_A5          = $a5
;                 $a6  (i)
;                 $a7  (i)
ram_A8          = $a8
ram_A9          = $a9
ram_AA          = $aa
ram_AB          = $ab
ram_AC          = $ac
ram_AD          = $ad; (s)
ram_AE          = $ae
ram_AF          = $af
ram_B0          = $b0
ram_B1          = $b1
ram_B2          = $b2; (s)
ram_B3          = $b3
ram_B4          = $b4
ram_B5          = $b5
ram_B6          = $b6
;                 $b7  (i)
ram_B8          = $b8
ram_B9          = $b9
;                 $ba  (i)
;                 $bb  (i)
ram_BC          = $bc
ram_BD          = $bd
ram_BE          = $be
ram_BF          = $bf
ram_C0          = $c0
ram_C1          = $c1
ram_C2          = $c2
ram_C3          = $c3
ram_C4          = $c4
ram_C5          = $c5
ram_C6          = $c6
ram_C7          = $c7
ram_C8          = $c8
ram_C9          = $c9
ram_CA          = $ca
ram_CB          = $cb
ram_CC          = $cc
ram_CD          = $cd
ram_CE          = $ce
ram_CF          = $cf
ram_D0          = $d0
ram_D1          = $d1
ram_D2          = $d2
ram_D3          = $d3
;                 $d4  (i)
ram_D5          = $d5
ram_D6          = $d6
ram_D7          = $d7
ram_D8          = $d8
ram_D9          = $d9
ram_DA          = $da
ram_DB          = $db
ram_DC          = $dc
ram_DD          = $dd
ram_DE          = $de
ram_DF          = $df
ram_E0          = $e0
ram_E1          = $e1
ram_E2          = $e2
ram_E3          = $e3
ram_E4          = $e4
ram_E5          = $e5
ram_E6          = $e6
ram_E7          = $e7
ram_E8          = $e8
ram_E9          = $e9
ram_EA          = $ea
ram_EB          = $eb
ram_EC          = $ec
ram_ED          = $ed
ram_EE          = $ee
ram_EF          = $ef
ram_F0          = $f0
ram_F1          = $f1

;                 $fc  (s)
;                 $fd  (s)
;                 $fe  (s)
;                 $ff  (s)


;===============================================================================
; M A C R O S
;===============================================================================

_OPTIMIZED SET 0

  MAC FILL_NOP
    IF FILL_OPT
      ds {1}, $ea
;      REPEAT {1}
;         NOP
;      REPEND
    ENDIF
_OPTIMIZED SET _OPTIMIZED + {1}
  ENDM

  MAC _CHECKPAGE ; internal, do not use directly
    IF >{1} != >{2}
      ECHO ""
     IF {3} != ""
      ECHO "ERROR: different pages! (", {3}, "=", {2}, ",", {1}, ")"
     ELSE
      ECHO "ERROR: different pages! (", {2}, ",", {1}, ")"
     ENDIF
      ECHO ""
      ERR
    ENDIF
  ENDM

  MAC CHECKPAGE_LBL
    _CHECKPAGE ., {1}, {2}
  ENDM

  MAC CHECKPAGE
    CHECKPAGE_LBL {1}, ""
  ENDM

  MAC CHECKPAGE_DATA_LBL
_ADDR SET . - 1 ; hack to convince DASM
    _CHECKPAGE _ADDR, {1}, {2}
  ENDM

  MAC CHECKPAGE_DATA
    CHECKPAGE_DATA_LBL {1}, ""
  ENDM


;-----------------------------------------------------------
;      User Defined Labels
;-----------------------------------------------------------

Start           = $1000

  IF PLUSROM == 1
WriteToBuffer           = $1FF0
WriteSendBuffer         = $1FF1
ReceiveBuffer           = $1FF2
ReceiveBufferSize       = $1FF3

HIGHSCORE_ID            = 87         ; Solar Storm game ID in Highscore DB
  ENDIF


;***********************************************************
;      Bank 0
;***********************************************************

    SEG     CODE
    ORG     $1000

Start
  IF !OPTIMIZE ;{
    sei                             ;2
  ELSE ;}
    FILL_NOP 1
  ENDIF
    cld                             ;2
    ldx     #$ff                    ;2
    txs                             ;2
    inx                             ;2
    txa                             ;2   =  12
L1007
    sta     VSYNC,x                 ;4
    inx                             ;2
    bne     L1007                   ;2/3
    jsr     L181e                   ;6
    lda     #$01                    ;2
    sta     ram_94                  ;3
    sta     ram_E5                  ;3
    lda     #$fe                    ;2
    sta     ram_82                  ;3
    lda     #$c0                    ;2
    sta     ram_9F                  ;3   =  32
L101d
    lda     #$82                    ;2
    sta     VBLANK                  ;3
    sta     WSYNC                   ;3   =   8
;---------------------------------------
    sta     VSYNC                   ;3
    sta     WSYNC                   ;3   =   6
;---------------------------------------
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     WSYNC                   ;3   =   3
;---------------------------------------
  IF !OPTIMIZE ;{
    lda     #$00                    ;2
  ELSE ;}
    lsr
    FILL_NOP 1
  ENDIF
    sta     VSYNC                   ;3
  IF PAL
    ldx     #$4c                    ;2
  ELSE
    ldx     #$2d                    ;2
  ENDIF
    stx     TIM64T                  ;4
    sta     ram_C4                  ;3
    dec     ram_96                  ;5
    bpl     L103c                   ;2/3
    inc     ram_96                  ;5   =  26
L103c
    lda     SWCHB                   ;4
    lsr                             ;2
    bcc     .reset                  ;2/3
    lsr                             ;2
    bcs     L107c                   ;2/3
    bcc     .select                 ;2/3 =  14

.reset
    jsr     L181e                   ;6
    lda     #$b0                    ;2
    sta     ram_9F                  ;3
    lda     #$01                    ;2
    sta     ram_82                  ;3
    bne     L107c                   ;2/3 =  18

.select
    lda     ram_96                  ;3
    bne     L107c                   ;2/3
  IF OPTIMIZE
    FILL_NOP 2
    sta     ram_82                  ;3
  ENDIF
    lda     #$1f                    ;2
    sta     ram_96                  ;3
    lda     ram_97                  ;3
    eor     #$01                    ;2
    sta     ram_97                  ;3
  IF !OPTIMIZE ;{
    lda     #$00                    ;2
    sta     ram_82                  ;3
  ENDIF ;}
    ldx     #$0a                    ;2
    lda     #<BlankGfx              ;2   =  27
L106a
    sta     ram_83,x                ;4
    dex                             ;2
    bpl     L106a                   ;2/3
    lda     ram_97                  ;3
    clc                             ;2
    adc     #$01                    ;2
    jsr     Mult10                  ;6
    sta     ram_87                  ;3
    jsr     L181e                   ;6   =  30
L107c
    ldx     #$0a                    ;2   =   2
L107e
  IF !OPTIMIZE ;{
    lda     #>DigitGfx              ;2
    sta     ram_84,x                ;4
    lda     #>FireGfx               ;2
    sta     ram_D2,x                ;4
  ELSE ;}
    ldy     #>DigitGfx              ;2          $1c
    sty     ram_84,x                ;4
    iny                             ;           $1d
    sty     ram_D2,x                ;4
    FILL_NOP 1
  ENDIF
    dex                             ;2
    dex                             ;2
    bpl     L107e                   ;2/3
    ldy     #$00                    ;2
    ldx     player                  ;3
    lda     ram_82                  ;3
    beq     L10d6                   ;2/3
    cmp     #$ff                    ;2
    bne     L10a2                   ;2/3
    lda     ram_97                  ;3
    beq     L10a2                   ;2/3
    lda     ram_80                  ;3         *
    bpl     L10a2                   ;2/3       *
    txa                             ;2         *
    eor     #$01                    ;2         *
    tax                             ;2   =  48 *
L10a2
    lda     L1ce4,x                 ;4
    sta     COLUP0                  ;3
    sta     COLUP1                  ;3   =  10
L10a9
    lda     ram_8F,x                ;4
    lsr                             ;2
    lsr                             ;2
    lsr                             ;2
    lsr                             ;2
    jsr     Mult10                  ;6
    sta.wy  ram_85,y                ;5
    lda     ram_8F,x                ;4
    and     #$0f                    ;2
    jsr     Mult10                  ;6
    sta.wy  ram_83,y                ;5
    iny                             ;2
    iny                             ;2
    iny                             ;2
    iny                             ;2
    inx                             ;2
    inx                             ;2
    cpx     #$04                    ;2
    bcc     L10a9                   ;2/3
    lda     #<BlankGfx              ;2
    sta     ram_8B                  ;3
    ldx     player                  ;3
    lda     ram_CF,x                ;4
    jsr     Mult10                  ;6
    sta     ram_8D                  ;3   =  77
L10d6
    lda     ram_82                  ;3
    cmp     #$fe                    ;2
    bne     L10ed                   ;2/3
    ldx     #$0a                    ;2
    lda     #<Copyright             ;2   =  11
L10e0
    ldy     #>Copyright             ;2
    sty     ram_84,x                ;4
    sta     ram_83,x                ;4
    clc                             ;2
    adc     #$0a                    ;2
    dex                             ;2
    dex                             ;2
    bpl     L10e0                   ;2/3 =  20
L10ed
    lda     ram_E5                  ;3
    beq     L10f4                   ;2/3
    jmp     L18a1                   ;3   =   8

L10f4
    ldx     player                  ;3
    lda     ram_82                  ;3
    bmi     L112c                   ;2/3!
    cmp     #$02                    ;2
    bne     L1116                   ;2/3!
    lda     ram_E1                  ;3
    cmp     #$2c                    ;2
    beq     L1116                   ;2/3
    cmp     #$1c                    ;2
    beq     L1116                   ;2/3
    lda     #$03                    ;2
    sta     ram_CC                  ;3
    and     ram_80                  ;3
    bne     L1148                   ;2/3
    dec     ram_E1                  ;5
    dec     ram_9F                  ;5
    bne     L1148                   ;2/3 =  45
L1116
    lda     ram_9F                  ;3
    cmp     #$c0                    ;2
    beq     L1148                   ;2/3
    cmp     #$e0                    ;2
    bcc     L1134                   ;2/3
    lda     ram_80                  ;3
    and     #$07                    ;2
    bne     L1148                   ;2/3
    lda     #$f0                    ;2
    cmp     ram_9F                  ;3
    bne     L1146                   ;2/3 =  25
L112c
    lda     #$3c                    ;2
    sta     ram_E1                  ;3
    lda     #$c0                    ;2
    bne     L1146                   ;2/3 =   9

L1134
    lda     L1ce6,x                 ;4
    sta     ram_E1                  ;3
    lda     #$00                    ;2
    sta     ram_CC                  ;3
    lda     ram_80                  ;3
    lsr                             ;2
    and     #$01                    ;2
    tax                             ;2
    lda     L1db8,x                 ;4   =  25
L1146
    sta     ram_9F                  ;3   =   3
L1148
  IF !OPTIMIZE ;{
    lda     #>EnemyGfx              ;2
    sta     ram_A0                  ;3
    lda     #>ColorData             ;2
    sta     ram_E2                  ;3
    lda     #>EnemyGfx              ;2
    sta     ram_BF                  ;3
  ELSE ;}
    ldy     #>EnemyGfx              ;2
    sty     ram_A0                  ;3
    sty     ram_BF                  ;3
    iny
    sty     ram_E2                  ;3
    FILL_NOP 3
  ENDIF
    ldy     #$04                    ;2   =  17
L1156
    lda.wy  ram_B3,y                ;4
    beq     L1187                   ;2/3
    cmp     #$e0                    ;2
    bcc     L1173                   ;2/3
    lda     ram_80                  ;3
    and     #$07                    ;2
    bne     L1187                   ;2/3
    lda     #$f0                    ;2
    cmp.wy  ram_B3,y                ;4
    bne     L1184                   ;2/3
    lda     #$f0                    ;2
    sta.wy  ram_A4,y                ;5
    bne     L1187                   ;2/3 =  34

L1173
    jsr     L1bfa                   ;6
    lda     ram_80                  ;3
  IF !OPTIMIZE ;{
    lsr                             ;2
    lsr                             ;2
    and     #$01                    ;2
    clc                             ;2
  ELSE ;}
    and     #$04                    ;2
    lsr                             ;2
    lsr                             ;2
    FILL_NOP 1
  ENDIF
    adc     L1da0,x                 ;4
    tax                             ;2
    lda     L1db8,x                 ;4   =  27
L1184
    sta.wy  ram_B3,y                ;5   =   5
L1187
    dey                             ;2
    bpl     L1156                   ;2/3
  IF !OPTIMIZE ;{
    ldy     #$00                    ;2   =   6
  ELSE ;}
    iny
    FILL_NOP 1
  ENDIF
L118c
    lda.wy  ram_A4,y                ;4
    cmp     #$f0                    ;2
    bne     L11b4                   ;2/3
    tya                             ;2   =  10
L1194
    ldx     ram_A5,y                ;4
    stx     ram_A4,y                ;4
    ldx     ram_AA,y                ;4
    stx     ram_A9,y                ;4
    ldx     ram_B4,y                ;4
    stx     ram_B3,y                ;4
    ldx     ram_B9,y                ;4
    stx     ram_B8,y                ;4
    iny                             ;2
    cpy     #$04                    ;2
    bne     L1194                   ;2/3
    ldx     #$f0                    ;2
    stx     ram_A8                  ;3
    ldx     #$00                    ;2
    stx     ram_BC                  ;3
    stx     ram_AD                  ;3
    tay                             ;2   =  53
L11b4
    iny                             ;2
    cpy     #$04                    ;2
    bcc     L118c                   ;2/3
    lda     ram_DF                  ;3
    bne     L11bf                   ;2/3
    lda     ram_A2                  ;3   =  14
L11bf
  IF !OPTIMIZE ;{
    clc                             ;2
    adc     #$04                    ;2
  ELSE ;}
    FILL_NOP 1
    adc     #$04-1                  ;2
  ENDIF
    jsr     L1881                   ;6
    sta     HMM0                    ;3
    sta     WSYNC                   ;3   =  16
;---------------------------------------
    jsr     Wait12                  ;6
    and     #$0f                    ;2
    tay                             ;2   =  10
L11cf
    dey                             ;2
    bpl     L11cf                   ;2/3
    sta     RESM0                   ;3
    sta     WSYNC                   ;3   =  10
;---------------------------------------
    sta     HMOVE                   ;3
    sta     WSYNC                   ;3   =   6
;---------------------------------------
  IF !OPTIMIZE ;{
    lda     #$00                    ;2
    sta     HMM0                    ;3
  ELSE ;}
    iny
    sty     HMM0                    ;3
    FILL_NOP 1
  ENDIF
    lda     ram_A2                  ;3
    jsr     L1881                   ;6
    sta     ram_A3                  ;3
    ldx     #$04                    ;2   =  19
L11e7
    lda     ram_A9,x                ;4
    jsr     L1881                   ;6
    sta     ram_AE,x                ;4
    dex                             ;2
    bpl     L11e7                   ;2/3
    ldy     #$04                    ;2   =  20
L11f3
    ldx     ram_A4,y                ;4
    cpx     #$f0                    ;2
    bne     L11fc                   ;2/3
    dey                             ;2
    bpl     L11f3                   ;2/3 =  12
L11fc
    sty     ram_BD                  ;3
    lda     ram_82                  ;3
    cmp     #$03                    ;2
    bne     L120c                   ;2/3
    ldx     #$42                 ;2
    lda     ram_80                  ;3
  IF !OPTIMIZE ;{
    and     #$01                    ;2
    beq     L1216                   ;2/3 =  19
  ELSE ;}
    lsr                             ;2
    FILL_NOP 1
    bcc     L1216                   ;2/3 =  19
  ENDIF
L120c
    ldx     ram_EC                  ;3
    bne     L1216                   ;2/3
    ldx     ram_C7                  ;3
    beq     L1216                   ;2/3
    ldx     #YELLOW|$0              ;2   =  12
L1216
    stx     COLUBK                  ;3
    inc     ram_80                  ;5
    bne     L121e                   ;2/3
    inc     ram_81                  ;5   =  15
L121e
    bit     TIMINT                  ;4
    bpl     L121e                   ;2/3
    sta     WSYNC                   ;3   =   9
;---------------------------------------
  IF !OPTIMIZE ;{
    lda     #$00                    ;2
  ELSE ;}
    lda     #$01                    ;2
  ENDIF
    sta     VBLANK                  ;3
    sta     PF0                     ;3
    sta     ENAM0                   ;3
    sta     WSYNC                   ;3   =  14
;---------------------------------------
    sta     REFP0                   ;3
    sta     REFP1                   ;3
  IF !OPTIMIZE ;{
    lda     #$01                    ;2
  ELSE ;}
    FILL_NOP 2
  ENDIF
    sta     CTRLPF                  ;3
    lda     #$03                    ;2
    sta     NUSIZ0                  ;3
    sta     NUSIZ1                  ;3
  IF !OPTIMIZE ;{
    ldy     #$06                    ;2
  ENDIF
    sta     WSYNC                   ;3   =  24
;---------------------------------------
  IF OPTIMIZE
    ldy     #$06                    ;2
  ENDIF
L1241
    dey                             ;2
    bpl     L1241                   ;2/3
  IF !OPTIMIZE ;{
    nop                             ;2
    sta     RESP0                   ;3
    sta     RESP1                   ;3
    lda     #$e0                    ;2
    sta     HMP0                    ;3
    lda     #$f0                    ;2
    sta     HMP1                    ;3
    lda     #$01                    ;2
    sta     VDELP0                  ;3
    sta     VDELP1                  ;3
  ELSE ;}
    sta     RESP0                   ;3
    sta     RESP1                   ;3
    lda     #$f0                    ;2
    sta     HMP1                    ;3
    asl
    sta     HMP0                    ;3
    FILL_NOP 4
    sty     VDELP0                  ;3
    sty     VDELP1                  ;3          JTZ: this must be set not too early!
  ENDIF
    sta     WSYNC                   ;3   =  33
;---------------------------------------
    sta     HMOVE                   ;3
    lda     #$09                    ;2
    sta     ram_9B                  ;3   =   8
L125f
    ldy     ram_9B                  ;3
    lda     (ram_8D),y              ;5
    sta     GRP0                    ;3
    sta     WSYNC                   ;3   =  14
;---------------------------------------
    lda     (ram_8B),y              ;5
    sta     GRP1                    ;3
    lda     (ram_89),y              ;5
    sta     GRP0                    ;3
    lda     (ram_87),y              ;5
    sta     ram_9C                  ;3
    lda     (ram_85),y              ;5
    tax                             ;2
    lda     (ram_83),y              ;5
    tay                             ;2
    lda     ram_9C                  ;3
    sta     GRP1                    ;3
    stx     GRP0                    ;3
    sty     GRP1                    ;3
    sty     GRP0                    ;3
    dec     ram_9B                  ;5
    bpl     L125f                   ;2/3
    sta     WSYNC                   ;3   =  63
;---------------------------------------
    lda     #$00                    ;2
    sta     GRP0                    ;3
    sta     GRP1                    ;3
    sta     GRP0                    ;3
  IF !OPTIMIZE ;{
    sta     GRP1                    ;3
  ELSE ;}
    FILL_NOP 2
  ENDIF
    sta     VDELP0                  ;3
    sta     VDELP1                  ;3
    sta     NUSIZ0                  ;3
    sta     NUSIZ1                  ;3
  IF !OPTIMIZE ;{
    lda     ram_E5                  ;3
  ELSE ;}
    ldy     ram_E5                  ;3
  ENDIF
    beq     L12a2                   ;2/3
    jmp     L1aab                   ;3   =  34

L12a2
    lda     ram_A3                  ;3
    sta     HMP0                    ;3
    sta     WSYNC                   ;3   =   9
;---------------------------------------
    jsr     Wait12                  ;6
    and     #$0f                    ;2
    tay                             ;2   =  10
L12ae
    dey                             ;2
    bpl     L12ae                   ;2/3
    CHECKPAGE_LBL L12ae, "L12ae"
    sta     RESP0                   ;3
    sta     WSYNC                   ;3   =  10
;---------------------------------------
    lda     #BLACK|$e               ;2
    sta     COLUP0                  ;3
    ldy     ram_BD                  ;3
    lda.wy  ram_B3,y                ;4
    sta     ram_BE                  ;3
    lda.wy  ram_AE,y                ;4
    sta     HMP1                    ;3
    sta     WSYNC                   ;3   =  25
;---------------------------------------
    jsr     Wait12                  ;6
    and     #$0f                    ;2
    tay                             ;2   =  10
L12cd
    dey                             ;2
    bpl     L12cd                   ;2/3
    CHECKPAGE_LBL L12cd, "L12cd"
    sta     RESP1                   ;3
    sta     WSYNC                   ;3   =  10
;---------------------------------------
    sta     HMOVE                   ;3
    ldx     #$a0                    ;2
    cpx     ram_C7                  ;3
    bcs     L12e0                   ;2/3
    lda     #$02                    ;2
    sta     ENAM0                   ;3   =  15
L12e0
    sta     WSYNC                   ;3   =   3
;---------------------------------------
  IF !OPTIMIZE ;{
    lda     #$00                    ;2
  ELSE ;}
    FILL_NOP 2
  ENDIF
    sta     HMP0                    ;3
    lda     ram_C1                  ;3
    and     #$07                    ;2
    tay                             ;2
    lda     L1d98,y                 ;4
    sta     ram_E3                  ;3
    lda     #$44                 ;2
    sta     COLUP1                  ;3
    lda     ram_BD                  ;3
    bpl     L12fb                   ;2/3
    jmp     L1399                   ;3   =  32

L12fb
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    ldy     ram_BD                  ;3
    txa                             ;2
    sec                             ;2
    sbc.wy  ram_A4,y                ;4
    tay                             ;2
    beq     L1357                   ;2/3
    and     #$f0                    ;2
    bne     L1313                   ;2/3
    lda     (ram_BE),y              ;5
    sta     GRP1                    ;3
    lda     (ram_E3),y              ;5
    sta     COLUP1                  ;3   =  35
L1313
    cpx     ram_9E                  ;3
    bcs     L131b                   ;2/3
    lda     #$40                    ;2
    sta     PF0                     ;3   =  10
L131b
    ldy     player                  ;3
    lda.wy  INPT0,y                 ;4
    bmi     L1324                   ;2/3
    stx     ram_C4                  ;3   =  12
L1324
    dex                             ;2
    cpx     #$0f                    ;2
    bne     L12fb                   ;2/3!
    lda     ram_DF                  ;3
    bne     L1331                   ;2/3
  IF !OPTIMIZE ;{
    lda     #$00                    ;2
  ELSE ;}
    FILL_NOP 2
  ENDIF
    sta     ENAM0                   ;3   =  16
L1331
    stx     WSYNC                   ;3   =   3
;---------------------------------------
    txa                             ;2
    tay                             ;2
    lda     (ram_9F),y              ;5
    sta     GRP0                    ;3
    lda     (ram_E1),y              ;5
    sta     COLUP0                  ;3
    ldy     ram_BD                  ;3
    txa                             ;2
    sec                             ;2
    sbc.wy  ram_A4,y                ;4
    tay                             ;2
    and     #$f0                    ;2
    bne     L1351                   ;2/3
    lda     (ram_BE),y              ;5
    sta     GRP1                    ;3
    lda     (ram_E3),y              ;5
    sta     COLUP1                  ;3   =  53
L1351
    dex                             ;2
    bpl     L1331                   ;2/3
  IF !OPTIMIZE ;{
    jmp     L13d1                   ;3   =   7
  ELSE ;}
    bmi     L13d1                   ;3   =   7
    FILL_NOP 1
  ENDIF
L1357
    dex                             ;2
    cpx     #$0f                    ;2
    beq     L13b9                   ;2/3
    dec     ram_BD                  ;5
    ldy     ram_BD                  ;3
    bmi     L1399                   ;2/3
    lda.wy  ram_B3,y                ;4
    sta     ram_BE                  ;3
    lda.wy  ram_AE,y                ;4
    sta     HMP1                    ;3
    dex                             ;2
    stx     ram_9C                  ;3
    ldx     player                  ;3
    sta     WSYNC                   ;3   =  41
;---------------------------------------
    and     #$0f                    ;2
    tay                             ;2
    lda     INPT0,x                 ;4
    bmi     L1395                   ;2/3
    lda     ram_9C                  ;3
    sta     ram_C4                  ;3   =  16
L137e
    dey                             ;2
    bpl     L137e                   ;2/3
    CHECKPAGE_LBL L137e, "L137e"
    sta     RESP1                   ;3
    sta     WSYNC                   ;3   =  10
;---------------------------------------
    sta     HMOVE                   ;3
    ldx     ram_9C                  ;3
    dex                             ;2
    cpx     ram_C7                  ;3
    bcs     L1392                   ;2/3
    lda     #$02                    ;2
    sta     ENAM0                   ;3   =  18
L1392
    jmp     L12fb                   ;3   =   3

L1395
    nop                             ;2
  IF !OPTIMIZE ;{
    jmp     L137e                   ;3   =   5
  ELSE ;}
    bmi     L137e                   ;3   =   5
    FILL_NOP 1
  ENDIF

L1399
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    cpx     ram_9E                  ;3
    bcs     L13a3                   ;2/3
    lda     #$40                    ;2
    sta     PF0                     ;3   =  10
L13a3
    ldy     player                  ;3
    lda.wy  INPT0,y                 ;4
    bmi     L13ac                   ;2/3
    stx     ram_C4                  ;3   =  12
L13ac
    cpx     ram_C7                  ;3
    bcs     L13b4                   ;2/3
    lda     #$02                    ;2
    sta     ENAM0                   ;3   =  10
L13b4
    dex                             ;2
    cpx     #$10                    ;2
    bcs     L1399                   ;2/3 =   6
L13b9
    ldy     #$0f                    ;2
    lda     ram_DF                  ;3
    bne     L13c3                   ;2/3
  IF !OPTIMIZE ;{
    lda     #$00                    ;2
  ELSE ;}
    FILL_NOP 2
  ENDIF
    sta     ENAM0                   ;3   =  12
L13c3
    stx     WSYNC                   ;3   =   3
;---------------------------------------
    lda     (ram_9F),y              ;5
    sta     GRP0                    ;3
    lda     (ram_E1),y              ;5
    sta     COLUP0                  ;3
    dey                             ;2
    dex                             ;2
    bpl     L13c3                   ;2/3 =  22
L13d1
    sta     WSYNC                   ;3   =   3
;---------------------------------------
  IF !OPTIMIZE ;{
    lda     #$00                    ;2
    sta     GRP0                    ;3
    sta     GRP1                    ;3
  ELSE ;}
    inx
    FILL_NOP 1
    stx     GRP0                    ;3
    stx     GRP1                    ;3
  ENDIF
    ldx     player                  ;3
    lda     #>FireGfx               ;2
    sta     ram_DE                  ;3
    ldy     #$88                    ;2
    lda     ram_C5,x                ;4
    cmp     #$40                    ;2
    bcc     L13e9                   ;2/3
    ldy     #$80                    ;2   =  28
L13e9
    sty     ram_DD                  ;3
    sta     WSYNC                   ;3   =   6
;---------------------------------------
    lda     #RED|$8                 ;2
    sta     COLUP0                  ;3
    sta     COLUP1                  ;3
    lda     ram_80                  ;3
    and     #$01                    ;2
    tax                             ;2
    lda     L1ce0,x                 ;4
    sta     HMP0                    ;3
    lda     L1ce2,x                 ;4
    nop                             ;2
    sta     HMP1                    ;3
    sta     RESP0                   ;3
    nop                             ;2
    sta     RESP1                   ;3
    sta     WSYNC                   ;3   =  42
;---------------------------------------
    sta     HMOVE                   ;3
    lda     #$06                    ;2
    sta     NUSIZ0                  ;3
    sta     NUSIZ1                  ;3
    ldy     #$07                    ;2
    lda     (ram_DD),y              ;5   =  18
L1416
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     COLUP0                  ;3
    sta     COLUP1                  ;3
    nop                             ;2
    lda     (ram_DB),y              ;5
    sta     GRP0                    ;3
    lda     (ram_D9),y              ;5
    sta     GRP1                    ;3
    lda     (ram_D3),y              ;5
    tax                             ;2
    lda     (ram_D7),y              ;5
    sta     GRP0                    ;3
    lda     (ram_D5),y              ;5
    sta     GRP1                    ;3
    stx     GRP0                    ;3
    lda     (ram_D1),y              ;5
    sta     GRP1                    ;3
    lda     (ram_DD),y              ;5
    dey                             ;2
    bpl     L1416                   ;2/3 =  67
L143b
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     #$00                    ;2
    sta     GRP0                    ;3
    sta     GRP1                    ;3
    sta     PF0                     ;3
    sta     ram_DF                  ;3
    sta     ENAM0                   ;3
    sta     ram_EC                  ;3
    lda     #BROWN|$8                    ;2
    sta     COLUP1                  ;3

  IF PAL
    lda     #$3C                    ;2
  ELSE
    lda     #$20                    ;2
  ENDIF

    sta     TIM64T                  ;4
    lda     ram_E5                  ;3
    beq     L145b                   ;2/3
    jmp     L19e3                   ;3   =  39

L145b
    lda     ram_82                  ;3
    cmp     #$01                    ;2
    bne     L14c1                   ;2/3
    lda     ram_C2                  ;3
    bne     L14c1                   ;2/3
    ldy     #$04                    ;2   =  14
L1467
    lda.wy  ram_A4,y                ;4
    cmp     #$f0                    ;2
    bne     L14c1                   ;2/3
    dey                             ;2
    bpl     L1467                   ;2/3
    ldx     player                  ;3
    lda     ram_CF,x                ;4
    beq     L148c                   ;2/3
    lda     ram_91,x                ;4
    cmp     ram_E8,x                ;4
    bcc     L148c                   ;2/3
    sed                             ;2         *
    lda     ram_E8,x                ;4         *
  IF !OPTIMIZE ;{
    clc                             ;2         *
    adc     #$05                    ;2         *
  ELSE ;}
    FILL_NOP 1
    adc     #$05-1                  ;2         *
  ENDIF
    sta     ram_E8,x                ;4         *
    cld                             ;2         *
    lda     #$01                    ;2         *
    sta     ram_E5                  ;3         *
    bne     L14d5                   ;2/3 =  54 *

L148c
    lda     ram_97                  ;3
    beq     L149f                   ;2/3
    lda     player                  ;3
    eor     #$01                    ;2
    tay                             ;2
    ldx     ram_CF,y                ;4
    beq     L149f                   ;2/3
    sta     player                  ;3
    lda     player                  ;3
    bne     L14ab                   ;2/3 =  26
L149f
    inc     ram_C1                  ;5
    lda     ram_C1                  ;3
    cmp     #$10                    ;2
    bcc     L14ab                   ;2/3
    lda     #$0c                    ;2         *
    sta     ram_C1                  ;3   =  17 *
L14ab
    lda     ram_C1                  ;3
    clc                             ;2
    adc     #$0a                    ;2
    sta     ram_C2                  ;3
    ldx     ram_C1                  ;3
    lda     L1f3c,x                 ;4
    sta     ram_C0                  ;3
    lda     #$02                    ;2
    sta     ram_82                  ;3
    lda     #$bf                    ;2
    sta     ram_CB                  ;3   =  30
L14c1
    lda     ram_82                  ;3
    cmp     #$02                    ;2
    bne     L14d5                   ;2/3
    lda     ram_CB                  ;3
    cmp     #$4f                    ;2
    beq     L14d1                   ;2/3
    dec     ram_CB                  ;5
    bne     L14d5                   ;2/3 =  21
L14d1
    lda     #$01                    ;2
    sta     ram_82                  ;3   =   5
L14d5
    lda     ram_82                  ;3
    bmi     L14e1                   ;2/3
    cmp     #$01                    ;2
    bne     L153c                   ;2/3!
    lda     ram_C2                  ;3
    beq     L153c                   ;2/3!=  14
L14e1
    dec     ram_C3                  ;5
    bne     L153c                   ;2/3!
    dec     ram_C2                  ;5
    ldx     ram_C1                  ;3
    lda     L1c64,x                 ;4
    sta     ram_C3                  ;3
    ldy     #$00                    ;2   =  24
L14f0
    lda.wy  ram_A4,y                ;4
    cmp     #$f0                    ;2
    beq     L14fe                   ;2/3
    iny                             ;2
    cpy     #$05                    ;2
    bne     L14f0                   ;2/3
    beq     L153c                   ;2/3!=  16 *
L14fe
    lda     #$a0                    ;2
    sta.wy  ram_A4,y                ;5
    ldx     #$10                    ;2
    lda     ram_94                  ;3
    and     #$1f                    ;2
    cmp     ram_C1                  ;3
    bcs     L1513                   ;2/3
    lda     #$3c                    ;2
    sta     ram_EA                  ;3
    ldx     #$00                    ;2   =  26
L1513
    stx     ram_B3,y                ;4
    lda     ram_94                  ;3
    cmp     #$8c                    ;2
    bcc     L151c                   ;2/3
    lsr                             ;2   =  13
L151c
    cmp     #$0c                    ;2
    bcs     L1522                   ;2/3
    lda     #$0c                    ;2   =   6
L1522
    sta.wy  ram_A9,y                ;5
    ldx     ram_C1                  ;3
    lda     L1f4c,x                 ;4
    ldx     ram_B3,y                ;4
    bne     L1530                   ;2/3
    lda     #$11                    ;2   =  20
L1530
    bit     ram_94                  ;3
    bpl     L1539                   ;2/3
    eor     #$ff                    ;2
    clc                             ;2
    adc     #$01                    ;2   =  11
L1539
    sta.wy  ram_B8,y                ;5   =   5
L153c
    lda     ram_C0                  ;3
    jsr     L186d                   ;6
    sta     ram_9B                  ;3
    ldy     #$04                    ;2   =  14
L1545
    lda.wy  ram_A4,y                ;4
    cmp     #$f0                    ;2
    beq     L1591                   ;2/3
    lda.wy  ram_A4,y                ;4
    sec                             ;2
    sbc     ram_9B                  ;3
    tax                             ;2
    clc                             ;2
    adc     #$0f                    ;2
    cmp     #$b1                    ;2
    bcc     L156b                   ;2/3
    lda     #$05                    ;2
    ldx     ram_B3,y                ;4
    beq     L1566                   ;2/3
    jsr     L1bfa                   ;6
    lda     L1da8,x                 ;4   =  45
L1566
    jsr     L1fd3                   ;6
    ldx     #$f0                    ;2   =   8
L156b
    stx     ram_A4,y                ;4
    sty     ram_9C                  ;3
    lda.wy  ram_B8,y                ;4
    jsr     L186d                   ;6
    ldy     ram_9C                  ;3
    clc                             ;2
    adc.wy  ram_A9,y                ;4
    sta.wy  ram_A9,y                ;5
    cmp     #$8c                    ;2
    bcs     L1586                   ;2/3
    cmp     #$0c                    ;2
    bcs     L1591                   ;2/3 =  39
L1586
    lda.wy  ram_B8,y                ;4
    eor     #$ff                    ;2
    clc                             ;2
    adc     #$01                    ;2
    sta.wy  ram_B8,y                ;5   =  15
L1591
    dey                             ;2
    bpl     L1545                   ;2/3
    ldy     player                  ;3
    lda     SWCHA                   ;4
    and     L1f00,y                 ;4
    bne     L15cb                   ;2/3
    lda     ram_CA                  ;3
    bne     L15cf                   ;2/3
    lda     ram_82                  ;3
    beq     L15a8                   ;2/3
    bpl     L15b7                   ;2/3 =  29
L15a8
    jsr     L181e                   ;6
    lda     #$01                    ;2
    sta     ram_82                  ;3
    sta     ram_CA                  ;3
    lda     #$b0                    ;2
    sta     ram_9F                  ;3
    bne     L1629                   ;2/3!=  21

L15b7
    lda     ram_9F                  ;3
    cmp     #$c0                    ;2
    beq     L15cb                   ;2/3
    cmp     #$e0                    ;2
    bcs     L15cb                   ;2/3
    lda     #$01                    ;2
    sta     ram_CA                  ;3
    lda     #$0d                    ;2
    sta     ram_CD                  ;3
    bne     L15d2                   ;2/3 =  23

L15cb
    lda     #$00                    ;2
    sta     ram_CA                  ;3   =   5
L15cf
    jmp     L1629                   ;3   =   3

L15d2
    ldy     #$00                    ;2   =   2
L15d4
    lda     ram_A2                  ;3
    clc                             ;2
    adc     #$04                    ;2
    tax                             ;2
    cmp.wy  ram_A9,y                ;4
    bcc     L1620                   ;2/3!
    lda.wy  ram_A9,y                ;4
  IF !OPTIMIZE ;{
    clc                             ;2
    adc     #$0a                    ;2
  ELSE ;}
    FILL_NOP 1
    adc     #$0a-1                  ;2
  ENDIF
    sta     ram_9B                  ;3
    cpx     ram_9B                  ;3
    bcs     L1620                   ;2/3!
    ldx     ram_B3,y                ;4
    bne     L1606                   ;2/3!
    ldx     #$04                    ;2
    lda     #$e0                    ;2   =  41
L15f3
    sta     ram_B3,x                ;4
    dex                             ;2
    bpl     L15f3                   ;2/3
    lda     #$46                    ;2
    sta     ram_C3                  ;3
    sta     ram_EC                  ;3
    lda     #$3f                    ;2
    sta     ram_CE                  ;3
    lda     #$50                    ;2
    bne     L1610                   ;2/3 =  25

L1606
    cpx     #$e0                    ;2
    bcs     L1620                   ;2/3
    jsr     L1bfa                   ;6
    lda     L1db0,x                 ;4   =  14
L1610
    jsr     L1fb6                   ;6
    lda.wy  ram_A4,y                ;4
    ldx     #$3f                    ;2
    stx     ram_CE                  ;3
    ldx     #$e0                    ;2
    stx     ram_B3,y                ;4
    bne     L162b                   ;2/3 =  23

L1620
    iny                             ;2
    cpy     #$05                    ;2
    bne     L15d4                   ;2/3!
    lda     #$a7                    ;2
    bne     L162b                   ;2/3 =  10

L1629
    lda     #$00                    ;2   =   2
L162b
    sta     ram_C7                  ;3
    lda     ram_82                  ;3
    cmp     #$01                    ;2
    bne     L168c                   ;2/3
    lda     ram_C1                  ;3
    lsr                             ;2
    bcs     L168c                   ;2/3
    dec     ram_EB                  ;5
    bne     L168c                   ;2/3
    and     #$07                    ;2
    tay                             ;2
    lda     L1f72,y                 ;4
    sta     ram_EB                  ;3
    lda     ram_C2                  ;3
    beq     L168c                   ;2/3
    ldy     #$04                    ;2   =  42
L164a
    lda.wy  ram_A4,y                ;4
    cmp     #$f0                    ;2
    bne     L1656                   ;2/3
    dey                             ;2
    bpl     L164a                   ;2/3
    bmi     L168c                   ;2/3 =  14

L1656
    lda.wy  ram_B3,y                ;4
    beq     L168c                   ;2/3
    cmp     #$e0                    ;2
    bcs     L168c                   ;2/3
    lda.wy  ram_A4,y                ;4
    sta     ram_C7                  ;3
    lda     #$0d                    ;2
    sta     ram_CD                  ;3
    lda.wy  ram_A9,y                ;4
    sta     ram_DF                  ;3
  IF !OPTIMIZE ;{
    clc                             ;2
  ELSE ;}
    FILL_NOP 1
  ENDIF
    adc     #$04                    ;2
    tax                             ;2
    cmp     ram_A2                  ;3
    bcc     L168c                   ;2/3
    lda     ram_A2                  ;3
  IF !OPTIMIZE ;{
    clc                             ;2
    adc     #$0a                    ;2
  ELSE ;}
    FILL_NOP 1
    adc     #$0a-1                  ;2
  ENDIF
    sta     ram_9B                  ;3
    cpx     ram_9B                  ;3
    bcs     L168c                   ;2/3
    lda     ram_9F                  ;3
    cmp     #$c0                    ;2
    beq     L168c                   ;2/3
    cmp     #$e0                    ;2
  IF !OPTIMIZE ;{
    bcs     L168c                   ;2/3
  ELSE ;}
    FILL_NOP 2
  ENDIF
    bcc     L16c7                   ;2/3 =  68

L168c
    lda     ram_82                  ;3
    cmp     #$01                    ;2
    bne     L16db                   ;2/3
    lda     ram_9F                  ;3
    cmp     #$c0                    ;2
    beq     L16db                   ;2/3
    cmp     #$e0                    ;2
    bcs     L16db                   ;2/3
    ldy     #$04                    ;2   =  20
L169e
    lda.wy  ram_B3,y                ;4
    cmp     #$e0                    ;2
    bcs     L16c2                   ;2/3
    lda.wy  ram_A4,y                ;4
    cmp     #$f0                    ;2
    beq     L16c2                   ;2/3
    cmp     #$0e                    ;2
    bcs     L16c2                   ;2/3
    lda     ram_A2                  ;3
  IF !OPTIMIZE ;{
    sec                             ;2
    sbc     #$07                    ;2
  ELSE ;}
    FILL_NOP 1
    sbc     #$07-1                  ;2
  ENDIF
    cmp.wy  ram_A9,y                ;4
    bcs     L16c2                   ;2/3
  IF !OPTIMIZE ;{
    clc                             ;2
  ELSE ;}
    FILL_NOP 1
  ENDIF
    adc     #$0e                    ;2
    cmp.wy  ram_A9,y                ;4
    bcs     L16c7                   ;2/3 =  43
L16c2
    dey                             ;2
    bpl     L169e                   ;2/3
    bmi     L16db                   ;2/3 =   6

L16c7
    jsr     L1fc6                   ;6
    lda     #$00                    ;2
    sta     ram_C2                  ;3
    ldx     player                  ;3
    lda     ram_CF,x                ;4
    beq     L16db                   ;2/3
    dec     ram_CF,x                ;6
    bne     L16db                   ;2/3
  IF PLUSROM
    jsr     SendPlusROMScore        ;6   =  34
  ELSE
    jsr     L1fe2                   ;6   =  34
  ENDIF

L16db
    lda     ram_82                  ;3
    bmi     L16fe                   ;2/3
    lda     ram_9F                  ;3
    cmp     #$e0                    ;2
    bcs     L16fe                   ;2/3
    lda     ram_C4                  ;3
  IF !OPTIMIZE ;{
    sec                             ;2
    sbc     #$10                    ;2
  ELSE ;}
    FILL_NOP 1
    sbc     #$10-1                  ;2
  ENDIF
    cmp     #$09                    ;2
    bcs     L16f2                   ;2/3
    lda     #$09                    ;2
    bne     L16f8                   ;2/3 =  27

L16f2
    cmp     #$91                    ;2
    bcc     L16f8                   ;2/3
  IF !OPTIMIZE ;{
    lda     #$90                    ;2   =   6
L16f8
    clc                             ;2
  ELSE ;}
    lda     #$90-1                  ;2   =   6
L16f8
    FILL_NOP 1
  ENDIF
    adc     ram_A2                  ;3
    ror                             ;2
    sta     ram_A2                  ;3   =  10
L16fe
    lda     ram_82                  ;3
    cmp     #$03                    ;2
    bne     L1726                   ;2/3
    dec     ram_CB                  ;5
    bne     L1726                   ;2/3
    lda     ram_97                  ;3
    beq     L1716                   ;2/3
    ldx     #$01                    ;2         *
    lda     ram_CF                  ;3         *
    bne     L1724                   ;2/3       *
    lda     ram_D0                  ;3         *
    bne     L1724                   ;2/3 =  31 *
L1716
    lda     #$05                    ;2
    sta     ram_C1                  ;3
    lda     L1f41                   ;4
    sta     ram_C0                  ;3
    ldx     #$00                    ;2
    stx     player                  ;3
    dex                             ;2   =  19
L1724
    stx     ram_82                  ;3   =   3
L1726
    lda     ram_80                  ;3
    and     #$1c                    ;2
    lsr                             ;2
    lsr                             ;2
    tax                             ;2
    ldy     L1ce8,x                 ;4
    sty     ram_D7                  ;3
    sty     ram_D5                  ;3
    iny                             ;2
    iny                             ;2
    sty     ram_D9                  ;3
    sty     ram_D3                  ;3
    iny                             ;2
    iny                             ;2
    sty     ram_DB                  ;3
    sty     ram_D1                  ;3
    jsr     L1fa2                   ;6
    tay                             ;2
    cpy     #$06                    ;2
    bcc     L174c                   ;2/3
    lda     ram_94                  ;3
    bne     L174f                   ;2/3 =  58
L174c
    lda     L1f5c,y                 ;4   =   4
L174f
    sta     COLUPF                  ;3
    lda     ram_82                  ;3
    cmp     #$03                    ;2
    beq     L1765                   ;2/3
    cmp     #$01                    ;2
    bne     L176b                   ;2/3
    lda     ram_80                  ;3
    bne     L176b                   ;2/3
    lda     ram_C5,x                ;4
    cmp     #$28                    ;2
    bcc     L176b                   ;2/3 =  27
L1765
    lda     ram_C5,x                ;4
    beq     L176b                   ;2/3
    dec     ram_C5,x                ;6   =  12
L176b
    lda     ram_C5,x                ;4
    clc                             ;2
    adc     ram_C8                  ;3
    sta     ram_9E                  ;3   =  12
L1772
    lda     ram_82                  ;3
    cmp     #$03                    ;2
    beq     L179d                   ;2/3
    jsr     L1fa2                   ;6
    tay                             ;2
    lda     L1f6a,y                 ;4
    and     ram_80                  ;3
    bne     L179d                   ;2/3
    lda     ram_C9                  ;3
    bmi     L1791                   ;2/3
    inc     ram_C8                  ;5
    lda     ram_C8                  ;3
    cmp     #$10                    ;2
    bcc     L179d                   ;2/3
    bcs     L1797                   ;2/3 =  43

L1791
    dec     ram_C8                  ;5
    lda     ram_C8                  ;3
    bpl     L179d                   ;2/3 =  10
L1797
    lda     ram_C9                  ;3
    eor     #$80                    ;2
    sta     ram_C9                  ;3   =   8
L179d
    ldy     #$00                    ;2
    lda     ram_82                  ;3
    beq     L17f2                   ;2/3
    bmi     L17f2                   ;2/3
    lda     #$0a                    ;2
    sta     AUDC1                   ;3
    sta     AUDC0                   ;3
    lda     ram_C8                  ;3
    tay                             ;2
    and     #$08                    ;2
    beq     L17b6                   ;2/3
    tya                             ;2
    eor     #$0f                    ;2
    tay                             ;2   =  32
L17b6
    sty     AUDV0                   ;3
    jsr     L1fa2                   ;6
    tax                             ;2
    lda     L1f62,x                 ;4
    tax                             ;2
    stx     AUDF0                   ;3
    lda     ram_80                  ;3
    and     #$04                    ;2
    bne     L17c9                   ;2/3
    dex                             ;2   =  29
L17c9
    lda     ram_CD                  ;3
    beq     L17de                   ;2/3
    dec     ram_CD                  ;5
    ldy     #$07                    ;2
    ldx     #$1f                    ;2
    lda     ram_80                  ;3
    lsr                             ;2
    bcc     L17da                   ;2/3
    ldx     #$00                    ;2   =  23
L17da
    lda     #$01                    ;2
    sta     AUDC1                   ;3   =   5
L17de
    lda     ram_EA                  ;3
    beq     L17f2                   ;2/3
    ldx     #$04                    ;2
    stx     AUDC1                   ;3
    dec     ram_EA                  ;5
    ldy     #$00                    ;2
    lda     ram_80                  ;3
    and     #$03                    ;2
    bne     L17f2                   ;2/3
    ldy     #$0c                    ;2   =  26
L17f2
    stx     AUDF1                   ;3
    sty     AUDV1                   ;3
    lda     ram_CC                  ;3
    beq     L1801                   ;2/3!
    tay                             ;2
    lda     #$08                    ;2
    sta     AUDC0                   ;3
    ldx     #$1f                    ;2   =  20
L1801
    lda     ram_CE                  ;3
    beq     L180f                   ;2/3
    lsr                             ;2
    lsr                             ;2
    tax                             ;2
    tay                             ;2
    dec     ram_CE                  ;5
    lda     #$02                    ;2
    sta     AUDC0                   ;3   =  23
L180f
    stx     AUDF0                   ;3
    sty     AUDV0                   ;3
    jsr     L1df2                   ;6   =  12
L1816
    bit     TIMINT                  ;4
    bpl     L1816                   ;2/3
    jmp     L101d                   ;3   =   9

L181e SUBROUTINE ; 4x
    lda     #$46                    ;2
    sta     ram_A2                  ;3
    ldy     #$05                    ;2
    sty     ram_E8                  ;3
    sty     ram_E9                  ;3
    ldy     #$04                    ;2
    sty     ram_CF                  ;3
    sty     ram_D0                  ;3   =  21
L182e
    lda     #$f0                    ;2
    sta.wy  ram_A4,y                ;5
    dey                             ;2
    bpl     L182e                   ;2/3
  IF !OPTIMIZE ;{
    lda     #$00                    ;2
    sta     ram_C1                  ;3
    sta     ram_EA                  ;3
    sta     ram_8F                  ;3
    sta     ram_90                  ;3
    sta     ram_91                  ;3
    sta     ram_92                  ;3
    sta     ram_E6                  ;3
    sta     ram_E7                  ;3
    sta     player                  ;3
    sta     ram_E5                  ;3
  ELSE ;}
    iny
    FILL_NOP 1
    sty     ram_C1                  ;3
    sty     ram_EA                  ;3
    sty     ram_8F                  ;3
    sty     ram_90                  ;3
    sty     ram_91                  ;3
    sty     ram_92                  ;3
    sty     ram_E6                  ;3
    sty     ram_E7                  ;3
    sty     player                  ;3
    sty     ram_E5                  ;3
  ENDIF
    lda     #$0a                    ;2
    sta     ram_C2                  ;3
    lda     #$48                    ;2
    sta     ram_C3                  ;3
    lda     #$08                    ;2
    sta     ram_C0                  ;3
    lda     #$20                    ;2
    sta     ram_C5                  ;3
    sta     ram_C6                  ;3
    lda     #GREEN|$8               ;2
    sta     COLUPF                  ;3
    lda     #>ColorData             ;2
    sta     ram_E4                  ;3
    lda     #$80                    ;2
    sta     ram_CB                  ;3
    sta     ram_EB                  ;3
    rts                             ;6   =  90

L186d SUBROUTINE ; 2x
    tax                             ;2
    lda     ram_80                  ;3
    and     #$07                    ;2
    tay                             ;2
    txa                             ;2
    clc                             ;2
    adc     L1dea,y                 ;4
    lsr                             ;2
    lsr                             ;2
    lsr                             ;2
    eor     #$10                    ;2
    sec                             ;2
    sbc     #$10                    ;2
    rts                             ;6   =  35

L1881 SUBROUTINE ; 9x
    sta     ram_9B                  ;3
    lsr                             ;2
    lsr                             ;2
    lsr                             ;2
    lsr                             ;2
    sec                             ;2
    adc     ram_9B                  ;3
    lsr                             ;2
    lsr                             ;2
    lsr                             ;2
    lsr                             ;2
    sta     ram_9A                  ;3
    clc                             ;2
    adc     ram_9B                  ;3
    and     #$0f                    ;2
    sec                             ;2
    sbc     #$07                    ;2
    eor     #$ff                    ;2
    asl                             ;2
    asl                             ;2
    asl                             ;2
    asl                             ;2
    ora     ram_9A                  ;3
    rts                             ;6   =  57

L18a1
    lda     ram_E5                  ;3
  IF !OPTIMIZE ;{
    cmp     #$01                    ;2
    bne     L18bc                   ;2/3
    inc     ram_E5                  ;5
    lda     #$00                    ;2
  ELSE ;}
    eor     #$01                    ;2
    bne     L18bc                   ;2/3
    inc     ram_E5                  ;5
    FILL_NOP 2
  ENDIF
    sta     ram_9D                  ;3
    lda     #$09                    ;2
    sta     ram_F1                  ;3
    lda     #$50                    ;2
    sta     ram_EE                  ;3
    sta     ram_EF                  ;3
    sta     ram_ED                  ;3
    jsr     L1cf0                   ;6   =  39
L18bc
  IF !OPTIMIZE ;{
    lda     #>FireGfx               ;2          = $1d
    sta     ram_DA                  ;3
    sta     ram_DC                  ;3
  ELSE ;}
    ldx     #>FireGfx               ;2          = $1d
    stx     ram_DA                  ;3
    stx     ram_DC                  ;3
  ENDIF
    lda     #<SunGfx                ;2
    sta     ram_D9                  ;3
    lda     #<SunColors             ;2
    sta     ram_DB                  ;3
  IF !OPTIMIZE ;{
    lda     #>DigitGfx              ;2          = $1c
    sta     ram_D6                  ;3
    sta     ram_D8                  ;3
    sta     ram_D2                  ;3
  ELSE ;}
    dex
    FILL_NOP 1
    stx     ram_D6                  ;3
    stx     ram_D8                  ;3
    stx     ram_D2                  ;3
  ENDIF
    lda     ram_82                  ;3
    cmp     #$fe                    ;2
    bne     L18df                   ;2/3
    lda     SWCHA                   ;4
    bpl     L18fb                   ;2/3
    bmi     L1902                   ;2/3!=  44

L18df
    lda     ram_F1                  ;3         *
    jsr     Mult10                  ;6         *
    sta     ram_8D                  ;3         *
    dec     ram_ED                  ;5         *
    bpl     L1902                   ;2/3!      *
    ldx     player                  ;3         *
    ldy     ram_E6,x                ;4         *
    lda     L1d90,y                 ;4         *
    sta     ram_ED                  ;3         *
    dec     ram_F1                  ;5         *
    bpl     L1902                   ;2/3!      *
    lda     #$02                    ;2         *
    sta     ram_82                  ;3   =  45 *
L18fb
    lda     #$00                    ;2
    sta     ram_E5                  ;3
    jmp     L10f4                   ;3   =   8

L1902
    lda     ram_80                  ;3
    lsr                             ;2
    and     #$01                    ;2
    tax                             ;2
    lda     L1dc0,x                 ;4
    ldy     #<BlankGfx              ;2
    cpy     ram_D5                  ;3
    beq     L1913                   ;2/3
    sta     ram_D5                  ;3   =  23
L1913
    cpy     ram_D7                  ;3
    beq     L1919                   ;2/3
    sta     ram_D7                  ;3   =   8
L1919
    lda     L1dc2,x                 ;4
    sta     ram_D1                  ;3
    inc     ram_A9                  ;5
    lda     ram_A9                  ;3
    cmp     #$8c                    ;2
    bcc     L1928                   ;2/3
    lda     #$0a                    ;2   =  21
L1928
    sta     ram_A9                  ;3
    jsr     L1881                   ;6
    sta     ram_AE                  ;3
    dec     ram_AA                  ;5
    lda     ram_AA                  ;3
    cmp     #$0a                    ;2
    bcs     L1939                   ;2/3
    lda     #$8c                    ;2   =  26
L1939
    sta     ram_AA                  ;3
    jsr     L1881                   ;6
    sta     ram_AF                  ;3
    lda     ram_AB                  ;3
    jsr     L1881                   ;6
    sta     ram_B0                  ;3
    lda     ram_AC                  ;3
    jsr     L1881                   ;6
    sta     ram_B1                  ;3
    inc     ram_B6                  ;5
    lda     ram_B6                  ;3
    and     #$0f                    ;2
    tax                             ;2
    lda     L1f8a,x                 ;4
    sta     ram_B3                  ;3
    lda     L1f7a,x                 ;4
    sta     HMM0                    ;3
    sta     WSYNC                   ;3   =  65
;---------------------------------------
    jsr     Wait12                  ;6
    and     #$0f                    ;2
    tay                             ;2   =  10
L1967
    dey                             ;2
    bpl     L1967                   ;2/3
    sta     RESM0                   ;3
    sta     WSYNC                   ;3   =  10
;---------------------------------------
    lda     ram_80                  ;3
    lsr                             ;2
    lsr                             ;2
    lsr                             ;2
    lsr                             ;2
    clc                             ;2
    adc     ram_B6                  ;3
    and     #$0f                    ;2
    tax                             ;2
    lda     L1f8a,x                 ;4
    sta     ram_B4                  ;3
    lda     L1f7a,x                 ;4
    sta     HMM1                    ;3
    sta     WSYNC                   ;3   =  37
;---------------------------------------
    jsr     Wait12                  ;6
    and     #$0f                    ;2
    tay                             ;2   =  10
L198c
    dey                             ;2
    bpl     L198c                   ;2/3
    sta     RESM1                   ;3
    sta     WSYNC                   ;3   =  10
;---------------------------------------
    lda     ram_AD                  ;3
    cmp     #$70                    ;2
    bcc     L199e                   ;2/3
  IF !OPTIMIZE ;{
    sec                             ;2         *
  ELSE ;}
    FILL_NOP 1
  ENDIF
    sbc     #$30                    ;2         *
    bne     L19ae                   ;2/3 =  13 *
L199e
    cmp     #$60                    ;2
    bcc     L19a7                   ;2/3
  IF !OPTIMIZE ;{
    sec                             ;2         *
  ELSE ;}
    FILL_NOP 1
  ENDIF
    sbc     #$20                    ;2         *
    bne     L19ae                   ;2/3 =  10 *
L19a7
    cmp     #$40                    ;2
    bcc     L19ae                   ;2/3
  IF !OPTIMIZE ;{
    sec                             ;2         *
  ELSE ;}
    FILL_NOP 1
  ENDIF
    sbc     #$10                    ;2   =   8 *
L19ae
    lsr                             ;2
    lsr                             ;2
    lsr                             ;2
    lsr                             ;2
    lsr                             ;2
    tax                             ;2
    stx     ram_B2                  ;3
    lda     L1f9e,x                 ;4
    sta     ram_B5                  ;3
    lda     L1f9a,x                 ;4
    jsr     L1881                   ;6
    nop                             ;2
    nop                             ;2
    sta     HMBL                    ;3
    sta     WSYNC                   ;3   =  42
;---------------------------------------
    jsr     Wait12                  ;6
    and     #$0f                    ;2
    tay                             ;2   =  10
L19cd
    dey                             ;2
    bpl     L19cd                   ;2/3
    sta     RESBL                   ;3
    sta     WSYNC                   ;3   =  10
;---------------------------------------
    sta     HMOVE                   ;3
    sta     WSYNC                   ;3   =   6
;---------------------------------------
  IF !OPTIMIZE ;{
    lda     #$00                    ;2
    sta     HMM0                    ;3
    sta     HMM1                    ;3
    sta     HMBL                    ;3
  ELSE ;}
    iny
    sty     HMM0                    ;3
    sty     HMM1                    ;3
    sty     HMBL                    ;3
    FILL_NOP 1
  ENDIF
    jmp     L11fc                   ;3   =  14

L19e3
    lda     ram_C4                  ;3
    beq     L19e9                   ;2/3
    sta     ram_AD                  ;3   =   8
L19e9
    lda     ram_82                  ;3
    cmp     #$fe                    ;2
    beq     L1a18                   ;2/3!
    ldy     player                  ;3         *
    lda     SWCHA                   ;4         *
    and     L1f00,y                 ;4         *
    bne     L1a18                   ;2/3!      *
  IF !OPTIMIZE ;{
    ldx     #$00                    ;2         *
  ELSE ;}
    tax
    FILL_NOP 1
  ENDIF
    lda     ram_AD                  ;3         *
    cmp     #$50                    ;2         *
    bcc     L1a02                   ;2/3       *
    inx                             ;2   =  31 *
L1a02
    lda     ram_EE,x                ;4         *
    cmp     #$50                    ;2         *
    bne     L1a18                   ;2/3       *
    ldy     ram_B2                  ;3         *
    lda     #$0d                    ;2         *
    sta     ram_CD                  ;3         *
    lda     L1bf6,y                 ;4         *
    sta     ram_AB,x                ;4         *
    lda     L1de8,x                 ;4         *
    sta     ram_EE,x                ;4   =  32 *
L1a18
    ldx     #$01                    ;2   =   2
L1a1a
    lda     ram_EE,x                ;4
    cmp     #$50                    ;2
    beq     L1a45                   ;2/3
    cmp     L1bf4,x                 ;4         *
    bne     L1a2b                   ;2/3       *
    lda     #$50                    ;2         *
    sta     ram_EE,x                ;4         *
    bne     L1a45                   ;2/3 =  22 *

L1a2b
  IF !OPTIMIZE ;{
    cpx     #$00                    ;2         *
  ELSE ;}
    FILL_NOP 1
    txa
  ENDIF
    beq     L1a35                   ;2/3       *
    dec     ram_EE,x                ;6         *
    dec     ram_EE,x                ;6         *
    bpl     L1a39                   ;2/3 =  18 *
L1a35
    inc     ram_EE,x                ;6         *
    inc     ram_EE,x                ;6   =  12 *
L1a39
    lda     ram_AB,x                ;4         *
    cmp     #$50                    ;2         *
    bcc     L1a43                   ;2/3       *
    inc     ram_AB,x                ;6         *
    bne     L1a45                   ;2/3 =  16 *
L1a43
    dec     ram_AB,x                ;6   =   6 *
L1a45
    dex                             ;2
    bpl     L1a1a                   ;2/3
    lda     #<BlankGfx              ;2
    cmp     ram_D5                  ;3
    bne     L1a5b                   ;2/3
    cmp     ram_D7                  ;3         *
    bne     L1a5b                   ;2/3       *
    lda     ram_80                  ;3         *
    and     #$3f                    ;2         *
    bne     L1a5b                   ;2/3       *
    jsr     L1cf0                   ;6   =  29 *
L1a5b
    lda     CXPPMM                  ;3
    bpl     L1a6d                   ;2/3
    lda     #$50                    ;2         *
    cmp     ram_EF                  ;3         *
    beq     L1a6d                   ;2/3       *
    sta     ram_EF                  ;3         *
    lda     #<BlankGfx              ;2         *
    sta     ram_D7                  ;3         *
    bne     L1a7d                   ;2/3 =  22 *

L1a6d
    lda     ram_F0                  ;3
    bpl     L1a88                   ;2/3
    lda     #$50                    ;2         *
    cmp     ram_EE                  ;3         *
    beq     L1a88                   ;2/3       *
    sta     ram_EE                  ;3         *
    lda     #<BlankGfx              ;2         *
    sta     ram_D5                  ;3   =  20 *
L1a7d
    lda     #$10                    ;2         *
    jsr     L1fb6                   ;6         *
    inc     ram_9D                  ;5         *
    lda     #$3f                    ;2         *
    sta     ram_CE                  ;3   =  18 *
L1a88
    lda     ram_9D                  ;3
    cmp     #$05                    ;2
    bne     L1aa8                   ;2/3
    lda     #$30                    ;2         *
    sta     ram_EA                  ;3         *
    lda     #$50                    ;2         *
    sta     ram_9D                  ;3         *
    ldx     player                  ;3         *
    lda     ram_E6,x                ;4         *
    cmp     #$07                    ;2         *
    bcs     L1aa0                   ;2/3       *
    inc     ram_E6,x                ;6   =  34 *
L1aa0
    lda     ram_CF,x                ;4         *
    cmp     #$08                    ;2         *
    bcs     L1aa8                   ;2/3       *
    inc     ram_CF,x                ;6   =  14 *
L1aa8
    jmp     L1772                   ;3   =   3

L1aab
  IF !OPTIMIZE
    ldx     #BLACK|$0               ;2
    stx     ram_C7                  ;3
  ELSE
    FILL_NOP 2
    stx     ram_C7                  ;3
  ENDIF
    lda     ram_82                  ;3
    cmp     #$fe                    ;2
    beq     L1ab7                   ;2/3
    ldx     #$0e                    ;2   =  14 *
L1ab7
    stx     COLUPF                  ;3
    sta     CXCLR                   ;3
    lda     #BLACK|$0               ;2
    sta     CTRLPF                  ;3
    sta     COLUBK                  ;3
    lda     ram_AE                  ;3
    sta     HMP0                    ;3
    sta     WSYNC                   ;3   =  23
;---------------------------------------
    jsr     Wait12                  ;6
    and     #$0f                    ;2
    tay                             ;2   =  10
L1acd
    dey                             ;2
    bpl     L1acd                   ;2/3
    CHECKPAGE_LBL L1acd, "L1acd"
    sta     RESP0                   ;3
    sta     WSYNC                   ;3   =  10
;---------------------------------------
    lda     ram_B0                  ;3
    sta     HMP1                    ;3
    sta     WSYNC                   ;3   =   9
;---------------------------------------
    jsr     Wait12                  ;6
    and     #$0f                    ;2
    tay                             ;2   =  10
L1ae0
    dey                             ;2
    bpl     L1ae0                   ;2/3
    CHECKPAGE_LBL L1ae0, "L1ae0"
    sta     RESP1                   ;3
    sta     WSYNC                   ;3   =  10
;---------------------------------------
    sta     HMOVE                   ;3
    ldy     player                  ;3
    ldx     ram_E6,y                ;4
    lda     L1bec,x                 ;4
    sta     COLUP0                  ;3
    ldx     #$ae                    ;2
    lda     #YELLOW|$8              ;2
    sta     COLUP1                  ;3   =  24
L1af8
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    txa                             ;2
    sec                             ;2
    sbc     #$98                    ;2
    tay                             ;2
    and     #$f8                    ;2
    bne     L1b07                   ;2/3
    lda     (ram_D5),y              ;5
    sta     GRP0                    ;3   =  20
L1b07
    txa                             ;2
    sec                             ;2
    sbc     ram_EE                  ;3
    tay                             ;2
    and     #$f8                    ;2
    bne     L1b14                   ;2/3
    lda     (ram_D1),y              ;5         *
    sta     GRP1                    ;3   =  21 *
L1b14
    ldy     player                  ;3
    lda.wy  INPT0,y                 ;4
    bmi     L1b1d                   ;2/3
    stx     ram_C4                  ;3   =  12
L1b1d
    dex                             ;2
    cpx     #$6a                    ;2
    bne     L1af8                   ;2/3!
    ldy     #$07                    ;2
    sta     WSYNC                   ;3   =  11
;---------------------------------------
L1b26
    dey                             ;2
    bpl     L1b26                   ;2/3
    CHECKPAGE_LBL L1b26, "L1b26"
  IF !OPTIMIZE ;{
    nop                             ;2
    sta     RESP0                   ;3
    sta     RESP1                   ;3
    lda     #$b0                    ;2
    sta     HMP0                    ;3
    lda     #$d0                    ;2
    sta     HMP1                    ;3
    lda     #$08                    ;2
    sta     REFP1                   ;3
  ELSE ;}
    lda     #$b0                    ;2
    sta     RESP0                   ;3
    sta     RESP1                   ;3
    sta     HMP0                    ;3
    lda     #$d8                    ;2
    sta     HMP1                    ;3
    sta     REFP1                   ;3
    FILL_NOP 3
  ENDIF
    sta     WSYNC                   ;3   =  30
;---------------------------------------
    sta     HMOVE                   ;3
    lda     #$44                    ;2
    sta     COLUP0                  ;3
    sta     COLUP1                  ;3
    lda     CXPPMM                  ;3
    sta     ram_F0                  ;3
    ldx     #$68                    ;2   =  19
L1b4a
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    txa                             ;2
    ldx     #$1f                    ;2
    txs                             ;2
    tax                             ;2
    sec                             ;2
    sbc     #$50                    ;2
    tay                             ;2
    and     #$f0                    ;2
    bne     L1b65                   ;2/3
    lda     (ram_D9),y              ;5
    sta     GRP0                    ;3
    sta     GRP1                    ;3
    lda     (ram_DB),y              ;5
    sta     COLUP0                  ;3
    sta     COLUP1                  ;3   =  40
L1b65
    cpx     ram_B5                  ;3
    php                             ;3
    cpx     ram_B3                  ;3
    php                             ;3
    cpx     ram_B4                  ;3
    php                             ;3
    dex                             ;2
    cpx     #$48                    ;2
    bne     L1b4a                   ;2/3
    sta     WSYNC                   ;3   =  27
;---------------------------------------
  IF !OPTIMIZE ;{
    lda     #$00                    ;2
    sta     ENAM0                   ;3
    sta     ENAM1                   ;3
    sta     ENABL                   ;3
  ELSE ;}
    FILL_NOP 2
    stx     ENAM0                   ;3
    stx     ENAM1                   ;3
    stx     ENABL                   ;3
  ENDIF
    ldx     #$ff                    ;2
    txs                             ;2
    lda     ram_AF                  ;3
    sta     HMP0                    ;3
    sta     WSYNC                   ;3   =  24
;---------------------------------------
    jsr     Wait12                  ;6
    and     #$0f                    ;2
    tay                             ;2   =  10
L1b8c
    dey                             ;2
    bpl     L1b8c                   ;2/3
    CHECKPAGE_LBL L1b8c, "L1b8c"
    sta     RESP0                   ;3
    sta     WSYNC                   ;3   =  10
;---------------------------------------
    lda     ram_B1                  ;3
    sta     HMP1                    ;3
    sta     WSYNC                   ;3   =   9
;---------------------------------------
    jsr     Wait12                  ;6
    and     #$0f                    ;2
    tay                             ;2   =  10
L1b9f
    dey                             ;2
    bpl     L1b9f                   ;2/3
    CHECKPAGE_LBL L1b9f, "L1b9f"
    sta     RESP1                   ;3
    sta     WSYNC                   ;3   =  10
;---------------------------------------
    sta     HMOVE                   ;3
    lda     #$00                    ;2
    sta     REFP1                   ;3
    lda     #$08                    ;2
    sta     REFP0                   ;3
    ldy     player                  ;3
    ldx     ram_E6,y                ;4
    lda     L1bec,x                 ;4
    sta     COLUP0                  ;3
    lda     #YELLOW|$8              ;2
    sta     COLUP1                  ;3
    sta     CXCLR                   ;3
    ldx     #$42                    ;2   =  37
L1bc1
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    txa                             ;2
    sec                             ;2
    sbc     #$10                    ;2
    tay                             ;2
    and     #$f8                    ;2
    bne     L1bd0                   ;2/3
    lda     (ram_D7),y              ;5
    sta     GRP0                    ;3   =  20
L1bd0
    txa                             ;2
    sec                             ;2
    sbc     ram_EF                  ;3
    tay                             ;2
    and     #$f8                    ;2
    bne     L1bdd                   ;2/3
    lda     (ram_D1),y              ;5         *
    sta     GRP1                    ;3   =  21 *
L1bdd
    ldy     player                  ;3
    lda.wy  INPT0,y                 ;4
    bmi     L1be6                   ;2/3
    stx     ram_C4                  ;3   =  12
L1be6
    dex                             ;2
    bne     L1bc1                   ;2/3
    jmp     L143b                   ;3   =   7

L1bec
    .byte   MAUVE|$6                        ; $1bec (C)
    .byte   PURPLE|$6                       ; $1bed (*)
    .byte   CYAN_GREEN|$6
    .byte   $46
    .byte   BEIGE|$6,MAUVE|$4,$06,YELLOW|$8
L1bf4
    .byte   $b4,$0a                         ; $1bf4 (*)
L1bf6
    .byte   $49,$55,$55,$49                 ; $1bf6 (*)

L1bfa SUBROUTINE ; 3x
    lda     ram_C1                  ;3
    and     #$07                    ;2
    tax                             ;2
    rts                             ;6   =  13

  IF OPTIMIZE
L1f9e
    .byte   $65                             ; $1f9e (D)
    .byte   $65,$4b,$4b                     ; $1f9f (*)
L1f62
    .byte   $1f                             ; $1f62 (A)
    .byte   $1f                             ; $1f63 (A)
    .byte   $1c                             ; $1f64 (A)
    .byte   $18                             ; $1f65 (A)
    .byte   $14                             ; $1f66 (A)
    .byte   $12                             ; $1f67 (A)
    .byte   $10                             ; $1f68 (A)
    .byte   $0c                             ; $1f69 (A)

    IF PLUSROM
L1f6a
    .byte   $07                             ; $1f6a (D)
    .byte   $07                             ; $1f6b (*)
    .byte   $07,$03,$03,$01                 ; $1f6c (D)
    .byte   $01,$00                         ; $1f70 (*)

SendPlusROMScore
    lda     ram_91                  ;3
    sta     WriteToBuffer           ; BCD score hi
    lda     ram_8F                  ;3
    sta     WriteToBuffer           ; BCD score lo
    lda     #HIGHSCORE_ID           ; game id in Highscore DB
    sta     WriteSendBuffer
    jmp     L1fe2

PlusROM_API
       .byte "a", 0, "h.firmaplus.de"
       .byte 0

    ENDIF
  ENDIF

    ALIGN   256

DigitGfx
    .byte   %00111000 ; |  ###   |            $1c00 (G)
    .byte   %01111100 ; | #####  |            $1c01 (G)
    .byte   %11101110 ; |### ### |            $1c02 (G)
    .byte   %11000110 ; |##   ## |            $1c03 (G)
    .byte   %11000110 ; |##   ## |            $1c04 (G)
    .byte   %11000110 ; |##   ## |            $1c05 (G)
    .byte   %11000110 ; |##   ## |            $1c06 (G)
    .byte   %11101110 ; |### ### |            $1c07 (G)
    .byte   %01111100 ; | #####  |            $1c08 (G)
    .byte   %00111000 ; |  ###   |            $1c09 (G)
    .byte   %00011000 ; |   ##   |            $1c0a (G)
    .byte   %00011000 ; |   ##   |            $1c0b (G)
    .byte   %00011000 ; |   ##   |            $1c0c (G)
    .byte   %00011000 ; |   ##   |            $1c0d (G)
    .byte   %00011000 ; |   ##   |            $1c0e (G)
    .byte   %00011000 ; |   ##   |            $1c0f (G)
    .byte   %00011000 ; |   ##   |            $1c10 (G)
    .byte   %00011000 ; |   ##   |            $1c11 (G)
    .byte   %00111000 ; |  ###   |            $1c12 (G)
    .byte   %00011000 ; |   ##   |            $1c13 (G)
    .byte   %11111110 ; |####### |            $1c14 (G)
    .byte   %11111110 ; |####### |            $1c15 (G)
    .byte   %11000000 ; |##      |            $1c16 (G)
    .byte   %11111100 ; |######  |            $1c17 (G)
    .byte   %11111100 ; |######  |            $1c18 (G)
    .byte   %00001110 ; |    ### |            $1c19 (G)
    .byte   %00000110 ; |     ## |            $1c1a (G)
    .byte   %00001110 ; |    ### |            $1c1b (G)
    .byte   %11111110 ; |####### |            $1c1c (G)
    .byte   %11111100 ; |######  |            $1c1d (G)
    .byte   %11111100 ; |######  |            $1c1e (G)
    .byte   %11111110 ; |####### |            $1c1f (G)
    .byte   %00000110 ; |     ## |            $1c20 (G)
    .byte   %00000110 ; |     ## |            $1c21 (G)
    .byte   %01111110 ; | ###### |            $1c22 (G)
    .byte   %00111100 ; |  ####  |            $1c23 (G)
    .byte   %00011000 ; |   ##   |            $1c24 (G)
    .byte   %00000000 ; |        |            $1c25 (G)
    .byte   %11111100 ; |######  |            $1c26 (G)
    .byte   %11111110 ; |####### |            $1c27 (G)
    .byte   %00011000 ; |   ##   |            $1c28 (G)
    .byte   %00011000 ; |   ##   |            $1c29 (G)
    .byte   %00011000 ; |   ##   |            $1c2a (G)
    .byte   %00000000 ; |        |            $1c2b (G)
    .byte   %11111110 ; |####### |            $1c2c (G)
    .byte   %11111110 ; |####### |            $1c2d (G)
    .byte   %11100000 ; |###     |            $1c2e (G)
    .byte   %01110000 ; | ###    |            $1c2f (G)
    .byte   %00111000 ; |  ###   |            $1c30 (G)
    .byte   %00011100 ; |   ###  |            $1c31 (G)
    .byte   %11111000 ; |#####   |            $1c32 (G)
    .byte   %11111100 ; |######  |            $1c33 (G)
    .byte   %00001110 ; |    ### |            $1c34 (G)
    .byte   %00000110 ; |     ## |            $1c35 (G)
    .byte   %00001110 ; |    ### |            $1c36 (G)
    .byte   %11111100 ; |######  |            $1c37 (G)
    .byte   %11111000 ; |#####   |            $1c38 (G)
    .byte   %11000000 ; |##      |            $1c39 (G)
    .byte   %11111100 ; |######  |            $1c3a (G)
    .byte   %11111100 ; |######  |            $1c3b (G)
    .byte   %00111000 ; |  ###   |            $1c3c (G)
    .byte   %01111100 ; | #####  |            $1c3d (G)
    .byte   %11101110 ; |### ### |            $1c3e (G)
    .byte   %11000110 ; |##   ## |            $1c3f (G)
    .byte   %11000110 ; |##   ## |            $1c40 (G)
    .byte   %11000110 ; |##   ## |            $1c41 (G)
    .byte   %11100000 ; |###     |            $1c42 (G)
    .byte   %01110000 ; | ###    |            $1c43 (G)
    .byte   %00111000 ; |  ###   |            $1c44 (G)
    .byte   %00011100 ; |   ###  |            $1c45 (G)
    .byte   %11000000 ; |##      |            $1c46 (G)
    .byte   %11000000 ; |##      |            $1c47 (G)
    .byte   %11100000 ; |###     |            $1c48 (G)
    .byte   %01110000 ; | ###    |            $1c49 (G)
    .byte   %00111000 ; |  ###   |            $1c4a (G)
    .byte   %00011100 ; |   ###  |            $1c4b (G)
    .byte   %00000000 ; |        |            $1c4c (G)
    .byte   %00000000 ; |        |            $1c4d (G)
    .byte   %11111100 ; |######  |            $1c4e (G)
    .byte   %11111110 ; |####### |            $1c4f (G)
    .byte   %00111000 ; |  ###   |            $1c50 (G)
    .byte   %01111100 ; | #####  |            $1c51 (G)
    .byte   %11101110 ; |### ### |            $1c52 (G)
    .byte   %11000110 ; |##   ## |            $1c53 (G)
    .byte   %11000110 ; |##   ## |            $1c54 (G)
    .byte   %11101110 ; |### ### |            $1c55 (G)
    .byte   %01101100 ; | ## ##  |            $1c56 (G)
    .byte   %11000110 ; |##   ## |            $1c57 (G)
    .byte   %11101110 ; |### ### |            $1c58 (G)
    .byte   %01111100 ; | #####  |            $1c59 (G)
    .byte   %01110000 ; | ###    |            $1c5a (G)
    .byte   %00111000 ; |  ###   |            $1c5b (G)
    .byte   %00011100 ; |   ###  |            $1c5c (G)
    .byte   %00001110 ; |    ### |            $1c5d (G)
    .byte   %01100110 ; | ##  ## |            $1c5e (G)
    .byte   %11000110 ; |##   ## |            $1c5f (G)
    .byte   %11000110 ; |##   ## |            $1c60 (G)
    .byte   %11101110 ; |### ### |            $1c61 (G)
    .byte   %01111100 ; | #####  |            $1c62 (G)
    .byte   %00111000 ; |  ###   |            $1c63 (G)

L1c64
    .byte   $48,$28,$40,$58,$40,$1c         ; $1c64 (D)
    .byte   $24,$40,$20,$28,$30,$20,$1c,$20 ; $1c6a (*)
    .byte   $28,$30                         ; $1c72 (*)

BlankGfx
    .byte   %00000000 ; |        |            $1c74 (G)
    .byte   %00000000 ; |        |            $1c75 (G)
    .byte   %00000000 ; |        |            $1c76 (G)
    .byte   %00000000 ; |        |            $1c77 (G)
    .byte   %00000000 ; |        |            $1c78 (G)
    .byte   %00000000 ; |        |            $1c79 (G)
    .byte   %00000000 ; |        |            $1c7a (G)
    .byte   %00000000 ; |        |            $1c7b (G)
    .byte   %00000000 ; |        |            $1c7c (G)
    .byte   %00000000 ; |        |            $1c7d (G)

    .byte   $00,$00,$00,$00,$00,$00         ; $1c7e (*)

Copyright
    .byte   %00111000 ; |  ###   |            $1c84 (G)
    .byte   %01000100 ; | #   #  |            $1c85 (G)
    .byte   %01000100 ; | #   #  |            $1c86 (G)
    .byte   %10010010 ; |#  #  # |            $1c87 (G)
    .byte   %10100010 ; |# #   # |            $1c88 (G)
    .byte   %10100010 ; |# #   # |            $1c89 (G)
    .byte   %10010010 ; |#  #  # |            $1c8a (G)
    .byte   %01000100 ; | #   #  |            $1c8b (G)
    .byte   %01000100 ; | #   #  |            $1c8c (G)
    .byte   %00111000 ; |  ###   |            $1c8d (G)
    .byte   %00000000 ; |        |            $1c8e (G)
    .byte   %00000000 ; |        |            $1c8f (G)
    .byte   %01000101 ; | #   # #|            $1c90 (G)
    .byte   %01000101 ; | #   # #|            $1c91 (G)
    .byte   %01000101 ; | #   # #|            $1c92 (G)
    .byte   %01011101 ; | # ### #|            $1c93 (G)
    .byte   %01010101 ; | # # # #|            $1c94 (G)
    .byte   %01011101 ; | # ### #|            $1c95 (G)
    .byte   %00000000 ; |        |            $1c96 (G)
    .byte   %00000000 ; |        |            $1c97 (G)
    .byte   %00000000 ; |        |            $1c98 (G)
    .byte   %00000000 ; |        |            $1c99 (G)
    .byte   %11011100 ; |## ###  |            $1c9a (G)
    .byte   %01000100 ; | #   #  |            $1c9b (G)
    .byte   %01000100 ; | #   #  |            $1c9c (G)
    .byte   %11011100 ; |## ###  |            $1c9d (G)
    .byte   %01000100 ; | #   #  |            $1c9e (G)
    .byte   %11011100 ; |## ###  |            $1c9f (G)
    .byte   %00000000 ; |        |            $1ca0 (G)
    .byte   %00000000 ; |        |            $1ca1 (G)

    .byte   $00,$aa,$aa,$aa,$aa,$aa,$aa,$aa ; $1ca2 (D)
    .byte   $94,$00                         ; $1caa (D)

    .byte   %00000000 ; |        |            $1cac (G)
    .byte   %10010011 ; |#  #  ##|            $1cad (G)
    .byte   %10010100 ; |#  # #  |            $1cae (G)
    .byte   %10010100 ; |#  # #  |            $1caf (G)
    .byte   %10010101 ; |#  # # #|            $1cb0 (G)
    .byte   %11110100 ; |#### #  |            $1cb1 (G)
    .byte   %10010100 ; |#  # #  |            $1cb2 (G)
    .byte   %10010100 ; |#  # #  |            $1cb3 (G)
    .byte   %01100011 ; | ##   ##|            $1cb4 (G)
    .byte   %00000000 ; |        |            $1cb5 (G)
    .byte   %00000000 ; |        |            $1cb6 (G)
    .byte   %00100110 ; |  #  ## |            $1cb7 (G)
    .byte   %10101001 ; |# # #  #|            $1cb8 (G)
    .byte   %10101000 ; |# # #   |            $1cb9 (G)
    .byte   %10101000 ; |# # #   |            $1cba (G)
    .byte   %00101000 ; |  # #   |            $1cbb (G)
    .byte   %00101000 ; |  # #   |            $1cbc (G)
    .byte   %10101001 ; |# # #  #|            $1cbd (G)
    .byte   %00100110 ; |  #  ## |            $1cbe (G)
    .byte   %00000000 ; |        |            $1cbf (G)
    .byte   %00000000 ; |        |            $1cc0 (G)
    .byte   %00000000 ; |        |            $1cc1 (G)
    .byte   %01111100 ; | #####  |            $1cc2 (G)
    .byte   %00101000 ; |  # #   |            $1cc3 (G)
    .byte   %11111000 ; |#####   |            $1cc4 (G)
    .byte   %00111110 ; |  ##### |            $1cc5 (G)
    .byte   %00010111 ; |   # ###|            $1cc6 (G)
    .byte   %01111000 ; | ####   |            $1cc7 (G)
    .byte   %00000000 ; |        |            $1cc8 (G)
    .byte   %00000000 ; |        |            $1cc9 (G)
    .byte   %10111100 ; |# ####  |            $1cca (G)
    .byte   %00101000 ; |  # #   |            $1ccb (G)
    .byte   %11111000 ; |#####   |            $1ccc (G)
    .byte   %00111110 ; |  ##### |            $1ccd (G)
    .byte   %00010111 ; |   # ###|            $1cce (G)
    .byte   %10111000 ; |# ###   |            $1ccf (G)

    .byte   $00,$18,$00,$18,$00,$18,$00,$00 ; $1cd0 (*)

    .byte   %00000000 ; |        |            $1cd8 (G)
    .byte   %00000000 ; |        |            $1cd9 (G)
    .byte   %00111100 ; |  ####  |            $1cda (G)
    .byte   %00000000 ; |        |            $1cdb (G)
    .byte   %00111100 ; |  ####  |            $1cdc (G)
    .byte   %00000000 ; |        |            $1cdd (G)
    .byte   %00000000 ; |        |            $1cde (G)
    .byte   %00000000 ; |        |            $1cdf (G)

L1ce0
    .byte   $60,$e0                         ; $1ce0 (D)
L1ce2
    .byte   $50,$d0                         ; $1ce2 (D)

L1ce4
    .byte   RED|$a                          ; $1ce4 (C)
    .byte   BLUE_CYAN|$a                    ; $1ce5 (C)

L1ce6
    .byte   $2c,$1c                         ; $1ce6 (D)
L1ce8
    .byte   $01,$11,$21,$31,$41,$51,$61,$71 ; $1ce8 (D)

L1cf0 SUBROUTINE ;2x
    lda     #$c0                    ;2
    sta     ram_D5                  ;3
    sta     ram_D7                  ;3
    lda     #$8c                    ;2
    sta     ram_AA                  ;3
    lda     #$0a                    ;2
    sta     ram_A9                  ;3
    rts                             ;6   =  24

    .byte   $f2                             ; $1cff (*)

    ALIGN   256

FireGfx
    .byte   %00110011 ; |  ##  ##|            $1d00 (G)
    .byte   %01100110 ; | ##  ## |            $1d01 (G)
    .byte   %01000011 ; | #    ##|            $1d02 (G)
    .byte   %00100010 ; |  #   # |            $1d03 (G)
    .byte   %00001000 ; |    #   |            $1d04 (G)
    .byte   %01001100 ; | #  ##  |            $1d05 (G)
    .byte   %00100010 ; |  #   # |            $1d06 (G)
    .byte   %00000000 ; |        |            $1d07 (G)
    .byte   %00000000 ; |        |            $1d08 (G)
    .byte   %00000000 ; |        |            $1d09 (G)
    .byte   %00000000 ; |        |            $1d0a (G)
    .byte   %00000000 ; |        |            $1d0b (G)
    .byte   %00000000 ; |        |            $1d0c (G)

    .byte   $00,$00,$00,$67                 ; $1d0d (*)

    .byte   %01110110 ; | ### ## |            $1d11 (G)
    .byte   %01100111 ; | ##  ###|            $1d12 (G)
    .byte   %00101011 ; |  # # ##|            $1d13 (G)
    .byte   %10100010 ; |# #   # |            $1d14 (G)
    .byte   %00000100 ; |     #  |            $1d15 (G)
    .byte   %00000000 ; |        |            $1d16 (G)
    .byte   %00000000 ; |        |            $1d17 (G)
    .byte   %00000000 ; |        |            $1d18 (G)
    .byte   %00000000 ; |        |            $1d19 (G)
    .byte   %00000000 ; |        |            $1d1a (G)
    .byte   %00000000 ; |        |            $1d1b (G)
    .byte   %00000000 ; |        |            $1d1c (G)

    .byte   $00,$00,$00,$e3                 ; $1d1d (*)

    .byte   %11011101 ; |## ### #|            $1d21 (G)
    .byte   %01110111 ; | ### ###|            $1d22 (G)
    .byte   %01101011 ; | ## # ##|            $1d23 (G)
    .byte   %00101011 ; |  # # ##|            $1d24 (G)
    .byte   %10110110 ; |# ## ## |            $1d25 (G)
    .byte   %00010000 ; |   #    |            $1d26 (G)
    .byte   %00000000 ; |        |            $1d27 (G)
    .byte   %00000000 ; |        |            $1d28 (G)
    .byte   %00000000 ; |        |            $1d29 (G)
    .byte   %00000000 ; |        |            $1d2a (G)
    .byte   %00000000 ; |        |            $1d2b (G)
    .byte   %00000000 ; |        |            $1d2c (G)

    .byte   $00,$00,$00,$df                 ; $1d2d (*)

    .byte   %11111101 ; |###### #|            $1d31 (G)
    .byte   %01001110 ; | #  ### |            $1d32 (G)
    .byte   %01101110 ; | ## ### |            $1d33 (G)
    .byte   %00101011 ; |  # # ##|            $1d34 (G)
    .byte   %00111010 ; |  ### # |            $1d35 (G)
    .byte   %01010001 ; | # #   #|            $1d36 (G)
    .byte   %00000000 ; |        |            $1d37 (G)
    .byte   %00000000 ; |        |            $1d38 (G)
    .byte   %00000000 ; |        |            $1d39 (G)
    .byte   %00000000 ; |        |            $1d3a (G)
    .byte   %00000000 ; |        |            $1d3b (G)
    .byte   %00000000 ; |        |            $1d3c (G)

    .byte   $00,$00,$00,$e7                 ; $1d3d (*)

    .byte   %10010001 ; |#  #   #|            $1d41 (G)
    .byte   %11100111 ; |###  ###|            $1d42 (G)
    .byte   %10010100 ; |#  # #  |            $1d43 (G)
    .byte   %11100111 ; |###  ###|            $1d44 (G)
    .byte   %00100010 ; |  #   # |            $1d45 (G)
    .byte   %10000100 ; |#    #  |            $1d46 (G)
    .byte   %00000000 ; |        |            $1d47 (G)
    .byte   %00000000 ; |        |            $1d48 (G)
    .byte   %00000000 ; |        |            $1d49 (G)
    .byte   %00000000 ; |        |            $1d4a (G)
    .byte   %00000000 ; |        |            $1d4b (G)
    .byte   %00000000 ; |        |            $1d4c (G)

    .byte   $00,$00,$00,$bf                 ; $1d4d (*)

    .byte   %11111011 ; |##### ##|            $1d51 (G)
    .byte   %01011010 ; | # ## # |            $1d52 (G)
    .byte   %01011110 ; | # #### |            $1d53 (G)
    .byte   %11001011 ; |##  # ##|            $1d54 (G)
    .byte   %10001010 ; |#   # # |            $1d55 (G)
    .byte   %10000110 ; |#    ## |            $1d56 (G)
    .byte   %00000100 ; |     #  |            $1d57 (G)
    .byte   %00000000 ; |        |            $1d58 (G)
    .byte   %00000000 ; |        |            $1d59 (G)
    .byte   %00000000 ; |        |            $1d5a (G)
    .byte   %00000000 ; |        |            $1d5b (G)
    .byte   %00000000 ; |        |            $1d5c (G)

    .byte   $00,$00,$00,$cf                 ; $1d5d (*)

    .byte   %11111100 ; |######  |            $1d61 (G)
    .byte   %11100110 ; |###  ## |            $1d62 (G)
    .byte   %11101011 ; |### # ##|            $1d63 (G)
    .byte   %10101001 ; |# # #  #|            $1d64 (G)
    .byte   %10001011 ; |#   # ##|            $1d65 (G)
    .byte   %01000010 ; | #    # |            $1d66 (G)
    .byte   %00010000 ; |   #    |            $1d67 (G)
    .byte   %00000000 ; |        |            $1d68 (G)
    .byte   %00000000 ; |        |            $1d69 (G)
    .byte   %00000000 ; |        |            $1d6a (G)
    .byte   %00000000 ; |        |            $1d6b (G)
    .byte   %00000000 ; |        |            $1d6c (G)

    .byte   $00,$00,$00,$f2                 ; $1d6d (*)

    .byte   %00101111 ; |  # ####|            $1d71 (G)
    .byte   %01111111 ; | #######|            $1d72 (G)
    .byte   %11001011 ; |##  # ##|            $1d73 (G)
    .byte   %10000001 ; |#      #|            $1d74 (G)
    .byte   %11001011 ; |##  # ##|            $1d75 (G)
    .byte   %01100000 ; | ##     |            $1d76 (G)
    .byte   %00010000 ; |   #    |            $1d77 (G)
    .byte   %00000000 ; |        |            $1d78 (G)
    .byte   %00000000 ; |        |            $1d79 (G)
    .byte   %00000000 ; |        |            $1d7a (G)
    .byte   %00000000 ; |        |            $1d7b (G)
    .byte   %00000000 ; |        |            $1d7c (G)

    .byte   $00,$00,$00                     ; $1d7d (*)
    .byte   $44                             ; $1d80 (D)

    .byte   $44                          ; $1d81 (C)
    .byte   $46                          ; $1d82 (C)
    .byte   ORANGE|$4                       ; $1d83 (C)
    .byte   ORANGE|$4                       ; $1d84 (C)
    .byte   ORANGE|$8                       ; $1d85 (C)
    .byte   YELLOW|$a                       ; $1d86 (C)
    .byte   YELLOW|$e                       ; $1d87 (C)

    .byte   $00                             ; $1d88 (D)

    .byte   CYAN|$0                         ; $1d89 (C)
    .byte   BLUE|$0                         ; $1d8a (C)
    .byte   BLUE|$0                         ; $1d8b (C)
    .byte   VIOLET|$0                       ; $1d8c (C)
    .byte   VIOLET|$0                       ; $1d8d (C)
    .byte   MAUVE|$0                        ; $1d8e (C)
    .byte   MAUVE|$0                        ; $1d8f (C)

L1d90
    .byte   $50,$46,$3c,$32,$2d,$28,$23,$20 ; $1d90 (*)
L1d98
    .byte   $1c,$00,$1c,$0e,$1c,$1c         ; $1d98 (D)
    .byte   $1c,$0e                         ; $1d9e (*)
L1da0
    .byte   $06,$02,$06,$04,$0e,$0c         ; $1da0 (D)
    .byte   $06,$04                         ; $1da6 (*)
L1da8
    .byte   $02,$03,$02,$01,$02,$02         ; $1da8 (D)
    .byte   $02,$01                         ; $1dae (*)
L1db0
    .byte   $10,$20,$10,$05,$10             ; $1db0 (D)
    .byte   $10,$10,$05                     ; $1db5 (*)
L1db8
    .byte   $b0,$d0,$70,$80,$10,$20,$30,$40 ; $1db8 (D)
L1dc0
    .byte   $c0,$c8                         ; $1dc0 (D)
L1dc2
    .byte   $d0,$d8,$50,$60,$90,$a0         ; $1dc2 (D)

SunGfx
    .byte   %00000000 ; |        |            $1dc8 (G)
    .byte   %00000001 ; |       #|            $1dc9 (G)
    .byte   %00000111 ; |     ###|            $1dca (G)
    .byte   %00001111 ; |    ####|            $1dcb (G)
    .byte   %00011111 ; |   #####|            $1dcc (G)
    .byte   %00011111 ; |   #####|            $1dcd (G)
    .byte   %00111111 ; |  ######|            $1dce (G)
    .byte   %00111111 ; |  ######|            $1dcf (G)
    .byte   %00111111 ; |  ######|            $1dd0 (G)
    .byte   %00111111 ; |  ######|            $1dd1 (G)
    .byte   %00111111 ; |  ######|            $1dd2 (G)
    .byte   %00011111 ; |   #####|            $1dd3 (G)
    .byte   %00011111 ; |   #####|            $1dd4 (G)
    .byte   %00001111 ; |    ####|            $1dd5 (G)
    .byte   %00000111 ; |     ###|            $1dd6 (G)
    .byte   %00000001 ; |       #|            $1dd7 (G)
SunColors
    .byte   $44                          ; $1de8 (C)
    .byte   $44                          ; $1de9 (C)
    .byte   $46                          ; $1dea (C)
    .byte   $46                          ; $1deb (C)
    .byte   ORANGE|$8                       ; $1dec (C)
    .byte   ORANGE|$8                       ; $1ded (C)
    .byte   ORANGE|$a                       ; $1dee (C)
    .byte   ORANGE|$a                       ; $1def (C)
    .byte   ORANGE|$a                       ; $1de0 (C)
    .byte   ORANGE|$a                       ; $1de1 (C)
    .byte   ORANGE|$8                       ; $1de2 (C)
    .byte   ORANGE|$8                       ; $1de3 (C)
    .byte   $46                          ; $1de4 (C)
    .byte   $46                          ; $1de5 (C)
    .byte   $44                          ; $1de6 (C)
    .byte   $44                          ; $1de7 (C)

L1de8
    .byte   $6a,$42                         ; $1de8 (*)
L1dea
    .byte   $05,$02,$07,$04,$01,$06,$03,$00 ; $1dea (D)

L1df2 SUBROUTINE ;1x
    lda     ram_94                  ;3
    ror                             ;2
    ror                             ;2
    ror                             ;2
    eor     ram_95                  ;3
    asl                             ;2
    asl                             ;2
    rol     ram_94                  ;5
    rol     ram_95                  ;5
    rts                             ;6   =  32

    ALIGN   256

EnemyGfx
    .byte   %00000000 ; |        |            $1e00 (G)
    .byte   %00000000 ; |        |            $1e01 (G)
    .byte   %00010000 ; |   #    |            $1e02 (G)
    .byte   %10010010 ; |#  #  # |            $1e03 (G)
    .byte   %01010100 ; | # # #  |            $1e04 (G)
    .byte   %00111000 ; |  ###   |            $1e05 (G)
    .byte   %00111000 ; |  ###   |            $1e06 (G)
    .byte   %01010100 ; | # # #  |            $1e07 (G)
    .byte   %10010010 ; |#  #  # |            $1e08 (G)
    .byte   %00010000 ; |   #    |            $1e09 (G)
    .byte   %00111000 ; |  ###   |            $1e0a (G)
    .byte   %00010000 ; |   #    |            $1e0b (G)
    .byte   %00111000 ; |  ###   |            $1e0c (G)
    .byte   %00010000 ; |   #    |            $1e0d (G)
    .byte   %00010000 ; |   #    |            $1e0e (G)
    .byte   %00000000 ; |        |            $1e0f (G)

    .byte   %00000000 ; |        |            $1e10 (G)
    .byte   %00000000 ; |        |            $1e11 (G)
    .byte   %00111000 ; |  ###   |            $1e12 (G)
    .byte   %01101100 ; | ## ##  |            $1e13 (G)
    .byte   %01011110 ; | # #### |            $1e14 (G)
    .byte   %00101110 ; |  # ### |            $1e15 (G)
    .byte   %00110111 ; |  ## ###|            $1e16 (G)
    .byte   %00011011 ; |   ## ##|            $1e17 (G)
    .byte   %00001110 ; |    ### |            $1e18 (G)
    .byte   %00000000 ; |        |            $1e19 (G)
    .byte   %00000000 ; |        |            $1e1a (G)
    .byte   %00000000 ; |        |            $1e1b (G)
    .byte   %00010000 ; |   #    |            $1e1c (G)
    .byte   %00000001 ; |       #|            $1e1d (G)
    .byte   %01000000 ; | #      |            $1e1e (G)
    .byte   %00000001 ; |       #|            $1e1f (G)

    .byte   %00000000 ; |        |            $1e20 (G)
    .byte   %00000000 ; |        |            $1e21 (G)
    .byte   %00011100 ; |   ###  |            $1e22 (G)
    .byte   %00111110 ; |  ##### |            $1e23 (G)
    .byte   %00111111 ; |  ######|            $1e24 (G)
    .byte   %01111111 ; | #######|            $1e25 (G)
    .byte   %11011011 ; |## ## ##|            $1e26 (G)
    .byte   %11000110 ; |##   ## |            $1e27 (G)
    .byte   %01111100 ; | #####  |            $1e28 (G)
    .byte   %00000001 ; |       #|            $1e29 (G)
    .byte   %01001000 ; | #  #   |            $1e2a (G)
    .byte   %00001000 ; |    #   |            $1e2b (G)
    .byte   %00001000 ; |    #   |            $1e2c (G)
    .byte   %01000000 ; | #      |            $1e2d (G)
    .byte   %00001000 ; |    #   |            $1e2e (G)
    .byte   %01000000 ; | #      |            $1e2f (G)

    .byte   %00000000 ; |        |            $1e30 (G)
    .byte   %00000000 ; |        |            $1e31 (G)
    .byte   %00011000 ; |   ##   |            $1e32 (G)
    .byte   %00011000 ; |   ##   |            $1e33 (G)
    .byte   %00111100 ; |  ####  |            $1e34 (G)
    .byte   %00111100 ; |  ####  |            $1e35 (G)
    .byte   %01100110 ; | ##  ## |            $1e36 (G)
    .byte   %01111110 ; | ###### |            $1e37 (G)
    .byte   %01111110 ; | ###### |            $1e38 (G)
    .byte   %11100111 ; |###  ###|            $1e39 (G)
    .byte   %11000011 ; |##    ##|            $1e3a (G)
    .byte   %11000011 ; |##    ##|            $1e3b (G)
    .byte   %11000011 ; |##    ##|            $1e3c (G)
    .byte   %11000011 ; |##    ##|            $1e3d (G)
    .byte   %10000001 ; |#      #|            $1e3e (G)
    .byte   %10000001 ; |#      #|            $1e3f (G)

    .byte   %00000000 ; |        |            $1e40 (G)
    .byte   %00000000 ; |        |            $1e41 (G)
    .byte   %00011000 ; |   ##   |            $1e42 (G)
    .byte   %00011000 ; |   ##   |            $1e43 (G)
    .byte   %00111100 ; |  ####  |            $1e44 (G)
    .byte   %00111100 ; |  ####  |            $1e45 (G)
    .byte   %00100100 ; |  #  #  |            $1e46 (G)
    .byte   %01111110 ; | ###### |            $1e47 (G)
    .byte   %01111110 ; | ###### |            $1e48 (G)
    .byte   %11100111 ; |###  ###|            $1e49 (G)
    .byte   %11001011 ; |##  # ##|            $1e4a (G)
    .byte   %11010011 ; |## #  ##|            $1e4b (G)
    .byte   %11001011 ; |##  # ##|            $1e4c (G)
    .byte   %11010011 ; |## #  ##|            $1e4d (G)
    .byte   %10001001 ; |#   #  #|            $1e4e (G)
    .byte   %10010001 ; |#  #   #|            $1e4f (G)

    .byte   %00000000 ; |        |            $1e50 (G)
    .byte   %00000000 ; |        |            $1e51 (G)
    .byte   %00000000 ; |        |            $1e52 (G)
    .byte   %00000000 ; |        |            $1e53 (G)
    .byte   %10000001 ; |#      #|            $1e54 (G)
    .byte   %10011001 ; |#  ##  #|            $1e55 (G)
    .byte   %11011011 ; |## ## ##|            $1e56 (G)
    .byte   %10011001 ; |#  ##  #|            $1e57 (G)
    .byte   %11011011 ; |## ## ##|            $1e58 (G)
    .byte   %01111110 ; | ###### |            $1e59 (G)
    .byte   %11111111 ; |########|            $1e5a (G)
    .byte   %01111110 ; | ###### |            $1e5b (G)
    .byte   %01010010 ; | # #  # |            $1e5c (G)
    .byte   %00100100 ; |  #  #  |            $1e5d (G)
    .byte   %00011000 ; |   ##   |            $1e5e (G)
    .byte   %00000000 ; |        |            $1e5f (G)

    .byte   %00000000 ; |        |            $1e60 (G)
    .byte   %00000000 ; |        |            $1e61 (G)
    .byte   %00011000 ; |   ##   |            $1e62 (G)
    .byte   %10000001 ; |#      #|            $1e63 (G)
    .byte   %10011001 ; |#  ##  #|            $1e64 (G)
    .byte   %11011011 ; |## ## ##|            $1e65 (G)
    .byte   %10011001 ; |#  ##  #|            $1e66 (G)
    .byte   %11011011 ; |## ## ##|            $1e67 (G)
    .byte   %01011010 ; | # ## # |            $1e68 (G)
    .byte   %01111110 ; | ###### |            $1e69 (G)
    .byte   %11111111 ; |########|            $1e6a (G)
    .byte   %11111111 ; |########|            $1e6b (G)
    .byte   %01001010 ; | #  # # |            $1e6c (G)
    .byte   %00100100 ; |  #  #  |            $1e6d (G)
    .byte   %00011000 ; |   ##   |            $1e6e (G)
    .byte   %00000000 ; |        |            $1e6f (G)

    .byte   %00000000 ; |        |            $1e70 (G)
    .byte   %00000000 ; |        |            $1e71 (G)
    .byte   %00000000 ; |        |            $1e72 (G)
    .byte   %00011100 ; |   ###  |            $1e73 (G)
    .byte   %00111110 ; |  ##### |            $1e74 (G)
    .byte   %00011100 ; |   ###  |            $1e75 (G)
    .byte   %00101010 ; |  # # # |            $1e76 (G)
    .byte   %01000010 ; | #    # |            $1e77 (G)
    .byte   %00101000 ; |  # #   |            $1e78 (G)
    .byte   %00101010 ; |  # # # |            $1e79 (G)
    .byte   %00000000 ; |        |            $1e7a (G)
    .byte   %00101000 ; |  # #   |            $1e7b (G)
    .byte   %00100010 ; |  #   # |            $1e7c (G)
    .byte   %00100000 ; |  #     |            $1e7d (G)
    .byte   %00100000 ; |  #     |            $1e7e (G)
    .byte   %00101000 ; |  # #   |            $1e7f (G)

    .byte   %00000000 ; |        |            $1e80 (G)
    .byte   %00000000 ; |        |            $1e81 (G)
    .byte   %00001000 ; |    #   |            $1e82 (G)
    .byte   %00001000 ; |    #   |            $1e83 (G)
    .byte   %00011100 ; |   ###  |            $1e84 (G)
    .byte   %01111111 ; | #######|            $1e85 (G)
    .byte   %00011100 ; |   ###  |            $1e86 (G)
    .byte   %00001000 ; |    #   |            $1e87 (G)
    .byte   %00001010 ; |    # # |            $1e88 (G)
    .byte   %01000010 ; | #    # |            $1e89 (G)
    .byte   %00100000 ; |  #     |            $1e8a (G)
    .byte   %00000010 ; |      # |            $1e8b (G)
    .byte   %01001000 ; | #  #   |            $1e8c (G)
    .byte   %00101000 ; |  # #   |            $1e8d (G)
    .byte   %00000010 ; |      # |            $1e8e (G)
    .byte   %00100000 ; |  #     |            $1e8f (G)

    .byte   %00000000 ; |        |            $1e90 (G)
    .byte   %00000000 ; |        |            $1e91 (G)
    .byte   %00001000 ; |    #   |            $1e92 (G)
    .byte   %00011100 ; |   ###  |            $1e93 (G)
    .byte   %00110110 ; |  ## ## |            $1e94 (G)
    .byte   %00010100 ; |   # #  |            $1e95 (G)
    .byte   %01011101 ; | # ### #|            $1e96 (G)
    .byte   %01011101 ; | # ### #|            $1e97 (G)
    .byte   %01111111 ; | #######|            $1e98 (G)
    .byte   %01011101 ; | # ### #|            $1e99 (G)
    .byte   %00011100 ; |   ###  |            $1e9a (G)
    .byte   %00111110 ; |  ##### |            $1e9b (G)
    .byte   %01111111 ; | #######|            $1e9c (G)
    .byte   %01011101 ; | # ### #|            $1e9d (G)
    .byte   %01010101 ; | # # # #|            $1e9e (G)
    .byte   %00000000 ; |        |            $1e9f (G)

    .byte   %00000000 ; |        |            $1ea0 (G)
    .byte   %00000000 ; |        |            $1ea1 (G)
    .byte   %00001000 ; |    #   |            $1ea2 (G)
    .byte   %00011100 ; |   ###  |            $1ea3 (G)
    .byte   %00110110 ; |  ## ## |            $1ea4 (G)
    .byte   %00010100 ; |   # #  |            $1ea5 (G)
    .byte   %01011101 ; | # ### #|            $1ea6 (G)
    .byte   %01011101 ; | # ### #|            $1ea7 (G)
    .byte   %01111111 ; | #######|            $1ea8 (G)
    .byte   %01011101 ; | # ### #|            $1ea9 (G)
    .byte   %00011100 ; |   ###  |            $1eaa (G)
    .byte   %00111110 ; |  ##### |            $1eab (G)
    .byte   %01111111 ; | #######|            $1eac (G)
    .byte   %01011101 ; | # ### #|            $1ead (G)
    .byte   %01011101 ; | # ### #|            $1eae (G)
    .byte   %01010101 ; | # # # #|            $1eaf (G)

    .byte   %01000100 ; | #   #  |            $1eb0 (G)
    .byte   %11101110 ; |### ### |            $1eb1 (G)
    .byte   %11111110 ; |####### |            $1eb2 (G)
    .byte   %10111010 ; |# ### # |            $1eb3 (G)
    .byte   %00111000 ; |  ###   |            $1eb4 (G)
    .byte   %00010000 ; |   #    |            $1eb5 (G)
    .byte   %01010100 ; | # # #  |            $1eb6 (G)
    .byte   %01010100 ; | # # #  |            $1eb7 (G)
    .byte   %01111100 ; | #####  |            $1eb8 (G)
    .byte   %01111100 ; | #####  |            $1eb9 (G)
    .byte   %00101000 ; |  # #   |            $1eba (G)
    .byte   %00111000 ; |  ###   |            $1ebb (G)
    .byte   %00010000 ; |   #    |            $1ebc (G)
    .byte   %00010000 ; |   #    |            $1ebd (G)
    .byte   %00000000 ; |        |            $1ebe (G)
    .byte   %00000000 ; |        |            $1ebf (G)

    .byte   %00000000 ; |        |            $1ec0 (G)
    .byte   %00000000 ; |        |            $1ec1 (G)
    .byte   %00000000 ; |        |            $1ec2 (G)
    .byte   %00000000 ; |        |            $1ec3 (G)
    .byte   %00000000 ; |        |            $1ec4 (G)
    .byte   %00000000 ; |        |            $1ec5 (G)
    .byte   %00000000 ; |        |            $1ec6 (G)
    .byte   %00000000 ; |        |            $1ec7 (G)
    .byte   %00000000 ; |        |            $1ec8 (G)
    .byte   %00000000 ; |        |            $1ec9 (G)
    .byte   %00000000 ; |        |            $1eca (G)
    .byte   %00000000 ; |        |            $1ecb (G)
    .byte   %00000000 ; |        |            $1ecc (G)
    .byte   %00000000 ; |        |            $1ecd (G)
    .byte   %00000000 ; |        |            $1ece (G)
    .byte   %00000000 ; |        |            $1ecf (G)

    .byte   %10000010 ; |#     # |            $1ed0 (G)
    .byte   %11101110 ; |### ### |            $1ed1 (G)
    .byte   %11111110 ; |####### |            $1ed2 (G)
    .byte   %10111010 ; |# ### # |            $1ed3 (G)
    .byte   %00111000 ; |  ###   |            $1ed4 (G)
    .byte   %00010000 ; |   #    |            $1ed5 (G)
    .byte   %00010000 ; |   #    |            $1ed6 (G)
    .byte   %01010100 ; | # # #  |            $1ed7 (G)
    .byte   %01111100 ; | #####  |            $1ed8 (G)
    .byte   %01111100 ; | #####  |            $1ed9 (G)
    .byte   %00101000 ; |  # #   |            $1eda (G)
    .byte   %00111000 ; |  ###   |            $1edb (G)
    .byte   %00010000 ; |   #    |            $1edc (G)
    .byte   %00010000 ; |   #    |            $1edd (G)
    .byte   %00000000 ; |        |            $1ede (G)
    .byte   %00000000 ; |        |            $1edf (G)

    .byte   %00000000 ; |        |            $1ee0 (G)
    .byte   %00000000 ; |        |            $1ee1 (G)
    .byte   %00000000 ; |        |            $1ee2 (G)
    .byte   %00000000 ; |        |            $1ee3 (G)
    .byte   %00101000 ; |  # #   |            $1ee4 (G)
    .byte   %01111100 ; | #####  |            $1ee5 (G)
    .byte   %00111010 ; |  ### # |            $1ee6 (G)
    .byte   %00011100 ; |   ###  |            $1ee7 (G)
    .byte   %00101000 ; |  # #   |            $1ee8 (G)
    .byte   %00000000 ; |        |            $1ee9 (G)
    .byte   %00000000 ; |        |            $1eea (G)
    .byte   %00000000 ; |        |            $1eeb (G)
    .byte   %00000000 ; |        |            $1eec (G)
    .byte   %00000000 ; |        |            $1eed (G)
    .byte   %00000000 ; |        |            $1eee (G)
    .byte   %00000000 ; |        |            $1eef (G)

    .byte   %00000000 ; |        |            $1ef0 (G)
    .byte   %00000000 ; |        |            $1ef1 (G)
    .byte   %00100010 ; |  #   # |            $1ef2 (G)
    .byte   %00000100 ; |     #  |            $1ef3 (G)
    .byte   %10000001 ; |#      #|            $1ef4 (G)
    .byte   %00000000 ; |        |            $1ef5 (G)
    .byte   %00010100 ; |   # #  |            $1ef6 (G)
    .byte   %00001000 ; |    #   |            $1ef7 (G)
    .byte   %00010100 ; |   # #  |            $1ef8 (G)
    .byte   %01000001 ; | #     #|            $1ef9 (G)
    .byte   %10000000 ; |#       |            $1efa (G)
    .byte   %00010010 ; |   #  # |            $1efb (G)
    .byte   %00100010 ; |  #   # |            $1efc (G)
    .byte   %01001000 ; | #  #   |            $1efd (G)
    .byte   %00000000 ; |        |            $1efe (G)
    .byte   %01000100 ; | #   #  |            $1eff (G)

L1f00
ColorData
    .byte   $80                         ; $1f00 (C)
    .byte   $40                         ; $1f01 (C)
    .byte   ORANGE|$4                       ; $1f02 (C)
    .byte   ORANGE|$4                       ; $1f03 (C)
    .byte   BROWN|$a                        ; $1f04 (C)
    .byte   YELLOW|$c                       ; $1f05 (C)
    .byte   BROWN|$a                        ; $1f06 (C)
    .byte   ORANGE|$4                       ; $1f07 (C)
    .byte   ORANGE|$4                       ; $1f08 (C)
    .byte   BROWN|$4                        ; $1f09 (C)
    .byte   BROWN|$4                        ; $1f0a (C)
    .byte   BROWN|$4                        ; $1f0b (C)
    .byte   BROWN|$4                        ; $1f0c (C)
    .byte   BROWN|$4                        ; $1f0d (C)
    .byte   BROWN|$4                        ; $1f0e (C)
    .byte   BROWN|$4                        ; $1f0f (C)
    .byte   YELLOW|$c                       ; $1f10 (C)
    .byte   YELLOW|$a                       ; $1f11 (C)
    .byte   BROWN|$8                        ; $1f12 (C)
    .byte   ORANGE|$6                       ; $1f13 (C)
    .byte   $44                          ; $1f14 (C)
    .byte   ORANGE|$4                       ; $1f15 (C)
    .byte   ORANGE|$4                       ; $1f16 (C)
    .byte   $42                          ; $1f17 (C)
    .byte   $42                          ; $1f18 (C)
    .byte   $42                          ; $1f19 (C)
    .byte   $42                          ; $1f1a (C)
    .byte   $42                          ; $1f1b (C)
    .byte   $42                          ; $1f1c (C)
    .byte   $42                          ; $1f1d (C)
    .byte   $44                          ; $1f1e (C)
    .byte   BLACK|$a                        ; $1f1f (C)
    .byte   $44                          ; $1f20 (C)
    .byte   VIOLET|$6                       ; $1f21 (C)
    .byte   VIOLET|$6                       ; $1f22 (C)
    .byte   VIOLET|$6                       ; $1f23 (C)
    .byte   BLUE|$8                         ; $1f24 (C)
    .byte   BLUE|$8                         ; $1f25 (C)
    .byte   BLUE|$8                         ; $1f26 (C)
    .byte   BLUE|$8                         ; $1f27 (C)
    .byte   GREEN|$8                        ; $1f28 (C)
    .byte   GREEN|$8                        ; $1f29 (C)
    .byte   GREEN|$8                        ; $1f2a (C)
    .byte   YELLOW|$c                       ; $1f2b (C)
    .byte   $46                          ; $1f2c (C)
    .byte   $46                          ; $1f2d (C)
    .byte   $46                          ; $1f2e (C)
    .byte   ORANGE|$8                       ; $1f2f (C)
    .byte   ORANGE|$8                       ; $1f30 (C)
    .byte   ORANGE|$8                       ; $1f31 (C)
    .byte   GREEN_BEIGE|$8                  ; $1f32 (C)
    .byte   GREEN_BEIGE|$8                  ; $1f33 (C)
    .byte   CYAN_GREEN|$a                   ; $1f34 (C)
    .byte   CYAN_GREEN|$a                   ; $1f35 (C)
    .byte   CYAN_GREEN|$a                   ; $1f36 (C)
    .byte   ORANGE|$8                       ; $1f37 (C)
    .byte   ORANGE|$8                       ; $1f38 (C)
    .byte   ORANGE|$8                       ; $1f39 (C)
    .byte   ORANGE|$8                       ; $1f3a (C)
    .byte   ORANGE|$8                       ; $1f3b (C)
L1f3c
    .byte   BLACK|$8                        ; $1f3c (C)
    .byte   $10                       ; $1f3d (C)
    .byte   BLACK|$b                        ; $1f3e (C)
    .byte   BLACK|$8                        ; $1f3f (C)
    .byte   BLACK|$f                        ; $1f40 (C)
L1f41
    .byte   $14                       ; $1f41 (C)
    .byte   $13                       ; $1f42 (C)
    .byte   BLACK|$b                        ; $1f43 (C)
    .byte   BLACK|$c                        ; $1f44 (C)
    .byte   $18                       ; $1f45 (C)
    .byte   $18                       ; $1f46 (C)
    .byte   $14                       ; $1f47 (C)
    .byte   $14                       ; $1f48 (C)
    .byte   $20                        ; $1f49 (C)
    .byte   $18                       ; $1f4a (C)
    .byte   BLACK|$8                        ; $1f4b (C)

L1f4c
    .byte   $00,$02,$01,$0b,$02,$02         ; $1f4c (D)
    .byte   $03,$13,$03,$05,$02,$10,$01,$00 ; $1f52 (*)
    .byte   $10,$20                         ; $1f5a (*)

L1f5c
    .byte   BLACK|$0                        ; $1f5c (CP)
    .byte   BLACK|$0                        ; $1f5d (CP)
    .byte   BLUE|$4                         ; $1f5e (CP)
    .byte   GREEN|$6                        ; $1f5f (CP)
    .byte   YELLOW|$a                       ; $1f60 (CP)
    .byte   ORANGE|$6                       ; $1f61 (CP)

  IF !OPTIMIZE ;{
L1f62
    .byte   $1f                             ; $1f62 (A)
    .byte   $1f                             ; $1f63 (A)
    .byte   $1c                             ; $1f64 (A)
    .byte   $18                             ; $1f65 (A)
    .byte   $14                             ; $1f66 (A)
    .byte   $12                             ; $1f67 (A)
    .byte   $10                             ; $1f68 (A)
    .byte   $0c                             ; $1f69 (A)
  ENDIF ;}
  IF !PLUSROM
L1f6a
    .byte   $07                             ; $1f6a (D)
    .byte   $07                             ; $1f6b (*)
    .byte   $07,$03,$03,$01                 ; $1f6c (D)
    .byte   $01,$00                         ; $1f70 (*)
  ENDIF ;}
L1f72
    .byte   $80,$70,$60                     ; $1f72 (D)
    .byte   $50,$40,$30,$25,$20             ; $1f75 (*)
L1f7a
    .byte   $b4,$a4,$84,$45,$f5,$a5,$66,$46 ; $1f7a (D)
    .byte   $36,$46,$66,$a5,$f5,$45,$84,$a4 ; $1f82 (D)
L1f8a
    .byte   $58,$5d,$62,$65,$67,$65,$62,$5d ; $1f8a (D)
    .byte   $58,$53,$4e,$4b,$49,$4b,$4e,$53 ; $1f92 (D)
L1f9a
    .byte   $4f                             ; $1f9a (D)
    .byte   $55,$55,$4f                     ; $1f9b (*)
  IF !OPTIMIZE ;{
L1f9e
    .byte   $65                             ; $1f9e (D)
    .byte   $65,$4b,$4b                     ; $1f9f (*)
  ENDIF ;}

L1fa2 SUBROUTINE ; 3x
    ldx     player                  ;3
    lda     ram_C5,x                ;4
    lsr                             ;2
    lsr                             ;2
    lsr                             ;2
    lsr                             ;2
    rts                             ;6   =  21

Mult10 SUBROUTINE ; 5x
    asl                             ;2
    sta     ram_99                  ;3
    asl                             ;2
    asl                             ;2
    clc                             ;2
    adc     ram_99                  ;3
    sta     ram_99                  ;3   =  17
Wait12
    rts                             ;6   =   6

L1fb6 SUBROUTINE ;2x
    ldx     player                  ;3
    sed                             ;2
    clc                             ;2
    adc     ram_8F,x                ;4
    sta     ram_8F,x                ;4
    lda     ram_91,x                ;4
    adc     #$00                    ;2
    sta     ram_91,x                ;4
    cld                             ;2
    rts                             ;6   =  33

L1fc6 SUBROUTINE ;2x
    lda     #$3f                    ;2
    sta     ram_CE                  ;3
    lda     #$04                    ;2
    sta     ram_E0                  ;3
    lda     #$e0                    ;2
    sta     ram_9F                  ;3
    rts                             ;6   =  21

L1fd3 SUBROUTINE ;1x
    ldx     ram_82                  ;3
    cpx     #$01                    ;2
    bne     L1ffb                   ;2/3
    ldx     player                  ;3
    clc                             ;2
    adc     ram_C5,x                ;4
    cmp     #$7f                    ;2
    bcc     L1ff9                   ;2/3 =  20
L1fe2
    ldx     player                  ;3
    lda     #$00                    ;2
    sta     ram_C2                  ;3
    sta     ram_CF,x                ;4
    lda     #$07                    ;2
    sta     ram_C8                  ;3
    jsr     L1fc6                   ;6
    lda     #$03                    ;2
    sta     ram_82                  ;3
    lda     #$7e                    ;2
    sta     ram_CB                  ;3   =  33
L1ff9
    sta     ram_C5,x                ;4   =   4
L1ffb
    rts                             ;6   =   6

  IF OPTIMIZE
    IF PLUSROM
    ORG     $1ffa
    .word (PlusROM_API) 
    ELSE 
    ORG     $1ffc
    ENDIF
  ENDIF

    .byte   $00,$10                         ; $1ffc (D)
    .byte   $00                             ; $1ffe (*)
    .byte   $10                             ; $1fff (*)

    echo "opt. space:", [_OPTIMIZED]d, "bytes"