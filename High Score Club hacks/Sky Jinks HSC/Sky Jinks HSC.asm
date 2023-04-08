; Disassembled 08/04/22 13:42:17
; Using Stella 6.6
;
; ROM properties name : Sky Jinks (1982) (Activision)
; ROM properties MD5  : 2a0ba55e56e7a596146fa729acf0e109
; ROM properties name : Sky Jinks (1982) (Activision) (PAL)
; ROM properties MD5  : 50a410a5ded0fc9aa6576be45a04f215
; Bankswitch type     : 2K* (2K) 
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

;-----------------------------------------------------------
; A S S E M B L E R - S W I T C H E S
;-----------------------------------------------------------

NTSC                    = 0
PAL50                   = 1
PAL60                   = 2

TRUE                    = 1
FALSE                   = 0

PLUSROM                 = 1

   IFNCONST COMPILE_REGION

COMPILE_REGION         = NTSC       ; change to compile for different regions

   ENDIF

   IF !(COMPILE_REGION = NTSC || COMPILE_REGION = PAL50 || COMPILE_REGION = PAL60)

      echo ""
      echo "*** ERROR: Invalid COMPILE_REGION value"
      echo "*** Valid values: NTSC = 0, PAL50 = 1, PAL60 = 2"
      echo ""
      err

   ENDIF

;-----------------------------------------------------------
; PlusROM hotspots and gameId for HSC backend
;-----------------------------------------------------------
 
   IF PLUSROM

WriteToBuffer           = $1FF0
WriteSendBuffer         = $1FF1
ReceiveBuffer           = $1FF2
ReceiveBufferSize       = $1FF3

HIGHSCORE_ID            = 61         ; Sky Jinks game ID in Highscore DB

   ENDIF

;-----------------------------------------------------------
;      Color constants
;-----------------------------------------------------------

   IF COMPILE_REGION = NTSC

BLACK            = $00
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
GREEN_YELLOW     = $d0
GREEN_BEIGE      = $e0
BEIGE            = $f0

   ELSE

BLACK0           = $00
BLACK1           = $10
YELLOW           = $20
GREEN_YELLOW     = $30
ORANGE           = $40
GREEN            = $50
RED              = $60
CYAN_GREEN       = $70
MAUVE            = $80
CYAN             = $90
VIOLET           = $a0
BLUE_CYAN        = $b0
PURPLE           = $c0
BLUE             = $d0
BLACKE           = $e0
BLACKF           = $f0

   ENDIF

;-----------------------------------------------------------
;      TIA and IO constants accessed
;-----------------------------------------------------------

;CXM0P          = $00  ; (Ri)
;CXM1P          = $01  ; (Ri)
CXP0FB          = $02  ; (R)
CXP1FB          = $03  ; (R)
;CXM0FB         = $04  ; (Ri)
;CXM1FB         = $05  ; (Ri)
;CXBLPF         = $06  ; (Ri)
;INPT0          = $08  ; (Ri)
;INPT1          = $09  ; (Ri)
;INPT2          = $0a  ; (Ri)
;INPT3          = $0b  ; (Ri)
INPT4           = $0c  ; (R)
;INPT5          = $0d  ; (Ri)
;$1e            = $0e  ; (Ri)

VSYNC           = $00  ; (W)
VBLANK          = $01  ; (W)
WSYNC           = $02  ; (W)
NUSIZ0          = $04  ; (W)
NUSIZ1          = $05  ; (W)
COLUP0          = $06  ; (W)
COLUP1          = $07  ; (W)
COLUPF          = $08  ; (W)
;COLUBK         = $09  ; (Wi)
CTRLPF          = $0a  ; (W)
REFP0           = $0b  ; (W)
REFP1           = $0c  ; (W)
PF1             = $0e  ; (W)
PF2             = $0f  ; (W)
RESP0           = $10  ; (W)
;RESP1          = $11  ; (Wi)
;RESM0          = $12  ; (Wi)
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
ENABL           = $1f  ; (W)
HMP0            = $20  ; (W)
HMP1            = $21  ; (W)
;HMM0           = $22  ; (Wi)
HMBL            = $24  ; (W)
VDELP0          = $25  ; (W)
VDELBL          = $27  ; (W)
HMOVE           = $2a  ; (W)
HMCLR           = $2b  ; (W)
CXCLR           = $2c  ; (W)

SWCHA           = $0280
SWCHB           = $0282
INTIM           = $0284
TIM64T          = $0296
$298            = $0298
$299            = $0299
$29b            = $029b
TIM8I           = $029d
T1024I          = $029f

;-----------------------------------------------------------
; U S E R - C O N S T A N T S
;-----------------------------------------------------------

ROM_BASE                = $F000


;-----------------------------------------------------------
;      RIOT RAM (zero-page) labels
;-----------------------------------------------------------

ram_80          = $80
ram_81          = $81
;                 $82  (i)
ram_83          = $83
;                 $84  (i)
ram_85          = $85
;                 $86  (i)
ram_87          = $87
ram_88          = $88
ram_89          = $89
;                 $8a  (i)
ram_8B          = $8b
;                 $8c  (i)
ram_8D          = $8d
;                 $8e  (i)
ram_8F          = $8f
;                 $90  (i)
ram_91          = $91
;                 $92  (i)
ram_93          = $93
ram_94          = $94
ram_95          = $95
ram_96          = $96
ram_97          = $97
ram_98          = $98
gameSelection   = $99   ; gameSelection id (0-4)
ram_9A          = $9a
ram_9B          = $9b
ram_9C          = $9c
pylonsLeft      = $9d   ; Top row gameSelection or pylonsLeft
playerScoresHigh= $9e   ; BCD Score high
ram_9F          = $9f
ram_A0          = $a0
ram_A1          = $a1
playerScoresLow = $a2   ; BCD Score low
ram_A3          = $a3
;                 $a4  (i)
ram_A5          = $a5
playerScoresMid = $a6   ; BCD Score Mid
ram_A7          = $a7
ram_A8          = $a8
ram_A9          = $a9
ram_AA          = $aa
ram_AB          = $ab
ram_AC          = $ac
ram_AD          = $ad
ram_AE          = $ae
ram_AF          = $af
ram_B0          = $b0
ram_B1          = $b1
ram_B2          = $b2
ram_B3          = $b3
ram_B4          = $b4
ram_B5          = $b5
ram_B6          = $b6
ram_B7          = $b7
ram_B8          = $b8
ram_B9          = $b9
ram_BA          = $ba
ram_BB          = $bb
;                 $bc  (i)
ram_BD          = $bd
ram_BE          = $be
ram_BF          = $bf
ram_C0          = $c0
;                 $c1  (i)
ram_C2          = $c2
ram_C3          = $c3
;                 $c4  (i)
;                 $c5  (i)
;                 $c6  (i)
ram_C7          = $c7
ram_C8          = $c8
;                 $c9  (i)
;                 $ca  (i)
;                 $cb  (i)
ram_CC          = $cc
ram_CD          = $cd
;                 $ce  (i)
;                 $cf  (i)
;                 $d0  (i)
ram_D1          = $d1
ram_D2          = $d2
ram_D3          = $d3
ram_D4          = $d4
ram_D5          = $d5
ram_D6          = $d6
ram_D7          = $d7
ram_D8          = $d8
ram_D9          = $d9
;                 $da  (i)

;                 $fb  (s)
;                 $fc  (s)
;                 $fd  (s)
;                 $fe  (s)
;                 $ff  (s)


;-----------------------------------------------------------
;      User Defined Labels
;-----------------------------------------------------------

Start           = $f000


;***********************************************************
;      Bank 0
;***********************************************************

    SEG     CODE
    ORG     ROM_BASE

Start
    sei                             ;2        
    cld                             ;2        
    ldx     #$00                    ;2        
    txa                             ;2   =   8
Lf005
    sta     VSYNC,x                 ;4        
    txs                             ;2        
    inx                             ;2        
    bne     Lf005                   ;2/3      
    jmp     Lf24c                   ;3   =  13
    
Lf00e
    lda     bcd_number_of_pylons,x  ;4        
    sta     pylonsLeft              ;3        
    sta     ram_D3                  ;3        
    ldx     #$16                    ;2   =  12
Lf017
    lda     #$00                    ;2        
    sta     ram_AD,x                ;4        
    sty     playerScoresHigh,x      ;4        
    lda     ram_D2                  ;3        
    sta     ram_A7,x                ;4        
    lda     #$4e                    ;2        
    sta     ram_A9,x                ;4        
    dex                             ;2        
    bpl     Lf017                   ;2/3      
    sta     AUDC0                   ;3        
    rts                             ;6   =  36
    
Lf02b
    lda     ram_9B                  ;3        
    beq     Lf079                   ;2/3 =   5
Lf02f
    lda     ram_9B                  ;3        
    beq     Lf054                   ;2/3 =   5
Lf033
    eor     (ram_87),y              ;5   =   5
Lf035
    and     ram_D6                  ;3        
    sta     COLUPF                  ;3        
    lda     (ram_85),y              ;5        
    sta     HMBL                    ;3        
    tax                             ;2        
    dec     ram_A1                  ;5        
    ldy     ram_A1                  ;3        
    cpy     #$0e                    ;2        
    bcs     Lf092                   ;2/3      
    lda     (ram_81),y              ;5        
    sta     GRP0                    ;3   =  36
Lf04a
    tya                             ;2        
    adc     ram_9F                  ;3        
    cmp     #$0e                    ;2        
    bcs     Lf02f                   ;2/3      
    tay                             ;2        
    lda     (ram_81),y              ;5   =  16
Lf054
    dec     ram_A0                  ;5        
    sta     HMOVE                   ;3        
    bmi     Lf0c9                   ;2/3      
    sta     GRP1                    ;3        
    stx     ENAM0                   ;3        
    txa                             ;2        
    asl                             ;2        
    asl                             ;2        
    sta     CTRLPF                  ;3   =  25
Lf063
    dec     ram_A1                  ;5        
    ldy     ram_A1                  ;3        
    cpy     #$0e                    ;2        
    bcs     Lf097                   ;2/3      
    lda     (ram_81),y              ;5        
    sta     GRP0                    ;3   =  20
Lf06f
    tya                             ;2        
    adc     ram_9F                  ;3        
    cmp     #$0e                    ;2        
    bcs     Lf02b                   ;2/3      
    tay                             ;2        
    lda     (ram_81),y              ;5   =  16
Lf079
    tax                             ;2        
    ldy     ram_A0                  ;3        
    lda     (ram_83),y              ;5        
    sta     HMBL                    ;3        
    asl                             ;2        
    asl                             ;2        
    cpy     #$07                    ;2        
    sta     HMOVE                   ;3        
    sta     CTRLPF                  ;3        
    stx     GRP1                    ;3        
    lda     ram_9A                  ;3        
    bcs     Lf033                   ;2/3      
    lda     ram_D8                  ;3        
    bcc     Lf035                   ;2/3 =  38
Lf092
    cpy     #$ee                    ;2        
    clc                             ;2        
    bne     Lf04a                   ;2/3 =   6
Lf097
    cpy     #$ee                    ;2        
    clc                             ;2        
    bne     Lf06f                   ;2/3      
    beq     Lf101                   ;2/3!=   8
Lf09e
    bne     Lf0a8                   ;2/3      
    lda     ram_D7                  ;3        
    stx     CTRLPF                  ;3   =   8
Lf0a4
    sta     COLUP0                  ;3        
    bcs     Lf0d9                   ;2/3 =   5
Lf0a8
    lda     ram_A5                  ;3        
    cpx     #$8d                    ;2        
    beq     Lf0a4                   ;2/3      
    bne     Lf0d9                   ;2/3 =   9
Lf0b0
    cpx     #$9a                    ;2        
    sta     WSYNC                   ;3   =   5
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     GRP1                    ;3        
    bcs     Lf0cb                   ;2/3      
    cpx     #$88                    ;2        
    bcs     Lf09e                   ;2/3      
    lda     Lf760,x                 ;4        
    sta     PF2                     ;3        
    lda     ram_D8                  ;3        
    eor     #$06                    ;2        
    bcc     Lf0d7                   ;2/3 =  26
Lf0c9
    bmi     Lf128                   ;2/3!=   2
Lf0cb
    lda     Lf6ee,x                 ;4        
    sta     PF1                     ;3        
    lda     Lf73f,x                 ;4        
    and     #$07                    ;2        
    eor     ram_D9                  ;3   =  16
Lf0d7
    sta     COLUPF                  ;3   =   3
Lf0d9
    dec     ram_A1                  ;5        
    ldy     ram_A1                  ;3        
    cpy     #$0e                    ;2        
    bcs     Lf0fa                   ;2/3      
    lda     (ram_81),y              ;5        
    sta     GRP0                    ;3   =  20
Lf0e5
    tya                             ;2        
    adc     ram_9F                  ;3        
    cmp     #$0e                    ;2        
    bcs     Lf123                   ;2/3!     
    tay                             ;2        
    lda     (ram_81),y              ;5   =  16
Lf0ef
    dex                             ;2        
    bmi     Lf0b0                   ;2/3      
    inx                             ;2        
    sta     WSYNC                   ;3   =   9
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     GRP1                    ;3        
    rts                             ;6   =  12
    
Lf0fa
    cpy     #$ee                    ;2        
    clc                             ;2        
    bne     Lf0e5                   ;2/3      
    pla                             ;4        
    pla                             ;4   =  14
Lf101
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    ldx     #$00                    ;2        
    stx     ENAM0                   ;3        
    stx     VDELBL                  ;3        
    stx     ENABL                   ;3        
    stx     VDELP0                  ;3        
    stx     PF1                     ;3        
    stx     COLUPF                  ;3        
    sta     RESBL                   ;3        
    stx     PF2                     ;3        
    lda     #$7a                    ;2        
    sta     HMBL                    ;3        
    sta     CTRLPF                  ;3        
    jsr     Lf5fa                   ;6        
    jmp     Lf5fa                   ;3   =  46
    
Lf123
    lda     #$00                    ;2        
    jmp     Lf0ef                   ;3   =   5
    
Lf128
    stx     ENABL                   ;3        
    sta     GRP1                    ;3   =   6
Lf12c
    ldx     ram_98                  ;3        
    jsr     Lf0d9                   ;6        
    ldy     ram_C2,x                ;4        
    bmi     Lf170                   ;2/3      
    beq     Lf158                   ;2/3 =  17
Lf137
    dey                             ;2        
    bne     Lf137                   ;2/3      
    sta     RESBL                   ;3   =   7
Lf13c
    sty     HMBL                    ;3        
    dec     ram_A1                  ;5        
    ldy     ram_A1                  ;3        
    cpy     #$0e                    ;2        
    bcs     Lf151                   ;2/3      
    lda     (ram_81),y              ;5        
    sta     GRP0                    ;3        
    bcc     Lf186                   ;2/3 =  25
Lf14c
    cpy     #$ee                    ;2        
    clc                             ;2        
    bne     Lf183                   ;2/3 =   6
Lf151
    cpy     #$ee                    ;2        
    beq     Lf101                   ;2/3      
    clc                             ;2        
    bne     Lf186                   ;2/3 =   8
Lf158
    ldy     #$30                    ;2        
    sta     RESBL                   ;3        
    bne     Lf13c                   ;2/3 =   7
Lf15e
    lda     ram_9B                  ;3        
    beq     Lf194                   ;2/3 =   5
Lf162
    jsr     Lf0d9                   ;6        
    lda     ram_A0                  ;3        
    asl                             ;2        
    adc     #$82                    ;2        
    tax                             ;2        
    jsr     Lf0d9                   ;6        
    bne     Lf12c                   ;2/3 =  23
Lf170
    lda     #$00                    ;2        
    dec     ram_A1                  ;5        
    sta     HMBL                    ;3   =  10
Lf176
    dey                             ;2        
    bmi     Lf176                   ;2/3      
    ldy     ram_A1                  ;3        
    cpy     #$0e                    ;2        
    bcs     Lf14c                   ;2/3      
    lda     (ram_81),y              ;5        
    sta     GRP0                    ;3   =  19
Lf183
    sta.w   RESBL                   ;4   =   4
Lf186
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    tya                             ;2        
    adc     ram_9F                  ;3        
    cmp     #$0e                    ;2        
    bcs     Lf15e                   ;2/3      
    tay                             ;2        
    lda     (ram_81),y              ;5   =  19
Lf194
    sta     GRP1                    ;3   =   3
Lf196
    jsr     Lf0d9                   ;6        
    lda     ram_B9                  ;3        
    beq     Lf196                   ;2/3      
    lda     ram_CC,x                ;4        
    sta     ram_A0                  ;3        
    jsr     Lf0d9                   ;6        
    lda     ram_A1                  ;3        
    bmi     Lf1ac                   ;2/3      
    sta     ram_AE                  ;3        
    stx     ram_A3                  ;3   =  35
Lf1ac
    jsr     Lf0d9                   ;6        
    lda     ram_C7,x                ;4        
    jsr     Lf1e6                   ;6        
    lda     ram_B8,x                ;4        
    tax                             ;2        
    jsr     Lf1e6                   ;6        
    dec     ram_98                  ;5        
    lda     Lf6f4,x                 ;4        
    sta     ram_83                  ;3        
    jsr     Lf0d9                   ;6        
    lda     Lf7ec,x                 ;4        
    sta     ram_85                  ;3        
    lda     #$04                    ;2        
    sta     CTRLPF                  ;3        
    jsr     Lf0d9                   ;6        
    cpx     #$06                    ;2        
    beq     Lf162                   ;2/3      
    jsr     Lf0d9                   ;6        
    lda     Lf7e7,x                 ;4        
    sta     ram_87                  ;3        
    jsr     Lf0d9                   ;6        
    ldx     #$02                    ;2        
    stx     ENABL                   ;3        
    jmp     Lf063                   ;3   =  95
    
Lf1e6
    sta     HMBL                    ;3        
    jmp     Lf0d9                   ;3   =   6
    
Lf1eb
    ldy     INTIM                   ;4        
    bne     Lf1eb                   ;2/3      
    ldx     #$ff                    ;2        
    sta     WSYNC                   ;3   =  11
;---------------------------------------
    stx     VSYNC                   ;3        
    inc     ram_80                  ;5        
    bne     Lf200                   ;2/3!     
    inc     ram_B8                  ;5        
    bne     Lf200                   ;2/3!     
    stx     ram_B7                  ;3   =  20 *
Lf200
    txa                             ;2        
    eor     SWCHB                   ;4        
    sta     ram_D5                  ;3        
    and     #$08                    ;2        
    asl                             ;2        
    sbc     #$00                    ;2        
    bit     ram_B7                  ;3        
    bpl     Lf211                   ;2/3      
    and     #$f7                    ;2   =  22 *
Lf211
    sta     ram_D6                  ;3        
    ldx     #$04                    ;2   =   5
Lf215
    lda     ram_B8                  ;3        
    and     ram_B7                  ;3        
    sta     ram_9A                  ;3        
    eor     Lf7a1,x                 ;4        
    and     ram_D6                  ;3        
    sta     ram_D6,x                ;4        
    sta     NUSIZ1,x                ;4        
    dex                             ;2        
    stx     COLUPF                  ;3        
    bne     Lf215                   ;2/3      
    bit     CXP1FB                  ;3        
    bpl     Lf22f                   ;2/3      
    lda     ram_D8                  ;3   =  39
Lf22f
    sta     ram_A5                  ;3        
    lda     ram_D5                  ;3        

    IF COMPILE_REGION = PAL50

    ldx     #$4b                    ;2        

    ELSE

    ldx     #$30                    ;2        

    ENDIF

    lsr                             ;2        
    sta     WSYNC                   ;3   =  13
;---------------------------------------
    sty     VSYNC                   ;3        
    stx     TIM64T                  ;4        
    ldx     gameSelection           ;3        
    bcc     Lf244                   ;2/3      
    jsr     Lf00e                   ;6   =  18
Lf244
    lsr                             ;2        
    bcc     Lf26e                   ;2/3      
    dec     ram_97                  ;5         *
    bpl     Lf270                   ;2/3       *
    inx                             ;2   =  13 *
Lf24c
    cpx     #$05                    ;2        
    bcc     Lf252                   ;2/3      
    ldx     #$00                    ;2   =   6 *
Lf252
    ldy     #$aa                    ;2        
    stx     gameSelection           ;3        
    lda     Lf6f1,x                 ;4        
    bne     Lf260                   ;2/3      
    lda     ram_80                  ;3         *
    bne     Lf260                   ;2/3       *
    tya                             ;2   =  18 *
Lf260
    sta     ram_D2                  ;3        
    jsr     Lf00e                   ;6        
    stx     ram_AF                  ;3        
    ldx     gameSelection           ;3        
    inx                             ;2        
    stx     pylonsLeft              ;3        
    ldy     #$1d                    ;2   =  22
Lf26e
    sty     ram_97                  ;3   =   3
Lf270
    ldy     ram_AD                  ;3        
    lda     INPT4                   ;3        
    asl                             ;2        
    lda     SWCHA                   ;4        
    ror                             ;2        
    cmp     #$f8                    ;2        
    bcs     Lf283                   ;2/3      
    ldx     #$00                    ;2        
    stx     ram_B7                  ;3        
    stx     ram_B8                  ;3   =  26
Lf283
    asl                             ;2        
    ora     ram_AF                  ;3        
    ror                             ;2        
    sta     ram_D4                  ;3        
    bit     ram_B0                  ;3        
    bpl     Lf29d                   ;2/3      
    lda     ram_B1                  ;3        
    asl                             ;2        
    asl                             ;2        
    and     #$38                    ;2        
    tax                             ;2        
    lda     Lf60f,x                 ;4        
    asl                             ;2        
    ror     ram_D4                  ;5        
    tax                             ;2        
    bne     Lf2a7                   ;2/3 =  41
Lf29d
    asl                             ;2        
    ldx     #$be                    ;2        
    asl                             ;2        
    bpl     Lf2ac                   ;2/3      
    bcc     Lf2b1                   ;2/3      
    ldx     #$b0                    ;2   =  12
Lf2a7
    tya                             ;2        
    beq     Lf2b6                   ;2/3      
    bmi     Lf2b1                   ;2/3 =   6
Lf2ac
    dey                             ;2        
    cpy     #$ef                    ;2        
    bne     Lf2b6                   ;2/3 =   6
Lf2b1
    cpy     #$10                    ;2        
    beq     Lf2b6                   ;2/3      
    iny                             ;2   =   6
Lf2b6
    stx     ram_81                  ;3        
    tya                             ;2        
    sta     ram_AD                  ;3        
    ldx     ram_B5                  ;3        
    cpx     #$20                    ;2        
    bcc     Lf2c2                   ;2/3      
    asl                             ;2   =  17
Lf2c2
    ldy     ram_A9                  ;3        
    clc                             ;2        
    adc     ram_B2                  ;3        
    bpl     Lf2d5                   ;2/3 =  10
Lf2c9
    cmp     #$df                    ;2        
    bcs     Lf2d9                   ;2/3      
    adc     #$20                    ;2        
    dey                             ;2        
    bne     Lf2c9                   ;2/3 =  10
Lf2d2
    sbc     #$20                    ;2        
    iny                             ;2   =   4
Lf2d5
    cmp     #$20                    ;2        
    bcs     Lf2d2                   ;2/3 =   4
Lf2d9
    sta     ram_B2                  ;3        
    cpy     #$08                    ;2        
    bcc     Lf2e5                   ;2/3      
    cpy     #$98                    ;2        
    bcs     Lf2e5                   ;2/3      
    sty     ram_A9                  ;3   =  14
Lf2e5
    txa                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    sta     ram_9F                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    adc     ram_A9                  ;3        
    sta     ram_AA                  ;3        
    ldx     ram_BD                  ;3        
    lda     Lf6f4,x                 ;4        
    sta     ram_83                  ;3        
    lda     Lf7ec,x                 ;4        
    sta     ram_85                  ;3        
    lda     ram_9C                  ;3        
    lsr                             ;2        
    tay                             ;2        
    lda     #$00                    ;2        
    sta     ram_95                  ;3        
    beq     Lf315                   ;2/3 =  52
Lf307
    iny                             ;2        
    lda     (ram_83),y              ;5        
    and     #$f0                    ;2        
    adc     (ram_85),y              ;5        
    and     #$f0                    ;2        
    cmp     #$80                    ;2        
    ror                             ;2        
    adc     ram_93                  ;3   =  23
Lf315
    sta     ram_93                  ;3        
    tya                             ;2        
    cmp     Lf7f0,x                 ;4        
    bcc     Lf307                   ;2/3      
    lda     ram_93                  ;3        
    eor     #$f8                    ;2        
    adc     #$08                    ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    cmp     #$10                    ;2        
    bcc     Lf32c                   ;2/3      
    ora     #$e0                    ;2   =  30
Lf32c
    ldx     #$04                    ;2   =   2
Lf32e
    clc                             ;2        
    adc     ram_BE,x                ;4        
    cmp     #$c8                    ;2        
    bcs     Lf33a                   ;2/3      
    sbc     #$9f                    ;2        
    bcs     Lf33c                   ;2/3      
    sec                             ;2   =  16
Lf33a
    adc     #$9f                    ;2   =   2
Lf33c
    jsr     Lf5c4                   ;6        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    sta     ram_C8,x                ;4        
    tya                             ;2        
    sbc     #$05                    ;2        
    bmi     Lf34d                   ;2/3      
    eor     #$80                    ;2        
    tay                             ;2   =  28
Lf34d
    sty     ram_C3,x                ;4        
    ldy     ram_B9,x                ;4        
    lda     Lf7f0,y                 ;4        
    sta     ram_CD,x                ;4        
    tya                             ;2        
    lsr                             ;2        
    eor     #$02                    ;2        
    bne     Lf38f                   ;2/3      
    ldy     ram_BE,x                ;4        
    lda     ram_80                  ;3        
    and     #$03                    ;2        
    ora     ram_AF                  ;3        
    bne     Lf36f                   ;2/3      
    lda     ram_B5                  ;3        
    beq     Lf36f                   ;2/3      
    iny                             ;2        
    bcs     Lf36f                   ;2/3      
    dey                             ;2        
    dey                             ;2   =  51
Lf36f
    cpy     #$a0                    ;2        
    beq     Lf379                   ;2/3      
    bcc     Lf37b                   ;2/3      
    ldy     #$9f                    ;2        
    bne     Lf37b                   ;2/3 =  10
Lf379
    ldy     #$00                    ;2   =   2 *
Lf37b
    sty     ram_BE,x                ;4        
    cpx     #$04                    ;2        
    bne     Lf387                   ;2/3      
    lda     ram_9C                  ;3        
    sbc     #$24                    ;2        
    bmi     Lf38f                   ;2/3 =  15
Lf387
    lda     ram_95                  ;3        
    bne     Lf38f                   ;2/3      
    inc     ram_95                  ;5        
    sty     ram_AB                  ;3   =  13
Lf38f
    lda     #$00                    ;2        
    dex                             ;2        
    bpl     Lf32e                   ;2/3      
    ldx     #$02                    ;2        
    jsr     Lf5e6                   ;6        
    ldy     #$05                    ;2        
    lda     ram_9C                  ;3        
    cmp     #$80                    ;2        
    ror                             ;2        
    sta     ram_D1                  ;3        
    lda     #$00                    ;2        
    rol                             ;2        
    ldx     ram_9C                  ;3        
    bpl     Lf3ad                   ;2/3      
    dey                             ;2        
    txa                             ;2        
    adc     #$0d                    ;2   =  41
Lf3ad
    sta     ram_95                  ;3        
    sty     ram_98                  ;3        
    lda     #$81                    ;2        
    sta     ram_A1                  ;3   =  11
Lf3b5
    ldy     INTIM                   ;4        
    bne     Lf3b5                   ;2/3      
    sta     WSYNC                   ;3   =   9
;---------------------------------------
    sty     VBLANK                  ;3        
    sta     CXCLR                   ;3        
    ldx     #$0a                    ;2        
    ldy     #$50                    ;2        
    sty     ram_8B                  ;3        
    sty     ram_8D                  ;3        
    lda     pylonsLeft              ;3        
    jsr     Lf6e1                   ;6        
    lsr                             ;2        
    bne     Lf3d2                   ;2/3      
    sty     ram_8F                  ;3   =  32
Lf3d2
    jsr     Lf673                   ;6        
    ldx     #$0a                    ;2   =   8
Lf3d7
    lda     ram_9C,x                ;4        
    jsr     Lf6e1                   ;6        
    bpl     Lf3d7                   ;2/3      
    adc     #$c0                    ;2        
    jsr     Lf673                   ;6        
    ldx     #$01                    ;2        
    jsr     Lf5de                   ;6        
    dex                             ;2        
    jsr     Lf5de                   ;6        
    sty     VDELP0                  ;3        
    sty     VDELBL                  ;3        
    bit     ram_D4                  ;3        
    bvc     Lf3f8                   ;2/3      
    sty     REFP0                   ;3        
    sty     REFP1                   ;3   =  53
Lf3f8
    stx     ENABL                   ;3        
    ldx     ram_95                  ;3        
    inx                             ;2        
    sta     HMCLR                   ;3   =  11
Lf3ff
    jsr     Lf0d9                   ;6        
    dex                             ;2        
    bne     Lf3ff                   ;2/3!     
    jsr     Lf12c                   ;6        
    sta     ENABL                   ;3        
    stx     REFP0                   ;3        
    stx     REFP1                   ;3        
    lda     #$3b                    ;2        
    jsr     Lf5e8                   ;6        
    lda     #$43                    ;2        
    inx                             ;2        
    jsr     Lf5e8                   ;6        
    ldx     #$0a                    ;2        
    lda     #$6c                    ;2        
    sec                             ;2   =  49
Lf41e
    ldy     #$f6                    ;2        
    sty     ram_88,x                ;4        
    iny                             ;2        
    sty     ram_80,x                ;4        
    dex                             ;2        
    sta     ram_88,x                ;4        
    sbc     #$07                    ;2        
    dex                             ;2        
    bne     Lf41e                   ;2/3      
    sty     HMP1                    ;3        
    txa                             ;2        
    jsr     Lf673                   ;6        

    IF COMPILE_REGION = PAL50

    lda     #$43                    ;2        

    ELSE

    lda     #$23                    ;2        

    ENDIF

    sta     TIM64T                  ;4        
    jsr     Lf5fa                   ;6        
    sta     VBLANK                  ;3        
    lda     ram_AF                  ;3        
    ora     playerScoresHigh        ;3        
    bmi     Lf446                   ;2/3      
    jsr     Lf6b6                   ;6   =  64
Lf446
    lda     ram_AE                  ;3        
    ldx     ram_A3                  ;3        
    ldy     ram_B8,x                ;4        
    sbc     Lf7f0,y                 ;4        
    sbc     #$06                    ;2        
    cmp     #$0c                    ;2        
    ror                             ;2        
    ora     ram_B0                  ;3        
    bmi     Lf49f                   ;2/3      
    clc                             ;2        
    lda     Lf727,y                 ;4        
    and     #$02                    ;2        
    adc     ram_BD,x                ;4        
    sbc     ram_A9                  ;3        
    ror                             ;2        
    php                             ;3        
    rol                             ;2        
    cmp     Lf6f9,y                 ;4        
    clv                             ;2        
    bcs     Lf46d                   ;2/3      
    bit     CXP0FB                  ;3   =  58
Lf46d
    pla                             ;4        
    eor     Lf64e,y                 ;4        
    ldx     #$3c                    ;2        
    ora     ram_B1                  ;3        
    asl                             ;2        
    bvc     Lf481                   ;2/3      
    stx     ram_B5                  ;3        
    sec                             ;2        
    ror     ram_B0                  ;5        
    ldx     ram_B3                  ;3        
    cpx     #$01                    ;2   =  32
Lf481
    bcs     Lf485                   ;2/3      
    cpy     #$03                    ;2   =   4
Lf485
    lda     #$28                    ;2        
    bvs     Lf48d                   ;2/3      
    bcs     Lf493                   ;2/3      
    lda     #$1f                    ;2   =   8
Lf48d
    sta     ram_B3                  ;3        
    and     #$0a                    ;2        
    sta     AUDC1                   ;3   =   8
Lf493
    lda     pylonsLeft              ;3        
    sed                             ;2        
    sbc     #$00                    ;2        
    cld                             ;2        
    sta     pylonsLeft              ;3        
    lda     #$e1                    ;2        
    sta     ram_B1                  ;3   =  17
Lf49f
    lda     ram_D4                  ;3        
    ldx     ram_B5                  ;3        
    bne     Lf4ac                   ;2/3      
    cmp     #$f8                    ;2        
    bcs     Lf4bb                   ;2/3      
    stx     ram_80                  ;3        
    inx                             ;2   =  17
Lf4ac
    cpx     #$5f                    ;2        
    bcs     Lf4b1                   ;2/3      
    ror                             ;2   =   6
Lf4b1
    ora     ram_AF                  ;3        
    ora     ram_B0                  ;3        
    bmi     Lf4ba                   ;2/3      
    inx                             ;2        
    bpl     Lf4bb                   ;2/3 =  12
Lf4ba
    dex                             ;2   =   2
Lf4bb
    stx     ram_B5                  ;3        
    txa                             ;2        
    beq     Lf4e7                   ;2/3      
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    clc                             ;2        
    pha                             ;3        
    eor     #$8f                    ;2        
    adc     #$88                    ;2        
    sta     AUDF0                   ;3        
    ldx     #$04                    ;2        
    pla                             ;4        
    bit     ram_B0                  ;3        
    bmi     Lf4dc                   ;2/3      
    ldy     ram_AD                  ;3        
    beq     Lf4dc                   ;2/3      
    sbc     #$02                    ;2        
    bcs     Lf4dc                   ;2/3      
    lda     #$01                    ;2   =  47
Lf4dc
    adc     ram_B6                  ;3        
    pha                             ;3        
    and     #$07                    ;2        
    sta     ram_B6                  ;3        
    pla                             ;4        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2   =  21
Lf4e7
    clc                             ;2        
    stx     AUDV0                   ;3        
    sta     ram_93                  ;3        
    adc     ram_9C                  ;3        
    sta     ram_9C                  ;3        
    lda     ram_B1                  ;3        
    clc                             ;2        
    adc     ram_93                  ;3        
    bmi     Lf4fa                   ;2/3      
    lsr                             ;2        
    sta     ram_B0                  ;3   =  29
Lf4fa
    sta     ram_B1                  ;3        
    lda     ram_B3                  ;3        
    beq     Lf502                   ;2/3      
    dec     ram_B3                  ;5   =  13
Lf502
    cmp     #$10                    ;2        
    ldx     #$17                    ;2        
    bcc     Lf50b                   ;2/3      
    dex                             ;2        
    eor     #$1f                    ;2   =  10
Lf50b
    stx     AUDF1                   ;3        
    sta     AUDV1                   ;3        
    lda     ram_B9                  ;3        
    beq     Lf521                   ;2/3      
    ldx     ram_BD                  ;3        
    lda     ram_9C                  ;3        
    clc                             ;2        
    sbc     Lf7f0,x                 ;4        
    clc                             ;2        
    sbc     Lf7f0,x                 ;4        
    bmi     Lf59e                   ;2/3 =  31
Lf521
    ldx     #$00                    ;2        
    sta     ram_93                  ;3   =   5
Lf525
    lda     ram_BF,x                ;4        
    sta     ram_BE,x                ;4        
    lda     ram_BA,x                ;4        
    sta     ram_B9,x                ;4        
    lda     ram_A8                  ;3        
    asl                             ;2        
    eor     ram_A8                  ;3        
    asl                             ;2        
    asl                             ;2        
    rol     ram_A7                  ;5        
    rol     ram_A8                  ;5        
    inx                             ;2        
    cpx     #$04                    ;2        
    bcc     Lf525                   ;2/3      
    ldy     ram_B4                  ;3        
    dey                             ;2        
    bmi     Lf551                   ;2/3      
    bpl     Lf565                   ;2/3 =  53
Lf544
    ldx     #$02                    ;2   =   2

    IF PLUSROM

    jmp SendPlusROMScore
    org $f551

    ELSE

Lf546
    lda     pylonsLeft              ;3        
    clc                             ;2        
    jsr     Lf6c8                   ;6        
    dex                             ;2        
    bpl     Lf546                   ;2/3      
    stx     ram_AF                  ;3   =  18

    ENDIF

Lf551
    lda     ram_D3                  ;3        
    sec                             ;2        
    sed                             ;2        
    sbc     #$01                    ;2        
    cld                             ;2        
    ldy     #$7f                    ;2        
    bcc     Lf565                   ;2/3      
    ldy     #$03                    ;2        
    ldx     ram_B9                  ;3        
    beq     Lf565                   ;2/3      
    dey                             ;2        
    sta     ram_D3                  ;3   =  27
Lf565
    sty     ram_B4                  ;3        
    tya                             ;2        
    bne     Lf5a1                   ;2/3      
    lda     ram_D3                  ;3        
    beq     Lf544                   ;2/3      
    and     #$01                    ;2        
    tay                             ;2        
    iny                             ;2        
    lda     ram_A7                  ;3        
    and     #$1f                    ;2        
    adc     #$1c                    ;2        
    eor     Lf64e,y                 ;4        
    adc     ram_BF                  ;3        
    bpl     Lf587                   ;2/3      
    cmp     #$cc                    ;2        
    and     #$87                    ;2        
    bcc     Lf587                   ;2/3      
    eor     #$98                    ;2   =  42 *
Lf587
    cmp     #$18                    ;2        
    bcs     Lf58d                   ;2/3      
    ora     #$18                    ;2   =   6
Lf58d
    ldx     ram_BB                  ;3        
    bne     Lf593                   ;2/3      
    and     #$e7                    ;2   =   7
Lf593
    sta     ram_C2                  ;3        
    sty     ram_BD                  ;3        
    lda     #$f5                    ;2        
    clc                             ;2        
    adc     ram_93                  ;3        
    sta     ram_9C                  ;3   =  16
Lf59e
    jmp     Lf1eb                   ;3   =   3
    
Lf5a1
    ldy     #$03                    ;2        
    bit     ram_D5                  ;3        
    cmp     #$02                    ;2        
    bcc     Lf5b4                   ;2/3      
    bne     Lf5bc                   ;2/3      
    lda     ram_A8                  ;3        
    and     #$03                    ;2        
    beq     Lf5b4                   ;2/3      
    adc     #$02                    ;2        
    tay                             ;2   =  22
Lf5b4
    lda     ram_A8                  ;3        
    and     #$07                    ;2        
    eor     ram_C0                  ;3        
    bvs     Lf58d                   ;2/3 =  10
Lf5bc
    lda     ram_A7                  ;3        
    and     #$7f                    ;2        
    adc     #$16                    ;2        
    bne     Lf58d                   ;2/3 =   9
Lf5c4
    tay                             ;2        
    iny                             ;2        
    tya                             ;2        
    and     #$0f                    ;2        
    sta     ram_93                  ;3        
    tya                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    tay                             ;2        
    clc                             ;2        
    adc     ram_93                  ;3        
    cmp     #$0f                    ;2        
    bcc     Lf5db                   ;2/3      
    sbc     #$0f                    ;2        
    iny                             ;2   =  36
Lf5db
    eor     #$07                    ;2        
    rts                             ;6   =   8
    
Lf5de
    lda     ram_D7,x                ;4        
    sta     COLUP0,x                ;4        
    ldy     #$28                    ;2        
    sty     NUSIZ0,x                ;4   =  14
Lf5e6
    lda     ram_A9,x                ;4   =   4
Lf5e8
    jsr     Lf5c4                   ;6        
    sta     WSYNC                   ;3   =   9
;---------------------------------------
    sta     HMCLR                   ;3        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    sta     HMP0,x                  ;4   =  15
Lf5f5
    dey                             ;2        
    bpl     Lf5f5                   ;2/3      
    sta     RESP0,x                 ;4   =   8
Lf5fa
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    rts                             ;6   =   9
    
    .byte   $00                             ; $f5ff (*)
    
    .byte   %00011110 ; |   #### |            $f600 (G)
    .byte   %00110011 ; |  ##  ##|            $f601 (G)
    .byte   %00110011 ; |  ##  ##|            $f602 (G)
    .byte   %00110011 ; |  ##  ##|            $f603 (G)
    .byte   %00110011 ; |  ##  ##|            $f604 (G)
    .byte   %00110011 ; |  ##  ##|            $f605 (G)
    .byte   %00011110 ; |   #### |            $f606 (G)
    
    .byte   $e6                             ; $f607 (*)
    
    .byte   %00111111 ; |  ######|            $f608 (G)
    .byte   %00001100 ; |    ##  |            $f609 (G)
    .byte   %00001100 ; |    ##  |            $f60a (G)
    .byte   %00001100 ; |    ##  |            $f60b (G)
    .byte   %00001100 ; |    ##  |            $f60c (G)
    .byte   %00111100 ; |  ####  |            $f60d (G)
    .byte   %00011100 ; |   ###  |            $f60e (G)
    
Lf60f
    .byte   $66                             ; $f60f (D)
    
    .byte   %00111111 ; |  ######|            $f610 (G)
    .byte   %00110000 ; |  ##    |            $f611 (G)
    .byte   %00110000 ; |  ##    |            $f612 (G)
    .byte   %00011110 ; |   #### |            $f613 (G)
    .byte   %00000011 ; |      ##|            $f614 (G)
    .byte   %00100011 ; |  #   ##|            $f615 (G)
    .byte   %00111110 ; |  ##### |            $f616 (G)
    
    .byte   $5f                             ; $f617 (D)
    
    .byte   %00011110 ; |   #### |            $f618 (G)
    .byte   %00100011 ; |  #   ##|            $f619 (G)
    .byte   %00000011 ; |      ##|            $f61a (G)
    .byte   %00000110 ; |     ## |            $f61b (G)
    .byte   %00000011 ; |      ##|            $f61c (G)
    .byte   %00100011 ; |  #   ##|            $f61d (G)
    .byte   %00011110 ; |   #### |            $f61e (G)
    
    .byte   $58                             ; $f61f (D)
    
    .byte   %00000110 ; |     ## |            $f620 (G)
    .byte   %00000110 ; |     ## |            $f621 (G)
    .byte   %00111111 ; |  ######|            $f622 (G)
    .byte   %00100110 ; |  #  ## |            $f623 (G)
    .byte   %00010110 ; |   # ## |            $f624 (G)
    .byte   %00001110 ; |    ### |            $f625 (G)
    .byte   %00000110 ; |     ## |            $f626 (G)
    
    .byte   $df                             ; $f627 (D)
    
    .byte   %00111110 ; |  ##### |            $f628 (G)
    .byte   %00100011 ; |  #   ##|            $f629 (G)
    .byte   %00000011 ; |      ##|            $f62a (G)
    .byte   %00111110 ; |  ##### |            $f62b (G)
    .byte   %00110000 ; |  ##    |            $f62c (G)
    .byte   %00110000 ; |  ##    |            $f62d (G)
    .byte   %00111111 ; |  ######|            $f62e (G)
    
    .byte   $e6                             ; $f62f (D)
    
    .byte   %00011110 ; |   #### |            $f630 (G)
    .byte   %00110011 ; |  ##  ##|            $f631 (G)
    .byte   %00110011 ; |  ##  ##|            $f632 (G)
    .byte   %00111110 ; |  ##### |            $f633 (G)
    .byte   %00110000 ; |  ##    |            $f634 (G)
    .byte   %00110001 ; |  ##   #|            $f635 (G)
    .byte   %00011110 ; |   #### |            $f636 (G)
    
    .byte   $66                             ; $f637 (D)
    
    .byte   %00001100 ; |    ##  |            $f638 (G)
    .byte   %00001100 ; |    ##  |            $f639 (G)
    .byte   %00001100 ; |    ##  |            $f63a (G)
    .byte   %00000110 ; |     ## |            $f63b (G)
    .byte   %00000011 ; |      ##|            $f63c (G)
    .byte   %00100001 ; |  #    #|            $f63d (G)
    .byte   %00111111 ; |  ######|            $f63e (G)
    
    .byte   $5f                             ; $f63f (D)
    
    .byte   %00011110 ; |   #### |            $f640 (G)
    .byte   %00110011 ; |  ##  ##|            $f641 (G)
    .byte   %00110011 ; |  ##  ##|            $f642 (G)
    .byte   %00011110 ; |   #### |            $f643 (G)
    .byte   %00110011 ; |  ##  ##|            $f644 (G)
    .byte   %00110011 ; |  ##  ##|            $f645 (G)
    .byte   %00011110 ; |   #### |            $f646 (G)
    
    .byte   $58                             ; $f647 (D)
    
    .byte   %00011110 ; |   #### |            $f648 (G)
    .byte   %00100011 ; |  #   ##|            $f649 (G)
    .byte   %00000011 ; |      ##|            $f64a (G)
    .byte   %00011111 ; |   #####|            $f64b (G)
    .byte   %00110011 ; |  ##  ##|            $f64c (G)
    .byte   %00110011 ; |  ##  ##|            $f64d (G)
Lf64e
    .byte   %00011110 ; |   #### |            $f64e (G)
    
    .byte   $ff                             ; $f64f (D)
    
    .byte   %00000000 ; |        |            $f650 (G)
    .byte   %00000000 ; |        |            $f651 (G)
    .byte   %00000000 ; |        |            $f652 (G)
    .byte   %00000000 ; |        |            $f653 (G)
    .byte   %00000000 ; |        |            $f654 (G)
    .byte   %00000000 ; |        |            $f655 (G)
    .byte   %00000000 ; |        |            $f656 (G)
    .byte   %10111010 ; |# ### # |            $f657 (G)
    .byte   %10001010 ; |#   # # |            $f658 (G)
    .byte   %10111010 ; |# ### # |            $f659 (G)
    .byte   %10100010 ; |# #   # |            $f65a (G)
    .byte   %00111010 ; |  ### # |            $f65b (G)
    .byte   %10000000 ; |#       |            $f65c (G)
    .byte   %11111110 ; |####### |            $f65d (G)
    .byte   %11101001 ; |### #  #|            $f65e (G)
    .byte   %10101011 ; |# # # ##|            $f65f (G)
    .byte   %10101111 ; |# # ####|            $f660 (G)
    .byte   %10101101 ; |# # ## #|            $f661 (G)
    .byte   %11101001 ; |### #  #|            $f662 (G)
    .byte   %00000000 ; |        |            $f663 (G)
    .byte   %00000000 ; |        |            $f664 (G)
    .byte   %10101101 ; |# # ## #|            $f665 (G)
    .byte   %10101001 ; |# # #  #|            $f666 (G)
    .byte   %11101001 ; |### #  #|            $f667 (G)
    .byte   %10101001 ; |# # #  #|            $f668 (G)
    .byte   %11101101 ; |### ## #|            $f669 (G)
    .byte   %01000001 ; | #     #|            $f66a (G)
    .byte   %00001111 ; |    ####|            $f66b (G)
    
    .byte   $50,$58,$5c,$56,$53,$11,$f0     ; $f66c (D)
    
Lf673
    and     #$c0                    ;2        
    sta     ram_93                  ;3        
    ldy     #$07                    ;2        
    lda     #$03                    ;2        
    sta     NUSIZ0                  ;3        
    lsr                             ;2        
    sta     NUSIZ1                  ;3        
    lda     ram_D9                  ;3        
    sta     COLUP0                  ;3        
    sta     COLUP1                  ;3   =  26
Lf686
    dey                             ;2        
    sty     ram_96                  ;3        
    lda     Lf7da,y                 ;4        
    and     ram_93                  ;3        
    asl                             ;2        
    sta     WSYNC                   ;3   =  17
;---------------------------------------
    ora     (ram_8F),y              ;5        
    sta     GRP1                    ;3        
    lda     (ram_89),y              ;5        
    sta     GRP0                    ;3        
    lda     (ram_91),y              ;5        
    sta     ram_94                  ;3        
    lda     #$00                    ;2        
    ror                             ;2        
    ora     (ram_8B),y              ;5        
    tax                             ;2        
    lda     (ram_8D),y              ;5        
    ldy     ram_94                  ;3        
    sty     GRP0                    ;3        
    stx     GRP1                    ;3        
    sta     GRP0                    ;3        
    ldy     ram_96                  ;3        
    bne     Lf686                   ;2/3      
    sty     GRP0                    ;3        
    sty     GRP1                    ;3        
    rts                             ;6   =  69
    
Lf6b6
    lda     ram_B5                  ;3        
    beq     Lf6e0                   ;2/3      
    lda     ram_AC                  ;3        
    adc     #$ab                    ;2        
    sta     ram_AC                  ;3        
    lda     #$01                    ;2        
    sed                             ;2        
    adc     playerScoresLow         ;3        
    sta     playerScoresLow         ;3        
    tya                             ;2   =  25
Lf6c8
    sed                             ;2        
    adc     playerScoresMid         ;3        
    cld                             ;2        
    ldy     #$ff                    ;2   =   9
Lf6ce
    iny                             ;2        
    bcc     Lf6d3                   ;2/3      
    adc     #$9f                    ;2   =   6
Lf6d3
    cmp     #$60                    ;2        
    bcs     Lf6ce                   ;2/3      
    sta     playerScoresMid         ;3        
    tya                             ;2        
    sed                             ;2        
    adc     playerScoresHigh        ;3        
    sta     playerScoresHigh        ;3        
    cld                             ;2   =  19
Lf6e0
    rts                             ;6   =   6
    
Lf6e1
    pha                             ;3        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    jsr     Lf6ea                   ;6        
    pla                             ;4        
    lsr                             ;2   =  21
Lf6ea
    and     #$78                    ;2        
    sta     ram_87,x                ;4   =   6
Lf6ee
    dex                             ;2        
    dex                             ;2        
    rts                             ;6   =  10
    
Lf6f1
    .byte   $58                             ; $f6f1 (D)
    .byte   $c3,$22                         ; $f6f2 (*)
Lf6f4
    .byte   $bb,$00,$00,$2a,$4b             ; $f6f4 (D)
Lf6f9
    .byte   $4b,$09,$09,$0d,$0b,$0b,$0d,$06 ; $f6f9 (D)
    .byte   $06,$06,$e6,$fa,$fa,$da,$0a,$0a ; $f701 (D)
    .byte   $0a,$0b,$1b,$07,$07,$07,$07,$c5 ; $f709 (D)
    .byte   $0c,$0c,$fc,$0c,$f8,$c0,$d0,$f0 ; $f711 (D)
    .byte   $04,$04,$00,$04,$04,$04,$e1,$d1 ; $f719 (D)
    .byte   $c1,$d1,$f9,$fb,$0d,$0d         ; $f721 (D)
Lf727
    .byte   $0d,$1f,$1b,$f0,$fa,$ea,$1c,$2e ; $f727 (D)
    .byte   $1e,$0a,$f6,$fe,$de,$ef,$ff,$eb ; $f72f (D)
    .byte   $fb,$0b,$07,$01,$f4,$f4,$f4,$08 ; $f737 (D)
Lf73f
    .byte   $08,$08,$2c,$08,$08,$09,$09,$05 ; $f73f (D)
    .byte   $05,$05,$05,$05,$da,$fe,$0e,$0e ; $f747 (D)
    .byte   $1e,$42,$22,$12,$f2,$f6,$e6,$00 ; $f74f (D)
    .byte   $00,$06,$12,$33,$33,$53,$f7,$0b ; $f757 (D)
    .byte   $0f                             ; $f75f (D)
Lf760
    .byte   $0f,$0f,$0f,$0b,$e9,$f4,$18,$e8 ; $f760 (D)
    .byte   $ec,$ec,$1c,$04,$e8,$2c,$3d,$1d ; $f768 (D)

    IF COMPILE_REGION = NTSC

    .byte   $3d,$29,$19,$15,$01,$14,$8e,$00 ; $f770 (D)
    .byte   $8e,$00,$8e,$00,$8e,$00,$8e,$14 ; $f778 (D)
    .byte   $00,$48,$00,$48,$00,$48,$00,$48 ; $f780 (D)

    ELSE

    .byte   $3d,$29,$19,$15,$01,$24,$be,$00 ; $f770 (D)
    .byte   $be,$00,$be,$00,$be,$00,$be,$24 ; $f778 (D)
    .byte   $00,$68,$00,$68,$00,$68,$00,$68 ; $f780 (D)

    ENDIF

    .byte   %00000000 ; |        |            $f788 (P)
    .byte   %01110000 ; | ***    |            $f789 (P)
    .byte   %11111100 ; |******  |            $f78a (P)
    .byte   %11111110 ; |******* |            $f78b (P)
    .byte   %01111111 ; | *******|            $f78c (P)
    .byte   %00111111 ; |  ******|            $f78d (P)
    .byte   %00011110 ; |   **** |            $f78e (P)
    .byte   %00001100 ; |    **  |            $f78f (P)

    IF COMPILE_REGION = NTSC

    .byte   $d2,$d2,$d2,$d2,$d6,$d6,$44,$00 ; $f790 (D)
    .byte   $00,$00,$00,$00,$2c,$2c,$82,$82 ; $f798 (D)
    .byte   $82                             ; $f7a0 (D)
Lf7a1
    .byte   $2c,$2c,$d2,$0c,$d6,$14,$e8,$ea ; $f7a1 (D)
    .byte   $ea,$ea,$ea,$ea,$ea,$fa,$f8     ; $f7a9 (D)

    ELSE

    .byte   $52,$52,$52,$52,$56,$56,$64,$00 ; $f790 (D)
    .byte   $00,$00,$00,$00,$4c,$4c,$b2,$b2 ; $f798 (D)
    .byte   $b2                             ; $f7a0 (D)
Lf7a1
    .byte   $4c,$4c,$52,$0c,$56,$24,$38,$3a ; $f7a1 (D)
    .byte   $3a,$3a,$3a,$3a,$3a,$3a,$28     ; $f7a9 (D)

    ENDIF

    .byte   %00000000 ; |        |            $f7b0 (G)
    .byte   %00011000 ; |   ##   |            $f7b1 (G)
    .byte   %01111110 ; | ###### |            $f7b2 (G)
    .byte   %01111110 ; | ###### |            $f7b3 (G)
    .byte   %00111100 ; |  ####  |            $f7b4 (G)
    .byte   %00011000 ; |   ##   |            $f7b5 (G)
    .byte   %00011000 ; |   ##   |            $f7b6 (G)
    .byte   %00011000 ; |   ##   |            $f7b7 (G)
    .byte   %01111110 ; | ###### |            $f7b8 (G)
    .byte   %11111111 ; |########|            $f7b9 (G)
    .byte   %11111111 ; |########|            $f7ba (G)
    .byte   %11111111 ; |########|            $f7bb (G)
    .byte   %00011000 ; |   ##   |            $f7bc (G)
    .byte   %00111100 ; |  ####  |            $f7bd (G)
    .byte   %00000000 ; |        |            $f7be (G)
    .byte   %00001100 ; |    ##  |            $f7bf (G)
    .byte   %00011110 ; |   #### |            $f7c0 (G)
    .byte   %00111110 ; |  ##### |            $f7c1 (G)
    .byte   %00111100 ; |  ####  |            $f7c2 (G)
    .byte   %00111000 ; |  ###   |            $f7c3 (G)
    .byte   %00011000 ; |   ##   |            $f7c4 (G)
    .byte   %00011100 ; |   ###  |            $f7c5 (G)
    .byte   %00111110 ; |  ##### |            $f7c6 (G)
    .byte   %01111110 ; | ###### |            $f7c7 (G)
    .byte   %01111110 ; | ###### |            $f7c8 (G)
    .byte   %01111000 ; | ####   |            $f7c9 (G)
    .byte   %00011100 ; |   ###  |            $f7ca (G)
    .byte   %00110000 ; |  ##    |            $f7cb (G)
    .byte   %00000000 ; |        |            $f7cc (G)
    .byte   %00011000 ; |   ##   |            $f7cd (G)
    .byte   %00011100 ; |   ###  |            $f7ce (G)
    .byte   %00111100 ; |  ####  |            $f7cf (G)
    .byte   %00111000 ; |  ###   |            $f7d0 (G)
    .byte   %00011000 ; |   ##   |            $f7d1 (G)
    .byte   %00011100 ; |   ###  |            $f7d2 (G)
    .byte   %00011100 ; |   ###  |            $f7d3 (G)
    .byte   %00111100 ; |  ####  |            $f7d4 (G)
    .byte   %00111100 ; |  ####  |            $f7d5 (G)
    .byte   %00111000 ; |  ###   |            $f7d6 (G)
    .byte   %00111000 ; |  ###   |            $f7d7 (G)
    .byte   %00011100 ; |   ###  |            $f7d8 (G)
    .byte   %00110000 ; |  ##    |            $f7d9 (G)
    
Lf7da
    .byte   $86                             ; $f7da (CP)
    .byte   $c6                             ; $f7db (CP)
    .byte   $44                             ; $f7dc (CP)
    .byte   $00                             ; $f7dd (CP)
    .byte   $42                             ; $f7de (CP)
    .byte   $40                             ; $f7df (CP)

    .byte   %00000000 ; |        |            $f7e0 (P)
    .byte   %00001110 ; |    *** |            $f7e1 (P)
    .byte   %00111111 ; |  ******|            $f7e2 (P)
    .byte   %01111111 ; | *******|            $f7e3 (P)
    .byte   %11111110 ; |******* |            $f7e4 (P)
    .byte   %11111100 ; |******  |            $f7e5 (P)
    .byte   %01111000 ; | ****   |            $f7e6 (P)
Lf7e7
    .byte   %00110000 ; |  **    |            $f7e7 (P)
    
    .byte   $6e,$78,$9f,$89                 ; $f7e8 (D)
Lf7ec
    .byte   $89,$3a,$3a,$64                 ; $f7ec (D)
Lf7f0
    .byte   $10,$10,$10,$10,$19,$19,$10     ; $f7f0 (D)
bcd_number_of_pylons
    .byte   $25,$50,$75,$99,$99

    IF PLUSROM

PlusROM_API
    .byte "a", 0, "h.firmaplus.de", 0

SendPlusROMScore
    ; First do the stuff we skiped before jumping here
Lf546
    lda     pylonsLeft              ;3
    clc                             ;2        
    jsr     Lf6c8                   ;6        
    dex                             ;2        
    bpl     Lf546                   ;2/3      
    stx     ram_AF                  ;3   =  18 (end of skipped stuff)
 
    lda gameSelection                ; get current game selection
    sta WriteToBuffer                ; game selection (0-4)
    lda SWCHB
    sta WriteToBuffer                ; game variation (a/b)
    lda playerScoresHigh
    sta WriteToBuffer
    lda playerScoresMid
    sta WriteToBuffer
    lda playerScoresLow
    sta WriteToBuffer
    lda pylonsLeft
    sta WriteToBuffer
    lda #HIGHSCORE_ID                ; game id in Highscore DB
    sta WriteSendBuffer
    jmp Lf551


   .org ROM_BASE + 4096 - 6, 0      ; 4K ROM
   .word (PlusROM_API - $E000)      ; PlusRom API pointer

   ELSE

   .org ROM_BASE + 2048 - 6, 0      ; 2K ROM
    .byte   $99,$99                         ; $fffa (*)

   ENDIF

    .word   Start
    .word   Start
