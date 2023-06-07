; Disassembly of Robot City RC8 (NTSC).bin
; Disassembled 06/05/23 09:37:27
; Using Stella 6.6
;
; ROM properties name : Robot City RC8 (NTSC)
; ROM properties MD5  : ec44dcf2ddb4319962fc43b725a902e8
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


SUPERCHARGER = 0
PLUSROM      = 0

   IF (SUPERCHARGER = 1 && PLUSROM = 1)

      echo ""
      echo "*** ERROR: SUPERCHARGER and PLUSROM is not possible"
      echo ""
      err

   ENDIF

   IF PLUSROM

WriteToBuffer           = $1FF0
WriteSendBuffer         = $1FF1
ReceiveBuffer           = $1FF2
ReceiveBufferSize       = $1FF3

HIGHSCORE_ID            = 64         ; Robot City game ID in Highscore DB

   ENDIF


;-----------------------------------------------------------
;      Color constants
;-----------------------------------------------------------

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


;-----------------------------------------------------------
;      TIA and IO constants accessed
;-----------------------------------------------------------

CXM0P           = $00  ; (R)
CXM1P           = $01  ; (R)
CXP0FB          = $02  ; (R)
;CXP1FB         = $03  ; (Ri)
CXM1FB          = $05  ; (R)
CXBLPF          = $06  ; (R)
CXPPMM          = $07  ; (R)
INPT4           = $0c  ; (R)

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
PF1             = $0e  ; (W)
PF2             = $0f  ; (W)
RESP0           = $10  ; (W)
RESP1           = $11  ; (W)
;RESM0          = $12  ; (Wi)
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
;HMM1           = $23  ; (Wi)
HMBL            = $24  ; (W)
VDELP0          = $25  ; (W)
VDELP1          = $26  ; (W)
VDELBL          = $27  ; (W)
HMOVE           = $2a  ; (W)
HMCLR           = $2b  ; (W)
CXCLR           = $2c  ; (W)
$3c             = $3c  ; (W)

SWCHA           = $0280
SWCHB           = $0282
INTIM           = $0284
TIM64T          = $0296
$298            = $0298
$299            = $0299


;-----------------------------------------------------------
;      RIOT RAM (zero-page) labels
;-----------------------------------------------------------

ram_80          = $80
ram_81          = $81
;                 $82  (i)
;                 $83  (i)
;                 $84  (i)
;                 $85  (i)
;                 $86  (i)
;                 $87  (i)
;                 $88  (i)
;                 $89  (i)
;                 $8a  (i)
;                 $8b  (i)
;                 $8c  (i)
;                 $8d  (i)
ram_8E          = $8e
;                 $8f  (i)
;                 $90  (i)
;                 $91  (i)
;                 $92  (i)
;                 $93  (i)
;                 $94  (i)
;                 $95  (i)
;                 $96  (i)
;                 $97  (i)
;                 $98  (i)
;                 $99  (i)
;                 $9a  (i)
ram_9B          = $9b
ram_9C          = $9c
;                 $9d  (i)
;                 $9e  (i)
;                 $9f  (i)
;                 $a0  (i)
;                 $a1  (i)
;                 $a2  (i)
;                 $a3  (i)
;                 $a4  (i)
;                 $a5  (i)
;                 $a6  (i)
;                 $a7  (i)
ram_A8          = $a8
ram_A9          = $a9
ram_AA          = $aa
ram_AB          = $ab
ram_AC          = $ac
ram_AD          = $ad
;                 $ae  (i)
;                 $af  (i)
;                 $b0  (i)
ram_B1          = $b1; (s)
ram_B2          = $b2
ram_B3          = $b3
ram_B4          = $b4
ram_B5          = $b5
ram_B6          = $b6
ram_B7          = $b7
ram_B8          = $b8
ram_B9          = $b9
ram_BA          = $ba
;                 $bb  (i)
;                 $bc  (i)
;                 $bd  (i)
ram_BE          = $be
ram_BF          = $bf
ram_C0          = $c0
ram_C1          = $c1
ram_C2          = $c2
ram_C3          = $c3
;                 $c4  (i)
ram_C5          = $c5
;                 $c6  (i)
ram_C7          = $c7
;                 $c8  (i)
;                 $c9  (i)
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

;                 $d6  (i)

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
;                 $e3  (i)
ram_E4          = $e4
ram_E5          = $e5
ram_E6          = $e6
;                 $e7  (i)
;                 $e8  (i)
ram_E9          = $e9; (s)
ram_EA          = $ea; (s)
ram_EB          = $eb; (s)
ram_EC          = $ec; (s)
ram_ED          = $ed; (s)
ram_EE          = $ee; (s)
ram_EF          = $ef; (s)
ram_F0          = $f0; (s)
ram_F1          = $f1; (s)
ram_F2          = $f2; (s)
ram_F3          = $f3; (s)
ram_F4          = $f4; (s)
ram_F5          = $f5
;                 $f6  (i)
;                 $f7  (i)
ram_F8          = $f8; (s)
;                 $f9  (is)
;                 $fa  (is)
ram_FB          = $fb; (s)
;                 $fc  (s)
;                 $fd  (s)
ram_FE          = $fe; (s)
;                 $ff  (s)


;-----------------------------------------------------------
;      User Defined Labels
;-----------------------------------------------------------

Start           = $f1dc
Break           = $fb90


;***********************************************************
;      Bank 0
;***********************************************************

    SEG     CODE
    ORG     $f000

Lf000
    ldx     INTIM                   ;4        
    bne     Lf000                   ;2/3      
    sta     WSYNC                   ;3   =   9
;---------------------------------------
    stx     VBLANK                  ;3        
    dex                             ;2        
    stx     PF1                     ;3        
    stx     PF2                     ;3        
    lda     #$c3                    ;2        
    sta     PF0                     ;3        
    ldx     #$0c                    ;2        
    ldy     #$05                    ;2        
    jsr     Lf2c5                   ;6        
    ldy     #$59                    ;2        
    clc                             ;2        
    bne     Lf06a                   ;2/3 =  32
Lf01e
    tya                             ;2        
    sbc     ram_C1                  ;3        
    adc     ram_B6                  ;3        
    lda     ram_9B,x                ;4        
    sta     PF2                     ;3        
    lda     ram_8E,x                ;4        
    sta     PF1                     ;3        
    lda     ram_81,x                ;4        
    sta     PF0                     ;3        
    lda     #$02                    ;2        
    sbc     #$00                    ;2        
    sta     ENAM1                   ;3        
    tya                             ;2        
    sbc     ram_C2                  ;3        
    adc     #$02                    ;2        
    lda     #$02                    ;2        
    sbc     #$00                    ;2        
    sta     ENABL                   ;3        
    tya                             ;2        
    sbc     ram_BF                  ;3        
    adc     #$07                    ;2        
    lda     #$00                    ;2        
    bcc     Lf04b                   ;2/3      
    lda     (ram_EB),y              ;5   =  66
Lf04b
    sec                             ;2        
    pha                             ;3        
    tya                             ;2        
    sbc     ram_B5                  ;3        
    adc     #$07                    ;2        
    bcs     Lf058                   ;2/3      
    lda     #$00                    ;2        
    bcc     Lf05f                   ;2/3 =  18
Lf058
    lda     (ram_EF),y              ;5        
    sta     HMM0                    ;3        
    asl                             ;2        
    asl                             ;2        
    sec                             ;2   =  14
Lf05f
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     NUSIZ0                  ;3        
    pla                             ;4        
    sta     GRP1                    ;3        
    lda     #$03                    ;2   =  15
Lf06a
    adc     #$fe                    ;2        
    sta.w   ENAM0                   ;4        
    dey                             ;2        
    tya                             ;2        
    sbc     ram_BE                  ;3        
    adc     #$07                    ;2        
    bcs     Lf07a                   ;2/3      
    lda     #$00                    ;2        
    .byte   $0c ;NOP                ;4-5 =  18
Lf07a
    lda     (ram_E9),y              ;5        
    sta     GRP0                    ;3        
    tya                             ;2        
    cmp     Lff8d,x                 ;4        
    bne     Lf01e                   ;2/3      
    dex                             ;2        
    bpl     Lf01e                   ;2/3      
    lda     #$ae                    ;2        
    sta     COLUP0                  ;3        
    lda     ram_80                  ;3        
    adc     #$01                    ;2        
    ldy     CXBLPF|$30              ;3        
    sty     ram_CC                  ;3        
    sta     WSYNC                   ;3   =  39
;---------------------------------------
    ldy     #$c0                    ;2        
    sty     PF0                     ;3        
    stx     PF1                     ;3        
    stx     PF2                     ;3   =  11
Lf09d
    sbc     #$0f                    ;2        
    bcs     Lf09d                   ;2/3      
    eor     #$07                    ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    sta     RESBL                   ;3        
    asl                             ;2        
    sta     HMBL                    ;3        
    sty     HMP0                    ;3        
    sta     WSYNC                   ;3   =  26
;---------------------------------------
    lda     #$a7                    ;2        
    sta     HMBL,x                  ;4        
    sta     NUSIZ1                  ;3        
    lda     #$05                    ;2        
    sta     NUSIZ0                  ;3        
    sta     REFP0                   ;3        
    lda     #$f6                    ;2        
    sta     RESM1                   ;3        
    sta     REFP1                   ;3        
    and     ram_DF                  ;3        
    adc     #$04                    ;2        
    sta     ram_E9                  ;3        
    lda     ram_DC                  ;3        
    ldy     ram_D8                  ;3        
    and     #$0c                    ;2        
    beq     Lf0d5                   ;2/3      
    cpy     #$03                    ;2        
    bne     Lf0d6                   ;2/3      
    beq     Lf0d8                   ;2/3 =  49
Lf0d5
    pla                             ;4   =   4
Lf0d6
    lda     #$00                    ;2   =   2
Lf0d8
    eor     ram_E9                  ;3        
    sta     ram_E9                  ;3        
    ldy     #$92                    ;2        
    sta     RESP1                   ;3        
    sta     RESP0                   ;3        
    sta     COLUP1                  ;3        
    sty     HMP1                    ;3        
    lda     ram_80                  ;3        
    cmp     #$04                    ;2        
    ldx     #$20                    ;2        
    sta     HMOVE                   ;3        
    sty     ENAM1                   ;3        
    lda     #$00                    ;2        
    sta     PF0                     ;3        
    sta     PF1                     ;3        
    sta     PF2                     ;3        
    sta     VDELBL                  ;3        
    stx     CTRLPF                  ;3        
    ror                             ;2        
    sta     ram_EA                  ;3        
    ldx     ram_DD                  ;3        
    stx     GRP0                    ;3        
    lda     #$fc                    ;2        
    sta     GRP1                    ;3        
    ldx     #$01                    ;2        
    sta     HMCLR                   ;3   =  71
Lf10b
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    ldy     #$12                    ;2        
    sty     ENABL                   ;3        
    lda     ram_FB                  ;3        
    sta     COLUPF                  ;3        
    lda     ram_EA                  ;3        
    sta     PF0                     ;3        
    lda     ram_F1                  ;3        
    sta     PF1                     ;3        
    lda     ram_F2                  ;3        
    sta     PF2                     ;3        
    lda     ram_F3                  ;3        
    sta     PF0                     ;3        
    lda     ram_F4                  ;3        
    sta     PF1                     ;3        
    lda     #$00                    ;2        
    sta     PF2                     ;3        
    sty     HMBL                    ;3        
    dex                             ;2        
    bpl     Lf10b                   ;2/3      
    sta     WSYNC                   ;3   =  59
;---------------------------------------
    sta     PF0                     ;3        
    sta     PF1                     ;3        
    sta     ENABL                   ;3        
    ldy     ram_E9                  ;3        
    sty     COLUPF                  ;3        
    ldy     #$c1                    ;2        
    sty     CTRLPF                  ;3        
    sta     WSYNC                   ;3   =  23
;---------------------------------------
    sta     ENAM1                   ;3        
    sty     PF0                     ;3        
    stx     PF1                     ;3        
    stx     PF2                     ;3        
    sta     GRP0                    ;3        
    sta     GRP1                    ;3        
    sta     GRP0                    ;3   =  21
Lf154
    ldx     #$f4                    ;2        
    txs                             ;2        
    ldx     #$06                    ;2   =   6
Lf159
    lda     #$fe                    ;2        
    pha                             ;3        
    lda     ram_F4,x                ;4        
    pha                             ;3        
    dex                             ;2        
    bne     Lf159                   ;2/3      
    ldy     ram_CE                  ;3        
    asl     ram_D9                  ;5        
    bcc     Lf16a                   ;2/3      
    ldy     #$05                    ;2   =  28 *
Lf16a
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    stx     PF0                     ;3        
    stx     PF1                     ;3        
    stx     PF2                     ;3        
    lda     Lffc7,y                 ;4        
    sta     COLUP0                  ;3        
    sta     COLUP1                  ;3        
    lda     #$c3                    ;2        
    sta     NUSIZ0                  ;3        
    sta     NUSIZ1                  ;3        
    sta     VDELP1                  ;3        
    ldx     #$d0                    ;2        
    sta     HMP0                    ;3        
    sta     RESP0                   ;3        
    sta     RESP1                   ;3        
    stx     HMP1                    ;3        
    ldy     #$07                    ;2   =  46
Lf18d
    dey                             ;2        
    lda     (ram_F3),y              ;5        
    sta     WSYNC                   ;3   =  10
;---------------------------------------
    sta     HMOVE                   ;3        
    sta.w   GRP0                    ;4        
    lda     (ram_F1),y              ;5        
    sta     GRP1                    ;3        
    lda     (ram_EF),y              ;5        
    sta     GRP0                    ;3        
    LAX     (ram_E9),y              ;5        
    txs                             ;2        
    LAX     (ram_ED),y              ;5        
    lda     (ram_EB),y              ;5        
    stx     GRP1                    ;3        
    sta     GRP0                    ;3        
    tsx                             ;2        
    stx     GRP1                    ;3        
    sty     GRP0                    ;3        
    sta     HMCLR                   ;3        
    tya                             ;2        
    bne     Lf18d                   ;2/3      
    sty     GRP1                    ;3        
    sty     GRP0                    ;3        
    sty     NUSIZ1                  ;3        
    sty     VDELP1                  ;3        
    lda     #$02                    ;2        
    sta     VBLANK                  ;3        
    ldx     #$fd                    ;2        
    txs                             ;2        
    lda     #$25                    ;2        
    sta     TIM64T                  ;4        
    rts                             ;6   =  94
    
Lf1c9
    ldx     #$09                    ;2   =   2
Lf1cb
    lda     INTIM                   ;4        
    bne     Lf1cb                   ;2/3      
    ldy     #$e6                    ;2        
    jsr     Lf2c5                   ;6        
    jsr     Lf9d3                   ;6        
    dex                             ;2        
    bpl     Lf1cb                   ;2/3      
    rts                             ;6   =  30
    
Start
    ldx     #$00                    ;2        
    sec                             ;2   =   4
Lf1df
    cld                             ;2        
    lda     #$00                    ;2   =   4
Lf1e2
    dex                             ;2        
    txs                             ;2        
    pha                             ;3        
    bne     Lf1e2                   ;2/3      
    bcc     Lf1f4                   ;2/3      
    lda     #$02                    ;2        
    sta     ram_CE                  ;3        
    ora     INTIM                   ;4        
    sta     ram_DB                  ;3        
    sta     ram_DE                  ;3   =  26
Lf1f4
    lda     SWCHB                   ;4        
    sta     ram_B5                  ;3   =   7
Lf1f9
    jsr     Lf9ce                   ;6        
    sta     ram_D9                  ;3        
    rol                             ;2        
    sta     CTRLPF                  ;3        
    sta     VDELP0                  ;3        
    ldy     #$1a                    ;2        
    jsr     Lf2c5                   ;6        
    sta     VBLANK                  ;3        
    lda     ram_B6                  ;3        
    cmp     #$1c                    ;2        
    bcc     Lf218                   ;2/3      
    cmp     #$e4                    ;2        
    bcc     Lf21b                   ;2/3      
    eor     #$ff                    ;2        
    adc     #$00                    ;2   =  43
Lf218
    lsr                             ;2        
    lsr                             ;2        
    .byte   $0c ;NOP                ;4-2 =   6
Lf21b
    lda     #$07                    ;2        
    sta     ram_ED                  ;3        
    ldy     #$22                    ;2        
    jsr     Lf2c5                   ;6        
    lda     #$ba                    ;2        
    sta     TIM64T                  ;4        
    lda     ram_ED                  ;3        
    eor     #$07                    ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    adc     ram_ED                  ;3        
    lsr                             ;2        
    tay                             ;2        
    jsr     Lf2c5                   ;6        
    lda     ram_B6                  ;3        
    asl                             ;2        
    asl                             ;2        
    tax                             ;2        
    ldy     #$0f                    ;2   =  56
Lf23f
    txa                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    sta     WSYNC                   ;3   =  13
;---------------------------------------
    tax                             ;2        
    lda     Lff40,x                 ;4        
    tax                             ;2        
    lda     ram_ED                  ;3        
    sta     ram_EE                  ;3        
    bpl     Lf25a                   ;2/3 =  16
Lf251
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    inx                             ;2        
    inx                             ;2        
    pha                             ;3        
    pla                             ;4        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2   =  17
Lf25a
    stx     COLUPF                  ;3        
    lda     Lff00,y                 ;4        
    sta     PF1                     ;3        
    lda     Lff10,y                 ;4        
    sta     PF2                     ;3        
    lda     Lff30,y                 ;4        
    sta     PF1                     ;3        
    lda     Lff20,y                 ;4        
    sta     PF2                     ;3        
    dec     ram_EE                  ;5        
    bpl     Lf251                   ;2/3      
    dey                             ;2        
    bpl     Lf23f                   ;2/3 =  42
Lf277
    lda     INTIM                   ;4        
    bne     Lf277                   ;2/3      
    ldx     #$05                    ;2        
    lda     #$bb                    ;2   =  10
Lf280
    sta     ram_F5,x                ;4        
    dex                             ;2        
    bpl     Lf280                   ;2/3      
    ldy     ram_CE                  ;3        
    lda     Lfd79,y                 ;4        
    sta     ram_F8                  ;3        
    jsr     Lf154                   ;6        
    ldx     ram_B6                  ;3        
    bit     INPT4|$30               ;3        
    bpl     Lf2a1                   ;2/3      
    lda     SWCHB                   ;4        
    cmp     ram_B5                  ;3        
    sta     ram_B5                  ;3        
    beq     Lf2b7                   ;2/3      
    lsr                             ;2         *
    bcs     Lf2a9                   ;2/3 =  48 *
Lf2a1
    cpx     #$e4                    ;2        
    bcs     Lf2bd                   ;2/3      
    ldx     #$e4                    ;2        
    bcc     Lf2be                   ;2/3 =   8
Lf2a9
    lsr                             ;2         *
    bcs     Lf2b7                   ;2/3       *
    ldy     ram_CE                  ;3         *
    iny                             ;2         *
    cpy     #$05                    ;2         *
    bcc     Lf2b5                   ;2/3       *
    ldy     #$00                    ;2   =  15 *
Lf2b5
    sty     ram_CE                  ;3   =   3 *
Lf2b7
    cpx     #$7f                    ;2        
    bne     Lf2bd                   ;2/3      
    ldx     #$43                    ;2   =   6 *
Lf2bd
    inx                             ;2   =   2
Lf2be
    stx     ram_B6                  ;3        
    beq     Lf2cb                   ;2/3      
    jmp     Lf1f9                   ;3   =   8
    
Lf2c5
    dey                             ;2        
    sta     WSYNC                   ;3   =   5
;---------------------------------------
    bne     Lf2c5                   ;2/3      
    rts                             ;6   =   8
    
Lf2cb
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2   =  60
Lf2e9
    ldx     ram_CE                  ;3        
    lda     Lffb8,x                 ;4        
    sta     ram_DD                  ;3        
    ldy     Lfd65,x                 ;4        
    ldx     #$07                    ;2   =  16
Lf2f5
    lda     Lfe0a,y                 ;4        
    sta     ram_E1,x                ;4        
    dey                             ;2        
    stx     ram_CF                  ;3        
    dex                             ;2        
    bne     Lf2f5                   ;2/3!     
    stx     ram_D0                  ;3        
    stx     ram_D1                  ;3        
    stx     ram_D2                  ;3        
    stx     AUDV0                   ;3        
    stx     AUDV1                   ;3        
    stx     ram_DA                  ;3        
    lda     ram_DE                  ;3        
    sta     ram_E0                  ;3        
    lda     ram_DF                  ;3        
    sta     ram_E1                  ;3   =  47
Lf314
    lda     #$fb                    ;2        
    sta     ram_EA                  ;3        
    lda     #$3a                    ;2        
    sta     ram_EF                  ;3        
    ldy     ram_CE                  ;3        
    lda     #$1f                    ;2        
    sta     ram_F0                  ;3        
    and     ram_DE                  ;3   =  21
Lf324
    lsr                             ;2        
    cmp     Lffc2,y                 ;4        
    bcs     Lf324                   ;2/3      
    adc     Lffbd,y                 ;4        
    sta     ram_EE                  ;3        
    ldx     #$19                    ;2        
    lda     #$40                    ;2        
    ldy     #$00                    ;2   =  21
Lf335
    sta     ram_81,x                ;4        
    sty     ram_8E,x                ;4        
    dex                             ;2        
    bpl     Lf335                   ;2/3 =  12
Lf33c
    ldx     #$0b                    ;2   =   2
Lf33e
    inx                             ;2        
    dey                             ;2        
    bmi     Lf356                   ;2/3      
    jsr     Lfbc4                   ;6        
    bne     Lf356                   ;2/3      
    iny                             ;2        
    jsr     Lfbaa                   ;6        
    bne     Lf357                   ;2/3      
    dex                             ;2        
    dex                             ;2        
    jsr     Lfbaa                   ;6        
    beq     Lf39a                   ;2/3      
    inx                             ;2        
    .byte   $0c ;NOP                ;4-2 =  40
Lf356
    iny                             ;2   =   2
Lf357
    dex                             ;2        
    jsr     Lfcb4                   ;6        
    bpl     Lf39e                   ;2/3      
    inx                             ;2        
    jsr     Lfbaa                   ;6        
    beq     Lf371                   ;2/3      
    cpy     #$04                    ;2        
    beq     Lf36c                   ;2/3      
    jsr     Lfbcf                   ;6        
    beq     Lf371                   ;2/3 =  32
Lf36c
    dex                             ;2        
    .byte   $82 ;NOP                ;2-2 =   2
Lf36e
    inx                             ;2        
    bpl     Lf39e                   ;2/3 =   4
Lf371
    dex                             ;2        
    dex                             ;2        
    jsr     Lfbaa                   ;6        
    beq     Lf381                   ;2/3      
    cpy     #$04                    ;2        
    beq     Lf36e                   ;2/3      
    jsr     Lfbc4                   ;6        
    bne     Lf36e                   ;2/3 =  24
Lf381
    inx                             ;2        
    jsr     Lfc9c                   ;6        
    cpy     #$02                    ;2        
    bcc     Lf396                   ;2/3      
    dex                             ;2        
    stx     ram_B5                  ;3        
    sty     ram_B6                  ;3        
    inx                             ;2        
    inx                             ;2        
    jsr     Lfc05                   ;6        
    inx                             ;2        
    bcs     Lf39b                   ;2/3 =  34
Lf396
    dec     ram_EE                  ;5        
    bpl     Lf39e                   ;2/3 =   7
Lf39a
    inx                             ;2   =   2
Lf39b
    jsr     Lfc9c                   ;6   =   6
Lf39e
    dec     ram_EF                  ;5        
    dex                             ;2        
    dex                             ;2        
    bpl     Lf33e                   ;2/3      
    cpy     #$04                    ;2        
    bcs     Lf41a                   ;2/3!     
    ldx     #$0c                    ;2        
    bne     Lf3c6                   ;2/3 =  19
Lf3ac
    jsr     Lfbcf                   ;6        
    bne     Lf3c6                   ;2/3      
    inx                             ;2        
    inx                             ;2        
    jsr     Lfbb7                   ;6        
    php                             ;3        
    dex                             ;2        
    dex                             ;2        
    plp                             ;4        
    bne     Lf3c6                   ;2/3      
    jsr     Lfcbd                   ;6        
    lda     ram_DF                  ;3        
    bmi     Lf410                   ;2/3!     
    sec                             ;2        
    bpl     Lf3cb                   ;2/3 =  46
Lf3c6
    jsr     Lfcb4                   ;6        
    bpl     Lf410                   ;2/3!=   8
Lf3cb
    ror     ram_ED                  ;5        
    cpy     #$03                    ;2        
    bne     Lf3d8                   ;2/3      
    cpx     #$0c                    ;2        
    beq     Lf410                   ;2/3!     
    txa                             ;2        
    beq     Lf410                   ;2/3!=  17
Lf3d8
    lda     #$01                    ;2        
    sta     ram_EB                  ;3        
    jsr     Lfbaa                   ;6        
    beq     Lf3e3                   ;2/3      
    dec     ram_EB                  ;5   =  18
Lf3e3
    jsr     Lfbcf                   ;6        
    beq     Lf3ea                   ;2/3      
    dec     ram_EB                  ;5   =  13
Lf3ea
    jsr     Lfbc4                   ;6        
    beq     Lf3f1                   ;2/3      
    dec     ram_EB                  ;5   =  13
Lf3f1
    bit     ram_EB                  ;3        
    bmi     Lf410                   ;2/3!     
    jsr     Lfc93                   ;6        
    tya                             ;2        
    beq     Lf405                   ;2/3!     
    stx     ram_B5                  ;3        
    sty     ram_B6                  ;3        
    iny                             ;2        
    jsr     Lfc05                   ;6        
    bcs     Lf40d                   ;2/3 =  31
Lf405
    bit     ram_ED                  ;3        
    bmi     Lf410                   ;2/3      
    dec     ram_EE                  ;5        
    bpl     Lf410                   ;2/3 =  12
Lf40d
    jsr     Lfc93                   ;6   =   6 *
Lf410
    dec     ram_EF                  ;5        
    dex                             ;2        
    dex                             ;2        
    bpl     Lf3ac                   ;2/3!     
    iny                             ;2        
    jmp     Lf33c                   ;3   =  16
    
Lf41a
    ldx     #$0b                    ;2   =   2
Lf41c
    lda     ram_8E,x                ;4        
    ora     #$22                    ;2        
    sta     ram_8E,x                ;4        
    lda     ram_9B,x                ;4        
    ora     ram_9C,x                ;4        
    dex                             ;2        
    bmi     Lf42b                   ;2/3      
    ora     ram_9B,x                ;4   =  26
Lf42b
    ora     #$04                    ;2        
    sta     ram_9C,x                ;4        
    dex                             ;2        
    bpl     Lf41c                   ;2/3      
    lda     ram_DF                  ;3        
    and     #$f6                    ;2        
    adc     #$04                    ;2        
    sta     COLUPF                  ;3        
    jsr     Lf1c9                   ;6   =  26
Lf43d
    ror     ram_EC                  ;5        
    ldy     #$0b                    ;2   =   7
Lf441
    ldx     Lfff0,y                 ;4        
    lda     Lfebc,y                 ;4        
    sta     VSYNC,x                 ;4        
    dey                             ;2        
    bpl     Lf441                   ;2/3      
    ldy     ram_CE                  ;3        
    lda     Lfef2,y                 ;4        
    sta     ram_EB                  ;3        
    ldx     #$04                    ;2   =  28
Lf455
    lda     #$20                    ;2        
    tay                             ;2        
    bit     ram_EC                  ;3        
    bmi     Lf460                   ;2/3      
    and     ram_AC,x                ;4        
    bne     Lf474                   ;2/3 =  15
Lf460
    lda     Lffaf,x                 ;4        
    sta     ram_B9,x                ;4        
    lda     #$00                    ;2        
    cpx     ram_EB                  ;3        
    bpl     Lf470                   ;2/3      
    lda     #$0a                    ;2        
    ldy     Lffb3,x                 ;4   =  21
Lf470
    sta     ram_C2,x                ;4        
    sty     ram_AC,x                ;4   =   8
Lf474
    dex                             ;2        
    bne     Lf455                   ;2/3      
    bit     ram_DB                  ;3        
    bpl     Lf485                   ;2/3      
    bvc     Lf47f                   ;2/3      
    ldx     #$02                    ;2   =  13 *
Lf47f
    lda     ram_AD,x                ;4        
    eor     #$08                    ;2        
    sta     ram_AD,x                ;4   =  10
Lf485
    sta     VDELBL                  ;3        
    lda     SWCHB                   ;4        
    lsr                             ;2        
    bcs     Lf498                   ;2/3 =  11
Lf48d
    lda     ram_E0                  ;3         *
    sta     ram_DE                  ;3         *
    lda     ram_E1                  ;3         *
    sta     ram_DF                  ;3         *
    jmp     Lf2e9                   ;3   =  15 *
    
Lf498
    lsr                             ;2        
    bcs     Lf4a0                   ;2/3      
    ldx     #$ce                    ;2         *
    jmp     Lf1df                   ;3   =   9 *
    
Lf4a0
    lda     ram_D8                  ;3        
    cmp     #$04                    ;2        
    bcc     Lf4c4                   ;2/3      
    bit     INPT4|$30               ;3         *
    bpl     Lf48d                   ;2/3       *
    cmp     #$05                    ;2         *
    ldy     #$0c                    ;2         *
    LAX     ram_A8                  ;3         *
    beq     Lf4da                   ;2/3       *
    bcs     Lf4bc                   ;2/3       *
    lsr                             ;2         *
    lsr                             ;2         *
    eor     #$1e                    ;2         *
    ldx     #$08                    ;2         *
    bpl     Lf4d8                   ;2/3 =  33 *
Lf4bc
    lsr                             ;2         *
    tax                             ;2         *
    and     #$02                    ;2         *
    ora     #$04                    ;2         *
    bpl     Lf4d8                   ;2/3 =  10 *
Lf4c4
    ldy     #$07                    ;2        
    LAX     ram_A8                  ;3        
    beq     Lf4da                   ;2/3      
    bpl     Lf4d6                   ;2/3      
    cmp     #$c0                    ;2        
    bcc     Lf4e0                   ;2/3      
    lsr                             ;2        
    lsr                             ;2        
    tax                             ;2        
    iny                             ;2        
    lda     #$17                    ;2   =  23
Lf4d6
    eor     #$0f                    ;2   =   2
Lf4d8
    dec     ram_A8                  ;5   =   5
Lf4da
    sta     AUDF0                   ;3        
    sty     AUDC0                   ;3        
    stx     AUDV0                   ;3   =   9
Lf4e0
    ldy     ram_D8                  ;3        
    beq     Lf4f2                   ;2/3      
    dey                             ;2        
    bne     Lf4f5                   ;2/3      
    lda     SWCHA                   ;4        
    eor     #$ff                    ;2        
    beq     Lf4f2                   ;2/3      
    sty     ram_D8                  ;3        
    sty     ram_AB                  ;3   =  23
Lf4f2
    jmp     Lf5bc                   ;3   =   3
    
Lf4f5
    cpy     #$02                    ;2        
    bne     Lf50a                   ;2/3!     
    lda     ram_80                  ;3        
    beq     Lf50a                   ;2/3!     
    lda     ram_DC                  ;3        
    and     #$03                    ;2        
    bne     Lf508                   ;2/3      
    lda     ram_CE                  ;3        
    jsr     Lfbf2                   ;6   =  25
Lf508
    dec     ram_80                  ;5   =   5
Lf50a
    lda     ram_DC                  ;3        
    and     #$07                    ;2        
    bne     Lf4f2                   ;2/3!     
    dec     ram_AB                  ;5        
    bne     Lf4f2                   ;2/3!     
    dey                             ;2        
    bne     Lf564                   ;2/3      
    lsr     ram_DD                  ;5        
    beq     Lf521                   ;2/3      
    lsr     ram_DD                  ;5        
    clc                             ;2        
    jmp     Lf43d                   ;3   =  35
    
Lf521
  IF PLUSROM = 1
    jmp SendPlusROMScore
    nop
Lf525
  ELSE

    sty     ram_DC                  ;3         *
    lda     ram_DA                  ;3         *

  ENDIF

    bne     Lf55a                   ;2/3       *
    ldx     #$04                    ;2         *
    lda     ram_CE                  ;3         *
    cmp     ram_D3                  ;3         *
    bne     Lf53b                   ;2/3       *
    inx                             ;2   =  20 *
Lf530
    dex                             ;2         *
    beq     Lf55a                   ;2/3       *
    lda     ram_D3,x                ;4         *
    sbc     ram_CE,x                ;4         *
    beq     Lf530                   ;2/3       *
    bcs     Lf55a                   ;2/3 =  16 *
Lf53b
    lda     ram_CE,x                ;4         *
    sta     ram_D3,x                ;4         *
    dex                             ;2         *
    bpl     Lf53b                   ;2/3       *
    nop                             ;2         *
    nop                             ;2         *
    nop                             ;2         *
    nop                             ;2         *
    nop                             ;2         *
    nop                             ;2         *
    nop                             ;2         *
    nop                             ;2         *
    nop                             ;2         *
    nop                             ;2         *
    nop                             ;2         *
    nop                             ;2         *
    nop                             ;2         *
    nop                             ;2         *
    nop                             ;2         *
    nop                             ;2         *
    nop                             ;2         *
    nop                             ;2         *
    nop                             ;2         *
    nop                             ;2         *
    nop                             ;2         *
    lda     #$05                    ;2         *
    .byte   $0c ;NOP                ;4-2 =  58 *
Lf55a
    lda     #$04                    ;2         *
    sta     ram_D8                  ;3         *
    lda     #$5f                    ;2         *
    sta     ram_A8                  ;3         *
    bne     Lf4f2                   ;2/3!=  12 *
Lf564
    dey                             ;2        
    bne     Lf50a                   ;2/3      
    sed                             ;2        
    clc                             ;2        
    lda     ram_CF                  ;3        
    adc     #$01                    ;2        
    bcs     Lf571                   ;2/3      
    sta     ram_CF                  ;3   =  18
Lf571
    cld                             ;2        
    lsr                             ;2        
    bcc     Lf579                   ;2/3      
    rol     ram_DD                  ;5        
    asl     ram_DD                  ;5   =  16
Lf579
    ldx     #$04                    ;2   =   2
Lf57b
    txa                             ;2        
    ldy     ram_CE                  ;3        
    clc                             ;2        
    adc     Lfd65,y                 ;4        
    sbc     #$05                    ;2        
    tay                             ;2        
    lda     Lfe2d,y                 ;4        
    sec                             ;2        
    sbc     ram_E2,x                ;4        
    sta     ram_EB                  ;3        
    lda     Lffd0,x                 ;4        
    pha                             ;3        
    beq     Lf599                   ;2/3      
    lda     Lfe31,y                 ;4        
    sec                             ;2        
    sbc     ram_E6,x                ;4   =  47
Lf599
    lsr                             ;2        
    ror     ram_EB                  ;5        
    lsr                             ;2        
    ror     ram_EB                  ;5        
    lsr                             ;2        
    ror     ram_EB                  ;5        
    lsr                             ;2        
    ror     ram_EB                  ;5        
    sta     ram_EC                  ;3        
    lda     ram_EB                  ;3        
    adc     ram_E2,x                ;4        
    sta     ram_E2,x                ;4        
    pla                             ;4        
    beq     Lf5b6                   ;2/3      
    lda     ram_EC                  ;3        
    adc     ram_E6,x                ;4        
    sta     ram_E6,x                ;4   =  59
Lf5b6
    dex                             ;2        
    bpl     Lf57b                   ;2/3      
    jmp     Lf314                   ;3   =   7
    
Lf5bc
    bit     SWCHB                   ;4        
    bmi     Lf5ca                   ;2/3      
    ldx     #$04                    ;2        
    jsr     Lf9ef                   ;6        
    dex                             ;2        
    jsr     Lf9ef                   ;6   =  22
Lf5ca
    jsr     Lf9e4                   ;6   =   6
Lf5cd
    lda     ram_BA,x                ;4        
    sta.wy  ram_B5,y                ;5        
    lda     ram_C3,x                ;4        
    sta.wy  ram_BE,y                ;5        
    lda     ram_AD,x                ;4        
    and     #$2f                    ;2        
    sta.wy  ram_ED,y                ;5        
    lda     Lffac,x                 ;4        
    sta.wy  COLUP0,y                ;5        
    dex                             ;2        
    dey                             ;2        
    bpl     Lf5cd                   ;2/3      
    ldx     #$01                    ;2   =  46
Lf5ea
    lda     #$1f                    ;2        
    cmp     ram_ED,x                ;4        
    bcs     Lf5fe                   ;2/3      
    ldy     #$09                    ;2        
    lda     ram_DC                  ;3        
    and     #$08                    ;2        
    bne     Lf60d                   ;2/3!     
    lda     ram_CE                  ;3        
    cmp     #$03                    ;2        
    bcc     Lf60d                   ;2/3!=  24
Lf5fe
    lda     ram_ED,x                ;4        
    and     #$df                    ;2        
    cmp     #$09                    ;2        
    bcc     Lf60c                   ;2/3      
    eor     #$ff                    ;2        
    adc     #$10                    ;2        
    dec     ram_B5,x                ;6   =  20
Lf60c
    tay                             ;2   =   2
Lf60d
    lda     #$07                    ;2        
    adc     #$00                    ;2        
    sta     REFP0,x                 ;4        
    lda     ram_DC                  ;3        
    and     #$02                    ;2        
    beq     Lf61d                   ;2/3      
    tya                             ;2        
    adc     #$0a                    ;2        
    tay                             ;2   =  21
Lf61d
    lda     Lfdf6,y                 ;4        
    sbc     ram_BE,x                ;4        
    sta     ram_EA,x                ;4        
    dex                             ;2        
    bpl     Lf5ea                   ;2/3!     
    sta     ram_E9                  ;3        
    ldx     #$04                    ;2   =  21
Lf62b
    lda     ram_B5,x                ;4        
    clc                             ;2        
    sta     WSYNC                   ;3   =   9
;---------------------------------------
Lf630
    sbc     #$0f                    ;2        
    bcs     Lf630                   ;2/3      
    eor     #$07                    ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    sta     HMP0,x                  ;4        
    sta.wx  RESP0,x                 ;5        
    dex                             ;2        
    bpl     Lf62b                   ;2/3      
    sta     WSYNC                   ;3   =  30
;---------------------------------------
    sta     HMOVE                   ;3        
    dex                             ;2        
    stx     ram_F0                  ;3        
    dex                             ;2        
    stx     ram_EA                  ;3        
    stx     ram_EC                  ;3        
    ldx     #$02                    ;2        
    ldy     ram_D8                  ;3        
    cpy     #$04                    ;2        
    bit     ram_DC                  ;3        
    bcs     Lf669                   ;2/3      
    dey                             ;2        
    bne     Lf689                   ;2/3      
    ldy     ram_CE                  ;3        
    lda     ram_DD                  ;3        
    eor     Lffb8,y                 ;4        
    bne     Lf676                   ;2/3      
    ldy     ram_CF                  ;3        
    dey                             ;2        
    bne     Lf676                   ;2/3 =  51
Lf669
    bmi     Lf674                   ;2/3      
    lda     ram_CE                  ;3        
    eor     ram_D3                  ;3        
    bne     Lf674                   ;2/3      
    ldx     #$07                    ;2         *
    clc                             ;2   =  14 *
Lf674
    bvc     Lf689                   ;2/3 =   2
Lf676
    ror     ram_D9                  ;5        
    bit     ram_EA                  ;3        
    lda     ram_CD,x                ;4        
    ldx     #$05                    ;2        
    ldy     #$bb                    ;2   =  16
Lf680
    sty     ram_F5,x                ;4        
    dex                             ;2        
    bpl     Lf680                   ;2/3      
    ldy     #$01                    ;2        
    bne     Lf692                   ;2/3 =  12
Lf689
    ror     ram_D9                  ;5        
    bit     ram_EA                  ;3        
    ldy     #$05                    ;2   =  10
Lf68f
    lda     ram_D0,x                ;4        
    dex                             ;2   =   6
Lf692
    stx     ram_B5                  ;3        
    pha                             ;3        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    sec                             ;2        
    .byte   $0c ;NOP                ;4-4 =  16
Lf69b
    pla                             ;4        
    clc                             ;2        
    and     #$0f                    ;2        
    bvc     Lf6a9                   ;2/3      
    bne     Lf6a8                   ;2/3      
    ldx     #$0a                    ;2        
    tya                             ;2        
    bne     Lf6aa                   ;2/3 =  18
Lf6a8
    clv                             ;2   =   2
Lf6a9
    tax                             ;2   =   2
Lf6aa
    lda     Lfd78,x                 ;4        
    ldx     ram_B5                  ;3        
    sta.wy  ram_F5,y                ;5        
    dey                             ;2        
    bcs     Lf69b                   ;2/3      
    bpl     Lf68f                   ;2/3      
    bit     ram_B4                  ;3        
    lda     ram_DC                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    lda     #$1c                    ;2        
    bvc     Lf6c9                   ;2/3      
    lda     ram_AA                  ;3        
    and     #$01                    ;2        
    beq     Lf6c9                   ;2/3      
    lda     #$0e                    ;2   =  41
Lf6c9
    bcc     Lf6cd                   ;2/3      
    adc     #$06                    ;2   =   4
Lf6cd
    adc     #$d0                    ;2        
    sbc     ram_C0                  ;3        
    sta     ram_EF                  ;3        
    iny                             ;2        
    lda     ram_D8                  ;3        
    cmp     #$04                    ;2        
    bcs     Lf6e5                   ;2/3      
    cmp     #$02                    ;2        
    bne     Lf6e3                   ;2/3      
    lda     ram_AB                  ;3        
    lsr                             ;2        
    bcs     Lf6e5                   ;2/3 =  28
Lf6e3
    ldy     ram_C0                  ;3   =   3
Lf6e5
    sty     ram_B5                  ;3        
    lda     #$15                    ;2        
    sta     CTRLPF                  ;3        
    sta     HMCLR                   ;3        
    lda     ram_80                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    ldy     #$00                    ;2        
    ldx     #$04                    ;2   =  22
Lf6f5
    dex                             ;2        
    sty     ram_F1,x                ;4        
    cmp     Lffe4,x                 ;4        
    bcc     Lf6f5                   ;2/3      
    adc     Lffe8,x                 ;4        
    tay                             ;2        
    lda     Lffd3,y                 ;4   =  22
Lf704
    sta     ram_F1,x                ;4        
    lda     #$ff                    ;2        
    dex                             ;2        
    bpl     Lf704                   ;2/3      
    lda     ram_80                  ;3   =  13
Lf70d
    inx                             ;2        
    cmp     Lfd61,x                 ;4        
    bcs     Lf70d                   ;2/3      
    ldy     Lffec,x                 ;4        
    lda     Lffcd,x                 ;4        
    beq     Lf72d                   ;2/3      
    and     ram_DC                  ;3        
    bne     Lf72d                   ;2/3      
    lda     ram_D8                  ;3        
    bne     Lf72d                   ;2/3      
    lda     ram_A9                  ;3         *
    bmi     Lf72b                   ;2/3       *
    lda     #$cf                    ;2         *
    sta     ram_A9                  ;3   =  38 *
Lf72b
    ldy     #$00                    ;2   =   2 *
Lf72d
    sty     ram_FB                  ;3        
    ldy     ram_D8                  ;3        
    bne     Lf742                   ;2/3      
    lda     ram_AB                  ;3        
    lsr                             ;2        
    ldx     #$07                    ;2        
    bcc     Lf73e                   ;2/3      
    ldx     #$01                    ;2        
    ldy     #$30                    ;2   =  21
Lf73e
    stx     ram_B6                  ;3        
    sty     NUSIZ1                  ;3   =   6
Lf742
    lda     ram_DB                  ;3        
    lsr                             ;2        
    bcc     Lf749                   ;2/3      
    eor     #$b2                    ;2   =   9
Lf749
    sta     ram_DB                  ;3        
    jsr     Lf000                   ;6        
    jsr     Lf9e4                   ;6        
    bit     CXM1P|$30               ;3        
    bvs     Lf758                   ;2/3      
    bpl     Lf75f                   ;2/3      
    dex                             ;2   =  24
Lf758
    cpx     ram_B1                  ;3        
    beq     Lf75f                   ;2/3      
    jsr     Lfbdc                   ;6   =  11
Lf75f
    lda.wy  CXP0FB|$30,y            ;4        
    asl                             ;2        
    bpl     Lf784                   ;2/3      
    lda     ram_AC                  ;3        
    asl                             ;2        
    asl                             ;2        
    sta     ram_EB                  ;3        
    lda     ram_AD,x                ;4        
    and     #$2f                    ;2        
    cmp     #$20                    ;2        
    bcs     Lf784                   ;2/3      
    adc     #$01                    ;2        
    and     #$0f                    ;2        
    sec                             ;2        
    sbc     ram_EB                  ;3        
    cmp     #$03                    ;2        
    bcs     Lf792                   ;2/3      
    jsr     Lfbdc                   ;6        
    jmp     Lf792                   ;3   =  50
    
Lf784
    dex                             ;2        
    dey                             ;2        
    bpl     Lf75f                   ;2/3      
    lda     ram_C2                  ;3        
    cmp     #$5b                    ;2        
    bcs     Lf792                   ;2/3      
    bit     ram_CC                  ;3        
    bpl     Lf796                   ;2/3 =  18
Lf792
    lda     #$00                    ;2        
    sta     ram_C2                  ;3   =   5
Lf796
    lda     ram_C1                  ;3        
    cmp     #$5b                    ;2        
    bcs     Lf7a0                   ;2/3      
    bit     CXM1FB|$30              ;3        
    bpl     Lf7a4                   ;2/3 =  12
Lf7a0
    lda     #$00                    ;2        
    sta     ram_C1                  ;3   =   5
Lf7a4
    lda     ram_D8                  ;3        
    bne     Lf7cf                   ;2/3      
    bit     CXM0P|$30               ;3        
    bvs     Lf7b2                   ;2/3      
    bmi     Lf7b2                   ;2/3      
    bit     CXPPMM|$30              ;3        
    bvc     Lf7bc                   ;2/3 =  17
Lf7b2
    sta     ram_C1                  ;3        
    lda     #$bf                    ;2        
    sta     ram_A9                  ;3        
    lda     #$02                    ;2        
    bne     Lf7c9                   ;2/3 =  12
Lf7bc
    ldx     #$03                    ;2        
    lda     #$20                    ;2   =   4
Lf7c0
    and     ram_AD,x                ;4        
    beq     Lf7d2                   ;2/3      
    dex                             ;2        
    bpl     Lf7c0                   ;2/3      
    lda     #$03                    ;2   =  12
Lf7c9
    sta     ram_D8                  ;3        
    lda     #$10                    ;2        
    sta     ram_AB                  ;3   =   8
Lf7cf
    jmp     Lf975                   ;3   =   3
    
Lf7d2
    bit     SWCHB                   ;4        
    bpl     Lf7db                   ;2/3      
    tax                             ;2         *
    jmp     Lf9c6                   ;3   =  11 *
    
Lf7db
    lda     ram_CA                  ;3        
    sec                             ;2        
    adc     ram_E5                  ;3        
    sta     ram_CA                  ;3        
    lsr                             ;2        
    cmp     ram_E5                  ;3        
    ror     ram_EE                  ;5        
    bmi     Lf850                   ;2/3!     
    lda     #$fb                    ;2        
    sta     ram_EA                  ;3        
    jsr     Lf9e4                   ;6        
    sty     ram_ED                  ;3   =  37
Lf7f2
    stx     ram_EB                  ;3        
    lda     ram_AD,x                ;4        
    tay                             ;2        
    and     #$33                    ;2        
    beq     Lf82b                   ;2/3!     
    cmp     #$20                    ;2        
    bcs     Lf84b                   ;2/3!     
    lda     ram_CD                  ;3        
    eor     Lff58,x                 ;4        
    cmp     ram_CD                  ;3        
    sta     ram_CD                  ;3        
    bcs     Lf84b                   ;2/3      
    tya                             ;2        
    and     #$03                    ;2        
    cmp     #$01                    ;2        
    tya                             ;2        
    bcs     Lf815                   ;2/3      
    and     #$ef                    ;2        
    tay                             ;2   =  46
Lf815
    cpy     #$c0                    ;2        
    bcs     Lf826                   ;2/3      
    and     #$0f                    ;2        
    eor     #$0f                    ;2        
    bne     Lf824                   ;2/3      
    tya                             ;2        
    and     #$90                    ;2        
    tay                             ;2        
    .byte   $82 ;NOP                ;2-2 =  16
Lf824
    iny                             ;2        
    .byte   $82 ;NOP                ;2-2 =   2
Lf826
    dey                             ;2        
    sty     ram_AD,x                ;4        
    bne     Lf84b                   ;2/3 =   8
Lf82b
    tya                             ;2        
    bmi     Lf835                   ;2/3      
    jsr     Lfa1b                   ;6        
    lda     ram_AD,x                ;4        
    bmi     Lf84b                   ;2/3 =  16
Lf835
    and     #$0c                    ;2        
    sta     ram_AD,x                ;4        
    lsr                             ;2        
    lsr                             ;2        
    tay                             ;2        
    txa                             ;2        
    adc     #$05                    ;2        
    tax                             ;2        
    sec                             ;2        
    lda     #$00                    ;2        
    jsr     Lf9fe                   ;6        
    ldx     ram_EB                  ;3        
    jsr     Lfb4a                   ;6   =  37
Lf84b
    dex                             ;2        
    dec     ram_ED                  ;5        
    bpl     Lf7f2                   ;2/3!=   9
Lf850
    ldy     ram_AA                  ;3        
    LAX     SWCHA                   ;4        
    bmi     Lf859                   ;2/3      
    ldy     #$03                    ;2   =  11
Lf859
    asl                             ;2        
    bmi     Lf85e                   ;2/3      
    ldy     #$06                    ;2   =   6
Lf85e
    asl                             ;2        
    asl                             ;2        
    php                             ;3        
    tya                             ;2        
    and     #$f9                    ;2        
    plp                             ;4        
    bpl     Lf86b                   ;2/3      
    .byte   $b0 ;bcs                ;2-8 =  11
Lf868
    SLO     (COLUBK,x)              ;8        
    .byte   $04 ;NOP                ;3-2 =   9
Lf86b
    tay                             ;2   =   2
Lf86c
    sty     ram_AA                  ;3        
    asl     $3c                     ;5        
    bcs     Lf8a0                   ;2/3      
    txa                             ;2        
    ldx     ram_D9                  ;3        
    bmi     Lf883                   ;2/3      
    ldx     #$80                    ;2        
    and     #$f0                    ;2        
    eor     #$f0                    ;2        
    ora     ram_B4                  ;3        
    bne     Lf883                   ;2/3      
    ldx     #$c0                    ;2   =  30
Lf883
    lda     ram_C2                  ;3        
    bne     Lf89f                   ;2/3      
    lda     ram_B7                  ;3        
    adc     #$04                    ;2        
    sta     ram_B9                  ;3        
    lda     ram_C0                  ;3        
    sbc     #$02                    ;2        
    sta     ram_C2                  ;3        
    tya                             ;2        
    lsr                             ;2        
    sta     ram_AC                  ;3        
    bit     ram_A8                  ;3        
    bvs     Lf8a0                   ;2/3      
    lda     #$0f                    ;2        
    sta     ram_A8                  ;3   =  38
Lf89f
    txa                             ;2   =   2
Lf8a0
    sta     ram_D9                  ;3        
    asl                             ;2        
    bpl     Lf8a8                   ;2/3      
    jmp     Lf956                   ;3   =  10
    
Lf8a8
    lda     ram_C7                  ;3        
    sec                             ;2        
    adc     ram_E2                  ;3        
    sta     ram_C7                  ;3        
    bcc     Lf8d4                   ;2/3      
    ldx     #$c0                    ;2        
    ldy     ram_B3                  ;3        
    lda     Lfe6a,y                 ;4        
    bit     ram_B4                  ;3        
    bmi     Lf8c5                   ;2/3      
    bvc     Lf8d4                   ;2/3      
    ldx     #$b7                    ;2        
    ldy     ram_B2                  ;3        
    lda     Lfe59,y                 ;4   =  38
Lf8c5
    cmp     CXM0P,x                 ;4        
    bcc     Lf8cc                   ;2/3      
    inc     VSYNC,x                 ;6        
    .byte   $0c ;NOP                ;4-6 =  10
Lf8cc
    dec     VSYNC,x                 ;6        
    eor     CXM0P,x                 ;4        
    bne     Lf8d4                   ;2/3      
    sta     ram_B4                  ;3   =  15
Lf8d4
    inc     ram_B4                  ;5        
    cpx     #$c0                    ;2        
    bne     Lf915                   ;2/3!=   9
Lf8da
    bit     ram_B4                  ;3        
    bmi     Lf913                   ;2/3!     
    lda     ram_B3                  ;3        
    lsr                             ;2        
    bcs     Lf913                   ;2/3!     
    ldx     ram_B2                  ;3        
    ldy     Lfe59,x                 ;4        
    bit     SWCHA                   ;4        
    bmi     Lf8fd                   ;2/3      
    cpy     ram_B7                  ;3        
    beq     Lf8f3                   ;2/3      
    bcs     Lf913                   ;2/3!=  32
Lf8f3
    brk                             ;7   =   7
    
    jsr     Lfbb7                   ;6        
    bne     Lf913                   ;2/3!     
    inc     ram_B2                  ;5        
    bpl     Lf90f                   ;2/3!=  15
Lf8fd
    bvs     Lf913                   ;2/3!     
    cpy     ram_B7                  ;3        
    bcc     Lf913                   ;2/3      
    txa                             ;2        
    lsr                             ;2        
    bcs     Lf90d                   ;2/3      
    brk                             ;7   =  20
    
    jsr     Lfbaa                   ;6        
    bne     Lf913                   ;2/3 =   8
Lf90d
    dec     ram_B2                  ;5   =   5
Lf90f
    lda     #$40                    ;2        
    bne     Lf94f                   ;2/3 =   4
Lf913
    dec     ram_B4                  ;5   =   5
Lf915
    bit     ram_B4                  ;3        
    bvs     Lf951                   ;2/3      
    lda     ram_B2                  ;3        
    lsr                             ;2        
    bcs     Lf951                   ;2/3      
    ldx     ram_B3                  ;3        
    ldy     Lfe6a,x                 ;4        
    lda     SWCHA                   ;4        
    asl                             ;2        
    asl                             ;2        
    bmi     Lf93c                   ;2/3      
    cpy     ram_C0                  ;3        
    bcc     Lf951                   ;2/3      
    txa                             ;2        
    lsr                             ;2        
    bcs     Lf938                   ;2/3      
    brk                             ;7   =  47
    
    jsr     Lfbc4                   ;6        
    bne     Lf951                   ;2/3 =   8
Lf938
    dec     ram_B3                  ;5        
    bpl     Lf94d                   ;2/3 =   7
Lf93c
    asl                             ;2        
    bmi     Lf951                   ;2/3      
    cpy     ram_C0                  ;3        
    beq     Lf945                   ;2/3      
    bcs     Lf951                   ;2/3 =  11
Lf945
    brk                             ;7   =   7
    
    jsr     Lfbcf                   ;6        
    bne     Lf951                   ;2/3      
    inc     ram_B3                  ;5   =  13
Lf94d
    lda     #$80                    ;2   =   2
Lf94f
    sta     ram_B4                  ;3   =   3
Lf951
    lda     ram_B4                  ;3        
    lsr                             ;2        
    bcs     Lf8da                   ;2/3!=   7
Lf956
    lda     ram_DC                  ;3        
    and     #$0f                    ;2        
    bne     Lf975                   ;2/3      
    lda     ram_CB                  ;3        
    sec                             ;2        
    sbc     ram_E6                  ;3        
    sta     ram_CB                  ;3        
    bcs     Lf975                   ;2/3      
    dec     ram_80                  ;5        
    bne     Lf975                   ;2/3      
    lda     #$02                    ;2         *
    sta     ram_D8                  ;3         *
    lda     #$10                    ;2         *
    sta     ram_AB                  ;3         *
    lda     #$bf                    ;2         *
    sta     ram_A9                  ;3   =  42 *
Lf975
    sta     CXCLR                   ;3        
    lda     ram_D8                  ;3        
    cmp     #$03                    ;2        
    bne     Lf98a                   ;2/3      
    ldy     #$0c                    ;2        
    ldx     ram_80                  ;3        
    beq     Lf9c2                   ;2/3      
    lda     ram_DC                  ;3        
    and     #$06                    ;2        
    tax                             ;2        
    bpl     Lf9c2                   ;2/3 =  26
Lf98a
    lda     ram_A9                  ;3        
    bpl     Lf9a6                   ;2/3      
    dec     ram_A9                  ;5        
    cmp     #$c0                    ;2        
    bcs     Lf99b                   ;2/3      
    lsr                             ;2        
    lsr                             ;2        
    tax                             ;2        
    eor     #$0f                    ;2        
    bpl     Lf9c0                   ;2/3 =  24
Lf99b
    bne     Lf99f                   ;2/3       *
    lsr     ram_A9                  ;5   =   7 *
Lf99f
    tax                             ;2         *
    ldy     #$04                    ;2         *
    lda     #$12                    ;2         *
    bpl     Lf9c2                   ;2/3 =   8 *
Lf9a6
    ldx     #$08                    ;2        
    lda     ram_C1                  ;3        
    beq     Lf9b4                   ;2/3      
    eor     ram_B8                  ;3        
    and     #$05                    ;2        
    ldy     #$08                    ;2        
    bpl     Lf9c2                   ;2/3 =  16
Lf9b4
    bit     ram_EE                  ;3        
    bpl     Lf9b9                   ;2/3      
    tax                             ;2   =   7
Lf9b9
    lda     ram_E5                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    eor     #$1f                    ;2   =  11
Lf9c0
    ldy     #$0f                    ;2   =   2
Lf9c2
    sta     AUDF1                   ;3        
    sty     AUDC1                   ;3   =   6
Lf9c6
    stx     AUDV1                   ;3        
    jsr     Lf9ce                   ;6        
    jmp     Lf485                   ;3   =  12
    
Lf9ce
    lda     INTIM                   ;4        
    bpl     Lf9ce                   ;2/3 =   6
Lf9d3
    lda     #$0e                    ;2   =   2
Lf9d5
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     VSYNC                   ;3        
    lsr                             ;2        
    bne     Lf9d5                   ;2/3      
    ldy     #$23                    ;2        
    sty     TIM64T                  ;4        
    inc     ram_DC                  ;5        
    rts                             ;6   =  24
    
Lf9e4
    lda     #$01                    ;2        
    tax                             ;2        
    tay                             ;2        
    bit     ram_DC                  ;3        
    beq     Lf9ee                   ;2/3      
    ldx     #$03                    ;2   =  13
Lf9ee
    rts                             ;6   =   6
    
Lf9ef
    lda     ram_BE,x                ;4        
    beq     Lfa17                   ;2/3!     
    lda     ram_C5,x                ;4        
    sec                             ;2        
    adc     ram_E0,x                ;4        
    sta     ram_C5,x                ;4        
    lda     ram_E4,x                ;4        
    ldy     ram_A8,x                ;4   =  28
Lf9fe
    dey                             ;2        
    bmi     Lfa12                   ;2/3      
    beq     Lfa0c                   ;2/3      
    sbc     #$00                    ;2        
    eor     #$ff                    ;2        
    cpy     #$02                    ;2        
    bcc     Lfa12                   ;2/3      
    clc                             ;2   =  16
Lfa0c
    iny                             ;2        
    adc     ram_B5,x                ;4        
    sta     ram_B5,x                ;4        
    rts                             ;6   =  16
    
Lfa12
    iny                             ;2        
    adc     ram_BE,x                ;4        
    sta     ram_BE,x                ;4   =  10
Lfa17
    rts                             ;6   =   6
    
Lfa18
    jmp.ind (ram_E9)                ;5   =   5
Lfa1b
    lda     ram_AD,x                ;4        
    sta     ram_EF                  ;3        
    ldy     ram_C1                  ;3        
    bne     Lfa64                   ;2/3      
    lsr                             ;2        
    lsr                             ;2        
    tay                             ;2        
    lsr                             ;2        
    bcc     Lfa3c                   ;2/3      
    lda     ram_C0                  ;3        
    sbc     ram_C3,x                ;4        
    adc     #$04                    ;2        
    cmp     #$09                    ;2        
    bcs     Lfa64                   ;2/3      
    lda     ram_BA,x                ;4        
    cmp     ram_B7                  ;3        
    tya                             ;2        
    eor     #$01                    ;2        
    bpl     Lfa4b                   ;2/3 =  48
Lfa3c
    lda     ram_B7                  ;3        
    sbc     ram_BA,x                ;4        
    adc     #$05                    ;2        
    cmp     #$09                    ;2        
    bcs     Lfa64                   ;2/3      
    lda     ram_C3,x                ;4        
    cmp     ram_C0                  ;3        
    tya                             ;2   =  22
Lfa4b
    bne     Lfa50                   ;2/3      
    bcc     Lfa52                   ;2/3      
    clc                             ;2   =   6
Lfa50
    bcc     Lfa64                   ;2/3 =   2
Lfa52
    sty     ram_AB                  ;3        
    lda     ram_BA,x                ;4        
    adc     Lff9a,y                 ;4        
    sta     ram_B8                  ;3        
    lda     ram_C3,x                ;4        
    adc     Lff9e,y                 ;4        
    sta     ram_C1                  ;3        
    stx     ram_B1                  ;3   =  28
Lfa64
    lda     ram_C3,x                ;4        
    ldy     #$0e                    ;2   =   6
Lfa68
    dey                             ;2        
    dey                             ;2        
    cmp     Lfe6a,y                 ;4        
    bcc     Lfa68                   ;2/3      
    bne     Lfa17                   ;2/3      
    sty     ram_B5                  ;3        
    lda     ram_BA,x                ;4        
    ldy     #$12                    ;2   =  21
Lfa77
    dey                             ;2        
    dey                             ;2        
    cmp     Lfe59,y                 ;4        
    bcc     Lfa77                   ;2/3      
    bne     Lfa17                   ;2/3      
    tya                             ;2        
    lsr                             ;2        
    tay                             ;2        
    ldx     ram_B5                  ;3        
    lda     ram_EF                  ;3        
    cmp     #$04                    ;2        
    beq     Lfa91                   ;2/3      
    jsr     Lfbaa                   ;6        
    sec                             ;2        
    beq     Lfa92                   ;2/3 =  38
Lfa91
    clc                             ;2   =   2
Lfa92
    rol     ram_EC                  ;5        
    lda     ram_EF                  ;3        
    cmp     #$00                    ;2        
    beq     Lfaa0                   ;2/3      
    jsr     Lfbc4                   ;6        
    sec                             ;2        
    beq     Lfaa1                   ;2/3 =  22
Lfaa0
    clc                             ;2   =   2
Lfaa1
    rol     ram_EC                  ;5        
    lda     ram_EF                  ;3        
    cmp     #$0c                    ;2        
    beq     Lfaaf                   ;2/3      
    jsr     Lfbb7                   ;6        
    sec                             ;2        
    beq     Lfab0                   ;2/3 =  22
Lfaaf
    clc                             ;2   =   2
Lfab0
    rol     ram_EC                  ;5        
    lda     ram_EF                  ;3        
    cmp     #$08                    ;2        
    beq     Lfabe                   ;2/3      
    jsr     Lfbcf                   ;6        
    sec                             ;2        
    beq     Lfabf                   ;2/3 =  22
Lfabe
    clc                             ;2   =   2
Lfabf
    rol     ram_EC                  ;5        
    ldx     ram_EB                  ;3        
    ldy     #$05                    ;2        
    lda     ram_B7                  ;3        
    sec                             ;2        
    sbc     ram_BA,x                ;4        
    beq     Lfad5                   ;2/3      
    bcs     Lfad7                   ;2/3      
    ldy     #$00                    ;2        
    sbc     #$00                    ;2        
    eor     #$ff                    ;2        
    .byte   $0c ;NOP                ;4-2 =  31
Lfad5
    ldy     #$08                    ;2   =   2
Lfad7
    sta     ram_B5                  ;3        
    lda     ram_C0                  ;3        
    sec                             ;2        
    sbc     ram_C3,x                ;4        
    beq     Lfae9                   ;2/3      
    bcs     Lfae8                   ;2/3      
    sbc     #$00                    ;2        
    eor     #$ff                    ;2        
    iny                             ;2        
    iny                             ;2   =  24
Lfae8
    iny                             ;2   =   2
Lfae9
    cmp     ram_B5                  ;3        
    bcc     Lfaee                   ;2/3      
    iny                             ;2   =   7
Lfaee
    lda     SWCHA                   ;4        
    ldx     #$01                    ;2        
    lsr                             ;2        
    bcc     Lfb02                   ;2/3!     
    inx                             ;2        
    lsr                             ;2        
    bcc     Lfb03                   ;2/3!     
    inx                             ;2        
    lsr                             ;2        
    bcc     Lfb03                   ;2/3!     
    lsr                             ;2        
    bcs     Lfb0f                   ;2/3      
    dex                             ;2   =  28 *
Lfb02
    dex                             ;2   =   2 *
Lfb03
    lda     Lff58,x                 ;4         *
    and     ram_EC                  ;3         *
    beq     Lfb0f                   ;2/3       *
    sta     ram_DA                  ;3         *
    txa                             ;2         *
    bpl     Lfb3f                   ;2/3 =  16 *
Lfb0f
    lda     ram_CE                  ;3        
    asl                             ;2        
    asl                             ;2        
    sta     ram_B6                  ;3        
    ora     #$03                    ;2        
    tax                             ;2        
    lda     Lff5c,y                 ;4   =  18
Lfb1b
    sta     ram_B5                  ;3        
    and     #$03                    ;2        
    tay                             ;2        
    lda     ram_DB                  ;3        
    cmp     Lff69,x                 ;4        
    bcc     Lfb2e                   ;2/3      
    lda     Lff58,y                 ;4        
    and     ram_EC                  ;3        
    bne     Lfb3e                   ;2/3 =  25
Lfb2e
    lda     ram_B5                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    dex                             ;2        
    cpx     ram_B6                  ;3        
    bpl     Lfb1b                   ;2/3      
    ldy     #$ff                    ;2   =  16
Lfb39
    iny                             ;2        
    lsr     ram_EC                  ;5        
    bcc     Lfb39                   ;2/3 =   9
Lfb3e
    tya                             ;2   =   2
Lfb3f
    ldx     ram_EB                  ;3        
    ora     ram_AD,x                ;4        
    tay                             ;2        
    lda     Lff7d,y                 ;4        
    sta     ram_AD,x                ;4        
    rts                             ;6   =  23
    
Lfb4a
    lda     ram_C3,x                ;4        
    sta     ram_B5                  ;3        
    lda     ram_BA,x                ;4        
    sta     ram_EC                  ;3        
    ldx     #$03                    ;2   =  16
Lfb54
    cpx     ram_EB                  ;3        
    beq     Lfb75                   ;2/3      
    lda     ram_EC                  ;3        
    clc                             ;2        
    adc     Lffa2,y                 ;4        
    sbc     ram_BA,x                ;4        
    cmp     Lffa8,y                 ;4        
    bcs     Lfb75                   ;2/3      
    lda     ram_C3,x                ;4        
    beq     Lfb75                   ;2/3      
    lda     ram_B5                  ;3        
    adc     Lffa3,y                 ;4        
    sbc     ram_C3,x                ;4        
    cmp     Lffa7,y                 ;4        
    bcc     Lfb7b                   ;2/3 =  47
Lfb75
    dex                             ;2        
    bpl     Lfb54                   ;2/3      
    ldx     ram_EB                  ;3        
    rts                             ;6   =  13
    
Lfb7b
    lda     ram_CE                  ;3        
    cmp     #$03                    ;2        
    bcc     Lfb87                   ;2/3      
    lda     ram_AD,x                ;4         *
    and     #$df                    ;2         *
    sta     ram_AD,x                ;4   =  17 *
Lfb87
    ldx     ram_EB                  ;3        
    lda     ram_AD,x                ;4        
    ora     #$91                    ;2        
    sta     ram_AD,x                ;4        
    rts                             ;6   =  19
    
    dec     ram_FE                  ;5        
    lda     ram_B3                  ;3        
    and     #$fe                    ;2        
    tax                             ;2        
    lda     ram_B2                  ;3        
    lsr                             ;2        
    tay                             ;2        
    rti                             ;6   =  25
    
Lfb9c
    cpy     #$06                    ;2        
    bcs     Lfba7                   ;2/3 =   4
Lfba0
    cpy     #$02                    ;2        
    bcc     Lfba7                   ;2/3      
    lda     ram_9B,x                ;4        
    rts                             ;6   =  14
    
Lfba7
    lda     ram_8E,x                ;4        
    rts                             ;6   =  10
    
Lfbaa
    cpy     #$01                    ;2        
    bcc     Lfbb6                   ;2/3      
    dey                             ;2        
    jsr     Lfb9c                   ;6        
    iny                             ;2        
    and     Lff4f,y                 ;4   =  18
Lfbb6
    rts                             ;6   =   6
    
Lfbb7
    cpy     #$07                    ;2        
    bcc     Lfbbd                   ;2/3      
    bne     Lfbc3                   ;2/3 =   6
Lfbbd
    jsr     Lfb9c                   ;6        
    and     Lff50,y                 ;4   =  10
Lfbc3
    rts                             ;6   =   6
    
Lfbc4
    cpx     #$01                    ;2        
    bcc     Lfbdb                   ;2/3      
    dex                             ;2        
    jsr     Lfb9c                   ;6        
    inx                             ;2        
    bne     Lfbd8                   ;2/3 =  16
Lfbcf
    cpx     #$0b                    ;2        
    bcs     Lfbdb                   ;2/3      
    inx                             ;2        
    jsr     Lfb9c                   ;6        
    dex                             ;2   =  14
Lfbd8
    and     Lfe50,y                 ;4   =   4
Lfbdb
    rts                             ;6   =   6
    
Lfbdc
    lda     ram_AD,x                ;4        
    ora     #$20                    ;2        
    cmp     ram_AD,x                ;4        
    beq     Lfc04                   ;2/3!     
    sta     ram_AD,x                ;4        
    lda     #$ff                    ;2        
    sta     ram_A8                  ;3        
    lda     ram_CE                  ;3        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    adc     #$0a                    ;2   =  34
Lfbf2
    sed                             ;2        
    adc     ram_D0                  ;3        
    sta     ram_D0                  ;3        
    lda     #$00                    ;2        
    adc     ram_D1                  ;3        
    sta     ram_D1                  ;3        
    lda     #$00                    ;2        
    adc     ram_D2                  ;3        
    sta     ram_D2                  ;3        
    cld                             ;2   =  26
Lfc04
    rts                             ;6   =   6
    
Lfc05
    lda     #$00                    ;2   =   2
Lfc07
    sta     ram_EB                  ;3        
    beq     Lfc17                   ;2/3      
    ldy     #$03                    ;2   =   7
Lfc0d
    ldx     #$0c                    ;2   =   2
Lfc0f
    jsr     Lfba0                   ;6        
    and     Lfe50,y                 ;4        
    beq     Lfc3f                   ;2/3 =  12
Lfc17
    dey                             ;2        
    bmi     Lfc1f                   ;2/3      
    lda     #$b7                    ;2        
    jsr     Lfc62                   ;6   =  12
Lfc1f
    iny                             ;2        
    iny                             ;2        
    lda     #$aa                    ;2        
    jsr     Lfc62                   ;6        
    dey                             ;2        
    dex                             ;2        
    bmi     Lfc31                   ;2/3      
    dex                             ;2        
    lda     #$cf                    ;2        
    jsr     Lfc62                   ;6        
    inx                             ;2   =  30
Lfc31
    inx                             ;2        
    cpx     #$0c                    ;2        
    bcs     Lfc3f                   ;2/3      
    inx                             ;2        
    inx                             ;2        
    lda     #$c4                    ;2        
    jsr     Lfc62                   ;6        
    dex                             ;2        
    dex                             ;2   =  22
Lfc3f
    dex                             ;2        
    dex                             ;2        
    bpl     Lfc0f                   ;2/3      
    dey                             ;2        
    bpl     Lfc0d                   ;2/3      
    tya                             ;2        
    cpy     ram_EB                  ;3        
    bne     Lfc07                   ;2/3 =  17
Lfc4b
    ldx     #$0c                    ;2   =   2
Lfc4d
    lda     ram_8E,x                ;4        
    and     #$77                    ;2        
    sta     ram_8E,x                ;4        
    lda     ram_9B,x                ;4        
    and     #$6e                    ;2        
    sta     ram_9B,x                ;4        
    dex                             ;2        
    dex                             ;2        
    bpl     Lfc4d                   ;2/3      
    ldx     ram_B5                  ;3        
    ldy     ram_B6                  ;3        
    rts                             ;6   =  38
    
Lfc62
    sta     ram_E9                  ;3        
    lda     Lfe50,y                 ;4        
    cpy     #$02                    ;2        
    bcs     Lfc6f                   ;2/3      
    and     ram_8E,x                ;4        
    bcc     Lfc71                   ;2/3 =  17
Lfc6f
    and     ram_9B,x                ;4   =   4
Lfc71
    bne     Lfc8d                   ;2/3      
    jsr     Lfa18                   ;6        
    bne     Lfc8d                   ;2/3      
    cpx     ram_B5                  ;3        
    bne     Lfc80                   ;2/3      
    cpy     ram_B6                  ;3        
    beq     Lfc8e                   ;2/3 =  20
Lfc80
    sta     ram_EB                  ;3        
    lda     Lfe50,y                 ;4        
    cpy     #$02                    ;2        
    bcs     Lfcaf                   ;2/3      
    ora     ram_8E,x                ;4        
    sta     ram_8E,x                ;4   =  19
Lfc8d
    rts                             ;6   =   6
    
Lfc8e
    pla                             ;4        
    pla                             ;4        
    clc                             ;2        
    bcc     Lfc4b                   ;2/3 =  12
Lfc93
    lda     Lfd6c,y                 ;4        
    pha                             ;3        
    lda     Lfd6a,y                 ;4        
    bpl     Lfcaa                   ;2/3 =  13
Lfc9c
    lda     Lfeba,y                 ;4        
    eor     ram_81,x                ;4        
    sta     ram_81,x                ;4        
    lda     Lfd73,y                 ;4        
    pha                             ;3        
    lda     Lfd70,y                 ;4   =  23
Lfcaa
    eor     ram_8E,x                ;4        
    sta     ram_8E,x                ;4        
    pla                             ;4   =  12
Lfcaf
    eor     ram_9B,x                ;4        
    sta     ram_9B,x                ;4        
    rts                             ;6   =  14
    
Lfcb4
    lda     ram_EF                  ;3        
    asl                             ;2        
    cmp     ram_F0                  ;3        
    bcs     Lfcbd                   ;2/3      
    lsr     ram_F0                  ;5   =  15
Lfcbd
    lda     ram_DF                  ;3        
    lsr                             ;2        
    ror     ram_DE                  ;5        
    bcc     Lfcc6                   ;2/3      
    eor     #$b4                    ;2   =  14
Lfcc6
    sta     ram_DF                  ;3        
    eor     ram_DE                  ;3        
    and     ram_F0                  ;3        
    cmp     ram_EF                  ;3        
    bcc     Lfcd2                   ;2/3      
    sbc     ram_EF                  ;3   =  17
Lfcd2
    cmp     ram_EE                  ;3        
    rts                             ;6   =   9
    
  IF SUPERCHARGER = 1 || PLUSROM = 1

Lfff0
    .byte   $19,$1a,$d9,$c1,$b4,$a8,$b7,$c0
    .byte   $b2,$b3,$80,$d8

    IF PLUSROM = 1
PlusROM_API
    .byte "a", 0, "h.firmaplus.de", 0

SendPlusROMScore
    lda ram_CE                       ; Game variant (0-4)
    sta WriteToBuffer                ; 
    lda ram_D2                       ; Score Hi BCD
    sta WriteToBuffer                ; 
    lda ram_D1                       ; Score Mid BCD
    sta WriteToBuffer                ; 
    lda ram_D0                       ; Score Lo BCD
    sta WriteToBuffer                ; 
    lda ram_CF                       ; Level
    sta WriteToBuffer                ; 
    lda #HIGHSCORE_ID                ; game id in Highscore DB
    sta WriteSendBuffer

    sty     ram_DC                  ;3         *
    lda     ram_DA                  ;3         *

    jmp Lf525

    ENDIF
  ELSE
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fcd5 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fcdd (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fce5 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fced (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fcf5 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fcfd (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fd05 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fd0d (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fd15 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fd1d (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fd25 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fd2d (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fd35 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fd3d (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fd45 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fd4d (*)
    .byte   $00,$00,$00,$00,$00,$4a,$54,$5a ; $fd55 (*)
    .byte   $52,$43,$38,$30                 ; $fd5d (*)
  ENDIF

    ORG     $fd61

Lfd61
    .byte   $0c,$17,$23,$74                 ; $fd61 (D)
Lfd65
    .byte   $06,$0d                         ; $fd65 (*)
    .byte   $14                             ; $fd67 (D)
    .byte   $1b,$22                         ; $fd68 (*)
Lfd6a
    .byte   $20,$02                         ; $fd6a (D)
Lfd6c
    .byte   $00,$00,$04,$40                 ; $fd6c (D)
Lfd70
    .byte   $e0,$3e,$03                     ; $fd70 (D)
Lfd73
    .byte   $00,$00,$07,$3c,$c0             ; $fd73 (D)
Lfd78
    .byte   $98                             ; $fd78 (D)
Lfd79
    .byte   $77,$9e,$b3,$8b,$84,$ac,$7e,$a5 ; $fd79 (D)
    .byte   $92,$bb                         ; $fd81 (D)
    
    .byte   %00101000 ; |  # #   |            $fd83 (G)
    .byte   %00000000 ; |        |            $fd84 (G)
    .byte   %00110010 ; |  ##  # |            $fd85 (G)
    .byte   %00011010 ; |   ## # |            $fd86 (G)
    .byte   %00110010 ; |  ##  # |            $fd87 (G)
    .byte   %00000000 ; |        |            $fd88 (G)
    .byte   %00101000 ; |  # #   |            $fd89 (G)
    .byte   %00000000 ; |        |            $fd8a (G)
    .byte   %00010010 ; |   #  # |            $fd8b (G)
    .byte   %00011000 ; |   ##   |            $fd8c (G)
    .byte   %10111010 ; |# ### # |            $fd8d (G)
    .byte   %00000000 ; |        |            $fd8e (G)
    .byte   %00101000 ; |  # #   |            $fd8f (G)
    .byte   %00000000 ; |        |            $fd90 (G)
    .byte   %10111010 ; |# ### # |            $fd91 (G)
    .byte   %00011000 ; |   ##   |            $fd92 (G)
    .byte   %00010010 ; |   #  # |            $fd93 (G)
    .byte   %00000000 ; |        |            $fd94 (G)
    .byte   %00101000 ; |  # #   |            $fd95 (G)
    .byte   %00000000 ; |        |            $fd96 (G)
    .byte   %10000010 ; |#     # |            $fd97 (G)
    .byte   %00000000 ; |        |            $fd98 (G)
    .byte   %10000010 ; |#     # |            $fd99 (G)
    .byte   %00000000 ; |        |            $fd9a (G)
    .byte   %00101000 ; |  # #   |            $fd9b (G)
    .byte   %00000100 ; |     #  |            $fd9c (G)
    .byte   %10011010 ; |#  ## # |            $fd9d (G)
    .byte   %00111000 ; |  ###   |            $fd9e (G)
    .byte   %00010010 ; |   #  # |            $fd9f (G)
    .byte   %00000000 ; |        |            $fda0 (G)
    .byte   %00001000 ; |    #   |            $fda1 (G)
    .byte   %00000000 ; |        |            $fda2 (G)
    .byte   %00010010 ; |   #  # |            $fda3 (G)
    .byte   %00111000 ; |  ###   |            $fda4 (G)
    .byte   %10011010 ; |#  ## # |            $fda5 (G)
    .byte   %00000100 ; |     #  |            $fda6 (G)
    .byte   %00101000 ; |  # #   |            $fda7 (G)
    .byte   %00000000 ; |        |            $fda8 (G)
    .byte   %10011010 ; |#  ## # |            $fda9 (G)
    .byte   %00111000 ; |  ###   |            $fdaa (G)
    .byte   %10001010 ; |#   # # |            $fdab (G)
    .byte   %00000000 ; |        |            $fdac (G)
    .byte   %00001000 ; |    #   |            $fdad (G)
    .byte   %00000000 ; |        |            $fdae (G)
    .byte   %10001010 ; |#   # # |            $fdaf (G)
    .byte   %00111000 ; |  ###   |            $fdb0 (G)
    .byte   %10011010 ; |#  ## # |            $fdb1 (G)
    .byte   %00000000 ; |        |            $fdb2 (G)
    .byte   %00101000 ; |  # #   |            $fdb3 (G)
    .byte   %00111000 ; |  ###   |            $fdb4 (G)
    .byte   %00000000 ; |        |            $fdb5 (G)
    .byte   %10010010 ; |#  #  # |            $fdb6 (G)
    .byte   %00111000 ; |  ###   |            $fdb7 (G)
    .byte   %10101010 ; |# # # # |            $fdb8 (G)
    .byte   %00000000 ; |        |            $fdb9 (G)
    .byte   %00000000 ; |        |            $fdba (G)
    .byte   %10101010 ; |# # # # |            $fdbb (G)
    .byte   %00111000 ; |  ###   |            $fdbc (G)
    .byte   %10010010 ; |#  #  # |            $fdbd (G)
    .byte   %00000000 ; |        |            $fdbe (G)
    .byte   %00111000 ; |  ###   |            $fdbf (G)
    .byte   %00000000 ; |        |            $fdc0 (G)
    .byte   %00000100 ; |     #  |            $fdc1 (G)
    .byte   %00001000 ; |    #   |            $fdc2 (G)
    .byte   %10111010 ; |# ### # |            $fdc3 (G)
    .byte   %00011000 ; |   ##   |            $fdc4 (G)
    .byte   %01000100 ; | #   #  |            $fdc5 (G)
    .byte   %00011000 ; |   ##   |            $fdc6 (G)
    .byte   %10111010 ; |# ### # |            $fdc7 (G)
    .byte   %00001000 ; |    #   |            $fdc8 (G)
    .byte   %00000100 ; |     #  |            $fdc9 (G)
    .byte   %00000000 ; |        |            $fdca (G)
    .byte   %01000100 ; | #   #  |            $fdcb (G)
    .byte   %00101000 ; |  # #   |            $fdcc (G)
    .byte   %10111010 ; |# ### # |            $fdcd (G)
    .byte   %00010000 ; |   #    |            $fdce (G)
    .byte   %01000100 ; | #   #  |            $fdcf (G)
    .byte   %00010000 ; |   #    |            $fdd0 (G)
    .byte   %10111010 ; |# ### # |            $fdd1 (G)
    .byte   %00101000 ; |  # #   |            $fdd2 (G)
    .byte   %01000100 ; | #   #  |            $fdd3 (G)
    .byte   %00000000 ; |        |            $fdd4 (G)
    .byte   %00010000 ; |   #    |            $fdd5 (G)
    .byte   %01000100 ; | #   #  |            $fdd6 (G)
    .byte   %00111010 ; |  ### # |            $fdd7 (G)
    .byte   %00011010 ; |   ## # |            $fdd8 (G)
    .byte   %00010000 ; |   #    |            $fdd9 (G)
    .byte   %00000100 ; |     #  |            $fdda (G)
    .byte   %00010000 ; |   #    |            $fddb (G)
    .byte   %10111010 ; |# ### # |            $fddc (G)
    .byte   %00011000 ; |   ##   |            $fddd (G)
    .byte   %01000100 ; | #   #  |            $fdde (G)
    .byte   %00010000 ; |   #    |            $fddf (G)
    .byte   %01000100 ; | #   #  |            $fde0 (G)
    .byte   %00000000 ; |        |            $fde1 (G)
    .byte   %10000010 ; |#     # |            $fde2 (G)
    .byte   %00000000 ; |        |            $fde3 (G)
    .byte   %01000100 ; | #   #  |            $fde4 (G)
    .byte   %00010000 ; |   #    |            $fde5 (G)
    .byte   %01000100 ; | #   #  |            $fde6 (G)
    .byte   %00011000 ; |   ##   |            $fde7 (G)
    .byte   %10111010 ; |# ### # |            $fde8 (G)
    .byte   %00010000 ; |   #    |            $fde9 (G)
    .byte   %00000100 ; |     #  |            $fdea (G)
    .byte   %00010000 ; |   #    |            $fdeb (G)
    .byte   %00011010 ; |   ## # |            $fdec (G)
    .byte   %00111010 ; |  ### # |            $fded (G)
    .byte   %01000100 ; | #   #  |            $fdee (G)
    .byte   %00010000 ; |   #    |            $fdef (G)
    .byte   %01000100 ; | #   #  |            $fdf0 (G)
    .byte   %00110000 ; |  ##    |            $fdf1 (G)
    .byte   %00011010 ; |   ## # |            $fdf2 (G)
    .byte   %00110000 ; |  ##    |            $fdf3 (G)
    .byte   %01000100 ; | #   #  |            $fdf4 (G)
    .byte   %00010000 ; |   #    |            $fdf5 (G)
    
Lfdf6
    .byte   $d2,$b5,$e1,$91,$f7,$97,$ed,$af ; $fdf6 (D)
    .byte   $d6,$e7,$c1,$c8,$a9,$f1,$8b,$dd ; $fdfe (D)
    .byte   $a3,$cc,$bc,$9d                 ; $fe06 (D)
Lfe0a
    .byte   $4e,$2f,$8a,$25,$66,$01,$01,$5f ; $fe0a (*)
    .byte   $7f,$df,$2f,$56,$01,$01         ; $fe12 (*)
    .byte   $5f,$7f,$df,$2f,$40,$01,$01     ; $fe18 (D)
    .byte   $5f,$7f,$df,$2f,$33,$01,$01,$71 ; $fe1f (*)
    .byte   $df,$39,$3b,$40,$01,$02         ; $fe27 (*)
Lfe2d
    .byte   $87,$41,$a6,$47                 ; $fe2d (*)
Lfe31
    .byte   $c3,$02,$02,$a4,$d9,$39,$5a,$a4 ; $fe31 (*)
    .byte   $02,$03                         ; $fe39 (*)
    .byte   $a4,$41,$39,$5a,$7b,$02,$03     ; $fe3b (D)
    .byte   $a4,$d9,$39,$5a,$62,$02,$03,$c3 ; $fe42 (*)
    .byte   $8f,$d3,$71,$7b,$03,$03         ; $fe4a (*)
Lfe50
    .byte   $80,$08,$01,$10,$80,$10,$01,$10 ; $fe50 (D)
    .byte   $80                             ; $fe58 (D)
Lfe59
    .byte   $0f,$17,$1f,$27,$2f,$37,$3f,$46 ; $fe59 (D)
    .byte   $4d,$54,$5b,$63,$6b,$73,$7b,$83 ; $fe61 (D)
    .byte   $8b                             ; $fe69 (D)
Lfe6a
    .byte   $0a                             ; $fe6a (D)
    .byte   $10                             ; $fe6b (*)
    .byte   $17,$1d,$24,$2a,$31,$37,$3e,$44 ; $fe6c (D)
    .byte   $4b,$51,$58                     ; $fe74 (D)
    
    .byte   %00111110 ; |  ##### |            $fe77 (G)
    .byte   %00111110 ; |  ##### |            $fe78 (G)
    .byte   %00111110 ; |  ##### |            $fe79 (G)
    .byte   %00001000 ; |    #   |            $fe7a (G)
    .byte   %00001000 ; |    #   |            $fe7b (G)
    .byte   %00001000 ; |    #   |            $fe7c (G)
    .byte   %00011000 ; |   ##   |            $fe7d (G)
    .byte   %00001110 ; |    ### |            $fe7e (G)
    .byte   %00001110 ; |    ### |            $fe7f (G)
    .byte   %00001110 ; |    ### |            $fe80 (G)
    .byte   %00001110 ; |    ### |            $fe81 (G)
    .byte   %00000010 ; |      # |            $fe82 (G)
    .byte   %00000010 ; |      # |            $fe83 (G)
    .byte   %01111110 ; | ###### |            $fe84 (G)
    .byte   %00001110 ; |    ### |            $fe85 (G)
    .byte   %00001110 ; |    ### |            $fe86 (G)
    .byte   %01111110 ; | ###### |            $fe87 (G)
    .byte   %01000000 ; | #      |            $fe88 (G)
    .byte   %01000000 ; | #      |            $fe89 (G)
    .byte   %01111100 ; | #####  |            $fe8a (G)
    .byte   %00001110 ; |    ### |            $fe8b (G)
    .byte   %00001110 ; |    ### |            $fe8c (G)
    .byte   %01111110 ; | ###### |            $fe8d (G)
    .byte   %01000100 ; | #   #  |            $fe8e (G)
    .byte   %01000100 ; | #   #  |            $fe8f (G)
    .byte   %01000000 ; | #      |            $fe90 (G)
    .byte   %01000000 ; | #      |            $fe91 (G)
    .byte   %00001110 ; |    ### |            $fe92 (G)
    .byte   %00001110 ; |    ### |            $fe93 (G)
    .byte   %00001110 ; |    ### |            $fe94 (G)
    .byte   %01111110 ; | ###### |            $fe95 (G)
    .byte   %01000010 ; | #    # |            $fe96 (G)
    .byte   %01000010 ; | #    # |            $fe97 (G)
    .byte   %01111110 ; | ###### |            $fe98 (G)
    .byte   %01001110 ; | #  ### |            $fe99 (G)
    .byte   %01001110 ; | #  ### |            $fe9a (G)
    .byte   %01001110 ; | #  ### |            $fe9b (G)
    .byte   %01000010 ; | #    # |            $fe9c (G)
    .byte   %01000010 ; | #    # |            $fe9d (G)
    .byte   %01111110 ; | ###### |            $fe9e (G)
    .byte   %01110000 ; | ###    |            $fe9f (G)
    .byte   %01110000 ; | ###    |            $fea0 (G)
    .byte   %01111110 ; | ###### |            $fea1 (G)
    .byte   %00000010 ; |      # |            $fea2 (G)
    .byte   %00000010 ; |      # |            $fea3 (G)
    .byte   %00111110 ; |  ##### |            $fea4 (G)
    .byte   %01111110 ; | ###### |            $fea5 (G)
    .byte   %01110010 ; | ###  # |            $fea6 (G)
    .byte   %01110010 ; | ###  # |            $fea7 (G)
    .byte   %01111110 ; | ###### |            $fea8 (G)
    .byte   %00100100 ; |  #  #  |            $fea9 (G)
    .byte   %00100100 ; |  #  #  |            $feaa (G)
    .byte   %00111100 ; |  ####  |            $feab (G)
    .byte   %01111110 ; | ###### |            $feac (G)
    .byte   %01110010 ; | ###  # |            $fead (G)
    .byte   %01110010 ; | ###  # |            $feae (G)
    .byte   %01111110 ; | ###### |            $feaf (G)
    .byte   %01000000 ; | #      |            $feb0 (G)
    .byte   %01000000 ; | #      |            $feb1 (G)
    .byte   %01111100 ; | #####  |            $feb2 (G)
    .byte   %01111110 ; | ###### |            $feb3 (G)
    .byte   %00001110 ; |    ### |            $feb4 (G)
    .byte   %00001110 ; |    ### |            $feb5 (G)
    .byte   %00111110 ; |  ##### |            $feb6 (G)
    .byte   %00000100 ; |     #  |            $feb7 (G)
    .byte   %00000100 ; |     #  |            $feb8 (G)
    .byte   %00111100 ; |  ####  |            $feb9 (G)
    
Lfeba
    .byte   $80                             ; $feba (D)
    
    .byte   %00000000 ; |        |            $febb (G)
Lfebc
    .byte   %00000000 ; |        |            $febc (G)
    .byte   %00000000 ; |        |            $febd (G)
    .byte   %00000000 ; |        |            $febe (G)
    .byte   %00000000 ; |        |            $febf (G)
    .byte   %00000000 ; |        |            $fec0 (G)
    .byte   %00000000 ; |        |            $fec1 (G)
    
    .byte   $8b,$58,$10,$0c,$73,$01         ; $fec2 (D)
    
    .byte   %00011000 ; |   ##   |            $fec8 (G)
    .byte   %11100000 ; |###     |            $fec9 (G)
    .byte   %11111000 ; |#####   |            $feca (G)
    .byte   %00101100 ; |  # ##  |            $fecb (G)
    .byte   %00011100 ; |   ###  |            $fecc (G)
    .byte   %00010000 ; |   #    |            $fecd (G)
    .byte   %11001000 ; |##  #   |            $fece (G)
    .byte   %00011000 ; |   ##   |            $fecf (G)
    .byte   %11100000 ; |###     |            $fed0 (G)
    .byte   %11111000 ; |#####   |            $fed1 (G)
    .byte   %00101100 ; |  # ##  |            $fed2 (G)
    .byte   %00101100 ; |  # ##  |            $fed3 (G)
    .byte   %11000000 ; |##      |            $fed4 (G)
    .byte   %00001000 ; |    #   |            $fed5 (G)
    .byte   %00101000 ; |  # #   |            $fed6 (G)
    .byte   %11110000 ; |####    |            $fed7 (G)
    .byte   %11011000 ; |## ##   |            $fed8 (G)
    .byte   %11101100 ; |### ##  |            $fed9 (G)
    .byte   %01011100 ; | # ###  |            $feda (G)
    .byte   %00010000 ; |   #    |            $fedb (G)
    .byte   %10101000 ; |# # #   |            $fedc (G)
    .byte   %00101000 ; |  # #   |            $fedd (G)
    .byte   %11110000 ; |####    |            $fede (G)
    .byte   %11011000 ; |## ##   |            $fedf (G)
    .byte   %11101100 ; |### ##  |            $fee0 (G)
    .byte   %01101100 ; | ## ##  |            $fee1 (G)
    .byte   %11000000 ; |##      |            $fee2 (G)
    .byte   %11101000 ; |### #   |            $fee3 (G)
    .byte   %11100000 ; |###     |            $fee4 (G)
    .byte   %11110100 ; |#### #  |            $fee5 (G)
    .byte   %00001000 ; |    #   |            $fee6 (G)
    .byte   %00011000 ; |   ##   |            $fee7 (G)
    .byte   %00010100 ; |   # #  |            $fee8 (G)
    .byte   %11000000 ; |##      |            $fee9 (G)
    .byte   %11111000 ; |#####   |            $feea (G)
    .byte   %00010000 ; |   #    |            $feeb (G)
    .byte   %11110100 ; |#### #  |            $feec (G)
    .byte   %00001000 ; |    #   |            $feed (G)
    .byte   %00011000 ; |   ##   |            $feee (G)
    .byte   %00000100 ; |     #  |            $feef (G)
    .byte   %00010000 ; |   #    |            $fef0 (G)
    .byte   %10111000 ; |# ###   |            $fef1 (G)
    
Lfef2
    .byte   $03,$04                         ; $fef2 (*)
    .byte   $05                             ; $fef4 (D)
    .byte   $05,$05,$00,$00,$00,$00,$00,$00 ; $fef5 (*)
    .byte   $00,$00,$00                     ; $fefd (*)
    
Lff00
    .byte   %00000000 ; |        |            $ff00 (P)
    .byte   %00000011 ; |      **|            $ff01 (P)
    .byte   %00000011 ; |      **|            $ff02 (P)
    .byte   %00000011 ; |      **|            $ff03 (P)
    .byte   %00000011 ; |      **|            $ff04 (P)
    .byte   %00000010 ; |      * |            $ff05 (P)
    .byte   %00000010 ; |      * |            $ff06 (P)
    .byte   %00000011 ; |      **|            $ff07 (P)
    .byte   %00000000 ; |        |            $ff08 (P)
    .byte   %01100101 ; | **  * *|            $ff09 (P)
    .byte   %01100101 ; | **  * *|            $ff0a (P)
    .byte   %01100101 ; | **  * *|            $ff0b (P)
    .byte   %01111101 ; | ***** *|            $ff0c (P)
    .byte   %01001001 ; | *  *  *|            $ff0d (P)
    .byte   %01001001 ; | *  *  *|            $ff0e (P)
    .byte   %01111001 ; | ****  *|            $ff0f (P)
Lff10
    .byte   %00000000 ; |        |            $ff10 (P)
    .byte   %00110111 ; |  ** ***|            $ff11 (P)
    .byte   %00110100 ; |  ** *  |            $ff12 (P)
    .byte   %00110000 ; |  **    |            $ff13 (P)
    .byte   %00110000 ; |  **    |            $ff14 (P)
    .byte   %00010000 ; |   *    |            $ff15 (P)
    .byte   %00010100 ; |   * *  |            $ff16 (P)
    .byte   %11010111 ; |** * ***|            $ff17 (P)
    .byte   %00000000 ; |        |            $ff18 (P)
    .byte   %11101111 ; |*** ****|            $ff19 (P)
    .byte   %01101000 ; | ** *   |            $ff1a (P)
    .byte   %01101000 ; | ** *   |            $ff1b (P)
    .byte   %11101100 ; |*** **  |            $ff1c (P)
    .byte   %00101100 ; |  * **  |            $ff1d (P)
    .byte   %00101100 ; |  * **  |            $ff1e (P)
    .byte   %11101111 ; |*** ****|            $ff1f (P)
Lff20
    .byte   %00000000 ; |        |            $ff20 (P)
    .byte   %11000011 ; |**    **|            $ff21 (P)
    .byte   %11000011 ; |**    **|            $ff22 (P)
    .byte   %11000011 ; |**    **|            $ff23 (P)
    .byte   %11001111 ; |**  ****|            $ff24 (P)
    .byte   %10001000 ; |*   *   |            $ff25 (P)
    .byte   %10001000 ; |*   *   |            $ff26 (P)
    .byte   %11101000 ; |*** *   |            $ff27 (P)
    .byte   %00000000 ; |        |            $ff28 (P)
    .byte   %11011111 ; |** *****|            $ff29 (P)
    .byte   %01010001 ; | * *   *|            $ff2a (P)
    .byte   %01010001 ; | * *   *|            $ff2b (P)
    .byte   %11010011 ; |** *  **|            $ff2c (P)
    .byte   %10010011 ; |*  *  **|            $ff2d (P)
    .byte   %10010011 ; |*  *  **|            $ff2e (P)
    .byte   %10011111 ; |*  *****|            $ff2f (P)
Lff30
    .byte   %00000000 ; |        |            $ff30 (P)
    .byte   %00000000 ; |        |            $ff31 (P)
    .byte   %00000000 ; |        |            $ff32 (P)
    .byte   %00000000 ; |        |            $ff33 (P)
    .byte   %00000001 ; |       *|            $ff34 (P)
    .byte   %00000001 ; |       *|            $ff35 (P)
    .byte   %00000001 ; |       *|            $ff36 (P)
    .byte   %00000001 ; |       *|            $ff37 (P)
    .byte   %00000000 ; |        |            $ff38 (P)
    .byte   %00011000 ; |   **   |            $ff39 (P)
    .byte   %00011000 ; |   **   |            $ff3a (P)
    .byte   %00011000 ; |   **   |            $ff3b (P)
    .byte   %00011000 ; |   **   |            $ff3c (P)
    .byte   %00001000 ; |    *   |            $ff3d (P)
    .byte   %00001000 ; |    *   |            $ff3e (P)
    .byte   %00111110 ; |  ***** |            $ff3f (P)
    
Lff40
    .byte   YELLOW|$0                       ; $ff40 (CP)
    .byte   BROWN|$0                        ; $ff41 (CP)
    .byte   ORANGE|$0                       ; $ff42 (CP)
    .byte   RED|$0                          ; $ff43 (CP)
    .byte   MAUVE|$0                        ; $ff44 (CP)
    .byte   VIOLET|$0                       ; $ff45 (CP)
    .byte   PURPLE|$0                       ; $ff46 (CP)
    .byte   BLUE|$0                         ; $ff47 (CP)
    .byte   BLUE_CYAN|$0                    ; $ff48 (CP)
    .byte   CYAN|$0                         ; $ff49 (CP)
    .byte   CYAN_GREEN|$0                   ; $ff4a (CP)
    .byte   GREEN|$0                        ; $ff4b (CP)
    .byte   GREEN_YELLOW|$0                 ; $ff4c (CP)
    .byte   GREEN_BEIGE|$0                  ; $ff4d (CP)
    .byte   BEIGE|$0                        ; $ff4e (CP)
Lff4f
    .byte   YELLOW|$0                       ; $ff4f (CP)
    
Lff50
    .byte   $20,$02,$04,$40,$40,$04,$02,$20 ; $ff50 (D)
Lff58
    .byte   $01,$02,$04,$08                 ; $ff58 (D)
Lff5c
    .byte   $63                             ; $ff5c (*)
    .byte   $63,$9c,$4b,$1e,$c9,$e1,$b4,$c9 ; $ff5d (D)
    .byte   $36,$b4                         ; $ff65 (D)
    .byte   $00,$1e                         ; $ff67 (*)
Lff69
    .byte   $00,$0d,$26,$59,$00,$0a,$1f,$47 ; $ff69 (*)
    .byte   $00,$08,$17,$36                 ; $ff71 (D)
    .byte   $00,$05,$0f,$24,$00,$03,$08,$12 ; $ff75 (*)
Lff7d
    .byte   $00,$81                         ; $ff7d (D)
    .byte   $ff                             ; $ff7f (*)
    .byte   $cf,$c3,$04,$85                 ; $ff80 (D)
    .byte   $ff,$ff                         ; $ff84 (*)
    .byte   $c7,$08,$89,$8d                 ; $ff86 (D)
    .byte   $ff                             ; $ff8a (*)
    .byte   $cb,$0c                         ; $ff8b (D)
Lff8d
    .byte   $00,$0a,$0d,$17,$1a,$24,$27,$31 ; $ff8d (D)
    .byte   $34,$3e,$41,$4b,$4e             ; $ff95 (D)
Lff9a
    .byte   $04,$04                         ; $ff9a (D)
    .byte   $03,$fc                         ; $ff9c (*)
Lff9e
    .byte   $01,$fc                         ; $ff9e (D)
    .byte   $fd,$fb                         ; $ffa0 (*)
Lffa2
    .byte   $08                             ; $ffa2 (D)
Lffa3
    .byte   $0b,$08,$01,$08                 ; $ffa3 (D)
Lffa7
    .byte   $0b                             ; $ffa7 (D)
Lffa8
    .byte   $0f,$0b,$0f,$0b                 ; $ffa8 (D)
Lffac
    .byte   $c8,$3c,$9c                     ; $ffac (D)
Lffaf
    .byte   $4a,$2f,$11,$47                 ; $ffaf (D)
Lffb3
    .byte   $69,$0c,$0c,$04,$04             ; $ffb3 (D)
Lffb8
    .byte   $aa,$aa                         ; $ffb8 (*)
    .byte   $2a                             ; $ffba (D)
    .byte   $2a,$0a                         ; $ffbb (*)
Lffbd
    .byte   $06,$05                         ; $ffbd (*)
    .byte   $04                             ; $ffbf (D)
    .byte   $03,$02                         ; $ffc0 (*)
Lffc2
    .byte   $07,$0a                         ; $ffc2 (*)
    .byte   $0d                             ; $ffc4 (D)
    .byte   $10,$13                         ; $ffc5 (*)
Lffc7
    .byte   $aa,$ca                         ; $ffc7 (*)
    
    .byte   YELLOW|$a                       ; $ffc9 (C)
    
    .byte   $3a,$4a,$0c                     ; $ffca (*)
Lffcd
    .byte   $08,$18,$30                     ; $ffcd (D)
Lffd0
    .byte   $00,$80,$80                     ; $ffd0 (D)
Lffd3
    .byte   $00,$00,$80,$c0,$e0,$f0,$f8,$fc ; $ffd3 (D)
    .byte   $fe,$00,$01,$03,$07,$0f,$1f,$3f ; $ffdb (D)
    .byte   $7f                             ; $ffe3 (D)
Lffe4
    .byte   $00,$09,$11,$15                 ; $ffe4 (D)
Lffe8
    .byte   $ff,$ff,$fb,$eb                 ; $ffe8 (D)
Lffec
    .byte   $4a,$3a,$1a,$ca                 ; $ffec (D)

  IF SUPERCHARGER = 1 || PLUSROM = 1
    IF PLUSROM = 1

    ORG     $fffa
    .word (PlusROM_API - $E000)      ; PlusRom API pointer

    ENDIF

    ORG     $fffc

  ELSE

Lfff0
    .byte   $19,$1a,$d9,$c1,$b4,$a8,$b7,$c0 ; $fff0 (D)
    .byte   $b2,$b3,$80,$d8

  ENDIF
    
    .word   Start
    .byte   $90,$fb
