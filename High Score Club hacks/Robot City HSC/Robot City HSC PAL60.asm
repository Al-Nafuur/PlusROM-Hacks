; Disassembly of Robot City RC8 (PAL-60).bin
; Disassembled 06/05/23 11:09:58
; Using Stella 6.6
;
; ROM properties name : Robot City RC8 (PAL-60)
; ROM properties MD5  : 726af33d9acfc8616a8df05de4f560af
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
Break           = $fb9f


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
    jsr     Lf2d4                   ;6        
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
    lda     #$9e                    ;2        
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
    jsr     Lf2d4                   ;6        
    jsr     Lf9e2                   ;6        
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
    jsr     Lf9dd                   ;6        
    sta     ram_D9                  ;3        
    rol                             ;2        
    sta     CTRLPF                  ;3        
    sta     VDELP0                  ;3        
    ldy     #$1a                    ;2        
    jsr     Lf2d4                   ;6        
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
    jsr     Lf2d4                   ;6        
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
    jsr     Lf2d4                   ;6        
    lda     ram_B6                  ;3        
    cmp     #$60                    ;2        
    php                             ;3        
    bcc     Lf243                   ;2/3      
    lda     #$bf                    ;2        
    sbc     ram_B6                  ;3   =  60
Lf243
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    and     #$e0                    ;2        
    plp                             ;4        
    sbc     #$10                    ;2        
    tax                             ;2        
    ldy     #$0f                    ;2   =  18
Lf24e
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
    bpl     Lf269                   ;2/3 =  16
Lf260
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    inx                             ;2        
    inx                             ;2        
    pha                             ;3        
    pla                             ;4        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2   =  17
Lf269
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
    bpl     Lf260                   ;2/3      
    dey                             ;2        
    bpl     Lf24e                   ;2/3 =  42
Lf286
    lda     INTIM                   ;4        
    bne     Lf286                   ;2/3      
    ldx     #$05                    ;2        
    lda     #$c3                    ;2   =  10
Lf28f
    sta     ram_F5,x                ;4        
    dex                             ;2        
    bpl     Lf28f                   ;2/3      
    ldy     ram_CE                  ;3        
    lda     Lfd81,y                 ;4        
    sta     ram_F8                  ;3        
    jsr     Lf154                   ;6        
    ldx     ram_B6                  ;3        
    bit     INPT4|$30               ;3        
    bpl     Lf2b0                   ;2/3      
    lda     SWCHB                   ;4        
    cmp     ram_B5                  ;3        
    sta     ram_B5                  ;3        
    beq     Lf2c6                   ;2/3      
    lsr                             ;2         *
    bcs     Lf2b8                   ;2/3 =  48 *
Lf2b0
    cpx     #$e4                    ;2        
    bcs     Lf2cc                   ;2/3      
    ldx     #$e4                    ;2        
    bcc     Lf2cd                   ;2/3 =   8
Lf2b8
    lsr                             ;2         *
    bcs     Lf2c6                   ;2/3       *
    ldy     ram_CE                  ;3         *
    iny                             ;2         *
    cpy     #$05                    ;2         *
    bcc     Lf2c4                   ;2/3       *
    ldy     #$00                    ;2   =  15 *
Lf2c4
    sty     ram_CE                  ;3   =   3 *
Lf2c6
    cpx     #$77                    ;2        
    bne     Lf2cc                   ;2/3      
    ldx     #$47                    ;2   =   6 *
Lf2cc
    inx                             ;2   =   2
Lf2cd
    stx     ram_B6                  ;3        
    beq     Lf2da                   ;2/3      
    jmp     Lf1f9                   ;3   =   8
    
Lf2d4
    dey                             ;2        
    sta     WSYNC                   ;3   =   5
;---------------------------------------
    bne     Lf2d4                   ;2/3      
    rts                             ;6   =   8
    
Lf2da
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
Lf2f8
    ldx     ram_CE                  ;3        
    lda     Lffb8,x                 ;4        
    sta     ram_DD                  ;3        
    ldy     Lfd6d,x                 ;4        
    ldx     #$07                    ;2   =  16
Lf304
    lda     Lfe12,y                 ;4        
    sta     ram_E1,x                ;4        
    dey                             ;2        
    stx     ram_CF                  ;3        
    dex                             ;2        
    bne     Lf304                   ;2/3      
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
Lf323
    lda     #$fb                    ;2        
    sta     ram_EA                  ;3        
    lda     #$3a                    ;2        
    sta     ram_EF                  ;3        
    ldy     ram_CE                  ;3        
    lda     #$1f                    ;2        
    sta     ram_F0                  ;3        
    and     ram_DE                  ;3   =  21
Lf333
    lsr                             ;2        
    cmp     Lffc2,y                 ;4        
    bcs     Lf333                   ;2/3      
    adc     Lffbd,y                 ;4        
    sta     ram_EE                  ;3        
    ldx     #$19                    ;2        
    lda     #$40                    ;2        
    ldy     #$00                    ;2   =  21
Lf344
    sta     ram_81,x                ;4        
    sty     ram_8E,x                ;4        
    dex                             ;2        
    bpl     Lf344                   ;2/3 =  12
Lf34b
    ldx     #$0b                    ;2   =   2
Lf34d
    inx                             ;2        
    dey                             ;2        
    bmi     Lf365                   ;2/3      
    jsr     Lfbd3                   ;6        
    bne     Lf365                   ;2/3      
    iny                             ;2        
    jsr     Lfbb9                   ;6        
    bne     Lf366                   ;2/3      
    dex                             ;2        
    dex                             ;2        
    jsr     Lfbb9                   ;6        
    beq     Lf3a9                   ;2/3      
    inx                             ;2        
    .byte   $0c ;NOP                ;4-2 =  40
Lf365
    iny                             ;2   =   2
Lf366
    dex                             ;2        
    jsr     Lfcc3                   ;6        
    bpl     Lf3ad                   ;2/3      
    inx                             ;2        
    jsr     Lfbb9                   ;6        
    beq     Lf380                   ;2/3      
    cpy     #$04                    ;2        
    beq     Lf37b                   ;2/3      
    jsr     Lfbde                   ;6        
    beq     Lf380                   ;2/3 =  32
Lf37b
    dex                             ;2        
    .byte   $82 ;NOP                ;2-2 =   2
Lf37d
    inx                             ;2        
    bpl     Lf3ad                   ;2/3 =   4
Lf380
    dex                             ;2        
    dex                             ;2        
    jsr     Lfbb9                   ;6        
    beq     Lf390                   ;2/3      
    cpy     #$04                    ;2        
    beq     Lf37d                   ;2/3      
    jsr     Lfbd3                   ;6        
    bne     Lf37d                   ;2/3 =  24
Lf390
    inx                             ;2        
    jsr     Lfcab                   ;6        
    cpy     #$02                    ;2        
    bcc     Lf3a5                   ;2/3      
    dex                             ;2        
    stx     ram_B5                  ;3        
    sty     ram_B6                  ;3        
    inx                             ;2        
    inx                             ;2        
    jsr     Lfc14                   ;6        
    inx                             ;2        
    bcs     Lf3aa                   ;2/3 =  34
Lf3a5
    dec     ram_EE                  ;5        
    bpl     Lf3ad                   ;2/3 =   7
Lf3a9
    inx                             ;2   =   2
Lf3aa
    jsr     Lfcab                   ;6   =   6
Lf3ad
    dec     ram_EF                  ;5        
    dex                             ;2        
    dex                             ;2        
    bpl     Lf34d                   ;2/3      
    cpy     #$04                    ;2        
    bcs     Lf429                   ;2/3!     
    ldx     #$0c                    ;2        
    bne     Lf3d5                   ;2/3 =  19
Lf3bb
    jsr     Lfbde                   ;6        
    bne     Lf3d5                   ;2/3      
    inx                             ;2        
    inx                             ;2        
    jsr     Lfbc6                   ;6        
    php                             ;3        
    dex                             ;2        
    dex                             ;2        
    plp                             ;4        
    bne     Lf3d5                   ;2/3      
    jsr     Lfccc                   ;6        
    lda     ram_DF                  ;3        
    bmi     Lf41f                   ;2/3!     
    sec                             ;2        
    bpl     Lf3da                   ;2/3 =  46
Lf3d5
    jsr     Lfcc3                   ;6        
    bpl     Lf41f                   ;2/3!=   8
Lf3da
    ror     ram_ED                  ;5        
    cpy     #$03                    ;2        
    bne     Lf3e7                   ;2/3      
    cpx     #$0c                    ;2        
    beq     Lf41f                   ;2/3!     
    txa                             ;2        
    beq     Lf41f                   ;2/3!=  17
Lf3e7
    lda     #$01                    ;2        
    sta     ram_EB                  ;3        
    jsr     Lfbb9                   ;6        
    beq     Lf3f2                   ;2/3      
    dec     ram_EB                  ;5   =  18
Lf3f2
    jsr     Lfbde                   ;6        
    beq     Lf3f9                   ;2/3      
    dec     ram_EB                  ;5   =  13
Lf3f9
    jsr     Lfbd3                   ;6        
    beq     Lf400                   ;2/3!     
    dec     ram_EB                  ;5   =  13
Lf400
    bit     ram_EB                  ;3        
    bmi     Lf41f                   ;2/3      
    jsr     Lfca2                   ;6        
    tya                             ;2        
    beq     Lf414                   ;2/3      
    stx     ram_B5                  ;3        
    sty     ram_B6                  ;3        
    iny                             ;2        
    jsr     Lfc14                   ;6        
    bcs     Lf41c                   ;2/3 =  31
Lf414
    bit     ram_ED                  ;3        
    bmi     Lf41f                   ;2/3      
    dec     ram_EE                  ;5        
    bpl     Lf41f                   ;2/3 =  12
Lf41c
    jsr     Lfca2                   ;6   =   6 *
Lf41f
    dec     ram_EF                  ;5        
    dex                             ;2        
    dex                             ;2        
    bpl     Lf3bb                   ;2/3!     
    iny                             ;2        
    jmp     Lf34b                   ;3   =  16
    
Lf429
    ldx     #$0b                    ;2   =   2
Lf42b
    lda     ram_8E,x                ;4        
    ora     #$22                    ;2        
    sta     ram_8E,x                ;4        
    lda     ram_9B,x                ;4        
    ora     ram_9C,x                ;4        
    dex                             ;2        
    bmi     Lf43a                   ;2/3      
    ora     ram_9B,x                ;4   =  26
Lf43a
    ora     #$04                    ;2        
    sta     ram_9C,x                ;4        
    dex                             ;2        
    bpl     Lf42b                   ;2/3      
    lda     ram_DF                  ;3        
    and     #$f6                    ;2        
    adc     #$04                    ;2        
    sta     COLUPF                  ;3        
    jsr     Lf1c9                   ;6   =  26
Lf44c
    ror     ram_EC                  ;5        
    ldy     #$0b                    ;2   =   7
Lf450
    ldx     Lfff0,y                 ;4        
    lda     Lfec4,y                 ;4        
    sta     VSYNC,x                 ;4        
    dey                             ;2        
    bpl     Lf450                   ;2/3      
    ldy     ram_CE                  ;3        
    lda     Lfefa,y                 ;4        
    sta     ram_EB                  ;3        
    ldx     #$04                    ;2   =  28
Lf464
    lda     #$20                    ;2        
    tay                             ;2        
    bit     ram_EC                  ;3        
    bmi     Lf46f                   ;2/3      
    and     ram_AC,x                ;4        
    bne     Lf483                   ;2/3 =  15
Lf46f
    lda     Lffaf,x                 ;4        
    sta     ram_B9,x                ;4        
    lda     #$00                    ;2        
    cpx     ram_EB                  ;3        
    bpl     Lf47f                   ;2/3      
    lda     #$0a                    ;2        
    ldy     Lffb3,x                 ;4   =  21
Lf47f
    sta     ram_C2,x                ;4        
    sty     ram_AC,x                ;4   =   8
Lf483
    dex                             ;2        
    bne     Lf464                   ;2/3      
    bit     ram_DB                  ;3        
    bpl     Lf494                   ;2/3      
    bvc     Lf48e                   ;2/3      
    ldx     #$02                    ;2   =  13
Lf48e
    lda     ram_AD,x                ;4        
    eor     #$08                    ;2        
    sta     ram_AD,x                ;4   =  10
Lf494
    sta     VDELBL                  ;3        
    lda     SWCHB                   ;4        
    lsr                             ;2        
    bcs     Lf4a7                   ;2/3 =  11
Lf49c
    lda     ram_E0                  ;3         *
    sta     ram_DE                  ;3         *
    lda     ram_E1                  ;3         *
    sta     ram_DF                  ;3         *
    jmp     Lf2f8                   ;3   =  15 *
    
Lf4a7
    lsr                             ;2        
    bcs     Lf4af                   ;2/3      
    ldx     #$ce                    ;2         *
    jmp     Lf1df                   ;3   =   9 *
    
Lf4af
    lda     ram_D8                  ;3        
    cmp     #$04                    ;2        
    bcc     Lf4d3                   ;2/3      
    bit     INPT4|$30               ;3         *
    bpl     Lf49c                   ;2/3       *
    cmp     #$05                    ;2         *
    ldy     #$0c                    ;2         *
    LAX     ram_A8                  ;3         *
    beq     Lf4e9                   ;2/3       *
    bcs     Lf4cb                   ;2/3       *
    lsr                             ;2         *
    lsr                             ;2         *
    eor     #$1e                    ;2         *
    ldx     #$08                    ;2         *
    bpl     Lf4e7                   ;2/3 =  33 *
Lf4cb
    lsr                             ;2         *
    tax                             ;2         *
    and     #$02                    ;2         *
    ora     #$04                    ;2         *
    bpl     Lf4e7                   ;2/3 =  10 *
Lf4d3
    ldy     #$07                    ;2        
    LAX     ram_A8                  ;3        
    beq     Lf4e9                   ;2/3      
    bpl     Lf4e5                   ;2/3      
    cmp     #$c0                    ;2        
    bcc     Lf4ef                   ;2/3      
    lsr                             ;2        
    lsr                             ;2        
    tax                             ;2        
    iny                             ;2        
    lda     #$17                    ;2   =  23
Lf4e5
    eor     #$0f                    ;2   =   2
Lf4e7
    dec     ram_A8                  ;5   =   5
Lf4e9
    sta     AUDF0                   ;3        
    sty     AUDC0                   ;3        
    stx     AUDV0                   ;3   =   9
Lf4ef
    ldy     ram_D8                  ;3        
    beq     Lf501                   ;2/3!     
    dey                             ;2        
    bne     Lf504                   ;2/3!     
    lda     SWCHA                   ;4        
    eor     #$ff                    ;2        
    beq     Lf501                   ;2/3!     
    sty     ram_D8                  ;3        
    sty     ram_AB                  ;3   =  23
Lf501
    jmp     Lf5cb                   ;3   =   3
    
Lf504
    cpy     #$02                    ;2        
    bne     Lf519                   ;2/3      
    lda     ram_80                  ;3        
    beq     Lf519                   ;2/3      
    lda     ram_DC                  ;3        
    and     #$03                    ;2        
    bne     Lf517                   ;2/3      
    lda     ram_CE                  ;3        
    jsr     Lfc01                   ;6   =  25
Lf517
    dec     ram_80                  ;5   =   5
Lf519
    lda     ram_DC                  ;3        
    and     #$07                    ;2        
    bne     Lf501                   ;2/3      
    dec     ram_AB                  ;5        
    bne     Lf501                   ;2/3      
    dey                             ;2        
    bne     Lf573                   ;2/3      
    lsr     ram_DD                  ;5        
    beq     Lf530                   ;2/3      
    lsr     ram_DD                  ;5        
    clc                             ;2        
    jmp     Lf44c                   ;3   =  35
    
Lf530
  IF PLUSROM = 1
    jmp SendPlusROMScore
    nop
Lf534
  ELSE

    sty     ram_DC                  ;3         *
    lda     ram_DA                  ;3         *

  ENDIF

    bne     Lf569                   ;2/3       *
    ldx     #$04                    ;2         *
    lda     ram_CE                  ;3         *
    cmp     ram_D3                  ;3         *
    bne     Lf54a                   ;2/3       *
    inx                             ;2   =  20 *
Lf53f
    dex                             ;2         *
    beq     Lf569                   ;2/3       *
    lda     ram_D3,x                ;4         *
    sbc     ram_CE,x                ;4         *
    beq     Lf53f                   ;2/3       *
    bcs     Lf569                   ;2/3 =  16 *
Lf54a
    lda     ram_CE,x                ;4         *
    sta     ram_D3,x                ;4         *
    dex                             ;2         *
    bpl     Lf54a                   ;2/3       *
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
Lf569
    lda     #$04                    ;2         *
    sta     ram_D8                  ;3         *
    lda     #$5f                    ;2         *
    sta     ram_A8                  ;3         *
    bne     Lf501                   ;2/3 =  12 *
Lf573
    dey                             ;2        
    bne     Lf519                   ;2/3      
    sed                             ;2        
    clc                             ;2        
    lda     ram_CF                  ;3        
    adc     #$01                    ;2        
    bcs     Lf580                   ;2/3      
    sta     ram_CF                  ;3   =  18
Lf580
    cld                             ;2        
    lsr                             ;2        
    bcc     Lf588                   ;2/3      
    rol     ram_DD                  ;5         *
    asl     ram_DD                  ;5   =  16 *
Lf588
    ldx     #$04                    ;2   =   2
Lf58a
    txa                             ;2        
    ldy     ram_CE                  ;3        
    clc                             ;2        
    adc     Lfd6d,y                 ;4        
    sbc     #$05                    ;2        
    tay                             ;2        
    lda     Lfe35,y                 ;4        
    sec                             ;2        
    sbc     ram_E2,x                ;4        
    sta     ram_EB                  ;3        
    lda     Lffd0,x                 ;4        
    pha                             ;3        
    beq     Lf5a8                   ;2/3      
    lda     Lfe39,y                 ;4        
    sec                             ;2        
    sbc     ram_E6,x                ;4   =  47
Lf5a8
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
    beq     Lf5c5                   ;2/3      
    lda     ram_EC                  ;3        
    adc     ram_E6,x                ;4        
    sta     ram_E6,x                ;4   =  59
Lf5c5
    dex                             ;2        
    bpl     Lf58a                   ;2/3      
    jmp     Lf323                   ;3   =   7
    
Lf5cb
    bit     SWCHB                   ;4        
    bmi     Lf5d9                   ;2/3      
    ldx     #$04                    ;2        
    jsr     Lf9fe                   ;6        
    dex                             ;2        
    jsr     Lf9fe                   ;6   =  22
Lf5d9
    jsr     Lf9f3                   ;6   =   6
Lf5dc
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
    bpl     Lf5dc                   ;2/3      
    ldx     #$01                    ;2   =  46
Lf5f9
    lda     #$1f                    ;2        
    cmp     ram_ED,x                ;4        
    bcs     Lf60d                   ;2/3!     
    ldy     #$09                    ;2        
    lda     ram_DC                  ;3        
    and     #$08                    ;2        
    bne     Lf61c                   ;2/3      
    lda     ram_CE                  ;3        
    cmp     #$03                    ;2        
    bcc     Lf61c                   ;2/3 =  24
Lf60d
    lda     ram_ED,x                ;4        
    and     #$df                    ;2        
    cmp     #$09                    ;2        
    bcc     Lf61b                   ;2/3      
    eor     #$ff                    ;2        
    adc     #$10                    ;2        
    dec     ram_B5,x                ;6   =  20
Lf61b
    tay                             ;2   =   2
Lf61c
    lda     #$07                    ;2        
    adc     #$00                    ;2        
    sta     REFP0,x                 ;4        
    lda     ram_DC                  ;3        
    and     #$02                    ;2        
    beq     Lf62c                   ;2/3      
    tya                             ;2        
    adc     #$0a                    ;2        
    tay                             ;2   =  21
Lf62c
    lda     Lfdfe,y                 ;4        
    sbc     ram_BE,x                ;4        
    sta     ram_EA,x                ;4        
    dex                             ;2        
    bpl     Lf5f9                   ;2/3!     
    sta     ram_E9                  ;3        
    ldx     #$04                    ;2   =  21
Lf63a
    lda     ram_B5,x                ;4        
    clc                             ;2        
    sta     WSYNC                   ;3   =   9
;---------------------------------------
Lf63f
    sbc     #$0f                    ;2        
    bcs     Lf63f                   ;2/3      
    eor     #$07                    ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    sta     HMP0,x                  ;4        
    sta.wx  RESP0,x                 ;5        
    dex                             ;2        
    bpl     Lf63a                   ;2/3      
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
    bcs     Lf678                   ;2/3      
    dey                             ;2        
    bne     Lf698                   ;2/3      
    ldy     ram_CE                  ;3        
    lda     ram_DD                  ;3        
    eor     Lffb8,y                 ;4        
    bne     Lf685                   ;2/3      
    ldy     ram_CF                  ;3        
    dey                             ;2        
    bne     Lf685                   ;2/3 =  51
Lf678
    bmi     Lf683                   ;2/3      
    lda     ram_CE                  ;3        
    eor     ram_D3                  ;3        
    bne     Lf683                   ;2/3      
    ldx     #$07                    ;2         *
    clc                             ;2   =  14 *
Lf683
    bvc     Lf698                   ;2/3 =   2
Lf685
    ror     ram_D9                  ;5        
    bit     ram_EA                  ;3        
    lda     ram_CD,x                ;4        
    ldx     #$05                    ;2        
    ldy     #$c3                    ;2   =  16
Lf68f
    sty     ram_F5,x                ;4        
    dex                             ;2        
    bpl     Lf68f                   ;2/3      
    ldy     #$01                    ;2        
    bne     Lf6a1                   ;2/3 =  12
Lf698
    ror     ram_D9                  ;5        
    bit     ram_EA                  ;3        
    ldy     #$05                    ;2   =  10
Lf69e
    lda     ram_D0,x                ;4        
    dex                             ;2   =   6
Lf6a1
    stx     ram_B5                  ;3        
    pha                             ;3        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    sec                             ;2        
    .byte   $0c ;NOP                ;4-4 =  16
Lf6aa
    pla                             ;4        
    clc                             ;2        
    and     #$0f                    ;2        
    bvc     Lf6b8                   ;2/3      
    bne     Lf6b7                   ;2/3      
    ldx     #$0a                    ;2        
    tya                             ;2        
    bne     Lf6b9                   ;2/3 =  18
Lf6b7
    clv                             ;2   =   2
Lf6b8
    tax                             ;2   =   2
Lf6b9
    lda     Lfd80,x                 ;4        
    ldx     ram_B5                  ;3        
    sta.wy  ram_F5,y                ;5        
    dey                             ;2        
    bcs     Lf6aa                   ;2/3      
    bpl     Lf69e                   ;2/3      
    bit     ram_B4                  ;3        
    lda     ram_DC                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    lda     #$1c                    ;2        
    bvc     Lf6d8                   ;2/3      
    lda     ram_AA                  ;3        
    and     #$01                    ;2        
    beq     Lf6d8                   ;2/3      
    lda     #$0e                    ;2   =  41
Lf6d8
    bcc     Lf6dc                   ;2/3      
    adc     #$06                    ;2   =   4
Lf6dc
    adc     #$d8                    ;2        
    sbc     ram_C0                  ;3        
    sta     ram_EF                  ;3        
    iny                             ;2        
    lda     ram_D8                  ;3        
    cmp     #$04                    ;2        
    bcs     Lf6f4                   ;2/3      
    cmp     #$02                    ;2        
    bne     Lf6f2                   ;2/3      
    lda     ram_AB                  ;3        
    lsr                             ;2        
    bcs     Lf6f4                   ;2/3 =  28
Lf6f2
    ldy     ram_C0                  ;3   =   3
Lf6f4
    sty     ram_B5                  ;3        
    lda     #$15                    ;2        
    sta     CTRLPF                  ;3        
    sta     HMCLR                   ;3        
    lda     ram_80                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    ldy     #$00                    ;2        
    ldx     #$04                    ;2   =  22
Lf704
    dex                             ;2        
    sty     ram_F1,x                ;4        
    cmp     Lffe4,x                 ;4        
    bcc     Lf704                   ;2/3      
    adc     Lffe8,x                 ;4        
    tay                             ;2        
    lda     Lffd3,y                 ;4   =  22
Lf713
    sta     ram_F1,x                ;4        
    lda     #$ff                    ;2        
    dex                             ;2        
    bpl     Lf713                   ;2/3      
    lda     ram_80                  ;3   =  13
Lf71c
    inx                             ;2        
    cmp     Lfd69,x                 ;4        
    bcs     Lf71c                   ;2/3      
    ldy     Lffec,x                 ;4        
    lda     Lffcd,x                 ;4        
    beq     Lf73c                   ;2/3      
    and     ram_DC                  ;3        
    bne     Lf73c                   ;2/3      
    lda     ram_D8                  ;3        
    bne     Lf73c                   ;2/3      
    lda     ram_A9                  ;3         *
    bmi     Lf73a                   ;2/3       *
    lda     #$cf                    ;2         *
    sta     ram_A9                  ;3   =  38 *
Lf73a
    ldy     #$00                    ;2   =   2 *
Lf73c
    sty     ram_FB                  ;3        
    ldy     ram_D8                  ;3        
    bne     Lf751                   ;2/3      
    lda     ram_AB                  ;3        
    lsr                             ;2        
    ldx     #$07                    ;2        
    bcc     Lf74d                   ;2/3      
    ldx     #$01                    ;2        
    ldy     #$30                    ;2   =  21
Lf74d
    stx     ram_B6                  ;3        
    sty     NUSIZ1                  ;3   =   6
Lf751
    lda     ram_DB                  ;3        
    lsr                             ;2        
    bcc     Lf758                   ;2/3      
    eor     #$b2                    ;2   =   9
Lf758
    sta     ram_DB                  ;3        
    jsr     Lf000                   ;6        
    jsr     Lf9f3                   ;6        
    bit     CXM1P|$30               ;3        
    bvs     Lf767                   ;2/3      
    bpl     Lf76e                   ;2/3      
    dex                             ;2   =  24
Lf767
    cpx     ram_B1                  ;3        
    beq     Lf76e                   ;2/3      
    jsr     Lfbeb                   ;6   =  11
Lf76e
    lda.wy  CXP0FB|$30,y            ;4        
    asl                             ;2        
    bpl     Lf793                   ;2/3      
    lda     ram_AC                  ;3        
    asl                             ;2        
    asl                             ;2        
    sta     ram_EB                  ;3        
    lda     ram_AD,x                ;4        
    and     #$2f                    ;2        
    cmp     #$20                    ;2        
    bcs     Lf793                   ;2/3      
    adc     #$01                    ;2        
    and     #$0f                    ;2        
    sec                             ;2        
    sbc     ram_EB                  ;3        
    cmp     #$03                    ;2        
    bcs     Lf7a1                   ;2/3      
    jsr     Lfbeb                   ;6        
    jmp     Lf7a1                   ;3   =  50
    
Lf793
    dex                             ;2        
    dey                             ;2        
    bpl     Lf76e                   ;2/3      
    lda     ram_C2                  ;3        
    cmp     #$5b                    ;2        
    bcs     Lf7a1                   ;2/3      
    bit     ram_CC                  ;3        
    bpl     Lf7a5                   ;2/3 =  18
Lf7a1
    lda     #$00                    ;2        
    sta     ram_C2                  ;3   =   5
Lf7a5
    lda     ram_C1                  ;3        
    cmp     #$5b                    ;2        
    bcs     Lf7af                   ;2/3      
    bit     CXM1FB|$30              ;3        
    bpl     Lf7b3                   ;2/3 =  12
Lf7af
    lda     #$00                    ;2        
    sta     ram_C1                  ;3   =   5
Lf7b3
    lda     ram_D8                  ;3        
    bne     Lf7de                   ;2/3      
    bit     CXM0P|$30               ;3        
    bvs     Lf7c1                   ;2/3      
    bmi     Lf7c1                   ;2/3      
    bit     CXPPMM|$30              ;3        
    bvc     Lf7cb                   ;2/3 =  17
Lf7c1
    sta     ram_C1                  ;3        
    lda     #$bf                    ;2        
    sta     ram_A9                  ;3        
    lda     #$02                    ;2        
    bne     Lf7d8                   ;2/3 =  12
Lf7cb
    ldx     #$03                    ;2        
    lda     #$20                    ;2   =   4
Lf7cf
    and     ram_AD,x                ;4        
    beq     Lf7e1                   ;2/3      
    dex                             ;2        
    bpl     Lf7cf                   ;2/3      
    lda     #$03                    ;2   =  12
Lf7d8
    sta     ram_D8                  ;3        
    lda     #$10                    ;2        
    sta     ram_AB                  ;3   =   8
Lf7de
    jmp     Lf984                   ;3   =   3
    
Lf7e1
    bit     SWCHB                   ;4        
    bpl     Lf7ea                   ;2/3      
    tax                             ;2         *
    jmp     Lf9d5                   ;3   =  11 *
    
Lf7ea
    lda     ram_CA                  ;3        
    sec                             ;2        
    adc     ram_E5                  ;3        
    sta     ram_CA                  ;3        
    lsr                             ;2        
    cmp     ram_E5                  ;3        
    ror     ram_EE                  ;5        
    bmi     Lf85f                   ;2/3!     
    lda     #$fb                    ;2        
    sta     ram_EA                  ;3        
    jsr     Lf9f3                   ;6        
    sty     ram_ED                  ;3   =  37
Lf801
    stx     ram_EB                  ;3        
    lda     ram_AD,x                ;4        
    tay                             ;2        
    and     #$33                    ;2        
    beq     Lf83a                   ;2/3      
    cmp     #$20                    ;2        
    bcs     Lf85a                   ;2/3      
    lda     ram_CD                  ;3        
    eor     Lff58,x                 ;4        
    cmp     ram_CD                  ;3        
    sta     ram_CD                  ;3        
    bcs     Lf85a                   ;2/3      
    tya                             ;2        
    and     #$03                    ;2        
    cmp     #$01                    ;2        
    tya                             ;2        
    bcs     Lf824                   ;2/3      
    and     #$ef                    ;2        
    tay                             ;2   =  46
Lf824
    cpy     #$c0                    ;2        
    bcs     Lf835                   ;2/3      
    and     #$0f                    ;2        
    eor     #$0f                    ;2        
    bne     Lf833                   ;2/3      
    tya                             ;2        
    and     #$90                    ;2        
    tay                             ;2        
    .byte   $82 ;NOP                ;2-2 =  16
Lf833
    iny                             ;2        
    .byte   $82 ;NOP                ;2-2 =   2
Lf835
    dey                             ;2        
    sty     ram_AD,x                ;4        
    bne     Lf85a                   ;2/3 =   8
Lf83a
    tya                             ;2        
    bmi     Lf844                   ;2/3      
    jsr     Lfa2a                   ;6        
    lda     ram_AD,x                ;4        
    bmi     Lf85a                   ;2/3 =  16
Lf844
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
    jsr     Lfa0d                   ;6        
    ldx     ram_EB                  ;3        
    jsr     Lfb59                   ;6   =  37
Lf85a
    dex                             ;2        
    dec     ram_ED                  ;5        
    bpl     Lf801                   ;2/3 =   9
Lf85f
    ldy     ram_AA                  ;3        
    LAX     SWCHA                   ;4        
    bmi     Lf868                   ;2/3      
    ldy     #$03                    ;2   =  11
Lf868
    asl                             ;2        
    bmi     Lf86d                   ;2/3      
    ldy     #$06                    ;2   =   6
Lf86d
    asl                             ;2        
    asl                             ;2        
    php                             ;3        
    tya                             ;2        
    and     #$f9                    ;2        
    plp                             ;4        
    bpl     Lf87a                   ;2/3      
    bcs     Lf87b                   ;2/3      
    ora     #$04                    ;2   =  21
Lf87a
    tay                             ;2   =   2
Lf87b
    sty     ram_AA                  ;3        
    asl     $3c                     ;5        
    bcs     Lf8af                   ;2/3      
    txa                             ;2        
    ldx     ram_D9                  ;3        
    bmi     Lf892                   ;2/3      
    ldx     #$80                    ;2        
    and     #$f0                    ;2        
    eor     #$f0                    ;2        
    ora     ram_B4                  ;3        
    bne     Lf892                   ;2/3      
    ldx     #$c0                    ;2   =  30
Lf892
    lda     ram_C2                  ;3        
    bne     Lf8ae                   ;2/3      
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
    bvs     Lf8af                   ;2/3      
    lda     #$0f                    ;2        
    sta     ram_A8                  ;3   =  38
Lf8ae
    txa                             ;2   =   2
Lf8af
    sta     ram_D9                  ;3        
    asl                             ;2        
    bpl     Lf8b7                   ;2/3      
    jmp     Lf965                   ;3   =  10
    
Lf8b7
    lda     ram_C7                  ;3        
    sec                             ;2        
    adc     ram_E2                  ;3        
    sta     ram_C7                  ;3        
    bcc     Lf8e3                   ;2/3      
    ldx     #$c0                    ;2        
    ldy     ram_B3                  ;3        
    lda     Lfe72,y                 ;4        
    bit     ram_B4                  ;3        
    bmi     Lf8d4                   ;2/3      
    bvc     Lf8e3                   ;2/3      
    ldx     #$b7                    ;2        
    ldy     ram_B2                  ;3        
    lda     Lfe61,y                 ;4   =  38
Lf8d4
    cmp     CXM0P,x                 ;4        
    bcc     Lf8db                   ;2/3      
    inc     VSYNC,x                 ;6        
    .byte   $0c ;NOP                ;4-6 =  10
Lf8db
    dec     VSYNC,x                 ;6        
    eor     CXM0P,x                 ;4        
    bne     Lf8e3                   ;2/3      
    sta     ram_B4                  ;3   =  15
Lf8e3
    inc     ram_B4                  ;5        
    cpx     #$c0                    ;2        
    bne     Lf924                   ;2/3!=   9
Lf8e9
    bit     ram_B4                  ;3        
    bmi     Lf922                   ;2/3!     
    lda     ram_B3                  ;3        
    lsr                             ;2        
    bcs     Lf922                   ;2/3!     
    ldx     ram_B2                  ;3        
    ldy     Lfe61,x                 ;4        
    bit     SWCHA                   ;4        
    bmi     Lf90c                   ;2/3!     
    cpy     ram_B7                  ;3        
    beq     Lf902                   ;2/3      
    bcs     Lf922                   ;2/3 =  32
Lf902
    brk                             ;7   =   7
    
    jsr     Lfbc6                   ;6        
    bne     Lf922                   ;2/3      
    inc     ram_B2                  ;5        
    bpl     Lf91e                   ;2/3 =  15
Lf90c
    bvs     Lf922                   ;2/3      
    cpy     ram_B7                  ;3        
    bcc     Lf922                   ;2/3      
    txa                             ;2        
    lsr                             ;2        
    bcs     Lf91c                   ;2/3      
    brk                             ;7   =  20
    
    jsr     Lfbb9                   ;6        
    bne     Lf922                   ;2/3 =   8
Lf91c
    dec     ram_B2                  ;5   =   5
Lf91e
    lda     #$40                    ;2        
    bne     Lf95e                   ;2/3 =   4
Lf922
    dec     ram_B4                  ;5   =   5
Lf924
    bit     ram_B4                  ;3        
    bvs     Lf960                   ;2/3      
    lda     ram_B2                  ;3        
    lsr                             ;2        
    bcs     Lf960                   ;2/3      
    ldx     ram_B3                  ;3        
    ldy     Lfe72,x                 ;4        
    lda     SWCHA                   ;4        
    asl                             ;2        
    asl                             ;2        
    bmi     Lf94b                   ;2/3      
    cpy     ram_C0                  ;3        
    bcc     Lf960                   ;2/3      
    txa                             ;2        
    lsr                             ;2        
    bcs     Lf947                   ;2/3      
    brk                             ;7   =  47
    
    jsr     Lfbd3                   ;6        
    bne     Lf960                   ;2/3 =   8
Lf947
    dec     ram_B3                  ;5        
    bpl     Lf95c                   ;2/3 =   7
Lf94b
    asl                             ;2        
    bmi     Lf960                   ;2/3      
    cpy     ram_C0                  ;3        
    beq     Lf954                   ;2/3      
    bcs     Lf960                   ;2/3 =  11
Lf954
    brk                             ;7   =   7
    
    jsr     Lfbde                   ;6        
    bne     Lf960                   ;2/3      
    inc     ram_B3                  ;5   =  13
Lf95c
    lda     #$80                    ;2   =   2
Lf95e
    sta     ram_B4                  ;3   =   3
Lf960
    lda     ram_B4                  ;3        
    lsr                             ;2        
    bcs     Lf8e9                   ;2/3!=   7
Lf965
    lda     ram_DC                  ;3        
    and     #$0f                    ;2        
    bne     Lf984                   ;2/3      
    lda     ram_CB                  ;3        
    sec                             ;2        
    sbc     ram_E6                  ;3        
    sta     ram_CB                  ;3        
    bcs     Lf984                   ;2/3      
    dec     ram_80                  ;5        
    bne     Lf984                   ;2/3      
    lda     #$02                    ;2         *
    sta     ram_D8                  ;3         *
    lda     #$10                    ;2         *
    sta     ram_AB                  ;3         *
    lda     #$bf                    ;2         *
    sta     ram_A9                  ;3   =  42 *
Lf984
    sta     CXCLR                   ;3        
    lda     ram_D8                  ;3        
    cmp     #$03                    ;2        
    bne     Lf999                   ;2/3      
    ldy     #$0c                    ;2        
    ldx     ram_80                  ;3        
    beq     Lf9d1                   ;2/3      
    lda     ram_DC                  ;3        
    and     #$06                    ;2        
    tax                             ;2        
    bpl     Lf9d1                   ;2/3 =  26
Lf999
    lda     ram_A9                  ;3        
    bpl     Lf9b5                   ;2/3      
    dec     ram_A9                  ;5        
    cmp     #$c0                    ;2        
    bcs     Lf9aa                   ;2/3      
    lsr                             ;2        
    lsr                             ;2        
    tax                             ;2        
    eor     #$0f                    ;2        
    bpl     Lf9cf                   ;2/3 =  24
Lf9aa
    bne     Lf9ae                   ;2/3       *
    lsr     ram_A9                  ;5   =   7 *
Lf9ae
    tax                             ;2         *
    ldy     #$04                    ;2         *
    lda     #$12                    ;2         *
    bpl     Lf9d1                   ;2/3 =   8 *
Lf9b5
    ldx     #$08                    ;2        
    lda     ram_C1                  ;3        
    beq     Lf9c3                   ;2/3      
    eor     ram_B8                  ;3        
    and     #$05                    ;2        
    ldy     #$08                    ;2        
    bpl     Lf9d1                   ;2/3 =  16
Lf9c3
    bit     ram_EE                  ;3        
    bpl     Lf9c8                   ;2/3      
    tax                             ;2   =   7
Lf9c8
    lda     ram_E5                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    eor     #$1f                    ;2   =  11
Lf9cf
    ldy     #$0f                    ;2   =   2
Lf9d1
    sta     AUDF1                   ;3        
    sty     AUDC1                   ;3   =   6
Lf9d5
    stx     AUDV1                   ;3        
    jsr     Lf9dd                   ;6        
    jmp     Lf494                   ;3   =  12
    
Lf9dd
    lda     INTIM                   ;4        
    bpl     Lf9dd                   ;2/3 =   6
Lf9e2
    lda     #$0e                    ;2   =   2
Lf9e4
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     VSYNC                   ;3        
    lsr                             ;2        
    bne     Lf9e4                   ;2/3      
    ldy     #$23                    ;2        
    sty     TIM64T                  ;4        
    inc     ram_DC                  ;5        
    rts                             ;6   =  24
    
Lf9f3
    lda     #$01                    ;2        
    tax                             ;2        
    tay                             ;2        
    bit     ram_DC                  ;3        
    beq     Lf9fd                   ;2/3      
    ldx     #$03                    ;2   =  13
Lf9fd
    rts                             ;6   =   6
    
Lf9fe
    lda     ram_BE,x                ;4        
    beq     Lfa26                   ;2/3      
    lda     ram_C5,x                ;4        
    sec                             ;2        
    adc     ram_E0,x                ;4        
    sta     ram_C5,x                ;4        
    lda     ram_E4,x                ;4        
    ldy     ram_A8,x                ;4   =  28
Lfa0d
    dey                             ;2        
    bmi     Lfa21                   ;2/3      
    beq     Lfa1b                   ;2/3      
    sbc     #$00                    ;2        
    eor     #$ff                    ;2        
    cpy     #$02                    ;2        
    bcc     Lfa21                   ;2/3      
    clc                             ;2   =  16
Lfa1b
    iny                             ;2        
    adc     ram_B5,x                ;4        
    sta     ram_B5,x                ;4        
    rts                             ;6   =  16
    
Lfa21
    iny                             ;2        
    adc     ram_BE,x                ;4        
    sta     ram_BE,x                ;4   =  10
Lfa26
    rts                             ;6   =   6
    
Lfa27
    jmp.ind (ram_E9)                ;5   =   5
Lfa2a
    lda     ram_AD,x                ;4        
    sta     ram_EF                  ;3        
    ldy     ram_C1                  ;3        
    bne     Lfa73                   ;2/3      
    lsr                             ;2        
    lsr                             ;2        
    tay                             ;2        
    lsr                             ;2        
    bcc     Lfa4b                   ;2/3      
    lda     ram_C0                  ;3        
    sbc     ram_C3,x                ;4        
    adc     #$04                    ;2        
    cmp     #$09                    ;2        
    bcs     Lfa73                   ;2/3      
    lda     ram_BA,x                ;4        
    cmp     ram_B7                  ;3        
    tya                             ;2        
    eor     #$01                    ;2        
    bpl     Lfa5a                   ;2/3 =  48
Lfa4b
    lda     ram_B7                  ;3        
    sbc     ram_BA,x                ;4        
    adc     #$05                    ;2        
    cmp     #$09                    ;2        
    bcs     Lfa73                   ;2/3      
    lda     ram_C3,x                ;4        
    cmp     ram_C0                  ;3        
    tya                             ;2   =  22
Lfa5a
    bne     Lfa5f                   ;2/3      
    bcc     Lfa61                   ;2/3      
    clc                             ;2   =   6
Lfa5f
    bcc     Lfa73                   ;2/3 =   2
Lfa61
    sty     ram_AB                  ;3        
    lda     ram_BA,x                ;4        
    adc     Lff9a,y                 ;4        
    sta     ram_B8                  ;3        
    lda     ram_C3,x                ;4        
    adc     Lff9e,y                 ;4        
    sta     ram_C1                  ;3        
    stx     ram_B1                  ;3   =  28
Lfa73
    lda     ram_C3,x                ;4        
    ldy     #$0e                    ;2   =   6
Lfa77
    dey                             ;2        
    dey                             ;2        
    cmp     Lfe72,y                 ;4        
    bcc     Lfa77                   ;2/3      
    bne     Lfa26                   ;2/3      
    sty     ram_B5                  ;3        
    lda     ram_BA,x                ;4        
    ldy     #$12                    ;2   =  21
Lfa86
    dey                             ;2        
    dey                             ;2        
    cmp     Lfe61,y                 ;4        
    bcc     Lfa86                   ;2/3      
    bne     Lfa26                   ;2/3      
    tya                             ;2        
    lsr                             ;2        
    tay                             ;2        
    ldx     ram_B5                  ;3        
    lda     ram_EF                  ;3        
    cmp     #$04                    ;2        
    beq     Lfaa0                   ;2/3      
    jsr     Lfbb9                   ;6        
    sec                             ;2        
    beq     Lfaa1                   ;2/3 =  38
Lfaa0
    clc                             ;2   =   2
Lfaa1
    rol     ram_EC                  ;5        
    lda     ram_EF                  ;3        
    cmp     #$00                    ;2        
    beq     Lfaaf                   ;2/3      
    jsr     Lfbd3                   ;6        
    sec                             ;2        
    beq     Lfab0                   ;2/3 =  22
Lfaaf
    clc                             ;2   =   2
Lfab0
    rol     ram_EC                  ;5        
    lda     ram_EF                  ;3        
    cmp     #$0c                    ;2        
    beq     Lfabe                   ;2/3      
    jsr     Lfbc6                   ;6        
    sec                             ;2        
    beq     Lfabf                   ;2/3 =  22
Lfabe
    clc                             ;2   =   2
Lfabf
    rol     ram_EC                  ;5        
    lda     ram_EF                  ;3        
    cmp     #$08                    ;2        
    beq     Lfacd                   ;2/3      
    jsr     Lfbde                   ;6        
    sec                             ;2        
    beq     Lface                   ;2/3 =  22
Lfacd
    clc                             ;2   =   2
Lface
    rol     ram_EC                  ;5        
    ldx     ram_EB                  ;3        
    ldy     #$05                    ;2        
    lda     ram_B7                  ;3        
    sec                             ;2        
    sbc     ram_BA,x                ;4        
    beq     Lfae4                   ;2/3      
    bcs     Lfae6                   ;2/3      
    ldy     #$00                    ;2        
    sbc     #$00                    ;2        
    eor     #$ff                    ;2        
    .byte   $0c ;NOP                ;4-2 =  31
Lfae4
    ldy     #$08                    ;2   =   2
Lfae6
    sta     ram_B5                  ;3        
    lda     ram_C0                  ;3        
    sec                             ;2        
    sbc     ram_C3,x                ;4        
    beq     Lfaf8                   ;2/3      
    bcs     Lfaf7                   ;2/3      
    sbc     #$00                    ;2        
    eor     #$ff                    ;2        
    iny                             ;2        
    iny                             ;2   =  24
Lfaf7
    iny                             ;2   =   2
Lfaf8
    cmp     ram_B5                  ;3        
    bcc     Lfafd                   ;2/3      
    iny                             ;2   =   7
Lfafd
    lda     SWCHA                   ;4        
    ldx     #$01                    ;2        
    lsr                             ;2        
    bcc     Lfb11                   ;2/3      
    inx                             ;2        
    lsr                             ;2        
    bcc     Lfb12                   ;2/3      
    inx                             ;2        
    lsr                             ;2        
    bcc     Lfb12                   ;2/3      
    lsr                             ;2        
    bcs     Lfb1e                   ;2/3      
    dex                             ;2   =  28 *
Lfb11
    dex                             ;2   =   2 *
Lfb12
    lda     Lff58,x                 ;4         *
    and     ram_EC                  ;3         *
    beq     Lfb1e                   ;2/3       *
    sta     ram_DA                  ;3         *
    txa                             ;2         *
    bpl     Lfb4e                   ;2/3 =  16 *
Lfb1e
    lda     ram_CE                  ;3        
    asl                             ;2        
    asl                             ;2        
    sta     ram_B6                  ;3        
    ora     #$03                    ;2        
    tax                             ;2        
    lda     Lff5c,y                 ;4   =  18
Lfb2a
    sta     ram_B5                  ;3        
    and     #$03                    ;2        
    tay                             ;2        
    lda     ram_DB                  ;3        
    cmp     Lff69,x                 ;4        
    bcc     Lfb3d                   ;2/3      
    lda     Lff58,y                 ;4        
    and     ram_EC                  ;3        
    bne     Lfb4d                   ;2/3 =  25
Lfb3d
    lda     ram_B5                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    dex                             ;2        
    cpx     ram_B6                  ;3        
    bpl     Lfb2a                   ;2/3      
    ldy     #$ff                    ;2   =  16
Lfb48
    iny                             ;2        
    lsr     ram_EC                  ;5        
    bcc     Lfb48                   ;2/3 =   9
Lfb4d
    tya                             ;2   =   2
Lfb4e
    ldx     ram_EB                  ;3        
    ora     ram_AD,x                ;4        
    tay                             ;2        
    lda     Lff7d,y                 ;4        
    sta     ram_AD,x                ;4        
    rts                             ;6   =  23
    
Lfb59
    lda     ram_C3,x                ;4        
    sta     ram_B5                  ;3        
    lda     ram_BA,x                ;4        
    sta     ram_EC                  ;3        
    ldx     #$03                    ;2   =  16
Lfb63
    cpx     ram_EB                  ;3        
    beq     Lfb84                   ;2/3      
    lda     ram_EC                  ;3        
    clc                             ;2        
    adc     Lffa2,y                 ;4        
    sbc     ram_BA,x                ;4        
    cmp     Lffa8,y                 ;4        
    bcs     Lfb84                   ;2/3      
    lda     ram_C3,x                ;4        
    beq     Lfb84                   ;2/3      
    lda     ram_B5                  ;3        
    adc     Lffa3,y                 ;4        
    sbc     ram_C3,x                ;4        
    cmp     Lffa7,y                 ;4        
    bcc     Lfb8a                   ;2/3 =  47
Lfb84
    dex                             ;2        
    bpl     Lfb63                   ;2/3      
    ldx     ram_EB                  ;3        
    rts                             ;6   =  13
    
Lfb8a
    lda     ram_CE                  ;3        
    cmp     #$03                    ;2        
    bcc     Lfb96                   ;2/3      
    lda     ram_AD,x                ;4         *
    and     #$df                    ;2         *
    sta     ram_AD,x                ;4   =  17 *
Lfb96
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
    
Lfbab
    cpy     #$06                    ;2        
    bcs     Lfbb6                   ;2/3 =   4
Lfbaf
    cpy     #$02                    ;2        
    bcc     Lfbb6                   ;2/3      
    lda     ram_9B,x                ;4        
    rts                             ;6   =  14
    
Lfbb6
    lda     ram_8E,x                ;4        
    rts                             ;6   =  10
    
Lfbb9
    cpy     #$01                    ;2        
    bcc     Lfbc5                   ;2/3      
    dey                             ;2        
    jsr     Lfbab                   ;6        
    iny                             ;2        
    and     Lff4f,y                 ;4   =  18
Lfbc5
    rts                             ;6   =   6
    
Lfbc6
    cpy     #$07                    ;2        
    bcc     Lfbcc                   ;2/3      
    bne     Lfbd2                   ;2/3 =   6
Lfbcc
    jsr     Lfbab                   ;6        
    and     Lff50,y                 ;4   =  10
Lfbd2
    rts                             ;6   =   6
    
Lfbd3
    cpx     #$01                    ;2        
    bcc     Lfbea                   ;2/3      
    dex                             ;2        
    jsr     Lfbab                   ;6        
    inx                             ;2        
    bne     Lfbe7                   ;2/3 =  16
Lfbde
    cpx     #$0b                    ;2        
    bcs     Lfbea                   ;2/3      
    inx                             ;2        
    jsr     Lfbab                   ;6        
    dex                             ;2   =  14
Lfbe7
    and     Lfe58,y                 ;4   =   4
Lfbea
    rts                             ;6   =   6
    
Lfbeb
    lda     ram_AD,x                ;4        
    ora     #$20                    ;2        
    cmp     ram_AD,x                ;4        
    beq     Lfc13                   ;2/3!     
    sta     ram_AD,x                ;4        
    lda     #$ff                    ;2        
    sta     ram_A8                  ;3        
    lda     ram_CE                  ;3        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    adc     #$0a                    ;2   =  34
Lfc01
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
Lfc13
    rts                             ;6   =   6
    
Lfc14
    lda     #$00                    ;2   =   2
Lfc16
    sta     ram_EB                  ;3        
    beq     Lfc26                   ;2/3      
    ldy     #$03                    ;2   =   7
Lfc1c
    ldx     #$0c                    ;2   =   2
Lfc1e
    jsr     Lfbaf                   ;6        
    and     Lfe58,y                 ;4        
    beq     Lfc4e                   ;2/3 =  12
Lfc26
    dey                             ;2        
    bmi     Lfc2e                   ;2/3      
    lda     #$c6                    ;2        
    jsr     Lfc71                   ;6   =  12
Lfc2e
    iny                             ;2        
    iny                             ;2        
    lda     #$b9                    ;2        
    jsr     Lfc71                   ;6        
    dey                             ;2        
    dex                             ;2        
    bmi     Lfc40                   ;2/3      
    dex                             ;2        
    lda     #$de                    ;2        
    jsr     Lfc71                   ;6        
    inx                             ;2   =  30
Lfc40
    inx                             ;2        
    cpx     #$0c                    ;2        
    bcs     Lfc4e                   ;2/3      
    inx                             ;2        
    inx                             ;2        
    lda     #$d3                    ;2        
    jsr     Lfc71                   ;6        
    dex                             ;2        
    dex                             ;2   =  22
Lfc4e
    dex                             ;2        
    dex                             ;2        
    bpl     Lfc1e                   ;2/3      
    dey                             ;2        
    bpl     Lfc1c                   ;2/3      
    tya                             ;2        
    cpy     ram_EB                  ;3        
    bne     Lfc16                   ;2/3 =  17
Lfc5a
    ldx     #$0c                    ;2   =   2
Lfc5c
    lda     ram_8E,x                ;4        
    and     #$77                    ;2        
    sta     ram_8E,x                ;4        
    lda     ram_9B,x                ;4        
    and     #$6e                    ;2        
    sta     ram_9B,x                ;4        
    dex                             ;2        
    dex                             ;2        
    bpl     Lfc5c                   ;2/3      
    ldx     ram_B5                  ;3        
    ldy     ram_B6                  ;3        
    rts                             ;6   =  38
    
Lfc71
    sta     ram_E9                  ;3        
    lda     Lfe58,y                 ;4        
    cpy     #$02                    ;2        
    bcs     Lfc7e                   ;2/3      
    and     ram_8E,x                ;4        
    bcc     Lfc80                   ;2/3 =  17
Lfc7e
    and     ram_9B,x                ;4   =   4
Lfc80
    bne     Lfc9c                   ;2/3      
    jsr     Lfa27                   ;6        
    bne     Lfc9c                   ;2/3      
    cpx     ram_B5                  ;3        
    bne     Lfc8f                   ;2/3      
    cpy     ram_B6                  ;3        
    beq     Lfc9d                   ;2/3 =  20
Lfc8f
    sta     ram_EB                  ;3        
    lda     Lfe58,y                 ;4        
    cpy     #$02                    ;2        
    bcs     Lfcbe                   ;2/3      
    ora     ram_8E,x                ;4        
    sta     ram_8E,x                ;4   =  19
Lfc9c
    rts                             ;6   =   6
    
Lfc9d
    pla                             ;4        
    pla                             ;4        
    clc                             ;2        
    bcc     Lfc5a                   ;2/3 =  12
Lfca2
    lda     Lfd74,y                 ;4        
    pha                             ;3        
    lda     Lfd72,y                 ;4        
    bpl     Lfcb9                   ;2/3 =  13
Lfcab
    lda     Lfec2,y                 ;4        
    eor     ram_81,x                ;4        
    sta     ram_81,x                ;4        
    lda     Lfd7b,y                 ;4        
    pha                             ;3        
    lda     Lfd78,y                 ;4   =  23
Lfcb9
    eor     ram_8E,x                ;4        
    sta     ram_8E,x                ;4        
    pla                             ;4   =  12
Lfcbe
    eor     ram_9B,x                ;4        
    sta     ram_9B,x                ;4        
    rts                             ;6   =  14
    
Lfcc3
    lda     ram_EF                  ;3        
    asl                             ;2        
    cmp     ram_F0                  ;3        
    bcs     Lfccc                   ;2/3      
    lsr     ram_F0                  ;5   =  15
Lfccc
    lda     ram_DF                  ;3        
    lsr                             ;2        
    ror     ram_DE                  ;5        
    bcc     Lfcd5                   ;2/3      
    eor     #$b4                    ;2   =  14
Lfcd5
    sta     ram_DF                  ;3        
    eor     ram_DE                  ;3        
    and     ram_F0                  ;3        
    cmp     ram_EF                  ;3        
    bcc     Lfce1                   ;2/3      
    sbc     ram_EF                  ;3   =  17
Lfce1
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

    jmp Lf534

    ENDIF
  ELSE
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fce4 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fcec (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fcf4 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fcfc (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fd04 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fd0c (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fd14 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fd1c (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fd24 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fd2c (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fd34 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fd3c (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fd44 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fd4c (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fd54 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fd5c (*)
    .byte   $00,$00,$00,$00,$00             ; $fd64 (*)
  ENDIF

    ORG     $fd69

Lfd69
    .byte   $0c,$17,$23,$74                 ; $fd69 (D)
Lfd6d
    .byte   $06,$0d                         ; $fd6d (*)
    .byte   $14                             ; $fd6f (D)
    .byte   $1b,$22                         ; $fd70 (*)
Lfd72
    .byte   $20,$02                         ; $fd72 (D)
Lfd74
    .byte   $00,$00,$04,$40                 ; $fd74 (D)
Lfd78
    .byte   $e0,$3e,$03                     ; $fd78 (D)
Lfd7b
    .byte   $00,$00,$07,$3c,$c0             ; $fd7b (D)
Lfd80
    .byte   $a0                             ; $fd80 (D)
Lfd81
    .byte   $7f,$a6,$bb,$93,$8c,$b4,$86,$ad ; $fd81 (D)
    .byte   $9a,$c3                         ; $fd89 (D)
    
    .byte   %00101000 ; |  # #   |            $fd8b (G)
    .byte   %00000000 ; |        |            $fd8c (G)
    .byte   %00110010 ; |  ##  # |            $fd8d (G)
    .byte   %00011010 ; |   ## # |            $fd8e (G)
    .byte   %00110010 ; |  ##  # |            $fd8f (G)
    .byte   %00000000 ; |        |            $fd90 (G)
    .byte   %00101000 ; |  # #   |            $fd91 (G)
    .byte   %00000000 ; |        |            $fd92 (G)
    .byte   %00010010 ; |   #  # |            $fd93 (G)
    .byte   %00011000 ; |   ##   |            $fd94 (G)
    .byte   %10111010 ; |# ### # |            $fd95 (G)
    .byte   %00000000 ; |        |            $fd96 (G)
    .byte   %00101000 ; |  # #   |            $fd97 (G)
    .byte   %00000000 ; |        |            $fd98 (G)
    .byte   %10111010 ; |# ### # |            $fd99 (G)
    .byte   %00011000 ; |   ##   |            $fd9a (G)
    .byte   %00010010 ; |   #  # |            $fd9b (G)
    .byte   %00000000 ; |        |            $fd9c (G)
    .byte   %00101000 ; |  # #   |            $fd9d (G)
    .byte   %00000000 ; |        |            $fd9e (G)
    .byte   %10000010 ; |#     # |            $fd9f (G)
    .byte   %00000000 ; |        |            $fda0 (G)
    .byte   %10000010 ; |#     # |            $fda1 (G)
    .byte   %00000000 ; |        |            $fda2 (G)
    .byte   %00101000 ; |  # #   |            $fda3 (G)
    .byte   %00000100 ; |     #  |            $fda4 (G)
    .byte   %10011010 ; |#  ## # |            $fda5 (G)
    .byte   %00111000 ; |  ###   |            $fda6 (G)
    .byte   %00010010 ; |   #  # |            $fda7 (G)
    .byte   %00000000 ; |        |            $fda8 (G)
    .byte   %00001000 ; |    #   |            $fda9 (G)
    .byte   %00000000 ; |        |            $fdaa (G)
    .byte   %00010010 ; |   #  # |            $fdab (G)
    .byte   %00111000 ; |  ###   |            $fdac (G)
    .byte   %10011010 ; |#  ## # |            $fdad (G)
    .byte   %00000100 ; |     #  |            $fdae (G)
    .byte   %00101000 ; |  # #   |            $fdaf (G)
    .byte   %00000000 ; |        |            $fdb0 (G)
    .byte   %10011010 ; |#  ## # |            $fdb1 (G)
    .byte   %00111000 ; |  ###   |            $fdb2 (G)
    .byte   %10001010 ; |#   # # |            $fdb3 (G)
    .byte   %00000000 ; |        |            $fdb4 (G)
    .byte   %00001000 ; |    #   |            $fdb5 (G)
    .byte   %00000000 ; |        |            $fdb6 (G)
    .byte   %10001010 ; |#   # # |            $fdb7 (G)
    .byte   %00111000 ; |  ###   |            $fdb8 (G)
    .byte   %10011010 ; |#  ## # |            $fdb9 (G)
    .byte   %00000000 ; |        |            $fdba (G)
    .byte   %00101000 ; |  # #   |            $fdbb (G)
    .byte   %00111000 ; |  ###   |            $fdbc (G)
    .byte   %00000000 ; |        |            $fdbd (G)
    .byte   %10010010 ; |#  #  # |            $fdbe (G)
    .byte   %00111000 ; |  ###   |            $fdbf (G)
    .byte   %10101010 ; |# # # # |            $fdc0 (G)
    .byte   %00000000 ; |        |            $fdc1 (G)
    .byte   %00000000 ; |        |            $fdc2 (G)
    .byte   %10101010 ; |# # # # |            $fdc3 (G)
    .byte   %00111000 ; |  ###   |            $fdc4 (G)
    .byte   %10010010 ; |#  #  # |            $fdc5 (G)
    .byte   %00000000 ; |        |            $fdc6 (G)
    .byte   %00111000 ; |  ###   |            $fdc7 (G)
    .byte   %00000000 ; |        |            $fdc8 (G)
    .byte   %00000100 ; |     #  |            $fdc9 (G)
    .byte   %00001000 ; |    #   |            $fdca (G)
    .byte   %10111010 ; |# ### # |            $fdcb (G)
    .byte   %00011000 ; |   ##   |            $fdcc (G)
    .byte   %01000100 ; | #   #  |            $fdcd (G)
    .byte   %00011000 ; |   ##   |            $fdce (G)
    .byte   %10111010 ; |# ### # |            $fdcf (G)
    .byte   %00001000 ; |    #   |            $fdd0 (G)
    .byte   %00000100 ; |     #  |            $fdd1 (G)
    .byte   %00000000 ; |        |            $fdd2 (G)
    .byte   %01000100 ; | #   #  |            $fdd3 (G)
    .byte   %00101000 ; |  # #   |            $fdd4 (G)
    .byte   %10111010 ; |# ### # |            $fdd5 (G)
    .byte   %00010000 ; |   #    |            $fdd6 (G)
    .byte   %01000100 ; | #   #  |            $fdd7 (G)
    .byte   %00010000 ; |   #    |            $fdd8 (G)
    .byte   %10111010 ; |# ### # |            $fdd9 (G)
    .byte   %00101000 ; |  # #   |            $fdda (G)
    .byte   %01000100 ; | #   #  |            $fddb (G)
    .byte   %00000000 ; |        |            $fddc (G)
    .byte   %00010000 ; |   #    |            $fddd (G)
    .byte   %01000100 ; | #   #  |            $fdde (G)
    .byte   %00111010 ; |  ### # |            $fddf (G)
    .byte   %00011010 ; |   ## # |            $fde0 (G)
    .byte   %00010000 ; |   #    |            $fde1 (G)
    .byte   %00000100 ; |     #  |            $fde2 (G)
    .byte   %00010000 ; |   #    |            $fde3 (G)
    .byte   %10111010 ; |# ### # |            $fde4 (G)
    .byte   %00011000 ; |   ##   |            $fde5 (G)
    .byte   %01000100 ; | #   #  |            $fde6 (G)
    .byte   %00010000 ; |   #    |            $fde7 (G)
    .byte   %01000100 ; | #   #  |            $fde8 (G)
    .byte   %00000000 ; |        |            $fde9 (G)
    .byte   %10000010 ; |#     # |            $fdea (G)
    .byte   %00000000 ; |        |            $fdeb (G)
    .byte   %01000100 ; | #   #  |            $fdec (G)
    .byte   %00010000 ; |   #    |            $fded (G)
    .byte   %01000100 ; | #   #  |            $fdee (G)
    .byte   %00011000 ; |   ##   |            $fdef (G)
    .byte   %10111010 ; |# ### # |            $fdf0 (G)
    .byte   %00010000 ; |   #    |            $fdf1 (G)
    .byte   %00000100 ; |     #  |            $fdf2 (G)
    .byte   %00010000 ; |   #    |            $fdf3 (G)
    .byte   %00011010 ; |   ## # |            $fdf4 (G)
    .byte   %00111010 ; |  ### # |            $fdf5 (G)
    .byte   %01000100 ; | #   #  |            $fdf6 (G)
    .byte   %00010000 ; |   #    |            $fdf7 (G)
    .byte   %01000100 ; | #   #  |            $fdf8 (G)
    .byte   %00110000 ; |  ##    |            $fdf9 (G)
    .byte   %00011010 ; |   ## # |            $fdfa (G)
    .byte   %00110000 ; |  ##    |            $fdfb (G)
    .byte   %01000100 ; | #   #  |            $fdfc (G)
    .byte   %00010000 ; |   #    |            $fdfd (G)
    
Lfdfe
    .byte   $da,$bd,$e9,$99,$ff,$9f,$f5,$b7 ; $fdfe (D)
    .byte   $de,$ef,$c9,$d0,$b1,$f9,$93,$e5 ; $fe06 (D)
    .byte   $ab,$d4,$c4,$a5                 ; $fe0e (D)
Lfe12
    .byte   $4e,$2f,$8a,$25,$66,$01,$01,$5f ; $fe12 (*)
    .byte   $7f,$df,$2f,$56,$01,$01         ; $fe1a (*)
    .byte   $5f,$7f,$df,$2f,$40,$01,$01     ; $fe20 (D)
    .byte   $5f,$7f,$df,$2f,$33,$01,$01,$71 ; $fe27 (*)
    .byte   $df,$39,$3b,$40,$01,$02         ; $fe2f (*)
Lfe35
    .byte   $87,$41,$a6,$47                 ; $fe35 (*)
Lfe39
    .byte   $c3,$02,$02,$a4,$d9,$39,$5a,$a4 ; $fe39 (*)
    .byte   $02,$03                         ; $fe41 (*)
    .byte   $a4,$41,$39,$5a,$7b,$02,$03     ; $fe43 (D)
    .byte   $a4,$d9,$39,$5a,$62,$02,$03,$c3 ; $fe4a (*)
    .byte   $8f,$d3,$71,$7b,$03,$03         ; $fe52 (*)
Lfe58
    .byte   $80,$08,$01,$10,$80,$10,$01,$10 ; $fe58 (D)
    .byte   $80                             ; $fe60 (D)
Lfe61
    .byte   $0f,$17,$1f,$27,$2f,$37,$3f,$46 ; $fe61 (D)
    .byte   $4d,$54,$5b,$63,$6b,$73,$7b,$83 ; $fe69 (D)
    .byte   $8b                             ; $fe71 (D)
Lfe72
    .byte   $0a,$10,$17,$1d,$24,$2a,$31,$37 ; $fe72 (D)
    .byte   $3e,$44,$4b,$51,$58             ; $fe7a (D)
    
    .byte   %00111110 ; |  ##### |            $fe7f (G)
    .byte   %00111110 ; |  ##### |            $fe80 (G)
    .byte   %00111110 ; |  ##### |            $fe81 (G)
    .byte   %00001000 ; |    #   |            $fe82 (G)
    .byte   %00001000 ; |    #   |            $fe83 (G)
    .byte   %00001000 ; |    #   |            $fe84 (G)
    .byte   %00011000 ; |   ##   |            $fe85 (G)
    .byte   %00001110 ; |    ### |            $fe86 (G)
    .byte   %00001110 ; |    ### |            $fe87 (G)
    .byte   %00001110 ; |    ### |            $fe88 (G)
    .byte   %00001110 ; |    ### |            $fe89 (G)
    .byte   %00000010 ; |      # |            $fe8a (G)
    .byte   %00000010 ; |      # |            $fe8b (G)
    .byte   %01111110 ; | ###### |            $fe8c (G)
    .byte   %00001110 ; |    ### |            $fe8d (G)
    .byte   %00001110 ; |    ### |            $fe8e (G)
    .byte   %01111110 ; | ###### |            $fe8f (G)
    .byte   %01000000 ; | #      |            $fe90 (G)
    .byte   %01000000 ; | #      |            $fe91 (G)
    .byte   %01111100 ; | #####  |            $fe92 (G)
    .byte   %00001110 ; |    ### |            $fe93 (G)
    .byte   %00001110 ; |    ### |            $fe94 (G)
    .byte   %01111110 ; | ###### |            $fe95 (G)
    .byte   %01000100 ; | #   #  |            $fe96 (G)
    .byte   %01000100 ; | #   #  |            $fe97 (G)
    .byte   %01000000 ; | #      |            $fe98 (G)
    .byte   %01000000 ; | #      |            $fe99 (G)
    .byte   %00001110 ; |    ### |            $fe9a (G)
    .byte   %00001110 ; |    ### |            $fe9b (G)
    .byte   %00001110 ; |    ### |            $fe9c (G)
    .byte   %01111110 ; | ###### |            $fe9d (G)
    .byte   %01000010 ; | #    # |            $fe9e (G)
    .byte   %01000010 ; | #    # |            $fe9f (G)
    .byte   %01111110 ; | ###### |            $fea0 (G)
    .byte   %01001110 ; | #  ### |            $fea1 (G)
    .byte   %01001110 ; | #  ### |            $fea2 (G)
    .byte   %01001110 ; | #  ### |            $fea3 (G)
    .byte   %01000010 ; | #    # |            $fea4 (G)
    .byte   %01000010 ; | #    # |            $fea5 (G)
    .byte   %01111110 ; | ###### |            $fea6 (G)
    .byte   %01110000 ; | ###    |            $fea7 (G)
    .byte   %01110000 ; | ###    |            $fea8 (G)
    .byte   %01111110 ; | ###### |            $fea9 (G)
    .byte   %00000010 ; |      # |            $feaa (G)
    .byte   %00000010 ; |      # |            $feab (G)
    .byte   %00111110 ; |  ##### |            $feac (G)
    .byte   %01111110 ; | ###### |            $fead (G)
    .byte   %01110010 ; | ###  # |            $feae (G)
    .byte   %01110010 ; | ###  # |            $feaf (G)
    .byte   %01111110 ; | ###### |            $feb0 (G)
    .byte   %00100100 ; |  #  #  |            $feb1 (G)
    .byte   %00100100 ; |  #  #  |            $feb2 (G)
    .byte   %00111100 ; |  ####  |            $feb3 (G)
    .byte   %01111110 ; | ###### |            $feb4 (G)
    .byte   %01110010 ; | ###  # |            $feb5 (G)
    .byte   %01110010 ; | ###  # |            $feb6 (G)
    .byte   %01111110 ; | ###### |            $feb7 (G)
    .byte   %01000000 ; | #      |            $feb8 (G)
    .byte   %01000000 ; | #      |            $feb9 (G)
    .byte   %01111100 ; | #####  |            $feba (G)
    .byte   %01111110 ; | ###### |            $febb (G)
    .byte   %00001110 ; |    ### |            $febc (G)
    .byte   %00001110 ; |    ### |            $febd (G)
    .byte   %00111110 ; |  ##### |            $febe (G)
    .byte   %00000100 ; |     #  |            $febf (G)
    .byte   %00000100 ; |     #  |            $fec0 (G)
    .byte   %00111100 ; |  ####  |            $fec1 (G)
    
Lfec2
    .byte   $80                             ; $fec2 (D)
    
    .byte   %00000000 ; |        |            $fec3 (G)
Lfec4
    .byte   %00000000 ; |        |            $fec4 (G)
    .byte   %00000000 ; |        |            $fec5 (G)
    .byte   %00000000 ; |        |            $fec6 (G)
    .byte   %00000000 ; |        |            $fec7 (G)
    .byte   %00000000 ; |        |            $fec8 (G)
    .byte   %00000000 ; |        |            $fec9 (G)
    
    .byte   $8b,$58,$10,$0c,$73,$01         ; $feca (D)
    
    .byte   %00011000 ; |   ##   |            $fed0 (G)
    .byte   %11100000 ; |###     |            $fed1 (G)
    .byte   %11111000 ; |#####   |            $fed2 (G)
    .byte   %00101100 ; |  # ##  |            $fed3 (G)
    .byte   %00011100 ; |   ###  |            $fed4 (G)
    .byte   %00010000 ; |   #    |            $fed5 (G)
    .byte   %11001000 ; |##  #   |            $fed6 (G)
    .byte   %00011000 ; |   ##   |            $fed7 (G)
    .byte   %11100000 ; |###     |            $fed8 (G)
    .byte   %11111000 ; |#####   |            $fed9 (G)
    .byte   %00101100 ; |  # ##  |            $feda (G)
    .byte   %00101100 ; |  # ##  |            $fedb (G)
    .byte   %11000000 ; |##      |            $fedc (G)
    .byte   %00001000 ; |    #   |            $fedd (G)
    .byte   %00101000 ; |  # #   |            $fede (G)
    .byte   %11110000 ; |####    |            $fedf (G)
    .byte   %11011000 ; |## ##   |            $fee0 (G)
    .byte   %11101100 ; |### ##  |            $fee1 (G)
    .byte   %01011100 ; | # ###  |            $fee2 (G)
    .byte   %00010000 ; |   #    |            $fee3 (G)
    .byte   %10101000 ; |# # #   |            $fee4 (G)
    .byte   %00101000 ; |  # #   |            $fee5 (G)
    .byte   %11110000 ; |####    |            $fee6 (G)
    .byte   %11011000 ; |## ##   |            $fee7 (G)
    .byte   %11101100 ; |### ##  |            $fee8 (G)
    .byte   %01101100 ; | ## ##  |            $fee9 (G)
    .byte   %11000000 ; |##      |            $feea (G)
    .byte   %11101000 ; |### #   |            $feeb (G)
    .byte   %11100000 ; |###     |            $feec (G)
    .byte   %11110100 ; |#### #  |            $feed (G)
    .byte   %00001000 ; |    #   |            $feee (G)
    .byte   %00011000 ; |   ##   |            $feef (G)
    .byte   %00010100 ; |   # #  |            $fef0 (G)
    .byte   %11000000 ; |##      |            $fef1 (G)
    .byte   %11111000 ; |#####   |            $fef2 (G)
    .byte   %00010000 ; |   #    |            $fef3 (G)
    .byte   %11110100 ; |#### #  |            $fef4 (G)
    .byte   %00001000 ; |    #   |            $fef5 (G)
    .byte   %00011000 ; |   ##   |            $fef6 (G)
    .byte   %00000100 ; |     #  |            $fef7 (G)
    .byte   %00010000 ; |   #    |            $fef8 (G)
    .byte   %10111000 ; |# ###   |            $fef9 (G)
    
Lfefa
    .byte   $03,$04                         ; $fefa (*)
    .byte   $05                             ; $fefc (D)
    .byte   $05,$05,$00                     ; $fefd (*)
    
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
    .byte   ORANGE|$0                       ; $ff41 (CP)
    .byte   ORANGE|$0                       ; $ff42 (CP)
    .byte   YELLOW|$0                       ; $ff43 (CP)
    .byte   RED|$0                          ; $ff44 (CP)
    .byte   GREEN_YELLOW|$0                 ; $ff45 (CP)
    .byte   MAUVE|$0                        ; $ff46 (CP)
    .byte   GREEN|$0                        ; $ff47 (CP)
    .byte   VIOLET|$0                       ; $ff48 (CP)
    .byte   CYAN_GREEN|$0                   ; $ff49 (CP)
    .byte   PURPLE|$0                       ; $ff4a (CP)
    .byte   CYAN|$0                         ; $ff4b (CP)
    .byte   BLUE|$0                         ; $ff4c (CP)
    .byte   BLUE_CYAN|$0                    ; $ff4d (CP)
    .byte   GREEN_YELLOW|$0                 ; $ff4e (CP)
Lff4f
    .byte   RED|$0                          ; $ff4f (CP)
    
Lff50
    .byte   $20,$02,$04,$40,$40,$04,$02,$20 ; $ff50 (D)
Lff58
    .byte   $01,$02,$04,$08                 ; $ff58 (D)
Lff5c
    .byte   $63,$63,$9c,$4b,$1e,$c9,$e1,$b4 ; $ff5c (D)
    .byte   $c9,$36,$b4                     ; $ff64 (D)
    .byte   $00                             ; $ff67 (*)
    .byte   $1e                             ; $ff68 (D)
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
    .byte   $03                             ; $ff9c (*)
    .byte   $fc                             ; $ff9d (D)
Lff9e
    .byte   $01,$fc                         ; $ff9e (D)
    .byte   $fd                             ; $ffa0 (*)
    .byte   $fb                             ; $ffa1 (D)
Lffa2
    .byte   $08                             ; $ffa2 (D)
Lffa3
    .byte   $0b,$08,$01,$08                 ; $ffa3 (D)
Lffa7
    .byte   $0b                             ; $ffa7 (D)
Lffa8
    .byte   $0f,$0b,$0f,$0b                 ; $ffa8 (D)
Lffac
    .byte   $58,$4c,$bc                     ; $ffac (D)
Lffaf
    .byte   $6a,$2f,$11,$47                 ; $ffaf (D)
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
    .byte   $9a,$5a                         ; $ffc7 (*)
    
    .byte   YELLOW|$a                       ; $ffc9 (C)
    
    .byte   $4a,$6a,$0c                     ; $ffca (*)
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
    .byte   $6a,$4a,$2a,$5a                 ; $ffec (D)

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
    .byte   $9f,$fb
