; Disassembly of Public ROMs\Classic Roms\NTSC\BY COMPANY\Activision\Kung-Fu Master.bin
; Disassembled 01/29/24 14:56:44
; Using Stella 6.6
;
; ROM properties name : Kung-Fu Master (1987) (Activision)
; ROM properties MD5  : 5b92a93b23523ff16e2789b820e2a4c5
; Bankswitch type     : F8* (8K) 
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

PLUSROM           = 1
CHEAT             = 0
PAL60             = 0               ; NTSC timing with the colors of Activision's PAL release


;-----------------------------------------------------------
;      Color constants
;-----------------------------------------------------------

  IF PAL60
; Hues as used by Activision for the PAL release (MD5 4474b3ad3bf6aabe719a2d7f1d1fb4cc)
BLACK            = $00
YELLOW           = $20
BROWN            = $40
ORANGE           = $60
RED              = $60
VIOLET           = $c0
BLUE             = $b0
CYAN             = $d0
GREEN            = $50
GREEN_YELLOW     = $50
  ELSE
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
  ENDIF


;-----------------------------------------------------------
;      TIA and IO constants accessed
;-----------------------------------------------------------

;CXM0P          = $00  ; (Ri)
CXP0FB          = $02  ; (R)
CXP1FB          = $03  ; (R)
CXPPMM          = $07  ; (R)
;INPT1          = $09  ; (Ri)
INPT4           = $0c  ; (R)
;INPT5          = $0d  ; (Ri)

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
RESM0           = $12  ; (W)
RESM1           = $13  ; (W)
RESBL           = $14  ; (W)
AUDC0           = $15  ; (W)
;AUDC1          = $16  ; (Wi)
AUDF0           = $17  ; (W)
;AUDF1          = $18  ; (Wi)
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
HMCLR           = $2b  ; (W)
CXCLR           = $2c  ; (W)

SWCHA           = $0280
SWCHB           = $0282
INTIM           = $0284
TIM64T          = $0296
$298            = $0298
$299            = $0299
$29a            = $029a
$29b            = $029b
TIM1I           = $029c
TIM8I           = $029d
TIM64I          = $029e
T1024I          = $029f


;-----------------------------------------------------------
;      RIOT RAM (zero-page) labels
;-----------------------------------------------------------

ram_80          = $80
ram_81          = $81
ram_82          = $82
ram_83          = $83
ram_84          = $84
ram_85          = $85
ram_86          = $86
ram_87          = $87
ram_88          = $88
;                 $89  (i)
ram_8A          = $8a
ram_8B          = $8b
;                 $8c  (i)
;                 $8d  (i)
;                 $8e  (i)
;                 $8f  (i)
ram_90          = $90
ram_91          = $91
ram_92          = $92
ram_93          = $93
ram_94          = $94
ram_95          = $95
;                 $96  (i)
;                 $97  (i)
ram_98          = $98
ram_99          = $99
ram_9A          = $9a
ram_9B          = $9b
ram_9C          = $9c
ram_9D          = $9d
ram_9E          = $9e
ram_9F          = $9f
;                 $a0  (i)
ram_A1          = $a1
ram_A2          = $a2
ram_A3          = $a3
ram_A4          = $a4
ram_A5          = $a5; (s)
ram_A6          = $a6
ram_A7          = $a7
ram_A8          = $a8
ram_A9          = $a9
ram_AA          = $aa
ram_AB          = $ab
ram_AC          = $ac
;                 $ad  (i)
ram_AE          = $ae
;                 $af  (i)
ram_B0          = $b0
;                 $b1  (i)
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
;                 $c7  (i)
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
ram_D4          = $d4
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
;                 $e3  (i)
;                 $e4  (i)
;                 $e5  (i)
;                 $e6  (i)
;                 $e7  (i)
;                 $e8  (i)
;                 $e9  (i)
ram_EA          = $ea
ram_EB          = $eb
ram_EC          = $ec
;                 $ed  (i)
ram_EE          = $ee
;                 $ef  (i)
ram_F0          = $f0; (s)
;                 $f1  (i)
ram_F2          = $f2
;                 $f3  (i)
ram_F4          = $f4
;                 $f5  (i)
ram_F6          = $f6
ram_F7          = $f7
ram_F8          = $f8
ram_F9          = $f9
ram_FA          = $fa

;                 $fc  (s)
;                 $fd  (s)
;                 $fe  (s)
;                 $ff  (s)


;-----------------------------------------------------------
;      User Defined Labels
;-----------------------------------------------------------
  IF PLUSROM == 1
WriteToBuffer           = $1FF0
WriteSendBuffer         = $1FF1
ReceiveBuffer           = $1FF2
ReceiveBufferSize       = $1FF3

HIGHSCORE_ID            = 99         ; Kung-Fu Master game ID in Highscore DB

; The PlusROM version uses F6 (16K) bankswitching:
;   bank 0, 1 : the original game (bank 0 and 1 of the F8 ROM)
;   bank 2    : empty
;   bank 3    : PlusROM HSC code, PlusROM API, HSC TOP-3table
; The original F8 hotspot $1ff9 selects bank 3 with F6, so the bit at Start_1+1
; becomes the bankswitch bank 1 -> bank 3 (-> bank 0).
HSC_SEND_DELAY          = 120        ; frames after the score request until the HSC table can be shown

BANK0                   = $fff6
BANK1                   = $fff7
BANK2                   = $fff8
BANK3                   = $fff9
  ENDIF


;***********************************************************
;      Bank 0 / 0..1
;***********************************************************

    SEG     CODE
    ORG     $0000
    RORG    $d000

Start_0
    sei                             ;2         *
  IF PLUSROM == 1
    bit     BANK1                   ;4   -> bank 1, continues at Start_1_Cold ($f004)
    nop                             ;2         *
    nop                             ;2         *
    nop                             ;2         *
    nop                             ;2         *
    nop                             ;2         *
; The nops are never executed, used for the bankswitch bank 0 -> bank 1 (via bank 3)
Ldff2
    bit     BANK3                   ;4   -> bank 3, continues at B3_FromBank0
  ELSE
    bit     Lfff9                   ;4         *
    nop                             ;2         *
    nop                             ;2         *
    nop                             ;2         *
    nop                             ;2         *
    nop                             ;2         *
    nop                             ;2         *
    nop                             ;2         *
    nop                             ;2   =  22 *
  ENDIF
  if * != $d00c
    echo "ERROR: Ld00c must be at $d00c, but is at ", *
    err
  endif
Ld00c
    bit     ram_CF                  ;3        
    bpl     Ld020                   ;2/3      
    bvs     Ld029                   ;2/3      
    ldx     ram_D2                  ;3        
    inx                             ;2        
    stx     ram_9A                  ;3        
    ldx     #$00                    ;2        
    stx     ram_98                  ;3        
    stx     ram_99                  ;3        
    jmp     Ld07e                   ;3   =  26
    
Ld020
    bvs     Ld025                   ;2/3      
    jsr     Ld7ae                   ;6   =   8
Ld025
    ldx     #$00                    ;2        
    bit     ram_A2                  ;3   =   5
Ld029
    bmi     Ld07e                   ;2/3      
    bvc     Ld02e                   ;2/3      
    inx                             ;2   =   6
Ld02e
    ldy     ram_B8                  ;3        
    dec     ram_BA                  ;5        
    bpl     Ld05f                   ;2/3      
    iny                             ;2        
    tya                             ;2        
    cmp     Ldfd3,x                 ;4        
    bcc     Ld058                   ;2/3      
    cmp     #$10                    ;2        
    bcs     Ld07e                   ;2/3      
    dec     ram_BB                  ;5        
    bpl     Ld056                   ;2/3      
    lda     ram_BB                  ;3        
    cmp     Ldfd5,x                 ;4        
    bcs     Ld058                   ;2/3      
    dec     ram_B9                  ;5        
    bpl     Ld052                   ;2/3      
    lda     #$09                    ;2        
    sta     ram_B9                  ;3   =  52
Ld052
    lda     #$02                    ;2        
    sta     ram_BB                  ;3   =   5
Ld056
    ldy     #$00                    ;2   =   2
Ld058
    sty     ram_B8                  ;3        
    lda     Ldcfe,x                 ;4        
    sta     ram_BA                  ;3   =  10
Ld05f
    ldx     ram_B9                  ;3        
    lda     Ldfd7,x                 ;4        
    sta     ram_F7                  ;3        
    lda     #>Ld9d8                 ;2        
    sta     ram_F8                  ;3        
    lda     (ram_F7),y              ;5        
    ldx     #$01                    ;2        
    jsr     Ld7f7                   ;6        
    bit     ram_A2                  ;3        
    bvc     Ld082                   ;2/3      
    dex                             ;2        
    lda     Ld90f,y                 ;4        
    jsr     Ld7f7                   ;6        
    bne     Ld082                   ;2/3 =  47
Ld07e
    lda     #$00                    ;2        
    sta     AUDV1                   ;3   =   5
Ld082
    ldy     ram_C0                  ;3        
    bne     Ld089                   ;2/3 =   5
Ld086
    jmp     Ld105                   ;3   =   3
    
Ld089
    dec     ram_BE                  ;5        
    beq     Ld086                   ;2/3      
    lda     Ldfe1,y                 ;4        
    sta     ram_F7                  ;3        
    lda     #>Start_0                    ;2        
    sta     ram_F8                  ;3        
    ldx     #$08                    ;2        
    jmp.ind (ram_F7)                ;5        
Ld09b
    lda     Ldafd,y                 ;4         *
    sta     AUDF0                   ;3         *
    bne     Ld10d                   ;2/3!      *
Ld0a2
    stx     AUDF0                   ;3        
    bne     Ld0f2                   ;2/3      
Ld0a6
    ldy     #$1b                    ;2        
    sty     ram_B8                  ;3        
    lda     #$ff                    ;2        
    sta     ram_BA                  ;3        
    ldy     #$1b                    ;2        
    lda     ram_80                  ;3        
    and     #$02                    ;2        
    bne     Ld0b8                   ;2/3      
    iny                             ;2        
    .byte   $24 ;bit                ;3-2 =  62
Ld0b8
    dey                             ;2        
    sty     AUDF0                   ;3        
    ldx     ram_BE                  ;3        
    cpx     #$0c                    ;2        
    bcc     Ld10d                   ;2/3!     
    ldx     #$0c                    ;2        
    bne     Ld10d                   ;2/3!     
Ld0c5
    lda     #$10                    ;2        
    sta     AUDF0                   ;3        
    ldy     ram_BE                  ;3        
    ldx     Ld84e,y                 ;4        
    jmp     Ld10d                   ;3   =  31
Ld0d1    
    stx     AUDF0                   ;3        
    lda     #$06                    ;2        
    sec                             ;2        
    sbc     ram_BE                  ;3        
    tax                             ;2        
    jmp     Ld10d                   ;3   =  15
Ld0dc    
    lda     #$1d                    ;2        
    sta     AUDF0                   ;3        
    bne     Ld0f2                   ;2/3      
Ld0e2    
    LDA     #$1D
    STA     AUDF0
    LDY     #$02
    LDA     ram_80
    AND     #$02
    BNE     Ld0f0
    LDY     #$06

Ld0f0
    sty     AUDC0
   
Ld0f2
    ldx     ram_BE                  ;3        
    bne     Ld10d                   ;2/3!     
Ld0f6
    ldx     #$10                    ;2         *
    lda     ram_BE                  ;3         *
    cmp     #$06                    ;2         *
    bcs     Ld0ff                   ;2/3       *
    tax                             ;2   =  23 *
Ld0ff
    stx     AUDF0                   ;3         *
    ldx     #$06                    ;2         *
    bne     Ld10d                   ;2/3 =   7 *
Ld105
    ldx     #$00                    ;2        
    stx     ram_C0                  ;3        
    bit     ram_A2                  ;3        
    bvs     Ld10f                   ;2/3 =  10
Ld10d
    stx     AUDV0                   ;3   =   3
Ld10f
    inc     ram_80                  ;5        
    bne     Ld126                   ;2/3      
    inc     ram_83                  ;5        
    lda     ram_83                  ;3        
    and     #$c7                    ;2        
    sta     ram_83                  ;3        
    and     #$07                    ;2        
    bne     Ld126                   ;2/3      
    inc     ram_82                  ;5        
    bne     Ld126                   ;2/3      
    sec                             ;2         *
    ror     ram_82                  ;5   =  38 *

Ld126
    lda     ram_81                  ;3        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    eor     ram_81                  ;3        
    asl                             ;2        
    rol     ram_81                  ;5        
    ldx     #$06                    ;2   =  21
Ld132
    lda     ram_C6,x                ;4        
    cpx     #$05                    ;2        
    bcc     Ld13a                   ;2/3      
    adc     #$4e                    ;2   =  10
Ld13a
    ldy     #$fd                    ;2        
    sec                             ;2   =   4
Ld13d
    iny                             ;2        
    sbc     #$0f                    ;2        
    bcs     Ld13d                   ;2/3      
    sty     ram_D3,x                ;4        
    eor     #$0f                    ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    adc     #$90                    ;2        
    and     #$f0                    ;2        
    ora     ram_D3,x                ;4        
    sta     ram_D3,x                ;4        
    dex                             ;2        
    bpl     Ld132                   ;2/3      
    ldx     #$04                    ;2   =  38
Ld157
    txa                             ;2        
    asl                             ;2        
    asl                             ;2        
    tay                             ;2        
    lda     ram_98,x                ;4        
    and     #$f0                    ;2        
    lsr                             ;2        
    sta.wy  ram_E0,y                ;5        
    lda     ram_98,x                ;4        
    and     #$0f                    ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    sta.wy  ram_E2,y                ;5        
    dex                             ;2        
    bpl     Ld157                   ;2/3      
    inx                             ;2        
    ldy     #$9d                    ;2   =  46
Ld173
    lda     ram_E0,x                ;4        
    beq     Ld17f                   ;2/3      
    cpx     #$0c                    ;2        
    bcs     Ld18b                   ;2/3      
    ldx     #$0c                    ;2        
    bne     Ld173                   ;2/3 =  14
Ld17f
    cpx     #$0a                    ;2        
    beq     Ld185                   ;2/3      
    sty     ram_E0,x                ;4   =   8
Ld185
    inx                             ;2        
    inx                             ;2        
    cpx     #$12                    ;2        
    bcc     Ld173                   ;2/3 =   8
Ld18b
    bit     ram_CE                  ;3        
    bvs     Ld1a4                   ;2/3      
    ldy     #$09                    ;2        
    ldx     #$12                    ;2   =   9
Ld193
    lda     Ldef5,y                 ;4
    bit     ram_80                  ;3
    bpl     Ld19d                   ;2/3
    lda     Ld905,y                 ;4   =  13

Ld19d
    sta     ram_E0,x                ;4        
    dex                             ;2        
    dex                             ;2        
    dey                             ;2        
    bpl     Ld193                   ;2/3 =  12
Ld1a4
    lda     ram_82                  ;3        
    rol                             ;2        
    rol                             ;2        
    rol                             ;2        
    and     #$02                    ;2   =  11
Ld1ab
    ldx     INTIM                   ;4        
    bne     Ld1ab                   ;2/3      
    sta     WSYNC                   ;3   =   9
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     VBLANK                  ;3        
    lda     #$04                    ;2        
    sta     COLUBK                  ;3        
    ldx     #$9d                    ;2        
    stx     ram_F4                  ;3        
    bit     ram_CE                  ;3        
    bvs     Ld1ca                   ;2/3      
    ldx     #<L_digit_4             ;2   "1984" (Irem)
    bit     ram_80                  ;3
    bmi     Ld1ca                   ;2/3
    ldx     #<L_digit_7             ;2   =  30
Ld1ca
    stx     ram_F9                  ;3        
    ldx     #$df                    ;2        
    sta     REFP0                   ;3        
    sta     REFP1                   ;3        
    stx     VDELP0                  ;3        
    sta     WSYNC                   ;3   =  17
;---------------------------------------
    sta     HMOVE                   ;3        
    stx     ram_FA                  ;3        
    stx     VDELP1                  ;3        
    ldy     #$f3                    ;2        
    lda     #$0c                    ;2        
    sta     COLUP0                  ;3        
    sta     COLUP1                  ;3        
    sty     NUSIZ0                  ;3        
    sty     NUSIZ1                  ;3        
    sta     RESP0                   ;3        
    sta     RESP1                   ;3        
    sty     HMP0                    ;3        
    jsr     Lde00                   ;6        
    ldx     #$0c                    ;2        
    bit     ram_CE                  ;3        
    bvc     Ld1ff                   ;2/3      
    ldx     #GREEN|$c               ;2        
    lda     ram_85                  ;3        
    beq     Ld1ff                   ;2/3      
    ldx     #RED|$a                 ;2   =  56
Ld1ff
    stx     COLUP0                  ;3        
    stx     COLUP1                  ;3        
    lda     ram_EA                  ;3        
    sta     ram_F9                  ;3        
    lda     ram_EB                  ;3        
    sta     ram_FA                  ;3        
    ldx     #$06                    ;2        
    sta     WSYNC                   ;3   =  23
;---------------------------------------
    sta     HMOVE                   ;3   =   3
Ld211
    lda     ram_E2,x                ;4        
    sta     ram_EC,x                ;4        
    dex                             ;2        
    dex                             ;2        
    bpl     Ld211                   ;2/3      
    lda     ram_E0                  ;3        
    sta     ram_F4                  ;3        
    sta     WSYNC                   ;3   =  23
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     ram_F7                  ;3        
    lda.w   ram_F7                  ;4        
    jsr     Lde3e                   ;6        
    jsr     Lde3e                   ;6        
    jsr     Lde00                   ;6        
    lda     #$30                    ;2        
    sta     CTRLPF                  ;3        
    sta     WSYNC                   ;3   =  36
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     VDELP0                  ;3        
    sta     VDELP1                  ;3        
    ldy     ram_85                  ;3        
    ldx     ram_9F,y                ;4        
    lda     ram_A2                  ;3        
    lsr                             ;2        
    bcs     Ld250                   ;2/3      
    bit     ram_A2                  ;3        
    bvs     Ld250                   ;2/3      
    bmi     Ld251                   ;2/3      
    lda     ram_80                  ;3        
    and     #$20                    ;2        
    bne     Ld251                   ;2/3 =  37
Ld250
    dex                             ;2   =   2
Ld251
    lda     Ld5a5,x                 ;4        
    sta     NUSIZ0                  ;3        
    sta     ram_F7                  ;3        
    ldy     #RED|$2                 ;2        
    lda     #$c0                    ;2        
    sta     HMP0                    ;3        
    sta     HMP1                    ;3        
    lda     Ld5b5,x                 ;4        
    sta     WSYNC                   ;3   =  27
;---------------------------------------
    sta     HMOVE                   ;3        
    sty     COLUP0                  ;3        
    sta     ram_F6                  ;3        
    sta     ram_F8                  ;3        
    ldy     ram_85                  ;3        
    lda.wy  ram_9D,y                ;4        
    beq     Ld276                   ;2/3      
    bpl     Ld27a                   ;2/3 =  23
Ld276
    lda     #<L_logo_spacer                    ;2        
    bne     Ld27d                   ;2/3 =   4
Ld27a
    asl                             ;2        
    asl                             ;2        
    asl                             ;2   =   6
Ld27d
    sta     ram_F9                  ;3        
    lda     #>L_logo_spacer                    ;2        
    sta     ram_FA                  ;3        
    lda     #$04                    ;2        
    sta     COLUPF                  ;3        
    lda     #$15                    ;2        
    sta     PF1                     ;3        
    lda     #$0a                    ;2        
    sta     PF2                     ;3        
    ldy     #$07                    ;2        
    sta     HMCLR                   ;3   =  28
Ld293
    lda     #RED|$2                 ;2        
    sta     COLUP1                  ;3        
    lda     Ldbf1,y                 ;4        
    bit     ram_F7                  ;3        
    sta     WSYNC                   ;3   =  15
;---------------------------------------
    sta     HMOVE                   ;3        
    bpl     Ld2a4                   ;2/3      
    bmi     Ld2a6                   ;2/3 =   7
Ld2a4
    sta     GRP0                    ;3   =   3
Ld2a6
    ldx     Ld97f,y                 ;4        
    stx     COLUPF                  ;3        
    ldx     ram_F6                  ;3        
    stx     NUSIZ1                  ;3        
    bit     ram_F8                  ;3        
    bpl     Ld2b5                   ;2/3      
    lda     #$00                    ;2   =  20
Ld2b5
    sta     GRP1                    ;3        
    ldx     #$04                    ;2        
    nop                             ;2        
    lda     (ram_F9),y              ;5        
    stx     NUSIZ1                  ;3        
    sta     GRP1                    ;3        
    lda     #$0c                    ;2        
    sta     COLUP1                  ;3        
    stx     COLUPF                  ;3        
    dey                             ;2        
    bpl     Ld293                   ;2/3      
    iny                             ;2        
    sty     GRP0                    ;3        
    sty     GRP1                    ;3        
    lda     #$04                    ;2        
    sta     WSYNC                   ;3   =  43
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     COLUPF                  ;3        
    dey                             ;2        
    sty     PF0                     ;3        
    sty     PF1                     ;3        
    sty     PF2                     ;3        
  IF PAL60
    lda     #$b2                    ;2        
  ELSE
    lda     #$84                    ;2        
  ENDIF
    sta     COLUBK                  ;3        
    ldx     #$30                    ;2        
    lda     #$0c                    ;2        
    sta     RESP0                   ;3        
    sta     RESP1                   ;3        
    stx     CTRLPF                  ;3        
    stx     NUSIZ1                  ;3        
    inx                             ;2        
    sta     VDELP0                  ;3        
    stx     NUSIZ0                  ;3        
    sta     COLUP0                  ;3        
    sta     COLUP1                  ;3        
    sta     VDELP1                  ;3        
    sta     WSYNC                   ;3   =  58
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     ram_CB                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    tax                             ;2        
    lda     Ld5ac,x                 ;4        
    sta     ram_F9                  ;3        
    lda     ram_D8                  ;3        
    sta     ram_FA                  ;3        
    ldy     #$ff                    ;2        
    sty     ENABL                   ;3        
    lda     #<Ldfa5                    ;2        
    sta     ram_EC                  ;3        
    lda     #<Ldfaa                    ;2        
    sta     ram_EE                  ;3        
    lda     #<Ldfaf                    ;2        
    sta     ram_F0                  ;3        
    lda     #YELLOW|$c              ;2        
    sta     ram_F7                  ;3        
    sta     WSYNC                   ;3   =  55
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     ram_FA                  ;3        
    and     #$0f                    ;2        
    tax                             ;2        
    lda     ram_FA                  ;3   =  13
Ld32a
    dex                             ;2        
    bpl     Ld32a                   ;2/3      
    sta.w   RESBL                   ;4        
    sta     HMBL                    ;3        
    lda     #$10                    ;2        
    sta     HMP1                    ;3        
    ldy     #$04                    ;2        
    jsr     Ld781                   ;6        
    sta     WSYNC                   ;3   =  27
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     #$04                    ;2        
    sta     COLUBK                  ;3        
    iny                             ;2        
    sty     GRP0                    ;3        
    sty     GRP1                    ;3        
    sty     GRP0                    ;3        
    lda     #<Ldfb4                 ;2        
    sta     ram_EC                  ;3        
    lda     #<Ldfb9                 ;2       
    sta     ram_EE                  ;3        
    lda     #<Ldfbe                 ;2        
    sta     ram_F0                  ;3        
  IF PAL60
    lda     #$66                    ;2        
  ELSE
    lda     #$44                    ;2        
  ENDIF
    sta     ram_F7                  ;3        
    sta     WSYNC                   ;3   =  42
;---------------------------------------
    sta     HMOVE                   ;3        
    dey                             ;2        
    sty     PF1                     ;3        
  IF PAL60
    lda     #$b2                    ;2        
  ELSE
    lda     #$84                    ;2        
  ENDIF
    sta     COLUBK                  ;3        
    lda     ram_CC                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    tax                             ;2        
    lda     Ld5ac,x                 ;4        
    sta     ram_F9                  ;3        
    lda     ram_D9                  ;3        
    sta     ram_FA                  ;3        
    sty     ENABL                   ;3        
    sta     WSYNC                   ;3   =  43
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     ram_FA                  ;3        
    and     #$0f                    ;2        
    tax                             ;2        
    lda     ram_FA                  ;3   =  13
Ld381
    dex                             ;2        
    bpl     Ld381                   ;2/3      
    sta.w   RESBL                   ;4        
    sta     HMBL                    ;3        
    ldy     #$04                    ;2        
    jsr     Ld781                   ;6        
    sta     WSYNC                   ;3   =  22
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     #$04                    ;2        
    sta     COLUBK                  ;3        
    iny                             ;2        
    sty     GRP0                    ;3        
    sty     GRP1                    ;3        
    sty     GRP0                    ;3        
    sty     PF0                     ;3        
    sty     PF1                     ;3        
    sty     ENABL                   ;3        
    sty     PF2                     ;3        
    sty     NUSIZ0                  ;3        
    lda     #$30                    ;2        
    sta.w   NUSIZ1                  ;4        
    lda     #$10                    ;2        
    sty     COLUP1                  ;3        
    sta     RESP0                   ;3        
    sta     RESP1                   ;3        
    sta     HMP1                    ;3        
    lda     #$b0                    ;2        
    sta     HMM1                    ;3        
    lda     ram_D5                  ;3        
    and     #$0f                    ;2        
    tax                             ;2        
    lda.w   ram_80                  ;4        
    sta     RESM1                   ;3        
    sta     WSYNC                   ;3   =  76
;---------------------------------------
    sta     HMOVE                   ;3        
    and     #$0f                    ;2        
    lsr                             ;2        
    ldy     ram_DE                  ;3        
    bmi     Ld3d6                   ;2/3      
    cpy     #$06                    ;2        
    beq     Ld3d9                   ;2/3      
    lda     #$02                    ;2        
    .byte   $2c ;bit                ;4-2 =  20
Ld3d6
    ora     #RED|$0                 ;2        
    .byte   $2c ;bit                ;4-2 =   4
Ld3d9
    ora     #YELLOW|$8              ;2        
    sta     COLUPF                  ;3        
    ldy     ram_91                  ;3        
    sty     CTRLPF                  ;3        
    lda     #$00                    ;2        
    ldy     #$0a                    ;2        
    sty     ENAM1                   ;3        
    sta     HMCLR                   ;3        
    sta     WSYNC                   ;3   =  24
;---------------------------------------
    sta     COLUBK                  ;3        
    lda     ram_D5                  ;3        
    sta.w   HMP1                    ;4   =  10
Ld3f2
    dex                             ;2        
    bpl     Ld3f2                   ;2/3      
    sta     RESP1                   ;3   =   7
Ld3f7
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     Ld87b,y                 ;4        
    sta     COLUBK                  ;3        
    dey                             ;2        
    bpl     Ld3f7                   ;2/3!     
    lda     ram_D3                  ;3        
    and     #$0f                    ;2        
    tax                             ;2        
    lda     ram_D4                  ;3        
    sta     WSYNC                   ;3   =  24
;---------------------------------------
    sta.w   HMP0                    ;4        
    and     #$0f                    ;2        
    tay                             ;2        
    sec                             ;2   =  10
Ld411
    dey                             ;2        
    bpl     Ld411                   ;2/3      
    sta     RESP0                   ;3        
    sta     WSYNC                   ;3   =  10
;---------------------------------------
    lda     ram_D3                  ;3        
    sta     HMM0                    ;3        
    ldy     #YELLOW|$c              ;2        
    lda     #$67                    ;2   =  10
Ld420
    dex                             ;2        
    bpl     Ld420                   ;2/3      
    sta     RESM0                   ;3        
    sta     WSYNC                   ;3   =  10
;---------------------------------------
    sta     HMOVE                   ;3        
    sty     COLUBK                  ;3        
    sty     COLUP0                  ;3        
    sbc     ram_A3                  ;3        
    tay                             ;2        
    lda     #$c0                    ;2        
    sta     GRP0                    ;3        
    lda     #$16                    ;2        
    sta     NUSIZ0                  ;3        
    sta     ENAM0                   ;3        
    sta     HMCLR                   ;3        
    sta     HMP0                    ;3        
    lda     #$23                    ;2        
    sec                             ;2        
    sbc     ram_B7                  ;3        
    sta     ram_E0                  ;3        
    lda     ram_C4                  ;3        
    sta     WSYNC                   ;3   =  49
;---------------------------------------
    sta     HMOVE                   ;3        
    tax                             ;2        
    lda     Ld59f,x                 ;4        
    sta     ram_F7                  ;3        
    lda     Ld5a2,x                 ;4        
    sta     ram_F8                  ;3        
    lda     ram_F7                  ;3        
    ldx     #$14                    ;2        
    sta     HMCLR                   ;3   =  27
Ld45c
    lda     (ram_A6),y              ;5        
    sta     ram_F9                  ;3        
    cpx     #$05                    ;2        
    txs                             ;2        
    lda     Ld8f0,x                 ;4        
    tax                             ;2        
    lda     #BLUE|$6                ;2        
    bcs     Ld470                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_F9                  ;3        
    txa                             ;2   =  29
Ld470
    cpy     ram_B4                  ;3        
    sta     WSYNC                   ;3   =   6
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     COLUBK                  ;3        
    stx     COLUP0                  ;3        
    bcs     Ld484                   ;2/3      
    lda     ram_F9                  ;3         *
    sta     GRP1                    ;3         *
    lda     (ram_B2),y              ;5         *
    sta     COLUP1                  ;3   =  25 *
Ld484
    tsx                             ;2        
    dey                             ;2        
    dex                             ;2        
    bpl     Ld45c                   ;2/3      
    inx                             ;2        
    stx     ENAM0                   ;3        
    stx     GRP0                    ;3        
    lda     ram_D6                  ;3        
    sta     HMBL                    ;3        
    and     #$0f                    ;2        
    tax                             ;2        
    dey                             ;2        
    dey                             ;2        
    sty     ram_F9                  ;3        
    lda     ram_D7                  ;3        
    sta     HMP0                    ;3        
    and     #$0f                    ;2        
    tay                             ;2        
    sta     WSYNC                   ;3   =  46
;---------------------------------------
    lda     #YELLOW|$c              ;2        
    sta     COLUBK                  ;3        
    lda     #$00                    ;2        
    sta     COLUP1                  ;3   =  10
Ld4aa
    dey                             ;2        
    bpl     Ld4aa                   ;2/3      
    sta     RESP0                   ;3        
    sta     WSYNC                   ;3   =  10
;---------------------------------------
    lda     #BLUE|$7                ;2        
    sty     VDELP0                  ;3        
    iny                             ;2        
    sty     GRP1                    ;3   =  10
Ld4b8
    dex                             ;2        
    bpl     Ld4b8                   ;2/3      
    sta     RESBL                   ;3        
    sta     WSYNC                   ;3   =  10
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     COLUBK                  ;3        
    ldy     ram_F9                  ;3        
    cpy     ram_B4                  ;3        
    bcs     Ld4d1                   ;2/3      
    lda     (ram_A6),y              ;5         *
    sta     GRP1                    ;3         *
    lda     (ram_B2),y              ;5         *
    sta     COLUP1                  ;3   =  30 *
Ld4d1
    dey                             ;2        
    lda     ram_DA                  ;3        
    sta     REFP0                   ;3        
    sta     ENAM1                   ;3        
    lda     ram_DC                  ;3        
    sta     REFP1                   ;3        
    lda     ram_BF                  ;3        
    ora     #$30                    ;2        
    sta     NUSIZ1                  ;3        
    lda     #$00                    ;2        
    sta     NUSIZ0                  ;3        
    cpy     ram_B4                  ;3        
    bcs     Ld4ec                   ;2/3      
    lda     (ram_A6),y              ;5   =  40 *
Ld4ec
    sta     HMCLR                   ;3        
    sta     WSYNC                   ;3   =   6
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     GRP1                    ;3        
    lda     (ram_B2),y              ;5        
    sta     COLUP1                  ;3        
    dey                             ;2        
    sta     CXCLR                   ;3        
    sty     ram_F9                  ;3        
    ldx     #$1c                    ;2        
    bne     Ld509                   ;2/3 =  26
Ld501
    lda     ram_F7                  ;3        
    nop                             ;2        
    nop                             ;2        
    lda     #$00                    ;2        
    beq     Ld523                   ;2/3 =  11
Ld509
    txa                             ;2        
    tay                             ;2        
    lda     (ram_AC),y              ;5        
    sta     GRP0                    ;3        
    lda     (ram_B0),y              ;5        
    ldy     ram_F9                  ;3        
    cpy     ram_B4                  ;3        
    sta     WSYNC                   ;3   =  26
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     COLUP0                  ;3        
    bcs     Ld501                   ;2/3      
    lda     (ram_B2),y              ;5         *
    sta     COLUP1                  ;3         *
    lda     (ram_A6),y              ;5   =  21 *
Ld523
    sta     GRP1                    ;3        
    dec     ram_F9                  ;5        
    dex                             ;2        
    bne     Ld509                   ;2/3      
    txa                             ;2        
    tay                             ;2        
    lda     (ram_AC),y              ;5        
    sta     GRP0                    ;3        
    lda     (ram_B0),y              ;5        
    ldy     ram_F9                  ;3        
    dec     ram_F9                  ;5        
    cpy     ram_B4                  ;3        
    sta     WSYNC                   ;3   =  43
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     COLUP0                  ;3        
    bcs     Ld54b                   ;2/3      
    lda     (ram_A6),y              ;5         *
    sta     GRP1                    ;3         *
    lda     (ram_B2),y              ;5         *
    sta     COLUP1                  ;3   =  24 *
Ld548
    jmp.ind (ram_F7)                ;5   =   5
Ld54b
    stx     GRP1                    ;3        
    lda     ram_F7                  ;3        
    lda     ram_F7                  ;3        
    lda     ram_F7                  ;3        
    jmp     Ld548                   ;3   =  15
Ld556    
    ldy     #$23                    ;2        
    ldx     ram_E0                  ;3   =   5
Ld55a
    lda     (ram_A4),y              ;5        
    sta     GRP0                    ;3        
    cpx     #$05                    ;2        
    bcs     Ld58b                   ;2/3      
    lda     ram_8B,x                ;4        
    sta     HMBL                    ;3        
    sta     ENABL                   ;3   =  22
Ld568
    txs                             ;2        
    lda     (ram_A8),y              ;5        
    sta     HMP0                    ;3        
    tax                             ;2        
    lda     (ram_B2),y              ;5        
    sta     COLUP1                  ;3        
    sta     HMOVE                   ;3        
    stx     NUSIZ0                  ;3        
    lda     (ram_A6),y              ;5        
    sta     GRP1                    ;3        
    lda     (ram_AE),y              ;5        
    sta.w   COLUP0                  ;4        
    tsx                             ;2        
    dex                             ;2        
    dey                             ;2        
    cpy     #$04                    ;2        
    bcs     Ld55a                   ;2/3      
    ldx     #$03                    ;2        
    jmp     Ld641                   ;3   =  58
    
Ld58b
    lda     ram_F7                  ;3
    lda     ram_F7                  ;3
    bcs     Ld568                   ;2/3 =   8

; The kernel code above must not move (branch page crossings -> broken timing)!
  if * != $d591
    echo "ERROR: Kernel code moved, end is at ", *, " instead of $d591"
    err
  endif
    ORG     $0591
    RORG    $d591
    
Ld591
    .byte   BLACK|$0                        ; $f591 (C)
    .byte   BLACK|$0                        ; $f592 (C)
    .byte   BLACK|$0                        ; $f593 (C)
    .byte   BLACK|$0                        ; $f594 (C)
    .byte   YELLOW|$c                       ; $f595 (C)
    .byte   YELLOW|$a                       ; $f596 (C)
    .byte   YELLOW|$a                       ; $f597 (C)
    .byte   YELLOW|$8                       ; $f598 (C)
    .byte   ORANGE|$2                       ; $f599 (C)
    .byte   ORANGE|$2                       ; $f59a (C)
    .byte   ORANGE|$2                       ; $f59b (C)
    .byte   ORANGE|$2                       ; $f59c (C)
    .byte   ORANGE|$2                       ; $f59d (C)
    .byte   ORANGE|$2                       ; $f59e (C)
    
Ld59f
    .byte   <Ld556,<Ld5bc,<Ld611            ; $f59f (D)
Ld5a2
    .byte   >Ld556,>Ld5bc,>Ld611            ; $f5a2 (D)
Ld5a5
    .byte   $ff,$00                         ; $f5a5 (D)
    .byte   $00,$01,$01,$03,$03             ; $f5a7 (*)
Ld5ac
    .byte   $00,$03,$0f,$3f,$ff             ; $f5ac (D)
    .byte   $ff,$ff,$ff,$ff                 ; $f5b1 (*)
Ld5b5
    .byte   $ff,$ff                         ; $f5b5 (D)
    .byte   $00,$00,$01,$01,$03             ; $f5b7 (*)
    
Ld5bc
    ldy     #$23                    ;2   =   2
Ld5be
    lda     (ram_A4),y              ;5        
    sta     GRP0                    ;3        
    lda     (ram_A8),y              ;5        
    sta     HMP0                    ;3        
    tax                             ;2        
    lda     (ram_AA),y              ;5        
    sta     HMP1                    ;3        
    sta     NUSIZ1                  ;3        
    lda     (ram_AE),y              ;5        
    sta     COLUP0                  ;3        
    lda     (ram_B2),y              ;5        
    sta     WSYNC                   ;3   =  45
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     COLUP1                  ;3        
    stx     NUSIZ0                  ;3        
    lda     (ram_A6),y              ;5        
    sta     GRP1                    ;3        
    dey                             ;2        
    cpy     #$04                    ;2        
    bcs     Ld5be                   ;2/3      
    bcc     Ld5e6                   ;2/3 =  25
Ld5e6
    lda     (ram_A4),y              ;5        
    sta     GRP0                    ;3        
    lda     (ram_A8),y              ;5        
    sta     HMP0                    ;3        
    tax                             ;2        
    lda     (ram_AA),y              ;5        
    sta     HMP1                    ;3        
    sta     WSYNC                   ;3   =  29
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     NUSIZ1                  ;3        
    stx     NUSIZ0                  ;3        
    lda     Ldd00,y                 ;4        
    sta     COLUBK                  ;3        
    lda     (ram_A6),y              ;5        
    sta     GRP1                    ;3        
    dey                             ;2        
    bpl     Ld5e6                   ;2/3!     
    bmi     Ld66e                   ;2/3 =  30
Ld609
    lda     ram_F7                  ;3        
    nop                             ;2        
    nop                             ;2        
    lda     #$00                    ;2        
    beq     Ld637                   ;2/3      
Ld611
    ldx     #$23                    ;2        
    lda     ram_F7                  ;3        
    nop                             ;2        
    nop                             ;2   =  20
Ld617
    txa                             ;2        
    tay                             ;2        
    lda     (ram_A4),y              ;5        
    sta     GRP0                    ;3        
    lda     (ram_A8),y              ;5        
    sta     HMP0                    ;3        
    txs                             ;2        
    tax                             ;2        
    lda     (ram_AE),y              ;5        
    ldy     ram_F9                  ;3        
    cpy     ram_B4                  ;3        
    sta     COLUP0                  ;3        
    sta     HMOVE                   ;3        
    stx     NUSIZ0                  ;3        
    bcs     Ld609                   ;2/3      
    lda     (ram_B2),y              ;5        
    sta     COLUP1                  ;3        
    lda     (ram_A6),y              ;5   =  59
Ld637
    sta     GRP1                    ;3        
    dec     ram_F9                  ;5        
    tsx                             ;2        
    dex                             ;2        
    cpx     #$04                    ;2        
    bcs     Ld617                   ;2/3 =  16
Ld641
    txa                             ;2        
    tay                             ;2        
    lda     (ram_A4),y              ;5        
    sta     GRP0                    ;3        
    lda     (ram_A8),y              ;5        
    sta     HMP0                    ;3        
    txs                             ;2        
    tax                             ;2        
    lda     Ldd00,y                 ;4        
    ldy     ram_F9                  ;3        
    cpy     ram_B4                  ;3        
    sta     WSYNC                   ;3   =  37
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     COLUBK                  ;3        
    bcs     Ld66a                   ;2/3      
    lda     (ram_A6),y              ;5   =  13
Ld65e
    sta     GRP1                    ;3        
    stx     NUSIZ0                  ;3        
    dec     ram_F9                  ;5        
    tsx                             ;2        
    dex                             ;2        
    bpl     Ld641                   ;2/3      
    bmi     Ld66e                   ;2/3 =  19
Ld66a
    lda     #$00                    ;2        
    beq     Ld65e                   ;2/3 =   4
Ld66e
    ldx     #$ff                    ;2        
    txs                             ;2        
    lda     #$30                    ;2        
    sta     WSYNC                   ;3   =   9
;---------------------------------------
    sta     HMOVE                   ;3        
    stx     ENAM1                   ;3        
    inx                             ;2        
    stx     GRP0                    ;3        
    stx     GRP1                    ;3        
    stx     COLUP1                  ;3        
    sta     NUSIZ1                  ;3        
    lda     CXPPMM                  ;3        
    and     #$80                    ;2        
    ora     CXP0FB                  ;3        
    sta     ram_F6                  ;3        
    sta     WSYNC                   ;3   =  34
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     WSYNC                   ;3   =   6
;---------------------------------------
    sta     HMOVE                   ;3        
    ldx     ram_85                  ;3        
    ldy     ram_9F,x                ;4        
    lda     #<Ld88d                    ;2        
    dey                             ;2        
    sty     ram_F9                  ;3        
    beq     Ld69f                   ;2/3      
    lda     #<Ld87f
    
Ld69f
    sta     ram_F7                  ;3        
    lda     #>Ld87f                    ;2        
    sta     ram_F8                  ;3        
    lda     ram_D3                  ;3        
    sta     HMP1                    ;3        
    and     #$0f                    ;2        
    tax                             ;2        
    lda     ram_D4                  ;3        
    sta     HMP0                    ;3        
    and     #$0f                    ;2        
    tay                             ;2        
    sta     WSYNC                   ;3   =  50
;---------------------------------------
    lda     #$00                    ;2        
    sta     COLUBK                  ;3        
    lda     #$06                    ;2        
    sta     NUSIZ0                  ;3   =  10
Ld6bd
    dey                             ;2        
    bpl     Ld6bd                   ;2/3      
    sta     RESP0                   ;3        
    sta     WSYNC                   ;3   =  10
;---------------------------------------
    lda     #BROWN|$6               ;2        
    sta     COLUBK                  ;3        
    ldy     #$00                    ;2        
    lda     ram_F7                  ;3   =  10
Ld6cc
    dex                             ;2        
    bpl     Ld6cc                   ;2/3      
    sta     RESP1                   ;3   =   7
Ld6d1
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     Ldd03,y                 ;4        
    sta     COLUBK                  ;3        
    lda     Ldd75,y                 ;4        
    sta     GRP0                    ;3        
    sta     GRP1                    ;3        
    lda     #$06                    ;2        
    sta     NUSIZ1                  ;3        
    lda     #YELLOW|$c              ;2        
    sta     ENAM1                   ;3        
    sta     COLUP0                  ;3        
    sta     COLUP1                  ;3        
    sta     HMCLR                   ;3        
    iny                             ;2        
    cpy     #$13                    ;2        
    bcc     Ld6d1                   ;2/3      
    ldy     #$0d                    ;2   =  47
Ld6f6
    ldx     Ldad3,y                 ;4        
    lda     ram_F9                  ;3        
    beq     Ld6ff                   ;2/3      
    ldx     #$00
    
Ld6ff
    lda     (ram_F7),y              ;5        
    sta     WSYNC                   ;3   =  17
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     COLUBK                  ;3        
    stx     GRP0                    ;3        
    stx     GRP1                    ;3        
    lda     Ld591,y                 ;4        
    sta     COLUP0                  ;3        
    sta     COLUP1                  ;3        
    dey                             ;2        
    bpl     Ld6f6                   ;2/3!     
    lda     #$1e                    ;2        
    ldx     #$82                    ;2        
    sta     WSYNC                   ;3   =  33
;---------------------------------------
    sta     TIM64T                  ;4        
    stx     VBLANK                  ;3        
    ldy     SWCHA                   ;4        
    iny                             ;2        
    beq     Ld72a                   ;2/3      
    ldy     #$00                    ;2        
    sty     ram_82                  ;3   =  20
Ld72a
    lda     SWCHB                   ;4        
    lsr                             ;2        
    bcc     Ld75e                   ;2/3      
    lsr                             ;2        
    bcs     Ld770                   ;2/3      
    lda     ram_D0                  ;3        
    beq     Ld73b                   ;2/3      
    dec     ram_D0                  ;5        
    bpl     Ld77c                   ;2/3 =  24
Ld73b
    lda     ram_D2                  ;3        
    eor     #$01                    ;2        
    sta     ram_D2                  ;3        
    sty     ram_C0                  ;3        
    sty     AUDV0                   ;3        
    sty     AUDV1                   ;3        
    lda     #$80                    ;2        
    sta     ram_CF                  ;3        
    sty     ram_85                  ;3        
    lsr                             ;2        
    bit     ram_CE                  ;3        
    bvs     Ld758                   ;2/3      
    sta     ram_CE
    sty     ram_9B
    sty     ram_9C
    
Ld758
    ldy     #$25                    ;2        
    sta     ram_A2                  ;3        
    bne     Ld770                   ;2/3 =  39
Ld75e
    bit     ram_CE                  ;3        
    bmi     Ld77c                   ;2/3      
    lda     #$c0                    ;2        
    sta     ram_CE                  ;3        
    sta     ram_CF                  ;3        
    sty     AUDV0                   ;3        
    sty     AUDV1                   ;3        
    iny                             ;2        
    jmp     Ldff2                   ;3   =  24
    
Ld770
    sty     ram_D0                  ;3        
    bit     ram_CE                  ;3        
    bpl     Ld77c                   ;2/3      
    sty     ram_CF                  ;3        
    lda     #$40                    ;2        
    sta     ram_CE                  ;3   =  16
Ld77c
    ldy     #$00                    ;2        
    jmp     Ldff2                   ;3   =   5
    
Ld781
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     (ram_EC),y              ;5        
    sta     GRP0                    ;3        
    lda     (ram_EE),y              ;5        
    sta     GRP1                    ;3        
    lda     (ram_F0),y              ;5        
    ldx     ram_F9                  ;3        
    stx     PF2                     ;3        
    sta     GRP0                    ;3        
    lda     ram_F7                  ;3        
    sta     COLUPF                  ;3        
    lda     #$c0                    ;2        
    sta     PF0                     ;3        
    ldx     #$04                    ;2        
    lda     #$ff                    ;2        
    stx.w   COLUPF                  ;4        
    sta     PF0                     ;3        
    sta     PF2                     ;3        
    sta     HMCLR                   ;3        
    dey                             ;2        
    bpl     Ld781                   ;2/3 =  65
Ld7ad
    rts                             ;6   =   6
    
Ld7ae
    lda     ram_A2                  ;3        
    lsr                             ;2        
    bcs     Ld7ad                   ;2/3      
    lda     ram_C5                  ;3        
    beq     Ld7bf                   ;2/3      
    lda     ram_80                  ;3        
    and     #$07                    ;2        
    bne     Ld7bf                   ;2/3      
    dec     ram_C5                  ;5   =  24
Ld7bf
    lda     ram_CC                  ;3        
    beq     Ld7cf                   ;2/3      
    cmp     #$27                    ;2        
    bcs     Ld7cf                   ;2/3      
    lda     ram_80                  ;3        
    and     #$3f                    ;2        
    bne     Ld7cf                   ;2/3      
    inc     ram_CC                  ;5   =  21
Ld7cf
    bit     ram_BD                  ;3        
    bpl     Ld7f6                   ;2/3      
    lda     ram_B6                  ;3        
    bmi     Ld7f6                   ;2/3      
    ldx     ram_BF                  ;3        
    lda     ram_80                  ;3        
    and     Ld91f,x                 ;4        
    bne     Ld7f6                   ;2/3      
    dec     ram_CB                  ;5        
    bne     Ld7f6                   ;2/3      
    sta     ram_CB                  ;3        
    ldy     #$09                    ;2        
    sty     ram_C0                  ;3        
    lda     #$27                    ;2        
    sta     ram_BE                  ;3        
    lda     #$0c                    ;2        
    sta     AUDC0                   ;3        
    lda     #$01                    ;2
    sta     ram_A2                  ;3   =  52
Ld7f6
    rts                             ;6   =   6

Ld7f7
    sta     AUDF0,x                 ;4        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    and     #$0c                    ;2        
    sta     AUDC0,x                 ;4        
    lda     Ldbfe,x                 ;4
    sta     AUDV0,x                 ;4
    rts                             ;6   =  30


    ORG     $0806
    RORG    $d806
Ld806
    .byte   $00,$54,$38                     ; $f806 (*)
Ld809
    .byte   $7c                             ; $f809 (D)
    .byte   $fe,$ba,$10,$7c,$54,$fe,$fe,$fe ; $f80a (*)
    .byte   $ba,$10,$7c,$54,$d6,$92,$38,$7c ; $f812 (*)
    .byte   $38,$10,$7c,$fe,$c6,$54,$10,$38 ; $f81a (*)
    .byte   $7c,$7c,$7c,$c6,$92,$38,$38,$7c ; $f822 (*)
    .byte   $7c,$38                         ; $f82a (*)
Ld82c
    .byte   $00,$1c,$3e,$76,$63,$63
    .byte   $f1,$c1,$c0,$70,$60,$78,$50,$50 ; $f832 (*)
    .byte   $28,$3c,$38,$2c,$38,$70,$58,$70 ; $f83a (*)
    .byte   $38,$2c,$38,$1e,$14,$0e,$1e,$fc ; $f842 (*)
    .byte   $1f,$3e,$fb,$8e                 ; $f84a (*)
Ld84e
    .byte   $06                             ; $f84e (*)
    .byte   $00                             ; $f84f (A)
    .byte   $01                             ; $f850 (A)
    .byte   $02                             ; $f851 (A)
    .byte   $03                             ; $f852 (A)
    .byte   $02                             ; $f853 (A)
Ld854
    .byte   $00                             ; $f854 (A)
    .byte   $00,$10,$02,$9c,$3e,$7e,$63,$63 ; $f855 (*)
    .byte   $f3,$c1,$c0,$74,$62,$78,$53,$52 ; $f85d (*)
    .byte   $28,$bc,$b9,$2d,$39,$72,$58,$70 ; $f865 (*)
    .byte   $39,$2d,$b8,$de,$14,$8e,$0e,$fc ; $f86d (*)
    .byte   $5f,$3e,$fb,$8e,$26,$30         ; $f875 (*)
    
Ld87b
    .byte   RED|$4                          ; $f87b (CB)
    .byte   RED|$4                          ; $f87c (CB)
    .byte   BLACK|$0                        ; $f87d (CB)
    .byte   BLACK|$0                        ; $f87e (CB)
Ld87f
    .byte   GREEN|$6                        ; $f87f (CB)
    .byte   GREEN|$4                        ; $f880 (CB)
    .byte   GREEN|$4                        ; $f881 (CB)
    .byte   GREEN|$2                        ; $f882 (CB)
    .byte   BLACK|$2                        ; $f883 (CB)
    .byte   GREEN|$6                        ; $f884 (CB)
    .byte   GREEN|$4                        ; $f885 (CB)
    
    .byte   GREEN|$2,$02,GREEN|$6,GREEN|$4,$02,GREEN|$4,GREEN|$2 ; $f886 (*)
    
Ld88d
    .byte   BLUE|$6                         ; $f88d (CB)
    .byte   BLUE|$6                         ; $f88e (CB)
    .byte   BLUE|$6                         ; $f88f (CB)
    .byte   BLUE|$6                         ; $f890 (CB)
    .byte   BLUE|$6                         ; $f891 (CB)
    .byte   BLUE|$6                         ; $f892 (CB)
    .byte   BLUE|$4                         ; $f893 (CB)
    .byte   BLUE|$4                         ; $f894 (CB)
    .byte   BLUE|$2                         ; $f895 (CB)
    .byte   BLUE|$2                         ; $f896 (CB)
    .byte   BLUE|$0                         ; $f897 (CB)
    .byte   BLUE|$0                         ; $f898 (CB)
    .byte   BLUE|$0                         ; $f899 (CB)
    .byte   BLUE|$0                         ; $f89a (CB)
    
    .byte   GREEN|$a,GREEN|$a,GREEN|$a,GREEN|$a,GREEN|$a,GREEN|$a,GREEN|$a,GREEN|$a ; $f89b (*)
    .byte   GREEN|$a,GREEN|$a,GREEN|$a,GREEN|$a,GREEN|$a,GREEN|$c,GREEN|$a,GREEN|$c ; $f8a3 (*)
    .byte   GREEN|$8,GREEN|$c,GREEN|$8,GREEN|$c,GREEN|$a,GREEN|$c,GREEN|$a,GREEN|$a ; $f8ab (*)
    .byte   GREEN|$a,GREEN|$a,GREEN|$a,GREEN|$8,GREEN_YELLOW|$8,GREEN_YELLOW|$a,GREEN_YELLOW|$a,GREEN_YELLOW|$8 ; $f8b3 (*)
    .byte   GREEN_YELLOW|$8,GREEN_YELLOW|$6,$0e,$0e,$0e,$0e,$0e,$0e ; $f8bb (*)
    .byte   $0e,$0e,$0e,$0c,$0c,$0c,$0c,$0c ; $f8c3 (*)
    .byte   $0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c ; $f8cb (*)
    .byte   $0c,$0e,$0e,$0e,$0e,$0c,$0c,$0c ; $f8d3 (*)
    .byte   $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e ; $f8db (*)
    .byte   $0e,ORANGE|$6,ORANGE|$8,ORANGE|$8,ORANGE|$a,ORANGE|$a,ORANGE|$c,ORANGE|$c ; $f8e3 (*)
    .byte   ORANGE|$a,ORANGE|$a,ORANGE|$8,ORANGE|$8,ORANGE|$6 ; $f8eb (*)
    
Ld8f0
    .byte   RED|$2                          ; $f8f0 (C)
    .byte   RED|$2                          ; $f8f1 (C)
    .byte   RED|$2                          ; $f8f2 (C)
    .byte   YELLOW|$c                       ; $f8f3 (C)
    .byte   YELLOW|$c                       ; $f8f4 (C)
    .byte   RED|$4                          ; $f8f5 (C)
    .byte   YELLOW|$8                       ; $f8f6 (C)
    .byte   RED|$4                          ; $f8f7 (C)
    .byte   YELLOW|$8                       ; $f8f8 (C)
    .byte   RED|$4                          ; $f8f9 (C)
    .byte   YELLOW|$8                       ; $f8fa (C)
    .byte   RED|$4                          ; $f8fb (C)
    .byte   YELLOW|$8                       ; $f8fc (C)
    .byte   RED|$4                          ; $f8fd (C)
    .byte   YELLOW|$8                       ; $f8fe (C)
    .byte   RED|$4                          ; $f8ff (C)
    .byte   YELLOW|$8                       ; $f900 (C)
    .byte   RED|$4                          ; $f901 (C)
    .byte   YELLOW|$8                       ; $f902 (C)
    .byte   RED|$4                          ; $f903 (C)
    .byte   YELLOW|$8                       ; $f904 (C)

Ld905
    .byte   <L_logo_spacer
    .byte   <L_logo_spacer
    .byte   <L_logo_spacer
    .byte   <L_logo_irem_1
    .byte   <L_logo_irem_2
    .byte   <L_logo_spacer
    .byte   <L_logo_copyr
    .byte   <L_digit_1,<L_digit_9,<L_digit_8

; Everything up to the "align 256" below must stay at its original address, otherwise
; the graphics tables and the 48 pixel routine at Lde00 cross pages -> broken kernel timing!
  if * != $d90f
    echo "ERROR: Ld90f must be at $d90f, but is at ", *
    err
  endif
Ld90f       ; audio ?
    .byte   $6d,$6f,$72,$74,$77,$7b,$77,$77 ; $f90f (D)
    .byte   $7b,$00,$00,$00,$72,$00,$6f,$6d ; $f917 (D)
Ld91f
    .byte   $0f,$07,$07,$03                 ; $f91f (D)
Ld923
    .byte   $00,$10,$38,$7c,$fe,$ba,$10,$7c ; $f923 (*)
    .byte   $44,$c6,$92,$38,$7c,$38,$10,$7c ; $f92b (*)
    .byte   $fe,$c6,$54,$10,$38,$7c,$7c,$7c ; $f933 (*)
    .byte   $c4,$92,$38,$38,$7c,$7c,$38     ; $f93b (*)
Ld942
    .byte   $00
    .byte   $90,$a8,$aa,$ad,$ad,$d5,$b5,$fd ; $f943 (*)
    .byte   $f9,$f1,$fe,$fe,$f0,$b8,$f8,$9c ; $f94b (*)
    .byte   $cc,$6c,$e8,$f8,$70,$60,$34,$30 ; $f953 (*)
    .byte   $3c,$1c,$18                     ; $f95b (*)
Ld95e
    .byte   $00,$28,$10,$38,$7c
    .byte   $38,$10,$7c,$44,$82,$10,$38,$fe ; $f963 (*)
    .byte   $44,$10,$10,$38,$7c,$7c,$38     ; $f96b (*)
Ld972
    .byte   $00
    .byte   $18,$38,$5e,$5e,$ef,$ef,$f7,$f7 ; $f973 (*)
    .byte   $7a,$7a,$3c,$18                 ; $f97b (*)
    
Ld97f
    .byte   BLACK|$4                        ; $f97f (CP)
    .byte   BLUE|$6                         ; $f980 (CP)
    .byte   BLUE|$6                         ; $f981 (CP)
    .byte   BLUE|$6                         ; $f982 (CP)
    .byte   BLUE|$6                         ; $f983 (CP)
    .byte   BLUE|$6                         ; $f984 (CP)
    .byte   BLACK|$4                        ; $f985 (CP)
    .byte   BLACK|$4                        ; $f986 (CP)
    
    .byte   GREEN|$0,GREEN|$0,GREEN|$0,GREEN|$0,GREEN|$2,GREEN|$2,GREEN|$4,GREEN|$4 ; $f987 (*)
    .byte   GREEN|$4,GREEN|$4,GREEN|$4,GREEN|$2,GREEN|$2 ; $f98f (*)
Ld994
    .byte   $00,$18,$3c
    .byte   $3c,$7e,$7e,$e7                 ; $f997 (*)
    .byte   $db,$bd,$e7,$5a,$5a,$24,$18,$00 ; $f99b (D)
    .byte   $3c,$7e,$e7,$c3                 ; $f9a3 (D)
    .byte   $81,$d3,$42,$08,$22,$10,$00,$3c ; $f9a7 (*)
    .byte   $7e,$e7,$c3,$81,$80,$10,$04,$81 ; $f9af (*)
    .byte   $00,$38,$7d,$e7,$c2,$d0,$80,$42 ; $f9b7 (*)
    .byte   $00,$1b,$3b,$7f,$6e,$66,$30,$30 ; $f9bf (*)
    .byte   $60,$60,$60,$00,$00,$33,$73,$fe ; $f9c7 (*)
    .byte   $de,$cc,$60,$60,$60,$c0,$c0,$c0 ; $f9cf (*)
    .byte   $80                             ; $f9d7 (*)

  if ($100 - (<*)) < (Lda00 - Ld9d8)
    echo	"Page crossed at Ld9d8 - Ld9e8 ",[($100 - (<*))]d , " end:",[(Lda00 - Ld9d8)]d
    ALIGN 256,$ff
  endif
Ld9d8
    .byte   $7b,$00,$7b,$7e,$7b,$77,$7b,$7e ; $f9d8 (D)
Ld9e0
    .byte   $74,$00,$74,$77,$74,$71,$74,$77 ; $f9e0 (D)
Ld9e8
    .byte   $72,$00,$72,$74,$72,$6f,$72,$74 ; $f9e8 (D)

    .byte   $7b,$00,$7b,$77,$77,$00,$72,$72 ; $f9f0 (D)
    .byte   $00,$72,$6f,$6d,$72,$00,$6f,$6d ; $f9f8 (D)
Lda00   
    .byte   %00000000 ; |        |            $fa00 (G)
    .byte   %00110000 ; |  ##    |            $fa01 (G)
    .byte   %00111000 ; |  ###   |            $fa02 (G)
    
    .byte   $0c                             ; $fa03 (*)
    
    .byte   %00011000 ; |   ##   |            $fa04 (G)
    .byte   %00111100 ; |  ####  |            $fa05 (G)
    .byte   %01111100 ; | #####  |            $fa06 (G)
    .byte   %01111000 ; | ####   |            $fa07 (G)
    .byte   %01111000 ; | ####   |            $fa08 (G)
    .byte   %01110000 ; | ###    |            $fa09 (G)
    .byte   %01110000 ; | ###    |            $fa0a (G)
    .byte   %00110000 ; |  ##    |            $fa0b (G)
    .byte   %00011000 ; |   ##   |            $fa0c (G)
    .byte   %01101000 ; | ## #   |            $fa0d (G)
    .byte   %01110100 ; | ### #  |            $fa0e (G)
    .byte   %00110100 ; |  ## #  |            $fa0f (G)
    .byte   %00100100 ; |  #  #  |            $fa10 (G)
    .byte   %00100100 ; |  #  #  |            $fa11 (G)
    .byte   %00101100 ; |  # ##  |            $fa12 (G)
    .byte   %00101100 ; |  # ##  |            $fa13 (G)
    .byte   %01100100 ; | ##  #  |            $fa14 (G)
    .byte   %01101100 ; | ## ##  |            $fa15 (G)
    .byte   %01101100 ; | ## ##  |            $fa16 (G)
    .byte   %01101100 ; | ## ##  |            $fa17 (G)
    .byte   %01111100 ; | #####  |            $fa18 (G)
    .byte   %01111010 ; | #### # |            $fa19 (G)
    .byte   %00111010 ; |  ### # |            $fa1a (G)
    .byte   %00010110 ; |   # ## |            $fa1b (G)
    .byte   %00100011 ; |  #   ##|            $fa1c (G)
    .byte   %01110011 ; | ###  ##|            $fa1d (G)
    .byte   %01110001 ; | ###   #|            $fa1e (G)
    .byte   %01111010 ; | #### # |            $fa1f (G)
    .byte   %10011010 ; |#  ## # |            $fa20 (G)
    .byte   %11111000 ; |#####   |            $fa21 (G)
    .byte   %01111000 ; | ####   |            $fa22 (G)
    .byte   %01110000 ; | ###    |            $fa23 (G)

Lda24
    .byte   %00000000 ; |        |            $fa24 (G)
    .byte   %00011000 ; |   ##   |            $fa25 (G)
    .byte   %00011100 ; |   ###  |            $fa26 (G)
    .byte   %00000110 ; |     ## |            $fa27 (G)
    .byte   %00001100 ; |    ##  |            $fa28 (G)
    .byte   %00011110 ; |   #### |            $fa29 (G)
    .byte   %00111110 ; |  ##### |            $fa2a (G)
    .byte   %00111100 ; |  ####  |            $fa2b (G)
    .byte   %00111100 ; |  ####  |            $fa2c (G)
    .byte   %00111000 ; |  ###   |            $fa2d (G)
    .byte   %00111000 ; |  ###   |            $fa2e (G)
    .byte   %00011000 ; |   ##   |            $fa2f (G)
    .byte   %00011100 ; |   ###  |            $fa30 (G)
    .byte   %00111100 ; |  ####  |            $fa31 (G)
    .byte   %00111110 ; |  ##### |            $fa32 (G)
    .byte   %00111110 ; |  ##### |            $fa33 (G)
    .byte   %00011110 ; |   #### |            $fa34 (G)
    .byte   %10110010 ; |# ##  # |            $fa35 (G)
    .byte   %11100110 ; |###  ## |            $fa36 (G)
    .byte   %00110110 ; |  ## ## |            $fa37 (G)
    .byte   %00011010 ; |   ## # |            $fa38 (G)
    .byte   %00101110 ; |  # ### |            $fa39 (G)
    .byte   %00110110 ; |  ## ## |            $fa3a (G)
    .byte   %00110110 ; |  ## ## |            $fa3b (G)
    .byte   %00111110 ; |  ##### |            $fa3c (G)
    .byte   %00111100 ; |  ####  |            $fa3d (G)
    .byte   %00011100 ; |   ###  |            $fa3e (G)
    .byte   %00001000 ; |    #   |            $fa3f (G)
    .byte   %00010000 ; |   #    |            $fa40 (G)
    .byte   %00111000 ; |  ###   |            $fa41 (G)
    .byte   %00111000 ; |  ###   |            $fa42 (G)
    .byte   %00111100 ; |  ####  |            $fa43 (G)
    .byte   %01001100 ; | #  ##  |            $fa44 (G)
    .byte   %01111100 ; | #####  |            $fa45 (G)
    .byte   %00111100 ; |  ####  |            $fa46 (G)
    .byte   %00111000 ; |  ###   |            $fa47 (G)
Lda48
    .byte   %00000000 ; |        |            $fa48 (G)
    .byte   %11000110 ; |##   ## |            $fa49 (G)
    .byte   %01100011 ; | ##   ##|            $fa4a (G)
    .byte   %00100001 ; |  #    #|            $fa4b (G)
    .byte   %01100110 ; | ##  ## |            $fa4c (G)
    .byte   %11100110 ; |###  ## |            $fa4d (G)
    .byte   %11100111 ; |###  ###|            $fa4e (G)
    .byte   %11100111 ; |###  ###|            $fa4f (G)
    .byte   %11001110 ; |##  ### |            $fa50 (G)
    .byte   %11001100 ; |##  ##  |            $fa51 (G)
    .byte   %11101110 ; |### ### |            $fa52 (G)
    .byte   %11101110 ; |### ### |            $fa53 (G)
    .byte   %01111110 ; | ###### |            $fa54 (G)
    .byte   %01111111 ; | #######|            $fa55 (G)
    .byte   %01111111 ; | #######|            $fa56 (G)
    .byte   %00111111 ; |  ######|            $fa57 (G)
    .byte   %00011111 ; |   #####|            $fa58 (G)
    .byte   %00011111 ; |   #####|            $fa59 (G)
    .byte   %00001111 ; |    ####|            $fa5a (G)
    .byte   %00001101 ; |    ## #|            $fa5b (G)
    .byte   %00011110 ; |   #### |            $fa5c (G)
    .byte   %00111111 ; |  ######|            $fa5d (G)
    .byte   %01010111 ; | # # ###|            $fa5e (G)
    .byte   %11101111 ; |### ####|            $fa5f (G)
    .byte   %11111111 ; |########|            $fa60 (G)
    .byte   %11011111 ; |## #####|            $fa61 (G)
    .byte   %10001111 ; |#   ####|            $fa62 (G)
    .byte   %11000110 ; |##   ## |            $fa63 (G)
    .byte   %11001000 ; |##  #   |            $fa64 (G)
    .byte   %00011100 ; |   ###  |            $fa65 (G)
    .byte   %00011100 ; |   ###  |            $fa66 (G)
    .byte   %00011110 ; |   #### |            $fa67 (G)
    .byte   %00100110 ; |  #  ## |            $fa68 (G)
    .byte   %00111110 ; |  ##### |            $fa69 (G)
    .byte   %00011110 ; |   #### |            $fa6a (G)
    .byte   %00011100 ; |   ###  |            $fa6b (G)
Lda6c
    .byte   %00000000 ; |        |            $fa6c (G)
    .byte   %01100110 ; | ##  ## |            $fa6d (G)
    .byte   %11100111 ; |###  ###|            $fa6e (G)
    .byte   %11100001 ; |###    #|            $fa6f (G)
    .byte   %00100110 ; |  #  ## |            $fa70 (G)
    .byte   %01100111 ; | ##  ###|            $fa71 (G)
    .byte   %11100111 ; |###  ###|            $fa72 (G)
    .byte   %11101110 ; |### ### |            $fa73 (G)
    .byte   %11101110 ; |### ### |            $fa74 (G)
    .byte   %01101110 ; | ## ### |            $fa75 (G)
    .byte   %01110100 ; | ### #  |            $fa76 (G)
    .byte   %01111100 ; | #####  |            $fa77 (G)
    .byte   %01111110 ; | ###### |            $fa78 (G)
    .byte   %00111111 ; |  ######|            $fa79 (G)
    .byte   %00111111 ; |  ######|            $fa7a (G)
    .byte   %00111101 ; |  #### #|            $fa7b (G)
    .byte   %00011100 ; |   ###  |            $fa7c (G)
    .byte   %00011100 ; |   ###  |            $fa7d (G)
    .byte   %11001101 ; |##  ## #|            $fa7e (G)
    .byte   %11111101 ; |###### #|            $fa7f (G)
    .byte   %11111101 ; |###### #|            $fa80 (G)
    .byte   %00111101 ; |  #### #|            $fa81 (G)
    .byte   %00111011 ; |  ### ##|            $fa82 (G)
    .byte   %00111011 ; |  ### ##|            $fa83 (G)
    .byte   %00111010 ; |  ### # |            $fa84 (G)
    .byte   %00011110 ; |   #### |            $fa85 (G)
    .byte   %00011110 ; |   #### |            $fa86 (G)
    .byte   %00001100 ; |    ##  |            $fa87 (G)
    .byte   %00010000 ; |   #    |            $fa88 (G)
    .byte   %00111000 ; |  ###   |            $fa89 (G)
    .byte   %00111000 ; |  ###   |            $fa8a (G)
    .byte   %00111100 ; |  ####  |            $fa8b (G)
    .byte   %01001100 ; | #  ##  |            $fa8c (G)
    .byte   %01111100 ; | #####  |            $fa8d (G)
    .byte   %00111100 ; |  ####  |            $fa8e (G)
    .byte   %00111000 ; |  ###   |            $fa8f (G)
Lda90
    .byte   %00000000 ; |        |            $fa90 (G)
    .byte   %00011000 ; |   ##   |            $fa91 (G)
    .byte   %00011100 ; |   ###  |            $fa92 (G)
    .byte   %00000110 ; |     ## |            $fa93 (G)
    .byte   %00001100 ; |    ##  |            $fa94 (G)
    .byte   %00011110 ; |   #### |            $fa95 (G)
    .byte   %00111110 ; |  ##### |            $fa96 (G)
    .byte   %00111100 ; |  ####  |            $fa97 (G)
    .byte   %00111100 ; |  ####  |            $fa98 (G)
    .byte   %00111000 ; |  ###   |            $fa99 (G)
    .byte   %00111000 ; |  ###   |            $fa9a (G)
    .byte   %00011000 ; |   ##   |            $fa9b (G)
    .byte   %00001100 ; |    ##  |            $fa9c (G)
    .byte   %00110100 ; |  ## #  |            $fa9d (G)
    .byte   %00111010 ; |  ### # |            $fa9e (G)
    .byte   %00011010 ; |   ## # |            $fa9f (G)
    .byte   %00010010 ; |   #  # |            $faa0 (G)
    .byte   %00010010 ; |   #  # |            $faa1 (G)
    .byte   %00010110 ; |   # ## |            $faa2 (G)
    .byte   %00010110 ; |   # ## |            $faa3 (G)
    .byte   %00110010 ; |  ##  # |            $faa4 (G)
    .byte   %00110110 ; |  ## ## |            $faa5 (G)
    .byte   %00110110 ; |  ## ## |            $faa6 (G)
    .byte   %00110110 ; |  ## ## |            $faa7 (G)
    .byte   %00111110 ; |  ##### |            $faa8 (G)
    .byte   %00111100 ; |  ####  |            $faa9 (G)
    .byte   %00011100 ; |   ###  |            $faaa (G)
    .byte   %00001000 ; |    #   |            $faab (G)
    .byte   %00010000 ; |   #    |            $faac (G)
    .byte   %00111000 ; |  ###   |            $faad (G)
    .byte   %00111000 ; |  ###   |            $faae (G)
    .byte   %00111100 ; |  ####  |            $faaf (G)
    .byte   %01001100 ; | #  ##  |            $fab0 (G)
    .byte   %01111100 ; | #####  |            $fab1 (G)
    .byte   %00111100 ; |  ####  |            $fab2 (G)
    .byte   %00111000 ; |  ###   |            $fab3 (G)
    
Ldab4
    .byte   BLACK|$0                        ; $fab4 (C)
    .byte   BLACK|$0                        ; $fab5 (C)
    .byte   BLACK|$0                        ; $fab6 (C)
    .byte   BLACK|$0                        ; $fab7 (C)
    .byte   BLACK|$c                        ; $fab8 (C)
Ldab9
    .byte   BLACK|$c                        ; $fab9 (C)
    .byte   BLACK|$c                        ; $faba (C)
    .byte   BLACK|$c                        ; $fabb (C)
    .byte   BLACK|$c                        ; $fabc (C)
    .byte   BLACK|$c                        ; $fabd (C)
    .byte   BLACK|$c                        ; $fabe (C)
    .byte   BLACK|$c                        ; $fabf (C)
    .byte   BLACK|$c                        ; $fac0 (C)
    .byte   BLACK|$c                        ; $fac1 (C)
    .byte   BLACK|$c                        ; $fac2 (C)
    .byte   BLACK|$0                        ; $fac3 (C)
    .byte   BLACK|$0                        ; $fac4 (C)
    .byte   BLACK|$c                        ; $fac5 (C)
    .byte   BLACK|$c                        ; $fac6 (C)
    .byte   BLACK|$c                        ; $fac7 (C)
    .byte   BLACK|$c                        ; $fac8 (C)
    .byte   BLACK|$c                        ; $fac9 (C)
    .byte   BLACK|$c                        ; $faca (C)
    .byte   BLACK|$c                        ; $facb (C)
    .byte   BLACK|$c                        ; $facc (C)
    .byte   BLACK|$c                        ; $facd (C)
    .byte   BLACK|$c                        ; $face (C)
    .byte   BROWN|$a                        ; $facf (C)
    .byte   BROWN|$a                        ; $fad0 (C)
    .byte   BROWN|$a                        ; $fad1 (C)
    .byte   BLACK|$0                        ; $fad2 (C)
    
Ldad3
    .byte   %00000000 ; |        |            $fad3 (G)
    .byte   %00000000 ; |        |            $fad4 (G)
    .byte   %00000000 ; |        |            $fad5 (G)
    .byte   %00000000 ; |        |            $fad6 (G)
    .byte   %00111100 ; |  ####  |            $fad7 (G)
    .byte   %01111110 ; | ###### |            $fad8 (G)
    .byte   %01100110 ; | ##  ## |            $fad9 (G)
    .byte   %00100100 ; |  #  #  |            $fada (G)
    .byte   %00011000 ; |   ##   |            $fadb (G)
    .byte   %00011000 ; |   ##   |            $fadc (G)
    .byte   %00011000 ; |   ##   |            $fadd (G)
    .byte   %00011000 ; |   ##   |            $fade (G)
    .byte   %00011000 ; |   ##   |            $fadf (G)
    .byte   %00011000 ; |   ##   |            $fae0 (G)
    
Ldae1
    .byte   BLACK|$0                        ; $fae1 (C)
    .byte   BLACK|$8                        ; $fae2 (C)
    .byte   $1c                             ; $fae3 (C)
    .byte   BLACK|$8                        ; $fae4 (C)
    .byte   BLACK|$0                        ; $fae5 (C)
    .byte   BLACK|$0                        ; $fae6 (C)
    .byte   BLACK|$0                        ; $fae7 (C)
    .byte   BLACK|$0                        ; $fae8 (C)
    .byte   BLACK|$0                        ; $fae9 (C)
    .byte   $4e                             ; $faea (C)
    .byte   $2e                             ; $faeb (C)
    .byte   $f6                             ; $faec (C)
    .byte   $74                             ; $faed (C)
    .byte   $28                             ; $faee (C)
    .byte   $d0                             ; $faef (C)
Ldaf0
    .byte   BLACK|$0                        ; $faf0 (C)
    .byte   BLACK|$4                        ; $faf1 (C)
    .byte   $16                             ; $faf2 (C)
    .byte   $3a                             ; $faf3 (C)
    
    .byte   $3a,$16,$2c                     ; $faf4 (*)
Ldaf7
    .byte   $00,$4e,$2c,$f7,$77
    .byte   $2e                             ; $fafc (*)
Ldafd
    .byte   $dc,$05,$04
Ldb00
    .byte   $00,$e4,$62,$66,$6e
    .byte   $7c,$78,$3a,$3e,$3e,$fe,$fe,$fe ; $fb05 (*)
    .byte   $3c,$1c,$18,$18,$18,$38,$38,$10 ; $fb0d (*)
    .byte   $00,$18,$14,$08,$18,$18,$1c,$1c ; $fb15 (*)
    .byte   $7c,$6c,$34,$3c,$3c,$3c,$1c,$18 ; $fb1d (*)
    .byte   $18,$18,$38,$38,$10,$00,$0c,$18 ; $fb25 (*)
    .byte   $30,$38,$3c,$2c,$2c,$34,$38,$18 ; $fb2d (*)
    .byte   $18,$38,$30,$10,$00,$10,$30,$38 ; $fb35 (*)
    .byte   $18,$18,$38,$34,$2c,$2c,$3c,$38 ; $fb3d (*)
    .byte   $30,$18,$0c,$00,GREEN|$8,GREEN|$8,GREEN|$8,GREEN|$8 ; $fb45 (*)
    .byte   $00,ORANGE|$2,ORANGE|$2,ORANGE|$2,ORANGE|$e,ORANGE|$e,$00,$00 ; $fb4d (*)
    .byte   $00,ORANGE|$e,ORANGE|$e,ORANGE|$2,ORANGE|$2,ORANGE|$2,$00,GREEN|$8 ; $fb55 (*)
    .byte   GREEN|$8,GREEN|$8,GREEN|$8      ; $fb5d (*)
    
    .byte   BLACK|$0                        ; $fb60 (C)
    .byte   BLACK|$a                        ; $fb61 (C)
    .byte   BLACK|$a                        ; $fb62 (C)
    .byte   BLACK|$a                        ; $fb63 (C)
    .byte   BLACK|$a                        ; $fb64 (C)
    .byte   BLACK|$a                        ; $fb65 (C)
    .byte   BLACK|$a                        ; $fb66 (C)
    .byte   BLACK|$a                        ; $fb67 (C)
    .byte   BLACK|$a                        ; $fb68 (C)
    .byte   BLACK|$a                        ; $fb69 (C)
    .byte   BLACK|$c                        ; $fb6a (C)
    .byte   BLACK|$c                        ; $fb6b (C)
    .byte   BLACK|$c                        ; $fb6c (C)
    .byte   BLACK|$0                        ; $fb6d (C)
    .byte   BLACK|$c                        ; $fb6e (C)
    .byte   BLACK|$a                        ; $fb6f (C)
    .byte   BLACK|$a                        ; $fb70 (C)
    .byte   BLACK|$a                        ; $fb71 (C)
    .byte   BLACK|$a                        ; $fb72 (C)
    .byte   BLACK|$a                        ; $fb73 (C)
    .byte   BLACK|$a                        ; $fb74 (C)
    .byte   BLACK|$a                        ; $fb75 (C)
    .byte   BLACK|$a                        ; $fb76 (C)
    .byte   BLACK|$a                        ; $fb77 (C)
    .byte   BROWN|$a                        ; $fb78 (C)
    .byte   BROWN|$a                        ; $fb79 (C)
    .byte   BROWN|$a                        ; $fb7a (C)
    .byte   BLACK|$0                        ; $fb7b (C)
    .byte   BLACK|$0                        ; $fb7c (C)
    .byte   BLACK|$c                        ; $fb7d (C)
    .byte   BLACK|$0                        ; $fb7e (C)
    
Ldb7f
    .byte   %00000000 ; |        |            $fb7f (G)
    .byte   %01010100 ; | # # #  |            $fb80 (G)
    .byte   %01011100 ; | # ###  |            $fb81 (G)
    .byte   %11011111 ; |## #####|            $fb82 (G)
    .byte   %11011010 ; |## ## # |            $fb83 (G)
    .byte   %11101100 ; |### ##  |            $fb84 (G)
    .byte   %11011100 ; |## ###  |            $fb85 (G)
    .byte   %11111110 ; |####### |            $fb86 (G)
    .byte   %01111111 ; | #######|            $fb87 (G)
    .byte   %00111111 ; |  ######|            $fb88 (G)
    .byte   %00011110 ; |   #### |            $fb89 (G)
    .byte   %00011110 ; |   #### |            $fb8a (G)
    .byte   %00011110 ; |   #### |            $fb8b (G)
    .byte   %00011111 ; |   #####|            $fb8c (G)
    .byte   %00011111 ; |   #####|            $fb8d (G)
    .byte   %11111100 ; |######  |            $fb8e (G)
    .byte   %10111101 ; |# #### #|            $fb8f (G)
    .byte   %11111101 ; |###### #|            $fb90 (G)
    .byte   %11111111 ; |########|            $fb91 (G)
    .byte   %00111110 ; |  ##### |            $fb92 (G)
    .byte   %11111100 ; |######  |            $fb93 (G)
    .byte   %01101100 ; | ## ##  |            $fb94 (G)
    .byte   %00001100 ; |    ##  |            $fb95 (G)
    .byte   %00011100 ; |   ###  |            $fb96 (G)
    .byte   %00011100 ; |   ###  |            $fb97 (G)
    .byte   %00011110 ; |   #### |            $fb98 (G)
    .byte   %00100110 ; |  #  ## |            $fb99 (G)
    .byte   %00111110 ; |  ##### |            $fb9a (G)
    .byte   %00011110 ; |   #### |            $fb9b (G)
    .byte   %00011100 ; |   ###  |            $fb9c (G)
    .byte   %00000000 ; |        |            $fb9d (G)
    .byte   %00000000 ; |        |            $fb9e (G)
    .byte   %00000000 ; |        |            $fb9f (G)
    .byte   %00000000 ; |        |            $fba0 (G)
    .byte   %00000000 ; |        |            $fba1 (G)
    .byte   %00000000 ; |        |            $fba2 (G)
    
    .byte   GREEN|$8,GREEN|$8,GREEN|$8,GREEN|$8,GREEN|$8,$00,ORANGE|$2,ORANGE|$2 ; $fba3 (D)
    .byte   ORANGE|$2,ORANGE|$2,ORANGE|$2,ORANGE|$2,ORANGE|$e,ORANGE|$e ; $fbab (D)
    
    .byte   ORANGE|$e                       ; $fbb1 (C)
    .byte   BLACK|$0                        ; $fbb2 (C)
    .byte   BLACK|$0                        ; $fbb3 (C)
    
    .byte   $00                             ; $fbb4 (D)
    
    .byte   VIOLET|$4                       ; $fbb5 (C)
    .byte   CYAN|$a                         ; $fbb6 (C)
    .byte   CYAN|$a                         ; $fbb7 (C)
    .byte   CYAN|$a                         ; $fbb8 (C)
    .byte   CYAN|$a                         ; $fbb9 (C)
    .byte   CYAN|$a                         ; $fbba (C)
    .byte   CYAN|$a                         ; $fbbb (C)
    .byte   CYAN|$a                         ; $fbbc (C)
    .byte   CYAN|$a                         ; $fbbd (C)
    .byte   CYAN|$a                         ; $fbbe (C)
    .byte   CYAN|$a                         ; $fbbf (C)
    .byte   CYAN|$a                         ; $fbc0 (C)
    .byte   CYAN|$a                         ; $fbc1 (C)
    .byte   VIOLET|$2                       ; $fbc2 (C)
    .byte   VIOLET|$2                       ; $fbc3 (C)
    .byte   VIOLET|$2                       ; $fbc4 (C)
    .byte   VIOLET|$2                       ; $fbc5 (C)
    .byte   VIOLET|$2                       ; $fbc6 (C)
    .byte   VIOLET|$2                       ; $fbc7 (C)
    .byte   VIOLET|$2                       ; $fbc8 (C)
    .byte   VIOLET|$2                       ; $fbc9 (C)
    .byte   VIOLET|$2                       ; $fbca (C)
    .byte   VIOLET|$2                       ; $fbcb (C)
    .byte   VIOLET|$2                       ; $fbcc (C)
    .byte   ORANGE|$8                       ; $fbcd (C)
    .byte   ORANGE|$8                       ; $fbce (C)
    .byte   ORANGE|$8                       ; $fbcf (C)
    .byte   BLACK|$0                        ; $fbd0 (C)
    .byte   BLACK|$0                        ; $fbd1 (C)
    .byte   BLACK|$0                        ; $fbd2 (C)
    .byte   BLACK|$0                        ; $fbd3 (C)
    .byte   BLACK|$0                        ; $fbd4 (C)
    .byte   BLACK|$0                        ; $fbd5 (C)
    .byte   BLACK|$0                        ; $fbd6 (C)
    .byte   BLACK|$0                        ; $fbd7 (C)
    .byte   BLACK|$0                        ; $fbd8 (C)
    .byte   BLACK|$0                        ; $fbd9 (C)
    .byte   BLACK|$0                        ; $fbda (C)
    .byte   BLACK|$0                        ; $fbdb (C)
    .byte   BLACK|$0                        ; $fbdc (C)
    .byte   BLACK|$c                        ; $fbdd (C)
    .byte   BLACK|$2                        ; $fbde (C)
    .byte   BLACK|$2                        ; $fbdf (C)
    .byte   BLACK|$2                        ; $fbe0 (C)
    .byte   BLACK|$2                        ; $fbe1 (C)
    .byte   BLACK|$2                        ; $fbe2 (C)
    .byte   BLACK|$2                        ; $fbe3 (C)
    .byte   BLACK|$2                        ; $fbe4 (C)
    .byte   BLACK|$2                        ; $fbe5 (C)
    .byte   BLACK|$2                        ; $fbe6 (C)
    .byte   BLACK|$2                        ; $fbe7 (C)
    .byte   ORANGE|$8                       ; $fbe8 (C)
    .byte   ORANGE|$8                       ; $fbe9 (C)
    .byte   ORANGE|$8                       ; $fbea (C)
    .byte   BLACK|$2                        ; $fbeb (C)
    .byte   BLACK|$2                        ; $fbec (C)
    .byte   BLACK|$f                        ; $fbed (C)
    .byte   BLACK|$2                        ; $fbee (C)
    .byte   BLACK|$2                        ; $fbef (C)
    .byte   BLACK|$2                        ; $fbf0 (C)
    
Ldbf1
    .byte   %00000000 ; |        |            $fbf1 (G)
    .byte   %00111100 ; |  ####  |            $fbf2 (G)
    .byte   %00111100 ; |  ####  |            $fbf3 (G)
    .byte   %00111100 ; |  ####  |            $fbf4 (G)
    .byte   %00111100 ; |  ####  |            $fbf5 (G)
    .byte   %00111100 ; |  ####  |            $fbf6 (G)
    .byte   %00000000 ; |        |            $fbf7 (G)
    .byte   %00000000 ; |        |            $fbf8 (G)
    
    .byte   RED|$0,RED|$0,RED|$0,RED|$0,$00 ; $fbf9 (*)
Ldbfe
    .byte   $02,$01
Ldc00
    .byte   $00,$05,$15,$05,$05,$05 ; $fbfe (D)
    .byte   $15,$05,$e5,$35,$00,$f0,$00,$00 ; $fc06 (D)
    .byte   $00,$00,$00,$00                 ; $fc0e (D)
Ldc12
    .byte   $00,$00,$00,$00
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fc16 (D)
    .byte   $00,$00,$00,$00,$00,$00,$10,$00 ; $fc1e (D)
    .byte   $d0,$10,$00,$10,$e0,$e5,$25,$35 ; $fc26 (D)
    .byte   $00,$00,$00,$00,$00,$00,$00,$f0 ; $fc2e (D)
Ldc36
    .byte   $00,$05,$45,$00,$f0,$00,$00,$f0 ; $fc36 (D)
Ldc3e
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fc3e (D)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fc46 (D)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fc4e (D)
    .byte   $00,$00,$00,$00                 ; $fc56 (D)
Ldc5a
    .byte   $00,$00,$00,$00
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fc5e (D)
    .byte   $00,$00,$00,$00,$00,$10,$00,$f0 ; $fc66 (D)
    .byte   $e0,$f5,$05,$f5,$05,$05,$f5,$35 ; $fc6e (D)
    .byte   $00,$00,$00,$00,$00,$00,$00,$10 ; $fc76 (D)
    
Ldc7e
    .byte   %00000000 ; |        |            $fc7e (G)
    .byte   %01111000 ; | ####   |            $fc7f (G)
    .byte   %01110000 ; | ###    |            $fc80 (G)
    .byte   %01100000 ; | ##     |            $fc81 (G)
    .byte   %01100000 ; | ##     |            $fc82 (G)
    .byte   %11110000 ; |####    |            $fc83 (G)
    .byte   %11110000 ; |####    |            $fc84 (G)
    .byte   %11110000 ; |####    |            $fc85 (G)
    .byte   %01110000 ; | ###    |            $fc86 (G)
    .byte   %01110000 ; | ###    |            $fc87 (G)
    .byte   %01111000 ; | ####   |            $fc88 (G)
    .byte   %00111000 ; |  ###   |            $fc89 (G)
    .byte   %00111000 ; |  ###   |            $fc8a (G)
    .byte   %00111000 ; |  ###   |            $fc8b (G)
    .byte   %00111000 ; |  ###   |            $fc8c (G)
    .byte   %00111100 ; |  ####  |            $fc8d (G)
    .byte   %01000100 ; | #   #  |            $fc8e (G)
    .byte   %01000010 ; | #    # |            $fc8f (G)
    .byte   %00111111 ; |  ######|            $fc90 (G)
    .byte   %01111111 ; | #######|            $fc91 (G)
    .byte   %11111001 ; |#####  #|            $fc92 (G)
    .byte   %00011011 ; |   ## ##|            $fc93 (G)
    .byte   %00111111 ; |  ######|            $fc94 (G)
    .byte   %00101001 ; |  # #  #|            $fc95 (G)
    .byte   %01111101 ; | ##### #|            $fc96 (G)
    .byte   %11001111 ; |##  ####|            $fc97 (G)
    .byte   %11000111 ; |##   ###|            $fc98 (G)
    .byte   %10000110 ; |#    ## |            $fc99 (G)
    .byte   %00000100 ; |     #  |            $fc9a (G)
    .byte   %00001110 ; |    ### |            $fc9b (G)
    .byte   %00001110 ; |    ### |            $fc9c (G)
    .byte   %00001111 ; |    ####|            $fc9d (G)
    .byte   %00010011 ; |   #  ##|            $fc9e (G)
    .byte   %00011111 ; |   #####|            $fc9f (G)
    .byte   %00001111 ; |    ####|            $fca0 (G)
    .byte   %00001110 ; |    ### |            $fca1 (G)
Ldca2
    .byte   %00000000 ; |        |            $fca2 (G)
    .byte   %11000110 ; |##   ## |            $fca3 (G)
    .byte   %01100011 ; | ##   ##|            $fca4 (G)
    .byte   %00100001 ; |  #    #|            $fca5 (G)
    .byte   %01100110 ; | ##  ## |            $fca6 (G)
    .byte   %11100110 ; |###  ## |            $fca7 (G)
    .byte   %11100111 ; |###  ###|            $fca8 (G)
    .byte   %11100111 ; |###  ###|            $fca9 (G)
    .byte   %11001110 ; |##  ### |            $fcaa (G)
    .byte   %11001100 ; |##  ##  |            $fcab (G)
    .byte   %11101110 ; |### ### |            $fcac (G)
    .byte   %11101110 ; |### ### |            $fcad (G)
    .byte   %01111110 ; | ###### |            $fcae (G)
    .byte   %01111111 ; | #######|            $fcaf (G)
    .byte   %01111111 ; | #######|            $fcb0 (G)
    .byte   %00111111 ; |  ######|            $fcb1 (G)
    .byte   %00011110 ; |   #### |            $fcb2 (G)
    .byte   %00011110 ; |   #### |            $fcb3 (G)
    .byte   %00011110 ; |   #### |            $fcb4 (G)
    .byte   %00011111 ; |   #####|            $fcb5 (G)
    .byte   %00011111 ; |   #####|            $fcb6 (G)
    .byte   %11111100 ; |######  |            $fcb7 (G)
    .byte   %10111101 ; |# #### #|            $fcb8 (G)
    .byte   %11111101 ; |###### #|            $fcb9 (G)
    .byte   %11111111 ; |########|            $fcba (G)
    .byte   %00111110 ; |  ##### |            $fcbb (G)
    .byte   %11111100 ; |######  |            $fcbc (G)
    .byte   %01101100 ; | ## ##  |            $fcbd (G)
    .byte   %00001100 ; |    ##  |            $fcbe (G)
    .byte   %00011100 ; |   ###  |            $fcbf (G)
    .byte   %00011100 ; |   ###  |            $fcc0 (G)
    .byte   %00011110 ; |   #### |            $fcc1 (G)
    .byte   %00100110 ; |  #  ## |            $fcc2 (G)
    .byte   %00111110 ; |  ##### |            $fcc3 (G)
    .byte   %00011110 ; |   #### |            $fcc4 (G)
    .byte   %00011100 ; |   ###  |            $fcc5 (G)
    
    .byte   $00,$00,$00,$00
Ldcca
    .byte   $00,$00,RED|$0,RED|$0           
    .byte   RED|$0,RED|$0,RED|$0,RED|$0,RED|$0,RED|$0,RED|$0,RED|$0 ; $fcce (D)
    .byte   RED|$0,RED|$0,RED|$0,RED|$0     ; $fcd6 (D)
Ldcda
    .byte   $00,$00,$00,$00
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fcde (D)
    .byte   $00,$00,$00,$00,$00,$f0,$00,$10 ; $fce6 (D)
    .byte   $80,$15,$05,$15,$05,$05,$15,$75 ; $fcee (D)
    .byte   $00,$00,$00,$00,$00,$00,$00,$f0 ; $fcf6 (D)
Ldcfe
    .byte   $05,$06                         ; $fcfe (D)
    
Ldd00
    .byte   BROWN|$6                        ; $fd00 (CB)
    .byte   BLACK|$0                        ; $fd01 (CB)
    .byte   BROWN|$6                        ; $fd02 (CB)
Ldd03
    .byte   BROWN|$6                        ; $fd03 (CB)
    .byte   YELLOW|$c                       ; $fd04 (CB)
    .byte   RED|$2                          ; $fd05 (CB)
    .byte   RED|$2                          ; $fd06 (CB)
    .byte   RED|$2                          ; $fd07 (CB)
    .byte   RED|$2                          ; $fd08 (CB)
    .byte   YELLOW|$c                       ; $fd09 (CB)
    .byte   BLACK|$0                        ; $fd0a (CB)
    .byte   RED|$2                          ; $fd0b (CB)
    .byte   RED|$2                          ; $fd0c (CB)
    .byte   RED|$2                          ; $fd0d (CB)
    .byte   RED|$2                          ; $fd0e (CB)
    .byte   RED|$2                          ; $fd0f (CB)
    .byte   RED|$2                          ; $fd10 (CB)
    .byte   RED|$2                          ; $fd11 (CB)
    .byte   RED|$2                          ; $fd12 (CB)
    .byte   RED|$2                          ; $fd13 (CB)
    .byte   RED|$2                          ; $fd14 (CB)
Ldd15
    .byte   BLACK|$0                        ; $fd15 (CB)
    
    .byte   $05,$45,$00,$f0,$00,$00,$f0,$00 ; $fd16 (D)
    .byte   $00,$00,$00,$f0,$00,$30,$f0,$00 ; $fd1e (D)
    .byte   $f0,$80,$25,$e5,$75,$00,$00,$00 ; $fd26 (D)
    .byte   $00,$00,$00,$00,$10,$00,$00,$00 ; $fd2e (D)
    .byte   $00,$00                         ; $fd36 (D)
Ldd38
    .byte   $00,$05,$65,$00,$10,$00
    .byte   $00,$10,$00,$00,$00,$00,$10,$00 ; $fd3e (D)
    .byte   $d0,$10,$00,$10,$e0,$e5,$25,$35 ; $fd46 (D)
    .byte   $00,$00,$00,$00,$00,$00,$00,$f0 ; $fd4e (D)
 
    .byte   $00,$00,$00,$00,$00
Ldd5b
    .byte   $00,$05,$f5
    .byte   $05,$05,$05,$f5,$05,$25,$75,$00 ; $fd5e (D)
    .byte   $10,$00,$00,$00,$00,$00,$00,$00 ; $fd66 (D)
    .byte   $00,$00,$00,$00,$00,$00,$00     ; $fd6e (D)
    
Ldd75
    .byte   %00000000 ; |        |            $fd75 (G)
    .byte   %00000000 ; |        |            $fd76 (G)
    .byte   %00000000 ; |        |            $fd77 (G)
    .byte   %00000000 ; |        |            $fd78 (G)
    .byte   %00000000 ; |        |            $fd79 (G)
    .byte   %00000000 ; |        |            $fd7a (G)
    .byte   %00000000 ; |        |            $fd7b (G)
    .byte   %00000000 ; |        |            $fd7c (G)
    .byte   %00000000 ; |        |            $fd7d (G)
    .byte   %00000000 ; |        |            $fd7e (G)
    .byte   %11100111 ; |###  ###|            $fd7f (G)
    .byte   %10100101 ; |# #  # #|            $fd80 (G)
    .byte   %10100101 ; |# #  # #|            $fd81 (G)
    .byte   %10100101 ; |# #  # #|            $fd82 (G)
    .byte   %10100101 ; |# #  # #|            $fd83 (G)
    .byte   %10000001 ; |#      #|            $fd84 (G)
    .byte   %11111111 ; |########|            $fd85 (G)
    .byte   %00000000 ; |        |            $fd86 (G)
    .byte   %00000000 ; |        |            $fd87 (G)
    
    .byte   $00                             ; $fd88 (*)
Ldd89
    .byte   $00,$05,$65,$00,$10,$00,$00,$10 ; $fd89 (D)
    .byte   $00,$00,$00,$00                 ; $fd91 (D)
    
    .byte   %00000000 ; |        |            $fd95 (G)
    .byte   %00000000 ; |        |            $fd96 (G)
    .byte   %00000000 ; |        |            $fd97 (G)
    .byte   %00000000 ; |        |            $fd98 (G)
    .byte   %00000000 ; |        |            $fd99 (G)
    .byte   %00000000 ; |        |            $fd9a (G)
    .byte   %00000000 ; |        |            $fd9b (G)
    .byte   %00000000 ; |        |            $fd9c (G)
    .byte   %00000000 ; |        |            $fd9d (G)
    .byte   %00000000 ; |        |            $fd9e (G)
    .byte   %00000000 ; |        |            $fd9f (G)
    .byte   %00000000 ; |        |            $fda0 (G)
    .byte   %00000000 ; |        |            $fda1 (G)
    .byte   %00000000 ; |        |            $fda2 (G)
    .byte   %00000000 ; |        |            $fda3 (G)
    .byte   %00000000 ; |        |            $fda4 (G)
    .byte   %00000000 ; |        |            $fda5 (G)
    .byte   %00000000 ; |        |            $fda6 (G)
    .byte   %00000000 ; |        |            $fda7 (G)
    .byte   %00000000 ; |        |            $fda8 (G)
    .byte   %00000000 ; |        |            $fda9 (G)
    .byte   %00000000 ; |        |            $fdaa (G)
    .byte   %00000000 ; |        |            $fdab (G)
    .byte   %00000000 ; |        |            $fdac (G)
Lddad
    .byte   %00000000 ; |        |            $fdad (G)
    .byte   %00000001 ; |       #|            $fdae (G)
    .byte   %00000001 ; |       #|            $fdaf (G)
    .byte   %00000001 ; |       #|            $fdb0 (G)
    .byte   %00000110 ; |     ## |            $fdb1 (G)
    .byte   %00000111 ; |     ###|            $fdb2 (G)
    .byte   %00000111 ; |     ###|            $fdb3 (G)
    .byte   %00001111 ; |    ####|            $fdb4 (G)
    .byte   %00011110 ; |   #### |            $fdb5 (G)
    .byte   %00111110 ; |  ##### |            $fdb6 (G)
    .byte   %00111100 ; |  ####  |            $fdb7 (G)
    .byte   %01011100 ; | # ###  |            $fdb8 (G)
    .byte   %11101100 ; |### ##  |            $fdb9 (G)
    .byte   %11101110 ; |### ### |            $fdba (G)
    .byte   %01111111 ; | #######|            $fdbb (G)
    .byte   %00111111 ; |  ######|            $fdbc (G)
    .byte   %00011111 ; |   #####|            $fdbd (G)
    .byte   %00011111 ; |   #####|            $fdbe (G)
    .byte   %00000111 ; |     ###|            $fdbf (G)
    .byte   %00001101 ; |    ## #|            $fdc0 (G)
    .byte   %00011110 ; |   #### |            $fdc1 (G)
    .byte   %00111111 ; |  ######|            $fdc2 (G)
    .byte   %01010111 ; | # # ###|            $fdc3 (G)
    .byte   %11101111 ; |### ####|            $fdc4 (G)
    .byte   %11111111 ; |########|            $fdc5 (G)
    .byte   %11011111 ; |## #####|            $fdc6 (G)
    .byte   %10001111 ; |#   ####|            $fdc7 (G)
    .byte   %11000110 ; |##   ## |            $fdc8 (G)
    .byte   %11001000 ; |##  #   |            $fdc9 (G)
    .byte   %00011100 ; |   ###  |            $fdca (G)
    .byte   %00011100 ; |   ###  |            $fdcb (G)
    .byte   %00011110 ; |   #### |            $fdcc (G)
    .byte   %00100110 ; |  #  ## |            $fdcd (G)
    .byte   %00111110 ; |  ##### |            $fdce (G)
    .byte   %00011110 ; |   #### |            $fdcf (G)
    .byte   %00011100 ; |   ###  |            $fdd0 (G)
    .byte   %00000000 ; |        |            $fdd1 (G)
    .byte   %00000000 ; |        |            $fdd2 (G)
    .byte   %00000000 ; |        |            $fdd3 (G)
    .byte   %00000000 ; |        |            $fdd4 (G)
    .byte   %00000000 ; |        |            $fdd5 (G)
    .byte   %00000000 ; |        |            $fdd6 (G)
    .byte   %00000000 ; |        |            $fdd7 (G)
    .byte   %00000000 ; |        |            $fdd8 (G)
    .byte   %00000000 ; |        |            $fdd9 (G)
    .byte   %00000000 ; |        |            $fdda (G)
    .byte   %00000000 ; |        |            $fddb (G)
Ldddc
    .byte   %00000000 ; |        |            $fddc (G)
    .byte   %00000000 ; |        |            $fddd (G)
    .byte   %00000000 ; |        |            $fdde (G)
    .byte   %00000000 ; |        |            $fddf (G)
    .byte   %00000000 ; |        |            $fde0 (G)
    .byte   %00000000 ; |        |            $fde1 (G)
    .byte   %00000000 ; |        |            $fde2 (G)
    .byte   %00000000 ; |        |            $fde3 (G)
    .byte   %00000000 ; |        |            $fde4 (G)
    .byte   %00000000 ; |        |            $fde5 (G)
    .byte   %00000000 ; |        |            $fde6 (G)
    .byte   %00000000 ; |        |            $fde7 (G)
    .byte   %00000000 ; |        |            $fde8 (G)
    .byte   %00000000 ; |        |            $fde9 (G)
    .byte   %00000000 ; |        |            $fdea (G)
    .byte   %00000000 ; |        |            $fdeb (G)
    .byte   %00000000 ; |        |            $fdec (G)
    .byte   %00000000 ; |        |            $fded (G)
    
    .byte   $f0,$00,$30,$f0,$00,$f0,$80,$25 ; $fdee (*)
    .byte   $e5,$75,$00,$00,$00,$00,$00,$00 ; $fdf6 (*)
    .byte   $00,$10                         ; $fdfe (*)
    
Lde00
    ldy     #$06                    ;2        
    sty     ram_F8                  ;3        
    lda     ram_F7                  ;3        
    clc                             ;2        
    bcc     Lde0b                   ;2/3 =  12
Lde09
    sta     HMCLR                   ;3   =   3
Lde0b
    ldy     ram_F8                  ;3        
    lda     (ram_F9),y              ;5        
    sta     ram_F7                  ;3        
    lda     (ram_F2),y              ;5        
    tax                             ;2        
    lda     (ram_F4),y              ;5        
    sta     HMOVE                   ;3        
    sta     GRP0                    ;3        
    lda     (ram_EC),y              ;5        
    sta     GRP1                    ;3        
    lda     (ram_EE),y              ;5        
    sta     GRP0                    ;3        
    lda     (ram_F0),y              ;5        
    ldy     ram_F7                  ;3        
    sta     GRP1                    ;3        
    stx     GRP0                    ;3        
    sty     GRP1                    ;3        
    sta     GRP0                    ;3        
    dec     ram_F8                  ;5        
    bpl     Lde09                   ;2/3      
    sta     WSYNC                   ;3   =  75
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     #$00                    ;2        
    sta     GRP0                    ;3        
    sta     GRP1                    ;3        
    sta     GRP0                    ;3   =  14
Lde3e
    rts                             ;6   =   6
    
    .byte   $00,$18,$3c,$3c,$3c,$3c,$18,$00 ; $fe3f (*)
    .byte   $00,$00                         ; $fe47 (*)
Lde49
    .byte   $00,$18,$3c,$1e,$7c,$ff
    .byte   $fe,$7f,$ff,$3c,$7e,$3c,$18,RED|$0 ; $fe4f (*)
    .byte   RED|$0,RED|$2,RED|$2,RED|$4,RED|$4,RED|$4,RED|$2,RED|$2 ; $fe57 (*)
    .byte   RED|$0,RED|$0,RED|$0,$0e,$0c,$0a,$0c ; $fe5f (*)
    .byte   $0c
    .byte   $0e,$0e,$0c,$0c,$0a,$0a,$0a     ; $fe67 (*)
Lde6e
    .byte   $00
    .byte   $80,$18,$3d,$24,$7e,$81,$81,$7e ; $fe6f (*)
    .byte   $24,$3c,$59,$00,$18,$01,$20,$45 ; $fe77 (*)
    .byte   $5a,$bd,$bd,$5a,$26,$40,$02,$18 ; $fe7f (*)
    .byte   $00,$44,$01,$00,$80,$19,$3c,$3c ; $fe87 (*)
    .byte   $98,$00,$a0,$05                 ; $fe8f (*)
Lde93
    .byte   $00,$18,$01,$20
    .byte   $45,$42,$81,$81,$42,$26,$40,$02 ; $fe97 (*)
    .byte   $18                             ; $fe9f (*)
Ldea0
    .byte   $00,$44,$01,$00,$80,$01,$40
    .byte   $00,$81,$08,$20,$04             ; $fea7 (*)
    
Ldeac
    .byte   %00000000 ; |        |            $feac (G)
    .byte   %01010100 ; | # # #  |            $fead (G)
    .byte   %01011100 ; | # ###  |            $feae (G)
    .byte   %11011111 ; |## #####|            $feaf (G)
    .byte   %11011010 ; |## ## # |            $feb0 (G)
    .byte   %11101100 ; |### ##  |            $feb1 (G)
    .byte   %11011100 ; |## ###  |            $feb2 (G)
    .byte   %11111110 ; |####### |            $feb3 (G)
    .byte   %01111111 ; | #######|            $feb4 (G)
    .byte   %00111111 ; |  ######|            $feb5 (G)
    .byte   %00011111 ; |   #####|            $feb6 (G)
    .byte   %00011111 ; |   #####|            $feb7 (G)
    .byte   %00000111 ; |     ###|            $feb8 (G)
    .byte   %00001101 ; |    ## #|            $feb9 (G)
    .byte   %00011110 ; |   #### |            $feba (G)
    .byte   %00111111 ; |  ######|            $febb (G)
    .byte   %01010111 ; | # # ###|            $febc (G)
    .byte   %11101111 ; |### ####|            $febd (G)
    .byte   %11011111 ; |## #####|            $febe (G)
    .byte   %11011111 ; |## #####|            $febf (G)
    .byte   %10001111 ; |#   ####|            $fec0 (G)
    .byte   %11000110 ; |##   ## |            $fec1 (G)
    .byte   %11001000 ; |##  #   |            $fec2 (G)
    .byte   %00011100 ; |   ###  |            $fec3 (G)
    .byte   %00011100 ; |   ###  |            $fec4 (G)
    .byte   %00011110 ; |   #### |            $fec5 (G)
    .byte   %00100110 ; |  #  ## |            $fec6 (G)
    .byte   %00111110 ; |  ##### |            $fec7 (G)
    .byte   %00011110 ; |   #### |            $fec8 (G)
    .byte   %00011100 ; |   ###  |            $fec9 (G)
    .byte   %00000000 ; |        |            $feca (G)
    .byte   %00000000 ; |        |            $fecb (G)
    .byte   %00000000 ; |        |            $fecc (G)
    .byte   %00000000 ; |        |            $fecd (G)
Ldece
    .byte   %00000000 ; |        |            $fece (G)
    .byte   %00000000 ; |        |            $fecf (G)
    
    .byte   $08,$00,$1c,$be,$7e,$63,$63,$f3 ; $fed0 (*)
    .byte   $c1,$c0,$75,$60,$78,$52,$50,$28 ; $fed8 (*)
    .byte   $3c,$b9,$ac,$38,$70,$58,$72,$38 ; $fee0 (*)
    .byte   $2c,$38,$1e,$54,$0e,$0e,$fc,$5f ; $fee8 (*)
    .byte   $3e,$fb,$8e,$06,$10             ; $fef0 (*)
Ldef5
    .byte   <L_logo_spacer
    .byte   <L_logo_activision_1
    .byte   <L_logo_activision_2
    .byte   <L_logo_activision_3
    .byte   <L_logo_activision_4
    .byte   <L_logo_activision_5
    .byte   <L_logo_copyr
    .byte   <L_digit_1,<L_digit_9,<L_digit_8

    .byte   $00 ; free byte ?

    align 256,$ff
L_digit_0
    .byte   %00111100 ; |  ####  |            $ff00 (G)
    .byte   %01100110 ; | ##  ## |            $ff01 (G)
    .byte   %01100110 ; | ##  ## |            $ff02 (G)
    .byte   %01100110 ; | ##  ## |            $ff03 (G)
    .byte   %01100110 ; | ##  ## |            $ff04 (G)
    .byte   %01100110 ; | ##  ## |            $ff05 (G)
    .byte   %00111100 ; |  ####  |            $ff06 (G)
    
    .byte   $00                             ; $ff07 (*)
    
L_digit_1
    .byte   %00111100 ; |  ####  |            $ff08 (G)
    .byte   %00011000 ; |   ##   |            $ff09 (G)
    .byte   %00011000 ; |   ##   |            $ff0a (G)
    .byte   %00011000 ; |   ##   |            $ff0b (G)
    .byte   %00011000 ; |   ##   |            $ff0c (G)
    .byte   %00111000 ; |  ###   |            $ff0d (G)
    .byte   %00011000 ; |   ##   |            $ff0e (G)
    .byte   %00000000 ; |        |            $ff0f (G)
L_digit_2
    .byte   %01111110 ; | ###### |            $ff10 (G)
    .byte   %01100000 ; | ##     |            $ff11 (G)
    .byte   %01100000 ; | ##     |            $ff12 (G)
    .byte   %00111100 ; |  ####  |            $ff13 (G)
    .byte   %00000110 ; |     ## |            $ff14 (G)
    .byte   %01000110 ; | #   ## |            $ff15 (G)
    .byte   %00111100 ; |  ####  |            $ff16 (G)
    .byte   %00000000 ; |        |            $ff17 (G)
L_digit_3
    .byte   %00111100 ; |  ####  |            $ff18 (G)
    .byte   %01000110 ; | #   ## |            $ff19 (G)
    .byte   %00000110 ; |     ## |            $ff1a (G)
    .byte   %00001100 ; |    ##  |            $ff1b (G)
    .byte   %00000110 ; |     ## |            $ff1c (G)
    .byte   %01000110 ; | #   ## |            $ff1d (G)
    .byte   %00111100 ; |  ####  |            $ff1e (G)
    .byte   %00000000 ; |        |            $ff1f (G)
L_digit_4
    .byte   %00001100 ; |    ##  |            $ff20 (G)
    .byte   %00001100 ; |    ##  |            $ff21 (G)
    .byte   %01111110 ; | ###### |            $ff22 (G)
    .byte   %01001100 ; | #  ##  |            $ff23 (G)
    .byte   %00101100 ; |  # ##  |            $ff24 (G)
    .byte   %00011100 ; |   ###  |            $ff25 (G)
    .byte   %00001100 ; |    ##  |            $ff26 (G)
    
    .byte   $00                             ; $ff27 (*)
    
L_digit_5
    .byte   %01111100 ; | #####  |            $ff28 (G)
    .byte   %01000110 ; | #   ## |            $ff29 (G)
    .byte   %00000110 ; |     ## |            $ff2a (G)
    .byte   %01111100 ; | #####  |            $ff2b (G)
    .byte   %01100000 ; | ##     |            $ff2c (G)
    .byte   %01100000 ; | ##     |            $ff2d (G)
    .byte   %01111110 ; | ###### |            $ff2e (G)
    
    .byte   $00                             ; $ff2f (*)
    
L_digit_6
    .byte   %00111100 ; |  ####  |            $ff30 (G)
    .byte   %01100110 ; | ##  ## |            $ff31 (G)
    .byte   %01100110 ; | ##  ## |            $ff32 (G)
    .byte   %01111100 ; | #####  |            $ff33 (G)
    .byte   %01100000 ; | ##     |            $ff34 (G)
    .byte   %01100010 ; | ##   # |            $ff35 (G)
    .byte   %00111100 ; |  ####  |            $ff36 (G)
    
    .byte   $00                             ; $ff37 (*)
L_digit_7
    .byte   %00011000 ; |   ##   |            $ff38 (G)
    .byte   %00011000 ; |   ##   |            $ff39 (G)
    .byte   %00011000 ; |   ##   |            $ff3a (G)
    .byte   %00001100 ; |    ##  |            $ff3b (G)
    .byte   %00000110 ; |     ## |            $ff3c (G)
    .byte   %01000010 ; | #    # |            $ff3d (G)
    .byte   %01111110 ; | ###### |            $ff3e (G)
    
    .byte   $00                             ; $ff3f (*)
    
L_digit_8
    .byte   %00111100 ; |  ####  |            $ff40 (G)
    .byte   %01100110 ; | ##  ## |            $ff41 (G)
    .byte   %01100110 ; | ##  ## |            $ff42 (G)
    .byte   %00111100 ; |  ####  |            $ff43 (G)
    .byte   %01100110 ; | ##  ## |            $ff44 (G)
    .byte   %01100110 ; | ##  ## |            $ff45 (G)
    .byte   %00111100 ; |  ####  |            $ff46 (G)
    
    .byte   $00                             ; $ff47 (*)
    
L_digit_9
    .byte   %00111100 ; |  ####  |            $ff48 (G)
    .byte   %01000110 ; | #   ## |            $ff49 (G)
    .byte   %00000110 ; |     ## |            $ff4a (G)
    .byte   %00111110 ; |  ##### |            $ff4b (G)
    .byte   %01100110 ; | ##  ## |            $ff4c (G)
    .byte   %01100110 ; | ##  ## |            $ff4d (G)
    .byte   %00111100 ; |  ####  |            $ff4e (G)
    
    .byte   $00                             ; $ff4f (*)
    
L_logo_activision_1
    .byte   %01100001 ; | ##    #|            $ff50 (G)
    .byte   %00110001 ; |  ##   #|            $ff51 (G)
    .byte   %00011111 ; |   #####|            $ff52 (G)
    .byte   %00001101 ; |    ## #|            $ff53 (G)
    .byte   %00000111 ; |     ###|            $ff54 (G)
    .byte   %00000011 ; |      ##|            $ff55 (G)
    .byte   %00000001 ; |       #|            $ff56 (G)
    
    .byte   $00                             ; $ff57 (*)
    
L_logo_activision_2
    .byte   %01101010 ; | ## # # |            $ff58 (G)
    .byte   %01001010 ; | #  # # |            $ff59 (G)
    .byte   %01001010 ; | #  # # |            $ff5a (G)
    .byte   %01001010 ; | #  # # |            $ff5b (G)
    .byte   %01101010 ; | ## # # |            $ff5c (G)
    .byte   %00001000 ; |    #   |            $ff5d (G)
    .byte   %01111111 ; | #######|            $ff5e (G)
    
    .byte   $00                             ; $ff5f (*)
    
L_logo_activision_3
    .byte   %10000101 ; |#    # #|            $ff60 (G)
    .byte   %11000100 ; |##   #  |            $ff61 (G)
    .byte   %11100101 ; |###  # #|            $ff62 (G)
    .byte   %10110101 ; |# ## # #|            $ff63 (G)
    .byte   %10011001 ; |#  ##  #|            $ff64 (G)
    .byte   %10001100 ; |#   ##  |            $ff65 (G)
    .byte   %10000111 ; |#    ###|            $ff66 (G)
    
    .byte   $00                             ; $ff67 (*)
    
L_logo_activision_4
    .byte   %11010111 ; |## # ###|            $ff68 (G)
    .byte   %01010101 ; | # # # #|            $ff69 (G)
    .byte   %11010101 ; |## # # #|            $ff6a (G)
    .byte   %00010101 ; |   # # #|            $ff6b (G)
    .byte   %11010111 ; |## # ###|            $ff6c (G)
    .byte   %00000000 ; |        |            $ff6d (G)
    .byte   %11110000 ; |####    |            $ff6e (G)
    
    .byte   $00                             ; $ff6f (*)
L_logo_activision_5
    .byte   $48,$58,$78,$68,$48,$00,$00     ; $ff70 (D)
    .byte   $00                             ; $ff77 (*)
    
L_logo_copyr
    .byte   %00111100 ; |  ####  |            $ff78 (G)
    .byte   %01000010 ; | #    # |            $ff79 (G)
    .byte   %01011010 ; | # ## # |            $ff7a (G)
    .byte   %01010010 ; | # #  # |            $ff7b (G)
    .byte   %01011010 ; | # ## # |            $ff7c (G)
    .byte   %01000010 ; | #    # |            $ff7d (G)
    .byte   %00111100 ; |  ####  |            $ff7e (G)

Ldf7f
    .byte   %00000000 ; |        |            $ff7f (G)
    .byte   %10000010 ; |#     # |            $ff80 (G)
    .byte   %11000010 ; |##    # |            $ff81 (G)
    .byte   %01100001 ; | ##    #|            $ff82 (G)
    .byte   %01100001 ; | ##    #|            $ff83 (G)
    .byte   %00110001 ; |  ##   #|            $ff84 (G)
    .byte   %00111011 ; |  ### ##|            $ff85 (G)
    .byte   %00011111 ; |   #####|            $ff86 (G)
    .byte   %00001101 ; |    ## #|            $ff87 (G)
    .byte   %00001110 ; |    ### |            $ff88 (G)
    .byte   %00101110 ; |  # ### |            $ff89 (G)
    .byte   %00101110 ; |  # ### |            $ff8a (G)
    .byte   %01101101 ; | ## ## #|            $ff8b (G)
    .byte   %11011101 ; |## ### #|            $ff8c (G)
    .byte   %11011101 ; |## ### #|            $ff8d (G)
    .byte   %11111101 ; |###### #|            $ff8e (G)
    .byte   %11011011 ; |## ## ##|            $ff8f (G)
    .byte   %01111011 ; | #### ##|            $ff90 (G)
    .byte   %01111010 ; | #### # |            $ff91 (G)
    .byte   %00111110 ; |  ##### |            $ff92 (G)
    .byte   %00011110 ; |   #### |            $ff93 (G)
    .byte   %00001100 ; |    ##  |            $ff94 (G)
    .byte   %00010000 ; |   #    |            $ff95 (G)
    .byte   %00111000 ; |  ###   |            $ff96 (G)
    .byte   %00111000 ; |  ###   |            $ff97 (G)
    .byte   %00111100 ; |  ####  |            $ff98 (G)
    .byte   %01001100 ; | #  ##  |            $ff99 (G)
    .byte   %01111100 ; | #####  |            $ff9a (G)
    .byte   %00111100 ; |  ####  |            $ff9b (G)
    .byte   %00111000 ; |  ###   |            $ff9c (G)

L_logo_spacer
    .byte   %00000000 ; |        |            $ff9d (G)
    .byte   %00000000 ; |        |            $ff9e (G)
    .byte   %00000000 ; |        |            $ff9f (G)
    .byte   %00000000 ; |        |            $ffa0 (G)
    .byte   %00000000 ; |        |            $ffa1 (G)
    .byte   %00000000 ; |        |            $ffa2 (G)
    .byte   %00000000 ; |        |            $ffa3 (G)
    .byte   %00000000 ; |        |            $ffa4 (G)

Ldfa5
    .byte   %10001110 ; |#   ### |            $ffa5 (G)
    .byte   %10001000 ; |#   #   |            $ffa6 (G)
    .byte   %11101000 ; |### #   |            $ffa7 (G)
    .byte   %10101000 ; |# # #   |            $ffa8 (G)
    .byte   %11101000 ; |### #   |            $ffa9 (G)
Ldfaa
    .byte   %10100100 ; |# #  #  |            $ffaa (G)
    .byte   %10100100 ; |# #  #  |            $ffab (G)
    .byte   %11101110 ; |### ### |            $ffac (G)
    .byte   %10101010 ; |# # # # |            $ffad (G)
    .byte   %11101010 ; |### # # |            $ffae (G)
Ldfaf
    .byte   %11101010 ; |### # # |            $ffaf (G)
    .byte   %10001100 ; |#   ##  |            $ffb0 (G)
    .byte   %11001110 ; |##  ### |            $ffb1 (G)
    .byte   %10001010 ; |#   # # |            $ffb2 (G)
    .byte   %11101110 ; |### ### |            $ffb3 (G)
Ldfb4
    .byte   %00111010 ; |  ### # |            $ffb4 (G)
    .byte   %00100011 ; |  #   ##|            $ffb5 (G)
    .byte   %00110011 ; |  ##  ##|            $ffb6 (G)
    .byte   %00100011 ; |  #   ##|            $ffb7 (G)
    .byte   %00111010 ; |  ### # |            $ffb8 (G)
Ldfb9
    .byte   %10111010 ; |# ### # |            $ffb9 (G)
    .byte   %10100010 ; |# #   # |            $ffba (G)
    .byte   %10110011 ; |# ##  ##|            $ffbb (G)
    .byte   %10100011 ; |# #   ##|            $ffbc (G)
    .byte   %10111010 ; |# ### # |            $ffbd (G)
Ldfbe
    .byte   %10010000 ; |#  #    |            $ffbe (G)
    .byte   %10010000 ; |#  #    |            $ffbf (G)
    .byte   %10111000 ; |# ###   |            $ffc0 (G)
    .byte   %10101000 ; |# # #   |            $ffc1 (G)
    .byte   %10101000 ; |# # #   |            $ffc2 (G)

L_logo_irem_1
    .byte   %11101010 ; |### # # |            $ffc3 (G)
    .byte   %01001010 ; | #  # # |            $ffc4 (G)
    .byte   %01001010 ; | #  # # |            $ffc5 (G)
    .byte   %01001100 ; | #  ##  |            $ffc6 (G)
    .byte   %01001010 ; | #  # # |            $ffc7 (G)
    .byte   %01001010 ; | #  # # |            $ffc8 (G)
    .byte   %11101110 ; |### ### |            $ffc9 (G)
    
    .byte   $00                             ; $ffca (*)

L_logo_irem_2
    .byte   %11101010 ; |### # # |            $ffcb (G)
    .byte   %10001010 ; |#   # # |            $ffcc (G)
    .byte   %10001010 ; |#   # # |            $ffcd (G)
    .byte   %11101110 ; |### ### |            $ffce (G)
    .byte   %10001110 ; |#   ### |            $ffcf (G)
    .byte   %10001110 ; |#   ### |            $ffd0 (G)
    .byte   %11101010 ; |### # # |            $ffd1 (G)
    
    .byte   $00                             ; $ffd2 (*)
Ldfd3
    .byte   $04,$0c                         ; $ffd3 (D)
Ldfd5
    .byte   $fc,$fb                         ; $ffd5 (D)
Ldfd7
    .byte   <Ld9e0,<Ld9e8,<Ld9d8,<Ld9d8
    .byte   <Ld9e0,<Ld9e0,<Ld9d8,<Ld9d8
    .byte   <Ld9d8,<Ld9d8

Ldfe1
    .byte   <Ld0f0,<Ld09b                      ; $ffe1 (D)
    .byte   <Ld09b                             ; $ffe3 (*)
    .byte   <Ld0a2                             ; $ffe4 (D)
    .byte   <Ld0f6                             ; $ffe5 (*)
    .byte   <Ld0c5,<Ld0d1,<Ld0dc               ; $ffe6 (D)
    .byte   <Ld0e2                             ; $ffe9 (*)
    .byte   <Ld0a6                             ; $ffea (D)

  IF PLUSROM == 1
; F6: bank 0 -> bank 1 is done by Ldff2 at $d009, bank 1 -> bank 0 continues at Ld00c
  ORG $0ff8
  RORG $dff8
    .byte   $ea,$ea

    .word   PLUSROM_NMI             ; NMI: PlusROM API pointer (same in all banks)
    .word   Start_0
    .word   Start_0

  ELSE
Ldfeb
    ldx     $eaea                   ;4        
    nop                             ;2        
Ldfef
    jmp     Ld00c                   ;3   =   9
    
Ldff2
    bit     Lfff9                   ;4   =   4
    jmp     Lfff5                   ;3   =   3 *

  ORG $0ff8
  RORG $dff8
Ldff8
    .byte   $ea                             ; $fff8 (*)
Ldff9
    .byte   $ea                             ; $fff9 (D)

    .word   $eaea
    .word   Start_0
    .word   Start_0
  ENDIF


;***********************************************************
;      Bank 1 / 0..1
;***********************************************************

    SEG     CODE
    ORG     $1000
    RORG    $f000

Start_1
    sei                             ;2
  IF PLUSROM == 1
Lffec                               ;    F6: bank 1 -> bank 3 (HSC check) -> bank 0
    bit     BANK3                   ;4   (same bytes as the original bit Lfff9)
Start_1_Cold                        ;    RESET bank 1, all other banks continue here
  ELSE
    bit     Lfff9                   ;4
  ENDIF
    cld                             ;2
    ldx     #$ff                    ;2        
    txs                             ;2        
    stx     ram_CF                  ;3        
    inx                             ;2        
    stx     ram_CE                  ;3        
    stx     ram_D2                  ;3   =  23
Lf00f
    lda     #$00                    ;2   =   2
Lf011
    sta     VSYNC,x                 ;4        
    inx                             ;2        
    cpx     #$ce                    ;2        
    bcc     Lf011                   ;2/3      
    ldy     #$31                    ;2        
    jsr     Lfa7f                   ;6        
    lda     ram_81                  ;3        
    bne     Lf032                   ;2/3      
    sta     ram_9D                  ;3        
    sta     ram_9E                  ;3        
    sta     ram_CB                  ;3        
    sta     ram_CC                  ;3        
    inc     ram_81                  ;5        
    bne     Lf032                   ;2/3 =  42
Lf02d
    ldx     #$82                    ;2        
    tya                             ;2        
    bne     Lf00f                   ;2/3 =   6
Lf032
    ldy     INTIM                   ;4        
    bne     Lf032                   ;2/3      
    ldx     #$82                    ;2        
    sta     WSYNC                   ;3   =  11
;---------------------------------------
    stx     VSYNC                   ;3        
    sta     WSYNC                   ;3   =   6
;---------------------------------------
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    dex                             ;2        
    stx     VSYNC                   ;3        
    lda     #$31                    ;2        
    sta     WSYNC                   ;3   =  10
;---------------------------------------
    sta     TIM64T                  ;4        
    ldx     ram_85                  ;3        
    lda     ram_CF                  ;3        
    bpl     Lf063                   ;2/3      
    cmp     #$ff                    ;2        
    bne     Lf060                   ;2/3      
    lda     ram_80                  ;3        
    and     #$7f                    ;2        
    bne     Lf060                   ;2/3      
    jsr     Lfe58                   ;6   =  29
Lf060
    jmp     Lfa54                   ;3   =   3
    
Lf063
    lda     ram_A2                  ;3        
    lsr                             ;2        
    bcc     Lf08f                   ;2/3      
    lda     ram_C0                  ;3        
    cmp     #$09                    ;2        
    beq     Lf0d1                   ;2/3      
    dec     ram_9D,x                ;6        
    lda     ram_D2                  ;3        
    beq     Lf07d                   ;2/3      
    txa                             ;2        
    eor     #$01                    ;2        
    tay                             ;2        
    lda.wy  ram_9D,y                ;4        
    bpl     Lf085                   ;2/3 =  37
Lf07d
    lda     ram_9D,x                ;4        
    bpl     Lf088                   ;2/3      
    sta     ram_CF                  ;3        
    bmi     Lf0d1                   ;2/3 =  11
Lf085
    jsr     Lfe58                   ;6   =   6
Lf088
    jsr     Lfa72                   ;6        
    lda     #$10                    ;2        
    sta     ram_B8                  ;3   =  11
Lf08f
    bit     ram_A2                  ;3        
    bmi     Lf0d7                   ;2/3      
    bvc     Lf0d4                   ;2/3      
    lda     ram_B8                  ;3        
    cmp     #$0f                    ;2        
    bcc     Lf0d1                   ;2/3      
    lda     ram_BA                  ;3        
    bpl     Lf0d1                   ;2/3      
    ldy     #$05                    ;2        
    jsr     Lfa57                   ;6        
    lda     ram_CA                  ;3        
    cmp     #$83                    ;2        
    beq     Lf0bb                   ;2/3      
    lda     ram_9F,x                ;4        
    lsr                             ;2        
    bcc     Lf0b5                   ;2/3      
    jsr     Lfabb                   ;6        
    jmp     Lfa54                   ;3   =  51
    
Lf0b5
    jsr     Lfb4d                   ;6         *
    jmp     Lfa54                   ;3   =   9 *
    
Lf0bb
    jsr     Lfd4d                   ;6        
    lda     #$04                    ;2        
    sta     ram_B8                  ;3        
    ldx     #$00                    ;2        
    stx     ram_A2                  ;3        
    dex                             ;2        
    stx     ram_BA                  ;3        
    lda     #$0a                    ;2        
    sta     ram_B9                  ;3        
    lda     #$fb                    ;2        
    sta     ram_BB                  ;3   =  31
Lf0d1
    jmp     Lfa54                   ;3   =   3
    
Lf0d4
    jmp     Lf194                   ;3   =   3
    
Lf0d7
    ldy     ram_94                  ;3         *
    beq     Lf0df                   ;2/3       *
    dec     ram_94                  ;5         *
    bne     Lf0d1                   ;2/3 =  12 *
Lf0df
    lda     ram_9F,x                ;4         *
    tay                             ;2         *
    lsr                             ;2         *
    lda     ram_A2                  ;3         *
    and     #$02                    ;2         *
    bne     Lf11f                   ;2/3!      *
    bcc     Lf0f3                   ;2/3       *
    inc     ram_CA                  ;5         *
    jsr     Lfaf5                   ;6         *
    jmp     Lf0f8                   ;3   =  31 *
    
Lf0f3
    dec     ram_CA                  ;5         *
    jsr     Lfb92                   ;6   =  11 *
Lf0f8
    ldx     ram_CA                  ;3         *
    cpx     #$82                    ;2         *
    beq     Lf119                   ;2/3!      *
    cpy     #$05                    ;2         *
    bcc     Lf0d1                   ;2/3!      *
    cpx     #$69                    ;2         *
    bcc     Lf0d1                   ;2/3!      *
    ldx     #$06                    ;2         *
    jsr     Lfdb5                   ;6         *
    lda     #$10                    ;2         *
    sta     ram_A3                  ;3         *
    sta     ram_DC                  ;3         *
    lda     #$02                    ;2         *
    sta     ram_C4                  ;3         *
    inc     ram_C8                  ;5         *
    bne     Lf0d1                   ;2/3!=  43 *
Lf119
    lda     #$82                    ;2         *
    sta     ram_A2                  ;3         *
    bne     Lf0d1                   ;2/3!=   7 *
Lf11f
    lda     ram_CA                  ;3         *
    jsr     Lfbdc                   ;6         *
    bcc     Lf142                   ;2/3       *
    cpy     #$05                    ;2         *
    bne     Lf12e                   ;2/3       *
    cmp     #$54                    ;2         *
    bcc     Lf142                   ;2/3 =  19 *
Lf12e
    tya                             ;2         *
    lsr                             ;2         *
    bcc     Lf135                   ;2/3       *
    dec     ram_CA                  ;5         *
    .byte   $2c ;bit ram_E6               ;4-5 =  10 *
Lf135
    inc     ram_CA                  ;5         *
    jsr     Lfb32                   ;6         *
    ldy     #$05                    ;2         *
    jsr     Lfa57                   ;6   =  19 *
Lf13f
    jmp     Lfa54                   ;3   =   3 *
    
Lf142
    lda     ram_CB                  ;3         *
    beq     Lf15b                   ;2/3       *
    lda     ram_80                  ;3         *
    and     #$03                    ;2         *
    bne     Lf13f                   ;2/3       *
    dec     ram_CB                  ;5         *
    ldy     #$01                    ;2         *
    jsr     Lfa57                   ;6         *
    lda     #$01                    ;2         *
    jsr     Lfe16                   ;6         *
    jmp     Lfa54                   ;3   =  36 *
    
Lf15b
    lda     #$16                    ;2         *
    jsr     Lfdf9                   ;6         *
    bcs     Lf180                   ;2/3       *
    ldx     ram_85                  ;3         *
    ldy     ram_9F,x                ;4         *
    iny                             ;2         *
    cpy     #$06                    ;2         *
    bcc     Lf178                   ;2/3       *
    ldy     #$01                    ;2         *
    lda     ram_A1                  ;3         *
    cmp     #$ff                    ;2         *
    beq     Lf178                   ;2/3       *
    clc                             ;2         *
    adc     #$20                    ;2         *
    sta     ram_A1                  ;3   =  39 *
Lf178
    sty     ram_9F,x                ;4         *
    jsr     Lfa72                   ;6         *
    jmp     Lfa54                   ;3   =  13 *
    
Lf180
    ldy     #$02                    ;2         *
    jsr     Lfa57                   ;6         *
    lda     #$60                    ;2         *
    ldx     #$02                    ;2         *
    jsr     Lfe18                   ;6         *
    lda     #$01                    ;2         *
    jsr     Lfe16                   ;6         *
    jmp     Lfa54                   ;3   =  29 *
    
Lf194
    ldy     ram_85                  ;3        
    lda     SWCHA                   ;4        
    ora     Lff2d,y                 ;4        
    ldx     #$0f                    ;2   =  13
Lf19e
    cmp     Lfeaa,x                 ;4        
    beq     Lf1b2                   ;2/3      
    dex                             ;2        
    bpl     Lf19e                   ;2/3      
    lda     ram_BC                  ;3        
    bne     Lf1fa                   ;2/3      
    jsr     Lfd4d                   ;6        
    lda     #$40                    ;2        
    jmp     Lf220                   ;3   =  26
    
Lf1b2
    cpx     #$08                    ;2        
    bcc     Lf1ba                   ;2/3      
    txa                             ;2        
    sbc     #$08                    ;2        
    tax                             ;2   =  10
Lf1ba
    cpx     #$07                    ;2        
    beq     Lf1fd                   ;2/3      
    lda.wy  INPT4,y                 ;4        
    bmi     Lf1fd                   ;2/3      
    bit     ram_CD                  ;3        
    bmi     Lf1fa                   ;2/3      
    sec                             ;2        
    ror     ram_CD                  ;5        
    lda     ram_B5                  ;3        
    bpl     Lf1fa                   ;2/3      
    lda     ram_BC                  ;3        
    bne     Lf1fa                   ;2/3      
    stx     ram_B5                  ;3        
    lda     Lff68,x                 ;4        
    ldy     #<Ldab9                 ;2        
    ldx     #$00                    ;2        
    and     #$c0                    ;2        
    beq     Lf1e8                   ;2/3      
    bpl     Lf1e6                   ;2/3      
    ldx     #$08                    ;2        
    asl                             ;2        
    bpl     Lf1e8                   ;2/3 =  55
Lf1e6
    ldy     #<Ldab4-1                    ;2   =   2
Lf1e8
    sty     ram_AE                  ;3        
    ldy     ram_B5                  ;3        
    beq     Lf1f0                   ;2/3      
    stx     ram_DA                  ;3   =  11
Lf1f0
    lda     #$0a                    ;2        
    sta     ram_8A                  ;3        
    ldx     Lfe84,y                 ;4        
    jsr     Lfd63                   ;6   =  15
Lf1fa
    jmp     Lf25f                   ;3   =   3
    
Lf1fd
    clc                             ;2        
    ror     ram_CD                  ;5        
    lda     Lffc8,x                 ;4        
    sta     ram_F9                  ;3        
    ldy     ram_BC                  ;3        
    bne     Lf25f                   ;2/3      
    ldy     ram_B5                  ;3        
    bpl     Lf25f                   ;2/3      
    lsr                             ;2        
    bcs     Lf224                   ;2/3      
    lsr                             ;2        
    bcc     Lf231                   ;2/3      
    bit     ram_84                  ;3        
    bmi     Lf25f                   ;2/3      
    inc     ram_BC                  ;5        
    ldx     #$06                    ;2        
    jsr     Lfd57                   ;6        
    lda     #$80                    ;2   =  52
Lf220
    sta     ram_84                  ;3        
    bne     Lf25f                   ;2/3 =   5
Lf224
    lda     #$40                    ;2        
    sta     ram_84                  ;3        
    lda     #<Ldab9                    ;2        
    sta     ram_AE                  ;3        
    ldx     #$01                    ;2        
    jsr     Lfd63                   ;6   =  18
Lf231
    bit     ram_F9                  ;3        
    bvs     Lf24c                   ;2/3      
    bpl     Lf25f                   ;2/3      
    jsr     Lfabc                   ;6        
    bit     ram_BD                  ;3        
    bpl     Lf25f                   ;2/3      
    bit     ram_92                  ;3        
    bmi     Lf25f                   ;2/3      
    inc     ram_C5                  ;5        
    inc     ram_C5                  ;5        
    lda     #$80                    ;2        
    sta     ram_92                  ;3        
    bne     Lf25f                   ;2/3 =  40
Lf24c
    jsr     Lfb4e                   ;6        
    bit     ram_BD                  ;3        
    bpl     Lf25f                   ;2/3      
    bit     ram_92                  ;3        
    bvs     Lf25f                   ;2/3      
    inc     ram_C5                  ;5        
    inc     ram_C5                  ;5        
    lda     #$40                    ;2        
    sta     ram_92                  ;3   =  31
Lf25f
    lda     ram_A2                  ;3        
    and     #$10                    ;2        
    beq     Lf282                   ;2/3      
    lda     ram_80                  ;3         *
    and     #$03                    ;2         *
    bne     Lf27f                   ;2/3       *
    inc     ram_94                  ;5         *
    lda     ram_94                  ;3         *
    cmp     #$03                    ;2         *
    bcc     Lf27a                   ;2/3       *
    jsr     Lfc1f                   ;6         *
    sta     ram_BD                  ;3         *
    beq     Lf27f                   ;2/3 =  37 *
Lf27a
    adc     #$07                    ;2         *
    jsr     Lfdb4                   ;6   =   8 *
Lf27f
    jmp     Lf98d                   ;3   =   3 *
    
Lf282
    bit     ram_F6                  ;3        
    bvc     Lf292                   ;2/3      
    lda     ram_91                  ;3        
    cmp     #$24                    ;2        
    bne     Lf28f                   ;2/3      
    jsr     Lfbcf                   ;6   =  18
Lf28f
    jmp     Lf2ed                   ;3   =   3
    
Lf292
    bmi     Lf2a3                   ;2/3 =   2
Lf294
    ldy     ram_BD                  ;3        
    bpl     Lf2fa                   ;2/3      
    ldx     ram_BF                  ;3        
    lda     ram_C5                  ;3        
    cmp     Lfe79,x                 ;4        
    bcc     Lf2fa                   ;2/3      
    sty     ram_C5                  ;3   =  20
Lf2a3
    ldy     ram_BF                  ;3        
    ldx     Lffd0,y                 ;4   =   7
Lf2a8
    lda     ram_C8                  ;3        
    clc                             ;2        
    adc     Lff2f,x                 ;4        
    sta     ram_F7                  ;3        
    sec                             ;2        
    ldy     ram_DA                  ;3        
    bne     Lf2ba                   ;2/3      
    lda     ram_CA                  ;3        
    sbc     ram_F7                  ;3        
    .byte   $2c ;bit                ;4-3 =  26
Lf2ba
    sbc     ram_CA                  ;3        
    cmp     #$f9                    ;2        
    bcs     Lf2d3                   ;2/3      
    cmp     #$07                    ;2        
    bcc     Lf2d3                   ;2/3      
    cmp     #$0e                    ;2        
    bcc     Lf2cd                   ;2/3      
    dex                             ;2        
    bpl     Lf2a8                   ;2/3      
    bmi     Lf2fa                   ;2/3 =  21
Lf2cd
    stx     ram_F9                  ;3        
    lda     ram_B5                  ;3        
    bpl     Lf343                   ;2/3!=   8
Lf2d3
    bit     ram_C5                  ;3        
    stx     ram_F9                  ;3        
    bmi     Lf318                   ;2/3!     
    lda     ram_B6                  ;3        
    bmi     Lf31b                   ;2/3!     
    cmp     #$01                    ;2        
    bne     Lf2fd                   ;2/3      
    ldy     ram_A3                  ;3         *
    cpy     #$11                    ;2         *
    bcc     Lf2fd                   ;2/3       *
    lda     ram_BC                  ;3         *
    bne     Lf318                   ;2/3!      *
    sta     ram_90                  ;3   =  32 *
Lf2ed
    ldy     #$07                    ;2   =   2
Lf2ef
    jsr     Lfbe6                   ;6        
    bvs     Lf2fa                   ;2/3 =   8
Lf2f4
    jsr     Lfa57                   ;6        
    jsr     Lfaa5                   ;6   =  12
Lf2fa
    jmp     Lf42a                   ;3   =   3
    
Lf2fd
    cmp     #$03                    ;2        
    bcc     Lf315                   ;2/3      
    jsr     Lfc61                   ;6        
    lsr                             ;2        
    bcc     Lf2fa                   ;2/3!     
    lda     ram_94                  ;3        
    bmi     Lf2fa                   ;2/3!=  19
Lf30b
    lda     ram_C3                  ;3        
    bne     Lf2fa                   ;2/3!     
    inc     ram_C3                  ;5        
    ldy     #$03                    ;2        
    bne     Lf2f4                   ;2/3!=  14
Lf315
    jmp     Lf294                   ;3   =   3
    
Lf318
    jmp     Lf3d8                   ;3   =   3
    
Lf31b
    cmp     #$80                    ;2         *
    bne     Lf32b                   ;2/3       *
    lda     ram_A6                  ;3         *
    cmp     #$72                    ;2         *
    beq     Lf337                   ;2/3       *
    lda     ram_C4                  ;3         *
    bne     Lf2fa                   ;2/3!      *
    beq     Lf2ed                   ;2/3!=  18 *
Lf32b
    cmp     #$81                    ;2         *
    beq     Lf337                   ;2/3       *
    cmp     #$83                    ;2         *
    beq     Lf2ed                   ;2/3!      *
    lda     ram_94                  ;3         *
    bne     Lf2ed                   ;2/3!=  13 *
Lf337
    lda     #$10                    ;2         *
    sta     ram_A2                  ;3         *
    lda     #$ff                    ;2         *
    sta     ram_94                  ;3         *
    ldy     #$03                    ;2         *
    bne     Lf2ef                   ;2/3!=  14 *
Lf343
    jsr     Lfbe6                   ;6        
    bvs     Lf364                   ;2/3      
    ldy     ram_B6                  ;3        
    bmi     Lf3b2                   ;2/3      
    beq     Lf367                   ;2/3      
    bit     ram_BD                  ;3        
    bpl     Lf357                   ;2/3      
    lda     ram_80                  ;3        
    lsr                             ;2        
    bcs     Lf364                   ;2/3 =  27
Lf357
    cpy     #$01                    ;2        
    bne     Lf370                   ;2/3      
    ldy     ram_B5                  ;3         *
    lda     Lff68,y                 ;4         *
    and     #$40                    ;2         *
    bvc     Lf3d8                   ;2/3 =  15 *
Lf364
    jmp     Lf42a                   ;3   =   3
    
Lf367
    bit     ram_C1                  ;3        
    bmi     Lf3d8                   ;2/3      
    dec     ram_C1                  ;5        
    jmp     Lf425                   ;3   =  13
    
Lf370
    cpy     #$03                    ;2        
    bcc     Lf3d8                   ;2/3      
    jsr     Lfc61                   ;6        
    ldx     ram_94                  ;3        
    bmi     Lf384                   ;2/3      
    cpx     #$05                    ;2        
    bcc     Lf384                   ;2/3      
    lsr                             ;2        
    bcs     Lf30b                   ;2/3      
    bcc     Lf38f                   ;2/3 =  25 *
Lf384
    lsr                             ;2        
    bcc     Lf38f                   ;2/3      
    lda     ram_CA                  ;3        
    sbc     ram_C8                  ;3        
    cmp     #$0d                    ;2        
    bcs     Lf3af                   ;2/3 =  14
Lf38f
    ldy     ram_B6                  ;3        
    cpy     #$06                    ;2        
    bne     Lf39b                   ;2/3      
    lda     ram_B5                  ;3         *
    cmp     #$04                    ;2         *
    bne     Lf3af                   ;2/3 =  14 *
Lf39b
    lda     ram_CC                  ;3        
    sec                             ;2        
    sbc     #$04                    ;2        
    beq     Lf3a4                   ;2/3      
    bcs     Lf3ab                   ;2/3 =  11
Lf3a4
    ldy     #$0a                    ;2         *
    jsr     Lfa57                   ;6         *
    lda     #$00                    ;2   =  10 *
Lf3ab
    sta     ram_CC                  ;3        
    beq     Lf3d8                   ;2/3 =   5
Lf3af
    jmp     Lf425                   ;3   =   3
    
Lf3b2
    cpy     #$81                    ;2         *
    beq     Lf337                   ;2/3       *
    cpy     #$80                    ;2         *
    bne     Lf3cd                   ;2/3       *
    lda     ram_A6                  ;3         *
    cmp     #$72                    ;2         *
    beq     Lf3d8                   ;2/3       *
    bit     ram_DF                  ;3         *
    bmi     Lf364                   ;2/3       *
    sec                             ;2         *
    ror     ram_DF                  ;5         *
    lda     #$04                    ;2         *
    sta     ram_94                  ;3         *
    bne     Lf3d8                   ;2/3 =  34 *
Lf3cd
    cpy     #$83                    ;2         *
    bcs     Lf3d8                   ;2/3       *
    lda     ram_94                  ;3         *
    beq     Lf3d8                   ;2/3       *
    jmp     Lf2ed                   ;3   =  12 *
    
Lf3d8
    bit     ram_BD                  ;3        
    bmi     Lf3fb                   ;2/3      
    ldx     ram_B5                  ;3        
    lda     ram_B6                  ;3        
    bpl     Lf3e5                   ;2/3      
    sec                             ;2         *
    sbc     #$78                    ;2   =  17 *
Lf3e5
    asl                             ;2        
    tay                             ;2        
    lda     Lff68,x                 ;4        
    lsr                             ;2        
    bcc     Lf3ee                   ;2/3      
    iny                             ;2   =  14
Lf3ee
    ldx     #$01                    ;2        
    lda     Lffd4,y                 ;4        
    bpl     Lf3f8                   ;2/3      
    and     #$7f                    ;2         *
    dex                             ;2   =  12 *
Lf3f8
    jsr     Lfe18                   ;6   =   6
Lf3fb
    lda     #$00                    ;2        
    sta     ram_BD                  ;3        
    sta     ram_C5                  ;3        
    lda     ram_BF                  ;3        
    beq     Lf419                   ;2/3      
    lda     ram_F9                  ;3        
    asl                             ;2        
    asl                             ;2        
    adc     ram_BF                  ;3        
    tay                             ;2        
    lda     Lff2f,y                 ;4        
    tay                             ;2        
    and     #$03                    ;2        
    sta     ram_DB                  ;3        
    jsr     Lfd0d                   ;6        
    bne     Lf425                   ;2/3 =  44
Lf419
    lda     ram_B6                  ;3        
    bmi     Lf422                   ;2/3      
    jsr     Lfc39                   ;6        
    beq     Lf425                   ;2/3 =  13
Lf422
    jsr     Lfc1f                   ;6   =   6 *
Lf425
    ldy     #$03                    ;2        
    jsr     Lfa57                   ;6   =   8
Lf42a
    lda     ram_93                  ;3        
    beq     Lf433                   ;2/3      
    dec     ram_93                  ;5   =  10
Lf430
    jmp     Lf575                   ;3   =   3
    
Lf433
    bit     ram_B6                  ;3        
    bvc     Lf430                   ;2/3      
    bit     ram_A2                  ;3        
    bmi     Lf430                   ;2/3      
    lda     ram_81                  ;3        
    and     #$03                    ;2        
    tax                             ;2        
    cpx     #$03                    ;2        
    bne     Lf445                   ;2/3      
    dex                             ;2   =  23
Lf445
    jsr     Lfc61                   ;6        
    ldy     ram_86                  ;3        
    cpy     #$06                    ;2        
    bcc     Lf4bb                   ;2/3      
    lsr                             ;2        
    bcs     Lf4bb                   ;2/3      
    sta     ram_F7                  ;3         *
    jsr     Lfbda                   ;6         *
    lda     ram_F7                  ;3         *
    bcs     Lf4b8                   ;2/3       *
    lsr                             ;2         *
    bcs     Lf4a4                   ;2/3       *
    ldx     ram_81                  ;3         *
    cpx     #$40                    ;2         *
    bcc     Lf47b                   ;2/3       *
    cpx     #$cc                    ;2         *
    bcs     Lf47b                   ;2/3       *
    txa                             ;2         *
    sbc     ram_CA                  ;3         *
    cmp     #$10                    ;2         *
    bcc     Lf4b8                   ;2/3       *
    cmp     #$ef                    ;2         *
    bcs     Lf4b8                   ;2/3       *
    stx     ram_C8                  ;3         *
    ldx     #$0a                    ;2         *
    ldy     #$00                    ;2         *
    tya                             ;2         *
    beq     Lf48c                   ;2/3 =  70 *
Lf47b
    jsr     Lfc08                   ;6         *
    cpy     #$0a                    ;2         *
    bcc     Lf485                   ;2/3       *
    jsr     Lfc08                   ;6   =  16 *
Lf485
    ldx     #$0c                    ;2         *
    ldy     #$03                    ;2         *
    jsr     Lfc12                   ;6   =  10 *
Lf48c
    sta     ram_DB                  ;3         *
    sty     ram_94                  ;3         *
    jsr     Lfbef                   ;6         *
    lda     #$83                    ;2         *
    sta     ram_B6                  ;3         *
    lda     ram_80                  ;3         *
    lsr                             ;2         *
    lsr                             ;2         *
    cmp     #$10                    ;2         *
    bcs     Lf4a1                   ;2/3       *
    adc     #$20                    ;2   =  30 *
Lf4a1
    jmp     Lf4d0                   ;3   =   3 *
    
Lf4a4
    sta     ram_94                  ;3         *
    lda     ram_81                  ;3         *
    cmp     #$47                    ;2         *
    bcc     Lf4b8                   ;2/3       *
    cmp     #$c5                    ;2         *
    bcs     Lf4b8                   ;2/3       *
    ldy     ram_DA                  ;3         *
    bne     Lf4be                   ;2/3       *
    cmp     #$6e                    ;2         *
    bcc     Lf4c2                   ;2/3 =  23 *
Lf4b8
    jmp     Lf575                   ;3   =   3 *
    
Lf4bb
    jmp     Lf4d7                   ;3   =   3
    
Lf4be
    cmp     #$9e                    ;2         *
    bcc     Lf4b8                   ;2/3 =   4 *
Lf4c2
    sta     ram_C8                  ;3         *
    txa                             ;2         *
    ora     #$80                    ;2         *
    sta     ram_B6                  ;3         *
    txa                             ;2         *
    clc                             ;2         *
    adc     #$03                    ;2         *
    tax                             ;2         *
    lda     #$67                    ;2   =  20 *
Lf4d0
    jsr     Lfdb5                   ;6         *
    ldx     #$02                    ;2         *
    bne     Lf54b                   ;2/3!=  10 *
Lf4d7
    lda     #$00                    ;2        
    sta     ram_DB                  ;3        
    sta     ram_BF                  ;3        
    jsr     Lfc61                   ;6        
    ldy     ram_86                  ;3        
    cpy     #$03                    ;2        
    bcc     Lf512                   ;2/3!=  21
Lf4e6
    cpy     #$0a                    ;2        
    bcs     Lf517                   ;2/3!=   4
Lf4ea
    cpy     #$02                    ;2        
    bcc     Lf4f5                   ;2/3      
    cpy     #$0b                    ;2        
    bcs     Lf4f5                   ;2/3      
    lda     ram_81                  ;3        
    asl                             ;2   =  13
Lf4f5
    jsr     Lfdb5                   ;6        
    jsr     Lfbf8                   ;6        
    jsr     Lfc08                   ;6        
    jsr     Lfc61                   ;6        
    cpx     #$01                    ;2        
    bne     Lf54d                   ;2/3      
    cmp     #$02                    ;2        
    bcc     Lf556                   ;2/3      
    jsr     Lfbda                   ;6         *
    bcs     Lf556                   ;2/3       *
    ldy     #$10                    ;2         *
    bne     Lf563                   ;2/3 =  44 *
Lf512
    lsr                             ;2        
    bcs     Lf4ea                   ;2/3!     
    bcc     Lf51a                   ;2/3 =   6 *
Lf517
    lsr                             ;2        
    bcc     Lf4ea                   ;2/3!=   4
Lf51a
    lda     ram_CC                  ;3        
    beq     Lf4ea                   ;2/3!     
    jsr     Lfbda                   ;6        
    bcs     Lf575                   ;2/3      
    ldy     ram_85                  ;3        
    ldx     ram_9F,y                ;4        
    dex                             ;2        
    txa                             ;2        
    lsr                             ;2        
    jsr     Lfbf8                   ;6        
    jsr     Lfc08                   ;6        
    lda     #$cc                    ;2        
    sta     ram_B2                  ;3        
    txa                             ;2        
    ldx     #$01                    ;2        
    bcc     Lf53a                   ;2/3      
    dex                             ;2   =  51 *
Lf53a
    clc                             ;2        
    adc     #$03                    ;2        
    sta     ram_B6                  ;3        
    lda     #$03                    ;2        
    sta     ram_B4                  ;3        
    lda     #$da                    ;2        
    sta     ram_A7                  ;3        
    sta     ram_94                  ;3        
    lda     #$2f                    ;2   =  22
Lf54b
    bne     Lf571                   ;2/3 =   2
Lf54d
    bcs     Lf55c                   ;2/3      
    lda     ram_81                  ;3        
    cmp     Lfeb9,y                 ;4        
    bcc     Lf558                   ;2/3 =  11
Lf556
    ldx     #$02                    ;2   =   2
Lf558
    lda     #$00                    ;2        
    beq     Lf55f                   ;2/3 =   4
Lf55c
    jsr     Lfc12                   ;6   =   6
Lf55f
    ldy     #$2f                    ;2        
    sta     ram_DB                  ;3   =   5
Lf563
    stx     ram_B6                  ;3        
    tya                             ;2        
    jsr     Lfdb5                   ;6        
    cpx     #$01                    ;2        
    bne     Lf56f                   ;2/3      
    inx                             ;2         *
    .byte   $2c ;bit                ;4-2 =  19 *
Lf56f
    ldx     #$00                    ;2   =   2
Lf571
    sta     ram_A3                  ;3        
    stx     ram_C4                  ;3   =   6
Lf575
    lda     ram_C8                  ;3        
    cmp     #$31                    ;2        
    beq     Lf5a0                   ;2/3      
    bit     ram_BD                  ;3        
    bmi     Lf5a0                   ;2/3      
    lda     ram_B6                  ;3        
    bpl     Lf586                   ;2/3      
    jmp     Lf7b3                   ;3   =  20 *
    
Lf586
    cmp     #$01                    ;2        
    bne     Lf590                   ;2/3      
    ldx     ram_A3                  ;3         *
    cpx     #$11                    ;2         *
    bcs     Lf5a3                   ;2/3 =  11 *
Lf590
    cmp     #$03                    ;2        
    bcc     Lf597                   ;2/3      
    jmp     Lf629                   ;3   =   7
    
Lf597
    jsr     Lfd2e                   ;6        
    bcs     Lf5a3                   ;2/3      
    lda     #$80                    ;2        
    sta     ram_BD                  ;3   =  13
Lf5a0
    jmp     Lf98d                   ;3   =   3
    
Lf5a3
    lda     ram_B6                  ;3        
    beq     Lf5aa                   ;2/3 =   5
Lf5a7
    jmp     Lf6e7                   ;3   =   3
    
Lf5aa
    lda     ram_D1                  ;3        
    beq     Lf5ff                   ;2/3      
    jsr     Lfc61                   ;6        
    tax                             ;2        
    lda     ram_80                  ;3        
    and     Lffc3,x                 ;4        
    bne     Lf5a0                   ;2/3      
    dec     ram_D1                  ;5        
    beq     Lf5f6                   ;2/3      
    jsr     Lfbda                   ;6        
    bcs     Lf5a0                   ;2/3      
    lda     ram_81                  ;3        
    cmp     #$70                    ;2        
    bcs     Lf5a0                   ;2/3      
    lda     ram_94                  ;3        
    beq     Lf612                   ;2/3!     
    dec     ram_94                  ;5        
    bne     Lf5a0                   ;2/3      
    jsr     Lfe4f                   ;6        
    sty     ram_B7                  ;3        
    lda     ram_C8                  ;3        
    ldx     ram_DC                  ;3        
    stx     ram_90                  ;3        
    bne     Lf5e0                   ;2/3      
    sbc     #$05                    ;2        
    .byte   $2c ;bit                ;4-2 =  80
Lf5e0
    adc     #$06                    ;2        
    sta     ram_C9                  ;3        
    lda     #$24                    ;2        
    sta     ram_91                  ;3        
    ldy     #$04                    ;2        
    jsr     Lfd9c                   ;6        
    ldy     #$06                    ;2        
    jsr     Lfa57                   ;6        
    ldy     #$90                    ;2        
    bne     Lf615                   ;2/3!=  30
Lf5f6
    clc                             ;2        
    lda     ram_DA                  ;3        
    bne     Lf5fc                   ;2/3      
    sec                             ;2   =   9
Lf5fc
    jsr     Lfbf8                   ;6   =   6
Lf5ff
    lda     ram_81                  ;3        
    cmp     #$10                    ;2        
    bcs     Lf5a7                   ;2/3!     
    ldy     ram_C8                  ;3        
    cpy     #$47                    ;2        
    bcc     Lf5a7                   ;2/3!     
    cpy     #$ba                    ;2        
    bcs     Lf5a7                   ;2/3!     
    asl                             ;2        
    sta     ram_D1                  ;3   =  23
Lf612
    jsr     Lfe45                   ;6   =   6
Lf615
    sty     ram_A6                  ;3        
    ldy     #$00                    ;2        
    lda     ram_C8                  ;3        
    cmp     ram_CA                  ;3        
    bcs     Lf621                   ;2/3      
    ldy     #$08                    ;2   =  15
Lf621
    sty     ram_DC                  ;3        
    lda     #$03                    ;2        
    sta     ram_94                  ;3        
    bne     Lf63a                   ;2/3 =  10
Lf629
    jsr     Lfc61                   ;6        
    tay                             ;2        
    lsr                             ;2        
    ldx     ram_94                  ;3        
    bmi     Lf65b                   ;2/3      
    beq     Lf6aa                   ;2/3      
    dec     ram_94                  ;5   =  22
Lf636
    lda     #$01                    ;2        
    sta     ram_D1                  ;3   =   5
Lf63a
    jmp     Lf98d                   ;3   =   3
    
Lf63d
    tya                             ;2        
    lsr                             ;2        
    bcc     Lf64e                   ;2/3      
    jsr     Lfd2e                   ;6        
    bcc     Lf63a                   ;2/3      
    lda     ram_C8                  ;3        
    cmp     #$50                    ;2        
    bcc     Lf654                   ;2/3      
    bcs     Lf636                   ;2/3 =  23
Lf64e
    lda     ram_C8                  ;3         *
    cmp     #$a0                    ;2         *
    bcc     Lf636                   ;2/3 =   7 *
Lf654
    lda     ram_D1                  ;3        
    bne     Lf636                   ;2/3      
    jmp     Lf78a                   ;3   =   8
    
Lf65b
    lda     ram_81                  ;3        
    cmp     Lfe72,y                 ;4        
    bcs     Lf63d                   ;2/3      
    ldx     #$06                    ;2        
    tya                             ;2        
    lsr                             ;2        
    bcs     Lf679                   ;2/3      
    jsr     Lfbda                   ;6         *
    bcs     Lf63d                   ;2/3       *
    stx     ram_94                  ;3         *
    ldx     #$09                    ;2         *
    jsr     Lfe45                   ;6         *
    bcc     Lf69f                   ;2/3       *
    inx                             ;2         *
    bcs     Lf69f                   ;2/3 =  42 *
Lf679
    asl                             ;2        
    asl                             ;2        
    sta     ram_F7                  ;3        
    lda     ram_CA                  ;3        
    sec                             ;2        
    sbc     ram_C8                  ;3        
    cmp     #$0f                    ;2        
    bcs     Lf63d                   ;2/3      
    stx     ram_94                  ;3        
    lda     ram_80                  ;3        
    and     #$03                    ;2        
    adc     ram_F7                  ;3        
    tay                             ;2        
    ldx     Lfe79,y                 ;4        
    cpy     #$0a                    ;2        
    ldy     #$d2                    ;2        
    bcs     Lf6a1                   ;2/3      
    cmp     #$02                    ;2        
    bcc     Lf6a1                   ;2/3      
    .byte   $2c ;bit                ;4-2 =  48
Lf69d
    ldx     #$07                    ;2   =   2
Lf69f
    ldy     #$cc                    ;2   =   2
Lf6a1
    sty     ram_B2                  ;3        
    ldy     #$02                    ;2        
    jsr     Lfd65                   ;6        
    bcs     Lf636                   ;2/3 =  13
Lf6aa
    stx     ram_C3                  ;3        
    dec     ram_94                  ;5        
    bcs     Lf69d                   ;2/3      
    ldy     #$01                    ;2         *
    ldx     #$24                    ;2         *
    lsr                             ;2         *
    bcc     Lf6ba                   ;2/3       *
    iny                             ;2         *
    ldx     #$14                    ;2   =  22 *
Lf6ba
    stx     ram_91                  ;3         *
    lda     ram_81                  ;3         *
    lsr                             ;2         *
    lda     #$00                    ;2         *
    ror                             ;2         *
    sta     ram_90                  ;3         *
    jsr     Lfd9c                   ;6         *
    lda     ram_C8                  ;3         *
    sec                             ;2         *
    sbc     #$01                    ;2         *
    sta     ram_C9                  ;3         *
    jsr     Lfe4f                   ;6         *
    sty     ram_B7                  ;3         *
    beq     Lf6dc                   ;2/3       *
    lda     #$40                    ;2         *
    cpx     #$24                    ;2         *
    bne     Lf6dc                   ;2/3       *
    asl                             ;2   =  50 *
Lf6dc
    ora     ram_90                  ;3         *
    sta     ram_90                  ;3         *
    ldy     #$06                    ;2         *
    jsr     Lfa57                   ;6         *
    bne     Lf69d                   ;2/3 =  16 *
Lf6e7
    jsr     Lfc61                   ;6        
    ldy     ram_86                  ;3        
    lsr                             ;2        
    bcc     Lf6f5                   ;2/3      
    cpy     #$0a                    ;2        
    bcs     Lf6f9                   ;2/3      
    bcc     Lf715                   ;2/3!=  19
Lf6f5
    cpy     #$03                    ;2         *
    bcs     Lf715                   ;2/3!=   4 *
Lf6f9
    lda     ram_CC                  ;3        
    beq     Lf715                   ;2/3!     
    lda     #$80                    ;2        
    bit     ram_DF                  ;3        
    bcc     Lf706                   ;2/3      
    bne     Lf715                   ;2/3      
    .byte   $2c ;bit                ;4-2 =  16
Lf706
    beq     Lf715                   ;2/3      
    eor     ram_DF                  ;3        
    sta     ram_DF                  ;3        
    lda     ram_DC                  ;3        
    eor     #$08                    ;2        
    sta     ram_DC                  ;3        
    jmp     Lf78a                   ;3   =  19
    
Lf715
    lda     ram_B6                  ;3        
    cmp     #$01                    ;2        
    bne     Lf78a                   ;2/3      
    lda     ram_A3                  ;3         *
    cmp     #$11                    ;2         *
    bcs     Lf744                   ;2/3       *
    sec                             ;2         *
    bit     ram_DF                  ;3         *
    bmi     Lf72d                   ;2/3       *
    lda     ram_CA                  ;3         *
    sbc     ram_C8                  ;3         *
    jmp     Lf731                   ;3   =  30 *
    
Lf72d
    lda     ram_C8                  ;3         *
    sbc     ram_CA                  ;3   =   6 *
Lf731
    cmp     #$e9                    ;2         *
    bcc     Lf78a                   ;2/3       *
    lda     ram_81                  ;3         *
    ldy     ram_85                  ;3         *
    ldx     ram_9F,y                ;4         *
    cmp     [Lfe6c+1],x                 ;4         *
    bcs     Lf78a                   ;2/3       *
    inc     ram_A3                  ;5         *
    bne     Lf757                   ;2/3 =  27 *
Lf744
    jsr     Lfc7f                   ;6         *
    bit     ram_90                  ;3         *
    bmi     Lf75a                   ;2/3       *
    inc     ram_A3                  ;5         *
    inc     ram_A3                  ;5         *
    lda     ram_A3                  ;3         *
    cmp     #$30                    ;2         *
    bcc     Lf76e                   ;2/3       *
    ror     ram_90                  ;5   =  33 *
Lf757
    jmp     Lf98d                   ;3   =   3 *
    
Lf75a
    dec     ram_A3                  ;5         *
    dec     ram_A3                  ;5         *
    lda     ram_A3                  ;3         *
    cmp     #$11                    ;2         *
    bcs     Lf76e                   ;2/3       *
    lda     #$00                    ;2         *
    ldy     #$16                    ;2         *
    sty     ram_90                  ;3         *
    ldx     #$a0                    ;2         *
    bne     Lf782                   ;2/3 =  28 *
Lf76e
    ldy     #$0f                    ;2         *
    lda     ram_80                  ;3         *
    and     #$03                    ;2         *
    bne     Lf757                   ;2/3       *
    ldx     #$47                    ;2         *
    lda     #$2a                    ;2         *
    cmp     ram_A6                  ;3         *
    bne     Lf782                   ;2/3       *
    lda     #$39                    ;2         *
    ldx     #$52                    ;2   =  22 *
Lf782
    stx     ram_B2                  ;3         *
    sta     ram_A6                  ;3         *
    sty     ram_B4                  ;3         *
    bne     Lf757                   ;2/3 =  11 *
Lf78a
    jsr     Lfc7f                   ;6        
    jsr     Lfc67                   ;6        
    lda     ram_80                  ;3        
    and     #$07                    ;2        
    bne     Lf7aa                   ;2/3      
    ldy     ram_B6                  ;3        
    cpy     #$03                    ;2        
    bcc     Lf79e                   ;2/3      
    ldy     #$00                    ;2   =  28
Lf79e
    lda     Lfec4,y                 ;4        
    cmp     ram_A6                  ;3        
    bne     Lf7a8                   ;2/3      
    lda     Lff83,y                 ;4   =  13
Lf7a8
    sta     ram_A6                  ;3   =   3
Lf7aa
    jmp     Lf98d                   ;3   =   3
    
Lf7ad
    jmp     Lf841                   ;3   =   3 *
    
Lf7b0
    jmp     Lf8e4                   ;3   =   3 *
    
Lf7b3
    ldx     ram_94                  ;3         *
    ldy     ram_A3                  ;3         *
    and     #$07                    ;2         *
    beq     Lf7b0                   ;2/3       *
    cmp     #$02                    ;2         *
    bcs     Lf7ad                   ;2/3       *
    cpy     #$40                    ;2         *
    bcc     Lf7ce                   ;2/3       *
    lda     ram_81                  ;3         *
    lsr                             ;2   =  23 *
    
Lf7c6
    sta ram_D1
    dec ram_A3
    dec ram_A3
    bne Lf830                       ; 
    
Lf7ce
    lda     ram_A6                  ;3         *
    cmp     #$49                    ;2         *
    bne     Lf801                   ;2/3!      *
    dec     ram_D1                  ;5         *
    bne     Lf7e0                   ;2/3       *
    ldy     #$08                    ;2         *
    jsr     Lfa57                   ;6         *
    jmp     Lf80b                   ;3   =  25 *
    
Lf7e0
    lda     ram_80                  ;3         *
    lsr                             ;2         *
    bcs     Lf830                   ;2/3!      *
    ldx     ram_87                  ;3         *
    inx                             ;2         *
    cpx     #$04                    ;2         *
    bcc     Lf7ee                   ;2/3       *
    ldx     #$00                    ;2   =  18 *
Lf7ee
    stx     ram_87                  ;3         *
    tya                             ;2         *
    clc                             ;2         *
    adc     Lff46,x                 ;4         *
    sta     ram_A3                  ;3         *
    lda     ram_C8                  ;3         *
    clc                             ;2         *
    adc     Lff45,x                 ;4         *
    sta     ram_C8                  ;3         *
    bne     Lf830                   ;2/3 =  28 *
Lf801
    cpx     #$04                    ;2         *
    bcs     Lf823                   ;2/3       *
    lda     ram_80                  ;3         *
    and     #$03                    ;2         *
    bne     Lf830                   ;2/3 =  11 *
Lf80b
    txa                             ;2         *
    clc                             ;2         *
    adc     #$13                    ;2         *
    tax                             ;2         *
    jsr     Lfdc8                   ;6         *
    lda     #$61                    ;2         *
    sta     ram_B2                  ;3         *
    inc     ram_94                  ;5         *
    cpx     #$15                    ;2         *
    bne     Lf830                   ;2/3       *
    lda     ram_81                  ;3         *
    asl                             ;2         *
    jsr     Lfbf8                   ;6   =  39 *
Lf823
    dec     ram_A3                  ;5         *
    dec     ram_A3                  ;5         *
    lda     ram_A3                  ;3         *
    cmp     #$02                    ;2         *
    bcs     Lf833                   ;2/3 =  17 *
Lf82d
    jsr     Lfc1f                   ;6   =   6 *
Lf830
    jmp     Lf98d                   ;3   =   3 *
    
Lf833
    jsr     Lfc79                   ;6         *
    jsr     Lfc67                   ;6         *
    lda     ram_C8                  ;3         *
    cmp     #$31                    ;2         *
    bne     Lf830                   ;2/3       *
    beq     Lf82d                   ;2/3 =  21 *
Lf841
    cmp     #$03                    ;2         *
    beq     Lf899                   ;2/3       *
    cpy     #$10                    ;2         *
    bcc     Lf84f                   ;2/3       *
    dec     ram_A3                  ;5         *
    dec     ram_A3                  ;5         *
    bne     Lf896                   ;2/3 =  20 *
Lf84f
    cpx     #$04                    ;2         *
    bcc     Lf876                   ;2/3       *
    jsr     Lfc79                   ;6         *
    jsr     Lfc67                   ;6         *
    lda     ram_C8                  ;3         *
    cmp     #$31                    ;2         *
    bne     Lf864                   ;2/3       *
    jsr     Lfc1f                   ;6         *
    beq     Lf896                   ;2/3 =  31 *
Lf864
    lda     ram_80                  ;3         *
    and     #$07                    ;2         *
    bne     Lf896                   ;2/3       *
    lda     #$cb                    ;2         *
    cmp     ram_A6                  ;3         *
    bne     Lf872                   ;2/3       *
    lda     #$bf                    ;2   =  16 *
Lf872
    sta     ram_A6                  ;3         *
    bne     Lf896                   ;2/3 =   5 *
Lf876
    cpx     #$01                    ;2         *
    bne     Lf87f                   ;2/3       *
    ldy     #$04                    ;2         *
    jsr     Lfa57                   ;6   =  12 *
Lf87f
    lda     ram_80                  ;3         *
    and     #$07                    ;2         *
    bne     Lf896                   ;2/3 =   7 *
Lf885
    txa                             ;2         *
    clc                             ;2         *
    adc     #$17                    ;2         *
    tax                             ;2         *
    jsr     Lfdc8                   ;6         *
    inc     ram_94                  ;5         *
    cpx     #$19                    ;2         *
    bne     Lf896                   ;2/3       *
    jsr     Lfbef                   ;6   =  29 *
Lf896
    jmp     Lf98d                   ;3   =   3 *
    
Lf899
    cpx     #$03                    ;2         *
    bcs     Lf8ae                   ;2/3       *
    lda     ram_80                  ;3         *
    and     #$07                    ;2         *
    bne     Lf8ab                   ;2/3       *
    inc     ram_94                  ;5         *
    txa                             ;2         *
    adc     #$0a                    ;2         *
    jsr     Lfdb4                   ;6   =  26 *
Lf8ab
    jmp     Lf98d                   ;3   =   3 *
    
Lf8ae
    lda     ram_80                  ;3         *
    lsr                             ;2         *
    bcc     Lf8b6                   ;2/3       *
    jsr     Lfc7f                   ;6   =  13 *
Lf8b6
    ldx     ram_A3                  ;3         *
    lda     ram_81                  ;3         *
    cmp     #$30                    ;2         *
    bcs     Lf8d2                   ;2/3       *
    lda     ram_80                  ;3         *
    and     #$40                    ;2         *
    beq     Lf8cc                   ;2/3       *
    cpx     #$3f                    ;2         *
    bcs     Lf8d2                   ;2/3       *
    inc     ram_A3                  ;5         *
    bne     Lf8d2                   ;2/3 =  28 *
Lf8cc
    cpx     #$11                    ;2         *
    bcc     Lf8d2                   ;2/3       *
    dec     ram_A3                  ;5   =   9 *
Lf8d2
    lda     ram_80                  ;3         *
    and     #$03                    ;2         *
    bne     Lf8ab                   ;2/3       *
    ldx     #$f7                    ;2         *
    cpx     ram_A6                  ;3         *
    bne     Lf8e0                   ;2/3       *
    ldx     #$e9                    ;2   =  16 *
Lf8e0
    stx     ram_A6                  ;3         *
    bne     Lf8ab                   ;2/3 =   5 *
Lf8e4
    cpy     #$0e                    ;2         *
    bcc     Lf8f7                   ;2/3       *
    lda     ram_C4                  ;3         *
    beq     Lf8f7                   ;2/3       *
    lda     #$40                    ;2         *
    sta     ram_87                  ;3         *
    dec     ram_A3                  ;5         *
    dec     ram_A3                  ;5   =  24 *
Lf8f4
    jmp     Lf98d                   ;3   =   3 *
    
Lf8f7
    cpx     #$06                    ;2         *
    beq     Lf950                   ;2/3!      *
    txa                             ;2         *
    bne     Lf907                   ;2/3!      *
    bit     ram_DF                  ;3         *
    bmi     Lf907                   ;2/3       *
    ldy     #$08                    ;2         *
    jsr     Lfa57                   ;6   =  21 *
Lf907
    lda     ram_80                  ;3         *
    and     #$07                    ;2         *
    bne     Lf8f4                   ;2/3!      *
    txa                             ;2         *
    clc                             ;2         *
    adc     #$0d                    ;2         *
    tax                             ;2         *
    jsr     Lfdc3                   ;6         *
    lda     #$bc                    ;2         *
    sta     ram_B2                  ;3         *
    lda     #$02                    ;2         *
    sta     ram_C4                  ;3         *
    ldy     #$00                    ;2         *
    lda     ram_CA                  ;3         *
    cmp     ram_C8                  ;3         *
    bcc     Lf927                   ;2/3       *
    ldy     #$08                    ;2   =  43 *
Lf927
    sty     ram_DC                  ;3         *
    lda     #$0d                    ;2         *
    sta     ram_A3                  ;3         *
    bit     ram_DF                  ;3         *
    bpl     Lf934                   ;2/3       *
    dec     ram_94                  ;5         *
    .byte   $2c ;bit                ;4-5 =  17 *
Lf934
    inc     ram_94                  ;5         *
    lda     ram_94                  ;3         *
    cmp     #$fe                    ;2         *
    beq     Lf98a                   ;2/3       *
    cmp     #$06                    ;2         *
    bne     Lf98d                   ;2/3       *
    lda     #$9a                    ;2         *
    sta     ram_B2                  ;3         *
    lda     #$00                    ;2         *
    sta     ram_C4                  ;3         *
    lda     #$2f                    ;2         *
    sta     ram_A3                  ;3         *
    lda     #$03                    ;2         *
    sta     ram_B4                  ;3   =  36 *
Lf950
    dec     ram_87                  ;5         *
    beq     Lf980                   ;2/3       *
    ldy     #$67                    ;2         *
    lda     ram_87                  ;3         *
    cmp     #$05                    ;2         *
    bcc     Lf97b                   ;2/3       *
    cmp     #$20                    ;2         *
    bcs     Lf98d                   ;2/3       *
    lda     ram_C8                  ;3         *
    ldy     #$01                    ;2         *
    ldx     ram_DC                  ;3         *
    beq     Lf96d                   ;2/3       *
    dey                             ;2         *
    adc     #$04                    ;2         *
    bcc     Lf970                   ;2/3 =  36 *
Lf96d
    sec                             ;2         *
    sbc     #$05                    ;2   =   4 *
Lf970
    sta     ram_C9                  ;3         *
    lda     #$34                    ;2         *
    sta     ram_91                  ;3         *
    jsr     Lfd9c                   ;6         *
    ldy     #$1c                    ;2   =  16 *
Lf97b
    sty     ram_B7                  ;3         *
    jmp     Lf98d                   ;3   =   6 *
    
Lf980
    lda     #$04                    ;2         *
    sta     ram_94                  ;3         *
    sec                             ;2         *
    ror     ram_DF                  ;5         *
    jmp     Lf98d                   ;3   =  15 *
    
Lf98a
    jsr     Lfc1f                   ;6   =   6 *
Lf98d
    lda     ram_BC                  ;3        
    beq     Lf9b0                   ;2/3      
    jsr     Lfdd3                   ;6        
    lda     ram_BC                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    tay                             ;2        
    lda     Lff49,y                 ;4        
    and     ram_80                  ;3        
    bne     Lf9a3                   ;2/3      
    jsr     Lfdd3                   ;6   =  35
Lf9a3
    ldx     ram_BC                  ;3        
    cpx     #$02                    ;2        
    bcs     Lf9b0                   ;2/3      
    ldx     #$00                    ;2        
    stx     ram_BC                  ;3        
    jsr     Lfd4d                   ;6   =  18
Lf9b0
    jsr     Lfbda                   ;6        
    bcc     Lfa28                   ;2/3!     
    ldy     ram_DE                  ;3        
    bmi     Lfa28                   ;2/3!     
    beq     Lfa14                   ;2/3!     
    ldx     ram_C9                  ;3         *
    cpy     #$06                    ;2         *
    bne     Lf9cd                   ;2/3       *
    cpx     #$50                    ;2         *
    bcc     Lfa25                   ;2/3!      *
    lda     ram_B7                  ;3         *
    cmp     #$05                    ;2         *
    bcs     Lf9d5                   ;2/3       *
    bcc     Lfa25                   ;2/3!=  35 *
Lf9cd
    cpx     #$5a                    ;2         *
    bcc     Lf9e2                   ;2/3       *
    cpx     #$69                    ;2         *
    bcs     Lf9ee                   ;2/3 =   8 *
Lf9d5
    bit     ram_90                  ;3         *
    bmi     Lf9ee                   ;2/3       *
    bvc     Lf9de                   ;2/3       *
    inc     ram_B7                  ;5         *
    .byte   $2c ;bit                ;4-5 =  11 *
Lf9de
    dec     ram_B7                  ;5         *
    bne     Lf9ee                   ;2/3 =   7 *
Lf9e2
    lda     #$08                    ;2         *
    bit     ram_90                  ;3         *
    bne     Lf9ee                   ;2/3       *
    sta     ram_C2                  ;3         *
    eor     ram_90                  ;3         *
    sta     ram_90                  ;3   =  16 *
Lf9ee
    cpy     #$06                    ;2         *
    beq     Lfa14                   ;2/3!      *
    ldy     #$02                    ;2         *
    lda     ram_80                  ;3         *
    and     #$08                    ;2         *
    bne     Lfa07                   ;2/3!      *
    lda     ram_80                  ;3         *
    and     #$07                    ;2         *
    bne     Lfa05                   ;2/3       *
    ldy     #$06                    ;2         *
    jsr     Lfa57                   ;6   =  28 *
Lfa05
    ldy     #$03                    ;2   =   2 *
Lfa07
    jsr     Lfda0                   ;6         *
    bit     CXP1FB                  ;3         *
    bvc     Lfa14                   ;2/3       *
    lda     ram_B6                  ;3         *
    cmp     #$04                    ;2         *
    beq     Lfa25                   ;2/3 =  18 *
Lfa14
    jsr     Lfd18                   ;6        
    lda     ram_80                  ;3        
    and     #$03                    ;2        
    bne     Lfa20                   ;2/3      
    jsr     Lfd18                   ;6   =  19
Lfa20
    jsr     Lfbda                   ;6        
    bcs     Lfa28                   ;2/3 =   8
Lfa25
    jsr     Lfbcf                   ;6   =   6
Lfa28
    lda     ram_80                  ;3        
    and     #$03                    ;2        
    bne     Lfa38                   ;2/3      
    lda     #$01                    ;2        
    jsr     Lfdf9                   ;6        
    bcs     Lfa38                   ;2/3      
    jsr     Lfaae                   ;6   =  23 *
Lfa38
    ldy     ram_B5                  ;3        
    bmi     Lfa54                   ;2/3      
    dec     ram_8A                  ;5        
    bne     Lfa54                   ;2/3      
    sty     ram_C2                  ;3        
    lda     Lff68,y                 ;4        
    and     #$40                    ;2        
    beq     Lfa4f                   ;2/3      
    jsr     Lfd4d                   ;6        
    jmp     Lfa54                   ;3   =  32
    
Lfa4f
    ldx     #$01                    ;2        
    jsr     Lfd4f                   ;6   =   8
Lfa54
    jmp     Lffec                   ;3   =   3
    
Lfa57
    cpy     ram_C0                  ;3        
    beq     Lfa71                   ;2/3      
    lda     ram_C0                  ;3        
    cmp     #$09                    ;2        
    bcc     Lfa65                   ;2/3      
    cpy     #$09                    ;2         *
    bne     Lfa71                   ;2/3 =  16 *
Lfa65
    sty     ram_C0                  ;3        
    lda     Lff18,y                 ;4        
    sta     ram_BE                  ;3        
    lda     Lff22,y                 ;4        
    sta     AUDC0                   ;3   =  17
Lfa71
    rts                             ;6   =   6
    
Lfa72
    lda     #$20                    ;2        
    sta     ram_9B                  ;3        
    asl                             ;2        
    sta     ram_A2                  ;3        
    lda     #$00                    ;2        
    sta     ram_9C                  ;3        
    ldy     #$2a                    ;2   =  17
Lfa7f
    ldx     #$31                    ;2   =   2
Lfa81
    lda     Lff8a,x                 ;4        
    sta     ram_9B,x                ;4        
    dex                             ;2        
    dey                             ;2        
    bpl     Lfa81                   ;2/3      
    ldx     #$16                    ;2        
    lda     #$df                    ;2   =  18
Lfa8e
    sta     ram_E1,x                ;4        
    dex                             ;2        
    dex                             ;2        
    bne     Lfa8e                   ;2/3      
    jsr     Lfc61                   ;6        
    lsr                             ;2        
    lda     #$d1                    ;2        
    bcs     Lfaa0                   ;2/3      
    ldx     #$0c                    ;2         *
    lda     #$31                    ;2   =  26 *
Lfaa0
    stx     ram_86                  ;3        
    sta     ram_CA                  ;3        
    rts                             ;6   =  12
    
Lfaa5
    lda     ram_CB                  ;3        
    sec                             ;2        
    sbc     #$07                    ;2        
  IF CHEAT == 1
    bpl     Lfaba                   ;2/3      
  ELSE
    beq     Lfaae                   ;2/3      
  ENDIF
    bcs     Lfab8                   ;2/3 =  11
Lfaae
    ldy     #$09                    ;2        
    jsr     Lfa57                   ;6        
    lda     #$01                    ;2        
    sta     ram_A2                  ;3        
    lsr                             ;2   =  15
Lfab8
    sta     ram_CB                  ;3        
Lfaba
    rts                             ;6   =   9
    
Lfabb
    clc                             ;2   =   2
Lfabc
    lda     #$00                    ;2        
    sta     ram_DA                  ;3        
    bcs     Lfae8                   ;2/3      
    bit     ram_BD                  ;3        
    bmi     Lfae8                   ;2/3      
    jsr     Lfd24                   ;6        
    bcs     Lfad1                   ;2/3      
    jsr     Lfc61                   ;6        
    lsr                             ;2        
    bcs     Lfb4c                   ;2/3!=  30
Lfad1
    lda     ram_86                  ;3        
    beq     Lfaeb                   ;2/3      
    cmp     #$0c                    ;2        
    bcc     Lfaf5                   ;2/3      
    lda     ram_CA                  ;3         *
    cmp     #$3a                    ;2         *
    bcs     Lfaf1                   ;2/3       *
    jsr     Lfc61                   ;6         *
    lsr                             ;2         *
    bcc     Lfb4c                   ;2/3!      *
    jmp     Lfb81                   ;3   =  29 *
    
Lfae8
    jmp     Lfb47                   ;3   =   3
    
Lfaeb
    lda     ram_CA                  ;3        
    cmp     #$83                    ;2        
    bcc     Lfaf5                   ;2/3 =   7
Lfaf1
    dec     ram_CA                  ;5        
    bne     Lfb2e                   ;2/3!=   7
Lfaf5
    ldx     #$01                    ;2   =   2
Lfaf7
    inc     ram_C6,x                ;6        
    lda     ram_C6,x                ;4        
    cmp     #$d2                    ;2        
    bcc     Lfb05                   ;2/3!     
    lda     #$32                    ;2        
    sta     ram_C6,x                ;4        
    inc     ram_86                  ;5   =  25
Lfb05
    dex                             ;2        
    bpl     Lfaf7                   ;2/3!     
    ldx     ram_B6                  ;3        
    bmi     Lfb18                   ;2/3      
    beq     Lfb12                   ;2/3      
    cpx     #$03                    ;2        
    bcc     Lfb2e                   ;2/3 =  15
Lfb12
    lda     ram_D1                  ;3        
    beq     Lfb2e                   ;2/3      
    bne     Lfb23                   ;2/3 =   7
Lfb18
    cpx     #$80                    ;2         *
    beq     Lfb23                   ;2/3       *
    lda     ram_94                  ;3         *
    cmp     Lfdf8,x                 ;4         *
    bcs     Lfb2e                   ;2/3 =  13 *
Lfb23
    inc     ram_C8                  ;5        
    lda     ram_C8                  ;3        
    cmp     #$d2                    ;2        
    bcc     Lfb2e                   ;2/3      
    jsr     Lfc1f                   ;6   =  18
Lfb2e
    lda     ram_A2                  ;3        
    bmi     Lfb4c                   ;2/3 =   5
Lfb32
    lda     ram_80                  ;3        
    and     #$07                    ;2        
    bne     Lfb4c                   ;2/3      
    lda     #<Ldab4-1                ;2        
    sta     ram_AE                  ;3        
    ldx     #$07                    ;2        
    lda     ram_A4                  ;3        
    cmp     #$6c                    ;2        
    bne     Lfb49                   ;2/3      
    ldx     #$08                    ;2        
    .byte   $2c ;bit                ;4-3 =  24
Lfb47
    ldx     ram_DD                  ;3   =   3
Lfb49
    jmp     Lfd63                   ;3   =   3
    
Lfb4c
    rts                             ;6   =   6
    
Lfb4d
    clc                             ;2   =   2 *
Lfb4e
    lda     #$08                    ;2        
    sta     ram_DA                  ;3        
    bcs     Lfb47                   ;2/3      
    bit     ram_BD                  ;3        
    bmi     Lfb47                   ;2/3      
    jsr     Lfd24                   ;6        
    bcs     Lfb63                   ;2/3      
    jsr     Lfc61                   ;6         *
    lsr                             ;2         *
    bcc     Lfbce                   ;2/3 =  30 *
Lfb63
    ldx     ram_CA                  ;3        
    lda     ram_86                  ;3        
    beq     Lfb73                   ;2/3      
    cmp     #$0c                    ;2        
    bcc     Lfb92                   ;2/3      
    cpx     #$83                    ;2         *
    bcs     Lfb92                   ;2/3       *
    bcc     Lfb77                   ;2/3 =  18 *
Lfb73
    cpx     #$c8                    ;2        
    bcs     Lfb7b                   ;2/3 =   4
Lfb77
    inc     ram_CA                  ;5        
    bne     Lfb2e                   ;2/3 =   7
Lfb7b
    jsr     Lfc61                   ;6         *
    lsr                             ;2         *
    bcs     Lfbce                   ;2/3 =  10 *
Lfb81
    lda     #$80                    ;2         *
    sta     ram_B6                  ;3         *
    jsr     Lfc1f                   ;6         *
    sec                             ;2         *
    ror     ram_A2                  ;5         *
    lda     #$30                    ;2         *
    sta     ram_94                  ;3         *
    jmp     Lfd4d                   ;3   =  26 *
    
Lfb92
    ldx     #$01                    ;2   =   2
Lfb94
    dec     ram_C6,x                ;6        
    lda     ram_C6,x                ;4        
    cmp     #$32                    ;2        
    bcs     Lfba2                   ;2/3      
    lda     #$d1                    ;2        
    sta     ram_C6,x                ;4        
    dec     ram_86                  ;5   =  25
Lfba2
    dex                             ;2        
    bpl     Lfb94                   ;2/3      
    ldx     ram_B6                  ;3        
    bmi     Lfbb5                   ;2/3      
    beq     Lfbaf                   ;2/3      
    cpx     #$03                    ;2        
    bcc     Lfbcb                   ;2/3 =  15
Lfbaf
    lda     ram_D1                  ;3        
    beq     Lfbcb                   ;2/3      
    bne     Lfbc0                   ;2/3 =   7
Lfbb5
    cpx     #$80                    ;2         *
    beq     Lfbc0                   ;2/3       *
    lda     ram_94                  ;3         *
    cmp     Lfdf8,x                 ;4         *
    bcs     Lfbcb                   ;2/3 =  13 *
Lfbc0
    dec     ram_C8                  ;5        
    lda     ram_C8                  ;3        
    cmp     #$32                    ;2        
    bcs     Lfbcb                   ;2/3      
    jsr     Lfc1f                   ;6   =  18 *
Lfbcb
    jmp     Lfb2e                   ;3   =   3
    
Lfbce
    rts                             ;6   =   6 *
    
Lfbcf
    lda     #$67                    ;2        
    sta     ram_B7                  ;3        
    lda     #$31                    ;2        
    sta     ram_C9                  ;3        
    sta     ram_C2                  ;3        
    rts                             ;6   =  19
    
Lfbda
    lda     ram_C9                  ;3   =   3
Lfbdc
    cmp     #$d1                    ;2        
    bcs     Lfbe4                   ;2/3      
    cmp     #$32                    ;2        
    bcs     Lfbe5                   ;2/3 =   8
Lfbe4
    clc                             ;2   =   2
Lfbe5
    rts                             ;6   =   6
    
Lfbe6
    bit     ram_C2                  ;3        
    bvs     Lfbee                   ;2/3      
    lda     #$40                    ;2        
    sta     ram_C2                  ;3   =  10
Lfbee
    rts                             ;6   =   6
    
Lfbef
    lda     ram_CA                  ;3         *
    cmp     ram_C8                  ;3         *
    bcc     Lfbf7                   ;2/3       *
    clc                             ;2         *
    .byte   $24 ;bit                ;3-2 =  11 *
Lfbf7
    sec                             ;2   =   2 *
Lfbf8
    lda     #$00                    ;2        
    bcs     Lfc01                   ;2/3!     
    lda     #$80                    ;2        
    ldy     #$08                    ;2        
    .byte   $2c ;bit                ;4-2 =  10
Lfc01
    ldy     #$00                    ;2        
    sty     ram_DC                  ;3        
    sta     ram_DF                  ;3        
    rts                             ;6   =  14
    
Lfc08
    bcc     Lfc0f                   ;2/3      
    lda     #$d1                    ;2        
    sta     ram_C8                  ;3        
    .byte   $2c ;bit                ;4-5 =   6
Lfc0f
    inc     ram_C8                  ;5        
    rts                             ;6   =  11
    
Lfc12
    jsr     Lfbda                   ;6        
    bcs     Lfc1c                   ;2/3      
    lda     ram_81                  ;3        
    and     #$03                    ;2        
    .byte   $2c ;bit                ;4-2 =  15
Lfc1c
    lda     #$00                    ;2        
    rts                             ;6   =   8
    
Lfc1f
    lda     #$31                    ;2        
    ldx     #$67                    ;2        
    sta     ram_C8                  ;3        
    stx     ram_A3                  ;3        
    ldy     ram_B6                  ;3        
    cpy     #$80                    ;2        
    bne     Lfc31                   ;2/3      
    sta     ram_C9                  ;3         *
    stx     ram_B7                  ;3   =  23 *
Lfc31
    lda     #$00                    ;2        
    sta     ram_BF                  ;3        
    sta     ram_87                  ;3        
    beq     Lfc46                   ;2/3 =  10
Lfc39
    lda     #$31                    ;2        
    sta     ram_C8                  ;3        
    jsr     Lfc61                   ;6        
    tay                             ;2        
    lda     ram_81                  ;3        
    and     Lfe8a,y                 ;4   =  20
Lfc46
    sta     ram_93                  ;3        
    lda     #$40                    ;2        
    sta     ram_B6                  ;3        
    lda     #$3e                    ;2        
    sta     ram_AA                  ;3        
    lda     #$dc                    ;2        
    sta     ram_AB                  ;3        
    lda     #$00                    ;2        
    sta     ram_A2                  ;3        
    sta     ram_C1                  ;3        
    sta     ram_C2                  ;3        
    sta     ram_D1                  ;3        
    sta     ram_DF                  ;3   =  35
Lfc60
    rts                             ;6   =   6
    
Lfc61
    ldy     ram_85                  ;3        
    lda.wy  ram_9F,y                ;4        
    rts                             ;6   =  13
    
Lfc67
    lda     ram_B6                  ;3        
    bne     Lfc6e                   ;2/3      
    lda     #$1f                    ;2        
    .byte   $2c ;bit                ;4-3 =   8
Lfc6e
    lda     ram_A1                  ;3        
    clc                             ;2        
    adc     ram_87                  ;3        
    sta     ram_87                  ;3        
    bcc     Lfc60                   ;2/3      
    bcs     Lfc7f                   ;2/3 =  15
Lfc79
    lda     #$00                    ;2         *
    sta     ram_BF                  ;3         *
    sta     ram_DB                  ;3   =   8 *
Lfc7f
    bit     ram_DF                  ;3        
    bmi     Lfc86                   ;2/3      
    dec     ram_C8                  ;5        
    .byte   $2c ;bit                ;4-5 =   9
Lfc86
    inc     ram_C8                  ;5        
    lda     ram_C8                  ;3        
    jsr     Lfbdc                   ;6        
    bcc     Lfc39                   ;2/3      
    tay                             ;2        
    ldx     ram_DB                  ;3        
    beq     Lfc60                   ;2/3      
    bit     ram_DF                  ;3        
    bmi     Lfcbc                   ;2/3      
    cpy     #$c0                    ;2        
    bne     Lfca2                   ;2/3      
    lda     Lffbf,x                 ;4        
    jmp     Lfcb3                   ;3   =  39
    
Lfca2
    cpy     #$b0                    ;2        
    bne     Lfcac                   ;2/3      
    lda     Lffc1,x                 ;4        
    jmp     Lfcb3                   ;3   =  11
    
Lfcac
    cpy     #$33                    ;2        
    bcs     Lfd17                   ;2/3!     
    lda     Lff2f,x                 ;4   =   8 *
Lfcb3
    cpy     #$80                    ;2        
    tay                             ;2        
    and     #$03                    ;2        
    bcs     Lfce8                   ;2/3      
    bcc     Lfced                   ;2/3 =  10 *
Lfcbc
    cpy     #$42                    ;2        
    bne     Lfcc6                   ;2/3      
    lda     Lffbb,x                 ;4        
    jmp     Lfce1                   ;3   =  11
    
Lfcc6
    cpy     #$52                    ;2        
    bne     Lfcd0                   ;2/3      
    lda     Lffbd,x                 ;4        
    jmp     Lfce1                   ;3   =  11
    
Lfcd0
    cpy     #$b0                    ;2        
    bne     Lfcda                   ;2/3      
    lda     Lffbf,x                 ;4         *
    jmp     Lfce1                   ;3   =  11 *
    
Lfcda
    cpy     #$c0                    ;2        
    bne     Lfd17                   ;2/3!     
    lda     Lff32,x                 ;4   =   8 *
Lfce1
    cpy     #$80                    ;2        
    tay                             ;2        
    and     #$03                    ;2        
    bcs     Lfced                   ;2/3 =   8
Lfce8
    cpx     ram_BF                  ;3        
    jmp     Lfcef                   ;3   =   6
    
Lfced
    cmp     ram_BF                  ;3   =   3 *
Lfcef
    beq     Lfcff                   ;2/3      
    ldx     ram_BF                  ;3        
    cpy     #$f3                    ;2        
    bne     Lfd0d                   ;2/3!     
    cpx     #$00                    ;2        
    bne     Lfd0d                   ;2/3!     
    lda     #$01                    ;2        
    bne     Lfd0d                   ;2/3!=  17
Lfcff
    cpy     #$11                    ;2        
    bne     Lfd17                   ;2/3      
    ldx     ram_BF                  ;3         *
    cpx     #$01                    ;2         *
    bne     Lfd17                   ;2/3       *
    lda     #$00                    ;2         *
    sta     ram_DB                  ;3   =  16 *
Lfd0d
    sta     ram_BF                  ;3        
    tya                             ;2        
    and     #$f0                    ;2        
    clc                             ;2        
    adc     ram_C8                  ;3        
    sta     ram_C8                  ;3   =  15
Lfd17
    rts                             ;6   =   6
    
Lfd18
    lda     ram_90                  ;3        
    and     #$08                    ;2        
    beq     Lfd21                   ;2/3      
    inc     ram_C9                  ;5        
    rts                             ;6   =  18
    
Lfd21
    dec     ram_C9                  ;5        
    rts                             ;6   =  11
    
Lfd24
    lda     ram_B6                  ;3        
    cmp     #$03                    ;2        
    bcc     Lfd4b                   ;2/3      
    cmp     #$08                    ;2        
    bcs     Lfd4b                   ;2/3 =  11
Lfd2e
    lda     ram_C8                  ;3        
    sec                             ;2        
    sbc     ram_CA                  ;3        
    cmp     #$09                    ;2        
    bcc     Lfd4a                   ;2/3      
    ldx     ram_BF                  ;3        
    lda     ram_C8                  ;3        
    clc                             ;2        
    adc     Lff87,x                 ;4        
    sta     ram_F8                  ;3        
    lda     ram_CA                  ;3        
    sec                             ;2        
    sbc     ram_F8                  ;3        
    cmp     #$09                    ;2
    bcs     Lfd4b                   ;2/3 =  39
Lfd4a
    rts                             ;6   =   6

Lfd4b
    sec                             ;2
    rts                             ;6   =   8

Lfd4d
    ldx     #$05                    ;2   =   2
Lfd4f
    ldy     #$ff                    ;2        
    sty     ram_B5                  ;3        
    cpx     #$05                    ;2        
    bne     Lfd63                   ;2/3 =   9
Lfd57
    lda     #<Ldab4-1               ;2        
    sta     ram_AE                  ;3        
    lda     #$d1                    ;2        
    sta     ram_AC                  ;3        
    lda     #$d7                    ;2        
    sta     ram_B0                  ;3   =  15
Lfd63
    ldy     #$00                    ;2   =   2
Lfd65
    stx     ram_DD                  ;3        
    lda     Lff3b,x                 ;4        
    sta.wy  ram_A4,y                ;5        
    lda     Lfee2,x                 ;4        
    sta.wy  ram_A5,y                ;5        
    cpx     #$05                    ;2        
    lda     #<Ldc3e                    ;2        
    ldx     #>Ldc3e                    ;2        
    bcs     Lfd95                   ;2/3      
    lda.wy  ram_DA,y                ;4        
    beq     Lfd82                   ;2/3      
    lda     #$01                    ;2   =  37
Lfd82
    sta     ram_FA                  ;3        
    lda     ram_DD                  ;3        
    asl                             ;2        
    adc     ram_FA                  ;3        
    tax                             ;2        
    lda     Lff6f,x                 ;4        
    sta     ram_F7                  ;3        
    lda     Lff79,x                 ;4        
    tax                             ;2        
    lda     ram_F7                  ;3   =  29
Lfd95
    sta.wy  ram_A8,y                ;5        
    stx     ram_A9,y                ;4        
    sec                             ;2        
    rts                             ;6   =  17
    
Lfd9c
    lda     ram_B6                  ;3        
    sta     ram_DE                  ;3   =   6
Lfda0
    lda     Lfebf,y                 ;4        
    sta     ram_F7                  ;3        
    lda     #$ff                    ;2-7 =   2
    sta     ram_F8                  ;7        
    ldy     #$04                    ;2   =   9
Lfdab
    lda     (ram_F7),y              ;5        
    sta.wy  ram_8B,y                ;5        
    dey                             ;2        
    bpl     Lfdab                   ;2/3      
    rts                             ;6   =  20
    
Lfdb4
    tax                             ;2   =   2 *
Lfdb5
    ldy     Lfeff,x                 ;4        
    sty     ram_B2                  ;3        
    ldy     Lff0c,x                 ;4        
    sty     ram_B3                  ;3        
    ldy     #$00                    ;2        
    sty     ram_D1                  ;3   =  19
Lfdc3
    ldy     Lfeec,x                 ;4        
    sty     ram_A7                  ;3   =   7
Lfdc8
    ldy     Lfec7,x                 ;4        
    sty     ram_A6                  ;3        
    ldy     Lfe8f,x                 ;4        
    sty     ram_B4                  ;3        
    rts                             ;6   =  20
    
Lfdd3
    bit     ram_84                  ;3        
    bvc     Lfde2                   ;2/3      
    dec     ram_BC                  ;5        
    inc     ram_A4                  ;5        
    inc     ram_AC                  ;5        
    inc     ram_AE                  ;5        
    inc     ram_B0                  ;5        
    rts                             ;6   =  36
    
Lfde2
    inc     ram_BC                  ;5        
    lda     ram_BC                  ;3        
    cmp     #$19                    ;2        
    bcc     Lfdf0                   ;2/3      
    lda     ram_84                  ;3        
    ora     #$40                    ;2        
    sta     ram_84                  ;3   =  20
Lfdf0
    dec     ram_A4                  ;5        
    dec     ram_AC                  ;5        
    dec     ram_AE                  ;5        
    dec     ram_B0                  ;5   =  20
Lfdf8
    rts                             ;6   =   6
    
Lfdf9
    sta     ram_F7                  ;3        
    sed                             ;2        
    sec                             ;2        
    lda     ram_9C                  ;3        
    sbc     ram_F7                  ;3        
    sta     ram_9C                  ;3        
    bcs     Lfe0d                   ;2/3      
    lda     ram_9B                  ;3        
    sbc     #$00                    ;2        
    sta     ram_9B                  ;3        
    cld                             ;2        
    rts                             ;6   =  34
    
Lfe0d
    bne     Lfe14                   ;2/3      
    lda     ram_9B                  ;3        
    bne     Lfe14                   ;2/3      
    clc                             ;2   =   9 *
Lfe14
    cld                             ;2        
    rts                             ;6   =   8
    
Lfe16
    ldx     #$01                    ;2   =   2 *
Lfe18
    sed                             ;2        
    clc                             ;2   =   4
Lfe1a
    adc     ram_98,x                ;4        
    sta     ram_98,x                ;4        
    lda     #$00                    ;2        
    dex                             ;2        
    bpl     Lfe1a                   ;2/3      
    cld                             ;2        
    bcc     Lfe2e                   ;2/3      
    stx     ram_98                  ;3         *
    stx     ram_99                  ;3         *
    stx     ram_9A                  ;3         *
    stx     ram_CF                  ;3   =  30 *
Lfe2e
    ldx     ram_85                  ;3        
    lda     ram_98                  ;3        
    cmp     #$07                    ;2        
    bne     Lfe44                   ;2/3      
    lda     ram_99                  ;3         *
    cmp     #$50                    ;2         *
    bcc     Lfe44                   ;2/3       *
    lda     ram_88,x                ;4         *
    bmi     Lfe44                   ;2/3       *
    ror     ram_88,x                ;6         *
    inc     ram_9D,x                ;6   =  35 *
Lfe44
    rts                             ;6   =   6
    
Lfe45
    lda     ram_80                  ;3        
    asl                             ;2        
    ldy     #$00                    ;2        
    bcs     Lfe4e                   ;2/3      
    ldy     #$24                    ;2   =  11
Lfe4e
    rts                             ;6   =   6
    
Lfe4f
    ldy     #$1d                    ;2        
    lda     ram_A6                  ;3        
    beq     Lfe57                   ;2/3      
    ldy     #$06                    ;2   =   9
Lfe57
    rts                             ;6   =   6

; Subroutine to swap player data, if 2 player game
; current player in index x
Lfe58
    lda     ram_D2                  ;3        
    beq     Lfe6e                   ;2/3      
    txa                             ;2        
    eor     #$01                    ;2        
    sta     ram_85                  ;3        
    ldx     #$02                    ;2   =  14
Lfe63
    lda     ram_98,x                ;4        
    ldy     ram_95,x                ;4        
    sta     ram_95,x                ;4        
    sty     ram_98,x                ;4        
    dex                             ;2        
Lfe6c
    bpl Lfe63                       ;2-4 =  16
Lfe6e
    rts                             ;6   =   6
    
    .byte   $05,$20,$30                     ; $fe6f (*)
Lfe72
    .byte   $60                             ; $fe72 (*)
    .byte   $10                             ; $fe73 (D)
    .byte   $20,$10,$10,$10,$ff             ; $fe74 (*)
Lfe79
    .byte   $04,$04,$03,$03                 ; $fe79 (D)
    .byte   $03,$02,$02,$03,$02,$03,$04     ; $fe7d (*)
Lfe84
    .byte   $00,$02                         ; $fe84 (D)
    .byte   $03                             ; $fe86 (*)
    .byte   $04,$04,$02                     ; $fe87 (D)
Lfe8a
    .byte   $03,$3f                         ; $fe8a (D)
    .byte   $1f,$0f,$07                     ; $fe8c (*)
Lfe8f
    .byte   $03,$16,$03                     ; $fe8f (D)
    .byte   $0d,$0d,$0e,$1c,$0c,$0d,$0c,$04 ; $fe92 (*)
    .byte   $07,$07,$14,$1f,$26,$27,$27,$00 ; $fe9a (*)
    .byte   $0c,$0d,$0c,$0b,$0b,$0a,$08,$0d ; $fea2 (*)
Lfeaa
    .byte   $df,$bf,$af,$9f,$5f,$7f,$6f,$ef ; $feaa (D)
    .byte   $fd,$fb,$fa,$f9,$f5,$f7,$f6     ; $feb2 (D)
Lfeb9
    .byte   $fe,$70                         ; $feb9 (D)
    .byte   $90,$b0,$d0,$f0                 ; $febb (*)
Lfebf
    .byte   <Lff50,<Lff55,<Lff5a,<Lff5f     ; $febf (*)
    .byte   <Lff64                          ; $fec3 (D)
Lfec4
    .byte   $6c                             ; $fec4 (D)
    .byte   $00                             ; $fec5 (*)
    .byte   $6c                             ; $fec6 (D)
Lfec7
    .byte   <Lda6c,<Ldb00,<Lda6c
    .byte   <Ld972,<Lde49,<Ld994,<Ld942,<Lde6e,<Lde93,<Ldea0,<Ldae1
    .byte   <Ldaf0,<Ldaf7,<Ld95e,<Ld923,<Ld806,<Ld854,<Ldece,<Ld82c

    .byte   $6e,$7a,$87,$3f,$a2,$ad,$b7,$cb ; $feda (*)
Lfee2
    .byte   >Ldf7f,>Ldeac,>Ldc7e,>Ldca2,>Ldb7f,>Lda48,>Lddad,>Lda6c
    .byte   >Lda90
    .byte   >Lda24
Lfeec
    .byte   >Lda6c,>Ldb00,>Lda6c
    .byte   >Ld972,>Lde49,>Ld994,>Ld942,>Lde6e,>Lde93,>Ldea0,>Ldae1
    .byte   >Ldaf0,>Ldaf7,>Ld95e,>Ld923,>Ld806,>Ld854,>Ldece,>Ld82c
Lfeff
    .byte   $5c,$a0,$b1                     ; $feff (D)
    .byte   $e3,$55,$86,$c5,$61,$61,$61,$f7 ; $ff02 (*)
    .byte   $f7,$f7                         ; $ff0a (*)
Lff0c
    .byte   $db,$db,$db                     ; $ff0c (D)
    .byte   $d8,$de,$d9,$dc,$de,$de,$de,$db ; $ff0f (*)
    .byte   $db                             ; $ff17 (*)
Lff18
    .byte   $db,$04,$03                     ; $ff18 (*)
    .byte   $0a                             ; $ff1b (D)
    .byte   $09                             ; $ff1c (*)
    .byte   $07,$06,$06                     ; $ff1d (D)
    .byte   $10                             ; $ff20 (*)
    .byte   $27                             ; $ff21 (D)
Lff22
    .byte   $27,$0c,$0c                     ; $ff22 (*)
    .byte   $08                             ; $ff25 (A)
    .byte   $04                             ; $ff26 (*)
    .byte   $08                             ; $ff27 (A)
    .byte   $08                             ; $ff28 (A)
    .byte   $0c                             ; $ff29 (A)
    .byte   $02                             ; $ff2a (*)
    .byte   $0c                             ; $ff2b (A)
    .byte   $0c                             ; $ff2c (*)
Lff2d
    .byte   $0f,$f0                         ; $ff2d (D)
Lff2f
    .byte   $00,$10,$20                     ; $ff2f (D)
Lff32
    .byte   $11                             ; $ff32 (D)
    .byte   $00                             ; $ff33 (*)
    .byte   $00                             ; $ff34 (D)
    .byte   $00,$02,$00,$00                 ; $ff35 (*)
    .byte   $00,$01                         ; $ff39 (D)
Lff3b
    .byte   <Ldf7f,<Ldeac,<Ldc7e,<Ldca2,<Ldb7f,<Lda48,<Lddad,<Lda6c
    .byte   <Lda90
    .byte   <Lda24
Lff45
    .byte   $00                             ; $ff45 (*)
Lff46
    .byte   $fe,$00,$02                     ; $ff46 (*)
Lff49
    .byte   $00,$00,$01,$01,$03,$03,$07     ; $ff49 (D)
Lff50
    .byte   $00,$d2,$d2,$d2,$d2
Lff55
    .byte   $00,$32,$32,$32,$32
Lff5a
    .byte   $00,$f2,$02,$12,$02
Lff5f
    .byte   $00,$12,$02,$f2,$02
Lff64
    .byte   $00,$00,$12,$12                 ; $ff64 (D)
Lff68
    .byte   $00,$40                         ; $ff68 (D)
    .byte   $41                             ; $ff6a (*)
    .byte   $01,$81,$c0,$c1                 ; $ff6b (D)
Lff6f
    .byte   <Ldd5b,<Ldc00,<Ldc36,<Ldd89,<Ldcda,<Ldc5a
    .byte   <Ldddc
    .byte   <Ldc12,<Ldd15,<Ldd38
Lff79
    .byte   >Ldd5b,>Ldc00,>Ldc36,>Ldd89,>Ldcda,>Ldc5a
    .byte   >Ldddc
    .byte   >Ldc12,>Ldd15,>Ldd38
Lff83
    .byte   $90                             ; $ff83 (D)
    .byte   $15                             ; $ff84 (*)
    .byte   $90                             ; $ff85 (D)
    .byte   $90                             ; $ff86 (*)
Lff87
    .byte   $00,$10,$20                     ; $ff87 (D)
Lff8a
    .byte   $20,$00,$03,$03,$01,$01,$1f,$40 ; $ff8a (D)
    .byte   $2f,$6c,$da,$6c,$da,$3e,$dc,$3e ; $ff92 (D)
    .byte   $dc,$d1,$dd,$b3,$da,$d7,$da,$b1 ; $ff9a (D)
    .byte   $db,$03,$ff,$40,$67,$0b,$0b,$ff ; $ffa2 (D)
    .byte   $fb,$00,$00,$00,$00,$00,$00,$00 ; $ffaa (D)
    .byte   $00,$02,$00,$bf,$5f,$31,$31,$31 ; $ffb2 (D)
    .byte   $27                             ; $ffba (D)
Lffbb
    .byte   $27                             ; $ffbb (D)
    .byte   $f1                             ; $ffbc (*)
Lffbd
    .byte   $00,$f3                         ; $ffbd (D)
Lffbf
    .byte   $e2,$01                         ; $ffbf (D)
Lffc1
    .byte   $00,$01                         ; $ffc1 (D)
Lffc3
    .byte   $02,$03                         ; $ffc3 (D)
    .byte   $03,$01,$01                     ; $ffc5 (*)
Lffc8
    .byte   $01,$80,$80,$81,$41,$40,$40,$02 ; $ffc8 (D)
Lffd0
    .byte   $00,$01,$02,$02                 ; $ffd0 (D)
Lffd4
    .byte   $05,$08,$02,$03                 ; $ffd4 (*)
    .byte   $01,$02                         ; $ffd8 (D)
    .byte   $20,$20,$30,$30,$30,$30,$50,$50 ; $ffda (*)
    .byte   $81,$81,$20,$20,$00,$00,$03,$02 ; $ffe2 (*)
    .byte   $05,$06                         ; $ffea (*)
  IF PLUSROM == 1
; F6: bank 1 -> bank 0 is done by Lffec at $f001, bank 0 -> bank 1 continues at Lf02d
  ORG $1ff8
  RORG $fff8
    .byte   $ea,$ea

    .word   PLUSROM_NMI             ; NMI: PlusROM API pointer (same in all banks)
    .word   Start_1_Cold            ; RESET
    .word   Start_1_Cold            ; IRQ


;***********************************************************
;      Bank 2 / 0..3 (PlusROM only, empty)
;***********************************************************

    SEG     CODE
    ORG     $2000
    RORG    $b000

Start_2
    sei
    bit     BANK1                   ; -> bank 1, continues at Start_1_Cold

    ORG     $2ffa
    RORG    $bffa
    .word   PLUSROM_NMI             ; NMI: PlusROM API pointer (same in all banks)
    .word   Start_2                 ; RESET
    .word   Start_2                 ; IRQ


;***********************************************************
;      Bank 3 / 0..3 (PlusROM only, HSC code)
;***********************************************************

    SEG     CODE
    ORG     $3000
    RORG    $9000

Start_3
    sei
    bit     BANK1                   ; -> bank 1, continues at Start_1_Cold
B3_FromBank1                        ; bank 1 (Lffec) -> bank 3 continues here
    jmp     HSCCheck
    .byte   $ea,$ea
ToBank0
    bit     BANK0                   ; -> bank 0, continues at Ld00c
B3_FromBank0                        ; bank 0 (Ldff2) -> bank 3 continues here
    jmp     ToBank1

; The bankswitch code of all banks must match:
  if ((Start_0 + 4) & $0fff) != (Start_1_Cold & $0fff) || ((Start_2 + 4) & $0fff) != (Start_1_Cold & $0fff) || ((Start_3 + 4) & $0fff) != (Start_1_Cold & $0fff)
    echo "ERROR: Start_x + 4 must match Start_1_Cold"
    err
  endif
  if ((Lffec + 3) & $0fff) != (B3_FromBank1 & $0fff) || ((ToBank0 + 3) & $0fff) != (Ld00c & $0fff)
    echo "ERROR: bankswitch bank 1 -> bank 3 -> bank 0 doesn't match"
    err
  endif
  if ((Ldff2 + 3) & $0fff) != (B3_FromBank0 & $0fff)
    echo "ERROR: bankswitch bank 0 -> bank 3 doesn't match"
    err
  endif

    ORG     $3000 + ((Lf02d - 3) & $0fff)
    RORG    $9000 + ((Lf02d - 3) & $0fff)
ToBank1
    bit     BANK1                   ; -> bank 1, continues at Lf02d

; Executed once per frame on the way from bank 1 to bank 0.
; Sends the score of a 1 player game once at game over.
; Joystick left on the start/game over screen shows the HSC table.
HSCCheck
    lda     ram_CF
    bpl     ToBank0                 ; the game is running
    cmp     #$ff                    ; game over (or power on)?
    bne     HSCCheckJoystick
    lda     ram_9D                  ; lives player 1, $ff after the last life
    cmp     #$ff                    ;   (0 at power on or when the game over is handled)
    bne     HSCCheckJoystick
    lda     #$80+HSC_SEND_DELAY     ; the game is over -> handle only once, lives = $80+delay
    sta     ram_9D                  ;   ($80..$fe are displayed like $ff, no lives)
    lda     ram_D2                  ; 2 player game?
    bne     ToBank0                 ;   yes, don't send
    lda     ram_98                  ; BCD score hi
    sta     WriteToBuffer
    lda     ram_99
    sta     WriteToBuffer
    lda     ram_9A                  ; BCD score lo
    sta     WriteToBuffer
    lda     #HIGHSCORE_ID           ; game id in Highscore DB
    sta     WriteSendBuffer         ; send request to backend..
    jmp     ToBank0

; The PlusCart needs some frames between two requests (and between reading the
; receive buffer and the next request), so the HSC table is blocked for
; HSC_SEND_DELAY frames after the game over.
HSCCheckJoystick
    lda     ram_9D
    bpl     HSCNoDelay              ; 0 (power on) or lives (RESET pressed)
    cmp     #$80
    beq     HSCNoDelay
    dec     ram_9D                  ; delay after the game over ($fe..$81 -> $80)
    jmp     ToBank0

HSCNoDelay
    bit     SWCHA                   ; joystick left?
    bvs     ToBank0                 ;   no
    lda     ReceiveBufferSize       ; $ff = ROM, no PlusROM device
    cmp     #$ff
    bne     EnterHSCScreen
    jmp     ToBank0

PlusROM_API
    .byte   "a", 0, "h.firmaplus.de", 0

    include "HSC_TextScreen.asm"

  if * > $9f00
    echo "ERROR: bank 3 too full, ends at ", *, " (the last page must not contain data)"
    err
  endif

PLUSROM_NMI = (PlusROM_API & $0fff) + $4000 ; PlusROM API pointer (image offset + $1000)

    ORG     $3ffa
    RORG    $9ffa
    .word   PLUSROM_NMI             ; NMI: PlusROM API pointer (same in all banks)
    .word   Start_3                 ; RESET
    .word   Start_3                 ; IRQ
  ELSE
Lffec
    bit     Lfff8                   ;4   =   4
Lffef
    jmp     Ldfef                   ;3   =   3 *
    
    nop                             ;2         *
    nop                             ;2         *
    nop                             ;2         *
Lfff5
    jmp     Lf02d                   ;3   =   9

  ORG $1ff8
  RORG $fff8
Lfff8
    .byte   $ea                             ; $fff8 (D)
Lfff9
    .byte   $ea                             ; $fff9 (D)

    .word   $eaea
    .word   Start_1
    .word   Start_1
  ENDIF
