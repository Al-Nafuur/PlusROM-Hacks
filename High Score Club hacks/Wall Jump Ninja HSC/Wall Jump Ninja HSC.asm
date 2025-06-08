; Disassembly of /Public ROMs/Homebrew/NTSC/T-Z/wall_jump_ninja_20150115_v1_0_NTSC.bin
; Disassembled 05/30/25 22:50:35
; Using Stella 7.0
;
; Copyright 2014 Walaber (https://www.walaber.com)
;
; Reverse-Engineered 2025 
; by Wolfgang Stubig (Al_Nafuur)
;
; ROM properties name : wall_jump_ninja_20150115_v1_0_NTSC
; ROM properties MD5  : 3c56c0c5f6f97850ed0aa7bcc2a4e30e
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

PLUSROM          = 1
PAL60            = 0

;-----------------------------------------------------------
;      Color constants
;-----------------------------------------------------------

BLACK            = $00

  IF PAL60
BLACK1           = $10
YELLOW           = $20
GREEN_YELLOW     = $30
GREEN_BEIGE      = $30
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
  ELSE
YELLOW           = $10
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

BROWN            = $20


;-----------------------------------------------------------
;      TIA and IO constants accessed
;-----------------------------------------------------------

CXM0P           = $00  ; (R)
CXM1P           = $01  ; (R)
CXP0FB          = $02  ; (R)
CXPPMM          = $07  ; (R)
INPT4           = $0c  ; (R)
INPT5           = $0d  ; (R)

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
PF0             = $0d  ; (W)
PF1             = $0e  ; (W)
PF2             = $0f  ; (W)
RESP0           = $10  ; (W)
;RESP1          = $11  ; (Wi)
;RESM0          = $12  ; (Wi)
;RESM1          = $13  ; (Wi)
;RESBL          = $14  ; (Wi)
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
;HMP1           = $21  ; (Wi)
HMM0            = $22  ; (W)
HMM1            = $23  ; (W)
HMBL            = $24  ; (W)
VDELP0          = $25  ; (W)
VDELP1          = $26  ; (W)
HMOVE           = $2a  ; (W)
HMCLR           = $2b  ; (W)
CXCLR           = $2c  ; (W)

SWCHA           = $0280
SWACNT          = $0281
SWCHB           = $0282
INTIM           = $0284
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
ram_86          = $86
ram_87          = $87
ram_88          = $88
ram_89          = $89
ram_8A          = $8a
ram_8B          = $8b
ram_8C          = $8c
ram_8D          = $8d
ram_8E          = $8e
ram_8F          = $8f
ram_90          = $90
ram_91          = $91
ram_92          = $92
ram_93          = $93
ram_94          = $94
ram_95          = $95
ram_96          = $96
ram_97          = $97
ram_98          = $98
ram_99          = $99
ram_9A          = $9a
ram_9B          = $9b
ram_9C          = $9c
ram_9D          = $9d
ram_9E          = $9e
ram_9F          = $9f
ram_A0          = $a0
ram_A1          = $a1
ram_A2          = $a2
ram_A3          = $a3
ram_A4          = $a4
ram_A5          = $a5
ram_A6          = $a6
;                 $a7  (i)
;                 $a8  (i)
;                 $a9  (i)
;                 $aa  (i)
;                 $ab  (i)
ram_AC          = $ac
;                 $ad  (i)
ram_AE          = $ae
ram_AF          = $af
ram_B0          = $b0
ram_B1          = $b1
ram_B2          = $b2
;                 $b3  (i)
;                 $b4  (i)
;                 $b5  (i)
ram_B6          = $b6
ram_B7          = $b7
ram_B8          = $b8
ram_B9          = $b9
ram_BA          = $ba
;                 $bb  (i)
ram_BC          = $bc
;                 $bd  (i)
;                 $be  (i)
;                 $bf  (i)
;                 $c0  (i)
;                 $c1  (i)
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
ram_D4          = $d4
ram_D5          = $d5
ram_D6          = $d6
;                 $d7  (i)
;                 $d8  (i)
;                 $d9  (i)
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

;                 $f6  (s)
;                 $f7  (s)
;                 $f8  (s)
;                 $f9  (s)
;                 $fa  (s)
;                 $fb  (s)
;                 $fc  (s)
;                 $fd  (s)
;                 $fe  (s)
;                 $ff  (s)


;-----------------------------------------------------------
;      User Defined Labels
;-----------------------------------------------------------

Start           = $f000

  IF PLUSROM == 1
WriteToBuffer           = $1FF0
WriteSendBuffer         = $1FF1
ReceiveBuffer           = $1FF2
ReceiveBufferSize       = $1FF3

HIGHSCORE_ID            = 85         ; Wall Jump Ninja game ID in Highscore DB
  ENDIF


;***********************************************************
;      Bank 0
;***********************************************************

    SEG     CODE
    ORG     $f000

Start
    sei                             ;2        
    cld                             ;2        
    ldx     #$ff                    ;2        
    txs                             ;2        
    lda     #$00                    ;2   =  10
Lf007
    sta     VSYNC,x                 ;4        
    dex                             ;2        
    bne     Lf007                   ;2/3      
  IF PLUSROM
    jsr     LoadHSCTable            ;6        
    jmp     Lf030 
  ELSE

    bit     INPT4|$30               ;3        
    bpl     Lf030                   ;2/3      
    ldx     #$00                    ;2        
    jsr     Lf447                   ;6        
    bcc     Lf030                   ;2/3      
    jsr     Lfe25                   ;6        
    jsr     Lfdd2                   ;6   =  35

Lf01d
    jsr     Lfdfb                   ;6        
    cmp     #$ff                    ;2        
    bne     Lf026                   ;2/3      
    lda     #$00                    ;2   =  12
Lf026
    sta     ram_D6,x                ;4        
    inx                             ;2        
    cpx     #$04                    ;2        
    bne     Lf01d                   ;2/3      
    jsr     Lfe1e                   ;6   =  16
  ENDIF

    org $f030
Lf030
    jsr     Lfbd4                   ;6        
    jmp     Lf089                   ;3   =   9
    
Lf036
    ldx     #$4d                    ;2        
    lda     #$00                    ;2   =   4
Lf03a
    sta     ram_80,x                ;4        
    dex                             ;2        
    bne     Lf03a                   ;2/3      
    sta     AUDV0                   ;3        
    lda     #$3c                    ;2        
    sta     ram_8C                  ;3        
    lda     #$fe                    ;2        
    sta     ram_DB                  ;3        
    lda     #$ce                    ;2        
    sta     ram_DA                  ;3        
    lda     #$10                    ;2        
    sta     NUSIZ0                  ;3        
    sta     NUSIZ1                  ;3        
    lda     #$d0                    ;2        
    sta     ram_9D                  ;3        
    sta     ram_9E                  ;3        
    sta     HMCLR                   ;3        
    sta     WSYNC                   ;3   =  48
;---------------------------------------
    lda     #$2f                    ;2        
    ldx     #$02                    ;2        
    jsr     Lfdc0                   ;6        
    lda     #$2c                    ;2        
    sta     ram_8D                  ;3        
    lda     #$77                    ;2        
    ldx     #$03                    ;2        
    jsr     Lfdc0                   ;6        
    sta     WSYNC                   ;3   =  28
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     #$01                    ;2        
    sta     ram_9B                  ;3        
    ldx     #$07                    ;2        
    stx     ram_DF                  ;3   =  13
Lf07b
    lda     Lffe5,x                 ;4        
    sta     ram_BC,x                ;4        
    dex                             ;2        
    bpl     Lf07b                   ;2/3      
    jsr     Lfa3a                   ;6        
    jmp     Lfb22                   ;3   =  21
    
Lf089
    jsr     Lf0c8_Vertical_Sync     ;6        
    lda     ram_C4                  ;3        
    beq     Lf09c                   ;2/3      
    jsr     Lf0d3                   ;6        
    jsr     Lfbf9                   ;6        
    jsr     Lfa02                   ;6        
    jmp     Lf089                   ;3   =  32
    
Lf09c
    lda     ram_A3                  ;3        
    bne     Lf0b0                   ;2/3      
    lda     ram_99                  ;3        
    bne     Lf0aa                   ;2/3      
    jsr     Lf14e                   ;6        
    jmp     Lf0bc                   ;3   =  19
    
Lf0aa
    jsr     Lf613                   ;6        
    jmp     Lf0bc                   ;3   =   9
    
Lf0b0
    jsr     Lf74e                   ;6         *
    jsr     Lf9bd                   ;6         *
    jsr     Lf92d                   ;6         *
    jmp     Lf0c2                   ;3   =  21 *
    
Lf0bc
    jsr     Lf85a                   ;6        
    jsr     Lf92d                   ;6   =  12
Lf0c2
    jsr     Lfa02                   ;6        
    jmp     Lf089                   ;3   =   9
    
Lf0c8_Vertical_Sync
    lda     #$02                    ;2        
    sta     VSYNC                   ;3        
    sta     WSYNC                   ;3   =   8
;---------------------------------------
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    rts                             ;6   =   6
    
Lf0d3
    lda     #$00                    ;2        
    sta     VSYNC                   ;3        
    sta     VBLANK                  ;3        
    ldx     #$2b                    ;2        
    stx     TIM64T                  ;4        
    sta     HMCLR                   ;3        
    lda     #$3b                    ;2        
    ldx     #$01                    ;2        
    jsr     Lfdc0                   ;6        
    lda     #$43                    ;2        
    ldx     #$00                    ;2        
    jsr     Lfdc0                   ;6        
    sta     WSYNC                   ;3   =  40
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     #$00                    ;2        
    sta     PF0                     ;3        
    lda     ram_81                  ;3        
    cmp     #$e1                    ;2        
    bne     Lf105                   ;2/3!     
    lda     #$0e                    ;2         *
    sta     ram_DC                  ;3         *
    sta     ram_DD                  ;3         *
    jmp     Lf114                   ;3   =  26 *
    
Lf105
    bit     INPT4|$30               ;3        
    bmi     Lf110                   ;2/3      
    lda     #$01                    ;2        
    sta     ram_CC                  ;3        
    jmp     Lf122                   ;3   =  13
    
Lf110
    lda     ram_CC                  ;3        
    beq     Lf122                   ;2/3 =   5
Lf114
    sta     CXCLR                   ;3        
    bit     INPT5|$30               ;3        
    bmi     Lf11e                   ;2/3      
    lda     #YELLOW|$e              ;2         *
    sta     ram_DD                  ;3   =  13 *
Lf11e
    lda     #$02                    ;2        
    sta     ram_D0                  ;3   =   5
Lf122
    jsr     Lf572                   ;6        
    beq     Lf12e                   ;2/3      
    ldx     ram_D4                  ;3         *
    lda     ram_D6,x                ;4         *
    jsr     Lfb78                   ;6   =  21 *
Lf12e
    jsr     Lf5bb                   ;6        
    jsr     Lfda5                   ;6        
    lda     ram_80                  ;3        
    lsr                             ;2        
    eor     #$ff                    ;2        
    sta     AUDV0                   ;3        
    lda     ram_81                  ;3        
    lsr                             ;2        
    and     #$3f                    ;2        
    tay                             ;2        
    lda     Lfe99,y                 ;4        
    sta     AUDF0                   ;3   =  38
Lf146
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     INTIM                   ;4        
    bne     Lf146                   ;2/3      
    rts                             ;6   =  12
    
Lf14e
    lda     #$00                    ;2        
    sta     VSYNC                   ;3        
    sta     VBLANK                  ;3        
    ldx     #$2b                    ;2        
    stx     TIM64T                  ;4        
    jsr     Lfda5                   ;6        
    lda     ram_C5                  ;3        
    bne     Lf168                   ;2/3      
    jsr     Lf2c2                   ;6        
    jsr     Lf340                   ;6        
    sta     CXCLR                   ;3   =  40
Lf168
    jsr     Lf262                   ;6        
    jsr     Lf1f4                   ;6        
    jsr     Lf45e                   ;6        
    jsr     Lf4ae                   ;6        
    jsr     Lf51a                   ;6        
    lda     #BLACK|$0               ;2        
    sta     COLUBK                  ;3        
    sta     COLUPF                  ;3        
    sta     ram_95                  ;3        
    sta     ram_96                  ;3        
    lda     ram_86                  ;3        
    beq     Lf190                   ;2/3      
    lda     #$fe                    ;2        
    sta     ram_85                  ;3        
    lda     #$3d                    ;2        
    sta     ram_84                  ;3        
    jmp     Lf1a7                   ;3   =  62
    
Lf190
    lda     ram_8F                  ;3        
    beq     Lf19f                   ;2/3      
    lda     #$fe                    ;2        
    sta     ram_85                  ;3        
    lda     #$46                    ;2        
    sta     ram_84                  ;3        
    jmp     Lf1a7                   ;3   =  18
    
Lf19f
    lda     #$fe                    ;2        
    sta     ram_85                  ;3        
    lda     #$4f                    ;2        
    sta     ram_84                  ;3   =  10
Lf1a7
    lda     #$25                    ;2        
    sta     CTRLPF                  ;3        
    lda     SWCHB                   ;4        
    and     #$01                    ;2        
    bne     Lf1b6                   ;2/3      
  IF PLUSROM
    jmp     ReloadHSCTable
    nop
  ELSE
    lda     #$01                    ;2         *
    sta     ram_D0                  ;3   =  18 *
  ENDIF
Lf1b6
    lda     ram_C5                  ;3        
    beq     Lf1c0                   ;2/3      
    cmp     #$c8                    ;2        
    bcs     Lf1c0                   ;2/3      
    inc     ram_C5                  ;5   =  14
Lf1c0
    ldy     ram_DC                  ;3        
    beq     Lf1cb                   ;2/3      
    cmp     #$50                    ;2         *
    bcc     Lf1cb                   ;2/3       *
    jmp     Lf1e4                   ;3   =  12 *
    
Lf1cb
    bit     INPT4|$30               ;3        
    bmi     Lf1e0                   ;2/3      
    lda     ram_DC                  ;3        
    bne     Lf1d9                   ;2/3      
    lda     ram_C5                  ;3        
    cmp     #$3c                    ;2        
    bcc     Lf1e0                   ;2/3 =  17
Lf1d9
    lda     #$01                    ;2         *
    sta     ram_CD                  ;3         *
    jmp     Lf1ec                   ;3   =   8 *
    
Lf1e0
    lda     ram_CD                  ;3        
    beq     Lf1e8                   ;2/3 =   5
Lf1e4
    lda     #$01                    ;2         *
    sta     ram_D0                  ;3   =   5 *
Lf1e8
    lda     #$00                    ;2        
    sta     ram_CD                  ;3   =   5
Lf1ec
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     INTIM                   ;4        
    bne     Lf1ec                   ;2/3      
    rts                             ;6   =  12
    
Lf1f4
    inc     ram_8B                  ;5        
    lda     ram_8B                  ;3        
    cmp     #$04                    ;2        
    bmi     Lf21a                   ;2/3!     
    lda     #$00                    ;2        
    sta     ram_8B                  ;3        
    inc     ram_89                  ;5        
    lda     ram_89                  ;3        
    cmp     #$01                    ;2        
    bmi     Lf21a                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_89                  ;3        
    inc     ram_8A                  ;5        
    lda     ram_8A                  ;3        
    cmp     #$05                    ;2        
    bmi     Lf21a                   ;2/3      
    lda     #$04                    ;2        
    sta     ram_8A                  ;3        
    sta     ram_E1                  ;3   =  54
Lf21a
    ldy     ram_8A                  ;3        
    lda     (ram_87),y              ;5        
    jsr     Lfdb0                   ;6        
    lda     ram_8F                  ;3        
    bne     Lf242                   ;2/3      
    sec                             ;2        
    lda     ram_8C                  ;3        
    sbc     ram_E0                  ;3        
    sta     ram_8C                  ;3        
    cmp     #$f0                    ;2        
    bne     Lf261                   ;2/3      
    lda     ram_C5                  ;3        
    bne     Lf23b                   ;2/3      
    lda     #$64                    ;2         *
    sta     ram_8C                  ;3         *
    jmp     Lf261                   ;3   =  47 *
    
Lf23b
    lda     #$f1                    ;2        
    sta     ram_8C                  ;3        
    jmp     Lf261                   ;3   =   8
    
Lf242
    clc                             ;2        
    lda     ram_8C                  ;3        
    adc     ram_E0                  ;3        
    sta     ram_8C                  ;3        
    lda     ram_E1                  ;3        
    cmp     #$04                    ;2        
    bne     Lf261                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_8F                  ;3        
    sta     ram_8B                  ;3        
    sta     ram_8A                  ;3        
    sta     ram_89                  ;3        
    lda     #$ff                    ;2        
    sta     ram_88                  ;3        
    lda     #$18                    ;2        
    sta     ram_87                  ;3   =  42
Lf261
    rts                             ;6   =   6
    
Lf262
    lda     ram_86                  ;3        
    bne     Lf2a3                   ;2/3      
    lda     Lff12                   ;4        
    jsr     Lfdb0                   ;6        
    lda     ram_91                  ;3        
    beq     Lf2a3                   ;2/3      
    cmp     #$01                    ;2        
    bne     Lf27e                   ;2/3      
    sec                             ;2        
    lda     ram_8D                  ;3        
    sbc     ram_E0                  ;3        
    sta     ram_8D                  ;3        
    jmp     Lf2a3                   ;3   =  38
    
Lf27e
    lda     ram_8D                  ;3        
    sta     ram_E1                  ;3        
    clc                             ;2        
    lda     ram_8D                  ;3        
    adc     ram_E0                  ;3        
    sta     ram_8D                  ;3        
    lda     ram_E1                  ;3        
    cmp     #$6e                    ;2        
    bpl     Lf2a3                   ;2/3      
    lda     ram_8D                  ;3        
    cmp     #$6c                    ;2        
    bmi     Lf2a3                   ;2/3      
    lda     ram_8C                  ;3        
    cmp     #$64                    ;2        
    bmi     Lf2a3                   ;2/3      
    lda     #$6d                    ;2         *
    sta     ram_8D                  ;3         *
    lda     #$00                    ;2         *
    sta     ram_91                  ;3   =  48 *
Lf2a3
    lda     ram_8D                  ;3        
    cmp     #$2a                    ;2        
    bpl     Lf2ad                   ;2/3      
    lda     #$2c                    ;2         *
    sta     ram_8D                  ;3   =  12 *
Lf2ad
    sta     HMCLR                   ;3        
    clc                             ;2        
    lda     ram_8D                  ;3        
    adc     #$03                    ;2        
    ldx     #$00                    ;2        
    jsr     Lfdc0                   ;6        
    lda     ram_8E                  ;3        
    sta     REFP0                   ;3        
    sta     WSYNC                   ;3   =  27
;---------------------------------------
    sta     HMOVE                   ;3        
    rts                             ;6   =   9
    
Lf2c2
    lda     ram_DC                  ;3        
    beq     Lf2e6                   ;2/3      
    lda     ram_DE                  ;3         *
    beq     Lf2cf                   ;2/3       *
    dec     ram_DE                  ;5         *
    jmp     Lf2ea                   ;3   =  18 *
    
Lf2cf
    lda     #$01                    ;2         *
    sta     ram_92                  ;3         *
    lda     ram_82                  ;3         *
    sec                             ;2         *
    sbc     ram_83                  ;3         *
    clc                             ;2         *
    adc     #$08                    ;2         *
    cmp     ram_8C                  ;3         *
    bcc     Lf2e6                   ;2/3       *
    lda     #$0f                    ;2         *
    sta     ram_DE                  ;3         *
    jmp     Lf2ea                   ;3   =  30 *
    
Lf2e6
    bit     INPT4|$30               ;3        
    bmi     Lf337                   ;2/3!=   5
Lf2ea
    lda     ram_86                  ;3        
    beq     Lf31f                   ;2/3!     
    lda     ram_92                  ;3        
    beq     Lf33f                   ;2/3!     
    lda     #$01                    ;2        
    sta     ram_8F                  ;3        
    lda     #$ff                    ;2        
    sta     ram_88                  ;3        
    lda     #$13                    ;2        
    sta     ram_87                  ;3        
    lda     #$18                    ;2        
    sta     ram_90                  ;3        
    lda     #$00                    ;2        
    sta     ram_92                  ;3        
    sta     ram_86                  ;3        
    sta     ram_8B                  ;3        
    sta     ram_89                  ;3        
    sta     ram_8A                  ;3        
    lda     #$04                    ;2        
    sta     ram_93                  ;3        
    lda     #$14                    ;2        
    ldx     #$06                    ;2        
    ldy     #$02                    ;2        
    jsr     Lf50f                   ;6        
    lda     #$fc                    ;2        
    sta     ram_C8                  ;3   =  69
Lf31f
    lda     ram_90                  ;3        
    cmp     #$00                    ;2        
    beq     Lf33f                   ;2/3      
    dec     ram_90                  ;5        
    lda     #$00                    ;2        
    sta     ram_8A                  ;3        
    sta     ram_89                  ;3        
    lda     ram_8B                  ;3        
    lsr                             ;2        
    beq     Lf33f                   ;2/3      
    inc     ram_8C                  ;5        
    jmp     Lf33f                   ;3   =  35
    
Lf337
    lda     #$01                    ;2        
    sta     ram_92                  ;3        
    lda     #$00                    ;2        
    sta     ram_90                  ;3   =  10
Lf33f
    rts                             ;6   =   6
    
Lf340
    lda     ram_93                  ;3        
    cmp     #$00                    ;2        
    beq     Lf348                   ;2/3      
    dec     ram_93                  ;5   =  12
Lf348
    lda     ram_93                  ;3        
    bne     Lf37c                   ;2/3      
    lda     ram_C5                  ;3        
    bne     Lf37c                   ;2/3      
    bit     CXPPMM|$30              ;3        
    bmi     Lf385                   ;2/3      
    bit     CXP0FB|$30              ;3        
    bvs     Lf38e                   ;2/3      
    lda     ram_D5                  ;3        
    beq     Lf362                   ;2/3      
    lda     ram_8C                  ;3         *
    cmp     #$06                    ;2         *
    bmi     Lf38e                   ;2/3 =  32 *
Lf362
    bit     CXM0P|$30               ;3        
    bvs     Lf3c9                   ;2/3      
    bit     CXM1P|$30               ;3        
    bmi     Lf3dc                   ;2/3      
    lda     ram_8D                  ;3        
    cmp     #$8c                    ;2        
    bpl     Lf37d                   ;2/3      
    lda     ram_86                  ;3        
    beq     Lf37c                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_86                  ;3        
    sta     ram_91                  ;3        
    sta     ram_8E                  ;3   =  33
Lf37c
    rts                             ;6   =   6
    
Lf37d
    lda     #$12                    ;2        
    sta     ram_99                  ;3        
    jsr     Lf431                   ;6        
    rts                             ;6   =  17
    
Lf385
    lda     ram_A0                  ;3        
    cmp     #$02                    ;2        
    bne     Lf38e                   ;2/3      
    jmp     Lf40e                   ;3   =  10
    
Lf38e
    lda     #$01                    ;2        
    sta     ram_C5                  ;3        
    ldx     #$18                    ;2        
    ldy     #$07                    ;2        
    jsr     Lf50f                   ;6        
    lda     #$fc                    ;2        
    sta     ram_C8                  ;3        
    sta     ram_8F                  ;3        
    lda     #$ff                    ;2        
    sta     ram_88                  ;3        
    lda     #$13                    ;2        
    sta     ram_87                  ;3        
    lda     #$00                    ;2        
    sta     ram_90                  ;3        
    sta     ram_86                  ;3        
    sta     ram_91                  ;3        
    sta     ram_8B                  ;3        
    sta     ram_89                  ;3        
    sta     ram_8A                  ;3        
    stx     ram_93                  ;3        
    lda     ram_DC                  ;3        
    bne     Lf3c8                   ;2/3      
  IF ! PLUSROM
    lda     ram_9B                  ;3        
    ldx     ram_D4                  ;3        
    cmp     ram_D6,x                ;4        
    bmi     Lf3c8                   ;2/3      
    sta     ram_D6,x                ;4        
  ENDIF
    jsr     Lf439                   ;6   =  83
Lf3c8
    rts                             ;6   =   6

    org $f3c9
Lf3c9
    lda     ram_86                  ;3        
    bne     Lf37c                   ;2/3      
    lda     #$02                    ;2        
    sta     ram_91                  ;3        
    lda     #$00                    ;2        
    sta     ram_8E                  ;3        
    lda     #$2c                    ;2        
    sta     ram_8D                  ;3        
    jmp     Lf3ec                   ;3   =  23
    
Lf3dc
    lda     ram_86                  ;3        
    bne     Lf37c                   ;2/3      
    lda     #$01                    ;2        
    sta     ram_91                  ;3        
    lda     #$08                    ;2        
    sta     ram_8E                  ;3        
    lda     #$6d                    ;2        
    sta     ram_8D                  ;3   =  20
Lf3ec
    lda     #$01                    ;2        
    sta     ram_86                  ;3        
    ldx     #$05                    ;2        
    ldy     #$0e                    ;2        
    jsr     Lf50f                   ;6        
    lda     #$02                    ;2        
    sta     ram_C8                  ;3        
    lda     #$00                    ;2        
    sta     ram_8F                  ;3        
    sta     ram_8B                  ;3        
    sta     ram_8A                  ;3        
    sta     ram_89                  ;3        
    lda     #$ff                    ;2        
    sta     ram_88                  ;3        
    lda     #$18                    ;2        
    sta     ram_87                  ;3        
    rts                             ;6   =  50
    
Lf40e
    lda     #$0f                    ;2        
    sta     ram_CA                  ;3        
    inc     ram_A2                  ;5        
    lda     ram_A2                  ;3        
    cmp     #$05                    ;2        
    bne     Lf42a                   ;2/3      
    lda     #$06                    ;2         *
    sta     ram_A3                  ;3         *
    lda     #$12                    ;2         *
    sta     ram_99                  ;3         *
    lda     ram_8D                  ;3         *
    sta     ram_CF                  ;3         *
    lda     #$02                    ;2         *
    sta     ram_91                  ;3   =  38 *
Lf42a
    lda     #$d0                    ;2        
    sta     ram_9D                  ;3        
    jmp     Lfb22                   ;3   =   8
    
Lf431
    lda     ram_83                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    sta     ram_9A                  ;3        
    rts                             ;6   =  18

Lf439    
  IF PLUSROM
SendPlusROMScore
    lda     ram_D4                  ;3        
    sta     WriteToBuffer           ; game variation
    lda     ram_9B                  ;3        
    sta     WriteToBuffer           ; BCD score
    lda     #HIGHSCORE_ID           ; game id in Highscore DB
    sta     WriteSendBuffer
    rts                             ;6   =   8 *

PlusROM_API
       .byte "a", 0, "h.firmaplus.de"
       .byte 0

  ELSE 
    jsr     Lf447                   ;6        
    bcc     Lf446                   ;2/3      
    lda     ram_D6,x                ;4        
    jsr     Lfde1                   ;6        
    jsr     Lfe25                   ;6   =  24
Lf446
    rts                             ;6   =   6

Lf447
    stx     ram_E0                  ;3        
    jsr     Lfdd5                   ;6        
    bne     Lf45c                   ;2/3      
    clv                             ;2        
    clc                             ;2        
    lda     #$07                    ;2        
    adc     ram_E0                  ;3        
    jsr     Lfde1                   ;6        
    lda     #$00                    ;2        
    jmp     Lfde1                   ;3   =  31
    
Lf45c
    clc                             ;2         *
    rts                             ;6   =   8 *
  ENDIF
    
    org $f45e
Lf45e
    lda     ram_8B                  ;3        
    bne     Lf476                   ;2/3      
    lda     ram_CB                  ;3        
    cmp     #$78                    ;2        
    bpl     Lf46a                   ;2/3      
    inc     ram_CB                  ;5   =  17
Lf46a
    lda     ram_D4                  ;3        
    and     #$01                    ;2        
    beq     Lf476                   ;2/3      
    lda     ram_8A                  ;3         *
    bne     Lf476                   ;2/3       *
    inc     ram_CB                  ;5   =  17 *
Lf476
    lda     #$00                    ;2        
    sta     ram_98                  ;3        
    sec                             ;2        
    lda     ram_CB                  ;3        
    sbc     #$14                    ;2        
    bcc     Lf495                   ;2/3      
    clc                             ;2        
    cmp     #$9f                    ;2        
    bcs     Lf495                   ;2/3      
    sta     HMCLR                   ;3        
    ldx     #$04                    ;2        
    jsr     Lfdc0                   ;6        
    sta     WSYNC                   ;3   =  34
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     #$02                    ;2        
    sta     ram_98                  ;3   =   8
Lf495
    lda     ram_BC                  ;3        
    sta     ram_E0                  ;3        
    ldy     #$00                    ;2        
    ldx     #$01                    ;2   =  10
Lf49d
    lda     ram_BC,x                ;4        
    sta.wy  ram_BC,y                ;5        
    inx                             ;2        
    iny                             ;2        
    tya                             ;2        
    cmp     #$07                    ;2        
    bne     Lf49d                   ;2/3      
    lda     ram_E0                  ;3        
    sta     ram_C3                  ;3        
    rts                             ;6   =  31
    
Lf4ae
    sta     HMCLR                   ;3        
    sta     WSYNC                   ;3   =   6
;---------------------------------------
    clc                             ;2        
    lda     ram_9F                  ;3        
    adc     #$03                    ;2        
    cmp     #$9c                    ;2        
    bmi     Lf4bd                   ;2/3      
    lda     #$9c                    ;2   =  13
Lf4bd
    ldx     #$01                    ;2        
    jsr     Lfdc0                   ;6        
    sta     WSYNC                   ;3   =  11
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     ram_A0                  ;3        
    cmp     #$01                    ;2        
    bne     Lf50e                   ;2/3!     
    lda     ram_9D                  ;3         *
    cmp     #$04                    ;2         *
    bmi     Lf4d9                   ;2/3       *
    cmp     #$5a                    ;2         *
    bpl     Lf4e0                   ;2/3       *
    jmp     Lf4e4                   ;3   =  24 *
    
Lf4d9
    lda     #$01                    ;2         *
    sta     ram_A1                  ;3         *
    jmp     Lf4e4                   ;3   =   8 *
    
Lf4e0
    lda     #$00                    ;2         *
    sta     ram_A1                  ;3   =   5 *
Lf4e4
    sec                             ;2         *
    lda     ram_9D                  ;3         *
    sbc     #$04                    ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    cmp     #$0c                    ;2         *
    bcc     Lf4f2                   ;2/3       *
    lda     #$0b                    ;2   =  19 *
Lf4f2
    tay                             ;2         *
    lda     Lff1d,y                 ;4         *
    jsr     Lfdb0                   ;6         *
    lda     ram_A1                  ;3         *
    beq     Lf507                   ;2/3!      *
    clc                             ;2         *
    lda     ram_9D                  ;3         *
    adc     ram_E0                  ;3         *
    sta     ram_9D                  ;3         *
    jmp     Lf50e                   ;3   =  31 *
    
Lf507
    sec                             ;2         *
    lda     ram_9D                  ;3         *
    sbc     ram_E0                  ;3         *
    sta     ram_9D                  ;3   =  11 *
Lf50e
    rts                             ;6   =   6
    
Lf50f
    sta     ram_C7                  ;3        
    stx     ram_C6                  ;3        
    sty     AUDC1                   ;3        
    lda     #$08                    ;2        
    sta     ram_C9                  ;3        
    rts                             ;6   =  20
    
Lf51a
    lda     ram_DC                  ;3        
    bne     Lf571                   ;2/3      
    lda     ram_86                  ;3        
    beq     Lf531                   ;2/3      
    lda     #$08                    ;2        
    sta     AUDC0                   ;3        
    lda     #$01                    ;2        
    sta     AUDV0                   ;3        
    lda     #$19                    ;2        
    sta     AUDF0                   ;3        
    jmp     Lf533                   ;3   =  28
    
Lf531
    sta     AUDV0                   ;3   =   3
Lf533
    lda     ram_C6                  ;3        
    beq     Lf54c                   ;2/3      
    lda     ram_C9                  ;3        
    sta     AUDV1                   ;3        
    lda     ram_C7                  ;3        
    sta     AUDF1                   ;3        
    clc                             ;2        
    adc     ram_C8                  ;3        
    sta     ram_C7                  ;3        
    dec     ram_C6                  ;5        
    bne     Lf54c                   ;2/3      
    lda     #$00                    ;2        
    sta     AUDV1                   ;3   =  37
Lf54c
    lda     ram_CA                  ;3        
    beq     Lf571                   ;2/3      
    lda     ram_80                  ;3        
    and     #$01                    ;2        
    beq     Lf55b                   ;2/3      
    lda     #$04                    ;2        
    jmp     Lf55d                   ;3   =  17
    
Lf55b
    lda     #$08                    ;2   =   2
Lf55d
    sta     AUDF1                   ;3        
    lda     ram_CA                  ;3        
    and     #$07                    ;2        
    sta     AUDV1                   ;3        
    lda     #$04                    ;2        
    sta     AUDC1                   ;3        
    dec     ram_CA                  ;5        
    lda     ram_CA                  ;3        
    bne     Lf571                   ;2/3      
    sta     AUDV1                   ;3   =  29
Lf571
    rts                             ;6   =   6
    
Lf572
    lda     ram_D4                  ;3        
    sta     ram_E0                  ;3        
    lda     #$00                    ;2        
    sta     ram_D4                  ;3        
    bit     SWCHB                   ;4        
    bvs     Lf584                   ;2/3      
    sta     ram_D5                  ;3        
    jmp     Lf58e                   ;3   =  23
    
Lf584
    lda     #$04                    ;2         *
    sta     ram_D5                  ;3         *
    lda     ram_D4                  ;3         *
    ora     #$02                    ;2         *
    sta     ram_D4                  ;3   =  13 *
Lf58e
    bit     SWCHB                   ;4        
    bmi     Lf5a3                   ;2/3      
    lda     #$0a                    ;2        
    sta     ram_D1                  ;3        
    lda     #$14                    ;2        
    sta     ram_D2                  ;3        
    clc                             ;2        
    adc     #$14                    ;2        
    sta     ram_D3                  ;3        
    jmp     Lf5b6                   ;3   =  26
    
Lf5a3
    lda     #$04                    ;2         *
    sta     ram_D1                  ;3         *
    lda     #$08                    ;2         *
    sta     ram_D2                  ;3         *
    clc                             ;2         *
    adc     #$08                    ;2         *
    sta     ram_D3                  ;3         *
    lda     ram_D4                  ;3         *
    ora     #$01                    ;2         *
    sta     ram_D4                  ;3   =  25 *
Lf5b6
    lda     ram_D4                  ;3        
    cmp     ram_E0                  ;3        
    rts                             ;6   =  12
    
Lf5bb
    ldy     #$00                    ;2        
    bit     SWCHB                   ;4        
    bpl     Lf5ca                   ;2/3      
    lda     #$00                    ;2         *
    jsr     Lf5e6                   ;6         *
    jmp     Lf5cf                   ;3   =  19 *
    
Lf5ca
    lda     #$01                    ;2        
    jsr     Lf5e6                   ;6   =   8
Lf5cf
    lda     #$00                    ;2        
    sta     ram_AC                  ;3        
    ldy     #$07                    ;2        
    bit     SWCHB                   ;4        
    bvc     Lf5e0                   ;2/3      
    jsr     Lf5e6                   ;6         *
    jmp     Lf5e5                   ;3   =  22 *
    
Lf5e0
    lda     #$01                    ;2        
    jsr     Lf5e6                   ;6   =   8
Lf5e5
    rts                             ;6   =   6
    
Lf5e6
    cmp     #$01                    ;2        
    bne     Lf5f5                   ;2/3      
    lda     #$08                    ;2        
    sta     ram_E0                  ;3        
    lda     #$ff                    ;2        
    sta     ram_E1                  ;3        
    jmp     Lf5fd                   ;3   =  17
    
Lf5f5
  IF PAL60
    lda     #$5e                    ;2        
  ELSE
    lda     #$ce                    ;2         *
  ENDIF

    sta     ram_E0                  ;3         *
    lda     #$1f                    ;2         *
    sta     ram_E1                  ;3   =  10 *
Lf5fd
    ldx     #$06                    ;2   =   2
Lf5ff
    lda     Lffcc,y                 ;4        
    and     ram_E1                  ;3        
    sta.wy  ram_AE,y                ;5        
    lda     ram_E0                  ;3        
    sta.wy  ram_BC,y                ;5        
    iny                             ;2        
    dex                             ;2        
    bne     Lf5ff                   ;2/3!     
    stx     ram_C2                  ;3        
    rts                             ;6   =  35
    
Lf613
    lda     #$00                    ;2        
    sta     VSYNC                   ;3        
    sta     VBLANK                  ;3        
    ldx     #$2b                    ;2        
    stx     TIM64T                  ;4        
    dec     ram_99                  ;5        
    lda     ram_DC                  ;3        
    bne     Lf631                   ;2/3      
    lda     #$0c                    ;2        
    sta     AUDC0                   ;3        
    lda     ram_99                  ;3        
    asl                             ;2        
    sta     AUDF0                   ;3        
    lda     ram_99                  ;3        
    sta     AUDV0                   ;3   =  43
Lf631
    lda     ram_99                  ;3        
    cmp     #$07                    ;2        
    bne     Lf642                   ;2/3      
    inc     ram_9B                  ;5        
    jsr     Lfa3a                   ;6        
    jsr     Lfb22                   ;6        
    jsr     Lf700                   ;6   =  30
Lf642
    bcc     Lf64f                   ;2/3      
    sec                             ;2        
    lda     ram_83                  ;3        
    sbc     ram_9A                  ;3        
    bpl     Lf64d                   ;2/3      
    lda     #$00                    ;2   =  14
Lf64d
    sta     ram_83                  ;3   =   3
Lf64f
    lda     ram_99                  ;3        
    bne     Lf6a0                   ;2/3      
    sta     HMCLR                   ;3        
    sta     WSYNC                   ;3   =  11
;---------------------------------------
    lda     #$2f                    ;2        
    ldx     #$02                    ;2        
    jsr     Lfdc0                   ;6        
    lda     #$77                    ;2        
    ldx     #$03                    ;2        
    jsr     Lfdc0                   ;6        
    sta     WSYNC                   ;3   =  23
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     ram_9B                  ;3        
    cmp     ram_D1                  ;3        
    bmi     Lf689                   ;2/3      
    and     #$01                    ;2        
    cmp     #$00                    ;2        
    beq     Lf689                   ;2/3      
    lda     ram_A0                  ;3        
    cmp     #$01                    ;2        
    bpl     Lf682                   ;2/3      
    lda     #$2c                    ;2        
    sta     ram_9F                  ;3        
    jmp     Lf69d                   ;3   =  32
    
Lf682
    lda     #$4c                    ;2        
    sta     ram_9F                  ;3        
    jmp     Lf69d                   ;3   =   8
    
Lf689
    lda     ram_A0                  ;3        
    cmp     #$01                    ;2        
    bpl     Lf696                   ;2/3      
    lda     #$74                    ;2        
    sta     ram_9F                  ;3        
    jmp     Lf69d                   ;3   =  15
    
Lf696
    lda     #$94                    ;2        
    sta     ram_9F                  ;3        
    jmp     Lf69d                   ;3   =   8
    
Lf69d
    jmp     Lf6ca                   ;3   =   3
    
Lf6a0
    lda     #$00                    ;2        
    sta     HMCLR                   ;3        
    lda     #$40                    ;2        
    sta     HMM0                    ;3        
    sta     HMM1                    ;3        
    sta     WSYNC                   ;3   =  16
;---------------------------------------
    sta     HMOVE                   ;3        
    sec                             ;2        
    lda     ram_8D                  ;3        
    sbc     #$04                    ;2        
    sta     ram_8D                  ;3        
    sec                             ;2        
    lda     ram_9F                  ;3        
    sbc     #$04                    ;2        
    sta     ram_9F                  ;3        
    sec                             ;2        
    lda     ram_CB                  ;3        
    sbc     #$04                    ;2        
    bpl     Lf6c5                   ;2/3      
    lda     #$00                    ;2   =  34
Lf6c5
    sta     ram_CB                  ;3        
    jsr     Lf45e                   ;6   =   9
Lf6ca
    sta     CXCLR                   ;3        
    lda     #BLACK|$0               ;2        
    sta     COLUBK                  ;3        
    sta     COLUPF                  ;3        
    lda     #$25                    ;2        
    sta     CTRLPF                  ;3        
    sta     WSYNC                   ;3   =  19
;---------------------------------------
    sta     HMCLR                   ;3        
    clc                             ;2        
    lda     ram_8D                  ;3        
    adc     #$03                    ;2        
    ldx     #$00                    ;2        
    jsr     Lfdc0                   ;6        
    clc                             ;2        
    lda     ram_9F                  ;3        
    adc     #$03                    ;2        
    cmp     #$9c                    ;2        
    bmi     Lf6ef                   ;2/3      
    lda     #$9c                    ;2   =  31
Lf6ef
    ldx     #$01                    ;2        
    jsr     Lfdc0                   ;6        
    sta     WSYNC                   ;3   =  11
;---------------------------------------
    sta     HMOVE                   ;3   =   3
Lf6f8
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     INTIM                   ;4        
    bne     Lf6f8                   ;2/3      
    rts                             ;6   =  12
    
Lf700
    lda     #$00                    ;2        
    sta     ram_97                  ;3        
    sta     HMCLR                   ;3        
    sta     WSYNC                   ;3   =  11
;---------------------------------------
    lda     #$4f                    ;2        
    ldx     #$02                    ;2        
    jsr     Lfdc0                   ;6        
    lda     #$99                    ;2        
    ldx     #$03                    ;2        
    jsr     Lfdc0                   ;6        
    lda     ram_9B                  ;3        
    cmp     ram_D1                  ;3        
    bmi     Lf74d                   ;2/3      
    and     #$01                    ;2        
    cmp     #$00                    ;2        
    bne     Lf73a                   ;2/3      
    sta     WSYNC                   ;3   =  37
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     ram_A0                  ;3        
    cmp     #$01                    ;2        
    bpl     Lf733                   ;2/3      
    lda     #$94                    ;2        
    sta     ram_9F                  ;3        
    jmp     Lf74d                   ;3   =  18
    
Lf733
    lda     #$b4                    ;2        
    sta     ram_9F                  ;3        
    jmp     Lf74d                   ;3   =   8
    
Lf73a
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     ram_A0                  ;3        
    cmp     #$01                    ;2        
    bpl     Lf749                   ;2/3      
    lda     #$4c                    ;2        
    sta     ram_9F                  ;3        
    jmp     Lf74d                   ;3   =  15
    
Lf749
    lda     #$6c                    ;2        
    sta     ram_9F                  ;3   =   5
Lf74d
    rts                             ;6   =   6
    
Lf74e
    ldy     #$00                    ;2         *
    sty     VSYNC                   ;3         *
    sty     VBLANK                  ;3         *
    ldx     #$2b                    ;2         *
    stx     TIM64T                  ;4         *
    jsr     Lfda5                   ;6         *
    jsr     Lf51a                   ;6         *
    inc     ram_CE                  ;5         *
    lda     ram_CE                  ;3         *
    cmp     #$04                    ;2         *
    bmi     Lf769                   ;2/3       *
    sty     ram_CE                  ;3   =  41 *
Lf769
    lda     ram_CE                  ;3         *
    cmp     #$01                    ;2         *
    beq     Lf780                   ;2/3       *
    cmp     #$03                    ;2         *
    beq     Lf780                   ;2/3       *
    cmp     #$02                    ;2         *
    beq     Lf78c                   ;2/3       *
    lda     ram_CF                  ;3         *
    sta     ram_8D                  ;3         *
    lda     #$10                    ;2         *
    jmp     Lf795                   ;3   =  26 *
    
Lf780
    sec                             ;2         *
    lda     ram_CF                  ;3         *
    sbc     #$10                    ;2         *
    sta     ram_8D                  ;3         *
    lda     #$11                    ;2         *
    jmp     Lf795                   ;3   =  15 *
    
Lf78c
    sec                             ;2         *
    lda     ram_CF                  ;3         *
    sbc     #$20                    ;2         *
    sta     ram_8D                  ;3         *
    lda     #$13                    ;2   =  12 *
Lf795
    sta     NUSIZ0                  ;3         *
    lda     ram_81                  ;3         *
    and     #$01                    ;2         *
    beq     Lf7a2                   ;2/3       *
    dec     ram_8C                  ;5         *
    jmp     Lf7a4                   ;3   =  18 *
    
Lf7a2
    inc     ram_8C                  ;5   =   5 *
Lf7a4
    lda     #$fe                    ;2         *
    sta     ram_85                  ;3         *
    lda     #$58                    ;2         *
    sta     ram_84                  ;3         *
    lda     #$03                    ;2         *
    sta     AUDC0                   ;3         *
    lda     #$08                    ;2         *
    sta     AUDV0                   ;3         *
    lda     ram_80                  ;3         *
    and     #$07                    ;2         *
    sta     AUDF0                   ;3         *
    dec     ram_99                  ;5         *
    lda     ram_99                  ;3         *
    cmp     #$07                    ;2         *
    bne     Lf7cd                   ;2/3       *
    inc     ram_9B                  ;5         *
    jsr     Lfa3a                   ;6         *
    jsr     Lfb22                   ;6         *
    jsr     Lf844                   ;6   =  63 *
Lf7cd
    lda     ram_99                  ;3         *
    bne     Lf810                   ;2/3!      *
    sta     HMCLR                   ;3         *
    sta     WSYNC                   ;3   =  11 *
;---------------------------------------
    lda     #$2f                    ;2         *
    ldx     #$04                    ;2         *
    jsr     Lfdc0                   ;6         *
    lda     #$77                    ;2         *
    ldx     #$03                    ;2         *
    jsr     Lfdc0                   ;6         *
    lda     ram_8D                  ;3         *
    clc                             ;2         *
    adc     #$03                    ;2         *
    ldx     #$00                    ;2         *
    jsr     Lfdc0                   ;6         *
    sta     WSYNC                   ;3   =  38 *
;---------------------------------------
    sta     HMOVE                   ;3         *
    dec     ram_A3                  ;5         *
    lda     ram_A3                  ;3         *
    beq     Lf7fe                   ;2/3       *
    lda     #$12                    ;2         *
    sta     ram_99                  ;3         *
    jmp     Lf80d                   ;3   =  21 *
    
Lf7fe
    sta     ram_A2                  ;3         *
    lda     #$d0                    ;2         *
    sta     ram_9D                  ;3         *
    sta     ram_9E                  ;3         *
    lda     ram_CF                  ;3         *
    sta     ram_8D                  ;3         *
    jsr     Lfb22                   ;6   =  23 *
Lf80d
    jmp     Lf820                   ;3   =   3 *
    
Lf810
    sta     HMCLR                   ;3         *
    lda     #$40                    ;2         *
    sta     HMBL                    ;3         *
    sta     HMM1                    ;3         *
    sta     WSYNC                   ;3   =  14 *
;---------------------------------------
    sta     HMOVE                   ;3         *
    lda     #$00                    ;2         *
    sta     ram_CB                  ;3   =   8 *
Lf820
    sta     CXCLR                   ;3         *
    lda     #$25                    ;2         *
    sta     CTRLPF                  ;3         *
    sta     WSYNC                   ;3   =  11 *
;---------------------------------------
    sta     HMCLR                   ;3         *
    clc                             ;2         *
    lda     ram_8D                  ;3         *
    adc     #$03                    ;2         *
    ldx     #$00                    ;2         *
    jsr     Lfdc0                   ;6         *
    sta     WSYNC                   ;3   =  21 *
;---------------------------------------
    sta     HMOVE                   ;3         *
    stx     COLUBK                  ;3         *
    stx     COLUPF                  ;3   =   9 *
Lf83c
    sta     WSYNC                   ;3   =   3 *
;---------------------------------------
    lda     INTIM                   ;4         *
    bne     Lf83c                   ;2/3       *
    rts                             ;6   =  12 *
    
Lf844
    lda     #$00                    ;2         *
    sta     ram_97                  ;3         *
    sta     HMCLR                   ;3         *
    sta     WSYNC                   ;3   =  11 *
;---------------------------------------
    lda     #$4f                    ;2         *
    ldx     #$04                    ;2         *
    jsr     Lfdc0                   ;6         *
    lda     #$97                    ;2         *
    ldx     #$03                    ;2         *
    jmp     Lfdc0                   ;3   =  17 *
    
Lf85a
    sec                             ;2        
    lda     #$5a                    ;2        
    sbc     ram_D5                  ;3        
    tax                             ;2        
    lda     #BLACK|$2               ;2        
    sta     ENAM0                   ;3   =  14
Lf864
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     COLUBK                  ;3        
    txa                             ;2        
    sec                             ;2        
    sbc     ram_82                  ;3        
    adc     ram_83                  ;3        
    bcc     Lf877                   ;2/3      
    lda     ram_8E                  ;3        
    sta     ram_94                  ;3        
    sec                             ;2        
    bcs     Lf87e                   ;2/3 =  25
Lf877
    lda     #$02                    ;2        
    sta     ram_94                  ;3        
    sec                             ;2        
    bcs     Lf87e                   ;2/3 =   9
Lf87e
    txa                             ;2        
    sec                             ;2        
    sbc     ram_8C                  ;3        
    adc     #$09                    ;2        
    bcs     Lf88c                   ;2/3      
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    sec                             ;2        
    bcs     Lf891                   ;2/3 =  21
Lf88c
    tay                             ;2        
    lda     (ram_84),y              ;5        
    sta     ram_95                  ;3   =  10
Lf891
    txa                             ;2        
    sec                             ;2        
    sbc     ram_9D                  ;3        
    adc     #$08                    ;2        
    bcs     Lf8a8                   ;2/3      
    txa                             ;2        
    sec                             ;2        
    sbc     ram_9E                  ;3        
    adc     #$08                    ;2        
    bcs     Lf8ae                   ;2/3      
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    sec                             ;2        
    jmp     Lf8b3                   ;3   =  33
    
Lf8a8
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    sec                             ;2        
    bcs     Lf8ae                   ;2/3 =  10
Lf8ae
    tay                             ;2        
    lda     (ram_A4),y              ;5        
    sta     ram_96                  ;3   =  10
Lf8b3
    lda     ram_98                  ;3        
    sta     ENABL                   ;3        
    txa                             ;2        
    and     ram_DF                  ;3        
    tay                             ;2        
    lda.wy  ram_BC,y                ;4        
    sta     COLUPF                  ;3        
    lda     ram_95                  ;3        
    sta     GRP0                    ;3        
    lda     ram_96                  ;3        
    sta     GRP1                    ;3        
    lda     ram_94                  ;3        
    sta     ENAM1                   ;3        
    txa                             ;2        
    lsr                             ;2        
    tay                             ;2        
    lda     Lfe99,y                 ;4        
    tay                             ;2        
    lda     (ram_DA),y              ;5        
    ldy     #BLACK|$0               ;2        
    sty     COLUPF                  ;3        
    dex                             ;2        
    bne     Lf864                   ;2/3      
    sta     WSYNC                   ;3   =  67
;---------------------------------------
    ldx     ram_D5                  ;3        
    beq     Lf918                   ;2/3!     
    lda     #$34                    ;2         *
    sta     CTRLPF                  ;3         *
    lda     #ORANGE|$c              ;2         *
    sta     COLUPF                  ;3         *
    lda     #$55                    ;2         *
    eor     ram_81                  ;3         *
    sta     PF0                     ;3         *
    lda     #$aa                    ;2         *
    eor     ram_81                  ;3         *
    sta     PF1                     ;3         *
    sta     PF2                     ;3         *
    sty     ENABL                   ;3         *
    sty     GRP0                    ;3         *
    sty     GRP1                    ;3   =  43 *
Lf8fe
    sta     WSYNC                   ;3   =   3 *
;---------------------------------------
    lda     #$ff                    ;2         *
    sta     PF0                     ;3         *
    sta     PF1                     ;3         *
    sta     PF2                     ;3         *
    sty     ram_E0                  ;3         *
    iny                             ;2         *
    iny                             ;2         *
    iny                             ;2         *
    lda     #ORANGE|$c              ;2         *
    sta     WSYNC                   ;3   =  25 *
;---------------------------------------
    sbc     ram_E0                  ;3         *
    sta     COLUPF                  ;3         *
    dex                             ;2         *
    bne     Lf8fe                   ;2/3!=  10 *
Lf918
    stx     COLUPF                  ;3        
    stx     PF0                     ;3        
    stx     COLUBK                  ;3        
    stx     PF1                     ;3        
    stx     PF2                     ;3        
    stx     GRP0                    ;3        
    stx     GRP1                    ;3        
    stx     ENAM0                   ;3        
    stx     ENAM1                   ;3        
    sta     WSYNC                   ;3   =  30
;---------------------------------------
    rts                             ;6   =   6
    
Lf92d
    sta     HMCLR                   ;3        
    lda     #$3b                    ;2        
    ldx     #$01                    ;2        
    jsr     Lfdc0                   ;6        
    lda     #$43                    ;2        
    ldx     #$00                    ;2        
    jsr     Lfdc0                   ;6        
    sta     WSYNC                   ;3   =  26
;---------------------------------------
    sta     HMOVE                   ;3        
    stx     ENABL                   ;3        
    stx     GRP0                    ;3        
    stx     GRP1                    ;3        
    stx     ENAM0                   ;3        
    stx     ENAM1                   ;3        
    stx     COLUBK                  ;3        
    stx     CTRLPF                  ;3        
    stx     ram_96                  ;3        
    stx     ram_95                  ;3        
    stx     REFP0                   ;3        
    lda     #GREEN_BEIGE|$e         ;2        
    sta     COLUP0                  ;3        
    sta     COLUP1                  ;3        
    lda     #$03                    ;2        
    sta     NUSIZ0                  ;3        
    sta     NUSIZ1                  ;3        
    lda     #$01                    ;2        
    sta     VDELP0                  ;3        
    sta     VDELP1                  ;3        
    ldy     #$04                    ;2        
    sty     ram_E0                  ;3   =  62
Lf96b
    ldy     ram_E0                  ;3        
    lda     Lff29,y                 ;4        
    sta     GRP1                    ;3        
    sta     WSYNC                   ;3   =  13
;---------------------------------------
    lda     Lff2e,y                 ;4        
    sta     GRP0                    ;3        
    lda     Lff33,y                 ;4        
    sta     GRP1                    ;3        
    lda.wy  ram_A6,y                ;4        
    sta     ram_E1                  ;3        
    lda.wy  ram_AE,y                ;4        
    tax                             ;2        
    lda.wy  ram_B6,y                ;4        
    tay                             ;2        
    lda     ram_E1                  ;3        
    lda     ram_E1                  ;3        
    nop                             ;2        
    sta     GRP0                    ;3        
    stx     GRP1                    ;3        
    sty     GRP0                    ;3        
    sta     GRP1                    ;3        
    dec     ram_E0                  ;5        
    bpl     Lf96b                   ;2/3      
    lda     ram_DD                  ;3        
    sta     COLUP0                  ;3        
    sta     COLUP1                  ;3        
    lda     #$10                    ;2        
    sta     NUSIZ0                  ;3        
    sta     NUSIZ1                  ;3        
    lda     #$00                    ;2        
    sta     VDELP0                  ;3        
    sta     VDELP1                  ;3        
    sta     GRP0                    ;3        
    sta     GRP1                    ;3        
    ldx     #$01                    ;2        
    jmp     Lf9b7                   ;3   =  96
    
Lf9b7
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    dex                             ;2        
    bne     Lf9b7                   ;2/3      
    rts                             ;6   =  10
    
Lf9bd
    ldx     #$5a                    ;2         *
    lda     #$00                    ;2         *
    sta     ENAM0                   ;3         *
    lda     #$02                    ;2         *
    sta     ENAM1                   ;3         *
    sta     ENABL                   ;3         *
    lda     #$11                    ;2         *
    sta     CTRLPF                  ;3   =  20 *
Lf9cd
    txa                             ;2         *
    sec                             ;2         *
    sbc     ram_8C                  ;3         *
    adc     #$09                    ;2         *
    bcs     Lf9db                   ;2/3       *
    nop                             ;2         *
    nop                             ;2         *
    nop                             ;2         *
    sec                             ;2         *
    bcs     Lf9e0                   ;2/3 =  21 *
Lf9db
    tay                             ;2         *
    lda     (ram_84),y              ;5         *
    sta     ram_95                  ;3   =  10 *
Lf9e0
    sta     WSYNC                   ;3   =   3 *
;---------------------------------------
    lda     ram_DD                  ;3         *
    sta     COLUPF                  ;3         *
    lda     ram_95                  ;3         *
    sta     GRP0                    ;3         *
    sta     WSYNC                   ;3   =  15 *
;---------------------------------------
    txa                             ;2         *
    lsr                             ;2         *
    sec                             ;2         *
    sbc     ram_81                  ;3         *
    and     #$0f                    ;2         *
    tay                             ;2         *
    lda     Lfefe,y                 ;4         *
    clc                             ;2         *
    adc     ram_E0                  ;3         *
    sta     COLUBK                  ;3         *
    dex                             ;2         *
    bne     Lf9cd                   ;2/3       *
    sta     WSYNC                   ;3   =  32 *
;---------------------------------------
    rts                             ;6   =   6 *
    
Lfa02
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     #$02                    ;2        
    sta     VBLANK                  ;3        
    ldx     #$23                    ;2        
    stx     TIM64T                  ;4        
    lda     #$30                    ;2        
    sta     PF0                     ;3        
    lda     ram_D0                  ;3        
    beq     Lfa30                   ;2/3      
    cmp     #$01                    ;2        
    bne     Lfa23                   ;2/3      
    lda     #$00                    ;2         *
    sta     ram_D0                  ;3         *
    jsr     Lfbd4                   ;6         *
    jmp     Lfa30                   ;3   =  39 *
    
Lfa23
    cmp     #$02                    ;2        
    bne     Lfa30                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_D0                  ;3        
    sta     CXCLR                   ;3        
    jsr     Lf036                   ;6   =  18
Lfa30
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     INTIM                   ;4        
    bne     Lfa30                   ;2/3      
    sta     WSYNC                   ;3   =   9
;---------------------------------------
    rts                             ;6   =   6
    
Lfa3a
    ldx     ram_9B                  ;3        
    lda     Start,x                 ;4        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    clc                             ;2        
    adc     #$40                    ;2        
    sta     ram_82                  ;3        
    lda     #$01                    ;2        
    bit     ram_D4                  ;3        
    beq     Lfa55                   ;2/3      
    lda     ram_9B                  ;3         *
    lsr                             ;2         *
    lsr                             ;2         *
    asl                             ;2         *
    jmp     Lfa5a                   ;3   =  39 *
    
Lfa55
    lda     ram_9B                  ;3        
    and     #$f8                    ;2        
    lsr                             ;2   =   7
Lfa5a
    cmp     #$1f                    ;2        
    bmi     Lfa60                   ;2/3      
    lda     #$1f                    ;2   =   6 *
Lfa60
    sta     ram_E0                  ;3        
    sec                             ;2        
    lda     #$28                    ;2        
    sbc     ram_E0                  ;3        
    sta     ram_83                  ;3        
    lda     ram_9B                  ;3        
    cmp     ram_D1                  ;3        
    bpl     Lfa72                   ;2/3      
    jmp     Lfafe                   ;3   =  24
    
Lfa72
    and     #$01                    ;2        
    cmp     #$00                    ;2        
    beq     Lfa7b                   ;2/3      
    jmp     Lfafe                   ;3   =   9
    
Lfa7b
    lda     ram_9B                  ;3        
    and     #$06                    ;2        
    cmp     #$02                    ;2        
    beq     Lfad5                   ;2/3      
    lda     ram_9B                  ;3        
    cmp     ram_D3                  ;3        
    bmi     Lfa8d                   ;2/3      
    and     #$06                    ;2         *
    bne     Lfabc                   ;2/3 =  21 *
Lfa8d
    lda     #$fe                    ;2        
    sta     ram_A5                  ;3        
    lda     #$61                    ;2        
    sta     ram_A4                  ;3        
    lda     #$00                    ;2        
    sta     ram_A0                  ;3        
    sta     ram_A1                  ;3        
    sec                             ;2        
    lda     ram_82                  ;3        
    sbc     ram_83                  ;3        
    sec                             ;2        
    sbc     #$08                    ;2        
    sta     ram_9D                  ;3        
    lda     ram_9B                  ;3        
    cmp     ram_D2                  ;3        
    bmi     Lfab5                   ;2/3      
    clc                             ;2        
    lda     ram_82                  ;3        
    adc     #$08                    ;2        
    sta     ram_9E                  ;3        
    jmp     Lfafe                   ;3   =  54
    
Lfab5
    lda     #$d0                    ;2        
    sta     ram_9E                  ;3        
    jmp     Lfafe                   ;3   =   8
    
Lfabc
    lda     #$fe                    ;2         *
    sta     ram_A5                  ;3         *
    lda     #$69                    ;2         *
    sta     ram_A4                  ;3         *
    lda     #$01                    ;2         *
    sta     ram_A0                  ;3         *
    sta     ram_A1                  ;3         *
    lda     #$04                    ;2         *
    sta     ram_9D                  ;3         *
    lda     #$d0                    ;2         *
    sta     ram_9E                  ;3         *
    jmp     Lfafe                   ;3   =  31 *
    
Lfad5
    lda     ram_A2                  ;3        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    sta     ram_E0                  ;3        
    clc                             ;2        
    lda     #$71                    ;2        
    adc     ram_E0                  ;3        
    sta     ram_A4                  ;3        
    lda     #$fe                    ;2        
    adc     #$00                    ;2        
    sta     ram_A5                  ;3        
    lda     #$02                    ;2        
    sta     ram_A0                  ;3        
    lda     #$00                    ;2        
    sta     ram_A1                  ;3        
    sec                             ;2        
    lda     ram_82                  ;3        
    sbc     ram_83                  ;3        
    sbc     #$08                    ;2        
    sta     ram_9D                  ;3        
    lda     #$d0                    ;2        
    sta     ram_9E                  ;3   =  57
Lfafe
    dec     ram_9C                  ;5        
    bpl     Lfb21                   ;2/3      
    lda     #$12                    ;2        
    sta     ram_9C                  ;3        
    clc                             ;2        
    lda     ram_DA                  ;3        
    adc     #$08                    ;2        
    sta     ram_DA                  ;3        
    lda     ram_DB                  ;3        
    adc     #$00                    ;2        
    sta     ram_DB                  ;3        
    lda     ram_9B                  ;3        
    cmp     #$5f                    ;2        
    bmi     Lfb21                   ;2/3      
    lda     #$00                    ;2         *
    sta     ram_DB                  ;3         *
    lda     #$bc                    ;2         *
    sta     ram_DA                  ;3   =  47 *
Lfb21
    rts                             ;6   =   6
    
Lfb22
    lda     ram_9B                  ;3        
    jsr     Lfb78                   ;6        
    lda     #$00                    ;2        
    sta     ram_E0                  ;3        
    sta     ram_E1                  ;3        
    ldx     ram_A2                  ;3        
    beq     Lfb53                   ;2/3      
    ora     #$0c                    ;2        
    sta     ram_E0                  ;3        
    dex                             ;2        
    beq     Lfb53                   ;2/3      
    ora     #$0f                    ;2        
    sta     ram_E0                  ;3        
    dex                             ;2        
    beq     Lfb53                   ;2/3      
    lda     ram_E1                  ;3        
    ora     #$c0                    ;2        
    sta     ram_E1                  ;3        
    dex                             ;2        
    beq     Lfb53                   ;2/3      
    ora     #$f0                    ;2         *
    sta     ram_E1                  ;3         *
    dex                             ;2         *
    beq     Lfb53                   ;2/3       *
    ora     #$fc                    ;2         *
    sta     ram_E1                  ;3   =  66 *
Lfb53
    lda     #$1f                    ;2        
    sta     ram_AE                  ;3        
    lda     #$10                    ;2        
    ora     ram_E0                  ;3        
    sta     ram_AF                  ;3        
    sta     ram_B0                  ;3        
    sta     ram_B1                  ;3        
    lda     #$0f                    ;2        
    sta     ram_B2                  ;3        
    lda     #$fc                    ;2        
    sta     ram_B6                  ;3        
    lda     #$02                    ;2        
    ora     ram_E1                  ;3        
    sta     ram_B7                  ;3        
    sta     ram_B8                  ;3        
    sta     ram_B9                  ;3        
    lda     #$fe                    ;2        
    sta     ram_BA                  ;3        
    rts                             ;6   =  54
    
Lfb78
    cmp     #$63                    ;2        
    bmi     Lfb7e                   ;2/3      
    lda     #$63                    ;2   =   6 *
Lfb7e
    ldx     #$00                    ;2   =   2
Lfb80
    cmp     #$0a                    ;2        
    bmi     Lfb8b                   ;2/3      
    inx                             ;2        
    sec                             ;2        
    sbc     #$0a                    ;2        
    jmp     Lfb80                   ;3   =  13
    
Lfb8b
    clc                             ;2        
    sta     ram_E2                  ;3        
    stx     ram_E3                  ;3        
    ldx     #$00                    ;2   =  10
Lfb92
    lda     ram_E3                  ;3        
    jsr     Lfbaf                   ;6        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    pha                             ;3        
    lda     ram_E2                  ;3        
    jsr     Lfbaf                   ;6        
    sta     ram_E0                  ;3        
    pla                             ;4        
    ora     ram_E0                  ;3        
    sta     ram_A6,x                ;4        
    inx                             ;2        
    txa                             ;2        
    cmp     #$05                    ;2        
    bne     Lfb92                   ;2/3      
    rts                             ;6   =  57
    
Lfbaf
    sta     ram_E1                  ;3        
    lsr                             ;2        
    sta     ram_E0                  ;3        
    asl                             ;2        
    asl                             ;2        
    adc     ram_E0                  ;3        
    sta     ram_E0                  ;3        
    txa                             ;2        
    adc     ram_E0                  ;3        
    tay                             ;2        
    lda     ram_E1                  ;3        
    and     #$01                    ;2        
    bne     Lfbce                   ;2/3      
    lda     Lff38,y                 ;4        
    and     #$f0                    ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    rts                             ;6   =  52
    
Lfbce
    lda     Lff38,y                 ;4        
    and     #$0f                    ;2        
    rts                             ;6   =  12
    
Lfbd4
    lda     #$03                    ;2        
    sta     NUSIZ0                  ;3        
    sta     NUSIZ1                  ;3        
    lda     #$01                    ;2        
    sta     VDELP0                  ;3        
    sta     VDELP1                  ;3        
    sta     ram_C4                  ;3        
    lda     #$00                    ;2        
    sta     ram_CC                  ;3        
    sta     ram_80                  ;3        
    sta     ram_81                  ;3        
    sta     ram_DC                  ;3        
    sta     ram_DD                  ;3        
    lda     #$0e                    ;2        
    sta     AUDC0                   ;3        
    ldx     ram_D4                  ;3        
    lda     ram_D6,x                ;4        
    jmp     Lfb78                   ;3   =  51
    
Lfbf9
    lda     #BLACK|$0               ;2        
    sta     COLUBK                  ;3        
    ldx     #$32                    ;2        
    jsr     Lf9b7                   ;6        
    lda     #BLACK|$e               ;2        
    sta     COLUP0                  ;3        
    sta     COLUP1                  ;3        
    sta     WSYNC                   ;3   =  24
;---------------------------------------
    lda     #$03                    ;2        
    sta     ram_E0                  ;3   =   5
Lfc0e
    ldy     ram_E0                  ;3        
    lda     Lff51,y                 ;4        
    sta     GRP1                    ;3        
    sta     WSYNC                   ;3   =  13
;---------------------------------------
    lda     Lff60,y                 ;4        
    sta     GRP0                    ;3        
    lda     Lff6f,y                 ;4        
    sta     GRP1                    ;3        
    lda     Lff7e,y                 ;4        
    sta     ram_E1                  ;3        
    lda     Lff8d,y                 ;4        
    tax                             ;2        
    lda     Lff91,y                 ;4        
    tay                             ;2        
    lda     ram_E1                  ;3        
    lda     ram_E1                  ;3        
    nop                             ;2        
    sta     GRP0                    ;3        
    stx     GRP1                    ;3        
    sty     GRP0                    ;3        
    sta     GRP1                    ;3        
    dec     ram_E0                  ;5        
    bpl     Lfc0e                   ;2/3      
    sta     WSYNC                   ;3   =  63
;---------------------------------------
    lda     #$21                    ;2        
    sta     CTRLPF                  ;3        
    lda     #BLACK|$0               ;2        
    sta     COLUP0                  ;3        
    sta     COLUP1                  ;3        
    sta     COLUPF                  ;3        
    sta     WSYNC                   ;3   =  19
;---------------------------------------
    lda     #$0a                    ;2        
    sta     ram_E0                  ;3        
    lda     #$f0                    ;2        
    sta     PF2                     ;3   =  10
Lfc57
    sta     GRP1                    ;3        
    lda     ram_E0                  ;3        
    tay                             ;2        
    lsr                             ;2        
    sta     WSYNC                   ;3   =  13
;---------------------------------------
    tax                             ;2        
    lda     Lfed7,x                 ;4        
    sta     COLUPF                  ;3        
    lda     Lff55,y                 ;4        
    sta     GRP0                    ;3        
    lda     Lff64,y                 ;4        
    sta     GRP1                    ;3        
    lda     Lff73,y                 ;4        
    sta     ram_E1                  ;3        
    LAX     Lff82,y                 ;4        
    lda     #$00                    ;2        
    ldy     ram_E1                  ;3        
    nop                             ;2        
    sty     GRP0                    ;3        
    stx     GRP1                    ;3        
    sta     GRP0                    ;3        
    sta     GRP1                    ;3        
    sta     GRP1                    ;3        
    lda     ram_E0                  ;3        
    tay                             ;2        
    lsr                             ;2        
    sta     WSYNC                   ;3   =  66
;---------------------------------------
    tax                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    lda     Lff55,y                 ;4        
    sta     GRP0                    ;3        
    lda     Lff64,y                 ;4        
    sta     GRP1                    ;3        
    lda     Lff73,y                 ;4        
    sta     ram_E1                  ;3        
    LAX     Lff82,y                 ;4        
    lda     #$00                    ;2        
    ldy     ram_E1                  ;3        
    nop                             ;2        
    sty     GRP0                    ;3        
    stx     GRP1                    ;3        
    sta     GRP0                    ;3        
    sta     GRP1                    ;3        
    dec     ram_E0                  ;5        
    bpl     Lfc57                   ;2/3      
    lda     #BLACK|$e               ;2        
    sta     COLUP0                  ;3        
    sta     COLUP1                  ;3        
    sta     WSYNC                   ;3   =  70
;---------------------------------------
    ldx     #$10                    ;2        
    lda     #$00                    ;2        
    sta     GRP0                    ;3        
    sta     COLUPF                  ;3        
    jsr     Lf9b7                   ;6        
    lda     #$04                    ;2        
    sta     ram_E0                  ;3        
    lda     #RED|$2                 ;2        
    sta     COLUP0                  ;3        
    sta     COLUP1                  ;3   =  29
Lfcd0
    ldy     ram_E0                  ;3        
    lda     Lff95,y                 ;4        
    sta     GRP1                    ;3        
    sta     WSYNC                   ;3   =  13
;---------------------------------------
    lda     Lff9a,y                 ;4        
    sta     GRP0                    ;3        
    lda     Lff9f,y                 ;4        
    sta     GRP1                    ;3        
    lda     Lffa4,y                 ;4        
    sta     ram_E1                  ;3        
    lda     Lffa9,y                 ;4        
    tax                             ;2        
    lda     Lffad,y                 ;4        
    tay                             ;2        
    lda     ram_E1                  ;3        
    lda     ram_E1                  ;3        
    nop                             ;2        
    sta     GRP0                    ;3        
    stx     GRP1                    ;3        
    sty     GRP0                    ;3        
    sta     GRP1                    ;3        
    dec     ram_E0                  ;5        
    bpl     Lfcd0                   ;2/3!     
    ldx     #$0c                    ;2        
    lda     #$00                    ;2        
    sta     GRP0                    ;3        
    sta     GRP1                    ;3        
    sta     GRP0                    ;3        
    sta     COLUPF                  ;3        
    jsr     Lf9b7                   ;6        
    lda     #$0c                    ;2        
    sta     ram_E0                  ;3        
    lda     #$f0                    ;2        
    sta     PF2                     ;3        
    lda     #BLACK|$0               ;2        
    sta     COLUP0                  ;3        
    sta     COLUP1                  ;3   = 100
Lfd1e
    ldy     ram_E0                  ;3        
    lda     Lfe3d                   ;4        
    sta     GRP1                    ;3        
    sta     WSYNC                   ;3   =  13
;---------------------------------------
    lda.wy  ram_BC,y                ;4        
    sta     COLUPF                  ;3        
    lda     Lffb2,y                 ;4        
    sta     GRP0                    ;3        
    lda.wy  ram_AE,y                ;4        
    sta     GRP1                    ;3        
    lda     Lffbf,y                 ;4        
    sta     ram_E1                  ;3        
    LAX     Lffd8,y                 ;4        
    lda     #$00                    ;2        
    tay                             ;2        
    nop                             ;2        
    lda     ram_E1                  ;3        
    sta     GRP0                    ;3        
    stx     GRP1                    ;3        
    sty     GRP0                    ;3        
    sta     GRP1                    ;3        
    dec     ram_E0                  ;5        
    bpl     Lfd1e                   ;2/3      
    ldx     #$14                    ;2        
    lda     #$00                    ;2        
    sta     GRP0                    ;3        
    sta     GRP1                    ;3        
    sta     GRP0                    ;3        
    sta     PF2                     ;3        
    jsr     Lf9b7                   ;6        
    lda     #$04                    ;2        
    sta     ram_E0                  ;3        
    lda     #BLACK|$6               ;2        
    sta     COLUP0                  ;3        
    sta     COLUP1                  ;3   =  95
Lfd69
    ldy     ram_E0                  ;3        
    lda     Lfec7,y                 ;4        
    sta     GRP1                    ;3        
    sta     WSYNC                   ;3   =  13
;---------------------------------------
    lda     Lfecc,y                 ;4        
    sta     GRP0                    ;3        
    lda     Lfed1,y                 ;4        
    sta     GRP1                    ;3        
    lda.wy  ram_A6,y                ;4        
    sta     ram_E1                  ;3        
    lda     #$00                    ;2        
    tax                             ;2        
    lda     #$00                    ;2        
    tay                             ;2        
    lda     ram_E1                  ;3        
    lda     ram_E1                  ;3        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    sta     GRP0                    ;3        
    stx     GRP1                    ;3        
    sty     GRP0                    ;3        
    sta     GRP1                    ;3        
    dec     ram_E0                  ;5        
    bpl     Lfd69                   ;2/3      
    ldx     #$27                    ;2        
    lda     #BLACK|$0               ;2        
    sta     COLUP0                  ;3        
    sta     COLUP1                  ;3        
    jmp     Lf9b7                   ;3   =  73
    
Lfda5
    inc     ram_80                  ;5        
    lda     ram_80                  ;3        
    and     #$03                    ;2        
    bne     Lfdaf                   ;2/3      
    inc     ram_81                  ;5   =  17
Lfdaf
    rts                             ;6   =   6
    
Lfdb0
    ldx     ram_8B                  ;3        
    and     Lff0e,x                 ;4   =   7
Lfdb5
    dex                             ;2        
    bmi     Lfdbd                   ;2/3      
    lsr                             ;2        
    lsr                             ;2        
    jmp     Lfdb5                   ;3   =  11
    
Lfdbd
    sta     ram_E0                  ;3        
    rts                             ;6   =   9
    
Lfdc0
    sec                             ;2        
    sta     WSYNC                   ;3   =   5
;---------------------------------------
Lfdc3
    sbc     #$0f                    ;2        
    bcs     Lfdc3                   ;2/3      
    eor     #$07                    ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    sta     HMP0,x                  ;4        
    sta     RESP0,x                 ;4        
    rts                             ;6   =  28

  IF PLUSROM
ReloadHSCTable
    sta     WSYNC                   ;3   =   3
    lda     INTIM                   ;4        
    bne     ReloadHSCTable          ;2/3      

    jsr     LoadHSCTable
    lda     #$01
    sta     ram_D0
    jmp     Lf1b6

LoadHSCTable
    lda     #0                      ;3        
    sta     WriteToBuffer           ; BCD score
    lda     #HIGHSCORE_ID           ; game id in Highscore DB
    sta     WriteSendBuffer

    lda     #BLUE|$8
    sta     COLUBK                  ; Background to PlusCart blue
    ldy     #0

WaitForResponseScreen
    jsr     Lf0c8_Vertical_Sync     ;6        
    lda #$02
    sta VBLANK           ; VBLANK an
    ldx #$43             ; 67 Zeilen (27 Overscan oben + 40 VBLANK)
TopLoop
    sta WSYNC
    dex
    bne TopLoop
    lda #$00
    sta VBLANK           ; VBLANK aus

    ldx #$E8             ; 232 Zeilen (192 visible + 40 Unter-Overscan)
BotLoop
    sta WSYNC            ; hier bleibt COLUBK=0 → schwarz
    dex
    bne BotLoop


    lda     ReceiveBufferSize
    cmp     #4
    beq     ReadPlusROMResponse
    dey                      ; — Frame-Counter–1
    bne WaitForResponseScreen                ; — wenn ≠0, neuer Frame
    rts

ReadPlusROMResponse
    lda     ReceiveBuffer
    sta     ram_D6
    lda     ReceiveBuffer
    sta     ram_D6+1
    lda     ReceiveBuffer
    sta     ram_D6+2
    lda     ReceiveBuffer
    sta     ram_D6+3
    rts

  ELSE
; SaveKey ??
Lfdd2
    ldy     #$a1                    ;2        
    .byte   $2c ;bit                ;4-2 =   4
Lfdd5
    ldy     #$a0                    ;2        
    lda     #$18                    ;2        
    sta     SWCHA                   ;4        
    lsr                             ;2        
    sta     SWACNT                  ;4        
    tya                             ;2   =  16
Lfde1
    eor     #$ff                    ;2        
    sec                             ;2        
    rol                             ;2   =   6
Lfde5
    tay                             ;2        
    lda     #$03                    ;2        
    sta     SWCHA                   ;4        
    adc     #$08                    ;2        
    sta     SWACNT                  ;4        
    lda     #$08                    ;2        
    sta     SWCHA                   ;4        
    tya                             ;2        
    asl                             ;2        
    bne     Lfde5                   ;2/3      
    beq     Lfe06                   ;2/3!=  28
Lfdfb
    bvc     Lfe00                   ;2/3!     
    jsr     Lfe2e                   ;6   =   8
Lfe00
    bit     Lfdfb                   ;4        
    lda     #$01                    ;2   =   6
Lfe05
    tay                             ;2   =   2
Lfe06
    lda     #$10                    ;2        
    sta     SWCHA                   ;4        
    lsr                             ;2        
    sta     SWACNT                  ;4        
    nop                             ;2        
    sta     SWCHA                   ;4        
    lda     SWCHA                   ;4        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    tya                             ;2        
    rol                             ;2        
    bcc     Lfe05                   ;2/3      
    rts                             ;6   =  40
    
Lfe1e
    bvc     Lfe25                   ;2/3      
    ldy     #$80                    ;2        
    jsr     Lfe06                   ;6   =  10
Lfe25
    jsr     Lfe2e                   ;6        
    lda     #$00                    ;2        
    sta     SWACNT                  ;4        
    rts                             ;6   =  18
    
Lfe2e
    lda     #$00                    ;2        
    sta     SWCHA                   ;4        
    lda     #$0c                    ;2        
    sta     SWACNT                  ;4        
    asl                             ;2        
    sta     SWCHA                   ;4        
    rts                             ;6   =  24
  ENDIF

   org $fe3d   
Lfe3d
    .byte   %00000000 ; |        |            $fe3d (G)
    
    .byte   $88,$5c,$34,$04,$3c,$c8,$98,$98 ; $fe3e (D)
    .byte   $00,$88,$44,$38,$10,$9c,$72,$18 ; $fe46 (D)
    .byte   $18,$00,$84,$44,$38,$10,$1c,$72 ; $fe4e (D)
    .byte   $98,$18                         ; $fe56 (D)
    .byte   $00,$30,$08,$c8,$3a,$0d,$04,$3b ; $fe58 (*)
    .byte   $03                             ; $fe60 (*)
    .byte   $00,$60,$7e,$60,$00,$60,$7e,$60 ; $fe61 (D)
    .byte   $00,$10,$10,$38,$7c,$38,$10,$10 ; $fe69 (*)
    .byte   $00,$fe,$d3,$d3,$cb,$cb,$db,$7f ; $fe71 (D)
    .byte   $00,$fc,$c6,$ee,$ee,$ee,$c6,$7e ; $fe79 (D)
    .byte   $00,$fe,$d3,$d3,$cb,$cb,$db,$7f ; $fe81 (D)
    .byte   $00,$fe,$cf,$d7,$f7,$f7,$e3,$7f ; $fe89 (*)
    .byte   $00,$fe,$db,$db,$c3,$db,$e7,$7f ; $fe91 (*)
Lfe99
    .byte   $00                             ; $fe99 (A)
    .byte   $00                             ; $fe9a (A)
    .byte   $00                             ; $fe9b (A)
    .byte   $00                             ; $fe9c (A)
    .byte   $00                             ; $fe9d (A)
    .byte   $01                             ; $fe9e (A)
    .byte   $00                             ; $fe9f (A)
    .byte   $00                             ; $fea0 (A)
    .byte   $01                             ; $fea1 (A)
    .byte   $00                             ; $fea2 (A)
    .byte   $01                             ; $fea3 (A)
    .byte   $01                             ; $fea4 (A)
    .byte   $02                             ; $fea5 (A)
    .byte   $03                             ; $fea6 (A)
    .byte   $03                             ; $fea7 (A)
    .byte   $03                             ; $fea8 (A)
    .byte   $04                             ; $fea9 (A)
    .byte   $03                             ; $feaa (A)
    .byte   $04                             ; $feab (A)
    .byte   $03                             ; $feac (A)
    .byte   $05                             ; $fead (A)
    .byte   $04                             ; $feae (A)
    .byte   $05                             ; $feaf (A)
    .byte   $05                             ; $feb0 (A)
    .byte   $04                             ; $feb1 (A)
    .byte   $05,$05,$06,$05,$06,$06,$06,$06 ; $feb2 (D)
    .byte   $06,$07,$06,$06,$07,$06,$07,$07 ; $feba (D)
    .byte   $07,$07,$07,$07,$07             ; $fec2 (D)
    
Lfec7
    .byte   %00001100 ; |    ##  |            $fec7 (G)
    .byte   %00001010 ; |    # # |            $fec8 (G)
    .byte   %00001100 ; |    ##  |            $fec9 (G)
    .byte   %00001010 ; |    # # |            $feca (G)
    .byte   %00000110 ; |     ## |            $fecb (G)
Lfecc
    .byte   %11101100 ; |### ##  |            $fecc (G)
    .byte   %10000010 ; |#     # |            $fecd (G)
    .byte   %11001110 ; |##  ### |            $fece (G)
    .byte   %10001000 ; |#   #   |            $fecf (G)
    .byte   %01100110 ; | ##  ## |            $fed0 (G)
Lfed1
    .byte   %01000000 ; | #      |            $fed1 (G)
    .byte   %01000100 ; | #   #  |            $fed2 (G)
    .byte   %01000000 ; | #      |            $fed3 (G)
    .byte   %01000100 ; | #   #  |            $fed4 (G)
    .byte   %11100000 ; |###     |            $fed5 (G)
    
    .byte   RED|$6                          ; $fed6 (CB)
Lfed7
    .byte   RED|$4                          ; $fed7 (CP)
    .byte   BROWN|$6                        ; $fed8 (CP)
    .byte   ORANGE|$6                       ; $fed9 (CP)
    .byte   ORANGE|$8                       ; $feda (CP)
    .byte   ORANGE|$a                       ; $fedb (CP)
    .byte   ORANGE|$c                       ; $fedc (CP)
    .byte   BROWN|$e                        ; $fedd (CB)
    .byte   GREEN|$6                        ; $fede (CB)
    .byte   GREEN|$4                        ; $fedf (CB)
    .byte   GREEN_YELLOW|$2                 ; $fee0 (CB)
    .byte   CYAN|$4                         ; $fee1 (CB)
    .byte   CYAN|$6                         ; $fee2 (CB)
    .byte   BLUE_CYAN|$8                    ; $fee3 (CB)
    .byte   BLUE_CYAN|$a                    ; $fee4 (CB)
    .byte   BLUE_CYAN|$c                    ; $fee5 (CB)
    
    .byte   CYAN_GREEN|$4
    .byte   CYAN_GREEN|$2
    .byte   GREEN_BEIGE|$4
    .byte   MAUVE|$6
    .byte   MAUVE|$8
    .byte   MAUVE|$a
    .byte   MAUVE|$c
    .byte   MAUVE|$e ; $fee6 (*)
    .byte   RED|$6
    .byte   RED|$4
    .byte   VIOLET|$2
    .byte   VIOLET|$6
    .byte   VIOLET|$8
    .byte   VIOLET|$a
    .byte   VIOLET|$c
    .byte   VIOLET|$e ; $feee (*)
    .byte   CYAN|$6
    .byte   CYAN|$4
    .byte   CYAN|$2
    .byte   BLACK|$4
    .byte   BLACK|$6
    .byte   BLACK|$8
    .byte   BLACK|$a
    .byte   BLACK|$c ; $fef6 (*)
Lfefe
    .byte   CYAN|$e
    .byte   CYAN|$e
    .byte   CYAN|$f
    .byte   BLACK|$f
    .byte   BLACK|$e
    .byte   BLACK|$f
    .byte   CYAN|$f
    .byte   CYAN|$e ; $fefe (*)
    .byte   CYAN|$e
    .byte   CYAN|$e
    .byte   CYAN|$f
    .byte   BLACK|$f
    .byte   BLACK|$e
    .byte   BLACK|$f
    .byte   CYAN|$f
    .byte   CYAN|$e ; $ff06 (*)
Lff0e
    .byte   $03,$0c,$30,$c0                 ; $ff0e (D)
Lff12
    .byte   $aa,$54,$44,$40,$40,$00,$01,$11 ; $ff12 (D)
    .byte   $15,$55,$55                     ; $ff1a (D)
Lff1d
    .byte   $40,$44,$55,$55,$99,$99,$99,$55 ; $ff1d (*)
    .byte   $55,$54,$40,$40                 ; $ff25 (*)
    
Lff29
    .byte   %10010111 ; |#  # ###|            $ff29 (G)
    .byte   %10010100 ; |#  # #  |            $ff2a (G)
    .byte   %11100100 ; |###  #  |            $ff2b (G)
    .byte   %10010100 ; |#  # #  |            $ff2c (G)
    .byte   %01110011 ; | ###  ##|            $ff2d (G)
Lff2e
    .byte   %00111001 ; |  ###  #|            $ff2e (G)
    .byte   %10100101 ; |# #  # #|            $ff2f (G)
    .byte   %10100101 ; |# #  # #|            $ff30 (G)
    .byte   %10100101 ; |# #  # #|            $ff31 (G)
    .byte   %10011101 ; |#  ### #|            $ff32 (G)
Lff33
    .byte   %00010000 ; |   #    |            $ff33 (G)
    .byte   %00010100 ; |   # #  |            $ff34 (G)
    .byte   %01010000 ; | # #    |            $ff35 (G)
    .byte   %10110100 ; |# ## #  |            $ff36 (G)
    .byte   %00010000 ; |   #    |            $ff37 (G)
    
Lff38
    .byte   $ce,$a4,$a4,$ac,$64,$ee,$82,$6e ; $ff38 (D)
    .byte   $22,$ec,$2c,$22,$ee,$a8,$ae,$c8 ; $ff40 (D)
    .byte   $a8,$e4,$82,$6e,$c2,$a2,$ee,$aa ; $ff48 (D)
    .byte   $66                             ; $ff50 (D)
    
Lff51
    .byte   %00001111 ; |    ####|            $ff51 (G)
    .byte   %00001010 ; |    # # |            $ff52 (G)
    .byte   %00001000 ; |    #   |            $ff53 (G)
    .byte   %00001000 ; |    #   |            $ff54 (G)
Lff55
    .byte   %11111111 ; |########|            $ff55 (G)
    .byte   %10011100 ; |#  ###  |            $ff56 (G)
    .byte   %00001000 ; |    #   |            $ff57 (G)
    .byte   %01000010 ; | #    # |            $ff58 (G)
    .byte   %01000110 ; | #   ## |            $ff59 (G)
    .byte   %01001010 ; | #  # # |            $ff5a (G)
    .byte   %00101001 ; |  # #  #|            $ff5b (G)
    .byte   %10110001 ; |# ##   #|            $ff5c (G)
    .byte   %10110101 ; |# ## # #|            $ff5d (G)
    .byte   %10100101 ; |# #  # #|            $ff5e (G)
    .byte   %10001100 ; |#   ##  |            $ff5f (G)
Lff60
    .byte   %00100101 ; |  #  # #|            $ff60 (G)
    .byte   %10101101 ; |# # ## #|            $ff61 (G)
    .byte   %10100101 ; |# #  # #|            $ff62 (G)
    .byte   %10011101 ; |#  ### #|            $ff63 (G)
Lff64
    .byte   %11111111 ; |########|            $ff64 (G)
    .byte   %00000011 ; |      ##|            $ff65 (G)
    .byte   %00000001 ; |       #|            $ff66 (G)
    .byte   %01001000 ; | #  #   |            $ff67 (G)
    .byte   %01001000 ; | #  #   |            $ff68 (G)
    .byte   %01001001 ; | #  #  #|            $ff69 (G)
    .byte   %01000101 ; | #   # #|            $ff6a (G)
    .byte   %00100110 ; |  #  ## |            $ff6b (G)
    .byte   %00100110 ; |  #  ## |            $ff6c (G)
    .byte   %00100100 ; |  #  #  |            $ff6d (G)
    .byte   %00000001 ; |       #|            $ff6e (G)
Lff6f
    .byte   %11011100 ; |## ###  |            $ff6f (G)
    .byte   %00010000 ; |   #    |            $ff70 (G)
    .byte   %00010000 ; |   #    |            $ff71 (G)
    .byte   %00010000 ; |   #    |            $ff72 (G)
    
Lff73
    .byte   $ff,$80,$00,$4e,$d2,$42,$21,$21 ; $ff73 (D)
    .byte   $a1,$a7,$80                     ; $ff7b (D)
Lff7e
    .byte   $e7,$94,$14,$34                 ; $ff7e (D)
    
Lff82
    .byte   %11111111 ; |########|            $ff82 (G)
    .byte   %00110011 ; |  ##  ##|            $ff83 (G)
    .byte   %00000001 ; |       #|            $ff84 (G)
    .byte   %10001001 ; |#   #  #|            $ff85 (G)
    .byte   %10001001 ; |#   #  #|            $ff86 (G)
    .byte   %01111001 ; | ####  #|            $ff87 (G)
    .byte   %01001001 ; | #  #  #|            $ff88 (G)
    .byte   %01001001 ; | #  #  #|            $ff89 (G)
    .byte   %00101001 ; |  # #  #|            $ff8a (G)
    .byte   %00010011 ; |   #  ##|            $ff8b (G)
    .byte   %01000111 ; | #   ###|            $ff8c (G)
Lff8d
    .byte   %00100010 ; |  #   # |            $ff8d (G)
    .byte   %10101010 ; |# # # # |            $ff8e (G)
    .byte   %10110110 ; |# ## ## |            $ff8f (G)
    .byte   %10100010 ; |# #   # |            $ff90 (G)
Lff91
    .byte   %10000000 ; |#       |            $ff91 (G)
    .byte   %11110000 ; |####    |            $ff92 (G)
    .byte   %10010000 ; |#  #    |            $ff93 (G)
    .byte   %11110000 ; |####    |            $ff94 (G)
Lff95
    .byte   %00010100 ; |   # #  |            $ff95 (G)
    .byte   %00101010 ; |  # # # |            $ff96 (G)
    .byte   %00100010 ; |  #   # |            $ff97 (G)
    .byte   %00100010 ; |  #   # |            $ff98 (G)
    .byte   %00100010 ; |  #   # |            $ff99 (G)
Lff9a
    .byte   %11101011 ; |### # ##|            $ff9a (G)
    .byte   %10101010 ; |# # # # |            $ff9b (G)
    .byte   %11101011 ; |### # ##|            $ff9c (G)
    .byte   %00101000 ; |  # #   |            $ff9d (G)
    .byte   %11001011 ; |##  # ##|            $ff9e (G)
Lff9f
    .byte   %10111001 ; |# ###  #|            $ff9f (G)
    .byte   %10101010 ; |# # # # |            $ffa0 (G)
    .byte   %10111011 ; |# ### ##|            $ffa1 (G)
    .byte   %10100010 ; |# #   # |            $ffa2 (G)
    .byte   %00100001 ; |  #    #|            $ffa3 (G)
    
Lffa4
    .byte   $a0,$20,$a0,$b4,$a8             ; $ffa4 (D)
    
Lffa9
    .byte   %11100100 ; |###  #  |            $ffa9 (G)
    .byte   %10001010 ; |#   # # |            $ffaa (G)
    .byte   %01001010 ; | #  # # |            $ffab (G)
    .byte   %10101010 ; |# # # # |            $ffac (G)
Lffad
    .byte   %01000100 ; | #   #  |            $ffad (G)
    .byte   %01000100 ; | #   #  |            $ffae (G)
    .byte   %01011100 ; | # ###  |            $ffaf (G)
    .byte   %11010100 ; |## # #  |            $ffb0 (G)
    .byte   %01010100 ; | # # #  |            $ffb1 (G)
Lffb2
    .byte   %10000000 ; |#       |            $ffb2 (G)
    .byte   %10010100 ; |#  # #  |            $ffb3 (G)
    .byte   %10011000 ; |#  ##   |            $ffb4 (G)
    .byte   %10010100 ; |#  # #  |            $ffb5 (G)
    .byte   %10011000 ; |#  ##   |            $ffb6 (G)
    .byte   %11000000 ; |##      |            $ffb7 (G)
    .byte   %11111111 ; |########|            $ffb8 (G)
    .byte   %10000000 ; |#       |            $ffb9 (G)
    .byte   %10011100 ; |#  ###  |            $ffba (G)
    .byte   %10010000 ; |#  #    |            $ffbb (G)
    .byte   %10010000 ; |#  #    |            $ffbc (G)
    .byte   %10010000 ; |#  #    |            $ffbd (G)
    .byte   %11000000 ; |##      |            $ffbe (G)
    
Lffbf
    .byte   $8b,$ab,$c9,$ea,$df,$ff,$ff,$8d ; $ffbf (D)
    .byte   $aa,$ca,$ef,$df,$ff             ; $ffc7 (D)
Lffcc
    .byte   $1a,$ea,$e8,$ea,$ea,$0f         ; $ffcc (D)
    .byte   $ff                             ; $ffd2 (*)
    .byte   $18,$eb,$eb,$eb,$eb             ; $ffd3 (D)
    
Lffd8
    .byte   %00011111 ; |   #####|            $ffd8 (G)
    .byte   %01011111 ; | # #####|            $ffd9 (G)
    .byte   %10011111 ; |#  #####|            $ffda (G)
    .byte   %11011111 ; |## #####|            $ffdb (G)
    .byte   %11011111 ; |## #####|            $ffdc (G)
    .byte   %11111111 ; |########|            $ffdd (G)
    .byte   %11111111 ; |########|            $ffde (G)
    .byte   %10001111 ; |#   ####|            $ffdf (G)
    .byte   %10101111 ; |# # ####|            $ffe0 (G)
    .byte   %11001111 ; |##  ####|            $ffe1 (G)
    .byte   %11101111 ; |### ####|            $ffe2 (G)
    .byte   %11011111 ; |## #####|            $ffe3 (G)
    .byte   %11111111 ; |########|            $ffe4 (G)
    
Lffe5
;  IF PAL60
;    .byte   $A4,$A4,$86,$88,$9A,$9E,$8A,$A8
;  ELSE
    .byte   VIOLET|$4
    .byte   VIOLET|$4
    .byte   MAUVE|$6
    .byte   MAUVE|$8
    .byte   CYAN|$a
    .byte   CYAN|$e
    .byte   MAUVE|$a
    .byte   VIOLET|$8 ; $ffe5 (D)
;  ENDIF

  IF PLUSROM

    org   $fffa
    .word (PlusROM_API - $E000)      ; PlusRom API pointer
    

  ELSE
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $ffed (*)
    .byte   $00,$00,$00,$00,$00             ; $fff5 (*)
    .word   $0000                           ; $fffa
  ENDIF

    .word   Start                           ; $fffc
    .word   Start                           ; $fffe
