; Disassembly of Public ROMs\Classic Roms\NTSC\BY ALPHABET\H-R\Mission 3,000 A.D..bin
; Disassembled 07/05/24 23:49:10
; Using Stella 6.6
;
; ROM properties name : Mission 3,000 A.D. (1983) (CCE)
; ROM properties MD5  : 4c6afb8a44adf8e28f49164c84144bfe
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

PLUSROM = 1
PAL60 = 0                       ; NTSC timing with the colors of the PAL release (Bit Corporation)

  IF PAL60
COL_SCORE             = $6a
COL_PF_GRADIENT       = $1c
COL_PF_GRADIENT_STEP  = $06
COL_GROUND_PF         = $49
COL_P0_HIT            = $82
COL_PF_BOTTOM         = $88
COL_GROUND_A          = $58
COL_GROUND_B          = $b6
COL_GROUND_C          = $68
  ELSE
COL_SCORE             = $1a
COL_PF_GRADIENT       = $85
COL_PF_GRADIENT_STEP  = $10
COL_GROUND_PF         = $e8
COL_P0_HIT            = $42
COL_PF_BOTTOM         = $78
COL_GROUND_A          = $a8
COL_GROUND_B          = $76
COL_GROUND_C          = $58
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
PF0             = $0d  ; (W)
PF1             = $0e  ; (W)
PF2             = $0f  ; (W)
RESP0           = $10  ; (W)
RESP1           = $11  ; (W)
RESM0           = $12  ; (W)
RESM1           = $13  ; (W)
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
HMP1            = $21  ; (W)
HMM0            = $22  ; (W)
HMM1            = $23  ; (W)
HMBL            = $24  ; (W)
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

lives           = $80
ram_81          = $81
ram_82          = $82
ram_83          = $83
ram_84          = $84
ram_85          = $85
ram_86          = $86
ram_87          = $87

ram_89          = $89
ram_8A          = $8a
ram_8B          = $8b
ram_8C          = $8c
ram_8D          = $8d
ram_8E          = $8e
ram_8F          = $8f
ram_90          = $90
ram_91          = $91
;                 $92  (i)
;                 $93  (i)
ram_94          = $94
ram_95          = $95
;                 $96  (i)
;                 $97  (i)
ram_98          = $98
ram_99          = $99
;                 $9a  (i)
;                 $9b  (i)
ram_9C          = $9c
ram_9D          = $9d
ram_9E          = $9e
ram_9F          = $9f
ram_A0          = $a0
ram_A1          = $a1
;                 $a2  (i)
ram_A3          = $a3
;                 $a4  (i)
ram_A5          = $a5
ram_A6          = $a6
ram_A7          = $a7
ram_A8          = $a8
ram_A9          = $a9
ram_AA          = $aa

ram_AC          = $ac

ram_AE          = $ae
ram_AF          = $af
ram_B0          = $b0
ram_B1          = $b1
ram_B2          = $b2
;                 $b3  (i)
ram_B4          = $b4
;                 $b5  (i)
;                 $b6  (i)
ram_B7          = $b7
;                 $b8  (i)
ram_B9          = $b9
ram_BA          = $ba
ram_BB          = $bb
ram_BC          = $bc
ram_BD          = $bd
;                 $be  (i)
ram_BF          = $bf
ram_C0          = $c0
ram_C1          = $c1
ram_C2          = $c2
ram_C3          = $c3
ram_C4          = $c4
;                 $c5  (i)
;                 $c6  (i)
ram_C7          = $c7
;                 $c8  (i)
;                 $c9  (i)
ram_CA          = $ca
;                 $cb  (i)
;                 $cc  (i)
;                 $cd  (i)
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
;                 $e7  (i)
ram_E8          = $e8
;                 $e9  (i)
ram_EA          = $ea
ram_EB          = $eb
ram_EC          = $ec
;                 $ed  (i)
ram_EE          = $ee
ram_EF          = $ef
;                 $f0  (i)
ram_F1          = $f1
;                 $f2  (i)
ram_F3          = $f3
ram_F4          = $f4
ram_F5          = $f5
ram_F6          = $f6
ram_F7          = $f7

;                 $fc  (s)
;                 $fd  (s)
;                 $fe  (s)
;                 $ff  (s)


   IF PLUSROM = 1

WriteToBuffer           = $1FF0
WriteSendBuffer         = $1FF1
ReceiveBuffer           = $1FF2
ReceiveBufferSize       = $1FF3

HIGHSCORE_ID            = 100         ; Mission 3000 AD game ID in Highscore DB

   ENDIF

;-----------------------------------------------------------
;      User Defined Labels
;-----------------------------------------------------------

Start           = $f000


;***********************************************************
;      Bank 0
;***********************************************************

    SEG     CODE
    ORG     $7000

    cld                             ;2        
    ldx     #$00                    ;2        
    lda     #$00                    ;2   =   6
L7005
    sta     VSYNC,x                 ;4        
    txs                             ;2        
    inx                             ;2        
    bne     L7005                   ;2/3      
    lda     #$22                    ;2        
    sta     ram_A7                  ;3        
    sta     TIM64T                  ;4        
    lda     #$01                    ;2        
    sta     ram_A5                  ;3        
    lda     #$00                    ;2        
    sta     ram_81                  ;3        
    sta     ram_8B                  ;3        
    lda     #$78                    ;2        
    sta     ram_8F                  ;3        
    jsr     L7426                   ;6   =  43
L7023
    lda     INTIM                   ;4        
    bne     L7023                   ;2/3      
    lda     #$82                    ;2        
    sta     WSYNC                   ;3   =  11
;---------------------------------------
    sta     VBLANK                  ;3        
    sta     VSYNC                   ;3        
    sta     WSYNC                   ;3   =   9
;---------------------------------------
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     #$00                    ;2        
    sta     VSYNC                   ;3        
    sta     VBLANK                  ;3        
    sta     PF0                     ;3        
    sta     PF1                     ;3        
    sta     PF2                     ;3        
    sta     GRP0                    ;3        
    sta     GRP1                    ;3        
    sta     ENAM0                   ;3        
    sta     ENAM1                   ;3        
    lda     #$34                    ;2        
    sta     TIM64T                  ;4        
    lda     SWCHB                   ;4        
    and     #$02                    ;2        
    bne     L7066                   ;2/3      
    ldx     #$01                    ;2        
    stx     ram_A7                  ;3        
    stx     ram_A5                  ;3        
    lda     #$00                    ;2        
    sta     ram_8B                  ;3        
    sta     ram_BF                  ;3        
    sta     ram_C0                  ;3        
    sta     ram_81                  ;3   =  65
L7066
    lda     SWCHB                   ;4        
    and     #$01                    ;2        
    bne     L70a2                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_8B                  ;3        
    sta     ram_8D                  ;3        
    sta     ram_A7                  ;3        
    sta     ram_BF                  ;3        
    sta     ram_81                  ;3        
    sta     ram_C0                  ;3        
    sta     ram_C1                  ;3        
    sta     ram_82                  ;3        
    lda     #$06                    ;2        
    sta     lives                   ;3   =  39
L7083
    jsr     L7426                   ;6        
    lda     ram_A5                  ;3        
    jsr     L7517                   ;6        
    sta     ram_A1                  ;3        
    dec     ram_A1                  ;5        
    lda     #$50                    ;2        
    sta     ram_9F                  ;3        
    lda     #$1e                    ;2        
    sta     ram_A0                  ;3        
    lda     #$ff                    ;2        
    sta     ram_E0                  ;3        
    sta     ram_A9                  ;3        
    sta     ram_A3                  ;3        
    jmp     L70e1                   ;3   =  47
    
L70a2
    lda     #$fe                    ;2        
    sta     ram_E0                  ;3        
    lda     ram_81                  ;3        
    beq     L70bf                   ;2/3      
    lda     #$ff                    ;2        
    sta     ram_B1                  ;3        
    sta     ram_B4                  ;3        
    sta     ram_B7                  ;3        
    sta     ram_BA                  ;3        
    sta     ram_BD                  ;3        
    sta     ram_A0                  ;3        
    lda     #$11                    ;2        
    sta     ram_9E                  ;3        
    jmp     L70e1                   ;3   =  38
    
L70bf
    lda     ram_A7                  ;3        
    bne     L7083                   ;2/3      
    lda     ram_82                  ;3        
    beq     L70cc                   ;2/3      
    dec     ram_82                  ;5         *
    jmp     L7083                   ;3   =  18 *
    
L70cc
    jsr     L783f                   ;6        
    lda     ram_AE                  ;3        
    and     #$01                    ;2        
    beq     L70db                   ;2/3      
    jsr     L7a71                   ;6        
    jmp     L70de                   ;3   =  22
    
L70db
    jsr     L76d2                   ;6   =   6
L70de
    jsr     L7adf                   ;6   =   6
L70e1
    lda     ram_B9                  ;3        
    ldx     #$02                    ;2        
    jsr     L7ec2                   ;6        
    lda     ram_BC                  ;3        
    ldx     #$03                    ;2        
    jsr     L7ec2                   ;6        
    lda     ram_8E                  ;3        
    ldx     #$04                    ;2        
    jsr     L7ec2                   ;6   =  33
L70f6
    lda     INTIM                   ;4        
    bne     L70f6                   ;2/3      
    lda     #COL_SCORE              ;2          score
    ldx     #$00                    ;2        
    ldy     #$46                    ;2        
    jsr     L7451                   ;6        
    lda     ram_81                  ;3        
    and     #$f3                    ;2        
    ora     #$01                    ;2        
    sta     COLUBK                  ;3        
    lda     #$92                    ;2        
    sta     TIM64T                  ;4        
    lda     #COL_PF_GRADIENT        ;2          playfield gradient, start
    sta     ram_D4                  ;3        
    lda     #$3c                    ;2        
    sta     ram_D8                  ;3        
    lda     #$fe                    ;2        
    sta     ram_D7                  ;3        
    ldx     #$ff                    ;2        
    stx     ram_A6                  ;3        
    lda     ram_81                  ;3        
    beq     L7127                   ;2/3      
    ldx     #$00                    ;2   =  61
L7127
    stx     ENABL                   ;3        
    lda     #$00                    ;2        
    sta     ram_DB                  ;3        
    sta     ram_DC                  ;3        
    ldx     #$00                    ;2        
    stx     COLUPF                  ;3        
    lda     #$10                    ;2        
    sta     NUSIZ0                  ;3        
    sta     NUSIZ1                  ;3        
L7139
    sta     WSYNC                   ;3   =  27
;---------------------------------------
L713b
    dec     ram_D8                  ;5        
    bmi     L718d                   ;2/3      
    lda     ram_D8                  ;3        
    cmp     ram_A0,x                ;4        
    beq     L71b8                   ;2/3      
    cmp     ram_A6,x                ;4        
    bne     L714c                   ;2/3      
    jmp     L7201                   ;3   =  25
    
L714c
    ldy     #$ff                    ;2        
    cmp     ram_BA                  ;3        
    bne     L7154                   ;2/3      
    sty     ENAM0                   ;3   =  10
L7154
    cmp     ram_BD                  ;3        
    bne     L715a                   ;2/3      
    sty     ENAM1                   ;3   =   8
L715a
    lda     #$80                    ;2        
    sta     WSYNC                   ;3   =   5
;---------------------------------------
    sta     HMBL                    ;3        
    ldy     ram_DB                  ;3        
    beq     L7170                   ;2/3      
    cpy     #$08                    ;2        
    bne     L716a                   ;2/3      
    sta     HMCLR                   ;3   =  15
L716a
    lda     (ram_DF),y              ;5        
    sta     GRP0                    ;3        
    dec     ram_DB                  ;5   =  13
L7170
    ldy     ram_DC                  ;3        
    beq     L7190                   ;2/3      
    cpy     #$08                    ;2        
    bne     L717a                   ;2/3      
    sta     HMCLR                   ;3   =  12
L717a
    lda     (ram_D6),y              ;5        
    sta     GRP1                    ;3        
    dec     ram_DC                  ;5        
    lda     #$00                    ;2        
    sta     ENAM1                   ;3        
    sta     ENAM0                   ;3        
    sta     WSYNC                   ;3   =  24
;---------------------------------------
    sta     HMOVE                   ;3        
    jmp     L713b                   ;3   =   6
    
L718d
    jmp     L7237                   ;3   =   3
    
L7190
    lda     ram_DB                  ;3        
    bne     L71f1                   ;2/3      
    ldy     ram_D4                  ;3        
    lda     ram_D8                  ;3        
    and     #$05                    ;2        
    sty     COLUPF                  ;3        
    beq     L71a2                   ;2/3      
    lda     #$00                    ;2        
    sta     COLUPF                  ;3   =  23
L71a2
    sec                             ;2        
    lda     ram_D4                  ;3        
    sbc     #COL_PF_GRADIENT_STEP   ;2          playfield gradient, step
    sta     ram_D4                  ;3        
    lda     #$00                    ;2        
    sta     ENAM0                   ;3        
    sta     ENAM1                   ;3        
    sta     WSYNC                   ;3   =  21
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     COLUPF                  ;3        
    jmp     L713b                   ;3   =   9
    
L71b8
    ldy     #$00                    ;2        
    sty     GRP0                    ;3        
    ldy     #$10                    ;2        
    lda     ram_A1,x                ;4        
    sta     ram_DF                  ;3        
    cmp     #$91                    ;2        
    bcc     L71c8                   ;2/3      
    ldy     #$15                    ;2   =  20
L71c8
    sty     NUSIZ0                  ;3        
    ora     #$0a                    ;2        
    sta     COLUP0                  ;3        
    lda     ram_9F,x                ;4        
    inx                             ;2        
    inx                             ;2        
    inx                             ;2        
    sta     HMCLR                   ;3        
    sta     WSYNC                   ;3   =  24
;---------------------------------------
    sec                             ;2   =   2
L71d8
    sbc     #$0f                    ;2        
    bcs     L71d8                   ;2/3      
    eor     #$07                    ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    sta     HMP0                    ;3        
    sta     RESP0                   ;3        
    ldy     #$08                    ;2        
    sty     ram_DB                  ;3        
    sta     WSYNC                   ;3   =  28
;---------------------------------------
    sta     HMOVE                   ;3        
    jmp     L713b                   ;3   =   6
    
L71f1
    lda     #$00                    ;2        
    sta     ENAM0                   ;3        
    sta     ENAM1                   ;3        
    sta     WSYNC                   ;3   =  11
;---------------------------------------
    sta     HMOVE                   ;3        
    jmp     L713b                   ;3   =   6
    jmp     L7139                   ; (*) unused?
    
L7201
    ldy     #$00                    ;2        
    sty     GRP1                    ;3        
    ldy     #$10                    ;2        
    lda     ram_A7,x                ;4        
    sta     ram_D6                  ;3        
    cmp     #$91                    ;2        
    bcc     L7211                   ;2/3      
    ldy     #$15                    ;2   =  20
L7211
    sty     NUSIZ1                  ;3        
    ora     #$0a                    ;2        
    sta     COLUP1                  ;3        
    lda     ram_A5,x                ;4        
    sta     HMCLR                   ;3        
    sta     WSYNC                   ;3   =  18
;---------------------------------------
    sec                             ;2   =   2
L721e
    sbc     #$0f                    ;2        
    bcs     L721e                   ;2/3      
    eor     #$07                    ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    sta     HMP1                    ;3        
    sta     RESP1                   ;3        
    ldy     #$08                    ;2        
    sty     ram_DC                  ;3        
    sta     WSYNC                   ;3   =  28
;---------------------------------------
    sta     HMOVE                   ;3        
    jmp     L713b                   ;3   =   6
    
L7237
    lda     #$00                    ;2        
    sta     GRP0                    ;3        
    sta     GRP1                    ;3        
    sta     ENAM0                   ;3        
    sta     ENAM1                   ;3        
    sta     ENABL                   ;3   =  17
L7243
    lda     INTIM                   ;4        
    bne     L7243                   ;2/3      
    sta     WSYNC                   ;3   =   9
;---------------------------------------
    lda     ram_AE                  ;3        
    and     #$01                    ;2        
    beq     L7254                   ;2/3      
    ldy     #$00                    ;2        
    beq     L7256                   ;2/3 =  11
L7254
    ldy     #$02                    ;2   =   2
L7256
    lda.wy  ram_91,y                ;4        
    sta     ram_D5                  ;3        
    lda.wy  ram_95,y                ;4        
    sta     ram_D7                  ;3        
    lda.wy  ram_99,y                ;4        
    sta     ram_D9                  ;3        
    clc                             ;2        
    lda.wy  ram_94,y                ;4        
    adc     #$32                    ;2        
    sta     ram_DE                  ;3        
    clc                             ;2        
    lda.wy  ram_98,y                ;4        
    adc     #$32                    ;2        
    sta     ram_DC                  ;3        
    clc                             ;2        
    lda.wy  ram_90,y                ;4        
    adc     #$32                    ;2        
    sta     WSYNC                   ;3   =  54
;---------------------------------------
    sec                             ;2   =   2
L727e
    sbc     #$0f                    ;2        
    bcs     L727e                   ;2/3      
    eor     #$07                    ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    sta     HMM0                    ;3        
    sta     RESM0                   ;3        
    lda     ram_DE                  ;3        
    sta     WSYNC                   ;3   =  26
;---------------------------------------
    sec                             ;2   =   2
L7291
    sbc     #$0f                    ;2        
    bcs     L7291                   ;2/3      
    eor     #$07                    ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    sta     HMM1                    ;3        
    sta     RESM1                   ;3        
    lda     ram_DC                  ;3        
    sta     WSYNC                   ;3   =  26
;---------------------------------------
    sec                             ;2   =   2
L72a4
    sbc     #$0f                    ;2        
    bcs     L72a4                   ;2/3      
    eor     #$07                    ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    sta     HMP1                    ;3        
    sta     RESP1                   ;3        
    lda     ram_9C                  ;3        
    lsr                             ;2        
    sta     WSYNC                   ;3   =  28
;---------------------------------------
    lsr                             ;2        
    clc                             ;2        
    adc     #$32                    ;2        
    sta     ram_DE                  ;3        
    lda     ram_9D                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    clc                             ;2        
    adc     #$01                    ;2        
    and     #$fe                    ;2        
    sta     ram_DF                  ;3        
    lda     #$01                    ;2        
    sta     CTRLPF                  ;3        
    lda     #$17                    ;2        
    sta     ram_D8                  ;3        
    lda     #$15                    ;2        
    sta     NUSIZ0                  ;3        
    sta     NUSIZ1                  ;3        
    lda     #$00                    ;2        
    sta     COLUP1                  ;3        
    lda     ram_DE                  ;3        
    ldx     #$00                    ;2        
    jsr     L7ec2                   ;6        
    lda     ram_8F                  ;3        
    sta     WSYNC                   ;3   =  67
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     COLUBK                  ;3        
    lda     #$ff                    ;2        
    sta     PF2                     ;3        
    lda     #COL_GROUND_PF          ;2          ground playfield
    sta     COLUPF                  ;3   =  16
L72f2
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMCLR                   ;3        
    dec     ram_D8                  ;5        
    bmi     L7338                   ;2/3!     
    ldx     #$00                    ;2        
    lda     ram_D8                  ;3        
    cmp     ram_D5                  ;3        
    bne     L7304                   ;2/3      
    ldx     #$ff                    ;2   =  22
L7304
    stx     ENAM0                   ;3        
    ldx     #$00                    ;2        
    cmp     ram_D7                  ;3        
    bne     L730e                   ;2/3      
    ldx     #$ff                    ;2   =  12
L730e
    stx     ENAM1                   ;3        
    ldx     #$00                    ;2        
    cmp     ram_D9                  ;3        
    bne     L7318                   ;2/3      
    ldx     #$80                    ;2   =  12
L7318
    stx     GRP1                    ;3        
    ldx     #$00                    ;2        
    cmp     ram_DF                  ;3        
    bne     L732b                   ;2/3      
    ldx     #COL_P0_HIT             ;2        
    stx     COLUP0                  ;3        
    ldx     #$80                    ;2        
    stx     GRP0                    ;3        
    jmp     L732f                   ;3   =  23
    
L732b
    stx     GRP0                    ;3        
    stx     COLUP0                  ;3   =   6
L732f
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    dec     ram_D8                  ;5        
    bmi     L7338                   ;2/3      
    jmp     L72f2                   ;3   =  10
    
L7338
    lda     #$00                    ;2        
    sta     PF2                     ;3        
    sta     COLUBK                  ;3        
    sta     COLUPF                  ;3        
    sta     COLUP0                  ;3        
    lda     #COL_PF_BOTTOM          ;2        
    sta     COLUPF                  ;3        
    sta     WSYNC                   ;3   =  22
;---------------------------------------
    lda     #$00                    ;2        
    ldy     #$04                    ;2        
    ldx     lives                   ;3        
    beq     L7366                   ;2/3      
    cpx     #$08                    ;2        
    bcc     L7358                   ;2/3      
    ldx     #$08                    ;2         *
    stx     lives                   ;3   =  18 *
L7358
    sec                             ;2        
    ror                             ;2        
    ror                             ;2        
    dex                             ;2        
    beq     L7366                   ;2/3      
    dey                             ;2        
    bne     L7358                   ;2/3      
    tay                             ;2        
    lda     #$00                    ;2        
    beq     L736d                   ;2/3 =  20
L7366
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    tay                             ;2        
    lda     #$00                    ;2        
    beq     L7373                   ;2/3 =   6
L736d
    sec                             ;2        
    rol                             ;2        
    rol                             ;2        
    dex                             ;2        
    bne     L736d                   ;2/3 =  10
L7373
    tax                             ;2        
    lda     #$06                    ;2        
    sta     ram_D4                  ;3   =   7
L7378
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    dec.w   ram_D4                  ;6        
    beq     L739c                   ;2/3      
    lda     #$00                    ;2        
    sta.w   PF0                     ;4        
    sty.w   PF1                     ;4        
    stx.w   PF2                     ;4        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    sta.w   PF1                     ;4        
    sta.w   PF2                     ;4        
    jmp     L7378                   ;3   =  51
    
L739c
    lda     #$34                    ;2        
    sta     TIM64T                  ;4        
    sec                             ;2        
    lda     ram_81                  ;3        
    beq     L73bc                   ;2/3      
    sbc     #$02                    ;2        
    sta     ram_81                  ;3   =  18
L73aa
    inc     ram_AE                  ;5        
    lda     ram_AE                  ;3        
    cmp     #$04                    ;2        
    bne     L73b9                   ;2/3      
    lda     #$01                    ;2        
    sta     ram_AE                  ;3        
    jsr     L7cf4                   ;6   =  23
L73b9
    jmp     L7023                   ;3   =   3
    
L73bc
    lda     ram_82                  ;3        
    beq     L73c3                   ;2/3      
    jmp     L73aa                   ;3   =   8 *
    
L73c3
    lda     ram_8B                  ;3        
    beq     L73ce                   ;2/3      
    cmp     #$50                    ;2        
    bcs     L73ce                   ;2/3      
    jsr     L7cff                   ;6   =  15
L73ce
    inc     ram_AE                  ;5        
    lda     ram_AE                  ;3        
    cmp     #$04                    ;2        
    bcc     L73da                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_AE                  ;3   =  17
L73da
    and     #$01                    ;2        
    beq     L73e7                   ;2/3      
    jsr     L79d2                   ;6        
    jsr     L7b65                   ;6        
    jmp     L7023                   ;3   =  19
    
L73e7
    lda     ram_AE                  ;3        
    beq     L741d                   ;2/3!     
    lda     SWCHA                   ;4        
    asl                             ;2        
    bcs     L73fc                   ;2/3      
    dec     ram_8E                  ;5        
    bne     L73f9                   ;2/3      
    lda     #$a0                    ;2        
    sta     ram_8E                  ;3   =  25
L73f9
    jmp     L7408                   ;3   =   3
    
L73fc
    inc     ram_8E                  ;5        
    lda     ram_8E                  ;3        
    cmp     #$a1                    ;2        
    bcc     L7408                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_8E                  ;3   =  17
L7408
    inc     ram_8C                  ;5        
    jsr     L7cf4                   ;6        
    jsr     L7f58                   ;6        
    lda     ram_8B                  ;3        
    cmp     #$a0                    ;2        
    bcs     L741a                   ;2/3      
    lda     #$00                    ;2        
    sta     AUDV0                   ;3   =  29
L741a
    jmp     L7023                   ;3   =   3
    
L741d
    jsr     L777b                   ;6        
    jsr     L791e                   ;6        
    jmp     L7023                   ;3   =  15
    
L7426
    sta     CXCLR                   ;3        
    lda     #$ff                    ;2        
    ldx     #$00                    ;2        
    stx     ram_8C                  ;3   =  10
L742e
    sta     ram_B1,x                ;4        
    inx                             ;2        
    inx                             ;2        
    inx                             ;2        
    cpx     #$0c                    ;2        
    bne     L742e                   ;2/3      
    sta     ram_BA                  ;3        
    sta     ram_BD                  ;3        
    sta     ram_A0                  ;3        
    sta     ram_EB                  ;3        
    sta     ram_EE                  ;3        
    sta     ram_E2                  ;3        
    sta     ram_E8                  ;3        
    sta     ram_F1                  ;3        
    sta     ram_E5                  ;3        
    lda     #$11                    ;2        
    sta     ram_9E                  ;3        
    jsr     L7874                   ;6        
    rts                             ;6   =  58
    
L7451
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     COLUP0                  ;3        
    sta     COLUP1                  ;3        
    lda     #$d0                    ;2        
    sta     HMP0                    ;3        
    lda     #$a0                    ;2        
    sta     HMP1                    ;3        
    nop                             ;2        
    lda     #$03                    ;2        
    sta     NUSIZ0                  ;3        
    sta     NUSIZ1                  ;3        
    sta     RESP0                   ;3        
    lda.wx  ram_BF,x                ;4        
    and     #$f0                    ;2        
    lsr                             ;2        
    clc                             ;2        
    adc     #$08                    ;2        
    sta     RESP1                   ;3        
    sta     WSYNC                   ;3   =  47
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     ram_D4                  ;3        
    lda     ram_BF,x                ;4        
    and     #$0f                    ;2        
    jsr     L7517                   ;6        
    sta     ram_D6                  ;3        
    lda     ram_C0,x                ;4        
    and     #$f0                    ;2        
    lsr                             ;2        
    clc                             ;2        
    adc     #$08                    ;2        
    sta     ram_D8                  ;3        
    lda     ram_C0,x                ;4        
    and     #$0f                    ;2        
    jsr     L7517                   ;6        
    sta     ram_DA                  ;3        
    sty     COLUBK                  ;3        
    lda     ram_C1,x                ;4        
    and     #$f0                    ;2        
    lsr                             ;2        
    clc                             ;2        
    adc     #$08                    ;2        
    sta     ram_DC                  ;3        
    sta     HMCLR                   ;3        
    lda     ram_C1,x                ;4        
    and     #$0f                    ;2        
    jsr     L7517                   ;6        
    sta     ram_DE                  ;3        
    ldy     #$07                    ;2        
    lda     #$ff                    ;2        
    sta     ram_D5                  ;3        
    sta     ram_D7                  ;3        
    sta     ram_D9                  ;3        
    sta     ram_DB                  ;3        
    sta     ram_DD                  ;3        
    sta     ram_DF                  ;3        
    ldx     #$00                    ;2        
    lda     ram_D4                  ;3        
    cmp     #$08                    ;2        
    bne     L74e6                   ;2/3      
    stx     ram_D4                  ;3        
    lda     ram_D6                  ;3        
    cmp     #$08                    ;2        
    bne     L74e6                   ;2/3      
    stx     ram_D6                  ;3        
    lda     ram_D8                  ;3        
    cmp     #$08                    ;2        
    bne     L74e6                   ;2/3      
    stx     ram_D8                  ;3        
    lda     ram_DA                  ;3        
    cmp     #$08                    ;2        
    bne     L74e6                   ;2/3      
    stx     ram_DA                  ;3        
    lda     ram_DC                  ;3        
    cmp     #$08                    ;2        
    bne     L74e6                   ;2/3      
    stx     ram_DC                  ;3   = 161
L74e6
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     (ram_D6),y              ;5        
    tax                             ;2        
    lda     (ram_D4),y              ;5        
    sta     GRP0                    ;3        
    lda     (ram_DA),y              ;5        
    sta     GRP1                    ;3        
    lda     (ram_DE),y              ;5        
    pha                             ;3        
    stx     GRP0                    ;3        
    lda     (ram_D8),y              ;5        
    sta     GRP0                    ;3        
    lda     (ram_DC),y              ;5        
    sta     GRP1                    ;3        
    pla                             ;4        
    sta     GRP1                    ;3        
    dey                             ;2        
    bne     L74e6                   ;2/3!     
    lda     #$00                    ;2        
    sta     GRP0                    ;3        
    sta     GRP1                    ;3        
    sta     NUSIZ0                  ;3        
    sta     NUSIZ1                  ;3        
    sta     WSYNC                   ;3   =  78
;---------------------------------------
    lda     #$00                    ;2        
    sta     COLUBK                  ;3        
    rts                             ;6   =  11
    
L7517
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    clc                             ;2        
    adc     #$08                    ;2        
    rts                             ;6   =  16
    
L751e
    lda     ram_B2,x                ;4        
    cmp     #$41                    ;2        
    beq     L7558                   ;2/3      
    cmp     #$49                    ;2        
    beq     L7554                   ;2/3      
    cmp     #$51                    ;2        
    beq     L7550                   ;2/3      
    cmp     #$59                    ;2        
    beq     L754c                   ;2/3      
    cmp     #$61                    ;2        
    beq     L7548                   ;2/3      
    cmp     #$69                    ;2        
    beq     L7544                   ;2/3      
    cmp     #$71                    ;2        
    beq     L7540                   ;2/3      
    lda     #$fe                    ;2        
    bne     L7560                   ;2/3 =  36
L7540
    lda     #$00                    ;2        
    beq     L7560                   ;2/3 =   4
L7544
    lda     #$02                    ;2        
    bne     L7560                   ;2/3 =   4
L7548
    lda     #$02                    ;2        
    bne     L755a                   ;2/3 =   4
L754c
    lda     #$02                    ;2        
    bne     L7566                   ;2/3 =   4
L7550
    lda     #$00                    ;2        
    beq     L7566                   ;2/3 =   4
L7554
    lda     #$fe                    ;2        
    bne     L7566                   ;2/3 =   4
L7558
    lda     #$fe                    ;2   =   2
L755a
    sta     ram_D9                  ;3        
    lda     #$00                    ;2        
    beq     L756a                   ;2/3 =   7
L7560
    sta     ram_D9                  ;3        
    lda     #$ff                    ;2        
    bne     L756a                   ;2/3 =   7
L7566
    sta     ram_D9                  ;3        
    lda     #$01                    ;2   =   5
L756a
    sta     ram_D5                  ;3        
    lda     ram_8F                  ;3        
    cmp     #COL_GROUND_C           ;2          ground color is used as state
    bne     L7579                   ;2/3      
    lda     ram_AE                  ;3        
    bne     L7579                   ;2/3      
    jmp     L75b0                   ;3   =  18
    
L7579
    lda     SWCHB                   ;4        
    and     #$40                    ;2        
    beq     L7586                   ;2/3      
    lda     ram_8C                  ;3         *
    and     #$03                    ;2         *
    bne     L75a3                   ;2/3 =  15 *
L7586
    lda     ram_AE                  ;3        
    beq     L75a3                   ;2/3      
    lda     ram_8F                  ;3        
    cmp     #COL_GROUND_A           ;2          ground color is used as state
    beq     L75a3                   ;2/3      
    cmp     #COL_GROUND_B           ;2        
    beq     L75b0                   ;2/3      
    lda     ram_D5                  ;3        
    beq     L75b0                   ;2/3      
    bmi     L759e                   ;2/3      
    inc     ram_D5                  ;5        
    bne     L75b0                   ;2/3 =  30
L759e
    dec     ram_D5                  ;5        
    jmp     L75b0                   ;3   =   8
    
L75a3
    lda     ram_D9                  ;3        
    beq     L75b0                   ;2/3      
    bmi     L75ae                   ;2/3      
    dec     ram_D9                  ;5        
    jmp     L75b0                   ;3   =  15
    
L75ae
    inc     ram_D9                  ;5   =   5
L75b0
    sec                             ;2        
    lda.wy  ram_83,y                ;4        
    sbc     #$01                    ;2        
    sta.wy  ram_83,y                ;5        
    bne     L75df                   ;2/3      
    lda     ram_8C                  ;3        
    adc     ram_DE,x                ;4        
    and     #$17                    ;2        
    ora     #$07                    ;2        
    sta.wy  ram_83,y                ;5        
    lda     ram_D9                  ;3        
    bne     L75e2                   ;2/3      
    lda     ram_B1,x                ;4        
    adc     ram_8C                  ;3        
    and     #$03                    ;2        
    cmp     #$02                    ;2        
    bcc     L75d8                   ;2/3      
    lda     #$02                    ;2        
    bne     L75da                   ;2/3 =  53
L75d8
    lda     #$fe                    ;2   =   2
L75da
    sta     ram_D9                  ;3        
    jmp     L760d                   ;3   =   6
    
L75df
    jmp     L7643                   ;3   =   3
    
L75e2
    lda     ram_D5                  ;3        
    bne     L75fb                   ;2/3      
    lda     ram_B1,x                ;4        
    adc     ram_8C                  ;3        
    and     #$03                    ;2        
    cmp     #$02                    ;2        
    bcc     L75f4                   ;2/3      
    lda     #$01                    ;2        
    bne     L75f6                   ;2/3 =  22
L75f4
    lda     #$ff                    ;2   =   2
L75f6
    sta     ram_D5                  ;3        
    jmp     L760d                   ;3   =   6
    
L75fb
    lda     ram_B1,x                ;4        
    adc     ram_8C                  ;3        
    and     #$03                    ;2        
    cmp     #$02                    ;2        
    bcc     L7609                   ;2/3      
    lda     #$00                    ;2        
    beq     L75f6                   ;2/3!=  17
L7609
    lda     #$00                    ;2        
    beq     L75da                   ;2/3!=   4
L760d
    lda     ram_D9                  ;3        
    beq     L7625                   ;2/3      
    bmi     L7631                   ;2/3      
    lda     ram_D5                  ;3        
    beq     L761d                   ;2/3      
    bmi     L7621                   ;2/3      
    lda     #$59                    ;2        
    bne     L7641                   ;2/3 =  18
L761d
    lda     #$61                    ;2        
    bne     L7641                   ;2/3 =   4
L7621
    lda     #$69                    ;2        
    bne     L7641                   ;2/3 =   4
L7625
    lda     ram_D5                  ;3        
    bmi     L762d                   ;2/3      
    lda     #$51                    ;2        
    bne     L7641                   ;2/3 =   9
L762d
    lda     #$71                    ;2        
    bne     L7641                   ;2/3 =   4
L7631
    lda     ram_D5                  ;3        
    beq     L763b                   ;2/3      
    bmi     L763f                   ;2/3      
    lda     #$49                    ;2        
    bne     L7641                   ;2/3 =  11
L763b
    lda     #$41                    ;2        
    bne     L7641                   ;2/3 =   4
L763f
    lda     #$79                    ;2   =   2
L7641
    sta     ram_B2,x                ;4   =   4
L7643
    sty     ram_DE                  ;3        
    lda     SWCHA                   ;4        
    asl                             ;2        
    bcs     L7659                   ;2/3      
    ldy     ram_D9                  ;3        
    bpl     L7655                   ;2/3      
    ldy     #$00                    ;2        
    sty     ram_D9                  ;3        
    beq     L7659                   ;2/3 =  23
L7655
    lda     #$fe                    ;2        
    sta     ram_D9                  ;3   =   5
L7659
    asl                             ;2        
    bcs     L766a                   ;2/3      
    ldy     ram_D9                  ;3        
    bmi     L7666                   ;2/3      
    ldy     #$00                    ;2        
    sty     ram_D9                  ;3        
    beq     L766a                   ;2/3 =  16
L7666
    lda     #$02                    ;2        
    sta     ram_D9                  ;3   =   5
L766a
    asl                             ;2        
    bcs     L767b                   ;2/3      
    ldy     ram_D5                  ;3        
    bmi     L7677                   ;2/3      
    ldy     #$00                    ;2        
    sty     ram_D5                  ;3        
    beq     L767b                   ;2/3 =  16
L7677
    lda     #$02                    ;2        
    sta     ram_D5                  ;3   =   5
L767b
    asl                             ;2        
    bcs     L768c                   ;2/3      
    ldy     ram_D5                  ;3        
    bpl     L7688                   ;2/3      
    ldy     #$00                    ;2        
    sty     ram_D5                  ;3        
    beq     L768c                   ;2/3 =  16
L7688
    lda     #$fe                    ;2        
    sta     ram_D5                  ;3   =   5
L768c
    ldy     ram_DE                  ;3        
    lda     ram_D9                  ;3        
    bne     L769f                   ;2/3      
    lda     ram_D5                  ;3        
    bne     L769f                   ;2/3      
    lda     #$01                    ;2        
    sta.wy  ram_83,y                ;5        
    sta     ram_D9                  ;3        
    sta     ram_D5                  ;3   =  26
L769f
    clc                             ;2        
    lda     ram_B1,x                ;4        
    adc     ram_D5                  ;3        
    sta     ram_B1,x                ;4        
    clc                             ;2        
    lda     ram_B0,x                ;4        
    adc     ram_D9                  ;3        
    cmp     #$f0                    ;2        
    bcs     L76b5                   ;2/3      
    cmp     #$94                    ;2        
    bcc     L76bb                   ;2/3      
    bcs     L76b9                   ;2/3 =  32
L76b5
    lda     #$93                    ;2        
    bne     L76bb                   ;2/3 =   4
L76b9
    lda     #$02                    ;2   =   2
L76bb
    sta     ram_B0,x                ;4        
    lda     ram_B1,x                ;4        
    cmp     #$f0                    ;2        
    bcs     L76c9                   ;2/3      
    cmp     #$3d                    ;2        
    bcc     L76d1                   ;2/3      
    bcs     L76cd                   ;2/3 =  18
L76c9
    lda     #$3b                    ;2        
    bne     L76cf                   ;2/3 =   4
L76cd
    lda     #$01                    ;2   =   2
L76cf
    sta     ram_B1,x                ;4   =   4
L76d1
    rts                             ;6   =   6
    
L76d2
    lda     ram_83                  ;3        
    cmp     #$01                    ;2        
    bne     L76ec                   ;2/3      
    lda     ram_84                  ;3        
    cmp     #$01                    ;2        
    bne     L76ec                   ;2/3      
    lda     #$02                    ;2        
    sta     ram_84                  ;3        
    lda     ram_85                  ;3        
    cmp     #$01                    ;2        
    bne     L76ec                   ;2/3      
    lda     #$03                    ;2        
    sta     ram_85                  ;3   =  31
L76ec
    ldx     #$00                    ;2        
    ldy     #$00                    ;2   =   4
L76f0
    lda     ram_B1,x                ;4        
    cmp     #$ff                    ;2        
    beq     L7712                   ;2/3!     
    lda     ram_B2,x                ;4        
    cmp     #$89                    ;2        
    bne     L7702                   ;2/3!     
    lda     #$ff                    ;2        
    sta     ram_B1,x                ;4        
    bne     L7712                   ;2/3 =  24
L7702
    cmp     #$81                    ;2        
    bne     L770f                   ;2/3      
    jsr     L7761                   ;6        
    lda     #$89                    ;2        
    sta     ram_B2,x                ;4        
    bne     L7712                   ;2/3 =  18
L770f
    jsr     L751e                   ;6   =   6
L7712
    iny                             ;2        
    inx                             ;2        
    inx                             ;2        
    inx                             ;2        
    cpx     #$09                    ;2        
    bne     L76f0                   ;2/3!     
    lda     ram_8B                  ;3        
    cmp     #$a0                    ;2        
    bcs     L7760                   ;2/3      
    lda     ram_AE                  ;3        
    bne     L7760                   ;2/3      
    lda     ram_8F                  ;3        
    cmp     #COL_GROUND_A           ;2          ground color is used as state
    beq     L7760                   ;2/3      
    cmp     #COL_GROUND_C           ;2        
    beq     L7738                   ;2/3      
    lda     ram_8C                  ;3        
    and     #$03                    ;2        
    bne     L7760                   ;2/3      
    lda     #$0c                    ;2        
    bne     L7756                   ;2/3 =  46
L7738
    lda     ram_F6                  ;3        
    cmp     #$13                    ;2        
    beq     L774a                   ;2/3      
    cmp     #$11                    ;2        
    beq     L774e                   ;2/3      
    cmp     #$0f                    ;2        
    beq     L7752                   ;2/3      
    lda     #$13                    ;2        
    bne     L7754                   ;2/3 =  19
L774a
    lda     #$11                    ;2        
    bne     L7754                   ;2/3 =   4
L774e
    lda     #$0f                    ;2        
    bne     L7754                   ;2/3 =   4
L7752
    lda     #$0e                    ;2   =   2
L7754
    sta     ram_F6                  ;3   =   3
L7756
    sta     AUDF0                   ;3        
    lda     #$04                    ;2        
    sta     AUDC0                   ;3        
    lda     #$08                    ;2        
    sta     AUDV0                   ;3   =  13
L7760
    rts                             ;6   =   6
    
L7761
    lda     SWCHA                   ;4        
    asl                             ;2        
    bcs     L7769                   ;2/3      
    dec     ram_B0,x                ;6   =  14
L7769
    asl                             ;2        
    bcs     L776e                   ;2/3      
    inc     ram_B0,x                ;6   =  10
L776e
    asl                             ;2        
    bcs     L7773                   ;2/3      
    inc     ram_B1,x                ;6   =  10
L7773
    asl                             ;2        
    bcs     L7778                   ;2/3      
    dec     ram_B1,x                ;6   =  10
L7778
    jmp     L769f                   ;3   =   3
    
L777b
    lda     SWCHA                   ;4        
    and     #$f0                    ;2        
    cmp     #$70                    ;2        
    beq     L77a3                   ;2/3      
    cmp     #$b0                    ;2        
    beq     L77a7                   ;2/3      
    cmp     #$d0                    ;2        
    beq     L77ab                   ;2/3      
    cmp     #$e0                    ;2        
    beq     L77af                   ;2/3      
    cmp     #$50                    ;2        
    beq     L77b3                   ;2/3      
    cmp     #$60                    ;2        
    beq     L77b7                   ;2/3      
    cmp     #$90                    ;2        
    beq     L77bb                   ;2/3      
    cmp     #$a0                    ;2        
    beq     L77bf                   ;2/3      
    jmp     L77c5                   ;3   =  41
    
L77a3
    lda     #$21                    ;2        
    bne     L77c1                   ;2/3 =   4
L77a7
    lda     #$01                    ;2        
    bne     L77c1                   ;2/3 =   4
L77ab
    lda     #$31                    ;2        
    bne     L77c1                   ;2/3 =   4
L77af
    lda     #$11                    ;2        
    bne     L77c1                   ;2/3 =   4
L77b3
    lda     #$29                    ;2        
    bne     L77c1                   ;2/3 =   4
L77b7
    lda     #$19                    ;2        
    bne     L77c1                   ;2/3 =   4
L77bb
    lda     #$39                    ;2        
    bne     L77c1                   ;2/3 =   4
L77bf
    lda     #$09                    ;2   =   2
L77c1
    sta     ram_F5                  ;3        
    sta     ram_9E                  ;3   =   6
L77c5
    lda.w   INPT4                   ;4        
    and     #$80                    ;2        
    bne     L77d8                   ;2/3      
    lda     ram_BA                  ;3        
    cmp     #$ff                    ;2        
    beq     L77d9                   ;2/3      
    lda     ram_BD                  ;3        
    cmp     #$ff                    ;2        
    beq     L77dd                   ;2/3 =  22
L77d8
    rts                             ;6   =   6
    
L77d9
    ldx     #$00                    ;2        
    beq     L77df                   ;2/3 =   4
L77dd
    ldx     #$03                    ;2   =   2
L77df
    lda     #$54                    ;2        
    sta     ram_B9,x                ;4        
    lda     #$1a                    ;2        
    sta     ram_BA,x                ;4        
    lda     ram_9E                  ;3        
    sta     ram_BB,x                ;4        
    lda     ram_8B                  ;3        
    bne     L77f3                   ;2/3      
    lda     #$44                    ;2        
    sta     ram_8B                  ;3   =  29
L77f3
    rts                             ;6   =   6
    
L77f4
    cmp     #$39                    ;2        
    beq     L7817                   ;2/3!     
    cmp     #$31                    ;2        
    beq     L781b                   ;2/3!     
    cmp     #$29                    ;2        
    beq     L7820                   ;2/3      
    cmp     #$21                    ;2        
    beq     L7827                   ;2/3      
    cmp     #$19                    ;2        
    beq     L782e                   ;2/3      
    cmp     #$11                    ;2        
    beq     L7832                   ;2/3      
    cmp     #$09                    ;2        
    beq     L7837                   ;2/3      
    dec     ram_D6                  ;5        
    dec     ram_D6                  ;5        
    jmp     L783e                   ;3   =  41
    
L7817
    dec     ram_D6                  ;5        
    dec     ram_D6                  ;5   =  10
L781b
    dec     ram_D7                  ;5        
    jmp     L783e                   ;3   =   8
    
L7820
    inc     ram_D6                  ;5        
    inc     ram_D6                  ;5        
    jmp     L781b                   ;3   =  13
    
L7827
    inc     ram_D6                  ;5        
    inc     ram_D6                  ;5        
    jmp     L783e                   ;3   =  13
    
L782e
    inc     ram_D6                  ;5        
    inc     ram_D6                  ;5   =  10
L7832
    inc     ram_D7                  ;5        
    jmp     L783e                   ;3   =   8
    
L7837
    dec     ram_D6                  ;5        
    dec     ram_D6                  ;5        
    jmp     L7832                   ;3   =  13
    
L783e
    rts                             ;6   =   6
    
L783f
    ldx     #$00                    ;2   =   2
L7841
    lda     ram_B9,x                ;4        
    sta     ram_D6                  ;3        
    lda     ram_BA,x                ;4        
    cmp     #$ff                    ;2        
    beq     L786c                   ;2/3      
    sta     ram_D7                  ;3        
    lda     ram_BB,x                ;4        
    jsr     L77f4                   ;6        
    lda     ram_D6                  ;3        
    cmp     #$98                    ;2        
    bcs     L7864                   ;2/3      
    cmp     #$02                    ;2        
    bcc     L7864                   ;2/3      
    sta     ram_B9,x                ;4        
    lda     ram_D7                  ;3        
    cmp     #$3c                    ;2        
    bcc     L786a                   ;2/3 =  50
L7864
    lda     #$00                    ;2        
    sta     ram_B9,x                ;4        
    lda     #$ff                    ;2   =   8
L786a
    sta     ram_BA,x                ;4   =   4
L786c
    inx                             ;2        
    inx                             ;2        
    inx                             ;2        
    cpx     #$03                    ;2        
    beq     L7841                   ;2/3      
    rts                             ;6   =  16
    
L7874
    ldy     #$00                    ;2        
    lda     #$06                    ;2        
    sta     ram_DD                  ;3        
    lda     ram_A5                  ;3        
    cmp     #$01                    ;2        
    beq     L78a4                   ;2/3      
    cmp     #$02                    ;2         *
    beq     L78a0                   ;2/3       *
    cmp     #$03                    ;2         *
    beq     L789c                   ;2/3       *
    cmp     #$04                    ;2         *
    beq     L7898                   ;2/3       *
    cmp     #$06                    ;2         *
    beq     L7894                   ;2/3       *
    ldx     #$04                    ;2         *
    bne     L78a6                   ;2/3 =  34 *
L7894
    ldx     #$04                    ;2         *
    bne     L78ff                   ;2/3 =   4 *
L7898
    ldx     #$02                    ;2         *
    bne     L78ff                   ;2/3 =   4 *
L789c
    ldx     #$02                    ;2         *
    bne     L78a6                   ;2/3 =   4 *
L78a0
    ldx     #$00                    ;2         *
    beq     L78ff                   ;2/3 =   4 *
L78a4
    ldx     #$00                    ;2   =   2
L78a6
    dec     ram_DD                  ;5   =   5
L78a8
    lda     L7a4b,x                 ;4        
    sta.wy  ram_C2,y                ;5        
    lda     L7a4c,x                 ;4        
    sta.wy  ram_C3,y                ;5        
    lda     #$91                    ;2        
    sta.wy  ram_C4,y                ;5        
    txa                             ;2        
    clc                             ;2        
    adc     #$08                    ;2        
    tax                             ;2        
    iny                             ;2        
    iny                             ;2        
    iny                             ;2        
    dec     ram_DD                  ;5        
    bne     L78a8                   ;2/3      
    lda     #$e8                    ;2        
    sta     ram_D1                  ;3        
    lda     #$43                    ;2        
    sta     ram_D2                  ;3   =  56
L78cd
    lda     #$99                    ;2        
    sta     ram_D3                  ;3        
    sta     ram_CA                  ;3        
    sta     ram_C7                  ;3        
    ldy     #$00                    ;2        
    ldx     #$00                    ;2   =  15
L78d9
    lda     ram_C2,x                ;4        
    lsr                             ;2        
    lsr                             ;2        
    sta.wy  ram_90,y                ;5        
    lda     ram_C3,x                ;4        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    clc                             ;2        
    adc     #$01                    ;2        
    and     #$fe                    ;2        
    sta.wy  ram_91,y                ;5        
    inx                             ;2        
    inx                             ;2        
    inx                             ;2        
    iny                             ;2        
    iny                             ;2        
    cpx     #$12                    ;2        
    bne     L78d9                   ;2/3      
    lda     #$28                    ;2        
    sta     ram_9C                  ;3        
    lda     #$1e                    ;2        
    sta     ram_9D                  ;3        
    rts                             ;6   =  64
    
L78ff
    lda     L7a4b,x                 ;4         *
    sta.wy  ram_C2,y                ;5         *
    lda     L7a4c,x                 ;4         *
    sta.wy  ram_C3,y                ;5         *
    lda     #$91                    ;2         *
    sta.wy  ram_C4,y                ;5         *
    txa                             ;2         *
    clc                             ;2         *
    adc     #$06                    ;2         *
    tax                             ;2         *
    iny                             ;2         *
    iny                             ;2         *
    iny                             ;2         *
    dec     ram_DD                  ;5         *
    bne     L78ff                   ;2/3!      *
    beq     L78cd                   ;2/3!=  48 *
L791e
    ldx     #$00                    ;2   =   2
L7920
    lda     ram_C3,x                ;4        
    cmp     #$ff                    ;2        
    beq     L798b                   ;2/3      
    lda     SWCHA                   ;4        
    asl                             ;2        
    bcs     L792e                   ;2/3      
    dec     ram_C2,x                ;6   =  22
L792e
    asl                             ;2        
    bcs     L7933                   ;2/3      
    inc     ram_C2,x                ;6   =  10
L7933
    asl                             ;2        
    bcs     L7938                   ;2/3      
    inc     ram_C3,x                ;6   =  10
L7938
    asl                             ;2        
    bcs     L793d                   ;2/3      
    dec     ram_C3,x                ;6   =  10
L793d
    lda     ram_C2,x                ;4        
    cmp     #$02                    ;2        
    bcc     L794b                   ;2/3      
    cmp     #$f1                    ;2        
    bcc     L794f                   ;2/3      
    lda     #$02                    ;2        
    bne     L794d                   ;2/3 =  16
L794b
    lda     #$f0                    ;2   =   2
L794d
    sta     ram_C2,x                ;4   =   4
L794f
    lda     ram_C3,x                ;4        
    cmp     #$f0                    ;2        
    bcs     L795d                   ;2/3      
    cmp     #$b4                    ;2        
    bcc     L7961                   ;2/3      
    lda     #$00                    ;2        
    beq     L795f                   ;2/3 =  16
L795d
    lda     #$b3                    ;2   =   2
L795f
    sta     ram_C3,x                ;4   =   4
L7961
    lda     ram_8C                  ;3        
    and     #$03                    ;2        
    bne     L798b                   ;2/3      
    lda     ram_C4,x                ;4        
    cmp     #$91                    ;2        
    beq     L7987                   ;2/3      
    cmp     #$99                    ;2        
    beq     L7983                   ;2/3      
    cmp     #$b1                    ;2        
    beq     L797f                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_C2,x                ;4        
    lda     #$ff                    ;2        
    sta     ram_C3,x                ;4        
    bne     L798b                   ;2/3 =  37
L797f
    lda     #$b9                    ;2        
    bne     L7989                   ;2/3 =   4
L7983
    lda     #$91                    ;2        
    bne     L7989                   ;2/3 =   4
L7987
    lda     #$99                    ;2   =   2
L7989
    sta     ram_C4,x                ;4   =   4
L798b
    inx                             ;2        
    inx                             ;2        
    inx                             ;2        
    cpx     #$12                    ;2        
    bne     L7920                   ;2/3      
    lda     SWCHA                   ;4        
    asl                             ;2        
    bcs     L799a                   ;2/3      
    inc     ram_9C                  ;5   =  23
L799a
    asl                             ;2        
    bcs     L799f                   ;2/3      
    dec     ram_9C                  ;5   =   9
L799f
    asl                             ;2        
    bcs     L79a4                   ;2/3      
    dec     ram_9D                  ;5   =   9
L79a4
    asl                             ;2        
    bcs     L79a9                   ;2/3      
    inc     ram_9D                  ;5   =   9
L79a9
    lda     ram_9C                  ;3        
    cmp     #$f8                    ;2        
    bcs     L79ba                   ;2/3      
    cmp     #$f1                    ;2        
    bcs     L79b6                   ;2/3      
    jmp     L79be                   ;3   =  14
    
L79b6
    lda     #$00                    ;2        
    beq     L79bc                   ;2/3 =   4
L79ba
    lda     #$f0                    ;2   =   2
L79bc
    sta     ram_9C                  ;3   =   3
L79be
    lda     ram_9D                  ;3        
    cmp     #$f0                    ;2        
    bcs     L79cd                   ;2/3      
    cmp     #$b5                    ;2        
    bcs     L79c9                   ;2/3      
    rts                             ;6   =  17
    
L79c9
    lda     #$00                    ;2        
    beq     L79cf                   ;2/3 =   4
L79cd
    lda     #$b4                    ;2   =   2
L79cf
    sta     ram_9D                  ;3        
    rts                             ;6   =   9
    
L79d2
    ldx     #$00                    ;2        
    lda     #$ff                    ;2   =   4
L79d6
    sta     ram_EB,x                ;4        
    inx                             ;2        
    inx                             ;2        
    inx                             ;2        
    cpx     #$09                    ;2        
    bne     L79d6                   ;2/3      
    ldy     #$00                    ;2        
    ldx     #$00                    ;2   =  18
L79e3
    lda     ram_C2,x                ;4        
    cmp     #$4b                    ;2        
    bcs     L79f2                   ;2/3      
    lda     ram_C3,x                ;4        
    cmp     #$3c                    ;2        
    bcs     L79f2                   ;2/3      
    jsr     L7a2d                   ;6   =  22
L79f2
    inx                             ;2        
    inx                             ;2        
    inx                             ;2        
    cpx     #$12                    ;2        
    bne     L79e3                   ;2/3      
    lda     ram_A5                  ;3        
    cmp     #$02                    ;2        
    beq     L7a0e                   ;2/3!     
    cmp     #$05                    ;2        
    beq     L7a0e                   ;2/3      
    cmp     #$03                    ;2        
    beq     L7a12                   ;2/3      
    cmp     #$06                    ;2        
    beq     L7a12                   ;2/3      
    jmp     L7a2c                   ;3   =  32
    
L7a0e
    ldx     #$10                    ;2         *
    bne     L7a14                   ;2/3 =   4 *
L7a12
    ldx     #$42                    ;2   =   2 *
L7a14
    lda     ram_EC                  ;3         *
    cmp     #$b1                    ;2         *
    bcs     L7a20                   ;2/3       *
    clc                             ;2         *
    txa                             ;2         *
    adc     ram_EC                  ;3         *
    sta     ram_EC                  ;3   =  17 *
L7a20
    lda     ram_EF                  ;3         *
    cmp     #$b1                    ;2         *
    bcs     L7a2c                   ;2/3       *
    clc                             ;2         *
    txa                             ;2         *
    adc     ram_EF                  ;3         *
    sta     ram_EF                  ;3   =  17 *
L7a2c
    rts                             ;6   =   6
    
L7a2d
    sta.wy  ram_EB,y                ;5        
    lda     ram_C4,x                ;4        
    sta.wy  ram_EC,y                ;5        
    lda     ram_C2,x                ;4        
    asl                             ;2        
    cmp     #$02                    ;2        
    bcs     L7a3e                   ;2/3      
    lda     #$02                    ;2   =  26 *
L7a3e
    cmp     #$94                    ;2        
    bcc     L7a44                   ;2/3      
    lda     #$93                    ;2   =   6
L7a44
    sta.wy  ram_EA,y                ;5        
    iny                             ;2        
    iny                             ;2        
    iny                             ;2        
    rts                             ;6   =  17
    
L7a4b
    .byte   $10                             ; $7a4b (D)
L7a4c
    .byte   $08                             ; $7a4c (D)
    .byte   $48,$0c,$59,$24,$74,$28         ; $7a4d (*)
    .byte   $93,$20                         ; $7a53 (D)
    .byte   $be,$3a,$28,$3e,$6d,$4f         ; $7a55 (*)
    .byte   $8f,$5e                         ; $7a5b (D)
    .byte   $ac,$6b,$c4,$50,$d8,$43         ; $7a5d (*)
    .byte   $26,$78                         ; $7a63 (D)
    .byte   $45,$8f,$6a,$6a,$84,$a9         ; $7a65 (*)
    .byte   $95,$83                         ; $7a6b (D)
    .byte   $ac,$8e,$c1,$b2                 ; $7a6d (*)
    
L7a71
    lda     #$50                    ;2        
    sta     ram_F3                  ;3        
    lda     #$1e                    ;2        
    sta     ram_F4                  ;3        
    lda     ram_9E                  ;3        
    sta     ram_F5                  ;3        
    ldx     #$00                    ;2   =  18
L7a7f
    lda     ram_B0,x                ;4        
    sta     ram_E1,x                ;4        
    inx                             ;2        
    cpx     #$09                    ;2        
    bne     L7a7f                   ;2/3      
    ldx     #$00                    ;2   =  16
L7a8a
    inc     ram_E2,x                ;6        
    inx                             ;2        
    inx                             ;2        
    inx                             ;2        
    cpx     #$15                    ;2        
    bne     L7a8a                   ;2/3      
    lda     #$12                    ;2        
    sta     ram_D5                  ;3   =  21
L7a97
    ldx     #$00                    ;2   =   2
L7a99
    lda     ram_E2,x                ;4        
    cmp     ram_E5,x                ;4        
    bcs     L7ac3                   ;2/3      
    lda     ram_E4,x                ;4        
    sta     ram_D9                  ;3        
    lda     ram_E1,x                ;4        
    sta     ram_E4,x                ;4        
    lda     ram_D9                  ;3        
    sta     ram_E1,x                ;4        
    lda     ram_E5,x                ;4        
    sta     ram_D9                  ;3        
    lda     ram_E2,x                ;4        
    sta     ram_E5,x                ;4        
    lda     ram_D9                  ;3        
    sta     ram_E2,x                ;4        
    lda     ram_E6,x                ;4        
    sta     ram_D9                  ;3        
    lda     ram_E3,x                ;4        
    sta     ram_E6,x                ;4        
    lda     ram_D9                  ;3        
    sta     ram_E3,x                ;4   =  76
L7ac3
    inx                             ;2        
    inx                             ;2        
    inx                             ;2        
    cpx     ram_D5                  ;3        
    bne     L7a99                   ;2/3      
    sec                             ;2        
    lda     ram_D5                  ;3        
    sbc     #$03                    ;2        
    sta     ram_D5                  ;3        
    bne     L7a97                   ;2/3      
    ldx     #$00                    ;2   =  25
L7ad5
    dec     ram_E2,x                ;6        
    inx                             ;2        
    inx                             ;2        
    inx                             ;2        
    cpx     #$15                    ;2        
    bne     L7ad5                   ;2/3      
    rts                             ;6   =  22
    
L7adf
    lda     #$ff                    ;2        
    sta     ram_A6                  ;3        
    sta     ram_AC                  ;3        
    sta     ram_AF                  ;3        
    ldy     #$00                    ;2        
    lda     ram_AE                  ;3        
    and     #$01                    ;2        
    beq     L7af3                   ;2/3      
    ldx     #$00                    ;2        
    beq     L7af5                   ;2/3 =  24
L7af3
    ldx     #$03                    ;2   =   2
L7af5
    lda     ram_E1,x                ;4        
    sta.wy  ram_9F,y                ;5        
    lda     ram_E2,x                ;4        
    sta.wy  ram_A0,y                ;5        
    lda     ram_E3,x                ;4        
    sta.wy  ram_A1,y                ;5        
    clc                             ;2        
    txa                             ;2        
    adc     #$0c                    ;2        
    tax                             ;2        
    iny                             ;2        
    iny                             ;2        
    iny                             ;2        
    cpy     #$03                    ;2        
    beq     L7af5                   ;2/3!     
    lda     ram_AE                  ;3        
    and     #$01                    ;2        
    beq     L7b1a                   ;2/3      
    ldx     #$06                    ;2        
    bne     L7b1c                   ;2/3 =  56
L7b1a
    ldx     #$09                    ;2   =   2
L7b1c
    lda     ram_E1,x                ;4        
    sta     ram_A8                  ;3        
    lda     ram_E2,x                ;4        
    sta     ram_A9                  ;3        
    lda     ram_E3,x                ;4        
    sta     ram_AA                  ;3        
    lda     ram_A1                  ;3        
    cmp     ram_9E                  ;3        
    bne     L7b45                   ;2/3      
    lda     ram_A3                  ;3        
    cmp     #$ff                    ;2        
    beq     L7b5b                   ;2/3      
    sec                             ;2        
    lda     ram_A0                  ;3        
    sbc     ram_A3                  ;3        
    cmp     #$04                    ;2        
    bcs     L7b5c                   ;2/3      
    sec                             ;2         *
    lda     ram_A3                  ;3         *
    sbc     #$03                    ;2         *
    sta     ram_A3                  ;3         *
    rts                             ;6   =  64 *
    
L7b45
    lda     ram_AA                  ;3        
    cmp     ram_9E                  ;3        
    bne     L7b5b                   ;2/3      
    lda     ram_A3                  ;3        
    cmp     ram_A9                  ;3        
    bcc     L7b53                   ;2/3      
    dec     ram_A3                  ;5   =  21
L7b53
    lda     ram_A9                  ;3        
    cmp     ram_A0                  ;3        
    bcc     L7b5b                   ;2/3      
    inc     ram_A0                  ;5   =  13
L7b5b
    rts                             ;6   =   6
    
L7b5c
    lda     ram_A3                  ;3        
    cmp     ram_A9                  ;3        
    bcc     L7b5b                   ;2/3      
    dec     ram_A3                  ;5         *
    rts                             ;6   =  19 *
    
L7b65
    lda.w   CXM0P                   ;4        
    and     #$c0                    ;2        
    beq     L7b71                   ;2/3      
    ldy     #$00                    ;2        
    jsr     L7c21                   ;6   =  16
L7b71
    lda.w   CXM1P                   ;4        
    and     #$c0                    ;2        
    beq     L7b7d                   ;2/3      
    ldy     #$03                    ;2        
    jsr     L7c21                   ;6   =  16
L7b7d
    sta     CXCLR                   ;3        
    ldx     #$00                    ;2   =   5
L7b81
    lda     ram_B2,x                ;4        
    cmp     #$81                    ;2        
    beq     L7ba5                   ;2/3      
    cmp     #$89                    ;2        
    beq     L7ba5                   ;2/3      
    sec                             ;2        
    lda     ram_B0,x                ;4        
    sbc     #$50                    ;2        
    cmp     #$07                    ;2        
    bcc     L7b98                   ;2/3      
    cmp     #$f9                    ;2        
    bcc     L7ba5                   ;2/3 =  28
L7b98
    sec                             ;2        
    lda     ram_B1,x                ;4        
    sbc     #$1e                    ;2        
    cmp     #$06                    ;2        
    bcc     L7baf                   ;2/3      
    cmp     #$fa                    ;2        
    bcs     L7baf                   ;2/3 =  16
L7ba5
    inx                             ;2        
    inx                             ;2        
    inx                             ;2        
    cpx     #$09                    ;2        
    bne     L7b81                   ;2/3      
    jmp     L7bc7                   ;3   =  13
    
L7baf
    lda     #$81                    ;2        
    sta     ram_B2,x                ;4        
    lda     #$41                    ;2        
    sta     ram_9E                  ;3        
    lda     #$aa                    ;2        
    sta     ram_8B                  ;3        
    lda     #$e0                    ;2        
    sta     ram_81                  ;3        
    dec     lives                   ;5        
    bne     L7bc7                   ;2/3      

   IF PLUSROM = 1
    jsr SendPlusROMScore
    nop
   ELSE
    lda     #$04                    ;2        
    sta     ram_A7                  ;3   =  33
   ENDIF


L7bc7
    ldy     #$00                    ;2        
    ldx     #$00                    ;2   =   4
L7bcb
    lda     ram_C4,x                ;4        
    cmp     #$b1                    ;2        
    beq     L7bf4                   ;2/3      
    cmp     #$b9                    ;2        
    beq     L7bf4                   ;2/3      
    sec                             ;2        
    lda     ram_C3,x                ;4        
    sbc     #$1e                    ;2        
    cmp     #$08                    ;2        
    bcc     L7be2                   ;2/3      
    cmp     #$f8                    ;2        
    bcc     L7bf4                   ;2/3 =  28
L7be2
    lda     ram_C2,x                ;4        
    cmp     #$4d                    ;2        
    bcs     L7bf4                   ;2/3      
    asl                             ;2        
    sec                             ;2        
    sbc     #$50                    ;2        
    cmp     #$0b                    ;2        
    bcc     L7c00                   ;2/3!     
    cmp     #$f5                    ;2        
    bcs     L7c00                   ;2/3!=  22
L7bf4
    iny                             ;2        
    iny                             ;2        
    inx                             ;2        
    inx                             ;2        
    inx                             ;2        
    cpx     #$12                    ;2        
    bne     L7bcb                   ;2/3      
    jmp     L7cd1                   ;3   =  17
    
L7c00
    lda     #$b1                    ;2         *
    sta     ram_C4,x                ;4         *
    lda     #$ff                    ;2         *
    sta.wy  ram_91,y                ;5         *
    lda     #$41                    ;2         *
    sta     ram_9E                  ;3         *
    lda     #$aa                    ;2         *
    sta     ram_8B                  ;3         *
    lda     #$e0                    ;2         *
    sta     ram_81                  ;3         *
    dec     lives                   ;5         *
    bne     L7c1e                   ;2/3       *

   IF PLUSROM = 1
    jmp SendPlusROMScore
    nop
   ELSE
    lda     #$04                    ;2        
    sta     ram_A7                  ;3   =  33
   ENDIF

    rts                             ;6   =  46 *
    
L7c1e
    jmp     L7cd1                   ;3   =   3 *
    
L7c21
    ldx     #$00                    ;2   =   2
L7c23
    lda     ram_B2,x                ;4        
    cmp     #$81                    ;2        
    beq     L7c49                   ;2/3      
    cmp     #$89                    ;2        
    beq     L7c49                   ;2/3      
    sec                             ;2        
    lda     ram_B0,x                ;4        
    sbc.wy  ram_B9,y                ;4        
    cmp     #$08                    ;2        
    bcc     L7c3b                   ;2/3      
    cmp     #$f8                    ;2        
    bcc     L7c49                   ;2/3 =  30
L7c3b
    sec                             ;2        
    lda     ram_B1,x                ;4        
    sbc.wy  ram_BA,y                ;4        
    cmp     #$07                    ;2        
    bcc     L7c53                   ;2/3      
    cmp     #$f9                    ;2        
    bcs     L7c53                   ;2/3 =  18
L7c49
    inx                             ;2        
    inx                             ;2        
    inx                             ;2        
    cpx     #$09                    ;2        
    bne     L7c23                   ;2/3      
    jmp     L7c75                   ;3   =  13
    
L7c53
    lda     #$81                    ;2        
    sta     ram_B2,x                ;4        
    lda     #$ff                    ;2        
    sta.wy  ram_BA,y                ;5        
    lda     ram_8B                  ;3        
    cmp     #$82                    ;2        
    bcs     L7c66                   ;2/3      
    lda     #$88                    ;2        
    sta     ram_8B                  ;3   =  25
L7c66
    lda     #$01                    ;2        
    clc                             ;2        
    sed                             ;2        
    adc     ram_C0                  ;3        
    sta     ram_C0                  ;3        
    lda     ram_BF                  ;3        
    adc     #$00                    ;2        
    sta     ram_BF                  ;3        
    cld                             ;2   =  22
L7c75
    ldx     #$00                    ;2        
    stx     ram_D9                  ;3   =   5
L7c79
    lda     ram_C4,x                ;4        
    cmp     #$b1                    ;2        
    beq     L7ca4                   ;2/3      
    cmp     #$b9                    ;2        
    beq     L7ca4                   ;2/3      
    sec                             ;2        
    lda     ram_C3,x                ;4        
    sbc.wy  ram_BA,y                ;4        
    cmp     #$09                    ;2        
    bcc     L7c91                   ;2/3      
    cmp     #$f9                    ;2        
    bcc     L7ca4                   ;2/3 =  30
L7c91
    lda     ram_C2,x                ;4        
    cmp     #$4d                    ;2        
    bcs     L7ca4                   ;2/3      
    asl                             ;2        
    sec                             ;2        
    sbc.wy  ram_B9,y                ;4        
    cmp     #$11                    ;2        
    bcc     L7cb0                   ;2/3      
    cmp     #$f0                    ;2        
    bcs     L7cb0                   ;2/3 =  24
L7ca4
    inc     ram_D9                  ;5        
    inc     ram_D9                  ;5        
    inx                             ;2        
    inx                             ;2        
    inx                             ;2        
    cpx     #$12                    ;2        
    bne     L7c79                   ;2/3      
    rts                             ;6   =  26
    
L7cb0
    lda     #$b1                    ;2        
    sta     ram_C4,x                ;4        
    lda     #$ff                    ;2        
    sta.wy  ram_BA,y                ;5        
    ldx     ram_D9                  ;3        
    sta     ram_91,x                ;4        
    lda     #$aa                    ;2        
    sta     ram_8B                  ;3        
    lda     #$05                    ;2        
    clc                             ;2        
    sed                             ;2        
    adc     ram_C0                  ;3        
    sta     ram_C0                  ;3        
    lda     ram_BF                  ;3        
    adc     #$00                    ;2        
    sta     ram_BF                  ;3        
    cld                             ;2        
    rts                             ;6   =  53
    
L7cd1
    ldx     #$00                    ;2   =   2
L7cd3
    lda     ram_91,x                ;4        
    cmp     #$ff                    ;2        
    bne     L7cf3                   ;2/3      
    inx                             ;2        
    inx                             ;2        
    cpx     #$0c                    ;2        
    bne     L7cd3                   ;2/3      
    sta     ram_A0                  ;3         *
    inc     ram_A5                  ;5         *
    lda     ram_A5                  ;3         *
    cmp     #$07                    ;2         *
    bne     L7ced                   ;2/3       *
    lda     #$01                    ;2         *
    sta     ram_A5                  ;3   =  36 *
L7ced
    inc     lives                   ;5         *
    lda     #$c7                    ;2         *
    sta     ram_82                  ;3   =  10 *
L7cf3
    rts                             ;6   =   6
    
L7cf4
    lda     ram_8B                  ;3        
    cmp     #$a0                    ;2        
    bcs     L7d50                   ;2/3!     
    cmp     #$80                    ;2        
    bcs     L7d26                   ;2/3!     
    rts                             ;6   =  17
    
L7cff
    cmp     #$44                    ;2        
    bne     L7d0b                   ;2/3      
    lda     #$07                    ;2        
    sta     ram_8A                  ;3        
    lda     #$41                    ;2        
    sta     ram_8B                  ;3   =  14
L7d0b
    lda     #$04                    ;2        
    sta     AUDC1                   ;3        
    lda     #$08                    ;2        
    sta     AUDV1                   ;3        
    inc     ram_8A                  ;5        
    inc     ram_8A                  ;5        
    lda     ram_8A                  ;3        
    cmp     #$18                    ;2        
    bcc     L7d23                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_8B                  ;3        
    sta     AUDV1                   ;3   =  35
L7d23
    sta     AUDF1                   ;3        
    rts                             ;6   =   9
    
L7d26
    cmp     #$88                    ;2        
    bne     L7d32                   ;2/3      
    lda     #$04                    ;2        
    sta     ram_89                  ;3        
    lda     #$81                    ;2        
    sta     ram_8B                  ;3   =  14
L7d32
    lda     #$04                    ;2        
    sta     AUDC1                   ;3        
    ldx     ram_89                  ;3        
    lda     L7d4c,x                 ;4        
    sta     AUDF1                   ;3        
    lda     #$08                    ;2        
    sta     AUDV1                   ;3        
    dec     ram_89                  ;5        
    bpl     L7d4b                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_8B                  ;3        
    sta     AUDV1                   ;3   =  35
L7d4b
    rts                             ;6   =   6
    
L7d4c
    .byte   $0f                             ; $7d4c (A)
    .byte   $0e                             ; $7d4d (A)
    .byte   $13                             ; $7d4e (A)
    .byte   $11                             ; $7d4f (A)
    
L7d50
    cmp     #$aa                    ;2        
    bne     L7d60                   ;2/3      
    lda     #$a1                    ;2        
    sta     ram_8B                  ;3        
    lda     #$10                    ;2        
    sta     ram_86                  ;3        
    lda     #$1f                    ;2        
    sta     ram_87                  ;3   =  19
L7d60
    lda     #$08                    ;2        
    sta     AUDC0                   ;3        
    lda     #$03                    ;2        
    sta     AUDC1                   ;3        
    inc     ram_F7                  ;5        
    lda     ram_86                  ;3        
    cmp     #$09                    ;2        
    bcs     L7d93                   ;2/3      
    lda     ram_F7                  ;3        
    and     #$03                    ;2        
    beq     L7d79                   ;2/3      
    jmp     L7da4                   ;3   =  32
    
L7d79
    lda     ram_86                  ;3        
    cmp     #$04                    ;2        
    bcs     L7d85                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_8B                  ;3        
    beq     L7da0                   ;2/3 =  14
L7d85
    sec                             ;2        
    sbc     #$01                    ;2        
    sta     ram_86                  ;3        
    sta     AUDV0                   ;3        
    and     #$01                    ;2        
    sta     AUDV1                   ;3        
    jmp     L7da4                   ;3   =  18
    
L7d93
    lda     ram_F7                  ;3        
    and     #$01                    ;2        
    beq     L7da4                   ;2/3      
    sec                             ;2        
    lda     ram_86                  ;3        
    sbc     #$01                    ;2        
    sta     ram_86                  ;3   =  17
L7da0
    sta     AUDV0                   ;3        
    sta     AUDV1                   ;3   =   6
L7da4
    sec                             ;2        
    lda     ram_87                  ;3        
    sbc     #$01                    ;2        
    sta     ram_87                  ;3        
    sta     AUDF0                   ;3        
    lda     #$15                    ;2        
    sta     AUDF1                   ;3        
    rts                             ;6   =  24
    
; start of unused code?
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $7db2 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$44     ; $d0 ; $7dba (*)
;    .byte   $08,$a9,$07,$85,$8a,$a9,$41,$85 ; $7dc2 (*)
;    .byte   $8b,$a9,$04,$85,$16,$a9,$08,$85 ; $7dca (*)
;    .byte   $1a,$e6,$8a,$e6,$8a,$a5,$8a,$c9 ; $7dd2 (*)
;    .byte   $18,$90,$06,$a9,$00,$85,$8b,$85 ; $7dda (*)
;    .byte   $1a,$85,$18,$60,$c9,$88,$d0,$08 ; $7de2 (*)
;    .byte   $a9,$04,$85,$89,$a9,$81,$85,$8b ; $7dea (*)
;    .byte   $a9,$04,$85,$16,$a6,$89,$bd,$4c ; $7df2 (*)
;    .byte   $7d,$85,$18,$a9,$08,$85,$ff,$ff ; $7dfa (*)
L7dc1
    bne L7dcb
    lda #$07
    sta $8a
    lda #$41
    sta $8b
L7dcb
    lda #$04
    sta $16
    lda #$08
    sta $1a
    inc $8a
    inc $8a
    lda $8a
    cmp #$18
    bcc L7de3
    lda #$00
    sta $8b
    sta $1a
L7de3
    sta $18
    rts

    cmp #$88
    bne L7df2
    lda #$04
    sta $89
    lda #$81
    sta $8b
L7df2
    lda #$04
    sta $16
    ldx $89
    lda L7d4c,x
    sta $18
    lda #$08
    sta $ff

    .byte $ff
; end of unused code?
 

    .byte   %00000000 ; |        |            $7e02 (G)
    .byte   %00001101 ; |    ## #|            $7e03 (G)
    .byte   %00011010 ; |   ## # |            $7e04 (G)
    .byte   %00110111 ; |  ## ###|            $7e05 (G)
    .byte   %11100101 ; |###  # #|            $7e06 (G)
    .byte   %00110111 ; |  ## ###|            $7e07 (G)
    .byte   %00011010 ; |   ## # |            $7e08 (G)
    .byte   %00001101 ; |    ## #|            $7e09 (G)
    .byte   %00000000 ; |        |            $7e0a (G)
    .byte   %10101100 ; |# # ##  |            $7e0b (G)
    .byte   %11011110 ; |## #### |            $7e0c (G)
    .byte   %01010110 ; | # # ## |            $7e0d (G)
    .byte   %01101100 ; | ## ##  |            $7e0e (G)
    .byte   %01110010 ; | ###  # |            $7e0f (G)
    .byte   %11111100 ; |######  |            $7e10 (G)
    .byte   %11000110 ; |##   ## |            $7e11 (G)
    .byte   %00000000 ; |        |            $7e12 (G)
    .byte   %10111010 ; |# ### # |            $7e13 (G)
    .byte   %01101100 ; | ## ##  |            $7e14 (G)
    .byte   %10111010 ; |# ### # |            $7e15 (G)
    .byte   %11000110 ; |##   ## |            $7e16 (G)
    .byte   %01101100 ; | ## ##  |            $7e17 (G)
    .byte   %00111000 ; |  ###   |            $7e18 (G)
    .byte   %00010000 ; |   #    |            $7e19 (G)
    .byte   %00000000 ; |        |            $7e1a (G)
    .byte   %01101010 ; | ## # # |            $7e1b (G)
    .byte   %11110110 ; |#### ## |            $7e1c (G)
    .byte   %11010100 ; |## # #  |            $7e1d (G)
    .byte   %01101100 ; | ## ##  |            $7e1e (G)
    .byte   %10011100 ; |#  ###  |            $7e1f (G)
    .byte   %01111110 ; | ###### |            $7e20 (G)
    .byte   %11000110 ; |##   ## |            $7e21 (G)
    .byte   %00000000 ; |        |            $7e22 (G)
    .byte   %10110000 ; |# ##    |            $7e23 (G)
    .byte   %01011000 ; | # ##   |            $7e24 (G)
    .byte   %11101100 ; |### ##  |            $7e25 (G)
    .byte   %10100111 ; |# #  ###|            $7e26 (G)
    .byte   %11101100 ; |### ##  |            $7e27 (G)
    .byte   %01011000 ; | # ##   |            $7e28 (G)
    .byte   %10110000 ; |# ##    |            $7e29 (G)
    .byte   %00000000 ; |        |            $7e2a (G)
    .byte   %11000110 ; |##   ## |            $7e2b (G)
    .byte   %01111110 ; | ###### |            $7e2c (G)
    .byte   %10011100 ; |#  ###  |            $7e2d (G)
    .byte   %01101100 ; | ## ##  |            $7e2e (G)
    .byte   %11010100 ; |## # #  |            $7e2f (G)
    .byte   %11110110 ; |#### ## |            $7e30 (G)
    .byte   %01101010 ; | ## # # |            $7e31 (G)
    .byte   %00000000 ; |        |            $7e32 (G)
    .byte   %00010000 ; |   #    |            $7e33 (G)
    .byte   %00111000 ; |  ###   |            $7e34 (G)
    .byte   %01101100 ; | ## ##  |            $7e35 (G)
    .byte   %11000110 ; |##   ## |            $7e36 (G)
    .byte   %10111010 ; |# ### # |            $7e37 (G)
    .byte   %01101100 ; | ## ##  |            $7e38 (G)
    .byte   %10111010 ; |# ### # |            $7e39 (G)
    .byte   %00000000 ; |        |            $7e3a (G)
    .byte   %11000110 ; |##   ## |            $7e3b (G)
    .byte   %11111100 ; |######  |            $7e3c (G)
    .byte   %01110010 ; | ###  # |            $7e3d (G)
    .byte   %01101100 ; | ## ##  |            $7e3e (G)
    .byte   %01010110 ; | # # ## |            $7e3f (G)
    .byte   %11011110 ; |## #### |            $7e40 (G)
    .byte   %10101100 ; |# # ##  |            $7e41 (G)
    .byte   %00000000 ; |        |            $7e42 (G)
    .byte   %00000000 ; |        |            $7e43 (G)
    .byte   %01111110 ; | ###### |            $7e44 (G)
    .byte   %11111111 ; |########|            $7e45 (G)
    .byte   %11011011 ; |## ## ##|            $7e46 (G)
    .byte   %01100110 ; | ##  ## |            $7e47 (G)
    .byte   %00111100 ; |  ####  |            $7e48 (G)
    .byte   %00011000 ; |   ##   |            $7e49 (G)
    .byte   %00000000 ; |        |            $7e4a (G)
    .byte   %00000000 ; |        |            $7e4b (G)
    .byte   %00000111 ; |     ###|            $7e4c (G)
    .byte   %00011110 ; |   #### |            $7e4d (G)
    .byte   %00111111 ; |  ######|            $7e4e (G)
    .byte   %01111110 ; | ###### |            $7e4f (G)
    .byte   %01111101 ; | ##### #|            $7e50 (G)
    .byte   %11101010 ; |### # # |            $7e51 (G)
    .byte   %00000000 ; |        |            $7e52 (G)
    .byte   %01110000 ; | ###    |            $7e53 (G)
    .byte   %11111000 ; |#####   |            $7e54 (G)
    .byte   %11110100 ; |#### #  |            $7e55 (G)
    .byte   %11111010 ; |##### # |            $7e56 (G)
    .byte   %11110100 ; |#### #  |            $7e57 (G)
    .byte   %11111000 ; |#####   |            $7e58 (G)
    .byte   %01110000 ; | ###    |            $7e59 (G)
    .byte   %00000000 ; |        |            $7e5a (G)
    .byte   %00000000 ; |        |            $7e5b (G)
    .byte   %11101010 ; |### # # |            $7e5c (G)
    .byte   %01111101 ; | ##### #|            $7e5d (G)
    .byte   %01111110 ; | ###### |            $7e5e (G)
    .byte   %00111111 ; |  ######|            $7e5f (G)
    .byte   %00011110 ; |   #### |            $7e60 (G)
    .byte   %00000111 ; |     ###|            $7e61 (G)
    .byte   %00000000 ; |        |            $7e62 (G)
    .byte   %00000000 ; |        |            $7e63 (G)
    .byte   %00011000 ; |   ##   |            $7e64 (G)
    .byte   %00111100 ; |  ####  |            $7e65 (G)
    .byte   %01100110 ; | ##  ## |            $7e66 (G)
    .byte   %11011011 ; |## ## ##|            $7e67 (G)
    .byte   %11111111 ; |########|            $7e68 (G)
    .byte   %01111110 ; | ###### |            $7e69 (G)
    .byte   %00000000 ; |        |            $7e6a (G)
    .byte   %00000000 ; |        |            $7e6b (G)
    .byte   %01010111 ; | # # ###|            $7e6c (G)
    .byte   %10111110 ; |# ##### |            $7e6d (G)
    .byte   %01111110 ; | ###### |            $7e6e (G)
    .byte   %11111100 ; |######  |            $7e6f (G)
    .byte   %01111000 ; | ####   |            $7e70 (G)
    .byte   %11100000 ; |###     |            $7e71 (G)
    .byte   %00000000 ; |        |            $7e72 (G)
    .byte   %00011100 ; |   ###  |            $7e73 (G)
    .byte   %00111110 ; |  ##### |            $7e74 (G)
    .byte   %01011110 ; | # #### |            $7e75 (G)
    .byte   %10111110 ; |# ##### |            $7e76 (G)
    .byte   %01011110 ; | # #### |            $7e77 (G)
    .byte   %00111110 ; |  ##### |            $7e78 (G)
    .byte   %00011100 ; |   ###  |            $7e79 (G)
    .byte   %00000000 ; |        |            $7e7a (G)
    .byte   %00000000 ; |        |            $7e7b (G)
    .byte   %11100000 ; |###     |            $7e7c (G)
    .byte   %01111000 ; | ####   |            $7e7d (G)
    .byte   %11111100 ; |######  |            $7e7e (G)
    .byte   %01111110 ; | ###### |            $7e7f (G)
    .byte   %10111110 ; |# ##### |            $7e80 (G)
    .byte   %01010111 ; | # # ###|            $7e81 (G)
    .byte   %00000000 ; |        |            $7e82 (G)
    .byte   %00000000 ; |        |            $7e83 (G)
    .byte   %00000000 ; |        |            $7e84 (G)
    .byte   %00000000 ; |        |            $7e85 (G)
    .byte   %00010000 ; |   #    |            $7e86 (G)
    .byte   %00101000 ; |  # #   |            $7e87 (G)
    .byte   %00010000 ; |   #    |            $7e88 (G)
    .byte   %00000000 ; |        |            $7e89 (G)
    .byte   %00000000 ; |        |            $7e8a (G)
    .byte   %00000000 ; |        |            $7e8b (G)
    .byte   %10000100 ; |#    #  |            $7e8c (G)
    .byte   %00110000 ; |  ##    |            $7e8d (G)
    .byte   %01111000 ; | ####   |            $7e8e (G)
    .byte   %01111000 ; | ####   |            $7e8f (G)
    .byte   %00110000 ; |  ##    |            $7e90 (G)
    .byte   %10000100 ; |#    #  |            $7e91 (G)
    .byte   %00000000 ; |        |            $7e92 (G)
    .byte   %01011010 ; | # ## # |            $7e93 (G)
    .byte   %00111100 ; |  ####  |            $7e94 (G)
    .byte   %11110111 ; |#### ###|            $7e95 (G)
    .byte   %01111110 ; | ###### |            $7e96 (G)
    .byte   %11101111 ; |### ####|            $7e97 (G)
    .byte   %00111100 ; |  ####  |            $7e98 (G)
    .byte   %01011010 ; | # ## # |            $7e99 (G)
    .byte   %00000000 ; |        |            $7e9a (G)
    .byte   %00011000 ; |   ##   |            $7e9b (G)
    .byte   %01111110 ; | ###### |            $7e9c (G)
    .byte   %11101111 ; |### ####|            $7e9d (G)
    .byte   %01111110 ; | ###### |            $7e9e (G)
    .byte   %11110111 ; |#### ###|            $7e9f (G)
    .byte   %01111110 ; | ###### |            $7ea0 (G)
    .byte   %00011000 ; |   ##   |            $7ea1 (G)
    .byte   %00000000 ; |        |            $7ea2 (G)
    .byte   $18,$3c,$b5,$ff,$ad,$3c,$18
    .byte   %00000000 ; |        |            $7eaa (G)
    .byte   $18,$3c,$6e,$7e,$76,$3c,$18
    .byte   %00000000 ; |        |            $7eb2 (G)
    .byte   %10010010 ; |#  #  # |            $7eb3 (G)
    .byte   %01000100 ; | #   #  |            $7eb4 (G)
    .byte   %00010000 ; |   #    |            $7eb5 (G)
    .byte   %10111010 ; |# ### # |            $7eb6 (G)
    .byte   %00010000 ; |   #    |            $7eb7 (G)
    .byte   %01000100 ; | #   #  |            $7eb8 (G)
    .byte   %10010010 ; |#  #  # |            $7eb9 (G)
    .byte   %00000000 ; |        |            $7eba (G)
    .byte   %00000000 ; |        |            $7ebb (G)
    .byte   %00001000 ; |    #   |            $7ebc (G)
    .byte   %00000000 ; |        |            $7ebd (G)
    .byte   %01000001 ; | #     #|            $7ebe (G)
    .byte   %00000000 ; |        |            $7ebf (G)
    .byte   %00001000 ; |    #   |            $7ec0 (G)
    .byte   %00000000 ; |        |            $7ec1 (G)
    
L7ec2
    sec                             ;2        
    sta     WSYNC                   ;3   =   5
;---------------------------------------
L7ec5
    sbc     #$0f                    ;2        
    bcs     L7ec5                   ;2/3      
    eor     #$07                    ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    sta     HMP0,x                  ;4        
    sta     RESP0,x                 ;4        
    rts                             ;6   =  28
    
   IF PLUSROM = 1
SendPlusROMScore
    lda ram_BF                     ; Score Hi BCD
    sta WriteToBuffer              ; 
    lda ram_C0                     ; Score Mid BCD
    sta WriteToBuffer              ; 
    lda ram_C1                     ; Score Lo BCD
    sta WriteToBuffer              ; 
    lda #HIGHSCORE_ID              ; game id in Highscore DB
    sta WriteSendBuffer
    lda     #$04
    sta     ram_A7
    rts
PlusROM_API
       .byte "a", 0, "h.firmaplus.de", 0

   ELSE
    .byte   $00,$24,$ff,$76,$ff,$6e,$ff,$24 ; $7ed4 (*)
    .byte   $00,$c3,$7e,$ef,$7e,$f7,$7e,$c3 ; $7edc (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $7ee4 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $7eec (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $7ef4 (*)
    .byte   $ff,$ff,$ff,$ff                 ; $7efc (*)
   ENDIF

     ORG $7f00

    .byte   $00                             ; $7f00 (*)
    
    .byte   %00000000 ; |        |            $7f01 (G)
    .byte   %00000000 ; |        |            $7f02 (G)
    .byte   %00000000 ; |        |            $7f03 (G)
    .byte   %00000000 ; |        |            $7f04 (G)
    .byte   %00000000 ; |        |            $7f05 (G)
    .byte   %00000000 ; |        |            $7f06 (G)
    .byte   %00000000 ; |        |            $7f07 (G)
    
    .byte   $00                             ; $7f08 (*)
    
    .byte   %00111100 ; |  ####  |            $7f09 (G)
    .byte   %01100110 ; | ##  ## |            $7f0a (G)
    .byte   %01100110 ; | ##  ## |            $7f0b (G)
    .byte   %01100110 ; | ##  ## |            $7f0c (G)
    .byte   %01100110 ; | ##  ## |            $7f0d (G)
    .byte   %01100110 ; | ##  ## |            $7f0e (G)
    .byte   %00111100 ; |  ####  |            $7f0f (G)
    .byte   %00000000 ; |        |            $7f10 (G)
    .byte   %00111100 ; |  ####  |            $7f11 (G)
    .byte   %00011000 ; |   ##   |            $7f12 (G)
    .byte   %00011000 ; |   ##   |            $7f13 (G)
    .byte   %00011000 ; |   ##   |            $7f14 (G)
    .byte   %00011000 ; |   ##   |            $7f15 (G)
    .byte   %00111000 ; |  ###   |            $7f16 (G)
    .byte   %00011000 ; |   ##   |            $7f17 (G)
    
    .byte   $00                             ; $7f18 (*)
    
    .byte   %01111110 ; | ###### |            $7f19 (G)
    .byte   %01100000 ; | ##     |            $7f1a (G)
    .byte   %01100000 ; | ##     |            $7f1b (G)
    .byte   %00111100 ; |  ####  |            $7f1c (G)
    .byte   %00000110 ; |     ## |            $7f1d (G)
    .byte   %01000110 ; | #   ## |            $7f1e (G)
    .byte   %00111100 ; |  ####  |            $7f1f (G)
    
    .byte   $00                             ; $7f20 (*)
    
    .byte   %00111100 ; |  ####  |            $7f21 (G)
    .byte   %01000110 ; | #   ## |            $7f22 (G)
    .byte   %00000110 ; |     ## |            $7f23 (G)
    .byte   %00011100 ; |   ###  |            $7f24 (G)
    .byte   %00000110 ; |     ## |            $7f25 (G)
    .byte   %01000110 ; | #   ## |            $7f26 (G)
    .byte   %00111100 ; |  ####  |            $7f27 (G)
    
    .byte   $00                             ; $7f28 (*)
    
    .byte   %00001100 ; |    ##  |            $7f29 (G)
    .byte   %00001100 ; |    ##  |            $7f2a (G)
    .byte   %01111110 ; | ###### |            $7f2b (G)
    .byte   %01101100 ; | ## ##  |            $7f2c (G)
    .byte   %00111100 ; |  ####  |            $7f2d (G)
    .byte   %00011100 ; |   ###  |            $7f2e (G)
    .byte   %00001100 ; |    ##  |            $7f2f (G)
    
    .byte   $00                             ; $7f30 (*)
    
    .byte   %01111100 ; | #####  |            $7f31 (G)
    .byte   %00000110 ; |     ## |            $7f32 (G)
    .byte   %00000110 ; |     ## |            $7f33 (G)
    .byte   %01111100 ; | #####  |            $7f34 (G)
    .byte   %01100000 ; | ##     |            $7f35 (G)
    .byte   %01100000 ; | ##     |            $7f36 (G)
    .byte   %01111110 ; | ###### |            $7f37 (G)
    
    .byte   $00                             ; $7f38 (*)
    
    .byte   %00111100 ; |  ####  |            $7f39 (G)
    .byte   %01100110 ; | ##  ## |            $7f3a (G)
    .byte   %01100110 ; | ##  ## |            $7f3b (G)
    .byte   %01111100 ; | #####  |            $7f3c (G)
    .byte   %01100000 ; | ##     |            $7f3d (G)
    .byte   %01100010 ; | ##   # |            $7f3e (G)
    .byte   %00111100 ; |  ####  |            $7f3f (G)
    
    .byte   $00                             ; $7f40 (*)
    
    .byte   %00011000 ; |   ##   |            $7f41 (G)
    .byte   %00011000 ; |   ##   |            $7f42 (G)
    .byte   %00011000 ; |   ##   |            $7f43 (G)
    .byte   %00001100 ; |    ##  |            $7f44 (G)
    .byte   %00000110 ; |     ## |            $7f45 (G)
    .byte   %01100110 ; | ##  ## |            $7f46 (G)
    .byte   %01111110 ; | ###### |            $7f47 (G)
    
    .byte   $00                             ; $7f48 (*)
    
    .byte   %00111100 ; |  ####  |            $7f49 (G)
    .byte   %01100110 ; | ##  ## |            $7f4a (G)
    .byte   %01100110 ; | ##  ## |            $7f4b (G)
    .byte   %00111100 ; |  ####  |            $7f4c (G)
    .byte   %01100110 ; | ##  ## |            $7f4d (G)
    .byte   %01100110 ; | ##  ## |            $7f4e (G)
    .byte   %00111100 ; |  ####  |            $7f4f (G)
    
    .byte   $00                             ; $7f50 (*)
    
    .byte   %00111100 ; |  ####  |            $7f51 (G)
    .byte   %01000110 ; | #   ## |            $7f52 (G)
    .byte   %00000110 ; |     ## |            $7f53 (G)
    .byte   %00111110 ; |  ##### |            $7f54 (G)
    .byte   %01100110 ; | ##  ## |            $7f55 (G)
    .byte   %01100110 ; | ##  ## |            $7f56 (G)
    .byte   %00111100 ; |  ####  |            $7f57 (G)
    
L7f58
    lda     ram_8C                  ;3        
    and     #$17                    ;2        
    cmp     #$17                    ;2        
    beq     L7f61                   ;2/3      
    rts                             ;6   =  15
    
L7f61
    inc     ram_8D                  ;5        
    lda     ram_8D                  ;3        
    cmp     #$20                    ;2        
    bcc     L7f7f                   ;2/3      
    cmp     #$40                    ;2        
    bcc     L7f83                   ;2/3      
    cmp     #$58                    ;2        
    bcc     L7f87                   ;2/3      
    cmp     #$80                    ;2         *
    bcc     L7f7f                   ;2/3       *
    cmp     #$a0                    ;2         *
    bcc     L7f83                   ;2/3       *
    cmp     #$b8                    ;2         *
    bcc     L7f87                   ;2/3       *
    bcs     L7f7f                   ;2/3 =  34 *
L7f7f
    lda     #COL_GROUND_A           ;2          ground background (ram_8F)
    bne     L7f89                   ;2/3 =   4
L7f83
    lda     #COL_GROUND_B           ;2        
    bne     L7f89                   ;2/3 =   4
L7f87
    lda     #COL_GROUND_C           ;2   =   2
L7f89
    sta     ram_8F                  ;3        
    ldy     #$03                    ;2        
    sty     ram_DD                  ;3        
    ldy     #$00                    ;2        
    ldx     #$06                    ;2   =  12
L7f93
    lda     ram_B1,x                ;4        
    cmp     #$ff                    ;2        
    beq     L7f9a                   ;2/3      
    iny                             ;2   =  10
L7f9a
    dex                             ;2        
    dex                             ;2        
    dex                             ;2        
    bpl     L7f93                   ;2/3      
    cpy     ram_DD                  ;3        
    bcs     L7fee                   ;2/3 =  13
L7fa3
    inx                             ;2        
    inx                             ;2        
    inx                             ;2        
    lda     ram_B1,x                ;4        
    cmp     #$ff                    ;2        
    bne     L7fa3                   ;2/3      
    lda     ram_8C                  ;3        
    and     #$3f                    ;2        
    cmp     #$3c                    ;2        
    bcc     L7fb6                   ;2/3      
    lda     #$27                    ;2   =  25
L7fb6
    sta     ram_B1,x                ;4        
    lda     ram_8C                  ;3        
    cmp     #$95                    ;2        
    bcc     L7fc0                   ;2/3      
    and     #$7f                    ;2   =  13
L7fc0
    sta     ram_B0,x                ;4        
    lda     ram_8C                  ;3        
    and     #$03                    ;2        
    beq     L7fe6                   ;2/3      
    cmp     #$02                    ;2        
    beq     L7fde                   ;2/3      
    bcc     L7fd6                   ;2/3      
    lda     #$02                    ;2        
    sta     ram_B0,x                ;4        
    lda     #$61                    ;2        
    bne     L7fec                   ;2/3 =  27
L7fd6
    lda     #$01                    ;2         *
    sta     ram_B1,x                ;4         *
    lda     #$51                    ;2         *
    bne     L7fec                   ;2/3 =  10 *
L7fde
    lda     #$94                    ;2         *
    sta     ram_B0,x                ;4         *
    lda     #$41                    ;2         *
    bne     L7fec                   ;2/3 =  10 *
L7fe6
    lda     #$3b                    ;2         *
    sta     ram_B1,x                ;4         *
    lda     #$71                    ;2   =   8 *
L7fec
    sta     ram_B2,x                ;4   =   4
L7fee
    rts                             ;6   =   6
    
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $7fef (*)
    .byte   $ff,$ff,$ff                     ; $7ff7 (*)


   IF PLUSROM = 1
    .word (PlusROM_API - $6000)           ; PlusRom API pointer
   ELSE
    .word   $ffff                         ; NMI
   ENDIF
    .word   Start                         ; start
    .word   Start                         ; brk
