; ***  N I N J I S H   G U Y   R C 3 ***
; Copyright 2021 Vladimir Zúñiga (VHZC)
; Game Design and Programming: Vladimir Zúñiga (VHZC)
; Disassembled 07/31/23 12:20:27
; Using Stella 6.6

; Analyzed, labeled and commented
;  by Wolfgang Stubig (Al_Nafuur)
; PlusROM HSC and PAL60 color hack added
;  by Wolfgang Stubig (Al_Nafuur)

PLUSROM = 1
PAL60   = 0 ; might be not complete!

;
; ROM properties name : NinjishGuyRC3
; ROM properties MD5  : 9e25d77f17008847d789d68bd1948ce1
; Bankswitch type     : F4* (32K) 
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
;      Color constants
;-----------------------------------------------------------

  IF PAL60
BLACK            = $00
YELLOW           = $20
BROWN            = $20
ORANGE           = $40
RED              = $60
MAUVE            = $80
VIOLET           = $a0
PURPLE           = $c0
BLUE             = $d0
BLUE_CYAN        = $b0
CYAN             = $90
CYAN_GREEN       = $70
GREEN            = $50
GREEN_YELLOW     = $30
GREEN_BEIGE      = $30
BEIGE            = $20
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

CXM0P           = $00  ; (R)
CXM1P           = $01  ; (R)
CXP0FB          = $02  ; (R)
CXP1FB          = $03  ; (R)
CXM0FB          = $04  ; (R)
CXM1FB          = $05  ; (R)
CXBLPF          = $06  ; (R)
CXPPMM          = $07  ; (R)
;INPT0          = $08  ; (Ri)
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
HMP1            = $21  ; (W)
;HMM0           = $22  ; (Wi)
;HMM1           = $23  ; (Wi)
;HMBL           = $24  ; (Wi)
VDELP0          = $25  ; (W)
VDELP1          = $26  ; (W)
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
;                 $a5  (i)
;                 $a6  (i)
;                 $a7  (i)
;                 $a8  (i)
;                 $a9  (i)
;                 $aa  (i)
;                 $ab  (i)
;                 $ac  (i)
;                 $ad  (i)
;                 $ae  (i)
;                 $af  (i)
;                 $b0  (i)
;                 $b1  (i)
;                 $b2  (i)
;                 $b3  (i)
;                 $b4  (i)
;                 $b5  (i)
;                 $b6  (i)
;                 $b7  (i)
;                 $b8  (i)
;                 $b9  (i)
;                 $ba  (i)
;                 $bb  (i)
;                 $bc  (i)
;                 $bd  (i)
;                 $be  (i)
;                 $bf  (i)
;                 $c0  (i)
;                 $c1  (i)
;                 $c2  (i)
;                 $c3  (i)
;                 $c4  (i)
;                 $c5  (i)
;                 $c6  (i)
;                 $c7  (i)
;                 $c8  (i)
;                 $c9  (i)
;                 $ca  (i)
;                 $cb  (i)
;                 $cc  (i)
;                 $cd  (i)
;                 $ce  (i)
;                 $cf  (i)
ram_D0          = $d0; (s)
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
ram_F2          = $f2
ram_F3          = $f3
ram_F4          = $f4

ram_F6          = $f6; (s)
ram_F7          = $f7; (s)
;                 $f8  (s)
;                 $f9  (s)
;                 $fa  (s)
;                 $fb  (is)
;                 $fc  (s)
;                 $fd  (is)
;                 $fe  (s)
;                 $ff  (is)


;-----------------------------------------------------------
;      User Defined Labels
;-----------------------------------------------------------

   IF PLUSROM = 1

WriteToBuffer           = $1FF0
WriteSendBuffer         = $1FF1
ReceiveBuffer           = $1FF2
ReceiveBufferSize       = $1FF3

HIGHSCORE_ID            = 71         ; Ninjish Guy game ID in Highscore DB

   ENDIF


;***********************************************************
;      Bank 0 / 0..7
;***********************************************************

    SEG     CODE
    ORG     $0000
    RORG    $1000

    sty     ram_9C                  ;3         *
    sta     ram_9D                  ;3         *
    ldx     #$08                    ;2         *
    dec     ram_9D                  ;5   =  13 *
L1008
    lsr                             ;2         *
    ror     ram_9C                  ;5         *
    bcc     L100f                   ;2/3       *
    adc     ram_9D                  ;3   =  12 *
L100f
    dex                             ;2         *
    bne     L1008                   ;2/3       *
    jmp     Lffdd                   ;3   =   7 *
    
    sty ram_9C
    ldx #$08
l1019
    cmp ram_9C
    bcc l1027
    sbc ram_9C
    rol ram_9D
    rol
    dex
    bne l1019
    beq l102d
l1027
    rol ram_9D
    rol
    dex
    bne l1019
l102d
    sta ram_9C
    lda ram_9D
    jmp     Lffdd                   ;3
    
L1034
    sta     ram_EE                  ;3        
    lda     #$6f                    ;2        
    pha                             ;3        
    lda     #$ff                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$04                    ;2        
    jmp     Lffeb                   ;3   =  29
    
L1046
    sta     ram_EE                  ;3        
    lda     #$10                    ;2        
    pha                             ;3        
    lda     #$5d                    ;2        
    pha                             ;3        
    lda     #$f4                    ;2        
    pha                             ;3        
    lda     #$35                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  39
    
    lda     #$00                    ;2        
    sta     ram_95                  ;3        
    lda     #$00                    ;2        
    sta     ram_94                  ;3        
    lda     #$00                    ;2        
    sta     ram_93                  ;3        
    lda     #$00                    ;2        
    sta     ram_A3                  ;3        
    lda     #BLACK                  ;2        
    sta     COLUPF                  ;3        
    lda     #$f6                    ;2        
    sta     ram_F1                  ;3        
    lda     #$20                    ;2        
    sta     ram_F0                  ;3        
    lda     #$00                    ;2        
    sta     ram_D4                  ;3        
    sta     ram_D5                  ;3        
    sta     ram_D6                  ;3        
    sta     ram_D7                  ;3        
    sta     ram_D8                  ;3        
    sta     ram_D9                  ;3        
    sta     ram_DA                  ;3        
    sta     ram_DB                  ;3        
    sta     ram_DC                  ;3        
    lda     #$00                    ;2        
    sta     ram_DD                  ;3        
    sta     ram_DE                  ;3        
    sta     ram_DF                  ;3        
    sta     ram_E0                  ;3        
    sta     ram_E1                  ;3        
    sta     ram_E2                  ;3        
    sta     ram_E3                  ;3        
    sta     ram_E4                  ;3        
    sta     ram_E5                  ;3        
    lda     #$00                    ;2        
    sta     ram_E6                  ;3        
    sta     ram_E7                  ;3        
    sta     ram_E8                  ;3        
    sta     ram_E9                  ;3        
    sta     ram_EA                  ;3        
    sta     ram_EB                  ;3        
    sta     ram_EC                  ;3        
    sta     ram_ED                  ;3        
    lda     ram_E3                  ;3        
    ora     #$08                    ;2        
    sta     ram_E3                  ;3        
    lda     #$a0                    ;2        
    sta     ram_F3                  ;3        
    lda     #$a0                    ;2        
    sta     ram_F2                  ;3        
    lda     ram_F3                  ;3        
    and     #$e0                    ;2        
    ora     #$16                    ;2        
    sta     ram_F3                  ;3   = 147
L10ca
    lda     #$01                    ;2        
    bit     SWCHB                   ;4        
    bne     L10d4                   ;2/3      
    jmp     L1046                   ;3   =  11 *
    
L10d4
    lda     #BLACK                  ;2        
    sta     COLUP0                  ;3        
    lda     #BLACK|$6               ;2        
    sta     COLUP1                  ;3        
    lda     #BLACK|$6               ;2        
    sta     COLUBK                  ;3        
    lda     #BLACK                  ;2        
    sta     COLUPF                  ;3        
    lda     #CYAN                   ;2        
    sta     ram_F4                  ;3        
    lda     ram_E0                  ;3        
    and     #$04                    ;2        
    beq     L10f2                   ;2/3      
    lda     #$08                    ;2        
    sta     REFP0                   ;3   =  37
L10f2
    inc     ram_DC                  ;5        
    lda     #$28                    ;2        
    cmp     ram_DC                  ;3        
    bcs     L10fe                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_DC                  ;3   =  17
L10fe
    lda     ram_DC                  ;3        
    cmp     #$14                    ;2        
    bne     L110c                   ;2/3      
    lda     #$01                    ;2        
    cmp     ram_E9                  ;3        
    bcs     L110c                   ;2/3      
    dec     ram_E9                  ;5   =  19
L110c
    lda     ram_E7                  ;3        
    cmp     #$00                    ;2        
    bcs     L1116                   ;2/3      
    lda     #$00                    ;2         *
    sta     ram_E7                  ;3   =  12 *
L1116
    lda     ram_E3                  ;3        
    and     #$04                    ;2        
    beq     L1120                   ;2/3      
    lda     #BROWN                  ;2        
    sta     COLUP1                  ;3   =  12
L1120
    lda     #$00                    ;2        
    cmp     ram_E4                  ;3        
    bcc     L1129                   ;2/3      
    jmp     L1190                   ;3   =  10
    
L1129
    dec     ram_E4                  ;5        
    lda     ram_E4                  ;3        
    cmp     #$00                    ;2        
    bne     L1141                   ;2/3      
    lda     ram_E3                  ;3        
    and     #$04                    ;2        
    beq     L1141                   ;2/3      
    lda     #$fa                    ;2        
    sta     ram_D7                  ;3        
    lda     ram_E3                  ;3        
    and     #$fb                    ;2        
    sta     ram_E3                  ;3   =  32
L1141
    lda     ram_E3                  ;3        
    and     #$04                    ;2        
    beq     L1171                   ;2/3      
    lda     #$02                    ;2        
    cmp     ram_E4                  ;3        
    bcc     L1171                   ;2/3      
    lda     #$07                    ;2        
    sta     AUDV0                   ;3        
    lda     #$17                    ;2        
    sta     AUDF0                   ;3        
    lda     #$07                    ;2        
    sta     AUDC0                   ;3        
    sta     ram_EE                  ;3        
    lda     #$11                    ;2        
    pha                             ;3        
    lda     #$70                    ;2        
    pha                             ;3        
    lda     #$dc                    ;2        
    pha                             ;3        
    lda     #$0d                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  68
    
L1171
    bit     ram_E0                  ;3        
    bpl     L118d                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$11                    ;2        
    pha                             ;3        
    lda     #$8c                    ;2        
    pha                             ;3        
    lda     #$dc                    ;2        
    pha                             ;3        
    lda     #$1c                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  44
    
L118d
    jmp     L1e82                   ;3   =   3
    
L1190
    lda     ram_EB                  ;3        
    and     #$08                    ;2        
    bne     L119a                   ;2/3      
    lda     #$00                    ;2        
    sta     AUDV0                   ;3   =  12
L119a
    lda     ram_EB                  ;3        
    and     #$10                    ;2        
    bne     L11a4                   ;2/3      
    lda     #$00                    ;2        
    sta     AUDV1                   ;3   =  12
L11a4
    lda     ram_E3                  ;3        
    and     #$08                    ;2        
    bne     L11ad                   ;2/3      
    jmp     L11fa                   ;3   =  10
    
L11ad
    lda     #$00                    ;2        
    cmp     ram_E9                  ;3        
    bcc     L11b6                   ;2/3      
    jmp     L11e2                   ;3   =  10
    
L11b6
    lda     #$01                    ;2        
    cmp     ram_E9                  ;3        
    bcs     L11e2                   ;2/3      
    dec     ram_E9                  ;5        
    sed                             ;2        
    clc                             ;2        
    lda     ram_95                  ;3        
    adc     #$01                    ;2        
    sta     ram_95                  ;3        
    lda     ram_94                  ;3        
    adc     #$00                    ;2        
    sta     ram_94                  ;3        
    lda     ram_93                  ;3        
    adc     #$00                    ;2        
    sta     ram_93                  ;3        
    cld                             ;2        
    lda     #$05                    ;2        
    sta     AUDV0                   ;3        
    lda     #$0d                    ;2        
    sta     AUDC0                   ;3        
    lda     ram_E9                  ;3        
    sta     AUDF0                   ;3        
    jmp     L1e82                   ;3   =  61
    
L11e2
    sta     ram_EE                  ;3        
    lda     #$11                    ;2        
    pha                             ;3        
    lda     #$f9                    ;2        
    pha                             ;3        
    lda     #$2f                    ;2        
    pha                             ;3        
    lda     #$ff                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$02                    ;2        
    jmp     Lffeb                   ;3   =  39
    
L11fa
    lda     #$20                    ;2        
    cmp     ram_F3                  ;3        
    bcs     L1229                   ;2/3      
    bit     ram_E0                  ;3        
    bpl     L1229                   ;2/3      
    lda     ram_F3                  ;3        
    sec                             ;2        
    sbc     #$20                    ;2        
    sta     ram_F3                  ;3        
    lda     ram_E0                  ;3        
    and     #$7f                    ;2        
    sta     ram_E0                  ;3        
    sta     ram_EE                  ;3        
    lda     #$12                    ;2        
    pha                             ;3        
    lda     #$28                    ;2        
    pha                             ;3        
    lda     #$98                    ;2        
    pha                             ;3        
    lda     #$54                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$05                    ;2        
    jmp     Lffeb                   ;3   =  69
    
L1229
    lda     ram_F3                  ;3        
    cmp     #$20                    ;2        
    bcs     L1245                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_ED                  ;3        
    sta     ram_EE                  ;3        
    lda     #$b1                    ;2        
    pha                             ;3        
    lda     #$ea                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$06                    ;2        
    jmp     Lffeb                   ;3   =  41
    
L1245
    bit     ram_E3                  ;3        
    bvs     L124c                   ;2/3      
    jmp     L1290                   ;3   =   8
    
L124c
    lda     #$07                    ;2        
    sta     AUDV0                   ;3        
    lda     #$08                    ;2        
    sta     AUDC0                   ;3        
    lda     #BLACK|$2                    ;2        
    sta     COLUP0                  ;3        
    lda     #$01                    ;2        
    cmp     ram_8E                  ;3        
    bcs     L1269                   ;2/3      
    lda     ram_8E                  ;3        
    sec                             ;2        
    sbc     #$02                    ;2        
    sta     ram_8E                  ;3        
    lda     ram_8E                  ;3        
    sta     AUDF0                   ;3   =  38
L1269
    lda     ram_8E                  ;3        
    cmp     #$01                    ;2        
    bne     L128d                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$12                    ;2        
    pha                             ;3        
    lda     #$86                    ;2        
    pha                             ;3        
    lda     #$d9                    ;2        
    pha                             ;3        
    lda     #$fc                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  46
    
    lda     ram_E3                  ;3        
    and     #$bf                    ;2        
    sta     ram_E3                  ;3   =   8
L128d
    jmp     L1e82                   ;3   =   3
    
L1290
    lda     ram_80                  ;3        
    sec                             ;2        
    sbc     #$11                    ;2        
    lsr                             ;2        
    lsr                             ;2        
    sta     ram_DD                  ;3        
    lda     ram_80                  ;3        
    sec                             ;2        
    sbc     #$0b                    ;2        
    lsr                             ;2        
    lsr                             ;2        
    sta     ram_DE                  ;3        
    lda     ram_85                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    sta     ram_DF                  ;3        
    bit     CXM1P                   ;3        
    bpl     L12ba                   ;2/3      
    lda     ram_E0                  ;3        
    ora     #$80                    ;2        
    sta     ram_E0                  ;3        
    lda     #$08                    ;2        
    sta     ram_E4                  ;3        
    jmp     L1e82                   ;3   =  61
    
L12ba
    bit     CXPPMM                  ;3        
    bmi     L12c1                   ;2/3      
    jmp     L12c6                   ;3   =   8
    
L12c1
    lda     ram_E3                  ;3        
    lsr                             ;2        
    bcc     L12c9                   ;2/3 =   7
L12c6
    jmp     L14c1                   ;3   =   3
    
L12c9
    lda     ram_E8                  ;3        
    and     #$10                    ;2        
    beq     L12dc                   ;2/3      
    lda     ram_E0                  ;3        
    ora     #$80                    ;2        
    sta     ram_E0                  ;3        
    lda     #$08                    ;2        
    sta     ram_E4                  ;3        
    jmp     L1e82                   ;3   =  23
    
L12dc
    lda     ram_E3                  ;3        
    and     #$10                    ;2        
    bne     L12e5                   ;2/3      
    jmp     L1430                   ;3   =  10
    
L12e5
    lda     ram_D7                  ;3        
    clc                             ;2        
    adc     #$05                    ;2        
    cmp     ram_D5                  ;3        
    bcs     L12f4                   ;2/3      
    lda     ram_E0                  ;3        
    and     #$fe                    ;2        
    sta     ram_E0                  ;3   =  20
L12f4
    lda     ram_D6                  ;3        
    clc                             ;2        
    adc     #$06                    ;2        
    cmp     ram_D4                  ;3        
    bcc     L1303                   ;2/3!     
    lda     ram_E0                  ;3        
    ora     #$01                    ;2        
    sta     ram_E0                  ;3   =  20
L1303
    lda     ram_E0                  ;3        
    lsr                             ;2        
    bcc     L132c                   ;2/3      
    lda     ram_DC                  ;3        
    cmp     #$00                    ;2        
    bne     L131a                   ;2/3      
    ldx     #$a8                    ;2        
    stx     ram_8A                  ;3        
    lda     #$f6                    ;2        
    sta     ram_8B                  ;3        
    lda     #$07                    ;2        
    sta     ram_8E                  ;3   =  29
L131a
    lda     ram_DC                  ;3        
    cmp     #$14                    ;2        
    bne     L132c                   ;2/3      
    ldx     #$b0                    ;2        
    stx     ram_8A                  ;3        
    lda     #$f6                    ;2        
    sta     ram_8B                  ;3        
    lda     #$07                    ;2        
    sta     ram_8E                  ;3   =  22
L132c
    lda     ram_85                  ;3        
    sec                             ;2        
    sbc     #$0a                    ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    sta     ram_A0                  ;3        
    bit     CXP0FB                  ;3        
    bpl     L135d                   ;2/3      
    lda     ram_DD                  ;3         *
    ldy     ram_A0                  ;3         *
    sta     ram_EE                  ;3         *
    lda     #$13                    ;2         *
    pha                             ;3         *
    lda     #$55                    ;2         *
    pha                             ;3         *
    lda     #$f2                    ;2         *
    pha                             ;3         *
    lda     #$a9                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$08                    ;2         *
    jmp     Lffeb                   ;3   =  66 *
    
    ; next 3 never reached ?
    bne L135d
    inc ram_D5
    jmp L1391

L135d
    bit     CXP0FB                  ;3        
    bpl     L1384                   ;2/3      
    lda     ram_DE                  ;3         *
    ldy     ram_A0                  ;3         *
    sta     ram_EE                  ;3         *
    lda     #$13                    ;2         *
    pha                             ;3         *
    lda     #$7c                    ;2         *
    pha                             ;3         *
    lda     #$f2                    ;2         *
    pha                             ;3         *
    lda     #$a9                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$08                    ;2         *
    jmp     Lffeb                   ;3   =  50 *

    ; never reached?
    bne L1384
    inc ram_D5
    jmp L1391
    
L1384
    lda     #$10                    ;2        
    bit     SWCHA                   ;4        
    bne     L1391                   ;2/3      
    dec     ram_D5                  ;5        
    lda     ram_D6                  ;3        
    sta     ram_D4                  ;3   =  19
L1391
    bit     CXP0FB                  ;3        
    bpl     L1398                   ;2/3      
    jmp     L13af                   ;3   =   8 *
    
L1398
    lda     #$20                    ;2        
    bit     SWCHA                   ;4        
    bne     L13a5                   ;2/3      
    inc     ram_D5                  ;5         *
    lda     ram_D6                  ;3         *
    sta     ram_D4                  ;3   =  19 *
L13a5
    lda     #$20                    ;2        
    bit     SWCHA                   ;4        
    beq     L13af                   ;2/3      
    jmp     L13af                   ;3   =  11
    
L13af
    lda     ram_D6                  ;3        
    clc                             ;2        
    adc     #$06                    ;2        
    pha                             ;3        
    tsx                             ;2        
    pla                             ;4        
    lda     ram_D4                  ;3        
    cmp     CXM1P,x                 ;4        
    bcc     L13c3                   ;2/3      
    lda     ram_E0                  ;3        
    and     #$fe                    ;2        
    sta     ram_E0                  ;3   =  33
L13c3
    lda     ram_D6                  ;3        
    sec                             ;2        
    sbc     #$03                    ;2        
    pha                             ;3        
    tsx                             ;2        
    pla                             ;4        
    lda     ram_D4                  ;3        
    cmp     CXM1P,x                 ;4        
    bcs     L13d7                   ;2/3      
    lda     ram_E0                  ;3        
    and     #$fe                    ;2        
    sta     ram_E0                  ;3   =  33
L13d7
    lda     #$04                    ;2        
    cmp     ram_D5                  ;3        
    bcc     L13fb                   ;2/3      
    inc     ram_E5                  ;5        
    lda     #$fa                    ;2        
    sta     ram_D5                  ;3        
    sed                             ;2        
    clc                             ;2        
    lda     ram_94                  ;3        
    adc     #$01                    ;2        
    sta     ram_94                  ;3        
    lda     ram_93                  ;3        
    adc     #$00                    ;2        
    sta     ram_93                  ;3        
    cld                             ;2        
    lda     ram_E3                  ;3        
    ora     #$08                    ;2        
    sta     ram_E3                  ;3        
    jmp     L1e82                   ;3   =  50
    
L13fb
    lda     #$10                    ;2        
    bit     SWCHA                   ;4        
    beq     L140c                   ;2/3      
    lda     #$20                    ;2        
    bit     SWCHA                   ;4        
    beq     L140c                   ;2/3      
    jmp     L1430                   ;3   =  19
    
L140c
    lda     ram_DC                  ;3        
    cmp     #$00                    ;2        
    bne     L141e                   ;2/3      
    lda     #$05                    ;2        
    sta     AUDV0                   ;3        
    lda     #$06                    ;2        
    sta     AUDC0                   ;3        
    lda     #$08                    ;2        
    sta     AUDF0                   ;3   =  22
L141e
    lda     ram_DC                  ;3        
    cmp     #$14                    ;2        
    bne     L1430                   ;2/3      
    lda     #$05                    ;2        
    sta     AUDV0                   ;3        
    lda     #$0a                    ;2        
    sta     AUDC0                   ;3        
    lda     #$17                    ;2        
    sta     AUDF0                   ;3   =  22
L1430
    lda     ram_E3                  ;3        
    and     #$10                    ;2        
    beq     L1439                   ;2/3      
    jmp     L143f                   ;3   =  10
    
L1439
    lda     ram_E8                  ;3        
    and     #$10                    ;2        
    beq     L1442                   ;2/3 =   7
L143f
    jmp     L14c1                   ;3   =   3
    
L1442
    lda     ram_D7                  ;3        
    sec                             ;2        
    sbc     ram_8F                  ;3        
    clc                             ;2        
    adc     #$02                    ;2        
    cmp     ram_D5                  ;3        
    bcs     L145b                   ;2/3      
    lda     ram_E0                  ;3        
    ora     #$80                    ;2        
    sta     ram_E0                  ;3        
    lda     #$03                    ;2        
    sta     ram_E4                  ;3        
    jmp     L1e82                   ;3   =  33
    
L145b
    bit     ram_E8                  ;3        
    bpl     L1463                   ;2/3      
    lda     #$fa                    ;2         *
    sta     ram_DB                  ;3   =  10 *
L1463
    dec     ram_E7                  ;5        
    bit     ram_E8                  ;3        
    bvc     L14a2                   ;2/3      
    lda     #$00                    ;2        
    cmp     ram_E7                  ;3        
    bcc     L14a2                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$14                    ;2        
    pha                             ;3        
    lda     #$86                    ;2        
    pha                             ;3        
    lda     #$dc                    ;2        
    pha                             ;3        
    lda     #$0d                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  56
    
    lda     #$0a                    ;2        
    sta     ram_E4                  ;3        
    lda     ram_E3                  ;3        
    ora     #$04                    ;2        
    sta     ram_E3                  ;3        
    lda     ram_D7                  ;3        
    clc                             ;2        
    adc     #$04                    ;2        
    sta     ram_D7                  ;3        
    lda     ram_D5                  ;3        
    sec                             ;2        
    sbc     #$03                    ;2        
    sta     ram_D5                  ;3        
    jmp     L1e82                   ;3   =  36
    
L14a2
    bit     ram_E8                  ;3        
    bvs     L14c1                   ;2/3      
    lda     #$0a                    ;2        
    sta     ram_E4                  ;3        
    lda     ram_E3                  ;3        
    ora     #$04                    ;2        
    sta     ram_E3                  ;3        
    lda     ram_D7                  ;3        
    clc                             ;2        
    adc     #$04                    ;2        
    sta     ram_D7                  ;3        
    lda     ram_D5                  ;3        
    sec                             ;2        
    sbc     #$03                    ;2        
    sta     ram_D5                  ;3        
    jmp     L1e82                   ;3   =  41
    
L14c1
    lda     ram_E0                  ;3        
    and     #$02                    ;2        
    bne     L14ca                   ;2/3      
    jmp     L1581                   ;3   =  10
    
L14ca
    lda     ram_80                  ;3        
    sec                             ;2        
    sbc     #$0a                    ;2        
    lsr                             ;2        
    lsr                             ;2        
    sta     ram_9E                  ;3        
    lda     ram_80                  ;3        
    sec                             ;2        
    sbc     #$11                    ;2        
    lsr                             ;2        
    lsr                             ;2        
    sta     ram_9F                  ;3        
    lda     ram_85                  ;3        
    sec                             ;2        
    sbc     #$09                    ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    sta     ram_A1                  ;3        
    lda     ram_D5                  ;3        
    cmp     #$08                    ;2        
    bcs     L14fb                   ;2/3      
    lda     ram_E0                  ;3        
    ora     #$08                    ;2        
    sta     ram_E0                  ;3        
    lda     ram_E0                  ;3        
    and     #$fd                    ;2        
    sta     ram_E0                  ;3        
    jmp     L1581                   ;3   =  70
    
L14fb
    bit     CXP0FB                  ;3        
    bpl     L1530                   ;2/3!     
    lda     ram_9E                  ;3        
    ldy     ram_A1                  ;3        
    sta     ram_EE                  ;3        
    lda     #$15                    ;2        
    pha                             ;3        
    lda     #$1a                    ;2        
    pha                             ;3        
    lda     #$f2                    ;2        
    pha                             ;3        
    lda     #$a9                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  50
    
    bne     L1530                   ;2/3      
    lda     ram_E0                  ;3        
    ora     #$08                    ;2        
    sta     ram_E0                  ;3        
    lda     ram_E0                  ;3        
    and     #$fd                    ;2        
    sta     ram_E0                  ;3        
    lda     #$00                    ;2        
    sta     ram_E1                  ;3        
    jmp     L17b0                   ;3   =  26
    
L1530
    bit     CXP0FB                  ;3        
    bpl     L1565                   ;2/3      
    lda     ram_9F                  ;3        
    ldy     ram_A1                  ;3        
    sta     ram_EE                  ;3        
    lda     #$15                    ;2        
    pha                             ;3        
    lda     #$4f                    ;2        
    pha                             ;3        
    lda     #$f2                    ;2        
    pha                             ;3        
    lda     #$a9                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  50
    
    bne     L1565                   ;2/3      
    lda     ram_E0                  ;3        
    ora     #$08                    ;2        
    sta     ram_E0                  ;3        
    lda     ram_E0                  ;3        
    and     #$fd                    ;2        
    sta     ram_E0                  ;3        
    lda     #$00                    ;2        
    sta     ram_E1                  ;3        
    jmp     L17b0                   ;3   =  26
    
L1565
    bit     CXP0FB                  ;3        
    bmi     L1578                   ;2/3      
    lda     #$07                    ;2        
    sta     AUDV0                   ;3        
    lda     #$0d                    ;2        
    sta     AUDC0                   ;3        
    lda     #$06                    ;2        
    clc                             ;2        
    adc     ram_E1                  ;3        
    sta     AUDF0                   ;3   =  25
L1578
    dec     ram_E1                  ;5        
    lda     ram_D5                  ;3        
    sec                             ;2        
    sbc     #$02                    ;2        
    sta     ram_D5                  ;3   =  15
L1581
    lda     ram_EB                  ;3        
    and     #$04                    ;2        
    beq     L158a                   ;2/3      
    jmp     L16bd                   ;3   =  10 *
    
L158a
    bit     SWCHA                   ;4        
    bvc     L1597                   ;2/3      
    bit     SWCHA                   ;4        
    bpl     L1597                   ;2/3      
    jmp     L16bd                   ;3   =  15
    
L1597
    lda     ram_E0                  ;3        
    and     #$08                    ;2        
    beq     L15a0                   ;2/3      
    jmp     L15a7                   ;3   =  10
    
L15a0
    lda     #$20                    ;2        
    bit     SWCHA                   ;4        
    bne     L15aa                   ;2/3 =   8
L15a7
    jmp     L15e0                   ;3   =   3
    
L15aa
    lda     ram_DC                  ;3        
    cmp     #$00                    ;2        
    bne     L15b3                   ;2/3      
    jmp     L15b9                   ;3   =  10
    
L15b3
    lda     ram_DC                  ;3        
    cmp     #$14                    ;2        
    bne     L15c5                   ;2/3 =   7
L15b9
    lda     #$05                    ;2        
    sta     AUDV0                   ;3        
    lda     #$06                    ;2        
    sta     AUDC0                   ;3        
    lda     #$08                    ;2        
    sta     AUDF0                   ;3   =  15
L15c5
    lda     ram_DC                  ;3        
    cmp     #$0a                    ;2        
    bne     L15ce                   ;2/3      
    jmp     L15d4                   ;3   =  10
    
L15ce
    lda     ram_DC                  ;3        
    cmp     #$1e                    ;2        
    bne     L15e0                   ;2/3 =   7
L15d4
    lda     #$05                    ;2        
    sta     AUDV0                   ;3        
    lda     #$0a                    ;2        
    sta     AUDC0                   ;3        
    lda     #$08                    ;2        
    sta     AUDF0                   ;3   =  15
L15e0
    bit     CXPPMM                  ;3        
    bpl     L15ed                   ;2/3      
    lda     ram_E3                  ;3        
    and     #$10                    ;2        
    beq     L15ed                   ;2/3      
    jmp     L162f                   ;3   =  15
    
L15ed
    lda     #$20                    ;2        
    bit     SWCHA                   ;4        
    bne     L15fa                   ;2/3      
    lda     ram_E0                  ;3        
    ora     #$08                    ;2        
    sta     ram_E0                  ;3   =  16
L15fa
    lda     #$20                    ;2        
    bit     SWCHA                   ;4        
    beq     L1604                   ;2/3      
    jmp     L1608                   ;3   =  11
    
L1604
    bit     CXP0FB                  ;3        
    bmi     L160b                   ;2/3 =   5
L1608
    jmp     L162f                   ;3   =   3
    
L160b
    lda     ram_DC                  ;3        
    cmp     #$00                    ;2        
    bne     L161d                   ;2/3      
    lda     #$05                    ;2        
    sta     AUDV0                   ;3        
    lda     #$03                    ;2        
    sta     AUDC0                   ;3        
    lda     #$11                    ;2        
    sta     AUDF0                   ;3   =  22
L161d
    lda     ram_DC                  ;3        
    cmp     #$14                    ;2        
    bne     L162f                   ;2/3      
    lda     #$05                    ;2        
    sta     AUDV0                   ;3        
    lda     #$06                    ;2        
    sta     AUDC0                   ;3        
    lda     #$11                    ;2        
    sta     AUDF0                   ;3   =  22
L162f
    bit     SWCHA                   ;4        
    bvc     L1637                   ;2/3      
    jmp     L1679                   ;3   =   9
    
L1637
    lda     ram_E0                  ;3        
    ora     #$04                    ;2        
    sta     ram_E0                  ;3        
    lda     ram_80                  ;3        
    sec                             ;2        
    sbc     #$12                    ;2        
    lsr                             ;2        
    lsr                             ;2        
    sta     ram_9F                  ;3        
    lda     ram_85                  ;3        
    sec                             ;2        
    sbc     #$01                    ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    sta     ram_A0                  ;3        
    lda     ram_9F                  ;3        
    ldy     ram_A0                  ;3        
    sta     ram_EE                  ;3        
    lda     #$16                    ;2        
    pha                             ;3        
    lda     #$6b                    ;2        
    pha                             ;3        
    lda     #$f2                    ;2        
    pha                             ;3        
    lda     #$a9                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  83
    
    bne     L1671                   ;2/3      
    jmp     L1679                   ;3   =   5
    
L1671
    lda     #$12                    ;2        
    cmp     ram_D4                  ;3        
    bcs     L1679                   ;2/3      
    dec     ram_D4                  ;5   =  12
L1679
    bit     SWCHA                   ;4        
    bpl     L1681                   ;2/3      
    jmp     L16bd                   ;3   =   9
    
L1681
    lda     ram_E0                  ;3        
    and     #$fb                    ;2        
    sta     ram_E0                  ;3        
    lda     ram_80                  ;3        
    sec                             ;2        
    sbc     #$09                    ;2        
    lsr                             ;2        
    lsr                             ;2        
    sta     ram_9F                  ;3        
    lda     ram_85                  ;3        
    sec                             ;2        
    sbc     #$01                    ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    sta     ram_A0                  ;3        
    lda     ram_9F                  ;3        
    ldy     ram_A0                  ;3        
    sta     ram_EE                  ;3        
    lda     #$16                    ;2        
    pha                             ;3        
    lda     #$b5                    ;2        
    pha                             ;3        
    lda     #$f2                    ;2        
    pha                             ;3        
    lda     #$a9                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  83
    
    bne     L16bb                   ;2/3      
    jmp     L16bd                   ;3   =   5
    
L16bb
    inc     ram_D4                  ;5   =   5
L16bd
    bit     CXPPMM                  ;3        
    bpl     L16d0                   ;2/3      
    lda     ram_E3                  ;3        
    and     #$10                    ;2        
    beq     L16d0                   ;2/3      
    lda     ram_E0                  ;3        
    and     #$fd                    ;2        
    sta     ram_E0                  ;3        
    jmp     L170e                   ;3   =  23
    
L16d0
    lda     ram_E1                  ;3        
    cmp     #$00                    ;2        
    bne     L16dc                   ;2/3      
    lda     ram_E0                  ;3        
    and     #$fd                    ;2        
    sta     ram_E0                  ;3   =  15
L16dc
    bit     INPT4                   ;3        
    bpl     L16e9                   ;2/3      
    lda     ram_E0                  ;3        
    and     #$f7                    ;2        
    sta     ram_E0                  ;3        
    jmp     L170e                   ;3   =  16
    
L16e9
    lda     ram_E0                  ;3        
    and     #$08                    ;2        
    beq     L16f2                   ;2/3      
    jmp     L170e                   ;3   =  10
    
L16f2
    ldx     #$b8                    ;2        
    stx     ram_8A                  ;3        
    lda     #$f6                    ;2        
    sta     ram_8B                  ;3        
    lda     #$06                    ;2        
    sta     ram_8E                  ;3        
    lda     ram_E0                  ;3        
    ora     #$08                    ;2        
    sta     ram_E0                  ;3        
    lda     ram_E0                  ;3        
    ora     #$02                    ;2        
    sta     ram_E0                  ;3        
    lda     #$16                    ;2        
    sta     ram_E1                  ;3   =  36
L170e
    lda     ram_E0                  ;3        
    and     #$02                    ;2        
    beq     L171d                   ;2/3      
    lda     #$00                    ;2        
    cmp     ram_E1                  ;3        
    bcs     L171d                   ;2/3      
    jmp     L17b0                   ;3   =  17
    
L171d
    bit     CXPPMM                  ;3        
    bpl     L172a                   ;2/3      
    lda     ram_E3                  ;3        
    and     #$10                    ;2        
    beq     L172a                   ;2/3      
    jmp     L17b0                   ;3   =  15
    
L172a
    bit     SWCHA                   ;4        
    bvc     L1740                   ;2/3      
    bit     SWCHA                   ;4        
    bpl     L1740                   ;2/3      
    ldx     #$bf                    ;2        
    stx     ram_8A                  ;3        
    lda     #$f6                    ;2        
    sta     ram_8B                  ;3        
    lda     #$07                    ;2        
    sta     ram_8E                  ;3   =  27
L1740
    bit     SWCHA                   ;4        
    bvc     L174d                   ;2/3      
    bit     SWCHA                   ;4        
    bpl     L174d                   ;2/3      
    jmp     L1790                   ;3   =  15
    
L174d
    lda     ram_E0                  ;3        
    and     #$02                    ;2        
    beq     L175a                   ;2/3      
    bit     CXP0FB                  ;3         *
    bmi     L175a                   ;2/3       *
    jmp     L1790                   ;3   =  15 *
    
L175a
    lda     ram_DC                  ;3        
    cmp     #$00                    ;2        
    bne     L1763                   ;2/3      
    jmp     L1769                   ;3   =  10
    
L1763
    lda     ram_DC                  ;3        
    cmp     #$14                    ;2        
    bne     L1775                   ;2/3 =   7
L1769
    ldx     #$c7                    ;2        
    stx     ram_8A                  ;3        
    lda     #$f6                    ;2        
    sta     ram_8B                  ;3        
    lda     #$07                    ;2        
    sta     ram_8E                  ;3   =  15
L1775
    lda     ram_DC                  ;3        
    cmp     #$0a                    ;2        
    bne     L177e                   ;2/3      
    jmp     L1784                   ;3   =  10
    
L177e
    lda     ram_DC                  ;3        
    cmp     #$1e                    ;2        
    bne     L1790                   ;2/3 =   7
L1784
    ldx     #$cf                    ;2        
    stx     ram_8A                  ;3        
    lda     #$f6                    ;2        
    sta     ram_8B                  ;3        
    lda     #$07                    ;2        
    sta     ram_8E                  ;3   =  15
L1790
    bit     CXPPMM                  ;3        
    bpl     L179d                   ;2/3      
    lda     ram_E3                  ;3        
    and     #$10                    ;2        
    beq     L179d                   ;2/3      
    jmp     L17b0                   ;3   =  15 *
    
L179d
    lda     #$20                    ;2        
    bit     SWCHA                   ;4        
    bne     L17b0                   ;2/3      
    ldx     #$d7                    ;2        
    stx     ram_8A                  ;3        
    lda     #$f6                    ;2        
    sta     ram_8B                  ;3        
    lda     #$05                    ;2        
    sta     ram_8E                  ;3   =  23
L17b0
    lda     #$13                    ;2        
    cmp     ram_E1                  ;3        
    bcs     L17bc                   ;2/3      
    lda     ram_E0                  ;3        
    and     #$02                    ;2        
    bne     L180e                   ;2/3!=  14
L17bc
    lda     ram_E0                  ;3        
    lsr                             ;2        
    bcc     L17c4                   ;2/3      
    jmp     L180e                   ;3   =  10
    
L17c4
    lda     ram_DD                  ;3        
    ldy     ram_DF                  ;3        
    sta     ram_EE                  ;3        
    lda     #$17                    ;2        
    pha                             ;3        
    lda     #$df                    ;2        
    pha                             ;3        
    lda     #$f2                    ;2        
    pha                             ;3        
    lda     #$a9                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  45
    
    bne     L17e5                   ;2/3      
    jmp     L180e                   ;3   =   5
    
L17e5
    lda     ram_DE                  ;3        
    ldy     ram_DF                  ;3        
    sta     ram_EE                  ;3        
    lda     #$18                    ;2        
    pha                             ;3        
    lda     #$00                    ;2        
    pha                             ;3        
    lda     #$f2                    ;2        
    pha                             ;3        
    lda     #$a9                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  45
    
    bne     L1806                   ;2/3      
    jmp     L180e                   ;3   =   5
    
L1806
    lda     ram_E0                  ;3        
    ora     #$08                    ;2        
    sta     ram_E0                  ;3        
    inc     ram_D5                  ;5   =  13
L180e
    lda     ram_E5                  ;3        
    cmp     #$64                    ;2        
    bne     L182f                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$18                    ;2        
    pha                             ;3        
    lda     #$2b                    ;2        
    pha                             ;3        
    lda     #$77                    ;2        
    pha                             ;3        
    lda     #$17                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$04                    ;2        
    jmp     Lffeb                   ;3   =  46
    
    jmp     L1e56                   ;3   =   3
    
L182f
    lda     ram_E5                  ;3        
    cmp     #$00                    ;2        
    bne     L1838                   ;2/3      
    jmp     L183e                   ;3   =  10
    
L1838
    lda     ram_E5                  ;3        
    cmp     #$01                    ;2        
    bne     L1859                   ;2/3 =   7
L183e
    sta     ram_EE                  ;3        
    lda     #$18                    ;2        
    pha                             ;3        
    lda     #$55                    ;2        
    pha                             ;3        
    lda     #$50                    ;2        
    pha                             ;3        
    lda     #$61                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$03                    ;2        
    jmp     Lffeb                   ;3   =  39
    
    jmp     L1e56                   ;3   =   3
    
L1859
    lda     ram_E5                  ;3        
    cmp     #$02                    ;2        
    bne     L187a                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$18                    ;2        
    pha                             ;3        
    lda     #$76                    ;2        
    pha                             ;3        
    lda     #$50                    ;2        
    pha                             ;3        
    lda     #$b7                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$03                    ;2        
    jmp     Lffeb                   ;3   =  46
    
    jmp     L1e56                   ;3   =   3
    
L187a
    lda     ram_E5                  ;3        
    cmp     #$03                    ;2        
    bne     L189b                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$18                    ;2        
    pha                             ;3        
    lda     #$97                    ;2        
    pha                             ;3        
    lda     #$51                    ;2        
    pha                             ;3        
    lda     #$a4                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$03                    ;2        
    jmp     Lffeb                   ;3   =  46
    
    jmp     L1e56                   ;3   =   3
    
L189b
    lda     ram_E5                  ;3        
    cmp     #$04                    ;2        
    bne     L18bc                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$18                    ;2        
    pha                             ;3        
    lda     #$b8                    ;2        
    pha                             ;3        
    lda     #$71                    ;2        
    pha                             ;3        
    lda     #$70                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$04                    ;2        
    jmp     Lffeb                   ;3   =  46
    
    jmp     L1e56                   ;3   =   3
    
L18bc
    lda     ram_E5                  ;3        
    cmp     #$05                    ;2        
    bne     L18dd                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$18                    ;2        
    pha                             ;3        
    lda     #$d9                    ;2        
    pha                             ;3        
    lda     #$52                    ;2        
    pha                             ;3        
    lda     #$09                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$03                    ;2        
    jmp     Lffeb                   ;3   =  46
    
    jmp     L1e56                   ;3   =   3
    
L18dd
    lda     ram_E5                  ;3        
    cmp     #$06                    ;2        
    bne     L18fe                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$18                    ;2        
    pha                             ;3        
    lda     #$fa                    ;2        
    pha                             ;3        
    lda     #$52                    ;2        
    pha                             ;3        
    lda     #$c5                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$03                    ;2        
    jmp     Lffeb                   ;3   =  46
    
    jmp     L1e56                   ;3   =   3
    
L18fe
    lda     ram_E5                  ;3        
    cmp     #$07                    ;2        
    bne     L191f                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$19                    ;2        
    pha                             ;3        
    lda     #$1b                    ;2        
    pha                             ;3        
    lda     #$53                    ;2        
    pha                             ;3        
    lda     #$69                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$03                    ;2        
    jmp     Lffeb                   ;3   =  46
    
    jmp     L1e56                   ;3   =   3
    
L191f
    lda     ram_E5                  ;3        
    cmp     #$08                    ;2        
    bne     L1940                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$19                    ;2        
    pha                             ;3        
    lda     #$3c                    ;2        
    pha                             ;3        
    lda     #$54                    ;2        
    pha                             ;3        
    lda     #$bd                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$03                    ;2        
    jmp     Lffeb                   ;3   =  46
    
    jmp     L1e56                   ;3   =   3
    
L1940
    lda     ram_E5                  ;3        
    cmp     #$09                    ;2        
    bne     L1961                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$19                    ;2        
    pha                             ;3        
    lda     #$5d                    ;2        
    pha                             ;3        
    lda     #$55                    ;2        
    pha                             ;3        
    lda     #$1f                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$03                    ;2        
    jmp     Lffeb                   ;3   =  46
    
    jmp     L1e56                   ;3   =   3
    
L1961
    lda     ram_E5                  ;3        
    cmp     #$0a                    ;2        
    bne     L1982                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$19                    ;2        
    pha                             ;3        
    lda     #$7e                    ;2        
    pha                             ;3        
    lda     #$bc                    ;2        
    pha                             ;3        
    lda     #$23                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$06                    ;2        
    jmp     Lffeb                   ;3   =  46
    
    jmp     L1e56                   ;3   =   3
    
L1982
    lda     ram_E5                  ;3        
    cmp     #$0b                    ;2        
    bne     L19a3                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$19                    ;2        
    pha                             ;3        
    lda     #$9f                    ;2        
    pha                             ;3        
    lda     #$58                    ;2        
    pha                             ;3        
    lda     #$05                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$03                    ;2        
    jmp     Lffeb                   ;3   =  46
    
    jmp     L1e56                   ;3   =   3
    
L19a3
    lda     ram_E5                  ;3        
    cmp     #$0c                    ;2        
    bne     L19c4                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$19                    ;2        
    pha                             ;3        
    lda     #$c0                    ;2        
    pha                             ;3        
    lda     #$5d                    ;2        
    pha                             ;3        
    lda     #$b3                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$03                    ;2        
    jmp     Lffeb                   ;3   =  46
    
    jmp     L1e56                   ;3   =   3
    
L19c4
    lda     ram_E5                  ;3        
    cmp     #$0d                    ;2        
    bne     L19e5                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$19                    ;2        
    pha                             ;3        
    lda     #$e1                    ;2        
    pha                             ;3        
    lda     #$5c                    ;2        
    pha                             ;3        
    lda     #$54                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$03                    ;2        
    jmp     Lffeb                   ;3   =  46
    
    jmp     L1e56                   ;3   =   3
    
L19e5
    lda     ram_E5                  ;3        
    cmp     #$0e                    ;2        
    bne     L19ee                   ;2/3      
    jmp     L19f4                   ;3   =  10
    
L19ee
    lda     ram_E5                  ;3        
    cmp     #$16                    ;2        
    bne     L1a0f                   ;2/3!=   7
L19f4
    sta     ram_EE                  ;3        
    lda     #$1a                    ;2        
    pha                             ;3        
    lda     #$0b                    ;2        
    pha                             ;3        
    lda     #$5b                    ;2        
    pha                             ;3        
    lda     #$5a                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$03                    ;2        
    jmp     Lffeb                   ;3   =  39
    
    jmp     L1e56                   ;3   =   3
    
L1a0f
    lda     ram_E5                  ;3        
    cmp     #$0f                    ;2        
    bne     L1a18                   ;2/3      
    jmp     L1a1e                   ;3   =  10
    
L1a18
    lda     ram_E5                  ;3        
    cmp     #$15                    ;2        
    bne     L1a39                   ;2/3 =   7
L1a1e
    sta     ram_EE                  ;3        
    lda     #$1a                    ;2        
    pha                             ;3        
    lda     #$35                    ;2        
    pha                             ;3        
    lda     #$55                    ;2        
    pha                             ;3        
    lda     #$1f                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$03                    ;2        
    jmp     Lffeb                   ;3   =  39
    
    jmp     L1e56                   ;3   =   3
    
L1a39
    lda     ram_E5                  ;3        
    cmp     #$10                    ;2        
    bne     L1a42                   ;2/3      
    jmp     L1a48                   ;3   =  10
    
L1a42
    lda     ram_E5                  ;3        
    cmp     #$14                    ;2        
    bne     L1a63                   ;2/3 =   7
L1a48
    sta     ram_EE                  ;3        
    lda     #$1a                    ;2        
    pha                             ;3        
    lda     #$5f                    ;2        
    pha                             ;3        
    lda     #$5d                    ;2        
    pha                             ;3        
    lda     #$b3                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$03                    ;2        
    jmp     Lffeb                   ;3   =  39
    
    jmp     L1e56                   ;3   =   3
    
L1a63
    lda     ram_E5                  ;3        
    cmp     #$11                    ;2        
    bne     L1a6c                   ;2/3      
    jmp     L1a72                   ;3   =  10
    
L1a6c
    lda     ram_E5                  ;3        
    cmp     #$13                    ;2        
    bne     L1a8d                   ;2/3 =   7
L1a72
    sta     ram_EE                  ;3        
    lda     #$1a                    ;2        
    pha                             ;3        
    lda     #$89                    ;2        
    pha                             ;3        
    lda     #$58                    ;2        
    pha                             ;3        
    lda     #$5c                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$03                    ;2        
    jmp     Lffeb                   ;3   =  39
    
    jmp     L1e56                   ;3   =   3
    
L1a8d
    lda     ram_E5                  ;3        
    cmp     #$27                    ;2        
    bne     L1a96                   ;2/3      
    jmp     L1a9c                   ;3   =  10 *
    
L1a96
    lda     ram_E5                  ;3        
    cmp     #$2b                    ;2        
    bne     L1ab7                   ;2/3 =   7
L1a9c
    sta     ram_EE                  ;3         *
    lda     #$1a                    ;2         *
    pha                             ;3         *
    lda     #$b3                    ;2         *
    pha                             ;3         *
    lda     #$58                    ;2         *
    pha                             ;3         *
    lda     #$5c                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$03                    ;2         *
    jmp     Lffeb                   ;3   =  39 *
    
    ; never reached ? $1ab4 (*)
    jmp L1e56
    
L1ab7
    lda     ram_E5                  ;3        
    cmp     #$28                    ;2        
    bne     L1ac0                   ;2/3      
    jmp     L1ac6                   ;3   =  10 *
    
L1ac0
    lda     ram_E5                  ;3        
    cmp     #$2a                    ;2        
    bne     L1ae1                   ;2/3 =   7
L1ac6
    sta     ram_EE                  ;3         *
    lda     #$1a                    ;2         *
    pha                             ;3         *
    lda     #$dd                    ;2         *
    pha                             ;3         *
    lda     #$b4                    ;2         *
    pha                             ;3         *
    lda     #$26                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$06                    ;2         *
    jmp     Lffeb                   ;3   =  39 *
    
    ; never reached? $1ade (*)
    jmp L1e56
    
L1ae1
    lda     ram_E5                  ;3        
    cmp     #$12                    ;2        
    bne     L1aea                   ;2/3      
    jmp     L1af0                   ;3   =  10
    
L1aea
    lda     ram_E5                  ;3        
    cmp     #$29                    ;2        
    bne     L1b0b                   ;2/3!=   7
L1af0
    sta     ram_EE                  ;3        
    lda     #$1b                    ;2        
    pha                             ;3        
    lda     #$07                    ;2        
    pha                             ;3        
    lda     #$75                    ;2        
    pha                             ;3        
    lda     #$5c                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$04                    ;2        
    jmp     Lffeb                   ;3   =  39
    
    jmp     L1e56                   ;3   =   3
    
L1b0b
    lda     ram_E5                  ;3        
    cmp     #$17                    ;2        
    bne     L1b2c                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$1b                    ;2        
    pha                             ;3        
    lda     #$28                    ;2        
    pha                             ;3        
    lda     #$73                    ;2        
    pha                             ;3        
    lda     #$62                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$04                    ;2        
    jmp     Lffeb                   ;3   =  46
    
    jmp     L1e56                   ;3   =   3
    
L1b2c
    lda     ram_E5                  ;3        
    cmp     #$18                    ;2        
    bne     L1b4d                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$1b                    ;2        
    pha                             ;3        
    lda     #$49                    ;2        
    pha                             ;3        
    lda     #$5a                    ;2        
    pha                             ;3        
    lda     #$29                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$03                    ;2        
    jmp     Lffeb                   ;3   =  46
    
    jmp     L1e56                   ;3   =   3
    
L1b4d
    lda     ram_E5                  ;3        
    cmp     #$19                    ;2        
    bne     L1b6e                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$1b                    ;2        
    pha                             ;3        
    lda     #$6a                    ;2        
    pha                             ;3        
    lda     #$78                    ;2        
    pha                             ;3        
    lda     #$a9                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$04                    ;2        
    jmp     Lffeb                   ;3   =  46
    
    jmp     L1e56                   ;3   =   3
    
L1b6e
    lda     ram_E5                  ;3        
    cmp     #$1a                    ;2        
    bne     L1b8f                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$1b                    ;2        
    pha                             ;3        
    lda     #$8b                    ;2        
    pha                             ;3        
    lda     #$79                    ;2        
    pha                             ;3        
    lda     #$fe                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$04                    ;2        
    jmp     Lffeb                   ;3   =  46
    
    jmp     L1e56                   ;3   =   3
    
L1b8f
    lda     ram_E5                  ;3         *
    cmp     #$1b                    ;2         *
    bne     L1bb0                   ;2/3       *
    sta     ram_EE                  ;3         *
    lda     #$1b                    ;2         *
    pha                             ;3         *
    lda     #$ac                    ;2         *
    pha                             ;3         *
    lda     #$7a                    ;2         *
    pha                             ;3         *
    lda     #$6e                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$04                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    ; never reached? $1bad (*)
    jmp L1e56
    
L1bb0
    lda     ram_E5                  ;3         *
    cmp     #$1c                    ;2         *
    bne     L1bd1                   ;2/3       *
    sta     ram_EE                  ;3         *
    lda     #$1b                    ;2         *
    pha                             ;3         *
    lda     #$cd                    ;2         *
    pha                             ;3         *
    lda     #$7a                    ;2         *
    pha                             ;3         *
    lda     #$ce                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$04                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    ; never reached? $1bce (*)
    jmp L1e56
    
L1bd1
    lda     ram_E5                  ;3         *
    cmp     #$1d                    ;2         *
    bne     L1bf2                   ;2/3       *
    sta     ram_EE                  ;3         *
    lda     #$1b                    ;2         *
    pha                             ;3         *
    lda     #$ee                    ;2         *
    pha                             ;3         *
    lda     #$7b                    ;2         *
    pha                             ;3         *
    lda     #$d0                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$04                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    ; never reached? $1bef (*)
    jmp L1e56
    
L1bf2
    lda     ram_E5                  ;3         *
    cmp     #$1e                    ;2         *
    bne     L1c13                   ;2/3!      *
    sta     ram_EE                  ;3         *
    lda     #$1c                    ;2         *
    pha                             ;3         *
    lda     #$0f                    ;2         *
    pha                             ;3         *
    lda     #$bd                    ;2         *
    pha                             ;3         *
    lda     #$1a                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$06                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    ; never reached? $1c10 (*)
    jmp L1e56
    
L1c13
    lda     ram_E5                  ;3         *
    cmp     #$1f                    ;2         *
    bne     L1c34                   ;2/3       *
    sta     ram_EE                  ;3         *
    lda     #$1c                    ;2         *
    pha                             ;3         *
    lda     #$30                    ;2         *
    pha                             ;3         *
    lda     #$55                    ;2         *
    pha                             ;3         *
    lda     #$a9                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$03                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    ; never reached? $1c31 (*)
    jmp L1e56
    
L1c34
    lda     ram_E5                  ;3         *
    cmp     #$20                    ;2         *
    bne     L1c3d                   ;2/3       *
    jmp     L1c43                   ;3   =  10 *
    
L1c3d
    lda     ram_E5                  ;3         *
    cmp     #$23                    ;2         *
    bne     L1c5e                   ;2/3 =   7 *
L1c43
    sta     ram_EE                  ;3         *
    lda     #$1c                    ;2         *
    pha                             ;3         *
    lda     #$5a                    ;2         *
    pha                             ;3         *
    lda     #$56                    ;2         *
    pha                             ;3         *
    lda     #$eb                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$03                    ;2         *
    jmp     Lffeb                   ;3   =  39 *
    
    ; never reached? $1c5b (*)
    jmp L1e56
    
L1c5e
    lda     ram_E5                  ;3         *
    cmp     #$21                    ;2         *
    bne     L1c7f                   ;2/3       *
    sta     ram_EE                  ;3         *
    lda     #$1c                    ;2         *
    pha                             ;3         *
    lda     #$7b                    ;2         *
    pha                             ;3         *
    lda     #$b5                    ;2         *
    pha                             ;3         *
    lda     #$ff                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$06                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    ; never reached? $1c7c (*)
    jmp L1e56
    
L1c7f
    lda     ram_E5                  ;3         *
    cmp     #$22                    ;2         *
    bne     L1ca0                   ;2/3       *
    sta     ram_EE                  ;3         *
    lda     #$1c                    ;2         *
    pha                             ;3         *
    lda     #$9c                    ;2         *
    pha                             ;3         *
    lda     #$ba                    ;2         *
    pha                             ;3         *
    lda     #$fe                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$06                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    ; never reached? $1c9d (*)
    jmp L1e56
    
L1ca0
    lda     ram_E5                  ;3         *
    cmp     #$24                    ;2         *
    bne     L1cc1                   ;2/3       *
    sta     ram_EE                  ;3         *
    lda     #$1c                    ;2         *
    pha                             ;3         *
    lda     #$bd                    ;2         *
    pha                             ;3         *
    lda     #$8f                    ;2         *
    pha                             ;3         *
    lda     #$ff                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$05                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    ; never reached? $1cbe (*)
    jmp L1e56
    
L1cc1
    lda     ram_E5                  ;3         *
    cmp     #$25                    ;2         *
    bne     L1ce2                   ;2/3       *
    sta     ram_EE                  ;3         *
    lda     #$1c                    ;2         *
    pha                             ;3         *
    lda     #$de                    ;2         *
    pha                             ;3         *
    lda     #$5b                    ;2         *
    pha                             ;3         *
    lda     #$5a                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$03                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    ; never reached? $1cdf (*)
    jmp L1e56
    
L1ce2
    lda     ram_E5                  ;3         *
    cmp     #$26                    ;2         *
    bne     L1ceb                   ;2/3       *
    jmp     L1cf1                   ;3   =  10 *
    
L1ceb
    lda     ram_E5                  ;3         *
    cmp     #$2c                    ;2         *
    bne     L1d0c                   ;2/3!=   7 *
L1cf1
    sta     ram_EE                  ;3         *
    lda     #$1d                    ;2         *
    pha                             ;3         *
    lda     #$08                    ;2         *
    pha                             ;3         *
    lda     #$b5                    ;2         *
    pha                             ;3         *
    lda     #$09                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$06                    ;2         *
    jmp     Lffeb                   ;3   =  39 *
    
    ; never reached? $1d09 (*)
    jmp L1e56
    
L1d0c
    lda     ram_E5                  ;3         *
    cmp     #$2d                    ;2         *
    bne     L1d2d                   ;2/3       *
    sta     ram_EE                  ;3         *
    lda     #$1d                    ;2         *
    pha                             ;3         *
    lda     #$29                    ;2         *
    pha                             ;3         *
    lda     #$5b                    ;2         *
    pha                             ;3         *
    lda     #$5a                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$03                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    ; never reached? $1d2a (*)
    jmp L1e56
    
L1d2d
    lda     ram_E5                  ;3         *
    cmp     #$2e                    ;2         *
    bne     L1d4e                   ;2/3       *
    sta     ram_EE                  ;3         *
    lda     #$1d                    ;2         *
    pha                             ;3         *
    lda     #$4a                    ;2         *
    pha                             ;3         *
    lda     #$b3                    ;2         *
    pha                             ;3         *
    lda     #$79                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$06                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    ; never reached? $1d4b (*)
    jmp L1e56
    
L1d4e
    lda     ram_E5                  ;3         *
    cmp     #$2f                    ;2         *
    bne     L1d6f                   ;2/3       *
    sta     ram_EE                  ;3         *
    lda     #$1d                    ;2         *
    pha                             ;3         *
    lda     #$6b                    ;2         *
    pha                             ;3         *
    lda     #$94                    ;2         *
    pha                             ;3         *
    lda     #$84                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$05                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    ; never reached? $1d6c (*)
    jmp L1e56
    
L1d6f
    lda     ram_E5                  ;3         *
    cmp     #$30                    ;2         *
    bne     L1d90                   ;2/3       *
    sta     ram_EE                  ;3         *
    lda     #$1d                    ;2         *
    pha                             ;3         *
    lda     #$8c                    ;2         *
    pha                             ;3         *
    lda     #$97                    ;2         *
    pha                             ;3         *
    lda     #$cb                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$05                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    ; never reached? $1d8d (*)
    jmp L1e56
    
L1d90
    lda     ram_E5                  ;3         *
    cmp     #$31                    ;2         *
    bne     L1db1                   ;2/3       *
    sta     ram_EE                  ;3         *
    lda     #$1d                    ;2         *
    pha                             ;3         *
    lda     #$ad                    ;2         *
    pha                             ;3         *
    lda     #$96                    ;2         *
    pha                             ;3         *
    lda     #$ca                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$05                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    ; never reached? $1dae (*)
    jmp L1e56
    
L1db1
    lda     ram_E5                  ;3         *
    cmp     #$32                    ;2         *
    bne     L1dd2                   ;2/3       *
    sta     ram_EE                  ;3         *
    lda     #$1d                    ;2         *
    pha                             ;3         *
    lda     #$ce                    ;2         *
    pha                             ;3         *
    lda     #$95                    ;2         *
    pha                             ;3         *
    lda     #$32                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$05                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    ; never reached? $1dcf (*)
    jmp L1e56
    
L1dd2
    lda     ram_E5                  ;3         *
    cmp     #$33                    ;2         *
    bne     L1df3                   ;2/3       *
    sta     ram_EE                  ;3         *
    lda     #$1d                    ;2         *
    pha                             ;3         *
    lda     #$ef                    ;2         *
    pha                             ;3         *
    lda     #$7c                    ;2         *
    pha                             ;3         *
    lda     #$14                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$04                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    ; never reached? $1df0 (*)
    jmp L1e56
    
L1df3
    lda     ram_E5                  ;3         *
    cmp     #$34                    ;2         *
    bne     L1e14                   ;2/3!      *
    sta     ram_EE                  ;3         *
    lda     #$1e                    ;2         *
    pha                             ;3         *
    lda     #$10                    ;2         *
    pha                             ;3         *
    lda     #$71                    ;2         *
    pha                             ;3         *
    lda     #$09                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$04                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    ; never reached? $1e11 (*)
    jmp L1e56
    
L1e14
    lda     ram_E5                  ;3         *
    cmp     #$35                    ;2         *
    bne     L1e35                   ;2/3       *
    sta     ram_EE                  ;3         *
    lda     #$1e                    ;2         *
    pha                             ;3         *
    lda     #$31                    ;2         *
    pha                             ;3         *
    lda     #$91                    ;2         *
    pha                             ;3         *
    lda     #$b2                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$05                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    ; never reached? $1e32 (*)
    jmp L1e56
    
L1e35
    lda     ram_E5                  ;3         *
    cmp     #$36                    ;2         *
    bne     L1e56                   ;2/3       *
    sta     ram_EE                  ;3         *
    lda     #$1e                    ;2         *
    pha                             ;3         *
    lda     #$52                    ;2         *
    pha                             ;3         *
    lda     #$b1                    ;2         *
    pha                             ;3         *
    lda     #$3b                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$06                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    ; never reached? $1e53 (*)
    jmp L1e56
    
L1e56
    bit     ram_E3                  ;3        
    bpl     L1e5d                   ;2/3      
    jmp     L1e82                   ;3   =   8
    
L1e5d
    lda     ram_E8                  ;3        
    and     #$08                    ;2        
    beq     L1e7e                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$1e                    ;2        
    pha                             ;3        
    lda     #$7a                    ;2        
    pha                             ;3        
    lda     #$b3                    ;2        
    pha                             ;3        
    lda     #$13                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$06                    ;2        
    jmp     Lffeb                   ;3   =  46
    
    jmp     L1e82                   ;3   =   3
    
L1e7e
    lda     #$35                    ;2        
    sta     CTRLPF                  ;3   =   5
L1e82
    lda     ram_D4                  ;3        
    sta     ram_80                  ;3        
    lda     ram_D5                  ;3        
    sta     ram_85                  ;3        
    lda     ram_D6                  ;3        
    sta     ram_81                  ;3        
    lda     ram_D7                  ;3        
    sta     ram_86                  ;3        
    lda     ram_D8                  ;3        
    sta     ram_82                  ;3        
    lda     ram_D9                  ;3        
    sta     ram_91                  ;3        
    lda     ram_DA                  ;3        
    sta     ram_83                  ;3        
    lda     ram_DB                  ;3        
    sta     ram_88                  ;3        
    lda     #$58                    ;2        
    cmp     ram_D5                  ;3        
    bcs     L1eae                   ;2/3      
    lda     ram_E0                  ;3        
    ora     #$80                    ;2        
    sta     ram_E0                  ;3   =  63
L1eae
    lda     #$86                    ;2        
    cmp     ram_D4                  ;3        
    bcs     L1eb7                   ;2/3      
    jmp     L1ebd                   ;3   =  10
    
L1eb7
    lda     ram_D4                  ;3        
    cmp     #$13                    ;2        
    bcs     L1ede                   ;2/3 =   7
L1ebd
    lda     #$4b                    ;2        
    sta     ram_D4                  ;3        
    sed                             ;2        
    clc                             ;2        
    lda     ram_94                  ;3        
    adc     #$01                    ;2        
    sta     ram_94                  ;3        
    lda     ram_93                  ;3        
    adc     #$00                    ;2        
    sta     ram_93                  ;3        
    cld                             ;2        
    lda     #$fa                    ;2        
    sta     ram_D5                  ;3        
    sta     ram_D7                  ;3        
    lda     ram_E3                  ;3        
    ora     #$08                    ;2        
    sta     ram_E3                  ;3        
    inc     ram_E5                  ;5   =  48
L1ede
    sta     ram_EE                  ;3        
    lda     #$1e                    ;2        
    pha                             ;3        
    lda     #$f5                    ;2        
    pha                             ;3        
    lda     #$f4                    ;2        
    pha                             ;3        
    lda     #$35                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  39
    
    jmp     L10ca                   ;3   =   3
   
  IF PLUSROM = 1
    ORG     $0fd0, $ff
    RORG    $1fd0
  ELSE
    ORG     $0fd4, $ff
    RORG    $1fd4
  ENDIF

   
Start
    ldx     #$ff                    ;2        
    txs                             ;2        
    lda     #$f2                    ;2        
    pha                             ;3        
    lda     #$4f                    ;2        
    pha                             ;3   =  14
L1fdd
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    tsx                             ;2        
    lda     CXM0FB,x                ;4        
    rol                             ;2        
    rol                             ;2        
    rol                             ;2        
    rol                             ;2        
    and     #$07                    ;2        
    tax                             ;2        
    inx                             ;2   =  28
L1feb
    lda     L1ff3,x                 ;4        
    pla                             ;4        
    tax                             ;2        
    pla                             ;4        
    rts                             ;6   =  20
    

    ORG     $0ff3, $ff
    RORG    $1ff3

L1ff3
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff     ; $1ff3 (*)

  IF PLUSROM = 1
    .word (PlusROM_API - $6000)             ; PlusRom API pointer
  ELSE
    .byte   $ff,$ff                         ; $1ffa (D)
  ENDIF
    .word Start, Start



;***********************************************************
;      Bank 1 / 0..7
;***********************************************************

    SEG     CODE
    ORG     $1000
    RORG    $3000

    lda     #$01                    ;2        
    sta     ram_92                  ;3        
    lda     #$32                    ;2        
    sta     ram_E9                  ;3        
    lda     #$36                    ;2        
    cmp     ram_E5                  ;3        
    bcs     L3018                   ;2/3      
    lda     ram_E5                  ;3        
    cmp     #$64                    ;2        
    bcs     L3018                   ;2/3      
    lda     #$00                    ;2         *
    sta     ram_E5                  ;3   =  29 *
L3018
    lda     #$64                    ;2        
    sta     ram_ED                  ;3        
    lda     #$00                    ;2        
    sta     ram_DC                  ;3        
    ldx     #$dd                    ;2        
    stx     ram_8A                  ;3        
    lda     #$f6                    ;2        
    sta     ram_8B                  ;3        
    lda     #$00                    ;2        
    sta     ram_8E                  ;3        
    ldx     #$de                    ;2        
    stx     ram_8C                  ;3        
    lda     #$f6                    ;2        
    sta     ram_8D                  ;3        
    lda     #$00                    ;2        
    sta     ram_8F                  ;3        
    lda     #$fa                    ;2        
    sta     ram_D7                  ;3        
    lda     #$fa                    ;2        
    sta     ram_89                  ;3        
    lda     #$fa                    ;2        
    sta     ram_DB                  ;3        
    lda     ram_E3                  ;3        
    and     #$f7                    ;2        
    sta     ram_E3                  ;3        
    lda     #$00                    ;2        
    sta     ram_E0                  ;3        
    lda     #$00                    ;2        
    sta     ram_E3                  ;3        
    lda     #$00                    ;2        
    sta     ram_E8                  ;3        
    lda     #BLACK                  ;2        
    sta     COLUPF                  ;3        
    lda     #$f6                    ;2        
    sta     ram_F1                  ;3        
    lda     #$21                    ;2        
    sta     ram_F0                  ;3        
    lda     ram_E5                  ;3        
    cmp     #$0b                    ;2        
    beq     L306b                   ;2/3      
    jmp     L30f0                   ;3   = 103
    
L306b
    sta     ram_EE                  ;3        
    lda     #$30                    ;2        
    pha                             ;3        
    lda     #$82                    ;2        
    pha                             ;3        
    lda     #$d1                    ;2        
    pha                             ;3        
    lda     #$ef                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  39
    
    ldx     #$00                    ;2        
    lda     #$00                    ;2        
    sta     ram_9E                  ;3        
    ldy     #$00                    ;2        
    lda     #$0f                    ;2        
    sta     ram_EE                  ;3        
    lda     #$30                    ;2        
    pha                             ;3        
    lda     #$a4                    ;2        
    pha                             ;3        
    lda     #$f2                    ;2        
    pha                             ;3        
    lda     #$ff                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  50
    
    ldx     #$00                    ;2        
    lda     #$10                    ;2        
    sta     ram_9E                  ;3        
    ldy     #$01                    ;2        
    lda     #$0e                    ;2        
    sta     ram_EE                  ;3        
    lda     #$30                    ;2        
    pha                             ;3        
    lda     #$c6                    ;2        
    pha                             ;3        
    lda     #$f2                    ;2        
    pha                             ;3        
    lda     #$e8                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  50
    
    ldx     #$01                    ;2        
    ldy     #$09                    ;2        
    lda     #$1f                    ;2        
    sta     ram_EE                  ;3        
    lda     #$30                    ;2        
    pha                             ;3        
    lda     #$e4                    ;2        
    pha                             ;3        
    lda     #$f2                    ;2        
    pha                             ;3        
    lda     #$b8                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  45
    
    lda     #$4b                    ;2        
    sta     ram_D6                  ;3        
    lda     #$20                    ;2        
    sta     ram_D7                  ;3        
    jmp     L3f80                   ;3   =  13
    
L30f0
    lda     ram_E5                  ;3        
    cmp     #$18                    ;2        
    beq     L30f9                   ;2/3      
    jmp     L3136                   ;3   =  10
    
L30f9
    lda     ram_E3                  ;3        
    ora     #$80                    ;2        
    sta     ram_E3                  ;3        
    lda     #$4b                    ;2        
    sta     ram_84                  ;3        
    lda     #$30                    ;2        
    sta     ram_89                  ;3        
    lda     #$15                    ;2        
    sta     ram_D6                  ;3        
    lda     #$10                    ;2        
    sta     ram_D7                  ;3        
    lda     #$1f                    ;2        
    sta     ram_DB                  ;3        
    lda     #$3e                    ;2        
    sta     ram_DA                  ;3        
    lda     #$04                    ;2        
    sta     ram_92                  ;3        
    sta     ram_EE                  ;3        
    lda     #$31                    ;2        
    pha                             ;3        
    lda     #$32                    ;2        
    pha                             ;3        
    lda     #$d5                    ;2        
    pha                             ;3        
    lda     #$73                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  82
    
    jmp     L3f80                   ;3   =   3
    
L3136
    lda     ram_E5                  ;3        
    cmp     #$19                    ;2        
    beq     L313f                   ;2/3      
    jmp     L3188                   ;3   =  10
    
L313f
    lda     ram_E3                  ;3        
    ora     #$01                    ;2        
    sta     ram_E3                  ;3        
    sta     ram_EE                  ;3        
    lda     #$31                    ;2        
    pha                             ;3        
    lda     #$5c                    ;2        
    pha                             ;3        
    lda     #$f5                    ;2        
    pha                             ;3        
    lda     #$53                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  47
    
    lda     #$3a                    ;2        
    sta     ram_D6                  ;3        
    lda     #$58                    ;2        
    sta     ram_D7                  ;3        
    lda     #$64                    ;2        
    sta     ram_84                  ;3        
    lda     #$53                    ;2        
    sta     ram_89                  ;3        
    sta     ram_EE                  ;3        
    lda     #$31                    ;2        
    pha                             ;3        
    lda     #$84                    ;2        
    pha                             ;3        
    lda     #$d8                    ;2        
    pha                             ;3        
    lda     #$49                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  59
    
    jmp     L3f80                   ;3   =   3
    
L3188
    lda     ram_E5                  ;3        
    cmp     #$1d                    ;2        
    beq     L3191                   ;2/3      
    jmp     L331c                   ;3   =  10
    
L3191
    sta     ram_EE                  ;3         *
    lda     #$31                    ;2         *
    pha                             ;3         *
    lda     #$a8                    ;2         *
    pha                             ;3         *
    lda     #$d5                    ;2         *
    pha                             ;3         *
    lda     #$eb                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$07                    ;2         *
    jmp     Lffeb                   ;3   =  39 *
    
L31a9
    lda #$23
    sta $d6
    lda #$38
    sta $d7
    sta $ee
    lda #$31
    pha
    lda #$c8
    pha
    lda #$f5
    pha
    lda #$b6
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    ldx #$00
    lda #$06
    sta $9e
    ldy #$07
    lda #$04
    sta $ee
    lda #$31
    pha
    lda #$ea
    pha
    lda #$f2
    pha
    lda #$e8
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    ldx #$00
    lda #$0a
    sta $9e
    ldy #$08
    lda #$05
    sta $ee
    lda #$32
    pha
    lda #$0c
    pha
    lda #$f2
    pha
    lda #$ff
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    ldx #$00
    lda #$0e
    sta $9e
    ldy #$07
    lda #$0c
    sta $ee
    lda #$32
    pha
    lda #$2e
    pha
    lda #$f2
    pha
    lda #$e8
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    ldx #$00
    lda #$0a
    sta $9e
    ldy #$08
    lda #$0d
    sta $ee
    lda #$32
    pha
    lda #$50
    pha
    lda #$f2
    pha
    lda #$ff
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    ldx #$00
    lda #$16
    sta $9e
    ldy #$07
    lda #$14
    sta $ee
    lda #$32
    pha
    lda #$72
    pha
    lda #$f2
    pha
    lda #$e8
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    ldx #$00
    lda #$0a
    sta $9e
    ldy #$08
    lda #$15
    sta $ee
    lda #$32
    pha
    lda #$94
    pha
    lda #$f2
    pha
    lda #$ff
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    ldx #$01
    ldy #$02
    lda #$1f
    sta $ee
    lda #$32
    pha
    lda #$b2
    pha
    lda #$f2
    pha
    lda #$b8
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    ldx #$00
    lda #$0a
    sta $9e
    ldy #$05
    lda #$09
    sta $ee
    lda #$32
    pha
    lda #$d4
    pha
    lda #$f2
    pha
    lda #$ff
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    ldx #$00
    lda #$0a
    sta $9e
    ldy #$05
    lda #$11
    sta $ee
    lda #$32
    pha
    lda #$f6
    pha
    lda #$f2
    pha
    lda #$ff
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    ldx #$00
    lda #$0a
    sta $9e
    ldy #$05
    lda #$19
    sta $ee
    lda #$33
    pha
    lda #$18
    pha
    lda #$f2
    pha
    lda #$ff
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    jmp L3f80

L331c
    lda     ram_E5                  ;3        
    cmp     #$1e                    ;2        
    beq     L3325                   ;2/3      
    jmp     L3374                   ;3   =  10
    
L3325
    sta     ram_EE                  ;3         *
    lda     #$33                    ;2         *
    pha                             ;3         *
    lda     #$3c                    ;2         *
    pha                             ;3         *
    lda     #$d7                    ;2         *
    pha                             ;3         *
    lda     #$6f                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$07                    ;2         *
    jmp     Lffeb                   ;3   =  39 *
    
    lda #BLACK
    sta COLUPF
    lda #$f6
    sta ram_F1
    lda #$22
    sta ram_F0
    sta $ee
    lda #$33
    pha
    lda #$60
    pha
    lda #$db
    pha
    lda #$80
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$07
    jmp Lffeb
    lda #$2f
    sta $d6
    lda #$50
    sta $d7
    lda #$4b
    sta $84
    lda #$16
    sta $89
    jmp L3f80
    
L3374
    lda     ram_E5                  ;3        
    cmp     #$21                    ;2        
    beq     L337d                   ;2/3      
    jmp     L33ac                   ;3   =  10
    
L337d
    lda     #$62                    ;2         *
    sta     ram_D7                  ;3         *
    lda     #$86                    ;2         *
    sta     ram_DA                  ;3         *
    lda     #BLACK                  ;2         *
    sta     COLUPF                  ;3         *
    lda     #$f6                    ;2         *
    sta     ram_F1                  ;3         *
    lda     #$23                    ;2         *
    sta     ram_F0                  ;3         *
    sta     ram_EE                  ;3         *
    lda     #$33                    ;2         *
    pha                             ;3         *
    lda     #$a8                    ;2         *
    pha                             ;3         *
    lda     #$d7                    ;2         *
    pha                             ;3         *
    lda     #$ab                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$07                    ;2         *
    jmp     Lffeb                   ;3   =  64 *
    
    ; never reached? $33a9 (*)
    jmp     L3f80
    
L33ac
    lda     ram_E5                  ;3        
    cmp     #$31                    ;2        
    bne     L33be                   ;2/3      
    lda     #BLACK                  ;2         *
    sta     COLUPF                  ;3         *
    lda     #$f7                    ;2         *
    sta     ram_F1                  ;3         *
    lda     #$04                    ;2         *
    sta     ram_F0                  ;3   =  22 *
L33be
    lda     ram_E5                  ;3        
    cmp     #$35                    ;2        
    bne     L33d0                   ;2/3      
    lda     #BLACK                  ;2         *
    sta     COLUPF                  ;3         *
    lda     #$f7                    ;2         *
    sta     ram_F1                  ;3         *
    lda     #$05                    ;2         *
    sta     ram_F0                  ;3   =  22 *
L33d0
    lda     ram_E5                  ;3        
    cmp     #$00                    ;2        
    bne     L3417                   ;2/3!     
    lda     #$1c                    ;2        
    sta     ram_D6                  ;3        
    lda     #$50                    ;2        
    sta     ram_D7                  ;3        
    lda     ram_E3                  ;3        
    ora     #$10                    ;2        
    sta     ram_E3                  ;3        
    sta     ram_EE                  ;3        
    lda     #$33                    ;2        
    pha                             ;3        
    lda     #$fb                    ;2        
    pha                             ;3        
    lda     #$dc                    ;2        
    pha                             ;3        
    lda     #$58                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  64
    
    sta     ram_EE                  ;3        
    lda     #$34                    ;2        
    pha                             ;3        
    lda     #$13                    ;2        
    pha                             ;3        
    lda     #$d1                    ;2        
    pha                             ;3        
    lda     #$ef                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  39
    
    jmp     L3f80                   ;3   =   3
    
L3417
    lda     ram_E5                  ;3        
    cmp     #$01                    ;2        
    bne     L3470                   ;2/3      
    lda     ram_E3                  ;3        
    ora     #$10                    ;2        
    sta     ram_E3                  ;3        
    sta     ram_EE                  ;3        
    lda     #$34                    ;2        
    pha                             ;3        
    lda     #$3a                    ;2        
    pha                             ;3        
    lda     #$dc                    ;2        
    pha                             ;3        
    lda     #$58                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  54
    
    lda     #$7c                    ;2        
    sta     ram_D6                  ;3        
    lda     #$50                    ;2        
    sta     ram_D7                  ;3        
    lda     #$3e                    ;2        
    sta     ram_DB                  ;3        
    lda     #$5a                    ;2        
    sta     ram_DA                  ;3        
    ldx     #$01                    ;2        
    lda     #$0a                    ;2        
    sta     ram_9E                  ;3        
    ldy     #$09                    ;2        
    lda     #$0f                    ;2        
    sta     ram_EE                  ;3        
    lda     #$34                    ;2        
    pha                             ;3        
    lda     #$6c                    ;2        
    pha                             ;3        
    lda     #$f2                    ;2        
    pha                             ;3        
    lda     #$ff                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  70
    
    jmp     L3f80                   ;3   =   3
    
L3470
    lda     ram_E5                  ;3        
    cmp     #$02                    ;2        
    bne     L34a5                   ;2/3      
    lda     #$34                    ;2        
    sta     ram_D6                  ;3        
    lda     #$50                    ;2        
    sta     ram_D7                  ;3        
    lda     #$22                    ;2        
    sta     NUSIZ1                  ;3        
    lda     #$3c                    ;2        
    sta     ram_DA                  ;3        
    lda     #$2a                    ;2        
    sta     ram_DB                  ;3        
    sta     ram_EE                  ;3        
    lda     #$34                    ;2        
    pha                             ;3        
    lda     #$a1                    ;2        
    pha                             ;3        
    lda     #$d2                    ;2        
    pha                             ;3        
    lda     #$2b                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  71
    
    jmp     L3f80                   ;3   =   3
    
L34a5
    lda     ram_E5                  ;3        
    cmp     #$03                    ;2        
    bne     L34d2                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$34                    ;2        
    pha                             ;3        
    lda     #$c2                    ;2        
    pha                             ;3        
    lda     #$d2                    ;2        
    pha                             ;3        
    lda     #$67                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  46
    
    lda     #$22                    ;2        
    sta     NUSIZ1                  ;3        
    lda     #$3c                    ;2        
    sta     ram_D6                  ;3        
    lda     #$14                    ;2        
    sta     ram_D7                  ;3        
    jmp     L3f80                   ;3   =  18
    
L34d2
    lda     ram_E5                  ;3        
    cmp     #$04                    ;2        
    bne     L34ff                   ;2/3      
    lda     #$01                    ;2        
    sta     ram_E7                  ;3        
    lda     #$38                    ;2        
    sta     ram_DB                  ;3        
    lda     #$5a                    ;2        
    sta     ram_DA                  ;3        
    sta     ram_EE                  ;3        
    lda     #$34                    ;2        
    pha                             ;3        
    lda     #$fb                    ;2        
    pha                             ;3        
    lda     #$d2                    ;2        
    pha                             ;3        
    lda     #$a3                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  61
    
    jmp     L3f80                   ;3   =   3
    
L34ff
    lda     ram_E5                  ;3        
    cmp     #$05                    ;2        
    bne     L354c                   ;2/3      
    lda     #$23                    ;2        
    sta     ram_D6                  ;3        
    lda     #$00                    ;2        
    sta     ram_D7                  ;3        
    sta     ram_DB                  ;3        
    lda     #$8a                    ;2        
    sta     ram_DA                  ;3        
    sta     ram_EE                  ;3        
    lda     #$35                    ;2        
    pha                             ;3        
    lda     #$2a                    ;2        
    pha                             ;3        
    lda     #$d2                    ;2        
    pha                             ;3        
    lda     #$df                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  64
    
    ldx     #$01                    ;2        
    ldy     #$09                    ;2        
    lda     #$1f                    ;2        
    sta     ram_EE                  ;3        
    lda     #$35                    ;2        
    pha                             ;3        
    lda     #$48                    ;2        
    pha                             ;3        
    lda     #$f2                    ;2        
    pha                             ;3        
    lda     #$b8                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  45
    
    jmp     L3f80                   ;3   =   3
    
L354c
    lda     ram_E5                  ;3        
    cmp     #$06                    ;2        
    bne     L35bd                   ;2/3      
    lda     #$5a                    ;2        
    sta     ram_D6                  ;3        
    lda     #$48                    ;2        
    sta     ram_D7                  ;3        
    lda     #$00                    ;2        
    sta     ram_DB                  ;3        
    sta     ram_EE                  ;3        
    lda     #$35                    ;2        
    pha                             ;3        
    lda     #$75                    ;2        
    pha                             ;3        
    lda     #$d1                    ;2        
    pha                             ;3        
    lda     #$ef                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  61
    
    ldx     #$00                    ;2        
    lda     #$05                    ;2        
    sta     ram_9E                  ;3        
    ldy     #$00                    ;2        
    lda     #$0e                    ;2        
    sta     ram_EE                  ;3        
    lda     #$35                    ;2        
    pha                             ;3        
    lda     #$97                    ;2        
    pha                             ;3        
    lda     #$f2                    ;2        
    pha                             ;3        
    lda     #$ff                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  50
    
    ldx     #$00                    ;2        
    lda     #$05                    ;2        
    sta     ram_9E                  ;3        
    ldy     #$00                    ;2        
    lda     #$10                    ;2        
    sta     ram_EE                  ;3        
    lda     #$35                    ;2        
    pha                             ;3        
    lda     #$b9                    ;2        
    pha                             ;3        
    lda     #$f2                    ;2        
    pha                             ;3        
    lda     #$ff                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  50
    
    jmp     L3f80                   ;3   =   3
    
L35bd
    lda     ram_E5                  ;3        
    cmp     #$07                    ;2        
    bne     L35f0                   ;2/3      
    lda     #$20                    ;2        
    sta     ram_D7                  ;3        
    lda     #$50                    ;2        
    sta     ram_D6                  ;3        
    lda     #$02                    ;2        
    sta     ram_E7                  ;3        
    lda     ram_E3                  ;3        
    ora     #$80                    ;2        
    sta     ram_E3                  ;3        
    sta     ram_EE                  ;3        
    lda     #$35                    ;2        
    pha                             ;3        
    lda     #$ec                    ;2        
    pha                             ;3        
    lda     #$d3                    ;2        
    pha                             ;3        
    lda     #$1b                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  69
    
    jmp     L3f80                   ;3   =   3
    
L35f0
    lda     ram_E5                  ;3        
    cmp     #$08                    ;2        
    bne     L362d                   ;2/3!     
    lda     #$00                    ;2        
    sta     ram_D7                  ;3        
    sta     ram_EE                  ;3        
    lda     #$36                    ;2        
    pha                             ;3        
    lda     #$11                    ;2        
    pha                             ;3        
    lda     #$f5                    ;2        
    pha                             ;3        
    lda     #$8c                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  51
    
    sta     ram_EE                  ;3        
    lda     #$36                    ;2        
    pha                             ;3        
    lda     #$29                    ;2        
    pha                             ;3        
    lda     #$d3                    ;2        
    pha                             ;3        
    lda     #$57                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  39
    
    jmp     L3f80                   ;3   =   3
    
L362d
    lda     ram_E5                  ;3        
    cmp     #$09                    ;2        
    bne     L3666                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$36                    ;2        
    pha                             ;3        
    lda     #$4a                    ;2        
    pha                             ;3        
    lda     #$d3                    ;2        
    pha                             ;3        
    lda     #$93                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  46
    
    sta     ram_EE                  ;3        
    lda     #$36                    ;2        
    pha                             ;3        
    lda     #$62                    ;2        
    pha                             ;3        
    lda     #$dc                    ;2        
    pha                             ;3        
    lda     #$49                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  39
    
    jmp     L3f80                   ;3   =   3
    
L3666
    lda     ram_E5                  ;3        
    cmp     #$0a                    ;2        
    bne     L36a7                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$36                    ;2        
    pha                             ;3        
    lda     #$83                    ;2        
    pha                             ;3        
    lda     #$d8                    ;2        
    pha                             ;3        
    lda     #$85                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  46
    
    sta     ram_EE                  ;3        
    lda     #$36                    ;2        
    pha                             ;3        
    lda     #$9b                    ;2        
    pha                             ;3        
    lda     #$f5                    ;2        
    pha                             ;3        
    lda     #$53                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  39
    
    lda     #$2f                    ;2        
    sta     ram_D6                  ;3        
    lda     #$57                    ;2        
    sta     ram_D7                  ;3        
    jmp     L3f80                   ;3   =  13
    
L36a7
    lda     ram_E5                  ;3        
    cmp     #$0c                    ;2        
    bne     L36c8                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$36                    ;2        
    pha                             ;3        
    lda     #$c4                    ;2        
    pha                             ;3        
    lda     #$d4                    ;2        
    pha                             ;3        
    lda     #$0b                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  46
    
    jmp     L3f80                   ;3   =   3
    
L36c8
    lda     ram_E5                  ;3        
    cmp     #$0d                    ;2        
    bne     L36fc                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$36                    ;2        
    pha                             ;3        
    lda     #$e5                    ;2        
    pha                             ;3        
    lda     #$d4                    ;2        
    pha                             ;3        
    lda     #$83                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  46
    
    lda     #$49                    ;2        
    sta     ram_D6                  ;3        
    lda     #$20                    ;2        
    sta     ram_D7                  ;3        
    lda     #$0a                    ;2        
    sta     ram_E7                  ;3        
    lda     ram_E9                  ;3        
    clc                             ;2        
    adc     #$64                    ;2        
    sta     ram_E9                  ;3        
    jmp     L3f80                   ;3   =  28
    
L36fc
    lda     ram_E5                  ;3        
    cmp     #$0e                    ;2        
    bne     L3705                   ;2/3      
    jmp     L370b                   ;3   =  10
    
L3705
    lda     ram_E5                  ;3        
    cmp     #$16                    ;2        
    bne     L375c                   ;2/3 =   7
L370b
    sta     ram_EE                  ;3        
    lda     #$37                    ;2        
    pha                             ;3        
    lda     #$22                    ;2        
    pha                             ;3        
    lda     #$d4                    ;2        
    pha                             ;3        
    lda     #$bf                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  39
    
    sta     ram_EE                  ;3        
    lda     #$37                    ;2        
    pha                             ;3        
    lda     #$3a                    ;2        
    pha                             ;3        
    lda     #$da                    ;2        
    pha                             ;3        
    lda     #$5f                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  39
    
    ldx     #$01                    ;2        
    ldy     #$03                    ;2        
    lda     #$00                    ;2        
    sta     ram_EE                  ;3        
    lda     #$37                    ;2        
    pha                             ;3        
    lda     #$58                    ;2        
    pha                             ;3        
    lda     #$f2                    ;2        
    pha                             ;3        
    lda     #$b8                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  45
    
    jmp     L3f80                   ;3   =   3
    
L375c
    lda     ram_E5                  ;3        
    cmp     #$0f                    ;2        
    bne     L37b9                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$37                    ;2        
    pha                             ;3        
    lda     #$79                    ;2        
    pha                             ;3        
    lda     #$d3                    ;2        
    pha                             ;3        
    lda     #$93                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  46
    
    ldx     #$01                    ;2        
    ldy     #$03                    ;2        
    lda     #$00                    ;2        
    sta     ram_EE                  ;3        
    lda     #$37                    ;2        
    pha                             ;3        
    lda     #$97                    ;2        
    pha                             ;3        
    lda     #$f2                    ;2        
    pha                             ;3        
    lda     #$b8                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  45
    
    ldx     #$00                    ;2        
    ldy     #$03                    ;2        
    lda     #$1f                    ;2        
    sta     ram_EE                  ;3        
    lda     #$37                    ;2        
    pha                             ;3        
    lda     #$b5                    ;2        
    pha                             ;3        
    lda     #$f2                    ;2        
    pha                             ;3        
    lda     #$b8                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  45
    
    jmp     L3f80                   ;3   =   3
    
L37b9
    lda     ram_E5                  ;3        
    cmp     #$10                    ;2        
    bne     L3816                   ;2/3!     
    sta     ram_EE                  ;3        
    lda     #$37                    ;2        
    pha                             ;3        
    lda     #$d6                    ;2        
    pha                             ;3        
    lda     #$d4                    ;2        
    pha                             ;3        
    lda     #$fb                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  46
    
    ldx     #$01                    ;2        
    ldy     #$03                    ;2        
    lda     #$00                    ;2        
    sta     ram_EE                  ;3        
    lda     #$37                    ;2        
    pha                             ;3        
    lda     #$f4                    ;2        
    pha                             ;3        
    lda     #$f2                    ;2        
    pha                             ;3        
    lda     #$b8                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  45
    
    ldx     #$00                    ;2        
    ldy     #$03                    ;2        
    lda     #$1f                    ;2        
    sta     ram_EE                  ;3        
    lda     #$38                    ;2        
    pha                             ;3        
    lda     #$12                    ;2        
    pha                             ;3        
    lda     #$f2                    ;2        
    pha                             ;3        
    lda     #$b8                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  45
    
    jmp     L3f80                   ;3   =   3
    
L3816
    lda     ram_E5                  ;3        
    cmp     #$11                    ;2        
    bne     L381f                   ;2/3      
    jmp     L3825                   ;3   =  10
    
L381f
    lda     ram_E5                  ;3        
    cmp     #$27                    ;2        
    bne     L387c                   ;2/3 =   7
L3825
    sta     ram_EE                  ;3        
    lda     #$38                    ;2        
    pha                             ;3        
    lda     #$3c                    ;2        
    pha                             ;3        
    lda     #$d5                    ;2        
    pha                             ;3        
    lda     #$37                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  39
    
    ldx     #$01                    ;2        
    ldy     #$03                    ;2        
    lda     #$00                    ;2        
    sta     ram_EE                  ;3        
    lda     #$38                    ;2        
    pha                             ;3        
    lda     #$5a                    ;2        
    pha                             ;3        
    lda     #$f2                    ;2        
    pha                             ;3        
    lda     #$b8                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  45
    
    ldx     #$00                    ;2        
    ldy     #$03                    ;2        
    lda     #$1f                    ;2        
    sta     ram_EE                  ;3        
    lda     #$38                    ;2        
    pha                             ;3        
    lda     #$78                    ;2        
    pha                             ;3        
    lda     #$f2                    ;2        
    pha                             ;3        
    lda     #$b8                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  45
    
    jmp     L3f80                   ;3   =   3
    
L387c
    lda     ram_E5                  ;3        
    cmp     #$12                    ;2        
    bne     L38b5                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$38                    ;2        
    pha                             ;3        
    lda     #$99                    ;2        
    pha                             ;3        
    lda     #$d9                    ;2        
    pha                             ;3        
    lda     #$b1                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  46
    
    sta     ram_EE                  ;3        
    lda     #$38                    ;2        
    pha                             ;3        
    lda     #$b1                    ;2        
    pha                             ;3        
    lda     #$da                    ;2        
    pha                             ;3        
    lda     #$5f                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  39
    
    jmp     L3f80                   ;3   =   3
    
L38b5
    lda     ram_E5                  ;3        
    cmp     #$13                    ;2        
    bne     L38ee                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$38                    ;2        
    pha                             ;3        
    lda     #$d2                    ;2        
    pha                             ;3        
    lda     #$d5                    ;2        
    pha                             ;3        
    lda     #$37                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  46
    
    sta     ram_EE                  ;3        
    lda     #$38                    ;2        
    pha                             ;3        
    lda     #$ea                    ;2        
    pha                             ;3        
    lda     #$da                    ;2        
    pha                             ;3        
    lda     #$5f                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  39
    
    jmp     L3f80                   ;3   =   3
    
L38ee
    lda     ram_E5                  ;3        
    cmp     #$14                    ;2        
    bne     L390f                   ;2/3!     
    sta     ram_EE                  ;3        
    lda     #$39                    ;2        
    pha                             ;3        
    lda     #$0b                    ;2        
    pha                             ;3        
    lda     #$d4                    ;2        
    pha                             ;3        
    lda     #$fb                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  46
    
    jmp     L3f80                   ;3   =   3
    
L390f
    lda     ram_E5                  ;3        
    cmp     #$15                    ;2        
    bne     L3930                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$39                    ;2        
    pha                             ;3        
    lda     #$2c                    ;2        
    pha                             ;3        
    lda     #$d3                    ;2        
    pha                             ;3        
    lda     #$93                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  46
    
    jmp     L3f80                   ;3   =   3
    
L3930
    lda     ram_E5                  ;3        
    cmp     #$17                    ;2        
    bne     L3959                   ;2/3      
    lda     #$02                    ;2        
    sta     ram_E7                  ;3        
    lda     #$00                    ;2        
    sta     ram_89                  ;3        
    sta     ram_EE                  ;3        
    lda     #$39                    ;2        
    pha                             ;3        
    lda     #$55                    ;2        
    pha                             ;3        
    lda     #$d5                    ;2        
    pha                             ;3        
    lda     #$af                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  56
    
    jmp     L3f80                   ;3   =   3
    
L3959
    lda     ram_E5                  ;3        
    cmp     #$1a                    ;2        
    bne     L399e                   ;2/3      
    lda     #$28                    ;2        
    sta     ram_D6                  ;3        
    sta     ram_D7                  ;3        
    sta     ram_EE                  ;3        
    lda     #$39                    ;2        
    pha                             ;3        
    lda     #$7c                    ;2        
    pha                             ;3        
    lda     #$d5                    ;2        
    pha                             ;3        
    lda     #$eb                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  54
    
    ldx     #$01                    ;2        
    ldy     #$02                    ;2        
    lda     #$1f                    ;2        
    sta     ram_EE                  ;3        
    lda     #$39                    ;2        
    pha                             ;3        
    lda     #$9a                    ;2        
    pha                             ;3        
    lda     #$f2                    ;2        
    pha                             ;3        
    lda     #$b8                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  45
    
    jmp     L3f80                   ;3   =   3
    
L399e
    lda     ram_E5                  ;3        
    cmp     #$1b                    ;2        
    bne     L39bf                   ;2/3      
    sta     ram_EE                  ;3         *
    lda     #$39                    ;2         *
    pha                             ;3         *
    lda     #$bb                    ;2         *
    pha                             ;3         *
    lda     #$d4                    ;2         *
    pha                             ;3         *
    lda     #$47                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$07                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    ; never reached? $39bc (*)
    jmp     L3f80
   
L39bf
    lda     ram_E5                  ;3        
    cmp     #$1c                    ;2        
    bne     L39e8                   ;2/3      
    sta     ram_EE                  ;3         *
    lda     #$39                    ;2         *
    pha                             ;3         *
    lda     #$dc                    ;2         *
    pha                             ;3         *
    lda     #$d6                    ;2         *
    pha                             ;3         *
    lda     #$63                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$07                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    ; never reached ? $39dd (*)
    lda #$86
    sta $da
    lda #$01
    sta $e7
    ; never reached? $39e5 (*)
    jmp L3f80

L39e8
    lda     ram_E5                  ;3        
    cmp     #$1f                    ;2        
    bne     L3a15                   ;2/3!     
    sta     ram_EE                  ;3         *
    lda     #$3a                    ;2         *
    pha                             ;3         *
    lda     #$05                    ;2         *
    pha                             ;3         *
    lda     #$d3                    ;2         *
    pha                             ;3         *
    lda     #$cf                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$07                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    ; never reached? $3a06 (*)
    lda #$2c
    sta $d6
    lda #$28
    sta $d7
    lda #$0c
    sta ram_87
    jmp L3f80
    
L3a15
    lda     ram_E5                  ;3        
    cmp     #$20                    ;2        
    bne     L3a1e                   ;2/3      
    jmp     L3a24                   ;3   =  10 *
    
L3a1e
    lda     ram_E5                  ;3        
    cmp     #$23                    ;2        
    bne     L3a65                   ;2/3 =   7
L3a24
    sta     ram_EE                  ;3         *
    lda     #$3a                    ;2         *
    pha                             ;3         *
    lda     #$3b                    ;2         *
    pha                             ;3         *
    lda     #$d8                    ;2         *
    pha                             ;3         *
    lda     #$c1                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$07                    ;2         *
    jmp     Lffeb                   ;3   =  39 *
    
    lda #$16
    sta $da
    lda #$20
    sta $db
    ldx #$01
    ldy #$09
    lda #$1f
    sta $ee
    lda #$3a
    pha
    lda #$61
    pha
    lda #$f2
    pha
    lda #$b8
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    jmp L3f80                         ; $3a5c (*) never reached?
    
L3a65
    lda     ram_E5                  ;3        
    cmp     #$22                    ;2        
    bne     L3a96                   ;2/3      
    sta     ram_EE                  ;3         *
    lda     #$3a                    ;2         *
    pha                             ;3         *
    lda     #$82                    ;2         *
    pha                             ;3         *
    lda     #$d8                    ;2         *
    pha                             ;3         *
    lda     #$fd                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$07                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    lda #$01
    sta $e7
    lda #$32
    sta $d6
    lda #$62
    sta $da
    lda #$36
    sta $db
    jmp L3f80
    
L3a96
    lda     ram_E5                  ;3        
    cmp     #$24                    ;2        
    bne     L3ade                   ;2/3      
    sta     ram_EE                  ;3         *
    lda     #$3a                    ;2         *
    pha                             ;3         *
    lda     #$b3                    ;2         *
    pha                             ;3         *
    lda     #$d6                    ;2         *
    pha                             ;3         *
    lda     #$27                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$07                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    lda #$05
    sta $e7
    sta $ee
    lda #$3a
    pha
    lda #$cf
    pha
    lda #$db
    pha
    lda #$ad
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$07
    jmp Lffeb
    lda #$12
    sta $d6
    lda $e9
    clc
    adc #$64
    sta $e9
    jmp L3f80
    
L3ade
    lda     ram_E5                  ;3        
    cmp     #$25                    ;2        
    bne     L3b35                   ;2/3!     
    sta     ram_EE                  ;3         *
    lda     #$3a                    ;2         *
    pha                             ;3         *
    lda     #$fb                    ;2         *
    pha                             ;3         *
    lda     #$d4                    ;2         *
    pha                             ;3         *
    lda     #$bf                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$07                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    sta $ee
    lda #$3b
    pha
    lda #$13
    pha
    lda #$da
    pha
    lda #$5f
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$07
    jmp Lffeb

    ldx #$01
    ldy #$03
    lda #$00
    sta $ee
    lda #$3b
    pha
    lda #$31
    pha
    lda #$f2
    pha
    lda #$b8
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    jmp L3f80                       ;3 never reached?        
    
L3b35
    lda     ram_E5                  ;3        
    cmp     #$26                    ;2        
    bne     L3b7c                   ;2/3      
    sta     ram_EE                  ;3         *
    lda     #$3b                    ;2         *
    pha                             ;3         *
    lda     #$52                    ;2         *
    pha                             ;3         *
    lda     #$d9                    ;2         *
    pha                             ;3         *
    lda     #$75                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$07                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    lda #$00
    sta $db
    lda #$80
    sta $d6
    ldx #$01
    ldy #$03
    lda #$00
    sta $ee
    lda #$3b
    pha
    lda #$78
    pha
    lda #$f2
    pha
    lda #$b8
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    jmp L3f80
    
L3b7c
    lda     ram_E5                  ;3        
    cmp     #$28                    ;2        
    bne     L3bd3                   ;2/3      
    sta     ram_EE                  ;3         *
    lda     #$3b                    ;2         *
    pha                             ;3         *
    lda     #$99                    ;2         *
    pha                             ;3         *
    lda     #$da                    ;2         *
    pha                             ;3         *
    lda     #$d8                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$07                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    sta $ee
    lda #$3b
    pha
    lda #$b1
    pha
    lda #$d9
    pha
    lda #$39
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$07
    jmp Lffeb
    ldx #$01
    ldy #$02
    lda #$00
    sta $ee
    lda #$3b
    pha
    lda #$cf
    pha
    lda #$f2
    pha
    lda #$b8
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    jmp L3f80                       ;3 never reached?
    
L3bd3
    lda     ram_E5                  ;3        
    cmp     #$29                    ;2        
    bne     L3c2b                   ;2/3!     
    sta     ram_EE                  ;3         *
    lda     #$3b                    ;2         *
    pha                             ;3         *
    lda     #$f0                    ;2         *
    pha                             ;3         *
    lda     #$da                    ;2         *
    pha                             ;3         *
    lda     #$5f                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$07                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    lda #$18
    sta $d6
    sta $ee
    lda #$3c
    pha
    lda #$0c
    pha
    lda #$d2
    pha
    lda #$67
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$07
    jmp Lffeb
    ldx #$00
    ldy #$03
    lda #$1f
    sta $ee
    lda #$3c
    pha
    lda #$2a
    pha
    lda #$f2
    pha
    lda #$b8
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    
L3c2b
    lda     ram_E5                  ;3        
    cmp     #$2a                    ;2        
    bne     L3c82                   ;2/3      
    sta     ram_EE                  ;3         *
    lda     #$3c                    ;2         *
    pha                             ;3         *
    lda     #$48                    ;2         *
    pha                             ;3         *
    lda     #$da                    ;2         *
    pha                             ;3         *
    lda     #$d8                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$07                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    sta $ee
    lda #$3c
    pha
    lda #$60
    pha
    lda #$d9
    pha
    lda #$39
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$07
    jmp Lffeb
    ldx #$01
    ldy #$02
    lda #$1f
    sta $ee
    lda #$3c
    pha
    lda #$7e
    pha
    lda #$f2
    pha
    lda #$b8
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    jmp L3f80
    
L3c82
    lda     ram_E5                  ;3        
    cmp     #$2b                    ;2        
    bne     L3cdf                   ;2/3      
    sta     ram_EE                  ;3         *
    lda     #$3c                    ;2         *
    pha                             ;3         *
    lda     #$9f                    ;2         *
    pha                             ;3         *
    lda     #$d5                    ;2         *
    pha                             ;3         *
    lda     #$37                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$07                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    ldx #$00
    ldy #$03
    lda #$00
    sta $ee
    lda #$3c
    pha
    lda #$bd
    pha
    lda #$f2
    pha
    lda #$b8
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    ldx #$01
    ldy #$03
    lda #$1f
    sta $ee
    lda #$3c
    pha
    lda #$db
    pha
    lda #$f2
    pha
    lda #$b8
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    jmp L3f80
    
L3cdf
    lda     ram_E5                  ;3        
    cmp     #$2c                    ;2        
    bne     L3d2c                   ;2/3!     
    sta     ram_EE                  ;3         *
    lda     #$3c                    ;2         *
    pha                             ;3         *
    lda     #$fc                    ;2         *
    pha                             ;3         *
    lda     #$d9                    ;2         *
    pha                             ;3         *
    lda     #$75                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$07                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    lda #$80
    sta $d6
    lda #$32
    sta $db
    lda $e8
    ora #$02
    sta $e8
    ldx #$01
    ldy #$02
    lda #$1f
    sta $ee
    lda #$3d
    pha
    lda #$28
    pha
    lda #$f2
    pha
    lda #$b8
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    jmp L3f80
    
L3d2c
    lda     ram_E5                  ;3        
    cmp     #$2d                    ;2        
    bne     L3d65                   ;2/3      
    sta     ram_EE                  ;3         *
    lda     #$3d                    ;2         *
    pha                             ;3         *
    lda     #$49                    ;2         *
    pha                             ;3         *
    lda     #$d4                    ;2         *
    pha                             ;3         *
    lda     #$bf                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$07                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    sta $ee
    lda #$3d
    pha
    lda #$61
    pha
    lda #$da
    pha
    lda #$5f
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$07
    jmp Lffeb
    jmp L3f80
    
L3d65
    lda     ram_E5                  ;3        
    cmp     #$2e                    ;2        
    bne     L3db2                   ;2/3      
    sta     ram_EE                  ;3         *
    lda     #$3d                    ;2         *
    pha                             ;3         *
    lda     #$82                    ;2         *
    pha                             ;3         *
    lda     #$d6                    ;2         *
    pha                             ;3         *
    lda     #$9f                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$07                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    lda #$46
    sta $db
    lda #$18
    sta $da
    sta $ee
    lda #$3d
    pha
    lda #$a2
    pha
    lda #$dc
    pha
    lda #$2b
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$07
    jmp Lffeb
    lda #$14
    sta $d6
    lda #$48
    sta $d7
    lda #$01
    sta $e7
    jmp L3f80
    
L3db2
    lda     ram_E5                  ;3        
    cmp     #$2f                    ;2        
    bne     L3dd3                   ;2/3      
    sta     ram_EE                  ;3         *
    lda     #$3d                    ;2         *
    pha                             ;3         *
    lda     #$cf                    ;2         *
    pha                             ;3         *
    lda     #$d6                    ;2         *
    pha                             ;3         *
    lda     #$db                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$07                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    jmp     L3f80                   ;3 never reached?
    
L3dd3
    lda     ram_E5                  ;3        
    cmp     #$30                    ;2        
    bne     L3e18                   ;2/3!     
    sta     ram_EE                  ;3         *
    lda     #$3d                    ;2         *
    pha                             ;3         *
    lda     #$f0                    ;2         *
    pha                             ;3         *
    lda     #$d1                    ;2         *
    pha                             ;3         *
    lda     #$b3                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$07                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    sta $ee
    lda #$3e
    pha
    lda #$08
    pha
    lda #$f5
    pha
    lda #$c5
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    lda #$4a
    sta $d6
    lda #$47
    sta $d7
    lda #$00
    sta $db
    jmp L3f80

L3e18
    lda     ram_E5                  ;3        
    cmp     #$31                    ;2        
    bne     L3e3d                   ;2/3      
    sta     ram_EE                  ;3         *
    lda     #$3e                    ;2         *
    pha                             ;3         *
    lda     #$35                    ;2         *
    pha                             ;3         *
    lda     #$d0                    ;2         *
    pha                             ;3         *
    lda     #$b3                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$07                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    lda #$05
    sta $e7
    jmp L3f80
   
L3e3d
    lda     ram_E5                  ;3        
    cmp     #$32                    ;2        
    bne     L3e7a                   ;2/3      
    sta     ram_EE                  ;3         *
    lda     #$3e                    ;2         *
    pha                             ;3         *
    lda     #$5a                    ;2         *
    pha                             ;3         *
    lda     #$d0                    ;2         *
    pha                             ;3         *
    lda     #$77                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$07                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    lda #$1c
    sta $d6
    lda #$18
    sta $d7
    lda #$50
    sta $da
    lda #$47
    sta $db
    lda #$48
    sta $89
    lda #$01
    sta $e7
    lda #$7c
    sta $84
    jmp L3f80

L3e7a
    lda     ram_E5                  ;3        
    cmp     #$33                    ;2        
    bne     L3ebd                   ;2/3      
    sta     ram_EE                  ;3         *
    lda     #$3e                    ;2         *
    pha                             ;3         *
    lda     #$97                    ;2         *
    pha                             ;3         *
    lda     #$d0                    ;2         *
    pha                             ;3         *
    lda     #$3b                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$07                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    lda $e3
    ora #$80
    sta $e3
    sta $ee
    lda #$3e
    pha
    lda #$b5
    pha
    lda #$f6
    pha
    lda #$01
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    lda #$74
    sta $d6
    jmp L3f80
    
L3ebd
    lda     ram_E5                  ;3        
    cmp     #$34                    ;2        
    bne     L3ee6                   ;2/3      
    sta     ram_EE                  ;3         *
    lda     #$3e                    ;2         *
    pha                             ;3         *
    lda     #$da                    ;2         *
    pha                             ;3         *
    lda     #$d7                    ;2         *
    pha                             ;3         *
    lda     #$33                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$07                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    lda #$82
    sta $d6
    lda #$40
    sta $d7
    jmp     L3f80
    
L3ee6
    lda     ram_E5                  ;3        
    cmp     #$35                    ;2        
    bne     L3f36                   ;2/3!     
    sta     ram_EE                  ;3         *
    lda     #$3f                    ;2         *
    pha                             ;3         *
    lda     #$03                    ;2         *
    pha                             ;3         *
    lda     #$d0                    ;2         *
    pha                             ;3         *
    lda     #$ef                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$07                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    lda $e9
    clc
    adc #$64
    sta $e9
    sta $ee
    lda #$3f
    pha
    lda #$22
    pha
    lda #$f6
    pha
    lda #$10
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    lda #$7c
    sta $d6
    lda #$38
    sta $d7
    lda #$00
    sta $8f
    lda #$0a
    sta $e7
    jmp L3f80
    
L3f36
    lda     ram_E5                  ;3        
    cmp     #$36                    ;2        
    bne     L3f5f                   ;2/3      
    sta     ram_EE                  ;3         *
    lda     #$3f                    ;2         *
    pha                             ;3         *
    lda     #$53                    ;2         *
    pha                             ;3         *
    lda     #$d1                    ;2         *
    pha                             ;3         *
    lda     #$2b                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$07                    ;2         *
    jmp     Lffeb                   ;3   =  46 *
    
    lda #$74
    sta $d6
    lda #$28
    sta $d7
    jmp     L3f80
    
L3f5f
    lda     ram_E5                  ;3        
    cmp     #$64                    ;2        
    bne     L3f80                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$3f                    ;2        
    pha                             ;3        
    lda     #$7c                    ;2        
    pha                             ;3        
    lda     #$d7                    ;2        
    pha                             ;3        
    lda     #$e7                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  46
    
    jmp     L3f80                   ;3   =   3
    
L3f80
    sta     ram_EE                  ;3        
    lda     #$3f                    ;2        
    pha                             ;3        
    lda     #$97                    ;2        
    pha                             ;3        
    lda     #$f4                    ;2        
    pha                             ;3        
    lda     #$35                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  39
    
    sta     ram_EE                  ;3        
    lda     #$3f                    ;2        
    pha                             ;3        
    lda     #$af                    ;2        
    pha                             ;3        
    lda     #$98                    ;2        
    pha                             ;3        
    lda     #$54                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$05                    ;2        
    jmp     Lffeb                   ;3   =  39
    
    jmp     Lffdd                   ;3   =   3
    

  IF PLUSROM = 1
    ORG     $1fd0, $ff
    RORG    $3fd0
  ELSE
    ORG     $1fd4, $ff
    RORG    $3fd4
  ENDIF

Start_b1
    ldx #$ff
    txs
    lda #$f2
    pha
    lda #$4f
    pha
    
L3fdd
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    tsx                             ;2        
    lda     CXM0FB,x                ;4        
    rol                             ;2        
    rol                             ;2        
    rol                             ;2        
    rol                             ;2        
    and     #$07                    ;2        
    tax                             ;2        
    inx                             ;2   =  28
L3feb
    lda     $1ff3,x                 ;4        
    pla                             ;4        
    tax                             ;2        
    pla                             ;4        
    rts                             ;6   =  20

    ORG     $1ff3, $ff
    RORG    $3ff3

L3ff3
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff     ; $3ff3 (*)

  IF PLUSROM = 1
    .word (PlusROM_API - $6000)             ; PlusRom API pointer
  ELSE
    .byte   $ff,$ff                         ; $3ffa (D)
  ENDIF

    .word Start_b1, Start_b1


;***********************************************************
;      Bank 2 / 0..7
;***********************************************************

    SEG     CODE
    ORG     $2000
    RORG    $5000

    lda     #$15                    ;2         *
    sta     CTRLPF                  ;3         *
    bit     CXP0FB                  ;3         *
    bvc     L501a                   ;2/3       *
    lda     ram_E0                  ;3         *
    ora     #$01                    ;2         *
    sta     ram_E0                  ;3         *
    lda     ram_E0                  ;3         *
    and     #$f7                    ;2         *
    sta     ram_E0                  ;3         *
    lda     ram_E0                  ;3         *
    and     #$fd                    ;2         *
    sta     ram_E0                  ;3   =  34 *
L501a
    bit     CXP0FB                  ;3         *
    bvs     L5027                   ;2/3       *
    lda     ram_E0                  ;3         *
    and     #$fe                    ;2         *
    sta     ram_E0                  ;3         *
    jmp     L505f                   ;3   =  16 *
    
L5027
    sta     ram_EE                  ;3         *
    lda     #$50                    ;2         *
    pha                             ;3         *
    lda     #$3e                    ;2         *
    pha                             ;3         *
    lda     #$da                    ;2         *
    pha                             ;3         *
    lda     #$0b                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$07                    ;2         *
    jmp     Lffeb                   ;3   =  39 *
    
    bit $02
    bpl l504d
    lda $d5
    clc
    adc #$02
    sta $d5
    jmp L505f
l504d
    lda #$10
    bit $0280
    bne l5056
    dec $d5
l5056
    lda #$20
    bit $0280
    bne L505f
    inc $d5
    
L505f
    jmp     Lffdd                   ;3   =   3 *
    
    lda     ram_E3                  ;3        
    ora     #$10                    ;2        
    sta     ram_E3                  ;3        
    lda     #BEIGE|$e               ;2        
    sta     COLUP1                  ;3        
    lda     ram_E5                  ;3        
    cmp     #$01                    ;2        
    beq     L5075                   ;2/3      
    jmp     L50b5                   ;3   =  23
    
L5075
    lda     #$30                    ;2        
    sta     NUSIZ1                  ;3        
    lda     #$08                    ;2        
    sta     ram_87                  ;3        
    lda     ram_DA                  ;3        
    cmp     #$5a                    ;2        
    bne     L508b                   ;2/3      
    lda     ram_DB                  ;3        
    cmp     #$52                    ;2        
    bcs     L508b                   ;2/3      
    inc     ram_DB                  ;5   =  29
L508b
    lda     ram_DB                  ;3        
    cmp     #$52                    ;2        
    bne     L5099                   ;2/3      
    lda     #$40                    ;2        
    cmp     ram_DA                  ;3        
    bcs     L5099                   ;2/3      
    dec     ram_DA                  ;5   =  19
L5099
    lda     ram_DA                  ;3        
    cmp     #$40                    ;2        
    bne     L50a7                   ;2/3      
    lda     #$3e                    ;2        
    cmp     ram_DB                  ;3        
    bcs     L50a7                   ;2/3      
    dec     ram_DB                  ;5   =  19
L50a7
    lda     ram_DB                  ;3        
    cmp     #$3e                    ;2        
    bne     L50b5                   ;2/3      
    lda     ram_DA                  ;3        
    cmp     #$5a                    ;2        
    bcs     L50b5                   ;2/3      
    inc     ram_DA                  ;5   =  19
L50b5
    jmp     Lffdd                   ;3   =   3
    
    lda     ram_E8                  ;3        
    ora     #$10                    ;2        
    sta     ram_E8                  ;3        
    sta     ram_EE                  ;3        
    lda     #$50                    ;2        
    pha                             ;3        
    lda     #$d5                    ;2        
    pha                             ;3        
    lda     #$da                    ;2        
    pha                             ;3        
    lda     #$e7                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  47
    
    lda     #$22                    ;2        
    sta     NUSIZ1                  ;3        
    lda     #$04                    ;2        
    sta     ram_87                  ;3        
    lda     #$02                    ;2        
    sta     ram_92                  ;3        
    lda     #RED                    ;2        
    sta     COLUP1                  ;3        
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcs     L50ff                   ;2/3      
    bit     CXM1FB                  ;3        
    bpl     L50ff                   ;2/3      
    lda     ram_DA                  ;3        
    sec                             ;2        
    sbc     #$02                    ;2        
    sta     ram_DA                  ;3        
    lda     ram_E8                  ;3        
    ora     #$01                    ;2        
    sta     ram_E8                  ;3        
    jmp     L5118                   ;3   =  53
    
L50ff
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcc     L5118                   ;2/3      
    bit     CXM1FB                  ;3        
    bpl     L5118                   ;2/3      
    lda     ram_DA                  ;3        
    clc                             ;2        
    adc     #$02                    ;2        
    sta     ram_DA                  ;3        
    lda     ram_E8                  ;3        
    and     #$fe                    ;2        
    sta     ram_E8                  ;3        
    jmp     L5118                   ;3   =  33
    
L5118
    lda     #$0a                    ;2        
    cmp     ram_DC                  ;3        
    bcs     L5121                   ;2/3      
    jmp     L512f                   ;3   =  10
    
L5121
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcs     L5128                   ;2/3      
    inc     ram_DA                  ;5   =  12
L5128
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcc     L512f                   ;2/3      
    dec     ram_DA                  ;5   =  12
L512f
    lda     #$1e                    ;2        
    sta     ram_84                  ;3        
    dec     ram_89                  ;5        
    lda     #$0a                    ;2        
    cmp     ram_89                  ;3        
    bcc     L5143                   ;2/3      
    lda     #$64                    ;2        
    sta     ram_89                  ;3        
    lda     #$1e                    ;2        
    sta     ram_84                  ;3   =  27
L5143
    lda     ram_84                  ;3        
    sec                             ;2        
    sbc     #$06                    ;2        
    pha                             ;3        
    tsx                             ;2        
    pla                             ;4        
    lda     ram_D4                  ;3        
    cmp     CXM1P,x                 ;4        
    bcs     L515a                   ;2/3      
    lda     ram_E0                  ;3         *
    and     #$fe                    ;2         *
    sta     ram_E0                  ;3         *
    jmp     L51a2                   ;3   =  36 *
    
L515a
    lda     ram_84                  ;3        
    clc                             ;2        
    adc     #$04                    ;2        
    cmp     ram_D4                  ;3        
    bcs     L516c                   ;2/3      
    lda     ram_E0                  ;3        
    and     #$fe                    ;2        
    sta     ram_E0                  ;3        
    jmp     L51a2                   ;3   =  23
    
L516c
    bit     CXP0FB                  ;3        
    bvs     L5179                   ;2/3      
    lda     ram_E0                  ;3        
    and     #$fe                    ;2        
    sta     ram_E0                  ;3        
    jmp     L51a2                   ;3   =  16
    
L5179
    bit     CXP0FB                  ;3        
    bvc     L5191                   ;2/3      
    lda     ram_89                  ;3        
    sec                             ;2        
    sbc     ram_92                  ;3        
    pha                             ;3        
    tsx                             ;2        
    pla                             ;4        
    lda     ram_D5                  ;3        
    cmp     CXM1P,x                 ;4        
    bcc     L5191                   ;2/3      
    lda     ram_E0                  ;3        
    ora     #$01                    ;2        
    sta     ram_E0                  ;3   =  39
L5191
    bit     CXP0FB                  ;3        
    bvc     L51a2                   ;2/3      
    lda     ram_89                  ;3        
    cmp     ram_D5                  ;3        
    bcs     L51a2                   ;2/3      
    lda     ram_89                  ;3        
    sec                             ;2        
    sbc     #$02                    ;2        
    sta     ram_D5                  ;3   =  23
L51a2
    jmp     Lffdd                   ;3   =   3
    
    lda     #$fa                    ;2        
    sta     ram_89                  ;3        
    lda     ram_E8                  ;3        
    ora     #$10                    ;2        
    sta     ram_E8                  ;3        
    sta     ram_EE                  ;3        
    lda     #$51                    ;2        
    pha                             ;3        
    lda     #$c6                    ;2        
    pha                             ;3        
    lda     #$da                    ;2        
    pha                             ;3        
    lda     #$6e                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  52
    
    lda     #RED                    ;2        
    sta     COLUP1                  ;3        
    lda     #$22                    ;2        
    sta     NUSIZ1                  ;3        
    lda     ram_D6                  ;3        
    cmp     #$3e                    ;2        
    bne     L51dd                   ;2/3      
    lda     ram_D7                  ;3        
    cmp     #$30                    ;2        
    bcs     L51dd                   ;2/3      
    inc     ram_D7                  ;5   =  29
L51dd
    lda     ram_D7                  ;3        
    cmp     #$30                    ;2        
    bne     L51eb                   ;2/3      
    lda     #$36                    ;2        
    cmp     ram_D6                  ;3        
    bcs     L51eb                   ;2/3      
    dec     ram_D6                  ;5   =  19
L51eb
    lda     ram_D6                  ;3        
    cmp     #$36                    ;2        
    bne     L51f9                   ;2/3      
    lda     #$14                    ;2        
    cmp     ram_D7                  ;3        
    bcs     L51f9                   ;2/3      
    dec     ram_D7                  ;5   =  19
L51f9
    lda     ram_D7                  ;3        
    cmp     #$14                    ;2        
    bne     L5207                   ;2/3!     
    lda     ram_D6                  ;3        
    cmp     #$3e                    ;2        
    bcs     L5207                   ;2/3      
    inc     ram_D6                  ;5   =  19
L5207
    jmp     Lffdd                   ;3   =   3
    
    lda     ram_E8                  ;3        
    ora     #$10                    ;2        
    sta     ram_E8                  ;3        
    lda     #$fa                    ;2        
    sta     ram_89                  ;3        
    lda     ram_D7                  ;3        
    clc                             ;2        
    adc     #$02                    ;2        
    sta     ram_D7                  ;3        
    lda     ram_DC                  ;3        
    cmp     #$14                    ;2        
    bcs     L522d                   ;2/3      
    lda     #$01                    ;2        
    sta     AUDV1                   ;3        
    lda     #$08                    ;2        
    sta     AUDC1                   ;3        
    lda     #$11                    ;2        
    sta     AUDF1                   ;3   =  45
L522d
    lda     #$14                    ;2        
    cmp     ram_DC                  ;3        
    bcs     L523d                   ;2/3      
    lda     #$01                    ;2        
    sta     AUDV1                   ;3        
    lda     #$08                    ;2        
    sta     AUDC1                   ;3        
    sta     AUDF1                   ;3   =  20
L523d
    lda     #$64                    ;2        
    cmp     ram_D7                  ;3        
    bcs     L5247                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_D7                  ;3   =  12
L5247
    sta     ram_EE                  ;3        
    lda     #$52                    ;2        
    pha                             ;3        
    lda     #$5e                    ;2        
    pha                             ;3        
    lda     #$da                    ;2        
    pha                             ;3        
    lda     #$95                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  39
    
    lda     #GREEN_BEIGE            ;2        
    sta     COLUP1                  ;3        
    lda     #$07                    ;2        
    sta     ram_87                  ;3        
    lda     #$20                    ;2        
    sta     NUSIZ1                  ;3        
    lda     ram_DB                  ;3        
    cmp     #$58                    ;2        
    bcc     L5279                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_DB                  ;3        
    lda     #$8a                    ;2        
    sta     ram_DA                  ;3   =  32
L5279
    bit     CXM1FB                  ;3        
    bmi     L5282                   ;2/3      
    inc     ram_DB                  ;5        
    jmp     L52b9                   ;3   =  13
    
L5282
    lda     #$30                    ;2        
    sta     NUSIZ1                  ;3        
    lda     ram_DA                  ;3        
    cmp     #$8a                    ;2        
    bcc     L5292                   ;2/3      
    lda     ram_E8                  ;3        
    and     #$fe                    ;2        
    sta     ram_E8                  ;3   =  20
L5292
    lda     ram_DA                  ;3        
    cmp     #$3c                    ;2        
    bcs     L529e                   ;2/3      
    lda     ram_E8                  ;3        
    ora     #$01                    ;2        
    sta     ram_E8                  ;3   =  15
L529e
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcc     L52ad                   ;2/3      
    lda     ram_DA                  ;3        
    clc                             ;2        
    adc     #$02                    ;2        
    sta     ram_DA                  ;3        
    jmp     L52b9                   ;3   =  20
    
L52ad
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcs     L52b9                   ;2/3      
    lda     ram_DA                  ;3        
    sec                             ;2        
    sbc     #$02                    ;2        
    sta     ram_DA                  ;3   =  17
L52b9
    lda     #$40                    ;2        
    cmp     ram_DB                  ;3        
    bcs     L52c3                   ;2/3      
    lda     #$42                    ;2        
    sta     ram_DA                  ;3   =  12
L52c3
    jmp     Lffdd                   ;3   =   3
    
    lda     #$fa                    ;2        
    sta     ram_89                  ;3        
    lda     ram_E8                  ;3        
    ora     #$10                    ;2        
    sta     ram_E8                  ;3        
    sta     ram_EE                  ;3        
    lda     #$52                    ;2        
    pha                             ;3        
    lda     #$e7                    ;2        
    pha                             ;3        
    lda     #$dc                    ;2        
    pha                             ;3        
    lda     #$67                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  52
    
    lda     #BLACK                  ;2        
    sta     COLUP1                  ;3        
    lda     #$20                    ;2        
    sta     NUSIZ1                  ;3        
    lda     #$3c                    ;2        
    sta     ram_87                  ;3        
    lda     ram_D6                  ;3        
    cmp     #$5a                    ;2        
    bne     L5302                   ;2/3!     
    lda     ram_D7                  ;3        
    cmp     #$52                    ;2        
    bcs     L5302                   ;2/3      
    inc     ram_D7                  ;5   =  34
L5302
    lda     ram_D7                  ;3        
    cmp     #$52                    ;2        
    bne     L5310                   ;2/3      
    lda     #$40                    ;2        
    cmp     ram_D6                  ;3        
    bcs     L5310                   ;2/3      
    dec     ram_D6                  ;5   =  19
L5310
    lda     ram_D6                  ;3        
    cmp     #$40                    ;2        
    bne     L531e                   ;2/3      
    lda     #$3e                    ;2        
    cmp     ram_D7                  ;3        
    bcs     L531e                   ;2/3      
    dec     ram_D7                  ;5   =  19
L531e
    lda     ram_D7                  ;3        
    cmp     #$3e                    ;2        
    bne     L532c                   ;2/3      
    lda     ram_D6                  ;3        
    cmp     #$5a                    ;2        
    bcs     L532c                   ;2/3      
    inc     ram_D6                  ;5   =  19
L532c
    lda     #$4e                    ;2        
    sta     ram_DA                  ;3        
    lda     #$08                    ;2        
    cmp     ram_DB                  ;3        
    bcc     L533c                   ;2/3      
    lda     ram_E8                  ;3        
    and     #$fe                    ;2        
    sta     ram_E8                  ;3   =  20
L533c
    lda     ram_DB                  ;3        
    cmp     #$40                    ;2        
    bcc     L5354                   ;2/3      
    lda     ram_E8                  ;3        
    ora     #$01                    ;2        
    sta     ram_E8                  ;3        
    lda     #$08                    ;2        
    sta     AUDV1                   ;3        
    lda     #$0a                    ;2        
    sta     AUDC1                   ;3        
    lda     #$11                    ;2        
    sta     AUDF1                   ;3   =  30
L5354
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcs     L5360                   ;2/3      
    lda     ram_DB                  ;3        
    clc                             ;2        
    adc     #$03                    ;2        
    sta     ram_DB                  ;3   =  17
L5360
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcc     L5367                   ;2/3      
    dec     ram_DB                  ;5   =  12
L5367
    jmp     Lffdd                   ;3   =   3
    
    lda     #$44                    ;2        
    sta     ram_92                  ;3        
    lda     #$44                    ;2        
    sta     ram_89                  ;3        
    lda     #$2d                    ;2        
    sta     ram_84                  ;3        
    lda     ram_E3                  ;3        
    ora     #$80                    ;2        
    sta     ram_E3                  ;3        
    lda     #$15                    ;2        
    sta     CTRLPF                  ;3        
    lda     ram_D4                  ;3        
    cmp     #$28                    ;2        
    bcs     L538f                   ;2/3      
    lda     ram_E0                  ;3        
    and     #$fe                    ;2        
    sta     ram_E0                  ;3        
    jmp     L53f0                   ;3   =  46
    
L538f
    bit     CXP0FB                  ;3        
    bvc     L53a5                   ;2/3      
    lda     ram_E0                  ;3        
    ora     #$01                    ;2        
    sta     ram_E0                  ;3        
    lda     ram_E0                  ;3        
    and     #$f7                    ;2        
    sta     ram_E0                  ;3        
    lda     ram_E0                  ;3        
    and     #$fd                    ;2        
    sta     ram_E0                  ;3   =  29
L53a5
    bit     CXP0FB                  ;3        
    bvs     L53b2                   ;2/3      
    lda     ram_E0                  ;3        
    and     #$fe                    ;2        
    sta     ram_E0                  ;3        
    jmp     L53f0                   ;3   =  16
    
L53b2
    sta     ram_EE                  ;3        
    lda     #$53                    ;2        
    pha                             ;3        
    lda     #$c9                    ;2        
    pha                             ;3        
    lda     #$da                    ;2        
    pha                             ;3        
    lda     #$0b                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  39
    
    lda     ram_E0                  ;3        
    ora     #$08                    ;2        
    sta     ram_E0                  ;3        
    bit     CXP0FB                  ;3        
    bpl     L53de                   ;2/3      
    lda     ram_D5                  ;3        
    clc                             ;2        
    adc     #$02                    ;2        
    sta     ram_D5                  ;3        
    jmp     L53f0                   ;3   =  26
    
L53de
    lda     #$10                    ;2        
    bit     SWCHA                   ;4        
    bne     L53e7                   ;2/3      
    dec     ram_D5                  ;5   =  13
L53e7
    lda     #$20                    ;2        
    bit     SWCHA                   ;4        
    bne     L53f0                   ;2/3      
    inc     ram_D5                  ;5   =  13 *
L53f0
    lda     #$00                    ;2        
    cmp     ram_E7                  ;3        
    bcc     L53ff                   ;2/3      
    lda     ram_E8                  ;3        
    ora     #$02                    ;2        
    sta     ram_E8                  ;3        
    jmp     L5491                   ;3   =  18
    
L53ff
    sta     ram_EE                  ;3        
    lda     #$54                    ;2        
    pha                             ;3        
    lda     #$16                    ;2        
    pha                             ;3        
    lda     #$db                    ;2        
    pha                             ;3        
    lda     #$bc                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  39
    
    lda     #CYAN                   ;2        
    sta     COLUP1                  ;3        
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcc     L5424                   ;2/3      
    lda     #$08                    ;2        
    sta     REFP1                   ;3   =  17
L5424
    lda     #$01                    ;2        
    cmp     ram_E7                  ;3        
    bcs     L542d                   ;2/3      
    jmp     L5456                   ;3   =  10
    
L542d
    lda     #$50                    ;2        
    cmp     ram_D7                  ;3        
    bcs     L543b                   ;2/3      
    lda     #$85                    ;2        
    sta     ram_D6                  ;3        
    lda     #$50                    ;2        
    sta     ram_D7                  ;3   =  17
L543b
    lda     ram_D6                  ;3        
    cmp     #$85                    ;2        
    bcc     L5447                   ;2/3      
    lda     ram_E8                  ;3        
    ora     #$01                    ;2        
    sta     ram_E8                  ;3   =  15
L5447
    lda     ram_D6                  ;3        
    cmp     #$5a                    ;2        
    bcs     L5453                   ;2/3      
    lda     ram_E8                  ;3        
    and     #$fe                    ;2        
    sta     ram_E8                  ;3   =  15
L5453
    jmp     L5477                   ;3   =   3
    
L5456
    lda     ram_E7                  ;3        
    cmp     #$02                    ;2        
    bcs     L545f                   ;2/3      
    jmp     L5491                   ;3   =  10 *
    
L545f
    lda     #$36                    ;2        
    cmp     ram_D6                  ;3        
    bcc     L546b                   ;2/3      
    lda     ram_E8                  ;3        
    and     #$fe                    ;2        
    sta     ram_E8                  ;3   =  15
L546b
    lda     ram_D6                  ;3        
    cmp     #$64                    ;2        
    bcc     L5477                   ;2/3      
    lda     ram_E8                  ;3        
    ora     #$01                    ;2        
    sta     ram_E8                  ;3   =  15
L5477
    lda     ram_DC                  ;3        
    cmp     #$0c                    ;2        
    bcs     L5480                   ;2/3      
    jmp     L5491                   ;3   =  10
    
L5480
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcs     L548a                   ;2/3      
    inc     ram_D6                  ;5        
    jmp     L5491                   ;3   =  15
    
L548a
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcc     L5491                   ;2/3      
    dec     ram_D6                  ;5   =  12
L5491
    lda     ram_E8                  ;3        
    and     #$02                    ;2        
    beq     L54bb                   ;2/3      
    ldx     #$01                    ;2        
    ldy     #$09                    ;2        
    lda     #$1f                    ;2        
    sta     ram_EE                  ;3        
    lda     #$54                    ;2        
    pha                             ;3        
    lda     #$b4                    ;2        
    pha                             ;3        
    lda     #$f2                    ;2        
    pha                             ;3        
    lda     #$b8                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  52
    
    lda     ram_E8                  ;3        
    and     #$fd                    ;2        
    sta     ram_E8                  ;3   =   8
L54bb
    jmp     Lffdd                   ;3   =   3
    
    lda     #CYAN                   ;2        
    sta     COLUP1                  ;3        
    lda     #$02                    ;2        
    sta     NUSIZ1                  ;3        
    lda     ram_E8                  ;3        
    ora     #$10                    ;2        
    sta     ram_E8                  ;3        
    lda     ram_E8                  ;3        
    ora     #$08                    ;2        
    sta     ram_E8                  ;3        
    lda     #$82                    ;2        
    sta     ram_84                  ;3        
    dec     ram_89                  ;5        
    lda     #$08                    ;2        
    cmp     ram_89                  ;3        
    bcc     L54e2                   ;2/3      
    lda     #$64                    ;2        
    sta     ram_89                  ;3   =  48
L54e2
    lda     #$3d                    ;2        
    sta     ram_D6                  ;3        
    lda     #$12                    ;2        
    cmp     ram_D7                  ;3        
    bcc     L54f2                   ;2/3      
    lda     ram_E8                  ;3        
    and     #$fe                    ;2        
    sta     ram_E8                  ;3   =  20
L54f2
    lda     ram_D7                  ;3        
    cmp     #$30                    ;2        
    bcc     L550a                   ;2/3!     
    lda     ram_E8                  ;3        
    ora     #$01                    ;2        
    sta     ram_E8                  ;3        
    lda     #$08                    ;2        
    sta     AUDV1                   ;3        
    lda     #$0a                    ;2        
    sta     AUDC1                   ;3        
    lda     #$11                    ;2        
    sta     AUDF1                   ;3   =  30
L550a
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcs     L5516                   ;2/3      
    lda     ram_D7                  ;3        
    clc                             ;2        
    adc     #$02                    ;2        
    sta     ram_D7                  ;3   =  17
L5516
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcc     L551d                   ;2/3      
    dec     ram_D7                  ;5   =  12
L551d
    jmp     Lffdd                   ;3   =   3
    
    lda     #$fa                    ;2        
    sta     ram_89                  ;3        
    lda     ram_E8                  ;3        
    ora     #$10                    ;2        
    sta     ram_E8                  ;3        
    lda     ram_E5                  ;3        
    cmp     #$09                    ;2        
    bne     L5534                   ;2/3      
    lda     #GREEN_BEIGE            ;2        
    sta     COLUP1                  ;3   =  25
L5534
    lda     ram_E5                  ;3        
    cmp     #$0f                    ;2        
    bne     L553d                   ;2/3      
    jmp     L5543                   ;3   =  10
    
L553d
    lda     ram_E5                  ;3        
    cmp     #$15                    ;2        
    bne     L555f                   ;2/3 =   7
L5543
    sta     ram_EE                  ;3        
    lda     #$55                    ;2        
    pha                             ;3        
    lda     #$5a                    ;2        
    pha                             ;3        
    lda     #$dc                    ;2        
    pha                             ;3        
    lda     #$67                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  39
    
    lda     #CYAN                   ;2        
    sta     COLUP1                  ;3   =   5
L555f
    lda     #$2b                    ;2        
    sta     ram_D6                  ;3        
    lda     #$06                    ;2        
    sta     NUSIZ1                  ;3        
    lda     ram_D7                  ;3        
    cmp     #$3c                    ;2        
    bcc     L5573                   ;2/3      
    lda     ram_E8                  ;3        
    and     #$fe                    ;2        
    sta     ram_E8                  ;3   =  25
L5573
    lda     #$14                    ;2        
    cmp     ram_D7                  ;3        
    bcc     L557f                   ;2/3      
    lda     ram_E8                  ;3        
    ora     #$01                    ;2        
    sta     ram_E8                  ;3   =  15
L557f
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcs     L5593                   ;2/3      
    dec     ram_D7                  ;5        
    lda     #$01                    ;2        
    sta     AUDV1                   ;3        
    lda     ram_D7                  ;3        
    sta     AUDC1                   ;3        
    sta     AUDF1                   ;3        
    jmp     L55a7                   ;3   =  29
    
L5593
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcc     L55a7                   ;2/3      
    inc     ram_D7                  ;5        
    lda     #$01                    ;2        
    sta     AUDV1                   ;3        
    lda     ram_D7                  ;3        
    sta     AUDC1                   ;3        
    sta     AUDF1                   ;3        
    jmp     L55a7                   ;3   =  29
    
L55a7
    jmp     Lffdd                   ;3   =   3
    
    lda $e8
    and #$02
    bne l55b3
    jmp l561d
l55b3
    lda #$fa
    sta $89
    lda #$24
    sta $da
    lda #$10
    sta $db
    lda #$0a
    sta ram_87
    lda #$30
    sta $05
    lda #$fa
    sta $d7
    lda #$4b
    cmp $e2
    bcc l55d5
    lda #GREEN|$4
    sta COLUP1
l55d5
    lda #$4b
    cmp $e2
    bcs l55df
    lda #YELLOW|$4
    sta COLUP1
l55df
    lda #$96
    cmp $e2
    bcs l55e9
    lda #ORANGE
    sta COLUP1
l55e9
    lda #$c8
    cmp $e2
    bcs l55f3
    lda #RED|$2
    sta COLUP1
l55f3
    lda ram_DC
    cmp #$00
    bne l55fc
    jmp l5602
l55fc
    lda ram_DC
    cmp #$14
    bne l5611
l5602
    lda #$07
    sta $1a
    lda #$0c
    sta $16
    lda #$11
    clc
    adc ram_DC
    sta $18
l5611
    lda #$f0
    cmp $e2
    bcs l561d
    lda $e8
    and #$fd
    sta $e8
l561d
    lda $e8
    and #$02
    beq l5628
    inc $e2
    jmp l56e9
l5628
    lda #RED|$2
    sta COLUP1
    lda #$2c
    sta $da
    sta $db
    lda #$36
    sta $05
    lda #$34
    sta $84
    lda #$10
    sta $89
    sta $ee
    lda #$56
    pha
    lda #$55
    pha
    lda #$f5
    pha
    lda #$d4
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    lda #$38
    sta $d7
    lda $d6
    clc
    adc #$08
    sta $d6
    lda #$14
    cmp ram_DC
    bcs l566b
    lda #$08
    sta $0c
l566b
    lda $d6
    cmp #$48
    bcc l5675
    lda #$15
    sta $d6
l5675
    lda #$03
    sta $1a
    sta $16
    lda #$08
    sta $18
    lda ram_87
    cmp #$0c
    bcc l568b
    lda $e8
    and #$fe
    sta $e8
l568b
    lda #$00
    cmp ram_87
    bcc l5697
    lda $e8
    ora #$01
    sta $e8
l5697
    lda $e8
    lsr
    bcs l569f
    jmp l56b0
l569f
    lda ram_DC
    cmp #$00
    bne l56a8
    jmp l56ae
l56a8
    lda ram_DC
    cmp #$0a
    bne l56b0
l56ae
    inc ram_87
l56b0
    lda $e8
    lsr
    bcc l56b8
    jmp l56c9
l56b8
    lda ram_DC
    cmp #$00
    bne l56c1
    jmp l56c7
l56c1
    lda ram_DC
    cmp #$0a
    bne l56c9
l56c7
    dec ram_87
l56c9
    lda $e0
    and #$02
    beq l56e9
    bit $02
    bvc l56e9
    lda $e8
    ora #$02
    sta $e8
    lda #$00
    sta $e2
    lda #$07
    sta $1a
    lda #$09
    sta $16
    lda #$08
    sta $18
l56e9
    jmp Lffdd
    lda #$80
    sta $84
    lda #$0e
    sta $89
    lda #$0e
    sta $92
    lda $e3
    ora #$80
    sta $e3
    lda #$15
    sta $0a
    lda $d5
    cmp #$06
    bcs l571f
    lda $e3
    ora #$08
    sta $e3
    inc $e5
    sed
    clc
    lda $94
    adc #$01
    sta $94
    lda $93
    adc #$00
    sta $93
    cld
l571f
    lda $d4
    cmp #$75
    bcs l572e
    lda $e0
    and #$fe
    sta $e0
    jmp l578f
l572e
    bit $02
    bvc l5744
    lda $e0
    ora #$01
    sta $e0
    lda $e0
    and #$f7
    sta $e0
    lda $e0
    and #$fd
    sta $e0
l5744
    bit $02
    bvs l5751
    lda $e0
    and #$fe
    sta $e0
    jmp l578f
l5751
    sta $ee
    lda #$57
    pha
    lda #$68
    pha
    lda #$da
    pha
    lda #$0b
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$07
    jmp Lffeb
    lda $e0
    ora #$08
    sta $e0
    bit $02
    bpl l577d
    lda $d5
    clc
    adc #$02
    sta $d5
    jmp l578f
l577d
    lda #$10
    bit $0280
    bne l5786
    dec $d5
l5786
    lda #$20
    bit $0280
    bne l578f
    inc $d5
l578f
    inc $e2
    lda #$4b
    cmp $e2
    bcs l579b
    lda #$00
    sta $e2
l579b
    lda #YELLOW|$a
    sta COLUP1
    lda #$3f
    sta $d6
    lda #$32
    sta $05
    lda $e2
    cmp #$28
    bcs l57dd
    sta $ee
    lda #$57
    pha
    lda #$c4
    pha
    lda #$dd
    pha
    lda #$0e
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$07
    jmp Lffeb
    lda #$50
    sta $d7
    lda #$03
    sta $1a
    lda #YELLOW|$c
    sta COLUP1
    lda #$03
    sta $16
    lda #$08
    sta $18
    lda #BLACK|$4
    sta COLUBK
l57dd
    lda $e2
    cmp #$28
    bne l57e7
    lda #$fa
    sta $d7
l57e7
    lda #$1f
    sta $db
    inc $da
    lda ram_DC
    cmp #$05
    bcs l57f5
    inc $da
l57f5
    lda $da
    cmp #$64
    bcc l57ff
    lda #$16
    sta $da
l57ff
    lda #$06
    sta ram_87
    jmp     Lffdd                   ;3

L5806
    lda     #$fa                    ;2        
    sta     ram_89                  ;3        
    lda     ram_DC                  ;3        
    cmp     #$00                    ;2        
    bne     L5812                   ;2/3      
    inc     ram_E2                  ;5   =  17
L5812
    lda     ram_E2                  ;3        
    cmp     #$02                    ;2        
    bcc     L581c                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_E2                  ;3   =  12
L581c
    lda     ram_E2                  ;3        
    cmp     #$00                    ;2        
    bne     L5826                   ;2/3      
    lda     #$fa                    ;2        
    sta     ram_D7                  ;3   =  12
L5826
    lda     ram_E2                  ;3        
    cmp     #$01                    ;2        
    bne     L585a                   ;2/3      
    lda     #$40                    ;2        
    sta     ram_D7                  ;3        
    lda     #YELLOW|$c              ;2        
    sta     COLUP1                  ;3        
    sta     ram_EE                  ;3        
    lda     #$58                    ;2        
    pha                             ;3        
    lda     #$4b                    ;2        
    pha                             ;3        
    lda     #$dd                    ;2        
    pha                             ;3        
    lda     #$0e                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  56
    
    lda     #$03                    ;2        
    sta     AUDV1                   ;3        
    sta     AUDC1                   ;3        
    lda     #$08                    ;2        
    sta     AUDF1                   ;3        
    lda     #BLACK|$4               ;2        
    sta     COLUBK                  ;3   =  18
L585a
    jmp     Lffdd                   ;3   =   3
    
    lda     #$fa                    ;2        
    sta     ram_89                  ;3        
    lda     ram_E8                  ;3        
    ora     #$10                    ;2        
    sta     ram_E8                  ;3        
    lda     ram_DC                  ;3        
    cmp     #$14                    ;2        
    bne     L586f                   ;2/3      
    inc     ram_E2                  ;5   =  25
L586f
    lda     ram_E2                  ;3        
    cmp     #$07                    ;2        
    bne     L5879                   ;2/3      
    lda     #$00                    ;2         *
    sta     ram_E2                  ;3   =  12 *
L5879
    lda     ram_E5                  ;3        
    cmp     #$27                    ;2        
    beq     L5888                   ;2/3      
    lda     ram_E5                  ;3        
    cmp     #$2b                    ;2        
    beq     L5888                   ;2/3      
    jmp     L596e                   ;3   =  17
    
L5888
    lda     #GREEN_YELLOW           ;2         *
    sta     COLUP1                  ;3         *
    lda     ram_E2                  ;3         *
    cmp     #$00                    ;2         *
    bne     L58cd                   ;2/3       *
    lda     #$20                    ;2         *
    sta     ram_D7                  ;3         *
    lda     #$2d                    ;2         *
    sta     ram_D6                  ;3         *
    lda     #$06                    ;2         *
    sta     NUSIZ1                  ;3         *
    sta     ram_EE                  ;3         *
    lda     #$58                    ;2         *
    pha                             ;3         *
    lda     #$b5                    ;2         *
    pha                             ;3         *
    lda     #$dd                    ;2         *
    pha                             ;3         *
    lda     #$0e                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$07                    ;2         *
    jmp     Lffeb                   ;3   =  66 *
    
    lda #$03
    sta $1a
    lda #YELLOW|$c
    sta COLUP1
    lda #$03
    sta $16
    lda #$08
    sta $18
    lda #BLACK|$4
    sta COLUBK
    jmp L596e
    
L58cd
    lda     ram_E2                  ;3         *
    cmp     #$01                    ;2         *
    bne     L58d6                   ;2/3       *
    jmp     L58dc                   ;3   =  10 *
    
L58d6
    lda     ram_E2                  ;3         *
    cmp     #$06                    ;2         *
    bne     L5917                   ;2/3!=   7 *
L58dc
    lda     #$20                    ;2         *
    sta     ram_D7                  ;3         *
    lda     #$4d                    ;2         *
    sta     ram_D6                  ;3         *
    lda     #$02                    ;2         *
    sta     NUSIZ1                  ;3         *
    sta     ram_EE                  ;3         *
    lda     #$58                    ;2         *
    pha                             ;3         *
    lda     #$ff                    ;2         *
    pha                             ;3         *
    lda     #$dd                    ;2         *
    pha                             ;3         *
    lda     #$0e                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$07                    ;2         *
    jmp     Lffeb                   ;3   =  54 *
    
    lda #$03
    sta $1a
    lda #YELLOW|$c
    sta COLUP1
    lda #$03
    sta $16
    lda #$08
    sta $18
    lda #BLACK|$4
    sta COLUBK
    jmp L596e

L5917
    lda     ram_E2                  ;3         *
    cmp     #$02                    ;2         *
    bne     L5920                   ;2/3       *
    jmp     L5926                   ;3   =  10 *
    
L5920
    lda     ram_E2                  ;3         *
    cmp     #$05                    ;2         *
    bne     L5961                   ;2/3 =   7 *
L5926
    lda     #$20                    ;2         *
    sta     ram_D7                  ;3         *
    lda     #$6d                    ;2         *
    sta     ram_D6                  ;3         *
    lda     #$00                    ;2         *
    sta     NUSIZ1                  ;3         *
    sta     ram_EE                  ;3         *
    lda     #$59                    ;2         *
    pha                             ;3         *
    lda     #$49                    ;2         *
    pha                             ;3         *
    lda     #$dd                    ;2         *
    pha                             ;3         *
    lda     #$0e                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$07                    ;2         *
    jmp     Lffeb                   ;3   =  54 *
    
    lda #$03
    sta $1a
    lda #YELLOW|$c
    sta COLUP1
    lda #$03
    sta $16
    lda #$08
    sta $18
    lda #BLACK|$4
    sta COLUBK
    jmp L596e
    
L5961
    lda     ram_E2                  ;3         *
    cmp     #$03                    ;2         *
    bne     L596e                   ;2/3       *
    lda     #$fa                    ;2         *
    sta     ram_D7                  ;3         *
    jmp     L596e                   ;3   =  15 *
    
L596e
    lda     ram_E5                  ;3        
    cmp     #$11                    ;2        
    beq     L597d                   ;2/3      
    lda     ram_E5                  ;3        
    cmp     #$13                    ;2        
    beq     L597d                   ;2/3      
    jmp     L5a27                   ;3   =  17 *
    
L597d
    lda     #RED                    ;2        
    sta     COLUP1                  ;3        
    lda     ram_E2                  ;3        
    cmp     #$00                    ;2        
    bne     L59ae                   ;2/3      
    lda     #$20                    ;2        
    sta     ram_D7                  ;3        
    lda     #$2d                    ;2        
    sta     ram_D6                  ;3        
    lda     #$06                    ;2        
    sta     NUSIZ1                  ;3        
    sta     ram_EE                  ;3        
    lda     #$59                    ;2        
    pha                             ;3        
    lda     #$aa                    ;2        
    pha                             ;3        
    lda     #$da                    ;2        
    pha                             ;3        
    lda     #$e7                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  66
    
    jmp     L5a27                   ;3   =   3
    
L59ae
    lda     ram_E2                  ;3        
    cmp     #$01                    ;2        
    bne     L59b7                   ;2/3      
    jmp     L59bd                   ;3   =  10
    
L59b7
    lda     ram_E2                  ;3        
    cmp     #$06                    ;2        
    bne     L59e4                   ;2/3 =   7
L59bd
    lda     #$20                    ;2        
    sta     ram_D7                  ;3        
    lda     #$4d                    ;2        
    sta     ram_D6                  ;3        
    lda     #$02                    ;2        
    sta     NUSIZ1                  ;3        
    sta     ram_EE                  ;3        
    lda     #$59                    ;2        
    pha                             ;3        
    lda     #$e0                    ;2        
    pha                             ;3        
    lda     #$da                    ;2        
    pha                             ;3        
    lda     #$e7                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  54
    
    jmp     L5a27                   ;3   =   3
    
L59e4
    lda     ram_E2                  ;3        
    cmp     #$02                    ;2        
    bne     L59ed                   ;2/3      
    jmp     L59f3                   ;3   =  10
    
L59ed
    lda     ram_E2                  ;3        
    cmp     #$05                    ;2        
    bne     L5a1a                   ;2/3!=   7
L59f3
    lda     #$20                    ;2        
    sta     ram_D7                  ;3        
    lda     #$6d                    ;2        
    sta     ram_D6                  ;3        
    lda     #$00                    ;2        
    sta     NUSIZ1                  ;3        
    sta     ram_EE                  ;3        
    lda     #$5a                    ;2        
    pha                             ;3        
    lda     #$16                    ;2        
    pha                             ;3        
    lda     #$da                    ;2        
    pha                             ;3        
    lda     #$e7                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  54
    
    jmp     L5a27                   ;3   =   3
    
L5a1a
    lda     ram_E2                  ;3        
    cmp     #$03                    ;2        
    bne     L5a27                   ;2/3      
    lda     #$fa                    ;2        
    sta     ram_D7                  ;3        
    jmp     L5a27                   ;3   =  15
    
L5a27
    jmp     Lffdd                   ;3   =   3
    
    lda     #$02                    ;2        
    sta     AUDV1                   ;3        
    lda     #$0c                    ;2        
    sta     AUDC1                   ;3        
    inc     ram_D6                  ;5        
    lda     ram_DC                  ;3        
    cmp     #$1e                    ;2        
    bcs     L5a3c                   ;2/3      
    inc     ram_D6                  ;5   =  27
L5a3c
    sta     ram_EE                  ;3        
    lda     #$5a                    ;2        
    pha                             ;3        
    lda     #$53                    ;2        
    pha                             ;3        
    lda     #$dc                    ;2        
    pha                             ;3        
    lda     #$b3                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  39
    
    lda     #$96                    ;2        
    cmp     ram_D6                  ;3        
    bcs     L5a5e                   ;2/3      
    lda     #$14                    ;2        
    sta     ram_D6                  ;3   =  12
L5a5e
    lda     ram_DC                  ;3        
    cmp     #$00                    ;2        
    bcc     L5a68                   ;2/3      
    lda     #BLUE                   ;2        
    sta     COLUP1                  ;3   =  12
L5a68
    lda     ram_DC                  ;3        
    cmp     #$0a                    ;2        
    bcc     L5a72                   ;2/3      
    lda     #BLUE|$2                ;2        
    sta     COLUP1                  ;3   =  12
L5a72
    lda     ram_DC                  ;3        
    cmp     #$14                    ;2        
    bcc     L5a7c                   ;2/3      
    lda     #BLUE|$4                ;2        
    sta     COLUP1                  ;3   =  12
L5a7c
    lda     ram_DC                  ;3        
    cmp     #$1e                    ;2        
    bcc     L5a86                   ;2/3      
    lda     #BLUE|$8                ;2        
    sta     COLUP1                  ;3   =  12
L5a86
    lda     #$1f                    ;2        
    cmp     ram_DB                  ;3        
    bcc     L5aac                   ;2/3      
    lda     ram_DA                  ;3        
    cmp     #$62                    ;2        
    bcs     L5aac                   ;2/3      
    lda     #$1f                    ;2        
    sta     ram_DB                  ;3        
    lda     ram_DA                  ;3        
    clc                             ;2        
    adc     #$02                    ;2        
    sta     ram_DA                  ;3        
    lda     #$30                    ;2        
    sta     NUSIZ1                  ;3        
    lda     #$19                    ;2        
    sta     AUDF1                   ;3        
    lda     #$04                    ;2        
    sta     ram_87                  ;3        
    jmp     L5b14                   ;3   =  47
    
L5aac
    lda     ram_DA                  ;3        
    cmp     #$62                    ;2        
    bcc     L5acd                   ;2/3      
    lda     ram_DB                  ;3        
    cmp     #$4d                    ;2        
    bcs     L5acd                   ;2/3      
    lda     #$62                    ;2        
    sta     ram_DA                  ;3        
    inc     ram_DB                  ;5        
    lda     #$20                    ;2        
    sta     NUSIZ1                  ;3        
    lda     #$17                    ;2        
    sta     AUDF1                   ;3        
    lda     #$08                    ;2        
    sta     ram_87                  ;3        
    jmp     L5b14                   ;3   =  42
    
L5acd
    lda     ram_DB                  ;3        
    cmp     #$4d                    ;2        
    bcc     L5af3                   ;2/3      
    lda     #$3e                    ;2        
    cmp     ram_DA                  ;3        
    bcs     L5af3                   ;2/3      
    lda     #$4d                    ;2        
    sta     ram_DB                  ;3        
    lda     ram_DA                  ;3        
    sec                             ;2        
    sbc     #$02                    ;2        
    sta     ram_DA                  ;3        
    lda     #$30                    ;2        
    sta     NUSIZ1                  ;3        
    lda     #$16                    ;2        
    sta     AUDF1                   ;3        
    lda     #$04                    ;2        
    sta     ram_87                  ;3        
    jmp     L5b14                   ;3   =  47
    
L5af3
    lda     #$3e                    ;2        
    cmp     ram_DA                  ;3        
    bcc     L5b14                   ;2/3!     
    lda     #$1f                    ;2        
    cmp     ram_DB                  ;3        
    bcs     L5b14                   ;2/3!     
    lda     #$3e                    ;2        
    sta     ram_DA                  ;3        
    dec     ram_DB                  ;5        
    lda     #$20                    ;2        
    sta     NUSIZ1                  ;3        
    lda     #$11                    ;2        
    sta     AUDF1                   ;3        
    lda     #$08                    ;2        
    sta     ram_87                  ;3        
    jmp     L5b14                   ;3   =  42
    
L5b14
    lda     ram_D4                  ;3        
    cmp     ram_84                  ;3        
    bcs     L5b25                   ;2/3      
    lda     #$47                    ;2        
    cmp     ram_84                  ;3        
    bcs     L5b25                   ;2/3      
    dec     ram_84                  ;5        
    jmp     L5b58                   ;3   =  23
    
L5b25
    lda     ram_84                  ;3        
    cmp     ram_D4                  ;3        
    bcs     L5b36                   ;2/3      
    lda     ram_84                  ;3        
    cmp     #$59                    ;2        
    bcs     L5b36                   ;2/3      
    inc     ram_84                  ;5        
    jmp     L5b58                   ;3   =  23
    
L5b36
    lda     ram_89                  ;3        
    cmp     ram_D5                  ;3        
    bcs     L5b47                   ;2/3      
    lda     ram_89                  ;3        
    cmp     #$3e                    ;2        
    bcs     L5b47                   ;2/3      
    inc     ram_89                  ;5        
    jmp     L5b58                   ;3   =  23
    
L5b47
    lda     ram_D5                  ;3        
    cmp     ram_89                  ;3        
    bcs     L5b58                   ;2/3      
    lda     #$2c                    ;2        
    cmp     ram_89                  ;3        
    bcs     L5b58                   ;2/3      
    dec     ram_89                  ;5        
    jmp     L5b58                   ;3   =  23
    
L5b58
    jmp     Lffdd                   ;3   =   3
    
    lda     ram_E8                  ;3        
    and     #$04                    ;2        
    beq     L5b64                   ;2/3      
    jmp     L5c12                   ;3   =  10
    
L5b64
    bit     CXP0FB                  ;3        
    bvs     L5b6b                   ;2/3      
    jmp     L5b71                   ;3   =   8
    
L5b6b
    lda     ram_D4                  ;3        
    cmp     #$28                    ;2        
    bcs     L5b74                   ;2/3 =   7
L5b71
    jmp     L5b93                   ;3   =   3
    
L5b74
    lda     #$46                    ;2        
    cmp     ram_89                  ;3        
    bcs     L5b7c                   ;2/3      
    dec     ram_D4                  ;5   =  12
L5b7c
    lda     ram_EB                  ;3        
    lsr                             ;2        
    bcc     L5b93                   ;2/3      
    lda     ram_EB                  ;3        
    and     #$fe                    ;2        
    sta     ram_EB                  ;3        
    lda     ram_E8                  ;3        
    ora     #$02                    ;2        
    sta     ram_E8                  ;3        
    lda     ram_E8                  ;3        
    ora     #$01                    ;2        
    sta     ram_E8                  ;3   =  31
L5b93
    lda     ram_E8                  ;3        
    and     #$02                    ;2        
    beq     L5be2                   ;2/3      
    lda     #$46                    ;2        
    cmp     ram_89                  ;3        
    bcs     L5bad                   ;2/3      
    dec     ram_89                  ;5        
    lda     #$07                    ;2        
    sta     AUDV1                   ;3        
    lda     #$08                    ;2        
    sta     AUDF1                   ;3        
    lda     #$6e                    ;2        
    sta     AUDC1                   ;3   =  34
L5bad
    lda     #$46                    ;2        
    cmp     ram_89                  ;3        
    bcc     L5bdf                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_D7                  ;3        
    lda     #$fa                    ;2        
    sta     ram_89                  ;3        
    lda     ram_E8                  ;3        
    ora     #$04                    ;2        
    sta     ram_E8                  ;3        
    lda     ram_E8                  ;3        
    and     #$fd                    ;2        
    sta     ram_E8                  ;3        
    sta     ram_EE                  ;3        
    lda     #$5b                    ;2        
    pha                             ;3        
    lda     #$de                    ;2        
    pha                             ;3        
    lda     #$dc                    ;2        
    pha                             ;3        
    lda     #$58                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  72
    
L5bdf
    jmp     L5c12                   ;3   =   3
    
L5be2
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcc     L5bea                   ;2/3      
    jmp     L5c12                   ;3   =  10 *
    
L5bea
    lda     ram_E3                  ;3        
    ora     #$80                    ;2        
    sta     ram_E3                  ;3        
    lda     #$4f                    ;2        
    sta     ram_89                  ;3        
    lda     #$72                    ;2        
    sta     ram_84                  ;3        
    lda     #$08                    ;2        
    sta     ram_92                  ;3        
    lda     #$31                    ;2        
    sta     CTRLPF                  ;3        
    lda     #BLACK|$4               ;2        
    sta     COLUP1                  ;3        
    lda     #$6f                    ;2        
    sta     ram_D6                  ;3        
    lda     ram_89                  ;3        
    sta     ram_D7                  ;3        
    lda     ram_E3                  ;3        
    ora     #$01                    ;2        
    sta     ram_E3                  ;3   =  52
L5c12
    lda     ram_E8                  ;3        
    and     #$04                    ;2        
    bne     L5c1b                   ;2/3      
    jmp     L5c52                   ;3   =  10
    
L5c1b
    lda     #$74                    ;2        
    sta     ram_D6                  ;3        
    lda     ram_E3                  ;3        
    and     #$fe                    ;2        
    sta     ram_E3                  ;3        
    lda     ram_E3                  ;3        
    ora     #$10                    ;2        
    sta     ram_E3                  ;3        
    lda     #GREEN_BEIGE            ;2        
    sta     COLUP1                  ;3        
    lda     ram_D7                  ;3        
    cmp     #$0c                    ;2        
    bcs     L5c43                   ;2/3      
    inc     ram_D7                  ;5        
    lda     #$04                    ;2        
    sta     AUDV1                   ;3        
    lda     ram_D7                  ;3        
    sta     AUDC1                   ;3        
    lda     #$11                    ;2        
    sta     AUDF1                   ;3   =  54
L5c43
    lda     ram_D7                  ;3        
    clc                             ;2        
    adc     #$04                    ;2        
    cmp     ram_D5                  ;3        
    bcs     L5c52                   ;2/3      
    lda     ram_E0                  ;3        
    and     #$fe                    ;2        
    sta     ram_E0                  ;3   =  20
L5c52
    jmp     Lffdd                   ;3   =   3
    
    lda     #$fa                    ;2        
    sta     ram_89                  ;3        
    lda     #$00                    ;2        
    cmp     ram_E7                  ;3        
    bcc     L5c62                   ;2/3      
    jmp     L5d4f                   ;3   =  15
    
L5c62
    lda     #$05                    ;2        
    sta     NUSIZ1                  ;3        
    lda     #CYAN                   ;2        
    sta     COLUP1                  ;3        
    sta     ram_EE                  ;3        
    lda     #$5c                    ;2        
    pha                             ;3        
    lda     #$81                    ;2        
    pha                             ;3        
    lda     #$dd                    ;2        
    pha                             ;3        
    lda     #$47                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  49
    
    lda     ram_E8                  ;3        
    ora     #$40                    ;2        
    sta     ram_E8                  ;3        
    lda     #$14                    ;2        
    cmp     ram_DC                  ;3        
    bcs     L5ca2                   ;2/3      
    lda     ram_D7                  ;3        
    cmp     #$4e                    ;2        
    bcs     L5ca2                   ;2/3      
    inc     ram_D7                  ;5        
    lda     #$04                    ;2        
    sta     AUDV1                   ;3        
    lda     ram_D7                  ;3        
    sta     AUDC1                   ;3        
    lda     #$08                    ;2        
    sta     AUDF1                   ;3   =  43
L5ca2
    lda     ram_D7                  ;3        
    cmp     #$4e                    ;2        
    bcs     L5cab                   ;2/3      
    jmp     L5d4f                   ;3   =  10
    
L5cab
    lda     #$1c                    ;2        
    cmp     ram_D6                  ;3        
    bcc     L5cbb                   ;2/3      
    lda     #$1c                    ;2        
    sta     ram_D6                  ;3        
    lda     ram_E8                  ;3        
    ora     #$01                    ;2        
    sta     ram_E8                  ;3   =  20
L5cbb
    lda     ram_D6                  ;3        
    cmp     #$78                    ;2        
    bcc     L5ccb                   ;2/3      
    lda     #$78                    ;2        
    sta     ram_D6                  ;3        
    lda     ram_E8                  ;3        
    and     #$fe                    ;2        
    sta     ram_E8                  ;3   =  20
L5ccb
    bit     CXPPMM                  ;3        
    bpl     L5cde                   ;2/3      
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcc     L5cde                   ;2/3      
    lda     ram_D6                  ;3        
    clc                             ;2        
    adc     #$03                    ;2        
    sta     ram_D6                  ;3        
    jmp     L5d0d                   ;3   =  25
    
L5cde
    bit     CXPPMM                  ;3        
    bpl     L5cee                   ;2/3      
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcs     L5cee                   ;2/3      
    lda     ram_D6                  ;3        
    sec                             ;2        
    sbc     #$03                    ;2        
    sta     ram_D6                  ;3   =  22
L5cee
    bit     CXPPMM                  ;3        
    bpl     L5d0d                   ;2/3!     
    lda     ram_D5                  ;3        
    sec                             ;2        
    sbc     #$08                    ;2        
    sta     ram_D5                  ;3        
    lda     #RED|$2                 ;2        
    sta     COLUP1                  ;3        
    lda     #$02                    ;2        
    sta     ram_E4                  ;3        
    lda     #$07                    ;2        
    sta     AUDV0                   ;3        
    lda     #$0a                    ;2        
    sta     AUDC1                   ;3        
    lda     #$11                    ;2        
    sta     AUDF1                   ;3   =  40
L5d0d
    lda     ram_DC                  ;3        
    cmp     #$00                    ;2        
    bne     L5d16                   ;2/3      
    jmp     L5d1c                   ;3   =  10
    
L5d16
    lda     ram_DC                  ;3        
    cmp     #$0a                    ;2        
    bne     L5d28                   ;2/3 =   7
L5d1c
    lda     #$03                    ;2        
    sta     AUDV1                   ;3        
    lda     #$0d                    ;2        
    sta     AUDC1                   ;3        
    lda     #$11                    ;2        
    sta     AUDF1                   ;3   =  15
L5d28
    lda     #$14                    ;2        
    cmp     ram_DC                  ;3        
    bcs     L5d3a                   ;2/3      
    lda     #$03                    ;2        
    sta     AUDV1                   ;3        
    lda     #$0e                    ;2        
    sta     AUDC1                   ;3        
    lda     #$08                    ;2        
    sta     AUDF1                   ;3   =  22
L5d3a
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcc     L5d48                   ;2/3      
    inc     ram_D6                  ;5        
    lda     #$08                    ;2        
    sta     REFP1                   ;3        
    jmp     L5d4f                   ;3   =  20
    
L5d48
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcs     L5d4f                   ;2/3      
    dec     ram_D6                  ;5   =  12
L5d4f
    lda     #$00                    ;2        
    cmp     ram_E7                  ;3        
    bcs     L5d58                   ;2/3      
    jmp     L5db1                   ;3   =  10
    
L5d58
    lda     #$64                    ;2        
    cmp     ram_D7                  ;3        
    bcs     L5d62                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_D7                  ;3   =  12
L5d62
    lda     ram_E3                  ;3        
    ora     #$10                    ;2        
    sta     ram_E3                  ;3        
    lda     #$4d                    ;2        
    sta     ram_D6                  ;3        
    lda     #GREEN_BEIGE            ;2        
    sta     COLUP1                  ;3        
    lda     ram_D7                  ;3        
    cmp     #$00                    ;2        
    bne     L5d8e                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$5d                    ;2        
    pha                             ;3        
    lda     #$8d                    ;2        
    pha                             ;3        
    lda     #$dc                    ;2        
    pha                             ;3        
    lda     #$58                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  64
    
L5d8e
    lda     ram_D7                  ;3        
    cmp     #$40                    ;2        
    bcs     L5da2                   ;2/3      
    inc     ram_D7                  ;5        
    lda     #$04                    ;2        
    sta     AUDV1                   ;3        
    lda     ram_D7                  ;3        
    sta     AUDC1                   ;3        
    lda     #$11                    ;2        
    sta     AUDF1                   ;3   =  28
L5da2
    lda     ram_D7                  ;3        
    clc                             ;2        
    adc     #$04                    ;2        
    cmp     ram_D5                  ;3        
    bcs     L5db1                   ;2/3      
    lda     ram_E0                  ;3        
    and     #$fe                    ;2        
    sta     ram_E0                  ;3   =  20
L5db1
    jmp     Lffdd                   ;3   =   3
    
    lda     #$fa                    ;2        
    sta     ram_89                  ;3        
    lda     ram_DC                  ;3        
    cmp     #$00                    ;2        
    bne     L5dc1                   ;2/3      
    jmp     L5dc7                   ;3   =  15
    
L5dc1
    lda     ram_DC                  ;3        
    cmp     #$14                    ;2        
    bne     L5dc9                   ;2/3 =   7
L5dc7
    inc     ram_E2                  ;5   =   5
L5dc9
    lda     #$04                    ;2        
    cmp     ram_E2                  ;3        
    bcs     L5dd3                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_E2                  ;3   =  12
L5dd3
    lda     ram_E2                  ;3        
    cmp     #$04                    ;2        
    bcs     L5ddd                   ;2/3      
    lda     #BLACK                  ;2        
    sta     COLUBK                  ;3   =  12
L5ddd
    lda     ram_E2                  ;3        
    cmp     #$04                    ;2        
    bne     L5df3                   ;2/3      
    lda     #BLACK|$6               ;2        
    sta     COLUBK                  ;3        
    lda     #$03                    ;2        
    sta     AUDV1                   ;3        
    lda     #$0b                    ;2        
    sta     AUDC1                   ;3        
    lda     #$08                    ;2        
    sta     AUDF1                   ;3   =  27
L5df3
    lda     ram_E3                  ;3        
    ora     #$01                    ;2        
    sta     ram_E3                  ;3        
    sta     ram_EE                  ;3        
    lda     #$5e                    ;2        
    pha                             ;3        
    lda     #$10                    ;2        
    pha                             ;3        
    lda     #$dc                    ;2        
    pha                             ;3        
    lda     #$ff                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  47
    
    lda     ram_D5                  ;3        
    clc                             ;2        
    adc     #$08                    ;2        
    sta     ram_D7                  ;3        
    lda     ram_D4                  ;3        
    sec                             ;2        
    sbc     #$0e                    ;2        
    sta     ram_D6                  ;3        
    lda     #$07                    ;2        
    sta     NUSIZ1                  ;3        
    jmp     Lffdd                   ;3   =  28
   
  IF PLUSROM = 1
    ORG     $2fd0, $ff
    RORG    $5fd0
  ELSE
    ORG     $2fd4, $ff
    RORG    $5fd4
  ENDIF

Start_b2
    ldx #$ff
    txs
    lda #$f2
    pha
    lda #$4f
    pha
    
L5fdd
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    tsx                             ;2        
    lda     CXM0FB,x                ;4        
    rol                             ;2        
    rol                             ;2        
    rol                             ;2        
    rol                             ;2        
    and     #$07                    ;2        
    tax                             ;2        
    inx                             ;2   =  28
L5feb
    lda     $1ff3,x                 ;4        
    pla                             ;4        
    tax                             ;2        
    pla                             ;4        
    rts                             ;6   =  20

    ORG     $2ff3, $ff
    RORG    $5ff3

L5ff3
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff     ; $5ff3 (*)

  IF PLUSROM = 1
    .word (PlusROM_API - $6000)             ; PlusRom API pointer
  ELSE
    .byte   $ff,$ff                         ; $5ffa (D)
  ENDIF

    .word Start_b2, Start_b2


;***********************************************************
;      Bank 3 / 0..7
;***********************************************************

    SEG     CODE
    ORG     $3000
    RORG    $7000

L7000
    lda     #BLACK                  ;2        
    sta     COLUPF                  ;3        
    lda     #$4b                    ;2        
    sta     ram_D6                  ;3        
    sta     ram_EE                  ;3        
    lda     #$70                    ;2        
    pha                             ;3        
    lda     #$1f                    ;2        
    pha                             ;3        
    lda     #$f5                    ;2        
    pha                             ;3        
    lda     #$71                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  49
    
    lda     #$00                    ;2        
    sta     ram_8E                  ;3        
    sta     ram_8F                  ;3   =   8
L7026
    lda     ram_D6                  ;3        
    cmp     #$4b                    ;2        
    beq     L702f                   ;2/3      
    jmp     L70aa                   ;3   =  10 *
    
L702f
    inc     ram_D5                  ;5        
    lda     #$28                    ;2        
    cmp     ram_D5                  ;3        
    bcs     L703b                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_D5                  ;3   =  17
L703b
    lda     #$00                    ;2        
    sta     AUDV1                   ;3        
    lda     #BLACK|$6                    ;2        
    sta     COLUBK                  ;3        
    lda     #$05                    ;2        
    sta     NUSIZ1                  ;3        
    sta     NUSIZ0                  ;3        
    lda     #BLACK                  ;2        
    sta     COLUP1                  ;3        
    sta     COLUP0                  ;3        
    lda     #$58                    ;2        
    sta     ram_85                  ;3        
    lda     #$40                    ;2        
    sta     ram_80                  ;3        
    lda     ram_85                  ;3        
    sta     ram_86                  ;3        
    lda     ram_80                  ;3        
    clc                             ;2        
    adc     #$10                    ;2        
    sta     ram_81                  ;3        
    lda     #$06                    ;2        
    sta     ram_A3                  ;3        
    lda     ram_D4                  ;3        
    cmp     #$07                    ;2        
    bcc     L706f                   ;2/3      
    jmp     L70aa                   ;3   =  67
    
L706f
    lda     ram_D4                  ;3        
    cmp     #$07                    ;2        
    bcs     L707c                   ;2/3      
    ldx     ram_D4                  ;3        
    lda     L7e05,x                 ;4        
    sta     ram_9C                  ;3   =  17
L707c
    lda     #$06                    ;2        
    sta     AUDV1                   ;3        
    lda     #$0d                    ;2        
    sta     AUDC1                   ;3        
    lda     ram_9C                  ;3        
    sta     AUDF1                   ;3        
    lda     ram_D5                  ;3        
    cmp     #$00                    ;2        
    bne     L7091                   ;2/3      
    jmp     L7097                   ;3   =  26
    
L7091
    lda     ram_D5                  ;3        
    cmp     #$0a                    ;2        
    bne     L7099                   ;2/3 =   7
L7097
    inc     ram_D4                  ;5   =   5
L7099
    lda     ram_D5                  ;3        
    cmp     #$14                    ;2        
    bne     L70a2                   ;2/3      
    jmp     L70a8                   ;3   =  10
    
L70a2
    lda     ram_D5                  ;3        
    cmp     #$1e                    ;2        
    bne     L70aa                   ;2/3 =   7
L70a8
    inc     ram_D4                  ;5   =   5
L70aa
    sta     ram_EE                  ;3        
    lda     #$70                    ;2        
    pha                             ;3        
    lda     #$c1                    ;2        
    pha                             ;3        
    lda     #$f4                    ;2        
    pha                             ;3        
    lda     #$35                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  39
    
    lda     ram_D6                  ;3        
    cmp     #$4b                    ;2        
    beq     L70cb                   ;2/3      
    jmp     L7000                   ;3   =  10 *
    
L70cb
    lda     ram_8E                  ;3        
    cmp     #$50                    ;2        
    bcs     L70d3                   ;2/3      
    inc     ram_8E                  ;5   =  12
L70d3
    lda     ram_8F                  ;3        
    cmp     #$50                    ;2        
    bcs     L70db                   ;2/3      
    inc     ram_8F                  ;5   =  12
L70db
    lda     ram_8E                  ;3        
    cmp     #$42                    ;2        
    bcs     L70e4                   ;2/3      
    jmp     L7104                   ;3   =  10
    
L70e4
    lda     #$01                    ;2        
    bit     SWCHB                   ;4        
    bne     L70ee                   ;2/3      
    jmp     L70f2                   ;3   =  11 *
    
L70ee
    bit     INPT4                   ;3        
    bmi     L7104                   ;2/3!=   5
L70f2
    sta     ram_EE                  ;3        
    lda     #$10                    ;2        
    pha                             ;3        
    lda     #$45                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$01                    ;2        
    jmp     Lffeb                   ;3   =  29
    
L7104
    jmp     L7026                   ;3   =   3
    
    jmp L7000
    lda $e0
    ora #$08
    sta $e0
    lda $e3
    ora #$01
    sta $e3
    lda #MAUVE
    sta COLUP1
    sta $ee
    lda #$71
    pha
    lda #$31
    pha
    lda #$f4
    pha
    lda #$d5
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    bit $07
    bpl l713e
    inc $e5
    lda $e3
    ora #$08
    sta $e3
l713e
    lda #$07
    sta $1a
    lda #$0c
    sta $16
    lda #$00
    cmp ram_DC
    bcs l7150
    lda #$08
    sta $18
l7150
    lda #$0a
    cmp ram_DC
    bcs l715a
    lda #$11
    sta $18
l715a
    lda #$14
    cmp ram_DC
    bcs l7164
    lda #$17
    sta $18
l7164
    lda #$1e
    cmp ram_DC
    bcs L716e
    lda #$1f
    sta $18
L716e
    jmp     Lffdd
    
L7171
    lda     #CYAN                   ;2        
    sta     COLUP1                  ;3        
    lda     #$20                    ;2        
    sta     NUSIZ1                  ;3        
    lda     #$10                    ;2        
    sta     ram_87                  ;3        
    lda     ram_DC                  ;3        
    cmp     #$02                    ;2        
    bcs     L7186                   ;2/3      
    jmp     L71be                   ;3   =  25
    
L7186
    lda     ram_DA                  ;3        
    cmp     #$5a                    ;2        
    bne     L7194                   ;2/3      
    lda     ram_DB                  ;3        
    cmp     #$50                    ;2        
    bcs     L7194                   ;2/3      
    inc     ram_DB                  ;5   =  19
L7194
    lda     ram_DB                  ;3        
    cmp     #$50                    ;2        
    bne     L71a2                   ;2/3      
    lda     #$42                    ;2        
    cmp     ram_DA                  ;3        
    bcs     L71a2                   ;2/3      
    dec     ram_DA                  ;5   =  19
L71a2
    lda     ram_DA                  ;3        
    cmp     #$42                    ;2        
    bne     L71b0                   ;2/3      
    lda     #$38                    ;2        
    cmp     ram_DB                  ;3        
    bcs     L71b0                   ;2/3      
    dec     ram_DB                  ;5   =  19
L71b0
    lda     ram_DB                  ;3        
    cmp     #$38                    ;2        
    bne     L71be                   ;2/3      
    lda     ram_DA                  ;3        
    cmp     #$5a                    ;2        
    bcs     L71be                   ;2/3      
    inc     ram_DA                  ;5   =  19
L71be
    bit     CXP0FB                  ;3        
    bvc     L71cf                   ;2/3      
    lda     #$00                    ;2        
    cmp     ram_E7                  ;3        
    bcs     L71cf                   ;2/3      
    lda     ram_D4                  ;3         *
    sec                             ;2         *
    sbc     #$02                    ;2         *
    sta     ram_D4                  ;3   =  22 *
L71cf
    lda     ram_E7                  ;3        
    cmp     #$00                    ;2        
    bne     L71df                   ;2/3      
    lda     ram_E8                  ;3        
    and     #$02                    ;2        
    bne     L71df                   ;2/3      
    lda     #$fa                    ;2        
    sta     ram_D7                  ;3   =  19
L71df
    lda     ram_E8                  ;3        
    and     #$02                    ;2        
    beq     L71e8                   ;2/3      
    jmp     L7219                   ;3   =  10
    
L71e8
    lda     ram_E7                  ;3        
    cmp     #$00                    ;2        
    bne     L7205                   ;2/3!     
    lda     #$00                    ;2        
    cmp     ram_92                  ;3        
    bcs     L7205                   ;2/3!     
    dec     ram_92                  ;5        
    lda     #$07                    ;2        
    sta     AUDV1                   ;3        
    lda     #$0c                    ;2        
    sta     AUDC0                   ;3        
    lda     #$1e                    ;2        
    sec                             ;2        
    sbc     ram_92                  ;3        
    sta     AUDF0                   ;3   =  39
L7205
    lda     ram_92                  ;3        
    cmp     #$00                    ;2        
    bne     L7219                   ;2/3      
    lda     ram_E8                  ;3        
    ora     #$02                    ;2        
    sta     ram_E8                  ;3        
    lda     #$27                    ;2        
    sta     ram_89                  ;3        
    lda     #$89                    ;2        
    sta     ram_84                  ;3   =  25
L7219
    lda     ram_E8                  ;3        
    and     #$02                    ;2        
    bne     L7222                   ;2/3      
    jmp     L7248                   ;3   =  10
    
L7222
    bit     CXP0FB                  ;3        
    bvc     L7248                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$72                    ;2        
    pha                             ;3        
    lda     #$3d                    ;2        
    pha                             ;3        
    lda     #$dd                    ;2        
    pha                             ;3        
    lda     #$90                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  44
    
    lda     ram_E8                  ;3        
    ora     #$04                    ;2        
    sta     ram_E8                  ;3        
    lda     #$fa                    ;2        
    sta     ram_89                  ;3   =  13
L7248
    lda     ram_E8                  ;3        
    and     #$04                    ;2        
    bne     L7251                   ;2/3      
    jmp     L72ad                   ;3   =  10
    
L7251
    lda     #$7c                    ;2        
    sta     ram_D6                  ;3        
    lda     #$26                    ;2        
    sta     ram_D7                  ;3        
    lda     #$fa                    ;2        
    sta     ram_89                  ;3        
    lda     ram_E3                  ;3        
    ora     #$01                    ;2        
    sta     ram_E3                  ;3        
    bit     CXPPMM                  ;3        
    bpl     L7284                   ;2/3      
    lda     ram_F3                  ;3        
    cmp     #$c0                    ;2        
    bcs     L7284                   ;2/3      
    lda     ram_F3                  ;3        
    clc                             ;2        
    adc     #$20                    ;2        
    sta     ram_F3                  ;3        
    lda     #$00                    ;2        
    sta     ram_ED                  ;3        
    sta     ram_DC                  ;3        
    lda     #$fa                    ;2        
    sta     ram_D7                  ;3        
    lda     ram_E8                  ;3        
    and     #$fb                    ;2        
    sta     ram_E8                  ;3   =  66
L7284
    bit     CXPPMM                  ;3        
    bpl     L72ad                   ;2/3      
    lda     #$c0                    ;2        
    cmp     ram_F3                  ;3        
    bcs     L72ad                   ;2/3      
    sed                             ;2         *
    clc                             ;2         *
    lda     ram_94                  ;3         *
    adc     #$01                    ;2         *
    sta     ram_94                  ;3         *
    lda     ram_93                  ;3         *
    adc     #$00                    ;2         *
    sta     ram_93                  ;3         *
    cld                             ;2         *
    lda     #$00                    ;2         *
    sta     ram_ED                  ;3         *
    sta     ram_DC                  ;3         *
    lda     #$fa                    ;2         *
    sta     ram_D7                  ;3         *
    lda     ram_E8                  ;3         *
    and     #$fb                    ;2         *
    sta     ram_E8                  ;3   =  55 *
L72ad
    lda     ram_ED                  ;3        
    cmp     #$07                    ;2        
    bcc     L72b6                   ;2/3      
    jmp     L72f1                   ;3   =  10
    
L72b6
    lda     ram_ED                  ;3        
    cmp     #$07                    ;2        
    bcs     L72c3                   ;2/3      
    ldx     ram_ED                  ;3        
    lda     L7e05,x                 ;4        
    sta     ram_9C                  ;3   =  17
L72c3
    lda     #$06                    ;2        
    sta     AUDV1                   ;3        
    lda     #$0d                    ;2        
    sta     AUDC1                   ;3        
    lda     ram_9C                  ;3        
    sta     AUDF1                   ;3        
    lda     ram_DC                  ;3        
    cmp     #$00                    ;2        
    bne     L72d8                   ;2/3      
    jmp     L72de                   ;3   =  26
    
L72d8
    lda     ram_DC                  ;3        
    cmp     #$0a                    ;2        
    bne     L72e0                   ;2/3 =   7
L72de
    inc     ram_ED                  ;5   =   5
L72e0
    lda     ram_DC                  ;3        
    cmp     #$14                    ;2        
    bne     L72e9                   ;2/3      
    jmp     L72ef                   ;3   =  10
    
L72e9
    lda     ram_DC                  ;3        
    cmp     #$1e                    ;2        
    bne     L72f1                   ;2/3 =   7
L72ef
    inc     ram_ED                  ;5   =   5
L72f1
    lda     #$00                    ;2        
    cmp     ram_E7                  ;3        
    bcc     L72fa                   ;2/3      
    jmp     L7360                   ;3   =  10
    
L72fa
    bit     CXP0FB                  ;3        
    bvc     L7306                   ;2/3!     
    lda     ram_84                  ;3         *
    cmp     #$6e                    ;2         *
    bne     L7306                   ;2/3       *
    dec     ram_D4                  ;5   =  17 *
L7306
    sta     ram_EE                  ;3        
    lda     #$73                    ;2        
    pha                             ;3        
    lda     #$1d                    ;2        
    pha                             ;3        
    lda     #$db                    ;2        
    pha                             ;3        
    lda     #$bc                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  39
    
    lda     #$10                    ;2        
    sta     ram_89                  ;3        
    lda     #$72                    ;2        
    sta     ram_84                  ;3        
    lda     #$0c                    ;2        
    sta     ram_92                  ;3        
    lda     ram_D7                  ;3        
    cmp     #$20                    ;2        
    bcs     L7332                   ;2/3      
    inc     ram_D7                  ;5   =  27
L7332
    lda     #$14                    ;2        
    cmp     ram_D6                  ;3        
    bcc     L7340                   ;2/3      
    inc     ram_D6                  ;5        
    lda     ram_E8                  ;3        
    and     #$fe                    ;2        
    sta     ram_E8                  ;3   =  20
L7340
    lda     ram_D6                  ;3        
    cmp     #$68                    ;2        
    bcc     L734e                   ;2/3      
    dec     ram_D6                  ;5        
    lda     ram_E8                  ;3        
    ora     #$01                    ;2        
    sta     ram_E8                  ;3   =  20
L734e
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcs     L7355                   ;2/3      
    inc     ram_D6                  ;5   =  12
L7355
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcc     L7360                   ;2/3      
    dec     ram_D6                  ;5        
    lda     #$08                    ;2        
    sta     REFP1                   ;3   =  17
L7360
    jmp     Lffdd                   ;3   =   3
    
    lda     ram_D5                  ;3        
    cmp     #$06                    ;2        
    bcs     L7380                   ;2/3      
    lda     ram_E3                  ;3        
    ora     #$08                    ;2        
    sta     ram_E3                  ;3        
    inc     ram_E5                  ;5        
    sed                             ;2        
    clc                             ;2        
    lda     ram_94                  ;3        
    adc     #$01                    ;2        
    sta     ram_94                  ;3        
    lda     ram_93                  ;3        
    adc     #$00                    ;2        
    sta     ram_93                  ;3        
    cld                             ;2   =  42
L7380
    lda     #$20                    ;2        
    sta     ram_92                  ;3        
    lda     #$80                    ;2        
    sta     ram_84                  ;3        
    lda     ram_E3                  ;3        
    ora     #$80                    ;2        
    sta     ram_E3                  ;3        
    lda     #$15                    ;2        
    sta     CTRLPF                  ;3        
    lda     ram_E7                  ;3        
    cmp     #$00                    ;2        
    beq     L739b                   ;2/3      
    jmp     L742f                   ;3   =  33
    
L739b
    lda     ram_E3                  ;3        
    ora     #$01                    ;2        
    sta     ram_E3                  ;3        
    lda     #RED|$4                 ;2        
    sta     COLUP1                  ;3        
    lda     ram_D7                  ;3        
    cmp     #$2e                    ;2        
    bcs     L73ad                   ;2/3      
    inc     ram_D7                  ;5   =  25
L73ad
    bit     CXPPMM                  ;3        
    bpl     L73c8                   ;2/3      
    lda     ram_F3                  ;3        
    cmp     #$c0                    ;2        
    bcs     L73c8                   ;2/3      
    lda     #$fa                    ;2        
    sta     ram_D7                  ;3        
    lda     ram_F3                  ;3        
    clc                             ;2        
    adc     #$20                    ;2        
    sta     ram_F3                  ;3        
    lda     #$00                    ;2        
    sta     ram_ED                  ;3        
    sta     ram_DC                  ;3   =  35
L73c8
    bit     CXPPMM                  ;3        
    bpl     L73eb                   ;2/3      
    lda     #$c0                    ;2        
    cmp     ram_F3                  ;3        
    bcs     L73eb                   ;2/3      
    lda     #$fa                    ;2         *
    sta     ram_D7                  ;3         *
    sed                             ;2         *
    clc                             ;2         *
    lda     ram_94                  ;3         *
    adc     #$01                    ;2         *
    sta     ram_94                  ;3         *
    lda     ram_93                  ;3         *
    adc     #$00                    ;2         *
    sta     ram_93                  ;3         *
    cld                             ;2         *
    lda     #$00                    ;2         *
    sta     ram_ED                  ;3         *
    sta     ram_DC                  ;3   =  47 *
L73eb
    lda     ram_ED                  ;3        
    cmp     #$07                    ;2        
    bcc     L73f4                   ;2/3      
    jmp     L742f                   ;3   =  10
    
L73f4
    lda     ram_ED                  ;3        
    cmp     #$07                    ;2        
    bcs     L7401                   ;2/3!     
    ldx     ram_ED                  ;3        
    lda     L7e05,x                 ;4        
    sta     ram_9C                  ;3   =  17
L7401
    lda     #$06                    ;2        
    sta     AUDV1                   ;3        
    lda     #$0d                    ;2        
    sta     AUDC1                   ;3        
    lda     ram_9C                  ;3        
    sta     AUDF1                   ;3        
    lda     ram_DC                  ;3        
    cmp     #$00                    ;2        
    bne     L7416                   ;2/3      
    jmp     L741c                   ;3   =  26
    
L7416
    lda     ram_DC                  ;3        
    cmp     #$0a                    ;2        
    bne     L741e                   ;2/3 =   7
L741c
    inc     ram_ED                  ;5   =   5
L741e
    lda     ram_DC                  ;3        
    cmp     #$14                    ;2        
    bne     L7427                   ;2/3      
    jmp     L742d                   ;3   =  10
    
L7427
    lda     ram_DC                  ;3        
    cmp     #$1e                    ;2        
    bne     L742f                   ;2/3 =   7
L742d
    inc     ram_ED                  ;5   =   5
L742f
    lda     #$00                    ;2        
    cmp     ram_E7                  ;3        
    bcc     L7449                   ;2/3      
    lda     ram_89                  ;3        
    cmp     #$20                    ;2        
    bcs     L7449                   ;2/3      
    inc     ram_89                  ;5        
    lda     #$05                    ;2        
    sta     AUDV1                   ;3        
    lda     #$08                    ;2        
    sta     AUDC1                   ;3        
    lda     ram_89                  ;3        
    sta     AUDF1                   ;3   =  35
L7449
    lda     ram_D4                  ;3        
    cmp     #$75                    ;2        
    bcs     L7458                   ;2/3      
    lda     ram_E0                  ;3        
    and     #$fe                    ;2        
    sta     ram_E0                  ;3        
    jmp     L74b9                   ;3   =  18
    
L7458
    bit     CXP0FB                  ;3        
    bvc     L746e                   ;2/3      
    lda     ram_E0                  ;3        
    ora     #$01                    ;2        
    sta     ram_E0                  ;3        
    lda     ram_E0                  ;3        
    and     #$f7                    ;2        
    sta     ram_E0                  ;3        
    lda     ram_E0                  ;3        
    and     #$fd                    ;2        
    sta     ram_E0                  ;3   =  29
L746e
    bit     CXP0FB                  ;3        
    bvs     L747b                   ;2/3      
    lda     ram_E0                  ;3        
    and     #$fe                    ;2        
    sta     ram_E0                  ;3        
    jmp     L74b9                   ;3   =  16
    
L747b
    sta     ram_EE                  ;3        
    lda     #$74                    ;2        
    pha                             ;3        
    lda     #$92                    ;2        
    pha                             ;3        
    lda     #$da                    ;2        
    pha                             ;3        
    lda     #$0b                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  39
    
    lda     ram_E0                  ;3        
    ora     #$08                    ;2        
    sta     ram_E0                  ;3        
    bit     CXP0FB                  ;3        
    bpl     L74a7                   ;2/3      
    lda     ram_D5                  ;3         *
    clc                             ;2         *
    adc     #$02                    ;2         *
    sta     ram_D5                  ;3         *
    jmp     L74b9                   ;3   =  26 *
    
L74a7
    lda     #$10                    ;2        
    bit     SWCHA                   ;4        
    bne     L74b0                   ;2/3      
    dec     ram_D5                  ;5   =  13
L74b0
    lda     #$20                    ;2        
    bit     SWCHA                   ;4        
    bne     L74b9                   ;2/3      
    inc     ram_D5                  ;5   =  13 *
L74b9
    lda     ram_E8                  ;3        
    and     #$02                    ;2        
    beq     L74c2                   ;2/3      
    jmp     L755a                   ;3   =  10
    
L74c2
    lda     ram_E7                  ;3        
    cmp     #$02                    ;2        
    bne     L74d0                   ;2/3      
    lda     #CYAN                   ;2        
    sta     COLUP1                  ;3        
    lda     #$40                    ;2        
    sta     ram_D7                  ;3   =  17
L74d0
    lda     ram_E7                  ;3        
    cmp     #$01                    ;2        
    bne     L74de                   ;2/3      
    lda     #CYAN_GREEN|$2          ;2        
    sta     COLUP1                  ;3        
    lda     #$20                    ;2        
    sta     ram_D7                  ;3   =  17
L74de
    lda     #$00                    ;2        
    cmp     ram_E7                  ;3        
    bcc     L750d                   ;2/3!     
    sta     ram_EE                  ;3        
    lda     #$74                    ;2        
    pha                             ;3        
    lda     #$fb                    ;2        
    pha                             ;3        
    lda     #$dd                    ;2        
    pha                             ;3        
    lda     #$90                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  46
    
    lda     #$15                    ;2        
    sta     ram_D6                  ;3        
    lda     #$00                    ;2        
    sta     ram_D7                  ;3        
    lda     ram_E8                  ;3        
    ora     #$02                    ;2        
    sta     ram_E8                  ;3        
    jmp     L755a                   ;3   =  21
    
L750d
    sta     ram_EE                  ;3        
    lda     #$75                    ;2        
    pha                             ;3        
    lda     #$24                    ;2        
    pha                             ;3        
    lda     #$db                    ;2        
    pha                             ;3        
    lda     #$bc                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  39
    
    lda     ram_D6                  ;3        
    cmp     #$6c                    ;2        
    bcc     L7535                   ;2/3      
    lda     #$6c                    ;2        
    sta     ram_D6                  ;3        
    lda     ram_E8                  ;3        
    ora     #$01                    ;2        
    sta     ram_E8                  ;3   =  20
L7535
    lda     #$28                    ;2        
    cmp     ram_D6                  ;3        
    bcc     L7545                   ;2/3      
    lda     #$28                    ;2        
    sta     ram_D6                  ;3        
    lda     ram_E8                  ;3        
    and     #$fe                    ;2        
    sta     ram_E8                  ;3   =  20
L7545
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcs     L754f                   ;2/3      
    inc     ram_D6                  ;5        
    jmp     L755a                   ;3   =  15
    
L754f
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcc     L755a                   ;2/3      
    dec     ram_D6                  ;5        
    lda     #$08                    ;2        
    sta     REFP1                   ;3   =  17
L755a
    jmp     Lffdd                   ;3   =   3
    
    lda     ram_E3                  ;3        
    ora     #$01                    ;2        
    sta     ram_E3                  ;3        
    lda     ram_E5                  ;3        
    cmp     #$29                    ;2        
    bne     L756c                   ;2/3      
    jmp     L7617                   ;3   =  18 *
    
L756c
    lda     ram_EB                  ;3        
    and     #$02                    ;2        
    beq     L7575                   ;2/3      
    jmp     L7617                   ;3   =  10
    
L7575
    lda     #$09                    ;2        
    sta     ram_92                  ;3        
    lda     ram_E8                  ;3        
    and     #$02                    ;2        
    beq     L75a7                   ;2/3      
    ldx     #$01                    ;2        
    lda     #$01                    ;2        
    sta     ram_9E                  ;3        
    ldy     #$03                    ;2        
    lda     #$00                    ;2        
    sta     ram_EE                  ;3        
    lda     #$75                    ;2        
    pha                             ;3        
    lda     #$a0                    ;2        
    pha                             ;3        
    lda     #$f2                    ;2        
    pha                             ;3        
    lda     #$e8                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  62
    
    lda     ram_E8                  ;3        
    and     #$fd                    ;2        
    sta     ram_E8                  ;3   =   8
L75a7
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcs     L75b4                   ;2/3      
    lda     #$20                    ;2        
    sta     ram_89                  ;3        
    lda     #$12                    ;2        
    sta     ram_84                  ;3   =  17
L75b4
    bit     CXP0FB                  ;3        
    bvc     L75bf                   ;2/3      
    lda     ram_D4                  ;3        
    clc                             ;2        
    adc     #$02                    ;2        
    sta     ram_D4                  ;3   =  15
L75bf
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcc     L75c7                   ;2/3      
    jmp     L75f5                   ;3   =  10
    
L75c7
    lda     ram_D4                  ;3        
    cmp     #$13                    ;2        
    beq     L75d6                   ;2/3      
    lda     ram_D5                  ;3        
    cmp     #$20                    ;2        
    beq     L75d6                   ;2/3      
    jmp     L75f5                   ;3   =  17
    
L75d6
    lda     #$20                    ;2        
    bit     SWCHA                   ;4        
    bne     L75f5                   ;2/3      
    bit     SWCHA                   ;4        
    bvs     L75f5                   ;2/3      
    lda     ram_E8                  ;3        
    ora     #$01                    ;2        
    sta     ram_E8                  ;3        
    lda     ram_E8                  ;3        
    ora     #$02                    ;2        
    sta     ram_E8                  ;3        
    lda     ram_D4                  ;3        
    clc                             ;2        
    adc     #$02                    ;2        
    sta     ram_D4                  ;3   =  40
L75f5
    lda     ram_DC                  ;3        
    cmp     #$14                    ;2        
    beq     L75fe                   ;2/3      
    jmp     L7617                   ;3   =  10
    
L75fe
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcc     L7617                   ;2/3      
    lda     #$18                    ;2        
    cmp     ram_89                  ;3        
    bcs     L7617                   ;2/3      
    dec     ram_89                  ;5        
    lda     #$07                    ;2        
    sta     AUDV1                   ;3        
    lda     #$09                    ;2        
    sta     AUDC1                   ;3        
    lda     ram_89                  ;3        
    sta     AUDF1                   ;3   =  35
L7617
    lda     #$15                    ;2        
    cmp     ram_D4                  ;3        
    bcc     L762d                   ;2/3      
    lda     ram_E5                  ;3        
    cmp     #$12                    ;2        
    bne     L762d                   ;2/3      
    lda     #$64                    ;2        
    sta     ram_E5                  ;3        
    lda     ram_E3                  ;3        
    ora     #$08                    ;2        
    sta     ram_E3                  ;3   =  27
L762d
    bit     CXPPMM                  ;3        
    bpl     L7690                   ;2/3      
    ldx     #$01                    ;2        
    lda     #$1f                    ;2        
    sta     ram_9E                  ;3        
    ldy     #$03                    ;2        
    lda     #$1e                    ;2        
    sta     ram_EE                  ;3        
    lda     #$76                    ;2        
    pha                             ;3        
    lda     #$52                    ;2        
    pha                             ;3        
    lda     #$f2                    ;2        
    pha                             ;3        
    lda     #$e8                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  55
    
    lda     #$89                    ;2        
    sta     ram_D6                  ;3        
    sta     ram_EE                  ;3        
    lda     #$76                    ;2        
    pha                             ;3        
    lda     #$6e                    ;2        
    pha                             ;3        
    lda     #$dc                    ;2        
    pha                             ;3        
    lda     #$0d                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  44
    
    lda     #BLACK                  ;2        
    sta     COLUP1                  ;3        
    lda     #$05                    ;2        
    sta     ram_E4                  ;3        
    lda     ram_EB                  ;3        
    ora     #$01                    ;2        
    sta     ram_EB                  ;3        
    lda     #$05                    ;2        
    sta     AUDV1                   ;3        
    lda     #$0b                    ;2        
    sta     AUDC1                   ;3        
    lda     #$08                    ;2        
    sta     AUDF1                   ;3        
    lda     #$00                    ;2        
    sta     ram_ED                  ;3        
    jmp     L76d1                   ;3   =  41
    
L7690
    lda     ram_EB                  ;3        
    lsr                             ;2        
    bcc     L76a2                   ;2/3      
    lda     ram_E4                  ;3        
    cmp     #$00                    ;2        
    bne     L76a2                   ;2/3      
    lda     #$fa                    ;2        
    sta     ram_D7                  ;3        
    jmp     L76d1                   ;3   =  22
    
L76a2
    lda     #GREEN_BEIGE            ;2        
    sta     COLUP1                  ;3        
    lda     ram_E5                  ;3        
    cmp     #$12                    ;2        
    bne     L76b0                   ;2/3      
    lda     #$4b                    ;2        
    sta     ram_D6                  ;3   =  17
L76b0
    lda     #$64                    ;2        
    cmp     ram_D7                  ;3        
    bcs     L76ba                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_D7                  ;3   =  12
L76ba
    lda     ram_D7                  ;3        
    cmp     #$1f                    ;2        
    bcs     L76d1                   ;2/3      
    inc     ram_D7                  ;5        
    lda     #$05                    ;2        
    sta     AUDV1                   ;3        
    lda     ram_D7                  ;3        
    sta     AUDC1                   ;3        
    lda     #$17                    ;2        
    sta     AUDF1                   ;3        
    jmp     L76d1                   ;3   =  31
    
L76d1
    lda     ram_ED                  ;3        
    cmp     #$07                    ;2        
    bcc     L76da                   ;2/3      
    jmp     L7715                   ;3   =  10
    
L76da
    lda     ram_ED                  ;3        
    cmp     #$07                    ;2        
    bcs     L76e7                   ;2/3      
    ldx     ram_ED                  ;3        
    lda     L7e05,x                 ;4        
    sta     ram_9C                  ;3   =  17
L76e7
    lda     #$06                    ;2        
    sta     AUDV1                   ;3        
    lda     #$0d                    ;2        
    sta     AUDC1                   ;3        
    lda     ram_9C                  ;3        
    sta     AUDF1                   ;3        
    lda     ram_DC                  ;3        
    cmp     #$00                    ;2        
    bne     L76fc                   ;2/3      
    jmp     L7702                   ;3   =  26
    
L76fc
    lda     ram_DC                  ;3        
    cmp     #$0a                    ;2        
    bne     L7704                   ;2/3 =   7
L7702
    inc     ram_ED                  ;5   =   5
L7704
    lda     ram_DC                  ;3        
    cmp     #$14                    ;2        
    bne     L770d                   ;2/3      
    jmp     L7713                   ;3   =  10
    
L770d
    lda     ram_DC                  ;3        
    cmp     #$1e                    ;2        
    bne     L7715                   ;2/3 =   7
L7713
    inc     ram_ED                  ;5   =   5
L7715
    jmp     Lffdd                   ;3   =   3
    
    lda     ram_EB                  ;3        
    ora     #$02                    ;2        
    sta     ram_EB                  ;3        
    lda     ram_E3                  ;3        
    ora     #$01                    ;2        
    sta     ram_E3                  ;3        
    lda     #$85                    ;2        
    cmp     ram_D4                  ;3        
    bcs     L773a                   ;2/3      
    lda     ram_D5                  ;3        
    cmp     #$14                    ;2        
    bcs     L773a                   ;2/3      
    lda     #$12                    ;2        
    sta     ram_E5                  ;3        
    lda     ram_E3                  ;3        
    ora     #$08                    ;2        
    sta     ram_E3                  ;3   =  43
L773a
    lda     #$48                    ;2        
    sta     ram_92                  ;3        
    lda     #$40                    ;2        
    sta     ram_89                  ;3        
    lda     #$1e                    ;2        
    sta     ram_84                  ;3        
    lda     ram_E3                  ;3        
    ora     #$80                    ;2        
    sta     ram_E3                  ;3        
    lda     #$15                    ;2        
    sta     CTRLPF                  ;3        
    bit     CXP0FB                  ;3        
    bvc     L7766                   ;2/3      
    lda     ram_E0                  ;3        
    ora     #$01                    ;2        
    sta     ram_E0                  ;3        
    lda     ram_E0                  ;3        
    and     #$f7                    ;2        
    sta     ram_E0                  ;3        
    lda     ram_E0                  ;3        
    and     #$fd                    ;2        
    sta     ram_E0                  ;3   =  57
L7766
    bit     CXP0FB                  ;3        
    bvs     L7773                   ;2/3      
    lda     ram_E0                  ;3        
    and     #$fe                    ;2        
    sta     ram_E0                  ;3        
    jmp     L77b1                   ;3   =  16
    
L7773
    sta     ram_EE                  ;3        
    lda     #$77                    ;2        
    pha                             ;3        
    lda     #$8a                    ;2        
    pha                             ;3        
    lda     #$da                    ;2        
    pha                             ;3        
    lda     #$0b                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  39
    
    lda     ram_E0                  ;3        
    ora     #$08                    ;2        
    sta     ram_E0                  ;3        
    bit     CXP0FB                  ;3        
    bpl     L779f                   ;2/3      
    lda     ram_D5                  ;3         *
    clc                             ;2         *
    adc     #$02                    ;2         *
    sta     ram_D5                  ;3         *
    jmp     L77b1                   ;3   =  26 *
    
L779f
    lda     #$10                    ;2        
    bit     SWCHA                   ;4        
    bne     L77a8                   ;2/3      
    dec     ram_D5                  ;5   =  13
L77a8
    lda     #$20                    ;2        
    bit     SWCHA                   ;4        
    bne     L77b1                   ;2/3      
    inc     ram_D5                  ;5   =  13 *
L77b1
    bit     CXPPMM                  ;3        
    bpl     L77cb                   ;2/3      
    lda     ram_F3                  ;3        
    cmp     #$c0                    ;2        
    bcs     L77cb                   ;2/3      
    lda     ram_F3                  ;3        
    clc                             ;2        
    adc     #$20                    ;2        
    sta     ram_F3                  ;3        
    lda     #$00                    ;2        
    sta     ram_ED                  ;3        
    sta     ram_DC                  ;3        
    jmp     L77ea                   ;3   =  33
    
L77cb
    bit     CXPPMM                  ;3        
    bpl     L77ea                   ;2/3      
    lda     #$c0                    ;2         *
    cmp     ram_F3                  ;3         *
    bcs     L77ea                   ;2/3       *
    sed                             ;2         *
    clc                             ;2         *
    lda     ram_94                  ;3         *
    adc     #$01                    ;2         *
    sta     ram_94                  ;3         *
    lda     ram_93                  ;3         *
    adc     #$00                    ;2         *
    sta     ram_93                  ;3         *
    cld                             ;2         *
    lda     #$00                    ;2         *
    sta     ram_ED                  ;3         *
    sta     ram_DC                  ;3   =  42 *
L77ea
    lda     ram_ED                  ;3        
    cmp     #$07                    ;2        
    bcc     L77f3                   ;2/3      
    jmp     L782e                   ;3   =  10
    
L77f3
    lda     ram_ED                  ;3        
    cmp     #$07                    ;2        
    bcs     L7800                   ;2/3!     
    ldx     ram_ED                  ;3        
    lda     L7e05,x                 ;4        
    sta     ram_9C                  ;3   =  17
L7800
    lda     #$06                    ;2        
    sta     AUDV1                   ;3        
    lda     #$0d                    ;2        
    sta     AUDC1                   ;3        
    lda     ram_9C                  ;3        
    sta     AUDF1                   ;3        
    lda     ram_DC                  ;3        
    cmp     #$00                    ;2        
    bne     L7815                   ;2/3      
    jmp     L781b                   ;3   =  26
    
L7815
    lda     ram_DC                  ;3        
    cmp     #$0a                    ;2        
    bne     L781d                   ;2/3 =   7
L781b
    inc     ram_ED                  ;5   =   5
L781d
    lda     ram_DC                  ;3        
    cmp     #$14                    ;2        
    bne     L7826                   ;2/3      
    jmp     L782c                   ;3   =  10
    
L7826
    lda     ram_DC                  ;3        
    cmp     #$1e                    ;2        
    bne     L782e                   ;2/3 =   7
L782c
    inc     ram_ED                  ;5   =   5
L782e
    bit     CXPPMM                  ;3        
    bpl     L7846                   ;2/3      
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcc     L7846                   ;2/3      
    lda     ram_E8                  ;3        
    and     #$fe                    ;2        
    sta     ram_E8                  ;3        
    lda     ram_E8                  ;3        
    ora     #$02                    ;2        
    sta     ram_E8                  ;3        
    jmp     L786d                   ;3   =  31
    
L7846
    bit     CXPPMM                  ;3        
    bpl     L785f                   ;2/3      
    lda     ram_E8                  ;3        
    and     #$02                    ;2        
    beq     L785f                   ;2/3      
    lda     ram_E8                  ;3        
    and     #$fd                    ;2        
    sta     ram_E8                  ;3        
    lda     ram_E8                  ;3        
    ora     #$04                    ;2        
    sta     ram_E8                  ;3        
    jmp     L786d                   ;3   =  31
    
L785f
    bit     CXPPMM                  ;3        
    bpl     L786d                   ;2/3      
    lda     ram_E8                  ;3        
    and     #$04                    ;2        
    beq     L786d                   ;2/3      
    lda     #$fa                    ;2        
    sta     ram_D7                  ;3   =  17
L786d
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcc     L7876                   ;2/3      
    lda     #$06                    ;2        
    sta     NUSIZ1                  ;3   =  12
L7876
    lda     ram_E8                  ;3        
    and     #$02                    ;2        
    beq     L7880                   ;2/3      
    lda     #$02                    ;2        
    sta     NUSIZ1                  ;3   =  12
L7880
    lda     ram_E8                  ;3        
    and     #$04                    ;2        
    beq     L788a                   ;2/3      
    lda     #$00                    ;2        
    sta     NUSIZ1                  ;3   =  12
L788a
    lda     #$64                    ;2        
    cmp     ram_D7                  ;3        
    bcs     L7893                   ;2/3      
    jmp     L78a7                   ;3   =  10
    
L7893
    lda     ram_DC                  ;3        
    cmp     #$14                    ;2        
    bcs     L789d                   ;2/3      
    lda     #RED|$4                 ;2        
    sta     COLUP1                  ;3   =  12
L789d
    lda     ram_DC                  ;3        
    cmp     #$14                    ;2        
    bcc     L78a7                   ;2/3      
    lda     #RED|$e                ;2        
    sta     COLUP1                  ;3   =  12
L78a7
    jmp     Lffdd                   ;3   =   3
    
    lda     #$04                    ;2        
    sta     ram_92                  ;3        
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcc     L78b5                   ;2/3      
    dec     ram_84                  ;5   =  17
L78b5
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcs     L78bc                   ;2/3      
    inc     ram_84                  ;5   =  12
L78bc
    lda     #$20                    ;2        
    sta     NUSIZ1                  ;3        
    lda     #$8c                    ;2        
    cmp     ram_DB                  ;3        
    bcs     L78d6                   ;2/3      
    lda     #$40                    ;2        
    sta     ram_DB                  ;3        
    lda     #$06                    ;2        
    sta     AUDV1                   ;3        
    lda     #$03                    ;2        
    sta     AUDC1                   ;3        
    lda     #$08                    ;2        
    sta     AUDF1                   ;3   =  32
L78d6
    lda     ram_DB                  ;3        
    clc                             ;2        
    adc     #$02                    ;2        
    sta     ram_DB                  ;3        
    lda     #$2a                    ;2        
    sta     ram_DA                  ;3        
    lda     #$04                    ;2        
    sta     ram_87                  ;3        
    lda     ram_E1                  ;3        
    cmp     #$28                    ;2        
    bcc     L78ef                   ;2/3      
    lda     #GREEN                  ;2        
    sta     COLUP1                  ;3   =  32
L78ef
    lda     ram_E1                  ;3        
    cmp     #$28                    ;2        
    bne     L790d                   ;2/3!     
    sta     ram_EE                  ;3        
    lda     #$79                    ;2        
    pha                             ;3        
    lda     #$0c                    ;2        
    pha                             ;3        
    lda     #$f5                    ;2        
    pha                             ;3        
    lda     #$53                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  46
    
L790d
    lda     ram_E1                  ;3        
    cmp     #$28                    ;2        
    bcs     L7917                   ;2/3      
    lda     #BLACK                  ;2        
    sta     COLUP1                  ;3   =  12
L7917
    bit     CXPPMM                  ;3        
    bpl     L7955                   ;2/3      
    lda     ram_E0                  ;3        
    ora     #$02                    ;2        
    sta     ram_E0                  ;3        
    lda     #$3c                    ;2        
    sta     ram_E1                  ;3        
    sta     ram_EE                  ;3        
    lda     #$79                    ;2        
    pha                             ;3        
    lda     #$3c                    ;2        
    pha                             ;3        
    lda     #$f5                    ;2        
    pha                             ;3        
    lda     #$62                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  57
    
    sta     ram_EE                  ;3        
    lda     #$79                    ;2        
    pha                             ;3        
    lda     #$54                    ;2        
    pha                             ;3        
    lda     #$da                    ;2        
    pha                             ;3        
    lda     #$41                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  39
    
L7955
    lda     ram_84                  ;3        
    sec                             ;2        
    sbc     #$06                    ;2        
    pha                             ;3        
    tsx                             ;2        
    pla                             ;4        
    lda     ram_D4                  ;3        
    cmp     CXM1P,x                 ;4        
    bcs     L796c                   ;2/3      
    lda     ram_E0                  ;3        
    and     #$fe                    ;2        
    sta     ram_E0                  ;3        
    jmp     L79ce                   ;3   =  36
    
L796c
    lda     ram_84                  ;3        
    clc                             ;2        
    adc     #$04                    ;2        
    cmp     ram_D4                  ;3        
    bcs     L797e                   ;2/3      
    lda     ram_E0                  ;3        
    and     #$fe                    ;2        
    sta     ram_E0                  ;3        
    jmp     L79ce                   ;3   =  23
    
L797e
    bit     CXP0FB                  ;3        
    bvs     L798b                   ;2/3      
    lda     ram_E0                  ;3        
    and     #$fe                    ;2        
    sta     ram_E0                  ;3        
    jmp     L79ce                   ;3   =  16
    
L798b
    bit     CXP0FB                  ;3        
    bvc     L79a3                   ;2/3      
    lda     ram_89                  ;3        
    sec                             ;2        
    sbc     ram_92                  ;3        
    pha                             ;3        
    tsx                             ;2        
    pla                             ;4        
    lda     ram_D5                  ;3        
    cmp     CXM1P,x                 ;4        
    bcc     L79a3                   ;2/3      
    lda     ram_E0                  ;3        
    ora     #$01                    ;2        
    sta     ram_E0                  ;3   =  39
L79a3
    bit     CXP0FB                  ;3        
    bvs     L79aa                   ;2/3      
    jmp     L79ce                   ;3   =   8 *
    
L79aa
    lda     ram_E0                  ;3        
    and     #$08                    ;2        
    beq     L79b3                   ;2/3      
    jmp     L79ce                   ;3   =  10
    
L79b3
    bit     SWCHA                   ;4        
    bvs     L79bb                   ;2/3      
    jmp     L79c0                   ;3   =   9 *
    
L79bb
    bit     SWCHA                   ;4        
    bmi     L79c3                   ;2/3 =   6
L79c0
    jmp     L79ce                   ;3   =   3
    
L79c3
    lda     ram_89                  ;3        
    sec                             ;2        
    sbc     #$03                    ;2        
    sta     ram_D5                  ;3        
    lda     ram_84                  ;3        
    sta     ram_D4                  ;3   =  16
L79ce
    bit     CXBLPF                  ;3        
    bmi     L79d5                   ;2/3      
    jmp     L79fc                   ;3   =   8
    
L79d5
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcc     L79ea                   ;2/3      
    lda     ram_E8                  ;3        
    and     #$fe                    ;2        
    sta     ram_E8                  ;3        
    lda     ram_84                  ;3        
    clc                             ;2        
    adc     #$02                    ;2        
    sta     ram_84                  ;3        
    jmp     L79fc                   ;3   =  28
    
L79ea
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcs     L79fc                   ;2/3      
    lda     ram_E8                  ;3        
    ora     #$01                    ;2        
    sta     ram_E8                  ;3        
    lda     ram_84                  ;3        
    sec                             ;2        
    sbc     #$02                    ;2        
    sta     ram_84                  ;3   =  25
L79fc
    jmp     Lffdd                   ;3   =   3
    
    lda     ram_E8                  ;3        
    ora     #$10                    ;2        
    sta     ram_E8                  ;3        
    lda     ram_E5                  ;3        
    cmp     #$1a                    ;2        
    bne     L7a27                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$7a                    ;2        
    pha                             ;3        
    lda     #$22                    ;2        
    pha                             ;3        
    lda     #$dd                    ;2        
    pha                             ;3        
    lda     #$56                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  54
    
    lda     #BLACK|$2               ;2        
    sta     COLUP1                  ;3   =   5
L7a27
    lda     #$06                    ;2        
    sta     NUSIZ1                  ;3        
    lda     #$10                    ;2        
    cmp     ram_DC                  ;3        
    bcs     L7a34                   ;2/3      
    jmp     L7a6c                   ;3   =  15
    
L7a34
    lda     #$1f                    ;2        
    cmp     ram_D7                  ;3        
    bcs     L7a42                   ;2/3      
    lda     ram_D6                  ;3        
    cmp     #$28                    ;2        
    bne     L7a42                   ;2/3      
    dec     ram_D7                  ;5   =  19
L7a42
    lda     ram_D7                  ;3        
    cmp     #$1f                    ;2        
    bne     L7a50                   ;2/3      
    lda     ram_D6                  ;3        
    cmp     #$3d                    ;2        
    bcs     L7a50                   ;2/3      
    inc     ram_D6                  ;5   =  19
L7a50
    lda     ram_D7                  ;3        
    cmp     #$31                    ;2        
    bcs     L7a5e                   ;2/3      
    lda     ram_D6                  ;3        
    cmp     #$3d                    ;2        
    bne     L7a5e                   ;2/3      
    inc     ram_D7                  ;5   =  19
L7a5e
    lda     ram_D7                  ;3        
    cmp     #$31                    ;2        
    bne     L7a6c                   ;2/3      
    lda     #$20                    ;2        
    cmp     ram_D6                  ;3        
    bcs     L7a6c                   ;2/3      
    dec     ram_D6                  ;5   =  19
L7a6c
    jmp     Lffdd                   ;3   =   3
    
    lda $e8
    ora #$10
    sta $e8
    lda #$28
    sta $d6
    lda $d7
    clc
    adc #$02
    sta $d7
    lda #$06
    sta $05
    lda #RED|$4
    sta COLUP1
    sta $ee
    lda #$7a
    pha
    lda #$9f
    pha
    lda #$da
    pha
    lda #$95
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$07
    jmp Lffeb
    lda #$64
    cmp $d7
    bcs l7aaa
    lda #$00
    sta $d7
l7aaa
    lda ram_DC
    cmp #$14
    bcs l7abc
    lda #$01
    sta $1a
    lda #$08
    sta $16
    lda #$11
    sta $18
l7abc
	lda #$14
    cmp ram_DC
    bcs l7acc
    lda #$01
    sta $1a
    lda #$08
    sta $16
    sta $18
l7acc
	jmp Lffdd
    lda $e8
    ora #$40
    sta $e8
    lda $e8
    ora #$80
    sta $e8
    lda $e7
    cmp #$00
    bne l7ae4
    jmp l7bce
l7ae4
	bit $01
    bvc l7b0b
    lda $d6
    sec
    sbc #$08
    sta $da
    lda $d7
    sta $db
    sta $ee
    lda #$7b
    pha
    lda #$0a
    pha
    lda #$db
    pha
    lda #$71
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$07
    jmp Lffeb
l7b0b
	lda $db
    cmp #$00
    bne l7b14
    jmp l7b1a
l7b14
	lda $db
    cmp #$28
    bne l7b32
l7b1a
	sta $ee
    lda #$7b
    pha
    lda #$31
    pha
    lda #$db
    pha
    lda #$62
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$07
    jmp Lffeb
l7b32
	lda #$64
    cmp $db
    bcs l7b50
    sta $ee
    lda #$7b
    pha
    lda #$4f
    pha
    lda #$db
    pha
    lda #$62
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$07
    jmp Lffeb
l7b50
	lda #$14
    cmp ram_DC
    bcs l7b6c
    lda #$28
    cmp $db
    bcs l7b6c
    lda #$08
    sta $0c
    lda #$07
    sta $1a
    lda #$0a
    sta $16
    lda #$17
    sta $18
l7b6c
	lda #$7c
    sta $d6
    lda #$18
    sta $d7
    lda #BEIGE
    sta COLUP1
    lda #$04
    sta ram_87
    lda #$35
    sta $05
    lda #$12
    cmp $da
    bcc l7b8e
    lda #$00
    sta $db
    lda #$86
    sta $da
l7b8e
	bit $05
    bmi l7b98
    bit $01
    bvs l7b98
    inc $db
l7b98
	bit $05
    bpl l7ba9
    lda $db
    cmp #$30
    bcs l7ba9
    lda $da
    sec
    sbc #$03
    sta $da
l7ba9
	bit $05
    bpl l7bbd
    lda $db
    cmp #$50
    bcc l7bbd
    lda $da
    sec
    sbc #$02
    sta $da
    jmp l7bce
l7bbd
	bit $05
    bpl l7bce
    lda $db
    cmp #$38
    bcc l7bce
    lda $da
    clc
    adc #$03
    sta $da
l7bce
	jmp Lffdd
    lda $e8
    ora #$10
    sta $e8
    lda #GREEN_YELLOW
    sta COLUP1
    lda #$02
    sta ram_87
    lda #$26
    sta $05
    lda $db
    cmp #$e6
    bne l7c0e
    lda $d7
    pha
    lda $8f
    sec
    sbc #$04
    tay
    pla
    tsx
    sty $00,x
    sec
    sbc $00,x
    sta $db
    lda $d6
    clc
    adc #$06
    sta $da
    lda #$07
    sta $1a
    lda #$09
    sta $16
    lda #$17
    sta $18
l7c0e
	inc $da
    dec $db
    jmp Lffdd
    lda $e8
    and #$04
    beq l7c1e
    jmp l7dbb
l7c1e
	bit $02
    bvc l7c78
    lda #$fa
    sta $89
    lda $e8
    ora #$02
    sta $e8
    lda #$18
    sta $d7
    lda #$7d
    sta $d6
    lda #$00
    sta $ed
    lda $e3
    ora #$01
    sta $e3
    sta $ee
    lda #$7c
    pha
    lda #$55
    pha
    lda #$dd
    pha
    lda #$90
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$07
    jmp Lffeb
    ldx #$01
    lda #$1f
    sta $9e
    ldy #$07
    lda #$1a
    sta $ee
    lda #$7c
    pha
    lda #$77
    pha
    lda #$f2
    pha
    lda #$e8
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
l7c78
	lda $e8
    and #$02
    bne l7c81
    jmp l7cf7
l7c81
	lda $d7
    cmp #$40
    bcs l7c89
    inc $d7
l7c89
	lda #RED|$4
    sta COLUP1
    lda #$05
    sta $05
    bit $07
    bmi l7c98
    jmp l7dbb
l7c98
	lda $eb
    ora #$02
    sta $eb
    lda #$00
    sta ram_DC
    lda #$fa
    sta $d7
    lda $e8
    and #$fb
    sta $e8
    lda #$00
    sta $ed
    lda $f3
    cmp #$a0
    bcs l7cc0
    lda $f3
    clc
    adc #$40
    sta $f3
    jmp l7dbb
l7cc0
	lda $f3
    cmp #$c0
    bcs l7cdf
    lda $f3
    clc
    adc #$20
    sta $f3
    sed
    clc
    lda $94
    adc #$01
    sta $94
    lda $93
    adc #$00
    sta $93
    cld
    jmp l7dbb
l7cdf
	lda #$c0
    cmp $f3
    bcs l7cf4
    sed
    clc
    lda $94
    adc #$02
    sta $94
    lda $93
    adc #$00
    sta $93
    cld
l7cf4
	jmp l7dbb
l7cf7
	lda #$06
    sta $89
    lda #$22
    sta $84
    lda #$08
    sta $92
    lda $e8
    ora #$10
    sta $e8
    lda #GREEN_YELLOW
    sta COLUP1
    lda #$20
    sta $05
    lda ram_DC
    cmp #$00
    bne l7d23
    ldx #$84
    stx $8c
    lda #$f7
    sta $8d
    lda #$06
    sta $8f
l7d23
	lda ram_DC
    cmp #$0a
    bne l7d2c
    jmp l7d32
l7d2c
	lda ram_DC
    cmp #$1e
    bne l7d3e
l7d32
	ldx #$8b
    stx $8c
    lda #$f7
    sta $8d
    lda #$06
    sta $8f
l7d3e
	lda ram_DC
    cmp #$14
    bne l7d50
    ldx #$92
    stx $8c
    lda #$f7
    sta $8d
    lda #$06
    sta $8f
l7d50
	lda #$14
    cmp ram_DC
    bcs l7d5a
    lda #$08
    sta $0c
l7d5a
	lda #$1c
    cmp ram_DC
    bcs l7d64
    lda #$08
    sta $0c
l7d64
	bit $03
    bmi l7d6d
    inc $d7
    jmp l7d8c
l7d6d
	lda $e8
    lsr
    bcc l7d7c
    lda $d6
    clc
    adc #$02
    sta $d6
    jmp l7d8c
l7d7c
	lda $e8
    lsr
    bcs l7d8c
    lda $d6
    sec
    sbc #$02
    sta $d6
    lda #$08
    sta $0c
l7d8c
	lda #$28
    cmp $d6
    bcc l7d98
    lda $e8
    ora #$01
    sta $e8
l7d98
	lda $d6
    cmp #$74
    bcc l7da1
    jmp l7da7
l7da1
	lda #$48
    cmp $d7
    bcs l7dad
l7da7
	lda $e8
    and #$fe
    sta $e8
l7dad
	lda $d6
    cmp #$15
    bcs l7dbb
    lda #$00
    sta $d7
    lda #$74
    sta $d6
l7dbb
	lda $ed
    cmp #$07
    bcc l7dc4
    jmp l7dff
l7dc4
	lda $ed
    cmp #$07
    bcs l7dd1
    ldx $ed
    lda $7e05,x
    sta $9c
l7dd1
	lda #$06
    sta $1a
    lda #$0d
    sta $16
    lda $9c
    sta $18
    lda ram_DC
    cmp #$00
    bne l7de6
    jmp l7dec
l7de6
	lda ram_DC
    cmp #$0a
    bne l7dee
l7dec
	inc $ed
l7dee
	lda ram_DC
    cmp #$14
    bne l7df7
    jmp l7dfd
l7df7
	lda ram_DC
    cmp #$1e
    bne l7dff
l7dfd
	inc $ed
l7dff
	jmp Lffdd
    jmp $7e0b                         ; never reached?

L7e05
    .byte   $19,$19,$19,$10,$19,$16,$ff     ; $7e05 (D)

  IF PLUSROM = 1
    ORG     $3fd0, $ff
    RORG    $7fd0
  ELSE
    ORG     $3fd4, $ff
    RORG    $7fd4
  ENDIF

Start_b3
    ldx #$ff
    txs
    lda #$f2
    pha
    lda #$4f
    pha
    
L7fdd
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    tsx                             ;2        
    lda     CXM0FB,x                ;4        
    rol                             ;2        
    rol                             ;2        
    rol                             ;2        
    rol                             ;2        
    and     #$07                    ;2        
    tax                             ;2        
    inx                             ;2   =  28
L7feb
    lda     $1ff3,x                 ;4        
    pla                             ;4        
    tax                             ;2        
    pla                             ;4        
    rts                             ;6   =  20
    
    ORG     $3ff3, $ff
    RORG    $7ff3

L7ff3
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff     ; $7ff3 (*)

  IF PLUSROM = 1
    .word (PlusROM_API - $6000)             ; PlusRom API pointer
  ELSE
    .byte   $ff,$ff                         ; $7ffa (D)
  ENDIF

    .word Start_b3, Start_b3


;***********************************************************
;      Bank 4 / 0..7
;***********************************************************

    SEG     CODE
    ORG     $4000
    RORG    $9000

    lda     ram_E8                  ;3         *
    and     #$02                    ;2         *
    bne     L9009                   ;2/3       *
    jmp     L909e                   ;3   =  10 *
    
L9009
    lda     #$30                    ;2         *
    sta     ram_92                  ;3         *
    lda     #$50                    ;2         *
    sta     ram_84                  ;3         *
    lda     ram_E3                  ;3         *
    ora     #$80                    ;2         *
    sta     ram_E3                  ;3         *
    lda     #$15                    ;2         *
    sta     CTRLPF                  ;3         *
    lda     ram_D5                  ;3         *
    cmp     #$08                    ;2         *
    bcs     L9029                   ;2/3       *
    lda     ram_E3                  ;3         *
    ora     #$08                    ;2         *
    sta     ram_E3                  ;3         *
    inc     ram_E5                  ;5   =  43 *
L9029
    lda     ram_89                  ;3         *
    cmp     #$28                    ;2         *
    bcs     L903d                   ;2/3       *
    inc     ram_89                  ;5         *
    lda     #$07                    ;2         *
    sta     AUDV1                   ;3         *
    lda     #$17                    ;2         *
    sta     AUDF1                   ;3         *
    lda     #$0c                    ;2         *
    sta     AUDC1                   ;3   =  27 *
L903d
    bit     CXP0FB                  ;3         *
    bvc     L9053                   ;2/3       *
    lda     ram_E0                  ;3         *
    ora     #$01                    ;2         *
    sta     ram_E0                  ;3         *
    lda     ram_E0                  ;3         *
    and     #$f7                    ;2         *
    sta     ram_E0                  ;3         *
    lda     ram_E0                  ;3         *
    and     #$fd                    ;2         *
    sta     ram_E0                  ;3   =  29 *
L9053
    bit     CXP0FB                  ;3         *
    bvs     L9060                   ;2/3       *
    lda     ram_E0                  ;3         *
    and     #$fe                    ;2         *
    sta     ram_E0                  ;3         *
    jmp     L909e                   ;3   =  16 *
    
L9060
    sta     ram_EE                  ;3         *
    lda     #$90                    ;2         *
    pha                             ;3         *
    lda     #$77                    ;2         *
    pha                             ;3         *
    lda     #$da                    ;2         *
    pha                             ;3         *
    lda     #$0b                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$07                    ;2         *
    jmp     Lffeb                   ;3   =  39 *
    
    lda $e0
    ora #$08
    sta $e0
    bit $02
    bpl l908c
    lda $d5
    clc
    adc #$02
    sta $d5
    jmp L909e
l908c
    lda #$10
    bit $0280
    bne l9095
    dec $d5
l9095
    lda #$20
    bit $0280
    bne L909e
    inc $d5
    
L909e
    lda     ram_E8                  ;3         *
    and     #$02                    ;2         *
    beq     L90a7                   ;2/3       *
    jmp     L91b0                   ;3   =  10 *
    
L90a7
    lda     #$01                    ;2         *
    sta     ram_87                  ;3         *
    lda     #$10                    ;2         *
    sta     NUSIZ1                  ;3         *
    lda     ram_E7                  ;3         *
    cmp     #$00                    ;2         *
    bne     L90c6                   ;2/3       *
    lda     #$fa                    ;2         *
    sta     ram_DB                  ;3         *
    lda     ram_E8                  ;3         *
    ora     #$02                    ;2         *
    sta     ram_E8                  ;3         *
    lda     #$00                    ;2         *
    sta     ram_89                  ;3         *
    jmp     L91b0                   ;3   =  38 *
    
L90c6
    lda     #GREEN_YELLOW           ;2         *
    sta     COLUP1                  ;3         *
    lda     ram_D7                  ;3         *
    sec                             ;2         *
    sbc     #$04                    ;2         *
    sta     ram_DB                  ;3         *
    lda     ram_D7                  ;3         *
    cmp     #$00                    ;2         *
    bne     L90ef                   ;2/3       *
    sta     ram_EE                  ;3         *
    lda     #$90                    ;2         *
    pha                             ;3         *
    lda     #$ee                    ;2         *
    pha                             ;3         *
    lda     #$db                    ;2         *
    pha                             ;3         *
    lda     #$ad                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$07                    ;2         *
    jmp     Lffeb                   ;3   =  61 *
    
L90ef
    lda     ram_D6                  ;3         *
    cmp     ram_D4                  ;3         *
    bcs     L90f9                   ;2/3       *
    lda     #$08                    ;2         *
    sta     REFP1                   ;3   =  13 *
L90f9
    lda     ram_D6                  ;3         *
    clc                             ;2         *
    adc     #$04                    ;2         *
    pha                             ;3         *
    tsx                             ;2         *
    pla                             ;4         *
    lda     ram_DA                  ;3         *
    cmp     CXM1P,x                 ;4         *
    bne     L9113                   ;2/3       *
    lda     ram_D6                  ;3         *
    cmp     ram_D4                  ;3         *
    bcs     L9113                   ;2/3       *
    lda     ram_E0                  ;3         *
    and     #$bf                    ;2         *
    sta     ram_E0                  ;3   =  41 *
L9113
    lda     ram_D6                  ;3         *
    clc                             ;2         *
    adc     #$04                    ;2         *
    pha                             ;3         *
    tsx                             ;2         *
    pla                             ;4         *
    lda     ram_DA                  ;3         *
    cmp     CXM1P,x                 ;4         *
    bne     L912d                   ;2/3       *
    lda     ram_D4                  ;3         *
    cmp     ram_D6                  ;3         *
    bcs     L912d                   ;2/3       *
    lda     ram_E0                  ;3         *
    ora     #$40                    ;2         *
    sta     ram_E0                  ;3   =  41 *
L912d
    lda     ram_D7                  ;3         *
    cmp     #$00                    ;2         *
    beq     L9136                   ;2/3       *
    jmp     L9168                   ;3   =  10 *
    
L9136
    lda     ram_E7                  ;3         *
    cmp     #$05                    ;2         *
    bne     L9140                   ;2/3       *
    lda     #$15                    ;2         *
    sta     ram_D6                  ;3   =  12 *
L9140
    lda     ram_E7                  ;3         *
    cmp     #$04                    ;2         *
    bne     L914a                   ;2/3       *
    lda     #$80                    ;2         *
    sta     ram_D6                  ;3   =  12 *
L914a
    lda     ram_E7                  ;3         *
    cmp     #$03                    ;2         *
    bne     L9154                   ;2/3       *
    lda     #$40                    ;2         *
    sta     ram_D6                  ;3   =  12 *
L9154
    lda     ram_E7                  ;3         *
    cmp     #$02                    ;2         *
    bne     L915e                   ;2/3       *
    lda     #$80                    ;2         *
    sta     ram_D6                  ;3   =  12 *
L915e
    lda     ram_E7                  ;3         *
    cmp     #$01                    ;2         *
    bne     L9168                   ;2/3       *
    lda     #$38                    ;2         *
    sta     ram_D6                  ;3   =  12 *
L9168
    bit     CXP1FB                  ;3         *
    bmi     L9178                   ;2/3       *
    inc     ram_D7                  ;5         *
    lda     ram_D6                  ;3         *
    clc                             ;2         *
    adc     #$04                    ;2         *
    sta     ram_DA                  ;3         *
    jmp     L91b0                   ;3   =  23 *
    
L9178
    lda     #$8c                    ;2         *
    cmp     ram_DA                  ;3         *
    bcs     L9181                   ;2/3       *
    jmp     L9187                   ;3   =  10 *
    
L9181
    lda     ram_DA                  ;3         *
    cmp     #$15                    ;2         *
    bcs     L919d                   ;2/3 =   7 *
L9187
    lda     ram_D6                  ;3         *
    clc                             ;2         *
    adc     #$04                    ;2         *
    sta     ram_DA                  ;3         *
    lda     #$07                    ;2         *
    sta     AUDV1                   ;3         *
    lda     #$1f                    ;2         *
    sta     AUDF1                   ;3         *
    lda     #$0d                    ;2         *
    sta     AUDC1                   ;3         *
    jmp     L91b0                   ;3   =  28 *
    
L919d
    bit     ram_E0                  ;3         *
    bvc     L91a6                   ;2/3       *
    dec     ram_DA                  ;5         *
    jmp     L91b0                   ;3   =  13 *
    
L91a6
    bit     ram_E0                  ;3         *
    bvs     L91b0                   ;2/3       *
    inc     ram_DA                  ;5         *
    lda     #$08                    ;2         *
    sta     REFP1                   ;3   =  15 *
L91b0
    jmp     Lffdd                   ;3   =   3 *
    
    lda $e8
    ora #$40
    sta $e8
    lda #ORANGE
    sta COLUP1
    lda #$25
    sta $05
    lda $d6
    cmp #$15
    bne l91cb
    lda #$08
    sta $0c
l91cb
	inc $e6
    lda #$0f
    cmp $e6
    bcs l91d7
    lda #$00
    sta $e6
l91d7
	lda $e7
    cmp #$01
    beq l91e0
    jmp l9296
l91e0
	lda $e3
    ora #$01
    sta $e3
    lda #$fa
    sta $db
    lda $ed
    cmp #$07
    bcc l91f3
    jmp l922e
l91f3
	lda $ed
    cmp #$07
    bcs l9200
    ldx $ed
    lda $9cde,x
    sta $9c
l9200
	lda #$06
    sta $1a
    lda #$0d
    sta $16
    lda $9c
    sta $18
    lda ram_DC
    cmp #$00
    bne l9215
    jmp l921b
l9215
	lda ram_DC
    cmp #$0a
    bne l921d
l921b
	inc $ed
l921d
	lda ram_DC
    cmp #$14
    bne l9226
    jmp l922c
l9226
	lda ram_DC
    cmp #$1e
    bne l922e
l922c
	inc $ed
l922e
	lda $e6
    cmp #$00
    beq l9237
    jmp l9296
l9237
	lda #$00
    cmp $8f
    bcs l9256
    dec $8f
    lda #RED
    sta COLUP1
    lda #$06
    sta $1a
    lda #$6e
    sta $16
    lda #$07
    sta $18
    lda #$00
    sta $e6
    jmp l9453
l9256
	lda #$00
    cmp $8f
    bcc l9296
    lda $d7
    cmp #$fa
    beq l9296
    lda #$fa
    sta $d7
    ldx #$01
    ldy #$06
    lda #$1f
    sta $ee
    lda #$92
    pha
    lda #$83
    pha
    lda #$f2
    pha
    lda #$b8
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    sed
    clc
    lda $94
    adc #$05
    sta $94
    lda $93
    adc #$00
    sta $93
    cld
    jmp l9453
l9296
	lda #$01
    cmp $e7
    bcc l929f
    jmp l9453
l929f
	lda #$00
    cmp $ea
    bcs l92a8
    jmp l92c3
l92a8
	lda $8f
    cmp #$0f
    bcs l92b3
    inc $8f
    jmp l9453
l92b3
	lda $8f
    cmp #$0f
    bcc l92c3
    lda $e8
    and #$ef
    sta $e8
    lda #$01
    sta $ea
l92c3
	lda $ea
    cmp #$03
    beq l92d0
    lda #$fa
    sta $db
    jmp l9321
l92d0
	lda #$03
    cmp ram_87
    bcs l92d8
    dec ram_87
l92d8
	lda $e2
    cmp #$00
    beq l92e1
    jmp l9301
l92e1
	lda #$8c
    cmp $da
    bcs l92ea
    jmp l92f0
l92ea
	lda $da
    cmp #$15
    bcs l9301
l92f0
	lda $d7
    sec
    sbc #$07
    sta $db
    lda $d6
    clc
    adc #$04
    sta $da
    jmp l9453
l9301
	lda $d6
    cmp #$15
    bne l9311
    lda $da
    clc
    adc #$02
    sta $da
    jmp l9453
l9311
	lda $d6
    cmp #$7c
    bne l9321
    lda $da
    sec
    sbc #$02
    sta $da
    jmp l9453
l9321
	lda #$01
    cmp $ea
    bcs l932a
    jmp l9387
l932a
	lda #$48
    sta $db
    lda #$07
    sta $1a
    lda #$06
    sta $16
    lda #$0a
    clc
    adc ram_87
    sta $18
    lda $e8
    lsr
    bcs l9346
    lda #$64
    sta $da
l9346
	lda $e8
    lsr
    bcc l934f
    lda #$38
    sta $da
l934f
	lda $e8
    and #$02
    bne l9357
    inc ram_87
l9357
	lda $e8
    and #$02
    beq l935f
    dec ram_87
l935f
	lda #$02
    cmp ram_87
    bcc l936e
    lda $e8
    and #$fd
    sta $e8
    jmp l9453
l936e
	lda ram_87
    cmp #$28
    bcc l9384
    lda $e8
    ora #$02
    sta $e8
    lda $ea
    clc
    adc #$02
    sta $ea
    jmp l9453
l9384
	jmp l9453
l9387
	lda $ea
    cmp #$07
    bne l9395
    lda #$00
    sta ram_DC
    lda #$08
    sta $ea
l9395
	lda $ea
    cmp #$08
    bcs l939e
    jmp l9411
l939e
	lda $e8
    ora #$10
    sta $e8
    lda #$07
    sta $1a
    lda #$0e
    sta $16
    lda $d6
    sta $18
    lda ram_DC
    cmp #$00
    bne l93c2
    ldx #$99
    stx $8c
    lda #$f7
    sta $8d
    lda #$09
    sta $8f
l93c2
	lda ram_DC
    cmp #$0a
    bne l93cb
    jmp l93d1
l93cb
	lda ram_DC
    cmp #$1e
    bne l93dd
l93d1
	ldx #$a3
    stx $8c
    lda #$f7
    sta $8d
    lda #$09
    sta $8f
l93dd
	lda ram_DC
    cmp #$14
    bne l93ef
    ldx #$ad
    stx $8c
    lda #$f7
    sta $8d
    lda #$09
    sta $8f
l93ef
	lda #$14
    cmp ram_DC
    bcs l93f9
    lda #$08
    sta $0c
l93f9
	lda $e8
    lsr
    bcc l9405
    lda $d6
    clc
    adc #$02
    sta $d6
l9405
	lda $e8
    lsr
    bcs l9411
    lda $d6
    sec
    sbc #$02
    sta $d6
l9411
	lda #$15
    cmp $d6
    bcs l941d
    lda $d6
    cmp #$7c
    bcc l9453
l941d
	lda #$00
    sta $ea
    lda $d6
    cmp #$15
    bcs l9431
    lda #$15
    sta $d6
    lda $e8
    ora #$01
    sta $e8
l9431
	lda #$7c
    cmp $d6
    bcs l9441
    lda #$7c
    sta $d6
    lda $e8
    and #$fe
    sta $e8
l9441
	lda $e8
    and #$ef
    sta $e8
    ldx #$b7
    stx $8c
    lda #$f7
    sta $8d
    lda #$0f
    sta $8f
l9453
	bit $07
    bpl l9472
    lda $d5
    sec
    sbc #$08
    sta $d5
    lda #RED|$2
    sta COLUP1
    lda #$07
    sta $1a
    lda #$0a
    sta $16
    lda #$11
    sta $18
    lda #$07
    sta $ea
l9472
	lda $d5
    cmp #$46
    bcc l9482
    bit $03
    bpl l9482
    lda $e0
    ora #$80
    sta $e0
l9482
	jmp Lffdd
    lda $e3
    ora #$80
    sta $e3
    lda #$31
    sta $0a
    lda $d5
    cmp #$40
    bcc l949b
    lda $e0
    ora #$80
    sta $e0
l949b
	lda #ORANGE
    sta COLUP1
    lda $d4
    cmp $d6
    bcs l94ad
    lda #$06
    cmp ram_DC
    bcs l94ad
    dec $d6
l94ad
	lda $d6
    cmp $d4
    bcs l94bb
    lda #$06
    cmp ram_DC
    bcs l94bb
    inc $d6
l94bb
	lda $d6
    cmp #$28
    bcs l94c5
    lda #$28
    sta $d6
l94c5
	lda #$80
    cmp $d6
    bcs l94cf
    lda #$7c
    sta $d6
l94cf
	lda $e3
    ora #$01
    sta $e3
    lda $e8
    ora #$10
    sta $e8
    bit $07
    bpl l94fc
    lda $d7
    sec
    sbc #$14
    sta $d5
    lda $e8
    ora #$01
    sta $e8
    lda $e0
    ora #$01
    sta $e0
    lda $e0
    ora #$08
    sta $e0
    lda $d6
    sta $d4
l94fc
	bit $07
    bmi l9506
    lda $e0
    and #$fe
    sta $e0
l9506
	lda $e8
    lsr
    bcs l950f
    dec $d7
    dec $d6
l950f
	lda $e8
    lsr
    bcc l9518
    inc $d7
    inc $d6
l9518
	lda $d7
    cmp #$57
    bcc l9524
    lda $e8
    and #$fe
    sta $e8
l9524
	lda #$40
    cmp $d7
    bcc l9530
    lda $e8
    ora #$01
    sta $e8
l9530
	jmp Lffdd
    lda #$34
    sta ram_87
    lda #$20
    sta $05
    lda #$03
    sta $92
    lda #CYAN
    sta COLUP1
    lda $84
    sec
    sbc #$06
    pha
    tsx
    pla
    lda $d4
    cmp $01,x
    bcs l955a
    lda $e0
    and #$fe
    sta $e0
    jmp l95bc
l955a
	lda $84
    clc
    adc #$04
    cmp $d4
    bcs l956c
    lda $e0
    and #$fe
    sta $e0
    jmp l95bc
l956c
	bit $02
    bvs l9579
    lda $e0
    and #$fe
    sta $e0
    jmp l95bc
l9579
	bit $02
    bvc l9591
    lda $89
    sec
    sbc $92
    pha
    tsx
    pla
    lda $d5
    cmp $01,x
    bcc l9591
    lda $e0
    ora #$01
    sta $e0
l9591
	bit $02
    bvs l9598
    jmp l95bc
l9598
	lda $e0
    and #$08
    beq l95a1
    jmp l95bc
l95a1
	bit $0280
    bvs l95a9
    jmp l95ae
l95a9
	bit $0280
    bmi l95b1
l95ae
	jmp l95bc
l95b1
	lda $89
    sec
    sbc #$02
    sta $d5
    lda $84
    sta $d4
l95bc
	lda $89
    cmp #$50
    bcc l95d1
    lda $84
    cmp #$7c
    bcs l95d1
    lda #$50
    sta $89
    inc $84
    jmp l9610
l95d1
	lda $84
    cmp #$7c
    bcc l95e6
    lda #$18
    cmp $89
    bcs l95e6
    lda #$7c
    sta $84
    dec $89
    jmp l9610
l95e6
	lda #$18
    cmp $89
    bcc l95fb
    lda #$28
    cmp $84
    bcs l95fb
    lda #$18
    sta $89
    dec $84
    jmp l9610
l95fb
	lda #$28
    cmp $84
    bcc l9610
    lda $89
    cmp #$50
    bcs l9610
    lda #$28
    sta $84
    inc $89
    jmp l9610
l9610
	lda $e8
    and #$02
    beq l9619
    jmp l96c8
l9619
	lda $e7
    cmp #$00
    bne l9665
    ldx #$01
    ldy #$02
    lda #$1f
    sta $ee
    lda #$96
    pha
    lda #$3c
    pha
    lda #$f2
    pha
    lda #$b8
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    ldx #$00
    lda #$1e
    sta $9e
    ldy #$03
    lda #$1d
    sta $ee
    lda #$96
    pha
    lda #$5e
    pha
    lda #$f2
    pha
    lda #$e8
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    lda $e8
    ora #$02
    sta $e8
l9665
	lda ram_DC
    cmp #$00
    bne l966e
    jmp l9674
l966e
	lda ram_DC
    cmp #$14
    bne l968c
l9674
	lda #$05
    sta $1a
    lda #$0d
    sta $16
    lda #$11
    sta $18
    ldx #$c7
    stx $8c
    lda #$f7
    sta $8d
    lda #$07
    sta $8f
l968c
	lda ram_DC
    cmp #$0a
    bne l96aa
    lda #$05
    sta $1a
    lda #$0d
    sta $16
    lda #$11
    sta $18
    ldx #$cf
    stx $8c
    lda #$f7
    sta $8d
    lda #$07
    sta $8f
l96aa
	lda ram_DC
    cmp #$1e
    bne l96c8
    lda #$05
    sta $1a
    lda #$06
    sta $16
    lda #$11
    sta $18
    ldx #$d7
    stx $8c
    lda #$f7
    sta $8d
    lda #$07
    sta $8f
l96c8
	jmp Lffdd
    lda $e8
    and #$02
    beq l96d4
    jmp l97c9
l96d4
	lda $e7
    cmp #$00
    bne l96fe
    ldx #$01
    ldy #$08
    lda #$1f
    sta $ee
    lda #$96
    pha
    lda #$f7
    pha
    lda #$f2
    pha
    lda #$b8
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    lda $e8
    ora #$02
    sta $e8
l96fe
	lda #GREEN_YELLOW|$2
    sta COLUP1
    lda $d7
    cmp #$fa
    bne l9724
    sta $ee
    lda #$97
    pha
    lda #$1f
    pha
    lda #$f5
    pha
    lda #$e3
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    lda #$00
    sta $d7
l9724
	lda $d7
    cmp #$00
    bne l973e
    lda $a2
    lsr
    bcc l9731
    eor #$b4
l9731
	sta $a2
    and #$3f
    clc
    adc #$26
    sta $d6
    lda #$01
    sta $d7
l973e
	lda #$4e
    cmp $d7
    bcs l9766
    lda $d7
    cmp #$50
    bcs l9766
    lda #$00
    sta ram_DC
    lda #$07
    sta $1a
    lda #$03
    sta $16
    lda #$1f
    sta $18
    ldx #$df
    stx $8c
    lda #$f7
    sta $8d
    lda #$08
    sta $8f
l9766
	bit $03
    bmi l9775
    inc $d7
    lda $e8
    ora #$10
    sta $e8
    jmp l97c9
l9775
	lda $e8
    and #$ef
    sta $e8
    lda ram_DC
    cmp #$00
    bne l978d
    ldx #$e8
    stx $8c
    lda #$f7
    sta $8d
    lda #$08
    sta $8f
l978d
	lda ram_DC
    cmp #$14
    bne l979f
    ldx #$f1
    stx $8c
    lda #$f7
    sta $8d
    lda #$07
    sta $8f
l979f
	lda $e8
    lsr
    bcc l97aa
    dec $d6
    lda #$08
    sta $0c
l97aa
	lda $e8
    lsr
    bcs l97b1
    inc $d6
l97b1
	lda $d6
    cmp #$78
    bcc l97bd
    lda $e8
    ora #$01
    sta $e8
l97bd
	lda #$25
    cmp $d6
    bcc l97c9
    lda $e8
    and #$fe
    sta $e8
l97c9
	jmp Lffdd
    lda $e8
    ora #$10
    sta $e8
    lda #$12
    sta $05
    lda #RED|$4
    sta COLUP1
    lda #$5c
    sta $da
    lda #$10
    sta ram_87
    lda $e8
    and #$04
    beq l97f0
    lda #$23
    cmp ram_DC
    bcs l97f0
    dec $db
l97f0
	lda $e8
    and #$04
    bne l97f8
    inc $db
l97f8
	lda $db
    cmp #$10
    bne l9804
    lda $e8
    ora #$04
    sta $e8
l9804
	lda $db
    cmp #$00
    bne l9810
    lda $e8
    and #$fb
    sta $e8
l9810
	lda $e8
    and #$02
    beq l9822
    lda #$09
    cmp ram_DC
    bcs l9822
    dec $d7
    lda #CYAN
    sta COLUP1
l9822
	lda $e8
    and #$02
    bne l9834
    lda #$0a
    cmp ram_DC
    bcs l9834
    inc $d7
    lda #CYAN
    sta COLUP1
l9834
	lda $d7
    cmp #$50
    bcc l9843
    lda $e8
    ora #$02
    sta $e8
    jmp L9852
l9843
	lda #$3e
    cmp $d7
    bcc L9852
    lda $e8
    and #$fd
    sta $e8
    jmp L9852

L9852
    jmp Lffdd
    
L9855
    lda     #$35                    ;2        
    sta     CTRLPF                  ;3        
    lda     #BLACK|$6               ;2        
    sta     COLUP1                  ;3        
    lda     #$00                    ;2        
    sta     ram_E1                  ;3        
    lda     ram_E0                  ;3        
    and     #$fd                    ;2        
    sta     ram_E0                  ;3        
    sta     ram_EE                  ;3        
    lda     #$98                    ;2        
    pha                             ;3        
    lda     #$7e                    ;2        
    pha                             ;3        
    lda     #$db                    ;2        
    pha                             ;3        
    lda     #$53                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  62
    
    lda     ram_E5                  ;3        
    cmp     #$26                    ;2        
    bne     L988b                   ;2/3      
    lda     ram_E8                  ;3         *
    and     #$fd                    ;2         *
    sta     ram_E8                  ;3   =  15 *
L988b
    lda     ram_E5                  ;3        
    cmp     #$2c                    ;2        
    bne     L9897                   ;2/3      
    lda     ram_E8                  ;3         *
    ora     #$02                    ;2         *
    sta     ram_E8                  ;3   =  15 *
L9897
    lda     #$00                    ;2        
    sta     ram_E2                  ;3        
    lda     ram_E5                  ;3        
    cmp     #$00                    ;2        
    bne     L98a4                   ;2/3      
    jmp     L98aa                   ;3   =  15
    
L98a4
    lda     ram_E5                  ;3        
    cmp     #$17                    ;2        
    bne     L98b5                   ;2/3 =   7
L98aa
    lda     #$80                    ;2        
    sta     ram_D4                  ;3        
    lda     #$4d                    ;2        
    sta     ram_D5                  ;3        
    jmp     L9cd2                   ;3   =  13
    
L98b5
    lda     ram_E5                  ;3        
    cmp     #$01                    ;2        
    bne     L98c6                   ;2/3      
    lda     #$1c                    ;2        
    sta     ram_D4                  ;3        
    lda     #$4d                    ;2        
    sta     ram_D5                  ;3        
    jmp     L9cd2                   ;3   =  20
    
L98c6
    lda     ram_E5                  ;3        
    cmp     #$02                    ;2        
    bne     L98d7                   ;2/3      
    lda     #$80                    ;2        
    sta     ram_D4                  ;3        
    lda     #$4d                    ;2        
    sta     ram_D5                  ;3        
    jmp     L9cd2                   ;3   =  20
    
L98d7
    lda     ram_E5                  ;3        
    cmp     #$03                    ;2        
    bne     L98e8                   ;2/3      
    lda     #$15                    ;2        
    sta     ram_D4                  ;3        
    lda     #$1e                    ;2        
    sta     ram_D5                  ;3        
    jmp     L9cd2                   ;3   =  20
    
L98e8
    lda     ram_E5                  ;3        
    cmp     #$04                    ;2        
    bne     L98ff                   ;2/3      
    lda     ram_E8                  ;3        
    and     #$02                    ;2        
    beq     L98ff                   ;2/3      
    lda     #$1c                    ;2         *
    sta     ram_D4                  ;3         *
    lda     #$1e                    ;2         *
    sta     ram_D5                  ;3         *
    jmp     L9cd2                   ;3   =  27 *
    
L98ff
    lda     ram_E5                  ;3        
    cmp     #$04                    ;2        
    bne     L9918                   ;2/3      
    lda     #$1c                    ;2        
    sta     ram_D4                  ;3        
    lda     #$1e                    ;2        
    sta     ram_D5                  ;3        
    lda     #$64                    ;2        
    sta     ram_D6                  ;3        
    lda     #$0c                    ;2        
    sta     ram_D7                  ;3        
    jmp     L9cd2                   ;3   =  30
    
L9918
    lda     ram_E5                  ;3        
    cmp     #$05                    ;2        
    bne     L9929                   ;2/3      
    lda     #$15                    ;2        
    sta     ram_D4                  ;3        
    lda     #$4e                    ;2        
    sta     ram_D5                  ;3        
    jmp     L9cd2                   ;3   =  20
    
L9929
    lda     ram_E5                  ;3        
    cmp     #$06                    ;2        
    bne     L9958                   ;2/3      
    lda     #$15                    ;2        
    sta     ram_D4                  ;3        
    lda     #$4e                    ;2        
    sta     ram_D5                  ;3        
    ldx     #$01                    ;2        
    ldy     #$09                    ;2        
    lda     #$1f                    ;2        
    sta     ram_EE                  ;3        
    lda     #$99                    ;2        
    pha                             ;3        
    lda     #$54                    ;2        
    pha                             ;3        
    lda     #$f2                    ;2        
    pha                             ;3        
    lda     #$b8                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  62
    
    jmp     L9cd2                   ;3   =   3
    
L9958
    lda     ram_E5                  ;3        
    cmp     #$07                    ;2        
    bne     L9961                   ;2/3      
    jmp     L9967                   ;3   =  10
    
L9961
    lda     ram_E5                  ;3        
    cmp     #$0c                    ;2        
    bne     L9976                   ;2/3 =   7
L9967
    lda     #$15                    ;2        
    sta     ram_D4                  ;3        
    lda     #$4e                    ;2        
    sta     ram_D5                  ;3        
    lda     #$15                    ;2        
    sta     CTRLPF                  ;3        
    jmp     L9cd2                   ;3   =  18
    
L9976
    lda     ram_E5                  ;3        
    cmp     #$08                    ;2        
    bne     L997f                   ;2/3      
    jmp     L9985                   ;3   =  10
    
L997f
    lda     ram_E5                  ;3        
    cmp     #$0b                    ;2        
    bne     L9990                   ;2/3 =   7
L9985
    lda     #$15                    ;2        
    sta     ram_D4                  ;3        
    lda     #$4e                    ;2        
    sta     ram_D5                  ;3        
    jmp     L9cd2                   ;3   =  13
    
L9990
    lda     ram_E5                  ;3        
    cmp     #$09                    ;2        
    bne     L99a5                   ;2/3      
    lda     #$15                    ;2        
    sta     ram_D4                  ;3        
    lda     #$1e                    ;2        
    sta     ram_D5                  ;3        
    lda     #$3c                    ;2        
    sta     ram_D7                  ;3        
    jmp     L9cd2                   ;3   =  25
    
L99a5
    lda     ram_E5                  ;3        
    cmp     #$0a                    ;2        
    bne     L99b6                   ;2/3      
    lda     #$15                    ;2        
    sta     ram_D4                  ;3        
    lda     #$1e                    ;2        
    sta     ram_D5                  ;3        
    jmp     L9cd2                   ;3   =  20
    
L99b6
    lda     ram_E5                  ;3        
    cmp     #$0d                    ;2        
    bne     L99c7                   ;2/3      
    lda     #$15                    ;2        
    sta     ram_D4                  ;3        
    lda     #$30                    ;2        
    sta     ram_D5                  ;3        
    jmp     L9cd2                   ;3   =  20
    
L99c7
    lda     ram_E5                  ;3        
    cmp     #$0e                    ;2        
    bne     L99d0                   ;2/3      
    jmp     L99d6                   ;3   =  10
    
L99d0
    lda     ram_E5                  ;3        
    cmp     #$25                    ;2        
    bne     L99e1                   ;2/3 =   7
L99d6
    lda     #$4b                    ;2        
    sta     ram_D4                  ;3        
    lda     #$4e                    ;2        
    sta     ram_D5                  ;3        
    jmp     L9cd2                   ;3   =  13
    
L99e1
    lda     ram_E5                  ;3        
    cmp     #$0f                    ;2        
    bne     L99f6                   ;2/3      
    lda     #$82                    ;2        
    sta     ram_D4                  ;3        
    lda     #$1e                    ;2        
    sta     ram_D5                  ;3        
    lda     #$3c                    ;2        
    sta     ram_D7                  ;3        
    jmp     L9cd2                   ;3   =  25
    
L99f6
    lda     ram_E5                  ;3        
    cmp     #$15                    ;2        
    bne     L9a0b                   ;2/3!     
    lda     #$16                    ;2        
    sta     ram_D4                  ;3        
    lda     #$1e                    ;2        
    sta     ram_D5                  ;3        
    lda     #$3c                    ;2        
    sta     ram_D7                  ;3        
    jmp     L9cd2                   ;3   =  25
    
L9a0b
    lda     #$0f                    ;2        
    cmp     ram_E5                  ;3        
    bcs     L9a22                   ;2/3      
    lda     ram_E5                  ;3        
    cmp     #$12                    ;2        
    bcs     L9a22                   ;2/3      
    lda     #$82                    ;2        
    sta     ram_D4                  ;3        
    lda     #$1e                    ;2        
    sta     ram_D5                  ;3        
    jmp     L9cd2                   ;3   =  27
    
L9a22
    lda     ram_E5                  ;3        
    cmp     #$12                    ;2        
    bne     L9a5b                   ;2/3      
    lda     ram_EB                  ;3        
    and     #$02                    ;2        
    beq     L9a5b                   ;2/3      
    lda     #$1c                    ;2        
    sta     ram_D4                  ;3        
    lda     #$08                    ;2        
    sta     ram_D5                  ;3        
    ldx     #$01                    ;2        
    lda     #$1f                    ;2        
    sta     ram_9E                  ;3        
    ldy     #$03                    ;2        
    lda     #$1e                    ;2        
    sta     ram_EE                  ;3        
    lda     #$9a                    ;2        
    pha                             ;3        
    lda     #$57                    ;2        
    pha                             ;3        
    lda     #$f2                    ;2        
    pha                             ;3        
    lda     #$e8                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  74
    
    jmp     L9cd2                   ;3   =   3
    
L9a5b
    lda     ram_E5                  ;3        
    cmp     #$12                    ;2        
    bne     L9a6c                   ;2/3      
    lda     #$82                    ;2        
    sta     ram_D4                  ;3        
    lda     #$1e                    ;2        
    sta     ram_D5                  ;3        
    jmp     L9cd2                   ;3   =  20
    
L9a6c
    lda     #$12                    ;2        
    cmp     ram_E5                  ;3        
    bcs     L9a83                   ;2/3      
    lda     ram_E5                  ;3        
    cmp     #$15                    ;2        
    bcs     L9a83                   ;2/3      
    lda     #$15                    ;2        
    sta     ram_D4                  ;3        
    lda     #$1e                    ;2        
    sta     ram_D5                  ;3        
    jmp     L9cd2                   ;3   =  27
    
L9a83
    lda     ram_E5                  ;3        
    cmp     #$16                    ;2        
    bne     L9a8c                   ;2/3      
    jmp     L9a92                   ;3   =  10
    
L9a8c
    lda     ram_E5                  ;3        
    cmp     #$25                    ;2        
    bne     L9ac1                   ;2/3 =   7
L9a92
    lda     #$15                    ;2        
    sta     ram_D4                  ;3        
    ldx     #$00                    ;2        
    ldy     #$02                    ;2        
    lda     #$00                    ;2        
    sta     ram_EE                  ;3        
    lda     #$9a                    ;2        
    pha                             ;3        
    lda     #$b3                    ;2        
    pha                             ;3        
    lda     #$f2                    ;2        
    pha                             ;3        
    lda     #$b8                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  50
    
    lda     #$1e                    ;2        
    sta     ram_D5                  ;3        
    lda     ram_EB                  ;3        
    and     #$fd                    ;2        
    sta     ram_EB                  ;3        
    jmp     L9cd2                   ;3   =  16
    
L9ac1
    lda     ram_E5                  ;3        
    cmp     #$18                    ;2        
    bne     L9ad6                   ;2/3      
    lda     #$80                    ;2        
    sta     ram_D4                  ;3        
    lda     #$4d                    ;2        
    sta     ram_D5                  ;3        
    lda     #$25                    ;2        
    sta     CTRLPF                  ;3        
    jmp     L9cd2                   ;3   =  25
    
L9ad6
    lda     ram_E5                  ;3        
    cmp     #$19                    ;2        
    bne     L9adf                   ;2/3      
    jmp     L9ae5                   ;3   =  10
    
L9adf
    lda     ram_E5                  ;3        
    cmp     #$1a                    ;2        
    bne     L9af0                   ;2/3 =   7
L9ae5
    lda     #$16                    ;2        
    sta     ram_D4                  ;3        
    lda     #$18                    ;2        
    sta     ram_D5                  ;3        
    jmp     L9cd2                   ;3   =  13
    
L9af0
    lda     ram_E5                  ;3        
    cmp     #$1b                    ;2        
    bne     L9af9                   ;2/3      
    jmp     L9aff                   ;3   =  10 *
    
L9af9
    lda     ram_E5                  ;3        
    cmp     #$1d                    ;2        
    bne     L9b0a                   ;2/3!=   7
L9aff
    lda     #$16                    ;2         *
    sta     ram_D4                  ;3         *
    lda     #$18                    ;2         *
    sta     ram_D5                  ;3         *
    jmp     L9cd2                   ;3   =  13 *
    
L9b0a
    lda     ram_E5                  ;3        
    cmp     #$1c                    ;2        
    bne     L9b23                   ;2/3      
    lda     #$15                    ;2         *
    sta     ram_D4                  ;3         *
    lda     #$4e                    ;2         *
    sta     ram_D5                  ;3         *
    lda     #$c8                    ;2         *
    sta     ram_DB                  ;3         *
    lda     #$86                    ;2         *
    sta     ram_DA                  ;3         *
    jmp     L9cd2                   ;3   =  30 *
    
L9b23
    lda     ram_E5                  ;3        
    cmp     #$1e                    ;2        
    bne     L9b34                   ;2/3      
    lda     #$15                    ;2         *
    sta     ram_D4                  ;3         *
    lda     #$18                    ;2         *
    sta     ram_D5                  ;3         *
    jmp     L9cd2                   ;3   =  20 *
    
L9b34
    lda     ram_E5                  ;3        
    cmp     #$1f                    ;2        
    bne     L9b4f                   ;2/3      
    lda     #$15                    ;2         *
    sta     ram_D4                  ;3         *
    lda     #$1e                    ;2         *
    sta     ram_D5                  ;3         *
    lda     ram_E3                  ;3         *
    ora     #$80                    ;2         *
    sta     ram_E3                  ;3         *
    lda     #$25                    ;2         *
    sta     CTRLPF                  ;3         *
    jmp     L9cd2                   ;3   =  33 *
    
L9b4f
    lda     ram_E5                  ;3        
    cmp     #$20                    ;2        
    bne     L9b64                   ;2/3      
    lda     #$15                    ;2         *
    sta     ram_D4                  ;3         *
    lda     #$4e                    ;2         *
    sta     ram_D5                  ;3         *
    lda     #$15                    ;2         *
    sta     CTRLPF                  ;3         *
    jmp     L9cd2                   ;3   =  25 *
    
L9b64
    lda     ram_E5                  ;3        
    cmp     #$21                    ;2        
    bne     L9b7d                   ;2/3      
    lda     #$1a                    ;2         *
    sta     ram_D4                  ;3         *
    lda     #$10                    ;2         *
    sta     ram_D5                  ;3         *
    lda     #$86                    ;2         *
    sta     ram_DA                  ;3         *
    lda     #$21                    ;2         *
    sta     CTRLPF                  ;3         *
    jmp     L9cd2                   ;3   =  30 *
    
L9b7d
    lda     ram_E5                  ;3        
    cmp     #$22                    ;2        
    bne     L9b86                   ;2/3      
    jmp     L9b8c                   ;3   =  10 *
    
L9b86
    lda     ram_E5                  ;3        
    cmp     #$25                    ;2        
    bne     L9b95                   ;2/3 =   7
L9b8c
    lda     #$4e                    ;2         *
    sta     ram_D4                  ;3         *
    sta     ram_D5                  ;3         *
    jmp     L9cd2                   ;3   =  11 *
    
L9b95
    lda     ram_E5                  ;3        
    cmp     #$23                    ;2        
    bne     L9baa                   ;2/3      
    lda     #$80                    ;2         *
    sta     ram_D4                  ;3         *
    lda     #$26                    ;2         *
    sta     ram_D5                  ;3         *
    lda     #$15                    ;2         *
    sta     CTRLPF                  ;3         *
    jmp     L9cd2                   ;3   =  25 *
    
L9baa
    lda     ram_E5                  ;3        
    cmp     #$24                    ;2        
    bne     L9bbf                   ;2/3      
    lda     #$80                    ;2         *
    sta     ram_D4                  ;3         *
    lda     #$4d                    ;2         *
    sta     ram_D5                  ;3         *
    lda     #$25                    ;2         *
    sta     CTRLPF                  ;3         *
    jmp     L9cd2                   ;3   =  25 *
    
L9bbf
    lda     ram_E5                  ;3        
    cmp     #$26                    ;2        
    bcc     L9bd6                   ;2/3      
    lda     #$29                    ;2        
    cmp     ram_E5                  ;3        
    bcc     L9bd6                   ;2/3      
    lda     #$82                    ;2         *
    sta     ram_D4                  ;3         *
    lda     #$18                    ;2         *
    sta     ram_D5                  ;3         *
    jmp     L9cd2                   ;3   =  27 *
    
L9bd6
    lda     ram_E5                  ;3        
    cmp     #$2a                    ;2        
    bcc     L9bed                   ;2/3      
    lda     #$2d                    ;2        
    cmp     ram_E5                  ;3        
    bcc     L9bed                   ;2/3      
    lda     #$15                    ;2         *
    sta     ram_D4                  ;3         *
    lda     #$18                    ;2         *
    sta     ram_D5                  ;3         *
    jmp     L9cd2                   ;3   =  27 *
    
L9bed
    lda     ram_E5                  ;3        
    cmp     #$2e                    ;2        
    bne     L9c06                   ;2/3!     
    lda     #$80                    ;2         *
    sta     ram_D4                  ;3         *
    lda     #$4d                    ;2         *
    sta     ram_D5                  ;3         *
    lda     #$46                    ;2         *
    sta     ram_DB                  ;3         *
    lda     #$18                    ;2         *
    sta     ram_DA                  ;3         *
    jmp     L9cd2                   ;3   =  30 *
    
L9c06
    lda     ram_E5                  ;3        
    cmp     #$2f                    ;2        
    bne     L9c1d                   ;2/3      
    lda     #$15                    ;2         *
    sta     ram_D4                  ;3         *
    lda     #$28                    ;2         *
    sta     ram_D5                  ;3         *
    lda     ram_E0                  ;3         *
    and     #$fe                    ;2         *
    sta     ram_E0                  ;3         *
    jmp     L9cd2                   ;3   =  28 *
    
L9c1d
    lda     ram_E5                  ;3        
    cmp     #$30                    ;2        
    bne     L9c2e                   ;2/3      
    lda     #$15                    ;2         *
    sta     ram_D4                  ;3         *
    lda     #$28                    ;2         *
    sta     ram_D5                  ;3         *
    jmp     L9cd2                   ;3   =  20 *
    
L9c2e
    lda     ram_E5                  ;3        
    cmp     #$31                    ;2        
    bne     L9c37                   ;2/3      
    jmp     L9c3d                   ;3   =  10 *
    
L9c37
    lda     ram_E5                  ;3        
    cmp     #$32                    ;2        
    bne     L9c48                   ;2/3 =   7
L9c3d
    lda     #$15                    ;2         *
    sta     ram_D4                  ;3         *
    lda     #$46                    ;2         *
    sta     ram_D5                  ;3         *
    jmp     L9cd2                   ;3   =  13 *
    
L9c48
    lda     ram_E5                  ;3        
    cmp     #$33                    ;2        
    bne     L9c61                   ;2/3      
    lda     #$15                    ;2         *
    sta     ram_D4                  ;3         *
    lda     #$20                    ;2         *
    sta     ram_D5                  ;3         *
    lda     #$74                    ;2         *
    sta     ram_D6                  ;3         *
    lda     #$fa                    ;2         *
    sta     ram_D7                  ;3         *
    jmp     L9cd2                   ;3   =  30 *
    
L9c61
    lda     ram_E5                  ;3        
    cmp     #$34                    ;2        
    bne     L9c7e                   ;2/3      
    lda     ram_EB                  ;3         *
    and     #$02                    ;2         *
    beq     L9c7e                   ;2/3       *
    lda     #$15                    ;2         *
    sta     ram_D4                  ;3         *
    lda     #$38                    ;2         *
    sta     ram_D5                  ;3         *
    lda     ram_EB                  ;3         *
    and     #$fd                    ;2         *
    sta     ram_EB                  ;3         *
    jmp     L9cd2                   ;3   =  35 *
    
L9c7e
    lda     ram_E5                  ;3        
    cmp     #$34                    ;2        
    bne     L9c8f                   ;2/3      
    lda     #$15                    ;2         *
    sta     ram_D4                  ;3         *
    lda     #$10                    ;2         *
    sta     ram_D5                  ;3         *
    jmp     L9cd2                   ;3   =  20 *
    
L9c8f
    lda     ram_E5                  ;3        
    cmp     #$35                    ;2        
    bne     L9ca8                   ;2/3      
    lda     #$50                    ;2         *
    sta     ram_D4                  ;3         *
    lda     #$10                    ;2         *
    sta     ram_D5                  ;3         *
    lda     #$fa                    ;2         *
    sta     ram_DB                  ;3         *
    lda     #$00                    ;2         *
    sta     ram_ED                  ;3         *
    jmp     L9cd2                   ;3   =  30 *
    
L9ca8
    lda     ram_E5                  ;3        
    cmp     #$36                    ;2        
    bne     L9cc1                   ;2/3      
    lda     #$15                    ;2         *
    sta     ram_D4                  ;3         *
    lda     #$28                    ;2         *
    sta     ram_D5                  ;3         *
    lda     #$fa                    ;2         *
    sta     ram_DB                  ;3         *
    lda     #$00                    ;2         *
    sta     ram_ED                  ;3         *
    jmp     L9cd2                   ;3   =  30 *
    
L9cc1
    lda     ram_E5                  ;3        
    cmp     #$64                    ;2        
    bne     L9cd2                   ;2/3      
    lda     #$80                    ;2        
    sta     ram_D4                  ;3        
    lda     #$1e                    ;2        
    sta     ram_D5                  ;3        
    jmp     L9cd2                   ;3   =  20
    
L9cd2
    lda     ram_E3                  ;3        
    ora     #$40                    ;2        
    sta     ram_E3                  ;3        
    jmp     Lffdd                   ;3   =  11
    
    .byte   $4c,$e4,$9c,$19,$19,$19,$10,$19 ; $9cdb (*)
    .byte   $16                             ; $9ce3 (*)

  IF PLUSROM = 1
    ORG     $4fd0, $ff
    RORG    $9fd0
  ELSE
    ORG     $4fd4, $ff
    RORG    $9fd4
  ENDIF

Start_b4
    ldx #$ff
    txs
    lda #$f2
    pha
    lda #$4f
    pha
    
L9fdd
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    tsx                             ;2        
    lda     CXM0FB,x                ;4        
    rol                             ;2        
    rol                             ;2        
    rol                             ;2        
    rol                             ;2        
    and     #$07                    ;2        
    tax                             ;2        
    inx                             ;2   =  28
L9feb
    lda     $1ff3,x                 ;4        
    pla                             ;4        
    tax                             ;2        
    pla                             ;4        
    rts                             ;6   =  20
    
    ORG     $4ff3, $ff
    RORG    $9ff3

L9ff3
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff     ; $9ff3 (*)

  IF PLUSROM = 1
    .word (PlusROM_API - $6000)             ; PlusRom API pointer
  ELSE
    .byte   $ff,$ff                         ; $9ffa (D)
  ENDIF

    .word Start_b4, Start_b4


;***********************************************************
;      Bank 5 / 0..7
;***********************************************************

    SEG     CODE
    ORG     $5000
    RORG    $b000

    lda     #$01                    ;2         *
    bit     SWCHB                   ;4         *
    bne     Lb019                   ;2/3       *
    sta     ram_EE                  ;3         *
    lda     #$10                    ;2         *
    pha                             ;3         *
    lda     #$45                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$01                    ;2         *
    jmp     Lffeb                   ;3   =  37 *
    
Lb019
    lda     #BLACK                  ;2         *
    sta     COLUPF                  ;3         *
    lda     #$f7                    ;2         *
    sta     ram_F1                  ;3         *
    lda     #$06                    ;2         *
    sta     ram_F0                  ;3         *
    ldx     #$f9                    ;2         *
    stx     ram_8A                  ;3         *
    lda     #$f7                    ;2         *
    sta     ram_8B                  ;3         *
    lda     #$06                    ;2         *
    sta     ram_8E                  ;3         *
    lda     #$fa                    ;2         *
    sta     ram_89                  ;3         *
    lda     #$05                    ;2         *
    cmp     ram_DC                  ;3         *
    bcs     Lb04a                   ;2/3       *
    lda     #$07                    ;2         *
    sta     AUDV0                   ;3         *
    lda     #$03                    ;2         *
    sta     AUDC0                   ;3         *
    lda     ram_DC                  ;3         *
    clc                             ;2         *
    adc     #$1e                    ;2         *
    sta     AUDF0                   ;3   =  62 *
Lb04a
    lda     ram_DC                  ;3         *
    cmp     #$00                    ;2         *
    bne     Lb05c                   ;2/3       *
    ldx     #$00                    ;2         *
    stx     ram_8C                  ;3         *
    lda     #$f8                    ;2         *
    sta     ram_8D                  ;3         *
    lda     #$07                    ;2         *
    sta     ram_8F                  ;3   =  22 *
Lb05c
    lda     ram_DC                  ;3         *
    cmp     #$14                    ;2         *
    bne     Lb06e                   ;2/3       *
    ldx     #$08                    ;2         *
    stx     ram_8C                  ;3         *
    lda     #$f8                    ;2         *
    sta     ram_8D                  ;3         *
    lda     #$07                    ;2         *
    sta     ram_8F                  ;3   =  22 *
Lb06e
    lda     ram_DC                  ;3         *
    cmp     #$28                    ;2         *
    bne     Lb080                   ;2/3       *
    ldx     #$10                    ;2         *
    stx     ram_8C                  ;3         *
    lda     #$f8                    ;2         *
    sta     ram_8D                  ;3         *
    lda     #$07                    ;2         *
    sta     ram_8F                  ;3   =  22 *
Lb080
    lda     #$1c                    ;2         *
    sta     ram_85                  ;3         *
    lda     #$68                    ;2         *
    sta     ram_80                  ;3         *
    lda     #$fa                    ;2         *
    sta     ram_91                  ;3         *
    lda     #$11                    ;2         *
    sta     CTRLPF                  ;3         *
    lda     #$15                    ;2         *
    sta     NUSIZ1                  ;3         *
    inc     ram_DC                  ;5         *
    lda     #$1c                    ;2         *
    sta     ram_A3                  ;3         *
    lda     #BLACK                  ;2         *
    sta     COLUBK                  ;3         *
    lda     #$00                    ;2         *
    sta     AUDV1                   ;3         *
    lda     #$3c                    ;2         *
    cmp     ram_DC                  ;3         *
    bcs     Lb0b4                   ;2/3       *
    lda     #$00                    ;2         *
    sta     ram_DC                  ;3         *
    sta     AUDV0                   ;3         *
    lda     ram_E8                  ;3         *
    and     #$fd                    ;2         *
    sta     ram_E8                  ;3   =  68 *
Lb0b4
    lda     #RED                    ;2         *
    sta     COLUP0                  ;3         *
    lda     ram_DC                  ;3         *
    sta     COLUP1                  ;3         *
    lda     ram_E8                  ;3         *
    lsr                             ;2         *
    bcs     Lb0ee                   ;2/3       *
    lda     ram_E8                  ;3         *
    and     #$02                    ;2         *
    bne     Lb0ee                   ;2/3       *
    lda     ram_A2                  ;3         *
    lsr                             ;2         *
    bcc     Lb0ce                   ;2/3       *
    eor     #$b4                    ;2   =  34 *
Lb0ce
    sta     ram_A2                  ;3         *
    and     #$3f                    ;2         *
    clc                             ;2         *
    adc     #$0a                    ;2         *
    sta     ram_EC                  ;3         *
    lda     ram_A2                  ;3         *
    lsr                             ;2         *
    bcc     Lb0de                   ;2/3       *
    eor     #$b4                    ;2   =  21 *
Lb0de
    sta     ram_A2                  ;3         *
    and     #$7f                    ;2         *
    sta     ram_83                  ;3         *
    lda     #$64                    ;2         *
    sta     ram_88                  ;3         *
    lda     ram_E8                  ;3         *
    ora     #$01                    ;2         *
    sta     ram_E8                  ;3   =  21 *
Lb0ee
    lda     ram_E8                  ;3         *
    and     #$02                    ;2         *
    bne     Lb0fc                   ;2/3       *
    lda     ram_EC                  ;3         *
    cmp     ram_88                  ;3         *
    bcs     Lb0fc                   ;2/3       *
    dec     ram_88                  ;5   =  20 *
Lb0fc
    lda     ram_EC                  ;3         *
    cmp     ram_88                  ;3         *
    bcc     Lb121                   ;2/3       *
    lda     ram_83                  ;3         *
    sec                             ;2         *
    sbc     #$08                    ;2         *
    sta     ram_81                  ;3         *
    lda     ram_EC                  ;3         *
    sta     ram_86                  ;3         *
    lda     #$64                    ;2         *
    sta     ram_88                  ;3         *
    lda     ram_E8                  ;3         *
    ora     #$02                    ;2         *
    sta     ram_E8                  ;3         *
    lda     ram_E8                  ;3         *
    and     #$fe                    ;2         *
    sta     ram_E8                  ;3         *
    lda     #$00                    ;2         *
    sta     ram_D6                  ;3   =  50 *
Lb121
    sta     ram_EE                  ;3         *
    lda     #$b1                    ;2         *
    pha                             ;3         *
    lda     #$38                    ;2         *
    pha                             ;3         *
    lda     #$f4                    ;2         *
    pha                             ;3         *
    lda     #$35                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$08                    ;2         *
    jmp     Lffeb                   ;3   =  39 *
    
    jmp $b000                       ; never reached?
    lda #ORANGE
    sta COLUP1
    lda #$05
    sta $05
    lda $e3
    ora #$01
    sta $e3
    bit $07
    bpl lb152
    lda #$54
    sta $d5
lb152
    lda $d5
    cmp #$54
    beq lb15b
    jmp Lb1e8
lb15b
    lda ram_DC
    cmp #$00
    bne lb169
    lda $e6
    cmp #$05
    bcs lb169
    inc $e6
lb169
    lda $e6
    cmp #$05
    bne lb177
    lda #$00
    cmp $d7
    bcs lb177
    dec $d7
lb177
    lda #$00
    cmp $d7
    bcc lb198
    sta $ee
    lda #$b1
    pha
    lda #$94
    pha
    lda #$d1
    pha
    lda #$77
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$07
    jmp Lffeb
    jmp $b000                     ; never reached?
lb198
    lda #$0e
    sta $15
    lda #$0a
    sta $19
    lda $e0
    ora #$08
    sta $e0
    lda ram_DC
    cmp #$00
    bne lb1bc
    lda #$15
    sta $17
    ldx #$18
    stx $8c
    lda #$f8
    sta $8d
    lda #$0f
    sta $8f
lb1bc
    lda ram_DC
    cmp #$0a
    bne lb1d2
    lda #$1f
    sta $17
    ldx #$28
    stx $8c
    lda #$f8
    sta $8d
    lda #$0f
    sta $8f
lb1d2
    lda ram_DC
    cmp #$1e
    bne $b1e8
    lda #$1b
    sta $17
    ldx #$38
    stx $8c
    lda #$f8
    sta $8d
    lda #$0f
    sta $8f
Lb1e8
    jmp   Lffdd                     ;3
    
Lb1eb
    sta     ram_EE                  ;3        
    lda     #$b2                    ;2        
    pha                             ;3        
    lda     #$02                    ;2        
    pha                             ;3        
    lda     #$f5                    ;2        
    pha                             ;3        
    lda     #$9b                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
  IF PLUSROM
    jmp     SendPlusROMScore        ;3   =  39
  ELSE
    jmp     Lffeb                   ;3   =  39
  ENDIF
    
    lda     #$00                    ;2        
    sta     ram_8E                  ;3        
    lda     #$01                    ;2        
    sta     ram_8F                  ;3        
    sta     ram_EE                  ;3        
    lda     #$b2                    ;2        
    pha                             ;3        
    lda     #$22                    ;2        
    pha                             ;3        
    lda     #$cf                    ;2        
    pha                             ;3        
    lda     #$ff                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  49
    
Lb223
    lda     #$fa                    ;2        
    sta     ram_89                  ;3        
    lda     #$fa                    ;2        
    sta     ram_88                  ;3        
    lda     #$fa                    ;2        
    sta     ram_91                  ;3        
    lda     #$01                    ;2        
    bit     SWCHB                   ;4        
    bne     Lb24e                   ;2/3      
    lda     #$fa                    ;2         *
    sta     ram_D7                  ;3         *
    sta     ram_D5                  ;3         *
    sta     ram_EE                  ;3         *
    lda     #$10                    ;2         *
    pha                             ;3         *
    lda     #$45                    ;2         *
    pha                             ;3         *
    lda     ram_EE                  ;3         *
    pha                             ;3         *
    txa                             ;2         *
    pha                             ;3         *
    ldx     #$01                    ;2         *
    jmp     Lffeb                   ;3   =  60 *
    
Lb24e
    inc     ram_DC                  ;5        
    lda     ram_DC                  ;3        
    cmp     #$14                    ;2        
    bcc     Lb280                   ;2/3      
    lda     ram_ED                  ;3        
    cmp     #$0b                    ;2        
    bcs     Lb280                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_DC                  ;3        
    lda     #$0a                    ;2        
    sta     AUDV0                   ;3        
    ldx     ram_ED                  ;3        
    lda     Lb309,x                 ;4        
    sta     AUDF0                   ;3        
    lda     #$65                    ;2        
    sta     AUDC0                   ;3        
    lda     #$0a                    ;2        
    sta     AUDV1                   ;3        
    ldx     ram_ED                  ;3        
    lda     Lb309,x                 ;4        
    sta     AUDF0                   ;3        
    lda     #$0a                    ;2        
    sta     AUDC1                   ;3        
    inc     ram_ED                  ;5   =  69
Lb280
    lda     ram_DC                  ;3        
    cmp     #$19                    ;2        
    bcc     Lb28c                   ;2/3      
    lda     #$00                    ;2        
    sta     AUDV0                   ;3        
    sta     AUDV1                   ;3   =  15
Lb28c
    lda     ram_DC                  ;3        
    cmp     #$3c                    ;2        
    bcc     Lb29c                   ;2/3      
    lda     #$3c                    ;2        
    sta     ram_DC                  ;3        
    lda     #$00                    ;2        
    sta     AUDV0                   ;3        
    sta     AUDV1                   ;3   =  20
Lb29c
    lda     ram_ED                  ;3        
    cmp     #$0b                    ;2        
    bcc     Lb2b8                   ;2/3      
    bit     INPT4                   ;3        
    bmi     Lb2b8                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$10                    ;2        
    pha                             ;3        
    lda     #$45                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$01                    ;2        
    jmp     Lffeb                   ;3   =  41
    
Lb2b8
    lda     #BLACK|$6               ;2        
    sta     COLUBK                  ;3        
    lda     #BLACK                  ;2        
    sta     COLUP0                  ;3        
    sta     COLUP1                  ;3        
    lda     #$05                    ;2        
    sta     NUSIZ0                  ;3        
    sta     NUSIZ1                  ;3        
    lda     #$41                    ;2        
    sta     ram_80                  ;3        
    lda     #$3c                    ;2        
    sta     ram_85                  ;3        
    lda     ram_80                  ;3        
    clc                             ;2        
    adc     #$10                    ;2        
    sta     ram_81                  ;3        
    lda     ram_85                  ;3        
    sta     ram_86                  ;3        
    lda     ram_8E                  ;3        
    cmp     #$1e                    ;2        
    bcs     Lb2e3                   ;2/3      
    inc     ram_8E                  ;5   =  59
Lb2e3
    lda     ram_8F                  ;3        
    cmp     #$1e                    ;2        
    bcs     Lb2eb                   ;2/3      
    inc     ram_8F                  ;5   =  12
Lb2eb
    sta     ram_EE                  ;3        
    lda     #$b3                    ;2        
    pha                             ;3        
    lda     #$02                    ;2        
    pha                             ;3        
    lda     #$f4                    ;2        
    pha                             ;3        
    lda     #$35                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  39
    
    jmp     Lb223                   ;3   =   3
    
    jmp     Lb314                   ;3 never reached?


Lb309
    .byte   $13,$13,$13,$13,$10,$11,$11,$13 ; $b309 (A)
    .byte   $13,$14,$13                     ; $b311 (A)
    
Lb314
    lda     #$02                    ;2        
    sta     ram_92                  ;3        
    lda     ram_84                  ;3        
    sec                             ;2        
    sbc     #$06                    ;2        
    pha                             ;3        
    tsx                             ;2        
    pla                             ;4        
    lda     ram_D4                  ;3        
    cmp     CXM1P,x                 ;4        
    bcs     Lb32f                   ;2/3      
    lda     ram_E0                  ;3        
    and     #$fe                    ;2        
    sta     ram_E0                  ;3        
    jmp     Lb377                   ;3   =  41
    
Lb32f
    lda     ram_84                  ;3        
    clc                             ;2        
    adc     #$04                    ;2        
    cmp     ram_D4                  ;3        
    bcs     Lb341                   ;2/3      
    lda     ram_E0                  ;3        
    and     #$fe                    ;2        
    sta     ram_E0                  ;3        
    jmp     Lb377                   ;3   =  23
    
Lb341
    bit     CXP0FB                  ;3        
    bvs     Lb34e                   ;2/3      
    lda     ram_E0                  ;3        
    and     #$fe                    ;2        
    sta     ram_E0                  ;3        
    jmp     Lb377                   ;3   =  16
    
Lb34e
    bit     CXP0FB                  ;3        
    bvc     Lb366                   ;2/3      
    lda     ram_89                  ;3        
    sec                             ;2        
    sbc     ram_92                  ;3        
    pha                             ;3        
    tsx                             ;2        
    pla                             ;4        
    lda     ram_D5                  ;3        
    cmp     CXM1P,x                 ;4        
    bcc     Lb366                   ;2/3      
    lda     ram_E0                  ;3        
    ora     #$01                    ;2        
    sta     ram_E0                  ;3   =  39
Lb366
    bit     CXP0FB                  ;3        
    bvc     Lb377                   ;2/3      
    lda     ram_89                  ;3        
    cmp     ram_D5                  ;3        
    bcs     Lb377                   ;2/3      
    lda     ram_89                  ;3        
    sec                             ;2        
    sbc     #$02                    ;2        
    sta     ram_D5                  ;3   =  23
Lb377
    jmp     Lffdd                   ;3   =   3
    
    lda $e8
    lsr
    bcc lb382
    jmp lb424
lb382
    lda $e7
    cmp #$00
    bne lb3b3
    lda #$fa
    sta $db
    lda $e8
    ora #$01
    sta $e8
    ldx #$01
    ldy #$05
    lda #$1e
    sta $ee
    lda #$b3
    pha
    lda #$af
    pha
    lda #$f2
    pha
    lda #$b8
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    jmp lb424
lb3b3
    lda #GREEN_YELLOW
    sta COLUP1
    lda #$08
    sta $0c
    lda #$04
    sta ram_87
    lda #$20
    sta $05
    lda $d6
    clc
    adc #$08
    cmp $da
    bcc lb3d8
    lda $db
    sta $18
    lda #$0e
    sta $16
    lda #$07
    sta $1a
lb3d8
    lda $da
    clc
    adc #$02
    sta $da
    lda $da
    cmp #$8c
    bcs lb3e8
    jmp lb424
lb3e8
    lda $db
    cmp #$46
    bne lb3fc
    lda #$2e
    sta $db
    lda $d6
    clc
    adc #$04
    sta $da
    jmp lb424
lb3fc
    lda $db
    cmp #$2e
    bne lb410
    lda #$16
    sta $db
    lda $d6
    clc
    adc #$04
    sta $da
    jmp lb424
lb410
    lda $db
    cmp #$16
    bne lb424
    lda #$46
    sta $db
    lda $d6
    clc
    adc #$04
    sta $da
    jmp lb424
lb424
    jmp Lffdd
    lda $e8
    ora #$10
    sta $e8
    bit $03
    bmi lb438
    lda $d7
    clc
    adc #$03
    sta $d7
lb438
    lda ram_DC
    cmp #$14
    bcs lb442
    lda #YELLOW
    sta COLUP1
lb442
    lda ram_DC
    cmp #$14
    bcc lb44c
    lda #YELLOW|$e
    sta COLUP1
lb44c
    lda $e8
    lsr
    bcc lb454
    jmp lb477
lb454
    bit $03
    bpl lb467
    lda $e8
    and #$02
    beq lb467
    lda $e8
    and #$fd
    sta $e8
    jmp lb477
lb467
    bit $03
    bpl lb477
    lda $e8
    and #$02
    bne lb477
    lda $e8
    ora #$02
    sta $e8
lb477
    lda $d7
    cmp #$00
    beq lb480
    jmp lb494
lb480
    lda $e8
    and #$02
    beq lb48a
    lda #$4e
    sta $d6
lb48a
    lda $e8
    and #$02
    bne lb494
    lda #$2b
    sta $d6
lb494
    lda $e8
    lsr
    bcc lb4c8
    lda $e2
    cmp #$06
    bne lb4c8
    sta $ee
    lda #$b4
    pha
    lda #$b6
    pha
    lda #$da
    pha
    lda #$d8
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$07
    jmp Lffeb
    lda $e8
    and #$fe
    sta $e8
    lda #$00
    sta $e2
    lda #$fa
    sta $d7
    jmp lb507
lb4c8
    lda $e8
    lsr
    bcc lb4d2
    inc $e2
    jmp lb507
lb4d2
    bit $03
    bpl lb507
    sta $ee
    lda #$b4
    pha
    lda #$ed
    pha
    lda #$dc
    pha
    lda #$0d
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$07
    jmp Lffeb
    lda #$00
    sta $e2
    lda $e8
    ora #$01
    sta $e8
    lda #$07
    sta $1a
    lda #$09
    sta $16
    lda $e2
    clc
    adc #$08
    sta $18
lb507
    jmp Lffdd
    lda $e8
    ora #$10
    sta $e8
    lda #$3b
    sta $84
    lda #$11
    sta $89
    bit $02
    bvc lb54c
    lda $d5
    cmp $89
    bcs lb525
    jmp lb54c
lb525
    lda $89
    sec
    sbc #$02
    sta $89
    inc $d7
    lda $e8
    and #$02
    beq lb53d
    lda $e8
    and #$fd
    sta $e8
    jmp lb54c
lb53d
    lda $e8
    and #$02
    bne lb54c
    lda $e8
    ora #$02
    sta $e8
    jmp lb54c
lb54c
    lda #$50
    sta $da
    lda #$18
    sta ram_87
    lda $e8
    and #$02
    beq lb567
    lda #$18
    cmp $db
    bcs lb567
    lda $db
    sec
    sbc #$02
    sta $db
lb567
    lda $e8
    and #$02
    bne lb57a
    lda $db
    cmp #$32
    bcs lb57a
    lda $db
    clc
    adc #$02
    sta $db
lb57a
    lda $d6
    cmp #$11
    bcs lb584
    lda #$88
    sta $d6
lb584
    lda #YELLOW
    sta COLUP1
    lda ram_DC
    cmp #$00
    bne lb59a
    ldx #$48
    stx $8c
    lda #$f8
    sta $8d
    lda #$03
    sta $8f
lb59a
    lda ram_DC
    cmp #$06
    bne lb5ac
    ldx #$4c
    stx $8c
    lda #$f8
    sta $8d
    lda #$02
    sta $8f
lb5ac
    lda ram_DC
    cmp #$0c
    bne lb5be
    ldx #$4f
    stx $8c
    lda #$f8
    sta $8d
    lda #$07
    sta $8f
lb5be
    lda ram_DC
    cmp #$12
    bne lb5d0
    ldx #$57
    stx $8c
    lda #$f8
    sta $8d
    lda #$07
    sta $8f
lb5d0
    lda ram_DC
    cmp #$18
    bne lb5e2
    ldx #$5f
    stx $8c
    lda #$f8
    sta $8d
    lda #$06
    sta $8f
lb5e2
    lda ram_DC
    cmp #$1e
    bne lb5f4
    ldx #$66
    stx $8c
    lda #$f8
    sta $8d
    lda #$04
    sta $8f
lb5f4
    lda #$50
    sta $d7
    dec $d6
    jmp Lffdd
    jmp Lffdd
    lda #BLACK|$6
    sta COLUP1
    lda $e8
    ora #$10
    sta $e8
    lda $e3
    ora #$80
    sta $e3
    lda #$20
    sta $05
    lda #$03
    sta ram_87
    lda $d4
    cmp #$17
    bcs lb622
    lda #$17
    sta $d4
lb622
    lda #$83
    cmp $d4
    bcs lb62c
    lda #$83
    sta $d4
lb62c
    lda $e8
    and #$02
    bne lb635
    jmp lb63b
lb635
    lda #$6e
    cmp $e2
    bcs lb63e
lb63b
    jmp lb819
lb63e
    lda #$06
    sta $1a
    lda #$0c
    sta $16
    lda $e0
    and #$fe
    sta $e0
    lda $e2
    cmp #$70
    bcs lb654
    inc $e2
lb654
    lda $e2
    cmp #$0a
    bne lb69f
    ldx #$01
    lda #$1f
    sta $9e
    ldy #$04
    lda #$00
    sta $ee
    lda #$b6
    pha
    lda #$7b
    pha
    lda #$f2
    pha
    lda #$e8
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    lda #$0b
    sta $e2
    lda #$06
    sta $18
    sta $ee
    lda #$b6
    pha
    lda #$9b
    pha
    lda #$f4
    pha
    lda #$35
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    jmp lbafc
lb69f
    lda $e2
    cmp #$1e
    bne lb6ea
    ldx #$01
    lda #$1f
    sta $9e
    ldy #$05
    lda #$00
    sta $ee
    lda #$b6
    pha
    lda #$c6
    pha
    lda #$f2
    pha
    lda #$e8
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    lda #$1f
    sta $e2
    lda #$08
    sta $18
    sta $ee
    lda #$b6
    pha
    lda #$e6
    pha
    lda #$f4
    pha
    lda #$35
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    jmp lbafc
lb6ea
    lda $e2
    cmp #$32
    bne lb735
    ldx #$01
    lda #$1f
    sta $9e
    ldy #$06
    lda #$00
    sta $ee
    lda #$b7
    pha
    lda #$11
    pha
    lda #$f2
    pha
    lda #$e8
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    lda #$33
    sta $e2
    lda #$0b
    sta $18
    sta $ee
    lda #$b7
    pha
    lda #$31
    pha
    lda #$f4
    pha
    lda #$35
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    jmp lbafc
lb735
    lda $e2
    cmp #$46
    bne lb780
    ldx #$01
    lda #$1f
    sta $9e
    ldy #$07
    lda #$00
    sta $ee
    lda #$b7
    pha
    lda #$5c
    pha
    lda #$f2
    pha
    lda #$e8
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    lda #$47
    sta $e2
    lda #$0f
    sta $18
    sta $ee
    lda #$b7
    pha
    lda #$7c
    pha
    lda #$f4
    pha
    lda #$35
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    jmp lbafc
lb780
    lda $e2
    cmp #$5a
    bne lb7cb
    ldx #$01
    lda #$1f
    sta $9e
    ldy #$08
    lda #$00
    sta $ee
    lda #$b7
    pha
    lda #$a7
    pha
    lda #$f2
    pha
    lda #$e8
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    lda #$5b
    sta $e2
    lda #$12
    sta $18
    sta $ee
    lda #$b7
    pha
    lda #$c7
    pha
    lda #$f4
    pha
    lda #$35
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    jmp lbafc
lb7cb
    lda $e2
    cmp #$6e
    bne lb816
    ldx #$01
    lda #$1f
    sta $9e
    ldy #$09
    lda #$00
    sta $ee
    lda #$b7
    pha
    lda #$f2
    pha
    lda #$f2
    pha
    lda #$e8
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    lda #$6f
    sta $e2
    lda #$17
    sta $18
    sta $ee
    lda #$b8
    pha
    lda #$12
    pha
    lda #$f4
    pha
    lda #$35
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    jmp lbafc
lb816
    jmp lbafc
lb819
    lda #$6e
    cmp $e2
    bcs lb831
    lda $eb
    and #$fb
    sta $eb
    lda #BLACK
    sta COLUPF
    lda #$f7
    sta ram_F1
    lda #$07
    sta ram_F0
lb831
    lda $e8
    and #$02
    beq lb83a
    jmp lb938
lb83a
    lda $e0
    and #$04
    beq lb844
    lda #$08
    sta $0b
lb844
    lda $eb
    and #$04
    beq lb868
    sta $ee
    lda #$b8
    pha
    lda #$61
    pha
    lda #$d9
    pha
    lda #$ed
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$07
    jmp Lffeb
    lda $e0
    ora #$01
    sta $e0
lb868
    lda $eb
    and #$04
    bne lb874
    lda $e0
    and #$fe
    sta $e0
lb874
    lda $e0
    and #$02
    beq lb886
    lda $d5
    cmp #$14
    bcs lb886
    lda $e0
    and #$fd
    sta $e0
lb886
    lda $e0
    and #$02
    beq lb8ad
    lda $d5
    cmp #$2a
    bcs lb8ad
    sta $ee
    lda #$b8
    pha
    lda #$a9
    pha
    lda #$da
    pha
    lda #$41
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$07
    jmp Lffeb
    jmp lb938
lb8ad
    lda $d5
    cmp #$28
    bcs lb8bc
    lda $eb
    and #$fb
    sta $eb
    jmp lb938
lb8bc
    lda $d5
    cmp #$28
    bcc lb8c8
    lda $eb
    ora #$04
    sta $eb
lb8c8
    lda $d5
    cmp #$34
    bcs lb8da
    lda #$10
    bit $0280
    beq lb8da
    inc $d5
    jmp lb938
lb8da
    lda $d4
    cmp #$17
    bcs lb8e7
    lda #$17
    sta $d4
    jmp lb938
lb8e7
    lda #$83
    cmp $d4
    bcs lb8f4
    lda #$83
    sta $d4
    jmp lb938
lb8f4
    bit $0280
    bvs lb904
    dec $d4
    lda $e0
    ora #$04
    sta $e0
    jmp lb938
lb904
    bit $0280
    bmi lb914
    inc $d4
    lda $e0
    and #$fb
    sta $e0
    jmp lb938
lb914
    lda #$10
    bit $0280
    bne lb926
    lda #$2b
    cmp $d5
    bcs lb926
    dec $d5
    jmp lb938
lb926
    lda #$20
    bit $0280
    bne lb938
    lda $d5
    cmp #$50
    bcs lb938
    inc $d5
    jmp lb938
lb938
    lda $e8
    and #$02
    bne lb941
    jmp lb9a2
lb941
    bit $02
    bvc lb957
    lda $e0
    ora #$01
    sta $e0
    lda $e0
    and #$f7
    sta $e0
    lda $e0
    and #$fd
    sta $e0
lb957
    bit $02
    bvs lb964
    lda $e0
    and #$fe
    sta $e0
    jmp lb9a2
lb964
    sta $ee
    lda #$b9
    pha
    lda #$7b
    pha
    lda #$da
    pha
    lda #$0b
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$07
    jmp Lffeb
    lda $e0
    ora #$08
    sta $e0
    bit $02
    bpl lb990
    lda $d5
    clc
    adc #$02
    sta $d5
    jmp lb9a2
lb990
    lda #$10
    bit $0280
    bne lb999
    dec $d5
lb999
    lda #$20
    bit $0280
    bne lb9a2
    inc $d5
lb9a2
    lda $d5
    cmp #$08
    bcs lb9b4
    lda $e3
    ora #$08
    sta $e3
    inc $e5
    lda #$fa
    sta $db
lb9b4
    lda $e8
    and #$02
    beq lb9c5
    lda $89
    cmp #$40
    bcs lb9c5
    inc $89
    jmp lbafc
lb9c5
    lda $e8
    and #$02
    beq lb9d6
    lda #GREEN|$2
    sta COLUP1
    lda #$11
    sta $0a
    jmp lbafc
lb9d6
    bit $02
    bvc lba2b
    lda #$11
    sta $0a
    lda $e8
    ora #$02
    sta $e8
    lda #$fa
    sta $d7
    ldx #$01
    lda #$12
    sta $9e
    ldy #$00
    lda #$0e
    sta $ee
    lda #$ba
    pha
    lda #$09
    pha
    lda #$f2
    pha
    lda #$e8
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    lda #$fa
    sta $db
    lda #$40
    sta $92
    lda #$53
    sta $84
    lda #$00
    sta $89
    sta $e2
    lda #$07
    sta $1a
    lda #$09
    sta $16
    lda #$08
    sta $18
    jmp lbafc
lba2b
    lda #$21
    sta $0a
    lda $e8
    and #$02
    bne lba41
    lda #$18
    sta $89
    lda #$7c
    sta $84
    lda #$06
    sta $92
lba41
    lda #$27
    sta $db
    lda #$0a
    cmp ram_DC
    bcs lba52
    lda $da
    sec
    sbc #$02
    sta $da
lba52
    lda #$15
    cmp $da
    bcc lba5c
    lda #$86
    sta $da
lba5c
    lda #GREEN_YELLOW|$e
    sta COLUP1
    lda $e8
    lsr
    bcc lba70
    lda #$3a
    sta $d6
    lda $d7
    sec
    sbc #$02
    sta $d7
lba70
    lda $e8
    lsr
    bcs lba80
    lda #$64
    sta $d6
    lda $d7
    clc
    adc #$02
    sta $d7
lba80
    lda $d7
    cmp #$26
    bne lbaa8
    sta $ee
    lda #$ba
    pha
    lda #$9d
    pha
    lda #$f5
    pha
    lda #$17
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    lda $e8
    and #$fe
    sta $e8
    lda #$00
    sta $8f
lbaa8
    lda $d7
    cmp #$64
    bne lbacc
    sta $ee
    lda #$ba
    pha
    lda #$c5
    pha
    lda #$f5
    pha
    lda #$44
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    lda $e8
    ora #$01
    sta $e8
lbacc
    lda #$14
    cmp ram_DC
    bcs lbad6
    lda #$08
    sta $0c
lbad6
    lda $e8
    lsr
    bcs lbade
    jmp lbaec
lbade
    lda $d7
    cmp #$40
    bcs lbaec
    lda #$00
    cmp $8f
    bcs lbaec
    dec $8f
lbaec
    lda $e8
    lsr
    bcc lbaf4
    jmp lbafc
lbaf4
    lda $8f
    cmp #$12
    bcs lbafc
    inc $8f
lbafc
    jmp Lffdd
    lda #CYAN
    sta COLUP1
    lda #$30
    sta $05
    lda #$06
    sta ram_87
    lda $e8
    and #$02
    beq lbb14
    jmp lbbf1
lbb14
    lda $e7
    cmp #$00
    bne lbb20
    lda $e8
    ora #$04
    sta $e8
lbb20
    lda $e8
    and #$04
    beq lbb50
    ldx #$01
    ldy #$02
    lda #$00
    sta $ee
    lda #$bb
    pha
    lda #$43
    pha
    lda #$f2
    pha
    lda #$b8
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$08
    jmp Lffeb
    lda $e8
    and #$fb
    sta $e8
    lda $e8
    ora #$02
    sta $e8
lbb50
    lda #$00
    cmp $e7
    bcc lbb59
    jmp lbbf1
lbb59
    lda ram_DC
    cmp #$00
    bne lbb62
    jmp lbb68
lbb62
    lda ram_DC
    cmp #$14
    bne lbb80
lbb68
    lda #$05
    sta $1a
    lda #$0d
    sta $16
    lda #$11
    sta $18
    ldx #$6b
    stx $8c
    lda #$f8
    sta $8d
    lda #$07
    sta $8f
lbb80
    lda ram_DC
    cmp #$0a
    bne lbb9e
    lda #$05
    sta $1a
    lda #$0d
    sta $16
    lda #$11
    sta $18
    ldx #$73
    stx $8c
    lda #$f8
    sta $8d
    lda #$07
    sta $8f
lbb9e
    lda ram_DC
    cmp #$1e
    bne lbbbc
    lda #$05
    sta $1a
    lda #$06
    sta $16
    lda #$11
    sta $18
    ldx #$7b
    stx $8c
    lda #$f8
    sta $8d
    lda #$07
    sta $8f
lbbbc
    lda #$20
    sta $d7
    lda $e8
    lsr
    bcc lbbcb
    dec $d6
    lda #$08
    sta $0c
lbbcb
    lda $e8
    lsr
    bcs lbbd2
    inc $d6
lbbd2
    lda #$29
    cmp $d6
    bcc lbbe3
    inc $d7
    lda $e8
    and #$fe
    sta $e8
    jmp lbbf1
lbbe3
    lda $d6
    cmp #$50
    bcc lbbf1
    dec $d7
    lda $e8
    ora #$01
    sta $e8
lbbf1
    lda #$60
    cmp $da
    bcc lbbfd
    lda #$60
    sta $da
    inc $db
lbbfd
    lda $db
    cmp #$48
    bcc lbc09
    lda #$48
    sta $db
    inc $da
lbc09
    lda $da
    cmp #$77
    bcc lbc15
    lda #$77
    sta $da
    dec $db
lbc15
    lda #$36
    cmp $db
    bcc Lbc21
    lda #$36
    sta $db
    dec $da
Lbc21
    jmp   Lffdd                     ;3
    
Lbc24
    lda     ram_E8                  ;3        
    ora     #$10                    ;2        
    sta     ram_E8                  ;3        
    lda     ram_E3                  ;3        
    ora     #$01                    ;2        
    sta     ram_E3                  ;3        
    lda     #$22                    ;2        
    sta     NUSIZ1                  ;3        
    lda     ram_E3                  ;3        
    ora     #$80                    ;2        
    sta     ram_E3                  ;3        
    lda     #$25                    ;2        
    sta     CTRLPF                  ;3        
    lda     #$78                    ;2        
    sta     ram_84                  ;3        
    lda     #$48                    ;2        
    sta     ram_89                  ;3        
    bit     CXP0FB                  ;3        
    bvc     Lbc68                   ;2/3      
    lda     ram_E0                  ;3        
    and     #$02                    ;2        
    beq     Lbc68                   ;2/3      
    lda     ram_E8                  ;3        
    ora     #$01                    ;2        
    sta     ram_E8                  ;3        
    lda     ram_E8                  ;3        
    ora     #$02                    ;2        
    sta     ram_E8                  ;3        
    lda     #$07                    ;2        
    sta     AUDV1                   ;3        
    lda     #$09                    ;2        
    sta     AUDC1                   ;3        
    lda     #$08                    ;2        
    sta     AUDF1                   ;3   =  87
Lbc68
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcs     Lbc75                   ;2/3      
    lda     #$78                    ;2        
    sta     ram_84                  ;3        
    lda     #$48                    ;2        
    sta     ram_89                  ;3   =  17
Lbc75
    lda     ram_E8                  ;3        
    lsr                             ;2        
    bcc     Lbc7e                   ;2/3      
    lda     #$fa                    ;2        
    sta     ram_89                  ;3   =  12
Lbc7e
    lda     ram_E8                  ;3        
    and     #$02                    ;2        
    beq     Lbca8                   ;2/3      
    ldx     #$01                    ;2        
    ldy     #$09                    ;2        
    lda     #$1f                    ;2        
    sta     ram_EE                  ;3        
    lda     #$bc                    ;2        
    pha                             ;3        
    lda     #$a1                    ;2        
    pha                             ;3        
    lda     #$f2                    ;2        
    pha                             ;3        
    lda     #$b8                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  52
    
    lda     ram_E8                  ;3        
    and     #$fd                    ;2        
    sta     ram_E8                  ;3   =   8
Lbca8
    lda     ram_E1                  ;3        
    cmp     #$28                    ;2        
    bcc     Lbcb2                   ;2/3      
    lda     #GREEN                  ;2        
    sta     COLUP1                  ;3   =  12
Lbcb2
    lda     ram_E1                  ;3        
    cmp     #$28                    ;2        
    bne     Lbcd0                   ;2/3      
    sta     ram_EE                  ;3        
    lda     #$bc                    ;2        
    pha                             ;3        
    lda     #$cf                    ;2        
    pha                             ;3        
    lda     #$f5                    ;2        
    pha                             ;3        
    lda     #$53                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  46
    
Lbcd0
    lda     ram_E1                  ;3        
    cmp     #$28                    ;2        
    bcs     Lbcda                   ;2/3      
    lda     #BLACK                  ;2        
    sta     COLUP1                  ;3   =  12
Lbcda
    bit     CXPPMM                  ;3        
    bpl     Lbd18                   ;2/3!     
    lda     ram_E0                  ;3        
    ora     #$02                    ;2        
    sta     ram_E0                  ;3        
    lda     #$34                    ;2        
    sta     ram_E1                  ;3        
    sta     ram_EE                  ;3        
    lda     #$bc                    ;2        
    pha                             ;3        
    lda     #$ff                    ;2        
    pha                             ;3        
    lda     #$f5                    ;2        
    pha                             ;3        
    lda     #$62                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$08                    ;2        
    jmp     Lffeb                   ;3   =  57
    
    sta     ram_EE                  ;3        
    lda     #$bd                    ;2        
    pha                             ;3        
    lda     #$17                    ;2        
    pha                             ;3        
    lda     #$da                    ;2        
    pha                             ;3        
    lda     #$41                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  39
    
Lbd18
    jmp     Lffdd                   ;3   =   3
    
    lda #GREEN|$8
    sta COLUP1
    lda $eb
    and #$04
    beq lbd43
    sta $ee
    lda #$bd
    pha
    lda #$3c
    pha
    lda #$d9
    pha
    lda #$ed
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$07
    jmp Lffeb
    lda $e0
    ora #$01
    sta $e0
lbd43
	lda $eb
    and #$04
    bne lbd4f
    lda $e0
    and #$fe
    sta $e0
lbd4f
	lda $e0
    and #$02
    beq lbd73
    lda $d5
    cmp #$28
    bcs lbd73
    sta $ee
    lda #$bd
    pha
    lda #$72
    pha
    lda #$da
    pha
    lda #$41
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$07
    jmp Lffeb
lbd73
	lda #$54
    cmp $84
    bcs lbd80
    lda #$54
    sta $84
    jmp lbd8d
lbd80
	lda $84
    cmp #$48
    bcs lbd8d
    lda #$48
    sta $84
    jmp lbd8d
lbd8d
	lda $d4
    cmp $84
    bcs lbd98
    dec $84
    jmp lbda3
lbd98
	lda $84
    cmp $d4
    bcs lbda3
    inc $84
    jmp lbda3
lbda3
	lda #$04
    sta $92
    lda $e0
    and #$04
    beq lbdb1
    lda #$08
    sta $0b
lbdb1
	lda $e8
    ora #$10
    sta $e8
    lda $e3
    ora #$80
    sta $e3
    lda #$31
    sta $0a
    lda #$36
    sta $05
    inc $e2
    lda #$50
    cmp $e2
    bcs lbdd1
    lda #$00
    sta $e2
lbdd1
	lda $e2
    cmp #$00
    bne lbdef
    sta $ee
    lda #$bd
    pha
    lda #$ee
    pha
    lda #$db
    pha
    lda #$80
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$07
    jmp Lffeb
lbdef
	lda $e2
    cmp #$28
    bne lbe0d
    sta $ee
    lda #$be
    pha
    lda #$0c
    pha
    lda #$db
    pha
    lda #$8f
    pha
    lda $ee
    pha
    txa
    pha
    ldx #$07
    jmp Lffeb
lbe0d
	lda $d5
    cmp #$28
    bcs lbe1c
    lda $eb
    and #$fb
    sta $eb
    jmp Lbe83
lbe1c
	lda $d5
    cmp #$28
    bcc lbe28
    lda $eb
    ora #$04
    sta $eb
lbe28
	lda $d5
    cmp #$30
    bcs lbe37
    lda #$10
    bit $0280
    beq lbe37
    inc $d5
lbe37
	lda $d4
    cmp #$17
    bcs lbe41
    lda #$17
    sta $d4
lbe41
	lda #$83
    cmp $d4
    bcs lbe4b
    lda #$83
    sta $d4
lbe4b
	bit $0280
    bvs lbe58
    dec $d4
    lda $e0
    ora #$04
    sta $e0
lbe58
	bit $0280
    bmi lbe65
    inc $d4
    lda $e0
    and #$fb
    sta $e0
lbe65
	lda #$10
    bit $0280
    bne lbe74
    lda #$29
    cmp $d5
    bcs lbe74
    dec $d5
lbe74
	lda #$20
    bit $0280
    bne Lbe83
    lda $d5
    cmp #$50
    bcs Lbe83
    inc $d5
Lbe83
    jmp   Lffdd                   ; $be83 (*)

  IF PLUSROM = 1

SendPlusROMScore
       lda $93                       ; Score Hi BCD
       sta WriteToBuffer             ; 
       lda $94                       ; Score Mid BCD
       sta WriteToBuffer             ; 
       lda $95                       ; Score Lo BCD
       sta WriteToBuffer             ; 
       lda #HIGHSCORE_ID             ; game id in Highscore DB
       sta WriteSendBuffer
    jmp     Lffeb                   ;3   =  39

    ORG     $5fd0, $ff
    RORG    $bfd0
  ELSE
    ORG     $5fd4, $ff
    RORG    $bfd4
  ENDIF

Start_b5
    ldx #$ff
    txs
    lda #$f2
    pha
    lda #$4f
    pha
    
Lbfdd
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    tsx                             ;2        
    lda     CXM0FB,x                ;4        
    rol                             ;2        
    rol                             ;2        
    rol                             ;2        
    rol                             ;2        
    and     #$07                    ;2        
    tax                             ;2        
    inx                             ;2   =  28
Lbfeb
    lda     $1ff3,x                 ;4        
    pla                             ;4        
    tax                             ;2        
    pla                             ;4        
    rts                             ;6   =  20
    
    ORG     $5ff3, $ff
    RORG    $bff3

Lbff3
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff     ; $bff3 (*)

  IF PLUSROM = 1
    .word (PlusROM_API - $6000)             ; PlusRom API pointer
  ELSE
    .byte   $ff,$ff                         ; $bffa (D)
  ENDIF

    .word Start_b5, Start_b5


;***********************************************************
;      Bank 6 / 0..7
;***********************************************************

    SEG     CODE
    ORG     $6000
    RORG    $d000

    ldx     #$2b                    ;2        
    jmp     Ld031                   ;3   =   5
    
Ld005
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $d005 (D)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $d00d (D)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $d015 (D)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $d01d (D)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $d025 (D)
    .byte   $00,$00,$00,$00                 ; $d02d (D)
    
Ld031
    lda     Ld005,x                 ;4        
    sta     ram_A4,x                ;4        
    dex                             ;2        
    bpl     Ld031                   ;2/3      
    jmp     Lffdd                   ;3   =  15

    ldx #$2b
    jmp ld06d

Ld041
    .byte   $fc,$00,$00                     ; $d03c (*)
    .byte   $fc,$88,$00,$00,$00,$88,$fc,$ff ; $d044 (*)
    .byte   $ff,$88,$00,$00,$84,$8f,$ff,$f0 ; $d04c (*)
    .byte   $84,$8c,$00,$00,$84,$8c,$f8,$ff ; $d054 (*)
    .byte   $87,$8c,$00,$00,$84,$8f,$ff,$f0 ; $d05c (*)
    .byte   $ff,$80,$00,$00,$04,$ff,$ff,$ff ; $d064 (*)
    .byte   $07                             ; $d06c (*)
ld06d
    lda Ld041,x
    sta ram_A4,x
    dex
    bpl ld06d
    jmp Lffdd
    ldx #$2b
    jmp ld0a9

Ld07d
    .byte   $80,$00,$00,$80,$80,$00,$00     ; $d07d (*)
    .byte   $80,$80,$00,$00,$80,$f8,$00,$00 ; $d084 (*)
    .byte   $80,$80,$00,$00,$80,$80,$00,$00 ; $d08c (*)
    .byte   $80,$80,$00,$00,$80,$80,$00,$00 ; $d094 (*)
    .byte   $80,$80,$00,$00,$80,$f8,$00,$00 ; $d09c (*)
    .byte   $80,$f8,$00,$00,$80             ; $d0a4 (*)

ld0a9
    lda Ld07d,x
    sta ram_A4,x
    dex
    bpl ld0a9
    jmp Lffdd
    ldx #$2b
    jmp ld0e5

Ld0b9
    .byte   $80,$00,$00                     ; $d0b4 (*)
    .byte   $80,$80,$00,$00,$80,$80,$00,$00 ; $d0bc (*)
    .byte   $80,$80,$00,$00,$80,$80,$00,$00 ; $d0c4 (*)
    .byte   $80,$80,$00,$00,$80,$80,$00,$00 ; $d0cc (*)
    .byte   $80,$80,$00,$00,$80,$80,$00,$00 ; $d0d4 (*)
    .byte   $80,$f0,$00,$00,$f0,$ff,$ff,$ff ; $d0dc (*)
    .byte   $ff                             ; $d0e4 (*)
ld0e5
    lda Ld0b9,x
    sta ram_A4,x
    dex
    bpl ld0e5
    jmp Lffdd
    ldx #$2b
    jmp ld121

Ld0f5
    .byte   $80,$00,$00,$80,$80,$00,$00     ; $d0f5 (*)
    .byte   $80,$80,$e0,$e0,$80,$80,$00,$00 ; $d0fc (*)
    .byte   $80,$80,$00,$00,$80,$80,$00,$00 ; $d104 (*)
    .byte   $80,$80,$00,$00,$80,$ff,$f9,$f3 ; $d10c (*)
    .byte   $ff,$00,$00,$00,$00,$ff,$ff,$ff ; $d114 (*)
    .byte   $ff,$ff,$ff,$ff,$ff             ; $d11c (*)
ld121
    lda Ld0f5,x
    sta ram_A4,x
    dex
    bpl ld121
    jmp Lffdd
    lda #$00
    sta $e6
    ldx #$83
    stx $8c
    lda #$f8
    sta $8d
    lda #$0f
    sta $8f
    ldx #$2b
    jmp ld16d

Ld141
    .byte   $80,$00,$00                     ; $d141 (*)
    .byte   $80,$80,$00,$00,$80,$80,$00,$00 ; $d144 (*)
    .byte   $80,$80,$00,$00,$80,$80,$00,$00 ; $d14c (*)
    .byte   $80,$80,$00,$01,$9f,$80,$00,$00 ; $d154 (*)
    .byte   $86,$ff,$ff,$fe,$ef,$00,$00,$00 ; $d15c (*)
    .byte   $00,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $d164 (*)
    .byte   $ff                             ; $d16c (*)

ld16d
    lda Ld141,x
    sta ram_A4,x
    dex
    bpl ld16d
    jmp Lffdd
    ldx #$2b
    jmp ld1a9

Ld17d
    .byte   $00,$00,$00,$00,$00,$00,$00     ; $d17d (*)
    .byte   $00,$88,$14,$e0,$00,$88,$14,$90 ; $d184 (*)
    .byte   $00,$ab,$15,$97,$e6,$aa,$15,$97 ; $d18c (*)
    .byte   $aa,$ab,$15,$95,$ea,$aa,$14,$95 ; $d194 (*)
    .byte   $2a,$bb,$15,$97,$ea,$60,$00,$e0 ; $d19c (*)
    .byte   $00,$00,$00,$00,$00             ; $d1a4 (*)
ld1a9
    lda Ld17d,x
    sta ram_A4,x
    dex
    bpl ld1a9
    jmp Lffdd
    ldx #$2b
    jmp ld1e5

Ld1b9
    .byte   $80,$00,$00                     ; $d1b9 (*)
    .byte   $80,$80,$00,$00,$80,$80,$fe,$ff ; $d1bc (*)
    .byte   $87,$80,$02,$00,$80,$83,$03,$00 ; $d1c4 (*)
    .byte   $80,$f0,$02,$00,$80,$80,$c2,$ff ; $d1cc (*)
    .byte   $ff,$80,$02,$00,$80,$80,$02,$00 ; $d1d4 (*)
    .byte   $00,$80,$02,$00,$e0,$80,$fe,$ff ; $d1dc (*)
    .byte   $ff                             ; $d1e4 (*)
ld1e5
    lda Ld1b9,x
    sta ram_A4,x
    dex
    bpl ld1e5
    jmp     Lffdd                    ;3
    
Ld1f0
    ldx     #$2b                    ;2        
    jmp     Ld221                   ;3   =   5
    
Ld1f5
    .byte   $80,$00,$00,$80,$80,$00,$00,$80 ; $d1f5 (D)
    .byte   $80,$00,$00,$80,$80,$00,$00,$80 ; $d1fd (D)
    .byte   $80,$00,$00,$80,$80,$00,$00,$80 ;$d205 (D)
    .byte   $80,$00,$00,$80,$80,$00,$00,$80 ; $d20d (D)
    .byte   $80,$c0,$80,$80,$80,$80,$00,$80 ; $d215 (D)
    .byte   $ff,$83,$07,$ff                 ; $d21d (D)
    
Ld221
    lda     Ld1f5,x                 ;4        
    sta     ram_A4,x                ;4        
    dex                             ;2        
    bpl     Ld221                   ;2/3      
    jmp     Lffdd                   ;3   =  15
    
    ldx     #$2b                    ;2        
    jmp     Ld25d                   ;3   =   5
    
Ld231
    .byte   $80,$c0,$00,$80,$80,$c0,$00,$00 ; $d231 (D)
    .byte   $e7,$c1,$ff,$f9,$83,$00,$01,$81 ; $d239 (D)
    .byte   $83,$00,$01,$9f,$83,$00,$00,$80 ; $d241 (D)
    .byte   $83,$ff,$ff,$ff,$80,$00,$00,$80 ; $d249 (D)
    .byte   $80,$00,$00,$80,$80,$00,$00,$80 ; $d251 (D)
    .byte   $83,$ff,$ff,$ff                 ; $d259 (D)
    
Ld25d
    lda     Ld231,x                 ;4        
    sta     ram_A4,x                ;4        
    dex                             ;2        
    bpl     Ld25d                   ;2/3      
    jmp     Lffdd                   ;3   =  15
    
    ldx     #$2b                    ;2        
    jmp     Ld299                   ;3   =   5
    
Ld26d
    .byte   $80,$00,$00,$80,$80,$00,$00,$80 ; $d26d (D)
    .byte   $80,$00,$00,$80,$80,$00,$00,$00 ; $d275 (D)
    .byte   $fe,$c0,$01,$ff,$80,$00,$00,$80 ; $d27d (D)
    .byte   $80,$00,$00,$80,$80,$00,$00,$80 ; $d285 (D)
    .byte   $80,$00,$00,$80,$80,$00,$00,$80 ; $d28d (D)
    .byte   $80,$00,$00,$80                 ; $d295 (D)
    
Ld299
    lda     Ld26d,x                 ;4        
    sta     ram_A4,x                ;4        
    dex                             ;2        
    bpl     Ld299                   ;2/3      
    jmp     Lffdd                   ;3   =  15
    
    ldx     #$2b                    ;2        
    jmp     Ld2d5                   ;3   =   5
    
Ld2a9
    .byte   $80,$00,$00,$ff,$80,$00,$00,$80 ; $d2a9 (D)
    .byte   $80,$00,$00,$8f,$80,$00,$00,$83 ; $d2b1 (D)
    .byte   $ff,$ff,$ff,$e3,$80,$00,$00,$80 ; $d2b9 (D)
    .byte   $86,$00,$00,$80,$87,$ef,$df,$ff ; $d2c1 (D)
    .byte   $86,$00,$00,$80,$80,$00,$00,$00 ; $d2c9 (D)
    .byte   $ff,$ff,$ff,$ff                 ; $d2d1 (D)
    
Ld2d5
    lda     Ld2a9,x                 ;4        
    sta     ram_A4,x                ;4        
    dex                             ;2        
    bpl     Ld2d5                   ;2/3      
    jmp     Lffdd                   ;3   =  15
    
    ldx     #$2b                    ;2        
    jmp     Ld311                   ;3   =   5
    
Ld2e5
    .byte   $80,$00,$00,$b0,$80,$00,$00,$b0 ; $d2e5 (D)
    .byte   $f0,$fe,$ff,$bc,$80,$02,$00,$80 ; $d2ed (D)
    .byte   $81,$e3,$ff,$ff,$80,$02,$00,$80 ; $d2f5 (D)
    .byte   $f0,$fe,$ff,$87,$80,$02,$00,$80 ; $d2fd (D)
    .byte   $81,$e3,$ff,$ff,$80,$02,$00,$80 ; $d305 (D)
    .byte   $f0,$ee,$ff,$ff                 ; $d30d (D)
    
Ld311
    lda     Ld2e5,x                 ;4        
    sta     ram_A4,x                ;4        
    dex                             ;2        
    bpl     Ld311                   ;2/3      
    jmp     Lffdd                   ;3   =  15
    
    ldx     #$2b                    ;2        
    jmp     Ld34d                   ;3   =   5
    
Ld321
    .byte   $83,$00,$00,$80,$80,$00,$00,$80 ; $d321 (D)
    .byte   $80,$00,$00,$80,$80,$00,$00,$80 ; $d329 (D)
    .byte   $80,$fe,$fc,$80,$80,$c0,$80,$80 ; $d331 (D)
    .byte   $80,$c0,$80,$80,$e0,$c0,$80,$e0 ; $d339 (D)
    .byte   $80,$80,$00,$80,$80,$80,$00,$80 ; $d341 (D)
    .byte   $ff,$8f,$1f,$ff                 ; $d349 (D)
    
Ld34d
    lda     Ld321,x                 ;4        
    sta     ram_A4,x                ;4        
    dex                             ;2        
    bpl     Ld34d                   ;2/3      
    jmp     Lffdd                   ;3   =  15
    
    ldx     #$2b                    ;2        
    jmp     Ld389                   ;3   =   5
    
Ld35d
    .byte   $87,$e7,$e7,$87,$84,$00,$00,$04 ; $d35d (D)
    .byte   $8e,$00,$00,$8e,$80,$00,$00,$80 ; $d365 (D)
    .byte   $80,$00,$00,$80,$80,$00,$00,$80 ; $d36d (D)
    .byte   $81,$ff,$ff,$81,$81,$ff,$7f,$81 ; $d375 (D)
    .byte   $87,$43,$c3,$87,$81,$c0,$40,$81 ; $d37d (D)
    .byte   $f1,$40,$c0,$81                 ; $d385 (D)
    
Ld389
    lda     Ld35d,x                 ;4        
    sta     ram_A4,x                ;4        
    dex                             ;2        
    bpl     Ld389                   ;2/3      
    jmp     Lffdd                   ;3   =  15
    
    ldx     #$2b                    ;2        
    jmp     Ld3c5                   ;3   =   5
    
Ld399
    .byte   $80,$00,$00,$80,$80,$00,$00,$80 ; $d399 (D)
    .byte   $80,$00,$00,$80,$80,$00,$00,$00 ; $d3a1 (D)
    .byte   $fc,$00,$00,$fc,$80,$1c,$38,$80 ; $d3a9 (D)
    .byte   $80,$08,$10,$80,$80,$08,$10,$80 ; $d3b1 (D)
    .byte   $80,$08,$10,$80,$80,$08,$10,$80 ; $d3b9 (D)
    .byte   $80,$08,$10,$80                 ; $d3c1 (D)
    
Ld3c5
    lda     Ld399,x                 ;4        
    sta     ram_A4,x                ;4        
    dex                             ;2        
    bpl     Ld3c5                   ;2/3      
    jmp     Lffdd                   ;3   =  15

    ldx #$2b
    jmp Ld401
   
Ld3d5
    .byte   $8e,$00,$00                     ; $d3d5 (*)
    .byte   $80,$8a,$03,$00,$80,$8e,$00,$00 ; $d3d8 (*)
    .byte   $80,$8f,$ff,$ff,$81,$80,$00,$00 ; $d3e0 (*)
    .byte   $80,$ff,$ff,$ff,$8f,$80,$00,$00 ; $d3e8 (*)
    .byte   $80,$8f,$ff,$ff,$ff,$80,$00,$00 ; $d3f0 (*)
    .byte   $80,$80,$00,$00,$00,$ff,$cc,$99 ; $d3f8 (*)
    .byte   $ff                             ; $d400 (*)
Ld401
    lda Ld3d5,x
    sta ram_A4,x
    dex
    bpl Ld401
    jmp     Lffdd                    ;3
    
Ld40c
    ldx     #$2b                    ;2        
    jmp     Ld43d                   ;3   =   5
    
Ld411
    .byte   $80,$00,$00,$80,$80,$00,$00,$80 ; $d411 (D)
    .byte   $80,$00,$00,$80,$80,$e0,$00,$80 ; $d419 (D)
    .byte   $83,$41,$00,$9c,$81,$40,$0e,$88 ; $d421 (D)
    .byte   $e1,$40,$04,$88,$81,$40,$04,$08 ; $d429 (D)
    .byte   $87,$40,$04,$e8,$81,$40,$04,$48 ; $d431 (D)
    .byte   $f1,$40,$04,$48                 ; $d439 (D)
    
Ld43d
    lda     Ld411,x                 ;4        
    sta     ram_A4,x                ;4        
    dex                             ;2        
    bpl     Ld43d                   ;2/3      
    jmp     Lffdd                   ;3   =  15
    
    ldx     #$2b                    ;2        
    jmp     Ld479                   ;3
 
Ld44d
    .byte   $80,$00,$00                     ; $d44d (*)
    .byte   $80,$80,$00,$00,$80,$80,$00,$00 ; $d450 (*)
    .byte   $80,$f8,$00,$70,$80,$20,$0e,$20 ; $d458 (*)
    .byte   $9c,$20,$04,$20,$88,$20,$04,$20 ; $d460 (*)
    .byte   $88,$20,$04,$20,$88,$20,$04,$20 ; $d468 (*)
    .byte   $88,$20,$04,$20,$08,$20,$04,$20 ; $d470 (*)
    .byte   $e8                             ; $d478 (*)
Ld479
    lda Ld44d,x
    sta ram_A4,x
    dex
    bpl Ld479
    jmp     Lffdd                    ;3
    
Ld484
    ldx     #$2b                    ;2        
    jmp     Ld4b5                   ;3   =   5
    
Ld489
    .byte   $f0,$0c,$0c,$f0,$10,$0c,$0c,$10 ; $d489 (D)
    .byte   $1f,$0f,$0f,$1f,$10,$00,$00,$10 ; $d491 (D)
    .byte   $f0,$00,$00,$f0,$80,$00,$00,$80 ; $d499 (D)
    .byte   $80,$00,$00,$80,$e0,$00,$00,$e0 ; $d4a1 (D)
    .byte   $c0,$00,$00,$c0,$c0,$00,$00,$c0 ; $d4a9 (D)
    .byte   $ff,$e1,$e1,$ff                 ; $d4b1 (D)
    
Ld4b5
    lda     Ld489,x                 ;4        
    sta     ram_A4,x                ;4        
    dex                             ;2        
    bpl     Ld4b5                   ;2/3      
    jmp     Lffdd                   ;3   =  15
    
    ldx     #$2b                    ;2        
    jmp     Ld4f1                   ;3   =   5
    
Ld4c5
    .byte   $ff,$00,$00,$80,$81,$00,$00,$80 ; $d4c5 (D)
    .byte   $81,$00,$00,$80,$81,$e0,$ff,$ff ; $d4cd (D)
    .byte   $e1,$03,$00,$80,$81,$03,$00,$80 ; $d4d5 (D)
    .byte   $87,$ff,$ff,$87,$80,$00,$00,$83 ; $d4dd (D)
    .byte   $e0,$00,$00,$e3,$80,$00,$00,$80 ; $d4e5 (D)
    .byte   $ff,$c7,$c7,$ff                 ; $d4ed (D)
    
Ld4f1
    lda     Ld4c5,x                 ;4        
    sta     ram_A4,x                ;4        
    dex                             ;2        
    bpl     Ld4f1                   ;2/3      
    jmp     Lffdd                   ;3   =  15
    
    ldx     #$2b                    ;2        
    jmp     Ld52d                   ;3   =   5
    
Ld501
    .byte   $80,$00,$00,$80,$80,$00,$00,$80 ; $d501 (D)
    .byte   $80,$00,$00,$80,$80,$00,$38,$00 ; $d509 (D)
    .byte   $f8,$00,$10,$f8,$20,$1c,$10,$20 ; $d511 (D)
    .byte   $20,$08,$10,$20,$20,$08,$10,$20 ; $d519 (D)
    .byte   $20,$08,$10,$20,$20,$08,$10,$20 ; $d521 (D)
    .byte   $20,$08,$10,$20                 ; $d529 (D)
    
Ld52d
    lda     Ld501,x                 ;4        
    sta     ram_A4,x                ;4        
    dex                             ;2        
    bpl     Ld52d                   ;2/3      
    jmp     Lffdd                   ;3   =  15
    
    ldx     #$2b                    ;2        
    jmp     Ld569                   ;3   =   5
    
Ld53d
    .byte   $80,$00,$00,$80,$80,$00,$00,$80 ; $d53d (D)
    .byte   $80,$00,$00,$80,$80,$00,$00,$00 ; $d545 (D)
    .byte   $f1,$81,$81,$f1,$20,$00,$00,$20 ; $d54d (D)
    .byte   $20,$00,$00,$20,$20,$00,$00,$20 ; $d555 (D)
    .byte   $20,$00,$00,$20,$20,$00,$00,$20 ; $d55d (D)
    .byte   $20,$00,$00,$20                 ; $d565 (D)
    
Ld569
    lda     Ld53d,x                 ;4        
    sta     ram_A4,x                ;4        
    dex                             ;2        
    bpl     Ld569                   ;2/3      
    jmp     Lffdd                   ;3   =  15
    
    ldx     #$2b                    ;2        
    jmp     Ld5a5                   ;3   =   5
    
Ld579
    .byte   $80,$00,$00,$80,$80,$00,$00,$00 ; $d579 (D)
    .byte   $e0,$ff,$ff,$ff,$80,$00,$00,$80 ; $d581 (D)
    .byte   $8e,$f0,$f0,$80,$80,$10,$10,$80 ; $d589 (D)
    .byte   $83,$17,$17,$83,$83,$16,$16,$83 ; $d591 (D)
    .byte   $8f,$f6,$f6,$8f,$80,$06,$06,$80 ; $d599 (D)
    .byte   $80,$06,$06,$f0                 ; $d5a1 (D)
    
Ld5a5
    lda     Ld579,x                 ;4        
    sta     ram_A4,x                ;4        
    dex                             ;2        
    bpl     Ld5a5                   ;2/3      
    jmp     Lffdd                   ;3   =  15
    
    ldx     #$2b                    ;2        
    jmp     Ld5e1                   ;3   =   5
    
Ld5b5
    .byte   $80,$00,$00,$80,$80,$00,$00,$80 ; $d5b5 (D)
    .byte   $80,$00,$00,$80,$80,$00,$00,$80 ; $d5bd (D)
    .byte   $83,$ff,$ff,$81,$80,$00,$00,$80 ; $d5c5 (D)
    .byte   $e0,$00,$00,$80,$e0,$00,$00,$80 ; $d5cd (D)
    .byte   $83,$ff,$ff,$81,$80,$e0,$c0,$80 ; $d5d5 (D)
    .byte   $f8,$80,$00,$f8                 ; $d5dd (D)
    
Ld5e1
    lda     Ld5b5,x                 ;4        
    sta     ram_A4,x                ;4        
    dex                             ;2        
    bpl     Ld5e1                   ;2/3      
    jmp     Lffdd                   ;3   =  15
    
    ldx     #$2b                    ;2        
    jmp     Ld61d                   ;3   =   5
    
Ld5f1
    .byte   $80,$00,$00,$80,$80,$00,$00,$80 ; $d5f1 (D)
    .byte   $80,$00,$00,$80,$e0,$00,$00,$e0 ; $d5f9 (D)
    .byte   $80,$07,$e0,$87,$80,$00,$00,$80 ; $d601 (D)
    .byte   $80,$00,$00,$80,$80,$00,$00,$80 ; $d609 (D)
    .byte   $80,$00,$00,$80,$80,$00,$00,$80 ; $d611 (D)
    .byte   $80,$00,$00,$80                 ; $d619 (D)
    
Ld61d
    lda     Ld5f1,x                 ;4        
    sta     ram_A4,x                ;4        
    dex                             ;2        
    bpl     Ld61d                   ;2/3      
    jmp     Lffdd                   ;3   =  15
    
    .byte   $a2,$2b,$4c,$59,$d6,$80,$00,$00 ; $d628 (*)
    .byte   $80,$80,$00,$00,$80,$80,$00,$00 ; $d630 (*)
    .byte   $80,$80,$00,$00,$80,$80,$00,$00 ; $d638 (*)
    .byte   $80,$80,$00,$00,$80,$80,$00,$00 ; $d640 (*)
    .byte   $80,$ff,$ff,$f0,$fc,$82,$08,$20 ; $d648 (*)
    .byte   $80,$82,$08,$26,$80,$82,$08,$26 ; $d650 (*)
    .byte   $ff,$bd,$2d,$d6,$95,$a4,$ca,$10 ; $d658 (*)
    .byte   $f8
    jmp     Lffdd                    ;3
    .byte $a2,$2b,$4c,$95 ; $d660 (*)
    .byte   $d6,$44,$00,$00,$80,$4e,$00,$00 ; $d668 (*)
    .byte   $80,$40,$00,$00,$00,$40,$00,$00 ; $d670 (*)
    .byte   $fe,$40,$00,$7f,$83,$f1,$ff,$c0 ; $d678 (*)
    .byte   $80,$80,$00,$00,$80,$ff,$03,$00 ; $d680 (*)
    .byte   $80,$80,$fe,$ff,$f1,$80,$00,$00 ; $d688 (*)
    .byte   $40,$ff,$ff,$ff,$ff,$bd,$69,$d6 ; $d690 (*)
    .byte   $95,$a4,$ca,$10,$f8
    jmp     Lffdd                    ;3
    .byte   $a2,$2b,$4c,$d1,$d6,$80,$00,$00 ; $d6a0 (*)
    .byte   $80,$80,$00,$00,$80,$80,$00,$00 ; $d6a8 (*)
    .byte   $80,$ff,$ff,$ff,$83,$c0,$00,$00 ; $d6b0 (*)
    .byte   $e0,$80,$00,$00,$40,$c0,$f8,$ff ; $d6b8 (*)
    .byte   $ff,$fc,$00,$00,$80,$80,$00,$00 ; $d6c0 (*)
    .byte   $80,$ff,$ff,$c0,$80,$00,$07,$01 ; $d6c8 (*)
    .byte   $ff,$bd,$a5,$d6,$95,$a4,$ca,$10 ; $d6d0 (*)
    .byte   $f8
    jmp     Lffdd                    ;3
    .byte $a9,$58,$85,$d7 ; $d6d8 (*)
    .byte   $a9,$00,$85,$08,$a9,$f8,$85,$f1 ; $d6e0 (*)
    .byte   $a9,$3f,$85,$f0,$a2,$2b,$4c,$1d ; $d6e8 (*)
    .byte   $d7,$80,$00,$00,$80,$80,$00,$00 ; $d6f0 (*)
    .byte   $80,$80,$00,$00,$80,$80,$00,$00 ; $d6f8 (*)
    .byte   $80,$80,$00,$00,$80,$80,$00,$00 ; $d700 (*)
    .byte   $00,$f0,$c3,$c6,$f8,$00,$00,$00 ; $d708 (*)
    .byte   $00,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $d710 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$bd,$f1,$d6 ; $d718 (*)
    .byte   $95,$a4,$ca,$10,$f8,$a2,$bf,$86 ; $d720 (*)
    .byte   $8c,$a9,$f8,$85,$8d,$a9,$17,$85 ; $d728 (*)
    .byte   $8f
    jmp     Lffdd                    ;3
    .byte $a2,$2b,$4c,$65 ; $d730 (*)
    .byte   $d7,$ff,$ff,$ff,$ff,$9f,$ff,$ff ; $d738 (*)
    .byte   $ff,$9f,$ff,$ff,$ff,$9f,$ff,$ff ; $d740 (*)
    .byte   $ff,$9f,$ff,$ff,$ff,$9f,$ff,$ff ; $d748 (*)
    .byte   $ff,$9f,$ff,$ff,$ff,$80,$00,$00 ; $d750 (*)
    .byte   $80,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $d758 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$bd,$39,$d7 ; $d760 (*)
    .byte   $95,$a4,$ca,$10,$f8
    jmp     Lffdd                    ;3
    .byte   $a2,$2b,$4c,$a1,$d7,$81,$80,$80 ; $d770 (*)
    .byte   $81,$81,$f8,$f8,$81,$81,$18,$18 ; $d778 (*)
    .byte   $01,$f1,$ff,$ff,$f1,$00,$00,$00 ; $d780 (*)
    .byte   $00,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $d788 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $d790 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $d798 (*)
    .byte   $ff,$bd,$75,$d7,$95,$a4,$ca,$10 ; $d7a0 (*)
    .byte   $f8
    jmp     Lffdd                    ;3
    .byte $a2,$2b,$4c,$dd ; $d7a8 (*)
    .byte   $d7,$c0,$f8,$fc,$c0,$c0,$3c,$1e ; $d7b0 (*)
    .byte   $c0,$c0,$1e,$0f,$cc,$c0,$1e,$0f ; $d7b8 (*)
    .byte   $c0,$c0,$1e,$0f,$c0,$ff,$ff,$ff ; $d7c0 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $d7c8 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $d7d0 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$bd,$b1,$d7 ; $d7d8 (*)
    .byte   $95,$a4,$ca,$10,$f8
    jmp     Lffdd                    ;3
    
Ld7e8
    lda     #$28                    ;2        
    sta     ram_D6                  ;3        
    lda     #$3f                    ;2        
    sta     ram_D7                  ;3        
    lda     ram_E8                  ;3        
    ora     #$01                    ;2        
    sta     ram_E8                  ;3        
    sta     ram_EE                  ;3        
    lda     #$d8                    ;2        
    pha                             ;3        
    lda     #$0d                    ;2        
    pha                             ;3        
    lda     #$dd                    ;2        
    pha                             ;3        
    lda     #$90                    ;2        
    pha                             ;3        
    lda     ram_EE                  ;3        
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    ldx     #$07                    ;2        
    jmp     Lffeb                   ;3   =  57
    
    ldx     #$2b                    ;2        
    jmp     Ld83f                   ;3   =   5
    
Ld813
    .byte   $ff,$ff,$ff,$ff,$80,$00,$00,$00 ; $d813 (D)
    .byte   $80,$e0,$ff,$ff,$87,$ff,$02,$40 ; $d81b (D)
    .byte   $87,$e0,$07,$40,$85,$a0,$05,$78 ; $d823 (D)
    .byte   $87,$e0,$07,$08,$80,$00,$00,$08 ; $d82b (D)
    .byte   $87,$e0,$07,$08,$85,$a0,$05,$08 ; $d833 (D)
    .byte   $fd,$bf,$fd,$0f                 ; $d83b (D)
    
Ld83f
    lda     Ld813,x                 ;4        
    sta     ram_A4,x                ;4        
    dex                             ;2        
    bpl     Ld83f                   ;2/3      
    jmp     Lffdd                   ;3   =  15
    
    ldx     #$2b                    ;2        
    jmp     Ld87b                   ;3   =   5
    
Ld84f
    .byte   $83,$ff,$fe,$80,$82,$00,$02,$00 ; $d84f (D)
    .byte   $82,$00,$02,$e0,$82,$e0,$03,$81 ; $d857 (D)
    .byte   $82,$40,$02,$80,$82,$40,$02,$e0 ; $d85f (D)
    .byte   $82,$40,$02,$80,$87,$40,$03,$81 ; $d867 (D)
    .byte   $80,$c0,$80,$e0,$80,$40,$00,$e0 ; $d86f (D)
    .byte   $fd,$63,$00,$e0                 ; $d877 (D)
    
Ld87b
    lda     Ld84f,x                 ;4        
    sta     ram_A4,x                ;4        
    dex                             ;2        
    bpl     Ld87b                   ;2/3      
    jmp     Lffdd                   ;3   =  15
    
    ldx     #$2b                    ;2        
    jmp     Ld8b7                   ;3   =   5
    
Ld88b
    .byte   $88,$04,$01,$80,$88,$04,$03,$81 ; $d88b (D)
    .byte   $88,$0e,$00,$80,$88,$00,$00,$80 ; $d893 (D)
    .byte   $88,$00,$03,$81,$88,$00,$01,$80 ; $d89b (D)
    .byte   $88,$0e,$01,$80,$9c,$04,$01,$80 ; $d8a3 (D)
    .byte   $80,$04,$01,$86,$80,$04,$01,$80 ; $d8ab (D)
    .byte   $fe,$04,$01,$ff                 ; $d8b3 (D)
    
Ld8b7
    lda     Ld88b,x                 ;4        
    sta     ram_A4,x                ;4        
    dex                             ;2        
    bpl     Ld8b7                   ;2/3      
    jmp     Lffdd                   ;3   =  15

    ldx #$2b
    jmp Ld8f3
   
Ld8c7
    .byte   $80,$00,$00                     ; $d8c7 (*)
    .byte   $80,$80,$00,$00,$80,$80,$00,$00 ; $d8ca (*)
    .byte   $80,$81,$7f,$bf,$ff,$f0,$00,$00 ; $d8d2 (*)
    .byte   $80,$e0,$00,$00,$80,$ff,$ff,$ff ; $d8da (*)
    .byte   $ff,$80,$00,$00,$80,$80,$00,$00 ; $d8e2 (*)
    .byte   $80,$80,$00,$00,$80,$fe,$f0,$f8 ; $d8ea (*)
    .byte   $fe                             ; $d8f2 (*)
Ld8f3
    lda Ld8c7,x
    sta ram_A4,x
    dex
    bpl Ld8f3
    jmp     Lffdd                    ;3

    ldx #$2b
    jmp Ld92f

Ld903
    .byte   $ff,$ff,$ff,$ff,$80,$00,$00     ; $d903 (*)
    .byte   $80,$80,$00,$00,$80,$fc,$00,$00 ; $d90a (*)
    .byte   $80,$07,$ff,$c3,$81,$00,$40,$00 ; $d912 (*)
    .byte   $e0,$00,$40,$00,$80,$00,$c0,$c3 ; $d91a (*)
    .byte   $81,$00,$40,$00,$80,$00,$40,$00 ; $d922 (*)
    .byte   $e0,$00,$c0,$c3,$81 ; $d92a (*)
Ld92f
    lda Ld903,x
    sta ram_A4,x
    dex
    bpl Ld92f
    jmp     Lffdd                    ;3
    .byte   $a2,$2b,$4c,$6b,$d9,$80,$00,$01 ; $d93a (*)
    .byte   $80,$80,$00,$01,$80,$80,$00,$01 ; $d942 (*)
    .byte   $80,$ff,$7f,$01,$e0,$10,$00,$01 ; $d94a (*)
    .byte   $83,$10,$00,$39,$80,$f0,$60,$11 ; $d952 (*)
    .byte   $e0,$81,$7f,$1f,$83,$80,$00,$00 ; $d95a (*)
    .byte   $80,$bf,$ff,$ff,$8f,$88,$00,$00 ; $d962 (*)
    .byte   $82,$bd,$3f,$d9,$95,$a4,$ca,$10 ; $d96a (*)
    .byte   $f8
    jmp     Lffdd                    ;3
    .byte $a2,$2b,$4c,$a7 ; $d972 (*)
    .byte   $d9,$80,$08,$00,$80,$80,$1e,$00 ; $d97a (*)
    .byte   $80,$80,$02,$00,$80,$80,$82,$ff ; $d982 (*)
    .byte   $ff,$f0,$0e,$e0,$80,$c0,$0e,$03 ; $d98a (*)
    .byte   $80,$c3,$ff,$ff,$80,$c0,$fe,$ff ; $d992 (*)
    .byte   $83,$f0,$00,$00,$e0,$f0,$00,$00 ; $d99a (*)
    .byte   $80,$ff,$ff,$ff,$ff,$bd,$7b,$d9 ; $d9a2 (*)
    .byte   $95,$a4,$ca,$10,$f8
    jmp     Lffdd                    ;3
    
Ld9b2
    ldx     #$2b                    ;2        
    jmp     Ld9e3                   ;3   =   5
    
Ld9b7
    .byte   $c0,$00,$00,$c0,$c0,$00,$00,$c0 ; $d9b7 (D)
    .byte   $c0,$00,$00,$c0,$c0,$00,$00,$c0 ; $d9bf (D)
    .byte   $f8,$c0,$c0,$f8,$c0,$80,$80,$c0 ; $d9c7 (D)
    .byte   $c0,$80,$80,$c0,$c0,$80,$80,$c0 ; $d9cf (D)
    .byte   $c0,$80,$80,$c0,$c0,$80,$80,$c0 ; $d9d7 (D)
    .byte   $c0,$80,$80,$c0                 ; $d9df (D)
    
Ld9e3
    lda     Ld9b7,x                 ;4        
    sta     ram_A4,x                ;4        
    dex                             ;2        
    bpl     Ld9e3                   ;2/3      
    jmp     Lffdd                   ;3   =  15
    
    ldx #$d7
    stx $8a
    lda #$f8
    sta $8b
    lda #$08
    sta $8e
    jmp     Lffdd                    ;3
    
Ld9fd
    ldx     #$e0                    ;2        
    stx     ram_8A                  ;3        
    lda     #$f8                    ;2        
    sta     ram_8B                  ;3        
    lda     #$07                    ;2        
    sta     ram_8E                  ;3        
    jmp     Lffdd                   ;3   =  18
    
    lda     #$14                    ;2        
    cmp     ram_DC                  ;3        
    bcs     Lda1e                   ;2/3      
    ldx     #$e8                    ;2        
    stx     ram_8A                  ;3        
    lda     #$f8                    ;2        
    sta     ram_8B                  ;3        
    lda     #$08                    ;2        
    sta     ram_8E                  ;3   =  22
Lda1e
    lda     #$14                    ;2        
    cmp     ram_DC                  ;3        
    bcc     Lda30                   ;2/3      
    ldx     #$f1                    ;2        
    stx     ram_8A                  ;3        
    lda     #$f8                    ;2        
    sta     ram_8B                  ;3        
    lda     #$06                    ;2        
    sta     ram_8E                  ;3   =  22
Lda30
    jmp     Lffdd                   ;3   =   3
    
    ldx #$f8
    stx $8a
    lda #$f8
    sta $8b
    lda #$05
    sta $8e
    jmp     Lffdd                    ;3
    
Lda42
    ldx     #$00                    ;2        
    stx     ram_8A                  ;3        
    lda     #$f9                    ;2        
    sta     ram_8B                  ;3        
    lda     #$06                    ;2        
    sta     ram_8E                  ;3        
    jmp     Lffdd                   ;3   =  18
    
    ldx #$07
    stx $8a
    lda #$f9
    sta $8b
    lda #$05
    sta $8e
    jmp     Lffdd                    ;3
    
Lda60
    ldx     #$0d                    ;2        
    stx     ram_8C                  ;3        
    lda     #$f9                    ;2        
    sta     ram_8D                  ;3        
    lda     #$07                    ;2        
    sta     ram_8F                  ;3        
    jmp     Lffdd                   ;3   =  18
    
    lda     ram_DC                  ;3        
    cmp     #$00                    ;2        
    bne     Lda81                   ;2/3      
    ldx     #$15                    ;2        
    stx     ram_8C                  ;3        
    lda     #$f9                    ;2        
    sta     ram_8D                  ;3        
    lda     #$06                    ;2        
    sta     ram_8F                  ;3   =  22
Lda81
    lda     ram_DC                  ;3        
    cmp     #$14                    ;2        
    bne     Lda93                   ;2/3      
    ldx     #$1c                    ;2        
    stx     ram_8C                  ;3        
    lda     #$f9                    ;2        
    sta     ram_8D                  ;3        
    lda     #$06                    ;2        
    sta     ram_8F                  ;3   =  22
Lda93
    jmp     Lffdd                   ;3   =   3
    
    lda     #$14                    ;2        
    cmp     ram_DC                  ;3        
    bcs     Ldaa0                   ;2/3      
    lda     #$08                    ;2        
    sta     REFP1                   ;3   =  12
Ldaa0
    lda     ram_DC                  ;3        
    cmp     #$00                    ;2        
    bne     Ldaa9                   ;2/3      
    jmp     Ldaaf                   ;3   =  10
    
Ldaa9
    lda     ram_DC                  ;3        
    cmp     #$14                    ;2        
    bne     Ldabb                   ;2/3 =   7
Ldaaf
    ldx     #$23                    ;2        
    stx     ram_8C                  ;3        
    lda     #$f9                    ;2        
    sta     ram_8D                  ;3        
    lda     #$11                    ;2        
    sta     ram_8F                  ;3   =  15
Ldabb
    lda     ram_DC                  ;3        
    cmp     #$0a                    ;2        
    bne     Ldac4                   ;2/3      
    jmp     Ldaca                   ;3   =  10
    
Ldac4
    lda     ram_DC                  ;3        
    cmp     #$1e                    ;2        
    bne     Ldad6                   ;2/3 =   7
Ldaca
    ldx     #$35                    ;2        
    stx     ram_8C                  ;3        
    lda     #$f9                    ;2        
    sta     ram_8D                  ;3        
    lda     #$11                    ;2        
    sta     ram_8F                  ;3   =  15
Ldad6
    jmp     Lffdd                   ;3   =   3
    
    ldx #$47
    stx $8c
    lda #$f9
    sta $8d
    lda #$08
    sta $8f
    jmp     Lffdd                    ;3
    
Ldae8
    lda     ram_DC                  ;3        
    cmp     #$00                    ;2        
    bne     Ldafa                   ;2/3      
    ldx     #$50                    ;2        
    stx     ram_8C                  ;3        
    lda     #$f9                    ;2        
    sta     ram_8D                  ;3        
    lda     #$07                    ;2        
    sta     ram_8F                  ;3   =  22
Ldafa
    lda     ram_DC                  ;3        
    cmp     #$0a                    ;2        
    bne     Ldb0c                   ;2/3      
    ldx     #$58                    ;2        
    stx     ram_8C                  ;3        
    lda     #$f9                    ;2        
    sta     ram_8D                  ;3        
    lda     #$05                    ;2        
    sta     ram_8F                  ;3   =  22
Ldb0c
    lda     ram_DC                  ;3        
    cmp     #$14                    ;2        
    bne     Ldb1e                   ;2/3      
    ldx     #$5e                    ;2        
    stx     ram_8C                  ;3        
    lda     #$f9                    ;2        
    sta     ram_8D                  ;3        
    lda     #$04                    ;2        
    sta     ram_8F                  ;3   =  22
Ldb1e
    lda     ram_DC                  ;3        
    cmp     #$0a                    ;2        
    bcs     Ldb30                   ;2/3      
    lda     #$01                    ;2        
    sta     AUDV1                   ;3        
    lda     #$08                    ;2        
    sta     AUDC1                   ;3        
    lda     #$17                    ;2        
    sta     AUDF1                   ;3   =  22
Ldb30
    lda     #$14                    ;2        
    cmp     ram_DC                  ;3        
    bcs     Ldb42                   ;2/3      
    lda     #$01                    ;2        
    sta     AUDV1                   ;3        
    lda     #$08                    ;2        
    sta     AUDC1                   ;3        
    lda     #$11                    ;2        
    sta     AUDF1                   ;3   =  22
Ldb42
    jmp     Lffdd                   ;3   =   3
    
    ldx #$63
    stx $8c
    lda #$f9
    sta $8d
    lda #$08
    sta $8f
    jmp     Lffdd                    ;3
    
Ldb54
    ldx     #$6c                    ;2        
    stx     ram_8A                  ;3        
    lda     #$f9                    ;2        
    sta     ram_8B                  ;3        
    lda     #$2f                    ;2        
    sta     ram_8E                  ;3        
    jmp     Lffdd                   ;3   =  18
    
    ldx #$9c
    stx $8c
    lda #$f9
    sta $8d
    lda #$08
    sta $8f
    jmp Lffdd
    ldx #$a5
    stx $8c
    lda #$f9
    sta $8d
    lda #$07
    sta $8f
    jmp Lffdd
    ldx #$ad
    stx $8c
    lda #$f9
    sta $8d
    lda #$2f
    sta $8f
    jmp Lffdd
    ldx #$00
    stx $8c
    lda #$fa
    sta $8d
    lda #$2f
    sta $8f
    jmp Lffdd
    ldx #$30
    stx $8c
    lda #$fa
    sta $8d
    lda #$2f
    sta $8f
    jmp Lffdd
    ldx #$60
    stx $8c
    lda #$fa
    sta $8d
    lda #$08
    sta $8f
    jmp     Lffdd                    ;3
    
Ldbbd
    lda     ram_DC                  ;3        
    cmp     #$00                    ;2        
    bne     Ldbc6                   ;2/3      
    jmp     Ldbcc                   ;3   =  10
    
Ldbc6
    lda     ram_DC                  ;3        
    cmp     #$14                    ;2        
    bne     Ldbe4                   ;2/3 =   7
Ldbcc
    lda     #$05                    ;2        
    sta     AUDV1                   ;3        
    lda     #$0d                    ;2        
    sta     AUDC1                   ;3        
    lda     #$11                    ;2        
    sta     AUDF1                   ;3        
    ldx     #$69                    ;2        
    stx     ram_8C                  ;3        
    lda     #$fa                    ;2        
    sta     ram_8D                  ;3        
    lda     #$07                    ;2        
    sta     ram_8F                  ;3   =  30
Ldbe4
    lda     ram_DC                  ;3        
    cmp     #$0a                    ;2        
    bne     Ldbed                   ;2/3      
    jmp     Ldbf3                   ;3   =  10
    
Ldbed
    lda     ram_DC                  ;3        
    cmp     #$1e                    ;2        
    bne     Ldc0b                   ;2/3!=   7
Ldbf3
    lda     #$05                    ;2        
    sta     AUDV1                   ;3        
    lda     #$0a                    ;2        
    sta     AUDC1                   ;3        
    lda     #$11                    ;2        
    sta     AUDF1                   ;3        
    ldx     #$71                    ;2        
    stx     ram_8C                  ;3        
    lda     #$fa                    ;2        
    sta     ram_8D                  ;3        
    lda     #$07                    ;2        
    sta     ram_8F                  ;3   =  30
Ldc0b
    jmp     Lffdd                   ;3   =   3
    
    ldx     #$79                    ;2        
    stx     ram_8C                  ;3        
    lda     #$fa                    ;2        
    sta     ram_8D                  ;3        
    lda     #$07                    ;2        
    sta     ram_8F                  ;3        
    jmp     Lffdd                   ;3   =  18
    
    ldx     #$81                    ;2        
    stx     ram_8A                  ;3        
    lda     #$fa                    ;2        
    sta     ram_8B                  ;3        
    lda     #$07                    ;2        
    sta     ram_8E                  ;3        
    jmp     Lffdd                   ;3   =  18
    
    ldx #$89
    stx $8c
    lda #$fa
    sta $8d
    lda #$37
    sta $8f
    jmp     Lffdd                    ;3

    ldx #$c1
    stx $8c
    lda #$fa
    sta $8d
    lda #$08
    sta $8f
    jmp     Lffdd                    ;3
    
Ldc4a
    ldx     #$ca                    ;2        
    stx     ram_8C                  ;3        
    lda     #$fa                    ;2        
    sta     ram_8D                  ;3        
    lda     #$08                    ;2        
    sta     ram_8F                  ;3        
    jmp     Lffdd                   ;3   =  18
    
    ldx     #$00                    ;2        
    stx     ram_8C                  ;3        
    lda     #$fb                    ;2        
    sta     ram_8D                  ;3        
    lda     #$4e                    ;2        
    sta     ram_8F                  ;3        
    jmp     Lffdd                   ;3   =  18
    
    lda     ram_DC                  ;3        
    cmp     #$00                    ;2        
    bne     Ldc71                   ;2/3      
    jmp     Ldc77                   ;3   =  10
    
Ldc71
    lda     ram_DC                  ;3        
    cmp     #$1e                    ;2        
    bne     Ldc83                   ;2/3 =   7
Ldc77
    ldx     #$4f                    ;2        
    stx     ram_8C                  ;3        
    lda     #$fb                    ;2        
    sta     ram_8D                  ;3        
    lda     #$07                    ;2        
    sta     ram_8F                  ;3   =  15
Ldc83
    lda     ram_DC                  ;3        
    cmp     #$0a                    ;2        
    bne     Ldc95                   ;2/3      
    ldx     #$57                    ;2        
    stx     ram_8C                  ;3        
    lda     #$fb                    ;2        
    sta     ram_8D                  ;3        
    lda     #$07                    ;2        
    sta     ram_8F                  ;3   =  22
Ldc95
    lda     ram_DC                  ;3        
    cmp     #$14                    ;2        
    bne     Ldca7                   ;2/3      
    ldx     #$5f                    ;2        
    stx     ram_8C                  ;3        
    lda     #$fb                    ;2        
    sta     ram_8D                  ;3        
    lda     #$07                    ;2        
    sta     ram_8F                  ;3   =  22
Ldca7
    lda     ram_DC                  ;3        
    cmp     #$1e                    ;2        
    bcc     Ldcb1                   ;2/3      
    lda     #$08                    ;2        
    sta     REFP1                   ;3   =  12
Ldcb1
    jmp     Lffdd                   ;3   =   3
    
    lda     ram_DC                  ;3        
    cmp     #$00                    ;2        
    bne     Ldcc6                   ;2/3      
    ldx     #$67                    ;2        
    stx     ram_8C                  ;3        
    lda     #$fb                    ;2        
    sta     ram_8D                  ;3        
    lda     #$07                    ;2        
    sta     ram_8F                  ;3   =  22
Ldcc6
    lda     ram_DC                  ;3        
    cmp     #$0a                    ;2        
    bne     Ldccf                   ;2/3      
    jmp     Ldcd5                   ;3   =  10
    
Ldccf
    lda     ram_DC                  ;3        
    cmp     #$1e                    ;2        
    bne     Ldce1                   ;2/3 =   7
Ldcd5
    ldx     #$6f                    ;2        
    stx     ram_8C                  ;3        
    lda     #$fb                    ;2        
    sta     ram_8D                  ;3        
    lda     #$07                    ;2        
    sta     ram_8F                  ;3   =  15
Ldce1
    lda     ram_DC                  ;3        
    cmp     #$14                    ;2        
    bne     Ldcf3                   ;2/3      
    ldx     #$77                    ;2        
    stx     ram_8C                  ;3        
    lda     #$fb                    ;2        
    sta     ram_8D                  ;3        
    lda     #$07                    ;2        
    sta     ram_8F                  ;3   =  22
Ldcf3
    lda     #$14                    ;2        
    cmp     ram_DC                  ;3        
    bcs     Ldcfd                   ;2/3      
    lda     #$08                    ;2        
    sta     REFP1                   ;3   =  12
Ldcfd
    jmp     Lffdd                   ;3   =   3
    
    ldx     #$7f                    ;2        
    stx     ram_8C                  ;3        
    lda     #$fb                    ;2        
    sta     ram_8D                  ;3        
    lda     #$17                    ;2        
    sta     ram_8F                  ;3        
    jmp     Lffdd                   ;3   =  18
    
    lda     ram_DC                  ;3        
    cmp     #$00                    ;2        
    bne     Ldd18                   ;2/3      
    jmp     Ldd1e                   ;3   =  10
    
Ldd18
    lda     ram_DC                  ;3        
    cmp     #$14                    ;2        
    bne     Ldd2a                   ;2/3 =   7
Ldd1e
    ldx     #$97                    ;2        
    stx     ram_8C                  ;3        
    lda     #$fb                    ;2        
    sta     ram_8D                  ;3        
    lda     #$2f                    ;2        
    sta     ram_8F                  ;3   =  15
Ldd2a
    lda     ram_DC                  ;3        
    cmp     #$0a                    ;2        
    bne     Ldd33                   ;2/3      
    jmp     Ldd39                   ;3   =  10
    
Ldd33
    lda     ram_DC                  ;3        
    cmp     #$1e                    ;2        
    bne     Ldd45                   ;2/3 =   7
Ldd39
    ldx     #$c7                    ;2        
    stx     ram_8C                  ;3        
    lda     #$fb                    ;2        
    sta     ram_8D                  ;3        
    lda     #$2f                    ;2        
    sta     ram_8F                  ;3   =  15
Ldd45
    jmp     Lffdd                   ;3   =   3
    
    ldx     #$00                    ;2        
    stx     ram_8C                  ;3        
    lda     #$fc                    ;2        
    sta     ram_8D                  ;3        
    lda     #$0c                    ;2        
    sta     ram_8F                  ;3        
    jmp     Lffdd                   ;3   =  18
    
    lda     ram_DC                  ;3        
    cmp     #$00                    ;2        
    bne     Ldd60                   ;2/3      
    jmp     Ldd66                   ;3   =  10
    
Ldd60
    lda     ram_DC                  ;3        
    cmp     #$1b                    ;2        
    bne     Ldd72                   ;2/3 =   7
Ldd66
    ldx     #$0d                    ;2        
    stx     ram_8C                  ;3        
    lda     #$fc                    ;2        
    sta     ram_8D                  ;3        
    lda     #$07                    ;2        
    sta     ram_8F                  ;3   =  15
Ldd72
    lda     ram_DC                  ;3        
    cmp     #$0d                    ;2        
    bne     Ldd84                   ;2/3      
    ldx     #$15                    ;2        
    stx     ram_8C                  ;3        
    lda     #$fc                    ;2        
    sta     ram_8D                  ;3        
    lda     #$07                    ;2        
    sta     ram_8F                  ;3   =  22
Ldd84
    lda     ram_DC                  ;3        
    cmp     #$1b                    ;2        
    bcc     Ldd8e                   ;2/3      
    lda     #$08                    ;2        
    sta     REFP1                   ;3   =  12
Ldd8e
    jmp     Lffdd                   ;3   =   3
    
    ldx     #$1d                    ;2        
    stx     ram_8C                  ;3        
    lda     #$fc                    ;2        
    sta     ram_8D                  ;3        
    lda     #$06                    ;2        
    sta     ram_8F                  ;3        
    jmp     Lffdd                   ;3   =  18
    
    ldx #$24
    stx $8c
    lda #$fc
    sta $8d
    lda #$04
    sta $8f
    jmp     Lffdd                    ;3

  IF PLUSROM = 1
PlusROM_API
       .byte "a", 0, "h.firmaplus.de", 0

    ORG     $6fd0, $ff
    RORG    $dfd0
  ELSE
    ORG     $6fd4, $ff
    RORG    $dfd4
  ENDIF

Start_b6
    ldx #$ff
    txs
    lda #$f2
    pha
    lda #$4f
    pha

Ldfdd
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    tsx                             ;2        
    lda     CXM0FB,x                ;4        
    rol                             ;2        
    rol                             ;2        
    rol                             ;2        
    rol                             ;2        
    and     #$07                    ;2        
    tax                             ;2        
    inx                             ;2   =  28
Ldfeb
    lda     $1ff3,x                 ;4        
    pla                             ;4        
    tax                             ;2        
    pla                             ;4        
    rts                             ;6   =  20
    
    ORG     $6ff3, $ff
    RORG    $dff3

Ldff3
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff     ; $dff3 (*)

  IF PLUSROM = 1
    .word (PlusROM_API - $6000)             ; PlusRom API pointer
  ELSE
    .byte   $ff,$ff                         ; $dffa (D)
  ENDIF

    .word Start_b6,Start_b6


;***********************************************************
;      Bank 7 / 0..7
;***********************************************************

    SEG     CODE
    ORG     $7000
    RORG    $f000

Lf000
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     #$ff                    ;2        
    sta     TIM64T                  ;4        
    lda     #$01                    ;2        
    sta     VDELBL                  ;3        
    sta     VDELP0                  ;3        
    ldx     ram_92                  ;3        
    inx                             ;2        
    inx                             ;2        
    stx     ram_9F                  ;3        
    lda     ram_86                  ;3        
    sta     ram_9E                  ;3        
    ldx     ram_90                  ;3        
    inx                             ;2        
    inx                             ;2        
    stx     ram_F6                  ;3        
    lda     ram_89                  ;3        
    sta     ram_F7                  ;3        
    lda     ram_85                  ;3        
    ldx     #$00                    ;2        
    sta     WSYNC                   ;3   =  54
;---------------------------------------
    stx     GRP0                    ;3        
    stx     GRP1                    ;3        
    stx     PF1                     ;3        
    stx     PF2                     ;3        
    stx     CXCLR                   ;3        
    NOP     VSYNC                   ;3        
    sta     ram_9D,x                ;4        
    ldx     #$54                    ;2        
    dec     ram_85                  ;5        
    lda     ram_91                  ;3        
    sta     ram_A0                  ;3        
    lda     ram_88                  ;3        
    sta     ram_A1                  ;3        
    lda     ram_EF                  ;3        
    sta     ram_9C                  ;3        
    lda     #$0a                    ;2        
    clc                             ;2        
    sbc     ram_EF                  ;3        
    sta     ram_EF                  ;3        
    jmp     Lf074                   ;3   =  60
    
Lf04f
    lda     #$00                    ;2        
    tay                             ;2        
    jmp     Lf0a2                   ;3   =   7
    
Lf055
    lda     #$00                    ;2        
    tay                             ;2        
    jmp     Lf07e                   ;3   =   7
    
Lf05b
    nop                             ;2        
    lda     ram_92                  ;3        
    ldy     CXM0P|$50,x             ;4        
    sty     PF1                     ;3        
    ldy     CXM1P|$50,x             ;4        
    sty     PF2                     ;3        
    ldy     CXP1FB|$50,x            ;4        
    sty     PF1                     ;3        
    ldy     CXP0FB|$50,x            ;4        
    sty     PF2                     ;3        
    DCP     ram_89                  ;5        
    rol                             ;2        
    rol                             ;2        
    sta     ENABL                   ;3   =  45
Lf074
    lda     ram_8F                  ;3        
    DCP     ram_86                  ;5        
    bcc     Lf055                   ;2/3      
    ldy     ram_86                  ;3        
    lda     (ram_8C),y              ;5   =  18
Lf07e
    sta     GRP1                    ;3        
    lda     ram_87                  ;3        
    DCP     ram_88                  ;5        
    rol                             ;2        
    rol                             ;2        
    sta     ENAM1                   ;3        
    lda     CXM0P|$50,x             ;4        
    sta     PF1                     ;3        
    lda     CXM1P|$50,x             ;4        
    sta     PF2                     ;3        
    lda     CXP1FB|$50,x            ;4        
    sta     PF1                     ;3        
    lda     CXP0FB|$50,x            ;4        
    sta     PF2                     ;3        
    lda     ram_8E                  ;3        
    DCP     ram_85                  ;5        
    bcc     Lf04f                   ;2/3      
    ldy     ram_85                  ;3        
    lda     (ram_8A),y              ;5   =  64
Lf0a2
    sta     GRP0                    ;3        
    dec     ram_9C                  ;5        
    beq     Lf0b4                   ;2/3      
    txa                             ;2        
    tay                             ;2        
    lda     (ram_F0),y              ;5        
    sta     COLUPF                  ;3        
    jmp     Lf05b                   ;3   =  25
    
    jmp     Lf05b                   ;3 never reached! bug in bB std kernel!
    
Lf0b4
    txa                             ;2        
    SBX     #$fc                    ;2        
    bmi     Lf0dd                   ;2/3      
    lda     #$08                    ;2        
    sta     ram_9C                  ;3        
    jmp     Lf05b                   ;3   =  14
    
    .byte   $a9,$00,$85,$0e,$85,$0f,$8a,$cb ; $f0c0 (*)
    .byte   $fc,$30,$12,$a8,$b1,$f0,$85,$08 ; $f0c8 (*)
    .byte   $a9,$08,$85,$9c,$a5,$92,$c7,$89 ; $f0d0 (*)
    .byte   $e5,$9f,$4c,$72,$f0             ; $f0d8 (*)
    
Lf0dd
    ldx     ram_EF                  ;3        
    NOP     VSYNC                   ;3        
    cpx     #$00                    ;2        
    bne     Lf107                   ;2/3!     
    jmp     Lf168                   ;3   =  13 *
    
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f0e8 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f0f0 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f0f8 (*)
    
Lf100
    lda     #$00                    ;2        
    tay                             ;2        
    jmp     Lf12d                   ;3   =   7
    
Lf106
    nop                             ;2   =   2 *
Lf107
    ldy.w   ram_D0                  ;4        
    sty     PF1                     ;3        
    ldy.w   ram_D1                  ;4        
    sty     PF2                     ;3        
    ldy.w   ram_D3                  ;4        
    sty     PF1                     ;3        
    ldy.w   ram_D2                  ;4        
    sty     PF2                     ;3        
    lda     ram_92                  ;3        
    DCP     ram_89                  ;5        
    rol                             ;2        
    rol                             ;2        
    sta     ENABL                   ;3        
    lda     ram_8F                  ;3        
    DCP     ram_86                  ;5        
    bcc     Lf100                   ;2/3      
    ldy     ram_86                  ;3        
    lda     (ram_8C),y              ;5   =  61
Lf12d
    sta     GRP1                    ;3        
    lda     ram_87                  ;3        
    DCP     ram_88                  ;5        
    dex                             ;2        
    beq     Lf16a                   ;2/3      
    ldy.w   ram_D0                  ;4         *
    sty     PF1                     ;3         *
    ldy.w   ram_D1                  ;4         *
    sty     PF2                     ;3         *
    ldy.w   ram_D3                  ;4         *
    sty     PF1                     ;3         *
    ldy.w   ram_D2                  ;4         *
    sty     PF2                     ;3         *
    rol                             ;2         *
    rol                             ;2         *
    sta     ENAM1                   ;3         *
    lda.w   ram_8E                  ;4         *
    DCP     ram_85                  ;5         *
    bcc     Lf162                   ;2/3       *
    ldy     ram_85                  ;3         *
    lda     (ram_8A),y              ;5   =  69 *
Lf159
    sta     GRP0                    ;3         *
    pla                             ;4         *
    pha                             ;3         *
    pla                             ;4         *
    pha                             ;3         *
    jmp     Lf106                   ;3   =  20 *
    
Lf162
    lda     #$00                    ;2         *
    tay                             ;2         *
    jmp     Lf159                   ;3   =   7 *
    
Lf168
    ldx     #$00                    ;2   =   2 *
Lf16a
    stx     PF1                     ;3        
    stx     PF2                     ;3        
    stx     PF0                     ;3        
    clc                             ;2        
    lda     #$0a                    ;2        
    sbc     ram_EF                  ;3        
    sta     ram_EF                  ;3        
    txa                             ;2        
    sta     WSYNC,x                 ;4   =  25
;---------------------------------------
    sta     REFP0                   ;3        
    sta     REFP1                   ;3        
    sta     GRP0                    ;3        
    sta     GRP1                    ;3        
    sta     HMCLR                   ;3        
    sta     ENAM0                   ;3        
    sta     ENAM1                   ;3        
    sta     ENABL                   ;3        
    lda     ram_9D                  ;3        
    sta     ram_85                  ;3        
    lda     ram_9E                  ;3        
    sta     ram_86                  ;3        
    lda     ram_A1                  ;3        
    sta     ram_88                  ;3        
    lda     ram_A0                  ;3        
    sta     ram_91                  ;3        
    lda     ram_F7                  ;3        
    sta     ram_89                  ;3        
    lda     INTIM                   ;4        
    clc                             ;2        
    adc     #$8e                    ;2        
    sta     TIM64T                  ;4        
    jsr     Lf620                   ;6        
    lda     ram_97                  ;3        
    sta     ram_9C                  ;3        
    lda     ram_99                  ;3        
    sta     ram_9E                  ;3        
    sta     HMCLR                   ;3        
    tsx                             ;2        
    stx     ram_F6                  ;3        
    ldx     #$e0                    ;2        
    stx     HMP0                    ;3        
    lda     ram_A3                  ;3        
    sta     COLUP0                  ;3        
    sta     COLUP1                  ;3        
    sta     WSYNC                   ;3   = 109
;---------------------------------------
    ldx     #$00                    ;2        
    stx     GRP0                    ;3        
    stx     GRP1                    ;3        
    lda     ram_9B                  ;3        
    sta     ram_A0,x                ;4        
    lda     #$ff                    ;2        
    sta     ram_97                  ;3        
    sta     ram_99                  ;3        
    sta     ram_9B                  ;3        
    sta     ram_9D                  ;3        
    sta     ram_9F                  ;3        
    sta     ram_A1                  ;3        
    ldy     #$07                    ;2        
    sty     VDELP0                  ;3        
    sta     RESP0                   ;3        
    sta     RESP1                   ;3        
    lda     #$03                    ;2        
    sta     NUSIZ0                  ;3        
    sta     NUSIZ1                  ;3        
    sta     VDELP1                  ;3        
    lda     #$f0                    ;2        
    sta     HMP1                    ;3        
    lda     (ram_96),y              ;5        
    sta     GRP0                    ;3        
    sta     HMOVE                   ;3        
    jmp     Lf208                   ;3   =  76
    
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f1f8 (*)
    
Lf200
    lda     (ram_96),y              ;5        
    sta     GRP0                    ;3        
    NOP     VSYNC                   ;3        
    nop                             ;2        
    nop                             ;2   =  15
Lf208
    lda     (ram_9E),y              ;5        
    sta     GRP1                    ;3        
    lda     (ram_9C),y              ;5        
    sta     GRP0                    ;3        
    LAX     (ram_98),y              ;5        
    txs                             ;2        
    LAX     (ram_9A),y              ;5        
    NOP     VSYNC                   ;3        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    lda     (ram_A0),y              ;5        
    stx     GRP1                    ;3        
    tsx                             ;2        
    stx     GRP0                    ;3        
    sta     GRP1                    ;3        
    sty     GRP0                    ;3        
    dey                             ;2        
    bpl     Lf200                   ;2/3      
    ldx     ram_F6                  ;3        
    txs                             ;2        
    ldy     ram_9C                  ;3        
    sty     ram_97                  ;3        
    lda     #$00                    ;2        
    sta     PF1                     ;3        
    sta     GRP0                    ;3        
    sta     GRP1                    ;3        
    sta     VDELP0                  ;3        
    sta     VDELP1                  ;3        
    sta     NUSIZ0                  ;3        
    sta     NUSIZ1                  ;3        
    ldy     ram_9E                  ;3        
    sty     ram_99                  ;3        
    ldy     ram_A0                  ;3        
    sty     ram_9B                  ;3        
    lda     #$c2                    ;2        
    sta     WSYNC                   ;3   = 111
;---------------------------------------
    sta     VBLANK                  ;3        
    jmp     Lffdd                   ;3   =   6
    
Init
    sei                             ;2        
    cld                             ;2        
    ldy     #$00                    ;2        
    lda     ram_D0                  ;3        
    cmp     #$2c                    ;2        
    bne     Lf261                   ;2/3      
    lda     ram_D1                  ;3         *
    cmp     #$a9                    ;2         *
    bne     Lf261                   ;2/3       *
    dey                             ;2   =  22 *
Lf261
    ldx     #$00                    ;2        
    txa                             ;2   =   4
Lf264
    inx                             ;2        
    txs                             ;2        
    pha                             ;3        
    bne     Lf264                   ;2/3      
    sty     ram_9C                  ;3        
    lda     #$08                    ;2        
    sta     ram_EF                  ;3        
    ldx     #$05                    ;2   =  19
Lf271
    lda     #$74                    ;2        
    sta     ram_96,x                ;4        
    dex                             ;2        
    bpl     Lf271                   ;2/3      
    lda     #$01                    ;2        
    sta     CTRLPF                  ;3        
    ora     INTIM                   ;4        
    sta     ram_A2                  ;3        
    lda     #$10                    ;2        
    pha                             ;3        
    lda     #$33                    ;2        
    pha                             ;3        
    pha                             ;3        
    pha                             ;3        
    ldx     #$01                    ;2        
    jmp     Lffeb                   ;3   =  43
    
    .byte   $a2,$2f,$95,$a4,$ca,$10,$fb    ; $f28e (*)
    jmp     Lffdd                   ;3
    
Lf298
    stx     ram_9D                  ;3        
    tax                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    sta     ram_9C                  ;3        
    tya                             ;2        
    asl                             ;2        
    asl                             ;2        
    clc                             ;2        
    adc     ram_9C                  ;3        
    tay                             ;2        
    lda     ram_9D                  ;3        
    rts                             ;6   =  36
    
    jsr     Lf298                   ;6        
    lda     Lf342,x                 ;4        
    and.wy  ram_A4,y                ;4        
    eor     Lf342,x                 ;4        
    jmp     Lffdd                   ;3   =  21
    
    jsr     Lf298                   ;6        
    lda     ram_9D                  ;3        
    beq     Lf2cf                   ;2/3      
    lsr                             ;2        
    bcs     Lf2db                   ;2/3      
    lda.wy  ram_A4,y                ;4         *
    eor     Lf342,x                 ;4         *
    sta.wy  ram_A4,y                ;5         *
    jmp     Lffdd                   ;3   =  31 *
    
Lf2cf
    lda.wy  ram_A4,y                ;4        
    ora     Lf342,x                 ;4        
    sta.wy  ram_A4,y                ;5        
    jmp     Lffdd                   ;3   =  16
    
Lf2db
    lda     Lf342,x                 ;4        
    eor     #$ff                    ;2        
    and.wy  ram_A4,y                ;4        
    sta.wy  ram_A4,y                ;5        
    jmp     Lffdd                   ;3   =  18
    
    jsr     Lf298                   ;6        
    jmp     Lf2f6                   ;3   =   9
    
Lf2ef
    inx                             ;2        
    txa                             ;2        
    and     #$07                    ;2        
    bne     Lf2f6                   ;2/3      
    iny                             ;2   =  10
Lf2f6
    jsr     Lf31b                   ;6        
    cpx     ram_9E                  ;3        
    bmi     Lf2ef                   ;2/3      
    jmp     Lffdd                   ;3   =  14
    
    jsr     Lf298                   ;6        
    sty     ram_9C                  ;3        
    inc     ram_9E                  ;5        
    lda     ram_9E                  ;3        
    asl                             ;2        
    asl                             ;2        
    sta     ram_9E                  ;3   =  24
Lf30d
    jsr     Lf31b                   ;6        
    iny                             ;2        
    iny                             ;2        
    iny                             ;2        
    iny                             ;2        
    cpy     ram_9E                  ;3        
    bmi     Lf30d                   ;2/3      
    jmp     Lffdd                   ;3   =  22
    
Lf31b
    lda     ram_9D                  ;3        
    beq     Lf32c                   ;2/3      
    lsr                             ;2        
    bcs     Lf336                   ;2/3      
    lda.wy  ram_A4,y                ;4         *
    eor     Lf342,x                 ;4         *
    sta.wy  ram_A4,y                ;5         *
    rts                             ;6   =  28 *
    
Lf32c
    lda.wy  ram_A4,y                ;4        
    ora     Lf342,x                 ;4        
    sta.wy  ram_A4,y                ;5        
    rts                             ;6   =  19
    
Lf336
    lda     Lf342,x                 ;4        
    eor     #$ff                    ;2        
    and.wy  ram_A4,y                ;4        
    sta.wy  ram_A4,y                ;5        
    rts                             ;6   =  21
    
Lf342
    .byte   $80,$40,$20,$10,$08,$04,$02,$01 ; $f342 (D)
    .byte   $01,$02,$04,$08,$10,$20,$40,$80 ; $f34a (D)
    .byte   $80,$40,$20,$10,$08,$04,$02,$01 ; $f352 (D)
    .byte   $01,$02,$04,$08,$10,$20,$40,$80 ; $f35a (D)
    .byte   $d0,$15,$a2,$30,$b5,$a3,$4a,$36 ; $f362 (*)
    .byte   $a2,$76,$a1,$36,$a0,$76,$a3,$8a ; $f36a (*)
    .byte   $cb,$04,$d0,$f0
    jmp     Lffdd                    ;3
    .byte $4a ; $f372 (*)
    .byte   $90,$15,$a2,$30,$b5,$a0,$4a,$36 ; $f37a (*)
    .byte   $a1,$76,$a2,$36,$a3,$76,$a0,$8a ; $f382 (*)
    .byte   $cb,$04,$d0,$f0
    jmp     Lffdd                    ;3
    .byte $4a ; $f38a (*)
    .byte   $90,$4b,$4a,$90,$02,$c6,$ef,$c6 ; $f392 (*)
    .byte   $ef,$f0,$02,$10,$3d,$a9,$08,$85 ; $f39a (*)
    .byte   $ef,$a5,$a7,$85,$9f,$a5,$a6,$85 ; $f3a2 (*)
    .byte   $9e,$a5,$a5,$85,$9d,$a5,$a4,$85 ; $f3aa (*)
    .byte   $9c,$a2,$00,$b5,$a8,$95,$a4,$b5 ; $f3b2 (*)
    .byte   $a9,$95,$a5,$b5,$aa,$95,$a6,$b5 ; $f3ba (*)
    .byte   $ab,$95,$a7,$8a,$cb             ; $f3c2 (*)
Lf3c7
    .byte   $fc,$e0,$2c,$d0,$e9,$a5,$9f,$85 ; $f3c7 (*)
    .byte   $d3,$a5,$9e,$85,$d2,$a5,$9d,$85 ; $f3cf (*)
    .byte   $d1,$a5,$9c,$85,$d0
    jmp     Lffdd                    ;3

    lsr
    bcs lf3e4
    inc $ef
lf3e4
    inc $ef
    lda $ef
    cmp #$09
    bcc lf427
    lda #$01
    sta $ef
    lda $d3
    sta $9f
    lda $d2
    sta $9e
    lda $d1
    sta $9d
    lda $d0
    sta $9c
    ldx #$2c
lf402
    lda $a3,x
    sta $a7,x
    lda $a2,x
    sta $a6,x
    lda $a1,x
    sta $a5,x
    lda $a0,x
    sta $a4,x
    txa
    sbx #$04
    bne lf402
    lda $9f
    sta $a7
    lda $9e
    sta $a6
    lda $9d
    sta $a5
    lda $9c
    sta $a4
lf427
    jmp     Lffdd                    ;3

    lda $a2
    lsr
    bcc lf42e
    eor #$b4
lf42e
    sta $a2
    jmp     Lffdd                    ;3
    
Lf436
    lda     INTIM                   ;4        
    bmi     Lf436                   ;2/3      
    lda     #$02                    ;2        
    sta     WSYNC                   ;3   =  11
;---------------------------------------
    sta     VSYNC                   ;3        
    sta     WSYNC                   ;3   =   6
;---------------------------------------
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lsr                             ;2        
    sta     WSYNC                   ;3   =   5
;---------------------------------------
    sta     VSYNC                   ;3        
    sta     VBLANK                  ;3        
    lda     #$a5                    ;2        
    sta     TIM64T                  ;4        
    sta     WSYNC                   ;3   =  15
;---------------------------------------
    ldx     #$04                    ;2        
    NOP     VSYNC                   ;3   =   5
Lf457
    lda     ram_80,x                ;4        
    sec                             ;2   =   6
Lf45a
    sbc     #$0f                    ;2        
    bcs     Lf45a                   ;2/3      
    sta     ram_9C,x                ;4        
    sta     RESP0,x                 ;4        
    sta     WSYNC                   ;3   =  15
;---------------------------------------
    dex                             ;2        
    bpl     Lf457                   ;2/3      
    ldx     #$04                    ;2        
    ldy     ram_9C,x                ;4        
    lda     Lf3c7,y                 ;4        
    sta     HMP0,x                  ;4        
    dex                             ;2        
    ldy     ram_9C,x                ;4        
    lda     Lf3c7,y                 ;4        
    sta     HMP0,x                  ;4        
    dex                             ;2        
    ldy     ram_9C,x                ;4        
    lda     Lf3c7,y                 ;4        
    sta     HMP0,x                  ;4        
    dex                             ;2        
    ldy     ram_9C,x                ;4        
    lda     Lf3c7,y                 ;4        
    sta     HMP0,x                  ;4        
    dex                             ;2        
    ldy     ram_9C,x                ;4        
    lda     Lf3c7,y                 ;4        
    sta     HMP0,x                  ;4        
    sta     WSYNC                   ;3   =  77
;---------------------------------------
    sta     HMOVE                   ;3        
    LAX     ram_95                  ;3        
    jsr     Lf4c7                   ;6        
    sty     ram_9B                  ;3        
    stx     ram_98                  ;3        
    LAX     ram_94                  ;3        
    jsr     Lf4c7                   ;6        
    sty     ram_9A                  ;3        
    stx     ram_97                  ;3        
    LAX     ram_93                  ;3        
    jsr     Lf4c7                   ;6        
    sty     ram_99                  ;3        
    stx     ram_96                  ;3   =  48
Lf4af
    lda     INTIM                   ;4        
    bmi     Lf4af                   ;2/3      
    jmp     Lf000                   ;3   =   9
    
    .byte   $80                             ; $f4b7 (*)
    .byte   $70,$60,$50,$40,$30,$20,$10,$00 ; $f4b8 (D)
    .byte   $f0,$e0,$d0,$c0,$b0,$a0,$90     ; $f4c0 (D)
    
Lf4c7
    and     #$0f                    ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    adc     #$74                    ;2        
    tay                             ;2        
    txa                             ;2        
    ASR     #$f0                    ;2        
    adc     #$74                    ;2        
    tax                             ;2        
    rts                             ;6   =  26
    
    .byte   $a5,$dc,$c9,$00,$d0,$0c,$a2,$29 ; $f4d6 (*)
    .byte   $86,$8c,$a9,$fc,$85,$8d,$a9,$07 ; $f4de (*)
    .byte   $85,$8f,$a5,$dc,$c9,$0a,$d0,$03 ; $f4e6 (*)
    .byte   $4c,$f7,$f4,$a5,$dc,$c9,$1e,$d0 ; $f4ee (*)
    .byte   $0c,$a2,$31,$86,$8c,$a9,$fc,$85 ; $f4f6 (*)
    .byte   $8d,$a9,$07,$85,$8f,$a5,$dc,$c9 ; $f4fe (*)
    .byte   $14,$d0,$0c,$a2,$39,$86,$8c,$a9 ; $f506 (*)
    .byte   $fc,$85,$8d,$a9,$07,$85,$8f     ; $f50e (*)
    jmp     Lffdd                    ;3
    .byte   $a2,$41,$86,$8c,$a9,$fc         ; $f518 (*)
    .byte   $85,$8d,$a9,$11,$85,$8f
    jmp     Lffdd                    ;3
    .byte   $a2,$53,$86,$8c,$a9,$fc,$85     ; $f526 (*)
    .byte   $8d,$a9,$11,$85,$8f
    jmp     Lffdd                    ;3
    .byte   $a2,$65,$86,$8c,$a9,$fc,$85,$8d ; $f536 (*)
    .byte   $a9,$11,$85,$8f
    jmp     Lffdd                    ;3
    .byte   $a2                             ; $f53e (*)
    .byte   $77,$86,$8c,$a9,$fc,$85,$8d,$a9 ; $f546 (*)
    .byte   $11,$85,$8f
    jmp     Lffdd                    ;3
    
Lf554
    ldx     #$89                    ;2        
    stx     ram_8C                  ;3        
    lda     #$fc                    ;2        
    sta     ram_8D                  ;3        
    lda     #$07                    ;2        
    sta     ram_8F                  ;3        
    jmp     Lffdd                   ;3   =  18
    
    ldx     #$91                    ;2        
    stx     ram_8C                  ;3        
    lda     #$fc                    ;2        
    sta     ram_8D                  ;3        
    lda     #$09                    ;2        
    sta     ram_8F                  ;3        
    jmp     Lffdd                   ;3   =  18
    
    ldx     #$9b                    ;2        
    stx     ram_8A                  ;3        
    lda     #$fc                    ;2        
    sta     ram_8B                  ;3        
    lda     #$50                    ;2        
    sta     ram_8E                  ;3        
    ldx     #$00                    ;2        
    stx     ram_8C                  ;3        
    lda     #$fd                    ;2        
    sta     ram_8D                  ;3        
    lda     #$50                    ;2        
    sta     ram_8F                  ;3        
    jmp     Lffdd                   ;3   =  33
    
    ldx     #$51                    ;2        
    stx     ram_8C                  ;3        
    lda     #$fd                    ;2        
    sta     ram_8D                  ;3        
    lda     #$2b                    ;2        
    sta     ram_8F                  ;3        
    jmp     Lffdd                   ;3   =  18
    
    ldx     #$7d                    ;2        
    stx     ram_8A                  ;3        
    lda     #$fd                    ;2        
    sta     ram_8B                  ;3        
    lda     #$1e                    ;2        
    sta     ram_8E                  ;3        
    ldx     #$9c                    ;2        
    stx     ram_8C                  ;3        
    lda     #$fd                    ;2        
    sta     ram_8D                  ;3        
    lda     #$1e                    ;2        
    sta     ram_8F                  ;3        
    jmp     Lffdd                   ;3   =  33
    
    .byte   $a2,$bb,$86,$8c,$a9,$fd,$85,$8d ; $f5b7 (*)
    .byte   $a9,$22,$85,$8f
    jmp     Lffdd                    ;3
    .byte   $a2                             ; $f5bf (*)
    .byte   $00,$86,$8c,$a9,$fe,$85,$8d,$a9 ; $f5c7 (*)
    .byte   $27,$85,$8f
    jmp     Lffdd                    ;3
    .byte   $a2,$28                         ; $f5cf (*)
    .byte   $86,$8c,$a9,$fe,$85,$8d,$a9,$07 ; $f5d7 (*)
    .byte   $85,$8f
    jmp     Lffdd                    ;3
    .byte   $a2,$30,$86                     ; $f5df (*)
    .byte   $8c,$a9,$fe,$85,$8d,$a9,$09,$85 ; $f5e7 (*)
    .byte   $8f
    jmp     Lffdd                    ;3
    .byte   $a2,$3a,$86,$8c                 ; $f5ef (*)
    .byte   $a9,$fe,$85,$8d,$a9,$05,$85,$8f ; $f5f7 (*)
    jmp     Lffdd                    ;3
    .byte   $a2,$40,$86,$8c,$a9             ; $f5ff (*)
    .byte   $fe,$85,$8d,$a9,$05,$85,$8f     ; $f607 (*)
    jmp     Lffdd                    ;3
    .byte   $a2,$46,$86,$8c,$a9,$fe         ; $f611 (*)
    .byte   $85,$8d,$a9,$0f,$85,$8f
    jmp     Lffdd                    ;3
    
Lf620
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    lda     #$00                    ;2        
    ldy     #$07                    ;2        
    sta     VDELP0                  ;3        
    sta     VDELP1                  ;3        
    lda     ram_F3                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    tax                             ;2        
    lda     Lf66c,x                 ;4        
    sta     RESP0                   ;3        
    sta     RESP1                   ;3        
    sta.w   NUSIZ0                  ;4        
    lda     Lf66b,x                 ;4        
    sta     NUSIZ1                  ;3        
    lda     ram_F4                  ;3        
    sta     COLUP0                  ;3        
    sta     COLUP1                  ;3        
    lda     #$10                    ;2        
    sta     HMP1                    ;3        
    sta     HMOVE                   ;3   =  73
Lf652
    cpx     #$00                    ;2        
    beq     Lf660                   ;2/3      
    lda     (ram_F2),y              ;5        
    sta     GRP0                    ;3        
    cpx     #$01                    ;2        
    beq     Lf660                   ;2/3      
    sta     GRP1                    ;3   =  19
Lf660
    dey                             ;2        
    sta     WSYNC                   ;3   =   5
;---------------------------------------
    bpl     Lf652                   ;2/3      
    iny                             ;2        
    sty     GRP0                    ;3        
    sty     GRP1                    ;3        
    rts                             ;6   =  16
    
Lf66b
    .byte   $00                             ; $f66b (D)
Lf66c
    .byte   $00,$00,$00,$01,$01,$03         ; $f66c (D)
    .byte   $03,$03                         ; $f672 (*)

Pfcolortable1
    .byte   BLACK|$0, BLACK|$0, BLACK|$0, BLACK|$0
    .byte   BLACK|$0, BLACK|$0, BLACK|$0, BLACK|$0
    .byte   BLACK|$0, BLACK|$0, BLACK|$0, BLACK|$0
    .byte   BLACK|$0, BLACK|$0, BLACK|$0, BLACK|$0
    .byte   BLACK|$0, BLACK|$0, BLUE|$0, BLUE|$e
    .byte   BLACK|$0, BLACK|$0, BLUE|$0, BLUE_CYAN|$2
    .byte   BLACK|$0, BLACK|$0, BLUE|$0, BLUE_CYAN|$2
    .byte   BLACK|$0, BLACK|$0, BLUE|$0, BLUE_CYAN|$2
    .byte   BLACK|$0, BLACK|$0, BLUE|$0, BLUE_CYAN|$2
    .byte   BLACK|$0, BLACK|$0, BLUE|$0, BLUE_CYAN|$2
    .byte   BLACK|$0, BLACK|$0, BLACK|$0, BLACK|$0            
    
    .byte   %00000000 ; |        |            $f6a0 (G)
    .byte   %00010000 ; |   #    |            $f6a1 (G)
    .byte   %00101000 ; |  # #   |            $f6a2 (G)
    .byte   %01010100 ; | # # #  |            $f6a3 (G)
    .byte   %10111010 ; |# ### # |            $f6a4 (G)
    .byte   %10111010 ; |# ### # |            $f6a5 (G)
    .byte   %10010010 ; |#  #  # |            $f6a6 (G)
    .byte   %01101100 ; | ## ##  |            $f6a7 (G)
    .byte   %00000100 ; |     #  |            $f6a8 (G)
    .byte   %00100100 ; |  #  #  |            $f6a9 (G)
    .byte   %00111100 ; |  ####  |            $f6aa (G)
    .byte   %00111100 ; |  ####  |            $f6ab (G)
    .byte   %01111100 ; | #####  |            $f6ac (G)
    .byte   %10111110 ; |# ##### |            $f6ad (G)
    .byte   %00111101 ; |  #### #|            $f6ae (G)
    .byte   %00111100 ; |  ####  |            $f6af (G)
    .byte   %00100000 ; |  #     |            $f6b0 (G)
    .byte   %00100100 ; |  #  #  |            $f6b1 (G)
    .byte   %00111100 ; |  ####  |            $f6b2 (G)
    .byte   %00111100 ; |  ####  |            $f6b3 (G)
    .byte   %01111100 ; | #####  |            $f6b4 (G)
    .byte   %10111110 ; |# ##### |            $f6b5 (G)
    .byte   %00111101 ; |  #### #|            $f6b6 (G)
    .byte   %00111100 ; |  ####  |            $f6b7 (G)
    .byte   %01100010 ; | ##   # |            $f6b8 (G)
    .byte   %00111110 ; |  ##### |            $f6b9 (G)
    .byte   %10111100 ; |# ####  |            $f6ba (G)
    .byte   %01111100 ; | #####  |            $f6bb (G)
    .byte   %00110110 ; |  ## ## |            $f6bc (G)
    .byte   %00110101 ; |  ## # #|            $f6bd (G)
    .byte   %00111100 ; |  ####  |            $f6be (G)
    .byte   %00100100 ; |  #  #  |            $f6bf (G)
    .byte   %00100100 ; |  #  #  |            $f6c0 (G)
    .byte   %00111100 ; |  ####  |            $f6c1 (G)
    .byte   %10111101 ; |# #### #|            $f6c2 (G)
    .byte   %01111110 ; | ###### |            $f6c3 (G)
    .byte   %00110100 ; |  ## #  |            $f6c4 (G)
    .byte   %00110100 ; |  ## #  |            $f6c5 (G)
    .byte   %00111100 ; |  ####  |            $f6c6 (G)
    .byte   %01001000 ; | #  #   |            $f6c7 (G)
    .byte   %00101000 ; |  # #   |            $f6c8 (G)
    .byte   %00111100 ; |  ####  |            $f6c9 (G)
    .byte   %10111101 ; |# #### #|            $f6ca (G)
    .byte   %01111110 ; | ###### |            $f6cb (G)
    .byte   %00011010 ; |   ## # |            $f6cc (G)
    .byte   %00011010 ; |   ## # |            $f6cd (G)
    .byte   %00011110 ; |   #### |            $f6ce (G)
    .byte   %00010000 ; |   #    |            $f6cf (G)
    .byte   %00011000 ; |   ##   |            $f6d0 (G)
    .byte   %00111100 ; |  ####  |            $f6d1 (G)
    .byte   %10111101 ; |# #### #|            $f6d2 (G)
    .byte   %01111110 ; | ###### |            $f6d3 (G)
    .byte   %00011010 ; |   ## # |            $f6d4 (G)
    .byte   %00011010 ; |   ## # |            $f6d5 (G)
    .byte   %00011110 ; |   #### |            $f6d6 (G)
    .byte   %01100001 ; | ##    #|            $f6d7 (G)
    .byte   %10111110 ; |# ##### |            $f6d8 (G)
    .byte   %11111100 ; |######  |            $f6d9 (G)
    .byte   %01101110 ; | ## ### |            $f6da (G)
    .byte   %01101000 ; | ## #   |            $f6db (G)
    .byte   %01111000 ; | ####   |            $f6dc (G)
    .byte   %00000000 ; |        |            $f6dd (G)
    .byte   %00000000 ; |        |            $f6de (G)
    
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f6df (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f6e7 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f6ef (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f6f7 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f6ff (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f707 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f70f (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f717 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f71f (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f727 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f72f (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f737 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f73f (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f747 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f74f (*)
    .byte   $00                             ; $f757 (*)
    
Pfcolortable2
    .byte   BLACK|$0, BLACK|$0, BLACK|$0, BLACK|$0
    .byte   BLACK|$0, BLACK|$0, BLACK|$2, BLACK|$0
    .byte   BLACK|$0, BLACK|$0, BLACK|$4, BLACK|$0
    .byte   BLACK|$0, BLACK|$0, BLACK|$6, BLACK|$0
    .byte   BLACK|$0, BLACK|$0, BLACK|$8, BLACK|$0
    .byte   BLACK|$0, BLACK|$0, BLACK|$a, BLACK|$0
    .byte   BLACK|$0, BLACK|$0, BLACK|$8, BLACK|$0
    .byte   BLACK|$0, BLACK|$0, BLACK|$6, BLACK|$0
    .byte   BLACK|$0, ORANGE|$0, BLACK|$4, BLACK|$0
    .byte   GREEN_YELLOW|$2, RED|$0, BLACK|$2, BLACK|$0
    .byte   BLACK|$0, RED|$0, BLACK|$0, BLACK|$0

    .byte   $77,$77,$77                 ; $f784 (*)
    .byte   $08,$77,$77,$77,$38,$3f,$3f,$77 ; $f787 (*)
    .byte   $7e,$7e,$0e,$1c,$1c,$7f,$77,$7f ; $f78f (*)
    .byte   $1c,$1c,$00,$ff,$ff,$ff,$ff,$ff ; $f797 (*)
    .byte   $ff,$ff,$ff,$00,$30,$38,$3c,$3c ; $f79f (*)
    .byte   $3c,$3c,$3c,$3c,$38,$30,$18,$18 ; $f7a7 (*)
    .byte   $18,$18,$18,$18,$18,$18,$18,$18 ; $f7af (*)
    .byte   $c3,$c3,$66,$66,$3c,$bd,$bd,$7f ; $f7b7 (*)
    .byte   $7e,$4c,$7c,$7c,$ae,$7c,$7c,$54 ; $f7bf (*)
    .byte   $81,$81,$e7,$3c,$18,$ff,$f3,$7e ; $f7c7 (*)
    .byte   $80,$81,$e1,$3f,$1c,$ff,$f3,$7e ; $f7cf (*)
    .byte   $01,$81,$87,$fc,$38,$ff,$f3,$7e ; $f7d7 (*)
    .byte   $ff,$7f,$2b,$60,$ea,$ff,$e6,$6e ; $f7df (*)
    .byte   $7c,$ff,$7f,$2b,$60,$ea,$ff,$e6 ; $f7e7 (*)
    .byte   $6e,$7c,$ff,$7f,$ea,$ff,$e6,$ee ; $f7ef (*)
    .byte   $7c,$38,$62,$3e,$bc,$7c,$36,$35 ; $f7f7 (*)
    .byte   $3c,$00,$00,$18,$3c,$3c,$18,$00 ; $f7ff (*)
    .byte   $00,$00,$08,$24,$1a,$40,$24,$42 ; $f807 (*)
    .byte   $10,$89,$42,$24,$01,$80,$24,$42 ; $f80f (*)
    .byte   $91,$0e,$0e,$7f,$7f,$fd,$fd,$fd ; $f817 (*)
    .byte   $fd,$b1,$b1,$1e,$1e,$08,$08,$7f ; $f81f (*)
    .byte   $7f,$0e,$0e,$7f,$7f,$fd,$fd,$fd ; $f827 (*)
    .byte   $fd,$b1,$b1,$1e,$1e,$08,$08,$3e ; $f82f (*)
    .byte   $3e,$0e,$0e,$7f,$7f,$fd,$fd,$fd ; $f837 (*)
    .byte   $fd,$b1,$b1,$1e,$1e,$08,$08,$1c ; $f83f (*)
    .byte   $1c,$3e,$1c,$1c,$08,$7f,$3e,$08 ; $f847 (*)
    .byte   $00,$00,$10,$14,$1c,$1c,$1c,$08 ; $f84f (*)
    .byte   $00,$00,$00,$02,$10,$1c,$38,$30 ; $f857 (*)
    .byte   $00,$00,$30,$30,$38,$10,$04,$3e ; $f85f (*)
    .byte   $1c,$0e,$06,$02,$81,$81,$e7,$3c ; $f867 (*)
    .byte   $18,$ff,$f3,$7e,$80,$81,$e1,$3f ; $f86f (*)
    .byte   $1c,$ff,$f3,$7e,$01,$81,$87,$fc ; $f877 (*)
    .byte   $38,$ff,$f3,$7e,$0e,$0e,$7f,$7f ; $f87f (*)
    .byte   $fd,$fd,$fd,$fd,$b1,$b1,$1e,$1e ; $f887 (*)
    .byte   $08,$08,$3e,$3e,$00,$00,$00,$00 ; $f88f (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f897 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f89f (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f8a7 (*)
    .byte   $30,$00,$00,$00,$30,$00,$00,$00 ; $f8af (*)
    .byte   $30,$00,$00,$00,$30,$00,$00,$00 ; $f8b7 (*)
    .byte   $bf,$fd,$fe,$bb,$7d,$ba,$bb,$39 ; $f8bf (*)
    .byte   $1d,$1c,$3c,$39,$38,$39,$38,$38 ; $f8c7 (*)
    .byte   $3c,$7e,$7e,$df,$9d,$15,$24,$08 ; $f8cf (*)
    .byte   $40,$20,$fe,$3d,$3e,$1e,$1a,$1a ; $f8d7 (*)
    .byte   $1e                             ; $f8df (*)
    
    .byte   %00100100 ; |  #  #  |            $f8e0 (G)
    .byte   %00100100 ; |  #  #  |            $f8e1 (G)
    .byte   %00111100 ; |  ####  |            $f8e2 (G)
    .byte   %10111101 ; |# #### #|            $f8e3 (G)
    .byte   %01111110 ; | ###### |            $f8e4 (G)
    .byte   %00110100 ; |  ## #  |            $f8e5 (G)
    .byte   %00110100 ; |  ## #  |            $f8e6 (G)
    .byte   %00111100 ; |  ####  |            $f8e7 (G)
    .byte   %00011000 ; |   ##   |            $f8e8 (G)
    .byte   %00011000 ; |   ##   |            $f8e9 (G)
    .byte   %00111100 ; |  ####  |            $f8ea (G)
    .byte   %00111100 ; |  ####  |            $f8eb (G)
    .byte   %01111110 ; | ###### |            $f8ec (G)
    .byte   %01111110 ; | ###### |            $f8ed (G)
    .byte   %00111100 ; |  ####  |            $f8ee (G)
    .byte   %00111100 ; |  ####  |            $f8ef (G)
    .byte   %00111100 ; |  ####  |            $f8f0 (G)
    .byte   %00011000 ; |   ##   |            $f8f1 (G)
    .byte   %00111100 ; |  ####  |            $f8f2 (G)
    .byte   %01111110 ; | ###### |            $f8f3 (G)
    .byte   %01111110 ; | ###### |            $f8f4 (G)
    .byte   %00111100 ; |  ####  |            $f8f5 (G)
    .byte   %00111100 ; |  ####  |            $f8f6 (G)
    .byte   %00111100 ; |  ####  |            $f8f7 (G)
    
    .byte   $00,$a3,$ff,$76,$34,$3c,$00,$00 ; $f8f8 (*)
    
    .byte   %01100010 ; | ##   # |            $f900 (G)
    .byte   %00111110 ; |  ##### |            $f901 (G)
    .byte   %10111100 ; |# ####  |            $f902 (G)
    .byte   %01111100 ; | #####  |            $f903 (G)
    .byte   %00110110 ; |  ## ## |            $f904 (G)
    .byte   %00110101 ; |  ## # #|            $f905 (G)
    .byte   %00111100 ; |  ####  |            $f906 (G)
    
    .byte   $61,$be,$fc,$6e,$68,$78         ; $f907 (*)
    
    .byte   %00001110 ; |    ### |            $f90d (G)
    .byte   %00010101 ; |   # # #|            $f90e (G)
    .byte   %00011011 ; |   ## ##|            $f90f (G)
    .byte   %00001110 ; |    ### |            $f910 (G)
    .byte   %00000100 ; |     #  |            $f911 (G)
    .byte   %00000110 ; |     ## |            $f912 (G)
    .byte   %00000100 ; |     #  |            $f913 (G)
    .byte   %00000110 ; |     ## |            $f914 (G)
    .byte   %10100101 ; |# #  # #|            $f915 (G)
    .byte   %11011011 ; |## ## ##|            $f916 (G)
    .byte   %11111111 ; |########|            $f917 (G)
    .byte   %01100110 ; | ##  ## |            $f918 (G)
    .byte   %01100110 ; | ##  ## |            $f919 (G)
    .byte   %00100100 ; |  #  #  |            $f91a (G)
    .byte   %00000000 ; |        |            $f91b (G)
    .byte   %00100100 ; |  #  #  |            $f91c (G)
    .byte   %00011000 ; |   ##   |            $f91d (G)
    .byte   %00111100 ; |  ####  |            $f91e (G)
    .byte   %01100110 ; | ##  ## |            $f91f (G)
    .byte   %11100111 ; |###  ###|            $f920 (G)
    .byte   %11100111 ; |###  ###|            $f921 (G)
    .byte   %11000011 ; |##    ##|            $f922 (G)
    .byte   %00111100 ; |  ####  |            $f923 (G)
    .byte   %01111110 ; | ###### |            $f924 (G)
    .byte   %11000010 ; |##    # |            $f925 (G)
    .byte   %10100111 ; |# #  ###|            $f926 (G)
    .byte   %11011111 ; |## #####|            $f927 (G)
    .byte   %11111111 ; |########|            $f928 (G)
    .byte   %10111111 ; |# ######|            $f929 (G)
    .byte   %10111111 ; |# ######|            $f92a (G)
    .byte   %00110101 ; |  ## # #|            $f92b (G)
    .byte   %01010100 ; | # # #  |            $f92c (G)
    .byte   %01010100 ; | # # #  |            $f92d (G)
    .byte   %00010000 ; |   #    |            $f92e (G)
    .byte   %00010100 ; |   # #  |            $f92f (G)
    .byte   %00010000 ; |   #    |            $f930 (G)
    .byte   %00000000 ; |        |            $f931 (G)
    .byte   %00010000 ; |   #    |            $f932 (G)
    .byte   %00010000 ; |   #    |            $f933 (G)
    .byte   %00000000 ; |        |            $f934 (G)
    .byte   %00111100 ; |  ####  |            $f935 (G)
    .byte   %01111110 ; | ###### |            $f936 (G)
    .byte   %11110010 ; |####  # |            $f937 (G)
    .byte   %10100011 ; |# #   ##|            $f938 (G)
    .byte   %11000011 ; |##    ##|            $f939 (G)
    .byte   %11101111 ; |### ####|            $f93a (G)
    .byte   %10111111 ; |# ######|            $f93b (G)
    .byte   %10111111 ; |# ######|            $f93c (G)
    .byte   %00110101 ; |  ## # #|            $f93d (G)
    .byte   %01010100 ; | # # #  |            $f93e (G)
    .byte   %01010100 ; | # # #  |            $f93f (G)
    .byte   %01010000 ; | # #    |            $f940 (G)
    .byte   %00010000 ; |   #    |            $f941 (G)
    .byte   %00010000 ; |   #    |            $f942 (G)
    .byte   %01000001 ; | #     #|            $f943 (G)
    .byte   %01000001 ; | #     #|            $f944 (G)
    .byte   %00000001 ; |       #|            $f945 (G)
    .byte   %00000000 ; |        |            $f946 (G)
    
    .byte   $20,$10,$18,$0c,$fe,$7f,$70,$38 ; $f947 (*)
    .byte   $3c                             ; $f94f (*)
    
    .byte   %01110110 ; | ### ## |            $f950 (G)
    .byte   %00110100 ; |  ## #  |            $f951 (G)
    .byte   %00101100 ; |  # ##  |            $f952 (G)
    .byte   %00111000 ; |  ###   |            $f953 (G)
    .byte   %00011000 ; |   ##   |            $f954 (G)
    .byte   %00011000 ; |   ##   |            $f955 (G)
    .byte   %00010000 ; |   #    |            $f956 (G)
    .byte   %00010010 ; |   #  # |            $f957 (G)
    .byte   %01110110 ; | ### ## |            $f958 (G)
    .byte   %00100100 ; |  #  #  |            $f959 (G)
    .byte   %00101100 ; |  # ##  |            $f95a (G)
    .byte   %00011000 ; |   ##   |            $f95b (G)
    .byte   %00011000 ; |   ##   |            $f95c (G)
    .byte   %00010100 ; |   # #  |            $f95d (G)
    .byte   %01110110 ; | ### ## |            $f95e (G)
    .byte   %00111100 ; |  ####  |            $f95f (G)
    .byte   %00111100 ; |  ####  |            $f960 (G)
    .byte   %00011000 ; |   ##   |            $f961 (G)
    .byte   %00010000 ; |   #    |            $f962 (G)
    
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $f963 (*)
    .byte   $ff                             ; $f96b (*)
    
    .byte   %00111000 ; |  ###   |            $f96c (G)
    .byte   %00011100 ; |   ###  |            $f96d (G)
    .byte   %01011000 ; | # ##   |            $f96e (G)
    .byte   %00001000 ; |    #   |            $f96f (G)
    .byte   %00001001 ; |    #  #|            $f970 (G)
    .byte   %00001000 ; |    #   |            $f971 (G)
    .byte   %01001000 ; | #  #   |            $f972 (G)
    .byte   %00011000 ; |   ##   |            $f973 (G)
    .byte   %00011000 ; |   ##   |            $f974 (G)
    .byte   %00011000 ; |   ##   |            $f975 (G)
    .byte   %00011000 ; |   ##   |            $f976 (G)
    .byte   %00010000 ; |   #    |            $f977 (G)
    .byte   %00010000 ; |   #    |            $f978 (G)
    .byte   %00010000 ; |   #    |            $f979 (G)
    .byte   %00010000 ; |   #    |            $f97a (G)
    .byte   %00010000 ; |   #    |            $f97b (G)
    .byte   %00110010 ; |  ##  # |            $f97c (G)
    .byte   %00100000 ; |  #     |            $f97d (G)
    .byte   %00100000 ; |  #     |            $f97e (G)
    .byte   %00100000 ; |  #     |            $f97f (G)
    .byte   %00110000 ; |  ##    |            $f980 (G)
    .byte   %01100000 ; | ##     |            $f981 (G)
    .byte   %01000000 ; | #      |            $f982 (G)
    .byte   %01000000 ; | #      |            $f983 (G)
    .byte   %01000000 ; | #      |            $f984 (G)
    .byte   %01100000 ; | ##     |            $f985 (G)
    .byte   %00110000 ; |  ##    |            $f986 (G)
    .byte   %00010000 ; |   #    |            $f987 (G)
    .byte   %00011000 ; |   ##   |            $f988 (G)
    .byte   %00001100 ; |    ##  |            $f989 (G)
    .byte   %00001100 ; |    ##  |            $f98a (G)
    .byte   %00001100 ; |    ##  |            $f98b (G)
    .byte   %00000100 ; |     #  |            $f98c (G)
    .byte   %00000110 ; |     ## |            $f98d (G)
    .byte   %00000110 ; |     ## |            $f98e (G)
    .byte   %00001110 ; |    ### |            $f98f (G)
    .byte   %00001000 ; |    #   |            $f990 (G)
    .byte   %00001100 ; |    ##  |            $f991 (G)
    .byte   %00001000 ; |    #   |            $f992 (G)
    .byte   %00001000 ; |    #   |            $f993 (G)
    .byte   %00001000 ; |    #   |            $f994 (G)
    .byte   %00001000 ; |    #   |            $f995 (G)
    .byte   %00001000 ; |    #   |            $f996 (G)
    .byte   %00001000 ; |    #   |            $f997 (G)
    .byte   %00000100 ; |     #  |            $f998 (G)
    .byte   %00000100 ; |     #  |            $f999 (G)
    
    .byte   $24,$0c,$60,$23,$3e,$18,$bc,$fe ; $f99a (*)
    .byte   $7f,$25,$3d,$e7,$7e,$ff,$1f,$ff ; $f9a2 (*)
    .byte   $16,$1e,$00,$1f,$1f,$0f,$1f,$3e ; $f9aa (*)
    .byte   $3c,$3c,$3c,$1e,$0f,$07,$0f,$0f ; $f9b2 (*)
    .byte   $0e,$1c,$18,$0c,$0c,$06,$00,$00 ; $f9ba (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f9c2 (*)
    .byte   $00,$60,$30,$30,$18,$38,$70,$f0 ; $f9ca (*)
    .byte   $f0,$e0,$f0,$78,$3c,$3c,$3c,$7c ; $f9d2 (*)
    .byte   $f8,$f0,$f8,$00,$00,$00,$00,$00 ; $f9da (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f9e2 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f9ea (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f9f2 (*)
    .byte   $00,$00,$00,$00,$00,$00,$3e,$1f ; $f9fa (*)
    .byte   $0f,$1f,$3f,$3e,$3c,$3c,$3e,$1f ; $fa02 (*)
    .byte   $0f,$0f,$1f,$3e,$3c,$3c,$3c,$1e ; $fa0a (*)
    .byte   $0f,$07,$0f,$0f,$0e,$1c,$18,$0c ; $fa12 (*)
    .byte   $0c,$06,$00,$00,$60,$30,$30,$18 ; $fa1a (*)
    .byte   $38,$70,$f0,$f0,$e0,$f0,$78,$3c ; $fa22 (*)
    .byte   $3c,$3c,$7c,$f8,$f0,$f8,$1f,$0f ; $fa2a (*)
    .byte   $1f,$3e,$3c,$3c,$3c,$1e,$0f,$07 ; $fa32 (*)
    .byte   $0f,$0f,$0e,$1c,$18,$0c,$0c,$06 ; $fa3a (*)
    .byte   $00,$00,$60,$30,$30,$18,$38,$70 ; $fa42 (*)
    .byte   $f0,$f0,$e0,$f0,$78,$3c,$3c,$3c ; $fa4a (*)
    .byte   $7c,$f8,$f0,$f0,$f8,$7c,$3c,$3c ; $fa52 (*)
    .byte   $7c,$fc,$f8,$f0,$f8,$7c,$12,$14 ; $fa5a (*)
    .byte   $5c,$7c,$fe,$1e,$16,$16,$1e     ; $fa62 (*)
    
    .byte   %01001000 ; | #  #   |            $fa69 (G)
    .byte   %00101000 ; |  # #   |            $fa6a (G)
    .byte   %00111100 ; |  ####  |            $fa6b (G)
    .byte   %10111101 ; |# #### #|            $fa6c (G)
    .byte   %01111110 ; | ###### |            $fa6d (G)
    .byte   %00011010 ; |   ## # |            $fa6e (G)
    .byte   %00011010 ; |   ## # |            $fa6f (G)
    .byte   %00011110 ; |   #### |            $fa70 (G)
    .byte   %00010000 ; |   #    |            $fa71 (G)
    .byte   %00011000 ; |   ##   |            $fa72 (G)
    .byte   %00111100 ; |  ####  |            $fa73 (G)
    .byte   %10111101 ; |# #### #|            $fa74 (G)
    .byte   %01111110 ; | ###### |            $fa75 (G)
    .byte   %00011010 ; |   ## # |            $fa76 (G)
    .byte   %00011010 ; |   ## # |            $fa77 (G)
    .byte   %00011110 ; |   #### |            $fa78 (G)
    .byte   %10001001 ; |#   #  #|            $fa79 (G)
    .byte   %01000010 ; | #    # |            $fa7a (G)
    .byte   %00100100 ; |  #  #  |            $fa7b (G)
    .byte   %00000001 ; |       #|            $fa7c (G)
    .byte   %10000000 ; |#       |            $fa7d (G)
    .byte   %00100100 ; |  #  #  |            $fa7e (G)
    .byte   %01000010 ; | #    # |            $fa7f (G)
    .byte   %10010001 ; |#  #   #|            $fa80 (G)
    .byte   %10001001 ; |#   #  #|            $fa81 (G)
    .byte   %01000010 ; | #    # |            $fa82 (G)
    .byte   %00100100 ; |  #  #  |            $fa83 (G)
    .byte   %00000001 ; |       #|            $fa84 (G)
    .byte   %10000000 ; |#       |            $fa85 (G)
    .byte   %00100100 ; |  #  #  |            $fa86 (G)
    .byte   %01000010 ; | #    # |            $fa87 (G)
    .byte   %10010001 ; |#  #   #|            $fa88 (G)
    
    .byte   $0f,$33,$7f,$2f,$4f,$ff,$f7,$1d ; $fa89 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fa91 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fa99 (*)
    .byte   $0f,$33,$7f,$2f,$4f,$ff,$f7,$1d ; $faa1 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $faa9 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fab1 (*)
    .byte   $0f,$33,$7f,$2f,$4f,$ff,$f7,$1d ; $fab9 (*)
    .byte   $08,$1c,$3e,$7f,$5d,$1c,$08,$08 ; $fac1 (*)
    .byte   $08                             ; $fac9 (*)
    
    .byte   %00111100 ; |  ####  |            $faca (G)
    .byte   %01000010 ; | #    # |            $facb (G)
    .byte   %00111100 ; |  ####  |            $facc (G)
    .byte   %01100110 ; | ##  ## |            $facd (G)
    .byte   %10011001 ; |#  ##  #|            $face (G)
    .byte   %10011001 ; |#  ##  #|            $facf (G)
    .byte   %11111111 ; |########|            $fad0 (G)
    .byte   %01111110 ; | ###### |            $fad1 (G)
    .byte   %00111100 ; |  ####  |            $fad2 (G)
    
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fad3 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fadb (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fae3 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $faeb (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $faf3 (*)
    .byte   $00,$00,$00,$00,$00             ; $fafb (*)
    
    .byte   %10000001 ; |#      #|            $fb00 (G)
    .byte   %11111111 ; |########|            $fb01 (G)
    .byte   %10000001 ; |#      #|            $fb02 (G)
    .byte   %10000001 ; |#      #|            $fb03 (G)
    .byte   %11111111 ; |########|            $fb04 (G)
    .byte   %10000001 ; |#      #|            $fb05 (G)
    .byte   %10000001 ; |#      #|            $fb06 (G)
    .byte   %11111111 ; |########|            $fb07 (G)
    .byte   %10000001 ; |#      #|            $fb08 (G)
    .byte   %10000001 ; |#      #|            $fb09 (G)
    .byte   %11111111 ; |########|            $fb0a (G)
    .byte   %10000001 ; |#      #|            $fb0b (G)
    .byte   %10000001 ; |#      #|            $fb0c (G)
    .byte   %11111111 ; |########|            $fb0d (G)
    .byte   %10000001 ; |#      #|            $fb0e (G)
    .byte   %10000001 ; |#      #|            $fb0f (G)
    .byte   %11111111 ; |########|            $fb10 (G)
    .byte   %10000001 ; |#      #|            $fb11 (G)
    .byte   %10000001 ; |#      #|            $fb12 (G)
    .byte   %11111111 ; |########|            $fb13 (G)
    .byte   %10000001 ; |#      #|            $fb14 (G)
    .byte   %10000001 ; |#      #|            $fb15 (G)
    .byte   %11111111 ; |########|            $fb16 (G)
    .byte   %10000001 ; |#      #|            $fb17 (G)
    .byte   %10000001 ; |#      #|            $fb18 (G)
    .byte   %11111111 ; |########|            $fb19 (G)
    .byte   %10000001 ; |#      #|            $fb1a (G)
    .byte   %10000001 ; |#      #|            $fb1b (G)
    .byte   %11111111 ; |########|            $fb1c (G)
    .byte   %10000001 ; |#      #|            $fb1d (G)
    .byte   %10000001 ; |#      #|            $fb1e (G)
    .byte   %11111111 ; |########|            $fb1f (G)
    .byte   %10000001 ; |#      #|            $fb20 (G)
    .byte   %10000001 ; |#      #|            $fb21 (G)
    .byte   %11111111 ; |########|            $fb22 (G)
    .byte   %10000001 ; |#      #|            $fb23 (G)
    .byte   %10000001 ; |#      #|            $fb24 (G)
    .byte   %11111111 ; |########|            $fb25 (G)
    .byte   %10000001 ; |#      #|            $fb26 (G)
    .byte   %10000001 ; |#      #|            $fb27 (G)
    .byte   %11111111 ; |########|            $fb28 (G)
    .byte   %10000001 ; |#      #|            $fb29 (G)
    .byte   %10000001 ; |#      #|            $fb2a (G)
    .byte   %11111111 ; |########|            $fb2b (G)
    .byte   %10000001 ; |#      #|            $fb2c (G)
    .byte   %10000001 ; |#      #|            $fb2d (G)
    .byte   %11111111 ; |########|            $fb2e (G)
    .byte   %10000001 ; |#      #|            $fb2f (G)
    .byte   %10000001 ; |#      #|            $fb30 (G)
    .byte   %11111111 ; |########|            $fb31 (G)
    .byte   %10000001 ; |#      #|            $fb32 (G)
    .byte   %10000001 ; |#      #|            $fb33 (G)
    .byte   %11111111 ; |########|            $fb34 (G)
    .byte   %10000001 ; |#      #|            $fb35 (G)
    .byte   %10000001 ; |#      #|            $fb36 (G)
    .byte   %11111111 ; |########|            $fb37 (G)
    .byte   %10000001 ; |#      #|            $fb38 (G)
    .byte   %10000001 ; |#      #|            $fb39 (G)
    .byte   %11111111 ; |########|            $fb3a (G)
    .byte   %10000001 ; |#      #|            $fb3b (G)
    .byte   %10000001 ; |#      #|            $fb3c (G)
    .byte   %11111111 ; |########|            $fb3d (G)
    .byte   %10000001 ; |#      #|            $fb3e (G)
    .byte   %10000001 ; |#      #|            $fb3f (G)
    .byte   %11111111 ; |########|            $fb40 (G)
    .byte   %10000001 ; |#      #|            $fb41 (G)
    .byte   %10000001 ; |#      #|            $fb42 (G)
    .byte   %11111111 ; |########|            $fb43 (G)
    .byte   %10000001 ; |#      #|            $fb44 (G)
    .byte   %10000001 ; |#      #|            $fb45 (G)
    .byte   %11111111 ; |########|            $fb46 (G)
    .byte   %10000001 ; |#      #|            $fb47 (G)
    .byte   %10000001 ; |#      #|            $fb48 (G)
    .byte   %11111111 ; |########|            $fb49 (G)
    .byte   %10000001 ; |#      #|            $fb4a (G)
    .byte   %10000001 ; |#      #|            $fb4b (G)
    .byte   %11111111 ; |########|            $fb4c (G)
    .byte   %10000001 ; |#      #|            $fb4d (G)
    .byte   %10000001 ; |#      #|            $fb4e (G)
    .byte   %00001000 ; |    #   |            $fb4f (G)
    .byte   %00011100 ; |   ###  |            $fb50 (G)
    .byte   %01111100 ; | #####  |            $fb51 (G)
    .byte   %11100110 ; |###  ## |            $fb52 (G)
    .byte   %01100111 ; | ##  ###|            $fb53 (G)
    .byte   %00111110 ; |  ##### |            $fb54 (G)
    .byte   %00111000 ; |  ###   |            $fb55 (G)
    .byte   %00010000 ; |   #    |            $fb56 (G)
    .byte   %00000100 ; |     #  |            $fb57 (G)
    .byte   %01001110 ; | #  ### |            $fb58 (G)
    .byte   %11111100 ; |######  |            $fb59 (G)
    .byte   %01100100 ; | ##  #  |            $fb5a (G)
    .byte   %00100110 ; |  #  ## |            $fb5b (G)
    .byte   %00111111 ; |  ######|            $fb5c (G)
    .byte   %01110010 ; | ###  # |            $fb5d (G)
    .byte   %00100000 ; |  #     |            $fb5e (G)
    .byte   %01000010 ; | #    # |            $fb5f (G)
    .byte   %11000111 ; |##   ###|            $fb60 (G)
    .byte   %01111100 ; | #####  |            $fb61 (G)
    .byte   %00100100 ; |  #  #  |            $fb62 (G)
    .byte   %00100100 ; |  #  #  |            $fb63 (G)
    .byte   %00111110 ; |  ##### |            $fb64 (G)
    .byte   %11100011 ; |###   ##|            $fb65 (G)
    .byte   %01000010 ; | #    # |            $fb66 (G)
    .byte   %00000000 ; |        |            $fb67 (G)
    .byte   %11111111 ; |########|            $fb68 (G)
    .byte   %11111111 ; |########|            $fb69 (G)
    .byte   %11111111 ; |########|            $fb6a (G)
    .byte   %11111111 ; |########|            $fb6b (G)
    .byte   %11111111 ; |########|            $fb6c (G)
    .byte   %11111111 ; |########|            $fb6d (G)
    .byte   %00000000 ; |        |            $fb6e (G)
    .byte   %00110000 ; |  ##    |            $fb6f (G)
    .byte   %00111000 ; |  ###   |            $fb70 (G)
    .byte   %00111100 ; |  ####  |            $fb71 (G)
    .byte   %00111100 ; |  ####  |            $fb72 (G)
    .byte   %00111100 ; |  ####  |            $fb73 (G)
    .byte   %00111100 ; |  ####  |            $fb74 (G)
    .byte   %00111000 ; |  ###   |            $fb75 (G)
    .byte   %00110000 ; |  ##    |            $fb76 (G)
    .byte   %00011000 ; |   ##   |            $fb77 (G)
    .byte   %00011000 ; |   ##   |            $fb78 (G)
    .byte   %00011000 ; |   ##   |            $fb79 (G)
    .byte   %00011000 ; |   ##   |            $fb7a (G)
    .byte   %00011000 ; |   ##   |            $fb7b (G)
    .byte   %00011000 ; |   ##   |            $fb7c (G)
    .byte   %00011000 ; |   ##   |            $fb7d (G)
    .byte   %00011000 ; |   ##   |            $fb7e (G)
    .byte   %00011000 ; |   ##   |            $fb7f (G)
    .byte   %00111100 ; |  ####  |            $fb80 (G)
    .byte   %00111100 ; |  ####  |            $fb81 (G)
    .byte   %01111110 ; | ###### |            $fb82 (G)
    .byte   %01111110 ; | ###### |            $fb83 (G)
    .byte   %01111110 ; | ###### |            $fb84 (G)
    .byte   %01111110 ; | ###### |            $fb85 (G)
    .byte   %11111111 ; |########|            $fb86 (G)
    .byte   %11111111 ; |########|            $fb87 (G)
    .byte   %11111111 ; |########|            $fb88 (G)
    .byte   %11111111 ; |########|            $fb89 (G)
    .byte   %11111111 ; |########|            $fb8a (G)
    .byte   %11111111 ; |########|            $fb8b (G)
    .byte   %11111111 ; |########|            $fb8c (G)
    .byte   %11111111 ; |########|            $fb8d (G)
    .byte   %11111111 ; |########|            $fb8e (G)
    .byte   %11111111 ; |########|            $fb8f (G)
    .byte   %11111111 ; |########|            $fb90 (G)
    .byte   %01111110 ; | ###### |            $fb91 (G)
    .byte   %01111110 ; | ###### |            $fb92 (G)
    .byte   %01111110 ; | ###### |            $fb93 (G)
    .byte   %00111100 ; |  ####  |            $fb94 (G)
    .byte   %00111100 ; |  ####  |            $fb95 (G)
    .byte   %00011000 ; |   ##   |            $fb96 (G)
    .byte   %00111000 ; |  ###   |            $fb97 (G)
    .byte   %00011100 ; |   ###  |            $fb98 (G)
    .byte   %01011000 ; | # ##   |            $fb99 (G)
    .byte   %00001000 ; |    #   |            $fb9a (G)
    .byte   %00001001 ; |    #  #|            $fb9b (G)
    .byte   %00001000 ; |    #   |            $fb9c (G)
    .byte   %01001000 ; | #  #   |            $fb9d (G)
    .byte   %00011000 ; |   ##   |            $fb9e (G)
    .byte   %00011000 ; |   ##   |            $fb9f (G)
    .byte   %00011000 ; |   ##   |            $fba0 (G)
    .byte   %00011000 ; |   ##   |            $fba1 (G)
    .byte   %00010000 ; |   #    |            $fba2 (G)
    .byte   %00010000 ; |   #    |            $fba3 (G)
    .byte   %00010000 ; |   #    |            $fba4 (G)
    .byte   %00010000 ; |   #    |            $fba5 (G)
    .byte   %00010000 ; |   #    |            $fba6 (G)
    .byte   %00110010 ; |  ##  # |            $fba7 (G)
    .byte   %00100000 ; |  #     |            $fba8 (G)
    .byte   %00100000 ; |  #     |            $fba9 (G)
    .byte   %00100000 ; |  #     |            $fbaa (G)
    .byte   %00110000 ; |  ##    |            $fbab (G)
    .byte   %01100000 ; | ##     |            $fbac (G)
    .byte   %01000000 ; | #      |            $fbad (G)
    .byte   %01000000 ; | #      |            $fbae (G)
    .byte   %01000000 ; | #      |            $fbaf (G)
    .byte   %01100000 ; | ##     |            $fbb0 (G)
    .byte   %00110000 ; |  ##    |            $fbb1 (G)
    .byte   %00010000 ; |   #    |            $fbb2 (G)
    .byte   %00011000 ; |   ##   |            $fbb3 (G)
    .byte   %00001100 ; |    ##  |            $fbb4 (G)
    .byte   %00001100 ; |    ##  |            $fbb5 (G)
    .byte   %00001100 ; |    ##  |            $fbb6 (G)
    .byte   %00000100 ; |     #  |            $fbb7 (G)
    .byte   %00000110 ; |     ## |            $fbb8 (G)
    .byte   %00000110 ; |     ## |            $fbb9 (G)
    .byte   %00001110 ; |    ### |            $fbba (G)
    .byte   %00001000 ; |    #   |            $fbbb (G)
    .byte   %00001100 ; |    ##  |            $fbbc (G)
    .byte   %00001000 ; |    #   |            $fbbd (G)
    .byte   %00001000 ; |    #   |            $fbbe (G)
    .byte   %00001000 ; |    #   |            $fbbf (G)
    .byte   %00001000 ; |    #   |            $fbc0 (G)
    .byte   %00001000 ; |    #   |            $fbc1 (G)
    .byte   %00001000 ; |    #   |            $fbc2 (G)
    .byte   %00000100 ; |     #  |            $fbc3 (G)
    .byte   %00000100 ; |     #  |            $fbc4 (G)
    .byte   %00100100 ; |  #  #  |            $fbc5 (G)
    .byte   %00001100 ; |    ##  |            $fbc6 (G)
    .byte   %00110000 ; |  ##    |            $fbc7 (G)
    .byte   %00100000 ; |  #     |            $fbc8 (G)
    .byte   %00100000 ; |  #     |            $fbc9 (G)
    .byte   %00100000 ; |  #     |            $fbca (G)
    .byte   %00010000 ; |   #    |            $fbcb (G)
    .byte   %00010000 ; |   #    |            $fbcc (G)
    .byte   %00010000 ; |   #    |            $fbcd (G)
    .byte   %00010000 ; |   #    |            $fbce (G)
    .byte   %00010000 ; |   #    |            $fbcf (G)
    .byte   %00010000 ; |   #    |            $fbd0 (G)
    .byte   %00110000 ; |  ##    |            $fbd1 (G)
    .byte   %00010000 ; |   #    |            $fbd2 (G)
    .byte   %01110000 ; | ###    |            $fbd3 (G)
    .byte   %01100000 ; | ##     |            $fbd4 (G)
    .byte   %01100000 ; | ##     |            $fbd5 (G)
    .byte   %00100000 ; |  #     |            $fbd6 (G)
    .byte   %00110000 ; |  ##    |            $fbd7 (G)
    .byte   %00110000 ; |  ##    |            $fbd8 (G)
    .byte   %00110000 ; |  ##    |            $fbd9 (G)
    .byte   %00011000 ; |   ##   |            $fbda (G)
    .byte   %00001000 ; |    #   |            $fbdb (G)
    .byte   %00001100 ; |    ##  |            $fbdc (G)
    .byte   %00000110 ; |     ## |            $fbdd (G)
    .byte   %00000010 ; |      # |            $fbde (G)
    .byte   %00000010 ; |      # |            $fbdf (G)
    .byte   %00000010 ; |      # |            $fbe0 (G)
    .byte   %00011110 ; |   #### |            $fbe1 (G)
    .byte   %00001100 ; |    ##  |            $fbe2 (G)
    .byte   %00000100 ; |     #  |            $fbe3 (G)
    .byte   %00000100 ; |     #  |            $fbe4 (G)
    .byte   %00000100 ; |     #  |            $fbe5 (G)
    .byte   %00001100 ; |    ##  |            $fbe6 (G)
    .byte   %00001000 ; |    #   |            $fbe7 (G)
    .byte   %00001000 ; |    #   |            $fbe8 (G)
    .byte   %00001000 ; |    #   |            $fbe9 (G)
    .byte   %00001000 ; |    #   |            $fbea (G)
    .byte   %00001000 ; |    #   |            $fbeb (G)
    .byte   %00011000 ; |   ##   |            $fbec (G)
    .byte   %00011000 ; |   ##   |            $fbed (G)
    .byte   %00011000 ; |   ##   |            $fbee (G)
    .byte   %00011000 ; |   ##   |            $fbef (G)
    .byte   %00010000 ; |   #    |            $fbf0 (G)
    .byte   %00010000 ; |   #    |            $fbf1 (G)
    .byte   %00010000 ; |   #    |            $fbf2 (G)
    .byte   %00010000 ; |   #    |            $fbf3 (G)
    .byte   %00011000 ; |   ##   |            $fbf4 (G)
    .byte   %00001000 ; |    #   |            $fbf5 (G)
    .byte   %00011100 ; |   ###  |            $fbf6 (G)
    
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fbf7 (*)
    .byte   $00                             ; $fbff (*)
    
    .byte   %00010010 ; |   #  # |            $fc00 (G)
    .byte   %00010010 ; |   #  # |            $fc01 (G)
    .byte   %00110110 ; |  ## ## |            $fc02 (G)
    .byte   %10100101 ; |# #  # #|            $fc03 (G)
    .byte   %10111101 ; |# #### #|            $fc04 (G)
    .byte   %01111110 ; | ###### |            $fc05 (G)
    .byte   %01111110 ; | ###### |            $fc06 (G)
    .byte   %00111100 ; |  ####  |            $fc07 (G)
    .byte   %01111110 ; | ###### |            $fc08 (G)
    .byte   %01001110 ; | #  ### |            $fc09 (G)
    .byte   %01001110 ; | #  ### |            $fc0a (G)
    .byte   %01111110 ; | ###### |            $fc0b (G)
    .byte   %00111110 ; |  ##### |            $fc0c (G)
    .byte   %00100000 ; |  #     |            $fc0d (G)
    .byte   %00110000 ; |  ##    |            $fc0e (G)
    .byte   %00111111 ; |  ######|            $fc0f (G)
    .byte   %00100110 ; |  #  ## |            $fc10 (G)
    .byte   %01100100 ; | ##  #  |            $fc11 (G)
    .byte   %11111100 ; |######  |            $fc12 (G)
    .byte   %00001100 ; |    ##  |            $fc13 (G)
    .byte   %00000100 ; |     #  |            $fc14 (G)
    .byte   %01000000 ; | #      |            $fc15 (G)
    .byte   %00110001 ; |  ##   #|            $fc16 (G)
    .byte   %00111110 ; |  ##### |            $fc17 (G)
    .byte   %00100110 ; |  #  ## |            $fc18 (G)
    .byte   %01100100 ; | ##  #  |            $fc19 (G)
    .byte   %01111100 ; | #####  |            $fc1a (G)
    .byte   %10001100 ; |#   ##  |            $fc1b (G)
    .byte   %00000010 ; |      # |            $fc1c (G)
    .byte   %00010000 ; |   #    |            $fc1d (G)
    .byte   %00101000 ; |  # #   |            $fc1e (G)
    .byte   %01010100 ; | # # #  |            $fc1f (G)
    .byte   %10111010 ; |# ### # |            $fc20 (G)
    .byte   %10111010 ; |# ### # |            $fc21 (G)
    .byte   %10010010 ; |#  #  # |            $fc22 (G)
    .byte   %01101100 ; | ## ##  |            $fc23 (G)
    
    .byte   $bc,$fe,$bf,$e6,$bc,$0c,$10,$80 ; $fc24 (*)
    .byte   $82,$41,$01,$08,$30,$00,$0c,$50 ; $fc2c (*)
    .byte   $44,$22,$0a,$30,$00,$00,$00,$10 ; $fc34 (*)
    .byte   $1c,$38,$08,$00,$00,$18,$3c,$3c ; $fc3c (*)
    .byte   $3c,$3c,$3c,$3c,$7e,$ff,$ff,$bd ; $fc44 (*)
    .byte   $3c,$18,$18,$0c,$07,$0f,$0c,$18 ; $fc4c (*)
    .byte   $3c,$3c,$3c,$3c,$3c,$3c,$7e,$ff ; $fc54 (*)
    .byte   $ff,$bd,$3c,$18,$18,$18,$3c,$7e ; $fc5c (*)
    .byte   $66,$66,$7e,$3c,$18,$18,$18,$3c ; $fc64 (*)
    .byte   $bd,$ff,$ff,$7e,$3c,$3c,$3c,$3c ; $fc6c (*)
    .byte   $3c,$3c,$18,$30,$f0,$e0,$30,$18 ; $fc74 (*)
    .byte   $18,$3c,$bd,$ff,$ff,$7e,$3c,$3c ; $fc7c (*)
    .byte   $3c,$3c,$3c,$3c,$18             ; $fc84 (*)
    
    .byte   %01111111 ; | #######|            $fc89 (G)
    .byte   %01000001 ; | #     #|            $fc8a (G)
    .byte   %01100011 ; | ##   ##|            $fc8b (G)
    .byte   %01110111 ; | ### ###|            $fc8c (G)
    .byte   %01111111 ; | #######|            $fc8d (G)
    .byte   %00011100 ; |   ###  |            $fc8e (G)
    .byte   %01111111 ; | #######|            $fc8f (G)
    .byte   %01111111 ; | #######|            $fc90 (G)
    .byte   %01111111 ; | #######|            $fc91 (G)
    .byte   %01000001 ; | #     #|            $fc92 (G)
    .byte   %01100011 ; | ##   ##|            $fc93 (G)
    .byte   %01110111 ; | ### ###|            $fc94 (G)
    .byte   %01111111 ; | #######|            $fc95 (G)
    .byte   %00011100 ; |   ###  |            $fc96 (G)
    .byte   %00001000 ; |    #   |            $fc97 (G)
    .byte   %00011100 ; |   ###  |            $fc98 (G)
    .byte   %01111111 ; | #######|            $fc99 (G)
    .byte   %01111111 ; | #######|            $fc9a (G)
    .byte   %11111111 ; |########|            $fc9b (G)
    .byte   %10001000 ; |#   #   |            $fc9c (G)
    .byte   %10011010 ; |#  ## # |            $fc9d (G)
    .byte   %11101010 ; |### # # |            $fc9e (G)
    .byte   %10001000 ; |#   #   |            $fc9f (G)
    .byte   %11111111 ; |########|            $fca0 (G)
    .byte   %11011010 ; |## ## # |            $fca1 (G)
    .byte   %10101000 ; |# # #   |            $fca2 (G)
    .byte   %10101010 ; |# # # # |            $fca3 (G)
    .byte   %10101010 ; |# # # # |            $fca4 (G)
    .byte   %11111111 ; |########|            $fca5 (G)
    .byte   %00000000 ; |        |            $fca6 (G)
    .byte   %11111111 ; |########|            $fca7 (G)
    .byte   %11000100 ; |##   #  |            $fca8 (G)
    .byte   %11000100 ; |##   #  |            $fca9 (G)
    .byte   %11010101 ; |## # # #|            $fcaa (G)
    .byte   %11010101 ; |## # # #|            $fcab (G)
    .byte   %11010101 ; |## # # #|            $fcac (G)
    .byte   %11010101 ; |## # # #|            $fcad (G)
    .byte   %11011101 ; |## ### #|            $fcae (G)
    .byte   %11011101 ; |## ### #|            $fcaf (G)
    .byte   %11000101 ; |##   # #|            $fcb0 (G)
    .byte   %11000101 ; |##   # #|            $fcb1 (G)
    .byte   %11111111 ; |########|            $fcb2 (G)
    .byte   %11111111 ; |########|            $fcb3 (G)
    .byte   %11000101 ; |##   # #|            $fcb4 (G)
    .byte   %11000101 ; |##   # #|            $fcb5 (G)
    .byte   %11010101 ; |## # # #|            $fcb6 (G)
    .byte   %11010101 ; |## # # #|            $fcb7 (G)
    .byte   %11010101 ; |## # # #|            $fcb8 (G)
    .byte   %11010101 ; |## # # #|            $fcb9 (G)
    .byte   %11110101 ; |#### # #|            $fcba (G)
    .byte   %11110101 ; |#### # #|            $fcbb (G)
    .byte   %11000101 ; |##   # #|            $fcbc (G)
    .byte   %11000101 ; |##   # #|            $fcbd (G)
    .byte   %11111111 ; |########|            $fcbe (G)
    .byte   %11111111 ; |########|            $fcbf (G)
    .byte   %11010101 ; |## # # #|            $fcc0 (G)
    .byte   %11010101 ; |## # # #|            $fcc1 (G)
    .byte   %11010101 ; |## # # #|            $fcc2 (G)
    .byte   %11010101 ; |## # # #|            $fcc3 (G)
    .byte   %11010101 ; |## # # #|            $fcc4 (G)
    .byte   %11010101 ; |## # # #|            $fcc5 (G)
    .byte   %11010101 ; |## # # #|            $fcc6 (G)
    .byte   %11010101 ; |## # # #|            $fcc7 (G)
    .byte   %11001101 ; |##  ## #|            $fcc8 (G)
    .byte   %11001101 ; |##  ## #|            $fcc9 (G)
    .byte   %11111111 ; |########|            $fcca (G)
    .byte   %00000000 ; |        |            $fccb (G)
    .byte   %00001000 ; |    #   |            $fccc (G)
    .byte   %00001000 ; |    #   |            $fccd (G)
    .byte   %00001100 ; |    ##  |            $fcce (G)
    .byte   %00001100 ; |    ##  |            $fccf (G)
    .byte   %00001100 ; |    ##  |            $fcd0 (G)
    .byte   %00001100 ; |    ##  |            $fcd1 (G)
    .byte   %00001100 ; |    ##  |            $fcd2 (G)
    .byte   %00001100 ; |    ##  |            $fcd3 (G)
    .byte   %00001110 ; |    ### |            $fcd4 (G)
    .byte   %00001110 ; |    ### |            $fcd5 (G)
    .byte   %00000110 ; |     ## |            $fcd6 (G)
    .byte   %00000110 ; |     ## |            $fcd7 (G)
    .byte   %00000111 ; |     ###|            $fcd8 (G)
    .byte   %00000111 ; |     ###|            $fcd9 (G)
    .byte   %01100111 ; | ##  ###|            $fcda (G)
    .byte   %01100111 ; | ##  ###|            $fcdb (G)
    .byte   %01100111 ; | ##  ###|            $fcdc (G)
    .byte   %01100111 ; | ##  ###|            $fcdd (G)
    .byte   %00111111 ; |  ######|            $fcde (G)
    .byte   %00111111 ; |  ######|            $fcdf (G)
    .byte   %00011111 ; |   #####|            $fce0 (G)
    .byte   %00011111 ; |   #####|            $fce1 (G)
    .byte   %00000011 ; |      ##|            $fce2 (G)
    .byte   %00000011 ; |      ##|            $fce3 (G)
    .byte   %00000011 ; |      ##|            $fce4 (G)
    .byte   %00000011 ; |      ##|            $fce5 (G)
    .byte   %00000011 ; |      ##|            $fce6 (G)
    .byte   %00000011 ; |      ##|            $fce7 (G)
    .byte   %00000011 ; |      ##|            $fce8 (G)
    .byte   %00000011 ; |      ##|            $fce9 (G)
    .byte   %00000000 ; |        |            $fcea (G)
    .byte   %00000000 ; |        |            $fceb (G)
    
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fcec (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fcf4 (*)
    .byte   $00,$00,$00,$00                 ; $fcfc (*)
    
    .byte   %11111111 ; |########|            $fd00 (G)
    .byte   %11011101 ; |## ### #|            $fd01 (G)
    .byte   %11010001 ; |## #   #|            $fd02 (G)
    .byte   %11010101 ; |## # # #|            $fd03 (G)
    .byte   %10010001 ; |#  #   #|            $fd04 (G)
    .byte   %11111111 ; |########|            $fd05 (G)
    .byte   %10010001 ; |#  #   #|            $fd06 (G)
    .byte   %10110111 ; |# ## ###|            $fd07 (G)
    .byte   %11010101 ; |## # # #|            $fd08 (G)
    .byte   %10010001 ; |#  #   #|            $fd09 (G)
    .byte   %11111111 ; |########|            $fd0a (G)
    .byte   %00000000 ; |        |            $fd0b (G)
    .byte   %11111111 ; |########|            $fd0c (G)
    .byte   %01101111 ; | ## ####|            $fd0d (G)
    .byte   %01101111 ; | ## ####|            $fd0e (G)
    .byte   %01101111 ; | ## ####|            $fd0f (G)
    .byte   %01101111 ; | ## ####|            $fd10 (G)
    .byte   %01101111 ; | ## ####|            $fd11 (G)
    .byte   %01101111 ; | ## ####|            $fd12 (G)
    .byte   %01000111 ; | #   ###|            $fd13 (G)
    .byte   %01000111 ; | #   ###|            $fd14 (G)
    .byte   %01010111 ; | # # ###|            $fd15 (G)
    .byte   %01010111 ; | # # ###|            $fd16 (G)
    .byte   %11111111 ; |########|            $fd17 (G)
    .byte   %11111111 ; |########|            $fd18 (G)
    .byte   %00010101 ; |   # # #|            $fd19 (G)
    .byte   %00010101 ; |   # # #|            $fd1a (G)
    .byte   %11010101 ; |## # # #|            $fd1b (G)
    .byte   %11010101 ; |## # # #|            $fd1c (G)
    .byte   %00010001 ; |   #   #|            $fd1d (G)
    .byte   %00010001 ; |   #   #|            $fd1e (G)
    .byte   %01110101 ; | ### # #|            $fd1f (G)
    .byte   %01110101 ; | ### # #|            $fd20 (G)
    .byte   %00010101 ; |   # # #|            $fd21 (G)
    .byte   %00010101 ; |   # # #|            $fd22 (G)
    .byte   %11111111 ; |########|            $fd23 (G)
    .byte   %11111111 ; |########|            $fd24 (G)
    .byte   %01011111 ; | # #####|            $fd25 (G)
    .byte   %01011111 ; | # #####|            $fd26 (G)
    .byte   %01011111 ; | # #####|            $fd27 (G)
    .byte   %01011111 ; | # #####|            $fd28 (G)
    .byte   %01011111 ; | # #####|            $fd29 (G)
    .byte   %01011111 ; | # #####|            $fd2a (G)
    .byte   %01011111 ; | # #####|            $fd2b (G)
    .byte   %01011111 ; | # #####|            $fd2c (G)
    .byte   %00111111 ; |  ######|            $fd2d (G)
    .byte   %00111111 ; |  ######|            $fd2e (G)
    .byte   %11111111 ; |########|            $fd2f (G)
    .byte   %00000000 ; |        |            $fd30 (G)
    .byte   %00000000 ; |        |            $fd31 (G)
    .byte   %00000000 ; |        |            $fd32 (G)
    .byte   %00000000 ; |        |            $fd33 (G)
    .byte   %00000000 ; |        |            $fd34 (G)
    .byte   %00000000 ; |        |            $fd35 (G)
    .byte   %00000000 ; |        |            $fd36 (G)
    .byte   %10000000 ; |#       |            $fd37 (G)
    .byte   %10000000 ; |#       |            $fd38 (G)
    .byte   %11111000 ; |#####   |            $fd39 (G)
    .byte   %11111000 ; |#####   |            $fd3a (G)
    .byte   %00111000 ; |  ###   |            $fd3b (G)
    .byte   %00111000 ; |  ###   |            $fd3c (G)
    .byte   %11111000 ; |#####   |            $fd3d (G)
    .byte   %11111000 ; |#####   |            $fd3e (G)
    .byte   %11000000 ; |##      |            $fd3f (G)
    .byte   %11000000 ; |##      |            $fd40 (G)
    .byte   %11100000 ; |###     |            $fd41 (G)
    .byte   %11100000 ; |###     |            $fd42 (G)
    .byte   %11110000 ; |####    |            $fd43 (G)
    .byte   %11110000 ; |####    |            $fd44 (G)
    .byte   %11110000 ; |####    |            $fd45 (G)
    .byte   %11110000 ; |####    |            $fd46 (G)
    .byte   %10011100 ; |#  ###  |            $fd47 (G)
    .byte   %10011100 ; |#  ###  |            $fd48 (G)
    .byte   %10011110 ; |#  #### |            $fd49 (G)
    .byte   %10011110 ; |#  #### |            $fd4a (G)
    .byte   %10010010 ; |#  #  # |            $fd4b (G)
    .byte   %10010010 ; |#  #  # |            $fd4c (G)
    .byte   %11110110 ; |#### ## |            $fd4d (G)
    .byte   %11110110 ; |#### ## |            $fd4e (G)
    .byte   %11110110 ; |#### ## |            $fd4f (G)
    .byte   %11110110 ; |#### ## |            $fd50 (G)
    .byte   %11011011 ; |## ## ##|            $fd51 (G)
    .byte   %11111111 ; |########|            $fd52 (G)
    .byte   %11111111 ; |########|            $fd53 (G)
    .byte   %11111111 ; |########|            $fd54 (G)
    .byte   %11111111 ; |########|            $fd55 (G)
    .byte   %01000010 ; | #    # |            $fd56 (G)
    .byte   %01111110 ; | ###### |            $fd57 (G)
    .byte   %00011000 ; |   ##   |            $fd58 (G)
    .byte   %00011000 ; |   ##   |            $fd59 (G)
    .byte   %00111000 ; |  ###   |            $fd5a (G)
    .byte   %00100100 ; |  #  #  |            $fd5b (G)
    .byte   %00100100 ; |  #  #  |            $fd5c (G)
    .byte   %00100100 ; |  #  #  |            $fd5d (G)
    .byte   %00011100 ; |   ###  |            $fd5e (G)
    .byte   %00011000 ; |   ##   |            $fd5f (G)
    .byte   %00111000 ; |  ###   |            $fd60 (G)
    .byte   %00100100 ; |  #  #  |            $fd61 (G)
    .byte   %00100100 ; |  #  #  |            $fd62 (G)
    .byte   %00100100 ; |  #  #  |            $fd63 (G)
    .byte   %00011100 ; |   ###  |            $fd64 (G)
    .byte   %00011000 ; |   ##   |            $fd65 (G)
    .byte   %00111000 ; |  ###   |            $fd66 (G)
    .byte   %00100100 ; |  #  #  |            $fd67 (G)
    .byte   %00100100 ; |  #  #  |            $fd68 (G)
    .byte   %00100100 ; |  #  #  |            $fd69 (G)
    .byte   %00011100 ; |   ###  |            $fd6a (G)
    .byte   %00011000 ; |   ##   |            $fd6b (G)
    .byte   %00111000 ; |  ###   |            $fd6c (G)
    .byte   %00100100 ; |  #  #  |            $fd6d (G)
    .byte   %00100100 ; |  #  #  |            $fd6e (G)
    .byte   %00100100 ; |  #  #  |            $fd6f (G)
    .byte   %00011100 ; |   ###  |            $fd70 (G)
    .byte   %00011000 ; |   ##   |            $fd71 (G)
    .byte   %00111000 ; |  ###   |            $fd72 (G)
    .byte   %00100100 ; |  #  #  |            $fd73 (G)
    .byte   %00100100 ; |  #  #  |            $fd74 (G)
    .byte   %00100100 ; |  #  #  |            $fd75 (G)
    .byte   %00011100 ; |   ###  |            $fd76 (G)
    .byte   %00011000 ; |   ##   |            $fd77 (G)
    .byte   %00111000 ; |  ###   |            $fd78 (G)
    .byte   %00100100 ; |  #  #  |            $fd79 (G)
    .byte   %00100100 ; |  #  #  |            $fd7a (G)
    .byte   %00100100 ; |  #  #  |            $fd7b (G)
    .byte   %00011100 ; |   ###  |            $fd7c (G)
    .byte   %01110111 ; | ### ###|            $fd7d (G)
    .byte   %00100101 ; |  #  # #|            $fd7e (G)
    .byte   %00100101 ; |  #  # #|            $fd7f (G)
    .byte   %00100101 ; |  #  # #|            $fd80 (G)
    .byte   %00100111 ; |  #  ###|            $fd81 (G)
    .byte   %01100000 ; | ##     |            $fd82 (G)
    .byte   %00000000 ; |        |            $fd83 (G)
    .byte   %00010011 ; |   #  ##|            $fd84 (G)
    .byte   %00010010 ; |   #  # |            $fd85 (G)
    .byte   %00111010 ; |  ### # |            $fd86 (G)
    .byte   %00101010 ; |  # # # |            $fd87 (G)
    .byte   %00101011 ; |  # # ##|            $fd88 (G)
    .byte   %00101000 ; |  # #   |            $fd89 (G)
    .byte   %00000000 ; |        |            $fd8a (G)
    .byte   %00001111 ; |    ####|            $fd8b (G)
    .byte   %00010010 ; |   #  # |            $fd8c (G)
    .byte   %00010010 ; |   #  # |            $fd8d (G)
    .byte   %00010000 ; |   #    |            $fd8e (G)
    .byte   %00100001 ; |  #    #|            $fd8f (G)
    .byte   %01001100 ; | #  ##  |            $fd90 (G)
    .byte   %01011100 ; | # ###  |            $fd91 (G)
    .byte   %10111110 ; |# ##### |            $fd92 (G)
    .byte   %10111110 ; |# ##### |            $fd93 (G)
    .byte   %10111110 ; |# ##### |            $fd94 (G)
    .byte   %10011110 ; |#  #### |            $fd95 (G)
    .byte   %01000000 ; | #      |            $fd96 (G)
    .byte   %01000000 ; | #      |            $fd97 (G)
    .byte   %00100000 ; |  #     |            $fd98 (G)
    .byte   %00100000 ; |  #     |            $fd99 (G)
    .byte   %00011000 ; |   ##   |            $fd9a (G)
    .byte   %00000111 ; |     ###|            $fd9b (G)
    .byte   %01110111 ; | ### ###|            $fd9c (G)
    .byte   %00010100 ; |   # #  |            $fd9d (G)
    .byte   %01110111 ; | ### ###|            $fd9e (G)
    .byte   %01000101 ; | #   # #|            $fd9f (G)
    .byte   %01110111 ; | ### ###|            $fda0 (G)
    .byte   %00000000 ; |        |            $fda1 (G)
    .byte   %00000000 ; |        |            $fda2 (G)
    .byte   %10111000 ; |# ###   |            $fda3 (G)
    .byte   %10101000 ; |# # #   |            $fda4 (G)
    .byte   %10101000 ; |# # #   |            $fda5 (G)
    .byte   %10101000 ; |# # #   |            $fda6 (G)
    .byte   %10101000 ; |# # #   |            $fda7 (G)
    .byte   %00000000 ; |        |            $fda8 (G)
    .byte   %00000000 ; |        |            $fda9 (G)
    .byte   %11110000 ; |####    |            $fdaa (G)
    .byte   %01001000 ; | #  #   |            $fdab (G)
    .byte   %01001000 ; | #  #   |            $fdac (G)
    .byte   %00001000 ; |    #   |            $fdad (G)
    .byte   %10000100 ; |#    #  |            $fdae (G)
    .byte   %00110010 ; |  ##  # |            $fdaf (G)
    .byte   %00111010 ; |  ### # |            $fdb0 (G)
    .byte   %01111101 ; | ##### #|            $fdb1 (G)
    .byte   %01111101 ; | ##### #|            $fdb2 (G)
    .byte   %01111101 ; | ##### #|            $fdb3 (G)
    .byte   %01111001 ; | ####  #|            $fdb4 (G)
    .byte   %00000010 ; |      # |            $fdb5 (G)
    .byte   %00000010 ; |      # |            $fdb6 (G)
    .byte   %00000100 ; |     #  |            $fdb7 (G)
    .byte   %00000100 ; |     #  |            $fdb8 (G)
    .byte   %00011000 ; |   ##   |            $fdb9 (G)
    .byte   %11100000 ; |###     |            $fdba (G)
    
    .byte   $00,$3c,$7e,$c6,$c6,$e6,$5c,$38 ; $fdbb (*)
    .byte   $76,$e3,$c0,$c0,$e0,$70,$38,$1c ; $fdc3 (*)
    .byte   $ee,$be,$de,$be,$7c,$7e,$f2,$e7 ; $fdcb (*)
    .byte   $c5,$c0,$e8,$f4,$7c,$3e,$bb,$e9 ; $fdd3 (*)
    .byte   $7c,$1e,$06,$00,$00,$00,$00,$00 ; $fddb (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fde3 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fdeb (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fdf3 (*)
    .byte   $00,$00,$00,$00,$00,$38,$54,$38 ; $fdfb (*)
    .byte   $6c,$92,$ba,$fe,$7c,$00,$00,$00 ; $fe03 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fe0b (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fe13 (*)
    .byte   $00,$00,$00,$00,$00,$1c,$2a,$1c ; $fe1b (*)
    .byte   $36,$49,$5d,$7f,$3e,$00,$00,$00 ; $fe23 (*)
    .byte   $c6,$4b,$29,$10,$00,$1c,$3e,$6f ; $fe2b (*)
    .byte   $5f,$5f,$2e,$26,$1c,$08,$08,$1e ; $fe33 (*)
    .byte   $31,$3f,$35,$35,$1e,$24,$12,$3f ; $fe3b (*)
    .byte   $35,$3f,$1e,$c3,$c3,$66,$66,$3c ; $fe43 (*)
    .byte   $bd,$bd,$7f,$7e,$4c,$7c,$7c,$ae ; $fe4b (*)
    .byte   $7c,$7c,$54,$ff,$ff,$ff,$ff,$ff ; $fe53 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $fe5b (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $fe63 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $fe6b (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $fe73 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $fe7b (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $fe83 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $fe8b (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $fe93 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $fe9b (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $fea3 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $feab (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $feb3 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $febb (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $fec3 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $fecb (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $fed3 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $fedb (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $fee3 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $feeb (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $fef3 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $fefb (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $ff03 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $ff0b (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $ff13 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $ff1b (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $ff23 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $ff2b (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $ff33 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $ff3b (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $ff43 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $ff4b (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $ff53 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $ff5b (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $ff63 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $ff6b (*)
    .byte   $ff                             ; $ff73 (*)
    
    .byte   %00000000 ; |        |            $ff74 (G)
    .byte   %00111000 ; |  ###   |            $ff75 (G)
    .byte   %00101000 ; |  # #   |            $ff76 (G)
    .byte   %00101000 ; |  # #   |            $ff77 (G)
    .byte   %00101000 ; |  # #   |            $ff78 (G)
    .byte   %00111000 ; |  ###   |            $ff79 (G)
    .byte   %00000000 ; |        |            $ff7a (G)
    .byte   %00000000 ; |        |            $ff7b (G)
    .byte   %00000000 ; |        |            $ff7c (G)
    .byte   %00010000 ; |   #    |            $ff7d (G)
    .byte   %00010000 ; |   #    |            $ff7e (G)
    .byte   %00010000 ; |   #    |            $ff7f (G)
    .byte   %00010000 ; |   #    |            $ff80 (G)
    .byte   %00010000 ; |   #    |            $ff81 (G)
    .byte   %00000000 ; |        |            $ff82 (G)
    .byte   %00000000 ; |        |            $ff83 (G)
    .byte   %00000000 ; |        |            $ff84 (G)
    .byte   %00111000 ; |  ###   |            $ff85 (G)
    .byte   %00100000 ; |  #     |            $ff86 (G)
    .byte   %00111000 ; |  ###   |            $ff87 (G)
    .byte   %00001000 ; |    #   |            $ff88 (G)
    .byte   %00111000 ; |  ###   |            $ff89 (G)
    .byte   %00000000 ; |        |            $ff8a (G)
    .byte   %00000000 ; |        |            $ff8b (G)
    .byte   %00000000 ; |        |            $ff8c (G)
    .byte   %00111000 ; |  ###   |            $ff8d (G)
    .byte   %00001000 ; |    #   |            $ff8e (G)
    .byte   %00111000 ; |  ###   |            $ff8f (G)
    .byte   %00001000 ; |    #   |            $ff90 (G)
    .byte   %00111000 ; |  ###   |            $ff91 (G)
    .byte   %00000000 ; |        |            $ff92 (G)
    .byte   %00000000 ; |        |            $ff93 (G)
    .byte   %00000000 ; |        |            $ff94 (G)
    .byte   %00001000 ; |    #   |            $ff95 (G)
    .byte   %00001000 ; |    #   |            $ff96 (G)
    .byte   %00111000 ; |  ###   |            $ff97 (G)
    .byte   %00101000 ; |  # #   |            $ff98 (G)
    .byte   %00101000 ; |  # #   |            $ff99 (G)
    .byte   %00000000 ; |        |            $ff9a (G)
    .byte   %00000000 ; |        |            $ff9b (G)
    .byte   %00000000 ; |        |            $ff9c (G)
    .byte   %00111000 ; |  ###   |            $ff9d (G)
    .byte   %00001000 ; |    #   |            $ff9e (G)
    .byte   %00111000 ; |  ###   |            $ff9f (G)
    .byte   %00100000 ; |  #     |            $ffa0 (G)
    .byte   %00111000 ; |  ###   |            $ffa1 (G)
    .byte   %00000000 ; |        |            $ffa2 (G)
    .byte   %00000000 ; |        |            $ffa3 (G)
    .byte   %00000000 ; |        |            $ffa4 (G)
    .byte   %00111000 ; |  ###   |            $ffa5 (G)
    .byte   %00101000 ; |  # #   |            $ffa6 (G)
    .byte   %00111000 ; |  ###   |            $ffa7 (G)
    .byte   %00100000 ; |  #     |            $ffa8 (G)
    .byte   %00111000 ; |  ###   |            $ffa9 (G)
    .byte   %00000000 ; |        |            $ffaa (G)
    .byte   %00000000 ; |        |            $ffab (G)
    .byte   %00000000 ; |        |            $ffac (G)
    .byte   %00001000 ; |    #   |            $ffad (G)
    .byte   %00001000 ; |    #   |            $ffae (G)
    .byte   %00001000 ; |    #   |            $ffaf (G)
    .byte   %00001000 ; |    #   |            $ffb0 (G)
    .byte   %00111000 ; |  ###   |            $ffb1 (G)
    .byte   %00000000 ; |        |            $ffb2 (G)
    .byte   %00000000 ; |        |            $ffb3 (G)
    .byte   %00000000 ; |        |            $ffb4 (G)
    .byte   %00111000 ; |  ###   |            $ffb5 (G)
    .byte   %00101000 ; |  # #   |            $ffb6 (G)
    .byte   %00111000 ; |  ###   |            $ffb7 (G)
    .byte   %00101000 ; |  # #   |            $ffb8 (G)
    .byte   %00111000 ; |  ###   |            $ffb9 (G)
    .byte   %00000000 ; |        |            $ffba (G)
    .byte   %00000000 ; |        |            $ffbb (G)
    .byte   %00000000 ; |        |            $ffbc (G)
    .byte   %00001000 ; |    #   |            $ffbd (G)
    .byte   %00001000 ; |    #   |            $ffbe (G)
    .byte   %00111000 ; |  ###   |            $ffbf (G)
    .byte   %00101000 ; |  # #   |            $ffc0 (G)
    .byte   %00111000 ; |  ###   |            $ffc1 (G)
    .byte   %00000000 ; |        |            $ffc2 (G)
    .byte   %00000000 ; |        |            $ffc3 (G)
    
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $ffc4 (*)

  IF PLUSROM = 1
    ORG     $7fd0, $ff
    RORG    $ffd0
  ELSE
    ORG     $7fd4, $ff
    RORG    $ffd4
  ENDIF

    ldx #$ff
    txs
    lda #$f2
    pha
    lda #$4f
    pha
    
Lffdd
    pha                             ;3        
    txa                             ;2        
    pha                             ;3        
    tsx                             ;2        
    lda     CXM0FB,x                ;4        
    rol                             ;2        
    rol                             ;2        
    rol                             ;2        
    rol                             ;2        
    and     #$07                    ;2        
    tax                             ;2        
    inx                             ;2   =  28
Lffeb
    lda     $1ff3,x                 ;4        
    pla                             ;4        
    tax                             ;2        
    pla                             ;4        
    rts                             ;6   =  20

    ORG     $7ff3, $ff
    RORG    $fff3

Lfff3
    .byte   $ff                             ; $fff3 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff         ; $fff4 (D)
  IF PLUSROM = 1
    .word (PlusROM_API - $6000)             ; PlusRom API pointer
  ELSE
    .byte   $ff,$ff                         ; $fffa (*)
  ENDIF
    .word Init,Init
