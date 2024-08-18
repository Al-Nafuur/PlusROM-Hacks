; Disassembly of Public ROMs/Classic Roms/NTSC/BY ALPHABET/S-Z\/Tac-Scan.bin
; Disassembled 08/16/24 03:38:51
; Using Stella 6.6
;
; ROM properties name : Tac-Scan (1983) (SEGA)
; ROM properties MD5  : d45ebf130ed9070ea8ebd56176e48a38
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

PLUSROM           = 1
PAL               = 0

; There are no different color values 
; used in the PAL version
; so it looks like PAL60 == NTSC !

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

;CXP0FB         = $02  ; (Ri)
;CXBLPF         = $06  ; (Ri)
;INPT0          = $08  ; (Ri)
;INPT2          = $0a  ; (Ri)
INPT3           = $0b  ; (R)
;$1e            = $0e  ; (Ri)

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
HMM0            = $22  ; (W)
HMBL            = $24  ; (W)
VDELP0          = $25  ; (W)
VDELP1          = $26  ; (W)
HMOVE           = $2a  ; (W)

SWCHA           = $0280
SWCHB           = $0282
INTIM           = $0284
TIM64T          = $0296
$298            = $0298
$299            = $0299
$29a            = $029a
TIM8I           = $029d
TIM64I          = $029e
T1024I          = $029f


;-----------------------------------------------------------
;      RIOT RAM (zero-page) labels
;-----------------------------------------------------------

ram_80          = $80
ram_81          = $81
ram_82          = $82
;                 $83  (i)
;                 $84  (i)
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
;                 $90  (i)
;                 $91  (i)
ram_92          = $92
ram_93          = $93
ram_94          = $94
ram_95          = $95
ram_96          = $96
ram_97          = $97
ram_98          = $98
ram_99          = $99
;                 $9a  (i)
;                 $9b  (i)
ram_9C          = $9c
ram_9D          = $9d
ram_9E          = $9e
ram_9F          = $9f
;                 $a0  (i)
;                 $a1  (i)
ram_A2          = $a2
ram_A3          = $a3
ram_A4          = $a4
ram_A5          = $a5
ram_A6          = $a6
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

ram_CE          = $ce
ram_CF          = $cf
;                 $d0  (i)
ram_D1          = $d1
;                 $d2  (i)
ram_D3          = $d3
;                 $d4  (i)
ram_D5          = $d5
;                 $d6  (i)
ram_D7          = $d7
;                 $d8  (i)
ram_D9          = $d9
;                 $da  (i)
ram_DB          = $db
ram_DC          = $dc
ram_DD          = $dd
ram_DE          = $de
ram_DF          = $df
ram_E0          = $e0
ram_E1          = $e1
ram_E2          = $e2
ram_E3          = $e3
;                 $e4  (i)
;                 $e5  (i)
;                 $e6  (i)
;                 $e7  (i)
;                 $e8  (i)
ram_E9          = $e9
ram_EA          = $ea
;                 $eb  (i)
;                 $ec  (i)
;                 $ed  (i)
;                 $ee  (i)
;                 $ef  (i)
ram_F0          = $f0
ram_F1          = $f1
ram_F2          = $f2
ram_F3          = $f3
;                 $f4  (i)
ram_F5          = $f5
;                 $f6  (i)
ram_F7          = $f7
;                 $f8  (i)
ram_F9          = $f9
;                 $fa  (i)
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

HIGHSCORE_ID            = 81         ; Tac-Scan game ID in Highscore DB
  ENDIF


;***********************************************************
;      Bank 0
;***********************************************************

    SEG     CODE
    ORG     $f000

Start
    sei                             ;2        
    cld                             ;2        
    ldx     #$28                    ;2        
    lda     #$00                    ;2   =   8
Lf006
    sta     NUSIZ0,x                ;4        
    dex                             ;2        
    bpl     Lf006                   ;2/3      
    txs                             ;2   =  10
Lf00c
    sta     VSYNC,x                 ;4        
    dex                             ;2        
    bmi     Lf00c                   ;2/3      
    dec     ram_F7                  ;5   =  13
Lf013
    ldx     #$06                    ;2        
    stx     ram_E1                  ;3   =   5
Lf017
    lda     Lffdd,x                 ;4        
    sta     ram_E2,x                ;4        
    lda     Lffe4,x                 ;4        
    sta     ram_E9,x                ;4        
    dex                             ;2        
    bpl     Lf017                   ;2/3      
    lda     #$7f                    ;2        
    sta     ram_B1                  ;3        
    sta     CTRLPF                  ;3        
    ldy     #$ff                    ;2        
    sty     ram_DD                  ;3        
    sty     ram_BE                  ;3        
    sty     ram_88                  ;3        
    lda     #$00                    ;2        
    sta     ram_A9                  ;3        
    sta     ram_AF                  ;3        
    sta     ram_AC                  ;3        
    sta     ram_CE                  ;3        
    sta     ram_B6                  ;3        
    ldx     #$0b                    ;2   =  58
Lf040
    sty     ram_CF,x                ;4        
    dex                             ;2        
    sta     ram_CF,x                ;4        
    dex                             ;2        
    bpl     Lf040                   ;2/3      
    ldx     #$03                    ;2        
    stx     ram_B8                  ;3        
    inx                             ;2   =  21
Lf04d
    adc     #$19                    ;2        
    sta     ram_98,x                ;4        
    sty     ram_8A,x                ;4        
    sty     ram_9E,x                ;4        
    sty     ram_C0,x                ;4        
    dex                             ;2        
    bpl     Lf04d                   ;2/3      
    lda     #$01                    ;2        
    sta     ram_B7                  ;3        
    sta     ram_A4                  ;3        
    lda     #$46                    ;2        
    sta     ram_B9                  ;3        
    lda     #$3e                    ;2        
    sta     ram_BA                  ;3   =  40
Lf068
    jmp     Lf85e                   ;3   =   3
    
Lf06b
    lda     ram_F7                  ;3        
    bmi     Lf072                   ;2/3      
    jmp     Lfacc                   ;3   =   8
    
Lf072
    lda     INTIM                   ;4        
    bne     Lf072                   ;2/3      
    lda     #$82                    ;2        
    sta     WSYNC                   ;3   =  11
;---------------------------------------
    sta     VBLANK                  ;3        
    sta     WSYNC                   ;3   =   6
;---------------------------------------
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     VSYNC                   ;3        
    sta     WSYNC                   ;3   =   6
;---------------------------------------
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     #$00                    ;2        
    sta     WSYNC                   ;3   =   5
;---------------------------------------
    sta     VSYNC                   ;3        
  IF PAL = 1
    lda     #$45                    ;2        
  ELSE
    lda     #$25                    ;2        
  ENDIF
    sta     TIM64T                  ;4        
    lda     #$c4                    ;2        
    sta     ram_B0                  ;3        
    lda     #$20                    ;2        
    sta     HMBL                    ;3        
    ldy     #$5a                    ;2        
    sty     ram_F3                  ;3        
    sty     ram_D9                  ;3        
    sta     WSYNC                   ;3   =  30
;---------------------------------------
    sta     RESBL                   ;3        
    sta     HMOVE                   ;3        
    lda     ram_80                  ;3        
    adc     ram_A7                  ;3        
    clc                             ;2        
    sta     COLUBK                  ;3        
    lda     #$00                    ;2        
    sta     HMBL                    ;3        
    lda     ram_B9                  ;3        
    bpl     Lf0bf                   ;2/3      
    lda     ram_BA                  ;3        
    bpl     Lf0bf                   ;2/3      
    lda     #$ff                    ;2        
    sta     ram_F7                  ;3   =  37
Lf0bf
    lda     ram_F7                  ;3        
    bpl     Lf0c9                   ;2/3      
    jmp     Lfbd0                   ;3   =   8
    
Lf0c6
    jmp     Lf1e0                   ;3   =   3
    
Lf0c9
    lda     ram_AC                  ;3        
    sta     ram_F9                  ;3        
    inc     ram_F9                  ;5        
    and     #$03                    ;2        
    tax                             ;2        
    lda     Lfe15,x                 ;4        
    sta     ram_80                  ;3        
    lda     Lfe19,x                 ;4        
    sta     ram_87                  ;3        
    lda     Lfefc,x                 ;4        
    sta     ram_97                  ;3        
    lda     ram_A4                  ;3        
    cmp     #$01                    ;2        
    bne     Lf0c6                   ;2/3      
    lda     ram_88                  ;3        
    bpl     Lf0c6                   ;2/3      
    lda     ram_AD                  ;3        
    bpl     Lf0c6                   ;2/3      
    cmp     #$ff                    ;2        
    bne     Lf144                   ;2/3!     
    ldx     #$04                    ;2   =  59
Lf0f5
    lda     ram_C0,x                ;4        
    beq     Lf0ff                   ;2/3      
    dex                             ;2        
    bpl     Lf0f5                   ;2/3      
    jmp     Lf1b4                   ;3   =  13 *
    
Lf0ff
    lda     ram_81                  ;3        
    bne     Lf113                   ;2/3      
    ldy     #$5a                    ;2        
    sty     ram_81                  ;3        
    ldx     #$08                    ;2   =  12
Lf109
    lda     ram_CF,x                ;4        
    sty     ram_CF,x                ;4        
    sta     ram_F2,x                ;4        
    dex                             ;2        
    dex                             ;2        
    bpl     Lf109                   ;2/3 =  18
Lf113
    lda     #$00                    ;2        
    sta     ram_D9                  ;3        
    sta     ram_D7                  ;3        
    ldy     ram_B6                  ;3        
    cpy     #$0a                    ;2        
    bcc     Lf128                   ;2/3      
    tya                             ;2         *
    sbc     #$0a                    ;2         *
    tay                             ;2         *
    lda     #$09                    ;2         *
    jsr     Lf55c                   ;6   =  29 *
Lf128
    lda     Lfff2,y                 ;4        
    jsr     Lf558                   ;6        
    lda     ram_D9                  ;3        
    sta     ram_96                  ;3        
    lda     #$5a                    ;2        
    sta     ram_D9                  ;3        
    dec     ram_AD                  ;5        
    lda     #$ff                    ;2        
    sta     ram_BE                  ;3        
    sta     ram_9F                  ;3        
    sta     ram_9E                  ;3        
    lda     #$4e                    ;2        
    sta     ram_99                  ;3   =  42
Lf144
    lda     #$01                    ;2        
    sta     ram_A8                  ;3        
    sta     ram_F9                  ;3        
    lda     ram_9F                  ;3        
    cmp     #$c5                    ;2        
    bcs     Lf160                   ;2/3      
    and     #$03                    ;2        
    bne     Lf160                   ;2/3      
    lda     ram_F0                  ;3        
    bmi     Lf15e                   ;2/3      
    beq     Lf160                   ;2/3      
    dec     ram_99                  ;5        
    bne     Lf160                   ;2/3 =  33
Lf15e
    inc     ram_99                  ;5   =   5
Lf160
    lda     ram_9F                  ;3        
    cmp     #$23                    ;2        
    bne     Lf183                   ;2/3      
    ldx     #$00                    ;2        
    lda     ram_99                  ;3   =  12
Lf16a
    cmp     Lffd3,x                 ;4        
    bcc     Lf1cb                   ;2/3      
    cmp     Lffd8,x                 ;4        
    bcs     Lf17c                   ;2/3      
    ldy     ram_C0,x                ;4        
    bne     Lf1cb                   ;2/3      
    sta     ram_C0,x                ;4        
    beq     Lf1a4                   ;2/3 =  24
Lf17c
    cpx     #$01                    ;2         *
    beq     Lf1cb                   ;2/3       *
    inx                             ;2         *
    bpl     Lf16a                   ;2/3 =   8 *
Lf183
    cmp     #$14                    ;2        
    bne     Lf1cb                   ;2/3      
    lda     ram_99                  ;3         *
    ldx     #$02                    ;2   =   9 *
Lf18b
    cmp     Lffd3,x                 ;4         *
    bcc     Lf1b4                   ;2/3       *
    cmp     Lffd8,x                 ;4         *
    bcs     Lf19d                   ;2/3       *
    ldy     ram_C0,x                ;4         *
    bne     Lf1b4                   ;2/3       *
    sta     ram_C0,x                ;4         *
    beq     Lf1a4                   ;2/3 =  24 *
Lf19d
    cpx     #$04                    ;2         *
    beq     Lf1b4                   ;2/3       *
    inx                             ;2         *
    bpl     Lf18b                   ;2/3 =   8 *
Lf1a4
    lda     #$00                    ;2        
    sta     ram_95                  ;3        
    dec     ram_B6                  ;5        
    lda     ram_B6                  ;3        
    beq     Lf1b4                   ;2/3      
    lda     #$ff                    ;2         *
    sta     ram_AD                  ;3         *
    bne     Lf1db                   ;2/3 =  22 *
Lf1b4
    lda     ram_81                  ;3        
    beq     Lf1c6                   ;2/3      
    ldx     #$08                    ;2   =   7
Lf1ba
    lda     ram_F2,x                ;4        
    sta     ram_CF,x                ;4        
    stx     ram_96                  ;3        
    stx     ram_81                  ;3        
    dex                             ;2        
    dex                             ;2        
    bpl     Lf1ba                   ;2/3 =  20
Lf1c6
    jsr     Lfbb4                   ;6        
    bpl     Lf1e0                   ;2/3 =   8
Lf1cb
    lda     ram_9F                  ;3        
    cmp     #$c5                    ;2        
    bne     Lf1db                   ;2/3      
    dec     ram_95                  ;5        
    ldx     ram_E1                  ;3        
    lda     ram_E9,x                ;4        
    cmp     #$a8                    ;2        
    bcs     Lf248                   ;2/3!=  23
Lf1db
    dec     ram_9F                  ;5        
    jmp     Lf248                   ;3   =   8
    
Lf1e0
    lda     ram_DF                  ;3        
    beq     Lf248                   ;2/3!     
    dec     ram_DF                  ;5        
    lda     #$1e                    ;2        
    sta     ram_B3                  ;3        
    lda     #$00                    ;2        
    sta     ram_A8                  ;3        
    lda     ram_B9                  ;3        
    ldy     ram_B2                  ;3        
    bmi     Lf1fd                   ;2/3      
    clc                             ;2        
    adc     #$04                    ;2        
    cpy     #$00                    ;2        
    beq     Lf1fd                   ;2/3      
    adc     #$04                    ;2   =  38
Lf1fd
    sta     ram_C5                  ;3        
    ldy     ram_C0                  ;3        
    beq     Lf208                   ;2/3      
    sta     ram_C7                  ;3        
    clc                             ;2        
    adc     #$10                    ;2   =  15
Lf208
    ldy     ram_C1                  ;3        
    beq     Lf20e                   ;2/3      
    sta     ram_C8                  ;3   =   8
Lf20e
    lda     ram_BA                  ;3        
    ldy     ram_B2                  ;3        
    bmi     Lf21d                   ;2/3      
    clc                             ;2        
    adc     #$04                    ;2        
    cpy     #$00                    ;2        
    beq     Lf21d                   ;2/3      
    adc     #$04                    ;2   =  18
Lf21d
    sta     ram_C6                  ;3        
    ldy     ram_C2                  ;3        
    beq     Lf227                   ;2/3      
    sta     ram_C9                  ;3        
    adc     #$10                    ;2   =  13
Lf227
    ldx     ram_C3                  ;3        
    beq     Lf22f                   ;2/3      
    sta     ram_CA                  ;3        
    ldy     #$01                    ;2   =  10
Lf22f
    cpy     #$00                    ;2        
    beq     Lf236                   ;2/3      
    clc                             ;2        
    adc     #$10                    ;2   =   8
Lf236
    ldx     ram_C4                  ;3        
    beq     Lf23c                   ;2/3      
    sta     ram_CB                  ;3   =   8
Lf23c
    lda     #$1b                    ;2        
    sta     ram_BE                  ;3        
    lda     ram_B7                  ;3        
    sta     ram_B4                  ;3        
    lda     ram_B8                  ;3        
    sta     ram_B5                  ;3   =  17
Lf248
    ldx     ram_88                  ;3   =   3
Lf24a
    bmi     Lf26e                   ;2/3      
    ldy     ram_F0                  ;3        
    beq     Lf26e                   ;2/3      
    bmi     Lf25f                   ;2/3      
    lda     ram_8E,x                ;4        
    bne     Lf25a                   ;2/3      
    lda     #$a1                    ;2        
    sta     ram_8E,x                ;4   =  21
Lf25a
    dec     ram_8E,x                ;6        
    jmp     Lf26b                   ;3   =   9
    
Lf25f
    lda     ram_8E,x                ;4        
    cmp     #$a0                    ;2        
    bne     Lf269                   ;2/3      
    lda     #$ff                    ;2        
    sta     ram_8E,x                ;4   =  14
Lf269
    inc     ram_8E,x                ;6   =   6
Lf26b
    dex                             ;2        
    bpl     Lf24a                   ;2/3 =   4
Lf26e
    jsr     Lfb5c                   ;6        
    lda     ram_AE                  ;3        
    bmi     Lf292                   ;2/3      
    ldy     #$74                    ;2        
    ldx     #$05                    ;2        
    cmp     #$06                    ;2        
    bcc     Lf281                   ;2/3      
    ldy     #$67                    ;2        
    ldx     #$07                    ;2   =  23
Lf281
    lda     ram_F7                  ;3        
    cmp     #$02                    ;2        
    bcc     Lf28e                   ;2/3      
    sty     ram_F3                  ;3        
    bcs     Lf290                   ;2/3 =  12
Lf28b
    jmp     Lf581                   ;3   =   3
    
Lf28e
    sty     ram_D9                  ;3   =   3
Lf290
    stx     ram_98                  ;3   =   3
Lf292
    dec     ram_9D                  ;5        
    lda     ram_9D                  ;3        
    cmp     #$fb                    ;2        
    bne     Lf29e                   ;2/3      
    lda     #$03                    ;2        
    sta     ram_9D                  ;3   =  17
Lf29e
    ldx     ram_A4                  ;3        
    dex                             ;2        
    stx     ram_A3                  ;3        
    ldx     ram_E1                  ;3        
    stx     ram_E0                  ;3        
    ldx     #$05                    ;2   =  16
Lf2a9
    dex                             ;2        
    beq     Lf2b9                   ;2/3      
    lda     ram_81,x                ;4        
    beq     Lf2a9                   ;2/3      
    cmp     #$01                    ;2        
    bne     Lf2a9                   ;2/3      
    dec     ram_81,x                ;6        
    jsr     Lf784                   ;6   =  26
Lf2b9
    lda     ram_BE                  ;3        
    cmp     #$ff                    ;2        
    bne     Lf2c5                   ;2/3      
    lda     ram_88                  ;3        
    bmi     Lf28b                   ;2/3      
    bpl     Lf341                   ;2/3!=  14
Lf2c5
    lda     ram_BE                  ;3        
    adc     #$06                    ;2        
    sta     ram_BE                  ;3        
    lda     ram_B2                  ;3        
    and     #$01                    ;2        
    beq     Lf2d5                   ;2/3      
    inc     ram_BE                  ;5        
    inc     ram_BE                  ;5   =  25
Lf2d5
    lda     ram_BE                  ;3        
    cmp     #$c5                    ;2        
    bcc     Lf2e1                   ;2/3      
    lda     #$ff                    ;2        
    sta     ram_BE                  ;3        
    sta     ram_B3                  ;3   =  15
Lf2e1
    lda     ram_BE                  ;3        
    sec                             ;2        
    sbc     #$0e                    ;2        
    sta     ram_BF                  ;3        
    ldx     ram_A4                  ;3   =  13
Lf2ea
    dex                             ;2        
    beq     Lf341                   ;2/3!     
    stx     ram_F1                  ;3        
    lda     ram_81,x                ;4        
    bne     Lf2ea                   ;2/3      
    lda     ram_9E,x                ;4        
    cmp     ram_BE                  ;3        
    bcc     Lf319                   ;2/3!     
    sbc     #$16                    ;2        
    cmp     ram_BE                  ;3        
    bcs     Lf319                   ;2/3!     
    lda     ram_98,x                ;4        
    ldx     #$01                    ;2   =  35
Lf303
    ldy     ram_C0,x                ;4        
    beq     Lf316                   ;2/3      
    cmp     ram_C7,x                ;4        
    beq     Lf352                   ;2/3      
    bcs     Lf319                   ;2/3      
    adc     #$0b                    ;2        
    cmp     ram_C7,x                ;4        
    bcs     Lf352                   ;2/3      
    sec                             ;2        
    sbc     #$0b                    ;2   =  26
Lf316
    dex                             ;2        
    beq     Lf303                   ;2/3 =   4
Lf319
    ldx     ram_F1                  ;3        
    lda     ram_9E,x                ;4        
    cmp     ram_BF                  ;3        
    bcc     Lf2ea                   ;2/3!     
    sbc     #$16                    ;2        
    cmp     ram_BF                  ;3        
    bcs     Lf2ea                   ;2/3!     
    lda     ram_98,x                ;4        
    ldx     #$02                    ;2   =  25
Lf32b
    ldy     ram_C2,x                ;4        
    beq     Lf33e                   ;2/3      
    cmp     ram_C9,x                ;4        
    beq     Lf352                   ;2/3      
    bcs     Lf341                   ;2/3      
    adc     #$0b                    ;2        
    cmp     ram_C9,x                ;4        
    bcs     Lf352                   ;2/3      
    sec                             ;2        
    sbc     #$0b                    ;2   =  26
Lf33e
    dex                             ;2        
    bpl     Lf32b                   ;2/3 =   4
Lf341
    lda     ram_A8                  ;3        
    bne     Lf3b9                   ;2/3      
    lda     ram_C5                  ;3        
    ldx     #$02                    ;2        
    jsr     Lfe00                   ;6        
    sta     WSYNC                   ;3   =  19
;---------------------------------------
    sta     HMOVE                   ;3        
    bmi     Lf3bc                   ;2/3 =   5
Lf352
    ldx     ram_F1                  ;3        
    lda     ram_9E,x                ;4        
    cmp     ram_BD                  ;3        
    bne     Lf36b                   ;2/3      
    lda     #$2d                    ;2        
    jsr     Lf568                   ;6        
    lda     #$0e                    ;2        
    sta     ram_F5                  ;3        
    sta     ram_AB                  ;3        
    sta     ram_AF                  ;3        
    lda     #$00                    ;2        
    sta     ram_BD                  ;3   =  36
Lf36b
    inc     ram_86                  ;5        
    ldx     ram_AC                  ;3        
    lda     Lfff3,x                 ;4        
    jsr     Lf568                   ;6        
    lda     ram_AD                  ;3        
    bne     Lf39f                   ;2/3      
    lda     ram_B6                  ;3        
    cmp     #$13                    ;2        
    beq     Lf381                   ;2/3      
    inc     ram_B6                  ;5   =  35
Lf381
    ldx     ram_AC                  ;3        
    cpx     #$03                    ;2        
    bcc     Lf38f                   ;2/3      
    lda     #$12                    ;2         *
    jsr     Lf560                   ;6         *
    jmp     Lf395                   ;3   =  18 *
    
Lf38f
    lda     Lfdf8,x                 ;4        
    jsr     Lf564                   ;6   =  10
Lf395
    lda     #$2d                    ;2        
    sta     ram_F5                  ;3        
    inc     ram_CE                  ;5        
    lda     #$00                    ;2        
    sta     ram_BD                  ;3   =  15
Lf39f
    lda     ram_AD                  ;3        
    bmi     Lf3a5                   ;2/3      
    dec     ram_AD                  ;5   =  10
Lf3a5
    ldx     ram_F1                  ;3        
    lda     #$0f                    ;2        
    sta     ram_81,x                ;4        
    lda     #$0e                    ;2        
    sta     ram_A7                  ;3        
    lda     #$0f                    ;2        
    sta     ram_A5                  ;3        
    lda     #$ff                    ;2        
    sta     ram_B3                  ;3        
    dec     ram_A3                  ;5   =  29
Lf3b9
    jmp     Lf581                   ;3   =   3
    
Lf3bc
    ldy     #$00                    ;2   =   2
Lf3be
    ldx     ram_88                  ;3        
    bmi     Lf3ff                   ;2/3 =   5
Lf3c2
    lda.wy  ram_9E,y                ;4        
    cmp     ram_8A,x                ;4        
    bcc     Lf3d4                   ;2/3      
    sbc     #$13                    ;2        
    cmp     ram_8A,x                ;4        
    bcs     Lf3d4                   ;2/3      
    dey                             ;2        
    sty     ram_A3                  ;3        
    bpl     Lf3de                   ;2/3 =  25
Lf3d4
    dex                             ;2        
    bpl     Lf3c2                   ;2/3      
    cpy     ram_A3                  ;3        
    bcs     Lf3de                   ;2/3      
    iny                             ;2        
    bpl     Lf3be                   ;2/3 =  13
Lf3de
    ldy     #$00                    ;2   =   2
Lf3e0
    ldx     ram_88                  ;3   =   3
Lf3e2
    lda.wy  ram_E9,y                ;4        
    cmp     ram_8A,x                ;4        
    bcc     Lf3f5                   ;2/3      
    sbc     #$05                    ;2        
    cmp     ram_8A,x                ;4        
    bcs     Lf3f5                   ;2/3      
    dey                             ;2        
    sty     ram_E0                  ;3        
    jmp     Lf3ff                   ;3   =  26
    
Lf3f5
    dex                             ;2        
    bpl     Lf3e2                   ;2/3      
    cpy     ram_E0                  ;3        
    bcs     Lf3ff                   ;2/3      
    iny                             ;2        
    bpl     Lf3e0                   ;2/3 =  13
Lf3ff
    lda     INTIM                   ;4        
    bne     Lf3ff                   ;2/3!     
    sta     WSYNC                   ;3   =   9
;---------------------------------------
    sta     VBLANK                  ;3        
    sta     WSYNC                   ;3   =   6
;---------------------------------------
    sta     HMM0                    ;3        
    lda     #$02                    ;2        
    sta     ENABL                   ;3        
    sta     WSYNC                   ;3   =  11
;---------------------------------------
Lf412
    lda     ram_B0                  ;3        
    ldx     ram_E0                  ;3        
    bmi     Lf421                   ;2/3      
    cmp     ram_E9,x                ;4        
    bne     Lf421                   ;2/3      
    jsr     Lf51e                   ;6        
    bpl     Lf412                   ;2/3 =  22
Lf421
    ldx     #$00                    ;2        
    cmp     ram_8A                  ;3        
    beq     Lf45b                   ;2/3      
    cmp     ram_8B                  ;3        
    beq     Lf45a                   ;2/3      
    cmp     ram_8C                  ;3        
    beq     Lf459                   ;2/3      
    cmp     ram_8D                  ;3        
    beq     Lf458                   ;2/3      
    cmp     ram_BE                  ;3        
    beq     Lf481                   ;2/3      
    ldx     ram_A3                  ;3        
    cmp     ram_9E,x                ;4        
    bne     Lf447                   ;2/3      
    sta     WSYNC                   ;3   =  39
;---------------------------------------
    sec                             ;2        
    sbc     #$11                    ;2        
    sta     ram_B0                  ;3        
    jsr     Lfd38                   ;6   =  13
Lf447
    dec     ram_B0                  ;5        
    sta     WSYNC                   ;3   =   8
;---------------------------------------
    bne     Lf412                   ;2/3      
    lda     ram_BC                  ;3        
    sta     ram_BB                  ;3        
    lda     #$01                    ;2        
    sta     ram_A8                  ;3        
    jmp     Lf69f                   ;3   =  16
    
Lf458
    inx                             ;2   =   2 *
Lf459
    inx                             ;2   =   2
Lf45a
    inx                             ;2   =   2
Lf45b
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sec                             ;2        
    sbc     #$0b                    ;2        
    sta     ram_B0                  ;3        
    lda     #$0f                    ;2        
    sta     COLUP0                  ;3        
    lda     ram_8E,x                ;4        
    ldx     #$00                    ;2        
    jsr     Lfe00                   ;6        
    sta     WSYNC                   ;3   =  27
;---------------------------------------
    sta     HMOVE                   ;3        
    stx     NUSIZ0                  ;3        
    ldx     #$06                    ;2   =   8
Lf475
    lda     Lfecb,x                 ;4        
    sta     WSYNC                   ;3   =   7
;---------------------------------------
    sta     GRP0                    ;3        
    dex                             ;2        
    bmi     Lf447                   ;2/3      
    bpl     Lf475                   ;2/3 =   9
Lf481
    lda     #$0f                    ;2        
    sta     COLUP0                  ;3        
    lda     ram_B4                  ;3        
    sta     WSYNC                   ;3   =  11
;---------------------------------------
    ldx     ram_B2                  ;3        
    beq     Lf490                   ;2/3      
    clc                             ;2        
    adc     #$10                    ;2   =   9
Lf490
    sta     NUSIZ0                  ;3        
    lda     #$00                    ;2        
    ldx     ram_B2                  ;3        
    beq     Lf49f                   ;2/3      
    bpl     Lf49c                   ;2/3      
    lda     #$e0                    ;2   =  14
Lf49c
    clc                             ;2        
    adc     #$10                    ;2   =   4
Lf49f
    sta     HMM0                    ;3        
    lda     #$02                    ;2        
    ldx     ram_B4                  ;3        
    bpl     Lf4a9                   ;2/3      
    lda     #$00                    ;2   =  12
Lf4a9
    sta     ENAM0                   ;3        
    lda     ram_B0                  ;3        
    sec                             ;2        
    sbc     #$19                    ;2        
    sta     ram_B0                  ;3        
    ldy     #$00                    ;2        
    ldx     #$09                    ;2   =  17
Lf4b6
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    cpx     #$05                    ;2        
    bne     Lf4c4                   ;2/3      
    lda     ram_B2                  ;3        
    beq     Lf4c4                   ;2/3      
    sty     ENAM0                   ;3   =  15
Lf4c4
    dex                             ;2        
    bpl     Lf4b6                   ;2/3      
    sty     ENAM0                   ;3        
    lda     ram_B5                  ;3        
    ldx     ram_B2                  ;3        
    beq     Lf4d2                   ;2/3      
    clc                             ;2        
    adc     #$10                    ;2   =  19
Lf4d2
    sta     NUSIZ0                  ;3        
    lda     ram_C6                  ;3        
    ldx     #$02                    ;2        
    jsr     Lfe00                   ;6        
    sta     WSYNC                   ;3   =  17
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     #$02                    ;2        
    ldx     ram_B5                  ;3        
    bpl     Lf4e7                   ;2/3      
    lda     #$00                    ;2   =  12 *
Lf4e7
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     ENAM0                   ;3        
    lda     #$00                    ;2        
    ldx     ram_B2                  ;3        
    beq     Lf4f8                   ;2/3      
    bpl     Lf4f5                   ;2/3      
    lda     #$e0                    ;2   =  14
Lf4f5
    clc                             ;2        
    adc     #$10                    ;2   =   4
Lf4f8
    sta     HMM0                    ;3        
    sta     WSYNC                   ;3   =   6
;---------------------------------------
    sta     HMOVE                   ;3        
    ldx     #$03                    ;2   =   5
Lf500
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    dex                             ;2        
    bpl     Lf500                   ;2/3      
    inx                             ;2        
    lda     ram_B2                  ;3        
    beq     Lf50e                   ;2/3      
    stx     ENAM0                   ;3   =  17
Lf50e
    ldy     #$03                    ;2   =   2
Lf510
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    dey                             ;2        
    bpl     Lf510                   ;2/3      
    stx     ENAM0                   ;3        
    sta     WSYNC                   ;3   =  13
;---------------------------------------
    jmp     Lf412                   ;3   =   3
    
Lf51e
    lda     ram_E2,x                ;4        
    ldx     #$01                    ;2        
    jsr     Lfe00                   ;6        
    lda     INPT3|$30               ;3        
    bmi     Lf52d                   ;2/3      
    inc     ram_BB                  ;5        
    inc     ram_BB                  ;5   =  27
Lf52d
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    jsr     Lffeb                   ;6        
    dec     ram_E0                  ;5        
    lda     ram_B0                  ;3        
    sec                             ;2        
    sbc     #$05                    ;2        
    sta     ram_B0                  ;3        
    lda     #$0f                    ;2        
    sta     COLUP1                  ;3        
    lda     #$06                    ;2        
    sta     NUSIZ1                  ;3        
    lda     #$10                    ;2        
    sta     WSYNC                   ;3   =  39
;---------------------------------------
    sta     GRP1                    ;3        
    jsr     Lffeb                   ;6        
    jsr     Lffeb                   ;6        
    lda     #$00                    ;2        
    sta     WSYNC                   ;3   =  20
;---------------------------------------
    sta     GRP1                    ;3        
    rts                             ;6   =   9
    
Lf558
    ldx     #$0a                    ;2        
    bne     Lf56a                   ;2/3 =   4
Lf55c
    ldx     #$08                    ;2         *
    bne     Lf56a                   ;2/3 =   4 *
Lf560
    ldx     #$02                    ;2         *
    bne     Lf56a                   ;2/3 =   4 *
Lf564
    ldx     #$04                    ;2        
    bne     Lf56a                   ;2/3 =   4
Lf568
    ldx     #$06                    ;2   =   2
Lf56a
    stx     ram_F7                  ;3   =   3
Lf56c
    clc                             ;2        
    adc     ram_CF,x                ;4        
    sec                             ;2        
    sbc     #$5a                    ;2        
    bcc     Lf57c                   ;2/3      
    sta     ram_CF,x                ;4        
    lda     #$09                    ;2        
    dex                             ;2        
    dex                             ;2        
    bpl     Lf56c                   ;2/3 =  24
Lf57c
    adc     #$5a                    ;2        
    sta     ram_CF,x                ;4        
    rts                             ;6   =  12
    
Lf581
    lda     #$01                    ;2        
    sta     ram_A8                  ;3        
    lda     ram_A4                  ;3        
    cmp     #$01                    ;2        
    bne     Lf5a4                   ;2/3      
    lda     ram_AD                  ;3        
    cmp     #$f0                    ;2        
    bcc     Lf5a4                   ;2/3      
    cmp     #$ff                    ;2        
    beq     Lf5a4                   ;2/3      
    lda     ram_9F                  ;3        
    cmp     #$c5                    ;2        
    bcs     Lf5a4                   ;2/3      
    and     #$01                    ;2        
    bne     Lf5a2                   ;2/3      
    jmp     Lf70e                   ;3   =  37
    
Lf5a2
    inc     ram_AD                  ;5   =   5
Lf5a4
    lda     ram_B7                  ;3        
    sta     NUSIZ0                  ;3        
    lda     ram_B9                  ;3        
    ldx     #$00                    ;2        
    sta     WSYNC                   ;3   =  14
;---------------------------------------
    jsr     Lfe00                   ;6        
    sta     WSYNC                   ;3   =   9
;---------------------------------------
    sta     HMOVE                   ;3   =   3
Lf5b5
    lda     INTIM                   ;4        
    bne     Lf5b5                   ;2/3      
    sta     ram_BB                  ;3        
    sta     WSYNC                   ;3   =  12
;---------------------------------------
    sta     VBLANK                  ;3        
    sta     WSYNC                   ;3   =   6
;---------------------------------------
    sta     HMP0                    ;3        
    lda     #$02                    ;2        
    sta     ENABL                   ;3        
    lda     ram_F7                  ;3        
    bpl     Lf5cf                   ;2/3      
    jmp     Lfbf9                   ;3   =  16
    
Lf5cf
    sta     WSYNC                   ;3   =   3
;---------------------------------------
Lf5d1
    lda     ram_B0                  ;3        
    ldx     ram_A3                  ;3        
    beq     Lf5ed                   ;2/3      
    cmp     ram_9E,x                ;4        
    bne     Lf5ed                   ;2/3      
    sec                             ;2        
    sbc     #$11                    ;2        
    sta     ram_B0                  ;3        
    sta     WSYNC                   ;3   =  24
;---------------------------------------
    lda     INPT3|$30               ;3        
    bmi     Lf5e8                   ;2/3      
    inc     ram_BB                  ;5   =  10
Lf5e8
    jsr     Lfd38                   ;6        
    bmi     Lf5fc                   ;2/3 =   8
Lf5ed
    cmp     #$29                    ;2        
    beq     Lf61f                   ;2/3!     
    ldx     ram_E0                  ;3        
    bmi     Lf5fc                   ;2/3      
    cmp     ram_E9,x                ;4        
    bne     Lf5fc                   ;2/3      
    jsr     Lf51e                   ;6   =  21
Lf5fc
    dec     ram_B0                  ;5        
    lda     INPT3|$30               ;3        
    bmi     Lf604                   ;2/3      
    inc     ram_BB                  ;5   =  15
Lf604
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    jmp     Lf5d1                   ;3   =   3
    
Lf609
    jsr     Lfed2                   ;6        
    bmi     Lf641                   ;2/3 =   8
Lf60e
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    stx     PF2                     ;3        
    stx     COLUPF                  ;3        
    sta     WSYNC                   ;3   =   9
;---------------------------------------
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    jsr     Lfed2                   ;6        
    bmi     Lf66b                   ;2/3 =   8
Lf61f
    lda     ram_AE                  ;3        
    bmi     Lf629                   ;2/3      
    lda     #$28                    ;2        
    eor     ram_AE                  ;3        
    sta     COLUP1                  ;3   =  13
Lf629
    ldx     ram_98                  ;3        
    stx     NUSIZ1                  ;3        
    ldx     #$01                    ;2        
    lda     ram_A6                  ;3        
    sta     WSYNC                   ;3   =  14
;---------------------------------------
    jsr     Lfe00                   ;6        
    sta     WSYNC                   ;3   =   9
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     ram_B9                  ;3        
    bmi     Lf609                   ;2/3      
    jsr     Lfd00                   ;6   =  14
Lf641
    lda     ram_AD                  ;3        
    bpl     Lf64b                   ;2/3      
    lda     #$60                    ;2        
    sta     PF2                     ;3        
    dec     COLUPF                  ;5   =  15
Lf64b
    lda     ram_B8                  ;3        
    sta     NUSIZ0                  ;3        
    lda     ram_F3                  ;3        
    sta     ram_D9                  ;3        
    ldx     #$00                    ;2        
    stx     HMP1                    ;3        
    lda     ram_BA                  ;3        
    bmi     Lf60e                   ;2/3      
    sta     WSYNC                   ;3   =  25
;---------------------------------------
    stx     PF2                     ;3        
    stx     COLUPF                  ;3        
    jsr     Lfe00                   ;6        
    sta     WSYNC                   ;3   =  15
;---------------------------------------
    sta     HMOVE                   ;3        
    jsr     Lfd00                   ;6   =   9
Lf66b
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     ram_AD                  ;3        
    bpl     Lf67b                   ;2/3      
    lda     #$98                    ;2        
    sta     PF2                     ;3        
    cmp     (ram_8A,x)              ;6        
    cmp     (ram_8A,x)              ;6        
    dec     COLUPF                  ;5   =  27
Lf67b
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     #$00                    ;2        
    sta     PF2                     ;3        
    sta     COLUPF                  ;3        
    sta     WSYNC                   ;3   =  11
;---------------------------------------
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     INPT3|$30               ;3        
    bmi     Lf68f                   ;2/3      
    lda     #$f0                    ;2        
    sta     ram_BB                  ;3   =  10
Lf68f
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     ram_88                  ;3        
    bpl     Lf69b                   ;2/3      
    lda     ram_BE                  ;3        
    cmp     #$ff                    ;2        
    beq     Lf69f                   ;2/3 =  12
Lf69b
    lda     #$00                    ;2        
    sta     ram_A8                  ;3   =   5
Lf69f
    lda     ram_87                  ;3        
    sta     COLUBK                  ;3        
    ldx     #$00                    ;2        
    stx     GRP0                    ;3        
    stx     GRP1                    ;3        
    stx     REFP0                   ;3        
    stx     REFP1                   ;3        
    lda     #$3a                    ;2        
    jsr     Lfe00                   ;6        
    lda     #$0f                    ;2        
    sta     COLUP0                  ;3        
    sta     COLUP1                  ;3        
    lda     ram_96                  ;3        
    sta     ram_D9                  ;3        
    ldx     #$01                    ;2        
    stx     VDELP0                  ;3        
    stx     VDELP1                  ;3        
    lda     #$42                    ;2        
    jsr     Lfe00                   ;6        
    sta     WSYNC                   ;3   =  61
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     #$03                    ;2        
    sta     NUSIZ0                  ;3        
    sta     NUSIZ1                  ;3        
    ldy     #$08                    ;2   =  13
Lf6d3
    sty     ram_94                  ;3        
    lda     (ram_D9),y              ;5        
    sta     ram_F1                  ;3        
    sta     WSYNC                   ;3   =  14
;---------------------------------------
    lda     (ram_CF),y              ;5        
    sta     GRP0                    ;3        
    lda     (ram_D1),y              ;5        
    sta     GRP1                    ;3        
    lda     (ram_D3),y              ;5        
    sta     GRP0                    ;3        
    lda     (ram_D5),y              ;5        
    nop                             ;2        
    tax                             ;2        
    lda     (ram_D7),y              ;5        
    ldy     ram_F1                  ;3        
    stx     GRP1                    ;3        
    sta     GRP0                    ;3        
    sty     GRP1                    ;3        
    sta     GRP0                    ;3        
    ldy     ram_94                  ;3        
    dey                             ;2        
    bpl     Lf6d3                   ;2/3      
    lda     #$00                    ;2        
    sta     VDELP0                  ;3        
    sta     VDELP1                  ;3        
    sta     GRP0                    ;3        
    sta     GRP1                    ;3        
  IF PAL = 1
    lda     #$38                    ;2        
  ELSE
    lda     #$18                    ;2        
  ENDIF
    sta     TIM64T                  ;4        
    jmp     Lf068                   ;3   =  83
    
Lf70e
    dec     ram_AD                  ;5        
    lda     ram_BB                  ;3        
  IF PAL = 1
    nop                             ;2
    nop                             ;2        
  ELSE
    sta     ram_BC                  ;3   =  11
  ENDIF

Lf714
    lda     INTIM                   ;4        
    bne     Lf714                   ;2/3      
    sta     WSYNC                   ;3   =   9
;---------------------------------------
    sta     VBLANK                  ;3        
    sta     WSYNC                   ;3   =   6
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     WSYNC                   ;3   =   6
;---------------------------------------
    sta     NUSIZ0                  ;3        
    ldx     #$08                    ;2        
    stx     COLUP0                  ;3        
    lda     #$02                    ;2        
    sta     ENABL                   ;3   =  13
Lf72d
    lda     ram_B0                  ;3        
    cmp     ram_9F                  ;3        
    bne     Lf768                   ;2/3      
    lda     ram_99                  ;3        
    ldx     #$00                    ;2        
    jsr     Lfe00                   ;6        
    sta     WSYNC                   ;3   =  22
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     ram_B0                  ;3        
    sec                             ;2        
    sbc     #$10                    ;2        
    sta     ram_B0                  ;3        
    ldy     #$0c                    ;2   =  15
Lf747
    cpy     #$04                    ;2        
    bne     Lf755                   ;2/3      
    lda     ram_AC                  ;3        
    and     #$03                    ;2        
    tax                             ;2        
    lda     Lfcfb,x                 ;4        
    sta     COLUP0                  ;3   =  18
Lf755
    lda     Lff81,y                 ;4        
    ldx     ram_9D                  ;3        
    bmi     Lf75f                   ;2/3      
    lda     Lff8e,y                 ;4   =  13
Lf75f
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     GRP0                    ;3        
    dey                             ;2        
    bpl     Lf747                   ;2/3      
    bmi     Lf72d                   ;2/3 =   9
Lf768
    ldx     ram_E0                  ;3        
    bmi     Lf773                   ;2/3      
    cmp     ram_E9,x                ;4        
    bne     Lf773                   ;2/3      
    jsr     Lf51e                   ;6   =  17
Lf773
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    dec     ram_B0                  ;5        
    bne     Lf72d                   ;2/3      
    lda     ram_BC                  ;3        
    sta     ram_BB                  ;3        
    lda     #$00                    ;2        
    sta     ram_DF                  ;3        
    jmp     Lf69f                   ;3   =  21
    
Lf784
    cpx     #$04                    ;2        
    beq     Lf797                   ;2/3      
    lda     ram_99,x                ;4        
    sta     ram_98,x                ;4        
    lda     ram_9F,x                ;4        
    sta     ram_9E,x                ;4        
    lda     ram_82,x                ;4        
    sta     ram_81,x                ;4        
    inx                             ;2        
    bpl     Lf784                   ;2/3 =  32
Lf797
    lda     ram_99                  ;3        
    clc                             ;2        
    adc     #$3f                    ;2        
    cmp     #$9a                    ;2        
    bcc     Lf7a2                   ;2/3      
    and     #$7f                    ;2   =  13
Lf7a2
    cmp     #$08                    ;2        
    bcs     Lf7a8                   ;2/3      
    adc     #$08                    ;2   =   6 *
Lf7a8
    sta     ram_9C                  ;3        
    lda     ram_CE                  ;3        
    and     #$03                    ;2        
    tax                             ;2        
    lda     Lfef8,x                 ;4        
    sta     ram_A2                  ;3        
    dec     ram_A4                  ;5        
    lda     #$00                    ;2        
    sta     ram_85                  ;3        
    rts                             ;6   =  33
    
Lf7bb
    lda     ram_F7                  ;3        
    bmi     Lf7c9                   ;2/3      
    lda     SWCHB                   ;4        
    and     #$01                    ;2        
    bne     Lf7c9                   ;2/3      
    jmp     Start                   ;3   =  16 *
    
Lf7c9
    ldx     #$08                    ;2        
  IF PAL = 1
    jmp     Lfdab-$E000
    nop
  ELSE
    lda     ram_BB                  ;3        
    sta     ram_BC                  ;3        
  ENDIF
Lf7cf
    ldy     #$00                    ;2        
    cmp     #$05                    ;2        
    bcs     Lf7db                   ;2/3      
    iny                             ;2        
    iny                             ;2   =  18
Lf7d7
    lda     #$b5                    ;2        
    bne     Lf80d                   ;2/3!=   4
Lf7db
    cmp     #$09                    ;2        
    bcc     Lf809                   ;2/3!     
    cmp     #$32                    ;2        
    bcs     Lf7e8                   ;2/3      
    iny                             ;2   =  10
Lf7e4
    lda     #$9b                    ;2        
    bne     Lf80d                   ;2/3!=   4
Lf7e8
    cmp     #$37                    ;2        
    bcc     Lf809                   ;2/3!     
    ldx     #$00                    ;2        
    cmp     #$73                    ;2        
    bcs     Lf7f6                   ;2/3      
    lda     #$81                    ;2        
    bne     Lf80d                   ;2/3!=  14
Lf7f6
    cmp     #$77                    ;2        
    bcc     Lf809                   ;2/3!     
    cmp     #$e0                    ;2        
    bcs     Lf801                   ;2/3!     
    dey                             ;2        
    bmi     Lf7e4                   ;2/3!=  12
Lf801
    cmp     #$aa                    ;2        
    bcc     Lf809                   ;2/3      
    dey                             ;2        
    dey                             ;2        
    bmi     Lf7d7                   ;2/3!=  10
Lf809
    lda     ram_DE                  ;3         *
    ldy     ram_F0                  ;3   =   6 *
Lf80d
    sta     ram_DC                  ;3        
    sta     ram_DE                  ;3        
    sty     ram_F0                  ;3        
    lda     ram_9D                  ;3        
    bmi     Lf81e                   ;2/3      
    lda     ram_DC                  ;3        
    clc                             ;2        
    adc     #$0d                    ;2        
    sta     ram_DC                  ;3   =  24
Lf81e
    stx     REFP0                   ;3        
  IF PAL = 1
    lda     REFP1                   ;3
    nop
  ELSE
    lda     SWCHA                   ;4        
  ENDIF
    eor     ram_CC                  ;3        
    beq     Lf830                   ;2/3      
  IF PAL = 1
    lda     REFP1                   ;3
    nop
    sta     ram_CC                  ;3        
    and     #$80                    ;2        
  ELSE
    lda     SWCHA                   ;4        
    sta     ram_CC                  ;3        
    and     #$04                    ;2        
  ENDIF
    beq     Lf838                   ;2/3 =  23
Lf830
    lda     ram_BE                  ;3        
    cmp     #$ff                    ;2        
    beq     Lf83e                   ;2/3      
    bne     Lf85b                   ;2/3 =   9
Lf838
    lda     ram_F0                  ;3        
    sta     ram_B2                  ;3        
    inc     ram_DF                  ;5   =  11
Lf83e
    ldy     ram_86                  ;3        
    cpy     #$03                    ;2        
    bcc     Lf857                   ;2/3      
    beq     Lf84a                   ;2/3       *
    lda     #$12                    ;2         *
    bpl     Lf84c                   ;2/3 =  13 *
Lf84a
    lda     #$09                    ;2   =   2 *
Lf84c
    jsr     Lf560                   ;6         *
    lda     #$0e                    ;2         *
    sta     ram_F5                  ;3         *
    sta     ram_AB                  ;3         *
    sta     ram_AF                  ;3   =  17 *
Lf857
    lda     #$00                    ;2        
    sta     ram_86                  ;3   =   5
Lf85b
    jmp     Lf06b                   ;3   =   3
    
Lf85e
    lda     ram_A7                  ;3        
    beq     Lf86e                   ;2/3      
    dec     ram_A7                  ;5        
    lda     ram_A7                  ;3        
    cmp     #$80                    ;2        
    bne     Lf86e                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_A7                  ;3   =  22
Lf86e
    ldy     #$00                    ;2        
    ldx     #$00                    ;2        
    lda     ram_B1                  ;3        
    bmi     Lf898                   ;2/3      
    dec     ram_B1                  ;5        
    and     #$07                    ;2        
    bne     Lf87e                   ;2/3      
    inc     ram_AF                  ;5   =  23
Lf87e
    ldy     ram_A9                  ;3        
    and     #$01                    ;2        
    beq     Lf88e                   ;2/3      
    lda     #$1f                    ;2        
    cpy     #$1f                    ;2        
    bne     Lf88c                   ;2/3      
    lda     #$18                    ;2   =  15
Lf88c
    sta     ram_A9                  ;3   =   3
Lf88e
    ldx     ram_AF                  ;3        
    lda     #$ff                    ;2        
    sta     ram_9F                  ;3        
    lda     #$04                    ;2        
    bne     Lf8ba                   ;2/3 =  12
Lf898
    lda     ram_B3                  ;3        
    bmi     Lf8ae                   ;2/3      
    cmp     #$1e                    ;2        
    bne     Lf8a6                   ;2/3      
    sty     ram_A9                  ;3        
    lda     #$08                    ;2        
    sta     AUDC0                   ;3   =  17
Lf8a6
    ldx     #$08                    ;2        
    ldy     ram_A9                  ;3        
    dec     ram_A9                  ;5        
    dec     ram_B3                  ;5   =  15
Lf8ae
    lda     ram_A5                  ;3        
    bmi     Lf8bc                   ;2/3      
    dec     ram_A5                  ;5        
    ldy     #$1f                    ;2        
    lda     #$08                    ;2        
    ldx     #$0f                    ;2   =  16
Lf8ba
    sta     AUDC0                   ;3   =   3
Lf8bc
    sty     AUDF0                   ;3        
    stx     AUDV0                   ;3        
    ldy     #$00                    ;2        
    ldx     #$00                    ;2        
    lda     ram_89                  ;3        
    bmi     Lf8d4                   ;2/3      
    ldx     ram_89                  ;3        
    dec     ram_89                  ;5        
    ldy     ram_AA                  ;3        
    inc     ram_AA                  ;5        
    lda     #$0a                    ;2        
    sta     AUDC1                   ;3   =  36
Lf8d4
    lda     ram_AE                  ;3        
    bmi     Lf8e2                   ;2/3      
    dec     ram_AE                  ;5        
    ldx     #$06                    ;2        
    lda     #$07                    ;2        
    sta     AUDC1                   ;3        
    ldy     #$0f                    ;2   =  19
Lf8e2
    lda     ram_F5                  ;3        
    beq     Lf90c                   ;2/3!     
    cmp     #$1e                    ;2        
    bcc     Lf8f4                   ;2/3      
    bne     Lf90a                   ;2/3!     
    sta     ram_AB                  ;3        
    ldx     #$0f                    ;2        
    stx     ram_AF                  ;3        
    bne     Lf8fe                   ;2/3 =  21
Lf8f4
    cmp     #$0f                    ;2        
    bcs     Lf8fe                   ;2/3      
    dec     ram_AB                  ;5        
    dec     ram_AB                  ;5        
    dec     ram_AF                  ;5   =  19
Lf8fe
    inc     ram_AB                  ;5        
    inc     ram_AF                  ;5        
    ldy     ram_AB                  ;3        
    lda     #$0c                    ;2        
    sta     AUDC1                   ;3        
    ldx     ram_AF                  ;3   =  21
Lf90a
    dec     ram_F5                  ;5   =   5
Lf90c
    lda     ram_95                  ;3        
    beq     Lf918                   ;2/3      
    ldx     #$03                    ;2        
    ldy     #$05                    ;2        
    lda     #$08                    ;2        
    sta     AUDC1                   ;3   =  14
Lf918
    sty     AUDF1                   ;3        
    stx     AUDV1                   ;3        
    jmp     Lf7bb                   ;3   =   9
    
Lf91f
    lda     ram_AD                  ;3        
    bpl     Lf933                   ;2/3      
    ldx     ram_A4                  ;3        
    cpx     #$01                    ;2        
    bne     Lf933                   ;2/3      
    lda     ram_88                  ;3        
    bmi     Lf930                   ;2/3      
    jmp     Lfa05                   ;3   =  20 *
    
Lf930
    jmp     Lfa89                   ;3   =   3
    
Lf933
    lda     ram_BD                  ;3        
    beq     Lf93c                   ;2/3      
    sec                             ;2        
    sbc     ram_F9                  ;3        
    sta     ram_BD                  ;3   =  13
Lf93c
    lda     ram_9F                  ;3        
    sec                             ;2        
    sbc     ram_F9                  ;3        
    cmp     #$40                    ;2        
    bcs     Lf94a                   ;2/3      
    ldx     #$01                    ;2        
    jsr     Lf784                   ;6   =  20
Lf94a
    ldx     ram_A4                  ;3   =   3
Lf94c
    dex                             ;2        
    beq     Lf977                   ;2/3      
    lda     ram_9E,x                ;4        
    sec                             ;2        
    sbc     ram_F9                  ;3        
    sta     ram_9E,x                ;4        
    lda     ram_F0                  ;3        
    beq     Lf94c                   ;2/3      
    bpl     Lf96b                   ;2/3      
    inc     ram_98,x                ;6        
    lda     ram_98,x                ;4        
    cmp     #$a0                    ;2        
    bcc     Lf94c                   ;2/3      
    lda     #$00                    ;2   =  40
Lf966
    sta     ram_98,x                ;4        
    jmp     Lf94c                   ;3   =   7
    
Lf96b
    dec     ram_98,x                ;6        
    lda     ram_98,x                ;4        
    cmp     #$a0                    ;2        
    bcc     Lf94c                   ;2/3      
    lda     #$a0                    ;2        
    bne     Lf966                   ;2/3 =  18
Lf977
    lda     ram_AD                  ;3        
    bmi     Lf9b2                   ;2/3      
    ldx     ram_A4                  ;3        
    cpx     #$05                    ;2        
    beq     Lf9b2                   ;2/3      
    lda     ram_9E,x                ;4        
    cmp     #$c5                    ;2        
    bne     Lf9b0                   ;2/3      
    ldx     ram_E1                  ;3        
    lda     ram_E9,x                ;4        
    cmp     #$a8                    ;2        
    bcs     Lf9b2                   ;2/3      
    ldx     ram_A4                  ;3        
    dex                             ;2        
    beq     Lf99a                   ;2/3      
    lda     ram_9E,x                ;4        
    cmp     #$a8                    ;2        
    bcs     Lf9b2                   ;2/3 =  46
Lf99a
    ldx     ram_A4                  ;3        
    lda     #$00                    ;2        
    sta     ram_81,x                ;4        
    inc     ram_A4                  ;5        
    lda     ram_AD                  ;3        
    cmp     #$04                    ;2        
    bne     Lf9b0                   ;2/3      
    lda     ram_BD                  ;3        
    bne     Lf9b0                   ;2/3      
    lda     #$c4                    ;2        
    sta     ram_BD                  ;3   =  31
Lf9b0
    dec     ram_9E,x                ;6   =   6
Lf9b2
    lda     ram_AD                  ;3        
    bmi     Lfa05                   ;2/3!     
    ldy     ram_88                  ;3        
    cpy     #$03                    ;2        
    beq     Lfa05                   ;2/3!     
    ldy     ram_A4                  ;3   =  15
Lf9be
    dey                             ;2        
    beq     Lfa05                   ;2/3!     
    lda.wy  ram_81,y                ;4        
    bne     Lf9be                   ;2/3      
    lda.wy  ram_9E,y                ;4        
    ldx     ram_F9                  ;3   =  17
Lf9cb
    dex                             ;2        
    bmi     Lf9be                   ;2/3      
    cmp     Lffcf,x                 ;4        
    bne     Lf9cb                   ;2/3      
    ldx     ram_88                  ;3        
    sec                             ;2        
    sbc     #$0a                    ;2        
    sta     ram_92                  ;3        
    lda.wy  ram_98,y                ;4        
    sta     ram_8F,x                ;4        
    ldy     ram_88                  ;3   =  31
Lf9e1
    bmi     Lf9f7                   ;2/3      
    lda.wy  ram_8A,y                ;4        
    clc                             ;2        
    adc     #$0c                    ;2        
    cmp     ram_92                  ;3        
    bcc     Lf9f4                   ;2/3      
    sec                             ;2         *
    sbc     #$19                    ;2         *
    cmp     ram_92                  ;3         *
    bcc     Lfa05                   ;2/3!=  24 *
Lf9f4
    dey                             ;2        
    bpl     Lf9e1                   ;2/3 =   4
Lf9f7
    lda     ram_92                  ;3        
    sta     ram_8B,x                ;4        
    lda     #$0c                    ;2        
    sta     ram_89                  ;3        
    lda     #$00                    ;2        
    sta     ram_AA                  ;3        
    inc     ram_88                  ;5   =  22
Lfa05
    ldx     ram_88                  ;3        
    bpl     Lfa0b                   ;2/3      
    bmi     Lfa89                   ;2/3 =   7
Lfa0b
    lda     ram_8A,x                ;4        
    cmp     #$c4                    ;2        
    bcs     Lfa1f                   ;2/3      
    sec                             ;2        
    sbc     ram_F9                  ;3        
    sbc     #$01                    ;2        
    sta     ram_8A,x                ;4        
    cmp     #$0d                    ;2        
    bcs     Lfa1f                   ;2/3      
    jsr     Lfede                   ;6   =  29
Lfa1f
    dex                             ;2        
    bpl     Lfa0b                   ;2/3      
    lda     ram_AE                  ;3        
    bpl     Lfa89                   ;2/3      
    ldy     #$00                    ;2        
    ldx     #$04                    ;2   =  13
Lfa2a
    dex                             ;2        
    bmi     Lfa89                   ;2/3      
    lda     ram_8A,x                ;4        
    cmp     #$28                    ;2        
    bcs     Lfa2a                   ;2/3      
    cmp     #$1f                    ;2        
    bcc     Lfa4b                   ;2/3      
    lda     ram_8E,x                ;4        
    cmp     #$41                    ;2        
    bcc     Lfa89                   ;2/3      
    cmp     #$4c                    ;2        
    bcc     Lfa6d                   ;2/3      
    cmp     #$51                    ;2        
    bcc     Lfa89                   ;2/3      
    cmp     #$5c                    ;2        
    bcc     Lfa6c                   ;2/3      
    bcs     Lfa89                   ;2/3 =  38
Lfa4b
    cmp     #$16                    ;2        
    bcs     Lfa2a                   ;2/3      
    lda     ram_8E,x                ;4        
    cmp     #$39                    ;2        
    bcc     Lfa89                   ;2/3      
    cmp     #$44                    ;2        
    bcc     Lfa6b                   ;2/3      
    cmp     #$49                    ;2        
    bcc     Lfa89                   ;2/3      
    cmp     #$55                    ;2        
    bcc     Lfa6a                   ;2/3      
    cmp     #$59                    ;2        
    bcc     Lfa89                   ;2/3      
    cmp     #$65                    ;2        
    bcs     Lfa89                   ;2/3      
    iny                             ;2   =  34
Lfa6a
    iny                             ;2   =   2
Lfa6b
    iny                             ;2   =   2
Lfa6c
    iny                             ;2   =   2
Lfa6d
    lda.wy  ram_C0,y                ;4        
    beq     Lfa89                   ;2/3      
    lda     Lfdfb,y                 ;4        
    sta     ram_A6                  ;3        
    lda     #$00                    ;2        
    sta.wy  ram_C0,y                ;5        
    sty     ram_F7                  ;3        
    jsr     Lfede                   ;6        
    lda     #$0c                    ;2        
    sta     ram_AE                  ;3        

  IF PLUSROM = 1
    jmp SendPlusROMScore
  ELSE
    lda     #$0f                    ;2        
    sta     ram_A7                  ;3   =  39
  ENDIF

Lfa89
    ldx     #$ff                    ;2        
    ldy     #$ff                    ;2        
    lda     ram_C1                  ;3        
    beq     Lfa94                   ;2/3      
    ldx     #$56                    ;2        
    iny                             ;2   =  13
Lfa94
    lda     ram_C0                  ;3        
    beq     Lfa9b                   ;2/3      
    ldx     #$46                    ;2        
    iny                             ;2   =   9
Lfa9b
    stx     ram_B9                  ;3        
    sty     ram_B7                  ;3        
    ldy     #$ff                    ;2        
    ldx     #$ff                    ;2        
    lda     ram_C4                  ;3        
    beq     Lfaaa                   ;2/3      
    iny                             ;2        
    ldx     #$5e                    ;2   =  19
Lfaaa
    lda     ram_C3                  ;3        
    beq     Lfab1                   ;2/3      
    iny                             ;2        
    ldx     #$4e                    ;2   =   9
Lfab1
    lda     ram_C2                  ;3        
    beq     Lfab8                   ;2/3      
    iny                             ;2        
    ldx     #$3e                    ;2   =   9
Lfab8
    stx     ram_BA                  ;3        
    cpy     #$02                    ;2        
    beq     Lfac6                   ;2/3      
    cpy     #$01                    ;2        
    bne     Lfac7                   ;2/3      
    lda     ram_C3                  ;3        
    bne     Lfac7                   ;2/3 =  16
Lfac6
    iny                             ;2   =   2
Lfac7
    sty     ram_B8                  ;3        
    jmp     Lf072                   ;3   =   6
    
Lfacc
    lda     ram_E1                  ;3        
    cmp     #$06                    ;2        
    beq     Lfb08                   ;2/3!     
    ldx     ram_A4                  ;3        
    cpx     #$05                    ;2        
    bne     Lfadb                   ;2/3      
    dex                             ;2        
    bpl     Lfaf4                   ;2/3 =  18
Lfadb
    lda     ram_9E,x                ;4        
    cmp     #$c5                    ;2        
    beq     Lfb08                   ;2/3!     
    cpx     #$01                    ;2        
    bne     Lfaf1                   ;2/3      
    ldy     ram_AD                  ;3        
    bpl     Lfafa                   ;2/3      
    lda     ram_9F                  ;3        
    cmp     #$c5                    ;2        
    bcs     Lfafa                   ;2/3      
    bcc     Lfaf4                   ;2/3 =  26
Lfaf1
    dex                             ;2        
    beq     Lfafa                   ;2/3 =   4
Lfaf4
    lda     ram_9E,x                ;4        
    cmp     #$bc                    ;2        
    bcs     Lfb08                   ;2/3!=   8
Lfafa
    ldx     ram_E1                  ;3        
    lda     ram_E9,x                ;4        
    cmp     #$b1                    ;2        
    bcs     Lfb08                   ;2/3      
    inc     ram_E1                  ;5        
    lda     #$c4                    ;2        
    sta     ram_EA,x                ;4   =  22
Lfb08
    lda     ram_E9                  ;3        
    sec                             ;2        
    sbc     ram_F9                  ;3        
    cmp     #$30                    ;2        
    bcs     Lfb2c                   ;2/3      
    ldx     #$00                    ;2        
    ldy     ram_E2                  ;3   =  17
Lfb15
    lda     ram_EA,x                ;4        
    sec                             ;2        
    sbc     ram_F9                  ;3        
    sta     ram_E9,x                ;4        
    lda     ram_E3,x                ;4        
    sta     ram_E2,x                ;4        
    inx                             ;2        
    cpx     ram_E1                  ;3        
    bne     Lfb15                   ;2/3      
    dec     ram_E1                  ;5        
    sty     ram_E2,x                ;4        
    jmp     Lfb38                   ;3   =  40
    
Lfb2c
    ldx     ram_E1                  ;3   =   3
Lfb2e
    lda     ram_E9,x                ;4        
    sec                             ;2        
    sbc     ram_F9                  ;3        
    sta     ram_E9,x                ;4        
    dex                             ;2        
    bpl     Lfb2e                   ;2/3 =  17
Lfb38
    ldx     #$06                    ;2   =   2
Lfb3a
    lda     ram_F0                  ;3        
    beq     Lfb59                   ;2/3      
    bmi     Lfb4a                   ;2/3      
    dec     ram_E2,x                ;6        
    bne     Lfb56                   ;2/3      
    lda     #$a0                    ;2        
    sta     ram_E2,x                ;4        
    bne     Lfb56                   ;2/3 =  23
Lfb4a
    inc     ram_E2,x                ;6        
    lda     ram_E2,x                ;4        
    cmp     #$a1                    ;2        
    bne     Lfb56                   ;2/3      
    lda     #$01                    ;2        
    sta     ram_E2,x                ;4   =  20
Lfb56
    dex                             ;2        
    bpl     Lfb3a                   ;2/3 =   4
Lfb59
    jmp     Lf91f                   ;3   =   3
    
Lfb5c
    ldx     #$06                    ;2        
    ldy     ram_B2                  ;3        
    beq     Lfb88                   ;2/3      
    bmi     Lfb76                   ;2/3 =   9
Lfb64
    lda     ram_C5,x                ;4        
    cpy     #$01                    ;2        
    clc                             ;2        
    beq     Lfb6d                   ;2/3      
    adc     #$01                    ;2   =  12
Lfb6d
    adc     #$02                    ;2        
    sta     ram_C5,x                ;4        
    dex                             ;2        
    bpl     Lfb64                   ;2/3      
    bmi     Lfb9e                   ;2/3 =  12
Lfb76
    lda     ram_C5,x                ;4        
    sec                             ;2        
    cpy     #$fe                    ;2        
    bne     Lfb7f                   ;2/3      
    sbc     #$01                    ;2   =  12
Lfb7f
    sbc     #$02                    ;2        
    sta     ram_C5,x                ;4        
    dex                             ;2        
    bpl     Lfb76                   ;2/3      
    bmi     Lfb9e                   ;2/3 =  12
Lfb88
    lda     ram_F0                  ;3        
    beq     Lfbb3                   ;2/3      
    bmi     Lfb97                   ;2/3      
    ldx     #$06                    ;2   =   9
Lfb90
    dec     ram_C5,x                ;6        
    dex                             ;2        
    bpl     Lfb90                   ;2/3      
    bmi     Lfb9e                   ;2/3 =  12
Lfb97
    ldx     #$06                    ;2   =   2
Lfb99
    inc     ram_C5,x                ;6        
    dex                             ;2        
    bpl     Lfb99                   ;2/3 =  10
Lfb9e
    lda     ram_B8                  ;3        
    bpl     Lfba9                   ;2/3      
    lda     ram_C5                  ;3         *
    cmp     #$98                    ;2         *
    jmp     Lfbad                   ;3   =  13 *
    
Lfba9
    lda     ram_C6                  ;3        
    cmp     #$8c                    ;2   =   5
Lfbad
    bcc     Lfbb3                   ;2/3      
    lda     #$ff                    ;2        
    sta     ram_BE                  ;3   =   7
Lfbb3
    rts                             ;6   =   6
    
Lfbb4
    lda     #$09                    ;2        
    sta     ram_AD                  ;3        
    lda     ram_F7                  ;3        
    bmi     Lfbc8                   ;2/3      
    lda     ram_CE                  ;3        
    and     #$03                    ;2        
    bne     Lfbc8                   ;2/3      
    inc     ram_AC                  ;5         *
    lda     #$7f                    ;2         *
    sta     ram_B1                  ;3   =  27 *
Lfbc8
    ldx     #$ff                    ;2        
    stx     ram_9F                  ;3        
    inx                             ;2        
    stx     ram_95                  ;3        
    rts                             ;6   =  16
    
Lfbd0
    jsr     Lfbb4                   ;6        
    lda     ram_A7                  ;3        
    sta     ram_95                  ;3        
    beq     Lfbe5                   ;2/3      
    bmi     Lfbdf                   ;2/3      
    lda     #$01                    ;2        
    sta     ram_A7                  ;3   =  21
Lfbdf
    dec     ram_A7                  ;5        
    dec     ram_A7                  ;5        
    bne     Lfbf0                   ;2/3 =  12
Lfbe5
    lda     ram_DF                  ;3        
    beq     Lfbf0                   ;2/3      
    dec     ram_DF                  ;5        
    inc     ram_F7                  ;5        
    jmp     Lf013                   ;3   =  18
    
Lfbf0
    lda     #$00                    ;2        
    sta     ram_DF                  ;3        
    sta     REFP0                   ;3        
    jmp     Lf5b5                   ;3   =  11
    
Lfbf9
    lda     #$4a                    ;2        
    ldx     #$01                    ;2        
    jsr     Lfe00                   ;6        
    lda     #$52                    ;2        
    ldx     #$00                    ;2        
    jsr     Lfe00                   ;6        
    stx     NUSIZ0                  ;3        
    stx     NUSIZ1                  ;3        
    sta     WSYNC                   ;3   =  29
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     #$27                    ;2        
    sta     TIM64T                  ;4        
    ldy     #$09                    ;2        
    sty     ram_98                  ;3        
    sty     ram_F1                  ;3        
    ldx     ram_CE                  ;3   =  20
Lfc1c
    dex                             ;2        
    bmi     Lfc35                   ;2/3      
    tya                             ;2        
    clc                             ;2        
    adc     ram_98                  ;3        
    cmp     #$63                    ;2        
    bne     Lfc28                   ;2/3      
    tya                             ;2   =  17 *
Lfc28
    sta     ram_98                  ;3        
    cmp     #$09                    ;2        
    bne     Lfc1c                   ;2/3      
    clc                             ;2         *
    adc     ram_F1                  ;3         *
    sta     ram_F1                  ;3         *
    bne     Lfc1c                   ;2/3 =  17 *
Lfc35
    lda     INTIM                   ;4        
    bne     Lfc35                   ;2/3 =   6
Lfc3a
    dec     ram_F1                  ;5        
    dec     ram_98                  ;5        
    ldx     ram_F1                  ;3        
    lda     Lff00,x                 ;4        
    ldx     ram_98                  ;3        
    sta     WSYNC                   ;3   =  23
;---------------------------------------
    sta     GRP1                    ;3        
    lda     Lff00,x                 ;4        
    sta     GRP0                    ;3        
    dey                             ;2        
    bpl     Lfc3a                   ;2/3      
    ldx     #$00                    ;2        
    stx     GRP0                    ;3        
    stx     GRP1                    ;3        
    lda     #$42                    ;2        
    jsr     Lfe00                   ;6        
    lda     #$4a                    ;2        
    ldx     #$01                    ;2        
    jsr     Lfe00                   ;6        
    sta     WSYNC                   ;3   =  43
;---------------------------------------
    sta     HMOVE                   ;3        
    stx     NUSIZ0                  ;3        
    stx     NUSIZ1                  ;3        
    lda     ram_93                  ;3        
    and     #$f0                    ;2        
    sta     ram_92                  ;3        
    ldx     #$2d                    ;2        
    jsr     Lfcf4                   ;6        
    ldx     #$0f                    ;2        
    ldy     ram_DB                  ;3   =  30
Lfc7a
    lda     ram_92                  ;3        
    sta     COLUP0                  ;3        
    sta     COLUP1                  ;3        
    sta     WSYNC                   ;3   =  12
;---------------------------------------
    lda     #$01                    ;2        
    sta     VDELP0                  ;3        
    sta     VDELP1                  ;3        

  IF PAL = 1 || PLUSROM = 1
; No copyright logo at title screen
    lda     #$00                    ;2        
    nop
    sta     GRP0                    ;3        
    lda     #$00                    ;2        
    nop
    nop                             ;2        
    nop                             ;2        
    sta     GRP1                    ;3        
    lda     #$00                    ;2        
    nop
    inc.w   ram_92                  ;6        
    sta     GRP0                    ;3        
    lda     #$00                    ;2        
    nop
  ELSE
    lda     Lfdab,y                 ;4        
    sta     GRP0                    ;3        
    lda     Lfdbe,y                 ;4        
    nop                             ;2        
    nop                             ;2        
    sta     GRP1                    ;3        
    lda     Lfdd1,y                 ;4        
    inc.w   ram_92                  ;6        
    sta     GRP0                    ;3        
    lda     Lfde4,y                 ;4        
  ENDIF

    nop                             ;2        
    sta     GRP1                    ;3        
    sta     GRP0                    ;3        
    dey                             ;2        
    bpl     Lfca9                   ;2/3      
    ldy     #$13                    ;2   =  57
Lfca9
    dex                             ;2        
    bpl     Lfc7a                   ;2/3      
    lda     #$00                    ;2        
    sta     VDELP0                  ;3        
    sta     VDELP1                  ;3        
    sta     GRP0                    ;3        
    sta     GRP1                    ;3        
    dec     ram_9D                  ;5        
    bmi     Lfcbc                   ;2/3      
    bne     Lfcd5                   ;2/3 =  27
Lfcbc
    lda     #$08                    ;2        
    sta     ram_9D                  ;3        
    dec     ram_DB                  ;5        
    bpl     Lfcd5                   ;2/3      
    lda     #$13                    ;2        
    sta     ram_DB                  ;3        
    lda     ram_93                  ;3        
    clc                             ;2        
    adc     #$10                    ;2        
    sta     ram_93                  ;3        
    sta     ram_87                  ;3        
    adc     #$80                    ;2        
    sta     ram_80                  ;3   =  35
Lfcd5
    ldy     #$23                    ;2   =   2
Lfcd7
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    dey                             ;2        
    bne     Lfcd7                   ;2/3      
    lda     ram_B9                  ;3        
    ldx     #$00                    ;2        
    jsr     Lfe00                   ;6        
    sta     WSYNC                   ;3   =  18
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     ram_B7                  ;3        
    sta     NUSIZ0                  ;3        
    lda     #$00                    ;2        
    sta     WSYNC                   ;3   =  14
;---------------------------------------
    sta     HMP0                    ;3        
    jmp     Lf61f                   ;3   =   6
    
Lfcf4
    dex                             ;2   =   2
Lfcf5
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    dex                             ;2        
    bpl     Lfcf5                   ;2/3      
    rts                             ;6   =  10
    
Lfcfb
    .byte   RED|$0                          ; $fcfb (C)
    
    .byte   $42,$0f,$0f,$01                 ; $fcfc (*)
    
Lfd00
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    ldy     #$07                    ;2        
    lda     SWCHB                   ;4        
    and     #$08                    ;2        
    bne     Lfd0d                   ;2/3      
    ldy     #$0e                    ;2   =  12 *
Lfd0d
    ldx     ram_BE                  ;3        
    bpl     Lfd15                   ;2/3      
    ldx     ram_88                  ;3        
    bmi     Lfd16                   ;2/3 =  10
Lfd15
    iny                             ;2   =   2
Lfd16
    sty     COLUP0                  ;3        
    ldy     #$0d                    ;2   =   5
Lfd1a
    dey                             ;2        
    bmi     Lfd37                   ;2/3      
    lda     (ram_DC),y              ;5        
    sta     WSYNC                   ;3   =  12
;---------------------------------------
    sta     GRP0                    ;3        
    lda     (ram_D9),y              ;5        
    sta     GRP1                    ;3        
    cpy     #$04                    ;2        
    bne     Lfd1a                   ;2/3      
    lda     ram_AC                  ;3        
    and     #$03                    ;2        
    tax                             ;2        
    lda     Lfcfb,x                 ;4        
    sta     COLUP0                  ;3        
    bne     Lfd1a                   ;2/3 =  31
Lfd37
    rts                             ;6   =   6
    
Lfd38
    lda     #$00                    ;2        
    sta     NUSIZ1                  ;3        
    ldx     ram_A3                  ;3        
    lda     ram_97                  ;3        
    ldy     ram_9E,x                ;4        
    cpy     ram_BD                  ;3        
    bne     Lfd48                   ;2/3      
    adc     ram_BD                  ;3   =  23
Lfd48
    sta     COLUP1                  ;3        
    lda     ram_98,x                ;4        
    ldx     #$01                    ;2        
    jsr     Lfe00                   ;6        
    lda     INPT3|$30               ;3        
    bmi     Lfd5b                   ;2/3      
    inc     ram_BB                  ;5        
    inc     ram_BB                  ;5        
    inc     ram_BB                  ;5   =  35
Lfd5b
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    ldx     ram_A3                  ;3        
    dec     ram_A3                  ;5        
    lda     ram_81,x                ;4        
    bne     Lfd7c                   ;2/3      
    ldy     #$0d                    ;2   =  19
Lfd69
    dey                             ;2        
    bmi     Lfdaa                   ;2/3      
    lda     Lfebe,y                 ;4        
    sta     WSYNC                   ;3   =  11
;---------------------------------------
    sta     GRP1                    ;3        
    lda     INPT3|$30               ;3        
    bmi     Lfd79                   ;2/3      
    inc     ram_BB                  ;5   =  13
Lfd79
    jmp     Lfd69                   ;3   =   3
    
Lfd7c
    dec     ram_81,x                ;6        
    ldy     #$4f                    ;2        
    cmp     #$0a                    ;2        
    bcs     Lfd8a                   ;2/3      
    ldy     #$05                    ;2        
    sty     NUSIZ1                  ;3        
    ldy     #$45                    ;2   =  19
Lfd8a
    sty     COLUP1                  ;3        
    ldy     #$0c                    ;2   =   5
Lfd8e
    lda     Lff67,y                 ;4   =   4
Lfd91
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     GRP1                    ;3        
    lda     INPT3|$30               ;3        
    bmi     Lfd9b                   ;2/3      
    inc     ram_BB                  ;5   =  13
Lfd9b
    dey                             ;2        
    bmi     Lfdaa                   ;2/3      
    lda     ram_81,x                ;4        
    cmp     #$07                    ;2        
    bcs     Lfd8e                   ;2/3      
    lda     Lff74,y                 ;4        
    jmp     Lfd91                   ;3   =  19
    
Lfdaa
    rts                             ;6   =   6

  IF PAL = 1
Lfdab
    ldx ram_BC
    lda SWCHA 
    and #$c0
    cmp #$80
    bne lfdbd
    inx
    inx
    inx
    inx
    jmp Lfdc5 - $E000
lfdbd
    cmp #$40
    bne Lfdd1_PAL
    dex
    dex
    dex
    dex
Lfdc5
    cpx #$05
    bcs lfdcb
    ldx #$05
lfdcb
    cpx #$fb
    bcc Lfdd1_PAL
    ldx #$fb
Lfdd1_PAL
    stx ram_BC
  IF PLUSROM = 1
    txa
  ELSE
    lda ram_BC
  ENDIF
    ldx #$08
    jmp Lf7cf - $E000

  ENDIF

    IF PLUSROM = 1
Lffe4
    .byte   $2f,$3d,$4e,$5e,$6f,$80,$90     ; $ffe4 (D)
Lffeb
    lda     INPT3|$30               ;3        
    bmi     Lfff1                   ;2/3      
    inc     ram_BB                  ;5   =  10
Lfff1
    rts                             ;6   =   6
    
Lfff2
    .byte   $00                             ; $fff2 (*)
Lfff3
    .byte   $09                             ; $fff3 (D)
    .byte   $12,$1b,$24,$2d,$36,$3f,$48,$51 ; $fff4 (*)

SendPlusROMScore
    ldx     #$04
.Check_last_Loop
    lda     ram_C0,x
    bne     .Return_from_HSC
    dex
    bpl     .Check_last_Loop  

    ldx     #$0a
.HSC_BCD_Loop
    lda     ram_CF,x                ;4        
    sta     WriteToBuffer           ; 
    dex                             ;2        
    dex                             ;2        
    bpl     .HSC_BCD_Loop 
    lda     #HIGHSCORE_ID           ; game id in Highscore DB
    sta     WriteSendBuffer
.Return_from_HSC
    lda     #$0f                    ;2        
    sta     ram_A7                  ;3   =  39
    jmp     Lfa89

    ORG $fdfd, $0

  ELSE
    IF PAL = 0
Lfdab
    .byte   %00000000 ; |        |            $fdab (G)
    .byte   %11111100 ; |######  |            $fdac (G)
    .byte   %00000010 ; |      # |            $fdad (G)
    .byte   %00000010 ; |      # |            $fdae (G)
    .byte   %01111100 ; | #####  |            $fdaf (G)
    .byte   %10000000 ; |#       |            $fdb0 (G)
    .byte   %10000000 ; |#       |            $fdb1 (G)
    .byte   %01111110 ; | ###### |            $fdb2 (G)
    .byte   %00000000 ; |        |            $fdb3 (G)
    .byte   %00000000 ; |        |            $fdb4 (G)
    .byte   %00000000 ; |        |            $fdb5 (G)
    .byte   %01111100 ; | #####  |            $fdb6 (G)
    .byte   %10000010 ; |#     # |            $fdb7 (G)
    .byte   %10111010 ; |# ### # |            $fdb8 (G)
    .byte   %10100010 ; |# #   # |            $fdb9 (G)
    .byte   %10111010 ; |# ### # |            $fdba (G)
    .byte   %10000010 ; |#     # |            $fdbb (G)
    .byte   %01111100 ; | #####  |            $fdbc (G)
    .byte   %00000000 ; |        |            $fdbd (G)
Lfdbe
    .byte   %00000000 ; |        |            $fdbe (G)
    .byte   %11111110 ; |####### |            $fdbf (G)
    .byte   %10000000 ; |#       |            $fdc0 (G)
    .byte   %10000000 ; |#       |            $fdc1 (G)
    .byte   %11111000 ; |#####   |            $fdc2 (G)
    .byte   %10000000 ; |#       |            $fdc3 (G)
    .byte   %10000000 ; |#       |            $fdc4 (G)
    .byte   %11111110 ; |####### |            $fdc5 (G)
    .byte   %00000000 ; |        |            $fdc6 (G)
    .byte   %00000000 ; |        |            $fdc7 (G)
    .byte   %00000000 ; |        |            $fdc8 (G)
    .byte   %01110000 ; | ###    |            $fdc9 (G)
    .byte   %00100000 ; |  #     |            $fdca (G)
    .byte   %00100000 ; |  #     |            $fdcb (G)
    .byte   %00100011 ; |  #   ##|            $fdcc (G)
    .byte   %00100100 ; |  #  #  |            $fdcd (G)
    .byte   %01100100 ; | ##  #  |            $fdce (G)
    .byte   %00100011 ; |  #   ##|            $fdcf (G)
    .byte   %00000000 ; |        |            $fdd0 (G)
Lfdd1
    .byte   %00000000 ; |        |            $fdd1 (G)
    .byte   %01111110 ; | ###### |            $fdd2 (G)
    .byte   %10000010 ; |#     # |            $fdd3 (G)
    .byte   %10000010 ; |#     # |            $fdd4 (G)
    .byte   %10011110 ; |#  #### |            $fdd5 (G)
    .byte   %10000000 ; |#       |            $fdd6 (G)
    .byte   %10000010 ; |#     # |            $fdd7 (G)
    .byte   %01111100 ; | #####  |            $fdd8 (G)
    .byte   %00000000 ; |        |            $fdd9 (G)
    .byte   %00000000 ; |        |            $fdda (G)
    .byte   %00000000 ; |        |            $fddb (G)
    .byte   %01001110 ; | #  ### |            $fddc (G)
    .byte   %01010001 ; | # #   #|            $fddd (G)
    .byte   %01010001 ; | # #   #|            $fdde (G)
    .byte   %11001110 ; |##  ### |            $fddf (G)
    .byte   %01010001 ; | # #   #|            $fde0 (G)
    .byte   %01010001 ; | # #   #|            $fde1 (G)
    .byte   %10001110 ; |#   ### |            $fde2 (G)
    .byte   %00000000 ; |        |            $fde3 (G)
Lfde4
    .byte   %00000000 ; |        |            $fde4 (G)
    .byte   %10000010 ; |#     # |            $fde5 (G)
    .byte   %10000010 ; |#     # |            $fde6 (G)
    .byte   %11111110 ; |####### |            $fde7 (G)
    .byte   %10000010 ; |#     # |            $fde8 (G)
    .byte   %10000010 ; |#     # |            $fde9 (G)
    .byte   %01000100 ; | #   #  |            $fdea (G)
    .byte   %00111000 ; |  ###   |            $fdeb (G)
    .byte   %00000000 ; |        |            $fdec (G)
    .byte   %00000000 ; |        |            $fded (G)
    .byte   %00000000 ; |        |            $fdee (G)
    .byte   %01111100 ; | #####  |            $fdef (G)
    .byte   %00100000 ; |  #     |            $fdf0 (G)
    .byte   %00010000 ; |   #    |            $fdf1 (G)
    .byte   %00001000 ; |    #   |            $fdf2 (G)
    .byte   %00000100 ; |     #  |            $fdf3 (G)
    .byte   %01000100 ; | #   #  |            $fdf4 (G)
    .byte   %00111000 ; |  ###   |            $fdf5 (G)
    .byte   %00000000 ; |        |            $fdf6 (G)
    .byte   %00000000 ; |        |            $fdf7 (G)
    ENDIF
    ORG $fdf8, $0
  ENDIF

Lfdf8
    .byte   $09                             ; $fdf8 (D)
    .byte   $12,$2d                         ; $fdf9 (*)

  IF PLUSROM = 0
Lfdfb
    .byte   $46,$56,$3e,$4e,$5e             ; $fdfb (D)
  ENDIF

Lfe00
    tay                             ;2        
    lda     Lfe1d,y                 ;4        
    pha                             ;3        
    and     #$0f                    ;2        
    tay                             ;2        
    pla                             ;4        
    sta     WSYNC                   ;3   =  20
;---------------------------------------
Lfe0b
    dey                             ;2        
    bpl     Lfe0b                   ;2/3      
    sta     RESP0,x                 ;4        
    sta     WSYNC                   ;3   =  11
;---------------------------------------
    sta     HMP0,x                  ;4        
    rts                             ;6   =  10
    
Lfe15
    .byte   $90                             ; $fe15 (D)
    .byte   $00,$50,$10                     ; $fe16 (*)
Lfe19
    .byte   $40                             ; $fe19 (D)
    .byte   $20,$90,$73                     ; $fe1a (*)
Lfe1d
    .byte   $73,$63,$53,$43,$33,$23,$13,$03 ; $fe1d (D)
    .byte   $f3,$e3,$d3,$c3,$b3,$a3,$93,$74 ; $fe25 (D)
    .byte   $64,$54,$44,$34,$24,$14,$04,$f4 ; $fe2d (D)
    .byte   $e4,$d4,$c4,$b4,$a4,$94,$75,$65 ; $fe35 (D)
    .byte   $55,$45,$35,$25,$15,$05,$f5,$e5 ; $fe3d (D)
    .byte   $d5,$c5,$b5,$a5,$95,$76,$66,$56 ; $fe45 (D)
    .byte   $46,$36,$26,$16,$06,$f6,$e6,$d6 ; $fe4d (D)
    .byte   $c6,$b6,$a6,$96,$77,$67,$57,$47 ; $fe55 (D)
    .byte   $37,$27,$17,$07,$f7,$e7,$d7,$c7 ; $fe5d (D)
    .byte   $b7,$a7,$97,$78,$68,$58,$48,$38 ; $fe65 (D)
    .byte   $28,$18,$08,$f8,$e8,$d8,$c8,$b8 ; $fe6d (D)
    .byte   $a8,$98,$79,$69,$59,$49,$39,$29 ; $fe75 (D)
    .byte   $19,$09,$f9,$e9,$d9,$c9,$b9,$a9 ; $fe7d (D)
    .byte   $99,$7a,$6a,$5a,$4a,$3a,$2a,$1a ; $fe85 (D)
    .byte   $0a,$fa,$ea,$da,$ca,$ba,$aa,$9a ; $fe8d (D)
    .byte   $7b,$6b,$5b,$4b,$3b,$2b,$1b,$0b ; $fe95 (D)
    .byte   $fb,$eb,$db,$cb,$bb,$ab,$9b,$7c ; $fe9d (D)
    .byte   $6c,$5c,$4c,$3c,$2c,$1c,$0c,$fc ; $fea5 (D)
    .byte   $ec,$dc,$cc,$bc,$ac,$9c,$7d,$6d ; $fead (D)
    .byte   $5d,$4d,$3d,$2d,$1d,$0d,$fd,$ed ; $feb5 (D)
    .byte   $dd                             ; $febd (D)

Lfebe
    .byte   %00000000 ; |        |            $febe (G)
    .byte   %00011000 ; |   ##   |            $febf (G)
    .byte   %00011000 ; |   ##   |            $fec0 (G)
    .byte   %00100100 ; |  #  #  |            $fec1 (G)
    .byte   %01111110 ; | ###### |            $fec2 (G)
    .byte   %11111111 ; |########|            $fec3 (G)
    .byte   %11111111 ; |########|            $fec4 (G)
    .byte   %11111111 ; |########|            $fec5 (G)
    .byte   %11100111 ; |###  ###|            $fec6 (G)
    .byte   %11000011 ; |##    ##|            $fec7 (G)
    .byte   %10000001 ; |#      #|            $fec8 (G)
    .byte   %10000001 ; |#      #|            $fec9 (G)
    .byte   %10000001 ; |#      #|            $feca (G)
Lfecb
    .byte   %00000000 ; |        |            $fecb (G)
    .byte   %01000010 ; | #    # |            $fecc (G)
    .byte   %01000010 ; | #    # |            $fecd (G)
    .byte   %01000010 ; | #    # |            $fece (G)
    .byte   %01000010 ; | #    # |            $fecf (G)
    .byte   %01000010 ; | #    # |            $fed0 (G)
    .byte   %01000010 ; | #    # |            $fed1 (G)
    
Lfed2
    ldy     #$0d                    ;2   =   2
Lfed4
    lda     (ram_D9),y              ;5        
    sta     WSYNC                   ;3   =   8
;---------------------------------------
    sta     GRP1                    ;3        
    dey                             ;2        
    bpl     Lfed4                   ;2/3      
    rts                             ;6   =  13
    
Lfede
    stx     ram_F1                  ;3        
    dec     ram_88                  ;5   =   8
Lfee2
    cpx     #$03                    ;2        
    beq     Lfef1                   ;2/3      
    lda     ram_8F,x                ;4        
    sta     ram_8E,x                ;4        
    lda     ram_8B,x                ;4        
    sta     ram_8A,x                ;4        
    inx                             ;2        
    bpl     Lfee2                   ;2/3 =  24
Lfef1
    lda     #$ff                    ;2        
    sta     ram_8D                  ;3        
    ldx     ram_F1                  ;3        
    rts                             ;6   =  14
    
Lfef8
    .byte   $f0,$e0,$d0,$d0                 ; $fef8 (D)
Lfefc
    .byte   $25                             ; $fefc (D)
    .byte   $0f                             ; $fefd (*)
    .byte   $d4                             ; $fefe (D)
    .byte   $54                             ; $feff (*)
    
Lff00
    .byte   %01111110 ; | ###### |            $ff00 (G)
    .byte   %01000010 ; | #    # |            $ff01 (G)
    .byte   %01000010 ; | #    # |            $ff02 (G)
    .byte   %01000010 ; | #    # |            $ff03 (G)
    .byte   %01000010 ; | #    # |            $ff04 (G)
    .byte   %01000010 ; | #    # |            $ff05 (G)
    .byte   %01000010 ; | #    # |            $ff06 (G)
    .byte   %01000010 ; | #    # |            $ff07 (G)
    .byte   %01111110 ; | ###### |            $ff08 (G)
    .byte   %00010000 ; |   #    |            $ff09 (G)
    .byte   %00010000 ; |   #    |            $ff0a (G)
    .byte   %00010000 ; |   #    |            $ff0b (G)
    .byte   %00010000 ; |   #    |            $ff0c (G)
    .byte   %00010000 ; |   #    |            $ff0d (G)
    .byte   %00010000 ; |   #    |            $ff0e (G)
    .byte   %00010000 ; |   #    |            $ff0f (G)
    .byte   %00010000 ; |   #    |            $ff10 (G)
    .byte   %00010000 ; |   #    |            $ff11 (G)
    .byte   %01111110 ; | ###### |            $ff12 (G)
    .byte   %01000000 ; | #      |            $ff13 (G)
    .byte   %01000000 ; | #      |            $ff14 (G)
    .byte   %01000000 ; | #      |            $ff15 (G)
    .byte   %01111110 ; | ###### |            $ff16 (G)
    .byte   %00000010 ; |      # |            $ff17 (G)
    .byte   %00000010 ; |      # |            $ff18 (G)
    .byte   %00000010 ; |      # |            $ff19 (G)
    .byte   %01111110 ; | ###### |            $ff1a (G)
    .byte   %01111110 ; | ###### |            $ff1b (G)
    .byte   %00000010 ; |      # |            $ff1c (G)
    .byte   %00000010 ; |      # |            $ff1d (G)
    .byte   %00000010 ; |      # |            $ff1e (G)
    .byte   %01111110 ; | ###### |            $ff1f (G)
    .byte   %00000010 ; |      # |            $ff20 (G)
    .byte   %00000010 ; |      # |            $ff21 (G)
    .byte   %00000010 ; |      # |            $ff22 (G)
    .byte   %01111110 ; | ###### |            $ff23 (G)
    .byte   %00000010 ; |      # |            $ff24 (G)
    .byte   %00000010 ; |      # |            $ff25 (G)
    .byte   %00000010 ; |      # |            $ff26 (G)
    .byte   %00000010 ; |      # |            $ff27 (G)
    .byte   %01111110 ; | ###### |            $ff28 (G)
    .byte   %01000010 ; | #    # |            $ff29 (G)
    .byte   %01000010 ; | #    # |            $ff2a (G)
    .byte   %01000010 ; | #    # |            $ff2b (G)
    .byte   %01000010 ; | #    # |            $ff2c (G)
    .byte   %01111110 ; | ###### |            $ff2d (G)
    .byte   %00000010 ; |      # |            $ff2e (G)
    .byte   %00000010 ; |      # |            $ff2f (G)
    .byte   %00000010 ; |      # |            $ff30 (G)
    .byte   %01111110 ; | ###### |            $ff31 (G)
    .byte   %01000000 ; | #      |            $ff32 (G)
    .byte   %01000000 ; | #      |            $ff33 (G)
    .byte   %01000000 ; | #      |            $ff34 (G)
    .byte   %01111110 ; | ###### |            $ff35 (G)
    .byte   %01111110 ; | ###### |            $ff36 (G)
    .byte   %01000010 ; | #    # |            $ff37 (G)
    .byte   %01000010 ; | #    # |            $ff38 (G)
    .byte   %01000010 ; | #    # |            $ff39 (G)
    .byte   %01111110 ; | ###### |            $ff3a (G)
    .byte   %01000000 ; | #      |            $ff3b (G)
    .byte   %01000000 ; | #      |            $ff3c (G)
    .byte   %01000000 ; | #      |            $ff3d (G)
    .byte   %01000000 ; | #      |            $ff3e (G)
    .byte   %00000010 ; |      # |            $ff3f (G)
    .byte   %00000010 ; |      # |            $ff40 (G)
    .byte   %00000010 ; |      # |            $ff41 (G)
    .byte   %00000010 ; |      # |            $ff42 (G)
    .byte   %00000010 ; |      # |            $ff43 (G)
    .byte   %00000010 ; |      # |            $ff44 (G)
    .byte   %00000010 ; |      # |            $ff45 (G)
    .byte   %00000010 ; |      # |            $ff46 (G)
    .byte   %01111110 ; | ###### |            $ff47 (G)
    .byte   %01111110 ; | ###### |            $ff48 (G)
    .byte   %01000010 ; | #    # |            $ff49 (G)
    .byte   %01000010 ; | #    # |            $ff4a (G)
    .byte   %01000010 ; | #    # |            $ff4b (G)
    .byte   %01111110 ; | ###### |            $ff4c (G)
    .byte   %01000010 ; | #    # |            $ff4d (G)
    .byte   %01000010 ; | #    # |            $ff4e (G)
    .byte   %01000010 ; | #    # |            $ff4f (G)
    .byte   %01111110 ; | ###### |            $ff50 (G)
    .byte   %00000010 ; |      # |            $ff51 (G)
    .byte   %00000010 ; |      # |            $ff52 (G)
    .byte   %00000010 ; |      # |            $ff53 (G)
    .byte   %00000010 ; |      # |            $ff54 (G)
    .byte   %01111110 ; | ###### |            $ff55 (G)
    .byte   %01000010 ; | #    # |            $ff56 (G)
    .byte   %01000010 ; | #    # |            $ff57 (G)
    .byte   %01000010 ; | #    # |            $ff58 (G)
    .byte   %01111110 ; | ###### |            $ff59 (G)
    .byte   %00000000 ; |        |            $ff5a (G)
    .byte   %00000000 ; |        |            $ff5b (G)
    .byte   %00000000 ; |        |            $ff5c (G)
    .byte   %00000000 ; |        |            $ff5d (G)
    .byte   %00000000 ; |        |            $ff5e (G)
    .byte   %00000000 ; |        |            $ff5f (G)
    .byte   %00000000 ; |        |            $ff60 (G)
    .byte   %00000000 ; |        |            $ff61 (G)
    .byte   %00000000 ; |        |            $ff62 (G)
    .byte   %00000000 ; |        |            $ff63 (G)
    .byte   %00000000 ; |        |            $ff64 (G)
    .byte   %00000000 ; |        |            $ff65 (G)
    .byte   %00000000 ; |        |            $ff66 (G)
Lff67
    .byte   %00000000 ; |        |            $ff67 (G)
    .byte   %00100000 ; |  #     |            $ff68 (G)
    .byte   %10010010 ; |#  #  # |            $ff69 (G)
    .byte   %00000000 ; |        |            $ff6a (G)
    .byte   %00100100 ; |  #  #  |            $ff6b (G)
    .byte   %00000101 ; |     # #|            $ff6c (G)
    .byte   %01000000 ; | #      |            $ff6d (G)
    .byte   %00100010 ; |  #   # |            $ff6e (G)
    .byte   %00000000 ; |        |            $ff6f (G)
    .byte   %00100100 ; |  #  #  |            $ff70 (G)
    .byte   %10000000 ; |#       |            $ff71 (G)
    .byte   %00000000 ; |        |            $ff72 (G)
    .byte   %00100001 ; |  #    #|            $ff73 (G)
Lff74
    .byte   %00000000 ; |        |            $ff74 (G)
    .byte   %10000000 ; |#       |            $ff75 (G)
    .byte   %00000000 ; |        |            $ff76 (G)
    .byte   %00000000 ; |        |            $ff77 (G)
    .byte   %00000001 ; |       #|            $ff78 (G)
    .byte   %00000000 ; |        |            $ff79 (G)
    .byte   %00000000 ; |        |            $ff7a (G)
    .byte   %01000000 ; | #      |            $ff7b (G)
    .byte   %00000000 ; |        |            $ff7c (G)
    .byte   %00000000 ; |        |            $ff7d (G)
    .byte   %00000010 ; |      # |            $ff7e (G)
    .byte   %00000000 ; |        |            $ff7f (G)
    .byte   %10000000 ; |#       |            $ff80 (G)
Lff81
    .byte   %00000000 ; |        |            $ff81 (G)
    .byte   %00000000 ; |        |            $ff82 (G)
    .byte   %00000000 ; |        |            $ff83 (G)
    .byte   %00000000 ; |        |            $ff84 (G)
    .byte   %00000000 ; |        |            $ff85 (G)
    .byte   %00000000 ; |        |            $ff86 (G)
    .byte   %10000001 ; |#      #|            $ff87 (G)
    .byte   %01100110 ; | ##  ## |            $ff88 (G)
    .byte   %01111110 ; | ###### |            $ff89 (G)
    .byte   %00111100 ; |  ####  |            $ff8a (G)
    .byte   %00111100 ; |  ####  |            $ff8b (G)
    .byte   %00011000 ; |   ##   |            $ff8c (G)
    .byte   %00011000 ; |   ##   |            $ff8d (G)
Lff8e
    .byte   %00000000 ; |        |            $ff8e (G)
    .byte   %00010000 ; |   #    |            $ff8f (G)
    .byte   %00011000 ; |   ##   |            $ff90 (G)
    .byte   %00111000 ; |  ###   |            $ff91 (G)
    .byte   %00000000 ; |        |            $ff92 (G)
    .byte   %00000000 ; |        |            $ff93 (G)
    .byte   %10000001 ; |#      #|            $ff94 (G)
    .byte   %01100110 ; | ##  ## |            $ff95 (G)
    .byte   %01111110 ; | ###### |            $ff96 (G)
    .byte   %00111100 ; |  ####  |            $ff97 (G)
    .byte   %00111100 ; |  ####  |            $ff98 (G)
    .byte   %00011000 ; |   ##   |            $ff99 (G)
    .byte   %00011000 ; |   ##   |            $ff9a (G)
    .byte   %00000000 ; |        |            $ff9b (G)
    .byte   %00000000 ; |        |            $ff9c (G)
    .byte   %00000000 ; |        |            $ff9d (G)
    .byte   %00000000 ; |        |            $ff9e (G)
    .byte   %10000000 ; |#       |            $ff9f (G)
    .byte   %10000000 ; |#       |            $ffa0 (G)
    .byte   %11000000 ; |##      |            $ffa1 (G)
    .byte   %01100000 ; | ##     |            $ffa2 (G)
    .byte   %01111000 ; | ####   |            $ffa3 (G)
    .byte   %01111111 ; | #######|            $ffa4 (G)
    .byte   %00111100 ; |  ####  |            $ffa5 (G)
    .byte   %00111000 ; |  ###   |            $ffa6 (G)
    .byte   %00100000 ; |  #     |            $ffa7 (G)
    .byte   %00000000 ; |        |            $ffa8 (G)
    .byte   %00000110 ; |     ## |            $ffa9 (G)
    .byte   %00001110 ; |    ### |            $ffaa (G)
    .byte   %00001100 ; |    ##  |            $ffab (G)
    .byte   %10001100 ; |#   ##  |            $ffac (G)
    .byte   %10000000 ; |#       |            $ffad (G)
    .byte   %11000000 ; |##      |            $ffae (G)
    .byte   %01100000 ; | ##     |            $ffaf (G)
    .byte   %01111000 ; | ####   |            $ffb0 (G)
    .byte   %01111111 ; | #######|            $ffb1 (G)
    .byte   %00111100 ; |  ####  |            $ffb2 (G)
    .byte   %00111000 ; |  ###   |            $ffb3 (G)
    .byte   %00100000 ; |  #     |            $ffb4 (G)
    .byte   %00000000 ; |        |            $ffb5 (G)
    .byte   %00000000 ; |        |            $ffb6 (G)
    .byte   %00000000 ; |        |            $ffb7 (G)
    .byte   %00000000 ; |        |            $ffb8 (G)
    .byte   %10000000 ; |#       |            $ffb9 (G)
    .byte   %10000000 ; |#       |            $ffba (G)
    .byte   %11000000 ; |##      |            $ffbb (G)
    .byte   %11000000 ; |##      |            $ffbc (G)
    .byte   %11100000 ; |###     |            $ffbd (G)
    .byte   %11100000 ; |###     |            $ffbe (G)
    .byte   %11110000 ; |####    |            $ffbf (G)
    .byte   %11111000 ; |#####   |            $ffc0 (G)
    .byte   %11111100 ; |######  |            $ffc1 (G)
    .byte   %00000000 ; |        |            $ffc2 (G)
    .byte   %00000000 ; |        |            $ffc3 (G)
    .byte   %00000110 ; |     ## |            $ffc4 (G)
    .byte   %00001110 ; |    ### |            $ffc5 (G)
    .byte   %10001100 ; |#   ##  |            $ffc6 (G)
    .byte   %10001100 ; |#   ##  |            $ffc7 (G)
    .byte   %11000000 ; |##      |            $ffc8 (G)
    .byte   %11000000 ; |##      |            $ffc9 (G)
    .byte   %11100000 ; |###     |            $ffca (G)
    .byte   %11100000 ; |###     |            $ffcb (G)
    .byte   %11110000 ; |####    |            $ffcc (G)
    .byte   %11111000 ; |#####   |            $ffcd (G)
    .byte   %11111100 ; |######  |            $ffce (G)
    
Lffcf
    .byte   $c4                             ; $ffcf (D)
    .byte   $a6,$4c,$90                     ; $ffd0 (*)
Lffd3
    .byte   $40                             ; $ffd3 (D)
    .byte   $50,$38,$48,$58                 ; $ffd4 (*)
Lffd8
    .byte   $4e                             ; $ffd8 (D)
    .byte   $5e,$47,$57,$67                 ; $ffd9 (*)
Lffdd
    .byte   $1e,$78,$3c,$5a,$96,$2d,$69     ; $ffdd (D)

    IF PLUSROM = 1
PlusROM_API
       .byte "a", 0, "h.firmaplus.de"
       .byte 0
Lfdfb
    .byte   $46,$56,$3e,$4e,$5e             ; $fdfb (D)

      ORG $FFFA
       .word (PlusROM_API - $E000)      ; PlusRom API pointer

    ELSE
Lffe4
    .byte   $2f,$3d,$4e,$5e,$6f,$80,$90     ; $ffe4 (D)
Lffeb
    lda     INPT3|$30               ;3        
    bmi     Lfff1                   ;2/3      
    inc     ram_BB                  ;5   =  10
Lfff1
    rts                             ;6   =   6
    
Lfff2
    .byte   $00                             ; $fff2 (*)
Lfff3
    .byte   $09                             ; $fff3 (D)
    .byte   $12,$1b,$24,$2d,$36,$3f,$48,$51 ; $fff4 (*)
    ENDIF

    .word   Start
    .word   Start
    