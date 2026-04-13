; Disassembly of \Public ROMs\Classic Roms\NTSC\BY ALPHABET\S-Z\Xevious (Prototype).bin
; Disassembled 04/11/26 03:04:15
; Using Stella 7.0
;
; ROM properties name : Xevious (01-18-1984) (Atari) (Prototype)
; ROM properties MD5  : af6f3e9718bccfcd8afb421f96561a34
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

PLUSROM     = 1
PAL         = 0

; constants

  IF PLUSROM == 1
WriteToBuffer           = $1FF0
WriteSendBuffer         = $1FF1
ReceiveBuffer           = $1FF2
ReceiveBufferSize       = $1FF3

HIGHSCORE_ID            = 92         ; Xevious game ID in Highscore DB
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
BEIGE            = $20
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
REFP0           = $0b  ; (W)
REFP1           = $0c  ; (W)
PF0             = $0d  ; (W)
PF1             = $0e  ; (W)
PF2             = $0f  ; (W)
RESP0           = $10  ; (W)
RESP1           = $11  ; (W)
RESM0           = $12  ; (W)
RESM1           = $13  ; (W)
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
HMP0            = $20  ; (W)
HMP1            = $21  ; (W)
HMM0            = $22  ; (W)
HMM1            = $23  ; (W)
VDELP0          = $25  ; (W)
VDELP1          = $26  ; (W)
HMOVE           = $2a  ; (W)
HMCLR           = $2b  ; (W)
CXCLR           = $2c  ; (W)

SWCHA           = $0280
SWACNT          = $0281
SWCHB           = $0282
SWBCNT          = $0283
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
score_bcd_hi    = $88
score_bcd_mid   = $89
score_bcd_lo    = $8a
ram_8B          = $8b
ram_8C          = $8c
ram_8D          = $8d
ram_8E          = $8e
ram_8F          = $8f
ram_90          = $90
ram_91          = $91
ram_92          = $92
game_variation  = $93
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
ram_A7          = $a7
;                 $a8  (i)
;                 $a9  (i)
;                 $aa  (i)
ram_AB          = $ab
;                 $ac  (i)
;                 $ad  (i)
;                 $ae  (i)
;                 $af  (i)
;                 $b0  (i)
;                 $b1  (i)
;                 $b2  (i)
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
ram_CB          = $cb
;                 $cc  (i)
;                 $cd  (i)
;                 $ce  (i)
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
ram_F5          = $f5
ram_F6          = $f6
ram_F7          = $f7
ram_F8          = $f8
ram_F9          = $f9
ram_FA          = $fa; (s)
;                 $fb  (is)
;                 $fc  (is)
;                 $fd  (is)
;                 $fe  (s)
;                 $ff  (s)


;-----------------------------------------------------------
;      User Defined Labels
;-----------------------------------------------------------

Start           = $f000


;***********************************************************
;      Bank 0 / 0..1
;***********************************************************

    SEG     CODE
    ORG     $0000
    RORG    $f000

Start
    .byte   $8d,$f8,$ff                     ; $f000 (*)
    
Lf003
    jmp     Lf01c                   ;3   =   3
    
    sta     Lfff9                   ;4        
    jmp     Lfd93                   ;3   =   7
    
    sta     Lfff9                   ;4        
    jmp     Lf120                   ;3   =   7
    
    sta     Lfff9                   ;4        
    jmp     Lf213                   ;3   =   7
    
    sta     Lfff9                   ;4        
    rts                             ;6   =  10
    
Lf01c
    cli                             ;2        
    cld                             ;2        
    ldx     #$ff                    ;2        
    txs                             ;2        
    inx                             ;2        
    txa                             ;2   =  12
Lf023
    sta     VSYNC,x                 ;4        
    inx                             ;2        
    bne     Lf023                   ;2/3      
    sta     SWACNT                  ;4        
    sta     SWBCNT                  ;4        
    dex                             ;2        
    stx     ram_83                  ;3        
    stx     ram_84                  ;3        
    lda     #$80                    ;2        
    sta     ram_91                  ;3        
    lda     #$04                    ;2        
    sta     ram_9E                  ;3        
    jmp     Lf8f5                   ;3   =  37
    
Lf03e
    lda     INTIM                   ;4        
    bne     Lf03e                   ;2/3      
    lda     #$03                    ;2        
    sta     WSYNC                   ;3   =  11
;---------------------------------------
    sta     VSYNC                   ;3        
    lda     ram_83                  ;3        
    asl                             ;2        
    eor     ram_83                  ;3        
    asl                             ;2        
    asl                             ;2        
    rol     ram_83                  ;5        
    rol     ram_84                  ;5        
    sta     WSYNC                   ;3   =  28
;---------------------------------------
    lda     ram_82                  ;3        
    and     #$fe                    ;2        
    sta     ram_82                  ;3        
    lda     #$40                    ;2        
    sta     ram_E7                  ;3        
    lda     ram_91                  ;3        
    and     #$01                    ;2        
    beq     Lf068                   ;2/3      
    asl     ram_E7                  ;5   =  25 *
Lf068
    lda     SWCHB                   ;4        
    and     ram_E7                  ;3        
    beq     Lf071                   ;2/3      
    inc     ram_82                  ;5   =  14 *
Lf071
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     #$00                    ;2        
    sta     VSYNC                   ;3        
    lda     #$22                    ;2        
    sta     TIM64T                  ;4        
    lda     ram_82                  ;3        
    clc                             ;2        
    adc     #$20                    ;2        
    bcc     Lf08d                   ;2/3      
    inc     ram_97                  ;5        
    lda     ram_82                  ;3        
    and     #$1f                    ;2        
    ora     #$60                    ;2   =  32
Lf08d
    sta     ram_82                  ;3        
    lda     ram_97                  ;3        
    bne     Lf09b                   ;2/3      
    inc     ram_80                  ;5        
    bne     Lf09b                   ;2/3      
    inc     ram_81                  ;5        
    lda     ram_81                  ;3   =  23
Lf09b
    jmp     Lf3b5                   ;3   =   3
    
Lf09e
    lda     INTIM                   ;4        
    bne     Lf09e                   ;2/3      
    lda     #$e1                    ;2        
    sta     TIM64T                  ;4        
    lda     #$01                    ;2        
    sta     CTRLPF                  ;3        
    sta     PF0                     ;3        
    lda     #BLACK|$8               ;2        
    sta     COLUP1                  ;3        
    sta     HMCLR                   ;3        
    sta     CXCLR                   ;3        
    lda     ram_82                  ;3        
    lsr                             ;2        
    lda     #$03                    ;2        
    bcs     Lf0bf                   ;2/3      
    lda     #$02                    ;2   =  42
Lf0bf
    sta     ram_E7                  ;3        
    jmp     $b006                   ;3   =   6
    
Lf0c4
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    ldx     #$ff                    ;2        
    txs                             ;2        
    stx     VBLANK                  ;3        
    inx                             ;2        
    stx     VDELP0                  ;3        
    stx     VDELP1                  ;3        
    lda     SWCHB                   ;4        
    and     #$01                    ;2        
    bne     Lf0da                   ;2/3      
    jmp     Lf8d1                   ;3   =  26
    
Lf0da
    lda     SWCHB                   ;4        
    eor     #$ff                    ;2        
    and     #$02                    ;2        
    beq     Lf101                   ;2/3!     
    inc     ram_97                  ;5        
    lda     ram_91                  ;3        
    ora     #$80                    ;2        
    sta     ram_91                  ;3        
    inc     ram_92                  ;5        
    lda     ram_92                  ;3        
    cmp     #$0a                    ;2        
    beq     Lf0f7                   ;2/3      
    cmp     #$28                    ;2        
    bne     Lf103                   ;2/3!=  39
Lf0f7
    inc     game_variation          ;5        
    lda     game_variation          ;3        
    and     #$07                    ;2        
    sta     game_variation          ;3        
    lda     #$0b                    ;2   =  15
Lf101
    sta     ram_92                  ;3   =   3
Lf103
    bit     ram_91                  ;3        
    bpl     Lf123                   ;2/3      
    lda     #$00                    ;2        
    sta     AUDV0                   ;3        
    sta     AUDV1                   ;3        
    sta     AUDC0                   ;3        
    sta     AUDC1                   ;3        
    lda     ram_81                  ;3        
    beq     Lf120                   ;2/3      
    bit     INPT4|$30               ;3        
    bmi     Lf120                   ;2/3      
    lda     ram_A4                  ;3         *
    sta     ram_87                  ;3         *
    jmp     Lf8d7                   ;3   =  38 *
    
Lf120
    jmp     Lf627                   ;3   =   3
    
Lf123
    jmp     $b00c                   ;3   =   3
    
Lf126
    lda     ram_82                  ;3        
    lsr                             ;2        
    lda     #$07                    ;2        
    bcs     Lf12e                   ;2/3      
    lsr                             ;2   =  11
Lf12e
    and     ram_80                  ;3        
    asl                             ;2        
    tax                             ;2        
    lda     Lf146,x                 ;4        
    sta     ram_DF                  ;3        
    lda     Lf147,x                 ;4        
    sta     ram_E0                  ;3        
    ldx     ram_95                  ;3        
    lda     Lfb67,x                 ;4        
    sta     ram_E7                  ;3        
    jmp.ind (ram_DF)                ;5   =  36
    
Lf146
    .byte   $56                             ; $f146 (D)
Lf147
    .byte   $f1,$97,$f7,$c0,$f1,$f9,$f1     ; $f147 (D)
    .byte   $1d,$f2,$97,$f7,$80,$f1,$80,$f1 ; $f14e (*)
    
Lf156
    lda     ram_D3                  ;3        
    and     #$07                    ;2        
    beq     Lf164                   ;2/3      
    lda     ram_E6                  ;3        
    ora     #$04                    ;2        
    sta     ram_E6                  ;3        
    inc     ram_D4                  ;5   =  20
Lf164
    lda     ram_91                  ;3        
    and     #$02                    ;2        
    bne     Lf175                   ;2/3      
    ldx     #$03                    ;2   =   9
Lf16c
    inc     ram_B7,x                ;6        
    bne     Lf172                   ;2/3      
    dec     ram_B7,x                ;6   =  14
Lf172
    dex                             ;2        
    bpl     Lf16c                   ;2/3 =   4
Lf175
    lda     ram_F8                  ;3        
    sec                             ;2        
    adc     #$01                    ;2        
    cmp     ram_A0                  ;3        
    beq     Lf181                   ;2/3      
    inc     ram_A0                  ;5        
    rts                             ;6   =  23
    
Lf181
    lda     #$00                    ;2        
    sta     ram_A0                  ;3        
    lda     ram_91                  ;3        
    and     #$08                    ;2        
    beq     Lf197                   ;2/3      
    lda     ram_91                  ;3        
    and     #$f7                    ;2        
    sta     ram_91                  ;3        
    ldx     ram_D7                  ;3        
    lda     #$01                    ;2        
    sta     ram_B7,x                ;4   =  29
Lf197
    lda     ram_91                  ;3        
    and     #$02                    ;2        
    beq     Lf1ad                   ;2/3      
    lda     ram_91                  ;3        
    and     #$fd                    ;2        
    sta     ram_91                  ;3        
    lda     #$00                    ;2        
    sta     ram_B3                  ;3        
    sta     ram_B4                  ;3        
    sta     ram_B5                  ;3        
    sta     ram_B6                  ;3   =  29
Lf1ad
    inc     ram_9E                  ;5        
    ldy     ram_87                  ;3        
    lda     Lfa1d,y                 ;4        
    cmp     ram_9E                  ;3        
    bne     Lf1bf                   ;2/3      
    lda     #$01                    ;2        
    sta     ram_9E                  ;3        
    jsr     Lf938                   ;6   =  28
Lf1bf
    rts                             ;6   =   6
    
    lda     ram_A0                  ;3        
    bne     Lf1f8                   ;2/3      
    lda     ram_9F                  ;3        
    and     #$03                    ;2        
    clc                             ;2        
    adc     ram_86                  ;3        
    tax                             ;2        
    lda     Lfb41,x                 ;4        
    sta     ram_94                  ;3        
    lda     ram_F7                  ;3        
    and     #$40                    ;2        
    beq     Lf1f8                   ;2/3      
    inc     ram_95                  ;5        
    ldx     ram_95                  ;3        
    cpx     #$2d                    ;2        
    bne     Lf1e3                   ;2/3      
    ldx     #$00                    ;2         *
    stx     ram_95                  ;3   =  48 *
Lf1e3
    ldx     ram_95                  ;3        
    lda     Lfb67,x                 ;4        
    and     #$40                    ;2        
    bne     Lf1f8                   ;2/3      
    inc     ram_86                  ;5        
    lda     ram_86                  ;3        
    cmp     #$21                    ;2        
    bne     Lf1f8                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_86                  ;3   =  28
Lf1f8
    rts                             ;6   =   6
    
    lda     ram_F8                  ;3        
    sec                             ;2        
    sbc     ram_A0                  ;3        
    cmp     #$05                    ;2        
    bne     Lf1f8                   ;2/3!     
    lda     ram_F7                  ;3        
    and     #$20                    ;2        
    beq     Lf1f8                   ;2/3!     
    ldx     ram_D7                  ;3        
    lda     Lfa7d,x                 ;4        
    sta     ram_D7                  ;3        
    tax                             ;2        
    jmp     $b012                   ;3   =  34
    
Lf213
    iny                             ;2        
    iny                             ;2        
    cpy     #$70                    ;2        
    bcc     Lf21b                   ;2/3      
    ldy     #$00                    ;2   =  10
Lf21b
    sty     ram_96                  ;3        
    rts                             ;6   =   9
    
Lf21e
    ldy     #$c0                    ;2        
    lda     ram_BF,x                ;4        
    and     #$3f                    ;2        
    sta     ram_E7                  ;3        
    lsr                             ;2        
    cmp     ram_D5                  ;3        
    bcs     Lf22d                   ;2/3      
    ldy     #$80                    ;2   =  20
Lf22d
    tya                             ;2        
    ora     ram_E7                  ;3        
    sta     ram_BF,x                ;4        
    lda     ram_A5                  ;3        
    lsr                             ;2        
    sec                             ;2        
    sbc     ram_BB                  ;3        
    bpl     Lf23c                   ;2/3      
    eor     #$ff                    ;2   =  23
Lf23c
    lsr                             ;2        
    sta     ram_E7                  ;3        
    lda     ram_BF,x                ;4        
    and     #$3f                    ;2        
    lsr                             ;2        
    sec                             ;2        
    sbc     ram_D5                  ;3        
    bpl     Lf24b                   ;2/3      
    eor     #$ff                    ;2   =  22
Lf24b
    cmp     ram_E7                  ;3        
    bcs     Lf255                   ;2/3      
    lda     ram_BF,x                ;4        
    and     #$7f                    ;2        
    sta     ram_BF,x                ;4   =  15
Lf255
    rts                             ;6   =   6
    
Lf256
    .byte   $01,$01,$01,$01,$03,$02,$03,$03 ; $f256 (D)
    .byte   $01,$01,$01,$01,$01,$02,$01,$02 ; $f25e (D)
    .byte   $02,$02,$02,$02,$03,$02,$03,$02 ; $f266 (D)
    .byte   $03,$03,$03,$03,$04,$03,$04,$03 ; $f26e (D)
    .byte   $00,$00,$00,$00,$02,$02,$02,$02 ; $f276 (D)
    .byte   $01,$01,$01,$01,$02,$02,$02,$01 ; $f27e (D)
    
Lf286
    ldy     ram_A7,x                ;4        
    lda     Lfa8d,y                 ;4        
    and     #$0e                    ;2        
    asl                             ;2        
    asl                             ;2   =  14
Lf28f
    sta     ram_E8                  ;3        
    lda     ram_BF,x                ;4        
    sta     ram_E7                  ;3        
    lda     ram_80                  ;3        
    and     #$0f                    ;2        
    lsr                             ;2        
    lsr                             ;2        
    bit     ram_E7                  ;3        
    bpl     Lf2a1                   ;2/3      
    ora     #$04                    ;2   =  26
Lf2a1
    ora     ram_E8                  ;3        
    tay                             ;2        
    lda     ram_E7                  ;3        
    and     #$3f                    ;2        
    bvs     Lf2b1                   ;2/3      
    clc                             ;2        
    adc     Lf256,y                 ;4        
    jmp     Lf2b7                   ;3   =  21
    
Lf2b1
    and     #$3f                    ;2        
    sec                             ;2        
    sbc     Lf256,y                 ;4   =   8
Lf2b7
    bmi     Lf2bd                   ;2/3      
    cmp     #$3c                    ;2        
    bcc     Lf2c5                   ;2/3 =   6
Lf2bd
    sec                             ;2        
    ldy     ram_A7,x                ;4        
    lda     #$00                    ;2        
    sta     ram_A7,x                ;4        
    rts                             ;6   =  18
    
Lf2c5
    sta     ram_E7                  ;3        
    lda     ram_BF,x                ;4        
    and     #$c0                    ;2        
    ora     ram_E7                  ;3        
    sta     ram_BF,x                ;4        
    rts                             ;6   =  22
    
Lf2d0
    lda     #$28                    ;2        
    jsr     Lf28f                   ;6        
    bcc     Lf2df                   ;2/3      
    lda     ram_BF,x                ;4        
    eor     #$40                    ;2        
    sta     ram_BF,x                ;4        
    sty     ram_A7,x                ;4   =  24
Lf2df
    rts                             ;6   =   6
    
Lf2e0
    .byte   $3a,$19,$1a,$1a,$1c,$1a,$1c     ; $f2e0 (D)
    .byte   $1a,$1c,$1c,$1c,$1c             ; $f2e7 (*)
    
Lf2ec
    lda     ram_91                  ;3        
    and     #$02                    ;2        
    bne     Lf34d                   ;2/3!     
    lda     ram_A0                  ;3        
    cmp     #$06                    ;2        
    bne     Lf31e                   ;2/3!     
    lda     ram_F7                  ;3        
    and     #$1f                    ;2        
    cmp     #$06                    ;2        
    bne     Lf31e                   ;2/3      
    lda     ram_91                  ;3        
    ora     #$02                    ;2        
    sta     ram_91                  ;3        
    lda     #$d8                    ;2        
    sta     ram_BA                  ;3        
    lda     #$ff                    ;2        
    sta     ram_B4                  ;3        
    lda     #$01                    ;2        
    sta     ram_B5                  ;3        
    sta     ram_B3                  ;3        
    sta     ram_B8                  ;3        
    lda     ram_82                  ;3        
    lsr                             ;2        
    bcs     Lf31d                   ;2/3      
    asl     ram_B8                  ;5   =  64
Lf31d
    rts                             ;6   =   6
    
Lf31e
    lda     ram_D7                  ;3        
    ora     #$0c                    ;2        
    sta     ram_D7                  ;3   =   8
Lf324
    tax                             ;2        
    jsr     Lfceb                   ;6        
    lda     ram_80                  ;3        
    and     #$07                    ;2        
    bne     Lf33c                   ;2/3      
    lda     ram_A7,x                ;4        
    and     #$3f                    ;2        
    tay                             ;2        
    lda     ram_A7,x                ;4        
    and     #$c0                    ;2        
    ora     Lfd2f,y                 ;4        
    sta     ram_A7,x                ;4   =  37
Lf33c
    lda     Lf3a5,x                 ;4        
    cmp     ram_D7                  ;3        
    bne     Lf324                   ;2/3      
    and     #$03                    ;2        
    sta     ram_D7                  ;3        
    rts                             ;6   =  20
    
Lf348
    .byte   $00                             ; $f348 (*)
    .byte   $02,$03,$04,$01                 ; $f349 (D)
    
Lf34d
    lda     ram_B8                  ;3        
    clc                             ;2        
    ldx     ram_B3                  ;3        
    beq     Lf3a9                   ;2/3      
    lda     Lf348,x                 ;4        
    sta     ram_B3                  ;3        
    lda     ram_A0                  ;3        
    and     #$07                    ;2        
    bne     Lf36b                   ;2/3      
    lda     ram_A0                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    tax                             ;2        
    lda     Lf2e0,x                 ;4        
    sta     ram_94                  ;3   =  44
Lf36b
    lda     #$30                    ;2        
    ldx     ram_B8                  ;3        
    bmi     Lf373                   ;2/3      
    lda     #$d0                    ;2   =   9
Lf373
    clc                             ;2        
    adc     ram_B9                  ;3        
    sta     ram_B9                  ;3        
    bcc     Lf39c                   ;2/3      
    lda     ram_B5                  ;3        
    cmp     #$14                    ;2        
    bcc     Lf386                   ;2/3      
    lda     ram_B8                  ;3        
    eor     #$ff                    ;2        
    sta     ram_B8                  ;3   =  25
Lf386
    lda     ram_B8                  ;3        
    bpl     Lf393                   ;2/3      
    lda     ram_B5                  ;3        
    bne     Lf3a9                   ;2/3      
    lda     ram_B4                  ;3        
    bpl     Lf3a3                   ;2/3      
    rts                             ;6   =  21 *
    
Lf393
    lda     ram_BA                  ;3        
    bpl     Lf39d                   ;2/3      
    clc                             ;2        
    adc     ram_B8                  ;3        
    sta     ram_BA                  ;3   =  13
Lf39c
    rts                             ;6   =   6
    
Lf39d
    lda     ram_B4                  ;3        
    cmp     #$0b                    ;2        
    beq     Lf3a9                   ;2/3 =   7
Lf3a3
    clc                             ;2        
    .byte   $65 ;adc                ;3-2 =   3
Lf3a5
    clv                             ;2        
    sta     ram_B4                  ;3        
    rts                             ;6   =  11
    
Lf3a9
    clc                             ;2        
    lda     ram_B5                  ;3        
    adc     ram_B8                  ;3        
    sta     ram_B5                  ;3        
    rts                             ;6   =  17
    
    .byte   $0f,$0c,$0d,$0e                 ; $f3b1 (D)
    
Lf3b5
    lda     ram_97                  ;3        
    beq     Lf3bc                   ;2/3      
    jmp     Lf424                   ;3   =   8
    
Lf3bc
    lda     ram_80                  ;3        
    and     #$03                    ;2        
    bne     Lf3d0                   ;2/3      
    jsr     Lf2ec                   ;6        
    ldx     ram_D6                  ;3        
    stx     ram_A6                  ;3        
    lda     ram_D8                  ;3        
    sta     ram_A5                  ;3        
    jmp     Lf403                   ;3   =  28
    
Lf3d0
    lda     #$03                    ;2        
    sta     ram_EB                  ;3        
    ldx     ram_A6                  ;3   =   8
Lf3d6
    jsr     Lfbde                   ;6        
    ldy     ram_A7,x                ;4        
    lda     Lfad7,y                 ;4        
    sta     ram_A7,x                ;4   =  18
Lf3e0
    lda     Lfa71,x                 ;4        
    tax                             ;2        
    lda     ram_A7,x                ;4        
    tay                             ;2        
    lda     #$04                    ;2        
    sta     ram_E7                  ;3        
    lda     Lfa8d,y                 ;4        
    lsr                             ;2        
    bcc     Lf3f3                   ;2/3      
    asl     ram_E7                  ;5   =  30
Lf3f3
    lda     ram_E7                  ;3        
    clc                             ;2        
    adc     ram_A5                  ;3        
    clc                             ;2        
    adc     #$04                    ;2        
    sta     ram_A5                  ;3        
    dec     ram_EB                  ;5        
    bpl     Lf3d6                   ;2/3!     
    stx     ram_A6                  ;3   =  25
Lf403
    jsr     Lf126                   ;6        
    lda     ram_80                  ;3        
    and     #$03                    ;2        
    sta     ram_E7                  ;3        
    bne     Lf419                   ;2/3      
    jsr     Lf98f                   ;6        
    jmp     Lf424                   ;3   =  25
    
Lf414
    .byte   $e7,$e7,$f3,$ff,$0b             ; $f414 (D)
    
Lf419
    ldx     #$03                    ;2   =   2
Lf41b
    lda     ram_BB,x                ;4        
    bmi     Lf421                   ;2/3      
    dec     ram_BB,x                ;6   =  12
Lf421
    dex                             ;2        
    bne     Lf41b                   ;2/3 =   4
Lf424
    ldx     #$b7                    ;2        
    stx     ram_F0                  ;3        
    inx                             ;2        
    stx     ram_F2                  ;3        
    lda     #$25                    ;2        
    sta     NUSIZ1                  ;3        
    lda     #$7b                    ;2        
    sta     ram_D9                  ;3        
    lda     ram_91                  ;3        
    and     #$02                    ;2        
    beq     Lf45c                   ;2/3      
    ldx     ram_B3                  ;3        
    lda     Lf414,x                 ;4        
    sta     ram_EF                  ;3        
    ldx     #$be                    ;2        
    stx     ram_F0                  ;3        
    lda     #$00                    ;2        
    sta     ram_F8                  ;3        
    lda     Lfa38                   ;4        
    sta     ram_FA                  ;3        
    lda     ram_B4                  ;3        
    sta     ram_F6                  ;3        
    ldx     #$e0                    ;2        
    stx     ram_D9                  ;3        
    lda     #$27                    ;2        
    sta     NUSIZ1                  ;3        
    jmp     Lf4a7                   ;3   =  73
    
Lf45c
    ldx     #$03                    ;2   =   2
Lf45e
    lda     ram_CB,x                ;4        
    lsr                             ;2        
    and     #$1f                    ;2        
    tay                             ;2        
    lda     Lfa2d,y                 ;4        
    sta     ram_FA,x                ;4        
    dex                             ;2        
    bpl     Lf45e                   ;2/3      
    lda     ram_91                  ;3        
    and     #$08                    ;2        
    beq     Lf496                   ;2/3      
    ldx     ram_D7                  ;3        
    stx     ram_F8                  ;3        
    lda     ram_B7,x                ;4        
    tay                             ;2        
    lda     Lf48e,y                 ;4        
    sta     ram_D9                  ;3        
    lda     Lf486,y                 ;4        
    sta     ram_F6                  ;3        
    jmp     Lf4a7                   ;3   =  58
    
Lf486
    .byte   $ff,$ff,$00,$01,$02,$03,$03,$03 ; $f486 (D)
Lf48e
    .byte   $7b,$7b,$7b,$7b,$7b,$7b,$34,$2e ; $f48e (D)
    
Lf496
    ldx     ram_D7                  ;3        
    stx     ram_F8                  ;3        
    lda     Lfa7d,x                 ;4        
    sta     ram_F8                  ;3        
    lda     #$ff                    ;2        
    sta     ram_F6                  ;3        
    lda     #$7b                    ;2        
    sta     ram_D9                  ;3   =  23
Lf4a7
    lda     ram_D6                  ;3        
    sta     ram_F7                  ;3        
    ldx     #$03                    ;2   =   8
Lf4ad
    lda     ram_BB,x                ;4        
    bpl     Lf4b4                   ;2/3      
    dex                             ;2        
    bne     Lf4ad                   ;2/3 =  10
Lf4b4
    stx     ram_F9                  ;3        
    stx     ram_E3                  ;3        
    lda     #$88                    ;2        
    sta     ram_DD                  ;3        
    lda     #$b1                    ;2        
    sta     ram_DE                  ;3        
    sta     WSYNC                   ;3   =  19
;---------------------------------------
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    inc     ram_E7                  ;5        
    inc     ram_E7                  ;5        
    inc     ram_E7                  ;5        
    ldx     ram_F8                  ;3        
    lda     ram_FA,x                ;4        
    asl                             ;2        
    sta     HMP1                    ;3        
    sta     RESP1                   ;3        
    inc     ram_E7                  ;5        
    inc     ram_E7                  ;5        
    inc     ram_E7                  ;5        
    bcc     Lf4de                   ;2/3      
    sta     RESP1                   ;3   =  56
Lf4de
    lda     ram_A0                  ;3        
    sta     ram_E4                  ;3        
    lda     #$00                    ;2        
    sta     ram_E5                  ;3        
    sta     ram_F4                  ;3        
    sta     ram_F3                  ;3        
    lda     ram_D8                  ;3        
    sta     ram_F5                  ;3        
    inc     ram_E7                  ;5        
    lda     ram_E6                  ;3        
    eor     #$e0                    ;2        
    ora     #$12                    ;2        
    sta     ram_E6                  ;3        
    ldx     #$b4                    ;2        
    stx     ram_EC                  ;3        
    inx                             ;2        
    stx     ram_EE                  ;3        
    lda     #$00                    ;2        
    sta     ram_EB                  ;3        
    sta     ram_ED                  ;3        
    lda     #$9a                    ;2        
    sta     ram_DB                  ;3        
    lda     #$b2                    ;2        
    sta     ram_DC                  ;3        
    lda     #$b3                    ;2        
    sta     ram_DA                  ;3        
    lda     #$88                    ;2        
    sta     ram_DD                  ;3        
    lda     #$b1                    ;2        
    sta     ram_DE                  ;3        
    sta     WSYNC                   ;3   =  84
;---------------------------------------
    jsr     Lf5bb                   ;6        
    jsr     Lf5bb                   ;6        
    inc     ram_E7                  ;5        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    sta     RESM0                   ;3        
    lda     ram_D3                  ;3        
    lda     ram_D3                  ;3        
    lda     ram_D3                  ;3        
    asl                             ;2        
    sta     HMM0                    ;3        
    bcc     Lf535                   ;2/3      
    sta     RESM0                   ;3   =  45
Lf535
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     ram_D3                  ;3        
    and     #$07                    ;2        
    asl                             ;2        
    sta     ram_E7                  ;3        
    beq     Lf567                   ;2/3      
    lda     ram_E6                  ;3        
    eor     #$01                    ;2        
    sta     ram_E6                  ;3        
    lsr                             ;2        
    bcs     Lf55f                   ;2/3      
    and     #$02                    ;2        
    beq     Lf557                   ;2/3      
    lda     ram_E6                  ;3        
    and     #$fb                    ;2        
    sta     ram_E6                  ;3        
    dec     ram_D3                  ;5   =  44
Lf557
    sec                             ;2        
    lda     ram_D4                  ;3        
    sbc     ram_E7                  ;3        
    jmp     Lf572                   ;3   =  11
    
Lf55f
    clc                             ;2        
    lda     ram_D4                  ;3        
    adc     ram_E7                  ;3        
    jmp     Lf572                   ;3   =  11
    
Lf567
    lda     ram_CF                  ;3        
    and     #$f8                    ;2        
    sta     ram_D3                  ;3        
    lda     ram_BB                  ;3        
    clc                             ;2        
    sbc     #$0b                    ;2   =  15
Lf572
    sta     ram_D4                  ;3        
    sta     WSYNC                   ;3   =   6
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     WSYNC                   ;3   =   6
;---------------------------------------
    sta     HMOVE                   ;3        
    jsr     Lf5bb                   ;6        
    jsr     Lf5bb                   ;6        
    jsr     Lf5bb                   ;6        
    sta     WSYNC                   ;3   =  24
;---------------------------------------
    sta     HMOVE                   ;3        
    jsr     Lf5bb                   ;6        
    jsr     Lf5bb                   ;6        
    jsr     Lf5bb                   ;6        
    lda     #$50                    ;2        
    sta     HMM0                    ;3        
    lda     #$00                    ;2        
    sta     HMP1                    ;3        
    sta     WSYNC                   ;3   =  34
;---------------------------------------
    sta     HMOVE                   ;3        
    jsr     Lf5bb                   ;6        
    jsr     Lf5bb                   ;6        
    jsr     Lf5bb                   ;6        
    lda     #$00                    ;2        
    sta     HMM0                    ;3        
    lda     #$47                    ;2        
    sta     ram_DF                  ;3        
    lda     ram_DE                  ;3        
    sta     ram_E0                  ;3        
    jmp     Lf09e                   ;3   =  40
    
Lf5b6
    lda     ram_80                  ;3        
    lsr                             ;2        
    bcs     Lf5bc                   ;2/3 =   7
Lf5bb
    rts                             ;6   =   6
    
Lf5bc
    lda     ram_91                  ;3        
    bmi     Lf5c7                   ;2/3      
    and     #$01                    ;2        
    tax                             ;2        
    lda     INPT4|$30,x             ;4        
    bmi     Lf620                   ;2/3!=  15
Lf5c7
    lda     ram_BE                  ;3        
    bpl     Lf5f4                   ;2/3      
    lda     ram_BC                  ;3        
    clc                             ;2        
    adc     #$0d                    ;2        
    cmp     ram_BB                  ;3        
    bcs     Lf5f4                   ;2/3      
    lda     ram_BD                  ;3        
    sta     ram_BE                  ;3        
    lda     ram_BC                  ;3        
    sta     ram_BD                  ;3        
    lda     ram_D1                  ;3        
    sta     ram_D2                  ;3        
    lda     ram_D0                  ;3        
    sta     ram_D1                  ;3        
    lda     ram_CF                  ;3        
    sta     ram_D0                  ;3        
    lda     ram_BB                  ;3        
    clc                             ;2        
    sbc     #$04                    ;2        
    sta     ram_BC                  ;3        
    ldy     #$01                    ;2        
    jsr     $b018                   ;6   =  65
Lf5f4
    lda     ram_91                  ;3        
    bmi     Lf5fc                   ;2/3      
    and     #$10                    ;2         *
    beq     Lf619                   ;2/3!=   9 *
Lf5fc
    lda     ram_D3                  ;3        
    and     #$07                    ;2        
    bne     Lf619                   ;2/3      
    lda     ram_CF                  ;3        
    ora     #$07                    ;2        
    sta     ram_D3                  ;3        
    lda     ram_BB                  ;3        
    clc                             ;2        
    sbc     #$0b                    ;2        
    sta     ram_D4                  ;3        
    lda     ram_E6                  ;3        
    and     #$fa                    ;2        
    sta     ram_E6                  ;3        
    lda     #$e0                    ;2        
    sta     ram_9D                  ;3   =  38
Lf619
    lda     ram_91                  ;3        
    and     #$ef                    ;2        
    sta     ram_91                  ;3        
    rts                             ;6   =  14
    
Lf620
    lda     ram_91                  ;3        
    ora     #$10                    ;2        
    sta     ram_91                  ;3        
    rts                             ;6   =  14
    
Lf627
    lda     ram_97                  ;3        
    beq     Lf6a4                   ;2/3      
    dec     ram_97                  ;5        
    bne     Lf6a4                   ;2/3      
    lda     ram_91                  ;3        
    and     #$20                    ;2        
    beq     Lf6a4                   ;2/3      
    lda     ram_91                  ;3         *
    eor     #$20                    ;2         *
    and     #$f5                    ;2         *
    sta     ram_91                  ;3         *
    lda     ram_91                  ;3         *
    bmi     Lf6a1                   ;2/3       *
    dec     ram_85                  ;5         *
    lda     ram_86                  ;3         *
    cmp     #$06                    ;2         *
    bcc     Lf64d                   ;2/3       *
    sbc     #$04                    ;2         *
    sta     ram_86                  ;3   =  51 *
Lf64d
    ldx     ram_87                  ;3         *
    lda     Lfa0d,x                 ;4         *
    cmp     ram_9E                  ;3         *
    bcs     Lf659                   ;2/3       *
    jsr     Lf938                   ;6   =  18 *
Lf659
    lda     game_variation          ;3         *
    and     #$01                    ;2         *
    beq     Lf679                   ;2/3       *
    ldx     #$05                    ;2         *
    lda     ram_8B                  ;3         *
    bmi     Lf679                   ;2/3       *
    lda     ram_91                  ;3         *
    eor     #$01                    ;2         *
    sta     ram_91                  ;3   =  22 *
Lf66b
    lda     ram_85,x                ;4         *
    ldy     ram_8B,x                ;4         *
    sty     ram_85,x                ;4         *
    sta     ram_8B,x                ;4         *
    dex                             ;2         *
    bpl     Lf66b                   ;2/3       *
    jmp     Lf8f5                   ;3   =  23 *
    
Lf679
    lda     ram_85                  ;3         *
    bmi     Lf680                   ;2/3       *
    jmp     Lf8f5                   ;3   =   8 *
    
Lf680 ; end of game here? 
    lda     ram_91                  ;3         *
    and     #$01                    ;2         *
    beq     Lf693                   ;2/3       *
    ldx     #$05                    ;2   =   9 *
Lf688
    lda     ram_85,x                ;4         *
    ldy     ram_8B,x                ;4         *
    sty     ram_85,x                ;4         *
    sta     ram_8B,x                ;4         *
    dex                             ;2         *
    bpl     Lf688                   ;2/3 =  20 *
Lf693
    lda     ram_87                  ;3         *
    sta     ram_A4                  ;3         *
    lda     #$00                    ;2         *
    sta     ram_81                  ;3         *
    lda     #$80                    ;2         *
  IF PLUSROM
    jmp     SendPlusROMScores
    nop
  ELSE
    sta     ram_80                  ;3         *
    sta     ram_91                  ;3   =  19 *
  ENDIF
Lf6a1
    jmp     Lf8fa                   ;3   =   3 *
    
Lf6a4
    bit     CXM0P|$30               ;3        
    bpl     Lf70b                   ;2/3!     
    lda     ram_D3                  ;3        
    and     #$07                    ;2        
    cmp     #$01                    ;2        
    bne     Lf70b                   ;2/3!     
    lda     ram_91                  ;3        
    and     #$02                    ;2        
    beq     Lf6f1                   ;2/3      
    lda     ram_D3                  ;3        
    and     #$f8                    ;2        
    cmp     Lfa3b                   ;4        
    beq     Lf6ce                   ;2/3      
    cmp     Lfa3c                   ;4        
    beq     Lf6ce                   ;2/3      
    cmp     Lfa3d                   ;4        
    beq     Lf6ce                   ;2/3      
    cmp     Lfa3e                   ;4        
    bne     Lf70b                   ;2/3!=  50
Lf6ce
    lda     ram_B5                  ;3        
    clc                             ;2        
    adc     #$05                    ;2        
    cmp     ram_D4                  ;3        
    bcs     Lf70b                   ;2/3!     
    adc     #$02                    ;2        
    cmp     ram_D4                  ;3        
    bcc     Lf70b                   ;2/3!     
    lda     ram_B8                  ;3        
    bpl     Lf6e5                   ;2/3      
    eor     #$ff                    ;2        
    sta     ram_B8                  ;3   =  29
Lf6e5
    lda     #$00                    ;2        
    sta     ram_B3                  ;3        
    ldy     #$0b                    ;2        
    jsr     Lf964                   ;6        
    jmp     Lf70b                   ;3   =  16
    
Lf6f1
    ldx     ram_E1                  ;3        
    lda     ram_B3,x                ;4        
    and     #$3f                    ;2        
    tay                             ;2        
    cmp     #$09                    ;2        
    bcc     Lf70b                   ;2/3!     
    lda     #$01                    ;2        
    sta     ram_B3,x                ;4        
    lda     Lfd14,y                 ;4        
    jsr     Lf964                   ;6        
    ldy     #$03                    ;2        
    jsr     $b018                   ;6   =  39
Lf70b
    bit     CXM1P|$30               ;3        
    bpl     Lf770                   ;2/3      
    ldx     ram_E2                  ;3        
    lda     ram_A7,x                ;4        
    cmp     #$44                    ;2        
    bcs     Lf770                   ;2/3      
    ldx     ram_E3                  ;3        
    bne     Lf73b                   ;2/3      
    lda     ram_97                  ;3        
    bne     Lf770                   ;2/3      
    lda     ram_91                  ;3        
    bmi     Lf770                   ;2/3      
    lda     #$64                    ;2         *
    ldy     ram_85                  ;3         *
    beq     Lf72b                   ;2/3       *
    lda     #$1e                    ;2   =  40 *
Lf72b
    sta     ram_97                  ;3         *
    lda     #$20                    ;2         *
    ora     ram_91                  ;3         *
    sta     ram_91                  ;3         *
    ldy     #$08                    ;2         *
    jsr     $b018                   ;6         *
    jmp     Lf770                   ;3   =  22 *
    
Lf73b
    ldy     ram_E2                  ;3        
    lda.wy  ram_A7,y                ;4        
    cmp     #$41                    ;2        
    bcs     Lf770                   ;2/3 =  11
Lf744
    cpx     #$03                    ;2        
    bcs     Lf754                   ;2/3      
    lda     ram_D0,x                ;4        
    sta     ram_CF,x                ;4        
    lda     ram_BC,x                ;4        
    sta     ram_BB,x                ;4        
    inx                             ;2        
    jmp     Lf744                   ;3   =  25
    
Lf754
    lda     #$ff                    ;2        
    sta     ram_BE                  ;3        
    ldx     ram_E2                  ;3        
    lda     ram_A7,x                ;4        
    tay                             ;2        
    cmp     #$31                    ;2        
    bcs     Lf770                   ;2/3      
    lda     #$44                    ;2        
    sta     ram_A7,x                ;4        
    jsr     Lf964                   ;6        
    lda     Lfb94,y                 ;4        
    ldy     #$02                    ;2        
    jsr     $b018                   ;6   =  42
Lf770
    jsr     Lf5b6                   ;6        
    lda     ram_80                  ;3        
    and     #$0f                    ;2        
    cmp     #$07                    ;2        
    bne     Lf77e                   ;2/3      
    jsr     Lf792                   ;6   =  21
Lf77e
    jmp     Lf03e                   ;3   =   3
    
Lf781
    ldy     ram_D6                  ;3        
    ldx     ram_A7,y                ;4        
    lda     #$04                    ;2        
    sta     ram_E7                  ;3        
    lda     Lfa8d,x                 ;4        
    lsr                             ;2        
    bcc     Lf791                   ;2/3      
    asl     ram_E7                  ;5   =  25
Lf791
    rts                             ;6   =   6
    
Lf792
    lda     ram_97                  ;3        
    beq     Lf797                   ;2/3 =   5
Lf796
    rts                             ;6   =   6
    
Lf797
    lda     ram_82                  ;3        
    and     #$02                    ;2        
    beq     Lf7c2                   ;2/3      
    dec     ram_D8                  ;5         *
    bpl     Lf7c1                   ;2/3       *
    ldy     ram_D6                  ;3         *
    lda.wy  ram_A7,y                ;4         *
    cmp     #$02                    ;2         *
    bne     Lf7b0                   ;2/3       *
    lda     ram_82                  ;3         *
    and     #$fd                    ;2         *
    sta     ram_82                  ;3   =  33 *
Lf7b0
    lda     #$00                    ;2         *
    sta.wy  ram_A7,y                ;5         *
    lda     Lfa71,y                 ;4         *
    sta     ram_D6                  ;3         *
    jsr     Lf781                   ;6         *
    lda     ram_E7                  ;3         *
    sta     ram_D8                  ;3   =  26 *
Lf7c1
    rts                             ;6   =   6 *
    
Lf7c2
    jsr     Lf781                   ;6        
    lda     ram_E7                  ;3        
    inc     ram_D8                  ;5        
    inc     ram_D8                  ;5        
    cmp     ram_D8                  ;3        
    bcs     Lf796                   ;2/3      
    lda     #$ff                    ;2        
    sta     ram_D8                  ;3        
    ldx     Lfa81,y                 ;4        
    stx     ram_D6                  ;3        
    lda     ram_84                  ;3        
    sta     ram_BF,x                ;4        
    lda     ram_D6                  ;3        
    sta     ram_E9                  ;3        
    lda     game_variation          ;3        
    and     #$04                    ;2        
    beq     Lf7ef                   ;2/3      
    lsr     ram_E9                  ;5         *
    bcc     Lf7ef                   ;2/3       *
    lda     #$01                    ;2         *
    sta     ram_A7,x                ;4         *
    rts                             ;6   =  75 *
    
Lf7ef
    ldx     ram_95                  ;3        
    lda     Lfb67,x                 ;4        
    sta     ram_E7                  ;3        
    and     #$40                    ;2        
    beq     Lf826                   ;2/3!     
    lda     ram_E9                  ;3        
    lsr                             ;2        
    bcs     Lf814                   ;2/3!     
    lsr                             ;2        
    bcs     Lf808                   ;2/3      
    lda     ram_E7                  ;3        
    and     #$20                    ;2        
    bne     Lf82c                   ;2/3 =  32
Lf808
    lda     ram_D6                  ;3        
    tax                             ;2        
    lda     ram_84                  ;3        
    and     #$07                    ;2        
    adc     #$31                    ;2        
    sta     ram_A7,x                ;4        
    rts                             ;6   =  22
    
Lf814
    lda     #$00                    ;2        
    ldx     ram_D6                  ;3        
    sta     ram_A7,x                ;4        
    rts                             ;6   =  15
    
Lf81b
    and     #$7f                    ;2         *
    sta     ram_94                  ;3         *
    lda     #$02                    ;2         *
    ldx     ram_D6                  ;3         *
    sta     ram_A7,x                ;4         *
    rts                             ;6   =  20 *
    
Lf826
    lda     ram_E9                  ;3        
    and     #$03                    ;2        
    beq     Lf814                   ;2/3 =   7
Lf82c
    lda     ram_94                  ;3        
    beq     Lf814                   ;2/3      
    bmi     Lf81b                   ;2/3      
    and     #$07                    ;2        
    sta     ram_E7                  ;3        
    lda     ram_94                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    and     #$0e                    ;2        
    tay                             ;2        
    dec     ram_94                  ;5        
    lda     ram_E7                  ;3        
    bne     Lf845                   ;2/3      
    sta     ram_94                  ;3   =  36
Lf845
    lda     Lf859,y                 ;4        
    ldx     ram_D6                  ;3        
    sta     ram_A7,x                ;4        
    lda     Lf869,y                 ;4        
    sta     ram_DF                  ;3        
    lda     Lf86a,y                 ;4        
    sta     ram_E0                  ;3        
    jmp.ind (ram_DF)                ;5   =  30
    
Lf859
    .byte   $22                             ; $f859 (D)
Lf85a
    .byte   $22                             ; $f85a (*)
    .byte   $1a,$12,$06                     ; $f85b (D)
    .byte   $06                             ; $f85e (*)
    .byte   $2f                             ; $f85f (D)
    .byte   $2f                             ; $f860 (*)
    .byte   $0e,$0a,$03                     ; $f861 (D)
    .byte   $03,$00,$00                     ; $f864 (*)
    .byte   $31                             ; $f867 (D)
    .byte   $31                             ; $f868 (*)
Lf869
    .byte   $ac                             ; $f869 (D)
Lf86a
    .byte   $f8,$79,$f8,$c1,$f8,$ac,$f8,$79 ; $f86a (D)
    .byte   $f8,$ac,$f8                     ; $f872 (D)
    .byte   $c0,$f8                         ; $f875 (*)
    .byte   $c0,$f8                         ; $f877 (D)
    
Lf879
    lda     #$00                    ;2        
    sta     ram_E7                  ;3        
    lda     ram_BF,x                ;4        
    ora     #$40                    ;2        
    sta     ram_E8                  ;3        
    and     #$3f                    ;2        
    sta     ram_E9                  ;3        
    and     #$20                    ;2        
    beq     Lf898                   ;2/3      
    lda     Lf85a,y                 ;4        
    sta     ram_A7,x                ;4        
    lda     ram_E8                  ;3        
    eor     #$40                    ;2        
    sta     ram_E8                  ;3        
    dec     ram_E7                  ;5   =  44
Lf898
    lda     ram_E7                  ;3        
    eor     #$10                    ;2        
    clc                             ;2        
    adc     ram_E9                  ;3        
    and     #$3f                    ;2        
    sta     ram_E9                  ;3        
    lda     ram_E8                  ;3        
    and     #$c0                    ;2        
    ora     ram_E9                  ;3        
    sta     ram_BF,x                ;4        
    rts                             ;6   =  33
    
    lda     ram_BF,x                ;4        
    and     #$bf                    ;2        
    sta     ram_E7                  ;3        
    and     #$20                    ;2        
    beq     Lf8bc                   ;2/3      
    lda     ram_E7                  ;3        
    ora     #$40                    ;2        
    sta     ram_E7                  ;3   =  21
Lf8bc
    lda     ram_E7                  ;3        
    sta     ram_BF,x                ;4        
    rts                             ;6   =  13
    
    lda     ram_94                  ;3        
    and     #$03                    ;2        
    clc                             ;2        
    adc     ram_A7,x                ;4        
    sta     ram_A7,x                ;4        
    lda     ram_BF,x                ;4        
    and     #$3f                    ;2        
    sta     ram_BF,x                ;4        
    rts                             ;6   =  31
    
Lf8d1
    lda     #$00                    ;2        
    sta     ram_87                  ;3        
    sta     ram_8D                  ;3   =   8
Lf8d7
    lda     #$00                    ;2        
    sta     ram_91                  ;3        
    sta     ram_94                  ;3        
    sta     ram_81                  ;3        
    sta     ram_86                  ;3        
    sta     ram_8C                  ;3        
    sta     score_bcd_hi            ;3        
    sta     score_bcd_mid           ;3        
    sta     score_bcd_lo            ;3        
    sta     ram_8E                  ;3        
    sta     ram_8F                  ;3        
    sta     ram_90                  ;3        
    lda     #$04                    ;2        
    sta     ram_85                  ;3        
    sta     ram_8B                  ;3   =  43
Lf8f5
    ldy     #$09                    ;2        
    jsr     $b018                   ;6   =   8
Lf8fa
    lda     #$00                    ;2        
    sta     ram_E6                  ;3        
    ldx     #$0f                    ;2        
    lda     #$00                    ;2   =   9
Lf902
    sta     ram_A7,x                ;4        
    dex                             ;2        
    bpl     Lf902                   ;2/3      
    lda     ram_82                  ;3        
    and     #$fd                    ;2        
    sta     ram_82                  ;3        
    lda     #$00                    ;2        
    sta     ram_D7                  ;3        
    sta     ram_D6                  ;3        
    sta     ram_A0                  ;3        
    sta     ram_F8                  ;3        
    sta     ram_94                  ;3        
    lda     #$01                    ;2        
    sta     ram_9E                  ;3        
    lda     ram_80                  ;3        
    ora     #$07                    ;2        
    sta     ram_80                  ;3        
    lda     #$0f                    ;2        
    sta     ram_D5                  ;3        
    lda     Lfa3c                   ;4        
    sta     ram_D3                  ;3        
    sta     ram_CF                  ;3        
    lda     #$1f                    ;2        
    sta     ram_BB                  ;3        
    jsr     Lf940                   ;6        
    jmp     Lf03e                   ;3   =  75
    
Lf938
    inc     ram_87                  ;5        
    lda     ram_87                  ;3        
    and     #$0f                    ;2        
    sta     ram_87                  ;3   =  13
Lf940
    ldy     ram_87                  ;3        
    lda     Lf9fd,y                 ;4        
    sta     ram_96                  ;3        
    lda     Lf9ed,y                 ;4        
    sta     ram_95                  ;3        
    rts                             ;6   =  23
    
Lf94d
    .byte   $10,$30,$50,$00,$50,$00,$00,$00 ; $f94d (*)
    .byte   $00,$00,$00                     ; $f955 (*)
Lf958
    .byte   $00,$00,$00,$01,$01,$02,$03,$07 ; $f958 (*)
    .byte   $08,$0a,$14,$4b                 ; $f960 (*)
    
Lf964
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    tay                             ;2        
    lda     ram_91                  ;3        
    bmi     Lf98e                   ;2/3      
    sed                             ;2         *
    clc                             ;2         *
    lda     score_bcd_lo            ;3         *
    adc     Lf94d,y                 ;4         *
    sta     score_bcd_lo            ;3         *
    lda     score_bcd_mid           ;3         *
    adc     Lf958,y                 ;4         *
    sta     score_bcd_mid           ;3         *
    bcc     Lf987                   ;2/3       *
    ldy     #$07                    ;2         *
    jsr     $b018                   ;6         *
    inc     ram_85                  ;5         *
    sec                             ;2   =  56 *
Lf987
    lda     score_bcd_hi            ;3         *
    adc     #$00                    ;2         *
    sta     score_bcd_hi            ;3         *
    cld                             ;2   =  10 *
Lf98e
    rts                             ;6   =   6
    
Lf98f
    lda     ram_91                  ;3        
    bpl     Lf9a0                   ;2/3      
    lda     ram_80                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    ldx     ram_80                  ;3        
    bmi     Lf99d                   ;2/3      
    eor     #$ff                    ;2   =  19
Lf99d
    jmp     Lf9e0                   ;3   =   3
    
Lf9a0
    lsr                             ;2        
    lda     SWCHA                   ;4        
    bcc     Lf9aa                   ;2/3      
    asl                             ;2         *
    asl                             ;2         *
    asl                             ;2         *
    asl                             ;2   =  16 *
Lf9aa
    sta     ram_E7                  ;3        
    asl                             ;2        
    asl                             ;2        
    bmi     Lf9bb                   ;2/3      
    lda     ram_BB                  ;3         *
    cmp     #$26                    ;2         *
    bcs     Lf9c6                   ;2/3       *
    inc     ram_BB                  ;5         *
    jmp     Lf9c6                   ;3   =  24 *
    
Lf9bb
    asl                             ;2        
    bmi     Lf9c6                   ;2/3      
    lda     ram_BB                  ;3         *
    cmp     #$0e                    ;2         *
    bcc     Lf9c6                   ;2/3       *
    dec     ram_BB                  ;5   =  16 *
Lf9c6
    lda     ram_E7                  ;3        
    bmi     Lf9d5                   ;2/3      
    lda     ram_D5                  ;3         *
    cmp     #$1f                    ;2         *
    beq     Lf9de                   ;2/3       *
    inc     ram_D5                  ;5         *
    jmp     Lf9de                   ;3   =  20 *
    
Lf9d5
    asl                             ;2        
    bmi     Lf9de                   ;2/3      
    lda     ram_D5                  ;3         *
    beq     Lf9de                   ;2/3       *
    dec     ram_D5                  ;5   =  14 *
Lf9de
    lda     ram_D5                  ;3   =   3
Lf9e0
    and     #$1f                    ;2        
    tax                             ;2        
    lda     ram_CF                  ;3        
    and     #$07                    ;2        
    ora     Lfa2d,x                 ;4        
    sta     ram_CF                  ;3        
    rts                             ;6   =  22
    
Lf9ed
    .byte   $19,$19,$19,$00,$1a,$1d,$00,$1d ; $f9ed (D)
    .byte   $05,$1f,$1f,$0a,$23,$0e,$1f,$13 ; $f9f5 (D)
Lf9fd
    .byte   $02,$3c,$4e,$56,$68,$56,$56,$26 ; $f9fd (D)
    .byte   $14,$56,$68,$68,$68,$68,$56,$68 ; $fa05 (D)
Lfa0d
    .byte   $10,$0f,$0e,$14,$11,$0e,$11,$12 ; $fa0d (*)
    .byte   $11,$13,$14,$13,$0d,$0d,$14,$10 ; $fa15 (*)
Lfa1d
    .byte   $16,$16,$14,$1b,$17,$14,$18,$18 ; $fa1d (D)
    .byte   $17,$16,$1d,$15,$14,$10,$1c,$14 ; $fa25 (D)
Lfa2d
    .byte   $38,$30,$28,$20,$18,$10,$08,$00 ; $fa2d (D)
    .byte   $78,$70,$68                     ; $fa35 (D)
Lfa38
    .byte   $60,$58,$50                     ; $fa38 (D)
Lfa3b
    .byte   $48                             ; $fa3b (D)
Lfa3c
    .byte   $b8                             ; $fa3c (D)
Lfa3d
    .byte   $b0                             ; $fa3d (D)
Lfa3e
    .byte   $a8,$a0,$98,$90,$88,$80,$f8,$f0 ; $fa3e (D)
    .byte   $e8,$e0,$d8,$d0,$c8,$c8,$c8     ; $fa46 (D)
Lfa4d
    .byte   $07,$06,$05,$04,$03,$02,$01,$00 ; $fa4d (D)
    .byte   $0e                             ; $fa55 (*)
    .byte   $0e,$0d,$0c,$0b,$0a,$09,$08,$16 ; $fa56 (D)
    .byte   $15,$14,$13,$12,$11,$10,$0f     ; $fa5e (D)
    .byte   $1d                             ; $fa65 (*)
    .byte   $1d,$1c,$1b,$1a,$19,$18,$17     ; $fa66 (D)
    .byte   $03,$00,$01,$02                 ; $fa6d (*)
Lfa71
    .byte   $0b,$00,$01,$02,$03,$04,$05,$06 ; $fa71 (D)
    .byte   $07,$08,$09,$0a                 ; $fa79 (D)
Lfa7d
    .byte   $01,$02,$03,$00                 ; $fa7d (D)
Lfa81
    .byte   $01,$02,$03,$04,$05,$06,$07,$08 ; $fa81 (D)
    .byte   $09,$0a,$0b,$00                 ; $fa89 (D)
Lfa8d
    .byte   $00                             ; $fa8d (D)
    .byte   $00,$00                         ; $fa8e (*)
    .byte   $02,$02,$02,$08,$08,$08,$08,$02 ; $fa90 (D)
    .byte   $00,$00,$04,$02,$00,$00,$04,$04 ; $fa98 (D)
    .byte   $00,$00,$00,$00,$00,$00,$06,$04 ; $faa0 (D)
    .byte   $00,$00,$00,$00,$00,$00,$06,$02 ; $faa8 (D)
    .byte   $02,$02,$02,$02,$02,$02,$02,$02 ; $fab0 (D)
    .byte   $02,$02,$02,$02,$00,$02,$01,$01 ; $fab8 (D)
    .byte   $01,$01,$01,$01,$01,$01,$01,$01 ; $fac0 (D)
    .byte   $01,$01,$01,$01,$01,$01,$00,$00 ; $fac8 (D)
    .byte   $00,$00,$00,$00,$00,$00,$00     ; $fad0 (D)
Lfad7
    .byte   $00                             ; $fad7 (D)
    .byte   $01,$02                         ; $fad8 (*)
    .byte   $04,$05,$03,$07,$08,$09,$06,$0a ; $fada (D)
    .byte   $0c,$0d,$0d,$0e,$10,$11,$11,$12 ; $fae2 (D)
    .byte   $14,$15,$16,$17,$18,$19,$19,$1a ; $faea (D)
    .byte   $1c,$1d,$1e,$1f,$20,$21,$21,$22 ; $faf2 (D)
    .byte   $24,$25,$26,$27,$28,$29,$2a,$2b ; $fafa (D)
    .byte   $2c,$2d,$2e,$23,$2f             ; $fb02 (D)
    .byte   $30                             ; $fb07 (*)
    .byte   $32,$33,$34,$35,$36,$37,$38,$39 ; $fb08 (D)
    .byte   $3a,$3b,$3c,$3d,$3e,$3f,$40,$31 ; $fb10 (D)
    .byte   $41,$41,$41,$45,$46,$47,$48,$49 ; $fb18 (D)
    .byte   $00                             ; $fb20 (D)
    .byte   $0e,$0c,$0a,$08,$06,$04,$02,$00 ; $fb21 (*)
    .byte   $00,$1c,$1a,$18,$16,$14,$12,$10 ; $fb29 (*)
    .byte   $2c,$2a,$28,$26,$24,$22,$20,$1e ; $fb31 (*)
    .byte   $00,$3a,$38,$36,$34,$32,$30,$2e ; $fb39 (*)
Lfb41
    .byte   $02,$03,$02,$03,$12,$04,$03,$12 ; $fb41 (D)
    .byte   $23,$06,$13,$2d,$0a,$23,$23,$2a ; $fb49 (D)
    .byte   $16,$0a,$1c,$14,$14,$2a,$23,$14 ; $fb51 (D)
    .byte   $2d,$0a,$23,$1c,$0e,$1c,$1c,$12 ; $fb59 (D)
    .byte   $16,$17,$1c,$14                 ; $fb61 (D)
    .byte   $2d,$03                         ; $fb65 (*)
Lfb67
    .byte   $00,$42,$42,$42                 ; $fb67 (D)
    .byte   $00                             ; $fb6b (*)
    .byte   $00,$43,$00,$44,$00,$00,$42     ; $fb6c (D)
    .byte   $62,$00                         ; $fb73 (*)
    .byte   $00                             ; $fb75 (D)
    .byte   $42,$44,$44,$00                 ; $fb76 (*)
    .byte   $00,$62                         ; $fb7a (D)
    .byte   $62,$62,$62,$00                 ; $fb7c (*)
    .byte   $03,$00,$01                     ; $fb80 (D)
    .byte   $02                             ; $fb83 (*)
    .byte   $03,$06,$02,$03,$04             ; $fb84 (D)
    .byte   $05                             ; $fb89 (*)
    .byte   $02,$03                         ; $fb8a (D)
    .byte   $04,$05,$06,$07,$04,$05,$06,$07 ; $fb8c (*)
Lfb94
    .byte   $02                             ; $fb94 (D)
    .byte   $00,$04                         ; $fb95 (*)
    .byte   $36,$36,$36,$38,$38,$38,$38,$65 ; $fb97 (D)
    .byte   $60,$60,$61,$65,$60,$60,$61,$75 ; $fb9f (D)
    .byte   $70,$70,$70,$70,$70,$70,$71,$75 ; $fba7 (D)
    .byte   $70,$70,$70,$70,$70,$70,$71,$15 ; $fbaf (D)
    .byte   $11,$11,$11,$11,$11,$11,$11,$11 ; $fbb7 (D)
    .byte   $11,$11,$11,$11,$03,$59,$00,$00 ; $fbbf (D)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fbc7 (D)
    .byte   $00,$00,$00,$00,$00,$00,$01,$07 ; $fbcf (D)
    .byte   $00,$00,$00,$00,$00,$00,$00     ; $fbd7 (D)
    
Lfbde
    cpx     ram_D6                  ;3        
    bne     Lfbe3                   ;2/3      
    rts                             ;6   =  11
    
Lfbe3
    lda     ram_A7,x                ;4        
    tay                             ;2        
    lda     Lfb94,y                 ;4        
    and     #$0f                    ;2        
    asl                             ;2        
    tay                             ;2        
    lda     Lfc03,y                 ;4        
    sta     ram_DF                  ;3        
    lda     Lfc04,y                 ;4        
    sta     ram_E0                  ;3        
    lda     #$00                    ;2        
    sta     ram_E8                  ;3        
    lda     ram_A5                  ;3        
    lsr                             ;2        
    sta     ram_E7                  ;3        
    jmp.ind (ram_DF)                ;5   =  48
    
Lfc03
    .byte   $59                             ; $fc03 (D)
Lfc04
    .byte   $fc,$1b,$fc,$43,$fc,$c0,$fc     ; $fc04 (D)
    .byte   $a0,$fc                         ; $fc0b (*)
    .byte   $ab,$fc,$17,$fc,$d1,$fc,$1e,$fc ; $fc0d (D)
    .byte   $d4,$fc                         ; $fc15 (D)
    
Lfc17
    and     #$0f                    ;2        
    beq     Lfc30                   ;2/3 =   4
Lfc1b
    jmp     Lf286                   ;3   =   3
    
    lda     #$80                    ;2        
    sta     ram_E8                  ;3        
    lda     ram_BF,x                ;4        
    bmi     Lfc1b                   ;2/3      
    ldy     ram_D4                  ;3        
    cpy     ram_E7                  ;3        
    bcc     Lfc1b                   ;2/3      
    ora     ram_E8                  ;3        
    sta     ram_BF,x                ;4   =  26
Lfc30
    jmp     Lf21e                   ;3   =   3
    
Lfc33
    .byte   $10,$02,$20,$02,$40,$20,$60,$20 ; $fc33 (D)
    .byte   $80,$40                         ; $fc3b (D)
    .byte   $a0,$80                         ; $fc3d (*)
    .byte   $c0,$80                         ; $fc3f (D)
    .byte   $f0,$a0                         ; $fc41 (*)
    
Lfc43
    sta     ram_E7                  ;3        
    lda     ram_91                  ;3        
    and     #$02                    ;2        
    bne     Lfc59                   ;2/3      
    dec     ram_E7                  ;5        
    ldy     #$03                    ;2   =  17
Lfc4f
    lda.wy  ram_B7,y                ;4        
    cmp     ram_E7                  ;3        
    beq     Lfc5a                   ;2/3      
    dey                             ;2        
    bpl     Lfc4f                   ;2/3 =  13
Lfc59
    rts                             ;6   =   6
    
Lfc5a
    lda.wy  ram_B7,y                ;4        
    bmi     Lfc59                   ;2/3      
    lda.wy  ram_B3,y                ;4        
    asl                             ;2        
    sty     ram_E8                  ;3        
    ldy     ram_95                  ;3        
    lda     Lfb67,y                 ;4        
    rol                             ;2        
    and     #$0f                    ;2        
    tay                             ;2        
    lda     Lfc33,y                 ;4        
    sta     ram_E7                  ;3        
    lda     game_variation          ;3        
    and     #$02                    ;2        
    beq     Lfc7b                   ;2/3      
    lsr     ram_E7                  ;5   =  47
Lfc7b
    lda     ram_E7                  ;3        
    cmp     ram_84                  ;3        
    bcc     Lfc59                   ;2/3      
    ldy     ram_E8                  ;3        
    lda.wy  ram_B3,y                ;4        
    and     #$3f                    ;2        
    cmp     #$09                    ;2        
    bcc     Lfc59                   ;2/3      
    lda     #$43                    ;2        
    sta     ram_A7,x                ;4        
    lda.wy  ram_CB,y                ;4        
    clc                             ;2        
    adc     #$01                    ;2        
    sta     ram_BF,x                ;4        
    jsr     Lf21e                   ;6        
    pla                             ;4        
    pla                             ;4        
    jmp     Lf3e0                   ;3   =  56
    
    .byte   $c9,$1e,$90,$06,$a5,$82,$09,$02 ; $fca0 (*)
    .byte   $85,$82,$60                     ; $fca8 (*)
    
Lfcab
    lsr                             ;2        
    cmp     #$06                    ;2        
    bne     Lfcbd                   ;2/3      
    lda     ram_BF,x                ;4        
    eor     #$40                    ;2        
    sta     ram_BF,x                ;4   =  16
Lfcb6
    inc     ram_A7,x                ;6        
    pla                             ;4        
    pla                             ;4        
    jmp     Lf3e0                   ;3   =  17
    
Lfcbd
    jmp     Lf286                   ;3   =   3
    
    lda     ram_84                  ;3        
    eor     ram_80                  ;3        
    cmp     #$a0                    ;2        
    bcs     Lfcc9                   ;2/3      
    rts                             ;6   =  16
    
Lfcc9
    ldy     #$05                    ;2        
    jsr     $b018                   ;6        
    jmp     Lfcb6                   ;3   =  11
    
    jmp     Lf21e                   ;3   =   3
    
    lda     ram_84                  ;3        
    cmp     #$c0                    ;2        
    bcc     Lfce3                   ;2/3      
    lda     #$42                    ;2        
    sta     ram_A7,x                ;4        
    ldy     #$04                    ;2        
    jsr     $b018                   ;6   =  21
Lfce3
    jsr     Lf286                   ;6        
    pla                             ;4        
    pla                             ;4        
    jmp     Lf3e0                   ;3   =  17
    
Lfceb
    lda     ram_A7,x                ;4        
    and     #$3f                    ;2        
    tay                             ;2        
    lda     Lfd14,y                 ;4        
    and     #$0f                    ;2        
    asl                             ;2        
    tay                             ;2        
    lda     Lfd08,y                 ;4        
    sta     ram_DF                  ;3        
    lda     Lfd09,y                 ;4        
    sta     ram_E0                  ;3        
    lda     ram_80                  ;3        
    and     #$04                    ;2        
    jmp.ind (ram_DF)                ;5   =  42
    
Lfd08
    .byte   $68                             ; $fd08 (D)
Lfd09
    .byte   $fd,$58,$fd,$4a,$fd,$69,$fd,$5b ; $fd09 (D)
    .byte   $fd                             ; $fd11 (D)
    .byte   $68,$fd                         ; $fd12 (*)
Lfd14
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $fd14 (D)
    .byte   $93,$90,$90,$90,$90,$91,$91,$92 ; $fd1c (D)
    .byte   $92,$94,$94                     ; $fd24 (D)
    .byte   $92,$92,$30                     ; $fd27 (*)
    .byte   $60,$30,$90,$90,$90             ; $fd2a (D)
Lfd2f
    .byte   $00,$02,$03,$04,$05,$06,$07,$07 ; $fd2f (D)
    .byte   $08,$0a,$0b,$0c,$09,$0e,$0d,$10 ; $fd37 (D)
    .byte   $0f,$12,$11                     ; $fd3f (D)
    .byte   $14,$13,$15                     ; $fd42 (*)
    .byte   $16,$17,$19,$1a,$18             ; $fd45 (D)
    
Lfd4a
    beq     Lfd58                   ;2/3      
    lda     ram_AB,x                ;4        
    and     #$03                    ;2        
    bne     Lfd58                   ;2/3      
    lda     #$80                    ;2   =  12
Lfd54
    eor     ram_BF,x                ;4        
    sta     ram_BF,x                ;4   =   8
Lfd58
    jmp     Lf2d0                   ;3   =   3
    
    beq     Lfd58                   ;2/3      
    lda     ram_AB,x                ;4        
    and     #$03                    ;2        
    bne     Lfd58                   ;2/3      
    lda     #$40                    ;2        
    jmp     Lfd54                   ;3   =  15
    
    rts                             ;6   =   6
    
    lda     ram_AB,x                ;4        
    clc                             ;2        
    adc     #$02                    ;2        
    lsr                             ;2        
    lsr                             ;2        
    sta     ram_E7                  ;3        
    lda     ram_D4                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    cmp     ram_E7                  ;3        
    bne     Lfd92                   ;2/3      
    lda     ram_D3                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    tay                             ;2        
    lda     Lfa4d,y                 ;4        
    lsr                             ;2        
    lsr                             ;2        
    sta     ram_E7                  ;3        
    lda     ram_BF,x                ;4        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    cmp     ram_E7                  ;3        
    bne     Lfd92                   ;2/3      
    inc     ram_A7,x                ;6   =  70
Lfd92
    rts                             ;6   =   6
    
Lfd93
    lda     INTIM                   ;4        
    bne     Lfd93                   ;2/3      
    sta     WSYNC                   ;3   =   9
;---------------------------------------
    ldx     #BLACK|$0               ;2        
    stx     COLUBK                  ;3        
    stx     COLUPF                  ;3        
    dex                             ;2        
    txs                             ;2        
    lda     #$28                    ;2        
    sta     TIM64T                  ;4        
    jsr     Lfe8b                   ;6        
    lda     #BEIGE|$8               ;2        
    sta     COLUP0                  ;3        
    sta     COLUP1                  ;3        
    lda     ram_91                  ;3        
    bmi     Lfe1b                   ;2/3!     
    lda     #$02                    ;2        
    sta     ram_E7                  ;3        
    lda     ram_91                  ;3   =  45
Lfdba
    ldx     #BEIGE|$6               ;2        
    lsr                             ;2        
    bcc     Lfdc1                   ;2/3 =   6
Lfdbf
    ldx     #$8c                    ;2   =   2 *
Lfdc1
    stx     COLUP0                  ;3        
    stx     COLUP1                  ;3        
    ldy     #$0b                    ;2   =   8
Lfdc7
    ldx     ram_E7                  ;3        
    lda     #$ff                    ;2        
    sta.wy  ram_EB,y                ;5        
    dey                             ;2        
    lda     score_bcd_hi,x          ;4        
    and     #$0f                    ;2        
    tax                             ;2        
    lda     Lff6a,x                 ;4        
    sta.wy  ram_EB,y                ;5        
    dey                             ;2        
    lda     #$ff                    ;2        
    sta.wy  ram_EB,y                ;5        
    dey                             ;2        
    ldx     ram_E7                  ;3        
    lda     score_bcd_hi,x          ;4        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    tax                             ;2        
    lda     Lff6a,x                 ;4        
    sta.wy  ram_EB,y                ;5        
    dec     ram_E7                  ;5        
    dey                             ;2        
    bpl     Lfdc7                   ;2/3      
    lda     ram_85,x                ;4        
    sta     ram_EA                  ;3        
    lda     #$06                    ;2        
    jsr     Lfeb5                   ;6        
    ldy     #$0a                    ;2        
    ldx     #$05                    ;2   =  94
Lfe02
    lda     Lff74                   ;4        
    cpx     ram_EA                  ;3        
    bpl     Lfe0b                   ;2/3      
    lda     #$21                    ;2   =  11
Lfe0b
    sta.wy  ram_EB,y                ;5        
    dex                             ;2        
    dey                             ;2        
    dey                             ;2        
    bpl     Lfe02                   ;2/3      
    lda     #$02                    ;2        
    jsr     Lfeb5                   ;6        
    jmp     Lfe88                   ;3   =  24
    
Lfe1b
    lda     ram_92                  ;3        
    bne     Lfe4c                   ;2/3      
    lda     ram_81                  ;3        
    and     #$01                    ;2        
    beq     Lfe62                   ;2/3      
    lda     score_bcd_hi            ;3        
    ora     score_bcd_mid           ;3        
    ora     score_bcd_lo            ;3        
    beq     Lfe38                   ;2/3      
    lda     ram_80                  ;3         *
    bmi     Lfe38                   ;2/3       *
    lda     #$02                    ;2         *
    sta     ram_E7                  ;3         *
    jmp     Lfdba                   ;3   =  36 *
    
Lfe38
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    ldx     #$0b                    ;2   =   2
Lfe3c
    lda     Lff0d,x                 ;4        
    sta     ram_EB,x                ;4        
    dex                             ;2        
    bpl     Lfe3c                   ;2/3 =  12
Lfe44
    lda     #$06                    ;2        
    jsr     Lfeb5                   ;6        
    jmp     Lfe88                   ;3   =  11
    
Lfe4c
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    ldx     #$0b                    ;2   =   2
Lfe50
    lda     Lff75,x                 ;4        
    sta     ram_EB,x                ;4        
    dex                             ;2        
    bpl     Lfe50                   ;2/3      
    ldx     game_variation          ;3        
    lda     Lff6b,x                 ;4        
    sta     ram_F5                  ;3        
    jmp     Lfe44                   ;3   =  25
    
Lfe62
    lda     ram_80                  ;3        
    bmi     Lfe77                   ;2/3      
    lda     ram_8E                  ;3        
    ora     ram_8F                  ;3        
    ora     ram_90                  ;3        
    beq     Lfe77                   ;2/3      
    lda     #$08                    ;2         *
    sta     ram_E7                  ;3         *
    lda     ram_81                  ;3         *
    jmp     Lfdbf                   ;3   =  27 *
    
Lfe77
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    ldx     #$0b                    ;2   =   2
Lfe7b
    lda     Lff81,x                 ;4        
    sta     ram_EB,x                ;4        
    dex                             ;2        
    bpl     Lfe7b                   ;2/3      
    lda     #$09                    ;2        
    jsr     Lfeb5                   ;6   =  20
Lfe88
    jmp     Lf0c4                   ;3   =   3
    
Lfe8b
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     #$03                    ;2        
    ldy     #$00                    ;2        
    sty     REFP1                   ;3        
    sta     NUSIZ0                  ;3        
    sta     NUSIZ1                  ;3        
    sta     VDELP0                  ;3        
    sta     VDELP1                  ;3        
    sty     GRP0                    ;3        
    sty     GRP1                    ;3        
    sty     GRP0                    ;3        
    sty     GRP1                    ;3        
    sty     HMP1                    ;3        
    nop                             ;2        
    sta     RESP0                   ;3        
    sta     RESP1                   ;3        
    lda     #$f0                    ;2        
    sta     HMP0                    ;3        
    sty     REFP0                   ;3        
    sta     WSYNC                   ;3   =  53
;---------------------------------------
    sta     HMOVE                   ;3        
    rts                             ;6   =   9
    
Lfeb5
    sta     ram_E7                  ;3   =   3
Lfeb7
    ldy     ram_E7                  ;3        
    lda     (ram_EB),y              ;5        
    sta     GRP0                    ;3        
    sta     WSYNC                   ;3   =  14
;---------------------------------------
    lda     (ram_ED),y              ;5        
    sta     GRP1                    ;3        
    lda     (ram_EF),y              ;5        
    sta     GRP0                    ;3        
    lda     (ram_F1),y              ;5        
    sta     ram_E8                  ;3        
    lda     (ram_F3),y              ;5        
    tax                             ;2        
    lda     (ram_F5),y              ;5        
    tay                             ;2        
    lda     ram_E8                  ;3        
    sta     GRP1                    ;3        
    stx     GRP0                    ;3        
    sty     GRP1                    ;3        
    sty     GRP0                    ;3        
    dec     ram_E7                  ;5        
    bpl     Lfeb7                   ;2/3      
    lda     #$00                    ;2        
    sta     GRP0                    ;3        
    sta     GRP1                    ;3        
    sta     GRP0                    ;3        
    sta     GRP1                    ;3        
    rts                             ;6   =  80
    
    .byte   %01111001 ; | ####  #|            $feea (G)
    .byte   %10000101 ; |#    # #|            $feeb (G)
    .byte   %10110101 ; |# ## # #|            $feec (G)
    .byte   %10100101 ; |# #  # #|            $feed (G)
    .byte   %10110101 ; |# ## # #|            $feee (G)
    .byte   %10000101 ; |#    # #|            $feef (G)
    .byte   %01111001 ; | ####  #|            $fef0 (G)
    .byte   %00010111 ; |   # ###|            $fef1 (G)
    .byte   %00010101 ; |   # # #|            $fef2 (G)
    .byte   %00010101 ; |   # # #|            $fef3 (G)
    .byte   %01110111 ; | ### ###|            $fef4 (G)
    .byte   %01010101 ; | # # # #|            $fef5 (G)
    .byte   %01010101 ; | # # # #|            $fef6 (G)
    .byte   %01110111 ; | ### ###|            $fef7 (G)
    .byte   %01110001 ; | ###   #|            $fef8 (G)
    .byte   %01010001 ; | # #   #|            $fef9 (G)
    .byte   %00010001 ; |   #   #|            $fefa (G)
    .byte   %00110001 ; |  ##   #|            $fefb (G)
    .byte   %00010001 ; |   #   #|            $fefc (G)
    .byte   %01010001 ; | # #   #|            $fefd (G)
    .byte   %01110000 ; | ###    |            $fefe (G)
    
    .byte   $49,$49,$49,$c9,$49,$49,$be     ; $feff (D)
    
    .byte   %01010101 ; | # # # #|            $ff06 (G)
    .byte   %01010101 ; | # # # #|            $ff07 (G)
    .byte   %01010101 ; | # # # #|            $ff08 (G)
    .byte   %11011001 ; |## ##  #|            $ff09 (G)
    .byte   %01010101 ; | # # # #|            $ff0a (G)
    .byte   %01010101 ; | # # # #|            $ff0b (G)
    .byte   %10011001 ; |#  ##  #|            $ff0c (G)
    
Lff0d
    .byte   $ea,$fe,$f1,$fe,$f8,$fe,$ff,$fe ; $ff0d (D)
    .byte   $06,$ff,$19,$ff                 ; $ff15 (D)
    
    .byte   %00000000 ; |        |            $ff19 (G)
    .byte   %00000000 ; |        |            $ff1a (G)
    .byte   %00000000 ; |        |            $ff1b (G)
    .byte   %00000000 ; |        |            $ff1c (G)
    .byte   %00000000 ; |        |            $ff1d (G)
    .byte   %00000000 ; |        |            $ff1e (G)
    .byte   %00000000 ; |        |            $ff1f (G)
    
    .byte   $00                             ; $ff20 (*)
    
    .byte   %00011000 ; |   ##   |            $ff21 (G)
    .byte   %00100100 ; |  #  #  |            $ff22 (G)
    .byte   %00011000 ; |   ##   |            $ff23 (G)
    .byte   %00111100 ; |  ####  |            $ff24 (G)
    .byte   %01100110 ; | ##  ## |            $ff25 (G)
    .byte   %01100110 ; | ##  ## |            $ff26 (G)
    .byte   %01100110 ; | ##  ## |            $ff27 (G)
    .byte   %01100110 ; | ##  ## |            $ff28 (G)
    .byte   %01100110 ; | ##  ## |            $ff29 (G)
    .byte   %00111100 ; |  ####  |            $ff2a (G)
    .byte   %01111110 ; | ###### |            $ff2b (G)
    .byte   %00011000 ; |   ##   |            $ff2c (G)
    .byte   %00011000 ; |   ##   |            $ff2d (G)
    .byte   %00011000 ; |   ##   |            $ff2e (G)
    .byte   %00011000 ; |   ##   |            $ff2f (G)
    .byte   %01111000 ; | ####   |            $ff30 (G)
    .byte   %00111000 ; |  ###   |            $ff31 (G)
    .byte   %01111110 ; | ###### |            $ff32 (G)
    .byte   %01100000 ; | ##     |            $ff33 (G)
    .byte   %01100000 ; | ##     |            $ff34 (G)
    .byte   %00111100 ; |  ####  |            $ff35 (G)
    .byte   %00000110 ; |     ## |            $ff36 (G)
    .byte   %01000110 ; | #   ## |            $ff37 (G)
    .byte   %01111100 ; | #####  |            $ff38 (G)
    .byte   %00111100 ; |  ####  |            $ff39 (G)
    .byte   %01000110 ; | #   ## |            $ff3a (G)
    .byte   %00000110 ; |     ## |            $ff3b (G)
    .byte   %00001100 ; |    ##  |            $ff3c (G)
    .byte   %00000110 ; |     ## |            $ff3d (G)
    .byte   %01000110 ; | #   ## |            $ff3e (G)
    .byte   %00111100 ; |  ####  |            $ff3f (G)
    
    .byte   $0c,$0c,$7e,$4c,$2c,$1c,$0c,$7c ; $ff40 (*)
    .byte   $46,$06,$7c,$60,$60,$7e,$3c,$66 ; $ff48 (*)
    .byte   $66,$7c,$60,$62,$3c,$18,$18,$08 ; $ff50 (*)
    .byte   $04,$02,$62,$7e,$3c,$66,$66,$3c ; $ff58 (*)
    .byte   $66,$66,$3c,$3c,$46,$06,$3e,$66 ; $ff60 (*)
    .byte   $66,$3c                         ; $ff68 (*)
Lff6a
    .byte   $24                             ; $ff6a (D)
Lff6b
    .byte   $2b,$32,$39                     ; $ff6b (D)
    .byte   $40,$47,$4e,$55,$5c,$63         ; $ff6e (*)
Lff74
    .byte   $19                             ; $ff74 (D)
Lff75
    .byte   $8d,$ff,$94,$ff,$9b,$ff,$a2,$ff ; $ff75 (D)
    .byte   $19,$ff,$19,$ff                 ; $ff7d (D)
Lff81
    .byte   $a9,$ff,$b3,$ff,$bd,$ff,$c7,$ff ; $ff81 (D)
    .byte   $d1,$ff,$db,$ff                 ; $ff89 (D)
    
    .byte   %01111101 ; | ##### #|            $ff8d (G)
    .byte   %11111101 ; |###### #|            $ff8e (G)
    .byte   %11001101 ; |##  ## #|            $ff8f (G)
    .byte   %11011101 ; |## ### #|            $ff90 (G)
    .byte   %11000001 ; |##     #|            $ff91 (G)
    .byte   %11111101 ; |###### #|            $ff92 (G)
    .byte   %01111100 ; | #####  |            $ff93 (G)
    .byte   %10011011 ; |#  ## ##|            $ff94 (G)
    .byte   %10011011 ; |#  ## ##|            $ff95 (G)
    .byte   %10011011 ; |#  ## ##|            $ff96 (G)
    .byte   %11111011 ; |##### ##|            $ff97 (G)
    .byte   %10011011 ; |#  ## ##|            $ff98 (G)
    .byte   %11111011 ; |##### ##|            $ff99 (G)
    .byte   %11110001 ; |####   #|            $ff9a (G)
    .byte   %01101100 ; | ## ##  |            $ff9b (G)
    .byte   %01101101 ; | ## ## #|            $ff9c (G)
    .byte   %01101101 ; | ## ## #|            $ff9d (G)
    .byte   %01101101 ; | ## ## #|            $ff9e (G)
    .byte   %01101101 ; | ## ## #|            $ff9f (G)
    .byte   %11111101 ; |###### #|            $ffa0 (G)
    .byte   %11111000 ; |#####   |            $ffa1 (G)
    
    .byte   $f0,$f0,$80,$e0,$80,$f0,$f0     ; $ffa2 (D)
    
    .byte   %11110111 ; |#### ###|            $ffa9 (G)
    .byte   %11110111 ; |#### ###|            $ffaa (G)
    .byte   %01111111 ; | #######|            $ffab (G)
    .byte   %00111110 ; |  ##### |            $ffac (G)
    .byte   %00011100 ; |   ###  |            $ffad (G)
    .byte   %00011100 ; |   ###  |            $ffae (G)
    .byte   %00111110 ; |  ##### |            $ffaf (G)
    .byte   %01111111 ; | #######|            $ffb0 (G)
    .byte   %11110111 ; |#### ###|            $ffb1 (G)
    .byte   %11110111 ; |#### ###|            $ffb2 (G)
    .byte   %10111100 ; |# ####  |            $ffb3 (G)
    .byte   %10111100 ; |# ####  |            $ffb4 (G)
    .byte   %01100001 ; | ##    #|            $ffb5 (G)
    .byte   %11100011 ; |###   ##|            $ffb6 (G)
    .byte   %11111011 ; |##### ##|            $ffb7 (G)
    .byte   %11111011 ; |##### ##|            $ffb8 (G)
    .byte   %11100011 ; |###   ##|            $ffb9 (G)
    .byte   %01100011 ; | ##   ##|            $ffba (G)
    .byte   %10111101 ; |# #### #|            $ffbb (G)
    .byte   %11011011 ; |## ## ##|            $ffbc (G)
    .byte   %01000111 ; | #   ###|            $ffbd (G)
    .byte   %11100111 ; |###  ###|            $ffbe (G)
    .byte   %10110111 ; |# ## ###|            $ffbf (G)
    .byte   %10111011 ; |# ### ##|            $ffc0 (G)
    .byte   %10111011 ; |# ### ##|            $ffc1 (G)
    .byte   %10111011 ; |# ### ##|            $ffc2 (G)
    .byte   %10111011 ; |# ### ##|            $ffc3 (G)
    .byte   %10111011 ; |# ### ##|            $ffc4 (G)
    .byte   %10111011 ; |# ### ##|            $ffc5 (G)
    .byte   %10111011 ; |# ### ##|            $ffc6 (G)
    
    .byte   $9e,$bf,$b7,$b7,$b7,$b7,$b7,$b7 ; $ffc7 (D)
    .byte   $bf,$9e                         ; $ffcf (D)
    
    .byte   %01111101 ; | ##### #|            $ffd1 (G)
    .byte   %01111100 ; | #####  |            $ffd2 (G)
    .byte   %01101110 ; | ## ### |            $ffd3 (G)
    .byte   %01101110 ; | ## ### |            $ffd4 (G)
    .byte   %01101110 ; | ## ### |            $ffd5 (G)
    .byte   %01101110 ; | ## ### |            $ffd6 (G)
    .byte   %01101110 ; | ## ### |            $ffd7 (G)
    .byte   %01101110 ; | ## ### |            $ffd8 (G)
    .byte   %01101110 ; | ## ### |            $ffd9 (G)
    .byte   %11101110 ; |### ### |            $ffda (G)
    .byte   %11110000 ; |####    |            $ffdb (G)
    .byte   %11111000 ; |#####   |            $ffdc (G)
    .byte   %00111000 ; |  ###   |            $ffdd (G)
    .byte   %00111000 ; |  ###   |            $ffde (G)
    .byte   %11111000 ; |#####   |            $ffdf (G)
    .byte   %11110000 ; |####    |            $ffe0 (G)
    .byte   %11000000 ; |##      |            $ffe1 (G)
    .byte   %11000000 ; |##      |            $ffe2 (G)
    .byte   %11111000 ; |#####   |            $ffe3 (G)
    .byte   %11110000 ; |####    |            $ffe4 (G)

  IF PLUSROM
SendPlusROMScores
    sta     ram_80    ; $ffe5
    sta     ram_91    ; $ffe7
    sta     Lfff9     ; $ffe9
    jmp     Lf8fa
  ENDIF

    ORG     $0ff0, $00
    RORG    $fff0, $00
Lfff0
    .byte   $00,$00,$00,$00,$00,$00,$00,$00

Lfff8
    .byte   $00
Lfff9
    .byte   $00

  IF PLUSROM
    .word (PlusROM_API - $9000)
  ELSE 
    .word $0000          ; NMI
  ENDIF
    .word $f000          ; RESET
    .word $f000          ; IRQ



;***********************************************************
;      Bank 1 / 0..1
;***********************************************************

    SEG     CODE
    ORG     $1000
    RORG    $b000

    sta     $fff8                   ;4        
    jmp     $f01c                   ;3   =   7 *
    
    sta     $fff8                   ;4        
    jmp     Lb01e                   ;3   =   7
    
    sta     $fff8                   ;4        
    jmp     Lbd21                   ;3   =   7
    
    sta     $fff8                   ;4        
    jmp     Lbf17                   ;3   =   7
    
    sta     $fff8                   ;4        
    jmp     Lbd0a                   ;3   =   7
    
Lb01e
    lda     ram_87                  ;3        
    clc                             ;2        
    asl                             ;2        
    tay                             ;2        
    lda     Lb761,y                 ;4        
    sta     ram_A2                  ;3        
    lda     Lb762,y                 ;4        
    sta     ram_A3                  ;3        
    lda     ram_9E                  ;3        
    sta     ram_9F                  ;3        
    ldy     ram_9F                  ;3        
    dec     ram_9F                  ;5        
    lda     (ram_A2),y              ;5        
    and     #$1f                    ;2        
    tay                             ;2        
    lda     Lbac9,y                 ;4        
    sta     COLUPF                  ;3        
    lda     Lbcc9,y                 ;4        
    clc                             ;2        
    adc     Lbbc9,y                 ;4        
    clc                             ;2        
    adc     #$02                    ;2        
    sec                             ;2        
    sbc     ram_A0                  ;3        
    sta     ram_E4                  ;3        
    ldx     ram_F7                  ;3        
    ldy     ram_A7,x                ;4        
    ldx     Lb694,y                 ;4        
    lda     Lb0fa,x                 ;4        
    sta     NUSIZ0                  ;3        
    lda     ram_91                  ;3        
    and     #$02                    ;2        
    bne     Lb06e                   ;2/3      
    ldx     ram_F8                  ;3        
    lda     ram_B3,x                ;4        
    and     #$3f                    ;2        
    tax                             ;2        
    lda     Lb6de,x                 ;4        
    sta     ram_EF                  ;3        
    sta     ram_F1                  ;3   = 121
Lb06e
    ldy     ram_E4                  ;3        
    lda     Lba00,y                 ;4        
    sta     PF1                     ;3        
    lda     Lbb00,y                 ;4        
    sta     PF2                     ;3        
    lda     Lbc00,y                 ;4        
    sta     COLUBK                  ;3        
    lda     ram_A0                  ;3        
    sta     ram_A1                  ;3        
    sta     WSYNC                   ;3   =  33
;---------------------------------------
    lda     ram_80                  ;3        
    lsr                             ;2        
    and     ram_E7                  ;3        
    tax                             ;2        
    lda     #$00                    ;2        
    cpx     #$00                    ;2        
    beq     Lb0ab                   ;2/3      
    cpx     #$01                    ;2        
    beq     Lb0a5                   ;2/3      
    cpx     #$02                    ;2        
    beq     Lb09f                   ;2/3      
    sta     WSYNC                   ;3   =  27 *
;---------------------------------------
    sta     HMOVE                   ;3         *
    sta     VBLANK                  ;3   =   6 *
Lb09f
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     VBLANK                  ;3   =   6
Lb0a5
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     VBLANK                  ;3   =   6
Lb0ab
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     VBLANK                  ;3        
    jmp.ind (ram_D9)                ;5   =  11
    
Lb0b4
    .byte   $79,$69,$59,$49,$39,$29,$19,$09 ; $b0b4 (D)
    .byte   $f9,$e9,$d9,$c9,$b9,$a9,$99,$70 ; $b0bc (D)
    .byte   $60,$50,$40,$30,$20,$10,$00,$f0 ; $b0c4 (D)
    .byte   $e0,$d0,$c0,$b0,$a0,$90,$73,$63 ; $b0cc (D)
    .byte   $53,$43,$33,$23,$13,$03,$f3,$e3 ; $b0d4 (D)
    .byte   $d3,$c3,$b3,$a3,$93,$76,$66,$56 ; $b0dc (D)
    .byte   $46,$36,$26,$16,$06,$f6,$e6,$d6 ; $b0e4 (D)
    .byte   $c6,$b6,$a6,$96,$96,$96,$70,$70 ; $b0ec (D)
Lb0f4
    .byte   $03,$00,$01,$02                 ; $b0f4 (D)
Lb0f8
    .byte   $04,$08                         ; $b0f8 (D)
Lb0fa
    .byte   $30,$35                         ; $b0fa (D)
    .byte   $00,$00,$00,$00                 ; $b0fc (*)
    
Lb100
    jmp     Lb212                   ;3   =   3
    
    jmp     Lb215                   ;3   =   3
    
    jmp     Lb21c                   ;3   =   3
    
    jmp     Lb200                   ;3   =   3
    
    lda     ram_F3                  ;3        
    sta     GRP0                    ;3        
    ldy     ram_F5                  ;3        
    bpl     Lb13b                   ;2/3      
    ldx     ram_F7                  ;3        
    ldy     Lb3e3,x                 ;4        
    sty     ram_F7                  ;3        
    ldx     ram_A7,y                ;4        
    ldy     Lb694,x                 ;4        
    lda     Lb0f8,y                 ;4        
    sta     ram_F5                  ;3        
    lda     Lb0fa,y                 ;4        
    sta     NUSIZ0                  ;3        
    lda     #$00                    ;2        
    sta     ram_F3                  ;3        
    ldx     #$88                    ;2        
    stx     ram_DD                  ;3        
    sta     WSYNC                   ;3   =  56
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     GRP0                    ;3        
    jmp.ind (ram_D9)                ;5   =  11
Lb13b
    lda     (ram_ED),y              ;5        
    sta     ram_F3                  ;3        
    lda     (ram_EB),y              ;5        
    dec     ram_F5                  ;5        
    ldx     #$57                    ;2        
    stx     ram_DD                  ;3        
    sta     WSYNC                   ;3   =  26
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     GRP0                    ;3        
    jmp.ind (ram_D9)                ;5   =  11
Lb150
    lda     #$00                    ;2        
    sta     ram_F3                  ;3        
    jmp     Lb16d                   ;3   =   8
    
    lda     ram_F3                  ;3        
    sta     GRP0                    ;3        
    ldx     #$0c                    ;2        
    stx     ram_DF                  ;3        
    ldy     ram_F5                  ;3        
    bmi     Lb150                   ;2/3      
    sty     HMP0                    ;3        
    lda     (ram_ED),y              ;5        
    sta     ram_F3                  ;3        
    lda     (ram_EB),y              ;5        
    dec     ram_F5                  ;5   =  37
Lb16d
    tax                             ;2        
    ldy     ram_E4                  ;3        
    lda     Lbc00,y                 ;4        
    sta     WSYNC                   ;3   =  12
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     COLUBK                  ;3        
    stx     GRP0                    ;3   =   9
Lb17b
    lda     Lba00,y                 ;4        
    sta     PF1                     ;3        
    lda     Lbb00,y                 ;4        
    sta     PF2                     ;3        
    jmp.ind (ram_DB)                ;5        
    ldx     ram_F7                  ;3        
    ldy     ram_A7,x                ;4        
    lda     Lb64a,y                 ;4        
    sta     COLUP0                  ;3        
    lda     Lb600,y                 ;4        
    sta     ram_ED                  ;3        
    sta     ram_EB                  ;3        
    lda     ram_BF,x                ;4        
    and     #$3f                    ;2        
    tay                             ;2        
    lda     Lb0b4,y                 ;4        
    sta     HMP0                    ;3        
    and     #$0f                    ;2        
    sta     ram_DF                  ;3        
    ldy     ram_E4                  ;3        
    lda     Lbc00,y                 ;4        
    sta     WSYNC                   ;3   =  73
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     COLUBK                  ;3        
    jmp     Lb17b                   ;3   =   9
    
    .byte   $00,$07,$2e,$0e,$21,$12,$0e,$0e ; $b1b4 (D)
    .byte   $23,$35,$03,$35,$30,$12,$2e,$07 ; $b1bc (D)
    .byte   $21,$07,$47,$0f,$07,$00         ; $b1c4 (D)
    
Lb1ca
    dec     ram_A1                  ;5        
    inc     ram_E4                  ;5        
    lda     ram_B5                  ;3        
    cmp     ram_E5                  ;3        
    bcs     Lb1e6                   ;2/3      
    ldy     ram_F6                  ;3        
    bmi     Lb1e2                   ;2/3      
    lda     (ram_EF),y              ;5        
    dec     ram_F6                  ;5   =  33
Lb1dc
    sta     ram_F4                  ;3        
    tax                             ;2        
    jmp     Lb3bd                   ;3   =   8
    
Lb1e2
    lda     #$7b                    ;2        
    sta     ram_D9                  ;3   =   5
Lb1e6
    lda     #$00                    ;2        
    jmp     Lb1dc                   ;3   =   5
    
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b1eb (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b1f3 (*)
    .byte   $00,$00,$00,$00,$00             ; $b1fb (*)
    
Lb200
    nop                             ;2        
    lda     ram_F7                  ;3        
    ldx     ram_F9                  ;3        
    bit     CXM1P|$30               ;3        
    sta     RESP0                   ;3        
    bmi     Lb20f                   ;2/3      
    sta     ram_E2                  ;3        
    sta     ram_E2                  ;3   =  22
Lb20f
    jmp     Lb239                   ;3   =   3
    
Lb212
    jmp     Lb226                   ;3   =   3
    
Lb215
    inc     ram_E7                  ;5        
    inc     ram_E7                  ;5        
    jmp     Lb226                   ;3   =  13
    
Lb21c
    inc     ram_E7                  ;5        
    inc     ram_E7                  ;5        
    dec     ram_E7                  ;5        
    dec     ram_E7                  ;5        
    sta     ram_E7                  ;3   =  23
Lb226
    lda     ram_F7                  ;3        
    ldx     ram_F9                  ;3        
    bit     CXM1P|$30               ;3        
    bpl     Lb233                   ;2/3      
    nop                             ;2        
    nop                             ;2        
    jmp     Lb237                   ;3   =  18
    
Lb233
    sta     ram_E2                  ;3        
    sta     ram_E2                  ;3   =   6
Lb237
    sta     RESP0                   ;3   =   3
Lb239
    lda     #$57                    ;2        
    sta     WSYNC                   ;3   =   5
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     ram_DD                  ;3        
    jmp.ind (ram_D9)                ;5        
    lda     #$00                    ;2         *
    sta     WSYNC                   ;3   =  16 *
;---------------------------------------
    sta     HMOVE                   ;3         *
    lda     ram_F4                  ;3         *
    sta     GRP1                    ;3         *
    jmp.ind (ram_DD)                ;5   =  14 *
Lb251
    ldy     #$ff                    ;2        
    lda     #$00                    ;2        
    sta     WSYNC                   ;3   =   7
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     GRP0                    ;3        
    sta     GRP1                    ;3        
    sta     ENAM1                   ;3        
    sta     ENAM0                   ;3        
    lda     INTIM                   ;4        
    cmp     #$05                    ;2        
    bcs     Lb251                   ;2/3      
    lda     #$00                    ;2        
    sta     WSYNC                   ;3   =  28
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     PF0                     ;3        
    sta     PF1                     ;3        
    sta     PF2                     ;3        
    sta     COLUBK                  ;3        
    ldy     ram_9E                  ;3        
    lda     (ram_A2),y              ;5        
    sta     ram_F7                  ;3        
    and     #$1f                    ;2        
    tay                             ;2        
    lda     Lbbc9,y                 ;4        
    sta     ram_F8                  ;3        
    jmp     $f006                   ;3   =  40
    
Lb287
    jmp     Lb251                   ;3   =   3
    
    lda     #$00                    ;2        
    sta     ENAM1                   ;3        
    sta     ENAM0                   ;3        
    lda     INTIM                   ;4        
    cmp     #$09                    ;2        
    bcc     Lb287                   ;2/3      
    jmp     Lb2c4                   ;3   =  19
    
    ldx     ram_F9                  ;3        
    lda     ram_BB,x                ;4        
    cmp     ram_E5                  ;3        
    bcs     Lb2a7                   ;2/3      
    lda     #$b0                    ;2        
    jmp     Lb2c2                   ;3   =  17
    
Lb2a7
    bit     CXM1P|$30               ;3        
    bmi     Lb2ad                   ;2/3      
    stx     ram_E3                  ;3   =   8
Lb2ad
    jmp     Lb2c4                   ;3   =   3
    
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    sta     RESM1                   ;3        
    ldx     ram_F9                  ;3        
    lda     ram_CF,x                ;4        
    asl                             ;2        
    sta     HMM1                    ;3        
    lda     #$d5                    ;2        
    bcc     Lb2c2                   ;2/3      
    sta     RESM1                   ;3   =  28
Lb2c2
    sta     ram_DB                  ;3   =   3
Lb2c4
    inc     ram_E5                  ;5        
    ldx     ram_F4                  ;3        
    lda     #$00                    ;2        
    sta     WSYNC                   ;3   =  13
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     ENAM0                   ;3        
    stx     GRP1                    ;3        
    jmp.ind (ram_DF)                ;5        
    ldy     ram_E6                  ;3        
    sty     ENAM1                   ;3        
    sty     HMM1                    ;3        
    dec     ram_F9                  ;5        
    lda     #$e2                    ;2        
    jmp     Lb2c2                   ;3   =  33
    
    ldx     #$00                    ;2        
    stx     HMM1                    ;3        
    lda     #$f3                    ;2        
    ldy     ram_F9                  ;3        
    bmi     Lb2f0                   ;2/3      
    stx     ENAM1                   ;3        
    lda     #$9a                    ;2   =  17
Lb2f0
    jmp     Lb2c2                   ;3   =   3
    
    lda     #$8a                    ;2        
    jmp     Lb2c2                   ;3   =   5
    
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b2f8 (*)
    .byte   $ea,$ea                         ; $b300 (*)
    
Lb302
    dec     ram_9F                  ;5        
    ldy     ram_F8                  ;3        
    ldx     Lb0f4,y                 ;4        
    lda     ram_B7,x                ;4        
    cmp     ram_E5                  ;3        
    bne     Lb320                   ;2/3      
    stx     ram_F8                  ;3        
    lda     #$03                    ;2        
    sta     ram_F6                  ;3        
    lda     #$3a                    ;2   =  31
Lb317
    inc     ram_E4                  ;5        
    sta     ram_D9                  ;3        
    ldx     #$00                    ;2        
    jmp     Lb3bd                   ;3   =  13
    
Lb320
    lda     #$25                    ;2        
    jmp     Lb317                   ;3   =   5
    
    lda     #$ff                    ;2        
    sta     ram_F6                  ;3        
    lda     #$7b                    ;2        
    jmp     Lb317                   ;3   =  10
    
    dec     ram_A1                  ;5        
    lda     #$34                    ;2        
    bne     Lb317                   ;2/3      
    dec     ram_A1                  ;5        
    lda     #$7b                    ;2        
    bne     Lb317                   ;2/3      
    inc     ram_E4                  ;5        
    ldx     #$7b                    ;2        
    stx     ram_D9                  ;3        
    ldx     ram_F8                  ;3        
    lda     ram_FA,x                ;4        
    asl                             ;2        
    sta     HMP1                    ;3        
    sta     RESP1                   ;3        
    lda     ram_B3,x                ;4        
    and     #$3f                    ;2        
    tay                             ;2        
    lda     Lb6de,y                 ;4        
    sta     ram_EF                  ;3        
    bcc     Lb357                   ;2/3      
    sta     RESP1                   ;3   =  63
Lb357
    sta     ram_F1                  ;3        
    ldy     ram_E5                  ;3        
    cpy     ram_D4                  ;3        
    bne     Lb36e                   ;2/3      
    lda     ram_F8                  ;3        
    ldx     #$02                    ;2        
    sta     WSYNC                   ;3   =  19
;---------------------------------------
    stx     HMOVE                   ;3        
    sta     ram_E1                  ;3        
    stx     ENAM0                   ;3        
    jmp.ind (ram_DD)                ;5   =  14
Lb36e
    ldx     #$00                    ;2        
    sta     WSYNC                   ;3   =   5
;---------------------------------------
    stx     HMOVE                   ;3        
    stx     ENAM0                   ;3        
    stx     ENAM0                   ;3        
    jmp.ind (ram_DD)                ;5        
    inc     ram_E4                  ;5        
    dec     ram_A1                  ;5        
    bpl     Lb3ae                   ;2/3      
    ldx     #$02                    ;2        
    ldy     ram_9F                  ;3        
    lda     (ram_A2),y              ;5        
    and     #$1f                    ;2        
    tay                             ;2        
    lda     Lbac9,y                 ;4        
    sta     COLUPF                  ;3        
    lda     Lbbc9,y                 ;4        
    sta     ram_A1                  ;3        
    lda     Lbcc9,y                 ;4        
    sta     ram_E4                  ;3        
    stx     ram_D9                  ;3        
    ldy     ram_E5                  ;3        
    cpy     ram_D4                  ;3        
    bne     Lb36e                   ;2/3      
    lda     ram_F8                  ;3        
    sta     WSYNC                   ;3   =  78
;---------------------------------------
    stx     HMOVE                   ;3        
    sta     ram_E1                  ;3        
    stx     ENAM0                   ;3        
    jmp.ind (ram_DD)                ;5   =  14
Lb3ae
    ldy     ram_F6                  ;3        
    sty     HMP1                    ;3        
    bmi     Lb3d9                   ;2/3      
    lda     (ram_F1),y              ;5        
    sta     ram_F4                  ;3        
    lda     (ram_EF),y              ;5        
    tax                             ;2        
    dec     ram_F6                  ;5   =  28
Lb3bd
    ldy     ram_E5                  ;3        
    cpy     ram_D4                  ;3        
    bne     Lb3d4                   ;2/3      
    lda     ram_F8                  ;3        
    sta     ram_E1                  ;3        
    lda     #$02                    ;2   =  16
Lb3c9
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     ENAM0                   ;3        
    stx     GRP1                    ;3        
    jmp.ind (ram_DD)                ;5   =  14
Lb3d4
    lda     #$00                    ;2        
    jmp     Lb3c9                   ;3   =   5
    
Lb3d9
    ldx     #$00                    ;2        
    stx     ram_F4                  ;3        
    jmp     Lb3bd                   ;3   =   8
    
    jmp     Lb1ca                   ;3   =   3
    
Lb3e3
    .byte   $0b,$00,$01,$02,$03,$04,$05,$06 ; $b3e3 (D)
    .byte   $07,$08,$09,$0a                 ; $b3eb (D)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b3ef (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b3f7 (*)
    .byte   $00,$00,$00                     ; $b3ff (*)
    
    .byte   %00000000 ; |        |            $b402 (G)
    .byte   %00000000 ; |        |            $b403 (G)
    .byte   %00000000 ; |        |            $b404 (G)
    .byte   %00000000 ; |        |            $b405 (G)
    .byte   %00000000 ; |        |            $b406 (G)
    .byte   %10011001 ; |#  ##  #|            $b407 (G)
    .byte   %10111101 ; |# #### #|            $b408 (G)
    .byte   %11111111 ; |########|            $b409 (G)
    .byte   %10000001 ; |#      #|            $b40a (G)
    .byte   %00000000 ; |        |            $b40b (G)
    .byte   %01011010 ; | # ## # |            $b40c (G)
    .byte   %01111110 ; | ###### |            $b40d (G)
    .byte   %01111110 ; | ###### |            $b40e (G)
    .byte   %01000010 ; | #    # |            $b40f (G)
    .byte   %00000000 ; |        |            $b410 (G)
    .byte   %00111100 ; |  ####  |            $b411 (G)
    .byte   %00111100 ; |  ####  |            $b412 (G)
    .byte   %00111100 ; |  ####  |            $b413 (G)
    .byte   %00011000 ; |   ##   |            $b414 (G)
    .byte   %00000000 ; |        |            $b415 (G)
    .byte   %01011010 ; | # ## # |            $b416 (G)
    .byte   %01111110 ; | ###### |            $b417 (G)
    .byte   %01111110 ; | ###### |            $b418 (G)
    .byte   %01000010 ; | #    # |            $b419 (G)
    .byte   %00000000 ; |        |            $b41a (G)
    .byte   %00011000 ; |   ##   |            $b41b (G)
    .byte   %00110100 ; |  ## #  |            $b41c (G)
    .byte   %01111110 ; | ###### |            $b41d (G)
    .byte   %00010000 ; |   #    |            $b41e (G)
    .byte   %00000000 ; |        |            $b41f (G)
    .byte   %01011000 ; | # ##   |            $b420 (G)
    .byte   %00110110 ; |  ## ## |            $b421 (G)
    .byte   %00111100 ; |  ####  |            $b422 (G)
    .byte   %00100100 ; |  #  #  |            $b423 (G)
    .byte   %00000000 ; |        |            $b424 (G)
    .byte   %00011010 ; |   ## # |            $b425 (G)
    .byte   %01110100 ; | ### #  |            $b426 (G)
    .byte   %00111100 ; |  ####  |            $b427 (G)
    .byte   %00001000 ; |    #   |            $b428 (G)
    .byte   %00000000 ; |        |            $b429 (G)
    .byte   %11110001 ; |####   #|            $b42a (G)
    .byte   %11011101 ; |## ### #|            $b42b (G)
    .byte   %01111111 ; | #######|            $b42c (G)
    .byte   %00101100 ; |  # ##  |            $b42d (G)
    .byte   %00000000 ; |        |            $b42e (G)
    .byte   %11100000 ; |###     |            $b42f (G)
    .byte   %11100010 ; |###   # |            $b430 (G)
    .byte   %01111011 ; | #### ##|            $b431 (G)
    .byte   %00110000 ; |  ##    |            $b432 (G)
    .byte   %00000000 ; |        |            $b433 (G)
    .byte   %11000000 ; |##      |            $b434 (G)
    .byte   %10110000 ; |# ##    |            $b435 (G)
    .byte   %10001100 ; |#   ##  |            $b436 (G)
    .byte   %00000000 ; |        |            $b437 (G)
    .byte   %00000000 ; |        |            $b438 (G)
    .byte   %00000000 ; |        |            $b439 (G)
    .byte   %01000011 ; | #    ##|            $b43a (G)
    .byte   %01011100 ; | # ###  |            $b43b (G)
    .byte   %01110000 ; | ###    |            $b43c (G)
    .byte   %00000000 ; |        |            $b43d (G)
    .byte   %10000111 ; |#    ###|            $b43e (G)
    .byte   %10111101 ; |# #### #|            $b43f (G)
    .byte   %11011000 ; |## ##   |            $b440 (G)
    .byte   %00110000 ; |  ##    |            $b441 (G)
    .byte   %00000000 ; |        |            $b442 (G)
    .byte   %01111000 ; | ####   |            $b443 (G)
    .byte   %00110110 ; |  ## ## |            $b444 (G)
    .byte   %00110110 ; |  ## ## |            $b445 (G)
    .byte   %01111000 ; | ####   |            $b446 (G)
    .byte   %00000000 ; |        |            $b447 (G)
    .byte   %00001100 ; |    ##  |            $b448 (G)
    .byte   %00010110 ; |   # ## |            $b449 (G)
    .byte   %00010110 ; |   # ## |            $b44a (G)
    .byte   %00001100 ; |    ##  |            $b44b (G)
    .byte   %00000000 ; |        |            $b44c (G)
    .byte   %00011100 ; |   ###  |            $b44d (G)
    .byte   %00111100 ; |  ####  |            $b44e (G)
    .byte   %00111100 ; |  ####  |            $b44f (G)
    .byte   %00011100 ; |   ###  |            $b450 (G)
    .byte   %00000000 ; |        |            $b451 (G)
    .byte   %00111100 ; |  ####  |            $b452 (G)
    .byte   %11110110 ; |#### ## |            $b453 (G)
    .byte   %11110110 ; |#### ## |            $b454 (G)
    .byte   %00111100 ; |  ####  |            $b455 (G)
    .byte   %00000000 ; |        |            $b456 (G)
    .byte   %01101100 ; | ## ##  |            $b457 (G)
    .byte   %11010110 ; |## # ## |            $b458 (G)
    .byte   %11000110 ; |##   ## |            $b459 (G)
    .byte   %00111000 ; |  ###   |            $b45a (G)
    .byte   %00000000 ; |        |            $b45b (G)
    .byte   %01111100 ; | #####  |            $b45c (G)
    .byte   %01111100 ; | #####  |            $b45d (G)
    .byte   %01101100 ; | ## ##  |            $b45e (G)
    .byte   %00111000 ; |  ###   |            $b45f (G)
    .byte   %00000000 ; |        |            $b460 (G)
    .byte   %00111000 ; |  ###   |            $b461 (G)
    .byte   %00111000 ; |  ###   |            $b462 (G)
    .byte   %00111000 ; |  ###   |            $b463 (G)
    .byte   %00010000 ; |   #    |            $b464 (G)
    .byte   %00000000 ; |        |            $b465 (G)
    .byte   %00010000 ; |   #    |            $b466 (G)
    .byte   %00010000 ; |   #    |            $b467 (G)
    .byte   %00010000 ; |   #    |            $b468 (G)
    .byte   %00010000 ; |   #    |            $b469 (G)
    .byte   %00000000 ; |        |            $b46a (G)
    .byte   %11111111 ; |########|            $b46b (G)
    .byte   %11111111 ; |########|            $b46c (G)
    .byte   %11111111 ; |########|            $b46d (G)
    .byte   %11111111 ; |########|            $b46e (G)
    .byte   %11111111 ; |########|            $b46f (G)
    .byte   %11111111 ; |########|            $b470 (G)
    .byte   %11111111 ; |########|            $b471 (G)
    .byte   %11111111 ; |########|            $b472 (G)
    .byte   %00000000 ; |        |            $b473 (G)
    .byte   %01111110 ; | ###### |            $b474 (G)
    .byte   %01111110 ; | ###### |            $b475 (G)
    .byte   %01111110 ; | ###### |            $b476 (G)
    .byte   %01111110 ; | ###### |            $b477 (G)
    .byte   %11111111 ; |########|            $b478 (G)
    .byte   %11111111 ; |########|            $b479 (G)
    .byte   %11111111 ; |########|            $b47a (G)
    .byte   %00000000 ; |        |            $b47b (G)
    .byte   %00000000 ; |        |            $b47c (G)
    .byte   %00000000 ; |        |            $b47d (G)
    .byte   %00000000 ; |        |            $b47e (G)
    .byte   %00111100 ; |  ####  |            $b47f (G)
    .byte   %01111110 ; | ###### |            $b480 (G)
    .byte   %01111110 ; | ###### |            $b481 (G)
    .byte   %11111111 ; |########|            $b482 (G)
    .byte   %00000000 ; |        |            $b483 (G)
    .byte   %00000000 ; |        |            $b484 (G)
    .byte   %00000000 ; |        |            $b485 (G)
    .byte   %00000000 ; |        |            $b486 (G)
    .byte   %00000000 ; |        |            $b487 (G)
    .byte   %00000000 ; |        |            $b488 (G)
    .byte   %00111100 ; |  ####  |            $b489 (G)
    .byte   %11111111 ; |########|            $b48a (G)
    .byte   %00000000 ; |        |            $b48b (G)
    .byte   %00000000 ; |        |            $b48c (G)
    .byte   %00000000 ; |        |            $b48d (G)
    .byte   %00000000 ; |        |            $b48e (G)
    .byte   %00000000 ; |        |            $b48f (G)
    .byte   %00000000 ; |        |            $b490 (G)
    .byte   %00000000 ; |        |            $b491 (G)
    .byte   %00000000 ; |        |            $b492 (G)
    .byte   %11111111 ; |########|            $b493 (G)
    .byte   %00000000 ; |        |            $b494 (G)
    .byte   %00000000 ; |        |            $b495 (G)
    .byte   %00000000 ; |        |            $b496 (G)
    .byte   %00000000 ; |        |            $b497 (G)
    .byte   %00000000 ; |        |            $b498 (G)
    .byte   %00000000 ; |        |            $b499 (G)
    .byte   %00000000 ; |        |            $b49a (G)
    .byte   %01111110 ; | ###### |            $b49b (G)
    .byte   %00011000 ; |   ##   |            $b49c (G)
    .byte   %00000000 ; |        |            $b49d (G)
    .byte   %00000000 ; |        |            $b49e (G)
    .byte   %00000000 ; |        |            $b49f (G)
    .byte   %00000000 ; |        |            $b4a0 (G)
    .byte   %00000000 ; |        |            $b4a1 (G)
    .byte   %11111111 ; |########|            $b4a2 (G)
    .byte   %11111111 ; |########|            $b4a3 (G)
    .byte   %01111110 ; | ###### |            $b4a4 (G)
    .byte   %00111100 ; |  ####  |            $b4a5 (G)
    .byte   %00111100 ; |  ####  |            $b4a6 (G)
    .byte   %00000000 ; |        |            $b4a7 (G)
    .byte   %00000000 ; |        |            $b4a8 (G)
    .byte   %00000000 ; |        |            $b4a9 (G)
    .byte   %11111111 ; |########|            $b4aa (G)
    .byte   %11111111 ; |########|            $b4ab (G)
    .byte   %11111111 ; |########|            $b4ac (G)
    .byte   %11111111 ; |########|            $b4ad (G)
    .byte   %01111110 ; | ###### |            $b4ae (G)
    .byte   %01111110 ; | ###### |            $b4af (G)
    .byte   %01111110 ; | ###### |            $b4b0 (G)
    .byte   %00000000 ; |        |            $b4b1 (G)
    .byte   %00000000 ; |        |            $b4b2 (G)
    .byte   %10000000 ; |#       |            $b4b3 (G)
    .byte   %00010111 ; |   # ###|            $b4b4 (G)
    .byte   %11111110 ; |####### |            $b4b5 (G)
    .byte   %11000111 ; |##   ###|            $b4b6 (G)
    .byte   %00000000 ; |        |            $b4b7 (G)
    .byte   %10000000 ; |#       |            $b4b8 (G)
    .byte   %00010101 ; |   # # #|            $b4b9 (G)
    .byte   %11001110 ; |##  ### |            $b4ba (G)
    .byte   %00110100 ; |  ## #  |            $b4bb (G)
    .byte   %00000000 ; |        |            $b4bc (G)
    .byte   %00000000 ; |        |            $b4bd (G)
    .byte   %00000010 ; |      # |            $b4be (G)
    .byte   %00001100 ; |    ##  |            $b4bf (G)
    .byte   %00011100 ; |   ###  |            $b4c0 (G)
    .byte   %00000000 ; |        |            $b4c1 (G)
    .byte   %00000000 ; |        |            $b4c2 (G)
    .byte   %00000000 ; |        |            $b4c3 (G)
    .byte   %00001000 ; |    #   |            $b4c4 (G)
    .byte   %00011100 ; |   ###  |            $b4c5 (G)
    .byte   %00000000 ; |        |            $b4c6 (G)
    .byte   %00111100 ; |  ####  |            $b4c7 (G)
    .byte   %01111110 ; | ###### |            $b4c8 (G)
    .byte   %00101100 ; |  # ##  |            $b4c9 (G)
    .byte   %00000000 ; |        |            $b4ca (G)
    .byte   %00000000 ; |        |            $b4cb (G)
    .byte   %00100010 ; |  #   # |            $b4cc (G)
    .byte   %11000001 ; |##     #|            $b4cd (G)
    .byte   %00100001 ; |  #    #|            $b4ce (G)
    .byte   %00101100 ; |  # ##  |            $b4cf (G)
    .byte   %00000000 ; |        |            $b4d0 (G)
    .byte   %00000000 ; |        |            $b4d1 (G)
    .byte   %00001000 ; |    #   |            $b4d2 (G)
    .byte   %00001000 ; |    #   |            $b4d3 (G)
    .byte   %00000000 ; |        |            $b4d4 (G)
    .byte   %00000000 ; |        |            $b4d5 (G)
    
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b4d6 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b4de (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b4e6 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b4ee (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b4f6 (*)
    .byte   $00,$00,$00,$00                 ; $b4fe (*)
    .byte   $00,$00,$00,$00,$00,$18,$b5,$f7 ; $b502 (D)
    .byte   $db,$00,$18,$7e,$7e,$5a,$00,$18 ; $b50a (D)
    .byte   $1c,$1c,$18,$00,$18,$6e,$6e,$5a ; $b512 (D)
    .byte   $00,$08,$7e,$34,$18,$00,$04,$3c ; $b51a (D)
    .byte   $74,$18,$00,$20,$3c,$36,$58,$00 ; $b522 (D)
    .byte   $7f,$f1,$bd,$76,$00,$00,$fe,$62 ; $b52a (D)
    .byte   $3f,$00,$e0,$80,$b4,$fc,$00,$00 ; $b532 (D)
    .byte   $7f,$5c,$60,$00,$fe,$bb,$bd,$60 ; $b53a (D)
    .byte   $00,$00,$5c,$fa,$5c,$00,$00,$0c ; $b542 (D)
    .byte   $3a,$0c,$00,$00,$18,$30,$18,$00 ; $b54a (D)
    .byte   $00,$74,$aa,$74,$00,$38,$c6,$d6 ; $b552 (D)
    .byte   $6c,$00,$38,$6c,$7c,$7c,$00,$10 ; $b55a (D)
    .byte   $38,$38,$38,$00,$10,$10,$10,$10 ; $b562 (D)
    .byte   $00,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $b56a (D)
    .byte   $ff,$00,$00,$7e,$7e,$7e,$ff,$ff ; $b572 (D)
    .byte   $ff,$ff,$00,$00,$00,$3c,$3c,$7e ; $b57a (D)
    .byte   $ff,$ff,$00,$00,$00,$00,$00,$18 ; $b582 (D)
    .byte   $7e,$00,$00,$00,$00,$00,$00,$00 ; $b58a (D)
    .byte   $00,$ff,$00,$00,$00,$00,$00,$00 ; $b592 (D)
    .byte   $00,$ff,$3c,$00,$00,$00,$00,$00 ; $b59a (D)
    .byte   $00,$ff,$7e,$7e,$3c,$00,$00,$00 ; $b5a2 (D)
    .byte   $00,$ff,$ff,$ff,$7e,$7e,$7e,$7e ; $b5aa (D)
    .byte   $00,$2c,$6a,$5b,$6e,$00,$44,$48 ; $b5b2 (D)
    .byte   $42,$0c,$00,$00,$28,$18,$08,$00 ; $b5ba (D)
    .byte   $00,$00,$00,$08,$00,$3c,$7e,$7e ; $b5c2 (D)
    .byte   $3c,$00,$56,$84,$82,$48,$00,$00 ; $b5ca (D)
    .byte   $00,$14,$00,$00                 ; $b5d2 (D)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b5d6 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b5de (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b5e6 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b5ee (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b5f6 (*)
    .byte   $00,$00                         ; $b5fe (*)
Lb600
    .byte   $02                             ; $b600 (D)
    .byte   $02,$02                         ; $b601 (*)
    .byte   $1b,$20,$25,$07,$0c,$11,$16,$43 ; $b603 (D)
    .byte   $48,$4d,$52,$52,$4d,$48,$43,$2a ; $b60b (D)
    .byte   $2f,$2f,$34,$34,$39,$39,$3e,$3e ; $b613 (D)
    .byte   $39,$39,$34,$34,$2f,$2f,$2a,$57 ; $b61b (D)
    .byte   $57,$5c,$61,$66,$61,$5c,$57,$5c ; $b623 (D)
    .byte   $61,$66,$61,$5c,$02,$c7,$6b,$74 ; $b62b (D)
    .byte   $7d,$86,$8f,$98,$a1,$aa,$6b,$74 ; $b633 (D)
    .byte   $7d,$86,$8f,$98,$a1,$aa,$d1,$cc ; $b63b (D)
    .byte   $bd,$b3,$b8,$b3,$b8,$bd,$c2     ; $b643 (D)
    
Lb64a
    .byte   BEIGE|$f                        ; $b64a (C)
    
    .byte   $ff,$ff                         ; $b64b (*)
    
    .byte   BLACK|$6                        ; $b64d (C)
    .byte   BLACK|$6                        ; $b64e (C)
    .byte   BLACK|$6                        ; $b64f (C)
    .byte   BLACK|$f                        ; $b650 (C)
    .byte   BLUE|$8                         ; $b651 (C)
    .byte   BLACK|$f                        ; $b652 (C)
    .byte   BLUE|$8                         ; $b653 (C)
    .byte   BEIGE|$6                        ; $b654 (C)
    .byte   BEIGE|$8                        ; $b655 (C)
    .byte   BEIGE|$a                        ; $b656 (C)
    .byte   BEIGE|$c                        ; $b657 (C)
    .byte   BEIGE|$6                        ; $b658 (C)
    .byte   BEIGE|$8                        ; $b659 (C)
    .byte   BEIGE|$a                        ; $b65a (C)
    .byte   BEIGE|$c                        ; $b65b (C)
    .byte   BLUE|$c                         ; $b65c (C)
    .byte   BLUE|$a                         ; $b65d (C)
    .byte   BLUE|$a                         ; $b65e (C)
    .byte   BLACK|$f                        ; $b65f (C)
    .byte   BLACK|$f                        ; $b660 (C)
    .byte   BLACK|$c                        ; $b661 (C)
    .byte   BLACK|$c                        ; $b662 (C)
    .byte   BLACK|$8                        ; $b663 (C)
    .byte   BLUE|$c                         ; $b664 (C)
    .byte   BLUE|$a                         ; $b665 (C)
    .byte   BLUE|$a                         ; $b666 (C)
    .byte   BLACK|$f                        ; $b667 (C)
    .byte   BLACK|$f                        ; $b668 (C)
    .byte   BLACK|$c                        ; $b669 (C)
    .byte   BLACK|$c                        ; $b66a (C)
    .byte   BLACK|$8                        ; $b66b (C)
    .byte   BLACK|$8                        ; $b66c (C)
    .byte   BLACK|$8                        ; $b66d (C)
    .byte   BLACK|$8                        ; $b66e (C)
    .byte   BLACK|$8                        ; $b66f (C)
    .byte   BLACK|$8                        ; $b670 (C)
    .byte   BLACK|$4                        ; $b671 (C)
    .byte   BLACK|$4                        ; $b672 (C)
    .byte   BLACK|$4                        ; $b673 (C)
    .byte   BLACK|$4                        ; $b674 (C)
    .byte   BLACK|$4                        ; $b675 (C)
    .byte   BLACK|$4                        ; $b676 (C)
    .byte   BLACK|$8                        ; $b677 (C)
    .byte   BLACK|$8                        ; $b678 (C)
    .byte   BEIGE|$f                        ; $b679 (C)
    .byte   BLACK|$0                        ; $b67a (C)
    .byte   BLACK|$8                        ; $b67b (C)
    .byte   BLACK|$8                        ; $b67c (C)
    .byte   BLACK|$8                        ; $b67d (C)
    .byte   BLACK|$8                        ; $b67e (C)
    .byte   BLACK|$4                        ; $b67f (C)
    .byte   BLACK|$4                        ; $b680 (C)
    .byte   BLACK|$4                        ; $b681 (C)
    .byte   BLACK|$4                        ; $b682 (C)
    .byte   BLACK|$4                        ; $b683 (C)
    .byte   BLACK|$4                        ; $b684 (C)
    .byte   BLACK|$4                        ; $b685 (C)
    .byte   BLACK|$4                        ; $b686 (C)
    .byte   BLACK|$4                        ; $b687 (C)
    .byte   BLACK|$8                        ; $b688 (C)
    .byte   BLACK|$8                        ; $b689 (C)
    .byte   BLACK|$8                        ; $b68a (C)
    .byte   BLACK|$c                        ; $b68b (C)
    .byte   BLACK|$c                        ; $b68c (C)
    .byte   ORANGE|$a                       ; $b68d (C)
    .byte   ORANGE|$8                       ; $b68e (C)
    .byte   ORANGE|$c                       ; $b68f (C)
    .byte   ORANGE|$f                       ; $b690 (C)
    .byte   ORANGE|$c                       ; $b691 (C)
    .byte   ORANGE|$6                       ; $b692 (C)
    .byte   ORANGE|$a                       ; $b693 (C)
    
Lb694
    .byte   $00                             ; $b694 (D)
    .byte   $00,$00                         ; $b695 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b697 (D)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b69f (D)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b6a7 (D)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b6af (D)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b6b7 (D)
    .byte   $00,$00,$00,$00,$00,$00,$01,$01 ; $b6bf (D)
    .byte   $01,$01,$01,$01,$01,$01,$01,$01 ; $b6c7 (D)
    .byte   $01,$01,$01,$01,$01,$01,$00,$00 ; $b6cf (D)
    .byte   $00,$00,$00,$00,$00,$00,$00     ; $b6d7 (D)
Lb6de
    .byte   $02,$4d,$52,$4d,$52,$57,$5c,$57 ; $b6de (D)
    .byte   $02,$34,$39,$3e,$43,$07,$0c,$07 ; $b6e6 (D)
    .byte   $0c,$07,$0c                     ; $b6ee (D)
    .byte   $11,$16,$1b                     ; $b6f1 (*)
    .byte   $20,$1b,$25,$2a,$2f             ; $b6f4 (D)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b6f9 (*)
    .byte   $00                             ; $b701 (*)
    
    .byte   %00000000 ; |        |            $b702 (G)
    .byte   %00000000 ; |        |            $b703 (G)
    .byte   %00000000 ; |        |            $b704 (G)
    .byte   %00000000 ; |        |            $b705 (G)
    
    .byte   $00                             ; $b706 (*)
    
    .byte   %00111000 ; |  ###   |            $b707 (G)
    .byte   %11111110 ; |####### |            $b708 (G)
    .byte   %11111110 ; |####### |            $b709 (G)
    .byte   %00111000 ; |  ###   |            $b70a (G)
    
    .byte   $00                             ; $b70b (*)
    
    .byte   %00111000 ; |  ###   |            $b70c (G)
    .byte   %11111110 ; |####### |            $b70d (G)
    .byte   %11111110 ; |####### |            $b70e (G)
    .byte   %00111000 ; |  ###   |            $b70f (G)
    
    .byte   $00,$e0,$d8,$e0,$f0,$00,$e0,$d8 ; $b710 (*)
    .byte   $e0,$f0,$00                     ; $b718 (*)
    
    .byte   %10000001 ; |#      #|            $b71b (G)
    .byte   %11011101 ; |## ### #|            $b71c (G)
    .byte   %11011101 ; |## ### #|            $b71d (G)
    .byte   %01111100 ; | #####  |            $b71e (G)
    
    .byte   $00                             ; $b71f (*)
    
    .byte   %10000000 ; |#       |            $b720 (G)
    .byte   %11000000 ; |##      |            $b721 (G)
    .byte   %01100010 ; | ##   # |            $b722 (G)
    .byte   %00011000 ; |   ##   |            $b723 (G)
    
    .byte   $00                             ; $b724 (*)
    
    .byte   %00100100 ; |  #  #  |            $b725 (G)
    .byte   %11111101 ; |###### #|            $b726 (G)
    .byte   %01100110 ; | ##  ## |            $b727 (G)
    .byte   %00011000 ; |   ##   |            $b728 (G)
    
    .byte   $00                             ; $b729 (*)
    
    .byte   %00100100 ; |  #  #  |            $b72a (G)
    .byte   %11111101 ; |###### #|            $b72b (G)
    .byte   %01101110 ; | ## ### |            $b72c (G)
    .byte   %00011000 ; |   ##   |            $b72d (G)
    
    .byte   $00                             ; $b72e (*)
    
    .byte   %00100100 ; |  #  #  |            $b72f (G)
    .byte   %11111101 ; |###### #|            $b730 (G)
    .byte   %01111110 ; | ###### |            $b731 (G)
    .byte   %00011000 ; |   ##   |            $b732 (G)
    
    .byte   $00                             ; $b733 (*)
    
    .byte   %11111110 ; |####### |            $b734 (G)
    .byte   %11111110 ; |####### |            $b735 (G)
    .byte   %11111110 ; |####### |            $b736 (G)
    .byte   %00000000 ; |        |            $b737 (G)
    
    .byte   $00                             ; $b738 (*)
    
    .byte   %11111110 ; |####### |            $b739 (G)
    .byte   %11101110 ; |### ### |            $b73a (G)
    .byte   %11111110 ; |####### |            $b73b (G)
    .byte   %00000000 ; |        |            $b73c (G)
    
    .byte   $00                             ; $b73d (*)
    
    .byte   %11111110 ; |####### |            $b73e (G)
    .byte   %11111010 ; |##### # |            $b73f (G)
    .byte   %11111110 ; |####### |            $b740 (G)
    .byte   %00000000 ; |        |            $b741 (G)
    
    .byte   $00                             ; $b742 (*)
    
    .byte   %10000000 ; |#       |            $b743 (G)
    .byte   %11000000 ; |##      |            $b744 (G)
    .byte   %11101010 ; |### # # |            $b745 (G)
    .byte   %00000000 ; |        |            $b746 (G)
    
    .byte   $00,$80,$fc,$fa,$7c,$00         ; $b747 (*)
    
    .byte   %11000111 ; |##   ###|            $b74d (G)
    .byte   %11111110 ; |####### |            $b74e (G)
    .byte   %00010111 ; |   # ###|            $b74f (G)
    .byte   %10000000 ; |#       |            $b750 (G)
    
    .byte   $00                             ; $b751 (*)
    
    .byte   %00110100 ; |  ## #  |            $b752 (G)
    .byte   %11001110 ; |##  ### |            $b753 (G)
    .byte   %00010101 ; |   # # #|            $b754 (G)
    .byte   %10000000 ; |#       |            $b755 (G)
    
    .byte   $00                             ; $b756 (*)
    
    .byte   %00011100 ; |   ###  |            $b757 (G)
    .byte   %00001100 ; |    ##  |            $b758 (G)
    .byte   %00000010 ; |      # |            $b759 (G)
    .byte   %00000000 ; |        |            $b75a (G)
    
    .byte   $00                             ; $b75b (*)
    
    .byte   %00011100 ; |   ###  |            $b75c (G)
    .byte   %00001000 ; |    #   |            $b75d (G)
    .byte   %00000000 ; |        |            $b75e (G)
    .byte   %00000000 ; |        |            $b75f (G)
    
    .byte   $00                             ; $b760 (*)
Lb761
    .byte   $81                             ; $b761 (D)
Lb762
    .byte   $b7,$97,$b7,$ad,$b7,$8d,$b8,$31 ; $b762 (D)
    .byte   $b9,$79,$b8,$c1,$b7,$61,$b8,$c4 ; $b76a (D)
    .byte   $b8,$b4,$b1,$db,$b8,$f8,$b8,$0d ; $b772 (D)
    .byte   $b9,$21,$b9,$a8,$b8,$48,$b9,$00 ; $b77a (D)
    .byte   $07,$07,$07,$07,$27,$07,$07,$a7 ; $b782 (D)
    .byte   $07,$a7,$07,$47,$07,$07,$aa,$aa ; $b78a (D)
    .byte   $aa,$2a,$07,$07,$00,$00,$09,$09 ; $b792 (D)
    .byte   $02,$08,$27,$27,$27,$42,$08,$a7 ; $b79a (D)
    .byte   $07,$22,$23,$11,$29,$49,$21,$02 ; $b7a2 (D)
    .byte   $a1,$11,$00,$00,$07,$47,$08,$28 ; $b7aa (D)
    .byte   $22,$08,$08,$02,$67,$27,$27,$a4 ; $b7b2 (D)
    .byte   $11,$02,$21,$21,$11,$08,$00,$00 ; $b7ba (D)
    .byte   $07,$2f,$07,$2f,$07,$6f,$07,$35 ; $b7c2 (D)
    .byte   $35,$2f,$47,$2f,$27,$07,$27,$27 ; $b7ca (D)
    .byte   $47,$27,$27,$07,$07,$07,$00     ; $b7d2 (D)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b7d9 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b7e1 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b7e9 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b7f1 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b7f9 (*)
    .byte   $00                             ; $b801 (*)
    .byte   $00,$00,$00,$00                 ; $b802 (D)
    .byte   $00                             ; $b806 (*)
    .byte   $00,$6c,$aa,$6c                 ; $b807 (D)
    .byte   $00                             ; $b80b (*)
    .byte   $00,$7c,$ee,$7c                 ; $b80c (D)
    .byte   $00,$e0,$d8,$e0,$f0,$00,$e0,$d8 ; $b810 (*)
    .byte   $e0,$f0,$00                     ; $b818 (*)
    .byte   $ff,$fd,$dd,$c5                 ; $b81b (D)
    .byte   $00                             ; $b81f (*)
    .byte   $ff,$fe,$fd,$24                 ; $b820 (D)
    .byte   $00                             ; $b824 (*)
    .byte   $18,$7a,$ed,$3c                 ; $b825 (D)
    .byte   $00                             ; $b829 (*)
    .byte   $18,$7a,$fd,$3c                 ; $b82a (D)
    .byte   $00                             ; $b82e (*)
    .byte   $18,$7a,$fd,$3c                 ; $b82f (D)
    .byte   $00                             ; $b833 (*)
    .byte   $fe,$fe,$fe,$00                 ; $b834 (D)
    .byte   $00                             ; $b838 (*)
    .byte   $fe,$fe,$fe,$00                 ; $b839 (D)
    .byte   $00                             ; $b83d (*)
    .byte   $fe,$c2,$ea,$00                 ; $b83e (D)
    .byte   $00                             ; $b842 (*)
    .byte   $fe,$fc,$f8,$7c                 ; $b843 (D)
    .byte   $00,$fe,$80,$c0,$68,$00         ; $b847 (*)
    .byte   $6e,$5b,$6a,$2c                 ; $b84d (D)
    .byte   $00                             ; $b851 (*)
    .byte   $0c,$42,$48,$44                 ; $b852 (D)
    .byte   $00                             ; $b856 (*)
    .byte   $08,$18,$28,$00                 ; $b857 (D)
    .byte   $00                             ; $b85b (*)
    .byte   $08,$00,$00,$00                 ; $b85c (D)
    .byte   $00                             ; $b860 (*)
    .byte   $00,$07,$07,$07,$08,$09,$a1,$14 ; $b861 (D)
    .byte   $25,$25,$65,$25,$25,$25,$65,$13 ; $b869 (D)
    .byte   $21,$11,$07,$07,$a2,$07,$07,$00 ; $b871 (D)
    .byte   $00,$09,$a9,$08,$09,$09,$47,$06 ; $b879 (D)
    .byte   $02,$30,$30,$30,$11,$47,$30,$11 ; $b881 (D)
    .byte   $07,$21,$11,$00,$00,$07,$27,$08 ; $b889 (D)
    .byte   $47,$a2,$09,$07,$09,$07,$a8,$09 ; $b891 (D)
    .byte   $07,$08,$09,$07,$09,$2e,$4e,$ae ; $b899 (D)
    .byte   $0e,$21,$51,$27,$07,$07,$00,$00 ; $b8a1 (D)
    .byte   $a7,$27,$a7,$a7,$27,$a7,$a7,$a7 ; $b8a9 (D)
    .byte   $27,$a7,$a7,$a7,$27,$a7,$27,$a7 ; $b8b1 (D)
    .byte   $e7,$27,$a7,$a7,$27,$e7,$a7,$a7 ; $b8b9 (D)
    .byte   $a7,$a7,$00,$00,$07,$47,$07,$07 ; $b8c1 (D)
    .byte   $2e,$2d,$2e,$2d,$07,$02,$07,$67 ; $b8c9 (D)
    .byte   $27,$07,$24,$07,$07,$70,$21,$11 ; $b8d1 (D)
    .byte   $47,$00,$00,$00,$08,$22,$07,$08 ; $b8d9 (D)
    .byte   $09,$01,$03,$06,$21,$02,$21,$11 ; $b8e1 (D)
    .byte   $08,$02,$07,$68,$29,$28,$08,$08 ; $b8e9 (D)
    .byte   $01,$11,$08,$22,$21,$11,$00,$00 ; $b8f1 (D)
    .byte   $07,$47,$a7,$a7,$27,$a3,$a3,$a3 ; $b8f9 (D)
    .byte   $a3,$a3,$a3,$a3,$a3,$a7,$a7,$07 ; $b901 (D)
    .byte   $27,$07,$27,$00,$00,$07,$0d,$2d ; $b909 (D)
    .byte   $2d,$54,$05,$a5,$05,$a5,$05,$a5 ; $b911 (D)
    .byte   $13,$b0,$30,$11,$07,$27,$07,$00 ; $b919 (D)
    .byte   $00,$07,$07,$27,$a7,$03,$22,$03 ; $b921 (D)
    .byte   $22,$27,$a7,$27,$27,$27,$07,$00 ; $b929 (D)
    .byte   $00,$07,$27,$07,$63,$94,$05,$25 ; $b931 (D)
    .byte   $05,$25,$05,$25,$05,$25,$05,$25 ; $b939 (D)
    .byte   $13,$27,$07,$27,$07,$07,$00,$00 ; $b941 (D)
    .byte   $07,$27,$47,$07,$07,$03,$22,$03 ; $b949 (D)
    .byte   $22,$03,$22,$03,$22,$06,$07,$27 ; $b951 (D)
    .byte   $07,$07,$00                     ; $b959 (D)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b95c (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b964 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b96c (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b974 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b97c (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b984 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b98c (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b994 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b99c (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b9a4 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b9ac (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b9b4 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b9bc (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b9c4 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b9cc (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b9d4 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b9dc (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b9e4 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b9ec (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $b9f4 (*)
    .byte   $00,$00,$00,$00                 ; $b9fc (*)
    
Lba00
    .byte   %00000000 ; |        |            $ba00 (P)
    .byte   %00000000 ; |        |            $ba01 (P)
    .byte   %00000000 ; |        |            $ba02 (P)
    .byte   %00000000 ; |        |            $ba03 (P)
    .byte   %00000000 ; |        |            $ba04 (P)
    .byte   %00000000 ; |        |            $ba05 (P)
    .byte   %00000000 ; |        |            $ba06 (P)
    .byte   %00100000 ; |  *     |            $ba07 (P)
    .byte   %00010000 ; |   *    |            $ba08 (P)
    .byte   %00100000 ; |  *     |            $ba09 (P)
    .byte   %01000000 ; | *      |            $ba0a (P)
    .byte   %00000000 ; |        |            $ba0b (P)
    .byte   %00000000 ; |        |            $ba0c (P)
    .byte   %00000000 ; |        |            $ba0d (P)
    .byte   %00000000 ; |        |            $ba0e (P)
    .byte   %00000000 ; |        |            $ba0f (P)
    .byte   %00000000 ; |        |            $ba10 (P)
    .byte   %00000000 ; |        |            $ba11 (P)
    .byte   %00000000 ; |        |            $ba12 (P)
    .byte   %00001000 ; |    *   |            $ba13 (P)
    .byte   %01000100 ; | *   *  |            $ba14 (P)
    .byte   %10000000 ; |*       |            $ba15 (P)
    .byte   %00000011 ; |      **|            $ba16 (P)
    .byte   %00000110 ; |     ** |            $ba17 (P)
    .byte   %00000110 ; |     ** |            $ba18 (P)
    .byte   %00101101 ; |  * ** *|            $ba19 (P)
    .byte   %01111011 ; | **** **|            $ba1a (P)
    .byte   %11101111 ; |*** ****|            $ba1b (P)
    .byte   %00110110 ; |  ** ** |            $ba1c (P)
    .byte   %10011101 ; |*  *** *|            $ba1d (P)
    .byte   %01101001 ; | ** *  *|            $ba1e (P)
    .byte   %00100011 ; |  *   **|            $ba1f (P)
    .byte   %10010000 ; |*  *    |            $ba20 (P)
    .byte   %11001001 ; |**  *  *|            $ba21 (P)
    .byte   %00100100 ; |  *  *  |            $ba22 (P)
    .byte   %00000000 ; |        |            $ba23 (P)
    .byte   %01011100 ; | * ***  |            $ba24 (P)
    .byte   %00111000 ; |  ***   |            $ba25 (P)
    .byte   %00010000 ; |   *    |            $ba26 (P)
    .byte   %00100000 ; |  *     |            $ba27 (P)
    .byte   %00000000 ; |        |            $ba28 (P)
    .byte   %00000000 ; |        |            $ba29 (P)
    .byte   %00000000 ; |        |            $ba2a (P)
    .byte   %00000000 ; |        |            $ba2b (P)
    .byte   %00000000 ; |        |            $ba2c (P)
    .byte   %00000000 ; |        |            $ba2d (P)
    .byte   %00010100 ; |   * *  |            $ba2e (P)
    .byte   %00000000 ; |        |            $ba2f (P)
    .byte   %00000000 ; |        |            $ba30 (P)
    .byte   %00000000 ; |        |            $ba31 (P)
    .byte   %00000000 ; |        |            $ba32 (P)
    .byte   %00000000 ; |        |            $ba33 (P)
    .byte   %00000000 ; |        |            $ba34 (P)
    .byte   %00000000 ; |        |            $ba35 (P)
    .byte   %00000000 ; |        |            $ba36 (P)
    .byte   %00000000 ; |        |            $ba37 (P)
    .byte   %00000000 ; |        |            $ba38 (P)
    .byte   %00000000 ; |        |            $ba39 (P)
    .byte   %01000000 ; | *      |            $ba3a (P)
    .byte   %10000000 ; |*       |            $ba3b (P)
    .byte   %11010000 ; |** *    |            $ba3c (P)
    .byte   %01100000 ; | **     |            $ba3d (P)
    .byte   %10000000 ; |*       |            $ba3e (P)
    .byte   %01000000 ; | *      |            $ba3f (P)
    .byte   %00000000 ; |        |            $ba40 (P)
    .byte   %00000000 ; |        |            $ba41 (P)
    .byte   %00000000 ; |        |            $ba42 (P)
    .byte   %00000000 ; |        |            $ba43 (P)
    .byte   %00000000 ; |        |            $ba44 (P)
    .byte   %00000000 ; |        |            $ba45 (P)
    .byte   %00000000 ; |        |            $ba46 (P)
    .byte   %00000001 ; |       *|            $ba47 (P)
    .byte   %00000100 ; |     *  |            $ba48 (P)
    .byte   %00001000 ; |    *   |            $ba49 (P)
    .byte   %00000000 ; |        |            $ba4a (P)
    .byte   %00000000 ; |        |            $ba4b (P)
    .byte   %00000000 ; |        |            $ba4c (P)
    .byte   %00000000 ; |        |            $ba4d (P)
    .byte   %00000000 ; |        |            $ba4e (P)
    .byte   %00000000 ; |        |            $ba4f (P)
    .byte   %00000000 ; |        |            $ba50 (P)
    .byte   %00000000 ; |        |            $ba51 (P)
    .byte   %00000000 ; |        |            $ba52 (P)
    .byte   %00000000 ; |        |            $ba53 (P)
    .byte   %00000000 ; |        |            $ba54 (P)
    .byte   %00000000 ; |        |            $ba55 (P)
    .byte   %00000000 ; |        |            $ba56 (P)
    .byte   %00000000 ; |        |            $ba57 (P)
    .byte   %00000000 ; |        |            $ba58 (P)
    .byte   %00000000 ; |        |            $ba59 (P)
    .byte   %00000000 ; |        |            $ba5a (P)
    .byte   %00000000 ; |        |            $ba5b (P)
    .byte   %00000000 ; |        |            $ba5c (P)
    .byte   %00000000 ; |        |            $ba5d (P)
    .byte   %00000000 ; |        |            $ba5e (P)
    .byte   %00000000 ; |        |            $ba5f (P)
    .byte   %00000000 ; |        |            $ba60 (P)
    .byte   %00000000 ; |        |            $ba61 (P)
    .byte   %00000000 ; |        |            $ba62 (P)
    .byte   %00000000 ; |        |            $ba63 (P)
    .byte   %00000000 ; |        |            $ba64 (P)
    .byte   %00000000 ; |        |            $ba65 (P)
    .byte   %00000000 ; |        |            $ba66 (P)
    .byte   %00000000 ; |        |            $ba67 (P)
    .byte   %00000000 ; |        |            $ba68 (P)
    .byte   %00000000 ; |        |            $ba69 (P)
    .byte   %00000000 ; |        |            $ba6a (P)
    .byte   %00000000 ; |        |            $ba6b (P)
    .byte   %00000000 ; |        |            $ba6c (P)
    .byte   %00000000 ; |        |            $ba6d (P)
    .byte   %00000000 ; |        |            $ba6e (P)
    .byte   %00000111 ; |     ***|            $ba6f (P)
    .byte   %00001000 ; |    *   |            $ba70 (P)
    .byte   %00010000 ; |   *    |            $ba71 (P)
    .byte   %00100000 ; |  *     |            $ba72 (P)
    .byte   %01000100 ; | *   *  |            $ba73 (P)
    .byte   %00101000 ; |  * *   |            $ba74 (P)
    .byte   %00010001 ; |   *   *|            $ba75 (P)
    .byte   %00001010 ; |    * * |            $ba76 (P)
    .byte   %00000100 ; |     *  |            $ba77 (P)
    .byte   %00000010 ; |      * |            $ba78 (P)
    .byte   %00000001 ; |       *|            $ba79 (P)
    .byte   %00000000 ; |        |            $ba7a (P)
    .byte   %00000000 ; |        |            $ba7b (P)
    .byte   %00000000 ; |        |            $ba7c (P)
    .byte   %00000000 ; |        |            $ba7d (P)
    .byte   %00000000 ; |        |            $ba7e (P)
    .byte   %00000000 ; |        |            $ba7f (P)
    .byte   %00000000 ; |        |            $ba80 (P)
    .byte   %00000000 ; |        |            $ba81 (P)
    .byte   %00000000 ; |        |            $ba82 (P)
    .byte   %00000000 ; |        |            $ba83 (P)
    .byte   %00000000 ; |        |            $ba84 (P)
    .byte   %00000000 ; |        |            $ba85 (P)
    .byte   %00000000 ; |        |            $ba86 (P)
    .byte   %00000000 ; |        |            $ba87 (P)
    .byte   %00000000 ; |        |            $ba88 (P)
    .byte   %00000000 ; |        |            $ba89 (P)
    .byte   %00000000 ; |        |            $ba8a (P)
    .byte   %00000000 ; |        |            $ba8b (P)
    .byte   %00001000 ; |    *   |            $ba8c (P)
    .byte   %01000100 ; | *   *  |            $ba8d (P)
    .byte   %10000000 ; |*       |            $ba8e (P)
    .byte   %00000011 ; |      **|            $ba8f (P)
    .byte   %00000110 ; |     ** |            $ba90 (P)
    .byte   %00000110 ; |     ** |            $ba91 (P)
    .byte   %00101101 ; |  * ** *|            $ba92 (P)
    .byte   %01111011 ; | **** **|            $ba93 (P)
    .byte   %11101111 ; |*** ****|            $ba94 (P)
    .byte   %00110110 ; |  ** ** |            $ba95 (P)
    .byte   %10011101 ; |*  *** *|            $ba96 (P)
    .byte   %01101001 ; | ** *  *|            $ba97 (P)
    .byte   %00100011 ; |  *   **|            $ba98 (P)
    .byte   %10010000 ; |*  *    |            $ba99 (P)
    .byte   %11001001 ; |**  *  *|            $ba9a (P)
    .byte   %00100100 ; |  *  *  |            $ba9b (P)
    .byte   %00000000 ; |        |            $ba9c (P)
    .byte   %00000000 ; |        |            $ba9d (P)
    .byte   %00000000 ; |        |            $ba9e (P)
    .byte   %00000000 ; |        |            $ba9f (P)
    .byte   %00000000 ; |        |            $baa0 (P)
    .byte   %00000000 ; |        |            $baa1 (P)
    .byte   %00000000 ; |        |            $baa2 (P)
    .byte   %00000000 ; |        |            $baa3 (P)
    .byte   %00000000 ; |        |            $baa4 (P)
    .byte   %00000000 ; |        |            $baa5 (P)
    .byte   %00000000 ; |        |            $baa6 (P)
    .byte   %00010000 ; |   *    |            $baa7 (P)
    .byte   %00010000 ; |   *    |            $baa8 (P)
    .byte   %00010000 ; |   *    |            $baa9 (P)
    .byte   %00010000 ; |   *    |            $baaa (P)
    .byte   %00000000 ; |        |            $baab (P)
    .byte   %00000000 ; |        |            $baac (P)
    .byte   %00000000 ; |        |            $baad (P)
    .byte   %00000000 ; |        |            $baae (P)
    .byte   %00000000 ; |        |            $baaf (P)
    .byte   %00000000 ; |        |            $bab0 (P)
    .byte   %00000000 ; |        |            $bab1 (P)
    .byte   %10011001 ; |*  **  *|            $bab2 (P)
    .byte   %01100110 ; | **  ** |            $bab3 (P)
    .byte   %00000000 ; |        |            $bab4 (P)
    .byte   %00000000 ; |        |            $bab5 (P)
    .byte   %00000000 ; |        |            $bab6 (P)
    .byte   %00000000 ; |        |            $bab7 (P)
    .byte   %00000000 ; |        |            $bab8 (P)
    .byte   %00000000 ; |        |            $bab9 (P)
    .byte   %00000000 ; |        |            $baba (P)
    .byte   %00000000 ; |        |            $babb (P)
    .byte   %00110000 ; |  **    |            $babc (P)
    .byte   %00000000 ; |        |            $babd (P)
    .byte   %00110000 ; |  **    |            $babe (P)
    .byte   %00000000 ; |        |            $babf (P)
    .byte   %00000000 ; |        |            $bac0 (P)
    .byte   %00000000 ; |        |            $bac1 (P)
    .byte   %00110000 ; |  **    |            $bac2 (P)
    .byte   %00000000 ; |        |            $bac3 (P)
    .byte   %00110000 ; |  **    |            $bac4 (P)
    .byte   %00000000 ; |        |            $bac5 (P)
    .byte   %00000000 ; |        |            $bac6 (P)
    .byte   %00000000 ; |        |            $bac7 (P)
    
    .byte   $00                             ; $bac8 (*)
    
Lbac9
    .byte   GREEN|$2                        ; $bac9 (CP)
    .byte   BLACK|$f                        ; $baca (CP)
    .byte   GREEN|$0                        ; $bacb (CP)
    .byte   BLACK|$f                        ; $bacc (CP)
    .byte   BEIGE|$4                        ; $bacd (CP)
    .byte   BLACK|$f                        ; $bace (CP)
    .byte   GREEN|$2                        ; $bacf (CP)
    .byte   GREEN|$2                        ; $bad0 (CP)
    .byte   GREEN|$4                        ; $bad1 (CP)
    .byte   GREEN|$6                        ; $bad2 (CP)
    .byte   GREEN|$2                        ; $bad3 (CP)
    
    .byte   $c2,$c4                         ; $bad4 (*)
    
    .byte   BEIGE|$4                        ; $bad6 (CP)
    .byte   BEIGE|$4                        ; $bad7 (CP)
    .byte   BEIGE|$4                        ; $bad8 (CP)
    .byte   BLACK|$f                        ; $bad9 (CP)
    .byte   GREEN|$2                        ; $bada (CP)
    .byte   BEIGE|$4                        ; $badb (CP)
    .byte   GREEN|$0                        ; $badc (CP)
    .byte   GREEN|$0                        ; $badd (CP)
    .byte   BEIGE|$8                        ; $bade (CP)
    
    .byte   $c2,$00,$00,$00,$00,$00,$00,$00 ; $badf (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $bae7 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $baef (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $baf7 (*)
    .byte   $00                             ; $baff (*)
    
Lbb00
    .byte   %00000000 ; |        |            $bb00 (P)
    .byte   %00000000 ; |        |            $bb01 (P)
    .byte   %00000000 ; |        |            $bb02 (P)
    .byte   %00000000 ; |        |            $bb03 (P)
    .byte   %00000010 ; |      * |            $bb04 (P)
    .byte   %00000001 ; |       *|            $bb05 (P)
    .byte   %00000000 ; |        |            $bb06 (P)
    .byte   %00000000 ; |        |            $bb07 (P)
    .byte   %00000000 ; |        |            $bb08 (P)
    .byte   %00000000 ; |        |            $bb09 (P)
    .byte   %00000000 ; |        |            $bb0a (P)
    .byte   %00000100 ; |     *  |            $bb0b (P)
    .byte   %00001000 ; |    *   |            $bb0c (P)
    .byte   %00000100 ; |     *  |            $bb0d (P)
    .byte   %00000000 ; |        |            $bb0e (P)
    .byte   %00000000 ; |        |            $bb0f (P)
    .byte   %00000000 ; |        |            $bb10 (P)
    .byte   %00000000 ; |        |            $bb11 (P)
    .byte   %00000000 ; |        |            $bb12 (P)
    .byte   %00010000 ; |   *    |            $bb13 (P)
    .byte   %10001000 ; |*   *   |            $bb14 (P)
    .byte   %01000110 ; | *   ** |            $bb15 (P)
    .byte   %01101111 ; | ** ****|            $bb16 (P)
    .byte   %11011110 ; |** **** |            $bb17 (P)
    .byte   %10111001 ; |* ***  *|            $bb18 (P)
    .byte   %01110100 ; | *** *  |            $bb19 (P)
    .byte   %10011010 ; |*  ** * |            $bb1a (P)
    .byte   %00111100 ; |  ****  |            $bb1b (P)
    .byte   %01101001 ; | ** *  *|            $bb1c (P)
    .byte   %11010110 ; |** * ** |            $bb1d (P)
    .byte   %11111011 ; |***** **|            $bb1e (P)
    .byte   %01111101 ; | ***** *|            $bb1f (P)
    .byte   %11010110 ; |** * ** |            $bb20 (P)
    .byte   %10111010 ; |* *** * |            $bb21 (P)
    .byte   %01100000 ; | **     |            $bb22 (P)
    .byte   %00000000 ; |        |            $bb23 (P)
    .byte   %01000000 ; | *      |            $bb24 (P)
    .byte   %10100000 ; |* *     |            $bb25 (P)
    .byte   %11100000 ; |***     |            $bb26 (P)
    .byte   %01110000 ; | ***    |            $bb27 (P)
    .byte   %10100000 ; |* *     |            $bb28 (P)
    .byte   %01010000 ; | * *    |            $bb29 (P)
    .byte   %10100000 ; |* *     |            $bb2a (P)
    .byte   %11100000 ; |***     |            $bb2b (P)
    .byte   %01000000 ; | *      |            $bb2c (P)
    .byte   %10000000 ; |*       |            $bb2d (P)
    .byte   %00000000 ; |        |            $bb2e (P)
    .byte   %00000000 ; |        |            $bb2f (P)
    .byte   %00000000 ; |        |            $bb30 (P)
    .byte   %00000000 ; |        |            $bb31 (P)
    .byte   %00000000 ; |        |            $bb32 (P)
    .byte   %00000000 ; |        |            $bb33 (P)
    .byte   %00000000 ; |        |            $bb34 (P)
    .byte   %00000000 ; |        |            $bb35 (P)
    .byte   %00000000 ; |        |            $bb36 (P)
    .byte   %00000000 ; |        |            $bb37 (P)
    .byte   %00000000 ; |        |            $bb38 (P)
    .byte   %00000000 ; |        |            $bb39 (P)
    .byte   %00000000 ; |        |            $bb3a (P)
    .byte   %00000000 ; |        |            $bb3b (P)
    .byte   %00000000 ; |        |            $bb3c (P)
    .byte   %00000010 ; |      * |            $bb3d (P)
    .byte   %00000101 ; |     * *|            $bb3e (P)
    .byte   %00000010 ; |      * |            $bb3f (P)
    .byte   %00000001 ; |       *|            $bb40 (P)
    .byte   %00000000 ; |        |            $bb41 (P)
    .byte   %00000000 ; |        |            $bb42 (P)
    .byte   %00000000 ; |        |            $bb43 (P)
    .byte   %01000000 ; | *      |            $bb44 (P)
    .byte   %01100000 ; | **     |            $bb45 (P)
    .byte   %11010000 ; |** *    |            $bb46 (P)
    .byte   %10101000 ; |* * *   |            $bb47 (P)
    .byte   %01110000 ; | ***    |            $bb48 (P)
    .byte   %00010000 ; |   *    |            $bb49 (P)
    .byte   %00000000 ; |        |            $bb4a (P)
    .byte   %00000000 ; |        |            $bb4b (P)
    .byte   %00000000 ; |        |            $bb4c (P)
    .byte   %00000000 ; |        |            $bb4d (P)
    .byte   %00000000 ; |        |            $bb4e (P)
    .byte   %00000000 ; |        |            $bb4f (P)
    .byte   %00000000 ; |        |            $bb50 (P)
    .byte   %00000000 ; |        |            $bb51 (P)
    .byte   %00000000 ; |        |            $bb52 (P)
    .byte   %00000000 ; |        |            $bb53 (P)
    .byte   %00000000 ; |        |            $bb54 (P)
    .byte   %00000000 ; |        |            $bb55 (P)
    .byte   %00000000 ; |        |            $bb56 (P)
    .byte   %00000000 ; |        |            $bb57 (P)
    .byte   %00000000 ; |        |            $bb58 (P)
    .byte   %00000000 ; |        |            $bb59 (P)
    .byte   %00000000 ; |        |            $bb5a (P)
    .byte   %00000000 ; |        |            $bb5b (P)
    .byte   %00000000 ; |        |            $bb5c (P)
    .byte   %00000000 ; |        |            $bb5d (P)
    .byte   %00000000 ; |        |            $bb5e (P)
    .byte   %00000000 ; |        |            $bb5f (P)
    .byte   %00000000 ; |        |            $bb60 (P)
    .byte   %00000000 ; |        |            $bb61 (P)
    .byte   %00000000 ; |        |            $bb62 (P)
    .byte   %10000000 ; |*       |            $bb63 (P)
    .byte   %01000000 ; | *      |            $bb64 (P)
    .byte   %01000000 ; | *      |            $bb65 (P)
    .byte   %01000000 ; | *      |            $bb66 (P)
    .byte   %01000000 ; | *      |            $bb67 (P)
    .byte   %01000000 ; | *      |            $bb68 (P)
    .byte   %00100000 ; |  *     |            $bb69 (P)
    .byte   %00010000 ; |   *    |            $bb6a (P)
    .byte   %00010000 ; |   *    |            $bb6b (P)
    .byte   %00100000 ; |  *     |            $bb6c (P)
    .byte   %01000000 ; | *      |            $bb6d (P)
    .byte   %01000000 ; | *      |            $bb6e (P)
    .byte   %00111111 ; |  ******|            $bb6f (P)
    .byte   %00000000 ; |        |            $bb70 (P)
    .byte   %00000000 ; |        |            $bb71 (P)
    .byte   %00000000 ; |        |            $bb72 (P)
    .byte   %00000000 ; |        |            $bb73 (P)
    .byte   %00000001 ; |       *|            $bb74 (P)
    .byte   %00001000 ; |    *   |            $bb75 (P)
    .byte   %00000100 ; |     *  |            $bb76 (P)
    .byte   %00100010 ; |  *   * |            $bb77 (P)
    .byte   %01010001 ; | * *   *|            $bb78 (P)
    .byte   %00001000 ; |    *   |            $bb79 (P)
    .byte   %01000101 ; | *   * *|            $bb7a (P)
    .byte   %01000010 ; | *    * |            $bb7b (P)
    .byte   %01000000 ; | *      |            $bb7c (P)
    .byte   %00100000 ; |  *     |            $bb7d (P)
    .byte   %00100000 ; |  *     |            $bb7e (P)
    .byte   %00100000 ; |  *     |            $bb7f (P)
    .byte   %00010000 ; |   *    |            $bb80 (P)
    .byte   %00010000 ; |   *    |            $bb81 (P)
    .byte   %01001000 ; | *  *   |            $bb82 (P)
    .byte   %01001000 ; | *  *   |            $bb83 (P)
    .byte   %01000100 ; | *   *  |            $bb84 (P)
    .byte   %00100100 ; |  *  *  |            $bb85 (P)
    .byte   %00100010 ; |  *   * |            $bb86 (P)
    .byte   %00010010 ; |   *  * |            $bb87 (P)
    .byte   %10010100 ; |*  * *  |            $bb88 (P)
    .byte   %01001000 ; | *  *   |            $bb89 (P)
    .byte   %01010000 ; | * *    |            $bb8a (P)
    .byte   %00000000 ; |        |            $bb8b (P)
    .byte   %00010000 ; |   *    |            $bb8c (P)
    .byte   %10001000 ; |*   *   |            $bb8d (P)
    .byte   %01000110 ; | *   ** |            $bb8e (P)
    .byte   %01101111 ; | ** ****|            $bb8f (P)
    .byte   %11011110 ; |** **** |            $bb90 (P)
    .byte   %10111001 ; |* ***  *|            $bb91 (P)
    .byte   %01110100 ; | *** *  |            $bb92 (P)
    .byte   %10011010 ; |*  ** * |            $bb93 (P)
    .byte   %00111100 ; |  ****  |            $bb94 (P)
    .byte   %01101001 ; | ** *  *|            $bb95 (P)
    .byte   %11010110 ; |** * ** |            $bb96 (P)
    .byte   %11111011 ; |***** **|            $bb97 (P)
    .byte   %01111101 ; | ***** *|            $bb98 (P)
    .byte   %11010110 ; |** * ** |            $bb99 (P)
    .byte   %10111010 ; |* *** * |            $bb9a (P)
    .byte   %01100000 ; | **     |            $bb9b (P)
    .byte   %00000000 ; |        |            $bb9c (P)
    .byte   %00000000 ; |        |            $bb9d (P)
    .byte   %00000000 ; |        |            $bb9e (P)
    .byte   %00000000 ; |        |            $bb9f (P)
    .byte   %00000000 ; |        |            $bba0 (P)
    .byte   %00000000 ; |        |            $bba1 (P)
    .byte   %00000000 ; |        |            $bba2 (P)
    .byte   %00000000 ; |        |            $bba3 (P)
    .byte   %00000000 ; |        |            $bba4 (P)
    .byte   %00000000 ; |        |            $bba5 (P)
    .byte   %00000000 ; |        |            $bba6 (P)
    .byte   %00100001 ; |  *    *|            $bba7 (P)
    .byte   %00100001 ; |  *    *|            $bba8 (P)
    .byte   %00100001 ; |  *    *|            $bba9 (P)
    .byte   %00100001 ; |  *    *|            $bbaa (P)
    .byte   %00000000 ; |        |            $bbab (P)
    .byte   %00000000 ; |        |            $bbac (P)
    .byte   %00000000 ; |        |            $bbad (P)
    .byte   %00000000 ; |        |            $bbae (P)
    .byte   %00000000 ; |        |            $bbaf (P)
    .byte   %00000000 ; |        |            $bbb0 (P)
    .byte   %00000000 ; |        |            $bbb1 (P)
    .byte   %01010101 ; | * * * *|            $bbb2 (P)
    .byte   %10101010 ; |* * * * |            $bbb3 (P)
    .byte   %00000000 ; |        |            $bbb4 (P)
    .byte   %00000000 ; |        |            $bbb5 (P)
    .byte   %00000000 ; |        |            $bbb6 (P)
    .byte   %00000000 ; |        |            $bbb7 (P)
    .byte   %00000000 ; |        |            $bbb8 (P)
    .byte   %00000000 ; |        |            $bbb9 (P)
    .byte   %00000000 ; |        |            $bbba (P)
    .byte   %00000000 ; |        |            $bbbb (P)
    .byte   %00110000 ; |  **    |            $bbbc (P)
    .byte   %00000000 ; |        |            $bbbd (P)
    .byte   %00110000 ; |  **    |            $bbbe (P)
    .byte   %00000000 ; |        |            $bbbf (P)
    .byte   %00000000 ; |        |            $bbc0 (P)
    .byte   %00000000 ; |        |            $bbc1 (P)
    .byte   %00110000 ; |  **    |            $bbc2 (P)
    .byte   %00000000 ; |        |            $bbc3 (P)
    .byte   %00110000 ; |  **    |            $bbc4 (P)
    .byte   %00000000 ; |        |            $bbc5 (P)
    .byte   %00000000 ; |        |            $bbc6 (P)
    .byte   %00000000 ; |        |            $bbc7 (P)
    
    .byte   $00                             ; $bbc8 (*)
Lbbc9
    .byte   $2c,$09,$07,$0c,$27,$09,$5f,$0e ; $bbc9 (D)
    .byte   $0d,$0e,$09                     ; $bbd1 (D)
    .byte   $09,$09                         ; $bbd4 (*)
    .byte   $07,$0d,$05,$06,$03,$03,$03,$03 ; $bbd6 (D)
    .byte   $05                             ; $bbde (D)
    .byte   $08,$00,$00,$00,$00,$00,$00,$00 ; $bbdf (*)
    .byte   $00,$00                         ; $bbe7 (*)
  IF PLUSROM
PlusROM_API
       .byte "a", 0, "h.firmaplus.de"
       .byte 0
  ENDIF

    ORG     $1c00, $00
    RORG    $bc00, $00

Lbc00
    .byte   GREEN|$0                        ; $bc00 (CB)
    .byte   GREEN|$0                        ; $bc01 (CB)
    .byte   GREEN|$0                        ; $bc02 (CB)
    .byte   GREEN|$0                        ; $bc03 (CB)
    .byte   GREEN|$0                        ; $bc04 (CB)
    .byte   GREEN|$0                        ; $bc05 (CB)
    .byte   GREEN|$0                        ; $bc06 (CB)
    .byte   GREEN|$0                        ; $bc07 (CB)
    .byte   GREEN|$0                        ; $bc08 (CB)
    .byte   GREEN|$0                        ; $bc09 (CB)
    .byte   GREEN|$0                        ; $bc0a (CB)
    .byte   GREEN|$0                        ; $bc0b (CB)
    .byte   GREEN|$0                        ; $bc0c (CB)
    .byte   GREEN|$0                        ; $bc0d (CB)
    .byte   GREEN|$0                        ; $bc0e (CB)
    .byte   GREEN|$0                        ; $bc0f (CB)
    .byte   GREEN|$0                        ; $bc10 (CB)
    .byte   GREEN|$0                        ; $bc11 (CB)
    .byte   GREEN|$0                        ; $bc12 (CB)
    .byte   GREEN|$0                        ; $bc13 (CB)
    .byte   GREEN|$0                        ; $bc14 (CB)
    .byte   GREEN|$0                        ; $bc15 (CB)
    .byte   GREEN|$0                        ; $bc16 (CB)
    .byte   GREEN|$0                        ; $bc17 (CB)
    .byte   GREEN|$0                        ; $bc18 (CB)
    .byte   GREEN|$0                        ; $bc19 (CB)
    .byte   GREEN|$0                        ; $bc1a (CB)
    .byte   GREEN|$0                        ; $bc1b (CB)
    .byte   GREEN|$0                        ; $bc1c (CB)
    .byte   GREEN|$0                        ; $bc1d (CB)
    .byte   GREEN|$0                        ; $bc1e (CB)
    .byte   GREEN|$0                        ; $bc1f (CB)
    .byte   GREEN|$0                        ; $bc20 (CB)
    .byte   GREEN|$0                        ; $bc21 (CB)
    .byte   GREEN|$0                        ; $bc22 (CB)
    .byte   GREEN|$0                        ; $bc23 (CB)
    .byte   GREEN|$0                        ; $bc24 (CB)
    .byte   GREEN|$0                        ; $bc25 (CB)
    .byte   GREEN|$0                        ; $bc26 (CB)
    .byte   GREEN|$0                        ; $bc27 (CB)
    .byte   GREEN|$0                        ; $bc28 (CB)
    .byte   GREEN|$0                        ; $bc29 (CB)
    .byte   GREEN|$0                        ; $bc2a (CB)
    .byte   GREEN|$0                        ; $bc2b (CB)
    .byte   GREEN|$0                        ; $bc2c (CB)
    .byte   GREEN|$0                        ; $bc2d (CB)
    .byte   GREEN|$0                        ; $bc2e (CB)
    .byte   GREEN|$0                        ; $bc2f (CB)
    .byte   BEIGE|$0                        ; $bc30 (CB)
    .byte   BLUE|$2                         ; $bc31 (CB)
    .byte   BLUE|$0                         ; $bc32 (CB)
    .byte   BLUE|$0                         ; $bc33 (CB)
    .byte   BLUE|$0                         ; $bc34 (CB)
    .byte   BLUE|$0                         ; $bc35 (CB)
    .byte   BLUE|$0                         ; $bc36 (CB)
    .byte   BLUE|$0                         ; $bc37 (CB)
    .byte   BLUE|$2                         ; $bc38 (CB)
    .byte   BEIGE|$0                        ; $bc39 (CB)
    .byte   GREEN|$0                        ; $bc3a (CB)
    .byte   GREEN|$0                        ; $bc3b (CB)
    .byte   GREEN|$0                        ; $bc3c (CB)
    .byte   GREEN|$0                        ; $bc3d (CB)
    .byte   GREEN|$0                        ; $bc3e (CB)
    .byte   GREEN|$0                        ; $bc3f (CB)
    .byte   GREEN|$0                        ; $bc40 (CB)
    .byte   GREEN|$0                        ; $bc41 (CB)
    .byte   GREEN|$0                        ; $bc42 (CB)
    .byte   GREEN|$0                        ; $bc43 (CB)
    .byte   GREEN|$0                        ; $bc44 (CB)
    .byte   GREEN|$0                        ; $bc45 (CB)
    .byte   GREEN|$0                        ; $bc46 (CB)
    .byte   GREEN|$0                        ; $bc47 (CB)
    .byte   GREEN|$0                        ; $bc48 (CB)
    .byte   GREEN|$0                        ; $bc49 (CB)
    .byte   BEIGE|$0                        ; $bc4a (CB)
    .byte   BEIGE|$2                        ; $bc4b (CB)
    .byte   BEIGE|$4                        ; $bc4c (CB)
    .byte   BLUE|$6                         ; $bc4d (CB)
    .byte   BLUE|$4                         ; $bc4e (CB)
    .byte   BLUE|$2                         ; $bc4f (CB)
    .byte   BLUE|$0                         ; $bc50 (CB)
    .byte   BLUE|$0                         ; $bc51 (CB)
    .byte   BLUE|$0                         ; $bc52 (CB)
    .byte   BLUE|$0                         ; $bc53 (CB)
    .byte   BLUE|$0                         ; $bc54 (CB)
    .byte   BLUE|$0                         ; $bc55 (CB)
    .byte   BLUE|$0                         ; $bc56 (CB)
    .byte   BLUE|$0                         ; $bc57 (CB)
    .byte   BLUE|$0                         ; $bc58 (CB)
    .byte   BLUE|$0                         ; $bc59 (CB)
    .byte   BLUE|$0                         ; $bc5a (CB)
    .byte   BLUE|$0                         ; $bc5b (CB)
    .byte   BLUE|$2                         ; $bc5c (CB)
    .byte   BLUE|$4                         ; $bc5d (CB)
    .byte   BLUE|$6                         ; $bc5e (CB)
    .byte   BEIGE|$6                        ; $bc5f (CB)
    .byte   BEIGE|$4                        ; $bc60 (CB)
    .byte   BEIGE|$2                        ; $bc61 (CB)
    .byte   GREEN|$0                        ; $bc62 (CB)
    .byte   GREEN|$0                        ; $bc63 (CB)
    .byte   GREEN|$0                        ; $bc64 (CB)
    .byte   GREEN|$0                        ; $bc65 (CB)
    .byte   GREEN|$0                        ; $bc66 (CB)
    .byte   GREEN|$0                        ; $bc67 (CB)
    .byte   GREEN|$0                        ; $bc68 (CB)
    .byte   GREEN|$0                        ; $bc69 (CB)
    .byte   GREEN|$0                        ; $bc6a (CB)
    .byte   GREEN|$0                        ; $bc6b (CB)
    .byte   GREEN|$0                        ; $bc6c (CB)
    .byte   GREEN|$0                        ; $bc6d (CB)
    .byte   GREEN|$0                        ; $bc6e (CB)
    .byte   GREEN|$0                        ; $bc6f (CB)
    .byte   GREEN|$0                        ; $bc70 (CB)
    .byte   GREEN|$0                        ; $bc71 (CB)
    .byte   GREEN|$0                        ; $bc72 (CB)
    .byte   GREEN|$0                        ; $bc73 (CB)
    .byte   GREEN|$0                        ; $bc74 (CB)
    .byte   GREEN|$0                        ; $bc75 (CB)
    .byte   GREEN|$0                        ; $bc76 (CB)
    .byte   GREEN|$0                        ; $bc77 (CB)
    .byte   GREEN|$0                        ; $bc78 (CB)
    .byte   GREEN|$0                        ; $bc79 (CB)
    .byte   GREEN|$0                        ; $bc7a (CB)
    .byte   GREEN|$0                        ; $bc7b (CB)
    .byte   GREEN|$0                        ; $bc7c (CB)
    .byte   GREEN|$0                        ; $bc7d (CB)
    .byte   GREEN|$0                        ; $bc7e (CB)
    .byte   GREEN|$0                        ; $bc7f (CB)
    .byte   GREEN|$0                        ; $bc80 (CB)
    .byte   GREEN|$0                        ; $bc81 (CB)
    .byte   GREEN|$0                        ; $bc82 (CB)
    .byte   GREEN|$0                        ; $bc83 (CB)
    .byte   GREEN|$0                        ; $bc84 (CB)
    .byte   GREEN|$0                        ; $bc85 (CB)
    .byte   GREEN|$0                        ; $bc86 (CB)
    .byte   GREEN|$0                        ; $bc87 (CB)
    .byte   GREEN|$0                        ; $bc88 (CB)
    .byte   GREEN|$0                        ; $bc89 (CB)
    .byte   GREEN|$0                        ; $bc8a (CB)
    .byte   GREEN|$0                        ; $bc8b (CB)
    .byte   BEIGE|$2                        ; $bc8c (CB)
    .byte   BEIGE|$2                        ; $bc8d (CB)
    .byte   BEIGE|$2                        ; $bc8e (CB)
    .byte   BEIGE|$2                        ; $bc8f (CB)
    .byte   BEIGE|$2                        ; $bc90 (CB)
    .byte   BEIGE|$2                        ; $bc91 (CB)
    .byte   BEIGE|$2                        ; $bc92 (CB)
    .byte   BEIGE|$2                        ; $bc93 (CB)
    .byte   BEIGE|$2                        ; $bc94 (CB)
    .byte   BEIGE|$2                        ; $bc95 (CB)
    .byte   BEIGE|$2                        ; $bc96 (CB)
    .byte   BEIGE|$2                        ; $bc97 (CB)
    .byte   BEIGE|$2                        ; $bc98 (CB)
    .byte   BEIGE|$2                        ; $bc99 (CB)
    .byte   BEIGE|$2                        ; $bc9a (CB)
    .byte   BEIGE|$2                        ; $bc9b (CB)
    .byte   BEIGE|$2                        ; $bc9c (CB)
    .byte   GREEN|$0                        ; $bc9d (CB)
    .byte   BEIGE|$2                        ; $bc9e (CB)
    .byte   BEIGE|$2                        ; $bc9f (CB)
    .byte   BEIGE|$2                        ; $bca0 (CB)
    .byte   BEIGE|$2                        ; $bca1 (CB)
    .byte   BEIGE|$2                        ; $bca2 (CB)
    .byte   BEIGE|$2                        ; $bca3 (CB)
    .byte   BEIGE|$2                        ; $bca4 (CB)
    .byte   BLACK|$0                        ; $bca5 (CB)
    .byte   BLACK|$2                        ; $bca6 (CB)
    .byte   BLACK|$2                        ; $bca7 (CB)
    .byte   BLACK|$2                        ; $bca8 (CB)
    .byte   BLACK|$2                        ; $bca9 (CB)
    .byte   BLACK|$2                        ; $bcaa (CB)
    .byte   BLACK|$2                        ; $bcab (CB)
    .byte   BLACK|$0                        ; $bcac (CB)
    .byte   BLACK|$0                        ; $bcad (CB)
    .byte   BLACK|$2                        ; $bcae (CB)
    .byte   BLACK|$2                        ; $bcaf (CB)
    .byte   BLACK|$2                        ; $bcb0 (CB)
    .byte   BLACK|$2                        ; $bcb1 (CB)
    .byte   BLACK|$2                        ; $bcb2 (CB)
    .byte   BLACK|$2                        ; $bcb3 (CB)
    .byte   BLACK|$2                        ; $bcb4 (CB)
    .byte   BLACK|$2                        ; $bcb5 (CB)
    .byte   BLACK|$2                        ; $bcb6 (CB)
    .byte   BLACK|$2                        ; $bcb7 (CB)
    .byte   BLACK|$0                        ; $bcb8 (CB)
    .byte   BLACK|$2                        ; $bcb9 (CB)
    .byte   BLACK|$0                        ; $bcba (CB)
    .byte   BLACK|$2                        ; $bcbb (CB)
    .byte   BLACK|$2                        ; $bcbc (CB)
    .byte   BLACK|$2                        ; $bcbd (CB)
    .byte   BLACK|$2                        ; $bcbe (CB)
    .byte   BLACK|$2                        ; $bcbf (CB)
    .byte   BLACK|$0                        ; $bcc0 (CB)
    .byte   BLACK|$2                        ; $bcc1 (CB)
    .byte   BLACK|$2                        ; $bcc2 (CB)
    .byte   BLACK|$2                        ; $bcc3 (CB)
    .byte   BLACK|$2                        ; $bcc4 (CB)
    .byte   BLACK|$2                        ; $bcc5 (CB)
    .byte   BLACK|$0                        ; $bcc6 (CB)
    .byte   BLACK|$2                        ; $bcc7 (CB)
    
    .byte   $c0                             ; $bcc8 (*)
Lbcc9
    .byte   $01,$ad,$30,$b9,$62,$50,$01,$13 ; $bcc9 (D)
    .byte   $3a,$00,$13                     ; $bcd1 (D)
    .byte   $3a,$00                         ; $bcd4 (*)
    .byte   $8c,$8c,$9d,$b9,$1e,$97,$4a,$5c ; $bcd6 (D)
    .byte   $a5                             ; $bcde (D)
    .byte   $3a                             ; $bcdf (*)
Lbce0
    .byte   $00,$00,$02,$06,$00,$00,$06,$00 ; $bce0 (*)
    .byte   $05                             ; $bce8 (*)
    .byte   $05                             ; $bce9 (D)
Lbcea
    .byte   $00,$0d,$17,$00,$00,$00,$00,$e2 ; $bcea (*)
    .byte   $26                             ; $bcf2 (*)
    .byte   $6c                             ; $bcf3 (D)
    .byte   $00                             ; $bcf4 (*)
Lbcf5
    .byte   $00,$08,$12,$30,$a1,$a8,$0f,$af ; $bcf5 (*)
    .byte   $1c                             ; $bcfd (*)
    .byte   $37                             ; $bcfe (D)
Lbcff
    .byte   $00,$84,$84,$88,$88,$85,$8d,$8d ; $bcff (*)
    .byte   $88                             ; $bd07 (*)
    .byte   $8d                             ; $bd08 (A)
    .byte   $01                             ; $bd09 (*)
    
Lbd0a
    cpy     ram_98                  ;3        
    bcc     Lbd1e                   ;2/3      
    sty     ram_98                  ;3        
    lda     Lbcf5,y                 ;4        
    sta     ram_9C                  ;3        
    lda     Lbcea,y                 ;4        
    sta     ram_99                  ;3        
    lda     #$00                    ;2        
    sta     ram_9A                  ;3   =  27
Lbd1e
    jmp     $f018                   ;3   =   3
    
Lbd21
    ldx     ram_98                  ;3        
    lda     Lbcff,x                 ;4        
    bmi     Lbd3b                   ;2/3      
    sta     AUDC0                   ;3         *
    lda     Lbcf5,x                 ;4         *
    sta     AUDV0                   ;3         *
    lda     Lbce0,x                 ;4         *
    sta     AUDF0                   ;3         *
    lda     #$00                    ;2         *
    sta     ram_98                  ;3         *
    jmp     Lbd58                   ;3   =  34 *
    
Lbd3b
    ldx     ram_98                  ;3        
    dec     ram_9A                  ;5        
    bpl     Lbd51                   ;2/3      
    inc     ram_99                  ;5        
    inc     ram_9C                  ;5        
    lda     Lbce0,x                 ;4        
    sta     ram_9A                  ;3        
    lda     Lbcff,x                 ;4        
    sta     AUDC0                   ;3        
    sta     AUDC1                   ;3   =  37
Lbd51
    ldy     ram_9C                  ;3        
    lda     Lbdd2,y                 ;4        
    bne     Lbd70                   ;2/3 =   9
Lbd58
    sta     ram_9C                  ;3         *
    sta     ram_98                  ;3         *
    lda     #$04                    ;2         *
    sta     AUDC0                   ;3         *
    lda     ram_9D                  ;3         *
    bne     Lbd70                   ;2/3       *
    lda     ram_91                  ;3         *
    and     #$06                    ;2         *
    beq     Lbd70                   ;2/3       *
    lda     #$09                    ;2         *
    sta     AUDC0                   ;3         *
    lda     #$8f                    ;2   =  30 *
Lbd70
    sta     AUDF0                   ;3        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    ora     #$01                    ;2        
    sta     AUDV0                   ;3        
    ldy     ram_99                  ;3        
    lda     Lbdd2,y                 ;4        
    bne     Lbd8f                   ;2/3      
    sta     ram_99                  ;3         *
    lda     ram_9B                  ;3         *
    and     #$1f                    ;2         *
    tax                             ;2         *
    lda     #$04                    ;2         *
    sta     AUDC1                   ;3         *
    lda     Lbdb1,x                 ;4   =  44 *
Lbd8f
    sta     AUDF1                   ;3        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    ora     #$01                    ;2        
    sta     AUDV1                   ;3        
    lda     ram_9B                  ;3        
    clc                             ;2        
    adc     #$20                    ;2        
    bcc     Lbda6                   ;2/3      
    adc     #$00                    ;2        
    and     #$1f                    ;2        
    ora     #$60                    ;2   =  31
Lbda6
    sta     ram_9B                  ;3        
    lda     ram_9D                  ;3        
    beq     Lbdae                   ;2/3      
    inc     ram_9D                  ;5   =  13
Lbdae
    jmp     $f00c                   ;3   =   3
    
Lbdb1
    .byte   $3d,$4e,$4f,$4e,$4b,$4e,$4f,$4e ; $bdb1 (*)
    .byte   $3d,$4e,$50,$4e,$4b,$4e,$50,$4e ; $bdb9 (*)
    .byte   $3d,$4e,$51,$4e,$4b,$4e,$51,$4e ; $bdc1 (*)
    .byte   $3d,$4e,$52,$4e,$4b,$4e,$52,$4e ; $bdc9 (*)
    .byte   $00                             ; $bdd1 (*)
Lbdd2
    .byte   $00,$00,$a7,$00,$a8,$a8,$a8,$00 ; $bdd2 (*)
    .byte   $a4,$a7,$aa,$af,$00,$9f,$98,$94 ; $bdda (*)
    .byte   $92,$00,$ab,$ae,$a9,$ac,$00,$af ; $bde2 (*)
    .byte   $b2,$ae,$b1,$00,$fe,$de,$be,$7e ; $bdea (*)
    .byte   $7e,$5e,$5e,$3e,$3e,$00,$ff,$df ; $bdf2 (*)
    .byte   $bf,$7f,$6f,$5f,$5f,$3f,$3f,$00 ; $bdfa (*)
    .byte   $b6,$96,$76,$56,$36,$36,$00,$b2 ; $be02 (*)
    .byte   $b2                             ; $be0a (A)
    
    .byte   %10110010 ; |# ##  # |            $be0b (G)
    .byte   %10110010 ; |# ##  # |            $be0c (G)
    .byte   %10110010 ; |# ##  # |            $be0d (G)
    .byte   %00000001 ; |       #|            $be0e (G)
    .byte   %10101111 ; |# # ####|            $be0f (G)
    .byte   %10110010 ; |# ##  # |            $be10 (G)
    .byte   %10101111 ; |# # ####|            $be11 (G)
    .byte   %10101011 ; |# # # ##|            $be12 (G)
    .byte   %10101111 ; |# # ####|            $be13 (G)
    .byte   %00000001 ; |       #|            $be14 (G)
    .byte   %10110010 ; |# ##  # |            $be15 (G)
    .byte   %10110001 ; |# ##   #|            $be16 (G)
    
    .byte   $01,$b1,$b1,$b1,$b1,$ad,$ad,$ad ; $be17 (*)
    .byte   $ad,$b1,$b1,$b1,$b1,$b2,$b2,$b2 ; $be1f (*)
    .byte   $b2,$01,$af,$b2,$af,$ab,$af,$01 ; $be27 (*)
    .byte   $b2,$b1,$01,$b1,$b1,$b1,$b1,$b2 ; $be2f (*)
    .byte   $b2,$b2,$b2,$b2,$b2,$b2,$00,$bf ; $be37 (*)
    .byte   $bf                             ; $be3f (A)
    .byte   $bf                             ; $be40 (A)
    .byte   $bf                             ; $be41 (A)
    .byte   $bf                             ; $be42 (A)
    .byte   $01                             ; $be43 (A)
    .byte   $b7                             ; $be44 (A)
    .byte   $bf                             ; $be45 (A)
    .byte   $b7,$b2,$b7,$01,$bf,$bb,$01,$bb ; $be46 (*)
    .byte   $bb,$bb,$bb,$b4,$b4,$b4,$b4,$bb ; $be4e (*)
    .byte   $bb,$bb,$bb,$bf,$bf,$bf,$bf,$01 ; $be56 (*)
    .byte   $b7,$bf,$b7,$b2,$b7,$01,$bf,$bb ; $be5e (*)
    .byte   $01,$bb,$bb,$bb,$bb,$bf,$bf,$bf ; $be66 (*)
    .byte   $bf,$bf,$bf,$bf,$00,$38,$56,$73 ; $be6e (*)
    .byte   $8f,$aa,$c4,$00,$b0,$ae,$8c,$6a ; $be76 (*)
    .byte   $48,$28,$00,$ad,$ed,$ed,$ed,$ed ; $be7e (*)
    .byte   $ad,$ad,$b0,$f0,$f0,$f0,$f0,$b0 ; $be86 (*)
    .byte   $b0,$b4,$f4,$f4,$f4,$f4,$b4,$b4 ; $be8e (*)
    .byte   $b4,$b4,$94,$94,$94,$54,$54,$54 ; $be96 (*)
    .byte   $f4,$f4,$f4,$f4,$b4,$b4,$b0,$f0 ; $be9e (*)
    .byte   $f0,$f0,$f0,$b0,$b0,$ad,$ed,$ed ; $bea6 (*)
    .byte   $ed,$ed,$ad,$ad,$ad,$00,$b0,$f0 ; $beae (*)
    .byte   $f0,$f0,$f0,$b0,$b0,$b4,$f4,$f4 ; $beb6 (*)
    .byte   $f4,$f4,$b4,$b4,$bb,$fb,$fb,$fb ; $bebe (*)
    .byte   $fb,$bb,$bb,$bb,$fb,$fb,$fb,$fb ; $bec6 (*)
    .byte   $bb,$bb,$bb,$fb,$fb,$fb,$fb,$bb ; $bece (*)
    .byte   $bb,$b4,$f4,$f4,$f4,$f4,$b4,$b4 ; $bed6 (*)
    .byte   $b0,$f0,$f0,$f0,$f0,$b0,$b0,$b0 ; $bede (*)
    .byte   $00                             ; $bee6 (*)
    
    .byte   %00111000 ; |  ###   |            $bee7 (G)
    .byte   %01111100 ; | #####  |            $bee8 (G)
    .byte   %11111110 ; |####### |            $bee9 (G)
    .byte   %11111110 ; |####### |            $beea (G)
    .byte   %11010110 ; |## # ## |            $beeb (G)
    .byte   %11101110 ; |### ### |            $beec (G)
    .byte   %11101110 ; |### ### |            $beed (G)
    .byte   %11010110 ; |## # ## |            $beee (G)
    .byte   %11111110 ; |####### |            $beef (G)
    .byte   %11111110 ; |####### |            $bef0 (G)
    .byte   %01111100 ; | #####  |            $bef1 (G)
    .byte   %00111000 ; |  ###   |            $bef2 (G)
    .byte   %00111000 ; |  ###   |            $bef3 (G)
    .byte   %01111100 ; | #####  |            $bef4 (G)
    .byte   %11111110 ; |####### |            $bef5 (G)
    .byte   %11010110 ; |## # ## |            $bef6 (G)
    .byte   %10111010 ; |# ### # |            $bef7 (G)
    .byte   %11101110 ; |### ### |            $bef8 (G)
    .byte   %11101110 ; |### ### |            $bef9 (G)
    .byte   %10111010 ; |# ### # |            $befa (G)
    .byte   %11010110 ; |## # ## |            $befb (G)
    .byte   %11111110 ; |####### |            $befc (G)
    .byte   %01111100 ; | #####  |            $befd (G)
    .byte   %00111000 ; |  ###   |            $befe (G)
    .byte   %00111000 ; |  ###   |            $beff (G)
    .byte   %01111100 ; | #####  |            $bf00 (G)
    .byte   %11111110 ; |####### |            $bf01 (G)
    .byte   %10010010 ; |#  #  # |            $bf02 (G)
    .byte   %10111010 ; |# ### # |            $bf03 (G)
    .byte   %11111110 ; |####### |            $bf04 (G)
    .byte   %11111110 ; |####### |            $bf05 (G)
    .byte   %10111010 ; |# ### # |            $bf06 (G)
    .byte   %10010010 ; |#  #  # |            $bf07 (G)
    .byte   %11111110 ; |####### |            $bf08 (G)
    .byte   %01111100 ; | #####  |            $bf09 (G)
    .byte   %00111000 ; |  ###   |            $bf0a (G)
    
    .byte   $38,$7c,$fe,$d6,$ba,$fe,$fe,$ba ; $bf0b (*)
    .byte   $d6,$fe,$7c,$38                 ; $bf13 (*)
    
Lbf17
    ldy     ram_96                  ;3        
    lda     ram_F7                  ;3        
    and     #$1f                    ;2        
    cmp     #$16                    ;2        
    bne     Lbf25                   ;2/3      
    lda     #$08                    ;2         *
    bne     Lbf2c                   ;2/3 =  16 *
Lbf25
    lda     ram_F7                  ;3        
    and     #$80                    ;2        
    ora     Lbf44,y                 ;4   =   9
Lbf2c
    sta     ram_B3,x                ;4        
    lda     Lbf45,y                 ;4        
    sta     ram_CB,x                ;4        
    lda     #$ff                    ;2        
    sta     ram_F6                  ;3        
    lda     #$00                    ;2        
    sta     ram_B7,x                ;4        
    lda     ram_91                  ;3        
    ora     #$08                    ;2        
    sta     ram_91                  ;3        
    jmp     $f012                   ;3   =  34
    
Lbf44
    .byte   $0d                             ; $bf44 (D)
Lbf45
    .byte   $0a,$17,$1e,$17,$1c,$17,$28,$17 ; $bf45 (D)
    .byte   $2c,$17,$1e,$17,$1e,$17,$3c,$17 ; $bf4d (D)
    .byte   $28,$17,$05,$0d,$f7,$0d,$f7,$0d ; $bf55 (D)
    .byte   $f7,$0d,$f7,$16,$32,$17,$0a,$08 ; $bf5d (D)
    .byte   $00,$0d,$f7,$0d,$f7,$0d,$19,$16 ; $bf65 (D)
    .byte   $23,$0d,$d4,$0d,$9e,$0f,$0a,$11 ; $bf6d (D)
    .byte   $a8,$0f,$14,$11,$4a,$0d,$9e,$0d ; $bf75 (D)
    .byte   $e8                             ; $bf7d (D)
    .byte   $0d,$1e                         ; $bf7e (*)
    .byte   $0d,$8a,$0d,$32,$18,$3c,$18,$0a ; $bf80 (D)
    .byte   $0d,$b2,$0d,$23,$0d,$19,$16,$23 ; $bf88 (D)
    .byte   $17,$0a,$0d,$0a,$18,$0a,$0d,$0a ; $bf90 (D)
    .byte   $08,$14,$0d,$14,$17,$0a,$0d,$3c ; $bf98 (D)
    .byte   $08,$00,$16,$0a,$0d,$00,$0d,$00 ; $bfa0 (D)
    .byte   $17,$3c,$17,$3c,$16,$19,$17,$0a ; $bfa8 (D)
    .byte   $16,$0a,$17,$28                 ; $bfb0 (D)
  IF PLUSROM
Lbfb4
    lda     game_variation          ; 3
    lsr
    sta     WriteToBuffer           ; game variation 1
    lda     SWCHB                   ; 3
    sta     WriteToBuffer           ; game variation 2
    lda     score_bcd_hi            ; 3
    sta     WriteToBuffer           ; BCD score hi
    lda     score_bcd_mid           ; 3
    sta     WriteToBuffer           ; BCD score mid
    lda     score_bcd_lo            ; 3
    sta     WriteToBuffer           ; BCD score lo
    lda     #HIGHSCORE_ID           ; game id in Highscore DB
    sta     WriteSendBuffer

    jmp Lbfe9

    ORG     $1fe9, $00
    RORG    $bfe9, $00
Lbfe9
    sta     Lbff8
    jmp     Lbfb4
  ENDIF

    ORG     $1ff0, $00
    RORG    $bff0, $00
Lbff0
    .byte   $00,$00,$00,$00,$00,$00,$00,$00

Lbff8
    .byte   $00
Lbff9
    .byte   $00

  IF PLUSROM
    .word (PlusROM_API - $9000)
  ELSE 
    .word $0000          ; NMI
  ENDIF
    .word $f000          ; RESET
    .word $f000          ; IRQ
