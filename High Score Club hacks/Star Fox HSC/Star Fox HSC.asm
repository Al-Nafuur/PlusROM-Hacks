; Disassembly of Public ROMs\Classic Roms\NTSC\BY ALPHABET\S-Z\Star Fox.bin
; Disassembled 04/30/26 23:55:48
; Using Stella 7.0
;
; ROM properties name : Star Fox (1983) (Mythicon)
; ROM properties MD5  : f526d0c519f5001adb1fc7948bfbb3ce
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

;-----------------------------------------------------------
;      User Defined Labels
;-----------------------------------------------------------

PLUSROM     = 1
PAL         = 0

; constants

  IF PLUSROM == 1
WriteToBuffer           = $1FF0
WriteSendBuffer         = $1FF1
ReceiveBuffer           = $1FF2
ReceiveBufferSize       = $1FF3

HIGHSCORE_ID            = 93         ; Star Fox game ID in Highscore DB
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
CXP1FB          = $03  ; (R)
CXPPMM          = $07  ; (R)
INPT4           = $0c  ; (R)
INPT5           = $0d  ; (R)
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
PF0             = $0d  ; (W)
PF1             = $0e  ; (W)
PF2             = $0f  ; (W)
RESP0           = $10  ; (W)
;RESP1          = $11  ; (Wi)
;RESM0          = $12  ; (Wi)
;RESM1          = $13  ; (Wi)
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
;HMP1           = $21  ; (Wi)
;HMM0           = $22  ; (Wi)
;HMM1           = $23  ; (Wi)
HMOVE           = $2a  ; (W)
CXCLR           = $2c  ; (W)

SWCHA           = $0280
SWACNT          = $0281
SWCHB           = $0282
SWBCNT          = $0283
TIMINT          = $0285
TIM8I           = $029d
TIM64I          = $029e


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
ram_8C          = $8c ;(i)
ram_8D          = $8d
ram_8E          = $8e
ram_8F          = $8f
;                 $90  (i)
;                 $91  (i)
ram_92          = $92
;                 $93  (i)
ram_94          = $94
ram_95          = $95
ram_96          = $96
ram_97          = $97
;                 $98  (i)
;                 $99  (i)
ram_9A          = $9a
;                 $9b  (i)
;                 $9c  (i)
;                 $9d  (i)
;                 $9e  (i)
ram_9F          = $9f
ram_A0          = $a0
ram_A1          = $a1
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
ram_CD          = $cd
ram_CE          = $ce
ram_CF          = $cf
;                 $d0  (i)
;                 $d1  (i)
ram_D2          = $d2
ram_D3          = $d3
ram_D4          = $d4
ram_D5          = $d5
;                 $d6  (i)
;                 $d7  (i)
;                 $d8  (i)
;                 $d9  (i)
;                 $da  (i)
;                 $db  (i)
;                 $dc  (i)

;                 $fa  (s)
;                 $fb  (s)
;                 $fc  (s)
;                 $fd  (s)
;                 $fe  (s)
;                 $ff  (s)


;***********************************************************
;      Bank 0
;***********************************************************

    SEG     CODE
    ORG     $f000

  IF PLUSROM
PlusROM_API
       .byte "a", 0, "h.firmaplus.de"
       .byte 0

SendPlusROMScore
    lda     ram_84                  ; 3
    and     #$07                    ; lower nibble is game
    lsr                             ; first bit is 1 or 2 player game
    sta     WriteToBuffer           ; game variation
    lda     ram_8B                  ; 3
    sta     WriteToBuffer           ; BCD score hi
    lda     ram_8D                  ; 3
    sta     WriteToBuffer           ; BCD score lo
    lda     #HIGHSCORE_ID           ; game id in Highscore DB
    sta     WriteSendBuffer
    jsr Lff68
    rts

    ORG     $f03f, $ff
Start
    cld                             ;2        
  ELSE
Checksum_lo
    .byte   $7e                             ; $f000 (D)
Checksum_hi
    .byte   $05                             ; $f001 (D)
Start
    cld                             ;2        
    lda     #$f0                    ;2        Start of ROM checksum check !
    sta     ram_95                  ;3        
    lda     #$01                    ;2        
    sta     ram_94                  ;3        
    ldx     #$00                    ;2        
    stx     ram_96                  ;3        
    stx     ram_97                  ;3   =  20
Lf011
    inc     ram_94                  ;5        
    beq     Lf02e                   ;2/3 =   7
Lf015
    lda     (ram_94,x)              ;6        
    tay                             ;2        
    eor     ram_96                  ;3        
    lsr                             ;2        
    bcc     Lf01f                   ;2/3      
    eor     #$95                    ;2   =  17
Lf01f
    sta     ram_96                  ;3        
    tya                             ;2        
    eor     ram_97                  ;3        
    asl                             ;2        
    bcc     Lf029                   ;2/3      
    eor     #$65                    ;2   =  14
Lf029
    sta     ram_97                  ;3        
    jmp     Lf011                   ;3   =   6
    
Lf02e
    inc     ram_95                  ;5        
    bne     Lf015                   ;2/3      
    lda     Checksum_lo             ;4        
    cmp     ram_96                  ;3   =  14
Lf037
    bne     Lf037                   ;2/3      Halts if lo byte of ROM checksum doesn't match!
    lda     Checksum_hi             ;4        
    cmp     ram_97                  ;3   =   9
Lf03e
    bne     Lf03e                   ;2/3      Halts if hi byte of ROM checksum doesn't match!
  ENDIF

    ORG     $f040

Lf040
    lda     #$00                    ;2        
    sta     SWACNT                  ;4        
    sta     SWBCNT                  ;4        
    tax                             ;2   =  14
Lf049
    sta     VSYNC,x                 ;4        
    txs                             ;2        
    inx                             ;2        
    bne     Lf049                   ;2/3      
    lda     #$72                    ;2        
    sta     ram_84                  ;3   =  15
Lf053
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     #$02                    ;2        
    sta     VBLANK                  ;3        
    sta     VSYNC                   ;3        
    lda     #$2a                    ;2        
    sta     TIM8I                   ;4        
    inc     ram_82                  ;5        
    bne     Lf06c                   ;2/3      
    inc     ram_81                  ;5        
    bit     ram_86                  ;3        
    bmi     Lf06c                   ;2/3      
    dec     ram_86                  ;5   =  36
Lf06c
    lda     SWCHB                   ;4        
    tax                             ;2        
    and     #$03                    ;2        
    cmp     #$03                    ;2        
    bne     Lf07a                   ;2/3      
    cpx     ram_A1                  ;3        
    beq     Lf07e                   ;2/3 =  17
Lf07a
    lda     #$07                    ;2        
    sta     ram_86                  ;3   =   5
Lf07e
    txa                             ;2        
    bit     Lffec                   ;4        
    bne     Lf090                   ;2/3      
    lda     ram_84                  ;3        
    and     #$1f                    ;2        
    ora     #$10                    ;2        
    sta     ram_84                  ;3        
    lda     #$00                    ;2        
    sta     ram_85                  ;3   =  23
Lf090
    txa                             ;2        
    bit     Lffeb                   ;4        
    beq     Lf09c                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_83                  ;3        
    beq     Lf0d2                   ;2/3 =  15
Lf09c
    lda     ram_83                  ;3        
    beq     Lf0a8                   ;2/3      
    dec     ram_83                  ;5        
    txa                             ;2        
    ora     #$02                    ;2        
    tax                             ;2        
    bne     Lf0d2                   ;2/3 =  18
Lf0a8
    lda     #$0f                    ;2        
    sta     ram_83                  ;3        
    lda     #$00                    ;2        
    sta     ram_85                  ;3        
    lda     ram_84                  ;3        
    and     #$07                    ;2        
    cmp     #$02                    ;2        
    bne     Lf0bc                   ;2/3      
    lda     #$00                    ;2        
    beq     Lf0ce                   ;2/3 =  23
Lf0bc
    cmp     #$01                    ;2        
    bne     Lf0c4                   ;2/3      
    lda     #$04                    ;2        
    bne     Lf0ce                   ;2/3 =   8
Lf0c4
    cmp     #$05                    ;2        
    bcc     Lf0cc                   ;2/3      
    lda     #$02                    ;2        
    bne     Lf0ce                   ;2/3 =   8
Lf0cc
    adc     #$01                    ;2   =   2
Lf0ce
    ora     #$70                    ;2        
    sta     ram_84                  ;3   =   5
Lf0d2
    stx     ram_A1                  ;3        
    ldy     #$07                    ;2        
    lda     ram_84                  ;3        
    bmi     Lf0e2                   ;2/3      
    ldx     INPT4|$30               ;3        
    bmi     Lf0e8                   ;2/3      
    sty     ram_86                  ;3        
    bne     Lf0e8                   ;2/3 =  20
Lf0e2
    ldx     INPT5|$30               ;3         *
    bmi Lf0e8
    sty ram_86    
    
Lf0e8
    and     #$60                    ;2        
    beq     Lf10d                   ;2/3!     
    sta     ram_A2                  ;3        
    lda     ram_85                  ;3        
    bne     Lf113                   ;2/3!     
    txa                             ;2        
    bmi     Lf117                   ;2/3!     
    lda     ram_84                  ;3        
    and     #$9f                    ;2        
    ora     #$08                    ;2        
    sta     ram_84                  ;3        
    lda     ram_A2                  ;3        
    and     #$60                    ;2        
    cmp     #$20                    ;2        
    bne     Lf113                   ;2/3      
    lda ram_84
    ora #$10
    sta ram_84
    bne Lf113    
    
Lf10d
    ldx     SWCHA                   ;4        
    inx                             ;2        
    beq     Lf117                   ;2/3 =  46
Lf113
    lda     #$07                    ;2        
    sta     ram_86                  ;3   =   5
Lf117
    lda     ram_84                  ;3        
    bit     Lffe8                   ;4        
    bne     Lf125                   ;2/3      
    bit     Lffe9                   ;4        
    bne     Lf12b                   ;2/3      
    beq     Lf12e                   ;2/3 =  17
Lf125
    jsr     Lff68                   ;6        
    jsr     Lffdb                   ;6   =  12
Lf12b
    jsr     Lfd32                   ;6   =   6
Lf12e
    lda     ram_84                  ;3        
    and     #$e7                    ;2        
    sta     ram_84                  ;3        
    lda     #$00                    ;2        
    bit     ram_86                  ;3        
    bpl     Lf13c                   ;2/3      
    lda ram_81    
    
Lf13c
    sta     ram_80                  ;3   =  18
Lf13e
    lda     #$00                    ;2        
    ldx     #$3f                    ;2        
    bit     TIMINT                  ;4        
    bpl     Lf13e                   ;2/3      
    sta     WSYNC                   ;3   =  13
;---------------------------------------
    sta     VSYNC                   ;3        
    stx     TIM64I                  ;4        
    lda     ram_C7                  ;3        
    tax                             ;2        
    and     #$30                    ;2        
    beq     Lf161                   ;2/3      
    sec                             ;2        
    sbc     #$10                    ;2        
    sta     ram_A2                  ;3        
    txa                             ;2        
    and     #$cf                    ;2        
    ora     ram_A2                  ;3        
    sta     ram_C7                  ;3   =  33
Lf161
    lda     ram_AF                  ;3        
    bmi     Lf1d2                   ;2/3      
    bit     CXM1P|$30               ;3        
    bpl     Lf177                   ;2/3      
    lda     #$02                    ;2        
    jsr     Lffbc                   ;6        
    jsr     Lfdb3                   ;6        
    lda     ram_C7                  ;3        
    ora     #$30                    ;2        
    sta     ram_C7                  ;3   =  32
Lf177
    bit     ram_B1                  ;3        
    bpl     Lf187                   ;2/3      
    bit     CXM0P|$30               ;3        
    bvc     Lf187                   ;2/3      
    lda     #$d0                    ;2        
    sta     ram_AF                  ;3        
    lda     #$00                    ;2        
    sta     ram_B1                  ;3   =  20
Lf187
    bit     ram_B0                  ;3        
    bmi     Lf198                   ;2/3      
    bit     CXP1FB|$30              ;3        
    bpl     Lf198                   ;2/3      
    lda     #$01                    ;2        
    jsr     Lffbc                   ;6        
    lda     #$d2                    ;2        
    sta     ram_B0                  ;3   =  23
Lf198
    bit     ram_B1                  ;3        
    bpl     Lf1b0                   ;2/3      
    lda     ram_C7                  ;3        
    and     #$0f                    ;2        
    cmp     #$07                    ;2        
    bcs     Lf1b0                   ;2/3      
    lda     ram_A8                  ;3        
    sbc     #$04                    ;2        
    cmp     ram_AA                  ;3        
    bne     Lf1b0                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_B1                  ;3   =  29
Lf1b0
    lda     ram_AF                  ;3        
    ora     ram_B0                  ;3        
    bmi     Lf1d2                   ;2/3      
    bit     CXPPMM|$30              ;3        
    bpl     Lf1d2                   ;2/3      
    lda     #$10                    ;2        
    jsr     Lffbc                   ;6        
    ldx     #$e0                    ;2        
    stx     ram_B0                  ;3        
    lda     ram_84                  ;3        
    bit     Lffea                   ;4        
    beq     Lf1d2                   ;2/3      

    ldx #$d0
    stx ram_AF
    ldx #$e0
    stx ram_B0    
    
Lf1d2
    sta     CXCLR                   ;3        
    ldx     ram_B2                  ;3        
    beq     Lf1da                   ;2/3      
    inc     ram_B2                  ;5   =  48
Lf1da
    ldx     ram_B1                  ;3        
    bpl     Lf1e0                   ;2/3      
    inc     ram_B1                  ;5   =  10
Lf1e0
    ldx     ram_B0                  ;3        
    bpl     Lf1e6                   ;2/3      
    inc     ram_B0                  ;5   =  10
Lf1e6
    bne     Lf1eb                   ;2/3      
    jsr     Lfd66                   ;6   =   8
Lf1eb
    ldx     ram_AF                  ;3        
    bpl     Lf1f1                   ;2/3      
    inc     ram_AF                  ;5   =  10
Lf1f1
    bne     Lf1f9                   ;2/3      
    jsr     Lff7a                   ;6        
    jsr     Lfd32                   ;6   =  14
Lf1f9
    lda     ram_82                  ;3        
    and     #$07                    ;2        
    bne     Lf20c                   ;2/3!     
    lda     ram_C8                  ;3        
    beq     Lf20c                   ;2/3      
    bpl     Lf20a                   ;2/3      
    inc     ram_C8                  ;5        
    jmp     Lf20c                   ;3   =  22
    
Lf20a
    dec     ram_C8                  ;5   =   5
Lf20c
    lda     ram_84                  ;3        
    and     #$60                    ;2        
    beq     Lf227                   ;2/3      
    cmp     #$60                    ;2        
    beq     Lf21c                   ;2/3      
    jsr     Lfd47                   ;6        
    jmp     Lf2b2                   ;3   =  20
    
Lf21c
    lda     ram_82                  ;3        
    eor     ram_81                  ;3        
    ror                             ;2        
    ror                             ;2        
    ror                             ;2        
    ror                             ;2        
    jmp     Lf24a                   ;3   =  17
    
Lf227
    lda     ram_AF                  ;3        
    bpl     Lf22e                   ;2/3      
    jmp     Lf2b2                   ;3   =   8
    
Lf22e
    lda     SWCHA                   ;4        
    bit     ram_84                  ;3        
    bmi     Lf240                   ;2/3      
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    sta     ram_A2                  ;3        
    lda     INPT4|$30               ;3        
    jmp     Lf246                   ;3   =  26
    
Lf240
    and     #$0f                    ;2         *
    sta ram_A2
    lda INPT5|$30    
    
Lf246
    and     #$80                    ;2        
    ora     ram_A2                  ;3   =   7
Lf24a
    bit     Lffec                   ;4        
    bne     Lf257                   ;2/3      
    ldy     ram_A8                  ;3        
    cpy     #$8c                    ;2        
    bcs     Lf257                   ;2/3      
    inc     ram_A8                  ;5   =  18
Lf257
    bit     Lffeb                   ;4        
    bne     Lf264                   ;2/3      
    ldy     ram_A8                  ;3        
    cpy     #$17                    ;2        
    bcc     Lf264                   ;2/3      
    dec     ram_A8                  ;5   =  18
Lf264
    bit     Lffea                   ;4        
    bne     Lf27f                   ;2/3      
    ldx     ram_A8                  ;3        
    cpx     #$2d                    ;2        
    bcc     Lf277                   ;2/3      
    ldx     ram_C8                  ;3        
    cpx     #$fd                    ;2        
    beq     Lf277                   ;2/3      
    dec     ram_C8                  ;5   =  25
Lf277
    tax                             ;2        
    lda     ram_C7                  ;3        
    and     #$7f                    ;2        
    sta     ram_C7                  ;3        
    txa                             ;2   =  12
Lf27f
    bit     Lffe9                   ;4        
    bne     Lf29a                   ;2/3      
    ldx     ram_A8                  ;3        
    cpx     #$2d                    ;2        
    bcc     Lf292                   ;2/3      
    ldx     ram_C8                  ;3        
    cpx     #$03                    ;2        
    beq     Lf292                   ;2/3      
    inc     ram_C8                  ;5   =  25
Lf292
    tax                             ;2        
    lda     ram_C7                  ;3        
    ora     #$80                    ;2        
    sta     ram_C7                  ;3        
    txa                             ;2   =  12
Lf29a
    cmp     #$00                    ;2        
    bmi     Lf2b2                   ;2/3      
    lda     ram_C7                  ;3        
    and     #$07                    ;2        
    cmp     #$07                    ;2        
    bcc     Lf2b2                   ;2/3      
    lda     ram_A8                  ;3        
    cmp     #$32                    ;2        
    bcc     Lf2b2                   ;2/3      
    lda     ram_C7                  ;3        
    and     #$f0                    ;2        
    sta     ram_C7                  ;3   =  28
Lf2b2
    lda     ram_84                  ;3        
    and     #$60                    ;2        
    beq     Lf2bf                   ;2/3      
    cmp     #$60                    ;2        
    beq     Lf2bf                   ;2/3      
    jmp     Lf47c                   ;3   =  14
    
Lf2bf
    bit     ram_B0                  ;3        
    bpl     Lf2c6                   ;2/3      
    jmp     Lf47c                   ;3   =   8
    
Lf2c6
    lda     ram_81                  ;3        
    and     #$07                    ;2        
    bne     Lf2d0                   ;2/3      
    lda     #$0a                    ;2        
    bne     Lf2fa                   ;2/3 =  11
Lf2d0
    lda     #$00                    ;2        
    sta     ram_A2                  ;3        
    lda     #$07                    ;2        
    sta     ram_A3                  ;3        
    lda     ram_A8                  ;3        
    cmp     #$32                    ;2        
    bcc     Lf2fc                   ;2/3      
    lda     ram_B0                  ;3        
    and     #$38                    ;2        
    eor     #$ff                    ;2        
    adc     #$3c                    ;2        
    sta     ram_A3                  ;3        
    bit     ram_C7                  ;3        
    bpl     Lf2f8                   ;2/3      
    lda     ram_A3                  ;3        
    eor     #$ff                    ;2        
    sta     ram_A3                  ;3        
    lda     #$ff                    ;2        
    sta     ram_A2                  ;3        
    bne     Lf2fc                   ;2/3 =  49
Lf2f8
    lda     #$00                    ;2   =   2
Lf2fa
    sta     ram_A2                  ;3   =   3
Lf2fc
    sec                             ;2        
    lda     ram_A3                  ;3        
    sbc     ram_C2                  ;3        
    sta     ram_A3                  ;3        
    lda     ram_A2                  ;3        
    sbc     ram_C1                  ;3        
    sta     ram_A2                  ;3        
    bmi     Lf328                   ;2/3      
    beq     Lf316                   ;2/3      
    jsr     Lf46d                   ;6        
    jsr     Lfd83                   ;6        
    jmp     Lf47c                   ;3   =  39
    
Lf316
    lda     ram_A3                  ;3        
    cmp     #$07                    ;2        
    bcc     Lf322                   ;2/3      
    jsr     Lf46d                   ;6        
    jmp     Lf389                   ;3   =  16
    
Lf322
    jsr     Lf436                   ;6        
    jmp     Lf341                   ;3   =   9
    
Lf328
    cmp     #$ff                    ;2        
    beq     Lf335                   ;2/3      
    jsr     Lf45b                   ;6        
    jsr     Lfd83                   ;6        
    jmp     Lf47c                   ;3   =  19
    
Lf335
    lda     ram_A3                  ;3        
    cmp     #$f9                    ;2        
    bcs     Lf322                   ;2/3      
    jsr     Lf45b                   ;6        
    jmp     Lf389                   ;3   =  16
    
Lf341
    lda     ram_C7                  ;3        
    bit     ram_C1                  ;3        
    bmi     Lf34b                   ;2/3      
    ora     #$40                    ;2        
    bne     Lf34d                   ;2/3 =  12
Lf34b
    and     #$bf                    ;2   =   2
Lf34d
    sta     ram_C7                  ;3        
    lda     ram_A8                  ;3        
    cmp     #$32                    ;2        
    bcc     Lf36d                   ;2/3      
    sec                             ;2        
    lda     ram_A8                  ;3        
    sbc     ram_A9                  ;3        
    bne     Lf35f                   ;2/3      
    jmp     Lf3b0                   ;3   =  23
    
Lf35f
    bmi     Lf367                   ;2/3      
    jsr     Lf449                   ;6        
    jmp     Lf3b0                   ;3   =  11
    
Lf367
    jsr     Lf452                   ;6        
    jmp     Lf3b0                   ;3   =   9
    
Lf36d
    lda     ram_B0                  ;3        
    and     #$06                    ;2        
    lsr                             ;2        
    tay                             ;2        
    lda     Lf385,y                 ;4        
    cmp     ram_A9                  ;3        
    bcc     Lf394                   ;2/3      
    bne     Lf37f                   ;2/3      
    jmp     Lf3ed                   ;3   =  23
    
Lf37f
    jsr     Lf449                   ;6        
    jmp     Lf47c                   ;3   =   9
    
Lf385
    .byte   $7a,$68,$56,$44                 ; $f385 (D)
    
Lf389
    lda     ram_B0                  ;3        
    and     #$07                    ;2        
    tax                             ;2        
    lda     ram_A8                  ;3        
    cmp     #$64                    ;2        
    bcc     Lf3a2                   ;2/3 =  14
Lf394
    lda     ram_A9                  ;3        
    sec                             ;2        
    sbc     Lf3e7,x                 ;4        
    cmp     #$35                    ;2        
    bcs     Lf3ae                   ;2/3      
    lda     #$35                    ;2        
    bne     Lf3ae                   ;2/3 =  17
Lf3a2
    lda     ram_A9                  ;3        
    clc                             ;2        
    adc     Lf3e7,x                 ;4        
    cmp     #$8c                    ;2        
    bcc     Lf3ae                   ;2/3      
    lda     #$8c                    ;2   =  15
Lf3ae
    sta     ram_A9                  ;3   =   3
Lf3b0
    ldx     ram_B1                  ;3        
    bpl     Lf3b7                   ;2/3      
    jmp     Lf47c                   ;3   =   8
    
Lf3b7
    cmp     #$0a                    ;2        
    bcc     Lf3c2                   ;2/3      
    cmp     #$f5                    ;2        
    bcs     Lf3c2                   ;2/3      
    jmp     Lf47c                   ;3   =  11
    
Lf3c2
    lda     #$04                    ;2        
    bit     ram_C7                  ;3        
    bvc     Lf3ca                   ;2/3      
    lda     #$fc                    ;2   =   9
Lf3ca
    sta     ram_CA                  ;3        
    clc                             ;2        
    lda     ram_A9                  ;3        
    adc     #$02                    ;2        
    sta     ram_AA                  ;3        
    lda     Lff80                   ;4        
    sta     ram_B1                  ;3        
    lda     #$00                    ;2        
    sta     ram_CC                  ;3        
    lda     ram_C1                  ;3        
    sta     ram_C3                  ;3        
    lda     ram_C2                  ;3        
    sta     ram_C4                  ;3        
    jmp     Lf47c                   ;3   =  40
    
Lf3e7
    .byte   $03,$04,$05,$06,$0a,$0e         ; $f3e7 (D)
    
Lf3ed
    lda     ram_B1                  ;3        
    bpl     Lf3f4                   ;2/3      
    jmp     Lf47c                   ;3   =   8
    
Lf3f4
    lda     ram_C1                  ;3        
    beq     Lf3fb                   ;2/3      
    jmp Lf47c
    
Lf3fb
    lda     ram_C2                  ;3        
    cmp     #$11                    ;2        
    bcs     Lf47c                   ;2/3      
    lda     ram_A9                  ;3        
    cmp     ram_A8                  ;3        
    beq     Lf47c                   ;2/3      
    bcc     Lf47c                   ;2/3      
    clc                             ;2        
    adc     #$02                    ;2        
    sta     ram_AA                  ;3        
    clc                             ;2        
    lda     ram_C2                  ;3        
    adc     #$02                    ;2        
    sta     ram_C4                  ;3        
    lda     ram_C1                  ;3        
    adc     #$00                    ;2        
    sta     ram_C3                  ;3        
    lda     ram_C8                  ;3        
    sta     ram_CA                  ;3        
    lda     ram_B0                  ;3        
    and     #$07                    ;2        
    tax                             ;2        
    lda     Lf430,x                 ;4        
    sta     ram_CC                  ;3        
    lda     #$80                    ;2        
    sta     ram_B1                  ;3        
    jmp     Lf47c                   ;3   =  75
    
Lf430
    .byte   $ff,$fe,$fe,$fe,$fd,$fd         ; $f430 (D)
    
Lf436
    ldx     #$01                    ;2        
    sec                             ;2        
    lda     ram_C8                  ;3        
    sbc     ram_C9                  ;3        
    beq     Lf448                   ;2/3      
    bpl     Lf445                   ;2/3      
    jsr     Lf45b                   ;6        
    rts                             ;6   =  26
    
Lf445
    jsr     Lf46d                   ;6   =   6
Lf448
    rts                             ;6   =   6
    
Lf449
    ldy     ram_A9                  ;3        
    cpy     #$8c                    ;2        
    bcs     Lf451                   ;2/3      
    inc     ram_A9                  ;5   =  12
Lf451
    rts                             ;6   =   6
    
Lf452
    ldy     ram_A9                  ;3        
    cpy     #$33                    ;2        
    bcc     Lf45a                   ;2/3      
    dec     ram_A9                  ;5   =  12
Lf45a
    rts                             ;6   =   6
    
Lf45b
    ldy     #$fe                    ;2        
    cpy     ram_C9                  ;3        
    beq     Lf463                   ;2/3      
    dec     ram_C9                  ;5   =  12
Lf463
    lda     ram_C7                  ;3        
    and     #$bf                    ;2        
    sta     ram_C7                  ;3        
    lda     SWCHA                   ;4        
    rts                             ;6   =  18
    
Lf46d
    ldy     #$02                    ;2        
    cpy     ram_C9                  ;3        
    beq     Lf475                   ;2/3      
    inc     ram_C9                  ;5   =  12
Lf475
    lda     ram_C7                  ;3        
    ora     #$40                    ;2        
    sta     ram_C7                  ;3        
    rts                             ;6   =  14
    
Lf47c
    lda     ram_AB                  ;3        
    bit     ram_82                  ;3        
    bpl     Lf48a                   ;2/3      
    cmp     #$15                    ;2        
    bcc     Lf490                   ;2/3      
    dec     ram_AB                  ;5        
    bne     Lf490                   ;2/3 =  19
Lf48a
    cmp     #$23                    ;2        
    bcs     Lf490                   ;2/3      
    inc     ram_AB                  ;5   =   9
Lf490
    ldx     #$01                    ;2   =   2
Lf492
    ldy     #$00                    ;2        
    sec                             ;2        
    lda     ram_C8,x                ;4        
    sbc     ram_C8                  ;3        
    sta     ram_A3                  ;3        
    bpl     Lf49f                   ;2/3      
    ldy     #$ff                    ;2   =  18
Lf49f
    sty     ram_A2                  ;3        
    txa                             ;2        
    asl                             ;2        
    tay                             ;2        
    clc                             ;2        
    lda.wy  ram_C0,y                ;4        
    adc     ram_A3                  ;3        
    sta.wy  ram_C0,y                ;5        
    lda.wy  ram_BF,y                ;4        
    adc     ram_A2                  ;3        
    beq     Lf4ca                   ;2/3      
    cmp     #$ff                    ;2        
    beq     Lf4ca                   ;2/3      
    bpl     Lf4c3                   ;2/3      
    lda     #$00                    ;2        
    sta.wy  ram_C0,y                ;5        
    lda     #$ff                    ;2        
    bne     Lf4ca                   ;2/3 =  49
Lf4c3
    lda     #$ff                    ;2        
    sta.wy  ram_C0,y                ;5        
    lda     #$00                    ;2   =   9
Lf4ca
    sta.wy  ram_BF,y                ;5        
    inx                             ;2        
    cpx     #$04                    ;2        
    bcc     Lf492                   ;2/3      
    clc                             ;2        
    lda     ram_AA                  ;3        
    adc     ram_CC                  ;3        
    beq     Lf4db                   ;2/3      
    sta     ram_AA                  ;3   =  24
Lf4db
    lda     ram_C8                  ;3        
    beq     Lf516                   ;2/3!     
    bpl     Lf4eb                   ;2/3      
    clc                             ;2        
    lda     ram_BD                  ;3        
    adc     #$02                    ;2        
    sta     ram_BD                  ;3        
    jmp     Lf4f0                   ;3   =  20
    
Lf4eb
    sec                             ;2        
    lda     ram_BD                  ;3        
    sbc     #$02                    ;2   =   7
Lf4f0
    sta     ram_BD                  ;3        
    cmp     #$99                    ;2        
    bcs     Lf4f9                   ;2/3      
    jmp     Lf516                   ;3   =  10
    
Lf4f9
    cmp     #$d2                    ;2        
    bcs     Lf50b                   ;2/3!     
    lda     #$00                    ;2        
    sta     ram_BD                  ;3        
    sec                             ;2        
    lda     ram_BE                  ;3        
    sbc     #$08                    ;2        
    sta     ram_BE                  ;3        
    jmp     Lf516                   ;3   =  22
    
Lf50b
    lda     #$98                    ;2        
    sta     ram_BD                  ;3        
    clc                             ;2        
    lda     ram_BE                  ;3        
    adc     #$08                    ;2        
    sta     ram_BE                  ;3   =  15
Lf516
    clc                             ;2        
    lda     ram_C8                  ;3        
    adc     #$03                    ;2        
    tax                             ;2        
    lda     Lf557,x                 ;4        
    cmp     ram_B9                  ;3        
    beq     Lf52b                   ;2/3      
    bcs     Lf529                   ;2/3      
    dec     ram_B9                  ;5        
    bne     Lf52b                   ;2/3 =  27
Lf529
    inc     ram_B9                  ;5   =   5
Lf52b
    ldy     #$01                    ;2        
    lda     ram_C1                  ;3        
    sta     ram_A2                  ;3        
    lda     ram_C2                  ;3        
    sta     ram_A3                  ;3        
    jsr     Lf55e                   ;6        
    stx     ram_BA                  ;3        
    lda     ram_C3                  ;3        
    sta     ram_A2                  ;3        
    lda     ram_C4                  ;3        
    sta     ram_A3                  ;3        
    jsr     Lf55e                   ;6        
    stx     ram_BB                  ;3        
    lda     ram_C5                  ;3        
    sta     ram_A2                  ;3        
    lda     ram_C6                  ;3        
    sta     ram_A3                  ;3        
    jsr     Lf55e                   ;6        
    stx     ram_BC                  ;3        
    jmp     Lf56d                   ;3   =  68
    
Lf557
    .byte   $1e,$2d,$3c,$4b,$5a,$69,$78     ; $f557 (D)
    
Lf55e
    clc                             ;2        
    lda     ram_B9                  ;3        
    adc     ram_A3                  ;3        
    tax                             ;2        
    lda     #$00                    ;2        
    adc     ram_A2                  ;3        
    beq     Lf56c                   ;2/3      
    ldx     #$ff                    ;2   =  19
Lf56c
    rts                             ;6   =   6
    
Lf56d
    lda     ram_AF                  ;3        
    bmi     Lf5a9                   ;2/3      
    lda     ram_C7                  ;3        
    and     #$07                    ;2        
    cmp     #$03                    ;2        
    bcs     Lf5a9                   ;2/3      
    inc     ram_C7                  ;5        
    lda     ram_B9                  ;3        
    clc                             ;2        
    adc     #$0f                    ;2        
    lsr                             ;2        
    lsr                             ;2        
    tay                             ;2        
    bit     ram_C7                  ;3        
    bmi     Lf598                   ;2/3 =  37
Lf587
    ldx     Lf5bb,y                 ;4        
    lda     ram_B3,x                ;4        
    ora     Lf5e3,y                 ;4        
    sta     ram_B3,x                ;4        
    dey                             ;2        
    cpy     #$ff                    ;2        
    bne     Lf587                   ;2/3      
    beq     Lf60b                   ;2/3!=  24
Lf598
    ldx     Lf5bb,y                 ;4        
    lda     ram_B3,x                ;4        
    ora     Lf5e3,y                 ;4        
    sta     ram_B3,x                ;4        
    iny                             ;2        
    cpy     #$28                    ;2        
    bcc     Lf598                   ;2/3      
    bcs     Lf60b                   ;2/3!=  24
Lf5a9
    cmp     #$07                    ;2        
    bcs     Lf5af                   ;2/3      
    inc     ram_C7                  ;5   =   9
Lf5af
    lda     #$00                    ;2        
    ldx     #$06                    ;2   =   4
Lf5b3
    sta     ram_B2,x                ;4        
    dex                             ;2        
    bne     Lf5b3                   ;2/3      
    jmp     Lf60b                   ;3   =  11
    
Lf5bb
    .byte   $00,$00,$00,$00,$01,$01,$01,$01 ; $f5bb (D)
    .byte   $01,$01,$01,$01,$02,$02,$02,$02 ; $f5c3 (D)
    .byte   $02,$02,$02,$02,$03,$03,$03,$03 ; $f5cb (D)
    .byte   $04,$04,$04,$04,$04,$04,$04,$04 ; $f5d3 (D)
    .byte   $05,$05,$05,$05,$05,$05,$05,$05 ; $f5db (D)
Lf5e3
    .byte   $10,$20,$40,$80,$80,$40,$20,$10 ; $f5e3 (D)
    .byte   $08,$04,$02,$01,$01,$02,$04,$08 ; $f5eb (D)
    .byte   $10,$20,$40,$80,$10,$20,$40,$80 ; $f5f3 (D)
    .byte   $80,$40,$20,$10,$08,$04,$02,$01 ; $f5fb (D)
    .byte   $01,$02,$04,$08,$10,$20,$40,$80 ; $f603 (D)
    
Lf60b
    lda     ram_AF                  ;3        
    bmi     Lf61f                   ;2/3      
    ldx     ram_A8                  ;3        
    ldy     #$0f                    ;2        
    cpx     #$32                    ;2        
    bcc     Lf61b                   ;2/3      
    lda     #$00                    ;2        
    beq     Lf623                   ;2/3 =  18
Lf61b
    lda     #$08                    ;2        
    bne     Lf623                   ;2/3 =   4
Lf61f
    ldy     ram_82                  ;3        
    eor     ram_82                  ;3   =   6
Lf623
    sty     ram_A6                  ;3        
    clc                             ;2        
    adc     #$8d                    ;2        
    sta     ram_A2                  ;3        
    lda     #$00                    ;2        
    adc     #$f6                    ;2        
    sta     ram_A3                  ;3        
    ldy     #$00                    ;2        
    lda     ram_B0                  ;3        
    bpl     Lf63d                   ;2/3      
    ldy     ram_82                  ;3        
    eor     ram_82                  ;3        
    jmp     Lf63f                   ;3   =  33
    
Lf63d
    and     #$38                    ;2   =   2
Lf63f
    sty     ram_A7                  ;3        
    clc                             ;2        
    adc     #$5d                    ;2        
    sta     ram_A4                  ;3        
    lda     #$00                    ;2        
    adc     #$f6                    ;2        
    sta     ram_A5                  ;3        
    ldy     #$07                    ;2   =  19
Lf64e
    lda     (ram_A2),y              ;5        
    sta.wy  ram_CD,y                ;5        
    lda     (ram_A4),y              ;5        
    sta.wy  ram_D5,y                ;5        
    dey                             ;2        
    bpl     Lf64e                   ;2/3      
    bmi     Lf69d                   ;2/3      
    
    .byte   $18,$7e,$c3,$c3,$ff,$ff,$7e,$18 ; $f65d (D)
    .byte   $00,$7c,$3e,$79,$0f,$1e,$3c,$00 ; $f665 (D)
    .byte   $00,$18,$3c,$c3,$e7,$3c,$00,$00 ; $f66d (D)
    .byte   $00,$00,$42,$e7,$ff,$e7,$42,$00 ; $f675 (D)
    .byte   $00,$00,$18,$ff,$99,$00,$00,$00 ; $f67d (D)
    .byte   $00,$00,$18,$24,$18,$00,$00,$00 ; $f685 (D)
    .byte   $30,$70,$fe,$87,$fe,$74,$30,$10 ; $f68d (D)
    .byte   $fa,$70,$fe,$87,$fe,$74,$30,$10 ; $f695 (D)
    
Lf69d
    lda     ram_84                  ;3        
    and     #$60                    ;2        
    beq     Lf6a6                   ;2/3      
    jmp     Lf746                   ;3   =  36
    
Lf6a6
    lda     ram_AF                  ;3        
    bpl     Lf6c4                   ;2/3      
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    eor     #$0f                    ;2        
    and     #$0f                    ;2        
    cmp     #$0c                    ;2        
    bne     Lf6b7                   ;2/3      
    lda #$00
    
Lf6b7
    sta     AUDV0                   ;3        
    adc     #$17                    ;2        
    sta     AUDF0                   ;3        
    lda     #$08                    ;2        
    sta     AUDC0                   ;3        
    jmp     Lf6f9                   ;3   =  35
    
Lf6c4
    lda     ram_C7                  ;3        
    and     #$0f                    ;2        
    cmp     #$08                    ;2        
    bcs     Lf6d7                   ;2/3      
    sta     AUDF0                   ;3        
    sta     AUDC0                   ;3        
    lda     #$01                    ;2        
    sta     AUDV0                   ;3        
    jmp     Lf6f9                   ;3   =  23
    
Lf6d7
    lda     ram_B2                  ;3         *
    bpl Lf6e9
    lda #$02
    sta AUDC0
    lda #$0f
    sta AUDF0
    lda #$0f
    sta AUDV0
    bne Lf6f9
Lf6e9
    lda #$06
    sta AUDC0
    lda #$01
    sta AUDV0
    lda ram_C8
    bpl Lf6f7
    eor #$ff
Lf6f7
    sta AUDF0
    
Lf6f9
    lda     ram_B0                  ;3        
    bpl     Lf715                   ;2/3!     
    lsr                             ;2        
    lsr                             ;2        
    eor     #$0f                    ;2        
    and     #$0f                    ;2        
    cmp     #$0c                    ;2        
    bne     Lf709                   ;2/3      
    lda #$00
    
Lf709
    sta     AUDV1                   ;3        
    adc     #$17                    ;2        
    sta     AUDF1                   ;3        
    lda     #$08                    ;2        
    sta     AUDC1                   ;3        
    bne     Lf74c                   ;2/3 =  35
Lf715
    lda     ram_B1                  ;3        
    bpl     Lf72b                   ;2/3      
    cmp     #$92                    ;2        
    bcs     Lf72b                   ;2/3      
    lda     #$04                    ;2        
    sta     AUDV1                   ;3        
    lda     #$06                    ;2        
    sta     AUDC1                   ;3        
    lda     #$1e                    ;2        
    sta     AUDF1                   ;3        
    bne     Lf74c                   ;2/3 =  26
Lf72b
    lda     #$0c                    ;2        
    sta     AUDC1                   ;3        
    lda     #$01                    ;2        
    sta     AUDV1                   ;3        
    lda     ram_A9                  ;3        
    eor     #$ff                    ;2        
    tax                             ;2        
    lda     ram_C7                  ;3        
    bit     Lffe7                   ;4        
    beq     Lf741                   ;2/3      
    adc     #$1f                    ;2   =  28
Lf741
    stx     AUDF1                   ;3        
    jmp     Lf74c                   ;3   =   6
    
Lf746
    lda     #$00                    ;2        
    sta     AUDV0                   ;3        
    sta     AUDV1                   ;3   =   8
Lf74c
    ldx     ram_AA                  ;3        
    cpx     #$14                    ;2        
    bcc     Lf75c                   ;2/3      
    ldy     ram_BB                  ;3        
    lda     ram_B1                  ;3        
    bpl     Lf75c                   ;2/3      
    cpy     #$99                    ;2        
    bcc     Lf760                   ;2/3 =  19
Lf75c
    ldx     #$00                    ;2        
    stx     ram_B1                  ;3   =   5
Lf760
    stx     ram_AD                  ;3        
    ldx     #$02                    ;2        
    jsr     Lfdd8                   ;6        
    lda     ram_AB                  ;3        
    ldy     ram_BC                  ;3        
    ldx     ram_B2                  ;3        
    bne     Lf773                   ;2/3      
    cpy     #$99                    ;2        
    bcc     Lf775                   ;2/3 =  26
Lf773
    lda     #$00                    ;2   =   2
Lf775
    sta     ram_AE                  ;3        
    ldx     #$03                    ;2        
    ldy     ram_BC                  ;3        
    jsr     Lfdd8                   ;6        
    lda     #BLUE                   ;2        
    sta     ram_87                  ;3        
    lda     ram_84                  ;3        
    tax                             ;2        
    and     #$60                    ;2        
    cmp     #$60                    ;2        
    bne     Lf78e                   ;2/3      
    jmp     Lf833                   ;3   =  33
    
Lf78e
    txa                             ;2        
    bit     Lffeb                   ;4        
    bne     Lf80c                   ;2/3!     
    bit     Lffec                   ;4        
    beq     Lf7a7                   ;2/3      

    bit Lffe7
    beq Lf7a4
    bit ram_82
    bpl Lf7a7
    bmi Lf7b3
Lf7a4
    txa
    bmi Lf7b3    
    
Lf7a7
    lda     #$14                    ;2        
    sta     ram_9F                  ;3        
    lda     ram_8B                  ;3        
    ldx     ram_8D                  ;3        
    ldy     #$1c                    ;2        
    bne     Lf7bd                   ;2/3      
    
Lf7b3
    lda #$64
    sta ram_9F
    ldy #$6c
    lda ram_8C
    ldx ram_8E
    
Lf7bd
    sty     ram_A0                  ;3        
    sta     ram_A2                  ;3        
    stx     ram_A3                  ;3        
    lda     #$00                    ;2        
    ldx     #$03                    ;2   =  42
Lf7c7
    sta     ram_8E,x                ;4        
    sta     ram_96,x                ;4        
    dex                             ;2        
    bne     Lf7c7                   ;2/3      
    ldy     #$04                    ;2   =  14
Lf7d0
    lda     ram_A2                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    jsr     Lf821                   ;6        
    and     #$f0                    ;2        
    sta     ram_8A                  ;3        
    lda     ram_A2                  ;3        
    and     #$0f                    ;2        
    jsr     Lf821                   ;6        
    and     #$0f                    ;2        
    ora     ram_8A                  ;3        
    sta.wy  ram_92,y                ;5        
    lda     ram_A3                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    jsr     Lf821                   ;6        
    and     #$f0                    ;2        
    sta     ram_8A                  ;3        
    lda     ram_A3                  ;3        
    and     #$0f                    ;2        
    jsr     Lf821                   ;6        
    and     #$0f                    ;2        
    ora     ram_8A                  ;3        
    sta.wy  ram_9A,y                ;5        
    dey                             ;2        
    bpl     Lf7d0                   ;2/3!     
    jmp     Lf93e                   ;3   =  93
    
Lf80c
    lda     #$14                    ;2        
    sta     ram_9F                  ;3        
    ldx     #$07                    ;2        
    ldy     #$00                    ;2   =   9
Lf814
    lda     Lf8d6,x                 ;4        
    sta     ram_8F,x                ;4        
    sty     ram_97,x                ;4        
    dex                             ;2        
    bpl     Lf814                   ;2/3      
    jmp     Lf93e                   ;3   =  19
    
Lf821
    tax                             ;2        
    clc                             ;2        
    lda     #$7e                    ;2        
    adc     Lf86e,x                 ;4        
    sta     ram_A4                  ;3        
    lda     #$f8                    ;2        
    adc     #$00                    ;2        
    sta     ram_A5                  ;3        
    lda     (ram_A4),y              ;5        
    rts                             ;6   =  31
    
Lf833
    lda     #$34                    ;2        
    sta     ram_9F                  ;3        
    lda     #$43                    ;2        
    sta     ram_A0                  ;3        
    lda     ram_84                  ;3        
    bit     Lffeb                   ;4        
    bne     Lf84d                   ;2/3      
    bit     Lffea                   ;4        
    bne     Lf853                   ;2/3      
    lda     #$de                    ;2        
    ldx     #$f8                    ;2        
    bne     Lf857                   ;2/3 =  31
Lf84d
    lda     #$d6                    ;2        
    ldx     #$f8                    ;2        
    bne     Lf857                   ;2/3 =   6
Lf853
    lda     #$ce                    ;2        
    ldx     #$f8                    ;2   =   4
Lf857
    sta     ram_A2                  ;3        
    stx     ram_A3                  ;3        
    ldy     #$07                    ;2   =   8
Lf85d
    lda     Lf92e,y                 ;4        
    sta.wy  ram_8F,y                ;5        
    lda     (ram_A2),y              ;5        
    sta.wy  ram_97,y                ;5        
    dey                             ;2        
    bpl     Lf85d                   ;2/3      
    jmp     Lf93e                   ;3   =  26
    
Lf86e
    .byte   $00,$05,$0a,$0f,$14,$19,$1e,$23 ; $f86e (D)
    .byte   $28,$2d,$32,$37,$3c,$41,$46,$4b ; $f876 (D)
    .byte   $77,$55,$55,$55,$77,$22,$66,$22 ; $f87e (D)
    .byte   $22,$77,$77,$11,$77,$44,$77,$77 ; $f886 (D)
    .byte   $11,$33,$11,$77,$55,$55,$77,$11 ; $f88e (D)
    .byte   $11,$77,$44,$77,$11,$77,$66,$44 ; $f896 (D)
    .byte   $77,$55,$77,$77,$11,$22,$22,$22 ; $f89e (D)
    .byte   $77,$55,$22,$55,$77,$77,$55,$77 ; $f8a6 (D)
    .byte   $11,$11,$22,$55,$77,$55,$55,$66 ; $f8ae (D)
    .byte   $55,$66,$55,$77,$33,$44,$44,$44 ; $f8b6 (D)
    .byte   $77,$66,$55,$55,$55,$66,$77,$44 ; $f8be (D)
    .byte   $66,$44,$77,$77,$44,$66,$44,$44 ; $f8c6 (D)
    .byte   $08,$1c,$3e,$7f,$3e,$1c,$08,$00 ; $f8ce (D)
Lf8d6
    .byte   $3c,$7e,$db,$ff,$bd,$c3,$7e,$3c ; $f8d6 (D)
    
    .byte   %00000000 ; |        |            $f8de (G)
    .byte   %00000000 ; |        |            $f8df (G)
    .byte   %00000000 ; |        |            $f8e0 (G)
    .byte   %00000000 ; |        |            $f8e1 (G)
    .byte   %00000000 ; |        |            $f8e2 (G)
    .byte   %00000000 ; |        |            $f8e3 (G)
    .byte   %00000000 ; |        |            $f8e4 (G)
    .byte   %00000000 ; |        |            $f8e5 (G)
    
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f8e6 (D)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f8ee (D)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f8f6 (D)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f8fe (D)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f906 (D)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f90e (D)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f916 (D)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f91e (D)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $f926 (D)
    
Lf92e
    .byte   %00010000 ; |   #    |            $f92e (G)
    .byte   %00000000 ; |        |            $f92f (G)
    .byte   %01111100 ; | #####  |            $f930 (G)
    .byte   %10111010 ; |# ### # |            $f931 (G)
    .byte   %00111000 ; |  ###   |            $f932 (G)
    .byte   %00101000 ; |  # #   |            $f933 (G)
    .byte   %00101000 ; |  # #   |            $f934 (G)
    .byte   %01101100 ; | ## ##  |            $f935 (G)
    .byte   %00000000 ; |        |            $f936 (G)
    .byte   %00001000 ; |    #   |            $f937 (G)
    .byte   %00001000 ; |    #   |            $f938 (G)
    .byte   %00001000 ; |    #   |            $f939 (G)
    .byte   %01011100 ; | # ###  |            $f93a (G)
    .byte   %01111111 ; | #######|            $f93b (G)
    .byte   %01111111 ; | #######|            $f93c (G)
    .byte   %00110110 ; |  ## ## |            $f93d (G)
    
Lf93e
    lda     ram_85                  ;3        
    beq     Lf952                   ;2/3      
    dec     ram_85
    sta     AUDF0
    sta     ram_80
    lda     #$05
    sta     AUDV0
    lda     #$00
    sta     AUDC0
    sta     AUDV1    
    
Lf952
    lda     ram_87                  ;3        
    eor     ram_80                  ;3        
    sta     COLUBK                  ;3        
    ldx     #$0f                    ;2        
    bit     Lffe9                   ;4        
    beq     Lf961                   ;2/3      
    ldx     #$00
    
Lf961
    stx     ram_A2                  ;3        
    and     #$f0                    ;2        
    ora     ram_A2                  ;3        
    sta     ram_87                  ;3        
    lda     #$d2                    ;2        
    sta     ram_8A                  ;3        
    ldx     #$00                    ;2        
    ldy     ram_9F                  ;3        
    jsr     Lfdd8                   ;6        
    ldx     #$01                    ;2        
    ldy     ram_A0                  ;3        
    jsr     Lfdd8                   ;6        
    lda     #$00                    ;2   =  62
Lf97d
    bit     TIMINT                  ;4        
    bpl     Lf97d                   ;2/3      
    sta     WSYNC                   ;3   =   9
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     VBLANK                  ;3        
    sta     WSYNC                   ;3   =   9
;---------------------------------------
    sta     HMOVE                   ;3        
    dec     ram_8A                  ;5        
    ldx     #$00                    ;2        
    stx     NUSIZ1                  ;3        
    lda     ram_84                  ;3        
    and     #$61                    ;2        
    cmp     #$61                    ;2        
    bne     Lf99c                   ;2/3      
    ldx     #$02                    ;2   =  24
Lf99c
    stx     NUSIZ0                  ;3        
    lda     ram_87                  ;3        
    sta     COLUP0                  ;3        
    sta     COLUP1                  ;3        
    lda     ram_8B                  ;3        
    bit     ram_84                  ;3        
    bpl     Lf9ac                   ;2/3      
    lda     ram_8C
    
Lf9ac
    sta     ram_87                  ;3        
    ldy     #$00                    ;2   =  25
Lf9b0
    lda.wy  ram_97,y                ;4        
    tax                             ;2        
    lda.wy  ram_8F,y                ;4        
    sta     WSYNC                   ;3   =  13
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     GRP0                    ;3        
    stx     GRP1                    ;3        
    dec     ram_8A                  ;5        
    sta     WSYNC                   ;3   =  17
;---------------------------------------
    sta     HMOVE                   ;3        
    dec     ram_8A                  ;5        
    iny                             ;2        
    cpy     #$08                    ;2        
    bcc     Lf9b0                   ;2/3      
    lda     #$00                    ;2        
    sta     WSYNC                   ;3   =  19
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     GRP0                    ;3        
    sta     GRP1                    ;3        
    dec     ram_8A                  ;5        
    ldy     ram_88                  ;3        
    lda     ram_84                  ;3        
    bit     Lffe5                   ;4        
    beq     Lf9e3                   ;2/3      
    ldy     ram_89
    
Lf9e3
    and     #$60                    ;2        
    cmp     #$60                    ;2        
    beq     Lfa1f                   ;2/3!     
    cmp     #$20                    ;2        
    beq     Lfa1f                   ;2/3!     
    cmp     #$40                    ;2        
    beq     Lfa18                   ;2/3!     
    lda     ram_84                  ;3        
    bit     Lffeb                   ;4        
    bne     Lfa1f                   ;2/3!     
    sta     WSYNC                   ;3   =  52
;---------------------------------------
    sta     HMOVE                   ;3        
    dec     ram_8A                  ;5        
    cpy     #$02                    ;2        
    bcc     Lfa1f                   ;2/3      
    ldx     #$00                    ;2        
    cpy     #$03                    ;2        
    bcc     Lfa0f                   ;2/3      
    inx                             ;2        
    cpy     #$04                    ;2        
    bcc     Lfa0f                   ;2/3      
    ldx     #$03                    ;2   =  26
Lfa0f
    stx     NUSIZ0                  ;3        
    lda     #$2e                    ;2        
    ldx     #$f9                    ;2        
    jmp     Lfa23                   ;3   =  10
    
Lfa18
    lda     #$36                    ;2        
    ldx     #$f9                    ;2        
    jmp     Lfa23                   ;3   =   7
    
Lfa1f
    lda     #$de                    ;2        
    ldx     #$f8                    ;2   =   4
Lfa23
    sta     ram_A2                  ;3        
    stx     ram_A3                  ;3        
    ldy     #$00                    ;2   =   8
Lfa29
    lda     (ram_A2),y              ;5        
    sta     WSYNC                   ;3   =   8
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     GRP0                    ;3        
    dec     ram_8A                  ;5        
    sta     WSYNC                   ;3   =  14
;---------------------------------------
    sta     HMOVE                   ;3        
    dec     ram_8A                  ;5        
    iny                             ;2        
    cpy     #$08                    ;2        
    bcc     Lfa29                   ;2/3      
    lda     #$00                    ;2        
    sta     WSYNC                   ;3   =  19
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     GRP0                    ;3        
    dec     ram_8A                  ;5        
    lda     #$af                    ;2   =  13
Lfa4a
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    dec     ram_8A                  ;5        
    cmp     ram_8A                  ;3        
    bcc     Lfa4a                   ;2/3      
    ldx     ram_80                  ;3        
    lda     #BLUE_CYAN|$4           ;2        
    eor     ram_80                  ;3        
    sta     WSYNC                   ;3   =  24
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     COLUBK                  ;3        
    sta     ram_A4                  ;3        
    dec     ram_8A                  ;5        
    stx     COLUP0                  ;3        
    ldx     #$00                    ;2        
    ldy     ram_BD                  ;3        
    jsr     Lfdd8                   ;6        
    sta     WSYNC                   ;3   =  31
;---------------------------------------
    sta     HMOVE                   ;3        
    dec     ram_8A                  ;5        
    lda     ram_BE                  ;3        
    and     #$18                    ;2        
    clc                             ;2        
    adc     #$c0                    ;2        
    sta     ram_A2                  ;3        
    lda     #$fa                    ;2        
    adc     #$00                    ;2        
    sta     ram_A3                  ;3        
    lda     #$00                    ;2        
    sta     NUSIZ0                  ;3        
    ldy     #$00                    ;2   =  34
Lfa88
    lda     (ram_A2),y              ;5        
    inc     ram_A4                  ;5        
    ldx     ram_A4                  ;3        
    sta     WSYNC                   ;3   =  16
;---------------------------------------
    sta     HMOVE                   ;3        
    stx     COLUBK                  ;3        
    sta     GRP0                    ;3        
    dec     ram_8A                  ;5        
    iny                             ;2        
    cpy     #$08                    ;2        
    bcc     Lfa88                   ;2/3      
    lda     #BLACK|$7               ;2        
    eor     ram_80                  ;3        
    ldx     #$00                    ;2        
    stx     WSYNC                   ;3   =  30
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     COLUBK                  ;3        
    stx     GRP0                    ;3        
    stx     GRP1                    ;3        
    lda     ram_A6                  ;3        
    sta     COLUP0                  ;3        
    lda     ram_A7                  ;3        
    sta     COLUP1                  ;3        
    dec     ram_8A                  ;5        
    stx     NUSIZ0                  ;3        
    stx     NUSIZ1                  ;3        
    sta     ram_A4                  ;3        
    jmp     Lfae0                   ;3   =  41
    
    .byte   %00010000 ; |   #    |            $fac0 (G)
    .byte   %00010000 ; |   #    |            $fac1 (G)
    .byte   %00110000 ; |  ##    |            $fac2 (G)
    .byte   %01010000 ; | # #    |            $fac3 (G)
    .byte   %00010000 ; |   #    |            $fac4 (G)
    .byte   %11111111 ; |########|            $fac5 (G)
    .byte   %11111111 ; |########|            $fac6 (G)
    .byte   %11111111 ; |########|            $fac7 (G)
    .byte   %00001000 ; |    #   |            $fac8 (G)
    .byte   %00011000 ; |   ##   |            $fac9 (G)
    .byte   %01111110 ; | ###### |            $faca (G)
    .byte   %00100100 ; |  #  #  |            $facb (G)
    .byte   %11111111 ; |########|            $facc (G)
    .byte   %00100100 ; |  #  #  |            $facd (G)
    .byte   %00100100 ; |  #  #  |            $face (G)
    .byte   %00100100 ; |  #  #  |            $facf (G)
    .byte   %00000000 ; |        |            $fad0 (G)
    .byte   %00001111 ; |    ####|            $fad1 (G)
    .byte   %00001001 ; |    #  #|            $fad2 (G)
    .byte   %11111111 ; |########|            $fad3 (G)
    .byte   %10001001 ; |#   #  #|            $fad4 (G)
    .byte   %11111111 ; |########|            $fad5 (G)
    .byte   %10001001 ; |#   #  #|            $fad6 (G)
    .byte   %11111111 ; |########|            $fad7 (G)
    .byte   %00111000 ; |  ###   |            $fad8 (G)
    .byte   %00111000 ; |  ###   |            $fad9 (G)
    .byte   %11111110 ; |####### |            $fada (G)
    .byte   %11000110 ; |##   ## |            $fadb (G)
    .byte   %10101010 ; |# # # # |            $fadc (G)
    .byte   %10010010 ; |#  #  # |            $fadd (G)
    .byte   %10101010 ; |# # # # |            $fade (G)
    .byte   %11000110 ; |##   ## |            $fadf (G)
    
Lfae0
    sta     CXCLR                   ;3        
    sta     WSYNC                   ;3   =   6
;---------------------------------------
    sta     HMOVE                   ;3        
    dec     ram_8A                  ;5        
    ldx     #$00                    ;2        
    ldy     ram_B9                  ;3        
    jsr     Lfdd8                   ;6        
    lda     ram_A9                  ;3        
    ldx     #$01                    ;2        
    ldy     ram_BA                  ;3        
    cpy     #$99                    ;2        
    bcc     Lfafb                   ;2/3      
    lda     #$00                    ;2   =  33
Lfafb
    sta     ram_AC                  ;3        
    jsr     Lfdd8                   ;6        
    sta     WSYNC                   ;3   =  12
;---------------------------------------
    sta     HMOVE                   ;3        
    dec     ram_8A                  ;5        
    lda     ram_82                  ;3        
    and     #$01                    ;2        
    beq     Lfb0e                   ;2/3      
    lda     #$0f                    ;2   =  17
Lfb0e
    ora     #$d0                    ;2        
    eor     ram_80                  ;3        
    sta     COLUPF                  ;3        
    sta     WSYNC                   ;3   =  11
;---------------------------------------
    sta     HMOVE                   ;3        
    dec     ram_8A                  ;5        
    lda     #$08                    ;2        
    bit     ram_C7                  ;3        
    bmi     Lfb22                   ;2/3      
    sta     REFP0                   ;3   =  18
Lfb22
    bit     ram_C7                  ;3        
    bvs     Lfb28                   ;2/3      
    sta     REFP1                   ;3   =   8
Lfb28
    lda     #$00                    ;2        
    sta     CTRLPF                  ;3        
    ldx     #$35                    ;2        
    bit     ram_CC                  ;3        
    bpl     Lfb34                   ;2/3      
    ldx     #$55                    ;2   =  14
Lfb34
    stx     NUSIZ0                  ;3        
    ldx     #$50                    ;2        
    stx     NUSIZ1                  ;3        
    sta     WSYNC                   ;3   =  11
;---------------------------------------
    sta     HMOVE                   ;3        
    dec     ram_8A                  ;5        
    ldx     #$1d                    ;2        
    txs                             ;2        
    sec                             ;2        
    lda     ram_8A                  ;3        
    sbc     ram_A8                  ;3        
    tax                             ;2        
    lda     ram_8A                  ;3        
    sbc     ram_AC                  ;3        
    tay                             ;2        
    sta     WSYNC                   ;3   =  33
;---------------------------------------
    sta     HMOVE                   ;3        
    dec     ram_8A                  ;5        
    lda     ram_C7                  ;3        
    and     #$0f                    ;2        
    cmp     #$03                    ;2        
    bcc     Lfb5f                   ;2/3      
    jmp     Lfc76                   ;3   =  20
    
Lfb5f
    lda     ram_8A                  ;3        
    cmp     #$31                    ;2        
    bcs     Lfb68                   ;2/3      
    jmp     Lfc9f                   ;3   =  10
    
Lfb68
    lda     #$00                    ;2        
    cpx     #$09                    ;2        
    bcc     Lfb89                   ;2/3      
    lda     #$00                    ;2        
    cpy     #$08                    ;2        
    bcs     Lfb77                   ;2/3      
    lda.wy  ram_D5,y                ;4   =  16
Lfb77
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     GRP1                    ;3        
    lda     ram_8A                  ;3        
    cmp     ram_AD                  ;3        
    php                             ;3        
    pla                             ;4        
    dex                             ;2        
    dey                             ;2        
    dec     ram_8A                  ;5        
    bne     Lfb5f                   ;2/3 =  30
Lfb89
    ldx     ram_8A                  ;3        
    lda     ram_D4                  ;3        
    sta     WSYNC                   ;3   =   9
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     GRP0                    ;3        
    lda     #$00                    ;2        
    cpy     #$08                    ;2        
    bcs     Lfb9c                   ;2/3      
    lda.wy  ram_D5,y                ;4   =  16
Lfb9c
    sta     GRP1                    ;3        
    cpx     ram_AD                  ;3        
    php                             ;3        
    pla                             ;4        
    dex                             ;2        
    dey                             ;2        
    lda     ram_D3                  ;3        
    sta     WSYNC                   ;3   =  23
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     GRP0                    ;3        
    lda     #$00                    ;2        
    cpy     #$08                    ;2        
    bcs     Lfbb5                   ;2/3      
    lda.wy  ram_D5,y                ;4   =  16
Lfbb5
    sta     GRP1                    ;3        
    cpx     ram_AD                  ;3        
    php                             ;3        
    pla                             ;4        
    dex                             ;2        
    dey                             ;2        
    lda     ram_D2                  ;3        
    sta     WSYNC                   ;3   =  23
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     GRP0                    ;3        
    lda     #$00                    ;2        
    cpy     #$08                    ;2        
    bcs     Lfbce                   ;2/3      
    lda.wy  ram_D5,y                ;4   =  16
Lfbce
    sta     GRP1                    ;3        
    cpx     ram_AD                  ;3        
    php                             ;3        
    pla                             ;4        
    dex                             ;2        
    dey                             ;2        
    lda     #$00                    ;2        
    cpy     #$08                    ;2        
    bcs     Lfbdf                   ;2/3      
    lda.wy  ram_D5,y                ;4   =  27
Lfbdf
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     GRP1                    ;3        
    lda     ram_B3                  ;3        
    sta     PF0                     ;3        
    lda     ram_B4                  ;3        
    sta     PF1                     ;3        
    lda     ram_B5                  ;3        
    sta     PF2                     ;3        
    lda     ram_B6                  ;3        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    sta     PF0                     ;3        
    lda     ram_B7                  ;3        
    sta     PF1                     ;3        
    lda     ram_B8                  ;3        
    sta     PF2                     ;3        
    dex                             ;2        
    dey                             ;2        
    lda     #$00                    ;2        
    cpy     #$08                    ;2        
    bcs     Lfc0e                   ;2/3      
    lda.wy  ram_D5,y                ;4   =  68
Lfc0e
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     GRP1                    ;3        
    lda     #$00                    ;2        
    sta     PF0                     ;3        
    sta     PF1                     ;3        
    sta     PF2                     ;3        
    cpx     ram_AD                  ;3        
    php                             ;3        
    pla                             ;4        
    dex                             ;2        
    dey                             ;2        
    lda     ram_CF                  ;3        
    sta     WSYNC                   ;3   =  37
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     GRP0                    ;3        
    lda     #$00                    ;2        
    cpy     #$08                    ;2        
    bcs     Lfc33                   ;2/3      
    lda.wy  ram_D5,y                ;4   =  16
Lfc33
    sta     GRP1                    ;3        
    cpx     ram_AD                  ;3        
    php                             ;3        
    pla                             ;4        
    dex                             ;2        
    dey                             ;2        
    lda     ram_CE                  ;3        
    sta     WSYNC                   ;3   =  23
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     GRP0                    ;3        
    lda     #$00                    ;2        
    cpy     #$08                    ;2        
    bcs     Lfc4c                   ;2/3      
    lda.wy  ram_D5,y                ;4   =  16
Lfc4c
    sta     GRP1                    ;3        
    cpx     ram_AD                  ;3        
    php                             ;3        
    pla                             ;4        
    dex                             ;2        
    dey                             ;2        
    lda     #$00                    ;2        
    cpy     #$08                    ;2        
    bcs     Lfc5d                   ;2/3      
    lda.wy  ram_D5,y                ;4   =  27
Lfc5d
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     GRP1                    ;3        
    lda     #$00                    ;2        
    sta     GRP0                    ;3        
    lda     ram_8A                  ;3        
    cmp     ram_AD                  ;3        
    php                             ;3        
    pla                             ;4        
    dex                             ;2        
    dey                             ;2        
    stx     ram_8A                  ;3        
    ldx     #$ff                    ;2        
    jmp     Lfb5f                   ;3   =  36
    
Lfc76
    lda     #$00                    ;2        
    cpx     #$08                    ;2        
    bcs     Lfc7e                   ;2/3      
    lda     ram_CD,x                ;4   =  10
Lfc7e
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     GRP0                    ;3        
    lda     #$00                    ;2        
    cpy     #$08                    ;2        
    bcs     Lfc8d                   ;2/3      
    lda.wy  ram_D5,y                ;4   =  16
Lfc8d
    sta     GRP1                    ;3        
    lda     ram_8A                  ;3        
    cmp     ram_AD                  ;3        
    php                             ;3        
    pla                             ;4        
    dex                             ;2        
    dey                             ;2        
    dec     ram_8A                  ;5        
    lda     ram_8A                  ;3        
    cmp     #$31                    ;2        
    bcs     Lfc76                   ;2/3 =  32
Lfc9f
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    dec     ram_8A                  ;5        
    dec     ram_8A                  ;5        
    txa                             ;2        
    ldx     #$1e                    ;2        
    txs                             ;2        
    tax                             ;2        
    lda     #BROWN|$f                    ;2        
    eor     ram_80                  ;3        
    sta     ram_A2                  ;3        
    lda     ram_82                  ;3        
    and     #$0f                    ;2        
    ora     #$90                    ;2        
    eor     ram_80                  ;3        
    sta     COLUP1                  ;3   =  42
Lfcbc
    ldy     #$00                    ;2        
    cpx     #$09                    ;2        
    bcs     Lfcc4                   ;2/3      
    ldy     ram_CD,x                ;4   =  10
Lfcc4
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    sty     GRP0                    ;3        
    sec                             ;2        
    lda     ram_AE                  ;3        
    sbc     ram_8A                  ;3        
    beq     Lfcd3                   ;2/3      
    cmp     #$01                    ;2   =  18
Lfcd3
    php                             ;3        
    lda     ram_8A                  ;3        
    cmp     ram_AD                  ;3        
    php                             ;3        
    pla                             ;4        
    dex                             ;2        
    dec     ram_8A                  ;5        
    ldy     #$00                    ;2        
    cpx     #$09                    ;2        
    bcs     Lfce5                   ;2/3      
    ldy     ram_CD,x                ;4   =  33
Lfce5
    lda     ram_A2                  ;3        
    sta     WSYNC                   ;3   =   6
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     COLUBK                  ;3        
    sty     GRP0                    ;3        
    lda     ram_AD                  ;3        
    cmp     ram_8A                  ;3        
    php                             ;3        
    pla                             ;4        
    pla                             ;4        
    dex                             ;2        
    lda     ram_A2                  ;3        
    tay                             ;2        
    and     #$0f                    ;2        
    cmp     #$05                    ;2        
    bcc     Lfd06                   ;2/3      
    tya                             ;2        
    sec                             ;2        
    sbc     #$02                    ;2        
    sta     ram_A2                  ;3   =  48
Lfd06
    dec     ram_8A                  ;5        
    lda     #$12                    ;2        
    cmp     ram_8A                  ;3        
    bcc     Lfcbc                   ;2/3!     
    ldx     #$ff                    ;2        
    txs                             ;2        
    sta     WSYNC                   ;3   =  19
;---------------------------------------
    lda     ram_80                  ;3        
    sta     COLUBK                  ;3        
    lda     #$00                    ;2        
    sta     NUSIZ0                  ;3        
    sta     REFP0                   ;3        
    sta     REFP1                   ;3        
    sta     GRP0                    ;3        
    sta     GRP1                    ;3        
    sta     ENAM0                   ;3        
    sta     ENAM1                   ;3        
    dec     ram_8A                  ;5   =  34
Lfd29
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    dec     ram_8A                  ;5        
    bne     Lfd29                   ;2/3      
    jmp     Lf053                   ;3   =  10
    
Lfd32
    jsr     Lfd47                   ;6        
    jsr     Lfd66                   ;6        
    jsr     Lfda4                   ;6        
    jsr     Lfdb3                   ;6        
    lda     #$00                    ;2        
    sta     ram_BD                  ;3        
    lda     #$1a                    ;2        
    sta     ram_BE                  ;3        
    rts                             ;6   =  40
    
Lfd47
    lda     #$6e                    ;2        
    sta     ram_A8                  ;3        
    lda     #$01                    ;2        
    sta     ram_AF                  ;3        
    lda     #$0c                    ;2        
    sta     ram_B3                  ;3        
    lda     #$00                    ;2        
    sta     ram_B4                  ;3        
    sta     ram_B5                  ;3        
    sta     ram_B6                  ;3        
    sta     ram_B7                  ;3        
    sta     ram_B8                  ;3        
    sta     ram_C8                  ;3        
    lda     #$50                    ;2        
    sta     ram_B9                  ;3        
    rts                             ;6   =  46
    
Lfd66
    lda     #$6e                    ;2        
    sta     ram_A9                  ;3        
    jsr     Lfd83                   ;6        
    bit     ram_82                  ;3        
    bpl     Lfd76                   ;2/3      
    lda     #$fb                    ;2        
    jmp     Lfd78                   ;3   =  21
    
Lfd76
    lda     #$05                    ;2   =   2
Lfd78
    sta     ram_C1                  ;3        
    lda     ram_82                  ;3        
    sta     ram_C2                  ;3        
    lda     #$00                    ;2        
    sta     ram_C9                  ;3        
    rts                             ;6   =  20
    
Lfd83
    lda     ram_84                  ;3        
    and     #$04                    ;2        
    beq     Lfd8b                   ;2/3      
    lda     #$09                    ;2   =   9
Lfd8b
    ora     #$40                    ;2        
    sta     ram_B0                  ;3        
    lda     ram_87                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    tax                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    sta     ram_A2                  ;3        
    txa                             ;2        
    and     #$38                    ;2        
    ora     ram_A2                  ;3        
    clc                             ;2        
    adc     ram_B0                  ;3        
    sta     ram_B0                  ;3        
    rts                             ;6   =  44
    
Lfda4
    lda     #$00                    ;2        
    sta     ram_AA                  ;3        
    sta     ram_B1                  ;3        
    sta     ram_C3                  ;3        
    sta     ram_C4                  ;3        
    sta     ram_CA                  ;3        
    sta     ram_CC                  ;3        
    rts                             ;6   =  26
    
Lfdb3
    lda     ram_82                  ;3        
    and     #$07                    ;2        
    clc                             ;2        
    adc     #$14                    ;2        
    sta     ram_AB                  ;3        
    lda     #$f8                    ;2        
    sta     ram_B2                  ;3        
    lda     ram_82                  ;3        
    bpl     Lfdc9                   ;2/3      
    lda     #$fd                    ;2        
    jmp     Lfdcb                   ;3   =  27
    
Lfdc9
    lda     #$03                    ;2   =   2
Lfdcb
    sta     ram_C5                  ;3        
    lda     ram_82                  ;3        
    eor     #$ff                    ;2        
    sta     ram_C6                  ;3        
    lda     #$00                    ;2        
    sta     ram_CB                  ;3        
    rts                             ;6   =  22
    
Lfdd8
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    cpy     #$99                    ;2        
    bcc     Lfde2                   ;2/3      
    ldy     #$4c                    ;2   =   9
Lfde2
    lda     Lfecf,y                 ;4        
    sta     HMP0,x                  ;4        
    lda     Lfe36,y                 ;4        
    and     #$0f                    ;2        
    sta     ram_A5                  ;3        
    lda     Lfe36,y                 ;4        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    and     #$07                    ;2        
    tay                             ;2        
    lda     Lfe26,y                 ;4        
    sta     ram_A2                  ;3        
    lda     Lfe2e,y                 ;4        
    sta     ram_A3                  ;3        
    ldy     ram_A5                  ;3        
    sta     WSYNC                   ;3   =  53
;---------------------------------------
    sta     HMOVE                   ;3        
    jmp.ind (ram_A2)                ;5        
    nop                             ;2        
    jmp     Lfe17                   ;3   =  13
    
    jmp     Lfe14                   ;3   =   3
    
    nop                             ;2        
    nop                             ;2   =   4
Lfe14
    dey                             ;2        
    bne     Lfe14                   ;2/3 =   4
Lfe17
    sta     RESP0,x                 ;4        
    sta     WSYNC                   ;3   =   7
;---------------------------------------
    sta     HMOVE                   ;3        
    dec     ram_8A                  ;5        
    dec     ram_8A                  ;5        
    lda     #$00                    ;2        
    sta     HMP0,x                  ;4        
    rts                             ;6   =  25
    
Lfe26
    .byte   $0b,$0c,$0f,$12,$13,$14,$17,$17 ; $fe26 (D)
Lfe2e
    .byte   $fe,$fe,$fe,$fe,$fe,$fe,$fe,$fe ; $fe2e (D)
Lfe36
    .byte   $01,$21,$61,$32,$01,$32,$32,$32 ; $fe36 (D)
    .byte   $53,$53,$53,$53,$53,$53,$43,$43 ; $fe3e (D)
    .byte   $43,$23,$23,$23,$33,$33,$33,$54 ; $fe46 (D)
    .byte   $54,$54,$54,$54,$54,$44,$44,$44 ; $fe4e (D)
    .byte   $24,$24,$24,$34,$34,$34,$55,$55 ; $fe56 (D)
    .byte   $55,$55,$55,$55,$45,$45,$45,$25 ; $fe5e (D)
    .byte   $25,$25,$35,$35,$35,$56,$56,$56 ; $fe66 (D)
    .byte   $56,$56,$46,$46,$46,$46,$26,$26 ; $fe6e (D)
    .byte   $26,$36,$36,$36,$57,$57,$57,$57 ; $fe76 (D)
    .byte   $57,$57,$47,$47,$47,$27,$27,$27 ; $fe7e (D)
    .byte   $37,$37,$37,$58,$58,$58,$58,$58 ; $fe86 (D)
    .byte   $58,$48,$48,$48,$28,$28,$28,$38 ; $fe8e (D)
    .byte   $38,$38,$59,$59,$59,$59,$59,$59 ; $fe96 (D)
    .byte   $49,$49,$49,$29,$29,$29,$39,$39 ; $fe9e (D)
    .byte   $39,$5a,$5a,$5a,$5a,$5a,$5a,$4a ; $fea6 (D)
    .byte   $4a,$4a,$2a,$2a,$2a,$3a,$3a,$3a ; $feae (D)
    .byte   $5b,$5b,$5b,$5b,$5b,$5b,$4b,$4b ; $feb6 (D)
    .byte   $4b,$2b,$2b,$2b,$3b,$3b,$3b,$3b ; $febe (D)
    .byte   $3b,$3b,$3b,$5c,$4c,$4c,$4c,$4c ; $fec6 (D)
    .byte   $4c                             ; $fece (D)
Lfecf
    .byte   $20,$20,$f0,$10,$f0,$f0,$e0,$d0 ; $fecf (D)
    .byte   $f0,$e0,$d0,$c0,$b0,$a0,$f0,$e0 ; $fed7 (D)
    .byte   $d0,$f0,$e0,$d0,$f0,$e0,$d0,$f0 ; $fedf (D)
    .byte   $e0,$d0,$c0,$b0,$a0,$f0,$e0,$d0 ; $fee7 (D)
    .byte   $f0,$e0,$d0,$f0,$e0,$d0,$f0,$e0 ; $feef (D)
    .byte   $d0,$c0,$b0,$a0,$f0,$e0,$d0,$f0 ; $fef7 (D)
    .byte   $e0,$d0,$f0,$e0,$d0,$f0,$e0,$d0 ; $feff (D)
    .byte   $c0,$b0,$00,$f0,$e0,$d0,$f0,$e0 ; $ff07 (D)
    .byte   $d0,$f0,$e0,$d0,$f0,$e0,$d0,$c0 ; $ff0f (D)
    .byte   $b0,$a0,$f0,$e0,$d0,$f0,$e0,$d0 ; $ff17 (D)
    .byte   $f0,$e0,$d0,$f0,$e0,$d0,$c0,$b0 ; $ff1f (D)
    .byte   $a0,$f0,$e0,$d0,$f0,$e0,$d0,$f0 ; $ff27 (D)
    .byte   $e0,$d0,$f0,$e0,$d0,$c0,$b0,$a0 ; $ff2f (D)
    .byte   $f0,$e0,$d0,$f0,$e0,$d0,$f0,$e0 ; $ff37 (D)
    .byte   $d0,$f0,$e0,$d0,$c0,$b0,$a0,$f0 ; $ff3f (D)
    .byte   $e0,$d0,$f0,$e0,$d0,$f0,$e0,$d0 ; $ff47 (D)
    .byte   $f0,$e0,$d0,$c0,$b0,$a0,$f0,$e0 ; $ff4f (D)
    .byte   $d0,$f0,$e0,$d0,$f0,$e0,$d0,$c0 ; $ff57 (D)
    .byte   $b0,$a0,$90,$b0,$00,$f0,$e0,$d0 ; $ff5f (D)
    .byte   $c0                             ; $ff67 (D)
    
Lff68
    ldx     #$04                    ;2        
    stx     ram_88                  ;3        
    ldx     #$00                    ;2        
    lda     ram_84                  ;3        
    bit     Lffec                   ;4        
    beq     Lff77                   ;2/3      
    ldx     #$04                    ;2   =  18
Lff77
    stx     ram_89                  ;3        
    rts                             ;6   =   9
    
Lff7a
    lda     ram_84                  ;3        
    and     #$60                    ;2        
    bne     Lffbb                   ;2/3 =   7
Lff80
    lda     ram_84                  ;3        
    bit     Lffeb                   ;4        
    bne     Lffa3                   ;2/3      

    bit $ffe5
    bne Lff91
    dec ram_88
    jmp Lff93
Lff91
    dec ram_89
Lff93
    clc
    lda ram_88
    tax
    adc ram_89
    beq Lffac
    lda ram_84
    and #$1f
    cpx ram_89
    bcc Lffa7
    
Lffa3
    ora     #$40                    ;2        
    bne     Lffa9                   ;2/3      
    
Lffa7
    ora #$c0
    
Lffa9
    sta     ram_84                  ;3        
    rts                             ;6   =  22

Lffac
    lda ram_84
    and #$07
    ora #$20
    sta ram_84
    lda #$ff
    sta ram_85
  IF PLUSROM
    jmp SendPlusROMScore
  ELSE
    jsr Lff68    
  ENDIF
    
Lffbb
    rts                             ;6   =   6 *
    
Lffbc
    ldx     #$00                    ;2        
    bit     ram_84                  ;3        
    bpl     Lffc3                   ;2/3      
    
    .byte   $e8                             ; $ffc2 (D)
    
Lffc3
    tay                             ;2        
    and     #$f0                    ;2        
    sed                             ;2        
    clc                             ;2        
    adc     ram_8D,x                ;4        
    sta     ram_8D,x                ;4        
    tya                             ;2        
    and     #$0f                    ;2        
    adc     ram_8B,x                ;4        
    bcc     Lffd7                   ;2/3 
    lda     #$99
    sta     ram_8D,x
    
Lffd7
    sta     ram_8B,x                ;4        
    cld                             ;2        
    rts                             ;6   =  45
    
Lffdb
    lda     #$00                    ;2        
    ldx     #$04                    ;2   =   4
Lffdf
    sta     ram_8A,x                ;4        
    dex                             ;2        
    bpl     Lffdf                   ;2/3      
    rts                             ;6   =  14
    
Lffe5
    .byte   $80,$40                         ; $ffe5 (D)
Lffe7
    .byte   $20                             ; $ffe7 (D)
Lffe8
    .byte   $10                             ; $ffe8 (D)
Lffe9
    .byte   $08                             ; $ffe9 (D)
Lffea
    .byte   $04                             ; $ffea (D)
Lffeb
    .byte   $02                             ; $ffeb (D)
Lffec
    .byte   $01
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $ffed (free)
    .byte   $ff,$ff,$ff,$ff,$ff
    
    
  IF PLUSROM
    .word (PlusROM_API - $E000)
  ELSE 
    .word Start          ; NMI
  ENDIF
    .word Start          ; RESET
    .word Start          ; IRQ
