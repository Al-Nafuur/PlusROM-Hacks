; Disassembly of Skiing.bin
; Disassembled 05/21/24 09:55:59
; Using Stella 6.7.1
;
; ROM properties name : Skiing (1980) (Activision)
; ROM properties MD5  : b76fbadc8ffb1f83e2ca08b6fb4d6c9f
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
;      Assembler switches
;-----------------------------------------------------------

NTSC                    = 0
PAL50                   = 1

PLUSROM                 = 1

   IFNCONST COMPILE_REGION

COMPILE_REGION         = PAL50       ; change to compile for different regions

   ENDIF

   IF !(COMPILE_REGION = NTSC || COMPILE_REGION = PAL50)

      echo ""
      echo "*** ERROR: Invalid COMPILE_REGION value"
      echo "*** Valid values: NTSC = 0, PAL50 = 1"
      echo ""
      err

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

;CXM0P          = $00  ; (Ri)
;CXM1P          = $01  ; (Ri)
CXPPMM          = $07  ; (R)
;INPT2          = $0a  ; (Ri)
INPT4           = $0c  ; (R)

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
PF0             = $0d  ; (W)
RESP0           = $10  ; (W)
;RESP1          = $11  ; (Wi)
AUDC0           = $15  ; (W)
AUDC1           = $16  ; (W)
AUDF0           = $17  ; (W)
AUDF1           = $18  ; (W)
AUDV0           = $19  ; (W)
AUDV1           = $1a  ; (W)
GRP0            = $1b  ; (W)
GRP1            = $1c  ; (W)
HMP0            = $20  ; (W)
HMP1            = $21  ; (W)
VDELP0          = $25  ; (W)
VDELP1          = $26  ; (W)
HMOVE           = $2a  ; (W)
HMCLR           = $2b  ; (W)
CXCLR           = $2c  ; (W)

SWCHA           = $0280
SWCHB           = $0282
INTIM           = $0284
TIM64T          = $0296


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
;                 $88  (i)
ram_89          = $89
;                 $8a  (i)
ram_8B          = $8b
game_nr         = $8c
ram_8D          = $8d
ram_8E          = $8e
ram_8F          = $8f
ram_90          = $90
ram_91          = $91
ram_92          = $92
ram_93          = $93
;                 $94  (i)
ram_95          = $95
;                 $96  (i)
ram_97          = $97
;                 $98  (i)
ram_99          = $99
ram_9A          = $9a
ram_9B          = $9b
ram_9C          = $9c
ram_9D          = $9d
ram_9E          = $9e
ram_9F          = $9f
;                 $a0  (i)
;                 $a1  (i)
;                 $a2  (i)
;                 $a3  (i)
;                 $a4  (i)
;                 $a5  (i)
ram_A6          = $a6
;                 $a7  (i)
;                 $a8  (i)
;                 $a9  (i)
;                 $aa  (i)
;                 $ab  (i)
;                 $ac  (i)
ram_AD          = $ad
ram_AE          = $ae
;                 $af  (i)
;                 $b0  (i)
;                 $b1  (i)
;                 $b2  (i)
;                 $b3  (i)
;                 $b4  (i)
ram_B5          = $b5
ram_B6          = $b6
;                 $b7  (i)
;                 $b8  (i)
;                 $b9  (i)
;                 $ba  (i)
;                 $bb  (i)
;                 $bc  (i)
ram_BD          = $bd
ram_BE          = $be
;                 $bf  (i)
;                 $c0  (i)
ram_C1          = $c1
;                 $c2  (i)
;                 $c3  (i)
;                 $c4  (i)
ram_C5          = $c5
ram_C6          = $c6
;                 $c7  (i)
;                 $c8  (i)
;                 $c9  (i)
;                 $ca  (i)
;                 $cb  (i)
;                 $cc  (i)
ram_CD          = $cd
ram_CE          = $ce
;                 $cf  (i)
;                 $d0  (i)
;                 $d1  (i)
;                 $d2  (i)
;                 $d3  (i)
;                 $d4  (i)
ram_D5          = $d5
ram_D6          = $d6
;                 $d7  (i)
;                 $d8  (i)
;                 $d9  (i)
;                 $da  (i)
;                 $db  (i)
;                 $dc  (i)
ram_DD          = $dd
ram_DE          = $de
ram_DF          = $df
ram_E0          = $e0
ram_E1          = $e1
ram_E2          = $e2
ram_E3          = $e3
;                 $e4  (i)
ram_E5          = $e5
ram_E6          = $e6
ram_E7          = $e7
timer_minute    = $e8
timer_seconds   = $e9
timer_t_seconds = $ea
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
ram_FA          = $fa
ram_FB          = $fb
;                 $fc  (is)
;                 $fd  (s)
;                 $fe  (s)
;                 $ff  (s)


;-----------------------------------------------------------
;      User Defined Labels
;-----------------------------------------------------------

Start           = $f000
ROM_BASE        = $f000

   IF PLUSROM

WriteToBuffer           = $1FF0
WriteSendBuffer         = $1FF1
ReceiveBuffer           = $1FF2
ReceiveBufferSize       = $1FF3

HIGHSCORE_ID            = 79         ; Skiing game ID in Highscore DB

   ENDIF


;***********************************************************
;      Bank 0
;***********************************************************

    SEG     CODE
    ORG     ROM_BASE

Start
    sei                             ;2        

   IF COMPILE_REGION = NTSC
    cld                             ;2        
    ldx     #$ff                    ;2        
    txs                             ;2        
    inx                             ;2        
   ELSE
    ldx     #$00                    ;2        
   ENDIF

    txa                             ;2   =  12
Lf007
    sta     VSYNC,x                 ;4        
   IF COMPILE_REGION = PAL50
    txs                             ;2        
   ENDIF
    inx                             ;2        
    bne     Lf007                   ;2/3      
    dec     game_nr                 ;5        
    jmp     Lf3c6                   ;3   =  16
    
Lf011
    lda     (ram_95),y              ;5   =   5
Lf013
    dey                             ;2        
    sty     ram_F3                  ;3        
    ldx     ram_9B                  ;3   =   8
Lf018
    dec     ram_8D                  ;5        
    sta     WSYNC                   ;3   =   8
;---------------------------------------
    sta     GRP0,x                  ;4        
    beq     Lf062                   ;2/3      
    ldx     ram_EE                  ;3        
    lda     ram_8D                  ;3        
    cmp     ram_D6,x                ;4        
    beq     Lf03c                   ;2/3      
    sec                             ;2        
    sbc     ram_9A                  ;3        
    cmp     #$14                    ;2        
    tay                             ;2        
    lda     ram_AE,x                ;4        
    sta     ram_97                  ;3        
    lda     ram_C6,x                ;4        
    sta     ram_F2                  ;3   =  41
Lf036
    bcc     Lf011                   ;2/3      
    lda     #$00                    ;2        
    beq     Lf013                   ;2/3 =   6
Lf03c
    bit     CXPPMM                  ;3        
    bmi     Lf042                   ;2/3      
    stx     ram_EF                  ;3   =   8
Lf042
    lda     ram_9E,x                ;4        
    ldx     ram_9C                  ;3        
    sta     HMP0,x                  ;4        
    sta     REFP0,x                 ;4        
    ldy     ram_F3                  ;3        
    dec     ram_F3                  ;5        
    cpy     #$14                    ;2        
    bcc     Lf056                   ;2/3      
    lda     #$00                    ;2        
    bcs     Lf058                   ;2/3 =  31
Lf056
    lda     (ram_95),y              ;5   =   5
Lf058
    ldx     ram_9B                  ;3        
    ldy     ram_EE                  ;3        
    dec     ram_8D                  ;5        
    sta     WSYNC                   ;3   =  14
;---------------------------------------
    sta     GRP0,x                  ;4   =   4
Lf062
    beq     Lf0ac                   ;2/3      
    ldx     ram_A6,y                ;4        
    bmi     Lf07f                   ;2/3 =   8
Lf068
    dex                             ;2        
    bpl     Lf068                   ;2/3      
    ldx     ram_9C                  ;3        
    sta     RESP0,x                 ;4        
    lda.wy  ram_CE,y                ;4        
    tay                             ;2        
    lda     Lf7bb,y                 ;4        
    eor     ram_F9                  ;3        
    and     ram_ED                  ;3        
    sta     COLUP0,x                ;4        
    jmp     Lf096                   ;3   =  34
    
Lf07f
    lda.wy  ram_CE,y                ;4        
    tay                             ;2        
    lda     Lf7bb,y                 ;4        
    eor     ram_F9                  ;3        
    and     ram_ED                  ;3        
    ldy     ram_9C                  ;3        
    sta.wy  COLUP0,y                ;5   =  24
Lf08f
    dex                             ;2        
    bmi     Lf08f                   ;2/3      
    ldx     ram_9C                  ;3        
    sta     RESP0,x                 ;4   =  11
Lf096
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    ldy     ram_F3                  ;3        
    cpy     #$14                    ;2        
    bcc     Lf0a4                   ;2/3      
    lda     #$00                    ;2        
    bcs     Lf0a6                   ;2/3 =  14
Lf0a4
    lda     (ram_95),y              ;5   =   5
Lf0a6
    ldx     ram_9B                  ;3        
    sta     GRP0,x                  ;4        
    dec     ram_8D                  ;5   =  12
Lf0ac
    beq     Lf105                   ;2/3!     
    ldx     ram_EE                  ;3        
    ldy     ram_B6,x                ;4        
    ldx     ram_9C                  ;3        
    bpl     Lf0ce                   ;2/3 =  14
Lf0b6
    bcc     Lf0ba                   ;2/3      
    eor     #$05                    ;2   =   4
Lf0ba
    ldy     ram_F3                  ;3        
    sta     WSYNC                   ;3   =   6
;---------------------------------------
    sta     NUSIZ0,x                ;4        
    cpy     #$14                    ;2        
    bcc     Lf0ea                   ;2/3      
    lda     #$00                    ;2        
    bcs     Lf0ec                   ;2/3 =  12
Lf0c8
    lda     (ram_97),y              ;5        
    ldx     ram_9C                  ;3        
    sta     GRP0,x                  ;4   =  12
Lf0ce
    sty     ram_F4                  ;3        
    dec     ram_F3                  ;5        
    lda     ram_F2                  ;3        
    cpy     #$10                    ;2        
    bne     Lf0b6                   ;2/3      
    sta     HMP0,x                  ;4        
    ldy     ram_F3                  ;3        
    cpy     #$14                    ;2        
    sta     WSYNC                   ;3   =  27
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     NUSIZ0,x                ;4        
    bcc     Lf0ea                   ;2/3      
    lda     #$00                    ;2        
    bcs     Lf0ec                   ;2/3 =  13
Lf0ea
    lda     (ram_95),y              ;5   =   5
Lf0ec
    ldx     ram_9B                  ;3        
    sta     GRP0,x                  ;4        
    dec     ram_8D                  ;5        
    beq     Lf103                   ;2/3!     
    ldy     ram_F4                  ;3        
    dey                             ;2        
    bpl     Lf0c8                   ;2/3      
    inc     ram_EE                  ;5        
    ldy     ram_F3                  ;3        
    dey                             ;2        
    cpy     #$14                    ;2        
    jmp     Lf036                   ;3   =  36
    
Lf103
    inc     ram_EE                  ;5   =   5
Lf105
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    ldx     #$00                    ;2        
    stx     VDELP0                  ;3        
    stx     VDELP1                  ;3        
    stx     GRP1                    ;3        
    stx     GRP0                    ;3        
    stx     REFP0                   ;3        
    stx     REFP1                   ;3        
    lda     #$38                    ;2        
    jsr     Lf691                   ;6        
    lda     #$3f                    ;2        
    inx                             ;2        
    jsr     Lf691                   ;6        
    ldx     #$09                    ;2   =  40
Lf122
    lda     Lf7c3,x                 ;4        
    sta     ram_81,x                ;4        
    dex                             ;2        
    bpl     Lf122                   ;2/3      
    jsr     Lf4f8                   ;6        

   IF COMPILE_REGION = NTSC
    ldy     #$25                    ;2        
   ELSE
    ldy     #$37                    ;2        
    sta     WSYNC                   ;3   =  23
;---------------------------------------
    sty     VBLANK                  ;3        
   ENDIF

    sty     TIM64T                  ;4        
    ldy     ram_91                  ;3        
    beq     Lf174                   ;2/3      
    sta     AUDC1                   ;3        
    sta     ram_DF                  ;3        
    bmi     Lf171                   ;2/3      
    cpy     #$0a                    ;2        
    bcs     Lf16f                   ;2/3      
    lda     SWCHA                   ;4        
    dey                             ;2        
    beq     Lf16b                   ;2/3      
    jsr     Lf59c                   ;6        
    ldx     ram_91                  ;3        
    lda     Lf7f2,x                 ;4        
    sta     ram_DD                  ;3        
    and     #$80                    ;2        
    ora     ram_BD                  ;3        
    sta     ram_BD                  ;3        
    ldx     game_nr                 ;3        
    ldy     #$aa                    ;2        
    cpx     #$05                    ;2        
    bcc     Lf160                   ;2/3      
    ldy     #$0a                    ;2   =  84 *
Lf160
    sty     ram_EC                  ;3        
    lda     Lf7e6,x                 ;4        
    and     #$f0                    ;2        
    sta     ram_EB                  ;3        
    sta     ram_F6                  ;3   =  15
Lf16b
    cmp     #$f0                    ;2        
    bcs     Lf171                   ;2/3 =   4
Lf16f
    dec     ram_91                  ;5   =   5
Lf171
    jmp     Lf37f                   ;3   =   3
    
Lf174
    sty     ram_E7                  ;3        
    sty     ram_E6                  ;3        
    clc                             ;2        
    lda     ram_9D                  ;3        
   IF COMPILE_REGION = NTSC
    adc     #$aa                    ;2        
   ELSE
    sec                             ;2        
    nop                             ;2        
   ENDIF
    sta     ram_9D                  ;3        
    lda     #$01                    ;2        
    sed                             ;2        
    adc     timer_t_seconds         ;3        
    sta     timer_t_seconds         ;3        
    tya                             ;2        
    jsr     Lf55e                   ;6        
    bcc     Lf18e                   ;2/3      
    stx     ram_91                  ;3   =  39 *
Lf18e
    lda     CXPPMM                  ;3        
    bpl     Lf1f7                   ;2/3      
    ldx     ram_EF                  ;3        
    lda     ram_CE,x                ;4        
    and     #$03                    ;2        
    cmp     #$01                    ;2        
    beq     Lf1ab                   ;2/3      
    lda     ram_D6,x                ;4        
    bcs     Lf1a4                   ;2/3      
    sbc     #$06                    ;2        
    bcc     Lf1a8                   ;2/3 =  28
Lf1a4
    sbc     ram_B6,x                ;4        
    bcs     Lf1a9                   ;2/3 =   6
Lf1a8
    tya                             ;2   =   2 *
Lf1a9
    cmp     ram_9A                  ;3   =   3
Lf1ab
    tya                             ;2        
    bcs     Lf1b0                   ;2/3      
    eor     #$01                    ;2   =   6
Lf1b0
    sta     ram_9B                  ;3        
    eor     #$01                    ;2        
    sta     ram_9C                  ;3        
    bit     ram_8F                  ;3        
    bvs     Lf1f7                   ;2/3      
    ldy     ram_CE,x                ;4        
    lda     #$f8                    ;2        
    clc                             ;2        
    adc     ram_D6,x                ;4        
    sec                             ;2        
    sbc     ram_B6,x                ;4        
    sec                             ;2        
    sbc     ram_9A                  ;3        
    cmp     #$f8                    ;2        
    bcc     Lf1f7                   ;2/3      
    tya                             ;2        
    and     #$03                    ;2        
    beq     Lf1f7                   ;2/3      
    tay                             ;2        
    dey                             ;2        
    bne     Lf1d8                   ;2/3      
    sty     ram_FA                  ;3        
    beq     Lf1e3                   ;2/3 =  57
Lf1d8
    lda     ram_BE,x                ;4         *
    sbc     #$09                    ;2         *
    sec                             ;2         *
    sbc     ram_99                  ;3         *
    cmp     #$ef                    ;2         *
    bcc     Lf1f7                   ;2/3 =  15 *
Lf1e3
    bit     ram_F5                  ;3        
    bmi     Lf1f9                   ;2/3      
    tya                             ;2        
    bne     Lf1f4                   ;2/3      
    lda     ram_E0                  ;3        
    and     #$0f                    ;2        
    bne     Lf1f7                   ;2/3      
    bit     ram_F8                  ;3         *
    bpl     Lf1f7                   ;2/3 =  21 *
Lf1f4
    jsr     Lf575                   ;6   =   6 *
Lf1f7
    ldy     CXPPMM                  ;3   =   3
Lf1f9
    sty     ram_F5                  ;3        
    ldx     #$07                    ;2   =   5
Lf1fd
    lda     ram_CE,x                ;4        
    and     #$03                    ;2        
    bne     Lf249                   ;2/3      
    bit     ram_92                  ;3        
    bpl     Lf249                   ;2/3      
    lda     ram_D6,x                ;4        
    sec                             ;2        
    sbc     ram_B6,x                ;4        
    sec                             ;2        
    sbc     ram_9A                  ;3        
    sec                             ;2        
    sbc     #$07                    ;2        
    cmp     #$04                    ;2        
    bcs     Lf249                   ;2/3      
    lda     ram_99                  ;3        
    cmp     #$40                    ;2        
    bcs     Lf226                   ;2/3      
    lda     ram_BE,x                ;4        
    cmp     #$64                    ;2        
    lda     ram_99                  ;3        
    bcc     Lf226                   ;2/3      
    adc     #$9f                    ;2   =  56 *
Lf226
    clc                             ;2        
    sbc     #$02                    ;2        
    sec                             ;2        
    sbc     ram_BE,x                ;4        
    cmp     #$1c                    ;2        
    bcc     Lf239                   ;2/3      
    sbc     #$20                    ;2         *
    cmp     #$dc                    ;2         *
    bcc     Lf24c                   ;2/3       *
    jsr     Lf575                   ;6   =  26 *
Lf239
    lda     ram_EB                  ;3        
    sec                             ;2        
    sed                             ;2        
    sbc     #$01                    ;2        
    cld                             ;2        
    sta     ram_EB                  ;3        
    lda     #$08                    ;2        
    jsr     Lf58d                   ;6        
    bcs     Lf24c                   ;2/3 =  24
Lf249
    dex                             ;2        
    bpl     Lf1fd                   ;2/3!=   4
Lf24c
    lda     ram_8F                  ;3        
    and     #$0f                    ;2        
    tax                             ;2        
    lda     SWCHA                   ;4        
    bit     ram_8F                  ;3        
    bvs     Lf25e                   ;2/3      
    cmp     #$c0                    ;2        
    ldy     #$00                    ;2        
    bcs     Lf262                   ;2/3 =  22
Lf25e
    ldy     ram_E2                  ;3        
    bne     Lf283                   ;2/3 =   5
Lf262
    sty     AUDC1                   ;3        
    sty     ram_E2                  ;3        
    stx     ram_8F                  ;3        
    asl                             ;2        
    bcc     Lf26f                   ;2/3      
    bmi     Lf287                   ;2/3      
    dex                             ;2        
    dex                             ;2   =  19
Lf26f
    inx                             ;2        
    cpx     #$10                    ;2        
    bcs     Lf276                   ;2/3      
    stx     ram_8F                  ;3   =   9
Lf276
    jsr     Lf552                   ;6        
    iny                             ;2        
    iny                             ;2        
    lda     ram_E5                  ;3        
    bne     Lf283                   ;2/3      
    lda     #$08                    ;2        
    sta     AUDC1                   ;3   =  20
Lf283
    dey                             ;2        
    sty     ram_E2                  ;3        
    iny                             ;2   =   7
Lf287
    sty     ram_E5                  ;3        
    jsr     Lf552                   ;6        
    lsr                             ;2        
    tax                             ;2        
    lda     #$a0                    ;2        
    bit     ram_8F                  ;3        
    bvs     Lf2a6                   ;2/3      
    lda     Lf7a4,y                 ;4        
    cmp     ram_8E                  ;3        
    beq     Lf2a3                   ;2/3      
    bcs     Lf29f                   ;2/3      
    dec     ram_8E                  ;5   =  36
Lf29f
    bcc     Lf2a3                   ;2/3      
    inc     ram_8E                  ;5   =   7
Lf2a3
    lda     Lf7f8,x                 ;4   =   4
Lf2a6
    sta     ram_95                  ;3        
    jsr     Lf552                   ;6        
    cmp     #$04                    ;2        
    bcc     Lf2b8                   ;2/3      
   IF COMPILE_REGION = NTSC
    ldx     #$04                    ;2        
    cpx     game_nr                 ;3        
    bcs     Lf2b8                   ;2/3      
   ELSE
    ldx     game_nr                 ;3        
    cpx     #$05                    ;2        
    nop                             ;2        
    nop                             ;2        
   ENDIF
    lda     Lf7ec,y                 ;4   =  24 *
Lf2b8
    adc     #$04                    ;2        
    ldx     #$01                    ;2   =   4
Lf2bc
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    cmp     ram_93,x                ;4        
    beq     Lf2cc                   ;2/3      
    bcc     Lf2c8                   ;2/3      
    inc     ram_93,x                ;6   =  22
Lf2c8
    bcs     Lf2cc                   ;2/3      
    dec     ram_93,x                ;6   =   8
Lf2cc
    lda     ram_93,x                ;4        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    sec                             ;2        
    sbc     #$04                    ;2        
    beq     Lf2dd                   ;2/3      
    bcs     Lf2db                   ;2/3      
    sbc     #$01                    ;2   =  22
Lf2db
    adc     #$00                    ;2   =   2
Lf2dd
    sta     ram_EF                  ;3        
    ldy     game_nr                 ;3        
    lda     Lf7e6,y                 ;4        
    lsr                             ;2        
    lda     ram_8E                  ;3        
    bcs     Lf2ec                   ;2/3      
    adc     #$20                    ;2        
    ror                             ;2   =  21
Lf2ec
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    tay                             ;2        
    lda     #$00                    ;2        
    beq     Lf2f8                   ;2/3 =  16
Lf2f6
    adc     ram_EF                  ;3   =   3
Lf2f8
    clc                             ;2        
    dey                             ;2        
    bpl     Lf2f6                   ;2/3      
    iny                             ;2        
    adc     ram_E3,x                ;4        
    bpl     Lf30d                   ;2/3 =  14
Lf301
    cmp     #$df                    ;2        
    bcs     Lf311                   ;2/3      
    adc     #$20                    ;2        
    dey                             ;2        
    bne     Lf301                   ;2/3 =  10
Lf30a
    iny                             ;2        
    sbc     #$20                    ;2   =   4
Lf30d
    cmp     #$20                    ;2        
    bcs     Lf30a                   ;2/3 =   4
Lf311
    sty     ram_F3,x                ;4        
    sta     ram_E3,x                ;4        
    clc                             ;2        
    lda     ram_8F                  ;3        
    adc     #$01                    ;2        
    lsr                             ;2        
    and     #$0f                    ;2        
    dex                             ;2        
    bpl     Lf2bc                   ;2/3!     
    lda     #$4c                    ;2        
    bit     ram_F8                  ;3        
    bvs     Lf334                   ;2/3      
    tya                             ;2        
    ldy     #$00                    ;2        
    clc                             ;2        
    adc     ram_99                  ;3        
    cmp     #$08                    ;2        
    bcc     Lf336                   ;2/3      
    cmp     #$90                    ;2        
    bcs     Lf336                   ;2/3 =  47
Lf334
    sta     ram_99                  ;3   =   3
Lf336
    sty     ram_DF                  ;3        
    ldx     #$07                    ;2        
    clc                             ;2        
    lda     ram_92                  ;3        
    bmi     Lf344                   ;2/3      
    clc                             ;2        
    adc     ram_F4                  ;3        
    sta     ram_92                  ;3   =  20
Lf344
    lda     ram_D6,x                ;4        
    clc                             ;2        
    adc     ram_F4                  ;3        
    bcs     Lf34d                   ;2/3      
    sta     ram_D6,x                ;4   =  15
Lf34d
    dex                             ;2        
    bpl     Lf344                   ;2/3      
    ldx     #$04                    ;2        
    cpx     game_nr                 ;3        
    bcs     Lf379                   ;2/3      
    lda     INPT4                   ;3         *
    bit     ram_F8                  ;3         *
    bmi     Lf35e                   ;2/3       *
    lda     ram_FA                  ;3   =  22 *
Lf35e
    ldx     ram_E0                  ;3         *
    bne     Lf36b                   ;2/3       *
    tay                             ;2         *
    bmi     Lf379                   ;2/3       *
    lda     #$09                    ;2         *
    tay                             ;2         *
    jsr     Lf593                   ;6   =  19 *
Lf36b
    ldy     #$7c                    ;2         *
    inx                             ;2         *
    cpx     #$10                    ;2         *
    beq     Lf379                   ;2/3       *
    bcc     Lf37b                   ;2/3       *
    ldx     #$00                    ;2         *
    asl                             ;2         *
    bcc     Lf37f                   ;2/3 =  16 *
Lf379
    ldy     #$78                    ;2   =   2
Lf37b
    sty     ram_9A                  ;3        
    stx     ram_E0                  ;3   =   6
Lf37f
    ldy     INTIM                   ;4        
    bne     Lf37f                   ;2/3      
    dey                             ;2        
    sta     WSYNC                   ;3   =  11
;---------------------------------------
    sty     VSYNC                   ;3        
   IF COMPILE_REGION = NTSC
    sty     VBLANK                  ;3        
   ENDIF
    sty     ram_EF                  ;3        
    ldx     #$05                    ;2        
    stx     CTRLPF                  ;3        
    dex                             ;2        
    inc     ram_80                  ;5        
    bne     Lf39c                   ;2/3      
    inc     ram_E6                  ;5        
    bne     Lf39c                   ;2/3      
    sty     ram_E7                  ;3   =  33 *
Lf39c
    lda     ram_F8                  ;3        
    and     #$08                    ;2        
    bne     Lf3a4                   ;2/3      
    ldy     #$0f                    ;2   =   9
Lf3a4
    tya                             ;2        
    ldy     ram_E7                  ;3        
    bpl     Lf3ab                   ;2/3      
    and     #$f7                    ;2   =   9 *
Lf3ab
    sta     ram_ED                  ;3   =   3
Lf3ad
    lda     ram_E6                  ;3        
    and     ram_E7                  ;3        
    eor     Lf7fb,x                 ;4        
    and     ram_ED                  ;3        
    sta     ram_F8,x                ;4        
    sta     NUSIZ1,x                ;4        
    dex                             ;2        
    bne     Lf3ad                   ;2/3      
    stx     COLUPF                  ;3        
    stx     ram_EE                  ;3        
    lda     SWCHB                   ;4        
    sta     ram_F8                  ;3   =  38
Lf3c6
   IF COMPILE_REGION = NTSC
    ldy     #$2c                    ;2        
   ELSE
    ldy     #$54                    ;2        
   ENDIF
    lsr                             ;2        
    sta     WSYNC                   ;3   =   7
;---------------------------------------
    stx     VSYNC                   ;3        
    sty     TIM64T                  ;4        
    ldy     game_nr                 ;3        
    ror                             ;2        
    bpl     Lf3dd                   ;2/3      
    rol                             ;2        
    cpy     #$05                    ;2        
    ror                             ;2        
    ora     INPT4                   ;3        
    bmi     Lf3f6                   ;2/3 =  25
Lf3dd
    lda     Lf7d9,y                 ;4        
    bne     Lf3e4                   ;2/3      
    lda     ram_F7                  ;3   =   9 *
Lf3e4
    sta     ram_F0                  ;3        
    sta     ram_F1                  ;3        
    ldx     #$3d                    ;2   =   8
Lf3ea
    lda     #$00                    ;2        
    sta     ram_AD,x                ;4        
    lda     Lf7ab,x                 ;4        
    sta     ram_8D,x                ;4        
    dex                             ;2        
    bne     Lf3ea                   ;2/3 =  18
Lf3f6
    bcs     Lf423                   ;2/3!     
    dec     ram_8B                  ;5        
    bpl     Lf425                   ;2/3!     
    iny                             ;2        
    tya                             ;2        
    cmp     #$0a                    ;2        
    bcc     Lf403                   ;2/3      
    txa                             ;2   =  19
Lf403
    sta     game_nr                 ;3        
    sed                             ;2        
    clc                             ;2        
    adc     #$01                    ;2        
    cld                             ;2        
    sta     ram_EB                  ;3        
    stx     ram_E6                  ;3        
    stx     ram_E7                  ;3        
    ldy     ram_80                  ;3        
    bne     Lf415                   ;2/3      
    tay                             ;2   =  27
Lf415
    sty     ram_F7                  ;3        
    lda     #$aa                    ;2        
    sta     ram_EC                  ;3        
    sta     timer_t_seconds         ;3        
    sta     timer_seconds           ;3        
    sta     ram_91                  ;3        
    ldx     #$1d                    ;2   =  19
Lf423
    stx     ram_8B                  ;3   =   3
Lf425
    sec                             ;2        
    lda     ram_DD                  ;3        
    sbc     #$05                    ;2        
    bcc     Lf439                   ;2/3      
    sbc     ram_BD                  ;3        
    bcc     Lf439                   ;2/3      
    ldx     ram_B6                  ;3        
    bpl     Lf439                   ;2/3      
    sta     ram_DE                  ;3        
    jsr     Lf59c                   ;6   =  28
Lf439
    ldx     ram_EE                  ;3        
    ldy     ram_D6,x                ;4        
    lda     ram_B6,x                ;4        
    bpl     Lf45b                   ;2/3 =  13
Lf441
    sbc     #$01                    ;2        
    bpl     Lf44d                   ;2/3      
    lda     #$ff                    ;2        
    sta     ram_B6,x                ;4        
    inc     ram_EE                  ;5        
    bpl     Lf439                   ;2/3 =  17
Lf44d
    cmp     #$0f                    ;2        
    bne     Lf45a                   ;2/3      
    lda     #$fb                    ;2        
    jsr     Lf656                   ;6        
    sta     ram_BE,x                ;4        
    lda     #$0f                    ;2   =  18
Lf45a
    dey                             ;2   =   2
Lf45b
    cpy     #$96                    ;2        
    bcs     Lf441                   ;2/3      
    sty     ram_D6,x                ;4        
    sta     ram_B6,x                ;4   =  12
Lf463
    lda     ram_DF                  ;3        
    sec                             ;2        
    eor     #$ff                    ;2        
    jsr     Lf657                   ;6        
    sta     ram_BE,x                ;4        
    jsr     Lf66e                   ;6        
    lsr                             ;2        
    ora     ram_CE,x                ;4        
    asl                             ;2        
    sta     ram_9E,x                ;4        
    tya                             ;2        
    sbc     #$04                    ;2        
    eor     #$80                    ;2        
    bmi     Lf47e                   ;2/3      
    tya                             ;2   =  45
Lf47e
    sta     ram_A6,x                ;4        
    inx                             ;2        
    cpx     #$08                    ;2        
    bcc     Lf463                   ;2/3      
    ldx     ram_E1                  ;3        
    beq     Lf493                   ;2/3      
    dex                             ;2        
    bpl     Lf493                   ;2/3      
    inx                             ;2        
    inx                             ;2        
    inx                             ;2        
    bmi     Lf493                   ;2/3      
    ldx     #$0b                    ;2   =  29
Lf493
    txa                             ;2        
    lsr                             ;2        
    stx     ram_E1                  ;3        
    sta     AUDV0                   ;3   =  10
Lf499
    ldx     INTIM                   ;4        
    bne     Lf499                   ;2/3      
    stx     WSYNC                   ;3   =   9
;---------------------------------------
    stx     VBLANK                  ;3        
    ldx     #$04                    ;2        
    stx     AUDF1                   ;3   =   8
Lf4a6
    ldy     #$02                    ;2        
    sty     AUDF0                   ;3   =   5
Lf4aa
    dex                             ;2        
    lda     timer_seconds,x         ;4        
    and     #$0f                    ;2        
    sta.wy  ram_81,y                ;5        
    lda     timer_seconds,x         ;4        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    bne     Lf4c0                   ;2/3      
    cpx     #$02                    ;2        
    bne     Lf4c0                   ;2/3      
    lda     #$0a                    ;2   =  33
Lf4c0
    sta.wy  ram_85,y                ;5        
    dey                             ;2        
    dey                             ;2        
    bpl     Lf4aa                   ;2/3      
    jsr     Lf4f8                   ;6        
    bne     Lf4a6                   ;2/3      
    ldx     ram_9C                  ;3        
    ldy     #$3f                    ;2        
    sty     PF0                     ;3        
    sty     VDELP0,x                ;4        
    jsr     Lf54d                   ;6        
    ldx     ram_9B                  ;3        
    lda     ram_99                  ;3        
    jsr     Lf691                   ;6        
    tya                             ;2        
    eor     ram_8F                  ;3        
    sta     REFP0,x                 ;4        
    lda     #$97                    ;2        
    sta     ram_8D                  ;3        
    sta     AUDV1                   ;3        
    lda     ram_FB                  ;3        
    sta     COLUP0,x                ;4        
    lda     #$00                    ;2        
    sta     NUSIZ0,x                ;4        
    sta     HMCLR                   ;3        
    sta     CXCLR                   ;3        
    jmp     Lf018                   ;3   =  88
    
Lf4f8
    stx     ram_FA                  ;3        
    dex                             ;2        
    stx     HMP1                    ;3        
    jsr     Lf69b                   ;6        
    lda     #$01                    ;2        
    ldy     ram_83                  ;3        
    cpy     #$0a                    ;2        
    ldx     timer_minute            ;3        
    ldy     #$00                    ;2        
    bcs     Lf50f                   ;2/3      
    dey                             ;2        
    stx     ram_89                  ;3   =  33
Lf50f
    sty     ram_F4                  ;3        
    sec                             ;2        
    sta     NUSIZ1                  ;3        
    rol                             ;2        
    sta     NUSIZ0                  ;3        
    lda     ram_F9                  ;3        
    sta     COLUP0                  ;3        
    sta     COLUP1                  ;3        
    lda     #$70                    ;2   =  24
Lf51f
    sbc     #$0f                    ;2        
    tay                             ;2        
    lda     Lf70b,y                 ;4        
    and     ram_F4                  ;3        
    asl                             ;2        
    sta     WSYNC                   ;3   =  16
;---------------------------------------
    ora     (ram_85),y              ;5        
    sta     GRP1                    ;3        
    lda     (ram_89),y              ;5        
    sta     GRP0                    ;3        
    lda     (ram_83),y              ;5        
    tax                             ;2        
    txs                             ;2        
    lda     (ram_81),y              ;5        
    tax                             ;2        
    lda     #$00                    ;2        
    ror                             ;2        
    ora     (ram_87),y              ;5        
    stx     GRP0                    ;3        
    sta     GRP1                    ;3        
    tsx                             ;2        
    stx     GRP0                    ;3        
    tya                             ;2        
    bne     Lf51f                   ;2/3      
    ldx     #$fd                    ;2        
    txs                             ;2        
    ldx     ram_FA                  ;3   =  63
Lf54d
    sta     GRP1                    ;3        
    sta     GRP0                    ;3        
    rts                             ;6   =  12
    
Lf552
    lda     ram_8F                  ;3        
    and     #$0f                    ;2        
    cmp     #$08                    ;2        
    bcc     Lf55c                   ;2/3      
    eor     #$0f                    ;2   =  11
Lf55c
    tay                             ;2        
    rts                             ;6   =   8
    
Lf55e
    sed                             ;2        
    adc     timer_seconds           ;3        
    cld                             ;2        
    bcc     Lf566                   ;2/3      
    adc     #$9f                    ;2   =  11 *
Lf566
    cmp     #$60                    ;2        
    bcc     Lf56e                   ;2/3      
    adc     #$9f                    ;2         *
    inc     timer_minute            ;5   =  11 *
Lf56e
    sta     timer_seconds           ;3        
    lda     timer_minute            ;3        
    cmp     #$05                    ;2        
    rts                             ;6   =  14
    
Lf575
    bit     ram_92                  ;3         *
    bpl     Lf59b                   ;2/3       *
    lda     ram_8F                  ;3         *
    cmp     #$08                    ;2         *
    lda     #$00                    ;2         *
    sta     AUDC1                   ;3         *
    sta     ram_E1                  ;3         *
    sta     ram_8E                  ;3         *
    adc     #$47                    ;2         *
    sta     ram_8F                  ;3         *
    sta     ram_E2                  ;3         *
    lda     #$02                    ;2   =  31 *
Lf58d
    ldy     #$74                    ;2        
    sty     ram_92                  ;3        
    ldy     #$e0                    ;2   =   7
Lf593
    bit     ram_E1                  ;3        
    bmi     Lf59b                   ;2/3      
    sta     AUDC0                   ;3        
    sty     ram_E1                  ;3   =  11
Lf59b
    rts                             ;6   =   6
    
Lf59c
    ldy     ram_90                  ;3        
    dey                             ;2        
    tya                             ;2        
    and     #$07                    ;2        
    sta     ram_90                  ;3        
    cmp     #$06                    ;2        
    bne     Lf5c2                   ;2/3      
    lda     ram_F6                  ;3        
    sed                             ;2        
    sbc     #$02                    ;2        
    cld                             ;2        
    sta     ram_F6                  ;3        
    bcs     Lf5c2                   ;2/3      
    ldx     #$04                    ;2   =  32

   IF PLUSROM = 1

    jmp SendPlusROMScore
ReturnFromSendPlusROMScore

   ELSE

Lf5b4
    lda     ram_EB                  ;3        
    clc                             ;2        
    jsr     Lf55e                   ;6        
    dex                             ;2        
    bpl     Lf5b4                   ;2/3 =  15

   ENDIF

Lf5bd
    stx     ram_91                  ;3        
    stx     ram_DE                  ;3        
    rts                             ;6   =  12

   IF PLUSROM = 1

    ORG $f5c2

   ENDIF
    
Lf5c2
    ldx     #$00                    ;2   =   2
Lf5c4
    lda     ram_9F,x                ;4        
    sta     ram_9E,x                ;4        
    inx                             ;2        
    cpx     #$40                    ;2        
    bcc     Lf5c4                   ;2/3      
    lda     ram_90                  ;3        
    tay                             ;2        
    ora     #$04                    ;2        
    tax                             ;2        
    and     #$03                    ;2        
    bne     Lf5fd                   ;2/3      
    tay                             ;2        
    lda     ram_F6                  ;3        
    bne     Lf5e4                   ;2/3      
    ldy     #$04                    ;2        
    lda     ram_90                  ;3        
    bne     Lf5e4                   ;2/3      
    inx                             ;2        
    iny                             ;2   =  45
Lf5e4
    lda     game_nr                 ;3        
    cmp     #$05                    ;2        
    bcc     Lf5fd                   ;2/3      
    sed                             ;2         *
    lda     ram_EB                  ;3         *
    sbc     #$01                    ;2         *
    sta     ram_EB                  ;3         *
    cld                             ;2         *
    tax                             ;2         *
    bne     Lf5f9                   ;2/3       *
    dex                             ;2         *
    jsr     Lf5bd                   ;6   =  31 *
Lf5f9
    lda     #$07                    ;2         *
    tax                             ;2         *
    tay                             ;2   =   6 *
Lf5fd
    lda     Lf7cd,x                 ;4        
    sta     ram_BD                  ;3        
    sty     ram_D5                  ;3        
    lda     Lf7c9,x                 ;4        
    sta     ram_B5                  ;3        
    lda     Lf7d1,x                 ;4        
    sta     ram_CD                  ;3        
    lda     ram_F0                  ;3        
    asl                             ;2        
    eor     ram_F0                  ;3        
    asl                             ;2        
    asl                             ;2        
    rol     ram_F1                  ;5        
    rol     ram_F0                  ;5        
    lda     ram_F1                  ;3        
    and     #$3f                    ;2        
    sec                             ;2        
    sbc     #$20                    ;2        
    ldy     game_nr                 ;3        
    cpy     #$05                    ;2        
    bcs     Lf64e                   ;2/3      
    adc     Lf7de,x                 ;4        
    bit     ram_F8                  ;3        
    bmi     Lf633                   ;2/3      
    clc                             ;2        
    adc     Lf6f8,x                 ;4        
    bit     ram_F8                  ;3   =  80
Lf633
    bvs     Lf64f                   ;2/3      
    cpx     #$04                    ;2        
    bne     Lf64f                   ;2/3      
    clc                             ;2        
    adc     ram_C1                  ;3        
    cmp     #$ec                    ;2        
    bcs     Lf644                   ;2/3      
    cmp     #$10                    ;2        
    bcs     Lf646                   ;2/3 =  19
Lf644
    adc     #$28                    ;2   =   2 *
Lf646
    cmp     #$70                    ;2        
    bcc     Lf653                   ;2/3      
    sbc     #$28                    ;2         *
    bcs     Lf646                   ;2/3 =   8 *
Lf64e
    asl                             ;2   =   2 *
Lf64f
    dex                             ;2        
    jsr     Lf656                   ;6   =   8
Lf653
    sta     ram_C5                  ;3        
    rts                             ;6   =   9
    
Lf656
    clc                             ;2   =   2
Lf657
    sta     ram_F3                  ;3        
    adc     ram_BE,x                ;4        
    bit     ram_F3                  ;3        
    bmi     Lf663                   ;2/3      
    bcc     Lf667                   ;2/3      
    bcs     Lf66b                   ;2/3 =  16 *
Lf663
    bcs     Lf66d                   ;2/3      
    adc     #$a0                    ;2   =   4 *
Lf667
    cmp     #$a0                    ;2        
    bcc     Lf66d                   ;2/3 =   4
Lf66b
    sbc     #$a0                    ;2   =   2
Lf66d
    rts                             ;6   =   6
    
Lf66e
    clc                             ;2        
    adc     #$02                    ;2        
    tay                             ;2        
    and     #$0f                    ;2        
    sta     ram_F4                  ;3        
    tya                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    tay                             ;2        
    clc                             ;2        
    adc     ram_F4                  ;3        
    cmp     #$0f                    ;2        
    bcc     Lf686                   ;2/3      
    sbc     #$0f                    ;2        
    iny                             ;2   =  36
Lf686
    eor     #$07                    ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    sta     WSYNC                   ;3   =  11
;---------------------------------------
    asl                             ;2        
    sta     HMCLR                   ;3        
    rts                             ;6   =  11
    
Lf691
    jsr     Lf66e                   ;6        
    sta     HMP0,x                  ;4   =  10
Lf696
    dey                             ;2        
    bpl     Lf696                   ;2/3      
    sta     RESP0,x                 ;4   =   8
Lf69b
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    rts                             ;6   =   9
    
    .byte   $0c,$1e,$3d,$5d,$ba,$38,$28,$ab ; $f6a0 (*)
    .byte   $6c,$38,$38,$6c,$44,$c3,$81,$00 ; $f6a8 (*)
    .byte   $00,$00,$00,$00,$0e,$38,$f7,$3c ; $f6b0 (D)
    .byte   $68,$28,$28,$38,$38,$3a,$5c,$2e ; $f6b8 (D)
    .byte   $1e,$0f,$03,$00,$08,$18,$12,$36 ; $f6c0 (D)
    .byte   $34,$6c,$f8,$b8,$28,$28,$38,$3a ; $f6c8 (D)
    .byte   $1e,$5c,$5c,$3e,$1e,$06,$00,$00 ; $f6d0 (D)
    .byte   $00,$00,$fe,$30,$7f,$2c,$38,$1c ; $f6d8 (D)
    .byte   $14,$1c,$1c,$4c,$2e,$1e,$0e,$07 ; $f6e0 (D)
    .byte   $03,$03,$28,$28,$28,$28,$28,$38 ; $f6e8 (D)
    .byte   $3c,$2c,$34,$3c,$7d,$5d,$5d,$7f ; $f6f0 (D)
Lf6f8
    .byte   $3e,$1e,$0c,$0c,$00,$00,$30,$78 ; $f6f8 (D)
    
    .byte   %00011110 ; |   #### |            $f700 (G)
    .byte   %00111111 ; |  ######|            $f701 (G)
    .byte   %00111111 ; |  ######|            $f702 (G)
    .byte   %00011110 ; |   #### |            $f703 (G)
    .byte   %00000110 ; |     ## |            $f704 (G)
    .byte   %00111110 ; |  ##### |            $f705 (G)
    .byte   %00011110 ; |   #### |            $f706 (G)
    .byte   %00001100 ; |    ##  |            $f707 (G)
    .byte   %00011110 ; |   #### |            $f708 (G)
    .byte   %00011110 ; |   #### |            $f709 (G)
    .byte   %00000000 ; |        |            $f70a (G)
    
Lf70b
    .byte   $80                             ; $f70b (D)
    
    .byte   %10101101 ; |# # ## #|            $f70c (G)
    .byte   %01010000 ; | # #    |            $f70d (G)
    .byte   %10111010 ; |# ### # |            $f70e (G)
    .byte   %11101001 ; |### #  #|            $f70f (G)
    .byte   %00110011 ; |  ##  ##|            $f710 (G)
    .byte   %00001100 ; |    ##  |            $f711 (G)
    .byte   %00110000 ; |  ##    |            $f712 (G)
    .byte   %00100011 ; |  #   ##|            $f713 (G)
    .byte   %00000110 ; |     ## |            $f714 (G)
    .byte   %00100011 ; |  #   ##|            $f715 (G)
    .byte   %00110011 ; |  ##  ##|            $f716 (G)
    .byte   %00001100 ; |    ##  |            $f717 (G)
    .byte   %00110011 ; |  ##  ##|            $f718 (G)
    .byte   %00100011 ; |  #   ##|            $f719 (G)
    .byte   %00000000 ; |        |            $f71a (G)
    
    .byte   $c0                             ; $f71b (D)
    
    .byte   %10101001 ; |# # #  #|            $f71c (G)
    .byte   %01011000 ; | # ##   |            $f71d (G)
    .byte   %10001010 ; |#   # # |            $f71e (G)
    .byte   %10101011 ; |# # # ##|            $f71f (G)
    .byte   %00110011 ; |  ##  ##|            $f720 (G)
    .byte   %00001100 ; |    ##  |            $f721 (G)
    .byte   %00110000 ; |  ##    |            $f722 (G)
    .byte   %00000011 ; |      ##|            $f723 (G)
    .byte   %00111111 ; |  ######|            $f724 (G)
    .byte   %00000011 ; |      ##|            $f725 (G)
    .byte   %00110011 ; |  ##  ##|            $f726 (G)
    .byte   %00001100 ; |    ##  |            $f727 (G)
    .byte   %00110011 ; |  ##  ##|            $f728 (G)
    .byte   %00000011 ; |      ##|            $f729 (G)
    .byte   %00000000 ; |        |            $f72a (G)
    
    .byte   $40                             ; $f72b (D)
    
    .byte   %11101001 ; |### #  #|            $f72c (G)
    .byte   %01011100 ; | # ###  |            $f72d (G)
    .byte   %10111010 ; |# ### # |            $f72e (G)
    .byte   %10101111 ; |# # ####|            $f72f (G)
    .byte   %00110011 ; |  ##  ##|            $f730 (G)
    .byte   %00001100 ; |    ##  |            $f731 (G)
    .byte   %00011110 ; |   #### |            $f732 (G)
    .byte   %00000110 ; |     ## |            $f733 (G)
    .byte   %00100110 ; |  #  ## |            $f734 (G)
    .byte   %00111110 ; |  ##### |            $f735 (G)
    .byte   %00111110 ; |  ##### |            $f736 (G)
    .byte   %00000110 ; |     ## |            $f737 (G)
    .byte   %00011110 ; |   #### |            $f738 (G)
    .byte   %00011111 ; |   #####|            $f739 (G)
    .byte   %00000000 ; |        |            $f73a (G)
    
    .byte   $00                             ; $f73b (D)
    
    .byte   %10101001 ; |# # #  #|            $f73c (G)
    .byte   %01010110 ; | # # ## |            $f73d (G)
    .byte   %10100010 ; |# #   # |            $f73e (G)
    .byte   %10101101 ; |# # ## #|            $f73f (G)
    .byte   %00110011 ; |  ##  ##|            $f740 (G)
    .byte   %00001100 ; |    ##  |            $f741 (G)
    .byte   %00000011 ; |      ##|            $f742 (G)
    .byte   %00000011 ; |      ##|            $f743 (G)
    .byte   %00010110 ; |   # ## |            $f744 (G)
    .byte   %00110000 ; |  ##    |            $f745 (G)
    .byte   %00110000 ; |  ##    |            $f746 (G)
    .byte   %00000011 ; |      ##|            $f747 (G)
    .byte   %00110011 ; |  ##  ##|            $f748 (G)
    .byte   %00110011 ; |  ##  ##|            $f749 (G)
    .byte   %00000000 ; |        |            $f74a (G)
    
    .byte   $40                             ; $f74b (D)
    
    .byte   %11101101 ; |### ## #|            $f74c (G)
    .byte   %01010011 ; | # #  ##|            $f74d (G)
    .byte   %00111010 ; |  ### # |            $f74e (G)
    .byte   %11101001 ; |### #  #|            $f74f (G)
    .byte   %00110011 ; |  ##  ##|            $f750 (G)
    .byte   %00111100 ; |  ####  |            $f751 (G)
    .byte   %00100011 ; |  #   ##|            $f752 (G)
    .byte   %00100011 ; |  #   ##|            $f753 (G)
    .byte   %00001110 ; |    ### |            $f754 (G)
    .byte   %00110000 ; |  ##    |            $f755 (G)
    .byte   %00110001 ; |  ##   #|            $f756 (G)
    .byte   %00100001 ; |  #    #|            $f757 (G)
    .byte   %00110011 ; |  ##  ##|            $f758 (G)
    .byte   %00110011 ; |  ##  ##|            $f759 (G)
    .byte   %00000000 ; |        |            $f75a (G)
    
    .byte   $40                             ; $f75b (D)
    
    .byte   %01000001 ; | #     #|            $f75c (G)
    .byte   %00010001 ; |   #   #|            $f75d (G)
    .byte   %10000000 ; |#       |            $f75e (G)
    .byte   %00000000 ; |        |            $f75f (G)
    .byte   %00011110 ; |   #### |            $f760 (G)
    .byte   %00011100 ; |   ###  |            $f761 (G)
    .byte   %00111110 ; |  ##### |            $f762 (G)
    .byte   %00011110 ; |   #### |            $f763 (G)
    .byte   %00000110 ; |     ## |            $f764 (G)
    .byte   %00111111 ; |  ######|            $f765 (G)
    .byte   %00011110 ; |   #### |            $f766 (G)
    .byte   %00111111 ; |  ######|            $f767 (G)
    .byte   %00011110 ; |   #### |            $f768 (G)
    .byte   %00011110 ; |   #### |            $f769 (G)
    .byte   %00000000 ; |        |            $f76a (G)
    
    .byte   $00                             ; $f76b (D)
    
    .byte   %00001111 ; |    ####|            $f76c (G)
    .byte   %11110000 ; |####    |            $f76d (G)
    .byte   %11111110 ; |####### |            $f76e (G)
    .byte   %00000000 ; |        |            $f76f (G)
    
    .byte   $08,$08,$08,$08,$08,$08,$08,$18 ; $f770 (D)
    .byte   $38,$78,$f8,$78,$38,$18,$00,$3c ; $f778 (D)
    .byte   $7e,$1f,$7f,$ff,$ff,$fe,$fe,$7e ; $f780 (D)
    .byte   $78,$7c,$7c,$3c,$3c,$1c,$3c,$f8 ; $f788 (D)
    .byte   $fe,$fc,$fc,$7c,$7c,$7c,$78,$78 ; $f790 (D)
    .byte   $38,$38,$30,$10,$10,$00,$7c,$ff ; $f798 (D)
    .byte   $ff,$7e,$1e,$0c                 ; $f7a0 (D)
Lf7a4
    .byte   $00,$20,$60,$a0,$e0,$e0,$e0     ; $f7a4 (D)
Lf7ab
    .byte   $e0,$80,$08,$10,$10,$80,$40,$80 ; $f7ab (D)
    .byte   $ea,$f6,$00,$f7,$4c,$78,$00,$01 ; $f7b3 (D)
Lf7bb
   IF COMPILE_REGION = NTSC
    .byte   $86,$0c,$d6,$d4,$44,$0a,$da,$c6 ; $f7bb (D)
   ELSE
    .byte   $b6,$0c,$36,$34,$64,$0a,$3a,$56 ; $f7bb (D)
   ENDIF
Lf7c3
    .byte   $0d,$f7,$0f,$f7,$0c,$f7         ; $f7c3 (D)
Lf7c9
    .byte   $0e,$f7,$0a,$f7                 ; $f7c9 (D)
Lf7cd
    .byte   $6f,$9d,$7e,$7e                 ; $f7cd (D)
Lf7d1
    .byte   $0f,$07,$1f,$1f,$02,$05,$55,$55 ; $f7d1 (D)
Lf7d9
    .byte   $53,$3c,$2f,$64,$00             ; $f7d9 (D)
Lf7de
    .byte   $23,$e7,$cb,$8e,$00,$08,$30,$e8 ; $f7de (D)
Lf7e6
    .byte   $22,$42,$33                     ; $f7e6 (D)
    .byte   $53,$33,$20                     ; $f7e9 (*)
Lf7ec
    .byte   $31,$51,$91,$91,$04,$06         ; $f7ec (*)
Lf7f2
   IF COMPILE_REGION = NTSC
    .byte   $08,$0b,$18,$28,$50,$a0         ; $f7f2 (D*)
   ELSE
    .byte   $09,$0a,$18,$28,$50,$a0         ; $f7f2 (D)
   ENDIF
Lf7f8
    .byte   $d6,$b0,$c3                     ; $f7f8 (D)
Lf7fb
    .byte   $ea                             ; $f7fb (D)
    .word   Start                           ; RESET vector for 2K
   IF COMPILE_REGION = NTSC
    .word   $0e48                           ; BRK vector for 2K
   ELSE
    .word   $0e68                           ; BRK vector for 2K
   ENDIF


   IF PLUSROM = 1

PlusROM_API
   .byte "a", 0, "h.firmaplus.de", 0

SendPlusROMScore
Lf5b4
    lda     ram_EB                  ;3     add penalty time here (+5 sec * missed gate)
    clc                             ;2        
    jsr     Lf55e                   ;6        
    dex                             ;2        
    bpl     Lf5b4                   ;2/3 =  15

    lda game_nr                      ; game number
    sta WriteToBuffer                ; 
    lda SWCHB                        ; difficulty switches
    sta WriteToBuffer                ; game nr. + difficulty * 10 (0-49)
    lda timer_minute
    sta WriteToBuffer
    lda timer_seconds
    sta WriteToBuffer
    lda timer_t_seconds
    sta WriteToBuffer
    lda #HIGHSCORE_ID                ; game id in Highscore DB
    sta WriteSendBuffer
    jmp ReturnFromSendPlusROMScore

    .org ROM_BASE + 4096 - 6, 0      ; 4K ROM
    .word (PlusROM_API - $E000)      ; PlusRom API pointer
    .word Start                      ; RESET vector
   IF COMPILE_REGION = NTSC
    .word   $0e48                           ; BRK vector for 2K
   ELSE
    .word   $0e68                           ; BRK vector for 2K
   ENDIF

   ENDIF




