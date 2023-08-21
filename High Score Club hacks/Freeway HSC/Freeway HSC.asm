; Disassembly of /Freeway.bin
; Disassembled Sun Aug 20 12:37:12 2023
; Using Stella 6.7
;
; ROM properties name : Freeway (1981) (Activision)
; ROM properties MD5  : 8e0ab801b1705a740b476b7f588c6d16
; and
; Disassembly of Freeway (Pal).bin
; Disassembled Mon Aug 21 00:31:53 2023
; Using Stella 6.7
;
; ROM properties name : Freeway (1981) (Activision) (PAL)
; ROM properties MD5  : 2ec6b045cfd7bc52d9cdfd1b1447d1e5
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

PLUSROM                 = 1
NTSC                    = 1
PAL50                   = 0

;-----------------------------------------------------------
;      Color constants
;-----------------------------------------------------------

    IF NTSC = 1
BLACK            = $00
YELLOW           = $10
RED              = $40
BLUE             = $80
GREEN            = $d0
    ELSE
BLACK            = $00
YELLOW           = $20
GREEN            = $50
RED              = $60
BLUE             = $b0
    ENDIF

;===============================================================================
; PlusROM hotspots and gameId for HSC backend
;===============================================================================
 
   IF PLUSROM

WriteToBuffer           = $1FF0
WriteSendBuffer         = $1FF1
ReceiveBuffer           = $1FF2
ReceiveBufferSize       = $1FF3

HIGHSCORE_ID            = 76         ; Freeway game ID in Highscore DB

   ENDIF

;-----------------------------------------------------------
;      TIA and IO constants accessed
;-----------------------------------------------------------

CXPPMM          = $07  ; (R)

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
RESP1           = $11  ; (W)
RESBL           = $14  ; (W)
AUDC0           = $15  ; (W)
AUDC1           = $16  ; (W)
AUDF0           = $17  ; (W)
AUDF1           = $18  ; (W)
AUDV0           = $19  ; (W)
AUDV1           = $1a  ; (W)
GRP0            = $1b  ; (W)
GRP1            = $1c  ; (W)
ENABL           = $1f  ; (W)
HMP0            = $20  ; (W)
HMP1            = $21  ; (W)
HMBL            = $24  ; (W)
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
;                 $8f  (i)
ram_90          = $90
ram_91          = $91
ram_92          = $92
ram_93          = $93
ram_94          = $94
ram_95          = $95
ram_96          = $96
ram_97          = $97
;                 $98  (i)
;                 $99  (i)
;                 $9a  (i)
;                 $9b  (i)
;                 $9c  (i)
;                 $9d  (i)
;                 $9e  (i)
;                 $9f  (i)
;                 $a0  (i)
ram_A1          = $a1
;                 $a2  (i)
;                 $a3  (i)
;                 $a4  (i)
;                 $a5  (i)
;                 $a6  (i)
;                 $a7  (i)
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
;                 $b3  (i)
;                 $b4  (i)
ram_B5          = $b5
ram_B6          = $b6
;                 $b7  (i)
;                 $b8  (i)
ram_B9          = $b9
;                 $ba  (i)
;                 $bb  (i)
;                 $bc  (i)
;                 $bd  (i)
;                 $be  (i)
;                 $bf  (i)
ram_C0          = $c0
ram_C1          = $c1
ram_C2          = $c2
ram_C3          = $c3
;                 $c4  (i)
;                 $c5  (i)
;                 $c6  (i)
;                 $c7  (i)
;                 $c8  (i)
;                 $c9  (i)
;                 $ca  (i)
;                 $cb  (i)
ram_CC          = $cc
ram_CD          = $cd
;                 $ce  (i)
;                 $cf  (i)
;                 $d0  (i)
;                 $d1  (i)
;                 $d2  (i)
;                 $d3  (i)
;                 $d4  (i)
;                 $d5  (i)
;                 $d6  (i)
ram_D7          = $d7
;                 $d8  (i)
ram_D9          = $d9
;                 $da  (i)
ram_DB          = $db
;                 $dc  (i)
ram_DD          = $dd
;                 $de  (i)
ram_DF          = $df
;                 $e0  (i)
ram_E1          = $e1
;                 $e2  (i)
ram_E3          = $e3
;                 $e4  (i)
ram_E5          = $e5
ram_E6          = $e6
ram_E7          = $e7
ram_E8          = $e8
ram_E9          = $e9
ram_EA          = $ea
ram_EB          = $eb
ram_EC          = $ec
;                 $ed  (i)
;                 $ee  (i)
;                 $ef  (i)
;                 $f0  (i)
;                 $f1  (i)
;                 $f2  (i)
;                 $f3  (i)
;                 $f4  (i)
;                 $f5  (i)
ram_F6          = $f6
ram_F7          = $f7
ram_F8          = $f8
ram_F9          = $f9
ram_FA          = $fa
ram_FB          = $fb
ram_FC          = $fc; (s)
ram_FD          = $fd; (s)
ram_FE          = $fe; (s)
;                 $ff  (is)


;-----------------------------------------------------------
;      User Defined Labels
;-----------------------------------------------------------

Start           = $f000


;***********************************************************
;      Bank 0
;***********************************************************

    SEG     CODE
    ORG     $f000

Start
    sei                             ;2        
    cld                             ;2        
    ldx     #$00                    ;2   =   6
Lf004
    lda     #$00                    ;2   =   2
Lf006
    sta     VSYNC,x                 ;4        
    txs                             ;2        
    inx                             ;2        
    bne     Lf006                   ;2/3      
    jsr     Lf5b0                   ;6   =  16
Lf00f
    ldx     #$05                    ;2   =   2
Lf011
    lda     Lf6f0,x                 ;4        
    eor     ram_86                  ;3        
    and     ram_87                  ;3        
    sta     ram_88,x                ;4        
    cpx     #$04                    ;2        
    bcs     Lf020                   ;2/3      
    sta     COLUP0,x                ;4   =  22
Lf020
    dex                             ;2        
    bpl     Lf011                   ;2/3      
    stx     ram_90                  ;3        
    stx     ram_91                  ;3        
    sta     WSYNC                   ;3   =  13
;---------------------------------------
    sta     RESBL                   ;3        
    lda     #$22                    ;2        
    sta     HMBL                    ;3        
    sta     ENABL                   ;3        
    lda     #$28                    ;2        
    inx                             ;2        
    stx     COLUPF                  ;3        
    jsr     Lf617                   ;6        
    lda     #$30                    ;2        
    sta     CTRLPF                  ;3        
    inx                             ;2        
    jsr     Lf617                   ;6        
    lda     #$04                    ;2        
    sta     NUSIZ0                  ;3        
    sta     NUSIZ1                  ;3        
    lda     ram_88                  ;3        
    ldy     ram_E6                  ;3        
    bne     Lf05d                   ;2/3      
    ldy     ram_E9                  ;3        
    cpy     #$20                    ;2        
    IF PLUSROM = 1
    jmp SendPlusROMScore
    nop
    ELSE
    bcc     Lf055                   ;2/3      
    inc     ram_E6                  ;5   =  65 *
    ENDIF

Lf055
    cpy     #$1e                    ;2        
    bcc     Lf05d                   ;2/3      
    lda     ram_81                  ;3         *
    and     ram_87                  ;3   =  10 *
Lf05d
    sta     COLUP0                  ;3        
    sta     COLUP1                  ;3   =   6
Lf061
    lda     INTIM                   ;4        
    bne     Lf061                   ;2/3      
    sta     WSYNC                   ;3   =   9
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     VBLANK                  ;3        
    sta     CXCLR                   ;3        
    ldy     #$07                    ;2   =  11
Lf070
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMCLR                   ;3        
    lda     (ram_DD),y              ;5        
    sta     GRP0                    ;3        
    lda     (ram_E1),y              ;5        
    sta     GRP1                    ;3        
    jsr     Lf613                   ;6        
    lda     (ram_DF),y              ;5        
    sta     GRP0                    ;3        
    lda     (ram_E3),y              ;5        
    sta     GRP1                    ;3        
    dey                             ;2        
    bpl     Lf070                   ;2/3      
    lda     #$40                    ;2        
    sta     HMP1                    ;3        
    sta     WSYNC                   ;3   =  53
;---------------------------------------
    sta     HMOVE                   ;3        
    iny                             ;2        
    sty     GRP0                    ;3        
    sty     GRP1                    ;3        
    lda     #$08                    ;2        
    sta     REFP0                   ;3        
    lda     ram_C0                  ;3        
    sta     ram_D9                  ;3        
    lda     ram_CC                  ;3        
    sta     ram_DB                  ;3        
    ldy     #$09                    ;2        
    sta     HMCLR                   ;3        
    sta     WSYNC                   ;3   =  36
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     ram_8C                  ;3        
    sta     COLUBK                  ;3        
    lda     ram_89                  ;3        
    sta     COLUP1                  ;3   =  15
Lf0b3
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     ram_8D                  ;3        
    cpy     #$01                    ;2        
    bne     Lf0bd                   ;2/3      
    lda     ram_8C                  ;3   =  10
Lf0bd
    sta     COLUBK                  ;3        
    lda     (ram_D9),y              ;5        
    sta     GRP1                    ;3        
    jsr     Lf615                   ;6        
    lda     (ram_DB),y              ;5        
    sta     GRP1                    ;3        
    dey                             ;2        
    bne     Lf0b3                   ;2/3      
    sta     WSYNC                   ;3   =  32
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     ram_8B                  ;3        
    sta     COLUBK                  ;3        
    lda     #$09                    ;2        
    sta     ram_95                  ;3        
    lda     (ram_D9),y              ;5        
    sta     GRP1                    ;3        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    lda     (ram_DB),y              ;5        
    sta     GRP1                    ;3        
    ldx     ram_95                  ;3        
    lda     ram_B6,x                ;4        
    sta     ram_D9                  ;3        
    lda     ram_C2,x                ;4        
    sta     ram_DB                  ;3   =  57
Lf0f0
    ldy     #$0f                    ;2        
    lda     #$00                    ;2        
    sta     WSYNC                   ;3   =   7
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     PF1                     ;3        
    sta     PF2                     ;3        
    sta     COLUPF                  ;3        
    lda     (ram_D9),y              ;5        
    sta     GRP1                    ;3        
    lda     ram_CD,x                ;4        
    sta     ram_94                  ;3        
    lda     ram_AB,x                ;4        
    and     #$0f                    ;2        
    sta     ram_F6                  ;3        
    lda     (ram_DB),y              ;5        
    dey                             ;2        
    sta     GRP1                    ;3        
    lda     ram_97,x                ;4        
    and     #$07                    ;2        
    sta     NUSIZ0                  ;3        
    cmp     #$05                    ;2        
    bne     Lf11f                   ;2/3      
    lda     #$c8                    ;2        
    bne     Lf122                   ;2/3 =  63
Lf11f
    lda     #$bd                    ;2        
    nop                             ;2   =   4
Lf122
    sta     ram_D7                  ;3        
    lda     (ram_D9),y              ;5        
    sta     GRP1                    ;3        
    lda     ram_97,x                ;4        
    bmi     Lf14a                   ;2/3      
    ldx     ram_F6                  ;3        
    cpx     #$03                    ;2        
    lda     (ram_DB),y              ;5   =  27
Lf132
    dex                             ;2        
    bpl     Lf132                   ;2/3      
    sta     RESP0                   ;3        
    bcs     Lf13c                   ;2/3      
    jsr     Lf616                   ;6   =  15
Lf13c
    dey                             ;2        
    sta     GRP1                    ;3        
    ldx     ram_95                  ;3        
    lda     ram_AB,x                ;4        
    sta     HMP0                    ;3        
    lda     ram_8C                  ;3        
    jmp     Lf166                   ;3   =  21
    
Lf14a
    nop                             ;2        
    nop                             ;2        
    sta     CXCLR                   ;3        
    ldx     ram_95                  ;3        
    lda     ram_AB,x                ;4        
    sta     HMP0                    ;3        
    lda     ram_F6                  ;3        
    sec                             ;2        
    sbc     #$06                    ;2        
    tax                             ;2        
    lda     (ram_DB),y              ;5        
    dey                             ;2        
    sta     GRP1                    ;3        
    lda     ram_8C                  ;3   =  39
Lf161
    dex                             ;2        
    bpl     Lf161                   ;2/3      
    sta     RESP0                   ;3   =   7
Lf166
    sta     WSYNC                   ;3   =   3
;---------------------------------------
Lf168
    sta     HMOVE                   ;3        
    sta     COLUP0                  ;3        
    lda     (ram_D7),y              ;5        
    sta     GRP0                    ;3        
    lda     ram_93                  ;3        
    ora     CXPPMM                  ;3        
    sta     ram_93                  ;3        
    sta     CXCLR                   ;3        
    lda     (ram_D9),y              ;5        
    sta     GRP1                    ;3        
    cpy     #$06                    ;2        
    lda     ram_92                  ;3        
    ora     CXPPMM                  ;3        
    sta     ram_92                  ;3        
    sta     CXCLR                   ;3        
    lda     (ram_DB),y              ;5        
    sta     GRP1                    ;3        
    bcc     Lf199                   ;2/3      
    dey                             ;2        
    sta.w   HMCLR                   ;4        
    lda     ram_94                  ;3        
    eor     ram_86                  ;3        
    and     ram_87                  ;3        
    jmp     Lf168                   ;3   =  76
    
Lf199
    lda     ram_8C                  ;3        
    dey                             ;2        
    sta     WSYNC                   ;3   =   8
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     COLUP0                  ;3        
    lda     (ram_D7),y              ;5        
    sta     GRP0                    ;3        
    lda     ram_93                  ;3        
    ora     CXPPMM                  ;3        
    sta     ram_93                  ;3        
    sta     CXCLR                   ;3        
    lda     (ram_D9),y              ;5        
    sta     GRP1                    ;3        
    nop                             ;2        
    lda     ram_92                  ;3        
    ora     CXPPMM                  ;3        
    sta     ram_92                  ;3        
    sta     CXCLR                   ;3        
    lda     (ram_DB),y              ;5        
    sta     GRP1                    ;3        
    dey                             ;2        
    sta     WSYNC                   ;3   =  61
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     #$00                    ;2        
    sta     GRP0                    ;3        
    lda     (ram_D9),y              ;5        
    sta     GRP1                    ;3        
    ldx     ram_95                  ;3        
    bit     ram_92                  ;3        
    bpl     Lf1d4                   ;2/3      
    stx     ram_90                  ;3   =  27
Lf1d4
    lda     ram_93                  ;3        
    ora     CXPPMM                  ;3        
    bpl     Lf1dc                   ;2/3      
    stx     ram_91                  ;3   =  11
Lf1dc
    lda     (ram_DB),y              ;5        
    sta     GRP1                    ;3        
    sta     CXCLR                   ;3        
    dey                             ;2        
    lda     ram_95                  ;3        
    beq     Lf252                   ;2/3!     
    ldx     ram_8B                  ;3        
    cmp     #$05                    ;2        
    bne     Lf1ef                   ;2/3      
    ldx     ram_89                  ;3   =  28
Lf1ef
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     #$aa                    ;2        
    sta     PF0                     ;3        
    sta     PF2                     ;3        
    lsr                             ;2        
    sta     PF1                     ;3        
    stx     COLUPF                  ;3        
    lda     (ram_D9),y              ;5        
    sta     GRP1                    ;3        
    dec     ram_95                  ;5        
    lda     (ram_DB),y              ;5        
    sta     GRP1                    ;3        
    dey                             ;2        
    sta     WSYNC                   ;3   =  45
;---------------------------------------
    sta     HMOVE                   ;3        
    cpx     ram_89                  ;3        
    bne     Lf21a                   ;2/3      
    lda     #$00                    ;2        
    sta     REFP0                   ;3        
    lda     ram_8B                  ;3        
    jmp     Lf21c                   ;3   =  19
    
Lf21a
    lda     ram_8A                  ;3   =   3
Lf21c
    sta     COLUPF                  ;3        
    lda     (ram_D9),y              ;5        
    sta     GRP1                    ;3        
    jsr     Lf616                   ;6        
    lda     (ram_DB),y              ;5        
    sta     GRP1                    ;3        
    dey                             ;2        
    sta     WSYNC                   ;3   =  30
;---------------------------------------
    sta     HMOVE                   ;3        
    stx     COLUPF                  ;3        
    lda     (ram_D9),y              ;5        
    sta     GRP1                    ;3        
    ldx     ram_95                  ;3        
    lda     ram_B6,x                ;4        
    sta     ram_D9                  ;3        
    lda     ram_C2,x                ;4        
    sta     ram_F6                  ;3        
    nop                             ;2        
    lda     (ram_DB),y              ;5        
    sta     GRP1                    ;3        
    lda     ram_F6                  ;3        
    sta     ram_DB                  ;3        
    lda     #$00                    ;2        
    sta     ram_92                  ;3        
    sta     ram_93                  ;3        
    sta     PF0                     ;3        
    jmp     Lf0f0                   ;3   =  61
    
Lf252
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     (ram_D9),y              ;5        
    sta     GRP1                    ;3        
    jsr     Lf615                   ;6        
    jsr     Lf616                   ;6        
    lda     (ram_DB),y              ;5        
    sta     GRP1                    ;3        
    dey                             ;2        
    bpl     Lf252                   ;2/3      
    ldy     #$0f                    ;2   =  37
Lf269
    lda     ram_8D                  ;3        
    sta     WSYNC                   ;3   =   6
;---------------------------------------
    sta     HMOVE                   ;3        
    cpy     #$0f                    ;2        
    bne     Lf275                   ;2/3      
    lda     ram_8C                  ;3   =  10
Lf275
    sta     COLUBK                  ;3        
    lda     ram_B5                  ;3        
    sta     ram_D9                  ;3        
    lda     ram_C1                  ;3        
    sta     ram_DB                  ;3        
    lda     (ram_D9),y              ;5        
    sta     GRP1                    ;3        
    lda     (ram_DB),y              ;5        
    dey                             ;2        
    sta     GRP1                    ;3        
    cpy     #$06                    ;2        
    bcs     Lf269                   ;2/3      
    sta     WSYNC                   ;3   =  40
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     ram_8C                  ;3        
    sta     COLUBK                  ;3        
    ldx     #$00                    ;2        
    stx     GRP1                    ;3        
    stx     HMCLR                   ;3        
    inx                             ;2        
    stx     NUSIZ0                  ;3        
    stx     NUSIZ1                  ;3        
    sta     RESP0                   ;3        
    sta     RESP1                   ;3        
    lda     #$10                    ;2        
    sta     HMP1                    ;3        
    lda     ram_88                  ;3        
    sta     COLUP0                  ;3        
    sta     COLUP1                  ;3        
    ldx     #$07                    ;2   =  47
Lf2af
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     Lf6a8,x                 ;4        
    sta     GRP0                    ;3        
    lda     Lf6b0,x                 ;4        
    sta     GRP1                    ;3        
    nop                             ;2        
    lda     Lf6c0,x                 ;4        
    tay                             ;2        
    lda     Lf6b8,x                 ;4        
    sta     GRP0                    ;3        
    sty     GRP1                    ;3        
    sta     HMCLR                   ;3        
    dex                             ;2        
    bpl     Lf2af                   ;2/3      
    IF NTSC = 1
    lda     #$1a                    ;2        
    ELSE
    sta     WSYNC                   ;3   =  45
;---------------------------------------
    stx     VBLANK                  ;3        
    lda     #$39                    ;2        
    ENDIF
    sta     TIM64T                  ;4        
    lda     ram_81                  ;3        
    and     #$01                    ;2        
    tax                             ;2        
    asl                             ;2        
    tay                             ;2        
    lda     ram_E7,x                ;4        
    and     #$f0                    ;2        
    lsr                             ;2        
    bne     Lf2e3                   ;2/3      
    lda     #$50                    ;2   =  71
Lf2e3
    sta.wy  ram_DD,y                ;5        
    lda     ram_E7,x                ;4        
    and     #$0f                    ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    sta.wy  ram_E1,y                ;5        
    ldy     #$00                    ;2        
    jsr     Lf69e                   ;6        
    bpl     Lf317                   ;2/3!     
    lda     ram_EA,x                ;4        
    beq     Lf34c                   ;2/3!     
    and     #$40                    ;2        
    beq     Lf330                   ;2/3!     
    lda     #$04                    ;2        
    sta     AUDC0,x                 ;4        
    dec     ram_EA,x                ;6        
    lda     ram_EA,x                ;4        
    and     #$1f                    ;2        
    cmp     #$10                    ;2        
    bcc     Lf317                   ;2/3      
    pha                             ;3        
    and     #$03                    ;2        
    adc     #$02                    ;2        
    sta     AUDF0,x                 ;4        
    pla                             ;4        
    ldy     #$04                    ;2   =  81
Lf317
    sty     AUDV0,x                 ;4        
    cmp     #$00                    ;2        
    bne     Lf321                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_EA,x                ;4   =  14
Lf321
    lda     SWCHB                   ;4        
    and     Lf7fe,x                 ;4        
    beq     Lf32d                   ;2/3      
    lda     #$06                    ;2         *
    sta     ram_8E,x                ;4   =  16 *
Lf32d
    jmp     Lf42f                   ;3   =   3
    
Lf330
    lda     ram_EA,x                ;4        
    sta     AUDV0,x                 ;4        
    lda     #$0c                    ;2        
    sta     AUDC0,x                 ;4        
    txa                             ;2        
    adc     #$06                    ;2        
    sta     AUDF0,x                 ;4        
    dec     ram_EA,x                ;6        
    lda     ram_EA,x                ;4        
    and     #$0f                    ;2        
    bne     Lf349                   ;2/3      
    IF NTSC = 1
    lda     #$00                    ;2        superfluous
    ENDIF
    sta     ram_EA,x                ;4   =  42 (PAL50  = 40)
Lf349
    jmp     Lf42f                   ;3   =   3
    
Lf34c
    lda     ram_83                  ;3        
    cmp     #$08                    ;2        
    lda     #$02                    ;2        
    bcs     Lf376                   ;2/3      
    lda     ram_E6                  ;3        
    beq     Lf35e                   ;2/3      
    lda     #$00                    ;2        
    sta     AUDV0,x                 ;4        
    beq     Lf349                   ;2/3 =  22
Lf35e
    lda     ram_EA                  ;3        
    ora     ram_EB                  ;3        
    bne     Lf38d                   ;2/3      
    lda     ram_82                  ;3        
    eor     #$40                    ;2        
    cmp     #$e0                    ;2        
    bcc     Lf38d                   ;2/3      
    lda     ram_82                  ;3        
    eor     ram_81                  ;3        
    and     #$3f                    ;2        
    beq     Lf38d                   ;2/3      
    lda     ram_82                  ;3   =  30
Lf376
    and     #$03                    ;2        
    ora     #$04                    ;2        
    sta     AUDF0                   ;3        
    sec                             ;2        
    sbc     #$01                    ;2        
    sta     AUDF1                   ;3        
    lda     #$01                    ;2        
    sta     AUDC0                   ;3        
    sta     AUDC1                   ;3        
    sta     AUDV0                   ;3        
    sta     AUDV1                   ;3        
    bne     Lf349                   ;2/3 =  30
Lf38d
    lda     ram_8E,x                ;4        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    tay                             ;2        
    cpy     #$0a                    ;2        
    bcc     Lf39a                   ;2/3      
    ldy     #$09                    ;2   =  20
Lf39a
    lda     #$00                    ;2        
    cpy     #$05                    ;2        
    bcc     Lf3a2                   ;2/3      
    lda     #$01                    ;2   =   8
Lf3a2
    sta     ram_FB                  ;3        
    lda.wy  ram_97,y                ;4        
    sta     ram_FA                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    and     #$07                    ;2        
    sta     ram_F8                  ;3        
    cmp     #$02                    ;2        
    lda     #$20                    ;2        
    bcc     Lf3bd                   ;2/3      
    lda     #$ff                    ;2        
    sta     ram_FB                  ;3        
    lda     #$10                    ;2   =  36
Lf3bd
    sta     ram_F7                  ;3        
    lda     #$03                    ;2        
    sta     AUDC0,x                 ;4        
    lda.wy  ram_EC,y                ;4        
    sta     ram_F9                  ;3        
    lda     #$7f                    ;2        
    sta     ram_FD                  ;3        
    lda     ram_FB                  ;3        
    sta     ram_FE                  ;3        
    lda     ram_FA                  ;3        
    and     #$07                    ;2        
    asl                             ;2        
    asl                             ;2        
    ora     #$03                    ;2        
    tay                             ;2   =  40
Lf3d9
    lda     ram_FB                  ;3        
    sta     ram_F6                  ;3        
    clc                             ;2        
    lda     Lf7da,y                 ;4        
    adc     ram_F9                  ;3        
    cmp     #$a0                    ;2        
    bcc     Lf3e9                   ;2/3      
    sbc     #$a0                    ;2   =  21
Lf3e9
    sta     ram_FC                  ;3        
    lda     Lf7f6,x                 ;4        
    sec                             ;2        
    sbc     ram_FC                  ;3        
    bcs     Lf3f7                   ;2/3      
    eor     #$ff                    ;2        
    inc     ram_F6                  ;5   =  21
Lf3f7
    cmp     ram_FD                  ;3        
    bcs     Lf401                   ;2/3!     
    sta     ram_FD                  ;3        
    lda     ram_F6                  ;3        
    sta     ram_FE                  ;3   =  14
Lf401
    dey                             ;2        
    tya                             ;2        
    and     #$03                    ;2        
    bne     Lf3d9                   ;2/3!     
    lda     ram_FD                  ;3        
    cmp     ram_F7                  ;3        
    bcc     Lf41c                   ;2/3      
    lda     #$0f                    ;2        
    sta     AUDC0,x                 ;4        
    lda     #$1f                    ;2        
    sta     AUDF0,x                 ;4        
    lda     #$01                    ;2        
    sta     AUDV0,x                 ;4        
    jmp     Lf42f                   ;3   =  37
    
Lf41c
    dec     ram_F7                  ;5        
    eor     ram_F7                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    sta     AUDV0,x                 ;4        
    ldy     ram_FE                  ;3        
    iny                             ;2        
    lda     Lf7f8,y                 ;4        
    clc                             ;2        
    adc     ram_F8                  ;3        
    sta     AUDF0,x                 ;4   =  34
Lf42f
    lda     ram_81                  ;3        
    and     #$1f                    ;2        
    bne     Lf43f                   ;2/3      
    lda     ram_82                  ;3        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    eor     ram_82                  ;3        
    asl                             ;2        
    rol     ram_82                  ;5   =  26
Lf43f
    lda     ram_E6                  ;3        
    bne     Lf451                   ;2/3      
    ldx     #$09                    ;2        
    lda     #$ff                    ;2        
    sta     ram_96                  ;3   =  12
Lf449
    jsr     Lf624                   ;6        
    dex                             ;2        
    cpx     #$05                    ;2        
    bcs     Lf449                   ;2/3 =  12
Lf451
    lda     INTIM                   ;4        
    bne     Lf451                   ;2/3      
    ldy     #$82                    ;2        
    sty     WSYNC                   ;3   =  11
;---------------------------------------
    IF NTSC = 1
    sty     VBLANK                  ;3        
    ENDIF
    sty     VSYNC                   ;3        
    sty     WSYNC                   ;3   =   9 (PAL50 = 6)
;---------------------------------------
    sty     WSYNC                   ;3   =   3
;---------------------------------------
    sty     WSYNC                   ;3   =   3
;---------------------------------------
    sta     VSYNC                   ;3        
    inc     ram_81                  ;5        
    bne     Lf473                   ;2/3      
    inc     ram_E9                  ;5        
    inc     ram_E5                  ;5        
    bne     Lf473                   ;2/3      
    sec                             ;2         *
    ror     ram_E5                  ;5   =  29 *
Lf473
    ldy     #$ff                    ;2        
    lda     SWCHB                   ;4        
    and     #$08                    ;2        
    bne     Lf47e                   ;2/3      
    ldy     #$0f                    ;2   =  12 *
Lf47e
    tya                             ;2        
    ldy     #$00                    ;2        
    bit     ram_E5                  ;3        
    bpl     Lf489                   ;2/3      
    and     #$f7                    ;2         *
    ldy     ram_E5                  ;3   =  14 *
Lf489
    sty     ram_86                  ;3        
    asl     ram_86                  ;5        
    sta     ram_87                  ;3        
    IF NTSC = 1
    lda     #$2c                    ;2        
    ELSE
    lda     #$48                    ;2        
    ENDIF
    sta     WSYNC                   ;3   =  16
;---------------------------------------
    sta     TIM64T                  ;4        
    lda     ram_E6                  ;3        
    bne     Lf4a6                   ;2/3      
    ldx     #$04                    ;2        
    lda     #$01                    ;2        
    sta     ram_96                  ;3   =  16
Lf4a0
    jsr     Lf624                   ;6        
    dex                             ;2        
    bpl     Lf4a0                   ;2/3 =  10
Lf4a6
    lda     SWCHA                   ;4        
    tay                             ;2        
    and     #$0f                    ;2        
    sta     ram_85                  ;3        
    tya                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    sta     ram_84                  ;3        
    iny                             ;2        
    beq     Lf4bc                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_E5                  ;3   =  33
Lf4bc
    lda     ram_82                  ;3        
    bne     Lf4c4                   ;2/3      
    inc     ram_82                  ;5        
    bne     Lf4dc                   ;2/3 =  12
Lf4c4
    jsr     Lf69e                   ;6        
    bmi     Lf4ce                   ;2/3      
    ldx     #$e5                    ;2        
    jmp     Lf004                   ;3   =  13
    
Lf4ce
    ldy     #$00                    ;2        
    bcs     Lf4f5                   ;2/3      
    lda     ram_83                  ;3        
    beq     Lf4da                   ;2/3      
    dec     ram_83                  ;5        
    bpl     Lf4f7                   ;2/3 =  16
Lf4da
    inc     ram_80                  ;5   =   5
Lf4dc
    jsr     Lf5b0                   ;6        
    lda     ram_80                  ;3        
    and     #$07                    ;2        
    sta     ram_80                  ;3        
    sta     ram_E5                  ;3        
    ora     #$a0                    ;2        
    tay                             ;2        
    iny                             ;2        
    sty     ram_E7                  ;3        
    lda     #$aa                    ;2        
    sta     ram_E8                  ;3        
    ldy     #$1e                    ;2        
    sty     ram_E6                  ;3   =  36
Lf4f5
    sty     ram_83                  ;3   =   3
Lf4f7
    lda     ram_E6                  ;3        
    beq     Lf4fe                   ;2/3      
    jmp     Lf00f                   ;3   =   8
    
Lf4fe
    ldx     #$01                    ;2   =   2
Lf500
    lda     ram_EA,x                ;4        
    beq     Lf50a                   ;2/3      
    and     #$10                    ;2        
    bne     Lf528                   ;2/3      
    beq     Lf534                   ;2/3 =  12
Lf50a
    lda     ram_84,x                ;4        
    lsr                             ;2        
    bcs     Lf525                   ;2/3      
    inc     ram_8E,x                ;6        
    ldy     ram_8E,x                ;4        
    cpy     #$b2                    ;2        
    bcc     Lf525                   ;2/3      
    sed                             ;2        
    lda     ram_E7,x                ;4        
    adc     #$00                    ;2        
    sta     ram_E7,x                ;4        
    cld                             ;2        
    lda     #$8f                    ;2        
    sta     ram_EA,x                ;4        
    bne     Lf530                   ;2/3 =  44
Lf525
    lsr                             ;2        
    bcs     Lf534                   ;2/3 =   4
Lf528
    dec     ram_8E,x                ;6        
    lda     ram_8E,x                ;4        
    cmp     #$06                    ;2        
    bcs     Lf534                   ;2/3 =  14
Lf530
    lda     #$06                    ;2        
    sta     ram_8E,x                ;4   =   6
Lf534
    lda     ram_EA,x                ;4        
    and     #$1f                    ;2        
    cmp     #$17                    ;2        
    bcs     Lf544                   ;2/3      
    lda     ram_90,x                ;4        
    bmi     Lf544                   ;2/3      
    lda     #$5c                    ;2        
    sta     ram_EA,x                ;4   =  22
Lf544
    dex                             ;2        
    bpl     Lf500                   ;2/3      
    ldx     #$00                    ;2        
    jsr     Lf671                   ;6        
    sta.wy  ram_B5,y                ;5        
    cpy     #$0b                    ;2        
    beq     Lf559                   ;2/3      
    clc                             ;2        
    adc     #$10                    ;2        
    sta.wy  ram_B6,y                ;5   =  30
Lf559
    inx                             ;2        
    jsr     Lf671                   ;6        
    sta.wy  ram_C1,y                ;5        
    cpy     #$0b                    ;2        
    beq     Lf56a                   ;2/3      
    clc                             ;2        
    adc     #$10                    ;2        
    sta.wy  ram_C2,y                ;5   =  26
Lf56a
    lda     ram_81                  ;3        
    and     #$70                    ;2        
    bne     Lf5ad                   ;2/3      
    lda     ram_80                  ;3        
    and     #$04                    ;2        
    beq     Lf5ad                   ;2/3      
    lda     ram_81                  ;3         *
    and     #$0f                    ;2         *
    tax                             ;2         *
    cpx     #$0a                    ;2         *
    bcs     Lf5ad                   ;2/3       *
    lda     ram_97,x                ;4         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    and     #$07                    ;2         *
    tay                             ;2         *
    lda     ram_81                  ;3         *
    eor     ram_82                  ;3         *
    lsr                             ;2         *
    bcc     Lf594                   ;2/3       *
    dey                             ;2         *
    bpl     Lf594                   ;2/3       *
    ldy     #$00                    ;2   =  57 *
Lf594
    lsr                             ;2         *
    bcc     Lf59e                   ;2/3       *
    iny                             ;2         *
    cpy     #$06                    ;2         *
    bcc     Lf59e                   ;2/3       *
    ldy     #$05                    ;2   =  12 *
Lf59e
    tya                             ;2         *
    asl                             ;2         *
    asl                             ;2         *
    asl                             ;2         *
    asl                             ;2         *
    sta     ram_F6                  ;3         *
    lda     ram_97,x                ;4         *
    and     #$8f                    ;2         *
    ora     ram_F6                  ;3         *
    sta     ram_97,x                ;4   =  26 *
Lf5ad
    jmp     Lf00f                   ;3   =   3
    
Lf5b0
    lda     ram_81                  ;3        
    and     #$01                    ;2        
    sta     ram_81                  ;3        
    ldx     #$01                    ;2   =  10
Lf5b8
    lda     #$06                    ;2        
    sta     ram_8E,x                ;4        
    lda     #$00                    ;2        
    sta     AUDV0,x                 ;4        
    dex                             ;2        
    bpl     Lf5b8                   ;2/3      
    ldx     #$0d                    ;2        
    lda     #$f7                    ;2   =  20
Lf5c7
    sta     ram_D7,x                ;4        
    dex                             ;2        
    dex                             ;2        
    bpl     Lf5c7                   ;2/3      
    ldx     #$09                    ;2   =  12
Lf5cf
    lda     #$01                    ;2        
    sta     ram_A1,x                ;4        
    lda     Lf6f6,x                 ;4        
    sta     ram_CD,x                ;4        
    clc                             ;2        
    lda     ram_80                  ;3        
    and     #$03                    ;2        
    tay                             ;2        
    txa                             ;2        
    adc     Lf7d6,y                 ;4        
    tay                             ;2        
    lda     Lf6c8,y                 ;4        
    sta     ram_97,x                ;4        
    lda     #$60                    ;2        
    sta     ram_AB,x                ;4        
    lda     #$50                    ;2        
    sta     ram_B5,x                ;4        
    sta     ram_B9,x                ;4        
    sta     ram_C3,x                ;4        
    dex                             ;2        
    bpl     Lf5cf                   ;2/3      
    rts                             ;6   =  69
    
Lf5f8
    clc                             ;2        
    adc     #$2e                    ;2        
    tay                             ;2        
    and     #$0f                    ;2        
    sta     ram_F6                  ;3        
    tya                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    tay                             ;2        
    clc                             ;2        
    adc     ram_F6                  ;3        
    cmp     #$0f                    ;2        
    bcc     Lf610                   ;2/3      
    sbc     #$0f                    ;2        
    iny                             ;2   =  36
Lf610
    eor     #$07                    ;2        
    asl                             ;2   =   4
Lf613
    asl                             ;2        
    asl                             ;2   =   4
Lf615
    asl                             ;2   =   2
Lf616
    rts                             ;6   =   6
    
Lf617
    jsr     Lf5f8                   ;6        
    sta     HMP0,x                  ;4        
    sta     WSYNC                   ;3   =  13
;---------------------------------------
Lf61e
    dey                             ;2        
    bpl     Lf61e                   ;2/3      
    sta     RESP0,x                 ;4        
    rts                             ;6   =  14
    
Lf624
    dec     ram_A1,x                ;6        
    bpl     Lf653                   ;2/3      
    lda     ram_97,x                ;4        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    and     #$07                    ;2        
    sec                             ;2        
    sbc     #$01                    ;2        
    bpl     Lf63e                   ;2/3      
    lda     ram_EC,x                ;4        
    clc                             ;2        
    adc     ram_96                  ;3        
    sta     ram_EC,x                ;4        
    lda     #$00                    ;2   =  43
Lf63e
    sta     ram_A1,x                ;4        
    lda     ram_EC,x                ;4        
    clc                             ;2        
    adc     ram_96                  ;3        
    cmp     #$c8                    ;2        
    bcc     Lf64b                   ;2/3      
    lda     #$9f                    ;2   =  19
Lf64b
    cmp     #$a0                    ;2        
    bcc     Lf651                   ;2/3      
    lda     #$00                    ;2   =   6
Lf651
    sta     ram_EC,x                ;4   =   4
Lf653
    lda     ram_EC,x                ;4        
    jsr     Lf5f8                   ;6        
    sta     ram_F6                  ;3        
    dey                             ;2        
    dey                             ;2        
    dey                             ;2        
    asl     ram_97,x                ;6        
    cpy     #$06                    ;2        
    ror     ram_97,x                ;6        
    tya                             ;2        
    ora     ram_F6                  ;3        
    sta     ram_AB,x                ;4        
    lda     #$50                    ;2        
    sta     ram_B5,x                ;4        
    sta     ram_C3,x                ;4        
    sta     ram_B9,x                ;4        
    rts                             ;6   =  62
    
Lf671
    lda     ram_8E,x                ;4        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    tay                             ;2        
    lda     ram_8E,x                ;4        
    and     #$0f                    ;2        
    sta     ram_F6                  ;3        
    lda     ram_EA,x                ;4        
    beq     Lf68f                   ;2/3      
    and     #$40                    ;2        
    beq     Lf68f                   ;2/3      
    lda     ram_8E,x                ;4        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lda     #$a0                    ;2        
    bcc     Lf69a                   ;2/3 =  47
Lf68f
    lda     ram_F6                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lda     #$60                    ;2        
    bcc     Lf69a                   ;2/3      
    lda     #$80                    ;2   =  15
Lf69a
    sec                             ;2        
    sbc     ram_F6                  ;3        
    rts                             ;6   =  11
    
Lf69e
    lda     SWCHB                   ;4        
    lsr                             ;2        
    ror                             ;2        
    rts                             ;6   =  14
    
    .byte   $25,$0c,$25,$0d                 ; $f6a4 (*)
    
Lf6a8
    .byte   %00000000 ; |        |            $f6a8 (G)
    .byte   %10101101 ; |# # ## #|            $f6a9 (G)
    .byte   %10101001 ; |# # #  #|            $f6aa (G)
    .byte   %11101001 ; |### #  #|            $f6ab (G)
    .byte   %10101001 ; |# # #  #|            $f6ac (G)
    .byte   %11101101 ; |### ## #|            $f6ad (G)
    .byte   %01000001 ; | #     #|            $f6ae (G)
    .byte   %00001111 ; |    ####|            $f6af (G)
Lf6b0
    .byte   %00000000 ; |        |            $f6b0 (G)
    .byte   %01010000 ; | # #    |            $f6b1 (G)
    .byte   %01011000 ; | # ##   |            $f6b2 (G)
    .byte   %01011100 ; | # ###  |            $f6b3 (G)
    .byte   %01010110 ; | # # ## |            $f6b4 (G)
    .byte   %01010011 ; | # #  ##|            $f6b5 (G)
    .byte   %00010001 ; |   #   #|            $f6b6 (G)
    .byte   %11110000 ; |####    |            $f6b7 (G)
Lf6b8
    .byte   %00000000 ; |        |            $f6b8 (G)
    .byte   %10111010 ; |# ### # |            $f6b9 (G)
    .byte   %10001010 ; |#   # # |            $f6ba (G)
    .byte   %10111010 ; |# ### # |            $f6bb (G)
    .byte   %10100010 ; |# #   # |            $f6bc (G)
    .byte   %00111010 ; |  ### # |            $f6bd (G)
    .byte   %10000000 ; |#       |            $f6be (G)
    .byte   %11111110 ; |####### |            $f6bf (G)
Lf6c0
    .byte   %00000000 ; |        |            $f6c0 (G)
    .byte   %11101001 ; |### #  #|            $f6c1 (G)
    .byte   %10101011 ; |# # # ##|            $f6c2 (G)
    .byte   %10101111 ; |# # ####|            $f6c3 (G)
    .byte   %10101101 ; |# # ## #|            $f6c4 (G)
    .byte   %11101001 ; |### #  #|            $f6c5 (G)
    .byte   %00000000 ; |        |            $f6c6 (G)
    .byte   %00000000 ; |        |            $f6c7 (G)
    
Lf6c8
    .byte   $50,$40,$30,$20,$10,$10,$20,$30 ; $f6c8 (D)
    .byte   $40,$50,$40,$31,$22,$13,$04,$15 ; $f6d0 (D)
    .byte   $14,$23,$32,$41,$46,$36,$20,$16 ; $f6d8 (D)
    .byte   $05,$00,$16,$20,$36,$46,$05,$15 ; $f6e0 (D)
    .byte   $25,$15,$05,$05,$15,$25,$15,$05 ; $f6e8 (D)
Lf6f0
    .byte   RED|$a,YELLOW|$e,BLACK|$c
    .byte   BLACK|$6,BLACK,BLACK|$8         ; $f6f0 (D)
Lf6f6
    .byte   YELLOW|$a,GREEN|$8,RED|$4,BLUE|$8
    .byte   $24,BLUE|$2,RED|$a,YELLOW|$2 ; $f6f6 (D)
    .byte   GREEN|$c,RED|$2                         ; $f6fe (D)
    
    .byte   %00111100 ; |  ####  |            $f700 (G)
    .byte   %01100110 ; | ##  ## |            $f701 (G)
    .byte   %01100110 ; | ##  ## |            $f702 (G)
    .byte   %01100110 ; | ##  ## |            $f703 (G)
    .byte   %01100110 ; | ##  ## |            $f704 (G)
    .byte   %01100110 ; | ##  ## |            $f705 (G)
    .byte   %01100110 ; | ##  ## |            $f706 (G)
    .byte   %00111100 ; |  ####  |            $f707 (G)
    .byte   %00111100 ; |  ####  |            $f708 (G)
    .byte   %00011000 ; |   ##   |            $f709 (G)
    .byte   %00011000 ; |   ##   |            $f70a (G)
    .byte   %00011000 ; |   ##   |            $f70b (G)
    .byte   %00011000 ; |   ##   |            $f70c (G)
    .byte   %00011000 ; |   ##   |            $f70d (G)
    .byte   %00111000 ; |  ###   |            $f70e (G)
    .byte   %00011000 ; |   ##   |            $f70f (G)
    .byte   %01111110 ; | ###### |            $f710 (G)
    .byte   %01100000 ; | ##     |            $f711 (G)
    .byte   %01100000 ; | ##     |            $f712 (G)
    .byte   %00111100 ; |  ####  |            $f713 (G)
    .byte   %00000110 ; |     ## |            $f714 (G)
    .byte   %00000110 ; |     ## |            $f715 (G)
    .byte   %01000110 ; | #   ## |            $f716 (G)
    .byte   %00111100 ; |  ####  |            $f717 (G)
    .byte   %00111100 ; |  ####  |            $f718 (G)
    .byte   %01000110 ; | #   ## |            $f719 (G)
    .byte   %00000110 ; |     ## |            $f71a (G)
    .byte   %00001100 ; |    ##  |            $f71b (G)
    .byte   %00001100 ; |    ##  |            $f71c (G)
    .byte   %00000110 ; |     ## |            $f71d (G)
    .byte   %01000110 ; | #   ## |            $f71e (G)
    .byte   %00111100 ; |  ####  |            $f71f (G)
    .byte   %00001100 ; |    ##  |            $f720 (G)
    .byte   %00001100 ; |    ##  |            $f721 (G)
    .byte   %00001100 ; |    ##  |            $f722 (G)
    .byte   %01111110 ; | ###### |            $f723 (G)
    .byte   %01001100 ; | #  ##  |            $f724 (G)
    .byte   %00101100 ; |  # ##  |            $f725 (G)
    .byte   %00011100 ; |   ###  |            $f726 (G)
    .byte   %00001100 ; |    ##  |            $f727 (G)
    .byte   %01111100 ; | #####  |            $f728 (G)
    .byte   %01000110 ; | #   ## |            $f729 (G)
    .byte   %00000110 ; |     ## |            $f72a (G)
    .byte   %00000110 ; |     ## |            $f72b (G)
    .byte   %01111100 ; | #####  |            $f72c (G)
    .byte   %01100000 ; | ##     |            $f72d (G)
    .byte   %01100000 ; | ##     |            $f72e (G)
    .byte   %01111110 ; | ###### |            $f72f (G)
    .byte   %00111100 ; |  ####  |            $f730 (G)
    .byte   %01100110 ; | ##  ## |            $f731 (G)
    .byte   %01100110 ; | ##  ## |            $f732 (G)
    .byte   %01100110 ; | ##  ## |            $f733 (G)
    .byte   %01111100 ; | #####  |            $f734 (G)
    .byte   %01100000 ; | ##     |            $f735 (G)
    .byte   %01100010 ; | ##   # |            $f736 (G)
    .byte   %00111100 ; |  ####  |            $f737 (G)
    .byte   %00011000 ; |   ##   |            $f738 (G)
    .byte   %00011000 ; |   ##   |            $f739 (G)
    .byte   %00011000 ; |   ##   |            $f73a (G)
    .byte   %00011000 ; |   ##   |            $f73b (G)
    .byte   %00001100 ; |    ##  |            $f73c (G)
    .byte   %00000110 ; |     ## |            $f73d (G)
    .byte   %01000010 ; | #    # |            $f73e (G)
    .byte   %01111110 ; | ###### |            $f73f (G)
    .byte   %00111100 ; |  ####  |            $f740 (G)
    .byte   %01100110 ; | ##  ## |            $f741 (G)
    .byte   %01100110 ; | ##  ## |            $f742 (G)
    .byte   %00111100 ; |  ####  |            $f743 (G)
    .byte   %00111100 ; |  ####  |            $f744 (G)
    .byte   %01100110 ; | ##  ## |            $f745 (G)
    .byte   %01100110 ; | ##  ## |            $f746 (G)
    .byte   %00111100 ; |  ####  |            $f747 (G)
    
    .byte   $3c,$46,$06,$3e,$66,$66,$66,$3c ; $f748 (*)
    
    .byte   %00000000 ; |        |            $f750 (G)
    .byte   %00000000 ; |        |            $f751 (G)
    .byte   %00000000 ; |        |            $f752 (G)
    .byte   %00000000 ; |        |            $f753 (G)
    .byte   %00000000 ; |        |            $f754 (G)
    .byte   %00000000 ; |        |            $f755 (G)
    .byte   %00000000 ; |        |            $f756 (G)
    .byte   %00000000 ; |        |            $f757 (G)
    .byte   %00000000 ; |        |            $f758 (G)
    .byte   %00000000 ; |        |            $f759 (G)
    .byte   %00000000 ; |        |            $f75a (G)
    .byte   %00000000 ; |        |            $f75b (G)
    .byte   %00000000 ; |        |            $f75c (G)
    .byte   %00000000 ; |        |            $f75d (G)
    .byte   %00000000 ; |        |            $f75e (G)
    .byte   %00000000 ; |        |            $f75f (G)
    .byte   %00110000 ; |  ##    |            $f760 (G)
    .byte   %01100000 ; | ##     |            $f761 (G)
    .byte   %01111000 ; | ####   |            $f762 (G)
    .byte   %11111000 ; |#####   |            $f763 (G)
    .byte   %10111000 ; |# ###   |            $f764 (G)
    .byte   %00001100 ; |    ##  |            $f765 (G)
    .byte   %00000110 ; |     ## |            $f766 (G)
    .byte   %00000100 ; |     #  |            $f767 (G)
    .byte   %00000000 ; |        |            $f768 (G)
    .byte   %00000000 ; |        |            $f769 (G)
    .byte   %00000000 ; |        |            $f76a (G)
    .byte   %00000000 ; |        |            $f76b (G)
    .byte   %00000000 ; |        |            $f76c (G)
    .byte   %00000000 ; |        |            $f76d (G)
    .byte   %00000000 ; |        |            $f76e (G)
    .byte   %00000000 ; |        |            $f76f (G)
    .byte   %00000000 ; |        |            $f770 (G)
    .byte   %00000000 ; |        |            $f771 (G)
    .byte   %00000000 ; |        |            $f772 (G)
    .byte   %00000000 ; |        |            $f773 (G)
    .byte   %00000000 ; |        |            $f774 (G)
    .byte   %00000000 ; |        |            $f775 (G)
    .byte   %00000000 ; |        |            $f776 (G)
    .byte   %00000000 ; |        |            $f777 (G)
    .byte   %00000000 ; |        |            $f778 (G)
    .byte   %00000000 ; |        |            $f779 (G)
    .byte   %00000000 ; |        |            $f77a (G)
    .byte   %00000000 ; |        |            $f77b (G)
    .byte   %00000000 ; |        |            $f77c (G)
    .byte   %00000000 ; |        |            $f77d (G)
    .byte   %00000000 ; |        |            $f77e (G)
    .byte   %00000000 ; |        |            $f77f (G)
    .byte   %00011000 ; |   ##   |            $f780 (G)
    .byte   %00110000 ; |  ##    |            $f781 (G)
    .byte   %01111000 ; | ####   |            $f782 (G)
    .byte   %11111000 ; |#####   |            $f783 (G)
    .byte   %10111000 ; |# ###   |            $f784 (G)
    .byte   %00011000 ; |   ##   |            $f785 (G)
    .byte   %00001100 ; |    ##  |            $f786 (G)
    .byte   %00001000 ; |    #   |            $f787 (G)
    .byte   %00000000 ; |        |            $f788 (G)
    .byte   %00000000 ; |        |            $f789 (G)
    .byte   %00000000 ; |        |            $f78a (G)
    .byte   %00000000 ; |        |            $f78b (G)
    .byte   %00000000 ; |        |            $f78c (G)
    .byte   %00000000 ; |        |            $f78d (G)
    .byte   %00000000 ; |        |            $f78e (G)
    .byte   %00000000 ; |        |            $f78f (G)
    .byte   %00000000 ; |        |            $f790 (G)
    .byte   %00000000 ; |        |            $f791 (G)
    .byte   %00000000 ; |        |            $f792 (G)
    .byte   %00000000 ; |        |            $f793 (G)
    .byte   %00000000 ; |        |            $f794 (G)
    .byte   %00000000 ; |        |            $f795 (G)
    .byte   %00000000 ; |        |            $f796 (G)
    .byte   %00000000 ; |        |            $f797 (G)
    .byte   %00000000 ; |        |            $f798 (G)
    .byte   %00000000 ; |        |            $f799 (G)
    .byte   %00000000 ; |        |            $f79a (G)
    .byte   %00000000 ; |        |            $f79b (G)
    .byte   %00000000 ; |        |            $f79c (G)
    .byte   %00000000 ; |        |            $f79d (G)
    .byte   %00000000 ; |        |            $f79e (G)
    .byte   %00000000 ; |        |            $f79f (G)
    .byte   %01100000 ; | ##     |            $f7a0 (G)
    .byte   %00110000 ; |  ##    |            $f7a1 (G)
    .byte   %01111000 ; | ####   |            $f7a2 (G)
    .byte   %11111000 ; |#####   |            $f7a3 (G)
    .byte   %10111000 ; |# ###   |            $f7a4 (G)
    .byte   %00111100 ; |  ####  |            $f7a5 (G)
    .byte   %00101000 ; |  # #   |            $f7a6 (G)
    .byte   %01000000 ; | #      |            $f7a7 (G)
    .byte   %00000000 ; |        |            $f7a8 (G)
    .byte   %00000000 ; |        |            $f7a9 (G)
    .byte   %00000000 ; |        |            $f7aa (G)
    .byte   %00000000 ; |        |            $f7ab (G)
    .byte   %00000000 ; |        |            $f7ac (G)
    .byte   %00000000 ; |        |            $f7ad (G)
    .byte   %00000000 ; |        |            $f7ae (G)
    .byte   %00000000 ; |        |            $f7af (G)
    .byte   %00000000 ; |        |            $f7b0 (G)
    .byte   %00000000 ; |        |            $f7b1 (G)
    .byte   %00000000 ; |        |            $f7b2 (G)
    .byte   %00000000 ; |        |            $f7b3 (G)
    .byte   %00000000 ; |        |            $f7b4 (G)
    .byte   %00000000 ; |        |            $f7b5 (G)
    .byte   %00000000 ; |        |            $f7b6 (G)
    .byte   %00000000 ; |        |            $f7b7 (G)
    .byte   %00000000 ; |        |            $f7b8 (G)
    .byte   %00000000 ; |        |            $f7b9 (G)
    .byte   %00000000 ; |        |            $f7ba (G)
    .byte   %00000000 ; |        |            $f7bb (G)
    .byte   %00000000 ; |        |            $f7bc (G)
    .byte   %00000000 ; |        |            $f7bd (G)
    .byte   %00000000 ; |        |            $f7be (G)
    .byte   %00000000 ; |        |            $f7bf (G)
    
    .byte   $00                             ; $f7c0 (*)
    
    .byte   %01100110 ; | ##  ## |            $f7c1 (G)
    .byte   %11111110 ; |####### |            $f7c2 (G)
    .byte   %11001111 ; |##  ####|            $f7c3 (G)
    .byte   %10110011 ; |# ##  ##|            $f7c4 (G)
    .byte   %10110011 ; |# ##  ##|            $f7c5 (G)
    .byte   %10110011 ; |# ##  ##|            $f7c6 (G)
    .byte   %10110011 ; |# ##  ##|            $f7c7 (G)
    .byte   %11001111 ; |##  ####|            $f7c8 (G)
    .byte   %11111110 ; |####### |            $f7c9 (G)
    .byte   %01100110 ; | ##  ## |            $f7ca (G)
    
    .byte   $00                             ; $f7cb (*)
    
    .byte   %10000101 ; |#    # #|            $f7cc (G)
    .byte   %11111111 ; |########|            $f7cd (G)
    .byte   %10000101 ; |#    # #|            $f7ce (G)
    .byte   %11111101 ; |###### #|            $f7cf (G)
    .byte   %11111101 ; |###### #|            $f7d0 (G)
    .byte   %11111101 ; |###### #|            $f7d1 (G)
    .byte   %11111101 ; |###### #|            $f7d2 (G)
    .byte   %10000101 ; |#    # #|            $f7d3 (G)
    .byte   %11111111 ; |########|            $f7d4 (G)
    .byte   %10000101 ; |#    # #|            $f7d5 (G)
    
Lf7d6
    .byte   $00,$0a,$14,$1e                 ; $f7d6 (D)
Lf7da
    .byte   $00                             ; $f7da (*)
    .byte   $00,$00,$00                     ; $f7db (D)
    .byte   $00,$00,$10,$10,$00,$00,$00,$20 ; $f7de (*)
    .byte   $00,$00,$10,$20,$00,$00,$00,$40 ; $f7e6 (*)
    .byte   $00                             ; $f7ee (*)
    .byte   $00,$00,$00                     ; $f7ef (D)
    .byte   $00                             ; $f7f2 (*)
    .byte   $00,$20,$40                     ; $f7f3 (D)
Lf7f6
    .byte   $30,$68                         ; $f7f6 (D)
Lf7f8
    .byte   $10,$10,$11,$10,$00,$f0         ; $f7f8 (D)
Lf7fe
    .byte   $40                             ; $f7fe (D)
    .byte   $80                             ; $f7ff (*)
    IF PLUSROM = 1

PlusROM_API
   .byte "a", 0, "h.firmaplus.de", 0

SendPlusROMScore
   bcc SkipSend
   inc ram_E6
   lda ram_80
   sta WriteToBuffer                ; game variation
   lda ram_E7
   sta WriteToBuffer
   lda #HIGHSCORE_ID                ; game id in Highscore DB
   sta WriteSendBuffer
SkipSend
   jmp Lf055
 
    ORG $fffa
    .word (PlusROM_API - $E000)      ; PlusRom API pointer
    .word Start,Start
    ENDIF
