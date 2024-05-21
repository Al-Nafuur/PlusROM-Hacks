; Disassembly of Cosmic Swarm.bin
; Disassembled 05/20/24 19:47:47
; Using Stella 6.7.1
;
; ROM properties name : Cosmic Swarm (1982) (CommaVid)
; ROM properties MD5  : e5f17b3e62a21d0df1ca9aee1aa8c7c5
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

COMPILE_REGION         = NTSC       ; change to compile for different regions

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
BROWN            = $20
RED              = $40
PURPLE           = $70
BEIGE            = $f0

   IF COMPILE_REGION = NTSC
YELLOW           = $10
ORANGE           = $30
MAUVE            = $50
BLUE             = $80
CYAN             = $a0
GREEN            = $c0
   ELSE
YELLOW           = $20
ORANGE           = $40
GREEN            = $50
MAUVE            = $60 
CYAN             = $70
BLUE             = $B0
   ENDIF


;-----------------------------------------------------------
;      TIA and IO constants accessed
;-----------------------------------------------------------

INPT4           = $0c  ; (R)

VSYNC           = $00  ; (W)
VBLANK          = $01  ; (W)
WSYNC           = $02  ; (W)
;RSYNC          = $03  ; (Wi)
NUSIZ0          = $04  ; (W)
NUSIZ1          = $05  ; (W)
COLUP0          = $06  ; (W)
COLUP1          = $07  ; (W)
COLUPF          = $08  ; (W)
COLUBK          = $09  ; (W)
;CTRLPF         = $0a  ; (Wi)
;REFP0          = $0b  ; (Wi)
;REFP1          = $0c  ; (Wi)
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
;ENAM1          = $1e  ; (Wi)
;ENABL          = $1f  ; (Wi)
HMP0            = $20  ; (W)
;HMP1           = $21  ; (Wi)
;HMM0           = $22  ; (Wi)
;HMM1           = $23  ; (Wi)
;HMBL           = $24  ; (Wi)
;VDELP0         = $25  ; (Wi)
;VDELP1         = $26  ; (Wi)
;VDELBL         = $27  ; (Wi)
;RESMP0         = $28  ; (Wi)
;RESMP1         = $29  ; (Wi)
HMOVE           = $2a  ; (W)
;HMCLR          = $2b  ; (Wi)
;CXCLR          = $2c  ; (Wi)
;$2d            = $2d  ; (Wi)
;$2e            = $2e  ; (Wi)
;$2f            = $2f  ; (Wi)
;$30            = $30  ; (Wi)
;$31            = $31  ; (Wi)
;$32            = $32  ; (Wi)
;$33            = $33  ; (Wi)
;$34            = $34  ; (Wi)
;$35            = $35  ; (Wi)
;$36            = $36  ; (Wi)
;$37            = $37  ; (Wi)
;$38            = $38  ; (Wi)
;$39            = $39  ; (Wi)
;$3a            = $3a  ; (Wi)
;$3b            = $3b  ; (Wi)
;$3c            = $3c  ; (Wi)
;$3d            = $3d  ; (Wi)
;$3e            = $3e  ; (Wi)
;$3f            = $3f  ; (Wi)

SWCHA           = $0280
SWCHB           = $0282
INTIM           = $0284
TIM8T           = $0295
TIM64T          = $0296


;-----------------------------------------------------------
;      RIOT RAM (zero-page) labels
;-----------------------------------------------------------
ram_80          = $80
ram_90          = $90
ram_96          = $96
ram_A0          = $a0
ram_AC          = $ac
ram_B0          = $b0
ram_C0          = $c0
ram_C2          = $c2
ram_D0          = $d0
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
score_lo_BCD    = $e9
score_hi_BCD    = $ea
ram_EB          = $eb
ram_ED          = $ed
ram_EE          = $ee
ram_F0          = $f0
ram_F1          = $f1
game_status     = $f2
ram_F3          = $f3
ram_F4          = $f4
ram_F5          = $f5
ram_F6          = $f6
ram_F7          = $f7
ram_F8          = $f8
ram_F9          = $f9
ram_FA          = $fa
ram_FB          = $fb
ram_FD          = $fd


;-----------------------------------------------------------
;      PlusROM hotspots and gameId for HSC backend
;-----------------------------------------------------------
 
   IF PLUSROM

WriteToBuffer           = $1FF0
WriteSendBuffer         = $1FF1
ReceiveBuffer           = $1FF2
ReceiveBufferSize       = $1FF3

HIGHSCORE_ID            = 78         ; Cosmic Swarm game ID in Highscore DB
ROM_BASE                = $f000

   ELSE

ROM_BASE                = $f800

   ENDIF



;***********************************************************
;      Bank 0
;***********************************************************

    SEG     CODE
    ORG     ROM_BASE


   IF PLUSROM = 1

PlusROM_API
   .byte "a", 0, "h.firmaplus.de", 0

Lf927
SendPlusROMScore
    lda SWCHB                        ; difficulty switches
    sta WriteToBuffer                ; game difficulty (0-63)
    lda score_hi_BCD
    sta WriteToBuffer
    lda score_lo_BCD
    sta WriteToBuffer
    lda #HIGHSCORE_ID                ; game id in Highscore DB
    sta WriteSendBuffer
    jmp ReturnFromSendPlusROMScore

L_Termite_4
    .byte   %01000010 ; | #    # |            $ffe8 (G)
    .byte   %00100100 ; |  #  #  |            $ffe9 (G)
    .byte   %10011000 ; |#  ##   |            $ffea (G)
    .byte   %01111111 ; | #######|            $ffeb (G)
    .byte   %00111100 ; |  ####  |            $ffec (G)
    .byte   %00111111 ; |  ######|            $ffed (G)
    .byte   %11111100 ; |######  |            $ffee (G)
    .byte   %00111100 ; |  ####  |            $ffef (G)
    .byte   %11111110 ; |####### |            $fff0 (G)
    .byte   %00011001 ; |   ##  #|            $fff1 (G)
    .byte   %00111100 ; |  ####  |            $fff2 (G)
    .byte   %01011010 ; | # ## # |            $fff3 (G)
    .byte   %00111100 ; |  ####  |            $fff4 (G)
    .byte   %01000010 ; | #    # |            $fff5 (G)
    .byte   %01111110 ; | ###### |            $fff6 (G)
    .byte   %00111100 ; |  ####  |            $fff7 (G)
    .byte   %00111100 ; |  ####  |            $fff8 (G)
    .byte   %00111100 ; |  ####  |            $fff9 (G)
    ORG     $f800

   ENDIF

Lf800
    .byte   $84 ;sty                ;3-7 =  -4
Lf801
    .byte   $fb ;ISB                ;7-2 =   5
Lf802
    nop                             ;2   =   2
Lf803
    sta     GRP1|$100               ;4        
    jmp     Lf84d                   ;3   =   7
    
Lf809
    sty     ram_FA                  ;3        
    nop                             ;2   =   5
Lf80c
    dec     ram_F9                  ;5        
    bmi     Lf879                   ;2/3      
    sta     ENAM0|$100              ;4        
    lda     #$00                    ;2        
    sta     PF0                     ;3        
    sta     GRP0|$100               ;4        
    bpl     Lf826                   ;2/3 =  22
Lf81c
    sta     ENAM0                   ;3        
    lda     #$00                    ;2        
    sta     PF0                     ;3        
    lda     (ram_F3),y              ;5        
    sta     GRP0                    ;3   =  16
Lf826
    lda     ram_80,x                  ;4        
    sta     PF1                     ;3        
    lda     ram_96,x                ;4        
    sta     PF2                     ;3        
    lda     ram_AC,x                ;4        
    sta     PF0                     ;3        
    ldy     ram_C2,x                ;4        
    sty     PF1                     ;3        
    and     #$0f                    ;2        
    sta     PF2                     ;3   =  33
Lf83a
    lda     #$00                    ;2        
    ldy     ram_FB                  ;3        
    iny                             ;2        
    sty     ram_FB                  ;3        
    sta     PF0                     ;3        
    bmi     Lf800                   ;2/3      
    cpy     ram_F8                  ;3        
    bpl     Lf803                   ;2/3      
    lda     (ram_F6),y              ;5        
    sta     GRP1                    ;3   =  28
Lf84d
    lda     ram_F9                  ;3        
    cmp     ram_D8                  ;3        
    php                             ;3        
    lsr                             ;2        
    lsr                             ;2        
    tax                             ;2        
    lda     ram_80,x                  ;4        
    sta     PF1                     ;3        
    lda     ram_96,x                ;4        
    sta     PF2                     ;3        
    lda     ram_AC,x                ;4        
    sta     PF0                     ;3        
    ldy     ram_C2,x                ;4        
    sty     PF1                     ;3        
    and     #$0f                    ;2        
    sta     PF2                     ;3        
    ldy     ram_FA                  ;3        
    pla                             ;4        
    iny                             ;2        
    sty     ram_FA                  ;3        
    bmi     Lf809                   ;2/3      
    cpy     ram_F5                  ;3        
    bpl     Lf80c                   ;2/3      
    dec     ram_F9                  ;5        
    bpl     Lf81c                   ;2/3 =  74
Lf879
    lda     #$00                    ;2        
    sta     PF0                     ;3        
    sta     GRP0                    ;3        
    sta     GRP1                    ;3        
    sta     ENAM0                   ;3        
    sta     PF1                     ;3        
    sta     PF2                     ;3        
    lda     ram_E7                  ;3        
    and     #$07                    ;2        
    tax                             ;2        
    lda     Lfefc,x                 ;4        
    sta     WSYNC                   ;3   =  34
;---------------------------------------
    sta     COLUBK                  ;3        
    lda     #$57                    ;2        
    sec                             ;2        
    sbc     ram_D8                  ;3        
    sta     ram_D8                  ;3        

   IF COMPILE_REGION = NTSC

    lda     #$1f                    ;2        

   ELSE

    lda     #$3c                    ;2        

   ENDIF

    sta     TIM64T                  ;4        
    jmp     Lfbfa                   ;3   =  22
    
Lf8a2
    cpx     #$02                    ;2        
    adc     #$10                    ;2        
    tay                             ;2        
    and     #$0f                    ;2        
    sta     ram_F3                  ;3        
    tya                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    tay                             ;2        
    clc                             ;2        
    adc     ram_F3                  ;3        
    cmp     #$0f                    ;2        
    bcc     Lf8bb                   ;2/3      
    sbc     #$0f                    ;2        
    iny                             ;2   =  36
Lf8bb
    eor     #$07                    ;2        
    asl                             ;2        
    sta     WSYNC                   ;3   =   7
;---------------------------------------
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    sta     HMP0,x                  ;4   =  10
Lf8c5
    dey                             ;2        
    bpl     Lf8c5                   ;2/3      
    sta     RESP0,x                 ;4        
    rts                             ;6   =  14
    
Lf8cb
    and     #$f0                    ;2        
    lsr                             ;2        
    lsr                             ;2        
    sta     ram_FD                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    bcc     Lf8db                   ;2/3 =  15
Lf8d5
    and     #$0f                    ;2        
    sta     ram_FD                  ;3        
    asl                             ;2        
    asl                             ;2   =   9
Lf8db
    adc     ram_FD                  ;3        
    sta     ram_FD                  ;3        
    txa                             ;2        
    adc     ram_FD                  ;3        
    tay                             ;2        
    lda     Lf8f5,y                 ;4        
    rts                             ;6   =  23
    
Lf8e7
    .byte   $80                             ; $f8e7 (D)
Lf8e8
    .byte   $80,$00,$80,$00,$00,$01,$00,$01 ; $f8e8 (D)
    .byte   $01                             ; $f8f0 (D)
    .byte   $03                             ; $f8f1 (*)
    .byte   $01,$03,$03                     ; $f8f2 (D)
Lf8f5
    .byte   $24,$5a,$5a,$5a,$24,$24,$24,$24 ; $f8f5 (D)
    .byte   $24,$24                         ; $f8fd (D)
    .byte   $7e                             ; $f8ff (A)
    .byte   $42,$7e,$18,$7e,$7e,$42,$7e,$42 ; $f900 (D)
    .byte   $7e,$5a,$5a,$7e,$42,$42,$7e,$18 ; $f908 (D)
    .byte   $7e,$42,$7e,$7e,$18,$7e,$5a,$7e ; $f910 (D)
    .byte   $7e,$42,$42,$42,$42,$7e,$5a,$7e ; $f918 (D)
    .byte   $5a,$7e,$7e,$5a,$7e,$42,$7e     ; $f920 (D)

   IF PLUSROM

ReturnFromSendPlusROMScore

   ELSE    

Lf927

   ENDIF

    lda     #$40                    ;2        
    sta     game_status             ;3        
    sta     AUDV0                   ;3        
    sta     AUDV1                   ;3   =  11
Lf92f
    jsr     Lfa61                   ;6        
    lda     ram_E7                  ;3        
    and     #$07                    ;2        
    sta     ram_E7                  ;3   =  14
Lf938
    ldx     #$04                    ;2   =   2
Lf93a
    lda     score_hi_BCD            ;3        
    jsr     Lf8d5                   ;6        
    and     #$0f                    ;2        
    sta     ram_F3,x                ;4        
    lda     score_lo_BCD            ;3        
    jsr     Lf8d5                   ;6        
    and     #$f0                    ;2        
    sta     ram_F8,x                ;4        
    lda     score_hi_BCD            ;3        
    jsr     Lf8cb                   ;6        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    ora     ram_F3,x                ;4        
    sta     ram_F3,x                ;4        
    lda     score_lo_BCD            ;3        
    jsr     Lf8cb                   ;6        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    ora     ram_F8,x                ;4        
    sta     ram_F8,x                ;4        
    dex                             ;2        
    bpl     Lf93a                   ;2/3      
    lda     ram_E7                  ;3        
    and     #$07                    ;2        
    tax                             ;2        
    lda     Lfef4,x                 ;4        
    sta     COLUPF                  ;3        
    lda     Lfef8,x                 ;4        
    sta     COLUP0                  ;3        
    sta     COLUP1                  ;3   = 108
Lf97a
    lda     INTIM                   ;4        
    bne     Lf97a                   ;2/3      
    sta     WSYNC                   ;3   =   9
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     VBLANK                  ;3        
    lda     Lfeec,x                 ;4        
    sta     COLUBK                  ;3        
    tax                             ;2        
    lda     ram_E8                  ;3        
    and     #$70                    ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    tay                             ;2        
    lda     Lf8e7,y                 ;4        
    sta     NUSIZ0                  ;3        
    bpl     Lf99c                   ;2/3      
    stx     COLUP0                  ;3   =  40
Lf99c
    lda     Lf8e8,y                 ;4        
    sta     NUSIZ1                  ;3        
    bpl     Lf9a5                   ;2/3      
    stx     COLUP1                  ;3   =  12
Lf9a5
    ldy     #$00                    ;2   =   2
Lf9a7
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    tya                             ;2        
    lsr                             ;2        
    tax                             ;2        
    lda     ram_F3,x                ;4        
    sta     PF1                     ;3        
    lda     ram_F8,x                ;4        
    sta     PF2                     ;3        
    lda     L_Ship_04,x             ;4        
    sta     GRP0                    ;3        
    sta     GRP1                    ;3        
    ldx     #$00                    ;2        
    iny                             ;2        
    lda     ram_E6                  ;3        
    stx     PF1                     ;3        
    and     #$01                    ;2        
    cpy     #$0a                    ;2        
    stx     PF2                     ;3        
    bmi     Lf9a7                   ;2/3      
    eor     #$01                    ;2        
    sta     ram_F9                  ;3        
    lda     game_status             ;3        
    asl                             ;2        
    lda     ram_E7                  ;3        
    inc     ram_E6                  ;5        
    bne     Lf9d9                   ;2/3      
    adc     #$07                    ;2   =  71
Lf9d9
    sta     ram_E7                  ;3        
    and     #$07                    ;2        
    tax                             ;2        
    stx     ram_F8                  ;3        
    asl                             ;2        
    adc     ram_F9                  ;3        
    tay                             ;2        
    lda     Lfa8b,y                 ;4        
    sta     COLUP0                  ;3        
    lda     Lfa8f,y                 ;4        
    sta     COLUP1                  ;3        
    lda     Lfef0,x                 ;4        
    sta     COLUPF                  ;3        
    ldx     ram_F9                  ;3        
    lda     ram_DE,x                ;4        
    ldx     #$00                    ;2        
    jsr     Lf8a2                   ;6        
    stx     NUSIZ0                  ;3        
    stx     NUSIZ1                  ;3        
    ldx     ram_F9                  ;3        
    lda     ram_E0,x           ;4        
    ldx     #$01                    ;2        
    jsr     Lf8a2                   ;6        
    lda     ram_DD                  ;3        
    inx                             ;2        
    jsr     Lf8a2                   ;6        
    ldx     ram_F9                  ;3        
    lda     ram_D9,x                ;4        
    eor     #$ff                    ;2        
    sta     ram_FA                  ;3        
    lda     ram_DB,x                ;4        
    eor     #$ff                    ;2        
    sta     ram_FB                  ;3        
    ldy     ram_E2,x                ;4        
    lda     Lff00,y                 ;4        
    sta     ram_F3                  ;3        
    lda     Lff01,y                 ;4        
    sta     ram_F4                  ;3        
    lda     Lff02,y                 ;4        
    sta     ram_F5                  ;3        
    ldy     ram_E4,x                ;4        
    ldx     ram_F8                  ;3        
    lda     Lfefc,x                 ;4        
    sta     WSYNC                   ;3   = 145
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     COLUBK                  ;3        
    sta     WSYNC                   ;3   =   9
;---------------------------------------
    asl     ram_F9                  ;5        
    lda     Lfeec,x                 ;4        
    sta     COLUBK                  ;3        
    lda     Lff00,y                 ;4        
    sta     ram_F6                  ;3        
    lda     Lff01,y                 ;4        
    sta     ram_F7                  ;3        
    lda     Lff02,y                 ;4        
    sta     ram_F8                  ;3        
    lda     #$57                    ;2        
    sta     ram_F9                  ;3        
    sec                             ;2        
    sbc     ram_D8                  ;3        
    sta     ram_D8                  ;3        
    ldx     #$15                    ;2        
    jmp     Lf83a                   ;3   =  51
    
Lfa61
    ldx     INTIM                   ;4        
    bne     Lfa61                   ;2/3      
    lda     #$2a                    ;2        
    sta     WSYNC                   ;3   =  11
;---------------------------------------
    sta     VBLANK                  ;3        
    sta     VSYNC                   ;3        
    sta     TIM8T                   ;4        
    lda     #$68                    ;2        
    jsr     Lf8a2                   ;6        
    inx                             ;2        
    lda     #$70                    ;2        
    jsr     Lf8a2                   ;6   =  28
Lfa7c
    lda     INTIM                   ;4        
    bne     Lfa7c                   ;2/3      
    sta     WSYNC                   ;3   =   9
;---------------------------------------
    sta     VSYNC                   ;3        

   IF COMPILE_REGION = NTSC
    lda     #$28                    ;2        
   ELSE
    lda     #$46                    ;2        
   ENDIF

    sta     TIM64T                  ;4        
    rts                             ;6   =  15
    
Lfa8b
    .byte   BLACK|$9                        ; $fa8b (C)
    .byte   MAUVE|$9                        ; $fa8c (C)
    .byte   BLACK|$9                        ; $fa8d (C)
    .byte   MAUVE|$9                        ; $fa8e (C)
Lfa8f
    .byte   BLUE|$9                         ; $fa8f (C)
    .byte   YELLOW|$9                       ; $fa90 (C)
    .byte   BLUE|$9                         ; $fa91 (C)
    .byte   YELLOW|$9                       ; $fa92 (C)
Lfa93
    .byte   BEIGE|$e                        ; $fa93 (C)
    .byte   BEIGE|$e                        ; $fa94 (C)
    .byte   BEIGE|$e                        ; $fa95 (C)
    .byte   BEIGE|$f                        ; $fa96 (C)
    .byte   BLACK|$0                        ; $fa97 (C)
    .byte   BLACK|$1                        ; $fa98 (C)
    .byte   BLACK|$2                        ; $fa99 (C)
    .byte   BLACK|$2                        ; $fa9a (C)
    .byte   BLACK|$2                        ; $fa9b (C)
    .byte   BLACK|$2                        ; $fa9c (C)
    .byte   BLACK|$2                        ; $fa9d (C)
    .byte   BLACK|$1                        ; $fa9e (C)
    
    .byte   $00,$ff                         ; $fa9f (*)
    .byte   $fe,$fe                         ; $faa1 (D)
Lfaa3
    .byte   $00,$01,$02,$02,$02,$02,$02,$01 ; $faa3 (D)
    .byte   $00,$ff,$fe,$fe,$fe,$fe         ; $faab (*)
    .byte   $fe,$ff                         ; $fab1 (D)
Lfab3
    .byte   $80,$40,$20,$10,$08,$04,$02,$01 ; $fab3 (D)
    
Lfabb
    cmp     #$90                    ;2        
    bcc     Lfac2                   ;2/3 =   4
Lfabf
    lda     #$ff                    ;2        
    rts                             ;6   =   8
    
Lfac2
    sbc     #$0f                    ;2        
    bcc     Lfabf                   ;2/3      
    and     #$fc                    ;2        
    tax                             ;2        
    lda     Lfeeb,x                 ;4        
    tay                             ;2        
    and     #$03                    ;2        
    asl                             ;2        
    asl                             ;2        
    tax                             ;2        
    lda     ram_F3                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    cmp     #$16                    ;2        
    bcs     Lfabf                   ;2/3      
    sta     ram_F9                  ;3        
    lda     Lfef2,x                 ;4        
    sbc     ram_F9                  ;3        
    tax                             ;2        
    tya                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    tay                             ;2   =  53
Lfae6
    rts                             ;6   =   6
    
Lfae7
    lda     ram_DD                  ;3        
    sec                             ;2        
    sbc     ram_DF,x                ;4        
    bcc     Lfae6                   ;2/3      
    cmp     #$08                    ;2        
    bcs     Lfae6                   ;2/3      
    sta     ram_F9                  ;3        
    lda     ram_D8                  ;3        
    sec                             ;2        
    sbc     ram_DA,x                ;4        
    bmi     Lfae6                   ;2/3      
    ldy     ram_E3,x                ;4        
    sec                             ;2        
    sbc     Lff02,y                 ;4        
    bcs     Lfae6                   ;2/3!     
    cmp     #$fd                    ;2        
    bmi     Lfb22                   ;2/3      
    lda     ram_F9                  ;3        
    cmp     #$02                    ;2        
    bmi     Lfb22                   ;2/3      
    cmp     #$06                    ;2        
    bpl     Lfb22                   ;2/3      
    tya                             ;2        
    and     #$f8                    ;2        
    cmp     #$48                    ;2        
    bne     Lfb22                   ;2/3      
    lda     #$01                    ;2        
    ora     ram_E7                  ;3        
    sta     ram_E7                  ;3        
    ldy     #$03                    ;2        
    bne     Lfb30                   ;2/3 =  76
Lfb22
    lda     ram_E7                  ;3        
    and     #$f8                    ;2        
    sta     ram_E7                  ;3        
    jsr     Lfbac                   ;6        
    ldy     #$01                    ;2        
    bcs     Lfb30                   ;2/3      
    iny                             ;2   =  20
Lfb30
    lda     #$d0                    ;2        
    sta     ram_D8                  ;3        
    sta     AUDV0                   ;3        
    lda     #$50                    ;2        
    cmp     ram_E3,x                ;4        
    sta     ram_E3,x                ;4        
    tya                             ;2        
    bcs     Lfb44                   ;2/3      
    rts                             ;6   =  28
    
Lfb40
    lda     #$01                    ;2        
    ldy     #$04                    ;2   =   4
Lfb44
    clc                             ;2        
    sed                             ;2        
    adc     score_lo_BCD            ;3        
    sta     score_lo_BCD            ;3        
    bcc     Lfb60                   ;2/3      
    adc     score_hi_BCD            ;3        
    cld                             ;2        
    sta     score_hi_BCD            ;3        
    lda     ram_E8                  ;3        
    and     #$70                    ;2        
    cmp     #$60                    ;2        
    bpl     Lfb60                   ;2/3      
    lda     #$10                    ;2        
    tay                             ;2        
    adc     ram_E8                  ;3        
    sta     ram_E8                  ;3   =  39
Lfb60
    cld                             ;2        
    tya                             ;2        
    and     #$1c                    ;2        
    ora     game_status             ;3        
    sta     game_status             ;3        
    rts                             ;6   =  18
    
Lfb69
    lda     #$3c                    ;2        
    sta     ram_E2                  ;3        
    lda     #$80                    ;2        
    ora     ram_F1                  ;3        
    sta     ram_F1                  ;3        
    rts                             ;6   =  19
    
Lfb74
    ldy     ram_E6                  ;3        
    lda     Lf800,y                 ;4        
    cmp     #$98                    ;2        
    bcc     Lfb7f                   ;2/3      
    and     #$7f                    ;2   =  13
Lfb7f
    sta     ram_DF,x                ;4        
    lda     #$e8                    ;2        
    sta     ram_DA,x                ;4        
    lda     Lf801,y                 ;4        
    eor     ram_E6                  ;3        
    eor     ram_D9                  ;3        
    cmp     #$98                    ;2        
    bcc     Lfb92                   ;2/3      
    and     #$7f                    ;2   =  26
Lfb92
    sta     ram_EE,x                ;4        
    lda     Lf802,y                 ;4        
    eor     ram_DE                  ;3        
    eor     ram_DD                  ;3        
    lsr                             ;2        
    cmp     #$57                    ;2        
    bcc     Lfba2                   ;2/3      
    and     #$3f                    ;2   =  22
Lfba2
    sec                             ;2        
    sbc     #$0e                    ;2        
    sta     ram_EB,x                ;4        
    lda     #$48                    ;2        
    sta     ram_E3,x                ;4        
    rts                             ;6   =  20
    
Lfbac
    ldy     ram_E3,x                ;4        
    tya                             ;2        
    and     #$f8                    ;2        
    cmp     #$48                    ;2        
    stx     ram_F5                  ;3        
    bne     Lfbd7                   ;2/3      
    lda     ram_DA,x                ;4        
    adc     Lff02,y                 ;4        
    sec                             ;2        
    sbc     #$03                    ;2        
    sta     ram_F3                  ;3        
    lda     ram_DF,x                ;4        
    clc                             ;2        
    adc     #$03                    ;2        
    jsr     Lfabb                   ;6        
    bmi     Lfbd7                   ;2/3      
    lda     Lfab3,y                 ;4        
    ora     ram_80,x                  ;4        
    cmp     ram_80,x                  ;4        
    clc                             ;2        
    sta     ram_80,x                  ;4        
    bne     Lfbd8                   ;2/3 =  66
Lfbd7
    sec                             ;2   =   2
Lfbd8
    ldx     ram_F5                  ;3        
    bcs     Lfbe2                   ;2/3      
    lda     ram_E3,x                ;4        
    and     #$f7                    ;2        
    sta     ram_E3,x                ;4   =  15
Lfbe2
    rts                             ;6   =   6
    
Start
    sei                             ;2        
    cld                             ;2        
    lda     #$00                    ;2        
    tax                             ;2   =   8
Lfbe8
    sta     VSYNC,x                 ;4        
    txs                             ;2        
    inx                             ;2        
    bne     Lfbe8                   ;2/3      
    lda     #$38                    ;2        
    sta     ram_E8                  ;3        
    lda     #$48                    ;2        
    sta     ram_E3                  ;3        
    sta     ram_E4                  ;3        
    sta     ram_E5                  ;3   =  26
Lfbfa
    lsr     SWCHB                   ;6        
    lda     #$80                    ;2        
    bit     game_status             ;3        
    bcs     Lfc12                   ;2/3      
    bvs     Start                   ;2/3!     
    sta     game_status             ;3        
    lda     #$96                    ;2        
    sta     score_lo_BCD            ;3        
    lda     #$99                    ;2        
    sta     score_hi_BCD            ;3   =  28
Lfc0f
    jmp     Lf92f                   ;3   =   3
    
Lfc12
    bpl     Lfc0f                   ;2/3      
    lda     game_status             ;3        
    ora     #$40                    ;2        
    sta     game_status             ;3        
    ldx     ram_D8                  ;3        
    bpl     Lfc2e                   ;2/3      
    and     #$1f                    ;2        
    sta     AUDV0                   ;3        
    beq     Lfc2e                   ;2/3      
    dec     game_status             ;5        
    sta     AUDF0                   ;3        
    lda     #$0c                    ;2        
    sta     AUDC0                   ;3        
    sta     AUDV0                   ;3   =  38
Lfc2e
    ldx     #$02                    ;2        
    lda     ram_F1                  ;3        
    bmi     Lfc8b                   ;2/3 =   7
Lfc34
    clc                             ;2        
    lda     ram_DE                  ;3        
    adc     #$04                    ;2        
    sec                             ;2        
    sbc     ram_DF,x                ;4        
    bcc     Lfc72                   ;2/3      
    cmp     #$0a                    ;2        
    bcs     Lfc72                   ;2/3      
    lda     ram_D9                  ;3        
    adc     #$04                    ;2        
    sec                             ;2        
    sbc     ram_DA,x                ;4        
    bmi     Lfc72                   ;2/3      
    sec                             ;2        
    sbc     #$03                    ;2        
    ldy     ram_E3,x                ;4        
    cmp     Lff02,y                 ;4        
    bpl     Lfc72                   ;2/3      
    cpy     #$3c                    ;2        
    bpl     Lfc65                   ;2/3      
    lda     ram_E7                  ;3        
    and     #$3f                    ;2        
    sta     ram_E7                  ;3        
    lda     #$20                    ;2        
    sta     AUDC1                   ;3        
    bpl     Lfc70                   ;2/3 =  65
Lfc65
    jsr     Lfb40                   ;6        
    jsr     Lfb69                   ;6        
    jsr     Lfbac                   ;6        
    lda     #$50                    ;2   =  20
Lfc70
    sta     ram_E3,x                ;4   =   4
Lfc72
    dex                             ;2        
    bpl     Lfc34                   ;2/3      
    lda     ram_E4                  ;3        
    ora     ram_E3                  ;3        
    cmp     #$50                    ;2        
    bpl     Lfc8b                   ;2/3      
    lda     ram_E5                  ;3        
    cmp     #$08                    ;2        
    bpl     Lfc8b                   ;2/3      
    sta     AUDF1                   ;3        
    lda     #$06                    ;2        
    sta     AUDV1                   ;3        
    sta     AUDC1                   ;3   =  32
Lfc8b
    lda     ram_E6                  ;3        
    and     #$03                    ;2        
    tax                             ;2        
    tay                             ;2        
    bne     Lfc99                   ;2/3      
    bit     SWCHB                   ;4        
    bvc     Lfccc                   ;2/3      
    inx                             ;2   =  19 *
Lfc99
    dex                             ;2        
    lda     ram_E3,x                ;4        
    cmp     #$50                    ;2        
    bmi     Lfcbf                   ;2/3      
    dey                             ;2        
    bmi     Lfccc                   ;2/3      
    adc     #$03                    ;2        
    sta     ram_E3,x                ;4        
    inc     ram_DA,x                ;6        
    ldy     #$08                    ;2        
    sty     AUDC1                   ;3        
    lsr                             ;2        
    sbc     #$33                    ;2        
    eor     #$ff                    ;2        
    tay                             ;2        
    lda     Lf800,y                 ;4        
    sta     AUDF1                   ;3        
    iny                             ;2        
    sty     AUDV1                   ;3        
    beq     Lfce9                   ;2/3      
    bne     Lfccc                   ;2/3 =  55
Lfcbf
    ldy     ram_EE,x                ;4        
    lda     ram_EB,x                ;4        
    cpy     #$c0                    ;2        
    bne     Lfcd5                   ;2/3      
    jsr     Lfbac                   ;6        
    bcs     Lfccf                   ;2/3 =  20
Lfccc
    jmp     Lfd6c                   ;3   =   3
    
Lfccf
    ldy     ram_DE                  ;3        
    lda     ram_D9                  ;3        
    sbc     #$0c                    ;2   =   8
Lfcd5
    sty     ram_FA                  ;3        
    sta     ram_FB                  ;3        
    lda     ram_DF,x                ;4        
    cmp     ram_FA                  ;3        
    bne     Lfd52                   ;2/3!     
    lda     ram_DA,x                ;4        
    cmp     ram_FB                  ;3        
    bne     Lfd5a                   ;2/3!     
    cmp     #$e9                    ;2        
    bpl     Lfd1b                   ;2/3!=  28
Lfce9
    cpx     #$02                    ;2        
    bne     Lfd16                   ;2/3!     
    bit     ram_E7                  ;3        
    bpl     Lfcf4                   ;2/3      
    jmp     Lf927                   ;3   =  12 *
    
Lfcf4
    bvc     Lfd16                   ;2/3!     
    lda     ram_E7                  ;3        
    eor     #$c0                    ;2        
    sta     ram_E7                  ;3        
    ldy     #$00                    ;2        
    sty     ram_E5                  ;3        
    lda     ram_E6                  ;3        
    and     #$04                    ;2        
    bne     Lfd08                   ;2/3      
    ldy     #$97                    ;2   =  24
Lfd08
    sty     ram_E1                  ;3        
    sty     ram_F0                   ;3        
    ldy     #$e8                    ;2        
    sty     ram_DC                  ;3        
    ldy     #$60                    ;2        
    sty     ram_ED                  ;3        
    bne     Lfd6c                   ;2/3 =  18
Lfd16
    jsr     Lfb74                   ;6        
    bne     Lfd6c                   ;2/3 =   8
Lfd1b
    ldy     ram_E3,x                ;4        
    cpy     #$3c                    ;2        
    bpl     Lfd25                   ;2/3      
    ldy     #$e7                    ;2        
    bne     Lfd4e                   ;2/3 =  12
Lfd25
    jsr     Lfbac                   ;6        
    bcs     Lfd49                   ;2/3      
    ldy     ram_E6                  ;3        
    bit     SWCHB                   ;4        
    bpl     Lfd3c                   ;2/3      
    lda     Lf800,y                 ;4         *
    bmi     Lfd49                   ;2/3       *
    and     #$1f                    ;2         *
    cmp     score_hi_BCD            ;3         *
    bcc     Lfd49                   ;2/3 =  30 *
Lfd3c
    lda     Lf801,y                 ;4        
    cmp     #$98                    ;2        
    bcc     Lfd45                   ;2/3      
    and     #$7f                    ;2   =  10
Lfd45
    ldy     #$e7                    ;2        
    bmi     Lfd4c                   ;2/3 =   4
Lfd49
    lda     #$c0                    ;2        
    tay                             ;2   =   4
Lfd4c
    sta     ram_EE,x                ;4   =   4
Lfd4e
    sty     ram_EB,x                ;4        
    bne     Lfd6c                   ;2/3 =   6
Lfd52
    bcc     Lfd58                   ;2/3      
    dec     ram_DF,x                ;6        
    bcs     Lfd5a                   ;2/3 =  10
Lfd58
    inc     ram_DF,x                ;6   =   6
Lfd5a
    lda     ram_DA,x                ;4        
    cmp     ram_FB                  ;3        
    bmi     Lfd64                   ;2/3      
    dec     ram_DA,x                ;6        
    dec     ram_DA,x                ;6   =  21
Lfd64
    inc     ram_DA,x                ;6        
    lda     ram_E3,x                ;4        
    eor     #$04                    ;2        
    sta     ram_E3,x                ;4   =  16
Lfd6c
    lda     ram_E6                  ;3        
    ldx     ram_F1                  ;3        
    bpl     Lfda4                   ;2/3      
    and     #$03                    ;2        
    bne     Lfda2                   ;2/3      
    lda     ram_E2                  ;3        
    sec                             ;2        
    sbc     #$04                    ;2        
    sta     ram_E2                  ;3        
    bne     Lfd99                   ;2/3      
    sta     AUDV1                   ;3        
    sta     ram_DE                  ;3        
    sta     ram_F1                  ;3        
    lda     ram_E8                  ;3        
    bit     Lfeee                   ;4        
    bne     Lfd8f                   ;2/3      
    jmp     Lf927                   ;3   =  45
    
Lfd8f
    sbc     #$10                    ;2        
    sta     ram_E8                  ;3        
    lda     #$52                    ;2        
    sta     ram_D9                  ;3        
    bne     Lfda2                   ;2/3 =  12
Lfd99
    lsr                             ;2        
    sta     AUDF1                   ;3        
    lda     #$0f                    ;2        
    sta     AUDC1                   ;3        
    sta     AUDV1                   ;3   =  13
Lfda2
    bne     Lfdf7                   ;2/3 =   2
Lfda4
    lsr                             ;2        
    bcs     Lfdf7                   ;2/3      
    ldy     SWCHA                   ;4        
    ldx     INPT4|$30               ;3        
    bmi     Lfdc6                   ;2/3      
    lsr                             ;2        
    bcs     Lfddd                   ;2/3      
    lda     ram_E2                  ;3        
    bit     SWCHA                   ;4        
    bmi     Lfdbc                   ;2/3      
    adc     #$04                    ;2        
    bcc     Lfdc0                   ;2/3 =  30
Lfdbc
    bvs     Lfddd                   ;2/3      
    sbc     #$03                    ;2   =   4
Lfdc0
    and     #$3c                    ;2        
    sta     ram_E2                  ;3        
    bpl     Lfddd                   ;2/3 =   7
Lfdc6
    ldx     ram_DE                  ;3        
    tya                             ;2        
    bmi     Lfdd3                   ;2/3      
    cpx     #$97                    ;2        
    bcs     Lfddd                   ;2/3      
    inc     ram_DE                  ;5        
    bne     Lfddd                   ;2/3 =  18
Lfdd3
    and     #$40                    ;2        
    bne     Lfddd                   ;2/3      
    cpx     #$01                    ;2        
    bcc     Lfddd                   ;2/3      
    dec     ram_DE                  ;5   =  13
Lfddd
    tya                             ;2        
    ldx     ram_D9                  ;3        
    and     #$20                    ;2        
    bne     Lfdec                   ;2/3      
    cpx     #$52                    ;2        
    bcs     Lfdf7                   ;2/3      
    inc     ram_D9                  ;5        
    bne     Lfdf7                   ;2/3 =  20
Lfdec
    tya                             ;2        
    and     #$10                    ;2        
    bne     Lfdf7                   ;2/3      
    cpx     #$01                    ;2        
    bcc     Lfdf7                   ;2/3      
    dec     ram_D9                  ;5   =  15
Lfdf7
    lda     ram_D9                  ;3        
    clc                             ;2        
    adc     #$04                    ;2        
    sta     ram_F3                  ;3        
    lda     ram_DE                  ;3        
    adc     #$05                    ;2        
    sta     ram_FA                  ;3        
    jsr     Lfabb                   ;6        
    bmi     Lfe10                   ;2/3      
    lda     Lfab3,y                 ;4        
    and     ram_80,x                  ;4        
    bne     Lfe42                   ;2/3 =  36
Lfe10
    ldx     ram_DE                  ;3        
    inx                             ;2        
    txa                             ;2        
    sta     ram_FB                  ;3        
    jsr     Lfabb                   ;6        
    bmi     Lfe22                   ;2/3      
    lda     Lfab3,y                 ;4        
    and     ram_80,x                  ;4        
    bne     Lfe42                   ;2/3 =  28
Lfe22
    lda     ram_D9                  ;3        
    sta     ram_F3                  ;3        
    lda     ram_FA                  ;3        
    jsr     Lfabb                   ;6        
    bmi     Lfe34                   ;2/3      
    lda     Lfab3,y                 ;4        
    and     ram_80,x                  ;4        
    bne     Lfe42                   ;2/3 =  27
Lfe34
    lda     ram_FB                  ;3        
    jsr     Lfabb                   ;6        
    bmi     Lfe4e                   ;2/3      
    lda     Lfab3,y                 ;4        
    and     ram_80,x                  ;4        
    beq     Lfe4e                   ;2/3 =  21
Lfe42
    eor     #$ff                    ;2        
    and     ram_80,x                  ;4        
    sta     ram_80,x                  ;4        
    jsr     Lfb40                   ;6        
    jsr     Lfb69                   ;6   =  22
Lfe4e
    jsr     Lfa61                   ;6        
    lda     ram_F1                  ;3        
    bmi     Lfe89                   ;2/3      
    ldx     INPT4|$30               ;3        
    bmi     Lfe61                   ;2/3      
    lda     #$80                    ;2        
    ora     ram_E8                  ;3        
    sta     ram_E8                  ;3        
    bmi     Lfe89                   ;2/3 =  26
Lfe61
    lda     ram_E8                  ;3        
    bpl     Lfe89                   ;2/3      
    and     #$70                    ;2        
    sta     ram_E8                  ;3        
    lda     ram_E2                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    ora     ram_E8                  ;3        
    sta     ram_E8                  ;3        
    lda     ram_D9                  ;3        
    adc     #$02                    ;2        
    sta     ram_D8                  ;3        
    lda     ram_DE                  ;3        
    adc     #$03                    ;2        
    sta     ram_DD                  ;3        
    lda     #$0f                    ;2        
    sta     AUDC0                   ;3        
    lda     #$08                    ;2        
    sta     AUDV0                   ;3        
    sta     ram_F1                  ;3        
    bne     Lfee8                   ;2/3 =  54
Lfe89
    lda     ram_D8                  ;3        
    bmi     Lfee8                   ;2/3      
    sta     ram_F3                  ;3        
    lda     ram_F1                  ;3        
    and     #$7f                    ;2        
    inc     ram_F1                  ;5        
    lsr                             ;2        
    lsr                             ;2        
    cmp     #$1f                    ;2        
    bcc     Lfe9d                   ;2/3      
    lda     #$1f                    ;2   =  28 *
Lfe9d
    sta     AUDF0                   ;3        
    lda     ram_DD                  ;3        
    jsr     Lfabb                   ;6        
    bmi     Lfebf                   ;2/3      
    lda     ram_80,x                  ;4        
    and     Lfab3,y                 ;4        
    beq     Lfebf                   ;2/3      
    eor     #$ff                    ;2        
    and     ram_80,x                  ;4        
    tay                             ;2        
    lda     ram_E7                  ;3        
    lsr                             ;2        
    bcc     Lfebc                   ;2/3      
    sty     ram_80,x                  ;4        
    jsr     Lfb40                   ;6   =  49
Lfebc
    sec                             ;2        
    bcs     Lfed6                   ;2/3 =   4
Lfebf
    ldx     #$02                    ;2   =   2
Lfec1
    jsr     Lfae7                   ;6        
    dex                             ;2        
    bpl     Lfec1                   ;2/3      
    lda     ram_E8                  ;3        
    and     #$0f                    ;2        
    tax                             ;2        
    lda     Lfa93,x                 ;4        
    clc                             ;2        
    adc     ram_D8                  ;3        
    cmp     #$57                    ;2        
    bcc     Lfed8                   ;2/3 =  30
Lfed6
    lda     #$d0                    ;2   =   2
Lfed8
    sta     ram_D8                  ;3        
    bcs     Lfee8                   ;2/3      
    lda     Lfaa3,x                 ;4        
    clc                             ;2        
    adc     ram_DD                  ;3        
    cmp     #$a0                    ;2        
    bcs     Lfed6                   ;2/3      
    sta     ram_DD                  ;3   =  21
Lfee8
    jmp     Lf938                   ;3   =   3
    
Lfeeb
    .byte   $00                             ; $feeb (D)
    
Lfeec
    .byte   BLACK|$0                        ; $feec (C)
    .byte   BLACK|$0                        ; $feed (CB)
Lfeee
    .byte   PURPLE|$0                       ; $feee (CB)
    .byte   BLACK|$4                        ; $feef (CB)
Lfef0
   IF COMPILE_REGION = NTSC
    .byte   GREEN|$5                        ; $fef0 (C)
    .byte   ORANGE|$3                       ; $fef1 (C)
   ELSE
    .byte   GREEN|$7                        ; $fef0 (C)
    .byte   ORANGE|$5                       ; $fef1 (C)
   ENDIF
Lfef2
    .byte   $16     ;(YELLOW|$6)            ; $fef2 (C)
    .byte   BLACK|$8                        ; $fef3 (C)
Lfef4
   IF COMPILE_REGION = NTSC
    .byte   YELLOW|$5                       ; $fef4 (CP)
    .byte   YELLOW|$5                       ; $fef5 (CP)
   ELSE
    .byte   YELLOW|$7                       ; $fef4 (CP)
    .byte   YELLOW|$7                       ; $fef5 (CP)
   ENDIF
    .byte   BROWN|$c                        ; $fef6 (CP)
    .byte   BLACK|$c                        ; $fef7 (CP)
Lfef8
    .byte   BLACK|$9                        ; $fef8 (C)
    .byte   BLACK|$9                        ; $fef9 (C)
    .byte   RED|$2                          ; $fefa (C)
    .byte   $10     ;(YELLOW|$0)            ; $fefb (C)
Lfefc
    .byte   CYAN|$5                         ; $fefc (C)
    .byte   CYAN|$5                         ; $fefd (C)
    .byte   $58     ;(MAUVE|$8)             ; $fefe (C)
    .byte   $14     ;(YELLOW|$4)            ; $feff (C)

; really colors here?
Lff00
    .byte   <L_Ship_01
Lff01
    .byte   >L_Ship_01
Lff02
    .byte   $05,$18
    .word   L_Ship_02
    .byte   $05,$1c
    .word   L_Ship_03
    .byte   $05,$1d
    .word   L_Ship_04
    .byte   $05,$19
    .word   L_Ship_05
    .byte   $05,$15
    .word   L_Ship_06
    .byte   $05,$11
    .word   L_Ship_07
    .byte   $05,$0d
    .word   L_Ship_08
    .byte   $05,$09
    .word   L_Ship_09
    .byte   $05,$05
    .word   L_Ship_10
    .byte   $05,$01
    .word   L_Ship_11
    .byte   $05,$0e
    .word   L_Ship_12
    .byte   $05,$0a
    .word   L_Ship_13
    .byte   $05,$06
    .word   L_Ship_14
    .byte   $05,$02
    .word   L_Ship_15
    .byte   $05,$03
    .word   L_Ship_16
    .byte   $05,$07
    .word   L_Termite_1
    .byte   $0f,$0b
    .word   L_Termite_2
    .byte   $0f,$0f
    .word   L_Termite_3
    .byte   $12,$13
    .word   L_Termite_4

    .byte   $12,$17,$00,$f8,$10,$1b
    .byte   $3a,$f8,$0e,$1f,$69,$f8,$0e,$1e ; $ff54 (D)
    .byte   $1f,$f8,$0c,$1a,$14,$f8,$0a,$16 ; $ff5c (D)
    .byte   $3b,$f8,$07,$12                 ; $ff64 (D)

L_Ship_01
    .byte   %00010000 ; |   #    |            $ff68 (G)
    .byte   %00010000 ; |   #    |            $ff69 (G)
    .byte   %00111000 ; |  ###   |            $ff6a (G)
    .byte   %00111000 ; |  ###   |            $ff6b (G)
    .byte   %01111100 ; | #####  |            $ff6c (G)
L_Ship_02
    .byte   %00001000 ; |    #   |            $ff6d (G)
    .byte   %00011000 ; |   ##   |            $ff6e (G)
    .byte   %00111000 ; |  ###   |            $ff6f (G)
    .byte   %01111000 ; | ####   |            $ff70 (G)
    .byte   %00011000 ; |   ##   |            $ff71 (G)
L_Ship_03
    .byte   %00000100 ; |     #  |            $ff72 (G)
    .byte   %00011000 ; |   ##   |            $ff73 (G)
    .byte   %01111000 ; | ####   |            $ff74 (G)
    .byte   %00110000 ; |  ##    |            $ff75 (G)
    .byte   %00010000 ; |   #    |            $ff76 (G)
L_Ship_04
    .byte   %00000010 ; |      # |            $ff77 (G)
    .byte   %01111100 ; | #####  |            $ff78 (G)
    .byte   %00111000 ; |  ###   |            $ff79 (G)
    .byte   %00110000 ; |  ##    |            $ff7a (G)
    .byte   %00000000 ; |        |            $ff7b (G)
L_Ship_05
    .byte   %01000000 ; | #      |            $ff7c (G)
    .byte   %01110000 ; | ###    |            $ff7d (G)
    .byte   %01111110 ; | ###### |            $ff7e (G)
    .byte   %01110000 ; | ###    |            $ff7f (G)
    .byte   %01000000 ; | #      |            $ff80 (G)
L_Ship_06
    .byte   %00000000 ; |        |            $ff81 (G)
    .byte   %00110000 ; |  ##    |            $ff82 (G)
    .byte   %00111000 ; |  ###   |            $ff83 (G)
    .byte   %01111100 ; | #####  |            $ff84 (G)
    .byte   %00000010 ; |      # |            $ff85 (G)
L_Ship_07
    .byte   %00010000 ; |   #    |            $ff86 (G)
    .byte   %00110000 ; |  ##    |            $ff87 (G)
    .byte   %01111000 ; | ####   |            $ff88 (G)
    .byte   %00011000 ; |   ##   |            $ff89 (G)
    .byte   %00000100 ; |     #  |            $ff8a (G)
L_Ship_08
    .byte   %00011000 ; |   ##   |            $ff8b (G)
    .byte   %01111000 ; | ####   |            $ff8c (G)
    .byte   %00111000 ; |  ###   |            $ff8d (G)
    .byte   %00011000 ; |   ##   |            $ff8e (G)
    .byte   %00001000 ; |    #   |            $ff8f (G)
L_Ship_09
    .byte   %01111100 ; | #####  |            $ff90 (G)
    .byte   %00111000 ; |  ###   |            $ff91 (G)
    .byte   %00111000 ; |  ###   |            $ff92 (G)
    .byte   %00010000 ; |   #    |            $ff93 (G)
    .byte   %00010000 ; |   #    |            $ff94 (G)
L_Ship_10
    .byte   %00110000 ; |  ##    |            $ff95 (G)
    .byte   %00111100 ; |  ####  |            $ff96 (G)
    .byte   %00111000 ; |  ###   |            $ff97 (G)
    .byte   %00110000 ; |  ##    |            $ff98 (G)
    .byte   %00100000 ; |  #     |            $ff99 (G)
L_Ship_11
    .byte   %00010000 ; |   #    |            $ff9a (G)
    .byte   %00011000 ; |   ##   |            $ff9b (G)
    .byte   %00111100 ; |  ####  |            $ff9c (G)
    .byte   %00110000 ; |  ##    |            $ff9d (G)
    .byte   %01000000 ; | #      |            $ff9e (G)
L_Ship_12
    .byte   %00000000 ; |        |            $ff9f (G)
    .byte   %00011000 ; |   ##   |            $ffa0 (G)
    .byte   %00111000 ; |  ###   |            $ffa1 (G)
    .byte   %01111100 ; | #####  |            $ffa2 (G)
    .byte   %10000000 ; |#       |            $ffa3 (G)
L_Ship_13
    .byte   %00000100 ; |     #  |            $ffa4 (G)
    .byte   %00011100 ; |   ###  |            $ffa5 (G)
    .byte   %11111100 ; |######  |            $ffa6 (G)
    .byte   %00011100 ; |   ###  |            $ffa7 (G)
    .byte   %00000100 ; |     #  |            $ffa8 (G)
L_Ship_14
    .byte   %10000000 ; |#       |            $ffa9 (G)
    .byte   %01111100 ; | #####  |            $ffaa (G)
    .byte   %00111000 ; |  ###   |            $ffab (G)
    .byte   %00011000 ; |   ##   |            $ffac (G)
    .byte   %00000000 ; |        |            $ffad (G)
L_Ship_15
    .byte   %01000000 ; | #      |            $ffae (G)
    .byte   %00110000 ; |  ##    |            $ffaf (G)
    .byte   %00111100 ; |  ####  |            $ffb0 (G)
    .byte   %00011000 ; |   ##   |            $ffb1 (G)
    .byte   %00010000 ; |   #    |            $ffb2 (G)
L_Ship_16
    .byte   %00100000 ; |  #     |            $ffb3 (G)
    .byte   %00110000 ; |  ##    |            $ffb4 (G)
    .byte   %00111000 ; |  ###   |            $ffb5 (G)
    .byte   %00111100 ; |  ####  |            $ffb6 (G)
    .byte   %00110000 ; |  ##    |            $ffb7 (G)
L_Termite_1
    .byte   %01000010 ; | #    # |            $ffb8 (G)
    .byte   %00100100 ; |  #  #  |            $ffb9 (G)
    .byte   %00011001 ; |   ##  #|            $ffba (G)
    .byte   %11111110 ; |####### |            $ffbb (G)
    .byte   %00111100 ; |  ####  |            $ffbc (G)
    .byte   %11111100 ; |######  |            $ffbd (G)
    .byte   %00111111 ; |  ######|            $ffbe (G)
    .byte   %00111100 ; |  ####  |            $ffbf (G)
    .byte   %01111111 ; | #######|            $ffc0 (G)
    .byte   %10011000 ; |#  ##   |            $ffc1 (G)
    .byte   %00111100 ; |  ####  |            $ffc2 (G)
    .byte   %01011010 ; | # ## # |            $ffc3 (G)
    .byte   %00111100 ; |  ####  |            $ffc4 (G)
    .byte   %01000010 ; | #    # |            $ffc5 (G)
    .byte   %01000010 ; | #    # |            $ffc6 (G)
L_Termite_2
    .byte   %01000010 ; | #    # |            $ffc7 (G)
    .byte   %00100100 ; |  #  #  |            $ffc8 (G)
    .byte   %10011000 ; |#  ##   |            $ffc9 (G)
    .byte   %01111111 ; | #######|            $ffca (G)
    .byte   %00111100 ; |  ####  |            $ffcb (G)
    .byte   %00111111 ; |  ######|            $ffcc (G)
    .byte   %11111100 ; |######  |            $ffcd (G)
    .byte   %00111100 ; |  ####  |            $ffce (G)
    .byte   %11111110 ; |####### |            $ffcf (G)
    .byte   %00011001 ; |   ##  #|            $ffd0 (G)
    .byte   %00111100 ; |  ####  |            $ffd1 (G)
    .byte   %01011010 ; | # ## # |            $ffd2 (G)
    .byte   %00111100 ; |  ####  |            $ffd3 (G)
    .byte   %01000010 ; | #    # |            $ffd4 (G)
    .byte   %01000010 ; | #    # |            $ffd5 (G)
L_Termite_3
    .byte   %01000010 ; | #    # |            $ffd6 (G)
    .byte   %00100100 ; |  #  #  |            $ffd7 (G)
    .byte   %00011001 ; |   ##  #|            $ffd8 (G)
    .byte   %11111110 ; |####### |            $ffd9 (G)
    .byte   %00111100 ; |  ####  |            $ffda (G)
    .byte   %11111100 ; |######  |            $ffdb (G)
    .byte   %00111111 ; |  ######|            $ffdc (G)
    .byte   %00111100 ; |  ####  |            $ffdd (G)
    .byte   %01111111 ; | #######|            $ffde (G)
    .byte   %10011000 ; |#  ##   |            $ffdf (G)
    .byte   %00111100 ; |  ####  |            $ffe0 (G)
    .byte   %01011010 ; | # ## # |            $ffe1 (G)
    .byte   %00111100 ; |  ####  |            $ffe2 (G)
    .byte   %01000010 ; | #    # |            $ffe3 (G)
    .byte   %01111110 ; | ###### |            $ffe4 (G)
    .byte   %00111100 ; |  ####  |            $ffe5 (G)
    .byte   %00111100 ; |  ####  |            $ffe6 (G)
    .byte   %00111100 ; |  ####  |            $ffe7 (G)
    

   IF PLUSROM = 1

    .org ROM_BASE + 4096 - 6, 0      ; 4K ROM
    .word (PlusROM_API - $E000)      ; PlusRom API pointer

   ELSE
L_Termite_4
    .byte   %01000010 ; | #    # |            $ffe8 (G)
    .byte   %00100100 ; |  #  #  |            $ffe9 (G)
    .byte   %10011000 ; |#  ##   |            $ffea (G)
    .byte   %01111111 ; | #######|            $ffeb (G)
    .byte   %00111100 ; |  ####  |            $ffec (G)
    .byte   %00111111 ; |  ######|            $ffed (G)
    .byte   %11111100 ; |######  |            $ffee (G)
    .byte   %00111100 ; |  ####  |            $ffef (G)
    .byte   %11111110 ; |####### |            $fff0 (G)
    .byte   %00011001 ; |   ##  #|            $fff1 (G)
    .byte   %00111100 ; |  ####  |            $fff2 (G)
    .byte   %01011010 ; | # ## # |            $fff3 (G)
    .byte   %00111100 ; |  ####  |            $fff4 (G)
    .byte   %01000010 ; | #    # |            $fff5 (G)
    .byte   %01111110 ; | ###### |            $fff6 (G)
    .byte   %00111100 ; |  ####  |            $fff7 (G)
    .byte   %00111100 ; |  ####  |            $fff8 (G)
    .byte   %00111100 ; |  ####  |            $fff9 (G)

    .org ROM_BASE + 2048 - 6, 0      ; 2K ROM
    .word Start                      ; NMI vector

   ENDIF
    .word Start                      ; RESET vector
    .word Start                      ; BRK vector