ENDGAME = 0 ;set to 1 if you want game to end at 999,999 points

; Disassembly of Megamana.bin
; Disassembled Tue May 01 23:40:50 2007
; Using DiStella v3.0
; Command Line: C:\BIN\D3.EXE -pafscMegamana.cfg Megamana.bin 
; Megamana.cfg contents:
;      ORG F000
;      CODE F000 FC15
;      GFX FC16 FF26
;      CODE FF27 FFC4
;      GFX FFC5 FFCD
;      CODE FFCE FFFB
;      GFX FFFC FFFF

      processor 6502

VSYNC   =  $00
VBLANK  =  $01
WSYNC   =  $02
RSYNC   =  $03
NUSIZ0  =  $04
NUSIZ1  =  $05
COLUP0  =  $06
COLUP1  =  $07
COLUPF  =  $08
COLUBK  =  $09
CTRLPF  =  $0A
REFP1   =  $0C
PF0     =  $0D
PF1     =  $0E
PF2     =  $0F
RESP0   =  $10
RESP1   =  $11
RESBL   =  $14
AUDC0   =  $15
AUDC1   =  $16
AUDF0   =  $17
AUDF1   =  $18
AUDV0   =  $19
AUDV1   =  $1A
GRP0    =  $1B
GRP1    =  $1C
ENAM0   =  $1D
ENAM1   =  $1E
ENABL   =  $1F
HMP0    =  $20
HMP1    =  $21
HMBL    =  $24
VDELP0  =  $25
VDELP1  =  $26
HMOVE   =  $2A
HMCLR   =  $2B
CXCLR   =  $2C
SWCHA   =  $0280
SWCHB   =  $0282
INTIM   =  $0284
TIM64T  =  $0296

       ORG $F000

START:
       sei                            ;2
       cld                            ;2
       ldx    #$00                    ;2
LF004:
       lda    #$00                    ;2
LF006:
       sta    VSYNC,X                 ;4
       txs                            ;2
       inx                            ;2
       bne    LF006                   ;2
       jsr    LFF27                   ;6
       lda    $82                     ;3
       bne    LF01B                   ;2
       ldx    #$01                    ;2
       stx    $82                     ;3
       dex                            ;2
       jmp    LF786                   ;3

LF01B:
       ldx    #$04                    ;2
LF01D:
       lda    LFCEF,X                 ;4
       eor    $86                     ;3
       and    $87                     ;3
       sta    $88,X                   ;4
       dex                            ;2
       bpl    LF01D                   ;2
       inx                            ;2
LF02A:
       stx    CTRLPF                  ;3
       lda    $88                     ;3
       ldx    $DA                     ;3
LF030:
       bne    LF041                   ;2
       ldx    $D7                     ;3
       lda    $99,X                   ;4
       ldx    $EB                     ;3
       beq    LF041                   ;2
       lda    LFD92,X                 ;4
       eor    $86                     ;3
       and    $87                     ;3
LF041:
       sta    $D4                     ;3
       jsr    LFB8B                   ;6
       lda    $DE                     ;3
       lsr                            ;2
       bcc    LF081                   ;2
       ldy    #$05                    ;2
LF04D:
       sty    $A7                     ;3
       lda    #$00                    ;2
       cpy    $F7                     ;3
       bcc    LF058                   ;2
       lda.wy $B1,Y                   ;4
LF058:
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       sta.wy $C6,Y                   ;5
       beq    LF06C                   ;2
       tax                            ;2
       lda.wy $F1,Y                   ;4
       clc                            ;2
       adc    LFD2A,X                 ;4
       jsr    LFBDA                   ;6
LF06C:
       jsr    LFF81                   ;6
       sta    $A2                     ;3
       tya                            ;2
       sec                            ;2
       sbc    #$03                    ;2
       ora    $A2                     ;3
       ldy    $A7                     ;3
       sta.wy $BE,Y                   ;5
       dey                            ;2
       bpl    LF04D                   ;2
       bmi    LF0FA                   ;2 always branch

LF081:
       lda    $F0                     ;3
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       tax                            ;2
       lda    $B1                     ;3
       and    LFD16,X                 ;4
       sta    $C6                     ;3
       lda    $B2                     ;3
       and    LFD20,X                 ;4
       sta    $C7                     ;3
       lda    $B3                     ;3
       and    LFD16,X                 ;4
       sta    $C8                     ;3
       ldy    #$02                    ;2
LF09F:
       ldx    #$01                    ;2
LF0A1:
       stx    $A6                     ;3
       lda.wy $C6,Y                   ;4
       cpx    #$01                    ;2
       beq    LF0AE                   ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
LF0AE:
       and    #$07                    ;2
       beq    LF0C7                   ;2
       tax                            ;2
       lda    $F1                     ;3
       clc                            ;2
       adc    LFD2A,X                 ;4
       jsr    LFBDA                   ;6
       ldx    $A6                     ;3
       adc    LFD37,X                 ;4
       adc    LFD39,Y                 ;4
       jsr    LFBDA                   ;6
LF0C7:
       sta.wy $CC,Y                   ;5
       ldx    $A6                     ;3
       beq    LF0D1                   ;2
       sta.wy $D0,Y                   ;5
LF0D1:
       dex                            ;2
       bpl    LF0A1                   ;2
       dey                            ;2
       bpl    LF09F                   ;2
       ldx    #$02                    ;2
LF0D9:
       lda    $CC,X                   ;4
       jsr    LFF81                   ;6
       sta    $A2                     ;3
       tya                            ;2
       sec                            ;2
       sbc    #$03                    ;2
       ora    $A2                     ;3
       sta    $BE,X                   ;4
       lda    $D0,X                   ;4
       jsr    LFF81                   ;6
       sta    $A2                     ;3
       tya                            ;2
       sec                            ;2
       sbc    #$03                    ;2
       ora    $A2                     ;3
       sta    $C2,X                   ;4
       dex                            ;2
       bpl    LF0D9                   ;2
LF0FA:
       lda    $E6                     ;3
       beq    LF101                   ;2
       clc                            ;2
       adc    #$2E                    ;2
LF101:
       jsr    LFF81                   ;6
       sta    $AA                     ;3
       dey                            ;2
       dey                            ;2
       dey                            ;2
       tya                            ;2
       ora    $AA                     ;3
       sta    $AA                     ;3
       lda    $D6                     ;3
       jsr    LFF81                   ;6
       sta    $AE                     ;3
       dey                            ;2
       dey                            ;2
       dey                            ;2
       tya                            ;2
       ora    $AE                     ;3
       sta    $AE                     ;3
       ldx    #$02                    ;2
       lda    $EE                     ;3
       jsr    LFFA0                   ;6
       inx                            ;2
       lda    $EF                     ;3
       jsr    LFFA0                   ;6
       inx                            ;2
       lda    $BD                     ;3
       jsr    LFF79                   ;6
       jsr    LFF9F                   ;6
       jsr    LFF9F                   ;6
       sta    HMCLR                   ;3
LF138:
       lda    INTIM                   ;4
       bne    LF138                   ;2
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       sta    VBLANK                  ;3
       sta    NUSIZ1                  ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       lda    $88                     ;3
       sta    COLUBK                  ;3
       lda    $8A                     ;3
       sta    COLUPF                  ;3
       lda    $DE                     ;3
       lsr                            ;2
       bcc    LF159                   ;2
       jmp    LF2A5                   ;3

LF159:
       ldx    #$8F                    ;2
LF15B:
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       dex                            ;2
       txa                            ;2
       sec                            ;2
       sbc    $F9                     ;3
       and    #$F8                    ;2
       bne    LF16A                   ;2
       lda    #$02                    ;2
LF16A:
       sta    ENABL                   ;3
       cpx    $A1                     ;3
       bcs    LF15B                   ;2
       ldy    #$02                    ;2
LF172:
       sty    $CF                     ;3
       lda    #$00                    ;2
       sta    $A2                     ;3
       sta    $A3                     ;3
       stx    $A5                     ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       lda.wy $BE,Y                   ;4
       and    #$0F                    ;2
       tax                            ;2
       bne    LF193                   ;2
       lda    #$60                    ;2
       sta    $A2                     ;3
       nop                            ;2
       nop                            ;2
       sta    RESP0                   ;3
       jmp    LF199                   ;3

LF193:
       dex                            ;2
       bpl    LF193                   ;2
       nop                            ;2
       sta    RESP0                   ;3
LF199:
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       lda.wy $C2,Y                   ;4
       and    #$0F                    ;2
       tax                            ;2
       bne    LF1B0                   ;2
       lda    #$60                    ;2
       sta    $A3                     ;3
       nop                            ;2
       nop                            ;2
       sta    RESP1                   ;3
       jmp    LF1B6                   ;3

LF1B0:
       dex                            ;2
       bpl    LF1B0                   ;2
       nop                            ;2
       sta    RESP1                   ;3
LF1B6:
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       lda.wy $C6,Y                   ;4
       and    #$07                    ;2
       tax                            ;2
       stx    $A6                     ;3
       lda    LFD2F,X                 ;4
       sta    NUSIZ1                  ;3
       lda.wy $C6,Y                   ;4
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       tax                            ;2
       stx    $A7                     ;3
       lda    LFD2F,X                 ;4
       sta    NUSIZ0                  ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       jsr    LFF9C                   ;6
       lda.wy $BE,Y                   ;4
       sta    HMP0                    ;3
       lda.wy $C2,Y                   ;4
       sta    HMP1                    ;3
       ldx    $9F                     ;3
       lda    $A7                     ;3
       bne    LF1EF                   ;2
       ldx    $D4                     ;3
LF1EF:
       stx    $A4                     ;3
       ldx    $9F                     ;3
       lda    $A6                     ;3
       bne    LF1F9                   ;2
       ldx    $D4                     ;3
LF1F9:
       sec                            ;2
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       lda    $A4                     ;3
       sta    COLUP0                  ;3
       stx    COLUP1                  ;3
       lda    $A5                     ;3
       sbc    #$04                    ;2
       tax                            ;2
       lda    $A2                     ;3
       sta    HMP0                    ;3
       lda    $A3                     ;3
       sta    HMP1                    ;3
       ldy    #$08                    ;2
LF213:
       dex                            ;2
       txa                            ;2
       sec                            ;2
       sbc    $EC                     ;3
       and    #$F8                    ;2
       bne    LF21E                   ;2
       lda    #$02                    ;2
LF21E:
       sta    ENAM0                   ;3
       txa                            ;2
       sec                            ;2
       sbc    $ED                     ;3
       and    #$F8                    ;2
       bne    LF22A                   ;2
       lda    #$02                    ;2
LF22A:
       sta    ENAM1                   ;3
       lda    ($AF),Y                 ;5
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       sta    GRP0                    ;3
       sta    GRP1                    ;3
       txa                            ;2
       sec                            ;2
       sbc    $F9                     ;3
       and    #$F8                    ;2
       bne    LF240                   ;2
       lda    #$02                    ;2
LF240:
       sta    ENABL                   ;3
       sta.w  HMCLR                   ;4
       dey                            ;2
       bpl    LF213                   ;2
       ldy    $CF                     ;3
       bit    WSYNC                   ;3
       bvs    LF250                   ;2
       sty    $E9                     ;3
LF250:
       bit    RSYNC                   ;3
       bvs    LF256                   ;2
       sty    $EA                     ;3
LF256:
       dey                            ;2
       bmi    LF25C                   ;2
       jmp    LF172                   ;3

LF25C:
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       dex                            ;2
       lda    #$00                    ;2
       sta    NUSIZ0                  ;3
       sta    NUSIZ1                  ;3
       lda    $D4                     ;3
       sta    COLUP0                  ;3
       sta    COLUP1                  ;3
       sta    COLUP1                  ;3
       jsr    LFF9C                   ;6
LF272:
       txa                            ;2
       sec                            ;2
       sbc    $EC                     ;3
       and    #$F8                    ;2
       bne    LF27C                   ;2
       lda    #$02                    ;2
LF27C:
       sta    ENAM0                   ;3
       txa                            ;2
       sec                            ;2
       sbc    $ED                     ;3
       and    #$F8                    ;2
       bne    LF288                   ;2
       lda    #$02                    ;2
LF288:
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       sta    ENAM1                   ;3
       txa                            ;2
       sec                            ;2
       sbc    $F9                     ;3
       and    #$F8                    ;2
       bne    LF298                   ;2
       lda    #$02                    ;2
LF298:
       sta    ENABL                   ;3
       jsr    LFF9E                   ;6
       dex                            ;2
       cpx    #$18                    ;2
       bcs    LF272                   ;2
       jmp    LF3C2                   ;3

LF2A5:
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       nop                            ;2
       nop                            ;2
       lda    $AE                     ;3
       and    #$0F                    ;2
       tay                            ;2
LF2B0:
       dey                            ;2
       bpl    LF2B0                   ;2
       nop                            ;2
       sta    RESP1                   ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       lda    $D4                     ;3
       sta    COLUP1                  ;3
       lda    $9F                     ;3
       sta    COLUP0                  ;3
       jsr    LFF9F                   ;6
       lda    $AE                     ;3
       sta    HMP1                    ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       ldx    #$90                    ;2
       ldy    #$05                    ;2
       sty    $CF                     ;3
LF2D3:
       dex                            ;2
       beq    LF2FF                   ;2
       lda.wy $C6,Y                   ;4
       tay                            ;2
       lda    LFD2F,Y                 ;4
       sta    NUSIZ0                  ;3
       sta    HMCLR                   ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       lda    #$00                    ;2
       cpx    #$15                    ;2
       bcs    LF2F0                   ;2
       sta    ENABL                   ;3
       lda    LFC6A,X                 ;4
LF2F0:
       sta    GRP1                    ;3
       txa                            ;2
       sec                            ;2
       sbc    $ED                     ;3
       and    #$F8                    ;2
       bne    LF2FC                   ;2
       lda    #$02                    ;2
LF2FC:
       sta    ENAM1                   ;3
       dex                            ;2
LF2FF:
       beq    LF31B                   ;2
       ldy    $CF                     ;3
       lda.wy $BE,Y                   ;4
       and    #$0F                    ;2
       tay                            ;2
       lda    #$00                    ;2
       cpx    #$15                    ;2
       bcs    LF314                   ;2
       sta    ENABL                   ;3
       lda    LFC6A,X                 ;4
LF314:
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       sta    GRP1                    ;3
       dex                            ;2
LF31B:
       beq    LF37A                   ;2
       cpy    #$00                    ;2
       bne    LF32E                   ;2
       nop                            ;2
       nop                            ;2
       nop                            ;2
       nop                            ;2
       sta    RESP0                   ;3
       lda    #$60                    ;2
       sta    HMP0                    ;3
       jmp    LF334                   ;3

LF32E:
       dey                            ;2
       bpl    LF32E                   ;2
       sta.w  RESP0                   ;4
LF334:
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       lda    #$00                    ;2
       cpx    #$15                    ;2
       bcs    LF343                   ;2
       sta    ENABL                   ;3
       lda    LFC6A,X                 ;4
LF343:
       sta    GRP1                    ;3
       txa                            ;2
       sec                            ;2
       sbc    $ED                     ;3
       and    #$F8                    ;2
       bne    LF34F                   ;2
       lda    #$02                    ;2
LF34F:
       sta    ENAM1                   ;3
       ldy    $CF                     ;3
       lda.wy $BE,Y                   ;4
       nop                            ;2
       sta    HMP0                    ;3
       lda    #$18                    ;2
       cpy    #$05                    ;2
       bne    LF361                   ;2
       lda    $A9                     ;3
LF361:
       tay                            ;2
       dex                            ;2
       beq    LF3C0                   ;2
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       lda    #$00                    ;2
       cpx    #$15                    ;2
       bcs    LF374                   ;2
       sta    ENABL                   ;3
       lda    LFC6A,X                 ;4
LF374:
       sta    GRP1                    ;3
       jsr    LFF9C                   ;6
LF379:
       dex                            ;2
LF37A:
       beq    LF3C0                   ;2
       txa                            ;2
       sec                            ;2
       sbc    $ED                     ;3
       and    #$F8                    ;2
       sta    HMCLR                   ;3
       bne    LF388                   ;2
       lda    #$02                    ;2
LF388:
       sta    ENAM1                   ;3
       lda    ($AF),Y                 ;5
       and    LFD7E,Y                 ;4
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       sta    GRP0                    ;3
       cpx    #$15                    ;2
       bcc    LF3B6                   ;2
       txa                            ;2
       sec                            ;2
       sbc    $F9                     ;3
       and    #$F8                    ;2
       bne    LF3A3                   ;2
       lda    #$02                    ;2
LF3A3:
       sta    ENABL                   ;3
       dey                            ;2
       bpl    LF379                   ;2
       ldy    $CF                     ;3
       bit    WSYNC                   ;3
       bvs    LF3B0                   ;2
       sty    $E9                     ;3
LF3B0:
       dey                            ;2
       sty    $CF                     ;3
       jmp    LF2D3                   ;3

LF3B6:
       lda    LFC6A,X                 ;4
       sta    GRP1                    ;3
       lda    #$00                    ;2
       jmp    LF3A3                   ;3

LF3C0:
       beq    LF410                   ;2
LF3C2:
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       dex                            ;2
       dex                            ;2
       lda    $AE                     ;3
       and    #$0F                    ;2
       tay                            ;2
LF3CD:
       dey                            ;2
       bpl    LF3CD                   ;2
       nop                            ;2
       sta    RESP1                   ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       lda    $D4                     ;3
       sta    COLUP1                  ;3
       jsr    LFF9C                   ;6
       lda    $AE                     ;3
       sta    HMP1                    ;3
LF3E2:
       txa                            ;2
       sec                            ;2
       sbc    $EC                     ;3
       and    #$F8                    ;2
       bne    LF3EC                   ;2
       lda    #$02                    ;2
LF3EC:
       sta    ENAM0                   ;3
       txa                            ;2
       sec                            ;2
       sbc    $ED                     ;3
       and    #$F8                    ;2
       bne    LF3F8                   ;2
       lda    #$02                    ;2
LF3F8:
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       sta    ENAM1                   ;3
       lda    LFC6A,X                 ;4
       sta    GRP1                    ;3
       lda    #$00                    ;2
       sta    ENABL                   ;3
       sta    ENABL                   ;3
       nop                            ;2
       nop                            ;2
       sta    HMCLR                   ;3
       dex                            ;2
       bne    LF3E2                   ;2
LF410:
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       stx    GRP1                    ;3
       stx    ENAM0                   ;3
       stx    ENAM1                   ;3
       stx    GRP0                    ;3
       ldx    $EB                     ;3
       bne    LF443                   ;2
       lda    $EC                     ;3
       cmp    #$14                    ;2
       bcs    LF42C                   ;2
       bit    VSYNC                   ;3
       bpl    LF42C                   ;2
       ldx    #$1F                    ;2
LF42C:
       lda    $ED                     ;3
       cmp    #$14                    ;2
       bcs    LF438                   ;2
       bit    VBLANK                  ;3
       bvc    LF438                   ;2
       ldx    #$1F                    ;2
LF438:
       lda    $DE                     ;3
       lsr                            ;2
       bcc    LF443                   ;2
       bit    COLUP1                  ;3
       bpl    LF443                   ;2
       ldx    #$1F                    ;2
LF443:
       stx    $EB                     ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       lda    $AA                     ;3
       and    #$0F                    ;2
       tax                            ;2
       nop                            ;2
       nop                            ;2
LF450:
       dex                            ;2
       bpl    LF450                   ;2
       nop                            ;2
       sta    RESBL                   ;3
       lda    $AA                     ;3
       sta    HMBL                    ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       lda    #$FF                    ;2
       sta    PF0                     ;3
       sta    PF1                     ;3
       sta    PF2                     ;3
       lda    $8C                     ;3
       sta    COLUPF                  ;3
       ldx    #$20                    ;2
       stx    CTRLPF                  ;3
       sta.w  RESP0                   ;4
       sta    RESP1                   ;3
       stx    NUSIZ1                  ;3
       inx                            ;2
       stx    NUSIZ0                  ;3
       lda    $88                     ;3
       sta    COLUP0                  ;3
       sta    COLUP1                  ;3
       sta    HMCLR                   ;3
       lda    #$10                    ;2
       sta    HMP1                    ;3
       lda    $89                     ;3
       sta    COLUBK                  ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       bit    WSYNC                   ;3
       bvs    LF494                   ;2
       ldx    #$00                    ;2
       stx    $E9                     ;3
LF494:
       stx    CXCLR                   ;3
       jsr    LFF9D                   ;6
       sta    HMCLR                   ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       jsr    LFF9C                   ;6
       jsr    LFF9C                   ;6
       jsr    LFF9F                   ;6
       jsr    LFF9E                   ;6
       lda    #$02                    ;2
       sta    ENABL                   ;3
       ldx    #$04                    ;2
LF4B1:
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       lda    LFCE0,X                 ;4
       sta    GRP0                    ;3
       lda    LFCE5,X                 ;4
       sta    GRP1                    ;3
       lda    LFCEA,X                 ;4
       ldy    $AD                     ;3
       sta    HMCLR                   ;3
       sty    PF2                     ;3
       sta    GRP0                    ;3
       lda    $8B                     ;3
       sta    COLUPF                  ;3
       lda    $AB                     ;3
       sta    PF0                     ;3
       lda    $AC                     ;3
       sta    PF1                     ;3
       ldy    #$FF                    ;2
       sty    PF2                     ;3
       sty    PF0                     ;3
       lda    $8C                     ;3
       sta    COLUPF                  ;3
       sty    PF1                     ;3
       dex                            ;2
       bpl    LF4B1                   ;2
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       inx                            ;2
       stx    GRP0                    ;3
       stx    GRP1                    ;3
       stx    ENABL                   ;3
       ldx    $D7                     ;3
       ldy    $9D,X                   ;4
       lda    LFD6F,Y                 ;4
       sta    NUSIZ1                  ;3
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       sta    NUSIZ0                  ;3
       sta    RESP0                   ;3
       sta    RESP1                   ;3
       lda    #$F0                    ;2
       sta    HMP0                    ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       lda    $9B,X                   ;4
       sta    COLUP0                  ;3
       sta    COLUP1                  ;3
       jsr    LFF9F                   ;6
       sta    HMCLR                   ;3
       tya                            ;2
       tax                            ;2
       ldy    #$09                    ;2
LF51A:
       lda    LFD0C,Y                 ;4
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       sta    GRP0                    ;3
       sta    GRP1                    ;3
       lda    #$00                    ;2
       cpx    #$02                    ;2
       bcs    LF52D                   ;2
       sta    GRP1                    ;3
LF52D:
       cpx    #$00                    ;2
       bne    LF533                   ;2
       sta    GRP0                    ;3
LF533:
       dey                            ;2
       bpl    LF51A                   ;2
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       iny                            ;2
       sty    GRP0                    ;3
       sty    GRP1                    ;3
       sty    GRP1                    ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       lda    #$03                    ;2
       sta    NUSIZ0                  ;3
       sta    NUSIZ1                  ;3
       jsr    LFF9C                   ;6
       sta    HMCLR                   ;3
       ldx    $D7                     ;3
       lda    $9B,X                   ;4
       sta    COLUP0                  ;3
       sta    COLUP1                  ;3
       ldy    #$07                    ;2
       sty    VDELP0                  ;3
       sty    VDELP1                  ;3
LF55E:
       sty    $A2                     ;3
       lda    ($97),Y                 ;5
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       sta    $A3                     ;3
       lda    ($8D),Y                 ;5
       sta    GRP0                    ;3
       lda    ($8F),Y                 ;5
       sta    GRP1                    ;3
       lda    ($91),Y                 ;5
       sta    GRP0                    ;3
       lda    ($95),Y                 ;5
       tax                            ;2
       lda    ($93),Y                 ;5
       ldy    $A3                     ;3
       sta    GRP1                    ;3
       stx    GRP0                    ;3
       sty    GRP1                    ;3
       sta    GRP0                    ;3
       ldy    $A2                     ;3
       dey                            ;2
       bpl    LF55E                   ;2
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       iny                            ;2
       sty    GRP0                    ;3
       sty    GRP1                    ;3
       sty    GRP0                    ;3
       sty    GRP1                    ;3
       nop                            ;2
       nop                            ;2
       nop                            ;2
       nop                            ;2
       sta    RESP0                   ;3
       sta    RESP1                   ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       lda    $88                     ;3
       sta    COLUBK                  ;3
       lda    #$00                    ;2
       sta    PF0                     ;3
       sta    PF1                     ;3
       sta    PF2                     ;3
       jsr    LFF9F                   ;6
       lda    #$10                    ;2
       sta    HMP1                    ;3
       lda    #$16                    ;2
       eor    $86                     ;3
       and    $87                     ;3
       sta    COLUP0                  ;3
       sta    COLUP1                  ;3
       ldx    #$03                    ;2
       stx    NUSIZ0                  ;3
       stx    NUSIZ1                  ;3
       ldy    #$0F                    ;2
       lda    #$07                    ;2
       sta    $A3                     ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       sta    VDELP0                  ;3
       sta    VDELP1                  ;3
       lda    #$00                    ;2
       sta    GRP0                    ;3
       sta    GRP1                    ;3
       sta    GRP0                    ;3
       lda    $DA                     ;3
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       cmp    #$14                    ;2
       bcs    LF5EC                   ;2
       ldy    #$07                    ;2
       cmp    #$0C                    ;2
       bcc    LF5EC                   ;2
       sbc    #$04                    ;2
       tay                            ;2
LF5EC:
       sty    $A4                     ;3
       sta    HMCLR                   ;3
LF5F0:
       ldy    $A4                     ;3
       lda    LFCD0,Y                 ;4
       sta    $A2                     ;3
       lda    LFCC0,Y                 ;4
       tax                            ;2
       lda    LFC80,Y                 ;4
       sta    GRP0                    ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       dec    $A4                     ;5
       lda    LFC90,Y                 ;4
       sta    GRP1                    ;3
       lda    LFCA0,Y                 ;4
       sta    GRP0                    ;3
       lda    LFCB0,Y                 ;4
       ldy    $A2                     ;3
       sta    GRP1                    ;3
       stx    GRP0                    ;3
       sty    GRP1                    ;3
       sta    GRP0                    ;3
       dec    $A3                     ;5
       bpl    LF5F0                   ;2
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       lda    #$00                    ;2
       sta    VDELP0                  ;3
       sta    VDELP1                  ;3
       sta    GRP0                    ;3
       sta    GRP1                    ;3
       lda    $DA                     ;3
       beq    LF639                   ;2
       dec    $DA                     ;5
       bne    LF639                   ;2
       dec    $DA                     ;5
LF639:
       lda    #$1F                    ;2
       ldx    #$82                    ;2
       sta    WSYNC                   ;3
       sta    TIM64T                  ;4
       stx    VBLANK                  ;3
       ldx    #$01                    ;2
LF646:
       ldy    $EC,X                   ;4
       tya                            ;2
       clc                            ;2
       adc    #$10                    ;2
       cmp    #$F0                    ;2
       bcc    LF654                   ;2
       ldy    #$F0                    ;2
       bne    LF65D                   ;2 always branch

LF654:
       dey                            ;2
       dey                            ;2
       lda    $DE                     ;3
       cmp    #$10                    ;2
       bcc    LF65D                   ;2
       dey                            ;2
LF65D:
       sty    $EC,X                   ;4
       dex                            ;2
       bpl    LF646                   ;2
       ldy    #$02                    ;2
LF664:
       tya                            ;2
       asl                            ;2
       asl                            ;2
       tax                            ;2
       lda.wy $DB,Y                   ;4
       and    #$F0                    ;2
       lsr                            ;2
       adc    #<DigitGFX              ;2
       sta    $8D,X                   ;4
       lda.wy $DB,Y                   ;4
       and    #$0F                    ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       clc                            ;2
       adc    #<DigitGFX              ;2
       sta    $8F,X                   ;4
       dey                            ;2
       bpl    LF664                   ;2
       ldx    #$00                    ;2
LF684:
       lda    $8D,X                   ;4
       eor    #<DigitGFX              ;2
       bne    LF694                   ;2
       lda    #<SpaceGFX              ;2
       sta    $8D,X                   ;4
       inx                            ;2
       inx                            ;2
       cpx    #$0A                    ;2
       bcc    LF684                   ;2
LF694:
       ldy    #$05                    ;2
LF696:
       lda.wy $B1,Y                   ;4
       bne    LF6B3                   ;2
       dey                            ;2
       bpl    LF696                   ;2
       ldx    #$07                    ;2
       stx    $D9                     ;3
       dex                            ;2
       stx    $F7                     ;3
       inc    $DE                     ;5
       lda    $DE                     ;3
       and    #$07                    ;2
       sta    $DF                     ;3
       ldx    #$00                    ;2
       stx    $D5                     ;3
       stx    $F0                     ;3
LF6B3:
       lda    $DA                     ;3
       beq    LF6D8                   ;2
       lda    #$00                    ;2
       sta    AUDV0                   ;3
       sta    AUDV1                   ;3
       lda    $DD                     ;3
       and    #$0F                    ;2
       bne    LF6D5                   ;2
       lda    $88                     ;3
       sta    $D4                     ;3
       lda    $80                     ;3
       lsr                            ;2
       bcc    LF6D5                   ;2
       lda    $81                     ;3
       and    #$7F                    ;2
       bne    LF6D5                   ;2
       jsr    LFBE4                   ;6
LF6D5:
       jmp    LF707                   ;3

LF6D8:
       ldy    $DF                     ;3
       ldx    $D5                     ;3
       lda    $81                     ;3
       and    #$03                    ;2
       bne    LF6E8                   ;2
       dex                            ;2
       bpl    LF6E8                   ;2
       ldx    LFD76,Y                 ;4
LF6E8:
       stx    $D5                     ;3
       jsr    LFC0A                   ;6
       ldx    $DF                     ;3
       lda    $81                     ;3
       and    LFDB2,X                 ;4
       bne    LF6F8                   ;2
       inc    $A0                     ;5
LF6F8:
       lda    $A0                     ;3
       and    #$7E                    ;2
       cmp    #$40                    ;2
       bcc    LF702                   ;2
       eor    #$7F                    ;2
LF702:
       clc                            ;2
       adc    #$50                    ;2
       sta    $A1                     ;3
LF707:
       lda    $DE                     ;3
       and    #$0F                    ;2
       tax                            ;2
       lda    LFCF4,X                 ;4
       eor    $86                     ;3
       and    $87                     ;3
       sta    $9F                     ;3
LF715:
       lda    INTIM                   ;4
       bne    LF715                   ;2
       ldy    #$82                    ;2
       sty    WSYNC                   ;3
       sty    VSYNC                   ;3
       sty    WSYNC                   ;3
       sty    WSYNC                   ;3
       sty    WSYNC                   ;3
       sta    VSYNC                   ;3
       inc    $81                     ;5
       bne    LF733                   ;2
       inc    $D3                     ;5
       bne    LF733                   ;2
       sec                            ;2
       ror    $D3                     ;5
LF733:
       ldy    #$FF                    ;2
       lda    SWCHB                   ;4
       and    #$08                    ;2
       bne    LF73E                   ;2
       ldy    #$0F                    ;2
LF73E:
       tya                            ;2
       ldy    #$00                    ;2
       bit    $D3                     ;3
       bpl    LF749                   ;2
       and    #$F7                    ;2
       ldy    $D3                     ;3
LF749:
       sty    $86                     ;3
       asl    $86                     ;5
       sta    $87                     ;3
       lda    #$30                    ;2
       sta    WSYNC                   ;3
       sta    TIM64T                  ;4
       lda    SWCHA                   ;4
       tay                            ;2
       and    #$0F                    ;2
       sta    $85                     ;3
       tya                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       sta    $84                     ;3
       iny                            ;2
       beq    LF76C                   ;2
       lda    #$00                    ;2
       sta    $D3                     ;3
LF76C:
       lda    SWCHB                   ;4
       lsr                            ;2
       bcs    LF777                   ;2
       ldx    #$D3                    ;2
       jmp    LF004                   ;3

LF777:
       ldy    #$00                    ;2
       lsr                            ;2
       bcs    LF7A6                   ;2
       lda    $83                     ;3
       beq    LF784                   ;2
       dec    $83                     ;5
       bpl    LF7A8                   ;2 always branch

LF784:
       inc    $80                     ;5
LF786:
       lda    $80                     ;3
       and    #$03                    ;2
       sta    $80                     ;3
       sta    $D3                     ;3
       ldy    #$00                    ;2
       sty    $DB                     ;3
       sty    $DC                     ;3
       sty    $DF                     ;3
       sty    $E0                     ;3
       sty    AUDV0                   ;3
       sty    AUDV1                   ;3
       tay                            ;2
       iny                            ;2
       sty    $DD                     ;3
       lda    #$FF                    ;2
       sta    $DA                     ;3
       ldy    #$1E                    ;2
LF7A6:
       sty    $83                     ;3
LF7A8:
       lda    $DA                     ;3
       beq    LF7AF                   ;2
       jmp    LF01B                   ;3

LF7AF:
       lda    $F8                     ;3
       beq    LF7C5                   ;2
       dec    $F8                     ;5
       lsr                            ;2
       sta    AUDV1                   ;3
       ldx    #$1F                    ;2
       lsr                            ;2
       bcs    LF7BF                   ;2
       ldx    #$00                    ;2
LF7BF:
       stx    AUDF1                   ;3
       lda    #$07                    ;2
       sta    AUDC1                   ;3
LF7C5:
       lda    $D9                     ;3
       lsr                            ;2
       bcs    LF824                   ;2
       lsr                            ;2
       bcs    LF7FB                   ;2
       lsr                            ;2
       bcs    LF7D3                   ;2
       jmp    LF85C                   ;3

LF7D3:
       inc    $E6                     ;5
       lda    $E6                     ;3
       cmp    #$53                    ;2
       bcs    LF7EB                   ;2
       lsr                            ;2
       lsr                            ;2
       eor    #$FF                    ;2
       sta    AUDF0                   ;3
       lda    #$08                    ;2
       sta    AUDC0                   ;3
       lda    #$04                    ;2
       sta    AUDV0                   ;3
       bne    LF80B                   ;2 always branch

LF7EB:
       lda    $D9                     ;3
       eor    #$04                    ;2
       sta    $D9                     ;3
       lda    #$00                    ;2
       sta    AUDV0                   ;3
       lda    #$40                    ;2
       sta    $A0                     ;3
       bne    LF80B                   ;2 always branch

LF7FB:
       lda    $81                     ;3
       and    #$01                    ;2
       bne    LF80B                   ;2
       lda    $E6                     ;3
       bne    LF80E                   ;2
       lda    $D9                     ;3
       eor    #$02                    ;2
       sta    $D9                     ;3
LF80B:
       jmp    LFB88                   ;3

LF80E:
       and    #$0F                    ;2
       eor    #$FF                    ;2
       sta    AUDF0                   ;3
       lda    #$04                    ;2
       sta    AUDV0                   ;3
       lda    #$0C                    ;2
       sta    AUDC0                   ;3
       jsr    LFFAD                   ;6
       dec    $E6                     ;5
       jmp    LFB4D                   ;3

LF824:
       lda    $D9                     ;3
       eor    #$01                    ;2
       sta    $D9                     ;3
       ldy    #$05                    ;2
LF82C:
       ldx    #$70                    ;2
       lda    $DE                     ;3
       cmp    #$10                    ;2
       bcc    LF840                   ;2
       cmp    #$17                    ;2
       beq    LF83E                   ;2
       and    #$07                    ;2
       cmp    #$03                    ;2
       bcs    LF840                   ;2
LF83E:
       ldx    #$50                    ;2
LF840:
       lsr                            ;2
       bcs    LF846                   ;2
       ldx    LFD6C,Y                 ;4
LF846:
       stx    $B1,Y                   ;4
       ldx    #$00                    ;2
       stx    $EB                     ;3
       stx    $F1,Y                   ;4
       dey                            ;2
       bpl    LF82C                   ;2
       iny                            ;2
       sty    $F0                     ;3
       lda    #$F0                    ;2
       sta    $EC                     ;3
       sta    $ED                     ;3
       bne    LF8C9                   ;2 always branch

LF85C:
       ldx    $EB                     ;3
       bne    LF86B                   ;2
       lda    $E6                     ;3
       beq    LF867                   ;2
       jmp    LF8CC                   ;3

LF867:
       ldx    #$1F                    ;2
       stx    $EB                     ;3
LF86B:
       txa                            ;2
       and    #$1F                    ;2
       tax                            ;2
       cmp    #$10                    ;2
       bcc    LF875                   ;2
       eor    #$1F                    ;2
LF875:
       lsr                            ;2
       sta    AUDV0                   ;3
       ldy    #$08                    ;2
       lda    $81                     ;3
       lsr                            ;2
       bcs    LF881                   ;2
       ldy    #$0F                    ;2
LF881:
       sty    AUDC0                   ;3
       lda    #$F0                    ;2
       sta    $EC                     ;3
       sta    $ED                     ;3
       lda    #$00                    ;2
       sta    AUDF0                   ;3
       sta    $E6                     ;3
       sta    $E9                     ;3
       sta    $EA                     ;3
       sta    $F9                     ;3
       cpx    #$09                    ;2
       bcs    LF8A5                   ;2
       sta    $F0                     ;3
       ldx    #$06                    ;2
       stx    $F7                     ;3
       dex                            ;2
LF8A0:
       sta    $F1,X                   ;4
       dex                            ;2
       bpl    LF8A0                   ;2
LF8A5:
       lda    $81                     ;3
       and    #$03                    ;2
       bne    LF8C9                   ;2
       dec    $EB                     ;5
       bne    LF8C9                   ;2
       lda    $9D                     ;3
       ora    $9E                     ;3
       bne    LF8BE                   ;2
       inc    $DA                     ;5
       lda    $88                     ;3
       sta    $D4                     ;3
       jmp    LF8C9                   ;3

LF8BE:
       jsr    LFFCE                   ;6
       lda    #$50                    ;2
       sta    $D6                     ;3
       lda    #$04                    ;2
       sta    $D9                     ;3
LF8C9:
       jmp    LFB88                   ;3

LF8CC:
       lda    $DE                     ;3
       lsr                            ;2
       bcc    LF8D4                   ;2
       jmp    LF919                   ;3

LF8D4:
       lda    $A1                     ;3
       cmp    #$55                    ;2
       bcs    LF8DD                   ;2
       jmp    LF946                   ;3

LF8DD:
       lda    $81                     ;3
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       and    #$01                    ;2
       tax                            ;2
       ldy    #$00                    ;2
LF8E8:
       sty    $A7                     ;3
       lda.wy $B1,Y                   ;4
       and    LFD0A,X                 ;4
       beq    LF911                   ;2
       lda    $EC,X                   ;4
       cmp    #$F0                    ;2
       bne    LF911                   ;2
       txa                            ;2
       asl                            ;2
       asl                            ;2
       clc                            ;2
       adc    $A7                     ;3
       tay                            ;2
       lda.wy $CC,Y                   ;4
       clc                            ;2
       adc    #$05                    ;2
       sta    $EE,X                   ;4
       lda    $A1                     ;3
       sec                            ;2
       ldy    $A7                     ;3
       sbc    LFD69,Y                 ;4
       sta    $EC,X                   ;4
LF911:
       iny                            ;2
       cpy    #$03                    ;2
       bcc    LF8E8                   ;2
       jmp    LF946                   ;3

LF919:
       lda    $DF                     ;3
       eor    #$07                    ;2
       beq    LF946                   ;2
       lda    $F7                     ;3
       cmp    #$05                    ;2
       bcs    LF946                   ;2
       lda    $ED                     ;3
       cmp    #$F0                    ;2
       bne    LF946                   ;2
       lda    $B5                     ;3
       beq    LF946                   ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       tay                            ;2
       lda    $F5                     ;3
       clc                            ;2
       adc    #$05                    ;2
       clc                            ;2
       adc    LFD2A,Y                 ;4
       sta    $EF                     ;3
       lda    #$70                    ;2
       sec                            ;2
       sbc    $A9                     ;3
       sta    $ED                     ;3
LF946:
       ldy    $D7                     ;3
       ldx    $D6                     ;3
       lda.wy $84,Y                   ;4
       and    #$08                    ;2
       bne    LF956                   ;2
       cpx    #$84                    ;2
       bcs    LF956                   ;2
       inx                            ;2
LF956:
       lda.wy $84,Y                   ;4
       and    #$04                    ;2
       bne    LF962                   ;2
       cpx    #$18                    ;2
       bcc    LF962                   ;2
       dex                            ;2
LF962:
       stx    $D6                     ;3
       lda    $D6                     ;3
       clc                            ;2
       adc    #$05                    ;2
       tax                            ;2
       lda    $80                     ;3
       cmp    #$02                    ;2
       bcs    LF976                   ;2
       stx    $BD                     ;3
       lda    #$FF                    ;2
       sta    $D8                     ;3
LF976:
       lda    $F9                     ;3
       bne    LF990                   ;2
       stx    $BD                     ;3
       sta    AUDV0                   ;3
       lda.wy REFP1,Y                 ;4
       tay                            ;2
       eor    $D8                     ;3
       and    $D8                     ;3
       sty    $D8                     ;3
       bpl    LF9B7                   ;2
       lda    #$04                    ;2
       sta    AUDV0                   ;3
       lda    #$11                    ;2
LF990:
       sta    $A2                     ;3
       ldy    #$04                    ;2
       lda    SWCHB                   ;4
       asl                            ;2
       ldx    $D7                     ;3
       bne    LF99D                   ;2
       asl                            ;2
LF99D:
       bcc    LF9A0                   ;2
       dey                            ;2
LF9A0:
       tya                            ;2
       clc                            ;2
       adc    $A2                     ;3
       cmp    #$92                    ;2
       bcc    LF9AC                   ;2
       lda    #$00                    ;2
       sta    AUDV0                   ;3
LF9AC:
       sta    $F9                     ;3
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       sta    AUDF0                   ;3
       lda    #$0F                    ;2
       sta    AUDC0                   ;3
LF9B7:
       ldx    #$01                    ;2
LF9B9:
       stx    $A6                     ;3
       ldy    $E9,X                   ;4
       beq    LFA04                   ;2
       dey                            ;2
       sty    $A7                     ;3
       ldx    $F1,Y                   ;4
       lda    $DE                     ;3
       lsr                            ;2
       bcs    LF9D4                   ;2
       ldx    $A6                     ;3
       lda    $F1                     ;3
       adc    LFD37,X                 ;4
       jsr    LFBDA                   ;6
       tax                            ;2
LF9D4:
       stx    $A2                     ;3
       lda    $BD                     ;3
       sec                            ;2
       sbc    $A2                     ;3
       bcs    LF9DF                   ;2
       adc    #$A0                    ;2
LF9DF:
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       tay                            ;2
       lda    LFD04,Y                 ;4
       ldx    $A6                     ;3
       cpx    #$00                    ;2
       beq    LF9F1                   ;2
       lda    LFD07,Y                 ;4
LF9F1:
       ldy    $A7                     ;3
       and.wy $B1,Y                   ;4
       sta.wy $B1,Y                   ;5
       lda    #$20                    ;2
       sta    $F8                     ;3
       jsr    LFFAD                   ;6
       lda    #$00                    ;2
       sta    $F9                     ;3
LFA04:
       ldx    $A6                     ;3
       dex                            ;2
       bpl    LF9B9                   ;2
       lda    $DF                     ;3
       cmp    #$05                    ;2
       bne    LFA12                   ;2
       jmp    LFADE                   ;3

LFA12:
       lsr                            ;2
       bcs    LFA44                   ;2
       ldy    #$00                    ;2
       lda    $DE                     ;3
       cmp    #$07                    ;2
       bcc    LFA29                   ;2
       lda    $81                     ;3
       cmp    #$50                    ;2
       bcc    LFA41                   ;2
       cmp    #$80                    ;2
       bcs    LFA29                   ;2
       ldy    #$01                    ;2
LFA29:
       ldx    $F0                     ;3
       inc    $F1                     ;5
       lda    $F1                     ;3
       cmp    #$A0                    ;2
       bcc    LFA35                   ;2
       sbc    #$A0                    ;2
LFA35:
       sta    $F1                     ;3
       cpx    #$91                    ;2
       bcs    LFA3C                   ;2
       inx                            ;2
LFA3C:
       stx    $F0                     ;3
       dey                            ;2
       bpl    LFA29                   ;2
LFA41:
       jmp    LFB41                   ;3

LFA44:
       ldy    #$00                    ;2
       lda    $DE                     ;3
       and    #$0F                    ;2
       cmp    #$07                    ;2
       bne    LFA50                   ;2
       ldy    #$01                    ;2
LFA50:
       lda    $DE                     ;3
       cmp    #$10                    ;2
       bcc    LFA58                   ;2
       ldy    #$01                    ;2
LFA58:
       sty    $A7                     ;3
       ldx    $A9                     ;3
       lda    $DE                     ;3
       cmp    #$07                    ;2
       bcs    LFA68                   ;2
       lda    $81                     ;3
       and    #$50                    ;2
       bne    LFA69                   ;2
LFA68:
       inx                            ;2
LFA69:
       cpx    #$1D                    ;2
       bcc    LFAAD                   ;2
       lda    $B1                     ;3
       sta    $A2                     ;3
       lda    $F1                     ;3
       sta    $A3                     ;3
       ldy    #$01                    ;2
LFA77:
       lda.wy $F1,Y                   ;4
       sta.wy $F0,Y                   ;5
       lda.wy $B1,Y                   ;4
       sta.wy $B0,Y                   ;5
       iny                            ;2
       cpy    #$06                    ;2
       bcc    LFA77                   ;2
       lda    $A2                     ;3
       sta    $B6                     ;3
       lda    $A3                     ;3
       sta    $F6                     ;3
       lda    $F7                     ;3
       beq    LFA96                   ;2
       dec    $F7                     ;5
LFA96:
       lda    $DF                     ;3
       cmp    #$05                    ;2
       beq    LFAA6                   ;2
       jsr    LFEFE                   ;6
       and    #$7F                    ;2
       clc                            ;2
       adc    #$10                    ;2
       sta    $F6                     ;3
LFAA6:
       lda    $A8                     ;3
       lsr                            ;2
       ror    $A8                     ;5
       ldx    #$00                    ;2
LFAAD:
       stx    $A9                     ;3
       ldy    $A7                     ;3
       dey                            ;2
       bpl    LFA58                   ;2
       lda    $DF                     ;3
       cmp    #$01                    ;2
       beq    LFACA                   ;2
       cmp    #$03                    ;2
       beq    LFAC5                   ;2
       cmp    #$07                    ;2
       beq    LFB29                   ;2
       jmp    LFB41                   ;3

LFAC5:
       lda    #$AA                    ;2
       jmp    LFB31                   ;3

LFACA:
       ldy    #$05                    ;2
LFACC:
       ldx    $F1,Y                   ;4
       inx                            ;2
       bit    $81                     ;3
       bmi    LFAD5                   ;2
       dex                            ;2
       dex                            ;2
LFAD5:
       jsr    LFFDF                   ;6
       dey                            ;2
       bpl    LFACC                   ;2
       jmp    LFB41                   ;3

LFADE:
       ldx    $A9                     ;3
       lda    $81                     ;3
       and    #$1F                    ;2
       bne    LFAE9                   ;2
       jsr    LFEFE                   ;6
LFAE9:
       lda    $81                     ;3
       and    #$01                    ;2
       bne    LFAF8                   ;2
       lda    $82                     ;3
       and    #$07                    ;2
       cmp    #$04                    ;2
       bcc    LFAF8                   ;2
       inx                            ;2
LFAF8:
       stx    $A2                     ;3
       ldy    #$05                    ;2
LFAFC:
       ldx    $F1,Y                   ;4
       txa                            ;2
       sec                            ;2
       sbc    #$1A                    ;2
       cmp    #$27                    ;2
       bcc    LFB08                   ;2
       ldx    #$20                    ;2
LFB08:
       bit    $82                     ;3
       bmi    LFB15                   ;2
       inx                            ;2
       cpx    #$40                    ;2
       bcc    LFB1C                   ;2
       ldx    #$40                    ;2
       bne    LFB1C                   ;2 always branch

LFB15:
       dex                            ;2
       cpx    #$1A                    ;2
       bcs    LFB1C                   ;2
       ldx    #$1A                    ;2
LFB1C:
       stx    $F1,Y                   ;4
       dey                            ;2
       bpl    LFAFC                   ;2
       iny                            ;2
       sty    $A7                     ;3
       ldx    $A2                     ;3
       jmp    LFA69                   ;3

LFB29:
       lda    $DE                     ;3
       cmp    #$08                    ;2
       bcc    LFB41                   ;2
       lda    $A8                     ;3
LFB31:
       ldy    #$05                    ;2
LFB33:
       ldx    $F1,Y                   ;4
       inx                            ;2
       rol                            ;2
       bcc    LFB3B                   ;2
       dex                            ;2
       dex                            ;2
LFB3B:
       jsr    LFFDF                   ;6
       dey                            ;2
       bpl    LFB33                   ;2
LFB41:
       lda    $81                     ;3
       and    #$1F                    ;2
       bne    LFB4D                   ;2
       lda    $E6                     ;3
       beq    LFB4D                   ;2
       dec    $E6                     ;5
LFB4D:
       lda    $E0                     ;3 check amount to add to score
       beq    LFB83                   ;2  ...branch if none
       sed                            ;2 Set decimal mode to update score
       clc                            ;2
       adc    $DD                     ;3 add amount to last 2 digits
       sta    $DD                     ;3
       bcc    LFB83                   ;2 branch if no carry needed
       lda    $DC                     ;3
       adc    #$00                    ;2 add carry
       sta    $DC                     ;3
       lda    $DB                     ;3
       adc    #$00                    ;2 add carry

  IF ENDGAME
       bcc    LFB6D                   ;2 branch if score hasn't rolled yet
       lda    #$99                    ;2  if it did, reset score to 999999**
       sta    $DC                     ;3  **
       sta    $DD                     ;3  **
       inc    $DA                     ;5 ...and set game to inactive
LFB6D:
  ENDIF

       sta    $DB                     ;3  **
       lda    $DC                     ;3
       and    #$FF                    ;2
       bne    LFB83                   ;2
       ldy    $D7                     ;3
       lda.wy $9D,Y                   ;4
       cmp    #$06                    ;2
       bcs    LFB83                   ;2
       adc    #$01                    ;2
       sta.wy $9D,Y                   ;5
LFB83:
       cld                            ;2
       lda    #$00                    ;2
       sta    $E0                     ;3
LFB88:
       jmp    LF01B                   ;3


  IF ENDGAME = 0
LFFC5:
       .byte $20 ; |  X     | $FFC5
       .byte $30 ; |  XX    | $FFC6
       .byte $40 ; | X      | $FFC7
       .byte $50 ; | X X    | $FFC8
       .byte $60 ; | XX     | $FFC9
       .byte $70 ; | XXX    | $FFCA
       .byte $80 ; |X       | $FFCB
       .byte $90 ; |X  X    | $FFCC
       .byte $90 ; |X  X    | $FFCD
       .byte $00 ; unused
  ENDIF

LFB8B:
       ldx    #$06                    ;2
       lda    $DE                     ;3
       lsr                            ;2
       bcs    LFB96                   ;2
       ldx    #$03                    ;2
       stx    $EA                     ;3
LFB96:
       stx    $E9                     ;3
       lda    $E6                     ;3
       lsr                            ;2
       lsr                            ;2
       tax                            ;2
       lda    LFD3B,X                 ;4
       sta    $AB                     ;3
       lda    LFD54,X                 ;4
       sta    $AC                     ;3
       lda    LFD3F,X                 ;4
       sta    $AD                     ;3
       lda    #$84                    ;2
       eor    $86                     ;3
       and    $87                     ;3
       sta    $99                     ;3
       lda    #$82                    ;2
       eor    $86                     ;3
       and    $87                     ;3
       sta    $9B                     ;3
       ldx    #$C4                    ;2
       ldy    #$C2                    ;2
       lda    SWCHB                   ;4
       and    #$08                    ;2
       bne    LFBCB                   ;2
       ldx    #$08                    ;2
       ldy    #$0C                    ;2
LFBCB:
       txa                            ;2
       eor    $86                     ;3
       and    $87                     ;3
       sta    $9A                     ;3
       tya                            ;2
       eor    $86                     ;3
       and    $87                     ;3
       sta    $9C                     ;3
       rts                            ;6

LFBDA:
       bcs    LFBE0                   ;2
       cmp    #$A0                    ;2
       bcc    LFBE2                   ;2
LFBE0:
       sbc    #$A0                    ;2
LFBE2:
       clc                            ;2
       rts                            ;6

LFBE4:
       ldx    #$05                    ;2
LFBE6:
       lda    $B1,X                   ;4
       ldy    $B7,X                   ;4
       sty    $B1,X                   ;4
       sta    $B7,X                   ;4
       dex                            ;2
       bpl    LFBE6                   ;2
       ldx    #$04                    ;2
LFBF3:
       lda    $DB,X                   ;4
       ldy    $E1,X                   ;4
       sty    $DB,X                   ;4
       sta    $E1,X                   ;4
       dex                            ;2
       bpl    LFBF3                   ;2
       lda    $D7                     ;3
       eor    #$01                    ;2
       sta    $D7                     ;3
       tax                            ;2
       lda    LFD76,X                 ;4
       sta    $D5                     ;3
LFC0A:
       lda    $DF                     ;3
       asl                            ;2
       tay                            ;2
       lda    LFDBA,Y                 ;4
       sta    $A2                     ;3
       jmp    LFFEE                   ;3

DigitGFX:
       .byte $3C ; |  XXXX  | $FC16
       .byte $66 ; | XX  XX | $FC17
       .byte $66 ; | XX  XX | $FC18
       .byte $66 ; | XX  XX | $FC19
       .byte $66 ; | XX  XX | $FC1A
       .byte $66 ; | XX  XX | $FC1B
       .byte $66 ; | XX  XX | $FC1C
       .byte $3C ; |  XXXX  | $FC1D

       .byte $3C ; |  XXXX  | $FC1E
       .byte $18 ; |   XX   | $FC1F
       .byte $18 ; |   XX   | $FC20
       .byte $18 ; |   XX   | $FC21
       .byte $18 ; |   XX   | $FC22
       .byte $18 ; |   XX   | $FC23
       .byte $38 ; |  XXX   | $FC24
       .byte $18 ; |   XX   | $FC25

       .byte $7E ; | XXXXXX | $FC26
       .byte $60 ; | XX     | $FC27
       .byte $60 ; | XX     | $FC28
       .byte $3C ; |  XXXX  | $FC29
       .byte $06 ; |     XX | $FC2A
       .byte $06 ; |     XX | $FC2B
       .byte $46 ; | X   XX | $FC2C
       .byte $3C ; |  XXXX  | $FC2D

       .byte $3C ; |  XXXX  | $FC2E
       .byte $46 ; | X   XX | $FC2F
       .byte $06 ; |     XX | $FC30
       .byte $0C ; |    XX  | $FC31
       .byte $0C ; |    XX  | $FC32
       .byte $06 ; |     XX | $FC33
       .byte $46 ; | X   XX | $FC34
       .byte $3C ; |  XXXX  | $FC35

       .byte $0C ; |    XX  | $FC36
       .byte $0C ; |    XX  | $FC37
       .byte $0C ; |    XX  | $FC38
       .byte $7E ; | XXXXXX | $FC39
       .byte $4C ; | X  XX  | $FC3A
       .byte $2C ; |  X XX  | $FC3B
       .byte $1C ; |   XXX  | $FC3C
       .byte $0C ; |    XX  | $FC3D

       .byte $7C ; | XXXXX  | $FC3E
       .byte $46 ; | X   XX | $FC3F
       .byte $06 ; |     XX | $FC40
       .byte $06 ; |     XX | $FC41
       .byte $7C ; | XXXXX  | $FC42
       .byte $60 ; | XX     | $FC43
       .byte $60 ; | XX     | $FC44
       .byte $7E ; | XXXXXX | $FC45

       .byte $3C ; |  XXXX  | $FC46
       .byte $66 ; | XX  XX | $FC47
       .byte $66 ; | XX  XX | $FC48
       .byte $66 ; | XX  XX | $FC49
       .byte $7C ; | XXXXX  | $FC4A
       .byte $60 ; | XX     | $FC4B
       .byte $62 ; | XX   X | $FC4C
       .byte $3C ; |  XXXX  | $FC4D

       .byte $18 ; |   XX   | $FC4E
       .byte $18 ; |   XX   | $FC4F
       .byte $18 ; |   XX   | $FC50
       .byte $18 ; |   XX   | $FC51
       .byte $0C ; |    XX  | $FC52
       .byte $06 ; |     XX | $FC53
       .byte $42 ; | X    X | $FC54
       .byte $7E ; | XXXXXX | $FC55

       .byte $3C ; |  XXXX  | $FC56
       .byte $66 ; | XX  XX | $FC57
       .byte $66 ; | XX  XX | $FC58
       .byte $3C ; |  XXXX  | $FC59
       .byte $3C ; |  XXXX  | $FC5A
       .byte $66 ; | XX  XX | $FC5B
       .byte $66 ; | XX  XX | $FC5C
       .byte $3C ; |  XXXX  | $FC5D

       .byte $3C ; |  XXXX  | $FC5E
       .byte $46 ; | X   XX | $FC5F
       .byte $06 ; |     XX | $FC60
       .byte $3E ; |  XXXXX | $FC61
       .byte $66 ; | XX  XX | $FC62
       .byte $66 ; | XX  XX | $FC63
       .byte $66 ; | XX  XX | $FC64
       .byte $3C ; |  XXXX  | $FC65

SpaceGFX:
       .byte $00 ; |        | $FC66
       .byte $00 ; |        | $FC67
       .byte $00 ; |        | $FC68
       .byte $00 ; |        | $FC69
LFC6A:
       .byte $00 ; |        | $FC6A shared
       .byte $00 ; |        | $FC6B shared
       .byte $00 ; |        | $FC6C shared
       .byte $00 ; |        | $FC6D shared
       .byte $41 ; | X     X| $FC6E
       .byte $41 ; | X     X| $FC6F
       .byte $41 ; | X     X| $FC70
       .byte $49 ; | X  X  X| $FC71
       .byte $6B ; | XX X XX| $FC72
       .byte $7F ; | XXXXXXX| $FC73
       .byte $5D ; | X XXX X| $FC74
       .byte $5D ; | X XXX X| $FC75
       .byte $49 ; | X  X  X| $FC76
       .byte $08 ; |    X   | $FC77
       .byte $08 ; |    X   | $FC78
       .byte $08 ; |    X   | $FC79
       .byte $1C ; |   XXX  | $FC7A
       .byte $1C ; |   XXX  | $FC7B
       .byte $3E ; |  XXXXX | $FC7C
       .byte $3E ; |  XXXXX | $FC7D
       .byte $1C ; |   XXX  | $FC7E
       .byte $08 ; |    X   | $FC7F
LFC80:
       .byte $00 ; |        | $FC80
       .byte $00 ; |        | $FC81
       .byte $00 ; |        | $FC82
       .byte $00 ; |        | $FC83
       .byte $00 ; |        | $FC84
       .byte $00 ; |        | $FC85
       .byte $00 ; |        | $FC86
       .byte $00 ; |        | $FC87
       .byte $00 ; |        | $FC88
       .byte $00 ; |        | $FC89
       .byte $F7 ; |XXXX XXX| $FC8A
       .byte $95 ; |X  X X X| $FC8B
       .byte $87 ; |X    XXX| $FC8C
       .byte $80 ; |X       | $FC8D
       .byte $90 ; |X  X    | $FC8E
       .byte $F0 ; |XXXX    | $FC8F
LFC90:
       .byte $AD ; |X X XX X| $FC90
       .byte $A9 ; |X X X  X| $FC91
       .byte $E9 ; |XXX X  X| $FC92
       .byte $A9 ; |X X X  X| $FC93
       .byte $ED ; |XXX XX X| $FC94
       .byte $41 ; | X     X| $FC95
       .byte $0F ; |    XXXX| $FC96
       .byte $00 ; |        | $FC97
       .byte $47 ; | X   XXX| $FC98
       .byte $41 ; | X     X| $FC99
       .byte $77 ; | XXX XXX| $FC9A
       .byte $55 ; | X X X X| $FC9B
       .byte $75 ; | XXX X X| $FC9C
       .byte $00 ; |        | $FC9D
       .byte $00 ; |        | $FC9E
       .byte $00 ; |        | $FC9F
LFCA0:
       .byte $50 ; | X X    | $FCA0
       .byte $58 ; | X XX   | $FCA1
       .byte $5C ; | X XXX  | $FCA2
       .byte $56 ; | X X XX | $FCA3
       .byte $53 ; | X X  XX| $FCA4
       .byte $11 ; |   X   X| $FCA5
       .byte $F0 ; |XXXX    | $FCA6
       .byte $00 ; |        | $FCA7
       .byte $03 ; |      XX| $FCA8
       .byte $00 ; |        | $FCA9
       .byte $4B ; | X  X XX| $FCAA
       .byte $4A ; | X  X X | $FCAB
       .byte $6B ; | XX X XX| $FCAC
       .byte $00 ; |        | $FCAD
       .byte $08 ; |    X   | $FCAE
       .byte $00 ; |        | $FCAF
LFCB0:
       .byte $BA ; |X XXX X | $FCB0
       .byte $8A ; |X   X X | $FCB1
       .byte $BA ; |X XXX X | $FCB2
       .byte $A2 ; |X X   X | $FCB3
       .byte $3A ; |  XXX X | $FCB4
       .byte $80 ; |X       | $FCB5
       .byte $FE ; |XXXXXXX | $FCB6
       .byte $00 ; |        | $FCB7
       .byte $80 ; |X       | $FCB8
       .byte $80 ; |X       | $FCB9
       .byte $AA ; |X X X X | $FCBA
       .byte $AA ; |X X X X | $FCBB
       .byte $BA ; |X XXX X | $FCBC
       .byte $22 ; |  X   X | $FCBD
       .byte $27 ; |  X  XXX| $FCBE
       .byte $02 ; |      X | $FCBF
LFCC0:
       .byte $E9 ; |XXX X  X| $FCC0
       .byte $AB ; |X X X XX| $FCC1
       .byte $AF ; |X X XXXX| $FCC2
       .byte $AD ; |X X XX X| $FCC3
       .byte $E9 ; |XXX X  X| $FCC4
       .byte $00 ; |        | $FCC5
       .byte $00 ; |        | $FCC6
       .byte $00 ; |        | $FCC7
       .byte $00 ; |        | $FCC8
       .byte $00 ; |        | $FCC9
       .byte $11 ; |   X   X| $FCCA
       .byte $11 ; |   X   X| $FCCB
       .byte $17 ; |   X XXX| $FCCC
       .byte $15 ; |   X X X| $FCCD
       .byte $17 ; |   X XXX| $FCCE
       .byte $00 ; |        | $FCCF
LFCD0:
       .byte $00 ; |        | $FCD0
       .byte $00 ; |        | $FCD1
       .byte $00 ; |        | $FCD2
       .byte $00 ; |        | $FCD3
       .byte $00 ; |        | $FCD4
       .byte $00 ; |        | $FCD5
       .byte $00 ; |        | $FCD6
       .byte $00 ; |        | $FCD7
       .byte $00 ; |        | $FCD8
       .byte $00 ; |        | $FCD9
       .byte $77 ; | XXX XXX| $FCDA
       .byte $54 ; | X X X  | $FCDB
       .byte $77 ; | XXX XXX| $FCDC
       .byte $51 ; | X X   X| $FCDD
       .byte $77 ; | XXX XXX| $FCDE
       .byte $00 ; |        | $FCDF
LFCE0:
       .byte $D2 ; |XX X  X | $FCE0
       .byte $96 ; |X  X XX | $FCE1
       .byte $DE ; |XX XXXX | $FCE2
       .byte $9A ; |X  XX X | $FCE3
       .byte $D2 ; |XX X  X | $FCE4
LFCE5:
       .byte $D5 ; |XX X X X| $FCE5
       .byte $99 ; |X  XX  X| $FCE6
       .byte $D5 ; |XX X X X| $FCE7
       .byte $95 ; |X  X X X| $FCE8
       .byte $D9 ; |XX XX  X| $FCE9
LFCEA:
       .byte $D0 ; |XX X    | $FCEA
       .byte $50 ; | X X    | $FCEB
       .byte $50 ; | X X    | $FCEC
       .byte $38 ; |  XXX   | $FCED
       .byte $A8 ; |X X X   | $FCEE
LFCEF:
       .byte $00 ; |        | $FCEF
       .byte $42 ; | X    X | $FCF0
       .byte $44 ; | X   X  | $FCF1
       .byte $1A ; |   XX X | $FCF2
       .byte $06 ; |     XX | $FCF3
LFCF4:
       .byte $58 ; | X XX   | $FCF4
       .byte $1A ; |   XX X | $FCF5
       .byte $C8 ; |XX  X   | $FCF6
       .byte $58 ; | X XX   | $FCF7
       .byte $C8 ; |XX  X   | $FCF8
       .byte $1A ; |   XX X | $FCF9
       .byte $36 ; |  XX XX | $FCFA
       .byte $1A ; |   XX X | $FCFB
       .byte $C8 ; |XX  X   | $FCFC
       .byte $36 ; |  XX XX | $FCFD
       .byte $58 ; | X XX   | $FCFE
       .byte $C8 ; |XX  X   | $FCFF
       .byte $1A ; |   XX X | $FD00
       .byte $58 ; | X XX   | $FD01
       .byte $C8 ; |XX  X   | $FD02
       .byte $1A ; |   XX X | $FD03
LFD04:
       .byte $3F ; |  XXXXXX| $FD04
       .byte $5F ; | X XXXXX| $FD05
       .byte $6F ; | XX XXXX| $FD06
LFD07:
       .byte $F3 ; |XXXX  XX| $FD07
       .byte $F5 ; |XXXX X X| $FD08
       .byte $F6 ; |XXXX XX | $FD09
LFD0A:
       .byte $F0 ; |XXXX    | $FD0A
       .byte $0F ; |    XXXX| $FD0B
LFD0C:
       .byte $00 ; |        | $FD0C
       .byte $F0 ; |XXXX    | $FD0D
       .byte $20 ; |  X     | $FD0E
       .byte $22 ; |  X   X | $FD0F
       .byte $37 ; |  XX XXX| $FD10
       .byte $7F ; | XXXXXXX| $FD11
       .byte $37 ; |  XX XXX| $FD12
       .byte $22 ; |  X   X | $FD13
       .byte $20 ; |  X     | $FD14
       .byte $F0 ; |XXXX    | $FD15
LFD16:
       .byte $01 ; |       X| $FD16
       .byte $01 ; |       X| $FD17
       .byte $03 ; |      XX| $FD18
       .byte $03 ; |      XX| $FD19
       .byte $07 ; |     XXX| $FD1A
       .byte $07 ; |     XXX| $FD1B
       .byte $17 ; |   X XXX| $FD1C
       .byte $17 ; |   X XXX| $FD1D
       .byte $37 ; |  XX XXX| $FD1E
       .byte $37 ; |  XX XXX| $FD1F
LFD20:
       .byte $00 ; |        | $FD20
       .byte $02 ; |      X | $FD21
       .byte $02 ; |      X | $FD22
       .byte $06 ; |     XX | $FD23
       .byte $06 ; |     XX | $FD24
       .byte $16 ; |   X XX | $FD25
       .byte $16 ; |   X XX | $FD26
       .byte $36 ; |  XX XX | $FD27
       .byte $36 ; |  XX XX | $FD28
       .byte $77 ; | XXX XXX| $FD29
LFD2A:
       .byte $00 ; |        | $FD2A
       .byte $40 ; | X      | $FD2B
       .byte $20 ; |  X     | $FD2C
       .byte $20 ; |  X     | $FD2D
       .byte $00 ; |        | $FD2E
LFD2F:
       .byte $00 ; |        | $FD2F
       .byte $00 ; |        | $FD30
       .byte $00 ; |        | $FD31
       .byte $02 ; |      X | $FD32
       .byte $00 ; |        | $FD33
       .byte $04 ; |     X  | $FD34
       .byte $02 ; |      X | $FD35
       .byte $06 ; |     XX | $FD36
LFD37:
       .byte $00 ; |        | $FD37
       .byte $60 ; | XX     | $FD38
LFD39:
       .byte $00 ; |        | $FD39
       .byte $10 ; |   X    | $FD3A
LFD3B:
       .byte $00 ; |        | $FD3B
       .byte $00 ; |        | $FD3C
       .byte $00 ; |        | $FD3D
       .byte $00 ; |        | $FD3E
LFD3F:
       .byte $00 ; |        | $FD3F
       .byte $01 ; |       X| $FD40
       .byte $03 ; |      XX| $FD41
       .byte $07 ; |     XXX| $FD42
       .byte $0F ; |    XXXX| $FD43
       .byte $1F ; |   XXXXX| $FD44
       .byte $3F ; |  XXXXXX| $FD45
       .byte $7F ; | XXXXXXX| $FD46
       .byte $FF ; |XXXXXXXX| $FD47
       .byte $FF ; |XXXXXXXX| $FD48
       .byte $FF ; |XXXXXXXX| $FD49
       .byte $FF ; |XXXXXXXX| $FD4A
       .byte $FF ; |XXXXXXXX| $FD4B
       .byte $FF ; |XXXXXXXX| $FD4C
       .byte $FF ; |XXXXXXXX| $FD4D
       .byte $FF ; |XXXXXXXX| $FD4E
       .byte $FF ; |XXXXXXXX| $FD4F
       .byte $FF ; |XXXXXXXX| $FD50
       .byte $FF ; |XXXXXXXX| $FD51
       .byte $FF ; |XXXXXXXX| $FD52
       .byte $FF ; |XXXXXXXX| $FD53
LFD54:
       .byte $00 ; |        | $FD54
       .byte $00 ; |        | $FD55
       .byte $00 ; |        | $FD56
       .byte $00 ; |        | $FD57
       .byte $00 ; |        | $FD58
       .byte $00 ; |        | $FD59
       .byte $00 ; |        | $FD5A
       .byte $00 ; |        | $FD5B
       .byte $00 ; |        | $FD5C
       .byte $00 ; |        | $FD5D
       .byte $00 ; |        | $FD5E
       .byte $00 ; |        | $FD5F
       .byte $00 ; |        | $FD60
       .byte $80 ; |X       | $FD61
       .byte $C0 ; |XX      | $FD62
       .byte $E0 ; |XXX     | $FD63
       .byte $F0 ; |XXXX    | $FD64
       .byte $F8 ; |XXXXX   | $FD65
       .byte $FC ; |XXXXXX  | $FD66
       .byte $FE ; |XXXXXXX | $FD67
       .byte $FF ; |XXXXXXXX| $FD68
LFD69:
       .byte $32 ; |  XX  X | $FD69
       .byte $29 ; |  X X  X| $FD6A
       .byte $1B ; |   XX XX| $FD6B
LFD6C:
       .byte $37 ; |  XX XXX| $FD6C
       .byte $77 ; | XXX XXX| $FD6D
       .byte $37 ; |  XX XXX| $FD6E
LFD6F:
       .byte $00 ; |        | $FD6F
       .byte $00 ; |        | $FD70
       .byte $00 ; |        | $FD71
       .byte $10 ; |   X    | $FD72
       .byte $11 ; |   X   X| $FD73
       .byte $31 ; |  XX   X| $FD74
       .byte $33 ; |  XX  XX| $FD75
LFD76:
       .byte $02 ; |      X | $FD76
       .byte $02 ; |      X | $FD77
       .byte $02 ; |      X | $FD78
       .byte $02 ; |      X | $FD79
       .byte $05 ; |     X X| $FD7A
       .byte $02 ; |      X | $FD7B
       .byte $05 ; |     X X| $FD7C
       .byte $0F ; |    XXXX| $FD7D
LFD7E:
       .byte $00 ; |        | $FD7E
       .byte $18 ; |   XX   | $FD7F
       .byte $3C ; |  XXXX  | $FD80
       .byte $7E ; | XXXXXX | $FD81
       .byte $FF ; |XXXXXXXX| $FD82
       .byte $FF ; |XXXXXXXX| $FD83
       .byte $FF ; |XXXXXXXX| $FD84
       .byte $FF ; |XXXXXXXX| $FD85
       .byte $FF ; |XXXXXXXX| $FD86
       .byte $FF ; |XXXXXXXX| $FD87
       .byte $FF ; |XXXXXXXX| $FD88
       .byte $FF ; |XXXXXXXX| $FD89
       .byte $7E ; | XXXXXX | $FD8A
       .byte $3C ; |  XXXX  | $FD8B
       .byte $18 ; |   XX   | $FD8C
       .byte $00 ; |        | $FD8D
       .byte $00 ; |        | $FD8E
       .byte $00 ; |        | $FD8F
       .byte $00 ; |        | $FD90
       .byte $00 ; |        | $FD91
LFD92:
       .byte $00 ; |        | $FD92
       .byte $00 ; |        | $FD93
       .byte $00 ; |        | $FD94
       .byte $00 ; |        | $FD95
       .byte $00 ; |        | $FD96
       .byte $00 ; |        | $FD97
       .byte $00 ; |        | $FD98
       .byte $00 ; |        | $FD99
       .byte $00 ; |        | $FD9A
       .byte $02 ; |      X | $FD9B
       .byte $02 ; |      X | $FD9C
       .byte $04 ; |     X  | $FD9D
       .byte $04 ; |     X  | $FD9E
       .byte $06 ; |     XX | $FD9F
       .byte $06 ; |     XX | $FDA0
       .byte $08 ; |    X   | $FDA1
       .byte $08 ; |    X   | $FDA2
       .byte $0A ; |    X X | $FDA3
       .byte $0A ; |    X X | $FDA4
       .byte $0C ; |    XX  | $FDA5
       .byte $0C ; |    XX  | $FDA6
       .byte $0E ; |    XXX | $FDA7
       .byte $8E ; |X   XXX | $FDA8
       .byte $0E ; |    XXX | $FDA9
       .byte $0E ; |    XXX | $FDAA
       .byte $8E ; |X   XXX | $FDAB
       .byte $0E ; |    XXX | $FDAC
       .byte $0C ; |    XX  | $FDAD
       .byte $0A ; |    X X | $FDAE
       .byte $08 ; |    X   | $FDAF
       .byte $06 ; |     XX | $FDB0
       .byte $04 ; |     X  | $FDB1
LFDB2:
       .byte $FF ; |XXXXXXXX| $FDB2
       .byte $FF ; |XXXXXXXX| $FDB3
       .byte $0F ; |    XXXX| $FDB4
       .byte $0F ; |    XXXX| $FDB5
       .byte $07 ; |     XXX| $FDB6
       .byte $07 ; |     XXX| $FDB7
       .byte $00 ; |        | $FDB8
       .byte $00 ; |        | $FDB9

LFDBA:
       .word LFE2B
       .word LFE00
       .word LFF09
       .word LFE9E
       .word LFDCA
       .word LFE49
       .word LFE74
       .word LFECD

LFDCA:
       .byte <LFDD0 ; $FDCA
       .byte <LFDD9 ; $FDCB
       .byte <LFDE2 ; $FDCC
       .byte <LFDEB ; $FDCD
       .byte <LFDE2 ; $FDCE
       .byte <LFDD9 ; $FDCF

LFDD0:
       .byte $00 ; |        | $FDD0
       .byte $10 ; |   X    | $FDD1
       .byte $38 ; |  XXX   | $FDD2
       .byte $7C ; | XXXXX  | $FDD3
       .byte $C6 ; |XX   XX | $FDD4
       .byte $C6 ; |XX   XX | $FDD5
       .byte $7C ; | XXXXX  | $FDD6
       .byte $38 ; |  XXX   | $FDD7
       .byte $10 ; |   X    | $FDD8
LFDD9:
       .byte $00 ; |        | $FDD9
       .byte $10 ; |   X    | $FDDA
       .byte $38 ; |  XXX   | $FDDB
       .byte $7C ; | XXXXX  | $FDDC
       .byte $6C ; | XX XX  | $FDDD
       .byte $6C ; | XX XX  | $FDDE
       .byte $7C ; | XXXXX  | $FDDF
       .byte $38 ; |  XXX   | $FDE0
       .byte $10 ; |   X    | $FDE1
LFDE2:
       .byte $00 ; |        | $FDE2
       .byte $10 ; |   X    | $FDE3
       .byte $38 ; |  XXX   | $FDE4
       .byte $38 ; |  XXX   | $FDE5
       .byte $28 ; |  X X   | $FDE6
       .byte $28 ; |  X X   | $FDE7
       .byte $38 ; |  XXX   | $FDE8
       .byte $38 ; |  XXX   | $FDE9
       .byte $10 ; |   X    | $FDEA
LFDEB:
       .byte $00 ; |        | $FDEB
       .byte $10 ; |   X    | $FDEC
       .byte $10 ; |   X    | $FDED
       .byte $10 ; |   X    | $FDEE
       .byte $10 ; |   X    | $FDEF
       .byte $10 ; |   X    | $FDF0
       .byte $10 ; |   X    | $FDF1
       .byte $10 ; |   X    | $FDF2
       .byte $10 ; |   X    | $FDF3
       .byte $00 ; |        | $FDF4
       .byte $00 ; |        | $FDF5
       .byte $00 ; |        | $FDF6
       .byte $00 ; |        | $FDF7
       .byte $00 ; |        | $FDF8
       .byte $00 ; |        | $FDF9
       .byte $00 ; |        | $FDFA
       .byte $00 ; |        | $FDFB
       .byte $00 ; |        | $FDFC
       .byte $00 ; |        | $FDFD
       .byte $00 ; |        | $FDFE
       .byte $00 ; |        | $FDFF

LFE00:
       .byte <LFE03 ; $FE00
       .byte <LFE0F ; $FE01
       .byte <LFE1B ; $FE02

LFE03:
       .byte $00 ; |        | $FE03
       .byte $00 ; |        | $FE04
       .byte $00 ; |        | $FE05
       .byte $00 ; |        | $FE06
       .byte $FF ; |XXXXXXXX| $FE07
       .byte $B6 ; |X XX XX | $FE08
       .byte $FF ; |XXXXXXXX| $FE09
       .byte $00 ; |        | $FE0A
       .byte $00 ; |        | $FE0B
       .byte $FF ; |XXXXXXXX| $FE0C
       .byte $6D ; | XX XX X| $FE0D
       .byte $FF ; |XXXXXXXX| $FE0E
LFE0F:
       .byte $00 ; |        | $FE0F
       .byte $00 ; |        | $FE10
       .byte $00 ; |        | $FE11
       .byte $00 ; |        | $FE12
       .byte $FF ; |XXXXXXXX| $FE13
       .byte $DB ; |XX XX XX| $FE14
       .byte $FF ; |XXXXXXXX| $FE15
       .byte $00 ; |        | $FE16
       .byte $00 ; |        | $FE17
       .byte $FF ; |XXXXXXXX| $FE18
       .byte $DB ; |XX XX XX| $FE19
       .byte $FF ; |XXXXXXXX| $FE1A
LFE1B:
       .byte $00 ; |        | $FE1B
       .byte $00 ; |        | $FE1C
       .byte $00 ; |        | $FE1D
       .byte $00 ; |        | $FE1E
       .byte $FF ; |XXXXXXXX| $FE1F
       .byte $6D ; | XX XX X| $FE20
       .byte $FF ; |XXXXXXXX| $FE21
       .byte $00 ; |        | $FE22
       .byte $00 ; |        | $FE23
       .byte $FF ; |XXXXXXXX| $FE24
       .byte $B6 ; |X XX XX | $FE25
       .byte $FF ; |XXXXXXXX| $FE26
       .byte $00 ; |        | $FE27
       .byte $00 ; |        | $FE28
       .byte $00 ; |        | $FE29
       .byte $00 ; |        | $FE2A

LFE2B:
       .byte <LFE2E ; $FE2B
       .byte <LFE37 ; $FE2C
       .byte <LFE40 ; $FE2D

LFE2E:
       .byte $00 ; |        | $FE2E
       .byte $3C ; |  XXXX  | $FE2F
       .byte $FF ; |XXXXXXXX| $FE30
       .byte $00 ; |        | $FE31
       .byte $B6 ; |X XX XX | $FE32
       .byte $B6 ; |X XX XX | $FE33
       .byte $00 ; |        | $FE34
       .byte $FF ; |XXXXXXXX| $FE35
       .byte $3C ; |  XXXX  | $FE36
LFE37:
       .byte $00 ; |        | $FE37
       .byte $3C ; |  XXXX  | $FE38
       .byte $FF ; |XXXXXXXX| $FE39
       .byte $00 ; |        | $FE3A
       .byte $DB ; |XX XX XX| $FE3B
       .byte $DB ; |XX XX XX| $FE3C
       .byte $00 ; |        | $FE3D
       .byte $FF ; |XXXXXXXX| $FE3E
       .byte $3C ; |  XXXX  | $FE3F
LFE40:
       .byte $00 ; |        | $FE40
       .byte $3C ; |  XXXX  | $FE41
       .byte $FF ; |XXXXXXXX| $FE42
       .byte $00 ; |        | $FE43
       .byte $6D ; | XX XX X| $FE44
       .byte $6D ; | XX XX X| $FE45
       .byte $00 ; |        | $FE46
       .byte $FF ; |XXXXXXXX| $FE47
       .byte $3C ; |  XXXX  | $FE48

LFE49:
       .byte <LFE4C ; $FE49
       .byte <LFE58 ; $FE4A
       .byte <LFE64 ; $FE4B

LFE4C:
       .byte $00 ; |        | $FE4C
       .byte $00 ; |        | $FE4D
       .byte $00 ; |        | $FE4E
       .byte $00 ; |        | $FE4F
       .byte $FF ; |XXXXXXXX| $FE50
       .byte $B6 ; |X XX XX | $FE51
       .byte $FF ; |XXXXXXXX| $FE52
       .byte $7E ; | XXXXXX | $FE53
       .byte $66 ; | XX  XX | $FE54
       .byte $7E ; | XXXXXX | $FE55
       .byte $42 ; | X    X | $FE56
       .byte $42 ; | X    X | $FE57
LFE58:
       .byte $00 ; |        | $FE58
       .byte $00 ; |        | $FE59
       .byte $00 ; |        | $FE5A
       .byte $00 ; |        | $FE5B
       .byte $FF ; |XXXXXXXX| $FE5C
       .byte $DB ; |XX XX XX| $FE5D
       .byte $FF ; |XXXXXXXX| $FE5E
       .byte $7E ; | XXXXXX | $FE5F
       .byte $66 ; | XX  XX | $FE60
       .byte $7E ; | XXXXXX | $FE61
       .byte $42 ; | X    X | $FE62
       .byte $42 ; | X    X | $FE63
LFE64:
       .byte $00 ; |        | $FE64
       .byte $00 ; |        | $FE65
       .byte $00 ; |        | $FE66
       .byte $00 ; |        | $FE67
       .byte $FF ; |XXXXXXXX| $FE68
       .byte $6D ; | XX XX X| $FE69
       .byte $FF ; |XXXXXXXX| $FE6A
       .byte $7E ; | XXXXXX | $FE6B
       .byte $66 ; | XX  XX | $FE6C
       .byte $7E ; | XXXXXX | $FE6D
       .byte $42 ; | X    X | $FE6E
       .byte $42 ; | X    X | $FE6F
       .byte $00 ; |        | $FE70
       .byte $00 ; |        | $FE71
       .byte $00 ; |        | $FE72
       .byte $00 ; |        | $FE73

LFE74:
       .byte <LFE7A ; $FE74
       .byte <LFE83 ; $FE75
       .byte <LFE8C ; $FE76
       .byte <LFE95 ; $FE77
       .byte <LFE8C ; $FE78
       .byte <LFE83 ; $FE79

LFE7A:
       .byte $00 ; |        | $FE7A
       .byte $82 ; |X     X | $FE7B
       .byte $C6 ; |XX   XX | $FE7C
       .byte $EE ; |XXX XXX | $FE7D
       .byte $FE ; |XXXXXXX | $FE7E
       .byte $FE ; |XXXXXXX | $FE7F
       .byte $EE ; |XXX XXX | $FE80
       .byte $C6 ; |XX   XX | $FE81
       .byte $82 ; |X     X | $FE82
LFE83:
       .byte $00 ; |        | $FE83
       .byte $44 ; | X   X  | $FE84
       .byte $44 ; | X   X  | $FE85
       .byte $6C ; | XX XX  | $FE86
       .byte $7C ; | XXXXX  | $FE87
       .byte $7C ; | XXXXX  | $FE88
       .byte $6C ; | XX XX  | $FE89
       .byte $44 ; | X   X  | $FE8A
       .byte $44 ; | X   X  | $FE8B
LFE8C:
       .byte $00 ; |        | $FE8C
       .byte $28 ; |  X X   | $FE8D
       .byte $28 ; |  X X   | $FE8E
       .byte $28 ; |  X X   | $FE8F
       .byte $38 ; |  XXX   | $FE90
       .byte $38 ; |  XXX   | $FE91
       .byte $28 ; |  X X   | $FE92
       .byte $28 ; |  X X   | $FE93
       .byte $28 ; |  X X   | $FE94
LFE95:
       .byte $00 ; |        | $FE95
       .byte $10 ; |   X    | $FE96
       .byte $10 ; |   X    | $FE97
       .byte $10 ; |   X    | $FE98
       .byte $10 ; |   X    | $FE99
       .byte $10 ; |   X    | $FE9A
       .byte $10 ; |   X    | $FE9B
       .byte $10 ; |   X    | $FE9C
       .byte $10 ; |   X    | $FE9D

LFE9E:
       .byte <LFEA1 ; $FE9E
       .byte <LFEAE ; $FE9F
       .byte <LFEBB ; $FEA0

LFEA1:
       .byte $00 ; |        | $FEA1
       .byte $00 ; |        | $FEA2
       .byte $00 ; |        | $FEA3
       .byte $00 ; |        | $FEA4
       .byte $42 ; | X    X | $FEA5
       .byte $FF ; |XXXXXXXX| $FEA6
       .byte $49 ; | X  X  X| $FEA7
       .byte $FF ; |XXXXXXXX| $FEA8
       .byte $6D ; | XX XX X| $FEA9
       .byte $FF ; |XXXXXXXX| $FEAA
       .byte $49 ; | X  X  X| $FEAB
       .byte $FF ; |XXXXXXXX| $FEAC
       .byte $42 ; | X    X | $FEAD
LFEAE:
       .byte $00 ; |        | $FEAE
       .byte $00 ; |        | $FEAF
       .byte $00 ; |        | $FEB0
       .byte $00 ; |        | $FEB1
       .byte $42 ; | X    X | $FEB2
       .byte $FF ; |XXXXXXXX| $FEB3
       .byte $24 ; |  X  X  | $FEB4
       .byte $FF ; |XXXXXXXX| $FEB5
       .byte $DB ; |XX XX XX| $FEB6
       .byte $FF ; |XXXXXXXX| $FEB7
       .byte $24 ; |  X  X  | $FEB8
       .byte $FF ; |XXXXXXXX| $FEB9
       .byte $42 ; | X    X | $FEBA
LFEBB:
       .byte $00 ; |        | $FEBB
       .byte $00 ; |        | $FEBC
       .byte $00 ; |        | $FEBD
       .byte $00 ; |        | $FEBE
       .byte $42 ; | X    X | $FEBF
       .byte $FF ; |XXXXXXXX| $FEC0
       .byte $92 ; |X  X  X | $FEC1
       .byte $FF ; |XXXXXXXX| $FEC2
       .byte $B6 ; |X XX XX | $FEC3
       .byte $FF ; |XXXXXXXX| $FEC4
       .byte $92 ; |X  X  X | $FEC5
       .byte $FF ; |XXXXXXXX| $FEC6
       .byte $42 ; | X    X | $FEC7
       .byte $00 ; |        | $FEC8
       .byte $00 ; |        | $FEC9
       .byte $00 ; |        | $FECA
       .byte $00 ; |        | $FECB
       .byte $00 ; |        | $FECC
LFECD:
       .byte $EC ; |XXX XX  | $FECD
       .byte $EB ; |XXX X XX| $FECE
       .byte $EA ; |XXX X X | $FECF
       .byte $E9 ; |XXX X  X| $FED0
       .byte $E8 ; |XXX X   | $FED1
       .byte $E7 ; |XXX  XXX| $FED2
       .byte $E6 ; |XXX  XX | $FED3
       .byte $E5 ; |XXX  X X| $FED4
       .byte $E4 ; |XXX  X  | $FED5
       .byte $E3 ; |XXX   XX| $FED6
       .byte $E2 ; |XXX   X | $FED7
       .byte $E1 ; |XXX    X| $FED8
       .byte $E0 ; |XXX     | $FED9
       .byte $DF ; |XX XXXXX| $FEDA
       .byte $DE ; |XX XXXX | $FEDB
       .byte $DD ; |XX XXX X| $FEDC
       .byte $00 ; |        | $FEDD
       .byte $7C ; | XXXXX  | $FEDE
       .byte $7C ; | XXXXX  | $FEDF
       .byte $FE ; |XXXXXXX | $FEE0
       .byte $FE ; |XXXXXXX | $FEE1
       .byte $4F ; | X  XXXX| $FEE2
       .byte $4F ; | X  XXXX| $FEE3
       .byte $FE ; |XXXXXXX | $FEE4
       .byte $FE ; |XXXXXXX | $FEE5
       .byte $7C ; | XXXXX  | $FEE6
       .byte $7C ; | XXXXX  | $FEE7
       .byte $3E ; |  XXXXX | $FEE8
       .byte $3E ; |  XXXXX | $FEE9
       .byte $73 ; | XXX  XX| $FEEA
       .byte $73 ; | XXX  XX| $FEEB
       .byte $FE ; |XXXXXXX | $FEEC
       .byte $FE ; |XXXXXXX | $FEED
       .byte $7C ; | XXXXX  | $FEEE
       .byte $7C ; | XXXXX  | $FEEF
       .byte $FE ; |XXXXXXX | $FEF0
       .byte $FE ; |XXXXXXX | $FEF1
       .byte $4F ; | X  XXXX| $FEF2
       .byte $4F ; | X  XXXX| $FEF3
       .byte $FE ; |XXXXXXX | $FEF4
       .byte $FE ; |XXXXXXX | $FEF5
       .byte $7C ; | XXXXX  | $FEF6
       .byte $7C ; | XXXXX  | $FEF7
       .byte $3E ; |  XXXXX | $FEF8
       .byte $3E ; |  XXXXX | $FEF9
       .byte $73 ; | XXX  XX| $FEFA
       .byte $73 ; | XXX  XX| $FEFB
       .byte $FE ; |XXXXXXX | $FEFC
       .byte $FE ; |XXXXXXX | $FEFD
LFEFE:
       .byte $A5 ; |X X  X X| $FEFE
       .byte $82 ; |X     X | $FEFF
       .byte $0A ; |    X X | $FF00
       .byte $0A ; |    X X | $FF01
       .byte $0A ; |    X X | $FF02
       .byte $45 ; | X   X X| $FF03
       .byte $82 ; |X     X | $FF04
       .byte $0A ; |    X X | $FF05
       .byte $26 ; |  X  XX | $FF06
       .byte $82 ; |X     X | $FF07
       .byte $60 ; | XX     | $FF08

LFF09:
       .byte <LFF0C ; $FF09
       .byte <LFF15 ; $FF0A
       .byte <LFF1E ; $FF0B

LFF0C:
       .byte $00 ; |        | $FF0C
       .byte $36 ; |  XX XX | $FF0D
       .byte $36 ; |  XX XX | $FF0E
       .byte $00 ; |        | $FF0F
       .byte $FF ; |XXXXXXXX| $FF10
       .byte $FF ; |XXXXXXXX| $FF11
       .byte $00 ; |        | $FF12
       .byte $6C ; | XX XX  | $FF13
       .byte $6C ; | XX XX  | $FF14
LFF15:
       .byte $00 ; |        | $FF15
       .byte $5A ; | X XX X | $FF16
       .byte $5A ; | X XX X | $FF17
       .byte $00 ; |        | $FF18
       .byte $FF ; |XXXXXXXX| $FF19
       .byte $FF ; |XXXXXXXX| $FF1A
       .byte $00 ; |        | $FF1B
       .byte $5A ; | X XX X | $FF1C
       .byte $5A ; | X XX X | $FF1D
LFF1E:
       .byte $00 ; |        | $FF1E
       .byte $6C ; | XX XX  | $FF1F
       .byte $6C ; | XX XX  | $FF20
       .byte $00 ; |        | $FF21
       .byte $FF ; |XXXXXXXX| $FF22
       .byte $FF ; |XXXXXXXX| $FF23
       .byte $00 ; |        | $FF24
       .byte $36 ; |  XX XX | $FF25
       .byte $36 ; |  XX XX | $FF26

LFF27:
       ldx    #$0B                    ;2
       lda    #>DigitGFX              ;2
LFF2B:
       sta    $8D,X                   ;4
       dex                            ;2
       dex                            ;2
       bpl    LFF2B                   ;2
       lda    #<LFE03                 ;2
       sta    $AF                     ;3
       lda    #>LFE03                 ;2
       sta    $B0                     ;3
       lda    #$AA                    ;2
       sta    $A8                     ;3
       lda    #$F0                    ;2
       sta    $EC                     ;3
       sta    $ED                     ;3
       lda    #$06                    ;2
       sta    $F7                     ;3
       sta    $D9                     ;3
       lda    #$50                    ;2
       sta    $D6                     ;3
       lda    #$40                    ;2
       sta    $A0                     ;3
       lda    #$90                    ;2
       sta    $A1                     ;3
       ldx    #$05                    ;2
LFF57:
       lda    LFD6C,X                 ;4
       sta    $B1,X                   ;4
       sta    $B7,X                   ;4
       dex                            ;2
       bpl    LFF57                   ;2
       inx                            ;2
       stx    AUDV0                   ;3
       stx    AUDV1                   ;3
       lda    #$16                    ;2
       sta    $A9                     ;3
       ldx    #$03                    ;2
       stx    $9D                     ;3
       inx                            ;2
       lda    $80                     ;3
       lsr                            ;2
       bcs    LFF76                   ;2
       ldx    #$00                    ;2
LFF76:
       stx    $9E                     ;3
       rts                            ;6

LFF79:
       jsr    LFFA0                   ;6
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       rts                            ;6

LFF81:
       clc                            ;2
       adc    #$2E                    ;2
       tay                            ;2
       and    #$0F                    ;2
       sta    $A2                     ;3
       tya                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       tay                            ;2
       clc                            ;2
       adc    $A2                     ;3
       cmp    #$0F                    ;2
       bcc    LFF99                   ;2
       sbc    #$0F                    ;2
       iny                            ;2
LFF99:
       eor    #$07                    ;2
       asl                            ;2
LFF9C:
       asl                            ;2
LFF9D:
       asl                            ;2
LFF9E:
       asl                            ;2
LFF9F:
       rts                            ;6

LFFA0:
       jsr    LFF81                   ;6
       sta    HMP0,X                  ;4
       sta    WSYNC                   ;3
LFFA7:
       dey                            ;2
       bpl    LFFA7                   ;2
       sta    RESP0,X                 ;4
       rts                            ;6

LFFAD:
       ldx    #$08                    ;2
       lda    $DE                     ;3
       cmp    #$08                    ;2
       bcs    LFFB6                   ;2
       tax                            ;2
LFFB6:
       ldy    LFFC5,X                 ;4
       lda    $D9                     ;3
       lsr                            ;2
       lsr                            ;2
       bcc    LFFC2                   ;2
       ldy    LFFC4,X                 ;4
LFFC2:
       sty    $E0                     ;3
LFFC4:
       rts                            ;6

  IF ENDGAME
LFFC5:
       .byte $20 ; |  X     | $FFC5
       .byte $30 ; |  XX    | $FFC6
       .byte $40 ; | X      | $FFC7
       .byte $50 ; | X X    | $FFC8
       .byte $60 ; | XX     | $FFC9
       .byte $70 ; | XXX    | $FFCA
       .byte $80 ; |X       | $FFCB
       .byte $90 ; |X  X    | $FFCC
       .byte $90 ; |X  X    | $FFCD
  ENDIF

LFFCE:
       jsr    LFBE4                   ;6
       ldx    $D7                     ;3
       lda    $9D,X                   ;4
       bne    LFFDA                   ;2
       jsr    LFBE4                   ;6
LFFDA:
       ldx    $D7                     ;3
       dec    $9D,X                   ;6
       rts                            ;6

LFFDF:
       cpx    #$FF                    ;2
       bne    LFFE5                   ;2
       ldx    #$9F                    ;2
LFFE5:
       cpx    #$A0                    ;2
       bcc    LFFEB                   ;2
       ldx    #$00                    ;2
LFFEB:
       stx    $F1,Y                   ;4
       rts                            ;6

LFFEE:
       lda    LFDBA+1,Y               ;4
       sta    $A3                     ;3
       sta    $B0                     ;3
       ldy    $D5                     ;3
       lda    ($A2),Y                 ;5
       sta    $AF                     ;3
       rts                            ;6

  IF ENDGAME = 0
       .byte "N.S.2007"
  ENDIF

       ORG $FFFC
       .word START,START
