;Disassembly of Thunderground, by Kurt (Nukey Shay) Howe

;Added Supercharger compatability
;Fixed scanline count @262
;Added "Alien Ants" edits
;Alien Ants bug corrected
;Fixed new game jitter
;HMOVE lines hidden

;Todo: correct game reset jitter

ORIGINAL = 1 ;set to 0 for Alien Ants hack


; Disassembly of T_Ground.bin
; Disassembled Sat Mar 01 16:38:33 2008
; Using DiStella v3.0
; Command Line: C:\BIN\D3.EXE -pafscT_Ground.cfg T_Ground.bin 
; T_Ground.cfg contents:
;      ORG F000
;      CODE F000 FAF5
;      GFX FAF6 FBFF
;      CODE FC00 FCA9
;      GFX FCAA FD98
;      CODE FD99 FDFF
;      GFX FE00 FE0D
;      CODE FE0E FEA8
;      GFX FEA9 FF5E
;      CODE FF5F FFF8
;      GFX FFF9 FFFF


      processor 6502

VSYNC   =  $00
VBLANK  =  $01
WSYNC   =  $02
NUSIZ0  =  $04
NUSIZ1  =  $05
COLUP0  =  $06
COLUP1  =  $07
COLUPF  =  $08
COLUBK  =  $09
CTRLPF  =  $0A
PF0     =  $0D
PF1     =  $0E
PF2     =  $0F
RESP0   =  $10
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
HMP0    =  $20
VDELP0  =  $25
VDELP1  =  $26
HMOVE   =  $2A
HMCLR   =  $2B
CXCLR   =  $2C
CXM1P   =  $31
CXM0FB  =  $34
CXM1FB  =  $35
CXPPMM  =  $37
INPT4   =  $3C
SWCHA   =  $0280
SWCHB   =  $0282
INTIM   =  $0284
TIM8T   =  $0295
TIM64T  =  $0296

       ORG $F000

LFF5F: ;26
       ldy    #$02                    ;2
       sec                            ;2
LFF62:
       iny                            ;2
       sbc    #$0F                    ;2
       bcs    LFF62                   ;2
       eor    #$FF                    ;2
       sbc    #$06                    ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       sta    HMP0,X                  ;4
       sta    WSYNC                   ;3
LFF73:
       dey                            ;2
       bpl    LFF73                   ;2
       sta    RESP0,X                 ;4
       rts                            ;6



LFF79: ;22
       ldx    #$08                    ;2
LFF7B:
       clc                            ;2
       adc    $EA,X                   ;4
       sta    $EA,X                   ;4
       cmp    #<(LFD00+$5A)           ;2
       bcc    LFF8E                   ;2
       sbc    #<(LFD00+$5A)           ;2
       sta    $EA,X                   ;4
       lda    #$09                    ;2
       dex                            ;2
       dex                            ;2
       bpl    LFF7B                   ;2
LFF8E:
       rts                            ;6





LFFCD: ;17
       sta    $FA                     ;3
       and    #$03                    ;2
       cmp    #$03                    ;2
       bne    LFFDD                   ;2
       lda    $FA                     ;3
       cmp    $E2                     ;3
       bne    LFFDD                   ;2
       sta    $E3,X                   ;4
LFFDD:
       rts                            ;6



LFFDE: ;27
       sta    $FA                     ;3
       and    #$03                    ;2
       cmp    #$01                    ;2
       bne    LFFF3                   ;2
       lda    $FA                     ;3
       adc    #$02                    ;2
       cmp    $E5                     ;3
       bcc    LFFF3                   ;2
       sbc    #$05                    ;2
LFFF0:
       cmp    $E5                     ;3
       rts                            ;6
LFFF3:
       lda    #$FF                    ;2
       bne    LFFF0                   ;2
       sec                            ;2
       rts                            ;6



LF00D:
       lda    $83                     ;3
       bne    LF014                   ;2
       jmp    LFA25                   ;3

LF014:
       lda    #$E0                    ;2
       clc                            ;2
       adc    $C6                     ;3
       sta    COLUBK                  ;3
       ldx    $C8                     ;3
       lda    LFCFC,X                 ;4
       sta    COLUPF                  ;3
       lda    #$05                    ;2
       sta    CTRLPF                  ;3
       lda    #$FF                    ;2
       sta    PF0                     ;3
       sta    PF1                     ;3
       sta    PF2                     ;3
       cmp    $DA                     ;3
       bne    LF036                   ;2
       lda    #$1E                    ;2
       sta    $84                     ;3
LF036:
       lda    $F8                     ;3
       beq    LF04A                   ;2
       lda    $F9                     ;3
       cmp    #$02                    ;2
       bcc    LF083                   ;2
       bne    LF046                   ;2
       lda    $F8                     ;3
       sta    $E0                     ;3
LF046:
       dec    $F9                     ;5
       bne    LF08D                   ;2
LF04A:
       ldx    $F9                     ;3
       lda    LFD91,X                 ;4
       cmp    $E5                     ;3
       bcs    LF083                   ;2
       adc    #$14                    ;2
       cmp    $E5                     ;3
       bcc    LF083                   ;2
       lda    LFD95,X                 ;4
       cmp    $E2                     ;3
       bcs    LF083                   ;2
       adc    #$14                    ;2
       cmp    $E2                     ;3
       bcc    LF083                   ;2
       lda    $E0                     ;3
       sta    $F8                     ;3
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       tax                            ;2
       lda    LFFF9-4,X               ;4
       sta    $E0                     ;3
       lda    LFF56,X                 ;4
       ldx    #$04                    ;2
       jsr    LFF7B                   ;6
       lda    #$2F                    ;2
       sta    $F9                     ;3
       sta    $8B                     ;3
       bne    LF08D                   ;2 always branch

LF083:
       lda    $80                     ;3
       bne    LF090                   ;2
       ldy    $DC                     ;3
       beq    LF08D                   ;2
       sta    $E7                     ;3
LF08D:
       jmp    LF260                   ;3

LF090:
       lda    $DB                     ;3
       beq    LF0A6                   ;2
       ldx    $DC                     ;3
       beq    LF0A6                   ;2
       ldy    #$01                    ;2
       cpx    #$E1                    ;2
       bcs    LF0A0                   ;2
       sty    $DC                     ;3
LF0A0:
       cmp    #$E1                    ;2
       bcs    LF0A6                   ;2
       sty    $DB                     ;3
LF0A6:
       dec    $85                     ;5
       ldy    #$00                    ;2
       lda    $DA                     ;3
       beq    LF0C3                   ;2
       dec    $DA                     ;5
       cmp    #$F0                    ;2
       bcs    LF0B6                   ;2
       ldy    #$08                    ;2
LF0B6:
       sty    $E0                     ;3
       cmp    #$D8                    ;2
       bne    LF0DC                   ;2
       dec    $83                     ;5
       jsr    LF5DE                   ;6
       bne    LF0ED                   ;2 always branch

LF0C3:
       lda    $E5                     ;3
       and    #$03                    ;2
       cmp    #$01                    ;2
       bne    LF0DC                   ;2
       lda    $E2                     ;3
       and    #$03                    ;2
       cmp    #$03                    ;2
       bne    LF0DC                   ;2
       ldx    $8C                     ;3
       lda    LFCB4,X                 ;4
       beq    LF0DC                   ;2
       sta    $E0                     ;3
LF0DC:
       lda    $85                     ;3
       and    #$03                    ;2
       beq    LF0F0                   ;2
       cmp    #$03                    ;2
       beq    LF0F0                   ;2
       tax                            ;2
       jsr    LF627                   ;6
       jsr    LF676                   ;6
LF0ED:
       jmp    LF1B7                   ;3

LF0F0:
       lda    $85                     ;3
       bne    LF108                   ;2
       lda    #$30                    ;2
       sta    $85                     ;3
       lda    $DA                     ;3
       bne    LF0ED                   ;2
       dec    $89                     ;5
       lda    $89                     ;3
       cmp    #$3C                    ;2
       bcs    LF108                   ;2
       lda    #$2F                    ;2
       sta    $8B                     ;3
LF108:
       lda    $DA                     ;3
       bmi    LF0ED                   ;2
       lda    $E5                     ;3
       and    #$03                    ;2
       cmp    #$01                    ;2
       bne    LF122                   ;2
       lda    $E2                     ;3
       and    #$03                    ;2
       cmp    #$03                    ;2
       bne    LF122                   ;2
       lda    $8C                     ;3
       cmp    #$0F                    ;2
       beq    LF0ED                   ;2
LF122:
       lda    $E0                     ;3
       cmp    #$18                    ;2
       bne    LF14B                   ;2
       lda    $E5                     ;3
       clc                            ;2
       adc    #$01                    ;2
       cmp    #$8A                    ;2
       bcc    LF133                   ;2
       lda    #$89                    ;2
LF133:
       sta    $E5                     ;3
       clc                            ;2
       adc    #$06                    ;2
       ldy    $E2                     ;3
       jsr    LFD99                   ;6
       lda    $E2                     ;3
       sec                            ;2
       sbc    #$07                    ;2
       tay                            ;2
       lda    $E5                     ;3
       clc                            ;2
       adc    #$06                    ;2
       jsr    LFD99                   ;6
LF14B:
       cmp    #$10                    ;2
       bne    LF16B                   ;2
       ldy    $E2                     ;3
       lda    $E5                     ;3
       sbc    #$01                    ;2
       cmp    #$11                    ;2
       bcs    LF15B                   ;2
       lda    #$11                    ;2
LF15B:
       sta    $E5                     ;3
       jsr    LFD99                   ;6
       lda    $E2                     ;3
       sec                            ;2
       sbc    #$07                    ;2
       tay                            ;2
       lda    $E5                     ;3
       jsr    LFD99                   ;6
LF16B:
       cmp    #$20                    ;2
       bne    LF18E                   ;2
       lda    $E2                     ;3
       clc                            ;2
       adc    #$01                    ;2
       cmp    #$38                    ;2
       bcc    LF17A                   ;2
       lda    #$37                    ;2
LF17A:
       sta    $E2                     ;3
       tay                            ;2
       dey                            ;2
       lda    $E5                     ;3
       jsr    LFD99                   ;6
       ldy    $E2                     ;3
       dey                            ;2
       lda    $E5                     ;3
       clc                            ;2
       adc    #$03                    ;2
       jsr    LFD99                   ;6
LF18E:
       cmp    #$28                    ;2
       bne    LF1B7                   ;2
       lda    $E2                     ;3
       sbc    #$01                    ;2
       cmp    #$07                    ;2
       bcs    LF19C                   ;2
       lda    #$07                    ;2
LF19C:
       sta    $E2                     ;3
       sec                            ;2
       sbc    #$07                    ;2
       tay                            ;2
       lda    $E5                     ;3
       sbc    #$01                    ;2
       jsr    LFD99                   ;6
       lda    $E2                     ;3
       sec                            ;2
       sbc    #$07                    ;2
       tay                            ;2
       lda    $E5                     ;3
       clc                            ;2
       adc    #$03                    ;2
       jsr    LFD99                   ;6
LF1B7:
       lda    $D7                     ;3
       beq    LF207                   ;2
       dec    $D7                     ;5
       lda    $DA                     ;3
       bmi    LF207                   ;2
       lda    $C9                     ;3
       eor    #$80                    ;2
       sta    $C9                     ;3
       bpl    LF1CF                   ;2
       ldx    #$01                    ;2
       lda    $D0                     ;3
       bmi    LF1DA                   ;2
LF1CF:
       ldx    #$00                    ;2
       lda    $CF                     ;3
       bmi    LF1DA                   ;2
       inx                            ;2
       lda    $D0                     ;3
       bpl    LF207                   ;2
LF1DA:
       lda    #$F0                    ;2
       sta    $82                     ;3
       ldy    $8C                     ;3
       lda    LFCB4,Y                 ;4
       tay                            ;2
       bne    LF1E8                   ;2
       ldy    $E0                     ;3
LF1E8:
       lda    $C9                     ;3
       cpy    #$10                    ;2
       bne    LF20A                   ;2
       and    LFAFB,X                 ;4
       sta    $C9                     ;3
       lda    $E5                     ;3
       clc                            ;2
       adc    #$04                    ;2
LF1F8:
       sta    $CB,X                   ;4
       lda    $E2                     ;3
       sec                            ;2
       sbc    #$02                    ;2
       cpx    #$01                    ;2
       bne    LF205                   ;2
       sbc    #$02                    ;2
LF205:
       sta    $CF,X                   ;4
LF207:
       jmp    LF24A                   ;3

LF20A:
       cpy    #$18                    ;2
       bne    LF21C                   ;2
       and    LFAFB,X                 ;4
       ora    LFE08,X                 ;4
       sta    $C9                     ;3
       lda    $E5                     ;3
       adc    #$04                    ;2
       bne    LF1F8                   ;2 always branch?
LF21C:
       cpy    #$20                    ;2
       bne    LF22E                   ;2
       and    LFAFB,X                 ;4
       ora    LFE0A,X                 ;4
       sta    $C9                     ;3
       lda    $E2                     ;3
       sbc    #$01                    ;2
       bne    LF23A                   ;2
LF22E:
       and    LFAFB,X                 ;4
       ora    LFE0C,X                 ;4
       sta    $C9                     ;3
       lda    $E2                     ;3
       sbc    #$05                    ;2
LF23A:
       sta    $CF,X                   ;4
       lda    $E5                     ;3
       clc                            ;2
       adc    #$03                    ;2
       cpx    #$01                    ;2
       bne    LF248                   ;2
       clc                            ;2
       adc    #$02                    ;2
LF248:
       sta    $CB,X                   ;4
LF24A:
       lda    $C9                     ;3
       ldx    $C5                     ;3
       beq    LF252                   ;2
       lsr                            ;2
       lsr                            ;2
LF252:
       ldy    $DA                     ;3
       bne    LF260                   ;2
       ldy    $CF,X                   ;4
       bmi    LF25D                   ;2
       jsr    LF955                   ;6
LF25D:
       jsr    LF87E                   ;6
LF260:
       ldx    #$01                    ;2
LF262:
       ldy    $D8,X                   ;4
       lda    LFE04,Y                 ;4
       sec                            ;2
       sbc    $E3,X                   ;4
       sta    $DE,X                   ;4
       dex                            ;2
       beq    LF262                   ;2
       ldx    $C5                     ;3
       lda    $DB,X                   ;4
       bne    LF287                   ;2
       lda    #$0F                    ;2
       ldy    $D3                     ;3
       beq    LF285                   ;2
       cpy    #$02                    ;2
       bcs    LF283                   ;2
       cpx    #$00                    ;2
       beq    LF285                   ;2
LF283:
       lda    #$96                    ;2
LF285:
       sta    $D5,X                   ;4
LF287:
       lda    #$08                    ;2
       sta    $DD                     ;3
       lda    $81                     ;3
       bne    LF297                   ;2
       lda    $86                     ;3
       ora    #$04                    ;2
       dec    $86                     ;5
       sta    COLUP1                  ;3
LF297:
       lda    $80                     ;3
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       tax                            ;2
       lda    LFF4E,X                 ;4
       sta    NUSIZ0                  ;3
       lda    LFD84,X                 ;4
       ldx    #$00                    ;2
       jsr    LFF5F                   ;6
       lda    $80                     ;3
       and    #$07                    ;2
       tax                            ;2
       lda    LFF4E,X                 ;4
       sta    NUSIZ1                  ;3
       lda    LFD7C,X                 ;4
       ldx    #$01                    ;2
       jsr    LFF5F                   ;6
       ldx    $C5                     ;3
       lda    $CB,X                   ;4
       ldx    #$02                    ;2
       jsr    LFF5F                   ;6
       ldx    $C5                     ;3
       lda    $CD,X                   ;4
       ldx    #$03                    ;2
       jsr    LFF5F                   ;6
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
LF2D3:
       lda    INTIM                   ;4
       bne    LF2D3                   ;2
       sta    HMCLR                   ;3
       sta    WSYNC                   ;3
       sta    VBLANK                  ;3
       ldy    #$08                    ;2 scanline correction
       jsr    LFDFA                   ;6
       lda    #$F9                    ;2
       sta    PF1                     ;3
       lda    #$99                    ;2
       sta    PF2                     ;3
       ldy    #$08                    ;2
LF2ED:
       lda    $85                     ;3
       adc    $F9                     ;3
       and    #$07                    ;2
       cmp    #$04                    ;2
       lda    LFCDC,Y                 ;4
       bcc    LF2FF                   ;2
       ldx    LFCE5,Y                 ;4
       bcs    LF305                   ;2 always branch

LF2FF:
       tax                            ;2
       lda    LFCE5,Y                 ;4
LF305:
       sta    WSYNC                   ;3
       sta    GRP0                    ;3
       stx    GRP1                    ;3
       dey                            ;2
       bpl    LF2ED                   ;2
       stx    NUSIZ0                  ;3
       stx    NUSIZ1                  ;3
       ldy    #$24                    ;2
       lda    $F8                     ;3
       beq    LF31A                   ;2
       ldy    #$98                    ;2
LF31A:
       sty    COLUP0                  ;3
       ldx    $C5                     ;3
       lda    $D5,X                   ;4
       sta    COLUP1                  ;3
       lda    #$38                    ;2
       sta    TIM8T                   ;4
       lda    $E5                     ;3
       ldx    #$00                    ;2
       jsr    LFF5F                   ;6
       lda    $E7                     ;3
       ldy    $C5                     ;3
       bne    LF336                   ;2
       lda    $E6                     ;3
LF336:
       ldx    #$01                    ;2
       jsr    LFF5F                   ;6
       ldx    $C8                     ;3
       ldy    LFCEE,X                 ;4
       lda    SWCHB                   ;4
       and    #$08                    ;2
       bne    LF352                   ;2
       ldy    #$04                    ;2
LF352:
       ldx    #$37                    ;2
LF33B:
       lda    INTIM                   ;4
       bne    LF33B                   ;2
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       sta    COLUPF                  ;3
       lda    $C5                     ;3
       bne    LF35F                   ;2
       jmp    LFE0EE                  ;3

LF35F:
       jmp    LFC000                  ;3

LF364:
       ldx    #$FF                    ;2
       stx    PF0                     ;3
       stx    PF1                     ;3
       stx    PF2                     ;3
       inx                            ;2
       stx    ENAM0                   ;3
       stx    ENAM1                   ;3
       sta    WSYNC                   ;3
       ldx    $83                     ;3
       cpx    #$08                    ;2
       bcc    LF381                   ;2
       ldx    #$07                    ;2
LF381:
       lda    LFEF4,X                 ;4
       sta    NUSIZ0                  ;3
       lda    #$37                    ;2
       sta    TIM8T                   ;4
       lda    $88                     ;3
       ldx    #$00                    ;2
       jsr    LFF5F                   ;6
       lda    $89                     ;3
       inx                            ;2
       jsr    LFF5F                   ;6
LF398:
       lda    INTIM                   ;4
       bne    LF398                   ;2
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       sta    COLUBK                  ;3 add to hide HMOVE line
       sta    PF0                     ;3
       sta    PF1                     ;3
       sta    PF2                     ;3
       sta    GRP0                    ;3
       sta    GRP1                    ;3
       ldx    $C8                     ;3
       lda    LFE00,X                 ;4
       sta    WSYNC                   ;3
       sta    COLUBK                  ;3
       ldy    #$05                    ;2
       jsr    LFDFA                   ;6
       lda    #$0F                    ;2
       sta    COLUP1                  ;3
       ldx    $83                     ;3
       ldy    #$08                    ;2 8 scanlines
LF3AE:
       lda    $88                     ;3
       bne    LF3B6                   ;2
       beq    LF3C0                   ;2 always branch

LF3B6:
       lda    LFF18,Y                 ;4
       cpx    #$01                    ;2
       bne    LF3C0                   ;2
       lda    LFCF2,Y                 ;4
LF3C0:
       sta    WSYNC                   ;3
       sta    GRP0                    ;3
       lda    $88                     ;3
       beq    LF3CB                   ;2
       lda    LFB00,Y                 ;4
LF3CB:
       sta    GRP1                    ;3
       sta    WSYNC                   ;3
       dey                            ;2
       bpl    LF3AE                   ;2
LF3D2:
       ldx    #$00                    ;2
       stx    GRP0                    ;3
       lda    #$40                    ;2
       jsr    LFF5F                   ;6
       inx                            ;2
       stx    VDELP0                  ;3
       stx    VDELP1                  ;3
       lda    #$4A                    ;2
       jsr    LFF5F                   ;6
       lda    #$0F                    ;2
       sta    COLUP0                  ;3
       sta    COLUP1                  ;3
       lda    #$03                    ;2
       sta    NUSIZ0                  ;3
       sta    NUSIZ1                  ;3
       ldy    #$08                    ;2 9 scanlines
       sta    HMOVE                   ;3
LF3F5:
       sty    $FA                     ;3
       lda    LFD00,Y                 ;4
       sta    $FB                     ;3
       sta    WSYNC                   ;3
       lda    ($EA),Y                 ;5
       sta    GRP0                    ;3
       lda    ($EC),Y                 ;5
       sta    GRP1                    ;3
       lda    ($EE),Y                 ;5
       sta    GRP0                    ;3
       lda    ($F0),Y                 ;5
       nop                            ;2
       tax                            ;2
       lda    ($F2),Y                 ;5
       ldy    $FB                     ;3
       stx    GRP1                    ;3
       sta    GRP0                    ;3
       sty    GRP1                    ;3
       sta    GRP0                    ;3
       ldy    $FA                     ;3
       dey                            ;2
       bpl    LF3F5                   ;2
       iny                            ;2
       sty    VDELP0                  ;3
       sty    VDELP1                  ;3
       sty    GRP0                    ;3
       sty    GRP1                    ;3
       sty    NUSIZ0                  ;3
       sty    NUSIZ1                  ;3
       lda    #$7C                    ;2
       ldx    #$01                    ;2
       jsr    LFF5F                   ;6
       lda    #$84                    ;2
       dex                            ;2
       jsr    LFF5F                   ;6
       sta    HMOVE                   ;3
       ldy    #$08                    ;2 9 scanlines
LF43F:
       lda    ($F4),Y                 ;5
       sta    WSYNC                   ;3
       sta    GRP1                    ;3
       lda    ($F6),Y                 ;5
       sta    GRP0                    ;3
       dey                            ;2
       bpl    LF43F                   ;2
       lda    $C5                     ;3
       tax                            ;2
       eor    #$01                    ;2
       sta    $C5                     ;3
       sta    WSYNC                   ;3
       lda    #$00                    ;2
       sta    GRP0                    ;3
       sta    GRP1                    ;3
       lda    $80                     ;3
       beq    LF48D                   ;2
       lda    $DA                     ;3
       bne    LF48D                   ;2
       lda    CXM0FB                  ;3
       bpl    LF46B                   ;2
       sty    $CF,X                   ;4
LF46B:
       lda    CXM1P                   ;3
       bmi    LF483                   ;2
       lda    $89                     ;3
       cmp    #$29                    ;2
       beq    LF483                   ;2
       lda    $C7                     ;3
       cmp    #$08                    ;2
       bcs    LF47F                   ;2
       lda    $DB,X                   ;4
       bne    LF48D                   ;2
LF47F:
       lda    CXPPMM                  ;3
       bpl    LF48D                   ;2
LF483:
       lda    #$00                    ;2
       sta    $8B                     ;3
       lda    #$06                    ;2
       sta    $C6                     ;3
       sty    $DA                     ;3
LF48D:
       sta    WSYNC                   ;3
       lda    CXM1FB                  ;3
       bpl    LF49D                   ;2
       cpx    #$00                    ;2
       beq    LF49B                   ;2
       sty    $D2                     ;3
       bne    LF49D                   ;2 always branch

LF49B:
       sty    $D1                     ;3
LF49D:
       lda    $E2                     ;3
       clc                            ;2
       adc    #$07                    ;2
       sta    $E2                     ;3
       sta    WSYNC                   ;3
       lda    #$20                    ;2
       sta    TIM64T                  ;4
       ldy    #$00                    ;2
       ldx    $E8                     ;3
       beq    LF4B9                   ;2
       dec    $E8                     ;5
       lda    #$01                    ;2
       ldy    #$03                    ;2
       bne    LF4E5                   ;2 always branch

LF4B9:
       lda    $84                     ;3
       bmi    LF4DB                   ;2
       lda    #$00                    ;2
       sta    $82                     ;3
       dec    $84                     ;5
       beq    LF4E5                   ;2
       lda    $C6                     ;3
       beq    LF4D3                   ;2
       cmp    #$44                    ;2
       bne    LF4D1                   ;2
       sty    $C6                     ;3
       beq    LF4D3                   ;2 always branch

LF4D1:
       dec    $C6                     ;5
LF4D3:
       ldx    #$1F                    ;2
       ldy    #$0F                    ;2
       bne    LF4E3                   ;2 always branch

LF4DB:
       ldx    $82                     ;3
       beq    LF4E5                   ;2
       inc    $82                     ;5
       ldy    #$08                    ;2
LF4E3:
       lda    #$08                    ;2
LF4E5:
       sta    AUDC0                   ;3
       stx    AUDF0                   ;3
       sty    AUDV0                   ;3
       ldy    $E9                     ;3
       beq    LF4F9                   ;2
       dec    $E9                     ;5
       lda    #$08                    ;2
       ldx    #$00                    ;2
       ldy    #$04                    ;2
       bne    LF525                   ;2 always branch

LF4F9:
       ldy    $8B                     ;3
       beq    LF50D                   ;2
       dec    $8B                     ;5
       cpy    #$12                    ;2
       bcs    LF507                   ;2
       ldy    #$00                    ;2
       beq    LF50D                   ;2 always branch

LF507:
       lda    #$06                    ;2
       ldx    #$01                    ;2
       bne    LF525                   ;2 always branch

LF50D:
       lda    $80                     ;3
       ora    $83                     ;3
       beq    LF529                   ;2
       lda    $DA                     ;3
       bne    LF529                   ;2
       lda    $8C                     ;3
       cmp    #$0F                    ;2
       beq    LF529                   ;2
       lda    #$0E                    ;2
       ldx    #$08                    ;2
       ldy    #$0A                    ;2
LF525:
       sta    AUDC1                   ;3
       stx    AUDF1                   ;3
LF529:
       sty    AUDV1                   ;3
LF52B:
       lda    $83                     ;3
       bne    LFA61                   ;2
       lda    $8A                     ;3
       cmp    #$80                    ;2
       beq    LFA3C                   ;2
       dec    $8A                     ;5
       bne    LFA61                   ;2
LFA3C:
       lda    INPT4                   ;3
       bmi    LFA61                   ;2
;new game
       jsr    LFC9B                   ;6
       ldy    #$FF                    ;2
       sty    $8A                     ;3
       sty    $D3                     ;3
       jsr    LFF8F                   ;6
       ldy    #$06                    ;2
       sty    $83                     ;3
       ldx    #$30                    ;2
       stx    $85                     ;3
       ldx    #$00                    ;2
       stx    $F8                     ;3
       stx    $C7                     ;3
       stx    $C8                     ;3
       stx    $80                     ;3
LFA61:
       lda    $88                     ;3
       beq    LF539                   ;2
       lda    SWCHB                   ;4
       and    #$01                    ;2
       bne    LF539                   ;2
START:
       sei                            ;2
       cld                            ;2
       lda    #$00                    ;2
       tax                            ;2
LF005:
       sta    VSYNC,X                 ;4
       txs                            ;2
       stx    $81                     ;3
       inx                            ;2
       bne    LF005                   ;2
       ldx    #$06                    ;2
       jmp    LF5B7a                  ;3



LF539:
       lda    SWCHA                   ;4
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       sta    $8C                     ;3
       lda    INPT4                   ;3
       eor    $D4                     ;3
       bpl    LF550                   ;2
       lda    INPT4                   ;3
       sta    $D4                     ;3
       bmi    LF550                   ;2
       inc    $D7                     ;5
LF550:
       lda    $84                     ;3
       bpl    LF5B7                   ;2
       lda    $80                     ;3
       bne    LF5B7                   ;2
       sta    $DA                     ;3
       lda    $88                     ;3
       beq    LF577                   ;2
       lda    $C5                     ;3
       bne    LF5B7                   ;2
       lda    #$04                    ;2
       sta    $E8                     ;3
       inc    $88                     ;5
       lda    #<LFD24                 ;2
       jsr    LFF79                   ;6
       lda    CXPPMM                  ;3
       bpl    LF5B7                   ;2
       lda    #$00                    ;2
       sta    $F8                     ;3
       sta    $E8                     ;3
LF577:
       inc    $D3                     ;5
       lda    #<LFD09                 ;2
       ldx    #$0C                    ;2
       jsr    LFF7B                   ;6
       lda    $F4                     ;3
       sec                            ;2
       sbc    #<(LFD00+$5A)           ;2
       bne    LF589                   ;2
       sta    $F4                     ;3
LF589:
       lda    $D3                     ;3
       cmp    #$04                    ;2
       bne    LF5A1                   ;2
       ldx    #$00                    ;2
       stx    $D3                     ;3
       inc    $83                     ;5
       ldx    #$2F                    ;2
       stx    $8B                     ;3
       inc    $C7                     ;5
       lda    $C7                     ;3
       and    #$03                    ;2
       sta    $C8                     ;3
LF5A1:
       lda    #$77                    ;2
       ldx    #$FF                    ;2
       ldy    $C7                     ;3
       beq    LF5AD                   ;2
       ldx    #$05                    ;2
       lda    #$01                    ;2
LF5AD:
       sta    $80                     ;3
       stx    $81                     ;3
       jsr    LF5DE                   ;6
       jsr    LFF8F                   ;6
LF5B7:
       lda    INTIM                   ;4
       bne    LF5B7                   ;2
       ldx    #$21                    ;2
LF5B7a:
       lda    #$02                    ;2
       sta    WSYNC                   ;3
       sta    VBLANK                  ;3
       sta    WSYNC                   ;3
       sta    CXCLR                   ;3
       sta    WSYNC                   ;3
       sta    WSYNC                   ;3
       sta    VSYNC                   ;3
       sta    WSYNC                   ;3
       sta    WSYNC                   ;3
       lda    #$00                    ;2
       sta    WSYNC                   ;3
       sta    VSYNC                   ;3
       stx    TIM64T                  ;4
       jmp    LF00D                   ;3




LFF8F: ;62
       sty    $9A                     ;3
       sty    $C4                     ;3
       sty    $A8                     ;3
       sty    $B6                     ;3
       lda    $C8                     ;3
       cmp    #$03                    ;2
       bcc    LFFA3                   ;2
       lda    $D3                     ;3
       beq    LFFAB                   ;2
       bne    LFFC6                   ;2 always branch
LFFA3:
       lda    $D3                     ;3
       cmp    #$03                    ;2
       bcc    LFFAB                   ;2
       ldy    #$00                    ;2
LFFAB:
       ldx    #$0A                    ;2
LFFAD:
       sty    $8D,X                   ;4
       sty    $9B,X                   ;4
       sty    $B7,X                   ;4
       sty    $A9,X                   ;4
       dex                            ;2
       bpl    LFFAD                   ;2
       lda    #$7F                    ;2
       cpy    #$00                    ;2
       beq    LFFC6                   ;2
       sta    $9B                     ;3
       sta    $9C                     ;3
       sta    $A9                     ;3
       sta    $AA                     ;3
LFFC6:
       lda    $85                     ;3
       and    #$03                    ;2
       sta    $F9                     ;3
       rts                            ;6






LF627:
       dex                            ;2
       lda    $DB,X                   ;4
       beq    LF675                   ;2
       dec    $DB,X                   ;6
       cmp    #$F0                    ;2
       beq    LF671                   ;2
       bcs    LF669                   ;2
       cmp    #$E0                    ;2
       bcc    LF649                   ;2
       dec    $D5,X                   ;6
       bne    LF675                   ;2
       lda    #$33                    ;2
       sta    $E3,X                   ;4
       lda    LFCFA,X                 ;4
       sta    $E6,X                   ;4
       txa                            ;2
       sta    $D8,X                   ;4
       rts                            ;6
LF649:
       ldy    $C7                     ;3
       cpy    #$08                    ;2
       bcs    LF656                   ;2
       lda    $DB,X                   ;4
       cmp    LFCC4,Y                 ;4
       bcs    LF675                   ;2
LF656:
       ldy    #$14                    ;2
       lda    $E6                     ;3
       sbc    $E7                     ;3
       cmp    #$E0                    ;2
       bcs    LF666                   ;2
       cmp    #$20                    ;2
       bcc    LF666                   ;2
       ldy    #$00                    ;2
LF666:
       sty    $DB,X                   ;4
       rts                            ;6
LF669:
       and    #$03                    ;2
       beq    LF671                   ;2
       lda    #$56                    ;2
       bne    LF673                   ;2 always branch
LF671:
       lda    #$0F                    ;2
LF673:
       sta    $D5,X                   ;4
LF675:
       rts                            ;6



LF676:
       stx    $FB                     ;3
       lda    $DA                     ;3
       bne    LF675                   ;2
       lda    $DB,X                   ;4
       bne    LF675                   ;2
       lda    $D8,X                   ;4
       beq    LF687                   ;2
       jmp    LF724                   ;3

LF687:
       lda    $E6,X                   ;4
       sec                            ;2
       sbc    #$01                    ;2
       cmp    #$11                    ;2
       bcc    LF699                   ;2
       jsr    LFFDE                   ;6
       bcs    LF69B                   ;2
LF695:
       lda    $FA                     ;3
       sta    $E6,X                   ;4
LF699:
       bne    LF6FF                   ;2
LF69B:
       lda    $E3,X                   ;4
       ldy    $C7                     ;3
       cpy    #$04                    ;2
       bcc    LF6B7                   ;2
       cmp    #$33                    ;2
       bcc    LF6AD                   ;2
       ldy    $E2                     ;3
       cpy    #$33                    ;2
       bcs    LF6B1                   ;2
LF6AD:
       cmp    $E2                     ;3
       bne    LF6B7                   ;2
LF6B1:
       ldy    $E6,X                   ;4
       cpy    $E5                     ;3
       bcc    LF6F9                   ;2
LF6B7:
       tay                            ;2
       lda    $FA                     ;3
       sec                            ;2
       sbc    #$01                    ;2
       jsr    LFD99                   ;6
       bne    LF6FF                   ;2
       ldx    $FB                     ;3
       lda    $E3,X                   ;4
       sec                            ;2
       sbc    #$07                    ;2
       tay                            ;2
       lda    $FA                     ;3
       sbc    #$01                    ;2
       jsr    LFD99                   ;6
       bne    LF6FF                   ;2
       ldx    $FB                     ;3
       txa                            ;2
       eor    #$01                    ;2
       tay                            ;2
       lda    $FA                     ;3
       cmp.wy $E6,Y                   ;4
       bcc    LF6FC                   ;2
       sbc    #$09                    ;2
       cmp.wy $E6,Y                   ;4
       bcs    LF6FC                   ;2
       lda    $E3,X                   ;4
       adc    #$07                    ;2
       cmp.wy $E3,Y                   ;4
       bcc    LF6FC                   ;2
       sbc    #$0F                    ;2
       bmi    LF6F9                   ;2
       cmp.wy $E3,Y                   ;4
       bcs    LF6FC                   ;2
LF6F9:
       jmp    LF817                   ;3

LF6FC:
       jmp    LF79F                   ;3

LF6FF:
       ldx    $FB                     ;3
       lda    $D5,X                   ;4
       cmp    #$0F                    ;2
       bne    LF713                   ;2
       lda    $85                     ;3
       cmp    #$21                    ;2
       bcc    LF713                   ;2
       cmp    #$27                    ;2
       bcc    LF719                   ;2
       bcs    LF71D                   ;2 always branch

LF713:
       lda    $E3,X                   ;4
       cmp    $E2                     ;3
       bcs    LF71D                   ;2
LF719:
       lda    #$02                    ;2
       bne    LF71F                   ;2 always branch

LF71D:
       lda    #$03                    ;2
LF71F:
       ldx    $FB                     ;3
       sta    $D8,X                   ;4
       rts                            ;6

LF724:
       cmp    #$01                    ;2
       bne    LF7A4                   ;2
LF72B:
       lda    $E6,X                   ;4
       clc                            ;2
       adc    #$01                    ;2
       cmp    #$8A                    ;2
       bcs    LF6FF                   ;2
       jsr    LFFDE                   ;6
       bcs    LF73C                   ;2
       jmp    LF695                   ;3

LF73C:
       lda    $E3,X                   ;4
       ldy    $C7                     ;3
       cpy    #$04                    ;2
       bcc    LF75B                   ;2
       cmp    #$33                    ;2
       bcc    LF74E                   ;2
       ldy    $E2                     ;3
       cpy    #$33                    ;2
       bcs    LF752                   ;2
LF74E:
       cmp    $E2                     ;3
       bne    LF75B                   ;2
LF752:
       ldy    $E6,X                   ;4
       cpy    $E5                     ;3
       bcc    LF75B                   ;2
LF758:
       jmp    LF81B                   ;3

LF75B:
       tay                            ;2
       lda    $FA                     ;3
       clc                            ;2
       adc    #$06                    ;2
       jsr    LFD99                   ;6
       bne    LF6FF                   ;2
       ldx    $FB                     ;3
       lda    $E3,X                   ;4
       sec                            ;2
       sbc    #$07                    ;2
       tay                            ;2
       lda    $FA                     ;3
       clc                            ;2
       adc    #$06                    ;2
       jsr    LFD99                   ;6
       bne    LF6FF                   ;2
       ldx    $FB                     ;3
       txa                            ;2
       eor    #$01                    ;2
       tay                            ;2
       lda    $FA                     ;3
       cmp.wy $E6,Y                   ;4
       bcs    LF79F                   ;2
       adc    #$09                    ;2
       cmp.wy $E6,Y                   ;4
       bcc    LF79F                   ;2
       lda    $E3,X                   ;4
       clc                            ;2
       adc    #$07                    ;2
       cmp.wy $E3,Y                   ;4
       bcc    LF79F                   ;2
       sbc    #$0F                    ;2
       bmi    LF758                   ;2
       cmp.wy $E3,Y                   ;4
       bcc    LF81B                   ;2
LF79F:
       lda    $FA                     ;3
       sta    $E6,X                   ;4
       rts                            ;6

LF7A4:
       cmp    #$02                    ;2
       bne    LF822                   ;2
       lda    $E3,X                   ;4
       clc                            ;2
       adc    #$01                    ;2
       cmp    #$38                    ;2
       bcs    LF7F7                   ;2
       jsr    LFFCD                   ;6
       beq    LF7F7                   ;2
       lda    $E6,X                   ;4
       ldy    $FA                     ;3
       jsr    LFD99                   ;6
       bne    LF7F7                   ;2
       ldx    $FB                     ;3
       ldy    $FA                     ;3
       lda    $E6,X                   ;4
       clc                            ;2
       adc    #$03                    ;2
       jsr    LFD99                   ;6
       bne    LF7F7                   ;2
       ldx    $FB                     ;3
       txa                            ;2
       eor    #$01                    ;2
       tay                            ;2
       lda    $FA                     ;3
       cmp.wy $E3,Y                   ;4
       bcs    LF7F4                   ;2
       adc    #$07                    ;2
       cmp.wy $E3,Y                   ;4
       bcc    LF7F4                   ;2
       lda    $E6,X                   ;4
       sbc    #$08                    ;2
       cmp.wy $E6,Y                   ;4
       bcs    LF7F4                   ;2
       adc    #$0F                    ;2
       cmp.wy $E6,Y                   ;4
       bcc    LF7F4                   ;2
       jmp    LF71D                   ;3

LF7F4:
       jmp    LF879                   ;3

LF7F7:
       ldx    $FB                     ;3
       lda    $D5,X                   ;4
       cmp    #$0F                    ;2
       bne    LF80B                   ;2
       ldy    $85                     ;3
       cpy    #$21                    ;2
       bcc    LF80B                   ;2
       cpy    #$27                    ;2
       bcc    LF817                   ;2
       bcs    LF81B                   ;2 always branch

LF80B:
       lda    $E6,X                   ;4
       cmp    $E5                     ;3
       bcc    LF817                   ;2
       bne    LF81B                   ;2
       cmp    #$50                    ;2
       bcs    LF81B                   ;2
LF817:
       lda    #$01                    ;2
       bne    LF81D                   ;2 always branch

LF81B:
       lda    #$00                    ;2
LF81D:
       ldx    $FB                     ;3
       sta    $D8,X                   ;4
       rts                            ;6

LF822:
       lda    $E3,X                   ;4
       sec                            ;2
       sbc    #$01                    ;2
       cmp    #$07                    ;2
       bcc    LF7F7                   ;2
       jsr    LFFCD                   ;6
       beq    LF7F7                   ;2
       lda    $FA                     ;3
       sec                            ;2
       sbc    #$07                    ;2
       tay                            ;2
       lda    $E6,X                   ;4
       sbc    #$01                    ;2
       jsr    LFD99                   ;6
       bne    LF7F7                   ;2
       ldx    $FB                     ;3
       lda    $FA                     ;3
       sec                            ;2
       sbc    #$07                    ;2
       tay                            ;2
       lda    $E6,X                   ;4
       clc                            ;2
       adc    #$03                    ;2
       jsr    LFD99                   ;6
       bne    LF7F7                   ;2
       ldx    $FB                     ;3
       txa                            ;2
       eor    #$01                    ;2
       tay                            ;2
       lda    $FA                     ;3
       cmp.wy $E3,Y                   ;4
       bcc    LF879                   ;2
       sbc    #$09                    ;2
       cmp.wy $E3,Y                   ;4
       bcs    LF879                   ;2
       lda    $E6,X                   ;4
       sec                            ;2
       sbc    #$08                    ;2
       cmp.wy $E6,Y                   ;4
       bcs    LF879                   ;2
       adc    #$10                    ;2
       cmp.wy $E6,Y                   ;4
       bcc    LF879                   ;2
       jmp    LF719                   ;3

LF879:
       lda    $FA                     ;3
       sta    $E3,X                   ;4
       rts                            ;6

LF87E:
       lda    $80                     ;3
       beq    LF88C                   ;2
       ldx    $C5                     ;3
       lda    $D1,X                   ;4
       bpl    LF8EF                   ;2
       ldy    $DB,X                   ;4
       beq    LF88D                   ;2
LF88C:
       rts                            ;6

LF88D:
       lda    $E3,X                   ;4
       adc    #$08                    ;2
       cmp    $E2                     ;3
       bcc    LF89D                   ;2
       sbc    #$18                    ;2
       bmi    LF8AB                   ;2
       cmp    $E2                     ;3
       bcc    LF8AB                   ;2
LF89D:
       lda    $E6,X                   ;4
       adc    #$10                    ;2
       cmp    $E5                     ;3
       bcc    LF88C                   ;2
       sbc    #$18                    ;2
       cmp    $E5                     ;3
       bcs    LF88C                   ;2
LF8AB:
       lda    $E5                     ;3
       ldy    $D8,X                   ;4
       bne    LF8B6                   ;2
       cmp    $E6,X                   ;4
       bcc    LF8CE                   ;2
       rts                            ;6

LF8B6:
       cpy    #$01                    ;2
       bne    LF8BF                   ;2
       cmp    $E6,X                   ;4
       bcs    LF8CE                   ;2
       rts                            ;6

LF8BF:
       lda    $E2                     ;3
       cpy    #$02                    ;2
       bne    LF8CA                   ;2
       cmp    $E3,X                   ;4
       bcs    LF8CE                   ;2
       rts                            ;6

LF8CA:
       cmp    $E3,X                   ;4
       bcs    LF88C                   ;2
LF8CE:
       lda    $E6,X                   ;4
       adc    LFCAA,Y                 ;4
       sta    $CD,X                   ;4
       lda    $E3,X                   ;4
       adc    LFCAE,Y                 ;4
       sta    $D1,X                   ;4
       tya                            ;2
       cpx    #$00                    ;2
       beq    LF8E3                   ;2
       asl                            ;2
       asl                            ;2
LF8E3:
       sta    $FA                     ;3
       lda    $CA                     ;3
       and    LFCB2,X                 ;4
       ora    $FA                     ;3
       sta    $CA                     ;3
       rts                            ;6

LF8EF:
       ldy    $C8                     ;3
       lda    $C7                     ;3
       cmp    #$04                    ;2
       bcc    LF8F8                   ;2
       iny                            ;2
LF8F8:
       lda    $CA                     ;3
       cpx    #$00                    ;2
       beq    LF900                   ;2
       lsr                            ;2
       lsr                            ;2
LF900:
       and    #$03                    ;2
       cmp    #$00                    ;2
       beq    LF920                   ;2
       cmp    #$02                    ;2
       bcc    LF929                   ;2
       beq    LF916                   ;2
       lda    $D1,X                   ;4
       sbc    LFAF6,Y                 ;4
       sta    $D1,X                   ;4
       jmp    LF930                   ;3

LF916:
       lda    $D1,X                   ;4
       clc                            ;2
       adc    LFAF6,Y                 ;4
       sta    $D1,X                   ;4
       bne    LF930                   ;2
LF920:
       lda    $CD,X                   ;4
       sbc    LFAF6,Y                 ;4
       sta    $CD,X                   ;4
       bne    LF930                   ;2
LF929:
       lda    $CD,X                   ;4
       adc    LFAF6,Y                 ;4
       sta    $CD,X                   ;4
LF930:
       txa                            ;2
       eor    #$01                    ;2
       tay                            ;2
       lda    $CD,X                   ;4
       cmp.wy $E6,Y                   ;4
       bcc    LF954                   ;2
       sbc    #$08                    ;2
       cmp.wy $E6,Y                   ;4
       bcs    LF954                   ;2
       lda    $D1,X                   ;4
       cmp.wy $E3,Y                   ;4
       bcs    LF954                   ;2
       adc    #$06                    ;2
       cmp.wy $E3,Y                   ;4
       bcc    LF954                   ;2
       lda    #$FF                    ;2
       sta    $D1,X                   ;4
LF954:
       rts                            ;6

LF955:
       and    #$03                    ;2
       bne    LF962                   ;2
       lda    $CB,X                   ;4
       sec                            ;2
       sbc    #$08                    ;2
LF95E:
       sta    $CB,X                   ;4
       bne    LF9D0                   ;2
LF962:
       cmp    #$02                    ;2
       bne    LF9D8                   ;2
       lda    $CF,X                   ;4
       clc                            ;2
       adc    #$04                    ;2
       sta    $CF,X                   ;4
       cmp    #$38                    ;2
       bcc    LF9EB                   ;2
       ldy    #$05                    ;2
LF973:
       lda    $CB,X                   ;4
       cmp    LFD70,Y                 ;4
       bcc    LF9CA                   ;2
       sbc    #$0C                    ;2
       cmp    LFD70,Y                 ;4
       bcs    LF9D2                   ;2
       lda    $80                     ;3
       and    LFD5A,Y                 ;4
       beq    LF9CA                   ;2
       lda    $80                     ;3
       and    LFD76,Y                 ;4
       sta    $80                     ;3
       ldx    $81                     ;3
       bmi    LF99D                   ;2
       dec    $81                     ;5
       bmi    LF99D                   ;2
       dex                            ;2
       lda    LFD8C,X                 ;4
       sta    $80                     ;3
LF99D:
       lda    #$1E                    ;2
       sta    $84                     ;3
       lda    #$4F                    ;2
       sta    $C6                     ;3
       lda    $C8                     ;3
       adc    $D3                     ;3
       ldy    $F8                     ;3
       beq    LF9AF                   ;2
       adc    #$02                    ;2
LF9AF:
       tax                            ;2
       lda    LFF56,X                 ;4
       ldy    $C7                     ;3
       cpy    #$04                    ;2
       bcc    LF9C0                   ;2
       ldx    #$04                    ;2
       jsr    LFF7B                   ;6
       lda    #<LFD2D                 ;2
LF9C0:
       ldx    #$06                    ;2
       jsr    LFF7B                   ;6
       lda    #<LFD2D                 ;2
       jsr    LFF79                   ;6
LF9CA:
       ldx    $C5                     ;3
       lda    #$FF                    ;2
       sta    $CF,X                   ;4
LF9D0:
       bne    LF9EB                   ;2
LF9D2:
       dey                            ;2
       bpl    LF973                   ;2
       bmi    LF9CA                   ;2 always branch

LF9D8:
       cmp    #$01                    ;2
       bne    LF9E4                   ;2
       lda    $CB,X                   ;4
       clc                            ;2
       adc    #$08                    ;2
       jmp    LF95E                   ;3

LF9E4:
       lda    $CF,X                   ;4
       sec                            ;2
       sbc    #$04                    ;2
       sta    $CF,X                   ;4
LF9EB:
       ldy    #$01                    ;2
LF9ED:
       lda    $CF,X                   ;4
       cmp.wy $E3,Y                   ;4
       beq    LF9FD                   ;2
       bcs    LFA21                   ;2
       adc    #$07                    ;2
       cmp.wy $E3,Y                   ;4
       bcc    LFA21                   ;2
LF9FD:
       lda    $CB,X                   ;4
       cmp.wy $E6,Y                   ;4
       bcc    LFA21                   ;2
       sbc    #$0A                    ;2
       cmp.wy $E6,Y                   ;4
       bcs    LFA21                   ;2
       lda    #$FF                    ;2
       sta    $CF,X                   ;4
       lda.wy $DB,Y                   ;4
       bne    LFA24                   ;2
       lda    #$FF                    ;2
       sta.wy $DB,Y                   ;5
       lda    #<LFD2D                 ;2
       sta    $E9                     ;3
       jmp    LFF79                   ;3

LFA21:
       dey                            ;2
       bpl    LF9ED                   ;2
LFA24:
       rts                            ;6


LFA25:
       sta    $88                     ;3
       ldy    #>LFD00                 ;2
       sty    $D3                     ;3
       cpy    $EB                     ;3
       beq    LFA32                   ;2
       jsr    LFC9B                   ;6
LFA32:
       lda    #$42                    ;2
       ldx    #$00                    ;2
       jsr    LFF5F                   ;6
       lda    #$4A                    ;2
       inx                            ;2
       jsr    LFF5F                   ;6
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       stx    NUSIZ0                  ;3
       stx    NUSIZ1                  ;3
       lda    $C7                     ;3
       and    #$F0                    ;2
       sta    COLUBK                  ;3
       adc    #$80                    ;2
       sta    $E5                     ;3
LFA81:
       lda    INTIM                   ;4
       bne    LFA81                   ;2
       sta    WSYNC                   ;3
       sta    VBLANK                  ;3
       ldy    #$50                    ;2 scanline correction
       jsr    LFDFA                   ;6
       ldx    #$10                    ;2
       ldy    $FD                     ;3
LFA93:
       lda    #$01                    ;2
       sta    VDELP0                  ;3
       sta    WSYNC                   ;3
       sta    VDELP1                  ;3
       lda    $E5                     ;3
       sta    COLUP0                  ;3
       sta    COLUP1                  ;3
       lda    LFEA9,Y                 ;4
       sta    GRP0                    ;3
       lda    LFEBC,Y                 ;4
       sta    GRP1                    ;3
       lda    LFECF,Y                 ;4
       inc.w  $E5                     ;6
       sta    GRP0                    ;3
       lda    LFEE2,Y                 ;4
       dey                            ;2
       sta    GRP1                    ;3
       sta    GRP0                    ;3
       bpl    LFAC2                   ;2
       ldy    #$13                    ;2
LFAC2:
       dex                            ;2
       bne    LFA93                   ;2
       stx    VDELP0                  ;3
       stx    VDELP1                  ;3
       stx    GRP0                    ;3
       stx    GRP1                    ;3
       sta    WSYNC                   ;3
       dec    $C6                     ;5
       bmi    LFAD7                   ;2
       bne    LFAEA                   ;2
LFAD7:
       lda    #$08                    ;2
       sta    $C6                     ;3
       dec    $FD                     ;5
       bpl    LFAEA                   ;2
       lda    #$13                    ;2
       sta    $FD                     ;3
       lda    $C7                     ;3
       clc                            ;2
       adc    #$10                    ;2
       sta    $C7                     ;3
LFAEA:
       ldy    #$47                    ;2
       jsr    LFDFA                   ;6
       lda    $E5                     ;3
       sta    COLUBK                  ;3
       jmp    LF3D2                   ;3



LFC000:
       sta    WSYNC                   ;3
       sty    COLUPF                  ;3
LFC00:
       txa                            ;2
       lsr                            ;2
       lsr                            ;2
       tay                            ;2
       sty    $FA                     ;3
       lda.wy $8D,Y                   ;4
       sta    PF1                     ;3
       lda.wy $9B,Y                   ;4
       sta    PF2                     ;3
       lda    #$02                    ;2
       cpx    $D0                     ;3
       bne    LFC18                   ;2
       beq    LFC1A                   ;2 always branch

LFC18:
       lsr                            ;2
LFC1A:
       sta    ENAM0                   ;3
       lda.wy $A9,Y                   ;4
       sta    PF2                     ;3
       lda.wy $B7,Y                   ;4
       sta    PF1                     ;3
       ldy    $DF                     ;3
       lda    LFB00,Y                 ;4
       dec    $DF                     ;5
       ldy    $FA                     ;3
       sta    GRP1                    ;3
       lda.wy $8D,Y                   ;4
       sta    PF1                     ;3
       lda.wy $9B,Y                   ;4
       sta    PF2                     ;3
       lda    #$02                    ;2
       cpx    $D2                     ;3
       bne    LFC43                   ;2
       beq    LFC45                   ;2 always branch

LFC43:
       lsr                            ;2
LFC45:
       sta    ENAM1                   ;3
       cpx    $E2                     ;3
       bne    LFC6C                   ;2
       dec    $DD                     ;5
       beq    LFC81                   ;2
       dec    $E2                     ;5
       lda.wy $A9,Y                   ;4
       sta    PF2                     ;3
       lda.wy $B7,Y                   ;4
       sta    PF1                     ;3
       ldy    $DD                     ;3
       lda    ($E0),Y                 ;5
LFC5F:
       dec    $2B                     ;5
       dex                            ;2
LFC63:
       sta    $FA                     ;3
       sta    GRP0                    ;3
       bpl    LFC00                   ;2
       jmp    LF364                   ;3

LFC6C:
       nop                            ;2
       nop                            ;2
       lda.wy $B7,Y                   ;4
       sta    PF1                     ;3
       lda.wy $A9,Y                   ;4
       sta    PF2                     ;3
       dec    $2B                     ;5
       dec    $2B                     ;5
       lda    #$00                    ;2
       beq    LFC5F                   ;2 always branch

LFC81:
       nop                            ;2
       nop                            ;2
       lda.wy $A9,Y                   ;4
       sta    PF2                     ;3
       lda.wy $B7,Y                   ;4
       sta    PF1                     ;3
       ldy    $DD                     ;3
       lda    ($E0),Y                 ;5
       dex                            ;2
       bmi    LFC96                   ;2
       bpl    LFC63                   ;2 always branch

LFC96:
       sta    GRP0                    ;3
       jmp    LF364                   ;3





LFC9B: ;15
       ldy    #>LFD00                 ;2
       lda    #<LFD00                 ;2
       ldx    #$0D                    ;2
LFCA1:
       sty    $EA,X                   ;4
       dex                            ;2
       sta    $EA,X                   ;4
       dex                            ;2
       bpl    LFCA1                   ;2
       rts                            ;6








LFD99:
       tax                            ;2
       cmp    #$30                    ;2
       bcs    LFDA4                   ;2
       sec                            ;2
       sbc    #$10                    ;2
       jmp    LFDCA                   ;3

LFDA4:
       cmp    #$50                    ;2
       bcs    LFDB2                   ;2
       tya                            ;2
       adc    #$38                    ;2
       tay                            ;2
       txa                            ;2
       sec                            ;2
       sbc    #$10                    ;2
       bne    LFDCA                   ;2
LFDB2:
       cmp    #$70                    ;2
       bcs    LFDC1                   ;2
       tya                            ;2
       adc    #$70                    ;2
       tay                            ;2
       txa                            ;2
       sec                            ;2
       sbc    #$50                    ;2
       jmp    LFDCA                   ;3

LFDC1:
       tya                            ;2
       clc                            ;2
       adc    #$A8                    ;2
       tay                            ;2
       txa                            ;2
       sec                            ;2
       sbc    #$50                    ;2
LFDCA:
       lsr                            ;2
       lsr                            ;2
       tax                            ;2
       tya                            ;2
       lsr                            ;2
       lsr                            ;2
       tay                            ;2
       lda    $85                     ;3
       and    #$03                    ;2
       beq    LFDEE                   ;2
       cmp    #$03                    ;2
       beq    LFDEE                   ;2
       lda    $D3                     ;3
       beq    LFDE7                   ;2
       cmp    #$02                    ;2
       bcs    LFDEE                   ;2
       lda    $FB                     ;3
       bne    LFDEE                   ;2
LFDE7:
       lda.wy $8D,Y                   ;4
       and    LFCCC,X                 ;4
       rts                            ;6

LFDEE:
       lda    LFCCC,X                 ;4
       eor    #$FF                    ;2
       and.wy $8D,Y                   ;4
       sta.wy $8D,Y                   ;5
       lda    #$00                    ;2
       rts                            ;6

LFDFA:
       sta    WSYNC                   ;3
       dey                            ;2
       bne    LFDFA                   ;2
       rts                            ;6





LFE0EE:
       sta    WSYNC                   ;3
       sty    COLUPF                  ;3
LFE0E:
       txa                            ;2
       lsr                            ;2
       lsr                            ;2
       tay                            ;2
       sty    $FA                     ;3
       lda.wy $8D,Y                   ;4
       sta    PF1                     ;3
       lda.wy $9B,Y                   ;4
       sta    PF2                     ;3
       lda    #$02                    ;2
       cpx    $CF                     ;3 CF/D0
       bne    LFE26                   ;2
       beq    LFE28                   ;2 always branch

LFE26:
       lsr                            ;2
LFE28:
       sta    ENAM0                   ;3
       lda.wy $A9,Y                   ;4
       sta    PF2                     ;3
       lda.wy $B7,Y                   ;4
       sta    PF1                     ;3
       ldy    $DE                     ;3 DE/DF
       lda    LFB00,Y                 ;4
       dec    $DE                     ;5 DE/DF
       ldy    $FA                     ;3
       sta    GRP1                    ;3
       lda.wy $8D,Y                   ;4
       sta    PF1                     ;3
       lda.wy $9B,Y                   ;4
       sta    PF2                     ;3
       lda    #$02                    ;2
       cpx    $D1                     ;3 D1/D2
       bne    LFE51                   ;2
       beq    LFE53                   ;2 always branch

LFE51:
       lsr                            ;2
LFE53:
       sta    ENAM1                   ;3
       cpx    $E2                     ;3
       bne    LFE7A                   ;2
       dec    $DD                     ;5
       beq    LFE8F                   ;2
       dec    $E2                     ;5
       lda.wy $A9,Y                   ;4
       sta    PF2                     ;3
       lda.wy $B7,Y                   ;4
       sta    PF1                     ;3
       ldy    $DD                     ;3
       lda    ($E0),Y                 ;5
LFE6D:
       dec    $2B                     ;5
       dex                            ;2
LFE71:
       sta    $FA                     ;3
       sta    GRP0                    ;3
       bpl    LFE0E                   ;2
       jmp    LF364                   ;3

LFE7A:
       nop                            ;2
       nop                            ;2
       lda.wy $B7,Y                   ;4
       sta    PF1                     ;3
       lda.wy $A9,Y                   ;4
       sta    PF2                     ;3
       dec    $2B                     ;5
       dec    $2B                     ;5
       lda    #$00                    ;2
       beq    LFE6D                   ;2 always branch

LFE8F:
       nop                            ;2
       nop                            ;2
       lda.wy $A9,Y                   ;4
       sta    PF2                     ;3
       lda.wy $B7,Y                   ;4
       sta    PF1                     ;3
       ldy    $DD                     ;3
       lda    ($E0),Y                 ;5
       dex                            ;2
       bmi    LFEA4                   ;2
       bpl    LFE71                   ;2 always branch

LFEA4:
       sta    GRP0                    ;3
       jmp    LF364                   ;3

       ORG $FCEC,0

LFE0A:
       .byte $02 ; |      X | $FE0A
       .byte $08 ; |    X   | $FE0B

LFCB2:
       .byte $FC ; |XXXXXX  | $FCB2
       .byte $F3 ; |XXXX  XX| $FCB3

LFE00:
       .byte $20 ; |  X     | $FE00
       .byte $30 ; |  XX    | $FE01
       .byte $20 ; |  X     | $FE02
       .byte $F0 ; |XXXX    | $FE03
LFE04:
       .byte $3E ; |  XXXXX | $FE04
       .byte $76 ; | XXX XX | $FE05
       .byte $AE ; |X X XXX | $FE06
       .byte $E6 ; |XXX  XX | $FE07

LFE0C:
       .byte $03 ; |      XX| $FE0C
       .byte $0C ; |    XX  | $FE0D


LFE08:
       .byte $01 ; |       X| $FE08
LFD8C:
       .byte $04 ; |     X  | $FD8C shared
       .byte $20 ; |  X     | $FD8D
       .byte $02 ; |      X | $FD8E
       .byte $10 ; |   X    | $FD8F
       .byte $40 ; | X      | $FD90

       ORG $FD00,0

LFD00:
       .byte %00111100 ;$3C ; |  XXXX  | $FD00
       .byte %01000010 ;$42 ; | X    X | $FD01
       .byte %01000010 ;$42 ; | X    X | $FD02
       .byte %01000010 ;$42 ; | X    X | $FD03
       .byte %01000010 ;$42 ; | X    X | $FD04
       .byte %01000010 ;$42 ; | X    X | $FD05
       .byte %01000010 ;$42 ; | X    X | $FD06
       .byte %01000010 ;$42 ; | X    X | $FD07
       .byte %00111100 ;$3C ; |  XXXX  | $FD08
LFD09:
       .byte %00111000 ;$38 ; |  XXX   | $FD09
       .byte %00010000 ;$10 ; |   X    | $FD0A
       .byte %00010000 ;$10 ; |   X    | $FD0B
       .byte %00010000 ;$10 ; |   X    | $FD0C
       .byte %00010000 ;$10 ; |   X    | $FD0D
       .byte %00010000 ;$10 ; |   X    | $FD0E
       .byte %00010000 ;$10 ; |   X    | $FD0F
       .byte %00010000 ;$10 ; |   X    | $FD10
       .byte %00110000 ;$30 ; |  XX    | $FD11
LFD12:
       .byte %01111110 ;$7E ; | XXXXXX | $FD12
       .byte %01000000 ;$40 ; | X      | $FD13
       .byte %01000000 ;$40 ; | X      | $FD14
       .byte %01000000 ;$40 ; | X      | $FD15
       .byte %00111100 ;$3C ; |  XXXX  | $FD16
       .byte %00000010 ;$02 ; |      X | $FD17
       .byte %00000010 ;$02 ; |      X | $FD18
       .byte %01000010 ;$42 ; | X    X | $FD19
       .byte %00111100 ;$3C ; |  XXXX  | $FD1A
LFD1B:
       .byte %00111100 ;$3C ; |  XXXX  | $FD1B
       .byte %01000010 ;$42 ; | X    X | $FD1C
       .byte %00000010 ;$02 ; |      X | $FD1D
       .byte %00000010 ;$02 ; |      X | $FD1E
       .byte %00011100 ;$1C ; |   XXX  | $FD1F
       .byte %00000010 ;$02 ; |      X | $FD20
       .byte %00000010 ;$02 ; |      X | $FD21
       .byte %01000010 ;$42 ; | X    X | $FD22
       .byte %00111100 ;$3C ; |  XXXX  | $FD23
LFD24:
       .byte %00000100 ;$04 ; |     X  | $FD24
       .byte %00000100 ;$04 ; |     X  | $FD25
       .byte %00000100 ;$04 ; |     X  | $FD26
       .byte %01111110 ;$7E ; | XXXXXX | $FD27
       .byte %01000100 ;$44 ; | X   X  | $FD28
       .byte %00100100 ;$24 ; |  X  X  | $FD29
       .byte %00010100 ;$14 ; |   X X  | $FD2A
       .byte %00001100 ;$0C ; |    XX  | $FD2B
       .byte %00000100 ;$04 ; |     X  | $FD2C
LFD2D:
       .byte %00111100 ;$3C ; |  XXXX  | $FD2D
       .byte %01000010 ;$42 ; | X    X | $FD2E
       .byte %00000010 ;$02 ; |      X | $FD2F
       .byte %00000010 ;$02 ; |      X | $FD30
       .byte %00111100 ;$3C ; |  XXXX  | $FD31
       .byte %01000000 ;$40 ; | X      | $FD32
       .byte %01000000 ;$40 ; | X      | $FD33
       .byte %01000000 ;$40 ; | X      | $FD34
       .byte %01111110 ;$7E ; | XXXXXX | $FD35
LFD36:
       .byte %00111100 ;$3C ; |  XXXX  | $FD36
       .byte %01000010 ;$42 ; | X    X | $FD37
       .byte %01000010 ;$42 ; | X    X | $FD38
       .byte %01000010 ;$42 ; | X    X | $FD39
       .byte %01111100 ;$7C ; | XXXXX  | $FD3A
       .byte %01000000 ;$40 ; | X      | $FD3B
       .byte %01000000 ;$40 ; | X      | $FD3C
       .byte %01000010 ;$42 ; | X    X | $FD3D
       .byte %00111100 ;$3C ; |  XXXX  | $FD3E
LFD3F:
       .byte %00100000 ;$20 ; |  X     | $FD3F
       .byte %00100000 ;$20 ; |  X     | $FD40
       .byte %00100000 ;$20 ; |  X     | $FD41
       .byte %00010000 ;$10 ; |   X    | $FD42
       .byte %00010000 ;$10 ; |   X    | $FD43
       .byte %00001000 ;$08 ; |    X   | $FD44
       .byte %00000100 ;$04 ; |     X  | $FD45
       .byte %00000010 ;$02 ; |      X | $FD46
       .byte %01111110 ;$7E ; | XXXXXX | $FD47
       .byte %00111100 ;$3C ; |  XXXX  | $FD48
       .byte %01000010 ;$42 ; | X    X | $FD49
       .byte %01000010 ;$42 ; | X    X | $FD4A
       .byte %01000010 ;$42 ; | X    X | $FD4B
       .byte %00111100 ;$3C ; |  XXXX  | $FD4C
       .byte %01000010 ;$42 ; | X    X | $FD4D
       .byte %01000010 ;$42 ; | X    X | $FD4E
       .byte %01000010 ;$42 ; | X    X | $FD4F
       .byte %00111100 ;$3C ; |  XXXX  | $FD50
       .byte %00111100 ;$3C ; |  XXXX  | $FD51
       .byte %01000010 ;$42 ; | X    X | $FD52
       .byte %00000010 ;$02 ; |      X | $FD53
       .byte %00000010 ;$02 ; |      X | $FD54
       .byte %00111110 ;$3E ; |  XXXXX | $FD55
       .byte %01000010 ;$42 ; | X    X | $FD56
       .byte %01000010 ;$42 ; | X    X | $FD57
       .byte %01000010 ;$42 ; | X    X | $FD58
       .byte %00111100 ;$3C ; |  XXXX  | $FD59

LF5DE: ;73
       ldx    $83                     ;3
       cpx    #$05                    ;2
       bcc    LF5E6                   ;2
       ldx    #$04                    ;2
LF5E6:
       lda    LFEFB,X                 ;4
       sta    $88                     ;3
       lda    #$8C                    ;2
       sta    $89                     ;3
       ldx    #$00                    ;2
       stx    $E9                     ;3
       stx    $D7                     ;3
       stx    $C6                     ;3
       stx    $DA                     ;3
       stx    $DB                     ;3
       stx    $DC                     ;3
       stx    $D8                     ;3
       inx                            ;2
       stx    $D9                     ;3
       lda    #$91                    ;2
       sta    $E6                     ;3
       lda    #$07                    ;2
       sta    $E7                     ;3
       sta    $E2                     ;3
       lda    #$33                    ;2
       sta    $E3                     ;3
       sta    $E4                     ;3
       lda    #$20                    ;2
       sta    $E0                     ;3
       lda    #$4D                    ;2
       sta    $E5                     ;3
       ldy    #$FF                    ;2
       sty    $D1                     ;3
       sty    $D2                     ;3
       sty    $CF                     ;3
       sty    $D0                     ;3
       sty    $E1                     ;3
       rts                            ;6

LFAF6:
       .byte $01 ; |       X| $FAF6
       .byte $02 ; |      X | $FAF7
       .byte $03 ; |      XX| $FAF8
       .byte $04 ; |     X  | $FAF9
       .byte $04 ; |     X  | $FAFA
LFAFB:
       .byte $FC ; |XXXXXX  | $FAFB
       .byte $F3 ; |XXXX  XX| $FAFC
       .byte $89 ; |X   X  X| $FAFD
       .byte $E9 ; |XXX X  X| $FAFE
       .byte $E9 ; |XXX X  X| $FAFF

LFCAA:
       .byte $FF ; |XXXXXXXX| $FCAA
       .byte $06 ; |     XX | $FCAB
       .byte $05 ; |     X X| $FCAC
       .byte $05 ; |     X X| $FCAD

LFCAE:
       .byte $FE ; |XXXXXXX | $FCAE
       .byte $FC ; |XXXXXX  | $FCAF
       .byte $01 ; |       X| $FCB0
       .byte $FD ; |XXXXXX X| $FCB1

LFCC4:
       .byte $02 ; |      X | $FCC4
       .byte $60 ; | XX     | $FCC5
       .byte $90 ; |X  X    | $FCC6
       .byte $C0 ; |XX      | $FCC7
       .byte $B0 ; |X XX    | $FCC8
       .byte $C0 ; |XX      | $FCC9
       .byte $D0 ; |XX X    | $FCCA
       .byte $E0 ; |XXX     | $FCCB
LFCCC:
       .byte $80 ; |X       | $FCCC
       .byte $40 ; | X      | $FCCD
       .byte $20 ; |  X     | $FCCE
       .byte $10 ; |   X    | $FCCF
       .byte $08 ; |    X   | $FCD0
       .byte $04 ; |     X  | $FCD1
       .byte $02 ; |      X | $FCD2
       .byte $01 ; |       X| $FCD3
       .byte $01 ; |       X| $FCD4
       .byte $02 ; |      X | $FCD5
       .byte $04 ; |     X  | $FCD6
       .byte $08 ; |    X   | $FCD7
       .byte $10 ; |   X    | $FCD8
       .byte $20 ; |  X     | $FCD9
       .byte $40 ; | X      | $FCDA
       .byte $80 ; |X       | $FCDB

LFD5A:
       .byte $01 ; |       X| $FD5A
       .byte $02 ; |      X | $FD5B
       .byte $04 ; |     X  | $FD5C
       .byte $10 ; |   X    | $FD5D
       .byte $20 ; |  X     | $FD5E
       .byte $40 ; | X      | $FD5F


LFD95:
       .byte $16 ; |   X XX | $FD95
       .byte $0A ; |    X X | $FD96
       .byte $16 ; |   X XX | $FD97
       .byte $02 ; |      X | $FD98



LFCFA:
       .byte $93 ; |X  X  XX| $FCFA
       .byte $07 ; |     XXX| $FCFB
LFCFC:
       .byte $C0 ; |XX      | $FCFC
       .byte $04 ; |     X  | $FCFD
       .byte $90 ; |X  X    | $FCFE
LFCEE:
       .byte $12 ; |   X  X | $FCEE shared
       .byte $B0 ; |X XX    | $FCEF
       .byte $04 ; |     X  | $FCF0
       .byte $90 ; |X  X    | $FCF1


LFD70:
       .byte $73 ; | XXX  XX| $FD70
       .byte $63 ; | XX   XX| $FD71
       .byte $53 ; | X X  XX| $FD72
       .byte $43 ; | X    XX| $FD73
       .byte $33 ; |  XX  XX| $FD74
       .byte $23 ; |  X   XX| $FD75
LFD76:
       .byte $F6 ; |XXXX XX | $FD76
       .byte $F5 ; |XXXX X X| $FD77
       .byte $F3 ; |XXXX  XX| $FD78
       .byte $6F ; | XX XXXX| $FD79
       .byte $5F ; | X XXXXX| $FD7A
       .byte $3F ; |  XXXXXX| $FD7B

LFD91:
       .byte $7C ; | XXXXX  | $FD91
       .byte $5C ; | X XXX  | $FD92
       .byte $2C ; |  X XX  | $FD93
       .byte $0C ; |    XX  | $FD94

LFD7C:
       .byte $08 ; |    X   | $FD7C
       .byte $75 ; | XXX X X| $FD7D
       .byte $65 ; | XX  X X| $FD7E
       .byte $65 ; | XX  X X| $FD7F
       .byte $55 ; | X X X X| $FD80
       .byte $55 ; | X X X X| $FD81
       .byte $55 ; | X X X X| $FD82
       .byte $55 ; | X X X X| $FD83
LFD84:
       .byte $00 ; |        | $FD84
       .byte $45 ; | X   X X| $FD85
       .byte $35 ; |  XX X X| $FD86
       .byte $35 ; |  XX X X| $FD87
       .byte $25 ; |  X  X X| $FD88
       .byte $25 ; |  X  X X| $FD89
       .byte $25 ; |  X  X X| $FD8A
       .byte $25 ; |  X  X X| $FD8B


       ORG $FE00,0

LFB00:
       .byte %00000000 ;$00 ; |        | $FB00
LFB01:
  IF ORIGINAL
       .byte %00111111 ;$3F ; |  XXXXXX| $FB01
       .byte %00001110 ;$0E ; |    XXX | $FB02
       .byte %00011111 ;$1F ; |   XXXXX| $FB03
       .byte %11111111 ;$FF ; |XXXXXXXX| $FB04
       .byte %00011111 ;$1F ; |   XXXXX| $FB05
       .byte %00001110 ;$0E ; |    XXX | $FB06
       .byte %00111111 ;$3F ; |  XXXXXX| $FB07
       ORG $FE39,0
       .byte %11111100 ;$FC ; |XXXXXX  | $FB39
       .byte %01110000 ;$70 ; | XXX    | $FB3A
       .byte %11111000 ;$F8 ; |XXXXX   | $FB3B
       .byte %11111111 ;$FF ; |XXXXXXXX| $FB3C
       .byte %11111000 ;$F8 ; |XXXXX   | $FB3D
       .byte %01110000 ;$70 ; | XXX    | $FB3E
       .byte %11111100 ;$FC ; |XXXXXX  | $FB3F
       ORG $FE71,0
       .byte %01011001 ;$5D ; | X XXX X| $FB71
       .byte %01111111 ;$7F ; | XXXXXXX| $FB72
       .byte %01111111 ;$7F ; | XXXXXXX| $FB73
       .byte %01011101 ;$5D ; | X XXX X| $FB74
       .byte %01001001 ;$49 ; | X  X  X| $FB75
       .byte %00001000 ;$08 ; |    X   | $FB76
       .byte %00001000 ;$08 ; |    X   | $FB77
       ORG $FEA9,0
       .byte %00001000 ;$08 ; |    X   | $FBA9
       .byte %00001000 ;$08 ; |    X   | $FBAA
       .byte %01001001 ;$49 ; | X  X  X| $FBAB
       .byte %01011101 ;$5D ; | X XXX X| $FBAC
       .byte %01111111 ;$7F ; | XXXXXXX| $FBAD
       .byte %01111111 ;$7F ; | XXXXXXX| $FBAE
       .byte %01011101 ;$5D ; | X XXX X| $FBAF
  ELSE
       .byte %00001001 ;$09 ; |    X  X| $FB01
       .byte %00010010 ;$12 ; |   X  X | $FB02
       .byte %10010010 ;$92 ; |X  X  X | $FB03
       .byte %01111111 ;$7F ; | XXXXXXX| $FB05
       .byte %10010010 ;$92 ; |X  X  X | $FB06
       .byte %00010010 ;$12 ; |   X  X | $FB07
       .byte %00001001 ;$09 ; |    X  X| $FB08
       ORG $FE39,0
       .byte %10010000 ;$90 ; |X  X    | $FB39
       .byte %01001000 ;$48 ; | X  X   | $FB3A
       .byte %01001001 ;$49 ; | X  X  X| $FB3B
       .byte %11111110 ;$FE ; |XXXXXXX | $FB3D
       .byte %01001001 ;$49 ; | X  X  X| $FB3E
       .byte %01001000 ;$48 ; | X  X   | $FB3F
       .byte %10010000 ;$90 ; |X  X    | $FB40
       ORG $FE71,0
       .byte %10011001 ;$99 ; |X  XX  X| $FB71
       .byte %01111110 ;$7E ; | XXXXXX | $FB72
       .byte %00011000 ;$18 ; |   XX   | $FB73
       .byte %10011001 ;$99 ; |X  XX  X| $FB74
       .byte %01111110 ;$7E ; | XXXXXX | $FB75
       .byte %00011000 ;$18 ; |   XX   | $FB76
       .byte %00100100 ;$24 ; |  X  X  | $FB77
       ORG $FEA9,0
       .byte %00100100 ;$24 ; |  X  X  | $FBA9
       .byte %00011000 ;$18 ; |   XX   | $FBAA
       .byte %01111110 ;$7E ; | XXXXXX | $FBAB
       .byte %10011001 ;$99 ; |X  XX  X| $FBAC
       .byte %00011000 ;$18 ; |   XX   | $FBAD
       .byte %01111110 ;$7E ; | XXXXXX | $FBAE
       .byte %10011001 ;$99 ; |X  XX  X| $FBAF
  ENDIF

       ORG $FF00,0

       .byte %00000000 ;$00 ; |        | $FF00
       .byte %01100100 ;$64 ; | XX  X  | $FF01
       .byte %10010010 ;$92 ; |X  X  X | $FF02
       .byte %01000100 ;$44 ; | X   X  | $FF03
       .byte %00101100 ;$2C ; |  X XX  | $FF04
       .byte %11001110 ;$CE ; |XX  XXX | $FF05
       .byte %01101100 ;$6C ; | XX XX  | $FF06
       .byte %00110000 ;$30 ; |  XX    | $FF07
       .byte %00000000 ;$00 ; |        | $FF08 shared
       .byte %01001000 ;$48 ; | X  X   | $FF09
       .byte %00000010 ;$02 ; |      X | $FF0A
       .byte %00000000 ;$00 ; |        | $FF0B
       .byte %00100000 ;$20 ; |  X     | $FF0C
       .byte %00000001 ;$01 ; |       X| $FF0D
       .byte %10000000 ;$80 ; |X       | $FF0E
       .byte %00100010 ;$22 ; |  X   X | $FF0F
LFF10:
       .byte %00000000 ;$00 ; |        | $FF10 shared
  IF ORIGINAL
       .byte %00010010 ;$12 ; |   X  X | $FF11
       .byte %00111111 ;$3F ; |  XXXXXX| $FF12
       .byte %01111001 ;$79 ; | XXXX  X| $FF13
       .byte %01000011 ;$43 ; | X    XX| $FF14
       .byte %01111001 ;$79 ; | XXXX  X| $FF15
       .byte %00111111 ;$3F ; |  XXXXXX| $FF16
       .byte %00010010 ;$12 ; |   X  X | $FF17
LFF18:
       .byte %00000000 ;$00 ; |        | $FF18 shared
       .byte %01001000 ;$48 ; | X  X   | $FF19
       .byte %11111100 ;$FC ; |XXXXXX  | $FF1A
       .byte %10011110 ;$9E ; |X  XXXX | $FF1B
       .byte %11000010 ;$C2 ; |XX    X | $FF1C
       .byte %10011110 ;$9E ; |X  XXXX | $FF1D
       .byte %11111100 ;$FC ; |XXXXXX  | $FF1E
       .byte %01001000 ;$48 ; | X  X   | $FF1F
LFF20:
       .byte %00000000 ;$00 ; |        | $FF20 shared
       .byte %01111100 ;$7C ; | XXXXX  | $FF21
       .byte %11010110 ;$D6 ; |XX X XX | $FF22
       .byte %01000100 ;$44 ; | X   X  | $FF23
       .byte %01101100 ;$6C ; | XX XX  | $FF24
       .byte %11101110 ;$EE ; |XXX XXX | $FF25
       .byte %01101100 ;$6C ; | XX XX  | $FF26
       .byte %00111000 ;$38 ; |  XXX   | $FF27
LFF28:
       .byte %00000000 ;$00 ; |        | $FF28 shared
LFF29:
       .byte %00111000 ;$38 ; |  XXX   | $FF29
       .byte %01101100 ;$6C ; | XX XX  | $FF2A
       .byte %11101110 ;$EE ; |XXX XXX | $FF2B
       .byte %01101100 ;$6C ; | XX XX  | $FF2C
       .byte %01000100 ;$44 ; | X   X  | $FF2D
       .byte %11010110 ;$D6 ; |XX X XX | $FF2E
       .byte %01111100 ;$7C ; | XXXXX  | $FF2F
  ELSE
       .byte %00111100 ;$3C ; |  XXXX  | $FF11
       .byte %00001000 ;$08 ; |    X   | $FF12
       .byte %01111110 ;$7E ; | XXXXXX | $FF13
       .byte %11111110 ;$FE ; |XXXXXXX | $FF14
       .byte %01111110 ;$7E ; | XXXXXX | $FF15
       .byte %00001000 ;$08 ; |    X   | $FF16
       .byte %00111100 ;$3C ; |  XXXX  | $FF17
LFF18:
       .byte %00000000 ;$00 ; |        | $FF18 shared
       .byte %01111000 ;$78 ; | XXXX   | $FF19
       .byte %00100000 ;$20 ; |  X     | $FF1A
       .byte %11111100 ;$FC ; |XXXXXX  | $FF1B
       .byte %11111110 ;$FE ; |XXXXXXX | $FF1C
       .byte %11111100 ;$FC ; |XXXXXX  | $FF1D
       .byte %00100000 ;$20 ; |  X     | $FF1E
       .byte %01111000 ;$78 ; | XXXX   | $FF1F
LFF20:
       .byte %00000000 ;$00 ; |        | $FF20 shared
       .byte %00111000 ;$38 ; |  XXX   | $FF21
       .byte %10111010 ;$BA ; |X XXX X | $FF22
       .byte %11111110 ;$FE ; |XXXXXXX | $FF23
       .byte %10111010 ;$BA ; |X XXX X | $FF24
       .byte %10111010 ;$BA ; |X XXX X | $FF25
       .byte %00111000 ;$38 ; |  XXX   | $FF26
       .byte %00010000 ;$10 ; |   X    | $FF27
LFF28:
       .byte %00000000 ;$00 ; |        | $FF28 shared
LFF29:
       .byte %00010000 ;$10 ; |   X    | $FF29
       .byte %00111000 ;$38 ; |  XXX   | $FF2A
       .byte %10111010 ;$BA ; |X XXX X | $FF2B
       .byte %10111010 ;$BA ; |X XXX X | $FF2C
       .byte %11111110 ;$FE ; |XXXXXXX | $FF2D
       .byte %10111010 ;$BA ; |X XXX X | $FF2E
       .byte %00111000 ;$38 ; |  XXX   | $FF2F
  ENDIF

LFF30:
       .byte %00000000 ;$00 ; |        | $FF30 shared
       .byte %01000000 ;$40 ; | X      | $FF31
       .byte %01000000 ;$40 ; | X      | $FF32
       .byte %01000110 ;$46 ; | X   XX | $FF33
       .byte %01001110 ;$4E ; | X  XXX | $FF34
       .byte %01111110 ;$7E ; | XXXXXX | $FF35
       .byte %01111110 ;$7E ; | XXXXXX | $FF36
       .byte %01110000 ;$70 ; | XXX    | $FF37
LFF38:
       .byte %00000000 ;$00 ; |        | $FF38 shared
       .byte %00111000 ;$38 ; |  XXX   | $FF39
       .byte %01111100 ;$7C ; | XXXXX  | $FF3A
       .byte %01111100 ;$7C ; | XXXXX  | $FF3B
       .byte %00010000 ;$10 ; |   X    | $FF3C
       .byte %00010000 ;$10 ; |   X    | $FF3D
       .byte %00010000 ;$10 ; |   X    | $FF3E
       .byte %00111000 ;$38 ; |  XXX   | $FF3F
LFF40:
       .byte %00000000 ;$00 ; |        | $FF40 shared
       .byte %00111110 ;$3E ; |  XXXXX | $FF41
       .byte %00101010 ;$2A ; |  X X X | $FF42
       .byte %00111110 ;$3E ; |  XXXXX | $FF43
       .byte %01110010 ;$72 ; | XXX  X | $FF44
       .byte %00111110 ;$3E ; |  XXXXX | $FF45
       .byte %00000010 ;$02 ; |      X | $FF46
       .byte %00000010 ;$02 ; |      X | $FF47
LFF48:
       .byte %00000000 ;$00 ; |        | $FF48 shared
       .byte %00000010 ;$02 ; |      X | $FF49
       .byte %10100101 ;$A5 ; |X X  X X| $FF4A
       .byte %11111101 ;$FD ; |XXXXXX X| $FF4B
       .byte %00000101 ;$05 ; |     X X| $FF4C
       .byte %00000010 ;$02 ; |      X | $FF4D
LFF4E:
       .byte $00 ; |        | $FF4E shared
       .byte $00 ; |        | $FF4F shared
       .byte $00 ; |        | $FF50 shared
       .byte $01 ; |       X| $FF51
       .byte $00 ; |        | $FF52
       .byte $02 ; |      X | $FF53
       .byte $01 ; |       X| $FF54
       .byte $03 ; |      XX| $FF55
LFF56:
       .byte <LFD00 ; |        | $FF56
       .byte <LFD00 ; |        | $FF57
       .byte <LFD09 ; |    X  X| $FF58
       .byte <LFD12 ; |   X  X | $FF59
       .byte <LFD1B ; |   XX XX| $FF5A
       .byte <LFD24 ; |  X  X  | $FF5B
       .byte <LFD2D ; |  X XX X| $FF5C
       .byte <LFD36 ; |  XX XX | $FF5D
       .byte <LFD3F ; |  XXXXXX| $FF5E


LFCDC:
       .byte %00000000 ;$00 ; |        | $FCDC
       .byte %01101110 ;$6E ; | XX XXX | $FCDD
       .byte %10011001 ;$99 ; |X  XX  X| $FCDE
       .byte %10111101 ;$BD ; |X XXXX X| $FCDF
       .byte %11100110 ;$E6 ; |XXX  XX | $FCE0
       .byte %01100111 ;$67 ; | XX  XXX| $FCE1
       .byte %10111101 ;$BD ; |X XXXX X| $FCE2
       .byte %10011001 ;$99 ; |X  XX  X| $FCE3
       .byte %01110110 ;$76 ; | XXX XX | $FCE4
LFCE5:
       .byte %00000000 ;$00 ; |        | $FCE5 shared
       .byte %00011000 ;$18 ; |   XX   | $FCE6
       .byte %00100100 ;$24 ; |  X  X  | $FCE7
       .byte %01111110 ;$7E ; | XXXXXX | $FCE8
       .byte %10100101 ;$A5 ; |X X  X X| $FCE9
       .byte %10100101 ;$A5 ; |X X  X X| $FCEA
       .byte %01111110 ;$7E ; | XXXXXX | $FCEB
       .byte %00100100 ;$24 ; |  X  X  | $FCEC
       .byte %00011000 ;$18 ; |   XX   | $FCED
LFCF2:
       .byte %00000000 ;$00 ; |        | $FCF2 shared
  IF ORIGINAL
       .byte %01000100 ;$44 ; | X   X  | $FCF3
       .byte %10111010 ;$BA ; |X XXX X | $FCF4
       .byte %10000001 ;$81 ; |X      X| $FCF5
       .byte %10000001 ;$81 ; |X      X| $FCF6
       .byte %10000001 ;$81 ; |X      X| $FCF7
       .byte %10111010 ;$BA ; |X XXX X | $FCF8
       .byte %01000100 ;$44 ; | X   X  | $FCF9
  ELSE
       .byte %01111000 ;$78 ; | XXXX   | $FCF3
       .byte %00100000 ;$20 ; |  X     | $FCF4
       .byte %11111100 ;$FC ; |XXXXXX  | $FCF5
       .byte %10000010 ;$82 ; |X     X | $FCF6
       .byte %11111100 ;$FC ; |XXXXXX  | $FCF7
       .byte %00100000 ;$20 ; |  X     | $FCF8
       .byte %01111000 ;$78 ; | XXXX   | $FCF9
  ENDIF
LFCB4:
       .byte $00 ; |        | $FCB4 - shared
       .byte $00 ; |        | $FCB5 -
       .byte $00 ; |        | $FCB6 -
       .byte $00 ; |        | $FCB7 -
       .byte $00 ; |        | $FCB8 -
       .byte $00 ; |        | $FCB9 -
       .byte $00 ; |        | $FCBA -
       .byte <LFF18 ; |   XX   | $FCBB right
       .byte $00 ; |        | $FCBC -
       .byte $00 ; |        | $FCBD -
       .byte $00 ; |        | $FCBE -
       .byte <LFF10 ; |   X    | $FCBF left
       .byte $00 ; |        | $FCC0 -
       .byte <LFF28 ; |  X X   | $FCC1 down
       .byte <LFF20 ; |  X     | $FCC2 up
       .byte $00 ; |        | $FCC3 -

       .byte <LFF38 ; | X  X   | $FFF9
       .byte <LFF40 ; |  XX    | $FFFA
LFFF9: ;must start > $xx03 and end with $00
       .byte <LFF48 ; | X  X   | $FFF9
       .byte <LFF30 ; |  XX    | $FFFA
       .byte <LFF29 ; |  X X  X| $FFFB
LFEA9:
       .byte %00000000 ;$00 ; |        | $FEA9 shared
  IF ORIGINAL
       .byte %11111100 ;$FC ; |XXXXXX  | $FEAA
       .byte %00000010 ;$02 ; |      X | $FEAB
       .byte %00000010 ;$02 ; |      X | $FEAC
       .byte %01111100 ;$7C ; | XXXXX  | $FEAD
       .byte %10000000 ;$80 ; |X       | $FEAE
       .byte %10000000 ;$80 ; |X       | $FEAF
       .byte %01111110 ;$7E ; | XXXXXX | $FEB0
       .byte %00000000 ;$00 ; |        | $FEB1
       .byte %00000000 ;$00 ; |        | $FEB2
       .byte %00000000 ;$00 ; |        | $FEB3
       .byte %01111100 ;$7C ; | XXXXX  | $FEB4
       .byte %10000010 ;$82 ; |X     X | $FEB5
       .byte %10111010 ;$BA ; |X XXX X | $FEB6
       .byte %10100010 ;$A2 ; |X X   X | $FEB7
       .byte %10111010 ;$BA ; |X XXX X | $FEB8
       .byte %10000010 ;$82 ; |X     X | $FEB9
       .byte %01111100 ;$7C ; | XXXXX  | $FEBA
       .byte %00000000 ;$00 ; |        | $FEBB
LFEBC:
       .byte %00000000 ;$00 ; |        | $FEBC shared
       .byte %11111110 ;$FE ; |XXXXXXX | $FEBD
       .byte %10000000 ;$80 ; |X       | $FEBE
       .byte %10000000 ;$80 ; |X       | $FEBF
       .byte %11111000 ;$F8 ; |XXXXX   | $FEC0
       .byte %10000000 ;$80 ; |X       | $FEC1
       .byte %10000000 ;$80 ; |X       | $FEC2
       .byte %11111110 ;$FE ; |XXXXXXX | $FEC3
       .byte %00000000 ;$00 ; |        | $FEC4
       .byte %00000000 ;$00 ; |        | $FEC5
       .byte %00000000 ;$00 ; |        | $FEC6
       .byte %01110000 ;$70 ; | XXX    | $FEC7
       .byte %00100000 ;$20 ; |  X     | $FEC8
       .byte %00100000 ;$20 ; |  X     | $FEC9
       .byte %00100011 ;$23 ; |  X   XX| $FECA
       .byte %00100100 ;$24 ; |  X  X  | $FECB
       .byte %01100100 ;$64 ; | XX  X  | $FECC
       .byte %00100011 ;$23 ; |  X   XX| $FECD
       .byte %00000000 ;$00 ; |        | $FECE
LFECF:
       .byte %00000000 ;$00 ; |        | $FECF shared
       .byte %01111110 ;$7E ; | XXXXXX | $FED0
       .byte %10000010 ;$82 ; |X     X | $FED1
       .byte %10000010 ;$82 ; |X     X | $FED2
       .byte %10011110 ;$9E ; |X  XXXX | $FED3
       .byte %10000000 ;$80 ; |X       | $FED4
       .byte %10000010 ;$82 ; |X     X | $FED5
       .byte %01111100 ;$7C ; | XXXXX  | $FED6
       .byte %00000000 ;$00 ; |        | $FED7
       .byte %00000000 ;$00 ; |        | $FED8
       .byte %00000000 ;$00 ; |        | $FED9
       .byte %01001110 ;$4E ; | X  XXX | $FEDA
       .byte %01010001 ;$51 ; | X X   X| $FEDB
       .byte %01010001 ;$51 ; | X X   X| $FEDC
       .byte %11001110 ;$CE ; |XX  XXX | $FEDD
       .byte %01010001 ;$51 ; | X X   X| $FEDE
       .byte %01010001 ;$51 ; | X X   X| $FEDF
       .byte %10001110 ;$8E ; |X   XXX | $FEE0
       .byte %00000000 ;$00 ; |        | $FEE1
LFEE2:
       .byte %00000000 ;$00 ; |        | $FEE2 shared
       .byte %10000010 ;$82 ; |X     X | $FEE3
       .byte %10000010 ;$82 ; |X     X | $FEE4
       .byte %11111110 ;$FE ; |XXXXXXX | $FEE5
       .byte %10000010 ;$82 ; |X     X | $FEE6
       .byte %10000010 ;$82 ; |X     X | $FEE7
       .byte %01000100 ;$44 ; | X   X  | $FEE8
       .byte %00111000 ;$38 ; |  XXX   | $FEE9
       .byte %00000000 ;$00 ; |        | $FEEA
       .byte %00000000 ;$00 ; |        | $FEEB
       .byte %00000000 ;$00 ; |        | $FEEC
       .byte %00111000 ;$38 ; |  XXX   | $FEED
       .byte %01000100 ;$44 ; | X   X  | $FEEE
       .byte %00000100 ;$04 ; |     X  | $FEEF
       .byte %00011000 ;$18 ; |   XX   | $FEF0
       .byte %00000100 ;$04 ; |     X  | $FEF1
       .byte %01000100 ;$44 ; | X   X  | $FEF2
       .byte %00111000 ;$38 ; |  XXX   | $FEF3
  ELSE
       .byte %10010010 ;$92 ; |X  X  X | $FEAA
       .byte %01111100 ;$7C ; | XXXXX  | $FEAB
       .byte %00010000 ;$10 ; |   X    | $FEAC
       .byte %10010010 ;$92 ; |X  X  X | $FEAD
       .byte %01111100 ;$7C ; | XXXXX  | $FEAE
       .byte %00010000 ;$10 ; |   X    | $FEAF
       .byte %00101000 ;$28 ; |  X X   | $FEB0
       .byte %00000000 ;$00 ; |        | $FEB1
       .byte %00000000 ;$00 ; |        | $FEB2
       .byte %00000000 ;$00 ; |        | $FEB3
       .byte %10001011 ;$8B ; |X   X XX| $FEB4
       .byte %10001010 ;$8A ; |X   X X | $FEB5
       .byte %10001010 ;$8A ; |X   X X | $FEB6
       .byte %11111010 ;$FA ; |XXXXX X | $FEB7
       .byte %10001010 ;$8A ; |X   X X | $FEB8
       .byte %10001010 ;$8A ; |X   X X | $FEB9
       .byte %01110010 ;$72 ; | XXX  X | $FEBA
       .byte %00000000 ;$00 ; |        | $FEBB
LFEBC:
       .byte %00000000 ;$00 ; |        | $FEBC shared
       .byte %10001010 ;$8A ; |X   X X | $FEBD
       .byte %10001010 ;$8A ; |X   X X | $FEBE
       .byte %10001010 ;$8A ; |X   X X | $FEBF
       .byte %11111010 ;$FA ; |XXXXX X | $FEC0
       .byte %10001011 ;$8B ; |X   X XX| $FEC1
       .byte %10001011 ;$8B ; |X   X XX| $FEC2
       .byte %01110010 ;$72 ; | XXX  X | $FEC3
       .byte %00000000 ;$00 ; |        | $FEC4
       .byte %00000000 ;$00 ; |        | $FEC5
       .byte %00000000 ;$00 ; |        | $FEC6
       .byte %11101011 ;$EB ; |XXX X XX| $FEC7
       .byte %00001010 ;$0A ; |    X X | $FEC8
       .byte %00001010 ;$0A ; |    X X | $FEC9
       .byte %00001011 ;$0B ; |    X XX| $FECA
       .byte %00001010 ;$0A ; |    X X | $FECB
       .byte %00001010 ;$0A ; |    X X | $FECC
       .byte %00001011 ;$0B ; |    X XX| $FECD
       .byte %00000000 ;$00 ; |        | $FECE
LFECF:
       .byte %00000000 ;$00 ; |        | $FECF shared
       .byte %00100010 ;$22 ; |  X   X | $FED0
       .byte %01100010 ;$62 ; | XX   X | $FED1
       .byte %01100010 ;$62 ; | XX   X | $FED2
       .byte %10100010 ;$A2 ; |X X   X | $FED3
       .byte %00100010 ;$22 ; |  X   X | $FED4
       .byte %00100010 ;$22 ; |  X   X | $FED5
       .byte %00101111 ;$2F ; |  X XXXX| $FED6
       .byte %00000000 ;$00 ; |        | $FED7
       .byte %00000000 ;$00 ; |        | $FED8
       .byte %00000000 ;$00 ; |        | $FED9
       .byte %11010001 ;$D1 ; |XX X   X| $FEDA
       .byte %00010011 ;$13 ; |   X  XX| $FEDB
       .byte %00010011 ;$13 ; |   X  XX| $FEDC
       .byte %11010101 ;$D5 ; |XX X X X| $FEDD
       .byte %00011001 ;$19 ; |   XX  X| $FEDE
       .byte %00011001 ;$19 ; |   XX  X| $FEDF
       .byte %11010001 ;$D1 ; |XX X   X| $FEE0
       .byte %00000000 ;$00 ; |        | $FEE1
LFEE2:
       .byte %00000000 ;$00 ; |        | $FEE2 shared
       .byte %00111110 ;$3E ; |  XXXXX | $FEE3
       .byte %00000010 ;$02 ; |      X | $FEE4
       .byte %00000010 ;$02 ; |      X | $FEE5
       .byte %00111110 ;$3E ; |  XXXXX | $FEE6
       .byte %00100000 ;$20 ; |  X     | $FEE7
       .byte %00100000 ;$20 ; |  X     | $FEE8
       .byte %10111110 ;$BE ; |X XXXXX | $FEE9
       .byte %00000000 ;$00 ; |        | $FEEA
       .byte %00000000 ;$00 ; |        | $FEEB
       .byte %00000000 ;$00 ; |        | $FEEC
       .byte %01001001 ;$49 ; | X  X  X| $FEED
       .byte %00111110 ;$3E ; |  XXXXX | $FEEE
       .byte %00001000 ;$08 ; |    X   | $FEEF
       .byte %01001001 ;$49 ; | X  X  X| $FEF0
       .byte %00111110 ;$3E ; |  XXXXX | $FEF1
       .byte %00001000 ;$08 ; |    X   | $FEF2
       .byte %00010100 ;$14 ; |   X X  | $FEF3
  ENDIF
LFEF4:
       .byte $00 ; |        | $FEF4 shared
       .byte $00 ; |        | $FEF5 shared
       .byte $00 ; |        | $FEF6
       .byte $01 ; |       X| $FEF7
       .byte $03 ; |      XX| $FEF8
       .byte $03 ; |      XX| $FEF9
       .byte $03 ; |      XX| $FEFA
LFEFB:
       .byte $03 ; |      XX| $FEFB shared
       .byte $22 ; |  X   X | $FEFC
       .byte $22 ; |  X   X | $FEFD
       .byte $12 ; |   X  X | $FEFE
       .byte $02 ; |      X | $FE0A

       .byte "Hack by Nukey Shay,2008"

       ORG $FFFC
       .word START,START
