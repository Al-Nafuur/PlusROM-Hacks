;KOOLAID MAN (c)1982 Mattel: Supercharger version by Kurt (Nukey Shay) Howe, 8/31/2009

PAL = 0

; Disassembly of Koolaid.bin
; Disassembled Wed Aug 26 17:54:18 2009
; Using DiStella v3.0
; Command Line: C:\BIN\D3.EXE -pafscKoolaid.cfg Koolaid.bin
; Koolaid.cfg contents:
;      ORG  F000
;      GFX  F000 F03C
;      CODE F03D F0D6
;      GFX  F0D7 F0DA
;      CODE F0DB F152
;      GFX  F153 F175
;      CODE F176 F88E
;      GFX  F88F F899
;      CODE F89A F8B5
;      GFX  F8B6 FACD
;      CODE FACE FEEF
;      GFX  FEF0 FEFF
;      CODE FF00 FF7A
;      GFX  FF7B FFA5
;      CODE FFA6 FFF9
;      GFX  FFFA FFFF

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
REFP0   =  $0B
REFP1   =  $0C
PF0     =  $0D
PF1     =  $0E
PF2     =  $0F
RESP0   =  $10
RESP1   =  $11
RESM0   =  $12
AUDC0   =  $15
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
HMM0    =  $22
VDELP0  =  $25
VDELP1  =  $26
HMOVE   =  $2A
HMCLR   =  $2B
CXCLR   =  $2C
CXPPMM  =  COLUP1
SWCHA   =  $0280
SWCHB   =  $0282
INTIM   =  $0284
TIM64T  =  $0296

       ORG $F000

LF001:
       .byte $7E ; | XXXXXX | $F001
       .byte $66 ; | XX  XX | $F002
       .byte $66 ; | XX  XX | $F003
       .byte $66 ; | XX  XX | $F004
       .byte $66 ; | XX  XX | $F005
       .byte $7E ; | XXXXXX | $F006
LF007:
       .byte $7E ; | XXXXXX | $F007
       .byte $18 ; |   XX   | $F008
       .byte $18 ; |   XX   | $F009
       .byte $18 ; |   XX   | $F00A
       .byte $18 ; |   XX   | $F00B
       .byte $78 ; | XXXX   | $F00C
LF00D:
       .byte $7E ; | XXXXXX | $F00D
       .byte $60 ; | XX     | $F00E
       .byte $7E ; | XXXXXX | $F00F
       .byte $06 ; |     XX | $F010
       .byte $66 ; | XX  XX | $F011
       .byte $7E ; | XXXXXX | $F012
LF013:
       .byte $7E ; | XXXXXX | $F013
       .byte $06 ; |     XX | $F014
       .byte $06 ; |     XX | $F015
       .byte $7C ; | XXXXX  | $F016
       .byte $06 ; |     XX | $F017
       .byte $7E ; | XXXXXX | $F018
LF019:
       .byte $06 ; |     XX | $F019
       .byte $06 ; |     XX | $F01A
       .byte $7E ; | XXXXXX | $F01B
       .byte $66 ; | XX  XX | $F01C
       .byte $66 ; | XX  XX | $F01D
       .byte $66 ; | XX  XX | $F01E
LF01F:
       .byte $7E ; | XXXXXX | $F01F
       .byte $66 ; | XX  XX | $F020
       .byte $06 ; |     XX | $F021
       .byte $7E ; | XXXXXX | $F022
       .byte $60 ; | XX     | $F023
       .byte $7E ; | XXXXXX | $F024
LF025:
       .byte $7E ; | XXXXXX | $F025
       .byte $66 ; | XX  XX | $F026
       .byte $66 ; | XX  XX | $F027
       .byte $7E ; | XXXXXX | $F028
       .byte $60 ; | XX     | $F029
       .byte $7E ; | XXXXXX | $F02A
LF02B:
       .byte $20 ; |  X     | $F02B
       .byte $30 ; |  XX    | $F02C
       .byte $18 ; |   XX   | $F02D
       .byte $0C ; |    XX  | $F02E
       .byte $06 ; |     XX | $F02F
       .byte $7E ; | XXXXXX | $F030
LF031:
       .byte $7E ; | XXXXXX | $F031
       .byte $66 ; | XX  XX | $F032
       .byte $66 ; | XX  XX | $F033
       .byte $3C ; |  XXXX  | $F034
       .byte $66 ; | XX  XX | $F035
       .byte $7E ; | XXXXXX | $F036
LF037:
       .byte $7E ; | XXXXXX | $F037
       .byte $06 ; |     XX | $F038
       .byte $7E ; | XXXXXX | $F039
       .byte $66 ; | XX  XX | $F03A
       .byte $66 ; | XX  XX | $F03B
       .byte $7E ; | XXXXXX | $F03C


LF03D:
       lda    $D6                     ;3
       lsr                            ;2
       lda    #$8F                    ;2
       bcs    LF04F                   ;2
       lda    $D6                     ;3
       lsr                            ;2
       tax                            ;2
       lda    LFA78,X                 ;4 could use Y...
LF04F:
       sta    WSYNC                   ;3
       sta    WSYNC                   ;3
       sta    WSYNC                   ;3
       sta    COLUBK                  ;3
       lda    #$F0                    ;2
       sta    HMP0                    ;3
       ldx    #$02                    ;2
LF066:
       dex                            ;2
       bne    LF066                   ;2
       stx    CTRLPF                  ;3
       stx    HMP1                    ;3
       stx    COLUP0                  ;3
       stx    COLUP1                  ;3
       ldy    #$05                    ;2
       sty    CTRLPF                  ;3
       sta    RESP0                   ;3
       sta    RESP1                   ;3
       sty    VDELP0                  ;3
       sty    VDELP1                  ;3
       stx    COLUPF                  ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       stx    GRP0                    ;3
       stx    GRP1                    ;3
       stx    GRP0                    ;3
       stx    REFP0                   ;3
       stx    REFP1                   ;3
       lda    #$03                    ;2
       sta    NUSIZ0                  ;3
       sta    NUSIZ1                  ;3
       iny                            ;2
LF09C:
       sta    WSYNC                   ;3
       inc    $2E                     ;5
       dey                            ;2
       ldx    LF001,Y                 ;4 last digit always zero (could have just loaded X)
       lda    ($BF),Y                 ;5
       sta    GRP0                    ;3
       lda    ($C1),Y                 ;5
       sta    GRP1                    ;3
       lda    ($C3),Y                 ;5
       sta    GRP0                    ;3
       lda    ($C5),Y                 ;5
       sta    GRP1                    ;3
       stx    GRP0                    ;3
       stx    GRP1                    ;3
       stx    GRP0                    ;3
       tya                            ;2
       bne    LF09C                   ;2
       sta    WSYNC                   ;3
       sta    VDELP0                  ;3
       sta    VDELP1                  ;3
       sta    GRP0                    ;3
       sta    GRP1                    ;3
       rts                            ;6


LF0DB: ;55
       ldx    #$03                    ;2
LF0DD:
       txa                            ;2
       lsr                            ;2
       tay                            ;2
       lda.wy $80,Y                   ;4
       cpx    #$03                    ;2
       beq    LF0F2                   ;2
       cpx    #$01                    ;2
       beq    LF0F2                   ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
LF0F2:
       and    #$0F                    ;2
       tay                            ;2
       lda    LFA6E,Y                 ;4
       sta    $BD                     ;3
       txa                            ;2
       asl                            ;2
       tay                            ;2
       lda    $BD                     ;3
       sta.wy $BF,Y                   ;5
       lda    #>LF001                 ;2
       sta.wy $C0,Y                   ;5
       dex                            ;2
       bpl    LF0DD                   ;2
       rts                            ;6



LF112:
       ldx    #$01                    ;2
LF114:
       txa                            ;2
       lsr                            ;2
       tay                            ;2
       lda    $B5                     ;3
       cpx    #$01                    ;2
       beq    LF124                   ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
LF124:
       and    #$0F                    ;2
       tay                            ;2
       lda    LFA6E,Y                 ;4
       sta    $BD                     ;3
       txa                            ;2
       asl                            ;2
       tay                            ;2
       lda    $BD                     ;3
       sta.wy $C8,Y                   ;5
       lda    #>LF001                 ;2
       sta.wy $C9,Y                   ;5
       dex                            ;2
       bpl    LF114                   ;2
       rts                            ;6



LF144: ;11
       ldx    #$22                    ;2
LF148:
       lda    LF153,X                 ;4
       sta    $93,x                   ;4
       dex                            ;2
       bpl    LF148                   ;2
       rts                            ;6



LFFA6: ;45
       lda    $B2                     ;3
       cmp    #$01                    ;2
       bne    LFFB5                   ;2
       lda    #$00                    ;2
       sta    $D3                     ;3
       lda    #$02                    ;2
       sta    $D1                     ;3
       rts                            ;6
LFFB5:
       lda    $82                     ;3
       and    #$03                    ;2
       beq    LFFBC                   ;2
       rts                            ;6
LFFBC:
       ldx    #$13                    ;2
       lda    #$80                    ;2
       sta    $D3                     ;3
LFFC2:
       lda    LF23F,X                 ;4
       ora    $B2                     ;3
       and    $B3                     ;3
       sta    $BD,X                   ;4
       dex                            ;2
       bpl    LFFC2                   ;2
       lsr    $B2                     ;5
       lsr    $B3                     ;5
       rts                            ;6



LFFD3: ;29
       lda    $AC                     ;3
       cmp    #$0B                    ;2
       bne    LFFEA                   ;2
       lda    $B5                     ;3
       cmp    #$40                    ;2
       bne    LFFEA                   ;2
       lda    SWCHB                   ;4
       ror                            ;2
       ror                            ;2
       bcs    LFFEA                   ;2
       ror                            ;2
       ror                            ;2
       bcc    LFFEB                   ;2
LFFEA:
       rts                            ;6
LFFEB:
       lda    #$80                    ;2
       sta    $DC                     ;3
       rts                            ;6


LFFF0: ;10
       lda    $8B                     ;3
       clc                            ;2
       adc    #$17                    ;2
       and    #$1F                    ;2
       sta    $8B                     ;3
       rts                            ;6

       ORG $F166

LF153:
       .byte $46 ; | X   XX | $F153
       .byte $5C ; | X XXX  | $F154
       .byte $50 ; | X X    | $F155
       .byte $5D ; | X XXX X| $F156
       .byte $4B ; | X  X XX| $F157
       .byte $54 ; | X X X  | $F158
       .byte $14 ; |   X X  | $F159
       .byte $EC ; |XXX XX  | $F15A
       .byte $E2 ; |XXX   X | $F15B
       .byte $1E ; |   XXXX | $F15C
       .byte $14 ; |   X X  | $F15D
       .byte $E7 ; |XXX  XXX| $F15E
       .byte $03 ; |      XX| $F15F
       .byte $03 ; |      XX| $F160
       .byte $03 ; |      XX| $F161
       .byte $03 ; |      XX| $F162
       .byte $03 ; |      XX| $F163
       .byte $03 ; |      XX| $F164
       .byte $06 ; |     XX | $F165
       .byte $06 ; |     XX | $F166
       .byte $06 ; |     XX | $F167
       .byte $06 ; |     XX | $F168
       .byte $06 ; |     XX | $F169
       .byte $06 ; |     XX | $F16A
       .byte $14 ; |   X X  | $F16B
       .byte $14 ; |   X X  | $F16C
       .byte $FF ; |XXXXXXXX| $F16D
       .byte $FF ; |XXXXXXXX| $F16E
       .byte $FF ; |XXXXXXXX| $F16F
       .byte $FF ; |XXXXXXXX| $F170
       .byte $80 ; |X       | $F171
       .byte $7F ; | XXXXXXX| $F172
       .byte $FF ; |XXXXXXXX| $F173
       .byte $50 ; | X X    | $F174
       .byte $60 ; | XX     | $F175



START:
       cld                            ;2
       ldx    #$00                    ;2
       txa                            ;2
LF17A:
       sta    VSYNC,X                 ;4
       txs                            ;2
       inx                            ;2
       bne    LF17A                   ;2
       lda    #$05                    ;2
       sta    $B6                     ;3
       lda    #$14                    ;2
       sta    $DB                     ;3
       lda    #$FF                    ;2
       sta    $E4                     ;3
       sta    $E5                     ;3
       ldx    #$13                    ;2
LF190:
       sta    $BD,X                   ;4
       dex                            ;2
       bpl    LF190                   ;2
       sta    $D5                     ;3
       lda    #$05                    ;2
       sta    CTRLPF                  ;3
       lda    #$08                    ;2
       sta    REFP0                   ;3
  IF PAL
       lda    #$42                    ;2
  ELSE
       lda    #$34                    ;2
  ENDIF
       sta    COLUP0                  ;3
       jsr    LF144                   ;6
LF1A6:
       sta    WSYNC                   ;3
       lda    #$02                    ;2
       sta    VBLANK                  ;3
       lda    #$1D                    ;2
       sta    TIM64T                  ;4
       lda    SWCHB                   ;4
       lsr                            ;2
       bcc    START                   ;2
       lda    SWCHB                   ;4
       asl                            ;2
       sta    $DD                     ;3
       bpl    LF1C9                   ;2
       lda    #$00                    ;2
       sta    AUDV0                   ;3
       sta    AUDV1                   ;3
       bpl    LF1FA                   ;2 always branch

LF1C9:
       inc    $82                     ;5
       jsr    LF711                   ;6
       lda    $D5                     ;3
       bpl    LF1DB                   ;2
       jsr    LFFF0                   ;6
       jsr    LF7A2                   ;6
       jmp    LF1FD                   ;3

LF1DB:
       lda    $D9                     ;3
       beq    LF1E1                   ;2
       dec    $D9                     ;5
LF1E1:
       lda    $D1                     ;3
       bmi    LF1E8                   ;2
       jsr    LF89A                   ;6
LF1E8:
       jsr    LFFF0                   ;6
       jsr    LF257                   ;6
       jsr    LFFD3                   ;6
       jsr    LFACE                   ;6
       jsr    LF677                   ;6
       jsr    LF44E                   ;6
LF1FA:
       jsr    LFB09                   ;6
LF1FD:
       lda    INTIM                   ;4
       bpl    LF1FD                   ;2
       sta    WSYNC                   ;3
       ldx    #$03                    ;2
       stx    VSYNC                   ;3
LF208:
       sta    WSYNC                   ;3
       jsr    LF441                   ;6
       dex                            ;2
       bne    LF208                   ;2
       stx    VSYNC                   ;3
       lda    #$27                    ;2
       sta    TIM64T                  ;4
       lda    $DD                     ;3
       bmi    LF23F                   ;2
       lda    $D5                     ;3
       bpl    LF225                   ;2
       jsr    LFB7B                   ;6
       jmp    LF242                   ;3

LF225:
       jsr    LF612                   ;6
       jsr    LF535                   ;6
       jsr    LF35B                   ;6
       jsr    LF5A2                   ;6
       sta    HMCLR                   ;3
       jsr    LF0DB                   ;6
       jsr    LF112                   ;6
       jsr    LF5C0                   ;6
LF23C:
       jsr    LFB7B                   ;6
LF23F:
       jsr    LFB49                   ;6
LF242:
       lda    INTIM                   ;4
       bne    LF242                   ;2
       lda    $D5                     ;3
       bpl    LF251                   ;2
       jsr    LFE16                   ;6
       jmp    LF1A6                   ;3

LF251:
       jsr    LFB99                   ;6
       jmp    LF1A6                   ;3

LF257:
       lda    $CF                     ;3
       beq    LF265                   ;2
       dec    $CF                     ;5
       ldx    #$05                    ;2
       jsr    LF783                   ;6
       jmp    LF28C                   ;3

LF265:
       lda    $AA                     ;3
       bne    LF273                   ;2
       lda    #$0A                    ;2
       sta    $84                     ;3
       lda    #$00                    ;2
       sta    $83                     ;3
       bpl    LF2AB                   ;2 always branch

LF273:
       lda    $AD                     ;3
       bmi    LF286                   ;2
       tay                            ;2
       lda    LFA40,Y                 ;4
       sta    $83                     ;3
       lda    LFA42,Y                 ;4
       asl                            ;2
       sta    $84                     ;3
       jmp    LF28C                   ;3

LF286:
       lda    #$00                    ;2
       sta    $83                     ;3
       sta    $84                     ;3
LF28C:
       lda    $D1                     ;3
       bpl    LF292                   ;2
       bmi    LF2AB                   ;2 always branch

LF292:
       lda    $82                     ;3
       and    #$3F                    ;2
       bne    LF2AB                   ;2
       lda    $D6                     ;3
       and    #$01                    ;2
       bne    LF2AB                   ;2
       lda    $B5                     ;3
       beq    LF2AB                   ;2
       sed                            ;2
       lda    $B5                     ;3
       sec                            ;2
       sbc    #$01                    ;2
       sta    $B5                     ;3
       cld                            ;2
LF2AB:
       lda    $83                     ;3
       ldx    #$AB                    ;2
       ldy    #$00                    ;2
       jsr    LF30B                   ;6
       lda    $89                     ;3
       sta    $83                     ;3
       lda    $AA                     ;3
       bne    LF2CE                   ;2
       lda    $AC                     ;3
       cmp    #$75                    ;2
       bne    LF2CE                   ;2
       lda    #$00                    ;2
       sta    $84                     ;3
       lda    $D6                     ;3
       and    #$01                    ;2
       bne    LF2CE                   ;2
       inc    $D6                     ;5
LF2CE:
       lda    $84                     ;3
       ldx    #$AC                    ;2
       ldy    #$02                    ;2
       jsr    LF30B                   ;6
       lda    $89                     ;3
       sta    $84                     ;3
       ldx    #$05                    ;2
LF2DD:
       lda    $A5,X                   ;4
       beq    LF2E4                   ;2
       jsr    LF2E8                   ;6
LF2E4:
       dex                            ;2
       bpl    LF2DD                   ;2
       rts                            ;6

LF2E8:
       lda    $99,X                   ;4
       clc                            ;2
       adc    $8B                     ;3
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       eor    #$04                    ;2
       sec                            ;2
       sbc    #$04                    ;2
       clc                            ;2
       adc    $93,X                   ;4
       cmp    #$A3                    ;2
       bcc    LF302                   ;2
       lda    #$04                    ;2
       bpl    LF308                   ;2 always branch

LF302:
       cmp    #$03                    ;2
       bcs    LF308                   ;2
       lda    #$A2                    ;2
LF308:
       sta    $93,X                   ;4
       rts                            ;6

LF30B:
       sta    $89                     ;3
       clc                            ;2
       adc    $8B                     ;3
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       eor    #$04                    ;2
       sec                            ;2
       sbc    #$04                    ;2
       clc                            ;2
       adc    $00,X                   ;4
       cmp    LFA4A,Y                 ;4
       beq    LF32D                   ;2
       bcc    LF32D                   ;2
       jsr    LF33B                   ;6
       lda    LFA4A,Y                 ;4
       jmp    LF338                   ;3

LF32D:
       cmp    LFA4A+1,Y               ;4
       bcs    LF338                   ;2
       jsr    LF33B                   ;6
       lda    LFA4A+1,Y               ;4
LF338:
       sta    $00,X                   ;4
       rts                            ;6

LF33B:
       lda    $89                     ;3
       eor    #$FF                    ;2
       clc                            ;2
       adc    #$01                    ;2
       sta    $89                     ;3
       lda    $D9                     ;3
       bne    LF35A                   ;2
       lda    LFA52,Y                 ;4
       sta    $CF                     ;3
       stx    $BD                     ;3
       sty    $BE                     ;3
       ldx    #$03                    ;2
       jsr    LF6C9                   ;6
       ldx    $BD                     ;3
       ldy    $BE                     ;3
LF35A:
       rts                            ;6

LF35B:
       lda    $D0                     ;3
       beq    LF364                   ;2
       dec    $D0                     ;5
       jmp    LF3DE                   ;3 could use BPL?

LF364:
       ldx    $C7                     ;3
       beq    LF3DE                   ;2
       dex                            ;2
       cpx    $B1                     ;3
       bne    LF397                   ;2
       lda    #$C8                    ;2
       sta    $D9                     ;3
       lda    #$80                    ;2
       sta    $B1                     ;3
       lda    #$05                    ;2
       sta    $99,X                   ;4
       lda    #$A0                    ;2
       sta    $93,X                   ;4
       lda    #$00                    ;2
       sta    $CF                     ;3
       lda    $D1                     ;3
       bmi    LF38F                   ;2
       lda    $B6                     ;3
       cmp    #$09                    ;2
       bcc    LF38F                   ;2
       sbc    #$04                    ;2
       sta    $B6                     ;3
LF38F:
       ldx    #$01                    ;2
       jsr    LF6C9                   ;6
       jmp    LF3DE                   ;3

LF397:
       inx                            ;2
       ldy    #$01                    ;2
LF39A:
       lda.wy $CC,Y                   ;4
       cmp    $C7                     ;3
       bne    LF3AE                   ;2
       lda    #$00                    ;2
       sta.wy $CC,Y                   ;5
       sta.wy $D3,Y                   ;5
       dec    $CE                     ;5
       jmp    LF3B3                   ;3

LF3AE:
       dey                            ;2
       bpl    LF39A                   ;2
       bmi    LF3F3                   ;2 always branch

LF3B3:
       dec    $A4,X                   ;6
       lda    $A4,X                   ;4
       bne    LF3C2                   ;2
       lda    #$00                    ;2
       sta    $98,X                   ;4
;       sta    $92,X                   ;4
;       jmp    LF3CA                   ;3 could use BEQ to previous line to reuse STA
;;since Y is rewritten by sub LF6C9...could also use that register for storing the other
       beq    LF3C8                   ;2

LF3C2:
       lda    #$05                    ;2
       sta    $98,X                   ;4
       lda    #$A0                    ;2
LF3C8:
       sta    $92,X                   ;4
LF3CA:
       ldx    #$02                    ;2
       jsr    LF6C9                   ;6
       sed                            ;2
       lda    $81                     ;3
       clc                            ;2
       adc    #$01                    ;2
       sta    $81                     ;3
       lda    $80                     ;3
       adc    #$00                    ;2
       sta    $80                     ;3
       cld                            ;2
LF3DE:
       sta    CXCLR                   ;3
       ldy    #$04                    ;2
LF3E2:
       lda.wy $A5,Y                   ;4
       bne    LF3F2                   ;2
       dey                            ;2
       bpl    LF3E2                   ;2
       lda    #$00                    ;2
       sta    $AA                     ;3
       sta    $9E                     ;3
       sta    $98                     ;3
LF3F2:
       rts                            ;6

LF3F3:
       lda    $D9                     ;3
;       beq    LF3FA                   ;2 ?? why not use BNE to LF3DE
;       jmp    LF3DE                   ;3
       bne    LF3DE                   ;2
;LF3FA:
       ldy    #$00                    ;2
       lda    $AB                     ;3
       sec                            ;2
       sbc    $92,X                   ;4
       bpl    LF408                   ;2
       lda    $92,X                   ;4
       sec                            ;2
       sbc    $AB                     ;3
LF408:
       jsr    LF42E                   ;6
       ldy    #$01                    ;2
       lda    $AC                     ;3
       sec                            ;2
       sbc    LFA33,X                 ;4
       bpl    LF41B                   ;2
       lda    LFA33,X                 ;4
       sec                            ;2
       sbc    $AC                     ;3
LF41B:
       jsr    LF42E                   ;6
       ldx    #$04                    ;2
       jsr    LF6C9                   ;6
       lda    #$3C                    ;2
       sta    $CF                     ;3
       lda    #$0A                    ;2
       sta    $D0                     ;3
       sta    CXCLR                   ;3
       rts                            ;6

LF42E:
       cmp    LFEFC,Y                 ;4
       bcc    LF43B                   ;2
       lda    #$D8                    ;2
;       sta.wy $83,Y                   ;5
;       jmp    LF440                   ;3 could use BNE to previous line to reuse STA
       bne    LF43E                   ;2

LF43B:
       lda    #$28                    ;2
LF43E:
       sta.wy $83,Y                   ;5
;;LF440:
       rts                            ;6



LF441: ;13
       lda    $D2                     ;3
       asl                            ;2
       asl                            ;2
       asl                            ;2
       eor    $D2                     ;3
       asl                            ;2
       rol    $D2                     ;5
       lda    $D2                     ;3
       rts                            ;6


LF44E:
       ldx    #$05                    ;2
       jsr    LF441                   ;6
LF453:
       lda    $93,X                   ;4
       cmp    #$04                    ;2
       beq    LF460                   ;2
       cmp    #$05                    ;2
       beq    LF460                   ;2
       jmp    LF52E                   ;3

LF460:
       txa                            ;2
       cmp    $B1                     ;3
       bne    LF469                   ;2
       lda    #$80                    ;2
       sta    $B1                     ;3
LF469:
       stx    $89                     ;3
       ldx    $DA                     ;3
;       bne    LF471                   ;2 superfluous!
       beq    LF4E4                   ;2 always branch
;LF471:
       ldy    #$00                    ;2
LF473:
       lda.wy $AE,Y                   ;4
       cmp    $89                     ;3
       bne    LF4E0                   ;2
       lda    #$80                    ;2
       sta.wy $AE,Y                   ;5
       ldx    $89                     ;3
       lda    #$45                    ;2
       sta    $93,X                   ;4
       stx    $8A                     ;3
       lda    #$00                    ;2
       sta    $89                     ;3
       ldx    #$05                    ;2
LF48D:
       lda    $A5,X                   ;4
       bne    LF493                   ;2
       inc    $89                     ;5
LF493:
       dex                            ;2
       bpl    LF48D                   ;2
       lda    $89                     ;3
       beq    LF4DB                   ;2
       cmp    #$01                    ;2
       bne    LF4A8                   ;2
       lda    $DA                     ;3
       cmp    #$03                    ;2
       bne    LF4C1                   ;2
       dec    $DA                     ;5
       bne    LF4C1                   ;2
LF4A8:
       cmp    #$02                    ;2
       bne    LF4B6                   ;2
LF4AC:
       lda    $DA                     ;3
       cmp    #$02                    ;2
       bcc    LF4C5                   ;2
       dec    $DA                     ;5
       bne    LF4AC                   ;2
LF4B6:
       lda    $DA                     ;3
       cmp    #$01                    ;2
       bcc    LF4CB                   ;2
       dec    $DA                     ;5
       jmp    LF4B6                   ;3

LF4C1:
       lda    $B0                     ;3
       bmi    LF4DB                   ;2
LF4C5:
       lda    $AF                     ;3
       bpl    LF4CF                   ;2
       bmi    LF4D3                   ;2 always branch

LF4CB:
       lda    #$80                    ;2
       sta    $AF                     ;3
LF4CF:
       lda    $AF                     ;3
       sta    $AE                     ;3
LF4D3:
       lda    $B0                     ;3
       sta    $AF                     ;3
       lda    #$80                    ;2
       sta    $B0                     ;3
LF4DB:
       ldx    $8A                     ;3
       jmp    LF52E                   ;3

LF4E0:
       iny                            ;2
       dex                            ;2
       bne    LF473                   ;2
LF4E4:
       ldx    $89                     ;3
       inc    $D7                     ;5
       lda    $D7                     ;3
       and    #$1F                    ;2
       bne    LF4FE                   ;2
       stx    $B1                     ;3
       inc    $D8                     ;5
       inc    $D8                     ;5
       lda    $D8                     ;3
       cmp    #$08                    ;2
       bne    LF4FE                   ;2
       lda    #$02                    ;2
       sta    $D8                     ;3
LF4FE:
       jsr    LF441                   ;6
       and    #$87                    ;2
       clc                            ;2
       adc    $DB                     ;3
       bpl    LF50C                   ;2
       eor    #$7F                    ;2
       adc    #$01                    ;2
LF50C:
       sta    $99,X                   ;4
       cpx    $B1                     ;3
       bne    LF518                   ;2
       lda    #$00                    ;2
       sta    $9F,X                   ;4
       beq    LF52E                   ;2 always branch

LF518:
       jsr    LF441                   ;6
       and    #$3F                    ;2
       clc                            ;2
       adc    #$30                    ;2
       sta    $9F,X                   ;4
       cmp    #$45                    ;2
       beq    LF518                   ;2
       cmp    #$46                    ;2
       beq    LF518                   ;2
       cmp    #$64                    ;2
       beq    LF518                   ;2
LF52E:
       dex                            ;2
       bpl    LF532                   ;2
       rts                            ;6

LF532:
       jmp    LF453                   ;3

LF535:
       ldx    #$04                    ;2
LF537:
       lda    $93,X                   ;4
       sta    $89                     ;3
       lda    $9F,X                   ;4
       cmp    $89                     ;3
       beq    LF54F                   ;2
       ldy    $99,X                   ;4
       bpl    LF549                   ;2
       dec    $89                     ;5
       bne    LF54B                   ;2
LF549:
       inc    $89                     ;5
LF54B:
       cmp    $89                     ;3
       bne    LF59E                   ;2
LF54F:
       lda    $CE                     ;3
       cmp    #$02                    ;2
       beq    LF591                   ;2
       inx                            ;2
       cpx    $CC                     ;3
       beq    LF589                   ;2
       cpx    $CD                     ;3
       beq    LF589                   ;2
       inc    $CE                     ;5
       lda    #$00                    ;2
       sta    $98,X                   ;4
       ldy    #$01                    ;2
LF566:
       lda.wy $CC,Y                   ;4
       beq    LF56E                   ;2
       dey                            ;2
       bpl    LF566                   ;2
LF56E:
       stx    $CC,Y                   ;4
       lda    LFA59,X                 ;4
       sta.wy $D3,Y                   ;5
       txa                            ;2
       pha                            ;3
       lda    $92,X                   ;4
       clc                            ;2
       adc    #$04                    ;2
       ldx    #$C2                    ;2
       jsr    LFF00                   ;6
       lda    $C2                     ;3
       sta.wy $90,Y                   ;5
       pla                            ;4
       tax                            ;2
LF589:
       dex                            ;2
       lda    $91                     ;3
       sta    $92                     ;3
       jmp    LF59E                   ;3

LF591:
       lda    $99,X                   ;4
       eor    #$FF                    ;2
       clc                            ;2
       adc    #$01                    ;2
       sta    $99,X                   ;4
       lda    #$C8                    ;2
       sta    $9F,X                   ;4
LF59E:
       dex                            ;2
       bpl    LF537                   ;2
       rts                            ;6

LF5A2:
       ldx    #$01                    ;2
LF5A4:
       lda    $CC,X                   ;4
       bne    LF5AC                   ;2
LF5A8:
       dex                            ;2
       bpl    LF5A4                   ;2
       rts                            ;6

LF5AC:
       lda    $82                     ;3
       and    #$0F                    ;2
       bne    LF5A8                   ;2
       lda    $D3,X                   ;4
       cmp    #$40                    ;2
       bcs    LF5A8                   ;2
       asl                            ;2
       ora    $D3,X                   ;4
       sta    $D3,X                   ;4
       jmp    LF5A8                   ;3 could use BNE?

LF5C0:
       lda    $D1                     ;3
       bpl    LF5C5                   ;2
       rts                            ;6

LF5C5:
       ldx    #$01                    ;2
LF5C7:
       lda    $D3,X                   ;4
       cmp    #$40                    ;2
       bcs    LF5D1                   ;2
LF5CD:
       dex                            ;2
       bpl    LF5C7                   ;2
       rts                            ;6

LF5D1:
       lda    $82                     ;3
       and    #$0F                    ;2
       bne    LF5CD                   ;2
       stx    $8A                     ;3
       ldx    #$06                    ;2
       jsr    LF6CD                   ;6
       ldx    $8A                     ;3
       lda    $82                     ;3
       and    #$7F                    ;2
       bne    LF5CD                   ;2
       inc    $B6                     ;5
       lda    $B6                     ;3
       cmp    #$2E                    ;2
       bne    LF5CD                   ;2
       lda    #$80                    ;2
       sta    $D1                     ;3
       ldx    $CC                     ;3
       lda    #$19                    ;2
       sta    $98,X                   ;4
       ldx    $CD                     ;3
       sta    $98,X                   ;4
       lda    #$00                    ;2
       sta    $B5                     ;3
       sta    $D3                     ;3
       sta    $D4                     ;3
       sta    $CC                     ;3
       sta    $CD                     ;3
       lda    #$02                    ;2
       sta    $CE                     ;3
       lda    #$FF                    ;2
       sta    $AD                     ;3
       bne    LF5CD                   ;2 always branch

LF612:
       lda    $AA                     ;3
       bne    LF641                   ;2
       lda    $AC                     ;3
       cmp    #$75                    ;2
       bne    LF641                   ;2
       lda    $82                     ;3
       and    #$07                    ;2
       bne    LF641                   ;2
       lda    $B5                     ;3
       beq    LF642                   ;2
       ldx    #$05                    ;2
       jsr    LF6C9                   ;6
       sed                            ;2
       lda    $B5                     ;3
       sec                            ;2
       sbc    #$01                    ;2
       sta    $B5                     ;3
       lda    $81                     ;3
       clc                            ;2
       adc    #$01                    ;2
       sta    $81                     ;3
       lda    $80                     ;3
       adc    #$00                    ;2
       sta    $80                     ;3
       cld                            ;2
LF641:
       rts                            ;6

LF642:
       lda    $D6                     ;3
       and    #$01                    ;2
       bne    LF64A                   ;2
       inc    $D6                     ;5
LF64A:
       lda    $D6                     ;3
       cmp    #$1B                    ;2
       bne    LF654                   ;2
       dec    $D6                     ;5
       bne    LF656                   ;2
LF654:
       inc    $D6                     ;5
LF656:
       pla                            ;4
       pla                            ;4
       lda    #$00                    ;2
       sta    $D3                     ;3
       sta    $D4                     ;3
       sta    $C7                     ;3
       ldy    $D6                     ;3
       lda    LF9FA,Y                 ;4
       sta    $DB                     ;3
       lda    LF9FA+1,Y               ;4
       sta    $DA                     ;3
       lda    INTIM                   ;4
       sta    $D2                     ;3
       jsr    LF144                   ;6
       jmp    LF23C                   ;3

LF677:
       ldx    #$05                    ;2
LF679:
       cpx    $B1                     ;3
       beq    LF6C5                   ;2
       cpx    $AE                     ;3
       beq    LF6C5                   ;2
       cpx    $AF                     ;3
       beq    LF6C5                   ;2
       cpx    $B0                     ;3
       beq    LF6C5                   ;2
       lda    $99,X                   ;4
       bpl    LF692                   ;2
       ldy    #$00                    ;2
;;       jmp    LF694                   ;3 could use BEQ or BIT.w opcode
       beq    LF694                   ;2

LF692:
       ldy    #$01                    ;2
LF694:
       lda    $93,X                   ;4
       cmp    LFEFE,Y                 ;4
       bne    LF6C5                   ;2
       stx    $89                     ;3
       ldx    $DA                     ;3
       beq    LF6C3                   ;2
       stx    $8A                     ;3
       ldy    #$00                    ;2
LF6A5:
       lda.wy $AE,Y                   ;4
       bpl    LF6BE                   ;2
       lda    $89                     ;3
       sta.wy $AE,Y                   ;5
       tax                            ;2
       lda    #$C8                    ;2
       sta    $9F,X                   ;4
       lda    $99,X                   ;4
       bmi    LF6C5                   ;2
       lda    #$06                    ;2
       sta    $93,X                   ;4
       bne    LF6C5                   ;2 always branch

LF6BE:
       iny                            ;2
       dec    $8A                     ;5
       bne    LF6A5                   ;2
LF6C3:
       ldx    $89                     ;3
LF6C5:
       dex                            ;2
       bpl    LF679                   ;2
       rts                            ;6

LF6C9:
       ldy    #$00                    ;2
       beq    LF6CF                   ;2 always branch

LF6CD:
       ldy    #$01                    ;2
LF6CF:
       txa                            ;2
       cmp.wy $E4,Y                   ;4
       beq    LF6D7                   ;2
       bcc    LF6D8                   ;2
LF6D7:
       rts                            ;6

LF6D8:
       stx    $E4,Y                   ;4
       dex                            ;2
       txa                            ;2
       asl                            ;2
       tax                            ;2
       tya                            ;2
       asl                            ;2
       tay                            ;2
       lda    LFA60,X                 ;4
       sta.wy $E0,Y                   ;5
       lda    LFA60+1,X               ;4
       sta.wy $E1,Y                   ;5
       txa                            ;2
       asl                            ;2
       tax                            ;2
       lda    LF903,X                 ;4
       sta.wy $E6,Y                   ;5
       lda    #$00                    ;2
       sta.wy $E7,Y                   ;5
       tya                            ;2
       lsr                            ;2
       tay                            ;2
       lda    LF900,X                 ;4
       sta.wy AUDC0,Y                 ;5
       lda    LF901,X                 ;4
       sta.wy AUDF0,Y                 ;5
       lda    LF902,X                 ;4
       sta.wy AUDV0,Y                 ;5
       rts                            ;6

LF711:
       ldy    #$02                    ;2
LF713:
       sty    $89                     ;3
       lda.wy $E1,Y                   ;4
       cmp    #$FF                    ;2
       beq    LF770                   ;2
       lda.wy $E7,Y                   ;4
       clc                            ;2
       adc    #$01                    ;2
       sta.wy $E7,Y                   ;5
       cmp.wy $E6,Y                   ;4
       bne    LF743                   ;2
       lda    #$00                    ;2
       sta.wy $E7,Y                   ;5
       lda.wy $E0,Y                   ;4
       clc                            ;2
       adc    #$02                    ;2
       sta.wy $E0,Y                   ;5
       bcc    LF743                   ;2
       lda.wy $E1,Y                   ;4
       clc                            ;2
       adc    #$01                    ;2
       sta.wy $E1,Y                   ;5
LF743:
       ldy    $89                     ;3
       beq    LF759                   ;2
       ldy    #$00                    ;2
       lda    ($E2),Y                 ;5
       cmp    #$00                    ;2
       beq    LF770                   ;2
       sta    AUDF1                   ;3
       iny                            ;2
       lda    ($E2),Y                 ;5
       sta    AUDV1                   ;3
       jmp    LF768                   ;3

LF759:
       ldy    #$00                    ;2
       lda    ($E0),Y                 ;5
       cmp    #$00                    ;2
       beq    LF770                   ;2
       sta    AUDF0                   ;3
       iny                            ;2
       lda    ($E0),Y                 ;5
       sta    AUDV0                   ;3
LF768:
       ldy    $89                     ;3
       tya                            ;2
       lsr                            ;2
       tay                            ;2
;;       jmp    LF77F                   ;3 could use BPL
       bpl    LF77F                   ;2 always branch

LF770:
       ldy    $89                     ;3
       lda    #$FF                    ;2
       sta.wy $E1,Y                   ;5
       tya                            ;2
       lsr                            ;2
       tay                            ;2
       lda    #$FF                    ;2
       sta.wy $E4,Y                   ;5
LF77F:
       dey                            ;2
       bpl    LF713                   ;2
       rts                            ;6

LF783:
       cpx    $E4                     ;3
       beq    LF78A                   ;2
       bcc    LF78A                   ;2
       rts                            ;6

LF78A:
       lda    $CF                     ;3
       beq    LF79D                   ;2
       lda    $AC                     ;3
       lsr                            ;2
       lsr                            ;2
       sta    AUDF0                   ;3
       lda    #$04                    ;2
       sta    AUDC0                   ;3
       lda    #$06                    ;2
       sta    AUDV0                   ;3 could use BIT.w opcode to reuse instructions ahead
       rts                            ;6

LF79D:
       lda    #$00                    ;2
       sta    AUDV0                   ;3
       rts                            ;6

LF7A2:
       lda    $D1                     ;3
       bne    LF7CE                   ;2
       lda    $AC                     ;3
       cmp    #$28                    ;2
       bne    LF7B1                   ;2
       ldx    #$01                    ;2
       jsr    LF6C9                   ;6
LF7B1:
       lda    $AC                     ;3
       cmp    #$64                    ;2
       bcc    LF7BF                   ;2
       lda    #$01                    ;2
       sta    $D1                     ;3
       sta    $82                     ;3
       bpl    LF7C7                   ;2 always branch

LF7BF:
       lda    #$0C                    ;2
       sta    $83                     ;3
       lda    #$14                    ;2
       sta    $84                     ;3
LF7C7:
       jsr    LF842                   ;6 (note: these could all be combined...)
       jsr    LF879                   ;6 could use JMP
       rts                            ;6

LF7CE:
       cmp    #$01                    ;2
       bne    LF7E4                   ;2
       ldx    #$07                    ;2
       jsr    LF6CD                   ;6
       jsr    LFF62                   ;6
       jsr    LFFA6                   ;6
       jsr    LF842                   ;6
;       jsr    LF879                   ;6 could use JMP
;       rts                            ;6
       jmp    LF879                   ;3

LF7E4:
       cmp    #$02                    ;2
       lda    #$50                    ;2
       sta    $B4                     ;3
       jsr    LF7F4                   ;6
       jsr    LF842                   ;6
;       jsr    LF879                   ;6 could use JMP
;       rts                            ;6
       jmp    LF879                   ;3

LF7F4:
       lda    $AC                     ;3
       cmp    #$80                    ;2
       bne    LF835                   ;2
       lda    $AB                     ;3
       cmp    #$9B                    ;2
       bne    LF82C                   ;2
       lda    #$00                    ;2
       ldx    #$09                    ;2
LF804:
       sta    $CC,X                   ;4
       dex                            ;2
       bpl    LF804                   ;2
       sta    $C7                     ;3
       sta    $83                     ;3
       sta    $84                     ;3
       lda    LF804-1                 ;4 ??
       sta    $D2                     ;3
       lda    SWCHB                   ;4
       rol                            ;2
       bcc    LF828                   ;2
       ldy    #$0C                    ;2
       sty    $D6                     ;3
       lda    LF9FA,Y                 ;4
       sta    $DB                     ;3
       lda    LF9FA+1,Y               ;4
       sta    $DA                     ;3
LF828:
;       jsr    LF144                   ;6 could use JMP
;       rts                            ;6
       jmp    LF144                   ;3

LF82C:
       lda    #$00                    ;2
       sta    $84                     ;3
       lda    #$1E                    ;2
       sta    $83                     ;3
       rts                            ;6

LF835:
       lda    #$14                    ;2
       sta    $84                     ;3
       lda    #$0C                    ;2
       sta    $83                     ;3
       lda    #$01                    ;2
       sta    CTRLPF                  ;3
       rts                            ;6

LF842:
       lda    $AC                     ;3
       cmp    #$46                    ;2
       bcs    LF852                   ;2
       lda    #$B8                    ;2
       sta    $85                     ;3
       lda    #$FA                    ;2
       sta    $86                     ;3 ...could reuse STA if order ahead is shuffled
       bne    LF85E                   ;2 always branch

LF852:
       lda    #$86                    ;2
       sta    $85                     ;3
       lda    #$FA                    ;2
       sta    $86                     ;3
       lda    #$05                    ;2
       sta    NUSIZ0                  ;3
LF85E:
       lda    $83                     ;3
       ldx    #$AB                    ;2
       ldy    #$04                    ;2
       jsr    LF30B                   ;6
       lda    $89                     ;3
       sta    $83                     ;3
       lda    $84                     ;3
       ldx    #$AC                    ;2
       ldy    #$06                    ;2
       jsr    LF30B                   ;6
       lda    $89                     ;3
       sta    $84                     ;3
       rts                            ;6

LF879:
       sta    WSYNC                   ;3
       lda    $8E                     ;3
       sta    HMP0                    ;3
       and    #$0F                    ;2
       tay                            ;2
       lda    $80                     ;3
       nop                            ;2
LF885:
       dey                            ;2
       bpl    LF885                   ;2
       sta    RESP0                   ;3
LF88A:
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       rts                            ;6

       ORG $F88F

       .byte $07 ; |     XXX| $F88F
       .byte $01 ; |       X| $F890
       .byte $00 ; |        | $F891
       .byte $FF ; |XXXXXXXX| $F892
       .byte $05 ; |     X X| $F893
       .byte $03 ; |      XX| $F894
       .byte $04 ; |     X  | $F895
       .byte $FF ; |XXXXXXXX| $F896
       .byte $06 ; |     XX | $F897
       .byte $02 ; |      X | $F898
       .byte $FF ; |XXXXXXXX| $F899





LF89A: lda    $AA     ;3
       bne    LF89F   ;2
       rts            ;6

LF89F: lda    SWCHA   ;4
       cpx    #$00    ;2
       beq    LF8AD   ;2
       lsr            ;2
       lsr            ;2
       lsr            ;2
       lsr            ;2
       jmp    LF8AF   ;3
LF8AD: and    #$0F    ;2
LF8AF: tay            ;2
       lda    LF88A,Y ;4
       sta    $AD     ;3
       rts            ;6



       ORG $F8B6

       .byte $00 ; |        | $F8B6
       .byte $00 ; |        | $F8B7
       .byte $00 ; |        | $F8B8
       .byte $00 ; |        | $F8B9
       .byte $00 ; |        | $F8BA
       .byte $00 ; |        | $F8BB
       .byte $63 ; | XX   XX| $F8BC
       .byte $63 ; | XX   XX| $F8BD
       .byte $6B ; | XX X XX| $F8BE
       .byte $6B ; | XX X XX| $F8BF
       .byte $6B ; | XX X XX| $F8C0
       .byte $6B ; | XX X XX| $F8C1
       .byte $7F ; | XXXXXXX| $F8C2
       .byte $7F ; | XXXXXXX| $F8C3
       .byte $36 ; |  XX XX | $F8C4
       .byte $36 ; |  XX XX | $F8C5
       .byte $36 ; |  XX XX | $F8C6
       .byte $00 ; |        | $F8C7
       .byte $00 ; |        | $F8C8
       .byte $00 ; |        | $F8C9
       .byte $00 ; |        | $F8CA
       .byte $00 ; |        | $F8CB
       .byte $00 ; |        | $F8CC
       .byte $00 ; |        | $F8CD
       .byte $00 ; |        | $F8CE
       .byte $00 ; |        | $F8CF
       .byte $00 ; |        | $F8D0
       .byte $00 ; |        | $F8D1
       .byte $38 ; |  XXX   | $F8D2
       .byte $7C ; | XXXXX  | $F8D3
       .byte $FE ; |XXXXXXX | $F8D4
       .byte $FD ; |XXXXXX X| $F8D5
       .byte $FD ; |XXXXXX X| $F8D6
       .byte $85 ; |X    X X| $F8D7
       .byte $86 ; |X    XX | $F8D8
       .byte $48 ; | X  X   | $F8D9
       .byte $F8 ; |XXXXX   | $F8DA
       .byte $00 ; |        | $F8DB
       .byte $00 ; |        | $F8DC
       .byte $00 ; |        | $F8DD
       .byte $00 ; |        | $F8DE
       .byte $00 ; |        | $F8DF
       .byte $00 ; |        | $F8E0
       .byte $00 ; |        | $F8E1
       .byte $00 ; |        | $F8E2
       .byte $FF ; |XXXXXXXX| $F8E3
       .byte $81 ; |X      X| $F8E4
       .byte $A5 ; |X X  X X| $F8E5
       .byte $A5 ; |X X  X X| $F8E6
       .byte $A9 ; |X X X  X| $F8E7
       .byte $B1 ; |X XX   X| $F8E8
       .byte $A9 ; |X X X  X| $F8E9
       .byte $A5 ; |X X  X X| $F8EA
       .byte $A5 ; |X X  X X| $F8EB
       .byte $81 ; |X      X| $F8EC
       .byte $FF ; |XXXXXXXX| $F8ED
       .byte $00 ; |        | $F8EE
       .byte $00 ; |        | $F8EF
       .byte $00 ; |        | $F8F0
       .byte $00 ; |        | $F8F1
       .byte $00 ; |        | $F8F2
       .byte $00 ; |        | $F8F3
       .byte $00 ; |        | $F8F4
       .byte $00 ; |        | $F8F5
LF8F6:
       .byte $00 ; |        | $F8F6
LF8F7:
       .byte $00 ; |        | $F8F7
       .byte $A4 ; |X X  X  | $F8F8
       .byte $FA ; |XXXXX X | $F8F9
       .byte $E2 ; |XXX   X | $F8FA
       .byte $F8 ; |XXXXX   | $F8FB
       .byte $BB ; |X XXX XX| $F8FC
       .byte $F8 ; |XXXXX   | $F8FD
       .byte $00 ; |        | $F8FE
       .byte $00 ; |        | $F8FF
LF900:
       .byte $0C ; |    XX  | $F900
LF901:
       .byte $13 ; |   X  XX| $F901
LF902:
       .byte $04 ; |     X  | $F902
LF903:
       .byte $0E ; |    XXX | $F903
       .byte $01 ; |       X| $F904
       .byte $06 ; |     XX | $F905
       .byte $03 ; |      XX| $F906
       .byte $01 ; |       X| $F907
       .byte $01 ; |       X| $F908
       .byte $0E ; |    XXX | $F909
       .byte $0F ; |    XXXX| $F90A
       .byte $06 ; |     XX | $F90B
       .byte $01 ; |       X| $F90C
       .byte $15 ; |   X X X| $F90D
       .byte $0F ; |    XXXX| $F90E
       .byte $06 ; |     XX | $F90F
       .byte $0C ; |    XX  | $F910
       .byte $0F ; |    XXXX| $F911
       .byte $0A ; |    X X | $F912
       .byte $03 ; |      XX| $F913
       .byte $08 ; |    X   | $F914
       .byte $0A ; |    X X | $F915
       .byte $01 ; |       X| $F916
       .byte $02 ; |      X | $F917
       .byte $08 ; |    X   | $F918
       .byte $1F ; |   XXXXX| $F919
       .byte $0F ; |    XXXX| $F91A
       .byte $05 ; |     X X| $F91B
LF91C:
       .byte $05 ; |     X X| $F91C
       .byte $06 ; |     XX | $F91D
       .byte $05 ; |     X X| $F91E
       .byte $07 ; |     XXX| $F91F
       .byte $04 ; |     X  | $F920
       .byte $08 ; |    X   | $F921
       .byte $04 ; |     X  | $F922
       .byte $09 ; |    X  X| $F923
       .byte $03 ; |      XX| $F924
       .byte $0A ; |    X X | $F925
       .byte $03 ; |      XX| $F926
       .byte $0C ; |    XX  | $F927
       .byte $02 ; |      X | $F928
       .byte $0F ; |    XXXX| $F929
       .byte $02 ; |      X | $F92A
       .byte $00 ; |        | $F92B
       .byte $00 ; |        | $F92C
LF92D:
       .byte $0F ; |    XXXX| $F92D
       .byte $00 ; |        | $F92E
       .byte $00 ; |        | $F92F
LF930:
       .byte $0B ; |    X XX| $F930
       .byte $01 ; |       X| $F931
       .byte $0A ; |    X X | $F932
       .byte $01 ; |       X| $F933
       .byte $09 ; |    X  X| $F934
       .byte $01 ; |       X| $F935
       .byte $08 ; |    X   | $F936
       .byte $01 ; |       X| $F937
       .byte $07 ; |     XXX| $F938
       .byte $02 ; |      X | $F939
       .byte $06 ; |     XX | $F93A
       .byte $02 ; |      X | $F93B
       .byte $04 ; |     X  | $F93C
       .byte $02 ; |      X | $F93D
       .byte $03 ; |      XX| $F93E
       .byte $03 ; |      XX| $F93F
       .byte $11 ; |   X   X| $F940
       .byte $06 ; |     XX | $F941
       .byte $03 ; |      XX| $F942
       .byte $00 ; |        | $F943
       .byte $03 ; |      XX| $F944
       .byte $00 ; |        | $F945
       .byte $03 ; |      XX| $F946
       .byte $00 ; |        | $F947
       .byte $03 ; |      XX| $F948
       .byte $00 ; |        | $F949
       .byte $00 ; |        | $F94A
LF94B:
       .byte $13 ; |   X  XX| $F94B
       .byte $04 ; |     X  | $F94C
       .byte $11 ; |   X   X| $F94D
       .byte $04 ; |     X  | $F94E
       .byte $11 ; |   X   X| $F94F
       .byte $04 ; |     X  | $F950
       .byte $11 ; |   X   X| $F951
       .byte $04 ; |     X  | $F952
       .byte $13 ; |   X  XX| $F953
       .byte $04 ; |     X  | $F954
       .byte $11 ; |   X   X| $F955
       .byte $04 ; |     X  | $F956
       .byte $11 ; |   X   X| $F957
       .byte $04 ; |     X  | $F958
       .byte $13 ; |   X  XX| $F959
       .byte $04 ; |     X  | $F95A
       .byte $13 ; |   X  XX| $F95B
       .byte $04 ; |     X  | $F95C
       .byte $11 ; |   X   X| $F95D
       .byte $04 ; |     X  | $F95E
       .byte $11 ; |   X   X| $F95F
       .byte $04 ; |     X  | $F960
       .byte $11 ; |   X   X| $F961
       .byte $04 ; |     X  | $F962
       .byte $0F ; |    XXXX| $F963
       .byte $04 ; |     X  | $F964
       .byte $0E ; |    XXX | $F965
       .byte $04 ; |     X  | $F966
       .byte $0C ; |    XX  | $F967
       .byte $00 ; |        | $F968
       .byte $00 ; |        | $F969
LF96A:
       .byte $0E ; |    XXX | $F96A
       .byte $06 ; |     XX | $F96B
       .byte $0E ; |    XXX | $F96C
       .byte $00 ; |        | $F96D
       .byte $00 ; |        | $F96E
       .byte $00 ; |        | $F96F
       .byte $00 ; |        | $F970
       .byte $00 ; |        | $F971
       .byte $00 ; |        | $F972
       .byte $00 ; |        | $F973
       .byte $00 ; |        | $F974
       .byte $00 ; |        | $F975
       .byte $00 ; |        | $F976
       .byte $00 ; |        | $F977
       .byte $00 ; |        | $F978
       .byte $00 ; |        | $F979
       .byte $00 ; |        | $F97A
       .byte $00 ; |        | $F97B
       .byte $00 ; |        | $F97C
       .byte $00 ; |        | $F97D
       .byte $00 ; |        | $F97E
       .byte $00 ; |        | $F97F
       .byte $00 ; |        | $F980
       .byte $00 ; |        | $F981
       .byte $00 ; |        | $F982
       .byte $80 ; |X       | $F983
       .byte $80 ; |X       | $F984
       .byte $80 ; |X       | $F985
       .byte $80 ; |X       | $F986
       .byte $80 ; |X       | $F987
       .byte $80 ; |X       | $F988
       .byte $80 ; |X       | $F989
       .byte $80 ; |X       | $F98A
       .byte $80 ; |X       | $F98B
       .byte $80 ; |X       | $F98C
       .byte $80 ; |X       | $F98D
       .byte $80 ; |X       | $F98E
       .byte $80 ; |X       | $F98F
       .byte $80 ; |X       | $F990
       .byte $80 ; |X       | $F991
       .byte $80 ; |X       | $F992
       .byte $80 ; |X       | $F993
       .byte $80 ; |X       | $F994
       .byte $80 ; |X       | $F995
       .byte $80 ; |X       | $F996
       .byte $80 ; |X       | $F997
       .byte $80 ; |X       | $F998
       .byte $00 ; |        | $F999
       .byte $00 ; |        | $F99A
       .byte $00 ; |        | $F99B
       .byte $00 ; |        | $F99C
       .byte $00 ; |        | $F99D
       .byte $00 ; |        | $F99E
       .byte $00 ; |        | $F99F
       .byte $00 ; |        | $F9A0
       .byte $00 ; |        | $F9A1
       .byte $00 ; |        | $F9A2
       .byte $00 ; |        | $F9A3
       .byte $00 ; |        | $F9A4
       .byte $00 ; |        | $F9A5
       .byte $00 ; |        | $F9A6
       .byte $00 ; |        | $F9A7
       .byte $00 ; |        | $F9A8
       .byte $00 ; |        | $F9A9
       .byte $00 ; |        | $F9AA
       .byte $00 ; |        | $F9AB
       .byte $00 ; |        | $F9AC
       .byte $00 ; |        | $F9AD
       .byte $00 ; |        | $F9AE
       .byte $00 ; |        | $F9AF
       .byte $00 ; |        | $F9B0
       .byte $00 ; |        | $F9B1
       .byte $00 ; |        | $F9B2
       .byte $08 ; |    X   | $F9B3
       .byte $08 ; |    X   | $F9B4
       .byte $08 ; |    X   | $F9B5
       .byte $1C ; |   XXX  | $F9B6
       .byte $3E ; |  XXXXX | $F9B7
       .byte $3E ; |  XXXXX | $F9B8
       .byte $7F ; | XXXXXXX| $F9B9
       .byte $49 ; | X  X  X| $F9BA
       .byte $7F ; | XXXXXXX| $F9BB
       .byte $7F ; | XXXXXXX| $F9BC
       .byte $7F ; | XXXXXXX| $F9BD
       .byte $3E ; |  XXXXX | $F9BE
       .byte $3E ; |  XXXXX | $F9BF
       .byte $1C ; |   XXX  | $F9C0
       .byte $00 ; |        | $F9C1
       .byte $00 ; |        | $F9C2
       .byte $00 ; |        | $F9C3
       .byte $00 ; |        | $F9C4
       .byte $00 ; |        | $F9C5
       .byte $00 ; |        | $F9C6
       .byte $00 ; |        | $F9C7
       .byte $00 ; |        | $F9C8
       .byte $02 ; |      X | $F9C9
       .byte $04 ; |     X  | $F9CA
       .byte $08 ; |    X   | $F9CB
       .byte $1C ; |   XXX  | $F9CC
       .byte $3E ; |  XXXXX | $F9CD
       .byte $3E ; |  XXXXX | $F9CE
       .byte $7F ; | XXXXXXX| $F9CF
       .byte $4F ; | X  XXXX| $F9D0
       .byte $7F ; | XXXXXXX| $F9D1
       .byte $7F ; | XXXXXXX| $F9D2
       .byte $7F ; | XXXXXXX| $F9D3
       .byte $3E ; |  XXXXX | $F9D4
       .byte $3E ; |  XXXXX | $F9D5
       .byte $1C ; |   XXX  | $F9D6
       .byte $00 ; |        | $F9D7
       .byte $00 ; |        | $F9D8
       .byte $00 ; |        | $F9D9
       .byte $00 ; |        | $F9DA
       .byte $00 ; |        | $F9DB
       .byte $00 ; |        | $F9DC
       .byte $00 ; |        | $F9DD
       .byte $00 ; |        | $F9DE
       .byte $00 ; |        | $F9DF
       .byte $00 ; |        | $F9E0
       .byte $00 ; |        | $F9E1
       .byte $00 ; |        | $F9E2
       .byte $00 ; |        | $F9E3
       .byte $00 ; |        | $F9E4
       .byte $00 ; |        | $F9E5
       .byte $00 ; |        | $F9E6
       .byte $00 ; |        | $F9E7
       .byte $00 ; |        | $F9E8
       .byte $00 ; |        | $F9E9
       .byte $00 ; |        | $F9EA
       .byte $00 ; |        | $F9EB
       .byte $00 ; |        | $F9EC
       .byte $00 ; |        | $F9ED
       .byte $00 ; |        | $F9EE
       .byte $00 ; |        | $F9EF
       .byte $00 ; |        | $F9F0
       .byte $00 ; |        | $F9F1
       .byte $00 ; |        | $F9F2
       .byte $00 ; |        | $F9F3
       .byte $00 ; |        | $F9F4
       .byte $00 ; |        | $F9F5
       .byte $00 ; |        | $F9F6
       .byte $00 ; |        | $F9F7
       .byte $00 ; |        | $F9F8
       .byte $00 ; |        | $F9F9
LF9FA:
       .byte $00 ; |        | $F9FA
       .byte $00 ; |        | $F9FB
       .byte $1E ; |   XXXX | $F9FC
       .byte $01 ; |       X| $F9FD
       .byte $28 ; |  X X   | $F9FE
       .byte $01 ; |       X| $F9FF
       .byte $32 ; |  XX  X | $FA00
       .byte $02 ; |      X | $FA01
       .byte $37 ; |  XX XXX| $FA02
       .byte $02 ; |      X | $FA03
       .byte $39 ; |  XXX  X| $FA04
       .byte $02 ; |      X | $FA05
       .byte $3F ; |  XXXXXX| $FA06
       .byte $03 ; |      XX| $FA07
       .byte $3F ; |  XXXXXX| $FA08
       .byte $03 ; |      XX| $FA09
       .byte $41 ; | X     X| $FA0A
       .byte $03 ; |      XX| $FA0B
       .byte $41 ; | X     X| $FA0C
       .byte $03 ; |      XX| $FA0D
       .byte $43 ; | X    XX| $FA0E
       .byte $03 ; |      XX| $FA0F
       .byte $43 ; | X    XX| $FA10
       .byte $03 ; |      XX| $FA11
       .byte $45 ; | X   X X| $FA12
       .byte $03 ; |      XX| $FA13
       .byte $45 ; | X   X X| $FA14
       .byte $03 ; |      XX| $FA15
LFA16:
       .byte $15 ; |   X X X| $FA16
       .byte $06 ; |     XX | $FA17
       .byte $15 ; |   X X X| $FA18
       .byte $01 ; |       X| $FA19
       .byte $15 ; |   X X X| $FA1A
       .byte $00 ; |        | $FA1B
       .byte $00 ; |        | $FA1C
LFA1D:
       .byte $1F ; |   XXXXX| $FA1D
       .byte $0F ; |    XXXX| $FA1E
       .byte $1F ; |   XXXXX| $FA1F
       .byte $0E ; |    XXX | $FA20
       .byte $1F ; |   XXXXX| $FA21
       .byte $0D ; |    XX X| $FA22
       .byte $1F ; |   XXXXX| $FA23
       .byte $0A ; |    X X | $FA24
       .byte $1F ; |   XXXXX| $FA25
       .byte $09 ; |    X  X| $FA26
       .byte $1F ; |   XXXXX| $FA27
       .byte $07 ; |     XXX| $FA28
       .byte $1F ; |   XXXXX| $FA29
       .byte $05 ; |     X X| $FA2A
       .byte $1F ; |   XXXXX| $FA2B
       .byte $03 ; |      XX| $FA2C
       .byte $1F ; |   XXXXX| $FA2D
       .byte $02 ; |      X | $FA2E
       .byte $1F ; |   XXXXX| $FA2F
       .byte $01 ; |       X| $FA30
       .byte $1F ; |   XXXXX| $FA31
       .byte $00 ; |        | $FA32
LFA33:
       .byte $00 ; |        | $FA33
       .byte $17 ; |   X XXX| $FA34
       .byte $2B ; |  X X XX| $FA35
       .byte $3F ; |  XXXXXX| $FA36
       .byte $53 ; | X X  XX| $FA37
       .byte $67 ; | XX  XXX| $FA38
       .byte $7B ; | XXXX XX| $FA39
LFA3A:
       .byte $26 ; |  X  XX | $FA3A
       .byte $3A ; |  XXX X | $FA3B
       .byte $4E ; | X  XXX | $FA3C
       .byte $62 ; | XX   X | $FA3D
       .byte $76 ; | XXX XX | $FA3E
       .byte $8A ; |X   X X | $FA3F
LFA40:
       .byte $1E ; |   XXXX | $FA40
       .byte $18 ; |   XX   | $FA41
LFA42:
       .byte $00 ; |        | $FA42
       .byte $E8 ; |XXX X   | $FA43
       .byte $E2 ; |XXX   X | $FA44
       .byte $E8 ; |XXX X   | $FA45
       .byte $00 ; |        | $FA46
       .byte $18 ; |   XX   | $FA47
       .byte $1E ; |   XXXX | $FA48
       .byte $18 ; |   XX   | $FA49
LFA4A:
       .byte $96 ; |X  X XX | $FA4A
       .byte $0A ; |    X X | $FA4B
       .byte $78 ; | XXXX   | $FA4C
       .byte $0B ; |    X XX| $FA4D
       .byte $C8 ; |XX  X   | $FA4E
       .byte $00 ; |        | $FA4F
       .byte $C8 ; |XX  X   | $FA50
       .byte $00 ; |        | $FA51
LFA52:
       .byte $3C ; |  XXXX  | $FA52
       .byte $00 ; |        | $FA53
       .byte $28 ; |  X X   | $FA54
       .byte $00 ; |        | $FA55
       .byte $00 ; |        | $FA56
       .byte $00 ; |        | $FA57
       .byte $00 ; |        | $FA58
LFA59:
       .byte $00 ; |        | $FA59
       .byte $02 ; |      X | $FA5A
       .byte $04 ; |     X  | $FA5B
       .byte $08 ; |    X   | $FA5C
       .byte $10 ; |   X    | $FA5D
       .byte $20 ; |  X     | $FA5E
       .byte $00 ; |        | $FA5F

LFA60:
       .word LF94B ; $FA60-$FA61
       .word LF91C ; $FA62-$FA63
       .word LF96A ; $FA64-$FA65
       .word LFA16 ; $FA66-$FA67
       .word LF92D ; $FA68-$FA69
       .word LF930 ; $FA6A-$FA6B
       .word LFA1D ; $FA6C-$FA6D

LFA6E: ;digit lookup offsets
       .byte LF001-LF001    ; $FA6E
       .byte LF007-LF001    ; $FA6F
       .byte LF00D-LF001    ; $FA70
       .byte LF013-LF001    ; $FA71
       .byte LF019-LF001    ; $FA72
       .byte LF01F-LF001    ; $FA73
       .byte LF025-LF001    ; $FA74
       .byte LF02B-LF001    ; $FA75
       .byte LF031-LF001    ; $FA76
       .byte LF037-LF001    ; $FA77

  IF PAL
LFA78:
       .byte $8C ; | X XXX  | $FA78
  ELSE
LFA78:
       .byte $5C ; | X XXX  | $FA78
  ENDIF
       .byte $DC ; |XX XXX  | $FA79
       .byte $9C ; |X  XXX  | $FA7A
       .byte $3C ; |  XXXX  | $FA7B
       .byte $CC ; |XX  XX  | $FA7C
       .byte $EC ; |XXX XX  | $FA7D
       .byte $AC ; |X X XX  | $FA7E
       .byte $2C ; |  X XX  | $FA7F
       .byte $8C ; |X   XX  | $FA80
       .byte $6C ; | XX XX  | $FA81
       .byte $4C ; | X  XX  | $FA82
       .byte $1C ; |   XXX  | $FA83
       .byte $BC ; |X XXXX  | $FA84
       .byte $7C ; | XXXXX  | $FA85

       .byte $F8 ; |XXXXX   | $FA86
       .byte $F8 ; |XXXXX   | $FA87
       .byte $48 ; | X  X   | $FA88
       .byte $48 ; | X  X   | $FA89
       .byte $FE ; |XXXXXXX | $FA8A
       .byte $FE ; |XXXXXXX | $FA8B
       .byte $D5 ; |XX X X X| $FA8C
       .byte $D5 ; |XX X X X| $FA8D
       .byte $D5 ; |XX X X X| $FA8E
       .byte $FD ; |XXXXXX X| $FA8F
       .byte $ED ; |XXX XX X| $FA90
       .byte $FD ; |XXXXXX X| $FA91
       .byte $D6 ; |XX X XX | $FA92
       .byte $D6 ; |XX X XX | $FA93
       .byte $6C ; | XX XX  | $FA94
       .byte $6C ; | XX XX  | $FA95
       .byte $38 ; |  XXX   | $FA96
       .byte $38 ; |  XXX   | $FA97
       .byte $28 ; |  X X   | $FA98
       .byte $28 ; |  X X   | $FA99
       .byte $6C ; | XX XX  | $FA9A
       .byte $6C ; | XX XX  | $FA9B
       .byte $00 ; |        | $FA9C
       .byte $00 ; |        | $FA9D
       .byte $00 ; |        | $FA9E
       .byte $00 ; |        | $FA9F
       .byte $00 ; |        | $FAA0
       .byte $00 ; |        | $FAA1
       .byte $00 ; |        | $FAA2
       .byte $00 ; |        | $FAA3
       .byte $00 ; |        | $FAA4
       .byte $FF ; |XXXXXXXX| $FAA5
       .byte $81 ; |X      X| $FAA6
       .byte $99 ; |X  XX  X| $FAA7
       .byte $A5 ; |X X  X X| $FAA8
       .byte $A1 ; |X X    X| $FAA9
       .byte $99 ; |X  XX  X| $FAAA
       .byte $85 ; |X    X X| $FAAB
       .byte $A5 ; |X X  X X| $FAAC
       .byte $99 ; |X  XX  X| $FAAD
       .byte $81 ; |X      X| $FAAE
       .byte $FF ; |XXXXXXXX| $FAAF
       .byte $00 ; |        | $FAB0
       .byte $00 ; |        | $FAB1
       .byte $00 ; |        | $FAB2
       .byte $00 ; |        | $FAB3
       .byte $00 ; |        | $FAB4
       .byte $00 ; |        | $FAB5
       .byte $00 ; |        | $FAB6
       .byte $00 ; |        | $FAB7
       .byte $00 ; |        | $FAB8
       .byte $00 ; |        | $FAB9
       .byte $00 ; |        | $FABA
       .byte $00 ; |        | $FABB
       .byte $00 ; |        | $FABC
       .byte $00 ; |        | $FABD
       .byte $00 ; |        | $FABE
       .byte $00 ; |        | $FABF
       .byte $F8 ; |XXXXX   | $FAC0
       .byte $48 ; | X  X   | $FAC1
       .byte $44 ; | X   X  | $FAC2
       .byte $86 ; |X    XX | $FAC3
       .byte $FD ; |XXXXXX X| $FAC4
       .byte $FD ; |XXXXXX X| $FAC5
       .byte $FD ; |XXXXXX X| $FAC6
       .byte $FE ; |XXXXXXX | $FAC7
       .byte $7C ; | XXXXX  | $FAC8
       .byte $38 ; |  XXX   | $FAC9
       .byte $00 ; |        | $FACA
       .byte $00 ; |        | $FACB
       .byte $00 ; |        | $FACC
       .byte $00 ; |        | $FACD


LFACE:
       lda    $D6                     ;3
       and    #$01                    ;2
       beq    LFADC                   ;2
       lda    #$DC                    ;2
       sta    $85                     ;3
       lda    #$F9                    ;2
       bne    LFB06                   ;2 always branch

LFADC:
       lda    $CF                     ;3
       beq    LFAE8                   ;2
       lda    #$CC                    ;2
       sta    $85                     ;3
       lda    #$F8                    ;2
       bne    LFB06                   ;2 always branch

LFAE8:
       lda    $DC                     ;3
       bpl    LFAF4                   ;2
       lda    #$90                    ;2
       sta    $85                     ;3
       lda    #$FF                    ;2
       bne    LFB06                   ;2 always branch

LFAF4:
       lda    $D9                     ;3
       beq    LFB00                   ;2
       lda    #$86                    ;2
       sta    $85                     ;3
       lda    #$FA                    ;2
       bne    LFB06                   ;2 always branch

LFB00:
       lda    #$B8                    ;2
       sta    $85                     ;3
       lda    #$FA                    ;2
LFB06:
       sta    $86                     ;3
       rts                            ;6


LFB09:
       lda    #$00                    ;2
       cmp    $B1                     ;3
       bne    LFB1C                   ;2
       lda    $D8                     ;3
       tay                            ;2
       lda    LF8F6,Y                 ;4
       sta    $8C                     ;3
       lda    LF8F7,Y                 ;4
       bne    LFB46                   ;2
LFB1C:
       lda    $A5                     ;3
       bne    LFB28                   ;2
       lda    #$DC                    ;2
       sta    $8C                     ;3
       lda    #$F9                    ;2
       bne    LFB46                   ;2 always branch

LFB28:
       lda    $DC                     ;3
       bpl    LFB34                   ;2
       lda    #$80                    ;2
       sta    $8C                     ;3
       lda    #$FF                    ;2
       bne    LFB46                   ;2 always branch

LFB34:
       lda    $99                     ;3
       beq    LFB40                   ;2
       lda    #$C9                    ;2
       sta    $8C                     ;3
       lda    #$F9                    ;2
       bne    LFB46                   ;2 always branch

LFB40:
       lda    #$B3                    ;2
       sta    $8C                     ;3
       lda    #$F9                    ;2
LFB46:
       sta    $8D                     ;3
       rts                            ;6


LFB49:
       ldy    #$05                    ;2
       ldx    #$BC                    ;2
LFB4D:
       lda.wy $93,Y                   ;4
       jsr    LFF00                   ;6
       dex                            ;2
       dey                            ;2
       bpl    LFB4D                   ;2
       lda    $B7                     ;3
       sta    $8F                     ;3
       lda    $8C                     ;3
       sec                            ;2
       sbc    LFA33+1                 ;4  static rom location!
       sta    $8C                     ;3
       ldy    #$00                    ;2
       sty    $89                     ;3
       ldx    $DA                     ;3
       beq    LFB98                   ;2
LFB6B:
       lda.wy $AE,Y                   ;4
       bne    LFB76                   ;2
       lda    #$80                    ;2
       sta    $89                     ;3
;;       bne    LFB98                   ;2 always branch (why not just RTS right here??)
       rts                            ;6

LFB76:
       iny                            ;2
       dex                            ;2
       bne    LFB6B                   ;2
       rts                            ;6

LFB7B:
       ldx    #$8E                    ;2
       lda    $AB                     ;3
       jsr    LFF00                   ;6
       lda    #$83                    ;2
       sec                            ;2
       sbc    $AC                     ;3
       sta    $87                     ;3
       lda    #$F9                    ;2
       sta    $88                     ;3
       lda    $85                     ;3
       sec                            ;2
       sbc    $AC                     ;3
       sta    $85                     ;3
       bcs    LFB98                   ;2
       dec    $86                     ;5
LFB98:
       rts                            ;6

LFB99:
       lda    #$00                    ;2
       sta    WSYNC                   ;3
       sta    WSYNC                   ;3
       sta    VBLANK                  ;3
       sta    WSYNC                   ;3
       jsr    LF03D                   ;6
       ldy    #$03                    ;2
LFBB8:
       dey                            ;2
       bne    LFBB8                   ;2

  IF PAL
       lda    #$46                    ;2
  ELSE
       lda    #$36                    ;2
  ENDIF
       sta    COLUP0                  ;3
       nop                            ;2

       sta    RESP0                   ;3
       sta    RESP1                   ;3

       sta    COLUP1                  ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
;       sty.w  GRP0                    ;4
       sty    GRP0                    ;3
       sty    GRP1                    ;3
       sty    NUSIZ0                  ;3
       sty    NUSIZ1                  ;3
       lda    $D3                     ;3
       sta    $DF                     ;3
       lda    $D4                     ;3
       sta    $DE                     ;3
       ldy    #$05                    ;2
       sta    HMCLR                   ;3
       sta    WSYNC                   ;3
LFBE5:
       lda    ($C8),Y                 ;5
       sta    WSYNC                   ;3
       sta    GRP0                    ;3
       lda    ($CA),Y                 ;5
       sta    GRP1                    ;3
       dey                            ;2
       bpl    LFBE5                   ;2
       iny                            ;2
;added cxclr
       sty    CXCLR                   ;3
       sta    WSYNC                   ;3
       sty    GRP0                    ;3
       sty    GRP1                    ;3
       jsr    LFF25                   ;6
  IF PAL
       lda    #$44                    ;2
  ELSE
       lda    #$34                    ;2
  ENDIF
       sta    COLUP0                  ;3
  IF PAL
       lda    #$54                    ;2
  ELSE
       lda    #$C6                    ;2
  ENDIF
       sta    COLUP1                  ;3
       sta    COLUPF                  ;3
       lda    $D9                     ;3
       beq    LFC18                   ;2
       lda    #$15                    ;2
;       jmp    LFC1A                   ;3 could use BNE
       bne    LFC1A                   ;2 always branch



       .byte 0,0,0,0,0,0,0,0
       .byte 0,0,0,0,0,0,0,0
       .byte 0,0,0,0,0,0,0,0
       .byte 0,0,0,0,0,0,0,0


LFC18:
       lda    #$10                    ;2
LFC1A:
       sta    NUSIZ0                  ;3
       lda    $89                     ;3
       bpl    LFC25                   ;2
       lda    #$14                    ;2
;       jmp    LFC27                   ;3 could use BNE
       bne    LFC27                   ;2 always branch

LFC25:
       lda    #$10                    ;2
LFC27:
       sta    NUSIZ1                  ;3
       lda    #$15                    ;2
       sta    CTRLPF                  ;3
       lda    #$30                    ;2
       sta    PF0                     ;3
       sta    WSYNC                   ;3
       lda    $D1                     ;3
       bpl    LFC3C                   ;2
       lda    #$00                    ;2
;       jmp    LFC3E                   ;3 could use BEQ
       beq    LFC3E                   ;2 always branch

LFC3C:
       lda    #$0C                    ;2
LFC3E:
       sta    COLUBK                  ;3
       lda    #$00                    ;2
       sta    REFP1                   ;3
       sta    REFP0                   ;3
       sta    $C7                     ;3
       lda    $DC                     ;3
       bmi    LFC62                   ;2
       lda    $83                     ;3
       bmi    LFC54                   ;2
       lda    #$08                    ;2
       sta    REFP0                   ;3
LFC54:
       lda    #$00                    ;2
       cmp    $B1                     ;3
       beq    LFC62                   ;2
       lda    $99                     ;3
       bmi    LFC62                   ;2
       lda    #$08                    ;2
       sta    REFP1                   ;3
LFC62:
       ldy    #$12                    ;2
       ldx    #$00                    ;2
       sta    HMCLR                   ;3
       sta    WSYNC                   ;3
       jmp    LFC6F                   ;3 could use BEQ

LFC6D:
       sta    WSYNC                   ;3
LFC6F:
       lda    ($87),Y                 ;5
       bpl    LFCA1                   ;2
       lda    ($85),Y                 ;5
       sta    GRP0                    ;3
LFC77:
       lda    ($8C),Y                 ;5
       sta    GRP1                    ;3
       iny                            ;2
       tya                            ;2
       cmp    LFA3A,X                 ;4
       bcc    LFC6D                   ;2
       inx                            ;2
       cpx    #$06                    ;2
       beq    LFC9A                   ;2
       stx    $89                     ;3
       lda    #$10                    ;2
       sta    NUSIZ1                  ;3
       lda    $AE                     ;3
       cmp    $89                     ;3
       bne    LFCAF                   ;2
       lda    #$14                    ;2
       sta    NUSIZ1                  ;3
       jmp    LFCAF                   ;3 could use BNE

LFC9A:
       lda    $DE                     ;3
       sta    ENAM1                   ;3
       jmp    LFDC5                   ;3

LFCA1:
       lda    #$00                    ;2
       sta    GRP0                    ;3
       jmp    LFC77                   ;3 could use BEQ to previous line to reuse STA

LFCA8:
       lda    #$00                    ;2
       sta    GRP0                    ;3
       jmp    LFCB9                   ;3 could use BEQ to previous line to reuse STA

LFCAF:
       sta    WSYNC                   ;3
       lda    ($87),Y                 ;5
       bpl    LFCA8                   ;2
       lda    ($85),Y                 ;5
       sta    GRP0                    ;3
LFCB9:
       lda    $DF                     ;3
       sta    ENAM0                   ;3
       lda    $DE                     ;3
       sta    ENABL                   ;3
       iny                            ;2
       lda    $A5,X                   ;4
       bne    LFCD1                   ;2
       lda    #$F9                    ;2
       sta    $8D                     ;3
       lda    #$DC                    ;2
       sta    $8C                     ;3
       jmp    LFD0F                   ;3 could use BNE

LFCD1:
       cpx    $B1                     ;3
       beq    LFCFE                   ;2
       lda    $DC                     ;3
       bpl    LFCE4                   ;2
       lda    #$FF                    ;2
       sta    $8D                     ;3
       lda    #$80                    ;2
       sta    $8C                     ;3
       jmp    LFD0F                   ;3 could use BNE

LFCE4:
       lda    $99,X                   ;4
       beq    LFCF3                   ;2
       lda    #$F9                    ;2
       sta    $8D                     ;3
       lda    #$C9                    ;2
       sta    $8C                     ;3
       jmp    LFD0F                   ;3 could use BNE

LFCF3:
       lda    #$F9                    ;2
       sta    $8D                     ;3
       lda    #$B3                    ;2
       sta    $8C                     ;3
       jmp    LFD0F                   ;3 could use BNE

LFCFE:
       tya                            ;2
       pha                            ;3
       lda    $D8                     ;3
       tay                            ;2
       lda    LF8F7,Y                 ;4
       sta    $8D                     ;3
       lda    LF8F6,Y                 ;4
       sta    $8C                     ;3
       pla                            ;4
       tay                            ;2
LFD0F:
       sta    WSYNC                   ;3
       lda    ($87),Y                 ;5
       bpl    LFD89                   ;2
       lda    ($85),Y                 ;5
       sta    GRP0                    ;3
LFD19:
       iny                            ;2
       txa                            ;2
       pha                            ;3
       cmp    $B1                     ;3
       beq    LFD2D                   ;2
       lda    $DC                     ;3
       bmi    LFD2D                   ;2
       lda    $99,X                   ;4
       bmi    LFD2D                   ;2
       lda    #$08                    ;2
       jmp    LFD2F                   ;3 could use BNE

LFD2D:
       lda    #$00                    ;2
LFD2F:
       sta    REFP1                   ;3
       lda    CXPPMM                  ;3
       bpl    LFD37                   ;2
       stx    $C7                     ;3
LFD37:
       sta.w  CXCLR                   ;4 why waste a cycle here?
       lda    $B7,X                   ;4
       sta    $8F                     ;3
       and    #$0F                    ;2
       tax                            ;2
       sta    HMCLR                   ;3
       sta    WSYNC                   ;3
       lda    ($87),Y                 ;5
       bpl    LFD82                   ;2
       lda    ($85),Y                 ;5
       sta.w  GRP0                    ;4
LFD4E:
       dex                            ;2
       bpl    LFD4E                   ;2
       sta    RESP1                   ;3

       sta    WSYNC                   ;3
       iny                            ;2 2
       lda    ($87),Y                 ;5 7
       bpl    LFD90                   ;2 9
       lda    ($85),Y                 ;5 14
       sta    GRP0                    ;3 17
LFD5E:
       iny                            ;2 19
       tya                            ;2 21
       and    #$F0                    ;2 23
  IF PAL
       eor    #$16                    ;2 25
  ELSE
       eor    #$06                    ;2 25
  ENDIF
       sta    COLUP1                  ;3 28
       sta    COLUPF                  ;3 31
       sec                            ;2 33
  IF PAL
       sbc    #$20                    ;2 35
  ELSE
       sbc    #$10                    ;2 35
  ENDIF
       sta    COLUP0                  ;3 38
       lda    $AF                     ;3 41
       cmp    $89                     ;3 44
       bne    LFD77                   ;2 46
       lda    #$14                    ;2 48
       sta    NUSIZ1                  ;3 51
LFD77:
       lda    $8F                     ;3 54
       sta    HMP1                    ;3 57
       pla                            ;4 61
       tax                            ;2 63
       lsr    $DE                     ;5 68
       jmp    LFD98                   ;3 71

LFD82:
       lda    #$00                    ;2
       sta    GRP0                    ;3
       jmp    LFD4E                   ;3 could use BEQ

LFD89:
       lda    #$00                    ;2
       sta    GRP0                    ;3
       jmp    LFD19                   ;3 could BEQ to previous line to reuse STA

LFD90:
       lda    #$00                    ;2
       sta.w  GRP0                    ;4
       jmp    LFD5E                   ;3 could use BEQ

LFD98:
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       lda    ($87),Y                 ;5
       bpl    LFDBE                   ;2
       lda    ($85),Y                 ;5
       sta    GRP0                    ;3
LFDA4:
       lda    $8C                     ;3
       sec                            ;2
       sbc    LFA33+1,X               ;4
       sta    $8C                     ;3
       lda    $B0                     ;3
       cmp    $89                     ;3
       bne    LFDB6                   ;2
       lda    #$14                    ;2
       sta    NUSIZ1                  ;3
LFDB6:
       iny                            ;2
       lsr    $DF                     ;5
       sta    HMCLR                   ;3
       jmp    LFC6D                   ;3




LFDBE:
       lda    #$00                    ;2
       sta    GRP0                    ;3
       jmp    LFDA4                   ;3 could BEQ to previous line to reuse STA

LFDC5:
       lda    #$10                    ;2
       sta    NUSIZ1                  ;3
       sta    WSYNC                   ;3
       lda    CXPPMM                  ;3
       bpl    LFDD1                   ;2
       stx    $C7                     ;3
LFDD1:
       sta.w  CXCLR                   ;4  more odd cycle wastes...
       lda    #$00                    ;2
       sta    ENABL                   ;3
       sta.w  GRP0                    ;4  ...
       ldx    #$11                    ;2
       stx    CTRLPF                  ;3
       ldy    #$00                    ;2
  IF PAL
       lda    #$32                    ;2
  ELSE
       lda    #$E4                    ;2
  ENDIF
       sta    COLUPF                  ;3
       sta    HMCLR                   ;3
LFDE7:
       sta    WSYNC                   ;3
       cpy    #$02                    ;2
       bcc    LFDFF                   ;2
       lda    #$FF                    ;2
       sta    PF0                     ;3
       cpy    $B6                     ;3
       bne    LFDFF                   ;2
  IF PAL
       lda    #$BA                    ;2
  ELSE
       lda    #$8A                    ;2
  ENDIF
       sta    COLUBK                  ;3
       lda    #$00                    ;2
       sta    ENAM0                   ;3
       sta    ENAM1                   ;3
LFDFF:
       iny                            ;2
       cpy    #$30                    ;2 scanline count
       bne    LFDE7                   ;2
       ldy    #$02                    ;2
LFE06:
       sta    WSYNC                   ;3
       lda    #$00                    ;2
       sta    COLUBK                  ;3
       sta    GRP0                    ;3
       sta    GRP1                    ;3
       sta    PF0                     ;3
       dey                            ;2
       bne    LFE06                   ;2
       rts                            ;6

LFE16:
       lda    #$00                    ;2
       sta    WSYNC                   ;3
       sta    VBLANK                  ;3
       sta    WSYNC                   ;3
  IF PAL
       lda    #$46                    ;2
  ELSE
       lda    #$36                    ;2
  ENDIF
       sta    COLUPF                  ;3
  IF PAL
       lda    #$BC                    ;2
  ELSE
       lda    #$9C                    ;2
  ENDIF
       ldy    #$00                    ;2
       ldx    #$00                    ;2
       sta    WSYNC                   ;3
       sta    COLUBK                  ;3
       beq    LFE38                   ;2 always branch

LFE2E:
       sta    WSYNC                   ;3
       lda    ($87),Y                 ;5
       bpl    LFE4B                   ;2
       lda    ($85),Y                 ;5
       sta    GRP0                    ;3
LFE38:
       iny                            ;2
       cpy    $B4                     ;3
       bne    LFE2E                   ;2
       lda    $B4                     ;3
       clc                            ;2
       adc    #$0A                    ;2
       sta    $89                     ;3
       clc                            ;2
       adc    #$2D                    ;2
       sta    $8A                     ;3
       bne    LFE55                   ;2 always branch?

LFE4B: ;BPL destination
       stx    GRP0                    ;3
       bpl    LFE38                   ;2 always branch

LFE4F:
       lda    #$00                    ;2
       sta    GRP0                    ;3
       bpl    LFE72                   ;2 could BEQ to previous line to reuse STA

LFE55:
       sta    WSYNC                   ;3
       lda    LFEF0,X                 ;4
       sta    PF0                     ;3
       sta    PF1                     ;3
       cpx    #$04                    ;2
       bcs    LFE66                   ;2
       and    $B3                     ;3
       bcc    LFE68                   ;2 always branch

LFE66:
       and    $B2                     ;3
LFE68:
       sta    PF2                     ;3
       lda    ($87),Y                 ;5
       bpl    LFE4F                   ;2
       lda    ($85),Y                 ;5
       sta    GRP0                    ;3
LFE72:
       inx                            ;2
       iny                            ;2
       cpy    $89                     ;3
       bne    LFE55                   ;2
       sta    WSYNC                   ;3
       sta    WSYNC                   ;3
       ldx    #$00                    ;2
LFE7E:
       sta    WSYNC                   ;3
  IF PAL
       lda    #$4A                    ;2
  ELSE
       lda    #$3A                    ;2
  ENDIF
       sta    COLUPF                  ;3
       lda    ($87),Y                 ;5
       bpl    LFEB6                   ;2
       lda    ($85),Y                 ;5
       sta    GRP0                    ;3
LFE8C:
       lda    $BD,X                   ;4
       sta    PF2                     ;3
       inx                            ;2
       iny                            ;2
       sta    WSYNC                   ;3
       lda    ($87),Y                 ;5
       bpl    LFEBC                   ;2
       lda    ($85),Y                 ;5
       sta    GRP0                    ;3
LFE9C:
       cpy    #$75                    ;2
       bcc    LFEA4                   ;2
  IF PAL
       lda    #$56                    ;2
  ELSE
       lda    #$D8                    ;2
  ENDIF
       sta    COLUBK                  ;3
LFEA4:
       iny                            ;2
       sta    WSYNC                   ;3
       lda    ($87),Y                 ;5
       bpl    LFEC2                   ;2
       lda    ($85),Y                 ;5
       sta    GRP0                    ;3
LFEAF:
       iny                            ;2
       cpy    $8A                     ;3
       bcc    LFE7E                   ;2
       bcs    LFECE                   ;2 always branch

LFEB6:
       lda    #$00                    ;2
       sta    GRP0                    ;3
       beq    LFE8C                   ;2 could BEQ to previous line to reuse STA

LFEBC:
       lda    #$00                    ;2
       sta    GRP0                    ;3
       beq    LFE9C                   ;2 could BEQ to previous line to reuse STA

LFEC2:
       lda    #$00                    ;2
       sta    GRP0                    ;3
       beq    LFEAF                   ;2 could BEQ to previous line to reuse STA

LFEC8:
       lda    #$00                    ;2
       sta    GRP0                    ;3
       beq    LFED8                   ;2 could BEQ to previous line to reuse STA

LFECE:
       sta    WSYNC                   ;3
       lda    ($87),Y                 ;5
       bpl    LFEC8                   ;2
       lda    ($85),Y                 ;5
       sta    GRP0                    ;3
LFED8:
       lda    #$00                    ;2
       sta    PF0                     ;3
       sta    PF1                     ;3
       sta    PF2                     ;3
       iny                            ;2
       cpy    #$C3                    ;2
       bne    LFECE                   ;2
       sta    WSYNC                   ;3
       lda    #$00                    ;2
       sta    GRP0                    ;3
       sta    COLUBK                  ;3
       sta    WSYNC                   ;3
       rts                            ;6


       ORG $FEF0

LFEF0:
       .byte $FF ; |XXXXXXXX| $FEF0
       .byte $FF ; |XXXXXXXX| $FEF1
       .byte $41 ; | X     X| $FEF2
       .byte $62 ; | XX   X | $FEF3
       .byte $54 ; | X X X  | $FEF4
       .byte $48 ; | X  X   | $FEF5
       .byte $54 ; | X X X  | $FEF6
       .byte $62 ; | XX   X | $FEF7
       .byte $41 ; | X     X| $FEF8
       .byte $FF ; |XXXXXXXX| $FEF9
       .byte $FF ; |XXXXXXXX| $FEFA
       .byte $FF ; |XXXXXXXX| $FEFB
LFEFC:
       .byte $02 ; |      X | $FEFC
       .byte $09 ; |    X  X| $FEFD
LFEFE:
       .byte $64 ; | XX  X  | $FEFE
       .byte $45 ; | X   X X| $FEFF


LFF00:
       sec                            ;2
       sbc    #$03                    ;2
       pha                            ;3
       and    #$0F                    ;2
       sta    $89                     ;3
       pla                            ;4
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       sta    $8A                     ;3
       clc                            ;2
       adc    $89                     ;3
       cmp    #$0F                    ;2
       bcc    LFF1A                   ;2
       sbc    #$0F                    ;2
       inc    $8A                     ;5
LFF1A:
       eor    #$07                    ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       ora    $8A                     ;3
       sta    $00,X                   ;4
       rts                            ;6

LFF25:
       ldx    #$02                    ;2
LFF27:
       sta    WSYNC                   ;3
       lda    $90,X                   ;4
       sta    HMM0,X                  ;4
       and    #$0F                    ;2
       tay                            ;2
       lda    $80                     ;3
LFF32:
       dey                            ;2
       bpl    LFF32                   ;2
       sta    RESM0,X                 ;4
       dex                            ;2
       bpl    LFF27                   ;2
       inx                            ;2 could place this on next scanline...
       sta    WSYNC                   ;3
       lda    $8F,X                   ;4
       sta    HMP1                    ;3
       and    #$0F                    ;2
       tay                            ;2
       lda    $80                     ;3
       nop                            ;2 ...to eliminate this
LFF47:
       dey                            ;2
       bpl    LFF47                   ;2
       sta    RESP1                   ;3
       sta    WSYNC                   ;3
       lda    $8E                     ;3
       sta    HMP0                    ;3
       and    #$0F                    ;2
       tay                            ;2
       lda    $80                     ;3
       nop                            ;2
LFF58:
       dey                            ;2
       bpl    LFF58                   ;2
       sta    RESP0                   ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       rts                            ;6

LFF62:
       lda    $D3                     ;3
       bpl    LFF7A                   ;2
       lda    $82                     ;3
       and    #$01                    ;2
       beq    LFF73                   ;2
       lda    $B4                     ;3
       sec                            ;2
       sbc    #$05                    ;2
       bne    LFF78                   ;2
LFF73:
       lda    $B4                     ;3
       clc                            ;2
       adc    #$05                    ;2
LFF78:
       sta    $B4                     ;3
LFF7A:
       rts                            ;6


       ORG $FF7B

       .byte $00 ; |        | $FF7B
       .byte $00 ; |        | $FF7C
       .byte $00 ; |        | $FF7D
       .byte $00 ; |        | $FF7E
       .byte $00 ; |        | $FF7F
       .byte $FF ; |XXXXXXXX| $FF80
       .byte $81 ; |X      X| $FF81
       .byte $BB ; |X XXX XX| $FF82
       .byte $BB ; |X XXX XX| $FF83
       .byte $BB ; |X XXX XX| $FF84
       .byte $DB ; |XX XX XX| $FF85
       .byte $EB ; |XXX X XX| $FF86
       .byte $EB ; |XXX X XX| $FF87
       .byte $AB ; |X X X XX| $FF88
       .byte $8B ; |X   X XX| $FF89
       .byte $FF ; |XXXXXXXX| $FF8A
       .byte $00 ; |        | $FF8B
       .byte $00 ; |        | $FF8C
       .byte $00 ; |        | $FF8D
       .byte $00 ; |        | $FF8E
       .byte $00 ; |        | $FF8F
       .byte $00 ; |        | $FF90
       .byte $00 ; |        | $FF91
       .byte $00 ; |        | $FF92
       .byte $00 ; |        | $FF93
       .byte $00 ; |        | $FF94
       .byte $7F ; | XXXXXXX| $FF95
       .byte $14 ; |   X X  | $FF96
       .byte $14 ; |   X X  | $FF97
       .byte $14 ; |   X X  | $FF98
       .byte $54 ; | X X X  | $FF99
       .byte $94 ; |X  X X  | $FF9A
       .byte $94 ; |X  X X  | $FF9B
       .byte $94 ; |X  X X  | $FF9C
       .byte $64 ; | XX  X  | $FF9D
       .byte $00 ; |        | $FF9E
       .byte $00 ; |        | $FF9F
       .byte $00 ; |        | $FFA0
       .byte $00 ; |        | $FFA1
       .byte $00 ; |        | $FFA2
       .byte $00 ; |        | $FFA3
       .byte $00 ; |        | $FFA4
       .byte $00 ; |        | $FFA5


       ORG $FFF8,$FF
       .byte "2009"
       .word START
       .word START
