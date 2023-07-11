;PLANET PATROL...corrections by Kurt (Nukey Shay) Howe, 3/25/2008
;Sprite positioning corrections (HMOVE's)
;Light/Dark scanline correction (LF2D9)
;Logo routine rewritten
;End-of-display bounce fixed
;Landing strip border fixed
;Score repositioning fixed
PAL60 = 0

; Disassembly of PlantPat.bin
; Disassembled Sat Mar 15 18:24:38 2008
; Using DiStella v3.0
; Command Line: C:\BIN\D3.EXE -pafscPlantPat.cfg PlantPat.bin 
; PlantPat.cfg contents:
;      ORG  F000
;      CODE F000 FD79
;      GFX  FD7A FFFF

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
RESP1   =  $11
RESM0   =  $12
RESM1   =  $13
AUDC0   =  $15
AUDC1   =  $16
AUDF0   =  $17
AUDF1   =  $18
AUDV0   =  $19
AUDV1   =  $1A
GRP0    =  $1B
GRP1    =  $1C
ENAM0   =  $1D
HMP0    =  $20
HMP1    =  $21
HMM0    =  $22
HMM1    =  $23
VDELP0  =  $25
VDELP1  =  $26
HMOVE   =  $2A
HMCLR   =  $2B
CXCLR   =  $2C
CXM0P   =  $30
CXM1P   =  $31
CXM1FB  =  $35
CXPPMM  =  $37
INPT4   =  $3C
SWCHA   =  $0280
SWACNT  =  $0281
SWCHB   =  $0282
SWBCNT  =  $0283
INTIM   =  $0284
TIM64T  =  $0296

       ORG $F000

START:
LF000: ;used for RND seed
       sei                            ;2
       cld                            ;2
       lda    #$00                    ;2
       tax                            ;2
LF005:
       sta    VSYNC,X                 ;4
       inx                            ;2
       bne    LF005                   ;2
       dex                            ;2
       txs                            ;2
       lda    #$FE                    ;2
       sta    $81                     ;3
       sta    $83                     ;3
       sta    $85                     ;3
       sta    $87                     ;3
       sta    $89                     ;3
       sta    $8B                     ;3
       sta    $B7                     ;3
       sta    $DC                     ;3
       jsr    LFCCA                   ;6
;       lda    $8C                     ;3
;       beq    LF029                   ;2
;       ldx    #$01                    ;2
;       bne    LF02B                   ;2 always branch...could use BIT.w instead
;LF029:
;       ldx    #$02                    ;2
;LF02B:
       ldx    #$02                    ;2
       lda    $8C                     ;3
       beq    LF029                   ;2
       dex                            ;2
LF029:


       stx    $CD                     ;3
       stx    $D3                     ;3
       inc    $DA                     ;5
       ldx    #$03                    ;2
       stx    $CC                     ;3
       stx    $D2                     ;3
       lda    #$60                    ;2
       sta    $AC                     ;3
;       lda    #$FF                    ;2
;       sta    $C4                     ;3
       lda    #$05                    ;2
       sta    $C5                     ;3
       sta    $CB                     ;3

       lda    #$FF                    ;2
       sta    $D1                     ;3
       sta    $D7                     ;3
       sta    $CA                     ;3
       sta    $C4                     ;3 moved
LF04D:
       lda    #$08                    ;2
       sta    AUDC0                   ;3
       lda    #$15                    ;2
       sta    AUDF0                   ;3
       lda    #$02                    ;2
       sta    AUDV0                   ;3
       lda    $B8                     ;3
       bne    LF0A5                   ;2
;       lda    #$00                    ;2 superfluous
;       ldy    #$00                    ;2 could be TAY
       ldx    #$06                    ;2
LF063:
       sta    $8E,X                   ;4
;       sty    $9A,X                   ;4
       sta    $9A,X                   ;4

       dex                            ;2
       bpl    LF063                   ;2
;       lda    #$FF                    ;2 could just reuse X
;       sta    $96                     ;3
;       sta    $98                     ;3
;       sta    $A2                     ;3
       stx    $96                     ;3
       stx    $98                     ;3
       stx    $A2                     ;3

       stx    $99                     ;3
       stx    $BB                     ;3
       stx    $B4                     ;3

       ldx    $CD                     ;3
       lda    $AA                     ;3
       cmp    #$02                    ;2
       bcs    LF088                   ;2
       cpx    #$06                    ;2
       bne    LF08E                   ;2
       ldx    #$02                    ;2
       bne    LF08E                   ;2 always branch

LF088:
       cpx    #$04                    ;2
       bne    LF08E                   ;2
       ldx    #$01                    ;2
LF08E:
       stx    $CD                     ;3
       dex                            ;2
       lda    LFFF6,X                 ;4
       sta    $B2                     ;3
       sta    $B1                     ;3
       lda    #$00                    ;2
       sta    $B3                     ;3
       sta    AUDV1                   ;3
;       lda    #$08                    ;2 could BEQ here to $F0D3
;       sta    $C8                     ;3
;       jmp    LF0D5                   ;3
       beq    LF0D3                   ;2 always branch

LF0A5:
       cmp    #$01                    ;2
       bne    LF0D5                   ;2
;       lda    #$FF                    ;2 could use Y instead...
;       sta    $A2                     ;3
;       lda    #$04                    ;2
;       sta    $A3                     ;3
;       ldx    #$06                    ;2
;       lda    #$00                    ;2 ...for INY here
;LF0B5:
;       sta    $9A,X                   ;4 ...(here)
       ldy    #$FF                    ;2
       sty    $A2                     ;3
       ldy    #$04                    ;2
       sty    $A3                     ;3
       ldx    #$06                    ;2
       iny                            ;2
LF0B5:
       sty    $9A,X                   ;4


       dex                            ;2
       bpl    LF0B5                   ;2
       lda    $AC                     ;3
       and    #$07                    ;2
       ldy    #$03                    ;2
       sty    $A4                     ;3
LF0C2:
       cmp    #$07                    ;2
       bcc    LF0C8                   ;2
       sbc    #$07                    ;2
LF0C8:
       tax                            ;2
       lda    #$50                    ;2 <power station
       sta    $9A,X                   ;4
       inx                            ;2
       inx                            ;2
       txa                            ;2
       dey                            ;2
       bne    LF0C2                   ;2
LF0D3:
       lda    #$08                    ;2
       sta    $C8                     ;3

LF0D5:
       lda    $C8                     ;3
  IF PAL60
       ora    #$B0                    ;2
  ELSE
       ora    #$90                    ;2
  ENDIF

LF0D7:
;       lda    INTIM                   ;4
;       bne    LF0D7                   ;2
;       lda    #$00                    ;2 superfluous
;       sta    WSYNC                   ;3
;       sta    VBLANK                  ;3
;       sta    WSYNC                   ;3
;       sta    COLUPF                  ;3
;       lda    #$FF                    ;2
;       sta    TIM64T                  ;4
       ldy    INTIM                   ;4
       bne    LF0D7                   ;2
       sty    WSYNC                   ;3
       sty    VBLANK                  ;3
       sty    WSYNC                   ;3
       sty    COLUPF                  ;3
;       dey                            ;2
;       sty    TIM64T                  ;4
       ldy    #$F2                    ;2
       sty    TIM64T                  ;4


       ldy    #$05                    ;2
       sty    CTRLPF                  ;3
       ldy    #$30                    ;2 mask HMOVE's
       sty    PF0                     ;3
;       sta    WSYNC                   ;3
;       lda    $C8                     ;3
;       ora    #$90                    ;2
       sta    COLUBK                  ;3


;       lda    $8C                     ;3
;       bne    LF170                   ;2
       lda    #$00                    ;2
       ldy    #$10                    ;2
       sta    HMP0                    ;3
       sty    HMP1                    ;3

       ldy    $8C                     ;3
       bne    LF170                   ;2


       ldy    #$08                    ;2
       sta    WSYNC                   ;3

;       ORG $F0F7

LF10B: ;waste 39
       dey                            ;2
       bne    LF10B                   ;2
       sta    RESP0                   ;3
       sta    RESP1                   ;3
       iny                            ;2
       sty    NUSIZ0                  ;3
       sty    NUSIZ1                  ;3
       ldx    #$1D                    ;2
;       lda    #$12                    ;2
;       sta    $D8                     ;3
;       lda    #$3A                    ;2
;       jsr    LF145                   ;6
;       lda    #$0C                    ;2
;       sta    $D8                     ;3
;       lda    $AD                     ;3
;       lsr                            ;2
;       lsr                            ;2
;       jsr    LF145                   ;6
;       lda    #$FF                    ;2
;       sta    $D8                     ;3
;       lda    #$3A                    ;2
;       jsr    LF145                   ;6
;       sta    WSYNC                   ;3
;       inx                            ;2
;       stx    GRP0                    ;3
;       stx    GRP1                    ;3
;       ldx    #$09                    ;2
;       jsr    LFD74                   ;6
;       jmp    LF258                   ;3
;LF145:
;       sta    COLUP0                  ;3
;       sta    COLUP1                  ;3
;LF149: ;display logo loop
;       sta    WSYNC                   ;3
;       sta    HMOVE                   ;3
;       lda    LFD88,X                 ;4
;       sta    GRP0                    ;3
;       lda    LFDA6,X                 ;4
;       sta    GRP1                    ;3
;       ldy    LFDE2,X                 ;4
;       lda    LFDC4,X                 ;4
;       cmp    ($80,X)                 ;6 waste 6
;       cmp    ($82,X)                 ;6 waste 6
;       cmp    ($84,X)                 ;6 waste 6
;       nop                            ;2
;       sta    GRP0                    ;3
;       sty    GRP1                    ;3
;       sta    HMCLR                   ;3
;       dex                            ;2
;       cpx    $D8                     ;3
;       bne    LF149                   ;2
;       rts                            ;6
;no need to JSR with all those cycles wasted in the center of the loop:
LF149: ;display logo loop
       lda    Logo1,X                 ;4
       ldy    Logo4,X                 ;4
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       sta    GRP0                    ;3
       lda    Logo2,X                 ;4
       sta    GRP1                    ;3
       lda    $AD                     ;3
       cpx    #$0D                    ;2

       bcc    LFxx1                   ;2
       cpx    #$14                    ;2
       bcs    LFxx2                   ;2
       lsr                            ;2
       lsr                            ;2
       bpl    LFxx3                   ;2
LFxx1:
       nop                            ;2
       nop                            ;2
LFxx2:

  IF PAL60
       lda    #$3A                    ;2
  ELSE
       lda    #$4A                    ;2
  ENDIF

       nop                            ;2
       nop                            ;2 ;13 cycles each branch
LFxx3:
       sta    COLUP0                  ;3
       sta    COLUP1                  ;3 @37
       lda    Logo3,X                 ;4
       dex                            ;2
       sta    GRP0                    ;3 @46
       sty    GRP1                    ;3
       sta    HMCLR                   ;3
       bpl    LF149                   ;2
       sta    WSYNC                   ;3
       inx                            ;2
       stx    GRP0                    ;3
       stx    GRP1                    ;3
       ldx    #$09                    ;2
       jsr    LFD74                   ;6
       jmp    LF258                   ;3




LF170: ;A=0
;       lda    $BC                     ;3
;       bne    LF178                   ;2
;       lda    #$0F                    ;2 white
;;       bne    LF17A                   ;2 always branch...could use BIT.w instead
;       .byte $2C                      ;4 skip 2 bytes
;LF178:
;       lda    #$38                    ;2 red
;;LF17A:
;;;       lda    #$00                    ;2
;;;       ldy    #$10                    ;2
;;;       sta    HMP0                    ;3
;;;       sty    HMP1                    ;3
;       sta    WSYNC                   ;3
;       ldy    #$06                    ;2
;LF190: ;waste 29
;       dey                            ;2
;       bne    LF190                   ;2
;       nop                            ;2
       sta    WSYNC                   ;3
       sta    GRP0                    ;3 clear old gfx
       ldx    #$0F                    ;2 white
       lda    $BC                     ;3
       bne    LF178                   ;2
       .byte $2C                      ;4 skip 2 bytes
LF178:

  IF PAL60
       ldx    #$48                    ;2 red
  ELSE
       ldx    #$38                    ;2 red
  ENDIF

;LF17A:
       stx    COLUP0                  ;3 save score color
       stx    COLUP1                  ;3
       lda    $F0,X                   ;3/4 @25 default color is +1 (due to BIT.w)...this fixes
       lda    #$03                    ;2
       sta    NUSIZ0                  ;3
       sta    NUSIZ1                  ;3 @33 combine both repositions in same scanline


       ldy    #$07                    ;2
       sta    RESP0                   ;3
       sta    RESP1                   ;3



       sta    WSYNC                   ;3
       sta    HMOVE                   ;3



;       ldy    #$06                    ;2
;LF19A: ;waste 29
;       dey                            ;2
;       bne    LF19A                   ;2
;       cmp    ($00),Y                 ;5 waste 5
;       sta    RESP1                   ;3
;       lda    #$10                    ;2
;       sta    HMP1                    ;3
;       ldy    #$07                    ;2
       sty    VDELP0                  ;3
       sty    VDELP1                  ;3
       dey                            ;2
LF1AB:
       sty    $AF                     ;3
       lda    ($80),Y                 ;5
       sta    $D8                     ;3
       sta    WSYNC                   ;3
       lda    ($8A),Y                 ;5
       sta    GRP0                    ;3
       lda    ($88),Y                 ;5
       sta    GRP1                    ;3
       lda    ($86),Y                 ;5
       sta    GRP0                    ;3
       lda    ($82),Y                 ;5
       tax                            ;2
       lda    ($84),Y                 ;5
       ldy    $D8                     ;3
       sta    GRP1                    ;3
       stx    GRP0                    ;3
       sty    GRP1                    ;3
       sta    GRP0                    ;3
       ldy    $AF                     ;3
       dey                            ;2
       bpl    LF1AB                   ;2
       sta    WSYNC                   ;3
;       lda    #$00                    ;2 could use Y instead
;       sta    GRP0                    ;3 ..
;       sta    GRP1                    ;3 ..
;       sta    VDELP0                  ;3 ..
;       sta    VDELP1                  ;3 ..
       iny                            ;2
       sty    GRP0                    ;3
       sty    GRP1                    ;3
       sty    VDELP0                  ;3
       sty    VDELP1                  ;3

       sta    WSYNC                   ;3
       sta    WSYNC                   ;3
       lda    $C8                     ;3
       cmp    #$08                    ;2
       beq    LF1F6                   ;2
       lda    #$1A                    ;2
       sta    $D8                     ;3
;       ldx    #>Moon                  ;2
       ldy    #<Moon                  ;2
;       lda    #$00                    ;2
;;       jmp    LF200                   ;3 could use BEQ/BPL instead
;       beq    LF200                   ;2 always branch
       lda    #<Cloud                 ;2
       bne    LF200                   ;2 always branch

LF1F6:
;       lda    #$36                    ;2 sun color


       lda    $AD                     ;3 frame counter
       asl                            ;2
       and    #$0F                    ;2
  IF PAL60
       eor    #$24                    ;2 sun color
  ELSE
       eor    #$14                    ;2 sun color
  ENDIF

       sta    $D8                     ;3
;       ldx    #>Sun                   ;2
       ldy    #<Sun                   ;2
       lda    #<Cloud                 ;2

LF200:
       ldx    #>Sun                   ;2
       stx    $DE                     ;3
       stx    $E0                     ;3
       sty    $DD                     ;3
       sta    $DF                     ;3

       lda    #$04                    ;2
       sta    NUSIZ0                  ;3
;       lda    #$06                    ;2
;       sta    NUSIZ1                  ;3
       ldx    #$06                    ;2
       stx    NUSIZ1                  ;3

       lda    #$30                    ;2
       sta    HMP0                    ;3
       lda    #$20                    ;2
       sta    HMP1                    ;3
       sta    WSYNC                   ;3
;       ldx    #$05                    ;2
       dex                            ;2

;       ORG $F203

LF21C: ;waste 24
       dex                            ;2
       bne    LF21C                   ;2
       sta    RESP0                   ;3
       ldx    #$03                    ;2
LF223: ;waste 14
       dex                            ;2
       bne    LF223                   ;2
       sta    RESP1                   ;3
       lda    #$0A                    ;2 cloud 1 color
       sta    COLUP1                  ;3
       ldy    #$0F                    ;2
LF22E:
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       lda    $D8                     ;3 sun/moon color
       sta    COLUP0                  ;3
;;       sta    HMOVE                   ;3
       lda    ($DD),Y                 ;5
       sta    GRP0                    ;3
       lda    ($DF),Y                 ;5
       sta    GRP1                    ;3
       ldx    #$02                    ;2
LF240: ;waste 9
       dex                            ;2
       bne    LF240                   ;2
       lda    #$0A                    ;2 cloud 2 color
       sta    COLUP0                  ;3
       lda    ($DF),Y                 ;5
       sta    GRP0                    ;3
       sta    HMCLR                   ;3
       dey                            ;2
       bpl    LF22E                   ;2
       sta    WSYNC                   ;3
;       lda    #$00                    ;2 could use Y instead (INY)
;       sta    GRP0                    ;3 ..
;       sta    GRP1                    ;3 ..
       iny                            ;2
       sty    GRP0                    ;3
       sty    GRP1                    ;3
LF258:
       sta    WSYNC                   ;3
       lda    $99                     ;3
       cmp    #$FF                    ;2
       beq    LF264                   ;2
       lda    #$40                    ;2
       sta    HMM1                    ;3
LF264:
       sta    WSYNC                   ;3
       sta    WSYNC                   ;3
       sta    WSYNC                   ;3
       lda    $C8                     ;3
       lsr                            ;2
       tax                            ;2
       lda    LFEB3,X                 ;4 small mountian color
       sta    COLUP0                  ;3
       lda    LFEB3,X                 ;4 large mountian color
       sta    COLUP1                  ;3
;       lda    #$06                    ;2 could use Y instead...
;       sta    NUSIZ0                  ;3
;       lda    #$05                    ;2 ...(DEY)
;       sta    NUSIZ1                  ;3
       ldy    #$06                    ;2
       sty    NUSIZ0                  ;3
       dey                            ;2
       sty    NUSIZ1                  ;3

       lda    $CB                     ;3
       sta    HMP0                    ;3
       sta    HMP1                    ;3
       and    #$0F                    ;2
       tay                            ;2
       sta    WSYNC                   ;3

;       ORG $F270

LF28B:
       dey                            ;2
       bne    LF28B                   ;2
       sta    RESP0                   ;3
       sta    RESP1                   ;3
       ldx    #$0B                    ;2
LF294: ;waste 54
       dex                            ;2
       bne    LF294                   ;2
       ldx    #$0F                    ;2
LF299:
       txa                            ;2
       lsr                            ;2
       adc    $C8                     ;3
       lsr                            ;2
       tay                            ;2
       lda    LFEA0,Y                 ;4 sky behind mountians color
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       sta    COLUBK                  ;3
;;       sta    HMOVE                   ;3
       lda    LFE68,X                 ;4
       sta    GRP0                    ;3
       lda    LFE78,X                 ;4
       sta    GRP1                    ;3
       cmp    ($80),Y                 ;5
       lda    LFE68,X                 ;4
       sta    GRP0                    ;3
       cmp    ($80),Y                 ;5
       cmp    ($80),Y                 ;5
       lda    LFE68,X                 ;4
       sta    GRP0                    ;3
       sta    HMCLR                   ;3
       dex                            ;2
       bpl    LF299                   ;2




;       lda    $C8                     ;3
;       cmp    #$04                    ;2
;       bcs    LF2D5                   ;2
;       lda    $C9                     ;3
;       beq    LF2D5                   ;2
;       lda    #$14                    ;2
;       bne    LF2D9                   ;2 always branch
;LF2D5:
;       lda    $C8                     ;3 ground brightness
;       ora    #$10                    ;2 add yellow hue
;LF2D9:
       lda    $C8                     ;3 ground brightness
       cmp    #$04                    ;2
       bcs    LF2D5                   ;2
       ldx    $C9                     ;3
       beq    LF2D5                   ;2
       lda    #$04                    ;2
LF2D5:

  IF PAL60
       ora    #$20                    ;2 add yellow"ish" hue
  ELSE
       ora    #$10                    ;2 add yellow hue
  ENDIF

LF2D9:
       sta    WSYNC                   ;3
       sta    COLUBK                  ;3
       lda    #$00                    ;2
       sta    GRP0                    ;3
       sta    GRP1                    ;3
       sta    CXCLR                   ;3
       lda    $B8                     ;3
       cmp    #$03                    ;2
       beq    LF2F6                   ;2
       cmp    #$09                    ;2
       beq    LF2F6                   ;2
       cmp    #$0A                    ;2
;       beq    LF2F6                   ;2
;       jmp    LF34D                   ;3 could use BNE instead to eliminate BEQ above
       bne    LF34D                   ;2
LF2F6:
       sta    WSYNC                   ;3
       lda    $AD                     ;3 ??
       sta    COLUBK                  ;3

  IF PAL60
       lda    #$34                    ;2 explosion color (end of wave)
  ELSE
       lda    #$E4                    ;2 explosion color (end of wave)
  ENDIF

       sta    COLUP0                  ;3
       ldy    #$0B                    ;2
       sta    WSYNC                   ;3

;       ORG $F2E6

LF304: ;waste 54
       dey                            ;2
       bne    LF304                   ;2
       sta    RESP1                   ;3
       sty    NUSIZ1                  ;3
       sty    HMP1                    ;3
       ldx    #$06                    ;2
LF30F:
       lda    $A3,X                   ;4
       sta    HMP0                    ;3
       and    #$0F                    ;2
       tay                            ;2
       sta    WSYNC                   ;3

;       ORG $F2FA

LF318:
       dey                            ;2
       bne    LF318                   ;2
       sta    RESP0                   ;3
       lda    $8E,X                   ;4
       sta    $95                     ;3 pointer to ship GFX
       clc                            ;2
       adc    #$50                    ;2 player ship color ptr (while dodging end/wave expl.)
       sta    $97                     ;3
       lda    $9A,X                   ;4
       sta    NUSIZ0                  ;3
       ldy    #$0F                    ;2
LF32C:
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       lda    LFFA0,Y                 ;4 explosion GFX
       sta    GRP0                    ;3
       lda    ($97),Y                 ;5
       sta    COLUP1                  ;3
       lda    ($95),Y                 ;5
       sta    GRP1                    ;3
       sta    HMCLR                   ;3
       dey                            ;2
       bpl    LF32C                   ;2
       dex                            ;2
       bpl    LF30F                   ;2
       sta    WSYNC                   ;3
;       lda    #$00                    ;2 could use X or Y instead
;       sta    GRP0                    ;3
;       sta    GRP1                    ;3
       iny                            ;2
       sty    GRP0                    ;3
       sty    GRP1                    ;3
LF34D:
       lda    $B8                     ;3
       beq    LF362                   ;2
       cmp    #$02                    ;2
       beq    LF35C                   ;2
       cmp    #$04                    ;2
       beq    LF362                   ;2
       jmp    LF407                   ;3 could use BNE instead

LF35C:
       sta    WSYNC                   ;3
       lda    $AD                     ;3 ground color when player hit
       sta    COLUBK                  ;3
LF362:
       ldy    #$0B                    ;2
       sta    WSYNC                   ;3

;       ORG $F347

LF366: ;waste 54
       dey                            ;2
       bne    LF366                   ;2
       sta    RESP1                   ;3
       sty    NUSIZ0                  ;3
       sty    HMP1                    ;3
       sty    $AB                     ;3
       tsx                            ;2
       stx    $D8                     ;3
       ldx    #$1E                    ;2
       txs                            ;2
       lda    #$10                    ;2
       sta    NUSIZ1                  ;3
       ldx    #$06                    ;2
LF37D:
       lda    $8E,X                   ;4
       sta    WSYNC                   ;3
       sta    $95                     ;3
       clc                            ;2
       adc    #$50                    ;2 ship shading pointer
       sta    $97                     ;3
       lda    $AB                     ;3
       eor    $99                     ;3
       and    #$FE                    ;2
       php                            ;3
       ldy    #$0F                    ;2
       lda    ($97),Y                 ;5
       sta    COLUP1                  ;3
       lda    ($95),Y                 ;5
       sta    GRP1                    ;3
       inc    $AB                     ;5
       pla                            ;4
       lda    $9A,X                   ;4
       sta    $A1                     ;3
       lda    $C8                     ;3
       bne    LF3A8                   ;2

  IF PAL60
       lda    #$20                    ;2
  ELSE
       lda    #$10                    ;2
  ENDIF

;       bne    LF3AA                   ;2 always branch
       .byte $2C                      ;4 skip 2 bytes
LF3A8:
       lda    ($A1),Y                 ;5 load enemy ship color
;LF3AA:
;;       sta    COLUP0                  ;3
       sta    WSYNC                   ;3
       sta.w  COLUP0                  ;4

       lda    $A3,X                   ;4
       sta    HMP0                    ;3
       and    #$0F                    ;2
       tay                            ;2

;       ORG $F396

LF3B5:
       dey                            ;2
       bne    LF3B5                   ;2
;;       nop                            ;2
;;       nop                            ;2
       sta    RESP0                   ;3
       lda    $AB                     ;3
       eor    $99                     ;3
       and    #$FE                    ;2
       php                            ;3
       inc    $AB                     ;5
       pla                            ;4
       ldy    #$0E                    ;2
       lda    ($97),Y                 ;5
       sta    COLUP1                  ;3
       lda    ($95),Y                 ;5
       sta    GRP1                    ;3
       ldy    #$06                    ;2
LF3D2: ;waste 29
       dey                            ;2
       bne    LF3D2                   ;2
       ldy    #$0D                    ;2
LF3D7:
       lda    ($97),Y                 ;5
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       sta    COLUP1                  ;3
;;       sta    HMOVE                   ;3
       lda    ($A1),Y                 ;5
       sta    GRP0                    ;3 save enemy ship GFX
       lda    ($95),Y                 ;5
       sta    GRP1                    ;3
       lda    $AB                     ;3
       eor    $99                     ;3
       and    #$FE                    ;2
       php                            ;3
       sta    HMCLR                   ;3
       inc    $AB                     ;5
       pla                            ;4
       dey                            ;2
       bpl    LF3D7                   ;2
       dex                            ;2
       bpl    LF37D                   ;2

       ldx    $D8                     ;3
       txs                            ;2
;       lda    #$00                    ;2 could use Y instead
;       sta    GRP0                    ;3
;       sta    GRP1                    ;3
       iny                            ;2
       sty    GRP0                    ;3
       sty    GRP1                    ;3
       sta    WSYNC                   ;3
       jmp    LF548                   ;3

LF407:
       lda    $B8                     ;3
       cmp    #$01                    ;2
       beq    LF418                   ;2
       cmp    #$08                    ;2
;       beq    LF414                   ;2
;       jmp    LF48A                   ;3 could use BNE instead to eliminate BEQ above
       bne    LF48A                   ;2
;LF414:
       lda    $AD                     ;3
       sta    COLUBK                  ;3
LF418:
       ldy    #$0B                    ;2
       sta    WSYNC                   ;3

;       ORG $F3F7

LF41C: ;waste 54
       dey                            ;2
       bne    LF41C                   ;2
       sta    RESP1                   ;3
       lda    $A3                     ;3
       sta    HMP0                    ;3
       sta    HMM0                    ;3
       and    #$0F                    ;2
       tay                            ;2
       sta    WSYNC                   ;3

;       ORG $F407

LF42C:
       dey                            ;2
       bne    LF42C                   ;2
       sta    RESP0                   ;3
       sty    NUSIZ0                  ;3
       sta    RESM0                   ;3
       sty    HMP1                    ;3
       sty    $AB                     ;3
       lda    #$10                    ;2
       sta    NUSIZ1                  ;3

  IF PAL60
       lda    #$54                    ;2
  ELSE
       lda    #$C4                    ;2
  ENDIF

       sta    COLUP0                  ;3
       tsx                            ;2
       stx    $D8                     ;3
       ldx    #$1E                    ;2
       txs                            ;2
       ldx    #$06                    ;2
LF449:
       lda    $8E,X                   ;4
       sta    $95                     ;3
       clc                            ;2
       adc    #$50                    ;2
       sta    $97                     ;3
       lda    $9A,X                   ;4
       sta    $A1                     ;3
       ldy    #$0F                    ;2
LF458:
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       lda    ($A1),Y                 ;5
       sta    GRP0                    ;3 save power station GFX
       lda    ($97),Y                 ;5
       sta    COLUP1                  ;3
       lda    ($95),Y                 ;5
       sta    GRP1                    ;3
       lda    $AB                     ;3
       sta    ENAM0                   ;3
       sta    HMCLR                   ;3
       eor    $99                     ;3
       php                            ;3
       inc    $AB                     ;5
       pla                            ;4
       dey                            ;2
       bpl    LF458                   ;2
       dex                            ;2
       bpl    LF449                   ;2
       ldx    $D8                     ;3
       txs                            ;2
       sta    WSYNC                   ;3
;       lda    #$00                    ;2 could use Y instead
;       sta    GRP0                    ;3
;       sta    GRP1                    ;3
;       sta    ENAM0                   ;3
       iny                            ;2
       sty    GRP0                    ;3
       sty    GRP1                    ;3
       sty    ENAM0                   ;3
       jmp    LF548                   ;3

LF48A:
       lda    $B8                     ;3
       cmp    #$05                    ;2
       beq    LF49F                   ;2
       cmp    #$06                    ;2
       beq    LF49F                   ;2
       cmp    #$07                    ;2
       beq    LF49F                   ;2
       cmp    #$0B                    ;2
       beq    LF49F                   ;2
       jmp    LF548                   ;3

LF49F:
       ldy    #$0B                    ;2
       sta    WSYNC                   ;3

;       ORG $F47D

LF4A3: ;waste 54
       dey                            ;2
       bne    LF4A3                   ;2
       sta    RESP0                   ;3
       sty    NUSIZ1                  ;3
       sty    NUSIZ0                  ;3
       sty    HMP0                    ;3
       lda    #$01                    ;2
       sta    CTRLPF                  ;3

  IF PAL60
       lda    #$54                    ;2
  ELSE
       lda    #$C4                    ;2
  ENDIF

       sta    COLUP1                  ;3
       lda    $C5                     ;3
       sta    HMP1                    ;3
       and    #$0F                    ;2
       tay                            ;2
       sta    WSYNC                   ;3

;       ORG $F499

LF4BF: ;reposition fuel truck
       dey                            ;2
       bne    LF4BF                   ;2
       sta    RESP1                   ;3
       ldx    #$06                    ;2
LF4C6:
       lda    #$30                    ;2
       sta    PF0                     ;3
       lda    #$00                    ;2
       sta    PF1                     ;3
       sta    PF2                     ;3
       lda    $8E,X                   ;4
       sta    $95                     ;3
       clc                            ;2
       adc    #$50                    ;2
       sta    $97                     ;3
       lda    $9A,X                   ;4 <- player color pointer (when landing)
       sta    $C3                     ;3
       cpx    $B0                     ;3
;       bne    LF4EE                   ;2
       beq    LF4E3                   ;2
;LF4EE:
;       lda    #$00                    ;2
;       sta    $A4                     ;3
;       sta    $A5                     ;3
;       sta    $A8                     ;3
;       sta    $A7                     ;3
       dec    $2E                     ;5
       dec    $2E                     ;5
       lda    #$30                    ;2
       sta    $A3                     ;3
       sta    $A6                     ;3
       lda    #$00                    ;2
       tay                            ;2
       sta    $A4                     ;3
       sta    $A5                     ;3
       beq    LF4FA                   ;2 always branch



;       ldy    #$05                    ;2
;LF4E3:
;       lda.wy $BD,Y                   ;4
;       sta.wy $A3,Y                   ;5
;       dey                            ;2
;       bpl    LF4E3                   ;2
;       bmi    LF4FE                   ;2 always branch


LF4E3:
       lda    $BD                     ;3
       sta    $A3                     ;3
       lda    $BD+1                   ;3
       sta    $A3+1                   ;3
       lda    $BD+2                   ;3
       sta    $A3+2                   ;3
       lda    $BD+3                   ;3

       ora    #$30                    ;2 fix landing strip glitch

       sta    $A3+3                   ;3
       lda    $BD+4                   ;3
       ldy    $BD+5                   ;3
LF4FA:
       sta    $A7                     ;3
       sty    $A8                     ;3
LF4FE:
       ldy    #$0F                    ;2
LF500:
       lda    ($97),Y                 ;5
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       sta    COLUP0                  ;3
       lda    $A3                     ;3
       sta    PF0                     ;3


       lda    ($C3),Y                 ;5 moved here to correct fuel truck skew
       sta    GRP1                    ;3 fuel truck gfx

       lda    $A4                     ;3
       sta    PF1                     ;3
       lda    $A5                     ;3
       sta    PF2                     ;3
;;       sta    HMOVE                   ;3 ??
       lda    ($95),Y                 ;5
       sta    GRP0                    ;3 save player ship GFX (when landing)
;;       lda    ($C3),Y                 ;5
;;       sta    GRP1                    ;3 fuel truck gfx
       sta    HMCLR                   ;3
       lda    $A8                     ;3
       sta    PF2                     ;3
       lda    $A7                     ;3
       sta    PF1                     ;3
       lda    $A6                     ;3
       sta    PF0                     ;3
       dey                            ;2 @63
       bpl    LF500                   ;2
       dex                            ;2
       bpl    LF4C6                   ;2
       sta    WSYNC                   ;3

;       lda    #$00                    ;2
;       sta    GRP0                    ;3
;       sta    GRP1                    ;3
;       sta    PF1                     ;3
;       sta    PF2                     ;3
       inx                            ;2
       stx    GRP0                    ;3
       stx    GRP1                    ;3
       stx    PF1                     ;3
       stx    PF2                     ;3

       lda    #$30                    ;2
       sta    PF0                     ;3
       ldx    #$07                    ;2
       jsr    LFD74                   ;6
;       jmp    LF599                   ;3 could use BEQ instead
       beq    LF599                   ;2 always branch

LF548:
       sta    WSYNC                   ;3
       lda    $C8                     ;3
       lsr                            ;2

  IF PAL60
       ora    #$20                    ;2
  ELSE
       ora    #$10                    ;2
  ENDIF

       sta    COLUP1                  ;3
       lda    #$00                    ;2
       sta    NUSIZ1                  ;3
       lda    $AC                     ;3
       and    #$3C                    ;2
       asl                            ;2
       asl                            ;2
       clc                            ;2
       adc    #$80                    ;2
       sta    HMP1                    ;3
       lda    $AC                     ;3
       and    #$40                    ;2
       sta    WSYNC                   ;3
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       eor    #$01                    ;2
       adc    #$08                    ;2
       tay                            ;2

;       ORG $F549

LF571:
       dey                            ;2
       bne    LF571                   ;2
       sta    RESP1                   ;3
       ldx    #$05                    ;2
LF578:
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       lda    #$00                    ;2
       sta    GRP0                    ;3
       lda    ShipShadow,X            ;4
       sta    GRP1                    ;3
       ldy    #$07                    ;2
LF587: ;waste 34
       dey                            ;2
       bne    LF587                   ;2
       sta    HMCLR                   ;3
       dex                            ;2
       bpl    LF578                   ;2
       sta    WSYNC                   ;3
;       lda    #$00                    ;2 could use X instead
;       sta    GRP0                    ;3
;       sta    GRP1                    ;3
;;       inx                            ;2 no longer needed
;;       stx    GRP0                    ;3 ..
;;       stx    GRP1                    ;3 ..

       sta    WSYNC                   ;3
LF599:
       lda    #$0F                    ;2

LF59B:
       ldx    INTIM                   ;4
       bne    LF59B                   ;2
       stx    COLUBK                  ;3
       sta    TIM64T                  ;4
       lda    $8C                     ;3
       bne    LF5A4                   ;2
       jmp    LF631                   ;3

LF5A4:
       ldy    $CC                     ;3
       bne    LF5AC                   ;2
LF5A8:
       ldx    #$00                    ;2
       beq    LF5C5                   ;2 always branch

LF5AC:
       dey                            ;2
       bne    LF5B3                   ;2
;       sty    NUSIZ0                  ;3
;       beq    LF5C3                   ;2 always branch
       tya                            ;2
       beq    LF5C1                   ;2 always branch

LF5B3:
       dey                            ;2
       bne    LF5BC                   ;2
       lda    #$01                    ;2
;       sta    NUSIZ0                  ;3
;       bne    LF5C3                   ;2 always branch
       bne    LF5C1                   ;2 always branch

LF5BC:
       dey                            ;2
       bne    LF5A8                   ;2
       lda    #$03                    ;2
LF5C1:
       sta    NUSIZ0                  ;3
LF5C3:

  IF PAL60
       ldx    #$58                    ;2
  ELSE
       ldx    #$C8                    ;2
  ENDIF

LF5C5:
       stx    COLUP0                  ;3
       lda    #$00                    ;2
       sta    PF0                     ;3
       sta    PF1                     ;3
       lda    $CA                     ;3
       cmp    #$01                    ;2
       beq    LF5D7                   ;2
       lda    #$0A                    ;2
       bne    LF5D9                   ;2 always branch

LF5D7:

  IF PAL60
       lda    #$64                    ;2
  ELSE
       lda    #$44                    ;2
  ENDIF

LF5D9:
       sta    COLUPF                  ;3
       sta    WSYNC                   ;3
       ldy    #$04                    ;2

;       ORG $F5B3

LF5DF: ;waste 19
       dey                            ;2
       bne    LF5DF                   ;2
       sta    RESP0                   ;3
       ldy    #$06                    ;2
LF5E6: ;waste 29
       dey                            ;2
       bne    LF5E6                   ;2
       sta    RESP1                   ;3
;       ldx    #$01                    ;2 could use Y instead
;       stx    CTRLPF                  ;3 ..
;       inx                            ;2 ..
;       stx    NUSIZ1                  ;3 ..
       iny                            ;2
       sty    CTRLPF                  ;3
       iny                            ;2
       sty    NUSIZ1                  ;3


       lda    #$06                    ;2
       sta    COLUP0                  ;3

  IF PAL60
       lda    #$56                    ;2
  ELSE
       lda    #$C6                    ;2
  ENDIF

       sta    COLUP1                  ;3
;       lda    #$40                    ;2 set fine positioning for "E" and "F" fuel indicators
       lda    #$30                    ;2 move 1 pixel further right
       sta    HMP1                    ;3
       ldx    #$07                    ;2
LF5FC:
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       lda    #$00                    ;2
       sta    PF1                     ;3
;;       sta    HMOVE                   ;3 ??
       lda    LFE98,X                 ;4 extra ships
       sta    GRP0                    ;3
       lda    LFEB9,X                 ;4 "E"
       sta    GRP1                    ;3

;       ldy    #$02                    ;2
;LF610: ;waste 9
;       dey                            ;2
;       bne    LF610                   ;2
;       lda    $CA                     ;3
;       sta    PF1                     ;3
;       cmp    ($80,X)                 ;6
;       cmp    ($82,X)                 ;6
;       nop                            ;2
;       nop                            ;2
;       lda    LFEC1,X                 ;4
       lda    LFEC1,X                 ;4 "F"
       ldy    $CA                     ;3

;;       dec    $2E                     ;5
       sta    HMCLR                   ;3
       nop                            ;2
       nop                            ;2



       sty    PF1                     ;3
       cmp    ($80,X)                 ;6
       cmp    ($80,X)                 ;6
       cmp    ($82,X)                 ;6
;;       nop                            ;2
       dex                            ;2

       sta    GRP1                    ;3
;;       sta    HMCLR                   ;3
;;       dex                            ;2
       bpl    LF5FC                   ;2
       sta    WSYNC                   ;3
;       lda    #$00                    ;2 could use X instead
;       sta    GRP0                    ;3 ..
;       sta    GRP1                    ;3 ..
;       sta    PF1                     ;3 ..
       inx                            ;2
       stx    GRP0                    ;3 ..
       stx    GRP1                    ;3 ..
       stx    PF1                     ;3 ..




LF631:
       ldx    INTIM                   ;4
       bne    LF631                   ;2
       lda    #$04                    ;2 ??
       sta    TIM64T                  ;4



       lda    #$82                    ;2

LF63B:
;       lda    INTIM                   ;4
;       bne    LF63B                   ;2
       ldx    INTIM                   ;4
       bne    LF63B                   ;2

       sta    WSYNC                   ;3
       sta    VBLANK                  ;3
       sta    VSYNC                   ;3
       sta    WSYNC                   ;3
       sta    WSYNC                   ;3
       sta    WSYNC                   ;3
;       lda    #$00                    ;2 could reuse loop register above
;       sta    VSYNC                   ;3
;       sta    WSYNC                   ;3
       stx    VSYNC                   ;3


;;       stx    WSYNC                   ;3 removed

       lda    #$30                    ;2
       sta    TIM64T                  ;4
       lda    $D9                     ;3
       beq    LF660                   ;2
       jmp    LF9D5                   ;3

LF660:
;       lda    $B8                     ;3 could use X...
;       beq    LF67F                   ;2
;       cmp    #$01                    ;2 ...to use DEX's instead
;       beq    LF67F                   ;2
;       cmp    #$03                    ;2 ..
;       beq    LF67F                   ;2
;       cmp    #$04                    ;2 ..
;       beq    LF67F                   ;2
;       cmp    #$05                    ;2 ..
;       beq    LF67F                   ;2
;       cmp    #$07                    ;2 ..
;       beq    LF67F                   ;2
;       cmp    #$09                    ;2 ..
       ldx    $B8                     ;3
       beq    LF67F                   ;2
       dex                            ;2 1
       beq    LF67F                   ;2
       dex                            ;2
       dex                            ;2 3
       beq    LF67F                   ;2
       dex                            ;2 4
       beq    LF67F                   ;2
       dex                            ;2 5
       beq    LF67F                   ;2
       dex                            ;2
       dex                            ;2 7
       beq    LF67F                   ;2
       dex                            ;2
       dex                            ;2 9


       beq    LF67F                   ;2
       jmp    LF723                   ;3

LF67F:
       lda    #$02                    ;2
       sta    AUDV0                   ;3
       lda    $8C                     ;3
       beq    LF6BD                   ;2
       lda    $C1                     ;3
       bne    LF6DB                   ;2
       sta    SWACNT                  ;4
       lda    SWCHA                   ;4
       ldx    $BC                     ;3
       bne    LF699                   ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
LF699:
       lsr                            ;2
       bcs    LF6AA                   ;2
       ldy    $AC                     ;3
       beq    LF6DB                   ;2
       dey                            ;2
       sty    $AC                     ;3
       lda    #$05                    ;2
       sta    AUDV0                   ;3
;       jmp    LF6DB                   ;3 could use BNE/BPL instead
       bne    LF6DB                   ;2 always branch

LF6AA:
       lsr                            ;2
       bcs    LF6DB                   ;2
       ldy    $AC                     ;3
       cpy    #$60                    ;2
       beq    LF6DB                   ;2
       iny                            ;2
       sty    $AC                     ;3
       lda    #$00                    ;2
       sta    AUDV0                   ;3
;       jmp    LF6DB                   ;3
       beq    LF6DB                   ;2 always branch

LF6BD:
       lda    $AC                     ;3
       bne    LF6C8                   ;2
       ldx    #$60                    ;2
;       stx    $8D                     ;3
;       jmp    LF6D0                   ;3 could use BNE/BPL instead
       bne    LF6CE                   ;2 always branch

LF6C8:
       cmp    #$60                    ;2
       bne    LF6D0                   ;2
       ldx    #$00                    ;2
LF6CE:
       stx    $8D                     ;3
LF6D0:
       cmp    $8D                     ;3
       bcc    LF6D9                   ;2
       dec    $AC                     ;5
;       jmp    LF6DB                   ;3 could use BIT.w instead
       .byte $2C                      ;4 skip 2 bytes
LF6D9:
       inc    $AC                     ;5
LF6DB:
       lda    #$00                    ;2
       ldx    #$06                    ;2
LF6DF:
       sta    $8E,X                   ;4
       dex                            ;2
       bpl    LF6DF                   ;2
       lda    $AC                     ;3
       and    #$F0                    ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       sta    $D8                     ;3
       lda    #$06                    ;2
       sec                            ;2
       sbc    $D8                     ;3
       tax                            ;2
       lda    $AC                     ;3
       and    #$0F                    ;2
       clc                            ;2
       adc    #$10                    ;2
       ldy    #$00                    ;2
       sty    SWBCNT                  ;4
       ldy    $BC                     ;3
       beq    LF70B                   ;2
       bit    SWCHB                   ;4
       bpl    LF715                   ;2
       bmi    LF710                   ;2 always branch

LF70B:
;       bit    SWCHB                   ;4
;       bvc    LF715                   ;2
;LF710:
;       adc    #<LFF00                 ;2
;;       jmp    LF717                   ;3 could use BIT.w instead
;       .byte $2C                      ;4 skip 2 bytes
;LF715:
;       adc    #<LFF20                 ;2
;;LF717:


       bit    SWCHB                   ;4
       bvs    LF717                   ;2
LF715:
       clc                            ;2
       adc    #<LFF20                 ;2
LF710:
LF717:




       sta    $8E,X                   ;4
       cpx    #$00                    ;2
       beq    LF723                   ;2
       dex                            ;2
       sec                            ;2
       sbc    #$10                    ;2
       sta    $8E,X                   ;4
LF723:
       lda    $B8                     ;3
       cmp    #$09                    ;2
       bne    LF737                   ;2
       bit    CXPPMM                  ;3
       bpl    LF737                   ;2
       lda    #$0A                    ;2
       sta    $B8                     ;3
       lda    #$00                    ;2
       sta    $B9                     ;3
       sta    $AD                     ;3
LF737:
       lda    $B8                     ;3
       cmp    #$01                    ;2
       beq    LF740                   ;2
       jmp    LF7D4                   ;3

LF740:
       lda    $AD                     ;3
       ldx    $CD                     ;3
       cpx    #$03                    ;2
       bcs    LF74D                   ;2
       and    #$03                    ;2
;       jmp    LF74F                   ;3 could use BPL instead
       bpl    LF74F                   ;2 always branch

LF74D:
       and    #$01                    ;2
LF74F:
       bne    LF758                   ;2
       lda    $A3                     ;3
       jsr    LFC84                   ;6
       sta    $A3                     ;3
LF758:
       bit    CXM0P                   ;3
       bpl    LF76B                   ;2
       lda    #$08                    ;2
       sta    $B8                     ;3
       lda    #$00                    ;2
       sta    $B9                     ;3
       sta    $AD                     ;3
       sta    $CC                     ;3
;       jmp    LF7B8                   ;3 could use BEQ instead
       beq    LF7B8                   ;2 always branch

LF76B:
       bit    CXM1P                   ;3
       bpl    LF7C9                   ;2
       jsr    LFD4A                   ;6
       lda    $99                     ;3
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       sta    $D8                     ;3
       lda    #$06                    ;2
       sec                            ;2
       sbc    $D8                     ;3
       tax                            ;2
       lda    #$00                    ;2
       sta    $9A,X                   ;4
       lda    #$FF                    ;2
       sta    $99                     ;3
       dec    $A4                     ;5
       bne    LF7D1                   ;2
       ldy    $AD                     ;3
       ldx    #$06                    ;2
LF790:
       lda    LF000,Y                 ;4 ??
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       and    #$07                    ;2
       sta    $9A,X                   ;4
       and    #$03                    ;2
       adc    #$04                    ;2
       sta    $A3,X                   ;4
       iny                            ;2
       iny                            ;2
       iny                            ;2
       dex                            ;2
       bpl    LF790                   ;2
;       lda    #$04                    ;2 could use X instead...
;       sta    $A3                     ;3
;       lda    #$03                    ;2 ...for DEX
;       sta    $B8                     ;3
;       lda    #$00                    ;2
;       sta    $AD                     ;3
       inx                            ;2
       stx    $AD                     ;3
       ldx    #$04                    ;2
       stx    $A3                     ;3
       dex                            ;2
       stx    $B8                     ;3


       ldx    #$06                    ;2
       jsr    LFC91                   ;6
LF7B8:
       ldx    #$FF                    ;2
       stx    $C6                     ;3
;       stx    $99                     ;3
       lda    #$08                    ;2
       sta    AUDC0                   ;3
       lda    #$1F                    ;2
       sta    AUDF0                   ;3
;       jmp    LF7D1                   ;3 could use BNE/BPL instead
       bne    LF7CF                   ;2 always branch

LF7C9:
       bit    CXM1FB                  ;3
       bpl    LF7D1                   ;2
;       lda    #$FF                    ;2
;       sta    $99                     ;3
       ldx    #$FF                    ;2
LF7CF:
       stx    $99                     ;3
LF7D1:
       jmp    LF93D                   ;3

LF7D4:
       lda    $B8                     ;3
       beq    LF7DB                   ;2
       jmp    LF93D                   ;3

LF7DB:
       lda    $8C                     ;3
       beq    LF7FD                   ;2
       lda    $B3                     ;3
       bpl    LF7FF                   ;2
       cmp    #$F0                    ;2
       beq    LF7EF                   ;2
       lda    $AD                     ;3
       and    #$0F                    ;2
       cmp    #$08                    ;2
       bcc    LF7F3                   ;2
LF7EF:
       lda    #$00                    ;2
       beq    LF7FD                   ;2 always branch

LF7F3:
       lda    #$04                    ;2
       sta    AUDC0                   ;3
       lda    #$08                    ;2
       sta    AUDF0                   ;3
       lda    #$0A                    ;2
LF7FD:
       sta    AUDV0                   ;3
LF7FF:
       bit    CXM1FB                  ;3
       bmi    LF83C                   ;2
       bit    CXM1P                   ;3
       bpl    LF840                   ;2
       lda    $99                     ;3
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       sta    $D8                     ;3
       lda    #$06                    ;2
       sec                            ;2
       sbc    $D8                     ;3
       tax                            ;2
       ldy    $9A,X                   ;4
       lda    #$00                    ;2
       sta    $9A,X                   ;4
       cpy    #$B0                    ;2
       bne    LF826                   ;2
       lda    #$B0                    ;2
       sta    $9A,X                   ;4
;       jmp    LF840                   ;3 could use BNE/BMI instead
       bne    LF840                   ;2 always branch

LF826:
       cpy    #$C0                    ;2
       bne    LF835                   ;2
       jsr    LFD4A                   ;6
       ldx    #$02                    ;2
       jsr    LFC91                   ;6
       jmp    LF83C                   ;3

LF835:
       lda    #$F0                    ;2
       sta    $B3                     ;3
       jsr    LFD4A                   ;6
LF83C:
       lda    #$FF                    ;2
       sta    $99                     ;3
LF840:
       bit    CXPPMM                  ;3
       bpl    LF883                   ;2
       lda    $8C                     ;3
       beq    LF883                   ;2
       lda    $AC                     ;3
       clc                            ;2
       adc    #$08                    ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       sta    $D8                     ;3
       lda    #$06                    ;2
       sec                            ;2
       sbc    $D8                     ;3
       tax                            ;2
       ldy    $9A,X                   ;4
       lda    #$00                    ;2
       sta    $9A,X                   ;4
       cpy    #$D0                    ;2
       bne    LF876                   ;2
       lda    #$F0                    ;2
       sta    $B3                     ;3
       ldx    #$04                    ;2
       jsr    LFC91                   ;6
       lda    #$03                    ;2
       ldx    #$F9                    ;2
       jsr    LFD61                   ;6
       jmp    LF883                   ;3

LF876:
       lda    #$02                    ;2
       sta    $B8                     ;3
       lda    #$00                    ;2
       sta    $B9                     ;3
       sta    $AD                     ;3
       jmp    LF7B8                   ;3

LF883:
       ldx    #$06                    ;2
LF885:
       lda    $9A,X                   ;4
       cmp    #$00                    ;2
       bne    LF8C3                   ;2
       lda    $B2                     ;3
       beq    LF8AF                   ;2
       lda    $B1                     ;3
       beq    LF8A6                   ;2
       adc    $AC                     ;3
       adc    $B2                     ;3
       adc    $99                     ;3
       and    #$01                    ;2
       beq    LF8A6                   ;2
LF89D:
       dec    $B1                     ;5
       lda    #$B0                    ;2
;       sta    $9A,X                   ;4
;       jmp    LF8BF                   ;3 could use BNE/BMI instead
       bne    LF8BD                   ;2 always branch

LF8A6:
       dec    $B2                     ;5
       lda    #$C0                    ;2
;       sta    $9A,X                   ;4
;       jmp    LF8BF                   ;3 could use BNE/BMI instead
       bne    LF8BD                   ;2 always branch

LF8AF:
       lda    $B1                     ;3
       bne    LF89D                   ;2
       bit    $B3                     ;3
       bmi    LF8C3                   ;2

       lda    #$FF                    ;2 moved
       sta    $B3                     ;3

       lda    #$D0                    ;2
;       sta    $9A,X                   ;4
;       lda    #$FF                    ;2
;       sta    $B3                     ;3
LF8BD:
       sta    $9A,X                   ;4


LF8BF:
       lda    #$01                    ;2
       sta    $A3,X                   ;4
LF8C3:
       dex                            ;2
       bpl    LF885                   ;2
       lda    $B8                     ;3
       cmp    #$02                    ;2
       beq    LF8E7                   ;2
       ldx    #$06                    ;2
LF8CE:
       lda    $9A,X                   ;4
       cmp    #$00                    ;2
       bne    LF8E7                   ;2
       dex                            ;2
       bpl    LF8CE                   ;2
       lda    #$04                    ;2
       sta    $B8                     ;3
       lda    #$01                    ;2
       sta    $B9                     ;3
       lda    #$00                    ;2
       sta    $AD                     ;3
       lda    #$FF                    ;2
       sta    $99                     ;3
LF8E7:
       ldx    #$06                    ;2
LF8E9:
       lda    $9A,X                   ;4
       cmp    #$00                    ;2
       beq    LF93A                   ;2
       lda    $CD                     ;3
       cmp    #$03                    ;2
       bcs    LF909                   ;2
       lda    $9A,X                   ;4
       cmp    #$B0                    ;2
       beq    LF901                   ;2
       lda    $AD                     ;3
       and    #$01                    ;2
       beq    LF93A                   ;2
LF901:
       lda    $A3,X                   ;4
LF903:
       jsr    LFC84                   ;6
       jmp    LF92E                   ;3

LF909:
       cmp    #$05                    ;2
       beq    LF91D                   ;2
       lda    $9A,X                   ;4
       cmp    #$B0                    ;2
       bne    LF901                   ;2
LF913:
       lda    $A3,X                   ;4
       jsr    LFC84                   ;6
       sta    $A3,X                   ;4
;       jmp    LF901                   ;3
       jmp    LF903                   ;3

LF91D:
       lda    $9A,X                   ;4
       cmp    #$B0                    ;2
       bne    LF913                   ;2
       lda    $A3,X                   ;4
       jsr    LFC84                   ;6
       jsr    LFC84                   ;6
       jsr    LFC84                   ;6
LF92E:
       cmp    #$4B                    ;2
       bne    LF938                   ;2
       lda    #$00                    ;2
       sta    $9A,X                   ;4
       lda    #$01                    ;2
LF938:
       sta    $A3,X                   ;4
LF93A:
       dex                            ;2
       bpl    LF8E9                   ;2
LF93D:
       lda    $B8                     ;3
       beq    LF948                   ;2
       cmp    #$01                    ;2
;       beq    LF948                   ;2
;       jmp    LF99D                   ;3 could use BNE to eliminate BEQ above
       bne    LF99D                   ;2

LF948:
       lda    $8C                     ;3
       bne    LF958                   ;2
       lda    $AC                     ;3
       and    #$0F                    ;2
       bne    LF99D                   ;2
       lda    #$FF                    ;2
       sta    $D1                     ;3
       bne    LF95E                   ;2 always branch

LF958:
       ldx    $BC                     ;3
       lda    INPT4,X                 ;4
       bmi    LF99D                   ;2
LF95E:
       lda    $99                     ;3
       cmp    #$FF                    ;2
       bne    LF99D                   ;2
       lda    $D1                     ;3
       beq    LF99D                   ;2
       ldx    #$0C                    ;2
       sta    WSYNC                   ;3

;       ORG $F91E

LF96C: ;waste 59
       dex                            ;2
       bne    LF96C                   ;2
       sta    RESM1                   ;3
       lda    #$10                    ;2
       sta    $C9                     ;3
       lda    $AC                     ;3
       and    #$FE                    ;2
       clc                            ;2
       adc    #$08                    ;2
       sta    $99                     ;3
       lda    $B4                     ;3
       bpl    LF998                   ;2
       lda    #$01                    ;2
       sta    AUDC1                   ;3
       lda    #$E1                    ;2
       sta    $DB                     ;3
       lda    #$02                    ;2
       sta    $B5                     ;3
       sta    $C7                     ;3
       lda    #$07                    ;2
       sta    $B4                     ;3
       lda    #$D9                    ;2
       sta    $B6                     ;3
LF998:
       dec    $D1                     ;5
       jsr    LFD31                   ;6
LF99D:
       lda    $B8                     ;3
       beq    LF9B8                   ;2
       cmp    #$01                    ;2
       beq    LF9B8                   ;2
       cmp    #$04                    ;2
       beq    LF9B8                   ;2
       cmp    #$05                    ;2
       beq    LF9B8                   ;2
       cmp    #$07                    ;2
       beq    LF9B8                   ;2
       cmp    #$09                    ;2
;       beq    LF9B8                   ;2
;       jmp    LF9D5                   ;3 could use BNE to eliminate BEQ above
       bne    LF9D5                   ;2
LF9B8:
       lda    $CD                     ;3
       cmp    #$03                    ;2
       bcs    LF9C2                   ;2
       lda    #$03                    ;2
;       bne    LF9C4                   ;2 always branch...could use BIT.w instead
       .byte $2C                      ;4 skip 2 bytes
LF9C2:
       lda    #$01                    ;2
;LF9C4:
       and    $AD                     ;3
       bne    LF9D5                   ;2
       lda    $CB                     ;3
       jsr    LFC84                   ;6
       cmp    #$7F                    ;2
       bne    LF9D3                   ;2
       lda    #$04                    ;2
LF9D3:
       sta    $CB                     ;3
LF9D5:
       lda    #$00                    ;2
       sta    SWBCNT                  ;4
       lda    SWCHB                   ;4
       lsr                            ;2
       bcs    LF9E9                   ;2
       ldx    #$B0                    ;2
       stx    $8C                     ;3
       lda    #$00                    ;2
       jmp    LF005                   ;3

LF9E9:
       and    #$01                    ;2
       tay                            ;2
       cmp    $DA                     ;3
       beq    LFA13                   ;2
       cpy    #$00                    ;2
       bne    LF9FF                   ;2
       ldx    $AA                     ;3
       inx                            ;2
       cpx    #$04                    ;2
       bne    LF9FD                   ;2
       ldx    #$00                    ;2
LF9FD:
       stx    $AA                     ;3
LF9FF:
       sta    $DA                     ;3
       lda    #$00                    ;2
       sta    $CE                     ;3
       sta    $CF                     ;3
       ldx    $AA                     ;3
       inx                            ;2
       stx    $D0                     ;3
       stx    $D9                     ;3
       stx    $8C                     ;3
       jsr    LFCCA                   ;6
LFA13:
       lda    $D9                     ;3
       beq    LFA1A                   ;2
       jmp    LF0D5                   ;3

LFA1A:
       lda    $B8                     ;3
       cmp    #$0B                    ;2
       bne    LFA39                   ;2
       ldx    $D1                     ;3
       cpx    #$FF                    ;2
       bne    LFA34                   ;2
       lda    #$06                    ;2
       sta    $B8                     ;3
       lda    $C5                     ;3
       jsr    LFC84                   ;6
       sta    $C5                     ;3
       jmp    LFA39                   ;3
LFA34:
       inc    $D1                     ;5
       jsr    LFD31                   ;6
LFA39:
       lda    $CD                     ;3
       cmp    #$02                    ;2
       bne    LFA7D                   ;2
       lda    $B8                     ;3
       bne    LFA7D                   ;2
       lda    $B1                     ;3
       clc                            ;2
       adc    $B2                     ;3
       cmp    #$49                    ;2
       bcs    LFA56                   ;2
       sta    $D8                     ;3
       lda    #$48                    ;2
       sec                            ;2
       sbc    $D8                     ;3
       jmp    LFA59                   ;3

LFA56:
;       sec                            ;2 superfluous
       sbc    #$48                    ;2
LFA59:
       cmp    #$3F                    ;2
       bcc    LFA61                   ;2
       lda    #$08                    ;2
       bne    LFA7B                   ;2

LFA61:
       cmp    #$36                    ;2
       bcc    LFA69                   ;2
       lda    #$06                    ;2
       bne    LFA7B                   ;2

LFA69:
       cmp    #$2D                    ;2
       bcc    LFA71                   ;2
       lda    #$04                    ;2
       bne    LFA7B                   ;2

LFA71:
       cmp    #$24                    ;2
       bcc    LFA79                   ;2
       lda    #$02                    ;2
;       bne    LFA7B                   ;2 could use BIT.w instead
       .byte $2C                      ;4 skip 2 bytes
LFA79:
       lda    #$00                    ;2
LFA7B:
       sta    $C8                     ;3
LFA7D:
       lda    $C9                     ;3
       beq    LFA83                   ;2
       dec    $C9                     ;5
LFA83:
       bit    $B4                     ;3
       bmi    LFAA3                   ;2
       dec    $C7                     ;5
       bne    LFA99                   ;2
       lda    $B5                     ;3
       sta    $C7                     ;3
       dec    $B4                     ;5
       bpl    LFA99                   ;2
       lda    #$00                    ;2
;       sta    AUDV1                   ;3
;       beq    LFAA3                   ;2 always branch...could go to FAA1 to reuse AUDV1
       beq    LFAA1                   ;2 always branch

LFA99:
       ldy    $B4                     ;3
       lda    ($B6),Y                 ;5
       sta    AUDF1                   ;3
       lda    ($DB),Y                 ;5
LFAA1:
       sta    AUDV1                   ;3
LFAA3:
       lda    $8C                     ;3
       bne    LFAAB                   ;2
       sta    AUDV0                   ;3
       sta    AUDV1                   ;3
LFAAB:
       lda    $AD                     ;3
       and    #$07                    ;2
       bne    LFABB                   ;2
       lda    $B8                     ;3
       cmp    #$05                    ;2
       beq    LFABE                   ;2
       cmp    #$07                    ;2
       beq    LFACB                   ;2
LFABB:
       jmp    LFB40                   ;3

LFABE:
       bit    $C1                     ;3
       bvs    LFACB                   ;2
       lda    $BD                     ;3
       ora    #$20                    ;2
       sta    $BD                     ;3
;       jmp    LFAD1                   ;3 could use BNE instead
       bne    LFAD1                   ;2 always branch

LFACB:
       lda    $BD                     ;3
       and    #$DF                    ;2
       sta    $BD                     ;3
LFAD1:
       rol    $BD                     ;5
       ror    $BE                     ;5
       rol    $BF                     ;5
       ror    $C2                     ;5
       rol    $C1                     ;5
       ror    $C0                     ;5
       lda    $BD                     ;3
       ora    #$30                    ;2
       sta    $BD                     ;3
       lda    $C0                     ;3
       ora    #$30                    ;2
       sta    $C0                     ;3
       lda    $B8                     ;3
       cmp    #$07                    ;2
       bne    LFB06                   ;2
       bit    $C0                     ;3
       bvs    LFB06                   ;2
       lda    #$04                    ;2
       sta    $B8                     ;3
       ldx    #$06                    ;2
LFAF9:
       sta    $A3,X                   ;4
       dex                            ;2
       bpl    LFAF9                   ;2
       inx                            ;2
       stx    $B9                     ;3
       stx    $AD                     ;3
       jmp    LF0D5                   ;3

LFB06:
       lda    $C2                     ;3
       cmp    #$0F                    ;2
       bne    LFB40                   ;2
       lda    #$06                    ;2
       sec                            ;2
       sbc    $B0                     ;3
       asl                            ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       sta    $D8                     ;3
       clc                            ;2
       adc    #$05                    ;2
       sta    $AF                     ;3
       lda    $D8                     ;3
       sbc    #$03                    ;2
       sta    $D8                     ;3
       lda    $AC                     ;3
       cmp    $D8                     ;3
       bcc    LFB3C                   ;2
       cmp    $AF                     ;3
       bcs    LFB3C                   ;2
       ldx    $B0                     ;3

       lda    #<FuelTruck             ;2
       inx                            ;2
       sta    $9A,X                   ;4
       dec    $B8                     ;5
       lda    #$03                    ;2
       ldx    #$FC                    ;2
       jsr    LFD61                   ;6
LFB3C:
       inc    $B8                     ;5
       inc    $B8                     ;5
LFB40:
       lda    $B8                     ;3
       cmp    #$09                    ;2
       bne    LFB73                   ;2
       ldx    #$06                    ;2
LFB48:
       lda    $A3,X                   ;4
       jsr    LFC84                   ;6
       sta    $A3,X                   ;4
       dex                            ;2
       bpl    LFB48                   ;2
       and    #$0F                    ;2
       cmp    #$0C                    ;2
       bne    LFB73                   ;2
       lda    #$05                    ;2
       sta    $B8                     ;3
       ldx    #$06                    ;2
       lda    #$00                    ;2
LFB60:
       sta    $9A,X                   ;4
       dex                            ;2
       bpl    LFB60                   ;2
       inc    $CD                     ;5
       lda    $AC                     ;3
       and    #$07                    ;2
       cmp    #$06                    ;2
       bcc    LFB71                   ;2
       lda    #$02                    ;2
LFB71:
       sta    $B0                     ;3
LFB73:
       lda    $B8                     ;3
       cmp    #$06                    ;2
       bne    LFBC5                   ;2
       lda    $AD                     ;3
       and    #$01                    ;2
       bne    LFBC5                   ;2
       sta    AUDV0                   ;3
       lda    $C5                     ;3
       jsr    LFC84                   ;6
       cmp    #$0D                    ;2
       bne    LFB99                   ;2
       lda    #$02                    ;2
       sta    AUDV0                   ;3
       inc    $B8                     ;5
       ldx    $B0                     ;3
       inx                            ;2
       lda    #$00                    ;2
       sta    $9A,X                   ;4
       lda    #$05                    ;2
LFB99:
       sta    $C5                     ;3
       cmp    #$0B                    ;2
       bne    LFBC5                   ;2
       lda    #$0B                    ;2
       sta    $B8                     ;3
       lda    #$01                    ;2
       sta    AUDC1                   ;3
       lda    #$F1                    ;2
       sta    $DB                     ;3
       lda    #$1F                    ;2
       sta    $B5                     ;3
       sta    $C7                     ;3
       lda    $D1                     ;3
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       sta    $D8                     ;3
       lda    #$07                    ;2
       sec                            ;2
       sbc    $D8                     ;3
       sta    $B4                     ;3
       lda    #$E9                    ;2
       sta    $B6                     ;3
LFBC5:
       inc    $AD                     ;5
       lda    $AD                     ;3
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       and    #$0F                    ;2
       ora    #$10                    ;2
       sta    $BA                     ;3
       lda    $B8                     ;3
       cmp    #$0A                    ;2
       beq    LFBE0                   ;2
       cmp    #$08                    ;2
       beq    LFBE0                   ;2
       cmp    #$02                    ;2
       bne    LFC2D                   ;2
LFBE0:
       lda    $AD                     ;3
       cmp    #$64                    ;2
       beq    LFBF0                   ;2
LFBE6:
       and    #$0B                    ;2
       bne    LFBED                   ;2
       jsr    LFD4A                   ;6
LFBED:
       jmp    LFC81                   ;3

LFBF0:
       lda    #$04                    ;2
       sta    $B8                     ;3
       lda    #$00                    ;2
       sta    $AD                     ;3
       dec    $CC                     ;5
       lda    $AA                     ;3
       and    #$01                    ;2
       beq    LFC04                   ;2
       lda    $D2                     ;3
       bpl    LFC11                   ;2
LFC04:
       lda    $CC                     ;3
       bpl    LFC2A                   ;2
       lda    #$06                    ;2
       ldx    #$F9                    ;2
       jsr    LFD61                   ;6
       bne    LFC2A                   ;2
LFC11:
       ldx    #$05                    ;2
LFC13:
       ldy    $CC,X                   ;4
       lda    $D2,X                   ;4
       sta    $CC,X                   ;4
       sty    $D2,X                   ;4
       dex                            ;2
       bpl    LFC13                   ;2
       jsr    LFCCA                   ;6
       jsr    LFD31                   ;6
       lda    $BC                     ;3
       eor    #$01                    ;2
       sta    $BC                     ;3
LFC2A:
       jmp    LFC81                   ;3

LFC2D:
       cmp    #$03                    ;2
       bne    LFC42                   ;2
       lda    $AD                     ;3
       cmp    #$C8                    ;2
       bne    LFBE6                   ;2
       lda    #$09                    ;2
       sta    $B8                     ;3
       lda    #$00                    ;2
       sta    $AD                     ;3
;       jmp    LFC81                   ;3 could use BEQ instead
LFC81: ;moved
       jmp    LF0D5                   ;3


LFC42:
       cmp    #$04                    ;2
       bne    LFC81                   ;2
       lda    #$00                    ;2
       sta    AUDV0                   ;3
       ldx    #$06                    ;2
       lda    #$00                    ;2
LFC4E:
       sta    $9A,X                   ;4
       dex                            ;2
       bpl    LFC4E                   ;2
       lda    $CC                     ;3
       bpl    LFC74                   ;2
       lda    $AD                     ;3
       cmp    #$FF                    ;2
       bne    LFC81                   ;2
       bit    $8C                     ;3
       beq    LFC6C                   ;2
       lda    $AA                     ;3
       and    #$01                    ;2
       beq    LFC81                   ;2
       inc    $AD                     ;5
       jmp    LFC11                   ;3

LFC6C:
       sta    $D1                     ;3
       lda    #$03                    ;2
       sta    $CC                     ;3
       bne    LFC7A                   ;2 always branch

LFC74:
       lda    $AD                     ;3
       cmp    #$78                    ;2
       bne    LFC81                   ;2
LFC7A:
       lda    $B9                     ;3
       sta    $B8                     ;3
       jmp    LF04D                   ;3

;;LFC81:
;;       jmp    LF0D5                   ;3

LFC84:
       tay                            ;2
       and    #$F0                    ;2
       cmp    #$80                    ;2
       bne    LFC8C                   ;2
       iny                            ;2
LFC8C:
       tya                            ;2
       sec                            ;2
       sbc    #$10                    ;2
       rts                            ;6

LFC91:
       ldy    $CD                     ;3
       sed                            ;2
LFC94:
       lda    LFEA9,X                 ;4
       clc                            ;2
       adc    $D0                     ;3
       sta    $D0                     ;3
       lda    LFEAA,X                 ;4
       adc    $CF                     ;3
       sta    $CF                     ;3
       bcc    LFCC6                   ;2
       lda    #$00                    ;2
       adc    $CE                     ;3
       sta    $CE                     ;3
       bcc    LFCBE                   ;2
       lda    #$99                    ;2
       sta    $D0                     ;3
       sta    $CF                     ;3
       sta    $CE                     ;3
       lda    #$FF                    ;2
       sta    $CC                     ;3
       sta    $D2                     ;3
;       jmp    LFCC9                   ;3 could use BNE/BMI instead
       bne    LFCC9                   ;2 always branch

LFCBE:
       lda    $CC                     ;3
       cmp    #$03                    ;2
       beq    LFCC6                   ;2
       inc    $CC                     ;5
LFCC6:
       dey                            ;2
       bne    LFC94                   ;2
LFCC9:
       cld                            ;2
LFCCA:
       lda    $D0                     ;3
       jsr    LFD0C                   ;6
       stx    $80                     ;3
       sty    $82                     ;3
       lda    $CF                     ;3
       jsr    LFD0C                   ;6
       stx    $84                     ;3
       sty    $86                     ;3
       lda    $CE                     ;3
       jsr    LFD0C                   ;6
       stx    $88                     ;3
       sty    $8A                     ;3
       ldy    #<DigitSpace            ;2
       lda    $CE                     ;3
       and    #$F0                    ;2
       bne    LFD0B                   ;2
       lda    $CE                     ;3
       bne    LFD09                   ;2
       lda    $CF                     ;3
       and    #$F0                    ;2
       bne    LFD07                   ;2
       lda    $CF                     ;3
       bne    LFD05                   ;2
       lda    $D0                     ;3
       and    #$F0                    ;2
       bne    LFD03                   ;2
       sty    $82                     ;3
LFD03:
       sty    $84                     ;3
LFD05:
       sty    $86                     ;3
LFD07:
       sty    $88                     ;3
LFD09:
       sty    $8A                     ;3
LFD0B:
       rts                            ;6

LFD0C: ;convert digit to pointers
       pha                            ;3
       and    #$0F                    ;2
       tay                            ;2
;       lda    #$00                    ;2
;LFD12:
;       dey                            ;2
;       bmi    LFD1B                   ;2
;       clc                            ;2
;       adc    #$08                    ;2
;;       jmp    LFD12                   ;3 could use BNE instead
;       bne    LFD12                   ;2 always branch
;LFD1B:

       ldx    DigitTbl,Y              ;4

;       tax                            ;2
       pla                            ;4
;       and    #$F0                    ;2 superfluous
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       tay                            ;2
;       lda    #$00                    ;2
;LFD26:
;       dey                            ;2
;       bmi    LFD2F                   ;2
;       clc                            ;2
;       adc    #$08                    ;2
;;       jmp    LFD26                   ;3 could use BNE instead
;       bne    LFD26                   ;2 always branch
;LFD2F:

       lda    DigitTbl,Y              ;4

       tay                            ;2
       rts                            ;6





LFD31:
       lda    $D1                     ;3
       beq    LFD47                   ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       tay                            ;2
       iny                            ;2
       lda    #$00                    ;2
       cpy    #$00                    ;2
       beq    LFD47                   ;2
LFD42:
       sec                            ;2
       rol                            ;2
       dey                            ;2
       bne    LFD42                   ;2
LFD47:
       sta    $CA                     ;3
       rts                            ;6

LFD4A:
       lda    #$08                    ;2
       sta    AUDC1                   ;3
       lda    #$D1                    ;2
       sta    $DB                     ;3
       lda    #$02                    ;2
       sta    $B5                     ;3
       sta    $C7                     ;3
       lda    #$07                    ;2
       sta    $B4                     ;3
       lda    #$C9                    ;2
       sta    $B6                     ;3
       rts                            ;6

LFD61:
       sta    $B4                     ;3
       lda    #$04                    ;2
       sta    AUDC1                   ;3
       lda    #$F1                    ;2
       sta    $DB                     ;3
       stx    $B6                     ;3
       lda    #$1E                    ;2
       sta    $B5                     ;3
       sta    $C7                     ;3
       rts                            ;6

LFD74:
       sta    WSYNC                   ;3
       dex                            ;2
       bne    LFD74                   ;2
       rts                            ;6

;       ORG $FCF7
;       rts                            ;6

       ORG $FD89,0

;       .byte $AA ; |X X X X | $FD7A
;       .byte $AA ; |X X X X | $FD7B
;       .byte $AA ; |X X X X | $FD7C
;       .byte $AA ; |X X X X | $FD7D
;       .byte $AA ; |X X X X | $FD7E
;       .byte $AA ; |X X X X | $FD7F
;       .byte $AA ; |X X X X | $FD80
;       .byte $AA ; |X X X X | $FD81
;       .byte $AA ; |X X X X | $FD82
;       .byte $AA ; |X X X X | $FD83
;       .byte $AA ; |X X X X | $FD84
;       .byte $AA ; |X X X X | $FD85
;       .byte $AA ; |X X X X | $FD86
;       .byte $AA ; |X X X X | $FD87

Logo2:
       .byte $FE ; |XXXXXXX | $FDA6
       .byte $1E ; |   XXXX | $FDA7
       .byte $1E ; |   XXXX | $FDA8
       .byte $3F ; |  XXXXXX| $FDA9
       .byte $3F ; |  XXXXXX| $FDAA
       .byte $3F ; |  XXXXXX| $FDAB
       .byte $7F ; | XXXXXXX| $FDAC
       .byte $7F ; | XXXXXXX| $FDAD
       .byte $7F ; | XXXXXXX| $FDAE
       .byte $F3 ; |XXXX  XX| $FDAF
       .byte $F3 ; |XXXX  XX| $FDB0
       .byte $F3 ; |XXXX  XX| $FDB1
       .byte $F3 ; |XXXX  XX| $FDB2
       .byte $00 ; |        | $FDB3
       .byte $77 ; | XXX XXX| $FDB4
       .byte $44 ; | X   X  | $FDB5
       .byte $74 ; | XXX X  | $FDB6
       .byte $44 ; | X   X  | $FDB7
       .byte $77 ; | XXX XXX| $FDB8
       .byte $00 ; |        | $FDB9
       .byte $C0 ; |XX      | $FDBA
       .byte $80 ; |X       | $FDBB
       .byte $80 ; |X       | $FDBC
       .byte $80 ; |X       | $FDBD
       .byte $00 ; |        | $FDBE
       .byte $00 ; |        | $FDBF
       .byte $00 ; |        | $FDC0
       .byte $80 ; |X       | $FDC1
       .byte $80 ; |X       | $FDC2
       .byte $80 ; |X       | $FDC3

Logo3:
       .byte $5A ; | X XX X | $FDC4
       .byte $4A ; | X  X X | $FDC5
       .byte $5A ; | X XX X | $FDC6
       .byte $52 ; | X X  X | $FDC7
       .byte $5A ; | X XX X | $FDC8
       .byte $00 ; |        | $FDC9
       .byte $80 ; |X       | $FDCA
       .byte $80 ; |X       | $FDCB
       .byte $80 ; |X       | $FDCC
       .byte $C0 ; |XX      | $FDCD
       .byte $C0 ; |XX      | $FDCE
       .byte $C0 ; |XX      | $FDCF
       .byte $E0 ; |XXX     | $FDD0
       .byte $00 ; |        | $FDD1
       .byte $24 ; |  X  X  | $FDD2
       .byte $25 ; |  X  X X| $FDD3
       .byte $27 ; |  X  XXX| $FDD4
       .byte $24 ; |  X  X  | $FDD5
       .byte $77 ; | XXX XXX| $FDD6
       .byte $00 ; |        | $FDD7
       .byte $F8 ; |XXXXX   | $FDD8
       .byte $7C ; | XXXXX  | $FDD9
       .byte $7C ; | XXXXX  | $FDDA
       .byte $7C ; | XXXXX  | $FDDB
       .byte $3E ; |  XXXXX | $FDDC
       .byte $3E ; |  XXXXX | $FDDD
       .byte $3E ; |  XXXXX | $FDDE
       .byte $7F ; | XXXXXXX| $FDDF
       .byte $7F ; | XXXXXXX| $FDE0
       .byte $7F ; | XXXXXXX| $FDE1

Logo4:
       .byte $E9 ; |XXX X  X| $FDE2
       .byte $A9 ; |X X X  X| $FDE3
       .byte $AB ; |X X X XX| $FDE4
       .byte $AF ; |X X XXXX| $FDE5
       .byte $ED ; |XXX XX X| $FDE6
       .byte $01 ; |       X| $FDE7
       .byte $01 ; |       X| $FDE8
       .byte $01 ; |       X| $FDE9
       .byte $01 ; |       X| $FDEA
       .byte $01 ; |       X| $FDEB
       .byte $01 ; |       X| $FDEC
       .byte $01 ; |       X| $FDED
       .byte $01 ; |       X| $FDEE
       .byte $00 ; |        | $FDEF
       .byte $A4 ; |X X  X  | $FDF0
       .byte $3C ; |  XXXX  | $FDF1
       .byte $A4 ; |X X  X  | $FDF2
       .byte $A4 ; |X X  X  | $FDF3
       .byte $18 ; |   XX   | $FDF4
       .byte $00 ; |        | $FDF5
       .byte $01 ; |       X| $FDF6
       .byte $01 ; |       X| $FDF7
       .byte $01 ; |       X| $FDF8
       .byte $01 ; |       X| $FDF9
       .byte $01 ; |       X| $FDFA
       .byte $01 ; |       X| $FDFB
       .byte $01 ; |       X| $FDFC
       .byte $81 ; |X      X| $FDFD
       .byte $81 ; |X      X| $FDFE
;       .byte $FF ; |XXXXXXXX| $FDFF
Logo1:
       .byte $FF ; |XXXXXXXX| $FD88 shared
       .byte $80 ; |X       | $FD89
       .byte $80 ; |X       | $FD8A
       .byte $80 ; |X       | $FD8B
       .byte $80 ; |X       | $FD8C
       .byte $80 ; |X       | $FD8D
       .byte $80 ; |X       | $FD8E
       .byte $80 ; |X       | $FD8F
       .byte $80 ; |X       | $FD90
       .byte $80 ; |X       | $FD91
       .byte $80 ; |X       | $FD92
       .byte $80 ; |X       | $FD93
       .byte $81 ; |X      X| $FD94
       .byte $00 ; |        | $FD95
       .byte $74 ; | XXX X  | $FD96
       .byte $14 ; |   X X  | $FD97
       .byte $77 ; | XXX XXX| $FD98
       .byte $45 ; | X   X X| $FD99
       .byte $77 ; | XXX XXX| $FD9A
       .byte $00 ; |        | $FD9B
       .byte $87 ; |X    XXX| $FD9C
       .byte $8F ; |X   XXXX| $FD9D
       .byte $8F ; |X   XXXX| $FD9E
       .byte $8F ; |X   XXXX| $FD9F
       .byte $9F ; |X  XXXXX| $FDA0
       .byte $9F ; |X  XXXXX| $FDA1
       .byte $9F ; |X  XXXXX| $FDA2
       .byte $FF ; |XXXXXXXX| $FDA3
       .byte $FF ; |XXXXXXXX| $FDA4
       .byte $FF ; |XXXXXXXX| $FDA5


       ORG $FE00

Digit0:
       .byte $3C ; |  XXXX  | $FE00
       .byte $66 ; | XX  XX | $FE01
       .byte $66 ; | XX  XX | $FE02
       .byte $66 ; | XX  XX | $FE03
       .byte $66 ; | XX  XX | $FE04
       .byte $66 ; | XX  XX | $FE05
;       .byte $3C ; |  XXXX  | $FE07
Digit5:
       .byte $3C ; |  XXXX  | $FE4F shared
       .byte $46 ; | X   XX | $FE29
       .byte $06 ; |     XX | $FE2A
       .byte $06 ; |     XX | $FE2B
       .byte $7C ; | XXXXX  | $FE2C
       .byte $60 ; | XX     | $FE2E
;       .byte $7E ; | XXXXXX | $FE2F
Digit2:
       .byte $7E ; | XXXXXX | $FE10 shared
       .byte $60 ; | XX     | $FE11
       .byte $60 ; | XX     | $FE12
       .byte $3C ; |  XXXX  | $FE13
       .byte $06 ; |     XX | $FE15
       .byte $46 ; | X   XX | $FE16
;       .byte $3C ; |  XXXX  | $FE17
Digit3:
       .byte $3C ; |  XXXX  | $FE18 shared
       .byte $46 ; | X   XX | $FE19
       .byte $06 ; |     XX | $FE1A
       .byte $0C ; |    XX  | $FE1C
       .byte $06 ; |     XX | $FE1D
       .byte $46 ; | X   XX | $FE1E
;       .byte $3C ; |  XXXX  | $FE1F
Digit6:
       .byte $3C ; |  XXXX  | $FE30 shared
       .byte $66 ; | XX  XX | $FE31
       .byte $66 ; | XX  XX | $FE32
       .byte $7C ; | XXXXX  | $FE34
       .byte $60 ; | XX     | $FE35
       .byte $62 ; | XX   X | $FE36
;       .byte $3C ; |  XXXX  | $FE37
Digit8:
       .byte $3C ; |  XXXX  | $FE40 shared
       .byte $66 ; | XX  XX | $FE41
       .byte $66 ; | XX  XX | $FE42
       .byte $3C ; |  XXXX  | $FE43
       .byte $66 ; | XX  XX | $FE45
       .byte $66 ; | XX  XX | $FE46
;       .byte $3C ; |  XXXX  | $FE47
Digit9:
       .byte $3C ; |  XXXX  | $FE48 shared
       .byte $46 ; | X   XX | $FE49
       .byte $06 ; |     XX | $FE4A
       .byte $3E ; |  XXXXX | $FE4B
       .byte $66 ; | XX  XX | $FE4C
       .byte $66 ; | XX  XX | $FE4E
       .byte $3C ; |  XXXX  | $FE4F





Digit4:
       .byte $0C ; |    XX  | $FE20
       .byte $0C ; |    XX  | $FE21
       .byte $7E ; | XXXXXX | $FE23
       .byte $4C ; | X  XX  | $FE24
       .byte $2C ; |  X XX  | $FE25
       .byte $1C ; |   XXX  | $FE26
       .byte $0C ; |    XX  | $FE27



Digit7:
       .byte $18 ; |   XX   | $FE38
       .byte $18 ; |   XX   | $FE39
       .byte $18 ; |   XX   | $FE3B
       .byte $0C ; |    XX  | $FE3C
       .byte $06 ; |     XX | $FE3D
       .byte $42 ; | X    X | $FE3E
;       .byte $7E ; | XXXXXX | $FE3F
Digit1:
       .byte $7E ; | XXXXXX | $FE08 shared
       .byte $18 ; |   XX   | $FE09
       .byte $18 ; |   XX   | $FE0A
       .byte $18 ; |   XX   | $FE0B
       .byte $18 ; |   XX   | $FE0C
       .byte $38 ; |  XXX   | $FE0E
;       .byte $18 ; |   XX   | $FE0F
Sun:
       .byte $18 ; |   XX   | $FE58 shared
       .byte $3C ; |  XXXX  | $FE59
       .byte $7E ; | XXXXXX | $FE5A
       .byte $7E ; | XXXXXX | $FE5B
       .byte $FF ; |XXXXXXXX| $FE5C
       .byte $FF ; |XXXXXXXX| $FE5D
       .byte $FF ; |XXXXXXXX| $FE5E
       .byte $FF ; |XXXXXXXX| $FE5F
       .byte $FF ; |XXXXXXXX| $FE60
       .byte $FF ; |XXXXXXXX| $FE61
       .byte $FF ; |XXXXXXXX| $FE62
       .byte $FF ; |XXXXXXXX| $FE63
       .byte $7E ; | XXXXXX | $FE64
       .byte $7E ; | XXXXXX | $FE65
       .byte $3C ; |  XXXX  | $FE66
;       .byte $18 ; |   XX   | $FE67
Moon:
       .byte $18 ; |   XX   | $FF70 shared
       .byte $0C ; |    XX  | $FF71
       .byte $06 ; |     XX | $FF72
       .byte $06 ; |     XX | $FF73
       .byte $07 ; |     XXX| $FF74
       .byte $07 ; |     XXX| $FF75
       .byte $07 ; |     XXX| $FF76
       .byte $07 ; |     XXX| $FF77
       .byte $07 ; |     XXX| $FF78
       .byte $07 ; |     XXX| $FF79
       .byte $07 ; |     XXX| $FF7A
       .byte $07 ; |     XXX| $FF7B
       .byte $06 ; |     XX | $FF7C
       .byte $06 ; |     XX | $FF7D
       .byte $0C ; |    XX  | $FF7E
       .byte $18 ; |   XX   | $FF7F



LFFF6:
       .byte $14 ; |   X X  | $FFF6
       .byte $48 ; | X  X   | $FFF7
       .byte $50 ; | X X    | $FFF8
       .byte $78 ; | XXXX   | $FFF9
       .byte $A0 ; |X X     | $FFFA
       .byte $AA ; |X X X X | $FFFB

DigitTbl:
       .byte <Digit0
       .byte <Digit1
       .byte <Digit2
       .byte <Digit3
       .byte <Digit4
       .byte <Digit5
       .byte <Digit6
       .byte <Digit7
       .byte <Digit8
       .byte <Digit9

LFE68:
       .byte $FF ; |XXXXXXXX| $FE68
       .byte $FE ; |XXXXXXX | $FE69
       .byte $7E ; | XXXXXX | $FE6A
       .byte $7E ; | XXXXXX | $FE6B
       .byte $7E ; | XXXXXX | $FE6C
       .byte $7E ; | XXXXXX | $FE6D
       .byte $7E ; | XXXXXX | $FE6E
       .byte $7E ; | XXXXXX | $FE6F
       .byte $7E ; | XXXXXX | $FE70
       .byte $6C ; | XX XX  | $FE71
;       .byte $00 ; |        | $FE72
;       .byte $00 ; |        | $FE73
;       .byte $00 ; |        | $FE74
;       .byte $00 ; |        | $FE75
;       .byte $00 ; |        | $FE76
;       .byte $00 ; |        | $FE77

DigitSpace:
       .byte $00 ; |        | $FE51 shared
;       .byte $00 ; |        | $FE54
;       .byte $00 ; |        | $FE55
;       .byte $00 ; |        | $FE56
;       .byte $00 ; |        | $FE57
Cloud:
       .byte $00 ; |        | $FE88 shared x2
       .byte $00 ; |        | $FE89 shared x2
       .byte $00 ; |        | $FE8A shared x2
       .byte $00 ; |        | $FE8A shared x2
       .byte $00 ; |        | $FE8A shared x2
       .byte $00 ; |        | $FE8B shared
       .byte $0C ; |    XX  | $FE8C
       .byte $3E ; |  XXXXX | $FE8D
       .byte $7F ; | XXXXXXX| $FE8E
       .byte $FF ; |XXXXXXXX| $FE8F
       .byte $FF ; |XXXXXXXX| $FE90
       .byte $7E ; | XXXXXX | $FE91
       .byte $3C ; |  XXXX  | $FE92
       .byte $00 ; |        | $FE95
       .byte $00 ; |        | $FE96
       .byte $00 ; |        | $FE97


LFE78:
       .byte $FF ; |XXXXXXXX| $FE78
       .byte $FF ; |XXXXXXXX| $FE79
       .byte $FF ; |XXXXXXXX| $FE7A
       .byte $FF ; |XXXXXXXX| $FE7B
       .byte $FF ; |XXXXXXXX| $FE7C
       .byte $FF ; |XXXXXXXX| $FE7D
       .byte $7F ; | XXXXXXX| $FE7E
       .byte $7F ; | XXXXXXX| $FE7F
       .byte $7F ; | XXXXXXX| $FE80
       .byte $7F ; | XXXXXXX| $FE81
       .byte $7E ; | XXXXXX | $FE82
       .byte $7E ; | XXXXXX | $FE83
       .byte $7E ; | XXXXXX | $FE84
       .byte $7E ; | XXXXXX | $FE85
       .byte $7E ; | XXXXXX | $FE86
       .byte $3E ; |  XXXXX | $FE87


LFE98: ;reserve ship gfx
       .byte $00 ; |        | $FEAF
       .byte $0E ; |    XXX | $FE98
       .byte $04 ; |     X  | $FE99
       .byte $1E ; |   XXXX | $FE9A
       .byte $37 ; |  XX XXX| $FE9C
       .byte $1E ; |   XXXX | $FE9D
       .byte $04 ; |     X  | $FE9E
       .byte $0E ; |    XXX | $FE9F

  IF PAL60
LFEA0: ;sky color behind mountians
       .byte $B6 ; |X  X XX | $FEA0
       .byte $B4 ; |X  X X  | $FEA1
       .byte $B2 ; |X  X  X | $FEA2
       .byte $B0 ; |X  X    | $FEA3
       .byte $B2 ; |X  X  X | $FEA4
       .byte $B4 ; |X  X X  | $FEA5
       .byte $B6 ; |X  X XX | $FEA6
       .byte $B8 ; |X  XX   | $FEA7
       .byte $B8 ; |X  XX   | $FEA8
  ELSE
LFEA0: ;sky color behind mountians
       .byte $96 ; |X  X XX | $FEA0
       .byte $94 ; |X  X X  | $FEA1
       .byte $92 ; |X  X  X | $FEA2
       .byte $90 ; |X  X    | $FEA3
       .byte $92 ; |X  X  X | $FEA4
       .byte $94 ; |X  X X  | $FEA5
       .byte $96 ; |X  X XX | $FEA6
       .byte $98 ; |X  XX   | $FEA7
       .byte $98 ; |X  XX   | $FEA8
  ENDIF

LFEA9:
       .byte $05 ; |     X X| $FEA9
LFEAA:
       .byte $00 ; |        | $FEAA
       .byte $0A ; |    X X | $FEAB
       .byte $00 ; |        | $FEAC
       .byte $00 ; |        | $FEAD
       .byte $01 ; |       X| $FEAE
       .byte $00 ; |        | $FEAF
       .byte $02 ; |      X | $FEB0
       .byte $00 ; |        | $FEB1
       .byte $05 ; |     X X| $FEB2

  IF PAL60
LFEB3:
       .byte $20 ; |XXX     | $FEB3
       .byte $20 ; |XXX     | $FEB4
       .byte $20 ; |XXX     | $FEB5
       .byte $22 ; |XXX   X | $FEB6
       .byte $24 ; |XXX  X  | $FEB7
       .byte $26 ; |XXX  XX | $FEB8
  ELSE
LFEB3:
       .byte $E0 ; |XXX     | $FEB3
       .byte $E0 ; |XXX     | $FEB4
       .byte $E0 ; |XXX     | $FEB5
       .byte $E2 ; |XXX   X | $FEB6
       .byte $E4 ; |XXX  X  | $FEB7
       .byte $E6 ; |XXX  XX | $FEB8
  ENDIF

LFEB9: ;"empty" fuel indicator
       .byte $00 ; |        | $FEAF
       .byte $E0 ; |XXX     | $FEB9
       .byte $80 ; |X       | $FEBA
       .byte $80 ; |X       | $FEBB
       .byte $C0 ; |XX      | $FEBD
       .byte $80 ; |X       | $FEBE
       .byte $80 ; |X       | $FEBF
       .byte $E0 ; |XXX     | $FEC0

LFEC1: ;"full" fuel indicator
       .byte $00 ; |        | $FEAF
       .byte $04 ; |     X  | $FEC1
       .byte $04 ; |     X  | $FEC2
       .byte $04 ; |     X  | $FEC3
       .byte $06 ; |     XX | $FEC5
       .byte $04 ; |     X  | $FEC6
       .byte $04 ; |     X  | $FEC7
       .byte $07 ; |     XXX| $FEC8


       .byte $1B ; |   XX XX| $FEC9
       .byte $1C ; |   XXX  | $FECA
       .byte $1D ; |   XXX X| $FECB
       .byte $1E ; |   XXXX | $FECC
       .byte $1F ; |   XXXXX| $FECD
       .byte $1F ; |   XXXXX| $FECE
       .byte $1F ; |   XXXXX| $FECF
       .byte $1F ; |   XXXXX| $FED0
       .byte $07 ; |     XXX| $FED1
       .byte $09 ; |    X  X| $FED2
       .byte $0C ; |    XX  | $FED3
       .byte $0E ; |    XXX | $FED4
       .byte $0F ; |    XXXX| $FED5
       .byte $0F ; |    XXXX| $FED6
       .byte $0F ; |    XXXX| $FED7
       .byte $0F ; |    XXXX| $FED8
       .byte $10 ; |   X    | $FED9
       .byte $0E ; |    XXX | $FEDA
       .byte $0C ; |    XX  | $FEDB
       .byte $0A ; |    X X | $FEDC
       .byte $08 ; |    X   | $FEDD
       .byte $06 ; |     XX | $FEDE
       .byte $04 ; |     X  | $FEDF
       .byte $02 ; |      X | $FEE0
       .byte $05 ; |     X X| $FEE1
       .byte $07 ; |     XXX| $FEE2
       .byte $09 ; |    X  X| $FEE3
       .byte $0B ; |    X XX| $FEE4
       .byte $0D ; |    XX X| $FEE5
       .byte $0F ; |    XXXX| $FEE6
       .byte $0F ; |    XXXX| $FEE7
       .byte $0F ; |    XXXX| $FEE8
       .byte $07 ; |     XXX| $FEE9
       .byte $08 ; |    X   | $FEEA
       .byte $09 ; |    X  X| $FEEB
       .byte $0A ; |    X X | $FEEC
       .byte $0B ; |    X XX| $FEED
       .byte $0C ; |    XX  | $FEEE
       .byte $0D ; |    XX X| $FEEF
       .byte $0E ; |    XXX | $FEF0
       .byte $0A ; |    X X | $FEF1
       .byte $0A ; |    X X | $FEF2
       .byte $0A ; |    X X | $FEF3
       .byte $0A ; |    X X | $FEF4
       .byte $0A ; |    X X | $FEF5
       .byte $0A ; |    X X | $FEF6
       .byte $0A ; |    X X | $FEF7
       .byte $0A ; |    X X | $FEF8
       .byte $10 ; |   X    | $FEF9
       .byte $18 ; |   XX   | $FEFA
       .byte $10 ; |   X    | $FEFB
       .byte $14 ; |   X X  | $FEFC
       .byte $18 ; |   XX   | $FEFD
       .byte $14 ; |   X X  | $FEFE
       .byte $18 ; |   XX   | $FEFF

       ORG $FF00

LFF00:
       .byte $00 ; |        | $FF00
       .byte $00 ; |        | $FF01
       .byte $00 ; |        | $FF02
       .byte $00 ; |        | $FF03
       .byte $00 ; |        | $FF04
       .byte $00 ; |        | $FF05
       .byte $00 ; |        | $FF06
       .byte $00 ; |        | $FF07
       .byte $00 ; |        | $FF08
       .byte $00 ; |        | $FF09
       .byte $00 ; |        | $FF0A
       .byte $00 ; |        | $FF0B
       .byte $00 ; |        | $FF0C
       .byte $00 ; |        | $FF0D
       .byte $00 ; |        | $FF0E
       .byte $00 ; |        | $FF0F

       .byte $1E ; |   XXXX | $FF10
       .byte $1E ; |   XXXX | $FF11
       .byte $0C ; |    XX  | $FF12
       .byte $0C ; |    XX  | $FF13
       .byte $0C ; |    XX  | $FF14
       .byte $3E ; |  XXXXX | $FF15
       .byte $7F ; | XXXXXXX| $FF16
       .byte $EF ; |XXX XXXX| $FF17
       .byte $CE ; |XX  XXX | $FF18
       .byte $EF ; |XXX XXXX| $FF19
       .byte $7F ; | XXXXXXX| $FF1A
       .byte $3E ; |  XXXXX | $FF1B
       .byte $0C ; |    XX  | $FF1C
       .byte $0C ; |    XX  | $FF1D
       .byte $0C ; |    XX  | $FF1E
       .byte $1E ; |   XXXX | $FF1F

LFF20:
       .byte $00 ; |        | $FF20
       .byte $00 ; |        | $FF21
       .byte $00 ; |        | $FF22
       .byte $00 ; |        | $FF23
       .byte $00 ; |        | $FF24
       .byte $00 ; |        | $FF25
       .byte $00 ; |        | $FF26
       .byte $00 ; |        | $FF27
       .byte $00 ; |        | $FF28
       .byte $00 ; |        | $FF29
       .byte $00 ; |        | $FF2A
       .byte $00 ; |        | $FF2B
       .byte $00 ; |        | $FF2C
       .byte $00 ; |        | $FF2D
       .byte $00 ; |        | $FF2E
       .byte $00 ; |        | $FF2F

       .byte $00 ; |        | $FF30
       .byte $00 ; |        | $FF31
       .byte $00 ; |        | $FF32
       .byte $1E ; |   XXXX | $FF33
       .byte $0C ; |    XX  | $FF34
       .byte $3E ; |  XXXXX | $FF35
       .byte $7F ; | XXXXXXX| $FF36
       .byte $EF ; |XXX XXXX| $FF37
       .byte $CC ; |XX  XX  | $FF38
       .byte $EF ; |XXX XXXX| $FF39
       .byte $7F ; | XXXXXXX| $FF3A
       .byte $3E ; |  XXXXX | $FF3B
       .byte $0C ; |    XX  | $FF3C
       .byte $1E ; |   XXXX | $FF3D
       .byte $00 ; |        | $FF3E
       .byte $00 ; |        | $FF3F

       .byte $00 ; |        | $FF40
       .byte $00 ; |        | $FF41
       .byte $00 ; |        | $FF42
       .byte $00 ; |        | $FF43
       .byte $00 ; |        | $FF44
       .byte $00 ; |        | $FF45
       .byte $00 ; |        | $FF46
       .byte $00 ; |        | $FF47
       .byte $00 ; |        | $FF48
       .byte $00 ; |        | $FF49
       .byte $00 ; |        | $FF4A
       .byte $00 ; |        | $FF4B
       .byte $00 ; |        | $FF4C
       .byte $00 ; |        | $FF4D
       .byte $00 ; |        | $FF4E
       .byte $00 ; |        | $FF4F

;power station gfx
       .byte $FF ; |XXXXXXXX| $FF50
       .byte $E0 ; |XXX     | $FF51
       .byte $EE ; |XXX XXX | $FF52
       .byte $EE ; |XXX XXX | $FF53
       .byte $5F ; | X XXXXX| $FF54
       .byte $9F ; |X  XXXXX| $FF55
       .byte $4E ; | X  XXX | $FF56
       .byte $8E ; |X   XXX | $FF57
       .byte $4E ; | X  XXX | $FF58
       .byte $8E ; |X   XXX | $FF59
       .byte $4A ; | X  X X | $FF5A
       .byte $8E ; |X   XXX | $FF5B
       .byte $70 ; | XXX    | $FF5C
       .byte $8E ; |X   XXX | $FF5D
       .byte $0E ; |    XXX | $FF5E
       .byte $04 ; |     X  | $FF5F

       .byte $06 ; |     XX | $FF60
       .byte $0E ; |    XXX | $FF61
       .byte $0E ; |    XXX | $FF62
       .byte $0E ; |    XXX | $FF63
       .byte $08 ; |    X   | $FF64
       .byte $04 ; |     X  | $FF65
       .byte $06 ; |     XX | $FF66
       .byte $0E ; |    XXX | $FF67
       .byte $0E ; |    XXX | $FF68
       .byte $0E ; |    XXX | $FF69
       .byte $06 ; |     XX | $FF6A
       .byte $06 ; |     XX | $FF6B
       .byte $08 ; |    X   | $FF6C
       .byte $0E ; |    XXX | $FF6D
       .byte $0E ; |    XXX | $FF6E
       .byte $0E ; |    XXX | $FF6F


;16 free
       .byte $00 ; |        | $FF80
       .byte $00 ; |        | $FF80
       .byte $00 ; |        | $FF80
       .byte $00 ; |        | $FF80
       .byte $00 ; |        | $FF80
       .byte $00 ; |        | $FF80
       .byte $00 ; |        | $FF80
       .byte $00 ; |        | $FF80
       .byte $00 ; |        | $FF80
       .byte $00 ; |        | $FF80
       .byte $00 ; |        | $FF80
       .byte $00 ; |        | $FF80
       .byte $00 ; |        | $FF80
       .byte $00 ; |        | $FF80
       .byte $00 ; |        | $FF80
       .byte $00 ; |        | $FF80

       .byte $00 ; |        | $FF80
       .byte $00 ; |        | $FF81
       .byte $00 ; |        | $FF82
       .byte $0E ; |    XXX | $FF83
       .byte $0C ; |    XX  | $FF84
       .byte $04 ; |     X  | $FF85
       .byte $06 ; |     XX | $FF86
       .byte $0E ; |    XXX | $FF87
       .byte $0E ; |    XXX | $FF88
       .byte $0E ; |    XXX | $FF89
       .byte $08 ; |    X   | $FF8A
       .byte $08 ; |    X   | $FF8B
       .byte $0C ; |    XX  | $FF8C
       .byte $0C ; |    XX  | $FF8D
       .byte $00 ; |        | $FF8E
       .byte $00 ; |        | $FF8F

       .byte $0F ; |    XXXX| $FF90
       .byte $06 ; |     XX | $FF91
       .byte $06 ; |     XX | $FF92
       .byte $06 ; |     XX | $FF93
       .byte $06 ; |     XX | $FF94
       .byte $06 ; |     XX | $FF95
       .byte $04 ; |     X  | $FF96
       .byte $00 ; |        | $FF97
       .byte $04 ; |     X  | $FF98
       .byte $04 ; |     X  | $FF99
       .byte $04 ; |     X  | $FF9A
       .byte $04 ; |     X  | $FF9B
       .byte $00 ; |        | $FF9C
       .byte $00 ; |        | $FF9D
       .byte $00 ; |        | $FF9E
       .byte $04 ; |     X  | $FF9F

LFFA0:
       .byte $00 ; |        | $FFA0
       .byte $AC ; |X X XX  | $FFA1
       .byte $A6 ; |X X  XX | $FFA2
       .byte $06 ; |     XX | $FFA3
       .byte $40 ; | X      | $FFA4
       .byte $48 ; | X  X   | $FFA5
       .byte $58 ; | X XX   | $FFA6
       .byte $58 ; | X XX   | $FFA7
       .byte $1A ; |   XX X | $FFA8
       .byte $02 ; |      X | $FFA9
       .byte $02 ; |      X | $FFAA
       .byte $44 ; | X   X  | $FFAB
       .byte $60 ; | XX     | $FFAC
       .byte $00 ; |        | $FFAD
       .byte $00 ; |        | $FFAE
       .byte $00 ; |        | $FFAF

       .byte $00 ; |        | $FFB0
       .byte $00 ; |        | $FFB1
       .byte $00 ; |        | $FFB2
       .byte $00 ; |        | $FFB3
       .byte $00 ; |        | $FFB4
       .byte $00 ; |        | $FFB5
       .byte $10 ; |   X    | $FFB6
       .byte $3E ; |  XXXXX | $FFB7
       .byte $10 ; |   X    | $FFB8
       .byte $00 ; |        | $FFB9
       .byte $00 ; |        | $FFBA
       .byte $00 ; |        | $FFBB
       .byte $00 ; |        | $FFBC
       .byte $00 ; |        | $FFBD
       .byte $00 ; |        | $FFBE
       .byte $92 ; |X  X  X | $FFBF
       .byte $00 ; |        | $FFC0
       .byte $00 ; |        | $FFC1
       .byte $00 ; |        | $FFC2
       .byte $38 ; |  XXX   | $FFC3
       .byte $20 ; |  X     | $FFC4
       .byte $30 ; |  XX    | $FFC5
       .byte $7E ; | XXXXXX | $FFC6
       .byte $FB ; |XXXXX XX| $FFC7
       .byte $7E ; | XXXXXX | $FFC8
       .byte $30 ; |  XX    | $FFC9
       .byte $20 ; |  X     | $FFCA
       .byte $38 ; |  XXX   | $FFCB
       .byte $00 ; |        | $FFCC
       .byte $00 ; |        | $FFCD
       .byte $00 ; |        | $FFCE
       .byte $42 ; | X    X | $FFCF
       .byte $00 ; |        | $FFD0
       .byte $00 ; |        | $FFD1
       .byte $00 ; |        | $FFD2
       .byte $78 ; | XXXX   | $FFD3
       .byte $30 ; |  XX    | $FFD4
       .byte $7C ; | XXXXX  | $FFD5
       .byte $FE ; |XXXXXXX | $FFD6
       .byte $F7 ; |XXXX XXX| $FFD7
       .byte $33 ; |  XX  XX| $FFD8
       .byte $F7 ; |XXXX XXX| $FFD9
       .byte $FE ; |XXXXXXX | $FFDA
       .byte $7C ; | XXXXX  | $FFDB
       .byte $30 ; |  XX    | $FFDC
       .byte $78 ; | XXXX   | $FFDD
       .byte $00 ; |        | $FFDE
       .byte $00 ; |        | $FFDF
FuelTruck:
       .byte $00 ; |        | $FFE0
       .byte $00 ; |        | $FFE1
       .byte $42 ; | X    X | $FFE2
       .byte $42 ; | X    X | $FFE3
       .byte $00 ; |        | $FFE4
       .byte $FF ; |XXXXXXXX| $FFE5
       .byte $03 ; |      XX| $FFE6
       .byte $FB ; |XXXXX XX| $FFE7
       .byte $FA ; |XXXXX X | $FFE8
       .byte $F8 ; |XXXXX   | $FFE9
       .byte $F8 ; |XXXXX   | $FFEA
       .byte $10 ; |   X    | $FFEB
       .byte $00 ; |        | $FFEC
       .byte $00 ; |        | $FFED
       .byte $00 ; |        | $FFEE
ShipShadow: ;6 bytes
       .byte $00 ; |        | $FFEF shared
       .byte $1E ; |   XXXX | $FFF0
       .byte $3F ; |  XXXXXX| $FFF1
       .byte $FF ; |XXXXXXXX| $FFF2
       .byte $7E ; | XXXXXX | $FFF3
       .byte $3C ; |  XXXX  | $FFF4
;       .byte $00 ; |        | $FFF5

       ORG $FFF5
       .byte "KH-2008"
       .word START,0
