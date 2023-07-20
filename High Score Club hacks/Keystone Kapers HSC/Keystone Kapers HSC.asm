;KEYSTONE KAPERS (c)1983 Activision ; Supercharger hack by Kurt (Nukey Shay) Howe, 8/30/2009
; PlusROM HSC hack added by Wolfgang Stubig (Al_Nafuur) 7/2023

PLUSROM = 1
PAL   = 0
PAL50 = 0

; Disassembly of Keystone.bin
; Disassembled Sat Aug 29 16:37:06 2009
; Using DiStella v3.0
; Command Line: C:\BIN\D3.EXE -pafscKeystone.cfg Keystone.bin
; Keystone.cfg contents:
;      ORG  F000
;      CODE F000 F11D
;      GFX  F11E F11E
;      CODE F11F F19B
;      GFX  F19C F19C
;      CODE F19D F2A4
;      GFX  F2A5 F2A5
;      CODE F2A6 F328
;      GFX  F329 F329
;      CODE F32A F32B
;      GFX  F32C F32C
;      CODE F32D F549
;      GFX  F54A F54A
;      CODE F54B F585
;      GFX  F586 F586
;      CODE F587 F588
;      GFX  F589 F589
;      CODE F58A FAFF
;      GFX  FB00 FCE6
;      CODE FCE7 FCED
;      GFX  FCEE FD0C
;      CODE FD0D FDEC
;      GFX  FDED FEBA
;      CODE FEBB FEC4
;      GFX  FEC5 FEF3
;      CODE FEF4 FEFF
;      GFX  FF00 FFFF

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
RESM1   =  $13
RESBL   =  $14
AUDC0   =  $15
AUDF0   =  $17
AUDV0   =  $19
GRP0    =  $1B
GRP1    =  $1C
ENAM0   =  $1D
ENAM1   =  $1E
ENABL   =  $1F
HMP0    =  $20
HMP1    =  $21
HMM0    =  $22
HMM1    =  $23
HMBL    =  $24
VDELP0  =  $25
VDELP1  =  $26
HMOVE   =  $2A
HMCLR   =  $2B
CXCLR   =  $2C
CXPPMM  =  $37
INPT4   =  $3C
SWCHA   =  $0280
SWCHB   =  $0282
INTIM   =  $0284
TIM64T  =  $0296

   IF PLUSROM = 1

WriteToBuffer           = $1FF0
WriteSendBuffer         = $1FF1
ReceiveBuffer           = $1FF2
ReceiveBufferSize       = $1FF3

HIGHSCORE_ID            = 70         ; Keystone Kapers game ID in Highscore DB

   ENDIF

       ORG $F000

START:
       sei                            ;2
       cld                            ;2
       ldx    #$FF                    ;2
       txs                            ;2
       inx                            ;2
       txa                            ;2
LF007:
       sta    VSYNC,X                 ;4
       inx                            ;2
       bne    LF007                   ;2
       ldy    #$0C                    ;2
       jsr    LFA2C                   ;6
LF011:
       ldx    #$03                    ;2
       stx    WSYNC                   ;3
       stx    VBLANK                  ;3
  IF PAL50
       lda    #$41                    ;2
  ELSE
       lda    #$23                    ;2
  ENDIF
       sta    TIM64T                  ;4
       ldy    #$FE                    ;2
LF01E:
       lda    #$48                    ;2
       sta    $EC,X                   ;4
       lda    #$05                    ;2
       sta    $B4,X                   ;4
       sty    $E8,X                   ;4
       dex                            ;2
       bpl    LF01E                   ;2
       ldy    $8E                     ;3
       bit    $87                     ;3
       bpl    LF061                   ;2
       ldy    #$05                    ;2
       lda    $8E                     ;3
       and    #$03                    ;2
       bne    LF05F                   ;2
       jsr    LFEF4                   ;6
       bcs    LF042                   ;2
       inc    $97                     ;5
       bcc    LF07F                   ;2 always branch

LF042:
       lda    #$01                    ;2
       sta    $8B                     ;3
       sta    $86                     ;3
       rol                            ;2
       ldy    $97                     ;3
       cpy    #$10                    ;2
       bcs    LF057                   ;2
       tya                            ;2
       and    #$08                    ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       adc    #$01                    ;2
LF057:
       jsr    LFDB6                   ;6
       lda    #$0C                    ;2
       jsr    LFDE4                   ;6
LF05F:
       ldy    #$00                    ;2
LF061:
       lda    $9D                     ;3
       cmp    #$11                    ;2
       bcc    LF069                   ;2
       ldy    #$00                    ;2
LF069:
       sty    $CA                     ;3
       ldy    #$07                    ;2
       bit    $98                     ;3
       bmi    LF082                   ;2
       lda    $87                     ;3
       ora    $88                     ;3
       bmi    LF089                   ;2
       lda    $89                     ;3
       cmp    #$09                    ;2
       bne    LF082                   ;2
       dec    $98                     ;5
LF07F:
       jmp    LF09A                   ;3

LF082:
       tya                            ;2
       and    $8E                     ;3
       bne    LF089                   ;2
       dec    $8A                     ;5
LF089:
       ldx    #$FF                    ;2
       cpx    SWCHA                   ;4
       beq    LF093                   ;2
       inx                            ;2
       stx    $99                     ;3
LF093:
       lsr    SWCHB                   ;6
       bcs    LF0AA                   ;2
       ldy    #$0C                    ;2
LF09A:
       ldx    #$41                    ;2
       lda    #$00                    ;2
LF09E:
       sta    $4E,X                   ;4
       dex                            ;2
       bne    LF09E                   ;2
       jsr    LFA2C                   ;6
       dec    $88                     ;5
       bmi    LF0B1                   ;2
LF0AA:
       jsr    LFCE7                   ;6
       bit    $88                     ;3
       bmi    LF0B4                   ;2
LF0B1:
       jmp    LF33C                   ;3

LF0B4:
       ldy    $8B                     ;3
       cpy    #$06                    ;2
       beq    LF0D6                   ;2
       inc    $F2                     ;5
       bne    LF0C0                   ;2
       inc    $F0                     ;5
LF0C0:
       lda    #$7F                    ;2
       cpy    #$05                    ;2
       bne    LF0C8                   ;2
       lda    #$03                    ;2
LF0C8:
       and    $8E                     ;3
       bne    LF0D6                   ;2
       jsr    LFEF4                   ;6
       bmi    LF0D3                   ;2
       bne    LF0D6                   ;2
LF0D3:
       jmp    LF327                   ;3

LF0D6:
       jsr    LFEBB                   ;6
       sta    $9F                     ;3
       cpy    #$05                    ;2
       bcc    LF0E2                   ;2
       jmp    LF338                   ;3

LF0E2:
       bit    $80                     ;3
       bpl    LF132                   ;2
       bvc    LF110                   ;2
       tay                            ;2
       rol                            ;2
       rol                            ;2
       rol                            ;2
       sec                            ;2
       rol                            ;2
       rol                            ;2
       sta    $91                     ;3
       lda    $A0,X                   ;4
       sta.wy $A0,Y                   ;5
       lda    LFEC5,Y                 ;4
       sta    $92                     ;3
       lda    $F2                     ;3
       bpl    LF0B1                   ;2
       cmp    #$F0                    ;2
       bcs    LF0B1                   ;2
       lda    SWCHA                   ;4
       and    #$20                    ;2
       bne    LF0B1                   ;2
       dec    $91                     ;5
       dec    $91                     ;5
       bpl    LF12B                   ;2
LF110:
       ldy    $80                     ;3
       lda    $8E                     ;3
       and    #$03                    ;2
       bne    LF0B1                   ;2
       cpy    #$A8                    ;2
       beq    LF11F                   ;2
       inc    $A0,X                   ;6
       .byte $2C                      ;4 skip 2 bytes
LF11F:
       dec    $A0,X                   ;6
       inc    $91                     ;5
       lda    $91                     ;3
       and    #$1F                    ;2
       bne    LF0B1                   ;2
       inc    $92                     ;5
LF12B:
       lda    #$0F                    ;2
       and    $80                     ;3
       jmp    LF1D3                   ;3

LF132:
       lda    $E1                     ;3
       ldy    $8D                     ;3
       bne    LF13B                   ;2
       lda    SWCHA                   ;4
LF13B:
       bit    $98                     ;3
       bpl    LF142                   ;2
       lda    LFCF6,X                 ;4
LF142:
       sta    $9E                     ;3
       and    #$20                    ;2
       bne    LF14D                   ;2
       lda    #$07                    ;2
       jmp    LF27E                   ;3

LF14D:
       lda    $9E                     ;3
       asl                            ;2
       bmi    LF175                   ;2
       lda    #$08                    ;2
       sta    $80                     ;3
       dec    $A0,X                   ;6
       lda    #$07                    ;2
       cmp    $A0,X                   ;4
       bcc    LF166                   ;2
       cmp    $93                     ;3
       bne    LF169                   ;2
       lda    #$09                    ;2
       sta    $A0,X                   ;4
LF166:
       jmp    LF1D8                   ;3

LF169:
       lda    #$9A                    ;2
       sta    $A0,X                   ;4
       txa                            ;2
       lsr                            ;2
       bcs    LF194                   ;2
LF171:
       inc    $92                     ;5
       bne    LF196                   ;2
LF175:
       bcs    LF1A5                   ;2
       lda    #$00                    ;2
       sta    $80                     ;3
       ldy    $A0,X                   ;4
       iny                            ;2
       cpy    #$9B                    ;2
       bcc    LF188                   ;2
       lda    $93                     ;3
       bne    LF18C                   ;2
       ldy    #$99                    ;2
LF188:
       sty    $A0,X                   ;4
       bne    LF1D8                   ;2
LF18C:
       ldy    #$08                    ;2
       sty    $A0,X                   ;4
       txa                            ;2
       lsr                            ;2
       bcs    LF171                   ;2
LF194:
       dec    $92                     ;5
LF196:
       lda    $80                     ;3
       bne    LF19D                   ;2
       dec    $93                     ;5
       .byte $2C                      ;4 skip 2 bytes
LF19D:
       inc    $93                     ;5
       jsr    LFA4D                   ;6
       jmp    LF1FB                   ;3

LF1A5:
       ldy    #$06                    ;2
       sty    $95                     ;3
       asl                            ;2
       asl                            ;2
       bmi    LF1D5                   ;2
       lda    $93                     ;3
       cmp    #$04                    ;2
       bne    LF1D5                   ;2
       lda    $8D                     ;3
       bne    LF1D5                   ;2
       lda    $F2                     ;3
       bpl    LF1D5                   ;2
       cmp    #$F0                    ;2
       bcs    LF1D5                   ;2
       cpx    $9F                     ;3
       bne    LF1D5                   ;2
       lda    $A0,X                   ;4
       sbc    #$4C                    ;2
       cmp    #$03                    ;2
       bcs    LF1D5                   ;2
       inc    $91                     ;5
       inc    $91                     ;5
       lda    #$E0                    ;2
       ora    $80                     ;3
LF1D3:
       sta    $80                     ;3
LF1D5:
       jmp    LF280                   ;3

LF1D8:
       lda    $A0,X                   ;4
       lsr                            ;2
       ldy    $95                     ;3
       bcc    LF1E8                   ;2
       iny                            ;2
       cpy    #$06                    ;2
       bcc    LF1E6                   ;2
       ldy    #$01                    ;2
LF1E6:
       sty    $95                     ;3
LF1E8:
       lda    $8B                     ;3
       bne    LF1FB                   ;2
       cpy    #$04                    ;2
       bne    LF1FB                   ;2
       lda    #$08                    ;2
       jsr    LFDE4                   ;6
       lda    #$02                    ;2
       sta    $8B                     ;3
       sta    $86                     ;3
LF1FB:
       jsr    LFCE7                   ;6
       lda    $93                     ;3
       cmp    LFD09,X                 ;4
       bne    LF20F                   ;2
       lda    $94                     ;3
       cmp    $92                     ;3
       bne    LF280                   ;2
       dec    $94                     ;5
       bpl    LF225                   ;2
LF20F:
       cmp    LFD0A,X                 ;4
       bne    LF280                   ;2
       lda    $94                     ;3
       sbc    $92                     ;3
       cmp    #$01                    ;2
       beq    LF225                   ;2
       tay                            ;2
       bne    LF239                   ;2
       inc    $94                     ;5
       lda    #$50                    ;2
       sta    $90                     ;3
LF225:
       txa                            ;2
       lsr                            ;2
       bcs    LF231                   ;2
       lda    #$78                    ;2
       cmp    $90                     ;3
       bcc    LF239                   ;2
       bcs    LF237                   ;2 always branch

LF231:
       lda    #$28                    ;2
       cmp    $90                     ;3
       bcs    LF239                   ;2
LF237:
       sta    $90                     ;3
LF239:
       lda    $93                     ;3
       lsr                            ;2
       lda    $91                     ;3
       and    #$1F                    ;2
       bcc    LF244                   ;2
       eor    #$FF                    ;2
LF244:
       clc                            ;2
       adc    LFBFA,X                 ;4
       clc                            ;2
       eor    #$FF                    ;2
       adc    $A0,X                   ;4
       cmp    #$02                    ;2
       bcs    LF280                   ;2
       lsr                            ;2
       sta    $8D                     ;3
       lda    #$FC                    ;2
       sta    $8C                     ;3
       and    $91                     ;3
       sta    $91                     ;3
       lda    $F2                     ;3
       lsr                            ;2
       lsr                            ;2
       and    #$03                    ;2
       ora    $91                     ;3
       sta    $91                     ;3
       and    #$1F                    ;2
       cpx    #$01                    ;2
       beq    LF271                   ;2
       eor    #$FF                    ;2
       clc                            ;2
       adc    #$03                    ;2
LF271
       clc                            ;2
       adc    LFBFA,X                 ;4
       sta    $A0,X                   ;4
       lda    LFBFD,X                 ;4
       sta    $80                     ;3
       lda    #$06                    ;2
LF27E:
       sta    $95                     ;3
LF280:
       ldy    $8D                     ;3
       beq    LF2B8                   ;2
       lda    $8B                     ;3
       cmp    #$03                    ;2
       bne    LF28E                   ;2
       tya                            ;2
       lsr                            ;2
       sta    AUDV0                   ;3
LF28E:
       tya                            ;2
       sec                            ;2
       sbc    #$06                    ;2
       cmp    #$06                    ;2
       bcs    LF29C                   ;2
       lda    $8E                     ;3
       and    #$03                    ;2
       bne    LF2A8                   ;2
LF29C:
       dey                            ;2
       sty    $8D                     ;3
       cpy    #$09                    ;2
       bcc    LF2A6                   ;2
       inc    $91                     ;5
       .byte $2C                      ;4 skip 2 bytes
LF2A6:
       dec    $91                     ;5
LF2A8:
       lda    #$02                    ;2
       sta    $95                     ;3
       cpy    #$10                    ;2
       bcc    LF2DC                   ;2
       bit    $E1                     ;3
       bpl    LF2DC                   ;2
       bvs    LF2D3                   ;2
       bmi    LF2DC                   ;2 always branch

LF2B8:
       ldy    #$00                    ;2
       lda    INPT4                   ;3
       ora    $98                     ;3
       bmi    LF2DA                   ;2
       bit    $8C                     ;3
       bmi    LF2DC                   ;2
       ldx    #$03                    ;2
       stx    $8B                     ;3
       lda    #$0C                    ;2
       sta    AUDC0                   ;3
       ldx    #$12                    ;2
       stx    $8D                     ;3
       dex                            ;2
       stx    $86                     ;3
LF2D3:
       lda    SWCHA                   ;4
       sta    $E1                     ;3
       ldy    #$FF                    ;2
LF2DA:
       sty    $8C                     ;3
LF2DC:
       jsr    LFCE7                   ;6
       bit    $8F                     ;3
       bmi    LF327                   ;2
       lda    $87                     ;3
       ora    $80                     ;3
       bmi    LF33C                   ;2
       lda    $81,X                   ;4
       bpl    LF33C                   ;2
       ldy    $AC,X                   ;4
       cpy    #$06                    ;2
       bcc    LF321                   ;2
       bit    $98                     ;3
       bmi    LF33C                   ;2
       cpy    #$08                    ;2
       bcs    LF303                   ;2
       lda    $91                     ;3
       and    #$1F                    ;2
       cmp    #$06                    ;2
       bcs    LF33C                   ;2
LF303:
       cpy    #$0A                    ;2
       bcc    LF32D                   ;2
       cpy    #$0C                    ;2
       bcc    LF327                   ;2
       cpy    #$0F                    ;2
       bcc    LF33C                   ;2
       stx    $9E                     ;3
       sty    $F5,X                   ;4
       lda    #$02                    ;2
       sta    $AC,X                   ;4
       tax                            ;2
       lda    #$50                    ;2
       jsr    LFDB8                   ;6
       ldx    $9E                     ;3
       bpl    LF32A                   ;2
LF321:
       dec    $87                     ;5
       inc    $88                     ;5
       beq    LF33C                   ;2
LF327:
       lda    #$06                    ;2
       .byte $2C                      ;4 skip 2 bytes
LF32A:
       lda    #$04                    ;2
       .byte $2C                      ;4 skip 2 bytes
LF32D:
       lda    #$05                    ;2
       sta    $8B                     ;3
       ldy    #$08                    ;2
       sty    $86                     ;3
       dey                            ;2
       sty    AUDC0                   ;3
LF338:
       lda    $86                     ;3
       sta    AUDV0                   ;3
LF33C:
       jsr    LFCE7                   ;6
       ldy    $95                     ;3
       lda    $91                     ;3
       and    #$1F                    ;2
       eor    #$FF                    ;2
       sta    $9E                     ;3
       clc                            ;2
       adc    LFECA,Y                 ;4
       sta    $E4,X                   ;4
       lda    #$35                    ;2
       cpy    #$07                    ;2
       bne    LF357                   ;2
       adc    #$02                    ;2
LF357:
       adc    $9E                     ;3
       sta    $EC,X                   ;4
       ldy    $80                     ;3
       sty    $F9,X                   ;4
       lda    $9E                     ;3
       cmp    #$F3                    ;2
       bcs    LF377                   ;2
       sty    $FA,X                   ;4
       lda    $E4,X                   ;4
       adc    #$20                    ;2
       sta    $E5,X                   ;4
       lda    $EC,X                   ;4
       adc    #$20                    ;2
       sta    $ED,X                   ;4
       lda    $A0,X                   ;4
       sta    $A1,X                   ;4
LF377:
       ldy    $8B                     ;3
       beq    LF3BB                   ;2
       cpy    #$04                    ;2
       bcc    LF385                   ;2
       lda    $8E                     ;3
       and    #$03                    ;2
       bne    LF3C1                   ;2
LF385:
       dec    $86                     ;5
       bmi    LF39E                   ;2
       lda    LFCF9,Y                 ;4
       sta    $9E                     ;3
       lda    #$FC                    ;2
       sta    $9F                     ;3
       ldy    $86                     ;3
       lda    ($9E),Y                 ;5
       bit    $98                     ;3
       bmi    LF3C1                   ;2
       sta    AUDF0                   ;3
       bpl    LF3C1                   ;2 always branch

LF39E:
       cpy    #$04                    ;2
       bcc    LF3BB                   ;2
       cpy    #$06                    ;2
       bne    LF3B9                   ;2
       ldy    $96                     ;3
       dey                            ;2
       bpl    LF3B2                   ;2
    IF PLUSROM = 1
       jsr SendPlusROMScore
    ELSE
       inc    $88                     ;5
       iny                            ;2
    ENDIF
       sty    $89                     ;3
       beq    LF3BB                   ;2
LF3B2:
       sty    $96                     ;3
       ldy    #$05                    ;2
       jmp    LF09A                   ;3

LF3B9:
       sta    $AC,X                   ;4
LF3BB:
       lda    #$00                    ;2
       sta    $8B                     ;3
       sta    AUDV0                   ;3
LF3C1:
       ldx    INTIM                   ;4
       bne    LF3C1                   ;2
       ldy    #$82                    ;2
       sty    WSYNC                   ;3
       sty    VSYNC                   ;3
       sty    WSYNC                   ;3
       sty    WSYNC                   ;3
       sty    WSYNC                   ;3
       stx    VSYNC                   ;3
       stx    GRP0                    ;3 moved here to save a scanline
       stx    GRP1                    ;3 ""
       stx    GRP0                    ;3 ""
       stx    PF1                     ;3 ""
       stx    ENABL                   ;3 ""
       inc    $8E                     ;5
       bne    LF3E7                   ;2
       inc    $89                     ;5
       lda    $89                     ;3
       and    #$07                    ;2
       bne    LF3E7                   ;2
       inc    $99                     ;5
       bne    LF3E7                   ;2
       sec                            ;2
       ror    $99                     ;5
LF3E7:
  IF PAL50
       lda    #$4B                    ;2
  ELSE
       lda    #$2F                    ;2
  ENDIF
       sta    WSYNC                   ;3
       sta    TIM64T                  ;4
       lda    $88                     ;3
       bpl    LF45A                   ;2
       lda    $8F                     ;3
       bmi    LF45A                   ;2
       lda    $8E                     ;3
       lsr                            ;2
       bcc    LF45A                   ;2
       ldy    $90                     ;3
       lda    $CC                     ;3
       and    #$08                    ;2
       bne    LF428                   ;2
       iny                            ;2
       cpy    #$9C                    ;2
       bcc    LF449                   ;2
       lda    $94                     ;3
       cmp    #$1F                    ;2
       bne    LF412                   ;2
       dec    $8F                     ;5
       bmi    LF45A                   ;2
LF412:
       ldy    #$04                    ;2
       lda    $94                     ;3
       and    #$08                    ;2
       bne    LF435                   ;2
LF41A:
       dec    $94                     ;5
       bpl    LF424                   ;2
       inc    $94                     ;5
       ldy    #$9A                    ;2
       bne    LF449                   ;2 always branch

LF424:
       lda    #$07                    ;2
       bpl    LF439                   ;2 always branch

LF428:
       dey                            ;2
       cpy    #$04                    ;2
       bcs    LF449                   ;2
       ldy    #$9B                    ;2
       lda    $94                     ;3
       and    #$08                    ;2
       bne    LF41A                   ;2
LF435:
       inc    $94                     ;5
       lda    #$00                    ;2
LF439:
       sta    $85                     ;3
       lda    #$07                    ;2
       and    $94                     ;3
       cmp    $85                     ;3
       bne    LF449                   ;2
       tya                            ;2
       eor    #$FF                    ;2
       adc    #$9E                    ;2
       tay                            ;2
LF449:
       sty    $90                     ;3
       tya                            ;2
       lsr                            ;2
       bcc    LF45A                   ;2
       ldy    $E2                     ;3
       iny                            ;2
       cpy    #$06                    ;2
       bcc    LF458                   ;2
       ldy    #$01                    ;2
LF458:
       sty    $E2                     ;3
LF45A:
       lda    $94                     ;3
       and    #$0F                    ;2
       cmp    #$08                    ;2
       bcc    LF464                   ;2
       eor    #$0F                    ;2
LF464:
       sta    $85                     ;3
       lda    $94                     ;3
       and    #$18                    ;2
       sta    $9E                     ;3
       eor    #$08                    ;2
       sta    $CC                     ;3
       lda    $92                     ;3
       and    #$18                    ;2
       cmp    $9E                     ;3
       beq    LF480                   ;2
       bcc    LF48C                   ;2
       lda    $CC                     ;3
       eor    #$08                    ;2
       bcs    LF48A                   ;2 always branch

LF480:
       lda    #$00                    ;2
       ldy    $85                     ;3
       cpy    $93                     ;3
       bcc    LF48A                   ;2
       lda    #$08                    ;2
LF48A:
       sta    $CC                     ;3
LF48C:
       ldx    #$03                    ;2
LF48E:
       lda    $94                     ;3
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       sta    $9E                     ;3
       cpx    $9E                     ;3
       bne    LF4D7                   ;2
       lda    $85                     ;3
       cmp    $93                     ;3
       bne    LF4D7                   ;2
       ldy    $E2                     ;3
       lda    $94                     ;3
       cmp    $92                     ;3
       beq    LF4BF                   ;2
       lda    $CC                     ;3
       sta    $F9,X                   ;4
       lda    #$01                    ;2
       sta    $EC,X                   ;4
       clc                            ;2
       adc    LFFCE,Y                 ;4
       sta    $E4,X                   ;4
       lda    #$FB                    ;2
       sta    $E8,X                   ;4
       lda    $90                     ;3
       sta    $A0,X                   ;4
       bne    LF4D7                   ;2 always branch?
LF4BF:
       sty    $AC,X                   ;4
       ldy    #$08                    ;2
       lda    $90                     ;3
       sta    $A4,X                   ;4
       cmp    $A0,X                   ;4
       bcc    LF4CD                   ;2
       ldy    #$00                    ;2
LF4CD:
       sty    $CC                     ;3
       sty    $B0,X                   ;4
       ldy    #$1A                    ;2
       sty    $A8,X                   ;4
       bne    LF4FF                   ;2 always branch

LF4D7:
       lda    $AC,X                   ;4
       cmp    #$06                    ;2
       bcc    LF4F8                   ;2
       cmp    #$08                    ;2
       bcs    LF502                   ;2
       lda    #$01                    ;2
       jsr    LFD91                   ;6
       bcc    LF4F8                   ;2
       lda    $8E                     ;3
       lsr                            ;2
       tay                            ;2
       lda    LF6D4,Y                 ;4
       and    #$01                    ;2
       clc                            ;2
       adc    #$06                    ;2
       sta    $AC,X                   ;4
       bpl    LF4FF                   ;2 always branch

LF4F8:
       ldy    #$00                    ;2
       sty    $AC,X                   ;4
       iny                            ;2
       sty    $A4,X                   ;4
LF4FF:
       jmp    LF5B5                   ;3

LF502:
       bne    LF554                   ;2
       lda    #$00                    ;2
       jsr    LFD91                   ;6
       ldy    #$0D                    ;2
       cmp    #$04                    ;2
       bcs    LF514                   ;2
       lsr                            ;2
       bcs    LF514                   ;2
       ldy    #$03                    ;2
LF514:
       sty    $D9                     ;3
       tya                            ;2
       clc                            ;2
       adc    #$08                    ;2
       asl                            ;2
       sta    $9E                     ;3
       ldy    $F1                     ;3
       iny                            ;2
       cpy    $9E                     ;3
       bcc    LF526                   ;2
       ldy    #$00                    ;2
LF526:
       sty    $9F                     ;3
       tya                            ;2
       sec                            ;2
       sbc    $D9                     ;3
       cmp    #$10                    ;2
       bcs    LF53E                   ;2
       cmp    #$08                    ;2
       bcc    LF536                   ;2
       eor    #$0F                    ;2
LF536:
       tay                            ;2
       lda    LFCDF,Y                 ;4
       and    $8E                     ;3
       bne    LF54D                   ;2
LF53E:
       ldy    $9F                     ;3
       sty    $F1                     ;3
       lsr    $9E                     ;5
       cpy    $9E                     ;3
       bcs    LF54B                   ;2
       dec    $A8,X                   ;6
       .byte $2C                      ;4 skip 2 bytes
LF54B:
       inc    $A8,X                   ;6
LF54D:
       lda    $8E                     ;3
       lsr                            ;2
       bcc    LF4FF                   ;2
       bcs    LF587                   ;2 always branch

LF554:
       cmp    #$09                    ;2
       bne    LF568                   ;2
       lda    #$02                    ;2
       jsr    LFD91                   ;6
LF55D:
       bcc    LF4F8                   ;2
       cmp    #$03                    ;2
       bcs    LF587                   ;2
       lsr                            ;2
       bcs    LF58A                   ;2
       bcc    LF587                   ;2 always branch

LF568:
       cmp    #$0C                    ;2
       bcs    LF5B5                   ;2
       lda    #$09                    ;2
       sta    $A8,X                   ;4
       lda    $8E                     ;3
       and    #$02                    ;2
       lsr                            ;2
       adc    #$0A                    ;2
       sta    $AC,X                   ;4
       lda    #$03                    ;2
       jsr    LFD91                   ;6
       bcc    LF55D                   ;2
       cmp    #$04                    ;2
       bcc    LF586                   ;2
       lda    #$03                    ;2
LF586:
       .byte $2C                      ;4 skip 2 bytes
LF587:
       lda    #$00                    ;2
       .byte $2C                      ;4 skip 2 bytes
LF58A:
       lda    #$01                    ;2
       sta    $9E                     ;3
LF58E:
       lda    $8B                     ;3
       cmp    #$05                    ;2
       bcs    LF5B5                   ;2
       bit    $88                     ;3
       bpl    LF5B5                   ;2
       ldy    #$01                    ;2
       lda    #$08                    ;2
       and    $B0,X                   ;4
       bne    LF5A2                   ;2
       ldy    #$FF                    ;2
LF5A2:
       tya                            ;2
       adc    $A4,X                   ;4
       bne    LF5A9                   ;2
       lda    #$A0                    ;2
LF5A9:
       cmp    #$A1                    ;2
       bcc    LF5AF                   ;2
       lda    #$01                    ;2
LF5AF:
       sta    $A4,X                   ;4
       dec    $9E                     ;5
       bpl    LF58E                   ;2
LF5B5:
       dex                            ;2
       bmi    LF5BB                   ;2
       jmp    LF48E                   ;3

LF5BB:
       ldy    #$18                    ;2
       lda    $F2                     ;3
       lsr                            ;2
       lsr                            ;2
       and    #$03                    ;2
       sta    $9E                     ;3
       ldx    $93                     ;3
       beq    LF5D1                   ;2
       cpx    #$07                    ;2
       bne    LF5ED                   ;2
       adc    #$03                    ;2
       ldy    #$31                    ;2
LF5D1:
       tax                            ;2
       lda    #$8A                    ;2
       sec                            ;2
       sbc    $9E                     ;3
       sta    $F3                     ;3
       lda    LFFC6,X                 ;4
       sta    $F4                     ;3
       ldx    #$18                    ;2
LF5E0:
       lda    LFBC1,Y                 ;4
       cmp    #$FF                    ;2
       beq    LF5E9                   ;2
       sta    $A4,X                   ;4
LF5E9:
       dey                            ;2
       dex                            ;2
       bpl    LF5E0                   ;2
LF5ED:
       cpx    #$04                    ;2
       beq    LF5F4                   ;2
       jmp    LF686                   ;3

LF5F4:
       dex                            ;2
       lda    #$06                    ;2
LF5F7:
       sta    $B7,X                   ;4
       dex                            ;2
       bne    LF5F7                   ;2
       jsr    LFEBB                   ;6
       tax                            ;2
       lda    $F2                     ;3
       and    #$7F                    ;2
       cmp    #$70                    ;2
       bcc    LF662                   ;2
       lsr                            ;2
       lsr                            ;2
       and    #$03                    ;2
       cmp    #$03                    ;2
       bne    LF63E                   ;2
       lda    #$80                    ;2
       bit    $F2                     ;3
       bpl    LF62A                   ;2
       lda    $F0                     ;3
       and    #$03                    ;2
       tay                            ;2
       lda    $94                     ;3
       cmp    LFEC6,Y                 ;4
       bne    LF626                   ;2
       dec    $F2                     ;5
       bne    LF67E                   ;2
LF626:
       lda    #$00                    ;2
       inc    $F0                     ;5
LF62A:
       sta    $F2                     ;3
       ldy    #$1A                    ;2
       sty    $A8,X                   ;4
       ldy    #$01                    ;2
       sty    $A4,X                   ;4
       ldy    $FD                     ;3
       sty    $B0,X                   ;4
       lda    $F1                     ;3
       sta    $AC,X                   ;4
       bpl    LF676                   ;2
LF63E:
       bit    $F2                     ;3
       bmi    LF646                   ;2
       eor    #$03                    ;2
       sbc    #$00                    ;2
LF646:
       ldy    #$18                    ;2
       sty    $A8,X                   ;4
       ldy    #$4C                    ;2
       sty    $A4,X                   ;4
       ldy    #$04                    ;2
       sty    $B4,X                   ;4
       ldy    #$0E                    ;2
       sty    $AC,X                   ;4
       tay                            ;2
       lda    LFDFD,Y                 ;4
       sta    $B0,X                   ;4
       bne    LF660                   ;2
       inc    $A4,X                   ;6
LF660:
       bne    LF67A                   ;2
LF662:
       cmp    #$6F                    ;2
       bne    LF676                   ;2
       lda    $A4,X                   ;4
       cmp    #$03                    ;2
       bcc    LF66E                   ;2
       dec    $F2                     ;5
LF66E:
       lda    $AC,X                   ;4
       sta    $F1                     ;3
       lda    $B0,X                   ;4
       sta    $FD                     ;3
LF676:
       bit    $F2                     ;3
       bpl    LF67E                   ;2
LF67A:
       inc    $B8,X                   ;6
       bne    LF686                   ;2
LF67E:
       bit    $80                     ;3
       bpl    LF686                   ;2
       lda    #$48                    ;2
       sta    $EC,X                   ;4
LF686:
       ldx    #$03                    ;2
LF688:
       txa                            ;2
       asl                            ;2
       asl                            ;2
       tay                            ;2
       lda    $9A,X                   ;4
       and    #$F0                    ;2
       lsr                            ;2
       sta.wy $BD,Y                   ;5
       lda    $9A,X                   ;4
       and    #$0F                    ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       sta.wy $BF,Y                   ;5
       dex                            ;2
       bpl    LF688                   ;2
       inx                            ;2
       ldy    #$58                    ;2
LF6A4:
       lda    $BD,X                   ;4
       bne    LF6B0                   ;2
       sty    $BD,X                   ;4
       inx                            ;2
       inx                            ;2
       cpx    #$0A                    ;2
       bcc    LF6A4                   ;2
LF6B0:
       jsr    LFCE7                   ;6
       lda    $A0,X                   ;4
       ldy    $93                     ;3
       jsr    LFDA7                   ;6
       sta    $9E                     ;3
       lda    $90                     ;3
       ldy    $85                     ;3
       jsr    LFDA7                   ;6
       sta    $9F                     ;3
       ldy    #$09                    ;2
LF6C7:
       lda.wy $9E,Y                   ;4
       jsr    LFD0D                   ;6
       bpl    LF6C7                   ;2
  IF PAL
       lda    #$D8                    ;2
  ELSE
       lda    #$88                    ;2
  ENDIF
       sta    COLUBK                  ;3
LF6D4:
       lda    INTIM                   ;4
       bne    LF6D4                   ;2
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       bit    $99                     ;3
       bpl    LF6E3                   ;2
       lda    #$02                    ;2
LF6E3:
       sta    VBLANK                  ;3
       jsr    LFD24                   ;6
       tax                            ;2
       lda    $C9                     ;3
       sta    $C5                     ;3
       lda    $CB                     ;3
       sta    $C7                     ;3
       ldy    #$40                    ;2
       sty    HMP0                    ;3
       sty    HMP1                    ;3
       ldy    $96                     ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
LF6FD:
       lda    #$58                    ;2
       dey                            ;2
       bmi    LF704                   ;2
       lda    #$50                    ;2
LF704:
       sta    $BD,X                   ;4
       inx                            ;2
       inx                            ;2
       cpx    #$06                    ;2
       bne    LF6FD                   ;2
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       inx                            ;2
       stx    $9F                     ;3
       lda    #$58                    ;2
       sta    $C3                     ;3
       ldx    $CA                     ;3
       stx    COLUP0                  ;3
       stx    COLUP1                  ;3
       jsr    LFD5A                   ;6
       sta    VDELP1                  ;3
       sta    NUSIZ1                  ;3
       sta    CXCLR                   ;3
       lda    #$FB                    ;2
       sta    $BE                     ;3
       lda    #$FC                    ;2
       sta    $C8                     ;3
       sta    $C2                     ;3
       sta    $C0                     ;3
       ldy    #$03                    ;2
       bpl    LF73C                   ;2 always branch

LF736:
       jmp    LF8A6                   ;3

LF739:
       ldy    $C4                     ;3
       dey                            ;2
LF73C:
       bmi    LF736                   ;2
       lda.wy $E4,Y                   ;4
       sta    $C5                     ;3
       lda.wy $E8,Y                   ;4
       sta    $C6                     ;3
       sty    $C4                     ;3
       ldx    #$22                    ;2
       stx    HMBL                    ;3
       lda    #$31                    ;2
       sta    CTRLPF                  ;3
       lda    #$00                    ;2
       stx    ENABL                   ;3
       sta    RESBL                   ;3
       sta    HMOVE                   ;3
       sta    GRP0                    ;3
       sta    GRP1                    ;3
       sta    PF1                     ;3
       sta    COLUPF                  ;3
       ldx    $B9,Y                   ;4
       cpx    #$08                    ;2
       bne    LF76A                   ;2
       lda    #$F0                    ;2
LF76A:
       sta    PF2                     ;3
       ldx    $AC,Y                   ;4
       lda    LFE00,X                 ;4
       sta    $C3                     ;3
       lda    LFFCE,X                 ;4
       sta    $BD                     ;3
       lda    LFFDF,X                 ;4
       sta    $C1                     ;3
       lda.wy $F9,Y                   ;4
       sta    REFP1                   ;3
       lda    CXPPMM                  ;3
       sta.wy $82,Y                   ;5
       lda.wy $D9,Y                   ;4
       sta    WSYNC                   ;3
       sta    HMP1                    ;3
       lda    #$00                    ;2
       sta    PF2                     ;3
       ldx    $CF,Y                   ;4
LF794:
       dex                            ;2
       bpl    LF794                   ;2
       ldx    $D3,Y                   ;4
       sta    RESP1                   ;3
       sta    WSYNC                   ;3
       lda.wy $DD,Y                   ;4
       sta    HMP0                    ;3
       lda    LFEDF,Y                 ;4
       sta    COLUBK                  ;3
LF7A7:
       dex                            ;2
       bpl    LF7A7                   ;2
       NOP                            ;2 waste
       sta    RESP0                   ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       sta    COLUPF                  ;3
       ldx    $B8,Y                   ;4
       lda.wy $EC,Y                   ;4
       sta    $C7                     ;3
       lda    LFE22,X                 ;4
       sta    PF2                     ;3
       lda    LFE1A,X                 ;4
       sta    PF1                     ;3
       lda    LFE11,X                 ;4
       ldx    $B4,Y                   ;4
       NOP                            ;2 waste
       sta    RESBL                   ;3
       bne    LF7D0                   ;2
       sta    RESBL                   ;3
LF7D0:
       cpx    #$04                    ;2
       bcs    LF7D6                   ;2
       lda    $F3                     ;3
LF7D6:
       sta    $BF                     ;3
       sta    HMCLR                   ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3 3
       lda    #$21                    ;2 5
       sta    CTRLPF                  ;3 8
       lda    LFFC0,X                 ;4 12
       sta    GRP0                    ;3 15
       lda.wy $B0,Y                   ;4 19
       sta    NUSIZ0                  ;3 22
       sta    REFP0                   ;3 25
       lda    LFCDB,X                 ;4 29
       sta    ENABL                   ;3 32
       sta    HMP0                    ;3 35
       cpy    #$03                    ;2 37
       beq    LF819                   ;2 39
  IF PAL
       ldx    #$50                    ;2 41 X is used to waste cycles ahead
  ELSE
       ldx    #$C0                    ;2 41 X is used to waste cycles ahead
  ENDIF
       stx    COLUP0                  ;3 44
  IF PAL
       lda    $F4-$50,X               ;4 48 waste add'l cycle
       sta    CXCLR+$B0,X             ;4 52 waste add'l cycle
       sta    HMBL+$B0,X              ;4 56 waste add'l cycle
  ELSE
       lda    $F4-$C0,X               ;4 48 waste add'l cycle
       sta    CXCLR+$40,X             ;4 52 waste add'l cycle
       sta    HMBL+$40,X              ;4 56 waste add'l cycle
  ENDIF
       ldx    $A8,Y                   ;4 60
       ldy    #$1A                    ;2 62
       lda    ($C7),Y                 ;5 67
       sta    COLUP1                  ;3 70
  IF PAL
       lda    #$54                    ;2 72
  ELSE
       lda    #$C4                    ;2 72
  ENDIF
       sta.w  COLUBK                  ;4 0  recolor platform...right at the last cycle
       sta    HMOVE                   ;3
       lda    ($C7),Y                 ;5
       jmp    LF863                   ;3

LF819:
       lda    $AB                     ;3
       sbc    #$06                    ;2
       sta    $9E                     ;3
       lda    #$04                    ;2
       sta    COLUPF                  ;3
       ldy    #$1D                    ;2
LF825:
       dey                            ;2
       lda    ($C7),Y                 ;5
       tax                            ;2
       and    #$01                    ;2
       beq    LF82F                   ;2
       lda    ($C5),Y                 ;5
LF82F:
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       sta    GRP1                    ;3
       stx    COLUP1                  ;3
       ldx    LFD00-20,Y              ;4+1 (crossing page)
       stx    COLUBK                  ;3
       lda    LFE50-20,Y              ;4
       sta    PF0                     ;3
       sta    PF1                     ;3
       sta    PF2                     ;3
       ldx    $9E                     ;3
       cpy    #$14                    ;2
       beq    LF873                   ;2
       bne    LF825                   ;2 always branch

LF84D:
       jsr    LFEFE                   ;6
  IF PAL
       lda    #$50                    ;2
       bne    LF883                   ;2 always branch
  ELSE
       lda    #$C0                    ;2
       bmi    LF883                   ;2 always branch
  ENDIF

LF854: ;BEQ destination
       sta.w  GRP1                    ;4
       beq    LF86B                   ;2 always branch

LF859:
       dex                            ;2
       sta.w  COLUP0                  ;4
       sta    HMOVE                   ;3
       lda    ($C7),Y                 ;5
       sta    COLUP1                  ;3
LF863:
       and    #$01                    ;2
       beq    LF854                   ;2
       lda    ($C5),Y                 ;5
       sta    GRP1                    ;3
LF86B:
       lda    ($BF),Y                 ;5
       sta    COLUPF                  ;3
       and    $BC                     ;3
       sta    HMBL                    ;3
LF873:
       sty    $9E                     ;3
       cpx    $C3                     ;3
       bcs    LF84D                   ;2
       txa                            ;2
       tay                            ;2
       lda    ($BD),Y                 ;5
       sta    GRP0                    ;3
       lda    ($C1),Y                 ;5
       ldy    $9E                     ;3
LF883:
       dey                            ;2
       bne    LF859                   ;2
       ldx    #$18                    ;2
       sta    COLUP0                  ;3
       sta    HMOVE                   ;3
       stx    COLUBK                  ;3
       lda    ($C7),Y                 ;5
       sta    COLUP1                  ;3
       and    #$01                    ;2
       beq    LF8A1                   ;2
       lda    ($C5),Y                 ;5
       sta    GRP1                    ;3
LF89A:
       sty    ENABL                   ;3
       NOP                            ;2 waste
       NOP                            ;2 waste
       jmp    LF739                   ;3

LF8A1: ;BEQ destination
       sta.w  GRP1                    ;4
       beq    LF89A                   ;2 always branch

LF8A6:
       iny                            ;2
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       sty    PF1                     ;3
       sty    PF2                     ;3
       sty    GRP0                    ;3
       sty    GRP1                    ;3
       sty    GRP0                    ;3
       sty    REFP1                   ;3
       sty    REFP0                   ;3
       sty    VDELP0                  ;3
       sty    NUSIZ0                  ;3
       lda    $91                     ;3
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       eor    #$FF                    ;2
       clc                            ;2
       sta    RESBL                   ;3
       adc    #$65                    ;2
       sta    $CF                     ;3
       lda    CXPPMM                  ;3
       sta    $81                     ;3
       lda    #$FC                    ;2
       sta    $D0                     ;3
       sta    $D4                     ;3
       sta    HMCLR                   ;3
       ldx    $CD                     ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       sta    $DA                     ;3
       lda    #$0F                    ;2
       sta    COLUP1                  ;3
       lda    $D7                     ;3
       NOP                            ;2 waste
LF8E6:
       dex                            ;2
       bpl    LF8E6                   ;2
       sta    RESM0                   ;3
       sta    HMCLR                   ;3
       sta    HMM0                    ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       sty    COLUP0                  ;3
  IF PAL
       ldy    #$26                    ;2
  ELSE
       ldy    #$16                    ;2
  ENDIF
       sty    COLUBK                  ;3
       NOP                            ;2 waste
       ldx    $CE                     ;3
LF8FC:
       dex                            ;2
       bpl    LF8FC                   ;2
       sta    RESM1                   ;3
       lda    $D8                     ;3
       sta    HMCLR                   ;3
       sta    HMM1                    ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       sty    COLUPF                  ;3
       lda    #$11                    ;2
       sta    CTRLPF                  ;3
       lda    $F0                     ;3
       and    #$03                    ;2
       tay                            ;2
       lda    LFBF6,Y                 ;4
       sta    $D9                     ;3
       lda    $94                     ;3
       and    #$18                    ;2
       lsr                            ;2
       eor    #$FF                    ;2
       adc    #$65                    ;2
       sta    $D3                     ;3
       ldy    #$10                    ;2
       lda    #$FC                    ;2
       sta    PF1                     ;3
       stx    PF0                     ;3
       sta    HMCLR                   ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       txa                            ;2
       ldx    #$0B                    ;2
LF937:
       sta    $BD,X                   ;4
       dex                            ;2
       dex                            ;2
       bpl    LF937                   ;2
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       lda    #$08                    ;2
       sta    COLUPF                  ;3
       bne    LF975                   ;2 always branch

LF947:
       dey                            ;2
       ldx    #$00                    ;2
  IF PAL
       lda    #$26                    ;2
  ELSE
       lda    #$16                    ;2
  ENDIF
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       sta    COLUBK                  ;3
       stx    GRP0                    ;3
       stx    ENAM0                   ;3
       stx    ENAM1                   ;3
       jsr    Waste_12_cycles         ;6
       lda    #$90                    ;2
       sta    RESP0                   ;3
       sta    HMP0                    ;3
       tya                            ;2
       and    #$04                    ;2
       bne    LF988                   ;2
       jsr    Waste_12_cycles         ;6
       sta    RESP0                   ;3
       bpl    LF988                   ;2 always branch

LF96D:
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       stx    GRP0                    ;3
       sta    ENABL                   ;3
LF975:
       lda    ($D3),Y                 ;5
       sta    ENAM1                   ;3
       lda    ($CF),Y                 ;5
       sta    ENAM0                   ;3
       lda    LFEE3,Y                 ;4
       sta    COLUBK                  ;3
       sta    HMCLR                   ;3
  IF PAL
       cmp    #$28                    ;2
  ELSE
       cmp    #$18                    ;2
  ENDIF
       beq    LF947                   ;2
LF988:
       lda    ($D9),Y                 ;5
       ldx    LFDEC,Y                 ;4
       dey                            ;2
       bpl    LF96D                   ;2
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       lda    #$08                    ;2
       sta    COLUBK                  ;3
       iny                            ;2
       sty    PF1                     ;3
       sty    PF0                     ;3
       ldy    #$07                    ;2
       lda    $8A                     ;3
       and    #$1F                    ;2
       cmp    #$14                    ;2
       bcs    LF9AE                   ;2
       ldy    #$00                    ;2
       sbc    #$0B                    ;2
       bcc    LF9AE                   ;2
       tay                            ;2
LF9AE:
       sty    $9F                     ;3
       tya                            ;2
       eor    #$07                    ;2
       sta    $85                     ;3
   IF PLUSROM = 1
       lda    #$58                    ;2
   ELSE
       lda    #$85                    ;2
   ENDIF
       ldx    #$0C                    ;2 just start X 4 bytes higher
       sec                            ;2
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
LF9BE:
       sta    $BF-4,X                 ;4
   IF PLUSROM = 1
       nop                            ;2
   ELSE
       sbc    #$08                    ;2
   ENDIF
       sta    $BD-4,X                 ;4
   IF PLUSROM = 1
       nop                            ;2
   ELSE
       sbc    #$08                    ;2
   ENDIF
       dex                            ;2
       dex                            ;2
       dex                            ;2
       dex                            ;2
       bne    LF9BE                   ;2 ...so that it's already zero when done
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       stx    COLUBK                  ;3 ...then use it to clear colors
       stx    COLUPF                  ;3 ""
       jsr    LFD28                   ;6
       lda    #$78                    ;2
       sta    PF1                     ;3
       lda    #$31                    ;2
       sta    CTRLPF                  ;3
       sta    NUSIZ1                  ;3
       lsr                            ;2
       ldy    #$07                    ;2
       sta    HMCLR                   ;3
       sta    HMBL                    ;3
       sty    ENABL                   ;3
LF9EC: ;58
       ldx    LFFB8,Y                 ;4
       lda    LFF98,Y                 ;4
       sta    GRP0                    ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3 3
       lda    LFF8D,Y                 ;4 7
       sta    COLUPF                  ;3 10
       lda    LFFA0,Y                 ;4 14
       sta    GRP1                    ;3 17
       lda    LFFA8,Y                 ;4 21
       sta    GRP0                    ;3 24
       lda    ($BD),Y                 ;5 29
       lda    LFFB0,Y                 ;4 34
       sta    GRP1                    ;3 37
       stx    GRP0                    ;3 40
       sta    GRP1                    ;3 43
       lda    #$00                    ;2 45
       sta    COLUPF                  ;3 48
       dey                            ;2 50
       dec    $85                     ;5 55
       bpl    LF9EC                   ;2 57
       jmp    LF011                   ;3

LFA2C:
       lda    LFED2,Y                 ;4
       sta.wy $90,Y                   ;5
       dey                            ;2
       bpl    LFA2C                   ;2
       sta    $A0                     ;3
       lda    #$50                    ;2
       sta    $9D                     ;3
       ldx    #$03                    ;2
LFA3D:
       ldy    $F1,X                   ;4
       lda    LF011,Y                 ;4
       and    #$07                    ;2
       tay                            ;2
       lda    LFCEE,Y                 ;4
       sta    $F5,X                   ;4
       dex                            ;2
       bpl    LFA3D                   ;2
LFA4D:
       lda    $93                     ;3
       and    #$03                    ;2
       sta    $9E                     ;3
       sta    $B8                     ;3
       sta    $BA                     ;3
       clc                            ;2
       adc    #$02                    ;2
       sta    $B9                     ;3
       ldx    #$03                    ;2
LFA5E:
       lda    $93                     ;3
       beq    LFA8D                   ;2
       cmp    $F5,X                   ;4
       bne    LFA6D                   ;2
       txa                            ;2
       and    #$01                    ;2
       adc    #$0E                    ;2
       bpl    LFA8D                   ;2
LFA6D:
       txa                            ;2
       clc                            ;2
       adc    $9E                     ;3
       adc    #$07                    ;2
       cmp    #$0B                    ;2
       bcc    LFA79                   ;2
       sbc    #$04                    ;2
LFA79:
       ldy    $9E                     ;3
       bne    LFA83                   ;2
       cmp    #$09                    ;2
       bcs    LFA83                   ;2
       adc    #$02                    ;2
LFA83:
       cpx    #$03                    ;2
       bne    LFA8D                   ;2
       ror                            ;2
       bcs    LFA8C                   ;2
       sbc    #$00                    ;2
LFA8C:
       rol                            ;2
LFA8D:
       sta    $AC,X                   ;4
       ldy    #$00                    ;2
       sty    $BB                     ;3
       sty    $F1                     ;3
       sty    $B0,X                   ;4
       sty    $81,X                   ;4
       ldy    #$1A                    ;2
       sty    $A8,X                   ;4
       tay                            ;2
       beq    LFAF3                   ;2
       cmp    #$0F                    ;2
       bcs    LFAEF                   ;2
       lda    LFFE9,Y                 ;4
       sta    $BD                     ;3
       lda    $97                     ;3
       lsr                            ;2
       lsr                            ;2
       sta    $85                     ;3
       lda    $97                     ;3
       and    #$03                    ;2
       cmp    LFF8D,Y                 ;4
       lda    $85                     ;3
       beq    LFAC2                   ;2
       sbc    #$00                    ;2
       cmp    #$04                    ;2
       bcc    LFAC2                   ;2
       lda    #$03                    ;2
LFAC2:
       tay                            ;2
       lda    ($BD),Y                 ;5
       ldy    #$04                    ;2
       cpy    $93                     ;3
       bne    LFACD                   ;2
       lda    #$00                    ;2
LFACD:
       ora    $80                     ;3
       sta    $B0,X                   ;4
       ldy    $AC,X                   ;4
       cpy    #$07                    ;2
       beq    LFAE9                   ;2
       ldy    #$06                    ;2
       and    #$08                    ;2
       bne    LFAF1                   ;2
       ldy    #$9A                    ;2
       lda    #$04                    ;2
       and    $B0,X                   ;4
       beq    LFAF1                   ;2
       ldy    #$5A                    ;2
       bne    LFAF1                   ;2 always branch
LFAE9:
       ldy    #$2D                    ;2
       and    #$07                    ;2
       bne    LFAF1                   ;2
LFAEF:
       ldy    #$4D                    ;2
LFAF1:
       sty    $A4,X                   ;4
LFAF3:
       dex                            ;2
       bmi    LFAF9                   ;2
       jmp    LFA5E                   ;3
LFAF9:
       lda    $8E                     ;3
       and    #$F3                    ;2
       sta    $8E                     ;3
       rts                            ;6


       .byte $00 ; |        | $FFBF

LFFB8:
       .byte $E9 ; |XXX X  X| $FFB8
       .byte $AB ; |X X X XX| $FFB9
       .byte $AF ; |X X XXXX| $FFBA
       .byte $AD ; |X X XX X| $FFBB
       .byte $E9 ; |XXX X  X| $FFBC
       .byte $00 ; |        | $FFBD
       .byte $00 ; |        | $FFBE
       .byte $00 ; |        | $FFBF

       ORG $FB00

       .byte $00 ; |        | $FB00
       .byte $3C ; |  XXXX  | $FB01
       .byte $7E ; | XXXXXX | $FB02
       .byte $7E ; | XXXXXX | $FB03
       .byte $7E ; | XXXXXX | $FB04
       .byte $7E ; | XXXXXX | $FB05
       .byte $3C ; |  XXXX  | $FB06
       .byte $80 ; |X       | $FB07
       .byte $80 ; |X       | $FB08
       .byte $C6 ; |XX   XX | $FB09
       .byte $64 ; | XX  X  | $FB0A
       .byte $6C ; | XX XX  | $FB0B
       .byte $7C ; | XXXXX  | $FB0C
       .byte $38 ; |  XXX   | $FB0D
       .byte $30 ; |  XX    | $FB0E
       .byte $7C ; | XXXXX  | $FB0F
       .byte $7E ; | XXXXXX | $FB10
       .byte $72 ; | XXX  X | $FB11
       .byte $70 ; | XXX    | $FB12
       .byte $30 ; |  XX    | $FB13
       .byte $38 ; |  XXX   | $FB14
       .byte $38 ; |  XXX   | $FB15
       .byte $3C ; |  XXXX  | $FB16
       .byte $38 ; |  XXX   | $FB17
       .byte $38 ; |  XXX   | $FB18
       .byte $38 ; |  XXX   | $FB19
       .byte $38 ; |  XXX   | $FB1A
       .byte $00 ; |        | $FB1B
       .byte $00 ; |        | $FB1C
       .byte $03 ; |      XX| $FB1D
       .byte $62 ; | XX   X | $FB1E
       .byte $F2 ; |XXXX  X | $FB1F
       .byte $BE ; |X XXXXX | $FB20
       .byte $3C ; |  XXXX  | $FB21
       .byte $B8 ; |X XXX   | $FB22
       .byte $B8 ; |X XXX   | $FB23
       .byte $FC ; |XXXXXX  | $FB24
       .byte $7E ; | XXXXXX | $FB25
       .byte $32 ; |  XX  X | $FB26
       .byte $32 ; |  XX  X | $FB27
       .byte $38 ; |  XXX   | $FB28
       .byte $38 ; |  XXX   | $FB29
       .byte $3C ; |  XXXX  | $FB2A
       .byte $38 ; |  XXX   | $FB2B
       .byte $38 ; |  XXX   | $FB2C
       .byte $38 ; |  XXX   | $FB2D
       .byte $38 ; |  XXX   | $FB2E
       .byte $04 ; |     X  | $FB2F
       .byte $86 ; |X    XX | $FB30
       .byte $88 ; |X   X   | $FB31
       .byte $E8 ; |XXX X   | $FB32
       .byte $28 ; |  X X   | $FB33
       .byte $38 ; |  XXX   | $FB34
       .byte $38 ; |  XXX   | $FB35
       .byte $30 ; |  XX    | $FB36
       .byte $7C ; | XXXXX  | $FB37
       .byte $7E ; | XXXXXX | $FB38
       .byte $72 ; | XXX  X | $FB39
       .byte $70 ; | XXX    | $FB3A
       .byte $30 ; |  XX    | $FB3B
       .byte $38 ; |  XXX   | $FB3C
       .byte $38 ; |  XXX   | $FB3D
       .byte $3C ; |  XXXX  | $FB3E
       .byte $38 ; |  XXX   | $FB3F
       .byte $38 ; |  XXX   | $FB40
       .byte $38 ; |  XXX   | $FB41
       .byte $38 ; |  XXX   | $FB42
       .byte $18 ; |   XX   | $FB43
       .byte $50 ; | X X    | $FB44
       .byte $50 ; | X X    | $FB45
       .byte $7E ; | XXXXXX | $FB46
       .byte $16 ; |   X XX | $FB47
       .byte $3C ; |  XXXX  | $FB48
       .byte $38 ; |  XXX   | $FB49
       .byte $3C ; |  XXXX  | $FB4A
       .byte $3C ; |  XXXX  | $FB4B
       .byte $38 ; |  XXX   | $FB4C
       .byte $38 ; |  XXX   | $FB4D
       .byte $30 ; |  XX    | $FB4E
       .byte $30 ; |  XX    | $FB4F
       .byte $38 ; |  XXX   | $FB50
       .byte $38 ; |  XXX   | $FB51
       .byte $3C ; |  XXXX  | $FB52
       .byte $38 ; |  XXX   | $FB53
       .byte $38 ; |  XXX   | $FB54
       .byte $38 ; |  XXX   | $FB55
       .byte $38 ; |  XXX   | $FB56
       .byte $20 ; |  X     | $FB57
       .byte $42 ; | X    X | $FB58
       .byte $44 ; | X   X  | $FB59
       .byte $62 ; | XX   X | $FB5A
       .byte $66 ; | XX  XX | $FB5B
       .byte $2E ; |  X XXX | $FB5C
       .byte $3C ; |  XXXX  | $FB5D
       .byte $38 ; |  XXX   | $FB5E
       .byte $3C ; |  XXXX  | $FB5F
       .byte $3C ; |  XXXX  | $FB60
       .byte $38 ; |  XXX   | $FB61
       .byte $30 ; |  XX    | $FB62
       .byte $30 ; |  XX    | $FB63
       .byte $38 ; |  XXX   | $FB64
       .byte $38 ; |  XXX   | $FB65
       .byte $3C ; |  XXXX  | $FB66
       .byte $38 ; |  XXX   | $FB67
       .byte $38 ; |  XXX   | $FB68
       .byte $38 ; |  XXX   | $FB69
       .byte $38 ; |  XXX   | $FB6A
       .byte $E7 ; |XXX  XXX| $FB6B
       .byte $BD ; |X XXXX X| $FB6C
       .byte $E7 ; |XXX  XXX| $FB6D
       .byte $BD ; |X XXXX X| $FB6E
       .byte $FF ; |XXXXXXXX| $FB6F
       .byte $A5 ; |X X  X X| $FB70
       .byte $DB ; |XX XX XX| $FB71
       .byte $5A ; | X XX X | $FB72
       .byte $66 ; | XX  XX | $FB73
       .byte $3C ; |  XXXX  | $FB74
       .byte $42 ; | X    X | $FB75
       .byte $18 ; |   XX   | $FB76
       .byte $00 ; |        | $FB77
       .byte $00 ; |        | $FB78
       .byte $E7 ; |XXX  XXX| $FB79
       .byte $BD ; |X XXXX X| $FB7A
       .byte $E7 ; |XXX  XXX| $FB7B
       .byte $BD ; |X XXXX X| $FB7C
       .byte $FF ; |XXXXXXXX| $FB7D
       .byte $A5 ; |X X  X X| $FB7E
       .byte $DB ; |XX XX XX| $FB7F
       .byte $5A ; | X XX X | $FB80
       .byte $66 ; | XX  XX | $FB81
       .byte $3C ; |  XXXX  | $FB82
       .byte $00 ; |        | $FB83
       .byte $00 ; |        | $FB84
       .byte $81 ; |X      X| $FB85
       .byte $24 ; |  X  X  | $FB86
       .byte $66 ; | XX  XX | $FB87
       .byte $7F ; | XXXXXXX| $FB88
       .byte $09 ; |    X  X| $FB89
       .byte $FE ; |XXXXXXX | $FB8A
       .byte $AA ; |X X X X | $FB8B
       .byte $FE ; |XXXXXXX | $FB8C
       .byte $AA ; |X X X X | $FB8D
       .byte $FE ; |XXXXXXX | $FB8E
       .byte $AA ; |X X X X | $FB8F
       .byte $FF ; |XXXXXXXX| $FB90
       .byte $FF ; |XXXXXXXX| $FB91
       .byte $FF ; |XXXXXXXX| $FB92
       .byte $FF ; |XXXXXXXX| $FB93
       .byte $FF ; |XXXXXXXX| $FB94
       .byte $FF ; |XXXXXXXX| $FB95
       .byte $FF ; |XXXXXXXX| $FB96
       .byte $FF ; |XXXXXXXX| $FB97
       .byte $24 ; |  X  X  | $FB98
       .byte $3C ; |  XXXX  | $FB99
       .byte $3E ; |  XXXXX | $FB9A
       .byte $7F ; | XXXXXXX| $FB9B
       .byte $7F ; | XXXXXXX| $FB9C
       .byte $7F ; | XXXXXXX| $FB9D
       .byte $7F ; | XXXXXXX| $FB9E
       .byte $7F ; | XXXXXXX| $FB9F
       .byte $3E ; |  XXXXX | $FBA0
       .byte $1C ; |   XXX  | $FBA1
       .byte $08 ; |    X   | $FBA2
       .byte $1C ; |   XXX  | $FBA3
       .byte $36 ; |  XX XX | $FBA4
       .byte $07 ; |     XXX| $FBA5
       .byte $0F ; |    XXXX| $FBA6
       .byte $13 ; |   X  XX| $FBA7
       .byte $23 ; |  X   XX| $FBA8
       .byte $43 ; | X    XX| $FBA9
       .byte $47 ; | X   XXX| $FBAA
       .byte $7E ; | XXXXXX | $FBAB
       .byte $7C ; | XXXXX  | $FBAC
       .byte $00 ; |        | $FBAD
       .byte $60 ; | XX     | $FBAE
       .byte $20 ; |  X     | $FBAF
       .byte $78 ; | XXXX   | $FBB0
       .byte $76 ; | XXX XX | $FBB1
       .byte $6F ; | XX XXXX| $FBB2
       .byte $DF ; |XX XXXXX| $FBB3
       .byte $B3 ; |X XX  XX| $FBB4
       .byte $83 ; |X     XX| $FBB5
       .byte $78 ; | XXXX   | $FBB6
       .byte $00 ; |        | $FBB7
       .byte $60 ; | XX     | $FBB8
       .byte $20 ; |  X     | $FBB9
       .byte $78 ; | XXXX   | $FBBA
       .byte $F6 ; |XXXX XX | $FBBB
       .byte $EF ; |XXX XXXX| $FBBC
       .byte $DF ; |XX XXXXX| $FBBD
       .byte $33 ; |  XX  XX| $FBBE
       .byte $03 ; |      XX| $FBBF
       .byte $78 ; | XXXX   | $FBC0
LFBC1:
       .byte $FF ; |XXXXXXXX| $FBC1
       .byte $4C ; | X  XX  | $FBC2
       .byte $6C ; | XX XX  | $FBC3
       .byte $FF ; |XXXXXXXX| $FBC4
       .byte $FF ; |XXXXXXXX| $FBC5
       .byte $1A ; |   XX X | $FBC6
       .byte $15 ; |   X X X| $FBC7
       .byte $FF ; |XXXXXXXX| $FBC8
       .byte $FF ; |XXXXXXXX| $FBC9
       .byte $0C ; |    XX  | $FBCA
       .byte $0D ; |    XX X| $FBCB
       .byte $FF ; |XXXXXXXX| $FBCC
       .byte $FF ; |XXXXXXXX| $FBCD
       .byte $05 ; |     X X| $FBCE
       .byte $05 ; |     X X| $FBCF
       .byte $FF ; |XXXXXXXX| $FBD0
       .byte $FF ; |XXXXXXXX| $FBD1
       .byte $00 ; |        | $FBD2
       .byte $02 ; |      X | $FBD3
       .byte $FF ; |XXXXXXXX| $FBD4
       .byte $03 ; |      XX| $FBD5
       .byte $00 ; |        | $FBD6
       .byte $08 ; |    X   | $FBD7
       .byte $00 ; |        | $FBD8
       .byte $40 ; | X      | $FBD9
       .byte $44 ; | X   X  | $FBDA
       .byte $24 ; |  X  X  | $FBDB
       .byte $44 ; | X   X  | $FBDC
       .byte $22 ; |  X   X | $FBDD
       .byte $1A ; |   XX X | $FBDE
       .byte $15 ; |   X X X| $FBDF
       .byte $1A ; |   XX X | $FBE0
       .byte $15 ; |   X X X| $FBE1
       .byte $0C ; |    XX  | $FBE2
       .byte $0D ; |    XX X| $FBE3
       .byte $0C ; |    XX  | $FBE4
       .byte $0D ; |    XX X| $FBE5
       .byte $0D ; |    XX X| $FBE6
       .byte $0D ; |    XX X| $FBE7
       .byte $0D ; |    XX X| $FBE8
       .byte $0D ; |    XX X| $FBE9
       .byte $01 ; |       X| $FBEA
       .byte $03 ; |      XX| $FBEB
       .byte $01 ; |       X| $FBEC
       .byte $03 ; |      XX| $FBED
       .byte $00 ; |        | $FBEE
       .byte $08 ; |    X   | $FBEF
       .byte $00 ; |        | $FBF0
       .byte $08 ; |    X   | $FBF1
       .byte $C0 ; |XX      | $FBF2
       .byte $4D ; | X  XX X| $FBF3
       .byte $2D ; |  X XX X| $FBF4
       .byte $2D ; |  X XX X| $FBF5
LFBF6:
       .byte $81 ; |X      X| $FBF6
       .byte $7D ; | XXXXX X| $FBF7
       .byte $79 ; | XXXX  X| $FBF8
       .byte $7D ; | XXXXX X| $FBF9
LFBFA:
       .byte $61 ; | XX    X| $FBFA
       .byte $38 ; |  XXX   | $FBFB
       .byte $61 ; | XX    X| $FBFC
LFBFD:
       .byte $A8 ; |X X X   | $FBFD
       .byte $80 ; |X       | $FBFE
       .byte $A8 ; |X X X   | $FBFF

       ORG $FC00

       .byte $01 ; |       X| $FC00
       .byte $03 ; |      XX| $FC01
       .byte $03 ; |      XX| $FC02
       .byte $03 ; |      XX| $FC03
       .byte $03 ; |      XX| $FC04
       .byte $0D ; |    XX X| $FC05
       .byte $03 ; |      XX| $FC06
       .byte $0D ; |    XX X| $FC07
       .byte $03 ; |      XX| $FC08
       .byte $0D ; |    XX X| $FC09
       .byte $03 ; |      XX| $FC0A
       .byte $0D ; |    XX X| $FC0B
       .byte $03 ; |      XX| $FC0C
       .byte $2B ; |  X X XX| $FC0D
       .byte $2B ; |  X X XX| $FC0E
       .byte $2B ; |  X X XX| $FC0F
       .byte $2B ; |  X X XX| $FC10
       .byte $2B ; |  X X XX| $FC11
       .byte $0D ; |    XX X| $FC12
       .byte $03 ; |      XX| $FC13
       .byte $0D ; |    XX X| $FC14

  IF PAL
       .byte $24 ; |   X X  | $FC15
       .byte $26 ; |   X XX | $FC16
       .byte $28 ; |   XX   | $FC17
       .byte $2A ; |   XX X | $FC18
       .byte $2C ; |   XXX  | $FC19
       .byte $2E ; |   XXXX | $FC1A
       .byte $2C ; |   XXX  | $FC1B
       .byte $28 ; |   XX   | $FC1C
       .byte $20 ; |   X    | $FC1D
       .byte $26 ; |   X XX | $FC1E
       .byte $28 ; |   XX   | $FC1F
       .byte $28 ; |   XX   | $FC20
       .byte $28 ; |   XX   | $FC21
       .byte $28 ; |   XX   | $FC22
       .byte $26 ; |   X XX | $FC23
       .byte $24 ; |   X X  | $FC24
       .byte $00 ; |        | $FC25
       .byte $00 ; |        | $FC26
       .byte $00 ; |        | $FC27

       .byte $2C ; |   XXX  | $FC28
       .byte $2C ; |   XXX  | $FC29
       .byte $2C ; |   XXX  | $FC2A
       .byte $2C ; |   XXX  | $FC2B
       .byte $2C ; |   XXX  | $FC2C
       .byte $00 ; |        | $FC2D
       .byte $62 ; | X    X | $FC2E
       .byte $64 ; | X   X  | $FC2F
       .byte $66 ; | X   XX | $FC30
       .byte $66 ; | X   XX | $FC31
       .byte $64 ; | X   X  | $FC32
       .byte $62 ; | X    X | $FC33
       .byte $D3 ; |X     XX| $FC34
       .byte $D3 ; |X     XX| $FC35
       .byte $D3 ; |X     XX| $FC36
       .byte $D3 ; |X     XX| $FC37
       .byte $D3 ; |X     XX| $FC38
       .byte $D3 ; |X     XX| $FC39
       .byte $D3 ; |X     XX| $FC3A
       .byte $D3 ; |X     XX| $FC3B
       .byte $D3 ; |X     XX| $FC3C
       .byte $D3 ; |X     XX| $FC3D

       .byte $2B ; |  X X XX| $FC3E
       .byte $2B ; |  X X XX| $FC3F
       .byte $2B ; |  X X XX| $FC40
       .byte $2B ; |  X X XX| $FC41
       .byte $2B ; |  X X XX| $FC42
       .byte $01 ; |       X| $FC43
       .byte $01 ; |       X| $FC44
       .byte $01 ; |       X| $FC45
       .byte $01 ; |       X| $FC46
       .byte $01 ; |       X| $FC47
       .byte $00 ; |        | $FC48
       .byte $00 ; |        | $FC49
       .byte $00 ; |        | $FC4A
       .byte $0C ; |    XX  | $FC4B
       .byte $0C ; |    XX  | $FC4C
       .byte $0C ; |    XX  | $FC4D
       .byte $0C ; |    XX  | $FC4E
       .byte $0C ; |    XX  | $FC4F
       .byte $0C ; |    XX  | $FC50
       .byte $0C ; |    XX  | $FC51
       .byte $0C ; |    XX  | $FC52
       .byte $0C ; |    XX  | $FC53
       .byte $0C ; |    XX  | $FC54
       .byte $0C ; |    XX  | $FC55
       .byte $0C ; |    XX  | $FC56
       .byte $0C ; |    XX  | $FC57
       .byte $0C ; |    XX  | $FC58
       .byte $0C ; |    XX  | $FC59
       .byte $0C ; |    XX  | $FC5A
       .byte $0C ; |    XX  | $FC5B
       .byte $0C ; |    XX  | $FC5C
       .byte $0C ; |    XX  | $FC5D
       .byte $0C ; |    XX  | $FC5E
       .byte $0C ; |    XX  | $FC5F
       .byte $0C ; |    XX  | $FC60
       .byte $0C ; |    XX  | $FC61
       .byte $0C ; |    XX  | $FC62
       .byte $0C ; |    XX  | $FC63
       .byte $0C ; |    XX  | $FC64
       .byte $FF ; |XXXXXXXX| $FC65
       .byte $FF ; |XXXXXXXX| $FC66
       .byte $FF ; |XXXXXXXX| $FC67

       .byte $28 ; |   XX   | $FC68
       .byte $50 ; |XX      | $FC69
       .byte $50 ; |XX      | $FC6A
       .byte $50 ; |XX      | $FC6B
       .byte $50 ; |XX      | $FC6C
       .byte $50 ; |XX      | $FC6D
       .byte $50 ; |XX      | $FC6E
       .byte $50 ; |XX      | $FC6F
       .byte $50 ; |XX      | $FC70
       .byte $50 ; |XX      | $FC71
       .byte $50 ; |XX      | $FC72
       .byte $50 ; |XX      | $FC73
       .byte $50 ; |XX      | $FC74
       .byte $50 ; |XX      | $FC75
       .byte $50 ; |XX      | $FC76
       .byte $50 ; |XX      | $FC77
       .byte $50 ; |XX      | $FC78
       .byte $50 ; |XX      | $FC79
       .byte $50 ; |XX      | $FC7A
       .byte $50 ; |XX      | $FC7B
       .byte $50 ; |XX      | $FC7C
       .byte $50 ; |XX      | $FC7D
       .byte $50 ; |XX      | $FC7E
       .byte $50 ; |XX      | $FC7F
       .byte $50 ; |XX      | $FC80
       .byte $50 ; |XX      | $FC81
       .byte $50 ; |XX      | $FC82
       .byte $50 ; |XX      | $FC83

       .byte $FF ; |XXXXXXXX| $FC84
       .byte $FF ; |XXXXXXXX| $FC85
       .byte $FC ; |XXXXXX  | $FC86
       .byte $0C ; |    XX  | $FC87
       .byte $0C ; |    XX  | $FC88
       .byte $0C ; |    XX  | $FC89
       .byte $FC ; |XXXXXX  | $FC8A
       .byte $0C ; |    XX  | $FC8B
       .byte $0C ; |    XX  | $FC8C
       .byte $0C ; |    XX  | $FC8D
       .byte $FC ; |XXXXXX  | $FC8E
       .byte $0C ; |    XX  | $FC8F
       .byte $0C ; |    XX  | $FC90
       .byte $0C ; |    XX  | $FC91
       .byte $FC ; |XXXXXX  | $FC92
       .byte $0C ; |    XX  | $FC93
       .byte $0C ; |    XX  | $FC94
       .byte $0C ; |    XX  | $FC95
       .byte $FC ; |XXXXXX  | $FC96
       .byte $0C ; |    XX  | $FC97
       .byte $0C ; |    XX  | $FC98
       .byte $0C ; |    XX  | $FC99
       .byte $FC ; |XXXXXX  | $FC9A
       .byte $0C ; |    XX  | $FC9B
       .byte $0C ; |    XX  | $FC9C
       .byte $0C ; |    XX  | $FC9D
       .byte $FC ; |XXXXXX  | $FC9E
       .byte $0C ; |    XX  | $FC9F
       .byte $0C ; |    XX  | $FCA0
       .byte $0C ; |    XX  | $FCA1
       .byte $FC ; |XXXXXX  | $FCA2
       .byte $0C ; |    XX  | $FCA3
       .byte $0C ; |    XX  | $FCA4
       .byte $0C ; |    XX  | $FCA5
       .byte $FC ; |XXXXXX  | $FCA6

       .byte $28 ; |   XX   | $FCA7
       .byte $28 ; |   XX   | $FCA8
       .byte $76 ; |X XX XX | $FCA9
       .byte $76 ; |X XX XX | $FCAA
       .byte $76 ; |X XX XX | $FCAB
       .byte $76 ; |X XX XX | $FCAC
       .byte $76 ; |X XX XX | $FCAD
       .byte $76 ; |X XX XX | $FCAE
       .byte $76 ; |X XX XX | $FCAF
       .byte $76 ; |X XX XX | $FCB0
       .byte $76 ; |X XX XX | $FCB1
       .byte $76 ; |X XX XX | $FCB2
       .byte $76 ; |X XX XX | $FCB3
       .byte $76 ; |X XX XX | $FCB4
       .byte $76 ; |X XX XX | $FCB5
       .byte $76 ; |X XX XX | $FCB6
       .byte $76 ; |X XX XX | $FCB7
       .byte $76 ; |X XX XX | $FCB8
       .byte $76 ; |X XX XX | $FCB9
       .byte $76 ; |X XX XX | $FCBA
       .byte $76 ; |X XX XX | $FCBB
       .byte $76 ; |X XX XX | $FCBC
       .byte $76 ; |X XX XX | $FCBD
       .byte $76 ; |X XX XX | $FCBE
       .byte $7E ; |X XXXXX | $FCBF
       .byte $7E ; |X XXXXX | $FCC0
       .byte $B6 ; |X  X XX | $FCC1
       .byte $B6 ; |X  X XX | $FCC2
       .byte $B6 ; |X  X XX | $FCC3
       .byte $B6 ; |X  X XX | $FCC4
       .byte $B6 ; |X  X XX | $FCC5
       .byte $B6 ; |X  X XX | $FCC6
       .byte $B6 ; |X  X XX | $FCC7
       .byte $B6 ; |X  X XX | $FCC8
       .byte $BA ; |X  XX X | $FCC9
       .byte $BA ; |X  XX X | $FCCA
       .byte $54 ; |XX   X  | $FCCB
       .byte $54 ; |XX   X  | $FCCC
       .byte $54 ; |XX   X  | $FCCD
       .byte $54 ; |XX   X  | $FCCE
       .byte $54 ; |XX   X  | $FCCF
       .byte $54 ; |XX   X  | $FCD0
       .byte $54 ; |XX   X  | $FCD1
       .byte $54 ; |XX   X  | $FCD2
       .byte $54 ; |XX   X  | $FCD3
       .byte $54 ; |XX   X  | $FCD4
       .byte $54 ; |XX   X  | $FCD5
       .byte $54 ; |XX   X  | $FCD6
       .byte $54 ; |XX   X  | $FCD7
       .byte $54 ; |XX   X  | $FCD8
       .byte $54 ; |XX   X  | $FCD9
       .byte $54 ; |XX   X  | $FCDA
  ELSE
       .byte $14 ; |   X X  | $FC15
       .byte $16 ; |   X XX | $FC16
       .byte $18 ; |   XX   | $FC17
       .byte $1A ; |   XX X | $FC18
       .byte $1C ; |   XXX  | $FC19
       .byte $1E ; |   XXXX | $FC1A
       .byte $1C ; |   XXX  | $FC1B
       .byte $18 ; |   XX   | $FC1C
       .byte $10 ; |   X    | $FC1D
       .byte $16 ; |   X XX | $FC1E
       .byte $18 ; |   XX   | $FC1F
       .byte $18 ; |   XX   | $FC20
       .byte $18 ; |   XX   | $FC21
       .byte $18 ; |   XX   | $FC22
       .byte $16 ; |   X XX | $FC23
       .byte $14 ; |   X X  | $FC24
       .byte $00 ; |        | $FC25
       .byte $00 ; |        | $FC26
       .byte $00 ; |        | $FC27
       .byte $1C ; |   XXX  | $FC28
       .byte $1C ; |   XXX  | $FC29
       .byte $1C ; |   XXX  | $FC2A
       .byte $1C ; |   XXX  | $FC2B
       .byte $1C ; |   XXX  | $FC2C
       .byte $00 ; |        | $FC2D
       .byte $42 ; | X    X | $FC2E
       .byte $44 ; | X   X  | $FC2F
       .byte $46 ; | X   XX | $FC30
       .byte $46 ; | X   XX | $FC31
       .byte $44 ; | X   X  | $FC32
       .byte $42 ; | X    X | $FC33
       .byte $83 ; |X     XX| $FC34
       .byte $83 ; |X     XX| $FC35
       .byte $83 ; |X     XX| $FC36
       .byte $83 ; |X     XX| $FC37
       .byte $83 ; |X     XX| $FC38
       .byte $83 ; |X     XX| $FC39
       .byte $83 ; |X     XX| $FC3A
       .byte $83 ; |X     XX| $FC3B
       .byte $83 ; |X     XX| $FC3C
       .byte $83 ; |X     XX| $FC3D
       .byte $2B ; |  X X XX| $FC3E
       .byte $2B ; |  X X XX| $FC3F
       .byte $2B ; |  X X XX| $FC40
       .byte $2B ; |  X X XX| $FC41
       .byte $2B ; |  X X XX| $FC42
       .byte $01 ; |       X| $FC43
       .byte $01 ; |       X| $FC44
       .byte $01 ; |       X| $FC45
       .byte $01 ; |       X| $FC46
       .byte $01 ; |       X| $FC47
       .byte $00 ; |        | $FC48
       .byte $00 ; |        | $FC49
       .byte $00 ; |        | $FC4A
       .byte $0C ; |    XX  | $FC4B
       .byte $0C ; |    XX  | $FC4C
       .byte $0C ; |    XX  | $FC4D
       .byte $0C ; |    XX  | $FC4E
       .byte $0C ; |    XX  | $FC4F
       .byte $0C ; |    XX  | $FC50
       .byte $0C ; |    XX  | $FC51
       .byte $0C ; |    XX  | $FC52
       .byte $0C ; |    XX  | $FC53
       .byte $0C ; |    XX  | $FC54
       .byte $0C ; |    XX  | $FC55
       .byte $0C ; |    XX  | $FC56
       .byte $0C ; |    XX  | $FC57
       .byte $0C ; |    XX  | $FC58
       .byte $0C ; |    XX  | $FC59
       .byte $0C ; |    XX  | $FC5A
       .byte $0C ; |    XX  | $FC5B
       .byte $0C ; |    XX  | $FC5C
       .byte $0C ; |    XX  | $FC5D
       .byte $0C ; |    XX  | $FC5E
       .byte $0C ; |    XX  | $FC5F
       .byte $0C ; |    XX  | $FC60
       .byte $0C ; |    XX  | $FC61
       .byte $0C ; |    XX  | $FC62
       .byte $0C ; |    XX  | $FC63
       .byte $0C ; |    XX  | $FC64
       .byte $FF ; |XXXXXXXX| $FC65
       .byte $FF ; |XXXXXXXX| $FC66
       .byte $FF ; |XXXXXXXX| $FC67
       .byte $18 ; |   XX   | $FC68
       .byte $C0 ; |XX      | $FC69
       .byte $C0 ; |XX      | $FC6A
       .byte $C0 ; |XX      | $FC6B
       .byte $C0 ; |XX      | $FC6C
       .byte $C0 ; |XX      | $FC6D
       .byte $C0 ; |XX      | $FC6E
       .byte $C0 ; |XX      | $FC6F
       .byte $C0 ; |XX      | $FC70
       .byte $C0 ; |XX      | $FC71
       .byte $C0 ; |XX      | $FC72
       .byte $C0 ; |XX      | $FC73
       .byte $C0 ; |XX      | $FC74
       .byte $C0 ; |XX      | $FC75
       .byte $C0 ; |XX      | $FC76
       .byte $C0 ; |XX      | $FC77
       .byte $C0 ; |XX      | $FC78
       .byte $C0 ; |XX      | $FC79
       .byte $C0 ; |XX      | $FC7A
       .byte $C0 ; |XX      | $FC7B
       .byte $C0 ; |XX      | $FC7C
       .byte $C0 ; |XX      | $FC7D
       .byte $C0 ; |XX      | $FC7E
       .byte $C0 ; |XX      | $FC7F
       .byte $C0 ; |XX      | $FC80
       .byte $C0 ; |XX      | $FC81
       .byte $C0 ; |XX      | $FC82
       .byte $C0 ; |XX      | $FC83
       .byte $FF ; |XXXXXXXX| $FC84
       .byte $FF ; |XXXXXXXX| $FC85
       .byte $FC ; |XXXXXX  | $FC86
       .byte $0C ; |    XX  | $FC87
       .byte $0C ; |    XX  | $FC88
       .byte $0C ; |    XX  | $FC89
       .byte $FC ; |XXXXXX  | $FC8A
       .byte $0C ; |    XX  | $FC8B
       .byte $0C ; |    XX  | $FC8C
       .byte $0C ; |    XX  | $FC8D
       .byte $FC ; |XXXXXX  | $FC8E
       .byte $0C ; |    XX  | $FC8F
       .byte $0C ; |    XX  | $FC90
       .byte $0C ; |    XX  | $FC91
       .byte $FC ; |XXXXXX  | $FC92
       .byte $0C ; |    XX  | $FC93
       .byte $0C ; |    XX  | $FC94
       .byte $0C ; |    XX  | $FC95
       .byte $FC ; |XXXXXX  | $FC96
       .byte $0C ; |    XX  | $FC97
       .byte $0C ; |    XX  | $FC98
       .byte $0C ; |    XX  | $FC99
       .byte $FC ; |XXXXXX  | $FC9A
       .byte $0C ; |    XX  | $FC9B
       .byte $0C ; |    XX  | $FC9C
       .byte $0C ; |    XX  | $FC9D
       .byte $FC ; |XXXXXX  | $FC9E
       .byte $0C ; |    XX  | $FC9F
       .byte $0C ; |    XX  | $FCA0
       .byte $0C ; |    XX  | $FCA1
       .byte $FC ; |XXXXXX  | $FCA2
       .byte $0C ; |    XX  | $FCA3
       .byte $0C ; |    XX  | $FCA4
       .byte $0C ; |    XX  | $FCA5
       .byte $FC ; |XXXXXX  | $FCA6
       .byte $18 ; |   XX   | $FCA7
       .byte $18 ; |   XX   | $FCA8
       .byte $B6 ; |X XX XX | $FCA9
       .byte $B6 ; |X XX XX | $FCAA
       .byte $B6 ; |X XX XX | $FCAB
       .byte $B6 ; |X XX XX | $FCAC
       .byte $B6 ; |X XX XX | $FCAD
       .byte $B6 ; |X XX XX | $FCAE
       .byte $B6 ; |X XX XX | $FCAF
       .byte $B6 ; |X XX XX | $FCB0
       .byte $B6 ; |X XX XX | $FCB1
       .byte $B6 ; |X XX XX | $FCB2
       .byte $B6 ; |X XX XX | $FCB3
       .byte $B6 ; |X XX XX | $FCB4
       .byte $B6 ; |X XX XX | $FCB5
       .byte $B6 ; |X XX XX | $FCB6
       .byte $B6 ; |X XX XX | $FCB7
       .byte $B6 ; |X XX XX | $FCB8
       .byte $B6 ; |X XX XX | $FCB9
       .byte $B6 ; |X XX XX | $FCBA
       .byte $B6 ; |X XX XX | $FCBB
       .byte $B6 ; |X XX XX | $FCBC
       .byte $B6 ; |X XX XX | $FCBD
       .byte $B6 ; |X XX XX | $FCBE
       .byte $BE ; |X XXXXX | $FCBF
       .byte $BE ; |X XXXXX | $FCC0
       .byte $96 ; |X  X XX | $FCC1
       .byte $96 ; |X  X XX | $FCC2
       .byte $96 ; |X  X XX | $FCC3
       .byte $96 ; |X  X XX | $FCC4
       .byte $96 ; |X  X XX | $FCC5
       .byte $96 ; |X  X XX | $FCC6
       .byte $96 ; |X  X XX | $FCC7
       .byte $96 ; |X  X XX | $FCC8
       .byte $9A ; |X  XX X | $FCC9
       .byte $9A ; |X  XX X | $FCCA
       .byte $C4 ; |XX   X  | $FCCB
       .byte $C4 ; |XX   X  | $FCCC
       .byte $C4 ; |XX   X  | $FCCD
       .byte $C4 ; |XX   X  | $FCCE
       .byte $C4 ; |XX   X  | $FCCF
       .byte $C4 ; |XX   X  | $FCD0
       .byte $C4 ; |XX   X  | $FCD1
       .byte $C4 ; |XX   X  | $FCD2
       .byte $C4 ; |XX   X  | $FCD3
       .byte $C4 ; |XX   X  | $FCD4
       .byte $C4 ; |XX   X  | $FCD5
       .byte $C4 ; |XX   X  | $FCD6
       .byte $C4 ; |XX   X  | $FCD7
       .byte $C4 ; |XX   X  | $FCD8
       .byte $C4 ; |XX   X  | $FCD9
       .byte $C4 ; |XX   X  | $FCDA
  ENDIF

LFCDB:
       .byte $12 ; |   X  X | $FCDB
       .byte $F2 ; |XXXX  X | $FCDC
       .byte $10 ; |   X    | $FCDD
       .byte $F0 ; |XXXX    | $FCDE
LFCDF:
       .byte $01 ; |       X| $FCDF
       .byte $01 ; |       X| $FCE0
       .byte $01 ; |       X| $FCE1
       .byte $03 ; |      XX| $FCE2
       .byte $03 ; |      XX| $FCE3
       .byte $07 ; |     XXX| $FCE4
       .byte $07 ; |     XXX| $FCE5
       .byte $0F ; |    XXXX| $FCE6


LFCE7: ;7
       lda    $92                     ;3
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       tax                            ;2
       rts                            ;6


LFCEE:
       .byte $01 ; |       X| $FCEE
       .byte $01 ; |       X| $FCEF
       .byte $02 ; |      X | $FCF0
       .byte $03 ; |      XX| $FCF1
       .byte $03 ; |      XX| $FCF2
       .byte $05 ; |     X X| $FCF3
       .byte $06 ; |     XX | $FCF4
       .byte $06 ; |     XX | $FCF5

LFCF6:
       .byte $BF ; |X XXXXXX| $FCF6
       .byte $7F ; | XXXXXXX| $FCF7
       .byte $BF ; |X XXXXXX| $FCF8
LFCF9:
       .byte $7F ; | XXXXXXX| $FCF9
       .byte $F1 ; |XXXX   X| $FCFA
       .byte $83 ; |X     XX| $FCFB
       .byte $87 ; |X    XXX| $FCFC
       .byte $EE ; |XXX XXX | $FCFD
       .byte $4B ; | X  X XX| $FCFE
       .byte $DF ; |XX XXXXX| $FCFF

       ORG $FD00

;NOTE: horizon colors used for # object copies too!
  IF PAL
LFD00: ;must cross page
       .byte $04 ; |     X  | $FD00
       .byte $28 ; |   XX   | $FD01
       .byte $28 ; |   XX   | $FD02
       .byte $28 ; |  X X   | $FD03
       .byte $28 ; |  X X   | $FD04
       .byte $48 ; |  XXX   | $FD05
       .byte $68 ; | X  X   | $FD06
       .byte $88 ; | X XX   | $FD07
       .byte $A8 ; | XX X   | $FD08
LFD09:
       .byte $A8 ; | XX X   | $FD09
  ELSE
LFD00: ;must cross page
       .byte $04 ; |     X  | $FD00
       .byte $18 ; |   XX   | $FD01
       .byte $18 ; |   XX   | $FD02
       .byte $28 ; |  X X   | $FD03
       .byte $28 ; |  X X   | $FD04
       .byte $38 ; |  XXX   | $FD05
       .byte $48 ; | X  X   | $FD06
       .byte $58 ; | X XX   | $FD07
       .byte $68 ; | XX X   | $FD08
LFD09:
       .byte $68 ; | XX X   | $FD09
  ENDIF
LFD0A:
       .byte $07 ; |     XXX| $FD0A
       .byte $00 ; |        | $FD0B
       .byte $07 ; |     XXX| $FD0C



LFD24:
       lda    #$07                    ;2
       sta    $9F                     ;3
LFD28:
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       lda    #$0F                    ;2
       sta    COLUP0                  ;3
       sta    COLUP1                  ;3
       ldx    #$F3                    ;2
       stx    NUSIZ0                  ;3
       stx    NUSIZ1                  ;3
       ldy    #$01                    ;2
       lda    #$40                    ;2
       NOP                            ;2 waste
       sta    RESP0                   ;3
       sta    RESP1                   ;3
       sta    RESBL                   ;3
       sty    CTRLPF                  ;3
       sta    HMBL                    ;3
       stx    HMP0                    ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       sty    VDELP0                  ;3
       sty    VDELP1                  ;3
       dey                            ;2
       sty    GRP0                    ;3
       sty    GRP1                    ;3
       sty    GRP0                    ;3
       sta    $9E                     ;3
LFD5A:
       sta    HMCLR                   ;3
LFD5C:
       ldy    $9F                     ;3
       lda    ($C7),Y                 ;5
       sta    $9E                     ;3
       lda    ($C5),Y                 ;5
       tax                            ;2
       lda    ($BD),Y                 ;5
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       sta    GRP0                    ;3
       lda    ($BF),Y                 ;5
       sta    GRP1                    ;3
       lda    ($C1),Y                 ;5
       sta    GRP0                    ;3
       lda    ($C3),Y                 ;5
       ldy    $9E                     ;3
       sta    GRP1                    ;3
       stx    GRP0                    ;3
       sty    GRP1                    ;3
       sta    GRP0                    ;3
       dec    $9F                     ;5
       bpl    LFD5C                   ;2
       lda    #$80                    ;2
       sta    HMP0                    ;3
       sta    HMP1                    ;3
       asl                            ;2
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       rts                            ;6






LFDB6: ;46
       ldx    #$01                    ;2
LFDB8:
       bit    $98                     ;3
       bmi    LFDE3                   ;2
       ldy    $9A                     ;3
       sed                            ;2
       clc                            ;2
LFDC0:
       adc    $9A,X                   ;4
       sta    $9A,X                   ;4
       lda    #$00                    ;2
       dex                            ;2
       bpl    LFDC0                   ;2
       cld                            ;2
       bcc    LFDD6                   ;2 branch if no rollover
       lda    #$AA                    ;2
       sta    $9C                     ;3 change score to all badges
       sta    $9B                     ;3
       sta    $9A                     ;3
       inc    $88                     ;5 set game to inactive
LFDD6:
       cpy    $9A                     ;3
       beq    LFDE3                   ;2
       ldx    $96                     ;3
       inx                            ;2
       cpx    #$04                    ;2
       bcs    LFDE3                   ;2
       stx    $96                     ;3
LFDE3:
       rts                            ;6


LFFC0:
       .byte $43 ; | X    XX| $FFC0
       .byte $43 ; | X    XX| $FFC1
       .byte $00 ; |        | $FFC2
       .byte $00 ; |        | $FFC3
       .byte $C0 ; |XX      | $FFC4
       .byte $00 ; |        | $FFC5
LFFC6: ;8 bytes
       .byte $E0 ; |XXX     | $FFC6
       .byte $D0 ; |XX X    | $FFC7
       .byte $C0 ; |XX      | $FFC8
       .byte $F0 ; |XXXX    | $FFC9
       .byte $D0 ; |XX X    | $FFCA
       .byte $E0 ; |XXX     | $FFCB
       .byte $F0 ; |XXXX    | $FFCC
       .byte $C0 ; |XX      | $FFCD
LFFCE:
       .byte $00 ; |        | $FFCE
       .byte $06 ; |     XX | $FFCF
       .byte $1A ; |   XX X | $FFD0
       .byte $2E ; |  X XXX | $FFD1
       .byte $42 ; | X    X | $FFD2
       .byte $56 ; | X X XX | $FFD3
       .byte $6A ; | XX X X | $FFD4
       .byte $78 ; | XXXX   | $FFD5
       .byte $00 ; |        | $FFD6
       .byte $86 ; |X    XX | $FFD7
       .byte $AD ; |X X XX X| $FFD8
       .byte $B7 ; |X XX XXX| $FFD9
       .byte $A4 ; |X X  X  | $FFDA
       .byte $A9 ; |X X X  X| $FFDB
       .byte $00 ; |        | $FFDC
       .byte $8F ; |X   XXXX| $FFDD
       .byte $99 ; |X  XX  X| $FFDE
LFFDF:
       .byte $2D ; |  X XX X| $FFDF
       .byte $00 ; |        | $FFE0
       .byte $00 ; |        | $FFE1
       .byte $00 ; |        | $FFE2
       .byte $00 ; |        | $FFE3
       .byte $00 ; |        | $FFE4
       .byte $79 ; | XXXX  X| $FFE5
       .byte $79 ; | XXXX  X| $FFE6
       .byte $2D ; |  X XX X| $FFE7
       .byte $47 ; | X   XXX| $FFE8
LFFE9:
       .byte $24 ; |  X  X  | $FFE9
       .byte $24 ; |  X  X  | $FFEA
       .byte $69 ; | XX X  X| $FFEB
       .byte $69 ; | XX X  X| $FFEC
       .byte $69 ; | XX X  X| $FFED
       .byte $1C ; |   XXX  | $FFEE
       .byte $14 ; |   X X  | $FFEF
;       .byte $F4 ; |XXXX X  | $FFF0
;       .byte $F8 ; |XXXXX   | $FFF1
;       .byte $F8 ; |XXXXX   | $FFF2
    IF PLUSROM = 1
       .byte <spriteCopies
       .byte <spriteCopies + 4 
       .byte <spriteCopies + 4
    ELSE
;altered so sprite copies won't bankswitch 
       .byte $F4 ; |        | $FFF4
       .byte $F6 ; |        | $FFF4 (Al_Nafuur: Will not work on the SC for higher levels,
       .byte $F6 ; |        | $FFF4  because index reads with +3 and +4 seem to occure!)
    ENDIF

       .byte $58 ; | X XX   | $FFF3
       .byte $00 ; |        | $FFF4
       .byte $04 ; |     X  | $FFF5
       .byte $06 ; |     XX | $FFF6
       .byte $06 ; |     XX | $FFF7
       .byte $00 ; |        | $FFF8
       .byte $00 ; |        | $FFF9
       .byte $04 ; |     X  | $FFFA
       .byte $04 ; |     X  | $FFFB


LFDE4: ;9
       bit    $98                     ;3
       bmi    LFDEC                   ;2
       sta    AUDC0                   ;3
       sta    AUDV0                   ;3
LFDEC:
       rts                            ;6


       ORG $FDED

       .byte $00 ; |        | $FDED
       .byte $06 ; |     XX | $FDEE
       .byte $0C ; |    XX  | $FDEF
       .byte $18 ; |   XX   | $FDF0
       .byte $00 ; |        | $FDF1
       .byte $60 ; | XX     | $FDF2
       .byte $30 ; |  XX    | $FDF3
       .byte $18 ; |   XX   | $FDF4
       .byte $00 ; |        | $FDF5
       .byte $06 ; |     XX | $FDF6
       .byte $0C ; |    XX  | $FDF7
       .byte $18 ; |   XX   | $FDF8
       .byte $00 ; |        | $FDF9
       .byte $00 ; |        | $FDFA
       .byte $00 ; |        | $FDFB
       .byte $00 ; |        | $FDFC
LFDFD:
       .byte $00 ; |        | $FDFD
       .byte $05 ; |     X X| $FDFE
       .byte $07 ; |     XXX| $FDFF

       ORG $FE00

LFE00:
       .byte $01 ; |       X| $FE00
       .byte $15 ; |   X X X| $FE01
       .byte $15 ; |   X X X| $FE02
       .byte $15 ; |   X X X| $FE03
       .byte $15 ; |   X X X| $FE04
       .byte $15 ; |   X X X| $FE05
       .byte $0F ; |    XXXX| $FE06
       .byte $0F ; |    XXXX| $FE07
       .byte $07 ; |     XXX| $FE08
       .byte $0B ; |    X XX| $FE09
       .byte $0A ; |    X X | $FE0A
       .byte $0A ; |    X X | $FE0B
       .byte $05 ; |     X X| $FE0C
       .byte $04 ; |     X  | $FE0D
       .byte $01 ; |       X| $FE0E
       .byte $0B ; |    X XX| $FE0F
       .byte $0C ; |    XX  | $FE10
LFE11:
       .byte $4A ; | X  X X | $FE11
       .byte $4A ; | X  X X | $FE12
       .byte $C0 ; |XX      | $FE13
       .byte $C0 ; |XX      | $FE14
       .byte $C0 ; |XX      | $FE15
       .byte $4A ; | X  X X | $FE16
       .byte $67 ; | XX  XXX| $FE17
       .byte $A6 ; |X X  XX | $FE18
       .byte $A6 ; |X X  XX | $FE19
LFE1A:
       .byte $00 ; |        | $FE1A
       .byte $10 ; |   X    | $FE1B
       .byte $1C ; |   XXX  | $FE1C
       .byte $03 ; |      XX| $FE1D
       .byte $1C ; |   XXX  | $FE1E
       .byte $10 ; |   X    | $FE1F
       .byte $00 ; |        | $FE20
       .byte $00 ; |        | $FE21
LFE22:
       .byte $00 ; |        | $FE22
       .byte $08 ; |    X   | $FE23
       .byte $00 ; |        | $FE24
       .byte $0F ; |    XXXX| $FE25
       .byte $1C ; |   XXX  | $FE26
       .byte $08 ; |    X   | $FE27
       .byte $80 ; |X       | $FE28
       .byte $80 ; |X       | $FE29
       .byte $00 ; |        | $FE2A
       .byte $70 ; | XXX    | $FE2B
       .byte $78 ; | XXXX   | $FE2C
       .byte $FE ; |XXXXXXX | $FE2D
       .byte $F6 ; |XXXX XX | $FE2E
       .byte $FC ; |XXXXXX  | $FE2F
       .byte $78 ; | XXXX   | $FE30
       .byte $38 ; |  XXX   | $FE31
       .byte $0C ; |    XX  | $FE32
       .byte $1C ; |   XXX  | $FE33
       .byte $1C ; |   XXX  | $FE34
       .byte $1E ; |   XXXX | $FE35
       .byte $1C ; |   XXX  | $FE36
       .byte $3F ; |  XXXXXX| $FE37
       .byte $1E ; |   XXXX | $FE38
       .byte $1E ; |   XXXX | $FE39
       .byte $1E ; |   XXXX | $FE3A
       .byte $0C ; |    XX  | $FE3B
       .byte $3E ; |  XXXXX | $FE3C
       .byte $34 ; |  XX X  | $FE3D
       .byte $3E ; |  XXXXX | $FE3E
       .byte $3E ; |  XXXXX | $FE3F
       .byte $7E ; | XXXXXX | $FE40
       .byte $76 ; | XXX XX | $FE41
       .byte $7E ; | XXXXXX | $FE42
       .byte $76 ; | XXX XX | $FE43
       .byte $7E ; | XXXXXX | $FE44
       .byte $34 ; |  XX X  | $FE45
       .byte $18 ; |   XX   | $FE46
       .byte $38 ; |  XXX   | $FE47
       .byte $38 ; |  XXX   | $FE48
       .byte $3C ; |  XXXX  | $FE49
       .byte $38 ; |  XXX   | $FE4A
       .byte $7E ; | XXXXXX | $FE4B
       .byte $3C ; |  XXXX  | $FE4C
       .byte $3C ; |  XXXX  | $FE4D
       .byte $3C ; |  XXXX  | $FE4E
       .byte $18 ; |   XX   | $FE4F
LFE50:
       .byte $00 ; |        | $FE50
       .byte $7F ; | XXXXXXX| $FE51
       .byte $7F ; | XXXXXXX| $FE52
       .byte $5F ; | X XXXXX| $FE53
       .byte $5F ; | X XXXXX| $FE54
       .byte $5A ; | X XX X | $FE55
       .byte $1A ; |   XX X | $FE56
       .byte $00 ; |        | $FE57
       .byte $00 ; |        | $FE58
       .byte $00 ; |        | $FE59
       .byte $00 ; |        | $FE5A
       .byte $B3 ; |X XX  XX| $FE5B
       .byte $FE ; |XXXXXXX | $FE5C
       .byte $36 ; |  XX XX | $FE5D
       .byte $BE ; |X XXXXX | $FE5E
       .byte $F4 ; |XXXX X  | $FE5F
       .byte $7E ; | XXXXXX | $FE60
       .byte $19 ; |   XX  X| $FE61
       .byte $39 ; |  XXX  X| $FE62
       .byte $39 ; |  XXX  X| $FE63
       .byte $3D ; |  XXXX X| $FE64
       .byte $39 ; |  XXX  X| $FE65
       .byte $7E ; | XXXXXX | $FE66
       .byte $3C ; |  XXXX  | $FE67
       .byte $3C ; |  XXXX  | $FE68
       .byte $3C ; |  XXXX  | $FE69
       .byte $18 ; |   XX   | $FE6A
       .byte $80 ; |X       | $FE6B
       .byte $83 ; |X     XX| $FE6C
       .byte $FF ; |XXXXXXXX| $FE6D
       .byte $7E ; | XXXXXX | $FE6E
       .byte $7E ; | XXXXXX | $FE6F
       .byte $36 ; |  XX XX | $FE70
       .byte $7E ; | XXXXXX | $FE71
       .byte $77 ; | XXX XXX| $FE72
       .byte $7D ; | XXXXX X| $FE73
       .byte $34 ; |  XX X  | $FE74
       .byte $18 ; |   XX   | $FE75
       .byte $38 ; |  XXX   | $FE76
       .byte $38 ; |  XXX   | $FE77
       .byte $3C ; |  XXXX  | $FE78
       .byte $38 ; |  XXX   | $FE79
       .byte $7E ; | XXXXXX | $FE7A
       .byte $3C ; |  XXXX  | $FE7B
       .byte $3C ; |  XXXX  | $FE7C
       .byte $3C ; |  XXXX  | $FE7D
       .byte $18 ; |   XX   | $FE7E
       .byte $02 ; |      X | $FE7F
       .byte $83 ; |X     XX| $FE80
       .byte $FC ; |XXXXXX  | $FE81
       .byte $FC ; |XXXXXX  | $FE82
       .byte $3C ; |  XXXX  | $FE83
       .byte $34 ; |  XX X  | $FE84
       .byte $7E ; | XXXXXX | $FE85
       .byte $77 ; | XXX XXX| $FE86
       .byte $7D ; | XXXXX X| $FE87
       .byte $34 ; |  XX X  | $FE88
       .byte $18 ; |   XX   | $FE89
       .byte $38 ; |  XXX   | $FE8A
       .byte $38 ; |  XXX   | $FE8B
       .byte $3C ; |  XXXX  | $FE8C
       .byte $38 ; |  XXX   | $FE8D
       .byte $7E ; | XXXXXX | $FE8E
       .byte $3C ; |  XXXX  | $FE8F
       .byte $3C ; |  XXXX  | $FE90
       .byte $3C ; |  XXXX  | $FE91
       .byte $18 ; |   XX   | $FE92
       .byte $18 ; |   XX   | $FE93
       .byte $50 ; | X X    | $FE94
       .byte $7E ; | XXXXXX | $FE95
       .byte $7E ; | XXXXXX | $FE96
       .byte $3C ; |  XXXX  | $FE97
       .byte $3E ; |  XXXXX | $FE98
       .byte $3E ; |  XXXXX | $FE99
       .byte $3C ; |  XXXX  | $FE9A
       .byte $3C ; |  XXXX  | $FE9B
       .byte $34 ; |  XX X  | $FE9C
       .byte $18 ; |   XX   | $FE9D
       .byte $38 ; |  XXX   | $FE9E
       .byte $38 ; |  XXX   | $FE9F
       .byte $3C ; |  XXXX  | $FEA0
       .byte $38 ; |  XXX   | $FEA1
       .byte $7E ; | XXXXXX | $FEA2
       .byte $3C ; |  XXXX  | $FEA3
       .byte $3C ; |  XXXX  | $FEA4
       .byte $3C ; |  XXXX  | $FEA5
       .byte $18 ; |   XX   | $FEA6
       .byte $20 ; |  X     | $FEA7
       .byte $46 ; | X   XX | $FEA8
       .byte $7C ; | XXXXX  | $FEA9
       .byte $7E ; | XXXXXX | $FEAA
       .byte $7E ; | XXXXXX | $FEAB
       .byte $34 ; |  XX X  | $FEAC
       .byte $3E ; |  XXXXX | $FEAD
       .byte $3E ; |  XXXXX | $FEAE
       .byte $3C ; |  XXXX  | $FEAF
       .byte $34 ; |  XX X  | $FEB0
       .byte $18 ; |   XX   | $FEB1
       .byte $38 ; |  XXX   | $FEB2
       .byte $38 ; |  XXX   | $FEB3
       .byte $3C ; |  XXXX  | $FEB4
       .byte $38 ; |  XXX   | $FEB5
       .byte $7E ; | XXXXXX | $FEB6
       .byte $3C ; |  XXXX  | $FEB7
       .byte $3C ; |  XXXX  | $FEB8
       .byte $3C ; |  XXXX  | $FEB9
       .byte $18 ; |   XX   | $FEBA


LFEBB: ;10
       lda    $F0                     ;3
       and    #$03                    ;2
       cmp    #$03                    ;2
       bne    LFEC4                   ;2
       lsr                            ;2
LFEC4:
       rts                            ;6

LFEC5:
       .byte $04 ; |     X  | $FEC5
LFEC6:
       .byte $0B ; |    X XX| $FEC6
       .byte $14 ; |   X X  | $FEC7
       .byte $0B ; |    X XX| $FEC8
       .byte $04 ; |     X  | $FEC9
LFECA:
       .byte $3D ; |  XXXX X| $FECA
       .byte $6C ; | XX XX  | $FECB
       .byte $58 ; | X XX   | $FECC
       .byte $80 ; |X       | $FECD
       .byte $94 ; |X  X X  | $FECE
       .byte $A8 ; |X X X   | $FECF
       .byte $3D ; |  XXXX X| $FED0
       .byte $2C ; |  X XX  | $FED1
LFED2:
       .byte $4D ; | X  XX X| $FED2
       .byte $00 ; |        | $FED3
       .byte $00 ; |        | $FED4
       .byte $00 ; |        | $FED5
       .byte $0B ; |    X XX| $FED6
       .byte $06 ; |     XX | $FED7
       .byte $03 ; |      XX| $FED8
       .byte $00 ; |        | $FED9
       .byte $00 ; |        | $FEDA
       .byte $00 ; |        | $FEDB
       .byte $00 ; |        | $FEDC
       .byte $00 ; |        | $FEDD
       .byte $00 ; |        | $FEDE
  IF PAL
LFEDF:
       .byte $26 ; |   X XX | $FEDF
       .byte $26 ; |   X XX | $FEE0
       .byte $26 ; |   X XX | $FEE1
       .byte $C8 ; | XXXX   | $FEE2
LFEE3:
       .byte $26 ; |   X XX | $FEE3
       .byte $28 ; |   XX   | $FEE4
       .byte $54 ; |XX   X  | $FEE5
       .byte $54 ; |XX   X  | $FEE6
       .byte $26 ; |   X XX | $FEE7
       .byte $28 ; |   XX   | $FEE8
       .byte $54 ; |XX   X  | $FEE9
       .byte $54 ; |XX   X  | $FEEA
       .byte $26 ; |   X XX | $FEEB
       .byte $28 ; |   XX   | $FEEC
       .byte $54 ; |XX   X  | $FEED
       .byte $54 ; |XX   X  | $FEEE
       .byte $26 ; |   X XX | $FEEF
       .byte $28 ; |   XX   | $FEF0
       .byte $08 ; |    X   | $FEF1
       .byte $08 ; |    X   | $FEF2
       .byte $08 ; |    X   | $FEF3
  ELSE
LFEDF:
       .byte $16 ; |   X XX | $FEDF
       .byte $16 ; |   X XX | $FEE0
       .byte $16 ; |   X XX | $FEE1
       .byte $78 ; | XXXX   | $FEE2
LFEE3:
       .byte $16 ; |   X XX | $FEE3
       .byte $18 ; |   XX   | $FEE4
       .byte $C4 ; |XX   X  | $FEE5
       .byte $C4 ; |XX   X  | $FEE6
       .byte $16 ; |   X XX | $FEE7
       .byte $18 ; |   XX   | $FEE8
       .byte $C4 ; |XX   X  | $FEE9
       .byte $C4 ; |XX   X  | $FEEA
       .byte $16 ; |   X XX | $FEEB
       .byte $18 ; |   XX   | $FEEC
       .byte $C4 ; |XX   X  | $FEED
       .byte $C4 ; |XX   X  | $FEEE
       .byte $16 ; |   X XX | $FEEF
       .byte $18 ; |   XX   | $FEF0
       .byte $08 ; |    X   | $FEF1
       .byte $08 ; |    X   | $FEF2
       .byte $08 ; |    X   | $FEF3
  ENDIF

LFEF4:
       sed                            ;2
       lda    $9D                     ;3
       sec                            ;2
       sbc    #$01                    ;2
       bcc    LFEFE                   ;2
       sta    $9D                     ;3
LFEFE:
       cld                            ;2
Waste_12_cycles:
       rts                            ;6

       ORG $FF00

       .byte $00 ; |        | $FF00
       .byte $3C ; |  XXXX  | $FF01
       .byte $66 ; | XX  XX | $FF02
       .byte $66 ; | XX  XX | $FF03
       .byte $66 ; | XX  XX | $FF04
       .byte $66 ; | XX  XX | $FF05
       .byte $66 ; | XX  XX | $FF06
       .byte $3C ; |  XXXX  | $FF07
       .byte $00 ; |        | $FF08
       .byte $3C ; |  XXXX  | $FF09
       .byte $18 ; |   XX   | $FF0A
       .byte $18 ; |   XX   | $FF0B
       .byte $18 ; |   XX   | $FF0C
       .byte $18 ; |   XX   | $FF0D
       .byte $38 ; |  XXX   | $FF0E
       .byte $18 ; |   XX   | $FF0F
       .byte $00 ; |        | $FF10
       .byte $7E ; | XXXXXX | $FF11
       .byte $60 ; | XX     | $FF12
       .byte $60 ; | XX     | $FF13
       .byte $3C ; |  XXXX  | $FF14
       .byte $06 ; |     XX | $FF15
       .byte $46 ; | X   XX | $FF16
       .byte $3C ; |  XXXX  | $FF17
       .byte $00 ; |        | $FF18
       .byte $3C ; |  XXXX  | $FF19
       .byte $46 ; | X   XX | $FF1A
       .byte $06 ; |     XX | $FF1B
       .byte $0C ; |    XX  | $FF1C
       .byte $06 ; |     XX | $FF1D
       .byte $46 ; | X   XX | $FF1E
       .byte $3C ; |  XXXX  | $FF1F
       .byte $00 ; |        | $FF20
       .byte $0C ; |    XX  | $FF21
       .byte $0C ; |    XX  | $FF22
       .byte $7E ; | XXXXXX | $FF23
       .byte $4C ; | X  XX  | $FF24
       .byte $2C ; |  X XX  | $FF25
       .byte $1C ; |   XXX  | $FF26
       .byte $0C ; |    XX  | $FF27
       .byte $00 ; |        | $FF28
       .byte $7C ; | XXXXX  | $FF29
       .byte $46 ; | X   XX | $FF2A
       .byte $06 ; |     XX | $FF2B
       .byte $7C ; | XXXXX  | $FF2C
       .byte $60 ; | XX     | $FF2D
       .byte $60 ; | XX     | $FF2E
       .byte $7E ; | XXXXXX | $FF2F
       .byte $00 ; |        | $FF30
       .byte $3C ; |  XXXX  | $FF31
       .byte $66 ; | XX  XX | $FF32
       .byte $66 ; | XX  XX | $FF33
       .byte $7C ; | XXXXX  | $FF34
       .byte $60 ; | XX     | $FF35
       .byte $62 ; | XX   X | $FF36
       .byte $3C ; |  XXXX  | $FF37
       .byte $00 ; |        | $FF38
       .byte $18 ; |   XX   | $FF39
       .byte $18 ; |   XX   | $FF3A
       .byte $18 ; |   XX   | $FF3B
       .byte $0C ; |    XX  | $FF3C
       .byte $06 ; |     XX | $FF3D
       .byte $42 ; | X    X | $FF3E
       .byte $7E ; | XXXXXX | $FF3F
       .byte $00 ; |        | $FF40
       .byte $3C ; |  XXXX  | $FF41
       .byte $66 ; | XX  XX | $FF42
       .byte $66 ; | XX  XX | $FF43
       .byte $3C ; |  XXXX  | $FF44
       .byte $66 ; | XX  XX | $FF45
       .byte $66 ; | XX  XX | $FF46
       .byte $3C ; |  XXXX  | $FF47
       .byte $00 ; |        | $FF48
       .byte $3C ; |  XXXX  | $FF49
       .byte $46 ; | X   XX | $FF4A
       .byte $06 ; |     XX | $FF4B
       .byte $3E ; |  XXXXX | $FF4C
       .byte $66 ; | XX  XX | $FF4D
       .byte $66 ; | XX  XX | $FF4E
       .byte $3C ; |  XXXX  | $FF4F
       .byte $00 ; |        | $FF50
       .byte $3F ; |  XXXXXX| $FF51
       .byte $1E ; |   XXXX | $FF52
       .byte $1E ; |   XXXX | $FF53
       .byte $1E ; |   XXXX | $FF54
       .byte $1E ; |   XXXX | $FF55
       .byte $0C ; |    XX  | $FF56
       .byte $00 ; |        | $FF57

       .byte $00 ; |        | $FF58
       .byte $00 ; |        | $FF59
       .byte $00 ; |        | $FF5A
       .byte $00 ; |        | $FF5B
       .byte $00 ; |        | $FF5C
       .byte $00 ; |        | $FF5D
       .byte $00 ; |        | $FF5E
       .byte $00 ; |        | $FF5F

    IF PLUSROM = 0
       .byte $F7 ; |XXXX XXX| $FF60
       .byte $95 ; |X  X X X| $FF61
       .byte $87 ; |X    XXX| $FF62
       .byte $90 ; |X  X    | $FF63
       .byte $F0 ; |XXXX    | $FF64
       .byte $00 ; |        | $FF65
       .byte $47 ; | X   XXX| $FF66
       .byte $41 ; | X     X| $FF67
       .byte $77 ; | XXX XXX| $FF68
       .byte $55 ; | X X X X| $FF69
       .byte $75 ; | XXX X X| $FF6A
       .byte $00 ; |        | $FF6B
       .byte $00 ; |        | $FF6C
       .byte $00 ; |        | $FF6D
       .byte $03 ; |      XX| $FF6E
       .byte $00 ; |        | $FF6F
       .byte $4B ; | X  X XX| $FF70
       .byte $4A ; | X  X X | $FF71
       .byte $6B ; | XX X XX| $FF72
       .byte $00 ; |        | $FF73
       .byte $08 ; |    X   | $FF74
       .byte $00 ; |        | $FF75
       .byte $80 ; |X       | $FF76
       .byte $80 ; |X       | $FF77
       .byte $AA ; |X X X X | $FF78
       .byte $AA ; |X X X X | $FF79
       .byte $BA ; |X XXX X | $FF7A
       .byte $27 ; |  X  XXX| $FF7B
       .byte $22 ; |  X   X | $FF7C
       .byte $00 ; |        | $FF7D
       .byte $00 ; |        | $FF7E
       .byte $00 ; |        | $FF7F
       .byte $11 ; |   X   X| $FF80
       .byte $11 ; |   X   X| $FF81
       .byte $17 ; |   X XXX| $FF82
       .byte $15 ; |   X X X| $FF83
       .byte $17 ; |   X XXX| $FF84
       .byte $00 ; |        | $FF85
       .byte $00 ; |        | $FF86
       .byte $00 ; |        | $FF87
       .byte $77 ; | XXX XXX| $FF88
       .byte $51 ; | X X   X| $FF89
       .byte $73 ; | XXX  XX| $FF8A
       .byte $51 ; | X X   X| $FF8B
       .byte $77 ; | XXX XXX| $FF8C
   ENDIF

  IF PAL
LFF8D:
       .byte $D4 ; |X    X  | $FF8D
       .byte $36 ; |XX X XX | $FF8E
       .byte $36 ; |XX X XX | $FF8F
       .byte $2A ; |   XX X | $FF90
       .byte $26 ; |  X  XX | $FF91
       .byte $26 ; |  X  XX | $FF92
       .byte $64 ; | X   X  | $FF93
  ELSE
LFF8D:
       .byte $84 ; |X    X  | $FF8D
       .byte $D6 ; |XX X XX | $FF8E
       .byte $D6 ; |XX X XX | $FF8F
       .byte $1A ; |   XX X | $FF90
       .byte $26 ; |  X  XX | $FF91
       .byte $26 ; |  X  XX | $FF92
       .byte $44 ; | X   X  | $FF93
  ENDIF
       .byte $01 ; |       X| $FF94
       .byte $00 ; |        | $FF95
       .byte $02 ; |      X | $FF96
       .byte $03 ; |      XX| $FF97
LFF98:
       .byte $0C ; |    XX  | $FF98
       .byte $06 ; |     XX | $FF99
       .byte $03 ; |      XX| $FF9A
       .byte $01 ; |       X| $FF9B
       .byte $00 ; |        | $FF9C
       .byte $00 ; |        | $FF9D
       .byte $00 ; |        | $FF9E
       .byte $00 ; |        | $FF9F

LFFA0:
       .byte $2D ; |  X XX X| $FFA0
       .byte $29 ; |  X X  X| $FFA1
       .byte $E9 ; |XXX X  X| $FFA2
       .byte $A9 ; |X X X  X| $FFA3
       .byte $ED ; |XXX XX X| $FFA4
       .byte $61 ; | XX    X| $FFA5
       .byte $2F ; |  X XXXX| $FFA6
       .byte $00 ; |        | $FFA7

LFFA8:
       .byte $50 ; | X X    | $FFA8
       .byte $58 ; | X XX   | $FFA9
       .byte $5C ; | X XXX  | $FFAA
       .byte $56 ; | X X XX | $FFAB
       .byte $53 ; | X X  XX| $FFAC
       .byte $11 ; |   X   X| $FFAD
       .byte $F0 ; |XXXX    | $FFAE
       .byte $00 ; |        | $FFAF
LFFB0:
       .byte $BA ; |X XXX X | $FFB0
       .byte $8A ; |X   X X | $FFB1
       .byte $BA ; |X XXX X | $FFB2
       .byte $A2 ; |X X   X | $FFB3
       .byte $3A ; |  XXX X | $FFB4
       .byte $80 ; |X       | $FFB5
       .byte $FE ; |XXXXXXX | $FFB6
       .byte $00 ; |        | $FFB7


LFD91: ;22
       sta    $9E                     ;3
       lda    $97                     ;3
       lsr                            ;2
       lsr                            ;2
       tay                            ;2
       lda    $97                     ;3
       and    #$03                    ;2
       cmp    $9E                     ;3
       bcs    LFDA5                   ;2
       tya                            ;2
       beq    LFDA5                   ;2
       sec                            ;2
       dey                            ;2
LFDA5:
       tya                            ;2
       rts                            ;6


LFD0D: ;23
       ldx    #$FF                    ;2
       sec                            ;2
LFD10:
       inx                            ;2
       sbc    #$0F                    ;2
       bcs    LFD10                   ;2
       stx    $CD,Y                   ;4
       eor    #$0F                    ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       adc    #$90                    ;2
       sta.wy $D7,Y                   ;5
       dey                            ;2
       rts                            ;6


LFDA7: ;15
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
LFDAB:
       clc                            ;2
       adc    #$0A                    ;2
       iny                            ;2
       cpy    #$08                    ;2
       bcc    LFDAB                   ;2
       adc    #$1F                    ;2
       rts                            ;6

    IF PLUSROM = 1
SendPlusROMScore
       lda $9A                        ; Score Hi BCD
       sta WriteToBuffer              ; 
       lda $9B                        ; Score Mid BCD
       sta WriteToBuffer              ; 
       lda $9C                        ; Score Lo BCD
       sta WriteToBuffer              ; 
       lda #HIGHSCORE_ID              ; game id in Highscore DB
       sta WriteSendBuffer
       inc    $88                     ;5
       iny                            ;2
       rts

        org $FFE0
spriteCopies
       .byte $00
       .byte $04
       .byte $06
       .byte $06
       .byte $00
       .byte $00
       .byte $04
       .byte $04

PlusROM_API
       .byte "a", 0, "h.firmaplus.de", 0

      ORG $FFFA
       .word (PlusROM_API - $E000)      ; PlusRom API pointer

    ELSE
       ORG $FFF4                        ;(this spriteCopies table seems to be wrong and too short!)
       .byte $00
       .byte $00
       .byte $00
       .byte $00
       .byte "2009"
    ENDIF
       .word START
       .word START
