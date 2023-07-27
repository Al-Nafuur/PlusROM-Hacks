;SPITFIRE ATTACK (c)1983 Milton Bradley: Supercharger conv. by Kurt (Nukey Shay) Howe, 10/06/08
; PlusROM HSC hack added by Wolfgang Stubig (Al_Nafuur) 7/2023

SUPERCHARGER = 0
PLUSROM = 1

PAL60 = 0
RIVER = 1 ;(alters color of the "road"...looks more like a river anyway)


   IF SUPERCHARGER = 1 && PLUSROM = 1

      echo ""
      echo "*** ERROR: SuperCharger and PlusROM arn't compatible"
      echo ""
      err

   ENDIF

;Changelog:
; Supercharger-compatability added
; Right difficulty switch implemented (to swap flightstick direction...thanks, Debro!)
; Scanline jitter fixed (simply bumped up the max timer by 1, robbing added scanline elsewhere)
; 7800 console detection added
; Autofire mode added (B&W)
; PAL60 option added


; Disassembly of Spitfire.bin
; Disassembled Mon Oct 06 16:34:52 2008
; Using DiStella v3.0
; Command Line: C:\BIN\D3.EXE -pafscSpitfire.cfg Spitfire.bin 
; Spitfire.cfg contents:
;      ORG  F000
;      CODE F000 F093
;      CODE F094 F096
;      CODE F097 F87C
;      GFX  F87D FCFF
;      CODE FD00 FFE0
;      GFX  FFE1 FFFF

      processor 6502

VSYNC   =  $00
VBLANK  =  $01
WSYNC   =  $02
NUSIZ0  =  $04
NUSIZ1  =  $05
COLUP0  =  $06
COLUP1  =  $07
COLUBK  =  $09
CTRLPF  =  $0A
PF0     =  $0D
RESP0   =  $10
RESP1   =  $11
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
HMP1    =  $21
HMM0    =  $22
HMM1    =  $23
VDELP0  =  $25
VDELP1  =  $26
HMOVE   =  $2A
HMCLR   =  $2B
CXCLR   =  $2C
CXM0P   =  $30
INPT4   =  $3C
SWCHA   =  $0280
SWCHB   =  $0282
TIMINT  =  $0285
$029D   =  $029D ; short
$029E   =  $029E ; long

console = $DF
;$E0-$FF unused (stack only on upper ram)

   IF PLUSROM = 1

WriteToBuffer           = $1FF0
WriteSendBuffer         = $1FF1
ReceiveBuffer           = $1FF2
ReceiveBufferSize       = $1FF3

HIGHSCORE_ID            = 70         ; Spitfire Attack game ID in Highscore DB

   ENDIF

       ORG $F000

START:
;       sei                            ;2
;       cld                            ;2
;       lda    #$00                    ;2
;       ldx    #$FF                    ;2
;       txs                            ;2
;LF007:
;       sta    VSYNC,X                 ;4
;       dex                            ;2
;       bne    LF007                   ;2

       sei                            ;2
       cld                            ;2
       lda    #$00                    ;2 Clear TIA Registers
       tay                            ;2 console = 2600
       ldx    $D0                     ;3
       cpx    #$2C                    ;2
       bne    SaveConsole             ;2
       ldx    $D1                     ;3
       cpx    #$A9                    ;2
       bne    SaveConsole             ;2
       ldy    #$80                    ;2 console = 7800
SaveConsole:
       tax                            ;2
;ram init loop...
ResetAll:
       dex                            ;2
       txs                            ;2
       pha                            ;3 Everything Off
       bne    ResetAll                ;2
       sty    console                 ;3 save console type

       jsr    LF6E6                   ;6
       lda    #$07                    ;2
       sta    $89                     ;3
       lda    #$06                    ;2
       sta    $DC                     ;3
       lda    #$67                    ;2
       sta    $BA                     ;3
LF01B:
       sta    WSYNC                   ;3
       lda    #$02                    ;2
       sta    VBLANK                  ;3
       sta    VSYNC                   ;3
       lda    #$2A                    ;2
       sta    $029D                   ;4
       lda    #$00                    ;2
       sta    AUDV0                   ;3
       lda    $DD                     ;3
       cmp    #$05                    ;2
       bcc    LF044                   ;2
       lda    $81                     ;3
       and    #$03                    ;2
       bne    LF044                   ;2
       lda    #$FF                    ;2
       sta    AUDV0                   ;3
       lda    #$10                    ;2
       sta    AUDF0                   ;3
       lda    #$01                    ;2
       sta    AUDC0                   ;3
LF044:
       nop                            ;2
       lda    $D9                     ;3
       bne    LF05A                   ;2
       lda    $D1                     ;3
       bne    LF056                   ;2
       lda    $D6                     ;3
       cmp    #$10                    ;2
;       bcs    LF056                   ;2 ?? JMP is in range for a BCC instead!
;       jmp    LF06B                   ;3
       bcc    LF06B                   ;2
LF056:
       lda    #$0F                    ;2
       sta    $D9                     ;3
LF05A:
       lda    $81                     ;3
       and    #$05                    ;2
       bne    LF062                   ;2
       dec    $D9                     ;5
LF062:
       lda    $D9                     ;3
       ldx    #$FF                    ;2
       ldy    #$08                    ;2
;       jmp    LF097                   ;3 could use BNE
       bne    LF097                   ;2 always branch

LF06B:
       lda    $9E                     ;3
       cmp    #$02                    ;2
       bcs    LF085                   ;2
       lda    $81                     ;3
       and    #$05                    ;2
       beq    LF07C                   ;2
       lda    #$00                    ;2
;       jmp    LF07E                   ;3 could use BIT.w
       .byte $2C                      ;4 skip 2 bytes
LF07C:
       lda    #$FF                    ;2
LF07E:
       ldx    #$0D                    ;2
       ldy    #$07                    ;2
;       jmp    LF097                   ;3 could use BNE
       bne    LF097                   ;2 always branch

LF085:
       lda    $83                     ;3
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       eor    #$1F                    ;2
       tax                            ;2
       lda    #$03                    ;2
;       ldy    #$03                    ;2 could use TAY
;       jmp    LF097                   ;3 could use BNE
       tay                            ;2
       bne    LF097                   ;2 always branch

;       jmp    LF09D                   ;3 unused code?

LF097:
       sta    AUDV1                   ;3
       stx    AUDF1                   ;3
       sty    AUDC1                   ;3
LF09D:
;       nop                            ;2 ??
       inc    $81                     ;5
       inc    $8B                     ;5
       lda    $89                     ;3
;       cmp    $8B                     ;3 could use EOR...
;       bne    LF0AC                   ;2
;       lda    #$00                    ;2 ...to eliminate this
       eor    $8B                     ;3
       bne    LF0AC                   ;2

       sta    $8B                     ;3
LF0AC:
       lda    TIMINT                  ;4
       bpl    LF0AC                   ;2
       lda    #$00                    ;2
       sta    WSYNC                   ;3
       sta    VSYNC                   ;3
       sta    VBLANK                  ;3
       sta    WSYNC                   ;3
;       lda    #$20                    ;2
       lda    #$21                    ;2
       sta    $029E                   ;4
       lda    SWCHB                   ;4


;added flippy switch routine
       tax                            ;2
       eor    console                 ;3 Compare with what was before
       and    #$08                    ;2 keep only color switch
       beq    NoToggle                ;2
       bit    console                 ;2
       bpl    A2600                   ;2
       txa                            ;2
       asl                            ;2
       and    #$08                    ;2 keep only color switch
       bne    NoToggle                ;2
A2600:
       lda    console                 ;3
       eor    #$40                    ;2 flip toggle
       .byte $2C                      ;4 skip 2 bytes
NoToggle:
       lda    console                 ;3
       and    #$F7                    ;2 keep settings
       sta    console                 ;3
       txa                            ;2
       and    #$08                    ;2 keep only color switch
       ora    console                 ;3
       sta    console                 ;3
       txa                            ;2


       and    #$01                    ;2
       bne    LF0CE                   ;2
;       jsr    LF6E6                   ;6 NOTE: could JSR to $F6E9 instead to store 0 to $DB!
;       lda    #$00                    ;2
;       sta    $DB                     ;3
       jsr    LF6E9                   ;6

LF0CE:
       lda    SWCHB                   ;4
       and    #$02                    ;2
       beq    LF0DC                   ;2
       lda    #$FF                    ;2
       sta    $DE                     ;3
;       jmp    LF103                   ;3 could BNE
       bne    LF103                   ;2 always branch

LF0DC:
       lda    $DE                     ;3
       beq    LF103                   ;2
       lda    #$00                    ;2
       sta    $DE                     ;3
       dec    $DC                     ;5
       lda    $DC                     ;3
;       cmp    #$00                    ;2 superfluous
       bne    LF0F4                   ;2

       sta    $8B                     ;3 moved

       lda    #$06                    ;2
       sta    $DC                     ;3
;       lda    #$00                    ;2
;       sta    $8B                     ;3 could move below the BNE above to eliminate LDA#
LF0F4:
;       nop                            ;2 ??
       ldx    $DC                     ;3
       lda    LF8F9,X                 ;4
       sta    $BA                     ;3
       jsr    LF750                   ;6
       lda    #$FF                    ;2
       sta    $DB                     ;3
LF103:
;       nop                            ;2 ??
       lda    $DB                     ;3
       bne    LF12E                   ;2
       lda    $81                     ;3
       bne    LF12E                   ;2
       lda    $AE                     ;3
       cmp    #$05                    ;2
       bcc    LF12E                   ;2
       lda    $AF                     ;3
       cmp    #$03                    ;2
       bne    LF12C                   ;2
       lda    #$30                    ;2
       sta    $D6                     ;3

       lda    #$00                    ;2 moved
       sta    $AE                     ;3 ""

       lda    #$FF                    ;2
       sta    $AF                     ;3
;       lda    #$00                    ;2 could move to preserve #$FF value...
;       sta    $AE                     ;3
       dec    $C8                     ;5
       bne    LF12C                   ;2
    IF PLUSROM
       jsr    SendPlusROMScore
    ELSE
;       lda    #$FF                    ;2
       sta    $DB                     ;3 ...to be used here
    ENDIF
LF12C:
       inc    $AF                     ;5
LF12E:
;       nop                            ;2 ??
       lda    $DB                     ;3
       bne    LF169                   ;2
       lda    $81                     ;3
       and    #$07                    ;2
       bne    LF169                   ;2
       lda    $83                     ;3
       cmp    #$5A                    ;2
       bcc    LF14A                   ;2
       lda    $CE                     ;3
       cmp    #$30                    ;2
       bcs    LF169                   ;2
       inc    $CE                     ;5
;       jmp    LF169                   ;3 $CE less than $31, so could use BPL here
       bpl    LF169                   ;2 always branch

LF14A:
       lda    $CE                     ;3
       beq    LF153                   ;2
       dec    $CE                     ;5
       jmp    LF169                   ;3

LF153:
       lda    #$00                    ;2 moved
       sta    $AE                     ;3 ""
       sta    $AF                     ;3 ""


       lda    #$30                    ;2
       sta    $D6                     ;3
;       lda    #$00                    ;2 could move these to preserve value #$30...
;       sta    $AE                     ;3
;       sta    $AF                     ;3
;       lda    #$30                    ;2
       sta    $CE                     ;3 ...to be used here
       dec    $C8                     ;5
       bne    LF169                   ;2
    IF PLUSROM
       jsr    SendPlusROMScore
    ELSE
       lda    #$FF                    ;2
       sta    $DB                     ;3
    ENDIF
LF169:
;       nop                            ;2 ??
       lda    $DB                     ;3
       beq    LF17F                   ;2
       lda    #$00                    ;2
       sta    $DD                     ;3
       sta    $AE                     ;3
       lda    #$62                    ;2
       sta    $83                     ;3
       lda    #$1F                    ;2
       sta    $84                     ;3
LF17C:
       jmp    LF203                   ;3 almost in range for BNE

LF17F:
       lda    $D6                     ;3
       cmp    #$05                    ;2
;       bcs    LF203                   ;2
       bcs    LF17C                   ;2

       ldx    #$01                    ;2
       ldy    #$A0                    ;2
       lda    SWCHA                   ;4
;       and    #$80                    ;2
;       bne    LF1AB                   ;2 could BMI to eliminate AND above
       bmi    LF1AB                   ;2

       dec    $AD                     ;5
       lda    $AD                     ;3
       bne    LF198                   ;2
       sty    $AD                     ;3
LF198:
       dec    $91                     ;5
       lda    $91                     ;3
       bne    LF1A0                   ;2
       sty    $91                     ;3
LF1A0:
       dec    $94                     ;5
       lda    $94                     ;3
       bne    LF1A8                   ;2
       sty    $94                     ;3
LF1A8:
       jsr    LF862                   ;6
LF1AB:
;       lda    SWCHA                   ;4
;       and    #$40                    ;2
;       bne    LF1D3                   ;2 could BIT SWCHA/BVS to eliminate AND above
       bit    SWCHA                   ;4
       bvs    LF1D3                   ;2

       inc    $AD                     ;5
       lda    $AD                     ;3
       cmp    #$A1                    ;2
       bne    LF1BC                   ;2
       stx    $AD                     ;3
LF1BC:
       inc    $91                     ;5
       lda    $91                     ;3
       cmp    #$A1                    ;2
       bne    LF1C6                   ;2
       stx    $91                     ;3
LF1C6:
       inc    $94                     ;5
       lda    $94                     ;3
       cmp    #$A1                    ;2
       bne    LF1D0                   ;2
       stx    $94                     ;3
LF1D0:
       jsr    LF86F                   ;6
LF1D3:
;;       lda    SWCHA                   ;4
;;       and    #$20                    ;2 use #$10 to alter flightstick
       lda    #$20                    ;2
       bit    SWCHB                   ;4 right difficulty swaps stick
       bmi    NoSwap1                 ;2
       lsr                            ;2 use #$10 to alter flightstick
NoSwap1:
       and    SWCHA                   ;4

       bne    LF1EB                   ;2
       lda    $83                     ;3
       cmp    #$9C                    ;2
       bcs    LF1EB                   ;2
       dec    $84                     ;5
       inc    $83                     ;5
       inc    $83                     ;5
       inc    $AC                     ;5
       inc    $AC                     ;5
;       nop                            ;2 ??
LF1EB:
;;       lda    SWCHA                   ;4
;;       and    #$10                    ;2 could use #$20 to alter flightstick
       lda    #$10                    ;2
       bit    SWCHB                   ;4 right difficulty swaps stick
       bmi    NoSwap2                 ;2
       asl                            ;2 use #$20 to alter flightstick
NoSwap2:
       and    SWCHA                   ;4


       bne    LF202                   ;2
       lda    #$0A                    ;2
       cmp    $83                     ;3
       bcs    LF203                   ;2
       inc    $84                     ;5
       dec    $83                     ;5
       dec    $83                     ;5
       dec    $AC                     ;5
       dec    $AC                     ;5
LF202:
;       nop                            ;2 ??
LF203:
;       nop                            ;2 ??
       lda    $DB                     ;3
       bne    LF228                   ;2

       bit    console                 ;3
       bvc    Autofire                ;2 no need to mash button when B&W is selected

       lda    $DD                     ;3
       cmp    #$05                    ;2
       bcs    LF226                   ;2

Autofire:
       bit    INPT4                   ;3
       bpl    LF219                   ;2

       lda    #$01                    ;2
       sta    $DD                     ;3
;       jmp    LF228                   ;3 could use BNE
       bne    LF228                   ;2 always branch

LF219:
       lda    $DD                     ;3
       cmp    #$01                    ;2
       bne    LF228                   ;2
       lda    #$10                    ;2
       sta    $DD                     ;3
;       jmp    LF228                   ;3 could use BNE
       bne    LF228                   ;2 always branch

LF226:
       dec    $DD                     ;5
LF228:
;       nop                            ;2 ??
       lda    $81                     ;3
       ldy    $DD                     ;3
       cpy    #$05                    ;2
       bcs    LF233                   ;2
       and    #$01                    ;2
LF233:
       and    #$03                    ;2
       tax                            ;2
       lda    LF8F5,X                 ;4
       sta    $96                     ;3
       lda    LF8F1,X                 ;4
       sta    $85                     ;3
       lda    LF8ED,X                 ;4
       sta    $A0                     ;3
       lda    $8B                     ;3
       bne    LF27E                   ;2
       lda    $88                     ;3
       sec                            ;2
       sbc    #$01                    ;2
       tay                            ;2
       lda    LFC00,Y                 ;4
       and    #$F0                    ;2
       beq    LF27E                   ;2
       tax                            ;2
       bpl    LF26C                   ;2
       cpx    #$E0                    ;2
       bne    LF263                   ;2
       jsr    LF862                   ;6
       jsr    LF862                   ;6
LF263:
       jsr    LF862                   ;6
       jsr    LF862                   ;6
       jmp    LF27E                   ;3

LF26C:
       lda    $95                     ;3
       cpx    #$20                    ;2
       bne    LF278                   ;2
       jsr    LF86F                   ;6
       jsr    LF86F                   ;6
LF278:
       jsr    LF86F                   ;6
       jsr    LF86F                   ;6
LF27E:
;       nop                            ;2 ??
       lda    $8B                     ;3
       bne    LF289                   ;2
       dec    $88                     ;5
       inc    $98                     ;5
       inc    $99                     ;5
LF289:
;       nop                            ;2 ??
       lda    $D1                     ;3
       bne    LF2B1                   ;2
       lda    $8B                     ;3
       bne    LF2B1                   ;2
       inc    $82                     ;5
       lda    $82                     ;3
;       cmp    #$0F                    ;2 could use EOR...
       eor    #$0F                    ;2

       bne    LF2B1                   ;2
;       lda    #$00                    ;2 ...to eliminate this
       sta    $82                     ;3
       lda    $9E                     ;3
       beq    LF2A4                   ;2
       dec    $9E                     ;5
LF2A4:
       lda    $9E                     ;3
       asl                            ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       sta    $AA                     ;3
       clc                            ;2
       adc    #$50                    ;2
       sta    $AB                     ;3
LF2B1:
;       nop                            ;2 ??
       lda    $9E                     ;3
       bne    LF2B9                   ;2
       jmp    LF34C                   ;3 nearly in range for a BEQ

LF2B9:
       lda    $D1                     ;3
       beq    LF2C0                   ;2
       jmp    LF34C                   ;3 nearly in range for a BNE

LF2C0:
       lda    $81                     ;3
       bit    SWCHB                   ;4 difficulty
       bvc    LF2C9                   ;2
       and    #$1F                    ;2
LF2C9:
       and    #$3F                    ;2
       bne    LF2DC                   ;2
       lda    $AD                     ;3
       cmp    #$50                    ;2
       bcs    LF2D8                   ;2
       lda    #$01                    ;2
;       jmp    LF2DA                   ;3 could use BIT.w
       .byte $2C                      ;4 skip 2 bytes
LF2D8:
       lda    #$02                    ;2
LF2DA:
       sta    $D7                     ;3
LF2DC:
       lda    $81                     ;3
       bit    SWCHB                   ;4
       bvc    LF2E5                   ;2 difficulty
       lda    #$00                    ;2
LF2E5:
       and    #$01                    ;2
       bne    LF325                   ;2
       lda    $D7                     ;3
       cmp    #$01                    ;2
       bne    LF30C                   ;2
       lda    $AD                     ;3
       cmp    #$A0                    ;2
       bcc    LF2F7                   ;2
       adc    #$5F                    ;2
LF2F7:
       clc                            ;2
       adc    #$01                    ;2
       sta    $AD                     ;3
       lda    $91                     ;3
       cmp    #$A0                    ;2
       bcc    LF304                   ;2
       adc    #$5F                    ;2
LF304:
       clc                            ;2
       adc    #$01                    ;2
       sta    $91                     ;3
       jmp    LF325                   ;3

LF30C:
;       nop                            ;2 ??
       dec    $AD                     ;5
       lda    $AD                     ;3
;       cmp    #$00                    ;2 superfluous
       bne    LF319                   ;2
       lda    #$A0                    ;2
       sta    $AD                     ;3
LF319:
       dec    $91                     ;5
       lda    $91                     ;3
;       cmp    #$00                    ;2 superfluous
       bne    LF325                   ;2
       lda    #$A0                    ;2
       sta    $91                     ;3
LF325:
       lda    $81                     ;3
       and    #$07                    ;2
       bne    LF34B                   ;2
       lda    $D8                     ;3
       bne    LF33A                   ;2
       lda    $AC                     ;3
       cmp    $83                     ;3
       bcs    LF33A                   ;2
       inc    $AC                     ;5
       jmp    LF33E                   ;3

LF33A:
       lda    #$01                    ;2
       sta    $D8                     ;3
LF33E:
       lda    $D8                     ;3
       beq    LF34B                   ;2
       bit    SWCHB                   ;4 difficulty
       bvc    LF349                   ;2
       dec    $AC                     ;5
LF349:
       dec    $AC                     ;5
LF34B:
;       nop                            ;2 ??
LF34C:
;       nop                            ;2 ??
       lda    $D6                     ;3
       cmp    #$05                    ;2
       bcs    LF398                   ;2
       lda    $9E                     ;3
       bne    LF37B                   ;2
;       lda    #$00                    ;2 superfluous!
       sta    $D8                     ;3
       lda    $D1                     ;3
       bne    LF37B                   ;2
       lda    $AC                     ;3
       sbc    #$02                    ;2
       sta    $AC                     ;3
       cmp    #$58                    ;2
       bcc    LF37B                   ;2
       lda    $AD                     ;3
       cmp    #$35                    ;2
       bcc    LF37B                   ;2
       cmp    #$62                    ;2
       bcs    LF37B                   ;2
       lda    $D6                     ;3
       bne    LF37B                   ;2
       lda    #$01                    ;2
       sta    $D6                     ;3
LF37B:
;       nop                            ;2 ??
       lda    $D6                     ;3
       cmp    #$01                    ;2
       bne    LF398                   ;2
       lda    $AC                     ;3
       cmp    #$30                    ;2
       bcs    LF398                   ;2
       lda    #$30                    ;2
       sta    $D6                     ;3
       lda    #$00                    ;2
       sta    $AE                     ;3
       dec    $C8                     ;5
       bne    LF398                   ;2
    IF PLUSROM
       jsr    SendPlusROMScore
    ELSE
       lda    #$FF                    ;2
       sta    $DB                     ;3
    ENDIF
LF398:
;       nop                            ;2 ??
       lda    $CF                     ;3
       beq    LF3BB                   ;2
       cmp    #$01                    ;2
       bne    LF3A8                   ;2
       lda    #$BD                    ;2
       sta    $A6                     ;3
;       jmp    LF3BB                   ;3 could use BNE
       bne    LF3BB                   ;2 always branch

LF3A8:
       and    #$02                    ;2
       beq    LF3B1                   ;2
       lda    #$AD                    ;2
;       jmp    LF3B3                   ;3 could use BIT.w
       .byte $2C                      ;4 skip 2 bytes
LF3B1:
       lda    #$B5                    ;2
LF3B3:
       sta    $A6                     ;3
       dec    $CF                     ;5
       lda    #$00                    ;2
       sta    $AE                     ;3
LF3BB:
;       nop                            ;2 ??
       lda    $D0                     ;3
       beq    LF3DA                   ;2
       cmp    #$01                    ;2
       bne    LF3CB                   ;2
       lda    #$BD                    ;2
       sta    $A8                     ;3
;       jmp    LF3DA                   ;3 could use BNE
       bne    LF3DA                   ;2 always branch

LF3CB:
       and    #$02                    ;2
       beq    LF3D4                   ;2
       lda    #$AD                    ;2
;       jmp    LF3D6                   ;3 could use BIT.w
       .byte $2C                      ;4 skip 2 bytes
LF3D4:
       lda    #$B5                    ;2
LF3D6:
       sta    $A8                     ;3
       dec    $D0                     ;5
LF3DA:
;       nop                            ;2 ??
       lda    $D1                     ;3
       beq    LF3FA                   ;2
       cmp    #$01                    ;2
       beq    LF3FA                   ;2
       lda    $81                     ;3
       and    #$03                    ;2
       beq    LF3EF                   ;2
       lda    $AB                     ;3
       eor    #$20                    ;2
       sta    $AB                     ;3
LF3EF:
       dec    $D1                     ;5
       lda    $81                     ;3
       asl                            ;2
       asl                            ;2
       asl                            ;2
       ora    #$04                    ;2
       sta    $D5                     ;3
LF3FA:
;       nop                            ;2 ??
       lda    $8B                     ;3
       bne    LF42B                   ;2
       inc    $8A                     ;5
       lda    $8A                     ;3
;       cmp    #$07                    ;2 could use EOR...
       eor    #$07                    ;2

       bne    LF42B                   ;2
;       lda    #$00                    ;2 ...to eliminate this
       sta    $8A                     ;3
       lda    $CF                     ;3
       bne    LF41A                   ;2
       lda    $A6                     ;3
       cmp    #$28                    ;2
       bcs    LF41A                   ;2
       clc                            ;2
       adc    #$08                    ;2
       sta    $A6                     ;3
LF41A:
;       nop                            ;2 ??
       lda    $D0                     ;3
       bne    LF42A                   ;2
       lda    $A8                     ;3
       cmp    #$58                    ;2
       bcs    LF42A                   ;2
       clc                            ;2
       adc    #$08                    ;2
       sta    $A8                     ;3
LF42A:
;       nop                            ;2 ??
LF42B:
;       nop                            ;2 ??
       lda    $81                     ;3
       adc    $CC                     ;3
       rol                            ;2
       rol                            ;2
       and    #$7F                    ;2
       adc    #$17                    ;2
       sta    $CC                     ;3
       ror                            ;2
       eor    #$55                    ;2
       sta    $CD                     ;3
       lda    #$C0                    ;2
       sta    $CA                     ;3
       lda    $81                     ;3
       and    #$03                    ;2
       bne    LF44D                   ;2
       lda    $AE                     ;3
       cmp    #$05                    ;2
       bcs    LF46B                   ;2
LF44D:
       ldx    $9E                     ;3
       lda    $AB                     ;3
       ldy    $D1                     ;3
       bne    LF458                   ;2
       eor    LFFF7,X                 ;4 NOTE: relocate table for SC-compatability
LF458:
       sta    $AB                     ;3
       sta    $A2                     ;3
       lda    $AA                     ;3
       sta    $A4                     ;3
       lda    $AC                     ;3
       sta    $97                     ;3
       lda    $AD                     ;3
;       sta    $90                     ;3
;       jmp    LF47B                   ;3 could go to $F479 to eliminate above STA
       jmp    LF479                   ;3

LF46B:
       lda    $CA                     ;3
       sta    $A2                     ;3
       lda    #$D0                    ;2
       sta    $A4                     ;3
       lda    $CD                     ;3
       sta    $97                     ;3
       lda    $CC                     ;3
LF479:
       sta    $90                     ;3
LF47B:
;       nop                            ;2 ??
       lda    $84                     ;3
       clc                            ;2
       adc    #$08                    ;2
       cmp    $98                     ;3
       bcs    LF489                   ;2
       lda    #$BD                    ;2
       sta    $A6                     ;3
LF489:
       lda    $84                     ;3
       clc                            ;2
       adc    #$08                    ;2
       cmp    $99                     ;3
       bcs    LF496                   ;2
       lda    #$BD                    ;2
       sta    $A8                     ;3
LF496:
       lda    $A6                     ;3
       cmp    #$BD                    ;2
       bne    LF4B8                   ;2
       lda    $A8                     ;3
       cmp    #$BD                    ;2
       bne    LF4B8                   ;2
       lda    $95                     ;3
       adc    #$40                    ;2
       cmp    #$A0                    ;2
       bcc    LF4AC                   ;2
       adc    #$60                    ;2
LF4AC:
       sta    $94                     ;3
       lda    #$00                    ;2
       sta    $CF                     ;3
       sta    $D0                     ;3
       sta    $A6                     ;3
       sta    $98                     ;3
LF4B8:
       lda    $A8                     ;3
       cmp    #$BD                    ;2
       bne    LF4CC                   ;2
       lda    $98                     ;3
;       cmp    #$18                    ;2 could EOR...
       eor    #$18                    ;2

       bne    LF4CC                   ;2
;       lda    #$00                    ;2 ...to eliminate this line
       sta    $99                     ;3
       lda    #$30                    ;2
       sta    $A8                     ;3
LF4CC:
;       nop                            ;2 ??
       lda    $D6                     ;3
       cmp    #$05                    ;2
       bcs    LF52D                   ;2
       lda    $AC                     ;3
       cmp    #$B0                    ;2
       bcs    LF4E1                   ;2
       lda    $D1                     ;3
       cmp    #$01                    ;2
       bne    LF52D                   ;2
       dec    $AE                     ;5
LF4E1:
       lda    #$00                    ;2
       sta    $D1                     ;3
       sta    $D3                     ;3
       sta    $D5                     ;3
       sta    $D8                     ;3
       sta    $D6                     ;3
       inc    $AE                     ;5
       lda    #$90                    ;2
       sta    $AB                     ;3
       lda    #$40                    ;2
       sta    $AA                     ;3
       lda    $D4                     ;3
       clc                            ;2
       adc    #$30                    ;2
       ora    #$03                    ;2
       sta    $D4                     ;3
       lda    #$04                    ;2
       sta    $9E                     ;3
       lda    $83                     ;3
       sbc    #$20                    ;2
       sta    $AC                     ;3
       lda    $81                     ;3
       eor    $95                     ;3
       and    #$1F                    ;2
       adc    $AC                     ;3
       sta    $AC                     ;3
       lda    $81                     ;3
       and    #$1F                    ;2
       adc    #$80                    ;2
       cmp    #$A1                    ;2
       bcc    LF520                   ;2
       adc    #$5F                    ;2
LF520:
       sta    $91                     ;3
       clc                            ;2
       adc    #$0D                    ;2
       cmp    #$A1                    ;2
       bcc    LF52B                   ;2
       adc    #$5F                    ;2
LF52B:
       sta    $AD                     ;3
LF52D:
;       nop                            ;2 ??
       ldx    #$03                    ;2
LF530:
       sta    WSYNC                   ;3
;       nop                            ;2 could remove this...
    IF PLUSROM
       nop                            ;2
       nop                            ;2
       nop                            ;2
    ELSE
       lda    ($80,X)                 ;6
    ENDIF
       lda    $90,X                   ;4
       sec                            ;2
LF538: ;could possibly reuse subroutine portion LF848!
       sbc    #$0F                    ;2
       bcs    LF538                   ;2

       eor    #$FF                    ;2 moved

       sta    RESP0,X                 ;4
       sta    WSYNC                   ;3
;       eor    #$FF                    ;2 ...by moving this line above the RESP0
       sbc    #$06                    ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       sta    HMP0,X                  ;4
       dex                            ;2
       bpl    LF530                   ;2
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
;;;       sta    WSYNC                   ;3
;;;       sta    HMCLR                   ;3
LF555:
       lda    TIMINT                  ;4
       bpl    LF555                   ;2

       sta    HMCLR                   ;3

       lda    $97                     ;3
       sta    $9B                     ;3
       lda    $98                     ;3
       sta    $9C                     ;3
       lda    $99                     ;3
       sta    $9D                     ;3
       lda    $88                     ;3
       sta    $87                     ;3
       lda    $96                     ;3
       sta    $9A                     ;3
       sta    WSYNC                   ;3
       lda    $D6                     ;3
       cmp    #$05                    ;2
       bcc    LF58C                   ;2
       jsr    LF773                   ;6
       lda    #$62                    ;2
       sta    $83                     ;3
       lda    #$1F                    ;2
       sta    $84                     ;3
       lda    #$00                    ;2
       sta    $9E                     ;3
;       lda    #$00                    ;2 superfluous!
       sta    $AC                     ;3
;       jmp    LF5BD                   ;3 could use BEQ
       beq    LF5BD                   ;2 always branch

LF58C:
  IF PAL60
       lda    #$BA                    ;2
  ELSE
       lda    #$9A                    ;2
  ENDIF
       sta    COLUBK                  ;3 sky
       lda    $D5                     ;3
       sta    COLUP0                  ;3
       lda    $D4                     ;3
       sta    COLUP1                  ;3
       sta    HMCLR                   ;3
       sta    WSYNC                   ;3
;       lda    #$00                    ;2
;       sta    GRP0                    ;3
;       sta    GRP1                    ;3
;;       lda    #$FF                    ;2 could swap order to preserve #$00 value...
;;       sta    VDELP0                  ;3
;;       lda    #$00                    ;2
;       sta    VDELP1                  ;3 ...to be used here

       ldx    #$00                    ;2
       stx    GRP0                    ;3
       stx    GRP1                    ;3
       stx    VDELP1                  ;3
       dex                            ;2
       stx    VDELP0                  ;3


       lda    $D3                     ;3
       sta    NUSIZ0                  ;3
       lda    #$07                    ;2
       sta    NUSIZ1                  ;3
       ldx    $83                     ;3
       sta    CXCLR                   ;3
       lda    #$00                    ;2
       sta    $D2                     ;3
       jsr    LFD00                   ;6
LF5BD:
       sta    WSYNC                   ;3
       lda    $CE                     ;3
       cmp    #$10                    ;2
       bcs    LF5DE                   ;2
       lda    $81                     ;3
       and    #$08                    ;2
       bne    LF5DE                   ;2
  IF PAL60
       lda    #$65                    ;2
  ELSE
       lda    #$45                    ;2
  ENDIF
       sta    COLUBK                  ;3
       lda    #$FF                    ;2
       sta    AUDV0                   ;3
       lda    #$08                    ;2
       sta    AUDF0                   ;3
       lda    #$05                    ;2
       sta    AUDC0                   ;3
;       jmp    LF5EA                   ;3 could use BNE
       bne    LF5EA                   ;2 always branch

LF5DE:
       lda    #$00                    ;2
       sta    COLUBK                  ;3
       lda    #$03                    ;2
       sta    AUDV0                   ;3
;       lda    #$03                    ;2 superfluous!
       sta    AUDC0                   ;3
LF5EA:
       jsr    LF796                   ;6
;;;       sta    WSYNC                   ;3
       sta    WSYNC                   ;3
       lda    #$00                    ;2
       sta    COLUBK                  ;3
;       lda    #$22                    ;2
       lda    #$23                    ;2
       sta    $029E                   ;4
       lda    $96                     ;3
       bmi    LF601                   ;2
       jmp    LF689                   ;3 nearly in range for a BPL

LF601:
       lda    $84                     ;3
       cmp    #$25                    ;2
       bcs    LF640                   ;2
       lda    $AC                     ;3
       cmp    #$69                    ;2
       bcs    LF615                   ;2
       cmp    #$5B                    ;2
       bcc    LF615                   ;2
       bit    $D2                     ;3
       bvs    LF618                   ;2
LF615:
       jmp    LF689                   ;3

LF618:
;       nop                            ;2 ??
       lda    $D1                     ;3
       bne    LF640                   ;2
       lda    #$20                    ;2
       inc    $DA                     ;5
       sta    $D1                     ;3
       lda    #$05                    ;2
       sta    $D3                     ;3
       lda    #$32                    ;2
       sta    $C9                     ;3
       lda    #$00                    ;2
       sta    $D6                     ;3
       lda    $AD                     ;3
       sbc    #$05                    ;2
       sta    $AD                     ;3
       lda    #$C0                    ;2
       sta    $AB                     ;3
       lda    #$D0                    ;2
       sta    $AA                     ;3
;       jmp    LF689                   ;3 could use BNE
       bne    LF689                   ;2 always branch

LF640:
;       nop                            ;2 ??
       lda    $94                     ;3
       cmp    #$49                    ;2
       bcc    LF689                   ;2
       cmp    #$51                    ;2
       bcs    LF689                   ;2
       bit    CXM0P                   ;3
       bvc    LF689                   ;2
       lda    $CF                     ;3
       bne    LF66C                   ;2
       lda    $84                     ;3
       sec                            ;2
       sbc    $98                     ;3
       cmp    #$1F                    ;2
       bcc    LF66C                   ;2
       cmp    #$28                    ;2
       bcs    LF66C                   ;2
       lda    #$10                    ;2
       sta    $CF                     ;3
       lda    #$0A                    ;2
       sta    $C9                     ;3
       lda    #$0F                    ;2
       sta    $D9                     ;3
LF66C:
       lda    $D0                     ;3
       bne    LF689                   ;2
       lda    $84                     ;3
       sec                            ;2
       sbc    $99                     ;3
       cmp    #$1F                    ;2
       bcc    LF689                   ;2
       cmp    #$28                    ;2
       bcs    LF689                   ;2
       lda    #$10                    ;2
       sta    $D0                     ;3
       lda    #$0A                    ;2
       sta    $C9                     ;3
       lda    #$0F                    ;2
       sta    $D9                     ;3
LF689:
;       nop                            ;2 ??
       lda    $DA                     ;3
       cmp    #$0A                    ;2
       bcc    LF69E                   ;2
       lda    $89                     ;3
       cmp    #$03                    ;2
       bcc    LF69E                   ;2
       dec    $89                     ;5
       lda    #$00                    ;2
       sta    $DA                     ;3
       sta    $8B                     ;3
LF69E:
;       nop                            ;2 ??
       ldy    #$FE                    ;2
       ldx    #$FF                    ;2
LF6A3:
       iny                            ;2
       iny                            ;2
       inx                            ;2
       cpx    $C8                     ;3
       bcs    LF6B2                   ;2
       lda    #$DA                    ;2
;       sta.wy $BC,Y                   ;5
;       jmp    LF6B7                   ;3 could use BIT.w to eliminate the above line!
       .byte $2C                      ;4 skip 2 bytes
LF6B2:
       lda    #$E1                    ;2
       sta.wy $BC,Y                   ;5
LF6B7:
       cpx    #$05                    ;2
       bne    LF6A3                   ;2
       ldx    #$08                    ;2
       lda    $C9                     ;3
       beq    LF6DB                   ;2
       dec    $C9                     ;5
LF6C3:
       lda    $B0,X                   ;4
       clc                            ;2
       adc    #$07                    ;2
       sta    $B0,X                   ;4
       cmp    #$A6                    ;2
       bne    LF6DB                   ;2
       lda    #$60                    ;2
       sta    $B0,X                   ;4
       dex                            ;2
       dex                            ;2
       cpx    #$FE                    ;2
;       beq    LF6DB                   ;2
;       jmp    LF6C3                   ;3 in range for a BNE!
       bne    LF6C3                   ;2
LF6DB:
;       nop                            ;2 ??
       sta    HMCLR                   ;3
LF6DE:
       lda    TIMINT                  ;4
       bpl    LF6DE                   ;2
       jmp    LF01B                   ;3

LF6E6:
;       nop                            ;2 ??
       lda    #$FF                    ;2
LF6E9: ;added
       sta    $DB                     ;3
       lda    #$40                    ;2
       sta    $AC                     ;3
       sta    $98                     ;3
       lda    #$26                    ;2
       sta    $8E                     ;3
       lda    #$36                    ;2
       sta    $8F                     ;3
       lda    $DC                     ;3
       clc                            ;2
       adc    #$01                    ;2
       sta    $89                     ;3
       lda    #$30                    ;2
       sta    PF0                     ;3
       sta    AUDF0                   ;3

       sta    $A8                     ;3 moved

       lda    #$05                    ;2
       sta    CTRLPF                  ;3
       lda    #>Digit_0               ;2
       sta    $A7                     ;3
       sta    $A9                     ;3
       sta    $B1                     ;3
       sta    $B3                     ;3
       sta    $B5                     ;3
       sta    $B7                     ;3
       sta    $B9                     ;3
       sta    $BB                     ;3
       lda    #>Reserve_Plane         ;2
       sta    $A1                     ;3
       sta    $BD                     ;3
       sta    $BF                     ;3
       sta    $C1                     ;3
       sta    $C3                     ;3
       sta    $C5                     ;3
       sta    $C7                     ;3
;       lda    #$30                    ;2
;       sta    $A8                     ;3 could move up to reuse PF0 value!
       lda    #$FA                    ;2
       sta    $A3                     ;3
       sta    $A5                     ;3
       lda    #$50                    ;2
       sta    $92                     ;3
       sta    $93                     ;3
       lda    #$62                    ;2
       sta    $83                     ;3
       lda    #$1F                    ;2
       sta    $84                     ;3
       lda    #$00                    ;2
       sta    $8B                     ;3
       sta    $AE                     ;3
       sta    $AF                     ;3
       lda    #$60                    ;2
       sta    $BA                     ;3
LF750:
       lda    #<Digit_0               ;2
       sta    $B0                     ;3
       sta    $B2                     ;3
       sta    $B4                     ;3
       sta    $B6                     ;3
       sta    $B8                     ;3
       lda    #<Reserve_Plane         ;2
       sta    $BC                     ;3
       sta    $BE                     ;3
       sta    $C0                     ;3
       sta    $C2                     ;3
       sta    $C4                     ;3
       sta    $C6                     ;3
       lda    #$06                    ;2
       sta    $C8                     ;3
       lda    #$30                    ;2
       sta    $CE                     ;3
       rts                            ;6

LF773:
       lda    $81                     ;3
       and    #$03                    ;2
       beq    LF77E                   ;2
       lda    #$00                    ;2
;       jmp    LF780                   ;3 could use BIT.w
       .byte $2C                      ;4 skip 2 bytes
LF77E:
  IF PAL60
       lda    #$4C                    ;2
  ELSE
       lda    #$3C                    ;2
  ENDIF
LF780:
       sta    COLUBK                  ;3
       ldx    #$A9                    ;2
LF784:
       stx    WSYNC                   ;3
       dex                            ;2
       bne    LF784                   ;2
       dec    $D6                     ;5
;;       cmp    #$05                    ;2
;;       bne    LF78F                   ;2 ???
LF78F:
       sta    WSYNC                   ;3
       lda    #$00                    ;2
       sta    COLUBK                  ;3
       rts                            ;6

LF796:
       lda    $8E                     ;3
       eor    #$1A                    ;2
       sta    $8E                     ;3
       lda    $8F                     ;3
       eor    #$72                    ;2
       sta    $8F                     ;3
       jsr    LF83E                   ;6
       lda    #$FF                    ;2
       sta    VDELP0                  ;3
       sta    VDELP1                  ;3
       ldy    #$06                    ;2
       sty    $8D                     ;3
       lda    #$0F                    ;2
       sta    COLUP0                  ;3
       sta    COLUP1                  ;3
       lda    #$03                    ;2
       sta    NUSIZ0                  ;3
       sta    NUSIZ1                  ;3
LF7BB:
       lda    ($B8),Y                 ;5
       sta    WSYNC                   ;3
       sta    $8C                     ;3
       lda    ($BA),Y                 ;5
       tax                            ;2
       lda    ($B0),Y                 ;5
       sta    GRP0                    ;3
       lda    ($B2),Y                 ;5
       sta    GRP1                    ;3
       lda    ($B4),Y                 ;5
       sta    GRP0                    ;3
       lda    ($B6),Y                 ;5
       ldy    $8C                     ;3
       sta    GRP1                    ;3
       sty    GRP0                    ;3
       stx    GRP1                    ;3
       sta    GRP0                    ;3
       dec    $8D                     ;5
       ldy    $8D                     ;3
       bpl    LF7BB                   ;2
;       lda    #$00                    ;2 could INY and use that instead!
;       sta    GRP0                    ;3
;       sta    GRP1                    ;3
;       sta    GRP0                    ;3
;       sta    GRP1                    ;3
       iny                            ;2
       sty    GRP0                    ;3
       sty    GRP1                    ;3
       sty    GRP0                    ;3
       sty    GRP1                    ;3

       lda    $8E                     ;3
       eor    #$1A                    ;2
       sta    $8E                     ;3
       lda    $8F                     ;3
       eor    #$72                    ;2
       sta    $8F                     ;3
       jsr    LF83E                   ;6
;;;       ldy    #$06                    ;2
;;;       sty    $8D                     ;3
  IF PAL60
       lda    #$8A                    ;2
  ELSE
       lda    #$5A                    ;2
  ENDIF
       sta    COLUP0                  ;3
       sta    COLUP1                  ;3
;;;       lda    #$06                    ;2
;;;       sta    NUSIZ0                  ;3 could use Y instead!
;;;       sta    NUSIZ1                  ;3
       ldy    #$06                    ;2
       sty    NUSIZ0                  ;3
       sty    NUSIZ1                  ;3
       dey                            ;2
       sty    $8D                     ;3
LF80B:
       lda    ($C4),Y                 ;5
       sta    $8C                     ;3
       sta    WSYNC                   ;3
       lda    ($C6),Y                 ;5
       tax                            ;2
       lda    ($BC),Y                 ;5
       sta    GRP0                    ;3
       lda    ($BE),Y                 ;5
       sta    GRP1                    ;3
       lda    ($C0),Y                 ;5
       sta    GRP0                    ;3
       lda    ($C2),Y                 ;5
       ldy    $8C                     ;3
       sta    GRP1                    ;3
       sty    GRP0                    ;3
;       stx    GRP1 + $0100            ;4
       stx.w  GRP1                    ;4
       sta    GRP0                    ;3
       dec    $8D                     ;5
       ldy    $8D                     ;3
       bpl    LF80B                   ;2
;       lda    #$00                    ;2 could INY and use that instead!
;       sta    GRP0                    ;3
;       sta    GRP1                    ;3
;       sta    GRP0                    ;3
;       sta    GRP1                    ;3
       iny                            ;2
       sty    GRP0                    ;3
       sty    GRP1                    ;3
       sty    GRP0                    ;3
       sty    GRP1                    ;3

       rts                            ;6

LF83E:
       ldx    #$01                    ;2
LF840:
       sta    WSYNC                   ;3
;       nop                            ;2 could eliminate this...
    IF PLUSROM
       nop                            ;2
       nop                            ;2
       nop                            ;2
    ELSE
       lda    ($80,X)                 ;6
    ENDIF
       lda    $8E,X                   ;4
       sec                            ;2
LF848:
       sbc    #$0F                    ;2
       bcs    LF848                   ;2

       eor    #$FF                    ;2 moved

       sta    RESP0,X                 ;4
       sta    WSYNC                   ;3
;       eor    #$FF                    ;2 by moving this above the RESP0
       sbc    #$06                    ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       sta    HMP0,X                  ;4
       dex                            ;2
       bpl    LF840                   ;2
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       rts                            ;6

LF862:
       lda    $95                     ;3
;       cmp    #$00                    ;2 superfluous!
       bne    LF86C                   ;2
       lda    #$A0                    ;2
       sta    $95                     ;3
LF86C:
       dec    $95                     ;5
       rts                            ;6

LF86F:
       lda    $95                     ;3
;       cmp    #$A0                    ;2 could EOR...
       eor    #$A0                    ;2

       bne    LF879                   ;2
;       lda    #$00                    ;2 ...to eliminate this
       sta    $95                     ;3
LF879:
       inc    $95                     ;5
;       nop                            ;2 ??
       rts                            ;6

;       ORG $F840
;56 bytes free space


    IF PLUSROM
SendPlusROMScore
       lda    #$FF                    ;2
       sta    $DB                     ;3
       lda $B0                       ; Score digit 6 
       sta WriteToBuffer             ; 
       lda $B2                       ; Score digit 5
       sta WriteToBuffer             ; 
       lda $B4                       ; Score digit 4
       sta WriteToBuffer             ; 
       lda $B6                       ; Score digit 3
       sta WriteToBuffer             ; 
       lda $B8                       ; Score digit 2
       sta WriteToBuffer             ; 
       lda $BA                       ; Score digit 1
       sta WriteToBuffer             ; 
       lda #HIGHSCORE_ID             ; game id in Highscore DB
       sta WriteSendBuffer
       rts
       
    ENDIF

    IF SUPERCHARGER
       ORG $F878,0

LFFF7: ;moved
       .byte $F0 ; |XXXX    | $FFF7 5 bytes
       .byte $D0 ; |XX X    | $FFF8
       .byte $00 ; |        | $FFF9
       .byte $00 ; |        | $FFFA
       .byte $00 ; |        | $FFFB
    ENDIF

       ORG $F87D,0
       .byte $2C ; |  X XX  | $F87D
       .byte $0E ; |    XXX | $F87E
       .byte $F2 ; |XXXX  X | $F87F
       .byte $3F ; |  XXXXXX| $F880
       .byte $A0 ; |X X     | $F881
       .byte $26 ; |  X  XX | $F882
       .byte $32 ; |  XX  X | $F883
       .byte $A3 ; |X X   XX| $F884
       .byte $87 ; |X    XXX| $F885
       .byte $17 ; |   X XXX| $F886
       .byte $A4 ; |X X  X  | $F887
       .byte $BA ; |X XXX X | $F888
       .byte $A0 ; |X X     | $F889
       .byte $A1 ; |X X    X| $F88A
       .byte $E0 ; |XXX     | $F88B
       .byte $C6 ; |XX   XX | $F88C
       .byte $A4 ; |X X  X  | $F88D
       .byte $96 ; |X  X XX | $F88E
       .byte $C6 ; |XX   XX | $F88F
       .byte $5F ; | X XXXXX| $F890
       .byte $A9 ; |X X X  X| $F891
       .byte $27 ; |  X  XXX| $F892
       .byte $BA ; |X XXX X | $F893
       .byte $AE ; |X X XXX | $F894
       .byte $E0 ; |XXX     | $F895
       .byte $A7 ; |X X  XXX| $F896
       .byte $BE ; |X XXXXX | $F897
       .byte $36 ; |  XX XX | $F898
       .byte $C0 ; |XX      | $F899
       .byte $9D ; |X  XXX X| $F89A
       .byte $B0 ; |X XX    | $F89B
LF89C:
       .byte $08 ; |    X   | $F89C
       .byte $08 ; |    X   | $F89D
       .byte $08 ; |    X   | $F89E
       .byte $08 ; |    X   | $F89F
       .byte $08 ; |    X   | $F8A0
       .byte $08 ; |    X   | $F8A1
       .byte $18 ; |   XX   | $F8A2
       .byte $18 ; |   XX   | $F8A3
       .byte $18 ; |   XX   | $F8A4
       .byte $18 ; |   XX   | $F8A5
       .byte $18 ; |   XX   | $F8A6
       .byte $18 ; |   XX   | $F8A7
       .byte $1C ; |   XXX  | $F8A8
       .byte $1C ; |   XXX  | $F8A9
       .byte $1C ; |   XXX  | $F8AA
       .byte $1C ; |   XXX  | $F8AB
       .byte $1C ; |   XXX  | $F8AC
       .byte $1C ; |   XXX  | $F8AD
       .byte $3C ; |  XXXX  | $F8AE
       .byte $3C ; |  XXXX  | $F8AF
       .byte $3C ; |  XXXX  | $F8B0
       .byte $3C ; |  XXXX  | $F8B1
       .byte $3C ; |  XXXX  | $F8B2
       .byte $3C ; |  XXXX  | $F8B3
       .byte $3E ; |  XXXXX | $F8B4
       .byte $3E ; |  XXXXX | $F8B5
       .byte $3E ; |  XXXXX | $F8B6
       .byte $3E ; |  XXXXX | $F8B7
       .byte $3E ; |  XXXXX | $F8B8
       .byte $3E ; |  XXXXX | $F8B9
       .byte $7E ; | XXXXXX | $F8BA
       .byte $7E ; | XXXXXX | $F8BB
       .byte $7E ; | XXXXXX | $F8BC
       .byte $7E ; | XXXXXX | $F8BD
       .byte $7E ; | XXXXXX | $F8BE
       .byte $7E ; | XXXXXX | $F8BF
       .byte $7F ; | XXXXXXX| $F8C0
       .byte $7F ; | XXXXXXX| $F8C1
       .byte $7F ; | XXXXXXX| $F8C2
       .byte $7F ; | XXXXXXX| $F8C3
       .byte $7F ; | XXXXXXX| $F8C4
       .byte $7F ; | XXXXXXX| $F8C5
       .byte $FF ; |XXXXXXXX| $F8C6
       .byte $FF ; |XXXXXXXX| $F8C7
       .byte $FF ; |XXXXXXXX| $F8C8
       .byte $FF ; |XXXXXXXX| $F8C9
       .byte $FF ; |XXXXXXXX| $F8CA
       .byte $FF ; |XXXXXXXX| $F8CB
       .byte $FF ; |XXXXXXXX| $F8CC
       .byte $FF ; |XXXXXXXX| $F8CD
       .byte $FF ; |XXXXXXXX| $F8CE
       .byte $FF ; |XXXXXXXX| $F8CF
       .byte $FF ; |XXXXXXXX| $F8D0
       .byte $FF ; |XXXXXXXX| $F8D1
       .byte $FF ; |XXXXXXXX| $F8D2
       .byte $FF ; |XXXXXXXX| $F8D3
       .byte $FF ; |XXXXXXXX| $F8D4
       .byte $FF ; |XXXXXXXX| $F8D5
       .byte $FF ; |XXXXXXXX| $F8D6
       .byte $FF ; |XXXXXXXX| $F8D7
       .byte $FF ; |XXXXXXXX| $F8D8
       .byte $FF ; |XXXXXXXX| $F8D9
       .byte $FF ; |XXXXXXXX| $F8DA
       .byte $FF ; |XXXXXXXX| $F8DB
       .byte $FF ; |XXXXXXXX| $F8DC
       .byte $FF ; |XXXXXXXX| $F8DD
       .byte $FF ; |XXXXXXXX| $F8DE
       .byte $FF ; |XXXXXXXX| $F8DF
       .byte $FF ; |XXXXXXXX| $F8E0
       .byte $FF ; |XXXXXXXX| $F8E1
       .byte $FF ; |XXXXXXXX| $F8E2
       .byte $FF ; |XXXXXXXX| $F8E3
       .byte $FF ; |XXXXXXXX| $F8E4
       .byte $FF ; |XXXXXXXX| $F8E5
       .byte $FF ; |XXXXXXXX| $F8E6
       .byte $FF ; |XXXXXXXX| $F8E7
       .byte $FF ; |XXXXXXXX| $F8E8
       .byte $FF ; |XXXXXXXX| $F8E9
       .byte $FF ; |XXXXXXXX| $F8EA
       .byte $FF ; |XXXXXXXX| $F8EB
       .byte $FF ; |XXXXXXXX| $F8EC
LF8ED:
       .byte $00 ; |        | $F8ED
       .byte $0D ; |    XX X| $F8EE
       .byte $1A ; |   XX X | $F8EF
       .byte $7A ; | XXXX X | $F8F0
LF8F1:
       .byte $0D ; |    XX X| $F8F1
       .byte $0D ; |    XX X| $F8F2
       .byte $60 ; | XX     | $F8F3
       .byte $60 ; | XX     | $F8F4
LF8F5:
       .byte $60 ; | XX     | $F8F5 NOTE: same 2 values
       .byte $60 ; | XX     | $F8F6
       .byte $B9 ; |X XXX  X| $F8F7
       .byte $B9 ; |X XXX  X| $F8F8
LF8F9:
       .byte $00 ; |        | $F8F9
       .byte <Digit_6 ; $8A ; |X   X X | $F8FA
       .byte <Digit_5 ; $83 ; |X     XX| $F8FB
       .byte <Digit_4 ; $7C ; | XXXXX  | $F8FC
       .byte <Digit_3 ; $75 ; | XXX X X| $F8FD
       .byte <Digit_2 ; $6E ; | XX XXX | $F8FE
       .byte <Digit_1 ; $67 ; | XX  XXX| $F8FF
       .byte $00 ; |        | $F900
       .byte $12 ; |   X  X | $F901
       .byte $12 ; |   X  X | $F902
       .byte $12 ; |   X  X | $F903
       .byte $12 ; |   X  X | $F904
       .byte $12 ; |   X  X | $F905
       .byte $12 ; |   X  X | $F906
       .byte $02 ; |      X | $F907
       .byte $F2 ; |XXXX  X | $F908
       .byte $F2 ; |XXXX  X | $F909
       .byte $F2 ; |XXXX  X | $F90A
       .byte $F2 ; |XXXX  X | $F90B
       .byte $F2 ; |XXXX  X | $F90C
       .byte $00 ; |        | $F90D
       .byte $F2 ; |XXXX  X | $F90E
       .byte $F2 ; |XXXX  X | $F90F
       .byte $F2 ; |XXXX  X | $F910
       .byte $F2 ; |XXXX  X | $F911
       .byte $F2 ; |XXXX  X | $F912
       .byte $F2 ; |XXXX  X | $F913
       .byte $02 ; |      X | $F914
       .byte $12 ; |   X  X | $F915
       .byte $12 ; |   X  X | $F916
       .byte $12 ; |   X  X | $F917
       .byte $12 ; |   X  X | $F918
       .byte $12 ; |   X  X | $F919
       .byte $00 ; |        | $F91A
       .byte $00 ; |        | $F91B
       .byte $00 ; |        | $F91C
       .byte $00 ; |        | $F91D
       .byte $00 ; |        | $F91E
       .byte $00 ; |        | $F91F
       .byte $22 ; |  X   X | $F920
       .byte $02 ; |      X | $F921
       .byte $00 ; |        | $F922
       .byte $00 ; |        | $F923
       .byte $00 ; |        | $F924
       .byte $00 ; |        | $F925
       .byte $00 ; |        | $F926
       .byte $00 ; |        | $F927
       .byte $22 ; |  X   X | $F928
       .byte $02 ; |      X | $F929
       .byte $00 ; |        | $F92A
       .byte $00 ; |        | $F92B
       .byte $00 ; |        | $F92C
       .byte $00 ; |        | $F92D
       .byte $00 ; |        | $F92E
       .byte $00 ; |        | $F92F
       .byte $22 ; |  X   X | $F930
       .byte $02 ; |      X | $F931
       .byte $00 ; |        | $F932
       .byte $00 ; |        | $F933
       .byte $00 ; |        | $F934
       .byte $00 ; |        | $F935
       .byte $00 ; |        | $F936
       .byte $00 ; |        | $F937
       .byte $22 ; |  X   X | $F938
       .byte $02 ; |      X | $F939
       .byte $00 ; |        | $F93A
       .byte $00 ; |        | $F93B
       .byte $00 ; |        | $F93C
       .byte $00 ; |        | $F93D
       .byte $00 ; |        | $F93E
       .byte $00 ; |        | $F93F
       .byte $22 ; |  X   X | $F940
       .byte $02 ; |      X | $F941
       .byte $00 ; |        | $F942
       .byte $00 ; |        | $F943
       .byte $00 ; |        | $F944
       .byte $00 ; |        | $F945
       .byte $00 ; |        | $F946
       .byte $00 ; |        | $F947
       .byte $22 ; |  X   X | $F948
       .byte $02 ; |      X | $F949
       .byte $00 ; |        | $F94A
       .byte $00 ; |        | $F94B
       .byte $00 ; |        | $F94C
       .byte $00 ; |        | $F94D
       .byte $00 ; |        | $F94E
       .byte $00 ; |        | $F94F
       .byte $22 ; |  X   X | $F950
       .byte $02 ; |      X | $F951
       .byte $00 ; |        | $F952
       .byte $00 ; |        | $F953
       .byte $00 ; |        | $F954
       .byte $00 ; |        | $F955
       .byte $00 ; |        | $F956
       .byte $00 ; |        | $F957
       .byte $22 ; |  X   X | $F958
       .byte $02 ; |      X | $F959
       .byte $00 ; |        | $F95A
       .byte $00 ; |        | $F95B
       .byte $00 ; |        | $F95C
       .byte $00 ; |        | $F95D
       .byte $00 ; |        | $F95E
       .byte $00 ; |        | $F95F
       .byte $22 ; |  X   X | $F960
       .byte $02 ; |      X | $F961
       .byte $00 ; |        | $F962
       .byte $00 ; |        | $F963
       .byte $00 ; |        | $F964
       .byte $00 ; |        | $F965
       .byte $00 ; |        | $F966
       .byte $00 ; |        | $F967
       .byte $22 ; |  X   X | $F968
       .byte $02 ; |      X | $F969
       .byte $00 ; |        | $F96A
       .byte $00 ; |        | $F96B
       .byte $00 ; |        | $F96C
       .byte $00 ; |        | $F96D
       .byte $00 ; |        | $F96E
       .byte $00 ; |        | $F96F
       .byte $22 ; |  X   X | $F970
       .byte $02 ; |      X | $F971
       .byte $00 ; |        | $F972
       .byte $00 ; |        | $F973
       .byte $00 ; |        | $F974
       .byte $00 ; |        | $F975
       .byte $00 ; |        | $F976
       .byte $00 ; |        | $F977
       .byte $22 ; |  X   X | $F978
       .byte $02 ; |      X | $F979
       .byte $00 ; |        | $F97A
       .byte $00 ; |        | $F97B
       .byte $00 ; |        | $F97C
       .byte $00 ; |        | $F97D
       .byte $00 ; |        | $F97E
       .byte $00 ; |        | $F97F
       .byte $E2 ; |XXX   X | $F980
       .byte $02 ; |      X | $F981
       .byte $00 ; |        | $F982
       .byte $00 ; |        | $F983
       .byte $00 ; |        | $F984
       .byte $00 ; |        | $F985
       .byte $00 ; |        | $F986
       .byte $00 ; |        | $F987
       .byte $E2 ; |XXX   X | $F988
       .byte $02 ; |      X | $F989
       .byte $00 ; |        | $F98A
       .byte $00 ; |        | $F98B
       .byte $00 ; |        | $F98C
       .byte $00 ; |        | $F98D
       .byte $00 ; |        | $F98E
       .byte $00 ; |        | $F98F
       .byte $E2 ; |XXX   X | $F990
       .byte $02 ; |      X | $F991
       .byte $00 ; |        | $F992
       .byte $00 ; |        | $F993
       .byte $00 ; |        | $F994
       .byte $00 ; |        | $F995
       .byte $00 ; |        | $F996
       .byte $00 ; |        | $F997
       .byte $E2 ; |XXX   X | $F998
       .byte $02 ; |      X | $F999
       .byte $00 ; |        | $F99A
       .byte $00 ; |        | $F99B
       .byte $00 ; |        | $F99C
       .byte $00 ; |        | $F99D
       .byte $00 ; |        | $F99E
       .byte $00 ; |        | $F99F
       .byte $E2 ; |XXX   X | $F9A0
       .byte $02 ; |      X | $F9A1
       .byte $00 ; |        | $F9A2
       .byte $00 ; |        | $F9A3
       .byte $00 ; |        | $F9A4
       .byte $00 ; |        | $F9A5
       .byte $00 ; |        | $F9A6
       .byte $00 ; |        | $F9A7
       .byte $E2 ; |XXX   X | $F9A8
       .byte $02 ; |      X | $F9A9
       .byte $00 ; |        | $F9AA
       .byte $00 ; |        | $F9AB
       .byte $00 ; |        | $F9AC
       .byte $00 ; |        | $F9AD
       .byte $00 ; |        | $F9AE
       .byte $00 ; |        | $F9AF
       .byte $E2 ; |XXX   X | $F9B0
       .byte $02 ; |      X | $F9B1
       .byte $00 ; |        | $F9B2
       .byte $00 ; |        | $F9B3
       .byte $00 ; |        | $F9B4
       .byte $00 ; |        | $F9B5
       .byte $00 ; |        | $F9B6
       .byte $00 ; |        | $F9B7
       .byte $E2 ; |XXX   X | $F9B8
       .byte $02 ; |      X | $F9B9
       .byte $00 ; |        | $F9BA
       .byte $00 ; |        | $F9BB
       .byte $00 ; |        | $F9BC
       .byte $00 ; |        | $F9BD
       .byte $00 ; |        | $F9BE
       .byte $00 ; |        | $F9BF
       .byte $E2 ; |XXX   X | $F9C0
       .byte $02 ; |      X | $F9C1
       .byte $00 ; |        | $F9C2
       .byte $00 ; |        | $F9C3
       .byte $00 ; |        | $F9C4
       .byte $00 ; |        | $F9C5
       .byte $00 ; |        | $F9C6
       .byte $00 ; |        | $F9C7
       .byte $E2 ; |XXX   X | $F9C8
       .byte $02 ; |      X | $F9C9
       .byte $00 ; |        | $F9CA
       .byte $00 ; |        | $F9CB
       .byte $00 ; |        | $F9CC
       .byte $00 ; |        | $F9CD
       .byte $00 ; |        | $F9CE
       .byte $00 ; |        | $F9CF
       .byte $E2 ; |XXX   X | $F9D0
       .byte $02 ; |      X | $F9D1
       .byte $00 ; |        | $F9D2
       .byte $00 ; |        | $F9D3
       .byte $00 ; |        | $F9D4
       .byte $00 ; |        | $F9D5
       .byte $00 ; |        | $F9D6
       .byte $00 ; |        | $F9D7
       .byte $E2 ; |XXX   X | $F9D8
       .byte $02 ; |      X | $F9D9
Reserve_Plane
       .byte $38 ; |  XXX   | $F9DA
       .byte $10 ; |   X    | $F9DB
       .byte $10 ; |   X    | $F9DC
;;;       .byte $10 ; |   X    | $F9DD
       .byte $FE ; |XXXXXXX | $F9DE
       .byte $38 ; |  XXX   | $F9DF
       .byte $10 ; |   X    | $F9E0

       .byte $00 ; |        | $F9E1 removed 1 line


       .byte $00 ; |        | $F9E1
       .byte $00 ; |        | $F9E2
       .byte $00 ; |        | $F9E3
       .byte $00 ; |        | $F9E4
       .byte $00 ; |        | $F9E5
       .byte $00 ; |        | $F9E6
       .byte $00 ; |        | $F9E7
       .byte $7B ; | XXXX XX| $F9E8
       .byte $E2 ; |XXX   X | $F9E9
       .byte $91 ; |X  X   X| $F9EA
       .byte $E0 ; |XXX     | $F9EB
       .byte $37 ; |  XX XXX| $F9EC
       .byte $A9 ; |X X X  X| $F9ED
       .byte $23 ; |  X   XX| $F9EE
       .byte $7C ; | XXXXX  | $F9EF
       .byte $1B ; |   XX XX| $F9F0
       .byte $D0 ; |XX X    | $F9F1
       .byte $3B ; |  XXX XX| $F9F2
       .byte $92 ; |X  X  X | $F9F3
       .byte $CB ; |XX  X XX| $F9F4
       .byte $B0 ; |X XX    | $F9F5
       .byte $F6 ; |XXXX XX | $F9F6
       .byte $AC ; |X X XX  | $F9F7
       .byte $85 ; |X    X X| $F9F8
       .byte $28 ; |  X X   | $F9F9
       .byte $3E ; |  XXXXX | $F9FA
       .byte $35 ; |  XX X X| $F9FB
       .byte $33 ; |  XX  XX| $F9FC
       .byte $91 ; |X  X   X| $F9FD
       .byte $30 ; |  XX    | $F9FE
       .byte $A2 ; |X X   X | $F9FF
       .byte $00 ; |        | $FA00
       .byte $00 ; |        | $FA01
       .byte $00 ; |        | $FA02
       .byte $00 ; |        | $FA03
       .byte $00 ; |        | $FA04
       .byte $00 ; |        | $FA05
       .byte $18 ; |   XX   | $FA06
       .byte $3C ; |  XXXX  | $FA07
       .byte $7E ; | XXXXXX | $FA08
       .byte $FF ; |XXXXXXXX| $FA09
       .byte $00 ; |        | $FA0A
       .byte $00 ; |        | $FA0B
       .byte $00 ; |        | $FA0C
       .byte $00 ; |        | $FA0D
       .byte $00 ; |        | $FA0E
       .byte $00 ; |        | $FA0F
       .byte $00 ; |        | $FA10
       .byte $00 ; |        | $FA11
       .byte $00 ; |        | $FA12
       .byte $00 ; |        | $FA13
       .byte $00 ; |        | $FA14
       .byte $00 ; |        | $FA15
       .byte $18 ; |   XX   | $FA16
       .byte $3C ; |  XXXX  | $FA17
       .byte $7E ; | XXXXXX | $FA18
       .byte $00 ; |        | $FA19
       .byte $00 ; |        | $FA1A
       .byte $00 ; |        | $FA1B
       .byte $00 ; |        | $FA1C
       .byte $00 ; |        | $FA1D
       .byte $00 ; |        | $FA1E
       .byte $00 ; |        | $FA1F
       .byte $00 ; |        | $FA20
       .byte $00 ; |        | $FA21
       .byte $00 ; |        | $FA22
       .byte $00 ; |        | $FA23
       .byte $00 ; |        | $FA24
       .byte $18 ; |   XX   | $FA25
       .byte $3C ; |  XXXX  | $FA26
       .byte $00 ; |        | $FA27
       .byte $00 ; |        | $FA28
       .byte $00 ; |        | $FA29
       .byte $00 ; |        | $FA2A
       .byte $00 ; |        | $FA2B
       .byte $00 ; |        | $FA2C
       .byte $00 ; |        | $FA2D
       .byte $00 ; |        | $FA2E
       .byte $00 ; |        | $FA2F
       .byte $00 ; |        | $FA30
       .byte $00 ; |        | $FA31
       .byte $00 ; |        | $FA32
       .byte $00 ; |        | $FA33
       .byte $18 ; |   XX   | $FA34
       .byte $3C ; |  XXXX  | $FA35
       .byte $00 ; |        | $FA36
       .byte $00 ; |        | $FA37
       .byte $00 ; |        | $FA38
       .byte $00 ; |        | $FA39
       .byte $00 ; |        | $FA3A
       .byte $00 ; |        | $FA3B
       .byte $00 ; |        | $FA3C
       .byte $00 ; |        | $FA3D
       .byte $00 ; |        | $FA3E
       .byte $00 ; |        | $FA3F
       .byte $00 ; |        | $FA40
       .byte $00 ; |        | $FA41
       .byte $00 ; |        | $FA42
       .byte $18 ; |   XX   | $FA43
       .byte $3C ; |  XXXX  | $FA44
       .byte $00 ; |        | $FA45
       .byte $00 ; |        | $FA46
       .byte $00 ; |        | $FA47
       .byte $00 ; |        | $FA48
       .byte $00 ; |        | $FA49
       .byte $00 ; |        | $FA4A
       .byte $00 ; |        | $FA4B
       .byte $00 ; |        | $FA4C
       .byte $00 ; |        | $FA4D
       .byte $00 ; |        | $FA4E
       .byte $00 ; |        | $FA4F
       .byte $00 ; |        | $FA50
       .byte $00 ; |        | $FA51
       .byte $80 ; |X       | $FA52
       .byte $80 ; |X       | $FA53
       .byte $78 ; | XXXX   | $FA54
       .byte $78 ; | XXXX   | $FA55
       .byte $7C ; | XXXXX  | $FA56
       .byte $7C ; | XXXXX  | $FA57
       .byte $FE ; |XXXXXXX | $FA58
       .byte $FE ; |XXXXXXX | $FA59
       .byte $5C ; | X XXX  | $FA5A
       .byte $5C ; | X XXX  | $FA5B
       .byte $3C ; |  XXXX  | $FA5C
       .byte $3C ; |  XXXX  | $FA5D
       .byte $12 ; |   X  X | $FA5E
       .byte $12 ; |   X  X | $FA5F
       .byte $00 ; |        | $FA60
       .byte $00 ; |        | $FA61
       .byte $40 ; | X      | $FA62
       .byte $40 ; | X      | $FA63
       .byte $38 ; |  XXX   | $FA64
       .byte $38 ; |  XXX   | $FA65
       .byte $7C ; | XXXXX  | $FA66
       .byte $7C ; | XXXXX  | $FA67
       .byte $38 ; |  XXX   | $FA68
       .byte $38 ; |  XXX   | $FA69
       .byte $14 ; |   X X  | $FA6A
       .byte $14 ; |   X X  | $FA6B
       .byte $00 ; |        | $FA6C
       .byte $00 ; |        | $FA6D
       .byte $00 ; |        | $FA6E
       .byte $00 ; |        | $FA6F
       .byte $00 ; |        | $FA70
       .byte $00 ; |        | $FA71
       .byte $10 ; |   X    | $FA72
       .byte $10 ; |   X    | $FA73
       .byte $38 ; |  XXX   | $FA74
       .byte $38 ; |  XXX   | $FA75
       .byte $38 ; |  XXX   | $FA76
       .byte $38 ; |  XXX   | $FA77
       .byte $10 ; |   X    | $FA78
       .byte $10 ; |   X    | $FA79
       .byte $00 ; |        | $FA7A
       .byte $00 ; |        | $FA7B
       .byte $00 ; |        | $FA7C
       .byte $00 ; |        | $FA7D
       .byte $00 ; |        | $FA7E
       .byte $00 ; |        | $FA7F
       .byte $00 ; |        | $FA80
       .byte $00 ; |        | $FA81
       .byte $10 ; |   X    | $FA82
       .byte $10 ; |   X    | $FA83
       .byte $38 ; |  XXX   | $FA84
       .byte $38 ; |  XXX   | $FA85
       .byte $10 ; |   X    | $FA86
       .byte $10 ; |   X    | $FA87
       .byte $00 ; |        | $FA88
       .byte $00 ; |        | $FA89
       .byte $00 ; |        | $FA8A
       .byte $00 ; |        | $FA8B
       .byte $00 ; |        | $FA8C
       .byte $00 ; |        | $FA8D
       .byte $00 ; |        | $FA8E
       .byte $00 ; |        | $FA8F
       .byte $00 ; |        | $FA90
       .byte $00 ; |        | $FA91
       .byte $10 ; |   X    | $FA92
       .byte $10 ; |   X    | $FA93
       .byte $10 ; |   X    | $FA94
       .byte $10 ; |   X    | $FA95
       .byte $00 ; |        | $FA96
       .byte $00 ; |        | $FA97
       .byte $00 ; |        | $FA98
       .byte $00 ; |        | $FA99
       .byte $00 ; |        | $FA9A
       .byte $00 ; |        | $FA9B
       .byte $00 ; |        | $FA9C
       .byte $00 ; |        | $FA9D
       .byte $00 ; |        | $FA9E
       .byte $00 ; |        | $FA9F
       .byte $00 ; |        | $FAA0
       .byte $00 ; |        | $FAA1
       .byte $02 ; |      X | $FAA2
       .byte $02 ; |      X | $FAA3
       .byte $3C ; |  XXXX  | $FAA4
       .byte $3C ; |  XXXX  | $FAA5
       .byte $7C ; | XXXXX  | $FAA6
       .byte $7C ; | XXXXX  | $FAA7
       .byte $FE ; |XXXXXXX | $FAA8
       .byte $FE ; |XXXXXXX | $FAA9
       .byte $74 ; | XXX X  | $FAAA
       .byte $74 ; | XXX X  | $FAAB
       .byte $78 ; | XXXX   | $FAAC
       .byte $78 ; | XXXX   | $FAAD
       .byte $90 ; |X  X    | $FAAE
       .byte $90 ; |X  X    | $FAAF
       .byte $00 ; |        | $FAB0
       .byte $00 ; |        | $FAB1
       .byte $04 ; |     X  | $FAB2
       .byte $04 ; |     X  | $FAB3
       .byte $38 ; |  XXX   | $FAB4
       .byte $38 ; |  XXX   | $FAB5
       .byte $7C ; | XXXXX  | $FAB6
       .byte $7C ; | XXXXX  | $FAB7
       .byte $38 ; |  XXX   | $FAB8
       .byte $38 ; |  XXX   | $FAB9
       .byte $50 ; | X X    | $FABA
       .byte $50 ; | X X    | $FABB
       .byte $00 ; |        | $FABC
       .byte $00 ; |        | $FABD
       .byte $00 ; |        | $FABE
       .byte $00 ; |        | $FABF
       .byte $00 ; |        | $FAC0
       .byte $00 ; |        | $FAC1
       .byte $41 ; | X     X| $FAC2
       .byte $41 ; | X     X| $FAC3
       .byte $96 ; |X  X XX | $FAC4
       .byte $96 ; |X  X XX | $FAC5
       .byte $68 ; | XX X   | $FAC6
       .byte $68 ; | XX X   | $FAC7
       .byte $99 ; |X  XX  X| $FAC8
       .byte $99 ; |X  XX  X| $FAC9
       .byte $38 ; |  XXX   | $FACA
       .byte $38 ; |  XXX   | $FACB
       .byte $55 ; | X X X X| $FACC
       .byte $55 ; | X X X X| $FACD
       .byte $D2 ; |XX X  X | $FACE
       .byte $D2 ; |XX X  X | $FACF
       .byte $00 ; |        | $FAD0
       .byte $00 ; |        | $FAD1
       .byte $00 ; |        | $FAD2
       .byte $00 ; |        | $FAD3
       .byte $00 ; |        | $FAD4
       .byte $00 ; |        | $FAD5
       .byte $00 ; |        | $FAD6
       .byte $00 ; |        | $FAD7
       .byte $00 ; |        | $FAD8
       .byte $00 ; |        | $FAD9
       .byte $00 ; |        | $FADA
       .byte $00 ; |        | $FADB
       .byte $00 ; |        | $FADC
       .byte $00 ; |        | $FADD
       .byte $00 ; |        | $FADE
       .byte $00 ; |        | $FADF
       .byte $00 ; |        | $FAE0
       .byte $00 ; |        | $FAE1
       .byte $24 ; |  X  X  | $FAE2
       .byte $24 ; |  X  X  | $FAE3
       .byte $98 ; |X  XX   | $FAE4
       .byte $98 ; |X  XX   | $FAE5
       .byte $72 ; | XXX  X | $FAE6
       .byte $72 ; | XXX  X | $FAE7
       .byte $15 ; |   X X X| $FAE8
       .byte $15 ; |   X X X| $FAE9
       .byte $28 ; |  X X   | $FAEA
       .byte $28 ; |  X X   | $FAEB
       .byte $24 ; |  X  X  | $FAEC
       .byte $24 ; |  X  X  | $FAED
       .byte $40 ; | X      | $FAEE
       .byte $40 ; | X      | $FAEF
       .byte $10 ; |   X    | $FAF0
       .byte $58 ; | X XX   | $FAF1
       .byte $44 ; | X   X  | $FAF2
       .byte $0C ; |    XX  | $FAF3
       .byte $68 ; | XX X   | $FAF4
       .byte $00 ; |        | $FAF5
       .byte $45 ; | X   X X| $FAF6
       .byte $0C ; |    XX  | $FAF7
       .byte $48 ; | X  X   | $FAF8
       .byte $00 ; |        | $FAF9
       .byte $01 ; |       X| $FAFA
       .byte $18 ; |   XX   | $FAFB
       .byte $08 ; |    X   | $FAFC
       .byte $40 ; | X      | $FAFD
       .byte $08 ; |    X   | $FAFE
       .byte $10 ; |   X    | $FAFF
       .byte $00 ; |        | $FB00
       .byte $E0 ; |XXX     | $FB01
       .byte $40 ; | X      | $FB02
       .byte $20 ; |  X     | $FB03
       .byte $00 ; |        | $FB04
       .byte $00 ; |        | $FB05
       .byte $00 ; |        | $FB06
       .byte $00 ; |        | $FB07
       .byte $00 ; |        | $FB08
       .byte $F0 ; |XXXX    | $FB09
       .byte $40 ; | X      | $FB0A
       .byte $20 ; |  X     | $FB0B
       .byte $10 ; |   X    | $FB0C
       .byte $00 ; |        | $FB0D
       .byte $00 ; |        | $FB0E
       .byte $00 ; |        | $FB0F
       .byte $00 ; |        | $FB10
       .byte $F8 ; |XXXXX   | $FB11
       .byte $60 ; | XX     | $FB12
       .byte $20 ; |  X     | $FB13
       .byte $10 ; |   X    | $FB14
       .byte $08 ; |    X   | $FB15
       .byte $00 ; |        | $FB16
       .byte $00 ; |        | $FB17
       .byte $00 ; |        | $FB18
       .byte $FC ; |XXXXXX  | $FB19
       .byte $60 ; | XX     | $FB1A
       .byte $30 ; |  XX    | $FB1B
       .byte $18 ; |   XX   | $FB1C
       .byte $0C ; |    XX  | $FB1D
       .byte $00 ; |        | $FB1E
       .byte $00 ; |        | $FB1F
       .byte $00 ; |        | $FB20
       .byte $FE ; |XXXXXXX | $FB21
       .byte $70 ; | XXX    | $FB22
       .byte $30 ; |  XX    | $FB23
       .byte $18 ; |   XX   | $FB24
       .byte $0C ; |    XX  | $FB25
       .byte $06 ; |     XX | $FB26
       .byte $00 ; |        | $FB27
       .byte $00 ; |        | $FB28
       .byte $FF ; |XXXXXXXX| $FB29
       .byte $78 ; | XXXX   | $FB2A
       .byte $38 ; |  XXX   | $FB2B
       .byte $1C ; |   XXX  | $FB2C
       .byte $0E ; |    XXX | $FB2D
       .byte $07 ; |     XXX| $FB2E
       .byte $03 ; |      XX| $FB2F
       .byte $00 ; |        | $FB30
       .byte $14 ; |   X X  | $FB31
       .byte $1C ; |   XXX  | $FB32
       .byte $08 ; |    X   | $FB33
       .byte $00 ; |        | $FB34
       .byte $00 ; |        | $FB35
       .byte $00 ; |        | $FB36
       .byte $00 ; |        | $FB37
       .byte $00 ; |        | $FB38
       .byte $14 ; |   X X  | $FB39
       .byte $14 ; |   X X  | $FB3A
       .byte $1C ; |   XXX  | $FB3B
       .byte $08 ; |    X   | $FB3C
       .byte $00 ; |        | $FB3D
       .byte $00 ; |        | $FB3E
       .byte $00 ; |        | $FB3F
       .byte $00 ; |        | $FB40
       .byte $36 ; |  XX XX | $FB41
       .byte $36 ; |  XX XX | $FB42
       .byte $3E ; |  XXXXX | $FB43
       .byte $1C ; |   XXX  | $FB44
       .byte $08 ; |    X   | $FB45
       .byte $00 ; |        | $FB46
       .byte $00 ; |        | $FB47
       .byte $00 ; |        | $FB48
       .byte $36 ; |  XX XX | $FB49
       .byte $36 ; |  XX XX | $FB4A
       .byte $3E ; |  XXXXX | $FB4B
       .byte $2A ; |  X X X | $FB4C
       .byte $1C ; |   XXX  | $FB4D
       .byte $08 ; |    X   | $FB4E
       .byte $00 ; |        | $FB4F
       .byte $00 ; |        | $FB50
       .byte $66 ; | XX  XX | $FB51
       .byte $66 ; | XX  XX | $FB52
       .byte $7E ; | XXXXXX | $FB53
       .byte $5A ; | X XX X | $FB54
       .byte $3C ; |  XXXX  | $FB55
       .byte $18 ; |   XX   | $FB56
       .byte $00 ; |        | $FB57
       .byte $00 ; |        | $FB58
       .byte $E7 ; |XXX  XXX| $FB59
       .byte $E7 ; |XXX  XXX| $FB5A
       .byte $E7 ; |XXX  XXX| $FB5B
       .byte $FF ; |XXXXXXXX| $FB5C
       .byte $5A ; | X XX X | $FB5D
       .byte $3C ; |  XXXX  | $FB5E
       .byte $18 ; |   XX   | $FB5F

Digit_0
       .byte $60 ; | XX     | $FB60
       .byte $90 ; |X  X    | $FB61
       .byte $90 ; |X  X    | $FB62
       .byte $90 ; |X  X    | $FB63
       .byte $90 ; |X  X    | $FB64
       .byte $90 ; |X  X    | $FB65
       .byte $60 ; | XX     | $FB66

Digit_1
       .byte $E0 ; |XXX     | $FB67
       .byte $40 ; | X      | $FB68
       .byte $40 ; | X      | $FB69
       .byte $40 ; | X      | $FB6A
       .byte $40 ; | X      | $FB6B
       .byte $C0 ; |XX      | $FB6C
       .byte $40 ; | X      | $FB6D

Digit_2
       .byte $F0 ; |XXXX    | $FB6E
       .byte $80 ; |X       | $FB6F
       .byte $40 ; | X      | $FB70
       .byte $20 ; |  X     | $FB71
       .byte $10 ; |   X    | $FB72
       .byte $90 ; |X  X    | $FB73
       .byte $60 ; | XX     | $FB74

Digit_3
       .byte $60 ; | XX     | $FB75
       .byte $90 ; |X  X    | $FB76
       .byte $10 ; |   X    | $FB77
       .byte $20 ; |  X     | $FB78
       .byte $10 ; |   X    | $FB79
       .byte $90 ; |X  X    | $FB7A
       .byte $60 ; | XX     | $FB7B

Digit_4
       .byte $10 ; |   X    | $FB7C
       .byte $10 ; |   X    | $FB7D
       .byte $10 ; |   X    | $FB7E
       .byte $F8 ; |XXXXX   | $FB7F
       .byte $90 ; |X  X    | $FB80
       .byte $50 ; | X X    | $FB81
       .byte $30 ; |  XX    | $FB82

Digit_5
       .byte $60 ; | XX     | $FB83
       .byte $90 ; |X  X    | $FB84
       .byte $10 ; |   X    | $FB85
       .byte $E0 ; |XXX     | $FB86
       .byte $80 ; |X       | $FB87
       .byte $80 ; |X       | $FB88
       .byte $F0 ; |XXXX    | $FB89

Digit_6
       .byte $60 ; | XX     | $FB8A
       .byte $90 ; |X  X    | $FB8B
       .byte $90 ; |X  X    | $FB8C
       .byte $E0 ; |XXX     | $FB8D
       .byte $80 ; |X       | $FB8E
       .byte $90 ; |X  X    | $FB8F
       .byte $60 ; | XX     | $FB90

Digit_7
       .byte $40 ; | X      | $FB91
       .byte $40 ; | X      | $FB92
       .byte $20 ; |  X     | $FB93
       .byte $20 ; |  X     | $FB94
       .byte $10 ; |   X    | $FB95
       .byte $90 ; |X  X    | $FB96
       .byte $F0 ; |XXXX    | $FB97

Digit_8
       .byte $60 ; | XX     | $FB98
       .byte $90 ; |X  X    | $FB99
       .byte $90 ; |X  X    | $FB9A
       .byte $60 ; | XX     | $FB9B
       .byte $90 ; |X  X    | $FB9C
       .byte $90 ; |X  X    | $FB9D
       .byte $60 ; | XX     | $FB9E

Digit_9
       .byte $60 ; | XX     | $FB9F
       .byte $90 ; |X  X    | $FBA0
       .byte $10 ; |   X    | $FBA1
       .byte $70 ; | XXX    | $FBA2
       .byte $90 ; |X  X    | $FBA3
       .byte $90 ; |X  X    | $FBA4
       .byte $60 ; | XX     | $FBA5

       .byte $00 ; |        | $FBA6
       .byte $00 ; |        | $FBA7
       .byte $00 ; |        | $FBA8
       .byte $00 ; |        | $FBA9
       .byte $00 ; |        | $FBAA
       .byte $00 ; |        | $FBAB
       .byte $00 ; |        | $FBAC
       .byte $00 ; |        | $FBAD
       .byte $24 ; |  X  X  | $FBAE
       .byte $98 ; |X  XX   | $FBAF
       .byte $72 ; | XXX  X | $FBB0
       .byte $15 ; |   X X X| $FBB1
       .byte $28 ; |  X X   | $FBB2
       .byte $24 ; |  X  X  | $FBB3
       .byte $40 ; | X      | $FBB4
       .byte $00 ; |        | $FBB5
       .byte $22 ; |  X   X | $FBB6
       .byte $08 ; |    X   | $FBB7
       .byte $5C ; | X XXX  | $FBB8
       .byte $3B ; |  XXX XX| $FBB9
       .byte $98 ; |X  XX   | $FBBA
       .byte $25 ; |  X  X X| $FBBB
       .byte $12 ; |   X  X | $FBBC
       .byte $00 ; |        | $FBBD
       .byte $00 ; |        | $FBBE
       .byte $00 ; |        | $FBBF
       .byte $00 ; |        | $FBC0
       .byte $00 ; |        | $FBC1
       .byte $00 ; |        | $FBC2
       .byte $00 ; |        | $FBC3
       .byte $00 ; |        | $FBC4
       .byte $0C ; |    XX  | $FBC5
       .byte $18 ; |   XX   | $FBC6
       .byte $15 ; |   X X X| $FBC7
       .byte $01 ; |       X| $FBC8
       .byte $18 ; |   XX   | $FBC9
       .byte $0C ; |    XX  | $FBCA
       .byte $14 ; |   X X  | $FBCB
       .byte $10 ; |   X    | $FBCC
       .byte $08 ; |    X   | $FBCD
       .byte $37 ; |  XX XXX| $FBCE
       .byte $19 ; |   XX  X| $FBCF
       .byte $04 ; |     X  | $FBD0
       .byte $1C ; |   XXX  | $FBD1
       .byte $0C ; |    XX  | $FBD2
       .byte $14 ; |   X X  | $FBD3
       .byte $84 ; |X    X  | $FBD4
       .byte $04 ; |     X  | $FBD5
       .byte $00 ; |        | $FBD6
       .byte $08 ; |    X   | $FBD7
       .byte $19 ; |   XX  X| $FBD8
       .byte $00 ; |        | $FBD9
       .byte $5A ; | X XX X | $FBDA
       .byte $10 ; |   X    | $FBDB
       .byte $40 ; | X      | $FBDC
       .byte $00 ; |        | $FBDD
       .byte $09 ; |    X  X| $FBDE
       .byte $0C ; |    XX  | $FBDF
       .byte $09 ; |    X  X| $FBE0
       .byte $00 ; |        | $FBE1
       .byte $41 ; | X     X| $FBE2
       .byte $58 ; | X XX   | $FBE3
       .byte $28 ; |  X X   | $FBE4
       .byte $14 ; |   X X  | $FBE5
       .byte $30 ; |  XX    | $FBE6
       .byte $00 ; |        | $FBE7
       .byte $48 ; | X  X   | $FBE8
       .byte $18 ; |   XX   | $FBE9
       .byte $04 ; |     X  | $FBEA
       .byte $08 ; |    X   | $FBEB
       .byte $4C ; | X  XX  | $FBEC
       .byte $14 ; |   X X  | $FBED
       .byte $00 ; |        | $FBEE
       .byte $10 ; |   X    | $FBEF
       .byte $0D ; |    XX X| $FBF0
       .byte $04 ; |     X  | $FBF1
       .byte $08 ; |    X   | $FBF2
       .byte $0C ; |    XX  | $FBF3
       .byte $01 ; |       X| $FBF4
       .byte $04 ; |     X  | $FBF5
       .byte $40 ; | X      | $FBF6
       .byte $5C ; | X XXX  | $FBF7
       .byte $00 ; |        | $FBF8
       .byte $00 ; |        | $FBF9
       .byte $68 ; | XX X   | $FBFA
       .byte $01 ; |       X| $FBFB
       .byte $A8 ; |X X X   | $FBFC
       .byte $08 ; |    X   | $FBFD
       .byte $04 ; |     X  | $FBFE
       .byte $18 ; |   XX   | $FBFF
LFC00:
       .byte $00 ; |        | $FC00
       .byte $00 ; |        | $FC01
       .byte $10 ; |   X    | $FC02
       .byte $10 ; |   X    | $FC03
       .byte $20 ; |  X     | $FC04
       .byte $20 ; |  X     | $FC05
       .byte $10 ; |   X    | $FC06
       .byte $10 ; |   X    | $FC07
       .byte $00 ; |        | $FC08
       .byte $00 ; |        | $FC09
       .byte $F0 ; |XXXX    | $FC0A
       .byte $F0 ; |XXXX    | $FC0B
       .byte $E0 ; |XXX     | $FC0C
       .byte $E0 ; |XXX     | $FC0D
       .byte $F0 ; |XXXX    | $FC0E
       .byte $F0 ; |XXXX    | $FC0F
       .byte $F0 ; |XXXX    | $FC10
       .byte $F0 ; |XXXX    | $FC11
       .byte $00 ; |        | $FC12
       .byte $00 ; |        | $FC13
       .byte $F0 ; |XXXX    | $FC14
       .byte $F0 ; |XXXX    | $FC15
       .byte $E0 ; |XXX     | $FC16
       .byte $E0 ; |XXX     | $FC17
       .byte $F0 ; |XXXX    | $FC18
       .byte $F0 ; |XXXX    | $FC19
       .byte $00 ; |        | $FC1A
       .byte $00 ; |        | $FC1B
       .byte $10 ; |   X    | $FC1C
       .byte $10 ; |   X    | $FC1D
       .byte $20 ; |  X     | $FC1E
       .byte $20 ; |  X     | $FC1F
       .byte $10 ; |   X    | $FC20
       .byte $10 ; |   X    | $FC21
       .byte $00 ; |        | $FC22
       .byte $00 ; |        | $FC23
       .byte $00 ; |        | $FC24
       .byte $00 ; |        | $FC25
       .byte $00 ; |        | $FC26
       .byte $00 ; |        | $FC27
       .byte $00 ; |        | $FC28
       .byte $00 ; |        | $FC29
       .byte $F0 ; |XXXX    | $FC2A
       .byte $F0 ; |XXXX    | $FC2B
       .byte $E0 ; |XXX     | $FC2C
       .byte $E0 ; |XXX     | $FC2D
       .byte $F0 ; |XXXX    | $FC2E
       .byte $F0 ; |XXXX    | $FC2F
       .byte $00 ; |        | $FC30
       .byte $00 ; |        | $FC31
       .byte $00 ; |        | $FC32
       .byte $00 ; |        | $FC33
       .byte $00 ; |        | $FC34
       .byte $00 ; |        | $FC35
       .byte $F0 ; |XXXX    | $FC36
       .byte $F0 ; |XXXX    | $FC37
       .byte $E0 ; |XXX     | $FC38
       .byte $E0 ; |XXX     | $FC39
       .byte $F0 ; |XXXX    | $FC3A
       .byte $F0 ; |XXXX    | $FC3B
       .byte $00 ; |        | $FC3C
       .byte $00 ; |        | $FC3D
       .byte $00 ; |        | $FC3E
       .byte $00 ; |        | $FC3F
       .byte $10 ; |   X    | $FC40
       .byte $10 ; |   X    | $FC41
       .byte $20 ; |  X     | $FC42
       .byte $20 ; |  X     | $FC43
       .byte $10 ; |   X    | $FC44
       .byte $10 ; |   X    | $FC45
       .byte $10 ; |   X    | $FC46
       .byte $10 ; |   X    | $FC47
       .byte $10 ; |   X    | $FC48
       .byte $10 ; |   X    | $FC49
       .byte $20 ; |  X     | $FC4A
       .byte $20 ; |  X     | $FC4B
       .byte $10 ; |   X    | $FC4C
       .byte $10 ; |   X    | $FC4D
       .byte $00 ; |        | $FC4E
       .byte $00 ; |        | $FC4F
       .byte $F0 ; |XXXX    | $FC50
       .byte $F0 ; |XXXX    | $FC51
       .byte $E0 ; |XXX     | $FC52
       .byte $E0 ; |XXX     | $FC53
       .byte $F0 ; |XXXX    | $FC54
       .byte $F0 ; |XXXX    | $FC55
       .byte $F0 ; |XXXX    | $FC56
       .byte $F0 ; |XXXX    | $FC57
       .byte $00 ; |        | $FC58
       .byte $00 ; |        | $FC59
       .byte $00 ; |        | $FC5A
       .byte $00 ; |        | $FC5B
       .byte $F0 ; |XXXX    | $FC5C
       .byte $F0 ; |XXXX    | $FC5D
       .byte $E0 ; |XXX     | $FC5E
       .byte $E0 ; |XXX     | $FC5F
       .byte $F0 ; |XXXX    | $FC60
       .byte $F0 ; |XXXX    | $FC61
       .byte $00 ; |        | $FC62
       .byte $00 ; |        | $FC63
       .byte $00 ; |        | $FC64
       .byte $00 ; |        | $FC65
       .byte $00 ; |        | $FC66
       .byte $00 ; |        | $FC67
       .byte $F0 ; |XXXX    | $FC68
       .byte $F0 ; |XXXX    | $FC69
       .byte $E0 ; |XXX     | $FC6A
       .byte $E0 ; |XXX     | $FC6B
       .byte $F0 ; |XXXX    | $FC6C
       .byte $F0 ; |XXXX    | $FC6D
       .byte $E0 ; |XXX     | $FC6E
       .byte $E0 ; |XXX     | $FC6F
       .byte $F0 ; |XXXX    | $FC70
       .byte $F0 ; |XXXX    | $FC71
       .byte $00 ; |        | $FC72
       .byte $00 ; |        | $FC73
       .byte $10 ; |   X    | $FC74
       .byte $10 ; |   X    | $FC75
       .byte $20 ; |  X     | $FC76
       .byte $20 ; |  X     | $FC77
       .byte $10 ; |   X    | $FC78
       .byte $10 ; |   X    | $FC79
       .byte $00 ; |        | $FC7A
       .byte $00 ; |        | $FC7B
       .byte $F0 ; |XXXX    | $FC7C
       .byte $F0 ; |XXXX    | $FC7D
       .byte $E0 ; |XXX     | $FC7E
       .byte $E0 ; |XXX     | $FC7F
       .byte $F0 ; |XXXX    | $FC80
       .byte $F0 ; |XXXX    | $FC81
       .byte $00 ; |        | $FC82
       .byte $00 ; |        | $FC83
       .byte $00 ; |        | $FC84
       .byte $00 ; |        | $FC85
       .byte $00 ; |        | $FC86
       .byte $00 ; |        | $FC87
       .byte $F0 ; |XXXX    | $FC88
       .byte $F0 ; |XXXX    | $FC89
       .byte $E0 ; |XXX     | $FC8A
       .byte $E0 ; |XXX     | $FC8B
       .byte $F0 ; |XXXX    | $FC8C
       .byte $F0 ; |XXXX    | $FC8D
       .byte $00 ; |        | $FC8E
       .byte $00 ; |        | $FC8F
       .byte $F0 ; |XXXX    | $FC90
       .byte $F0 ; |XXXX    | $FC91
       .byte $E0 ; |XXX     | $FC92
       .byte $E0 ; |XXX     | $FC93
       .byte $F0 ; |XXXX    | $FC94
       .byte $F0 ; |XXXX    | $FC95
       .byte $00 ; |        | $FC96
       .byte $00 ; |        | $FC97
       .byte $00 ; |        | $FC98
       .byte $00 ; |        | $FC99
       .byte $00 ; |        | $FC9A
       .byte $00 ; |        | $FC9B
       .byte $10 ; |   X    | $FC9C
       .byte $10 ; |   X    | $FC9D
       .byte $20 ; |  X     | $FC9E
       .byte $20 ; |  X     | $FC9F
       .byte $10 ; |   X    | $FCA0
       .byte $10 ; |   X    | $FCA1
       .byte $00 ; |        | $FCA2
       .byte $00 ; |        | $FCA3
       .byte $00 ; |        | $FCA4
       .byte $00 ; |        | $FCA5
       .byte $00 ; |        | $FCA6
       .byte $00 ; |        | $FCA7
       .byte $10 ; |   X    | $FCA8
       .byte $10 ; |   X    | $FCA9
       .byte $20 ; |  X     | $FCAA
       .byte $20 ; |  X     | $FCAB
       .byte $10 ; |   X    | $FCAC
       .byte $10 ; |   X    | $FCAD
       .byte $20 ; |  X     | $FCAE
       .byte $20 ; |  X     | $FCAF
       .byte $10 ; |   X    | $FCB0
       .byte $10 ; |   X    | $FCB1
       .byte $00 ; |        | $FCB2
       .byte $00 ; |        | $FCB3
       .byte $00 ; |        | $FCB4
       .byte $00 ; |        | $FCB5
       .byte $00 ; |        | $FCB6
       .byte $00 ; |        | $FCB7
       .byte $10 ; |   X    | $FCB8
       .byte $10 ; |   X    | $FCB9
       .byte $20 ; |  X     | $FCBA
       .byte $20 ; |  X     | $FCBB
       .byte $10 ; |   X    | $FCBC
       .byte $10 ; |   X    | $FCBD
       .byte $F0 ; |XXXX    | $FCBE
       .byte $F0 ; |XXXX    | $FCBF
       .byte $E0 ; |XXX     | $FCC0
       .byte $E0 ; |XXX     | $FCC1
       .byte $F0 ; |XXXX    | $FCC2
       .byte $F0 ; |XXXX    | $FCC3
       .byte $10 ; |   X    | $FCC4
       .byte $10 ; |   X    | $FCC5
       .byte $20 ; |  X     | $FCC6
       .byte $20 ; |  X     | $FCC7
       .byte $10 ; |   X    | $FCC8
       .byte $10 ; |   X    | $FCC9
       .byte $00 ; |        | $FCCA
       .byte $00 ; |        | $FCCB
       .byte $00 ; |        | $FCCC
       .byte $00 ; |        | $FCCD
       .byte $00 ; |        | $FCCE
       .byte $00 ; |        | $FCCF
       .byte $10 ; |   X    | $FCD0
       .byte $10 ; |   X    | $FCD1
       .byte $20 ; |  X     | $FCD2
       .byte $20 ; |  X     | $FCD3
       .byte $10 ; |   X    | $FCD4
       .byte $10 ; |   X    | $FCD5
       .byte $00 ; |        | $FCD6
       .byte $00 ; |        | $FCD7
       .byte $00 ; |        | $FCD8
       .byte $00 ; |        | $FCD9
       .byte $F0 ; |XXXX    | $FCDA
       .byte $F0 ; |XXXX    | $FCDB
       .byte $E0 ; |XXX     | $FCDC
       .byte $E0 ; |XXX     | $FCDD
       .byte $E0 ; |XXX     | $FCDE
       .byte $E0 ; |XXX     | $FCDF
       .byte $F0 ; |XXXX    | $FCE0
       .byte $F0 ; |XXXX    | $FCE1
       .byte $00 ; |        | $FCE2
       .byte $00 ; |        | $FCE3
       .byte $F0 ; |XXXX    | $FCE4
       .byte $F0 ; |XXXX    | $FCE5
       .byte $E0 ; |XXX     | $FCE6
       .byte $E0 ; |XXX     | $FCE7
       .byte $F0 ; |XXXX    | $FCE8
       .byte $F0 ; |XXXX    | $FCE9
       .byte $00 ; |        | $FCEA
       .byte $00 ; |        | $FCEB
       .byte $10 ; |   X    | $FCEC
       .byte $10 ; |   X    | $FCED
       .byte $20 ; |  X     | $FCEE
       .byte $20 ; |  X     | $FCEF
       .byte $20 ; |  X     | $FCF0
       .byte $20 ; |  X     | $FCF1
       .byte $10 ; |   X    | $FCF2
       .byte $10 ; |   X    | $FCF3
       .byte $00 ; |        | $FCF4
       .byte $00 ; |        | $FCF5
       .byte $10 ; |   X    | $FCF6
       .byte $20 ; |  X     | $FCF7
       .byte $10 ; |   X    | $FCF8
       .byte $00 ; |        | $FCF9
       .byte $F0 ; |XXXX    | $FCFA
       .byte $E0 ; |XXX     | $FCFB
       .byte $F0 ; |XXXX    | $FCFC
       .byte $00 ; |        | $FCFD
       .byte $00 ; |        | $FCFE
       .byte $00 ; |        | $FCFF


LFD00:
       sta    WSYNC                   ;3
LFD02:
       sta    HMOVE                   ;3
       dec    $9A                     ;5
       ldy    $9A                     ;3
       cpy    $85                     ;3
       bcc    LFD16                   ;2
    IF PLUSROM
       jsr    Waste_13                ;6
    ELSE
;waste 22 here (including BCS)
       lda    $80                     ;3
       nop                            ;2
       nop                            ;2
       lda    ($80,X)                 ;6
       lda    ($80,X)                 ;6
    ENDIF
       bcs    LFD24                   ;2 always branch

LFD16:
       lda    ($A0),Y                 ;5
       sta    ENAM0                   ;3
       sta    HMM0                    ;3
       eor    #$F0                    ;2
       adc    #$10                    ;2
       sta    ENAM1                   ;3
       sta    HMM1                    ;3
LFD24:
       dex                            ;2
       beq    LFD45                   ;2
       nop                            ;2
       dec    $9B                     ;5
       ldy    $9B                     ;3
       cpy    #$10                    ;2
       bcc    LFD3A                   ;2
       lda    #$00                    ;2
       sta    GRP0                    ;3
       sta    GRP1                    ;3
    IF PLUSROM
       nop                            ;2
       nop                            ;2
       nop                            ;2
    ELSE
       lda    ($80,X)                 ;6
    ENDIF
       bcs    LFD42                   ;2 always branch

LFD3A:
       lda    ($A2),Y                 ;5
       sta    GRP0                    ;3
       lda    ($A4),Y                 ;5
       sta    GRP1                    ;3
LFD42:
       jmp    LFD02                   ;3

LFD45:
    IF PLUSROM
       nop                            ;2
       nop                            ;2
       nop                            ;2
    ELSE
       lda    ($80,X)                 ;6
    ENDIF
       lda    CXM0P                   ;3
       sta    $D2                     ;3
       sta    CXCLR                   ;3
       lda    $80                     ;3
       nop                            ;2
       nop                            ;2
       sta    GRP0                    ;3
       sta    GRP1                    ;3
  IF PAL60
       lda    #$26                    ;2
  ELSE
       lda    #$14                    ;2
  ENDIF
       sta    COLUBK                  ;3
       sta    HMOVE                   ;3
       dec    $9A                     ;5
       ldy    $9A                     ;3
       cpy    $85                     ;3
       bcc    LFD6D                   ;2
    IF PLUSROM
       jsr    Waste_13                ;6
    ELSE
;waste 22 here (including BCS)
       lda    $80                     ;3
       nop                            ;2
       nop                            ;2
       lda    ($80,X)                 ;6
       lda    ($80,X)                 ;6
    ENDIF
       bcs    LFD7B                   ;2 always branch

LFD6D:
       lda    ($A0),Y                 ;5
       sta    ENAM0                   ;3
       sta    HMM0                    ;3
       eor    #$F0                    ;2
       adc    #$10                    ;2
       sta    ENAM1                   ;3
       sta    HMM1                    ;3
LFD7B:
       dec    $9A                     ;5
       ldy    $9A                     ;3
       cpy    $85                     ;3
       bcc    LFD8D                   ;2
       ldx    $80                     ;3
       ldy    #$00                    ;2
    IF PLUSROM
       jsr    Waste_6                ;6
    ELSE
       lda    ($80,X)                 ;6
       lda    ($80,X)                 ;6
    ENDIF
       bcs    LFD99                   ;2 always branch

LFD8D:
       lda    ($A0),Y                 ;5
       sta    ENAM0                   ;3
       tax                            ;2
       eor    #$F0                    ;2
       adc    #$10                    ;2
       sta    ENAM1                   ;3
       tay                            ;2
LFD99:
       lda    #$70                    ;2
       sta    HMP0                    ;3
       sta    HMOVE                   ;3
       lda    $94                     ;3
       cmp    #$0F                    ;2
       bcc    LFDAC                   ;2
       cmp    #$70                    ;2
       bcc    LFDBB                   ;2
       jmp    LFDCB                   ;3 could use BCS

LFDAC:
       sec                            ;2
LFDAD:
       sbc    #$0F                    ;2
       bcs    LFDAD                   ;2
       nop                            ;2
       sta    RESP0                   ;3
       stx    HMM0                    ;3
       sty    HMM1                    ;3
       jmp    LFDDF                   ;3 could use BCC

LFDBB:
       sbc    #$05                    ;2
       sec                            ;2
LFDBE:
       sbc    #$0F                    ;2
       bcs    LFDBE                   ;2
       sta    RESP0                   ;3
       stx    HMM0                    ;3
       sty    HMM1                    ;3
       jmp    LFDDF                   ;3 could use BCC

LFDCB:
    IF PLUSROM
       nop                            ;2
       nop                            ;2
       nop                            ;2
    ELSE
       lda    ($80,X)                 ;6
    ENDIF
       lda    $80                     ;3
       lda    $94                     ;3
       stx    HMM0                    ;3
       sty    HMM1                    ;3
       sbc    #$48                    ;2
       sec                            ;2
LFDD8:
       sbc    #$0F                    ;2
       bcs    LFDD8                   ;2
       nop                            ;2
       sta    RESP0                   ;3
LFDDF:
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       stx    ENAM0                   ;3
       sty    ENAM1                   ;3
       tax                            ;2
       dec    $9A                     ;5
       ldy    $9A                     ;3
       cpy    $85                     ;3
       bcc    LFDFA                   ;2
    IF PLUSROM
       jsr    Waste_14                ;6
    ELSE
;waste 23 here (including BCS page cross)
       lda    $80                     ;3
       nop                            ;2
       nop                            ;2
       lda    ($80,X)                 ;6
       lda    ($80,X)                 ;6
    ENDIF
       bcs    LFE08                   ;2 always branch

LFDFA:
       lda    ($A0),Y                 ;5
       sta    ENAM0                   ;3
       sta    HMM0                    ;3
       eor    #$F0                    ;2
       adc    #$10                    ;2
       sta    ENAM1                   ;3
       sta    HMM1                    ;3
LFE08:
       txa                            ;2
       eor    #$FF                    ;2
       clc                            ;2
       sbc    #$06                    ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       sta    HMP0                    ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       dec    $9A                     ;5
       ldy    $9A                     ;3
       cpy    $85                     ;3
       bcc    LFE2A                   ;2
    IF PLUSROM
       jsr    Waste_13                ;6
    ELSE
;waste 22 here (including BCS)
       lda    $80                     ;3
       nop                            ;2
       nop                            ;2
       lda    ($80,X)                 ;6
       lda    ($80,X)                 ;6
    ENDIF
       bcs    LFE38                   ;2 always branch

LFE2A:
       lda    ($A0),Y                 ;5
       sta    ENAM0                   ;3
       sta    HMM0                    ;3
       eor    #$F0                    ;2
       adc    #$10                    ;2
       sta    ENAM1                   ;3
       sta    HMM1                    ;3
LFE38:
       lda    #$00                    ;2
       sta    HMP0                    ;3
       sta    NUSIZ0                  ;3
       lda    #$05                    ;2
       sta    NUSIZ1                  ;3 could move lower to reuse #$00 value x3 ahead!
       lda    #$00                    ;2
       sta    COLUP0                  ;3
  IF RIVER
    IF PAL60
       lda    #$D0                    ;2
    ELSE
       lda    #$80                    ;2
    ENDIF
  ELSE
    IF PAL60
       lda    #$28                    ;2
    ELSE
       lda    #$16                    ;2
    ENDIF
  ENDIF
       sta    COLUP1                  ;3
       lda    #$00                    ;2
       sta    VDELP0                  ;3
       lda    #$00                    ;2
       sta    VDELP1                  ;3
  IF PAL60
       lda    #$2A                    ;2
  ELSE
       lda    #$18                    ;2
  ENDIF
       sta    COLUBK                  ;3
       sta    HMOVE                   ;3
       dec    $9A                     ;5
       ldy    $9A                     ;3
       cpy    $85                     ;3
       bcc    LFE6A                   ;2
    IF PLUSROM
       jsr    Waste_13                ;6
    ELSE
;waste 22 here (including BCS)
       lda    $80                     ;3
       nop                            ;2
       nop                            ;2
       lda    ($80,X)                 ;6
       lda    ($80,X)                 ;6
    ENDIF
       bcs    LFE78                   ;2 always branch

LFE6A:
       lda    ($A0),Y                 ;5
       sta    ENAM0                   ;3
       sta    HMM0                    ;3
       eor    #$F0                    ;2
       adc    #$10                    ;2
       sta    ENAM1                   ;3
       sta    HMM1                    ;3
LFE78:
       dec    $9A                     ;5
       ldy    $9A                     ;3
       cpy    $85                     ;3
       bcc    LFE8B                   ;2
       lda    #$00                    ;2
       tay                            ;2
       tax                            ;2
       nop                            ;2
    IF PLUSROM
       nop                            ;2
       nop                            ;2
       nop                            ;2
    ELSE
       lda    ($80,X)                 ;6
    ENDIF
       lda    $80                     ;3
       bcs    LFE97                   ;2 always branch

LFE8B:
       lda    ($A0),Y                 ;5
       sta    ENAM0                   ;3
       tax                            ;2
       eor    #$F0                    ;2
       adc    #$10                    ;2
       sta    ENAM1                   ;3
       tay                            ;2
LFE97:
       lda    #$70                    ;2
       sta    HMP1                    ;3
       sta    HMOVE                   ;3
       lda    $95                     ;3
       cmp    #$0F                    ;2
       bcc    LFEAA                   ;2
       cmp    #$70                    ;2
       bcc    LFEB9                   ;2
       jmp    LFEC9                   ;3 could use BCS

LFEAA:
       sec                            ;2
LFEAB:
       sbc    #$0F                    ;2
       bcs    LFEAB                   ;2
       nop                            ;2
       sta    RESP1                   ;3
       stx    HMM0                    ;3
       sty    HMM1                    ;3
       jmp    LFEDD                   ;3 could use BCC

LFEB9:
       sbc    #$05                    ;2
       sec                            ;2
LFEBC:
       sbc    #$0F                    ;2
       bcs    LFEBC                   ;2
       sta    RESP1                   ;3
       stx    HMM0                    ;3
       sty    HMM1                    ;3
       jmp    LFEDD                   ;3 could use BCC

LFEC9:
    IF PLUSROM
       nop                            ;2
       nop                            ;2
       nop                            ;2
    ELSE
       lda    ($80,X)                 ;6
    ENDIF
       lda    $80                     ;3
       lda    $95                     ;3
       stx    HMM0                    ;3
       sty    HMM1                    ;3
       sbc    #$48                    ;2
       sec                            ;2
LFED6:
       sbc    #$0F                    ;2
       bcs    LFED6                   ;2
       nop                            ;2
       sta    RESP1                   ;3
LFEDD:
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       stx    ENAM0                   ;3
       sty    ENAM1                   ;3
       tax                            ;2
       dec    $9A                     ;5
       ldy    $9A                     ;3
       cpy    $85                     ;3
       bcc    LFEF8                   ;2
    IF PLUSROM
       jsr    Waste_14                ;6
    ELSE
;waste 23 here (including BCS page cross)
       lda    $80                     ;3
       nop                            ;2
       nop                            ;2
       lda    ($80,X)                 ;6
       lda    ($80,X)                 ;6
    ENDIF
       bcs    LFF06                   ;2 always branch

LFEF8:
       lda    ($A0),Y                 ;5
       sta    ENAM0                   ;3
       sta    HMM0                    ;3
       eor    #$F0                    ;2
       adc    #$10                    ;2
       sta    ENAM1                   ;3
       sta    HMM1                    ;3
LFF06:
       txa                            ;2
       eor    #$FF                    ;2
       clc                            ;2
       sbc    #$06                    ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       sta    HMP1                    ;3
       lda    #$00                    ;2
       sta    HMP0                    ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       dec    $9A                     ;5
       ldy    $9A                     ;3
       cpy    $85                     ;3
       bcc    LFF2C                   ;2
    IF PLUSROM
       jsr    Waste_13                ;6
    ELSE
;waste 22 here (including BCS)
       lda    $80                     ;3
       nop                            ;2
       nop                            ;2
       lda    ($80,X)                 ;6
       lda    ($80,X)                 ;6
    ENDIF
       bcs    LFF3A                   ;2 always branch

LFF2C:
       lda    ($A0),Y                 ;5
       sta    ENAM0                   ;3
       sta    HMM0                    ;3
       eor    #$F0                    ;2
       adc    #$10                    ;2
       sta    ENAM1                   ;3
       sta    HMM1                    ;3
LFF3A:
       lda    #$00                    ;2
       sta    HMP1                    ;3
       ldx    #$00                    ;2
    IF PLUSROM
       jsr Waste_20                   ;6
    ELSE
;waste 26
       lda    ($80,X)                 ;6
       lda    ($80,X)                 ;6
       lda    ($80,X)                 ;6
       lda    ($80,X)                 ;6
       nop                            ;2
    ENDIF
  IF PAL60
       lda    #$58                    ;2
  ELSE
       lda    #$C8                    ;2
  ENDIF
       sta    COLUBK                  ;3
LFF4D:
       sta    HMOVE                   ;3
       dec    $9A                     ;5
       ldy    $9A                     ;3
       cpy    $85                     ;3
       bcc    LFF61                   ;2
    IF PLUSROM
       jsr    Waste_13                ;6
    ELSE
;waste 22 here (including BCS)
       lda    $80                     ;3
       nop                            ;2
       nop                            ;2
       lda    ($80,X)                 ;6
       lda    ($80,X)                 ;6
    ENDIF
       bcs    LFF6F                   ;2 always branch

LFF61:
       lda    ($A0),Y                 ;5
       sta    ENAM0                   ;3
       sta    HMM0                    ;3
       eor    #$F0                    ;2
       adc    #$10                    ;2
       sta    ENAM1                   ;3
       sta    HMM1                    ;3
LFF6F:
       lda    LF89C,X                 ;4
       sta    GRP1                    ;3
       ldy    $87                     ;3
       lda    LFC00,Y                 ;4
       sta    HMP1                    ;3
       dec    $9C                     ;5
       ldy    $9C                     ;3
       cpy    #$08                    ;2
       bcc    LFF89                   ;2
       lda    $80                     ;3
       sta    $80                     ;3
       bcs    LFF8D                   ;2 always branch

LFF89:
       lda    ($A6),Y                 ;5
       sta    GRP0                    ;3
LFF8D:
       sta    HMOVE                   ;3
       dec    $9A                     ;5
       ldy    $9A                     ;3
       cpy    $85                     ;3
       bcc    LFFA1                   ;2
    IF PLUSROM
       jsr    Waste_13                ;6
    ELSE
;waste 22 here (including BCS)
       lda    $80                     ;3
       nop                            ;2
       nop                            ;2
       lda    ($80,X)                 ;6
       lda    ($80,X)                 ;6
    ENDIF
       bcs    LFFAF                   ;2 always branch

LFFA1:
       lda    ($A0),Y                 ;5
       sta    ENAM0                   ;3
       sta    HMM0                    ;3
       eor    #$F0                    ;2
       adc    #$10                    ;2
       sta    ENAM1                   ;3
       sta    HMM1                    ;3
LFFAF:
       inc    $87                     ;5
       inx                            ;2
       cpx    $84                     ;3
       beq    LFFCC                   ;2
       nop                            ;2
       dec    $9D                     ;5
       ldy    $9D                     ;3
       cpy    #$08                    ;2
       bcc    LFFC5                   ;2
       lda    $80                     ;3
       sta    $80                     ;3
       bcs    LFFC9                   ;2 always branch

LFFC5:
       lda    ($A8),Y                 ;5
       sta    GRP0                    ;3
LFFC9:
       jmp    LFF4D                   ;3

LFFCC:
       sta    WSYNC                   ;3
       lda    #$00                    ;2
       sta    COLUBK                  ;3
       sta    GRP0                    ;3
       sta    GRP1                    ;3
       sta    GRP0                    ;3
       sta    GRP1                    ;3
       sta    ENAM0                   ;3
       sta    ENAM1                   ;3
       sta    HMCLR                   ;3
       rts                            ;6

; unused?
       .byte $10 ; |   X    | $FFE1
       .byte $08 ; |    X   | $FFE2
       .byte $00 ; |        | $FFE3
       .byte $50 ; | X X    | $FFE4
       .byte $04 ; |     X  | $FFE5
       .byte $00 ; |        | $FFE6
       .byte $00 ; |        | $FFE7
       .byte $A1 ; |X X    X| $FFE8
       .byte $0C ; |    XX  | $FFE9
       .byte $00 ; |        | $FFEA
       .byte $08 ; |    X   | $FFEB
       .byte $17 ; |   X XXX| $FFEC
       .byte $08 ; |    X   | $FFED
       .byte $08 ; |    X   | $FFEE
       .byte $00 ; |        | $FFEF
       .byte $58 ; | X XX   | $FFF0
       .byte $08 ; |    X   | $FFF1
       .byte $00 ; |        | $FFF2
       .byte $00 ; |        | $FFF3
       .byte $18 ; |   XX   | $FFF4
       .byte $1C ; |   XXX  | $FFF5
       .byte $08 ; |    X   | $FFF6

    IF SUPERCHARGER = 1
       ORG $FFF7,0
       .byte "-2008"
    ELSE
LFFF7:
       .byte $F0 ; |XXXX    | $FFF7 5 bytes
       .byte $D0 ; |XX X    | $FFF8
       .byte $00 ; |        | $FFF9
       .byte $00 ; |        | $FFFA
       .byte $00 ; |        | $FFFB

    ENDIF

    IF PLUSROM = 1

Waste_13
        nop                   ; 2
        nop                   ; 2
        nop 0                 ; 3
        rts                   ; 6

Waste_20
        nop                   ; 2
        nop                   ; 2
        nop                   ; 2
Waste_14
        nop                   ; 2
        nop                   ; 2
        nop                   ; 2
        nop                   ; 2
Waste_6
        rts                   ; 6

PlusROM_API
       .byte "a", 0, "h.firmaplus.de", 0

       ORG $FFFA
        .word (PlusROM_API - $E000)      ; PlusRom API pointer

    ENDIF

       ORG $FFFC
       .word START,START
