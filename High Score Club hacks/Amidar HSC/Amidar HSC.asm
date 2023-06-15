AMIDAR_DS = 0
PAL = 0
PLUSROM = 1


; Disassembly of Amidar.bin
; Disassembled Tue Mar 25 15:19:09 2008
; Using DiStella v3.0
;
; Disassembled by AtariAge user Nukey Shay (https://forums.atariage.com/profile/222-nukey-shay/)
; PlusROM HSC function added by W.Stubig (Al_Nafuur https://forums.atariage.com/profile/41465-al_nafuur/)
;
; Command Line: C:\BIN\D3.EXE -pafsicAmidar.cfg Amidar.bin 
; Amidar.cfg contents:
;      ORG  F000
;      CODE F000 F8CD
;      GFX  F8CE F8E0
;      CODE F8E1 F8FA
;      GFX  F8FB F9A9
;      CODE F9AA F9B9
;      GFX  F9BA FA82
;      CODE FA83 FFF7
;      GFX  FFF8 FFFF

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
PF1     =  $0E
PF2     =  $0F
RESP0   =  $10
RESP1   =  $11
RESM0   =  $12
AUDC0   =  $15
AUDF0   =  $17
AUDV0   =  $19
GRP0    =  $1B
GRP1    =  $1C
ENAM0   =  $1D
HMP0    =  $20
HMP1    =  $21
HMM0    =  $22
VDELP0  =  $25
VDELP1  =  $26
HMOVE   =  $2A
HMCLR   =  $2B
INPT4   =  $3C
SWCHA   =  $0280
SWCHB   =  $0282
INTIM   =  $0284
TIM64T  =  $0296


   IF PLUSROM

WriteToBuffer           = $1FF0
WriteSendBuffer         = $1FF1
ReceiveBuffer           = $1FF2
ReceiveBufferSize       = $1FF3

HIGHSCORE_ID            = 65         ; Amidar game ID in Highscore DB

   ENDIF


       ORG $F000

LF024:
       lda    #$B0                    ;2
       sta    TIM64T                  ;4
       inc    $80                     ;5
       lda    $80                     ;3
       and    #$03                    ;2
       sta    $BA                     ;3
       lda    #$BA                    ;2
       sta    $82                     ;3
       sta    $83                     ;3
       lda    #$FF                    ;2
       sta    $B4                     ;3
       sta    $B5                     ;3
       ldx    #$07                    ;2
       stx    $E5                     ;3
       bne    LF051                   ;2 always branch

LF043:
       ldy    #$01                    ;2
       bne    LF049                   ;2 always branch

LF047:
       ldy    #$02                    ;2
LF049:
       sty    $B8                     ;3
       txa                            ;2
       ora    $E8                     ;3
       sta.wy $B3,Y                   ;5
LF051:
       dec    $E5                     ;5
       bmi    LF0BA                   ;2
       ldx    $E5                     ;3
       lda    $BB,X                   ;4
       cmp    #$FF                    ;2
       beq    LF051                   ;2
       lda    $C9,X                   ;4
       and    #$03                    ;2
       cmp    $BA                     ;3
       bne    LF069                   ;2
       lda    #$18                    ;2
       bne    LF070                   ;2 always branch

LF069:
       lda    #$10                    ;2
       cpx    $D7                     ;3
       beq    LF070                   ;2
       lsr                            ;2
LF070:
       sta    $E8                     ;3
       lda    $B5                     ;3
       bmi    LF047                   ;2
       lda    $B4                     ;3
       bmi    LF043                   ;2
       lda    $B8                     ;3
       cmp    #$01                    ;2
       bne    LF0A8                   ;2
       beq    LF08E                   ;2 always branch

LF082:
       lda    $B4                     ;3
       cmp    $E8                     ;3
       bcc    LF043                   ;2
       lda    $E8                     ;3
       cmp    #$18                    ;2
       bne    LF051                   ;2
LF08E:
       lda    $B5                     ;3
       and    #$07                    ;2
       tay                            ;2
       lda.wy $BB,Y                   ;4
       sbc    $BB,X                   ;4
       cmp    #$12                    ;2
       bcs    LF0CF                   ;2
       lda    $B5                     ;3
       cmp    $E8                     ;3
       bcc    LF047                   ;2
       lda    $E8                     ;3
       cmp    #$18                    ;2
       bne    LF051                   ;2
LF0A8:
       lda    $B4                     ;3
       and    #$07                    ;2
       tay                            ;2
       lda.wy $BB,Y                   ;4
       sbc    $BB,X                   ;4
       cmp    #$12                    ;2
       bcc    LF082                   ;2
LF0B6:
       ldy    #$00                    ;2
       beq    LF0D1                   ;2 always branch

LF0BA:
       lda    #$00                    ;2
       sta    $B8                     ;3
       lda    $B5                     ;3
       and    #$07                    ;2
       tax                            ;2
       lda    $B4                     ;3
       and    #$07                    ;2
       tay                            ;2
       lda    $BB,X                   ;4
       cmp.wy $BB,Y                   ;4
       bcc    LF0B6                   ;2
LF0CF:
       ldy    #$01                    ;2
LF0D1:
       lda.wy $B4,Y                   ;4
       and    #$07                    ;2
       tax                            ;2
       sty    $84                     ;3
       sec                            ;2
       lda.wy $82,Y                   ;4
       sbc    $BB,X                   ;4
       ldy    #$FF                    ;2
LF0E1:
       iny                            ;2
       sbc    #$05                    ;2
       bcs    LF0E1                   ;2
       sty    $B9                     ;3
       ldy    $84                     ;3
       sta    $84                     ;3
       eor    #$FF                    ;2
       adc    #$01                    ;2
       sta    $E4                     ;3
       bne    LF0F8                   ;2
       dec    $B9                     ;5
       sta    $84                     ;3
LF0F8:
       lda    $C9,X                   ;4
       and    #$70                    ;2
       ora    $E4                     ;3
       adc    LF9CE,Y                 ;4
       pha                            ;3
       lda    $BB,X                   ;4
       adc    $84                     ;3
       sta.wy $82,Y                   ;5
       lda    $B9                     ;3
       cmp    #$10                    ;2
       bcs    LF172                   ;2
LF10F:
       lda    $C2,X                   ;4
       sta    $84                     ;3
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       sec                            ;2
       adc    $84                     ;3
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       sta    $85                     ;3
       clc                            ;2
       adc    $84                     ;3
       eor    #$FF                    ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       ora    $85                     ;3
       clc                            ;2
       adc    #$80                    ;2
       tax                            ;2
       and    #$F0                    ;2
       ora    $B9                     ;3
       pha                            ;3
       txa                            ;2
       and    #$0F                    ;2
       asl                            ;2
       adc    LF968,Y                 ;4
       tax                            ;2
       inx                            ;2
       lda    LF903,X                 ;4
       pha                            ;3
       dex                            ;2
       lda    LF903,X                 ;4
       pha                            ;3
       dey                            ;2
       bmi    LF17B                   ;2
       lda    $83                     ;3
       cmp    $82                     ;3
       bcc    LF162                   ;2
       tsx                            ;2
       lda    #$04                    ;2
       sta    $84                     ;3
       inx                            ;2
LF155:
       lda    VSYNC,X                 ;4
       ldy    NUSIZ0,X                ;4
       sta    NUSIZ0,X                ;4
       sty    VSYNC,X                 ;4
       inx                            ;2
       dec    $84                     ;5
       bne    LF155                   ;2
LF162:
       ldx    $E5                     ;3
       lda    $B8                     ;3
       bne    LF16D                   ;2
       dec    $B8                     ;5
       jmp    LF0B6                   ;3

LF16D:
       bmi    LF18B                   ;2
       jmp    LF047                   ;3

LF172:
       sta.wy $DF,Y                   ;5
       lda    #$00                    ;2
       sta    $B9                     ;3
       beq    LF10F                   ;2 always branch

LF17B:
       ldx    $E5                     ;3
       lda    $B8                     ;3
       bne    LF186                   ;2
       dec    $B8                     ;5
       jmp    LF0CF                   ;3

LF186:
       bmi    LF18B                   ;2
       jmp    LF043                   ;3

LF18B:
       ldx    #$02                    ;2
LF18D:
       ldy    #$FF                    ;2
       sec                            ;2
       lda    $81,X                   ;4
LF192:
       iny                            ;2
       sbc    #$05                    ;2
       bcs    LF192                   ;2
       dey                            ;2
       sty    $B3,X                   ;4
       dex                            ;2
       bne    LF18D                   ;2
       stx    $82                     ;3
       stx    $83                     ;3
       stx    $84                     ;3
       stx    $85                     ;3
       stx    $B8                     ;3
       lda    #$0C                    ;2
       sta    $B9                     ;3
       lda    #$20                    ;2
       sta    $BA                     ;3
LF1AF:
       lda    INTIM                   ;4
       bmi    LF1AF                   ;2
       lda    #$40                    ;2
       sta    WSYNC                   ;3
       sta    VBLANK                  ;3
       jmp    LFD4D                   ;3

LF1BD:
       sta    WSYNC                   ;3
       ldx    #$00                    ;2
       stx    GRP0                    ;3
       stx    GRP1                    ;3
       stx    PF2                     ;3
       stx    COLUPF                  ;3
       lda    #$15                    ;2
       sta    CTRLPF                  ;3
       lda    $B7                     ;3
;  IF PAL
;       adc    #$98                    ;2
;  ELSE
;       adc    #$A8                    ;2
;  ENDIF

  IF PAL
       adc    #$68                    ;2 C6
  ELSE
       adc    #$A8                    ;2 76
  ENDIF

       sta    COLUP0                  ;3
       sta    COLUP1                  ;3
       lda    $D6                     ;3
       and    #$03                    ;2
       tay                            ;2
       sta    RESP0                   ;3
       sta    RESP1                   ;3
       lda    LF9D0,Y                 ;4
       sta    PF1                     ;3
       lda    #$10                    ;2
       sta    HMP0                    ;3
       asl                            ;2
       sta    RESM0                   ;3
       sta    HMP1                    ;3
       stx    HMM0                    ;3
;       stx    WSYNC                   ;3
;       stx    HMOVE                   ;3
       ldy    #$02                    ;2
LF1F4: ;convert digits to pointers
;       lda.wy $D9,Y                   ;4
;       pha                            ;3
;       and    #$F0                    ;2
;       lsr                            ;2
;       adc    #<Digit0                ;2
;       sta    $EE,X                   ;4
;       inx                            ;2
;       pla                            ;4
;       and    #$0F                    ;2
;       asl                            ;2
;       asl                            ;2
;       asl                            ;2
;       adc    #<Digit0                ;2
;       sta    $EE,X                   ;4
;       inx                            ;2
;       dey                            ;2
;       bpl    LF1F4                   ;2
       sty    $FD                     ;4
       lda.wy $D9,Y                   ;4
       pha                            ;3
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       tay                            ;2
       lda    DigitTbl,Y              ;4
       sta    $EE,X                   ;4
       inx                            ;2
       pla                            ;4
       and    #$0F                    ;2
       tay                            ;2
       lda    DigitTbl,Y              ;4
       sta    $EE,X                   ;4
       inx                            ;2
       ldy    $FD                     ;4
       dey                            ;2
       bpl    LF1F4                   ;2
;;       sta    WSYNC                   ;3
       ldy    #$0B                    ;2
LF212:
       dex                            ;2
;       lda    #$FA                    ;2
       lda    #>Digit0                ;2
       sta.wy $F4,Y                   ;5
       dey                            ;2
       lda    $EE,X                   ;4
       sta.wy $F4,Y                   ;5
       dey                            ;2
       bpl    LF212                   ;2
       ldy    #<DigitSpace            ;2
LF223:
       lda    $F4,X                   ;4
       cmp    #<Digit0                ;2
       bne    LF231                   ;2
       sty    $F4,X                   ;4
       inx                            ;2
       inx                            ;2
       cpx    #$0A                    ;2
       bne    LF223                   ;2
LF231:
       txa                            ;2
       asl                            ;2
       tax                            ;2
       inx                            ;2
       inx                            ;2
       cmp    #$16                    ;2
       bcc    LF231                   ;2
       ldx    #$03                    ;2
       stx    WSYNC                   ;3

       stx    HMOVE                   ;3


       stx    NUSIZ0                  ;3
       stx    NUSIZ1                  ;3
       stx    VDELP0                  ;3
       stx    VDELP1                  ;3
       txa                            ;2
       and    $D6                     ;3
       beq    LF24D                   ;2
       stx    ENAM0                   ;3
LF24D:
;       lda    #$06                    ;2
;       sta    $E4                     ;3
       ldy    #$06                    ;2
LF251:
;       ldy    $E4                     ;3
       sty    $E4                     ;3
       lda    ($F4),Y                 ;5
       sta    GRP0                    ;3
       sta    WSYNC                   ;3
       lda    ($F6),Y                 ;5
       sta    GRP1                    ;3
       lda    ($F8),Y                 ;5
       sta    GRP0                    ;3
       lda    ($FA),Y                 ;5
       sta    $84                     ;3
       lda    ($FC),Y                 ;5
       tax                            ;2
       lda    ($FE),Y                 ;5
       tay                            ;2
       lda    $84                     ;3
       sta    GRP1                    ;3
       stx    GRP0                    ;3
       sty    GRP1                    ;3
       sty    GRP0                    ;3
;       dec    $E4                     ;5
       ldy    $E4                     ;3
       dey                            ;2

       bpl    LF251                   ;2
;       lda    #$00                    ;2
       iny                            ;2

       ldx    #$01                    ;2
       stx    CTRLPF                  ;3
LF27F:
;       sta    NUSIZ0,X                ;4
;       sta    VDELP0,X                ;4
;       sta    GRP0,X                  ;4
       sty    NUSIZ0,X                ;4
       sty    VDELP0,X                ;4
       sty    GRP0,X                  ;4

       dex                            ;2
       bpl    LF27F                   ;2
;       sta    ENAM0                   ;3
;       sta    $F3                     ;3
       sty    ENAM0                   ;3
       sty    $F3                     ;3

LF28C:
       ldy    #$AC                    ;2
       sty    TIM64T                  ;4
       lda    $E1                     ;3
       sta    $DF                     ;3
       lda    #$F8                    ;2
       sta    $E0                     ;3
       lda    $D6                     ;3
       and    #$20                    ;2
       beq    LF2CC                   ;2
       ldx    $D7                     ;3
       dec    $DC                     ;5
       beq    LF2B6                   ;2
       lda    $DC                     ;3
       lsr                            ;2
       bcc    LF2D9                   ;2
       lda    $C9,X                   ;4
       and    #$08                    ;2
       bne    LF306                   ;2
       jsr    LF507                   ;6
       jmp    LF311                   ;3

LF2B6:

  IF AMIDAR_DS
       lda    #$04                    ;2
  ELSE
       lda    #$05                    ;2
  ENDIF

       sta    $DC                     ;3
       lda    $D6                     ;3
       lsr                            ;2
       lsr                            ;2
       and    #$07                    ;2
       tay                            ;2
       lda    LFA0B,Y                 ;4
       adc    $D8                     ;3
       sta    $D8                     ;3
       bcc    LF321                   ;2
       bcs    LF2D9                   ;2 always branch

LF2CC:
       jsr    LF760                   ;6
       lda    $D6                     ;3
       bpl    LF321                   ;2
       lda    $80                     ;3
       and    #$01                    ;2
       beq    LF321                   ;2
LF2D9:
       ldx    #$07                    ;2
LF2DB:
       dex                            ;2
       bmi    LF2FA                   ;2
       lda    $BB,X                   ;4
       cmp    #$FF                    ;2
       beq    LF2DB                   ;2
       cpx    $D7                     ;3
       beq    LF2DB                   ;2
       cpx    $D0                     ;3
       beq    LF2DB                   ;2
       jsr    LF408                   ;6
       jsr    LF495                   ;6
       beq    LF2DB                   ;2
       jsr    LF3BD                   ;6
       jmp    LF2DB                   ;3

LF2FA:
       jsr    LF383                   ;6
       jmp    LF321                   ;3

LF300:
       jsr    LF760                   ;6
       jmp    LF321                   ;3

LF306:
       jsr    LF5F2                   ;6
       ldx    $D7                     ;3
       lda    $C9,X                   ;4
       and    #$F7                    ;2
       sta    $C9,X                   ;4
LF311:
       lda    $DC                     ;3
       cmp    #$03                    ;2
       bne    LF300                   ;2
       lda    INTIM                   ;4
       cmp    #$A0                    ;2
       bcc    LF321                   ;2
       jsr    LF68D                   ;6
LF321:
       jsr    LF426                   ;6
LF324:
       lda    INTIM                   ;4
       bmi    LF324                   ;2
       sta    WSYNC                   ;3
       lda    $F3                     ;3
       clc                            ;2
       sed                            ;2
       adc    $D9                     ;3
       sta    $D9                     ;3
       lda    #$00                    ;2
       adc    $DA                     ;3
       sta    $DA                     ;3
       lda    #$00                    ;2
       adc    $DB                     ;3
       sta    $DB                     ;3
       cld                            ;2
       lda    #$02                    ;2
       sta    WSYNC                   ;3
       sta    VBLANK                  ;3
       sta    WSYNC                   ;3
       ldy    $E2                     ;3
       bmi    LF36A                   ;2
       bne    LF368                   ;2
       lda    ($DF),Y                 ;5
       beq    LF364                   ;2
       pha                            ;3
       and    #$03                    ;2
       tax                            ;2
       lda    LFFF8,X                 ;4
       sta    AUDC0                   ;3
       pla                            ;4
       lsr                            ;2
       lsr                            ;2
       sta    AUDF0                   ;3
       lda    #$04                    ;2
       sta    $E2                     ;3
LF364:
       sta    AUDV0                   ;3
       inc    $DF                     ;5
LF368:
       dec    $E2                     ;5
LF36A:
       sta    WSYNC                   ;3
       lda    $DF                     ;3
       sta    $E1                     ;3
       sta    WSYNC                   ;3
       lda    #$02                    ;2
       sta    VSYNC                   ;3
       sta    WSYNC                   ;3
       sta    WSYNC                   ;3
;       lda    #$00                    ;2
       lsr                            ;2
       sta    WSYNC                   ;3
       sta    VSYNC                   ;3
       jmp    LF024                   ;3

LF383:
       ldx    $D0                     ;3
       jsr    LF408                   ;6
       lda    $C9,X                   ;4
       and    #$04                    ;2
       beq    LF3A6                   ;2
       lda    $BB,X                   ;4
       cmp    #$07                    ;2
       beq    LF398                   ;2
       cmp    #$99                    ;2
       bne    LF3DF                   ;2
LF398:
       lda    $C2,X                   ;4
       cmp    #$06                    ;2
       beq    LF3D9                   ;2
LF39E:
       lda    $C9,X                   ;4
       and    #$FB                    ;2
       ora    #$80                    ;2
       bne    LF3BA                   ;2 always branch

LF3A6:
       lda    $C2,X                   ;4
       cmp    #$06                    ;2
       beq    LF3B0                   ;2
       cmp    #$82                    ;2
       bne    LF3DF                   ;2
LF3B0:
       lda    $BB,X                   ;4
       cmp    #$07                    ;2
       bne    LF400                   ;2
LF3B6:
       lda    #$84                    ;2
LF3B8:
       ora    $C9,X                   ;4
LF3BA:
       sta    $C9,X                   ;4
       rts                            ;6

LF3BD:
       cmp    #$03                    ;2
       beq    LF3E8                   ;2
       bcs    LF3B6                   ;2
       lda    $C2,X                   ;4
       cmp    #$06                    ;2
       beq    LF3D5                   ;2
       cmp    #$82                    ;2
       beq    LF3E0                   ;2
       lda    $C9,X                   ;4
       and    #$08                    ;2
       bne    LF39E                   ;2
       beq    LF3D9                   ;2 always branch

LF3D5:
       lda    #$73                    ;2
       bne    LF3DB                   ;2 always branch

LF3D9:
       lda    #$7B                    ;2
LF3DB:
       and    $C9,X                   ;4
LF3DD:
       sta    $C9,X                   ;4
LF3DF:
       rts                            ;6

LF3E0:
       lda    $C9,X                   ;4
       ora    #$08                    ;2
       sta    $C9,X                   ;4
       bne    LF39E                   ;2 always branch

LF3E8:
       cpx    $D7                     ;3
       beq    LF400                   ;2
       lda    $BB,X                   ;4
       cmp    #$43                    ;2
       beq    LF3F6                   ;2
       cmp    #$7F                    ;2
       bne    LF400                   ;2
LF3F6:
       lda    $C2,X                   ;4
       cmp    #$06                    ;2
       beq    LF3B6                   ;2
       cmp    #$82                    ;2
       beq    LF3B6                   ;2
LF400:
       lda    $C9,X                   ;4
       ora    #$04                    ;2
       and    #$7F                    ;2
       bne    LF3DD                   ;2
LF408:
       lda    $C9,X                   ;4

  IF AMIDAR_DS
       asl                            ;2
       and    #$08                    ;2
       bne    LF41A                   ;2
       lda    #$02                    ;2
       bcc    LF415                   ;2
       lda    #$FD                    ;2
LF415:
       adc    $C2,X                   ;4
       sta    $C2,X                   ;4
LF419:
       rts                            ;6

LF41A:
       lda    #$FE                    ;2
       bcc    LF420                   ;2
       lda    #$01                    ;2
LF420:
       adc    $BB,X                   ;4
       sta    $BB,X                   ;4
       rts                            ;6
       nop                            ;2
  ELSE
       and    #$04                    ;2
       bne    LF41A                   ;2
       lda    $C9,X                   ;4
       and    #$80                    ;2
       beq    LF417                   ;2
       dec    $C2,X                   ;6
       rts                            ;6

LF417:
       inc    $C2,X                   ;6
LF419:
       rts                            ;6

LF41A:
       lda    $C9,X                   ;4
       and    #$80                    ;2
       beq    LF423                   ;2
       inc    $BB,X                   ;6
       rts                            ;6

LF423:
       dec    $BB,X                   ;6
       rts                            ;6
  ENDIF

LF426:
       lda    #$00                    ;2
       sta    $84                     ;3
       sta    $EB                     ;3
LF42C:
       ldx    #$07                    ;2
       ldy    #$06                    ;2
LF430:
       dex                            ;2
       dey                            ;2
       bmi    LF489                   ;2
       cpx    $84                     ;3
       beq    LF419                   ;2
       lda    $BB,X                   ;4
       cmp.wy $BB,Y                   ;4
       bcs    LF430                   ;2
       lda    $BB,X                   ;4
       sta    $E4                     ;3
       lda.wy $BB,Y                   ;4
       sta    $BB,X                   ;4
       lda    $E4                     ;3
       sta.wy $BB,Y                   ;5
       lda    $C2,X                   ;4
       sta    $E4                     ;3
       lda.wy $C2,Y                   ;4
       sta    $C2,X                   ;4
       lda    $E4                     ;3
       sta.wy $C2,Y                   ;5
       lda    $C9,X                   ;4
       sta    $E4                     ;3
       lda.wy $C9,Y                   ;4
       sta    $C9,X                   ;4
       lda    $E4                     ;3
       sta.wy $C9,Y                   ;5
       cpx    $D7                     ;3
       bne    LF471                   ;2
       sty    $D7                     ;3
       beq    LF47F                   ;2 always branch

LF471:
       cpy    $D7                     ;3
       bne    LF477                   ;2
       stx    $D7                     ;3
LF477:
       cpx    $D0                     ;3
       bne    LF47F                   ;2
       sty    $D0                     ;3
       beq    LF485                   ;2 always branch

LF47F:
       cpy    $D0                     ;3
       bne    LF485                   ;2
       stx    $D0                     ;3
LF485:
       inc    $EB                     ;5
       bne    LF430                   ;2
LF489:
       lda    $EB                     ;3
       beq    LF419                   ;2
       inc    $84                     ;5
       lda    #$00                    ;2
       sta    $EB                     ;3
       beq    LF42C                   ;2 always branch

LF495:
       lda    $BB,X                   ;4
       clc                            ;2
       adc    #$09                    ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       tay                            ;2
       lda    $C9,X                   ;4
       and    #$04                    ;2
       beq    LF4B3                   ;2
       lda    LFA05,Y                 ;4
       cmp    $BB,X                   ;4
       beq    LF4B0                   ;2
LF4AD:
       lda    #$00                    ;2
       rts                            ;6

LF4B0:
       lda    #$02                    ;2
       rts                            ;6

LF4B3:
       tya                            ;2
       asl                            ;2
       asl                            ;2
       tay                            ;2
       lda    $C2,X                   ;4
       cmp    #$26                    ;2
       bcc    LF4D2                   ;2
       cmp    #$46                    ;2
       bcc    LF4D6                   ;2
       cmp    #$66                    ;2
       bcc    LF4CC                   ;2
       lda    #$62                    ;2
       iny                            ;2
       iny                            ;2
       iny                            ;2
       bne    LF4D9                   ;2 always branch?
LF4CC:
       iny                            ;2
       iny                            ;2
       lda    #$42                    ;2
       bne    LF4D9                   ;2 always branch

LF4D2:
       lda    #$02                    ;2
       bne    LF4D9                   ;2 always branch

LF4D6:
       lda    #$22                    ;2
       iny                            ;2
LF4D9:
       sta    $BA                     ;3
       lda    LFA67,Y                 ;4
       sta    $84                     ;3
       lda    LFA6B,Y                 ;4
       sta    $85                     ;3
       lda    $BA                     ;3
       ldy    #$09                    ;2
LF4E9:
       dey                            ;2
       beq    LF4AD                   ;2
       clc                            ;2
       adc    #$04                    ;2
       ror    $84                     ;5
       bcs    LF4FE                   ;2
       ror    $85                     ;5
       bcc    LF4E9                   ;2
       cmp    $C2,X                   ;4
       bne    LF4E9                   ;2
       lda    #$04                    ;2
LF4FD:
       rts                            ;6

LF4FE:
       ror    $85                     ;5
       cmp    $C2,X                   ;4
       bne    LF4E9                   ;2
       lda    #$03                    ;2
       rts                            ;6

LF507:
       jsr    LF5B9                   ;6
       bcc    LF4FD                   ;2
       lda    $F0                     ;3
       cmp    $D5                     ;3
       bne    LF51F                   ;2
       lda    $EB                     ;3
       sta    $D5                     ;3
       lda    $C9,X                   ;4
       eor    #$80                    ;2
       sta    $C9,X                   ;4
       jmp    LF408                   ;3

LF51F:
       jsr    LF495                   ;6
       bne    LF531                   ;2
       jsr    LF408                   ;6
       jsr    LF495                   ;6
       beq    LF4FD                   ;2
       lda    #$08                    ;2
       jmp    LF3B8                   ;3

LF531:
       sta    $84                     ;3
       lda    $EB                     ;3
       cmp    $D5                     ;3
       bne    LF573                   ;2
       cmp    #$40                    ;2
       bcs    LF5A7                   ;2
       lda    $C2,X                   ;4
       cmp    #$06                    ;2
       beq    LF547                   ;2
       cmp    #$82                    ;2
       bne    LF551                   ;2
LF547:
       lda    $BB,X                   ;4
       cmp    #$07                    ;2
       beq    LF551                   ;2
       cmp    #$99                    ;2
       bne    LF5B6                   ;2
LF551:
       rts                            ;6

LF552:
       tay                            ;2
       lda    LF9C5,Y                 ;4
       cmp    $EB                     ;3
       beq    LF570                   ;2
       lda    $BB,X                   ;4
       cmp    #$99                    ;2
       beq    LF5A7                   ;2
       cmp    #$07                    ;2
       beq    LF5A7                   ;2
       lda    $C2,X                   ;4
       cmp    #$82                    ;2
       beq    LF56E                   ;2
       cmp    #$06                    ;2
       bne    LF5A7                   ;2
LF56E:
       ldy    #$04                    ;2
LF570:
       tya                            ;2
       bne    LF598                   ;2
LF573:
       lda    $84                     ;3
       cmp    #$02                    ;2
       bne    LF552                   ;2
       lda    $EB                     ;3
       and    #$80                    ;2
       beq    LF589                   ;2
       lda    $C9,X                   ;4
       and    #$F7                    ;2
       sta    $C9,X                   ;4
       ldy    #$82                    ;2
       bne    LF591                   ;2 always branch

LF589:
       ldy    #$06                    ;2
       lda    $C9,X                   ;4
       ora    #$08                    ;2
       sta    $C9,X                   ;4
LF591:
       tya                            ;2
       cmp    $C2,X                   ;4
       beq    LF551                   ;2
       lda    #$02                    ;2
LF598:
       jsr    LF3BD                   ;6
       lda    $C9,X                   ;4
       eor    #$80                    ;2
       sta    $C9,X                   ;4
       lda    $F0                     ;3
       sta    $D5                     ;3
       bne    LF551                   ;2 always branch?
LF5A7:
       ldy    #$82                    ;2
       lda    $C9,X                   ;4
       and    #$80                    ;2
       beq    LF5B1                   ;2
       ldy    #$06                    ;2
LF5B1:
       tya                            ;2
       cmp    $C2,X                   ;4
       beq    LF551                   ;2
LF5B6:
       jmp    LF408                   ;3

LF5B9:
       ldy    #$30                    ;2
       sty    $85                     ;3
       clc                            ;2
       ldy    #$C0                    ;2
       lda    $C9,X                   ;4
       and    #$04                    ;2
       beq    LF5CA                   ;2
       sty    $85                     ;3
       ldy    #$30                    ;2
LF5CA:
       sty    $84                     ;3
       lda    SWCHA                   ;4
       eor    #$FF                    ;2
       and    #$F0                    ;2
       beq    LF5EA                   ;2
       sta    $EB                     ;3
       and    $84                     ;3
       beq    LF5E3                   ;2
       sta    $EB                     ;3
       eor    $84                     ;3
       bne    LF5E7                   ;2
       sec                            ;2
       rts                            ;6

LF5E3:
       lda    $EB                     ;3
       eor    $85                     ;3
LF5E7:
       sta    $F0                     ;3
       sec                            ;2
LF5EA:
       rts                            ;6

LF5EB:
       rol    $D4                     ;5
       tya                            ;2
       sta    $E5                     ;3
       bne    LF629                   ;2 always branch?
LF5F2:
       lda    $D3                     ;3
       tay                            ;2
       sec                            ;2
       lda    $C2,X                   ;4
       sbc    $D1                     ;3
       beq    LF659                   ;2
       lda    #$FC                    ;2
       sta    $E4                     ;3
       lda    #$FF                    ;2
       sta    $84                     ;3
       bcs    LF60D                   ;2
       tya                            ;2
       and    #$01                    ;2
       bne    LF61A                   ;2
       beq    LF630                   ;2 always branch

LF60D:
       lda    #$04                    ;2
       sta    $E4                     ;3
       lda    #$01                    ;2
       sta    $84                     ;3
       tya                            ;2
       and    #$01                    ;2
       bne    LF630                   ;2
LF61A:
       asl    $D4                     ;5
       bcs    LF622                   ;2
       sty    $E5                     ;3
       bcc    LF638                   ;2 always branch

LF622:
       ror    $D4                     ;5
       lda    #$00                    ;2
       sta    $E5                     ;3
       tya                            ;2
LF629:
       adc    $84                     ;3
       sta    $D3                     ;3
       tay                            ;2
       bne    LF638                   ;2
LF630:
       lsr    $D4                     ;5
       bcs    LF5EB                   ;2
       lda    #$00                    ;2
       sta    $E5                     ;3
LF638:
       lda    ($81),Y                 ;5
       ora    $D4                     ;3
       cmp    ($81),Y                 ;5
       beq    LF648                   ;2
       inc    $F3                     ;5
       sta    ($81),Y                 ;6
       lda    #$D6                    ;2
       brk                            ;7
       nop                            ;2
LF648:
       clc                            ;2
       lda    $D1                     ;3
       adc    $E4                     ;3
       sta    $D1                     ;3
       cmp    $C2,X                   ;4
       beq    LF68C                   ;2
       lda    $E5                     ;3
       beq    LF630                   ;2
       bne    LF61A                   ;2 always branch

LF659:
       lda    $BB,X                   ;4
       ldx    #$01                    ;2
       cmp    $D2                     ;3
       beq    LF68C                   ;2
       bcc    LF664                   ;2
       dex                            ;2
LF664:
       sta    $D2                     ;3
       lda    $81                     ;3
       clc                            ;2
       adc    LF9CC,X                 ;4
       sta    $81                     ;3
       lda    ($81),Y                 ;5
       ora    $D4                     ;3
       cmp    ($81),Y                 ;5
       beq    LF67E                   ;2
       inc    $F3                     ;5
       sta    ($81),Y                 ;6
       lda    #$D6                    ;2
       brk                            ;7
       nop                            ;2
LF67E:
       lda    $81                     ;3
       clc                            ;2
       adc    LF9CC,X                 ;4
       sta    $81                     ;3
       lda    ($81),Y                 ;5
       ora    $D4                     ;3
       sta    ($81),Y                 ;6
LF68C:
       rts                            ;6

LF68D:
       dec    $DD                     ;5
       bpl    LF695                   ;2
       lda    #$04                    ;2
       sta    $DD                     ;3
LF695:
       lda    $DD                     ;3
       asl                            ;2
       tay                            ;2
       ldx    LF9BC,Y                 ;4
       stx    $ED                     ;3
       stx    $F0                     ;3
       ldx    LF9BD,Y                 ;4
       stx    $EE                     ;3
       stx    $EF                     ;3
       asl                            ;2
       asl                            ;2
       adc    #$09                    ;2
       tay                            ;2
       ldx    #$05                    ;2
       stx    $84                     ;3
       dex                            ;2
LF6B1:
       lda.wy $84,Y                   ;4
       sta    $E4,X                   ;4
       dey                            ;2
       stx    $EB                     ;3
       dex                            ;2
       bpl    LF6B1                   ;2
       dey                            ;2
LF6BD:
       lda    #$80                    ;2
       bne    LF6CB                   ;2 always branch

LF6C1:
       lda    #$00                    ;2
       sta    $EB                     ;3
       lda    $EA                     ;3
       bpl    LF6BD                   ;2
LF6C9:
       lda    #$01                    ;2
LF6CB:
       sta    $EA                     ;3
       iny                            ;2
       inx                            ;2
       dec    $84                     ;5
       beq    LF68C                   ;2
       lda.wy $86,Y                   ;4
       cmp    #$FF                    ;2
       beq    LF6C1                   ;2
       and    $ED,X                   ;4
       beq    LF6C1                   ;2
       lda    $EB                     ;3
       bne    LF71B                   ;2
LF6E2:
       lda    $ED,X                   ;4
       and    $EA                     ;3
       beq    LF6FD                   ;2
       and.wy $86,Y                   ;4
       beq    LF6FD                   ;2
       bne    LF751                   ;2 always branch

LF6EF:
       lda.wy $85,Y                   ;4
       sta    $E4,X                   ;4
       lda.wy $86,Y                   ;4
       sta    $E5,X                   ;4
       lda    #$00                    ;2
       sta    $EB                     ;3
LF6FD:
       tya                            ;2
       lsr                            ;2
       bcs    LF707                   ;2
       lsr    $EA                     ;5
       bcc    LF6E2                   ;2
       bcs    LF6C9                   ;2 always branch

LF707:
       asl    $EA                     ;5
       bcc    LF6E2                   ;2
       bcs    LF6BD                   ;2 always branch

LF70D:
       tya                            ;2
       lsr                            ;2
       bcs    LF717                   ;2
       lsr    $EA                     ;5
       bcc    LF71B                   ;2
       bcs    LF6C9                   ;2 always branch

LF717:
       asl    $EA                     ;5
       bcs    LF6BD                   ;2
LF71B:
       lda    $EA                     ;3
       and.wy $8A,Y                   ;4
       and.wy $82,Y                   ;4
       beq    LF6EF                   ;2
       and    $ED,X                   ;4
       bne    LF738                   ;2
       lda    $EA                     ;3
       and.wy $86,Y                   ;4
       bne    LF6EF                   ;2
       lda    $EA                     ;3
       ora    $E5,X                   ;4
       sta    $E5,X                   ;4
       bne    LF70D                   ;2 always branch

LF738:
       and.wy $86,Y                   ;4
       beq    LF6EF                   ;2
       lda    $E4,X                   ;4
       sta.wy $85,Y                   ;5
       lda    $E5,X                   ;4
       sta.wy $86,Y                   ;5
       lda    #$D2                    ;2
       brk                            ;7
       nop                            ;2
       lda    #$48                    ;2
       sta    $F3                     ;3
       bne    LF70D                   ;2 always branch

LF751:
       sta    $EB                     ;3
       lda.wy $85,Y                   ;4
       sta    $E4,X                   ;4
       lda.wy $86,Y                   ;4
       sta    $E5,X                   ;4
       jmp    LF70D                   ;3

LF760:
       lda    SWCHB                   ;4
       and    #$01                    ;2
       bne    LF76A                   ;2
       jmp    LF000                   ;3 reboot game

LF76A:
       lda    $D6                     ;3
       and    #$20                    ;2
       bne    LF78A                   ;2
       dec    $DE                     ;5
       bne    LF789                   ;2
       lda    $D6                     ;3
       and    #$03                    ;2
       bne    LF783                   ;2
       lda    #$80                    ;2
       sta    $D6                     ;3
       ldx    $D7                     ;3
       jmp    LF9B1                   ;3

LF783:
       lda    $D6                     ;3
       ora    #$20                    ;2
       sta    $D6                     ;3
LF789:
       rts                            ;6

LF78A:
       lda    INPT4                   ;3
       bmi    LF791                   ;2
       jmp    LF8B3                   ;3

LF791:
       ldx    #$27                    ;2
       lda    #$FF                    ;2
LF795:
       and    $8A,X                   ;4
       dex                            ;2
       bpl    LF795                   ;2
       cmp    #$FF                    ;2
       beq    LF7A1                   ;2
       jmp    LF82A                   ;3

LF7A1:
       ldy    $D6                     ;3
       iny                            ;2
       tya                            ;2
       and    #$03                    ;2
       bne    LF7AB                   ;2
       lda    #$03                    ;2
LF7AB:
       sta    $84                     ;3
       lda    $D6                     ;3
       and    #$FC                    ;2
       ora    $84                     ;3
       sta    $D6                     ;3
       lda    #$00                    ;2
       sta    $E3                     ;3
       ldx    #$2F                    ;2
LF7BB:
       sta    $82,X                   ;4
       dex                            ;2
       bpl    LF7BB                   ;2
       clc                            ;2
       lda    $D6                     ;3
       adc    #$04                    ;2
       and    #$1C                    ;2
       bne    LF7CB                   ;2
       lda    #$18                    ;2
LF7CB:
       sta    $84                     ;3
       lda    $D6                     ;3
       and    #$E3                    ;2
       ora    $84                     ;3
       sta    $D6                     ;3
LF7D5:
       lda    #$00                    ;2
       sta    $DE                     ;3
       lda    $D6                     ;3
       and    #$DF                    ;2
       sta    $D6                     ;3
LF7DF:
       ldy    #$24                    ;2
       lda    $D6                     ;3
       and    #$04                    ;2
       beq    LF7E9                   ;2
  IF PAL
       ldy    #$34                    ;2
  ELSE
       ldy    #$D4                    ;2
  ENDIF
LF7E9:
       sty    $B6                     ;3
  IF PAL
       ldy    #$C6                    ;2
  ELSE
       ldy    #$76                    ;2
  ENDIF
       sty    $B7                     ;3
       ldx    #$1B                    ;2
LF7F1:
       lda    LF9E9,X                 ;4
       sta    $BA,X                   ;4
       dex                            ;2
       bne    LF7F1                   ;2
       stx    $D7                     ;3
       lda    #$9D                    ;2
       sta    $81                     ;3
       lda    #$80                    ;2
       ora    $9D                     ;3
       sta    $9D                     ;3
       ora    $A1                     ;3
       sta    $A1                     ;3
       lda    $D6                     ;3
       and    #$1C                    ;2
       cmp    #$08                    ;2
       bcs    LF814                   ;2
       dex                            ;2
       stx    $C0                     ;3
LF814:
       lda    $D6                     ;3
       and    #$04                    ;2
       asl                            ;2
       asl                            ;2
       sta    $84                     ;3
       ldx    #$06                    ;2
LF81E:
       lda    $C9,X                   ;4
       and    #$AF                    ;2
       ora    $84                     ;3
       sta    $C9,X                   ;4
       dex                            ;2
       bpl    LF81E                   ;2
       rts                            ;6

LF82A:
       lda    $8A                     ;3
       and    $8D                     ;3
       and    $AA                     ;3
       and    $AD                     ;3
       and    #$FE                    ;2
       cmp    #$FE                    ;2
       bne    LF85D                   ;2
       lda    $E3                     ;3
       bmi    LF84F                   ;2
       bne    LF85D                   ;2
       lda    #$FF                    ;2
       sta    $E3                     ;3
       lda    $DE                     ;3
       and    #$F0                    ;2
       sta    $DE                     ;3
       lda    #$70                    ;2
       sta    $84                     ;3
       jmp    LF8E9                   ;3

LF84F:
       dec    $E3                     ;5
LF851:
       bpl    LF814                   ;2
       lda    $E3                     ;3
       cmp    #$90                    ;2
       bne    LF85D                   ;2
       lda    #$DA                    ;2
       brk                            ;7
       nop                            ;2
LF85D:
       ldx    #$07                    ;2
       ldy    $D7                     ;3
       sec                            ;2
LF862:
       dex                            ;2
       bmi    LF8A3                   ;2
       cpx    $D7                     ;3
       beq    LF862                   ;2
       lda.wy $BB,Y                   ;4
       sbc    $BB,X                   ;4
       bcs    LF874                   ;2
       eor    #$FF                    ;2
       adc    #$01                    ;2
LF874:
       cmp    #$06                    ;2
       bcs    LF862                   ;2
       lda.wy $C2,Y                   ;4
       sbc    $C2,X                   ;4
       bcs    LF883                   ;2
       eor    #$FF                    ;2
       adc    #$01                    ;2
LF883:
       cmp    #$06                    ;2
       bcs    LF862                   ;2
       lda    $C9,X                   ;4
       and    #$70                    ;2
       cmp    #$60                    ;2
       beq    LF8A3                   ;2
       cmp    #$70                    ;2
       bne    LF8AA                   ;2
       lda    $C9,X                   ;4
       and    #$8F                    ;2
       ora    #$60                    ;2
       sta    $C9,X                   ;4
       lda    #$99                    ;2
       sta    $F3                     ;3
       lda    #$CE                    ;2
       brk                            ;7
       nop                            ;2
LF8A3:
       lda    $DE                     ;3
       and    #$0F                    ;2
       bne    LF8BD                   ;2
LF8A9:
       rts                            ;6

LF8AA:
       dec    $D6                     ;5
       lda    #$CE                    ;2
       brk                            ;7
       nop                            ;2

    IF PLUSROM = 1

       jmp    SendPlusROMScore        ;3

    ELSE

       jmp    LF7D5                   ;3

    ENDIF

LF8B3:
       lda    $E3                     ;3
       bmi    LF8CB                   ;2
       lda    $DE                     ;3
       and    #$0F                    ;2
       beq    LF8C7                   ;2
LF8BD:
       dec    $DE                     ;5
       lda    $DE                     ;3
       and    #$0F                    ;2
       bne    LF8A9                   ;2
       beq    LF851                   ;2 always branch

LF8C7:
       lda    $DE                     ;3
       bpl    LF8E1                   ;2
LF8CB:
       jmp    LF791                   ;3

    IF PLUSROM = 1

BRK_ROUTINE:
       sta    $DF                     ;3
       lda    #$00                    ;2
       sta    $E2                     ;3
       rti                            ;6

    ENDIF

       ORG $F8CE

       .byte $72 ; | XXX  X | $F8CE
       .byte $42 ; | X    X | $F8CF
       .byte $4E ; | X  XXX | $F8D0
       .byte $00 ; |        | $F8D1
       .byte $28 ; |  X X   | $F8D2
       .byte $38 ; |  XXX   | $F8D3
       .byte $54 ; | X X X  | $F8D4
       .byte $00 ; |        | $F8D5
       .byte $2C ; |  X XX  | $F8D6
       .byte $38 ; |  XXX   | $F8D7
       .byte $40 ; | X      | $F8D8
       .byte $00 ; |        | $F8D9
       .byte $3D ; |  XXXX X| $F8DA
       .byte $25 ; |  X  X X| $F8DB
       .byte $15 ; |   X X X| $F8DC
       .byte $39 ; |  XXX  X| $F8DD
       .byte $2D ; |  X XX X| $F8DE
       .byte $11 ; |   X   X| $F8DF
       .byte $00 ; |        | $F8E0


LF8E1:
       adc    #$2E                    ;2
       sta    $DE                     ;3
       lda    #$60                    ;2
       sta    $84                     ;3
LF8E9:
       ldx    #$06                    ;2
LF8EB:
       cpx    $D7                     ;3
       beq    LF8F7                   ;2
       lda    $C9,X                   ;4
       and    #$8F                    ;2
       ora    $84                     ;3
       sta    $C9,X                   ;4
LF8F7:
       dex                            ;2
       bpl    LF8EB                   ;2
       rts                            ;6

  IF AMIDAR_DS
       .byte $4A ; | X  X X | $F8FB
       .byte $54 ; | X X X  | $F8FC
       .byte $5A ; | X XX X | $F8FD
       .byte $30 ; |  XX    | $F8FE
       .byte $32 ; |  XX  X | $F8FF
  ELSE
       .byte $00 ; |        | $F8FB
       .byte $00 ; |        | $F8FC
       .byte $00 ; |        | $F8FD
       .byte $00 ; |        | $F8FE
       .byte $00 ; |        | $F8FF
  ENDIF

       .byte $00 ; |        | $F900
       .byte $00 ; |        | $F901
       .byte $70 ; | XXX    | $F902


LF903:
;       .byte $82 ; |X     X | $F903
;       .byte $FA ; |XXXXX X | $F904
;       .byte $AB ; |X X X XX| $F905
;       .byte $FA ; |XXXXX X | $F906
;       .byte $CA ; |XX  X X | $F907
;       .byte $FA ; |XXXXX X | $F908
;       .byte $F8 ; |XXXXX   | $F909
;       .byte $FA ; |XXXXX X | $F90A
;       .byte $21 ; |  X    X| $F90B
;       .byte $FB ; |XXXXX XX| $F90C
;       .byte $48 ; | X  X   | $F90D
;       .byte $FB ; |XXXXX XX| $F90E
;       .byte $70 ; | XXX    | $F90F
;       .byte $FB ; |XXXXX XX| $F910
;       .byte $97 ; |X  X XXX| $F911
;       .byte $FB ; |XXXXX XX| $F912
;       .byte $BF ; |X XXXXXX| $F913
;       .byte $FB ; |XXXXX XX| $F914
;       .byte $EA ; |XXX X X | $F915
;       .byte $FB ; |XXXXX XX| $F916
;       .byte $10 ; |   X    | $F917
;       .byte $FC ; |XXXXXX  | $F918
;       .byte $2F ; |  X XXXX| $F919
;       .byte $FC ; |XXXXXX  | $F91A
;       .byte $5D ; | X XXX X| $F91B
;       .byte $FC ; |XXXXXX  | $F91C
;       .byte $86 ; |X    XX | $F91D
;       .byte $FC ; |XXXXXX  | $F91E
;       .byte $AD ; |X X XX X| $F91F
;       .byte $FC ; |XXXXXX  | $F920
;       .byte $D8 ; |XX XX   | $F921
;       .byte $FC ; |XXXXXX  | $F922
;       .byte $FC ; |XXXXXX  | $F923
;       .byte $FC ; |XXXXXX  | $F924
;       .byte $24 ; |  X  X  | $F925
;       .byte $FD ; |XXXXXX X| $F926

       .word IFA83-1 ; $F903/4
       .word IFAAC-1 ; $F905/6
       .word IFACB-1 ; $F907/8
       .word IFAF9-1 ; $F909/A
       .word IFB22-1 ; $F90B/C
       .word IFB49-1 ; $F90D/E
       .word IFB71-1 ; $F90F/10
       .word IFB98-1 ; $F911/2
       .word IFBC0-1 ; $F913/4
       .word IFBEB-1 ; $F915/6
       .word IFC11-1 ; $F917/8
       .word IFC30-1 ; $F919/A
       .word IFC5E-1 ; $F91B/C
       .word IFC87-1 ; $F91D/E
       .word IFCAE-1 ; $F91F/20
       .word IFCD9-1 ; $F921/2
       .word IFCFD-1 ; $F923/4
       .word IFD25-1 ; $F925/6

LF927:
       .byte $00 ; |        | $F927
       .byte $00 ; |        | $F928
       .byte $0A ; |    X X | $F929
       .byte $0A ; |    X X | $F92A
       .byte $0A ; |    X X | $F92B
       .byte $0A ; |    X X | $F92C
       .byte $0A ; |    X X | $F92D
       .byte $00 ; |        | $F92E
       .byte $08 ; |    X   | $F92F
       .byte $08 ; |    X   | $F930
       .byte $08 ; |    X   | $F931
       .byte $08 ; |    X   | $F932
       .byte $08 ; |    X   | $F933
       .byte $00 ; |        | $F934
       .byte $06 ; |     XX | $F935
       .byte $06 ; |     XX | $F936
       .byte $06 ; |     XX | $F937
       .byte $06 ; |     XX | $F938
       .byte $06 ; |     XX | $F939
       .byte $00 ; |        | $F93A
       .byte $04 ; |     X  | $F93B
       .byte $04 ; |     X  | $F93C
       .byte $04 ; |     X  | $F93D
       .byte $04 ; |     X  | $F93E
       .byte $04 ; |     X  | $F93F
       .byte $00 ; |        | $F940
       .byte $02 ; |      X | $F941
       .byte $02 ; |      X | $F942
       .byte $02 ; |      X | $F943
       .byte $02 ; |      X | $F944
       .byte $02 ; |      X | $F945
       .byte $00 ; |        | $F946
       .byte $0C ; |    XX  | $F947
LF948:
       .byte $00 ; |        | $F948
       .byte $2C ; |  X XX  | $F949
       .byte $28 ; |  X X   | $F94A
       .byte $28 ; |  X X   | $F94B
       .byte $28 ; |  X X   | $F94C
       .byte $28 ; |  X X   | $F94D
       .byte $28 ; |  X X   | $F94E
       .byte $24 ; |  X  X  | $F94F
       .byte $20 ; |  X     | $F950
       .byte $20 ; |  X     | $F951
       .byte $20 ; |  X     | $F952
       .byte $20 ; |  X     | $F953
       .byte $20 ; |  X     | $F954
       .byte $1C ; |   XXX  | $F955
       .byte $18 ; |   XX   | $F956
       .byte $18 ; |   XX   | $F957
       .byte $18 ; |   XX   | $F958
       .byte $18 ; |   XX   | $F959
       .byte $18 ; |   XX   | $F95A
       .byte $14 ; |   X X  | $F95B
       .byte $10 ; |   X    | $F95C
       .byte $10 ; |   X    | $F95D
       .byte $10 ; |   X    | $F95E
       .byte $10 ; |   X    | $F95F
       .byte $10 ; |   X    | $F960


       .byte $0C ; |    XX  | $F961
       .byte $08 ; |    X   | $F962
       .byte $08 ; |    X   | $F963
       .byte $08 ; |    X   | $F964
       .byte $08 ; |    X   | $F965
       .byte $08 ; |    X   | $F966
       .byte $04 ; |     X  | $F967
LF968:
       .byte $00 ; |        | $F968
       .byte $12 ; |   X  X | $F969
       .byte $FF ; |XXXXXXXX| $F96A
LF96B:
       .byte $00 ; |        | $F96B
       .byte $5D ; | X XXX X| $F96C
       .byte $5D ; | X XXX X| $F96D
       .byte $5D ; | X XXX X| $F96E
       .byte $7F ; | XXXXXXX| $F96F
       .byte $6B ; | XX X XX| $F970
       .byte $3E ; |  XXXXX | $F971
       .byte $1C ; |   XXX  | $F972
       .byte $00 ; |        | $F973
       .byte $00 ; |        | $F974
       .byte $00 ; |        | $F975
       .byte $00 ; |        | $F976
       .byte $00 ; |        | $F977
       .byte $00 ; |        | $F978
  IF PAL
       .byte $2E ; |  X XXX | $F979
  ELSE
       .byte $1E ; |   XXXX | $F979
  ENDIF
LF97A:
       .byte $FF ; |XXXXXXXX| $F97A
       .byte $00 ; |        | $F97B
       .byte $02 ; |      X | $F97C
       .byte $02 ; |      X | $F97D
       .byte $02 ; |      X | $F97E
       .byte $7E ; | XXXXXX | $F97F
       .byte $40 ; | X      | $F980
       .byte $7F ; | XXXXXXX| $F981
       .byte $7F ; | XXXXXXX| $F982
       .byte $00 ; |        | $F983
       .byte $00 ; |        | $F984
       .byte $00 ; |        | $F985
       .byte $00 ; |        | $F986
       .byte $00 ; |        | $F987
       .byte $00 ; |        | $F988
  IF PAL
       .byte $2E ; |  X XXX | $F989
  ELSE
       .byte $1E ; |   XXXX | $F989
  ENDIF

       .byte $FF ; |XXXXXXXX| $F98A
       .byte $00 ; |        | $F98B
       .byte $36 ; |  XX XX | $F98C
       .byte $14 ; |   X X  | $F98D
       .byte $7F ; | XXXXXXX| $F98E
       .byte $7F ; | XXXXXXX| $F98F
       .byte $2A ; |  X X X | $F990
       .byte $1C ; |   XXX  | $F991
       .byte $3E ; |  XXXXX | $F992
       .byte $00 ; |        | $F993
       .byte $00 ; |        | $F994
       .byte $00 ; |        | $F995
       .byte $00 ; |        | $F996
       .byte $00 ; |        | $F997
       .byte $00 ; |        | $F998
  IF PAL
       .byte $38 ; |  XXX   | $F999
  ELSE
       .byte $D8 ; |XX XX   | $F999
  ENDIF

       .byte $FF ; |XXXXXXXX| $F99A
       .byte $00 ; |        | $F99B
       .byte $24 ; |  X  X  | $F99C
       .byte $7E ; | XXXXXX | $F99D
       .byte $7F ; | XXXXXXX| $F99E
       .byte $7F ; | XXXXXXX| $F99F
       .byte $7A ; | XXXX X | $F9A0
       .byte $7C ; | XXXXX  | $F9A1
       .byte $04 ; |     X  | $F9A2
       .byte $00 ; |        | $F9A3
       .byte $00 ; |        | $F9A4
       .byte $00 ; |        | $F9A5
       .byte $00 ; |        | $F9A6
       .byte $00 ; |        | $F9A7
       .byte $00 ; |        | $F9A8
  IF PAL
       .byte $68 ; | XX X   | $F9A9
  ELSE
       .byte $48 ; | X  X   | $F9A9
  ENDIF

;free
       .byte $00 ; |        | $F9A8
       .byte $00 ; |        | $F9A8
       .byte $00 ; |        | $F9A8

LFFF8:
       .byte $0C ; |    XX  | $FFF8
       .byte $04 ; |     X  | $FFF9
       .byte $08 ; |    X   | $FFFA
       .byte $09 ; |    X  X| $FFFB




      ds 9, 0




LF9BA:
       .byte $FF ; |XXXXXXXX| $F9BA
LF9BB:
       .byte $FF ; |XXXXXXXX| $F9BB
LF9BC:
       .byte $82 ; |X     X | $F9BC
LF9BD:
       .byte $44 ; | X   X  | $F9BD
       .byte $88 ; |X   X   | $F9BE
       .byte $12 ; |   X  X | $F9BF
       .byte $90 ; |X  X    | $F9C0
       .byte $08 ; |    X   | $F9C1
       .byte $84 ; |X    X  | $F9C2
       .byte $40 ; | X      | $F9C3
       .byte $82 ; |X     X | $F9C4
LF9C5:
       .byte $10 ; |   X    | $F9C5
       .byte $00 ; |        | $F9C6
       .byte $00 ; |        | $F9C7
       .byte $10 ; |   X    | $F9C8
       .byte $20 ; |  X     | $F9C9
       .byte $FF ; |XXXXXXXX| $F9CA
       .byte $00 ; |        | $F9CB
LF9CC:
       .byte $04 ; |     X  | $F9CC
       .byte $FC ; |XXXXXX  | $F9CD
LF9CE:
       .byte $09 ; |    X  X| $F9CE
       .byte $08 ; |    X   | $F9CF
LF9D0:
       .byte $00 ; |        | $F9D0
       .byte $FF ; |XXXXXXXX| $F9D1
       .byte $0F ; |    XXXX| $F9D2
       .byte $00 ; |        | $F9D3
       .byte $00 ; |        | $F9D4
       .byte $00 ; |        | $F9D5
       .byte $00 ; |        | $F9D6
       .byte $00 ; |        | $F9D7
       .byte $00 ; |        | $F9D8
       .byte $00 ; |        | $F9D9
       .byte $FF ; |XXXXXXXX| $F9DA
       .byte $00 ; |        | $F9DB
       .byte $0C ; |    XX  | $F9DC
       .byte $08 ; |    X   | $F9DD
       .byte $1C ; |   XXX  | $F9DE
       .byte $3C ; |  XXXX  | $F9DF
       .byte $0C ; |    XX  | $F9E0
       .byte $06 ; |     XX | $F9E1
       .byte $04 ; |     X  | $F9E2
       .byte $00 ; |        | $F9E3
       .byte $00 ; |        | $F9E4
       .byte $00 ; |        | $F9E5
       .byte $00 ; |        | $F9E6
       .byte $00 ; |        | $F9E7
       .byte $00 ; |        | $F9E8
LF9E9:
  IF PAL
       .byte $2E ; |  X XXX | $F9E9
  ELSE
       .byte $1E ; |   XXXX | $F9E9
  ENDIF

  IF AMIDAR_DS
       .byte $53 ; | X X  XX| $F9EA
  ELSE
       .byte $52 ; | X X  X | $F9EA
  ENDIF

       .byte $07 ; |     XXX| $F9EB
       .byte $07 ; |     XXX| $F9EC
       .byte $07 ; |     XXX| $F9ED

  IF AMIDAR_DS
       .byte $83 ; |X     XX| $F9EE
  ELSE
       .byte $82 ; |X     X | $F9EE
  ENDIF

       .byte $99 ; |X  XX  X| $F9EF
       .byte $99 ; |X  XX  X| $F9F0
       .byte $82 ; |X     X | $F9F1
       .byte $06 ; |     XX | $F9F2
       .byte $22 ; |  X   X | $F9F3
       .byte $06 ; |     XX | $F9F4
       .byte $06 ; |     XX | $F9F5
       .byte $06 ; |     XX | $F9F6
       .byte $2A ; |  X X X | $F9F7
       .byte $04 ; |     X  | $F9F8
       .byte $21 ; |  X    X| $F9F9
       .byte $22 ; |  X   X | $F9FA
       .byte $23 ; |  X   XX| $F9FB
       .byte $24 ; |  X  X  | $F9FC
       .byte $21 ; |  X    X| $F9FD
       .byte $22 ; |  X   X | $F9FE
       .byte $01 ; |       X| $F9FF
       .byte $82 ; |X     X | $FA00
       .byte $61 ; | XX    X| $FA01
       .byte $04 ; |     X  | $FA02
       .byte $80 ; |X       | $FA03
       .byte $10 ; |   X    | $FA04
LFA05:
       .byte $07 ; |     XXX| $FA05
       .byte $25 ; |  X  X X| $FA06
       .byte $43 ; | X    XX| $FA07
       .byte $61 ; | XX    X| $FA08
       .byte $7F ; | XXXXXXX| $FA09
       .byte $99 ; |X  XX  X| $FA0A
LFA0B:
       .byte $00 ; |        | $FA0B
       .byte $32 ; |  XX  X | $FA0C
       .byte $64 ; | XX  X  | $FA0D
       .byte $64 ; | XX  X  | $FA0E
       .byte $C8 ; |XX  X   | $FA0F
       .byte $C8 ; |XX  X   | $FA10
       .byte $FF ; |XXXXXXXX| $FA11
       .byte $FF ; |XXXXXXXX| $FA12

       ORG $FA13








LF9B1:
       inc    $B7                     ;5
       inc    $B6                     ;5
       lda    #$FF                    ;2
       sta    $BB,X                   ;4
       rts                            ;6


    IF PLUSROM = 1
PlusROM_API
       .byte "a", 0, "h.firmaplus.de", 0

SendPlusROMScore

       lda $D6
       and #$03
       bne SkipSendScore
       lda SWCHB                     ; Game variant difficulty P0 (0 - 1)
       sta WriteToBuffer             ; 
       lda $DB                       ; Score Hi BCD
       sta WriteToBuffer             ; 
       lda $DA                       ; Score Mid BCD
       sta WriteToBuffer             ; 
       lda $D9                       ; Score Lo BCD
       sta WriteToBuffer             ; 
       lda #HIGHSCORE_ID             ; game id in Highscore DB
       sta WriteSendBuffer
SkipSendScore
       jmp LF7D5

    ENDIF

Waste10AndFixEarlyExit
       lda SWCHA                    ;4
       jmp LFD8B                    ;3


       ORG $FA84

IFA83: ;first
       pla                            ;4
       sta    RESP0                   ;3
       sta.w  HMP0                    ;4
       and    #$0F                    ;2
;       bne    LFAA9                   ;2
;       lda.w  $DF                     ;4
;LFA90:
       beq    LFA90                   ;2
       .byte $2C                      ;4 skip next 2 bytes
LFA90:
       lda    $DF                     ;3


       sta    $B4                     ;3
       lda.wy $84,Y                   ;4
       sta    PF2                     ;3
       lda.wy $85,Y                   ;4
       sta    PF1                     ;3
       lda    $80                     ;3 waste
LFA9E:
       nop                            ;2
       pla                            ;4
       sta    $B2                     ;3
       lda    $B6                     ;3
       sta    COLUPF                  ;3
       jmp    LFEB0                   ;3

;LFAA9:
;       jmp    LFA90                   ;3

IFAAC:
       pla                            ;4
       sta    HMP0                    ;3
       and    #$0F                    ;2
       sta    RESP0                   ;3

;       bne    LFAC8                   ;2
;       lda.w  $DF                     ;4
;LFAB8:
       beq    LFAB8                   ;2
       .byte $2C                      ;4 skip next 2 bytes
LFAB8:
       lda    $DF                     ;3


       sta.w  $B4                     ;4
       lda.wy $84,Y                   ;4
       sta    PF2                     ;3
       lda.wy $85,Y                   ;4
       sta    PF1                     ;3
       jmp    LFA9E                   ;3

;LFAC8:
;       jmp    LFAB8                   ;3

IFACB:
       pla                            ;4
       sta    HMP0                    ;3
       and    #$0F                    ;2
       bne    LFAE6                   ;2
       lda    $DF                     ;3
       sta    RESP0                   ;3
       nop                            ;2
       sta    $B4                     ;3
       lda.wy $84,Y                   ;4
       sta    PF2                     ;3
       lda.wy $85,Y                   ;4
       sta    PF1                     ;3
       jmp    LFA9E                   ;3

LFAE6:
       nop                            ;2
       sta    RESP0                   ;3
       sta    $B4                     ;3
       nop                            ;2
       lda.wy $84,Y                   ;4
       sta    PF2                     ;3
       lda.wy $85,Y                   ;4
       sta    PF1                     ;3
       jmp    LFA9E                   ;3



IFAF9:
       pla                            ;4
       sta    HMP0                    ;3
       and    #$0F                    ;2


;       bne    LFB1F                   ;2
;       lda.w  $DF                     ;4
;LFB03:
       beq    LFB03                   ;2
       .byte $2C                      ;4 skip next 2 bytes
LFB03:
       lda    $DF                     ;3...possible 1 cycle missing


       sta.w  $B4                     ;4
       sta    RESP0                   ;3
       lda.wy $84,Y                   ;4
       sta    PF2                     ;3
       lda.wy $85,Y                   ;4
       sta    PF1                     ;3


;       lda    $80                     ;3 waste
;       nop                            ;2
;       pla                            ;4
;       sta    $B2                     ;3
;       lda    $B6                     ;3
;       sta    COLUPF                  ;3
;       jmp    LFEB0                   ;3
;LFB1F:
;       jmp    LFB03                   ;3

       clc                            ;2
       bcc    LFB30                   ;2 always branch








IFB22:
       pla                            ;4
       sta    HMP0                    ;3
       and    #$0F                    ;2

;       bne    LFB46                   ;2
;       lda.w  $DF                     ;4
;LFB2C:
       beq    LFB2C                   ;2
       .byte $2C                      ;4 skip next 2 bytes
LFB2C:
       lda    $DF                     ;3


       sta    $B4                     ;3
       nop                            ;2
       ldx    $84,Y                   ;4
       sta    RESP0                   ;3
       stx    PF2                     ;3
       lda.wy $85,Y                   ;4
       sta    PF1                     ;3
       nop                            ;2
       nop                            ;2
LFB30:
       pla                            ;4
       sta    $B2                     ;3
LFB34:
       lda    $B6                     ;3
       sta    COLUPF                  ;3
       jmp    LFEB0                   ;3

;LFB46:
;       jmp    LFB2C                   ;3

IFB49:
       pla                            ;4
       sta    HMP0                    ;3
       and    #$0F                    ;2

;       bne    LFB6E                   ;2
;       lda.w  $DF                     ;4
;LFB53:
       beq    LFB53                   ;2
       .byte $2C                      ;4 skip next 2 bytes
LFB53:
       lda    $DF                     ;3


       sta    $B4                     ;3
       ldx    $85,Y                   ;4
       lda.wy $84,Y                   ;4
       sta    PF2                     ;3
       sta    RESP0                   ;3
       lda    $80                     ;3
       stx    PF1                     ;3
;       lda    $80                     ;3 waste
;       pla                            ;4
;       sta    $B2                     ;3
;       lda    $B6                     ;3
;       sta    COLUPF                  ;3
;       jmp    LFEB0                   ;3
       jmp    LFB30                   ;3


;LFB6E:
;       jmp    LFB53                   ;3

IFB71:
       pla                            ;4
       sta    HMP0                    ;3
       and    #$0F                    ;2


;       bne    LFB95                   ;2
;       lda.w  $DF                     ;4
;LFB7B:
       beq    LFB7B                   ;2
       .byte $2C                      ;4 skip next 2 bytes
LFB7B:
       lda    $DF                     ;3


       sta    $B4                     ;3
       ldx    $85,Y                   ;4
       lda.wy $84,Y                   ;4
       sta    PF2                     ;3
       stx    PF1                     ;3
;       nop                            ;2
       clc                            ;2
       sta    RESP0                   ;3
;       nop                            ;2
;       nop                            ;2
;       pla                            ;4
;       sta    $B2                     ;3
;       lda    $B6                     ;3
;       sta    COLUPF                  ;3
;       jmp    LFEB0                   ;3
       pla                            ;4
       sta.w  $B2                     ;3
       bcc    LFB34                   ;2 always branch


;LFB95:
;       jmp    LFB7B                   ;3

IFB98:
       pla                            ;4
       sta    HMP0                    ;3
       and    #$0F                    ;2

;       bne    LFBBD                   ;2
;       lda.w  $DF                     ;4
;LFBA2:
       beq    LFBA2                   ;2
       .byte $2C                      ;4 skip next 2 bytes
LFBA2:
       lda    $DF                     ;3


       sta    $B4                     ;3
       ldx    $85,Y                   ;4
       lda.wy $84,Y                   ;4
       sta    PF2                     ;3
       stx    PF1                     ;3
       pla                            ;4
       sta    $B2                     ;3
       sta    RESP0                   ;3
       lda    $80                     ;3 waste
;       lda    $80                     ;3 waste
;       lda    $B6                     ;3
;       sta    COLUPF                  ;3
;       jmp    LFEB0                   ;3
       jmp    LFB34                   ;3 always branch


;LFBBD:
;       jmp    LFBA2                   ;3

IFBC0:
       pla                            ;4
       sta    HMP0                    ;3
       and    #$0F                    ;2

;       bne    LFBE5                   ;2
;       lda.w  $DF                     ;4
;LFBCA:
       beq    LFBCA                   ;2
       .byte $2C                      ;4 skip next 2 bytes
LFBCA:
       lda    $DF                     ;3


       sta    $B4                     ;3
       ldx    $85,Y                   ;4
       lda.wy $84,Y                   ;4
       sta    PF2                     ;3
       stx    PF1                     ;3
       pla                            ;4
       sta    $B2                     ;3
       nop                            ;2
       lda    $80                     ;3
       sta    RESP0                   ;3
       lda    $B6                     ;3
       sta.w  COLUPF                  ;4
       jmp    LFEB0                   ;3

;LFBE5:
;       jmp    LFBCA                   ;3

;LFBE8:
;       jmp    LFBF8                   ;3









IFC5E:
       pla                            ;4
       sta    HMP1                    ;3
       and    #$0F                    ;2

;       bne    LFC84                   ;2
;       lda.w  $E0                     ;4
;LFC68:
       beq    LFC68                   ;2
       .byte $2C                      ;4 skip next 2 bytes
LFC68:
       lda    $E0                     ;3


       sta.w  $B5                     ;4
       sta    RESP1                   ;3
       lda.wy $84,Y                   ;4
       sta    PF2                     ;3
       lda.wy $85,Y                   ;4
       sta    PF1                     ;3
;       lda    $80                     ;3 waste
;       nop                            ;2
;       pla                            ;4
;       sta    $B3                     ;3
;       lda    $B6                     ;3
;       sta    COLUPF                  ;3
;       jmp    LFFB3                   ;3
;LFC84:
;       jmp    LFC68                   ;3
       jmp    LFCB0                   ;3





;LFCD6:
;       jmp    LFCE3                   ;3

IFCD9:
       pla                            ;4
       sta    HMP1                    ;3
       and    #$0F                    ;2

;       bne    LFCD6                   ;2
;       lda.w  $E0                     ;4
;LFCE3:
       beq    LFCE3                   ;2
       .byte $2C                      ;4 skip next 2 bytes
LFCE3:
       lda    $E0                     ;3


       sta    $B5                     ;3
       ldx    $85,Y                   ;4
       lda.wy $84,Y                   ;4
       sta    PF2                     ;3
       stx    PF1                     ;3
       nop                            ;2
       sta    RESP1                   ;3
;       nop                            ;2
;       nop                            ;2
;       pla                            ;4
;       sta    $B3                     ;3
;       lda    $B6                     ;3
;       sta    COLUPF                  ;3
;       jmp    LFFB3                   ;3
       pla                            ;4
       sta.w  $B3                     ;4
       jmp    LFCB3                   ;3




;       ORG $FBED

IFC11:
       pla                            ;4
       sta    HMP1                    ;3
       and    #$0F                    ;2
       sta    RESP1                   ;3

;       bne    LFC2D                   ;2
;       lda.w  $E0                     ;4
;LFC1D:
       beq    LFC1D                   ;2
       .byte $2C                      ;4 skip next 2 bytes...possible 1 cycle missing
LFC1D:
       lda    $E0                     ;3

       sta.w  $B5                     ;4
       lda.wy $84,Y                   ;4
       sta    PF2                     ;3
       lda.wy $85,Y                   ;4
       sta    PF1                     ;3
       jmp    LFC06                   ;3

;LFC2D:
;       jmp    LFC1D                   ;3





IFBEB:
       pla                            ;4
       sty    RESP1                   ;3
       sta.w  HMP1                    ;4
       and    #$0F                    ;2

;       bne    LFBE8                   ;2
;       lda.w  $E0                     ;4
;LFBF8:
       beq    LFBF8                   ;2
       .byte $2C                      ;4 skip next 2 bytes...possible 1 cycle missing
LFBF8:
       lda    $E0                     ;3


       sta    $B5                     ;3
       lda.wy $84,Y                   ;4
       sta    PF2                     ;3
       lda.wy $85,Y                   ;4
       sta    PF1                     ;3
;       lda    $80                     ;3 waste
;       nop                            ;2
;       pla                            ;4
;       sta    $B3                     ;3
;       lda    $B6                     ;3
;       sta    COLUPF                  ;3
;       jmp    LFFB3                   ;3
       jmp    LFC06                   ;3








IFC30:
       pla                            ;4
       sta    HMP1                    ;3
       and    #$0F                    ;2
       bne    LFC4B                   ;2
       lda    $E0                     ;3
       sta    RESP1                   ;3
;       nop                            ;2
       clc                            ;2
       sta    $B5                     ;3
       lda.wy $84,Y                   ;4
       sta    PF2                     ;3
       lda.wy $85,Y                   ;4
       sta    PF1                     ;3
;       jmp    LFC06                   ;3
       bcc    LFC06                   ;2 always branch

LFC4B:
       nop                            ;2
       sta    RESP1                   ;3
       sta    $B5                     ;3
;       nop                            ;2
       clc                            ;2
       lda.wy $84,Y                   ;4
       sta    PF2                     ;3
       lda.wy $85,Y                   ;4
       sta    PF1                     ;3
;       jmp    LFC06                   ;3
       bcc    LFC06                   ;2 always branch


;       ORG $FC51




IFC87:
       pla                            ;4
       sta    HMP1                    ;3
       and    #$0F                    ;2

;       bne    LFCAB                   ;2
;       lda.w  $E0                     ;4
;LFC91:
       beq    LFCAB                   ;2
       .byte $2C                      ;4 skip next 2 bytes
LFCAB:
       lda    $E0                     ;3

       sta    $B5                     ;3
       nop                            ;2
       ldx    $84,Y                   ;4
       sta    RESP1                   ;3
       stx    PF2                     ;3
       lda.wy $85,Y                   ;4
       sta    PF1                     ;3
       nop                            ;2
LFC06:
LFCB0:
       nop                            ;2
LFCB1:
       pla                            ;4
LFCB2:
       sta    $B3                     ;3
LFCB3:
       lda    $B6                     ;3
       sta    COLUPF                  ;3
       jmp    LFFB3                   ;3

;LFCAB:
;       jmp    LFC91                   ;3

IFCAE:
       pla                            ;4
       sta    HMP1                    ;3
       and    #$0F                    ;2

;       bne    LFCD3                   ;2
;       lda.w  $E0                     ;4
;LFCB8:
       beq    LFCB8                   ;2
       .byte $2C                      ;4 skip next 2 bytes
LFCB8:
       lda    $E0                     ;3


       sta    $B5                     ;3
       ldx    $85,Y                   ;4
       lda.wy $84,Y                   ;4
       sta    PF2                     ;3
       sta    RESP1                   ;3
       lda    $80                     ;3
       stx    PF1                     ;3
;       lda    $80                     ;3 waste
;       pla                            ;4
;       sta    $B3                     ;3
;       lda    $B6                     ;3
;       sta    COLUPF                  ;3
;       jmp    LFFB3                   ;3
;LFCD3:
;       jmp    LFCB8                   ;3
       jmp    LFCB1                   ;3




IFCFD:
       pla                            ;4
       sta    HMP1                    ;3
       and    #$0F                    ;2

;       bne    LFD22                   ;2
;       lda.w  $E0                     ;4
;LFD07:
       beq    LFD07                   ;2
       .byte $2C                      ;4 skip next 2 bytes
LFD07:
       lda    $E0                     ;3

       sta    $B5                     ;3
       ldx    $85,Y                   ;4
       lda.wy $84,Y                   ;4
       sta    PF2                     ;3
       stx    PF1                     ;3
       pla                            ;4
       sta    $B3                     ;3
       sta    RESP1                   ;3
;       lda    $80                     ;3 waste
;       lda    $80                     ;3 waste
;       lda    $B6                     ;3
;       sta    COLUPF                  ;3
;       jmp    LFFB3                   ;3
       jmp    LFCB2                   ;3

;LFD22:
;       jmp    LFD07                   ;3

IFD25: ;last
       pla                            ;4
       sta    HMP1                    ;3
       and    #$0F                    ;2

;       bne    LFD4A                   ;2
;       lda.w  $E0                     ;4
;LFD2F:
       beq    LFD2F                   ;2
       .byte $2C                      ;4 skip next 2 bytes
LFD2F:
       lda    $E0                     ;3

       sta    $B5                     ;3
       ldx    $85,Y                   ;4
       lda.wy $84,Y                   ;4
       sta    PF2                     ;3
       stx    PF1                     ;3
       pla                            ;4
       sta    $B3                     ;3
;       nop                            ;2
;       lda    $80                     ;3 waste
       dec    $2E                     ;3 waste5
       sta    RESP1                   ;3
       lda    $B6                     ;3
       sta.w  COLUPF                  ;4
       jmp    LFFB3                   ;3

;LFD4A:
;       jmp    LFD2F                   ;3


;       ORG $FCD2

START:
LF000:
       cld                            ;2
       ldx    #$00                    ;2
       txa                            ;2
LF004:
       sta    VSYNC,X                 ;4
       txs                            ;2
       stx    $E2                     ;3
       inx                            ;2
       bne    LF004                   ;2
       inx                            ;2
       stx    CTRLPF                  ;3
       sta    COLUBK                  ;3
       ldy    #$03                    ;2
       lda    SWCHB                   ;4
       and    #$40                    ;2
       bne    LF01C                   ;2
       ldy    #$0B                    ;2
LF01C:
       sty    $D6                     ;3
       jsr    LF7DF                   ;6
       jmp    LF28C                   ;3

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

       ORG $FD00

Digit0:
       .byte $1C ; |   XXX  | $FA13
       .byte $22 ; |  X   X | $FA14
       .byte $63 ; | XX   XX| $FA15
       .byte $63 ; | XX   XX| $FA16
       .byte $63 ; | XX   XX| $FA17
       .byte $22 ; |  X   X | $FA18
       .byte $1C ; |   XXX  | $FA19

Digit4:
       .byte $06 ; |     XX | $FA33
       .byte $7F ; | XXXXXXX| $FA34
       .byte $26 ; |  X  XX | $FA35
       .byte $16 ; |   X XX | $FA36
       .byte $0E ; |    XXX | $FA37
       .byte $06 ; |     XX | $FA38
       .byte $02 ; |      X | $FA39

Digit3:
       .byte $7E ; | XXXXXX | $FA2B
       .byte $03 ; |      XX| $FA2C
       .byte $03 ; |      XX| $FA2D
       .byte $3E ; |  XXXXX | $FA2E
       .byte $03 ; |      XX| $FA2F
       .byte $03 ; |      XX| $FA30
;       .byte $7E ; | XXXXXX | $FA31
Digit5:
       .byte $7E ; | XXXXXX | $FA3B shared
       .byte $03 ; |      XX| $FA3C
       .byte $03 ; |      XX| $FA3D
       .byte $3E ; |  XXXXX | $FA3E
       .byte $60 ; | XX     | $FA3F
       .byte $60 ; | XX     | $FA40
;       .byte $7E ; | XXXXXX | $FA41
Digit1:
       .byte $7F ; | XXXXXXX| $FA1B shared
       .byte $0C ; |    XX  | $FA1C
       .byte $0C ; |    XX  | $FA1D
       .byte $0C ; |    XX  | $FA1E
       .byte $1C ; |   XXX  | $FA1F
       .byte $0C ; |    XX  | $FA20
       .byte $04 ; |     X  | $FA21


Digit7:
       .byte $30 ; |  XX    | $FA4B
       .byte $18 ; |   XX   | $FA4C
       .byte $0C ; |    XX  | $FA4D
       .byte $06 ; |     XX | $FA4E
       .byte $03 ; |      XX| $FA4F
       .byte $61 ; | XX    X| $FA50
;       .byte $7F ; | XXXXXXX| $FA51
Digit2:
       .byte $7F ; | XXXXXXX| $FA23 shared
       .byte $60 ; | XX     | $FA24
       .byte $60 ; | XX     | $FA25
       .byte $3E ; |  XXXXX | $FA26
       .byte $03 ; |      XX| $FA27
       .byte $03 ; |      XX| $FA28
;       .byte $3E ; |  XXXXX | $FA29
Digit6:
       .byte $3E ; |  XXXXX | $FA43 shared
       .byte $63 ; | XX   XX| $FA44
       .byte $63 ; | XX   XX| $FA45
       .byte $7E ; | XXXXXX | $FA46
       .byte $60 ; | XX     | $FA47
       .byte $60 ; | XX     | $FA48
;       .byte $3E ; |  XXXXX | $FA49
Digit8:
       .byte $3E ; |  XXXXX | $FA53 shared
       .byte $63 ; | XX   XX| $FA54
       .byte $63 ; | XX   XX| $FA55
       .byte $3E ; |  XXXXX | $FA56
       .byte $63 ; | XX   XX| $FA57
       .byte $63 ; | XX   XX| $FA58
;       .byte $3E ; |  XXXXX | $FA59
Digit9:
       .byte $3E ; |  XXXXX | $FA5B shared
       .byte $03 ; |      XX| $FA5C
       .byte $03 ; |      XX| $FA5D
       .byte $3F ; |  XXXXXX| $FA5E
       .byte $63 ; | XX   XX| $FA5F
       .byte $63 ; | XX   XX| $FA60
       .byte $3E ; |  XXXXX | $FA61

DigitSpace:
       .byte $00 ; |        | $FA63
       .byte $00 ; |        | $FA64
       .byte $00 ; |        | $FA65
LFA67:
       .byte $00 ; |        | $FA67 shared
       .byte $00 ; |        | $FA68 shared
       .byte $00 ; |        | $FA69 shared
       .byte $00 ; |        | $FA6A shared
LFA6B:
       .byte $41 ; | X     X| $FA6B
       .byte $44 ; | X   X  | $FA6C
       .byte $22 ; |  X   X | $FA6D
       .byte $82 ; |X     X | $FA6E
       .byte $11 ; |   X   X| $FA6F
       .byte $12 ; |   X  X | $FA70
       .byte $48 ; | X  X   | $FA71
       .byte $88 ; |X   X   | $FA72
       .byte $09 ; |    X  X| $FA73
       .byte $08 ; |    X   | $FA74
       .byte $10 ; |   X    | $FA75
       .byte $90 ; |X  X    | $FA76
       .byte $21 ; |  X    X| $FA77
       .byte $40 ; | X      | $FA78
       .byte $02 ; |      X | $FA79
       .byte $84 ; |X    X  | $FA7A
       .byte $41 ; | X     X| $FA7B
       .byte $10 ; |   X    | $FA7C
       .byte $08 ; |    X   | $FA7D
       .byte $82 ; |X     X | $FA7E
       .byte $00 ; |        | $FA7F
       .byte $00 ; |        | $FA80
       .byte $00 ; |        | $FA81
       .byte $00 ; |        | $FA82

    IF PLUSROM = 0

BRK_ROUTINE:
       sta    $DF                     ;3
       lda    #$00                    ;2
       sta    $E2                     ;3
       rti                            ;6

;all above ok
;       ORG $FD61
       ORG $FD66

    ENDIF


LFD4D:
       sta    WSYNC                   ;3
LFD4F:
       ldy    $B2                     ;3
       lda    LF96B-1,Y               ;4
       bmi    LFD6B                   ;2
       sta    GRP0                    ;3
       ldy    $B3                     ;3
       lda    LF96B-1,Y               ;4
       bmi    LFD7B                   ;2
       sta    GRP1                    ;3
       dec    $B2                     ;5
       dec    $B3                     ;5
       sta.w  HMCLR                   ;4
       jmp    LFD91                   ;3

LFD6B:
       ldy    $B3                     ;3
       lda    LF96B-1,Y               ;4
       bmi    LFD82                   ;2
       sta    GRP1                    ;3
       dec    $B3                     ;5
;       nop                            ;2
;       nop                            ;2
       jmp    LFD8B                   ;3

LFD7B:
       dec    $B2                     ;5
;       nop                            ;2
;       nop                            ;2
       jmp    LFD8B                   ;3

LFD82:
;       lda    $80                     ;3 waste
;       nop                            ;2
;       nop                            ;2
;       nop                            ;2
;       nop                            ;2
;       jmp    LFD8B                   ;3 waste 3
;       dec    $2E                     ;5 waste 5
;       dec    $2E                     ;5 waste 5
       jmp Waste10AndFixEarlyExit      ;3 waste 10
       nop
LFD8B:
       nop                            ;2
       nop                            ;2
;LFD8B:
;       sta.w  HMCLR                   ;4
;       jmp    LFD91                   ;3 waste 3
       sta    HMCLR                   ;3
       nop                            ;2
       nop                            ;2
LFD91:
       ldy    $B3                     ;3
       ldx    LF96B-1,Y               ;4
       bmi    LFDB3                   ;2
       lda    $80                     ;3 waste
       dec    $B3                     ;5
LFD9C:

       lda    $B7                     ;3
       ldy    $B4                     ;3
       bne    LFDB9                   ;2
       ldy    $B8                     ;3
       sta    COLUPF                  ;3
       stx    GRP1                    ;3
       lda.wy $82,Y                   ;4
       sta    PF1                     ;3
       lda.wy $83,Y                   ;4
       sta    PF2                     ;3
       rts                            ;6

LFDB3:
       ldx    #$00                    ;2
       nop                            ;2
       jmp    LFD9C                   ;3




LFDB9:
       sta.w  COLUPF                  ;4
       dec    $B4                     ;5
       stx    GRP1                    ;3
       ldy    $B2                     ;3
       lda    LF96B-1,Y               ;4
       bmi    LFDDA                   ;2
       sta    GRP0                    ;3
       ldy    $B8                     ;3
       lda.wy $82,Y                   ;4
       sta    PF1                     ;3
       lda.wy $83,Y                   ;4
       sta    PF2                     ;3
       dec    $B2                     ;5
LFDD7:
       jmp    LFDEB                   ;3

LFDDA:
       ldy    $B8                     ;3
       lda.wy $82,Y                   ;4
       sta    PF1                     ;3
       lda.wy $83,Y                   ;4
       sta    PF2                     ;3
       nop                            ;2
       nop                            ;2
       jmp    LFDD7                   ;3

LFDEB:
       lda.wy $84,Y                   ;4
       sta.w  PF2                     ;4
       lda.wy $85,Y                   ;4
       sta    PF1                     ;3
       ldy    $B9                     ;3
       ldx    LF9BA,Y                 ;4
;       lda    $80                     ;3 waste
;       lda    $80                     ;3 waste
;       nop                            ;2
       dec    $2E                     ;5 waste
       lda    $80                     ;3 waste

       lda    $B6                     ;3
       sta    COLUPF                  ;3
       ldy    $B3                     ;3
       lda    LF96B-1,Y               ;4
       bmi    LFE46                   ;2
       sta    GRP1                    ;3
       stx    PF1                     ;3
       ldy    $B2                     ;3
       lda    LF96B-1,Y               ;4
       bmi    LFE36                   ;2
       sta    GRP0                    ;3
       ldy    $B9                     ;3
       lda    LF9BB,Y                 ;4
       sta    PF2                     ;3
       dec    $B2                     ;5
       dec    $B3                     ;5
       ldy    $B2                     ;3
       ldx    LF96B-1,Y               ;4
       bmi    LFE7D                   ;2
       dec    $B2                     ;5
       dec    $B5                     ;5
       lda    $B7                     ;3
       sta    COLUPF                  ;3
       nop                            ;2
       jmp    LFF01                   ;3





LFE36:
       ldy    $B9                     ;3
       lda    LF9BB,Y                 ;4
       sta    PF2                     ;3
       dec    $B3                     ;5
;       nop                            ;2
;       nop                            ;2
;       lda    $80                     ;3 waste
;       jmp    LFE9B                   ;3
       jmp    LFE98                   ;3

LFE7D:
       ldx    #$00                    ;2
       nop                            ;2
       nop                            ;2
       dec    $B5                     ;5
       lda    $B7                     ;3
       sta    COLUPF                  ;3
       jmp    LFF01                   ;3

LFE46:
       stx    PF1                     ;3
       ldy    $B2                     ;3
       lda    LF96B-1,Y               ;4
       bmi    LFE8A                   ;2
       sta    GRP0                    ;3
       ldy    $B9                     ;3
       lda    LF9BB,Y                 ;4
       sta    PF2                     ;3
       dec    $B2                     ;5
       ldy    $B2                     ;3
       ldx    LF96B-1,Y               ;4
       bmi    LFE9B                   ;2
       dec    $B2                     ;5
       lda    $B7                     ;3
       ldy    $B5                     ;3
       bne    LFEA7                   ;2
       ldy    $80                     ;3 waste
LFE6B:
       ldy    $B8                     ;3
       sta.w  COLUPF                  ;4
       stx    GRP0                    ;3
       lda.wy $82,Y                   ;4
       sta    PF1                     ;3
       lda.wy $83,Y                   ;4
       sta    PF2                     ;3
       rts                            ;6

LFE8A:
       ldy    $B9                     ;3
       lda    LF9BB,Y                 ;4
       sta    PF2                     ;3
;       nop                            ;2
;       nop                            ;2
;       nop                            ;2
;       nop                            ;2
;       nop                            ;2
       dec    $2E                     ;5 waste5
       dec    $2E                     ;5 waste5
;       nop                            ;2
;       nop                            ;2
;       jmp    LFE9B                   ;3 waste3
LFE98:
       nop                            ;2
       dec    $2E                     ;5 waste5
LFE9B:
       ldx    #$00                    ;2
       lda    $B7                     ;3
       ldy    $B5                     ;3
       nop                            ;2
       bne    LFEA7                   ;2
       jmp    LFE6B                   ;3

LFEB0:
       sta    HMOVE                   ;3
       ldy    $B3                     ;3
       lda    LF96B-1,Y               ;4
       bmi    LFEF0                   ;2
       sta    GRP1                    ;3
       ldy    $B9                     ;3
       lda    LF9BA,Y                 ;4
       sta    PF1                     ;3
       lda    LF9BB,Y                 ;4
       sta    PF2                     ;3
       dec    $B3                     ;5
LFEC9:
       lda    $B2                     ;3
       and    #$70                    ;2
       tay                            ;2
       lda    LF97A-1,Y               ;4
       sta.w  COLUP0                  ;4
       ldx    #$00                    ;2
       lda    $B7                     ;3
       ldy    $B5                     ;3
       bne    LFEA7                   ;2
       ldy    $B8                     ;3
       stx    GRP0                    ;3
       sta.w  COLUPF                  ;4
       lda.wy $83,Y                   ;4
       sta    PF2                     ;3
       lda.wy $82,Y                   ;4
       sta    PF1                     ;3
       sta    HMCLR                   ;3
       rts                            ;6

LFEF0:
       ldy    $B9                     ;3
       lda    LF9BA,Y                 ;4
       sta    PF1                     ;3
       lda    LF9BB,Y                 ;4
       sta    PF2                     ;3
       nop                            ;2
;       nop                            ;2
;       jmp    LFEC9                   ;3
       clc                            ;2
       bcc    LFEC9                   ;2 always branch

;       ORG $FEFA

LFEA7:
       dec    $B5                     ;5
       ldy    $80                     ;3 waste
       sta    COLUPF                  ;3
;       jmp    LFF01                   ;3
       ldy    $80                     ;3 waste

LFF01:
       stx    GRP0                    ;3
       ldy    $B3                     ;3
       lda    LF96B-1,Y               ;4
       bmi    LFF35                   ;2
       sta    GRP1                    ;3
       ldy    $B8                     ;3
       lda.wy $82,Y                   ;4
       sta    PF1                     ;3
       lda.wy $83,Y                   ;4
       sta    PF2                     ;3
       dec    $B3                     ;5
LFF1A:
       lda    $80                     ;3 waste
       lda.wy $84,Y                   ;4
       sta    PF2                     ;3
       lda.wy $85,Y                   ;4
       sta    PF1                     ;3
       ldy    $B9                     ;3
       ldx    LF9BA,Y                 ;4
       lda    $80                     ;3 waste
       nop                            ;2
       lda    $B6                     ;3
       sta    COLUPF                  ;3
       jmp    LFF46                   ;3

LFF35:
       ldy    $B8                     ;3
       lda.wy $82,Y                   ;4
       sta    PF1                     ;3
       lda.wy $83,Y                   ;4
       sta    PF2                     ;3
       nop                            ;2
       nop                            ;2
       jmp    LFF1A                   ;3

LFF46:
       ldy    $B3                     ;3
       lda    LF96B-1,Y               ;4
       bmi    LFF8A                   ;2
       sta    GRP1                    ;3
       stx    PF1                     ;3
       ldy    $B2                     ;3
       lda    LF96B-1,Y               ;4
       bmi    LFF7C                   ;2
       sta    GRP0                    ;3
       ldy    $B9                     ;3
       lda    LF9BB,Y                 ;4
       sta    PF2                     ;3
       dec    $B2                     ;5
LFF63:
       dec    $B3                     ;5
LFF65:
       dec    $BA                     ;5
       bne    LFF6C                   ;2
       jmp    LF1BD                   ;3

LFF6C:
       ldy    $BA                     ;3
       lda    LF948,Y                 ;4
       sta    $B8                     ;3
       lda    LF927,Y                 ;4
       sta.w  $B9                     ;4
       jmp    LFD4F                   ;3

LFF7C:
       ldy    $B9                     ;3
       lda    LF9BB,Y                 ;4
       sta    PF2                     ;3
;       dec    $B3                     ;5
;       nop                            ;2
;       nop                            ;2
;       jmp    LFF65                   ;3
       nop                            ;2
       nop                            ;2
       jmp    LFF63                   ;3

LFF8A:
       stx    PF1                     ;3
       ldy    $B2                     ;3
       lda    LF96B-1,Y               ;4
       bmi    LFFA3                   ;2
       sta    GRP0                    ;3
       ldy    $B9                     ;3
       lda    LF9BB,Y                 ;4
       sta    PF2                     ;3
       dec    $B2                     ;5
LFF90:
       nop                            ;2
       nop                            ;2
       jmp    LFF65                   ;3

LFFA3:
       ldy    $B9                     ;3
       lda    LF9BB,Y                 ;4
       sta    PF2                     ;3
       nop                            ;2
       nop                            ;2
;       nop                            ;2
;       nop                            ;2
;       lda    $80                     ;3 waste
;       jmp    LFF65                   ;3
       jmp    LFF90                   ;3

LFFB3:
       sta    HMOVE                   ;3
       ldy    $B2                     ;3
       lda    LF96B-1,Y               ;4
       bmi    LFFE7                   ;2
       sta    GRP0                    ;3
       ldy    $B9                     ;3
       lda    LF9BA,Y                 ;4
       sta    PF1                     ;3
       lda    LF9BB,Y                 ;4
       sta    PF2                     ;3
       dec    $B2                     ;5
LFFCC:
       lda    $B3                     ;3
       and    #$70                    ;2
       tay                            ;2
       lda    LF97A-1,Y               ;4
       sta    COLUP1                  ;3
       dec    $BA                     ;5
       ldy    $BA                     ;3
       lda    LF948,Y                 ;4
       sta    $B8                     ;3
       lda    LF927,Y                 ;4
       sta    $B9                     ;3
       jmp    LFD4F                   ;3

LFFE7:
       ldy    $B9                     ;3
       lda    LF9BA,Y                 ;4
       sta    PF1                     ;3
       lda    LF9BB,Y                 ;4
    IF PLUSROM = 0
       sta    PF2                     ;3
       nop                            ;2
       nop                            ;2
       jmp    LFFCC                   ;3

    ELSE
       sta.w    PF2                   ;4
       jmp    LFFF4                   ;3


    ORG $FFF4
LFFF4
       jmp    LFFCC                   ;3

       ORG $FFFA
       .word (PlusROM_API - $E000)      ; PlusRom API pointer

    ELSE

       ORG $FFF8
       .byte "2008"

    ENDIF

       .word START,BRK_ROUTINE
