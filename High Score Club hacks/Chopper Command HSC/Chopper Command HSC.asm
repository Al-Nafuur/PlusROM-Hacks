; Chopper Command: Supercharger hack by Kurt (Nukey Shay) Howe, 2006
; PlusROM HSC hack added by Wolfgang Stubig (Al_Nafuur) 7/2023
; Disassembly of Choprcmd.bin
; Disassembled Tue Mar 28 17:22:01 2006
; Using DiStella v2.0
; Command Line: C:\BIN\DISTELLA.EXE -pafscChoprcmd.cfg Choprcmd.bin 
; Choprcmd.cfg contents:
;      CODE F000 FDEF
;      GFX FDF0 FF40
;      CODE FF41 FFA0
;      GFX FFA1 FFFF

PLUSROM = 1
PAL = 0

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
REFP0   =  $0B
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
ENABL   =  $1F
HMP0    =  $20
HMP1    =  $21
HMBL    =  $24
VDELP0  =  $25
VDEL01  =  $26
HMOVE   =  $2A
HMCLR   =  $2B
CXCLR   =  $2C
SWCHA   =  $0280
SWCHB   =  $0282
INTIM   =  $0284
TIM64T  =  $0296

   IF PLUSROM = 1

WriteToBuffer           = $1FF0
WriteSendBuffer         = $1FF1
ReceiveBuffer           = $1FF2
ReceiveBufferSize       = $1FF3

HIGHSCORE_ID            = 68         ; Chopper Command game ID in Highscore DB

   ENDIF

       ORG $F000

LF014:
       STA    $D0                     ;3
       TAY                            ;2
       INY                            ;2
       TYA                            ;2
       AND    #$0F                    ;2
       STA    $CF                     ;3
       TYA                            ;2
       LSR                            ;2
       LSR                            ;2
       LSR                            ;2
       LSR                            ;2
       TAY                            ;2
       CLC                            ;2
       ADC    $CF                     ;3
       CMP    #$0F                    ;2
       BCC    LF02D                   ;2
       SBC    #$0F                    ;2
       INY                            ;2
LF02D:
       EOR    #$07                    ;2
       RTS                            ;6

LF030:
       LDA    #$05                    ;2
       CMP    $A1,X                   ;4
       BCS    LF091                   ;2
       LDY    #$1F                    ;2
       STY    $9D,X                   ;4
       STY    $9B                     ;3
       LDY    LFEF0,X                 ;4
       LDA    LFEF4,X                 ;4
       EOR    #$FF                    ;2
       AND.wy $85,Y                   ;4
       STA.wy $85,Y                   ;5
       LDA    $A1,X                   ;4
       LSR                            ;2
       LSR                            ;2
       EOR    #$03                    ;2
       LDY    #$00                    ;2
       STA    $A1,X                   ;4
       LDX    $EB                     ;3
       DEC    $E2,X                   ;6
       BNE    LF06C                   ;2
       STY    $A5                     ;3
       LDY    $E8,X                   ;4
       CPY    #$0A                    ;2
       BCS    LF064                   ;2
       INC    $E8,X                   ;6
LF064:
       LDY    #$BF                    ;2
       STY    $E1                     ;3
       LDY    #$0C                    ;2
       STY    $E2,X                   ;4
LF06C:
       SED                            ;2
       CLC                            ;2
       ADC    $EE,X                   ;4
       STA    $EE,X                   ;4
       BCC    LF090                   ;2
       LDA    $E4,X                   ;4
       EOR    #$07                    ;2
       BEQ    LF07C                   ;2
       INC    $E4,X                   ;6
LF07C:
       LDA    #$00                    ;2
       ADC    $EC,X                   ;4
       BCC    LF08E                   ;2
       STA    $E4,X                   ;4
       LDA    #$BF                    ;2
       STA    $E1                     ;3
       LDA    #$99                    ;2
       STA    $F0,X                   ;4
       STA    $EE,X                   ;4
LF08E:
       STA    $EC,X                   ;4
LF090:
       CLD                            ;2
LF091:
       RTS                            ;6

LF092:
       CPX    $82                     ;3
       BNE    LF0C8                   ;2
       LDA    $DF                     ;3
       STA    $82                     ;3
       LDA    #$02                    ;2
LF09C:
       STA    WSYNC                   ;3
       STA    HMOVE                   ;3
       STA    ENAM0                   ;3
       LDY    $CF                     ;3
       STY    GRP0                    ;3
       TXA                            ;2
       SBC    $C8                     ;3
       TAY                            ;2
       CPY    #$0A                    ;2
       BCS    LF0CD                   ;2
       LDA    ($C9),Y                 ;5
       STA    $CF                     ;3
LF0B2:
       TXA                            ;2
       SBC    $D2                     ;3
       CMP    #$0A                    ;2
       BCS    LF0D4                   ;2
       TAY                            ;2
       LDA    ($CB),Y                 ;5
       STA    GRP1                    ;3
LF0BE:
       DEX                            ;2
       CPX    $C1                     ;3
       BNE    LF092                   ;2
       JSR    LF2A5                   ;6
       BCC    LF092                   ;2
LF0C8:
       SEC                            ;2
       LDA    #$00                    ;2
       BEQ    LF09C                   ;2 always branch

LF0CD:
       TXA                            ;2
       CLC                            ;2
       BNE    LF0B2                   ;2
       JMP    LF9C0                   ;3

LF0D4:
       BMI    LF0E4                   ;2
       TAY                            ;2
       BCS    LF0BE                   ;2
LF0D9:
       SEC                            ;2
       LDA    #$00                    ;2
       BEQ    LF0F6                   ;2 always branch

LF0DE:
       STA    HMCLR                   ;3
       STA    CXCLR                   ;3
       LDX    #$6F                    ;2
LF0E4:
       DEX                            ;2
       CPX    $C1                     ;3
       BNE    LF0EC                   ;2
       JSR    LF2A3                   ;6
LF0EC:
       CPX    $82                     ;3
       BNE    LF0D9                   ;2
       LDA    $DF                     ;3
       STA    $82                     ;3
       LDA    #$02                    ;2
LF0F6:
       STA    WSYNC                   ;3
       STA    HMOVE                   ;3
       STA    ENAM0                   ;3
       LDY    $CF                     ;3
       STY    GRP0                    ;3
       TXA                            ;2
       SBC    $C8                     ;3
       CMP    #$0A                    ;2
       TAY                            ;2
       BCS    LF10C                   ;2
       LDA    ($C9),Y                 ;5
       STA    $CF                     ;3
LF10C:
       SEC                            ;2
       TXA                            ;2
       SBC    $C1                     ;3
       CMP    #$03                    ;2
       BCC    LF0E4                   ;2
       DEY                            ;2
       STY    $D1                     ;3
       LDY    $D5                     ;3
       LDA.wy $8D,Y                   ;4
       LDY    #$00                    ;2
       DEX                            ;2
       CPX    $82                     ;3
       BNE    LF129                   ;2
       LDY    $DF                     ;3
       STY    $82                     ;3
       LDY    #$02                    ;2
LF129:
       SEC                            ;2
       STA    WSYNC                   ;3
       STA    HMOVE                   ;3
       STY    ENAM0                   ;3
       DEX                            ;2
       LDY    $CF                     ;3
       STY    GRP0                    ;3
       ORA    $9C                     ;3
       BMI    LF15A                   ;2
       BEQ    LF154                   ;2
LF13B:
       SBC    #$01                    ;2
       BNE    LF13B                   ;2
       STA    RESP1                   ;3
LF141:
       STA    HMP1                    ;3
       CPX    $82                     ;3
       BNE    LF14D                   ;2
       LDA    $DF                     ;3
       STA    $82                     ;3
       LDA    #$02                    ;2
LF14D:
       LDY    $D1                     ;3
       CPY    #$0A                    ;2
       JMP    LF172                   ;3

LF154:
       LDA    #$60                    ;2
       STA    RESP1                   ;3
       BNE    LF141                   ;2 always branch

LF15A:
       TAY                            ;2
LF15B:
       DEY                            ;2
       BMI    LF15B                   ;2
       LDA    $9C                     ;3
       STA    HMP1                    ;3
       CPX    $82                     ;3
       BNE    LF17C                   ;2
       LDA    $DF                     ;3
       STA    $82                     ;3
       LDA    #$02                    ;2
LF16C:
       LDY    $D1                     ;3
       CPY    #$0A                    ;2
       STA    RESP1                   ;3
LF172:
       STA    WSYNC                   ;3
       STA    HMOVE                   ;3
       STA    ENAM0                   ;3
       BCC    LF181                   ;2
       LDA    $9C                     ;3
LF17C:
       BEQ    LF183                   ;2
       SEC                            ;2
       BCS    LF16C                   ;2 always branch

LF181:
       LDA    ($C9),Y                 ;5
LF183:
       STA    GRP0                    ;3
       DEY                            ;2
       STY    $D1                     ;3
       LDA    $D5                     ;3
       BIT    RSYNC                   ;3
       BMI    LF1B4                   ;2
       STA.w  $90                     ;4
LF191:
       JSR    LF270                   ;6
       LDY    $D5                     ;3
       LDA.wy $A1,Y                   ;4
       STA    $CB                     ;3
       LDA.wy $9D,Y                   ;4
       STA    HMP1                    ;3
       JSR    LF26E                   ;6
       LDY    $D5                     ;3
       BEQ    LF1B6                   ;2
       LDA.wy $95,Y                   ;4
       NOP                            ;2
       JSR    LF26A                   ;6
       LDA    $9C                     ;3
       STA    ENABL                   ;3
       BEQ    LF1C3                   ;2
LF1B4:
       BMI    LF191                   ;2
LF1B6:
       LDA    #$FD                    ;2
       STA    $D2                     ;3
       JSR    LF26A                   ;6
       LDA    VSYNC                   ;3
       STA    $8D                     ;3
       LDA    $95                     ;3
LF1C3:
       STA    NUSIZ1                  ;3
       DEC    $D5                     ;5
       JSR    LF26E                   ;6
       LDY    $CB                     ;3
       LDA    LFEE4,Y                 ;4
       EOR    $F5                     ;3
       AND    $F4                     ;3
       STA.w  COLUP1                  ;4
       JSR    LF26E                   ;6
       LDY    $CB                     ;3
       LDA    LFE80,Y                 ;4
       STA    REFP1                   ;3
       LDA    LFF0E,Y                 ;4
       STA    $CB                     ;3
       JSR    LF26E                   ;6
       LDY    $D1                     ;3
       CPY    #$0A                    ;2
       LDA    #$00                    ;2
       BCS    LF1F2                   ;2
       LDA    ($C9),Y                 ;5
LF1F2:
       STA    $CF                     ;3
       LDY    #$80                    ;2
       JMP    LF0BE                   ;3

LF1F9:
       LDY    #$F0                    ;2
       LDX    $A5                     ;3
       BEQ    LF231                   ;2
       DEC    $A5                     ;5
       LDA    $C6                     ;3
       LDY    $9A                     ;3
       DEY                            ;2
       CLC                            ;2
       BPL    LF20C                   ;2
       ADC    $A6                     ;3
       SEC                            ;2
LF20C:
       STA    $D0                     ;3
       LDA    #$FE                    ;2
       LDX    #$02                    ;2
       BIT    $A5                     ;3
       BPL    LF21D                   ;2
       LDA    $A7                     ;3
       BCC    LF21C                   ;2
       LDA    #$00                    ;2
LF21C:
       TAX                            ;2
LF21D:
       CLC                            ;2
       ADC    $DF                     ;3
       TAY                            ;2
       TXA                            ;2
       CLC                            ;2
       ADC    $DE                     ;3
       CPY    #$6F                    ;2
       BCC    LF22B                   ;2
       LDY    #$F0                    ;2
LF22B:
       STA    $82                     ;3
       CMP    #$6F                    ;2
       BCC    LF235                   ;2
LF231:
       LDA    #$F0                    ;2
       STY    $82                     ;3
LF235:
       STA    $DE                     ;3
       STY    $DF                     ;3
       AND    $DF                     ;3
       BMI    LF247                   ;2
       LDA    $D0                     ;3
       CMP    #$07                    ;2
       BCC    LF247                   ;2
       CMP    #$A0                    ;2
       BCC    LF24F                   ;2
LF247:
       LDA    #$00                    ;2
       STA    $A5                     ;3
       LDY    #$F0                    ;2
       STY    $82                     ;3
LF24F:
       STA    $C6                     ;3
       LDX    #$02                    ;2
LF253:
       JSR    LF014                   ;6
       STA    WSYNC                   ;3
       STA    HMCLR                   ;3
       ASL                            ;2
       ASL                            ;2
       ASL                            ;2
       ASL                            ;2
       STA    HMP0,X                  ;4
LF260:
       DEY                            ;2
       BPL    LF260                   ;2
       STA    RESP0,X                 ;4
LF265:
       STA    WSYNC                   ;3
       STA    HMOVE                   ;3
       RTS                            ;6

LF26A:
       STA    $D2                     ;3
       STA    HMCLR                   ;3
LF26E:
       LDY    $D1                     ;3
LF270:
       DEX                            ;2
       CPY    #$0A                    ;2
       BCC    LF279                   ;2
       LDA    $9C                     ;3
       BEQ    LF27B                   ;2
LF279:
       LDA    ($C9),Y                 ;5
LF27B:
       CPX    $C1                     ;3
       BEQ    LF298                   ;2
LF27F:
       DEY                            ;2
       STY    $D1                     ;3
       CPX    $82                     ;3
       BNE    LF293                   ;2
       LDY    $DF                     ;3
       STY    $82                     ;3
       LDY    #$02                    ;2
LF28C:
       STA    HMOVE                   ;3
       STY    ENAM0                   ;3
       STA    GRP0                    ;3
       RTS                            ;6

LF293:
       SEC                            ;2
       LDY    #$00                    ;2
       BEQ    LF28C                   ;2 always branch

LF298:
       JSR    LF2A1                   ;6
       LDY    $D1                     ;3
       DEY                            ;2
       DEY                            ;2
       BCC    LF27F                   ;2
LF2A1:
       STA    $CF                     ;3
LF2A3:
       LDY    #$80                    ;2
LF2A5:
       STA    WSYNC                   ;3
       STA    HMOVE                   ;3
       LDA    $B1                     ;3
       STA    PF0                     ;3
       STA    ENAM0                   ;3
       LDA    $CF                     ;3
       STA    GRP0                    ;3
       LDA    $B4                     ;3
       STA    PF1                     ;3
       LDA    $B7                     ;3
       STA    PF2                     ;3
       LDA    $BA                     ;3
       STA    PF0                     ;3
       DEX                            ;2
       DEX                            ;2
       LDA    $BD                     ;3
       STA    PF1                     ;3
       LDA    $C0                     ;3
       STA    PF2                     ;3
       DEY                            ;2
       CPY    #$0A                    ;2
       BCS    LF30B                   ;2
       LDA    ($CB),Y                 ;5
       STA    GRP1                    ;3
LF2D2:
       DEY                            ;2
       STA    HMCLR                   ;3
       LDA    $CD                     ;3
       CPY    #$0A                    ;2
       STA    HMOVE                   ;3
       STA    GRP0                    ;3
       LDA    $BA                     ;3
       STA    ENAM0                   ;3
       LDA    #$00                    ;2
       STA    PF0                     ;3
       STA    PF1                     ;3
       STA    PF2                     ;3
       BCC    LF2EF                   ;2
       LDA    $9C                     ;3
       BCS    LF2F1                   ;2
LF2EF:
       LDA    ($CB),Y                 ;5
LF2F1:
       STA    GRP1                    ;3
       CLC                            ;2
       LDA    $CE                     ;3
       STA    $CF                     ;3
       RTS                            ;6

LF2F9:
       LDY    LFEF0,X                 ;4
       LDA.wy $85,Y                   ;4
       AND    LFEF4,X                 ;4
       CMP    #$08                    ;2
       BCC    LF309                   ;2
       LSR                            ;2
       LSR                            ;2
       LSR                            ;2
LF309:
       TAY                            ;2
       RTS                            ;6

LF30B:
       NOP                            ;2
       BCS    LF2D2                   ;2
LF30E:
       TYA                            ;2
LF30F:
       AND    #$02                    ;2
       ASL                            ;2
       STA    $E9                     ;3
       LDX    #$08                    ;2
       LDY    #$00                    ;2
LF318:
       STA    $E0,X                   ;4
       STY    $E9,X                   ;4
       DEY                            ;2
       STY    $82,X                   ;4
       INY                            ;2
       LDA    LFEC9,X                 ;4
       DEX                            ;2
       BNE    LF318                   ;2
       LDY    #$41                    ;2
LF328:
       STY    $CF                     ;3
       LDX    #$17                    ;2
       LDY    #$00                    ;2
LF32E:
       LDA    LFEF6,X                 ;4
       STA    $B1,X                   ;4
       STY    $9B,X                   ;4
       DEX                            ;2
       BNE    LF32E                   ;2
       LDX    $EB                     ;3
       LDA    $E2,X                   ;4
       STA    $D0                     ;3
       LDX    #$04                    ;2
       STX    $A1                     ;3
       LDA    $F2                     ;3
       STA    $E7                     ;3
       LDA    $80                     ;3
       STA    $E6                     ;3
LF34A:
       LDA    $F2                     ;3
       AND    #$87                    ;2
       EOR    $84,X                   ;4
       ORA    $CF                     ;3
       DEC    $D0                     ;5
       BPL    LF358                   ;2
       AND    #$78                    ;2
LF358:
       STA    $84,X                   ;4
       LDA    $F3                     ;3
       ORA    #$09                    ;2
       DEC    $D0                     ;5
       BPL    LF364                   ;2
       LDA    #$00                    ;2
LF364:
       DEC    $D0                     ;5
       BPL    LF36A                   ;2
       AND    #$78                    ;2
LF36A:
       STA    $88,X                   ;4
       JSR    LFD6C                   ;6
       BNE    LF34A                   ;2
       RTS                            ;6



START:
       SEI                            ;2
       CLD                            ;2
       LDX    #$00                    ;2
       TXA                            ;2
LF005:
       STA    VSYNC,X                 ;4
       TXS                            ;2
       INX                            ;2
       BNE    LF005                   ;2
       LDY    #$FF                    ;2
       STY    $F2                     ;3
       STY    $F3                     ;3
       BNE    LF3EF                   ;2 always branch


LF372:
       LDA    $80                     ;3
       ORA    $E1                     ;3
       BNE    LF395                   ;2
       LDA    $F2                     ;3
       AND    #$03                    ;2
       TAX                            ;2
       EOR    #$02                    ;2
       BEQ    LF395                   ;2
       LDA    $85,X                   ;4
       AND    #$07                    ;2
       ORA    $89,X                   ;4
       AND    #$3F                    ;2
       BEQ    LF395                   ;2
       LDA    $F3                     ;3
       AND    #$03                    ;2
       TAY                            ;2
       BEQ    LF395                   ;2
       JSR    LFF6E                   ;6
LF395:
       LDY    INTIM                   ;4
       BNE    LF395                   ;2
       STY    $F5                     ;3
       DEY                            ;2
       STA    WSYNC                   ;3
       STY    VSYNC                   ;3
       SEC                            ;2
       INC    $80                     ;5
       BNE    LF3AC                   ;2
       INC    $EA                     ;5
       BNE    LF3AC                   ;2
       ROR    $EA                     ;5
LF3AC:
       TYA                            ;2
       EOR    SWCHB                   ;4
       AND    #$08                    ;2
       ASL                            ;2
       SBC    #$00                    ;2
       LDY    $EA                     ;3
       BPL    LF3BD                   ;2
       STY    $F5                     ;3
       AND    #$F7                    ;2
LF3BD:
       STA    $F4                     ;3
       ASL    $F5                     ;5
       LDX    #$04                    ;2
LF3C3:
       LDA    $F5                     ;3
       EOR    LFEE0,X                 ;4
       AND    $F4                     ;3
       STA    $F5,X                   ;4
       STA    NUSIZ1,X                ;4
       DEX                            ;2
       STX    COLUPF                  ;3
       BNE    LF3C3                   ;2
       LDA    SWCHB                   ;4
    IF PAL = 1
       LDY    #$52                    ;2
    ELSE
       LDY    #$3A                    ;2
    ENDIF
       LSR                            ;2
       STA    WSYNC                   ;3
       STX    VSYNC                   ;3
       STY    TIM64T                  ;4
       LDY    $E0                     ;3
       ROR                            ;2
       BMI    LF3E9                   ;2
       JSR    LF30E                   ;6
       SEC                            ;2
LF3E9:
       BCS    LF406                   ;2
       DEC    $81                     ;5
       BPL    LF408                   ;2
LF3EF:
       INY                            ;2
       TYA                            ;2
       AND    #$03                    ;2
       STA    $E0                     ;3
       JSR    LF30F                   ;6
       STX    $E4                     ;3
       STX    $C2                     ;3
       DEX                            ;2
       STX    $A9                     ;3
       LDX    $E0                     ;3
       INX                            ;2
       STX    $F0                     ;3
       LDX    #$1D                    ;2
LF406:
       STX    $81                     ;3
LF408:
       LDA    $E1                     ;3
       CMP    #$5E                    ;2
       BNE    LF42F                   ;2
       LDA    $E0                     ;3
       LSR                            ;2
       BCC    LF474                   ;2
       LDX    #$03                    ;2
LF415:
       LDA    $85,X                   ;4
       ASL                            ;2
       ASL                            ;2
       LDY    #$02                    ;2
LF41B:
       ASL                            ;2
       ROL    $83                     ;5
       ROL    $84                     ;5
       DEY                            ;2
       BPL    LF41B                   ;2
       LDA    $84                     ;3
;       LSR                            ;2
;       AND    #$38                    ;2
       asr    #$70                    ;2 illegal

       JSR    LFF96                   ;6
       DEX                            ;2
       BPL    LF415                   ;2
       TXA                            ;2
LF42F:
       LDX    $EB                     ;3
       LDY    #$79                    ;2
       EOR    #$80                    ;2
       BEQ    LF467                   ;2
       EOR    #$40                    ;2
       BEQ    LF441                   ;2
       LDY    #$41                    ;2
       EOR    #$9F                    ;2
       BNE    LF474                   ;2
LF441:
       LDA    $E4,X                   ;4
       STA    $CF                     ;3
       TXA                            ;2
       EOR    #$01                    ;2
       AND    $E0                     ;3
       STA    $EB                     ;3
       TAX                            ;2
       LDA    $E4,X                   ;4
       BEQ    LF457                   ;2
       LDA    #$01                    ;2
       DEC    $E4,X                   ;6
       BNE    LF46B                   ;2
    IF PLUSROM = 1
       JSR SendPlusROMScoreCheck
    ENDIF
LF457:
       LDA    $CF                     ;3
       BNE    LF441                   ;2
       LDY    #$80                    ;2
       STY    $C8                     ;3
       LDY    $A9                     ;3
       BNE    LF470                   ;2
       DEC    $A9                     ;5
       BNE    LF470                   ;2
LF467:
       LDA    $F0,X                   ;4
       BNE    LF441                   ;2
LF46B:
       PHA                            ;3
       JSR    LF328                   ;6
       PLA                            ;4
LF470:
       ORA    #$5E                    ;2
       STA    $E1                     ;3
LF474:
       LDY    #$08                    ;2
       LDA    $9B                     ;3
       BEQ    LF47E                   ;2
       DEC    $9B                     ;5
       BPL    LF4BD                   ;2 always branch

LF47E:
       LDX    $E1                     ;3
       BMI    LF49C                   ;2
       BNE    LF4DA                   ;2
       BIT    $EA                     ;3
       BMI    LF4DA                   ;2
       LDA    $80                     ;3
       AND    #$07                    ;2
       CLC                            ;2
       BIT    $D4                     ;3
       BMI    LF493                   ;2
       BVC    LF495                   ;2
LF493:
       ORA    #$04                    ;2
LF495:
       ADC    #$1A                    ;2
       TAX                            ;2
       LDA    #$02                    ;2
       BPL    LF4DA                   ;2 always branch

LF49C:
       TXA                            ;2
       LSR                            ;2
       LSR                            ;2
       TAX                            ;2
       AND    #$03                    ;2
       BEQ    LF4B7                   ;2
       TAY                            ;2
       TXA                            ;2
       LSR                            ;2
;       LSR                            ;2
;       AND    #$03                    ;2
       asr    #$07                    ;2 illegal

       TAX                            ;2
       JSR    LFF6E                   ;6
       BEQ    LF4B7                   ;2
       LDX    $EB                     ;3
       LDA    $E8,X                   ;4
       JSR    LF06C                   ;6
LF4B7:
       LDA    $E1                     ;3
       AND    #$BF                    ;2
       LDY    #$0F                    ;2
LF4BD:
       PHA                            ;3
       LSR                            ;2
       LSR                            ;2
       TAX                            ;2
       AND    #$07                    ;2
       STA    $CF                     ;3
       PLA                            ;4
       CPY    #$0C                    ;2
       BCC    LF4D0                   ;2
       AND    #$03                    ;2
       ORA    #$04                    ;2
       BCS    LF4DA                   ;2 always branch

LF4D0:
       PHA                            ;3
       AND    #$01                    ;2
       EOR    #$19                    ;2
       SBC    $CF                     ;3
       TAX                            ;2
       PLA                            ;4
       LSR                            ;2
LF4DA:
       STA    AUDV1                   ;3
       STY    AUDC1                   ;3
       STX    AUDF1                   ;3
       LDA    $AD                     ;3
       AND    #$F8                    ;2
       BNE    LF529                   ;2
       LDX    $EB                     ;3
       LDY    REFP1,X                 ;4
       BMI    LF525                   ;2
       STA    $EA                     ;3
       LDA    $E1                     ;3
       BNE    LF525                   ;2
       LDY    $C2                     ;3
       SEC                            ;2
       LDA    $C7                     ;3
       ADC    LFE8C,Y                 ;4
       BPL    LF4FD                   ;2
       TXA                            ;2
LF4FD:
       LSR                            ;2
       LSR                            ;2
       STA    $AC                     ;3
       LDA    LFDF7,X                 ;4
       LDX    #$11                    ;2
       STX    $AD                     ;3
       AND    SWCHB                   ;4
       BEQ    LF513                   ;2
       ASL    $AD                     ;5
       DEC    $AD                     ;5
       INY                            ;2
       INY                            ;2
LF513:
       STY    $99                     ;3
       LDA    #$00                    ;2
LF517:
       STA    $AF,X                   ;4
       DEX                            ;2
       DEX                            ;2
       DEX                            ;2
       BPL    LF517                   ;2
       LDA    $C8                     ;3
       CLC                            ;2
       ADC    #$03                    ;2
       STA    $C1                     ;3
LF525:
       LDA    $AD                     ;3
       BEQ    LF59B                   ;2
LF529:
       BIT    $CA                     ;3
       LDA    $99                     ;3
       LSR                            ;2
       BCS    LF531                   ;2
       CLV                            ;2
LF531:
       LDX    #$02                    ;2
       LDY    #$03                    ;2
       JSR    LFD82                   ;6
       LDY    $AC                     ;3
       DEC    $AD                     ;5
       CPY    #$20                    ;2
       LDA    $C0                     ;3
       BCS    LF589                   ;2
       CPY    #$08                    ;2
       BCS    LF54D                   ;2
       LDA    $B4                     ;3
       ORA    LFF39,Y                 ;4
       STA    $B4                     ;3
LF54D:
       CPY    #$05                    ;2
       BCC    LF57C                   ;2
       CPY    #$10                    ;2
       BCS    LF55C                   ;2
       LDA    $B7                     ;3
       ORA    LFF27,Y                 ;4
       STA    $B7                     ;3
LF55C:
       CPY    #$0D                    ;2
       BCC    LF57C                   ;2
       CPY    #$14                    ;2
       BCS    LF56D                   ;2
       LDA    LFF23,Y                 ;4
       AND    #$F0                    ;2
       ORA    $BA                     ;3
       STA    $BA                     ;3
LF56D:
       CPY    #$11                    ;2
       BCC    LF57C                   ;2
       CPY    #$1C                    ;2
       BCS    LF57C                   ;2
       LDA    $BD                     ;3
       ORA    LFF25,Y                 ;4
       STA    $BD                     ;3
LF57C:
       LDA    $C0                     ;3
       CPY    #$19                    ;2
       BCC    LF589                   ;2
       CPY    #$20                    ;2
       BCS    LF589                   ;2
       ORA    LFF13,Y                 ;4
LF589:
       STA    $C0                     ;3
       TYA                            ;2
       CLC                            ;2
       LDY    $99                     ;3
       ADC    LFF29,Y                 ;4
       STA    $AC                     ;3
       LDA    $AD                     ;3
       CPY    #$02                    ;2
       BCC    LF59B                   ;2
       LSR                            ;2
LF59B:
       STA    AUDV0                   ;3
       LDY    #$0F                    ;2
       STY    AUDC0                   ;3
       CLC                            ;2
       EOR    #$0E                    ;2
       ADC    #$02                    ;2
       STA    AUDF0                   ;3
       LDX    #$03                    ;2
LF5AA:
       JSR    LF2F9                   ;6
       STA    $DD                     ;3
       LDA    $A1,X                   ;4
       BEQ    LF612                   ;2
       CMP    #$06                    ;2
       BCC    LF617                   ;2
       LDA    $E1                     ;3
       BNE    LF617                   ;2
       LDA    $80                     ;3
       LSR                            ;2
       LSR                            ;2
       LDY    LFEF0,X                 ;4
       LDA.wy $85,Y                   ;4
       AND    LFDF6,X                 ;4
       TAY                            ;2
       DEY                            ;2
       BMI    LF5CF                   ;2
       LDA    #$01                    ;2
       TAY                            ;2
LF5CF:
       STY    $D0                     ;3
       LDY    $A1,X                   ;4
       ROL                            ;2
       ORA    #$08                    ;2
       CPY    #$08                    ;2
       BCS    LF5DD                   ;2
       LSR                            ;2
       EOR    #$02                    ;2
LF5DD:
       STA    $A1,X                   ;4
;       AND    #$04                    ;2
;       LSR                            ;2
       asr    #$04                    ;2 illegal

       LDY    $EB                     ;3
       ADC.wy $E8,Y                   ;4
       LSR                            ;2
       TAY                            ;2
       CLC                            ;2
       DEY                            ;2
       BPL    LF5F0                   ;2
       LDA    $80                     ;3
       LSR                            ;2
LF5F0:
       LDA    #$00                    ;2
       BCS    LF5FA                   ;2
LF5F4:
       CLC                            ;2
       ADC    $D0                     ;3
       DEY                            ;2
       BPL    LF5F4                   ;2
LF5FA:
       CLC                            ;2
       ADC    $91,X                   ;4
       CMP    #$C0                    ;2
       BCS    LF629                   ;2
       STA    $D0                     ;3
       LDY    $DD                     ;3
       BEQ    LF647                   ;2
       CMP    LFE4E,Y                 ;4
       BCS    LF61A                   ;2
       LDA    #$FF                    ;2
       DEY                            ;2
       SEC                            ;2
       BCS    LF627                   ;2 always branch

LF612:
       LDA    LFE60,X                 ;4
       STA    $95,X                   ;4
LF617:
       JMP    LF67F                   ;3

LF61A:
       CMP    LFE4F,Y                 ;4
       BCC    LF643                   ;2
       LDA    #$00                    ;2
       CPY    $AE                     ;3
       BEQ    LF629                   ;2
       CPY    #$07                    ;2
LF627:
       BNE    LF637                   ;2
LF629:
       LDY    LFEF0,X                 ;4
       LDA.wy $85,Y                   ;4
       EOR    LFDF6,X                 ;4
       STA.wy $85,Y                   ;5
       BCS    LF647                   ;2
LF637:
       LDY    LFEF0,X                 ;4
       EOR    LFE88,X                 ;4
       ADC.wy $85,Y                   ;4
       STA.wy $85,Y                   ;5
LF643:
       LDA    $D0                     ;3
       STA    $91,X                   ;4
LF647:
       TXA                            ;2
       TAY                            ;2
       LDA    $F3                     ;3
LF64B:
       LSR                            ;2
       LSR                            ;2
       DEY                            ;2
       BNE    LF64B                   ;2
       AND    #$01                    ;2
       ADC    #$FF                    ;2
       CLC                            ;2
       ADC    $95,X                   ;4
       CMP    LFE60,X                 ;4
       BCS    LF612                   ;2
       CPX    #$03                    ;2
       BEQ    LF66C                   ;2
       LDY    $A2,X                   ;4
       BEQ    LF66C                   ;2
       PHA                            ;3
       ADC    #$17                    ;2
       CMP    $96,X                   ;4
       PLA                            ;4
       BCS    LF67F                   ;2
LF66C:
       CMP    LFE63,X                 ;4
       BCC    LF67F                   ;2
       LDY    $A0,X                   ;4
       BEQ    LF67D                   ;2
       PHA                            ;3
       SBC    #$17                    ;2
       CMP    $94,X                   ;4
       PLA                            ;4
       BCC    LF67F                   ;2
LF67D:
       STA    $95,X                   ;4
LF67F:
       LDA    $9D,X                   ;4
       AND    #$0F                    ;2
       TAY                            ;2
       BEQ    LF690                   ;2
       DEY                            ;2
       LSR                            ;2
       BEQ    LF68E                   ;2
       LSR                            ;2
       LSR                            ;2
       ADC    #$01                    ;2
LF68E:
       STA    $A1,X                   ;4
LF690:
       STY    $9D,X                   ;4
       TXA                            ;2
       BNE    LF6BB                   ;2
       LDA    $A8                     ;3
       SEC                            ;2
       SBC    #$08                    ;2
       BCC    LF6A4                   ;2
       STA    $A8                     ;3
       AND    #$0F                    ;2
       CMP    #$08                    ;2
       BCC    LF6A9                   ;2
LF6A4:
       LDA    $87                     ;3
       LSR                            ;2
       LSR                            ;2
       LSR                            ;2
LF6A9:
       AND    #$07                    ;2
       BEQ    LF71E                   ;2
       TAY                            ;2
       LDA    LFE80,Y                 ;4
       TAY                            ;2
       AND    #$07                    ;2
       STA    $95                     ;3
       TYA                            ;2
       AND    #$F0                    ;2
       BNE    LF6C1                   ;2
LF6BB:
       LDA    $A1,X                   ;4
       BEQ    LF71E                   ;2
       LDA    $91,X                   ;4
LF6C1:
       CLC                            ;2
       ADC    $C4                     ;3
       PHP                            ;3
       PHA                            ;3
       LDA    $C3                     ;3
       LSR                            ;2
       PLA                            ;4
       BCS    LF6CE                   ;2
       ADC    #$60                    ;2
LF6CE:
       STA    $91                     ;3
       LDA    #$00                    ;2
       ADC    #$FF                    ;2
       PLP                            ;4
       ADC    #$00                    ;2
       BMI    LF6FA                   ;2
       BNE    LF71E                   ;2
       LDA    $91                     ;3
       CMP    #$A0                    ;2
       BCS    LF71E                   ;2
       CPX    #$00                    ;2
       BNE    LF724                   ;2
       CMP    #$80                    ;2
LF6E7:
       LDY    #$01                    ;2
       BCS    LF6F1                   ;2
       CMP    #$60                    ;2
       LDY    #$03                    ;2
       BCC    LF724                   ;2
LF6F1:
       PHA                            ;3
       TYA                            ;2
       AND    $95                     ;3
       STA    $95                     ;3
       PLA                            ;4
       BCS    LF724                   ;2
LF6FA:
       TXA                            ;2
       BNE    LF71E                   ;2
       LDA    $91                     ;3
       CMP    #$C0                    ;2
       BCC    LF71E                   ;2
       ADC    #$1F                    ;2
       TAY                            ;2
       BMI    LF715                   ;2
       LDA    $95                     ;3
       LSR                            ;2
       LSR                            ;2
       BCC    LF714                   ;2
       ASL                            ;2
       STA    $95                     ;3
       TYA                            ;2
       BCC    LF724                   ;2
LF714:
       TYA                            ;2
LF715:
       CLC                            ;2
       ADC    #$20                    ;2
       LDY    $95                     ;3
       CPY    #$04                    ;2
       BCS    LF6E7                   ;2
LF71E:
       LDA    #$00                    ;2
       STA    $95                     ;3
       STA    $91                     ;3
LF724:
       JSR    LF014                   ;6
       ASL                            ;2
       ASL                            ;2
       ASL                            ;2
       ASL                            ;2
       ORA    $9D,X                   ;4
       STA    $9D,X                   ;4
       TYA                            ;2
       SEC                            ;2
       SBC    #$06                    ;2
       BMI    LF738                   ;2
       EOR    #$80                    ;2
       TAY                            ;2
LF738:
       STY    $8D,X                   ;4
       TXA                            ;2
       BEQ    LF79D                   ;2
       EOR    $F3                     ;3
       AND    #$03                    ;2
       ORA    $A5                     ;3
       ORA    $E1                     ;3
       BNE    LF799                   ;2
       LDA    $F3                     ;3
       AND    #$02                    ;2
       LSR                            ;2
       TAY                            ;2
       DEY                            ;2
       LDA    $D0                     ;3
       BEQ    LF799                   ;2
       SBC    $C7                     ;3
       BPL    LF757                   ;2
       INY                            ;2
LF757:
       LDA    $C8                     ;3
       CMP    $95,X                   ;4
       LDA    $F3                     ;3
       AND    #$60                    ;2
       BNE    LF765                   ;2
       TAY                            ;2
       CLC                            ;2
       BCC    LF769                   ;2 always branch

LF765:
       LDA    $F3                     ;3
       AND    #$01                    ;2
LF769:
       SBC    #$00                    ;2
       STY    $A6                     ;3
       STA    $A7                     ;3
       LDY    $EB                     ;3
       LDA.wy $E8,Y                   ;4
       CMP    #$03                    ;2
       LDA    $F2                     ;3
       BCC    LF77F                   ;2
       LSR                            ;2
       ASL    $A7                     ;5
       ASL    $A6                     ;5
LF77F:
       LSR                            ;2
       SEC                            ;2
       ROR                            ;2
       STA    $A5                     ;3
       LDA    $95,X                   ;4
       CLC                            ;2
       ADC    #$04                    ;2
       STA    $DF                     ;3
       STA    $DE                     ;3
       DEC    $DF                     ;5
       LDA    $D0                     ;3
       CMP    #$9C                    ;2
       BCS    LF797                   ;2
       ADC    #$03                    ;2
LF797:
       STA    $C6                     ;3
LF799:
       DEX                            ;2
       JMP    LF5AA                   ;3

LF79D:
       LDA    $C1                     ;3
       CLC                            ;2
       SBC    $C8                     ;3
       TAY                            ;2
       INX                            ;2
LF7A4:
       LDA    #$00                    ;2
       CPY    #$0A                    ;2
       BCS    LF7AC                   ;2
       LDA    ($C9),Y                 ;5
LF7AC:
       STA    $CD,X                   ;4
       INY                            ;2
       DEX                            ;2
       BPL    LF7A4                   ;2
       LDA    $B1                     ;3
       AND    #$F0                    ;2
       LDX    $C1                     ;3
       CPX    $DF                     ;3
       BEQ    LF7C4                   ;2
       CPX    $82                     ;3
       BNE    LF7C6                   ;2
       LDY    $DF                     ;3
       STY    $82                     ;3
LF7C4:
       ORA    #$02                    ;2
LF7C6:
       STA    $B1                     ;3
       DEX                            ;2
       LDA    $BA                     ;3
       AND    #$F0                    ;2
       CPX    $DF                     ;3
       BEQ    LF7D9                   ;2
       CPX    $82                     ;3
       BNE    LF7DB                   ;2
       LDY    $DF                     ;3
       STY    $82                     ;3
LF7D9:
       ORA    #$02                    ;2
LF7DB:
       STA    $BA                     ;3
       LDX    #$02                    ;2
LF7DF:
       LDY    #$FF                    ;2
       LDA    $C3,X                   ;4
       SEC                            ;2
LF7E4:
       SBC    #$14                    ;2
       INY                            ;2
       BCS    LF7E4                   ;2
       TYA                            ;2
       ASL                            ;2
       ASL                            ;2
       STA    $F9,X                   ;4
       DEX                            ;2
       BNE    LF7DF                   ;2
       ASL                            ;2
       ASL                            ;2
       ASL                            ;2
       TAY                            ;2
       LDA    $C3                     ;3
       LSR                            ;2
       TYA                            ;2
       ROR                            ;2
       EOR    #$70                    ;2
       STA    $FA                     ;3
       INX                            ;2
       LDY    $EB                     ;3
       LDA    LFECA,Y                 ;4
       STA    $D1                     ;3
       ADC    #$08                    ;2
       JSR    LF253                   ;6
       DEX                            ;2
       LDA    $D1                     ;3
       JSR    LF253                   ;6
       LDY    $EB                     ;3
       LDA    #$96                    ;2
       STA    $CF                     ;3
       LDX    #$0A                    ;2
LF819:
       LDA.wy $EC,Y                   ;4
       LSR                            ;2
       JSR    LFDD8                   ;6
       LDA.wy $EC,Y                   ;4
       INY                            ;2
       INY                            ;2
       ASL                            ;2
       ASL                            ;2
       ASL                            ;2
       JSR    LFDD8                   ;6
       BPL    LF819                   ;2
LF82D:
       LDY    INTIM                   ;4
       BNE    LF82D                   ;2
       STA    WSYNC                   ;3
       STY    VBLANK                  ;3
       LDA    #$30                    ;2
       STA    CTRLPF                  ;3
       LDX    $EB                     ;3
       LDA    $F6,X                   ;4
       LDY    #$06                    ;2
       STA    COLUP0                  ;3
       STA    COLUP1                  ;3
       BIT    $CA                     ;3
       LDA    $EB                     ;3
       BEQ    LF84D                   ;2
       CLV                            ;2
       STA    WSYNC                   ;3
LF84D:
       LDA    ($DC),Y                 ;5
       STA    GRP0                    ;3
       LDA    ($DA),Y                 ;5
       STA    GRP1                    ;3
       BVC    LF859                   ;2
       STA    WSYNC                   ;3
LF859:
       STY    $CF                     ;3
       LDA    ($D8),Y                 ;5
       STA    GRP0                    ;3
       LDA    ($D6),Y                 ;5
       STA    $D0                     ;3
       LDA    ($D4),Y                 ;5
       TAX                            ;2
       LDA    ($D2),Y                 ;5
       LDY    $D0                     ;3
       STY    GRP1                    ;3
       STX    GRP0                    ;3
       STA    GRP1                    ;3
       STA    GRP0                    ;3
       LDY.w  $CF                     ;4
       BEQ    LF885                   ;2
       DEY                            ;2
       BVC    LF84D                   ;2
       LDA    ($DC),Y                 ;5
       STA    GRP0                    ;3
       LDA    ($DA),Y                 ;5
       STA.w  GRP1                    ;4
       BVS    LF859                   ;2 always branch

LF885:
       STA    WSYNC                   ;3
       STY    VDELP0                  ;3
       STY    GRP1                    ;3
       STY    GRP0                    ;3
       LDX    $EB                     ;3
       LDY    $E4,X                   ;4
       LDA    LFDF8,Y                 ;4
       STA    $CF                     ;3
       STA    NUSIZ0                  ;3
       LSR                            ;2
       LSR                            ;2
       LSR                            ;2
       STA    NUSIZ1                  ;3
       LDX    #$0A                    ;2
LF89F:
       STA    WSYNC                   ;3
       LDA    LFEB5,X                 ;4
       BIT    $CF                     ;3
       BMI    LF8AA                   ;2
       STA    GRP1                    ;3
LF8AA:
       BVS    LF8AE                   ;2
       LDA    #$00                    ;2
LF8AE:
       STA    GRP0                    ;3
       LDA    $82,X                   ;4
       ASL                            ;2 0F
       ASL                            ;2 0E
       ASL                            ;2 1C
       AND    #$38                    ;2
       TAY                            ;2
       LDA    LFE07,Y                 ;4
       STA    $D3,X                   ;4
       DEX                            ;2
       BNE    LF89F                   ;2
       LDA    $C5                     ;3
       JSR    LF253                   ;6
       INX                            ;2
       LDA    $C5                     ;3
       SEC                            ;2
       SBC    #$20                    ;2
       BPL    LF8CF                   ;2
       EOR    #$60                    ;2
LF8CF:
       JSR    LF253                   ;6
       LDA    $F8                     ;3
       STA    COLUPF                  ;3
    IF PAL = 1
       LDA    #$44                    ;2
    ELSE
       LDA    #$34                    ;2
    ENDIF
       EOR    $F5                     ;3
       AND    $F4                     ;3
       STA    COLUP0                  ;3
       STA    COLUP1                  ;3
       STY    NUSIZ0                  ;3
       STY    NUSIZ1                  ;3
       LDX    #$0B                    ;2
       STA    HMCLR                   ;3
LF8E8:
       LDA    LFF1D-6,X               ;4
       EOR    $F5                     ;3
       STA    WSYNC                   ;3
       STA    HMOVE                   ;3
       AND    $F4                     ;3
       STA    COLUBK                  ;3
       LDA    LFE8D-6,X               ;4
       STA    GRP1                    ;3
       LDA    LFDF0-6,X               ;4
       STA    GRP0                    ;3
       LDA    LFF17-6,X               ;4
       EOR    $F5                     ;3
       AND    $F4                     ;3
       STA    $C9,X                   ;4
       DEX                            ;2
       CPX    #$06                    ;2
       BCS    LF8E8                   ;2
       LDY    #$01                    ;2
       LDA    #$70                    ;2
       STA    HMP1                    ;3
       LDA    #$80                    ;2
       STA    HMP0                    ;3
LF917:
       LDA    $CF,X                   ;4
       STA    WSYNC                   ;3
       STA    HMOVE                   ;3
       STA    COLUBK                  ;3
       LDA    LFE8D-6,X               ;4
       STA    GRP0                    ;3
       LDA.wy $AF,Y                   ;4
       STA    PF0                     ;3
       LDA.wy $B2,Y                   ;4
       STA    PF1                     ;3
       LDA.wy $B5,Y                   ;4
       STA    PF2                     ;3
       LDA.wy $B8,Y                   ;4
       STA    PF0                     ;3
       LDA.wy $BB,Y                   ;4
       STA    PF1                     ;3
       LDA.wy $BE,Y                   ;4
       STA    PF2                     ;3
       CPX    #$03                    ;2
       BNE    LF949                   ;2
       STX    $D5                     ;3
       DEY                            ;2
LF949:
       DEX                            ;2
       BPL    LF917                   ;2
       STA    WSYNC                   ;3
       INX                            ;2
       LDA    $F8                     ;3
       STA    COLUBK                  ;3
       STX    GRP1                    ;3
       STX    GRP0                    ;3
       STX    PF0                     ;3
       STX    COLUPF                  ;3
       STX    PF1                     ;3
       STX    PF2                     ;3
       STX    NUSIZ0                  ;3
       LDY    $EB                     ;3
       LDA    $E1                     ;3
       BPL    LF968                   ;2
       LSR                            ;2
LF968:
       EOR    #$60                    ;2
       LSR                            ;2
       LSR                            ;2
       CMP    #$08                    ;2
       ORA    #$28                    ;2
       EOR    $F5                     ;3
       AND    $F4                     ;3
       BCC    LF979                   ;2
       LDA.wy $F6,Y                   ;4
LF979:
       STA    COLUP0                  ;3
       LDY    $C2                     ;3
       DEY                            ;2
       STY    REFP0                   ;3
       LDA    #$20                    ;2
       EOR    $F5                     ;3
       AND    $F4                     ;3
       STA    HMCLR                   ;3
       STA    COLUBK                  ;3
       EOR    #$06                    ;2
       STA    $F8                     ;3
       LDA    $C7                     ;3
       JSR    LF014                   ;6
       ASL                            ;2
       ASL                            ;2
       ASL                            ;2
       ASL                            ;2
       STA    HMP0,X                  ;4
       LDA    #$15                    ;2
       STA    NUSIZ0                  ;3
       LDA    $F8                     ;3
       EOR    #$04                    ;2
       STA    COLUBK                  ;3
       EOR    #$06                    ;2
       STA    WSYNC                   ;3
       STA    COLUBK                  ;3
       LDA    $F8                     ;3
       STX    $CF                     ;3
       JSR    LF260                   ;6
       STA    COLUBK                  ;3
       LDA    $AD                     ;3
       CLC                            ;2
    IF PAL = 1
       ADC    #$BF                    ;2
    ELSE
       ADC    #$6F                    ;2
    ENDIF
       EOR    #$0F                    ;2
       AND    $F4                     ;3
       STA    COLUPF                  ;3
       JMP    LF0DE                   ;3

LF9C0:
       LDA    #$55                    ;2
       STA    $82                     ;3
       LDY    #$03                    ;2
       LDA    $F8                     ;3
    IF PAL = 1
       EOR    #$04                    ;2
    ELSE
       EOR    #$06                    ;2
    ENDIF
       STX    ENAM0                   ;3
       STA    COLUPF                  ;3
       JSR    LF265                   ;6
       STY    NUSIZ0                  ;3
       STY    NUSIZ1                  ;3
       STA    COLUBK                  ;3
       STX    GRP1                    ;3
       STX    GRP0                    ;3
       DEX                            ;2
       STX    PF0                     ;3
       STX    PF1                     ;3
       STY    PF2                     ;3
       LDX    #$15                    ;2
       STA    RESP0                   ;3
       STA    RESP1                   ;3
       STA    RESBL                   ;3
       LDA    #$20                    ;2
       STA    HMP1                    ;3
       STX    HMP0                    ;3
       STX    CTRLPF                  ;3
       JSR    LF265                   ;6
       LDX    #$07                    ;2
       STY    REFP0                   ;3
       STY    REFP1                   ;3
       LDA    $F8                     ;3
       EOR    #$02                    ;2
       STA    COLUP0                  ;3
       STA    COLUP1                  ;3
       STA    HMCLR                   ;3
LFA05:
       LDA    $85,X                   ;4
       AND    #$78                    ;2
       CPX    #$04                    ;2
       BCC    LFA0F                   ;2
       AND    #$38                    ;2
LFA0F:
       TAY                            ;2
       LDA    LFE07,Y                 ;4
       STA    $CE,X                   ;4
       LDY    $F9                     ;3
       TXA                            ;2
       ORA    $FB                     ;3
       STY    COLUBK                  ;3
       TAY                            ;2
       JSR    LF265                   ;6
       BCS    LFA29                   ;2
       LDA    LFFCD,Y                 ;4
       STA    GRP1                    ;3
       STA    GRP0                    ;3
LFA29:
       LDA    $F8                     ;3
       EOR    #$04                    ;2
       DEX                            ;2
       BPL    LFA05                   ;2
       STA    COLUBK                  ;3
       LDA    $C7                     ;3
       LSR                            ;2
       CLC                            ;2
       ADC    #$F2                    ;2
       STA    RESP1                   ;3
       EOR    #$F0                    ;2
       INX                            ;2
       LDY    $FA                     ;3
       STY    HMP0                    ;3
       STA    HMBL                    ;3
       STY    HMP1                    ;3
       STX    NUSIZ0                  ;3
       STX    VDEL01                  ;3
       JSR    LF265                   ;6
       LDA    $F6                     ;3
       EOR    #$02                    ;2
       STA    COLUP0                  ;3
       STA    COLUP1                  ;3
       LDA    $F8                     ;3
       STA    COLUBK                  ;3
       LDA    $C4                     ;3
       CMP    #$50                    ;2
       LDA    $C3                     ;3
       ROR                            ;2
       ROR                            ;2
       STA    $CB                     ;3
       LDA    $C8                     ;3
       AND    #$70                    ;2
       LSR                            ;2
       TAX                            ;2
       LDA    LFE0F,X                 ;4
       STA    $CD                     ;3
       STA    HMCLR                   ;3
       LDA    #$11                    ;2
LFA71:
       BEQ    LFA9F                   ;2
       SBC    #$04                    ;2
       TAX                            ;2
LFA76:
       LDA    $CD                     ;3
       STA    WSYNC                   ;3
       STA    HMOVE                   ;3
       STA    ENABL                   ;3
       LDA    $D1,X                   ;4
       STA    GRP0                    ;3
       LDA    $D0,X                   ;4
       STA    GRP1                    ;3
       LSR    $CD                     ;5
       BIT    $CB                     ;3
       BPL    LFA8E                   ;2
       LDA    $80                     ;3
LFA8E:
       BVS    LFA90                   ;2
LFA90:
       LDA    $CF,X                   ;4
       LDY    $CE,X                   ;4
       ASL    $82                     ;5
       STA    GRP1                    ;3
       TXA                            ;2
       STY    GRP1                    ;3
       BCS    LFA71                   ;2
       BCC    LFA76                   ;2 always branch

LFA9F:
    IF PAL = 1
       LDA    #$04                    ;2
    ELSE
       LDA    #$06                    ;2
    ENDIF
       JSR    LF265                   ;6
       EOR    $F8                     ;3
       STA    COLUBK                  ;3
       LDA    #$53                    ;2
       STA    VDELP0                  ;3
       STA    VDEL01                  ;3
       STX    GRP1                    ;3
       STX    GRP0                    ;3
       STX    GRP1                    ;3
       STX    PF0                     ;3
       STX    PF1                     ;3
       STA    RESP0                   ;3
       STA    RESP1                   ;3
       STX    PF2                     ;3
       STA    NUSIZ0                  ;3
       STA    HMP1                    ;3
       LDA    #$41                    ;2
       STA    HMP0                    ;3
       STA    $D0                     ;3
       LDA    #>GFX                   ;2
       JSR    LF265                   ;6
       STA    $CA                     ;3
       STA    $CC                     ;3

   IF PLUSROM = 1
       LDA    #$10                    ;2
   ELSE

       LDA    $A9                     ;3
       BEQ    LFAD9                   ;2
LFAD5:
       DEC    $A9                     ;5
       BEQ    LFAD5                   ;2
LFAD9:
       ASL                            ;2
       BPL    LFADE                   ;2
       AND    #$80                    ;2
LFADE:
       ROR                            ;2
       BMI    LFAE3                   ;2
       LDA    #$80                    ;2
LFAE3:
       LSR                            ;2
       LSR                            ;2
       LSR                            ;2
   ENDIF

       STA    $CF                     ;3
       STA    HMCLR                   ;3
;copyright message
LFAEA:
       LDY    $CF                     ;3
       STA    WSYNC                   ;3
       STA    HMOVE                   ;3
       LDA    Copyright1gfx-9,Y       ;4
       STA    GRP0                    ;3
       LDA    Copyright2gfx-9,Y       ;4
       STA    GRP1                    ;3
       LDA    Copyright3gfx-9,Y       ;4
       STA    GRP0                    ;3
       LDA    Copyright4gfx-9,Y       ;4
       STA    $D1                     ;3
       LDX    Copyright5gfx-9,Y       ;4
       LDA    Copyright6gfx-9,Y       ;4
       LDY    $D1                     ;3
       STY    GRP1                    ;3
       STX    GRP0                    ;3
       STA    GRP1                    ;3
       STA    GRP0                    ;3
       DEC    $CF                     ;5
       ASL    $D0                     ;5
       BNE    LFAEA                   ;2
       LDA    #$00                    ;2
       LDX    #$03                    ;2
       STA    GRP1                    ;3
       STA    GRP0                    ;3
       STA    HMOVE                   ;3
       STA    GRP1                    ;3

    IF PAL = 1
       LDA    #$45                    ;2
    ELSE
       LDA    #$21                    ;2
    ENDIF

       STA    TIM64T                  ;4
       JSR    LF265                   ;6
       STX    VBLANK                  ;3
       STX    ENABL                   ;3
       LDA    $E1                     ;3
       BNE    LFB75                   ;2
       BIT    COLUP1                  ;3
       BPL    LFB5F                   ;2
       LDA    $C8                     ;3
       ADC    #$0B                    ;2
LFB3E:
       CMP    $95,X                   ;4
       BCS    LFB5A                   ;2
       DEX                            ;2
       BNE    LFB3E                   ;2
       LDA    $C7                     ;3
       JSR    LFF41                   ;6
LFB4A:
       LDA    $E1                     ;3
       ORA    #$7F                    ;2
       STA    $E1                     ;3
       LDA    #$00                    ;2
       STA    $A5                     ;3
LFB54:
       LDA    #$1F                    ;2
       STA    $9B                     ;3
       BNE    LFB75                   ;2 always branch

LFB5A:
       JSR    LF030                   ;6
       BCC    LFB4A                   ;2
LFB5F:
       LDX    $90                     ;3
       JSR    LF030                   ;6
       BIT    VSYNC                   ;3
       BVS    LFB4A                   ;2
       BPL    LFB75                   ;2
       BIT    $8D                     ;3
       BMI    LFB75                   ;2
       LDA    $C6                     ;3
       JSR    LFF41                   ;6
       BNE    LFB54                   ;2
LFB75:
       LDX    #$04                    ;2
       LDA    #$01                    ;2
       JSR    LF253                   ;6
       TYA                            ;2
       INY                            ;2
       EOR    SWCHA                   ;4
       BEQ    LFB85                   ;2
       STY    $EA                     ;3
LFB85:
       LDX    $A9                     ;3
       BEQ    LFB8A                   ;2
       TYA                            ;2
LFB8A:
       LDY    $EB                     ;3
       BEQ    LFB92                   ;2
       ASL                            ;2
       ASL                            ;2
       ASL                            ;2
       ASL                            ;2
LFB92:
       STA    $D4                     ;3
       LDA    $80                     ;3
;       LSR                            ;2
;       AND    #$01                    ;2
       asr    #$02                    ;2 illegal

       TAY                            ;2
       LDX    LFF16,Y                 ;4
       LDY    $AA                     ;3
       BIT    $D4                     ;3
       BMI    LFBAF                   ;2
       BVS    LFBAA                   ;2
       TYA                            ;2
       BEQ    LFBB4                   ;2
       BMI    LFBAF                   ;2
LFBAA:
       DEY                            ;2
       CPY    #$BF                    ;2
       BNE    LFBB4                   ;2
LFBAF:
       CPY    #$40                    ;2
       BEQ    LFBB4                   ;2
       INY                            ;2
LFBB4:
       LDA    $E1                     ;3
       PHP                            ;3
       BEQ    LFBEE                   ;2
       LDY    $9B                     ;3
       BNE    LFBDF                   ;2
       DEC    $E1                     ;5
       BNE    LFBDF                   ;2
       LDX    $F0                     ;3
       DEX                            ;2
       BPL    LFBDB                   ;2
       LDX    $EB                     ;3
       LDA    #$5F                    ;2
       LDY    $E4,X                   ;4
       BEQ    LFBDD                   ;2
       LDA    #$0F                    ;2
       CMP    $D4                     ;3
       ROR                            ;2
       AND    REFP1,X                 ;4
       BMI    LFBDB                   ;2
       STY    $EA                     ;3
       BPL    LFBDF                   ;2 always branch

LFBDB:
       LDA    #$01                    ;2
LFBDD:
       STA    $E1                     ;3
LFBDF:
       LDX    #$B6                    ;2
       LDY    #$00                    ;2
       CMP    #$60                    ;2
       BCC    LFBEE                   ;2
       ASL                            ;2
       BCC    LFBEC                   ;2
       BPL    LFBEE                   ;2
LFBEC:
       LDX    #$AD                    ;2
LFBEE:
       STX    $C9                     ;3
       STY    $AA                     ;3
       TYA                            ;2
       CMP    #$80                    ;2
       ROR                            ;2
       STA    $D0                     ;3
       ROL                            ;2
       LDY    #$00                    ;2
       CLC                            ;2
       ADC    $AB                     ;3
       BPL    LFC0C                   ;2
LFC00:
       CMP    #$DF                    ;2
       BCS    LFC10                   ;2
       ADC    #$20                    ;2
       DEY                            ;2
       BMI    LFC00                   ;2
LFC09:
       SBC    #$20                    ;2
       INY                            ;2
LFC0C:
       CMP    #$20                    ;2
       BCS    LFC09                   ;2
LFC10:
       STA    $AB                     ;3
       STY    $CF                     ;3
       LDY    #$00                    ;2
       PLP                            ;4
       BPL    LFC1D                   ;2
       STY    $CF                     ;3
       BMI    LFC84                   ;2 always branch

LFC1D:
       BNE    LFC49                   ;2
       BIT    $D4                     ;3
       BVS    LFC26                   ;2
       BPL    LFC34                   ;2
       INY                            ;2
LFC26:
       CPY    $C2                     ;3
       STY    $C2                     ;3
       BEQ    LFC34                   ;2
       CLC                            ;2
       LDA    $C7                     ;3
       ADC    LFECF,Y                 ;4
       STA    $C7                     ;3
LFC34:
       LDA    $D4                     ;3
       ASL                            ;2
       ASL                            ;2
       LDY    $C8                     ;3
       ASL                            ;2
       BPL    LFC40                   ;2
       INY                            ;2
       CPY    #$60                    ;2
LFC40:
       BCC    LFC47                   ;2
       CPY    #$05                    ;2
       BCC    LFC47                   ;2
       DEY                            ;2
LFC47:
       STY    $C8                     ;3
LFC49:
       LDY    $C2                     ;3
;       LDA    $C7                     ;3
;       TAX                            ;2
       lax    $C7                     ;3 illegal
       SEC                            ;2
       SBC    $D0                     ;3
       CMP    #$C8                    ;2
       BCC    LFC57                   ;2
       LDA    #$00                    ;2
LFC57:
       AND    #$FE                    ;2
       DEY                            ;2
       BMI    LFC72                   ;2
       CMP    #$10                    ;2
       BCC    LFC76                   ;2
LFC60:
       BEQ    LFC84                   ;2
       CPX    #$11                    ;2
       BEQ    LFC84                   ;2
       BCC    LFC76                   ;2
LFC68:
       DEC    $C7                     ;5
       CPX    #$83                    ;2
       BCS    LFC84                   ;2
       INC    $CF                     ;5
       BCC    LFC84                   ;2
LFC72:
       CMP    #$86                    ;2
       BCS    LFC60                   ;2
LFC76:
       CPX    #$86                    ;2
       BEQ    LFC84                   ;2
       BCS    LFC68                   ;2
       INC    $C7                     ;5
       CPX    #$15                    ;2
       BCC    LFC84                   ;2
       DEC    $CF                     ;5
LFC84:
       SEC                            ;2
       LDA    $C5                     ;3
       PHA                            ;3
       LDX    $EB                     ;3
       DEC    $9A                     ;5
       BPL    LFCB0                   ;2
       LDA    $E1                     ;3
       BNE    LFCB0                   ;2
       ROL                            ;2
       EOR    $A1                     ;3
       STA    $A1                     ;3
       LDA    #$04                    ;2
       SBC    $E8,X                   ;4
       STA    $9A                     ;3
       AND    $AA                     ;3
       EOR    #$80                    ;2
       ORA    $C4                     ;3
       ROL                            ;2
       ROL                            ;2
       ORA    $C3                     ;3
       LSR                            ;2
       LDA    #$02                    ;2
       BCC    LFCAE                   ;2
       ADC    #$04                    ;2
LFCAE:
       STA    $AE                     ;3
LFCB0:
       LDA    #$FF                    ;2
       LDX    #$00                    ;2
LFCB4:
       ADC    $C4,X                   ;4
       SEC                            ;2
       SBC    $CF                     ;3
       STA    $C4,X                   ;4
       CMP    #$A0                    ;2
       BCC    LFD25                   ;2
       CMP    #$C0                    ;2
       EOR    #$A0                    ;2
       BCC    LFCC7                   ;2
       EOR    #$C0                    ;2
LFCC7:
       STA    $C4,X                   ;4
       TXA                            ;2
       BNE    LFD25                   ;2
       LDA    $C3                     ;3
       ROR                            ;2
       BMI    LFD46                   ;2
       DEC    $C3                     ;5
       BCC    LFD25                   ;2
       LDA    $89                     ;3
       PHA                            ;3
       LDA    $85                     ;3
       PHA                            ;3
       LDA    $E7                     ;3
       DEX                            ;2
       BCS    LFCE8                   ;2
LFCE0:
       LDY    $86,X                   ;4
       STY    $85,X                   ;4
       LDY    $8A,X                   ;4
       STY    $89,X                   ;4
LFCE8:
       LSR                            ;2
       ROR    $E6                     ;5
       ROR    $E7                     ;5
       INX                            ;2
       CPX    #$03                    ;2
       BCC    LFCE0                   ;2
LFCF2:
       PLA                            ;4
       STA    $85,X                   ;4
       PLA                            ;4
       STA    $89,X                   ;4
       LDX    #$03                    ;2
       LDA    $E6                     ;3
       STA    $D1                     ;3
LFCFE:
       JSR    LF2F9                   ;6
       LSR    $D1                     ;5
       LDA    #$00                    ;2
       STA    $9D,X                   ;4
       DEY                            ;2
       INY                            ;2
       BEQ    LFD0F                   ;2
       ROL                            ;2
       ASL                            ;2
       ADC    #$06                    ;2
LFD0F:
       STA    $A1,X                   ;4
       JSR    LFD6C                   ;6
       AND    #$0F                    ;2
       ADC    LFE4E,Y                 ;4
       STA    $92,X                   ;4
       AND    #$03                    ;2
       ADC    LFF1A,X                 ;4
       STA    $96,X                   ;4
       TXA                            ;2
       BNE    LFCFE                   ;2
LFD25:
       INX                            ;2
       CPX    #$03                    ;2
       LDA    #$00                    ;2
       BCC    LFCB4                   ;2
       TAX                            ;2
       TAY                            ;2
       PLA                            ;4
       EOR    $C5                     ;3
       AND    #$0C                    ;2
       BEQ    LFD3B                   ;2
       INY                            ;2
       BIT    $CF                     ;3
       JSR    LFD82                   ;6
LFD3B:
       LDA    #$06                    ;2
       JSR    LFD68                   ;6
       JSR    LF1F9                   ;6
       JMP    LF372                   ;3

LFD46:
       INC    $C3                     ;5
       BCS    LFD25                   ;2
       LDX    #$04                    ;2
       LDA    $8C                     ;3
       PHA                            ;3
       LDA    $88                     ;3
       PHA                            ;3
       LDA    $E6                     ;3
       BCC    LFD5E                   ;2
LFD56:
       LDY    $84,X                   ;4
       STY    $85,X                   ;4
       LDY    $88,X                   ;4
       STY    $89,X                   ;4
LFD5E:
       ASL                            ;2
       ROL    $E7                     ;5
       ROL    $E6                     ;5
       DEX                            ;2
       BNE    LFD56                   ;2
       BEQ    LFCF2                   ;2 always branch

LFD68:
       AND    $80                     ;3
       BNE    LFD76                   ;2
LFD6C:
       LDA    $F3                     ;3
       ASL                            ;2
       ASL                            ;2
       ASL                            ;2
       EOR    $F3                     ;3
       ASL                            ;2
       ROL    $F3                     ;5
LFD76:
       LDA    $F2                     ;3
       ASL                            ;2
       ASL                            ;2
       ASL                            ;2
       EOR    $F2                     ;3
       ASL                            ;2
       ROL    $F2                     ;5
       DEX                            ;2
       RTS                            ;6







;85
LFD82:
       BVC    LFDA6                   ;2
       LDA    $BE,X                   ;4
       LSR                            ;2
       LSR                            ;2
       LSR                            ;2
;       LSR                            ;2
;       AND    #$08                    ;2
       asr    #$10                    ;2 illegal

       ORA    $AF,X                   ;4
       ASL                            ;2
       STA    $AF,X                   ;4
       ROR    $B2,X                   ;6
       ROL    $B5,X                   ;6
       ROL                            ;2
       ASL                            ;2
       ASL                            ;2
       ASL                            ;2
       AND    #$08                    ;2
       ORA    $B8,X                   ;4
       ASL                            ;2
       STA    $B8,X                   ;4
       ROR    $BB,X                   ;6
       ROL    $BE,X                   ;6
       BVS    LFDBE                   ;2
LFDA6:
       LDA    $AF,X                   ;4
       ASL                            ;2
       ASL                            ;2
       ASL                            ;2
       ASL                            ;2
       ROR    $BE,X                   ;6
       ROL    $BB,X                   ;6
       ROR    $B8,X                   ;6
       LDA    $B8,X                   ;4
       LSR                            ;2
       LSR                            ;2
       LSR                            ;2
       LSR                            ;2
       ROR    $B5,X                   ;6
       ROL    $B2,X                   ;6
       ROR    $AF,X                   ;6
LFDBE:
       LDA    $B8,X                   ;4
       AND    #$F0                    ;2
       STA    $B8,X                   ;4
       LDA    $AF,X                   ;4
       AND    #$F0                    ;2
       STA    $AF,X                   ;4
       INX                            ;2
       CPX    #$03                    ;2
       BNE    LFDD4                   ;2
       DEX                            ;2
       AND    #$C0                    ;2
       STA    $AF,X                   ;4
LFDD4:
       DEY                            ;2
       BPL    LFD82                   ;2
       RTS                            ;6


;24
LFDD8:
       AND    #$78                    ;2
       BNE    LFDE1                   ;2
       TXA                            ;2
       BEQ    LFDE1                   ;2
       LDA    $CF                     ;3
LFDE1:
       STA    $D2,X                   ;4
       BMI    LFDE9                   ;2
       LDA    #$00                    ;2
       STA    $CF                     ;3
LFDE9:
       LDA    #>DigitGFX              ;2
       STA    $D3,X                   ;4
       DEX                            ;2
       DEX                            ;2
       RTS                            ;6



;45
LFF41:
       SEC                            ;2
       SBC    $C4                     ;3
       PHA                            ;3
       LDA    $C3                     ;3
       LSR                            ;2
       PLA                            ;4
       BCS    LFF4D                   ;2
       SBC    #$5F                    ;2
LFF4D:
       CLC                            ;2
       SBC    #$28                    ;2
       LDY    #$03                    ;2
LFF52:
       SBC    #$20                    ;2
       BCC    LFF5A                   ;2
       DEY                            ;2
       BNE    LFF52                   ;2
LFF59:
       RTS                            ;6
LFF5A:
       LDA    $91                     ;3
       ORA    $95                     ;3
       BEQ    LFF59                   ;2
       LDX    #$02                    ;2
       JSR    LFF6E                   ;6
       BEQ    LFF59                   ;2
       LDA    $CF                     ;3
       ORA    #$F8                    ;2
       STA    $A8                     ;3
       RTS                            ;6





;51
LFF6E:
       LDA    $85,X                   ;4
       AND    #$38                    ;2
       LSR                            ;2
       LSR                            ;2
       LSR                            ;2
       STX    $D0                     ;3
       TAX                            ;2
       LDA    LFE57,X                 ;4
       AND    LFE57,Y                 ;4
       BEQ    LFFA0                   ;2
       LDA    LFE6F,X                 ;4
LFF83:
       DEY                            ;2
       BEQ    LFF8A                   ;2
       LSR                            ;2
       LSR                            ;2
       BPL    LFF83                   ;2
LFF8A:
       ORA    #$FC                    ;2
       STX    $CF                     ;3
       CLC                            ;2
       ADC    $CF                     ;3
       ASL                            ;2
       ASL                            ;2
       ASL                            ;2
       LDX    $D0                     ;3
LFF96:
       STA    $D0                     ;3
LFF98:
       LDA    $85,X                   ;4
       AND    #$C7                    ;2
       ORA    $D0                     ;3
       STA    $85,X                   ;4
LFFA0:
       RTS                            ;6




    IF PLUSROM = 1
PlusROM_API
       .byte "a", 0, "h.firmaplus.de", 0
    ELSE

;       ORG $FE49
;       .byte 0

       ORG $FE49
    ENDIF

LFDF6:
       .byte $40 ; | X      | $FDF6
LFDF7:
       .byte $40 ; | X      | $FDF7
LFDF8:
       .byte $80 ; |X       | $FDF8
       .byte $80 ; |X       | $FDF9
       .byte $C0 ; |XX      | $FDFA
       .byte $40 ; | X      | $FDFB
       .byte $41 ; | X     X| $FDFC
       .byte $49 ; | X  X  X| $FDFD
       .byte $4B ; | X  X XX| $FDFE
       .byte $5B ; | X XX XX| $FDFF




LFEF0:
       .byte $02 ; |      X | $FEF0
       .byte $06 ; |     XX | $FEF1
       .byte $02 ; |      X | $FEF2
       .byte $06 ; |     XX | $FEF3
LFEF4:
       .byte $78 ; | XXXX   | $FEF4
       .byte $38 ; |  XXX   | $FEF5
LFEF6:
       .byte $07 ; |     XXX| $FEF6
       .byte $07 ; |     XXX| $FEF7
       .byte $00 ; |        | $FEF8
       .byte $00 ; |        | $FEF9
       .byte $FF ; |XXXXXXXX| $FEFA
       .byte $FE ; |XXXXXXX | $FEFB
       .byte $00 ; |        | $FEFC
       .byte $F0 ; |XXXX    | $FEFD
       .byte $F0 ; |XXXX    | $FEFE
       .byte $00 ; |        | $FEFF
       .byte $FF ; |XXXXXXXX| $FF00
       .byte $E0 ; |XXX     | $FF01
       .byte $00 ; |        | $FF02
       .byte $01 ; |       X| $FF03
       .byte $00 ; |        | $FF04
       .byte $00 ; |        | $FF05
       .byte $40 ; | X      | $FF06
       .byte $01 ; |       X| $FF07
       .byte $00 ; |        | $FF08
       .byte $50 ; | X X    | $FF09
       .byte $50 ; | X X    | $FF0A
       .byte $50 ; | X X    | $FF0B
       .byte $86 ; |X    XX | $FF0C
       .byte $38 ; |  XXX   | $FF0D
LFF0E:
       .byte $96 ; |X  X XX | $FF0E
       .byte $AD ; |X X XX X| $FF0F
       .byte $AD ; |X X XX X| $FF10
       .byte $AD ; |X X XX X| $FF11
       .byte $75 ; | XXX X X| $FF12
LFF13:
       .byte $65 ; | XX  X X| $FF13
       .byte $A5 ; |X X  X X| $FF14
       .byte $A5 ; |X X  X X| $FF15
LFF16:
       .byte $C0 ; |XX      | $FF16
       .byte $B6 ; |X XX XX | $FF17
       .byte $C0 ; |XX      | $FF18
       .byte $B6 ; |X XX XX | $FF19
LFF1A:
       .byte $19 ; |   XX  X| $FF1A
       .byte $36 ; |  XX XX | $FF1B
       .byte $56 ; | X X XX | $FF1C
LFF17:
    IF PAL = 1
       .byte $2E, $2C, $2A, $28, $48, $68
LFF1D:
LFF23:
       .byte $88, $88
LFF25:
       .byte $A8, $A8
LFF27:
       .byte $C8, $C8
    ELSE
       .byte $1E ; |   XXXX | $FF1D
       .byte $1C ; |   XXX  | $FF1E
       .byte $1A ; |   XX X | $FF1F
       .byte $18 ; |   XX   | $FF20
       .byte $28 ; |  X X   | $FF21
       .byte $38 ; |  XXX   | $FF22
LFF1D:
LFF23:
       .byte $48 ; | X  X   | $FF23
       .byte $58 ; | X XX   | $FF24
LFF25:
       .byte $68 ; | XX X   | $FF25
       .byte $68 ; | XX X   | $FF26
LFF27:
       .byte $78 ; | XXXX   | $FF27
       .byte $78 ; | XXXX   | $FF28
    ENDIF

LFF29:
       .byte $FE ; |XXXXXXX | $FF29
       .byte $02 ; |      X | $FF2A
       .byte $FF ; |XXXXXXXX| $FF2B
       .byte $01 ; |       X| $FF2C
       .byte $03 ; |      XX| $FF2D
       .byte $07 ; |     XXX| $FF2E
       .byte $0F ; |    XXXX| $FF2F
       .byte $1E ; |   XXXX | $FF30
       .byte $3C ; |  XXXX  | $FF31
       .byte $78 ; | XXXX   | $FF32
       .byte $F0 ; |XXXX    | $FF33
       .byte $E0 ; |XXX     | $FF34
       .byte $C0 ; |XX      | $FF35
       .byte $80 ; |X       | $FF36
       .byte $C0 ; |XX      | $FF37
       .byte $E0 ; |XXX     | $FF38
LFF39:
       .byte $F0 ; |XXXX    | $FF39
       .byte $78 ; | XXXX   | $FF3A
       .byte $3C ; |  XXXX  | $FF3B
       .byte $1E ; |   XXXX | $FF3C
       .byte $0F ; |    XXXX| $FF3D
       .byte $07 ; |     XXX| $FF3E
       .byte $03 ; |      XX| $FF3F
       .byte $01 ; |       X| $FF40



;91
Copyright2gfx:
       .byte $AD ; |X X XX X| $FFA1
       .byte $A9 ; |X X X  X| $FFA2
       .byte $E9 ; |XXX X  X| $FFA3
       .byte $A9 ; |X X X  X| $FFA4
       .byte $ED ; |XXX XX X| $FFA5
       .byte $41 ; | X     X| $FFA6
       .byte $0F ; |    XXXX| $FFA7
       .byte $00 ; |        | $FFA8
   IF PLUSROM = 0
       .byte $47 ; | X   XXX| $FFA9
       .byte $41 ; | X     X| $FFAA
       .byte $77 ; | XXX XXX| $FFAB
       .byte $55 ; | X X X X| $FFAC
       .byte $75 ; | XXX X X| $FFAD
Copyright1gfx:
       .byte $00 ; |        | $FFAE shared
       .byte $00 ; |        | $FFAF shared
       .byte $00 ; |        | $FFB0 shared
       .byte $00 ; |        | $FFB1
       .byte $00 ; |        | $FFB2
       .byte $00 ; |        | $FFB3
       .byte $00 ; |        | $FFB4
       .byte $00 ; |        | $FFB5
       .byte $00 ; |        | $FFB6
       .byte $00 ; |        | $FFB7
       .byte $F7 ; |XXXX XXX| $FFB8
       .byte $95 ; |X  X X X| $FFB9
       .byte $87 ; |X    XXX| $FFBA
       .byte $80 ; |X       | $FFBB
       .byte $90 ; |X  X    | $FFBC
       .byte $F0 ; |XXXX    | $FFBD
   ENDIF

Copyright3gfx:
       .byte $50 ; | X X    | $FFBE
       .byte $58 ; | X XX   | $FFBF
       .byte $5C ; | X XXX  | $FFC0
       .byte $56 ; | X X XX | $FFC1
       .byte $53 ; | X X  XX| $FFC2
       .byte $11 ; |   X   X| $FFC3
       .byte $F0 ; |XXXX    | $FFC4
       .byte $00 ; |        | $FFC5
   IF PLUSROM = 0
       .byte $03 ; |      XX| $FFC6
       .byte $00 ; |        | $FFC7
       .byte $4B ; | X  X XX| $FFC8
       .byte $4A ; | X  X X | $FFC9
       .byte $6B ; | XX X XX| $FFCA
       .byte $00 ; |        | $FFCB
       .byte $08 ; |    X   | $FFCC
   ENDIF

LFFCD:
       .byte $00 ; |        | $FFCD shared
       .byte $F7 ; |XXXX XXX| $FFCE
       .byte $E3 ; |XXX   XX| $FFCF
       .byte $41 ; | X     X| $FFD0
       .byte $00 ; |        | $FFD1
       .byte $FB ; |XXXXX XX| $FFD2
       .byte $F1 ; |XXXX   X| $FFD3
       .byte $A0 ; |X X     | $FFD4
       .byte $00 ; |        | $FFD5
       .byte $FD ; |XXXXXX X| $FFD6
       .byte $F8 ; |XXXXX   | $FFD7
       .byte $50 ; | X X    | $FFD8
       .byte $00 ; |        | $FFD9
       .byte $FE ; |XXXXXXX | $FFDA
       .byte $7C ; | XXXXX  | $FFDB
       .byte $28 ; |  X X   | $FFDC
       .byte $00 ; |        | $FFDD
       .byte $7F ; | XXXXXXX| $FFDE
       .byte $3E ; |  XXXXX | $FFDF
       .byte $14 ; |   X X  | $FFE0
       .byte $00 ; |        | $FFE1
       .byte $BF ; |X XXXXXX| $FFE2
       .byte $1F ; |   XXXXX| $FFE3
       .byte $0A ; |    X X | $FFE4
       .byte $00 ; |        | $FFE5
       .byte $DF ; |XX XXXXX| $FFE6
       .byte $8F ; |X   XXXX| $FFE7
       .byte $05 ; |     X X| $FFE8
       .byte $00 ; |        | $FFE9
       .byte $EF ; |XXX XXXX| $FFEA
       .byte $C7 ; |XX   XXX| $FFEB
       .byte $82 ; |X     X | $FFEC
Copyright5gfx:
       .byte $E9 ; |XXX X  X| $FFED
       .byte $AB ; |X X X XX| $FFEE
       .byte $AF ; |X X XXXX| $FFEF
       .byte $AD ; |X X XX X| $FFF0
       .byte $E9 ; |XXX X  X| $FFF1
       .byte $00 ; |        | $FFF2
       .byte $00 ; |        | $FFF3
       .byte $00 ; |        | $FFF4
   IF PLUSROM = 0
       .byte $00 ; |        | $FFF5
       .byte $00 ; |        | $FFF6
       .byte $11 ; |   X   X| $FFF7
       .byte $11 ; |   X   X| $FFF8
       .byte $17 ; |   X XXX| $FFF9
       .byte $15 ; |   X X X| $FFFA
       .byte $17 ; |   X XXX| $FFFB
       .byte $00 ; |        |
   ELSE

SendPlusROMScore
       beq SendPlusROMScoreEnd       ; Not player one
       lda $E0                       ; Game Select (0 - 3)
       lsr                           ; only the second bit (cadet or commander)
       sta WriteToBuffer             ; 
       lda SWCHB                     ; Difficulty switches
       sta WriteToBuffer             ; 
       lda $EC,x                     ; Score Hi BCD (we might send score of second player too, but..)
       sta WriteToBuffer             ; 
       lda $EE,x                     ; Score Mid BCD
       sta WriteToBuffer             ; 
       lda $F0,x                     ; Score Lo BCD
       sta WriteToBuffer             ; 
       lda #HIGHSCORE_ID             ; game id in Highscore DB
       sta WriteSendBuffer
SendPlusROMScoreEnd
       rts
    ENDIF


       ORG $FF00

GFX: ;every 8th byte shared for blip patterns
DigitGFX:
       .byte $1E ; |   XXXX | $FE00
       .byte $33 ; |  XX  XX| $FE01
       .byte $33 ; |  XX  XX| $FE02
       .byte $33 ; |  XX  XX| $FE03
       .byte $33 ; |  XX  XX| $FE04
       .byte $33 ; |  XX  XX| $FE05
       .byte $1E ; |   XXXX | $FE06
LFE07:
       .byte $00 ; |        | $FE07 0
       .byte $3F ; |  XXXXXX| $FE08
       .byte $0C ; |    XX  | $FE09
       .byte $0C ; |    XX  | $FE0A
       .byte $0C ; |    XX  | $FE0B
       .byte $0C ; |    XX  | $FE0C
       .byte $3C ; |  XXXX  | $FE0D
       .byte $1C ; |   XXX  | $FE0E
LFE0F:
       .byte $C0 ; |XX      | $FE0F 1
       .byte $3F ; |  XXXXXX| $FE10
       .byte $30 ; |  XX    | $FE11
       .byte $30 ; |  XX    | $FE12
       .byte $1E ; |   XXXX | $FE13
       .byte $03 ; |      XX| $FE14
       .byte $23 ; |  X   XX| $FE15
       .byte $3E ; |  XXXXX | $FE16
       .byte $60 ; | XX     | $FE17 2
       .byte $1E ; |   XXXX | $FE18
       .byte $23 ; |  X   XX| $FE19
       .byte $03 ; |      XX| $FE1A
       .byte $06 ; |     XX | $FE1B
       .byte $03 ; |      XX| $FE1C
       .byte $23 ; |  X   XX| $FE1D
       .byte $1E ; |   XXXX | $FE1E
       .byte $30 ; |  XX    | $FE1F 3
       .byte $06 ; |     XX | $FE20
       .byte $06 ; |     XX | $FE21
       .byte $3F ; |  XXXXXX| $FE22
       .byte $26 ; |  X  XX | $FE23
       .byte $16 ; |   X XX | $FE24
       .byte $0E ; |    XXX | $FE25
       .byte $06 ; |     XX | $FE26
       .byte $18 ; |   XX   | $FE27 4
       .byte $3E ; |  XXXXX | $FE28
       .byte $23 ; |  X   XX| $FE29
       .byte $03 ; |      XX| $FE2A
       .byte $3E ; |  XXXXX | $FE2B
       .byte $30 ; |  XX    | $FE2C
       .byte $30 ; |  XX    | $FE2D
       .byte $3F ; |  XXXXXX| $FE2E
       .byte $0C ; |    XX  | $FE2F 5
       .byte $1E ; |   XXXX | $FE30
       .byte $33 ; |  XX  XX| $FE31
       .byte $33 ; |  XX  XX| $FE32
       .byte $3E ; |  XXXXX | $FE33
       .byte $30 ; |  XX    | $FE34
       .byte $31 ; |  XX   X| $FE35
       .byte $1E ; |   XXXX | $FE36
       .byte $06 ; |     XX | $FE37 6
       .byte $0C ; |    XX  | $FE38
       .byte $0C ; |    XX  | $FE39
       .byte $0C ; |    XX  | $FE3A
       .byte $06 ; |     XX | $FE3B
       .byte $03 ; |      XX| $FE3C
       .byte $21 ; |  X    X| $FE3D
       .byte $3F ; |  XXXXXX| $FE3E
       .byte $03 ; |      XX| $FE3F 7
       .byte $1E ; |   XXXX | $FE40
       .byte $33 ; |  XX  XX| $FE41
       .byte $33 ; |  XX  XX| $FE42
       .byte $1E ; |   XXXX | $FE43
       .byte $33 ; |  XX  XX| $FE44
       .byte $33 ; |  XX  XX| $FE45
       .byte $1E ; |   XXXX | $FE46
       .byte $00 ; |        | $FE47 8
       .byte $1E ; |   XXXX | $FE48
       .byte $23 ; |  X   XX| $FE49
       .byte $03 ; |      XX| $FE4A
       .byte $1F ; |   XXXXX| $FE4B
       .byte $33 ; |  XX  XX| $FE4C
       .byte $33 ; |  XX  XX| $FE4D
LFE4E:
       .byte $1E ; |   XXXX | $FE4E
LFE4F:
       .byte $01 ; |       X| $FE4F 9
       .byte $14 ; |   X X  | $FE50
       .byte $28 ; |  X X   | $FE51
       .byte $3C ; |  XXXX  | $FE52
       .byte $50 ; | X X    | $FE53
       .byte $78 ; | XXXX   | $FE54
       .byte $8C ; |X   XX  | $FE55
       .byte $9C ; |X  XXX  | $FE56
LFE57:
       .byte $04 ; |     X  | $FE57 A
       .byte $08 ; |    X   | $FE58
       .byte $10 ; |   X    | $FE59
       .byte $20 ; |  X     | $FE5A
       .byte $18 ; |   XX   | $FE5B
       .byte $28 ; |  X X   | $FE5C
       .byte $30 ; |  XX    | $FE5D
       .byte $38 ; |  XXX   | $FE5E
       .byte $10 ; |   X    | $FE5F B
LFE60:
       .byte $00 ; |        | $FE60
       .byte $28 ; |  X X   | $FE61
       .byte $42 ; | X    X | $FE62
LFE63:
       .byte $5B ; | X XX XX| $FE63
       .byte $19 ; |   XX  X| $FE64
       .byte $32 ; |  XX  X | $FE65
       .byte $4B ; | X  X XX| $FE66
       .byte $05 ; |     X X| $FE67 C
       .byte $A5 ; |X X  X X| $FE68
       .byte $5A ; | X XX X | $FE69
       .byte $BD ; |X XXXX X| $FE6A
       .byte $FF ; |XXXXXXXX| $FE6B
       .byte $DF ; |XX XXXXX| $FE6C
       .byte $3F ; |  XXXXXX| $FE6D
       .byte $1F ; |   XXXXX| $FE6E
LFE6F:
       .byte $11 ; |   X   X| $FE6F D
       .byte $FF ; |XXXXXXXX| $FE70
       .byte $FB ; |XXXXX XX| $FE71
       .byte $DF ; |XX XXXXX| $FE72
       .byte $F6 ; |XXXX XX | $FE73
       .byte $CE ; |XX  XXX | $FE74
       .byte $C7 ; |XX   XXX| $FE75
       .byte $DB ; |XX XX XX| $FE76
       .byte $14 ; |   X X  | $FE77 E
       .byte $42 ; | X    X | $FE78
       .byte $BD ; |X XXXX X| $FE79
       .byte $5A ; | X XX X | $FE7A
       .byte $FF ; |XXXXXXXX| $FE7B
       .byte $DF ; |XX XXXXX| $FE7C
       .byte $3F ; |  XXXXXX| $FE7D
       .byte $1F ; |   XXXXX| $FE7E
       .byte $15 ; |   X X X| $FE7F F
LFE80:
       .byte $60 ; | XX     | $FE80
       .byte $80 ; |X       | $FE81
       .byte $60 ; | XX     | $FE82
       .byte $40 ; | X      | $FE83
       .byte $62 ; | XX   X | $FE84
       .byte $44 ; | X   X  | $FE85
       .byte $42 ; | X    X | $FE86
       .byte $4E ; | X  XXX | $FE87
LFE88:
       .byte $08 ; |    X   | $FE88
       .byte $08 ; |    X   | $FE89
       .byte $01 ; |       X| $FE8A
       .byte $01 ; |       X| $FE8B
LFE8C:
       .byte $E4 ; |XXX  X  | $FE8C
       .byte $FC ; |XXXXXX  | $FE8D
       .byte $FE ; |XXXXXXX | $FE8E
       .byte $FB ; |XXXXX XX| $FE8F
       .byte $FC ; |XXXXXX  | $FE90
       .byte $FE ; |XXXXXXX | $FE91
       .byte $FF ; |XXXXXXXX| $FE92
LFE8D:
       .byte $FF ; |XXXXXXXX| $FE93
       .byte $79 ; | XXXX  X| $FE94
       .byte $30 ; |  XX    | $FE95
Copyright6gfx:
   IF PLUSROM = 1
Copyright1gfx:
   ENDIF
       .byte $00 ; |        | $FE96 shared
       .byte $00 ; |        | $FE97 shared
       .byte $00 ; |        | $FE98 shared
       .byte $00 ; |        | $FE99 shared
       .byte $00 ; |        | $FE9A shared
       .byte $00 ; |        | $FE9B shared
       .byte $00 ; |        | $FE9C shared
       .byte $00 ; |        | $FE9D shared
       .byte $00 ; |        | $FE9E shared
       .byte $00 ; |        | $FE9F
;    IF PLUSROM = 0
       .byte $77 ; | XXX XXX| $FEA0
       .byte $54 ; | X X X  | $FEA1
       .byte $77 ; | XXX XXX| $FEA2
       .byte $51 ; | X X   X| $FEA3
       .byte $77 ; | XXX XXX| $FEA4
;    ENDIF
       .byte $00 ; |        | $FEA5
       .byte $7C ; | XXXXX  | $FEA6
       .byte $C3 ; |XX    XX| $FEA7
       .byte $FE ; |XXXXXXX | $FEA8
       .byte $66 ; | XX  XX | $FEA9
       .byte $03 ; |      XX| $FEAA
       .byte $01 ; |       X| $FEAB
       .byte $00 ; |        | $FEAC
       .byte $00 ; |        | $FEAD
       .byte $00 ; |        | $FEAE
       .byte $08 ; |    X   | $FEAF
       .byte $24 ; |  X  X  | $FEB0
       .byte $80 ; |X       | $FEB1
       .byte $10 ; |   X    | $FEB2
       .byte $24 ; |  X  X  | $FEB3
       .byte $08 ; |    X   | $FEB4
LFEB5:
       .byte $10 ; |   X    | $FEB5
       .byte $00 ; |        | $FEB6
       .byte $0E ; |    XXX | $FEB7
       .byte $04 ; |     X  | $FEB8
       .byte $0E ; |    XXX | $FEB9
       .byte $1F ; |   XXXXX| $FEBA
       .byte $7F ; | XXXXXXX| $FEBB
       .byte $CE ; |XX  XXX | $FEBC
       .byte $04 ; |     X  | $FEBD
       .byte $07 ; |     XXX| $FEBE
       .byte $1C ; |   XXX  | $FEBF
       .byte $00 ; |        | $FEC0
       .byte $0E ; |    XXX | $FEC1
       .byte $04 ; |     X  | $FEC2
       .byte $0E ; |    XXX | $FEC3
       .byte $1F ; |   XXXXX| $FEC4
       .byte $7F ; | XXXXXXX| $FEC5
       .byte $CE ; |XX  XXX | $FEC6
       .byte $04 ; |     X  | $FEC7
       .byte $1C ; |   XXX  | $FEC8
LFEC9:
       .byte $07 ; |     XXX| $FEC9
LFECA:
       .byte $21 ; |  X    X| $FECA
       .byte $59 ; | X XX  X| $FECB
       .byte $0C ; |    XX  | $FECC
       .byte $0C ; |    XX  | $FECD
       .byte $03 ; |      XX| $FECE
LFECF:
       .byte $04 ; |     X  | $FECF
       .byte $FC ; |XXXXXX  | $FED0
Copyright4gfx:
       .byte $BA ; |X XXX X | $FED1
       .byte $8A ; |X   X X | $FED2
       .byte $BA ; |X XXX X | $FED3
       .byte $A2 ; |X X   X | $FED4
       .byte $3A ; |  XXX X | $FED5
       .byte $80 ; |X       | $FED6
       .byte $FE ; |XXXXXXX | $FED7
       .byte $00 ; |        | $FED8
    IF PLUSROM = 0
       .byte $80 ; |X       | $FED9
       .byte $80 ; |X       | $FEDA
       .byte $AA ; |X X X X | $FEDB
       .byte $AA ; |X X X X | $FEDC
       .byte $BA ; |X XXX X | $FEDD
       .byte $27 ; |  X  XXX| $FEDE
       .byte $22 ; |  X   X | $FEDF
    ENDIF

LFEE0:
       .byte $00 ; |        | $FEE0 shared
       .byte $2C ; |  X XX  | $FEE1
    IF PAL = 1
        .byte $52, $20
LFEE4:
        .byte $D8
    ELSE
       .byte $D2 ; |XX X  X | $FEE2
       .byte $10 ; |   X    | $FEE3
LFEE4:
       .byte $88 ; |X   X   | $FEE4 shared
    ENDIF
       .byte $2A ; |  X X X | $FEE5
       .byte $2C ; |  X XX  | $FEE6
       .byte $2E ; |  X XXX | $FEE7
       .byte $00 ; |        | $FEE8
       .byte $00 ; |        | $FEE9
    IF PAL = 1
       .byte $B2, $B2
    ELSE
       .byte $80 ; |X       | $FEEA
       .byte $80 ; |X       | $FEEB
    ENDIF
       .byte $0E ; |    XXX | $FEEC
       .byte $0E ; |    XXX | $FEED
       .byte $0E ; |    XXX | $FEEE
       .byte $0E ; |    XXX | $FEEF


LFDF0:
       .byte $FF ; |XXXXXXXX| $FDF0
       .byte $FE ; |XXXXXXX | $FDF1
       .byte $7C ; | XXXXX  | $FDF2
       .byte $38 ; |  XXX   | $FDF3
       .byte $10 ; |   X    | $FDF4
       .byte $00 ; |        | $FDF5

    IF PLUSROM = 1
       ORG $FFF0
       .byte $ff,$ff,$ff,$ff             ; Protect PlusROM hotspots
SendPlusROMScoreCheck
       CPX #$01                          ; Check for first player
       JMP SendPlusROMScore

       ORG $FFFA
        .word (PlusROM_API - $E000)      ; PlusRom API pointer

    ELSE
       .byte $40 ; | X      | $FDF6 unused
       .byte $40 ; | X      | $FDF7 unused

       ORG $FFF8
       .byte "2007"
    ENDIF

       .word START,0
