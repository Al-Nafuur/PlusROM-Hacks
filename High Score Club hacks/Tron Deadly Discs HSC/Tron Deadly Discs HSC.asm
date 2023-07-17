;TRON - DEADLY DISCS: Supercharger hack by Kurt (Nukey Shay) Howe, 5/18/2008

PAL = 0

; Disassembly of trondead.bin
; Disassembled Sun Apr 24 22:38:11 2005
; Using DiStella v2.0
; Command Line: C:\BIN\DISTELLA.EXE -pafsctrondead.cfg trondead.bin 
; trondead.cfg contents:
;      ORG F000
;      CODE F000 F094
;      GFX F095 F0FF
;      CODE F100 F1FC
;      GFX F1FD F1FF
;      CODE F200 F321
;      GFX F322 F333
;      CODE F334 F52F
;      GFX F530 F54E
;      CODE F54F F68E
;      GFX F68F F693
;      CODE F694 F735
;      GFX F736 F749
;      CODE F74A F89C
;      GFX F89D F8A4
;      CODE F8A5 F9DA
;      GFX F9DB F9EC
;      CODE F9ED FBC5 
;      GFX FBC6 FD86
;      CODE FD87 FDE6
;      GFX FDE7 FDFF
;      CODE FE00 FEE7
;      GFX FEE8 FEE9
;      CODE FEEA FFF7
;      GFX FFF8 FFFF

      processor 6502

;hardware register equates
VSYNC   =  $00 ;Vertical Sync Set-Clear
VBLANK  =  $01 ;Vertical Blank Set-Clear
WSYNC   =  $02 ;Wait for Horizontal Blank
RSYNC   =  $03 ;Reset Horizontal Sync Counter
NUSIZ0  =  $04 ;Number-Size player/missle 0
NUSIZ1  =  $05 ;Number-Size player/missle 1
COLUP0  =  $06 ;Color-Luminance Player 0
COLUP1  =  $07 ;Color-Luminance Player 1
COLUPF  =  $08 ;Color-Luminance Playfield
COLUBK  =  $09 ;Color-Luminance Background
CTRLPF  =  $0A ;Control Playfield, Ball, Collisions
REFP0   =  $0B ;Reflection Player 0
REFP1   =  $0C ;Reflection Player 1
PF0     =  $0D ;Playfield Register Byte 0 (upper nybble used only)
PF1     =  $0E ;Playfield Register Byte 1
PF2     =  $0F ;Playfield Register Byte 2
RESP0   =  $10 ;Reset Player 0
RESP1   =  $11 ;Reset Player 1
RESM0   =  $12 ;Reset Missle 0
RESM1   =  $13 ;Reset Missle 1
RESBL   =  $14 ;Reset Ball
;Audio registers
AUDC0   =  $15 ;Audio Control - Voice 0 (distortion)
AUDC1   =  $16 ;Audio Control - Voice 1 (distortion)
AUDF0   =  $17 ;Audio Frequency - Voice 0
AUDF1   =  $18 ;Audio Frequency - Voice 1
AUDV0   =  $19 ;Audio Volume - Voice 0
AUDV1   =  $1A ;Audio Volume - Voice 1
;Sprite registers
GRP0    =  $1B ;Graphics Register Player 0
GRP1    =  $1C ;Graphics Register Player 1
ENAM0   =  $1D ;Graphics Enable Missle 0
ENAM1   =  $1E ;Graphics Enable Missle 1
ENABL   =  $1F ;Graphics Enable Ball
HMP0    =  $20 ;Horizontal Motion Player 0
HMP1    =  $21 ;Horizontal Motion Player 1
HMM0    =  $22 ;Horizontal Motion Missle 0
HMM1    =  $23 ;Horizontal Motion Missle 1
HMBL    =  $24 ;Horizontal Motion Ball
VDELP0  =  $25 ;Vertical Delay Player 0
VDELP1  =  $26 ;Vertical Delay Player 1
VDEL01  =  $26 ;Vertical Delay Player 1
VDELBL  =  $27 ;Vertical Delay Ball
RESMP0  =  $28 ;Reset Missle 0 to Player 0
RESMP1  =  $29 ;Reset Missle 1 to Player 1
HMOVE   =  $2A ;Apply Horizontal Motion
HMCLR   =  $2B ;Clear Horizontal Move Registers
CXCLR   =  $2C ;Clear Collision Latches
Waste1  =  $2D ;Unused
Waste2  =  $2E ;Unused
Waste3  =  $2F ;Unused
;collisions                     (bit 7) (bit 6)
CXM0P   =  $30 ;Read Collision - M0-P1   M0-P0
CXM1P   =  $31 ;Read Collision - M1-P0   M1-P1
CXP0FB  =  $32 ;Read Collision - P0-PF   P0-BL
CXP1FB  =  $33 ;Read Collision - P1-PF   P1-BL
CXM0FB  =  $34 ;Read Collision - M0-PF   M0-BL
CXM1FB  =  $35 ;Read Collision - M1-PF   M1-BL
CXBLPF  =  $36 ;Read Collision - BL-PF   -----
CXPPMM  =  $37 ;Read Collision - P0-P1   M0-M1
INPT0   =  $38 ;Read Pot Port 0
INPT1   =  $39 ;Read Pot Port 1
INPT2   =  $3A ;Read Pot Port 2
INPT3   =  $3B ;Read Pot Port 3
INPT4   =  $3C ;Read Input - Trigger 0 (bit 7)
INPT5   =  $3D ;Read Input - Trigger 1 (bit 7)
;RIOT registers
SWCHA  = $0280 ;Port A data register for joysticks (High nybble-player0,low nybble-player1)
SWACNT = $0281 ;Port A data direction register (DDR)
SWCHB  = $0282 ;Port B data (console switches) bit pattern LR--B-SR
SWBCNT = $0283 ;Port B data direction register (DDR)
INTIM  = $0284 ;Timer output
TIMINT = $0285 ;
TIM1T  = $0294 ;set 1 clock interval
TIM8T  = $0295 ;set 8 clock interval
TIM64T = $0296 ;set 64 clock interval
T1024T = $0297 ;set 1024 clock interval

  IF PAL
SCORECOLOR       = $2A
SCOREBACKGROUND  = $34
WALLCOLOR        = $C6
SCREENBACKGROUND = $06
NEWDOORCOLOR     = $68 ;color of doors that haven't been shot yet
HITDOORCOLOR     = $2A ;color of doors OK to move through

HIT0COLOR        = $D2 ;player...no hits taken yet
HIT1COLOR        = $A4
HIT2COLOR        = $84
HIT3COLOR        = $68
HIT4COLOR        = $28
HIT5COLOR        = $2C

ENEMY1COLOR      = $2A
ENEMY2COLOR      = $54
ENEMY3COLOR      = $D8
ENEMY4COLOR      = $0E
ENEMY5COLOR      = $58
ENEMY6COLOR      = $02
ENEMY7COLOR      = $22
ENEMY8COLOR      = $B8
  ELSE
SCORECOLOR       = $E8
SCOREBACKGROUND  = $D4
WALLCOLOR        = $76
SCREENBACKGROUND = $06
NEWDOORCOLOR     = $48 ;color of doors that haven't been shot yet
HITDOORCOLOR     = $E8 ;color of doors OK to move through

HIT0COLOR        = $82 ;player...no hits taken yet
HIT1COLOR        = $64
HIT2COLOR        = $54
HIT3COLOR        = $48
HIT4COLOR        = $28
HIT5COLOR        = $2C

ENEMY1COLOR      = $EA
ENEMY2COLOR      = $C4
ENEMY3COLOR      = $88
ENEMY4COLOR      = $0E
ENEMY5COLOR      = $C8
ENEMY6COLOR      = $02
ENEMY7COLOR      = $22
ENEMY8COLOR      = $F8
  ENDIF



       ORG $F000

LF000:
       LDX    #$03                    ;2
       LDY    #$01                    ;2
       BNE    LF00A                   ;2
LF006:
       LDX    #$07                    ;2
       LDY    #$06                    ;2
LF00A:
       STY    $DF                     ;3
LF00C:
       TXA                            ;2
       TAY                            ;2
       DEY                            ;2
LF00F:
       LDA    $88,X                   ;4
       CMP.wy $88,Y                   ;4
       BCC    LF043                   ;2
       BEQ    LF043                   ;2
       STX    $E1                     ;3
       STY    $E2                     ;3
       LDA    #$08                    ;2
       CPX    #$04                    ;2
       BMI    LF024                   ;2
       SBC    #$02                    ;2
LF024:
       STA    $E0                     ;3
LF026:
       LDA    $80,X                   ;4
       PHA                            ;3
       LDA.wy $80,Y                   ;4
       STA    $80,X                   ;4
       PLA                            ;4
       STA.wy $80,Y                   ;5
       TXA                            ;2
       CLC                            ;2
       ADC    #$08                    ;2
       TAX                            ;2
       TYA                            ;2
       ADC    #$08                    ;2
       TAY                            ;2
       DEC    $E0                     ;5
       BPL    LF026                   ;2
       LDX    $E1                     ;3
       LDY    $E2                     ;3
LF043:
       DEY                            ;2
       CPY    $DF                     ;3
       BPL    LF00F                   ;2
       DEX                            ;2
       CPX    $DF                     ;3
       BNE    LF00C                   ;2
       RTS                            ;6

LF04E:
       LSR                            ;2
       BCC    LF077                   ;2
       LSR                            ;2
       BCS    LF078                   ;2
       AND    #$03                    ;2
       STA    $DF                     ;3
       LDX    #$03                    ;2
LF05A:
       LDY    $DF                     ;3
       LDA    $98,X                   ;4
       BNE    LF065                   ;2
       LDA    $A0,X                   ;4
       BNE    LF065                   ;2
       TAY                            ;2
LF065:
       LDA    LF095,Y                 ;4
       CPX    #$00                    ;2
       BEQ    LF06F                   ;2
       LDA    LF099,Y                 ;4
LF06F:
       SEC                            ;2
       SBC    $88,X                   ;4
       STA    $A8,X                   ;4
       DEX                            ;2
       BPL    LF05A                   ;2
LF077:
       RTS                            ;6

LF078:
       LSR                            ;2
       BCC    LF077                   ;2
       LDA    $80                     ;3
       BEQ    LF077                   ;2
       LDA    $B0                     ;3
       LSR                            ;2
       BCS    LF077                   ;2
       DEC    $B8                     ;5
       LDA    $B8                     ;3
       AND    #$0B                    ;2
       BNE    LF077                   ;2
       TAX                            ;2
       JSR    LF8A5                   ;6
       LDX    #$04                    ;2
       JMP    LF8A5                   ;3

LF095:
       .byte $A0 ; |X X     | $F095
       .byte $B0 ; |X XX    | $F096
       .byte $C0 ; |XX      | $F097
       .byte $D0 ; |XX X    | $F098
LF099:
       .byte $D7 ; |XX X XXX| $F099
       .byte $D8 ; |XX XX   | $F09A
       .byte $E7 ; |XXX  XXX| $F09B
LF09C:
       .byte $E8 ; |XXX X   | $F09C
LF09D:
       .byte $03 ; |      XX| $F09D
       .byte $02 ; |      X | $F09E
       .byte $01 ; |       X| $F09F
       .byte $0C ; |    XX  | $F0A0
       .byte $0E ; |    XXX | $F0A1
       .byte $00 ; |        | $F0A2
       .byte $78 ; | XXXX   | $F0A3
       .byte $58 ; | X XX   | $F0A4
       .byte $5F ; | X XXXXX| $F0A5
       .byte $18 ; |   XX   | $F0A6
       .byte $00 ; |        | $F0A7
       .byte $18 ; |   XX   | $F0A8
       .byte $3C ; |  XXXX  | $F0A9
       .byte $24 ; |  X  X  | $F0AA
       .byte $66 ; | XX  XX | $F0AB
       .byte $C2 ; |XX    X | $F0AC
       .byte $82 ; |X     X | $F0AD
       .byte $03 ; |      XX| $F0AE
       .byte $00 ; |        | $F0AF
       .byte $06 ; |     XX | $F0B0
       .byte $07 ; |     XXX| $F0B1
       .byte $00 ; |        | $F0B2
       .byte $0C ; |    XX  | $F0B3
       .byte $1C ; |   XXX  | $F0B4
       .byte $1C ; |   XXX  | $F0B5
       .byte $1E ; |   XXXX | $F0B6
       .byte $18 ; |   XX   | $F0B7
       .byte $18 ; |   XX   | $F0B8
       .byte $18 ; |   XX   | $F0B9
       .byte $78 ; | XXXX   | $F0BA
       .byte $48 ; | X  X   | $F0BB
       .byte $48 ; | X  X   | $F0BC
       .byte $08 ; |    X   | $F0BD
       .byte $0C ; |    XX  | $F0BE
       .byte $00 ; |        | $F0BF
       .byte $06 ; |     XX | $F0C0
       .byte $07 ; |     XXX| $F0C1
       .byte $00 ; |        | $F0C2
       .byte $0C ; |    XX  | $F0C3
       .byte $1C ; |   XXX  | $F0C4
       .byte $1C ; |   XXX  | $F0C5
       .byte $1E ; |   XXXX | $F0C6
       .byte $18 ; |   XX   | $F0C7
       .byte $1C ; |   XXX  | $F0C8
       .byte $3E ; |  XXXXX | $F0C9
       .byte $22 ; |  X   X | $F0CA
       .byte $2E ; |  X XXX | $F0CB
       .byte $28 ; |  X X   | $F0CC
       .byte $20 ; |  X     | $F0CD
       .byte $10 ; |   X    | $F0CE
       .byte $00 ; |        | $F0CF
       .byte $0C ; |    XX  | $F0D0
       .byte $0E ; |    XXX | $F0D1
       .byte $00 ; |        | $F0D2
       .byte $78 ; | XXXX   | $F0D3
       .byte $58 ; | X XX   | $F0D4
       .byte $5F ; | X XXXXX| $F0D5
       .byte $18 ; |   XX   | $F0D6
       .byte $00 ; |        | $F0D7
       .byte $18 ; |   XX   | $F0D8
       .byte $3F ; |  XXXXXX| $F0D9
       .byte $21 ; |  X    X| $F0DA
       .byte $62 ; | XX   X | $F0DB
       .byte $42 ; | X    X | $F0DC
       .byte $C0 ; |XX      | $F0DD
       .byte $80 ; |X       | $F0DE
       .byte $00 ; |        | $F0DF
       .byte $00 ; |        | $F0E0
       .byte $00 ; |        | $F0E1
       .byte $00 ; |        | $F0E2
       .byte $00 ; |        | $F0E3
       .byte $02 ; |      X | $F0E4
       .byte $02 ; |      X | $F0E5
       .byte $02 ; |      X | $F0E6
       .byte $02 ; |      X | $F0E7
       .byte $00 ; |        | $F0E8
       .byte $00 ; |        | $F0E9
       .byte $00 ; |        | $F0EA
       .byte $00 ; |        | $F0EB
       .byte $00 ; |        | $F0EC
       .byte $00 ; |        | $F0ED
       .byte $00 ; |        | $F0EE
       .byte $00 ; |        | $F0EF
       .byte $00 ; |        | $F0F0
       .byte $00 ; |        | $F0F1
       .byte $00 ; |        | $F0F2
       .byte $02 ; |      X | $F0F3
       .byte $00 ; |        | $F0F4
       .byte $02 ; |      X | $F0F5
       .byte $02 ; |      X | $F0F6
       .byte $00 ; |        | $F0F7
       .byte $00 ; |        | $F0F8
       .byte $00 ; |        | $F0F9
       .byte $00 ; |        | $F0FA
       .byte $00 ; |        | $F0FB
       .byte $00 ; |        | $F0FC
       .byte $00 ; |        | $F0FD
       .byte $00 ; |        | $F0FE
       .byte $00 ; |        | $F0FF

LF100:
       LDX    $E6                     ;3
       LDA    $90,X                   ;4
       STA    RESM1                   ;3
       STA    HMCLR                   ;3
       NOP                            ;2
       STA    HMM1                    ;3
       LDA    #$EB                    ;2
       STA    $EC                     ;3
       JMP    LF211                   ;3

LF112:
       LDX    $E6                     ;3
       LDA    $90,X                   ;4
       STA    HMCLR                   ;3
       NOP                            ;2
       STA    RESM1                   ;3
       STA    HMM1                    ;3
       LDA    #$EB                    ;2
       STA    $EC                     ;3
       JMP    LF211                   ;3

LF124:
       LDX    $E6                     ;3
       LDA    $90,X                   ;4
       STA    HMCLR                   ;3
       STA    HMM1                    ;3
       NOP                            ;2
       LDA    #$EB                    ;2
       STA    RESM1                   ;3
       STA    $EC                     ;3
       JMP    LF211                   ;3

LF136:
       LDX    $E6                     ;3
       LDA    $90,X                   ;4
       STA    HMCLR                   ;3
       STA    HMM1                    ;3
       LDA    #$EB                    ;2
       STA    $EC                     ;3
       LDX    LFD00,Y                 ;4
       STA    RESM1                   ;3
       NOP                            ;2
       JMP    LF214                   ;3

LF14B:
       LDX    $E6                     ;3
       LDA    $90,X                   ;4
       STA    HMCLR                   ;3
       STA    HMM1                    ;3
       LDA    #$EB                    ;2
       STA    $EC                     ;3
       LDX    LFD00,Y                 ;4
       LDA    VBLANK,X                ;4
       STA.w  RESM1                   ;4
       STA.w  COLUPF                  ;4
       JMP    LF218                   ;3

LF165:
       LDX    $E6                     ;3
       LDA    $90,X                   ;4
       STA    HMCLR                   ;3
       STA    HMM1                    ;3
       LDA    #$EB                    ;2
       STA    $EC                     ;3
       LDX    LFD00,Y                 ;4
       LDA    VBLANK,X                ;4
       STA    $E8                     ;3
       STA    COLUPF                  ;3
       STA    RESM1                   ;3
       NOP                            ;2
       JMP    LF21A                   ;3

LF180:
       LDX    $E6                     ;3
       LDA    $90,X                   ;4
       STA    HMCLR                   ;3
       STA    HMM1                    ;3
       LDA    #$EB                    ;2
       STA    $EC                     ;3
       LDX    LFD00,Y                 ;4
       LDA    VBLANK,X                ;4
       STA    $E8                     ;3
       STA    COLUPF                  ;3
       LDA    ($F6),Y                 ;5
       STA    RESM1                   ;3
       NOP                            ;2
       JMP    LF21C                   ;3

LF19D:
       LDX    $E6                     ;3
       LDA    $90,X                   ;4
       STA    HMCLR                   ;3
       STA    HMM1                    ;3
       LDX    LFD00,Y                 ;4
       LDA    VBLANK,X                ;4
       STA    $E8                     ;3
       STA    COLUPF                  ;3
       LDA    ($F6),Y                 ;5
       BPL    LF1C0                   ;2 <-
       LDA    ($EE),Y                 ;5
       STA    GRP0                    ;3
LF1B6:
       STA    RESM1                   ;3
       LDA    #$EB                    ;2
       STA    $EC                     ;3
       NOP                            ;2
       JMP    LF222                   ;3

LF1C0:
       NOP                            ;2
       NOP                            ;2
       BPL    LF1B6                   ;2 always branch

LF1C4:
       LDX    $E6                     ;3
       LDA    $90,X                   ;4
       STA    HMCLR                   ;3
       STA    HMM1                    ;3
       LDA    #$EB                    ;2
       STA    $EC                     ;3
       LDX    LFD00,Y                 ;4
       LDA    VBLANK,X                ;4
       STA    $E8                     ;3
       STA    COLUPF                  ;3
       LDA    ($F6),Y                 ;5
       BPL    LF1E7                   ;2
       LDA    ($EE),Y                 ;5
       STA    GRP0                    ;3
LF1E1:
       STA    RESM1                   ;3
       NOP                            ;2
       JMP    LF222                   ;3

LF1E7:
       NOP                            ;2
       NOP                            ;2
       BPL    LF1E1                   ;2
       LDA    ($F4),Y                 ;5
       STA    ENAM1                   ;3
       STA    HMCLR                   ;3
       LDA    #$6E                    ;2
       STA    $EC                     ;3
       LDA    #$F2                    ;2
       STA    $ED                     ;3
       NOP                            ;2
       JMP    LF211                   ;3

;unused?
       .byte $FF ; |XXXXXXXX| $F1FD
       .byte $FF ; |XXXXXXXX| $F1FE
       .byte $FF ; |XXXXXXXX| $F1FF


LF200:
       LDX    $E6                     ;3
       LDA    #$3C                    ;2
       STA    $EC                     ;3
       STA    HMCLR                   ;3
       LDA    $A8,X                   ;4
       STA    $F4                     ;3
       LDA    $B0,X                   ;4
       STA.w  NUSIZ1                  ;4
LF211:
       LDX    LFD00,Y                 ;4
LF214:
       LDA    VBLANK,X                ;4
       STA    COLUPF                  ;3
LF218:
       STA    $E8                     ;3
LF21A:
       LDA    ($F6),Y                 ;5
LF21C:
       BPL    LF22E                   ;2
       LDA    ($EE),Y                 ;5
       STA    GRP0                    ;3
LF222:
       LDA    VSYNC,X                 ;4
       STA    HMOVE                   ;3
       STA    COLUPF                  ;3
       STA    $E7                     ;3
       INY                            ;2
       JMP.ind ($EA)                  ;5

LF22E:
       NOP                            ;2
       NOP                            ;2
       BPL    LF222                   ;2
LF232:
       NOP                            ;2
       NOP                            ;2
       NOP                            ;2
       CPY    #$77                    ;2
       BCC    LF211                   ;2
       JMP    LFF41                   ;3

LF23C:
       LDX    $E6                     ;3
       TYA                            ;2
       CMP    $88,X                   ;4
       STA    HMCLR                   ;3
       BCC    LF232                   ;2
       LDA    $DE,X                   ;4
       STA    $EC                     ;3
       LDA    #$F1                    ;2
       STA    $ED                     ;3
       LDX    LFD00,Y                 ;4
       LDA    VBLANK,X                ;4
       STA    COLUPF                  ;3
       STA    $E8                     ;3
       LDA    ($F6),Y                 ;5
       BPL    LF26A                   ;2
       LDA    ($EE),Y                 ;5
       STA    GRP0                    ;3
LF25E:
       LDA    VSYNC,X                 ;4
       STA    HMOVE                   ;3
       STA    COLUPF                  ;3
       STA    $E7                     ;3
       INY                            ;2
       JMP.ind ($EA)                  ;5

LF26A:
       NOP                            ;2
       NOP                            ;2
       BPL    LF25E                   ;2
       LDA    ($F4),Y                 ;5
       STA    ENAM1                   ;3
       STA    HMCLR                   ;3
       BNE    LF27F                   ;2
       DEC    $E6                     ;5
       LDA    #$00                    ;2
       STA    $EC                     ;3
       JMP    LF211                   ;3

LF27F:
       PLA                            ;4
       PHA                            ;3
       NOP                            ;2
       JMP    LF211                   ;3

LF285:
       LDA    ($F2),Y                 ;5
       STA    GRP1                    ;3
       STA    HMCLR                   ;3
       LDA    #$95                    ;2
       STA.w  $EA                     ;4
       DEC    $E2                     ;5
       JMP    LF2FA                   ;3

LF295:
       LDA    ($F2),Y                 ;5
       STA    GRP1                    ;3
       STA    HMCLR                   ;3
       BNE    LF2A6                   ;2
       LDA    #$EA                    ;2
       STA    $EA                     ;3
       NOP                            ;2
LF2A2:
       NOP                            ;2
       JMP    LF2FA                   ;3

LF2A6:
       STA    $3F                     ;3
       JMP    LF2A2                   ;3

LF2AB:
       LDA    $DE,X                   ;4
       STA    $E9                     ;3
       JMP    LF2FA                   ;3

LF2B2:
       LDX    $E2                     ;3
       TYA                            ;2
       CMP    $88,X                   ;4
       STA    HMCLR                   ;3
       BCC    LF2AB                   ;2
       LDA    $E9                     ;3
       STA    $EA                     ;3
       LDA    #$FA                    ;2
       STA    $EB                     ;3
       LDA    $E8                     ;3
       STA    COLUPF                  ;3
       LDA    ($F8),Y                 ;5
       BPL    LF2E1                   ;2
       LDA    ($F0),Y                 ;5
       STA    ENAM0                   ;3
LF2CF:
       LDA    ($F6),Y                 ;5
       BPL    LF2E5                   ;2
       LDA    ($EE),Y                 ;5
       STA    GRP0                    ;3
LF2D7:
       STA    HMOVE                   ;3
       LDA    $E7                     ;3
       STA    COLUPF                  ;3
       INY                            ;2
       JMP.ind ($EC)                  ;5

LF2E1:
       NOP                            ;2
       NOP                            ;2
       BPL    LF2CF                   ;2
LF2E5:
       NOP                            ;2
       NOP                            ;2
       JMP    LF2D7                   ;3

LF2EA:
       LDX    $E2                     ;3
       LDA    #$B2                    ;2
       STA    $EA                     ;3
       STA    HMCLR                   ;3
       LDA    $A8,X                   ;4
       STA    $F2                     ;3
       LDA    $B0,X                   ;4
       STA    REFP1                   ;3
LF2FA:
       LDA    $E8                     ;3
LF2FC:
       STA    COLUPF                  ;3
       LDA    ($F8),Y                 ;5
       BPL    LF318                   ;2
LF302:
       LDA    ($F0),Y                 ;5
LF304:
       STA    ENAM0                   ;3
LF306:
       LDA    ($F6),Y                 ;5
       BPL    LF31D                   ;2
LF30A:
       LDA    ($EE),Y                 ;5
LF30C:
       STA    GRP0                    ;3
LF30E:
       STA    HMOVE                   ;3
       LDA    $E7                     ;3
       STA    COLUPF                  ;3
       INY                            ;2
       JMP.ind ($EC)                  ;5

LF318:
       NOP                            ;2
       NOP                            ;2
       JMP    LF306                   ;3

LF31D:
       NOP                            ;2
       NOP                            ;2
       JMP    LF30E                   ;3

LF322:
       .byte $00 ; |        | $F322
       .byte $12 ; |   X  X | $F323
       .byte $24 ; |  X  X  | $F324
       .byte $38 ; |  XXX   | $F325
       .byte $52 ; | X X  X | $F326
       .byte $70 ; | XXX    | $F327
       .byte $A6 ; |X X  XX | $F328
       .byte $D6 ; |XX X XX | $F329
       .byte $FC ; |XXXXXX  | $F32A
LF32B:
       .byte $00 ; |        | $F32B
       .byte $12 ; |   X  X | $F32C
       .byte $24 ; |  X  X  | $F32D
       .byte $36 ; |  XX XX | $F32E
       .byte $4B ; | X  X XX| $F32F
       .byte $65 ; | XX  X X| $F330
       .byte $80 ; |X       | $F331
       .byte $9D ; |X  XXX X| $F332
       .byte $C4 ; |XX   X  | $F333

START: ;@ $F334
       CLD                            ;2
       LDX    #$00                    ;2
       TXA                            ;2
LF338:
       STA    VSYNC,X                 ;4
       TXS                            ;2
       INX                            ;2
       BNE    LF338                   ;2
       LDX    #$07                    ;2
LF340:
       JSR    LF8A5                   ;6
       DEX                            ;2
       BPL    LF340                   ;2
       LDX    #$0D                    ;2
       LDA    #WALLCOLOR              ;2 <-starting side walls/unopened doors color
LF34A:
       STA    $D1,X                   ;4
       DEX                            ;2
       BPL    LF34A                   ;2
       BIT    SWCHB                   ;4
       BVC    LF358                   ;2
       LDX    #$28                    ;2
       STX    $CB                     ;3
LF358:
       LDX    #$00                    ;2
       LDA    #$46                    ;2
       JSR    LFFCE                   ;6
       LDA    #$34                    ;2
       STA    $88                     ;3
       LDA    #$6C                    ;2
       STA    $A8                     ;3
       LDA    #HIT0COLOR              ;2
       STA    $B8                     ;3
       DEC    $CC                     ;5
       LDA    REFP1                   ;3
       ORA    PF0                     ;3
       LSR                            ;2
       LSR                            ;2
       AND    #$20                    ;2
       EOR    #$21                    ;2
       STA    $B0                     ;3
LF379:
       LDA    #$2F                    ;2
       STA    TIM64T                  ;4
       LDA    $C4                     ;3
       LSR                            ;2
       BCS    LF3A0                   ;2
       JSR    LF04E                   ;6
       JSR    LF61B                   ;6
       JSR    LF7E7                   ;6
       JSR    LF74A                   ;6
       LDA    $C7                     ;3
       BEQ    LF3A9                   ;2
       JSR    LFB3E                   ;6
       BIT    $B0                     ;3
       BVC    LF3A9                   ;2
       JSR    LF5AB                   ;6
       JMP    LF3A9                   ;3
LF3A0:
       JSR    LF54F                   ;6
       LDA    SWCHB                   ;4
       LSR                            ;2
       BCC    START                   ;2
LF3A9:
       LDA    INTIM                   ;4
       STA    WSYNC                   ;3
       BPL    LF3A9                   ;2
       LDX    #$02                    ;2
       STX    VSYNC                   ;3
LF3B4:
       STA    WSYNC                   ;3
       DEX                            ;2
       BPL    LF3B4                   ;2
       INX                            ;2
       STX    VSYNC                   ;3
       LDA    #$38                    ;2
       STA    TIM64T                  ;4
       LDA    #$20                    ;2
       AND    $B0                     ;3
       BEQ    LF3D9                   ;2
       LDA    $C4                     ;3
       AND    #$0F                    ;2
       CMP    #$0C                    ;2
       BCS    LF3D9                   ;2
       TAX                            ;2
       JSR    LFBAF                   ;6
       AND    #$F0                    ;2
       ORA    #$08                    ;2
       STA    $D1,X                   ;4
LF3D9:
       INC    $C4                     ;5
       LDA    $C4                     ;3
       LSR                            ;2
       BCS    LF3EC                   ;2
       JSR    LFDB7                   ;6
       JSR    LFF89                   ;6
       JSR    LF8C0                   ;6
       JMP    LF3F2                   ;3
LF3EC:
       JSR    LF694                   ;6
       JSR    LF402                   ;6
LF3F2:
       JSR    LFD87                   ;6
LF3F5:
       LDA    INTIM                   ;4
       STA    WSYNC                   ;3
       BPL    LF3F5                   ;2
       JSR    LFE00                   ;6
       JMP    LF379                   ;3
LF402:
       LDA    #$10                    ;2
       BIT    $B0                     ;3
       BMI    LF40B                   ;2
       JMP    LF48A                   ;3
LF40B:
       BEQ    LF411                   ;2
       BIT    VSYNC                   ;3
       BMI    LF42F                   ;2
LF411:
       LDX    #$04                    ;2
       JSR    LF826                   ;6
       BEQ    LF42C                   ;2
       BMI    LF41D                   ;2
       JMP    LF49D                   ;3
LF41D:
       LDA.wy $D1,Y                   ;4
       CMP    #NEWDOORCOLOR           ;2
       BNE    LF42C                   ;2
       LDA    #HITDOORCOLOR           ;2
       STA.wy $D1,Y                   ;5
       JSR    LF7CF                   ;6
LF42C:
       JMP    LFB86                   ;3
LF42F:
       LDX    #$03                    ;2
LF431:
       LDA    $88,X                   ;4
       SEC                            ;2
       SBC    $8C                     ;3
       CMP    #$04                    ;2
       BPL    LF487                   ;2
       CMP    #$F5                    ;2
       BMI    LF487                   ;2
       JSR    LF8A5                   ;6
       INC    $CB                     ;5
       LDX    $CA                     ;3
       LDA    #$7F                    ;2
       STA    $CC                     ;3
       DEC    $C9                     ;5
       BNE    LF46A                   ;2
       INC    $CC                     ;5
       LSR                            ;2
       STA    $CD                     ;3
       LDA    #$10                    ;2
       EOR    $B0                     ;3
       STA    $B0                     ;3
       CPX    #$07                    ;2
       BEQ    LF45E                   ;2
       INC    $CA                     ;5
LF45E:
       LDY    $C8                     ;3
       BEQ    LF46A                   ;2
       DEY                            ;2
       STY    $C8                     ;3
       LDA    LF53D,Y                 ;4
       STA    $B8                     ;3
LF46A:
       SED                            ;2
       CLC                            ;2
       LDA    $BF                     ;3
       ADC    LF530,X                 ;4
       STA    $BF                     ;3
       LDA    $BE                     ;3
       ADC    LF535,X                 ;4
       STA    $BE                     ;3
       LDA    $BD                     ;3
       ADC    #$00                    ;2
       STA    $BD                     ;3
       CLD                            ;2
       JSR    LF000                   ;6
       JMP    LF7D5                   ;3
LF487:
       DEX                            ;2
       BNE    LF431                   ;2
LF48A:
       BIT    VSYNC                   ;3
       BVC    LF49D                   ;2
       BIT    $B0                     ;3
       BVC    LF49D                   ;2
       LDA    $B0                     ;3
       EOR    #$40                    ;2
       STA    $B0                     ;3
       LDX    #$04                    ;2
       JSR    LF8A5                   ;6
LF49D:
       BIT    VBLANK                  ;3
       BPL    LF4DA                   ;2
       LDX    #$07                    ;2
LF4A3:
       LDA    $88,X                   ;4
       SEC                            ;2
       SBC    $88                     ;3
       CMP    #$0A                    ;2
       BPL    LF4D5                   ;2
       CMP    #$F8                    ;2
       BMI    LF4D5                   ;2
       INC    $C8                     ;5
       LDY    $C8                     ;3
       LDA    LF53D,Y                 ;4
       STA    $B8                     ;3
       CPY    #$05                    ;2
       BMI    LF4CC                   ;2
       LDA    $B0                     ;3
       AND    #$FE                    ;2
       STA    $B0                     ;3
       LDA    #$00                    ;2
       STA    $98                     ;3
       STA    $A0                     ;3
       JSR    LF7E1                   ;6
LF4CC:
       JSR    LF7DB                   ;6
       JSR    LF8A5                   ;6
       JMP    LF006                   ;3
LF4D5:
       DEX                            ;2
       CPX    #$05                    ;2
       BNE    LF4A3                   ;2
LF4DA:
       LDX    #$00                    ;2
       JSR    LF826                   ;6
       BPL    LF51B                   ;2
       LDA    LF543,Y                 ;4
       TAX                            ;2
       LDA    #$20                    ;2
       AND    $B0                     ;3
       BNE    LF4FD                   ;2
       LDA.wy $D1,Y                   ;4
       CMP    #HITDOORCOLOR           ;2
       BNE    LF51B                   ;2
       LDA    $D1,X                   ;4
       CMP    #HITDOORCOLOR           ;2
       BNE    LF51B                   ;2
       LDA    #WALLCOLOR              ;2 <-color of door that has been used
       STA.wy $D1,Y                   ;5
LF4FD:
       LDA    #$1E                    ;2
       STA    $CE                     ;3
       LDA    LFC57,X                 ;4
       CPX    #$08                    ;2
       BMI    LF50A                   ;2
       ADC    #$05                    ;2
LF50A:
       STA    $88                     ;3
       LDA    #$A0                    ;2
       SEC                            ;2
       SBC    $88                     ;3
       STA    $A8                     ;3
       LDA    LF736,X                 ;4
       LDX    #$00                    ;2
       JMP    LFFCE                   ;3
LF51B:
       LDX    #$07                    ;2
LF51D:
       JSR    LF826                   ;6
       CPY    #$0D                    ;2
       BEQ    LF52A                   ;2
       JSR    LF8A5                   ;6
       JSR    LF006                   ;6
LF52A:
       DEX                            ;2
       CPX    #$05                    ;2
       BNE    LF51D                   ;2
       RTS                            ;6

LF530:
       .byte $10 ; |   X    | $F530
       .byte $20 ; |  X     | $F531
       .byte $40 ; | X      | $F532
       .byte $75 ; | XXX X X| $F533
       .byte $50 ; | X X    | $F534
LF535:
       .byte $00 ; |        | $F535
       .byte $00 ; |        | $F536
       .byte $00 ; |        | $F537
       .byte $00 ; |        | $F538
       .byte $01 ; |       X| $F539
       .byte $03 ; |      XX| $F53A
       .byte $05 ; |     X X| $F53B
       .byte $08 ; |    X   | $F53C

;player color
LF53D:
       .byte HIT0COLOR   ; |X     X | $F53D 0 hits
       .byte HIT1COLOR   ; | XX  X  | $F53E 1 hit
       .byte HIT2COLOR   ; | X X X  | $F53F 2 hits
       .byte HIT3COLOR   ; | X  X   | $F540 3 hits
       .byte HIT4COLOR   ; |  X X   | $F541 4 hits
       .byte HIT5COLOR   ; |  X XX  | $F542 5 hits
LF543:
       .byte $08 ; |    X   | $F543
       .byte $09 ; |    X  X| $F544
       .byte $0A ; |    X X | $F545
       .byte $0B ; |    X XX| $F546
       .byte $05 ; |     X X| $F547
       .byte $04 ; |     X  | $F548
       .byte $07 ; |     XXX| $F549
       .byte $06 ; |     XX | $F54A
       .byte $00 ; |        | $F54B
       .byte $01 ; |       X| $F54C
       .byte $02 ; |      X | $F54D
       .byte $03 ; |      XX| $F54E
LF54F:
       LDA    $C0                     ;3
       CLC                            ;2
       ADC    #$09                    ;2
       AND    #$0F                    ;2
       STA    $C0                     ;3
       LDX    #$07                    ;2
       LDY    #$00                    ;2
LF55C:
       LDA    $80,X                   ;4
       BEQ    LF58F                   ;2
       LDA    $A0,X                   ;4
       JSR    LF819                   ;6
       ASL                            ;2
       STA    $E0                     ;3
       CLC                            ;2
       ADC    $88,X                   ;4
LF56B:
       CMP    #$F0                    ;2
       BCS    LF573                   ;2
       CMP    #$02                    ;2
       BCS    LF57B                   ;2
LF573:
       CLC                            ;2
       ADC    #$01                    ;2
       INC    $E0                     ;5
       JMP    LF56B                   ;3
LF57B:
       CMP    LFD77,X                 ;4
       BCC    LF586                   ;2
       SBC    #$01                    ;2
       DEC    $E0                     ;5
       BCS    LF57B                   ;2
LF586:
       STA    $88,X                   ;4
       LDA    $A8,X                   ;4
       SEC                            ;2
       SBC    $E0                     ;3
       STA    $A8,X                   ;4
LF58F:
       LDA    $98,X                   ;4
       JSR    LF819                   ;6
       CLC                            ;2
       ADC    $80,X                   ;4
       CMP    #$0A                    ;2
       BCC    LF5A7                   ;2
LF59B:
       CMP    LFD7F,X                 ;4
       BCC    LF5A4                   ;2
       SBC    #$01                    ;2
       BCS    LF59B                   ;2
LF5A4:
       JSR    LFFCE                   ;6
LF5A7:
       DEX                            ;2
       BPL    LF55C                   ;2
       RTS                            ;6

LF5AB:
       LDX    #$04                    ;2
       TXA                            ;2
       CLC                            ;2
       ADC    $80                     ;3
       STA    $E8                     ;3
       LDA    $88                     ;3
       STA    $E9                     ;3
LF5B7:
       LDA    $E8                     ;3
       SEC                            ;2
       SBC    $80,X                   ;4
       STA    $E0                     ;3
       LDA    $E9                     ;3
       SEC                            ;2
       SBC    $88,X                   ;4
       STA    $E1                     ;3
       JSR    LF9BC                   ;6
       CMP    #$55                    ;2
       BCS    LF5E3                   ;2
       CMP    #$3C                    ;2
       BCS    LF5F2                   ;2
       CMP    #$28                    ;2
       BCS    LF60D                   ;2
       ASL    $E0                     ;5
       ASL    $E1                     ;5
       CMP    #$12                    ;2
       BCS    LF60D                   ;2
       ASL    $E0                     ;5
       ASL    $E1                     ;5
       JMP    LF60D                   ;3
LF5E3:
       TAY                            ;2
       LDA    $E0                     ;3
       ROL                            ;2
       ROR    $E0                     ;5
       LDA    $E1                     ;3
       ROL                            ;2
       ROR    $E1                     ;5
       CPY    #$64                    ;2
       BCC    LF60D                   ;2
LF5F2:
       STX    $E2                     ;3
       LDX    #$01                    ;2
LF5F6:
       LDA    $E0,X                   ;4
       STA    $DF                     ;3
       TAY                            ;2
       ROL                            ;2
       ROR    $DF                     ;5
       TYA                            ;2
       ROL                            ;2
       ROR    $DF                     ;5
       TYA                            ;2
       SEC                            ;2
       SBC    $DF                     ;3
       STA    $E0,X                   ;4
       DEX                            ;2
       BPL    LF5F6                   ;2
       LDX    $E2                     ;3
LF60D:
       LDA    $E0                     ;3
       STA    $98,X                   ;4
       LDA    $E1                     ;3
       ASL                            ;2
       ROR    $E1                     ;5
       LDA    $E1                     ;3
       STA    $A0,X                   ;4
       RTS                            ;6

LF61B:
       LDA    $CB                     ;3
       LDY    #$04                    ;2
LF61F:
       CMP    LF68F,Y                 ;4
       BCC    LF627                   ;2
       DEY                            ;2
       BNE    LF61F                   ;2
LF627:
       LDA    $C4                     ;3
       LSR                            ;2
       AND    #$03                    ;2
       BEQ    LF68E                   ;2
       TAX                            ;2
       LDA    #$0D                    ;2
       STA    $DF                     ;3
       LDA    #$82                    ;2
       STA    $E0                     ;3
LF637:
       LDA    $98,X                   ;4
       CLC                            ;2
       ADC    $B8,X                   ;4
       PHP                            ;3
       CMP    LFC63,Y                 ;4
       BPL    LF649                   ;2
       CMP    LFC7D,Y                 ;4
       BMI    LF649                   ;2
       STA    $98,X                   ;4
LF649:
       CPX    #$08                    ;2
       BPL    LF659                   ;2
       LDA    $B0,X                   ;4
       AND    #$F7                    ;2
       PLP                            ;4
       PHP                            ;3
       BPL    LF657                   ;2
       ORA    #$08                    ;2
LF657:
       STA    $B0,X                   ;4
LF659:
       PLP                            ;4
       LDA    $80,X                   ;4
       CMP    $DF                     ;3
       BCS    LF666                   ;2
       LDA    $98,X                   ;4
       BPL    LF678                   ;2
       BMI    LF670                   ;2
LF666:
       CMP    $E0                     ;3
       BCC    LF678                   ;2
       LDA    $98,X                   ;4
       BMI    LF678                   ;2
       BEQ    LF678                   ;2
LF670:
       LDA    #$00                    ;2
       STA    $98,X                   ;4
       SBC    $B8,X                   ;4
       STA    $B8,X                   ;4
LF678:
       LDA    #$03                    ;2
       STA    $DF                     ;3
       LDA    #$6C                    ;2
       STA    $E0                     ;3
       TXA                            ;2
       CLC                            ;2
       ADC    #$08                    ;2
       TAX                            ;2
       TYA                            ;2
       CLC                            ;2
       ADC    #$05                    ;2
       TAY                            ;2
       CPX    #$0C                    ;2
       BMI    LF637                   ;2
LF68E:
       RTS                            ;6

LF68F:
       .byte $78 ; | XXXX   | $F68F
       .byte $50 ; | X X    | $F690
       .byte $32 ; |  XX  X | $F691
       .byte $1E ; |   XXXX | $F692
       .byte $0F ; |    XXXX| $F693

LF694:
       LDX    $CD                     ;3
       DEX                            ;2
       BMI    LF69C                   ;2
       STX    $CD                     ;3
       RTS                            ;6

LF69C:
       LDX    $CC                     ;3
       DEX                            ;2
       BMI    LF6A5                   ;2
       STX    $CC                     ;3
       BNE    LF6E4                   ;2
LF6A5:
       LDX    #$03                    ;2
       CPX    $C9                     ;3
       BEQ    LF6E5                   ;2
       JSR    LFBAF                   ;6
       BPL    LF6E4                   ;2
       LDA    $C7                     ;3
       BEQ    LF6E4                   ;2
LF6B4:
       LDA    #$00                    ;2
       STA    $DF,X                   ;4
       LDY    #$03                    ;2
LF6BA:
       LDA.wy $88,Y                   ;4
       CMP    LFC47,X                 ;4
       BCC    LF6C9                   ;2
       CMP    LFC4B,X                 ;4
       BCS    LF6C9                   ;2
       INC    $DF,X                   ;6
LF6C9:
       DEY                            ;2
       BNE    LF6BA                   ;2
       DEX                            ;2
       BPL    LF6B4                   ;2
       JSR    LFBAF                   ;6
       AND    #$03                    ;2
       TAY                            ;2
       LDX    #$03                    ;2
LF6D7:
       LDA.wy $DF,Y                   ;4
       BEQ    LF6EC                   ;2
       DEY                            ;2
       BPL    LF6E1                   ;2
       LDY    #$03                    ;2
LF6E1:
       DEX                            ;2
       BPL    LF6D7                   ;2
LF6E4:
       RTS                            ;6

LF6E5:
       LDA    #$10                    ;2
       ORA    $B0                     ;3
       STA    $B0                     ;3
       RTS                            ;6

LF6EC:
       LDA    LFC4F,Y                 ;4
       STA    $E3                     ;3
       JSR    LFBAF                   ;6
       AND    LFC53,Y                 ;4
       CLC                            ;2
       ADC    $E3                     ;3
       LDY    $C9                     ;3
       LDX    LF09D,Y                 ;4
       TAY                            ;2
       INC    $C9                     ;5
       LDA    LF736,Y                 ;4
       JSR    LFFCE                   ;6
       LDA    LFC57,Y                 ;4
       STA    $88,X                   ;4
       LDA    #$D7                    ;2
       SEC                            ;2
       SBC    $88,X                   ;4
       STA    $A8,X                   ;4
       LDX    $CA                     ;3
       LDA    LF742,X                 ;4
       STA    $BC                     ;3
       LDA    #HITDOORCOLOR           ;2
       CMP.wy $D1,Y                   ;4
       BEQ    LF727                   ;2
       LDA    #NEWDOORCOLOR           ;2
       STA.wy $D1,Y                   ;5
LF727:
       BIT    $CC                     ;3
       BPL    LF733                   ;2
       LDX    $C9                     ;3
       CPX    #$03                    ;2
       BNE    LF733                   ;2
       STX    $CC                     ;3
LF733:
       JMP    LF000                   ;3
LF736:
       .byte $22 ; |  X   X | $F736
       .byte $36 ; |  XX XX | $F737
       .byte $56 ; | X X XX | $F738
       .byte $6A ; | XX X X | $F739
       .byte $0A ; |    X X | $F73A
       .byte $82 ; |X     X | $F73B
       .byte $0A ; |    X X | $F73C
       .byte $82 ; |X     X | $F73D
       .byte $22 ; |  X   X | $F73E
       .byte $36 ; |  XX XX | $F73F
       .byte $56 ; | X X XX | $F740
       .byte $6A ; | XX X X | $F741

;enemy colors
LF742:
       .byte ENEMY1COLOR ; |XXX X X | $F742
       .byte ENEMY2COLOR ; |XX   X  | $F743
       .byte ENEMY3COLOR ; |X   X   | $F744
       .byte ENEMY4COLOR ; |    XXX | $F745
       .byte ENEMY5COLOR ; |XX  X   | $F746
       .byte ENEMY6COLOR ; |      X | $F747
       .byte ENEMY7COLOR ; |  X   X | $F748
       .byte ENEMY8COLOR ; |XXXXX   | $F749

LF74A:
       LDA    $CF                     ;3
       BEQ    LF782                   ;2
       DEC    $D0                     ;5
       BEQ    LF765                   ;2
       LSR                            ;2
       BCS    LF76B                   ;2
       LSR                            ;2
       BCS    LF783                   ;2
       LSR                            ;2
       BCS    LF7A6                   ;2
       LSR                            ;2
       BCS    LF790                   ;2
       BCC    LF79C                   ;2
LF760:
       LDA    $CF                     ;3
       LSR                            ;2
       BCC    LF782                   ;2
LF765:
       LDA    #$00                    ;2
       STA    $CF                     ;3
       BEQ    LF780                   ;2
LF76B:
       LDA    #$0E                    ;2
       STA    AUDC0                   ;3
       LDA    $D0                     ;3
       EOR    #$FF                    ;2
       LSR                            ;2
       LSR                            ;2
       SEC                            ;2
       SBC    #$06                    ;2
       STA    AUDF0                   ;3
       EOR    #$3F                    ;2
       LSR                            ;2
       SEC                            ;2
       SBC    #$03                    ;2
LF780:
       STA    AUDV0                   ;3
LF782:
       RTS                            ;6

LF783:
       LDA    #$04                    ;2
       STA    AUDC0                   ;3
       ASL                            ;2
       STA    AUDF0                   ;3
       LDA    $D0                     ;3
       ASL                            ;2
       ASL                            ;2
       BPL    LF780                   ;2
LF790:
       LDA    #$06                    ;2
       STA    AUDC0                   ;3
       LDA    $D0                     ;3
       ASL                            ;2
       STA    AUDV0                   ;3
       LSR                            ;2
       BPL    LF7AF                   ;2
LF79C:
       LDA    #$06                    ;2
       STA    AUDC0                   ;3
       LDA    $D0                     ;3
       LSR                            ;2
       LSR                            ;2
       BPL    LF7AC                   ;2
LF7A6:
       LDA    #$08                    ;2
       STA    AUDC0                   ;3
       LDA    $D0                     ;3
LF7AC:
       LSR                            ;2
       STA    AUDV0                   ;3
LF7AF:
       LSR                            ;2
       EOR    #$FF                    ;2
       CLC                            ;2
       ADC    #$04                    ;2
       STA    $DF                     ;3
       LDA    $D0                     ;3
       AND    #$01                    ;2
       ASL                            ;2
       ASL                            ;2
       ADC    $DF                     ;3
       STA    AUDF0                   ;3
       RTS                            ;6

LF7C2:
       LDY    #$3F                    ;2
       LDA    #$01                    ;2
LF7C6:
       CMP    $CF                     ;3
       BCC    LF7CE                   ;2
       STY    $D0                     ;3
       STA    $CF                     ;3
LF7CE:
       RTS                            ;6

LF7CF:
       LDY    #$04                    ;2
       LDA    #$02                    ;2
       BNE    LF7C6                   ;2
LF7D5:
       LDY    #$19                    ;2
       LDA    #$04                    ;2
       BNE    LF7C6                   ;2
LF7DB:
       LDY    #$07                    ;2
       LDA    #$08                    ;2
       BNE    LF7C6                   ;2
LF7E1:
       LDY    #$7F                    ;2
       LDA    #$10                    ;2
       BNE    LF7C6                   ;2
LF7E7:
       LDA    SWCHA                   ;4
       LSR                            ;2
       LSR                            ;2
       LSR                            ;2
       LSR                            ;2
       AND    SWCHA                   ;4 XX
       TAY                            ;2
       CPY    #$0F                    ;2
       BEQ    LF7F8                   ;2
       STY    $C7                     ;3
LF7F8:
       LDA    $B0                     ;3
       LSR                            ;2
       BCC    LF818                   ;2
       LDA    LFCC1,Y                 ;4
       ASL                            ;2
       STA    $A0                     ;3
       LDA    $B0                     ;3
       AND    #$F7                    ;2
       STA    $DF                     ;3
       LDA    LFCB6,Y                 ;4
       ASL                            ;2
       STA    $98                     ;3
       LSR                            ;2
       BEQ    LF818                   ;2
       AND    #$08                    ;2
       ORA    $DF                     ;3
       STA    $B0                     ;3
LF818:
       RTS                            ;6

LF819:
       CLC                            ;2
       ADC    $C0                     ;3
       LSR                            ;2
       LSR                            ;2
       LSR                            ;2
       LSR                            ;2
       EOR    #$08                    ;2
       SEC                            ;2
       SBC    #$08                    ;2
       RTS                            ;6

LF826:
       LDY    #$0D                    ;2
       LDA    $80,X                   ;4
       CMP    #$0D                    ;2
       BCS    LF836                   ;2
       LDA    $98,X                   ;4
       BPL    LF858                   ;2
       LDY    #$04                    ;2
       BNE    LF843                   ;2
LF836:
       CMP    LF89D,X                 ;4
       BCC    LF858                   ;2
       LDA    $98,X                   ;4
       BMI    LF858                   ;2
       BEQ    LF858                   ;2
       LDY    #$05                    ;2
LF843:
       LDA    $88,X                   ;4
       CMP    #$1D                    ;2
       BCC    LF898                   ;2
       CMP    #$2B                    ;2
       BCC    LF89A                   ;2
       INY                            ;2
       INY                            ;2
       CMP    #$3D                    ;2
       BCC    LF898                   ;2
       CMP    #$4B                    ;2
       BCS    LF898                   ;2
       RTS                            ;6

LF858:
       LDA    $88,X                   ;4
       CMP    #$05                    ;2
       BCS    LF866                   ;2
       LDA    $A0,X                   ;4
       BPL    LF89A                   ;2
       LDY    #$00                    ;2
       BEQ    LF873                   ;2
LF866:
       CMP    LFCD1,X                 ;4
       BCC    LF89A                   ;2
       LDA    $A0,X                   ;4
       BMI    LF89A                   ;2
       BEQ    LF89A                   ;2
       LDY    #$08                    ;2
LF873:
       LDA    $80,X                   ;4
       CMP    #$1E                    ;2
       BCC    LF898                   ;2
       CMP    #$2A                    ;2
       BCC    LF89A                   ;2
       INY                            ;2
       CMP    #$32                    ;2
       BCC    LF898                   ;2
       CMP    #$3E                    ;2
       BCC    LF89A                   ;2
       INY                            ;2
       CMP    #$52                    ;2
       BCC    LF898                   ;2
       CMP    #$5E                    ;2
       BCC    LF89A                   ;2
       INY                            ;2
       CMP    #$66                    ;2
       BCC    LF898                   ;2
       CMP    #$72                    ;2
       BCC    LF89A                   ;2
LF898:
       LDY    #$0C                    ;2
LF89A:
       CPY    #$0C                    ;2
       RTS                            ;6

LF89D:
       .byte $82 ; |X     X | $F89D
       .byte $00 ; |        | $F89E
       .byte $00 ; |        | $F89F
       .byte $00 ; |        | $F8A0
       .byte $87 ; |X    XXX| $F8A1
       .byte $86 ; |X    XX | $F8A2
       .byte $86 ; |X    XX | $F8A3
       .byte $86 ; |X    XX | $F8A4
LF8A5:
       LDA    #$7C                    ;2
       STA    $88,X                   ;4
       LDY    #$00                    ;2
       TXA                            ;2
       AND    #$03                    ;2
       BNE    LF8B2                   ;2
       STA    $88,X                   ;4
LF8B2:
       CPX    #$05                    ;2
       BCC    LF8B8                   ;2
       STY    $B0,X                   ;4
LF8B8:
       STY    $98,X                   ;4
       STY    $A0,X                   ;4
       TYA                            ;2
       JMP    LFFCE                   ;3
LF8C0:
       DEC    $CE                     ;5
       BPL    LF91B                   ;2
       LDA    #$08                    ;2
       STA    $CE                     ;3
       LDY    $C9                     ;3
       BEQ    LF91B                   ;2
LF8CC:
       JSR    LFBAF                   ;6
       LSR                            ;2
       LSR                            ;2
       AND    #$03                    ;2
       CMP    LF09C,Y                 ;4
       BCC    LF8CC                   ;2
       TAY                            ;2
       LDA    $B0                     ;3
       LSR                            ;2
       BCC    LF91B                   ;2

       BIT    $CB                     ;3
       BMI    LF8ED                   ;2
       BVS    LF8ED                   ;2

       LDA    $CB                     ;3
       ASL                            ;2
       ADC    #$50                    ;2
       CMP    $C5                     ;3
       BCC    LF91B                   ;2
LF8ED:
       LDX    #$07                    ;2
       BIT    $B7                     ;3
       BPL    LF8FE                   ;2
       DEX                            ;2
       BIT    $B6                     ;3
       BMI    LF91B                   ;2
       LDA    $CA                     ;3
       CMP    #$02                    ;2
       BMI    LF91B                   ;2
LF8FE:
       LDA.wy $88,Y                   ;4
       CLC                            ;2
       ADC    #$04                    ;2
       CPX    #$06                    ;2
       BNE    LF91E                   ;2
       STA    $E7                     ;3
       LDA    $8F                     ;3
       SEC                            ;2
       SBC    #$18                    ;2
       BMI    LF915                   ;2
       CMP    $E7                     ;3
       BCS    LF91C                   ;2
LF915:
       ADC    #$30                    ;2
       CMP    $E7                     ;3
       BCC    LF91C                   ;2
LF91B:
       RTS                            ;6

LF91C:
       LDA    $E7                     ;3
LF91E:
       STA    $88,X                   ;4
       CLC                            ;2
       EOR    #$FF                    ;2
       ADC    #$F9                    ;2
       STA    $A8,X                   ;4
       LDA.wy $80,Y                   ;4
       STA    $80,X                   ;4
       LDA    #$03                    ;2
       JSR    LFFCB                   ;6
       LDA    #$A0                    ;2
       STA    $B0,X                   ;4
       LDA    #$00                    ;2
       SEC                            ;2
       SBC    $CB                     ;3
       LSR                            ;2
       LSR                            ;2
       LSR                            ;2
       LSR                            ;2
       LSR                            ;2
       STA    $E6                     ;3
       LDY    #$01                    ;2
LF943:
       JSR    LFBAF                   ;6
       CMP    $E6                     ;3
       BCC    LF94C                   ;2
       LDA    $E6                     ;3
LF94C:
       BIT    $C5                     ;3
       BPL    LF952                   ;2
       EOR    #$FF                    ;2
LF952:
       STA.wy $E8,Y                   ;5
       DEY                            ;2
       BPL    LF943                   ;2
       LDA    $80                     ;3
       CLC                            ;2
       ADC    $E8                     ;3
       STA    $E8                     ;3
       LDA    $88                     ;3
       CLC                            ;2
       ADC    $E9                     ;3
       STA    $E9                     ;3
       JSR    LF5B7                   ;6
       LDA    $98,X                   ;4
       STA    $E0                     ;3
       LDA    $A0,X                   ;4
       STA    $E1                     ;3
       JSR    LF9BC                   ;6
       BEQ    LF9B6                   ;2
       LDA    $CB                     ;3
       LSR                            ;2
       LSR                            ;2
       LSR                            ;2
       CLC                            ;2
       ADC    #$1E                    ;2
       STA    $E6                     ;3
       LSR                            ;2
       STA    $E5                     ;3
LF983:
       LDA    $DF                     ;3
       CMP    $E6                     ;3
       BCC    LF997                   ;2
       LDA    $98,X                   ;4
       ASL                            ;2
       ROR    $98,X                   ;6
       LDA    $A0,X                   ;4
       ASL                            ;2
       ROR    $A0,X                   ;6
       LSR    $DF                     ;5
       BNE    LF983                   ;2
LF997:
       CMP    $E5                     ;3
       BCS    LF9A3                   ;2
       ASL    $98,X                   ;6
       ASL    $A0,X                   ;6
       ASL    $DF                     ;5
       BNE    LF983                   ;2
LF9A3:
       CPX    #$06                    ;2
       BNE    LF9B9                   ;2
       LDA    $8E                     ;3
       SBC    $8F                     ;3
       STA    $DF                     ;3
       LDA    $A6                     ;3
       SEC                            ;2
       SBC    $A7                     ;3
       EOR    $DF                     ;3
       BPL    LF9B9                   ;2
LF9B6:
       JMP    LF8A5                   ;3
LF9B9:
       JMP    LF006                   ;3
LF9BC:
       LDA    $E0                     ;3
       BPL    LF9C2                   ;2
       EOR    #$FF                    ;2
LF9C2:
       STA    $DF                     ;3
       LDA    $E1                     ;3
       BPL    LF9CA                   ;2
       EOR    #$FF                    ;2
LF9CA:
       CMP    $DF                     ;3
       BCS    LF9D3                   ;2
       LDY    $DF                     ;3
       STA    $DF                     ;3
       TYA                            ;2
LF9D3:
       LSR    $DF                     ;5
       CLC                            ;2
       ADC    $DF                     ;3
       STA    $DF                     ;3
       RTS                            ;6

;unused?
       .byte $FF ; |XXXXXXXX| $F9DB
       .byte $FF ; |XXXXXXXX| $F9DC
       .byte $FF ; |XXXXXXXX| $F9DD
       .byte $FF ; |XXXXXXXX| $F9DE
       .byte $FF ; |XXXXXXXX| $F9DF
       .byte $FF ; |XXXXXXXX| $F9E0
       .byte $FF ; |XXXXXXXX| $F9E1
       .byte $FF ; |XXXXXXXX| $F9E2
       .byte $FF ; |XXXXXXXX| $F9E3
       .byte $FF ; |XXXXXXXX| $F9E4
       .byte $FF ; |XXXXXXXX| $F9E5
       .byte $FF ; |XXXXXXXX| $F9E6
       .byte $FF ; |XXXXXXXX| $F9E7
       .byte $FF ; |XXXXXXXX| $F9E8
       .byte $FF ; |XXXXXXXX| $F9E9
       .byte $FF ; |XXXXXXXX| $F9EA
       .byte $FF ; |XXXXXXXX| $F9EB
       .byte $FF ; |XXXXXXXX| $F9EC

LF9ED:
       LDA    $E8                     ;3
       STA    COLUPF                  ;3
       JMP    LF318                   ;3
LF9F4:
       LDA    $E8                     ;3
       STA.w  RESP1                   ;4
       STA    COLUPF                  ;3
       STA    $3F                     ;3
       JMP    LFA6B                   ;3

LFA00:
       LDX    $E2                     ;3
       LDA    $90,X                   ;4
       STA.w  RESP1                   ;4
       STA    HMCLR                   ;3
       LDX    #$94                    ;2
       STX    $EA                     ;3
       STA    HMP1                    ;3
       JMP    LF2FA                   ;3

LFA12:
       LDX    $E2                     ;3
       LDA    $90,X                   ;4
       STA    HMCLR                   ;3
       STA    HMP1                    ;3
       STA    RESP1                   ;3
       LDA    #$94                    ;2
       STA.w  $EA                     ;4
       JMP    LF2FA                   ;3

LFA24:
       LDX    $E2                     ;3
       LDA    $90,X                   ;4
       STA    HMCLR                   ;3
       STA    HMP1                    ;3
       LDA    #$94                    ;2
       STA    $EA                     ;3
       STA    RESP1                   ;3
       LDA.w  $E8                     ;4
       JMP    LF2FC                   ;3

LFA38:
       LDX    $E2                     ;3
       LDA    $90,X                   ;4
       STA    HMCLR                   ;3
       STA    HMP1                    ;3
       LDA    #$94                    ;2
       STA    $EA                     ;3
       LDA    ($F8),Y                 ;5
       STA    RESP1                   ;3
       BPL    LF9ED                   ;2
       LDA    $E8                     ;3
       STA.w  COLUPF                  ;4
       JMP    LF302                   ;3

LFA52:
       LDX    $E2                     ;3
       LDA    $90,X                   ;4
       STA    HMCLR                   ;3
       STA    HMP1                    ;3
       LDX    #$94                    ;2
       LDA    ($F8),Y                 ;5
       BPL    LF9F4                   ;2
       LDA    ($F0),Y                 ;5
       STA.w  RESP1                   ;4
       STA    ENAM0                   ;3
       LDA    $E8                     ;3
       STA    COLUPF                  ;3
LFA6B:
       STX    $EA                     ;3
       JMP    LF306                   ;3

LFA70:
       LDX    $E2                     ;3
       LDA    $90,X                   ;4
       STA    HMCLR                   ;3
       STA    HMP1                    ;3
       LDX    #$94                    ;2
       LDA    $E8                     ;3
       STA    COLUPF                  ;3
       LDA    ($F8),Y                 ;5
       BPL    LFA8C                   ;2
       LDA    ($F0),Y                 ;5
       STA    RESP1                   ;3
       STX.w  $EA                     ;4
       JMP    LF304                   ;3

LFA8C:
       STX    $EA                     ;3
       STA.w  RESP1                   ;4
       JMP    LF318                   ;3

LFA94:
       LDA    ($F2),Y                 ;5
       STA    GRP1                    ;3
       STA    HMCLR                   ;3
       LDA    #$85                    ;2
       STA    $EA                     ;3
       LDA    #$F2                    ;2
       STA.w  $EB                     ;4
       JMP    LF2FA                   ;3

LFAA6:
       LDX    $E2                     ;3
       LDA    $90,X                   ;4
       STA    HMCLR                   ;3
       STA    HMP1                    ;3
       LDA    ($F8),Y                 ;5
       BPL    LFB2C                   ;2
       LDA    ($F0),Y                 ;5
       STA    ENAM0                   ;3
LFAB6:
       LDA    ($F6),Y                 ;5
       BPL    LFAC8                   ;2
       LDA    $E8                     ;3
       STA    RESP1                   ;3
       STA    COLUPF                  ;3
       LDA    #$94                    ;2
       STA.w  $EA                     ;4
       JMP    LF30A                   ;3

LFAC8:
       LDA    #$94                    ;2
       STA    RESP1                   ;3
       STA    $EA                     ;3
       LDA    $E8                     ;3
       STA.w  COLUPF                  ;4
       JMP    LF31D                   ;3

LFAD6:
       LDX    $E2                     ;3
       LDA    $90,X                   ;4
       STA    HMCLR                   ;3
       STA    HMP1                    ;3
       LDX    #$94                    ;2
       LDA    ($F8),Y                 ;5
       BPL    LFB2F                   ;2
       LDA    ($F0),Y                 ;5
       STA    ENAM0                   ;3
LFAE8:
       LDA    ($F6),Y                 ;5
       BPL    LFB32                   ;2
       LDA    ($EE),Y                 ;5
       STA.w  RESP1                   ;4
       STA    GRP0                    ;3
       LDA    $E8                     ;3
       STA    COLUPF                  ;3
LFAF7:
       STX    $EA                     ;3
       JMP    LF30E                   ;3

LFAFC:
       LDX    $E2                     ;3
       LDA    $90,X                   ;4
       STA    HMCLR                   ;3
       STA    HMP1                    ;3
       LDX    #$94                    ;2
       LDA    $E8                     ;3
       STA    COLUPF                  ;3
       LDA    ($F8),Y                 ;5
       BPL    LFB20                   ;2
       LDA    ($F0),Y                 ;5
       STA    ENAM0                   ;3
LFB12:
       LDA    ($F6),Y                 ;5
       BPL    LFB24                   ;2
       LDA    ($EE),Y                 ;5
       STA    RESP1                   ;3
       STX.w  $EA                     ;4
       JMP    LF30C                   ;3

LFB20:
       NOP                            ;2
       NOP                            ;2
       BPL    LFB12                   ;2
LFB24:
       STX    $EA                     ;3
       STA.w  RESP1                   ;4
       JMP    LF31D                   ;3

LFB2C:
       NOP                            ;2
       BPL    LFAB6                   ;2
LFB2F:
       NOP                            ;2
       BPL    LFAE8                   ;2
LFB32:
       LDA    $E8                     ;3
       STA.w  RESP1                   ;4
       STA    COLUPF                  ;3
       STA    $3F                     ;3
       JMP    LFAF7                   ;3

LFB3E:
       LDA    $B0                     ;3
       LSR                            ;2
       BCC    LFBAE                   ;2
       LSR                            ;2
       BCS    LFBA1                   ;2
       LDA    REFP1                   ;3
       EOR    PF0                     ;3
       ASL                            ;2
       BCC    LFBAE                   ;2
       LDA    $B0                     ;3
       EOR    #$02                    ;2
       STA    $B0                     ;3

;check if returning disc
       BIT    $B0                     ;3
       BMI    LFB86                   ;2
       BVS    LFBAE                   ;2

       LDA    $B0                     ;3
       EOR    #$80                    ;2
       STA    $B0                     ;3
       LDA    $80                     ;3
       CLC                            ;2
       ADC    #$04                    ;2
       STA    $84                     ;3
       LDA    $88                     ;3
       STA    $8C                     ;3
       LDA    #$F0                    ;2
       SEC                            ;2
       SBC    $8C                     ;3
       STA    $AC                     ;3
       LDA    #$20                    ;2
       STA    $B4                     ;3
       LDX    $C7                     ;3
       LDA    LFCD4,X                 ;4
       ASL                            ;2
       STA    $9C                     ;3
       LDA    LFCDE,X                 ;4
       ASL                            ;2
       STA    $A4                     ;3
       JMP    LF7C2                   ;3

LFB86:
       LDA    $B0                     ;3
       EOR    #$C0                    ;2
       STA    $B0                     ;3
       LDA    $AC                     ;3
       SEC                            ;2
       SBC    #$10                    ;2
       STA    $AC                     ;3
       LDA    $84                     ;3
       CLC                            ;2
       ADC    #$01                    ;2
       STA    $84                     ;3
       LDA    #$10                    ;2
       STA    $B4                     ;3
       JMP    LF760                   ;3

LFBA1:
       LDA    REFP1                   ;3
       EOR    PF0                     ;3
       ASL                            ;2
       BCS    LFBAE                   ;2
       LDA    $B0                     ;3
       AND    #$FD                    ;2
       STA    $B0                     ;3
LFBAE:
       RTS                            ;6

LFBAF:
       LDA    $C5                     ;3
       ASL                            ;2
       ASL                            ;2
       ASL                            ;2
       ASL                            ;2
       CLC                            ;2
       ADC    $C5                     ;3
       ASL                            ;2
       ASL                            ;2
       ASL                            ;2
       CLC                            ;2
       ADC    $C5                     ;3
       CLC                            ;2
       ADC    #$95                    ;2
       EOR    $C4                     ;3
       STA    $C5                     ;3
       RTS                            ;6

;unused
       .byte $FF ; |XXXXXXXX| $FBC6
       .byte $FF ; |XXXXXXXX| $FBC7
       .byte $FF ; |XXXXXXXX| $FBC8
       .byte $FF ; |XXXXXXXX| $FBC9
       .byte $FF ; |XXXXXXXX| $FBCA
       .byte $FF ; |XXXXXXXX| $FBCB
       .byte $FF ; |XXXXXXXX| $FBCC
       .byte $FF ; |XXXXXXXX| $FBCD
       .byte $FF ; |XXXXXXXX| $FBCE
       .byte $FF ; |XXXXXXXX| $FBCF
       .byte $FF ; |XXXXXXXX| $FBD0
       .byte $FF ; |XXXXXXXX| $FBD1
       .byte $FF ; |XXXXXXXX| $FBD2
       .byte $FF ; |XXXXXXXX| $FBD3
       .byte $FF ; |XXXXXXXX| $FBD4
       .byte $FF ; |XXXXXXXX| $FBD5
       .byte $FF ; |XXXXXXXX| $FBD6
       .byte $FF ; |XXXXXXXX| $FBD7
       .byte $FF ; |XXXXXXXX| $FBD8
       .byte $FF ; |XXXXXXXX| $FBD9
       .byte $FF ; |XXXXXXXX| $FBDA


       .byte $0C ; |    XX  | $FBDB
       .byte $0C ; |    XX  | $FBDC
       .byte $1C ; |   XXX  | $FBDD
       .byte $1C ; |   XXX  | $FBDE
       .byte $38 ; |  XXX   | $FBDF
       .byte $38 ; |  XXX   | $FBE0
       .byte $1E ; |   XXXX | $FBE1
       .byte $38 ; |  XXX   | $FBE2
       .byte $38 ; |  XXX   | $FBE3
       .byte $30 ; |  XX    | $FBE4
       .byte $28 ; |  X X   | $FBE5
       .byte $30 ; |  XX    | $FBE6
       .byte $08 ; |    X   | $FBE7
       .byte $20 ; |  X     | $FBE8
       .byte $00 ; |        | $FBE9
       .byte $00 ; |        | $FBEA
       .byte $0C ; |    XX  | $FBEB
       .byte $0C ; |    XX  | $FBEC
       .byte $1C ; |   XXX  | $FBED
       .byte $3D ; |  XXXX X| $FBEE
       .byte $38 ; |  XXX   | $FBEF
       .byte $5E ; | X XXXX | $FBF0
       .byte $1E ; |   XXXX | $FBF1
       .byte $18 ; |   XX   | $FBF2
       .byte $38 ; |  XXX   | $FBF3
       .byte $3C ; |  XXXX  | $FBF4
       .byte $6C ; | XX XX  | $FBF5
       .byte $E2 ; |XXX   X | $FBF6
       .byte $40 ; | X      | $FBF7
       .byte $02 ; |      X | $FBF8
       .byte $00 ; |        | $FBF9
       .byte $00 ; |        | $FBFA
       .byte $02 ; |      X | $FBFB
       .byte $02 ; |      X | $FBFC
       .byte $02 ; |      X | $FBFD
       .byte $00 ; |        | $FBFE
       .byte $00 ; |        | $FBFF

IFC00:
       .byte $7E ; | XXXXXX | $FC00
       .byte $66 ; | XX  XX | $FC01
       .byte $66 ; | XX  XX | $FC02
       .byte $66 ; | XX  XX | $FC03
       .byte $66 ; | XX  XX | $FC04
       .byte $7E ; | XXXXXX | $FC05
IFC06:
       .byte $7E ; | XXXXXX | $FC06
       .byte $18 ; |   XX   | $FC07
       .byte $18 ; |   XX   | $FC08
       .byte $18 ; |   XX   | $FC09
       .byte $18 ; |   XX   | $FC0A
       .byte $78 ; | XXXX   | $FC0B
IFC0C:
       .byte $7E ; | XXXXXX | $FC0C
       .byte $60 ; | XX     | $FC0D
       .byte $7E ; | XXXXXX | $FC0E
       .byte $06 ; |     XX | $FC0F
       .byte $66 ; | XX  XX | $FC10
       .byte $7E ; | XXXXXX | $FC11
IFC12:
       .byte $7E ; | XXXXXX | $FC12
       .byte $06 ; |     XX | $FC13
       .byte $06 ; |     XX | $FC14
       .byte $7C ; | XXXXX  | $FC15
       .byte $06 ; |     XX | $FC16
       .byte $7E ; | XXXXXX | $FC17
IFC18:
       .byte $06 ; |     XX | $FC18
       .byte $06 ; |     XX | $FC19
       .byte $7E ; | XXXXXX | $FC1A
       .byte $66 ; | XX  XX | $FC1B
       .byte $66 ; | XX  XX | $FC1C
       .byte $66 ; | XX  XX | $FC1D
IFC1E:
       .byte $7E ; | XXXXXX | $FC1E
       .byte $66 ; | XX  XX | $FC1F
       .byte $06 ; |     XX | $FC20
       .byte $7E ; | XXXXXX | $FC21
       .byte $60 ; | XX     | $FC22
       .byte $7E ; | XXXXXX | $FC23
IFC24:
       .byte $7E ; | XXXXXX | $FC24
       .byte $66 ; | XX  XX | $FC25
       .byte $66 ; | XX  XX | $FC26
       .byte $7E ; | XXXXXX | $FC27
       .byte $60 ; | XX     | $FC28
       .byte $7E ; | XXXXXX | $FC29
IFC2A:
       .byte $20 ; |  X     | $FC2A
       .byte $30 ; |  XX    | $FC2B
       .byte $18 ; |   XX   | $FC2C
       .byte $0C ; |    XX  | $FC2D
       .byte $06 ; |     XX | $FC2E
       .byte $7E ; | XXXXXX | $FC2F
IFC30:
       .byte $7E ; | XXXXXX | $FC30
       .byte $66 ; | XX  XX | $FC31
       .byte $66 ; | XX  XX | $FC32
       .byte $3C ; |  XXXX  | $FC33
       .byte $66 ; | XX  XX | $FC34
       .byte $7E ; | XXXXXX | $FC35
IFC36:
       .byte $7E ; | XXXXXX | $FC36
       .byte $06 ; |     XX | $FC37
       .byte $7E ; | XXXXXX | $FC38
       .byte $66 ; | XX  XX | $FC39
       .byte $66 ; | XX  XX | $FC3A
       .byte $7E ; | XXXXXX | $FC3B
LFC3C:
       .byte <IFC00 ;Digit 0 $FC3C
       .byte <IFC06 ;Digit 1 $FC3D
       .byte <IFC0C ;Digit 2 $FC3E
       .byte <IFC12 ;Digit 3 $FC3F
       .byte <IFC18 ;Digit 4 $FC40
       .byte <IFC1E ;Digit 5 $FC41
       .byte <IFC24 ;Digit 6 $FC42
       .byte <IFC2A ;Digit 7 $FC43
       .byte <IFC30 ;Digit 8 $FC44
       .byte <IFC36 ;Digit 9 $FC45
       .byte <LFC3C ;Digit table $FC46
LFC47:
       .byte $00 ; |        | $FC47
       .byte $04 ; |     X  | $FC48
       .byte $24 ; |  X  X  | $FC49
       .byte $44 ; | X   X  | $FC4A
LFC4B:
       .byte $20 ; |  X     | $FC4B
       .byte $40 ; | X      | $FC4C
       .byte $5E ; | X XXXX | $FC4D
       .byte $68 ; | XX X   | $FC4E
LFC4F:
       .byte $00 ; |        | $FC4F
       .byte $04 ; |     X  | $FC50
       .byte $06 ; |     XX | $FC51
       .byte $08 ; |    X   | $FC52
LFC53:
       .byte $03 ; |      XX| $FC53
       .byte $01 ; |       X| $FC54
       .byte $01 ; |       X| $FC55
       .byte $03 ; |      XX| $FC56
LFC57:
       .byte $02 ; |      X | $FC57
       .byte $02 ; |      X | $FC58
       .byte $02 ; |      X | $FC59
       .byte $02 ; |      X | $FC5A
       .byte $22 ; |  X   X | $FC5B
       .byte $22 ; |  X   X | $FC5C
       .byte $42 ; | X    X | $FC5D
       .byte $42 ; | X    X | $FC5E
       .byte $60 ; | XX     | $FC5F
       .byte $60 ; | XX     | $FC60
       .byte $60 ; | XX     | $FC61
       .byte $60 ; | XX     | $FC62
LFC63:
       .byte $10 ; |   X    | $FC63
       .byte $0C ; |    XX  | $FC64
       .byte $08 ; |    X   | $FC65
       .byte $06 ; |     XX | $FC66
       .byte $04 ; |     X  | $FC67
       .byte $08 ; |    X   | $FC68
       .byte $06 ; |     XX | $FC69
       .byte $04 ; |     X  | $FC6A
       .byte $03 ; |      XX| $FC6B
       .byte $02 ; |      X | $FC6C
       .byte $00 ; |        | $FC6D
       .byte $00 ; |        | $FC6E
       .byte $00 ; |        | $FC6F
       .byte $00 ; |        | $FC70
       .byte $00 ; |        | $FC71
       .byte $00 ; |        | $FC72
       .byte $00 ; |        | $FC73
       .byte $00 ; |        | $FC74
       .byte $00 ; |        | $FC75
       .byte $00 ; |        | $FC76
       .byte $80 ; |X       | $FC77
       .byte $80 ; |X       | $FC78
       .byte $80 ; |X       | $FC79
       .byte $80 ; |X       | $FC7A
       .byte $80 ; |X       | $FC7B
       .byte $80 ; |X       | $FC7C
LFC7D:
       .byte $F0 ; |XXXX    | $FC7D
       .byte $F4 ; |XXXX X  | $FC7E
       .byte $F8 ; |XXXXX   | $FC7F
       .byte $FA ; |XXXXX X | $FC80
       .byte $FC ; |XXXXXX  | $FC81
       .byte $F8 ; |XXXXX   | $FC82
       .byte $FA ; |XXXXX X | $FC83
       .byte $FC ; |XXXXXX  | $FC84
       .byte $FD ; |XXXXXX X| $FC85
       .byte $FE ; |XXXXXXX | $FC86
       .byte $00 ; |        | $FC87
       .byte $00 ; |        | $FC88
       .byte $00 ; |        | $FC89
       .byte $00 ; |        | $FC8A
       .byte $00 ; |        | $FC8B
       .byte $00 ; |        | $FC8C
       .byte $00 ; |        | $FC8D
       .byte $00 ; |        | $FC8E
       .byte $00 ; |        | $FC8F
       .byte $00 ; |        | $FC90
       .byte $00 ; |        | $FC91
       .byte $00 ; |        | $FC92
       .byte $00 ; |        | $FC93
       .byte $00 ; |        | $FC94
       .byte $00 ; |        | $FC95
       .byte $00 ; |        | $FC96
       .byte $00 ; |        | $FC97
       .byte $00 ; |        | $FC98
       .byte $00 ; |        | $FC99
       .byte $00 ; |        | $FC9A
       .byte $00 ; |        | $FC9B
       .byte $00 ; |        | $FC9C
       .byte $00 ; |        | $FC9D
       .byte $00 ; |        | $FC9E
       .byte $00 ; |        | $FC9F
       .byte $00 ; |        | $FCA0
       .byte $00 ; |        | $FCA1
       .byte $00 ; |        | $FCA2
       .byte $00 ; |        | $FCA3
       .byte $00 ; |        | $FCA4
       .byte $00 ; |        | $FCA5
       .byte $00 ; |        | $FCA6
       .byte $00 ; |        | $FCA7
       .byte $00 ; |        | $FCA8
       .byte $00 ; |        | $FCA9
       .byte $00 ; |        | $FCAA
       .byte $00 ; |        | $FCAB
       .byte $00 ; |        | $FCAC
       .byte $00 ; |        | $FCAD
       .byte $00 ; |        | $FCAE
       .byte $00 ; |        | $FCAF
       .byte $00 ; |        | $FCB0
       .byte $00 ; |        | $FCB1
       .byte $00 ; |        | $FCB2
       .byte $00 ; |        | $FCB3
       .byte $00 ; |        | $FCB4
       .byte $00 ; |        | $FCB5
LFCB6:
       .byte $00 ; |        | $FCB6
       .byte $00 ; |        | $FCB7
       .byte $00 ; |        | $FCB8
       .byte $00 ; |        | $FCB9
       .byte $00 ; |        | $FCBA
       .byte $05 ; |     X X| $FCBB
       .byte $05 ; |     X X| $FCBC
       .byte $07 ; |     XXX| $FCBD
       .byte $00 ; |        | $FCBE
       .byte $7B ; | XXXX XX| $FCBF
       .byte $7B ; | XXXX XX| $FCC0
LFCC1:
       .byte $79 ; | XXXX  X| $FCC1
       .byte $00 ; |        | $FCC2
       .byte $00 ; |        | $FCC3
       .byte $00 ; |        | $FCC4
       .byte $00 ; |        | $FCC5
       .byte $03 ; |      XX| $FCC6
       .byte $7D ; | XXXXX X| $FCC7
       .byte $00 ; |        | $FCC8
       .byte $00 ; |        | $FCC9
       .byte $03 ; |      XX| $FCCA
       .byte $7D ; | XXXXX X| $FCCB
       .byte $00 ; |        | $FCCC
       .byte $00 ; |        | $FCCD
       .byte $05 ; |     X X| $FCCE
       .byte $7B ; | XXXX XX| $FCCF
       .byte $00 ; |        | $FCD0
LFCD1:
       .byte $66 ; | XX  XX | $FCD1
       .byte $00 ; |        | $FCD2
       .byte $00 ; |        | $FCD3
LFCD4:
       .byte $00 ; |        | $FCD4
       .byte $6E ; | XX XXX | $FCD5
       .byte $6C ; | XX XX  | $FCD6
       .byte $6C ; | XX XX  | $FCD7
       .byte $6C ; | XX XX  | $FCD8
       .byte $0F ; |    XXXX| $FCD9
       .byte $0F ; |    XXXX| $FCDA
       .byte $15 ; |   X X X| $FCDB
       .byte $00 ; |        | $FCDC
       .byte $71 ; | XXX   X| $FCDD
LFCDE:
       .byte $71 ; | XXX   X| $FCDE
       .byte $6B ; | XX X XX| $FCDF
       .byte $00 ; |        | $FCE0
       .byte $00 ; |        | $FCE1
       .byte $00 ; |        | $FCE2
       .byte $0B ; |    X XX| $FCE3
       .byte $75 ; | XXX X X| $FCE4
       .byte $00 ; |        | $FCE5
       .byte $00 ; |        | $FCE6
       .byte $0B ; |    X XX| $FCE7
       .byte $75 ; | XXX X X| $FCE8
       .byte $00 ; |        | $FCE9
       .byte $00 ; |        | $FCEA
       .byte $0F ; |    XXXX| $FCEB
       .byte $71 ; | XXX   X| $FCEC

;unused?
       .byte $FF ; |XXXXXXXX| $FCED
       .byte $FF ; |XXXXXXXX| $FCEE
       .byte $FF ; |XXXXXXXX| $FCEF
       .byte $FF ; |XXXXXXXX| $FCF0
       .byte $FF ; |XXXXXXXX| $FCF1
       .byte $FF ; |XXXXXXXX| $FCF2
       .byte $FF ; |XXXXXXXX| $FCF3
       .byte $FF ; |XXXXXXXX| $FCF4
       .byte $FF ; |XXXXXXXX| $FCF5
       .byte $FF ; |XXXXXXXX| $FCF6
       .byte $FF ; |XXXXXXXX| $FCF7
       .byte $FF ; |XXXXXXXX| $FCF8
       .byte $FF ; |XXXXXXXX| $FCF9
       .byte $FF ; |XXXXXXXX| $FCFA
       .byte $FF ; |XXXXXXXX| $FCFB
       .byte $FF ; |XXXXXXXX| $FCFC
       .byte $FF ; |XXXXXXXX| $FCFD
       .byte $FF ; |XXXXXXXX| $FCFE
       .byte $FF ; |XXXXXXXX| $FCFF
LFD00:
       .byte $00 ; |        | $FD00
       .byte $DD ; |XX XXX X| $FD01
       .byte $00 ; |        | $FD02
       .byte $DD ; |XX XXX X| $FD03
       .byte $00 ; |        | $FD04
       .byte $DD ; |XX XXX X| $FD05
       .byte $00 ; |        | $FD06
       .byte $DD ; |XX XXX X| $FD07
       .byte $00 ; |        | $FD08
       .byte $DD ; |XX XXX X| $FD09
       .byte $00 ; |        | $FD0A
       .byte $DD ; |XX XXX X| $FD0B
       .byte $00 ; |        | $FD0C
       .byte $DD ; |XX XXX X| $FD0D
       .byte $00 ; |        | $FD0E
       .byte $DD ; |XX XXX X| $FD0F
       .byte $00 ; |        | $FD10
       .byte $DD ; |XX XXX X| $FD11
       .byte $00 ; |        | $FD12
       .byte $DD ; |XX XXX X| $FD13
       .byte $00 ; |        | $FD14
       .byte $DD ; |XX XXX X| $FD15
       .byte $00 ; |        | $FD16
       .byte $DD ; |XX XXX X| $FD17
       .byte $00 ; |        | $FD18
       .byte $DD ; |XX XXX X| $FD19
       .byte $00 ; |        | $FD1A
       .byte $DD ; |XX XXX X| $FD1B
       .byte $00 ; |        | $FD1C
       .byte $DD ; |XX XXX X| $FD1D
       .byte $00 ; |        | $FD1E
       .byte $DD ; |XX XXX X| $FD1F
       .byte $00 ; |        | $FD20
       .byte $DD ; |XX XXX X| $FD21
       .byte $00 ; |        | $FD22
       .byte $D5 ; |XX X X X| $FD23
       .byte $00 ; |        | $FD24
       .byte $D5 ; |XX X X X| $FD25
       .byte $00 ; |        | $FD26
       .byte $D5 ; |XX X X X| $FD27
       .byte $00 ; |        | $FD28
       .byte $D5 ; |XX X X X| $FD29
       .byte $00 ; |        | $FD2A
       .byte $D5 ; |XX X X X| $FD2B
       .byte $00 ; |        | $FD2C
       .byte $D5 ; |XX X X X| $FD2D
       .byte $00 ; |        | $FD2E
       .byte $D5 ; |XX X X X| $FD2F
       .byte $00 ; |        | $FD30
       .byte $D5 ; |XX X X X| $FD31
       .byte $00 ; |        | $FD32
       .byte $DD ; |XX XXX X| $FD33
       .byte $00 ; |        | $FD34
       .byte $DD ; |XX XXX X| $FD35
       .byte $00 ; |        | $FD36
       .byte $DD ; |XX XXX X| $FD37
       .byte $00 ; |        | $FD38
       .byte $DD ; |XX XXX X| $FD39
       .byte $00 ; |        | $FD3A
       .byte $DD ; |XX XXX X| $FD3B
       .byte $00 ; |        | $FD3C
       .byte $DD ; |XX XXX X| $FD3D
       .byte $00 ; |        | $FD3E
       .byte $DD ; |XX XXX X| $FD3F
       .byte $00 ; |        | $FD40
       .byte $DD ; |XX XXX X| $FD41
       .byte $00 ; |        | $FD42
       .byte $D7 ; |XX X XXX| $FD43
       .byte $00 ; |        | $FD44
       .byte $D7 ; |XX X XXX| $FD45
       .byte $00 ; |        | $FD46
       .byte $D7 ; |XX X XXX| $FD47
       .byte $00 ; |        | $FD48
       .byte $D7 ; |XX X XXX| $FD49
       .byte $00 ; |        | $FD4A
       .byte $D7 ; |XX X XXX| $FD4B
       .byte $00 ; |        | $FD4C
       .byte $D7 ; |XX X XXX| $FD4D
       .byte $00 ; |        | $FD4E
       .byte $D7 ; |XX X XXX| $FD4F
       .byte $00 ; |        | $FD50
       .byte $D7 ; |XX X XXX| $FD51
       .byte $00 ; |        | $FD52
       .byte $DD ; |XX XXX X| $FD53
       .byte $00 ; |        | $FD54
       .byte $DD ; |XX XXX X| $FD55
       .byte $00 ; |        | $FD56
       .byte $DD ; |XX XXX X| $FD57
       .byte $00 ; |        | $FD58
       .byte $DD ; |XX XXX X| $FD59
       .byte $00 ; |        | $FD5A
       .byte $DD ; |XX XXX X| $FD5B
       .byte $00 ; |        | $FD5C
       .byte $DD ; |XX XXX X| $FD5D
       .byte $00 ; |        | $FD5E
       .byte $DD ; |XX XXX X| $FD5F
       .byte $00 ; |        | $FD60
       .byte $DD ; |XX XXX X| $FD61
       .byte $00 ; |        | $FD62
       .byte $DD ; |XX XXX X| $FD63
       .byte $00 ; |        | $FD64
       .byte $DD ; |XX XXX X| $FD65
       .byte $00 ; |        | $FD66
       .byte $DD ; |XX XXX X| $FD67
       .byte $00 ; |        | $FD68
       .byte $DD ; |XX XXX X| $FD69
       .byte $00 ; |        | $FD6A
       .byte $DD ; |XX XXX X| $FD6B
       .byte $00 ; |        | $FD6C
       .byte $DD ; |XX XXX X| $FD6D
       .byte $00 ; |        | $FD6E
       .byte $DD ; |XX XXX X| $FD6F
       .byte $00 ; |        | $FD70
       .byte $DD ; |XX XXX X| $FD71
       .byte $00 ; |        | $FD72
       .byte $DD ; |XX XXX X| $FD73
       .byte $00 ; |        | $FD74
       .byte $DD ; |XX XXX X| $FD75
       .byte $00 ; |        | $FD76
LFD77:
       .byte $67 ; | XX  XXX| $FD77
       .byte $61 ; | XX    X| $FD78
       .byte $61 ; | XX    X| $FD79
       .byte $61 ; | XX    X| $FD7A
       .byte $6F ; | XX XXXX| $FD7B
       .byte $6D ; | XX XX X| $FD7C
       .byte $6D ; | XX XX X| $FD7D
       .byte $6D ; | XX XX X| $FD7E
LFD7F:
       .byte $84 ; |X    X  | $FD7F
       .byte $84 ; |X    X  | $FD80
       .byte $84 ; |X    X  | $FD81
       .byte $84 ; |X    X  | $FD82
       .byte $88 ; |X   X   | $FD83
       .byte $8A ; |X   X X | $FD84
       .byte $8A ; |X   X X | $FD85
       .byte $8A ; |X   X X | $FD86


LFD87:
       LDX    #$02                    ;2
LFD89:
       TXA                            ;2
       ASL                            ;2
       ASL                            ;2
       TAY                            ;2
       LDA    #$FC                    ;2
       STA.wy $EB,Y                   ;5
       STA.wy $ED,Y                   ;5
       STY    $DF                     ;3
       LDA    $BD,X                   ;4
       AND    #$0F                    ;2
       TAY                            ;2
       LDA    LFC3C,Y                 ;4
       LDY    $DF                     ;3
       STA.wy $EC,Y                   ;5
       LDA    $BD,X                   ;4
       LSR                            ;2
       LSR                            ;2
       LSR                            ;2
       LSR                            ;2
       TAY                            ;2
       LDA    LFC3C,Y                 ;4
       LDY    $DF                     ;3
       STA.wy $EA,Y                   ;5
       DEX                            ;2
       BPL    LFD89                   ;2
       RTS                            ;6

LFDB7:
       AND    #$3F                    ;2
       BNE    LFDE6                   ;2
       LDA    #$03                    ;2
       TAX                            ;2
       INC    $C6                     ;5
       AND    $C6                     ;3
       STA    $C6                     ;3
       ASL                            ;2
       ADC    $C6                     ;3
       TAY                            ;2
LFDC8:
       LDA    $80,X                   ;4
       CMP    $80                     ;3
       LDA    LFDE7,Y                 ;4
       BCC    LFDD3                   ;2
       EOR    #$FF                    ;2
LFDD3:
       STA    $B8,X                   ;4
       LDA    $88,X                   ;4
       CMP    $88                     ;3
       LDA    LFDF3,Y                 ;4
       BCC    LFDE0                   ;2
       EOR    #$FF                    ;2
LFDE0:
       STA    $C0,X                   ;4
       INY                            ;2
       DEX                            ;2
       BNE    LFDC8                   ;2
LFDE6:
       RTS                            ;6

LFDE7:
       .byte $04 ; |     X  | $FDE7
       .byte $04 ; |     X  | $FDE8
       .byte $04 ; |     X  | $FDE9
       .byte $FC ; |XXXXXX  | $FDEA
       .byte $FC ; |XXXXXX  | $FDEB
       .byte $FC ; |XXXXXX  | $FDEC
       .byte $00 ; |        | $FDED
       .byte $04 ; |     X  | $FDEE
       .byte $02 ; |      X | $FDEF
       .byte $04 ; |     X  | $FDF0
       .byte $02 ; |      X | $FDF1
       .byte $02 ; |      X | $FDF2
LFDF3:
       .byte $02 ; |      X | $FDF3
       .byte $02 ; |      X | $FDF4
       .byte $02 ; |      X | $FDF5
       .byte $FE ; |XXXXXXX | $FDF6
       .byte $FE ; |XXXXXXX | $FDF7
       .byte $FE ; |XXXXXXX | $FDF8
       .byte $00 ; |        | $FDF9
       .byte $02 ; |      X | $FDFA
       .byte $02 ; |      X | $FDFB
       .byte $02 ; |      X | $FDFC
       .byte $02 ; |      X | $FDFD
       .byte $FE ; |XXXXXXX | $FDFE
       .byte $FF ; |XXXXXXXX| $FDFF


LFE00:
       LDA    $88                     ;3
       PHA                            ;3
       EOR    #$FF                    ;2
       SEC                            ;2
       ADC    #$77                    ;2
       STA    $F6                     ;3
       LDA    $8C                     ;3
       PHA                            ;3
       EOR    #$FF                    ;2
       SEC                            ;2
       ADC    #$77                    ;2
       STA    $F8                     ;3
       LDX    #$02                    ;2
LFE16:
       LDA    $95,X                   ;4
       AND    #$0F                    ;2
       TAY                            ;2
       LDA    LF32B,Y                 ;4
       STA    $E3,X                   ;4
       LDA    $91,X                   ;4
       AND    #$0F                    ;2
       TAY                            ;2
       LDA    LF322,Y                 ;4
       STA    $DF,X                   ;4
       DEX                            ;2
       BPL    LFE16                   ;2
       STX    $88                     ;3
       STX    $8C                     ;3
       STA    WSYNC                   ;3
       INX                            ;2
       STX    PF0                     ;3
       STX    PF1                     ;3
       STX    PF2                     ;3
       LDA    #SCOREBACKGROUND         ;2 <-green background
       STA    COLUBK                  ;3
       LDA    #$05                    ;2
       STA    CTRLPF                  ;3
       STA    VDELP0                  ;3
       STA    VDELP1                  ;3
       LDA    #$00                    ;2
       STA    HMP0                    ;3
       LDA    #$10                    ;2
       STA    HMP1                    ;3
       STA    RESP0                   ;3
       STA    RESP1                   ;3
       LDA    #SCORECOLOR             ;2 <-yellow/green players
       STA    COLUP0                  ;3
       STA    COLUP1                  ;3
       LDA    #WALLCOLOR              ;2 <-top scanline of side walls color
       STA    $E8                     ;3
       STA    $E7                     ;3
       LDA    #$00                    ;2
       STA    WSYNC                   ;3
       STA    HMOVE                   ;3
       STA    VBLANK                  ;3
       STA    GRP0                    ;3
       STA    GRP1                    ;3
       STA    GRP0                    ;3
       STA    ENAM0                   ;3
       STA    REFP0                   ;3
       STA    REFP1                   ;3
       LDY    #$05                    ;2
       LDA    #$03                    ;2
       STA    NUSIZ0                  ;3
       STA    NUSIZ1                  ;3
       STA    HMCLR                   ;3
LFE7C:
       LDA    ($F4),Y                 ;5
       TAX                            ;2
       LDA    ($EA),Y                 ;5
       STA    WSYNC                   ;3
       STA    HMOVE                   ;3
       STY    $E2                     ;3
       STA    GRP0                    ;3
       LDA    ($EC),Y                 ;5
       STA    GRP1                    ;3
       LDA    ($EE),Y                 ;5
       STA    GRP0                    ;3
       LDA    ($F0),Y                 ;5
       STA    $E6                     ;3
       LDA    ($F2),Y                 ;5
       LDY    $E6                     ;3
       STY    GRP1                    ;3
       STA    GRP0                    ;3
       STX    GRP1                    ;3
       STX    GRP0                    ;3
       LDY    $E2                     ;3
       DEY                            ;2
       BPL    LFE7C                   ;2
       INY                            ;2
       STY    VDELP0                  ;3
       STY    VDELP1                  ;3
       LDA    #$F2                    ;2
       STA    WSYNC                   ;3
       STA    HMOVE                   ;3
       STA    $ED                     ;3
       STY    GRP0                    ;3
       STY    GRP1                    ;3
       LDA    $A8                     ;3
       STA    $EE                     ;3
       LDA    $AC                     ;3
       STA    $F0                     ;3
       LDA    $AB                     ;3
       STA    $F2                     ;3
       LDA    #$07                    ;2
       STA    $E6                     ;3
       LDA    #$03                    ;2
       STA    $E2                     ;3
       LDA    #$FC                    ;2
       STA    $F7                     ;3
       STA    $F9                     ;3
       LDA    $B8                     ;3
       STA    COLUP0                  ;3
       LDA    $B4                     ;3
       STA    NUSIZ0                  ;3
       LDA    $B3                     ;3
       STA    REFP1                   ;3
       LDA    $94                     ;3
       LDY    #$02                    ;2
       STA    WSYNC                   ;3
       STA.w  HMOVE                   ;4
       BNE    LFEF1                   ;2 always branch

;;LFEE8:
;;       .byte $10 ; |   X    | $FEE8
;;       .byte $12 ; |   X  X | $FEE9

LFEEA:
       STA    WSYNC                   ;3
       STA    HMOVE                   ;3
       LDA.wy $8F,Y                   ;4
LFEF1:
       LDX    LFEE8-1,Y               ;4
       AND    #$0F                    ;2
       SEC                            ;2
LFEF7:
       SBC    #$01                    ;2
       BPL    LFEF7                   ;2
       DEY                            ;2
       STA    VSYNC,X                 ;4
       BNE    LFEEA                   ;2

       STA    WSYNC                   ;3
       STA    HMOVE                   ;3
       LDA    #SCREENBACKGROUND       ;2 <-grey background
       STA    COLUBK                  ;3

;indirect pointers
       LDA    #$B2                    ;2
       STA    $EA                     ;3
       LDA    #$F2                    ;2
       STA    $EB                     ;3
       LDA    #$00                    ;2
       STA    $EC                     ;3
       LDA    #$F0                    ;2
       STA    $EF                     ;3
       STA    $F1                     ;3
       LDA    #$FB                    ;2
       STA    $F3                     ;3
       STA    $F5                     ;3
       LDA    $B0                     ;3
       STA    REFP0                   ;3
       STA    CXCLR                   ;3
       LDA    $90                     ;3
       STA    HMP0                    ;3
       LDA    $94                     ;3
       STA    HMM0                    ;3
       LDX    #$00                    ;2
       JSR    LFF57                   ;6
       STY    PF1                     ;3
       STA    HMOVE                   ;3
       STY    PF2                     ;3
       LDA    $BC                     ;3
       STA.w  COLUP1                  ;4
       JMP    LF2B2                   ;3
LFF41:
       LDX    #$08                    ;2
       LDA    #$00                    ;2
       STA    GRP0                    ;3
       STA    GRP1                    ;3
       JSR    LFF57                   ;6
       LDA    #$02                    ;2
       STA    VBLANK                  ;3
       PLA                            ;4
       STA    $8C                     ;3
       PLA                            ;4
       STA    $88                     ;3
       RTS                            ;6

LFF57:
       LDY    #$10                    ;2
LFF59:
       STA    WSYNC                   ;3
       STA    HMOVE                   ;3
       NOP                            ;2
       LDA    #WALLCOLOR              ;2 <- Indigo playfield (top and bottom walls)
       STA    COLUPF                  ;3

       LDA    #$C0                    ;2
       STA    PF0                     ;3
       LDA    #$FC                    ;2
       STA    PF1                     ;3
       LDA    #$E7                    ;2
       STA    PF2                     ;3

;screen data:
;|XX    XXXXXXXXX XXXXXX XXXXXXXXX    XX|


       LDA    $D1,X                   ;4
       STA    COLUBK                  ;3
       LDA    $D2,X                   ;4
       STA    COLUBK                  ;3
       LDA    $D3,X                   ;4
       STA    COLUBK                  ;3
       STA    HMCLR                   ;3
       NOP                            ;2
       LDA    $D4,X                   ;4
       STA    COLUBK                  ;3
       LDA    #SCREENBACKGROUND       ;2 ;grey background
       STA    COLUBK                  ;3
       DEY                            ;2
       BNE    LFF59                   ;2
       RTS                            ;6

LFF89:
       LDX    #$03                    ;2
       LDY    #$02                    ;2
       STY    $E1                     ;3
LFF8F:
       LDA.wy $88,Y                   ;4
       CMP    #$7C                    ;2
       BEQ    LFFCA                   ;2
       SEC                            ;2
       SBC    $88,X                   ;4
       CMP    #$20                    ;2
       BCS    LFFC0                   ;2
       LDA    $A0,X                   ;4
       BMI    LFFAA                   ;2
       EOR    #$FF                    ;2
       CLC                            ;2
       ADC    #$01                    ;2
       STA    $A0,X                   ;4
       DEC    $DF,X                   ;6
LFFAA:
       LDA.wy $A0,Y                   ;4
       BPL    LFFC0                   ;2
       EOR    #$FF                    ;2
       CLC                            ;2
       ADC    #$01                    ;2
       STA.wy $A0,Y                   ;5
       LDA.wy $DF,Y                   ;4
       SEC                            ;2
       SBC    #$01                    ;2
       STA.wy $DF,Y                   ;5
LFFC0:
       DEX                            ;2
       DEY                            ;2
       BNE    LFF8F                   ;2
       LDA    $E1                     ;3
       BNE    LFFCA                   ;2
       STA    $A2                     ;3
LFFCA:
       RTS                            ;6

LFFCB:
       CLC                            ;2
       ADC    $80,X                   ;4
LFFCE:
       STA    $80,X                   ;4
       TXA                            ;2
       AND    #$03                    ;2
       PHP                            ;3
       LDA    $80,X                   ;4
       PLP                            ;4
       BEQ    LFFDC                   ;2
       SEC                            ;2
       SBC    #$06                    ;2
LFFDC:
       STA    $E0                     ;3
       LSR                            ;2
       LSR                            ;2
       LSR                            ;2
       LSR                            ;2
       STA    $E1                     ;3
       LDA    $E0                     ;3
       ASL                            ;2
       ASL                            ;2
       ASL                            ;2
       ASL                            ;2
       ORA    $E1                     ;3
       CLC                            ;2
       ADC    $E0                     ;3
       AND    #$F0                    ;2
       ADC    $E1                     ;3
       EOR    #$70                    ;2
       STA    $90,X                   ;4
       RTS                            ;6


LFEE8:
       .byte $10 ; |   X    | $FEE8
       .byte $12 ; |   X  X | $FEE9

       ORG $FFF8
       .byte "2008"
       .word START,0
