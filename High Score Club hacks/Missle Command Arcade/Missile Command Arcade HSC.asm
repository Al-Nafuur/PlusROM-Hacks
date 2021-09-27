;Hack: Missile Command Arcade...by Kurt (Nukey Shay) Howe, 7/25/2006

TF = 1 ;enable Track and Field controller (use right fire button for center silo) cost=3 bytes
;regardless of TF, missiles fired by use of right controller left/down/right

PLUSROM                 = 1

; Disassembly of MissComm.bin
; Disassembled Tue Apr 26 22:21:45 2005
; Using DiStella v2.0
; Command Line: C:\BIN\DISTELLA.EXE -pafscMissComm.cfg MissComm.bin 
; MissComm.cfg contents:
;      CODE F000 F596
;      GFX F597 F5A2
;      CODE F5A3 F689
;      GFX F68A F68E
;      CODE F68F FCAD
;      GFX FCAE FCAE
;      CODE FCAF FDDE
;      GFX FDDF FFFF

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
SWCHA  = $0280 ;Port A data register for joysticks (High nybble:player0,low nybble:player1)
SWACNT = $0281 ;Port A data direction register (DDR)
SWCHB  = $0282 ;Port B data (console switches) bit pattern LR--B-SR
SWBCNT = $0283 ;Port B data direction register (DDR)
INTIM  = $0284 ;Timer output
TIMINT = $0285 ;
WasteA = $0286 ;Unused/undefined
WasteB = $0287 ;Unused/undefined
WasteC = $0288 ;Unused/undefined
WasteD = $0289 ;Unused/undefined
WasteE = $028A ;Unused/undefined
WasteF = $028B ;Unused/undefined
WasteG = $028C ;Unused/undefined
WasteH = $028D ;Unused/undefined
WasteI = $028E ;Unused/undefined
WasteJ = $028F ;Unused/undefined
WasteK = $0290 ;Unused/undefined
WasteL = $0291 ;Unused/undefined
WasteM = $0292 ;Unused/undefined
WasteN = $0293 ;Unused/undefined
TIM1T  = $0294 ;set 1 clock interval
TIM8T  = $0295 ;set 8 clock interval
TIM64T = $0296 ;set 64 clock interval
T1024T = $0297 ;set 1024 clock interval

; PlusROM hotspots and gameId for HSC backend
   IF PLUSROM
WriteToBuffer           = $1FF0
WriteSendBuffer         = $1FF1
ReceiveBuffer           = $1FF2
ReceiveBufferSize       = $1FF3

HIGHSCORE_ID            = 45        ;Missile Command Arcade game ID in Highscore DB
   ENDIF

;ram assignments
FrameCounter = $80

LineCount = $81

rE6 = $82
Marquee = $82 ;all above used to display "Missile Command" (reset upon exit...rE6 held in S.P.)

r83 = $83
r84 = $84
r85 = $85
r86 = $86
r87 = $87
r88 = $88
r89 = $89
r8A = $8A

CacheCount = $8B
;r8C = $8C
;r8D = $8D

r81 = $8E
rFB = $8F

rEF = $90
;rF0 = $91

rF1 = $92
;rF2 = $93

rF3 = $94
;rF4 = $95

rF5 = $96
rE9 = $97


rDB = $98
rDC = $99
rDD = $9A
rDE = $9B
rDF = $9C
rE0 = $9D
rE1 = $9E

DifSw = $9F

CityPtr = $A0 ;(A9-AE)
;rAA = $A1
;rAB = $A2
;rAC = $A3
;rAD = $A4
;rAE = $A5

rCA = $A6
Hz20 = rCA


r9C = $A7
TheEnd = r9C ;(70 bytes shared) all above used to display "The End"
;r9D = $A8

r9E = $A9
;r9F = $AA
;rA0 = $AB

rA1 = $AC
rA2 = $AD

rA3 = $AE
;rA4 = $AF
;rA5 = $B0

rA6 = $B1
;rA7 = $B2
;rA8 = $B3

r8E = $B4
r8F = $B5
r90 = $B6
r91 = $B7
r92 = $B8
r93 = $B9
r94 = $BA
r95 = $BB
r96 = $BC
r97 = $BD
r98 = $BE
r99 = $BF
r9A = $C0
r9B = $C1

SatX = $C2
CacheTotal = SatX
SatY = $C3
CountTemp = SatY

rB5 = $C4
;rB6 = $C5
;rB7 = $C6

rB8 = $C7
;rB9 = $C8
;rBA = $C9

rBB = $CA
;rBC = $CB
;rBD = $CC

rBE = $CD
;rBF = $CE
;rC0 = $CF

rC1 = $D0
;rC2 = $D1
;rC3 = $D2

rC4 = $D3
;rC5 = $D4
;rC6 = $D5

rC7 = $D6
;rC8 = $D7
;rC9 = $D8

SatDir = $D9 ;bit6=direction,Bit7=(non-moving?). Counts upward to 0 when non-moving (animation)
CityCount = SatDir

rF6 = $DA
rF7 = $DB
rF8 = $DC

rF9 = $DD
;rFA = $DE

rD7 = $DF
rD8 = $E0
rD9 = $E1


ShotOrigin = $E2 ;(e1-e3)
;rE3 = $E3
;rE4 = $E4

;$E5-$ED free ram

rE7 = $EE
rE8 = $EF
rEA = $F0
rEB = $F1
rEC = $F2
rED = $F3

PrintTemp = $F4 ;(F4-FF)
;rCC = $F5
;rCD = $F6
;rCE = $F7
;rCF = $F8
;rD0 = $F9
;rD1 = $FA
;rD2 = $FB
;rD3 = $FC
;rD4 = $FD
;rD5 = $FE
;rD6 = $FF
;NOTE: Stack shares last 4 bytes. Do not use stack between score and city printing!


Temp1 = PrintTemp+2

Temp2 = PrintTemp+3
Crosshair = Temp2


;Ram change history:
;changed $8B,$8C to $D5,$D6...used free ram to hold additional cache counts
;removed color register ram ($E2 to $E5)...used $E2-$E4 to hold missile origin
;moved printing temps to the end of ram (simple trick to reclaim 4 bytes ram used by the stack)
;counting/gfx temps removed from score routine...then shared with printing temps
;reorganized ram to group free bytes...then moved variables that must remain untouched by PF
;9 bytes of free ram remain!

       ORG  $1000
       RORG $D000

;Y=2, A=0
Display:
       sta    $1FF9                   ;3
LF056:
       sty    VBLANK                  ;3
       sty    VSYNC                   ;3
       sty    WSYNC                   ;3
       ldy    #$30                    ;2
       sty    WSYNC                   ;3
       sta    VSYNC                   ;3
       sty    TIM64T                  ;4
       tay                            ;2 A/Y=0
       lda    SWCHB                   ;4
       tax                            ;2  copy to X (to be used later)
       eor    DifSw                   ;2  check previous switches
       beq    Switch_ending           ;2³ branch if the same (branch to end)
       bit    rE6                     ;3  first, load in the console type
       bvs    AA7800                  ;2³ Branch if so
       txa                            ;2
       and    #$08                    ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       sta    DifSw                   ;3
       lda    rE6                     ;3
       and    #$FE                    ;2
       ora    DifSw                   ;3
       jmp    NoPauseChange0          ;3

AA7800:
       and    #$08                    ;2
       beq    NoPauseChange           ;2³ branch if no change
       txa                            ;2  get back the full array for checking other switches
       and    #$08                    ;2  keep only Color/B&W
       bne    NoPauseChange           ;2³ Branch if on color selection (skip)
PauseChange:
       lda    rE6                     ;3  load a ram location used to hold
       eor    #$01                    ;2  the color mode, and flip it's status
NoPauseChange0:
       sta    rE6                     ;3
NoPauseChange:
       stx    DifSw                   ;3  ...and save the new ones
Switch_ending:
;pause routine
       lda    rE6                     ;3
       asl                            ;2
       rol    INPT4                   ;3
       ror                            ;2
       bpl    Save                    ;2³ skip if not changed (+current status in A)
       bit    rE6                     ;3
       bmi    Skip                    ;2³ skip if still pressed
       eor    #$20                    ;2 flip modes
Skip:
Save:
       sta    rE6                     ;3
       and    #$20                    ;2
       bne    NotPaused               ;2³
       lda    FrameCounter            ;3
       eor    #$01                    ;2
       sta    FrameCounter            ;3
       jmp    Pause                   ;3

NotPaused:
       lda    SatDir                  ;3
       bpl    SkipSat                 ;2³
       inc    SatDir                  ;5
       bne    SkipSat                 ;2³
       ldx    #$FF                    ;2
       stx    SatY                    ;3
       inx                            ;2
       stx    SatDir                  ;3
SkipSat:
       inc    FrameCounter            ;5
       bne    LF07F                   ;2³
       inc    rE8                     ;5
       lda    rDB                     ;3
       bpl    LF07F                   ;2³
;buggy...flip players
;       lda    rF8                     ;3
;       bpl    LF07F                   ;2³
;       nop                            ;2
;       nop                            ;2
       lda    rDB                     ;3
       and    #$04                    ;2
       bne    LF07F                   ;2³
       lda    rF5                     ;3
       eor    #$01                    ;2
       sta    rF5                     ;3
LF07F:
       lda    rDC                     ;3
       beq    LF08A                   ;2³
       dec    rDC                     ;5
       bne    LF08A                   ;2³
LF08A:
       bit    rDB                     ;3
       bpl    LF092                   ;2³ branch if missiles dropping
;clear silos
       lda    #$0B                    ;2 blank gfx
       sta    CacheCount              ;3
       sta    CacheCount+1            ;3
       sta    CacheCount+2            ;3
       dey                            ;2 Y=FF
       sty    rDE                     ;3 set mode to inactive
LF092:
       lda    rDE                     ;3
       cmp    #$E8                    ;2
       bne    LF0C5                   ;2³
       lda    rE0                     ;3
       bne    LF0B8                   ;2³
       lda    #$07                    ;2
       sta    rE0                     ;3 tic timer
;bugfix (works!??!)
       lda    CacheCount              ;3
       ora    CacheCount+1            ;3
       ora    CacheCount+2            ;3
;subtract missiles
       ldx    #$03                    ;2
LFCCX:
       lda    CacheCount-1,X          ;4
       beq    LFCCY                   ;2³
       dec    CacheCount-1,X          ;6

       inc    CacheTotal              ;5
       lda    #$04                    ;2
       sta    rDF                     ;3
       lda    #$05                    ;2
       ldy    #$00                    ;2
       jsr    LFCCB                   ;6
LF0B8:
       jmp    LF132                   ;3

LFCCY:
       dex                            ;2
       bne    LFCCX                   ;2³
       stx    rDF                     ;3
       stx    rE0                     ;3
       lda    #$E0                    ;2
       sta    rDE                     ;3
       bne    LF0B8                   ;2³ always branch

LF0C5:
       lda    rDE                     ;3
       cmp    #$E0                    ;2
       bne    LF10C                   ;2³
       lda    FrameCounter            ;3
       and    #$0F                    ;2
       bne    LF132                   ;2³
       ldx    rE0                     ;3
LF0D3:
       cpx    #$06                    ;2
       beq    LF0FA                   ;2³
       lda    CityPtr,X               ;4 load city pointer
       beq    LF0E4                   ;2³ branch if a city
       inx                            ;2
       bne    LF0D3                   ;2³
LF0E4:
       lda    #<Blank                 ;2 load blank space
       sta    CityPtr,X               ;4 save to city pointer
       inc    CityCount               ;5 bump counter for display
       inx                            ;2
       stx    rE0                     ;3
       lda    #$00                    ;2
       ldy    #$01                    ;2
       jsr    LFCCB                   ;6
       lda    #$05                    ;2
       sta    rDF                     ;3
       bne    LF132                   ;2³ always branch

LF0FA:
       lda    #$A8                    ;2
       sta    rDE                     ;3
       lda    #$04                    ;2
       sta    rE0                     ;3
       lda    #$01                    ;2
       sta    rDF                     ;3
       jsr    LFD96                   ;6
       jmp    LF4D6                   ;3

LF10C:
       lda    rDF                     ;3
       cmp    #$06                    ;2
       beq    LF132                   ;2³
       bit    rDE                     ;3
       bpl    LF132                   ;2³
       lda    FrameCounter            ;3
       and    #$03                    ;2
       bne    LF132                   ;2³
       dec    rDE                     ;5
       bmi    LF123                   ;2³
       jmp    LF1D8                   ;3

LF123:
       lda    rDE                     ;3
       cmp    #$A0                    ;2
       bne    LF132                   ;2³
       lda    #$0A                    ;2 reset 10 missiles to each cache
       sta    CacheCount              ;3
       sta    CacheCount+1            ;3
       sta    CacheCount+2            ;3
       jsr    LFD16                   ;6
AllMissilesCounted:
LF132:
       ldy    #$00                    ;2
       bit    rDB                     ;3
       bvc    LF147                   ;2³
       lda    DifSw                   ;3
       and    #$03                    ;2
       beq    LF140                   ;2³
       lsr                            ;2
       bcc    LF143                   ;2³
LF1D5:
LF140:
       jmp    LF210                   ;3

;New game...
LF143:
       sty    rDB                     ;3
LF147:
       lda    #$FF                    ;2
       ldx    #$0A                    ;2
LF14D:
       sta    PrintTemp+1,X           ;4
       dex                            ;2
       dex                            ;2
       bpl    LF14D                   ;2³
       ldx    #$05                    ;2
LF15A:
       sty    CityPtr,X               ;4 reset cities
       dex                            ;2
       bpl    LF15A                   ;2³
       sty    rEF                     ;3 reset p1 score
       sty    rEF+2                   ;3 reset p1 score
       sty    rEF+4                   ;3 reset p1 score
       sty    rED                     ;3
       sty    rF5                     ;3
       sty    rFB                     ;3
       sty    rF6                     ;3
       sty    rF7                     ;3
       ldy    #$50                    ;2 reset crosshair to starting position
       sty    r90                     ;3 X
       ldy    #$32                    ;2 reset crosshair to starting position
       sty    r8E                     ;3 Y
       ldy    rE9                     ;3
       lda    LFF82,Y                 ;4
       ldx    #$3F                    ;2 reset all cities
       stx    rEC                     ;3
       cpy    #$18                    ;2
       bcc    LF188                   ;2³
       stx    rED                     ;3 reset all cities (p2)
       ldx    #$00                    ;2
       stx    rEF+1                   ;3 reset p2 score
       stx    rEF+3                   ;3 reset p2 score
       stx    rEF+5                   ;3 reset p2 score
       tya                            ;2
       sed                            ;2
       sec                            ;2
       sbc    #$17                    ;2
       tay                            ;2
       cld                            ;2
       lda    LFF82,Y                 ;4
       ora    #$80                    ;2
       ldx    #$01                    ;2
       stx    rF5                     ;3

LF188:
       sta    rF8                     ;3
       cpy    #$17                    ;2
       bne    LF192                   ;2³
       ldy    #$40                    ;2
       sty    rFB                     ;3
LF192:
       and    #$0E                    ;2
       tax                            ;2
       dex                            ;2
       stx    rE7                     ;3
       lda    #$54                    ;2
       sta    r93                     ;3
       sta    r94                     ;3
       lda    rDB                     ;3
       ora    #$40                    ;2
       sta    rDB                     ;3
       bmi    LF1D5                   ;2³
       ldx    #$01                    ;2
       stx    rDF                     ;3
       ldx    #$04                    ;2
       stx    rE0                     ;3
       lda    #$13                    ;2
       bit    rFB                     ;3
       bvc    LF1BE                   ;2³
       lda    #$FB                    ;2
LF1BE:
       ldy    rE7                     ;3
       iny                            ;2
LF1C1:
       clc                            ;2
       adc    #$0D                    ;2
       dey                            ;2
       bpl    LF1C1                   ;2³
       sta    r9B                     ;3
       lda    #$B0                    ;2
       sta    rDE                     ;3
       lda    FrameCounter            ;3
       ora    #$02                    ;2
       sta    rD8                     ;3
       sta    rD9                     ;3
       bne    LF210                   ;2³ always branch


LF1D8:
       ldy    #$0F                    ;2
       lda    rE7                     ;3
       cmp    #$10                    ;2
       bcs    LF1E1                   ;2³
       tay                            ;2
LF1E1:
       lda    LFF6C,Y                 ;4
       bit    rFB                     ;3
       bvc    LF1E9                   ;2³
       lsr                            ;2
LF1E9:
       sta    rDE                     ;3
       lda    LFF2A,Y                 ;4
       sta    rEB                     ;3
       ldy    #$01                    ;2
       ldx    #$02                    ;2
LF1FA:
       sty    rC7,X                   ;4
       sty    rB5,X                   ;4
       dec    rB5,X                   ;6
       dex                            ;2
       bpl    LF1FA                   ;2³
       stx    r91                     ;3
       stx    r92                     ;3
       inx                            ;2
       stx    rDF                     ;3
       lda    #$40                    ;2
       sta    rDB                     ;3
LF210:
       ldy    #$1F                    ;2
       lda    DifSw                   ;3
       and    #$03                    ;2
       bne    LF21B                   ;2³
       ldy    #$07                    ;2
LF21B:
       sty    Temp2                   ;3
       lda    DifSw                   ;3
       lsr                            ;2
       lsr                            ;2
       lda    #$FF                    ;2
       bcc    LF22A                   ;2³
       sta    rEA                     ;3
       bne    LF257                   ;2³
LF22A:
       sta    rDB                     ;3
       lda    rEA                     ;3
       bmi    LF236                   ;2³
       eor    FrameCounter            ;3
       and    Temp2                   ;3
       bne    LF257                   ;2³
LF236:
       lda    FrameCounter            ;3
       and    Temp2                   ;3
       sta    rEA                     ;3
       sed                            ;2
       clc                            ;2
       lda    rE9                     ;3
       adc    #$01                    ;2
       cmp    #$35                    ;2
       bne    LF248                   ;2³
       lda    #$01                    ;2
LF248:
       sta    rE9                     ;3
       cld                            ;2
       lda    #$00                    ;2
;select used...
       sta    rEF+1                   ;3 reset p2 score
       sta    rEF+3                   ;3 reset p2 score
       sta    rEF+5                   ;3 reset p2 score

       sta    rDF                     ;3
       sta    rE8                     ;3
       sta    rE7                     ;3
LF257:
       bit    rFB                     ;3
       bpl    LF25E                   ;2³
       jmp    LF4D6                   ;3

LF25E:
       asl    rD8                     ;5
       rol    rD9                     ;5
       bpl    LFC9A                   ;2³
       inc    rD8                     ;5
LFC9A:
       lda    rD8                     ;3
       eor    #$01                    ;2
       sta    rD8                     ;3
       ora    rD9                     ;3
       bne    LFCAB                   ;2³
       inc    rD8                     ;5
LFCAB:
       lda    rD8                     ;3
  IF TF
;added right fire button to the mix
       lda    SWCHA                   ;4
       lsr                            ;2
       rol    INPT5                   ;5
       rol                            ;2
       and    #$0F                    ;2
       eor    #$0F                    ;2
       bne    LF270                   ;2³
  ELSE
;edited to use stick2 instead of trigger
       lda    SWCHA                   ;4
       and    #$0F                    ;2
       eor    #$0F                    ;2
       bne    LF270                   ;2³
  ENDIF
;clear button press flag
       lda    rA2                     ;3
       and    #$F7                    ;2
       sta    rA2                     ;3
LF2EDJ:
       jmp    LF2ED                   ;3
;check button press flag
LF270:
       ldy    #$02                    ;2
;do twice...both down & fire = center silo
       lsr                            ;2 SWCHA bits 1-3 (reversed) still in A
       bcs    LF2AA                   ;2³
       lsr                            ;2
       bcs    LF2AA                   ;2³
       dey                            ;2
       lsr                            ;2
       bcs    LF2AA                   ;2³
       dey                            ;2
LF2AA:
       lda    rA2                     ;3
       and    #$08                    ;2
       bne    LF2EDJ                  ;2³ quit if already pressed before
;check for free slots
       ldx    #$02                    ;2
LF278:
       lda    rB5,X                   ;4
       bpl    LF28F                   ;2³ slot free, branch ahead
       dex                            ;2
       bpl    LF278                   ;2³
;no slots free
LF27F:
       lda    rDF                     ;3
       and    #$0F                    ;2
       bne    LF2ED                   ;2³ <- watch out for out of range errors
       sta    rE0                     ;3
       lda    rDF                     ;3
       ora    #$02                    ;2
       sta    rDF                     ;3
       bne    LF2ED                   ;2³ <- " " always branch

;slots free
LF28F:
       lda    #$08                    ;2
       ora    rA2                     ;3
       sta    rA2                     ;3
       lda    rDE                     ;3
       bmi    LF2ED                   ;2³ <- " "
       lda.wy CacheCount,Y            ;4
       beq    LF27F                   ;2³
       sec                            ;2
       sbc    #$01                    ;2
       sta.wy CacheCount,Y            ;6
;save missile origin
       lda    StartTbl,Y              ;4
       sta    ShotOrigin,X            ;4 save start location (used for compares)
       sta    rC4,X                   ;4 save current missile X location
LF2A4:
       lda    rDF                     ;3
       cmp    #$20                    ;2
       bcs    LF2B4                   ;2³
       and    #$0F                    ;2
       ora    #$10                    ;2
       sta    rDF                     ;3
       lda    #$0A                    ;2
       sta    rE1                     ;3
LF2B4:
       lda    ShotOrigin,X            ;4 new missile origin
       sec                            ;2
       sbc    r90                     ;3
       bcs    LF2C1                   ;2³
       inc    rC4,X                   ;6
       eor    #$FF                    ;2
       adc    #$01                    ;2
LF2C1:
       sta    Temp1                   ;3
       lda    r8E                     ;3
       sec                            ;2
       sbc    #$01                    ;2
       cmp    Temp1                   ;3
       bcc    LF2D5                   ;2³
       sta    r83                     ;3
       lda    #$C0                    ;2 starting Y
       bne    LF2DD                   ;3 always branch

LF2D5:
       ldy    Temp1                   ;3
       sta    Temp1                   ;3
       sty    r83                     ;3
       lda    #$80                    ;2
LF2DD:
       sta    rB5,X                   ;4
LF2DF:
       lda    r83                     ;3
       sta    rB8,X                   ;4
       sta    rBB,X                   ;4
       lda    Temp1                   ;3
       sta    rBE,X                   ;4
       lda    #$00                    ;2
       sta    rC1,X                   ;4
LF2ED:
       lda    DifSw                   ;3
       asl                            ;2
       ldx    rF5                     ;3
       bne    LF2F6                   ;2³
       asl                            ;2
LF2F6:
       ldy    #$02                    ;2
       bcc    LF2FB                   ;2³
       dey                            ;2
LF2FB:
       sty    Temp2                   ;3
       ldx    #$02                    ;2 3 active player missiles (max)
MoveMissileLoop:
       lda    rB5,X                   ;4
       bpl    NextMissile             ;2³
       and    #$20                    ;2
       bne    NextMissile             ;2³
       lda    rC1,X                   ;4
       clc                            ;2
       adc    rBE,X                   ;4
       sta    rC1,X                   ;4
       cmp    rBB,X                   ;4
       bcc    LF340                   ;2³
       sbc    rBB,X                   ;4
       sta    rC1,X                   ;4
       lda    rB5,X                   ;4
       and    #$40                    ;2
       bne    LF329                   ;2³
       jsr    AddDA                   ;6
       jmp    LF340                   ;3

;add/subtract missile trajectory
LF329:
       lda    ShotOrigin,X            ;4 new missile origin
       cmp    rC4,X                   ;4 check against current missile X
       bcs    LF339                   ;2³
       jsr    AddDAA                  ;6
       jmp    LF340                   ;3

LF339:
       jsr    SubDA                   ;6
LF340:
       lda    rB5,X                   ;4
       and    #$40                    ;2
       beq    LF350                   ;2³
       jsr    AddDA                   ;6
       jmp    LF367                   ;3

LF350:
       lda    ShotOrigin,X            ;4 new missile origin
       cmp    rC4,X                   ;4
       bcs    LF360                   ;2³
       jsr    AddDAA                  ;6
       jmp    LF367                   ;3

LF360:
       jsr    SubDA                   ;6
LF367:
       lda    rB8,X                   ;4
       sec                            ;2
       sbc    Temp2                   ;3
       sta    rB8,X                   ;4
       bcc    LF372                   ;2³
       bne    NextMissile             ;2³
LF372:
       lda    #$20                    ;2
       ora    rB5,X                   ;4
       sta    rB5,X                   ;4
       ldy    #$00                    ;2
       sty    r9E,X                   ;4
       lda    rC4,X                   ;4
       sec                            ;2
       sbc    #$04                    ;2
       sta    rA6,X                   ;4
       lda    rC7,X                   ;4
       sbc    #$04                    ;2
       sta    rA3,X                   ;4
NextMissile:
       dex                            ;2
       bpl    MoveMissileLoop         ;2³
       ldx    rCA                     ;3
       ldy    #$54                    ;2
       lda    rC4,X                   ;4 ??
       sta    r89                     ;3 ?? fired missile destination X
       lda    rB5,X                   ;4
       and    #$20                    ;2
       bne    LF39F                   ;2³
       ldy    rC7,X                   ;4
LF39F:
       sty    r84                     ;3
;       lda    rDE                     ;3
;       bpl    LF3AC                   ;2³
;       cmp    #$A0                    ;2
;       bcc    LF3AC                   ;2³
;       jmp    LF431                   ;3
;LF3AC:
       lda    rDE                     ;3
       bpl    LF3AC                   ;2³
       lda    #$FF                    ;2
       sta    SatY                    ;3
       jmp    Get_Input               ;3
LF3AC:
;deal w/satellite
       lda    SatY                    ;3
       bpl    Move_Object             ;2³
;no satellite...do rnd here
       lda    FrameCounter            ;3
       bne    Get_Input               ;2³
;"rnd"
       lda    rF7                     ;3
       lsr                            ;2
       eor    rC4                     ;3
       asl                            ;2
       eor    rC7                     ;3
       tay                            ;2
       ora    #$08                    ;2
       and    #$3F                    ;2
       sta    SatY                    ;3
       tya                            ;2
       ldy    #$96                    ;2
       and    #$40                    ;2
       sta    SatDir                  ;3
       bne    Moving_Left             ;2³
       ldy    #$01                    ;2
Moving_Left:
       sty    SatX                    ;3
       jmp    Get_Input               ;3 temp

Move_Object:
       ldy    #$00                    ;2
       lda    SatDir                  ;3
       bmi    Non_Moving              ;2³
       dey                            ;2
       and    #$40                    ;2
       bne    Move_Left               ;2³
       ldy    #$01                    ;2
Move_Left:
Non_Moving:
       lda    FrameCounter            ;3
       lsr                            ;2
       bcs    Get_Input               ;2³ skip moving every other frame
       tya                            ;2
       clc                            ;2
       adc    SatX                    ;3
       sta    SatX                    ;3
       beq    Kill_Object             ;2³
       cmp    #$98                    ;2
       bcc    Get_Input               ;2³ object position OK
Kill_Object:
       lda    #$FF                    ;2
       sta    SatY                    ;3

Get_Input:
       lda    rDE                     ;3
       bpl    LF3AD                   ;2³
       cmp    #$E0                    ;2
       bne    LF3AD                   ;2³
       jmp    LF431                   ;3
LF3AD:

       lda    SWCHA                   ;4
       ldy    rF5                     ;3
;jstick...these lines needed
       beq    LF3B8                   ;2³ branch if player 1
       dey                            ;2 player 2...adjust Y
LF3B8:
       sta    Temp2                   ;3
       bit    rFB                     ;3
       bvc    LF3C2                   ;2³
       ldy    #$04                    ;2
       bne    LF3C9                   ;2³
LF3C2:
       lda    rF8                     ;3
       lsr                            ;2
       bcc    LF3C9                   ;2³
       ldy    #$02                    ;2
LF3C9:
       lda    r8F                     ;3
       rol    Temp2                   ;5
       bcs    LF3DB                   ;2³
       adc    LFF99,Y                 ;4
       sta    r8F                     ;3
       lda    r90                     ;3
       adc    LFF9A,Y                 ;4
       sta    r90                     ;3
LF3DB:
       rol    Temp2                   ;5
       bcs    LF3EE                   ;2³
       sec                            ;2
       lda    r8F                     ;3
       sbc    LFF99,Y                 ;4
       sta    r8F                     ;3
       lda    r90                     ;3
       sbc    LFF9A,Y                 ;4
       sta    r90                     ;3
LF3EE:
       lda    rDD                     ;3
       rol    Temp2                   ;5
       bcs    LF401                   ;2³
       sec                            ;2
       sbc    LFF99,Y                 ;4
       sta    rDD                     ;3
       lda    r8E                     ;3
       sbc    LFF9A,Y                 ;4
       sta    r8E                     ;3
LF401:
       rol    Temp2                   ;5
       bcs    LF411                   ;2³
       adc    LFF99,Y                 ;4
       sta    rDD                     ;3
       lda    r8E                     ;3
       adc    LFF9A,Y                 ;4
       sta    r8E                     ;3
LF411:
       ldy    #$97                    ;2 right side screen border limit
       cpy    r90                     ;3
       bcs    LF419                   ;2³
       sty    r90                     ;3
LF419:
       ldy    #$09                    ;2 left side screen border limit
       cpy    r90                     ;3
       bcc    LF421                   ;2³
       sty    r90                     ;3
LF421:
       ldy    #$0A                    ;2 lower screen border limit
       cpy    r8E                     ;3
       bcc    LF429                   ;2³
       sty    r8E                     ;3
LF429:
;       ldy    #$53                    ;2 upper screen border limit
;moved a bit lower since $51-$53 causes lower crosshair pixels to disappear
       ldy    #$51                    ;2 upper screen border limit
       cpy    r8E                     ;3
       bcs    LF431                   ;2³
       sty    r8E                     ;3
LF431:
;       lda    FrameCounter            ;3
;       and    #$01                    ;2
;       tay                            ;2
;       lda    HposAddTbl,Y            ;4
;       clc                            ;2
;       adc    r90                     ;3 ??
;       sta    r8A                     ;3 ?? player's X
;       lda    r8E                     ;3
;       sta    r85                     ;3
;revised to take larger cursor size into account...
       lda    FrameCounter            ;3
       and    #$01                    ;2
       tay                            ;2
       lda    HposAddTbl,Y            ;4
       clc                            ;2
       adc    r90                     ;3 ??
       sta    r8A                     ;3 ?? player's X
       ldy    r8E                     ;3
       dey                            ;2
       sty    r85                     ;3
       ldy    r92                     ;3
       lda    r98                     ;3
       and    #$02                    ;2
       beq    LF445                   ;2³
       tya                            ;2
       ora    #$10                    ;2
       tay                            ;2
LF445:
       lda    r91                     ;3
       sty    r91                     ;3
       sta    r92                     ;3
       lda    r93                     ;3
       ldy    r94                     ;3
       sty    r93                     ;3
       sta    r94                     ;3
       lda    r97                     ;3
       ldy    r98                     ;3
       sty    r97                     ;3
       sta    r98                     ;3
       lda    r95                     ;3
       ldy    r96                     ;3
       sty    r95                     ;3
       sta    r96                     ;3
       lda    r9A                     ;3
       sta    r88                     ;3
       ldy    r99                     ;3
       sty    r9A                     ;3
       sta    r99                     ;3
       lda    r91                     ;3
       cmp    #$FF                    ;2
       bne    LF4A4                   ;2³
       ldx    #$05                    ;2
LF475:
       lda    CityPtr,X               ;4
       beq    LF486                   ;2³ branch if a city
       dex                            ;2
       bpl    LF475                   ;2³
       lda    rD8                     ;3
       and    #$07                    ;2
       bpl    LF4A2                   ;2³ always branch

LF486:
       lda    rD8                     ;3
       and    #$07                    ;2
       cmp    #$06                    ;2
       bcs    LF4A2                   ;2³
       tax                            ;2
LF490:
       lda    CityPtr,X               ;4
       beq    LF4A0                   ;2³ branch if a city
       inx                            ;2
       cpx    #$06                    ;2
       bne    LF490                   ;2³
       ldx    #$00                    ;2
       beq    LF490                   ;2³ always branch

LF4A0:
       txa                            ;2
LF4A2:
       sta    r95                     ;3
LF4A4:
       lda    FrameCounter            ;3
       and    #$01                    ;2
       tax                            ;2
       lda    r9C,X                   ;4
       clc                            ;2
       adc    r9B                     ;3
       sta    r9C,X                   ;4
       bcc    LF4D6                   ;2³
       lda    r93                     ;3
       cmp    #$54                    ;2
       beq    LF4D6                   ;2³
       lda    r97                     ;3
       and    #$02                    ;2
       beq    LF4D4                   ;2³
       bit    rF8                     ;3
       bvc    LF4D4                   ;2³
       lda    FrameCounter            ;3
       and    #$01                    ;2
       tax                            ;2
       lda    rF9,X                   ;4
       beq    LF4D4                   ;2³
       inc    r93                     ;5
       inc    r93                     ;5
       dec    rF9,X                   ;6
       .byte $2C                      ;4 skip 2 bytes
LF4D4:
       dec    r93                     ;5
LF4D6:
;;Pause: ;still messed up here
       ldx    rCA                     ;3
       dex                            ;2
       bpl    LF4DD                   ;2³
       ldx    #$02                    ;2
LF4DD:
       stx    rCA                     ;3
       lda    FrameCounter            ;3
;;       sta    MissileColor            ;3
       and    #$0F                    ;2
       sta    Temp2                   ;3
       asl                            ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       ora    Temp2                   ;3
       sta    rD7                     ;3
       lda    rA2                     ;3
       and    #$7F                    ;2
       sta    rA2                     ;3
       lda    #$F8                    ;2
       cpx    #$04                    ;2
       bcc    LF51D                   ;2³
       cpx    #$0C                    ;2
       bcs    LF51D                   ;2³
       lda    rA2                     ;3
       ora    #$80                    ;2
       sta    rA2                     ;3
       lda    #$F0                    ;2
LF51D:
       sta    rA1                     ;3
       ldx    rCA                     ;3
       lda    rA3,X                   ;4
       bit    rA2                     ;3
       bpl    LF52C                   ;2³
       clc                            ;2
       adc    #$FC                    ;2
LF52C:
       sta    r83                     ;3
       lda    FrameCounter            ;3
       and    #$07                    ;2
       bne    CheckNukedCities        ;2³
       ldx    #$02                    ;2
LF536:
       inc    r9E,X                   ;6
       lda    r9E,X                   ;4
       cmp    #$10                    ;2
       bne    LF54A                   ;2³
       lda    #$00                    ;2
       sta    rB5,X                   ;4
       lda    ShotOrigin,X            ;4 new missile origin
       sta    rC4,X                   ;4
       lda    #$01                    ;2
       sta    rC7,X                   ;4
LF54A:
       dex                            ;2
       bpl    LF536                   ;2³

CheckNukedCities:
       ldx    #$05                    ;2 for all cities...
NukeAnimationLoop:
       lda    CityPtr,X               ;4
       beq    NextCity                ;2³ branch if a city
       cmp    #<RubbleGFX-1           ;2 is it lower than rubble?
       bcs    NextCity                ;2³ branch if higher (already or not being nuked)
;city currently being nuked...adjust animation frame
       lda    FrameCounter            ;3
       and    #$0F                    ;2
       bne    NextCity                ;2³ every 16th frame go up a frame (city destroyed)
       lda    CityPtr,X               ;4
       clc                            ;2
       adc    #$09                    ;2
;city bugfix
       cmp    #<RubbleGFX             ;2 is it lower than rubble?
       bcc    NoSub                   ;2³ branch if lower
       lda    #<RubbleGFX             ;2
NoSub:
       sta    CityPtr,X               ;4
NextCity:
       dex                            ;2
       bpl    NukeAnimationLoop       ;2³
Pause:
       lda    #$38                    ;2
       sta    r86                     ;3
       lda    #$40                    ;2
       sta    r87                     ;3
       lda    rA1                     ;3
       sta    Temp2                   ;3
       lda    #$00                    ;2
       sta    rA1                     ;3
       ldx    #$04                    ;2
LF005a:
       lda    #$02                    ;2
       cpx    #$02                    ;2
       bcs    LF015a                  ;2³
       lda    #$01                    ;2
       ldy    rA1                     ;3
       cpy    #$F0                    ;2
       bne    LF015a                  ;2³
       lda    #$FC                    ;2
LF015a:
       clc                            ;2
       adc    r86,X                   ;4
       ldy    #$02                    ;2
       sec                            ;2
LF01Ba:
       iny                            ;2
       sbc    #$0F                    ;2
       bcs    LF01Ba                  ;2³
       eor    #$FF                    ;2
       sbc    #$06                    ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       sta    WSYNC                   ;3
LF02Aa:
       dey                            ;2
       bpl    LF02Aa                  ;2³
       sta    RESP0,X                 ;4
       sta    HMP0,X                  ;4
       lda    rA1                     ;3
       bne    LF038a                  ;2³
       dex                            ;2
       bpl    LF005a                  ;2³
LF038a:
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       lda    Temp2                   ;3
       sta    rA1                     ;3
       ldx    #$00                    ;2

       lda    rE6                     ;3
       and    #$20                    ;2
       beq    Paused_Sound            ;2³

       lda    rDF                     ;3
       and    #$0F                    ;2
       asl                            ;2
       tay                            ;2
       lda    LFDF1,Y                 ;4
       pha                            ;3
       lda    LFDF0,Y                 ;4
       pha                            ;3
       rts                            ;6

Paused_Sound:
       tay                            ;2
       sta    AUDF0                   ;3
       sta    AUDC0                   ;3
       sta    AUDV0                   ;3
       jmp    LF6D0                   ;3

Sat_Sound:
       ldx    SatDir                  ;3
       bmi    LF614b                  ;2³
       lda    SatY                    ;3
       bmi    LF614b                  ;2³
       ldx    #$06                    ;2
       cmp    #$20                    ;2
       bcs    Satellite_Sound         ;2³
;plane sound
       ldy    #$03                    ;2
       lda    #$0C                    ;2
       bne    LF614b                  ;2³

Satellite_Sound:
       ldy    #$0C                    ;2
       lda    FrameCounter            ;3
       and    #$07                    ;2
       ora    #$18                    ;2
LF614b:
       jmp    LF655                   ;3

LF5A7:
       lda    rE0                     ;3
       and    #$03                    ;2
       bne    LF5B3                   ;2³
       lda    rD8                     ;3
       and    #$07                    ;2
       sta    rE1                     ;3
LF5B3:
       ldy    #$0C                    ;2
       ldx    #$08                    ;2
       lda    rE1                     ;3
       dec    rE0                     ;5
       bne    LF614                   ;2³
       lda    #$01                    ;2
       sta    rDF                     ;3
       lda    #$04                    ;2
       sta    rE0                     ;3
       bne    LF614                   ;2³
LF5C7:
       dec    rE0                     ;5
       lda    rE0                     ;3
       jmp    LF5D8                   ;3
LF5CE:
       lda    FrameCounter            ;3
       and    #$0F                    ;2
       cmp    #$03                    ;2
       bcs    LF614                   ;2³
       eor    #$07                    ;2
LF5D8:
       asl                            ;2
       tax                            ;2
       ldy    #$08                    ;2
       lda    #$18                    ;2
       bne    LF614                   ;2³
LF5E0:
       lda    FrameCounter            ;3
       ldx    rE0                     ;3
       and    #$1F                    ;2
       bne    LF5EE                   ;2³
       cpx    #$0E                    ;2
       beq    LF5EE                   ;2³
       inc    rE0                     ;5
LF5EE:
       lda    FrameCounter            ;3
       and    #$03                    ;2
       ora    #$08                    ;2
       ldy    #$05                    ;2
       bne    LF655                   ;2³
LF5F8:
       ldy    rE0                     ;3
       lda    LFF7C,Y                 ;4
       ldx    #$08                    ;2
       iny                            ;2
       sty    rE0                     ;3
       cpy    #$08                    ;2
       beq    LF60A                   ;2³
       ldy    #$05                    ;2
       bne    LF614                   ;2³
LF60A:
       lda    rDF                     ;3
       and    #$F0                    ;2
       sta    rDF                     ;3
       ldx    #$00                    ;2
       stx    rE0                     ;3
LF614:
       jmp    LF655                   ;3

;end of game sound?
LF617:
       ldy    #$08                    ;2
       sty    AUDC0                   ;3
       lda    FrameCounter            ;3
       and    #$0F                    ;2
       bne    LF633                   ;2³
       inc    rE0                     ;5
       lda    rE0                     ;3
       cmp    #$10                    ;2 did it reach 16 clicks?
       bne    LF633                   ;2³ do rising sound?
       lda    #$30                    ;2
       sta    rDF                     ;3
       ldx    #$50                    ;2
       stx    rE1                     ;3
       bne    LF655                   ;2³ always branch

LF633:
       ldx    rE0                     ;3
       stx    AUDV0                   ;3
       txa                            ;2
       sta    AUDF0                   ;3
       eor    #$FF                    ;2
       jmp    LF6D0                   ;3


LF63F:
       lda    rDE                     ;3
       cmp    #$A0                    ;2
       bcs    LF655                   ;2³
       ldx    rE0                     ;3
       txa                            ;2
       inx                            ;2
       cpx    #$14                    ;2
       bne    LF64F                   ;2³
       ldx    #$04                    ;2
LF64F:
       stx    rE0                     ;3
       ldy    #$0C                    ;2
       ldx    #$08                    ;2
LF655:
       stx    AUDV0                   ;3
       sty    AUDC0                   ;3
       sta    AUDF0                   ;3
       ldx    #$00                    ;2
       lda    rDF                     ;3
       and    #$F0                    ;2
       cmp    #$10                    ;2
       beq    LF68F                   ;2³
       cmp    #$20                    ;2
       beq    LF6BA                   ;2³
       cmp    #$30                    ;2
       bcc    LF6D0                   ;2³
LF670:
       dec    rE1                     ;5
       beq    LF6A1                   ;2³
       lda    rE1                     ;3
       and    #$70                    ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       tay                            ;2
       ldx    LFF55,Y                 ;4
       ldy    #$08                    ;2
       lda    rE1                     ;3
       and    #$0F                    ;2
       ora    #$10                    ;2
       bne    LF6D0                   ;2³ always branch

LF68F:
       ldy    #$08                    ;2
       ldx    #$06                    ;2
       lda    FrameCounter            ;3
       and    #$03                    ;2
       bne    LF69D                   ;2³
       dec    rE1                     ;5
       beq    LF6A1                   ;2³
LF69D:
       lda    rE1                     ;3
       bne    LF6D0                   ;2³
LF6A1:
       lda    rDF                     ;3
       and    #$0F                    ;2
       sta    rDF                     ;3
       bit    rFB                     ;3
       bpl    LF6D0                   ;2³
       lda    #$C3                    ;2
       sta    rDB                     ;3
       lda    #$00                    ;2
       sta    rFB                     ;3
       sta    rE7                     ;3
       sta    rE8                     ;3
       beq    LF6D0                   ;2³  always branch

LF6BA:
       ldy    rE1                     ;3
       ldx    LFF59,Y                 ;4
       lda    FrameCounter            ;3
       and    #$07                    ;2
       bne    LF6CC                   ;2³
       iny                            ;2
       sty    rE1                     ;3
       cpy    #$10                    ;2
       beq    LF6A1                   ;2³
LF6CC:
       lda    #$1F                    ;2
       ldy    #$08                    ;2
LF6D0:
       stx    AUDV1                   ;3
       sty    AUDC1                   ;3
       sta    AUDF1                   ;3
       jmp    Display                 ;2





LFCCB:
       sta    Temp2                   ;3
       ldx    #$05                    ;2
       lda    rE7                     ;3
       cmp    #$0C                    ;2
       bcs    LFCD7                   ;2³
       lsr                            ;2
       tax                            ;2
LFCD7:
       stx    Temp1                   ;3
LFCD9:
       ldx    rF5                     ;3
       lda    Temp2                   ;3
       sed                            ;2
       clc                            ;2
       adc    rEF,X                   ;4
       sta    rEF,X                   ;4
       tya                            ;2
       adc    rF1,X                   ;4
       sta    rF1,X                   ;4
       lda    #$00                    ;2
       adc    rF3,X                   ;4
       sta    rF3,X                   ;4
       cld                            ;2
       dec    Temp1                   ;5
       bpl    LFCD9                   ;2³
       rts                            ;6











LFD16:
       bit    rF8                     ;3
       bmi    LFD3A                   ;2³
       lda    rEC                     ;3
       bne    LFD63                   ;2³
LFD1E:
       lda    #$07                    ;2
       sta    rDF                     ;3
       ldx    #$FF                    ;2
       stx    rFB                     ;3

   IF PLUSROM
       jsr SendPlusROMScore
   ENDIF


;High_Score:
       lda    rE9                     ;3
       cmp    #$17                    ;2
       bcs    LF0F0                   ;2³
;check high score...
       lda    rEF+4                   ;4
       cmp    rEF+5                   ;3 compare the high byte
       beq    LF0E2                   ;2 branch if the same (and check next)
       bcc    LF0F0                   ;2 branch if lower (and end)
       bcs    Set_HiScore             ;2 higher...new high score
LF0E2:
       lda    rEF+2                   ;3
       cmp    rEF+3                   ;3 check the low byte
       beq    LF0E3                   ;2 branch if the same (and check next)
       bcc    LF0F0                   ;2 branch if lower (and end)
       bcs    Set_HiScore             ;2 higher...new high score
LF0E3:
       lda    rEF                     ;3
       cmp    rEF+1                   ;3 check the low byte
       bcc    LF0F0                   ;2 branch if lower (and end)
Set_HiScore:
       lda    rEF+4                   ;3  New high score...transfer ram
       sta    rEF+5                   ;3
       lda    rEF+2                   ;3
       sta    rEF+3                   ;3
       lda    rEF                     ;3
       sta    rEF+1                   ;3
LF0F0:
       ldx    #$05                    ;2
TxtLoop:
       lda    TxtTbl,X                ;4
       sta    CityPtr,X               ;4
       dex                            ;2
       bpl    TxtLoop                 ;2³
       ldx    #$FF                    ;2
       stx    rDE                     ;3
       inx                            ;2
       stx    rE0                     ;3
       rts                            ;6



LFD3A:
       ldx    rF5                     ;3
       lda    rEC,X                   ;4
       bne    LFD4D                   ;2³
       txa                            ;2
       eor    #$01                    ;2
       tax                            ;2
       lda    rEC,X                   ;4
       beq    LFD1E                   ;2³
       stx    rF5                     ;3
       bne    LFD63                   ;2³ always branch
LFD4D:
;swap players
       txa                            ;2
       eor    #$01                    ;2
       tax                            ;2
       lda    rEC,X                   ;4
       beq    LFD5A                   ;2³
       stx    rF5                     ;3
       bne    LFD63                   ;2³ always branch
;no cities left
LFD5A:
       txa                            ;2
       eor    #$01                    ;2
       sta    rF5                     ;3
       tax                            ;2
       bpl    LFD67                   ;2³ always branch
LFD63:
       ldx    rF5                     ;3
       bne    LFD7C                   ;2³
LFD67:
       inc    rE7                     ;5
       lda    rE7                     ;3
       cmp    #$10                    ;2
       bcs    LFD7C                   ;2³
       lda    r9B                     ;3
       clc                            ;2
       adc    #$08                    ;2
       bit    rFB                     ;3
       bvs    LFD7A                   ;2³
       adc    #$05                    ;2
LFD7A:
       sta    r9B                     ;3
LFD7C:
       lda    rEC,X                   ;4 EC/ED = bitpattern of each player's intact cities
       ldx    #$05                    ;2
LFD80:
       lsr                            ;2
       ldy    #<CityGFX               ;2
       bcs    LFD88                   ;2³
       ldy    #<RubbleGFX             ;2 load rubble
LFD88:
       sty    CityPtr,X               ;4
       dex                            ;2
       bpl    LFD80                   ;2³
       rts                            ;6










  IF PLUSROM
PlusROM_API
    .byte "a", 0, "h.firmaplus.de", 0

SendPlusROMScore:
   lda rE9
   sta WriteToBuffer                ; game variation ? 
   lda rEF + 4
   sta WriteToBuffer
   lda rEF + 2
   sta WriteToBuffer
   lda rEF
   sta WriteToBuffer
   lda #HIGHSCORE_ID                ; game id in Highscore DB
   sta WriteSendBuffer
   rts

       ORG  $1E20
       RORG $DE20

  ELSE

       ORG  $1832
       RORG $D832
;2k free space...
       .byte "Program hack: Missile Command Arcade, by Kurt(Nukey Shay)Howe."
       .byte "v7/27/2006."
       .byte "Original program by Robert Fulop, (c)1981 Atari Corp."
       .byte "Special thanks to AtariAge and it's members!"
       .byte "Hack history (in order of appearence):"
       .byte "Attract mode color shift removed."
       .byte "Various superfluous code removed."
       .byte "Level 13 Easter Egg removed."
       .byte "Playfield missile cache removed."
       .byte "Level-specific colors removed."
       .byte "B&W support removed."
       .byte "Supercharger-compatability added."
       .byte "THE END text added."
       .byte "3-button control added."
       .byte "2 additional sprite missile caches added."
       .byte "AI adjusted to also target the additional caches."
       .byte "Flashing PLAYER# text added."
       .byte "7800 autodetect added."
       .byte "NTSC/PAL palette select added."
       .byte "Ground zero color shading added."
       .byte "Left controller fire button pause routine added."
       .byte "Track & Field controller support added."
       .byte "Hilltop gfx altered, border silos corrected."
       .byte "Sunset sky added behind cities/hilltops."
       .byte "Paused missile trail skew removed, all sprite color cleared."
       .byte "Crosshair targeting added."
       .byte "Program altered to use F8 bankswitching."
       .byte "Satellite object added."
       .byte "Bomber object added."
       .byte "Missile cache gfx altered."
       .byte "Ground area color table added."
       .byte "Score font reduced to 5 points."
       .byte "Satellite/bomber collision detect added @100 points each."
       .byte "Animation table added for satellite/bomber destruction."
       .byte "Satellite/bomber sounds added."
       .byte "Animated title screen added, fast exit w/INPT5."
       .byte "Animated end text screen added."
       .byte "End-of-wave tally screen added."
       .byte "Single-player high score function added."
       .byte "Paused text added."
       .byte "Todo list:"
       .byte "Fix Stella-only problems (short screen, misaligned crosshair sides)."
       .byte "Sound routine index needs bugfixing."
       .byte "Fix enemy warhead color skew."
       .byte "Fix end-of-game crosshair skew."
       .byte "Tweak satellite/bomber collision detection."
       .byte "Correct randomness of satellite/bomber appearance & vertical position."
       .byte "Adjust AI to target caches better."
       .byte "Create satellite/bomber abilities via difficulty switch?"

       ORG  $1F20
       RORG $DF20
  ENDIF

LFF2A: ;16
       .byte $00 ; |        | $FF2A
       .byte $00 ; |        | $FF2B
       .byte $00 ; |        | $FF2C
       .byte $00 ; |        | $FF2D
       .byte $00 ; |        | $FF2E
       .byte $01 ; |       X| $FF2F
       .byte $01 ; |       X| $FF30
       .byte $02 ; |      X | $FF31
       .byte $03 ; |      XX| $FF32
       .byte $04 ; |     X  | $FF33
       .byte $04 ; |     X  | $FF34
       .byte $05 ; |     X X| $FF35
       .byte $05 ; |     X X| $FF36
       .byte $06 ; |     XX | $FF37
       .byte $06 ; |     XX | $FF38
       .byte $07 ; |     XXX| $FF39

LFF51:
       .byte $20 ; |  X     | $FF51
       .byte $10 ; |   X    | $FF52
       .byte $08 ; |    X   | $FF53
       .byte $04 ; |     X  | $FF54
LFF55:
       .byte $02 ; |      X | $FF55 shared
       .byte $04 ; |     X  | $FF56
       .byte $06 ; |     XX | $FF57
       .byte $08 ; |    X   | $FF58
LFF59:
       .byte $0E ; |    XXX | $FF59 shared
       .byte $0C ; |    XX  | $FF5A
       .byte $0A ; |    X X | $FF5B
       .byte $08 ; |    X   | $FF5C
       .byte $0A ; |    X X | $FF5D
       .byte $08 ; |    X   | $FF5E
       .byte $08 ; |    X   | $FF5F
       .byte $06 ; |     XX | $FF60
       .byte $04 ; |     X  | $FF61
       .byte $04 ; |     X  | $FF62
       .byte $08 ; |    X   | $FF63
       .byte $08 ; |    X   | $FF64
       .byte $06 ; |     XX | $FF65
       .byte $04 ; |     X  | $FF66
       .byte $02 ; |      X | $FF67
       .byte $13 ; |   X  XX| $FF68
       .byte $14 ; |   X X  | $FF69
       .byte $15 ; |   X X X| $FF6A
       .byte $16 ; |   X XX | $FF6B

LFF6C: ;16
       .byte $0C ; |    XX  | $FF6C
       .byte $0F ; |    XXXX| $FF6D
       .byte $12 ; |   X  X | $FF6E
       .byte $0C ; |    XX  | $FF6F
       .byte $10 ; |   X    | $FF70
       .byte $0E ; |    XXX | $FF71
       .byte $11 ; |   X   X| $FF72
       .byte $0A ; |    X X | $FF73
       .byte $0D ; |    XX X| $FF74
       .byte $10 ; |   X    | $FF75
       .byte $13 ; |   X  XX| $FF76
       .byte $0C ; |    XX  | $FF77
       .byte $0E ; |    XXX | $FF78
       .byte $10 ; |   X    | $FF79
       .byte $12 ; |   X  X | $FF7A
       .byte $14 ; |   X X  | $FF7B

;41
LFF7C: ;8
       .byte $0C ; |    XX  | $FF7C
       .byte $0A ; |    X X | $FF7D
       .byte $08 ; |    X   | $FF7E
       .byte $06 ; |     XX | $FF7F
       .byte $04 ; |     X  | $FF80
       .byte $02 ; |      X | $FF81
LFF82: ;24
       .byte $00 ; |        | $FF82 shared
       .byte $00 ; |        | $FF83 shared
       .byte $01 ; |       X| $FF84
       .byte $40 ; | X      | $FF85
       .byte $41 ; | X     X| $FF86
       .byte $06 ; |     XX | $FF87
       .byte $07 ; |     XXX| $FF88
       .byte $46 ; | X   XX | $FF89
       .byte $47 ; | X   XXX| $FF8A
       .byte $0A ; |    X X | $FF8B
       .byte $00 ; |        | $FF8C
       .byte $00 ; |        | $FF8D
       .byte $00 ; |        | $FF8E
       .byte $00 ; |        | $FF8F
       .byte $00 ; |        | $FF90
       .byte $00 ; |        | $FF91
       .byte $0B ; |    X XX| $FF92
       .byte $4A ; | X  X X | $FF93
       .byte $4B ; | X  X XX| $FF94
       .byte $0E ; |    XXX | $FF95
       .byte $0F ; |    XXXX| $FF96
       .byte $4E ; | X  XXX | $FF97
       .byte $4F ; | X  XXXX| $FF98
LFF99: ;5
       .byte $00 ; |        | $FF99 shared
LFF9A: ;5
       .byte $01 ; |       X| $FF9A shared
       .byte $80 ; |X       | $FF9B shared
       .byte $01 ; |       X| $FF9C shared
       .byte $80 ; |X       | $FF9D shared
       .byte $00 ; |        | $FF9E


TxtTbl:
       .byte <Txt1 ; $FFCF
       .byte <Txt2 ; $FFD0
       .byte <Txt3 ; $FFD1
       .byte <Txt3 ; $FFD2
       .byte <Txt4 ; $FFD3
       .byte <Txt5 ; $FFD4


StartTbl: ;3 bytes
       .byte $9B ; |X  XX  X| $FDD2 silo
       .byte $03 ; |     X X| $FDD3 shared
       .byte $4F ; | X  XXXX| $FDD4 silo

;indirect jump point table
LFDF0:
LFDF1 = LFDF0+1
       .word Sat_Sound-1
       .word LF63F-1 ; $FDF2/3
       .word LF5F8-1 ; $FDF4/5
       .word LF5E0-1 ; $FDF6/7
       .word LF5C7-1 ; $FDF8/9
       .word LF5CE-1 ; $FDFA/B
       .word LF5A7-1 ; $FDFC/D
       .word LF617-1 ; $FDFE/F

HposAddTbl: ;upper scanline pixel X adjustment value of crosshair
       .byte $01 ; $FDE2
       .byte $07 ; $FDE3

LFDCC:
       lda    rEC,X                   ;4
       ora    Temp2                   ;3
       sta    rEC,X                   ;4
       lda    rF6,X                   ;4
       sed                            ;2
       clc                            ;2
       adc    #$01                    ;2
       cld                            ;2
       sta    rF6,X                   ;4
       .byte $C9                      ;2 skip next byte (CMP#)
LFDDE:
       rts                            ;6
LFD96:
       ldx    rF5                     ;3
       lda    rF3,X                   ;4
       cmp    rF6,X                   ;4
       beq    LFDDE                   ;2³
       lda    rEC,X                   ;4
       cmp    #$3F                    ;2
       beq    LFDDE                   ;2³
       lda    #$06                    ;2
       sta    rDF                     ;3
       lda    #$A0                    ;2
       sta    rE0                     ;3
       lda    rD8                     ;3
       and    #$07                    ;2
       cmp    #$06                    ;2
       bcc    LFDB6                   ;2³
       sbc    #$04                    ;2
LFDB6:
       tay                            ;2
       lda    LFF51,Y                 ;4
       sta    Temp2                   ;3
LFDBC:
       lda    Temp2                   ;3
       and    rEC,X                   ;4
       beq    LFDCC                   ;2³
       lsr    Temp2                   ;5
       bcc    LFDBC                   ;2³
       lda    #$20                    ;2
       sta    Temp2                   ;3
       bne    LFDBC                   ;2³ always branch


AddDAA:
       lda    rC4,X                   ;4
       clc                            ;2
       adc    Temp2                   ;3
       sta    rC4,X                   ;4
       rts                            ;6


AddDA:
       lda    rC7,X                   ;4
       clc                            ;2
       adc    Temp2                   ;3
       sta    rC7,X                   ;4
       rts                            ;6 shared

SubDA:
       lda    rC4,X                   ;4
       sec                            ;2
       sbc    Temp2                   ;3
       sta    rC4,X                   ;4
       rts                            ;6

   IF PLUSROM
       ORG  $1FEA
       RORG $1FEA
START1
       lda    $1FF9

       ORG  $1FFA
       RORG $1FFA
   .word (PlusROM_API-$C000)        ; PlusRom API pointer
   .word START1,0
   ELSE
       ORG  $1FF8
       RORG $DFF8
       .byte 0
START1:
       jmp    START                   ;3
       .word START1,0
   ENDIF

































       ORG  $2000
       RORG $F000

Display_Done:
       sta    $1FF8                   ;3
;removed B&W code
;removed level colors
       lda    rDF                     ;3
       and    #$0F                    ;2
       cmp    #$07                    ;2
       bne    InGame                  ;2³
       jmp    Game_End                ;3 jump to display "The End" text
InGame:
       ldy    #$0A                    ;2
       lda    rE6                     ;3
       and    #$20                    ;2
       bne    NotPaused3              ;2³
       ldy    #<Paused6               ;2
       sty    PrintTemp               ;3
       ldy    #$0A+11                 ;2
NotPaused3:
       ldx    #$0B                    ;2
PlayerStartLoop:
       lda    PlayerUpTxtTbl,y        ;4
       sta    PrintTemp,x             ;4
       dey                            ;2
       dex                            ;2
       bne    PlayerStartLoop         ;2³
       iny                            ;2
       bne    Use_Paused_Text         ;2³
       ldy    rF5                     ;3
       lda    rDB                     ;3
       and    #$04                    ;2
       beq    LF748                   ;2³

       ldy    #$00                    ;2

       lda    rE9                     ;3
       sta    rF3                     ;3
       stx    rF1                     ;3 x=0
       inx                            ;2
       cmp    #$18                    ;2
       bcc    LF746                   ;2³
       inx                            ;2
LF746:
       stx    rEF                     ;3
LF748:
;Score offsets: 0, 2, and 4
       iny                            ;2
       iny                            ;2
       iny                            ;2
       iny                            ;2
       ldx    #$0C                    ;2
;added...flash text
       lda    rDB                     ;3
       and    #$04                    ;2
       bne    SetScorePointersLoop    ;2³ branch if game inactive
       lda    rE6                     ;3
       and    #$20                    ;2
       beq    PrintPlayer             ;2³
       lda    rDE                     ;3
       bpl    SetScorePointersLoop    ;2³
       cmp    #$E0                    ;2
       beq    SetScorePointersLoop    ;2³
       lda    FrameCounter            ;3
       and    #$30                    ;2
       bne    SetScorePointersLoop    ;2³
PrintPlayer:
       ldy    rF5                     ;3
       iny                            ;2
       lda    DigitTbl,y              ;3
       sta    PrintTemp               ;3
Use_Paused_Text:
       bne    NotGameSelect           ;2³ always branch

;moved here to correct page break error (when printing hilltops)
;increase shot count display
Roll:
       ror    PrintTemp,x             ;6 carry -> left PF1.7-0 ...
       rol    PrintTemp+1,x           ;6 ...left PF2.0-7...
       ror    PrintTemp+2,x           ;6 ...right PF2.7-0...
       rol    PrintTemp+3,x           ;6 ...right PF1.0-7 (ending carry unused)
       rts                            ;6


;subroutine to print a line of counted missiles (using X as the set # offset)
Print_Missiles:
       lda    PrintTemp,x             ;4
       sta    PF1                     ;3
       lda    PrintTemp+1,x           ;4
       sta    PF2                     ;3
       dec    Waste2                  ;5 waste some time between left and right PF gfx...
       dec    Waste2                  ;5 ..
       dec    Waste2                  ;5 ..
       nop                            ;2 ..
       nop                            ;2 ..
       nop                            ;2 ..
       lda    PrintTemp+2,x           ;4 @45
       sta    PF2                     ;3
       lda    PrintTemp+3,x           ;4
       sta    PF1                     ;3
       dey                            ;2
       rts                            ;6 @63


SetScorePointersLoop:
       lda.wy rEF,y                   ;4
       sta    PrintTemp+2             ;3
       sty    PrintTemp               ;3
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       tay                            ;2
       lda    DigitTbl,y              ;3
       ldy    PrintTemp+2             ;3
       sta    PrintTemp-2,x           ;4
       dex                            ;2
       dex                            ;2
       tya                            ;2
       and    #$0F                    ;2
       tay                            ;2
       lda    DigitTbl,y              ;3
       ldy    PrintTemp               ;3
       sta    PrintTemp-2,x           ;4
       dex                            ;2
       dex                            ;2
       dey                            ;2
       dey                            ;2
       bpl    SetScorePointersLoop    ;2³
;clear leading zeros for first 5 digits only
       ldx    #$08                    ;2
       ldy    #<DigitBlank            ;2
LF78B:
       lda    PrintTemp+2,X           ;4
       bpl    CheckGameSelect         ;2³
       sty    PrintTemp+2,X           ;4 save blank space
       dex                            ;2
       dex                            ;2
       bpl    LF78B                   ;2³
;clear digits not used in game selection mode
CheckGameSelect:
       lda    rDB                     ;3
       and    #$04                    ;2
       beq    NotGameSelect           ;2³
       sty    PrintTemp+2             ;3
       sty    PrintTemp+4             ;3
       sty    PrintTemp+6             ;3
NotGameSelect:
       lda    rD7                     ;3
       and    r86                     ;3
       sta    rD7                     ;3
       lda    rE6                     ;3
       and    #$01                    ;2
       sta    COLUBK                  ;3
       asl                            ;2
       ora    rF5                     ;3
       tax                            ;2
;color for the background of the score
       lda    #$03                    ;2
       sta    NUSIZ1                  ;3
       sta    VDELP0                  ;3
       sta    VDELP1                  ;3
;color for the score digits
       ldy    ScoreColor,X            ;4 load custom score color
       sty    COLUP0                  ;3
DisplayStartWait:
       lda    INTIM                   ;4
       bne    DisplayStartWait        ;2³
       sta    WSYNC                   ;3
       sta    VBLANK                  ;3
       sta    r81                     ;3
       sta    HMM0                    ;3
       sta    HMM1                    ;3
       sta    HMBL                    ;3
       sty    COLUP1                  ;3
       ldy    #$04                    ;2
DrawScoreLoop:
;No need to waste 2 bytes of ram in this version...
;though it does garble sprites (1 bit in 5th and 2 bits in 6th)
;Sprite data edited to clear those bits.
       lda    (PrintTemp+10),Y        ;5
       sta    GRP0                    ;3
       sta    WSYNC                   ;3
       lda    (PrintTemp+8),Y         ;5
       sta    GRP1                    ;3
       lda    (PrintTemp+6),Y         ;5
       sta    GRP0                    ;3
       lax    (PrintTemp+2),Y         ;5
       txs                            ;2
       lax    (PrintTemp+4),Y         ;5
       lda    (PrintTemp),Y           ;5
       jmp    Waste3cycles            ;3
Counting_Playfield:
       jmp    Counting_Playfield2     ;3
Waste3cycles:
       dec    Waste2                  ;5
       stx    GRP1                    ;3
       tsx                            ;2 <-cause of the garble
       stx    GRP0                    ;3
       sta    GRP1                    ;3
       sta    GRP0                    ;3
       dey                            ;2
       bpl    DrawScoreLoop           ;2³
       iny                            ;2 Y now = $00
       sty    GRP1                    ;3
       sty    GRP0                    ;3
       sty    GRP1                    ;3
       lda    FrameCounter            ;3
       and    #$0F                    ;2
       sta    COLUPF                  ;3
       lda    #>GFX                   ;2
       sta    PrintTemp+1             ;3
       sta    PrintTemp+3             ;3
       sta    PrintTemp+5             ;3
       sta    PrintTemp+7             ;3
       sta    PrintTemp+9             ;3
       sta    PrintTemp+11            ;3
;keep Y's value of $00 by using A instead here
       sty    COLUBK                  ;3 Y = 00 from above (black)

       lda    rDE                     ;3
       adc    #$17                    ;2
       cmp    #$E0                    ;2 counting cities
       bcs    Counting_Playfield      ;2³ branch if end-of-wave tally
;Game_Playfield:
       ldx    rCA                     ;3
       ldy    #<Blank                 ;2
       lda    rB5,X                   ;4
       and    #$20                    ;2

       sty    VDELP0                  ;3
       sty    VDELP1                  ;3

       beq    LF4FB                   ;2³
       lda    r9E,X                   ;4
       tax                            ;2
       ldy    ExplosionAnim,X         ;4
LF4FB:
;;       sta    WSYNC                   ;3  watch out!
       sty    PrintTemp+6             ;3 save current frame of explosion
       ldy    FrameCounter            ;3
       lda    r97                     ;3
       and    #$02                    ;2
       bne    LF843                   ;2³
       tay                            ;2
       lda    FrameCounter            ;3
       and    #$08                    ;2
       bne    LF843                   ;2³
       ldy    #$0F                    ;2
LF843:
       tya                            ;2
       and    r86                     ;3
       sta    rD7                     ;3
       ldx    rCA                     ;3
       lda    rA6,X                   ;4
       sta    r87                     ;3
;       lda    rCA                     ;3
;       ldy    #$10                    ;2 explosion sprite single width??
;       cmp    #$04                    ;2
;       bcc    LF51DD                  ;2³
;       cmp    #$0C                    ;2
;       bcs    LF51DD                  ;2³
       ldy    #$15                    ;2 explosion sprite = double width
;LF51DD:
       sty    NUSIZ1                  ;3
       lda    #$44                    ;2 OK to reduce some if needed
       sta    TIM8T                   ;4
;position explosion cloud
       lda    #$01                    ;2
       ldy    rA1                     ;3
       cpy    #$F0                    ;2
       bne    LF015                   ;2³
       lda    #$FC                    ;2
LF015:
       clc                            ;2
       adc    r87                     ;3
       ldy    #$02                    ;2
       sec                            ;2
LF01B:
       iny                            ;2
       sbc    #$0F                    ;2
       bcs    LF01B                   ;2³
       eor    #$FF                    ;2
       sbc    #$06                    ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       sta    WSYNC                   ;3
LF02A:
       dey                            ;2
       bpl    LF02A                   ;2³
       sta.w  RESP1                   ;4
       sta    WSYNC                   ;3
       sta    HMP1                    ;3
;position satellite...
       ldy    SatX                    ;3
       lda    XposTbl,y               ;4
       sta    HMP0                    ;3
       and    #$0F                    ;2
       tay                            ;2
       dey                            ;2
Reposition_Sat:
       dey                            ;2
       bpl    Reposition_Sat          ;2³
       sta    RESP0                   ;3
       lda    CityPtr                 ;3
       sta    PrintTemp               ;3
       lda    CityPtr+1               ;3
       sta    PrintTemp+2             ;3
       lda    CityPtr+2               ;3
       sta    PrintTemp+4             ;3
       lda    CityPtr+5               ;3
       sta    PrintTemp+10            ;3
       lda    FrameCounter            ;3
       and    #$03                    ;2
       bne    LF864                   ;2³
       lda    SatDir                  ;3
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       sta    REFP0                   ;3
       and    #$10                    ;2
       bne    Sat_Destroyed           ;2³
       lda    rE6                     ;3
       and    #$01                    ;2
       tax                            ;2
       lda    SatColor,x              ;4
       bne    Sat_Moving              ;2³
Sat_Destroyed:
       lda    FrameCounter            ;3
Sat_Moving:
       ldx    #$00                    ;2
       beq    PausedTrail             ;2³ always branch


LF864:
       ldx    r91                     ;3
       ldy    #$00                    ;2
       lda    rE6                     ;3
       and    #$20                    ;2
       beq    PausedTrail             ;2³
       lda    rE6                     ;3
       and    #$01                    ;2
       tay                            ;2
;       lda    #$40                    ;2
       lda    RedColor,y              ;4
       ldy    #$28                    ;2 edit explosion color here (Light brown)
PausedTrail:
       sta    COLUP0                  ;3
       sty    COLUP1                  ;3
       stx    NUSIZ0                  ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       lda    FrameCounter            ;3 flicker crosshair sides @ 30hz
       and    #$01                    ;2
       tay                            ;2
       lda    HposTbl,Y               ;4
       sta    Crosshair               ;3
       lda    SatDir                  ;3
       bit    SatDir                  ;3  when destroyed...
       bvc    Not_Destroyed           ;2³ show original object for the first second
       bmi    Destroyed               ;2³ show cloud for the next second
Not_Destroyed:
       lda    #<SatGfx                ;2 determine object type here...set satellite
       ldx    SatY                    ;3 check height
       cpx    #$20                    ;2 higher than 32 lines?
       bcs    Use_Satellite           ;2³ branch if so
       lda    #<PlaneGfx              ;2 use plane instead
       bne    Use_Satellite           ;2³ always branch
Destroyed:
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       tax                            ;2
       lda    ExplosionAnim,X         ;4
Use_Satellite:
       sta    PrintTemp+8             ;3
LF864wait:
       ldx    INTIM                   ;4
       bne    LF864wait               ;2³
       stx    HMP0                    ;3
       stx    HMP1                    ;3
       ldx    #$53                    ;2
;new...rather than use a single scanline comparison for cursor location, use a range instead
       txa                            ;2
       sec                            ;2
       sbc    r85                     ;3
       sbc    #$04                    ;2
       cmp    #$FA                    ;2
LF86B:
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       bcc    LF8A0                   ;2³
       lda    Crosshair               ;3
       sta    HMBL                    ;3
       ldy    #$02                    ;2
       bne    LF8A00                  ;2³
LF8A0:
       ldy    #$00                    ;2
LF8A00:
       sty    ENABL                   ;3 @40
       lda    FrameCounter            ;3
       and    #$03                    ;2
       bne    DisplayMiss             ;2³
       lda    SatY                    ;3
       bmi    No_Satellite            ;2³  branch if OK to display
       txa                            ;2
       sec                            ;2
       sbc    SatY                    ;3
       cmp    #$08                    ;2
       bcs    LF8A1                   ;2³
       tay                            ;2
       lda    (PrintTemp+8),Y         ;5
       jmp    LF8A2                   ;3

No_Satellite:
LF8A1:
       lda    #$00                    ;2
LF8A2:
       sta    GRP0                    ;3
       jmp    Sat_Displayed           ;3
DisplayMiss:
       ldy    FrameCounter            ;3 flashing missile heads
       lda    #$02                    ;2
       cpx    r93                     ;3
       bne    LF87B                   ;2³
       sty    COLUP0                  ;3
       beq    LF883                   ;2³ always branch

LF87B:
       bit    r97                     ;3
       bne    LF881                   ;2³
       bcs    LF883                   ;2³
LF881:
       lda    #$00                    ;2
LF883:
       sta    ENAM0                   ;3
       lda    r81                     ;3
       clc                            ;2
       adc    r95                     ;3
       sta    r81                     ;3
       ldy    #$00                    ;2
       bcc    LF892                   ;2³
       ldy    r97                     ;3
LF892:
       sty    HMM0                    ;3
Sat_Displayed:
       sta    WSYNC                   ;3
       txa                            ;2
       sec                            ;2
       sbc    r83                     ;3
       tay                            ;2
       bit    rA2                     ;3
       bpl    LF8AC                   ;2³
       lsr                            ;2
       tay                            ;2
       asl                            ;2
LF8AC:
       and    rA1                     ;3
       bne    LF8B5                   ;2³
       lda    (PrintTemp+6),Y         ;5
       jmp    LF8B7                   ;3
LF8B5:
       lda    #$00                    ;2
LF8B7:
       sta    GRP1                    ;3
       lda    #$00                    ;2
       cpx    r84                     ;3
       bne    LF8C1                   ;2³
       lda    #$02                    ;2
LF8C1:
       sta    ENAM1                   ;3
       dex                            ;2
       beq    LF86D                   ;2³
;new...rather than use a single scanline comparison for cursor location, use a range instead
       txa                            ;2
       sec                            ;2
       sbc    r85                     ;3
       sbc    #$04                    ;2
       cmp    #$FA                    ;2
       jmp    LF86B                   ;3

LF86D:
       ldy    CityPtr+4               ;3
       sty    PrintTemp+8             ;3
       ldy    #>GFX                   ;2
       sty    PrintTemp+3             ;3
       sta    WSYNC                   ;3
       ldy    #$42                    ;2
       sty    COLUBK                  ;3
       lda    r84                     ;3
       cmp    #$01                    ;2
       beq    LF8D5                   ;2³
       ldy    #$00                    ;2
       beq    LF8D7                   ;2³ always branch
LF8D5:
       nop                            ;2
       nop                            ;2
LF8D7:
       sty    ENAM1                   ;3
LF8D9:
       lda    #$01                    ;2
       stx    ENAM0                   ;3
       stx    RESP0                   ;3
       ldx    CityPtr+3               ;3
       stx    PrintTemp+6             ;3
;;       stx    ENAM0                   ;3
;cities color
       and    rE6                     ;3
       tay                            ;2
       ldx    CityColor,Y             ;4
       stx    COLUP0                  ;3
       sta    REFP0                   ;3
       stx    COLUP1                  ;3
       sta    RESP1                   ;3
       ldy    #$08                    ;2
       sty    ENAM1                   ;3
       ldx    #$23                    ;2 brown (shared for sprite size)
       stx    COLUPF                  ;3
       stx    NUSIZ0                  ;3
       stx    NUSIZ1                  ;3
DrawCitiesLoop:
       ldx    HillGFX2,Y              ;4 get hill gfx
       tya                            ;2
       stx    PF0                     ;3
       eor    #$4F                    ;2
       sta    COLUBK                  ;3
       lda    (PrintTemp),Y           ;5
       sta    GRP0                    ;3
       lda    HillGFX,Y               ;4 get hill gfx
       sta    PF2                     ;3
       lda    (PrintTemp+2),Y         ;5
       sta    GRP0                    ;3
       lda    (PrintTemp+4),Y         ;5
       sta    GRP0                    ;3
       lax    (PrintTemp+10),Y        ;5 (illegal)
       lda    (PrintTemp+6),Y         ;5
       sta    GRP1                    ;3
       lda    (PrintTemp+8),Y         ;5
       sta    GRP1                    ;3
       lda    #$00                    ;2 null value used below to reset size/playfield
       stx    GRP1                    ;3
       dey                            ;2
       bpl    DrawCitiesLoop          ;2³
       sty    PrintTemp+3             ;3
       sta    NUSIZ0                  ;3
       sta    NUSIZ1                  ;3
       ldy    #$0F                    ;2 white missiles in cache
;top section of land (below the hill)...clear playfield and sprites
       sta    PF0                     ;3
       ldx    #$20                    ;2
       stx    COLUBK                  ;3
       sta.w  PF2                     ;4
       sta    GRP0                    ;3
       sta    GRP1                    ;3
       ldx    Hz20                    ;3
       sty    COLUP0                  ;3
       sty    COLUP1                  ;3
       ldy    Pos0,X                  ;4
       sty    HMP0                    ;3
       ldy    Pos1,X                  ;4
       sty    HMP1                    ;3
       ldy    CacheCount,X            ;4
       bit    rDE                     ;3
       bmi    Counting                ;2³ branch if missiles not dropping
       lda    CacheGFX,Y              ;4 load count gfx pointer for silo (with low/out text)
       sta    PrintTemp               ;3
       lda    CacheGFX2,Y             ;4 load count gfx pointer for silo (with low/out text)
       jmp    Not_Counting            ;3
Counting:
       nop                            ;2
       lda    CacheGFX+11,Y           ;4 load count gfx pointer for silo (without low/out text)
       sta    PrintTemp               ;3
       lda    CacheGFX2+11,Y          ;4 load count gfx pointer for silo (without low/out text)
Not_Counting:
       sta    PrintTemp+2             ;3
       ldy    CachePos,X              ;4 get horizontal positioning
;new scanline
       ldx    #$22                    ;2
       stx    COLUBK                  ;3
PositionCache:
       dey                            ;2
       bpl    PositionCache           ;2³
       sta    RESP0                   ;3
       sta    RESP1                   ;3
       sty    PrintTemp+1             ;3 Y=FF
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       iny                            ;2
       sty    COLUBK                  ;3
       ldy    #$0B                    ;2
DrawCacheLoop:
;missiles, cache color
       ldx    GroundColor,y           ;4
       lda    (PrintTemp),y           ;5  
DrawCacheLoop2:
       sta    WSYNC                   ;3
       stx    COLUBK                  ;3
       sta    GRP0                    ;3  
       lda    (PrintTemp+2),y         ;5  
       sta    GRP1                    ;3  
       dey                            ;2
       bpl    DrawCacheLoop           ;2³ loop until all lines done
       ldx    #$FF                    ;3
       txs                            ;2
       ldx    #$2A                    ;3
       lda    rDF                     ;3
       cmp    #$30                    ;2
       bcc    LF730                   ;2³
       ldx    FrameCounter            ;3
LF730:
;;       lda    rE6                     ;3
;;       and    #$20                    ;2
;;       bne    NotPaused2              ;2³
;;       tax                            ;2
;;NotPaused2:
       sta    WSYNC                   ;3
;color for the remaining part of land
       stx    COLUBK                  ;3
       iny                            ;2
       sty    GRP0                    ;3
       sty    GRP1                    ;3
;reset the sprite sizes (small triple)
       lda    #$03                    ;3
       sta    NUSIZ0                  ;3
       sta    NUSIZ1                  ;3
       lda    #$11                    ;2
       sta    TIM64T                  ;4
       lda    r91                     ;3
       cmp    #$FF                    ;2
       bne    LF991                   ;2³
       bit    rDE                     ;3
       bmi    LF991                   ;2³
       jsr    LFA81                   ;6
       bcc    LF98E                   ;2³
       lda    r94                     ;3
       cmp    #$54                    ;2
       bne    LF98E                   ;2³
       jsr    LFCF4                   ;6
       beq    LF98E                   ;2³ always branch

LF991:
       ldy    r93                     ;3
       bne    LF99B                   ;2³
       jsr    LFBB2                   ;6
LF98E:
       jmp    LFA6F                   ;3

LF99B:
       and    #$07                    ;2
       tax                            ;2
       lda    LFF22,X                 ;4
       sta    r88                     ;3
       ldy    LFFEF,X                 ;4
       lda    LFDE4,Y                 ;4
       sta    PrintTemp+6             ;3
       lda    LFDE5,Y                 ;4
       sta    PrintTemp+7             ;3
       lda    #$53                    ;2
       sec                            ;2
       sbc    r93                     ;3
       sta    r81                     ;3
       lda    r95                     ;3
       sta    Temp2                   ;3
       lda    #$00                    ;2
       sta    r87                     ;3
       ldx    #$08                    ;2
LFC7EE:
       asl                            ;2
       rol    r87                     ;5
       asl    r81                     ;5
       bcc    LFC8CC                  ;2³
       clc                            ;2
       adc    Temp2                   ;3
       bcc    LFC8CC                  ;2³
       inc    r87                     ;5
LFC8CC:
       dex                            ;2
       bne    LFC7EE                  ;2³
       sta    r86                     ;3
LF9BE:
       ldy    r88                     ;3
       lda    r99                     ;3
       clc                            ;2
       adc    (PrintTemp+6),Y         ;5
       ldy    r93                     ;3
       sty    r83                     ;3
       ldy    r97                     ;3
       cpy    #$F0                    ;2
       bcc    LF9D5                   ;2³
       clc                            ;2
       adc    r87                     ;3
       jmp    LF9D8                   ;3

LF9D5:
       sec                            ;2
       sbc    r87                     ;3
LF9D8:
       sta    Temp1                   ;3
       ldx    rCA                     ;3
       lda    rB5,X                   ;4
       and    #$20                    ;2
       bne    CheckHit                ;2³ xx
       jmp    LFA68                   ;2³
CheckHit:
       lda    SatY                    ;3
       bmi    No_sat_hit              ;2³
       sec                            ;2
       sbc    rC7,X                   ;4
;;       sbc    #$03                    ;2
       cmp    #$F8                    ;2
       bcc    No_sat_hit              ;2³
       lda    SatX                    ;3
       sec                            ;2
       sbc    rC4,X                   ;4
       sec                            ;2
       sbc    #$03                    ;2
       cmp    #$F8                    ;2
       bcc    No_sat_hit              ;2³
       lda    SatDir                  ;3
       bmi    No_sat_hit              ;2³
       ora    #$80                    ;2
       sta    SatDir                  ;3
       ldy    #$01                    ;2 100 points
       lda    #$00                    ;2
       jsr    LFCCBB                  ;6
No_sat_hit:
       ldy    rC7,X                   ;4
       lda    rC4,X                   ;4
       tax                            ;2
       txa                            ;2
       sec                            ;2
       sbc    Temp1                   ;3
       bcs    LFC56                   ;2³
       eor    #$FF                    ;2
       adc    #$01                    ;2
LFC56:
       sta    Temp1                   ;3
       tya                            ;2
       sec                            ;2
       sbc    r83                     ;3
       bcs    LFC62                   ;2³
       eor    #$FF                    ;2
       adc    #$01                    ;2
LFC62:
       cmp    Temp1                   ;3
       bcc    LFC6B                   ;2³
       ldx    Temp1                   ;3
       sta    Temp1                   ;3
       txa                            ;2
LFC6B:
       lsr                            ;2
       lsr                            ;2
       sta    r83                     ;3
       asl                            ;2
       clc                            ;2
       adc    r83                     ;3
       lsr                            ;2
       clc                            ;2
       adc    Temp1                   ;3
       sta    Temp1                   ;3
       ldx    rCA                     ;3
       ldy    r9E,X                   ;4
       lda    r97                     ;3
       and    #$02                    ;2
       beq    LFA14                   ;2³
       bit    rF8                     ;3
       bvc    LFA14                   ;2³
       lda    LFF3A,Y                 ;4
       cmp    Temp1                   ;3
       bcc    LFA68                   ;2³
       lda    Temp1                   ;3
       cmp    #$03                    ;2
       bcc    LFA1E                   ;2³
       lda    FrameCounter            ;3
       and    #$01                    ;2
       tax                            ;2
       lda    #$02                    ;2
       sta    rF9,X                   ;4
       bne    LFA68                   ;2³ always branch


LFA53:
       lda    PrintTemp+6             ;3
       clc                            ;2
       adc    #$0D                    ;2
       sta    PrintTemp+6             ;3
       lda    r99                     ;3
       clc                            ;2
       adc    (PrintTemp+6),Y         ;5
       sta    r99                     ;3
       lda    PrintTemp+6             ;3
       sec                            ;2
       sbc    #$1A                    ;2
       sta    PrintTemp+6             ;3
LFA68:
       dec    r88                     ;5
       bmi    LFA6F                   ;2³
       jmp    LF9BE                   ;3

LFA14:
       lda    LFF3A,Y                 ;4
       cmp    Temp1                   ;3
       bcc    LFA68                   ;2³

;missile hit
LFA1E:
       ldy    #$00                    ;2
       lda    r97                     ;3
       and    #$02                    ;2
       beq    LFA27                   ;2³
       iny                            ;2
LFA27:
       lda    #$25                    ;2 add 25 pts
       jsr    LFCCBB                  ;6
       lda    PrintTemp+6             ;3
       clc                            ;2
       adc    #$0D                    ;2
       sta    PrintTemp+6             ;3
       ldy    r88                     ;3
       lda    (PrintTemp+6),Y         ;5
       sta    r91                     ;3
       cmp    #$FF                    ;2
       bne    LFA53                   ;2³
       lda    #$55                    ;2
       sta    r93                     ;3
LFA6F:
LFA6Fwait:
       lda    INTIM                   ;4
       bne    LFA6Fwait               ;2³
       ldy    #$02                    ;2
       sty    WSYNC                   ;3
       sty    VBLANK                  ;3
       sta    COLUBK                  ;3 A=0 from above
       jmp    Display_Done            ;3















START:
       sei                            ;2
       cld                            ;2
       lda    #$00                    ;2  clear A
;detect the console type
       tay                            ;2  Console type = 2600
       ldx    $D0                     ;3  Check $D0
       cpx    #$2C                    ;2  Does it contain value $2C?
       bne    Save_Console            ;2³  If not, branch (keep 2600 mode)
       ldx    $D1                     ;3  Check $D1
       cpx    #$A9                    ;2  Does it contain value $A9?
       bne    Save_Console            ;2³  If not, branch (keep 2600 mode)
       ldy    #$40                    ;2  set 7800 mode (bit 6)
Save_Console:
;now, clear out all ram
       tax                            ;2
LF047:
       sta    VSYNC,X                 ;4
       inx                            ;2
       bne    LF047                   ;2³
       iny                            ;2 bugfix for 7800
       tya                            ;2
       tax                            ;2
       txs                            ;2
       ldx    #119                    ;2
Ram_Setup:
       lda    MarqueeData,x           ;4
       sta    Marquee,x               ;4
       dex                            ;2
       bpl    Ram_Setup               ;2³
       inx                            ;2
       lda    #<SplashExplosion       ;2
       sta    PrintTemp+10            ;3
       lda    #>SplashExplosion       ;2
       sta    PrintTemp+11            ;3
       sta    PrintTemp+9             ;3
       sta    PrintTemp+8             ;3
Splash_Display:
       ldx    #$00                    ;2
       ldy    #$02                    ;2
       sty    VBLANK                  ;3
       sty    VSYNC                   ;3
       sty    WSYNC                   ;3
       ldy    #$20                    ;2 247
       sty    WSYNC                   ;3
       stx    VSYNC                   ;3 x=0
       sty    TIM64T                  ;4
       lda    #$28                    ;2
       sta    COLUPF                  ;3
       lda    #$05                    ;3
       sta    NUSIZ0                  ;3
       sta    NUSIZ1                  ;3
       lda    FrameCounter            ;3
       sta    COLUP0                  ;3
       sta    COLUP1                  ;3
       ldx    PrintTemp+11            ;3
       cpx    #>SplashExplosion       ;2
       beq    Skip_Degrade            ;2³
       and    #$03                    ;2
       beq    Too_High                ;2³
Skip_Degrade:
       jmp    SplashDisplayWait       ;3
;pick a new location...
Too_High:
       dec    PrintTemp+9             ;5
       lda    FrameCounter            ;3
       ror                            ;2
       ror                            ;2
       ror                            ;2
       lsr                            ;2
       lsr                            ;2
       eor    INTIM                   ;4
       asl                            ;2
       eor    PrintTemp+9             ;3
       and    #$1F                    ;2
       sta    PrintTemp+9             ;3
       cmp    #23                     ;2
       bcs    Too_High                ;2³
Too_High2:
       dec    PrintTemp+8             ;5
       lda    FrameCounter            ;3
       rol                            ;2
       rol                            ;2
       rol                            ;2
       rol                            ;2
       lsr                            ;2
       lsr                            ;2
       eor    INTIM                   ;4
       asl                            ;2
       eor    PrintTemp+8             ;3
       and    #$1F                    ;2
       sta    PrintTemp+8             ;3
       cmp    #23                     ;2
       bcs    Too_High2               ;2³
       asl                            ;2
       asl                            ;2
       eor    PrintTemp+9             ;3
       asl                            ;2
       eor    INTIM                   ;4
       and    #80                     ;2
       sta    WSYNC                   ;3
       sta.w  PrintTemp+6             ;4
       tay                            ;2
;position explosion1...
       lda    XposTbl,y               ;4
       sta    HMP0                    ;3
       and    #$0F                    ;2
       tay                            ;2
       dey                            ;2
Reposition_Ex1:
       dey                            ;2
       bpl    Reposition_Ex1          ;2³
       sta    RESP0                   ;3
       lda    PrintTemp+6             ;3
       sta    WSYNC                   ;3
       eor    #$7F                    ;2
       nop                            ;2
       tay                            ;2
;position explosion2...
       lda    XposTbl,y               ;4
       sta    HMP1                    ;3
       and    #$0F                    ;2
       tay                            ;2
       dey                            ;2
Reposition_Ex2:
       dey                            ;2
       bpl    Reposition_Ex2          ;2³
       sta    RESP1                   ;3
       lda    PrintTemp+6             ;3
       lsr                            ;2
       lsr                            ;2
       tay                            ;2
       lda    #23                     ;2
       sec                            ;2
       sbc    PrintTemp+9             ;3
       sta    PrintTemp+7             ;3
       asl                            ;2
       asl                            ;2
       clc                            ;2
       adc    PrintTemp+7             ;3
       adc    ByteRemoveTbl,y         ;4
       tax                            ;2
       lda    Marquee,x               ;4
       and    BitRemoveTbl,y          ;4
       and    BitRemoveTbl+1,y        ;4
       sta    Marquee,x               ;4
       lda    PrintTemp+6             ;3
       eor    #$7F                    ;2
       lsr                            ;2
       lsr                            ;2
       tay                            ;2
       lda    #23                     ;2
       sec                            ;2
       sbc    PrintTemp+8             ;3
       sta    PrintTemp+7             ;3
       asl                            ;2
       asl                            ;2
       clc                            ;2
       adc    PrintTemp+7             ;3
       adc    ByteRemoveTbl,y         ;4
       tax                            ;2
       lda    Marquee,x               ;4
       and    BitRemoveTbl,y          ;4
       and    BitRemoveTbl+1,y        ;4
       sta    Marquee,x               ;4
SplashDisplayWait:
       lda    INTIM                   ;4
       bne    SplashDisplayWait       ;2³
       sta    WSYNC                   ;3
       sta    VBLANK                  ;3
;transfer title bitmap to playfield
       ldx    #23                     ;2
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       sta    WSYNC                   ;3
Display_Blocks:
       ldy    #$04                    ;2
Display_Lines:
       sty    LineCount               ;3
       lda    #$00                    ;2
       sta    PF0                     ;3
       sta    PF1                     ;3
       sta    PF2                     ;3
       lda    (PrintTemp+10),y        ;5 load explosion gfx for this block
       tay                            ;2
       lda    #$00                    ;2
       cpx    PrintTemp+9             ;3
       bne    No_Explosion            ;2³
       tya                            ;2
No_Explosion:
       sta    PrintTemp+7             ;3
       lda    #$00                    ;2
       cpx    PrintTemp+8             ;3
       bne    No_Explosion2           ;2³
       tya                            ;2
No_Explosion2:
       sta    GRP1                    ;3
       lda    PrintTemp+7             ;3
       sta    WSYNC                   ;3
       sta    GRP0                    ;3
       lda    Marquee,x               ;4
       sta    PF0                     ;3
       ldy    Marquee+24,x            ;4
       sty    PF1                     ;3
       ldy    Marquee+48,x            ;4
       sty    PF2                     ;3
       asl                            ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       sta    PF0                     ;3
       ldy    Marquee+72,x            ;4
       sty    PF1                     ;3
       ldy    Marquee+96,x            ;4
       sty    PF2                     ;3
       ldy    LineCount               ;3
       sta    WSYNC                   ;3
       dey                            ;2
       bne    Display_Lines           ;2³ @56/7
       cpx    #$0C                    ;2
       sty    PF0                     ;3
       sty    PF1                     ;3
       sty    PF2                     ;3
       beq    Gap                     ;2³
Next_Block:
       dex                            ;2
       bpl    Display_Blocks          ;2³
       inx                            ;2
       stx    GRP0                    ;3
       stx    GRP1                    ;3
       jmp    Print_Year              ;3
Gap:
       ldy    #$0C                    ;2
Display_Gap:
       sta    WSYNC                   ;3
       lda    #$00                    ;2
       sta    PF0                     ;3
       sta    GRP0                    ;3
       sta    GRP1                    ;3
       sta    PF1                     ;3
       sta    PF2                     ;3
       dey                            ;2
       bpl    Display_Gap             ;2³
       bmi    Next_Block              ;2³
Print_Year:
       stx    NUSIZ0                  ;3
       stx    NUSIZ1                  ;3
       inc    FrameCounter            ;5
       inc    FrameCounter            ;5
       lda    PrintTemp+11            ;3
       sec                            ;2
       sbc    #$40                    ;2
       ldx    #$44                    ;2 red
       stx    COLUP0                  ;3
       stx    COLUP1                  ;3
       stx    RESP0                   ;3
       stx    RESP1                   ;3
       stx    WSYNC                   ;3
       stx    WSYNC                   ;3
       ldx    #$07                    ;2
Display_Year_Loop:
       ldy    Year1,x                 ;4
       stx    WSYNC                   ;3
       sty    GRP0                    ;3
       ldy    Year2,x                 ;4
       sty    GRP1                    ;3
       dex                            ;2
       bpl    Display_Year_Loop       ;2³
       ldy    INPT5                   ;3
       bpl    Splash_Exit             ;2³
       ldy    FrameCounter            ;3
       bne    Splash_Continue         ;2³
       sta    PrintTemp+11            ;3
       bcc    Splash_Exit             ;2³
Splash_Continue:
       ldy    #$02                    ;2
       sty    VBLANK                  ;3
       ldy    #$0E                    ;2
Display_Gap2:
       sta    WSYNC                   ;3
       dey                            ;2
       bpl    Display_Gap2             ;2³
       jmp    Splash_Display          ;3 go back for another frame
Splash_Exit:
       lda    #$02                    ;2
       sta    VBLANK                  ;3
       lda    #$00                    ;2
       ldx    #119                    ;2
Ram_Clear:
       sta    Marquee,x               ;4
       dex                            ;2
       bpl    Ram_Clear               ;2³
       tsx                            ;2
       stx    rE6                     ;3
       ldx    #$FF                    ;2
       txs                            ;2
;reset cursor location to center
       lda    #$50                    ;2 reset crosshair to starting position
       sta    r90                     ;3 X
       lda    #$32                    ;2 reset crosshair to starting position
       sta    r8E                     ;3 Y
       ldy    #$87                    ;2
       sty    rDB                     ;3
       ldy    #$01                    ;2
       sty    rE9                     ;3
       iny                            ;2
       lda    #$00                    ;2
       jmp    Display_Done            ;3







;Ending screen...
Game_End:
       ldx    rDB                     ;3
       txs                            ;2
       ldx    #69                    ;2
Ram_Setup_End:
       lda    EndGfx,x                ;4
       sta    TheEnd,x                ;4
       dex                            ;2
       bpl    Ram_Setup_End           ;2³
       sty    WSYNC                   ;3
       inx                            ;2
       stx    FrameCounter            ;3

       stx    PrintTemp+9             ;3
       stx    PrintTemp+11            ;3

       ldy    #$01                    ;2
       sty    CTRLPF                  ;3
       ldy    #$0E                    ;2
       sty    COLUPF                  ;3
;       ldx    #$3F                    ;2
       ldx    #$3E                    ;2
       jmp    Skip_Vblank             ;3
Display_End_Top:
       ldy    #$02                    ;2
       sty    VBLANK                  ;3
       sty    VSYNC                   ;3
       sty    WSYNC                   ;3
       ldy    #$20                    ;2 247
       sty    WSYNC                   ;3
       stx    VSYNC                   ;3 x=0
       sty    TIM64T                  ;4
LF617a:
       ldy    #$08                    ;2
       sty    AUDC0                   ;3
       lda    FrameCounter            ;3
       and    #$0F                    ;2
       bne    LF633a                  ;2³
       inc    rE0                     ;5
       lda    rE0                     ;3
       cmp    #$10                    ;2 did it reach 16 clicks?
       bne    LF633a                  ;2³ do rising sound?
       lda    #$30                    ;2
       sta    rDF                     ;3
       ldx    #$50                    ;2
       stx    rE1                     ;3
       stx    AUDV0                   ;3
       sty    AUDC0                   ;3
       sta    AUDF0                   ;3
       bne    Explos                  ;2³
LF633a:
       ldx    rE0                     ;3
       stx    AUDV0                   ;3
       txa                            ;2
       sta    AUDF0                   ;3
       eor    #$FF                    ;2
       stx    AUDV1                   ;3
       sty    AUDC1                   ;3
       sta    AUDF1                   ;3
Explos:
       lda    FrameCounter            ;3
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       cmp    #$13                    ;2
       bcs    Red                     ;2³
       sta    PrintTemp+8             ;3
       lda    #$13                    ;2
       sec                            ;2
       sbc    PrintTemp+8             ;3 reverse (20 playfield pixel positions only)
       tay                            ;2
       ldx    BitRemoveTbl,y          ;4
       lda    ByteRemoveTbl,y         ;4
       asl                            ;2
       sta    PrintTemp+10            ;3
       asl                            ;2
       asl                            ;2
       asl                            ;2
       sec                            ;2
       sbc    PrintTemp+10            ;3 multiply by 14
       clc                            ;2
       adc    #TheEnd                 ;2 add starting location
       sta    PrintTemp+8             ;3 ...save as indirect address
       ldy    PrintTemp+10            ;3 Is it still on PF0?
       beq    dPF0                    ;2³ ...branch if so (and use identical address)
       clc                            ;2  otherwise, adjust for the right side
       adc    #28                     ;2
dPF0:
       sta    PrintTemp+10            ;3
       ldy    #13                     ;2 14 bytes each column
Swallow_Loop:
       txa                            ;2 get AND value this column
       and    (PrintTemp+8),y         ;5
       sta    (PrintTemp+8),y         ;5 ...and remove the corresponding PF bit
       txa                            ;2 (do the same for the right side)
       and    (PrintTemp+10),y        ;5
       sta    (PrintTemp+10),y        ;5
       dey                            ;2
       bpl    Swallow_Loop            ;2³
Skip_PF0:
       ldy    #$0E                    ;2
       lda    FrameCounter            ;3
       and    #$03                    ;2
       beq    Skip_Close              ;2³
Red:
       lda    rE6                     ;3
       and    #$01                    ;2
       tax                            ;2
       ldy    RedColor,x              ;4
Skip_Close:
       sty    COLUBK                  ;3
       ldx    #$42                    ;2
Display_End_Wait:
       lda    INTIM                   ;4
       bne    Display_End_Wait        ;2³
       sta    WSYNC                   ;3
       sta    VBLANK                  ;3
Skip_Vblank:
Waste_Lines_Top:
       sta    WSYNC                   ;3
       dex                            ;2
       bpl    Waste_Lines_Top         ;2³
       ldx    #13                     ;2
Display_Blocks_End:
       ldy    #$07                    ;2
Display_Lines_End:
       sta    WSYNC                   ;3
       sty    LineCount               ;3
       lda    TheEnd,x                ;4
       sta    PF0                     ;3
       ldy    TheEnd+14,x             ;4
       sty    PF1                     ;3
       ldy    TheEnd+28,x             ;4
       sty    PF2                     ;3
       dec    Waste2                  ;5
       dec    Waste2                  ;5
       nop                            ;2
       dec    LineCount               ;5
       ldy    TheEnd+56,x             ;4
       sty    PF2                     ;3
       ldy    TheEnd+42,x             ;4
       sty    PF1                     ;3
       sta    PF0                     ;3
       ldy    LineCount               ;3
       bne    Display_Lines_End       ;2³
       dex                            ;2
       bpl    Display_Blocks_End      ;2³
       inx                            ;2
       stx    PF0                     ;3
       stx    PF1                     ;3
       stx    PF2                     ;3
       dec    FrameCounter            ;5
       beq    End_Exit                ;2³
End_Continue:
       ldx    #$43                    ;2
Waste_Lines_Bottom:
       sta    WSYNC                   ;3
       dex                            ;2
       bne    Waste_Lines_Bottom      ;2³
       lda    #$02                    ;2
       sta    VBLANK                  ;3
       jmp    Display_End_Top         ;3
End_Exit:
       ldx    #$3A                    ;2
Waste_Lines_Bottom2:
       sta    WSYNC                   ;3
       dex                            ;2
       bne    Waste_Lines_Bottom2     ;2³
       ldy    #$02                    ;2
       sty    VBLANK                  ;3
       lda    #$00                    ;2
       ldx    #69                     ;2
Return_Ram_Clear:
       sta    TheEnd,x                ;4
       dex                            ;2
       bpl    Return_Ram_Clear        ;2³
       tsx                            ;2
       stx    rDB                     ;3
       ldx    #$FF                    ;2
       txs                            ;2
       stx    SatY                    ;3
       jsr    Expl                    ;6
       lda    #$00                    ;2
       jmp    Display_Done            ;3








;center of playfield while tally is in progress...
Counting_Playfield2:
       lda    #$57                    ;2 set upper delay timer
       sta    TIM64T                  ;4
       lda    #$01                    ;2 mirrored playfield
       sta    CTRLPF                  ;3
       and    rE6                     ;3
       tay                            ;2 Y = PAL/NTSC
       lda    CityColor,Y             ;4 set counted city color
       sta    COLUP0                  ;3
       sta    COLUP1                  ;3
       lda    ShotColor,Y             ;4 set counted missile color (uses PF gfx)
       sta    COLUPF                  ;3
       ldx    #$07                    ;2 missile count lines use 8 bytes of PrintTemp array
       ldy    #$00                    ;2
ClearLoop:
       sty    PrintTemp,x             ;4 ...clear each one to begin
       dex                            ;2
       bpl    ClearLoop               ;2³
       txs                            ;2 clear stack pointer (to use JSR's)
       inx                            ;2 clear X
       lda    CacheTotal              ;3
       beq    No_Missiles             ;2³ branch if nothing to display
;Begin count (Y) at zero and count upward.
McountLoop:
       cpy    #$0F                    ;2 15 missiles maximum each line
       bcs    Second_Set              ;2³ branch if dealing with 2nd set
;first set
       ldx    #$00                    ;2 PrintTemp offset = first set
       sec                            ;2  first line = bits first, then gaps
       jsr    Roll                    ;6
       clc                            ;2
       bcc    Roll2                   ;2³ always branch
Second_Set:
       ldx    #$04                    ;2 PrintTemp offset = second set
       clc                            ;2
       jsr    Roll                    ;6  second line = gaps first, then bits
       sec                            ;2
Roll2:
       jsr    Roll                    ;6
       iny                            ;2
       cpy    CacheTotal              ;3 all missiles displayed?
       bne    McountLoop              ;2³ loop if not
;adjust lower line of missiles...
       ldx    #$04                    ;2 PrintTemp offset = second set
       clc                            ;2
       jsr    Roll                    ;6 move second line to be between first line
       jsr    Roll                    ;6
No_Missiles:
CountTop:
       ldx    INTIM                   ;4
       bne    CountTop                ;2³
;X = 0...first set
       ldy    #$05                    ;2 6 scanlines
CountShotsLoop1:
       sta    WSYNC                   ;3
       jsr    Print_Missiles          ;6
       bpl    CountShotsLoop1         ;2³ @65/66 loop for the first line of missiles
       ldx    #$04                    ;2 @67   2nd set
       ldy    #$05                    ;2 @69   6 scanlines
CountShotsLoop2:
       sta    WSYNC                   ;3
       jsr    Print_Missiles          ;6
       bpl    CountShotsLoop2         ;2³ loop for the second line of missiles
       iny                            ;2
       ldx    #$0A                    ;2 set X=last sprite offset (low byte)
       stx    TIM64T                  ;4  ...also use for a delay timer (gap below missiles)
       sty    PF1                     ;3 clear the PF used for missiles
       sty    PF2                     ;3
       ldy    CityCount               ;3 get total number of cities remaining (if counting now)
CountSetupLoop:
       lda    #<Blank                 ;2 set blank gfx for this sprite
       dey                            ;2 reduce count
       bmi    NoCity                  ;2³ branch if this city (and subsequent cities) gone
       lda    #<CountGfx              ;2  ...otherwise, set to the smaller city gfx (w/gaps)
NoCity:
       sta    PrintTemp,x             ;4 store as current for this sprite
       lda    #>GFX                   ;2 ...and set the upper byte
       sta    PrintTemp+1,x           ;4 ..
       dex                            ;2
       dex                            ;2 bump down by 2's
       bpl    CountSetupLoop          ;2³ loop for all 6 sprites
CountMiddle:
       ldx    INTIM                   ;4  finish delay between missiles/cities
       bne    CountMiddle             ;2³
       ldy    #$07                    ;2 8 scanlines
;display counted cities...uses original score positioning @ 6 sprites close (still unmoved)
CountCitiesLoop:
       sty    LineCount               ;3
       lda    (PrintTemp+10),Y        ;5
       sta    GRP0                    ;3
       sta    WSYNC                   ;3
       lda    (PrintTemp+8),Y         ;5
       sta    GRP1                    ;3
       lda    (PrintTemp+6),Y         ;5
       sta    GRP0                    ;3
       lda    (PrintTemp+4),Y         ;5
       sta    CountTemp               ;3
       lda    (PrintTemp+2),Y         ;5
       tax                            ;2
       lda    (PrintTemp),Y           ;5
       tay                            ;2
       lda    CountTemp               ;3
       sta    GRP1                    ;3
       stx    GRP0                    ;3
       sty    GRP1                    ;3
       sty    GRP0                    ;3
       ldy    LineCount               ;3
       dey                            ;2
       bpl    CountCitiesLoop         ;2³ loop for 8 scanlines
       sty    CountTemp               ;3 reset SatY to none
       iny                            ;2
       sty    GRP1                    ;3
       sty    GRP0                    ;3
       sty    GRP1                    ;3
       sty    VDELP0                  ;3 bugfix...reset vertical delay for sprites
       sty    VDELP1                  ;3
       lda    #$57                    ;2 set lower delay timer
       sta    TIM64T                  ;4
       lda    CityPtr                 ;3 restore city pointers to temp array...
       sta    PrintTemp               ;3 ..
       lda    CityPtr+1               ;3 ..
       sta    PrintTemp+2             ;3 ..
       lda    CityPtr+2               ;3 ..
       sta    PrintTemp+4             ;3 ..
       lda    CityPtr+4               ;3 ..
       ldy    #$42                    ;2 upper line of sunset color
CountBottom:
       ldx    INTIM                   ;4  finish delay loop under counted cities
       bne    CountBottom             ;2³
       sta    WSYNC                   ;3
       sty    COLUBK                  ;3
       sta    PrintTemp+8             ;3 ..
       lda    CityPtr+5               ;3 ..
       sta    PrintTemp+10            ;3 ..
       lda    #>GFX                   ;2 ..
       sta    PrintTemp+3             ;3 (was shared w/temps)
       jmp    LF8D9                   ;3 x must be zero when exiting














;assorted subroutines...
LFA81:
       ldy    #$00                    ;2
       lda    r97                     ;3
       and    #$02                    ;2
       beq    LFA95                   ;2³
       lda    r98                     ;3
       and    #$02                    ;2
       bne    LFA95                   ;2³
       lda    rDF                     ;3
       and    #$F0                    ;2
       sta    rDF                     ;3
LFA95:
       sty    r97                     ;3
       bit    rDE                     ;3
       bmi    LFAA3                   ;2³
       lda    rDE                     ;3
       bne    LFAC5                   ;2³
       lda    rEB                     ;3
       bne    LFAA9                   ;2³
LFAA3:
       lda    #$54                    ;2
       sta    r93                     ;3
R14:
       sec                            ;2
R12:
       rts                            ;6


LFAA9:
       ldy    #$02                    ;2
       sty    rE0                     ;3
       ldy    #$FF                    ;2
       sty    r97                     ;3
       lda    FrameCounter            ;3
       and    #$01                    ;2
       tax                            ;2
       iny                            ;2
       sty    rF9,X                   ;4
       dec    rEB                     ;5
       lda    rDF                     ;3
       and    #$F0                    ;2
       ora    #$03                    ;2
       sta    rDF                     ;3
       bne    LFACF                   ;2³
LFAC5:
       lda    rD8                     ;3
       and    #$18                    ;2
       bne    LFACF                   ;2³
       lda    rEB                     ;3
       bne    LFAA9                   ;2³
LFACF:
       ldy    r95                     ;3
       lda    TargetTbl,Y             ;4
       sta    Temp1                   ;3
       bit    r97                     ;3
       bmi    LFAEB                   ;2³
       lda    rDE                     ;3
       cmp    #$04                    ;2
       bcs    LFAE7                   ;2³
       tay                            ;2
       lda    LFFCB,Y                 ;4
       bpl    LFAFA                   ;2³ always branch


LFAE7:
       cpy    #$06                    ;2
       bcc    LFAEF                   ;2³
LFAEB:
       lda    #$00                    ;2
       beq    LFAFA                   ;2³
LFAEF:
       lda    rD8                     ;3
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       and    #$07                    ;2
       tay                            ;2
       lda    LFF49,Y                 ;4
LFAFA:
       tay                            ;2
       sta    r91                     ;3
       lda    LFF9E,Y                 ;4
       sta    r83                     ;3
       lda    rD8                     ;3
       cmp    #$A0                    ;2
       bcc    LFB09                   ;2³
       lsr                            ;2
LFB09:
       clc                            ;2
       adc    r83                     ;3
       cmp    #$A0                    ;2
       bcc    LFB11                   ;2³
       lsr                            ;2
LFB11:
       sec                            ;2
       sbc    r83                     ;3
       sta    r99                     ;3
       cmp    Temp1                   ;3
       bcs    LFB4E                   ;2³
       clc                            ;2
       adc    r83                     ;3
       cmp    Temp1                   ;3
       bcc    LFB4A                   ;2³
       lda    Temp1                   ;3
       clc                            ;2
       adc    r83                     ;3
       cmp    #$A0                    ;2
       bcs    LFB34                   ;2³
       lda    r83                     ;3
       lsr                            ;2
       clc                            ;2
       adc    r99                     ;3
       cmp    Temp1                   ;3
       bcs    LFB43                   ;2³
LFB34:
       lda    Temp1                   ;3
       cmp    r83                     ;3
       bcc    LFB43                   ;2³
       ldx    #$10                    ;2
LFB3C:
       lda    r83                     ;3
       clc                            ;2
       adc    r99                     ;3
       bne    LFB52                   ;2³
LFB43:
       lda    r99                     ;3
       ldx    #$F0                    ;2
       bne    LFB52                   ;2³ always branch


LFB4A:
       ldx    #$F0                    ;2
       bne    LFB3C                   ;2³ always branch


LFB4E:
       ldx    #$10                    ;2
       lda    r99                     ;3
LFB52:
       stx    Temp2                   ;3
       tax                            ;2
       lda    r97                     ;3
       and    #$02                    ;2
       ora    Temp2                   ;3
       sta    r97                     ;3
       txa                            ;2
       ldy    #$53                    ;2
       sty    r93                     ;3
       sec                            ;2
       sbc    Temp1                   ;3
       bcs    LFB6B                   ;2³
       eor    #$FF                    ;2
       adc    #$01                    ;2
LFB6B:
       sty    r81                     ;3
       jsr    LFB7E                   ;6
       stx    r87                     ;3
       ldx    #$00                    ;2
       sta    Temp2                   ;3
       jsr    LFB82                   ;6
       stx    r86                     ;3
       jmp    LFB9C                   ;3
LFB7E:
       sta    Temp2                   ;3
       lda    #$00                    ;2
LFB82:
       ldy    #$07                    ;2 ???
LFB84:
       rol    Temp2                   ;5
       rol                            ;2
       bcs    LFB97                   ;2³
       cmp    r81                     ;3
       bcc    LFB8F                   ;2³
       sbc    r81                     ;3
LFB8F:
       dey                            ;2
       bpl    LFB84                   ;2³
       rol    Temp2                   ;5
       ldx    Temp2                   ;3
       rts                            ;6


LFB97:
       sbc    r81                     ;3
       sec                            ;2
       bcs    LFB8F                   ;2³
LFB9C:
       lda    r86                     ;3
       sta    r95                     ;3
       lda    r97                     ;3
       and    #$02                    ;2
       bne    LFBB0                   ;2³
       ldy    r91                     ;3
       lda    rDE                     ;3
       clc                            ;2
       sbc    LFF22,Y                 ;4
       sta    rDE                     ;3
LFBB0:
       clc                            ;2
       rts                            ;6














LFBB2:
       lda    r91                     ;3
       and    #$07                    ;2
       tay                            ;2
       lda    LFF22,Y                 ;4
       sta    Temp1                   ;3
       lda    LFFEF,Y                 ;4
       tay                            ;2
       lda    LFDE4,Y                 ;4
       sta    PrintTemp+6             ;3
       lda    LFDE5,Y                 ;4
       sta    PrintTemp+7             ;3
       lda    #$53                    ;2
       sta    r81                     ;3
       lda    r95                     ;3
       sta    Temp2                   ;3
       lda    #$00                    ;2
       sta    r87                     ;3
       ldx    #$08                    ;2
LFC7E:
       asl                            ;2
       rol    r87                     ;5
       asl    r81                     ;5
       bcc    LFC8C                   ;2³
       clc                            ;2
       adc    Temp2                   ;3
       bcc    LFC8C                   ;2³
       inc    r87                     ;5
LFC8C:
       dex                            ;2
       bne    LFC7E                   ;2³
       sta    r86                     ;3
LFBD5:
       ldy    Temp1                   ;3
       lda    r99                     ;3
       clc                            ;2
       adc    (PrintTemp+6),Y         ;5
       ldx    r97                     ;3
       cpx    #$F0                    ;2
       bcs    LFBE8                   ;2³
       sec                            ;2
       sbc    r87                     ;3
       jmp    LFBEB                   ;3

LFBE8:
       clc                            ;2
       adc    r87                     ;3
LFBEB:
       sta    r81                     ;3
       ldy    #$08                    ;2
CheckHitLoop:
       lda    TargetTbl,Y             ;4
       sec                            ;2
       sbc    #$04                    ;2
       cmp    r81                     ;3
       bcs    NothingHit              ;2³
       adc    #$08                    ;2
       cmp    r81                     ;3
       bcs    CheckRightSilo          ;2³
NothingHit:
       dey                            ;2
       bpl    CheckHitLoop            ;2³
       bmi    LFC3F                   ;2³ always branch


NoSilosHit:
       tya                            ;2
       tax                            ;2
       ldy    #<Nuke2                 ;2 mushroom cloud only
       lda    CityPtr,X               ;4
       bne    LFC3D                   ;2³ branch if not a city
       ldy    #<Nuke                  ;2 start on earlier frames (small city gfx + inverse)
       lda    rDF                     ;3 otherwise, nuke the city
       and    #$0F                    ;2
       ora    #$40                    ;2
       sta    rDF                     ;3
       lda    #$50                    ;2
       sta    rE1                     ;3
LFC3D:
       sty    CityPtr,X               ;4 save as current pointer this city
LFC3F:
       dec    Temp1                   ;5
       bpl    LFBD5                   ;2³ <-
       lda    #$FF                    ;2
       sta    r91                     ;3
       lda    #$55                    ;2
       sta    r93                     ;3
       rts                            ;6


;new silos
CheckRightSilo:
       lda    #$00                    ;2
       cpy    #$06                    ;2
       bne    CheckCenterSilo         ;2³
       ldy    CacheCount              ;3
       sta    CacheCount              ;3
       bpl    AllSilos                ;2³ always branch
CheckCenterSilo:
       cpy    #$07                    ;2
       bne    CheckLeftSilo           ;2³
       ldy    CacheCount+2            ;3
       sta    CacheCount+2            ;3
       bpl    AllSilos                ;2³ always branch
CheckLeftSilo:
       cpy    #$08                    ;2
       bne    NoSilosHit              ;2³
       ldy    CacheCount+1            ;3
       sta    CacheCount+1            ;3
AllSilos:
       lda    #$20                    ;2
       sta    rDC                     ;3
       tya                            ;2
       beq    NoExplosionSound        ;2³
;set explosion sound
       lda    rDF                     ;3
       and    #$0F                    ;2
       ora    #$30                    ;2
       sta    rDF                     ;3
NoExplosionSound:
       lda    #$50                    ;2
       sta    rE1                     ;3
       bne    LFC3F                   ;2³ always branch





LFCF4:
       ldx    #$00                    ;2
;reset counters
       stx    CacheTotal              ;3
       stx    CityCount               ;3
       txa                            ;2
LFCF7:
       ldy    CityPtr,X               ;4
       sec                            ;2
       cpy    #<CityGFX               ;2
       beq    LFCFF                   ;2³
       clc                            ;2
LFCFF:
       rol                            ;2
       inx                            ;2
       cpx    #$06                    ;2
       bne    LFCF7                   ;2³
       ldx    rF5                     ;3
       sta    rEC,X                   ;4
       ldx    #$FF                    ;2
       stx    rDE                     ;3
       inx                            ;2
       stx    rE0                     ;3
       rts                            ;6


LFCCBB:
       sta    Temp2                   ;3
       ldx    #$05                    ;2
       lda    rE7                     ;3
       cmp    #$0C                    ;2
       bcs    LFCD77                  ;2³
       lsr                            ;2
       tax                            ;2
LFCD77:
       stx    Temp1                   ;3
LFCD99:
       ldx    rF5                     ;3
       lda    Temp2                   ;3
       sed                            ;2
       clc                            ;2
       adc    rEF,X                   ;4
       sta    rEF,X                   ;4
       tya                            ;2
       adc    rF1,X                   ;4
       sta    rF1,X                   ;4
       lda    #$00                    ;2
       adc    rF3,X                   ;4
       sta    rF3,X                   ;4
       cld                            ;2
       dec    Temp1                   ;5
       bpl    LFCD99                  ;2³
Expl:
       lda    rDF                     ;3
       cmp    #$30                    ;2
       bcs    LFA3C                   ;2³
       and    #$0F                    ;2
       ora    #$20                    ;2
       sta    rDF                     ;3
       lda    #$00                    ;2
       sta    rE1                     ;3
LFA3C:
       rts                            ;6



       ORG  $2B81
       RORG $FB81

Year1:
       .byte %00000000 ; |        | $FF22
       .byte %01110111 ; |        | $FF22
       .byte %01000101 ; |        | $FF22
       .byte %01000101 ; |        | $FF22
       .byte %01110101 ; |        | $FF22
       .byte %00010101 ; |        | $FF22
       .byte %00010101 ; |        | $FF22
       .byte %01110111 ; |        | $FF22

Year2:
       .byte %00000000 ; |        | $FF22
       .byte %11101110 ; |        | $FF22
       .byte %10101010 ; |        | $FF22
       .byte %10101010 ; |        | $FF22
       .byte %10101110 ; |        | $FF22
       .byte %10101000 ; |        | $FF22
       .byte %10101000 ; |        | $FF22
       .byte %11101110 ; |        | $FF22

DigitTbl:
       .byte <Zero
       .byte <One
       .byte <Two
       .byte <Three
       .byte <Four
       .byte <Five
       .byte <Six
       .byte <Seven
       .byte <Eight
       .byte <Nine

CachePos:
       .byte $0B ; |        | $FFB3
       .byte $02 ; |        | $FFB3
       .byte $07 ; |        | $FFB3

SatColor:
       .byte $80 ; |X  X    | $FFF6 purple PAL
       .byte $50 ; | X X    | $FFF7 purple NTSC

ShotColor:
       .byte $9A ; |X  XX X | $FFF6 Cyan PAL
       .byte $AA ; |X X X X | $FFF6 Cyan NTSC



LFF22: ;9 bytes?
       .byte $00 ; |        | $FF22
       .byte $01 ; |       X| $FF23
       .byte $01 ; |       X| $FF24
       .byte $02 ; |      X | $FF25
       .byte $01 ; |       X| $FF26
       .byte $00 ; |        | $FF27
       .byte $02 ; |      X | $FF28
       .byte $01 ; |       X| $FF29
ByteRemoveTbl:
       .byte $00
       .byte $00
       .byte $00
       .byte $00

       .byte $01
       .byte $01
       .byte $01
       .byte $01
       .byte $01
       .byte $01
       .byte $01
       .byte $01

       .byte $02
       .byte $02
       .byte $02
       .byte $02
       .byte $02
       .byte $02
       .byte $02
       .byte $02

       .byte $00
       .byte $00
       .byte $00
       .byte $00

       .byte $03
       .byte $03
       .byte $03
       .byte $03
       .byte $03
       .byte $03
       .byte $03
       .byte $03

       .byte $04
       .byte $04
       .byte $04
       .byte $04
       .byte $04
       .byte $04
       .byte $04
       .byte $04


;Cache GFX pointers (in page $FFxx)
CacheGFX: ;left half
       .byte <Count0a  ; $FFDC during level...
       .byte <Count1a  ; $FFDD
       .byte <Count2a  ; $FFDE
       .byte <Count3a  ; $FFDF
       .byte <Count4a  ; $FFE0
       .byte <Count5a  ; $FFE1
       .byte <Count6a  ; $FFE2
       .byte <Count7a  ; $FFE3
       .byte <Count8a  ; $FFE4
       .byte <Count9a  ; $FFE5
       .byte <Count10a ; $FFE6
       .byte <Count00  ; $FFE6 end of level...
       .byte <Count1aa ; $FFDD
       .byte <Count2aa ; $FFDE
       .byte <Count3aa ; $FFDF
       .byte <Count4a  ; $FFE0
       .byte <Count5a  ; $FFE1
       .byte <Count6a  ; $FFE2
       .byte <Count7a  ; $FFE3
       .byte <Count8a  ; $FFE4
       .byte <Count9a  ; $FFE5
       .byte <Count10a ; $FFE6
       .byte <Count00  ; $FFE6

CacheGFX2: ;right half
       .byte <Count0b  ; $FFDC during level...
       .byte <Count1b  ; $FFDD
       .byte <Count2b  ; $FFDE
       .byte <Count3b  ; $FFDF
       .byte <Count4b  ; $FFE0
       .byte <Count5b  ; $FFE1
       .byte <Count6b  ; $FFE2
       .byte <Count7b  ; $FFE3
       .byte <Count8b  ; $FFE4
       .byte <Count9b  ; $FFE5
       .byte <Count10b ; $FFE6
       .byte <Count00  ; $FFDC end of level...
       .byte <Count1bb ; $FFDD
       .byte <Count2bb ; $FFDE
       .byte <Count3bb ; $FFDF
       .byte <Count4b  ; $FFE0
       .byte <Count5b  ; $FFE1
       .byte <Count6b  ; $FFE2
       .byte <Count7b  ; $FFE3
       .byte <Count8b  ; $FFE4
       .byte <Count9b  ; $FFE5
       .byte <Count10b ; $FFE6
       .byte <Count00  ; $FFE6

       ORG  $2C00
       RORG $FC00

MarqueeData:
;PF0 (lower nybble = PF0 on right side of screen)
       .byte %11001010 ; 25
       .byte %11101010 ; 24
       .byte %00101010 ; 23
       .byte %00111010 ; 22
       .byte %00011010 ; 21
       .byte %00011010 ; 20
       .byte %00011010 ; 19
       .byte %00011111 ; 18
       .byte %00111101 ; 17
       .byte %00101101 ; 16
       .byte %11101000 ; 15
       .byte %11001000 ; 14

       .byte %00001001 ; 11
       .byte %00001011 ; 10
       .byte %00000010 ; 9
       .byte %00000010 ; 8
       .byte %00000010 ; 7
       .byte %00000011 ; 6
       .byte %00000001 ; 5
       .byte %00000000 ; 4
       .byte %00000000 ; 3
       .byte %00000010 ; 2
       .byte %00001011 ; 1
       .byte %00001001 ; 0

;PF1 left half
       .byte %00001100 ; 25
       .byte %10011110 ; 24
       .byte %10010010 ; 23
       .byte %00110011 ; 22
       .byte %00100001 ; 21
       .byte %00100001 ; 20
       .byte %00100001 ; 19
       .byte %00100001 ; 18
       .byte %00110011 ; 17
       .byte %10010010 ; 16
       .byte %10011110 ; 15
       .byte %00001100 ; 14

       .byte %01010101 ; 11
       .byte %01010101 ; 10
       .byte %01010100 ; 9
       .byte %01010100 ; 8
       .byte %01010100 ; 7
       .byte %01010100 ; 6
       .byte %01010100 ; 5
       .byte %01111100 ; 4
       .byte %01101100 ; 3
       .byte %01101100 ; 2
       .byte %01000101 ; 1
       .byte %01000101 ; 0

;PF2 left half
       .byte %10101010 ; 25
       .byte %10101010 ; 24
       .byte %10101010 ; 23
       .byte %10101010 ; 22
       .byte %10101010 ; 21
       .byte %10101010 ; 20
       .byte %10101010 ; 19
       .byte %10111110 ; 18
       .byte %10110110 ; 17
       .byte %10110110 ; 16
       .byte %10100010 ; 15
       .byte %10100010 ; 14

       .byte %00010011 ; 11
       .byte %10111011 ; 10
       .byte %10101001 ; 9
       .byte %00100001 ; 8
       .byte %00100001 ; 7
       .byte %00110001 ; 6
       .byte %10011001 ; 5
       .byte %10001001 ; 4
       .byte %10001001 ; 3
       .byte %10101001 ; 2
       .byte %10111011 ; 1
       .byte %00010011 ; 0

;PF1 right half
       .byte %01001010 ; 25
       .byte %01001010 ; 24
       .byte %01001010 ; 23
       .byte %01001010 ; 22
       .byte %01111010 ; 21
       .byte %01111010 ; 20
       .byte %01001010 ; 19
       .byte %01001011 ; 18
       .byte %01001011 ; 17
       .byte %01001011 ; 16
       .byte %01111010 ; 15
       .byte %00110010 ; 14

       .byte %11011101 ; 11
       .byte %11011101 ; 10
       .byte %10010001 ; 9
       .byte %10010001 ; 8
       .byte %10010001 ; 7
       .byte %10010001 ; 6
       .byte %10010001 ; 5
       .byte %10010001 ; 4
       .byte %10010001 ; 3
       .byte %10010001 ; 2
       .byte %11010001 ; 1
       .byte %11010001 ; 0

;PF2 right half
       .byte %00011010 ; 25
       .byte %00111010 ; 24
       .byte %00101010 ; 23
       .byte %01101010 ; 22
       .byte %01001010 ; 21
       .byte %01001011 ; 20
       .byte %01001011 ; 19
       .byte %01001011 ; 18
       .byte %01101010 ; 17
       .byte %00101010 ; 16
       .byte %00111010 ; 15
       .byte %00011010 ; 14
EndGfx:
;PF0 shared
       .byte %00000011 ; 11/13 Shared
       .byte %00000011 ; 10/12 Shared
       .byte %00000000 ; 9/11  Shared
       .byte %00000000 ; 8/10  Shared
       .byte %00000000 ; 7/9   Shared
       .byte %00000001 ; 6/8   Shared
       .byte %00000001 ; 5/7   Shared
       .byte %00000000 ; 4/6   Shared
       .byte %00000000 ; 3/5   Shared
       .byte %00000000 ; 2/4   Shared
       .byte %00000011 ; 1/3   Shared
       .byte %00000011 ; 0/2   Shared
       .byte %00000000 ; 1
       .byte %00000000 ; 0

;PF1 left side
       .byte %00100010 ; 13
       .byte %00100010 ; 12
       .byte %00100010 ; 11
       .byte %00100010 ; 10
       .byte %00100010 ; 9
       .byte %00100010 ; 8
       .byte %00100011 ; 7
       .byte %00100011 ; 6
       .byte %00100010 ; 5
       .byte %00100010 ; 4
       .byte %00100010 ; 3
       .byte %00100010 ; 2
       .byte %11111010 ; 1
       .byte %11111010 ; 0

;PF2 left side
       .byte %01111010 ; 13
       .byte %01111010 ; 12
       .byte %00001010 ; 11
       .byte %00001010 ; 10
       .byte %00001010 ; 9
       .byte %00001010 ; 8
       .byte %00111011 ; 7
       .byte %00111011 ; 6
       .byte %00001010 ; 5
       .byte %00001010 ; 4
       .byte %00001010 ; 3
       .byte %00001010 ; 2
       .byte %01111010 ; 1
       .byte %01111010 ; 0

;PF1 right side
       .byte %00110100 ; 13
       .byte %01110100 ; 12
       .byte %01010100 ; 11
       .byte %11010100 ; 10
       .byte %10010100 ; 9
       .byte %10010100 ; 8
       .byte %10010100 ; 7
       .byte %10010110 ; 6
       .byte %10010110 ; 5
       .byte %10010111 ; 4
       .byte %11010101 ; 3
       .byte %01010101 ; 2
       .byte %01110100 ; 1
       .byte %00110100 ; 0

;PF2 right side
       .byte %00111101 ; 13
       .byte %00111101 ; 12
       .byte %00100001 ; 11
       .byte %00100001 ; 10
       .byte %00100001 ; 9
       .byte %00100001 ; 8
       .byte %00111001 ; 7
       .byte %00111001 ; 6
       .byte %00100001 ; 5
       .byte %00100001 ; 4
       .byte %00100001 ; 3
       .byte %00100001 ; 2
       .byte %00111101 ; 1
       .byte %00111101 ; 0


BitRemoveTbl:
       .byte $EF
       .byte $DF
       .byte $BF
       .byte $7F

       .byte $7F
       .byte $BF
       .byte $DF
       .byte $EF
       .byte $F7
       .byte $FB
       .byte $FD
       .byte $FE

       .byte $FE
       .byte $FD
       .byte $FB
       .byte $F7
       .byte $EF
       .byte $DF
       .byte $BF
       .byte $7F

       .byte $FE
       .byte $FD
       .byte $FB
       .byte $F7

       .byte $7F
       .byte $BF
       .byte $DF
       .byte $EF
       .byte $F7
       .byte $FB
       .byte $FD
       .byte $FE

       .byte $FE
       .byte $FD
       .byte $FB
       .byte $F7
       .byte $EF
       .byte $DF
       .byte $BF
       .byte $7F



PlayerUpTxtTbl: ;11
       .byte >One      ; $FDD5
       .word PlayerUp5 ; $FDD6/7
       .word PlayerUp4 ; $FDD8/9
       .word PlayerUp3 ; $FDDA/B
       .word PlayerUp2 ; $FDDC/D
       .word PlayerUp1 ; $FDDE/F
       .byte >Paused6  ; $FDD5
       .word Paused5   ; $FDD6/7
       .word Paused4   ; $FDD6/7
       .word Paused3   ; $FDD6/7
       .word Paused2   ; $FDD6/7
       .word Paused1   ; $FDD6/7

ScoreColor:
       .byte $40 ; | X      | $FFA4 red PAL
       .byte $50 ; | X X    | $FEC6 green PAL
       .byte $30 ; |  XX    | $FEC7 red NTSC
       .byte $C0 ; |XX      | $FEC8 green NTSC

   IF PLUSROM
;missile targets
TargetTbl:
       .byte $18 ; |   XX   | $FF00 city1
       .byte $28 ; |  X X   | $FF01 city2
       .byte $38 ; |  XXX   | $FF02 city3
       .byte $6A ; | XX X X | $FF03 city4
       .byte $7A ; | XXXX X | $FF04 city5
       .byte $8A ; |X   X X | $FF05 city6
       .byte $9A ; |X  XX X | $FF06 silo
       .byte $50 ; | X X    | $FF07 silo
       .byte $10 ; |   X    | $FF07 silo
   ENDIF

       ORG  $2D00
       RORG $FD00

XposTbl:
       .byte $4B ; | X  X X | $DCB6 NOTE: positioning past end of scanline (to hit earlier)
       .byte $3B ; |  XXX X | $DCB7 ""
       .byte $2B ; |  X X X | $DCB8 ""
       .byte $1B ; |   XX  X| $DCAA ""
       .byte $0B ; |    X  X| $DCAB ""
       .byte $FB ; |XXXXX  X| $DCAC ""
       .byte $EB ; |XXX X  X| $DCAD ""
       .byte $DB ; |XX XX  X| $DCAE ""
       .byte $CB ; |XX  X  X| $DCAF ""
       .byte $61 ; | XX   X | $DC3C
       .byte $51 ; | X X  X | $DC3D
       .byte $41 ; | X    X | $DC3E
       .byte $31 ; |  XX  X | $DC3F
       .byte $21 ; |  X   X | $DC40
       .byte $11 ; |   X  X | $DC41
       .byte $01 ; |      X | $DC42
       .byte $F1 ; |XXXX   X| $DC34
       .byte $E1 ; |XXX    X| $DC35
       .byte $D1 ; |XX X   X| $DC36
       .byte $C1 ; |XX     X| $DC37
       .byte $B1 ; |X XX   X| $DC38
       .byte $A1 ; |X X    X| $DC39
       .byte $91 ; |X  X   X| $DC3A
       .byte $81 ; |X      X| $DC3B
       .byte $62 ; | XX   X | $DC3C
       .byte $52 ; | X X  X | $DC3D
       .byte $42 ; | X    X | $DC3E
       .byte $32 ; |  XX  X | $DC3F
       .byte $22 ; |  X   X | $DC40
       .byte $12 ; |   X  X | $DC41
       .byte $02 ; |      X | $DC42
       .byte $F2 ; |XXXX  X | $DC43
       .byte $E2 ; |XXX   X | $DC44
       .byte $D2 ; |XX X  X | $DC45
       .byte $C2 ; |XX    X | $DC46
       .byte $B2 ; |X XX  X | $DC47
       .byte $A2 ; |X X   X | $DC48
       .byte $92 ; |X  X  X | $DC49
       .byte $82 ; |X     X | $DC4A
       .byte $63 ; | XX   XX| $DC4B
       .byte $53 ; | X X  XX| $DC4C
       .byte $43 ; | X    XX| $DC4D
       .byte $33 ; |  XX  XX| $DC4E
       .byte $23 ; |  X   XX| $DC4F
       .byte $13 ; |   X  XX| $DC50
       .byte $03 ; |      XX| $DC51
       .byte $F3 ; |XXXX  XX| $DC52
       .byte $E3 ; |XXX   XX| $DC53
       .byte $D3 ; |XX X  XX| $DC54
       .byte $C3 ; |XX    XX| $DC55
       .byte $B3 ; |X XX  XX| $DC56
       .byte $A3 ; |X X   XX| $DC57
       .byte $93 ; |X  X  XX| $DC58
       .byte $83 ; |X     XX| $DC59
       .byte $64 ; | XX  X  | $DC5A
       .byte $54 ; | X X X  | $DC5B
       .byte $44 ; | X   X  | $DC5C
       .byte $34 ; |  XX X  | $DC5D
       .byte $24 ; |  X  X  | $DC5E
       .byte $14 ; |   X X  | $DC5F
       .byte $04 ; |     X  | $DC60
       .byte $F4 ; |XXXX X  | $DC61
       .byte $E4 ; |XXX  X  | $DC62
       .byte $D4 ; |XX X X  | $DC63
       .byte $C4 ; |XX   X  | $DC64
       .byte $B4 ; |X XX X  | $DC65
       .byte $A4 ; |X X  X  | $DC66
       .byte $94 ; |X  X X  | $DC67
       .byte $84 ; |X    X  | $DC68
       .byte $65 ; | XX  X X| $DC69
       .byte $55 ; | X X X X| $DC6A
       .byte $45 ; | X   X X| $DC6B
       .byte $35 ; |  XX X X| $DC6C
       .byte $25 ; |  X  X X| $DC6D
       .byte $15 ; |   X X X| $DC6E
       .byte $05 ; |     X X| $DC6F
       .byte $F5 ; |XXXX X X| $DC70
       .byte $E5 ; |XXX  X X| $DC71
       .byte $D5 ; |XX X X X| $DC72
       .byte $C5 ; |XX   X X| $DC73
       .byte $B5 ; |X XX X X| $DC74
       .byte $A5 ; |X X  X X| $DC75
       .byte $95 ; |X  X X X| $DC76
       .byte $85 ; |X    X X| $DC77
       .byte $66 ; | XX  XX | $DC78
       .byte $56 ; | X X XX | $DC79
       .byte $46 ; | X   XX | $DC7A
       .byte $36 ; |  XX XX | $DC7B
       .byte $26 ; |  X  XX | $DC7C
       .byte $16 ; |   X XX | $DC7D
       .byte $06 ; |     XX | $DC7E
       .byte $F6 ; |XXXX XX | $DC7F
       .byte $E6 ; |XXX  XX | $DC80
       .byte $D6 ; |XX X XX | $DC81
       .byte $C6 ; |XX   XX | $DC82
       .byte $B6 ; |X XX XX | $DC83
       .byte $A6 ; |X X  XX | $DC84
       .byte $96 ; |X  X XX | $DC85
       .byte $86 ; |X    XX | $DC86
       .byte $67 ; | XX  XXX| $DC87
       .byte $57 ; | X X XXX| $DC88
       .byte $47 ; | X   XXX| $DC89
       .byte $37 ; |  XX XXX| $DC8A
       .byte $27 ; |  X  XXX| $DC8B
       .byte $17 ; |   X XXX| $DC8C
       .byte $07 ; |     XXX| $DC8D
       .byte $F7 ; |XXXX XXX| $DC8E
       .byte $E7 ; |XXX  XXX| $DC8F
       .byte $D7 ; |XX X XXX| $DC90
       .byte $C7 ; |XX   XXX| $DC91
       .byte $B7 ; |X XX XXX| $DC92
       .byte $A7 ; |X X  XXX| $DC93
       .byte $97 ; |X  X XXX| $DC94
       .byte $87 ; |X    XXX| $DC95
       .byte $68 ; | XX X   | $DC96
       .byte $58 ; | X XX   | $DC97
       .byte $48 ; | X  X   | $DC98
       .byte $38 ; |  XXX   | $DC99
       .byte $28 ; |  X X   | $DC9A
       .byte $18 ; |   XX   | $DC9B
       .byte $08 ; |    X   | $DC9C
       .byte $F8 ; |XXXXX   | $DC9D
       .byte $E8 ; |XXX X   | $DC9E
       .byte $D8 ; |XX XX   | $DC9F
       .byte $C8 ; |XX  X   | $DCA0
       .byte $B8 ; |X XXX   | $DCA1
       .byte $A8 ; |X X X   | $DCA2
       .byte $98 ; |X  XX   | $DCA3
       .byte $88 ; |X   X   | $DCA4
       .byte $69 ; | XX X  X| $DCA5
       .byte $59 ; | X XX  X| $DCA6
       .byte $49 ; | X  X  X| $DCA7
       .byte $39 ; |  XXX  X| $DCA8
       .byte $29 ; |  X X  X| $DCA9
       .byte $19 ; |   XX  X| $DCAA
       .byte $09 ; |    X  X| $DCAB
       .byte $F9 ; |XXXXX  X| $DCAC
       .byte $E9 ; |XXX X  X| $DCAD
       .byte $D9 ; |XX XX  X| $DCAE
       .byte $C9 ; |XX  X  X| $DCAF
       .byte $B9 ; |X XXX  X| $DCB0
       .byte $A9 ; |X X X  X| $DCB1
       .byte $99 ; |X  XX  X| $DCB2
       .byte $89 ; |X   X  X| $DCB3
       .byte $6A ; | XX X X | $DCB4
       .byte $5A ; | X XX X | $DCB5
       .byte $4A ; | X  X X | $DCB6
       .byte $3A ; |  XXX X | $DCB7
       .byte $2A ; |  X X X | $DCB8
       .byte $1A ; |   XX  X| $DCAA
       .byte $0A ; |    X  X| $DCAB
       .byte $FA ; |XXXXX  X| $DCAC
       .byte $EA ; |XXX X  X| $DCAD
       .byte $DA ; |XXX X  X| $DCAD


LFDE4: ;12
LFDE5 = LFDE4+1
       .word LFFA5 ; $FDE4/5
       .word LFFA6 ; $FDE6/7
       .word LFFA8 ; $FDE8/9
       .word LFFAB ; $FDEA/B
       .word LFFAD ; $FDEC/D
       .word LFFB0 ; $FDEE/F

LFF3A: ;16
       .byte $01 ; |       X| $FF3A
       .byte $02 ; |      X | $FF3B
       .byte $03 ; |      XX| $FF3C
       .byte $04 ; |     X  | $FF3D
       .byte $02 ; |      X | $FF3E
       .byte $04 ; |     X  | $FF3F
       .byte $06 ; |     XX | $FF40
       .byte $08 ; |    X   | $FF41
       .byte $06 ; |     XX | $FF42
       .byte $04 ; |     X  | $FF43
       .byte $02 ; |      X | $FF44
       .byte $04 ; |     X  | $FF45
       .byte $03 ; |      XX| $FF46
       .byte $02 ; |      X | $FF47
       .byte $01 ; |       X| $FF48
LFFA5: ;28
       .byte $00 ; |        | $FFA5 shared
LFFA6: ;28
       .byte $00 ; |        | $FFA6 shared
       .byte $10 ; |   X    | $FFA7 shared
LFFA8: ;28
       .byte $00 ; |        | $FFA8 shared
       .byte $10 ; |   X    | $FFA9
       .byte $20 ; |  X     | $FFAA
LFFAB: ;28
       .byte $00 ; |        | $FFAB
       .byte $20 ; |  X     | $FFAC
LFFAD: ;28
       .byte $00 ; |        | $FFAD shared
       .byte $20 ; |  X     | $FFAE
       .byte $40 ; | X      | $FFAF
LFFB0: ;28
       .byte $00 ; |        | $FFB0
       .byte $40 ; | X      | $FFB1
       .byte $FF ; |XXXXXXXX| $FFB2
       .byte $00 ; |        | $FFB3
       .byte $00 ; |        | $FFB4
       .byte $01 ; |       X| $FFB5
       .byte $02 ; |      X | $FFB6
       .byte $01 ; |       X| $FFB7
       .byte $00 ; |        | $FFB8
       .byte $00 ; |        | $FFB9
       .byte $02 ; |      X | $FFBA
       .byte $04 ; |     X  | $FFBB
       .byte $02 ; |      X | $FFBC
       .byte $00 ; |        | $FFBD
       .byte $00 ; |        | $FFBE
       .byte $00 ; |        | $FFBF
       .byte $10 ; |   X    | $FFC0
       .byte $00 ; |        | $FFC1
       .byte $10 ; |   X    | $FFC2
       .byte $00 ; |        | $FFC3
       .byte $00 ; |        | $FFC4
       .byte $20 ; |  X     | $FFC5
       .byte $00 ; |        | $FFC6
       .byte $20 ; |  X     | $FFC7
       .byte $00 ; |        | $FFC8
       .byte $00 ; |        | $FFC9
       .byte $40 ; | X      | $FFCA
LFF9E: ;7
       .byte $00 ; |        | $FF9E shared
       .byte $10 ; |   X    | $FF9F
       .byte $20 ; |  X     | $FFA0
       .byte $20 ; |  X     | $FFA1
       .byte $40 ; | X      | $FFA2
       .byte $00 ; |        | $FFA3
       .byte $40 ; | X      | $FFA2


GroundColor: ;12
       .byte $2A ; |  X X X | $FFB3 shades of brown...both palettes
       .byte $28 ; |  X X   | $FFB3
       .byte $26 ; |  X  XX | $FFB3
       .byte $24 ; |  X  X  | $FFB3
       .byte $22 ; |  X   X | $FFB3
       .byte $20 ; |  X     | $FFB3
       .byte $26 ; |  X  XX | $FFB3
       .byte $24 ; |  X  X  | $FFB3
       .byte $22 ; |  X   X | $FFB3
       .byte $20 ; |  X     | $FFB3
       .byte $24 ; |  X  X  | $FFB3
       .byte $22 ; |  X   X | $FFB3


HillGFX:
       .byte %11110000 ; |XXXX    | $FFD5
       .byte %11110000 ; |XXXX    | $FFD5
       .byte %11110000 ; |XXXX    | $FFD6
       .byte %11100000 ; |XXX     | $FFD7
       .byte %11100000 ; |XXX     | $FFD8
       .byte %11100000 ; |XXX     | $FFD9
       .byte %11000000 ; |XX      | $FFDA
       .byte %11000000 ; |XX      | $FFDB
       .byte %10000000 ; |X       | $FFDB

HillGFX2:
       .byte %11110000 ; |XXXX    | $FFE7
       .byte %11110000 ; |XXXX    | $FFE7
       .byte %11110000 ; |XXXX    | $FFE8
       .byte %01110000 ; | XXX    | $FFE9
       .byte %01110000 ; | XXX    | $FFEA
       .byte %01110000 ; | XXX    | $FFEB
       .byte %00110000 ; |  XX    | $FFEC
       .byte %00110000 ; |  XX    | $FFED
       .byte %00010000 ; |   X    | $FFEE


       ORG  $2E00
       RORG $FE00

GFX:
CityGFX: ;must be @ $FE00
       .byte %11111111 ; |XXXXXXXX| $FE00
       .byte %11111111 ; |XXXXXXXX| $FE01
       .byte %11111111 ; |XXXXXXXX| $FE02
       .byte %01111110 ; | XXXXXX | $FE03
       .byte %01111110 ; | XXXXXX | $FE04
       .byte %01110110 ; | XXX XX | $FE05
       .byte %00110110 ; |  XX XX | $FE06
       .byte %00100110 ; |  X  XX | $FE07
;each bitmap must be 9 bytes spaced evenly
Nuke:
       .byte %00100010 ; |  X   X | $FE08 shared
CountGfx:
       .byte %01111110 ; | XXXXXX | $FE01
       .byte %01111110 ; | XXXXXX | $FE02
       .byte %00111100 ; |  XXXX  | $FE03
       .byte %00111100 ; |  XXXX  | $FE04
       .byte %00110100 ; |  XX X  | $FE05
       .byte %00010100 ; |   X X  | $FE06
       .byte %00010100 ; |   X X  | $FE07
       .byte %00010000 ; |   X    | $FE08

       .byte %11111111 ; |XXXXXXXX| $FE00
       .byte %10000001 ; |X      X| $FE01
       .byte %10000001 ; |X      X| $FE02
       .byte %01000010 ; | X    X | $FE03
       .byte %01001010 ; | X  X X | $FE04
       .byte %01010110 ; | X X XX | $FE05
       .byte %00110110 ; |  XX XX | $FE06
       .byte %00100110 ; |  X  XX | $FE07
       .byte %00100010 ; |  X   X | $FE08

Nuke2:
       .byte %11111111 ; |XXXXXXXX| $FE4E
       .byte %11111111 ; |XXXXXXXX| $FE4F
       .byte %11111111 ; |XXXXXXXX| $FE50
       .byte %01111110 ; | XXXXXX | $FE51
       .byte %11111111 ; |XXXXXXXX| $FE52
       .byte %11111111 ; |XXXXXXXX| $FE53
       .byte %01111110 ; | XXXXXX | $FE54
       .byte %01111110 ; | XXXXXX | $FE55
       .byte %00000000 ; |        | $FE56

       .byte %11111111 ; |XXXXXXXX| $FE5A
       .byte %11111111 ; |XXXXXXXX| $FE5B
       .byte %00111100 ; |  XXXX  | $FE5C
       .byte %00111100 ; |  XXXX  | $FE5D
       .byte %11111111 ; |XXXXXXXX| $FE5E
       .byte %11111111 ; |XXXXXXXX| $FE5F
       .byte %01111110 ; | XXXXXX | $FE60
       .byte %00000000 ; |        | $FE61
       .byte %00000000 ; |        | $FE56

       .byte %11111111 ; |XXXXXXXX| $FE66
       .byte %00011000 ; |   XX   | $FE67
       .byte %00011000 ; |   XX   | $FE68
       .byte %00011000 ; |   XX   | $FE69
       .byte %11011011 ; |XX XX XX| $FE6A
       .byte %11111111 ; |XXXXXXXX| $FE6B
       .byte %11111111 ; |XXXXXXXX| $FE6C
       .byte %01111110 ; | XXXXXX | $FE6D
       .byte %00000000 ; |        | $FE56

       .byte %11111111 ; |XXXXXXXX| $FE72
       .byte %00011000 ; |   XX   | $FE73
       .byte %00011000 ; |   XX   | $FE74
       .byte %00011000 ; |   XX   | $FE75
       .byte %00011000 ; |   XX   | $FE76
       .byte %00000000 ; |        | $FE77
       .byte %10000001 ; |X      X| $FE78
       .byte %11000011 ; |XX    XX| $FE79
       .byte %01111110 ; | XXXXXX | $FE7A

RubbleGFX:
       .byte %11111111 ; |XXXXXXXX| $FE7B
       .byte %01101101 ; | XX XX X| $FE7C
DigitBlank:
Blank:
       .byte %00000000 ; |        | $FE7D shared
       .byte %00000000 ; |        | $FE7E shared
       .byte %00000000 ; |        | $FE7F shared
       .byte %00000000 ; |        | $FE80 shared x2
       .byte %00000000 ; |        | $FE81 shared x2
       .byte %00000000 ; |        | $FE82 shared x2
Explosion1:
       .byte %00000000 ; |        | $FE83 shared x3
       .byte %00000000 ; |        | $FE84 shared x2
       .byte %00000000 ; |        | $FE85 shared x2
       .byte %00011000 ; |   XX   | $FE86
       .byte %00011000 ; |   XX   | $FE87
       .byte %00000000 ; |        | $FE88
Explosion2:
       .byte %00000000 ; |        | $FE94 shared
SplashExplosion:
       .byte %00000000 ; |        | $FE95 shared
       .byte %00011000 ; |   XX   | $FE96
       .byte %00111100 ; |  XXXX  | $FE97
       .byte %00111100 ; |  XXXX  | $FE98
       .byte %00011000 ; |   XX   | $FE99
       .byte %00000000 ; |        | $FE9A
       .byte %00000000 ; |        | $FEE1


DigitGFX:
Four:
       .byte %00000110 ; |     XX | $DF2F
       .byte %00111110 ; |  XXXXX | $DF30
       .byte %00010110 ; |   X XX | $DF31
       .byte %00001110 ; |    XXX | $DF32
       .byte %00000110 ; |     XX | $DF33


RedColor:
       .byte $60 ; | XX     | $FFF6 red PAL
Seven:
       .byte %00110000 ; |  XX    | $DF1A shared red NTSC
       .byte %00011000 ; |   XX   | $DF1B
       .byte %00001100 ; |    XX  | $DF1C
       .byte %00000110 ; |     XX | $DF1D
Two:
       .byte %00111110 ; |  XXXXX | $DF1E shared
       .byte %00011000 ; |   XX   | $DF1F
       .byte %00001100 ; |    XX  | $DF20
       .byte %00100110 ; |  X  XX | $DF21
Three:
       .byte %00011100 ; |   XXX  | $DF2A shared
       .byte %00100110 ; |  X  XX | $DF2B
       .byte %00001100 ; |    XX  | $DF2C
       .byte %00000110 ; |     XX | $DF2D
       .byte %00011110 ; |   XXXX | $DF2E

Five:
       .byte %00111100 ; |  XXXX  | $DF46
       .byte %00000110 ; |     XX | $DF47
       .byte %00111100 ; |  XXXX  | $DF48
       .byte %00110000 ; |  XX    | $DF49
PlayerUp4: ;bit 7/6 shared w/6th sprite
       .byte %00111110 ; |  XXXXX | $FEA5 shared
       .byte %00110000 ; |  XX    | $FEA7
       .byte %00111100 ; |  XXXX  | $FEA8
       .byte %00110000 ; |  XX    | $FEA9
       .byte %00111110 ; |  XXXXX | $FEAB



HposTbl: ;HMBL value (which direction to move pixels on subsequent scanlines)
       .byte $F0 ; $FDE0
Nine:
       .byte %00011000 ; |   XX   | $DF0A shared
       .byte %00001100 ; |    XX  | $DF0B
       .byte %00011110 ; |   XXXX | $DF0C
       .byte %00110110 ; |  XX XX | $DF0D
Eight:
       .byte %00011100 ; |   XXX  | $DF22 shared
       .byte %00110110 ; |  XX XX | $DF23
       .byte %00011100 ; |   XXX  | $DF24
       .byte %00110110 ; |  XX XX | $DF25
Six:
       .byte %00011100 ; |   XXX  | $DF12 shared
       .byte %00110110 ; |  XX XX | $DF13
       .byte %00111100 ; |  XXXX  | $DF14
       .byte %00011000 ; |   XX   | $DF15
One:
       .byte %00001100 ; |    XX  | $DF16 shared
       .byte %00001100 ; |    XX  | $DF17
       .byte %00001100 ; |    XX  | $DF18
       .byte %00011100 ; |   XXX  | $DF19
       .byte %00001100 ; |    XX  | $DF1A




       ORG  $2E82
       RORG $FE82

Zero: ;only this digit must exist at a negative location ($xx80+)
       .byte %00011100 ; |   XXX  | $DF0E
       .byte %00110110 ; |  XX XX | $DF0F
       .byte %00110110 ; |  XX XX | $DF10
       .byte %00110110 ; |  XX XX | $DF11
Paused3:
       .byte %00011100 ; |   XXX  | $DF12 shared
       .byte %00110110 ; |        | $FEB8
       .byte %00110110 ; |        | $FEB8
Paused2:
       .byte %00110110 ; |        | $FEB8 shared
       .byte %00110110 ; |        | $FEB8 shared
       .byte %00111110 ; |        | $FEB8
       .byte %00110110 ; |        | $FEB8
       .byte %00011100 ; |        | $FEB8




PlayerUp5: ;bit 7 shared w/3rd sprite
       .byte %11000110 ; |XX   XX | $FEAC
       .byte %11001100 ; |XX  XX  | $FEAE
       .byte %11111000 ; |XXXXX   | $FEAF
       .byte %11001100 ; | XX XX  | $FEB1
       .byte %11111000 ; | XXXX   | $FEB2

PlayerUp3:
       .byte %10001100 ; |X   XX  | $FEBE
       .byte %10001100 ; |X   XX  | $FEC0
       .byte %10011110 ; |X  XXXX | $FEC1
       .byte %00110011 ; |  XX  XX| $FEC3
       .byte %00110011 ; |  XX  XX| $FEC4

PlayerUp2:
       .byte %11101101 ; |XXX XX X| $FEB3
       .byte %00001111 ; |    XXXX| $FEB5
       .byte %00001101 ; |    XX X| $FEB7
       .byte %00000111 ; |     XXX| $FEB8
       .byte %00000010 ; |      X | $FEB8

SatGfx:
       .byte %10000001 ; |X      X| $FEB9
       .byte %01000010 ; | X    X | $FEB9
       .byte %00111100 ; |  XXXX  | $FEB9
       .byte %01111110 ; | XXXXXX | $FEB9
       .byte %00111100 ; |  XXXX  | $FEB9
       .byte %01000010 ; | X    X | $FEB9
       .byte %10000001 ; |X      X| $FEB9
PlaneGfx:
       .byte %00000000 ; |        | $FEF8 shared
       .byte %11000000 ; |XX      | $FEF8
       .byte %01110000 ; | XXX    | $FEF8
       .byte %00111000 ; |  XXX   | $FEF8
       .byte %11111111 ; |XXXXXXXX| $FEF8
       .byte %10110011 ; |X XX  XX| $FEF8
       .byte %01100000 ; | XX     | $FEF8
Txt1:
       .byte %00000000 ; |        | $FEF8 shared
       .byte %00011000 ; |   XX   | $FEE9
       .byte %00011000 ; |   XX   | $FEEA
       .byte %00011000 ; |   XX   | $FEEB
       .byte %00011000 ; |   XX   | $FEEC
       .byte %00011000 ; |   XX   | $FEED
       .byte %00011000 ; |   XX   | $FEEE
       .byte %01111110 ; | XXXXXX | $FEEF
Txt2:
       .byte %00000000 ; |        | $FEF8 shared
       .byte %01100110 ; | XX  XX | $FED2
       .byte %01100110 ; | XX  XX | $FED3
       .byte %01100110 ; | XX  XX | $FED4
       .byte %01111110 ; | XXXXXX | $FED5
       .byte %01100110 ; | XX  XX | $FED6
       .byte %01100110 ; | XX  XX | $FED7
       .byte %01100110 ; | XX  XX | $FED8
Txt3:
       .byte %00000000 ; |        | $FEF8 shared
       .byte %01111110 ; | XXXXXX | $FECA
       .byte %01100000 ; | XX     | $FECB
       .byte %01100000 ; | XX     | $FECC
       .byte %01111000 ; | XXXX   | $FECD
       .byte %01100000 ; | XX     | $FECE
       .byte %01100000 ; | XX     | $FECF
       .byte %01111110 ; | XXXXXX | $FED0
Txt4:
       .byte %00000000 ; |        | $FEF8 shared
       .byte %01100110 ; | XX  XX | $FEDA
       .byte %01100110 ; | XX  XX | $FEDB
       .byte %01100110 ; | XX  XX | $FEDC
       .byte %01101110 ; | XX XXX | $FEDD
       .byte %01111110 ; | XXXXXX | $FEDE
       .byte %01110110 ; | XXX XX | $FEDF
       .byte %01100110 ; | XX  XX | $FEE0
Txt5:
       .byte %00000000 ; |        | $FEF8 shared
       .byte %01111100 ; | XXXXX  | $FEF1
       .byte %01100110 ; | XX  XX | $FEF2
       .byte %01100110 ; | XX  XX | $FEF3
       .byte %01100110 ; | XX  XX | $FEF4
       .byte %01100110 ; | XX  XX | $FEF5
       .byte %01100110 ; | XX  XX | $FEF6
       .byte %01111100 ; | XXXXX  | $FEF7
Explosion3:
       .byte %00000000 ; |        | $FEE1 shared
       .byte %00011000 ; |   XX   | $FEE2
       .byte %00111100 ; |  XXXX  | $FEE3
       .byte %01111110 ; | XXXXXX | $FEE4
       .byte %01111110 ; | XXXXXX | $FEE5
       .byte %00111100 ; |  XXXX  | $FEE6
       .byte %00011000 ; |   XX   | $FEE7
       .byte %00000000 ; |        | $FEF8

PlayerUp1:
       .byte %11000011 ; |XX    XX| $FE9E
       .byte %11000011 ; |XX    XX| $FEA0
       .byte %11110011 ; |XXXX  XX| $FEA1
       .byte %11011011 ; |XX XX XX| $FEA3
       .byte %11110011 ; |XXXX  XX| $FEA4



Paused5:
       .byte %00111110 ; |        | $FEB8
       .byte %00110000 ; |        | $FEB8
       .byte %00111100 ; |        | $FEB8
       .byte %00110000 ; |        | $FEB8
       .byte %00111110 ; |        | $FEB8

Paused1:
       .byte %00110000 ; |        | $FEB8
       .byte %00110000 ; |        | $FEB8
       .byte %00111100 ; |        | $FEB8
       .byte %00110110 ; |        | $FEB8
Paused6:
       .byte %00111100 ; |        | $FEB8 shared
       .byte %00110110 ; |        | $FEB8
       .byte %00110110 ; |        | $FEB8
       .byte %00110110 ; |        | $FEB8
Paused4:
       .byte %00111100 ; |        | $FEB8 shared
       .byte %00000110 ; |        | $FEB8
       .byte %00011100 ; |        | $FEB8
       .byte %00110000 ; |        | $FEB8
       .byte %00011100 ; |        | $FEB8


Explosion4:
       .byte %00011000 ; |   XX   | $FE34
       .byte %00111100 ; |  XXXX  | $FE35
       .byte %01111110 ; | XXXXXX | $FE36
       .byte %11111111 ; |XXXXXXXX| $FE37
       .byte %11111111 ; |XXXXXXXX| $FE38
       .byte %01111110 ; | XXXXXX | $FE39
       .byte %00111100 ; |  XXXX  | $FE3A
       .byte %00011000 ; |   XX   | $FE34

Pos0:
       .byte $90 ; |        | $FFB3
       .byte $30 ; |        | $FFB3
       .byte $60 ; |        | $FFB3
Pos1:
       .byte $A0 ; |        | $FFB3
       .byte $40 ; |        | $FFB3
       .byte $70 ; |        | $FFB3


       ORG  $2F00
       RORG $FF00

Count1b:
Count2b:
       .byte %00101000 ; |  X X   | $FFFC shared
       .byte %01010100 ; | X X X  | $FFFC shared
       .byte %01010100 ; | X X X  | $FFFC shared
       .byte %01000100 ; | X   X  | $FFFC shared
       .byte %01000100 ; | X   X  | $FFFD shared
       .byte %00000000 ; |        | $FFFD shared
       .byte %00000000 ; |        | $FFFD shared
       .byte %00000000 ; |        | $FFFD shared
       .byte %00000000 ; |        | $FFFE shared
       .byte %10000000 ; |X       | $FFFE shared
       .byte %10000000 ; |X       | $FFFE shared
Count00:
       .byte %00000000 ; |        | $FFFC sharedx2
       .byte %00000000 ; |        | $FFFC
       .byte %00000000 ; |        | $FFFC
Count1bb:
Count2bb:
       .byte %00000000 ; |        | $FFFC sharedx2
       .byte %00000000 ; |        | $FFFD sharedx2
       .byte %00000000 ; |        | $FFFD sharedx2
       .byte %00000000 ; |        | $FFFC sharedx2
       .byte %00000000 ; |        | $FFFD sharedx2
       .byte %00000000 ; |        | $FFFD sharedx2
       .byte %00000000 ; |        | $FFFD sharedx2
       .byte %00000000 ; |        | $FFFD sharedx2
       .byte %00000000 ; |        | $FFFE sharedx2
       .byte %10000000 ; |X       | $FFFE shared
       .byte %10000000 ; |X       | $FFFE shared
Count0a:
       .byte %00000000 ; |        | $FFFC sharedx2
       .byte %00000000 ; |        | $FFFC
       .byte %00000000 ; |        | $FFFC
       .byte %00110001 ; |  XX   X| $FFFC
       .byte %01101011 ; | XX X XX| $FFFD
       .byte %01101011 ; | XX X XX| $FFFD
       .byte %01101011 ; | XX X XX| $FFFD
       .byte %01101011 ; | XX X XX| $FFFE
       .byte %00110011 ; |  XX  XX| $FFFE
Count0b:
       .byte %00000000 ; |        | $FFFC shared
       .byte %00000000 ; |        | $FFFC shared
       .byte %00000000 ; |        | $FFFC shared
       .byte %10001100 ; |X   XX  | $FFFC
       .byte %01001100 ; | X  XX  | $FFFD
       .byte %01001100 ; | X  XX  | $FFFD
       .byte %01001100 ; | X  XX  | $FFFD
       .byte %01001100 ; | X  XX  | $FFFE
       .byte %01011110 ; | X XXXX | $FFFE
Count4a:
       .byte %00000000 ; |        | $FFFC shared
       .byte %00000000 ; |        | $FFFC shared
       .byte %00000000 ; |        | $FFFC shared
       .byte %00000010 ; |      X | $FFFC
       .byte %00000011 ; |      XX| $FFFD
       .byte %00000001 ; |       X| $FFFD
       .byte %00001010 ; |    X X | $FFFD
       .byte %00001110 ; |    XXX | $FFFD
       .byte %00000100 ; |     X  | $FFFE
       .byte %00000010 ; |      X | $FFFE
       .byte %00000011 ; |      XX| $FFFE
       .byte %00000001 ; |       X| $FFFE


Count1a:
       .byte %01110010 ; | XXX  X | $FFFC
       .byte %01000101 ; | X   X X| $FFFC
       .byte %01000101 ; | X   X X| $FFFC
       .byte %01000101 ; | X   X X| $FFFC
       .byte %01000010 ; | X    X | $FFFD
       .byte %00000000 ; |        | $FFFD
       .byte %00000000 ; |        | $FFFD
       .byte %00000000 ; |        | $FFFD
       .byte %00000000 ; |        | $FFFE
       .byte %00000010 ; |      X | $FFFE
       .byte %00000011 ; |      XX| $FFFE
       .byte %00000001 ; |       X| $FFFE


Count2a:
Count3a:
       .byte %01110010 ; | XXX  X | $FEFC shared
       .byte %01000101 ; | X   X X| $FEFC shared
       .byte %01000101 ; | X   X X| $FEFC shared
       .byte %01000101 ; | X   X X| $FEFC shared
       .byte %01000010 ; | X    X | $FEFD shared
       .byte %00000000 ; |        | $FEFD shared
       .byte %00001010 ; |    X X | $FEFD shared
       .byte %00001110 ; |    XXX | $FEFD shared
       .byte %00000100 ; |     X  | $FEFE shared
       .byte %00000010 ; |      X | $FEFE shared
       .byte %00000011 ; |      XX| $FEFE shared
       .byte %00000001 ; |       X| $FEFE shared

Count3b:
       .byte %00101000 ; |  X X   | $FEFC
       .byte %01010100 ; | X X X  | $FEFC
       .byte %01010100 ; | X X X  | $FEFC
       .byte %01000100 ; | X   X  | $FEFC
       .byte %01000100 ; | X   X  | $FEFD
       .byte %00000000 ; |        | $FEFD
       .byte %10100000 ; |X X     | $FEFD
       .byte %11100000 ; |XXX     | $FEFD
       .byte %01000000 ; | X      | $FEFE
       .byte %10000000 ; |X       | $FEFE
       .byte %10000000 ; |X       | $FEFE
Count4b:
Count5b:
       .byte %00000000 ; |        | $FEFC sharedx2
       .byte %00000000 ; |        | $FEFC shared
       .byte %00000000 ; |        | $FEFC shared
       .byte %10000000 ; |X       | $FEFC shared
       .byte %10000000 ; |X       | $FEFD shared
       .byte %00000000 ; |        | $FEFD shared
       .byte %10100000 ; |X X     | $FEFD shared
       .byte %11100000 ; |XXX     | $FEFD shared
       .byte %01000000 ; | X      | $FEFE shared
       .byte %10000000 ; |X       | $FEFE shared
       .byte %10000000 ; |X       | $FEFE shared
Count5a:
Count6a:
       .byte %00000000 ; |        | $FEFC sharedx3
       .byte %00000000 ; |        | $FEFC shared
       .byte %00000000 ; |        | $FEFC shared
       .byte %00101010 ; |  X X X | $FEFC shared
       .byte %00111011 ; |  XXX XX| $FEFD shared
       .byte %00010001 ; |   X   X| $FEFD shared
       .byte %00001010 ; |    X X | $FEFD shared
       .byte %00001110 ; |    XXX | $FEFD shared
       .byte %00000100 ; |     X  | $FEFE shared
       .byte %00000010 ; |      X | $FEFE shared
       .byte %00000011 ; |      XX| $FEFE shared
       .byte %00000001 ; |       X| $FEFE shared

Count8b:
Count9b:
       .byte %10100000 ; |X X     | $FEFC shared
       .byte %11100000 ; |XXX     | $FEFC shared
       .byte %01000000 ; | X      | $FEFC shared
       .byte %10101000 ; |X X X   | $FEFC shared
       .byte %10111000 ; |X XXX   | $FEFD shared
       .byte %00010000 ; |   X    | $FEFD shared
       .byte %10100000 ; |X X     | $FEFD shared
       .byte %11100000 ; |XXX     | $FEFD shared
       .byte %01000000 ; | X      | $FEFE shared
       .byte %10000000 ; |X       | $FEFE shared
       .byte %10000000 ; |X       | $FEFE shared
Count6b:
Count7b:
       .byte %00000000 ; |        | $FEFC sharedx3
       .byte %00000000 ; |        | $FEFC shared
       .byte %00000000 ; |        | $FEFC shared
       .byte %10101000 ; |X X X   | $FEFC shared
       .byte %10111000 ; |X XXX   | $FEFD shared
       .byte %00010000 ; |   X    | $FEFD shared
       .byte %10100000 ; |X X     | $FEFD shared
       .byte %11100000 ; |XXX     | $FEFD shared
       .byte %01000000 ; | X      | $FEFE shared
       .byte %10000000 ; |X       | $FEFE shared
       .byte %10000000 ; |X       | $FEFE shared
LFF49: ;8
       .byte $00 ; |        | $FF49 sharedx2
       .byte $01 ; |       X| $FF4A
       .byte $02 ; |      X | $FF4B
       .byte $03 ; |      XX| $FF4C
       .byte $04 ; |     X  | $FF4D
       .byte $00 ; |        | $FF4E
       .byte $06 ; |     XX | $FF4F
       .byte $04 ; |     X  | $FF50


Count7a:
Count8a:
       .byte %00001010 ; |    X X | $FEFC shared
       .byte %00001110 ; |    XXX | $FEFC shared
       .byte %00000100 ; |     X  | $FEFC shared
       .byte %00101010 ; |  X X X | $FEFC shared
       .byte %00111011 ; |  XXX XX| $FEFD shared
       .byte %00010001 ; |   X   X| $FEFD shared
       .byte %00001010 ; |    X X | $FEFD shared
       .byte %00001110 ; |    XXX | $FEFD shared
       .byte %00000100 ; |     X  | $FEFE shared
       .byte %00000010 ; |      X | $FEFE shared
       .byte %00000011 ; |      XX| $FEFE shared
       .byte %00000001 ; |       X| $FEFE shared


Count9a:
Count10a:
       .byte %10101010 ; |X X X X | $FEFC shared
       .byte %11101110 ; |XXX XXX | $FEFC shared
       .byte %01000100 ; | X   X  | $FEFC shared
       .byte %00101010 ; |  X X X | $FEFC shared
       .byte %00111011 ; |  XXX XX| $FEFD shared
       .byte %00010001 ; |   X   X| $FEFD shared
       .byte %00001010 ; |    X X | $FEFD shared
       .byte %00001110 ; |    XXX | $FEFD shared
       .byte %00000100 ; |     X  | $FEFE shared
       .byte %00000010 ; |      X | $FEFE shared
       .byte %00000011 ; |      XX| $FEFE shared
       .byte %00000001 ; |       X| $FEFE shared

Count10b:
       .byte %10101010 ; |X X X X | $FEFC
       .byte %11101110 ; |XXX XXX | $FEFC
       .byte %01000100 ; | X   X  | $FEFC
       .byte %10101000 ; |X X X   | $FEFC
       .byte %10111000 ; |X XXX   | $FEFD
       .byte %00010000 ; |   X    | $FEFD
       .byte %10100000 ; |X X     | $FEFD
       .byte %11100000 ; |XXX     | $FEFD
       .byte %01000000 ; | X      | $FEFE
       .byte %10000000 ; |X       | $FEFE
       .byte %10000000 ; |X       | $FEFE
LFFCB:
       .byte $00 ; |        | $FFCB shared
LFFEF: ;7
       .byte $00 ; |        | $FFEF shared
       .byte $02 ; |      X | $FFF0 shared
       .byte $06 ; |     XX | $FFF1 shared
       .byte $04 ; |     X  | $FFF2
       .byte $0A ; |    X X | $FFF3
       .byte $00 ; |        | $FFF4
       .byte $08 ; |    X   | $FFF5


Count3bb:
       .byte %00000000 ; |        | $FEFC
       .byte %00000000 ; |        | $FEFC
       .byte %00000000 ; |        | $FEFC
       .byte %00000000 ; |        | $FEFC
       .byte %00000000 ; |        | $FEFD
       .byte %00000000 ; |        | $FEFD
       .byte %10100000 ; |X X     | $FEFD
       .byte %11100000 ; |XXX     | $FEFD
       .byte %01000000 ; | X      | $FEFE
       .byte %10000000 ; |X       | $FEFE
       .byte %10000000 ; |X       | $FEFE
Count1aa:
       .byte %00000000 ; |        | $FEFC shared
       .byte %00000000 ; |        | $FEFC
       .byte %00000000 ; |        | $FEFC
       .byte %00000000 ; |        | $FEFC
       .byte %00000000 ; |        | $FEFD
       .byte %00000000 ; |        | $FEFD
       .byte %00000000 ; |        | $FEFD
       .byte %00000000 ; |        | $FEFD
       .byte %00000000 ; |        | $FEFE
       .byte %00000010 ; |      X | $FEFE
       .byte %00000011 ; |      XX| $FEFE
       .byte %00000001 ; |       X| $FEFE


Count2aa:
Count3aa:
       .byte %00000000 ; |        | $FEFC shared
       .byte %00000000 ; |        | $FEFC shared
       .byte %00000000 ; |        | $FEFC shared
       .byte %00000000 ; |        | $FEFC shared
       .byte %00000000 ; |        | $FEFD shared
       .byte %00000000 ; |        | $FEFD shared
       .byte %00001010 ; |    X X | $FEFD shared
       .byte %00001110 ; |    XXX | $FEFD shared
       .byte %00000100 ; |     X  | $FEFE shared
       .byte %00000010 ; |      X | $FEFE shared
       .byte %00000011 ; |      XX| $FEFE shared
       .byte %00000001 ; |       X| $FEFE shared



;explosion table
ExplosionAnim: ;16
       .byte <Explosion1 ; $FF12
       .byte <Explosion2 ; $FF13
       .byte <Explosion3 ; $FF14
       .byte <Explosion4 ; $FF15
       .byte <Explosion1 ; $FF16
       .byte <Explosion2 ; $FF17
       .byte <Explosion3 ; $FF18
       .byte <Explosion4 ; $FF19
       .byte <Explosion4 ; $FF1A
       .byte <Explosion3 ; $FF1B
       .byte <Explosion2 ; $FF1C
       .byte <Explosion1 ; $FF1D
       .byte <Explosion4 ; $FF1E
       .byte <Explosion3 ; $FF1F
       .byte <Explosion2 ; $FF20
       .byte <Explosion1 ; $FF21



   IF PLUSROM
       jmp    START                   ;3
       ; Keep PlusROM hotspots free
       ORG  $2FF6
       RORG $FFF6
   ELSE
;missile targets
TargetTbl:
       .byte $18 ; |   XX   | $FF00 city1
       .byte $28 ; |  X X   | $FF01 city2
       .byte $38 ; |  XXX   | $FF02 city3
       .byte $6A ; | XX X X | $FF03 city4
       .byte $7A ; | XXXX X | $FF04 city5
       .byte $8A ; |X   X X | $FF05 city6
       .byte $9A ; |X  XX X | $FF06 silo
       .byte $50 ; | X X    | $FF07 silo
       .byte $10 ; |   X    | $FF07 silo
   ENDIF

CityColor:
       .byte $D0 ; |XX X    | $FFF6 blue PAL
       .byte $80 ; |X       | $FFF7 blue NTSC


   IF PLUSROM
       ORG  $2FFA
       RORG $FFFA
   .word (PlusROM_API-$C000)        ; PlusRom API pointer
   .word START,0
   ELSE
       ORG  $2FF8
       RORG $FFF8
       .byte 0
START2:
       jmp    START                   ;3
       .word START2,0
   ENDIF