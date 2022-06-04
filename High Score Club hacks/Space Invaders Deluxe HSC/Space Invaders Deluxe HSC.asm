;       XXXXXXX    XXXXXXXX                XX      XXXXXX    XXXXXXXX
;     XXX     XXX  XXX    XXX            XXXX    XXX    XXX  XXX
;    XXX           XXX      XXX        XXX XX  XXX           XXX
;     XXX          XXX      XXX      XXX   XX  XXX           XXX
;       XXX        XXX      XXX    XXX     XX  XXX           XXXXXX
;         XXX      XXX    XXX    XXX       XX  XXX           XXX
;           XXX    XXXXXXXX    XXX         XX  XXX           XXX
;            XXX   XXX       XXXXXXXXXXXXXXXX  XXX           XXX
;    XXX    XXX    XXX     XXX             XX    XXX    XXX  XXX
;      XXXXXX      XXX   XXX               XX      XXXXXX    XXXXXXXX
;
;    XX  XX    XX  XX      XX    XX  XXXX    XXXXXX  XXXXXX      XXXX
;    XX  XXX   XX  XX    XX    XXXX  XX  XX  XX      XX    XX  XX
;    XX  XX XX XX  XX  XX    XX  XX  XX  XX  XXXX    XX    XX    XX
;    XX  XX   XXX  XXXX    XXXXXXXX  XX  XX  XX      XXXXXX        XX
;    XX  XX    XX  XX    XX      XX  XXXX    XXXXXX  XX    XX  XXXX
;
;            XXXX    XXXXXX  XX      XX  XX  XX  XX  XXXXXX
;            XX  XX  XX      XX      XX  XX  XX  XX  XX
;            XX  XX  XXXX    XX      XX  XX    XX    XXXX
;            XX  XX  XX      XX      XX  XX  XX  XX  XX
;            XXXX    XXXXXX  XXXXXX  XXXXXX  XX  XX  XXXXXX
;
;Space Invaders Deluxe
;Hack by: Kurt(Nukey Shay)Howe
;Original program:
;Space Invaders
;By: Rick Maurer (c) 1980 Atari
;Special thanks to:
;Franklin(NE146)Cruz' 'Space Invaders' for paving the way
;Rob(raindog)Kudla's 'Space Invaders Arcade' for refining that idea
;Jeff(Yak)Minter's 'Beast Invaders' for supplying double-shot capability
;Albert(Albert)Yarusso, Alex(Alex)Bilstein, and AtariAge.com...
;for giving us all a place to show off ;)
;And the members of AtariAge.com for playtesting and support!
;
;Hack history:
;03/17/04 Flashing missiles, additional mothership added, "rainbow" bonus added.
;03/18/04 Difficulty switches changed, line added to represent "Earth", colorbars added.
;03/19/04 Explosion animation removed.
;03/20/04 Sounds altered, shot counting added.
;03/21/04 End-of-movement markers fixed.
;03/26/04 Final 4k build.
;10/04/04 Binary size doubled to 8k, 1st intermission added, mothership animations added.
;10/27/04 5-digit scoring implemented.
;11/27/04 Title marquee added.
;11/30/04 System stack eliminated.
;12/04/04 Onscreen point values shown for hit invaders/motherships.
;01/17/05 NTSC/PAL palette switching implemented.
;03/04/05 Pause function added (to the intermission kernal only).
;03/05/05 Hit sound altered, missile size altered, "rainbow" bonus sound added.
;09/20/05 7800 autodetect added, 2nd intermission added, true shot count!
;09/21/05 Display cleaned up (visible "attract" area reduced, HMOVE line now invisible).
;09/24/05 High Score function added (single-player and co-op variations).
;09/25/05 True rainbow bonus, player-independant shot counting capability.
;09/26/05 Invader GFX altered (3 new types), mothership now always enabled.
;09/27/05 High score routine moved, additional title display color tables, markers altered,
;         original SI lives bug (that displays 1 life left in 2-player attract mode) corrected,
;         title screen and intermission screen skip routines added.
;06/04/22 PlusROM HSC Support added by Wolfgang(Al_Nafuur)Stubig.
;
;Instructions:
; Game variations follow the original manual with these exceptions:
;
; Console switches:
; *Color/B&W is used to toggle NTSC/PAL60 modes (except while the power-up title is displayed).
; *Left difficulty selects the base width of BOTH players.
; *Right difficulty selects between single and double shots (in non-simultanous variations).
;
; Scoring:
; *Points per invader are 30, 30, 20, 20, 10, and 10 (top row to bottom). In addition...
; *500-point bonus awarded for hitting a lowest invader last (1000 if from leftmost column).
; *500-point bonus awarded for hitting next-from-lowest invader last (leftmost column only).
; *Mothership values range from 50 to 300 points, the blinking mothership worth 500 points.
;
; Cosmetic:
; *A game title marquee is shown momentarily upon powerup. This display can be skipped by
;  moving any direction on either joystick (also see 'Gameplay' below).
; *Game colors and sprite shapes altered to approximate coin-op "Deluxe"/"Part II" versions.
; *Display altered to keep track of up to 99,990 points each player (instead of only 9,990)!
; *The right score color (indicating # of players for that game variation) will be displayed
;  in RED if that variation includes the high-score function...YELLOW if not.  All single-
;  player and co-op variations include the function.  The high score is cleared if GAME SELECT
;  is used at any time.
; *End-of-movement markers altered to indicate the number of player bases remaining.
;
; Gameplay:
; *One of two "intermissions" will appear following the completion of each board...during
;  which gameplay can be suspended indefinately by pressing a controller button. Press again
;  to resume. Intermissions can be skipped individually by moving a stick or using any console
;  switch. The intermission display can be disabled completely by holding the RIGHT joystick
;  in the DOWN direction when the powerup title display is shown.
;
; New strategies:
; *Blinking motherships:
;   The blinking motherships are only allowed to appear when an internal 4-bit missile counter
;   rolls to zero, and are always worth 500 points instead of a random value. Missile counters
;   begin with a value of 1 for each player when a new game is started...so 15 more shots are
;   required to make the blinking craft appear (and 16 shots following that, etc.). By keeping
;   track of how many shots are fired, you can force this craft to appear every time. Seperate
;   counters are used for each player during 2-player competition (non-simultanous) play.
;   NOTE: The blinking craft cannot be struck by a missile when it is momentarily invisible.
; *The "rainbow" bonus:
;   You collect bonus points if you can shoot a low invader (out of the original 36) last!
;   Since the leftmost column has two bonus point invaders available instead of only one, you
;   can better your odds of getting one by going after the other columns first and finishing
;   the round with that column. Because player 1 begins the round under this column, you may
;   find it to your advantage to begin a 2-player game instead and use the right (yellow) base, ;   which begins each round under the rightmost column of invaders.
;
;
;
;
;
;
; Disassembly of sia.bin
; Disassembled Tue Mar 16 23:27:53 2004
; Using DiStella v2.0
;
; Command Line: C:\BIN\DISTELLA.EXE -pafscsi.cfg sia.bin 
;
; si.cfg contents:
;      CODE F000 FBFD
;      GFX FBFE FD66
;      CODE FD67 FF4B
;      GFX FF4C FFFB
;      
; current .cfg contents:
;Bank 1...
;      CODE D000 DAF8
;      GFX DAF9 DFF0
;      CODE DFF1 DFF4
;      GFX DFF5 DFFF
;Bank 2...
;      CODE F000 FCAB
;      GFX FCAC FFF0
;      CODE FFF1 FFF7
;      GFX FFF8 FFFF
;
;
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






Invaders   =  $91 ;# of invaders left for current player
Mship      =  $C9
Framecount =  $CA
Variation  =  $DC
Score      =  $E6
P1scoreHi  =  Score
P2scoreHi  =  Score+1
P1scoreLo  =  Score+2
P2scoreLo  =  Score+3

;ram assignment changes
Icolor     =  $DD
Temp       =  $DE
ColorShift =  $DF
Ctrl       =  $FA
DifSw      =  $FB
Mask       =  $FC

Lives      =  $FD
;bit0-1=lives, bit2=intermission autoskip (set in boot sequence).
;bits 7,4,3=pointer to bonus score gfx,bit5=P1 bonus double flag,bit6=hiscore flag

FLAG2      =  $FE
;bit4=Mystery ship,bit5=P2 bonus double flag,bit6=PAL/NTSC,bit7=console type,low nybble free

SHOT       =  $FF ;nybble shot counter


;intermission ram (all shared)
PrintTemp    =  $AB
TempCounter  =  $EB
TempGFX      =  $CC
PlayerX      =  $CB
ShipPos      =  $C8
ShipMem      =  $9D
Trigger      =  $9C
OldTrigger   =  $9B
ShipWaits    =  $9A



;constants:
PLUSROM     = 1

   IF PLUSROM
WriteToBuffer     = $1ff0
WriteSendBuffer   = $1ff1
ReceiveBuffer     = $1ff2
ReceiveBufferSize = $1ff3
HIGHSCORE_ID      = 54	 ; Space Invaders Deluxe game ID in Highscore DB
   ENDIF
;colors
YEAR     =  $98
SKY      =  $00


;Begin first bank
       ORG $1000
       RORG $D000

LD000:
       sta    $1FF9                   ;4 go back to second bank
;check score color
       lda    FLAG2                   ;3
       and    #$40                    ;2
       tax                            ;2
       bit    Lives                   ;3
       bvs    LD00D                   ;2
       inx                            ;2
LD00D:
       lda    YELLOW,x                ;4
       sta    COLUP1                  ;3
LD012:
       lda    INTIM                   ;4
       bne    LD012                   ;2
       sta    VBLANK                  ;3 enable TIA
       sta    CXCLR                   ;3
       ldx    #$00                    ;2
;mothership disable
       lda    Framecount              ;3
       and    #$18                    ;2
       lsr                            ;2
       lsr                            ;2
       sta    Temp                    ;3
       lda    #$EA                    ;2
       sta    WSYNC                   ;3
       sta    TIM64T                  ;4
       sta    HMCLR                   ;3
       bit    $AA                     ;3
       bmi    LD03B                   ;2
;check mothership
       lda    $9E                     ;3
       cmp    #$B4                    ;2
       beq    LD03B                   ;2
       jmp    LD113                   ;3 jump to display mothership
LD03B:
       jmp    LD984                   ;3 jump to display score




LD03E:
       stx    Waste2                  ;3
       dec    $89                     ;5
       lda    $89                     ;3
       bmi    LD055                   ;2
       cmp    #$03                    ;2
       lda    #$02                    ;2
       bcc    LD04D                   ;2
       lsr                            ;2
LD04D:
       sta    ENABL                   ;3
       sta    WSYNC                   ;3
       sta    HMCLR                   ;3
       bpl    LD072                   ;2
LD055:
       clc                            ;2
       adc    $8A                     ;3
       sta    $89                     ;3
       lda    #$00                    ;2
       sta    WSYNC                   ;3
       sta    HMCLR                   ;3
       sta    ENABL                   ;3
       lda    $88                     ;3
       sta    HMBL                    ;3
       and    #$0F                    ;2
       tay                            ;2
LD069:
       dey                            ;2
       bpl    LD069                   ;2
       sta    RESBL                   ;3
       lda    #$7C                    ;2
       sta    $8A                     ;3
LD072:
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       stx    Waste2                  ;3
       jmp    LD421                   ;3




LD07B:
       sta    WSYNC                   ;3
       lda    FLAG2                   ;3
       and    #$40                    ;2
       eor    #$40                    ;2
       tay                            ;2
       lda    CYAN,y                  ;4 get P1 base/score color
       sta    COLUP0                  ;3
       lda    #$02                    ;2
       sta    CTRLPF                  ;3
       lda    #$04                    ;2
       sta    $EE                     ;3
       lda    #$DF                    ;2
       sta    $EF                     ;3
       bit    $AA                     ;3
       bmi    LD09D                   ;2
       lda    #$FF                    ;2
       bne    LD09F                   ;2
LD09D:
       txa                            ;2
       NOP                            ;2
LD09F:
       sta.w  Mask                    ;4
;invisible
       ldy    Icolor                  ;3
       lda    ColorShift              ;3
       bne    LD0AC                   ;2
       ldy    #$05                    ;2<- adjusted invisible color
       bne    LD0AE                   ;2
LD0AC:
       NOP                            ;2
       NOP                            ;2
LD0AE:
       sty    Icolor                  ;3
LD0B0:
       ldy    $F0                     ;3
       lda    ($EE),y                 ;5
       lsr                            ;2
       sta    PF0                     ;3
       NOP                            ;2
       stx    PF1                     ;3
       ldy    $EE                     ;3
       lda    UTdigits,y              ;4
       and    Mask                    ;3
       ldy    $F5                     ;3
       ora    ($EE),y                 ;5
       lsr                            ;2
       sta    PF2                     ;3
       ldy    $F2                     ;3
       lda    ($EE),y                 ;5
       sta    PF0                     ;3
       ldy    $EE                     ;3
       lda    UTdigits,y              ;4
       and    Mask                    ;3
       ldy    $F7                     ;3
       ora    ($EE),y                 ;5
       sta    PF2                     ;3
       ldy    $F4                     ;3
       lda    ($EE),y                 ;5
       sta    $81                     ;3
       ldy    $F1                     ;3
       lda    ($EE),y                 ;5
       ora    $81                     ;3
       asl                            ;2
       sta    PF1                     ;3
       NOP                            ;2
       stx    PF0                     ;3
       stx    PF2                     ;3
       ldy    $F6                     ;3
       lda    ($EE),y                 ;5
       sta    $81                     ;3
       ldy    $F3                     ;3
       lda    ($EE),y                 ;5
       ora    $81                     ;3
       NOP                            ;2
       NOP                            ;2
       NOP                            ;2
       NOP                            ;2
       NOP                            ;2
       sta    PF1                     ;3
       dec    $EE                     ;5
       bpl    LD0B0                   ;2
       lda    $89                     ;3
       clc                            ;2
       adc    #$F9                    ;2
       sta    $89                     ;3
       NOP                            ;2
       stx    PF1                     ;3
       jmp    LD1A7                   ;3



;bugfix...correct ground during UFO
LD113:
       cmp    #$0F                    ;2
       bcs    LD119                   ;2
       sta    WSYNC                   ;3
LD119:
       cmp    #$20                    ;2
       bcs    LD11F                   ;2
       sta    WSYNC                   ;3
LD11F:
       ldy    #$FF                    ;2
       sec                            ;2 don't remove
LD122:
       iny                            ;2
       sbc    #$0F                    ;2
       bcs    LD122                   ;2
       eor    #$FF                    ;2
       sbc    #$06                    ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       sty    $83,x                   ;4
       ora    $83,x                   ;4
       sta    $83,x                   ;4
       sta    WSYNC                   ;3
       NOP                            ;2
       iny                            ;2
       sta    HMP0,x                  ;4
       NOP                            ;2
LD13C:
       dey                            ;2
       bpl    LD13C                   ;2
       sta    RESP0,x                 ;4
       dec    $89                     ;5
       lda    $89                     ;3
       ldx    #$02                    ;2
       cmp    #$03                    ;2
       bcc    LD14D                   ;2
       ldx    #$00                    ;2
LD14D:
       stx    ENABL                   ;3
       ldx    Temp                    ;3
;shot check
       lda    FLAG2                   ;3
       and    #$10                    ;2
       bne    LD158                   ;2 branch if mystery ship
       inx                            ;2 blinking
LD158:
       lda    MothershipGFXtbl,x      ;4
       sta    $EE                     ;3
       lda    #>MothershipGFX         ;2
       sta    $EF                     ;3
       lda    #$00                    ;2
       sta    NUSIZ0                  ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       lda    $C8                     ;3
       and    #$39                    ;2
       cmp    #$39                    ;2
       bne    LD178                   ;2
       ldy    Mship                   ;3
       lda    MothershipScoreGFXtbl,y ;4
       sta    $EE                     ;3
LD178:
       sta    WSYNC                   ;3
       sta    HMCLR                   ;3
       dec    $89                     ;5
       lda    $89                     ;3
       ldy    #$02                    ;2
       cmp    #$03                    ;2
       bcc    LD188                   ;2
       ldy    #$00                    ;2
LD188:
       sty    ENABL                   ;3
       ldy    #$08                    ;2
LD18C:
       sta    WSYNC                   ;3
       lda    ($EE),y                 ;5
       sta    GRP0                    ;3
       tya                            ;2
       lsr                            ;2
       bcs    LD1A4                   ;2
       dec    $89                     ;5
       lda    $89                     ;3
       ldx    #$02                    ;2
       cmp    #$03                    ;2
       bcc    LD1A2                   ;2
       ldx    #$00                    ;2
LD1A2:
       stx    ENABL                   ;3
LD1A4:
       dey                            ;2
       bpl    LD18C                   ;2




LD1A7:
       lda    #$00                    ;2
       sta    WSYNC                   ;3
       sta    GRP0                    ;3
       lda    $84                     ;3
       sta    HMP1                    ;3
       sta    HMP0                    ;3
       and    #$0F                    ;2
       tay                            ;2
LD1B6:
       dey                            ;2
       bpl    LD1B6                   ;2
       sta    RESP0                   ;3
       lda    #$06                    ;2
       sta    RESP1                   ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       sta    NUSIZ0                  ;3
       sta    NUSIZ1                  ;3
       ldx    #$0A                    ;2
       lda    #>SpriteGFX             ;2 <-invaders
LD1CB:
       sta    $EF,x                   ;4
       dex                            ;2
       dex                            ;2
       bpl    LD1CB                   ;2
       dec    $89                     ;5
       lda    $89                     ;3
       ldy    #$02                    ;2
       cmp    #$03                    ;2
       bcc    LD1DD                   ;2
       ldy    #$00                    ;2
LD1DD:
       sty    ENABL                   ;3
       lda    WSYNC                   ;3
       and    #$40                    ;2
       sta    $82                     ;3
       sta    CXCLR                   ;3
       sta    HMCLR                   ;3
       lda    #$F0                    ;2
       sta    HMP1                    ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
LD1F1:
       dec    $8F                     ;5
       bpl    LD200                   ;2
       ldy    #$05                    ;2
       lda    #$01                    ;2
       sta    VDELP0                  ;3
       sta    VDELP1                  ;3
       jmp    LD2E5                   ;3
LD200:
       jmp    LDA35                   ;3




LD203:
       stx    $EE                     ;3
       inx                            ;2
       lda    $C7                     ;3
       asl                            ;2
       asl                            ;2
       clc                            ;2
       adc    $C7                     ;3
       clc                            ;2
       adc    #$59                    ;2
       sta    $C7                     ;3
       and    #$20                    ;2
       bne    LD218                   ;2
       dex                            ;2
       dex                            ;2
LD218:
       cpx    #$08                    ;2
       bcs    LD226                   ;2
       lda    $C7                     ;3
       cmp    #$C0                    ;2
       bcc    LD226                   ;2
       lda    ($F0),y                 ;5
       sta    ($F0),y                 ;6
LD226:
       ldx    $EE                     ;3
       lda    ($F0),y                 ;5
       and    LDD79,x                 ;4
       sta    ($F0),y                 ;6
       jmp    LD636                   ;3



LD232:
       sta    HMCLR                   ;3
       lda    $84                     ;3 horizontal position
       bmi    LD238                   ;2 (req'd)
LD238:
       and    #$0F                    ;2
       tax                            ;2
LD23B:
       dex                            ;2
       bpl    LD23B                   ;2
LD23E:
       lda    ($F8),y                 ;5
       tax                            ;2
       lda    ($EE),y                 ;5
       sta    GRP0                    ;3
       lda    ($F0),y                 ;5
       sta    GRP1                    ;3
       lda    ($F2),y                 ;5
       sta    GRP0                    ;3
       lda    ($F4),y                 ;5
       sta    GRP1                    ;3
       lda    ($F6),y                 ;5
       sta    GRP0                    ;3
       txa                            ;2
       sta    GRP1                    ;3
       sta    GRP0                    ;3
       dec    $89                     ;5
       dey                            ;2
;waste 14 cycles
       NOP                            ;2
       NOP                            ;2
       NOP                            ;2
       NOP                            ;2
       NOP                            ;2
       NOP                            ;2
       NOP                            ;2
       lda    ($F8),y                 ;5
       tax                            ;2
       lda    ($EE),y                 ;5
       sta    GRP0                    ;3
       lda    ($F0),y                 ;5
       sta    GRP1                    ;3
       lda    ($F2),y                 ;5
       sta    GRP0                    ;3
       lda    ($F4),y                 ;5
       sta    GRP1                    ;3
       lda    ($F6),y                 ;5
       sta    GRP0                    ;3
       txa                            ;2
       sta    GRP1                    ;3
       sta    GRP0                    ;3
       lda    $89                     ;3
       cmp    #$04                    ;2
       bcc    LD28A                   ;2
       lda    #$00                    ;2
       bcs    LD28D                   ;2
LD28A:
       NOP                            ;2
       lda    #$02                    ;2
LD28D:
       sta.w  ENABL                   ;4
       dey                            ;2
       bpl    LD23E                   ;2
       iny                            ;2
       sty    GRP0                    ;3
       sty    GRP1                    ;3
       sty    HMCLR                   ;3
       sty    GRP0                    ;3
       sty    GRP1                    ;3
       sta    WSYNC                   ;3
       dec    $89                     ;5
       lda    $89                     ;3
       cmp    #$03                    ;2
       bcc    LD2AB                   ;2
       tya                            ;2 A/Y=0
       bcs    LD2AE                   ;2
LD2AB:
       NOP                            ;2
       lda    #$02                    ;2
LD2AE:
       sta    ENABL                   ;3
       ldy    $80                     ;3
       dec    Icolor                  ;5 switch to next color band
       lda    WSYNC                   ;3
       ora    RSYNC                   ;3
       asl                            ;2
       bmi    LD2C1                   ;2
       NOP                            ;2 waste
       NOP                            ;2 waste
       sta    Waste2                  ;3 waste
       bpl    LD2C8                   ;2 always branch



LD2C1:
       lda    $82                     ;3
       ora    LDE61,y                 ;4
       sta    $82                     ;3
LD2C8:
       sta    CXCLR                   ;3
       dey                            ;2
       dec    $8C                     ;5
       bpl    LD2D7                   ;2
       lda    #$00                    ;2
       sta    VDELP0                  ;3
       sta    VDELP1                  ;3
       beq    LD34E                   ;2 always branch
LD2D7:
       dec    $89                     ;5
       lda    $89                     ;3
       ldx    #$02                    ;2
       cmp    #$03                    ;2
       bcc    LD2E3                   ;2
       ldx    #$00                    ;2
LD2E3:
       stx    ENABL                   ;3
LD2E5:
       sty    $80                     ;3
       lda.wy $92,y                   ;4
       sta    $F8                     ;3
       ldx    #$F4                    ;2
LD2EE:
       lsr    $F8                     ;5
       bcc    LD2F9                   ;2
       lda    InvaderGFXtbl,y         ;4
       adc    $8B                     ;3
       bne    LD2FF                   ;2
;change invaders to point GFX
LD2F9:
       lda    Lives                   ;3
       and    #$98                    ;2 keep only bits 7,4, and 8 (A=$00, $90, or $98)
       NOP                            ;2
       NOP                            ;2
LD2FF:
       sta    Ctrl,x                  ;4
       inx                            ;2
       inx                            ;2
       bmi    LD2EE                   ;2
       dec    $89                     ;5
       lda    $89                     ;3
       ldx    #$02                    ;2
       cmp    #$03                    ;2
       bcc    LD311                   ;2
       ldx    #$00                    ;2
LD311:
       stx    ENABL                   ;3
       lda    $C8                     ;3
       and    #$38                    ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       cmp    $80                     ;3
       bne    LD332                   ;2
       lda    $C8                     ;3
       and    #$07                    ;2
       asl                            ;2
       tax                            ;2
       NOP                            ;2
       NOP                            ;2
       NOP                            ;2
       NOP                            ;2
       NOP                            ;2
       NOP                            ;2
       NOP                            ;2
       NOP                            ;2
       lda    #<ExplosionGFX          ;2
       sta    $EE,x                   ;4
       bne    LD337                   ;2
LD332:
       ldx    #$05                    ;2
LD334:
       dex                            ;2
       bpl    LD334                   ;2
LD337:
       sta    HMCLR                   ;3
       jmp    LDAAD                   ;3



LD33C:
       ldx    Icolor                  ;3 load current pointer
       ldy    SpriteGFX,x             ;4 ...and get color band info
;change color
       lda    $AA                     ;3
       and    #$06                    ;2
       bne    LD355                   ;2
;waste 12 more
       dec    Waste2                  ;5
       NOP                            ;2
       NOP                            ;2
       clc                            ;2
       bcc    LD35E                   ;2 always branch
LD34E:
       dec    $8E                     ;5
       bmi    LD368                   ;2
       jmp    LDA70                   ;3
LD355:
       lda    FLAG2                   ;3 check for color change
       and    #$40                    ;2 (when player has been hit)
       tay                            ;2
       lda    RED,y                   ;4
       tay                            ;2
LD35E:
       NOP                            ;2
       sty    COLUP0                  ;3 store the color of this row
       sty    COLUP1                  ;3
       ldy    #$09                    ;2
       jmp    LD232                   ;3



LD368:
       bit    $98                     ;3
       bvs    LD36F                   ;2
       jmp    LD428                   ;3
LD36F:
       lda    FLAG2                   ;3
       and    #$40                    ;2
       tay                            ;2
       lda    RED,y                   ;4 get shield color
       sta    COLUP0                  ;3
       lda    #$01                    ;2
       sta    $8E                     ;3
       lsr                            ;2
       sta    $EF                     ;3
       sta    $F1                     ;3
       sta    $F3                     ;3
       lda    #$AB                    ;2
       sta    $EE                     ;3
       lda    #$B4                    ;2
       sta    $F0                     ;3
       lda    #$BD                    ;2
       sta    $F2                     ;3
       lda    #$11                    ;2
       sta    WSYNC                   ;3
       sta    HMCLR                   ;3
       sta    $F4                     ;3
       lda    $85                     ;3
       sta    HMP0                    ;3
       and    #$0F                    ;2
       tay                            ;2
LD39F:
       dey                            ;2
       bpl    LD39F                   ;2
       sta    RESP0                   ;3
       dec    $89                     ;5
       lda    $89                     ;3
       cmp    #$03                    ;2
       lda    #$02                    ;2
       bcc    LD3AF                   ;2
       lsr                            ;2
LD3AF:
       sta    ENABL                   ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       ldy    #$00                    ;2
       lda    $85                     ;3
       bpl    LD3BD                   ;2
       lda    $85                     ;3
LD3BD:
       and    #$0F                    ;2
       tax                            ;2
       dex                            ;2
       dex                            ;2
LD3C2:
       dex                            ;2
       bpl    LD3C2                   ;2
LD3C5:
       lda    ($EE),y                 ;5
       sta    GRP0                    ;3
       NOP                            ;2
       lda    ($F0),y                 ;5
       sta    GRP0                    ;3
       lda    ($F2),y                 ;5
       sta    GRP0                    ;3
       dec    $F4                     ;5
       bmi    LD3FC                   ;2
       lda    $F4                     ;3
       lsr                            ;2
       bcc    LD3E3                   ;2
       iny                            ;2
       lda    #$20                    ;2
LD3DE:
       lsr                            ;2
       bne    LD3DE                   ;2
       beq    LD3C5                   ;2 always branch

LD3E3:
       NOP                            ;2
       NOP                            ;2
       NOP                            ;2
       dec    $89                     ;5
       lda    $89                     ;3
       cmp    #$03                    ;2
       bcc    LD3F2                   ;2
       lda    #$00                    ;2
       bcs    LD3F5                   ;2
LD3F2:
       NOP                            ;2 don't remove
       lda    #$02                    ;2
LD3F5:
       sta    ENABL                   ;3
       NOP                            ;2
       NOP                            ;2
       NOP                            ;2
       bpl    LD3C5                   ;2
LD3FC:
       lda    #$00                    ;2
       sta    GRP0                    ;3
       sta    WSYNC                   ;3
       lda    WSYNC                   ;3
       asl                            ;2
       and    #$80                    ;2
       ora    $82                     ;3
       sta    $82                     ;3
       sta    CXCLR                   ;3
       dec    $89                     ;5
       lda    $89                     ;3
       ldx    #$02                    ;2
       cmp    #$03                    ;2
       bcc    LD419                   ;2
       ldx    #$00                    ;2
LD419:
       stx    ENABL                   ;3
       sta    HMCLR                   ;3
       sta    WSYNC                   ;3
       sta    WSYNC                   ;3
LD421:
       dec    $8E                     ;5
       bmi    LD428                   ;2
       jmp    LD03E                   ;3
LD428:
       bit    $98                     ;3
       bpl    LD42F                   ;2
       jmp    LD50A                   ;3
LD42F:
       dec    $89                     ;5
       lda    $89                     ;3
       ldx    #$02                    ;2
       cmp    #$03                    ;2
       bcc    LD43B                   ;2
       ldx    #$00                    ;2
LD43B:
       stx    ENABL                   ;3
       sta    HMCLR                   ;3
       lda    FLAG2                   ;3
       and    #$40                    ;2
       eor    #$40                    ;2
       tay                            ;2
       lda    CYAN,y                  ;4 get P1 base/score color
       sta    WSYNC                   ;3
       sta    Waste2                  ;3
       sta    COLUP0                  ;3
       lda    $86                     ;3
       sta    HMP0                    ;3
       and    #$0F                    ;2
       tay                            ;2
LD456:
       dey                            ;2
       bpl    LD456                   ;2
       sta    RESP0                   ;3
       lda    FLAG2                   ;3
       and    #$40                    ;2
       tay                            ;2
       lda    YELLOW,y                ;4 get P2 base/score color
       sta    WSYNC                   ;3
       sta    Waste2                  ;3
       sta    COLUP1                  ;3
       lda    $87                     ;3
       sta    HMP1                    ;3
       and    #$0F                    ;2
       tay                            ;2
LD470:
       dey                            ;2
       bpl    LD470                   ;2
       sta    RESP1                   ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       dec    $89                     ;5
       lda    $89                     ;3
       ldx    #$02                    ;2
       cmp    #$03                    ;2
       bcc    LD485                   ;2
       ldx    #$00                    ;2
LD485:
       stx    ENABL                   ;3
       lda    #$00                    ;2
       bit.w  DifSw                   ;4
       bvc    LD490                   ;2
       lda    #$05                    ;2
LD490:
       sta    NUSIZ1                  ;3 save left difficulty to both size registers
       sta    NUSIZ0                  ;3
       lda    $98                     ;3
       and    #$10                    ;2
       beq    LD49C                   ;2
       lda    #<BaseGFX               ;2
LD49C:
       sta    $F4                     ;3
       lda    $98                     ;3
       and    #$20                    ;2
       beq    LD4A6                   ;2
       lda    #<BaseGFX               ;2
LD4A6:
       sta    $F6                     ;3
       lda    $AA                     ;3
       lsr                            ;2
       sta    WSYNC                   ;3
       bcc    LD4C9                   ;2
       lda    Lives                   ;3
       and    #$03                    ;2
       tax                            ;2
       lda    DigitGFXtbl,x           ;4 digits 0 to 3 (extra lives)
       sta    $F8                     ;3
       lda    #$DC                    ;2
       sta    $F9                     ;3
       lda    Framecount              ;3
       and    #$08                    ;2
       bne    LD4C7                   ;2
       sta    $F4                     ;3
       sta    $F6                     ;3
LD4C7:
       bpl    LD4DF                   ;2
LD4C9:
       lda    #$00                    ;2
       sta    $F8                     ;3
       ldy    #<ExplosionGFX          ;2
       lda    $AA                     ;3
       and    #$04                    ;2
       beq    LD4D7                   ;2
       sty    $F4                     ;3
LD4D7:
       lda    $AA                     ;3
       and    #$02                    ;2
       beq    LD4DF                   ;2
       sty    $F6                     ;3
LD4DF:
       ldy    #$09                    ;2
LD4E1:
       sta    WSYNC                   ;3
       lda    #$00                    ;2
       sta    PF0                     ;3
       lda    ($F4),y                 ;5
       sta    GRP0                    ;3
       lda    ($F6),y                 ;5
       sta    GRP1                    ;3
       NOP                            ;2
       NOP                            ;2
       NOP                            ;2
       lda    ($F8),y                 ;5
       sta    PF0                     ;3
       tya                            ;2
       lsr                            ;2
       bcc    LD507                   ;2
       dec    $89                     ;5
       lda    $89                     ;3
       cmp    #$03                    ;2
       lda    #$02                    ;2
       bcc    LD505                   ;2
       lsr                            ;2
LD505:
       sta    ENABL                   ;3
LD507:
       dey                            ;2
       bpl    LD4E1                   ;2
LD50A:
       lda    FLAG2                   ;3
       and    #$40                    ;2
       tax                            ;2
       lda    RED,x                   ;4 get ground color (thin line)
       sta    WSYNC                   ;3
       sta    COLUBK                  ;3
       lda    #$00                    ;2
       sta    PF0                     ;3
       sta    ENABL                   ;3
       sta    GRP0                    ;3
       sta    GRP1                    ;3
       sta    NUSIZ0                  ;3
       sta    NUSIZ1                  ;3
       lda.w  $81                     ;4
       sta    RESP0                   ;3
       NOP                            ;2
       ldx    #$05                    ;2
LD52C:
       dex                            ;2
       bne    LD52C                   ;2
       lda    #$20                    ;2
       sta    RESP1                   ;3
       sta    HMCLR                   ;3
       sta    HMP1                    ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       stx    COLUBK                  ;3 clear ground strip
       ldx    ColorShift              ;3 (shift color)
       lda    Lives                   ;3
       and    #$03                    ;2
       sta    WSYNC                   ;3
       stx    COLUBK                  ;3 save border color w/attract
       tax                            ;2
       lda    Marker1GFXtbl,x         ;4 save P1's marker
       sta    GRP0                    ;3
       bit    Lives                   ;3
       bvs    LD553                   ;2
       ldx    #$00                    ;2
LD553:
       lda    Marker2GFXtbl,x         ;4 save P2's marker
       sta    GRP1                    ;3
;check if blinking mothership bit needs to be set
       lda    $9E                     ;3
       cmp    #$B4                    ;2
       bne    LD570                   ;2
       lda    FLAG2                   ;3
       and    #$EF                    ;2
       sta    FLAG2                   ;3
       lda    SHOT                    ;3
       and    #$0F                    ;2
       bne    LD570                   ;2
       lda    FLAG2                   ;3
       ora    #$10                    ;2
       sta    FLAG2                   ;3
LD570:
       ldx    #$04                    ;2
LD572:
       sta    $F3,x                   ;4
       dex                            ;2
       bne    LD572                   ;2
       jmp    LD843                   ;3
LD57A:
       lda    $AA                     ;3
       and    #$81                    ;2
       bne    LD5C8                   ;2
       bit    WSYNC                   ;3
       bvc    LD58A                   ;2
       lda    #$04                    ;2
       bit    $AA                     ;3
       beq    LD595                   ;2
LD58A:
       dex                            ;2
       bit    RSYNC                   ;3
       bvc    LD5C8                   ;2
       lda    #$02                    ;2
       bit    $AA                     ;3
       bne    LD5C8                   ;2
LD595:
       ora    $AA                     ;3
       sta    $AA                     ;3
       sta    $C6                     ;3
       lda    Framecount              ;3
       and    #$01                    ;2
       ora    #$80                    ;2
       sta    Framecount              ;3
       lda    #$06                    ;2
       jmp    LD975                   ;3
LD5A8:
       lda    $EA                     ;3
       bne    LD5C8                   ;2
       bit    $DB                     ;3
       bvc    LD5C8                   ;2
       lda    #$05                    ;2
       sta    Mship                   ;3
;bugfix...swap X
       inx                            ;2
       txa                            ;2
       and    #$01                    ;2
       tax                            ;2
       sed                            ;2
       clc                            ;2
       lda    P1scoreLo,x             ;4
       adc    #$20                    ;2 score 200 points from dead opponent...
       sta    P1scoreLo,x             ;4
       lda    P1scoreHi,x             ;4
       adc    #$00                    ;2
       sta    P1scoreHi,x             ;4
       cld                            ;2
LD5C8:
       ldx    INTIM                   ;4
       bne    LD5C8                   ;2
       lda    #$C8                    ;2
       sta    TIM8T                   ;4
       stx    $F1                     ;3
       lda    $82                     ;3
       sta    WSYNC                   ;3
       stx    COLUBK                  ;3
       stx    GRP0                    ;3
       stx    GRP1                    ;3
       bpl    LD63F                   ;2
       lda    Framecount              ;3
       lsr                            ;2
       bcc    LD5E7                   ;2
       ldx    #$04                    ;2
LD5E7:
       lda    $D1,x                   ;4
       cmp    #$49                    ;2
       bcc    LD5F1                   ;2
       cmp    #$58                    ;2
       bcc    LD5F2                   ;2
LD5F1:
       inx                            ;2
LD5F2:
       cpx    #$03                    ;2
       bcc    LD602                   ;2
       lda    #$09                    ;2
       sta    $EF                     ;3
       lda    #$FF                    ;2
       sta    $F2                     ;3
       lda    #$7F                    ;2
       bne    LD60C                   ;2 always branch

LD602:
       lda    #$FF                    ;2
       sta    $EF                     ;3
       lda    #$01                    ;2
       sta    $F2                     ;3
       lda    #$F6                    ;2
LD60C:
       sta    $D1,x                   ;4
       lda    $D3,x                   ;4
       ldy    #$03                    ;2
       clc                            ;2
       sbc    $9B                     ;3
LD615:
       dey                            ;2
       clc                            ;2
       adc    #$E0                    ;2
       bpl    LD615                   ;2
       adc    #$20                    ;2
       tax                            ;2
       lda    LDFF5,y                 ;4
       sta    $F0                     ;3
       ldy    $EF                     ;3
LD625:
       tya                            ;2
       clc                            ;2
       adc    $F2                     ;3
       tay                            ;2
       lda    LDD79,x                 ;4
       eor    #$FF                    ;2
       and    ($F0),y                 ;5
       beq    LD625                   ;2
       jmp    LD203                   ;3
LD636:
       dey                            ;2
       jmp    LD93F                   ;3
LD63A:
       iny                            ;2
       iny                            ;2
       jmp    LD9FF                   ;3
LD63F:
       lda    Framecount              ;3
       lsr                            ;2
       bcs    LD647                   ;2
       jmp    LD79A                   ;3
LD647:
       bit    $82                     ;3
       bvc    LD68D                   ;2
       lda    $C8                     ;3
       and    #$39                    ;2
       cmp    #$39                    ;2
       beq    LD68D                   ;2
       lda    #$39                    ;2
       sta    $C8                     ;3
       ldx    #$01                    ;2
       lda    $98                     ;3
       and    #$04                    ;2
       bne    LD660                   ;2
       dex                            ;2
LD660:
       lda    #$04                    ;2 mothership...
       sta    $C6                     ;3
       lda    FLAG2                   ;3
       and    #$10                    ;2
       beq    LD66E                   ;2 branch if mystery ship
       lda    #$08                    ;2 blinking.
       bne    LD675                   ;2 always branch

LD66E:
       lda    Framecount              ;3
       eor    SHOT                    ;3
       lsr                            ;2
       and    #$07                    ;2
LD675:
       tay                            ;2
       lda    MothershipPointsTbl,y   ;4
       sty    Mship                   ;3
       sed                            ;2
       clc                            ;2
       adc    P1scoreLo,x             ;4
       sta    P1scoreLo,x             ;4
       lda    P1scoreHi,x             ;4
       adc    #$00                    ;2
       sta    P1scoreHi,x             ;4
       cld                            ;2
       lda    #$05                    ;2
       jmp    LDAEA                   ;3
LD68D:
       lda    #$06                    ;2
       sta    $F0                     ;3
LD691:
       dec    $F0                     ;5
       bpl    LD698                   ;2
       jmp    LD000                   ;3
LD698:
       ldx    $F0                     ;3
       lda    $82                     ;3
       and    LDE61,x                 ;4
       beq    LD691                   ;2
       ldy    #$01                    ;2
       lda    #$35                    ;2
       sec                            ;2
       sbc    LDFDB,x                 ;4
       clc                            ;2
       adc    $90                     ;3
       cmp    #$52                    ;2
       bcs    LD691                   ;2
       cmp    $D5                     ;3
       bcs    LD6BB                   ;2
       adc    #$0D                    ;2
       cmp    $D5                     ;3
       bcc    LD6BB                   ;2
       dey                            ;2
LD6BB:
       sty    $EE                     ;3
       ldx    $EE                     ;3
       ldy    #$FF                    ;2
       lda    $9A                     ;3
       clc                            ;2
       adc    #$FD                    ;2
LD6C6:
       iny                            ;2
       adc    #$10                    ;2
       cmp    $D7,x                   ;4
       bcc    LD6C6                   ;2
       sty    $EF                     ;3
       ldx    $F0                     ;3
       lda    LDE61,y                 ;4
       and    $92,x                   ;4
       beq    LD691                   ;2
       eor    $92,x                   ;4
       sta    $92,x                   ;4
       ldy    $EE                     ;3
       lda    LDE63,y                 ;4
       ldy    #$01                    ;2
       and    $98                     ;3
       bne    LD6E8                   ;2
       dey                            ;2
LD6E8:
       lda    #$02                    ;2
       cmp    $CB                     ;3
       bcc    LD6F6                   ;2
       sta    $CB                     ;3
       lda    #$00                    ;2
       sta    $CD                     ;3
       sta    $CF                     ;3
LD6F6:
;check mothership
       lda    $9E                     ;3
       cmp    #$B4                    ;2
       bne    LD702                   ;2
;clear off the rainbow bonus GFX value (in bits 7,4, and 3)
       lda    Lives                   ;3
       and    #$67                    ;2
       sta    Lives                   ;3
LD702:
;invader shot...
       sed                            ;2
       clc                            ;2
       lda    InvaderPointsTbl,x      ;4
       adc.wy P1scoreLo,y             ;4
       sta.wy P1scoreLo,y             ;5
       lda.wy P1scoreHi,y             ;4
       adc    #$00                    ;2
       sta.wy P1scoreHi,y             ;5
       cld                            ;2
       dec    Invaders                ;5 decrease invader counter
       bne    LD778                   ;2 branch if still some left
;all invaders shot
       cpx    #$01                    ;2 last invader from the 2nd-to-lowest row?
       beq    LD736                   ;2 branch if so
       bcs    LD760                   ;2 branch if higher row
;bottom invader shot
       lda.wy Lives,y                 ;4
       and    #$20                    ;2
       bne    LD73D                   ;2 branch if not 1st column
       clc                            ;2
       sed                            ;2
       lda.wy P1scoreHi,y             ;4
       adc    #$01                    ;2 add 1000 points
       sta.wy P1scoreHi,y             ;5
       cld                            ;2
       lda    #<Rainbow1000GFX        ;2 (value #$98)
       bne    LD752                   ;2 always branch
LD736:
       lda.wy Lives,y                 ;4
       and    #$20                    ;2
       bne    LD760                   ;2 branch if not 1st column
LD73D:
       clc                            ;2
       sed                            ;2
       lda.wy P1scoreLo,y             ;4
       adc    #$50                    ;2 add 500 points
       sta.wy P1scoreLo,y             ;5
       lda.wy P1scoreHi,y             ;4
       adc    #$00                    ;2
       sta.wy P1scoreHi,y             ;5
       cld                            ;2
       lda    #<Rainbow500GFX         ;2 (value #$90)
LD752:
       ora    Lives                   ;3
       sta    Lives                   ;3
       lda    #<RBsound               ;2 <-rainbow bonus sound index value
       sta    $CD                     ;3
       lda    Ctrl                    ;3
       ora    #$80                    ;2 set ! flag
       bne    LD762                   ;2
LD760:
       lda    Ctrl                    ;3
LD762:
       ora    #$40                    ;2 In either case, set intermission flag
       sta    Ctrl                    ;3
       lda.wy Lives,y                 ;4
       and    #$DF                    ;2 clear off rainbow double bit for next round
       sta.wy Lives,y                 ;5
       lda    $AA                     ;3
       ora    #$08                    ;2
       sta    $AA                     ;3
       lda    #$61                    ;2
       sta    Framecount              ;3
LD778:
       lda    $C8                     ;3
       and    #$39                    ;2
       cmp    #$39                    ;2
       bne    LD788                   ;2
       lda    #$B4                    ;2
       sta    $9E                     ;3
       lda    #$00                    ;2
       sta    $CC                     ;3
LD788:
       txa                            ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       ora    $EF                     ;3
       sta    $C8                     ;3
       lda    #$F6                    ;2
       ldy    $EE                     ;3
       sta.wy $D5,y                   ;5
       jmp    LD691                   ;3


LD79A:
       lsr                            ;2
       bcs    LD7CA                   ;2
       lda    $C8                     ;3
       and    #$39                    ;2
       cmp    #$39                    ;2
       beq    LD7CA                   ;2
       lda    $9E                     ;3
       cmp    #$B4                    ;2
       beq    LD7CA                   ;2
       lda    $98                     ;3
       lsr                            ;2
       bcs    LD7B6                   ;2
       dec    $9E                     ;5
       bne    LD7CA                   ;2
       beq    LD7BE                   ;2 always branch

LD7B6:
       inc    $9E                     ;5
       lda    $9E                     ;3
       cmp    #$98                    ;2
       bcc    LD7CA                   ;2
LD7BE:
       lda    #$B4                    ;2
       sta    $9E                     ;3
       lda    #$00                    ;2
       sta    $CC                     ;3
       lda    #$04                    ;2
       sta    $C6                     ;3
LD7CA:
       lda    SWCHA                   ;4
       tay                            ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       sta    $EE                     ;3
       and    #$80                    ;2
       sta    $81                     ;3
       lda    $DB                     ;3
       cmp    #$90                    ;2
       bne    LD7E6                   ;2
       bit    $AA                     ;3
       bvc    LD7F0                   ;2
       ldy    $EE                     ;3
       bvs    LD7F0                   ;2
LD7E6:
       and    #$02                    ;2
       beq    LD7F0                   ;2
       tya                            ;2
       and    #$40                    ;2
       ora    $81                     ;3
       tay                            ;2
LD7F0:
       ldx    #$01                    ;2
LD7F2:
       lda    $AA                     ;3
       and    LDE5F,x                 ;4
       bne    LD811                   ;2
       bit    $EE                     ;3
       bmi    LD7FF                   ;2
       inc    $9C,x                   ;6
LD7FF:
       bvs    LD803                   ;2
       dec    $9C,x                   ;6
LD803:
       lda    $9C,x                   ;4
       cmp    #$76                    ;2
       bcc    LD80B                   ;2
       dec    $9C,x                   ;6
LD80B:
       cmp    #$23                    ;2
       bcs    LD811                   ;2
       inc    $9C,x                   ;6
LD811:
       sty    $EE                     ;3
       dex                            ;2
       bpl    LD7F2                   ;2
       lda    Framecount              ;3
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       bcs    LD840                   ;2
       lda    Variation               ;3
       lsr                            ;2
       bcc    LD840                   ;2
       lda    $AA                     ;3
       and    #$10                    ;2
       beq    LD832                   ;2
       inc    $9B                     ;5
       lda    $9B                     ;3
       cmp    #$35                    ;2
       bcc    LD840                   ;2
       bcs    LD83A                   ;2 always branch

LD832:
       dec    $9B                     ;5
       lda    $9B                     ;3
       cmp    #$21                    ;2
       bcs    LD840                   ;2
LD83A:
       lda    $AA                     ;3
       eor    #$10                    ;2
       sta    $AA                     ;3
LD840:
       jmp    LD000                   ;3 jump back (and bankswitch to next section)


LD843:
       lda    $EA                     ;3
       bne    LD861                   ;2
       inc    $CD                     ;5
       ldy    $CB                     ;3
       beq    LD861                   ;2
       cpy    #$05                    ;2
       beq    LD855                   ;2
       cpy    #$02                    ;2
       bne    LD86F                   ;2
LD855:
       ldy    $CD                     ;3
       cpy    #$08                    ;2
       bne    LD899                   ;2
       lda    $CB                     ;3
       cmp    #$05                    ;2
       beq    LD865                   ;2
LD861:
       lda    #$00                    ;2
       sta    $CB                     ;3
LD865:
       lda    #$00                    ;2
       sta    AUDV0                   ;3
       sta    $CD                     ;3
       sta    $CF                     ;3
       beq    LD8BB                   ;2 always branch

LD86F:
       lda    SoundTbl-1,y            ;4
       sta    $EE                     ;3
       lda    #$DD                    ;2
       sta    $EF                     ;3
       ldy    $CF                     ;3
       lda    ($EE),y                 ;5
       cmp    $CD                     ;3
       bne    LD8BB                   ;2
       iny                            ;2
       lda    ($EE),y                 ;5
       bmi    LD861                   ;2
       cmp    #$3F                    ;2
       beq    LD865                   ;2
       sta    AUDF0                   ;3
       iny                            ;2
       lda    ($EE),y                 ;5
       sta    AUDC0                   ;3
       iny                            ;2
       lda    ($EE),y                 ;5
       iny                            ;2
       sty    $CF                     ;3
       jmp    LD8B9                   ;3

LD899:
       lda    InvaderHitSound,y       ;4
       sta    AUDF1                   ;3
       and    #$0F                    ;2
       sta    AUDF0                   ;3
       lda    #$0D                    ;2
       sta    AUDC0                   ;3
       lda    #$05                    ;2
       sta    AUDC1                   ;3
       tya                            ;2
       eor    #$07                    ;2
       sta    AUDV1                   ;3
       lda    #$CB                    ;2
       cmp    #$05                    ;2
       lda    #$04                    ;2
       bcc    LD8B9                   ;2
       lda    #$08                    ;2
LD8B9:
       sta    AUDV0                   ;3
LD8BB:
       lda    $EA                     ;3
       bne    LD8D9                   ;2
       inc    $CE                     ;5
       ldy    $CC                     ;3
       beq    LD8D9                   ;2
       cpy    #$05                    ;2
       beq    LD8CD                   ;2
       cpy    #$02                    ;2
       bne    LD8E7                   ;2
LD8CD:
       ldy    $CE                     ;3
       cpy    #$08                    ;2
       bne    LD911                   ;2
       lda    $CC                     ;3
       cmp    #$05                    ;2
       beq    LD8DD                   ;2
LD8D9:
       lda    #$00                    ;2
       sta    $CC                     ;3
LD8DD:
       lda    #$00                    ;2
       sta    AUDV1                   ;3
       sta    $CE                     ;3
       sta    $D0                     ;3
       beq    LD93C                   ;2 always branch

LD8E7:
       lda    SoundTbl-1,y            ;4
       sta    $EE                     ;3
       lda    #$DD                    ;2
       sta    $EF                     ;3
       ldy    $D0                     ;3
       lda    ($EE),y                 ;5
       cmp    $CE                     ;3
       bne    LD93C                   ;2
       iny                            ;2
       lda    ($EE),y                 ;5
       bmi    LD8D9                   ;2
       cmp    #$3F                    ;2
       beq    LD8DD                   ;2
       sta    AUDF1                   ;3
       iny                            ;2
       lda    ($EE),y                 ;5
       sta    AUDC1                   ;3
       iny                            ;2
       lda    ($EE),y                 ;5
       iny                            ;2
       sty    $D0                     ;3
       jmp    LD93A                   ;3



;various redundancy routines
LD911:
       lda    InvaderHitSound,y       ;4
       sta    AUDF1                   ;3
       lda    #$05                    ;2
       sta    AUDC1                   ;3
       lda    InvaderHitSound,y       ;4
       sta    AUDF1                   ;3
       and    #$0F                    ;2
       sta    AUDF0                   ;3
       lda    #$0D                    ;2
       sta    AUDC0                   ;3
       lda    #$05                    ;2
       sta    AUDC1                   ;3
       tya                            ;2
       eor    #$07                    ;2
       sta    AUDV1                   ;3
       lda    #$CB                    ;2
       cmp    #$05                    ;2
       lda    #$04                    ;2
       bcc    LD93A                   ;2
       lda    #$08                    ;2
LD93A:
       sta    AUDV1                   ;3
LD93C:
       jmp    LD57A                   ;3




LD93F:
       cpy    #$09                    ;2
       bcs    LD972                   ;2
       stx    $EE                     ;3
       inx                            ;2
       lda    $C7                     ;3
       asl                            ;2
       asl                            ;2
       clc                            ;2
       adc    $C7                     ;3
       clc                            ;2
       adc    #$59                    ;2
       sta    $C7                     ;3
       and    #$20                    ;2
       bne    LD958                   ;2
       dex                            ;2
       dex                            ;2
LD958:
       cpx    #$08                    ;2
       bcs    LD969                   ;2
       lda    $C7                     ;3
       cmp    #$C0                    ;2
       bcc    LD969                   ;2
       lda    ($F0),y                 ;5
       and    LDD79,x                 ;4
       sta    ($F0),y                 ;6
LD969:
       ldx    $EE                     ;3
       lda    ($F0),y                 ;5
       and    LDD79,x                 ;4
       sta    ($F0),y                 ;6
LD972:
       jmp    LD63A                   ;3




LD975:
       cmp    $CB                     ;3
       bcc    LD981                   ;2
       sta    $CB                     ;3
       lda    #$00                    ;2
       sta    $CD                     ;3
       sta    $CF                     ;3
LD981:
       jmp    LD5A8                   ;3




LD984:
       lda    P2scoreLo               ;3
       and    #$0F                    ;2
       sta    $F0                     ;3
       asl                            ;2
       asl                            ;2
       adc    $F0                     ;3
       adc    #<ULdigits              ;2
       sta    $F7                     ;3
       lda    P2scoreLo               ;3
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       sta    $F0                     ;3
       asl                            ;2
       asl                            ;2
       adc    $F0                     ;3
       adc    #<LTdigits              ;2 (zero)
       sta    $F6                     ;3
       lda    P1scoreLo               ;3
       and    #$0F                    ;2
       sta    $F0                     ;3
       asl                            ;2
       asl                            ;2
       adc    $F0                     ;3
       adc    #<ULdigits              ;2
       sta    $F5                     ;3
       lda    P1scoreLo               ;3
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       sta    $F0                     ;3
       asl                            ;2
       asl                            ;2
       adc    $F0                     ;3
       adc    #<LTdigits              ;2 (zero)
       sta    $F4                     ;3
       lda    P2scoreHi               ;3
       and    #$0F                    ;2
       sta    $F0                     ;3
       asl                            ;2
       asl                            ;2
       adc    $F0                     ;3
       adc    #<LUdigits              ;2
       sta    $F3                     ;3
       lda    P2scoreHi               ;3
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       sta    $F0                     ;3
       asl                            ;2
       asl                            ;2
       adc    $F0                     ;3
       adc    #<UTdigits              ;2
       sta    $F2                     ;3
       lda    P1scoreHi               ;3
       and    #$0F                    ;2
       sta    $F0                     ;3
       asl                            ;2
       asl                            ;2
       adc    $F0                     ;3
       adc    #<LUdigits              ;2
       sta    $F1                     ;3
       lda    P1scoreHi               ;3
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       sta    $F0                     ;3
       asl                            ;2
       asl                            ;2
       adc    $F0                     ;3
       adc    #<UTdigits              ;2
       sta    $F0                     ;3
       jmp    LD07B                   ;3




LD9FF:
       cpy    #$09                    ;2
       bcs    LDA32                   ;2
       stx    $EE                     ;3
       inx                            ;2
       lda    $C7                     ;3
       asl                            ;2
       asl                            ;2
       clc                            ;2
       adc    $C7                     ;3
       clc                            ;2
       adc    #$59                    ;2
       sta    $C7                     ;3
       and    #$20                    ;2
       bne    LDA18                   ;2
       dex                            ;2
       dex                            ;2
LDA18:
       cpx    #$08                    ;2
       bcs    LDA29                   ;2
       lda    $C7                     ;3
       cmp    #$C0                    ;2
       bcc    LDA29                   ;2
       lda    ($F0),y                 ;5
       and    LDD79,x                 ;4
       sta    ($F0),y                 ;6
LDA29:
       ldx    $EE                     ;3
       lda    ($F0),y                 ;5
       and    LDD79,x                 ;4
       sta    ($F0),y                 ;6
LDA32:
       jmp    LD63F                   ;3




LDA35:
       stx    Waste2                  ;3
       dec    $89                     ;5
       lda    $89                     ;3
       bmi    LDA4C                   ;2
       cmp    #$03                    ;2
       lda    #$02                    ;2
       bcc    LDA44                   ;2
       lsr                            ;2
LDA44:
       sta    ENABL                   ;3
       sta    WSYNC                   ;3
       sta    HMCLR                   ;3
       bpl    LDA69                   ;2
LDA4C:
       clc                            ;2
       adc    $8A                     ;3
       sta    $89                     ;3
       lda    #$00                    ;2
       sta    WSYNC                   ;3
       sta    HMCLR                   ;3
       sta    ENABL                   ;3
       lda    $88                     ;3
       sta    HMBL                    ;3
       and    #$0F                    ;2
       tay                            ;2
LDA60:
       dey                            ;2
       bpl    LDA60                   ;2
       sta    RESBL                   ;3
       lda    #$7C                    ;2
       sta    $8A                     ;3
LDA69:
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       jmp    LD1F1                   ;3




LDA70:
       stx    Waste2                  ;3
       dec    $89                     ;5
       lda    $89                     ;3
       bmi    LDA87                   ;2
       cmp    #$03                    ;2
       lda    #$02                    ;2
       bcc    LDA7F                   ;2
       lsr                            ;2
LDA7F:
       sta    ENABL                   ;3
       sta    WSYNC                   ;3
       sta    HMCLR                   ;3
       bpl    LDAA4                   ;2
LDA87:
       clc                            ;2
       adc    $8A                     ;3
       sta    $89                     ;3
       lda    #$00                    ;2
       sta    WSYNC                   ;3
       sta    HMCLR                   ;3
       sta    ENABL                   ;3
       lda    $88                     ;3
       sta    HMBL                    ;3
       and    #$0F                    ;2
       tay                            ;2
LDA9B:
       dey                            ;2
       bpl    LDA9B                   ;2
       sta    RESBL                   ;3
       lda    #$7C                    ;2
       sta    $8A                     ;3
LDAA4:
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       stx    Waste2                  ;3
       jmp    LD34E                   ;3




LDAAD:
       stx    Waste2                  ;3
       dec    $89                     ;5
       lda    $89                     ;3
       bmi    LDAC4                   ;2
       cmp    #$03                    ;2
       lda    #$02                    ;2
       bcc    LDABC                   ;2
       lsr                            ;2
LDABC:
       sta    ENABL                   ;3
       sta    WSYNC                   ;3
       sta    HMCLR                   ;3
       bpl    LDAE1                   ;2
LDAC4:
       clc                            ;2
       adc    $8A                     ;3
       sta    $89                     ;3
       lda    #$00                    ;2
       sta    WSYNC                   ;3
       sta    HMCLR                   ;3
       sta    ENABL                   ;3
       lda    $88                     ;3
       sta    HMBL                    ;3
       and    #$0F                    ;2
       tay                            ;2
LDAD8:
       dey                            ;2
       bpl    LDAD8                   ;2
       sta    RESBL                   ;3
       lda    #$7C                    ;2
       sta    $8A                     ;3
LDAE1:
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       stx    Waste2                  ;3
       jmp    LD33C                   ;3




LDAEA:
       cmp    $CC                     ;3
       bcc    LDAF6                   ;2
       sta    $CC                     ;3
       lda    #$00                    ;2
       sta    $CE                     ;3
       sta    $D0                     ;3
LDAF6:
       jmp    LD68D                   ;3


  IF PLUSROM = 0
       ORG $1AF9
       RORG $DAF9
;extra bytes...use to give kudos
       .byte ";Space Invaders Deluxe"
       .byte ";Hack by:Kurt(Nukey Shay)Howe"
       .byte ";Space Invaders by Rick Maurer (c)1980 Atari"
       .byte ";Special thanks to:"
       .byte ";Franklin(NE146)Cruz' 'Space Invaders' for paving the way"
       .byte ";Rob(raindog)Kudla's 'Space Invaders Arcade' for refining that idea"
       .byte ";Jeff(Yak)Minter's 'Beast Invaders' for supplying double-shot capability"
       .byte ";Albert(Albert)Yarusso, Alex(Alex)Bilstein, and AtariAge"
       .byte ";for giving us a place to show off ;)"
       .byte ";And the members of AtariAge.com for playtesting and support!"
       .byte ";"
  ELSE
PlusROM_API:
    .byte "a", 0, "h.firmaplus.de", 0

MothershipPointsTbl:
       .byte $05 ; |     X X| $DFE2
       .byte $05 ; |     X X| $DFE3
       .byte $10 ; |   X    | $DFE4
       .byte $10 ; |   X    | $DFE5
       .byte $15 ; |   X X X| $DFE6
       .byte $20 ; |  X     | $DFE7
       .byte $25 ; |  X  X X| $DFE8
       .byte $30 ; |  XX    | $DFE9
       .byte $50 ; | X X    | $DFEA

SoundTbl:
       .byte <ThumpSound      ; |X      X| $DFEB
       .byte <DeathSound      ; |XX XXXXX| $DFEC
       .byte <MissileSound    ; |XX  X  X| $DFED
       .byte <MothershipSound ; |X  X XXX| $DFEE
       .byte <Sound           ; |XXX    X| $DFEF
       .byte <DeathSound      ; |XX XXXXX| $DFF0
  ENDIF

       ORG $1CCA
       RORG $DCCA

Marker1GFXtbl:
       .byte $03 ; |      XX| $DCCA
       .byte $0B ; |    X XX| $DCCB
       .byte $2B ; |  X X XX| $DCCC
       .byte $AB ; |X X X XX| $DCCD

Marker2GFXtbl:
       .byte $C0 ; |XX      | $DCCE
       .byte $D0 ; |XX X    | $DCCF
       .byte $D4 ; |XX X X  | $DCD0
       .byte $D5 ; |XX X X X| $DCD1

InvaderGFXtbl:
       .byte <Invader6GFX ; |    X  X| $DCD2
       .byte <Invader5GFX ; |   XXX  | $DCD3
       .byte <Invader4GFX ; | X    XX| $DCD4
       .byte <Invader3GFX ; |  X XXXX| $DCD5
       .byte <Invader2GFX ; | XX X  X| $DCD6
       .byte <Invader1GFX ; | XXXXX  | $DCD7

Digit2GFX:
       .byte $E7 ; |XXX  XXX| $DCD8
       .byte $E7 ; |XXX  XXX| $DCD9
       .byte $24 ; |  X  X  | $DCDA
       .byte $24 ; |  X  X  | $DCDB
Digit3GFX:
       .byte $E7 ; |XXX  XXX| $DCDC shared
       .byte $E7 ; |XXX  XXX| $DCDD shared
       .byte $81 ; |X      X| $DCDE shared
       .byte $81 ; |X      X| $DCDF shared
       .byte $E7 ; |XXX  XXX| $DCE0 shared
       .byte $E7 ; |XXX  XXX| $DCE1 shared
       .byte $81 ; |X      X| $DCE2
       .byte $81 ; |X      X| $DCE3
Digit0GFX:
       .byte $E7 ; |XXX  XXX| $DCE4 shared
       .byte $E7 ; |XXX  XXX| $DCE5 shared
       .byte $A5 ; |X X  X X| $DCE6
       .byte $A5 ; |X X  X X| $DCE7
       .byte $A5 ; |X X  X X| $DCE8
       .byte $A5 ; |X X  X X| $DCE9
       .byte $A5 ; |X X  X X| $DCEA
       .byte $A5 ; |X X  X X| $DCEB
Digit1GFX:
       .byte $E7 ; |XXX  XXX| $DCEC shared
       .byte $E7 ; |XXX  XXX| $DCED shared
       .byte $42 ; | X    X | $DCEE
       .byte $42 ; | X    X | $DCEF
       .byte $42 ; | X    X | $DCF0
       .byte $42 ; | X    X | $DCF1
       .byte $66 ; | XX  XX | $DCF2
       .byte $66 ; | XX  XX | $DCF3
       .byte $42 ; | X    X | $DCF4
       .byte $42 ; | X    X | $DCF5

InvaderPointsTbl:
       .byte $01 ; |       X| $DCF6
       .byte $01 ; |       X| $DCF7
       .byte $02 ; |      X | $DCF8
       .byte $02 ; |      X | $DCF9
       .byte $03 ; |      XX| $DCFA
       .byte $03 ; |      XX| $DCFB

DigitGFXtbl:
       .byte <Digit0GFX ; |XXX  X  | $DCFC
       .byte <Digit1GFX ; |XXX XX  | $DCFD
       .byte <Digit2GFX ; |XX XX   | $DCFE
       .byte <Digit3GFX ; |XX XXX  | $DCFF

MothershipGFX:
MothersGFX3:
MothersGFX4:
       .byte $00 ; |        | $DD00
       .byte $00 ; |        | $DD01
       .byte $00 ; |        | $DD02
       .byte $00 ; |        | $DD03
       .byte $00 ; |        | $DD04
       .byte $00 ; |        | $DD05
       .byte $00 ; |        | $DD06
MysteryGFX1:
       .byte $00 ; |        | $DD07 shared
       .byte $00 ; |        | $DD08
       .byte $42 ; | X    X | $DD09
       .byte $FF ; |XXXXXXXX| $DD0A
       .byte $CC ; |XX  XX  | $DD0B
       .byte $7E ; | XXXXXX | $DD0C
       .byte $3C ; |  XXXX  | $DD0D
       .byte $18 ; |   XX   | $DD0E
MysteryGFX2:
       .byte $00 ; |        | $DD0F shared
       .byte $00 ; |        | $DD10
       .byte $42 ; | X    X | $DD11
       .byte $FF ; |XXXXXXXX| $DD12
       .byte $99 ; |X  XX  X| $DD13
       .byte $7E ; | XXXXXX | $DD14
       .byte $3C ; |  XXXX  | $DD15
       .byte $18 ; |   XX   | $DD16
MysteryGFX3:
       .byte $00 ; |        | $DD17 shared
       .byte $00 ; |        | $DD18
       .byte $42 ; | X    X | $DD19
       .byte $FF ; |XXXXXXXX| $DD1A
       .byte $33 ; |  XX  XX| $DD1B
       .byte $7E ; | XXXXXX | $DD1C
       .byte $3C ; |  XXXX  | $DD1D
       .byte $18 ; |   XX   | $DD1E
MysteryGFX4:
       .byte $00 ; |        | $DD1F shared
       .byte $00 ; |        | $DD20
       .byte $42 ; | X    X | $DD21
       .byte $FF ; |XXXXXXXX| $DD22
       .byte $66 ; | XX  XX | $DD23
       .byte $7E ; | XXXXXX | $DD24
       .byte $3C ; |  XXXX  | $DD25
       .byte $18 ; |   XX   | $DD26
M300pointsGFX:
       .byte $00 ; |        | $DD27 shared
       .byte $07 ; |     XXX| $DD28
       .byte $1D ; |   XXX X| $DD29
       .byte $D5 ; |XX X X X| $DD2A
       .byte $57 ; | X X XXX| $DD2B
       .byte $DC ; |XX XXX  | $DD2C
       .byte $40 ; | X      | $DD2D
       .byte $C0 ; |XX      | $DD2E
M500pointsGFX:
       .byte $00 ; |        | $DD2F shared
       .byte $07 ; |     XXX| $DD30
       .byte $1D ; |   XXX X| $DD31
       .byte $D5 ; |XX X X X| $DD32
       .byte $57 ; | X X XXX| $DD33
       .byte $DC ; |XX XXX  | $DD34
       .byte $80 ; |X       | $DD35
       .byte $C0 ; |XX      | $DD36
MothersGFX1:
       .byte $00 ; |        | $DD37 shared
       .byte $00 ; |        | $DD38
       .byte $7E ; | XXXXXX | $DD39
       .byte $AB ; |X X X XX| $DD3A
       .byte $7E ; | XXXXXX | $DD3B
       .byte $24 ; |  X  X  | $DD3C
       .byte $3C ; |  XXXX  | $DD3D
       .byte $18 ; |   XX   | $DD3E
MothersGFX2:
       .byte $00 ; |        | $DD3F shared
       .byte $00 ; |        | $DD40
       .byte $7E ; | XXXXXX | $DD41
       .byte $D5 ; |XX X X X| $DD42
       .byte $7E ; | XXXXXX | $DD43
       .byte $24 ; |  X  X  | $DD44
       .byte $3C ; |  XXXX  | $DD45
       .byte $18 ; |   XX   | $DD46
M50pointsGFX:
       .byte $00 ; |        | $DD47 shared
       .byte $0E ; |    XXX | $DD48
       .byte $6A ; | XX X X | $DD49
       .byte $2A ; |  X X X | $DD4A
       .byte $6E ; | XX XXX | $DD4B
       .byte $40 ; | X      | $DD4C
       .byte $60 ; | XX     | $DD4D
       .byte $00 ; |        | $DD4E
M100pointsGFX:
       .byte $00 ; |        | $DD4F shared
       .byte $07 ; |     XXX| $DD50
       .byte $1D ; |   XXX X| $DD51
       .byte $55 ; | X X X X| $DD52
       .byte $57 ; | X X XXX| $DD53
       .byte $5C ; | X XXX  | $DD54
       .byte $40 ; | X      | $DD55
       .byte $40 ; | X      | $DD56
M150pointsGFX:
       .byte $00 ; |        | $DD57 shared
       .byte $07 ; |     XXX| $DD58
       .byte $35 ; |  XX X X| $DD59
       .byte $95 ; |X  X X X| $DD5A
       .byte $B7 ; |X XX XXX| $DD5B
       .byte $A0 ; |X X     | $DD5C
       .byte $B0 ; |X XX    | $DD5D
       .byte $80 ; |X       | $DD5E
M200pointsGFX:
       .byte $00 ; |        | $DD5F shared
       .byte $07 ; |     XXX| $DD60
       .byte $1D ; |   XXX X| $DD61
       .byte $D5 ; |XX X X X| $DD62
       .byte $97 ; |X  X XXX| $DD63
       .byte $DC ; |XX XXX  | $DD64
       .byte $40 ; | X      | $DD65
       .byte $C0 ; |XX      | $DD66
M250pointsGFX:
       .byte $00 ; |        | $DD67 shared
       .byte $07 ; |     XXX| $DD68
       .byte $1D ; |   XXX X| $DD69
       .byte $CD ; |XX  XX X| $DD6A
       .byte $9F ; |X  XXXXX| $DD6B
       .byte $D0 ; |XX X    | $DD6C
       .byte $58 ; | X XX   | $DD6D
       .byte $C0 ; |XX      | $DD6E
       .byte $00 ; |        | $DD6F

MothershipScoreGFXtbl:
       .byte <M50pointsGFX ; | X   XXX| $DD70
       .byte <M50pointsGFX ; | X   XXX| $DD71
       .byte <M100pointsGFX ; | X  XXXX| $DD72
       .byte <M100pointsGFX ; | X  XXXX| $DD73
       .byte <M150pointsGFX ; | X X XXX| $DD74
       .byte <M200pointsGFX ; | X XXXXX| $DD75
       .byte <M250pointsGFX ; | XX  XXX| $DD76
       .byte <M300pointsGFX ; |  X  XXX| $DD77
       .byte <M500pointsGFX ; |  X XXXX| $DD78

LDD79:
       .byte $7F ; | XXXXXXX| $DD79
       .byte $BF ; |X XXXXXX| $DD7A
       .byte $DF ; |XX XXXXX| $DD7B
       .byte $EF ; |XXX XXXX| $DD7C
       .byte $F7 ; |XXXX XXX| $DD7D
       .byte $FB ; |XXXXX XX| $DD7E
       .byte $FD ; |XXXXXX X| $DD7F
       .byte $FE ; |XXXXXXX | $DD80

ThumpSound:
       .byte $01 ; |       X| $DD81
       .byte $0F ; |    XXXX| $DD82
       .byte $0A ; |    X X | $DD83
       .byte $0F ; |    XXXX| $DD84
       .byte $02 ; |      X | $DD85
       .byte $1F ; |   XXXXX| $DD86
       .byte $0A ; |    X X | $DD87
       .byte $0A ; |    X X | $DD88
       .byte $03 ; |      XX| $DD89
       .byte $1C ; |   XXX  | $DD8A
       .byte $0A ; |    X X | $DD8B
       .byte $06 ; |     XX | $DD8C
       .byte $04 ; |     X  | $DD8D
       .byte $1F ; |   XXXXX| $DD8E
       .byte $0A ; |    X X | $DD8F
       .byte $09 ; |    X  X| $DD90
       .byte $05 ; |     X X| $DD91
       .byte $10 ; |   X    | $DD92
       .byte $0A ; |    X X | $DD93
       .byte $06 ; |     XX | $DD94
       .byte $06 ; |     XX | $DD95
       .byte $FF ; |XXXXXXXX| $DD96

MothershipSound:
       .byte $01 ; |       X| $DD97
       .byte $1F ; |   XXXXX| $DD98
       .byte $0C ; |    XX  | $DD99
       .byte $06 ; |     XX | $DD9A
       .byte $02 ; |      X | $DD9B
       .byte $0C ; |    XX  | $DD9C
       .byte $0C ; |    XX  | $DD9D
       .byte $03 ; |      XX| $DD9E
       .byte $03 ; |      XX| $DD9F
       .byte $0C ; |    XX  | $DDA0
       .byte $0C ; |    XX  | $DDA1
       .byte $03 ; |      XX| $DDA2
       .byte $04 ; |     X  | $DDA3
       .byte $09 ; |    X  X| $DDA4
       .byte $0C ; |    XX  | $DDA5
       .byte $02 ; |      X | $DDA6
       .byte $05 ; |     X X| $DDA7
YELLOW:
       .byte $2C ; |  X XX  | $DDA8 shared...yellow (PAL)
       .byte $6C ; | XX XX  | $DDA9 shared...red (PAL)
       .byte $02 ; |      X | $DDAA
       .byte $06 ; |     XX | $DDAB
       .byte $08 ; |    X   | $DDAC
       .byte $0C ; |    XX  | $DDAD
       .byte $01 ; |       X| $DDAE
       .byte $07 ; |     XXX| $DDAF
       .byte $06 ; |     XX | $DDB0
       .byte $0C ; |    XX  | $DDB1
       .byte $01 ; |       X| $DDB2
       .byte $08 ; |    X   | $DDB3
       .byte $03 ; |      XX| $DDB4
       .byte $0C ; |    XX  | $DDB5
       .byte $01 ; |       X| $DDB6
       .byte $09 ; |    X  X| $DDB7
       .byte $03 ; |      XX| $DDB8
       .byte $0C ; |    XX  | $DDB9
       .byte $01 ; |       X| $DDBA
       .byte $0B ; |    X XX| $DDBB
       .byte $02 ; |      X | $DDBC
       .byte $0C ; |    XX  | $DDBD
       .byte $01 ; |       X| $DDBE
       .byte $0D ; |    XX X| $DDBF
       .byte $01 ; |       X| $DDC0
       .byte $0C ; |    XX  | $DDC1
       .byte $01 ; |       X| $DDC2
       .byte $0F ; |    XXXX| $DDC3
       .byte $00 ; |        | $DDC4
       .byte $0C ; |    XX  | $DDC5
       .byte $01 ; |       X| $DDC6
       .byte $11 ; |   X   X| $DDC7
       .byte $3F ; |  XXXXXX| $DDC8
MissileSound:
       .byte $01 ; |       X| $DDC9
       .byte $02 ; |      X | $DDCA
       .byte $08 ; |    X   | $DDCB
       .byte $05 ; |     X X| $DDCC
       .byte $03 ; |      XX| $DDCD
       .byte $00 ; |        | $DDCE
       .byte $03 ; |      XX| $DDCF
       .byte $04 ; |     X  | $DDD0
       .byte $06 ; |     XX | $DDD1
       .byte $03 ; |      XX| $DDD2
       .byte $08 ; |    X   | $DDD3
       .byte $04 ; |     X  | $DDD4
       .byte $0A ; |    X X | $DDD5
       .byte $04 ; |     X  | $DDD6
       .byte $08 ; |    X   | $DDD7
       .byte $03 ; |      XX| $DDD8
       .byte $14 ; |   X X  | $DDD9
RBsound:
       .byte $18 ; |   XX   | $DDDA
       .byte $08 ; |    X   | $DDDB
       .byte $01 ; |       X| $DDDC
       .byte $48 ; | X  X   | $DDDD
       .byte $FF ; |XXXXXXXX| $DDDE
DeathSound:
       .byte $01 ; |       X| $DDDF
       .byte $18 ; |   XX   | $DDE0
Sound:
       .byte $03 ; |      XX| $DDE1
       .byte $0C ; |    XX  | $DDE2
       .byte $09 ; |    X  X| $DDE3
       .byte $10 ; |   X    | $DDE4
       .byte $08 ; |    X   | $DDE5
       .byte $08 ; |    X   | $DDE6
       .byte $11 ; |   X   X| $DDE7
       .byte $1A ; |   XX X | $DDE8 shared...yellow (NTSC)
       .byte $48 ; | X  X   | $DDE9 shared...red (NTSC)
       .byte $0A ; |    X X | $DDEA
       .byte $19 ; |   XX  X| $DDEB
       .byte $16 ; |   X XX | $DDEC
       .byte $08 ; |    X   | $DDED
       .byte $08 ; |    X   | $DDEE
       .byte $29 ; |  X X  X| $DDEF
       .byte $1A ; |   XX X | $DDF0
       .byte $08 ; |    X   | $DDF1
       .byte $04 ; |     X  | $DDF2
       .byte $39 ; |  XXX  X| $DDF3
       .byte $1D ; |   XXX X| $DDF4
       .byte $08 ; |    X   | $DDF5
       .byte $02 ; |      X | $DDF6
       .byte $49 ; | X  X  X| $DDF7
InvaderHitSound:
       .byte $FF ; |XXXXXXXX| $DDF8 shared
       .byte $13 ; |   X  XX| $DDF9
       .byte $15 ; |   X X X| $DDFA
       .byte $17 ; |   X XXX| $DDFB
       .byte $00 ; |        | $DDFC
       .byte $12 ; |   X  X | $DDFD
       .byte $14 ; |   X X  | $DDFE
       .byte $16 ; |   X XX | $DDFF


SpriteGFX:
       .byte $00 ; |        | $DE00
       .byte $00 ; |        | $DE01
       .byte $00 ; |        | $DE02
       .byte $00 ; |        | $DE03
       .byte $00 ; |        | $DE04
       .byte $00 ; |        | $DE05
       .byte $00 ; |        | $DE06
       .byte $00 ; |        | $DE07
       .byte $00 ; |        | $DE08
Invader6GFX:
       .byte $00 ; |        | $DE09 shared
       .byte $24 ; |  X  X  | $DE0A
       .byte $5A ; | X XX X | $DE0B
       .byte $24 ; |  X  X  | $DE0C
       .byte $FF ; |XXXXXXXX| $DE0D
       .byte $DB ; |XX XX XX| $DE0E
       .byte $FF ; |XXXXXXXX| $DE0F
       .byte $7E ; | XXXXXX | $DE10
       .byte $18 ; |   XX   | $DE11
       .byte $00 ; |        | $DE12
       .byte $00 ; |        | $DE13
       .byte $81 ; |X      X| $DE14
       .byte $5A ; | X XX X | $DE15
       .byte $24 ; |  X  X  | $DE16
       .byte $FF ; |XXXXXXXX| $DE17
       .byte $DB ; |XX XX XX| $DE18
       .byte $FF ; |XXXXXXXX| $DE19
       .byte $7E ; | XXXXXX | $DE1A
       .byte $18 ; |   XX   | $DE1B
Invader5GFX:
       .byte $00 ; |        | $DE1C shared
       .byte $00 ; |        | $DE1D
       .byte $A5 ; |X X  X X| $DE1E
       .byte $5A ; | X XX X | $DE1F
       .byte $24 ; |  X  X  | $DE20
       .byte $FF ; |XXXXXXXX| $DE21
       .byte $DB ; |XX XX XX| $DE22
       .byte $FF ; |XXXXXXXX| $DE23
       .byte $7E ; | XXXXXX | $DE24
       .byte $18 ; |   XX   | $DE25
       .byte $00 ; |        | $DE26
       .byte $00 ; |        | $DE27
       .byte $42 ; | X    X | $DE28
       .byte $81 ; |X      X| $DE29
       .byte $5A ; | X XX X | $DE2A
       .byte $FF ; |XXXXXXXX| $DE2B
       .byte $DB ; |XX XX XX| $DE2C
       .byte $FF ; |XXXXXXXX| $DE2D
       .byte $7E ; | XXXXXX | $DE2E
Invader3GFX:
       .byte $18 ; |   XX   | $DE2F sharedx2 - yellow (NTSC)
       .byte $A5 ; |X X  X X| $DE30
       .byte $BD ; |X XXXX X| $DE31
       .byte $FF ; |XXXXXXXX| $DE32
       .byte $FF ; |XXXXXXXX| $DE33
       .byte $5A ; | X XX X | $DE34
       .byte $3C ; |  XXXX  | $DE35
       .byte $18 ; |   XX   | $DE36
       .byte $24 ; |  X  X  | $DE37
       .byte $42 ; | X    X | $DE38
       .byte $42 ; | X    X | $DE39
       .byte $24 ; |  X  X  | $DE3A
       .byte $3C ; |  XXXX  | $DE3B
       .byte $7E ; | XXXXXX | $DE3C
       .byte $FF ; |XXXXXXXX| $DE3D
       .byte $DB ; |XX XX XX| $DE3E
       .byte $BD ; |X XXXX X| $DE3F
       .byte $99 ; |X  XX  X| $DE40
       .byte $24 ; |  X  X  | $DE41
       .byte $42 ; | X    X | $DE42

Invader4GFX:
       .byte $00 ; |        | $DE43
       .byte $28 ; |  X X   | $DE44
       .byte $44 ; | X   X  | $DE45
       .byte $28 ; |  X X   | $DE46
       .byte $7C ; | XXXXX  | $DE47
       .byte $6C ; | XX XX  | $DE48
       .byte $7C ; | XXXXX  | $DE49
       .byte $54 ; | X X X  | $DE4A
       .byte $7C ; | XXXXX  | $DE4B
       .byte $38 ; |  XXX   | $DE4C
       .byte $00 ; |        | $DE4D
       .byte $00 ; |        | $DE4E
       .byte $44 ; | X   X  | $DE4F
       .byte $28 ; |  X X   | $DE50
       .byte $10 ; |   X    | $DE51
       .byte $38 ; |  XXX   | $DE52
       .byte $54 ; | X X X  | $DE53
       .byte $38 ; |  XXX   | $DE54
       .byte $10 ; |   X    | $DE55
       .byte $00 ; |        | $DE56

MothershipGFXtbl:
       .byte <MothersGFX1 ; |  XX XXX| $DE57
       .byte <MysteryGFX1 ; |     XXX| $DE58
       .byte <MothersGFX2 ; |  XXXXXX| $DE59
       .byte <MysteryGFX2 ; |    XXXX| $DE5A
       .byte <MothersGFX3 ; |        | $DE5B
       .byte <MysteryGFX3 ; |   X XXX| $DE5C
       .byte <MothersGFX4 ; |        | $DE5D
       .byte <MysteryGFX4 ; |   XXXXX| $DE5E

LDE5F:
       .byte $05 ; |     X X| $DE5F
       .byte $03 ; |      XX| $DE60
LDE61:
       .byte $01 ; |       X| $DE61
       .byte $02 ; |      X | $DE62
LDE63:
       .byte $04 ; |     X  | $DE63
       .byte $08 ; |    X   | $DE64
       .byte $10 ; |   X    | $DE65
       .byte $20 ; |  X     | $DE66
       .byte $40 ; | X      | $DE67
       .byte $80 ; |X       | $DE68

Invader2GFX:
       .byte $00 ; |        | $DE69
       .byte $A5 ; |X X  X X| $DE6A
       .byte $5A ; | X XX X | $DE6B
       .byte $24 ; |  X  X  | $DE6C
       .byte $FF ; |XXXXXXXX| $DE6D
       .byte $DB ; |XX XX XX| $DE6E
       .byte $7E ; | XXXXXX | $DE6F
       .byte $3C ; |  XXXX  | $DE70
       .byte $18 ; |   XX   | $DE71
       .byte $00 ; |        | $DE72
       .byte $00 ; |        | $DE73
RED:
       .byte $42 ; | X    X | $DE74 shared - red (NTSC)
       .byte $81 ; |X      X| $DE75
       .byte $5A ; | X XX X | $DE76
       .byte $FF ; |XXXXXXXX| $DE77
       .byte $DB ; |XX XX XX| $DE78
       .byte $7E ; | XXXXXX | $DE79
       .byte $3C ; |  XXXX  | $DE7A
       .byte $18 ; |   XX   | $DE7B
Invader1GFX:
       .byte $00 ; |        | $DE7C shared
       .byte $28 ; |  X X   | $DE7D
       .byte $44 ; | X   X  | $DE7E
       .byte $28 ; |  X X   | $DE7F
       .byte $FE ; |XXXXXXX | $DE80
       .byte $D6 ; |XX X XX | $DE81
       .byte $7C ; | XXXXX  | $DE82
       .byte $38 ; |  XXX   | $DE83
       .byte $10 ; |   X    | $DE84
       .byte $00 ; |        | $DE85
       .byte $00 ; |        | $DE86
       .byte $82 ; |X     X | $DE87
       .byte $44 ; | X   X  | $DE88
       .byte $28 ; |  X X   | $DE89
       .byte $FE ; |XXXXXXX | $DE8A
       .byte $D6 ; |XX X XX | $DE8B
       .byte $7C ; | XXXXX  | $DE8C
       .byte $38 ; |  XXX   | $DE8D
       .byte $10 ; |   X    | $DE8E
       .byte $00 ; |        | $DE8F
Rainbow500GFX:
       .byte $00 ; |        | $DE90 shared
       .byte $07 ; |     XXX| $DE91
       .byte $1D ; |   XXX X| $DE92
       .byte $D5 ; |XX X X X| $DE93
       .byte $57 ; | X X XXX| $DE94
       .byte $DC ; |XX XXX  | $DE95
       .byte $80 ; |X       | $DE96
       .byte $C0 ; |XX      | $DE97
Rainbow1000GFX:
       .byte $00 ; |        | $DE98 shared
       .byte $00 ; |        | $DE99 shared
       .byte $07 ; |     XXX| $DE9A
       .byte $1D ; |   XXX X| $DE9B
       .byte $75 ; | XXX X X| $DE9C
       .byte $D7 ; |XX X XXX| $DE9D
       .byte $DC ; |XX XXX  | $DE9E
       .byte $F0 ; |XXXX    | $DE9F
       .byte $80 ; |X       | $DEA0
BaseGFX:
       .byte $00 ; |        | $DEA1 shared
       .byte $00 ; |        | $DEA2 shared
       .byte $7F ; | XXXXXXX| $DEA3
       .byte $7F ; | XXXXXXX| $DEA4
       .byte $7F ; | XXXXXXX| $DEA5
       .byte $7F ; | XXXXXXX| $DEA6
       .byte $3E ; |  XXXXX | $DEA7
       .byte $08 ; |    X   | $DEA8
       .byte $08 ; |    X   | $DEA9
ExplosionGFX:
       .byte $00 ; |        | $DEAA shared
       .byte $08 ; |    X   | $DEAB
       .byte $41 ; | X     X| $DEAC
       .byte $2A ; |  X X X | $DEAD
       .byte $08 ; |    X   | $DEAE
       .byte $63 ; | XX   XX| $DEAF
       .byte $08 ; |    X   | $DEB0
LCOLOR:
;NTSC
       .byte $2A ; |  X X X | $DEB1 null
       .byte $41 ; | X     X| $DEB2 shared (dark red)
       .byte $08 ; |    X   | $DEB3 null
       .byte $30 ; |  XX    | $DEB4
       .byte $34 ; |  XX X  | $DEB5
       .byte $34 ; |  XX X  | $DEB6
       .byte $34 ; |  XX X  | $DEB7
       .byte $1A ; |   XX X | $DEB8
       .byte $1A ; |   XX X | $DEB9
       .byte $C6 ; |XX   XX | $DEBA
       .byte $C6 ; |XX   XX | $DEBB
       .byte $56 ; | X X XX | $DEBC
       .byte $56 ; | X X XX | $DEBD
CYAN:
       .byte $98 ; |X  XX   | $DEBE shared...NTSC CYAN
       .byte $98 ; |X  XX   | $DEBF
       .byte $C6 ; |XX   XX | $DEC0
       .byte $C6 ; |XX   XX | $DEC1
       .byte $C6 ; |XX   XX | $DEC2
;PAL
       .byte $60 ; | XX     | $DEC3
       .byte $64 ; | XX  X  | $DEC4
       .byte $64 ; | XX  X  | $DEC5
       .byte $64 ; | XX  X  | $DEC6
       .byte $2A ; |  X X X | $DEC7
       .byte $2A ; |  X X X | $DEC8
       .byte $56 ; | X X XX | $DEC9
       .byte $56 ; | X X XX | $DECA
       .byte $86 ; |X    XX | $DECB
       .byte $86 ; |X    XX | $DECC
       .byte $B8 ; |X XXX   | $DECD
       .byte $B8 ; |X XXX   | $DECE
       .byte $56 ; | X X XX | $DECF
       .byte $56 ; | X X XX | $DED0
       .byte $56 ; | X X XX | $DED1

;Rainbow bonus sound
       .byte $01 ; |       X| $DED2
       .byte $03 ; |      XX| $DED3
       .byte $05 ; |     X X| $DED4
       .byte $07 ; |     XXX| $DED5
       .byte $09 ; |    X  X| $DED6
       .byte $0B ; |    X XX| $DED7
       .byte $0D ; |    XX X| $DED8
       .byte $0F ; |    XXXX| $DED9
       .byte $11 ; |   X   X| $DEDA
       .byte $13 ; |   X  XX| $DEDB
       .byte $15 ; |   X X X| $DEDC
       .byte $17 ; |   X XXX| $DEDD
       .byte $19 ; |   XX  X| $DEDE
       .byte $1B ; |   XX XX| $DEDF
       .byte $1D ; |   XXX X| $DEE0
       .byte $1F ; |   XXXXX| $DEE1
       .byte $1D ; |   XXX X| $DEE2
       .byte $1B ; |   XX XX| $DEE3
       .byte $19 ; |   XX  X| $DEE4
       .byte $17 ; |   X XXX| $DEE5
       .byte $15 ; |   X X X| $DEE6
       .byte $13 ; |   X  XX| $DEE7
       .byte $11 ; |   X   X| $DEE8
       .byte $0F ; |    XXXX| $DEE9
       .byte $0D ; |    XX X| $DEEA
       .byte $0B ; |    X XX| $DEEB
       .byte $09 ; |    X  X| $DEEC
       .byte $07 ; |     XXX| $DEED
       .byte $05 ; |     X X| $DEEE
       .byte $03 ; |      XX| $DEEF
       .byte $01 ; |       X| $DEF0
       .byte $02 ; |      X | $DEF1
       .byte $03 ; |      XX| $DEF2
       .byte $04 ; |     X  | $DEF3
       .byte $05 ; |     X X| $DEF4
       .byte $06 ; |     XX | $DEF5
       .byte $07 ; |     XXX| $DEF6
       .byte $08 ; |    X   | $DEF7
       .byte $08 ; |    X   | $DEF8
       .byte $07 ; |     XXX| $DEF9
       .byte $06 ; |     XX | $DEFA
       .byte $05 ; |     X X| $DEFB
       .byte $04 ; |     X  | $DEFC
       .byte $03 ; |      XX| $DEFD
       .byte $B8 ; |X XXX   | $DEFE shared...PAL CYAN
       .byte $01 ; |       X| $DEFF

LTdigits:
       .byte $07 ; |     XXX| $DF00
       .byte $05 ; |     X X| $DF01
       .byte $05 ; |     X X| $DF02
       .byte $05 ; |     X X| $DF03
       .byte $07 ; |     XXX| $DF04
       .byte $07 ; |     XXX| $DF05
       .byte $02 ; |      X | $DF06
       .byte $02 ; |      X | $DF07
       .byte $06 ; |     XX | $DF08
       .byte $02 ; |      X | $DF09
       .byte $07 ; |     XXX| $DF0A
       .byte $04 ; |     X  | $DF0B
       .byte $07 ; |     XXX| $DF0C
       .byte $01 ; |       X| $DF0D
       .byte $07 ; |     XXX| $DF0E
       .byte $07 ; |     XXX| $DF0F
       .byte $01 ; |       X| $DF10
       .byte $07 ; |     XXX| $DF11
       .byte $01 ; |       X| $DF12
       .byte $07 ; |     XXX| $DF13
       .byte $01 ; |       X| $DF14
       .byte $01 ; |       X| $DF15
       .byte $07 ; |     XXX| $DF16
       .byte $05 ; |     X X| $DF17
       .byte $05 ; |     X X| $DF18
       .byte $07 ; |     XXX| $DF19
       .byte $01 ; |       X| $DF1A
       .byte $07 ; |     XXX| $DF1B
       .byte $04 ; |     X  | $DF1C
       .byte $07 ; |     XXX| $DF1D
       .byte $07 ; |     XXX| $DF1E
       .byte $05 ; |     X X| $DF1F
       .byte $07 ; |     XXX| $DF20
       .byte $04 ; |     X  | $DF21
       .byte $04 ; |     X  | $DF22
       .byte $01 ; |       X| $DF23
       .byte $01 ; |       X| $DF24
       .byte $01 ; |       X| $DF25
       .byte $01 ; |       X| $DF26
       .byte $07 ; |     XXX| $DF27
       .byte $07 ; |     XXX| $DF28
       .byte $05 ; |     X X| $DF29
       .byte $07 ; |     XXX| $DF2A
       .byte $05 ; |     X X| $DF2B
       .byte $07 ; |     XXX| $DF2C
       .byte $01 ; |       X| $DF2D
       .byte $01 ; |       X| $DF2E
       .byte $07 ; |     XXX| $DF2F
       .byte $05 ; |     X X| $DF30
       .byte $07 ; |     XXX| $DF31
       .byte $00 ; |        | $DF32
       .byte $00 ; |        | $DF33
       .byte $00 ; |        | $DF34
       .byte $00 ; |        | $DF35
       .byte $00 ; |        | $DF36
UTdigits:
       .byte $E0 ; |XXX     | $DF37
       .byte $A0 ; |X X     | $DF38
       .byte $A0 ; |X X     | $DF39
       .byte $A0 ; |X X     | $DF3A
       .byte $E0 ; |XXX     | $DF3B
       .byte $E0 ; |XXX     | $DF3C
       .byte $40 ; | X      | $DF3D
       .byte $40 ; | X      | $DF3E
       .byte $60 ; | XX     | $DF3F
       .byte $40 ; | X      | $DF40
       .byte $E0 ; |XXX     | $DF41
       .byte $20 ; |  X     | $DF42
       .byte $E0 ; |XXX     | $DF43
       .byte $80 ; |X       | $DF44
       .byte $E0 ; |XXX     | $DF45
       .byte $E0 ; |XXX     | $DF46
       .byte $80 ; |X       | $DF47
       .byte $E0 ; |XXX     | $DF48
       .byte $80 ; |X       | $DF49
       .byte $E0 ; |XXX     | $DF4A
       .byte $80 ; |X       | $DF4B
       .byte $80 ; |X       | $DF4C
       .byte $E0 ; |XXX     | $DF4D
       .byte $A0 ; |X X     | $DF4E
       .byte $A0 ; |X X     | $DF4F
       .byte $E0 ; |XXX     | $DF50
       .byte $80 ; |X       | $DF51
       .byte $E0 ; |XXX     | $DF52
       .byte $20 ; |  X     | $DF53
       .byte $E0 ; |XXX     | $DF54
       .byte $E0 ; |XXX     | $DF55
       .byte $A0 ; |X X     | $DF56
       .byte $E0 ; |XXX     | $DF57
       .byte $20 ; |  X     | $DF58
       .byte $20 ; |  X     | $DF59
       .byte $80 ; |X       | $DF5A
       .byte $80 ; |X       | $DF5B
       .byte $80 ; |X       | $DF5C
       .byte $80 ; |X       | $DF5D
       .byte $E0 ; |XXX     | $DF5E
       .byte $E0 ; |XXX     | $DF5F
       .byte $A0 ; |X X     | $DF60
       .byte $E0 ; |XXX     | $DF61
       .byte $A0 ; |X X     | $DF62
       .byte $E0 ; |XXX     | $DF63
       .byte $80 ; |X       | $DF64
       .byte $80 ; |X       | $DF65
       .byte $E0 ; |XXX     | $DF66
       .byte $A0 ; |X X     | $DF67
       .byte $E0 ; |XXX     | $DF68
       .byte $00 ; |        | $DF69
       .byte $00 ; |        | $DF6A
       .byte $00 ; |        | $DF6B
       .byte $00 ; |        | $DF6C
       .byte $00 ; |        | $DF6D
LUdigits:
       .byte $70 ; | XXX    | $DF6E
       .byte $50 ; | X X    | $DF6F
       .byte $50 ; | X X    | $DF70
       .byte $50 ; | X X    | $DF71
       .byte $70 ; | XXX    | $DF72
       .byte $70 ; | XXX    | $DF73
       .byte $20 ; |  X     | $DF74
       .byte $20 ; |  X     | $DF75
       .byte $60 ; | XX     | $DF76
       .byte $20 ; |  X     | $DF77
       .byte $70 ; | XXX    | $DF78
       .byte $40 ; | X      | $DF79
       .byte $70 ; | XXX    | $DF7A
       .byte $10 ; |   X    | $DF7B
       .byte $70 ; | XXX    | $DF7C
       .byte $70 ; | XXX    | $DF7D
       .byte $10 ; |   X    | $DF7E
       .byte $70 ; | XXX    | $DF7F
       .byte $10 ; |   X    | $DF80
       .byte $70 ; | XXX    | $DF81
       .byte $10 ; |   X    | $DF82
       .byte $10 ; |   X    | $DF83
       .byte $70 ; | XXX    | $DF84
       .byte $50 ; | X X    | $DF85
       .byte $50 ; | X X    | $DF86
       .byte $70 ; | XXX    | $DF87
       .byte $10 ; |   X    | $DF88
       .byte $70 ; | XXX    | $DF89
       .byte $40 ; | X      | $DF8A
       .byte $70 ; | XXX    | $DF8B
       .byte $70 ; | XXX    | $DF8C
       .byte $50 ; | X X    | $DF8D
       .byte $70 ; | XXX    | $DF8E
       .byte $40 ; | X      | $DF8F
       .byte $40 ; | X      | $DF90
       .byte $10 ; |   X    | $DF91
       .byte $10 ; |   X    | $DF92
       .byte $10 ; |   X    | $DF93
       .byte $10 ; |   X    | $DF94
       .byte $70 ; | XXX    | $DF95
       .byte $70 ; | XXX    | $DF96
       .byte $50 ; | X X    | $DF97
       .byte $70 ; | XXX    | $DF98
       .byte $50 ; | X X    | $DF99
       .byte $70 ; | XXX    | $DF9A
       .byte $10 ; |   X    | $DF9B
       .byte $10 ; |   X    | $DF9C
       .byte $70 ; | XXX    | $DF9D
       .byte $50 ; | X X    | $DF9E
       .byte $70 ; | XXX    | $DF9F
       .byte $00 ; |        | $DFA0
       .byte $00 ; |        | $DFA1
       .byte $00 ; |        | $DFA2
       .byte $00 ; |        | $DFA3
       .byte $00 ; |        | $DFA4
ULdigits:
       .byte $0E ; |    XXX | $DFA5
       .byte $0A ; |    X X | $DFA6
       .byte $0A ; |    X X | $DFA7
       .byte $0A ; |    X X | $DFA8
       .byte $0E ; |    XXX | $DFA9
       .byte $0E ; |    XXX | $DFAA
       .byte $04 ; |     X  | $DFAB
       .byte $04 ; |     X  | $DFAC
       .byte $06 ; |     XX | $DFAD
       .byte $04 ; |     X  | $DFAE
       .byte $0E ; |    XXX | $DFAF
       .byte $02 ; |      X | $DFB0
       .byte $0E ; |    XXX | $DFB1
       .byte $08 ; |    X   | $DFB2
       .byte $0E ; |    XXX | $DFB3
       .byte $0E ; |    XXX | $DFB4
       .byte $08 ; |    X   | $DFB5
       .byte $0E ; |    XXX | $DFB6
       .byte $08 ; |    X   | $DFB7
       .byte $0E ; |    XXX | $DFB8
       .byte $08 ; |    X   | $DFB9
       .byte $08 ; |    X   | $DFBA
       .byte $0E ; |    XXX | $DFBB
       .byte $0A ; |    X X | $DFBC
       .byte $0A ; |    X X | $DFBD
       .byte $0E ; |    XXX | $DFBE
       .byte $08 ; |    X   | $DFBF
       .byte $0E ; |    XXX | $DFC0
       .byte $02 ; |      X | $DFC1
       .byte $0E ; |    XXX | $DFC2
       .byte $0E ; |    XXX | $DFC3
       .byte $0A ; |    X X | $DFC4
       .byte $0E ; |    XXX | $DFC5
       .byte $02 ; |      X | $DFC6
       .byte $02 ; |      X | $DFC7
       .byte $08 ; |    X   | $DFC8
       .byte $08 ; |    X   | $DFC9
       .byte $08 ; |    X   | $DFCA
       .byte $08 ; |    X   | $DFCB
       .byte $0E ; |    XXX | $DFCC
       .byte $0E ; |    XXX | $DFCD
       .byte $0A ; |    X X | $DFCE
       .byte $0E ; |    XXX | $DFCF
       .byte $0A ; |    X X | $DFD0
       .byte $0E ; |    XXX | $DFD1
       .byte $08 ; |    X   | $DFD2
       .byte $08 ; |    X   | $DFD3
       .byte $0E ; |    XXX | $DFD4
       .byte $0A ; |    X X | $DFD5
       .byte $0E ; |    XXX | $DFD6
       .byte $00 ; |        | $DFD7
       .byte $00 ; |        | $DFD8
       .byte $00 ; |        | $DFD9
       .byte $00 ; |        | $DFDA
LDFDB:
       .byte $00 ; |        | $DFDB shared
       .byte $09 ; |    X  X| $DFDC
       .byte $12 ; |   X  X | $DFDD
       .byte $1B ; |   XX XX| $DFDE
       .byte $24 ; |  X  X  | $DFDF
       .byte $2D ; |  X XX X| $DFE0
       .byte $36 ; |  XX XX | $DFE1
  IF PLUSROM = 0
MothershipPointsTbl:
       .byte $05 ; |     X X| $DFE2
       .byte $05 ; |     X X| $DFE3
       .byte $10 ; |   X    | $DFE4
       .byte $10 ; |   X    | $DFE5
       .byte $15 ; |   X X X| $DFE6
       .byte $20 ; |  X     | $DFE7
       .byte $25 ; |  X  X X| $DFE8
       .byte $30 ; |  XX    | $DFE9
       .byte $50 ; | X X    | $DFEA

SoundTbl:
       .byte <ThumpSound      ; |X      X| $DFEB
       .byte <DeathSound      ; |XX XXXXX| $DFEC
       .byte <MissileSound    ; |XX  X  X| $DFED
       .byte <MothershipSound ; |X  X XXX| $DFEE
       .byte <Sound           ; |XXX    X| $DFEF
       .byte <DeathSound      ; |XX XXXXX| $DFF0
  ENDIF

START1:
       sta    $1FF9                   ;4 switch to bank2 and boot game
       NOP                            ;2

LDFF5:
       .byte $BD ; |X XXXX X| $DFF5
       .byte $B4 ; |X XX X  | $DFF6
       .byte $AB ; |X X X XX| $DFF7

  IF PLUSROM = 1
       ORG $1FF0
       RORG $DFF0

       .byte " HSC 2022 "             ; Keep PlusROM hotspots free
       .word ( PlusROM_API - $c000 )
  ELSE
       ORG $1FF8
       RORG $DFF8

       .byte "2005"
  ENDIF
       .word START1,0































;Begin second bank
       ORG $2000
       RORG $F000

LF000:
       sta    $1FF8                   ;4 bankswitch to display kernal
LF003:
       lda    INTIM                   ;4
       bne    LF003                   ;2
       lda    #$02                    ;2
       sta    VBLANK                  ;3 disable TIA
       sta    WSYNC                   ;3
LF00E:
       sta    WSYNC                   ;3
       lda    Framecount              ;3
       and    #$07                    ;2
       bne    LF031                   ;2
       lda    $C8                     ;3
       clc                            ;2 don't remove
       adc    #$40                    ;2
       sta    $C8                     ;3
       cmp    #$40                    ;2
       bcs    LF031                   ;2
       cmp    #$39                    ;2
       bne    LF02D                   ;2
       lda    #$B4                    ;2
       sta    $9E                     ;3
       lda    #$00                    ;2
       sta    $CC                     ;3
LF02D:
       lda    #$30                    ;2
       sta    $C8                     ;3
LF031:
       lda    #$02                    ;2
       sta    WSYNC                   ;3
       sta    VSYNC                   ;3
       sta    WSYNC                   ;3
       lda    #$30                    ;2
       sta    TIM64T                  ;4
       lda    #$00                    ;2
       sta    WSYNC                   ;3
       sta    VSYNC                   ;3
       lda    Framecount              ;3
       lsr                            ;2
       bcc    LF06C                   ;2
       ldx    #$01                    ;2
LF04B:
       lda    $D5,x                   ;4
       cmp    #$79                    ;2
       bne    LF055                   ;2
       lda    #$F6                    ;2
       sta    $D5,x                   ;4
LF055:
       lda    $D5,x                   ;4
       cmp    #$EC                    ;2
       bcs    LF067                   ;2
       lda    $D5,x                   ;4
       adc    #$FE                    ;2
       cmp    #$03                    ;2
       bcs    LF065                   ;2
       lda    #$F6                    ;2
LF065:
       sta    $D5,x                   ;4
LF067:
       dex                            ;2
       bpl    LF04B                   ;2
       bmi    LF0CE                   ;2
LF06C:
       lda    Framecount              ;3
       and    #$0F                    ;2
       cmp    #$0F                    ;2
       beq    LF082                   ;2
       lda    $C7                     ;3
       asl                            ;2
       asl                            ;2
       clc                            ;2
       adc    $C7                     ;3
       clc                            ;2
       adc    #$59                    ;2
       sta    $C7                     ;3
       sta    $DA                     ;3
LF082:
       lda    Variation               ;3
       and    #$04                    ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lda    #$01                    ;2
       tax                            ;2
       bcc    LF08F                   ;2
       asl                            ;2
LF08F:
       sta    $81                     ;3
LF091:
       lda    $D1,x                   ;4
       cmp    #$EC                    ;2
       bcs    LF0CB                   ;2
       lda    Variation               ;3
       and    #$02                    ;2
       beq    LF0BC                   ;2
       lda    $DA                     ;3
       cpx    #$00                    ;2
       beq    LF0A5                   ;2
       asl                            ;2
       asl                            ;2
LF0A5:
       asl                            ;2
       bcc    LF0BC                   ;2
       bpl    LF0B4                   ;2
       lda    $D3,x                   ;4
       cmp    #$81                    ;2
       bcs    LF0BC                   ;2
       inc    $D3,x                   ;6
       bne    LF0BC                   ;2
LF0B4:
       lda    $D3,x                   ;4
       cmp    #$17                    ;2
       bcc    LF0BC                   ;2
       dec    $D3,x                   ;6
LF0BC:
       lda    $D1,x                   ;4
       clc                            ;2
       adc    $81                     ;3
       sta    $D1,x                   ;4
       cmp    #$6C                    ;2
       bcc    LF0CB                   ;2
       lda    #$F6                    ;2
       sta    $D1,x                   ;4
LF0CB:
       dex                            ;2
       bpl    LF091                   ;2
LF0CE:
;check hiscore
       bit    Lives                   ;3
       bvs    LF0F0                   ;2 branch if competition play
       lda    P2scoreHi               ;3
       cmp    #$99                    ;2
       bcs    LF0F0                   ;2 skip if game select shown
       lda    P1scoreHi               ;3
       cmp    P2scoreHi               ;3 compare the high byte
       beq    LF0E2                   ;2 branch if the same (and check next)
       bcc    LF0F0                   ;2 branch if lower (and end)
       bcs    LF0E8                   ;2 higher...new high score
LF0E2:
       lda    P1scoreLo               ;3
       cmp    P2scoreLo               ;3 check the low byte
       bcc    LF0F0                   ;2 branch if lower (and end)
;new high score
LF0E8:
       lda    P1scoreHi               ;3 copy P1 score to P2 score
       sta    P2scoreHi               ;3
       lda    P1scoreLo               ;3
       sta    P2scoreLo               ;3
LF0F0:
       dec    Framecount              ;5 bump framecounter here
       beq    LF0F7                   ;2
       jmp    LF20E                   ;3
LF0F7:
       lda    $EA                     ;3
       beq    LF0FF                   ;2
       inc    $EA                     ;5
       inc    $EA                     ;5
LF0FF:
       bit    $E5                     ;3
       bmi    LF17F                   ;2
       lda    $AA                     ;3
       and    #$08                    ;2
       beq    LF142                   ;2
       eor    $AA                     ;3
       sta    $AA                     ;3
       lda    Lives                   ;3
       and    #$04                    ;2 intermission skip bit set? (from pressing down)
       beq    LF11C                   ;2 branch if so (and skip the intermission sequence)
;strip exclaimation
       lda    Ctrl                    ;3
       and    #$7F                    ;2
       sta    Ctrl                    ;3
       jmp    LF8EF                   ;3 jump to the intermission screen
LF11C: ;return from intermission
       ldx    $99                     ;3
       lda    LFFA4,x                 ;4
       sta    $90                     ;3
       cpx    #$03                    ;2
       bcs    LF129                   ;2
       inc    $99                     ;5
LF129:
       bit    $98                     ;3
       bmi    LF13C                   ;2
       jmp    LF847                   ;3

LF130:
       lda    $AA                     ;3
       and    #$06                    ;2
       bne    LF142                   ;2
       lda    $AA                     ;3
       ora    #$01                    ;2
       sta    $AA                     ;3
LF13C:
       lda    #$40                    ;2
       sta    Framecount              ;3 reset the framecounter
       bne    LF17C                   ;2 always branch

LF142:
       bit    $98                     ;3
       bpl    LF154                   ;2
       bit    $A7                     ;3
       bmi    LF16C                   ;2
       lda    $DB                     ;3
       cmp    #$10                    ;2
       bne    LF16C                   ;2
       lda    #$00                    ;2
       beq    LF185                   ;2 always branch

LF154:
       lda    $AA                     ;3
       and    #$01                    ;2
       beq    LF17F                   ;2
       eor    $AA                     ;3
       sta    $AA                     ;3
       lda    #$50                    ;2
       sta    $D9                     ;3
       lda    #$05                    ;2
       sta    $C6                     ;3
       lda    Lives                   ;3
       and    #$03                    ;2
       bne    LF1DB                   ;2
LF16C:
       lda    $E5                     ;3
       ora    #$80                    ;2
       sta    $E5                     ;3
       lda    $EA                     ;3
       bne    LF17F                   ;2
       lda    #$BF                    ;2
       sta    $EA                     ;3
       bpl    LF17F                   ;2
LF17C:
       jmp    LF427                   ;3

LF17F:
       lda    $AA                     ;3
       and    #$06                    ;2
       beq    LF1E7                   ;2
LF185:
       ora    #$01                    ;2
       eor    $AA                     ;3
       sta    $AA                     ;3
       lda    #$23                    ;2
       sta    $9C                     ;3
       lda    #$75                    ;2
       sta    $9D                     ;3
       lda    $DB                     ;3
       cmp    #$10                    ;2
       bne    LF1D3                   ;2
       bit    $A7                     ;3
       bmi    LF1D3                   ;2
       ldx    #$0A                    ;2
LF19F:
;swap player data
       ldy    $90,x                   ;4
       lda    $9F,x                   ;4
       sta    $90,x                   ;4
       sty    $9F,x                   ;4
       dex                            ;2
       bpl    LF19F                   ;2
;swap the shot counter
       lda    SHOT                    ;3
       rol                            ;2
       rol    SHOT                    ;5
       rol                            ;2
       rol    SHOT                    ;5
       rol                            ;2
       rol    SHOT                    ;5
       rol                            ;2
       rol    SHOT                    ;5
;copy shield data to ram
       ldx    #$1A                    ;2
       ldy    #$08                    ;2
LF1BC:
       lda    ShieldGFX,y             ;4
       sta    $AB,x                   ;4
       dey                            ;2
       bpl    LF1C6                   ;2
       ldy    #$08                    ;2
LF1C6:
       dex                            ;2
       bpl    LF1BC                   ;2
       lda    $AA                     ;3
       eor    #$40                    ;2
       sta    $AA                     ;3
       and    #$40                    ;2
       bne    LF1DB                   ;2
LF1D3:
       lda    Lives                   ;3
       and    #$03                    ;2
       beq    LF1DB                   ;2 skip if lives already zero (bugfix)
       dec    Lives                   ;5 lose life
  IF PLUSROM = 1
       tay
       dey
       beq    GAME_OVER
  ENDIF
LF1DB:
       lda    #$40                    ;2
       sta    Framecount              ;3
       lda    #$B4                    ;2
       sta    $9E                     ;3
       lda    #$00                    ;2
       sta    $CC                     ;3
LF1E7:
       dec    $C6                     ;5
       bne    LF20B                   ;2
       lda    $C7                     ;3
       asl                            ;2
       asl                            ;2
       clc                            ;2
       adc    $C7                     ;3
       clc                            ;2
       adc    #$59                    ;2
       sta    $C7                     ;3
       and    #$01                    ;2
       eor    $98                     ;3
       sta    $98                     ;3
       lsr                            ;2
       lda    #$98                    ;2
       bcc    LF204                   ;2
       lda    #$00                    ;2
LF204:
       sta    $9E                     ;3
       lda    #$04                    ;2
       jmp    LFB1A                   ;3
LF20B:
       jmp    LF427                   ;3

LF20E:
       lda    Framecount              ;3
       lsr                            ;2
       bcs    LF216                   ;2
       jmp    LF384                   ;3

  IF PLUSROM = 1
GAME_OVER:
       jmp SendPlusROMScore
  ENDIF

;console switches...
LF216:
       lda    SWCHB                   ;4
       tax                            ;2 copy to X (to be used later)
       eor    DifSw                   ;3 compare against stored switches
       beq    LF234                   ;2 branch if no change
       and    #$08                    ;2 check if the B&W switch tripped
       beq    LF231                   ;2 branch if not
;flip palettes
       lda    FLAG2                   ;3
       bpl    LF22D                   ;2
       txa                            ;2
       and    #$08                    ;2
       bne    LF231                   ;2
       lda    FLAG2                   ;3
LF22D:
       eor    #$40                    ;2
       sta    FLAG2                   ;3
LF231:
       txa                            ;2
       sta    DifSw                   ;3
LF234:
       and    #$03                    ;2
       cmp    #$02                    ;2
       bne    LF23D                   ;2
       jmp    LF62D                   ;3

LF23D:
       lda    DifSw                   ;3
       and    #$02                    ;2
       beq    LF24C                   ;2
       lda    $AA                     ;3 select pressed
       sta    $ED                     ;3
       bmi    LF294                   ;2
       jmp    LF2ED                   ;3

LF24C:
       lda    #$B1                    ;2
       sta    $EA                     ;3
       bit    $AA                     ;3
       bmi    LF25F                   ;2
       lda    $AA                     ;3
       and    #$B0                    ;2
       ora    #$80                    ;2
       sta    $ED                     ;3
       jmp    LFC19                   ;3

LF25F:
       inc    $ED                     ;5
       lda    $ED                     ;3
       cmp    #$0F                    ;2
       bcc    LF294                   ;2
       lda    DifSw                   ;3
       lsr                            ;2
       lda    #$0D                    ;2
       bcc    LF270                   ;2
       lda    #$02                    ;2
LF270:
       sta    $ED                     ;3
       lda    $98                     ;3
       and    #$F3                    ;2
       sta    $98                     ;3
       lda    Variation               ;3
       clc                            ;2
       adc    #$91                    ;2
       beq    LF281                   ;2
       adc    #$70                    ;2
LF281:
       sta    Variation               ;3 save game selection
       tay                            ;2
       lda    Lives                   ;3
       ora    #$40                    ;2
       cpy    #$10                    ;2
       bcc    LF290                   ;2
       cpy    #$40                    ;2
       bcc    LF292                   ;2
LF290:
       and    #$B4                    ;2 strip off "p1 only" bit...but keep interskip bit
LF292:
       sta    Lives                   ;3
LF294:
       lda    $AA                     ;3
       ora    #$80                    ;2
       sta    $AA                     ;3
       lda    Variation               ;3
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       sta    $EC                     ;3
       tay                            ;2
       lda    LFFAE,y                 ;4
       sta    $DB                     ;3
       lda    $98                     ;3
       and    #$CF                    ;2
       ora    LFFC1,y                 ;4
       sta    $98                     ;3
       lda    #$AA                    ;2 blank
       sta    P2scoreHi               ;3
       lda    #$A2                    ;2 "1"
       sta    P2scoreLo               ;3
       ldy    #$00                    ;2
       tya                            ;2
       sec                            ;2
       adc    Variation               ;3
LF2BF:
       cmp    #$0A                    ;2
       bcc    LF2C9                   ;2
       iny                            ;2
       sbc    #$0A                    ;2
       jmp    LF2BF                   ;3

LF2C9:
       adc    LFFB6,y                 ;4
       sta    P1scoreLo               ;3
       lda    Variation               ;3
       cmp    #$63                    ;2 game number over 99?
       lda    #$AA                    ;2
       bcc    LF2D8                   ;2 branch if less
       lda    #$A1                    ;2 use a hundreds digit for games over 99
LF2D8:
       sta    P1scoreHi               ;3
       lda    Variation               ;3
       cmp    #$10                    ;2
       bcs    LF2E2                   ;2
       dec    P2scoreLo               ;5
LF2E2:
       cmp    #$09                    ;2
       bcs    LF2EA                   ;2
       adc    #$A1                    ;2
       sta    P1scoreLo               ;3
LF2EA:
       jmp    LF427                   ;3
LF2ED:
       lda    Framecount              ;3
       lsr                            ;2
       bcs    LF2F5                   ;2
LF2F2:
       jmp    LF384                   ;3
LF2F5:
       lda    $EA                     ;3
       bne    LF2F2                   ;2
       bit    $98                     ;3
       bmi    LF315                   ;2
       lda    $AA                     ;3
       and    #$07                    ;2
       bne    LF315                   ;2
;single/double shots
       lda    #$10                    ;2 single
       bit    DifSw                   ;3
       bmi    LF30B                   ;2
       lda    #$00                    ;2 double
LF30B:
       and    $DB                     ;3
       beq    LF364                   ;2
       lda    $D5                     ;3
       cmp    #$EC                    ;2
       bcs    LF318                   ;2
LF315:
       jmp    LF427                   ;3
LF318:
       lda    $DB                     ;3
       cmp    #$14                    ;2
       beq    LF326                   ;2
       cmp    #$90                    ;2
       bne    LF32B                   ;2
       bit    $AA                     ;3
       bvc    LF347                   ;2
LF326:
       bit    PF0                     ;3
       jmp    LF349                   ;3
LF32B:
       lda    $DB                     ;3
       bpl    LF339                   ;2
       dec    $D9                     ;5
       bne    LF339                   ;2
       bit    $AA                     ;3
       bvc    LF34B                   ;2
       bvs    LF357                   ;2 always branch


;deal with ball/sprite collisions
LF339:
       bit    $AA                     ;3
       bvs    LF353                   ;2
       lda    $DB                     ;3
       and    #$20                    ;2
       beq    LF347                   ;2
       bit    PF0                     ;3
       bpl    LF34B                   ;2
LF347:
       bit    REFP1                   ;3
LF349:
       bmi    LF381                   ;2
LF34B:
       lda    $98                     ;3
       and    #$FB                    ;2
       ldx    #$00                    ;2
       bpl    LF35D                   ;2
LF353:
       bit    PF0                     ;3
       bmi    LF381                   ;2
LF357:
       ldx    #$01                    ;2
       lda    $98                     ;3
       ora    #$04                    ;2
LF35D:
       sta    $98                     ;3
       ldy    #$00                    ;2
       jmp    LF6DC                   ;3

LF364:
       ldy    #$01                    ;2
LF366:
       ldx    #$01                    ;2
       lda    $98                     ;3
       and    LFD82,y                 ;4
       bne    LF370                   ;2
       dex                            ;2
LF370:
       lda.wy $D5,y                   ;4
       cmp    #$EC                    ;2
       bcc    LF37E                   ;2
       lda    REFP1,x                 ;4
       bmi    LF37E                   ;2
       jmp    LF89B                   ;3

LF37E:
       dey                            ;2
       bpl    LF366                   ;2
LF381:
       jmp    LF427                   ;3


LF384:
       lda    $AA                     ;3
       and    #$07                    ;2
       bne    LF381                   ;2
       tay                            ;2
       lda    Invaders                ;3
       beq    LF381                   ;2
       lda    #$EB                    ;2
       sta    $EE                     ;3
       cmp    $D2                     ;3
       bcs    LF381                   ;2
       lda    $C7                     ;3
       asl                            ;2
       asl                            ;2
       clc                            ;2
       adc    $C7                     ;3
       clc                            ;2
       adc    #$59                    ;2
       sta    $C7                     ;3
       bpl    LF3BF                   ;2
       and    #$03                    ;2
       asl                            ;2
       sta    $EE                     ;3
       lda    $C7                     ;3
       lsr                            ;2
       lsr                            ;2
       tax                            ;2
LF3AF:
       txa                            ;2
       sec                            ;2
       adc    $EE                     ;3
       and    #$07                    ;2
       tax                            ;2
       lda    LFD80,x                 ;4
       and    $EB                     ;3
       beq    LF3AF                   ;2
       bne    LF3EE                   ;2 always branch

LF3BF:
       lda    $98                     ;3
       and    #$04                    ;2
       beq    LF3C6                   ;2
       iny                            ;2
LF3C6:
       ldx    #$05                    ;2
LF3C8:
       lda    LFD80,x                 ;4
       and    $EB                     ;3
       beq    LF3DF                   ;2
       lda    $9A                     ;3
       clc                            ;2
       adc    #$FD                    ;2
       clc                            ;2
       adc    LFFB6,x                 ;4
       cmp.wy $9C,y                   ;4
       bcc    LF3E3                   ;2
       stx    $EE                     ;3
LF3DF:
       dex                            ;2
       bpl    LF3C8                   ;2
       inx                            ;2
LF3E3:
       lda    $C7                     ;3
       and    #$10                    ;2
       bne    LF3EE                   ;2
       lda    $EE                     ;3
       bmi    LF3EE                   ;2
       tax                            ;2
LF3EE:
       stx    $EF                     ;3
       lda    LFD80,x                 ;4
       sta    $F0                     ;3
       ldx    #$FF                    ;2
LF3F7:
       inx                            ;2
       cpx    #$06                    ;2
       bcs    LF427                   ;2
       lda    $92,x                   ;4
       and    $F0                     ;3
       beq    LF3F7                   ;2
       lda    #$3C                    ;2
       adc    $90                     ;3
       sbc    LFF95,x                 ;4
       sta    $D2                     ;3
       sec                            ;2
       sbc    $D1                     ;3
       cmp    #$10                    ;2
       bcc    LF423                   ;2
       cmp    #$F1                    ;2
       bcs    LF423                   ;2
       ldy    $EF                     ;3
       lda    $9A                     ;3
       adc    LFFB6,y                 ;4
       adc    #$04                    ;2
       sta    $D4                     ;3
       bne    LF427                   ;2
LF423:
       lda    #$F6                    ;2
       sta    $D2                     ;3
LF427:
       lda    $AA                     ;3
       and    #$07                    ;2
       bne    LF4A6                   ;2
       bit    $98                     ;3
       bmi    LF4A6                   ;2
       ldy    #$FF                    ;2
       lda    Invaders                ;3
       beq    LF4A6                   ;2
LF437:
       iny                            ;2
       cmp    LFFC8,y                 ;4
       bcc    LF437                   ;2
       lda    LFF9C,y                 ;4
       sta    $EE                     ;3
       lda    LFFD4,y                 ;4
       sta    $EF                     ;3
       lda    Framecount              ;3
       and    #$3F                    ;2
       sta    $F0                     ;3
       clc                            ;2
       adc    $EF                     ;3
       cmp    #$41                    ;2
       bcs    LF4A6                   ;2
       lda    $F0                     ;3
LF456:
       beq    LF460                   ;2
       cmp    $EF                     ;3
       bcc    LF4A6                   ;2
       sbc    $EF                     ;3
       bpl    LF456                   ;2 always branch

LF460:
       bit    $8B                     ;3
       lda    #$09                    ;2
       bvs    LF468                   ;2
       lda    #$FF                    ;2
LF468:
       sta    $8B                     ;3
       lda    #$01                    ;2
       jmp    LFB42                   ;3

LF46F:
       lda    $98                     ;3
       and    #$02                    ;2
       beq    LF484                   ;2
       lda    $9A                     ;3
       clc                            ;2
       adc    $EE                     ;3
       sta    $9A                     ;3
       cmp    $8D                     ;3
       bcc    LF4A6                   ;2
       lda    $8D                     ;3
       bne    LF491                   ;2
LF484:
       lda    $9A                     ;3
       sec                            ;2
       sbc    $EE                     ;3
       sta    $9A                     ;3
       cmp    #$17                    ;2
       bcs    LF4A6                   ;2
       lda    #$17                    ;2
LF491:
       sta    $9A                     ;3
       lda    $98                     ;3
       eor    #$02                    ;2
       sta    $98                     ;3
       bmi    LF4A6                   ;2
       bit    $AA                     ;3
       bmi    LF4A6                   ;2
       lda    $90                     ;3
       clc                            ;2 don't remove
       adc    #$05                    ;2 move invaders lower
       sta    $90                     ;3
LF4A6:
       lda    #$05                    ;2
       sta    $8C                     ;3
       lda    #$0B                    ;2
       sec                            ;2
       sbc    $90                     ;3
       sta    $8E                     ;3
       bit    $98                     ;3
       bvs    LF4BC                   ;2
       lda    $8E                     ;3
       clc                            ;2
       adc    #$0C                    ;2
       sta    $8E                     ;3
LF4BC:
       ldx    #$FB                    ;2
LF4BE:
       lda    $97,x                   ;4
       bne    LF4CE                   ;2
       dec    $8C                     ;5
       lda    $8E                     ;3
       clc                            ;2
       adc    #$09                    ;2
       sta    $8E                     ;3
       inx                            ;2
       bne    LF4BE                   ;2
LF4CE:
       lda    $8E                     ;3
       bpl    LF508                   ;2
       lda    $98                     ;3
       and    #$40                    ;2
       beq    LF4E5                   ;2
       eor    $98                     ;3
       sta    $98                     ;3
       lda    $8E                     ;3
       clc                            ;2
       adc    #$0C                    ;2
       sta    $8E                     ;3
       bpl    LF508                   ;2
LF4E5:
       lda    #$00                    ;2
       sta    $8E                     ;3
       lda    $98                     ;3
       bmi    LF508                   ;2
       ora    #$80                    ;2
       sta    $98                     ;3
       ldy    $8C                     ;3
       lda    $90                     ;3
       clc                            ;2
       adc    Marquee0a,y             ;4
       sta    $90                     ;3
       lda    Framecount              ;3
       and    #$01                    ;2
       ora    #$80                    ;2
       sta    Framecount              ;3
       lda    #$06                    ;2
       jmp    LFB0B                   ;3

LF508:
       ldx    #$05                    ;2
       lda    #$00                    ;2
LF50C:
       ora    $92,x                   ;4
       dex                            ;2
       bpl    LF50C                   ;2
       sta    $EB                     ;3
LF513:
       lda    $EB                     ;3
       beq    LF55F                   ;2
       lsr                            ;2
       bcs    LF54D                   ;2
       lda    $C8                     ;3
       and    #$39                    ;2
       cmp    #$39                    ;2
       bne    LF52A                   ;2
       lda    #$B4                    ;2
       sta    $9E                     ;3
       lda    #$00                    ;2
       sta    $CC                     ;3
LF52A:
       lda    #$3A                    ;2
       sta    $C8                     ;3
;1st column empty...move all columns left
       ldx    #$05                    ;2
LF530:
       lsr    $92,x                   ;6
       dex                            ;2
       bpl    LF530                   ;2
       inx                            ;2
       lda    $98                     ;3
       and    #$04                    ;2
       beq    LF53D                   ;2
       inx                            ;2
LF53D:
       lda    Lives,x                 ;4
       ora    #$20                    ;2 set a bit to disable double rainbow bonus
       sta    Lives,x                 ;4
       lda    $9A                     ;3
       adc    #$10                    ;2
       sta    $9A                     ;3
       lsr    $EB                     ;5
       bne    LF513                   ;2
LF54D:
       ldx    #$06                    ;2
LF54F:
       dex                            ;2
       lda    LFD80,x                 ;4
       and    $EB                     ;3
       beq    LF54F                   ;2
       lda    #$82                    ;2
       sec                            ;2
       sbc    LFFB6,x                 ;4
       sta    $8D                     ;3
LF55F:
       lda    $90                     ;3
       sta    $8F                     ;3
       ldx    #$04                    ;2
LF565:
       lda    $99,x                   ;4 horizontal position
       jmp    LF82E                   ;3

LF56A:
       dex                            ;2
       bne    LF565                   ;2
       lda    #$F7                    ;2
       sta    $EE                     ;3
       lda    #$01                    ;2 color shift
       eor    $EA                     ;3
       and    $EE                     ;3
       sta    ColorShift              ;3
       bit    $AA                     ;3
       bmi    LF581                   ;2
       lda    $EA                     ;3
       bne    LF592                   ;2
LF581:
       lda    $C8                     ;3
       and    #$38                    ;2
       cmp    #$30                    ;2
       bne    LF592                   ;2
       lda    Variation               ;3
       and    #$08                    ;2
       beq    LF592                   ;2
       txa                            ;2
       sta    ColorShift              ;3
LF592:
       lda    Framecount              ;3
       tax                            ;2
       and    #$0F                    ;2
       ora    #$30                    ;2
       bit    FLAG2                   ;3
       bvs    LF59F                   ;2
       eor    #$50                    ;2
LF59F:
       sta    COLUP0                  ;3 mothership
       lda    #SKY                    ;2 (sky color)
       sta    COLUBK                  ;3
       txa                            ;2
       and    #$0F                    ;2
       sta    COLUPF                  ;3 missile
       txa                            ;2
       lsr                            ;2
       ldx    #$04                    ;2
       bcs    LF5B2                   ;2
       ldx    #$00                    ;2
LF5B2:
       stx    $EF                     ;3
       lda    $D1,x                   ;4
       cmp    $D2,x                   ;4
       bcc    LF5E5                   ;2
       sta    $81                     ;3
       lda    $D2,x                   ;4
       sta    $D1,x                   ;4
       lda    $81                     ;3
       sta    $D2,x                   ;4
       lda    $D3,x                   ;4
       sta    $81                     ;3
       lda    $D4,x                   ;4
       sta    $D3,x                   ;4
       lda    $81                     ;3
       sta    $D4,x                   ;4
       lda    Framecount              ;3
       lsr                            ;2
       bcc    LF5E5                   ;2
       lda    $98                     ;3
       and    #$0C                    ;2
       lsr                            ;2
       lsr                            ;2
       tay                            ;2
       lda    $98                     ;3
       and    #$F3                    ;2
       ora    LFFD0,y                 ;4
       sta    $98                     ;3
LF5E5:
       lda    $D4,x                   ;4
       jmp    LFB29                   ;3
LF5EA:
       ldx    $EF                     ;3
       lda    $D1,x                   ;4
       sta    $89                     ;3
       lda    $D2,x                   ;4
       cmp    #$EC                    ;2
       bcs    LF5F9                   ;2
       sec                            ;2
       sbc    $D1,x                   ;4
LF5F9:
       sta    $8A                     ;3
;adjust invader color
       lda    $90                     ;3
       lsr                            ;2
       lsr                            ;2
       tay                            ;2
       lsr                            ;2
       lsr                            ;2
       sta    Temp                    ;3
       tya                            ;2
       sec                            ;2
       sbc    Temp                    ;3
       eor    #$0F                    ;2
       bit    FLAG2                   ;3
       bvs    LF610                   ;2
       adc    #$0F                    ;2
LF610:
       adc    #<LCOLOR                ;2
       sta    Icolor                  ;3
       lda    $D3,x                   ;4
       jmp    LFC00                   ;3
LF619:
       sta    WSYNC                   ;3
       ldx    #$04                    ;2
       iny                            ;2
       sta    HMP0,x                  ;4
       NOP                            ;2
LF621:
       dey                            ;2
       bpl    LF621                   ;2
       sta    RESP0,x                 ;4
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       jmp    LF000                   ;3 jump bank and run display kernal



;various redundancy routines
LF62D:
       ldx    #$00                    ;2
       stx    P1scoreHi               ;3
       stx    P1scoreLo               ;3
       stx    $EA                     ;3
       lda    P2scoreHi               ;3
       cmp    #$99                    ;2
       bcs    LF63F                   ;2
       bit    Lives                   ;3
       bvc    LF643                   ;2 don't reset p2 score
LF63F:
       stx    P2scoreHi               ;3
       stx    P2scoreLo               ;3
LF643:
;reset shot counter to 1 for both players
       lda    #$11                    ;2
       sta    SHOT                    ;3
       lda    #$01                    ;2
       sta    $AA                     ;3
       lda    #$00                    ;2
       sta    $90                     ;3
       sta    $99                     ;3
       sta    $C6                     ;3
       lda    Framecount              ;3
       and    #$01                    ;2
       ora    #$80                    ;2
       sta    Framecount              ;3
       lda    Lives                   ;3
       and    #$44                    ;2
       ora    #$03                    ;2 3 lives
       sta    Lives                   ;3
       lda    #$FF                    ;2
       sta    $8B                     ;3
       lda    $98                     ;3
       and    #$53                    ;2
       ldy    $EC                     ;3
       ora    LFFC1,y                 ;4
       ora    LFF1B,y                 ;4
       sta    $98                     ;3
       lda    $E5                     ;3
       and    #$7F                    ;2
       sta    $E5                     ;3
       ldx    #$05                    ;2
       lda    #$3F                    ;2
LF67F:
       sta    $92,x                   ;4
       dex                            ;2
       bpl    LF67F                   ;2
       sta    $EB                     ;3
       sta    $D7                     ;3
       sta    $D8                     ;3
       sta    $D3                     ;3
       sta    $D4                     ;3
       lda    #$F6                    ;2
       sta    $D5                     ;3
       sta    $D6                     ;3
       sta    $D1                     ;3
       sta    $D2                     ;3
       ldx    #$05                    ;2
LF69A:
       lda    LFFA8,x                 ;4
       sta    $99,x                   ;4
       dex                            ;2
       bne    LF69A                   ;2
       txa                            ;2
       sta    $CB                     ;3
       sta    $CC                     ;3
       lda    #$24                    ;2
       sta    Invaders                ;3
       lda    #$42                    ;2
       ora    $98                     ;3
       sta    $98                     ;3
       lda    #$30                    ;2
       sta    $C8                     ;3
       lda    $AA                     ;3
       and    #$F7                    ;2
       sta    $AA                     ;3
       ldx    #$1A                    ;2
       ldy    #$08                    ;2
LF6BF:
       lda    ShieldGFX,y             ;4
       sta    $AB,x                   ;4
       dey                            ;2
       bpl    LF6C9                   ;2
       ldy    #$08                    ;2
LF6C9:
       dex                            ;2
       bpl    LF6BF                   ;2
       ldx    #$0A                    ;2
LF6CE:
       lda    $90,x                   ;4
       sta    $9F,x                   ;4
       dex                            ;2
       bpl    LF6CE                   ;2
       lda    #$6E                    ;2
       sta    $A7                     ;3
       jmp    LF2EA                   ;3




LF6DC:
       stx    $F2                     ;3
       ldx    LFFB5,y                 ;4
       lda    $D5,x                   ;4
       cmp    #$56                    ;2
       bcs    LF6EB                   ;2
       cmp    #$45                    ;2
       bcs    LF72D                   ;2
LF6EB:
       ldx    $F2                     ;3
       lda    #$55                    ;2
       sta.wy $D5,y                   ;5
       LDA    #$00                    ;2
       bit    DifSw                   ;3
       bvc    LF6FA                   ;2
       lda    #$06                    ;2
LF6FA:
       clc                            ;2
       adc    #$05                    ;2
       adc    $9C,x                   ;4
       sta.wy $D7,y                   ;5
;shot counter
       lda    SHOT                    ;3
       tax                            ;2
       and    #$F0                    ;2
       sta    SHOT                    ;3
       inx                            ;2
       txa                            ;2
       and    #$0F                    ;2
       ora    SHOT                    ;3
       sta    SHOT                    ;3
       lda    #$03                    ;2
       cmp    $CC                     ;3
       bcc    LF71F                   ;2
       sta    $CC                     ;3
       lda    #$00                    ;2
       sta    $CE                     ;3
       sta    $D0                     ;3
LF71F:
       lda    $DB                     ;3
       bpl    LF72D                   ;2
       lda    $AA                     ;3
       eor    #$40                    ;2
       sta    $AA                     ;3
       lda    #$50                    ;2
       sta    $D9                     ;3
LF72D:
       jmp    LF427                   ;3







;boot game
START:
       cld                            ;2
       sei                            ;2
       lda    #$00                    ;2
       tay                            ;2 Console type = 2600
       ldx    $D0                     ;3
       cpx    #$2C                    ;2 $D0 contain value $2C?
       bne    LF743                   ;2 branch if not (keep 2600 mode)
       ldx    $D1                     ;3
       cpx    #$A9                    ;2 $D1 contain value $A9?
       bne    LF743                   ;2 branch if not (keep 2600 mode)
       ldy    #$80                    ;2 high bit...set 7800 mode
;clear ram
LF743:
       tax                            ;2
LF744:
       dex                            ;2
       txs                            ;2
       pha                            ;3
       bne    LF744                   ;2

       sty    FLAG2                   ;3 store game mode
       inc    Temp                    ;5
;ram initialization done
       jmp    LFB51                   ;3

LF750:
       lda    #$C5                    ;2
       sta    $EA                     ;3
       lda    #$80                    ;2
       sta    $AA                     ;3
LF758:
       lda    #$02                    ;2
       sta    WSYNC                   ;3
       sta    VSYNC                   ;3
       sta    WSYNC                   ;3
       sta    WSYNC                   ;3
       ldy    #$26                    ;2
       sta    Waste2                  ;3
       lsr                            ;2
       sta    VSYNC                   ;3
LF769:
       sta    WSYNC                   ;3
       dey                            ;2
       bne    LF769                   ;2
       sta    VBLANK                  ;3
       ldx    #$0C                    ;2
LF772:
       sta    WSYNC                   ;3
       dex                            ;2
       bne    LF772                   ;2
       lda    #$FD                    ;2
       sta    $A2                     ;3
       ldy    #$28                    ;2
LF77D:
       sta    WSYNC                   ;3
;blank line between each row
       lda    #$00                    ;2
       sta    PF0                     ;3
       sta    PF1                     ;3
       sta    PF2                     ;3
;use to select current color table
       lda    Temp                    ;3
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       tax                            ;2
       lda    TitleColorTbl,x         ;4
       sta    $A1                     ;3
       ldx    #$03                    ;2
       stx    Icolor                  ;3
       lda    ($A1),y                 ;5
       tax                            ;2
;transfer title bitmap to playfield
LF79C:
       sta    WSYNC                   ;3
       stx    COLUPF                  ;3
       lda    Marquee0-1,y            ;4
       sta    PF0                     ;3
       lda    Marquee1-1,y            ;4
       sta    PF1                     ;3
       lda    Marquee2-1,y            ;4
       sta    PF2                     ;3
       NOP                            ;2
       NOP                            ;2
       lda    Marquee3-1,y            ;4
       sta    PF0                     ;3
       lda    Marquee4-1,y            ;4
       sta    PF1                     ;3
       lda    Marquee5-1,y            ;4
       sta    PF2                     ;3
       dec    Icolor                  ;5
       bne    LF79C                   ;2
       dey                            ;2
       bne    LF77D                   ;2
       sta    WSYNC                   ;3
       lda    #$09                    ;2
       tax                            ;2
       inx                            ;2
LF7CD:
       dex                            ;2
       bpl    LF7CD                   ;2
       sty    RESP0                   ;3
       ldy    #$88                    ;2
       sty    RESP1                   ;3
       sty    HMP0                    ;3
       sta    HMP1                    ;3
       lda    Temp                    ;3
       sty    WSYNC                   ;3
       sty    HMOVE                   ;3
       sty    REFP1                   ;3 reflect "20" to be "05" in GRP1!
       and    #$80                    ;2 both sprites = dark blue
       sta    COLUP0                  ;3
       sta    COLUP1                  ;3
       ldy    #$0F                    ;2 16 lines
LF7EA:
       lda    YearGFX,y               ;4 copy year bitmap to both sprites
       sta    GRP0                    ;3
       sta    GRP1                    ;3
       sta    WSYNC                   ;3
       dey                            ;2
       bpl    LF7EA                   ;2
       sta    REFP1                   ;3 reflect off
       ldy    #$07                    ;2
LF7FA:
       sta    WSYNC                   ;3
       dey                            ;2
       bne    LF7FA                   ;2
       lda    #$02                    ;2
       sta    VBLANK                  ;3
       ldx    #$15                    ;2
LF805:
       sta    WSYNC                   ;3
       dex                            ;2
       bpl    LF805                   ;2
       inc    $A3                     ;5
       lda    $A3                     ;3
       lsr                            ;2
       bcc    LF813                   ;2
       inc    Temp                    ;5
LF813:
       lda    SWCHA                   ;4
       asl                            ;2
       and    #$04                    ;2 save to disable intermissions if p2 down pressed
       sta    Lives                   ;3
       lda    Temp                    ;3
       beq    LF82B                   ;2
       and    SWCHA                   ;4
       cmp    Temp                    ;3 return if either stick moved
       bne    LF82B                   ;2
       sta    WSYNC                   ;3
       jmp    LF758                   ;3 go back and do another frame
LF82B:
       jmp    LF00E                   ;3 exit boot screen






;horizontal positioning
LF82E:
       ldy    #$FF                    ;2
       sec                            ;2
LF831:
       iny                            ;2
       sbc    #$0F                    ;2
       bcs    LF831                   ;2
       eor    #$FF                    ;2
       sbc    #$06                    ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       sty    $83,x                   ;4
       ora    $83,x                   ;4
       sta    $83,x                   ;4
       jmp    LF56A                   ;3




LF847:
       ldx    #$05                    ;2
       lda    #$3F                    ;2
LF84B:
       sta    $92,x                   ;4
       dex                            ;2
       bpl    LF84B                   ;2
       sta    $EB                     ;3
       sta    $D7                     ;3
       sta    $D8                     ;3
       sta    $D3                     ;3
       sta    $D4                     ;3
       lda    #$F6                    ;2
       sta    $D5                     ;3
       sta    $D6                     ;3
       sta    $D1                     ;3
       sta    $D2                     ;3
       ldx    #$05                    ;2
LF866:
       lda    LFFA8,x                 ;4
       sta    $99,x                   ;4
       dex                            ;2
       bne    LF866                   ;2
       txa                            ;2
       sta    $CB                     ;3
       sta    $CC                     ;3
       lda    #$24                    ;2
       sta    Invaders                ;3
       lda    #$42                    ;2
       ora    $98                     ;3
       sta    $98                     ;3
       lda    #$30                    ;2
       sta    $C8                     ;3
       lda    $AA                     ;3
       and    #$F7                    ;2
       sta    $AA                     ;3
       ldx    #$1A                    ;2
       ldy    #$08                    ;2
LF88B:
       lda    ShieldGFX,y             ;4
       sta    $AB,x                   ;4
       dey                            ;2
       bpl    LF895                   ;2
       ldy    #$08                    ;2
LF895:
       dex                            ;2
       bpl    LF88B                   ;2
       jmp    LF130                   ;3




LF89B:
       stx    $F2                     ;3
       ldx    LFFB5,y                 ;4
       lda    $D5,x                   ;4
       cmp    #$56                    ;2
       bcs    LF8AA                   ;2
       cmp    #$45                    ;2
       bcs    LF8EC                   ;2
LF8AA:
       ldx    $F2                     ;3
       lda    #$55                    ;2
       sta.wy $D5,y                   ;5
       lda    #$00                    ;2
       bit    DifSw                   ;3
       bvc    LF8B9                   ;2
       lda    #$06                    ;2
LF8B9:
       clc                            ;2
       adc    #$05                    ;2
       adc    $9C,x                   ;4
       sta.wy $D7,y                   ;5
;shot counter
       lda    SHOT                    ;3
       tax                            ;2
       and    #$F0                    ;2
       sta    SHOT                    ;3
       inx                            ;2
       txa                            ;2
       and    #$0F                    ;2
       ora    SHOT                    ;3
       sta    SHOT                    ;3
       lda    #$03                    ;2
       cmp    $CC                     ;3
       bcc    LF8DE                   ;2
       sta    $CC                     ;3
       lda    #$00                    ;2
       sta    $CE                     ;3
       sta    $D0                     ;3
LF8DE:
       lda    $DB                     ;3
       bpl    LF8EC                   ;2
       lda    $AA                     ;3
       eor    #$40                    ;2
       sta    $AA                     ;3
       lda    #$50                    ;2
       sta    $D9                     ;3
LF8EC:
       jmp    LF37E                   ;3






;intermission kernal
LF8EF:
       lda    INPT5                   ;3
       rol                            ;2
       lda    INPT4                   ;3
       ror                            ;2
       and    #$C0                    ;2
       sta    $9B                     ;3 save button status of both sticks
       ldx    #$42                    ;2
       stx    $C8                     ;3 set invader HPOS
       ldx    #$66                    ;2
       stx    $CB                     ;3
       ldx    #$7F                    ;2
       stx    $9A                     ;3 set wait value to maximum
LF905:
;text setup
       ldx    #$0A                    ;2
       ldy    #$05                    ;2
       lda    P2scoreHi               ;3
       eor    P1scoreLo               ;3
       and    #$02                    ;2
       beq    LF917                   ;2
       lda    Framecount              ;3
       bpl    LF917                   ;2
       ldy    #$0B                    ;2
LF917:
       lda    TextTbl,y               ;4
       sta    $AB,x                   ;4
       lda    #>TextGFX               ;2
       sta    $AC,x                   ;4
       dey                            ;2
       dex                            ;2
       dex                            ;2
       bpl    LF917                   ;2
       lda    INPT5                   ;3
       rol                            ;2
       lda    INPT4                   ;3
       ror                            ;2
       and    #$C0                    ;2
       tax                            ;2
       cpx    $9B                     ;3
       beq    LF93C                   ;2
       cpx    #$C0                    ;2
       beq    LF93C                   ;2
       lda    $9C                     ;3
       eor    #$80                    ;2
       sta    $9C                     ;3
LF93C:
       stx    $9B                     ;3
       lda    #$00                    ;2
       sta    AUDV1                   ;3
       ldy    #$07                    ;2
       ldx    #$07                    ;2
       sta    WSYNC                   ;3
LF948:
       dex                            ;2
       bne    LF948                   ;2
       sta    RESP0                   ;3
       lda    #$93                    ;2
       sta    RESP1                   ;3
       sta    HMP0                    ;3
       sty    HMP1                    ;3
       sta    NUSIZ0                  ;3
       sta    NUSIZ1                  ;3
       ldx    #$0A                    ;2
LF95B:
       lda    INTIM                   ;4
       bne    LF95B                   ;2
       sta    VBLANK                  ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       stx    COLUP0                  ;3
       stx    COLUP1                  ;3
       stx    $EB                     ;3
       dex                            ;2
       dex                            ;2
       NOP                            ;2
LF96F:
       dex                            ;2
       bpl    LF96F                   ;2
LF972:
       ldy    $EB                     ;3
       lda    ($AB),y                 ;5
       sta    GRP0                    ;3
       bit    VSYNC                   ;3
       lda    ($AD),y                 ;5
       sta    GRP1                    ;3
       lda    ($B5),y                 ;5
       tax                            ;2
       txs                            ;2
       lda    ($AF),y                 ;5
       sta    $CC                     ;3
       lda    ($B1),y                 ;5
       tax                            ;2
       lda    ($B3),y                 ;5
       ldy    $CC                     ;3
       sty    GRP0                    ;3
       stx    GRP1                    ;3
       sta    GRP0                    ;3
       tsx                            ;2
       stx    GRP1                    ;3
       dec    $EB                     ;5
       bpl    LF972                   ;2
       lda    $CB                     ;3
       NOP                            ;2
       NOP                            ;2
       tay                            ;2
       and    #$0F                    ;2
       sta    WSYNC                   ;3
       tax                            ;2
       lda    #$00                    ;2
       sta    COLUP0                  ;3
       sta    COLUP1                  ;3
       sty    HMP1                    ;3
       NOP                            ;2
       dex                            ;2
LF9AE:
       dex                            ;2
       bpl    LF9AE                   ;2
       sta    RESP1                   ;3
       sta    WSYNC                   ;3
       sta    HMOVE                   ;3
       txs                            ;2
       inx                            ;2
       stx    NUSIZ0                  ;3
       stx    NUSIZ1                  ;3
       stx    GRP0                    ;3
       stx    GRP1                    ;3
       lda    P2scoreHi               ;3
       eor    P1scoreLo               ;3
       and    #$03                    ;2
       bit    FLAG2                   ;3
       bvs    LF9CD                   ;2
       ora    #$04                    ;2
LF9CD:
       tay                            ;2
       lda    IntermissionColorTbl,y  ;4
       sta    COLUP1                  ;3
       ldy    #>EscapeShip1           ;2
       sty    $9E                     ;3
       lda    Framecount              ;3
       and    #$04                    ;2
       asl                            ;2
       beq    LF9E0                   ;2
       ora    #$01                    ;2 ship gfx index = 0 or 9
LF9E0:
       sta    WSYNC                   ;3
       tay                            ;2
       lda    P2scoreHi               ;3
       eor    P1scoreLo               ;3
       and    #$02                    ;2
       beq    LFA3D                   ;2
       tya                            ;2
       ora    #$20                    ;2 alternate ship gfx #$20 from 1st
       tay                            ;2
       lda    Framecount              ;3
       bpl    LFA3D                   ;2
       bit    $9A                     ;3
       bpl    LFA3D                   ;2
       ldy    #$20                    ;2 kill animation when jettisoned

;"engine trouble" 2nd part
       sty    $9D                     ;3
       eor    #$FF                    ;2
       lsr                            ;2
       tay                            ;2
       bpl    LFA03                   ;2
LFA01:
       sta    WSYNC                   ;3
LFA03:
       dey                            ;2
       bpl    LFA01                   ;2
       ldy    #$08                    ;2
LFA08:
       lda    ($9D),y                 ;5
       sta    GRP1                    ;3
       sta    WSYNC                   ;3
       dey                            ;2
       cpy    #$03                    ;2
       bne    LFA08                   ;2
       stx    GRP1                    ;3
       lda    Framecount              ;3
       sec                            ;2
       sbc    #$7E                    ;2
       lsr                            ;2
       tax                            ;2
LFA1C:
       sta    WSYNC                   ;3
       ldy    #$00                    ;2
       txa                            ;2
       and    #$03                    ;2
       bne    LFA27                   ;2
       ldy    #$42                    ;2
LFA27:
       sty    GRP1                    ;3
       dex                            ;2
       bne    LFA1C                   ;2
       ldy    #$04                    ;2
LFA2E:
       lda    ($9D),y                 ;5
       sta    GRP1                    ;3
       sta    WSYNC                   ;3
       dey                            ;2
       bpl    LFA2E                   ;2
       stx    GRP1                    ;3
       ldy    #$41                    ;2
       bne    LFA5D                   ;2 always branch

;regular intermissions ("S.O.S!" and "Engine trouble" part 1)
LFA3D:
       sty    $9D                     ;3
       lda    Framecount              ;3
       eor    #$FF                    ;2
       lsr                            ;2
       tay                            ;2
LFA45:
       sta    WSYNC                   ;3
       dey                            ;2
       bpl    LFA45                   ;2
       ldy    #$09                    ;2
LFA4C:
       lda    ($9D),y                 ;5
       sta    GRP1                    ;3
       sta    WSYNC                   ;3
       dey                            ;2
       bpl    LFA4C                   ;2
       stx    GRP1                    ;3
       lda    Framecount              ;3
       lsr                            ;2
       tay                            ;2
LFA5B:
       sta    WSYNC                   ;3
LFA5D:
       dey                            ;2
       bpl    LFA5B                   ;2
       lda    #$50                    ;2
       sta    TIM64T                  ;4
       lda    Framecount              ;3
       bpl    LFA6F                   ;2
       lda    $9D                     ;3
       cmp    #<EscapeShip2-1         ;2
       bcs    LFA95                   ;2
LFA6F:
       lda    $C8                     ;3
       clc                            ;2
       adc    #$17                    ;2
       tay                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       lsr                            ;2
       sta    Temp                    ;3
       tya                            ;2
       and    #$0F                    ;2
       clc                            ;2
       sta    WSYNC                   ;3
       adc    Temp                    ;3
       cmp    #$0F                    ;2
       bcc    LFA8B                   ;2
       sbc    #$0F                    ;2
       inc    Temp                    ;5
LFA8B:
       eor    #$07                    ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       ora    Temp                    ;3
       sta    $CB                     ;3
LFA95:
       lda    INTIM                   ;4
       bne    LFA95                   ;2
       lda    #$02                    ;2
       sta    VBLANK                  ;3 disable TIA
       sta    WSYNC                   ;3
       sta    WSYNC                   ;3
       sta    VSYNC                   ;3
       sta    WSYNC                   ;3
       lda    #$30                    ;2
       sta    TIM64T                  ;4
       lda    #$00                    ;2
       sta    WSYNC                   ;3
       sta    VSYNC                   ;3
       ldx    SWCHA                   ;4
       inx                            ;2
       bne    LFB02                   ;2 return if stick is moved
       lda    SWCHB                   ;4
       cmp    DifSw                   ;3
       bne    LFB02                   ;2 return if console switch is moved
       bit    $9C                     ;3
       bmi    LFAF7                   ;2
       ldx    #$05                    ;2
       lda    Framecount              ;3
       bpl    LFAD4                   ;2
       lda    $9D                     ;3
       cmp    #<EscapeShip2-1         ;2
       bcc    LFAD4                   ;2
       ldx    #$00                    ;2
       dec    $9A                     ;5
       bpl    LFAF7                   ;2
LFAD4:
       stx    AUDC0                   ;3
       stx    AUDV0                   ;3
       lda    Framecount              ;3
       eor    #$15                    ;2
       sta    AUDF0                   ;3
       ldx    $C8                     ;3
       lda    Framecount              ;3
       lsr                            ;2
       bcc    LFAF0                   ;2
       and    #$10                    ;2
       beq    LFAED                   ;2
       dex                            ;2
       jmp    LFAEE                   ;3
LFAED:
       inx                            ;2
LFAEE:
       stx    $C8                     ;3
LFAF0:
       inc    Framecount              ;5
       beq    LFB02                   ;2
       jmp    LF905                   ;3
;pause intermission
LFAF7:
       lda    #$00                    ;2
       sta    AUDC0                   ;3
       sta    AUDV0                   ;3
       sta    AUDF0                   ;3
       jmp    LF905                   ;3
;exit intermission
LFB02:
       sta    AUDC0                   ;3
       sta    AUDV0                   ;3
       sta    AUDF0                   ;3
       jmp    LF11C                   ;3




;various redundancy routines
LFB0B:
       cmp    $CB                     ;3
       bcc    LFB17                   ;2
       sta    $CB                     ;3
       lda    #$00                    ;2
       sta    $CD                     ;3
       sta    $CF                     ;3
LFB17:
       jmp    LF508                   ;3





LFB1A:
       cmp    $CC                     ;3
       bcc    LFB26                   ;2
       sta    $CC                     ;3
       lda    #$00                    ;2
       sta    $CE                     ;3
       sta    $D0                     ;3
LFB26:
       jmp    LF427                   ;3





LFB29:
       ldy    #$FF                    ;2
       sec                            ;2
LFB2C:
       iny                            ;2
       sbc    #$0F                    ;2
       bcs    LFB2C                   ;2
       eor    #$FF                    ;2
       sbc    #$06                    ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       sty    $88                     ;3
       ora    $88                     ;3
       sta    $88                     ;3
       jmp    LF5EA                   ;3





LFB42:
       cmp    $CB                     ;3
       bcc    LFB4E                   ;2
       sta    $CB                     ;3
       lda    #$00                    ;2
       sta    $CD                     ;3
       sta    $CF                     ;3
LFB4E:
       jmp    LF46F                   ;3





LFB51:
       ldx    #$00                    ;2
       stx    P1scoreHi               ;3
       stx    P1scoreLo               ;3
       stx    $EA                     ;3
       lda    P2scoreHi               ;3
       cmp    #$99                    ;2
       bcs    LFB63                   ;2
       bit    Lives                   ;3
       bvc    LFB67                   ;2 don't reset p2 score
LFB63:
       stx    P2scoreHi               ;3
       stx    P2scoreLo               ;3
LFB67:
;reset shot counter to 1 for both players
       lda    #$11                    ;2
       sta    SHOT                    ;3
       lda    #$01                    ;2
       sta    $AA                     ;3
       lda    #$00                    ;2
       sta    $90                     ;3
       sta    $99                     ;3
       sta    $C6                     ;3
       lda    Framecount              ;3
       and    #$01                    ;2
       ora    #$80                    ;2
       sta    Framecount              ;3
       lda    Lives                   ;3
       and    #$44                    ;2
       ora    #$03                    ;2 3 lives
       sta    Lives                   ;3
       lda    #$FF                    ;2
       sta    $8B                     ;3
       lda    $98                     ;3
       and    #$53                    ;2
       ldy    $EC                     ;3
       ora    LFFC1,y                 ;4
       ora    LFF1B,y                 ;4
       sta    $98                     ;3
       lda    $E5                     ;3
       and    #$7F                    ;2
       sta    $E5                     ;3
       ldx    #$05                    ;2
       lda    #$3F                    ;2
LFBA3:
       sta    $92,x                   ;4
       dex                            ;2
       bpl    LFBA3                   ;2
       sta    $EB                     ;3
       sta    $D7                     ;3
       sta    $D8                     ;3
       sta    $D3                     ;3
       sta    $D4                     ;3
       lda    #$F6                    ;2
       sta    $D5                     ;3
       sta    $D6                     ;3
       sta    $D1                     ;3
       sta    $D2                     ;3
       ldx    #$05                    ;2
LFBBE:
       lda    LFFA8,x                 ;4
       sta    $99,x                   ;4
       dex                            ;2
       bne    LFBBE                   ;2
       txa                            ;2
       sta    $CB                     ;3
       sta    $CC                     ;3
       lda    #$24                    ;2
       sta    Invaders                ;3
       lda    #$42                    ;2
       ora    $98                     ;3
       sta    $98                     ;3
       lda    #$30                    ;2
       sta    $C8                     ;3
       lda    $AA                     ;3
       and    #$F7                    ;2
       sta    $AA                     ;3
       ldx    #$1A                    ;2
       ldy    #$08                    ;2
LFBE3:
       lda    ShieldGFX,y             ;4
       sta    $AB,x                   ;4
       dey                            ;2
       bpl    LFBED                   ;2
       ldy    #$08                    ;2
LFBED:
       dex                            ;2
       bpl    LFBE3                   ;2
       ldx    #$0A                    ;2
LFBF2:
       lda    $90,x                   ;4
       sta    $9F,x                   ;4
       dex                            ;2
       bpl    LFBF2                   ;2
       lda    #$6E                    ;2
       sta    $A7                     ;3
       jmp    LF750                   ;3





LFC00:
       ldy    #$FF                    ;2
       sec                            ;2 don't remove
LFC03:
       iny                            ;2
       sbc    #$0F                    ;2
       bcs    LFC03                   ;2
       eor    #$FF                    ;2
       sbc    #$06                    ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       asl                            ;2
       sty    $83                     ;3
       ora    $83                     ;3
       sta    $83                     ;3
       jmp    LF619                   ;3





LFC19:
       sta    $AA                     ;3
       lda    #$00                    ;2
       sta    $90                     ;3
       sta    $99                     ;3
       sta    $C6                     ;3
       lda    Framecount              ;3
       and    #$01                    ;2
       ora    #$80                    ;2
       sta    Framecount              ;3
       lda    Lives                   ;3
       and    #$44                    ;2
       ora    #$03                    ;2 3 lives
       sta    Lives                   ;3
       lda    #$FF                    ;2
       sta    $8B                     ;3
       lda    $98                     ;3
       and    #$53                    ;2
       ldy    $EC                     ;3
       ora    LFFC1,y                 ;4
       ora    LFF1B,y                 ;4
       sta    $98                     ;3
       lda    $E5                     ;3
       and    #$7F                    ;2
       sta    $E5                     ;3
       ldx    #$05                    ;2
       lda    #$3F                    ;2
LFC4F:
       sta    $92,x                   ;4
       dex                            ;2
       bpl    LFC4F                   ;2
       sta    $EB                     ;3
       sta    $D7                     ;3
       sta    $D8                     ;3
       sta    $D3                     ;3
       sta    $D4                     ;3
       lda    #$F6                    ;2
       sta    $D5                     ;3
       sta    $D6                     ;3
       sta    $D1                     ;3
       sta    $D2                     ;3
       ldx    #$05                    ;2
LFC6A:
       lda    LFFA8,x                 ;4
       sta    $99,x                   ;4
       dex                            ;2
       bne    LFC6A                   ;2
       txa                            ;2
       sta    $CB                     ;3
       sta    $CC                     ;3
       lda    #$24                    ;2
       sta    Invaders                ;3
       lda    #$42                    ;2
       ora    $98                     ;3
       sta    $98                     ;3
       lda    #$30                    ;2
       sta    $C8                     ;3
       lda    $AA                     ;3
       and    #$F7                    ;2
       sta    $AA                     ;3
       ldx    #$1A                    ;2
       ldy    #$08                    ;2
LFC8F:
       lda    ShieldGFX,y             ;4
       sta    $AB,x                   ;4
       dey                            ;2
       bpl    LFC99                   ;2
       ldy    #$08                    ;2
LFC99:
       dex                            ;2
       bpl    LFC8F                   ;2
       ldx    #$0A                    ;2
LFC9E:
       lda    $90,x                   ;4
       sta    $9F,x                   ;4
       dex                            ;2
       bpl    LFC9E                   ;2
       lda    #$6E                    ;2
       sta    $A7                     ;3
       jmp    LF2EA                   ;3


  IF PLUSROM = 0
       ORG $2CAC
       RORG $FCAC

;extra bytes...
       .byte ";Hack start date: 03/16/04"
       .byte ";Revisions:"
       .byte ";03/17/04"
       .byte ";03/18/04"
       .byte ";03/19/04"
       .byte ";03/20/04"
       .byte ";03/21/04"
       .byte ";10/04/04"
       .byte ";10/27/04"
       .byte ";11/27/04"
       .byte ";11/30/04"
       .byte ";12/04/04"
       .byte ";01/17/05"
       .byte ";03/04/05"
       .byte ";03/05/05"
       .byte ";09/20/05"
       .byte ";09/21/05"
       .byte ";09/24/05"
       .byte ";09/25/05"
       .byte ";09/26/05"
       .byte ";09/27/05"
  ELSE
SendPlusROMScore:
       lda Variation
       sta WriteToBuffer
       lda P1scoreHi
       sta WriteToBuffer
       lda P1scoreLo
       sta WriteToBuffer
       lda #HIGHSCORE_ID               ; game id in Highscore DB
       sta WriteSendBuffer             ; send request to backend..
       jmp LF1DB

IntermissionColorTbl:
;NTSC
       .byte $34 ; |  XX X  | $FFDD
       .byte $56 ; | X X XX | $FFDE
       .byte $98 ; |X  XX   | $FFDF
       .byte $C6 ; |XX   XX | $FFE0
;PAL
       .byte $64 ; | XX  X  | $FFE1
       .byte $86 ; |X    XX | $FFE2
       .byte $B8 ; |X XXX   | $FFE3
       .byte $56 ; | X X XX | $FFE4

TextTbl:
       .byte <SOS1     ; |  XX  X | $FFE5
       .byte <SOS2     ; |  XXX XX| $FFE6
       .byte <SOS3     ; | X   X  | $FFE7
       .byte <SOS4     ; |  XXX XX| $FFE8
       .byte <SOS5     ; |  XX  X | $FFE9
       .byte <SOS6     ; | X  XX X| $FFEA
       .byte <TROUBLE1 ; | X XX   | $FFEB
       .byte <TROUBLE2 ; | XX X  X| $FFEC
       .byte <TROUBLE3 ; | XXX X  | $FFED
       .byte <TROUBLE4 ; | XXXXXXX| $FFEE
       .byte <TROUBLE5 ; |X   X X | $FFEF
       .byte <TROUBLE6 ; | X XXXX | $FFF0
  ENDIF

       ORG $2D7C
       RORG $FD7C

TitleColorTbl:
       .byte <TitleColors1-1 ; |X    XXX| $FD7C
       .byte <TitleColors2-1 ; |X X XXXX| $FD7D
       .byte <TitleColors3-1 ; |XX X XXX| $FD7E
       .byte <TitleColors3-1 ; |XX X XXX| $FD7F

LFD80:
       .byte $01 ; |       X| $FD80
       .byte $02 ; |      X | $FD81
LFD82:
       .byte $04 ; |     X  | $FD82 shared
       .byte $08 ; |    X   | $FD83
       .byte $10 ; |   X    | $FD84
       .byte $20 ; |  X     | $FD85
       .byte $40 ; | X      | $FD86
       .byte $80 ; |X       | $FD87

TitleColors1:
       .byte $00 ; |        | $FD88
       .byte $00 ; |        | $FD89
       .byte $00 ; |        | $FD8A
       .byte $00 ; |        | $FD8B
       .byte $00 ; |        | $FD8C
       .byte $00 ; |        | $FD8D
       .byte $00 ; |        | $FD8E
       .byte $00 ; |        | $FD8F
       .byte $00 ; |        | $FD90
       .byte $00 ; |        | $FD91
       .byte $00 ; |        | $FD92
       .byte $00 ; |        | $FD93
       .byte $00 ; |        | $FD94
       .byte $00 ; |        | $FD95
       .byte $00 ; |        | $FD96
       .byte $00 ; |        | $FD97
       .byte $00 ; |        | $FD98
       .byte $00 ; |        | $FD99
       .byte $00 ; |        | $FD9A
       .byte $00 ; |        | $FD9B
       .byte $00 ; |        | $FD9C
       .byte $00 ; |        | $FD9D
       .byte $00 ; |        | $FD9E
       .byte $00 ; |        | $FD9F
       .byte $98 ; |X  XX   | $FDA0
       .byte $98 ; |X  XX   | $FDA1
       .byte $98 ; |X  XX   | $FDA2
       .byte $98 ; |X  XX   | $FDA3
       .byte $98 ; |X  XX   | $FDA4
       .byte $98 ; |X  XX   | $FDA5
       .byte $98 ; |X  XX   | $FDA6
       .byte $98 ; |X  XX   | $FDA7
       .byte $C6 ; |XX   XX | $FDA8
       .byte $C6 ; |XX   XX | $FDA9
       .byte $C6 ; |XX   XX | $FDAA
       .byte $C6 ; |XX   XX | $FDAB
       .byte $C6 ; |XX   XX | $FDAC
       .byte $C6 ; |XX   XX | $FDAD
       .byte $C6 ; |XX   XX | $FDAE
       .byte $C6 ; |XX   XX | $FDAF

TitleColors2:
       .byte $00 ; |        | $FDB0
       .byte $00 ; |        | $FDB1
       .byte $00 ; |        | $FDB2
       .byte $00 ; |        | $FDB3
       .byte $00 ; |        | $FDB4
       .byte $00 ; |        | $FDB5
       .byte $00 ; |        | $FDB6
       .byte $00 ; |        | $FDB7
       .byte $00 ; |        | $FDB8
       .byte $00 ; |        | $FDB9
       .byte $00 ; |        | $FDBA
       .byte $00 ; |        | $FDBB
       .byte $1A ; |   XX X | $FDBC
       .byte $1A ; |   XX X | $FDBD
       .byte $1A ; |   XX X | $FDBE
       .byte $1A ; |   XX X | $FDBF
       .byte $56 ; | X X XX | $FDC0
       .byte $56 ; | X X XX | $FDC1
       .byte $56 ; | X X XX | $FDC2
       .byte $56 ; | X X XX | $FDC3
       .byte $00 ; |        | $FDC4
       .byte $00 ; |        | $FDC5
       .byte $00 ; |        | $FDC6
       .byte $00 ; |        | $FDC7
       .byte $98 ; |X  XX   | $FDC8
       .byte $98 ; |X  XX   | $FDC9
       .byte $98 ; |X  XX   | $FDCA
       .byte $98 ; |X  XX   | $FDCB
       .byte $98 ; |X  XX   | $FDCC
       .byte $98 ; |X  XX   | $FDCD
       .byte $98 ; |X  XX   | $FDCE
       .byte $98 ; |X  XX   | $FDCF
       .byte $C6 ; |XX   XX | $FDD0
       .byte $C6 ; |XX   XX | $FDD1
       .byte $C6 ; |XX   XX | $FDD2
       .byte $C6 ; |XX   XX | $FDD3
       .byte $C6 ; |XX   XX | $FDD4
       .byte $C6 ; |XX   XX | $FDD5
       .byte $C6 ; |XX   XX | $FDD6
       .byte $C6 ; |XX   XX | $FDD7

TitleColors3:
       .byte $00 ; |        | $FDD8
       .byte $30 ; |  XX    | $FDD9
       .byte $32 ; |  XX  X | $FDDA
       .byte $32 ; |  XX  X | $FDDB
       .byte $32 ; |  XX  X | $FDDC
       .byte $32 ; |  XX  X | $FDDD
       .byte $32 ; |  XX  X | $FDDE
       .byte $32 ; |  XX  X | $FDDF
       .byte $32 ; |  XX  X | $FDE0
       .byte $00 ; |        | $FDE1
       .byte $00 ; |        | $FDE2
       .byte $00 ; |        | $FDE3
       .byte $1A ; |   XX X | $FDE4
       .byte $1A ; |   XX X | $FDE5
       .byte $1A ; |   XX X | $FDE6
       .byte $1A ; |   XX X | $FDE7
       .byte $56 ; | X X XX | $FDE8
       .byte $56 ; | X X XX | $FDE9
       .byte $56 ; | X X XX | $FDEA
       .byte $56 ; | X X XX | $FDEB
       .byte $00 ; |        | $FDEC
       .byte $00 ; |        | $FDED
       .byte $00 ; |        | $FDEE
       .byte $00 ; |        | $FDEF
       .byte $98 ; |X  XX   | $FDF0
       .byte $98 ; |X  XX   | $FDF1
       .byte $98 ; |X  XX   | $FDF2
       .byte $98 ; |X  XX   | $FDF3
       .byte $98 ; |X  XX   | $FDF4
       .byte $98 ; |X  XX   | $FDF5
       .byte $98 ; |X  XX   | $FDF6
       .byte $98 ; |X  XX   | $FDF7
       .byte $C6 ; |XX   XX | $FDF8
       .byte $C6 ; |XX   XX | $FDF9
       .byte $C6 ; |XX   XX | $FDFA
       .byte $C6 ; |XX   XX | $FDFB
       .byte $C6 ; |XX   XX | $FDFC
       .byte $C6 ; |XX   XX | $FDFD
       .byte $C6 ; |XX   XX | $FDFE
       .byte $C6 ; |XX   XX | $FDFF

YearGFX:
       .byte $00 ; |        | $FE00
       .byte $EE ; |XXX XXX | $FE01
       .byte $8A ; |X   X X | $FE02
       .byte $8A ; |X   X X | $FE03
       .byte $8A ; |X   X X | $FE04
       .byte $8A ; |X   X X | $FE05
       .byte $8A ; |X   X X | $FE06
       .byte $8A ; |X   X X | $FE07
       .byte $EA ; |XXX X X | $FE08
       .byte $2A ; |  X X X | $FE09
       .byte $2A ; |  X X X | $FE0A
       .byte $2A ; |  X X X | $FE0B
       .byte $2A ; |  X X X | $FE0C
       .byte $2A ; |  X X X | $FE0D
       .byte $2A ; |  X X X | $FE0E
       .byte $EE ; |XXX XXX | $FE0F

Marquee0:
       .byte $00 ; |        | $FE10
       .byte $00 ; |        | $FE11
Marquee0a:
       .byte $06 ; |     XX | $FE12 shared
       .byte $07 ; |     XXX| $FE13 shared
       .byte $03 ; |      XX| $FE14 shared
       .byte $04 ; |     X  | $FE15 shared
       .byte $05 ; |     X X| $FE16 shared
       .byte $06 ; |     XX | $FE17 shared
       .byte $00 ; |        | $FE18
       .byte $00 ; |        | $FE19
       .byte $00 ; |        | $FE1A
       .byte $00 ; |        | $FE1B
       .byte $B0 ; |X XX    | $FE1C
       .byte $B0 ; |X XX    | $FE1D
       .byte $B0 ; |X XX    | $FE1E
       .byte $B0 ; |X XX    | $FE1F
       .byte $B0 ; |X XX    | $FE20
       .byte $B0 ; |X XX    | $FE21
       .byte $B0 ; |X XX    | $FE22
       .byte $B0 ; |X XX    | $FE23
       .byte $00 ; |        | $FE24
       .byte $00 ; |        | $FE25
       .byte $00 ; |        | $FE26
       .byte $00 ; |        | $FE27
       .byte $00 ; |        | $FE28
       .byte $80 ; |X       | $FE29
       .byte $C0 ; |XX      | $FE2A
       .byte $C0 ; |XX      | $FE2B
       .byte $C0 ; |XX      | $FE2C
       .byte $00 ; |        | $FE2D
       .byte $80 ; |X       | $FE2E
       .byte $E0 ; |XXX     | $FE2F
       .byte $E0 ; |XXX     | $FE30
       .byte $70 ; | XXX    | $FE31
       .byte $30 ; |  XX    | $FE32
       .byte $30 ; |  XX    | $FE33
       .byte $F0 ; |XXXX    | $FE34
       .byte $F0 ; |XXXX    | $FE35
       .byte $F0 ; |XXXX    | $FE36
       .byte $E0 ; |XXX     | $FE37

Marquee1:
       .byte $00 ; |        | $FE38
       .byte $73 ; | XXX  XX| $FE39
       .byte $6B ; | XX X XX| $FE3A
       .byte $6B ; | XX X XX| $FE3B
       .byte $6B ; | XX X XX| $FE3C
       .byte $6B ; | XX X XX| $FE3D
       .byte $6B ; | XX X XX| $FE3E
       .byte $6B ; | XX X XX| $FE3F
       .byte $73 ; | XXX  XX| $FE40
       .byte $00 ; |        | $FE41
       .byte $00 ; |        | $FE42
       .byte $00 ; |        | $FE43
       .byte $91 ; |X  X   X| $FE44
       .byte $91 ; |X  X   X| $FE45
       .byte $B3 ; |X XX  XX| $FE46
       .byte $F3 ; |XXXX  XX| $FE47
       .byte $F6 ; |XXXX XX | $FE48
       .byte $D6 ; |XX X XX | $FE49
       .byte $96 ; |X  X XX | $FE4A
       .byte $96 ; |X  X XX | $FE4B
       .byte $00 ; |        | $FE4C
       .byte $00 ; |        | $FE4D
       .byte $00 ; |        | $FE4E
       .byte $00 ; |        | $FE4F
       .byte $E7 ; |XXX  XXX| $FE50
       .byte $F7 ; |XXXX XXX| $FE51
       .byte $F7 ; |XXXX XXX| $FE52
       .byte $37 ; |  XX XXX| $FE53
       .byte $37 ; |  XX XXX| $FE54
       .byte $77 ; | XXX XXX| $FE55
       .byte $F7 ; |XXXX XXX| $FE56
       .byte $E7 ; |XXX  XXX| $FE57
       .byte $8F ; |X   XXXX| $FE58
       .byte $0E ; |    XXX | $FE59
       .byte $EE ; |XXX XXX | $FE5A
       .byte $EE ; |XXX XXX | $FE5B
       .byte $EF ; |XXX XXXX| $FE5C
       .byte $EF ; |XXX XXXX| $FE5D
       .byte $CF ; |XX  XXXX| $FE5E
       .byte $0F ; |    XXXX| $FE5F

Marquee2:
       .byte $00 ; |        | $FE60
       .byte $7B ; | XXXX XX| $FE61
       .byte $18 ; |   XX   | $FE62
       .byte $18 ; |   XX   | $FE63
       .byte $19 ; |   XX  X| $FE64
       .byte $18 ; |   XX   | $FE65
       .byte $18 ; |   XX   | $FE66
       .byte $18 ; |   XX   | $FE67
       .byte $1B ; |   XX XX| $FE68
       .byte $00 ; |        | $FE69
       .byte $00 ; |        | $FE6A
       .byte $00 ; |        | $FE6B
       .byte $58 ; | X XX   | $FE6C
       .byte $58 ; | X XX   | $FE6D
       .byte $79 ; | XXXX  X| $FE6E
       .byte $59 ; | X XX  X| $FE6F
       .byte $5B ; | X XX XX| $FE70
       .byte $5B ; | X XX XX| $FE71
       .byte $33 ; |  XX  XX| $FE72
       .byte $33 ; |  XX  XX| $FE73
       .byte $00 ; |        | $FE74
       .byte $00 ; |        | $FE75
       .byte $00 ; |        | $FE76
       .byte $00 ; |        | $FE77
       .byte $C0 ; |XX      | $FE78
       .byte $C0 ; |XX      | $FE79
       .byte $60 ; | XX     | $FE7A
       .byte $60 ; | XX     | $FE7B
       .byte $C0 ; |XX      | $FE7C
       .byte $C0 ; |XX      | $FE7D
       .byte $83 ; |X     XX| $FE7E
       .byte $E7 ; |XXX  XXX| $FE7F
       .byte $6F ; | XX XXXX| $FE80
       .byte $6E ; | XX XXX | $FE81
       .byte $EE ; |XXX XXX | $FE82
       .byte $EE ; |XXX XXX | $FE83
       .byte $CF ; |XX  XXXX| $FE84
       .byte $CF ; |XX  XXXX| $FE85
       .byte $87 ; |X    XXX| $FE86
       .byte $83 ; |X     XX| $FE87

Marquee3:
       .byte $00 ; |        | $FE88
       .byte $E0 ; |XXX     | $FE89
       .byte $B0 ; |X XX    | $FE8A
       .byte $B0 ; |X XX    | $FE8B
       .byte $B0 ; |X XX    | $FE8C
       .byte $B0 ; |X XX    | $FE8D
       .byte $B0 ; |X XX    | $FE8E
       .byte $B0 ; |X XX    | $FE8F
       .byte $B0 ; |X XX    | $FE90
       .byte $00 ; |        | $FE91
       .byte $00 ; |        | $FE92
       .byte $00 ; |        | $FE93
       .byte $70 ; | XXX    | $FE94
       .byte $B0 ; |X XX    | $FE95
       .byte $B0 ; |X XX    | $FE96
       .byte $B0 ; |X XX    | $FE97
       .byte $B0 ; |X XX    | $FE98
       .byte $B0 ; |X XX    | $FE99
       .byte $B0 ; |X XX    | $FE9A
       .byte $70 ; | XXX    | $FE9B
       .byte $00 ; |        | $FE9C
       .byte $00 ; |        | $FE9D
       .byte $00 ; |        | $FE9E
       .byte $00 ; |        | $FE9F
       .byte $60 ; | XX     | $FEA0
       .byte $60 ; | XX     | $FEA1
       .byte $C0 ; |XX      | $FEA2
       .byte $C0 ; |XX      | $FEA3
       .byte $60 ; | XX     | $FEA4
       .byte $60 ; | XX     | $FEA5
       .byte $30 ; |  XX    | $FEA6
       .byte $F0 ; |XXXX    | $FEA7
       .byte $D0 ; |XX X    | $FEA8
       .byte $D0 ; |XX X    | $FEA9
       .byte $F0 ; |XXXX    | $FEAA
       .byte $F0 ; |XXXX    | $FEAB
       .byte $70 ; | XXX    | $FEAC
       .byte $70 ; | XXX    | $FEAD
       .byte $30 ; |  XX    | $FEAE
       .byte $30 ; |  XX    | $FEAF

Marquee4:
       .byte $00 ; |        | $FEB0
       .byte $6D ; | XX XX X| $FEB1
       .byte $6D ; | XX XX X| $FEB2
       .byte $39 ; |  XXX  X| $FEB3
       .byte $11 ; |   X   X| $FEB4
       .byte $11 ; |   X   X| $FEB5
       .byte $39 ; |  XXX  X| $FEB6
       .byte $6D ; | XX XX X| $FEB7
       .byte $6D ; | XX XX X| $FEB8
       .byte $00 ; |        | $FEB9
       .byte $00 ; |        | $FEBA
       .byte $00 ; |        | $FEBB
       .byte $7B ; | XXXX XX| $FEBC
       .byte $63 ; | XX   XX| $FEBD
       .byte $63 ; | XX   XX| $FEBE
       .byte $73 ; | XXX  XX| $FEBF
       .byte $63 ; | XX   XX| $FEC0
       .byte $63 ; | XX   XX| $FEC1
       .byte $63 ; | XX   XX| $FEC2
       .byte $7B ; | XXXX XX| $FEC3
       .byte $00 ; |        | $FEC4
       .byte $00 ; |        | $FEC5
       .byte $00 ; |        | $FEC6
       .byte $00 ; |        | $FEC7
       .byte $18 ; |   XX   | $FEC8
       .byte $3C ; |  XXXX  | $FEC9
       .byte $7E ; | XXXXXX | $FECA
       .byte $76 ; | XXX XX | $FECB
       .byte $76 ; | XXX XX | $FECC
       .byte $70 ; | XXX    | $FECD
       .byte $70 ; | XXX    | $FECE
       .byte $70 ; | XXX    | $FECF
       .byte $38 ; |  XXX   | $FED0
       .byte $38 ; |  XXX   | $FED1
       .byte $3B ; |  XXX XX| $FED2
       .byte $3B ; |  XXX XX| $FED3
       .byte $3F ; |  XXXXXX| $FED4
       .byte $3F ; |  XXXXXX| $FED5
       .byte $1E ; |   XXXX | $FED6
       .byte $0C ; |    XX  | $FED7

Marquee5:
       .byte $00 ; |        | $FED8
       .byte $07 ; |     XXX| $FED9
       .byte $01 ; |       X| $FEDA
       .byte $01 ; |       X| $FEDB
       .byte $03 ; |      XX| $FEDC
       .byte $01 ; |       X| $FEDD
       .byte $01 ; |       X| $FEDE
       .byte $01 ; |       X| $FEDF
       .byte $07 ; |     XXX| $FEE0
       .byte $00 ; |        | $FEE1
       .byte $00 ; |        | $FEE2
       .byte $00 ; |        | $FEE3
       .byte $72 ; | XXX  X | $FEE4
       .byte $DA ; |XX XX X | $FEE5
       .byte $C2 ; |XX    X | $FEE6
       .byte $71 ; | XXX   X| $FEE7
       .byte $1A ; |   XX X | $FEE8
       .byte $1A ; |   XX X | $FEE9
       .byte $DA ; |XX XX X | $FEEA
       .byte $71 ; | XXX   X| $FEEB
       .byte $00 ; |        | $FEEC
       .byte $00 ; |        | $FEED
       .byte $00 ; |        | $FEEE
       .byte $00 ; |        | $FEEF
       .byte $0F ; |    XXXX| $FEF0
       .byte $0F ; |    XXXX| $FEF1
       .byte $0F ; |    XXXX| $FEF2
       .byte $03 ; |      XX| $FEF3
       .byte $03 ; |      XX| $FEF4
       .byte $03 ; |      XX| $FEF5
       .byte $3E ; |  XXXXX | $FEF6
       .byte $3E ; |  XXXXX | $FEF7
       .byte $3E ; |  XXXXX | $FEF8
       .byte $0C ; |    XX  | $FEF9
       .byte $0C ; |    XX  | $FEFA
       .byte $0C ; |    XX  | $FEFB
       .byte $FC ; |XXXXXX  | $FEFC
       .byte $F8 ; |XXXXX   | $FEFD
       .byte $F8 ; |XXXXX   | $FEFE
       .byte $F8 ; |XXXXX   | $FEFF

EscapeShip1:
       .byte $18 ; |   XX   | $FF00
       .byte $A5 ; |X X  X X| $FF01
       .byte $BD ; |X XXXX X| $FF02
       .byte $FF ; |XXXXXXXX| $FF03
       .byte $FF ; |XXXXXXXX| $FF04
       .byte $5A ; | X XX X | $FF05
       .byte $3C ; |  XXXX  | $FF06
       .byte $18 ; |   XX   | $FF07
       .byte $24 ; |  X  X  | $FF08
       .byte $42 ; | X    X | $FF09 shared
       .byte $24 ; |  X  X  | $FF0A
       .byte $3C ; |  XXXX  | $FF0B
       .byte $7E ; | XXXXXX | $FF0C
       .byte $FF ; |XXXXXXXX| $FF0D
       .byte $DB ; |XX XX XX| $FF0E
       .byte $BD ; |X XXXX X| $FF0F
       .byte $99 ; |X  XX  X| $FF10
       .byte $24 ; |  X  X  | $FF11
       .byte $42 ; | X    X | $FF12

ShieldGFX:
       .byte $3C ; |  XXXX  | $FF13
       .byte $7E ; | XXXXXX | $FF14
       .byte $FF ; |XXXXXXXX| $FF15
       .byte $FF ; |XXXXXXXX| $FF16
       .byte $FF ; |XXXXXXXX| $FF17
       .byte $FF ; |XXXXXXXX| $FF18
       .byte $E7 ; |XXX  XXX| $FF19
       .byte $C3 ; |XX    XX| $FF1A
LFF1B:
       .byte $00 ; |        | $FF1B shared
       .byte $00 ; |        | $FF1C
       .byte $08 ; |    X   | $FF1D
       .byte $00 ; |        | $FF1E
       .byte $00 ; |        | $FF1F
EscapeShip2:
       .byte $00 ; |        | $FF20 shared
       .byte $24 ; |  X  X  | $FF21
       .byte $42 ; | X    X | $FF22
       .byte $24 ; |  X  X  | $FF23
       .byte $FF ; |XXXXXXXX| $FF24
       .byte $DB ; |XX XX XX| $FF25
       .byte $7E ; | XXXXXX | $FF26
       .byte $3C ; |  XXXX  | $FF27
       .byte $18 ; |   XX   | $FF28
       .byte $00 ; |        | $FF29 shared
       .byte $81 ; |X      X| $FF2A
       .byte $42 ; | X    X | $FF2B
       .byte $24 ; |  X  X  | $FF2C
       .byte $FF ; |XXXXXXXX| $FF2D
       .byte $DB ; |XX XX XX| $FF2E
       .byte $7E ; | XXXXXX | $FF2F
       .byte $3C ; |  XXXX  | $FF30
       .byte $18 ; |   XX   | $FF31
TextGFX:
SOS1:
SOS5:
       .byte $00 ; |        | $FF32 shared
       .byte $00 ; |        | $FF33
       .byte $3C ; |  XXXX  | $FF34
       .byte $66 ; | XX  XX | $FF35
       .byte $06 ; |     XX | $FF36
       .byte $3C ; |  XXXX  | $FF37
       .byte $60 ; | XX     | $FF38
       .byte $66 ; | XX  XX | $FF39
       .byte $3C ; |  XXXX  | $FF3A
SOS2:
SOS4:
       .byte $00 ; |        | $FF3B shared
       .byte $00 ; |        | $FF3C shared
       .byte $18 ; |   XX   | $FF3D
       .byte $18 ; |   XX   | $FF3E
       .byte $00 ; |        | $FF3F
       .byte $00 ; |        | $FF40
       .byte $00 ; |        | $FF41
       .byte $00 ; |        | $FF42
       .byte $00 ; |        | $FF43
SOS3:
       .byte $00 ; |        | $FF44 shared
       .byte $00 ; |        | $FF45 shared
       .byte $3C ; |  XXXX  | $FF46
       .byte $66 ; | XX  XX | $FF47
       .byte $66 ; | XX  XX | $FF48
       .byte $66 ; | XX  XX | $FF49
       .byte $66 ; | XX  XX | $FF4A
       .byte $66 ; | XX  XX | $FF4B
       .byte $3C ; |  XXXX  | $FF4C
SOS6:
       .byte $00 ; |        | $FF4D shared
       .byte $00 ; |        | $FF4E shared
       .byte $60 ; | XX     | $FF4F
       .byte $00 ; |        | $FF50
       .byte $30 ; |  XX    | $FF51
       .byte $38 ; |  XXX   | $FF52
       .byte $1C ; |   XXX  | $FF53
       .byte $1E ; |   XXXX | $FF54
       .byte $1E ; |   XXXX | $FF55
       .byte $00 ; |        | $FF56
       .byte $00 ; |        | $FF57

TROUBLE1:
       .byte $30 ; |  XX    | $FF58
       .byte $30 ; |  XX    | $FF59
       .byte $30 ; |  XX    | $FF5A
       .byte $30 ; |  XX    | $FF5B
       .byte $FC ; |XXXXXX  | $FF5C
       .byte $00 ; |        | $FF5D
TROUBLE6:
       .byte $7E ; | XXXXXX | $FF5E shared
       .byte $60 ; | XX     | $FF5F shared
       .byte $78 ; | XXXX   | $FF60 shared
       .byte $60 ; | XX     | $FF61 shared
       .byte $7E ; | XXXXXX | $FF62 shared
       .byte $00 ; |        | $FF63
       .byte $7E ; | XXXXXX | $FF64
       .byte $60 ; | XX     | $FF65
       .byte $78 ; | XXXX   | $FF66
       .byte $60 ; | XX     | $FF67
       .byte $7E ; | XXXXXX | $FF68

TROUBLE2:
       .byte $8C ; |X   XX  | $FF69
       .byte $99 ; |X  XX  X| $FF6A
       .byte $F1 ; |XXXX   X| $FF6B
       .byte $99 ; |X  XX  X| $FF6C
       .byte $F0 ; |XXXX    | $FF6D
       .byte $00 ; |        | $FF6E
       .byte $66 ; | XX  XX | $FF6F
       .byte $6E ; | XX XXX | $FF70
       .byte $7E ; | XXXXXX | $FF71
       .byte $76 ; | XXX XX | $FF72
       .byte $66 ; | XX  XX | $FF73

TROUBLE3:
       .byte $F1 ; |XXXX   X| $FF74
       .byte $9B ; |X  XX XX| $FF75
       .byte $9B ; |X  XX XX| $FF76
       .byte $9B ; |X  XX XX| $FF77
       .byte $F3 ; |XXXX  XX| $FF78
       .byte $00 ; |        | $FF79
       .byte $3E ; |  XXXXX | $FF7A
       .byte $66 ; | XX  XX | $FF7B
       .byte $6E ; | XX XXX | $FF7C
       .byte $60 ; | XX     | $FF7D
       .byte $3C ; |  XXXX  | $FF7E

TROUBLE4:
       .byte $F7 ; |XXXX XXX| $FF7F
       .byte $36 ; |  XX XX | $FF80
       .byte $37 ; |  XX XXX| $FF81
       .byte $36 ; |  XX XX | $FF82
       .byte $37 ; |  XX XXX| $FF83
       .byte $00 ; |        | $FF84
       .byte $7E ; | XXXXXX | $FF85
       .byte $18 ; |   XX   | $FF86
       .byte $18 ; |   XX   | $FF87
       .byte $18 ; |   XX   | $FF88
       .byte $7E ; | XXXXXX | $FF89

TROUBLE5:
       .byte $9F ; |X  XXXXX| $FF8A
       .byte $D8 ; |XX XX   | $FF8B
       .byte $98 ; |X  XX   | $FF8C
       .byte $D8 ; |XX XX   | $FF8D
       .byte $98 ; |X  XX   | $FF8E
       .byte $00 ; |        | $FF8F
       .byte $66 ; | XX  XX | $FF90
       .byte $6E ; | XX XXX | $FF91
       .byte $7E ; | XXXXXX | $FF92
       .byte $76 ; | XXX XX | $FF93
       .byte $66 ; | XX  XX | $FF94

LFF95:
       .byte $00 ; |        | $FF95
       .byte $09 ; |    X  X| $FF96
       .byte $12 ; |   X  X | $FF97
       .byte $1B ; |   XX XX| $FF98
       .byte $24 ; |  X  X  | $FF99
       .byte $2D ; |  X XX X| $FF9A
       .byte $36 ; |  XX XX | $FF9B

LFF9C:
       .byte $01 ; |       X| $FF9C
       .byte $01 ; |       X| $FF9D
       .byte $02 ; |      X | $FF9E
       .byte $02 ; |      X | $FF9F
       .byte $03 ; |      XX| $FFA0
       .byte $03 ; |      XX| $FFA1
       .byte $03 ; |      XX| $FFA2
       .byte $04 ; |     X  | $FFA3
LFFA4:
       .byte $05 ; |     X X| $FFA4 shared
       .byte $0A ; |    X X | $FFA5
       .byte $0F ; |    XXXX| $FFA6
       .byte $14 ; |   X X  | $FFA7

LFFA8:
       .byte $03 ; |      XX| $FFA8
       .byte $17 ; |   X XXX| $FFA9
       .byte $2B ; |  X X XX| $FFAA
       .byte $23 ; |  X   XX| $FFAB
       .byte $75 ; | XXX X X| $FFAC
       .byte $B4 ; |X XX X  | $FFAD

LFFAE:
       .byte $18 ; |   XX   | $FFAE
       .byte $10 ; |   X    | $FFAF
       .byte $61 ; | XX    X| $FFB0
       .byte $D1 ; |XX X   X| $FFB1
       .byte $32 ; |  XX  X | $FFB2
       .byte $90 ; |X  X    | $FFB3
       .byte $14 ; |   X X  | $FFB4

LFFB5:
       .byte $01 ; |       X| $FFB5
LFFB6:
       .byte $00 ; |        | $FFB6 shared
       .byte $10 ; |   X    | $FFB7
       .byte $20 ; |  X     | $FFB8
       .byte $30 ; |  XX    | $FFB9
       .byte $40 ; | X      | $FFBA
       .byte $50 ; | X X    | $FFBB
       .byte $60 ; | XX     | $FFBC
       .byte $70 ; | XXX    | $FFBD
       .byte $80 ; |X       | $FFBE
       .byte $90 ; |X  X    | $FFBF
       .byte $00 ; |        | $FFC0
LFFC1:
       .byte $10 ; |   X    | $FFC1 shared
       .byte $10 ; |   X    | $FFC2
       .byte $30 ; |  XX    | $FFC3
       .byte $30 ; |  XX    | $FFC4
       .byte $10 ; |   X    | $FFC5
       .byte $10 ; |   X    | $FFC6
       .byte $10 ; |   X    | $FFC7

LFFC8:
       .byte $22 ; |  X   X | $FFC8
       .byte $16 ; |   X XX | $FFC9
       .byte $0C ; |    XX  | $FFCA
       .byte $08 ; |    X   | $FFCB
       .byte $05 ; |     X X| $FFCC
       .byte $04 ; |     X  | $FFCD
       .byte $03 ; |      XX| $FFCE
       .byte $02 ; |      X | $FFCF
LFFD0:
       .byte $00 ; |        | $FFD0 shared
       .byte $08 ; |    X   | $FFD1
       .byte $04 ; |     X  | $FFD2
       .byte $0C ; |    XX  | $FFD3

LFFD4:
       .byte $20 ; |  X     | $FFD4
       .byte $20 ; |  X     | $FFD5
       .byte $15 ; |   X X X| $FFD6
       .byte $15 ; |   X X X| $FFD7
       .byte $10 ; |   X    | $FFD8
       .byte $0B ; |    X XX| $FFD9
       .byte $07 ; |     XXX| $FFDA
       .byte $07 ; |     XXX| $FFDB
       .byte $04 ; |     X  | $FFDC

  IF PLUSROM = 0
IntermissionColorTbl:
;NTSC
       .byte $34 ; |  XX X  | $FFDD
       .byte $56 ; | X X XX | $FFDE
       .byte $98 ; |X  XX   | $FFDF
       .byte $C6 ; |XX   XX | $FFE0
;PAL
       .byte $64 ; | XX  X  | $FFE1
       .byte $86 ; |X    XX | $FFE2
       .byte $B8 ; |X XXX   | $FFE3
       .byte $56 ; | X X XX | $FFE4

TextTbl:
       .byte <SOS1     ; |  XX  X | $FFE5
       .byte <SOS2     ; |  XXX XX| $FFE6
       .byte <SOS3     ; | X   X  | $FFE7
       .byte <SOS4     ; |  XXX XX| $FFE8
       .byte <SOS5     ; |  XX  X | $FFE9
       .byte <SOS6     ; | X  XX X| $FFEA
       .byte <TROUBLE1 ; | X XX   | $FFEB
       .byte <TROUBLE2 ; | XX X  X| $FFEC
       .byte <TROUBLE3 ; | XXX X  | $FFED
       .byte <TROUBLE4 ; | XXXXXXX| $FFEE
       .byte <TROUBLE5 ; |X   X X | $FFEF
       .byte <TROUBLE6 ; | X XXXX | $FFF0

  ELSE
       ORG $2FE2
       RORG $FFE2

  ENDIF
START2:
;filler
       NOP                            ;2
       NOP                            ;2
       NOP                            ;2
;from bank 1...
       NOP                            ;2
       jmp    START                   ;3 boot game

  IF PLUSROM = 1
       ORG $2FF0
       RORG $FFF0

       .byte " HSC 2022 "             ; Keep PlusROM hotspots free
       .word ( PlusROM_API - $c000 )
  ELSE
       ORG $2FF8
       RORG $FFF8

       .byte "2005"
  ENDIF
       .word START2,0
