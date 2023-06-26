; -----------------------------------------------------------------------------
; Juno First v1.0 (FINAL VERSION) - Atari 2600
; Copyright (C) Chris Walton 2008 <cwalton@gmail.com>
; -----------------------------------------------------------------------------

  PROCESSOR 6502
  
  ; Game Includes
  INCLUDE "vcs.h"
  INCLUDE "macro.h"
  INCLUDE "waves.h"         ; Wave Data
  INCLUDE "junodata.h"      ; Game Data

  ; Flickersort Macro (Compare two alien positions and swap if they overlap)
  MAC FLICKERSORT
.POS      SET {1}
  ; Check Scanner Sprites
  lda INVY+.POS             ; [0] + 3
  bpl .Compare              ; [3] + 2
  and #%11110000            ; [5] + 2
.Compare             
  ; Check If Sprites Should Swap
  cmp SpacingTable,Y        ; [7] + 4
  bcs .Swap                 ; [11] + 3
  ldy TEMP
  stx INVS,Y
  stx TEMP
  jmp .Finish
.Swap
  lda INVT+.POS             ; [14] + 3
  ldy INVT,X                ; [17] + 4
  sta INVT,X                ; [21] + 4
  sty INVT+.POS             ; [25] + 3
  lda INVX+.POS             ; [28] + 3
  ldy INVX,X                ; [31] + 4
  sta INVX,X                ; [35] + 4
  sty INVX+.POS             ; [39] + 3
  lda INVY+.POS             ; [42] + 3
  ldy INVY,X                ; [45] + 4
  sta INVY,X                ; [49] + 4
  sty INVY+.POS             ; [53] + 3
.Finish                     ; WORST CASE = 56 CYCLES
  ENDM

  ; Alien Movement Macro
  MAC MOVEALIEN
.ALIEN    SET {1}
  lax INVY+.ALIEN           ; [0] + 3
  ldy Movement,X            ; [3] + 4
  clc                       ; [7] + 2
  adc DVDTAB,Y              ; [9] + 4
  sta INVY+.ALIEN           ; [13] + 3
  ENDM                      ; RUNTIME = 16 CYCLES

  ; Bullet Movement Macro
  MAC MOVEBULLET
.BULLET    SET {1}
  lax B1Y+.BULLET           ; [0] + 3
  ldy BulletMovement,X      ; [3] + 4
  clc                       ; [7] + 2
  adc DVDTAB,Y              ; [9] + 4
  sta B1Y+.BULLET           ; [13] + 3 
  ENDM                      ; RUNTIME = 16 CYCLES

  ; Clear Hiscore Table
  MAC CLEAR_HISCORES
  lda #0                    ; [0] + 2
  ldx #59                   ; [2] + 2 = 4
.WipeScoresLoop
  sta HISCORES,X            ; [0] + 4
  dex                       ; [4] + 2
  bpl .WipeScoresLoop       ; [6] + 2/3
  ENDM                      ; RUNTIME = (9*59) + 8 + 4 = 543 (7.1 SCANLINES)

  ; Clear Non-Main Game Variables Macro
  MAC CLEAR_GAMEVARS
  lda #0                    ; [0] + 2
  ldx #<TIME                ; [2] + 2 = 4
.ClearVarLoop
  sta $00,X                 ; [0] + 4
  inx                       ; [4] + 2
  bne .ClearVarLoop         ; [6] + 2/3
  ENDM                      ; RUNTIME = (9*116) + 8 + 4 = 1056 (14 SCANLINES)

  ; Start Vertical Blank Macro (by Manuel Rotschkar)
  MAC START_VBLANK
  lda #2                    ; VSYNC enable
  sta WSYNC
  sta VSYNC
  sta WSYNC
  sta WSYNC
  lsr                       ; VSYNC disable
  sta WSYNC
  sta VSYNC
  ; Set VBlank Timer
  lda #VBLANKDELAY
  sta TIM64T
  ENDM

  ; Wait for Vertical Blank End Macro
  MAC WAIT_VBLANK
.WaitVblank
  lda INTIM
  bne .WaitVblank
  sta VBLANK
  sta WSYNC
  ENDM

  ; Start Screen Macro
  MAC START_SCREEN
  lda #SCREENDELAY
  sta TIM64T
  ENDM

  ; Wait for Screen End Macro
  MAC WAIT_SCREEN
.WaitFrame
  lda INTIM
  bne .WaitFrame
  ENDM
  
  ; Start Overscan Macro
  MAC START_OVERSCAN
  lda #2
  sta WSYNC
  sta VBLANK
  lda #OVERDELAY
  sta TIM64T
  ENDM

  ; Wait for Overscan End Macro
  MAC WAIT_OVERSCAN
.WaitOverscan
  lda INTIM
  bne .WaitOverscan
  ENDM
  
  ; Skip Scanlines Macro
  MAC SKIP_LINES
.LINES  SET {1}
  ldx #.LINES
.SkipLoop
  sta WSYNC
  dex 
  bpl .SkipLoop
  ENDM

; Game Constants
NO_ILLEGAL_OPCODES = 0      ; Allow Undocumented Instructions
PALCOLS     = 0             ; PAL/NTSC Colours (0 = NTSC)

; Time Constants
VBLANKDELAY = 44            ; 60Hz - (37*76)/64 ~ 44
SCREENDELAY = 229           ; 60Hz - (192*76)/64 ~ 229
OVERDELAY   = 35            ; 60Hz - (30*76)/64 ~ 35
KEYWAIT     = 20            ; Default Debounce Delay
KEYSHORT    = 10            ; Short Debounce Delay
MAXTIME     = 41            ; Wave Time (42*(256/180)) seconds (~60s) (<=63)
LOWTIME     = 6             ; Low Time Warning
WARPTIME    = 14            ; Warp Time (<= 31)
WARPLOW     = 4             ; Low Time Threshold
HYPERTIME   = 31            ; Hyperspace Time (<= 31)
HYPERLOW    = 12            ; Low Hyperspace Threshold
DEADTIME    = 3             ; Death Delay

; Kernel Constants
SCANH       = 7             ; Scanner Kernel Height/3
GRIDH       = 126           ; Grid Kernel Height
SHIPH       = 40            ; Ship Kernel Height
LOGOH       = 41            ; Title Logo Height (42 Pixels)
TEXTH       = 5             ; Text Height (6 Pixels)
XMAX        = 147           ; Maximum Sprite X Position
XMIN        = 4             ; Minimum Sprite X Position
XMIDDLE     = (XMAX-XMIN)/2 ; Middle Sprite Position

; Player Constants
MAXSPEED    = 80            ; Maximum Ship Speed
MINSPEED    = 254-MAXSPEED  ; Minimum Ship Speed
SPEEDINC    = 2             ; Speed Change
MAXLIVES    = 7             ; Maximum Lives

; Laser & Bullet Constants
LASERSLOW   = 4             ; Laser Beam Movement Speed
LASERFAST   = 2             ; Laser Beam Extra Movement
MAXSHOTS    = 5             ; Laser Beam Maximum Shots (MAX = 7)
BMAX        = 4             ; Maximum Bullets of each type
BMAX2       = BMAX+BMAX     ; Total Maximum Bullets
BXMAX       = XMAX/2        ; Maximum X Coordinate
    
; Alien Constants
INVMAX      = 16            ; Maximum Number of Aliens
INVYMIN     = 100           ; Minimum Y Position For Spawning (<= 135)

; Highscore Constants
HISCOREID   = 48            ; Juno First game ID in PlusROM high score DB
ATARIH      = 24            ; Startup Logo Height
MAXSCORES   = 10            ; Scores In Table
BANKHIGH    = $03           ; Highscore Static Allocation Area (Hi)
BANKLOW     = $40           ; Highscore Static Allocation Area (Lo)
VOXTIME     = $FF           ; AtariVox Timeout (~4s)

; Game Variables (V = Volatile, VV = Very Volatile!)
  SEG.U VARS
  ORG $80

; Global Variables
CYCLE       DS.B 1          ; Game Cycle
RANDOM      DS.B 1          ; Random Seed
COUNTER     DS.B 1          ; Game Counter (0-4), Shot Counter (5-7)

; Player Variables
SCORE       DS.B 3          ; Current Score (6-Digit BCD)
HISCORE     DS.B 4          ; High Score (6-Digit BCD + Wave)
WAVE        DS.B 1          ; Wave (0-5), Paused (7)
LIVES       DS.B 1          ; Lives (0-2), Practise (3), Anim (4-6), Dead (7)
TIME        DS.B 1          ; Timer/Fuel (0-5), Astronaut (6), Hyperspace (7)
MESSAGE     DS.B 1          ; Message (0-2), Counter (3-7)
ACCUM       DS.B 1          ; Score Accumulator (During Hyperspace)
SND0        DS.B 1          ; Sound Effect Ch0 (Counter=0-4, Sound=5-7)
SPEECH      DS.B 1          ; Speech Effect (Counter=0-4, Speech=5-7)

; Temporary Variables
TEMP        DS.B 1          ; Temporary Variable (VV)
TEMP2       DS.B 1
TEMP3       DS.B 1
TEMP4       DS.B 1

; Laser Beam Variables
LASERX      DS.B 1          ; X Position 
LASERY      DS.B 1          ; Y Position (Start)
LASERS      EQU TEMP2       ; Laser Size (V)

; Ship Variables
PLAYERX     DS.B 1          ; Ship Position
SPEED       DS.B 1          ; Ship Speed
SPTR        DS.B 2          ; Ship Data Pointer (V)
CPTR        DS.B 2          ; Ship Colour Pointer (V)
JPTR        DS.B 2          ; Kernel Jump Pointer + Sound Pointer (V)
    
; Alien Invader Variables
TYPE        DS.B 1          ; Current Alien Type (V)
ALIENY      DS.B 1          ; Current Alien Position (V)
INVT        DS.B INVMAX     ; Score (0-1), Invader Type (2-5), Movement (6-7)
INVX        DS.B INVMAX     ; Invader X Coordinates
INVY        DS.B INVMAX     ; Invader Y Coordinates
INVS        DS.B INVMAX     ; Invader Ordering (V)
INDEX       DS.B 1          ; Current Alien Index (V)
SPAWN       DS.B 1          ; Spawn Counter
KILLS       DS.B 1          ; Kills Counter
MOTION      DS.B 1          ; Motion Counter
STIME       DS.B 1          ; Spawn Timer

; Bullets (B1 = Normal, B2 = Homing)
B1          EQU TEMP3       ; Bullet 1 Temp Variable (V)
B2          EQU TEMP4       ; Bullet 2 Temp Variable (V)
B1X         DS.B BMAX       ; X Coordinates (0-6=Pos, 7=Direction)
B2X         DS.B BMAX
B1Y         DS.B BMAX       ; Y Coordinates (0-6=Pos, 7=Visible)
B2Y         DS.B BMAX

; Grid Variables
START       DS.B 1          ; Line Start
LINE        DS.B 1          ; Next Line Position (V)
DVD         DS.B 1          ; Line Divide Pointer (V)
DVDTAB      DS.B 7          ; Line Divide Table (V)
SKYCOL      EQU LINE        ; Sky Colour

; Title Screen Variables
DEBOUNCE    EQU COUNTER     ; Key Debouncing
STARTWAVE   EQU LIVES       ; Starting Wave Number
FASTCYCLE   EQU TIME        ; CYCLE / 4
SLOWCYCLE   EQU MESSAGE     ; CYCLE / 16
NAMECACHE   EQU SND0        ; Name Pack/Unpack Store (2 bytes)
ROW         EQU TEMP2
POSITION    EQU TEMP3
STACK       EQU TEMP3
ERROR       EQU TEMP3       ; Type (7), Action (0)

; Music Variables
MEASURE     EQU LASERX
BEAT        EQU LASERY
TEMPOCOUNT  EQU PLAYERX
CHAN        EQU TEMP
TEMP16L     EQU TEMP2
TEMP16H     EQU TEMP3
FREQ        EQU TEMP4
ATTEN       EQU SPEED

; Text Buffer
BUFF        EQU SPTR        ; 35 bytes

; High Score Table
HISCORES    EQU SPTR+35     ; 60 bytes

; Startup Logo
LOGOBUFF    EQU HISCORES    ; 42 bytes

; Text Pointers
TEXT0       EQU BUFF+0
TEXT1       EQU BUFF+1
TEXT2       EQU BUFF+5
TEXT3       EQU BUFF+6
TEXT4       EQU BUFF+10
TEXT5       EQU BUFF+11
TEXT6       EQU BUFF+15
TEXT7       EQU BUFF+16
TEXT8       EQU BUFF+20
TEXT9       EQU BUFF+21
TEXT10      EQU BUFF+25
TEXT11      EQU BUFF+26
TEXT12      EQU BUFF+30
TEXT13      EQU BUFF+31

; Misc Pointers
PTR0        EQU BUFF+0      ; 2
PTR1        EQU BUFF+2      ; 2
PTR2        EQU BUFF+4      ; 2
PTR3        EQU BUFF+6      ; 2
PTR4        EQU BUFF+8      ; 2
PTR5        EQU BUFF+10     ; 2
PTRC        EQU BUFF+12     ; 2
NAME        EQU BUFF+31     ; 3
CURSOR      EQU BUFF+34     ; 1

; Logo Colours
COL0        EQU PTRC        ; Logo Light Colour
COL1        EQU PTRC+1      ; Logo Dark Colour

  ; Display Remaining RAM
  echo "----",($100 - *) , "bytes left (RAM)"

; -----------------------------------------------------------------------------
; PlusROM Extensions
; -----------------------------------------------------------------------------

  ; PlusROM Base Address
  IFNCONST PLUSROM_BASE_ADDRESS
PLUSROM_BASE_ADDRESS = $1FF0
  ENDIF

  ; PlusROM Registers
  SEG.U PLUSROM_REGISTERS
  ORG PLUSROM_BASE_ADDRESS

  ; PlusROM Registers
WRITETOBUFF         DS 1    ; $1FF0
WRITESENDBUFF       DS 1    ; $1FF1
RECEIVEBUFF         DS 1    ; $1FF2
RECEIVEBUFFSIZE     DS 1    ; $1FF3

  SEG

; -----------------------------------------------------------------------------
; BANKS 1-8
; -----------------------------------------------------------------------------

  INCLUDE bank1.asm         ; Main Game
  INCLUDE bank2.asm         ; Kernel 1
  INCLUDE bank3.asm         ; Kernel 2
  INCLUDE bank4.asm         ; Lower Kernel
  INCLUDE bank5.asm         ; Alien Spawning
  INCLUDE bank6.asm         ; Savekey Handler
  INCLUDE bank7.asm         ; Music
  INCLUDE bank8.asm         ; Title Screen

; -----------------------------------------------------------------------------


