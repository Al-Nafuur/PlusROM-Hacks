; -----------------------------------------------------------------------------
; CHETIRY - Atari 2600 - (C) Copyright 2011 - AtariAge
; Programming - Chris Walton
; Kernel      - Zach Matley
; Graphics    - Nathun Strum
; -----------------------------------------------------------------------------

  PROCESSOR 6502

  INCLUDE "vcs.h"
  INCLUDE "macro.h"
  INCLUDE "chetiry.h"
  INCLUDE "chetirydata.h"
  
; -------------------------- GAME MACROS --------------------------------------

  ; Play Audio
  MAC PLAY_MUSIC
  IF (MELODY)
  lda #$F2                ; [0] + 2
  sta AUDV0               ; [2] + 3
  ELSE
  lda #0                  ; [0] + 2
  sta AUDV0               ; [2] + 3
  ENDIF
  ENDM

  ; Start Vertical Blank Macro
  MAC START_VBLANK
  lda #%00001110
.SyncLoop
  sta WSYNC               ; [0]
  sta VSYNC               ; [0] + 3
  lsr                     ; [3] + 2
  bne .SyncLoop           ; [5] + 2/3
  lda #VBLANKDELAY        ; [7] + 2
  sta TIM64T              ; [9] + 3
  ENDM
  
  ; Start Vertical Blank Macro (Music Version)
  MAC START_VBLANK_MUSIC
  lda #2                  ; VSYNC enable
  sta WSYNC               ; [0]
  sta VSYNC               ; [0] + 3
  PLAY_MUSIC              ; [3] + 5
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  ldx #VBLANKDELAY        ; [5] + 2
  lda #0                  ; [7] + 2
  sta WSYNC               ; [0]
  sta VSYNC               ; [0] + 3
  PLAY_MUSIC              ; [3] + 5
  stx TIM64T              ; [8] + 3
  ENDM

  ; Start Frame Macro
  MAC START_FRAME
.WaitVblank
  lda INTIM
  bne .WaitVblank
  sta VBLANK
  sta WSYNC               ; [0]
  lda #SCREENDELAY        ; [0] + 2
  sta TIM64T              ; [2] + 3
  ENDM

  ; Start Frame Macro (Music Version)
  MAC START_FRAME_MUSIC
  ldx #SCREENDELAY
.WaitVblank
  PLAY_MUSIC
  lda INTIM
  bne .WaitVblank
  sta VBLANK
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  stx TIM64T              ; [5] + 3
  ENDM

  ; Start Overscan Macro
  MAC START_OVERSCAN
.WaitFrame
  lda INTIM
  bne .WaitFrame
  lda #2
  sta WSYNC               ; [0]
  sta VBLANK              ; [0] + 3
  lda #OVERDELAY          ; [3] + 2
  sta TIM64T              ; [5] + 3
  ENDM

  ; Start Overscan Macro (Music Version)
  MAC START_OVERSCAN_MUSIC
  ldx #OVERDELAY
.WaitFrame
  PLAY_MUSIC
  lda INTIM
  bne .WaitFrame
  lda #2
  sta WSYNC               ; [0]
  sta VBLANK              ; [0] + 3
  PLAY_MUSIC              ; [3] + 5
  stx TIM64T              ; [8] + 3
  ENDM

  ; Wait For Overscan End Macro
  MAC WAIT_OVERSCAN
.WaitOverscan
  lda INTIM
  bne .WaitOverscan
  ENDM
  
  ; Wait For Overscan End Macro (Music Version)
  MAC WAIT_OVERSCAN_MUSIC
.WaitOverscan
  PLAY_MUSIC
  lda INTIM
  bne .WaitOverscan
  ENDM

  ; Skip Lines Macro
  MAC SKIP_LINES
.LINES  SET {1}
  ldx #.LINES
.SkipLoop
  sta WSYNC
  dex
  bpl .SkipLoop
  ENDM

; -------------------------- GAME CONSTANTS -----------------------------------

; Game Constants
MELODY                    = 0
PLUSROM                   = 1

NO_ILLEGAL_OPCODES        = 0           ; Allow Undocumented Opcodes
FAKESPIN                  = 0           ; Fake Spinner (1 = Yes)
PALMODE                   = 0           ; PAL/NTSC (0 = NTSC)
RANDOMSEED                = 15          ; Initial Random Number
RAMSPINNER                = $B0         ; Stall Code Address

; Graphics Constants
LOGOH                     = 18          ; Menu Logo Height
STARH                     = 9           ; Menu Star Height
AAHEIGHT                  = 24          ; Startup Logo Height
MAXSCORES                 = 10          ; Scores In Table
STARTFRAMES               = 127         ; Ship Frame Delay

; Time Constants
  IF (PALMODE)
VBLANKDELAY               = 62          ; PAL Frame Timings
SCREENDELAY               = 255
OVERDELAY                 = 50
CYCLETIME                 = 50          ; Cycles Per Second
HDELAY                    = 5           ; Horizontal Movement Delay (max 7)
VDELAY                    = (3 << 3)    ; Vertical Movement Delay (max 3)
KEYWAIT                   = 16          ; Default Debounce Delay
KEYSHORT                  = 8           ; Short Debounce Delay
FLASHTIME                 = 26          ; Flash Timer (max 31)
GAMEOVERTIME              = 26          ; Gameover Timer (max 31)
MUSICTEMPO                = 4           ; Default Music Tempo
TITLETEMPO                = 5           ; Default Tempo (Title Screen)
SLOWTEMPO                 = 7           ; Slow Tempo (Title Screen)
  ELSE
VBLANKDELAY               = 44          ; NTSC Frame Timings
SCREENDELAY               = 246
OVERDELAY                 = 18
CYCLETIME                 = 60          ; Cycles Per Second
HDELAY                    = 6           ; Horizontal Movement Delay (max 7)
VDELAY                    = (3 << 3)    ; Vertical Movement Delay (max 3)
KEYWAIT                   = 20          ; Default Debounce Delay
KEYSHORT                  = 10          ; Short Debounce Delay
FLASHTIME                 = 31          ; Flash Timer (max 31)
GAMEOVERTIME              = 31          ; Gameover Timer (max 31)
MUSICTEMPO                = 5           ; Default Music Tempo
TITLETEMPO                = 6           ; Default Tempo (Title Screen)
SLOWTEMPO                 = 8           ; Slow Tempo (Title Screen)
  ENDIF
RETRYOP                   = 5           ; Retry eeprom operations

; Menu Colour Constants
  IF (PALMODE)            ; PAL
RED                       =  $64
YELLOW                    =  $26
ORANGE                    =  $46
BLUE                      =  $D6
WHITE                     =  $0E
  ELSE                    ; NTSC
RED                       =  $44
YELLOW                    =  $1A
ORANGE                    =  $36
BLUE                      =  $86
WHITE                     =  $0E
  ENDIF

; PlusROM hotspots and gameId for HSC backend
   IF PLUSROM
WriteToBuffer           = $1FF0
WriteSendBuffer         = $1FF1
ReceiveBuffer           = $1FF2
ReceiveBufferSize       = $1FF3

HIGHSCORE_ID            = 47        ; Chetiry game ID in Highscore DB
   ENDIF


  SEG.U   VARS
  ORG     $80

; -------------------------- GLOBAL VARIABLES ---------------------------------

INITIALS  DS.B 2          ; Initials (packed), 7 = 2600(1)/7800(0) Console
LEVEL     DS.B 1          ; 0-4 = Current Level, 5-7 = Height
SCORE     DS.B 3          ; Current Score (6-Digit BCD)

CYCLE     DS.B 1          ; 0-5 = Game Cycle, 6 = B/W Switch, 7 = Pause
RANDOM    DS.B 1          ; Random Seed (Replace With DPC RNG)

MUSIC     DS.B 1          ; 0-4 = Tempo, 5-7 = Tune Type
SFX       DS.B 1          ; 0-4 = Sound Position, 5-7 = Sound Effect

DEBOUNCE  DS.B 1          ; 0-2 = L/R Delay, 3-4 = D Delay, 5 = Fire, 6-7 = L/R
TEMP      DS.B 1          ; All-purpose Temp
TEMP2     DS.B 1          ; Colour 0

; -------------------------- GAME VARIABLES -----------------------------------

LINES     DS.B 2          ; Line Count/Timer (3 Digit BCD)
                          ; 4-5 = Game (00 = Marathon, 01 = Sprint25,
                          ;             10 = Sprint40, 11 = Ultra)
                          ; 6-7 = Mode (00 = Normal, 01 = Settle,
                          ;             10 = Flash, 11 = Game Over)
XPOS      DS.B 1          ; 0-3 = XPos, 4-6 = Type, 7 = Joystick Up
YPOS      DS.B 1          ; 0-4 = YPos, 5-6 = Rot, 7 = Hard Drop
TIMER     DS.B 1          ; 0-4 = Drop Timer, 5-7 = Next Piece (& GameOver Data)

; Chetiry Grid (10x18): (8T)10|(8)32|(9T)54|(9)76
GRID      DS.B 72

; Display Remaining RAM
  echo "----",($DA - *) , "bytes left (RAM)"

; -------------------------- TEMPORARY VARIABLES ------------------------------

; Stall Code
RESUME    EQU TEMP        ; 2

; Self Modifying Kernel
KERNEL    EQU $DA         ; Chetiry Kernel (37 bytes)
Modify    EQU KERNEL+0    ; Entry Point
Col1      EQU KERNEL+1    ; Colour Values
Col2      EQU KERNEL+5
Col3      EQU KERNEL+9
Col4      EQU KERNEL+13
Col6      EQU KERNEL+20
Col7      EQU KERNEL+24
Col8      EQU KERNEL+28
JPtr      EQU KERNEL+35   ; Return Address

; Score Kernel
SBUFF     EQU $DA         ; 30 bytes (6*5)
PFBUFF0   EQU $F8
PFBUFF1   EQU $F9

; Game Text Kernel
GPTR0     EQU $DA
GPTR1     EQU $DC
GPTR2     EQU $DE
GPTR3     EQU $E0
GPTR4     EQU $E2
GPTR5     EQU $E4
GLINE     EQU $E6

; Temp Game Values
TEMPX     EQU $DA
TEMPX2    EQU $DB
TEMPY     EQU $DC
TEMPROT   EQU $DD
TEMPROT2  EQU $DE
TEMPVAL   EQU $DF
VALUE1    EQU $E0
VALUE2    EQU $E1
VALUE3    EQU $E2
VALUE4    EQU $E3
SPTR      EQU $E4
SPTR2     EQU $E6

; -------------------------- SHUTTLE VARIABLES --------------------------------

; Animation
SHIP      EQU GRID+0      ; Ship Type (0 = Shuttle, 1 = Soyuz, 2 = Rocket)
HEIGHT    EQU GRID+1      ; Ship Height
PHASE     EQU GRID+2      ; Animation Phase (4->0)
FRAME     EQU GRID+3      ; Phase Frame

; Lines
SLINE     EQU GRID+4      ; 1
FLINE     EQU GRID+5      ; 1
TLINE     EQU GRID+6      ; 1

; Gaps
GAPTOP    EQU GRID+7      ; 1
GAPBOT    EQU GRID+8      ; 1
GPF       EQU GRID+9      ; 2

; Ship Pointers
SDATA1    EQU GRID+12     ; 2
SDATA2    EQU GRID+14     ; 2
SDATA3    EQU GRID+16     ; 2
SDATA4    EQU GRID+18     ; 2
SPF       EQU GRID+20     ; 2

; Flame Pointers
FDATA1    EQU GRID+22     ; 2
FDATA2    EQU GRID+24     ; 2
FDATA3    EQU GRID+26     ; 2
FDATA4    EQU GRID+28     ; 2
FPF       EQU GRID+30     ; 2

; TakeOff Pointers
TDATA0    EQU GRID+32     ; 2
TDATA1    EQU GRID+34     ; 2
TDATA2    EQU GRID+36     ; 2
TDATA3    EQU GRID+38     ; 2
TDATA4    EQU GRID+40     ; 2
TDATA5    EQU GRID+42     ; 2

; -------------------------- MENU VARIABLES -----------------------------------

STACK     EQU TEMP2       ; Stack Storage
MBUFF     EQU GRID+0      ; Menu Text Buffer - 42 Bytes (7*6)

TCOL      EQU GRID+42     ; Default Text Colour
TCOL0     EQU GRID+43     ; Text Colours
TCOL1     EQU GRID+44
TCOL2     EQU GRID+45
TCOL3     EQU GRID+46
HCOL      EQU GRID+47     ; Highlighted Text Colour
FCOL      EQU GRID+48     ; Flashing Colour

STATE     EQU GRID+49     ; Menu State
MSG       EQU GRID+50     ; Message Number
LINE      EQU GRID+51     ; Menu Line
STARPTR   EQU GRID+52     ; Star Colour Pointer (2)
STAROFF   EQU GRID+54     ; Star Offset
LOGOCOL   EQU GRID+55     ; Logo Colour

TMPGAME   EQU GRID+56     ; Game Type
TMPMUSIC  EQU GRID+57     ; Music Type
TMPLEVEL  EQU GRID+58     ; Starting Level
TMPHEIGHT EQU GRID+59     ; Starting Height

ROW       EQU GRID+60     ; Row
DPTR      EQU GRID+61     ; Drums (2)

; -------------------------- HISCORE VARIABLES --------------------------------

; Bankswitch Variables
  IF (MELODY)
HI_WRITE  EQU $1004
HI_READ   EQU $1044
  ELSE
HI_WRITE  EQU $1004
HI_READ   EQU $1084
  ENDIF

; Counters
POSITION  EQU XPOS        ; HiScore Position
TABLE     EQU YPOS        ; HiScore Table To Display
SLOWCYCLE EQU LINES+0     ; Slow Cycle Timer
TEMPO     EQU LINES+1     ; Music Tempo
TIMEOUT   EQU TIMER       ; Operation Timeout

; HiScore Table Variables
HBUFF     EQU GRID        ; HiScore Table (35)
HTEXT0    EQU HBUFF+0     ; HiScore Text
HTEXT1    EQU HBUFF+1
HTEXT2    EQU HBUFF+5
HTEXT3    EQU HBUFF+6
HTEXT4    EQU HBUFF+10
HTEXT5    EQU HBUFF+11
HTEXT6    EQU HBUFF+15
HTEXT7    EQU HBUFF+16
HTEXT8    EQU HBUFF+20
HTEXT9    EQU HBUFF+21
HTEXT10   EQU HBUFF+25
HTEXT11   EQU HBUFF+26
HTEXT12   EQU HBUFF+30
HTEXT13   EQU HBUFF+31

; Name Entry Variables
HPTR0     EQU HBUFF+0     ; 2
HPTR1     EQU HBUFF+2     ; 2
HPTR2     EQU HBUFF+4     ; 2
HPTR3     EQU HBUFF+6     ; 2
HPTR4     EQU HBUFF+8     ; 2
HPTR5     EQU HBUFF+10    ; 2
NAME      EQU HBUFF+31    ; 3
CURSOR    EQU HBUFF+34    ; 1
HSCORE    EQU HBUFF+35    ; 3
NAMECOL   EQU HBUFF+38    ; 3

; -----------------------------------------------------------------------------
; BANK 0 - Custom Bankswitching (overwritten by makeimg)
; -----------------------------------------------------------------------------

  SEG     BANK0
  ORG     $8000
  RORG    $F000

  ; Space for DPC registers
  DS.B    256,0

Init0
  ; Switch to Bank 7 (required to execute game on Stella)
  nop     $FFFB
  
  echo "----",($FFF4 - *) , "bytes left (BANK 0 - BANKSWITCHING)"
  
  ORG     $8FF4
  RORG    $FFF4
  DC.B    "BANK0", 0
  DC.W    (PlusROM_API - $D000)
  DC.W    Init0, Init0

; -----------------------------------------------------------------------------
; BANKS 1-7
; -----------------------------------------------------------------------------

  INCLUDE bank1.asm
  INCLUDE bank2.asm
  INCLUDE bank3.asm
  INCLUDE bank4.asm
  INCLUDE bank5.asm
  INCLUDE bank6.asm
  INCLUDE bank7.asm

; -----------------------------------------------------------------------------

