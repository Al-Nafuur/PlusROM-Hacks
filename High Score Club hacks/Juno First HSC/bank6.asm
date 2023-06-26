; -----------------------------------------------------------------------------
; BANK 6 - HIGH SCORE HANDLER
; -----------------------------------------------------------------------------

; -- Table Format --
;
; The hiscore table occupies 64 bytes and is arranged as follows:
;
; Signature         - 2 bytes
; Previous Initials - 2 bytes
; Hiscore table     - 60 bytes
; TOTAL             = 64 bytes
;
; The game scores are stored from highest to lowest as follows:
;
; Highest Score (0) - 6 bytes
; ...
; Lowest Score (9)  - 6 bytes
; TOTAL             = 6 * 10 = 60 bytes
;
; Each score is arranged as follows:
; Score             - bytes 0-2
; Initials          - bytes 3-4
; Wave              - byte 5
; TOTAL             = 6 bytes

  SEG     BANK6
  ORG     $D000
  RORG    $F000

Init6
  nop     $FFFB             ; Switch to Bank 8
  jmp StartupSequence
  jmp EndOfGame
  jmp HighScoreTable
  nop
  nop
  nop
EndHighScoreTable
  nop     $FFFB             ; Switch To Bank 8
  jmp HighScoreTable        ; Show Highscore Table
EndHighScores
  nop     $FFFB             ; Switch To Bank 8

; -----------------------------------------------------------------------------
; PART 0 - HISCORE UPDATE
; -----------------------------------------------------------------------------

EndOfGame
  ; Clear Sound Effects
  lda #0
  sta AUDV0
  sta AUDV1
  
  ; Reset Stack Pointer
  ldx #$FF
  txs

  ; Wait for Fire Button Release
  jsr JoyFireRelease

  ; Clear Game Variables
  jsr StartScreen
  CLEAR_GAMEVARS
  jsr NextFrame

  ; Clear Wave Bits
  lda WAVE
  and #%00111111
  sta WAVE
  
  ; Reset/Continue According to Right Difficulty Switch
  lda #0
  bit SWCHB
  bmi SkipContinue
  lda WAVE
SkipContinue
  sta STARTWAVE

  ; Increment Wave Number
  inc WAVE

  ; Write Scores
  jsr WriteScores

  ; Wait ~20 frames for highscore request to be sent
  ldy #20
SendWaitLoop
  jsr StartScreen
  jsr NextFrame
  dey
  bne SendWaitLoop

  ; Read new Scores
  jsr ReadScores


FinishHiScores
  ; Reset Music Variables
  lda #0
  sta MEASURE
  sta BEAT
  sta TEMPOCOUNT
  ; Show Score Table
  lda #127
  sta FASTCYCLE
  ; Debounce Fire Button
  lda #KEYWAIT
  sta DEBOUNCE
  ; Resume Title Screen
  jmp EndHighScores


; -----------------------------------------------------------------------------
; PART 1 - DISPLAY HIGH SCORE TABLE
; -----------------------------------------------------------------------------

HighScoreTable
  ; Set Heading Text Colours
  IF (PALCOLS)
  lda #$0F
  ELSE
  lda #$0F
  ENDIF
  sta COLUP1
  sta COLUP0
  
  ; Load Heading Text
  jsr PositionText
  ldy #(10*12)
  jsr LoadText

  ; Start Screen
  jsr StartScreen
  
  ; Show Heading
  jsr NormalTextKernel

  ; Adjust Sprite Positions
  jsr PositionHighScore

  ; Set Lines For Score Table
  lda #MAXSCORES-1
  sta ROW

  ; Show Score Table
ScoreTableLoop
  ; Set Score Colour
  lax ROW
  clc
  adc FASTCYCLE
  and #%00000111
  tay
  lda ScoreColTab,Y
  sta COLUP0
  sta COLUP1
  ; Load and Display Score
  jsr LoadScore
  jsr HighScoreKernel
  dec ROW
  bpl ScoreTableLoop
EndScoreTableLoop

  ; Gap Between Scores
  sta WSYNC
  sta WSYNC

  ; Set Colour for Last Score
  lda CYCLE
  lsr
  lsr
  and #%00000111
  tax
  eor #%00000011
  sta TEMP
  lda ScoreColTab2,X
  ora ScoreFastFade,X
  sta COLUP0
  sta COLUP1

  ; Display Last Score
  jsr LoadLast
  jsr HighScoreKernel
  
  ; Set Colour for Best Score
  ldx TEMP
  lda ScoreColTab2,X
  ora ScoreFastFade,X
  sta COLUP0
  sta COLUP1

  ; Display Best Score
  jsr LoadBest
  jsr HighScoreKernel
  jmp EndHighScoreTable

; -----------------------------------------------------------------------------
; PART 2 - READ HIGHSCORE TABLE
; -----------------------------------------------------------------------------

ReadScores
  ; Ensure there are PlusROM functions available on this device
  lda RECEIVEBUFFSIZE
  cmp #255
  bne PlusROMfunctionsAvailable
  jmp HandleReadError
PlusROMfunctionsAvailable
  
  ; Select Table 0
  lda #0
  sta WRITETOBUFF
  lda #HISCOREID            ; Game ID for Highscore Database
  sta WRITESENDBUFF

WaitForDataToRead
  ; Start Screen
  jsr StartScreen

  ; Ensure there is data to read
  lda RECEIVEBUFFSIZE
  cmp #60
  bmi WaitForDataToRead

  ; Read Stored Initials
;  lda RECEIVEBUFF
;  sta NAMECACHE+0
;  lda RECEIVEBUFF
;  sta NAMECACHE+1

  ; Unpack Stored Initials
;  jsr UnpackName

  ; Skip To Next Frame
;  jsr NextFrame

  ; Read Entries
  ldx #MAXSCORES-1
ReadEntryLoop
  ; Read During Screen
  jsr StartScreen

  ; Set Score Pointer
  lda #>HISCORES
  sta PTR0+1
  lda ScoreOffsetPtrs,X
  sta PTR0
  
  ; Read 6 Bytes (Score, Name, Wave)
  ldy #5
  sty ROW
ReadBytesLoop
  lda RECEIVEBUFF
  ldy ROW
  sta (PTR0),Y
  dec ROW
  bpl ReadBytesLoop
  
  ; Check Score Validity
  ldy #2
CheckBCDLoop
  lda (PTR0),Y
  and #%00001111
  cmp #10
  bcs HandleReadError
  lda (PTR0),Y
  and #%11110000
  cmp #10<<4
  bcs HandleReadError
  dey
  bpl CheckBCDLoop
  
  ; Skip To Next Frame
  jsr NextFrame
  
  ; Check If Highscore Table Is Finished
  dex
  bpl ReadEntryLoop
  
  ; Get Last Score from Response
  lda RECEIVEBUFF
  sta SCORE+2
  lda RECEIVEBUFF
  sta SCORE+1
  lda RECEIVEBUFF
  sta SCORE
  lda RECEIVEBUFF
  sta WAVE

  ; Copy First table entry to HighScore ?
  lda HISCORES+5
  sta HISCORE+3
  lda HISCORES+2
  sta HISCORE+2
  lda HISCORES+1
  sta HISCORE+1
  lda HISCORES+0
  sta HISCORE+0

  ; Start Screen
  jsr StartScreen

FinishReadScores
  ; Skip To Next Frame
  jsr NextFrame

  ; Keep reading until buffer is empty
FlushBufferLoop
  lda RECEIVEBUFFSIZE
  beq FinishedRead
  lda RECEIVEBUFF
  jmp FlushBufferLoop

FinishedRead
  rts

HandleReadError
  ; Skip To Next Frame
  jsr NextFrame
  
  ; Wipe Score Table (as entries may be corrupt)
  CLEAR_HISCORES
  rts

; -----------------------------------------------------------------------------
; PART 3 - SEND SCORE TO PLUSROM HSC
; -----------------------------------------------------------------------------

WriteScores
  ; Start Screen
  jsr StartScreen

  ; Write Scores
  lda SCORE+2
  sta WRITETOBUFF
  lda SCORE+1
  sta WRITETOBUFF
  lda SCORE
  sta WRITETOBUFF
  lda WAVE
  sta WRITETOBUFF
  lda #HISCOREID            ; Game ID for Highscore Database
  sta WRITESENDBUFF
  rts

; -----------------------------------------------------------------------------
; PART 5 - CLEAR HIGHSCORE TABLE
; -----------------------------------------------------------------------------

WipeHighScores
  ; Wait for Fire Button Release
  jsr JoyFireRelease

  ; Initialise Prompt
  lda #0
  sta ERROR
  
  ; Clear Score Table
  jsr StartScreen
  CLEAR_HISCORES
  jsr NextFrame

HighScoreWipeLoop
  ; Check Joystick
  lda DEBOUNCE
  beq CheckWipeJoy
  dec DEBOUNCE
  jmp EndCheckWipeJoy
CheckWipeJoy
  ; Change Selection If Joystick Moves Up or Down
  lda SWCHA
  and #%00110000
  eor #%00110000
  beq CheckWipeFire
  ; Invert Selection
  lda ERROR
  eor #%11111111
  sta ERROR
  ; Set Debounce
  lda #KEYWAIT
  sta DEBOUNCE
  jmp EndCheckWipeJoy
CheckWipeFire
  ; Check If Fire Pressed
  lda INPT4
  bmi EndCheckWipeJoy
  ; Set Debounce
  lda #KEYWAIT
  sta DEBOUNCE
  ; Check Result
  lda ERROR
  beq NoWipeScores
  jmp WriteScores
NoWipeScores
  rts
EndCheckWipeJoy

  ; Reflect PF and Set PF Colour
  lda #%00000001
  sta CTRLPF
  IF (PALCOLS)
  lda #$60
  ELSE
  lda #$40
  ENDIF
  sta COLUPF

  ; Set First Message Colour
  IF (PALCOLS)
  lda #$0F
  ELSE
  lda #$0F
  ENDIF
  sta COLUP1
  sta COLUP0

  ; Position Sprites and Load First Message
  jsr PositionText
  ldy #(11*12)
  jsr LoadText

  ; Start Screen and Show First Message
  jsr StartScreen
  SKIP_LINES 55
  jsr NormalTextKernel
  SKIP_LINES 27

  ; Load "Wipe" Message
  ldy #(12*12)
  jsr LoadText
  
  ; Show Wipe Message
  sta WSYNC
  ldx #%11111000
  lda ERROR
  beq SkipDoWipe
  bne ShowDoWipe
SkipDoWipe
  ldx #0
ShowDoWipe
  stx PF2
  jsr NormalTextKernel
  sta WSYNC
  ldx #0
  stx PF2
  SKIP_LINES 6

  ; Load "Ignore" Message
  ldy #(6*12)
  jsr LoadText
  
  ; Show Ignore Message
  sta WSYNC
  ldx #%11111000
  lda ERROR
  bne SkipWipeIgnore
  beq ShowWipeIgnore
SkipWipeIgnore
  ldx #0
ShowWipeIgnore
  stx PF2
  jsr NormalTextKernel
  sta WSYNC
  ldx #0
  stx PF2
  
  ; Skip To Next Frame
  jsr NextFrame
  jmp HighScoreWipeLoop

  ; Jump To Start Of Screen
StartScreen
  WAIT_VBLANK
  START_SCREEN
  rts

  ; Jump To Next Frame
NextFrame
  WAIT_SCREEN
  START_OVERSCAN
  WAIT_OVERSCAN
  START_VBLANK
  rts

  ; Wait For Fire Button Release
JoyFireRelease
  ; Check Joystick Button Status
  lda INPT4
  bpl JoyFireWait
  rts
JoyFireWait
  ; Skip to Next Frame
  WAIT_VBLANK
  START_SCREEN
  WAIT_SCREEN
  START_OVERSCAN
  WAIT_OVERSCAN
  START_VBLANK
  jmp JoyFireRelease

  ; Copy text Into Buffer For Display (Combining Pairs Of Characters)
HighScoreTextCopy
  ldx TEXT13
  ldy TEXT12
  lda RChars+4,X
  ora LChars+4,Y
  sta BUFF+34
  lda RChars+3,X
  ora LChars+3,Y
  sta BUFF+33
  lda RChars+2,X
  ora LChars+2,Y
  sta BUFF+32
  lda RChars+1,X
  ora LChars+1,Y
  sta BUFF+31
  lda RChars+0,X
  ora LChars+0,Y
  sta BUFF+30
NormalTextCopy
  ldx TEXT11
  ldy TEXT10
  lda RChars+4,X
  ora LChars+4,Y
  sta BUFF+29
  lda RChars+3,X
  ora LChars+3,Y
  sta BUFF+28
  lda RChars+2,X
  ora LChars+2,Y
  sta BUFF+27
  lda RChars+1,X
  ora LChars+1,Y
  sta BUFF+26
  lda RChars+0,X
  ora LChars+0,Y
  sta BUFF+25
  ldx TEXT9
  ldy TEXT8
  lda RChars+4,X
  ora LChars+4,Y
  sta BUFF+24
  lda RChars+3,X
  ora LChars+3,Y
  sta BUFF+23
  lda RChars+2,X
  ora LChars+2,Y
  sta BUFF+22
  lda RChars+1,X
  ora LChars+1,Y
  sta BUFF+21
  lda RChars+0,X
  ora LChars+0,Y
  sta BUFF+20
  ldx TEXT7
  ldy TEXT6
  lda RChars+4,X
  ora LChars+4,Y
  sta BUFF+19
  lda RChars+3,X
  ora LChars+3,Y
  sta BUFF+18
  lda RChars+2,X
  ora LChars+2,Y
  sta BUFF+17
  lda RChars+1,X
  ora LChars+1,Y
  sta BUFF+16
  lda RChars+0,X
  ora LChars+0,Y
  sta BUFF+15
  ldx TEXT5
  ldy TEXT4
  lda RChars+4,X
  ora LChars+4,Y
  sta BUFF+14
  lda RChars+3,X
  ora LChars+3,Y
  sta BUFF+13
  lda RChars+2,X
  ora LChars+2,Y
  sta BUFF+12
  lda RChars+1,X
  ora LChars+1,Y
  sta BUFF+11
  lda RChars+0,X
  ora LChars+0,Y
  sta BUFF+10
  ldx TEXT3
  ldy TEXT2
  lda RChars+4,X
  ora LChars+4,Y
  sta BUFF+9
  lda RChars+3,X
  ora LChars+3,Y
  sta BUFF+8
  lda RChars+2,X
  ora LChars+2,Y
  sta BUFF+7
  lda RChars+1,X
  ora LChars+1,Y
  sta BUFF+6
  lda RChars+0,X
  ora LChars+0,Y
  sta BUFF+5
  ldx TEXT1
  ldy TEXT0
  lda RChars+4,X
  ora LChars+4,Y
  sta BUFF+4
  lda RChars+3,X
  ora LChars+3,Y
  sta BUFF+3
  lda RChars+2,X
  ora LChars+2,Y
  sta BUFF+2
  lda RChars+1,X
  ora LChars+1,Y
  sta BUFF+1
  lda RChars+0,X
  ora LChars+0,Y
  sta BUFF+0
  rts

  ; Copy Score Into ZP RAM (X Contains Score Offset)
LoadScore
  ; Show Score Position (1 to 10)
  ldy #<L_GAP
  sty TEXT2
  lda ScoreNumber,X
  bpl StoreDigit1
ShowDigit1
  ldy #<L_11
StoreDigit1
  sty TEXT0
  and #%00001111
  tay
  lda ScoreDigits,Y
  sta TEXT1
  ; Copy Score
  clc
  lda #<HISCORES
  adc ScoreOffsets,X
  tay
  jsr CopyScore
  ; Copy Name
  lda $03,Y
  and #%00011111
  tax
  lda ScoreLetters,X
  sta TEXT3
  lda $03,Y
  and #%11100000
  lsr
  lsr
  lsr
  sta TEMP
  lda $04,Y
  and #%00000011
  ora TEMP
  tax
  lda ScoreLetters,X
  sta TEXT4
  lda $04,Y
  and #%01111100
  lsr
  lsr
  tax
  lda ScoreLetters,X
  sta TEXT5
  ; Copy Wave Number
  ldx $05,Y
  jsr CopyWave
  jmp HighScoreTextCopy

  ; Create Text Showing Score and Table Position
LoadPosition
  ldx POSITION              ; [0] + 3
  lda ScoreNumber,X         ; [3] + 4
  bpl ShowNormalPosition    ; [7] + 2/3
  lda #<L_11                ; [9] + 2
  sta TEXT0                 ; [11] + 3
  lda #<L_00                ; [14] + 3
  sta TEXT1                 ; [17] + 3
  lda #<L_DOT               ; [20] + 3
  jmp ContinueLoadPosition  ; [23] + 3
ShowNormalPosition
  and #%00001111            ; [10] + 2
  tay                       ; [12] + 2
  lda ScoreDigits,Y         ; [14] + 4
  sta TEXT0                 ; [18] + 3
  lda #<L_DOT               ; [21] + 2
  sta TEXT1                 ; [23] + 3
ContinueLoadPosition
  sta TEXT2
  sta TEXT3
  sta TEXT4
  sta TEXT5
  ldy #<SCORE
  jsr CopyScore
  jmp NormalTextCopy
  
  ; Create text Showing Best/Last Scores
LoadBest
  lda #<L_BB
  sta TEXT2
  lda #<L_EE
  sta TEXT3
  ldx HISCORE+3
  jsr CopyWave
  ldy #<HISCORE
  jmp CopyBestLast
LoadLast
  lda #<L_LL
  sta TEXT2
  lda #<L_AA
  sta TEXT3
  ldx WAVE
  jsr CopyWave
  ldy #<SCORE
CopyBestLast
  lda #<L_SS
  sta TEXT4
  lda #<L_TT
  sta TEXT5
  lda #<L_GAP
  sta TEXT0
  sta TEXT1
  jsr CopyScore
  jmp HighScoreTextCopy

; Copy Wave (Wave Number Is In X)
CopyWave
  lda HighScoreBCDTable,X
  tay
  lsr
  lsr
  lsr
  lsr
  tax
  lda ScoreDigits,X
  sta TEXT12
  tya
  and #%00001111
  tax
  lda ScoreDigits,X
  sta TEXT13
  rts

; Copy Score (Score Start Pointer Is In Y)
CopyScore
  lda $02,Y
  lsr
  lsr
  lsr
  lsr
  tax
  lda ScoreDigits,X
  sta TEXT6
  lda $02,Y
  and #%00001111
  tax
  lda ScoreDigits,X
  sta TEXT7
  lda $01,Y
  lsr
  lsr
  lsr
  lsr
  tax
  lda ScoreDigits,X
  sta TEXT8
  lda $01,Y
  and #%00001111
  tax
  lda ScoreDigits,X
  sta TEXT9
  lda $00,Y
  lsr
  lsr
  lsr
  lsr
  tax
  lda ScoreDigits,X
  sta TEXT10
  lda $00,Y
  and #%00001111
  tax
  lda ScoreDigits,X
  sta TEXT11
  rts

; -----------------------------------------------------------------------------
; PART 7 - STARTUP DISPLAY
; -----------------------------------------------------------------------------

  ALIGN 256

StartupSequence
  ; Reflect PF
  lda #%00000001
  sta CTRLPF

  ; Clear Position Counter
  lda #0
  sta POSITION

StartupLoop
  ; Update Game Cycle
  dec CYCLE
  
  ; Check For Wipe Scores
  lda INPT4
  bmi NoStartupFire
  lda #KEYWAIT
  sta DEBOUNCE
  bit SWCHA
  bmi EndWipeCheck
  jsr WipeHighScores
  ; Delay 1 Frame After Wiping Scores
  jsr StartScreen
  jsr NextFrame
  jmp FinishStartup2
EndWipeCheck
  jmp FinishStartup
NoStartupFire

  ; Load Text Into Buffer
  lax POSITION
  and #%00000111
  bne EndStartupText
  txa
  cmp #80
  bcc NextTextPosition
EmptyTextPosition
  ldx #10
  bne LoadAAText
NextTextPosition
  lsr
  lsr
  lsr
  tax
LoadAAText
  lda AALine6,X
  sta LOGOBUFF+6
  lda AALine5,X
  sta LOGOBUFF+13
  lda AALine4,X
  sta LOGOBUFF+20
  lda AALine3,X
  sta LOGOBUFF+27
  lda AALine2,X
  sta LOGOBUFF+34
  lda AALine1,X
  sta LOGOBUFF+41
EndStartupText

  ; Scroll Logo
  lda #35
ScrollLoop
  tax
  rol LOGOBUFF+6,X
  rol LOGOBUFF+5,X
  ror LOGOBUFF+4,X
  rol LOGOBUFF+3,X
  ror LOGOBUFF+2,X
  rol LOGOBUFF+1,X
  ror LOGOBUFF+0,X
  sec
  sbc #7
  bpl ScrollLoop

  ; Increment Position
  inc POSITION
  bpl StartupKernel

FinishStartup
  ; Clear Highscore Table Area
  jsr StartScreen
  CLEAR_HISCORES
  jsr NextFrame

FinishStartup2
  ; Read Hiscore Table
  jsr ReadScores
SkipReadScores

  ; Skip To Title Screen
  jsr StartScreen
  jsr NextFrame
  ; Reset Title Variables
  lda #$FF
  sta FASTCYCLE
  ; Reset Starting Wave
  lda #0
  sta STARTWAVE
  jmp EndHighScores

StartupKernel
  ; Start Screen Display
  jsr StartScreen

  ; Position Sprites
  jsr PositionText
  SKIP_LINES 12
  
  ; Set Logo Sizes
  ldx #35
StartupOuterLoop
  ldy #ATARIH
StartupInnerLoop
  sta WSYNC                 ; [0]
  ; Set Logo Colour
  IF (PALCOLS)
  lda #$D4                  ; [0] + 2
  ELSE
  lda #$94                  ; [0] + 2
  ENDIF
  sta COLUPF                ; [2] + 3     < 22
  ; Copy Logo To Screen
  lda LOGOBUFF+0,X          ; [5] + 4
  sta PF0                   ; [9] + 3     < 22
  lda LOGOBUFF+1,X          ; [12] + 4
  sta PF1                   ; [16] + 3    < 28
  lda LOGOBUFF+2,X          ; [19] + 4
  sta PF2                   ; [23] + 3    < 38
  lda LOGOBUFF+5,X          ; [26] + 4
  sta PF0                   ; [30] + 3    > 28  < 70
  lda LOGOBUFF+4,X          ; [33] + 4
  sta PF1                   ; [37] + 3    > 38  < 60
  lda LOGOBUFF+3,X          ; [40] + 4
  sta.w PF2                 ; [44] + 4    = 48!
  dey                       ; [48] + 2
  bpl StartupInnerLoop      ; [50] + 2/3
  sec                       ; [52] + 2
  txa                       ; [54] + 2
  sbc #7                    ; [56] + 2
  tax                       ; [58] + 2
  bpl StartupOuterLoop      ; [60] + 2/3
  SLEEP 3                   ; [62] + 3
  lda #0                    ; [65] + 2
  sta PF2                   ; [67] + 3
  sta PF1                   ; [70] + 3
  sta PF0                   ; [73] + 3
EndStartupKernel
  if (>StartupKernel != >EndStartupKernel)
    echo "WARNING: Startup Kernel Crosses Page Boundary!"
  endif
  
  ; Calculate Colour Fade
  lda CYCLE
  lsr
  lsr
  and #%00001111
  tax
  lda ScoreSlowFade,X
  sta TEMP
  clc
  IF (PALCOLS)
  lda #$50
  ELSE
  lda #$C0
  ENDIF
  adc TEMP
  sta COLUP0
  sta COLUP1
  ldy #(0*12)
  jsr LoadText
  jsr NormalTextKernel
  ldy #(1*12)
  jsr LoadText
  jsr NormalTextKernel

  ; Next Iteration
  jsr NextFrame
  jmp StartupLoop

; -----------------------------------------------------------------------------
; PART 8 - HIGHSCORE DISPLAY KERNELS & SUBROUTINES
; -----------------------------------------------------------------------------

  ALIGN   256
  
NameKernel
  sta WSYNC                 ; [0]
  sta HMCLR                 ; [0] + 3
  lda #0                    ; [3] + 2
  sta NUSIZ1                ; [5] + 3
  sta VDELP0                ; [8] + 3
  sta VDELP1                ; [11] + 3
  IF (PALCOLS)
  lda #$36                  ; [14] + 2
  ELSE
  lda #$D6                  ; [14] + 2
  ENDIF
  sta COLUP0                ; [16] + 3
  sta COLUP1                ; [19] + 3
  lda #%11110001            ; [22] + 2
  sta HMP0                  ; [24] + 3
  sta NUSIZ0                ; [27] + 3
  lda #>LChars              ; [30] + 2
  sta PTR0+1                ; [32] + 3
  sta PTR1+1                ; [35] + 3
  sta PTR2+1                ; [38] + 3
  sta RESP0                 ; [41] + 3 = 44 EXACT
  sta RESP1                 ; [44] + 3 = 47 EXACT
  ldx NAME+0                ; [47] + 3
  ldy ScoreLetters,X        ; [50] + 4
  sty PTR0                  ; [54] + 3
  ldx NAME+1                ; [57] + 3
  ldy ScoreLetters,X        ; [60] + 4
  sty PTR1                  ; [64] + 3
  ldx NAME+2                ; [67] + 3
  ldy ScoreLetters,X        ; [70] + 4
  lda #%00000010            ; [74] + 2
  sta HMOVE                 ; [0] + 3
  sty PTR2                  ; [3] + 3
  sta ENABL                 ; [6] + 3
  ldy #4                    ; [9] + 2
NameKernelLoop
  sta WSYNC                 ; [0]
  lda (PTR0),Y              ; [0] + 5
  sta GRP0                  ; [5] + 3   < 46
  lda (PTR1),Y              ; [8] + 5
  sta GRP1                  ; [13] + 3  < 48
  lda (PTR2),Y              ; [16] + 5
  SLEEP 25                  ; [21] + 25
  sta GRP0                  ; [46] + 3  > 48 < 52
  dey
  bpl NameKernelLoop
  sta WSYNC
  iny
  sty GRP1
  sty GRP0
  sta WSYNC
  sty ENABL
  rts
EndNameKernel
  if (>NameKernel != >EndNameKernel)
    echo "WARNING: Name Kernel Crosses Page Boundary!"
  endif

CursorPosition
  sta WSYNC                 ; [0]
  ldx CURSOR                ; [0] + 3
  beq Cursor0               ; [3] + 2/3
  dex                       ; [5] + 2
  beq Cursor1               ; [7] + 2/3
Cursor2
  SLEEP 3                   ; [9] + 3
  lda #0                    ; [12] + 2
  beq CursorDelay           ; [14] + 3
Cursor1
  lda #%00100000            ; [10] + 2
  bne CursorDelay           ; [12] + 3
Cursor0
  SLEEP 4                   ; [6] + 4
  lda #%00010000            ; [10] + 2
CursorDelay
  SLEEP 27                  ; [12][15][17] + 27
  sta HMBL                  ; [39][42][44] + 3
  sta RESBL                 ; [42][45][47] + 3
  sta WSYNC
  sta HMOVE
  rts
EndCursorPosition
  if (>CursorPosition != >EndCursorPosition)
    echo "WARNING: CursorPosition Crosses Page Boundary!"
  endif

PositionHighScore
  ; Position Sprites
  sta WSYNC                 ; [0]
  ; Set 3 sprite copies (medium)
  lda #%00000110            ; [0] + 2
  sta NUSIZ0                ; [2] + 3
  ; Set 3 sprite copies (close)
  lda #%00000011            ; [5] + 2
  sta NUSIZ1                ; [7] + 3
  ; No Delay
  lda #0                    ; [10] + 2
  sta VDELP0                ; [12] + 3
  sta VDELP1                ; [15] + 3
  ; Position Sprites
  lda #%11100000            ; [18] + 2
  sta HMP0                  ; [20] + 3
  sta HMP1                  ; [23] + 3
  nop                       ; [26] + 2
  sta RESP0                 ; [28] + 3  = 31
  SLEEP 5                   ; [31] + 5
  sta RESP1                 ; [36] + 3  = 39
  sta WSYNC
  sta HMOVE
  rts
EndPositionHighScore
  if (>PositionHighScore != >EndPositionHighScore)
    echo "WARNING: PositionHighScore Crosses Page Boundary!"
  endif

  ; Copy Text into ZP RAM (Y Contains Message Offset)
LoadText
  lda HighScoreMessages+11,Y
  sta TEXT11
  lda HighScoreMessages+10,Y
  sta TEXT10
  lda HighScoreMessages+9,Y
  sta TEXT9
  lda HighScoreMessages+8,Y
  sta TEXT8
  lda HighScoreMessages+7,Y
  sta TEXT7
  lda HighScoreMessages+6,Y
  sta TEXT6
  lda HighScoreMessages+5,Y
  sta TEXT5
  lda HighScoreMessages+4,Y
  sta TEXT4
  lda HighScoreMessages+3,Y
  sta TEXT3
  lda HighScoreMessages+2,Y
  sta TEXT2
  lda HighScoreMessages+1,Y
  sta TEXT1
  lda HighScoreMessages+0,Y
  sta TEXT0
  jmp NormalTextCopy
EndLoadText

  ALIGN   256

  ; Display High Score Table Line (Unrolled)
HighScoreKernel
  sta WSYNC
  lda #%00000011            ; [0] + 2
  sta NUSIZ1                ; [2] + 3
  lda BUFF+4                ; [5] + 3
  sta GRP0                  ; [8] + 3   < 34
  lda BUFF+19               ; [11] + 3
  sta GRP1                  ; [14] + 3  < 42
  ldx BUFF+14               ; [17] + 3
  ldy BUFF+34               ; [20] + 3
  lda BUFF+24               ; [23] + 3
  SLEEP 8                   ; [26] + 8
  sta GRP0                  ; [34] + 3  > 36 < 44
  nop                       ; [37] + 2
  lda BUFF+29               ; [39] + 3
  sta GRP1                  ; [42] + 3  > 44 < 47
  lda BUFF+9                ; [45] + 3
  sta GRP1                  ; [48] + 3  > 49 < 52 
  stx GRP0                  ; [51] + 3  > 46 < 55
  lda #%00000110            ; [54] + 2
  sta NUSIZ1                ; [56] + 3  > 54 < 63
  sty GRP1                  ; [59] + 3  > 54 < 63
  sta WSYNC                 ; [0]
  lda #%00000011            ; [0] + 2
  sta NUSIZ1                ; [2] + 3
  lda BUFF+3                ; [5] + 3
  sta GRP0                  ; [8] + 3   < 34
  lda BUFF+18               ; [11] + 3
  sta GRP1                  ; [14] + 3  < 42
  ldx BUFF+13               ; [17] + 3
  ldy BUFF+33               ; [20] + 3
  lda BUFF+23               ; [23] + 3
  SLEEP 8                   ; [26] + 8
  sta GRP0                  ; [34] + 3  > 36 < 44
  nop                       ; [37] + 2
  lda BUFF+28               ; [39] + 3
  sta GRP1                  ; [42] + 3  > 44 < 47
  lda BUFF+8                ; [45] + 3
  sta GRP1                  ; [48] + 3  > 49 < 52 
  stx GRP0                  ; [51] + 3  > 46 < 55
  lda #%00000110            ; [54] + 2
  sta NUSIZ1                ; [56] + 3  > 54 < 63
  sty GRP1                  ; [59] + 3  > 54 < 63
  sta WSYNC                 ; [0]
  lda #%00000011            ; [0] + 2
  sta NUSIZ1                ; [2] + 3
  lda BUFF+2                ; [5] + 3
  sta GRP0                  ; [8] + 3   < 34
  lda BUFF+17               ; [11] + 3
  sta GRP1                  ; [14] + 3  < 42
  ldx BUFF+12               ; [17] + 3
  ldy BUFF+32               ; [20] + 3
  lda BUFF+22               ; [23] + 3
  SLEEP 8                   ; [26] + 8
  sta GRP0                  ; [34] + 3  > 36 < 44
  nop                       ; [37] + 2
  lda BUFF+27               ; [39] + 3
  sta GRP1                  ; [42] + 3  > 44 < 47
  lda BUFF+7                ; [45] + 3
  sta GRP1                  ; [48] + 3  > 49 < 52 
  stx GRP0                  ; [51] + 3  > 46 < 55
  lda #%00000110            ; [54] + 2
  sta NUSIZ1                ; [56] + 3  > 54 < 63
  sty GRP1                  ; [59] + 3  > 54 < 63
  sta WSYNC                 ; [0]
  lda #%00000011            ; [0] + 2
  sta NUSIZ1                ; [2] + 3
  lda BUFF+1                ; [5] + 3
  sta GRP0                  ; [8] + 3   < 34
  lda BUFF+16               ; [11] + 3
  sta GRP1                  ; [14] + 3  < 42
  ldx BUFF+11               ; [17] + 3
  ldy BUFF+31               ; [20] + 3
  lda BUFF+21               ; [23] + 3
  SLEEP 8                   ; [26] + 8
  sta GRP0                  ; [34] + 3  > 36 < 44
  nop                       ; [37] + 2
  lda BUFF+26               ; [39] + 3
  sta GRP1                  ; [42] + 3  > 44 < 47
  lda BUFF+6                ; [45] + 3
  sta GRP1                  ; [48] + 3  > 49 < 52 
  stx GRP0                  ; [51] + 3  > 46 < 55
  lda #%00000110            ; [54] + 2
  sta NUSIZ1                ; [56] + 3  > 54 < 63
  sty GRP1                  ; [59] + 3  > 54 < 63
  sta WSYNC                 ; [0]
  lda #%00000011            ; [0] + 2
  sta NUSIZ1                ; [2] + 3
  lda BUFF+0                ; [5] + 3
  sta GRP0                  ; [8] + 3   < 34
  lda BUFF+15               ; [11] + 3
  sta GRP1                  ; [14] + 3  < 42
  ldx BUFF+10               ; [17] + 3
  ldy BUFF+30               ; [20] + 3
  lda BUFF+20               ; [23] + 3
  SLEEP 8                   ; [26] + 8
  sta GRP0                  ; [34] + 3  > 36 < 44
  nop                       ; [37] + 2
  lda BUFF+25               ; [39] + 3
  sta GRP1                  ; [42] + 3  > 44 < 47
  lda BUFF+5                ; [45] + 3
  sta GRP1                  ; [48] + 3  > 49 < 52 
  stx GRP0                  ; [51] + 3  > 46 < 55
  lda #%00000110            ; [54] + 2
  sta NUSIZ1                ; [56] + 3  > 54 < 63
  sty GRP1                  ; [59] + 3  > 54 < 63
  lda #0                    ; [62] + 2
  sta GRP0                  ; [64] + 3  > 57
  sta GRP1                  ; [67] + 3  > 65
  sta GRP0                  ; [70] + 3
  rts
EndHighScoreKernel
  if (>HighScoreKernel != >EndHighScoreKernel)
    echo "WARNING: HighScoreKernel Crosses Page Boundary!"
  endif

  ; Score Tables
ScoreNumber
  DC.B  128, 9, 8, 7, 6, 5, 4, 3, 2, 1
ScoreOffsets
  DC.B  54, 48, 42, 36, 30, 24, 18, 12, 6, 0
ScoreOffsetPtrs
  DC.B  <HISCORES+54, <HISCORES+48, <HISCORES+42, <HISCORES+36, <HISCORES+30
  DC.B  <HISCORES+24, <HISCORES+18, <HISCORES+12, <HISCORES+6, <HISCORES+0
EndScoreTables
  if (>ScoreNumber != >EndScoreTables)
    echo "WARNING: Score Tables Cross Page Boundary!"
  endif
  
  ALIGN   256
  
PositionText
  sta WSYNC                 ; [0]
  ; Set 3 Copies Close and Delay
  lda #%00000011            ; [0] + 2
  sta VDELP0                ; [2] + 3
  sta VDELP1                ; [5] + 3
  sta NUSIZ0                ; [8] + 3
  sta NUSIZ1                ; [11] + 3
  lda #0                    ; [14] + 2
  sta GRP0                  ; [16] + 3
  sta GRP1                  ; [19] + 3
  SLEEP 4                   ; [22] + 4
  ; Position Sprites
  lda #%11100000            ; [26] + 2
  sta HMP0                  ; [28] + 3
  lda #%11110000            ; [31] + 2
  sta HMP1                  ; [33] + 3
  sta RESP0                 ; [36] + 3  = 39
  sta RESP1                 ; [39] + 3  = 42
  sta WSYNC                 ; [0]
  sta HMOVE
  rts

NormalTextKernel
  sta WSYNC
  SLEEP 15                  ; [0] + 15
  lda BUFF+4                ; [15] + 3                  0A 0B 1A 1B
  sta GRP0                  ; [18] + 3                  0 - - -
  lda BUFF+9                ; [21] + 3
  sta GRP1                  ; [24] + 3  < 42            - 0 1 0
  lda BUFF+14               ; [27] + 3
  sta GRP0                  ; [30] + 3  < 44            2 0 - 1
  ldx BUFF+24               ; [33] + 3
  ldy BUFF+29               ; [36] + 3
  lda BUFF+19               ; [39] + 3
  sta GRP1                  ; [42] + 3  > 44 < 47       - 2 3 1
  stx GRP0                  ; [45] + 3  > 46 < 50       4 2 - 3
  sty GRP1                  ; [48] + 3  > 49 < 52       - 4 5 3
  sta GRP0                  ; [51] + 3  > 52 < 55       - 4 - 5
  sta WSYNC                 ; [0]
  SLEEP 15                  ; [0] + 15
  lda BUFF+3                ; [15] + 3                  0A 0B 1A 1B
  sta GRP0                  ; [18] + 3                  0 - - -
  lda BUFF+8                ; [21] + 3
  sta GRP1                  ; [24] + 3  < 42            - 0 1 0
  lda BUFF+13               ; [27] + 3
  sta GRP0                  ; [30] + 3  < 44            2 0 - 1
  ldx BUFF+23               ; [33] + 3
  ldy BUFF+28               ; [36] + 3
  lda BUFF+18               ; [39] + 3
  sta GRP1                  ; [42] + 3  > 44 < 47       - 2 3 1
  stx GRP0                  ; [45] + 3  > 46 < 50       4 2 - 3
  sty GRP1                  ; [48] + 3  > 49 < 52       - 4 5 3
  sta GRP0                  ; [51] + 3  > 52 < 55       - 4 - 5
  sta WSYNC                 ; [0]
  SLEEP 15                  ; [0] + 15
  lda BUFF+2                ; [15] + 3                  0A 0B 1A 1B
  sta GRP0                  ; [18] + 3                  0 - - -
  lda BUFF+7                ; [21] + 3
  sta GRP1                  ; [24] + 3  < 42            - 0 1 0
  lda BUFF+12               ; [27] + 3
  sta GRP0                  ; [30] + 3  < 44            2 0 - 1
  ldx BUFF+22               ; [33] + 3
  ldy BUFF+27               ; [36] + 3
  lda BUFF+17               ; [39] + 3
  sta GRP1                  ; [42] + 3  > 44 < 47       - 2 3 1
  stx GRP0                  ; [45] + 3  > 46 < 50       4 2 - 3
  sty GRP1                  ; [48] + 3  > 49 < 52       - 4 5 3
  sta GRP0                  ; [51] + 3  > 52 < 55       - 4 - 5
  sta WSYNC                 ; [0]
  SLEEP 15                  ; [0] + 15
  lda BUFF+1                ; [15] + 3                  0A 0B 1A 1B
  sta GRP0                  ; [18] + 3                  0 - - -
  lda BUFF+6                ; [21] + 3
  sta GRP1                  ; [24] + 3  < 42            - 0 1 0
  lda BUFF+11               ; [27] + 3
  sta GRP0                  ; [30] + 3  < 44            2 0 - 1
  ldx BUFF+21               ; [33] + 3
  ldy BUFF+26               ; [36] + 3
  lda BUFF+16               ; [39] + 3
  sta GRP1                  ; [42] + 3  > 44 < 47       - 2 3 1
  stx GRP0                  ; [45] + 3  > 46 < 50       4 2 - 3
  sty GRP1                  ; [48] + 3  > 49 < 52       - 4 5 3
  sta GRP0                  ; [51] + 3  > 52 < 55       - 4 - 5
  sta WSYNC                 ; [0]
  SLEEP 15                  ; [0] + 15
  lda BUFF+0                ; [15] + 3                  0A 0B 1A 1B
  sta GRP0                  ; [18] + 3                  0 - - -
  lda BUFF+5                ; [21] + 3
  sta GRP1                  ; [24] + 3  < 42            - 0 1 0
  lda BUFF+10               ; [27] + 3
  sta GRP0                  ; [30] + 3  < 44            2 0 - 1
  ldx BUFF+20               ; [33] + 3
  ldy BUFF+25               ; [36] + 3
  lda BUFF+15               ; [39] + 3
  sta GRP1                  ; [42] + 3  > 44 < 47       - 2 3 1
  stx GRP0                  ; [45] + 3  > 46 < 50       4 2 - 3
  sty GRP1                  ; [48] + 3  > 49 < 52       - 4 5 3
  sta GRP0                  ; [51] + 3  > 52 < 55       - 4 - 5
  lda #0
  sta GRP1
  sta GRP0
  sta GRP1
  rts
EndNormalTextKernel
  if (>PositionText != >EndNormalTextKernel)
    echo "WARNING: NormalTextKernel Crosses Page Boundary!"
  endif

; -----------------------------------------------------------------------------
; PART 9 - HIGHSCORE DATA
; -----------------------------------------------------------------------------

  ALIGN   256

  ; HighScore Messages (156 Entries)
HighScoreMessages
  ; PlusROM
  DC.B  <L_PP, <L_LL, <L_UU, <L_SS, <L_RR, <L_OO, <L_MM, <L_GAP
  DC.B  <L_GAP, <L_HH, <L_SS, <L_CC                                  ; 0
  ; Edition
  DC.B  <L_GAP, <L_GAP, <L_EE, <L_DD, <L_II, <L_TT, <L_II, <L_OO
  DC.B  <L_NN, <L_BANG, <L_GAP, <L_GAP                               ; 1
  ; Not Found
  DC.B  <L_GAP, <L_NN, <L_OO, <L_TT, <L_GAP, <L_FF, <L_OO, <L_UU
  DC.B  <L_NN, <L_DD, <L_BANG, <L_GAP                               ; 2
  ; Read Error
  DC.B  <L_RR, <L_EE, <L_AA, <L_DD, <L_GAP, <L_EE, <L_RR, <L_RR
  DC.B  <L_OO, <L_RR, <L_BANG, <L_BANG                              ; 3
  ; Write Error
  DC.B  <L_WW, <L_RR, <L_II, <L_TT, <L_EE, <L_GAP, <L_EE, <L_RR
  DC.B  <L_RR, <L_OO, <L_RR, <L_BANG                                ; 4
  ; Retry
  DC.B  <L_GAP, <L_GAP, <L_GAP, <L_RR, <L_EE, <L_TT, <L_RR, <L_YY
  DC.B  <L_QMARK, <L_GAP, <L_GAP, <L_GAP                            ; 5
  ; Ignore
  DC.B  <L_GAP, <L_GAP, <L_GAP, <L_II, <L_GG, <L_NN, <L_OO, <L_RR
  DC.B  <L_EE, <L_GAP, <L_GAP, <L_GAP                               ; 6
  ; New Hiscore
  DC.B  <L_NN, <L_EE, <L_WW, <L_GAP, <L_HH, <L_II, <L_SS, <L_CC
  DC.B  <L_OO, <L_RR, <L_EE, <L_BANG                                ; 7
  ; Enter Name
  DC.B  <L_GAP, <L_EE, <L_NN, <L_TT, <L_EE, <L_RR, <L_GAP, <L_NN
  DC.B  <L_AA, <L_MM, <L_EE, <L_GAP                                 ; 8
  ; Fire To Save
  DC.B  <L_FF, <L_II, <L_RR, <L_EE, <L_GAP, <L_TT, <L_OO, <L_GAP
  DC.B  <L_SS, <L_AA, <L_VV, <L_EE                                  ; 9
  ; Juno Ranking
  DC.B  <L_JJ, <L_UU, <L_NN, <L_OO, <L_GAP, <L_RR, <L_AA, <L_NN
  DC.B  <L_KK, <L_II, <L_NN, <L_GG                                  ; 10
  ; Wipe Scores
  DC.B  <L_WW, <L_II, <L_PP, <L_EE, <L_GAP, <L_SS, <L_CC, <L_OO
  DC.B  <L_RR, <L_EE, <L_SS, <L_QMARK                               ; 11
  ; Wipe
  DC.B  <L_GAP, <L_GAP, <L_GAP, <L_WW, <L_II, <L_PP, <L_EE, <L_BANG
  DC.B  <L_BANG, <L_GAP, <L_GAP, <L_GAP                             ; 12
EndHighScoreMessages

  ; Binary To BCD Table (100 Entries)
HighScoreBCDTable
  DC.B  %00000000, %00000001, %00000010, %00000011, %00000100
  DC.B  %00000101, %00000110, %00000111, %00001000, %00001001
  DC.B  %00010000, %00010001, %00010010, %00010011, %00010100
  DC.B  %00010101, %00010110, %00010111, %00011000, %00011001
  DC.B  %00100000, %00100001, %00100010, %00100011, %00100100
  DC.B  %00100101, %00100110, %00100111, %00101000, %00101001
  DC.B  %00110000, %00110001, %00110010, %00110011, %00110100
  DC.B  %00110101, %00110110, %00110111, %00111000, %00111001
  DC.B  %01000000, %01000001, %01000010, %01000011, %01000100
  DC.B  %01000101, %01000110, %01000111, %01001000, %01001001
  DC.B  %01010000, %01010001, %01010010, %01010011, %01010100
  DC.B  %01010101, %01010110, %01010111, %01011000, %01011001
  DC.B  %01100000, %01100001, %01100010, %01100011, %01100100
  DC.B  %01100101, %01100110, %01100111, %01101000, %01101001
  DC.B  %01110000, %01110001, %01110010, %01110011, %01110100
  DC.B  %01110101, %01110110, %01110111, %01111000, %01111001
  DC.B  %10000000, %10000001, %10000010, %10000011, %10000100
  DC.B  %10000101, %10000110, %10000111, %10001000, %10001001
  DC.B  %10010000, %10010001, %10010010, %10010011, %10010100
  DC.B  %10010101, %10010110, %10010111, %10011000, %10011001
EndHighScoreBCDTable

  ALIGN   256

  ; Small Font LHS
  LEFTCHARS

  ; Score Digits Table (10 Entries)
ScoreDigits
  DC.B  <L_00, <L_11, <L_22, <L_33, <L_44, <L_55, <L_66, <L_77, <L_88, <L_99
  
  ; Score Letters Table (32 Entries)
ScoreLetters
  DC.B  <L_AA, <L_BB, <L_CC, <L_DD, <L_EE, <L_FF, <L_GG, <L_HH
  DC.B  <L_II, <L_JJ, <L_KK, <L_LL, <L_MM, <L_NN, <L_OO, <L_PP
  DC.B  <L_QQ, <L_RR, <L_SS, <L_TT, <L_UU, <L_VV, <L_WW, <L_XX
  DC.B  <L_YY, <L_ZZ, <L_DOT, <L_BANG, <L_QMARK, <L_DASH, <L_COLON, <L_GAP

  ; Score Colour Table (8 Entries)
ScoreColTab
  IF (PALCOLS)
  DC.B  $2E, $4E, $6E, $8E, $9E, $7E, $5E, $3E
  ELSE
  DC.B  $1E, $2E, $4E, $5E, $6E, $9E, $BE, $DE
  ENDIF

  ; Score Colour Table 2 (8 Entries)
ScoreColTab2
  IF (PALCOLS)
  DC.B  $80, $90, $70, $50, $30, $20, $40, $60
  ELSE
  DC.B  $50, $60, $90, $B0, $D0, $10, $20, $40
  ENDIF

  ; Score Fast Fade (8 Entries)
ScoreFastFade
  DC.B  $02, $04, $08, $0A, $0E, $0A, $08, $04
  
  ; Score Slow Fade (16 Entries)
ScoreSlowFade
  DC.B  $00, $02, $04, $06, $08, $0A, $0C, $0E
  DC.B  $0E, $0C, $0A, $08, $06, $04, $02, $00

  ALIGN   256

  ; Small Font RHS
  RIGHTCHARS

  ; AtariAge Startup Logo
  AALOGO

  echo "----",($FFF0 - *) , "bytes left (BANK 6 - HIGHSCORES)"

  ORG     $DFF0
  RORG    $FFF0
  DC.B    0, 0, 0, 255
  DC.B    0, 0, 0, 0, 0, 0
  DC.W    (PlusAPI - $E000)
  DC.W    Init6, Init6

