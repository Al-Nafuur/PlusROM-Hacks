; -----------------------------------------------------------------------------
; CHETIRY - Atari 2600 - (C) Copyright 2011 - AtariAge
; BANK 7 - HIGH SCORES
; -----------------------------------------------------------------------------

  ; Logo Colours
  IF (PALMODE)
AABLUE    = $B6
AAORANGE  = $46
  ELSE
AABLUE    = $96
AAORANGE  = $36
  ENDIF

  ; Clear Hiscore Table
  MAC CLEAR_HISCORES
  lda #0                  ; [0] + 2
  ldx #59                 ; [2] + 2 = 4
.WipeScoresLoop
  sta HI_WRITE,X          ; [0] + 4
  dex                     ; [4] + 2
  bpl .WipeScoresLoop     ; [6] + 2/3
  ENDM                    ; RUNTIME = (9*59) + 8 + 4 = 543 (7.1 SCANLINES)

  SEG     BANK7
  ORG     $F000
  RORG    $F000

  ; Space for DPC registers
  DS.B    256,0

  ; Display Title Screen
ShowTitleScreen
  ; Switch To Bank 5
  nop     $FFF9
  ; Entry Point
  jmp     Start
  ; End Of Game
  jmp     EndOfGame
  ; End Of Titles
  jmp     EndHiScores
  ; End Of Title Music
  jmp     ResumeTitleMusic
ShowCredits
  ; Switch To Bank 6
  nop     $FFFA
ShowMenu
  ; Switch To Bank 6
  nop     $FFFA
PlayTitleMusic
  ; Switch To Bank 5
  nop     $FFF9

; -----------------------------------------------------------------------------
; TITLE SCREENS
; -----------------------------------------------------------------------------

ShowTitles
  ; Display Title Screen
  ldx #127
  stx SLOWCYCLE

RestartTitles
  ; Load Title Tune
  lda #5
  ldx #1
  jsr DoOperation

  ; Set Music Tempo
  ldx #TITLETEMPO
  stx TEMPO
  stx MUSIC

  ; Titles Loop
TitlesLoop
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; Update Game Cycle & Counters
  dec CYCLE               ; [5] + 5
  lda CYCLE               ; [10] + 3
  lsr                     ; [13] + 2
  bcc EndCycle            ; [15] + 2/3
  lsr                     ; [17] + 2
  bcc EndCycle            ; [19] + 2/3
  lsr                     ; [21] + 2
  bcc EndCycle            ; [23] + 2/3
  dec SLOWCYCLE           ; [25] + 5
EndCycle

  ; Debounce Joystick
  lax DEBOUNCE            ; [30] + 3
  and #%10000000          ; [33] + 2
  sta TEMP                ; [35] + 3
  txa                     ; [38] + 2
  and #%01111111          ; [40] + 2
  beq CheckTitleJoy       ; [42] + 2/3
  sec                     ; [44] + 2
  sbc #1                  ; [46] + 2
  ora TEMP                ; [48] + 3
  sta DEBOUNCE            ; [51] + 3
  jmp EndCheckTitleJoy    ; [54] + 3
CheckTitleJoy
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  ; Check if Already Displaying Scores
  bit SLOWCYCLE           ; [5] + 3
  bvc CheckNewTable       ; [8] + 2/3
  ; Go Directly To Score Table on Left/Right
  bit SWCHA               ; [10] + 3
  bpl ShowScoreTable      ; [13] + 2/3
  bvc ShowScoreTable      ; [15] + 2/3
  jmp CheckTitleUpDown    ; [17] + 2/3
CheckNewTable
  ; Change Score Table on Left/Right
  bit SWCHA               ; [11] + 3
  bvc PrevTable           ; [14] + 2/3
  bpl NextTable           ; [16] + 2/3
CheckTitleUpDown
  ; Show Credits on Joystick Down
  lax SWCHA               ; [20] + 3
  and #%00100000          ; [23] + 2
  beq FlipCredits         ; [25] + 2/3
  ; Show Title Screen on Joystick Up
  txa                     ; [27] + 2
  and #%00010000          ; [29] + 2
  bne EndCheckTitleJoy    ; [31] + 2/3
FlipTitles
  ; Show Titles
  ldx #%01111111          ; [33] + 2
  jmp StoreNewCycle       ; [35] + 3
FlipCredits
  ; Show Credits
  ldx #%11111111          ; [28] + 2
  jmp StoreNewCycle       ; [30] + 3
PrevTable
  dec TABLE               ; [17] + 5
  jmp StoreNewTable       ; [22] + 3
NextTable
  inc TABLE               ; [19] + 5
StoreNewTable
  ; Silence Music (Temporarily)
  ldx #0                  ; [25] + 2
  stx AUDV0               ; [27] + 3
  stx AUDV1               ; [30] + 3
  ; Wrap Table
  lda TABLE               ; [33] + 3
  and #%00000011          ; [36] + 2
  sta TABLE               ; [38] + 3
  ; Load Score Table
  ldx #2                  ; [41] + 2
  jsr DoOperation         ; [43] + 6
  jsr ReadHscTableFromBuffer

ShowScoreTable
  ; Show Score Table
  lda SLOWCYCLE
  and #%10000000
  ora #%00111111
  tax
StoreNewCycle
  stx SLOWCYCLE           ; [38] + 3
  ; Debounce Joystick
  lda DEBOUNCE            ; [41] + 3
  and #%10000000          ; [44] + 2
  ora #KEYWAIT            ; [46] + 2
  sta DEBOUNCE            ; [48] + 3
EndCheckTitleJoy

  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; Check Fire & Show Menu
  lda INPT4               ; [5] + 3
  bmi ResetTitleFire      ; [8] + 2/3
  lda DEBOUNCE            ; [10] + 3
  bmi EndCheckTitleFire   ; [13] + 2/3
  ; Debounce Fire
  ldx #%00001000
  stx DEBOUNCE
  ; Silence Music
  ldx #0
  stx MUSIC
  stx SFX
  stx AUDV0
  stx AUDV1
  sta TUNERESET_W
  ; Load Menu Tune
  lda #6
  ldx #1
  jsr DoOperation
  ; Set Music Tempo
  ldx #MUSICTEMPO
  stx MUSIC
  ; Show Menu
  jmp ShowMenu
ResetTitleFire
  lda DEBOUNCE            ; [11] + 3
  and #%01111111          ; [14] + 2
  sta DEBOUNCE            ; [16] + 3
EndCheckTitleFire

  ; Update Random Number
GetRandomA
  lda RANDOM              ; [19] + 3
  bne NoResetRandomA      ; [22] + 2/3
  lda #RANDOMSEED         ; [24] + 2
NoResetRandomA
  asl                     ; [26] + 2
  bcc NoEorA              ; [28] + 2/3
  eor #$AF                ; [30] + 2
NoEorA
  sta RANDOM              ; [32] + 3

  ; Play Title Screen Music
  jmp PlayTitleMusic      ; [35] + 3
ResumeTitleMusic

  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  
  bit SLOWCYCLE           ; [5] + 3
  bvc HiScorePhase        ; [8] + 2/3
  bpl TitleScreenPhase    ; [10] + 2/3
  jmp ShowCredits         ; [12] + 3
HiScorePhase
  jmp HiScoreTable        ; [11] + 3
TitleScreenPhase
  jmp ShowTitleScreen     ; [13] + 3

EndHiScores
  jsr NextFrameMusic
  jmp TitlesLoop

; -----------------------------------------------------------------------------
; HISCORE UPDATE
; -----------------------------------------------------------------------------

EndOfGame
  ; Debounce (assume fire is still pressed)
  ldx #KEYSHORT|%10000000
  stx DEBOUNCE

  ; Store Game Type
  lda LINES+1
  and #%00110000
  lsr
  lsr
  lsr
  lsr
  sta TABLE

  ; Read Hi Score Table
  ; lda TABLE
  ldx #2
  jsr DoOperation
  
  IF PLUSROM

  jsr ReadHscTableFromBuffer
  jmp FinishHiScores

ReadHscTableFromBuffer
  ldx #0
  lda ReceiveBufferSize
  beq FinishHiScores
FillRAMFromInternet
  lda ReceiveBuffer
  sta HI_WRITE,x
  inx
  lda ReceiveBufferSize
  bne FillRAMFromInternet
  rts

  ELSE
  ; Check For Empty Score
  lda SCORE+2
  ora SCORE+1
  ora SCORE+0
  beq FinishHiScores
  
  ; Calculate Position
  ldx #MAXSCORES-1
FindPositionLoop
  ldy ScoreOffsets,X        ; [0] + 4
  lda SCORE+2               ; [4] + 3
  cmp HI_READ+2,Y           ; [7] + 4
  bcc NextScorePosition     ; [11] + 2/3  <
  bne StoreScorePosition    ; [13] + 2/3  >
  lda SCORE+1               ; [15] + 3
  cmp HI_READ+1,Y           ; [18] + 4
  bcc NextScorePosition     ; [22] + 2/3  <
  bne StoreScorePosition    ; [24] + 2/3  >
  lda SCORE+0               ; [26] + 3
  cmp HI_READ+0,Y           ; [29] + 4
  bcc NextScorePosition     ; [33] + 2/3  <
StoreScorePosition
  stx POSITION              ; [0] + 3
  ; Don't Copy In Lowest Position
  cpx #0                    ; [3] + 2
  beq NameEntry             ; [5] + 2/3
  ; Move Entries Down 
  ldx #0                    ; [7] + 2
  stx ROW                   ; [9] + 3 = 12
CopyLoop
  ; ldx ROW
  lda ScoreOffsets,X        ; [0] + 4
  ldy ScoreOffsets+1,X      ; [4] + 4
  tax                       ; [8] + 2
  lda HI_READ+0,Y           ; [10] + 4
  sta HI_WRITE+0,X          ; [14] + 4
  lda HI_READ+1,Y           ; [18] + 4
  sta HI_WRITE+1,X          ; [22] + 4
  lda HI_READ+2,Y           ; [26] + 4
  sta HI_WRITE+2,X          ; [30] + 4
  lda HI_READ+3,Y           ; [34] + 4
  sta HI_WRITE+3,X          ; [38] + 4
  lda HI_READ+4,Y           ; [42] + 4
  sta HI_WRITE+4,X          ; [46] + 4
  lda HI_READ+5,Y           ; [50] + 4
  sta HI_WRITE+5,X          ; [54] + 4
  ldx ROW                   ; [58] + 3
  inx                       ; [61] + 2
  cpx POSITION              ; [63] + 3
  bcs NameEntry             ; [66] + 2/3
  stx ROW                   ; [69] + 3
  jmp CopyLoop              ; [72] + 3 = 75
NextScorePosition
  dex                       ; [36] + 2
  bpl FindPositionLoop      ; [38] + 2/3
                            ; WORST CASE = 41*8 + 40 + 12 + 8*75 
                            ;            = 980 (APPROX 13 SCANLINES)
  ENDIF

FinishHiScores
  ; Debounce Fire Button
  ldx #KEYSHORT|%10000000
  stx DEBOUNCE
  ; Show HiScore Table
  ldx #191
  stx SLOWCYCLE
  ; Resume Title Screen
  jmp RestartTitles

NameEntry
  ; Reflect PF and Set Ball Size
  ldx #%00100001
  stx CTRLPF

  ; Reset Cursor Position
  ldx #0
  stx CURSOR
  
  ; Unpack Stored Initials
  lax INITIALS+0
  and #%00011111
  sta NAME+0
  txa
  and #%11100000
  lsr
  lsr
  lsr
  sta TEMP
  lax INITIALS+1
  and #%00000011
  ora TEMP
  sta NAME+1
  txa
  and #%01111100
  lsr
  lsr
  sta NAME+2

NameLoop
  ; Update Game Cycle
  dec CYCLE

  ; Play High Score SFX
  ldx #>HSoundTab
  stx SPTR+1
  lax SFX
  beq EndHSounds
  and #%00011111
  tay
  dey
  bpl PlayHSound
  ldx #0
  stx SFX
  beq EndHSounds
PlayHSound
  sty TEMP
  txa
  and #%11100000
  ora TEMP
  sta SFX
  lsr
  lsr
  lsr
  lsr
  lsr
  tax
  lda HSoundTab,X
  sta SPTR
  lda (SPTR),Y
  sta AUDF1
  ldx #5
  stx AUDC1
EndHSounds
  stx AUDV1
  
  ; Move Cursor With Joystick
  lax DEBOUNCE
  and #%10000000
  sta TEMP
  txa
  and #%01111111
  beq CheckNameJoy
  sec
  sbc #1
  ora TEMP
  sta DEBOUNCE
  jmp EndCheckNameJoy
CheckNameJoy
  ldx CURSOR
  ldy NAME,X
  bit SWCHA
  bpl CursorRight
  bvc CursorLeft
  jmp CheckCursorDown
CursorLeft
  dex
  bpl StoreCursor
  ldx #2
  bne StoreCursor
CursorRight
  inx
  cpx #3
  bcc StoreCursor
  ldx #0
  beq StoreCursor
CheckCursorDown
  lda #%00100000
  and SWCHA
  bne CheckCursorUp
  dey
  jmp StoreName
CheckCursorUp
  lda #%00010000
  and SWCHA
  bne EndCheckNameJoy
  iny
StoreName
  tya
  and #%00011111
  sta NAME,X
  stx CURSOR
  ldx #((2<<5)|3)
  stx SFX
  jmp DebounceNameJoy
StoreCursor
  stx CURSOR
  ldx #((1<<5)|3)
  stx SFX
DebounceNameJoy
  ldx #KEYSHORT
  txa
  ora TEMP
  sta DEBOUNCE
EndCheckNameJoy

  ; Check If Fire Pressed
CheckNameFire
  lda INPT4
  bmi ResetNameFire
  lda DEBOUNCE
  bmi EndCheckNameFire
  ; Debounce Fire
  lda DEBOUNCE
  ora #%10000000
  sta DEBOUNCE
  ; Copy Score Into Table
  ldx POSITION
  ldy ScoreOffsets,X
  lda SCORE+0
  sta HI_WRITE+0,Y
  lda SCORE+1
  sta HI_WRITE+1,Y
  lda SCORE+2
  sta HI_WRITE+2,Y
  ; Copy Name Into Table
  jsr PackName
  lda INITIALS+0
  sta HI_WRITE+3,Y
  lda INITIALS+1
  and #%01111111
  sta HI_WRITE+4,Y
  ; Copy Level Into Table
  lda LEVEL
  and #%00011111
  sta HI_WRITE+5,Y
  ; Write Score Table
  lda TABLE
  ldx #3
  jsr DoOperation
  jmp FinishHiScores
ResetNameFire
  lda DEBOUNCE
  and #%01111111
  sta DEBOUNCE
EndCheckNameFire
  
  ; Start Screen
  jsr StartScreen
  ldy #40
  jsr SkipLines
  
  ; Show New HiScore Message
  ldx #WHITE
  stx COLUP1
  stx COLUP0
  jsr PositionText
  ldy #(3*12)
  jsr LoadText
  jsr NormalTextKernel

  ; Display Last Score
  ldx TABLE
  lda BaseCols,X
  ora #$0E
  sta COLUP1
  sta COLUP0
  
  ; Create Text Showing Score and Table Position
  ldx POSITION
  lda ScoreLabelsLo,X
  sta HTEXT0
  lda ScoreLabelsHi,X
  sta HTEXT1
  ldx <#HL_DOT
  stx HTEXT2
  stx HTEXT3
  stx HTEXT4
  ldx <#HL_GAP
  stx HTEXT5
  ldy #<SCORE
  jsr CopyScore
  jsr NormalTextCopy
  jsr NormalTextKernel
  ldy #15
  jsr SkipLines

  ; Show Enter Name Message
  ldx #WHITE
  stx COLUP1
  stx COLUP0
  ldy #(4*12)
  jsr LoadText
  jsr NormalTextKernel

  ; Set Cursor Colour
  lda #BLUE
  sta NAMECOL+0
  sta NAMECOL+1
  sta NAMECOL+2
  lda CYCLE
  lsr
  and #%00001111
  tax
  lda ScoreSlowFade,X
  ldy CURSOR
  sta NAMECOL,Y

  ; Show Name
  ldy #4
  jsr SkipLines
  jsr NameKernel
  ldy #12
  jsr SkipLines

  ; Set Fire Message Colour
  lda CYCLE
  lsr
  lsr
  and #%00001111
  tax
  clc
  lda ScoreSlowFade,X
  IF (PALMODE)
  adc #$60
  ELSE
  adc #$40
  ENDIF
  sta COLUP0
  sta COLUP1

  ; Show Fire To Save Message
  ldy #(5*12)
  jsr PositionText
  jsr LoadText
  jsr NormalTextKernel

  ; Skip to Next Frame
  jsr NextFrame
  jmp NameLoop

; -----------------------------------------------------------------------------
; HIGH SCORE TABLE
; -----------------------------------------------------------------------------

HiScoreTable
  ; Set Heading Text Colours
  ldx #WHITE
  stx COLUP1              ; [14] + 3
  stx COLUP0              ; [17] + 3
  
  ; Load Heading Text
  jsr PositionText        ; [20] + 6
  ldx TABLE               ; [26] + 3
  ldy ScoreHeadings,X     ; [29] + 4
  jsr LoadText            ; [33] + 6

  ; Start Screen
  START_FRAME_MUSIC       ; [8]
  
  ; Show Heading
  jsr NormalTextKernel    ; [11]

  ; Adjust Sprite Positions
  jsr PositionHiScore     ; [14]

  ; Set Lines For Score Table
  ldx #MAXSCORES-1        ; [14] + 2
  stx ROW                 ; [16] + 3

  ; Get Base Colour
  ldx TABLE               ; [19] + 3
  lda BaseCols,X          ; [22] + 4
  sta TEMP2               ; [24] + 3

  ; Show Coloured Score Table
ScoreTableLoop
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  
  ; Set Score Colour
  lax ROW                 ; [5] + 3
  clc                     ; [8] + 2
  adc SLOWCYCLE           ; [10] + 3
  and #%00000111          ; [13] + 2
  tay                     ; [15] + 2
  clc                     ; [17] + 2
  lda ScoreSlowFade,Y     ; [19] + 4
  adc TEMP2               ; [23] + 3
  sta COLUP0              ; [26] + 3
  sta COLUP1              ; [29] + 3

  ; Show Score Position (1 to 10)
  ldy #<HL_GAP            ; [32] + 2
  sty HTEXT2              ; [34] + 3
  lda ScoreLabelsLo,X     ; [37] + 4
  sta HTEXT1              ; [41] + 3
  lda ScoreLabelsHi,X     ; [44] + 4
  sta HTEXT0              ; [48] + 3

  ; Copy Score
  ldy ScoreOffsets,X      ; [51] + 4
  lda HI_READ+$00,Y       ; [55] + 4
  sta HSCORE+0            ; [59] + 3
  lda HI_READ+$01,Y       ; [62] + 4
  sta HSCORE+1            ; [66] + 3
  
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  lda HI_READ+$02,Y       ; [5] + 4
  sta HSCORE+2            ; [9] + 3
  sty TEMP                ; [12] + 3
  ldy #<HSCORE            ; [15] + 2
  jsr CopyScore           ; [17] + 6
  
  ; Copy Name
  ldy TEMP                ; [67] + 3
  lda HI_READ+$03,Y       ; [70] + 4
  and #%00011111          ; [74] + 2
  tax                     ; [0] + 2
  
  PLAY_MUSIC              ; [2] + 5
  
  lda ScoreLetters,X      ; [7] + 4
  sta HTEXT3              ; [11] + 3
  lda HI_READ+$03,Y       ; [13] + 4
  and #%11100000          ; [17] + 2
  lsr                     ; [19] + 2
  lsr                     ; [21] + 2
  lsr                     ; [23] + 2
  sta TEMP                ; [25] + 3
  lda HI_READ+$04,Y       ; [28] + 4
  and #%00000011          ; [32] + 2
  ora TEMP                ; [34] + 3
  tax                     ; [37] + 2
  lda ScoreLetters,X      ; [39] + 4
  sta HTEXT4              ; [43] + 3
  lda HI_READ+$04,Y       ; [46] + 4
  and #%01111100          ; [50] + 2
  lsr                     ; [52] + 2
  lsr                     ; [54] + 2
  tax                     ; [56] + 2
  lda ScoreLetters,X      ; [58] + 4
  sta HTEXT5              ; [62] + 3
  
  ; Copy Level Number
  lda HI_READ+$05,Y       ; [65] + 4
  tay                     ; [69] + 2
  
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  lax HiScoreBCDTable,Y   ; [5] + 4
  lsr                     ; [9] + 2
  lsr                     ; [11] + 2
  lsr                     ; [13] + 2
  lsr                     ; [15] + 2
  tay                     ; [17] + 2
  lda HiScoreDigits,Y     ; [19] + 4
  sta HTEXT12             ; [23] + 3
  txa                     ; [26] + 2
  and #%00001111          ; [28] + 2
  tay                     ; [30] + 2
  lda HiScoreDigits,Y     ; [32] + 4
  sta HTEXT13             ; [36] + 3

  ; Show Line
  jsr HiScoreTextCopy     ; [39] + 6
  jsr HiScoreKernel       ; [66] + 6
  dec ROW                 ; [11] + 5
  bmi EndScoreTableLoop   ; [16] + 2/3
  jmp ScoreTableLoop      ; [18] + 3
  
EndScoreTableLoop
  jmp EndHiScores         ; [19] + 3

; -----------------------------------------------------------------------------
; EEPROM OPERATIONS
; -----------------------------------------------------------------------------

  ; X = Operation, A = Index
  ; Operations:
  ; 1 = Read Tune
  ; 2 = Read Score Table
  ; 3 = Write Score Table
  ; 4 = Wipe Scores
DoOperation
  IF PLUSROM
  cpx #2               ; check for Read Score operation
  bne EndOperation
  lda TABLE
  sta WriteToBuffer
  lda #HIGHSCORE_ID
  sta WriteSendBuffer  ; send highscore_id request to receive HS table from backend
  ELSE
  ; Store Operation & Index
  stx TEMP
  asl
  asl
  asl
  asl
  ora TEMP
  sta OPERATION_W

  ; Initialise Timeout Counter
  ldx #RETRYOP
  stx TIMEOUT
  ENDIF

TimeoutLoop
  ; Do Operation
  jmp DoStall
Resume
  ; Set Vblank Timer
  ldx #VBLANKDELAY
  stx TIM64T

  ; Reset Sprites
  ldx #0
  stx REFP0
  stx REFP1
  stx GRP0
  stx GRP1
  stx GRP0

  ; Skip To Next Frame
  jsr StartScreen
  jsr NextFrame

  ; Check Error And Retry
  IF (MELODY)
  lda ERRORCODE_R
  beq EndOperation
  dec TIMEOUT
  bpl TimeoutLoop
  ENDIF
EndOperation
  rts

; -----------------------------------------------------------------------------
; WIPE HISCORE TABLES
; -----------------------------------------------------------------------------

WipeHiScores
  ; Initialise Prompt
  ldx #0
  stx ROW

  ; Reset Music
  stx AUDV0
  sta TUNERESET_W

HiScoreWipeLoop
  ; Check Joystick
  lax DEBOUNCE
  and #%10000000
  sta TEMP
  txa
  and #%01111111
  beq CheckWipeJoy
  sec
  sbc #1
  ora TEMP
  sta DEBOUNCE
  jmp EndCheckWipeJoy

  ; Change Selection If Joystick Moves Up or Down
CheckWipeJoy
  lda SWCHA
  and #%00110000
  eor #%00110000
  beq CheckWipeFire
  ; Invert Selection
  lda ROW
  eor #%11111111
  sta ROW
  ; Set Debounce
  ldx #KEYWAIT
  stx DEBOUNCE
  jmp EndCheckWipeJoy
  
  ; Check If Fire Pressed
CheckWipeFire
  lda INPT4
  bmi EndCheckWipeJoy
  ; Set Debounce
  ldx #KEYWAIT|%10000000
  stx DEBOUNCE
  ; Check Result
  lda ROW
  beq EndWipeScores
WipeScores
  lda #0
  ldx #4
  jsr DoOperation
EndWipeScores
  jmp FinishStartup
EndCheckWipeJoy

  ; Reflect PF and Set PF Colour
  ldx #%00000001
  stx CTRLPF
  ldx #RED
  stx COLUPF

  ; Set Message Colour
  ldx #WHITE
  stx COLUP1
  stx COLUP0

  ; Position Sprites and Load First Message
  jsr PositionText
  ldy #(0*12)
  jsr LoadText

  ; Start Screen and Show First Message
  jsr StartScreen
  ldy #55
  jsr SkipLines
  jsr NormalTextKernel
  ldy #27
  jsr SkipLines

  ; Load "Wipe" Message
  ldy #(1*12)
  jsr LoadText
  
  ; Show Wipe Message
  sta WSYNC
  ldx #%11111000
  lda ROW
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
  ldy #6
  jsr SkipLines

  ; Load "Ignore" Message
  ldy #(2*12)
  jsr LoadText
  
  ; Show Ignore Message
  sta WSYNC
  ldx #%11111000
  lda ROW
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
  jmp HiScoreWipeLoop

; -----------------------------------------------------------------------------
; HISCORE SUBROUTINES
; -----------------------------------------------------------------------------

  ; Copy text Into Buffer For Display (Combining Pairs Of Characters)
HiScoreTextCopy
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  ldx HTEXT13             ; [5] + 3
  ldy HTEXT12             ; [8] + 3
  lda HRChars+4,X         ; [11] + 4
  ora HLChars+4,Y         ; [15] + 4
  sta HBUFF+34            ; [19] + 3
  lda HRChars+3,X         ; [22] + 4
  ora HLChars+3,Y         ; [26] + 4
  sta HBUFF+33            ; [30] + 3
  lda HRChars+2,X         ; [33] + 4
  ora HLChars+2,Y         ; [37] + 4
  sta HBUFF+32            ; [41] + 3
  lda HRChars+1,X         ; [44] + 4
  ora HLChars+1,Y         ; [48] + 4
  sta HBUFF+31            ; [52] + 3
  lda HRChars+0,X         ; [55] + 4
  ora HLChars+0,Y         ; [59] + 4
  sta HBUFF+30            ; [63] + 3
NormalTextCopy
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  ldx HTEXT11             ; [5] + 3
  ldy HTEXT10             ; [8] + 3
  lda HRChars+4,X         ; [11] + 4
  ora HLChars+4,Y         ; [15] + 4
  sta HBUFF+29            ; [19] + 3
  lda HRChars+3,X         ; [22] + 4
  ora HLChars+3,Y         ; [26] + 4
  sta HBUFF+28            ; [30] + 3
  lda HRChars+2,X         ; [33] + 4
  ora HLChars+2,Y         ; [37] + 4
  sta HBUFF+27            ; [41] + 3
  lda HRChars+1,X         ; [44] + 4
  ora HLChars+1,Y         ; [48] + 4
  sta HBUFF+26            ; [52] + 3
  lda HRChars+0,X         ; [55] + 4
  ora HLChars+0,Y         ; [59] + 4
  sta HBUFF+25            ; [63] + 3
  ldx HTEXT9              ; [66] + 3
  ldy HTEXT8              ; [69] + 3
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  lda HRChars+4,X         ; [5] + 4
  ora HLChars+4,Y         ; [9] + 4
  sta HBUFF+24            ; [13] + 3
  lda HRChars+3,X         ; [16] + 4
  ora HLChars+3,Y         ; [20] + 4
  sta HBUFF+23            ; [24] + 3
  lda HRChars+2,X         ; [27] + 4
  ora HLChars+2,Y         ; [31] + 4
  sta HBUFF+22            ; [35] + 3
  lda HRChars+1,X         ; [38] + 4
  ora HLChars+1,Y         ; [42] + 4
  sta HBUFF+21            ; [46] + 3
  lda HRChars+0,X         ; [49] + 4
  ora HLChars+0,Y         ; [53] + 4
  sta HBUFF+20            ; [57] + 3
  ldx HTEXT7              ; [60] + 3
  ldy HTEXT6              ; [63] + 3
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  lda HRChars+4,X         ; [5] + 4
  ora HLChars+4,Y         ; [9] + 4
  sta HBUFF+19            ; [13] + 3
  lda HRChars+3,X         ; [16] + 4
  ora HLChars+3,Y         ; [20] + 4
  sta HBUFF+18            ; [24] + 3
  lda HRChars+2,X         ; [27] + 4
  ora HLChars+2,Y         ; [31] + 4
  sta HBUFF+17            ; [35] + 3
  lda HRChars+1,X         ; [38] + 4
  ora HLChars+1,Y         ; [42] + 4
  sta HBUFF+16            ; [46] + 3
  lda HRChars+0,X         ; [49] + 4
  ora HLChars+0,Y         ; [53] + 4
  sta HBUFF+15            ; [57] + 3
  ldx HTEXT5              ; [60] + 3
  ldy HTEXT4              ; [63] + 3
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  lda HRChars+4,X         ; [5] + 4
  ora HLChars+4,Y         ; [9] + 4
  sta HBUFF+14            ; [13] + 3
  lda HRChars+3,X         ; [16] + 4
  ora HLChars+3,Y         ; [20] + 4
  sta HBUFF+13            ; [24] + 3
  lda HRChars+2,X         ; [27] + 4
  ora HLChars+2,Y         ; [31] + 4
  sta HBUFF+12            ; [35] + 3
  lda HRChars+1,X         ; [38] + 4
  ora HLChars+1,Y         ; [42] + 4
  sta HBUFF+11            ; [46] + 3
  lda HRChars+0,X         ; [49] + 4
  ora HLChars+0,Y         ; [53] + 4
  sta HBUFF+10            ; [57] + 3
  ldx HTEXT3              ; [60] + 3
  ldy HTEXT2              ; [63] + 3
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  lda HRChars+4,X         ; [5] + 4
  ora HLChars+4,Y         ; [9] + 4
  sta HBUFF+9             ; [13] + 3
  lda HRChars+3,X         ; [16] + 4
  ora HLChars+3,Y         ; [20] + 4
  sta HBUFF+8             ; [24] + 3
  lda HRChars+2,X         ; [27] + 4
  ora HLChars+2,Y         ; [31] + 4
  sta HBUFF+7             ; [35] + 3
  lda HRChars+1,X         ; [38] + 4
  ora HLChars+1,Y         ; [42] + 4
  sta HBUFF+6             ; [46] + 3
  lda HRChars+0,X         ; [49] + 4
  ora HLChars+0,Y         ; [53] + 4
  sta HBUFF+5             ; [57] + 3
  ldx HTEXT1              ; [60] + 3
  ldy HTEXT0              ; [63] + 3
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  lda HRChars+4,X         ; [5] + 4
  ora HLChars+4,Y         ; [9] + 4
  sta HBUFF+4             ; [13] + 3
  lda HRChars+3,X         ; [16] + 4
  ora HLChars+3,Y         ; [20] + 4
  sta HBUFF+3             ; [24] + 3
  lda HRChars+2,X         ; [27] + 4
  ora HLChars+2,Y         ; [31] + 4
  sta HBUFF+2             ; [35] + 3
  lda HRChars+1,X         ; [38] + 4
  ora HLChars+1,Y         ; [42] + 4
  sta HBUFF+1             ; [46] + 3
  lda HRChars+0,X         ; [49] + 4
  ora HLChars+0,Y         ; [53] + 4
  sta HBUFF+0             ; [57] + 3
  rts                     ; [60] + 6

; Copy Score (Score Start Pointer Is In Y)
CopyScore
  lda $02,Y               ; [23] + 4
  lsr                     ; [27] + 2
  lsr                     ; [29] + 2
  lsr                     ; [31] + 2
  lsr                     ; [33] + 2
  tax                     ; [35] + 2
  lda HiScoreDigits,X     ; [37] + 4
  sta HTEXT6              ; [41] + 3
  lda $02,Y               ; [44] + 4
  and #%00001111          ; [48] + 2
  tax                     ; [50] + 2
  lda HiScoreDigits,X     ; [52] + 4
  sta HTEXT7              ; [56] + 3
  lda $01,Y               ; [59] + 4
  lsr                     ; [63] + 2
  lsr                     ; [65] + 2
  lsr                     ; [67] + 2
  lsr                     ; [69] + 2
  tax                     ; [71] + 2
  PLAY_MUSIC              ; [74] + 5
  lda HiScoreDigits,X     ; [3] + 4
  sta HTEXT8              ; [7] + 3
  lda $01,Y               ; [10] + 4
  and #%00001111          ; [14] + 2
  tax                     ; [16] + 2
  lda HiScoreDigits,X     ; [18] + 4
  sta HTEXT9              ; [22] + 3
  lda $00,Y               ; [25] + 4
  lsr                     ; [29] + 2
  lsr                     ; [31] + 2
  lsr                     ; [33] + 2
  lsr                     ; [35] + 2
  tax                     ; [37] + 2
  lda HiScoreDigits,X     ; [39] + 4
  sta HTEXT10             ; [43] + 3
  lda $00,Y               ; [46] + 4
  and #%00001111          ; [50] + 2
  tax                     ; [52] + 2
  lda HiScoreDigits,X     ; [54] + 4
  sta HTEXT11             ; [58] + 3
  rts                     ; [61] + 6 = 67

  ; Pack NAME Into 2 Bytes 
PackName
  lda INITIALS+1
  and #%10000000
  sta TEMP2               ; Preserve 2600/7800 Bit
  lda NAME+0
  sta TEMP
  lax NAME+1
  and #%00011100
  asl
  asl
  asl
  ora TEMP
  sta INITIALS+0
  txa
  and #%00000011
  sta TEMP
  lda NAME+2
  asl
  asl
  ora TEMP
  ora TEMP2
  sta INITIALS+1
  rts
  
  DC.B  "AA0"

DoStall
  ; Initialise & Reposition Stall Sprites
  ldx #1
StallPos
  sta WSYNC               ; [0]
  ldy #0                  ; [0] + 2
  sty GRP0                ; [2] + 3
  sty GRP1                ; [5] + 3
  sty VDELP0              ; [8] + 3
  sty VDELP1              ; [11] + 3
  sty NUSIZ0              ; [14] + 3
  sty NUSIZ1              ; [17] + 3
  sty COLUPF              ; [20] + 3
  sty PF0                 ; [23] + 3
  sty PF1                 ; [26] + 3
  sty PF2                 ; [29] + 3
  ldy #WHITE              ; [32] + 2
  sty COLUP1              ; [34] + 3
  ldy #RED                ; [37] + 2
  sty COLUP0              ; [39] + 3
  sta RESP0,X             ; [42] + 4 = 46
  dex
  bpl StallPos
  jmp CopyCodeStall



; -----------------------------------------------------------------------------
; STARTUP CODE
; -----------------------------------------------------------------------------

  ALIGN 256

Start
  IF (MELODY)
  ldy INITIALS+1          ; Preserve 7800 bit
  ldx #0
  txa
  cld
ClearMemLoop
  dex
  txs
  pha
  bne ClearMemLoop        ; SP=$FF, X = A = 0
  sty INITIALS+1
  ELSE

  ; Check for 7800 Console (needed for pause button handling)
  sei
  ldy #$FF
  lda $D0
  cmp #$2C
  bne Not7800
  lda $D1
  cmp #$A9
  bne Not7800
  iny                     ; 7800 - bit7 = 0
Not7800                   ; 2600 - bit7 = 1

  ; Clear Memory
  ldx #0
  txa
  cld
ClearMemLoop
  dex
  txs
  pha
  bne ClearMemLoop        ; SP=$FF, X = A = 0

  ; Store 7800 Console Bit
  tya
  and #%10000000
  sta INITIALS+1
  ENDIF

  ; Clear Hiscore Table
  CLEAR_HISCORES

  ; Load Startup Tune
  lda #0
  ldx #1
  jsr DoOperation

  ; Set Music Tempo
  ldx #MUSICTEMPO
  stx MUSIC
  
  ; Startup Delay
  lda #(25*5)             ; Tune Length = 25
  sta CYCLE

StartupLoop
  sta WSYNC
  PLAY_MUSIC

  ; Update Game Cycle
  dec CYCLE               ; [11] + 5
  beq FinishStartup       ; [16] + 2/3

  ; Play Startup Music
  dec MUSIC               ; [18] + 5
  bpl SkipTuneUpdate      ; [23] + 2/3
  sta TUNESTEP_W          ; [25] + 3
  nop                     ; [28] + 2
  ldx #MUSICTEMPO         ; [30] + 2
  stx MUSIC               ; [32] + 3
SkipTuneUpdate

  ; Check For Fire/Wipe Scores
  lda INPT4               ; [35] + 3
  bmi StartupKernel       ; [38] + 2/3
  ldx #KEYWAIT|%10000000 
  stx DEBOUNCE
  bit SWCHA
  bmi FinishStartup
  jmp WipeHiScores

FinishStartup
  ; Silence Music
  lda #0
  sta MUSIC
  sta AUDV0
  sta TUNERESET_W
  ; Read Hiscore Table
  ; lda #0
  sta TABLE
  ldx #2
  jsr DoOperation
  jsr ReadHscTableFromBuffer

  ; Skip To Title Screen
  jmp ShowTitles

StartupKernel
  ; Set Sprite Positions
  jsr PositionText

  ; Begin Frame
  START_FRAME_MUSIC       ; [14]

  ; Non-Reflected Playfield & Black Sprites
  ldx #0                  ; [14] + 2
  stx CTRLPF              ; [16] + 3
  stx COLUP0              ; [19] + 3
  stx COLUP1              ; [22] + 3
  
  ; Skip 86 Lines
  ldy #86                 ; [25] + 2
StartupSkipLoop
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; Blank Screen and Set Playfield
  ldx #%00110010          ; [5] + 2
  stx VBLANK              ; [7] + 3
  stx ENAM0               ; [10] + 3
  stx PF0                 ; [13] + 3
  ldx #%11111100          ; [16] + 2
  stx PF2                 ; [18] + 3

  ; Logo Colours
  ldx #AABLUE             ; [21] + 2
  stx COLUPF              ; [23] + 3
  ldx #AAORANGE           ; [26] + 2
  stx COLUBK              ; [28] + 3

  dey                     ; [31] + 2
  bpl StartupSkipLoop     ; [33] + 2/3
  jmp AAShowLogo          ; [35] + 3

  ; Skip Overscan (Music)
NextFrameMusic
  START_OVERSCAN_MUSIC
  WAIT_OVERSCAN_MUSIC
  START_VBLANK_MUSIC
  rts

  DC.B  "AA1"

; -----------------------------------------------------------------------------
; HISCORE DISPLAY KERNELS & SUBROUTINES
; -----------------------------------------------------------------------------

  ALIGN 256

AAShowLogo
  ; First Logo Sprites
  ldy #AAHEIGHT           ; [38] + 2
  sty POSITION            ; [40] + 3
  lda AALogo0,Y           ; [43] + 4
  sta GRP0                ; [47] + 3
  ldx AALogo1,Y           ; [50] + 4

  ; Begin Logo Display
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  stx.w GRP1              ; [5] + 4
  jmp AALogoStart         ; [9] + 3

AALogoLoop
  ldy POSITION            ; [65] + 3
  lda AALogo0,Y           ; [68] + 4
  sta.w GRP0              ; [72] + 4
  PLAY_MUSIC              ; [0] + 5
  lda AALogo1,Y           ; [5] + 4
  sta GRP1                ; [9] + 3  < 42
AALogoStart
  lda AALogo2,Y           ; [12] + 4
  sta GRP0                ; [16] + 3  < 44
  ldx AALogo4,Y           ; [19] + 4
  lda AALogo5,Y           ; [23] + 4
  sta TEMP                ; [27] + 3
  lda AALogo3,Y           ; [30] + 4
  ldy #0                  ; [34] + 2
  sty VBLANK              ; [36] + 3  = 39
  ldy TEMP                ; [39] + 3
  sta GRP1                ; [42] + 3  > 44 < 47
  stx GRP0                ; [45] + 3  > 46 < 50
  sty GRP1                ; [48] + 3  > 49 < 52
  sta GRP0                ; [51] + 3  > 52 < 55
  stx VBLANK              ; [54] + 3  = 57
  dec POSITION            ; [57] + 5
  bpl AALogoLoop          ; [62] + 2/3
  ldx #0                  ; [64] + 2
  stx GRP0                ; [66] + 3
  stx GRP1                ; [69] + 3
  stx GRP0                ; [72] + 3
  PLAY_MUSIC              ; [75] + 5
  stx ENAM0               ; [4] + 3
  stx PF0                 ; [7] + 3
  stx PF2                 ; [10] + 3
  stx COLUBK              ; [13] + 3
EndAALogoLoop
  jsr NextFrameMusic
  jmp StartupLoop

  ; Copy Text into ZP RAM (Y Contains Message Offset)
LoadText
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  lda HiScoreMsg+11,Y     ; [5] + 4
  sta HTEXT11             ; [9] + 3
  lda HiScoreMsg+10,Y     ; [12] + 4
  sta HTEXT10             ; [16] + 3
  lda HiScoreMsg+9,Y      ; [19] + 4
  sta HTEXT9              ; [23] + 3
  lda HiScoreMsg+8,Y      ; [26] + 4
  sta HTEXT8              ; [30] + 3
  lda HiScoreMsg+7,Y      ; [33] + 4
  sta HTEXT7              ; [37] + 3
  lda HiScoreMsg+6,Y      ; [40] + 4
  sta HTEXT6              ; [44] + 3
  lda HiScoreMsg+5,Y      ; [47] + 4
  sta HTEXT5              ; [51] + 3
  lda HiScoreMsg+4,Y      ; [54] + 4
  sta HTEXT4              ; [58] + 3
  lda HiScoreMsg+3,Y      ; [61] + 4
  sta HTEXT3              ; [65] + 3
  lda HiScoreMsg+2,Y      ; [68] + 4
  sta HTEXT2              ; [72] + 3
  PLAY_MUSIC              ; [75] + 5
  lda HiScoreMsg+1,Y      ; [4] + 4
  sta HTEXT1              ; [8] + 3
  lda HiScoreMsg+0,Y      ; [11] + 4
  sta HTEXT0              ; [15] + 3
  jmp NormalTextCopy      ; [18] + 3
EndLoadText

  ; AtariAge Logo
  AALOGOA

  DC.B  "AA2"

  ALIGN   256
  
NameKernel
  sta WSYNC                 ; [0]
  sta HMCLR                 ; [0] + 3
  ldx #%11110001            ; [3] + 2
  stx HMP0                  ; [5] + 3
  stx NUSIZ0                ; [8] + 3
  ldx #0                    ; [11] + 2
  stx NUSIZ1                ; [13] + 3
  stx VDELP0                ; [16] + 3
  stx VDELP1                ; [19] + 3
  ldx #>HLChars             ; [22] + 2
  stx HPTR0+1               ; [24] + 3
  stx HPTR1+1               ; [27] + 3
  stx HPTR2+1               ; [30] + 3
  ldx NAME+0                ; [33] + 3
  ldy ScoreLetters,X        ; [36] + 4
  sta.w RESP0               ; [40] + 4 = 44 EXACT
  sta RESP1                 ; [44] + 3 = 47 EXACT
  sty HPTR0                 ; [47] + 3
  ldx NAME+1                ; [50] + 3
  ldy ScoreLetters,X        ; [53] + 4
  sty HPTR1                 ; [57] + 3
  ldx NAME+2                ; [60] + 3
  ldy ScoreLetters,X        ; [63] + 4
  sty HPTR2                 ; [67] + 3
  sta WSYNC                 ; [0]
  sta HMOVE                 ; [0] + 3
  ldy #4                    ; [3] + 2
NameKernelLoop
  sta WSYNC                 ; [0]
  lda NAMECOL+0             ; [0] + 3
  sta COLUP0                ; [3] + 3   < 46
  lda (HPTR0),Y             ; [6] + 5
  sta GRP0                  ; [11] + 3  < 46
  lda NAMECOL+1             ; [14] + 3
  sta COLUP1                ; [17] + 3  < 48
  lda (HPTR1),Y             ; [20] + 5
  sta GRP1                  ; [25] + 3  < 48
  ldx NAMECOL+2             ; [28] + 3
  lda (HPTR2),Y             ; [31] + 5
  SLEEP 7                   ; [36] + 7
  dey                       ; [43] + 2
  sta GRP0                  ; [45] + 3  > 48 < 52
  stx COLUP0                ; [48] + 3  > 48 < 52
  bpl NameKernelLoop
  sta WSYNC
  iny
  sty GRP1
  sty GRP0
  sty GRP1
  rts
EndNameKernel
  if (>NameKernel != >EndNameKernel)
    echo "WARNING: Name Kernel Crosses Page Boundary!"
  endif

PositionHiScore
  ; Position Sprites
  sta WSYNC                 ; [0]
  PLAY_MUSIC                ; [0] + 5
  ; Set 3 sprite copies (medium) and no delay
  ldx #%00000110            ; [5] + 2
  stx NUSIZ0                ; [7] + 3
  stx VDELP0                ; [10] + 3
  stx VDELP1                ; [13] + 3
  ; Set 3 sprite copies (close)
  ldx #%00000011            ; [16] + 2
  stx NUSIZ1                ; [18] + 3
  ; Position Sprites
  ldx #%11100000            ; [21] + 2
  stx HMP0                  ; [23] + 3
  nop                       ; [26] + 2
  sta RESP0                 ; [28] + 3  = 31
  stx HMP1                  ; [31] + 3
  nop                       ; [34] + 2
  sta RESP1                 ; [36] + 3  = 39
  sta WSYNC                 ; [0]
  sta HMOVE                 ; [0] + 3
  PLAY_MUSIC                ; [3] + 5
  rts                       ; [8] + 6 = 14
EndPositionHiScore
  if (>PositionHiScore != >EndPositionHiScore)
    echo "WARNING: PositionHiScore Crosses Page Boundary!"
  endif

PositionText
  sta WSYNC                 ; [0]
  PLAY_MUSIC                ; [0] + 5
  ; Set 3 Copies Close and Delay
  ldx #%00110011            ; [5] + 2
  stx VDELP0                ; [7] + 3
  stx VDELP1                ; [10] + 3
  stx NUSIZ0                ; [13] + 3
  stx NUSIZ1                ; [16] + 3
  ; Coarse Position
  sta HMCLR                 ; [19] + 3
  sta.w RESM0               ; [22] + 4  = 26
  ldx #%11100000            ; [26] + 2
  stx HMP0                  ; [28] + 3
  stx HMM0                  ; [31] + 3
  ldx #%11110000            ; [34] + 2
  sta RESP0                 ; [36] + 3  = 39
  sta RESP1                 ; [39] + 3  = 42
  stx HMP1                  ; [42] + 3
  ; Fine Position
  sta WSYNC                 ; [0]
  sta HMOVE                 ; [0] + 3
  PLAY_MUSIC                ; [3] + 5
  rts                       ; [8] + 6 = 14
EndPositionText
  if (>PositionText != >EndPositionText)
    echo "WARNING: PositionText Crosses Page Boundary!"
  endif

  ; Jump To Start Of Screen
StartScreen
  START_FRAME
  rts
  
  ; Skip Screen Lines
SkipLines
  sta WSYNC
  dey
  bpl SkipLines
  rts

  DC.B  "AA3"

  ALIGN   256

  ; Display Hi Score Table Line (Unrolled)
HiScoreKernel
  sta WSYNC                 ; [0]
  PLAY_MUSIC                ; [0] + 5
  lda #%00000011            ; [5] + 2
  sta NUSIZ1                ; [7] + 3
  lda HBUFF+4               ; [10] + 3
  sta GRP0                  ; [13] + 3   < 34
  lda HBUFF+19              ; [16] + 3
  sta GRP1                  ; [19] + 3  < 42
  ldx HBUFF+14              ; [22] + 3
  ldy HBUFF+34              ; [25] + 3
  lda HBUFF+24              ; [28] + 3
  SLEEP 3                   ; [31] + 3
  sta GRP0                  ; [34] + 3  > 36 < 44
  nop                       ; [37] + 2
  lda HBUFF+29              ; [39] + 3
  sta GRP1                  ; [42] + 3  > 44 < 47
  lda HBUFF+9               ; [45] + 3
  sta GRP1                  ; [48] + 3  > 49 < 52 
  stx GRP0                  ; [51] + 3  > 46 < 55
  lda #%00000110            ; [54] + 2
  sta NUSIZ1                ; [56] + 3  > 54 < 63
  sty GRP1                  ; [59] + 3  > 54 < 63
  sta WSYNC                 ; [0]
  PLAY_MUSIC                ; [0] + 5
  lda #%00000011            ; [5] + 2
  sta NUSIZ1                ; [7] + 3
  lda HBUFF+3               ; [10] + 3
  sta GRP0                  ; [13] + 3   < 34
  lda HBUFF+18              ; [16] + 3
  sta GRP1                  ; [19] + 3  < 42
  ldx HBUFF+13              ; [22] + 3
  ldy HBUFF+33              ; [25] + 3
  lda HBUFF+23              ; [28] + 3
  SLEEP 3                   ; [31] + 3
  sta GRP0                  ; [34] + 3  > 36 < 44
  nop                       ; [37] + 2
  lda HBUFF+28              ; [39] + 3
  sta GRP1                  ; [42] + 3  > 44 < 47
  lda HBUFF+8               ; [45] + 3
  sta GRP1                  ; [48] + 3  > 49 < 52 
  stx GRP0                  ; [51] + 3  > 46 < 55
  lda #%00000110            ; [54] + 2
  sta NUSIZ1                ; [56] + 3  > 54 < 63
  sty GRP1                  ; [59] + 3  > 54 < 63
  sta WSYNC                 ; [0]
  PLAY_MUSIC                ; [0] + 5
  lda #%00000011            ; [5] + 2
  sta NUSIZ1                ; [7] + 3
  lda HBUFF+2               ; [10] + 3
  sta GRP0                  ; [13] + 3   < 34
  lda HBUFF+17              ; [16] + 3
  sta GRP1                  ; [19] + 3  < 42
  ldx HBUFF+12              ; [22] + 3
  ldy HBUFF+32              ; [25] + 3
  lda HBUFF+22              ; [28] + 3
  SLEEP 3                   ; [31] + 3
  sta GRP0                  ; [34] + 3  > 36 < 44
  nop                       ; [37] + 2
  lda HBUFF+27              ; [39] + 3
  sta GRP1                  ; [42] + 3  > 44 < 47
  lda HBUFF+7               ; [45] + 3
  sta GRP1                  ; [48] + 3  > 49 < 52 
  stx GRP0                  ; [51] + 3  > 46 < 55
  lda #%00000110            ; [54] + 2
  sta NUSIZ1                ; [56] + 3  > 54 < 63
  sty GRP1                  ; [59] + 3  > 54 < 63
  sta WSYNC                 ; [0]
  PLAY_MUSIC                ; [0] + 5
  lda #%00000011            ; [5] + 2
  sta NUSIZ1                ; [7] + 3
  lda HBUFF+1               ; [10] + 3
  sta GRP0                  ; [13] + 3   < 34
  lda HBUFF+16              ; [16] + 3
  sta GRP1                  ; [19] + 3  < 42
  ldx HBUFF+11              ; [22] + 3
  ldy HBUFF+31              ; [25] + 3
  lda HBUFF+21              ; [28] + 3
  SLEEP 3                   ; [31] + 3
  sta GRP0                  ; [34] + 3  > 36 < 44
  nop                       ; [37] + 2
  lda HBUFF+26              ; [39] + 3
  sta GRP1                  ; [42] + 3  > 44 < 47
  lda HBUFF+6               ; [45] + 3
  sta GRP1                  ; [48] + 3  > 49 < 52 
  stx GRP0                  ; [51] + 3  > 46 < 55
  lda #%00000110            ; [54] + 2
  sta NUSIZ1                ; [56] + 3  > 54 < 63
  sty GRP1                  ; [59] + 3  > 54 < 63
  sta WSYNC                 ; [0]
  PLAY_MUSIC                ; [0] + 5
  lda #%00000011            ; [5] + 2
  sta NUSIZ1                ; [7] + 3
  lda HBUFF+0               ; [10] + 3
  sta GRP0                  ; [13] + 3   < 34
  lda HBUFF+15              ; [16] + 3
  sta GRP1                  ; [19] + 3  < 42
  ldx HBUFF+10              ; [22] + 3
  ldy HBUFF+30              ; [25] + 3
  lda HBUFF+20              ; [28] + 3
  SLEEP 3                   ; [31] + 3
  sta GRP0                  ; [34] + 3  > 36 < 44
  nop                       ; [37] + 2
  lda HBUFF+25              ; [39] + 3
  sta GRP1                  ; [42] + 3  > 44 < 47
  lda HBUFF+5               ; [45] + 3
  sta GRP1                  ; [48] + 3  > 49 < 52 
  stx GRP0                  ; [51] + 3  > 46 < 55
  lda #%00000110            ; [54] + 2
  sta NUSIZ1                ; [56] + 3  > 54 < 63
  sty GRP1                  ; [59] + 3  > 54 < 63
  lda #0                    ; [62] + 2
  sta GRP0                  ; [64] + 3  > 57
  sta GRP1                  ; [67] + 3  > 65
  sta GRP0                  ; [70] + 3
  SLEEP 3                   ; [73] + 3
  PLAY_MUSIC                ; [0] + 5
  rts                       ; [5] + 6 = 11
EndHiScoreKernel
  if (>HiScoreKernel != >EndHiScoreKernel)
    echo "WARNING: HiScoreKernel Crosses Page Boundary!"
  endif

  ; Sound Effects (Lower = Higher Priority)
  ; 1 = Select
  ; 2 = Move
HSoundTab
  DC.B  0, <HSelectSnd, <HMoveSnd
HSelectSnd
  DC.B  31, 31, 31, 31
HMoveSnd
  DC.B  24, 24, 24, 24

  DC.B  "AA4"
  
; -----------------------------------------------------------------------------
; HISCORE DATA
; -----------------------------------------------------------------------------

  ALIGN 256

  ; HiScore Message (156 Entries)
HiScoreMsg
  ; Wipe Scores
  DC.B  <HL_WW, <HL_II, <HL_PP, <HL_EE, <HL_GAP, <HL_SS
  DC.B  <HL_CC, <HL_OO, <HL_RR, <HL_EE, <HL_SS, <HL_QMARK             ; 0
  ; Wipe
  DC.B  <HL_GAP, <HL_GAP, <HL_GAP, <HL_WW, <HL_II, <HL_PP
  DC.B  <HL_EE, <HL_BANG, <HL_BANG, <HL_GAP, <HL_GAP, <HL_GAP         ; 1
  ; Ignore
  DC.B  <HL_GAP, <HL_GAP, <HL_GAP, <HL_II, <HL_GG, <HL_NN
  DC.B  <HL_OO, <HL_RR, <HL_EE, <HL_GAP, <HL_GAP, <HL_GAP             ; 2
  ; New Hiscore
  DC.B  <HL_NN, <HL_EE, <HL_WW, <HL_GAP, <HL_HH, <HL_II
  DC.B  <HL_SS, <HL_CC, <HL_OO, <HL_RR, <HL_EE, <HL_BANG              ; 3
  ; Enter Name
  DC.B  <HL_GAP, <HL_EE, <HL_NN, <HL_TT, <HL_EE, <HL_RR
  DC.B  <HL_GAP, <HL_NN, <HL_AA, <HL_MM, <HL_EE, <HL_GAP              ; 4
  ; Fire To Save
  DC.B  <HL_FF, <HL_II, <HL_RR, <HL_EE, <HL_GAP, <HL_TT
  DC.B  <HL_OO, <HL_GAP, <HL_SS, <HL_AA, <HL_VV, <HL_EE               ; 5
  ; A - Marathon
  DC.B  <HL_GAP, <HL_GAP, <HL_MM, <HL_AA, <HL_RR, <HL_AA
  DC.B  <HL_TT, <HL_HH, <HL_OO, <HL_NN, <HL_GAP, <HL_GAP              ; 6
  ; B - Sprint 25
  DC.B  <HL_GAP, <HL_SS, <HL_PP, <HL_RR, <HL_II, <HL_NN
  DC.B  <HL_TT, <HL_GAP, <HL_GAP, <HL_22, <HL_55, <HL_GAP             ; 7
  ; C - Sprint 40
  DC.B  <HL_GAP, <HL_SS, <HL_PP, <HL_RR, <HL_II, <HL_NN
  DC.B  <HL_TT, <HL_GAP, <HL_GAP, <HL_44, <HL_00, <HL_GAP             ; 8
  ; D - Ultra
  DC.B  <HL_GAP, <HL_GAP, <HL_GAP, <HL_UU, <HL_LL, <HL_TT
  DC.B  <HL_RR, <HL_AA, <HL_BANG, <HL_GAP, <HL_GAP, <HL_GAP           ; 9
EndHiScoreMsg

  ; Binary To BCD Table (100 Entries)
HiScoreBCDTable
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
EndHiScoreBCDTable

  ; Jump To Next Frame
NextFrame
  START_OVERSCAN
  WAIT_OVERSCAN
  START_VBLANK
  rts

  ; DC.B  "AA5"

  ALIGN   256

  ; Small Font LHS
  HLEFTCHARS

  ; Score Letters Table (32 Entries)
ScoreLetters
  DC.B  <HL_AA, <HL_BB, <HL_CC, <HL_DD, <HL_EE, <HL_FF, <HL_GG, <HL_HH
  DC.B  <HL_II, <HL_JJ, <HL_KK, <HL_LL, <HL_MM, <HL_NN, <HL_OO, <HL_PP
  DC.B  <HL_QQ, <HL_RR, <HL_SS, <HL_TT, <HL_UU, <HL_VV, <HL_WW, <HL_XX
  DC.B  <HL_YY, <HL_ZZ, <HL_DOT, <HL_BANG
  DC.B  <HL_QMARK, <HL_DASH, <HL_COLON, <HL_GAP

  ; Score Digits Table (30 Entries)
HiScoreDigits
  DC.B  <HL_00, <HL_11, <HL_22, <HL_33, <HL_44
  DC.B  <HL_55, <HL_66, <HL_77, <HL_88, <HL_99
ScoreLabelsLo
  DC.B  <HL_00, <HL_99, <HL_88, <HL_77, <HL_66
  DC.B  <HL_55, <HL_44, <HL_33, <HL_22, <HL_11
ScoreLabelsHi
  DC.B  <HL_11, <HL_GAP, <HL_GAP, <HL_GAP, <HL_GAP
  DC.B  <HL_GAP, <HL_GAP, <HL_GAP, <HL_GAP, <HL_GAP

  ; HiScore Table Colours
BaseCols
  IF (PALMODE)
  DC.B  $60, $40, $50, $20
  ELSE
  DC.B  $40, $30, $C0, $10
  ENDIF

  ; Score Tables
ScoreOffsets
  DC.B  54, 48, 42, 36, 30, 24, 18, 12, 6, 0
  
  ; HiScore Table Headings (into HiScoreMsg)
ScoreHeadings
  DC.B  6*12, 7*12, 8*12, 9*12

  ; Score Fast Fade (8 Entries)
ScoreFastFade
  DC.B  $02, $04, $08, $0A, $0E, $0A, $08, $04

  DC.B  "AA6"

  ALIGN 256

  ; Small Font RHS
  HRIGHTCHARS

  ; AtariAge Logo
  AALOGOB
  
    ; Score Slow Fade (16 Entries)
ScoreSlowFade
  DC.B  $00, $02, $04, $06, $08, $0A, $0C, $0E
  DC.B  $0E, $0C, $0A, $08, $06, $04, $02, $00
  
  ALIGN 256

NormalTextKernel
  ldx #4
  stx TEMP2
  ldy HBUFF+0,X
  jmp HiScoreTextStart
HiScoreTextLoop
  ldx TEMP2               ; [62] + 3
  ldy HBUFF+0,X           ; [65] + 4                  0A 0B 1A 1B
HiScoreTextStart
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  nop                     ; [5] + 2
  sty GRP0                ; [7] + 3                  0 - - -
  lda HBUFF+5,X           ; [10] + 4
  sta GRP1                ; [14] + 3  < 42            - 0 1 0
  lda HBUFF+10,X          ; [17] + 4
  sta GRP0                ; [21] + 3  < 44            2 0 - 1
  ldy HBUFF+20,X          ; [24] + 4
  lda HBUFF+25,X          ; [28] + 4
  sta TEMP                ; [32] + 3
  lda HBUFF+15,X          ; [35] + 4
  ldx TEMP                ; [39] + 3
  sta GRP1                ; [42] + 3  > 44 < 47       - 2 3 1
  sty GRP0                ; [45] + 3  > 46 < 50       4 2 - 3
  stx GRP1                ; [48] + 3  > 49 < 52       - 4 5 3
  sta GRP0                ; [51] + 3  > 52 < 55       - 4 - 5
  dec TEMP2               ; [54] + 5
  bpl HiScoreTextLoop     ; [59] + 2/3
  ldx #0                  ; [61] + 2
  stx GRP1                ; [63] + 3
  stx GRP0                ; [66] + 3
  stx GRP1                ; [69] + 3
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  rts                     ; [5] + 6 = 11
EndNormalTextKernel
  if (>NormalTextKernel != >EndNormalTextKernel)
    echo "WARNING: NormalTextKernel Crosses Page Boundary!"
  endif

; -----------------------------------------------------------------------------
; STALL CODE
; -----------------------------------------------------------------------------

CopyCodeStall
  ; Copy Stall Code
  START_FRAME
  ldx #(EndStall - RAMSPINNER)
CopyStall
  lda $FF80,X
  sta RAMSPINNER,X
  dex
  bpl CopyStall

  ; Begin Stall
  START_OVERSCAN
  WAIT_OVERSCAN
  jmp StartStall

  echo "----",($FF80 - *) , "bytes left (BANK 7 - HIGH SCORES)"

  ORG   $FF80
  RORG  RAMSPINNER

  ; Spinner Data
Spinner
  DC.B %00001111
  DC.B %00001111
  DC.B %00001111
  DC.B %00001111
  DC.B %11111111
  DC.B %11111111
  DC.B %11111111
  DC.B %11111111
  DC.B %11110000
  DC.B %11110000
  DC.B %11110000
  DC.B %11110000

  ; Spinner Loop
SpriteLoop
SpriteOffset
  lda Spinner,X
  sta GRP0
  dex
  sta WSYNC
  sta WSYNC
  bpl SpriteLoop

  ; End Of Frame
  IF (PALMODE)
  ldx #127
  ELSE
  ldx #115
  ENDIF
  stx VBLANK
LastLoop
  dex
  sta WSYNC
  bpl LastLoop
  stx GRP1

  ; Entry Point
StartStall
  ; Do Vertical Sync
  lda #%00001110
SyncLoop
  sta WSYNC
  sta VSYNC
  lsr
  bne SyncLoop

  ; Check End of Stall
  IF (MELODY)
  bit $1FF4
  bvc FinishStall
  ELSE
  IF (FAKESPIN)
  cpy #0
  beq FinishStall
  nop
  ELSE
  IF PLUSROM
  lda ReceiveBufferSize
  bne FinishStall               ; can fail on PlusCart, because ReceiveBufferSize > 0 doesn't mean request has been fully received
  ELSE
  jmp FinishStall
  nop
  nop
  ENDIF
  ENDIF
  ENDIF

  ; Calculate Spinner Frame
  dey
  sty REFP0
  tya
  lsr
  lsr
  and #%00000100
  ora #RAMSPINNER
  sta SpriteOffset+1

  ; Start Of Frame
  IF (PALMODE)
  ldx #164
  ELSE
  ldx #126
  ENDIF
FirstLoop
  dex
  sta WSYNC
  bne FirstLoop
  stx VBLANK

  ; Draw Sprite
  ldx #7
  bne SpriteLoop

  ; Resume
FinishStall
  jmp ($FFFE)
EndStall                  ; Leave Space For Stack!

  ; echo "----",($100 - *) , " bytes left (BANK 7 - STALL)"

  ; Switch Points (BANK 3 & 4)
  ORG     $FFE9
  RORG    $FFE9
  jmp     Start
  jmp     EndOfGame
  ORG     $FFF4
  RORG    $FFF4
  DC.B    "CHETIR"
  DC.W    (PlusROM_API - $D000)
  DC.W    Start, Resume
  
