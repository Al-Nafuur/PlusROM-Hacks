; -----------------------------------------------------------------------------
; CHETIRY - Atari 2600 - (C) Copyright 2011 - AtariAge
; BANK 2 - GAME KERNEL
; -----------------------------------------------------------------------------

; Block Colours (Bit 1 Must Be Set)
  IF (PALMODE)            ; PAL
BCOL0      =  $02         ; Grey
BCOL1      =  $7A         ; Cyan
BCOL2      =  $D6         ; Blue
BCOL3      =  $46         ; Orange
BCOL4      =  $2A         ; Yellow
BCOL5      =  $56         ; Green
BCOL6      =  $AA         ; Magenta
BCOL7      =  $66         ; Red
BCOL8      =  $0E         ; White
  ELSE                    ; NTSC
BCOL0      =  $02         ; Grey
BCOL1      =  $BA         ; Cyan
BCOL2      =  $82         ; Blue
BCOL3      =  $36         ; Orange
BCOL4      =  $1A         ; Yellow
BCOL5      =  $C6         ; Green
BCOL6      =  $56         ; Magenta
BCOL7      =  $42         ; Red
BCOL8      =  $0E         ; White
  ENDIF

  ; Self Modifying Kernel (42 bytes)
  MAC KERNEL
Modify
Col1
  lda #$FF                ; [17] + 2
  sta COLUPF              ; [19] + 3
Col2
  lda #$FF                ; [22] + 2
  sta COLUP0              ; [24] + 3
Col3
  lda #$FF                ; [27] + 2
  sta COLUP1              ; [29] + 3
Col4
  lda #$FF                ; [32] + 2
  php                     ; [34] + 3  = 37
  sta COLUBK              ; [37] + 3  >= 40 <= 48
Col5Y
  sty COLUPF              ; [40] + 3  > 42 < 51
Col6
  lda #$FF                ; [43] + 2
  sta COLUP0              ; [45] + 3  > 45 < 54
Col7
  lda #$FF                ; [48] + 2
  sta COLUP1              ; [50] + 3  >=48 <= 56
Col8
  lda #$FF                ; [53] + 2
  sta COLUBK              ; [55] + 3  > 50 < 59
Col9X
  stx COLUPF              ; [58] + 3  > 53 < 62
  stx VBLANK              ; [61] + 3  = 64
JPtr
  jmp Modify              ; [64] + 3
  ENDM

; BANK 2
  SEG     BANK2
  ORG     $A000
  RORG    $F000

  ; Space for DPC registers
  DS.B    256,0

Init2
  ; Switch to Bank 7
  nop     $FFFB
  nop
  nop
  nop
KernelExit
  ; Switch To Bank 1
  nop     $FFF5
  ; Begin Score Kernel
  jmp     ScoreKernelSetup
  ; Begin Game
  jmp     GameSetup
StartGame
  ; Switch To Bank 1
  nop     $FFF5

; -----------------------------------------------------------------------------
; SPECIAL TUNE
; -----------------------------------------------------------------------------

LoadSpecialTune
  ; Resume Pointers
  ldx #<ResumeSpecial
  stx RESUME
  ldx #>ResumeSpecial
  stx RESUME+1

  ; Load Tune 4
  ldx #((4<<4)|1)
  stx OPERATION_W
  jmp DoMusicStall
ResumeSpecial

  ; Set Vblank Timer
  ldx #VBLANKDELAY
  stx TIM64T

  ; Reset Audio
  ldx #0
  stx AUDV0
  stx AUDV1
  stx SFX
  stx TUNERESET_W

  ; Reset Sprites
  stx REFP0
  stx REFP1
  stx GRP0
  stx GRP1
  stx GRP0

  ; Set Tune Tempo
  ldx #MUSICTEMPO
  stx MUSIC
  
  ; Skip To Next Frame
  START_FRAME
  START_OVERSCAN
  WAIT_OVERSCAN
  jmp ShowSpecial

; -----------------------------------------------------------------------------
; GAME SETUP CODE
; -----------------------------------------------------------------------------

GameSetup
  ; Reset Score
  ldx #0
  stx SCORE+0
  stx SCORE+1
  stx SCORE+2
  
  ; Fire Button Pressed
  ldx #%00100000
  stx DEBOUNCE

  ; Initialise Cycle Counter
  ldx #CYCLETIME
  stx CYCLE

  ; Game Setup
  lda TMPGAME
  cmp #3
  beq StartUltra
  cmp #2
  beq StartSprint40
  cmp #1
  beq StartSprint25
StartMarathon
  ldx #0                  ; 0 Lines
  ldy #0
  jmp StoreGame
StartSprint25
  ldx #%00100101          ; 25 Lines (BCD)
  ldy #1<<4
  jmp StoreGame
StartSprint40
  ldx #%01000000          ; 40 Lines (BCD)
  ldy #2<<4
  jmp StoreGame
StartUltra
  ldx #%10000000          ; 180 Seconds (BCD)
  ldy #%00110001
StoreGame
  stx LINES+0
  sty LINES+1

  ; Set Starting Level
  lda TMPHEIGHT
  asl
  sta YPOS               ; Depth Temp
  asl
  asl
  asl
  asl
  ora TMPLEVEL
  sta LEVEL

  ; Set Music Type
  clc
  lda TMPMUSIC
  adc #1                  ; Music = Selection + 1
  and #%00000011          ; Position 4 = Silence (Maps to Zero)
  asl
  asl
  asl
  asl
  asl
  sta MUSIC
  
  ; Load Music
  lax MUSIC
  beq ResumeStartup       ; Skip For Silence
  lsr
  ora #%00000001
  sta OPERATION_W
  txa
  ora #MUSICTEMPO
  sta MUSIC
  ldx #<ResumeStartup
  stx RESUME
  ldx #>ResumeStartup
  stx RESUME+1
  jmp DoMusicStall
ResumeStartup

  ; Set Vblank Timer
  ldx #VBLANKDELAY
  stx TIM64T

  ; Reset Audio
  ldx #0
  stx AUDV0
  stx AUDV1
  stx SFX
  sta TUNERESET_W

  ; Reset Sprites
  stx REFP0
  stx REFP1
  stx GRP0
  stx GRP1
  stx GRP0
  
  ; Clear Well
  ldx #71
  ldy #0
ClearWellLoop
  sty GRID,X
  dex
  bpl ClearWellLoop

  ; Begin Frame
  START_FRAME

  ; Fill Well With Random Blocks (YPOS = Row)
  dec YPOS
  bmi EndHeight
HeightOuterLoop
  ldx #9
  stx TEMPY               ; TEMPY = Column
HeightInnerLoop
  jsr GetRandom2
  lsr
  and #%01110000
  bcs SkipVal
  sta TEMPVAL
  jsr MakeValues
  ldx TEMPY
  lda YPOS
  jsr SetData
SkipVal
  dec TEMPY
  bpl HeightInnerLoop
  dec YPOS
  bpl HeightOuterLoop
EndHeight

  START_OVERSCAN
  WAIT_OVERSCAN
  START_VBLANK
  jmp StartGame

   ; Binary To BCD Table (100 Entries)
BCDTable:
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
EndBCDTable
  if (>BCDTable != >EndBCDTable)
    echo "WARNING: BCD Table Cross Page Boundary!"
  endif

  DC.B  "STALIN"

; -----------------------------------------------------------------------------
; GAME MESSAGE KERNEL SETUP
; -----------------------------------------------------------------------------

  ALIGN 256

  ; Setup Game Messages (X = Colour Offset, Y = Message Offset)
GameMessageSetup
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  ; Set 3 Copies Close and Delay
  lda #%00000011          ; [5] + 2
  sta VDELP0              ; [7] + 3
  sta VDELP1              ; [10] + 3
  sta NUSIZ0              ; [13] + 3
  sta NUSIZ1              ; [16] + 3
  ; Position Sprites (Fine)
  sta HMCLR               ; [19] + 3
  lda #%00010000          ; [22] + 2
  sta HMP1                ; [24] + 3
  ; Set Message High Pointers
  lda #>MChars            ; [27] + 2
  sta GPTR0+1             ; [29] + 3
  sta GPTR1+1             ; [32] + 3
  sta GPTR2+1             ; [35] + 3
  ; Position Sprites (Coarse)
  sta RESP0               ; [38] + 3    = 41
  sta RESP1               ; [41] + 3    = 44
  ; Remaining High Pointers
  sta GPTR3+1             ; [44] + 3
  sta GPTR4+1             ; [47] + 3
  sta GPTR5+1             ; [50] + 3
  ; Message Pointers
  lda GameMessages+0,Y    ; [53] + 4
  sta GPTR0               ; [57] + 3
  lda GameMessages+1,Y    ; [60] + 4
  sta GPTR1               ; [64] + 3
  lda GameMessages+2,Y    ; [67] + 4
  sta GPTR2               ; [71] + 3
  sta HMOVE               ; [74] + 3     > 74 < 4
  PLAY_MUSIC              ; [1] + 5
  lda GameMessages+3,Y    ; [6] + 4
  sta GPTR3               ; [10] + 3
  lda GameMessages+4,Y    ; [13] + 4
  sta GPTR4               ; [17] + 3
  lda GameMessages+5,Y    ; [20] + 4
  sta GPTR5               ; [24] + 3
  ; Fashing Message Colour
  ldy GameMessageCols,X   ; [27] + 4
  lda CYCLE               ; [31] + 3
  and #%00010000          ; [34] + 2
  beq ResetGameMessageCol ; [36] + 2/3
  bne StoreGameMessageCol ; [38] + 3
ResetGameMessageCol
  ldy #$0E                ; [39] + 2
StoreGameMessageCol
  sty COLUP0              ; [41] + 3
  sty COLUP1              ; [44] + 3
  ; Set Lines
  ldy #4                  ; [47] + 2
  sty GLINE               ; [49] + 3
  ; Preload First Lines
  lda (GPTR0),Y           ; [52] + 5
  sta GRP0                ; [57] + 3
  lax (GPTR1),Y           ; [60] + 5

  ; Wait For Frame Start
WaitSetupVblank
  PLAY_MUSIC
  lda INTIM
  bne WaitSetupVblank
  
  sta WSYNC               ; [0]
  sta VBLANK              ; [0] + 3
  PLAY_MUSIC              ; [3] + 5
  ; Start Display
  stx GRP1                ; [8] + 3
  jmp GameTextStart       ; [11] + 3
  ; Display Game Text
GameTextLoop
  ldy GLINE               ; [63] + 3
  lda (GPTR0),Y           ; [66] + 5
  sta GRP0                ; [71] + 3
  SLEEP 3                 ; [74] + 3
  PLAY_MUSIC              ; [1] + 5
  lda (GPTR1),Y           ; [6] + 5
  sta GRP1                ; [11] + 3    < 43
GameTextStart
  lda (GPTR2),Y           ; [14] + 5
  sta GRP0                ; [19] + 3    < 46
  lax (GPTR4),Y           ; [22] + 5
  lda (GPTR5),Y           ; [27] + 5
  sta TEMP                ; [32] + 3
  lda (GPTR3),Y           ; [35] + 5
  ldy TEMP                ; [40] + 3
  sta GRP1                ; [43] + 3    > 45 <= 48
  stx GRP0                ; [46] + 3    >= 48 < 51
  sty GRP1                ; [49] + 3    > 50 < 54
  sta GRP0                ; [52] + 3    > 53 <= 56
  dec GLINE               ; [55] + 5
  bpl GameTextLoop        ; [60] + 2/3
  ; Cleanup
  lda #0                  ; [62] + 2
  sta VDELP0              ; [64] + 3
  sta VDELP1              ; [67] + 3
  jmp GridSetup           ; [70] + 3
EndGameText
  if (>GameMessageSetup != >EndGameText)
    echo "WARNING: Game Message Kernel Crosses Page Boundary!"
  endif

  ; Shape Preview Data (16 Bytes)
Previews:
NoPreview:
  DC.B  %00000000
  DC.B  %00000000
IPreview:
  DC.B  %01110000
  DC.B  %00000000
JPreview:
  DC.B  %01110000
  DC.B  %00010000
LPreview:
  DC.B  %01110000
  DC.B  %01000000
OPreview:
  DC.B  %00100000
  DC.B  %00100000
SPreview:
  DC.B  %00110000
  DC.B  %01100000
TPreview:
  DC.B  %01110000
  DC.B  %00100000
ZPreview:
  DC.B  %01100000
  DC.B  %00110000
EndPreviews:
  if (>Previews != >EndPreviews)
    echo "WARNING: Previews crosses a page boundary!"
  endif
  
  DC.B  "BREZHNEV"

; -----------------------------------------------------------------------------
; SCORE KERNEL SETUP
; -----------------------------------------------------------------------------

  ALIGN 256

GameOverMessages
  bit TIMER               ; [13] + 3
  bmi SprintGameOver      ; [16] + 2/3
  bvs UltraGameOver       ; [18] + 2/3
  ldx #0                  ; [20] + 2
  ldy #0                  ; [22] + 2
  jmp GameMessageSetup    ; [24] + 3
SprintGameOver
  ldx #1                  ; [19] + 2
  ldy #6                  ; [21] + 2
  jmp GameMessageSetup    ; [23] + 3
UltraGameOver
  ldx #2                  ; [21] + 2
  ldy #12                 ; [23] + 2
  jmp GameMessageSetup    ; [25] + 3
PausedMessage
  ldx #3                  ; [18] + 2
  ldy #18                 ; [20] + 2
  jmp GameMessageSetup    ; [22] + 3

ScoreKernelSetup
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  
  ; Check for GameOver or Paused Modes
  lda LINES+1             ; [5] + 3
  cmp #%11000000          ; [8] + 2
  bcs GameOverMessages    ; [10] + 2/3
  lda CYCLE               ; [12] + 3
  bmi PausedMessage       ; [15] + 2/3
  
  ; Preview on P0 Beginner Mode
  bit SWCHB               ; [17] + 3
  bvs SkipPreview         ; [20] + 2/3
  ; Shape Preview 
  lda TIMER               ; [22] + 3
  and #%11100000          ; [25] + 2
  tax                     ; [27] + 2
  ldy ColTable2,X         ; [29] + 4
  sty COLUPF              ; [33] + 3
  lsr                     ; [36] + 2
  lsr                     ; [38] + 2
  lsr                     ; [40] + 2
  lsr                     ; [42] + 2
  tax                     ; [44] + 2
  lda Previews,X          ; [46] + 4
  sta PFBUFF1             ; [50] + 3
  lda Previews+1,X        ; [53] + 4
  sta PFBUFF0             ; [57] + 3
SkipPreview

  ; Set Stack To Buffer
  ldx #SBUFF+29           ; [60] + 2
  txs                     ; [62] + 2
  
  ; Load Level Number
  lda LEVEL               ; [64] + 3
  and #%00011111          ; [67] + 2
  tay                     ; [69] + 2

  ; Play Music
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; Store Level Number
  lax BCDTable,Y          ; [5] + 4
  lsr                     ; [9] + 2
  lsr                     ; [11] + 2
  lsr                     ; [13] + 2
  lsr                     ; [15] + 2
  tay                     ; [17] + 2
  txa                     ; [19] + 2
  ldx ScoreDigits,Y       ; [21] + 4
  and #%00001111          ; [25] + 2
  tay                     ; [27] + 2
  lda ScoreDigits,Y       ; [29] + 4
  tay                     ; [33] + 2
  lda LChars+4,X          ; [35] + 4
  ora RChars+4,Y          ; [39] + 4
  pha                     ; [43] + 3
  lda LChars+3,X          ; [46] + 4
  ora RChars+3,Y          ; [50] + 4
  pha                     ; [54] + 3
  lda LChars+2,X          ; [57] + 4
  ora RChars+2,Y          ; [61] + 4
  pha                     ; [65] + 3
  nop                     ; [68] + 2
  PLAY_MUSIC              ; [70] + 5
  lda LChars+1,X          ; [75] + 4
  ora RChars+1,Y          ; [3] + 4
  pha                     ; [7] + 3
  lda LChars+0,X          ; [10] + 4
  ora RChars+0,Y          ; [14] + 4
  pha                     ; [18] + 3
  
  ; Load Lower Lines Digits
  lda LINES+0             ; [21] + 3
  lsr                     ; [24] + 2
  lsr                     ; [26] + 2
  lsr                     ; [28] + 2
  lsr                     ; [30] + 2
  tax                     ; [32] + 2
  ldy ScoreDigits,X       ; [34] + 4
  lda LINES+0             ; [38] + 3
  and #%00001111          ; [41] + 2
  tax                     ; [43] + 2
  lda ScoreDigits,X       ; [45] + 4
  tax                     ; [49] + 2
  lda LChars+4,Y          ; [51] + 4
  ora RChars+4,X          ; [55] + 4
  pha                     ; [59] + 3
  lda LChars+3,Y          ; [62] + 4
  ora RChars+3,X          ; [66] + 4
  pha                     ; [70] + 3
  nop                     ; [73] + 2
  PLAY_MUSIC              ; [75] + 5
  lda LChars+2,Y          ; [4] + 4
  ora RChars+2,X          ; [8] + 4
  pha                     ; [12] + 3
  lda LChars+1,Y          ; [15] + 4
  ora RChars+1,X          ; [19] + 4
  pha                     ; [23] + 3
  lda LChars+0,Y          ; [26] + 4
  ora RChars+0,X          ; [30] + 4
  pha                     ; [34] + 3
  jmp LinesLevelScore     ; [37] + 3
EndScoreKernelSetup
  if (>ScoreKernelSetup != >EndScoreKernelSetup)
    echo "WARNING: Score Kernel Setup Crosses Page Boundary!"
  endif

  ; End Of Line Kernel
Line8_2
  lda #<GridLoop          ; [70] + 2
  sta JPtr+1              ; [72] + 3
  PLAY_MUSIC              ; [75] + 5
  pla                     ; [4] + 4
  lda TEMP2               ; [8] + 3
  sta COLUBK              ; [11] + 3
  jmp Modify              ; [14] + 3

  DC.B  "TROTSKY"

  ALIGN 256

LinesLevelScore
  ; Load Upper Lines Digit
  lda LINES+1             ; [40] + 3
  and #%00110000          ; [43] + 2
  lsr                     ; [45] + 2
  lsr                     ; [47] + 2
  lsr                     ; [49] + 2
  lsr                     ; [51] + 2
  tax                     ; [53] + 2
  ldy Letters,X           ; [55] + 4
  lda LINES+1             ; [59] + 3
  and #%00001111          ; [62] + 2
  tax                     ; [64] + 2
  lda ScoreDigits,X       ; [66] + 4
  tax                     ; [70] + 2
  PLAY_MUSIC              ; [72] + 5
  lda LChars+4,Y          ; [1] + 4
  ora RChars+4,X          ; [5] + 4
  pha                     ; [9] + 3
  lda LChars+3,Y          ; [12] + 4
  ora RChars+3,X          ; [16] + 4
  pha                     ; [20] + 3
  lda LChars+2,Y          ; [23] + 4
  ora RChars+2,X          ; [27] + 4
  pha                     ; [31] + 3
  lda LChars+1,Y          ; [34] + 4
  ora RChars+1,X          ; [38] + 4
  pha                     ; [42] + 3
  lda LChars+0,Y          ; [45] + 4
  ora RChars+0,X          ; [49] + 4
  pha                     ; [53] + 3

  ; Load Lower Score
  lda SCORE+0             ; [56] + 3
  lsr                     ; [59] + 2
  lsr                     ; [61] + 2
  lsr                     ; [63] + 2
  lsr                     ; [65] + 2
  tax                     ; [67] + 2
  ldy ScoreDigits,X       ; [69] + 4
  PLAY_MUSIC              ; [73] + 5
  lda SCORE+0             ; [2] + 3
  and #%00001111          ; [5] + 2
  tax                     ; [7] + 2
  lda ScoreDigits,X       ; [9] + 4
  tax                     ; [13] + 2
  lda LChars+4,Y          ; [15] + 4
  ora RChars+4,X          ; [19] + 4
  pha                     ; [23] + 3
  lda LChars+3,Y          ; [26] + 4
  ora RChars+3,X          ; [30] + 4
  pha                     ; [34] + 3
  lda LChars+2,Y          ; [37] + 4
  ora RChars+2,X          ; [41] + 4
  pha                     ; [45] + 3
  lda LChars+1,Y          ; [48] + 4
  ora RChars+1,X          ; [52] + 4
  pha                     ; [56] + 3
  lda LChars+0,Y          ; [59] + 4
  ora RChars+0,X          ; [63] + 4
  pha                     ; [67] + 3
  nop                     ; [70] + 2
  PLAY_MUSIC              ; [72] + 5

  ; Load Middle Score Digits
  lda SCORE+1             ; [1] + 3
  lsr                     ; [4] + 2
  lsr                     ; [6] + 2
  lsr                     ; [8] + 2
  lsr                     ; [10] + 2
  tax                     ; [12] + 2
  ldy ScoreDigits,X       ; [14] + 4
  lda SCORE+1             ; [18] + 3
  and #%00001111          ; [21] + 2
  tax                     ; [23] + 2
  lda ScoreDigits,X       ; [25] + 4
  tax                     ; [29] + 2
  lda LChars+4,Y          ; [31] + 4
  ora RChars+4,X          ; [35] + 4
  pha                     ; [39] + 3
  lda LChars+3,Y          ; [42] + 4
  ora RChars+3,X          ; [46] + 4
  pha                     ; [50] + 3
  lda LChars+2,Y          ; [53] + 4
  ora RChars+2,X          ; [57] + 4
  pha                     ; [61] + 3
  lda LChars+1,Y          ; [64] + 4
  ora RChars+1,X          ; [68] + 4
  pha                     ; [72] + 3
  nop                     ; [75] + 2
  PLAY_MUSIC              ; [1] + 5
  lda LChars+0,Y          ; [6] + 4
  ora RChars+0,X          ; [10] + 4
  pha                     ; [14] + 3

  ; Load Upper Score Digits
  lda SCORE+2             ; [17] + 3
  lsr                     ; [20] + 2
  lsr                     ; [22] + 2
  lsr                     ; [24] + 2
  lsr                     ; [26] + 2
  tax                     ; [28] + 2
  ldy ScoreDigits,X       ; [30] + 4
  lda SCORE+2             ; [34] + 3
  and #%00001111          ; [37] + 2
  tax                     ; [39] + 2
  lda ScoreDigits,X       ; [41] + 4
  tax                     ; [45] + 2
  lda LChars+4,Y          ; [47] + 4
  ora RChars+4,X          ; [51] + 4
  pha                     ; [55] + 3
  lda LChars+3,Y          ; [58] + 4
  ora RChars+3,X          ; [62] + 4
  pha                     ; [66] + 3
  nop                     ; [69] + 2
  PLAY_MUSIC              ; [71] + 5
  lda LChars+2,Y          ; [0] + 4
  ora RChars+2,X          ; [4] + 4
  pha                     ; [8] + 3
  lda LChars+1,Y          ; [11] + 4
  ora RChars+1,X          ; [15] + 4
  pha                     ; [19] + 3
  lda LChars+0,Y          ; [22] + 4
  ora RChars+0,X          ; [26] + 4
  pha                     ; [30] + 3

  ; Set Coarse Position
  sta RESP0               ; [33] + 3    = 36
  sta RESP1               ; [36] + 3    = 39

  ; Display Score
  lda #0                  ; [39] + 2
  jmp ScoreKernel         ; [41] + 3

EndLinesLevelScore
  if (>LinesLevelScore != >EndLinesLevelScore)
    echo "WARNING: Score Setup Crosses Page Boundary!"
  endif

  DC.B   "K0"

; -----------------------------------------------------------------------------
; SCORE KERNEL
; -----------------------------------------------------------------------------

  ALIGN 256

ScoreKernel
  ; Reset Sprite Movements
  sta HMCLR               ; [44] + 3

  ; Set Non-Reflected PF
  sta CTRLPF              ; [47] + 3

  ; Set P0 Three Copies Close & P1 Three Copies Medium
  lda #%00000011          ; [50] + 2
  sta NUSIZ0              ; [52] + 3
  asl                     ; [55] + 2
  sta NUSIZ1              ; [57] + 3

  ; Load Fine Position
  ldx #%00010000          ; [60] + 2
  stx HMP1                ; [62] + 3

  ; Set Score Sprite Colours
  ldx #$0E                ; [65] + 2
  stx COLUP0              ; [67] + 3
  stx COLUP1              ; [70] + 3

  ; Wait for End of VBlank Region
WaitMainVblank
  PLAY_MUSIC
  lda INTIM
  bne WaitMainVblank
  sta VBLANK

  sta WSYNC               ; [0]
  sta HMOVE               ; [0] + 3
  PLAY_MUSIC              ; [3] + 5     Line 0
  SLEEP 4                 ; [8] + 4
  lda #0                  ; [12] + 2
  sta PF1                 ; [14] + 3    <= 28
  lda SBUFF+4             ; [17] + 3
  sta GRP0                ; [20] + 3    < 38 (> 50)
  lda SBUFF+9             ; [23] + 3
  sta GRP1                ; [26] + 3    < 41 (>= 64)
  lda SBUFF+14            ; [29] + 3
  ldx SBUFF+19            ; [32] + 3
  ldy PFBUFF1             ; [35] + 3
  sta GRP0                ; [38] + 3    > 40 <= 43
  lda SBUFF+24            ; [41] + 3
  stx GRP0                ; [44] + 3    > 45 < 49
  sta GRP1                ; [47] + 3    >= 43 <= 50
  sty PF1                 ; [50] + 3    > 38 < 55
  lda SBUFF+29            ; [53] + 3
  sta GRP1                ; [56] + 3    > 53 < 62

  sta WSYNC               ; [0]         Line 1
  PLAY_MUSIC              ; [0] + 5
  SLEEP 7                 ; [5] + 7
  lda #0                  ; [12] + 2
  sta PF1                 ; [14] + 3    < = 28
  lda SBUFF+3             ; [17] + 3
  sta GRP0                ; [20] + 3    < 38 (> 50)
  lda SBUFF+8             ; [23] + 3
  sta GRP1                ; [26] + 3    <= 40 (>= 64)
  lda SBUFF+13            ; [29] + 3
  ldx SBUFF+18            ; [32] + 3
  ldy PFBUFF1             ; [35] + 3
  sta GRP0                ; [38] + 3    >= 40 < 43
  lda SBUFF+23            ; [41] + 3
  stx GRP0                ; [44] + 3    > 45 <= 48 
  sta GRP1                ; [47] + 3    > 41 < 51
  sty PF1                 ; [50] + 3    > 38 < 55
  lda SBUFF+28            ; [53] + 3
  sta GRP1                ; [56] + 3    > 53 < 62

  sta WSYNC               ; [0]         Line 2
  PLAY_MUSIC              ; [0] + 5
  SLEEP 10                ; [5] + 10
  ldy #0                  ; [15] + 2
  sty PF1                 ; [17] + 3    < = 28
  lda SBUFF+2             ; [20] + 3
  sta GRP0                ; [23] + 3    < 38 (> 50)
  lda SBUFF+7             ; [26] + 3
  sta GRP1                ; [29] + 3    <= 40 (>= 64)
  lda SBUFF+12            ; [32] + 3
  ldx SBUFF+17            ; [35] + 3
  sta GRP0                ; [38] + 3    >= 40 < 43
  lda SBUFF+22            ; [41] + 3
  stx GRP0                ; [44] + 3    > 45 <= 48 
  sta GRP1                ; [47] + 3    > 41 < 51
  sty PF1                 ; [50] + 3    > 38 < 55
  lda SBUFF+27            ; [53] + 3
  sta GRP1                ; [56] + 3    > 53 < 62

  sta WSYNC               ; [0]         Line 3
  PLAY_MUSIC              ; [0] + 5
  SLEEP 7                 ; [5] + 7
  lda #0                  ; [12] + 2
  sta PF1                 ; [14] + 3    < = 28
  lda SBUFF+1             ; [17] + 3
  sta GRP0                ; [20] + 3    < 38 (> 50)
  lda SBUFF+6             ; [23] + 3
  sta GRP1                ; [26] + 3    <= 40 (>= 64)
  lda SBUFF+11            ; [29] + 3
  ldx SBUFF+16            ; [32] + 3
  ldy PFBUFF0             ; [35] + 3
  sta GRP0                ; [38] + 3    >= 40 < 43
  lda SBUFF+21            ; [41] + 3
  stx GRP0                ; [44] + 3    > 45 <= 48 
  sta GRP1                ; [47] + 3    > 41 < 51
  sty PF1                 ; [50] + 3    > 38 < 55
  lda SBUFF+26            ; [53] + 3
  sta GRP1                ; [56] + 3    > 53 < 62

  sta WSYNC               ; [0]         Line 4
  PLAY_MUSIC              ; [0] + 5
  SLEEP 7                 ; [5] + 7
  lda #0                  ; [12] + 2
  sta PF1                 ; [14] + 3    < = 28
  lda SBUFF+0             ; [17] + 3
  sta GRP0                ; [20] + 3    < 38 (> 50)
  lda SBUFF+5             ; [23] + 3
  sta GRP1                ; [26] + 3    <= 40 (>= 64)
  lda SBUFF+10            ; [29] + 3
  ldx SBUFF+15            ; [32] + 3
  ldy PFBUFF0             ; [35] + 3
  sta GRP0                ; [38] + 3    >= 40 < 43
  lda SBUFF+20            ; [41] + 3
  stx GRP0                ; [44] + 3    > 45 <= 48 
  sta GRP1                ; [47] + 3    > 41 < 51
  sty PF1                 ; [50] + 3    > 38 < 55
  lda SBUFF+25            ; [53] + 3
  sta GRP1                ; [56] + 3    > 53 < 62

  ; Score Cleanup
  lda #0                  ; [59] + 2
  sta PF1                 ; [64] + 3
  sta COLUPF              ; [67] + 3
  jmp GridSetup           ; [70] + 3

  DC.B  "K1"

EndScoreKernel
  if (>ScoreKernel != >EndScoreKernel)
    echo "WARNING: Score Kernel Crosses Page Boundary!"
  endif

; -----------------------------------------------------------------------------
; GRID KERNEL SETUP
; -----------------------------------------------------------------------------

  ALIGN 256

GridSetup
  ; Play Audio
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  
  ; Clear Score Sprites
  ldy #0                  ; [5] + 2
  sty GRP0                ; [7] + 3
  sty GRP1                ; [10] + 3
  sty COLUP0              ; [13] + 3
  sty COLUP1              ; [16] + 3

  ; Kernel Setup
  ldy #$84                ; [19] + 2    STY
  sty KERNEL+17           ; [21] + 3
  iny                     ; [24] + 2    STA
  sty KERNEL+2            ; [26] + 3
  sty KERNEL+6            ; [29] + 3
  sty KERNEL+10           ; [32] + 3
  sty KERNEL+15           ; [35] + 3
  sty KERNEL+21           ; [38] + 3
  sty KERNEL+25           ; [41] + 3
  sty KERNEL+29           ; [44] + 3
  iny                     ; [47] + 2    STX
  sty KERNEL+31           ; [49] + 3
  sty KERNEL+33           ; [52] + 3
  ldy #COLUBK             ; [55] + 2    COLUBK
  sty KERNEL+16           ; [57] + 3
  sty KERNEL+30           ; [60] + 3
  ldy #COLUPF             ; [63] + 2    COLUPF
  sty KERNEL+3            ; [65] + 3
  sty KERNEL+18           ; [68] + 3
  sty KERNEL+32           ; [71] + 3
  
  ; Play Audio
  nop                     ; [74] + 2
  PLAY_MUSIC              ; [0] + 5
  
  ldy #COLUP0             ; [5] + 2     COLUP0
  sty KERNEL+7            ; [7] + 3
  sty KERNEL+22           ; [10] + 3
  ldy #COLUP1             ; [13] + 2    COLUP1
  sty KERNEL+11           ; [15] + 3
  sty KERNEL+26           ; [18] + 3
  ldy #VBLANK             ; [21] + 2    VBLANK
  sty KERNEL+34           ; [23] + 3
  ldy #$4C                ; [26] + 2    JMP
  sty KERNEL+35           ; [28] + 3
  ; Grid Kernel Setup
  ldy #$A9                ; [31] + 2    LDA
  sty KERNEL+0            ; [33] + 3
  sty KERNEL+4            ; [36] + 3
  sty KERNEL+8            ; [39] + 3
  sty KERNEL+12           ; [42] + 3
  sty KERNEL+19           ; [45] + 3
  sty KERNEL+23           ; [48] + 3
  sty KERNEL+27           ; [51] + 3
  ldy #$08                ; [54] + 2    PHP
  sty KERNEL+14           ; [56] + 3

  ; Set Jump Pointer High
  ldy #>GridLoop          ; [59] + 2
  sty JPtr+2              ; [61] + 3

  ; Clear Previous Sprite Movements
  sta HMCLR               ; [64] + 3

  ; Play Audio
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; Set Sprite Data
  ldy #$FF                ; [5] + 2
  sty GRP0                ; [7] + 3
  sty GRP1                ; [10] + 3
  ldy #2                  ; [13] + 2
  sty ENABL               ; [15] + 3

  ; Set PF Data
  ldy #%00000110          ; [18] + 2
  sty PF2                 ; [20] + 3
  ldy #%01100000          ; [23] + 2
  sty PF0                 ; [25] + 3
  ldy #%00000110          ; [28] + 2
  sty PF1                 ; [30] + 3
  
  ; Set Stack Pointer
  ldx #VBLANK-1           ; [33] + 2
  txs                     ; [35] + 2

  ; Position Sprites
  sta.w RESP0             ; [37] + 4    = 41
  sta RESP1               ; [41] + 3    = 44
  ldy #%00010000          ; [44] + 2
  sty HMP1                ; [46] + 3
  sty HMBL                ; [49] + 3

  ; Set Two Sprite Copies
  ldy #%00000010          ; [52] + 2
  sty NUSIZ0              ; [54] + 3
  sty NUSIZ1              ; [57] + 3
  
  ; Set Ball Position
  sta RESBL               ; [60] + 3    = 63

   ; Non-Reflected Playfield (Probably Not Needed)
  lda #0                  ; [63] + 2
  sta CTRLPF              ; [65] + 3
  
  ; Set Starting Line
  ldy #72                 ; [68] + 2
  sty TEMP                ; [70] + 3

  ; Move Sprites
  nop                     ; [73] + 2
  sta HMOVE               ; [75] + 3    > 74 < 4

  ; Update Music
  PLAY_MUSIC              ; [2] + 5
  
  ; Display Grid
  jmp StartGrid           ; [7] + 3     = 10

FlashLine
  ; Set Flash Colour
  ldx #BCOL0              ; [43] + 2
  lda GRID+2,Y            ; [45] + 4
  asl                     ; [49] + 2
  bmi ShowFlashLine       ; [51] + 2/3
  bpl HideFlashLine       ; [53] + 3
ShowFlashLine
  ldx #BCOL8              ; [54] + 2
HideFlashLine

  ; Enable Blank
  SLEEP 5                 ; [56] + 5
  stx VBLANK              ; [61] + 3    = 64

  ; Play Music
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; Store Kernel Colours
  stx TEMP2               ; [5] + 3
  stx Col1                ; [8] + 3 
  stx Col2                ; [11] + 3
  stx Col3                ; [14] + 3
  stx Col4                ; [17] + 3
  stx Col6                ; [20] + 3
  stx Col7                ; [23] + 3
  stx Col8                ; [26] + 3

  ; Set Line Pointer
  lda #<Line1             ; [29] + 2
  sta JPtr+1              ; [31] + 3

  ; Unblank
  sty VBLANK              ; [34] + 3    = 37

  ; Copy Line Colour to Y
  txa                     ; [37] + 2
  tay                     ; [39] + 2
  
  ; Enable Blank
  SLEEP 20                ; [41] + 20
  sty VBLANK              ; [61] + 3    = 64
  
  ; Display Grid
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  SLEEP 6                 ; [5] + 3
  sty COLUBK              ; [11] + 3
  jmp Modify              ; [14] + 3

  DC.B  "K2"

; -----------------------------------------------------------------------------
; GRID KERNEL LOOP
; -----------------------------------------------------------------------------

  ALIGN 256

EndGrid
  ; Finish Kernel
  jmp FinishKernel        ; [20] + 3

FlashingLine
  ; Show Flashing Line
  jmp FlashLine           ; [40] + 3

GridLoop
  ; Reset Colours
  ldx #BCOL0              ; [67] + 2
  stx COLUP0              ; [69] + 3
  stx COLUP1              ; [72] + 3
  
  ; Play Audio
  PLAY_MUSIC              ; [75] + 5
  
  ; Reset Colours
  stx COLUBK              ; [4] + 3
  stx COLUPF              ; [7] + 3

StartGrid
  ; Calculate Current Line
  sec                     ; [10] + 2
  
  lda TEMP                ; [12] + 3
  sbc #4                  ; [15] + 2
  bmi EndGrid             ; [17] + 2/3
  tay                     ; [19] + 2
  sty TEMP                ; [21] + 3

  ; Reset Stack Pointer
  pla                     ; [24] + 4

  ; Get First Grid Position
  lax GRID+0,Y            ; [28] + 4
  rol                     ; [32] + 2

  ; Disable Blank
  sty VBLANK              ; [34] + 3    = 37
  
  ; Check for flashing line
  bmi FlashingLine        ; [37] + 2/3
  
  ; Store Colours 0 & 1
  lda ColTable0,X         ; [39] + 4
  sta TEMP2               ; [43] + 3
  lda ColTable1,X         ; [46] + 4
  sta Col1                ; [50] + 3

  ; Get Second Grid Position
  ldx GRID+1,Y            ; [53] + 4
  lda ColTable0,X         ; [57] + 4

  ; Enable Blank
  sta VBLANK              ; [61] + 3    = 64

  ; Store Colour 2
  sta Col2                ; [64] + 3

  ; Set First Line Pointer
  lda #<Line1             ; [67] + 2
  sta JPtr+1              ; [69] + 3

  ; Get Color 3
  lda ColTable1,X         ; [72] + 4
  sta Col3                ; [0] + 3

  PLAY_MUSIC              ; [3] + 5
  
  ; Store Colour 3 & 8
  txa                     ; [8] + 2
  ror                     ; [10] + 2
  tax                     ; [12] + 2
  lda ColTable2,X         ; [14] + 4
  sta Col8                ; [18] + 3

  ; Get Third Grid Position
  lax GRID+2,Y            ; [21] + 4
  rol                     ; [25] + 2
  lda ColTable0,X         ; [27] + 4
  
  ; Store Colour 4
  sta Col4                ; [31] + 3

  ; Disable Blank
  sty VBLANK              ; [34] + 3    = 37

  ; Get Colour 5
  lda ColTable1,X         ; [37] + 4
  
  ; Get Fourth Grid Position
  ldx GRID+3,Y            ; [41] + 4

  ; Store Colours 5 (in Y), 6 and 7
  tay                     ; [45] + 2
  lda ColTable0,X         ; [47] + 4
  sta Col6                ; [51] + 3
  lda ColTable1,X         ; [54] + 4
  sta Col7                ; [58] + 3
  
  ; Enable Blank
  sta VBLANK              ; [61] + 3    = 64

  ; Get Colour 9 (in X)
  txa                     ; [64] + 2
  ror                     ; [66] + 2
  tax                     ; [68] + 2
  lda ColTable2,X         ; [70] + 4
  tax                     ; [74] + 2

  ; Play Audio
  PLAY_MUSIC              ; [0] + 5
  SLEEP 3                 ; [5] + 3

  ; Display Grid (X = Colour 4, Y = Colour 9)
Line0
  lda TEMP2               ; [8] + 3
  sta COLUBK              ; [11] + 3
  jmp Modify              ; [14] + 3
Line1
  pla                     ; [67] + 4
  SLEEP 3                 ; [71] + 3
  PLAY_MUSIC              ; [74] + 5
  lda #<Line2             ; [3] + 2
  sta JPtr+1              ; [5] + 3
  lda TEMP2               ; [8] + 3
  sta COLUBK              ; [11] + 3
  jmp Modify              ; [14] + 3
Line2
  pla                     ; [67] + 4
  SLEEP 3                 ; [71] + 3
  PLAY_MUSIC             ; [74] + 5
  lda #<Line3             ; [3] + 2
  sta JPtr+1              ; [5] + 3
  lda TEMP2               ; [8] + 3
  sta COLUBK              ; [11] + 3
  jmp Modify              ; [14] + 3
Line3
  pla                     ; [67] + 4
  SLEEP 3                 ; [71] + 3
  PLAY_MUSIC             ; [74] + 5
  lda #<Line4             ; [3] + 2
  sta JPtr+1              ; [5] + 3
  lda TEMP2               ; [8] + 3
  sta COLUBK              ; [11] + 3
  jmp Modify              ; [14] + 3
Line4
  pla                     ; [67] + 4
  SLEEP 3                 ; [71] + 3
  PLAY_MUSIC             ; [74] + 5
  lda #<Line5             ; [3] + 2
  sta JPtr+1              ; [5] + 3
  lda TEMP2               ; [8] + 3
  sta COLUBK              ; [11] + 3
  jmp Modify              ; [14] + 3
Line5
  pla                     ; [67] + 4
  SLEEP 3                 ; [71] + 3
  PLAY_MUSIC             ; [74] + 5
  lda #<Line6             ; [3] + 2
  sta JPtr+1              ; [5] + 3
  lda TEMP2               ; [8] + 3
  sta COLUBK              ; [11] + 3
  jmp Modify              ; [14] + 3
Line6
  pla                     ; [67] + 4
  SLEEP 3                 ; [71] + 3
  PLAY_MUSIC             ; [74] + 5
  lda #<Line7             ; [3] + 2
  sta JPtr+1              ; [5] + 3
  lda TEMP2               ; [8] + 3
  sta COLUBK              ; [11] + 3
  jmp Modify              ; [14] + 3
Line7
  pla                     ; [67] + 4
  SLEEP 3                 ; [71] + 3
  PLAY_MUSIC             ; [74] + 5
  lda #<Line8             ; [3] + 2
  sta JPtr+1              ; [5] + 3
  lda TEMP2               ; [8] + 3
  sta COLUBK              ; [11] + 3
  jmp Modify              ; [14] + 3
Line8
  jmp Line8_2             ; [67] + 3
EndLine
  if (>GridLoop != >EndLine)
    echo "WARNING: Line Kernel Crosses Page Boundary!"
  endif

  ; DC.B  "K3"

  ALIGN 256

; -----------------------------------------------------------------------------
; FINISH KERNEL & PLAY SOUNDS
; -----------------------------------------------------------------------------

FinishKernel
  ; Clear Everything
  ldx #0                  ; [23] + 2
  stx COLUBK              ; [25] + 3
  stx COLUPF              ; [28] + 3
  stx COLUP0              ; [31] + 3
  stx COLUP1              ; [34] + 3
  stx GRP0                ; [37] + 3
  stx GRP1                ; [40] + 3
  stx PF0                 ; [43] + 3
  stx PF1                 ; [46] + 3
  stx PF2                 ; [49] + 3
  stx ENABL               ; [52] + 3

  ; Reset Stack
  dex                     ; [55] + 2
  txs                     ; [57] + 2

  ; Set Sound Table MSB
  ldx #>GSoundTab         ; [59] + 2
  stx SPTR+1              ; [61] + 3

  IF (PALMODE)
  ldx #7
PalSkipLoop
  sta WSYNC
  PLAY_MUSIC
  SLEEP 30
  dex
  bpl PalSkipLoop
  ENDIF

  ; Begin Overscan
  ldx #OVERDELAY          ; [64] + 2
  lda #2                  ; [66] + 2
  sta WSYNC               ; [0]
  sta VBLANK              ; [0] + 3
  PLAY_MUSIC              ; [3] + 5
  stx TIM64T              ; [8] + 3

  ; Play Sound Effects
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  lax SFX                 ; [5] + 3
  beq EndGSounds          ; [8] + 2/3
  and #%00011111          ; [10] + 2
  tay                     ; [12] + 2
  dey                     ; [14] + 2
  bpl PlayGSound          ; [16] + 2/3
  lda #0                  ; [18] + 2
  sta SFX                 ; [20] + 3
  beq EndGSounds          ; [23] + 3
PlayGSound
  sty TEMP                ; [19] + 3
  txa                     ; [22] + 2
  and #%11100000          ; [24] + 2
  ora TEMP                ; [26] + 3
  sta SFX                 ; [29] + 3
  lsr                     ; [32] + 2
  lsr                     ; [34] + 2
  lsr                     ; [36] + 2
  lsr                     ; [38] + 2
  lsr                     ; [40] + 2
  tax                     ; [42] + 2
  lda GSoundTab,X         ; [44] + 4
  sta SPTR                ; [48] + 3
  lda (SPTR),Y            ; [51] + 5
  sta AUDF1               ; [56] + 3
  lda GSoundType,X        ; [59] + 4
  sta AUDC1               ; [63] + 3
  lda GSoundVol,X         ; [66] + 4
EndGSounds
  sta AUDV1               ; [70] + 3

  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  jmp KernelExit          ; [5] + 3

; -----------------------------------------------------------------------------
; SUBROUTINES
; -----------------------------------------------------------------------------

  ; Calculate Shifted Shape Values
MakeValues
  lax TEMPVAL             ; [0] + 3
  lsr                     ; [3] + 2
  sta VALUE2              ; [5] + 3
  lsr                     ; [8] + 2
  lsr                     ; [10] + 2
  lsr                     ; [12] + 2
  sta VALUE1              ; [14] + 3
  txa                     ; [17] + 2
  rol                     ; [19] + 2
  rol                     ; [21] + 2
  sta VALUE4              ; [23] + 3
  lda #0                  ; [26] + 2
  ror                     ; [28] + 2
  sta VALUE3              ; [30] + 3
  rts                     ; [33] + 6    = 39

; Set Data at Grid Position (bottom left = 0,0)
; A = Row (0-17)
; X = Column (0-9)
; VALUE1 = Colour (0-7), VALUE2 = Colour << 3
; VALUE3 = (Colour << 6) & 255, VALUE4 = (Colour << 5) & 128
SetData
  ; Check if row is off the grid (ignore)
  cmp #18                 ; [0] + 2
  bcs SetIgnore           ; [2] + 2/3
  ; Start position = Y * 4
  asl                     ; [4] + 2
  asl                     ; [6] + 2
  tay                     ; [8] + 2
  ; Check if column is off the edge (ignore)
  cpx #10                 ; [10] + 2
  bcs SetIgnore           ; [12] + 2/3
  ; Special case for position 8 & 9
  cpx #8                  ; [14] + 2
  beq SetEight            ; [16] + 2/3
  bcs SetNine             ; [18] + 2/3
  ; Start at column/2
  sty TEMP                ; [20] + 3
  txa                     ; [23] + 2
  lsr                     ; [25] + 2
  bcc SetLower            ; [27] + 2/3
SetUpper
  ; Set upper position
  clc                     ; [29] + 2
  adc TEMP                ; [31] + 3
  tay                     ; [34] + 2
  lda GRID,Y              ; [36] + 4
  and #%11000111          ; [40] + 2
  ora VALUE2              ; [42] + 3
  sta GRID,Y              ; [45] + 4
SetIgnore
  rts                     ; [49] + 6    = 55
SetLower
  ; Set lower position
  ; clc
  adc TEMP                ; [30] + 3
  tay                     ; [33] + 2
  lda GRID,Y              ; [35] + 4
  and #%11111000          ; [39] + 2
  ora VALUE1              ; [41] + 3
  sta GRID,Y              ; [44] + 4
  rts                     ; [48] + 6    = 54
SetEight
  ; Set position 8
  lda GRID,Y              ; [19] + 4
  and #%00111111          ; [23] + 2
  ora VALUE3              ; [25] + 3
  sta GRID,Y              ; [28] + 4
  lda GRID+1,Y            ; [32] + 4
  and #%00111111          ; [36] + 2
  ora VALUE4              ; [38] + 3
  sta GRID+1,Y            ; [41] + 4
  rts                     ; [45] + 6    = 51
SetNine
  ; Set position 9
  lda GRID+2,Y            ; [21] + 4
  and #%00111111          ; [25] + 2
  ora VALUE3              ; [27] + 3
  sta GRID+2,Y            ; [30] + 4
  lda GRID+3,Y            ; [34] + 4
  and #%00111111          ; [38] + 2
  ora VALUE4              ; [40] + 3
  sta GRID+3,Y            ; [43] + 4
  rts                     ; [47] + 6    = 53

  DC.B  "OCTOBER"

; -----------------------------------------------------------------------------
; KERNEL DATA
; -----------------------------------------------------------------------------

  ALIGN 256

  ; Sound Effects (Lower = Higher Priority)
  ; 0 = Win Sound
  ; 1 = Line 1
  ; 2 = Line 2
  ; 3 = Line 3
  ; 4 = Line 4
  ; 5 = Settle
  ; 6 = Rotate
  ; 7 = Fail Sound
GSoundTab
  DC.B  <GWinSnd, <GLine1Snd, <GLine2Snd, <GLine3Snd
  DC.B  <GLine4Snd, <GSettleSnd, <GRotateSnd, <GFailSnd
GSoundType
  DC.B  6, 12, 12, 12, 12, 2, 12, 7
GSoundVol
  DC.B  7, 6, 6, 6, 6, 6, 5, 7
; GSoundLen
; DC.B  30, 30, 30, 30, 30, 3, 7, 31
GLine1Snd
  DC.B  31, 31, 31, 15, 15, 15, 31, 31, 31, 15, 15, 15, 31, 31, 31
  DC.B  20, 20, 20, 6, 6, 6, 20, 20, 20, 6, 6, 6, 20, 20, 20
GLine2Snd
  DC.B  31, 31, 31, 7, 7, 7, 31, 31, 31, 7, 7, 7, 31, 31, 31
  DC.B  20, 20, 20, 6, 6, 6, 20, 20, 20, 6, 6, 6, 20, 20, 20
GLine3Snd
  DC.B  31, 31, 31, 3, 3, 3, 31, 31, 31, 3, 3, 3, 31, 31, 31
  DC.B  20, 20, 20, 6, 6, 6, 20, 20, 20, 6, 6, 6, 20, 20, 20
GLine4Snd
  DC.B  31, 31, 31, 0, 0, 0, 31, 31, 31, 1, 1, 1, 31, 31, 31
  DC.B  20, 20, 20, 2, 2, 2, 20, 20, 20, 3, 3, 3, 20, 20, 20
GSettleSnd
  DC.B  10, 8, 6, 4
GRotateSnd
  DC.B  8, 10, 12, 14, 18, 16, 20, 18
GWinSnd
  DC.B  0, 1, 1, 1, 3, 3, 3, 2, 2, 2, 4, 4, 4, 3, 3, 3
  DC.B  5, 5, 5, 2, 2, 2, 7, 7, 7, 4, 4, 4, 8, 8, 8
GFailSnd
  DC.B  30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30
  DC.B  15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15
EndGSoundTab
  if (>GSoundTab != >EndGSoundTab)
    echo "WARNING: Game Sound Table Crosses Page Boundary!"
  endif

  ALIGN 256
  
  ; Color Table 0 (256 Bytes)
  ; MASK = %00000111
ColTable0
  REPEAT 32
  DC.B  BCOL0, BCOL1, BCOL2, BCOL3, BCOL4, BCOL5, BCOL6, BCOL7
  REPEND

  ALIGN 256

  ; Color Table 1 (256 Bytes)
  ; MASK = %00111000
ColTable1
  REPEAT 4
  DS.B  8, BCOL0
  DS.B  8, BCOL1
  DS.B  8, BCOL2
  DS.B  8, BCOL3
  DS.B  8, BCOL4
  DS.B  8, BCOL5
  DS.B  8, BCOL6
  DS.B  8, BCOL7
  REPEND

  ALIGN 256

  ; Color Table 2 (256 Bytes)
  ; MASK = %11100000
ColTable2
  DS.B  32, BCOL0
  DS.B  32, BCOL1
  DS.B  32, BCOL2
  DS.B  32, BCOL3
  DS.B  32, BCOL4
  DS.B  32, BCOL5
  DS.B  32, BCOL6
  DS.B  32, BCOL7
  
  ALIGN 256

  ; Small Numbers Set - LHS Characters
LChars
L_GAP
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
L_55
  DC.B  %11000000
  DC.B  %00100000
  DC.B  %11000000
  DC.B  %10000000
L_22
  DC.B  %11100000
  DC.B  %10000000
  DC.B  %01000000
  DC.B  %00100000
L_33
  DC.B  %11000000
  DC.B  %00100000
  DC.B  %01100000
  DC.B  %00100000
L_99
  DC.B  %11000000
  DC.B  %00100000
  DC.B  %01100000
  DC.B  %10100000
L_00
  DC.B  %01000000
  DC.B  %10100000
  DC.B  %10100000
  DC.B  %10100000
L_88
  DC.B  %01000000
  DC.B  %10100000
  DC.B  %01000000
  DC.B  %10100000
L_77
  DC.B  %01000000
  DC.B  %01000000
  DC.B  %00100000
  DC.B  %00100000
L_11
  DC.B  %11100000
  DC.B  %01000000
  DC.B  %01000000
  DC.B  %11000000
L_66
  DC.B  %01000000
  DC.B  %10100000
  DC.B  %11000000
  DC.B  %10000000
  DC.B  %01100000
L_44
  DC.B  %00100000
  DC.B  %11100000
  DC.B  %10100000
  DC.B  %10000000
  DC.B  %10000000
L_AA
  DC.B  %10100000
  DC.B  %10100000
  DC.B  %11100000
  DC.B  %10100000
  DC.B  %01000000
L_BB
  DC.B  %11000000
  DC.B  %10100000
  DC.B  %11000000
  DC.B  %10100000
L_DD
  DC.B  %11000000
  DC.B  %10100000
  DC.B  %10100000
  DC.B  %10100000
  DC.B  %11000000
L_CC
  DC.B  %01100000
  DC.B  %10000000
  DC.B  %10000000
  DC.B  %10000000
  DC.B  %01100000
EndLChars
  if (>LChars != >EndLChars)
    echo "WARNING: LChars crosses a page boundary!"
  endif

  ; Small Numbers Set - RHS Characters
RChars
R_GAP
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
R_55
  DC.B  %00001100
  DC.B  %00000010
  DC.B  %00001100
  DC.B  %00001000
R_22
  DC.B  %00001110
  DC.B  %00001000
  DC.B  %00000100
  DC.B  %00000010
R_33
  DC.B  %00001100
  DC.B  %00000010
  DC.B  %00000110
  DC.B  %00000010
R_99
  DC.B  %00001100
  DC.B  %00000010
  DC.B  %00000110 
  DC.B  %00001010
R_00
  DC.B  %00000100
  DC.B  %00001010
  DC.B  %00001010
  DC.B  %00001010
R_88
  DC.B  %00000100
  DC.B  %00001010
  DC.B  %00000100
  DC.B  %00001010
R_77
  DC.B  %00000100
  DC.B  %00000100
  DC.B  %00000010
  DC.B  %00000010
R_11
  DC.B  %00001110
  DC.B  %00000100
  DC.B  %00000100
  DC.B  %00001100
R_66
  DC.B  %00000100
  DC.B  %00001010
  DC.B  %00001100
  DC.B  %00001000
  DC.B  %00000110
R_44
  DC.B  %00000010
  DC.B  %00001110
  DC.B  %00001010
  DC.B  %00001000
  DC.B  %00001000
EndRChars
  if (>RChars != >EndRChars)
    echo "WARNING: RChars crosses a page boundary!"
  endif

  ; Game Messages
MChars
M__G
  DC.B  %00000110
  DC.B  %00001010
  DC.B  %00001010
  DC.B  %00001000
  DC.B  %00000110
M_AM
  DC.B  %10101010
  DC.B  %10101010
  DC.B  %11101010
  DC.B  %10101110
  DC.B  %01001010
M_E_
  DC.B  %11100000
  DC.B  %10000000
  DC.B  %11000000
  DC.B  %10000000
  DC.B  %11100000
M__O
  DC.B  %00000100
  DC.B  %00001010
  DC.B  %00001010
  DC.B  %00001010
  DC.B  %00000100
M_VE
  DC.B  %01001110
  DC.B  %01001000
  DC.B  %10101100
  DC.B  %10101000
  DC.B  %10101110
M_R_
  DC.B  %10100000
  DC.B  %10100000
  DC.B  %11000000
  DC.B  %10100000
  DC.B  %11000000
M___
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
M__P
  DC.B  %00001000
  DC.B  %00001000
  DC.B  %00001100
  DC.B  %00001010
  DC.B  %00001100
M_AU
  DC.B  %10100100
  DC.B  %10101010
  DC.B  %11101010
  DC.B  %10101010
  DC.B  %01001010
M_SE
  DC.B  %11001110
  DC.B  %00101000
  DC.B  %01001100
  DC.B  %10001000
  DC.B  %01101110
M_D_
  DC.B  %11000000
  DC.B  %10100000
  DC.B  %10100000
  DC.B  %10100000
  DC.B  %11000000
M__W
  DC.B  %00001010
  DC.B  %00001110
  DC.B  %00001010
  DC.B  %00001010
  DC.B  %00001010
M_EL
  DC.B  %11101110
  DC.B  %10001000
  DC.B  %11001000
  DC.B  %10001000
  DC.B  %11101000
M_L_
  DC.B  %11100000
  DC.B  %10000000
  DC.B  %10000000
  DC.B  %10000000
  DC.B  %10000000
M__D
  DC.B  %00001100
  DC.B  %00001010
  DC.B  %00001010
  DC.B  %00001010
  DC.B  %00001100
M_ON
  DC.B  %01001010
  DC.B  %10101010
  DC.B  %10101010
  DC.B  %10101010
  DC.B  %01001100
M_TI
  DC.B  %01001110
  DC.B  %01000100
  DC.B  %01000100
  DC.B  %01000100
  DC.B  %11101110
M_ME
  DC.B  %10101110
  DC.B  %10101000
  DC.B  %10101100
  DC.B  %11101000
  DC.B  %10101110
M_UT
  DC.B  %01000100
  DC.B  %10100100
  DC.B  %10100100
  DC.B  %10100100
  DC.B  %10101110
EndMChars
  if (>MChars != >EndMChars)
    echo "WARNING: MChars crosses a page boundary!"
  endif

ScoreDigits
  DC.B  <L_00, <L_11, <L_22, <L_33, <L_44, <L_55, <L_66, <L_77, <L_88, <L_99
Letters
  DC.B  <L_AA, <L_BB, <L_CC, <L_DD, <L_GAP
EndChars
  if (>ScoreDigits != >EndChars)
    echo "WARNING: Character pointers cross a page boundary!"
  endif

  ; Game Messages
GameMessages
  DC.B  <M__G, <M_AM, <M_E_, <M__O, <M_VE, <M_R_
  DC.B  <M__W, <M_EL, <M_L_, <M__D, <M_ON, <M_E_
  DC.B  <M___, <M_TI, <M_ME, <M__O, <M_UT, <M___
  DC.B  <M___, <M__P, <M_AU, <M_SE, <M_D_, <M___
EndGameMessages
  if (>GameMessages != >EndGameMessages)
    echo "WARNING: Game messages cross a page boundary!"
  endif

GameMessageCols
  DC.B  BCOL7, BCOL5, BCOL5, BCOL2
EndGameMessageCols
  if (>GameMessageCols != >EndGameMessageCols)
    echo "WARNING: Game message cols cross a page boundary!"
  endif

  DC.B  "KVASS"

; -----------------------------------------------------------------------------
; STALL CODE
; -----------------------------------------------------------------------------

  ALIGN 256

  ; Stall Atari
DoMusicStall
  ; Initialise & Reposition Stall Sprites
  ldx #1
MusicStallPos
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
  ldy #BLUE               ; [37] + 2
  sty COLUP0              ; [39] + 3
  sta RESP0,X             ; [42] + 4 = 46
  dex
  bpl MusicStallPos

  START_FRAME

  ; Copy Stall Code
  ldx #(EndMusicStall - RAMSPINNER)
CopyMusicStall
  lda $FF80,X
  sta RAMSPINNER,X
  dex
  bpl CopyMusicStall

  ; Begin Stall
  START_OVERSCAN
  WAIT_OVERSCAN
  jmp StartMusicStall

  ; Next Random Number
GetRandom2
  lda RANDOM              ; [0] + 3
  beq ResetRandom2        ; [3] + 2/3
  bne NoResetRandom2      ; [5] + 3
ResetRandom2
  lda #RANDOMSEED         ; [6] + 2
NoResetRandom2
  asl                     ; [8] + 2
  bcs RndEor2             ; [10] + 2/3
  bcc NoRndEor2           ; [12] + 3
RndEor2
  eor #$AF                ; [13] + 2
NoRndEor2
  sta RANDOM              ; [15] + 3
  rts                     ; [18] + 6    = 24

  DC.B  "KRUSHCHEV"

  echo "----",($FF80 - *) , "bytes left (BANK 2 - GAME KERNEL)"

  ORG   $AF80
  RORG  RAMSPINNER

  ; Spinner Data
MusicSpinner
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
MusicSpriteLoop
MusicSpriteOffset
  lda MusicSpinner,X
  sta GRP0
  dex
  sta WSYNC
  sta WSYNC
  bpl MusicSpriteLoop

  ; End Of Frame
  IF (PALMODE)
  ldx #127
  ELSE
  ldx #115
  ENDIF
  stx VBLANK
MusicLastLoop
  dex
  sta WSYNC
  bpl MusicLastLoop
  stx GRP1

  ; Entry Point
StartMusicStall
  ; Do Vertical Sync
  lda #%00001110
MusicSyncLoop
  sta WSYNC
  sta VSYNC
  lsr
  bne MusicSyncLoop

  ; Check End of Stall
  IF (MELODY)
  bit $1FF4
  bvc FinishMusicStall
  ELSE
  IF (FAKESPIN)
  cpy #0
  beq FinishStall
  nop
  ELSE
  jmp FinishMusicStall
  nop
  nop
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
  sta MusicSpriteOffset+1

  ; Start Of Frame
  IF (PALMODE)
  ldx #164
  ELSE
  ldx #126
  ENDIF
MusicFirstLoop
  dex
  sta WSYNC
  bne MusicFirstLoop
  stx VBLANK

  ; Draw Sprite
  ldx #7
  bne MusicSpriteLoop

  ; Resume
FinishMusicStall
  jmp (RESUME)
EndMusicStall

  ; echo "----",($100 - *) , " bytes left (BANK 2 - STALL)"

  ORG     $AFE9
  RORG    $FFE9

ShowSpecial
  ; Switch To Bank 4
  nop     $FFF8
  nop
  nop
  nop

  ORG     $AFF4
  RORG    $FFF4
  DC.B    "BANK2", 0
  DC.W    (PlusROM_API - $D000)
  DC.W    Init2, Init2

