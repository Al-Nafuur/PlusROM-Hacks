; -----------------------------------------------------------------------------
; BANK 8 - TITLE SCREEN
; -----------------------------------------------------------------------------

  SEG     BANK8
  ORG     $F000
  RORG    $F000

ShowStartup
  nop     $FFF9       ; Switch to Bank 6
  jmp Start           ; Jump to Entry Point (when called from other Banks)
PlayMusic
  nop     $FFFA       ; Switch to Bank 7
  jmp ResumeTitles
  nop
  nop
  nop
ShowHighScoreTable
  nop     $FFF9       ; Switch to Bank 6
  jmp FinishTitles
BeginGame
  nop     $FFF4       ; Switch To Bank 1
  jmp ResumeTitles

TitleLoop
  ; Play Music
  jmp PlayMusic
ResumeTitles

  ; Update Game Cycle & Counters
  dec CYCLE
  lda CYCLE
  lsr
  bcc EndCycle
  lsr
  bcc EndCycle
  dec FASTCYCLE
  lsr
  bcc EndCycle
  lsr
  bcc EndCycle
  dec SLOWCYCLE
EndCycle

  ; Alternate Between Logo and Score Table
  lda FASTCYCLE
  bmi ShowLogo
  jmp ShowHighScoreTable
ShowLogo

  ; Set Three Sprite Copies
  lda #%00000110
  sta NUSIZ0
  sta NUSIZ1

  ; Delay P0 & P1
  lda #%00000001
  sta VDELP0
  sta VDELP1

  ; Update Logo Colour Tables
  lda SLOWCYCLE
  lsr
  lsr
  lsr
  lsr
  and #%00000111
  tax
  lda ColTab0,X
  sta COL0
  lda ColTab1,X
  sta COL1

  ; Wait for Vertical Blank End
  WAIT_VBLANK
  START_SCREEN

  ; Swap Kernels on Alternate Cycles
  lda CYCLE
  lsr
  bcs DoLogoKernel1
  jmp LogoKernel2
DoLogoKernel1
  jmp LogoKernel1

  ALIGN   256

LogoKernel1
  ; Sprite Positioning (Fine Tuning)
  sta WSYNC                 ; [0]
  lda #%10010000            ; [0] + 2
  sta HMP0                  ; [2] + 3
  lda #%10000000            ; [5] + 2
  sta HMP1                  ; [7] + 3

  ; Set Sprite Colours
  lda COL0                  ; [10] + 3
  sta COLUP0                ; [13] + 3
  sta COLUP1                ; [16] + 3

  ; Set Lines To Display
  ldy #LOGOH                ; [19] + 2

  ; Sprite Positioning (Coarse)
  SLEEP 8                   ; [21] + 8
  sta RESP0                 ; [29] + 3 = 32
  nop                       ; [32] + 2 = 34
  sta RESP1                 ; [34] + 3 = 37

  ; Display Sprites On Alternate Lines
KernelLoop1
  sta WSYNC                 ; [0]
  sta HMOVE                 ; [0] + 3   P0A P1A P0B P1B
  lda LRow1,Y               ; [3] + 4
  sta GRP0                  ; [7] + 3   - - 0 -
  lda LRow3,Y               ; [10] + 4
  sta GRP1                  ; [14] + 3  0 - - 3    < 36
  lda LRow5,Y               ; [17] + 4
  sta GRP0                  ; [21] + 3  0 3 5 -    < 42
  lda #0                    ; [24] + 2
  sta HMP0                  ; [26] + 3
  sta HMP1                  ; [39] + 3  
  lda LRow7,Y               ; [32] + 4
  sta GRP1                  ; [36] + 3  5 3 - 7    > 38 < 47
  lda LRow9,Y               ; [39] + 4  
  sta GRP0                  ; [43] + 3  5 7 9 -    > 44 < 52 
  lda LRow11,Y              ; [46] + 4
  sta GRP1                  ; [50] + 3  9 7 - 11  > 49 < 58
  sta GRP0                  ; [53] + 3  9 11 - -  > 54 < 63
  SLEEP 9                   ; [56] + 9
  lda COL1                  ; [65] + 3
  sta COLUP0                ; [68] + 3
  sta HMOVE                 ; [71] + 3  = 74!
  sta COLUP1                ; [74] + 3
  lda DRow0,Y               ; [1] + 4   P0A P1A P0B P1B  
  sta GRP0                  ; [5] + 3   - - 0 -
  lda DRow2,Y               ; [8] + 4
  sta GRP1                  ; [12] + 3  0 - - 2    < 34
  lda DRow4,Y               ; [15] + 4
  sta GRP0                  ; [19] + 3  0 2 4 -    < 39
  lda #%10000000            ; [22] + 2
  sta HMP0                  ; [24] + 3
  sta HMP1                  ; [27] + 3
  lda DRow6,Y               ; [30] + 4  
  sta GRP1                  ; [34] + 3  4 2 - 6    > 36 < 44
  lda DRow8,Y               ; [37] + 4
  sta GRP0                  ; [41] + 3  4 6 8 -    > 41 < 50 
  lda DRow10,Y              ; [44] + 4
  sta GRP1                  ; [48] + 3  8 6 - 10  > 47 < 55
  sta GRP0                  ; [51] + 3  8 11 - -  > 52 < 60
  SLEEP 5                   ; [54] + 5
  lda COL0                  ; [59] + 3
  sta COLUP0                ; [62] + 3
  sta COLUP1                ; [65] + 3
  dey                       ; [68] + 2
  bpl KernelLoop1           ; [70] + 2/3
EndLogoKernel1
  sta WSYNC                 ; [0]
  jmp EndLogoKernel         ; [0] + 3

  if (>LogoKernel1 != >EndLogoKernel1)
    echo "WARNING: Logo Kernel Crosses Page Boundary!"
  endif

LoadTitleMessage
  ; Set Text Pointers (MSB)
  lda #>TitleLetters
  sta PTR0+1
  sta PTR1+1
  sta PTR2+1
  sta PTR3+1
  sta PTR4+1
  sta PTR5+1
  ; Load Message Details (LSB)
  lda TitleMessages+0,X
  sta PTR0
  lda TitleMessages+1,X
  sta PTR1
  lda TitleMessages+2,X
  sta PTR2
  lda TitleMessages+3,X
  sta PTR3
  lda TitleMessages+4,X
  sta PTR4
  lda TitleMessages+5,X
  sta PTR5
  rts
EndLoadTitleMessage
  if (>LoadTitleMessage != >EndLoadTitleMessage)
    echo "WARNING: Load Title message Crosses Page Boundary!"
  endif

  ALIGN   256

LogoKernel2
  ; Position Sprites
  sta WSYNC                 ; [0]
  SLEEP 29                  ; [0] + 29
  sta RESP0                 ; [29] + 3 = 32
  nop                       ; [32] + 2
  sta RESP1                 ; [34] + 3 = 37
  lda #%10010000            ; [37] + 2
  sta HMP0                  ; [39] + 3
  lda #%10000000            ; [42] + 2
  sta HMP1                  ; [44] + 3
  SLEEP 13                  ; [47] + 13

  ; Load Kernel Height
  ldy #LOGOH                ; [60] + 2

  ; Set Sprite Colours
  lda COL0                  ; [62] + 3
  sta COLUP0                ; [65] + 3
  sta COLUP1                ; [68] + 3

KernelLoop2
  sta HMOVE                 ; [71] + 3  = 74!
  sta COLUP1                ; [74] + 3
  lda LRow0,Y               ; [1] + 4  P0A P1A P0B P1B  
  sta GRP0                  ; [5] + 3  - - 0 -
  lda LRow2,Y               ; [8] + 4
  sta GRP1                  ; [12] + 3  0 - - 2    < 34
  lda LRow4,Y               ; [15] + 4
  sta GRP0                  ; [19] + 3  0 2 4 -    < 39
  lda #%10000000            ; [22] + 2
  sta HMP0                  ; [24] + 3
  sta HMP1                  ; [27] + 3
  lda LRow6,Y               ; [30] + 4  
  sta GRP1                  ; [34] + 3  4 2 - 6    > 36 < 44
  lda LRow8,Y               ; [37] + 4
  sta GRP0                  ; [41] + 3  4 6 8 -    > 41 < 50 
  lda LRow10,Y              ; [44] + 4
  sta GRP1                  ; [48] + 3  8 6 - 10  > 47 < 55
  sta GRP0                  ; [51] + 3  8 11 - -  > 52 < 60
  lda COL1                  ; [54] + 3
  sta COLUP0                ; [57] + 3
  sta COLUP1                ; [60] + 3
  sta WSYNC                 ; [0]
  sta HMOVE                 ; [0] + 3  P0A P1A P0B P1B
  lda DRow1,Y               ; [3] + 4
  sta GRP0                  ; [7] + 3  - - 0 -
  lda DRow3,Y               ; [10] + 4
  sta GRP1                  ; [14] + 3  0 - - 3    < 36
  lda DRow5,Y               ; [17] + 4
  sta GRP0                  ; [21] + 3  0 3 5 -    < 42
  SLEEP 4                   ; [24] + 4
  ldx DRow11,Y              ; [28] + 4  
  lda DRow7,Y               ; [32] + 4
  sta GRP1                  ; [36] + 3  5 3 - 7    > 38 < 47
  lda DRow9,Y               ; [39] + 4  
  sta GRP0                  ; [43] + 3  5 7 9 -    > 44 < 52
  lda #0                    ; [46] + 2
  stx GRP1                  ; [48] + 3  9 7 - 11  > 49 < 58
  sta HMP0                  ; [51] + 3  
  sta GRP0                  ; [54] + 3  9 11 - -  > 54 < 63
  sta HMP1                  ; [57] + 3
  lda COL0                  ; [60] + 3
  sta COLUP0                ; [63] + 3
  dey                       ; [66] + 2
  bpl KernelLoop2           ; [68] + 2/3
EndLogoKernel2
  sta WSYNC                 ; [0]
  jmp EndLogoKernel         ; [0] + 3

  if (>LogoKernel2 != >EndLogoKernel2)
    echo "WARNING: Logo Kernel Crosses Page Boundary!"
  endif

LoadTitleWave
  ; Set Message Pointers (MSB)
  lda #>TitleLetters
  sta PTR0+1
  sta PTR1+1
  sta PTR2+1
  sta PTR3+1
  lda #>TitleScoreNumbers
  sta PTR4+1
  sta PTR5+1
  ; Load Wave Number
  lda #<T_WW
  sta PTR0
  lda #<T_AA
  sta PTR1
  lda #<T_VV
  sta PTR2
  lda #<T_EE
  sta PTR3
  lda STARTWAVE
  and #%00011111
  tay
  iny
  lax TitleBCDTable,Y
  and #%11110000
  lsr
  lsr
  lsr
  lsr
  tay
  lda TitleNumberPtrs,Y
  sta PTR4
  txa
  and #%00001111
  tay
  lda TitleNumberPtrs,Y
  sta PTR5
  rts

  ALIGN   256

EndLogoKernel
  ; Clear Current Sprite Positions
  sta HMCLR                 ; [3] + 3

  ; Clear Sprite Data
  lda #0                    ; [6] + 2
  sta GRP0                  ; [8] + 3
  sta GRP1                  ; [11] + 3

  ; Set Delay and Three Sprite Copies
  lda #%00000011            ; [14] + 2
  sta VDELP0                ; [16] + 3
  sta VDELP1                ; [19] + 3
  sta NUSIZ0                ; [22] + 3
  sta NUSIZ1                ; [25] + 3

  ; Set Sprite1 Offset (Wont Change Until HMOVE)
  lda #%00010000            ; [28] + 2
  sta HMP1                  ; [30] + 3

  ; Set Sprite Positions
  SLEEP 5                   ; [33] + 5
  sta RESP0                 ; [38] + 3 = 41 EXACT
  sta RESP1                 ; [41] + 3 = 44 EXACT
  
  ; Move Sprites
  sta WSYNC                 ; [0]
  sta HMOVE                 ; [0] + 3
  
  ; Set Score Colour
  lda COL0                  ; [3] + 3
  sta COLUP0                ; [6] + 3
  sta COLUP1                ; [9] + 3
  
  ; Position Missiles For Later
  SLEEP 22                  ; [12] + 22
  sta RESM0                 ; [34] + 3 = 37 EXACT
  sta RESM1                 ; [37] + 3 = 40 EXACT
  
  ; Gap Under Logo
  SKIP_LINES 5

  ; Display Score Table Message
  ldx #0
  jsr LoadTitleMessage
  jsr TextDisplay
  SKIP_LINES 4

  ; Clear Sprite Movements
  sta WSYNC
  sta HMCLR                 ; [0] + 3
  
  ; P0 Two Copies Medium
  lda #%00000010            ; [3] + 2
  sta NUSIZ0                ; [5] + 3
  
  ; P1 Two Copies Close
  lda #%00000001            ; [8] + 2
  sta NUSIZ1                ; [10] + 3
  
  ; No Sprite Delays
  lda #0                    ; [13] + 2
  sta VDELP0                ; [15] + 3
  sta VDELP1                ; [18] + 3

  ; Load Sprite Data High Addresses
  lda #>TitleSpriteData     ; [21] + 2
  sta PTR0+1                ; [23] + 3
  sta PTR1+1                ; [26] + 3
  sta PTR2+1                ; [29] + 3
  sta PTR3+1                ; [32] + 3
  sta PTRC+1                ; [35] + 3

  ; Position P0 & P1 Sprites
  sta RESP0                 ; [38] + 3 = 41!
  SLEEP 5                   ; [41] + 5
  sta RESP1                 ; [46] + 3 = 49!
  
  ; Set PF to Mask Missiles
  lda #0
  sta COLUPF
  sta PF0
  lda #%00000111
  sta PF2
  sta PF1
  lda #%00000100
  sta CTRLPF
  
  ; Store Stack Pointer
  tsx
  stx STACK

  ; Do Seven Iterations
  lda #6
  sta ROW
  
  ; Start Loop
  jmp DetailsLoop
  
EndScoreSetup
  if (>EndLogoKernel != >EndScoreSetup)
    echo "WARNING: Score Setup Crosses Page Boundary!"
  endif

  ALIGN   256

DetailsLoop
  sta WSYNC
  
  ; Calculate Sprite Data Offset
  lda ROW
  asl
  asl
  tax
  
  ; Setup Sprite Frame Pointer (Animated Sprites)
  lda SLOWCYCLE
  lsr
  bcs Frame2
Frame1
  lda TitleSpriteDetails+0,X
  bcc StoreFrame
Frame2
  nop
  lda TitleSpriteDetails+1,X
StoreFrame
  sta PTR0
  
  ; Setup Sprite Colour Pointer
  lda TitleSpriteDetails+2,X
  sta PTRC
  
  ; Calculate Score Data Offset
  lda TitleSpriteDetails+3,X
  asl
  asl
  tax
  
  ; Setup Score Pointers
  lda ScorePtrs+0,X
  sta PTR1
  lda ScorePtrs+1,X
  sta PTR2
  lda ScorePtrs+2,X
  sta PTR3

  ; Set Sprite Height
  ldy #7
  
  ; Set Score Colour
  IF (PALCOLS)
  lda #$0F
  ELSE
  lda #$0F
  ENDIF
  sta COLUP1

SpriteScores
  ; Display Sprites & Scores
  sta WSYNC               ; [0]
SpriteLoop
  SLEEP 3                 ; [0] + 3
  lda (PTRC),Y            ; [3] + 5
  sta COLUP0              ; [8] + 3       < 43
  lda (PTR0),Y            ; [11] + 5
  sta GRP0                ; [16] + 3      < 43 
  lda (PTR1),Y            ; [19] + 5
  sta GRP1                ; [24] + 3      < 51
  lda (PTR3),Y            ; [27] + 5
  sta.w TEMP              ; [32] + 4
  lda (PTR2),Y            ; [36] + 5
  IF (PALCOLS)
  ldx #$0F                ; [41] + 2
  ELSE
  ldx #$0F                ; [41] + 2
  ENDIF
  stx COLUP0              ; [43] + 3      = 46
  ldx TEMP                ; [46] + 3 
  sta GRP0                ; [49] + 3      > 45 < 54
  stx GRP1                ; [52] + 3      > 53 < 56
  ldx #ENAM1              ; [55] + 2
  txs                     ; [57] + 2
  cpy #2                  ; [59] + 2
  php                     ; [61] + 3
  php                     ; [64] + 3
  nop                     ; [67] + 2
  nop                     ; [69] + 2
  dey                     ; [71] + 2
  bpl SpriteLoop          ; [73] + 2/3
EndSpriteScores

  ; Clear Sprite Data
  lda #0                  ; [75] + 2
  sta GRP0                ; [1] + 3
  sta GRP1                ; [4] + 3
  dec ROW                 ; [7] + 3
  
  ; Do Remaining Iterations
  bpl DetailsLoop         ; [10] + 2/3
  
  ; Restore Stack
  ldx STACK               ; [12] + 3
  txs                     ; [15] + 2
  
  ; Clear Current Sprite Positions
  sta WSYNC               ; [0]
  sta HMCLR               ; [0] + 3

  ; Clear PF Data
  lda #0                  ; [3] + 2
  sta PF0                 ; [5] + 3
  sta PF1                 ; [8] + 3
  sta PF2                 ; [11] + 3

  ; Set Delay and Three Sprite Copies
  lda #%00000011          ; [14] + 2
  sta VDELP0              ; [16] + 3
  sta VDELP1              ; [19] + 3
  sta NUSIZ0              ; [22] + 3
  sta NUSIZ1              ; [25] + 3

  ; Set Sprite1 Offset (Wont Change Until HMOVE)
  lda #%00010000          ; [28] + 2
  sta HMP1                ; [30] + 3

  ; Set Sprite Positions
  SLEEP 5                 ; [33] + 5
  sta RESP0               ; [38] + 3 = 41 EXACT
  sta RESP1               ; [41] + 3 = 44 EXACT

  ; Move Sprite
  sta WSYNC
  sta HMOVE
  
EndAlienScores
  SKIP_LINES 5

  ; Display "Fire To Play" Message Or Wave Number
  lda STARTWAVE
  bne ShowWaveStart
  ldx #6
  jsr LoadTitleMessage
  jmp EndWaveStart
ShowWaveStart
  jsr LoadTitleWave
EndWaveStart

  ; Set Sprite Colours To Flashing Red
  lda CYCLE
  lsr
  lsr
  and #%00001111
  tax
  lda TitleFade,X
  sta COLUP0
  sta COLUP1

  ; Display Message
  jsr TextDisplay

FinishTitles
  ; Begin Overscan
  WAIT_SCREEN
  START_OVERSCAN

  ; Check If Fire Pressed
  lda DEBOUNCE
  beq CheckFirePressed
  dec DEBOUNCE
  jmp EndJoyCheck
CheckFirePressed
  lda INPT4
  bmi CheckJoyDirections
  lda #KEYWAIT
  sta DEBOUNCE
  jmp BeginGame
CheckJoyDirections
  ; Check Joystick Left/Right
  bit SWCHA
  bvc FlipScreens
  bpl FlipScreens
  ; Check Right Difficulty Switch
  bit SWCHB
  bmi ResetStartWave
  ; Check Joystick Up
  lda #%00010000
  bit SWCHA
  beq IncStartWave
  ; Check Joystick Down
  lda #%00100000
  bit SWCHA
  bne CheckSwitchesPressed
DecStartWave
  ; Decrement Starting Wave
  sec
  lda STARTWAVE
  sbc #1
  and #%00011111
  ; Show Title Screen
  ldx #255
  stx FASTCYCLE
  jmp StoreStartWave
FlipScreens
  ; Debounce Keys
  lda #KEYSHORT
  sta DEBOUNCE
  ; Flip Between Titles and High Score Table
  ldx #255
  lda FASTCYCLE
  bpl StoreFlip
  ldx #127
StoreFlip
  stx FASTCYCLE
CheckSwitchesPressed
  ; Check Right Difficulty
  lda SWCHB
  bmi ResetStartWave
  ; Check Reset Swicth
  lsr
  bcs CheckSelectPressed
ResetStartWave
  ; Reset Starting Wave
  lda #0
  beq StoreStartWave
CheckSelectPressed
  ; Check Select Switch
  lsr
  bcs EndJoyCheck
IncStartWave
  ; Increment Starting Wave
  clc
  lda STARTWAVE
  adc #1
  and #%00011111
  ; Show Title Screen
  ldx #255
  stx FASTCYCLE
StoreStartWave
  sta STARTWAVE
  ; Debounce Switch
  lda #KEYSHORT
  sta DEBOUNCE
EndJoyCheck

  ; Start Next Frame
  WAIT_OVERSCAN
  START_VBLANK
  
  ; Loop To Beginning
  jmp TitleLoop

  if (>SpriteScores != >EndSpriteScores)
    echo "WARNING: Sprite Score Kernel Crosses Page Boundary!"
  endif

TextDisplay
  ; Store Text Height
  ldy #(TEXTH - 1)
  sty ROW 
  ; Load First Char
  lda (PTR0),Y
  sta GRP0
  sta WSYNC                 ; [0]
  jmp StartText             ; [0] + 3
TextLoop
  ; Fetch Current Line
  ldy ROW                   ; [62] + 3
  SLEEP 6                   ; [65] + 6
  ; Display First 3 Chars
  lda (PTR0),Y              ; [71] + 5
  sta GRP0                  ; [0] + 3       > 54
StartText      
  lda (PTR1),Y              ; [3] + 5
  sta GRP1                  ; [8] + 3       < 42
  lda (PTR2),Y              ; [11] + 5
  sta GRP0                  ; [16] + 3      < 44    
  ; Pre-fetch Remaining 3 Chars
  lax (PTR3),Y              ; [19] + 5
  lda (PTR4),Y              ; [24] + 5
  sta TEMP                  ; [29] + 3
  lda (PTR5),Y              ; [32] + 5
  tay                       ; [37] + 2
  lda TEMP                  ; [39] + 3
  ; Display Remaining 3 Chars
  stx GRP1                  ; [42] + 3      > 44 < 47
  sta GRP0                  ; [45] + 3      > 46 < 50
  sty GRP1                  ; [48] + 3      > 49 < 52
  sta GRP0                  ; [51] + 3      > 52 < 55
  ; Update Counter
  dec ROW                   ; [54] + 5
  bpl TextLoop              ; [59] + 2/3
  ; Clear Sprite Data
  lda #0                    ; [61] + 2      
  sta GRP0                  ; [63] + 3      > 54
  sta GRP1                  ; [66] + 3
  sta GRP0                  ; [69] + 3
  rts
EndTextDisplay
  if (>TextDisplay != >EndTextDisplay)
    echo "WARNING: Text Display Kernel Crosses Page Boundary!"
  endif

  ALIGN   256

TitleData

  ; Title Alien Sprite Pointers
TitleSpriteDetails
  DC.B  <T_Astronaut, <T_Astronaut, <T_AstronautCol, 0
  DC.B  <T_Sphere, <T_Sphere, <T_SphereCol, 3
  DC.B  <T_BugAlien, <T_BugAlien, <T_BugAlienCol, 1
  DC.B  <T_SmallUFO, <T_SmallUFO, <T_SmallUFOCol, 2
  DC.B  <T_BigUFO, <T_BigUFO, <T_BigUFOCol, 3
  DC.B  <T_Alien2_1, <T_Alien2_2, <T_Alien2Col, 2
  DC.B  <T_Alien1_1, <T_Alien1_2, <T_Alien1Col, 1
  
  ; Title Alien Score Pointers
ScorePtrs
  DC.B  <T_Question, <T_Question, <T_Question, 0
  DC.B  <T_One, <T_Five, <T_Zero, 0
  DC.B  <T_Three, <T_Zero, <T_Zero, 0
  DC.B  <T_Five, <T_Zero, <T_Zero, 0

  ; Title Number Pointers
TitleNumberPtrs
  DC.B  <T_Zero+1, <T_One+1, <T_Two+1, <T_Three+1, <T_Four+1
  DC.B  <T_Five+1, <T_Six+1, <T_Seven+1, <T_Eight+1, <T_Nine+1

  ; Juno Logo Colours
ColTab0
  IF (PALCOLS)
  DC.B  $5E, $3E, $2E, $4E, $6E, $8E, $9E, $7E
  ELSE
  DC.B  $BE, $DE, $1E, $2E, $4E, $5E, $6E, $9E
  ENDIF
  
ColTab1
  IF (PALCOLS)
  DC.B  $52, $32, $22, $42, $62, $82, $92, $72
  ELSE
  DC.B  $B2, $D2, $12, $22, $42, $52, $62, $92
  ENDIF

  ; Red Fade Effect Colours
TitleFade
  IF (PALCOLS)
  DC.B  $60, $62, $64, $66, $68, $6A, $6C, $6E
  DC.B  $6E, $6C, $6A, $68, $66, $64, $62, $60
  ELSE
  DC.B  $40, $42, $44, $46, $48, $4A, $4C, $4E
  DC.B  $4E, $4C, $4A, $48, $46, $44, $42, $40
  ENDIF

  ; Title Screen Messages
TitleMessages
  ; Score Table (0)
  DC.B  <T_SC, <T_OR, <T_E_, <T_TA, <T_BL, <T_ECOLON
  ; Fire To Play (6)
  DC.B  <T_FI, <T_RE, <T__T, <T_O_, <T_PL, <T_AY

EndTitleData
  if (>TitleData != >EndTitleData)
    echo "WARNING: Title Sprite Data Crosses Page Boundary!"
  endif

  ALIGN   256
  
TitleSpriteData
  ; Title Alien Sprites
T_Alien1_1             ; Alien 1 (Frame 1)
  DC.B  %10011001
  DC.B  %01011010
  DC.B  %00111100
  DC.B  %01111110
  DC.B  %10101001
  DC.B  %01101010
  DC.B  %00111100
  DC.B  %00011000
T_Alien1_2             ; Alien1 (Frame 2)
  DC.B  %10011001
  DC.B  %01011010
  DC.B  %00111100
  DC.B  %01111110
  DC.B  %10010101
  DC.B  %01010110
  DC.B  %00111100
  DC.B  %00011000
T_Alien2_1             ; Alien 2 (Frame 1) 
  DC.B  %00101000
  DC.B  %00101000
  DC.B  %01111100
  DC.B  %10111010
  DC.B  %11111110
  DC.B  %01000100
  DC.B  %00111000
  DC.B  %00010000
T_Alien2_2             ; Alien 2 (Frame 2)
  DC.B  %10010010
  DC.B  %01010100
  DC.B  %01111100
  DC.B  %10111010
  DC.B  %11111110
  DC.B  %01000100
  DC.B  %00111000
  DC.B  %00010000
T_BigUFO               ; Big UFO
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00111000
  DC.B  %01111100
  DC.B  %10101010
  DC.B  %01111100
  DC.B  %00111000
  DC.B  %00000000  
T_SmallUFO             ; Small UFO 
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00111000
  DC.B  %01111100
  DC.B  %10101010
  DC.B  %01111100
  DC.B  %00101000
  DC.B  %00010000  
T_BugAlien             ; Bug Alien
  DC.B  %01010100
  DC.B  %00111000
  DC.B  %11111110
  DC.B  %01101100
  DC.B  %01101100
  DC.B  %00101000
  DC.B  %01010100
  DC.B  %00010000
T_Sphere               ; Sphere
  DC.B  %00111000
  DC.B  %01001100
  DC.B  %10010110
  DC.B  %11110010
  DC.B  %11001110
  DC.B  %10101010
  DC.B  %01110100
  DC.B  %00111000
T_Astronaut            ; Astronaut
  DC.B  %00101000
  DC.B  %00101000
  DC.B  %00111000
  DC.B  %01111100
  DC.B  %01010100
  DC.B  %00101000
  DC.B  %00101000
  DC.B  %00010000

  ; Title Alien Sprite Colours (NTSC)
  IF (PALCOLS)
T_Alien1Col
  DC.B  $64, $64, $62, $28, $28, $64, $28, $28
T_Alien2Col
  DC.B  $64, $64, $D6, $D6, $9A, $9A, $9A, $9A
T_BugAlienCol
  DC.B  $36, $36, $36, $88, $88, $88, $36, $28
T_BigUFOCol
  DC.B  $0E, $0E, $66, $88, $88, $88, $66, $0E
T_SmallUFOCol
  DC.B  $0E, $0E, $36, $64, $64, $28, $28, $28
T_SphereCol
  DC.B  $D6, $D6, $D6, $9A, $9A, $9A, $9A, $9A
T_AstronautCol
  DC.B  $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
  ELSE
T_Alien1Col
  DC.B  $44, $44, $42, $28, $28, $44, $28, $28
T_Alien2Col
  DC.B  $44, $44, $86, $86, $AA, $AA, $AA, $AA
T_BugAlienCol
  DC.B  $D6, $D6, $D6, $58, $58, $58, $D6, $18
T_BigUFOCol
  DC.B  $0E, $0E, $46, $58, $58, $58, $46, $0E
T_SmallUFOCol
  DC.B  $0E, $0E, $D6, $44, $44, $28, $28, $28
T_SphereCol
  DC.B  $86, $86, $86, $AA, $AA, $AA, $AA, $AA
T_AstronautCol
  DC.B  $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
  ENDIF
EndTitleSpriteData
  if (>TitleSpriteData != >EndTitleSpriteData)
    echo "WARNING: Title Sprite Data Crosses Page Boundary!"
  endif

  ; Title Font and Logo Data
  TITLENUMBERS
  
  ALIGN   256
  
  TITLELETTERS
  JUNOLOGO
  
Start
  ; Wipe Registers
  CLEAN_START

  ; Start First Frame
  START_VBLANK

  ; Initialise Random Seed
  lda #15
  sta RANDOM

  ; Show Startup Message
  jmp ShowStartup
  
  ; Binary To BCD Table (100 Entries)
TitleBCDTable
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
EndTitleBCDTable
  if (>TitleBCDTable != >EndTitleBCDTable)
    echo "WARNING: Title BCD Table crosses a page boundary!"
  endif

  ALIGN 256

  ; Embedded Copyright Notice
  DC.B  "Juno First - Atari 2600 Version - "
  DC.B  "Copyright (C) 2008 Chris Walton (cwalton@gmail.com) - "
  DC.B  "Distributed exclusively by AtariAge (http://www.atariage.com)"

  echo "----",($FFF0 - *) , "bytes left (BANK 8 - TITLES)"

  ; Startup Code (Jump To Start)
  ORG     $FFF0
  RORG    $FFF0
  DC.B    0, 0, 0, 255
  DC.B    0, 0, 0, 0, 0, 0
  DC.W    (PlusAPI - $E000)
  DC.W    Start, Start

