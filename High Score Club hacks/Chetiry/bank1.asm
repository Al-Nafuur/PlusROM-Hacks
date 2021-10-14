; -----------------------------------------------------------------------------
; CHETIRY - Atari 2600 - (C) Copyright 2011 - AtariAge
; BANK 1 - MAIN GAME
; -----------------------------------------------------------------------------

; Flash Masks
FLASHON                   = %01000000
FLASHOFF                  = %00000000
  
; GameOver Fill Colours
FILL0                     = %10111111     ; Fill A (BCOL7)
FILL1                     = %11111111     ; Fill B (BCOL7)

  ; Load Temporary Shape Values
  MAC LOADTEMPS
  ; Y Position
  lax YPOS                ; [0] + 3
  and #%00011111          ; [3] + 2
  sta TEMPY               ; [5] + 3
  ; Rotation
  txa                     ; [8] + 2
  and #%01100000          ; [10] + 2
  sta TEMPROT             ; [12] + 3
  ; Rotation * 4
  lsr                     ; [15] + 2
  lsr                     ; [17] + 2
  lsr                     ; [19] + 2
  sta TEMP                ; [21] + 3
  ; X Position (-1)
  lax XPOS                ; [24] + 3
  and #%00001111          ; [27] + 2
  sec                     ; [29] + 2
  sbc #1                  ; [31] + 2
  sta TEMPX               ; [33] + 3
  ; Shape Type
  txa                     ; [36] + 2
  and #%01110000          ; [38] + 2
  sta TEMPVAL             ; [40] + 3
  ; Shape Pointer (Type * 32 + Rotation * 4)
  asl                     ; [43] + 2
  ora TEMP                ; [45] + 3
  sta SPTR                ; [48] + 3      = 51
  ENDM

; -----------------------------------------------------------------------------

  MAC SHAPEVALS
  ; Calculate Shifted Shape Values
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
  sta VALUE3              ; [30] + 3      = 33
  ENDM
  
; -----------------------------------------------------------------------------

  ; Set/Reset Drop Timer
  MAC SETTIMER
  ; Store Next Piece
  lda TIMER               ; [0] + 3
  and #%11100000          ; [3] + 2
  sta TEMP                ; [5] + 3
  ; Get Level
  lda LEVEL               ; [8] + 3
  and #%00011111          ; [11] + 2
  tay                     ; [13] + 2
  ; Lightning After Level 19
  IF (PALMODE)
  lda #3                  ; [15] + 2
  ELSE
  lda #4                  ; [15] + 2
  ENDIF
  cpy #19                 ; [17] + 2
  bcc .NormalTimer        ; [19] + 2/3
  nop                     ; [21] + 2
  bcs .StoreTimer         ; [23] + 3
.NormalTimer
  ; Normal Levels (0->19)
  lda Levels,Y            ; [22] + 4
.StoreTimer
  ora TEMP                ; [26] + 3
  sta TIMER               ; [29] + 3      = 32
  ENDM

; -----------------------------------------------------------------------------

; Get Data at Grid Position (bottom left = 0,0)
; A = Row (0-17)
; X = Column (0-9)
  MAC GETDATA
.ENDJUMP SET {1}
  ; Check if X is off the edge (error)
  cpx #10                 ; [0] + 2
  bcs .GetError           ; [2] + 2/3
  ; Check if row is off the top/bottom of grid
  cmp #18                 ; [4] + 2
  bcs .GetTop             ; [6] + 2/3
  ; Start position = Y * 4
  asl                     ; [8] + 2
  asl                     ; [10] + 2
  tay                     ; [12] + 2
  ; Special case for position 8 & 9
  cpx #8                  ; [14] + 2
  bcc .GetNormal          ; [16] + 2/3
  beq .GetEight           ; [18] + 2/3
.GetNine
  ; Fetch data
  lda GRID+2,Y            ; [20] + 4
  rol                     ; [24] + 2
  ; Merge data 
  lda GRID+3,Y            ; [26] + 4
  rol                     ; [30] + 2
  rol                     ; [32] + 2
  rol                     ; [34] + 2
  and #%00000111          ; [36] + 2
  jmp .ENDJUMP            ; [38] + 3      = 41
.GetEight
  ; Fetch data
  lda GRID+0,Y            ; [21] + 4
  rol                     ; [25] + 2
  ; Merge data 
  lda GRID+1,Y            ; [27] + 4
  rol                     ; [31] + 2
  rol                     ; [33] + 2
  rol                     ; [35] + 2
  and #%00000111          ; [37] + 2
  jmp .ENDJUMP            ; [39] + 3      = 42
.GetTop
  ; Check if row is off bottom of grid (error)
  tay                     ; [9] + 2
  bmi .GetError           ; [11] + 2/3
  lda #0                  ; [13] + 2
  jmp .ENDJUMP            ; [15] + 3      = 18
.GetError
  lda #%00000111          ; [14] + 2
  jmp .ENDJUMP            ; [16] + 3      = 19
.GetNormal
  ; Start at X/2
  sty TEMP                ; [19] + 3
  txa                     ; [22] + 2
  lsr                     ; [24] + 2
  bcs .GetUpper           ; [26] + 2/3
.GetLower
  ; Start position + Y
  ; clc
  adc TEMP                ; [28] + 3
  tay                     ; [31] + 2
  lda GRID,Y              ; [33] + 4
  and #%00000111          ; [37] + 2
  jmp .ENDJUMP            ; [39] + 3      = 42
.GetUpper
  ; Start position + Y
  clc                     ; [29] + 2
  adc TEMP                ; [31] + 3
  tay                     ; [34] + 2
  lda GRID,Y              ; [36] + 4
  and #%00111000          ; [39] + 2      = 41
  ; Shift to position (not required for overlap test)
  ; lsr
  ; lsr
  ; lsr
  ENDM

; -----------------------------------------------------------------------------

  ; Draw Current Shape on Grid
  ; A = Row (0-17)
  ; Y = Column (0-9) 
  ; VALUE1-4 = Shape Colour (0-7)
  MAC SETDATA
.ENDJUMP    SET {1}
.SKIPLABEL  SET {2}
  ; Check if column is off the edge (ignore)
  cpy #10                 ; [0] + 2
  bcs .SKIPLABEL          ; [2] + 2/3
  ; Special case for position 8 & 9
  cpy #8                  ; [4] + 2
  bcc .SetNormal          ; [6] + 2/3
  beq .SetEight           ; [8] + 2/3
.SetNine
  ; Set position 9
  lda GRID+2,X            ; [10] + 4
  and #%00111111          ; [14] + 2
  ora VALUE3              ; [16] + 3
  sta GRID+2,X            ; [19] + 4
  lda GRID+3,X            ; [23] + 4
  and #%00111111          ; [27] + 2
  ora.w VALUE4            ; [29] + 4
  sta GRID+3,X            ; [33] + 4
  jmp .ENDJUMP            ; [37] + 3      = 40
.SetEight
  ; Set position 8
  lda GRID,X              ; [11] + 4
  and #%00111111          ; [15] + 2
  ora VALUE3              ; [17] + 3
  sta GRID,X              ; [20] + 4
  lda GRID+1,X            ; [24] + 4
  and #%00111111          ; [28] + 2
  ora VALUE4              ; [30] + 3
  sta GRID+1,X            ; [33] + 4
  jmp .ENDJUMP            ; [37] + 3      = 40
.SetLower
  ; Set lower position
  ; clc
  adc TEMP                ; [19] + 3
  tax                     ; [22] + 2
  lda GRID,X              ; [24] + 4
  and #%11111000          ; [28] + 2
  ora VALUE1              ; [30] + 3
  sta GRID,X              ; [33] + 4
  jmp .ENDJUMP            ; [37] + 3      = 40
.SetNormal
  ; Start at column/2
  stx TEMP                ; [9] + 3
  tya                     ; [12] + 2
  lsr                     ; [14] + 2
  bcc .SetLower           ; [16] + 2/3
.SetUpper
  ; Set upper position
  ; clc
  adc TEMP                ; [18] + 3
  tax                     ; [21] + 2
  lda GRID-1,X            ; [23] + 4
  and #%11000111          ; [27] + 2
  ora.w VALUE2            ; [29] + 4
  sta GRID-1,X            ; [33] + 4      = 37
  ENDM

; -----------------------------------------------------------------------------

; BANK 1
  SEG     BANK1
  ORG     $9000
  RORG    $F000

  ; Space for DPC registers
  DS.B    256,0

Init1
  ; Switch to Bank 7
  nop     $FFFB
FinishGame
  ; Switch To Bank 7
  nop     $FFFB
StartKernel
  ; Switch To Bank 2
  nop     $FFF6
  ; Resume Bank 2 After Kernel
  jmp     OverLogic
  nop
  nop
  nop
StartSpecial
  ; Switch to Bank 2
  nop     $FFF6

GameStart
  ; Random Initial Shape
  ldy RANDOM
  lda Mod7,Y
  sta TIMER

  ; Spawn First Shape
  jsr GetRandom
  jmp Spawn

; -----------------------------------------------------------------------------
; MAIN GAME LOOP
; -----------------------------------------------------------------------------

MainLoop
  lda #2
  sta WSYNC               ; [0]
  sta VSYNC               ; [0] + 3       VSYNC enable
  PLAY_MUSIC              ; [3] + 5

  ; Get Y Position
  lax YPOS                ; [8] + 3
  and #%00011111          ; [11] + 2
  sta TEMPY               ; [13] + 3
  ; Get Rotation
  txa                     ; [16] + 2
  and #%01100000          ; [18] + 2
  sta TEMPROT             ; [20] + 3
  sta TEMPROT2            ; [23] + 3
  lsr                     ; [26] + 2
  lsr                     ; [28] + 2
  lsr                     ; [30] + 2
  sta TEMP                ; [32] + 3
  ; Get X Position
  lax XPOS                ; [35] + 3
  and #%00001111          ; [38] + 2
  sec                     ; [40] + 2
  sbc #1                  ; [42] + 2
  sta TEMPX               ; [44] + 3
  sta TEMPX2              ; [47] + 3
  ; Get Shape Type
  txa                     ; [50] + 2
  and #%01110000          ; [52] + 2
  sta TEMPVAL             ; [54] + 3
  ; Get Shape Pointer (Type * 32 + Rotation * 4)
  asl                     ; [57] + 2
  ora TEMP                ; [59] + 3
  sta SPTR                ; [62] + 3 
  sta SPTR2               ; [65] + 3

  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; Check For Pause
  bit CYCLE               ; [5] + 3
  bmi NoMusic             ; [8] + 2/3
  ; Check For Tune Type
  lax MUSIC               ; [10] + 3
  and #%11100000          ; [13] + 2
  bne PlayMusic           ; [15] + 2/3
NoMusic
  ; Silence Music
  ldx #0                  ; [17] + 2
  stx AUDV0               ; [19] + 3
  sta TUNERESET_W         ; [22] + 3
  jmp EndMusic            ; [25] + 3
PlayMusic
  ; Update Music Counter
  sta TEMP                ; [18] + 3
  txa                     ; [21] + 2
  and #%00011111          ; [23] + 2
  sec                     ; [25] + 2
  sbc #1                  ; [27] + 2
  bpl StoreMusic          ; [29] + 2/3
  ; Advance Music Position
  sta TUNESTEP_W          ; [31] + 3
  nop                     ; [34] + 2
  ; Reset Tempo
  ldx #MUSICTEMPO         ; [36] + 2
  txa                     ; [38] + 2
StoreMusic
  ora TEMP                ; [40] + 3
  sta MUSIC               ; [43] + 3
EndMusic

  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; Calculate Shifted Shape Values
  SHAPEVALS               ; [5] + 33

  ; Set VBlank Timer
  ldx #VBLANKDELAY
  lda #0
  sta WSYNC               ; [0]
  sta VSYNC               ; [0] + 3       VSYNC disable
  PLAY_MUSIC              ; [3] + 5
  stx TIM64T              ; [8] + 3

  ; Check For Pause
  bit CYCLE               ; [11] + 3
  bpl CheckMode           ; [14] + 2/3
  jmp ShowShape           ; [16] + 3

  ; Check Mode Bits
CheckMode
  bit LINES+1             ; [17] + 3
  bvs SettleMode1         ; [20] + 2/3    Settle Mode
  bmi FlashMode           ; [22] + 2/3    Flash Mode
  jmp NormalMode          ; [24] + 3      Normal Mode
SettleMode1
  bmi GameOverMode        ; [23] + 2/3    Game Over Mode
  jmp SettleMode          ; [25] + 3

GameOverMode
  ; Check Timer
  lax TIMER               ; [26] + 3
  and #%00011111          ; [29] + 2
  beq CheckGameOverSfx    ; [31] + 2/3
  ; Check GameOver Type
  txa                     ; [33] + 2
  and #%11100000          ; [35] + 2
  beq GameOverFill        ; [37] + 2/3
WaitGameEnd
  jmp StartKernel         ; [40] + 3      = 45
CheckGameOverSfx
  ; Ensure Sounds Are Done
  lda SFX                 ; [34] + 3
  bne WaitGameEnd         ; [37] + 2/3
  jmp EndGame             ; [39] + 3

GameOverFill
  ; Fill Well With Blocks
  ldy #FILL1              ; [40] + 2
  ldx #0                  ; [42] + 2
FillLoop
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  lda GRID+2,X            ; [5] + 4
  and #%01000000          ; [9] + 2       Test Fill Bit
  bne NextFill            ; [11] + 2/3
  lda #FILL0              ; [13] + 2
  sta GRID+0,X            ; [15] + 4
  sty GRID+1,X            ; [19] + 4
  ora #%01000000          ; [23] + 2      Set Fill Bit
  sta GRID+2,X            ; [25] + 4
  sty GRID+3,X            ; [29] + 4
  ldx #GAMEOVERTIME       ; [33] + 2
  stx TIMER               ; [35] + 3
  jmp StartKernel         ; [38] + 3      = 41
NextFill
  inx                     ; [14] + 2
  inx                     ; [16] + 2
  inx                     ; [18] + 2
  inx                     ; [20] + 2
  cpx #72                 ; [22] + 2
  bcc FillLoop            ; [24] + 2/3
  jmp StartKernel         ; [26] + 3      = 29

; -----------------------------------------------------------------------------

FlashMode
  ; Continue Flashing Until Times Is Zero
  lda TIMER               ; [25] + 3
  and #%00011111          ; [28] + 2
  ; Compress When Timer = 0
  bne DoFlash             ; [30] + 2/3
  jmp CompressMode        ; [32] + 3
DoFlash
  ; Add Score When Timer = 1
  cmp #1                  ; [33] + 2
  bne DoFlash2            ; [35] + 2/3
  jmp AddScore            ; [37] + 3
DoFlash2
  ; Calculate Flash
  lsr                     ; [38] + 2
  lsr                     ; [40] + 2
  lsr                     ; [42] + 2
  ldx #FLASHON            ; [44] + 2
  bcs FlashOff            ; [46] + 2/3
  bcc FlashStore          ; [48] + 3
FlashOff
  ldx #FLASHOFF           ; [49] + 2
FlashStore
  stx TEMP                ; [51] + 3
  ldx #68                 ; [54] + 2
FlashLoop
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  ; Check For Complete Line
  lda GRID+0,X            ; [5] + 4
  and #%01000000          ; [9] + 2
  beq NextFlashA          ; [11] + 2/3
  ; Update Line
  lda GRID+2,X            ; [13] + 4
  and #%10111111          ; [17] + 2
  ora TEMP                ; [19] + 3
  sta GRID+2,X            ; [22] + 4
NextFlashA
  dex                     ; [26] + 2
  dex                     ; [28] + 2
  dex                     ; [30] + 2
  dex                     ; [32] + 2
  ; Check For Complete Line
  lda GRID+0,X            ; [34] + 4
  and #%01000000          ; [38] + 2
  beq NextFlashB          ; [40] + 2/3
  ; Update Line
  lda GRID+2,X            ; [42] + 4
  and #%10111111          ; [46] + 2
  ora TEMP                ; [48] + 3
  sta GRID+2,X            ; [51] + 4
NextFlashB
  dex                     ; [55] + 2
  dex                     ; [57] + 2
  dex                     ; [59] + 2
  dex                     ; [61] + 2
  bpl FlashLoop           ; [63] + 2/3
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  jmp StartKernel         ; [5] + 3       = 8
  
; -----------------------------------------------------------------------------

CompressMode
  ; Compress Grid
  ldx #0                  ; [35] + 2      0
  bit GRID+0              ; [37] + 3
  bvs CompressLoop        ; [40] + 2/3
  ldx #4                  ; [42] + 2      1
  bit GRID+4              ; [44] + 3
  bvs CompressLoop        ; [47] + 2/3
  ldx #8                  ; [49] + 2      2
  bit GRID+8              ; [51] + 3
  bvs CompressLoop        ; [54] + 2/3
  ldx #12                 ; [56] + 2      3
  bit GRID+12             ; [58] + 3
  bvs CompressLoop        ; [61] + 2/3
  ldx #16                 ; [63] + 2      4
  bit GRID+16             ; [65] + 3
  bvs CompressLoop        ; [68] + 2/3
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  ldx #20                 ; [5] + 2       5
  bit GRID+20             ; [7] + 3
  bvs CompressLoop        ; [10] + 2/3
  ldx #24                 ; [12] + 2      6
  bit GRID+24             ; [14] + 3
  bvs CompressLoop        ; [17] + 2/3
  ldx #28                 ; [19] + 2      7
  bit GRID+28             ; [21] + 3
  bvs CompressLoop        ; [24] + 2/3
  ldx #32                 ; [26] + 2      8
  bit GRID+32             ; [28] + 3
  bvs CompressLoop        ; [31] + 2/3
  ldx #36                 ; [33] + 2      9
  bit GRID+36             ; [35] + 3
  bvs CompressLoop        ; [38] + 2/3
  ldx #40                 ; [40] + 2      10
  bit GRID+40             ; [42] + 3
  bvs CompressLoop        ; [45] + 2/3
  ldx #44                 ; [47] + 2      11
  bit GRID+44             ; [49] + 3
  bvs CompressLoop        ; [52] + 2/3
  ldx #48                 ; [54] + 2      12
  bit GRID+48             ; [56] + 3
  bvs CompressLoop        ; [59] + 2/3
  ldx #52                 ; [61] + 2      13
  bit GRID+52             ; [63] + 3
  bvs CompressLoop        ; [66] + 2/3
  ldx #56                 ; [68] + 2      14
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  bit GRID+56             ; [5] + 3
  bvs CompressLoop        ; [8] + 2/3
  ldx #60                 ; [10] + 2      15
  bit GRID+60             ; [12] + 3
  bvs CompressLoop        ; [15] + 2/3
  ldx #64                 ; [17] + 2      16
  bit GRID+64             ; [19] + 3
  bvs CompressLoop        ; [22] + 2/3
  bit GRID+68             ; [24] + 3      17
  bvs ClearTopLine        ; [27] + 2/3
  ; Finished Compression
  jmp EndCompressMode     ; [29] + 3
  
CompressLoop
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  lda GRID+4,X            ; [5] + 4
  sta GRID+0,X            ; [9] + 4
  lda GRID+5,X            ; [13] + 4
  sta GRID+1,X            ; [17] + 4
  lda GRID+6,X            ; [21] + 4
  sta GRID+2,X            ; [25] + 4
  lda GRID+7,X            ; [29] + 4
  sta GRID+3,X            ; [33] + 4
  inx                     ; [37] + 2
  inx                     ; [39] + 2
  inx                     ; [41] + 2
  inx                     ; [43] + 2
  cpx #68                 ; [45] + 2
  bcc CompressLoop        ; [47] + 2/3
ClearTopLine
  ; Clear Top Line
  ldx #0                  ; [49] + 2
  stx GRID+68             ; [51] + 3
  stx GRID+69             ; [54] + 3
  stx GRID+70             ; [57] + 3
  stx GRID+71             ; [60] + 3
  
  ; Line Compressed
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  ; Store Mode/Game Bits
  lax LINES+1             ; [5] + 3
  and #%11110000          ; [8] + 2
  sta TEMP                ; [10] + 3
  ; Handle Game Modes
  and #%00110000          ; [13] + 2
  beq MarathonLine        ; [15] + 2/3
  cmp #3<<4               ; [17] + 2
  beq EndCompress         ; [19] + 2/3    Skip Ultra Mode

SprintLine
  ; Decrement Lines
  sed                     ; [21] + 2
  sec                     ; [23] + 2
  lda LINES+0             ; [25] + 3
  beq NoSprintDec         ; [28] + 2/3    Prevent Count Going Negative
  sbc #1                  ; [30] + 2
  sta LINES+0             ; [32] + 3      LINES+1 not used in Sprint Mode
NoSprintDec
  cld                     ; [35] + 2
  jmp EndCompress         ; [37] + 3      = 40

MarathonLine
  ; Increment Lines
  sed                     ; [18] + 2
  clc                     ; [20] + 2
  lda LINES+0             ; [22] + 3
  adc #1                  ; [25] + 2
  sta LINES+0             ; [27] + 3
  txa                     ; [30] + 2
  adc #0                  ; [32] + 2
  and #%00001111          ; [34] + 2      Restrict to 999 Lines
  ora TEMP                ; [36] + 3
  sta LINES+1             ; [39] + 3
  cld                     ; [42] + 2
  
  ; Check for Lines > 100
  and #%00001111          ; [44] + 2
  beq NoLineShift         ; [46] + 2/3
  lda #10                 ; [48] + 2
NoLineShift
  sta TEMP2               ; [50] + 3

  ; Calculate Lines/10 + Shift
  lax LINES+0             ; [53] + 3
  lsr                     ; [56] + 2
  lsr                     ; [58] + 2
  lsr                     ; [60] + 2
  lsr                     ; [62] + 2
  adc TEMP2               ; [64] + 3
  sta TEMP2               ; [67] + 3

  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; Check Level Up on Multiples of 10 Lines
  lax LINES+0             ; [5] + 3
  and #%00001111          ; [8] + 2
  bne EndCompress         ; [10] + 2/3

  ; Compare Level Against Lines/10
  lax LEVEL               ; [12] + 3
  and #%11100000          ; [15] + 2
  sta TEMP                ; [17] + 3
  txa                     ; [20] + 2
  and #%00011111          ; [22] + 2
  cmp TEMP2               ; [24] + 3
  bcs EndCompress         ; [27] + 2/3
  
  ; Update Level
  lda TEMP2               ; [29] + 3
  ora TEMP                ; [32] + 3
  sta LEVEL               ; [35] + 3

EndCompress
  ; Compress Sound
  lda SFX                 ; [40] + 3
  beq PlayCompressSound   ; [43] + 2/3
  and #%11100000          ; [45] + 2
  cmp #5<<5               ; [47] + 2
  bcc EndCompressSound    ; [49] + 2/3
PlayCompressSound
  ldx #((5<<5)|3)         ; [51] + 2
  stx SFX                 ; [53] + 3
EndCompressSound
  jmp StartKernel         ; [56] + 3      = 59

EndCompressMode
  ; Check Game Mode
  lax LINES+1             ; [32] + 3
  and #%00110000          ; [35] + 2
  beq NextShape           ; [37] + 2/3    Marathon
  cmp #3<<4               ; [39] + 2
  beq NextShape           ; [41] + 2/3    Ultra
  ; Check For End of Sprint Mode
  lda LINES+0             ; [43] + 3
  bne NextShape           ; [46] + 2/3    Sprint
  ; Set GameOver Mode
  txa                     ; [48] + 2
  ora #%11000000          ; [50] + 3
  sta LINES+1             ; [53] + 3
  ; Set GameOver Type
  ldx #128|GAMEOVERTIME   ; [56] + 2
  stx TIMER               ; [58] + 3
  jsr SendPlusROMScore
  jmp StartKernel         ; [61] + 3      = 64

NextShape
  ; Clear Modes
  lda LINES+1             ; [49] + 3
  and #%00111111          ; [52] + 2
  sta LINES+1             ; [54] + 3
  jmp Spawn               ; [57] + 3

; -----------------------------------------------------------------------------

SettleMode
  ; Check Timer (0 = New Shape)
  lda TIMER               ; [28] + 3
  and #%00011111          ; [31] + 2
  beq NextShape           ; [33] + 2/3
  ; Reset Line Count
  lda #0                  ; [35] + 2
  sta TEMP                ; [37] + 3
  ; Test For Completed Lines
  jmp CheckShapeLine      ; [40] + 3
EndCheckShapeLine
  ; Check Line Counter (in TEMP)
  lda TEMP                ; [8] + 3
  beq EndNewLines         ; [11] + 2/3
  ; Play Sound Effect
  asl                     ; [13] + 2
  asl                     ; [15] + 2
  asl                     ; [17] + 2
  asl                     ; [19] + 2
  asl                     ; [21] + 2
  ora #30                 ; [23] + 2      Sound Length
  sta SFX                 ; [25] + 3
  ; Store Lines in XPOS
  lda XPOS                ; [28] + 3
  and #%11110000          ; [31] + 2
  ora TEMP                ; [33] + 3
  sta XPOS                ; [36] + 3
  ; Set Flash Timer
  lda TIMER               ; [39] + 3
  ora #FLASHTIME          ; [42] + 2
  sta TIMER               ; [44] + 3
  ; Set Flash Mode
  lda LINES+1             ; [47] + 3
  and #%00111111          ; [50] + 2
  ora #%10000000          ; [52] + 2
  sta LINES+1             ; [54] + 3
EndNewLines
  jmp StartKernel         ; [57] + 3      = 60

; -----------------------------------------------------------------------------

Spawn
  ; Generate New Shape
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  ; Defauly Row & Rotation
  ldy #18                 ; [5] + 2
  ; Get Drop Bit
  lda XPOS                ; [7] + 3
  and #%10000000          ; [10] + 2
  sta TEMP                ; [12] + 3
  ; Copy Previous Piece
  lda TIMER               ; [15] + 3
  and #%11100000          ; [18] + 2
  ; Increment Y For I Piece
  cmp #(1<<5)             ; [20] + 2
  beq DoYAdjust           ; [22] + 2/3
  bne NoYAdjust           ; [24] + 3
DoYAdjust
  iny                     ; [25] + 2      Increment Row
NoYAdjust
  lsr                     ; [27] + 2
  ora #4                  ; [29] + 2      Middle Position
  ora TEMP                ; [31] + 3
  sta XPOS                ; [34] + 3
  sty YPOS                ; [37] + 3
  ; Generate New Piece
  ldy RANDOM              ; [40] + 3
  lda Mod7,Y              ; [43] + 4
  sta TIMER               ; [47] + 3
  ; Set Drop & L/R Delays
  lda DEBOUNCE            ; [50] + 3
  ora #(VDELAY | HDELAY)  ; [53] + 2
  sta DEBOUNCE            ; [55] + 3
  
  ; Get Shape Details
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  LOADTEMPS               ; [5] + 51
  ; Check Overlap
  jsr CheckOverlap        ; [0] + 13
  bne FullGameOver        ; [13] + 2/3
  ; Reset Timer
  SETTIMER                ; [15] + 32
  ; Display Shape
  jmp ShapeCalc           ; [47] + 3      = 50

FullGameOver
  ; Set GameOver Mode
  lda LINES+1             ; [16] + 3
  ora #%11000000          ; [19] + 2
  sta LINES+1             ; [21] + 3
  ; Set Timer/Clear Next Piece
  ldx #GAMEOVERTIME       ; [24] + 2
  stx TIMER               ; [26] + 3
  ; Play Fail Sound
  ldx #((7<<5)|31)        ; [29] + 2
  stx SFX                 ; [31] + 3
  ; Stop Music
  lda #0                  ; [34] + 2
  sta MUSIC               ; [36] + 3
  sta AUDV0               ; [39] + 3
  sta TUNERESET_W         ; [42] + 4
  jsr SendPlusROMScore

  
ShapeCalc
  ; Recalculate Shape Values
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  SHAPEVALS               ; [5] + 33
  jmp ShowShape           ; [38] + 3      = 41

; -----------------------------------------------------------------------------

NormalMode
  ; Check Hard Drop
  lda YPOS                ; [27] + 3
  bpl NoHardDrop          ; [30] + 2/3
  jmp DropShape           ; [32] + 3      = 35
NoHardDrop

  ; Check Down Debounce
  lax DEBOUNCE            ; [33] + 3
  and #%11100111          ; [36] + 2
  sta TEMP                ; [38] + 3
  txa                     ; [41] + 2
  and #%00011000          ; [43] + 2
  beq JoyDown             ; [45] + 2/3
  sec                     ; [47] + 2
  sbc #8                  ; [49] + 2
  and #%00011000          ; [51] + 2
  ora TEMP                ; [53] + 3
  sta DEBOUNCE            ; [56] + 3
  jmp EndJoyDown          ; [59] + 3      = 62
JoyDown
  ; Check Down
  lda #%00100000          ; [48] + 2
  bit SWCHA               ; [50] + 3
  bne EndJoyDown          ; [53] + 2/3    = 56
  ; Debounce Down
  txa                     ; [55] + 2
  ora #VDELAY             ; [57] + 2
  sta DEBOUNCE            ; [59] + 3
  ; Drop Shape
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  jmp DropShape           ; [5] + 3       = 8
EndJoyDown

; -----------------------------------------------------------------------------

  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; Fetch Previous Joystick State (Left/Right only)
  lax DEBOUNCE            ; [5] + 3
  and #%11000000          ; [8] + 2
  sta TEMP                ; [10] + 3
  ; Fetch Current Joystick State (Left/Right only)
  lda SWCHA               ; [13] + 3
  and #%11000000          ; [16] + 2
  ; Reset Debounce If Joystick Has Moved
  cmp TEMP                ; [18] + 3
  bne ResetDebounce       ; [21] + 2/3
  ; Fetch Joystick & Button State
  txa                     ; [23] + 2
  and #%11111000          ; [25] + 2
  sta TEMP                ; [27] + 3
  ; Repeat Joystick Movement When Counter reaches Zero (Auto-Repeat)
  txa                     ; [30] + 2
  and #%00000111          ; [32] + 2
  beq JoyLeftRight        ; [34] + 2/3
  ; Decrement Debounce Counter
  sec                     ; [36] + 2
  sbc #1                  ; [38] + 2
  ora TEMP                ; [40] + 3
  sta DEBOUNCE            ; [43] + 3
  jmp EndJoyLeftRight     ; [46] + 3
ResetDebounce
  ; Update Joystick State & Reset Counter
  sta TEMP                ; [24] + 3
  txa                     ; [27] + 2
  and #%00111000          ; [29] + 2
  ora TEMP                ; [31] + 3
  sta DEBOUNCE            ; [34] + 3
JoyLeftRight
  ; Check for Left/Right
  bit DEBOUNCE            ; [37] + 3
  bpl JoyRight            ; [40] + 2/3
  bvc JoyLeft             ; [42] + 2/3
  jmp EndJoyLeftRight     ; [44] + 3
JoyLeft
  ; Move Left
  dec TEMPX               ; [45] + 5
  jmp TestPosition        ; [50] + 3
JoyRight
  ; Move Right
  inc TEMPX               ; [43] + 5
TestPosition
  ; Check For Overlap
  jsr CheckOverlap        ; [0] + 13
  beq StoreLeftRight      ; [13] + 2/3
  ; Restore Previous Value
  lda TEMPX2              ; [15] + 3
  sta TEMPX               ; [18] + 3
  jmp EndJoyLeftRight     ; [21] + 3
StoreLeftRight
  ; Store Position
  ldx TEMPX               ; [16] + 3
  inx                     ; [19] + 2
  bmi EndJoyLeftRight     ; [21] + 2/3   (Paranoia)
  stx TEMP                ; [23] + 3
  lda XPOS                ; [26] + 3
  and #%11110000          ; [29] + 2
  ora TEMP                ; [31] + 3
  sta XPOS                ; [34] + 3
  ; Debounce Left/Right
  lda DEBOUNCE            ; [37] + 3
  and #%11111000          ; [40] + 2
  ora #HDELAY             ; [42] + 2
  sta DEBOUNCE            ; [44] + 3
EndJoyLeftRight

; -----------------------------------------------------------------------------

  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; Check Fire Button
  lda INPT4               ; [5] + 3
  bmi ResetFire           ; [8] + 2/3
  ; Check Previous Button State
  lda DEBOUNCE            ; [10] + 3
  and #%00100000          ; [13] + 2
  bne EndJoyFire          ; [15] + 2/3    = 18
  ; Rotate Piece
  clc                     ; [17] + 2
  lda TEMPROT             ; [19] + 3
  adc #32                 ; [22] + 2
  and #%01100000          ; [24] + 2
  sta TEMPROT             ; [26] + 3
  ; Recalculate Shape Pointer
  lsr                     ; [29] + 2
  lsr                     ; [31] + 2
  lsr                     ; [33] + 2
  sta TEMP                ; [35] + 3
  ; Get Shape Pointer (Type * 32 + Rotation * 4)
  lda TEMPVAL             ; [38] + 3
  asl                     ; [41] + 2
  ora TEMP                ; [43] + 3
  sta SPTR                ; [46] + 3
  ; Check Overlap
  jsr CheckOverlap        ; [0] + 13
  beq StoreRotate         ; [13] + 2/3
  ; Restore Previous Values
  lda TEMPROT2            ; [15] + 3
  sta TEMPROT             ; [18] + 3
  lda SPTR2               ; [21] + 3
  sta SPTR                ; [24] + 3
  jmp EndJoyFire          ; [27] + 3      = 30
StoreRotate
  ; Set Fire Button State
  lda DEBOUNCE            ; [16] + 3
  ora #%00100000          ; [19] + 2
  sta DEBOUNCE            ; [21] + 3
  ; Update Rotation
  lda YPOS                ; [24] + 3
  and #%10011111          ; [27] + 2
  ora TEMPROT             ; [29] + 3
  sta YPOS                ; [32] + 3
  ; Rotate Sound
  lda SFX                 ; [35] + 3
  beq PlayRotSound        ; [38] + 2/3
  and #%11100000          ; [40] + 2
  cmp #6<<5               ; [42] + 2
  bcc EndJoyFire          ; [44] + 2/3
PlayRotSound
  ldx #((6<<5)|7)         ; [46] + 2
  stx SFX                 ; [48] + 3
  jmp EndJoyFire          ; [51] + 3      = 54
ResetFire
  ; Clear Fire Button State
  lda DEBOUNCE            ; [11] + 3
  and #%11011111          ; [14] + 2
  sta DEBOUNCE            ; [16] + 3
EndJoyFire

; -----------------------------------------------------------------------------

  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; Check Up
  lda SWCHA               ; [5] + 3
  cmp #%11000000          ; [8] + 2
  bcc ResetJoyUp          ; [10] + 2/3    Skip Diagonals
  and #%00010000          ; [12] + 2
  bne ResetJoyUp          ; [14] + 2/3
  ; Check Previous State
  lda XPOS                ; [16] + 3
  bmi EndJoyUp            ; [19] + 2/3    = 22
  ; Skip Hard Drop In P1 Beginner Mode
  bit SWCHB               ; [21] + 3
  bpl EndJoyUp            ; [24] + 2/3    = 27
  ; Set Up State
  ora #%10000000          ; [26] + 2
  sta XPOS                ; [28] + 3
  ; Start Hard Drop
  lda YPOS                ; [31] + 3
  ora #%10000000          ; [34] + 2
  sta YPOS                ; [36] + 3
  jmp DropShape           ; [39] + 3      = 42
ResetJoyUp
  lda XPOS                ; [17] + 3
  and #%01111111          ; [20] + 2
  sta XPOS                ; [22] + 3      = 21
EndJoyUp

  ; Check Drop Timer
  lda TIMER               ; [27] + 3
  and #%00011111          ; [30] + 2
  bne EndDropShape        ; [32] + 2/3    = 35
DropShape
  ; New Y Position
  dec TEMPY               ; [42] + 5
  bmi StartSettle         ; [47] + 2/3
  ; Check Overlap
  jsr CheckOverlap        ; [0] + 13
  bne StartSettle2        ; [13] + 2/3
  ; Update Position
  lda YPOS                ; [15] + 3
  and #%11100000          ; [18] + 2
  ora TEMPY               ; [20] + 3
  sta YPOS                ; [23] + 3
  ; Reset Timer
  SETTIMER                ; [26] + 32
  jmp ShowShape           ; [58] + 3      = 61
StartSettle
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
StartSettle2
  ; Restore Previous Value
  inc TEMPY               ; [16] + 5
  ; Settle Mode
  lda LINES+1             ; [21] + 3
  ora #%01000000          ; [24] + 2
  sta LINES+1             ; [26] + 3
  ; Settle Timer (2 phases)
  lda TIMER               ; [29] + 3
  and #%11100000          ; [32] + 2
  ora #%00000010          ; [34] + 2
  sta TIMER               ; [36] + 3
  ; Settle Sound
  lda SFX                 ; [39] + 3
  beq PlaySettleSound     ; [42] + 2/3
  and #%11100000          ; [44] + 2
  cmp #5<<5               ; [46] + 2
  bcc EndDropShape        ; [48] + 2/3    = 51
PlaySettleSound
  ldx #((5<<5)|3)         ; [50] + 2
  stx SFX                 ; [52] + 3
EndDropShape

; -----------------------------------------------------------------------------

ShowShape
  ; Draw Shape
  jsr DrawShape           ; [0] + 11
  jmp StartKernel         ; [11] + 3      = 14

; -----------------------------------------------------------------------------
; OVERSCAN REGION (AFTER KERNEL)
; -----------------------------------------------------------------------------

OverLogic
  ; Check for Pause
  lda CYCLE               ; [20] + 3
  bmi DeleteShape         ; [23] + 2/3

  ; Check for Game Over
  ldx LINES+1             ; [25] + 3
  cpx #%11000000          ; [28] + 2
  bcc UpdateTimer         ; [30] + 2/3
  
  ; Slow Timer During GameOver
  lsr                     ; [32] + 2
  bcs SkipDelete          ; [34] + 2/3    = 37
  lsr                     ; [36] + 2
  bcs SkipDelete          ; [38] + 2/3    = 41

UpdateTimer
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; Update Drop Timer
  lax TIMER               ; [5] + 3
  and #%11100000          ; [8] + 2
  sta TEMP                ; [10] + 3
  txa                     ; [13] + 2
  and #%00011111          ; [15] + 2
  beq SkipTimer           ; [17] + 2/3
  sec                     ; [19] + 2
  sbc #1                  ; [21] + 2
  ora TEMP                ; [23] + 3
  sta TIMER               ; [26] + 3
SkipTimer

; -----------------------------------------------------------------------------

  ; Check for Game Over
  lax LINES+1             ; [29] + 3
  cmp #%11000000          ; [32] + 2
  bcc CheckUltraMode      ; [34] + 2/3
SkipDelete
  jmp EndOverLogic        ; [36] + 3

CheckUltraMode
  ; Check for Ultra Mode
  txa                     ; [36] + 2
  and #%00110000          ; [38] + 2
  cmp #3<<4               ; [40] + 2
  bne EndUltraCycle       ; [42] + 2/3

  ; Check for Zero Cycle
  lda CYCLE               ; [44] + 3
  and #%00111111          ; [47] + 2
  bne EndUltraCycle       ; [49] + 2/3

  ; Store Game Bits
  txa                     ; [51] + 2
  and #%11110000          ; [53] + 2
  sta TEMP                ; [55] + 3
  
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  
  ; Decrement Game Timer
  sed                     ; [5] + 2
  sec                     ; [7] + 2
  lda LINES+0             ; [9] + 3
  sbc #1                  ; [12] + 2
  sta LINES+0             ; [14] + 3
  txa                     ; [17] + 2
  and #%00001111          ; [19] + 2
  sbc #0                  ; [21] + 2
  ora TEMP                ; [23] + 3
  sta LINES+1             ; [26] + 3
  cld                     ; [29] + 2

  ; Check for Game End
  lda LINES+0             ; [31] + 3
  bne EndUltraCycle       ; [34] + 2/3
  lda LINES+1             ; [36] + 3
  and #%00001111          ; [39] + 2
  bne EndUltraCycle       ; [41] + 2/3
  ; Set GameOver Mode
  lda LINES+1             ; [43] + 3
  ora #%11000000          ; [46] + 2
  sta LINES+1             ; [48] + 3
  ; Prevent Fill
  ldx #64|GAMEOVERTIME    ; [51] + 2
  stx TIMER               ; [53] + 3
  ; Play Win Sound
  ldx #((0<<5)|30)        ; [56] + 2
  stx SFX                 ; [58] + 3
EndUltraCycle

; -----------------------------------------------------------------------------

DeleteShape
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; Delete Shape If No Mode Set
  lda LINES+1             ; [5] + 3
  and #%11000000          ; [8] + 2
  bne EndOverLogic        ; [10] + 2/3

  ; Set Zero Values To Delete
  ; lda #0
  sta VALUE1              ; [12] + 3
  sta VALUE2              ; [15] + 3
  sta VALUE3              ; [18] + 3
  sta VALUE4              ; [21] + 3
  
  ; Get Shape Details
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  LOADTEMPS               ; [5] + 51
  ; Delete Shape
  jsr DrawShape           ; [0] + 11
  
; -----------------------------------------------------------------------------

EndOverLogic
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; Test B&W/Pause Switch
  lda SWCHB               ; [5] + 3
  and #%00001000          ; [8] + 2
  cmp #%00001000          ; [10] + 2
  ; Fetch Pause Status
  lda CYCLE               ; [12] + 3
  ; Check For 7800 Mode
  ldx INITIALS+1          ; [15] + 3
  bmi Pause2600           ; [18] + 2/3
Pause7800
  bcs PauseNotPressed     ; [20] + 2/3
  ; Previously Pressed?
  and #%01000000          ; [22] + 2
  bne EndPause            ; [24] + 2/3
  ; Toggle Pause & Button Status
  lda CYCLE               ; [26] + 3
  eor #%11000000          ; [29] + 2
  jmp StorePause          ; [31] + 3
PauseNotPressed
  ; Clear Previous Status
  and #%10111111          ; [23] + 2
  jmp StorePause          ; [25] + 3
Pause2600
  bcs NoPause             ; [21] + 2/3
PauseGame
  ora #%10000000          ; [23] + 2
  bne StorePause          ; [25] + 2/3
NoPause
  and #%01111111          ; [27] + 2
StorePause
  sta CYCLE               ; [34] + 3
EndPause

  ; Check For Reset Switch
  lda SWCHB               ; [37] + 3
  lsr                     ; [40] + 2
  bcs NoReset             ; [42] + 2/3
  ; Reset Score
  ldx #0                  ; [44] + 2
  stx SCORE+0             ; [46] + 3
  stx SCORE+1             ; [49] + 3
  stx SCORE+2             ; [52] + 3
  ; Skip To Next Frame & Quit
  WAIT_OVERSCAN_MUSIC
  START_VBLANK_MUSIC
  jmp EndGame
NoReset

  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; Update Game Timer
  lax CYCLE               ; [5] + 3
  and #%11000000          ; [8] + 2
  sta TEMP                ; [10] + 3
  txa                     ; [13] + 2
  and #%00111111          ; [15] + 2
  sec                     ; [17] + 2
  sbc #1                  ; [19] + 2
  bmi ResetCycle          ; [21] + 2/3
  nop                     ; [23] + 2
  bpl StoreCycle          ; [25] + 3
ResetCycle
  ldx #CYCLETIME          ; [24] + 2
  txa                     ; [26] + 2
StoreCycle
  ora TEMP                ; [28] + 3
  sta CYCLE               ; [31] + 3

  ; Update Random Number Generator
  jsr GetRandom           ; [34] + 24 + 6 = 64

  WAIT_OVERSCAN_MUSIC
  jmp MainLoop

; -----------------------------------------------------------------------------
; END GAME (CHECK SPECIAL)
; -----------------------------------------------------------------------------

  ; End Of Game
EndGame
  ; Clear Sound Effects & Music
  ldx #0
  stx AUDV0
  stx AUDV1
  sta TUNERESET_W
  
  ; Reset Stack Pointer
  ldx #$FF
  txs

  ; Check For Sprint Modes
  lax LINES+1
  and #%00110000
  beq CheckSpecial
  cmp #(3<<4)
  bcs CheckSpecial
  tay

  ; Check For Zero Lines (Level Complete)
  txa
  and #%00001111
  ora LINES+0
  bne CheckSpecial

  ; Check For Level 9 & Height 5
  lda LEVEL
  cmp #(9 | (5<<5))
  bne CheckSpecial

  ; Show Shuttle For Sprint40, Rocket for Sprint25
  ldx #0
  cpy #(2<<4)
  beq SpecialStore
  inx
  jmp SpecialStore

CheckSpecial
  ; Check For Special Scores (>= 100000)
  lda SCORE+2
  and #%11110000
  bne SpecialScore

  ; Finish Game
  jmp FinishGame

  ; Display Ship Launch
SpecialScore
  ldx #0                    ; Shuttle at 500000
  cmp #%01010000
  bcs SpecialStore
  inx                       ; Large Rocket at 200000
  cmp #%00100000
  bcs SpecialStore
  inx                       ; Small Rocket at 100000
SpecialStore
  stx SHIP

  ; Animation Variables
  ldx #4
  stx PHASE
  ldx #STARTFRAMES
  stx FRAME

  ; Start Special (Load Tune)
  jmp StartSpecial

; -----------------------------------------------------------------------------
; GAME DATA
; -----------------------------------------------------------------------------

  ; Level Speeds (Levels 1 -> 20)
Levels
  IF (PALMODE)
  DC.B  26, 24, 22, 20, 18
  DC.B  16, 15, 14, 13, 9
  DC.B  8, 7, 6, 6, 5
  DC.B  5, 4, 4, 3, 3
  ELSE
  DC.B  31, 29, 27, 25, 23
  DC.B  21, 19, 17, 15, 11
  DC.B  10, 9, 8, 7, 6
  DC.B  6, 5, 5, 4, 4
  ENDIF
EndLevels
  if (>Levels != >EndLevels)
    echo "WARNING: Levels Data Crosses Page Boundary!"
  endif

  DC.B  "LENIN"

  IF PLUSROM
PlusROM_API
    .byte "a", 0, "h.firmaplus.de", 0

SendPlusROMScore:

   lda LINES+1
   sta WriteToBuffer
   lda LEVEL
   sta WriteToBuffer
   lda SCORE+2
   sta WriteToBuffer
   lda SCORE+1
   sta WriteToBuffer
   lda SCORE+0
   sta WriteToBuffer

   lda #HIGHSCORE_ID                ; game id in Highscore DB
   sta WriteSendBuffer
   rts
  ENDIF

  ALIGN 256

  ; Row Offsets for Shape Drawing (256 Bytes)
ShapePosition
  DC.B  0, 4, 8, 12, 16, 20, 24, 28, 32
  DC.B  36, 40, 44, 48, 52, 56, 60, 64, 68
  DS.B  238, $FF

  ALIGN 256

  ; Shape Probabilities (256 bytes)
  ; (((Pos % 7) + 1) * 32)
Mod7:
  DC.B  0, 32, 64, 96, 128, 160, 192, 224
  DC.B  32, 64, 96, 128, 160, 192, 224, 32
  DC.B  64, 96, 128, 160, 192, 224, 32, 64
  DC.B  96, 128, 160, 192, 224, 32, 64, 96
  DC.B  128, 160, 192, 224, 32, 64, 96, 128
  DC.B  160, 192, 224, 32, 64, 96, 128, 160
  DC.B  192, 224, 32, 64, 96, 128, 160, 192
  DC.B  224, 32, 64, 96, 128, 160, 192, 224
  DC.B  32, 64, 96, 128, 160, 192, 224, 32
  DC.B  64, 96, 128, 160, 192, 224, 32, 64
  DC.B  96, 128, 160, 192, 224, 32, 64, 96
  DC.B  128, 160, 192, 224, 32, 64, 96, 128
  DC.B  160, 192, 224, 32, 64, 96, 128, 160
  DC.B  192, 224, 32, 64, 96, 128, 160, 192
  DC.B  224, 32, 64, 96, 128, 160, 192, 224
  DC.B  32, 64, 96, 128, 160, 192, 224, 32
  DC.B  64, 96, 128, 160, 192, 224, 32, 64
  DC.B  96, 128, 160, 192, 224, 32, 64, 96
  DC.B  128, 160, 192, 224, 32, 64, 96, 128
  DC.B  160, 192, 224, 32, 64, 96, 128, 160
  DC.B  192, 224, 32, 64, 96, 128, 160, 192
  DC.B  224, 32, 64, 96, 128, 160, 192, 224
  DC.B  32, 64, 96, 128, 160, 192, 224, 32
  DC.B  64, 96, 128, 160, 192, 224, 32, 64
  DC.B  96, 128, 160, 192, 224, 32, 64, 96
  DC.B  128, 160, 192, 224, 32, 64, 96, 128
  DC.B  160, 192, 224, 32, 64, 96, 128, 160
  DC.B  192, 224, 32, 64, 96, 128, 160, 192
  DC.B  224, 32, 64, 96, 128, 160, 192, 224
  DC.B  32, 64, 96, 128, 160, 192, 224, 32
  DC.B  64, 96, 128, 160, 192, 224, 32, 64
  DC.B  96, 128, 160, 192, 224, 32, 64, 96

  ALIGN 256

  ; Shapes Data (256 bytes)
ShapeData:
NoShape:                  ; 0
  DC.B  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  DC.B  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
IShape:                   ; 32
  DC.B  0, 1, 2, 3, 1, 1, 1, 1, 0, 1, 2, 3, 1, 1, 1, 1
  DC.B  2, 2, 2, 2, 0, 1, 2, 3, 2, 2, 2, 2, 0, 1, 2, 3
JShape:                   ; 64
  DC.B  0, 1, 2, 2, 1, 1, 1, 0, 0, 0, 1, 2, 1, 1, 1, 2
  DC.B  1, 1, 1, 2, 0, 1, 2, 2, 0, 1, 1, 1, 0, 1, 2, 0
LShape:                   ; 96
  DC.B  0, 0, 1, 2, 0, 1, 1, 1, 0, 1, 2, 2, 1, 1, 1, 2
  DC.B  1, 2, 1, 1, 0, 0, 1, 2, 1, 1, 1, 0, 0, 1, 2, 2
OShape:                   ; 128
  DC.B  1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2
  DC.B  1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2
SShape:                   ; 160
  DC.B  0, 1, 1, 2, 0, 0, 1, 1, 0, 1, 1, 2, 0, 0, 1, 1
  DC.B  2, 2, 1, 1, 0, 1, 1, 2, 2, 2, 1, 1, 0, 1, 1, 2
TShape:                   ; 192
  DC.B  0, 1, 2, 1, 1, 1, 1, 0, 0, 1, 2, 1, 1, 1, 1, 2
  DC.B  1, 1, 1, 2, 0, 1, 2, 1, 1, 1, 1, 0, 0, 1, 2, 1
ZShape:                   ; 224
  DC.B  0, 1, 1, 2, 1, 1, 0, 0, 0, 1, 1, 2, 1, 1, 0, 0
  DC.B  1, 1, 2, 2, 0, 1, 1, 2, 1, 1, 2, 2, 0, 1, 1, 2

  ALIGN 256

  ; Score Table
ScoreLo:
  DC.B  %00000000         ; 0     (0 Lines)
  DC.B  %01000000         ; 40    (1 Line)
  DC.B  %00000000         ; 0
ScoreHi:
  DC.B  %00000000         ; 0
  DC.B  %00000000         ; 0
  DC.B  %00000001         ; 100   (2 Lines)
  DC.B  %00000011         ; 300   (3 Lines)
  DC.B  %00010010         ; 1200  (4 Lines = Chetiry!)
  
; -----------------------------------------------------------------------------
; SUBROUTINES
; -----------------------------------------------------------------------------

  ; Add Score
  ; XPOS = Lines
AddScore
  ; Get Level
  lda LEVEL               ; [40] + 3
  and #%00011111          ; [43] + 2
  tax                     ; [45] + 2
  lda XPOS                ; [47] + 3
  and #%00001111          ; [50] + 2
  tay                     ; [52] + 2
  sed                     ; [54] + 2
  sta WSYNC               ; [0]
AddScoreLoop
  ; Add Score (Part 1)
  PLAY_MUSIC              ; [0] + 5
  clc                     ; [5] + 2
  lda SCORE+0             ; [7] + 3
  adc ScoreLo,Y           ; [10] + 4
  sta SCORE+0             ; [14] + 3
  lda SCORE+1             ; [17] + 3
  adc ScoreHi,Y           ; [20] + 4
  sta SCORE+1             ; [24] + 3
  lda SCORE+2             ; [27] + 3
  adc #0                  ; [30] + 2
  sta SCORE+2             ; [32] + 3
  dex                     ; [35] + 2
  bmi EndAddScoreLoop     ; [37] + 2/3
  ; Add Score (Part 2)
  clc                     ; [39] + 2
  lda SCORE+0             ; [41] + 3
  adc ScoreLo,Y           ; [44] + 4
  sta SCORE+0             ; [48] + 3
  lda SCORE+1             ; [51] + 3
  adc ScoreHi,Y           ; [54] + 4
  sta SCORE+1             ; [58] + 3
  lda SCORE+2             ; [61] + 3
  adc #0                  ; [64] + 2
  sta SCORE+2             ; [66] + 3
  dex                     ; [69] + 2
  nop                     ; [71] + 2
  bpl AddScoreLoop        ; [73] + 2/3
EndAddScore
  PLAY_MUSIC              ; [75] + 5
EndAddScoreLoop
  cld                     ; [40] + 2
  jmp StartKernel         ; [42] + 3      = 45

; -----------------------------------------------------------------------------

CheckShapeLine
  ; Shape Part 1
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  sec                     ; [5] + 2
  ldx SPTR                ; [7] + 3
  lda TEMPY               ; [10] + 3
  sbc ShapeData+19,X      ; [13] + 4
  jsr CheckRow            ; [17] + 6
  ; Shape Part 2
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  sec                     ; [5] + 2
  ldx SPTR                ; [7] + 3
  lda TEMPY               ; [10] + 3
  sbc ShapeData+18,X      ; [13] + 4
  jsr CheckRow            ; [17] + 6
  ; Shape Part 3
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  sec                     ; [5] + 2
  ldx SPTR                ; [7] + 3
  lda TEMPY               ; [10] + 3
  sbc ShapeData+17,X      ; [13] + 4
  jsr CheckRow            ; [17] + 6
  ; Shape Part 4
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  sec                     ; [5] + 2
  ldx SPTR                ; [7] + 3
  lda TEMPY               ; [10] + 3
  sbc ShapeData+16,X      ; [13] + 4
  jsr CheckRow            ; [17] + 6
  ; Check For New Lines (Count in TEMP)
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  jmp EndCheckShapeLine   ; [5] + 3

; -----------------------------------------------------------------------------

  ; Check for Complete Row
  ; A = Row (0-18)
CheckRow
  ; Check For Invalid Row
  cmp #18                 ; [23] + 2
  bcs EndCheckLine        ; [25] + 2/3
  asl                     ; [27] + 2
  asl                     ; [29] + 2
  tay                     ; [31] + 2
  lax GRID+0,Y            ; [33] + 4
  and #%00000111          ; [37] + 2
  beq EndCheckLine        ; [39] + 2/3
  txa                     ; [41] + 2
  rol                     ; [43] + 2
  and #%01110000          ; [45] + 2
  beq EndCheckLine        ; [47] + 2/3
  lax GRID+1,Y            ; [49] + 4
  and #%00000111          ; [53] + 2
  beq EndCheckLine        ; [55] + 2/3
  txa                     ; [57] + 2
  and #%00111000          ; [59] + 2
  beq EndCheckLine        ; [61] + 2/3
  txa                     ; [63] + 2
  ror                     ; [65] + 2
  and #%11100000          ; [67] + 2
  beq EndCheckLine        ; [69] + 2/3
  
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  
  lax GRID+2,Y            ; [5] + 4
  and #%00000111          ; [9] + 2
  beq EndCheckLine        ; [11] + 2/3
  txa                     ; [13] + 2
  rol                     ; [15] + 2
  and #%01110000          ; [17] + 2
  beq EndCheckLine        ; [19] + 2/3
  lax GRID+3,Y            ; [21] + 4
  and #%00000111          ; [25] + 2
  beq EndCheckLine        ; [27] + 2/3
  txa                     ; [29] + 2
  and #%00111000          ; [31] + 2
  beq EndCheckLine        ; [33] + 2/3
  txa                     ; [35] + 2
  ror                     ; [37] + 2
  and #%11100000          ; [39] + 2
  beq EndCheckLine        ; [41] + 2/3
FoundLine
  ; Check If Line Already Marked
  lax GRID+0,Y            ; [43] + 4
  and #%01000000          ; [47] + 2
  bne EndCheckLine        ; [49] + 2/3
  ; Mark Line
  txa                     ; [51] + 2
  ora #%01000000          ; [53] + 2
  sta GRID+0,Y            ; [55] + 4
  ; Increment Counter
  inc TEMP                ; [59] + 5
EndCheckLine
  rts                     ; [64] + 6 = 70

  DC.B  "X0"

; -----------------------------------------------------------------------------

  ALIGN 256
  
  ; Next Random Number
GetRandom
  lda RANDOM              ; [0] + 3
  beq ResetRandom         ; [3] + 2/3
  bne NoResetRandom       ; [5] + 3
ResetRandom
  lda #RANDOMSEED         ; [6] + 2
NoResetRandom
  asl                     ; [8] + 2
  bcs RndEor              ; [10] + 2/3
  bcc NoRndEor            ; [12] + 3
RndEor
  eor #$AF                ; [13] + 2
NoRndEor
  sta RANDOM              ; [15] + 3
  rts                     ; [18] + 6    = 24

; -----------------------------------------------------------------------------

; Check Shape Overlap
; TEMPX = Column (0-9)
; TEMPY = Row (0-19)
; SPTR = Shape Pointer
CheckOverlap
  ; Part 1
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  clc                     ; [5] + 2
  ldy SPTR                ; [7] + 3
  lda ShapeData+3,Y       ; [10] + 4
  adc TEMPX               ; [14] + 3
  tax                     ; [17] + 2
  sec                     ; [19] + 2
  lda TEMPY               ; [21] + 3
  sbc ShapeData+19,Y      ; [24] + 4
  GETDATA EndOverlap1     ; [28] + 41/42
EndOverlap1
  bne FoundOverlap1       ; [70] + 2/3

  ; Part 2
CheckOverlap2
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  clc                     ; [5] + 2
  ldy SPTR                ; [7] + 3
  lda ShapeData+2,Y       ; [10] + 4
  adc TEMPX               ; [14] + 3
  tax                     ; [17] + 2
  sec                     ; [19] + 2
  lda TEMPY               ; [21] + 3
  sbc ShapeData+18,Y      ; [24] + 4
  GETDATA EndOverlap2     ; [28] + 41
  beq CheckOverlap3       ; [69] + 2/4*
FoundOverlap1
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  lda #1                  ; [5] + 2
  rts                     ; [7] + 6     = 13

  DC.B  "X1"
  ALIGN 256

  ; Part 3
EndOverlap2
  bne FoundOverlap2       ; [70] + 2/3
CheckOverlap3
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  clc                     ; [5] + 2
  ldy SPTR                ; [7] + 3
  lda ShapeData+1,Y       ; [10] + 4
  adc TEMPX               ; [14] + 3
  tax                     ; [17] + 2
  sec                     ; [19] + 2
  lda TEMPY               ; [21] + 3
  sbc ShapeData+17,Y      ; [24] + 4
  GETDATA EndOverlap3     ; [28] + 42
EndOverlap3
  beq CheckOverlap4       ; [70] + 2/3
FoundOverlap2
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  lda #1                  ; [5] + 2
  rts                     ; [7] + 6     = 13

  ; Part 4
CheckOverlap4
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  clc                     ; [5] + 2
  ldy SPTR                ; [7] + 3
  lda ShapeData+0,Y       ; [10] + 4
  adc TEMPX               ; [14] + 3
  tax                     ; [17] + 2
  sec                     ; [19] + 2
  lda TEMPY               ; [21] + 3
  sbc ShapeData+16,Y      ; [24] + 4
  GETDATA EndOverlap4     ; [28] + 42
EndOverlap4
  bne FoundOverlap2       ; [70] + 2/3
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  lda #0                  ; [5] + 2
  rts                     ; [7] + 6    = 13

  DC.B  "X2"

; -----------------------------------------------------------------------------

  ALIGN 256

  ; Draw Current Shape on Grid
  ; TEMPX = Column (0-9)
  ; TEMPY = Row (0-19)
  ; SPTR = Shape Pointer
  ; VALUE1-4 = Shape Colour (0-7)
DrawShape
  ; Part 1
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  ldx SPTR                ; [5] + 3
  sec                     ; [8] + 2
  lda TEMPY               ; [10] + 3
  sbc ShapeData+19,X      ; [13] + 4
  tay                     ; [17] + 2
  lda ShapeData+3,X       ; [19] + 4
  ldx ShapePosition,Y     ; [23] + 4
  bmi DrawShape2A         ; [27] + 2/3
  clc                     ; [29] + 2
  adc TEMPX               ; [31] + 3
  tay                     ; [34] + 2
  SETDATA DrawShape2B, DrawShape2A

  ; Part 2
DrawShape2A
  sta WSYNC               ; [0]
DrawShape2B
  PLAY_MUSIC              ; [0] + 5
  ldx SPTR                ; [5] + 3
  sec                     ; [8] + 2
  lda TEMPY               ; [10] + 3
  sbc ShapeData+18,X      ; [13] + 4
  tay                     ; [17] + 2
  lda ShapeData+2,X       ; [19] + 4
  ldx ShapePosition,Y     ; [23] + 4
  bmi DrawShape2C         ; [27] + 2/3
  clc                     ; [29] + 2
  adc TEMPX               ; [31] + 3
  tay                     ; [34] + 2
  SETDATA DrawShape3B, DrawShape2C
  jmp DrawShape3B         ; [73] + 3
DrawShape2C
  jmp DrawShape3A

  DC.B  "X3"
  ALIGN 256

  ; Part 3
DrawShape3A
  sta WSYNC               ; [0]
DrawShape3B
  PLAY_MUSIC              ; [0] + 5
  ldx SPTR                ; [5] + 3
  sec                     ; [8] + 2
  lda TEMPY               ; [10] + 3
  sbc ShapeData+17,X      ; [13] + 4
  tay                     ; [17] + 2
  lda ShapeData+1,X       ; [19] + 4
  ldx ShapePosition,Y     ; [23] + 4
  bmi DrawShape4A         ; [27] + 2/3
  clc                     ; [29] + 2
  adc TEMPX               ; [31] + 3
  tay                     ; [34] + 2    = 36
  SETDATA DrawShape4B, DrawShape4A

  ; Part 4
DrawShape4A
  sta WSYNC               ; [0]
DrawShape4B
  PLAY_MUSIC              ; [0] + 5
  ldx SPTR                ; [5] + 3
  sec                     ; [8] + 2
  lda TEMPY               ; [10] + 3
  sbc ShapeData+16,X      ; [13] + 4
  tay                     ; [17] + 2
  lda ShapeData+0,X       ; [19] + 4
  ldx ShapePosition,Y     ; [23] + 4
  bmi DrawShape5A         ; [27] + 2/3
  clc                     ; [29] + 2
  adc TEMPX               ; [31] + 3
  tay                     ; [34] + 2    = 36
  SETDATA DrawShape5B, DrawShape5A
  
DrawShape5A
  sta WSYNC               ; [0]
DrawShape5B
  PLAY_MUSIC              ; [0] + 5
  rts                     ; [5] + 6     = 11

  DC.B  "X4"

  echo "----",($FFF4 - *) , "bytes left (BANK 1 - MAIN GAME)"

  ORG     $9FF4
  RORG    $FFF4
  DC.B    "BANK1", 0
  DC.W    (PlusROM_API - $D000)
  DC.W    Init1, Init1
