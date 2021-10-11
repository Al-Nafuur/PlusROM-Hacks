; -----------------------------------------------------------------------------
; CHETIRY - Atari 2600 - (C) Copyright 2011 - AtariAge
; BANK 6 - MAIN MENU
; -----------------------------------------------------------------------------

  SEG     BANK6
  ORG     $E000
  RORG    $F000

  ; Space for DPC registers
  DS.B    256,0

Init6
  ; Switch to Bank 7
  nop     $FFFB
  nop
  nop
  nop     ;
EndCredits
  ; Switch To Bank 7
  nop     $FFFB
BeginGame
  ; Switch to Bank 2
  nop     $FFF6
  nop
  nop
  nop     ;
  nop
  nop
  nop     ;
  ; Show Credits
  jmp     Credits

; -----------------------------------------------------------------------------
; MAIN MENU
; -----------------------------------------------------------------------------

BeginMenu
  ; Set Menu Variables
  ldx #3
  stx STATE
  ldx #0
  stx TMPGAME
  stx TMPMUSIC
  stx TMPLEVEL
  stx TMPHEIGHT

  ; Colour Cycling
  ldx #10
  stx STAROFF
  ldx #>MenuStarCols
  stx STARPTR+1
  ldx #RED
  stx LOGOCOL

  ; Sound Effects
  ldx #>MSoundTab
  stx SPTR+1

MenuLoop
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; Update Cycle
  dec CYCLE               ; [5] + 5

  ; Handle Joystick on Alternate Cycles
  lda CYCLE               ; [10] + 3
  lsr                     ; [13] + 2
  bcs MenuCheckReset      ; [15] + 2/3
  jmp EndMenuJoy          ; [17] + 3

  ; Check Reset Switch
MenuCheckReset
  lda SWCHB               ; [18] + 3
  lsr                     ; [21] + 2
  bcs EndMenuCheckReset   ; [23] + 2/3
  ; Check State
  lda STATE               ; [25] + 3
  cmp #3                  ; [28] + 2
  bcs EndMenuFire         ; [30] + 2/3
  ; Reset To Start State
  ldx #3                  ; [32] + 2
  stx STATE               ; [34] + 3
  ; Move Sound
  ldx #((1<<5)|3)         ; [37] + 2
  stx SFX                 ; [39] + 3
  jmp EndMenuFire         ; [42] + 3
EndMenuCheckReset

  ; Check Fire Button
MenuCheckFire
  lda INPT4               ; [26] + 3
  bmi ResetMenuFire       ; [29] + 2/3
  ; Check Previous Button State
  lax DEBOUNCE            ; [31] + 3
  and #%00001000          ; [34] + 2
  bne EndMenuFire         ; [36] + 2/3
  ; Set Fire Button State
  txa                     ; [38] + 2
  ora #%00001000          ; [40] + 2
  sta DEBOUNCE            ; [42] + 3
  ; Move Sound
  ldx #((1<<5)|3)         ; [45] + 2
  stx SFX                 ; [47] + 3
EndButtonSound
  ; Update State
  dec STATE               ; [50] + 5
  bpl EndMenuFire         ; [55] + 2/3
  ; Stop Music & SFX
  ldx #0
  stx SFX
  stx AUDV0
  stx AUDV1
  stx VDELP0
  stx VDELP1
  ; Start Game
  jmp BeginGame
ResetMenuFire
  ; Clear Fire Button State
  lda DEBOUNCE            ; [32] + 3
  and #%11110111          ; [35] + 2
  sta DEBOUNCE            ; [37] + 3
EndMenuFire

; -----------------------------------------------------------------------------
; MENu CONTROLS
; -----------------------------------------------------------------------------

  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; Debounce Joystick Directions
  lax DEBOUNCE            ; [5] + 3
  ; Fetch Previous Joystick State
  and #%11110000          ; [8] + 2
  sta TEMP                ; [10] + 3
  ; Fetch Current Joystick State
  lda SWCHA               ; [13] + 3
  and #%11110000          ; [16] + 2
  ; Reset Debounce If Joystick Has Moved
  cmp TEMP                ; [18] + 3
  bne ResetMenuDebounce   ; [21] + 2/3
  ; Fetch Joystick & Button State
  txa                     ; [23] + 2
  and #%11111000          ; [25] + 2
  sta TEMP                ; [27] + 3
  ; Repeat Joystick Movement When Counter reaches Zero (Auto-Repeat)
  txa                     ; [30] + 2
  and #%00000111          ; [32] + 2
  beq MenuJoy             ; [34] + 2/3
  ; Decrement Debounce Counter
  sec                     ; [36] + 2
  sbc #1                  ; [38] + 2
  ora TEMP                ; [40] + 3
  sta DEBOUNCE            ; [43] + 3
  ; Skip Line
  jmp EndMenuJoy          ; [46] + 3
ResetMenuDebounce
  ; Update Joystick State & Reset Counter
  sta TEMP                ; [24] + 3
  txa                     ; [27] + 2
  and #%00001000          ; [29] + 2
  ora TEMP                ; [31] + 3
  sta DEBOUNCE            ; [34] + 3

MenuJoy
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; State Machine
  ; State 3 = Game
  ; State 2 = Music
  ; State 1 = Level
  ; State 0 = Depth
  lda STATE               ; [5] + 3
  beq State0Joy           ; [8] + 2/3
  cmp #1                  ; [10] + 2
  beq State1Joy           ; [12] + 2/3
  cmp #2                  ; [14] + 2
  beq State2Joy           ; [16] + 2/3
  jmp State3Joy           ; [18] + 3

  ; Depth
State0Joy
  ldy TMPHEIGHT           ; [11] + 3
  lda #%00100000          ; [14] + 2
  bit DEBOUNCE            ; [16] + 3
  bpl State0JoyRight      ; [19] + 2/3
  bvc State0JoyLeft       ; [21] + 2/3
  beq State0JoyDown       ; [23] + 2/3
  lda DEBOUNCE            ; [25] + 3
  and #%00010000          ; [28] + 2
  bne EndState0Joy        ; [30] + 2/3
State0JoyUp
  cpy #3                  ; [32] + 2
  bcc EndState0Joy        ; [34] + 2/3
  tya                     ; [36] + 2
  sbc #3                  ; [38] + 2
  sta TMPHEIGHT           ; [40] + 3
  jmp DebounceMenuJoy     ; [43] + 3 = 46
State0JoyDown
  cpy #3                  ; [26] + 2
  bcs EndState0Joy        ; [28] + 2/3
  tya                     ; [30] + 2
  adc #3                  ; [32] + 2
  sta TMPHEIGHT           ; [34] + 3
  jmp DebounceMenuJoy     ; [37] + 3 = 40
State0JoyLeft
  tya                     ; [24] + 2
  beq EndState0Joy        ; [26] + 2/3
  dec TMPHEIGHT           ; [28] + 5
  jmp DebounceMenuJoy     ; [33] + 3 = 36
State0JoyRight
  cpy #5                  ; [22] + 2
  bcs EndState0Joy        ; [24] + 2/3
  inc TMPHEIGHT           ; [26] + 5
  jmp DebounceMenuJoy     ; [31] + 3 = 34
EndState0Joy
  jmp EndMenuJoy          ; [37] + 3
  
  ; Level
State1Joy
  ldy TMPLEVEL            ; [15] + 3
  lda #%00100000          ; [18] + 2
  bit DEBOUNCE            ; [20] + 3
  bpl State1JoyRight      ; [23] + 2/3
  bvc State1JoyLeft       ; [25] + 2/3
  beq State1JoyDown       ; [27] + 2/3
  lda DEBOUNCE            ; [29] + 3
  and #%00010000          ; [32] + 2
  bne EndState0Joy        ; [34] + 2/3
State1JoyUp
  cpy #5                  ; [36] + 2
  bcc EndState0Joy        ; [38] + 2/3
  tya                     ; [40] + 2
  sbc #5                  ; [42] + 2
  sta TMPLEVEL            ; [44] + 3
  jmp DebounceMenuJoy     ; [47] + 3  = 50
State1JoyDown
  cpy #5                  ; [30] + 2
  bcs EndState0Joy        ; [32] + 2/3
  tya                     ; [34] + 2
  adc #5                  ; [36] + 2
  sta TMPLEVEL            ; [38] + 3
  jmp DebounceMenuJoy     ; [41] + 3  = 44
State1JoyLeft
  tya                     ; [28] + 2
  beq EndState0Joy        ; [30] + 2/3
  dec TMPLEVEL            ; [32] + 5
  jmp DebounceMenuJoy     ; [37] + 3  = 40
State1JoyRight
  cpy #9                  ; [26] + 2
  bcs EndState0Joy        ; [28] + 2/3
  inc TMPLEVEL            ; [30] + 5
  jmp DebounceMenuJoy     ; [35] + 3  = 38

  ; Music
State2Joy
  ldy TMPMUSIC            ; [19] + 3
  lda #%00100000          ; [22] + 2
  bit DEBOUNCE            ; [24] + 3
  bpl State2JoyRight      ; [27] + 2/3
  bvc State2JoyLeft       ; [29] + 2/3
  jmp EndMenuJoy          ; [31] + 3
State2JoyLeft
  cpy #1                  ; [32] + 2
  bcc EndMenuJoy          ; [34] + 2/3
  dec TMPMUSIC            ; [36] + 5
  jmp DebounceMenuJoy     ; [41] + 3  = 44
State2JoyRight
  cpy #3                  ; [30] + 2
  bcs EndMenuJoy          ; [32] + 2/3
  inc TMPMUSIC            ; [34] + 5
  jmp DebounceMenuJoy     ; [39] + 3  = 42

  ; Game
State3Joy
  lda DEBOUNCE            ; [21] + 3
  and #%00100000          ; [24] + 2
  beq State3JoyDown       ; [26] + 2/3
  lda DEBOUNCE            ; [28] + 3
  and #%00010000          ; [31] + 2
  bne EndMenuJoy          ; [33] + 2/3
State3JoyUp
  lda TMPGAME             ; [35] + 3
  beq EndMenuJoy          ; [38] + 2/3
  dec TMPGAME             ; [40] + 5
  jmp DebounceMenuJoy     ; [45] + 3 = 48
State3JoyDown
  lda TMPGAME             ; [29] + 3
  cmp #3                  ; [32] + 2
  bcs EndMenuJoy          ; [34] + 2/3
  inc TMPGAME             ; [36] + 5
DebounceMenuJoy
  ; Debounce Joystick
  lda DEBOUNCE            ; [50] + 3
  and #%11111000          ; [53] + 2
  ora #%00000111          ; [55] + 2
  sta DEBOUNCE            ; [57] + 3
  ; Move Sound
  lda SFX                 ; [60] + 3
  bne EndMenuJoy          ; [63] + 2/3
  ldx #((2<<5)|3)         ; [65] + 2
  stx SFX                 ; [67] + 3  = 70
EndMenuJoy

; -----------------------------------------------------------------------------
; MENU SOUNDS
; -----------------------------------------------------------------------------

  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; Play Sound Effects
  lax SFX                 ; [5] + 3
  beq EndMSounds          ; [8] + 2/3
  and #%00011111          ; [10] + 2
  tay                     ; [12] + 2
  dey                     ; [14] + 2
  bpl PlayMSound          ; [16] + 2/3
  ldx #0                  ; [18] + 2
  stx SFX                 ; [20] + 3
  beq EndMSounds          ; [23] + 3
PlayMSound
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
  lda MSoundTab,X         ; [44] + 4
  sta SPTR                ; [48] + 3
  lda (SPTR),Y            ; [51] + 5
  sta AUDF1               ; [56] + 3
  ldx #5                  ; [59] + 2
  stx AUDC1               ; [61] + 3
EndMSounds
  stx AUDV1               ; [64] + 3
  
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; Play Startup Music
  dec MUSIC               ; [5] + 5
  bpl SkipMenuTune        ; [10] + 2/3
  sta TUNESTEP_W          ; [12] + 3
  nop                     ; [15] + 2
  ldx #MUSICTEMPO         ; [17] + 2
  stx MUSIC               ; [19] + 3
SkipMenuTune

; -----------------------------------------------------------------------------
; MENU KERNEL SETUP
; -----------------------------------------------------------------------------

  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; Set Cursor Colour
  lda CYCLE               ; [5] + 3
  lsr                     ; [8] + 2
  lsr                     ; [10] + 2
  and #%00001111          ; [12] + 2
  tax                     ; [14] + 2
  lda YellowFade,X        ; [16] + 4
  sta FCOL                ; [20] + 3

  ; Update Colour Offset
  lda CYCLE               ; [23] + 3
  lsr                     ; [26] + 2
  bcs EndStarOffset       ; [28] + 2/3
  lsr                     ; [30] + 2
  bcc EndStarOffset       ; [32] + 2/3
  dec STAROFF             ; [34] + 5
  bpl EndStarOffset       ; [39] + 2/3
  ldx #10                 ; [41] + 2
  stx STAROFF             ; [43] + 3
EndStarOffset

  ; Set Star Colour Pointer
  clc                     ; [46] + 2
  ldx #<MenuStarCols      ; [48] + 2
  txa                     ; [50] + 2
  adc STAROFF             ; [52] + 3
  sta STARPTR             ; [55] + 3

  ; Position Text
  jsr PositionMenuText    ; [0 -> 14]

  START_FRAME_MUSIC       ; [8]

  jsr MenuLogoKernel      ; [41 -> 15]
  ldx #15                 ; [15] + 2
  jsr SkipMenuLines       ; [0 -> 45]

  lda STATE               ; [45] + 3
  cmp #2                  ; [48] + 2
  bcs MenuPage0           ; [50] + 2/3
  jmp MenuPage1           ; [52] + 3

; -----------------------------------------------------------------------------
; MENU PAGE 0
; -----------------------------------------------------------------------------

MenuPage0
  ldx #4                  ; [53] + 2
  stx LINE                ; [55] + 3
  ldx #3                  ; [58] + 2
  jsr PageSetup           ; [0 -> 45]
  ldx #0*12               ; [45] + 2      Game Type
  stx MSG                 ; [47] + 3
  jsr LoadMenuText        ; [0 -> 27]

GameTypeLoop
  jsr MenuTextKernel      ; [52 -> 11]
  dec LINE                ; [11] + 5
  bmi EndGameTypeLoop     ; [16] + 2/3
  ldx HCOL                ; [18] + 3
  ldy LINE                ; [21] + 3
  lda TMPGAME             ; [24] + 3
  cmp GameLine,Y          ; [27] + 2
  beq StoreGameCol0       ; [29] + 2/3
GameCol0
  ldx TCOL                ; [31] + 3
StoreGameCol0
  stx COLUP0              ; [34] + 3
  stx COLUP1              ; [37] + 3
  lda GameTypeMessage,Y   ; [40] + 4
  sta MSG                 ; [44] + 3
  jsr LoadMenuText        ; [0 -> 27]
  jmp GameTypeLoop        ; [27] + 3
EndGameTypeLoop

  ldx #15                 ; [19] + 2
  jsr SkipMenuLines       ; [0 -> 45]
  
  ldx #2                  ; [27] + 2
  jsr PageSetup           ; [0 -> 45]
  ldx #5*12               ; [45] + 2      Music Type
  stx MSG                 ; [47] + 3
  
  jsr LoadMenuText        ; [0 -> 27]
  jsr MenuTextKernel      ; [52 -> 11]
  jsr PositionMusic       ; [0 -> 14]
  
  ldx #6*12               ; [14] + 2      A,B,C
  stx MSG                 ; [16] + 3
  jsr LoadMenuText        ; [0 -> 27]
  clc                     ; [27] + 2
  ldx TMPMUSIC            ; [29] + 3
  jsr MusicKernel         ; [45 -> 0]
  
  PLAY_MUSIC              ; [0] + 5
  jmp EndMenuPages        ; [5] + 3

; -----------------------------------------------------------------------------
; MENU PAGE 1
; -----------------------------------------------------------------------------

MenuPage1
  ; Setup State 1
  ldx #1                  ; [55] + 2
  jsr PageSetup           ; [0 -> 45]
  
  ; Display Level Message
  ldx #7*12               ; [45] + 2    TMPLEVEL
  stx MSG                 ; [47] + 3
  jsr LoadMenuText        ; [0 -> 27]
  jsr MenuTextKernel      ; [52 -> 11]
  
  ; Level Top Row
  jsr PositionMenuNumbers ; [0 -> 14]
  ldx #8*12               ; [14] + 2    0,1,2,3,4
  stx MSG                 ; [16] + 3
  jsr LoadMenuText        ; [0 -> 27]
  clc                     ; [27] + 2
  lda TMPLEVEL            ; [29] + 3
  sta TEMP                ; [32] + 3
  adc #0                  ; [35] + 2
  tay                     ; [37] + 2
  jsr NumberKernelSetup0  ; [45 -> 0]
  PLAY_MUSIC              ; [0] + 5
  
  ; Level Bottom Row
  ldx #9*12               ; [5] + 2     5,6,7,8,9
  stx MSG                 ; [7] + 3
  jsr LoadMenuText        ; [0 -> 27]
  clc                     ; [27] + 2
  lda TMPLEVEL            ; [29] + 3
  sta TEMP                ; [32] + 3
  adc #10                 ; [35] + 2
  tay                     ; [37] + 2
  jsr NumberKernelSetup1  ; [45 -> 0]
  PLAY_MUSIC              ; [0] + 5
  
  ldx #15                 ; [5] + 2
  jsr SkipMenuLines       ; [0 -> 45]

  ; State 0 Setup
  ldx #0                  ; [45] + 2
  jsr PageSetup           ; [0 -> 45]
  
  ; Display Height Message
  jsr PositionMenuText    ; [0 -> 14]
  ldx #10*12              ; [14] + 2      DEPTH
  stx MSG                 ; [16] + 3
  jsr LoadMenuText        ; [0 -> 27]
  jsr MenuTextKernel      ; [52 -> 11]
  
  ; Height Top Row
  jsr PositionMenuNumbers ; [0 -> 14]
  ldx #11*12              ; [14] + 2      0,1,2
  stx MSG                 ; [16] + 3
  jsr LoadMenuText        ; [0 -> 27]
  clc                     ; [27] + 2
  lda TMPHEIGHT           ; [29] + 3
  sta TEMP                ; [32] + 3
  adc #20                 ; [35] + 2
  tay                     ; [37] + 2
  jsr NumberKernelSetup1  ; [45 -> 0]
  PLAY_MUSIC              ; [0] + 5

  ; Height Bottom Row
  ldx #12*12              ; [5] + 2       3,4,5
  stx MSG                 ; [7] + 3
  jsr LoadMenuText        ; [0 -> 27]
  clc                     ; [27] + 2
  lda TMPHEIGHT           ; [29] + 3
  sta TEMP                ; [32] + 3
  adc #26                 ; [35] + 2
  tay                     ; [37] + 2
  jsr NumberKernelSetup0  ; [45 -> 0]
  PLAY_MUSIC              ; [0] + 5
EndMenuPages

  START_OVERSCAN_MUSIC
  WAIT_OVERSCAN_MUSIC
  START_VBLANK_MUSIC
  jmp MenuLoop

  DC.B "MOSCOW"

; -----------------------------------------------------------------------------
; TEXT LOADING
; -----------------------------------------------------------------------------

  ALIGN 256

   ; Copy Text into Buffer
LoadMenuText
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  
  ; Store Stack
  tsx                     ; [5] + 2
  stx STACK               ; [7] + 3
  ldx #<MBUFF+41          ; [10] + 2
  txs                     ; [12] + 2
  
  ldx MSG                 ; [14] + 3
  ldy MenuMessages+10,X   ; [17] + 4
  lda MenuMessages+11,X   ; [21] + 4
  tax                     ; [25] + 2
  lda BRChars+6,X         ; [27] + 4
  ora BLChars+6,Y         ; [31] + 4
  pha                     ; [35] + 3
  lda BRChars+5,X         ; [38] + 4
  ora BLChars+5,Y         ; [42] + 4
  pha                     ; [46] + 3
  lda BRChars+4,X         ; [49] + 4
  ora BLChars+4,Y         ; [53] + 4
  pha                     ; [57] + 3
  lda BRChars+3,X         ; [60] + 4
  ora BLChars+3,Y         ; [64] + 4
  pha                     ; [68] + 3
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  lda BRChars+2,X         ; [5] + 4
  ora BLChars+2,Y         ; [9] + 4
  pha                     ; [13] + 3
  lda BRChars+1,X         ; [16] + 4
  ora BLChars+1,Y         ; [20] + 4
  pha                     ; [24] + 3
  lda BRChars+0,X         ; [27] + 4
  ora BLChars+0,Y         ; [31] + 4
  pha                     ; [35] + 3
  ldx MSG                 ; [38] + 3
  ldy MenuMessages+8,X    ; [41] + 4
  lda MenuMessages+9,X    ; [45] + 4
  tax                     ; [49] + 2
  lda BRChars+6,X         ; [51] + 4
  ora BLChars+6,Y         ; [54] + 4
  pha                     ; [58] + 3
  lda BRChars+5,X         ; [61] + 4
  ora BLChars+5,Y         ; [65] + 4
  pha                     ; [69] + 3
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  lda BRChars+4,X         ; [5] + 4
  ora BLChars+4,Y         ; [9] + 4
  pha                     ; [13] + 3
  lda BRChars+3,X         ; [16] + 4
  ora BLChars+3,Y         ; [20] + 4
  pha                     ; [24] + 3
  lda BRChars+2,X         ; [27] + 4
  ora BLChars+2,Y         ; [31] + 4
  pha                     ; [35] + 3
  lda BRChars+1,X         ; [38] + 4
  ora BLChars+1,Y         ; [42] + 4
  pha                     ; [46] + 3
  lda BRChars+0,X         ; [49] + 4
  ora BLChars+0,Y         ; [53] + 4
  pha                     ; [57] + 3
  ldx MSG                 ; [60] + 3
  ldy MenuMessages+6,X    ; [63] + 4
  lda MenuMessages+7,X    ; [67] + 4
  tax                     ; [71] + 2
  sta WSYNC               ; [73] + 3
  PLAY_MUSIC              ; [0] + 5
  lda BRChars+6,X         ; [5] + 4
  ora BLChars+6,Y         ; [9] + 4
  pha                     ; [13] + 3
  lda BRChars+5,X         ; [16] + 4
  ora BLChars+5,Y         ; [20] + 4
  pha                     ; [24] + 3
  lda BRChars+4,X         ; [27] + 4
  ora BLChars+4,Y         ; [31] + 4
  pha                     ; [35] + 3
  lda BRChars+3,X         ; [38] + 4
  ora BLChars+3,Y         ; [42] + 4
  pha                     ; [46] + 3
  lda BRChars+2,X         ; [49] + 4
  ora BLChars+2,Y         ; [53] + 4
  pha                     ; [57] + 3
  lda BRChars+1,X         ; [60] + 4
  ora BLChars+1,Y         ; [64] + 4
  pha                     ; [68] + 3
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  lda BRChars+0,X         ; [5] + 4
  ora BLChars+0,Y         ; [9] + 4
  pha                     ; [13] + 3
  ldx MSG                 ; [16] + 3
  ldy MenuMessages+4,X    ; [19] + 4
  lda MenuMessages+5,X    ; [23] + 4
  tax                     ; [27] + 2
  lda BRChars+6,X         ; [29] + 4
  ora BLChars+6,Y         ; [33] + 4
  pha                     ; [37] + 3
  lda BRChars+5,X         ; [40] + 4
  ora BLChars+5,Y         ; [44] + 4
  pha                     ; [48] + 3
  lda BRChars+4,X         ; [51] + 4
  ora BLChars+4,Y         ; [55] + 4
  pha                     ; [59] + 3
  lda BRChars+3,X         ; [62] + 4
  ora BLChars+3,Y         ; [66] + 4
  pha                     ; [70] + 3
  jmp LoadMenuTextB       ; [73] + 3
EndLoad0
  if (>LoadMenuText != >EndLoad0)
    echo "WARNING: LoadMenuText0 Crosses Page Boundary!"
  endif
  
  DC.B  "PENZA"
  
  ALIGN 256
  
LoadMenuTextB
  PLAY_MUSIC              ; [0] + 5
  lda BRChars+2,X         ; [5] + 4
  ora BLChars+2,Y         ; [9] + 4
  pha                     ; [13] + 3
  lda BRChars+1,X         ; [16] + 4
  ora BLChars+1,Y         ; [20] + 4
  pha                     ; [24] + 3
  lda BRChars+0,X         ; [27] + 4
  ora BLChars+0,Y         ; [31] + 4
  pha                     ; [35] + 3
  ldx MSG                 ; [38] + 3
  ldy MenuMessages+2,X    ; [41] + 4
  lda MenuMessages+3,X    ; [45] + 4
  tax                     ; [49] + 2
  lda BRChars+6,X         ; [51] + 4
  ora BLChars+6,Y         ; [55] + 4
  pha                     ; [59] + 3
  lda BRChars+5,X         ; [62] + 4
  ora BLChars+5,Y         ; [66] + 4
  pha                     ; [70] + 3
  sta WSYNC               ; [73] + 3
  PLAY_MUSIC              ; [0] + 5
  lda BRChars+4,X         ; [5] + 4
  ora BLChars+4,Y         ; [9] + 4
  pha                     ; [13] + 3
  lda BRChars+3,X         ; [16] + 4
  ora BLChars+3,Y         ; [20] + 4
  pha                     ; [24] + 3
  lda BRChars+2,X         ; [27] + 4
  ora BLChars+2,Y         ; [31] + 4
  pha                     ; [35] + 3
  lda BRChars+1,X         ; [38] + 4
  ora BLChars+1,Y         ; [42] + 4
  pha                     ; [46] + 3
  lda BRChars+0,X         ; [49] + 4
  ora BLChars+0,Y         ; [53] + 4
  pha                     ; [57] + 3
  ldx MSG                 ; [60] + 3
  ldy MenuMessages+0,X    ; [63] + 4
  lda MenuMessages+1,X    ; [67] + 4
  tax                     ; [71] + 2
  sta WSYNC               ; [73] + 3
  PLAY_MUSIC              ; [0] + 5
  lda BRChars+6,X         ; [5] + 4
  ora BLChars+6,Y         ; [9] + 4
  pha                     ; [13] + 3
  lda BRChars+5,X         ; [16] + 4
  ora BLChars+5,Y         ; [20] + 4
  pha                     ; [24] + 3
  lda BRChars+4,X         ; [27] + 4
  ora BLChars+4,Y         ; [31] + 4
  pha                     ; [35] + 3
  lda BRChars+3,X         ; [38] + 4
  ora BLChars+3,Y         ; [42] + 4
  pha                     ; [46] + 3
  lda BRChars+2,X         ; [49] + 4
  ora BLChars+2,Y         ; [53] + 4
  pha                     ; [57] + 3
  lda BRChars+1,X         ; [60] + 4
  ora BLChars+1,Y         ; [64] + 4
  pha                     ; [68] + 3
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  lda BRChars+0,X         ; [5] + 4
  ora BLChars+0,Y         ; [9] + 4
  pha                     ; [13] + 3

  ; Restore Stack
  ldx STACK               ; [16] + 3
  txs                     ; [19] + 2
  rts                     ; [21] + 6 = 27

; -----------------------------------------------------------------------------
; NUMBER KERNEL
; -----------------------------------------------------------------------------

  ; Positon Sprites
PositionMenuNumbers
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  ; Set 3 Copies Medium
  ldx #%00000110          ; [5] + 2
  stx NUSIZ0              ; [7] + 3
  ; Set Two Copies Medium
  ldx #%00000010          ; [10] + 2
  stx NUSIZ1              ; [12] + 3
  ; Delay Sprites
  ldx #%00000001          ; [15] + 2
  stx VDELP0              ; [17] + 3
  stx VDELP1              ; [20] + 3
  ; Position Sprites (Fine)
  sta HMCLR               ; [23] + 3
  ldx #%11110000          ; [26] + 2  (+ 1)
  stx HMP1                ; [28] + 3
  ; Position Sprites (Coarse)
  SLEEP 3                 ; [31] + 3
  sta RESP0               ; [34] + 3  = 37
  nop                     ; [37] + 2
  sta RESP1               ; [39] + 3  = 42
  ; Reposition Sprites
  sta WSYNC               ; [0]
  sta HMOVE               ; [0] + 3
  PLAY_MUSIC              ; [3] + 5
  rts                     ; [8] + 6 = 14
EndPositionMenuNumbers
  if (>PositionMenuNumbers != >EndPositionMenuNumbers)
    echo "WARNING: PositionMenuNumbers Crosses Page Boundary!"
  endif

  ; Setup Number Kernel 0 (Y = Position)
NumberKernelSetup0
  ; Default Colours
  lda TCOL                ; [45] + 3
  sta TCOL0               ; [48] + 3
  sta TCOL1               ; [51] + 3
  sta TCOL2               ; [54] + 3
  sta TCOL3               ; [57] + 3
  ; Set Highlight Colour
  ldx NumberOffsets,Y     ; [60] + 4
  lda HCOL                ; [64] + 3
  sta TCOL0,X             ; [67] + 5**
  ; Play Music
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  ; Jump to Number Kernel
  lsr TEMP                ; [5] + 5
  bcs SecondNumber0       ; [10] + 2/3
  jmp NumberKernel0A      ; [12] + 3
SecondNumber0
  jmp NumberKernel1A      ; [13] + 3
EndNumberKernelSetup0
  if (>NumberKernelSetup0 != >EndNumberKernelSetup0)
    echo "WARNING: NumberKernelSetup0 Crosses Page Boundary!"
  endif

  ; Game Type Offsets
GameLine
  DC.B  3, 2, 1, 0
GameTypeMessage
  DC.B  4*12, 3*12, 2*12, 1*12

  DC.B  "VLADIMIR"

  ALIGN 256

NumberKernel0A
  lda TCOL0
  sta COLUP0
  ldx #6
  ldy MBUFF+0,X
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  sty GRP0                ; [5] + 3
  jmp StartNumberKernel0  ; [8] + 3
NumberKernel0Loop
  SLEEP 4                 ; [65] + 4
  lda TCOL0               ; [69] + 3
  sta COLUP0              ; [72] + 3  > 62
  PLAY_MUSIC              ; [75] + 5
  lda MBUFF+0,X           ; [4] + 4                  0A 0B 1A 1B
  sta GRP0                ; [8] + 3   < 39            0  -  -  -
StartNumberKernel0
  lda MBUFF+7,X           ; [11] + 4
  sta GRP1                ; [15] + 3  < 39            -  0  1  -   0
  lda MBUFF+14,X          ; [18] + 4
  sta GRP0                ; [22] + 3  <= 44           2  0  -  1   0+1
  lda TCOL1               ; [25] + 3
  sta COLUP1              ; [28] + 3  < 39
  ldy MBUFF+21,X          ; [31] + 4
  lda MBUFF+28,X          ; [35] + 4
  sty GRP1                ; [39] + 3  > 41 < 50       -  2  3  1    2+1
  ldy TCOL2               ; [42] + 3
  sty COLUP0              ; [45] + 3  > 41 < 50
  sta GRP0                ; [48] + 3  > 46 < 55       4  2  -  3    2+3
  lda TCOL3               ; [51] + 3
  sta COLUP0              ; [54] + 3  >= 52 <= 60
  sta GRP1                ; [57] + 3  >= 52 <= 60     -  4  -  3    4+3
  dex                     ; [60] + 2
  bpl NumberKernel0Loop   ; [62] + 2/3
  inx                     ; [57] + 2
  stx GRP0                ; [59] + 3
  stx GRP1                ; [62] + 3
  stx GRP0                ; [65] + 3
  nop                     ; [68] + 2
  rts                     ; [70] + 6  = 76(0)

EndNumberKernel0A
  if (>NumberKernel0A != >EndNumberKernel0A)
    echo "WARNING: NumberKernel0A Crosses Page Boundary!"
  endif

  ; Setup Number Kernel 1 (Y = Position)
NumberKernelSetup1
  ; Default Colours
  lda TCOL                ; [45] + 3
  sta TCOL0               ; [48] + 3
  sta TCOL1               ; [51] + 3
  sta TCOL2               ; [54] + 3
  sta TCOL3               ; [57] + 3
  ; Set Highlight Colour
  ldx NumberOffsets,Y     ; [60] + 4
  lda HCOL                ; [64] + 3
  sta TCOL0,X             ; [67] + 5**
  ; Play Music
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  ; Jump to Number Kernel
  lsr TEMP                ; [5] + 5
  bcc SecondNumber1       ; [10] + 2/3
  jmp NumberKernel0A      ; [12] + 3
SecondNumber1
  jmp NumberKernel1A      ; [13] + 3
EndNumberKernelSetup1
  if (>NumberKernelSetup1 != >EndNumberKernelSetup1)
    echo "WARNING: NumberKernelSetup1 Crosses Page Boundary!"
  endif

NumberKernel1A
  lda TCOL0               ; [67] + 3
  sta COLUP0              ; [70] + 3
  
  sta WSYNC               ; [0]                     Line 6
  PLAY_MUSIC              ; [0] + 5
  lda MBUFF+6             ; [5] + 3                  0A 0B 1A 1B
  sta GRP0                ; [8] + 3   < 39            0  -  -  -
  lda MBUFF+13            ; [11] + 3
  sta GRP1                ; [14] + 3  < 39            -  0  1  -   0
  lda MBUFF+20            ; [17] + 3
  sta GRP0                ; [20] + 3  <= 44           2  0  -  1   0+1
  lda TCOL1               ; [23] + 3
  sta.w COLUP1            ; [26] + 4  < 39
  ldx MBUFF+27            ; [30] + 3
  ldy MBUFF+34            ; [33] + 3
  lda TCOL2               ; [36] + 3
  stx GRP1                ; [39] + 3  > 41 < 50       -  2  3  1    2+1
  sta COLUP0              ; [42] + 3  > 41 < 50
  sty GRP0                ; [45] + 3  > 46 < 55       4  2  -  3    2+3
  lda TCOL3               ; [48] + 3
  sta COLUP1              ; [51] + 3  > 46 < 55
  sta GRP1                ; [54] + 3  >= 52 <= 60     -  4  -  3    4+3
  lda TCOL0               ; [57] + 3
  sta COLUP0              ; [60] + 3  > 62
  
  sta WSYNC               ; [0]                     Line 5
  PLAY_MUSIC              ; [0] + 5
  lda MBUFF+5             ; [5] + 3                  0A 0B 1A 1B
  sta GRP0                ; [8] + 3   < 39            0  -  -  -
  lda MBUFF+12            ; [11] + 3
  sta GRP1                ; [14] + 3  < 39            -  0  1  -   0
  lda MBUFF+19            ; [17] + 3
  sta GRP0                ; [20] + 3  <= 44           2  0  -  1   0+1
  lda TCOL1               ; [23] + 3
  sta.w COLUP1            ; [26] + 4  < 39
  ldx MBUFF+26            ; [30] + 3
  ldy MBUFF+33            ; [33] + 3
  lda TCOL2               ; [36] + 3
  stx GRP1                ; [39] + 3  > 41 < 50       -  2  3  1    2+1
  sta COLUP0              ; [42] + 3  > 41 < 50
  sty GRP0                ; [45] + 3  > 46 < 55       4  2  -  3    2+3
  lda TCOL3               ; [48] + 3
  sta COLUP1              ; [51] + 3  > 46 < 55
  sta GRP1                ; [54] + 3  >= 52 <= 60     -  4  -  3    4+3
  lda TCOL0               ; [57] + 3
  sta COLUP0              ; [60] + 3  > 62
  
  sta WSYNC               ; [0]                     Line 4
  PLAY_MUSIC              ; [0] + 5
  lda MBUFF+4             ; [5] + 3                  0A 0B 1A 1B
  sta GRP0                ; [8] + 3   < 39            0  -  -  -
  lda MBUFF+11            ; [11] + 3
  sta GRP1                ; [14] + 3  < 39            -  0  1  -   0
  lda MBUFF+18            ; [17] + 3
  sta GRP0                ; [20] + 3  <= 44           2  0  -  1   0+1
  lda TCOL1               ; [23] + 3
  sta.w COLUP1            ; [26] + 4  < 39
  ldx MBUFF+25            ; [30] + 3
  ldy MBUFF+32            ; [33] + 3
  lda TCOL2               ; [36] + 3
  stx GRP1                ; [39] + 3  > 41 < 50       -  2  3  1    2+1
  sta COLUP0              ; [42] + 3  > 41 < 50
  sty GRP0                ; [45] + 3  > 46 < 55       4  2  -  3    2+3
  lda TCOL3               ; [48] + 3
  sta COLUP1              ; [51] + 3  > 46 < 55
  sta GRP1                ; [54] + 3  >= 52 <= 60     -  4  -  3    4+3
  
  lda TCOL0               ; [57] + 3
  sta COLUP0              ; [60] + 3  > 62
  jmp NumberKernel1B      ; [63] + 3

EndNumberKernel1A
  if (>NumberKernel1A != >EndNumberKernel1A)
    echo "WARNING: NumberKernel1A Crosses Page Boundary!"
  endif

  DC.B  "ORSK"
  
  ALIGN 256

NumberKernel1B
  sta WSYNC               ; [0]                     Line 3
  PLAY_MUSIC              ; [0] + 5
  lda MBUFF+3             ; [5] + 3                  0A 0B 1A 1B
  sta GRP0                ; [8] + 3   < 39            0  -  -  -
  lda MBUFF+10            ; [11] + 3
  sta GRP1                ; [14] + 3  < 39            -  0  1  -   0
  lda MBUFF+17            ; [17] + 3
  sta GRP0                ; [20] + 3  <= 44           2  0  -  1   0+1
  lda TCOL1               ; [23] + 3
  sta.w COLUP1            ; [26] + 4  < 39
  ldx MBUFF+24            ; [30] + 3
  ldy MBUFF+31            ; [33] + 3
  lda TCOL2               ; [36] + 3
  stx GRP1                ; [39] + 3  > 41 < 50       -  2  3  1    2+1
  sta COLUP0              ; [42] + 3  > 41 < 50
  sty GRP0                ; [45] + 3  > 46 < 55       4  2  -  3    2+3
  lda TCOL3               ; [48] + 3
  sta COLUP1              ; [51] + 3  > 46 < 55
  sta GRP1                ; [54] + 3  >= 52 <= 60     -  4  -  3    4+3
  lda TCOL0               ; [57] + 3
  sta COLUP0              ; [60] + 3  > 62

  sta WSYNC               ; [0]                     Line 2
  PLAY_MUSIC              ; [0] + 5
  lda MBUFF+2             ; [5] + 3                  0A 0B 1A 1B
  sta GRP0                ; [8] + 3   < 39            0  -  -  -
  lda MBUFF+9             ; [11] + 3
  sta GRP1                ; [14] + 3  < 39            -  0  1  -   0
  lda MBUFF+16            ; [17] + 3
  sta GRP0                ; [20] + 3  <= 44           2  0  -  1   0+1
  lda TCOL1               ; [23] + 3
  sta.w COLUP1            ; [26] + 4  < 39
  ldx MBUFF+23            ; [30] + 3
  ldy MBUFF+30            ; [33] + 3
  lda TCOL2               ; [36] + 3
  stx GRP1                ; [39] + 3  > 41 < 50       -  2  3  1    2+1
  sta COLUP0              ; [42] + 3  > 41 < 50
  sty GRP0                ; [45] + 3  > 46 < 55       4  2  -  3    2+3
  lda TCOL3               ; [48] + 3
  sta COLUP1              ; [51] + 3  > 46 < 55
  sta GRP1                ; [54] + 3  >= 52 <= 60     -  4  -  3    4+3
  lda TCOL0               ; [57] + 3
  sta COLUP0              ; [60] + 3  > 62
  
  sta WSYNC               ; [0]                     Line 1
  PLAY_MUSIC              ; [0] + 5
  lda MBUFF+1             ; [5] + 3                  0A 0B 1A 1B
  sta GRP0                ; [8] + 3   < 39            0  -  -  -
  lda MBUFF+8             ; [11] + 3
  sta GRP1                ; [14] + 3  < 39            -  0  1  -   0
  lda MBUFF+15            ; [17] + 3
  sta GRP0                ; [20] + 3  <= 44           2  0  -  1   0+1
  lda TCOL1               ; [23] + 3
  sta.w COLUP1            ; [26] + 4  < 39
  ldx MBUFF+22            ; [30] + 3
  ldy MBUFF+29            ; [33] + 3
  lda TCOL2               ; [36] + 3
  stx GRP1                ; [39] + 3  > 41 < 50       -  2  3  1    2+1
  sta COLUP0              ; [42] + 3  > 41 < 50
  sty GRP0                ; [45] + 3  > 46 < 55       4  2  -  3    2+3
  lda TCOL3               ; [48] + 3
  sta COLUP1              ; [51] + 3  > 46 < 55
  sta GRP1                ; [54] + 3  >= 52 <= 60     -  4  -  3    4+3
  lda TCOL0               ; [57] + 3
  sta COLUP0              ; [60] + 3  > 62
  
  sta WSYNC               ; [0]                     Line 0
  PLAY_MUSIC              ; [0] + 5
  lda MBUFF+0             ; [5] + 3                  0A 0B 1A 1B
  sta GRP0                ; [8] + 3   < 39            0  -  -  -
  lda MBUFF+7             ; [11] + 3
  sta GRP1                ; [14] + 3  < 39            -  0  1  -   0
  lda MBUFF+14            ; [17] + 3
  sta GRP0                ; [20] + 3  <= 44           2  0  -  1   0+1
  lda TCOL1               ; [23] + 3
  sta.w COLUP1            ; [26] + 4  < 39
  ldx MBUFF+21            ; [30] + 3
  ldy MBUFF+28            ; [33] + 3
  lda TCOL2               ; [36] + 3
  stx GRP1                ; [39] + 3  > 41 < 50       -  2  3  1    2+1
  sta COLUP0              ; [42] + 3  > 41 < 50
  sty GRP0                ; [45] + 3  > 46 < 55       4  2  -  3    2+3
  lda TCOL3               ; [48] + 3
  sta COLUP1              ; [51] + 3  > 46 < 55
  sta GRP1                ; [54] + 3  >= 52 <= 60     -  4  -  3    4+3

  ldx #0                  ; [57] + 2
  stx GRP0                ; [59] + 3
  stx GRP1                ; [62] + 3
  stx GRP0                ; [65] + 3
  nop                     ; [68] + 2
  rts                     ; [70] + 6 = 76(0)
EndNumberKernel1B
  if (>NumberKernel1B != >EndNumberKernel1B)
    echo "WARNING: NumberKernel1B Crosses Page Boundary!"
  endif

; -----------------------------------------------------------------------------
; MENU TEXT
; -----------------------------------------------------------------------------

PositionMenuText
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  ; Set 3 Copies Close and Delay
  ldx #%00110011          ; [5] + 2
  stx VDELP0              ; [7] + 3
  stx VDELP1              ; [10] + 3
  stx NUSIZ0              ; [13] + 3
  stx NUSIZ1              ; [16] + 3
  ; Position Sprites (Fine)
  sta HMCLR               ; [19] + 3
  SLEEP 3                 ; [22] + 3
  sta RESM0               ; [25] + 3  = 28
  ldx #%00010000          ; [28] + 2
  stx HMP1                ; [30] + 3
  ; Position Sprites (Coarse)
  SLEEP 5                 ; [33] + 5
  sta RESP0               ; [38] + 3  = 41
  sta RESP1               ; [41] + 3  = 44
  ; Reposition Sprites
  sta WSYNC               ; [0]
  sta HMOVE               ; [0] + 3
  PLAY_MUSIC              ; [3] + 5
  rts                     ; [8] + 6   = 14
EndPositionMenuText
  if (>PositionMenuText != >EndPositionMenuText)
    echo "WARNING: PositionMenuText Crosses Page Boundary!"
  endif

  ; Menu Colour Fade
YellowFade
  IF (PALMODE)
  DC.B  $30, $32, $34, $36, $38, $3A, $3C, $3E
  DC.B  $3E, $3C, $3A, $38, $36, $34, $32, $30
  ELSE
  DC.B  $10, $12, $14, $16, $18, $1A, $1C, $1E
  DC.B  $1E, $1C, $1A, $18, $16, $14, $12, $10
  ENDIF

  DC.B  "PETROGRAD"

  ALIGN 256

MenuTextKernel
  ldx #6                  ; [58] + 2
  stx TEMP2               ; [60] + 3
MenuTextLoop
  ldx TEMP2               ; [63] + 3
  ldy MBUFF+0,X           ; [66] + 4                 0A 0B 1A 1B
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  SLEEP 3                 ; [5] + 3
  sty GRP0                ; [8] + 3                  0 - - -
  lda MBUFF+7,X           ; [11] + 4
  sta GRP1                ; [15] + 3  < 43            - 0 1 0
  lda MBUFF+14,X          ; [18] + 4
  sta GRP0                ; [22] + 3  < 46            2 0 - 1
  ldy MBUFF+28,X          ; [25] + 4
  lda MBUFF+35,X          ; [29] + 4
  sta TEMP                ; [33] + 3
  lda MBUFF+21,X          ; [36] + 4
  ldx TEMP                ; [40] + 3
  sta GRP1                ; [43] + 3  > 45 <= 48      - 2 3 1
  sty GRP0                ; [46] + 3  >= 48 < 51      4 2 - 3
  stx GRP1                ; [49] + 3  > 50 < 54       - 4 5 3
  sta GRP0                ; [52] + 3  > 53 <= 56      - 4 - 5
  dec TEMP2               ; [55] + 5
  bpl MenuTextLoop        ; [60] + 2/3
  ldx #0                  ; [62] + 2
  stx GRP1                ; [64] + 3
  stx GRP0                ; [67] + 3
  stx GRP1                ; [70] + 3
  stx WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  rts                     ; [5] + 6  = 11
EndMenuTextKernel
  if (>MenuTextKernel != >EndMenuTextKernel)
    echo "WARNING: MenuTextKernel Crosses Page Boundary!"
  endif

; -----------------------------------------------------------------------------
; LOGO KERNEL
; -----------------------------------------------------------------------------

  ; Draw Menu Logo
MenuLogoKernel
  ; Reflect Playfield & Reset Colours
  ldx #%00000001          ; [41] + 2
  stx CTRLPF              ; [43] + 3
  stx COLUPF              ; [46] + 3
  stx COLUP0              ; [49] + 3
  stx COLUP1              ; [52] + 3
  
  ; Set Playfield
  ldx #%11111110          ; [55] + 2
  stx PF2                 ; [57] + 3
  stx ENAM0               ; [60] + 3

  ; Set First Sprite
  ldy #STARH              ; [63] + 3
  sty LINE                ; [66] + 3

MenuStarLoop
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  ldy LINE                ; [5] + 3
  lda (STARPTR),Y         ; [8] + 5
  sta COLUPF              ; [13] + 3
  lda #$FF                ; [16] + 2
  sta GRP0                ; [18] + 3
  sta GRP1                ; [21] + 3
  sta GRP0                ; [24] + 3
  lda StarLogo0,Y         ; [27] + 4
  ldx StarLogo1,Y         ; [31] + 4
  ldy #$FF                ; [35] + 2
  dec LINE                ; [37] + 5
  sta.w GRP1              ; [42] + 4    > 45 <= 48
  stx GRP0                ; [46] + 3    >= 48 < 51
  sty GRP1                ; [49] + 3    > 50 < 54
  sty GRP0                ; [52] + 3    > 53 <= 56
  bpl MenuStarLoop        ; [55] + 2/3

  ; Set Logo Height
  ldx #LOGOH              ; [57] + 3
  stx LINE                ; [60] + 3
  lda LOGOCOL             ; [63] + 3
  sta COLUPF              ; [66] + 3
  lda MenuLogo0,X         ; [69] + 4
  sta GRP0                ; [73] + 3
  PLAY_MUSIC              ; [0] + 5
  SLEEP 3                 ; [5] + 3
  jmp MenuLogoStart       ; [8] + 3

MenuLogoLoop
  SLEEP 3                 ; [63] + 3
  ldx LINE                ; [66] + 3
  lda MenuLogo0,X         ; [69] + 4
  sta GRP0                ; [73] + 3
  PLAY_MUSIC              ; [0] + 5
  lda LOGOCOL             ; [5] + 3
  sta COLUPF              ; [8] + 3
MenuLogoStart
  lda MenuLogo1,X         ; [11] + 4
  sta GRP1                ; [15] + 3
  lda MenuLogo2,X         ; [18] + 4
  sta GRP0                ; [22] + 3
  ldy MenuLogo4,X         ; [25] + 4
  lda MenuLogo5,X         ; [29] + 4
  sta TEMP                ; [33] + 3
  lda MenuLogo3,X         ; [36] + 4
  ldx TEMP                ; [40] + 3
  sta GRP1                ; [43] + 3    > 45 <= 48
  sty GRP0                ; [46] + 3    >= 48 < 51
  stx GRP1                ; [49] + 3    > 50 < 54
  sta GRP0                ; [52] + 3    > 53 <= 56
  dec LINE                ; [55] + 5
  bpl MenuLogoLoop        ; [60] + 2/3
  ldx #0                  ; [62] + 3
  stx GRP1                ; [65] + 3
  stx GRP0                ; [68] + 3
  stx GRP1                ; [71] + 3
  stx ENAM0               ; [74] + 3
  PLAY_MUSIC              ; [1] + 5
  stx PF2                 ; [6] + 3
  rts                     ; [9] + 6   = 15
EndMenuLogoKernel
  if (>MenuLogoKernel != >EndMenuLogoKernel)
    echo "WARNING: MenuLogoKernel Crosses Page Boundary!"
  endif
 
  ; Number Kernel Offsets (36 entries)
NumberOffsets 
  DC.B  0, 1, 2, 3, 3, 4, 4, 4, 4, 4  ; 0 -  Level Top
  DC.B  4, 4, 4, 4, 4, 0, 1, 2, 3, 3  ; 10 - Level Bottom
  DC.B  1, 2, 3, 4, 4, 4              ; 20 - Height Top
  DC.B  4, 4, 4, 1, 2, 3              ; 26 - Height Bottom
  DC.B  4, 1, 2, 3                    ; 32 - Music Positions
EndNumberOffsets
  if (>NumberOffsets != >EndNumberOffsets)
    echo "WARNING: Number Offsets Crosses Page Boundary!"
  endif
  
  DC.B  "USSR"

; -----------------------------------------------------------------------------
; MUSIC TYPE KERNEL
; -----------------------------------------------------------------------------

  ALIGN 256

  ; Positon Music Type Sprites
PositionMusic
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  ; Set Two Copies Medium & No Delay
  ldx #%00000010          ; [5] + 2
  stx NUSIZ0              ; [7] + 3
  stx NUSIZ1              ; [10] + 3
  stx VDELP0              ; [13] + 3
  stx VDELP1              ; [16] + 3
  ; Position Sprites (Fine)
  sta HMCLR               ; [19] + 3
  ldx #%11100000          ; [22] + 2  (+ 2)
  stx HMP0                ; [24] + 3
  ; Position Sprites (Coarse)
  SLEEP 9                 ; [27] + 9
  sta RESP0               ; [36] + 3  = 39
  SLEEP 3                 ; [39] + 3
  sta RESP1               ; [42] + 3  = 45
  ; Reposition Sprites
  sta WSYNC               ; [0]
  sta HMOVE               ; [0] + 3
  PLAY_MUSIC              ; [3] + 5
  rts                     ; [8] + 6 = 14
EndPositionMusic
  if (>PositionMusic != >EndPositionMusic)
    echo "WARNING: PositionMusic Crosses Page Boundary!"
  endif

  ; Music Types Kernel
MusicKernel
  ; Set Colours
  lda TCOL                ; [38] + 3
  sta TCOL0               ; [41] + 3
  sta TCOL1               ; [44] + 3
  sta TCOL2               ; [47] + 3
  sta TCOL3               ; [50] + 3
  lda HCOL                ; [53] + 3
  sta TCOL0,X             ; [56] + 4
  ldy #6
  sty LINE
  lda TCOL0
  sta COLUP0
  ldx TCOL1
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  stx COLUP1              ; [5] + 3    < 47
  jmp MusicKernelStart    ; [8] + 3
MusicKernelLoop
  nop                     ; [65] + 2
  ldy LINE                ; [67] + 3
  lda TCOL0               ; [70] + 3
  sta COLUP0              ; [73] + 3    < 41
  PLAY_MUSIC              ; [0] + 5
  lda TCOL1               ; [5] + 3
  sta COLUP1              ; [8] + 3    < 47
MusicKernelStart
  lda MBUFF+0,Y           ; [11] + 4
  sta GRP0                ; [15] + 3    < 41
  lda MBUFF+7,Y           ; [18] + 4
  sta GRP1                ; [22] + 3    < 47
  lda MBUFF+21,Y          ; [25] + 4
  sta TEMP                ; [29] + 3
  lda TCOL2               ; [32] + 3
  ldx MBUFF+14,Y          ; [35] + 4
  ldy TCOL3               ; [39] + 3
  sta COLUP0              ; [42] + 3    >= 44 <= 52
  stx GRP0                ; [45] + 3    >= 44 <= 52
  sty COLUP1              ; [48] + 3    > 49 < 58
  lda TEMP                ; [51] + 3
  sta GRP1                ; [54] + 3    > 49 < 58
  dec LINE                ; [57] + 5
  bpl MusicKernelLoop     ; [62] + 2/3
  ldx #0                  ; [64] + 2
  stx GRP0                ; [66] + 3
  stx GRP1                ; [69] + 3
  stx GRP0                ; [72] + 3
  PLAY_MUSIC              ; [75] + 5
  rts                     ; [4] + 6    = 10
EndMusicKernel
  if (>MusicKernel != >EndMusicKernel)
    echo "WARNING: MusicKernel Crosses Page Boundary!"
  endif

; -----------------------------------------------------------------------------
; MENU SETUP
; -----------------------------------------------------------------------------

  ; Setup Menu Page (A = Section)
PageSetup
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; Background & Highlight Colour
  ldy #YELLOW             ; [7] + 2     Yellow
  cpx STATE               ; [9] + 3
  beq PageActiveBack      ; [12] + 2/3
  SLEEP 3                 ; [14] + 3
  jmp StorePageBack       ; [17] + 3
PageActiveBack
  ldy FCOL                ; [17] + 3    Flashing Yellow
StorePageBack
  sty HCOL                ; [20] + 3

  ; Normal Menu Colour
  ldy #ORANGE             ; [26] + 2
  sty TCOL                ; [28] + 3
  
  ; Heading Colour
  ldy #WHITE              ; [31] + 2
  sty COLUP0              ; [33] + 3
  sty COLUP1              ; [36] + 3
  rts                     ; [39] + 6    = 35

  ; Skip Lines (X = Lines to Skip)
SkipMenuLines
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  SLEEP 30                ; [5] + 30
  dex                     ; [35] + 2
  bpl SkipMenuLines       ; [37] + 2/3
  rts                     ; [39] + 6 = 45

  ; Sound Effects (Lower = Higher Priority)
  ; 1 = Select
  ; 2 = Move
MSoundTab
  DC.B  0, <MSelectSnd, <MMoveSnd
MSelectSnd
  DC.B  31, 31, 31, 31
MMoveSnd
  DC.B  24, 24, 24, 24

  DC.B  "NOVGOROD"

; -----------------------------------------------------------------------------
; MENU DATA
; -----------------------------------------------------------------------------

  ALIGN 256

  ; Menu Text
MenuMessages
  ; Game Type
  DC.B  <BL_GG, <BL_AA, <BL_MM, <BL_EE, <BL_GAP, <BL_SS
  DC.B  <BL_TT, <BL_YY, <BL_LL, <BL_EE, <BL_COLON, <BL_GAP
  ; A - Marathon
  DC.B  <BL_AA, <BL_GAP, <BL_MM, <BL_AA, <BL_RR, <BL_AA
  DC.B  <BL_TT, <BL_HH, <BL_OO, <BL_NN, <BL_GAP, <BL_GAP
  ; B - Sprint 25
  DC.B  <BL_BB, <BL_GAP, <BL_SS, <BL_PP, <BL_RR, <BL_II
  DC.B  <BL_NN, <BL_TT, <BL_GAP, <BL_22, <BL_55, <BL_GAP
  ; C - Sprint 40
  DC.B  <BL_CC, <BL_GAP, <BL_SS, <BL_PP, <BL_RR, <BL_II
  DC.B  <BL_NN, <BL_TT, <BL_GAP, <BL_44, <BL_00, <BL_GAP
  ; D - Ultra
  DC.B  <BL_DD, <BL_GAP, <BL_UU, <BL_LL, <BL_TT, <BL_RR
  DC.B  <BL_AA, <BL_BANG, <BL_GAP, <BL_GAP, <BL_GAP, <BL_GAP
  ; Music
  DC.B  <BL_MM, <BL_UU, <BL_SS, <BL_II, <BL_CC, <BL_GAP
  DC.B  <BL_TT, <BL_YY, <BL_PP, <BL_EE, <BL_COLON, <BL_GAP
  ; 123X
  DC.B  <BL_GAP, <BL_11, <BL_GAP, <BL_22, <BL_GAP, <BL_33
  DC.B  <BL_GAP, <BL_PAUSE, <BL_GAP, <BL_GAP, <BL_GAP, <BL_GAP
  ; Level
  DC.B  <BL_GAP, <BL_GAP, <BL_GAP, <BL_LL, <BL_EE, <BL_VV
  DC.B  <BL_EE, <BL_LL, <BL_COLON, <BL_GAP, <BL_GAP, <BL_GAP
  DC.B  <BL_GAP, <BL_00, <BL_GAP, <BL_11, <BL_GAP, <BL_22
  DC.B  <BL_GAP, <BL_33, <BL_GAP, <BL_44, <BL_GAP, <BL_GAP
  DC.B  <BL_GAP, <BL_55, <BL_GAP, <BL_66, <BL_GAP, <BL_77
  DC.B  <BL_GAP, <BL_88, <BL_GAP, <BL_99, <BL_GAP, <BL_GAP
  ; Depth
  DC.B  <BL_GAP, <BL_GAP, <BL_GAP, <BL_DD, <BL_EE, <BL_PP
  DC.B  <BL_TT, <BL_HH, <BL_COLON, <BL_GAP, <BL_GAP, <BL_GAP
  DC.B  <BL_GAP, <BL_GAP, <BL_GAP, <BL_00, <BL_GAP, <BL_11
  DC.B  <BL_GAP, <BL_22, <BL_GAP, <BL_GAP, <BL_GAP, <BL_GAP
  DC.B  <BL_GAP, <BL_GAP, <BL_GAP, <BL_33, <BL_GAP, <BL_44
  DC.B  <BL_GAP, <BL_55, <BL_GAP, <BL_GAP, <BL_GAP, <BL_GAP
  ; Credits
  DC.B  <BL_CC, <BL_OO, <BL_DD, <BL_II, <BL_NN, <BL_GG
  DC.B  <BL_COLON, <BL_GAP, <BL_GAP, <BL_GAP, <BL_GAP, <BL_GAP
  DC.B  <BL_CC, <BL_HH, <BL_RR, <BL_II, <BL_SS, <BL_GAP
  DC.B  <BL_WW, <BL_AA, <BL_LL, <BL_TT, <BL_OO, <BL_NN
  DC.B  <BL_ZZ, <BL_AA, <BL_CC, <BL_HH, <BL_GAP, <BL_MM
  DC.B  <BL_AA, <BL_TT, <BL_LL, <BL_EE, <BL_YY, <BL_GAP
  DC.B  <BL_FF, <BL_RR, <BL_EE, <BL_DD, <BL_GAP, <BL_QQ
  DC.B  <BL_UU, <BL_II, <BL_MM, <BL_BB, <BL_YY, <BL_GAP
  DC.B  <BL_AA, <BL_RR, <BL_TT, <BL_WW, <BL_OO, <BL_RR
  DC.B  <BL_KK, <BL_COLON, <BL_GAP, <BL_GAP, <BL_GAP, <BL_GAP
  DC.B  <BL_NN, <BL_AA, <BL_TT, <BL_HH, <BL_AA, <BL_NN
  DC.B  <BL_GAP, <BL_SS, <BL_TT, <BL_RR, <BL_UU, <BL_MM
  DC.B  <BL_PP, <BL_UU, <BL_BB, <BL_LL, <BL_II, <BL_SS
  DC.B  <BL_HH, <BL_II, <BL_NN, <BL_GG, <BL_COLON, <BL_GAP
  DC.B  <BL_AA, <BL_LL, <BL_GAP, <BL_YY, <BL_AA, <BL_RR
  DC.B  <BL_UU, <BL_SS, <BL_SS, <BL_OO, <BL_GAP, <BL_GAP
EndMenuMessages
  if (>MenuMessages != >EndMenuMessages)
    echo "WARNING: Menu Messages Crosses Page Boundary!"
  endif

  DC.B  "PERM"

  ; Font LHS
  ALIGN   256
  LEFTCHARS
  
  ; Font RHS
  ALIGN   256
  RIGHTCHARS
  
  ; Logo
  ALIGN   256
  MENULOGO

  DC.B  "UKHTA"

  ALIGN 256

Credits
  ; Position Text
  jsr PositionMenuText    ; [0 -> 14]

  ; Chetiry Logo Colour
  ldx #RED                ; [14] + 2
  stx LOGOCOL             ; [16] + 3

  ; Flashing Star Colour
  ldx #>MenuStarCols      ; [19] + 2
  stx STARPTR+1           ; [21] + 3
  ldx #<MenuStarCols      ; [24] + 2
  lda CYCLE               ; [26] + 3
  and #%00010000          ; [29] + 2
  bne CreditsStarCol      ; [31] + 2/3
  beq NoCreditsStarCol    ; [33] + 3
CreditsStarCol
  ldx #<BlackStarCols     ; [34] + 2
NoCreditsStarCol
  stx STARPTR             ; [36] + 3

  ; Flashing Text Colour
  ldx #WHITE             ; [39] + 2
  lda CYCLE               ; [41] + 3
  and #%00110000          ; [44] + 2
  beq NoCreditsTextCol    ; [46] + 2/3
  bne CreditsTextCol      ; [48] + 3
NoCreditsTextCol
  ldx #0                  ; [49] + 3
CreditsTextCol
  stx HCOL                ; [42] + 3

  START_FRAME_MUSIC       ; [8]

  ; Display Credits
  jsr MenuLogoKernel      ; [41->15]
  ldx HCOL                ; [15] + 3
  stx COLUP0              ; [18] + 3
  stx COLUP1              ; [21] + 3
  ldx #8                  ; [24] + 2
  jsr SkipMenuLines       ; [0 -> 45]
  ldx #13*12              ; [45] + 2      Coding:
  stx MSG                 ; [47] + 3
  jsr LoadMenuText        ; [0 -> 27]
  jsr MenuTextKernel      ; [52 -> 11]
  ldy #ORANGE             ; [11] + 2
  sty COLUP0              ; [13] + 3
  sty COLUP1              ; [16] + 3
  ldx #14*12              ; [19] + 2      Chris Walton
  stx MSG                 ; [21] + 3
  jsr LoadMenuText        ; [0 -> 27]
  jsr MenuTextKernel      ; [52 -> 11]
  ldx #15*12              ; [11] + 2      Zack Matley
  stx MSG                 ; [13] + 3
  jsr LoadMenuText        ; [0 -> 27]
  jsr MenuTextKernel      ; [52 -> 11]
  ldx #16*12              ; [11] + 2      Fred Quimby
  stx MSG                 ; [13] + 3
  jsr LoadMenuText        ; [0 -> 27]
  jsr MenuTextKernel      ; [52 -> 11]
  ldx HCOL                ; [11] + 3
  stx COLUP0              ; [14] + 3
  stx COLUP1              ; [17] + 3
  ldx #8                  ; [20] + 2
  jsr SkipMenuLines       ; [0 -> 45]
  ldx #17*12              ; [54] + 2      Artwork:
  stx MSG                 ; [56] + 3
  jsr LoadMenuText        ; [0 -> 27]
  jsr MenuTextKernel      ; [52 -> 11]
  ldy #ORANGE             ; [11] + 2
  sty COLUP0              ; [13] + 3
  sty COLUP1              ; [16] + 3
  ldx #18*12              ; [19] + 2      Nathan Strum
  stx MSG                 ; [21] + 3
  jsr LoadMenuText        ; [0 -> 27]
  jsr MenuTextKernel      ; [52 -> 11]
  ldx HCOL                ; [11] + 3
  stx COLUP0              ; [14] + 3
  stx COLUP1              ; [17] + 3
  ldx #8                  ; [20] + 2
  jsr SkipMenuLines       ; [0 -> 45]
  ldx #19*12              ; [45] + 2      Publishing:
  stx MSG                 ; [47] + 3
  jsr LoadMenuText        ; [0 -> 27]
  jsr MenuTextKernel      ; [52 -> 11]
  ldy #ORANGE             ; [11] + 2
  sty COLUP0              ; [13] + 3
  sty COLUP1              ; [16] + 3
  ldx #20*12              ; [19] + 2      Al Yarusso
  stx MSG                 ; [21] + 3
  jsr LoadMenuText        ; [0 -> 27]
  jsr MenuTextKernel      ; [52 -> 11]
  jmp EndCredits

  DC.B  "CCCP"

  echo "----",($FFF4 - *) , "bytes left (BANK 6 - MAIN MENU)"

  ORG     $EFF4
  RORG    $FFF4
  DC.B    "BANK6", 0, 0, 0
  DC.W    Init6, Init6

