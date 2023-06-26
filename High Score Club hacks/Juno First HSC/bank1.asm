; -----------------------------------------------------------------------------
; BANK 1 - MAIN GAME
; -----------------------------------------------------------------------------

  SEG     BANK1
  ORG     $8000
  RORG    $F000

Init1
  ; Switch to Bank 8
  nop     $FFFB
EndGame
  ; Switch to Bank 6
  nop     $FFF9
  ; End of Game Logic
  jmp MainLoop
ShowKernel1
  ; Switch to Bank 2
  nop     $FFF5
ShowKernel2
  ; Switch to Bank 3
  nop     $FFF6
GameSpeech
  ; Switch To Bank 7
  nop     $FFFA
  ; Exit Speech
  jmp EndSpeech0
  nop
  nop
  nop

; -----------------------------------------------------------------------------
; PART 0 - GAME INITIALISATION & DISPLAY "GET READY" MESSAGE
; -----------------------------------------------------------------------------

GameStart
  ; Silence Any Sounds
  lda #0
  sta AUDV0
  sta AUDV1

  ; Reset Stack Pointer
  ldx #$FF
  txs

  ; Start Next Frame
  WAIT_OVERSCAN
  START_VBLANK
  
  ; Play Start Speech
  SET_POINTER SPTR, JunoFirstSpeech
  jsr PlaySpeech

  ; Set "Get Ready" Timer
  lda #80
  sta COUNTER

  ; Set Fire Button Variable
  lda #0
  sta TEMP2

  ; Set Three Sprite Copies and Delay
  ldx #>GetReadyText
  sta WSYNC                 ; [0] 
  lda #%00000011            ; [0] + 2
  sta VDELP0                ; [2] + 3
  sta VDELP1                ; [5] + 3
  sta NUSIZ0                ; [8] + 3
  sta NUSIZ1                ; [11] + 3
  ; Set Fine Sprite Positionins
  sta HMCLR                 ; [14] + 3
  lda #%00010000            ; [17] + 2
  sta HMP1                  ; [19] + 3
  ; Load Sprite Pointers (Part 1)
  lda #<G__G                ; [22] + 2
  sta INVS+0                ; [24] + 3
  stx INVS+1                ; [27] + 3
  lda #<G_ET                ; [30] + 2
  sta INVS+2                ; [32] + 3
  stx INVS+3                ; [35] + 3
  ; Set Coarse Sprite Positions
  sta RESP0                 ; [38] + 3    = 41 EXACT
  sta RESP1                 ; [41] + 3    = 44 EXACT
  ; Load Sprite Pointers (Part 2)
  lda #<G__R                ; [44] + 2
  sta INVS+4                ; [46] + 3
  stx INVS+5                ; [49] + 3
  lda #<G_EA                ; [52] + 2
  sta INVS+6                ; [54] + 3
  stx INVS+7                ; [57] + 3
  lda #<G_DY                ; [60] + 2
  sta INVS+8                ; [62] + 3
  stx INVS+9                ; [65] + 3
  lda #<G_BANG_             ; [68] + 2
  sta INVS+10               ; [70] + 3
  stx INVS+11               ; [73] + 3
  ; Reposition Sprites
  sta HMOVE                 ; [0] + 3
  
ShowGetReady
  ; Set Text Colour
  dec CYCLE
  lda #$0F
  sta COLUP0
  sta COLUP1

  ; Start Screen
  WAIT_VBLANK
  START_SCREEN
  SKIP_LINES 90

  ; Set Lines
  ldy #4 
  sty LINE
  ; Load First Sprite Data
  lda (INVS+0),Y
  sta GRP0
  sta WSYNC                 ; [0]
  jmp GetReadyKernel        ; [0] + 3
GetReadyLoop
  ; Fetch Current Line
  ldy LINE                  ; [62] + 3
  SLEEP 6                   ; [65] + 6
  ; Display First 3 Digits
  lda (INVS+0),Y            ; [71] + 5
  sta GRP0                  ; [0] + 3     > 54
GetReadyKernel
  lda (INVS+2),Y            ; [3] + 5
  sta GRP1                  ; [8] + 3     < 42
  lda (INVS+4),Y            ; [11] + 5
  sta GRP0                  ; [16] + 3    < 44
  ; Pre-fetch Remaining 3 Digits
  lax (INVS+6),Y            ; [19] + 5
  lda (INVS+8),Y            ; [24] + 5
  sta TEMP                  ; [29] + 3
  lda (INVS+10),Y           ; [32] + 5
  tay                       ; [37] + 2
  lda TEMP                  ; [39] + 3
  ; Display Remaining 3 Digits
  stx GRP1                  ; [42] + 3    > 44 < 47
  sta GRP0                  ; [45] + 3    > 46 < 50
  sty GRP1                  ; [48] + 3    > 49 < 52
  sta GRP0                  ; [51] + 3    > 52 < 55
  ; Update Counter
  dec LINE                  ; [54] + 5
  bpl GetReadyLoop          ; [59] + 2/3
  lda #0                    ; [61] + 2
  sta GRP1                  ; [63] + 3
  sta GRP0                  ; [66] + 3
  sta GRP1                  ; [69] + 3
EndGetReadyKernel
  if (>GetReadyKernel != >EndGetReadyKernel)
    echo "WARNING: GetReady Kernel Crosses Page Boundary!"
  endif
  
  ; Skip To Next Frame
  WAIT_SCREEN
  START_OVERSCAN
  WAIT_OVERSCAN
  START_VBLANK
  
  ; Check If Fire Pressed
  lda INPT4
  bmi FireNotPressed
  ; Check If Fire Released
  lda TEMP2
  beq EndFireCheck
  ; Finish Early
  bne EndGetReady
FireNotPressed
  sta TEMP2
EndFireCheck
  
  ; Decrease Counter
  dec COUNTER
  beq EndGetReady
  jmp ShowGetReady
EndGetReady

  ; Clear Game Variables
  CLEAR_GAMEVARS

  ; Hide All Bullets
  ldx #BMAX2-1
  lda #$FF
BInit    
  sta B1Y,X
  dex
  bpl BInit
  
  ; Set Counters To -1 (causes next wave to be loaded)
  ; lda #$FF
  sta SPAWN
  sta KILLS
  
  ; Reset Cycle and Score (but not High Score!)
  lda #0
  sta CYCLE
  sta SCORE+0
  sta SCORE+1
  sta SCORE+2

  ; Reset Sprite Registers
  sta NUSIZ0
  sta NUSIZ1
  sta VDELP0
  sta VDELP1

  ; Reflect Playfield
  lda #%00000001
  sta CTRLPF

  ; Reset Game Timers
  lda #0
  sta TIME
  sta STIME

  ; Set Starting Wave (-1) & Lives
  ldx #%01000010
  lda STARTWAVE
  ; Set Practise Bit (3) If Starting Wave Non Zero
  beq SetStartWave
  ldx #%01001010
SetStartWave
  sec
  sbc #1
  and #%00111111
  sta WAVE
  stx LIVES

  ; Set Initial Ship Position
  lda #XMIDDLE
  sta PLAYERX
  
  ; Set Warp Counter (Invulnerability) For Ship
  lda #WARPTIME
  sta COUNTER
  
  ; Set Default Ship & Colour Pointers
  lda #<A_EmptyShip
  sta SPTR
  lda #>A_Ship
  sta SPTR+1
  lda #<A_ShipCol
  sta CPTR
  lda #>A_ShipCol
  sta CPTR+1

  ; Begin Game Kernel
  jmp ShowKernel1

; -----------------------------------------------------------------------------
; PART 1 - MAIN GAME LOOP
; -----------------------------------------------------------------------------

MainLoop
  ; Start Next Frame
  WAIT_OVERSCAN
  
  ; Do Vertical Blank
  lda #2                    ; VSYNC enable
  sta WSYNC                 ; [0]
  sta VSYNC                 ; [0] + 3 = 3 CYCLES
  
  ; Update Game Cycle & Message Timer
  dec CYCLE                 ; [0] + 5
  lda CYCLE                 ; [5] + 3
  lsr                       ; [8] + 2
  bcc EndMessageDec         ; [10] + 2/3
  lsr                       ; [12] + 2
  bcc EndMessageDec         ; [14] + 2/3
  lsr                       ; [16] + 2
  bcc EndMessageDec         ; [18] + 2/3
  lsr                       ; [20] + 2
  bcc EndMessageDec         ; [22] + 2/3
  ; Check Message Timer
  lda MESSAGE               ; [24] + 3
  and #%11111000            ; [27] + 2
  beq EndMessageDec         ; [29] + 2/3
  ; Decrease Timer
  sec                       ; [31] + 2
  sbc #8                    ; [33] + 2
  and #%11111000            ; [35] + 2
  beq ClearMessage          ; [37] + 2/3
  ; Merge Timer and Message
  sta TEMP                  ; [39] + 3
  lda MESSAGE               ; [42] + 3
  and #%00000111            ; [45] + 2
  ora TEMP                  ; [47] + 3
ClearMessage
  ; Remove Message When Timer Reaches Zero
  sta MESSAGE               ; [50] + 3
EndMessageDec               ; WORST CASE = 53 CYCLES
  
  ; Generate Pseudo-Random Number
  lda RANDOM                ; [0] + 3
  asl                       ; [3] + 2
  bcc NoEor                 ; [5] + 2/3
  eor #$CF                  ; [7] + 2
NoEor
  sta RANDOM                ; [9] + 3
                            ; WORST CASE = 12 CYCLES
  
  sta WSYNC                 ; [0]

  ; Set Default Ship Pointers
  lda #<A_EmptyShip         ; [0] + 2
  sta SPTR                  ; [2] + 3
  lda #>A_Ship              ; [5] + 2
  sta SPTR+1                ; [7] + 3
  ; Set Default Ship Colour Pointers
  lda #<A_ShipCol           ; [10] + 2
  sta CPTR                  ; [12] + 3
  lda #>A_ShipCol           ; [15] + 2
  sta CPTR+1                ; [17] + 3 
                            ; RUNTIME = 20 CYCLES

  ; Don't Change Time If Paused
  lda WAVE                  ; [0] + 3
  bmi SkipUpdateTime        ; [3] + 2/3
  ; Update Timer On 0, 85, & 170
  lda CYCLE                 ; [5] + 3
  beq UpdateTime            ; [8] + 2/3
  cmp #85                   ; [10] + 2
  beq UpdateTime            ; [12] + 2/3
  cmp #170                  ; [14] + 2
  beq UpdateTime            ; [16] + 2/3
SkipUpdateTime
  sta WSYNC                 ; [0]
  jmp EndTimeDec            ; [0] + 3
TimeOut
  sta WSYNC                 ; [0]
  ; Clear Any Messages
  lda #0                    ; [0] + 2
  sta MESSAGE               ; [2] + 3
  ; Clear Warp/Shot Counter
  sta COUNTER               ; [5] + 3
  ; Reset Fuel Timer & End HyperSpace
  lda #MAXTIME              ; [8] + 2
  sta TIME                  ; [10] + 3
  ; Check If Already Dead
  lda LIVES                 ; [13] + 3
  bmi EndTimeDec            ; [16] + 2/3
  ; Explode Player
  and #%00001111            ; [18] + 2
  ora #%10010000            ; [20] + 2
  sta LIVES                 ; [22] + 3
  jmp EndTimeDec            ; [25] + 3
UpdateTime
  ; Store Hyperspace/Astronaut Bits
  lax TIME                  ; [19] + 3
  and #%11000000            ; [22] + 2
  sta TEMP                  ; [24] + 3
  ; Decrease Fuel Timer
  txa                       ; [27] + 3
  sec                       ; [30] + 2
  and #%00111111            ; [32] + 2
  sbc #1                    ; [34] + 2
  ; Check Time Out
  bmi TimeOut               ; [36] + 2/3
  ; Check Low Fuel
  cmp #LOWTIME              ; [38] + 2
  ; Merge With Hyperspace
  ora TEMP                  ; [40] + 3
  sta TIME                  ; [43] + 3  
                            ; WORST CASE = 46 CYCLES
  
  sta WSYNC                 ; [0]
  
  ; Check If Fuel Is Low
  bcs EndFuelCheck          ; [0] + 2/3
FuelLow
  lda SPEECH                ; [2] + 3
  bne SkipFuelLowSpeech     ; [5] + 2/3
  lda #((3<<6)|24)          ; [7] + 2
  sta SPEECH                ; [9] + 3
SkipFuelLowSpeech
  ; Show "Low Fuel" If No Message is Being Displayed
  lda MESSAGE               ; [12] + 3
  bne CheckFuelLowSound     ; [15] + 2/3
  lda #%01000010            ; [17] + 2
  sta MESSAGE               ; [19] + 3
CheckFuelLowSound
  ; Check Sound Priority
  lda SND0                  ; [22] + 3
  beq PlayFuelLowSound      ; [25] + 2/3
  and #%11100000            ; [27] + 2
  cmp #3<<5                 ; [29] + 2
  bcc EndFuelCheck          ; [31] + 2/3
PlayFuelLowSound
  lda #((3<<5)|31)          ; [33] + 2
  sta SND0                  ; [35] + 3
EndFuelCheck
  ; Don't Decrease If Spawn Timer Is At Zero
  lda STIME                 ; [38] + 3
  beq EndTimeDec            ; [41] + 2/3
  ; Calculate Aliens On Screen (KILLS >= SPAWN)
  sec                       ; [43] + 2
  lda KILLS                 ; [45] + 3
  sbc SPAWN                 ; [48] + 3
  ; Don't Decrease Spawn Timer If At Max Aliens
  cmp #INVMAX               ; [51] + 2
  bcs EndTimeDec            ; [53] + 2/3
  ; Decrease Spawn Timer
  dec STIME                 ; [55] + 5
EndTimeDec                  ; WORST CASE = 60 CYCLES


  lda #1                    ; [0] + 2     VSYNC disable
  sta WSYNC                 ; [0]
  sta VSYNC                 ; [0] + 3
  
  ; Set VBlank Timer
  lda #VBLANKDELAY          ; [3] + 2
  sta TIM64T                ; [5] + 3 
                            ; RUNTIME = 8 CYCLES

  ; Set Sky Colours & Sound Effects
  ldx #0                    ; [0] + 2     Default Volume
  ldy #0                    ; [2] + 2     Default Sky Colour
  ; Check For Pause
  bit WAVE                  ; [4] + 3
  bmi PauseSky              ; [7] + 2/3
  ; Check If Dead
  bit LIVES                 ; [9] + 3
  bmi PauseSky              ; [12] + 2/3
  ; Check For Astronaut and Hyperspace
  bit TIME                  ; [14] + 3
  bvs Astronaut             ; [17] + 2/3
  bmi Hyperspace            ; [19] + 2/3
  ; Check If Warping
  lda COUNTER               ; [21] + 3
  and #%00011111            ; [24] + 2
  beq NormalThrob           ; [26] + 2/3
  ; Flash Sky When Nearing End Of Warp
  cmp #WARPLOW              ; [28] + 2
  bcs PauseSky              ; [30] + 2/3
  ; Preload Paused Sky
  IF (PALCOLS)
  ldy #$B4                  ; [32] + 2
  ELSE
  ldy #$A4                  ; [32] + 2
  ENDIF
  lda CYCLE                 ; [34] + 3
  lsr                       ; [37] + 2
  lsr                       ; [39] + 2
  lsr                       ; [41] + 2
  bcs NormalThrob           ; [43] + 2/3
PauseSky
  ; Show Paused Sky
  IF (PALCOLS)
  ldy #$B0                  ; [45] + 2
  ELSE
  ldy #$A0                  ; [45] + 2
  ENDIF
NormalThrob
  ; Play Normal Throb Sound
  lda #6                    ; [47] + 2
  sta AUDC1                 ; [49] + 3
  lda CYCLE                 ; [52] + 3
  lsr                       ; [55] + 2 
  and #%00011111            ; [57] + 2
  tax                       ; [59] + 2
  lda ThrobSnd,X            ; [61] + 4
  ldx #4                    ; [65] + 2
  bne StoreSoundSky         ; [67] + 3 = 70
EndHyperspace
  ; Clear Message
  sta MESSAGE               ; [30] + 3
  ; Clear Hyperspace/Astronaut Bits
  lda TIME                  ; [33] + 3
  and #%00111111            ; [36] + 2
  sta TIME                  ; [38] + 3
  jmp NormalThrob           ; [41] + 3
Hyperspace
  lda COUNTER               ; [22] + 3
  and #%00011111            ; [25] + 2
  beq EndHyperspace         ; [27] + 2/3
  cmp #HYPERLOW             ; [29] + 2
  bcs HyperSky              ; [31] + 2/3
  ; Preload Hyperspace Sky Colour (Light)
  IF (PALCOLS)
  ldy #$64                  ; [33] + 2
  ELSE
  ldy #$44                  ; [33] + 2
  ENDIF
  ; Flash Sky When Nearing End Of Hyperspace
  lda CYCLE                 ; [35] + 3
  lsr                       ; [38] + 2
  lsr                       ; [40] + 2
  lsr                       ; [42] + 2
  bcs HyperThrob            ; [44] + 2/3
HyperSky
  ; Set Hyperspace Sky (Dark)
  IF (PALCOLS)
  ldy #$60                  ; [46] + 2
  ELSE
  ldy #$40                  ; [46] + 2
  ENDIF
HyperThrob
  ; Play Hyperspace Throb Sound
  lda #6                    ; [48] + 2
  sta AUDC1                 ; [50] + 3
  lda CYCLE                 ; [53] + 3
  and #%00000111            ; [56] + 2
  tax                       ; [58] + 2
  lda HyperSnd,X            ; [60] + 4
  ldx #7                    ; [64] + 2
  bne StoreSoundSky         ; [66] + 3 = 69
Astronaut
  ; Astronaut Sky Effect
  IF (PALCOLS)
  ldy #$50                  ; [12] + 2
  ELSE
  ldy #$D0                  ; [12] + 2
  ENDIF
  ; Astronaut Sound Effect
  lda #3                    ; [14] + 2
  sta AUDC1                 ; [16] + 3
  lda CYCLE                 ; [19] + 3
  and #%01111111            ; [22] + 2
  lsr                       ; [24] + 2
  lsr                       ; [26] + 2
  lsr                       ; [28] + 2
  adc #5                    ; [30] + 2
  ldx #4                    ; [32] + 2 = 34
StoreSoundSky
  sty SKYCOL                ; [70] + 3
  stx AUDV1                 ; [73] + 3
  sta AUDF1                 ; [76] + 3
                            ; WORST CASE = 79 CYCLES

; -----------------------------------------------------------------------------
; PART 2 - ALIEN AND BULLET FLICKER SORTING
; -----------------------------------------------------------------------------

  ; Find First Non-Empty Alien
FlickerIndex
  ldx #16                   ; [0] + 2
  stx TEMP                  ; [2] + 3
First15
  lda INVT+15               ; [5] + 3
  beq First14               ; [8] + 3
  jmp Store15
First14
  lda INVT+14               ; [11] + 3
  beq First13               ; [14] + 3
  jmp Store14
First13
  lda INVT+13               ; [17] + 3
  beq First12               ; [20] + 3
  jmp Store13
First12
  lda INVT+12               ; [23] + 3
  beq First11               ; [26] + 3
  jmp Store12
First11
  lda INVT+11               ; [29] + 3
  beq First10               ; [32] + 3
  jmp Store11
First10
  lda INVT+10               ; [35] + 3
  beq First9                ; [38] + 3
  jmp Store10
First9
  lda INVT+9                ; [41] + 3
  beq First8                ; [44] + 3
  jmp Store9  
First8
  lda INVT+8                ; [47] + 3
  beq First7                ; [50] + 3
  jmp Store8
First7
  lda INVT+7                ; [53] + 3
  beq First6                ; [56] + 3
  jmp Store7
First6
  lda INVT+6                ; [59] + 3
  beq First5                ; [62] + 3
  jmp Store6
First5
  lda INVT+5                ; [65] + 3
  beq First4                ; [68] + 3
  jmp Store5
First4
  lda INVT+4                ; [71] + 3
  beq First3                ; [74] + 3
  jmp Store4
First3
  lda INVT+3                ; [77] + 3
  beq First2                ; [80] + 3
  jmp Store3
First2
  lda INVT+2                ; [83] + 3
  beq First1                ; [86] + 3
  jmp Store2
First1
  lda INVT+1                ; [89] + 3
  beq First0                ; [92] + 3
  jmp Store1
First0
  lda INVT+0                ; [95] + 3
  beq EndFirst              ; [98] + 3
  jmp Store0
EndFirst
  ; ldx TEMP
  jmp StoreEndMarker        ; [101] + 3 
                            ; WORST CASE = 104 (1.4 SCANLINES)

  ; Unrolled Flickersort Algorithm (Essentially Bubblesort)
Store15
  ldy INVY+15
  ldx #15
Do14
  lda INVT+14               ; [0] + 3
  beq Do13                  ; [3] + 2
  FLICKERSORT 14            ; [5] + 56
Store14
  ldy INVY+14               ; [61] + 3
  ldx #14                   ; [64] + 2 
Do13                        ; WORST CASE = 66
  lda INVT+13
  beq Do12
  FLICKERSORT 13
Store13
  ldy INVY+13
  ldx #13
Do12
  lda INVT+12
  beq Do11
  FLICKERSORT 12
Store12
  ldy INVY+12
  ldx #12
Do11
  lda INVT+11
  beq Do10
  FLICKERSORT 11
Store11
  ldy INVY+11
  ldx #11
Do10
  lda INVT+10
  beq Do9
  FLICKERSORT 10
Store10
  ldy INVY+10
  ldx #10
Do9
  lda INVT+9
  beq Do8
  FLICKERSORT 9
Store9
  ldy INVY+9
  ldx #9
Do8
  lda INVT+8
  beq Do7
  FLICKERSORT 8
Store8
  ldy INVY+8
  ldx #8
Do7
  lda INVT+7
  beq Do6
  FLICKERSORT 7
Store7
  ldy INVY+7
  ldx #7
Do6
  lda INVT+6
  beq Do5
  FLICKERSORT 6
Store6
  ldy INVY+6
  ldx #6
Do5
  lda INVT+5
  beq Do4
  FLICKERSORT 5
Store5
  ldy INVY+5
  ldx #5
Do4
  lda INVT+4
  beq Do3
  FLICKERSORT 4
Store4
  ldy INVY+4
  ldx #4
Do3
  lda INVT+3
  beq Do2
  FLICKERSORT 3
Store3
  ldy INVY+3
  ldx #3
Do2
  lda INVT+2
  beq Do1
  FLICKERSORT 2
Store2
  ldy INVY+2
  ldx #2
Do1
  lda INVT+1
  beq Do0
  FLICKERSORT 1
Store1
  ldy INVY+1
  ldx #1
Do0
  lda INVT+0
  beq EndNext
  FLICKERSORT 0
Store0
  ldx #0                    ; WORST CASE = 15 * 66 = 990 (13 SCANLINES)
EndNext
  ldy TEMP                  ; [0] + 3
  stx INVS,Y                ; [3] + 4
StoreEndMarker
  ldy #$FF                  ; [7] + 2
  sty INVS,X                ; [9] + 4
                            ; WORST CASE = 13

  ; Find First Bullet
  ldx #0                    ; [0] + 2
  ldy B1Y+3                 ; [2] + 3
  bpl B1Store3              ; [3] + 3 = 6
  ldy B1Y+2
  bpl B1Store2
  ldy B1Y+1
  bpl B1Store1
  bmi EndBullet1
  ; Flickersort First Bullets
B1Store3
  ldx #3                    ; [0] + 2
B1Do2
  lda B1Y+2                 ; [2] + 3
  bmi B1Do1                 ; [5] + 2/3
  ldy B1Y,X                 ; [7] + 4
  sta B1Y,X                 ; [11] + 4
  sty B1Y+2                 ; [15] + 3
  lda B1X+2                 ; [18] + 3
  ldy B1X,X                 ; [21] + 4
  sta B1X,X                 ; [25] + 4
  sty B1X+2                 ; [29] + 3 = 32
B1Store2
  ldx #2
B1Do1
  lda B1Y+1
  bmi B1Do0
  ldy B1Y,X
  sta B1Y,X
  sty B1Y+1
  lda B1X+1
  ldy B1X,X
  sta B1X,X
  sty B1X+1
B1Store1
  ldx #1
B1Do0
  lda B1Y+0
  bmi EndBullet1
  ldy B1Y,X
  sta B1Y,X
  sty B1Y+0
  lda B1X+0
  ldy B1X,X
  sta B1X,X
  sty B1X+0
  ldx #0
EndBullet1
  ; Store Visible Bullet
  stx B1                    ; WORST CASE = 6 + 32*3 + 3 = 105 (1.4 SCANLINES)
  
  ; Find Second Bullet
  ldx #0                    ; [0] + 2
  ldy B2Y+3                 ; [2] + 3
  bpl B2Store3              ; [3] + 3
  ldy B2Y+2
  bpl B2Store2
  ldy B2Y+1
  bpl B2Store1
  bmi EndBullet2
  ; Flickersort Second Bullets
B2Store3
  ldx #3                    ; [0] + 2
B2Do2
  lda B2Y+2                 ; [2] + 3
  bmi B2Do1                 ; [5] + 2/3
  ldy B2Y,X                 ; [7] + 4
  sta B2Y,X                 ; [11] + 4
  sty B2Y+2                 ; [15] + 3
  lda B2X+2                 ; [18] + 3
  ldy B2X,X                 ; [21] + 4
  sta B2X,X                 ; [25] + 4
  sty B2X+2                 ; [29] + 3 = 32
B2Store2
  ldx #2
B2Do1
  lda B2Y+1
  bmi B2Do0
  ldy B2Y,X
  sta B2Y,X
  sty B2Y+1
  lda B2X+1
  ldy B2X,X
  sta B2X,X
  sty B2X+1
B2Store1
  ldx #1
B2Do0
  lda B2Y+0
  bmi EndBullet2
  ldy B2Y,X
  sta B2Y,X
  sty B2Y+0
  lda B2X+0
  ldy B2X,X
  sta B2X,X
  sty B2X+0
  ldx #0
EndBullet2
  ; Store Visible Bullets
  stx B2                    ; WORST CASE = 6 + 32*3 + 3 = 105 (1.4 SCANLINES)

  ; FLICKER SORT OF ALIENS AND BULLETS
  ; WORST CASE  = 104 + 990 + 13 + 105 + 105
  ;             = 1317 (17.3 SCANLINES)

; -----------------------------------------------------------------------------
; PART 3 - JOYSTICK HANDLING & PLAYER MOTION
; -----------------------------------------------------------------------------

  ; Check If Paused
  lda WAVE                  ; [0] + 3
  bpl CheckReset            ; [3] + 2/3
  ; Check For Reappear Delay
  lda COUNTER               ; [5] + 3
  and #%00011111            ; [8] + 2
  beq SkipFireCheck         ; [10] + 2/3
  ; Short Pause Before Allowing Fire Button
  cmp #(KEYWAIT-2)          ; [12] + 2
  bcs CheckReset            ; [14] + 2/3
  ; Check If Fire Pressed
  lda INPT4                 ; [16] + 3
  bmi CheckReset            ; [19] + 2/3
SkipFireCheck
  ; Check For Game Over
  lda LIVES
  bpl EndPause
  ; Silence Any SFX
  lda #0
  sta AUDV0
  sta AUDV1
  ; Reset Stack Pointer
  ldx #$FF
  txs
  ; Play Random Insult
  lda RANDOM
  and #%00000111    ; 8 Insults
  tax
  lda InsultTabHi,X
  sta SPTR+1
  lda InsultTabLo,X
  sta SPTR
  jsr PlaySpeech
  ; End Game
  jmp EndGame
EndPause
  ; Clear Pause Bit
  lda WAVE
  and #%01111111
  sta WAVE
  ; Clear Hyperspace/Astronaut
  lda TIME
  and #%00111111
  sta TIME
  ; Set Warp Timer
  lda #WARPTIME
  sta COUNTER
  
CheckReset
  ; Check Reset Switch
  lda SWCHB                 ; [22] + 3
  lsr                       ; [25] + 2
  bcs EndSwitches           ; [27] + 2/3
  ; Set Score To Zero
  lda #0
  sta SCORE+0
  sta SCORE+1
  sta SCORE+2
  sta WAVE
  ; Silence Sounds
  sta AUDV0
  sta AUDV1
  ; End Game
  jmp EndGame
EndSwitches                 ; WORST CASE = 30 CYCLES

  ; Check If Paused 
  lda WAVE                  ; [0] + 3
  bpl NotPaused             ; [3] + 2/3
Paused
  ; Slow Down (Or Grid Scrolls Forever)
  jmp Inertia
NotPaused                   ; WORST CASE = 6 CYCLES

  ; Display Ship Explosion/Appearance
  lda LIVES                 ; [0] + 3
  and #%01110000            ; [3] + 2
  beq NoExplosion           ; [5] + 2/3
  lsr                       ; [7] + 2
  lsr                       ; [9] + 2
  lsr                       ; [11] + 2
  lsr                       ; [13] + 2
  tax                       ; [15] + 2
  lda ExplosionOffset,X     ; [17] + 4
  sta SPTR                  ; [21] + 3
  lda #>A_ShipExplosion0    ; [24] + 2
  sta SPTR+1                ; [26] + 3
  lda #<A_ExplosionCol      ; [29] + 2
  sta CPTR                  ; [31] + 3
  jmp Inertia               ; [34] + 3
NoExplosion
  ; Display Ship As Normal
  lda #<A_Ship              ; [8] + 2
  sta SPTR                  ; [10] + 3

  ; Horizontal Joystick Movements
MoveHoriz
  lda PLAYERX               ; [13] + 3
  bit SWCHA                 ; [16] + 3
  bvc MoveLeft              ; [19] + 2/3
  bpl MoveRight             ; [21] + 2/3
  jmp EndHoriz              ; [23] + 3
MoveLeft
  ; Check Left Edge
  cmp #XMIN                 ; [22] + 2
  bcc EndHoriz              ; [24] + 2/3
  ; Decrement Position  
  dec PLAYERX               ; [26] + 5
  bcs EndHoriz              ; [31] + 3
MoveRight
  ; Check Right Edge
  cmp #XMAX                 ; [24] + 2
  bcs EndHoriz              ; [26] + 2/3
  ; Increment Position
  inc PLAYERX               ; [28] + 5
EndHoriz                    ; WORST CASE = 34 CYCLES

  ; Vertical Joystick Movements
MoveVertical
  lda #%00010000            ; [0] + 2
  bit SWCHA                 ; [2] + 3
  bne JoyDown               ; [5] + 2/3
  ; Play Sound
  lda #8                    ; [7] + 2
  sta AUDC1                 ; [9] + 3
  lda #4                    ; [12] + 2
  sta AUDV1                 ; [14] + 3
  lda #12                   ; [17] + 2
  sta AUDF1                 ; [19] + 3
  ; Update Ship Frame
  ldy #<A_ForwardFlame1     ; [22] + 2
  lda CYCLE                 ; [24] + 3
  and #%00000010            ; [27] + 2
  beq FlameUp2              ; [29] + 2/3
  bne StoreFlameUp          ; [31] + 3
FlameUp2
  ldy #<A_ForwardFlame2     ; [32] + 2
StoreFlameUp
  sty SPTR                  ; [34] + 3
  ; Check Max Speed
  lda SPEED                 ; [37] + 3
  bmi IncSpeed              ; [40] + 2/3
  cmp #MAXSPEED             ; [42] + 2
  bcs EndSpeed              ; [44] + 2/3 = 47
  ; Increase Speed
IncSpeed
  clc                       ; [43] + 2
  adc #SPEEDINC             ; [45] + 2
  sta SPEED                 ; [47] + 3
  jmp EndSpeed              ; [50] + 3 = 53
JoyDown
  lda #%00100000            ; [8] + 2
  bit SWCHA                 ; [10] + 3
  bne Inertia               ; [13] + 2/3
  ; Play Sound
  lda #8                    ; [15] + 2
  sta AUDC1                 ; [17] + 3
  lda #4                    ; [20] + 2
  sta AUDV1                 ; [22] + 3
  lda #10                   ; [25] + 2
  sta AUDF1                 ; [27] + 3
  ; Update Ship Frame
  ldy #<A_ReverseFlame1     ; [30] + 2
  lda CYCLE                 ; [32] + 3
  and #%00000010            ; [35] + 2
  beq FlameDown2            ; [37] + 2/3
  bne StoreFlameDown        ; [39] + 3
FlameDown2
  ldy #<A_ReverseFlame2     ; [40] + 2
StoreFlameDown
  sty SPTR                  ; [42] + 3
  ; Check Min Speed
  lda SPEED                 ; [45] + 3
  bpl DecSpeed              ; [48] + 2/3
  cmp #MINSPEED             ; [50] + 2
  bcc EndSpeed              ; [52] + 2/3 = 55
  ; Decrease Speed
DecSpeed
  sec                       ; [54] + 2
  sbc #SPEEDINC             ; [56] + 2
  sta SPEED                 ; [58] + 3
  jmp EndSpeed              ; [61] + 3 = 64
  
Inertia
  ; Decrease Speed by Inertia
  ldy SPEED
  beq EndSpeed
  lda InertiaTable,Y
  sta SPEED
EndSpeed                    ; WORST CASE = 64 CYCLES

JoyFire
  ; Check If Already Firing
  lda LASERY                ; [0] + 3
  bne LaserFire             ; [3] + 2/3
  ; Skip If Animating
  lda LIVES                 ; [5] + 3
  and #%11110000            ; [8] + 2
  bne ResetFireCount        ; [10] + 2/3
  ; Skip If Paused
  bit WAVE                  ; [12] + 3
  bmi ResetFireCount        ; [15] + 2/3
  ; Read Fire Button
  lda INPT4                 ; [17] + 3
  bmi ResetFireCount        ; [20] + 2/3
  ; Reset Debounce If Not In Hyperspace
  lda #0                    ; [22] + 2
  bit TIME                  ; [24] + 3
  bpl ResetWarp             ; [27] + 2/3
  lda COUNTER               ; [29] + 3
  and #%00011111            ; [32] + 2
ResetWarp
  sta TEMP                  ; [34] + 3
SkipResetDebounce
  ; Check P0 Difficulty Switch
  bit SWCHB                 ; [37] + 3
  bvc SkipLaserCounter      ; [40] + 2/3
  ; Check Fire Counter
  lda COUNTER               ; [42] + 3
  and #%11100000            ; [45] + 2
  cmp #MAXSHOTS<<5          ; [47] + 2
  bcs EndFire               ; [49] + 2/3
  ; Increment Fire Counter
  ; clc
  adc #32                   ; [51] + 2
  ora TEMP                  ; [53] + 3
SkipLaserCounter
  sta COUNTER               ; [56] + 3
  ; Play Sound Effect
  lda SND0                  ; [59] + 3
  beq PlayLaserSound        ; [62] + 2/3
  and #%11100000            ; [64] + 2
  cmp #6<<5                 ; [66] + 2
  bcc EndLaserSound         ; [68] + 2/3
PlayLaserSound
  lda #((6<<5)|17)          ; [70] + 2
  sta SND0                  ; [72] + 3
EndLaserSound
  ; Set Initial Laser Position
  clc                       ; [75] + 2
  lda PLAYERX               ; [77] + 3
  adc #4                    ; [80] + 2
  sta LASERX                ; [82] + 3
  lda #SHIPH/2              ; [85] + 2    2LK
  bne StoreFire             ; [87] + 3
LaserFire
  ; Check If LaserY Is At Max
  cmp #(GRIDH/2)-2          ; [6] + 2     2LK
  bcc Decreasing            ; [8] + 2/3
  ; Reset Position
  lda #0                    ; [10] + 2
  beq StoreFire             ; [12] + 3
Decreasing
  ; Check P0 Difficulty Switch
  bit SWCHB                 ; [11] + 3
  bvc SlowLaserSpd          ; [14] + 2/3
  adc #LASERFAST            ; [16] + 2
SlowLaserSpd
  adc #LASERSLOW            ; [18] + 2
StoreFire
  sta LASERY                ; [90] + 3
  ; Check If Fire Still Pressed
  lda INPT4                 ; [93] + 3
  bpl EndFire               ; [96] + 2/3
ResetFireCount
  lda COUNTER               ; [98] + 3
  and #%00011111            ; [101] + 2
  sta COUNTER               ; [103] + 3
EndFire                     ; WORST CASE = 106 CYCLES (1.4 SCANLINES)

; -----------------------------------------------------------------------------
; PART 4 - SOUND & SPEECH EFFECTS
; -----------------------------------------------------------------------------

  ; Play Sound Effects
  lax SND0                  ; [0] + 3
  beq EndSounds0            ; [3] + 2/3
  and #%00011111            ; [5] + 2
  tay                       ; [7] + 2
  dey                       ; [9] + 2
  bpl PlaySound0            ; [11] + 2/3
  lda #0                    ; [13] + 2
  sta SND0                  ; [15] + 3
  beq EndSounds0            ; [18] + 3
PlaySound0
  sty TEMP                  ; [14] + 3
  txa                       ; [17] + 2
  and #%11100000            ; [19] + 2
  ora TEMP                  ; [21] + 3
  sta SND0                  ; [24] + 3
  lsr                       ; [27] + 2
  lsr                       ; [29] + 2
  lsr                       ; [31] + 2
  lsr                       ; [33] + 2
  lsr                       ; [35] + 2
  tax                       ; [37] + 2
  lda SoundTab,X            ; [39] + 4
  sta JPTR                  ; [43] + 3
  lda #>SoundTab            ; [46] + 2
  sta JPTR+1                ; [48] + 3
  lda (JPTR),Y              ; [51] + 5
  sta AUDF0                 ; [56] + 3
  lda SoundType,X           ; [59] + 4
  sta AUDC0                 ; [63] + 3
  lda SoundVol,X            ; [66] + 4
EndSounds0
  sta AUDV0                 ; [70] + 3 
                            ; WORST CASE = 73 CYCLES (1 SCANLINE)

  ; Play Speech Effects
  lax SPEECH                ; [0] + 3
  beq EndSpeech0            ; [3] + 2/3
  and #%00111111            ; [5] + 2
  tay                       ; [7] + 2
  ; Check If AtariVox Is Ready
  lda SWCHA                 ; [9] + 3
  and #%00000010            ; [12] + 2
  beq EndSpeech0            ; [14] + 2/3
  ; Update Speech Counter
  dey                       ; [16] + 2
  bpl PlaySpeech0           ; [18] + 2/3
  lda #0                    ; [20] + 2
  sta SPEECH                ; [22] + 3
  beq EndSpeech0            ; [25] + 3
PlaySpeech0
  txa                       ; [28] + 2
  jmp GameSpeech            ; [30] + 3
EndSpeech0                  ; WORST CASE = 9 SCANLINES

; -----------------------------------------------------------------------------
; PART 5 - END MAIN LOOP (CALL DISPLAY KERNEL & GAME LOGIC)
; -----------------------------------------------------------------------------

  ; Flash Ship If Warping (but not in hyperspace)
  bit TIME                  ; [0] + 3
  bmi NoPlayerFlash         ; [3] + 2/3
  lda LIVES                 ; [5] + 3
  and #%11110000            ; [8] + 2
  bne NoPlayerFlash         ; [10] + 2/3
  lda COUNTER               ; [12] + 3
  and #%00011111            ; [15] + 2
  beq NoPlayerFlash         ; [17] + 2/3
  lda CYCLE                 ; [19] + 3
  lsr                       ; [22] + 2
  lsr                       ; [24] + 2
  lsr                       ; [26] + 2
  bcc NoPlayerFlash         ; [28] + 2/3
HideShip
  lda #<A_DarkCol           ; [30] + 2
  sta CPTR                  ; [32] + 3
NoPlayerFlash               ; WORST CASE = 35 CYCLES

  ; Clear Sprite Registers
  lda #0                    ; [0] + 2
  sta NUSIZ0                ; [2] + 3
  sta NUSIZ1                ; [5] + 3
  sta VDELP0                ; [8] + 3
  sta VDELP1                ; [11] + 3
                            ; RUNTIME = 14 CYCLES

  ; Alternate Kernels On Each Wave
ShowKernel
  lda WAVE
  lsr
  bcs SecondKernel
FirstKernel
  jmp ShowKernel1
SecondKernel
  jmp ShowKernel2

; -----------------------------------------------------------------------------
; PART 6 - ATARIVOX SPEECH PLAYER
; -----------------------------------------------------------------------------

  ; Play Speech (SPTR Contains Start Of Speech Data)
PlaySpeech
  ; Reset Position Counter
  lda #0
  sta POSITION

  ; Set Initial Timeout Counter
  lda #VOXTIME
  sta TIME

SpeechLoop
  ; Fetch Next Speech Byte
  ldy POSITION
  lda (SPTR),Y
  
  ; Invert Data & Check For End Marker
  eor #$FF
  beq EndSpeech
  sta TEMP
  
  ; Check Ready Timeout
  lda TIME
  beq EndSpeech
  
  ; Start New Frame
  WAIT_VBLANK
  START_SCREEN
  
  ; Check AtariVox Ready
  lda SWCHA
  and #%00000010
  beq NotReady

WriteSpeechByte
  ; Reset Timeout
  lda #VOXTIME
  sta TIME
  
  ; Output 10 bits (1 Start + 8 Data + 1 Stop)
  ldy #0
  sec
ByteOutLoop
  ; Copy Carry Into Bit 0 Of SWACNT
  lda SWACNT
  and #$FE
  adc #0
  sta SWACNT
  
  ; Check If Finished
  cpy #9
  beq FinishedSpeechByte
  iny
  
  ; Waste Cycles
  SLEEP 36
  
  ; Put Next Byte Into Carry
  lsr TEMP
  jmp ByteOutLoop

NotReady
  dec TIME
FinishedSpeechByte

  ; Skip To Next Frame
  WAIT_SCREEN
  START_OVERSCAN
  WAIT_OVERSCAN
  START_VBLANK
  
  ; Increment Position Counter
  inc POSITION
  jmp SpeechLoop
EndSpeech
  rts

  ; Juno First
JunoFirstSpeech
  DC.B  20, 127, 21, 7, 22, 88, 23, 5
  DC.B  21, 90, 23, 5, 20, 127, 165, 22
  DC.B  170, 139, 21, 90, 142, 21, 110, 22
  DC.B  90, 137, 21, 127, 22, 110, 137, 22
  DC.B  120, 137, 22, 130, 137, 22, 140, 137
  DC.B  22, 150, 137, 22, 155, 137, 22, 162
  DC.B  164, 0, 21, 90, 186, 21, 127, 22
  DC.B  200, 148, 22, 180, 148, 22, 160, 148
  DC.B  22, 140, 148, 22, 100, 148, 21, 100, 188, 191, 2, $FF
EndJunoFirstSpeech
  if (>JunoFirstSpeech != >EndJunoFirstSpeech)
    echo "WARNING: Juno First Speech Crosses Page Boundary!"
  endif
  
  DC.B  "SPACE CADET"

; -----------------------------------------------------------------------------
; PART 7 - GAME DATA
; -----------------------------------------------------------------------------

  ALIGN   256

  ; This table is used to calculate the flickersort spacings:
  ; For X < 128 the calculation is X-10 (minimum 0)
  ; For X > 127 the calculation is X&%11110000
SpacingTable
  DC.B    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  2,  3,  4,  5
  DC.B    6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21
  DC.B   22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37
  DC.B   38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53
  DC.B   54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69
  DC.B   70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85
  DC.B   86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99,100,101
  DC.B  102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117
  DC.B  128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128
  DC.B  144,144,144,144,144,144,144,144,144,144,144,144,144,144,144,144
  DC.B  160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160
  DC.B  176,176,176,176,176,176,176,176,176,176,176,176,176,176,176,176
  DC.B  192,192,192,192,192,192,192,192,192,192,192,192,192,192,192,192
  DC.B  208,208,208,208,208,208,208,208,208,208,208,208,208,208,208,208
  DC.B  224,224,224,224,224,224,224,224,224,224,224,224,224,224,224,224
  DC.B  240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240
  
  ALIGN   256

  ; This table is used to slow the ship down when no thrust is applied.
  ; The values were computed by: New = Old-A-Old*B (A=1/4, B=1/96)
InertiaTable
  DC.B    0,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14
  DC.B   15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30
  DC.B   31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46
  DC.B   47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62
  DC.B   63, 64, 65, 66, 67, 68, 69, 70, 71, 71, 72, 73, 74, 75, 76, 77
  DC.B   78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93
  DC.B   94, 95, 96, 97, 98, 99,100,101,102,103,104,105,106,107,108,109
  DC.B  110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125
  DC.B  130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145
  DC.B  146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161
  DC.B  162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177
  DC.B  178,179,180,181,182,183,184,185,185,186,187,188,189,190,191,192
  DC.B  193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208
  DC.B  209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224
  DC.B  225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240
  DC.B  241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,255

  ALIGN   256
  
  ; Sound Effects (Lower = Higher Priority)
  ; 0 - New Wave
  ; 1 - Player Explosion
  ; 2 - Player Appear
  ; 3 - Time Out
  ; 4 - Alien Explosion
  ; 5 - Alien Appear
  ; 6 - Laser Fire
  ; 7 - Bullet Fire
  
SoundTab
  DC.B  <NewWaveSnd, <PlayerExplodeSnd, <PlayerAppearSnd, <TimeSnd
  DC.B  <ExplodeSnd, <WarpSnd, <LaserSnd, <BulletSnd
SoundType
  DC.B  12, 8, 8, 4, 8, 8, 8, 7
SoundVol
  DC.B  8, 8, 8, 7, 8, 8, 8, 6
; SoundLen
; DC.B  31, 31, 31, 31, 11, 17, 17, 7
NewWaveSnd
  DC.B  9, 9, 9, 14, 14, 14, 14, 21
  DC.B  21, 21, 21, 16, 16, 16, 16, 24
  DC.B  24, 24, 24, 18, 18, 18, 18, 27
  DC.B  27, 27, 20, 20, 20, 31, 31, 31
PlayerExplodeSnd
  DC.B  20, 20, 18, 18, 16, 16, 14, 14
  DC.B  12, 12, 10, 10, 8, 8, 6, 6
  DC.B  4, 4, 2, 2, 1, 1, 0, 0
  DC.B  1, 1, 2, 2, 3, 3, 4, 4
PlayerAppearSnd
  DC.B  4, 4, 3, 3, 2, 2, 1, 1
  DC.B  0, 0, 1, 1, 2, 2, 4, 4
  DC.B  6, 6, 8, 8, 10, 10, 12, 12
  DC.B  14, 14, 16, 16, 18, 18, 20, 20
ExplodeSnd
  DC.B  1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3
TimeSnd
  DC.B  15, 15, 15, 15, 15, 15, 15, 15
  DC.B  0, 0, 0, 0, 0, 0, 0, 0
  DC.B  0, 0, 0, 0, 0, 0, 0, 0
  DC.B  15, 15, 15, 15, 15, 15, 15, 15
WarpSnd
  DC.B  6, 7, 8, 8, 9, 9, 10, 10
  DC.B  11, 11, 12, 12, 13, 13, 14, 14
  DC.B  14, 14
LaserSnd
  DC.B  18, 17, 16, 15, 14, 13, 12, 11
  DC.B  10, 9, 8, 7, 6, 5, 4, 3, 2, 1
BulletSnd
  DC.B  30, 28, 26, 24, 22, 20, 18, 16
ThrobSnd
  DC.B  28, 28, 28, 28, 28, 28, 28, 28
  DC.B  26, 26, 26, 26, 26, 26, 26, 26
  DC.B  24, 24, 24, 24, 24, 24, 24, 24
  DC.B  26, 26, 26, 26, 26, 26, 26, 26
HyperSnd
  DC.B  2, 2, 1, 1, 2, 2, 3, 3
EndSnd
  if (>SoundTab != >EndSnd)
    echo "WARNING: Sound Effects Table Crosses Page Boundary!"
  endif

ExplosionOffset
  DC.B  0, 40, 80, 120, 160, 200, 240
EndExplosionOffset
  if (>ExplosionOffset != >EndExplosionOffset)
    echo "WARNING: Explosion Offset Table Crosses Page Boundary!"
  endif

  ALIGN   256
  
  ; AtariVox Speech Data (Part 1)
JunoSpeech1

  ; Insult Pointers
InsultTabHi
  DC.B  >Insult0, >Insult1, >Insult2, >Insult3
  DC.B  >Insult4, >Insult5, >Insult6, >Insult7
InsultTabLo
  DC.B  <Insult0, <Insult1, <Insult2, <Insult3
  DC.B  <Insult4, <Insult5, <Insult6, <Insult7

  ; You Have Failed
Insult0
  DC.B  20, 127, 21, 119, 22, 227, 23, 5
  DC.B  21, 100, 22, 150, 20, 95, 23, 5
  DC.B  160, 183, 132, 166, 1, 20, 127, 21
  DC.B  1, 186, 21, 100, 14, 21, 100, 22
  DC.B  210, 130, 21, 127, 22, 200, 130, 22
  DC.B  180, 130, 22, 160, 130, 22, 140, 130
  DC.B  22, 120, 130, 22, 95, 154, 21, 100
  DC.B  145, 176, $FF

  ; Foolish Human
Insult1
  DC.B  20, 127, 21, 119, 22, 227, 23, 5
  DC.B  21, 110, 22, 227, 20, 127, 23, 5
  DC.B  186, 139, 145, 22, 110, 129, 189
  DC.B  0, 184, 160, 140, 134, 21, 127, 22
  DC.B  120, 134, 22, 140, 134, 22, 160, 134
  DC.B  22, 180, 134, 22, 200, 134, 21, 110
  DC.B  141, $FF

  ; Useless Mammal
Insult2
  DC.B  20, 52, 21, 114, 22, 88, 23, 5
  DC.B  21, 100, 22, 68, 20, 90, 23, 5
  DC.B  14, 160, 187, 15, 22, 65, 145, 131
  DC.B  187, 187, 30, 30, 21, 40, 140, 21
  DC.B  120, 8, 14, 132, 140, 7, 15, 134
  DC.B  21, 100, 145, $FF

  ; Ha Ha
Insult3
  DC.B  20, 100, 21, 120, 22, 78, 23, 15
  DC.B  23, 15, 21, 120, 20, 100, 22, 180
  DC.B  147, 132, 2, 22, 170, 147, 132, 2
  DC.B  22, 160, 147, 132, 2, 22, 150, 147
  DC.B  21, 10, 132, $FF

EndJunoSpeech1
  if (>JunoSpeech1 != >EndJunoSpeech1)
    echo "WARNING: Speech Table 1 Crosses Page Boundary!"
  endif

  ALIGN   256

  ; AtariVox Speech Data (Part 2)
JunoSpeech2

  ; You're A Dum Dum
Insult4
  DC.B  20, 96, 21, 114, 22, 88, 23, 5
  DC.B  21, 127, 22, 150, 23, 5, 20, 70
  DC.B  128, 21, 100, 153, 7, 22, 140, 134
  DC.B  21, 115, 20, 127, 22, 200, 175, 21
  DC.B  100, 14, 134, 140, 21, 115, 20, 115
  DC.B  22, 100, 175, 22, 110, 134, 22, 100
  DC.B  134, 15, 22, 80, 134, 21, 100, 140, $FF

  ; You Got Owned
Insult5
  DC.B  20, 127, 21, 114, 22, 151, 23, 5
  DC.B  20, 70, 23, 5, 21, 100, 22, 200
  DC.B  160, 30, 50, 22, 140, 179, 136, 191
  DC.B  30, 50, 20, 127, 21, 127, 22, 100
  DC.B  137, 22, 90, 137, 22, 80, 137, 22
  DC.B  70, 137, 22, 60, 137, 21, 100, 141, 175, $FF

  ; Pathetic Earthling
Insult6
  DC.B  20, 127, 21, 100, 22, 100, 23, 7
  DC.B  199, 21, 60, 134, 1, 22, 75, 21
  DC.B  120, 190, 14, 131, 22, 50, 20, 100
  DC.B  191, 15, 129, 194, 2, 20, 80, 21
  DC.B  127, 151, 190, 190, 2, 22, 60, 145
  DC.B  129, 22, 70, 129, 143, 178, $FF

  ; You Lose
Insult7
  DC.B  20, 90, 21, 110, 22, 160, 23, 7
  DC.B  160, 21, 100, 20, 127, 22, 210, 146
  DC.B  22, 210, 139, 21, 127, 22, 230, 139
  DC.B  22, 220, 139, 22, 200, 139, 22, 180
  DC.B  139, 22, 140, 139, 22, 100, 139, 167
  DC.B  22, 80, 167, 188, 188, 188, $FF

EndJunoSpeech2
  if (>JunoSpeech2 != >EndJunoSpeech2)
    echo "WARNING: Speech Table 2 Crosses Page Boundary!"
  endif

  ALIGN   256

  ; AtariVox Speech Data (Part 3)
JunoSpeech3

  ; Your Truck Of Fail Has Arrived
Insult8
  DC.B  20, 96, 21, 127, 22, 98, 23, 5
  DC.B  20, 96, 21, 114, 22, 88, 23, 5
  DC.B  128, 153, 191, 148, 21, 127, 22, 88
  DC.B  134, 22, 95, 134, 22, 105, 134, 22
  DC.B  115, 134, 22, 125, 134, 6, 21, 114
  DC.B  196, 30, 50, 8, 134, 166, 30, 10
  DC.B  22, 100, 186, 154, 145, 30, 50, 183
  DC.B  132, 167, 22, 95, 134, 148, 22, 75
  DC.B  21, 100, 157, 166, 174, $FF

  ; You're Despicable
Insult9
  DC.B  20, 96, 21, 114, 22, 88, 23, 5
  DC.B  20, 73, 23, 5, 21, 100, 22, 240
  DC.B  128, 20, 127, 21, 127, 22, 250, 137
  DC.B  22, 245, 137, 22, 240, 137, 22, 235
  DC.B  137, 22, 230, 153, 21, 75, 20, 73
  DC.B  22, 225, 174, 131, 187, 187, 187, 187
  DC.B  21, 114, 23, 5, 20, 127, 22, 100
  DC.B  198, 129, 194, 22, 50, 134, 22, 25
  DC.B  171, 145, $FF

EndJunoSpeech3
  if (>JunoSpeech3 != >EndJunoSpeech3)
    echo "WARNING: Speech Table 3 Crosses Page Boundary!"
  endif
  
GetReadyText
G__G
  DC.B  %00000110
  DC.B  %00001010
  DC.B  %00001010
  DC.B  %00001000
  DC.B  %00000110
G_ET
  DC.B  %11100100
  DC.B  %10000100
  DC.B  %11000100
  DC.B  %10000100
  DC.B  %11101110
G__R
  DC.B  %00001010
  DC.B  %00001010
  DC.B  %00001100
  DC.B  %00001010
  DC.B  %00001100
G_EA
  DC.B  %11101010
  DC.B  %10001010
  DC.B  %11001110
  DC.B  %10001010
  DC.B  %11100100
G_DY
  DC.B  %11000100
  DC.B  %10100100
  DC.B  %10100100
  DC.B  %10101010
  DC.B  %11001010
G_BANG_
  DC.B  %01000000
  DC.B  %00000000
  DC.B  %01000000
  DC.B  %01000000
  DC.B  %01000000
EndGetReadyText
  if (>GetReadyText != >EndGetReadyText)
    echo "WARNING: Get Ready Text Crosses Page Boundary!"
  endif

  ; PlusROM API
PlusAPI:
  DC.B  "a", 0, "h.firmaplus.de", 0

  echo "----",($FFF0 - *) , "bytes left (BANK 1 - MAIN GAME)"
    
  ORG     $8FF0
  RORG    $FFF0
  DC.B    0, 0, 0, 255
  DC.B    0, 0, 0, 0, 0, 0
  DC.W    (PlusAPI - $E000)
  DC.W    Init1, Init1

