; -----------------------------------------------------------------------------
; CHETIRY - Atari 2600 - (C) Copyright 2011 - AtariAge
; BANK 4 - SHUTTLE B
; -----------------------------------------------------------------------------

; Shuttle Constants
SHUTTLEH      = 98
SOYUZH        = 86
ROCKETH       = 43
SCREENH       = 160

; Shuttle Colours
BGCOL         = $0E
FGCOL         = $00

  SEG     BANK4
  ORG     $C000
  RORG    $F000

  ; Space for DPC registers
  DS.B    256,0

; -------------------------- SHIP (WITHOUT GAP) -------------------------------

 ; Display Ship
BuranLoop1
  ldy SLINE               ; [62] + 3
  ldx #0                  ; [65] + 2
  stx GRP0                ; [67] + 3
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  lda (SPF),Y             ; [5] + 5
  sta.w PF2               ; [10] + 4
  lda (SDATA1),Y          ; [14] + 5
  sta GRP1                ; [19] + 3
  lda (SDATA2),Y          ; [22] + 5
  sta GRP0                ; [27] + 3
  lax (SDATA4),Y          ; [30] + 5
  lda (SDATA3),Y          ; [35] + 5
  ldy #0                  ; [40] + 2
  sta GRP1                ; [42] + 3    >= 44 < 47
  stx GRP0                ; [45] + 3    > 46 < 50
  sty GRP1                ; [48] + 3    > 49 <= 52
  sta GRP0                ; [51] + 3    >= 52 < 55
  dec SLINE               ; [54] + 5
  bpl BuranLoop1          ; [59] + 2/3

; -------------------------- FLAME --------------------------------------------

  ; Display Flame
FlameLoop1
  ldy FLINE               ; [61] + 3
  ldx #0                  ; [64] + 2
  stx GRP0                ; [66] + 3
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  lda (FPF),Y             ; [5] + 5
  sta PF2                 ; [10] + 3
  lda (FDATA1),Y          ; [13] + 5
  sta GRP1                ; [18] + 3
  lda (FDATA2),Y          ; [21] + 5
  sta GRP0                ; [26] + 3
  lax (FDATA4),Y          ; [29] + 5
  lda (FDATA3),Y          ; [34] + 5
  ldy #0                  ; [39] + 2
  sta GRP1                ; [41] + 3    >= 44 < 47
  stx GRP0                ; [44] + 3    > 46 < 50
  sty GRP1                ; [47] + 3    > 49 <= 52
  sta GRP0                ; [50] + 3    >= 52 < 55
  dec FLINE               ; [53] + 5
  bpl FlameLoop1          ; [58] + 2/3
  
SwitchPoint
  ldx #0                  ; [60] + 2
  stx PF2                 ; [62] + 3

  ; Switch To TakeOff Kernel (Bank 3)
  ORG     $C159
  RORG    $F159
  lda     $FFF7           ; [65] + 4
  ORG     $C15C
  RORG    $F15C

; -------------------------- POSITION -----------------------------------------

PositionBuran
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  
  ; Set 3 Copies Close and Delay
  ldx #%00000011          ; [5] + 2
  stx VDELP0              ; [7] + 3
  stx VDELP1              ; [10] + 3
  stx NUSIZ0              ; [13] + 3
  stx NUSIZ1              ; [16] + 3
  ; Position Sprites (Fine)
  stx HMCLR               ; [19] + 3
  ldx #%11100000          ; [22] + 2    + 2
  stx HMP0                ; [24] + 3
  ldx #%11110000          ; [27] + 2    + 1
  stx HMP1                ; [29] + 3
  ; Position Sprites (Coarse)
  ldx #FGCOL              ; [32] + 2
  nop                     ; [34] + 2
  sta RESP0               ; [36] + 3    = 39
  sta RESP1               ; [39] + 3    = 42
  ; Sprite Colours
  stx COLUP1              ; [42] + 3
  stx COLUP0              ; [45] + 3
  stx COLUPF              ; [48] + 3
  stx COLUBK              ; [51] + 3
  ; Reflect Playfield
  ldx #%00000001          ; [54] + 2
  stx CTRLPF              ; [56] + 3
  ; Border
  ldx #%11111100          ; [59] + 2
  stx PF0                 ; [61] + 3
  stx PF1                 ; [64] + 3
  ldx #0                  ; [67] + 2
  stx PF2                 ; [69] + 3
  ; Reposition Sprites
  sta HMOVE               ; [72] + 3    > 74 < 4

  ; Start Frame
  START_FRAME_MUSIC

  ; Skip 8 Lines
  ldx #8
BuranSkipLoop
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  SLEEP 30                ; [5] + 30
  dex                     ; [35] + 2
  bpl BuranSkipLoop       ; [37] + 2/3
  
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; Set Backgound Colour
  ldx #BGCOL              ; [5] + 2
  stx COLUBK              ; [7] + 3

  ; Begin Buran Drawing
  SLEEP 16                ; [10] + 16
  jmp BeginBuran          ; [26] + 3

  DC.B  "SPUTNIK"
  
  CHECKPAGE PositionBuran, "PositionBuran"
  ALIGN 256

; -------------------------- SHIP (WITH GAP) ----------------------------------
  
  ; Display Buran
BuranLoop2
  ldy SLINE               ; [62] + 3
  ldx #0                  ; [65] + 2
  stx GRP0                ; [67] + 3
  stx WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  lda (SPF),Y             ; [5] + 5
  sta PF2                 ; [10] + 3
  lda (SDATA1),Y          ; [13] + 5
  sta GRP1                ; [18] + 3
  lda (SDATA2),Y          ; [21] + 5
  sta GRP0                ; [26] + 3
  lax (SDATA4),Y          ; [29] + 5
  lda (SDATA3),Y          ; [34] + 5
  ldy #0                  ; [39] + 2
  sta.w GRP1              ; [41] + 4    >= 44 < 47
  stx GRP0                ; [45] + 3    > 46 < 50
  sty GRP1                ; [48] + 3    > 49 <= 52
  sta GRP0                ; [51] + 3    >= 52 < 55
  dec SLINE               ; [54] + 5
  bpl BuranLoop2          ; [59] + 2/3
  
; -------------------------- FLAME --------------------------------------------

  ; Display Flame
FlameLoop2
  ldy FLINE               ; [61] + 3
  ldx #0                  ; [64] + 2
  stx GRP0                ; [66] + 3
  stx WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  lda (FPF),Y             ; [5] + 5
  sta PF2                 ; [10] + 3
  lda (FDATA1),Y          ; [13] + 5
  sta GRP1                ; [18] + 3
  lda (FDATA2),Y          ; [21] + 5
  sta GRP0                ; [26] + 3
  lax (FDATA4),Y          ; [29] + 5
  lda (FDATA3),Y          ; [34] + 5
  ldy #0                  ; [39] + 2
  sta GRP1                ; [41] + 3    >= 44 < 47
  stx GRP0                ; [44] + 3    > 46 < 50
  sty GRP1                ; [47] + 3    > 49 <= 52
  sta GRP0                ; [50] + 3    >= 52 < 55
  dec FLINE               ; [53] + 5
  bpl FlameLoop2          ; [58] + 2/3
  sty GRP1                ; [60] + 3
  sty GRP0                ; [63] + 3
  sty GRP1                ; [66] + 3

; -------------------------- GAP ----------------------------------------------

  ; Gap Under Flame
  ldy GAPBOT              ; [69] + 3
GapLoop
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  lda (GPF),Y             ; [5] + 5
  sta PF2                 ; [10] + 3
  SLEEP 40                ; [13] + 40
  dey                     ; [53] + 2
  bpl GapLoop             ; [55] + 2/3
  jmp SwitchPoint         ; [57] + 3

; -----------------------------------------------------------------------------
; MAIN SHIP CODE
; -----------------------------------------------------------------------------

ShipMainLoop
  START_VBLANK_MUSIC      ; [11]

  ; Update Music Position
  dec MUSIC               ; [11] + 5
  bpl SkipSpecialTune     ; [16] + 2/3
  sta TUNESTEP_W          ; [18] + 4
  nop                     ; [22] + 2
  ldx #MUSICTEMPO         ; [24] + 2
  stx MUSIC               ; [26] + 3
SkipSpecialTune

  ; Play Rocket SFX
  ldx SFX                 ; [29] + 3
  beq SkipSFX             ; [32] + 2/3
  stx AUDF1               ; [34] + 3
  ldx #8                  ; [37] + 2
  stx AUDC1               ; [39] + 3
  ldx #3                  ; [42] + 2
SkipSFX
  stx AUDV1               ; [44] + 3

  ; Update Frame and Phase
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  dec CYCLE               ; [5] + 5
  lda CYCLE               ; [10] + 3
  lsr                     ; [13] + 2
  bcs EndFrameUpdate      ; [15] + 2/3
  lsr                     ; [17] + 2
  bcc EndFrameUpdate      ; [19] + 2/3
  lsr                     ; [25] + 2
  bcc EndFrameUpdate      ; [27] + 2/3
  dec FRAME               ; [29] + 5
  bpl EndFrameUpdate      ; [34] + 2/3
  dec PHASE               ; [36] + 5
  bpl FrameUpdate         ; [41] + 2/3
  
  ; Finish Music
  ldx #0
  stx AUDV0
  stx AUDV1
  stx MUSIC
  stx SFX
  stx TUNERESET_W

  ; End Kernel
  jmp QuitSpecial

  ; Update Frame
FrameUpdate
  lda SHIP                ; [44] + 3
  asl                     ; [47] + 2
  asl                     ; [49] + 2
  adc PHASE               ; [51] + 3
  tax                     ; [54] + 2
  lda PhaseFrames,X       ; [56] + 4
  sta FRAME               ; [60] + 3
EndFrameUpdate

  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; No Bottom Gap (Default)
  ldx #$FF                ; [5] + 2
  stx GAPBOT              ; [7] + 3

  ; Background High Pointers
  ldx #>Background        ; [10] + 2
  stx SPF+1               ; [12] + 3
  stx FPF+1               ; [15] + 3
  stx GPF+1               ; [18] + 3

  ; Phase Actions
  lda PHASE               ; [21] + 3
  cmp #4                  ; [24] + 2
  bcs DoShipPhase4        ; [26] + 2/3
  cmp #3                  ; [28] + 2
  bcs DoShipPhase3        ; [30] + 2/3
  cmp #2                  ; [32] + 2
  bcs DoShipPhase2A       ; [34] + 2/3
  cmp #1                  ; [36] + 2
  bcs DoShipPhase1A       ; [38] + 2/3
  jmp DoShipPhase0        ; [40] + 3
DoShipPhase1A
  jmp DoShipPhase1        ; [41] + 3
DoShipPhase2A
  jmp DoShipPhase2        ; [37] + 3

; -----------------------------------------------------------------------------
; PHASE 4 - START
; -----------------------------------------------------------------------------

DoShipPhase4
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; No Sound Effect
  ldx #0                  ; [5] + 2
  stx SFX                 ; [7] + 3

  ; Top Gap
  sec                     ; [10] + 2
  lda #SCREENH            ; [12] + 2
  ldx SHIP                ; [14] + 3
  sbc ShipHeight,X        ; [17] + 4
  sta GAPTOP              ; [21] + 3

  ; Ship Top
  sec                     ; [24] + 2
  lda ShipHeight,X        ; [26] + 4
  sbc #20                 ; [30] + 2
  sta SLINE               ; [32] + 3
  ldy #20                 ; [35] + 2
  jsr LoadShip            ; [37] + 6    = 43

  ; Ship Bottom
  ldx #10                 ; [35] + 2
  stx FLINE               ; [37] + 3
  ldy #9                  ; [40] + 2
  ldx SHIP                ; [42] + 3
  jsr LoadFlameData       ; [45] + 6    = 51

  ; Background
  ldx #<Background+20     ; [35] + 2
  stx SPF                 ; [37] + 3
  ldx #<Background+9      ; [40] + 2
  stx FPF                 ; [42] + 3

  ; Base Length
  ldx #8                  ; [45] + 2
  stx TLINE               ; [47] + 3

  ; Ship Base Type
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  ldx SHIP                ; [5] + 3
  bne ShowShip4           ; [8] + 2/3
ShowBuran4
  ldx #20                 ; [10] + 2    Buran Base Frame
  jsr LoadTakeOff         ; [12] + 6
  jmp EndPhaseUpdate      ; [32] + 3    = 35
ShowShip4
  cpx #2                  ; [11] + 2
  bcs ShowRocket4         ; [13] + 2/3
ShowSoyuz4
  ldx #36                 ; [24] + 2    Soyuz Base Frame
  bne LoadLiftOff4        ; [22] + 2/3
ShowRocket4
  ldx #<Background+60     ; [15] + 2    Rocket Background
  stx SPF                 ; [17] + 3
  ldx #0                  ; [20] + 2    Rocket Base Frame
LoadLiftOff4
  jsr LoadLiftOff         ; [26] + 6
  jmp EndPhaseUpdate      ; [25] + 3    = 28

; -----------------------------------------------------------------------------
; PHASE 3 - SMOKE
; -----------------------------------------------------------------------------

DoShipPhase3
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; Rocket Ignition
  ldx #31                 ; [5] + 2
  stx SFX                 ; [7] + 3

  ; Top Gap
  sec                     ; [10] + 2
  lda #SCREENH            ; [12] + 2
  ldx SHIP                ; [14] + 3
  sbc ShipHeight,X        ; [17] + 4
  sta GAPTOP              ; [21] + 3
  
  ; Ship
  sec                     ; [24] + 2
  lda ShipHeight,X        ; [26] + 4
  sbc #31                 ; [30] + 2
  sta SLINE               ; [32] + 3
  ldy #31                 ; [35] + 2
  jsr LoadShip            ; [37] + 6    = 43
  
  ; Ship Flame
  ldx #21                 ; [35] + 2
  stx FLINE               ; [37] + 3
  ldx SHIP                ; [40] + 3
  ldy SmokeHeight,X       ; [43] + 4
  sty TLINE               ; [47] + 3    TakeOff Height = Smoke Height -1
  iny                     ; [50] + 2
  jsr LoadFlameData       ; [52] + 6    = 58

  ; Smoke
  ldy FRAME               ; [35] + 3
  ldx SHIP                ; [38] + 3
  bne ShowShip3           ; [41] + 2/3
  
ShowBuran3
  ; Buran Flame Height
  ldx #9                  ; [43] + 2
  stx FLINE               ; [45] + 3

  ; Buran Smoke
  ldx BuranSmokeSeq,Y     ; [48] + 4
  ldy #>Smoke1_0          ; [52] + 2
  jsr LoadSmoke           ; [54] + 6    = 60
  jmp Phase2BG            ; [32] + 3    = 35

ShowShip3
  cpx #2                  ; [44] + 2
  bcs ShowRocket3         ; [46] + 2/3

ShowSoyuz3
  ; Soyuz Smoke
  ldx SoyuzSmokeSeq,Y     ; [48] + 4
  ldy #>RocketSmoke1_0    ; [52] + 2
  jsr LoadSmoke           ; [54] + 6    = 60

  ; Soyuz Background
  ldx #<Background+31     ; [32] + 2
  ldy #<Background+19     ; [34] + 2
  jmp Phase2BGStore       ; [36] + 3    = 39

ShowRocket3
  ; Rocket Smoke
  ldx RocketSmokeSeq,Y    ; [49] + 4
  ldy #>RocketSmoke1_0    ; [53] + 2
  jsr LoadSmoke           ; [55] + 6    = 61

  ; Rocket Background
  ldx #<Background+71     ; [32] + 2
  ldy #<Background+9      ; [34] + 2
  jmp Phase2BGStore       ; [36] + 3    = 39

; -----------------------------------------------------------------------------
; PHASE 2 - TAKEOFF A
; -----------------------------------------------------------------------------

DoShipPhase2
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; Rocket Sound
  ldx #31                 ; [5] + 2
  stx SFX                 ; [7] + 3

  ; Ship Height
  sec                     ; [10] + 2
  ldx SHIP                ; [12] + 3
  lda TakeOffFramesA,X    ; [15] + 4
  sbc FRAME               ; [19] + 3
  sta HEIGHT              ; [22] + 3

  ; Top Gap
  sec                     ; [25] + 2
  lda #SCREENH            ; [27] + 2
  sbc ShipHeight,X        ; [29] + 4
  sec                     ; [33] + 2
  sbc HEIGHT              ; [35] + 3
  sta GAPTOP              ; [38] + 3

  ; Ship
  sec                     ; [41] + 2
  lda ShipHeight,X        ; [43] + 4
  sbc #31                 ; [47] + 2
  clc                     ; [49] + 2
  adc HEIGHT              ; [51] + 3
  sta SLINE               ; [54] + 3
  sec                     ; [57] + 2
  lda #31                 ; [59] + 2
  sbc HEIGHT              ; [61] + 3
  tay                     ; [64] + 2
  jsr LoadShip            ; [66] + 6    = 72

  ; Ship Flame
  ldx #21                 ; [35] + 2
  stx FLINE               ; [37] + 3
  ldx SHIP                ; [40] + 3
  ldy SmokeHeight,X       ; [43] + 4
  sty TLINE               ; [47] + 3    TakeOff Height = Smoke Height -1
  iny                     ; [50] + 2
  tya                     ; [52] + 2
  sec                     ; [54] + 2
  sbc HEIGHT              ; [56] + 3
  tay                     ; [59] + 2
  jsr LoadFlameData       ; [61] + 6    = 67
  
  ; TakeOff Frame
  ldx SHIP                ; [35] + 3
  bne ShowShip2           ; [38] + 2/3
  
ShowBuran2
  ; Buran TakeOff
  ldx FRAME               ; [40] + 3
  jsr LoadTakeOff         ; [43] + 6    = 49
  
  ; Buran Flame
  ldx #9                  ; [32] + 2
  stx FLINE               ; [34] + 3
  
  ; Background
Phase2BG
  ldx #<Background+31     ; [37] + 2
  ldy #<Background+21     ; [39] + 2
Phase2BGStore
  stx SPF                 ; [47] + 3
  sty FPF                 ; [50] + 3
  jmp EndPhaseUpdate      ; [58] + 3    = 61

ShowShip2
  cpx #2                  ; [41] + 2
  bcs ShowRocket2         ; [43] + 2/3

ShowSoyuz2
  ; Soyuz TakeOff
  ; clc
  lda FRAME               ; [45] + 3
  adc #27                 ; [48] + 2
  tax                     ; [50] + 2
  jsr LoadLiftOff         ; [52] + 6    = 58

  ; Soyuz Background
  ldx #<Background+31     ; [25] + 2
  ldy #<Background+19     ; [27] + 2
  jmp Phase2BGStore       ; [29] + 3    = 32

ShowRocket2
  ; Rocket TakeOff
  clc                     ; [46] + 2
  lda FRAME               ; [48] + 3
  adc #18                 ; [51] + 2
  tax                     ; [53] + 2
  jsr LoadLiftOff         ; [55] + 6    = 61

  ; Rocket Background
  ldx #<Background+71     ; [25] + 2
  ldy #<Background+9      ; [27] + 2
  jmp Phase2BGStore       ; [29] + 3    = 32

; -----------------------------------------------------------------------------
; PHASE 1 - TAKEOFF B
; -----------------------------------------------------------------------------

DoShipPhase1
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; Ship Height
  sec                     ; [5] + 2
  ldx SHIP                ; [7] + 3
  lda TakeOffFramesB,X    ; [10] + 4
  sbc FRAME               ; [14] + 3
  sta HEIGHT              ; [17] + 3

  ; Top Gap
  sec                     ; [20] + 2
  lda #SCREENH            ; [22] + 2
  sbc ShipHeight,X        ; [24] + 4
  sec                     ; [28] + 2
  sbc TakeOffFramesA,X    ; [30] + 4    LiftOff Height
  sec                     ; [34] + 2
  sbc HEIGHT              ; [36] + 3    Flame Height
  sta GAPTOP              ; [39] + 3

  ; Ship
  ldy ShipHeight,X        ; [42] + 4
  sty SLINE               ; [46] + 3
  ldy #0                  ; [49] + 2
  jsr LoadShip            ; [51] + 6    = 57

  ; Rocket SFX
  lda HEIGHT              ; [35] + 3
  lsr                     ; [38] + 2
  lsr                     ; [40] + 2
  lsr                     ; [42] + 2
  sta TEMP                ; [44] + 3
  sec                     ; [47] + 2
  lda #31                 ; [49] + 2
  sbc TEMP                ; [51] + 3
  sta SFX                 ; [54] + 3

  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; Ship Type
  ldx SHIP                ; [5] + 3
  bne ShowShip1           ; [8] + 2/3

  ; Flame
  clc                     ; [10] + 2
  lda #9                  ; [12] + 2
  adc HEIGHT              ; [14] + 3
  sta FLINE               ; [17] + 3
  
  lda FRAME               ; [20] + 3
  lsr                     ; [23] + 2
  bcc ShowShortFlameB_Shuttle   ; [25] + 2/3
  
ShowLongFlameB_Shuttle
  ; sec
  lda #20                 ; [27] + 2
  sbc HEIGHT              ; [29] + 3
  tay                     ; [32] + 2
  ldx #3                  ; [34] + 2
  jsr LoadFlameData       ; [36] + 6    = 42
  jmp EndShowFlameB_Shuttle   ; [35] + 3
ShowShortFlameB_Shuttle
  sec                     ; [28] + 2
  lda #10                 ; [30] + 2
  sbc HEIGHT              ; [32] + 3
  tay                     ; [35] + 2
  ldx #4                  ; [37] + 2
  jsr LoadFlameData       ; [39] + 6    = 45
EndShowFlameB_Shuttle
  
  ; Take Off Frame
  lda #9                  ; [38] + 2
  sta TLINE               ; [40] + 3
  clc                     ; [43] + 2
  lda FRAME               ; [45] + 3
  adc #22                 ; [48] + 2
  tax                     ; [50] + 2
  jsr LoadTakeOff         ; [52] + 6    = 58

  ; Background
  clc                     ; [32] + 2
  lda #<Background+20     ; [34] + 2
  sta FPF                 ; [36] + 3
  adc HEIGHT              ; [39] + 3
  sta SPF                 ; [42] + 3
  jmp EndPhaseUpdate      ; [45] + 3    = 48
  
ShowShip1
  ; Flame Height
  ldx HEIGHT              ; [11] + 3
  dex                     ; [14] + 2
  stx FLINE               ; [16] + 3

  ; Flame
  lda FRAME               ; [19] + 3
  lsr                     ; [22] + 2
  bcc ShowShortFlameB_Rocket    ; [24] + 2/3
  ; sec
  lda #23                 ; [26] + 2
  sbc HEIGHT              ; [28] + 3
  tay                     ; [31] + 2
  ldx #5                  ; [33] + 2
  jsr LoadFlameData       ; [35] + 6    = 41
  jmp EndShowFlameB_Rocket  ; [35] + 3
  
ShowShortFlameB_Rocket
  sec                     ; [27] + 2
  lda #14                 ; [29] + 2
  sbc HEIGHT              ; [31] + 3
  tay                     ; [34] + 2
  ldx #6                  ; [36] + 2
  jsr LoadFlameData       ; [38] + 6    = 44
EndShowFlameB_Rocket

  ; Take Off Frame
  lda #8                  ; [38] + 2
  sta TLINE               ; [40] + 3
  ldx FRAME               ; [43] + 3
  inx                     ; [46] + 2
  jsr LoadLiftOff         ; [48] + 6    = 54

  ldx SHIP                ; [25] + 3
  cpx #2                  ; [28] + 2
  bcs ShowRocket1         ; [30] + 2/3

ShowSoyuz1
  ; Background
  ; clc
  lda #<Background+9      ; [32] + 2
  adc HEIGHT              ; [34] + 3
  tax                     ; [37] + 2    SPF
  ldy #<Background+19     ; [39] + 2    FPF
  jmp Phase2BGStore       ; [41] + 3    = 44
  
ShowRocket1
  ; Background
  clc                     ; [33] + 2
  lda #<Background+49     ; [35] + 2
  adc HEIGHT              ; [37] + 3
  tax                     ; [40] + 2    SPF
  ldy #<Background+9      ; [42] + 2    FPF
  jmp Phase2BGStore       ; [44] + 3    = 47

; -----------------------------------------------------------------------------
; PHASE 0 - FLIGHT
; -----------------------------------------------------------------------------

DoShipPhase0
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; Ship Height
  sec                     ; [5] + 2
  lda #118                ; [7] + 2
  sbc FRAME               ; [9] + 3
  sta HEIGHT              ; [12] + 3
  
  ; Top Gap
  sec                     ; [15] + 2
  ldx SHIP                ; [17] + 3
  lda #SCREENH            ; [20] + 2
  sbc ShipHeight,X        ; [22] + 4
  sec                     ; [26] + 2
  sbc FlyingFrames,X      ; [28] + 4
  sta TEMP                ; [32] + 3
  sec                     ; [35] + 2
  sbc HEIGHT              ; [37] + 3
  bmi NoShipTopGap        ; [40] + 2/3
  sta GAPTOP              ; [42] + 3
  ldy #0                  ; [45] + 2
  sty TEMP                ; [47] + 3
  jmp EndShipTopGap       ; [50] + 3    = 53
NoShipTopGap
  sec                     ; [43] + 2
  lda HEIGHT              ; [45] + 3
  sbc TEMP                ; [48] + 3
  sta TEMP                ; [51] + 3
  ldy #0                  ; [54] + 2
  sty GAPTOP              ; [56] + 3
EndShipTopGap

  ; Ship
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  sec                     ; [5] + 2
  lda ShipHeight,X        ; [7] + 4
  sbc TEMP                ; [11] + 3
  sta SLINE               ; [14] + 3
  ldy #0                  ; [17] + 2
  jsr LoadShip            ; [19] + 6    = 25
  
  ; Rocket SFX
  lda HEIGHT              ; [35] + 3
  lsr                     ; [38] + 2
  lsr                     ; [40] + 2
  lsr                     ; [42] + 2
  sta TEMP                ; [44] + 3
  sec                     ; [47] + 2
  lda #28                 ; [49] + 2
  sbc TEMP                ; [51] + 3
  sta SFX                 ; [54] + 3
  
  ; Ship Type
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  ldx SHIP                ; [5] + 3
  bne ShowShip0           ; [8] + 2/3

ShowBuran0
  ; Flame & Bottom Gap
  lda FRAME               ; [10] + 3
  lsr                     ; [13] + 2
  bcc ShowShortFlameA_Buran   ; [15] + 2/3

ShowLongFlameA_Buran
  clc                     ; [17] + 2
  lda #$FE                ; [19] + 2
  adc HEIGHT              ; [21] + 3
  sta GAPBOT              ; [24] + 3
  clc                     ; [27] + 2
  lda #<Background        ; [29] + 2
  adc HEIGHT              ; [31] + 3
  sta FPF                 ; [34] + 3
  ldy #29                 ; [37] + 2
  sty FLINE               ; [39] + 3
  ldy #0                  ; [42] + 2
  ldx #3                  ; [44] + 2
  jsr LoadFlameData       ; [46] + 6    = 52
  jmp EndShowFlameA_Buran ; [35] + 3
  
ShowShortFlameA_Buran
  ; clc
  lda #8                  ; [18] + 2
  adc HEIGHT              ; [20] + 3
  sta GAPBOT              ; [23] + 3
  clc                     ; [26] + 2
  lda #<Background+10     ; [28] + 2
  adc HEIGHT              ; [30] + 3
  sta FPF                 ; [33] + 3
  ldy #19                 ; [36] + 2
  sty FLINE               ; [38] + 3
  ldy #0                  ; [41] + 2
  ldx #4                  ; [43] + 2
  jsr LoadFlameData       ; [45] + 6    = 51
EndShowFlameA_Buran

  ; Default Base
  ldx #21                 ; [38] + 2
  jsr LoadTakeOff         ; [40] + 6    = 46
  lda #0                  ; [32] + 2
  sta TLINE               ; [34] + 3

  ; Background
  clc                     ; [37] + 2
  lda #<Background+30     ; [39] + 2
  adc HEIGHT              ; [41] + 3
  tax                     ; [44] + 2
  ldy #<Background+1      ; [46] + 2
  jmp Phase0BGStore       ; [48] + 3    = 51

ShowShip0
  cpx #2                  ; [11] + 2
  bcs ShowRocket0         ; [13] + 2/3

ShowSoyuz0
  ; Flame & Bottom Gap
  lda FRAME               ; [15] + 3
  lsr                     ; [18] + 2
  bcc ShowShortFlameA_Soyuz   ; [20] + 2/3
  
ShowLongFlameA_Soyuz
  clc                     ; [22] + 2
  lda #$FE                ; [24] + 2
  adc HEIGHT              ; [26] + 3
  sta GAPBOT              ; [29] + 3
  clc                     ; [32] + 2
  lda #<Background+3      ; [34] + 2
  adc HEIGHT              ; [36] + 3
  sta FPF                 ; [39] + 3
  ldy #22                 ; [42] + 2
  sty FLINE               ; [44] + 3
  ldy #0                  ; [47] + 2
  ldx #5                  ; [49] + 2
  jsr LoadFlameData       ; [51] + 6    = 57
  jmp EndShowFlameA_Soyuz ; [35] + 3
  
ShowShortFlameA_Soyuz
  ; clc
  lda #7                  ; [23] + 2
  adc HEIGHT              ; [25] + 3
  sta GAPBOT              ; [28] + 3
  clc                     ; [31] + 2
  lda #<Background+12     ; [33] + 2
  adc HEIGHT              ; [35] + 3
  sta FPF                 ; [38] + 3
  ldy #13                 ; [41] + 2
  sty FLINE               ; [43] + 3
  ldy #0                  ; [46] + 2
  ldx #6                  ; [48] + 2
  jsr LoadFlameData       ; [50] + 6    = 56
EndShowFlameA_Soyuz

  ; Default Base
  ldx #21                 ; [38] + 2
  jsr LoadTakeOff         ; [40] + 6    = 46
  lda #3                  ; [32] + 2
  sta TLINE               ; [34] + 3

  ; Background
  clc                     ; [37] + 2
  lda #<Background+26     ; [39] + 2
  adc HEIGHT              ; [41] + 3
  tax                     ; [44] + 2
  ldy #<Background+4      ; [46] + 2
  jmp Phase0BGStore       ; [48] + 3    = 51

ShowRocket0
  ; Flame & Bottom Gap
  lda FRAME               ; [16] + 2
  lsr                     ; [18] + 2
  bcc ShowShortFlameA_Rocket   ; [20] + 2/3
  
ShowLongFlameA_Rocket
  clc                     ; [22] + 2
  lda #$FE                ; [24] + 2
  adc HEIGHT              ; [26] + 3
  sta GAPBOT              ; [29] + 3
  clc                     ; [32] + 2
  lda #<Background+43     ; [34] + 2
  adc HEIGHT              ; [36] + 3
  sta FPF                 ; [39] + 3
  ldy #22                 ; [42] + 2
  sty FLINE               ; [44] + 3
  ldy #0                  ; [47] + 2
  ldx #7                  ; [49] + 2
  jsr LoadFlameData       ; [51] + 6    = 57
  jmp EndShowFlameA_Rocket   ; [35] + 3
  
ShowShortFlameA_Rocket
  ; clc
  lda #7                  ; [23] + 2
  adc HEIGHT              ; [25] + 3
  sta GAPBOT              ; [28] + 3
  clc                     ; [31] + 2
  lda #<Background+52     ; [33] + 2
  adc HEIGHT              ; [35] + 3
  sta FPF                 ; [38] + 3
  ldy #13                 ; [41] + 2
  sty FLINE               ; [43] + 3
  ldy #0                  ; [46] + 2
  ldx #8                  ; [48] + 2
  jsr LoadFlameData       ; [50] + 6  = 56
EndShowFlameA_Rocket

  ; Default Base
  ldx #21                 ; [38] + 2
  jsr LoadTakeOff         ; [40] + 6    = 46
  lda #3                  ; [32] + 2
  sta TLINE               ; [34] + 3

  ; Background
  clc                     ; [37] + 2
  lda #<Background+66     ; [39] + 2
  adc HEIGHT              ; [41] + 3
  tax                     ; [44] + 2
  ldy #<Background+44     ; [46] + 2
Phase0BGStore
  stx SPF                 ; [51] + 3
  sty GPF                 ; [54] + 3
  
EndPhaseUpdate
  jmp PositionBuran       ; [57] + 3

; -----------------------------------------------------------------------------
; SHIP FUNCTIONS
; -----------------------------------------------------------------------------

LoadShip                  ; X = Ship Type, Y = Ship Height
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  sty TEMP                ; [5] + 3
  lda ShipData1Hi,X       ; [8] + 4
  sta SDATA1+1            ; [12] + 3
  lda ShipData2Hi,X       ; [15] + 4
  sta SDATA2+1            ; [19] + 3
  lda ShipData3Hi,X       ; [22] + 4
  sta SDATA3+1            ; [26] + 3
  lda ShipData4Hi,X       ; [29] + 4
  sta SDATA4+1            ; [33] + 3
  clc                     ; [36] + 2
  lda ShipData1Lo,X       ; [38] + 4
  adc TEMP                ; [42] + 3
  sta SDATA1              ; [45] + 3
  clc                     ; [48] + 2
  lda ShipData2Lo,X       ; [50] + 4
  adc TEMP                ; [54] + 3
  sta SDATA2              ; [57] + 3
  sta WSYNC               ; [0] 
  PLAY_MUSIC              ; [0] + 5
  clc                     ; [5] + 2
  lda ShipData3Lo,X       ; [7] + 4
  adc TEMP                ; [11] + 3
  sta SDATA3              ; [14] + 3
  clc                     ; [17] + 2
  lda ShipData4Lo,X       ; [19] + 4
  adc TEMP                ; [23] + 3
  sta SDATA4              ; [26] + 3
  rts                     ; [29] + 6    = 35

LoadFlameData             ; X = Flame Type, Y = Offset From Base
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  sty TEMP                ; [5] + 3
  lda ShipData1Hi,X       ; [8] + 4
  sta FDATA1+1            ; [12] + 3
  lda ShipData2Hi,X       ; [15] + 4
  sta FDATA2+1            ; [19] + 3
  lda ShipData3Hi,X       ; [22] + 4
  sta FDATA3+1            ; [26] + 3
  lda ShipData4Hi,X       ; [29] + 4
  sta FDATA4+1            ; [33] + 3
  clc                     ; [36] + 2
  lda ShipData1Lo,X       ; [38] + 4
  adc TEMP                ; [42] + 3
  sta FDATA1              ; [45] + 3
  clc                     ; [48] + 2
  lda ShipData2Lo,X       ; [50] + 4
  adc TEMP                ; [54] + 3
  sta FDATA2              ; [57] + 3
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  clc                     ; [5] + 2
  lda ShipData3Lo,X       ; [7] + 4
  adc TEMP                ; [11] + 3
  sta FDATA3              ; [14] + 3
  clc                     ; [17] + 2
  lda ShipData4Lo,X       ; [19] + 4
  adc TEMP                ; [23] + 3
  sta FDATA4              ; [26] + 3
  rts                     ; [29] + 6    = 35

LoadSmoke                 ; X = Frame, Y = Data Hi (Smoke1_0 or RocketSmoke1_0)
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  sty TDATA0+1            ; [5] + 3
  sty TDATA5+1            ; [8] + 3
  lda SmokeFrames1Hi,X    ; [11] + 4
  sta TDATA1+1            ; [15] + 3
  lda SmokeFrames2Hi,X    ; [18] + 4
  sta TDATA2+1            ; [22] + 3
  lda SmokeFrames3Hi,X    ; [25] + 4
  sta TDATA3+1            ; [29] + 3
  lda SmokeFrames4Hi,X    ; [32] + 4
  sta TDATA4+1            ; [36] + 3
  lda SmokeFrames0Lo,X    ; [39] + 4
  sta TDATA0              ; [43] + 3
  lda SmokeFrames1Lo,X    ; [46] + 4
  sta TDATA1              ; [50] + 3
  lda SmokeFrames2Lo,X    ; [53] + 4
  sta TDATA2              ; [57] + 3
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  lda SmokeFrames3Lo,X    ; [5] + 4
  sta TDATA3              ; [9] + 3
  lda SmokeFrames4Lo,X    ; [12] + 4
  sta TDATA4              ; [16] + 3
  lda SmokeFrames5Lo,X    ; [19] + 4
  sta TDATA5              ; [23] + 3
  rts                     ; [26] + 6    = 32

LoadTakeOff               ; X = Frame
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  ldy #>TakeOff1_0        ; [5] + 2
  sty TDATA0+1            ; [7] + 3
  sty TDATA5+1            ; [10] + 3
  lda TakeOffFrames1AHi,X ; [13] + 4
  sta TDATA1+1            ; [17] + 3
  lda TakeOffFrames2AHi,X ; [20] + 4
  sta TDATA2+1            ; [24] + 3
  lda TakeOffFrames3AHi,X ; [27] + 4
  sta TDATA3+1            ; [31] + 3
  lda TakeOffFrames4AHi,X ; [34] + 4
  sta TDATA4+1            ; [38] + 3
  lda TakeOffFrames0ALo,X ; [41] + 4
  sta TDATA0              ; [45] + 3
  lda TakeOffFrames1ALo,X ; [48] + 4
  sta TDATA1              ; [52] + 3
  lda TakeOffFrames2ALo,X ; [55] + 4
  sta TDATA2              ; [59] + 3
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  lda TakeOffFrames3ALo,X ; [5] + 4
  sta TDATA3              ; [9] + 3
  lda TakeOffFrames4ALo,X ; [12] + 4
  sta TDATA4              ; [16] + 3
  lda TakeOffFrames5ALo,X ; [19] + 4
  sta TDATA5              ; [23] + 3
  rts                     ; [26] + 6    = 32

LoadLiftOff               ; X = Frame
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  ldy #>RocketLiftOff1_0  ; [5] + 2
  sty TDATA0+1            ; [7] + 3
  sty TDATA1+1            ; [10] + 3
  sty TDATA4+1            ; [13] + 3
  sty TDATA5+1            ; [16] + 3
  lda LiftOffFrames2Hi,X  ; [19] + 4
  sta TDATA2+1            ; [23] + 3
  lda LiftOffFrames3Hi,X  ; [26] + 4
  sta TDATA3+1            ; [30] + 3
  lda LiftOffFrames0Lo,X  ; [33] + 4
  sta TDATA0              ; [37] + 3
  lda LiftOffFrames1Lo,X  ; [40] + 4
  sta TDATA1              ; [44] + 3
  lda LiftOffFrames2Lo,X  ; [47] + 4
  sta TDATA2              ; [51] + 3
  lda LiftOffFrames3Lo,X  ; [54] + 4
  sta TDATA3              ; [58] + 3
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  lda LiftOffFrames4Lo,X  ; [5] + 4
  sta TDATA4              ; [9] + 3
  lda LiftOffFrames5Lo,X  ; [12] + 4
  sta TDATA5              ; [16] + 3
  rts                     ; [19] + 6    = 25

BeginBuran
  ; Gap Above Shuttle
  ldy GAPTOP              ; [29] + 3
  bmi ShowBuran           ; [32] + 2/3
TopGap
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  SLEEP 26                ; [5] + 26
  dey                     ; [31] + 2
  bpl TopGap              ; [33] + 2/3
ShowBuran
  SLEEP 16                ; [35] + 16
  lda GAPBOT              ; [51] + 3
  bpl BuranGap            ; [54] + 2/3
NoBuranGap
  SLEEP 3                 ; [56] + 3
  jmp BuranLoop1          ; [59] + 3    = 62
BuranGap
  nop                     ; [57] + 2
  jmp BuranLoop2          ; [59] + 3    = 62

  DC.B  "SOYUZ"

; -----------------------------------------------------------------------------
; ANIMATION DATA
; -----------------------------------------------------------------------------

  ALIGN 256

; Smoke Animations
SmokeFrames0Lo
  DC.B  <Smoke3_0, <Smoke2_0, <Smoke1_0
  DC.B  <SoyuzSmoke2_0, <SoyuzSmoke1_0, <RocketSmoke2_0, <RocketSmoke1_0
SmokeFrames1Lo
  DC.B  <Smoke3_1, <Smoke2_1, <Smoke1_1
  DC.B  <SoyuzSmoke2_1, <SoyuzSmoke1_1, <RocketSmoke2_1, <RocketSmoke1_1
SmokeFrames2Lo
  DC.B  <Smoke3_2, <Smoke2_2, <Smoke1_2
  DC.B  <SoyuzSmoke2_2, <SoyuzSmoke1_2, <RocketSmoke2_2, <RocketSmoke1_2
SmokeFrames3Lo
  DC.B  <Smoke3_3, <Smoke2_3, <Smoke1_3
  DC.B  <SoyuzSmoke2_3, <SoyuzSmoke1_3, <RocketSmoke2_3, <RocketSmoke1_3
SmokeFrames4Lo
  DC.B  <Smoke3_4, <Smoke2_4, <Smoke1_4
  DC.B  <SoyuzSmoke2_4, <SoyuzSmoke1_4, <RocketSmoke2_4, <RocketSmoke1_4
SmokeFrames5Lo
  DC.B  <Smoke3_5, <Smoke2_5, <Smoke1_5
  DC.B  <SoyuzSmoke2_5, <SoyuzSmoke1_5, <RocketSmoke2_5, <RocketSmoke1_5
SmokeFrames1Hi
  DC.B  >Smoke3_1, >Smoke2_1, >Smoke1_1
  DC.B  >SoyuzSmoke2_1, >SoyuzSmoke1_1, >RocketSmoke2_1, >RocketSmoke1_1
SmokeFrames2Hi
  DC.B  >Smoke3_2, >Smoke2_2, >Smoke1_2
  DC.B  >SoyuzSmoke2_2, >SoyuzSmoke1_2, >RocketSmoke2_2, >RocketSmoke1_2
SmokeFrames3Hi
  DC.B  >Smoke3_3, >Smoke2_3, >Smoke1_3
  DC.B  >SoyuzSmoke2_3, >SoyuzSmoke1_3, >RocketSmoke2_3, >RocketSmoke1_3
SmokeFrames4Hi
  DC.B  >Smoke3_4, >Smoke2_4, >Smoke1_4
  DC.B  >SoyuzSmoke2_4, >SoyuzSmoke1_4, >RocketSmoke2_4, >RocketSmoke1_4

; Ship and Flame Animations
ShipData1Lo
  DC.B  <Shuttle1, <Shuttle0, <Shuttle0
  DC.B  <LongFlame1, <ShortFlame1
  DC.B  <LongFlame0, <LongFlame0
  DC.B  <LongFlame0, <LongFlame0
ShipData2Lo
  DC.B  <Shuttle2, <Soyuz1, <Rocket1
  DC.B  <LongFlame2, <ShortFlame2
  DC.B  <SoyuzBigFlame_0, <SoyuzSmallFlame_0
  DC.B  <RocketBigFlame_0, <RocketSmallFlame_0
ShipData3Lo
  DC.B  <Shuttle3, <Soyuz2, <Rocket2
  DC.B  <LongFlame3, <ShortFlame3
  DC.B  <SoyuzBigFlame_1, <SoyuzSmallFlame_1
  DC.B  <RocketBigFlame_1, <RocketSmallFlame_1
ShipData4Lo
  DC.B  <Shuttle4, <Shuttle0, <Shuttle0
  DC.B  <LongFlame4, <ShortFlame4
  DC.B  <LongFlame0, <LongFlame0
  DC.B  <LongFlame0, <LongFlame0
ShipData1Hi
  DC.B  >Shuttle1, >Shuttle0, >Shuttle0
  DC.B  >LongFlame1, >ShortFlame1
  DC.B  >LongFlame0, >LongFlame0
  DC.B  >LongFlame0, >LongFlame0
ShipData2Hi
  DC.B  >Shuttle2, >Soyuz1, >Rocket1
  DC.B  >LongFlame2, >ShortFlame2
  DC.B  >SoyuzBigFlame_0, >SoyuzSmallFlame_0
  DC.B  >RocketBigFlame_0, >RocketSmallFlame_0
ShipData3Hi
  DC.B  >Shuttle3, >Soyuz2, >Rocket2
  DC.B  >LongFlame3, >ShortFlame3
  DC.B  >SoyuzBigFlame_1, >SoyuzSmallFlame_1
  DC.B  >RocketBigFlame_1, >RocketSmallFlame_1
ShipData4Hi
  DC.B  >Shuttle4, >Shuttle0, >Shuttle0
  DC.B  >LongFlame4, >ShortFlame4
  DC.B  >LongFlame0, >LongFlame0
  DC.B  >LongFlame0, >LongFlame0

; Buran Takeoff Animations
TakeOffFrames0ALo
  DC.B  <TakeOff20_0, <TakeOff19_0, <TakeOff18_0, <TakeOff17_0      ; A
  DC.B  <TakeOff16_0, <TakeOff15_0, <TakeOff14_0, <TakeOff13_0
  DC.B  <TakeOff12_0, <TakeOff11_0, <TakeOff10_0, <TakeOff9_0
  DC.B  <TakeOff8_0,  <TakeOff7_0,  <TakeOff6_0,  <TakeOff5_0
  DC.B  <TakeOff4_0,  <TakeOff3_0,  <TakeOff2_0,  <TakeOff1_0
  DC.B  <TakeOff32_0, <TakeOff31_0, <TakeOff30_0, <TakeOff29_0      ; B
  DC.B  <TakeOff28_0, <TakeOff27_0, <TakeOff26_0, <TakeOff25_0
  DC.B  <TakeOff24_0, <TakeOff23_0, <TakeOff22_0, <TakeOff21_0
TakeOffFrames1ALo
  DC.B  <TakeOff20_1, <TakeOff19_1, <TakeOff18_1, <TakeOff17_1      ; A
  DC.B  <TakeOff16_1, <TakeOff15_1, <TakeOff14_1, <TakeOff13_1
  DC.B  <TakeOff12_1, <TakeOff11_1, <TakeOff10_1, <TakeOff9_1
  DC.B  <TakeOff8_1,  <TakeOff7_1,  <TakeOff6_1,  <TakeOff5_1
  DC.B  <TakeOff4_1,  <TakeOff3_1,  <TakeOff2_1,  <TakeOff1_1
  DC.B  <TakeOff32_1, <TakeOff31_1, <TakeOff30_1, <TakeOff29_1      ; B
  DC.B  <TakeOff28_1, <TakeOff27_1, <TakeOff26_1, <TakeOff25_1
  DC.B  <TakeOff24_1, <TakeOff23_1, <TakeOff22_1, <TakeOff21_1
TakeOffFrames2ALo
  DC.B  <TakeOff20_2, <TakeOff19_2, <TakeOff18_2, <TakeOff17_2      ; A
  DC.B  <TakeOff16_2, <TakeOff15_2, <TakeOff14_2, <TakeOff13_2
  DC.B  <TakeOff12_2, <TakeOff11_2, <TakeOff10_2, <TakeOff9_2
  DC.B  <TakeOff8_2,  <TakeOff7_2,  <TakeOff6_2,  <TakeOff5_2
  DC.B  <TakeOff4_2,  <TakeOff3_2,  <TakeOff2_2,  <TakeOff1_2
  DC.B  <TakeOff32_2, <TakeOff31_2, <TakeOff30_2, <TakeOff29_2      ; B
  DC.B  <TakeOff28_2, <TakeOff27_2, <TakeOff26_2, <TakeOff25_2
  DC.B  <TakeOff24_2, <TakeOff23_2, <TakeOff22_2, <TakeOff21_2
TakeOffFrames3ALo
  DC.B  <TakeOff20_3, <TakeOff19_3, <TakeOff18_3, <TakeOff17_3      ; A
  DC.B  <TakeOff16_3, <TakeOff15_3, <TakeOff14_3, <TakeOff13_3
  DC.B  <TakeOff12_3, <TakeOff11_3, <TakeOff10_3, <TakeOff9_3
  DC.B  <TakeOff8_3,  <TakeOff7_3,  <TakeOff6_3,  <TakeOff5_3
  DC.B  <TakeOff4_3,  <TakeOff3_3,  <TakeOff2_3,  <TakeOff1_3
  DC.B  <TakeOff32_3, <TakeOff31_3, <TakeOff30_3, <TakeOff29_3      ; B
  DC.B  <TakeOff28_3, <TakeOff27_3, <TakeOff26_3, <TakeOff25_3
  DC.B  <TakeOff24_3, <TakeOff23_3, <TakeOff22_3, <TakeOff21_3
TakeOffFrames4ALo
  DC.B  <TakeOff20_4, <TakeOff19_4, <TakeOff18_4, <TakeOff17_4      ; A
  DC.B  <TakeOff16_4, <TakeOff15_4, <TakeOff14_4, <TakeOff13_4
  DC.B  <TakeOff12_4, <TakeOff11_4, <TakeOff10_4, <TakeOff9_4
  DC.B  <TakeOff8_4,  <TakeOff7_4,  <TakeOff6_4,  <TakeOff5_4
  DC.B  <TakeOff4_4,  <TakeOff3_4,  <TakeOff2_4,  <TakeOff1_4
  DC.B  <TakeOff32_4, <TakeOff31_4, <TakeOff30_4, <TakeOff29_4      ; B
  DC.B  <TakeOff28_4, <TakeOff27_4, <TakeOff26_4, <TakeOff25_4
  DC.B  <TakeOff24_4, <TakeOff23_4, <TakeOff22_4, <TakeOff21_4
TakeOffFrames5ALo
  DC.B  <TakeOff20_5, <TakeOff19_5, <TakeOff18_5, <TakeOff17_5      ; A
  DC.B  <TakeOff16_5, <TakeOff15_5, <TakeOff14_5, <TakeOff13_5
  DC.B  <TakeOff12_5, <TakeOff11_5, <TakeOff10_5, <TakeOff9_5
  DC.B  <TakeOff8_5,  <TakeOff7_5,  <TakeOff6_5,  <TakeOff5_5
  DC.B  <TakeOff4_5,  <TakeOff3_5,  <TakeOff2_5,  <TakeOff1_5
  DC.B  <TakeOff32_5, <TakeOff31_5, <TakeOff30_5, <TakeOff29_5      ; B
  DC.B  <TakeOff28_5, <TakeOff27_5, <TakeOff26_5, <TakeOff25_5
  DC.B  <TakeOff24_5, <TakeOff23_5, <TakeOff22_5, <TakeOff21_5
TakeOffFrames1AHi
  DC.B  >TakeOff20_1, >TakeOff19_1, >TakeOff18_1, >TakeOff17_1      ; A
  DC.B  >TakeOff16_1, >TakeOff15_1, >TakeOff14_1, >TakeOff13_1
  DC.B  >TakeOff12_1, >TakeOff11_1, >TakeOff10_1, >TakeOff9_1
  DC.B  >TakeOff8_1,  >TakeOff7_1,  >TakeOff6_1,  >TakeOff5_1
  DC.B  >TakeOff4_1,  >TakeOff3_1,  >TakeOff2_1,  >TakeOff1_1
  DC.B  >TakeOff31_1, >TakeOff31_1, >TakeOff30_1, >TakeOff29_1      ; B
  DC.B  >TakeOff28_1, >TakeOff27_1, >TakeOff26_1, >TakeOff25_1
  DC.B  >TakeOff24_1, >TakeOff23_1, >TakeOff22_1, >TakeOff21_1
TakeOffFrames2AHi
  DC.B  >TakeOff20_2, >TakeOff19_2, >TakeOff18_2, >TakeOff17_2      ; A
  DC.B  >TakeOff16_2, >TakeOff15_2, >TakeOff14_2, >TakeOff13_2
  DC.B  >TakeOff12_2, >TakeOff11_2, >TakeOff10_2, >TakeOff9_2
  DC.B  >TakeOff8_2,  >TakeOff7_2,  >TakeOff6_2,  >TakeOff5_2
  DC.B  >TakeOff4_2,  >TakeOff3_2,  >TakeOff2_2,  >TakeOff1_2
  DC.B  >TakeOff31_2, >TakeOff31_2, >TakeOff30_2, >TakeOff29_2      ; B
  DC.B  >TakeOff28_2, >TakeOff27_2, >TakeOff26_2, >TakeOff25_2
  DC.B  >TakeOff24_2, >TakeOff23_2, >TakeOff22_2, >TakeOff21_2
TakeOffFrames3AHi
  DC.B  >TakeOff20_3, >TakeOff19_3, >TakeOff18_3, >TakeOff17_3      ; A
  DC.B  >TakeOff16_3, >TakeOff15_3, >TakeOff14_3, >TakeOff13_3
  DC.B  >TakeOff12_3, >TakeOff11_3, >TakeOff10_3, >TakeOff9_3
  DC.B  >TakeOff8_3,  >TakeOff7_3,  >TakeOff6_3,  >TakeOff5_3
  DC.B  >TakeOff4_3,  >TakeOff3_3,  >TakeOff2_3,  >TakeOff1_3
  DC.B  >TakeOff31_3, >TakeOff31_3, >TakeOff30_3, >TakeOff29_3      ; B
  DC.B  >TakeOff28_3, >TakeOff27_3, >TakeOff26_3, >TakeOff25_3
  DC.B  >TakeOff24_3, >TakeOff23_3, >TakeOff22_3, >TakeOff21_3
TakeOffFrames4AHi
  DC.B  >TakeOff20_4, >TakeOff19_4, >TakeOff18_4, >TakeOff17_4      ; A
  DC.B  >TakeOff16_4, >TakeOff15_4, >TakeOff14_4, >TakeOff13_4
  DC.B  >TakeOff12_4, >TakeOff11_4, >TakeOff10_4, >TakeOff9_4
  DC.B  >TakeOff8_4,  >TakeOff7_4,  >TakeOff6_4,  >TakeOff5_4
  DC.B  >TakeOff4_4,  >TakeOff3_4,  >TakeOff2_4,  >TakeOff1_4
  DC.B  >TakeOff31_4, >TakeOff31_4, >TakeOff30_4, >TakeOff29_4      ; B
  DC.B  >TakeOff28_4, >TakeOff27_4, >TakeOff26_4, >TakeOff25_4
  DC.B  >TakeOff24_4, >TakeOff23_4, >TakeOff22_4, >TakeOff21_4

; Rocket/Soyuz LiftOff Animations
LiftOffFrames0Lo
  DC.B  <RocketLiftOff27_0, <RocketLiftOff26_0, <RocketLiftOff25_0  ; Rocket
  DC.B  <RocketLiftOff24_0, <RocketLiftOff23_0, <RocketLiftOff22_0
  DC.B  <RocketLiftOff21_0, <RocketLiftOff20_0, <RocketLiftOff19_0
  DC.B  <RocketLiftOff18_0, <RocketLiftOff17_0, <RocketLiftOff16_0
  DC.B  <RocketLiftOff15_0, <RocketLiftOff14_0, <RocketLiftOff13_0
  DC.B  <RocketLiftOff12_0, <RocketLiftOff11_0, <RocketLiftOff10_0
  DC.B  <RocketLiftOff9_0, <RocketLiftOff8_0, <RocketLiftOff7_0
  DC.B  <RocketLiftOff6_0, <RocketLiftOff5_0, <RocketLiftOff4_0
  DC.B  <RocketLiftOff3_0, <RocketLiftOff2_0, <RocketLiftOff1_0
  DC.B  <RocketLiftOff9_0, <SoyuzLiftOff8_0, <SoyuzLiftOff7_0       ; Soyuz
  DC.B  <SoyuzLiftOff6_0, <SoyuzLiftOff5_0, <SoyuzLiftOff4_0
  DC.B  <SoyuzLiftOff3_0, <SoyuzLiftOff2_0, <SoyuzLiftOff1_0
  DC.B  <SoyuzLiftOff27_0
LiftOffFrames1Lo
  DC.B  <RocketLiftOff27_1, <RocketLiftOff26_1, <RocketLiftOff25_1  ; Rocket
  DC.B  <RocketLiftOff24_1, <RocketLiftOff23_1, <RocketLiftOff22_1
  DC.B  <RocketLiftOff21_1, <RocketLiftOff20_1, <RocketLiftOff19_1
  DC.B  <RocketLiftOff18_1, <RocketLiftOff17_1, <RocketLiftOff16_1
  DC.B  <RocketLiftOff15_1, <RocketLiftOff14_1, <RocketLiftOff13_1
  DC.B  <RocketLiftOff12_1, <RocketLiftOff11_1, <RocketLiftOff10_1
  DC.B  <RocketLiftOff9_1, <RocketLiftOff8_1, <RocketLiftOff7_1
  DC.B  <RocketLiftOff6_1, <RocketLiftOff5_1, <RocketLiftOff4_1
  DC.B  <RocketLiftOff3_1, <RocketLiftOff2_1, <RocketLiftOff1_1
  DC.B  <RocketLiftOff9_1, <SoyuzLiftOff8_1, <SoyuzLiftOff7_1       ; Soyuz
  DC.B  <SoyuzLiftOff6_1, <SoyuzLiftOff5_1, <SoyuzLiftOff4_1
  DC.B  <SoyuzLiftOff3_1, <SoyuzLiftOff2_1, <SoyuzLiftOff1_1
  DC.B  <SoyuzLiftOff27_1
LiftOffFrames2Lo
  DC.B  <RocketLiftOff27_2, <RocketLiftOff26_2, <RocketLiftOff25_2  ; Rocket
  DC.B  <RocketLiftOff24_2, <RocketLiftOff23_2, <RocketLiftOff22_2
  DC.B  <RocketLiftOff21_2, <RocketLiftOff20_2, <RocketLiftOff19_2
  DC.B  <RocketLiftOff18_2, <RocketLiftOff17_2, <RocketLiftOff16_2
  DC.B  <RocketLiftOff15_2, <RocketLiftOff14_2, <RocketLiftOff13_2
  DC.B  <RocketLiftOff12_2, <RocketLiftOff11_2, <RocketLiftOff10_2
  DC.B  <RocketLiftOff9_2, <RocketLiftOff8_2, <RocketLiftOff7_2
  DC.B  <RocketLiftOff6_2, <RocketLiftOff5_2, <RocketLiftOff4_2
  DC.B  <RocketLiftOff3_2, <RocketLiftOff2_2, <RocketLiftOff1_2
  DC.B  <RocketLiftOff9_2, <SoyuzLiftOff8_2, <SoyuzLiftOff7_2       ; Soyuz
  DC.B  <SoyuzLiftOff6_2, <SoyuzLiftOff5_2, <SoyuzLiftOff4_2
  DC.B  <SoyuzLiftOff3_2, <SoyuzLiftOff2_2, <SoyuzLiftOff1_2
  DC.B  <SoyuzLiftOff27_2
LiftOffFrames3Lo
  DC.B  <RocketLiftOff27_3, <RocketLiftOff26_3, <RocketLiftOff25_3  ; Rocket
  DC.B  <RocketLiftOff24_3, <RocketLiftOff23_3, <RocketLiftOff22_3
  DC.B  <RocketLiftOff21_3, <RocketLiftOff20_3, <RocketLiftOff19_3
  DC.B  <RocketLiftOff18_3, <RocketLiftOff17_3, <RocketLiftOff16_3
  DC.B  <RocketLiftOff15_3, <RocketLiftOff14_3, <RocketLiftOff13_3
  DC.B  <RocketLiftOff12_3, <RocketLiftOff11_3, <RocketLiftOff10_3
  DC.B  <RocketLiftOff9_3, <RocketLiftOff8_3, <RocketLiftOff7_3
  DC.B  <RocketLiftOff6_3, <RocketLiftOff5_3, <RocketLiftOff4_3
  DC.B  <RocketLiftOff3_3, <RocketLiftOff2_3, <RocketLiftOff1_3
  DC.B  <RocketLiftOff9_3, <SoyuzLiftOff8_3, <SoyuzLiftOff7_3       ; Soyuz
  DC.B  <SoyuzLiftOff6_3, <SoyuzLiftOff5_3, <SoyuzLiftOff4_3
  DC.B  <SoyuzLiftOff3_3, <SoyuzLiftOff2_3, <SoyuzLiftOff1_3
  DC.B  <SoyuzLiftOff27_3
LiftOffFrames4Lo
  DC.B  <RocketLiftOff27_4, <RocketLiftOff26_4, <RocketLiftOff25_4  ; Rocket
  DC.B  <RocketLiftOff24_4, <RocketLiftOff23_4, <RocketLiftOff22_4
  DC.B  <RocketLiftOff21_4, <RocketLiftOff20_4, <RocketLiftOff19_4
  DC.B  <RocketLiftOff18_4, <RocketLiftOff17_4, <RocketLiftOff16_4
  DC.B  <RocketLiftOff15_4, <RocketLiftOff14_4, <RocketLiftOff13_4
  DC.B  <RocketLiftOff12_4, <RocketLiftOff11_4, <RocketLiftOff10_4
  DC.B  <RocketLiftOff9_4, <RocketLiftOff8_4, <RocketLiftOff7_4
  DC.B  <RocketLiftOff6_4, <RocketLiftOff5_4, <RocketLiftOff4_4
  DC.B  <RocketLiftOff3_4, <RocketLiftOff2_4, <RocketLiftOff1_4
  DC.B  <RocketLiftOff9_4, <SoyuzLiftOff8_4, <SoyuzLiftOff7_4       ; Soyuz
  DC.B  <SoyuzLiftOff6_4, <SoyuzLiftOff5_4, <SoyuzLiftOff4_4
  DC.B  <SoyuzLiftOff3_4, <SoyuzLiftOff2_4, <SoyuzLiftOff1_4
  DC.B  <SoyuzLiftOff27_4
LiftOffFrames5Lo
  DC.B  <RocketLiftOff27_5, <RocketLiftOff26_5, <RocketLiftOff25_5  ; Rocket
  DC.B  <RocketLiftOff24_5, <RocketLiftOff23_5, <RocketLiftOff22_5 
  DC.B  <RocketLiftOff21_5, <RocketLiftOff20_5, <RocketLiftOff19_5
  DC.B  <RocketLiftOff18_5, <RocketLiftOff17_5, <RocketLiftOff16_5
  DC.B  <RocketLiftOff15_5, <RocketLiftOff14_5, <RocketLiftOff13_5
  DC.B  <RocketLiftOff12_5, <RocketLiftOff11_5, <RocketLiftOff10_5
  DC.B  <RocketLiftOff9_5, <RocketLiftOff8_5, <RocketLiftOff7_5
  DC.B  <RocketLiftOff6_5, <RocketLiftOff5_5, <RocketLiftOff4_5
  DC.B  <RocketLiftOff3_5, <RocketLiftOff2_5, <RocketLiftOff1_5
  DC.B  <RocketLiftOff9_5, <SoyuzLiftOff8_5, <SoyuzLiftOff7_5       ; Soyuz
  DC.B  <SoyuzLiftOff6_5, <SoyuzLiftOff5_5, <SoyuzLiftOff4_5
  DC.B  <SoyuzLiftOff3_5, <SoyuzLiftOff2_5, <SoyuzLiftOff1_5
  DC.B  <SoyuzLiftOff27_5
LiftOffFrames2Hi
  DC.B  >RocketLiftOff27_2, >RocketLiftOff26_2, >RocketLiftOff25_2  ; Rocket
  DC.B  >RocketLiftOff24_2, >RocketLiftOff23_2, >RocketLiftOff22_2
  DC.B  >RocketLiftOff21_2, >RocketLiftOff20_2, >RocketLiftOff19_2
  DC.B  >RocketLiftOff18_2, >RocketLiftOff17_2, >RocketLiftOff16_2
  DC.B  >RocketLiftOff15_2, >RocketLiftOff14_2, >RocketLiftOff13_2
  DC.B  >RocketLiftOff12_2, >RocketLiftOff11_2, >RocketLiftOff10_2
  DC.B  >RocketLiftOff9_2, >RocketLiftOff8_2, >RocketLiftOff7_2
  DC.B  >RocketLiftOff6_2, >RocketLiftOff5_2, >RocketLiftOff4_2
  DC.B  >RocketLiftOff3_2, >RocketLiftOff2_2, >RocketLiftOff1_2
  DC.B  >RocketLiftOff9_2, >SoyuzLiftOff8_2, >SoyuzLiftOff7_2       ; Soyuz
  DC.B  >SoyuzLiftOff6_2, >SoyuzLiftOff5_2, >SoyuzLiftOff4_2
  DC.B  >SoyuzLiftOff3_2, >SoyuzLiftOff2_2, >SoyuzLiftOff1_2
  DC.B  >SoyuzLiftOff27_2
LiftOffFrames3Hi
  DC.B  >RocketLiftOff27_3, >RocketLiftOff26_3, >RocketLiftOff25_3  ; Rocket
  DC.B  >RocketLiftOff24_3, >RocketLiftOff23_3, >RocketLiftOff22_3
  DC.B  >RocketLiftOff21_3, >RocketLiftOff20_3, >RocketLiftOff19_3
  DC.B  >RocketLiftOff18_3, >RocketLiftOff17_3, >RocketLiftOff16_3
  DC.B  >RocketLiftOff15_3, >RocketLiftOff14_3, >RocketLiftOff13_3
  DC.B  >RocketLiftOff12_3, >RocketLiftOff11_3, >RocketLiftOff10_3
  DC.B  >RocketLiftOff9_3, >RocketLiftOff8_3, >RocketLiftOff7_3
  DC.B  >RocketLiftOff6_3, >RocketLiftOff5_3, >RocketLiftOff4_3
  DC.B  >RocketLiftOff3_3, >RocketLiftOff2_3, >RocketLiftOff1_3
  DC.B  >RocketLiftOff9_3, >SoyuzLiftOff8_3, >SoyuzLiftOff7_3       ; Soyuz
  DC.B  >SoyuzLiftOff6_3, >SoyuzLiftOff5_3, >SoyuzLiftOff4_3
  DC.B  >SoyuzLiftOff3_3, >SoyuzLiftOff2_3, >SoyuzLiftOff1_3
  DC.B  >SoyuzLiftOff27_3

  DC.B  "BURAN"
  
  ; Buran Data
  ALIGN 256
  BURAN0
  ALIGN 256
  BURAN1
  DC.B  "N1"
  ALIGN 256
  BURAN2
  DC.B  "N2"
  ALIGN 256
  BURAN3

BuranSmokeSeq
  DC.B  0, 1, 0, 1, 0, 1, 0, 1, 2, 1, 2, 1, 2, 1, 2
SoyuzSmokeSeq
  DC.B  4, 3, 4, 3, 4, 3, 4, 3, 4
RocketSmokeSeq
  DC.B  6, 5, 6, 5, 6, 5, 6, 5, 6

  DC.B  "N3"
  ALIGN 256
  BURAN4

ShipHeight
  DC.B  SHUTTLEH, SOYUZH, ROCKETH

PhaseFrames
  DC.B  117, 9, 19, 14     ; Buran
  DC.B  117, 16, 8, 8      ; Soyuz
  DC.B  117, 16, 8, 8      ; Rocket
  
TakeOffFramesA
  DC.B  20, 9, 9
TakeOffFramesB
  DC.B  10, 17, 17
FlyingFrames
  DC.B  30, 26, 26

SmokeHeight
  DC.B  20, 8, 8
 
  ; DC.B  "N4"
  echo "----",($FFE6 - *) , "bytes left (BANK 4 - SHUTTLE B)"

  ; Switch Points
  ORG     $CFE6
  RORG    $FFE6
Init4
  ; Switch to Bank 7
  nop     $FFFB
QuitSpecial
  ; Switch to Bank 7
  nop     $FFFB
  ; Begin Ship Kernel
  jmp     ShipMainLoop

  ORG     $CFF4
  RORG    $FFF4
  DC.B    "BANK4", 0
  DC.W    (PlusROM_API - $D000)
  DC.W    Init4, Init4

