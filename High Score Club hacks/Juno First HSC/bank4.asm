; -----------------------------------------------------------------------------
; BANK 4 - LOWER KERNEL (COLLISION DETECTION & SHOW TIME/SCORE/LIVES/MESSAGES)
; -----------------------------------------------------------------------------

  SEG     BANK4
  ORG     $B000
  RORG    $F000
  
Init4
  ; Switch to Bank 8
  nop     $FFFB
EndGameLogic
  ; Switch to Bank 1
  nop     $FFF4
SpawnAliens
  ; Switch to Bank 5
  nop     $FFF8
  jmp FinishSpawn

; -----------------------------------------------------------------------------
; PART 1 - LASER/ALIEN & BULLET/SHIP COLLISION DETECTION
; -----------------------------------------------------------------------------

  ; Normal Collision Detection Unless Warping
  bit TIME
  bmi CollDetect
  lda COUNTER
  and #%00011111
  beq CollDetect
WarpDetect
  sta WSYNC                 ; [0]
  ; Detect Lasers On Alternate Cycles During Warping
  lda CYCLE                 ; [0] + 3
  lsr                       ; [3] + 2
  bcs LaserDetect1          ; [5] + 2/3
  ; Check For Astronaut Collisions During Warping
  lda CXPPMM                ; [7] + 3
  bpl LaserDetect2          ; [10] + 2/3
  jmp AstroDetect           ; [12] + 3
CollDetect
  sta WSYNC                 ; [0]
  ; Skip Ship Collision Detection If Exploding/Appearing
  lda LIVES                 ; [0] + 3
  and #%11110000            ; [3] + 2
  bne LaserDetect1          ; [5] + 2/3
  ; Ship+Alien/Bullet and Laser+Alien Collisions On Alternate Frames
  lda CYCLE                 ; [7] + 3
  lsr                       ; [10] + 2
  bcc ShipDetect1           ; [12] + 2/3
  ; Check Ship Collisions If Not Firing
  lda LASERY                ; [14] + 3
  beq ShipDetect2           ; [17] + 2/3
LaserDetect
  ; Calculate Laser Extent
  asl                       ; [19] + 2
  clc                       ; [21] + 2
  adc #4                    ; [23] + 2
  sta LINE                  ; [25] + 3
  ; Loop Through Aliens
  ldx #INVMAX-1             ; [28] + 2 = 30
LaserCollLoop
  ; Check Y Position (SHIPH-8 < Y-4 < LASERY)
  lda INVY,X                ; [0] + 4
  cmp #SHIPH-4              ; [4] + 2
  bcc NoLaserColl0          ; [6] + 2/3
  cmp LINE                  ; [8] + 3
  bcs NoLaserColl1          ; [11] + 2/3
  ; Check LASERX > X
  lda LASERX                ; [13] + 3
  sbc INVX,X                ; [16] + 4
  sbc #8                    ; [20] + 2
  bcs NoLaserColl2          ; [22] + 2/3
  ; Check For Empty Sprite
  lda INVT,X                ; [24] + 4
  and #%00111100            ; [28] + 2    
  beq NoLaserColl3          ; [30] + 2/3
  ; Cant Shoot Astronauts or Explosions
  cmp #9<<2                 ; [32] + 2
  bcs NoLaserColl4          ; [34] + 2/3
  tay                       ; [36] + 2
  lda Scores,Y              ; [38] + 4
  sta INVT,X                ; [42] + 4
NextLaserColl
  dex                       ; [46] + 2
  bpl LaserCollLoop         ; [48] + 2/3 
                            ; TOTAL = 30 + (51*15) + 50 = 845 (11.1 SCANLINES)
  jmp EndBColl              ; SKIP FINAL SCANLINE
  
  ; Pad Out Routines To Constant Time
NoLaserColl0
  SLEEP 5                   ; [9] + 5
NoLaserColl1    
  SLEEP 11                  ; [14] + 11
NoLaserColl2    
  SLEEP 8                   ; [25] + 8
NoLaserColl3 
  SLEEP 4                   ; [33] + 4
NoLaserColl4    
  SLEEP 6                   ; [37] + 6
  jmp NextLaserColl         ; [43] + 3
  
  ; Pad Out Starting Cycle Counts
LaserDetect1
  SLEEP 5                   ; [8] + 5
LaserDetect2
  lda LASERY                ; [13] + 3
  jmp LaserDetect           ; [16] + 3
  
ShipDetect1
  SLEEP 5                   ; [15] + 5
ShipDetect2
  ; Check Collisions Between Ship And Bullets
  lda CXPPMM                ; [20] + 3
  bpl BulletDetect          ; [23] + 2/3
  jmp ShipDetect            ; [25] + 3
EndLaserColl
  if (>WarpDetect != >EndLaserColl)
    echo "WARNING: Laser Collision Detection Crosses Page Boundary!"
  endif

BulletDetect
  ; Check Collisions Between Ship And Bullets
  lda CXM0P                 ; [26] + 3
  asl                       ; [29] + 2
  ora CXM1P                 ; [31] + 3
  and #%10000000            ; [34] + 2
  bne CheckBColl            ; [36] + 2/3
  SKIP_LINES 10             ; SKIP 11 LINES
  jmp EndBColl              ; SKIP FINAL SCANLINE
CheckBColl
  ; Pre-calculate Explosion Timer
  lda LIVES                 ; [39] + 3
  and #%00001111            ; [42] + 2
  ora #%10010000            ; [44] + 2
  tay                       ; [46] + 2
  ldx #BMAX2-1              ; [48] + 2 = 50
BCollLoop
  ; Skip Empty Bullet
  lda B1Y,X                 ; [0] + 4
  bmi NoBColl0              ; [4] + 2/3
  ; Check Y Overlap
  and #%00111111            ; [6] + 2
  asl                       ; [8] + 2
  ; sec
  sbc #18                   ; [10] + 2
  sbc #13                   ; [12] + 2
  bcs NoBColl1              ; [14] + 2/3
  ; Check X Overlap
  lda B1X,X                 ; [16] + 4
  asl                       ; [20] + 2
  clc                       ; [22] + 2
  sbc PLAYERX               ; [24] + 3
  sbc #7                    ; [27] + 2
  bcs NoBColl2              ; [29] + 2/3
  ; Explode Player
  sty.w LIVES               ; [31] + 4
NextBColl
  dex                       ; [35] + 2
  bpl BCollLoop             ; [37] + 3 = (40 * 7) + 39 = 319
                            ; TOTAL = 50 + 319 = 369 (4.9 SCANLINES)
  ; Skip Remaining Scanlines
  SKIP_LINES 6              ; SKIP 7 SCANLINES
  jmp EndBColl              ; SKIP FINAL SCANLINE
NoBColl0
  SLEEP 10                  ; [7] + 10
NoBColl1
  SLEEP 15                  ; [17] + 15
NoBColl2
  jmp NextBColl             ; [32] + 3
EndBColl
  sta WSYNC                 ; [0]
EndColl
  jmp StartScoreKernel      ; [0] + 3
EndBulletDetect
  if (>BulletDetect != >EndBulletDetect)
    echo "WARNING: Bullet Collision Detection Crosses Page Boundary!"
  endif

  DC.B  "PART1"

; -----------------------------------------------------------------------------
; PART 2 - ALIEN/SHIP/ASTRONAUT COLLISION DETECTION
; -----------------------------------------------------------------------------

  ALIGN   256

ShipDetect
  ; Pre-calculate Explosion Timer
  lda LIVES                 ; [28] + 3
  and #%00001111            ; [31] + 2
  ora #%10010000            ; [33] + 2
  sta TEMP                  ; [35] + 3
  ; Hyperspace Counter
  ldy #0                    ; [38] + 2
  ; Loop Through All Aliens
  ldx #INVMAX-1             ; [40] + 2 = 42
AlienCollLoop
  ; Check X Region Overlap
  clc                       ; [0] + 2
  lda INVX,X                ; [2] + 4
  sbc PLAYERX               ; [6] + 3
  sbc #6                    ; [9] + 2
  adc #13                   ; [11] + 2
  bcc NoAlienColl0          ; [13] + 2/3
  ; Check Y Region Overlap
  lda INVY,X                ; [15] + 4
  sbc #SHIPH                ; [19] + 2
  adc #20                   ; [21] + 2
  bcc NoAlienColl1          ; [23] + 2/3
  ; Skip Empty Aliens
  lda INVT,X                ; [25] + 4
  and #%00111100            ; [29] + 2
  beq NoAlienColl2          ; [31] + 2/3
  ; Check For Astronauts
  cmp #9<<2                 ; [33] + 2
  beq RemoveAstro           ; [35] + 2/3
  ; Skip Warps & Explosions
  bcs NoAlienColl3          ; [37] + 2/3
  ; Explode Player
  lda TEMP                  ; [39] + 3
  sta.w LIVES               ; [42] + 4
NextAlienColl
  dex                       ; [46] + 2
  bpl AlienCollLoop         ; [48] + 2/3 = (51 * 15) + 50 = 815
  bmi EndAlienColl          ; [0] + 3
RemoveAstro
  ; Remove Astronaut
  lda #0                    ; [38] + 2
  sta INVT,X                ; [40] + 4
  ; Increment Hyperspace Counter
  iny                       ; [44] + 2
  ; Continue Looping
  dex                       ; [46] + 2
  bpl AlienCollLoop         ; [48] + 2/3 = (51 * 15) + 50 = 815
  bmi EndAlienColl          ; [0] + 3
  ; Pad To Constant Cycle Count
NoAlienColl0
  SLEEP 10                  ; [16] + 10 
NoAlienColl1
  SLEEP 8                   ; [26] + 8
NoAlienColl2
  SLEEP 6                   ; [34] + 6
NoAlienColl3
  SLEEP 3                   ; [40] + 3
  jmp NextAlienColl         ; [43] + 3
NoHyperspace
  SLEEP 37                  ; [11] + 37
NoHyperSpeech
  SLEEP 4                   ; [48] + 4
  jmp EndColl               ; [52] + 3
EndAlienColl
  ; Check Hyperspace Counter
  cpy #0                    ; [3] + 2
  sty TEMP                  ; [5] + 3
  beq NoHyperspace          ; [8] + 2/3
EnterHyperspace
  ; Adjust Kills Counter
  ; sec
  lda KILLS                 ; [10] + 3
  sbc TEMP                  ; [13] + 3 
  sta KILLS                 ; [16] + 3
  ; Set Hyperspace Bit
  lda TIME                  ; [19] + 3
  ora #%10000000            ; [22] + 2
  sta TIME                  ; [24] + 3
  ; Set Hyperspace Timer
  lda #HYPERTIME            ; [27] + 2
  sta COUNTER              ; [29] + 3
  ; Reset Score Accumulator
  lda #0                    ; [32] + 2
  sta ACCUM                 ; [34] + 3
  ; Show Hyperspace Message
  lda #%01000100            ; [37] + 2
  sta MESSAGE               ; [39] + 3
  ; Hyperspace Speech
  lda SPEECH                ; [42] + 3
  bne NoHyperSpeech         ; [45] + 2/3
  lda #((1<<6)|43)          ; [47] + 2
  sta SPEECH                ; [49] + 3
  jmp EndColl               ; [52] + 3
                            ; TOTAL = 42 + 815 + 55 = 912 (12 SCANLINES EXACT!)
EndShipDetect
  if (>ShipDetect != >EndShipDetect)
    echo "WARNING: Ship Collision Detection Crosses Page Boundary!"
  endif

  ; Check For Collisions Between Ship & Astronaut (used during warping)
AstroDetect
  ldy #0                    ; [15] + 2
  ldx #INVMAX-1             ; [17] + 2 = 19
AstroCollLoop
  ; Check For Astronaut
  lda INVT,X                ; [0] + 4
  and #%00111100            ; [4] + 2
  cmp #9<<2                 ; [6] + 2
  bne NoAstroColl0          ; [8] + 2/3
  ; Check X Region Overlap
  clc                       ; [10] + 2
  lda INVX,X                ; [12] + 4
  sbc PLAYERX               ; [16] + 3
  sbc #6                    ; [19] + 2
  adc #13                   ; [21] + 2
  bcc NoAstroColl1          ; [23] + 2/3
  ; Check Y Region Overlap
  lda INVY,X                ; [25] + 4
  sbc #SHIPH                ; [29] + 2
  adc #20                   ; [31] + 2
  bcc NoAstroColl2          ; [33] + 2/3
  ; Remova Astronaut
  lda #0                    ; [35] + 2
  sta INVT,X                ; [37] + 4
  iny                       ; [41] + 2
NextAstroColl
  dex                       ; [43] + 2
  bpl AstroCollLoop         ; [45] + 2/3
                            ; RUNTIME = 19 + (15*48) + 47 = 786 (10.3 SCANLINES)
  ; Pad To 11 Scanlines Exact
  sta WSYNC                 ; [0]
  ; Check For Hyperspace
  SLEEP 21                  ; [0] + 21
  jmp EndAlienColl          ; [0] + 3
                            ; RUNTIME = 21 + 55 = 76 (1 SCANLINE)
  ; Pad To Constant Cycle Count
NoAstroColl0
  SLEEP 15                  ; [11] + 15
NoAstroColl1
  SLEEP 10                  ; [26] + 10
NoAstroColl2
  SLEEP 4                   ; [36] + 4
  jmp NextAstroColl         ; [40] + 3
EndAstroDetect
  if (>AstroDetect != >EndAstroDetect)
    echo "WARNING: Astro Detection Crosses Page Boundary!"
  endif
  
  DC.B  "PART2"

; -----------------------------------------------------------------------------
; PART 3 - DISPLAY GAME SCORE/BONUS
; -----------------------------------------------------------------------------
 
  ALIGN   256
 
StartScoreKernel
  ; Set Iterations
  lda #1                    ; [3] + 2
  sta TEMP2                 ; [5] + 3

  ; Check For Wave/Accumulator Messages
  lda MESSAGE               ; [8] + 3
  and #%00000111            ; [11] + 2
  cmp #4                    ; [13] + 2
  bcc ShowScore0            ; [15] + 2/3

ShowAccumulator
  lax ACCUM                 ; [17] + 3
  beq ShowScore1            ; [20] + 2/3
  ; Show Accumulator
  lsr                       ; [22] + 2
  lsr                       ; [24] + 2
  lsr                       ; [26] + 2
  lsr                       ; [28] + 2
  bne UpperDigit            ; [30] + 2/3
  lda #<___                 ; [32] + 2
  sta INVS+4                ; [34] + 3
  nop                       ; [37] + 2
  jmp EndUpperDigit         ; [39] + 3
UpperDigit
  tay                       ; [33] + 2
  lda Digit,Y               ; [35] + 4
  sta INVS+4                ; [39] + 3
EndUpperDigit
  txa                       ; [42] + 2
  and #%00001111            ; [44] + 2
  tay                       ; [46] + 2
  lda Digit,Y               ; [48] + 4
  sta INVS+6                ; [52] + 3
  ; Lower Digits Always Zero
  lda #<Zero                ; [55] + 2
  sta INVS+8                ; [57] + 3
  sta INVS+10               ; [60] + 3
  ; Plus Symbol
  lda #<_PLUS               ; [63] + 2
  sta INVS+0                ; [65] + 3
  lda #<___                 ; [68] + 2
  sta INVS+2                ; [70] + 3
  ; Show Blue Fade
  lda CYCLE                 ; [73] + 3
  lsr                       ; [0] + 2 
  lsr                       ; [2] + 2
  and #%00001111            ; [4] + 2 
  tax                       ; [6] + 2
  lda BlueFade,X            ; [8] + 4
  ldy #4                    ; [12] + 2
  bne ScoreKernel           ; [14] + 3 = 17

  ; Prepare 6-Digit Score
ShowScore0
  SLEEP 5                   ; [18] + 5
ShowScore1
  ; Digits 5-4
  lax SCORE+2               ; [23] + 3
  lsr                       ; [26] + 2
  lsr                       ; [28] + 2
  lsr                       ; [30] + 2
  lsr                       ; [32] + 2
  tay                       ; [34] + 2
  lda Digit,Y               ; [36] + 4
  sta INVS+0                ; [40] + 3
  txa                       ; [43] + 2
  and #%00001111            ; [45] + 2
  tay                       ; [47] + 2
  lda Digit,Y               ; [49] + 4
  sta INVS+2                ; [53] + 3
  ; Digits 3-2
  lax SCORE+1               ; [56] + 3
  lsr                       ; [59] + 2
  lsr                       ; [61] + 2
  lsr                       ; [63] + 2
  lsr                       ; [65] + 2
  tay                       ; [67] + 2
  lda Digit,Y               ; [69] + 4
  sta INVS+4                ; [73] + 3
  txa                       ; [0] + 2
  and #%00001111            ; [2] + 2
  tay                       ; [4] + 2
  lda Digit,Y               ; [6] + 4
  sta INVS+6                ; [10] + 3
  ; Digits 1-0
  lax SCORE+0               ; [13] + 3
  lsr                       ; [16] + 2
  lsr                       ; [18] + 2
  lsr                       ; [20] + 2
  lsr                       ; [22] + 2
  tay                       ; [24] + 2
  lda Digit,Y               ; [26] + 4
  sta INVS+8                ; [30] + 3
  txa                       ; [33] + 2
  and #%00001111            ; [35] + 2
  tay                       ; [37] + 2
  lda Digit,Y               ; [39] + 4
  sta INVS+10               ; [43] + 3
  ; Set Score Colour/Lines
  IF (PALCOLS)
  lda #$0E                  ; [46] + 2
  ELSE
  lda #$0E                  ; [46] + 2
  ENDIF
  ldy #4                    ; [48] + 2 = 50

   ; Display 48-Pixel (6 Sprite) Score
ScoreKernel
  ; Set Score Colour
  sta COLUP0                ; [50] + 3
  sta COLUP1                ; [53] + 3
  ; Store Line Counter
  sty LINE                  ; [56] + 3
  ; Load First Digit
  lda (INVS+0),Y            ; [59] + 5
  sta GRP0                  ; [64] + 3 
  sta WSYNC                 ; [0]
  jmp StartScore            ; [0] + 3
ScoreLoop
  ; Fetch Current Line
  ldy LINE                  ; [62] + 3
  SLEEP 6                   ; [65] + 6
  ; Display First 3 Digits
  lda (INVS+0),Y            ; [71] + 5
  sta GRP0                  ; [0] + 3    > 54
StartScore    
  lda (INVS+2),Y            ; [3] + 5
  sta GRP1                  ; [8] + 3    < 42
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
  bpl ScoreLoop             ; [59] + 2/3
  
  ; Clear Sprite Data
  ldx #0                    ; [61] + 2
  stx GRP1                  ; [63] + 3
  stx GRP0                  ; [66] + 3
  stx GRP1                  ; [69] + 3
  dec TEMP2                 ; [72] + 5
  beq MoveTable             ; [1] + 2/3

  ; Calculate Movement Tables
  jmp MoveTable2            ; [3] + 3
MoveTable
  jmp MoveTable1            ; [4] + 3
EndScoreKernel
  if (>StartScoreKernel != >EndScoreKernel)
    echo "WARNING: Score Kernel Crosses Page Boundary!"
  endif

  DC.B  "PART3"

; -----------------------------------------------------------------------------
; PART 3 - DISPLAY LIVES/MESSAGES
; -----------------------------------------------------------------------------

  ALIGN   256

ShowLivesMessages
  ; Check Message Type
  lda MESSAGE               ; [0] + 3
  beq ShowLives             ; [3] + 2/3
  and #%00000111            ; [5] + 2
  cmp #5                    ; [7] + 2
  bcc ShowMessages          ; [9] + 2/3
  
ShowWave
  ; Load WAVE Message
  lda #<_WW                 ; [11] + 2
  sta INVS+0                ; [13] + 3
  lda #<_AA                 ; [16] + 2
  sta INVS+2                ; [18] + 3
  lda #<_VV                 ; [21] + 2
  sta INVS+4                ; [23] + 3
  lda #<_EE                 ; [26] + 2
  sta INVS+6                ; [28] + 3
  ; Convert WAVE Number to BCD Using Table
  lda WAVE                  ; [31] + 3
  and #%00111111            ; [34] + 2
  tay                       ; [36] + 2
  iny                       ; [38] + 2
  lax BCDTable,Y            ; [40] + 4
  lsr                       ; [44] + 2
  lsr                       ; [46] + 2
  lsr                       ; [48] + 2
  lsr                       ; [50] + 2
  tay                       ; [52] + 2
  lda Digit,Y               ; [54] + 4
  sta INVS+8                ; [58] + 3
  txa                       ; [61] + 2
  and #%00001111            ; [63] + 2
  tay                       ; [65] + 2
  lda Digit,Y               ; [67] + 4
  sta INVS+10               ; [71] + 3
  ; Set Message Colour and Lines
  lda CYCLE                 ; [74] + 3
  lsr                       ; [1] + 2 
  lsr                       ; [3] + 2
  and #%00001111            ; [5] + 2 
  tax                       ; [7] + 2
  lda GreenFade,X           ; [9] + 4
  ldy #4                    ; [13] + 2
  jmp ScoreKernel           ; [15] + 3 = 18

  ; Display Game Messages
ShowMessages
  asl                       ; [12] + 2
  asl                       ; [14] + 2
  asl                       ; [16] + 2
  tax                       ; [18] + 2
  lda GameMessages+0,X      ; [20] + 4
  sta INVS+0                ; [24] + 3
  lda GameMessages+1,X      ; [27] + 4
  sta INVS+2                ; [31] + 3
  lda GameMessages+2,X      ; [34] + 4
  sta INVS+4                ; [38] + 3
  lda GameMessages+3,X      ; [41] + 4
  sta INVS+6                ; [45] + 3
  lda GameMessages+4,X      ; [48] + 4
  sta INVS+8                ; [52] + 3
  lda GameMessages+5,X      ; [55] + 4
  sta INVS+10               ; [59] + 3
  SLEEP 12                  ; [62] + 12
  lda CYCLE                 ; [74] + 3
  lsr                       ; [1] + 2 
  lsr                       ; [3] + 2
  and #%00001111            ; [5] + 2 
  tax                       ; [7] + 2
  lda RedFade,X           ; [9] + 4
  ldy #4                    ; [13] + 2
  jmp ScoreKernel           ; [15] + 3 = 18
  
ShowLives
  ; Convert Lives To Sprite Data
  sta WSYNC                 ; [0]
  lda LIVES                 ; [0] + 3
  and #%00000111            ; [3] + 2
  tax                       ; [5] + 2
  lda LifeBits,X            ; [7] + 4
  ldx #10                   ; [11] + 2 = 13
LivesLoop
  lsr                       ; [0] + 2
  bcc EmptyLife             ; [2] + 2/3
  ldy #<Life                ; [4] + 2
  jmp StoreLife             ; [6] + 3
EmptyLife
  ldy #<NoLife              ; [5] + 2
  nop                       ; [7] + 2
StoreLife
  sty INVS,X                ; [9] + 4
  dex                       ; [13] + 2
  dex                       ; [15] + 2
  bpl LivesLoop             ; [17] + 2/3 = (20*5) + 19 = 119
  ; Set Lives Colour/Lines
  IF (PALCOLS)
  lda #$64                  ; [56] + 2
  ELSE
  lda #$44                  ; [58] + 2
  ENDIF
  ldy #2                    ; [60] + 2
  jmp ScoreKernel           ; [62] + 3 = 65
EndShowLivesMessages
  if (>ShowLivesMessages != >EndShowLivesMessages)
    echo "WARNING: ShowLivesMessages Crosses Page Boundary!"
  endif

    ; Messages (Padded to 8 Bytes)
  ; 0 = None
  ; 1 = GAME OVER
  ; 2 = FUEL LOW
  ; 3 = BONUS SHIP
  ; 4 = HYPERSPACE + ACCUMULATOR
  ; 5 = WAVE NUMBER + TIME BONUS
GameMessages
  DC.B  0, 0, 0, 0, 0, 0, 0, 0
  DC.B  <__G, <_AM, <_E_, <__O, <_VE, <_R_, 0, 0
  DC.B  <___, <_FU, <_EL, <__L, <_OW, <___, 0, 0
  DC.B  <__B, <_ON, <_US, <__S, <_HI, <_P_, 0, 0
  DC.B  <__H, <_YP, <_ER, <_SP, <_AC, <_E_, 0, 0
EndGameMessages
  if (>GameMessages != >EndGameMessages)
    echo "WARNING: Game Messages Cross Page Boundary!"
  endif

LifeBits
  ; Table Of Positions For Lives and Warp Indicators (8 Entries)
  DC.B  %00000000
  DC.B  %00000001
  DC.B  %00000011
  DC.B  %00000111
  DC.B  %00001111
  DC.B  %00011111
  DC.B  %00111111
  DC.B  %00111111
EndLifeBits
  if (>LifeBits != >EndLifeBits)
    echo "WARNING: LifeBits Table Crosses Page Boundary!"
  endif 

  ; Alien Scores (BCD)
  ; 0 = No Score
  ; 1 = 150
  ; 2 = 300
  ; 3 = 500
ScoreHi
  DC.B  %00000000
  DC.B  %00000001
  DC.B  %00000011
  DC.B  %00000101
ScoreLo   
  DC.B  %00000000
  DC.B  %01010000
  DC.B  %00000000
  DC.B  %00000000
EndScoreLo
  if (>ScoreHi != >EndScoreLo)
    echo "WARNING: ScoreHi/Lo Table Crosses Page Boundary!"
  endif 

; -----------------------------------------------------------------------------
; PART 5 - ALIEN/BULLET MOTION CALCULATIONS
; -----------------------------------------------------------------------------

  ALIGN   256

  ; Alien Sprite Behaviour Table (64 Entries)
  ; 4 Bytes Per Sprite Type:
  ; 0 - Next Animation Frame
  ; 1 - Movement Function (LSB)
  ; 2 - Firing Function (LSB)
  ; 3 - Score Value (see ScoreLo/Hi)
AFrame
  DC.B  0<<2                ; 0     0<<2    No Alien
MoveFn
  DC.B  #<NoMove            ; 1
FireFn
  DC.B  #<NoFire            ; 2
Scores
  DC.B  0|15<<2             ; 3
  DC.B  7<<2                ; 4     1<<2    Alien1
  DC.B  #<MoveLR            ; 5
  DC.B  #<Alien1Fire        ; 6
  DC.B  1|15<<2             ; 7
  DC.B  8<<2                ; 8     2<<2    Alien2
  DC.B  #<MoveSwirl         ; 9
  DC.B  #<Alien2Fire        ; 10
  DC.B  2|15<<2             ; 11
  DC.B  3<<2                ; 12    3<<2    BugAlien
  DC.B  #<MoveSwirl2        ; 13
  DC.B  #<BugAlienFire      ; 14
  DC.B  1|15<<2             ; 15
  DC.B  4<<2                ; 16    4<<2    Sphere
  DC.B  #<MoveFastLR        ; 17
  DC.B  #<NoFire            ; 18
  DC.B  #%01101010          ; 19    SPHERE CHANGES TO ASTRONAUT
  DC.B  5<<2                ; 20    5<<2    BigUFO
  DC.B  #<MoveUltraLR       ; 21
  DC.B  #<BigUFOFire        ; 22
  DC.B  3|15<<2             ; 23
  DC.B  6<<2                ; 24    6<<2    SmallUFO
  DC.B  #<MoveFastLR        ; 25
  DC.B  #<SmallUFOFire      ; 26
  DC.B  2|15<<2             ; 27
  DC.B  1<<2                ; 28    7<<2    Alien1 (Frame2)
  DC.B  #<MoveLR            ; 29
  DC.B  #<Alien1Fire        ; 30
  DC.B  1|15<<2             ; 31
  DC.B  2<<2                ; 32    8<<2    Alien2 (Frame2)
  DC.B  #<MoveSwirl         ; 33
  DC.B  #<Alien2Fire        ; 34
  DC.B  2|15<<2             ; 35
  DC.B  9<<2                ; 36    9<<2    Astronaut
  DC.B  #<MoveAstroBack     ; 37
  DC.B  #<NoFire            ; 38
  DC.B  0|15<<2             ; 39
  DC.B  10<<2               ; 40    10<<2    Warp1
  DC.B  #<NoMove            ; 41
  DC.B  #<NoFire            ; 42
  DC.B  0|15<<2             ; 43
  DC.B  10<<2               ; 44    11<<2    Warp2
  DC.B  #<NoMove            ; 45
  DC.B  #<NoFire            ; 46
  DC.B  0|15<<2             ; 47
  DC.B  11<<2               ; 48    12<<2    Warp3
  DC.B  #<NoMove            ; 49
  DC.B  #<NoFire            ; 50
  DC.B  0|15<<2             ; 51
  DC.B  13<<2               ; 52    13<<2    Explosion1
  DC.B  #<NoMove            ; 53
  DC.B  #<NoFire            ; 54
  DC.B  0|15<<2             ; 55
  DC.B  13<<2               ; 56    14<<2    Explosion2
  DC.B  #<NoMove            ; 57
  DC.B  #<NoFire            ; 58
  DC.B  0|15<<2             ; 59
  DC.B  14<<2               ; 60    15<<2    Explosion3
  DC.B  #<NoMove            ; 61
  DC.B  #<NoFire            ; 62
  DC.B  0|15<<2             ; 63
EndATable
  if (>AFrame != >EndATable)
    echo "WARNING: Alien Behaviour Table Crosses Page Boundary!"
  endif 

  ; Generate Movement Table For Aliens/Bullets
MoveTable1
  ; Set Alien Line Start (START/4)
  lda START                 ; [0] + 3
  lsr                       ; [3] + 2
  lsr                       ; [5] + 2
  sta TEMP                  ; [7] + 3
  ; Calculate New Position (START+SPEED)
  lda SPEED                 ; [10] + 3
  bmi SubSpeed              ; [13] + 2/3
AddSpeed
  lsr                       ; [15] + 2
  lsr                       ; [17] + 2
  clc                       ; [19] + 2
  adc START                 ; [21] + 3
  SLEEP 10                  ; [24] + 10
  jmp FinishSpeed           ; [34] + 3
SubSpeed
  ; Negation
  clc                       ; [16] + 2
  eor #$FF                  ; [18] + 2
  adc #1                    ; [20] + 2
  lsr                       ; [22] + 2
  lsr                       ; [24] + 2
  sta DVD                   ; [26] + 3
  sec                       ; [29] + 2
  lda START                 ; [31] + 3
  sbc DVD                   ; [34] + 3
FinishSpeed
  sta START                 ; [37] + 3
  ; Set Alien Line End ((START+SPEED)/4)
  lsr                       ; [40] + 2
  lsr                       ; [42] + 2
  sta DVD                   ; [44] + 3
  ; Calculate Alien Movement Offsets
  lda SPEED                 ; [47] + 3
  bmi BackwardAlien         ; [50] + 2/3
ForwardAlien
  lda DVD                   ; [52] + 3
  cmp.w TEMP                ; [55] + 4
  bcs NormalLoop            ; [59] + 2/3
  sec                       ; [0] + 2
  lda DVD                   ; [2] + 3
  sbc #64                   ; [5] + 2
  clc                       ; [7] + 2
  adc TEMP                  ; [9] + 3
  sta DVDTAB+4              ; [12] + 3
  lsr DVD                   ; [15] + 5
  lsr TEMP                  ; [20] + 5 = 25
  sec                       ; [0] + 2
  lda DVD                   ; [2] + 3
  sbc #32                   ; [5] + 2
  clc                       ; [7] + 2
  adc TEMP                  ; [9] + 3
  sta DVDTAB+3              ; [12] + 3
  lsr DVD                   ; [15] + 5
  lsr TEMP                  ; [20] + 5 = 25
  sec                       ; [0] + 2
  lda DVD                   ; [2] + 3
  sbc #16                   ; [5] + 2
  clc                       ; [7] + 2
  adc TEMP                  ; [9] + 3
  sta DVDTAB+2              ; [12] + 3
  lsr DVD                   ; [15] + 5
  lsr TEMP                  ; [20] + 5 = 25
  sec                       ; [0] + 2
  lda DVD                   ; [2] + 3
  sbc #8                    ; [5] + 2
  clc                       ; [7] + 2
  adc TEMP                  ; [9] + 3
  sta DVDTAB+1              ; [12] + 3
  lsr DVD                   ; [15] + 5
  lsr TEMP                  ; [20] + 5 = 25
  jmp EndDiv                ; 61 + (25*4) + 3 = 164
BackwardAlien
  lda TEMP                  ; [53] + 3
  cmp DVD                   ; [56] + 3
  bcs NormalLoop            ; [59] + 2/3
  sec                       ; [0] + 2
  lda #64                   ; [2] + 2
  sbc DVD                   ; [4] + 3
  clc                       ; [7] + 2
  adc TEMP                  ; [9] + 3
  sta DVDTAB+4              ; [12] + 3
  lsr DVD                   ; [15] + 5
  lsr TEMP                  ; [20] + 5 = 25
  sec                       ; [0] + 2
  lda #32                   ; [2] + 2
  sbc DVD                   ; [4] + 3
  clc                       ; [7] + 2
  adc TEMP                  ; [9] + 3
  sta DVDTAB+3              ; [12] + 3
  lsr DVD                   ; [15] + 5
  lsr TEMP                  ; [20] + 5 = 25
  sec                       ; [0] + 2
  lda #16                   ; [2] + 2
  sbc DVD                   ; [4] + 3
  clc                       ; [7] + 2
  adc TEMP                  ; [9] + 3
  sta DVDTAB+2              ; [12] + 3
  lsr DVD                   ; [15] + 5
  lsr TEMP                  ; [20] + 5 = 25
  sec                       ; [0] + 2
  lda #8                    ; [2] + 2
  sbc DVD                   ; [4] + 3
  clc                       ; [7] + 2
  adc TEMP                  ; [9] + 3
  sta DVDTAB+1              ; [12] + 3
  lsr DVD                   ; [15] + 5
  lsr TEMP                  ; [20] + 5 = 25
  jmp EndDiv                ; 61 + (25*4) + 3 = 164
NormalLoop
  sec                       ; [0] + 2
  lda TEMP                  ; [2] + 3
  sbc DVD                   ; [5] + 3
  sta DVDTAB+4              ; [8] + 3
  lsr DVD                   ; [11] + 5
  lsr TEMP                  ; [16] + 5 = 21
  sec                       ; [0] + 2
  lda TEMP                  ; [2] + 3
  sbc DVD                   ; [5] + 3
  sta DVDTAB+3              ; [8] + 3
  lsr DVD                   ; [11] + 5
  lsr TEMP                  ; [16] + 5 = 21
  sec                       ; [0] + 2
  lda TEMP                  ; [2] + 3
  sbc DVD                   ; [5] + 3
  sta DVDTAB+2              ; [8] + 3
  lsr DVD                   ; [11] + 5
  lsr TEMP                  ; [16] + 5 = 21
  sec                       ; [0] + 2
  lda TEMP                  ; [2] + 3
  sbc DVD                   ; [5] + 3
  sta DVDTAB+1              ; [8] + 3
  lsr DVD                   ; [11] + 5
  lsr TEMP                  ; [16] + 5 = 21
  SLEEP 18                  ; 62 + (21*4) + 18 = 164
EndDiv
  lda #0                    ; [164] + 2
  sta DVDTAB+0              ; [166] + 3 
                            ; RUNTIME = 169 (2.2 SCANLINES)

  ; Move All Aliens (Unrolled)
AINDEX    SET INVMAX-1
  REPEAT INVMAX
  MOVEALIEN AINDEX          ; [0] + 16
AINDEX    SET AINDEX-1
  REPEND                    ; RUNTIME = 16 * 16 = 256

  ; Move All Bullets (Unrolled)
BINDEX    SET BMAX2-1
  REPEAT BMAX2
  MOVEBULLET BINDEX         ; [0] + 16
BINDEX    SET BINDEX-1
  REPEND                    ; RUNTIME = 8 * 16 = 128
                            ; TOTAL = 256 + 128 = 384 (5.1 SCANLINES)

  ; Display Lives & Messages
  jmp ShowLivesMessages

MoveTable2
  ; Calculate Wave Pointer (into WaveTable/SpawnTable)
  lda WAVE                  ; [0] + 3
  and #%00111111            ; [3] + 2
  asl                       ; [5] + 2
  tay                       ; [7] + 2
  ; Divide Counter By 2
  lax MOTION              ; [9] + 3
  lsr                       ; [12] + 2
  lsr                       ; [14] + 2
  sta TEMP                  ; [16] + 3
  ; Load Alien Speed
  clc                       ; [19] + 2
  txa                       ; [21] + 2
  adc WaveTable+1,Y         ; [23] + 4
  sta MOTION              ; [27] + 3 = 30
  
  ; Construct Movement Table For Aliens & Bullets
  lsr                       ; [0] + 2
  lsr                       ; [2] + 2
  sta DVD                   ; [4] + 3
  cmp TEMP                  ; [7] + 3
  bcc ADivLoop3             ; [10] + 2/3 = 12/13
DivLoop3
  sec                       ; [0] + 2
  lda DVD                   ; [2] + 3
  sbc TEMP                  ; [5] + 3
  sta DVDTAB+4              ; [8] + 3
  lsr DVD                   ; [11] + 5
  lsr TEMP                  ; [16] + 5 = 21
  sec                       ; [0] + 2
  lda DVD                   ; [2] + 3
  sbc TEMP                  ; [5] + 3
  sta DVDTAB+3              ; [8] + 3
  lsr DVD                   ; [11] + 5
  lsr TEMP                  ; [16] + 5 = 21
  sec                       ; [0] + 2
  lda DVD                   ; [2] + 3
  sbc TEMP                  ; [5] + 3
  sta DVDTAB+2              ; [8] + 3
  lsr DVD                   ; [11] + 5
  lsr TEMP                  ; [16] + 5 = 21
  sec                       ; [0] + 2
  lda DVD                   ; [2] + 3
  sbc TEMP                  ; [5] + 3
  sta DVDTAB+1              ; [8] + 3
  lsr DVD                   ; [11] + 5
  lsr TEMP                  ; [16] + 5 = 21
  sec                       ; [0] + 2
  lda DVD                   ; [2] + 3
  sbc TEMP                  ; [5] + 3
  sta DVDTAB+0              ; [8] + 3 = 11
  SLEEP 8                   ; [0] + 8
  jmp EndDivLoop3           ; [8] + 3 = 11 
                            ; RUNTIME = 12 + (21 * 4) + 11 + 11 = 118
ADivLoop3
  sec                       ; [0] + 2
  lda #64                   ; [2] + 2
  sbc TEMP                  ; [4] + 3
  ; clc (Carry Always Clear Here)
  adc DVD                   ; [7] + 3
  sta DVDTAB+4              ; [10] + 3
  lsr DVD                   ; [13] + 5
  lsr TEMP                  ; [18] + 5 = 23
  sec                       ; [0] + 2
  lda #32                   ; [2] + 2
  sbc TEMP                  ; [4] + 3
  ; clc (Carry Always Clear Here)
  adc DVD                   ; [7] + 3
  sta DVDTAB+3              ; [10] + 3
  lsr DVD                   ; [13] + 5
  lsr TEMP                  ; [18] + 5 = 23
  sec                       ; [0] + 2
  lda #16                   ; [2] + 2
  sbc TEMP                  ; [4] + 3
  ; clc (Carry Always Clear Here)
  adc DVD                   ; [7] + 3
  sta DVDTAB+2              ; [10] + 3
  lsr DVD                   ; [13] + 5
  lsr TEMP                  ; [18] + 5 = 23
  sec                       ; [0] + 2
  lda #8                    ; [2] + 2
  sbc TEMP                  ; [4] + 3
  ; clc (Carry Always Clear Here)
  adc DVD                   ; [7] + 3
  sta DVDTAB+1              ; [10] + 3
  lsr DVD                   ; [13] + 5
  lsr TEMP                  ; [18] + 5 = 23
  sec                       ; [0] + 2
  lda #4                    ; [2] + 2
  sbc TEMP                  ; [4] + 3
  ; clc (Carry Always Clear Here)
  adc DVD                   ; [7] + 3
  sta DVDTAB+0              ; [10] + 3
EndDivLoop3                 ; RUNTIME = 13 + (23 * 4) + 13 = 118 (1.6 SCANLINES)

; -----------------------------------------------------------------------------
; PART 6 - FOUR PHASE GAME LOGIC 
; -----------------------------------------------------------------------------

  ; Start Overscan
  START_OVERSCAN

GameLogic
  ; Game Logic Is Done In 4 Phases (Y Contains WaveTable/SpawnTable Pointer)
  lda CYCLE
  lsr
  bcc PhaseB
  lsr
  bcc Phase1

; -----------------------------------------------------------------------------
; PHASE 0 - MOVE ALIENS ACCORDING TO TYPE
; -----------------------------------------------------------------------------

Phase0
  ; Set Movement Function MSB
  lda #>NoMove
  sta SPTR+1
  ; Store Hyperspace/Astronaut Bits
  lda TIME
  and #%11000000
  sta TEMP2
  ; Loop Through Aliens
  ldx #INVMAX-1
MotionLoop
  ; Check Empty Type
  lda INVT,X                ; [0] + 4
  and #%00111100            ; [4] + 2
  beq NextMotion            ; [6] + 2/3
  ; Don't Move Explosions/Warps
  cmp #10<<2                ; [8] + 2
  bcs NextMotion            ; [10] + 2/3
  tay                       ; [12] + 2
  lda MoveFn,Y              ; [14] + 4
  sta SPTR                  ; [18] + 3
  ; Jump To Movement Function
  jmp (SPTR)                ; [21] + 5
                            ; [26] + 64     Worst Case
EndMotion
  ; Check If In Hyperspace
  lda TEMP2                 ; [90] + 3
  bpl NextMotion            ; [93] + 2/3
  ; Move Aliens Back
  ldy INVY,X                ; [95] + 4
  lda Movement,Y            ; [99] + 4
  tay                       ; [103] + 2
  lda DVDTAB,Y              ; [105] + 4
  asl                       ; [109] + 2     Double Speed
  clc                       ; [111] + 2
  adc INVY,X                ; [113] + 4
  sta INVY,X                ; [117] + 4
NextMotion
  dex                       ; [121] + 2
  bpl MotionLoop            ; [123] + 3 = 126
  jmp EndPhase              ; WORST CASE = 16 * 126 = 2016 (27 SCANLINES) 

; Phase B (Split Into Phase 2 & 3)
PhaseB
  lsr
  bcs Phase2
  jmp Phase3

; -----------------------------------------------------------------------------
; PHASE 1 - COUNTER & FIRE BULLETS
; -----------------------------------------------------------------------------

Phase1
  ; Check Every 16th Frame
  lsr                       ; [0] + 2
  bcc EndDebounce           ; [2] + 2/3
  lsr
  bcc EndDebounce
  ; Check Debounce Counter
  lda COUNTER              ; [4] + 3
  and #%00011111            ; [7] + 2
  beq EndDebounce           ; [9] + 2/3
  ; Decrement Counter
  tax                       ; [11] + 2
  dex                       ; [13] + 2
  stx TEMP                  ; [15] + 3
  ; Merge With Shot Counter
  lda COUNTER              ; [18] + 3
  and #%11100000            ; [21] + 2
  ora TEMP                  ; [23] + 3
  sta COUNTER              ; [26] + 3
EndDebounce                 ; WORST CASE = 29 CYCLES

  ; Don't Fire While Paused
  bit WAVE
  bmi NoNewFire
  ; Don't Fire In Hyperspace
  bit TIME
  bpl FireBullets
NoNewFire
  jmp EndPhase
  
FireBullets
  ; Set Firing Function MSB
  lda #>NoFire
  sta SPTR+1

  ; Find Available Slots For B1 ($FF = No Space)
  ldx #BMAX
FindB1Loop
  dex                       ; [0] + 2
  bmi EndFindB1Loop         ; [2] + 2
  lda B1Y,X                 ; [4] + 4
  bpl FindB1Loop            ; [8] + 3 = 11*4
EndFindB1Loop
  stx B1

  ; Find Available Slots For B2 ($FF = No Space)
  ldx #BMAX
FindB2Loop
  dex
  bmi EndFindB2Loop
  lda B2Y,X
  bpl FindB2Loop
EndFindB2Loop
  stx B2

  ; Find First Valid Alien To Fire (Maximum Of 1 New Bullet)
  ldx #INVMAX-1
FireLoop
  ; Check Alien Exists
  lda INVT,X                ; [0] + 4
  and #%00111100            ; [4] + 2
  beq NextFire              ; [6] + 2
  tay                       ; [8] + 2
  ; Check Alien Visible
  lda INVY,X                ; [10] + 4
  bmi NextFire              ; [14] + 2
  lda FireFn,Y              ; [16] + 4
  sta SPTR                  ; [20] + 3
  jmp (SPTR)                ; [23] + 8  (Only NoFire Returns Here!)
NextFire
  dex                       ; [31] + 2
  bpl FireLoop              ; [33] + 2/3
EndBulletFire               ; WORST CASE = 7*36 + 35 = 287 (4 SCANLINES)
  jmp EndPhase

; -----------------------------------------------------------------------------
; PHASE 2 - SPAWN ALIENS & MOVE BULLETS
; -----------------------------------------------------------------------------

Phase2
  ; Spawn New Aliens (Handled In Bank 5)
  jmp SpawnAliens           ; WORST CASE ~ 5 SCANLINES
FinishSpawn

  ; Calculate Ship Position for Homing Bullets
  clc                       ; [0] + 2
  lda PLAYERX               ; [2] + 3
  adc #4                    ; [5] + 2
  sta TEMP                  ; [7] + 3
  ldx #BMAX-1               ; [10] + 2 = 12
  ; Loop Through Homing Bullets
HomingLoop
  ; Check If Bullet is Visible
  ldy B2Y,X                 ; [0] + 4
  bmi NextHoming            ; [4] + 2/3
  ; Compare to Ship Position and Adjust Direction
  lda B2X,X                 ; [6] + 4
  asl                       ; [10] + 2
  cmp TEMP                  ; [12] + 3
  ror                       ; [15] + 2
  sta B2X,X                 ; [17] + 4
NextHoming            
  dex                       ; [21] + 2
  bpl HomingLoop            ; [23] + 2/3
                            ; WORST CASE = 12+(4*26) = 116 (1.5 SCANLINES)

  ; Move All Bullets
  ldx #BMAX2-1
BulletMoveLoop
  ; Fetch Bullet Y Position
  lda B1Y,X                 ; [0] + 4
  ; Check Bullet Visible (Bit 7)    
  bmi NextBulletMove        ; [4] + 2/3
MoveBulletForward
  ; Decrease Bullet Position By Movement Table
  txs                       ; [6] + 2
  tay                       ; [8] + 2
  ldx Movement,Y            ; [10] + 4
  sec                       ; [14] + 2
  sbc DVDTAB,X              ; [16] + 4
  ; Check Bullet Is Still Visible
  bmi HideBullet2           ; [20] + 2/3
  tsx                       ; [22] + 2
  ; Store Bullet Y Position
  sta B1Y,X                 ; [24] + 4
MoveBulletHoriz
  ; Fetch Bullet X Position
  lda B1X,X                 ; [28] + 4
  ; Check Bullet Direction (Bit 7)
  bmi MoveBulletLeft        ; [32] + 3
MoveBulletRight
  ; Increase Bullet Position
  clc
  adc #1
  cmp #BXMAX
  bcc StoreBulletX
HideBullet
  ; Set Invisible Bullet
  lda #$FF
  sta B1Y,X
  bne NextBulletMove
HideBullet2
  tsx
  ; Set Invisible Bullet
  lda #$FF
  sta B1Y,X
  bne NextBulletMove
MoveBulletLeft
  ; Mask Direction Bit
  and #%01111111            ; [35] + 2
  ; Decrease Bullet Position
  sec                       ; [37] + 2 
  sbc #1                    ; [39] + 2
  bmi HideBullet            ; [41] + 2
  ; Restore Direction Bit
  ora #%10000000            ; [43] + 2
StoreBulletX
  sta B1X,X                 ; [45] + 4
NextBulletMove
  dex                       ; [49] + 2
  bpl BulletMoveLoop        ; [51] + 3 = 54
                            ; WORST CASE = 8 * 54 = 432 (6 SCANLINES)
  jmp EndPhase

; -----------------------------------------------------------------------------
; PHASE 3 - PLAYER & ALIEN ANIMATIONS
; -----------------------------------------------------------------------------

Phase3
  ; Animate Player/Aliens On Alternate Cycles
  lsr
  bcc PlayerAnim
  jmp AlienAnim

  ; Animate Player (Explosion/Appear Effects)
PlayerAnim
  ; No Animation If Paused
  lda WAVE                  ; [0] + 3
  bmi NoPlayerAnim          ; [3] + 2/3
  ; Check If Dying/Dead
  lda LIVES                 ; [5] + 3
  and #%11110000            ; [8] + 2
  beq NoPlayerAnim          ; [10] + 2/3
  bpl PlayerAppear          ; [12] + 2/3
  cmp #%11010000            ; [14] + 2
  bcc PlayerExplosion       ; [16] + 2/3
EndDeathSequence
  ; Pause Game
  lda WAVE                  ; [18] + 3
  and #%00111111            ; [21] + 2
  ora #%10000000
  sta WAVE                  ; [23] + 3
  ; Clear Hyperpsace & Reset Timers
  lda #MAXTIME              ; [26] + 2
  sta TIME                  ; [28] + 3
  ; Decrease Lives
  lax LIVES                 ; [31] + 3
  and #%01111000            ; [34] + 2
  sta TEMP                  ; [36] + 3
  sec                       ; [39] + 2
  txa                       ; [41] + 2
  and #%00000111            ; [43] + 2
  sbc #1                    ; [45] + 2
  bmi GameOver              ; [47] + 2/3
  ora TEMP                  ; [49] + 3
  sta LIVES                 ; [52] + 3
  ; Set Warp & Clear Shot Counter
  lda #DEADTIME             ; [55] + 2
  sta COUNTER              ; [57] + 3
  ; Move Ship To Middle
  lda #XMIDDLE              ; [60] + 2
  sta PLAYERX               ; [62] + 3
  ; Clear Messages
  lda #0                    ; [65] + 2
  beq StorePlayerMessage    ; [67] + 3
  
GameOver
  ; Pause Game
  lda WAVE                  ; [50] + 3
  ora #%10000000            ; [53] + 2
  sta WAVE                  ; [55] + 3
  ; Set Dead Bit
  lda #%10000000            ; [58] + 2
  sta LIVES                 ; [60] + 3
  ; Clear Shot Counter & Set Delay
  lda #KEYWAIT              ; [63] + 2
  sta DEBOUNCE              ; [65] + 3
  ; Display Game Over Message
  lda #%00000001            ; [68] + 2
StorePlayerMessage
  sta MESSAGE               ; [70] + 3
  jmp EndPhase              ; [73] + 3 = 76

NoPlayerAnim
  jmp CheckWaveEnd

PlayerAppear
  ; Update Animation Frame
  sec                       ; [15] + 2
  sbc #16                   ; [17] + 2
  and #%01110000            ; [19] + 2
  sta TEMP                  ; [21] + 3
  ; Appear Sound Effect
  cmp #%00110000            ; [24] + 2
  bne StorePlayer           ; [26] + 2/3
  lda SND0                  ; [28] + 3
  beq PlayPlayerAppear      ; [31] + 2/3
  and #%11100000            ; [33] + 2
  cmp #2<<5                 ; [35] + 2
  beq StorePlayer           ; [37] + 2/3
  bcc StorePlayer           ; [39] + 2/3
PlayPlayerAppear
  lda #((2<<5)|31)          ; [41] + 2
  sta SND0                  ; [43] + 3
  jmp StorePlayer           ; [46] + 3

PlayerExplosion
  ; Increment Animation Frame
  ; clc
  adc #16                   ; [19] + 2
  ; and #%11110000
  sta TEMP                  ; [21] + 3
  ; Explode Sound Effect
  cmp #%10100000            ; [24] + 2
  bne StorePlayer           ; [26] + 2/3
  lda SND0                  ; [28] + 3
  beq PlayPlayerExplosion   ; [31] + 2/3
  and #%11100000            ; [33] + 2
  cmp #1<<5                 ; [35] + 2
  beq StorePlayer           ; [37] + 2/3
  bcc StorePlayer           ; [39] + 2/3
PlayPlayerExplosion
  lda #((1<<5)|31)          ; [41] + 2
  sta SND0                  ; [43] + 3
  
StorePlayer
  ; Merge Animation With Lives
  lda LIVES                 ; [49] + 3
  and #%00001111            ; [52] + 2
  ora TEMP                  ; [54] + 3
  sta LIVES                 ; [57] + 3

CheckWaveEnd
  lda KILLS                 ; [60] + 3
  bpl EndWaveCheck          ; [63] + 2/3
  ; Don't Advance If Paused
  lda WAVE                  ; [65] + 3
  bmi EndWaveCheck          ; [68] + 2/3
  ; Increment Wave Counter
  and #%00111111            ; [70] + 2
  clc                       ; [72] + 2
  adc #1                    ; [74] + 2
  and #%00111111            ; [76] + 2    Wrap After 64 Waves
  sta WAVE                  ; [78] + 3
  ; Set Alien Kill Counters
  asl                       ; [81] + 2
  tay                       ; [83] + 2
  lda WaveTable+0,Y         ; [85] + 4
  sta SPAWN                 ; [89] + 3
  sta KILLS                 ; [92] + 3
  ; Play New Wave Sound
  lda #((0<<5)|31)          ; [95] + 2
  sta SND0                  ; [97] + 3
  ; New Wave Speech
  lda SPEECH                ; [100] + 3
  bne SkipWaveSpeech        ; [103] + 2/3
  lda #((0<<6)|44)          ; [105] + 2
StoreWaveSpeech
  sta SPEECH                ; [107] + 3
SkipWaveSpeech
  ; Show WAVE Message
  lda #%01000101            ; [110] + 2
  sta MESSAGE               ; [112] + 3
  ; Time Bonus
  lda TIME                  ; [125] + 3
  and #%00111111            ; [118] + 2
  lsr                       ; [120] + 2
  lsr                       ; [122] + 2
  tay                       ; [124] + 2
  lda BCDTable,Y            ; [126] + 4
  sta ACCUM                 ; [130] + 3
  ; Add Bonus to Score
  sed                       ; [133] + 2
  clc                       ; [135] + 2
  adc SCORE+1               ; [137] + 3
  sta SCORE+1               ; [140] + 3
  bcc NoExtraLife0          ; [143] + 2/3
  lda SCORE+2               ; [145] + 3
  adc #0                    ; [148] + 2
  sta SCORE+2               ; [150] + 3
  and #%00001111            ; [153] + 2
  bne NoExtraLife0          ; [155] + 2/3
  ; Increment Lives
  cld                       ; [157] + 2
  lda LIVES                 ; [159] + 3
  and #%11111000            ; [162] + 2
  sta TEMP                  ; [164] + 3
  lda LIVES                 ; [167] + 3
  and #%00000111            ; [170] + 2
  cmp #MAXLIVES             ; [172] + 2
  bcs EndExtraLife0         ; [174] + 2/3
  adc #1                    ; [176] + 2
  ora TEMP                  ; [178] + 3
  sta LIVES                 ; [182] + 3
  jmp EndExtraLife0         ; [185] + 3
NoExtraLife0
  cld
EndExtraLife0
  ; Reset Counter If Previously In Hyperspace
  lda TIME                  ; [188] + 3 
  bpl NoResetHyperspace     ; [191] + 2/3
  lda #0                    ; [193] + 2
  sta COUNTER               ; [195] + 3
NoResetHyperspace
  ; Reset Timer & Clear Hyperspace
  lda #MAXTIME              ; [198] + 2
  sta TIME                  ; [200] + 3
  sta STIME                 ; [203] + 3
EndWaveCheck
  jmp EndPhase              ; [206] + 3 
                            ; WORST CASE = 209 CYCLES (2.75 SCANLINES)

  ; Animate Aliens
AlienAnim
  ; Clear Score Collector
  lda #0
  sta DVD
  ; Clear Astronaut Bit
  lda TIME
  and #%10111111
  sta TIME
  ; Loop Through All Aliens
  ldx #INVMAX-1
AnimLoop
  ; Check Invader Type
  lda INVT,X                ; [0] + 4
  and #%00111100            ; [4] + 2
  beq NextAnim              ; [6] + 2
  ; End Alien Explosion
  cmp #13<<2                ; [8] + 2
  beq EndExplosion          ; [10] + 2/3
  ; Alien Warps
  cmp #10<<2                ; [12] + 2
  beq EndWarp               ; [14] + 2/3
  ; Start Alien Explosion
  cmp #15<<2                ; [16] + 2
  beq StartExplosion        ; [18] + 2/3
  ; Astronaut
  cmp #9<<2                 ; [20] + 2
  beq AstroFound            ; [22] + 2/3
  ; Update Animation Frame
UpdateAnim
  tay                       ; [42] + 2
  lda INVT,X                ; [44] + 4
  and #%11000011            ; [48] + 2
  ora AFrame,Y              ; [50] + 4
  sta INVT,X                ; [54] + 4
NextAnim
  dex                       ; [132] + 2
  bpl AnimLoop              ; [134] + 3
  jmp EndPhase              ; WORST CASE = 137 * 16 = 2192 (29 SCANLINES)

  ; Play Explosion Sound Effect
StartExplosion
  lda SND0                  ; [21] + 3
  beq PlayAlienExplosion    ; [24] + 2/3
  and #%11100000            ; [26] + 2
  cmp #4<<5                 ; [28] + 2
  bcc EndPlayerExplosion    ; [30] + 2/3
PlayAlienExplosion
  lda #((4<<5)|11)          ; [32] + 2
  sta SND0                  ; [34] + 3
EndPlayerExplosion    
  lda #15<<2                ; [37] + 2
  bne UpdateAnim            ; [39] + 3  = 42

  ; Set Astronaut Found Bit
AstroFound
  lda TIME                  ; [21] + 3
  ora #%01000000            ; [24] + 2
  sta TIME                  ; [26] + 3
  bne NextAnim              ; [29] + 3  = 32
  
  ; End Warp Sequence (Substitute Warp Effect For Actual Alien)
EndWarp    
  ; Extract Alien Type
  lda INVT,X                ; [17] + 4
  cmp #%10000000            ; [21] + 2    Copy Bit 7 Into Carry
  rol                       ; [23] + 2
  cmp #%10000000            ; [25] + 2    Copy Bit 6 Into Carry
  rol                       ; [27] + 2
  rol                       ; [29] + 2
  rol                       ; [31] + 2
  and #%00111100            ; [33] + 2    Clear Extra Bits
  ; Check For Astronaut
  cmp #9<<2                 ; [35] + 2
  beq SphereScore           ; [37] + 2/3
  ; Set Initial Movement Direction
  ldy INVX,X                ; [39] + 4
  cpy #XMIDDLE              ; [43] + 2
  bcc StartRight            ; [45] + 2/3
  ora #%10000000            ; [47] + 2
StartRight
  ora #%01000000            ; [49] + 2
  sta INVT,X                ; [51] + 4
  jmp NextAnim              ; [55] + 3 = 58

  ; End Explosion Effect (Increase Score & Remove Alien)
EndExplosion
  ; Check If In Hyperspace
  lda TIME                  ; [13] + 3
  bpl IncreaseScore         ; [16] + 2/3
  ; Remove Alien & Decrement Counter
  lda #0                    ; [18] + 2
  sta INVT,X                ; [20] + 4
  dec KILLS                 ; [24] + 5
  ; Increment Score Accumulator
  sed                       ; [29] + 2
  clc                       ; [31] + 2
  lda ACCUM                 ; [33] + 3
  adc #1                    ; [36] + 2
  sta ACCUM                 ; [38] + 3
  ; Add accumulator to score (x 100)
  clc                       ; [41] + 2
  lda SCORE+1               ; [43] + 3
  adc ACCUM                 ; [46] + 3
  sta SCORE+1               ; [49] + 3
  bcc NoExtraLife1          ; [52] + 2/3
  lda SCORE+2               ; [54] + 3
  adc #0                    ; [57] + 2
  sta SCORE+2               ; [59] + 3
  and #%00001111            ; [62] + 2
  beq ExtraLife             ; [64] + 2/3
NoExtraLife1
  cld                       ; [66] + 2
  ; Show Accumulator Message
  lda #%01000100            ; [68] + 2
  sta MESSAGE               ; [70] + 3
  jmp NextAnim              ; [73] + 3 = 76
  
  ; Add Sphere Score During Change To Astronaut
SphereScore
  sta INVT,X                ; [40] + 4
  ldy #3                    ; [44] + 2
  jmp IncreaseScore2        ; [46] + 3
  
IncreaseScore
  ; Increase Score According To Alien Type (Encoded In Bits 0-1)
  lda INVT,X                ; [19] + 4
  and #%00000011            ; [23] + 2
  tay                       ; [25] + 2
  ; Remove Alien & Decrement Counter
  lda #0                    ; [27] + 2
  sta INVT,X                ; [29] + 4
  dec KILLS                 ; [33] + 5
IncreaseScore2
  ; Increase Score using ScoreLo Table
  sed                       ; [49] + 2
  clc                       ; [51] + 2
  lda SCORE+0               ; [53] + 3
  adc ScoreLo,Y             ; [56] + 4
  sta SCORE+0               ; [60] + 3
  lda SCORE+1               ; [63] + 3
  adc ScoreHi,Y             ; [66] + 4
  sta SCORE+1               ; [70] + 3
  bcc NoExtraLife2          ; [73] + 2/3
  lda SCORE+2               ; [75] + 3
  adc #0                    ; [78] + 2
  sta SCORE+2               ; [80] + 3
  and #%00001111            ; [83] + 2
  bne NoExtraLife2          ; [85] + 2/3
ExtraLife
  ; Increment Lives Unless at Maximum
  cld                       ; [87] + 2
  lda LIVES                 ; [89] + 3
  and #%11111000            ; [92] + 2
  sta TEMP                  ; [94] + 3
  lda LIVES                 ; [97] + 3
  and #%00000111            ; [100] + 2
  cmp #MAXLIVES             ; [102] + 2
  bcs SkipExtraLife         ; [104] + 2/3
  adc #1                    ; [106] + 2
  ora TEMP                  ; [108] + 3
  sta LIVES                 ; [111] + 3
  ; Show Extra Life Message
  lda #%01000011            ; [114] + 2
  sta MESSAGE               ; [116] + 3
  ; Play Extra Life Speech
  lda SPEECH                ; [119] + 3
  bne SkipExtraLife         ; [122] + 2/3
  lda #((2<<6)|32)          ; [124] + 2
  sta SPEECH                ; [126] + 3
SkipExtraLife
  jmp NextAnim              ; [129] + 3 = 132
NoExtraLife2
  cld                       ; [88] + 2
  jmp NextAnim              ; [90] + 3 = 93

EndPhase
  jmp EndGameLogic
  
; -----------------------------------------------------------------------------
; ALIEN MOTION FUNCTIONS
; -----------------------------------------------------------------------------

  ALIGN   256

  ; Astronaut Movement
MoveAstroBack
  ldy INVY,X                ; [0] + 4
  lda Movement,Y            ; [4] + 4
  tay                       ; [8] + 2
  lda DVDTAB,Y              ; [10] + 4
  asl                       ; [14] + 2    Double Speed
  clc                       ; [16] + 2
  adc INVY,X                ; [18] + 4
  bpl EndMoveBack           ; [22] + 2/3
  ; Delete Astronaut If Off Screen
  lda #0                    ; [24] + 2
  sta INVT,X                ; [26] + 4
  dec KILLS                 ; [30] + 5
EndMoveBack
  sta INVY,X                ; [35] + 4
NoMove
  jmp EndMotion             ; [39] + 3 
                            ; WORST CASE = 42
     
  ; Basic Left/Right Movement
MoveLR
  ldy INVX,X                ; [0] + 4
  lda INVT,X                ; [4] + 4
  bmi LeftMotion1           ; [8] + 2/3
RightMotion1
  iny
  cpy #XMAX
  bcs SwapXDirection1
  sty INVX,X
  bcc ForwardMotion1
LeftMotion1
  dey                       ; [21] + 2
  cpy #XMIN                 ; [23] + 2
  bcc SwapXDirection1       ; [25] + 2/3
  sty INVX,X                ; [27] + 4
  bcs ForwardMotion1        ; [31] + 3
SwapXDirection1
  eor #%10000000            ; [28] + 2
  sta INVT,X                ; [30] + 4
ForwardMotion1
  ldy INVY,X                ; [34] + 4
  lda Movement,Y            ; [38] + 4
  tay                       ; [42] + 2
  sec                       ; [44] + 2
  lda INVY,X                ; [46] + 4
  sbc DVDTAB,Y              ; [50] + 4
  sta INVY,X                ; [54] + 4
  jmp EndMotion             ; [58] + 3
                            ; WORST CASE = 61
  
  ; Fast Left/Right Movement
MoveFastLR
  ldy INVX,X                ; [0] + 4
  lda INVT,X                ; [4] + 4
  bmi LeftMotion2           ; [8] + 2/3
RightMotion2
  iny                       ; [10] + 2
  jmp RightMotion1          ; [12] + 3
LeftMotion2
  dey                       ; [11] + 2
  jmp LeftMotion1           ; [13] + 3
  
  ; Ultra Fast Left/Right Movement
MoveUltraLR
  ldy INVX,X                ; [0] + 4
  lda INVT,X                ; [4] + 4
  bmi LeftMotion3           ; [8] + 2/3
RightMotion3
  iny                       ; [10] + 2
  iny                       ; [12] + 2
  iny                       ; [14] + 2
  jmp RightMotion1          ; [16] + 3
LeftMotion3
  dey                       ; [11] + 2
  dey                       ; [13] + 2
  dey                       ; [15] + 2
  jmp LeftMotion1           ; [18] + 3
    
  ; Change Y Direction At Middle Of Screen
MoveSwirl
  ldy INVX,X                ; [0] + 4
  lda INVT,X                ; [4] + 4
  bmi LeftMotion4           ; [8] + 2/3
RightMotion4
  cpy #XMIDDLE              ; [10] + 2
  bcc MiddleMotion4         ; [12] + 2/3
  iny                       ; [14] + 2
  iny                       ; [16] + 2
  cpy #XMAX                 ; [18] + 2
  bcs SwapXDirection4       ; [20] + 2/3
  sty INVX,X                ; [22] + 4
  bcc ForwardMotion4        ; [26] + 3
LeftMotion4
  dey                       ; [11] + 2
  dey                       ; [13] + 2
  cpy #XMIN                 ; [15] + 2
  bcc SwapXDirection4       ; [17] + 2/3
  sty INVX,X                ; [19] + 4
  bcs ForwardMotion4        ; [23] + 3
SwapXDirection4
  eor #%10000000            ; [23] + 2
  sta INVT,X                ; [25] + 4
  jmp ForwardMotion4        ; [29] + 3
MiddleMotion4
  iny                       ; [15] + 2
  iny                       ; [17] + 2
  sty INVX,X                ; [19] + 4
  cpy #XMIDDLE              ; [23] + 2
  bcc ForwardMotion4        ; [25] + 2/3
SwapYDirection4
  eor #%01000000            ; [27] + 2
  sta INVT,X                ; [29] + 4
  jmp EndMotion             ; [33] + 3
ForwardMotion4
  ldy INVY,X                ; [32] + 4
  and #%01000000            ; [36] + 2
  beq BackwardMotion4       ; [38] + 2/3
  lda Movement,Y            ; [40] + 4
  tay                       ; [44] + 2
  sec                       ; [46] + 2
  lda INVY,X                ; [48] + 4
  sbc DVDTAB,Y              ; [52] + 4
  sta INVY,X                ; [56] + 4
  jmp EndMotion             ; [60] + 3
BackwardMotion4
  lda Movement,Y            ; [41] + 4
  tay                       ; [45] + 2
  clc                       ; [47] + 2
  lda INVY,X                ; [49] + 4
  adc DVDTAB,Y              ; [53] + 4
  sta INVY,X                ; [57] + 4
  jmp EndMotion             ; [61] + 3
                            ; WORST CASE = 64 CYCLES
  
  ; Change Y Direction At Middle And Edge Of Screen
MoveSwirl2
  ldy INVX,X                ; [0] + 4
  lda INVT,X                ; [4] + 4
  bmi LeftMotion5           ; [8] + 2/3
RightMotion5
  cpy #XMIDDLE              ; [10] + 2
  bcc MiddleMotion4         ; [12] + 2/3
  iny                       ; [14] + 2
  iny                       ; [16] + 2
  cpy #XMAX                 ; [18] + 2
  bcs SwapXDirection5       ; [20] + 2/3
  sty INVX,X                ; [22] + 4
  bcc ForwardMotion4        ; [26] + 3
LeftMotion5
  dey                       ; [11] + 2
  dey                       ; [13] + 2
  cpy #XMIN                 ; [15] + 2
  bcc SwapXDirection5       ; [17] + 2/3
  sty INVX,X                ; [19] + 4
  bcs ForwardMotion4        ; [23] + 3
SwapXDirection5
  eor #%11000000            ; [20] + 2
  sta INVT,X                ; [22] + 4
  jmp ForwardMotion4        ; [26] + 3

EndMoveFns
  if (>NoMove != >EndMoveFns)
    echo "WARNING: Alien Motion Functions Cross Page Boundary!"
  endif

; -----------------------------------------------------------------------------
; ALIEN FIRING FUNCTIONS
; -----------------------------------------------------------------------------

  ALIGN   256
    
NoFire
  ; Don't Fire Bullets
  jmp NextFire
  
Alien1Fire
  ; Fire Normal Bullets Occasionally
  lda RANDOM
  cmp #16
  bcc FireB1
  jmp EndBulletFire

Alien2Fire
  ; Fire Homing Bullets Occasionally
  lda RANDOM
  cmp #16
  bcc FireB2
  jmp EndBulletFire
  
BugAlienFire
  ; Always Fire Normal Bullets
  jmp FireB1
  
BigUFOFire
  ; Fire Normal or Homing Bullets
  lda RANDOM                ; [0] + 3
  bpl BigFire               ; [3] + 2/3
  jmp EndBulletFire         ; [5] + 3
BigFire
  cmp #64                   ; [6] + 2
  bcs FireB1                ; [8] + 2/3
  bcc FireB2                ; [10] + 3 
                            ; WORST CASE = 13 CYCLES
  
SmallUFOFire
  ; Fire Homing Bullets 1/2 Times
  lda RANDOM
  bmi FireB2
  jmp EndBulletFire

  ; Fire Normal Bullet
FireB1
  ldy B1                    ; [0] + 3
  bmi EndFireB1B2           ; [3] + 2/3
  ; Store Bullet Y Position
  sec                       ; [5] + 2
  lda INVY,X                ; [7] + 4
  sbc #6                    ; [11] + 2
  lsr                       ; [13] + 2
  and #%00111111            ; [15] + 2
  sta B1Y,Y                 ; [17] + 4
  ; Store Bullet X Position
  clc                       ; [21] + 2
  lda INVX,X                ; [23] + 4
  adc #4                    ; [27] + 2
  ; Set Bullet Direction
  cmp PLAYERX               ; [29] + 3
  ror                       ; [32] + 2
  sta B1X,Y                 ; [34] + 4
  jmp BulletSound           ; [38] + 3

  ; Fire Homing Bullet
FireB2
  ldy B2                    ; [0] + 3
  bmi EndFireB1B2           ; [3] + 2/3
  ; Store Bullet Y Position
  sec                       ; [5] + 2
  lda INVY,X                ; [7] + 4
  sbc #6                    ; [11] + 2
  lsr                       ; [13] + 2
  and #%00111111            ; [15] + 2
  sta B2Y,Y                 ; [17] + 4
  ; Store Bullet X Position
  clc                       ; [21] + 2
  lda INVX,X                ; [23] + 4
  adc #4                    ; [27] + 2
  ; Set Initial Bullet Direction
  cmp PLAYERX               ; [29] + 3
  ror                       ; [32] + 2
  sta B2X,Y                 ; [34] + 4
  ; Play Bullet Sound
BulletSound
  lda SND0                  ; [41] + 3
  beq PlayBulletSound       ; [44] + 2/3
  and #%11100000            ; [46] + 2
  cmp #7<<5                 ; [48] + 2
  bcc EndFireB1B2           ; [50] + 2/3
PlayBulletSound
  lda #((7<<5)|7)           ; [52] + 2
  sta SND0                  ; [54] + 3
EndFireB1B2
  jmp EndBulletFire         ; [57] + 3 
                            ; WORST CASE = 60 CYCLES
                            
EndFireFns
  if (>NoFire != >EndFireFns)
    echo "WARNING: Alien Firing Functions Cross Page Boundary!"
  endif

  ; Blue Fade Colours
BlueFade
  IF (PALCOLS)
  DC.B  $D0, $D2, $D4, $D6, $D8, $DA, $DC, $DE
  DC.B  $DE, $DC, $DA, $D8, $D6, $D4, $D2, $D0
  ELSE
  DC.B  $70, $72, $74, $76, $78, $7A, $7C, $7E
  DC.B  $7E, $7C, $7A, $78, $76, $74, $72, $70
  ENDIF
EndBlueFade
  if (>BlueFade != >EndBlueFade)
    echo "WARNING: Blue Fade Crosses Page Boundary!"
  endif

  ; Red Fade Colours
RedFade
  IF (PALCOLS)
  DC.B  $60, $62, $64, $66, $68, $6A, $6C, $6E
  DC.B  $6E, $6C, $6A, $68, $66, $64, $62, $60
  ELSE
  DC.B  $40, $42, $44, $46, $48, $4A, $4C, $4E
  DC.B  $4E, $4C, $4A, $48, $46, $44, $42, $40
  ENDIF
EndRedFade
  if (>RedFade != >EndRedFade)
    echo "WARNING: Red Fade Crosses Page Boundary!"
  endif

  ; Green Fade Colours
GreenFade
  IF (PALCOLS)
  DC.B  $50, $52, $54, $56, $58, $5A, $5C, $5E
  DC.B  $5E, $5C, $5A, $58, $56, $54, $52, $50
  ELSE
  DC.B  $C0, $C2, $C4, $C6, $C8, $CA, $CC, $CE
  DC.B  $CE, $CC, $CA, $C8, $C6, $C4, $C2, $C0
  ENDIF
EndGreenFade
  if (>GreenFade != >EndGreenFade)
    echo "WARNING: Green Fade Crosses Page Boundary!"
  endif

  ALIGN   256
  
  ; Alien Movement Table (256 Entries)
Movement
  DS.B    72, 4        ; 0 -> 64+8
  DS.B    30, 3        ; 64+8 -> 96+6
  DS.B    14, 2        ; 96+6 -> 112+4
  DS.B    140,1        ; Remainder
  
  ALIGN   256

  ; Bullet Movement Table (256 Entries)
BulletMovement
  DS.B    32, 3        ; 0 -> 64 (/2)
  DS.B    16, 2        ; 64 -> 96 (/2)
  DS.B    8,  1        ; 96 -> 104 (/2)
  DS.B    8,  0        ; 104 - >112 (/2)
  DS.B    192, 0       ; Remainder

; -----------------------------------------------------------------------------
; PART 8 - TIME/SCORE/MESSAGE DATA
; -----------------------------------------------------------------------------

  ALIGN   256

  ; Wave Table (2 Bytes Per Wave - Must Match SpawnTable!)
  ; = 128 BYTES TOTAL
  ; 0 - Total Aliens -1
  ; 1 - Alien Movement Speed
WaveTable
  DC.B  WAVE1SIZE, 5
  DC.B  WAVE2SIZE, 5
  DC.B  WAVE3SIZE, 5
  DC.B  WAVE4SIZE, 5
  DC.B  WAVE5SIZE, 5
  DC.B  WAVE6SIZE, 5
  DC.B  WAVE7SIZE, 7
  DC.B  WAVE8SIZE, 7
  DC.B  WAVE9SIZE, 7
  DC.B  WAVE10SIZE, 7
  DC.B  WAVE11SIZE, 9
  DC.B  WAVE12SIZE, 9
  DC.B  WAVE13SIZE, 9
  DC.B  WAVE14SIZE, 9
  DC.B  WAVE15SIZE, 9
  DC.B  WAVE16SIZE, 11
  DC.B  WAVE17SIZE, 11
  DC.B  WAVE18SIZE, 11
  DC.B  WAVE19SIZE, 11
  DC.B  WAVE20SIZE, 11
  DC.B  WAVE21SIZE, 11
  DC.B  WAVE22SIZE, 13
  DC.B  WAVE23SIZE, 13
  DC.B  WAVE24SIZE, 13
  DC.B  WAVE25SIZE, 13
  DC.B  WAVE26SIZE, 13
  DC.B  WAVE27SIZE, 15
  DC.B  WAVE28SIZE, 15
  DC.B  WAVE29SIZE, 15
  DC.B  WAVE30SIZE, 15
  DC.B  WAVE31SIZE, 15
  DC.B  WAVE32SIZE, 17
  ; Repeat Levels But Faster!
  DC.B  WAVE1SIZE, 15
  DC.B  WAVE2SIZE, 15
  DC.B  WAVE3SIZE, 15
  DC.B  WAVE4SIZE, 15
  DC.B  WAVE5SIZE, 15
  DC.B  WAVE6SIZE, 15
  DC.B  WAVE7SIZE, 17
  DC.B  WAVE8SIZE, 17
  DC.B  WAVE9SIZE, 17
  DC.B  WAVE10SIZE, 17
  DC.B  WAVE11SIZE, 19
  DC.B  WAVE12SIZE, 19
  DC.B  WAVE13SIZE, 19
  DC.B  WAVE14SIZE, 19
  DC.B  WAVE15SIZE, 19
  DC.B  WAVE16SIZE, 21
  DC.B  WAVE17SIZE, 21
  DC.B  WAVE18SIZE, 21
  DC.B  WAVE19SIZE, 21
  DC.B  WAVE20SIZE, 21
  DC.B  WAVE21SIZE, 21
  DC.B  WAVE22SIZE, 23
  DC.B  WAVE23SIZE, 23
  DC.B  WAVE24SIZE, 23
  DC.B  WAVE25SIZE, 23
  DC.B  WAVE26SIZE, 23
  DC.B  WAVE27SIZE, 25
  DC.B  WAVE28SIZE, 25
  DC.B  WAVE29SIZE, 25
  DC.B  WAVE30SIZE, 25
  DC.B  WAVE31SIZE, 25
  DC.B  WAVE32SIZE, 27
EndWaveTable
  if (>WaveTable != >EndWaveTable)
    echo "WARNING: Wave Table Cross Page Boundary!"
  endif

  ; Binary To BCD Table (100 Entries)
BCDTable
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
    echo "WARNING: BCD Table crosses a page boundary!"
  endif

  ; Score Digits
Digit
  DC.B  <Zero, <One, <Two, <Three, <Four
  DC.B  <Five, <Six, <Seven, <Eight, <Nine
EndDigit
  if (>Digit != >EndDigit)
    echo "WARNING: Digit Crosses Page Boundary!"
  endif
  
  ; Numbers and Letters For Main Game
  NUMBERLETTERS


  echo "----",($FFF0 - *) , "bytes left (BANK 4 - GAME LOGIC)"
         
  ORG     $BFF0
  RORG    $FFF0
  DC.B    0, 0, 0, 255
  DC.B    0, 0, 0, 0, 0, 0
  DC.W    (PlusAPI - $E000)
  DC.W    Init4, Init4

