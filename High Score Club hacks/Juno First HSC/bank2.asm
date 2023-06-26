; -----------------------------------------------------------------------------
; BANK 2 - KERNEL 1
; -----------------------------------------------------------------------------

; Grid Colours (PAL/NTSC)
  IF (PALCOLS)
A_GRIDCOL0 = $D0
A_GRIDCOL1 = $D2
A_GRIDCOL2 = $D4
A_GRIDCOL3 = $D6
A_GRIDCOL4 = $D8
A_GRIDCOL5 = $DA
A_GRIDCOL6 = $DC
A_GRIDCOL7 = $DE
  ELSE
A_GRIDCOL0 = $80
A_GRIDCOL1 = $82
A_GRIDCOL2 = $84
A_GRIDCOL3 = $86
A_GRIDCOL4 = $88
A_GRIDCOL5 = $8A
A_GRIDCOL6 = $8C
A_GRIDCOL7 = $8E
  ENDIF

  ; Sprite Positioning Macro (BattleZone Divide By 15 Algorithm)
  MAC A_XPOSITION
.RES   SET {1}
.HM    SET {2}
.Div15
  sbc #15                   ; [11] + 2
  bcs .Div15                ; [13/18/23/28/33/38/43/48/53/58] + 2/3
  tax                       ; [15/20/25/30/35/40/45/50/55/60] + 2
  lda A_FineTuneEnd,X       ; [17/22/27/32/37/42/47/52/57/62] + 5
  sta .RES                  ; [22/27/32/37/42/47/52/57/62/67] + 3
  sta .HM                   ; [25/30/35/40/45/50/55/60/65/70] + 3
  ENDM                      ; WORST CASE = 73

  ; Grid-line Offset Calculation
  MAC A_GRIDOFFSET
.LINE    SET {1}
  lsr DVD                   ; [0] + 5
  sec                       ; [5] + 2
  lda A_LineTable0+.LINE    ; [7] + 3
  sbc DVD                   ; [10] + 3
  sta DVD+.LINE             ; [13] + 3 = 16
  ENDM
  
  ; Bullet Drawing Macro (Combat PHP Stack Trick)
  MAC A_DRAWBULLETS
  tya                       ; [0] + 2
  lsr                       ; [2] + 2
  cmp B2                    ; [4] + 3
  php                       ; [7] + 3
  cmp B1                    ; [10] + 3
  php                       ; [13] + 3 = 16
  ENDM

  SEG     BANK2
  ORG     $9000
  RORG    $F000

Init2
  ; Switch to Bank 8
  nop     $FFFB
  nop
  nop
  nop
  nop
  nop
  nop
KernelExit1
  ; Switch to Bank 4
  nop     $FFF7

; Start Of Kernel Here
Kernel1

; -----------------------------------------------------------------------------
; PART 0 - REPOSITION SPRITES & CALCULATE GRIDLINE POSITIONS
; -----------------------------------------------------------------------------

A_MoveSprites
  ; Delay P0 Sprite
  sta WSYNC                 ; [0]
  lda #%00000001            ; [0] + 2
  sta.w VDELP0              ; [2] + 4
  
  ; Position Player Sprite
  lda PLAYERX               ; [6] + 3
  sec                       ; [9] + 2
  A_XPOSITION RESP0,HMP0    ; [11]
  sta WSYNC                 ; [0]
  
  ; Position Bullet Sprite 1
  ldx B1                    ; [0] + 3
  lda B1X,X                 ; [3] + 4
  asl                       ; [7] + 2
  sec                       ; [9] + 2
  A_XPOSITION RESM0,HMM0    ; [11]
  sta WSYNC                 ; [0]

  ; Position Bullet Sprite 2
  ldx B2                    ; [0] + 3
  lda B2X,X                 ; [3] + 4
  asl                       ; [7] + 2
  sec                       ; [9] + 2
  A_XPOSITION RESM1,HMM1    ; [11]
  sta WSYNC                 ; [0]

  ; Set Laser Colour
  IF (PALCOLS)
  lda #$64                  ; [0] + 2
  ELSE
  lda #$44                  ; [0] + 2
  ENDIF
  sta.w COLUPF              ; [2] + 4

  ; Move Laser Beam Sprite
  lda LASERX                ; [6] + 3
  sec                       ; [9] + 2
  A_XPOSITION RESBL,HMBL    ; [11]
  sta WSYNC                 ; [0]

  ; Move Sprites to Positions
  sta HMOVE                 ; [0] + 3

  ; Copy B1 Y-Position
  ldx B1                    ; [3] + 3
  lda B1Y,X                 ; [6] + 4
  sta B1                    ; [10] + 3
  ; Copy B2 Y-Position    
  ldx B2                    ; [13] + 3
  lda B2Y,X                 ; [16] + 4
  sta B2                    ; [20] + 3 

  ; Store Repositioning Functions Pointer
  lda #>A_ReposFunctions0   ; [23] + 2
  sta JPTR+1                ; [25] + 3 = 28
  
  ; Calculate Grid Line Offsets (Unrolled)
  lda START                 ; [0] + 3
  lsr                       ; [3] + 2
  lsr                       ; [5] + 2
  sta DVD                   ; [7] + 3
  sec                       ; [10] + 2
  lda A_LineTable0+1        ; [12] + 3
  sbc DVD                   ; [15] + 3
  sta DVD+1                 ; [18] + 3 = 21
GINDEX    SET 2
  REPEAT 6
  A_GRIDOFFSET GINDEX       ; [0] + 16
GINDEX    SET GINDEX+1
  REPEND                    ; RUNTIME = 21 + (16*6) = 117 (1.5 SCANLINES)
  
  ; End of Vertical Blank
  WAIT_VBLANK               ; [0]

  ; Set Sky Colour
  ldy SKYCOL                ; [0] + 3
  sty.w COLUBK              ; [3] + 4

  ; Calculate Laser Beam Size
  ldy LASERY                ; [7] + 3
  lda A_LaserSize,Y         ; [10] + 4
  sta LASERS                ; [14] + 3

  ; Set Scanner Height    
  ldy #SCANH                ; [17] + 2
  
  ; Start Scanner Kernel
  jmp A_ScannerLoop         ; [19] + 3
A_EndMoveSprites
  if (>A_MoveSprites != >A_EndMoveSprites)
    echo "WARNING: Sprite Movement Code Crosses Page Boundary!"
  endif

  ; Grid Line Dividers (8 Entries)
A_LineTable0
  DC.B  128-128, 128-64, 128-32, 128-16, 128-8, 128-4, 128-2, 128-1
A_EndLineTable0
  if (>A_LineTable0 != >A_EndLineTable0)
    echo "WARNING: Line Table Crosses Page Boundary!"
  endif
  
  DC.B  "PARTA"
    
; -----------------------------------------------------------------------------
; PART 1 - DISPLAY ALIEN SCANNER AT TOP OF SCREEN
; -----------------------------------------------------------------------------

  ALIGN   256
  
  ; Skip The Rest Of The Scanner If No Sprites Are Displayed
A_FinishLoop
  sta WSYNC                 ; [0]
  ; Clear Alien Sprite 
  lda #0                    ; [0]
  sta GRP1                  ; [0] + 3    > 75 < 23
  sta WSYNC                 ; [0]
  sta WSYNC                 ; [0]
  ; Check If We Are Done
  dey                       ; [0] + 2
  bpl A_FinishLoop          ; [2] + 2/3
  jmp A_EndScannerLoop      ; [4] + 3

  ; Skip Scanner Region If It Contains No Sprites
A_SkipLine
  ; Clear Alien Sprite
  lda #0                    ; [9] + 2
  sta GRP1                  ; [11] + 3    < 23
  ; Skip 3 Rows
  sta WSYNC                 ; [0]
  sta WSYNC                 ; [0]
  sta WSYNC                 ; [0]
  ; Check If We Are Done
  dey                       ; [0] + 2
  bpl A_NextScannerLine     ; [2] + 2/3
  jmp A_EndScannerLoop      ; [4] + 3

  ; Main Scanner Kernel Loop
A_ScannerLoop
  ; Fetch Index Of Next Alien
  ldx INDEX                 ; [22] + 3
  
  ; Clear Sprite Movements
  sta.w HMCLR               ; [25] + 4    > 24(+3?)

  ; Check If We Are Finished Drawing Sprites
  bmi A_FinishLoop          ; [29] + 2/3
  
  ; Fetch Y Position Of Alien
  lda INVY,X                ; [31] + 4
  
  ; Scanner Shows Positions 255->128 only
  bpl A_FinishLoop          ; [35] + 2/3
  
  ; Convert Coordinate Into Row Number 1->7
  and #%01110000            ; [37] + 2
  lsr                       ; [39] + 2
  lsr                       ; [41] + 2
  lsr                       ; [43] + 2
  lsr                       ; [45] + 2
  sta ALIENY                ; [47] + 3
  
  ; Fetch Alien Type
  lda INVT,X                ; [50] + 4
  and #%00111100            ; [54] + 2
  txs                       ; [56] + 2
  tax                       ; [58] + 2
  lda A_AlienSprites+3,X    ; [60] + 4
  sta TYPE                  ; [64] + 3
  tsx                       ; [67] + 2
  
  ; Update Index
  lda INVS,X                ; [69] + 4
  sta INDEX                 ; [73] + 3

  ; Clear Any Previous Alien Sprite
  lda #0                    ; [0] + 2
  sta GRP1                  ; [2] + 3    > 75 < 23
  
  ; Fetch Alien X Position & Reposition Sprite
  lda INVX,X                ; [5] + 4
  sec                       ; [9] + 2
  A_XPOSITION RESP1,HMP1    ; [11]
  sta WSYNC                 ; [0] 
  sta HMOVE                 ; [0] + 3

A_NextScannerLine
  ; Check If We Have Reached Alien Position
  cpy ALIENY                ; [3] + 3
  bne A_SkipLine            ; [6] + 2/3

  ; Draw Alien (Line 0)
  ldx TYPE                  ; [8] + 3
  lda A_Aliens+1,X          ; [11] + 4
  sta GRP1                  ; [15] + 3    < 23
  lda A_AlienCols+1,X       ; [18] + 4
  sta COLUP1                ; [22] + 3    < 23 !!!

  ; Draw Alien (Line 1)
  sta WSYNC                 ; [0]
  ldx TYPE                  ; [0] + 3
  lda A_Aliens+2,X          ; [3] + 4
  sta GRP1                  ; [7] + 3    < 23
  lda A_AlienCols+2,X       ; [10] + 4
  sta COLUP1                ; [14] + 3    < 23

  ; Check If We Are Done
  dey                       ; [17] + 2
  bpl A_ScannerLoop         ; [19] + 2/3
A_EndScannerLoop

  ; Reset Sprite Collisions
  sta CXCLR                 ; [21] + 3

  ; Set Stack To M1 For Bullet Drawing
  ldx #ENAM1                ; [24] + 2
  txs                       ; [26] + 2
  
  ; Set Initial Alien Counters
  ldx INDEX                 ; [28] + 3
  bmi A_NoMoreSprites       ; [31] + 2/3
  ldy INVY,X                ; [33] + 4
  sty ALIENY                ; [37] + 3
  lda INVT,X                ; [40] + 4
  and #%00111100            ; [44] + 2
  clc                       ; [46] + 2
  adc A_AlienSize,Y         ; [48] + 4
  tay                       ; [52] + 2
  lda A_AlienSprites,Y      ; [54] + 4
  sta TYPE                  ; [58] + 3
  
  ; Clear Alien Sprite
  sta WSYNC                 ; [0]
  lda #0                    ; [0] + 2
  sta GRP1                  ; [2] + 3    < 23

  ; Set Initial Alien Sprite Position
  lda INVX,X                ; [5] + 4 
  sec                       ; [9] + 2
  A_XPOSITION RESP1,HMP1    ; [11]
  sta WSYNC                 ; [0]
  sta HMOVE                 ; [0] + 3
  
  ; Decrease Alien Index
  ldx INDEX                 ; [3] + 3
  lda INVS,X                ; [6] + 4
  sta INDEX                 ; [10] + 3

A_FinishScanner
  ; Set Default Missile Colours
  IF (PALCOLS)
  lda #$0E                  ; [13] + 2
  ELSE
  lda #$0E                  ; [13] + 2
  ENDIF
  sta COLUP0                ; [15] + 3
  sta COLUP1                ; [18] + 3
  
  ; Reset Starting Gridline Position
  lda DVD+7                 ; [21] + 3
  sta LINE                  ; [24] + 3

  ; Set Gridline Counter
  lda #6                    ; [27] + 2
  sta DVD                   ; [29] + 3
      
  ; Set Grid Kernel Height
  ldy #GRIDH                ; [32] + 2

  ; Start Grid Kernel
  SLEEP 3                   ; [34] + 3
  jmp A_Resume1             ; [37] + 3

  ; No More Sprites To Display
A_NoMoreSprites
  sta WSYNC                 ; [0]
  ; Clear Graphics & Alien Counter
  lda #0                    ; [0] + 2
  sta GRP1                  ; [2] + 3    < 23
  sta ALIENY                ; [5] + 3
  sta WSYNC                 ; [0]
  SLEEP 10                  ; [0] + 10
  jmp A_FinishScanner       ; [10] + 3
A_EndScannerKernel

  if (>A_FinishLoop != >A_EndScannerKernel)
    echo "WARNING: Scanner Kernel Crosses Page Boundary!"
  endif

  DC.B  "PARTB"

  ALIGN   256

  ; HMOVE Finetune Table (15 Entries)
A_FineTune
  DC.B  %01100000         ; + 6 (Left)
  DC.B  %01010000         ; + 5
  DC.B  %01000000
  DC.B  %00110000
  DC.B  %00100000
  DC.B  %00010000
  DC.B  %00000000         ; Centre
  DC.B  %11110000
  DC.B  %11100000
  DC.B  %11010000
  DC.B  %11000000
  DC.B  %10110000
  DC.B  %10100000
  DC.B  %10010000         ; - 7
  DC.B  %10000000         ; - 8 (Right)
A_FineTuneEnd = A_FineTune - 241

; -----------------------------------------------------------------------------
; PART 2 - GRID KERNEL: DISPLAYS ALIENS, GRID LINES, LASER BEAM, AND BULLETS
; -----------------------------------------------------------------------------
        
A_DrawLine0
  ; Display Gridline (By Changing PF Background Colour)
  ldx DVD                   ; [12] + 3
  lda A_GridCols,X          ; [15] + 4
  sta COLUBK                ; [19] + 3    < 23
  
  ; Calculate Next Gridline Position
  lda DVD,X                 ; [22] + 4
  sta LINE                  ; [26] + 3
  dec DVD                   ; [29] + 5
  
  ; Resume Kernel
  SLEEP 3                   ; [34] + 3
  jmp A_Resume1             ; [37] + 3

A_DrawLaser0
  ; Draw Laser Beam
  sbc LASERY                ; [23] + 3
  adc LASERS                ; [26] + 3
  rol                       ; [29] + 2
  rol                       ; [31] + 2
  sta.w ENABL               ; [33] + 4
  jmp A_Resume1             ; [37] + 3

  ; Main Grid Kernel Loop
A_GridLoop
  ; Draw Alien (Variant of Thomas Jentzsch SkipDraw Routine)
  SLEEP 3                   ; [65] + 3    
  lda A_Aliens,X            ; [68] + 4
  sta GRP1                  ; [72] + 3    > 75  !!!!
  lda A_AlienCols,X         ; [75] + 4
  sta COLUP1                ; [3] + 3    > 75 < 23

  ; Check If Grid Line Should Be Displayed
A_Resume0
  cpy LINE                  ; [6] + 3
  bcc A_DrawLine0           ; [9] + 2/3

  ; Clear Background
  lda #0                    ; [11] + 2
  sta COLUBK                ; [13] + 3    < 23

  ; Draw Bullets/Laser
  tya                       ; [16] + 2
  lsr                       ; [18] + 2
  bcs A_DrawLaser0          ; [20] + 2/3
  cmp B2                    ; [22] + 3
  php                       ; [25] + 3
  cmp B1                    ; [28] + 3
  php                       ; [31] + 3
  ldx #ENAM1                ; [34] + 2
  txs                       ; [36] + 2
  nop                       ; [38] + 2
A_Resume1
  ; Check If Alien Should Be Drawn Next Line
  sec                       ; [40] + 2
  tya                       ; [42] + 2
  sbc ALIENY                ; [44] + 3
  bcs A_NoDraw0             ; [47] + 2/3
  
  ; Check For New Alien
  adc #8                    ; [49] + 2
  bcc A_NewSprite0          ; [51] + 2/3

  ; Calculate Alien Sprite Pointer
  adc TYPE                  ; [53] + 3
  tax                       ; [56] + 2
  
  ; Decrease Row  Counter
  dey                       ; [58] + 2
  cpy #SHIPH                ; [60] + 2
  bcs A_GridLoop            ; [62] + 2/3
  
  ; Disable Laser Beam
  lda #0                    ; [64] + 2
  sta ENABL                 ; [66] + 3

  ; Draw Alien
  lda A_Aliens,X            ; [69] + 4
  sta GRP1                  ; [73] + 3    > 75 < 23
  lda A_AlienCols,X         ; [0] + 4 
  sta COLUP1                ; [4] + 3    > 75 < 23

  ; Start Ship Kernel
  jmp A_ShipKernel          ; [7] + 3

A_NoDraw0
  ; Cycle Padding    Code
  SLEEP 12                  ; [50] + 12    
  lda #0                    ; [62] + 2
  beq A_Cont0               ; [64] + 3
A_NoNewSprite0
  ; Set Alien Coordinate to 0
  nop                       ; [60] + 2
  lda #0                    ; [62] + 2
  sta ALIENY                ; [64] + 3
A_Cont0
  ; Decrease Row Counter
  dey                       ; [67] + 2
  cpy #SHIPH                ; [69] + 2
  bcs A_Cont1               ; [71] + 2/3
  ; Clear Alien Sprite, Disable Laser Beam, and Start Ship Kernel
  SLEEP 4                   ; [73] + 4    
  sta GRP1                  ; [1] + 3    > 75 < 23
  sta ENABL                 ; [4] + 3
  jmp A_ShipKernel          ; [7] + 3
A_Cont1
  ; Resume Grid Kernel
  nop                       ; [74] + 2    
  sta GRP1                  ; [0] + 3    > 75 < 23
  jmp A_Resume0             ; [3] + 3

; -----------------------------------------------------------------------------
; PART 3 - REPOSITION ALIEN DURING GRID KERNEL
; -----------------------------------------------------------------------------

A_NewSprite0
  ; Fetch Current Alien Index
  ldx INDEX                 ; [54] + 3
  bmi A_NoNewSprite0        ; [57] + 2/3

  ; Update Next Alien Index Counter
  lda INVS,X                ; [59] + 4
  
  ; Check If Grid Line Should Be Drawn Now
  dey                       ; [63] + 2
  cpy LINE                  ; [65] + 3
  bcc A_LineFirst0          ; [68] + 2/4*

A_RepositionFirst0
  ; Store Next Index
  sta TEMP                  ; [70] + 3
  
  ; Fetch Alien X Position
  lda INVX,X                ; [73] + 4

  ; Clear Background & Alien Sprite
  ldx #0                    ; [1] + 2
  stx COLUBK                ; [3] + 3    < 23
  stx GRP1                  ; [6] + 3    < 23
  
  ; Reposition Alien Sprite (Note: Carry Already Set)
  sec                       ; [9] + 2
  A_XPOSITION RESP1,HMP1    ; [11]
  sta WSYNC                 ; [0] 
  sta HMOVE                 ; [0] + 3
          
  ; Check Grid Line
  dey                       ; [3] + 2
  cpy LINE                  ; [5] + 3
  bcs A_SkipLine2           ; [8] + 2/3
  
  ; Display Grid Line
  ldx DVD                   ; [10] + 3
  lda A_GridCols,X          ; [13] + 4    
  sta COLUBK                ; [17] + 3    < 23 

  ; Calculate Next Line Position
  lda DVD,X                 ; [20] + 4
  sta LINE                  ; [24] + 3
  dec DVD                   ; [27] + 5
  nop                       ; [32] + 2
A_EndSkip2

  ; Load & Update Index
  ldx INDEX                 ; [34] + 3
  lda TEMP                  ; [37] + 3
  sta INDEX                 ; [40] + 3
  
  ; Store Row Counter    
  sty TEMP                  ; [43] + 3
  
  ; Fetch Alien Y Position (& Store For Later)
  ldy INVY,X                ; [46] + 4
  sty ALIENY                ; [50] + 3
  
  ; Fetch Alien Type
  lda INVT,X                ; [53] + 4
  and #%00111100            ; [57] + 2
  
  ; Add Alien Size
  clc                       ; [59] + 2
  adc A_AlienSize,Y         ; [61] + 4
  tay                       ; [65] + 2
  
  ; Fetch Sprite Offset (& Store For Later)
  lda A_AlienSprites,Y      ; [67] + 4
  sta TYPE                  ; [71] + 3
  
  ; Restore Row Counter
  ldy TEMP                  ; [74] + 3
  dey                       ; [1] + 2
  
  ; Resume Grid Kernel
  jmp A_Resume0             ; [3] + 3 

A_SkipLine2
  ; Draw Bullets
  A_DRAWBULLETS             ; [11] + 16
  ldx #ENAM1                ; [27] + 2
  txs                       ; [29] + 2
  jmp A_EndSkip2            ; [31] + 3

A_EndGridKernel
  if (>A_DrawLine0 != >A_EndGridKernel)
    echo "WARNING: Grid Kernel Crosses Page Boundary!"
  endif

  ; DC.B  "PARTC"

; -----------------------------------------------------------------------------
; PART 4 - DELAYED ALIEN REPOSITIONING (WHEN GRIDLINE NEEDS TO BE DRAWN FIRST)
; -----------------------------------------------------------------------------

  ALIGN   256

A_LineFirst0
  ; Store Index & Row Counter
  sta INDEX                 ; [72] + 3
  sty TEMP                  ; [75] + 3
  
  ; Fetch Grid Line Index
  ldy DVD                   ; [2] + 3
      
  ; Clear Alien Sprite
  lda #0                    ; [5] + 2
  sta GRP1                  ; [7] + 3    > 75 < 23

  ; Display Grid Line
  lda A_GridCols,Y          ; [10] + 4
  sta COLUBK                ; [14] + 3    < 23
  
  ; Calculate Next Line Position
  lda DVD,Y                 ; [17] + 4
  sta LINE                  ; [21] + 3
  dec DVD                   ; [24] + 5
  
  ; Update Alien Variables
  ldy INVY,X                ; [29] + 4
  sty ALIENY                ; [33] + 3
  lda INVT,X                ; [36] + 4
  and #%00111100            ; [40] + 2
  ; clc (Carry Already Clear)
  adc A_AlienSize,Y         ; [42] + 4
  tay                       ; [46] + 2
  lda A_AlienSprites,Y      ; [48] + 4
  sta TYPE                  ; [52] + 3
  
  ; Restore Row Counter
  ldy TEMP                  ; [55] + 3

  ; Draw Bullets
  A_DRAWBULLETS             ; [58] + 16

  ; Decrease Row
  dey                       ; [74] + 2
      
  ; Fetch Sprite X Position
  lda INVX,X                ; [0] + 4

  ; Clear Background
  ldx #0                    ; [4] + 2
  stx COLUBK                ; [6] + 3    > 75 < 23 

  ; Reposition Alien Sprite
  sec                       ; [9] + 2
  A_XPOSITION RESP1,HMP1    ; [11]
  sta WSYNC                 ; [0]
  sta HMOVE                 ; [0] + 3

  ; Check Grid Line
  dey                       ; [3] + 2
  cpy LINE                  ; [5] + 3
  bcs A_SkipLine3           ; [8] + 2/3
  
  ; Display Grid Line
  ldx DVD                   ; [10] + 3
  lda A_GridCols,X          ; [13] + 4
  sta COLUBK                ; [17] + 3    < 23

  ; Calculate Next Grid Line Position
  lda DVD,X                 ; [20] + 4
  sta.w LINE                ; [24] + 4
  dec DVD                   ; [28] + 5
      
  ; Reset Stack
  ldx #ENAM1                ; [33] + 2
  txs                       ; [35] + 2
  jmp A_Resume1             ; [37] + 3

A_SkipLine3
  ; Draw Bullets
  ldx #ENAM1                ; [11] + 2
  txs                       ; [13] + 2
  A_DRAWBULLETS             ; [15] + 16
  ldx #ENAM1                ; [31] + 2
  txs                       ; [33] + 2
  nop                       ; [35] + 2
  jmp A_Resume1             ; [37] + 3

  ; Include Alien Sprite Pointers
A_AlienSprites
  ; Sprite Type/Size Table
  ; 0 = Empty
  DC.B  #<A_Empty, #<A_Empty, #<A_Empty, #<A_Empty
  ; 1 = Alien 1 (Frame 1)
  DC.B  #<A_Alien1_1-1, #<A_Alien1_3-1, #<A_Alien1_4-1, #<A_Alien1_5-1
  ; 2 = Alien 2 (Frame 1)
  DC.B  #<A_Alien2_1-1, #<A_Alien2_3-1, #<A_Alien2_5-1, #<A_Alien2_6-1
  ; 3 = Bug Alien
  DC.B  #<A_BugAlien_1-1, #<A_BugAlien_2-1, #<A_BugAlien_3-1, #<A_BugAlien_4-1
  ; 4 = Sphere
  DC.B  #<A_Sphere_1-1, #<A_Sphere_2-1, #<A_Sphere_3-1, #<A_Sphere_4-1
  ; 5 = Big UFO
  DC.B  #<A_BigUFO_1-1, #<A_BigUFO_2-1, #<A_BigUFO_3-1, #<A_BigUFO_4-1
  ; 6 = Small UFO
  DC.B  #<A_SmallUFO_1-1, #<A_SmallUFO_2-1, #<A_SmallUFO_3-1, #<A_SmallUFO_4-1
  ; 7 = Alien 1 (Frame 2)
  DC.B  #<A_Alien1_2-1, #<A_Alien1_3-1, #<A_Alien1_4-1, #<A_Alien1_5-1
  ; 8 = Alien 2 (Frame 2)
  DC.B  #<A_Alien2_2-1, #<A_Alien2_4-1, #<A_Alien2_5-1, #<A_Alien2_6-1
  ; 9 = Astronaut
  DC.B  #<A_Astronaut_1-1, #<A_Astronaut_2-1, #<A_Astronaut_3-1, #<A_Empty
  ; 10->12 = Warp
  DC.B  #<A_Explosion_3-1, #<A_Explosion_2-1, #<A_Explosion_1-1, #<A_Empty
  DC.B  #<A_Explosion_4-1, #<A_Explosion_3-1, #<A_Explosion_2-1, #<A_Empty
  DC.B  #<A_Explosion_5-1, #<A_Explosion_5-1, #<A_Explosion_5-1, #<A_Empty
  ; 13->15 = Explosion
  DC.B  #<A_Explosion_5-1, #<A_Explosion_5-1, #<A_Explosion_5-1, #<A_Empty
  DC.B  #<A_Explosion_4-1, #<A_Explosion_3-1, #<A_Explosion_2-1, #<A_Empty
  DC.B  #<A_Explosion_3-1, #<A_Explosion_2-1, #<A_Explosion_1-1, #<A_Empty
A_EndAlienSprites
  if (>A_AlienSprites != >A_EndAlienSprites)
    echo "WARNING: Alien Sprite Table crosses a page boundary!"
  endif
    
; -----------------------------------------------------------------------------
; PART 5 - SHIP KERNEL - DISPLAYS SHIP, ALIENS, BULLETS, AND FINAL GRID LINE
; -----------------------------------------------------------------------------

A_DrawLine1
  ; Draw Line
  lda #A_GRIDCOL7           ; [16] + 2
  sta.w COLUBK              ; [18] + 4    < 23
  jmp A_Resume6             ; [22] + 3    = 25
  
  ; Ship Kernel Start Code
A_ShipKernel
  ; Check Grid Line
  cpy LINE                  ; [10] + 3
  bcc A_DrawLine1           ; [13] + 2/3

  ; Clear Background
  lda #0                    ; [15] + 2
  sta COLUBK                ; [17] + 3    < 23
  
  ; Display Bullets
  A_DRAWBULLETS             ; [20] + 16
  ldx #ENAM1                ; [36] + 2
  txs                       ; [38] + 2

  ; Start Ship Kernel
  sec                       ; [40] + 2
  jmp A_ShipLoop            ; [42] + 3

A_NoDraw2
  ; Preload Ship Data
  lda (SPTR),Y              ; [54] + 5
  sta GRP0                  ; [59] + 3    VDEL
  SLEEP 13                  ; [62] + 13
  
  ; Display Ship (By Setting GRP1)
  lda #0                    ; [75] + 2
  sta GRP1                  ; [1] + 3     > 75 < 23
  jmp A_Resume2             ; [4] + 3

A_EndLineFirst0
  if (>A_LineFirst0 != >A_EndLineFirst0)
    echo "WARNING: Repositioning Code Crosses Page Boundary!"
  endif

  DC.B  "PARTD"
  
  ALIGN   256

A_NewSprite1
  ; Load Ship Data
  lda (SPTR),Y              ; [57] + 5
  sta GRP0                  ; [62] + 3    VDEL
  
  ; Fetch Alien Index
  lda #0                    ; [65] + 2
  ldx INDEX                 ; [67] + 3

  ; Prevent Further Aliens Being Displayed
  sta.w ALIENY              ; [70] + 4
  
  ; Display Ship & Clear Background
  sta GRP1                  ; [74] + 3    > 75 < 23
  sta COLUBK                ; [1] + 3     > 75 < 23

  ; Resume Ship Kernel If No New Sprite Required
  bmi A_Resume2             ; [4] + 2/3

    ; Update Ship Colour
  lda (CPTR),Y              ; [6] + 5
  sta COLUP0                ; [11] + 3    > 75 < 23

  ; Calculate Alien Variables
  lda INVY,X                ; [14] + 4
  sta ALIENY                ; [18] + 3
  lda INVT,X                ; [21] + 4
  and #%00111100            ; [25] + 2
  tax                       ; [27] + 2
  lda A_AlienSprites,X      ; [29] + 4
  sta TYPE                  ; [33] + 3
  
  ; Update Alien Index
  ldx INDEX                 ; [36] + 3
  lda INVS,X                ; [39] + 4
  sta INDEX                 ; [43] + 3

  ; Decrease Line Counter
  dey                       ; [46] + 2
  
  ; Preload Ship Data
  lda (SPTR),Y              ; [48] + 5
  sta GRP0                  ; [53] + 3    VDEL

  ; Calculate Kernel Pointer
  lda INVX,X                ; [56] + 4
  tax                       ; [60] + 2
  lda A_ReposTable,X        ; [62] + 4
  sta JPTR                  ; [66] + 3

  ; Set Ship Colour
  lda (CPTR),Y              ; [69] + 5
  sta COLUP0                ; [74] + 3    > 75 < 23
  
  ; Draw Ship
  lda #0                    ; [1] + 2
  sta GRP1                  ; [3] + 3     < 23

  ; Jump To Repositioning Kernel
  jmp (JPTR)                ; [6] + 5     = 11

A_ShipLoop
  ; Calculate Alien Row
  ; sec (Carry Already Set)
  tya                       ; [45] + 2
  sbc ALIENY                ; [47] + 3
  bcs A_NoDraw2             ; [50] + 2/4*
  adc #8                    ; [52] + 2
  bcc A_NewSprite1          ; [54] + 2/3
  adc TYPE                  ; [56] + 3
  tax                       ; [59] + 2
  
  ; Preload Ship Data
  lda (SPTR),Y              ; [61] + 5
  sta GRP0                  ; [66] + 3    VDEL
      
  ; Draw Alien & Ship
  lda A_Aliens,X            ; [69] + 4
  sta GRP1                  ; [73] + 3    > 75
  lda A_AlienCols,X         ; [0] + 4
  sta COLUP1                ; [4] + 3     > 75 < 23
A_Resume2
  ; Set Ship Colour
  lda (CPTR),Y              ; [7] + 5
  sta COLUP0                ; [12] + 3    < 23
  
  ; Draw Bullets
  A_DRAWBULLETS             ; [15] + 16
  ldx #ENAM1                ; [31] + 2
  txs                       ; [33] + 2
A_Resume3
  ; Check End of Kernel
  dey                       ; [35] + 2
  bmi A_EndShipKernel       ; [37] + 2/3
  
  ; Check Line
  cpy LINE                  ; [39] + 3
  bcs A_ShipLoop            ; [42] + 2/3
  
; -----------------------------------------------------------------------------
; PART 6 - DRAW GRID LINE DURING SHIP KERNEL
; -----------------------------------------------------------------------------

A_DrawLine2
  ; Calculate Alien Row
  sec                       ; [44] + 2
  tya                       ; [46] + 2
  sbc ALIENY                ; [48] + 3
  bcs A_NoDraw3             ; [51] + 2/3
  adc #8                    ; [53] + 2
  bcc A_NewSprite3          ; [55] + 2/3
  adc TYPE                  ; [57] + 3
  tax                       ; [60] + 2
  
  ; Preload Ship Data
  lda (SPTR),Y              ; [62] + 5
  sta GRP0                  ; [67] + 3    VDEL
      
  ; Draw Alien & Ship
  lda A_Aliens,X            ; [70] + 4
  sta GRP1                  ; [74] + 3    > 75
  lda A_AlienCols,X         ; [1] + 4
  sta COLUP1                ; [5] + 3     > 75 < 23
A_Resume4
  ; Set Ship Colour
  lda (CPTR),Y              ; [8] + 5
  sta COLUP0                ; [13] + 3    < 23

  ; Draw Grid Line
  lda #A_GRIDCOL7           ; [16] + 2
  sta COLUBK                ; [18] + 3    < 23

A_Resume5            
  ; Decrease Row Counter
  dey                       ; [21] + 2
  bmi A_EndShipKernel       ; [23] + 2/3
A_Resume6
  ; Clear LINE To Prevent More Grid Lines
  lda #0                    ; [25] + 2
  sta LINE                  ; [27] + 3
  
  ; Calculate Alien Row
  sec                       ; [30] + 2
  tya                       ; [32] + 2
  sbc ALIENY                ; [34] + 3
  bcs A_NoDraw4             ; [37] + 2/4*
  adc #8                    ; [39] + 2
  bcc A_NewSprite2          ; [41] + 2/3
  adc TYPE                  ; [43] + 3
  tax                       ; [46] + 2

  ; Draw Bullets
  A_DRAWBULLETS             ; [48] + 16

  ; Preload Ship Data
  lda (SPTR),Y              ; [64] + 5
  sta GRP0                  ; [69] + 3    VDEL
      
  ; Draw Alien & Ship
  lda A_Aliens,X            ; [72] + 4
  sta GRP1                  ; [0] + 3     > 75
  lda A_AlienCols,X         ; [3] + 4
  sta COLUP1                ; [7] + 3     > 75 < 23    
A_Resume7
  ; Clear Grid Line
  lda #0                    ; [10] + 2
  sta COLUBK                ; [12] + 3    < 23
  
  ; Set Ship Colour
  lda (CPTR),Y              ; [15] + 5
  sta COLUP0                ; [20] + 3    < 23

  ; Reset Stack
  ldx #ENAM1                ; [23] + 2
  txs                       ; [25] + 2

  ; Resume Ship Kernel
  SLEEP 5                   ; [27] + 5
  jmp A_Resume3             ; [32] + 3

  ; Finish Ship Kernel
A_EndShipKernel
  jmp A_TimeBar             ; [40] + 3 (Worst Case)
    
A_NewSprite3
  ; Draw Ship
  lda (SPTR),Y              ; [58] + 5
  sta GRP0                  ; [63] + 3    VDEL
  lda #0                    ; [66] + 2

  ; Check If New Alien Sprite Required    
  ldx INDEX                 ; [68] + 3

  ; Prevent Further Aliens Being Displayed
  sta ALIENY                ; [71] + 3

  ; Draw Ship
  sta GRP1                  ; [74] + 3    > 75 < 23

  ; Return To Ship Kernel If No More Sprites
  bpl A_NewSprite5          ; [1] + 2/4*
  nop                       ; [3] + 2
  jmp A_Resume4             ; [5] + 3
    
A_NoDraw3
  ; Preload Ship Data
  lda (SPTR),Y              ; [54] + 5
  sta GRP0                  ; [59] + 3    VDEL
  SLEEP 14                  ; [62] + 14
  
  ; Display Ship (By Setting GRP1)
  lda #0                    ; [0] + 2
  sta GRP1                  ; [2] + 3     > 75 < 23
  jmp A_Resume4             ; [5] + 3

A_NewSprite2
  ; No Line (Ever)
  SLEEP 10                  ; [44] + 10
  jmp A_NewSprite1          ; [54] + 3

A_EndSK
  if (>A_NewSprite1 != >A_EndSK)
    echo "WARNING: Ship Kernel Crosses Page Boundary!"
  endif

  ; DC.B  "PARTE"

; -----------------------------------------------------------------------------
; PART 7 - REPOSITION ALIEN SPRITE AND DRAW LINE DURING SHIP KERNEL
; -----------------------------------------------------------------------------
    
  ALIGN   256
  
A_NoDraw4
  ; Draw Bullets
  A_DRAWBULLETS             ; [41] + 16
  ldx #ENAM1                ; [57] + 2
  txs                       ; [59] + 2
      
  ; Preload Ship Data
  lda (SPTR),Y              ; [61] + 5
  sta GRP0                  ; [66] + 3    VDEL    
  SLEEP 9                   ; [69] + 9
  
  ; Display Ship (By Setting GRP1)
  lda #0                    ; [2] + 2
  sta GRP1                  ; [4] + 3    > 75 < 23
  jmp A_Resume7             ; [7] + 3
      
A_NewSprite5
  ; Change Ship Colour
  lda (CPTR),Y              ; [5] + 5
  sta COLUP0                ; [10] + 3    > 75 < 23

  ; Draw Gridline
  lda #A_GRIDCOL7           ; [13] + 2    
  sta COLUBK                ; [15] + 3    < 23
  
  ; Store Row Counter
  sty TEMP                  ; [18] + 3

  ; Update Alien Index
  lda INVS,X                ; [21] + 4
  sta INDEX                 ; [25] + 3
  
  ; Calculate Alien Variables    
  ldy INVY,X                ; [28] + 4
  sty ALIENY                ; [32] + 3
  lda INVT,X                ; [35] + 4
  and #%00111100            ; [39] + 2
  tay                       ; [41] + 2
  lda A_AlienSprites,Y      ; [43] + 4
  sta TYPE                  ; [47] + 3

  ; Restore Row Counter
  ldy TEMP                  ; [50] + 3
  dey                       ; [53] + 2

  ; Draw Ship
  lda (SPTR),Y              ; [55] + 5
  sta GRP0                  ; [60] + 3    VDEL    
  lda (CPTR),Y              ; [63] + 5

  ; Decrease Row
  dey                       ; [68] + 2

  ; Set Carry For Repositioning Routine
  sec                       ; [70] + 2
  
  ; Set Ship Colour
  sta COLUP0                ; [72] + 3    > 75 < 23 !!!

  ; Clear Gridline
  lda #0                    ; [75] + 2
  sta COLUBK                ; [1] + 3    > 75 < 23

  ; Draw Spaceship
  sta GRP1                  ; [4] + 3    > 75 < 23 
  
  ; Reposition Alien Sprite
  lda INVX,X                ; [7] + 4
  A_XPOSITION RESP1,HMP1    ; [11]
  sta WSYNC                 ; [0]
  sta HMOVE                 ; [0] + 3
  
    ; Preload Ship Data
  lda (SPTR),Y              ; [3] + 5
  sta GRP0                  ; [8] + 3    VDEL    
  lda #0                    ; [11] + 2
  sta GRP1                  ; [13] + 3
  
    ; Set Ship Colour
  lda (CPTR),Y              ; [16] + 5
  sta COLUP0                ; [21] + 3    < 23 !!!

  ; Clear LINE To Prevent Further Gridlines
  lda #0                    ; [24] + 2
  sta LINE                  ; [26] + 3
  
  ; Resume Kernel
  SLEEP 3                   ; [29] + 3
  jmp A_Resume3             ; [32] + 3
A_EndNewSprite5
  if (>A_NewSprite5 != >A_EndNewSprite5)
    echo "WARNING: Ship Kernel Repositioning Crosses Page Boundary!"
  endif    

; Alien Size Table (128 Entries)
A_AlienSize
  DS.B  96, 0             ; 0 -> 96
  DS.B  24, 1             ; 96 -> 120
  DS.B  8,  2             ; 120 -> 128
A_EndAlienSize
  if (>A_AlienSize != >A_EndAlienSize)
    echo "WARNING: Alien Size Table Crosses Page Boundary!"
  endif

; Grid Line Colours (8 Entries)
A_GridCols
  DC.B  A_GRIDCOL7, A_GRIDCOL6, A_GRIDCOL5, A_GRIDCOL4
  DC.B  A_GRIDCOL3, A_GRIDCOL2, A_GRIDCOL1, A_GRIDCOL0
A_EndGridCols
  if (>A_GridCols != >A_EndGridCols)
    echo "WARNING: Grid Colours Cross Page Boundary!"
  endif

  DC.B  "PARTF"

; -----------------------------------------------------------------------------
; PART 8 - DISPLAY TIME (FUEL) REMAINING
; -----------------------------------------------------------------------------

  ALIGN 256

A_TimeBar
  ; Calculate Time Remaining
  lda TIME                  ; [43] + 3
  and #%00111111            ; [46] + 2
  tax                       ; [48] + 2

   ; Set Timebar Colour & Preload Data
  lda A_TimeCol,X           ; [50] + 4
  sta COLUPF                ; [54] + 3
  lda A_PF0ATime,X          ; [57] + 4

  ; Clear Sprite Positions & Data (Delayed)
  ldy #0                    ; [61] + 2
  sty HMCLR                 ; [63] + 3
  sty GRP0                  ; [66] + 3

  ; Clear Sprite Movements
  sta WSYNC                 ; [0]
  sty COLUBK                ; [0] + 3

  ; Draw Time Bar (First Part)
  sta PF0                   ; [3] + 3     > 0 < 23
  
  ; Clear Sprite Data & Disable Missiles
  sty GRP1                  ; [6] + 3
  sty GRP0                  ; [9] + 3
  sty ENAM0                 ; [12] + 3
  sty ENAM1                 ; [15] + 3
  
  ; Draw Time Bar (Second Part)
  lda A_PF1ATime,X          ; [18] + 4
  sta PF1                   ; [22] + 3    < 28
  lda A_PF2ATime,X          ; [25] + 4
  sta PF2                   ; [29] + 3    < 38
  lda A_PF2BTime,X          ; [32] + 4

  ; Set Coarse Sprite Positions
  nop                       ; [36] + 2
  sta RESP0                 ; [38] + 3    = 41 EXACT
  sta RESP1                 ; [41] + 3    = 44 EXACT

  ; Display Time Bar (Third Part)
  sta.w PF2                 ; [44] + 4    = 48 EXACT
  lda A_PF1BTime,X          ; [48] + 4
  sta PF1                   ; [52] + 3    > 38 < 60
  lda A_PF0BTime,X          ; [55] + 4
  sta PF0                   ; [59] + 3    > 28 < 70

  ; Set Sprite Offsets For Score (Wont Change Until HMOVE)
  lda #%00010000            ; [62] + 2
  sta HMP1                  ; [64] + 3
  
  ; Set Delay and Three Copies For Score
  lda #%00000011            ; [67] + 2
  sta VDELP1                ; [69] + 3
  sta NUSIZ0                ; [72] + 3
  sta NUSIZ1                ; [75] + 3
  
  lda A_PF0ATime,X          ; [2] + 4
  sta PF0                   ; [6] + 3     > 0 < 23
  lda A_PF1ATime,X          ; [9] + 4
  sta PF1                   ; [13] + 3    < 28
  lda A_PF2ATime,X          ; [16] + 4
  sta PF2                   ; [20] + 3    < 38
  nop                       ; [23] + 2
  nop                       ; [25] + 2
  lda A_PF0BTime,X          ; [27] + 4
  sta PF0                   ; [31] + 3    > 28 < 70
  lda A_PF1BTime,X          ; [34] + 4
  sta PF1                   ; [38] + 3    > 38 < 60
  lda A_PF2BTime,X          ; [41] + 4
  sta PF2                   ; [45] + 3    = 48 EXACT
 
  ; Set Font Pointers (MSB)
  lda #>NumberLetters       ; [48] + 2
  sta INVS+11               ; [50] + 3
  sta INVS+9                ; [53] + 3
  sta INVS+7                ; [56] + 3
  sta INVS+5                ; [59] + 3
  sta INVS+3                ; [62] + 3
  sta INVS+1                ; [65] + 3

  ; Clear Line & Reposition Sprites
  ldy #0                    ; [68] + 2
  sty PF2                   ; [70] + 3    > 60
  sty PF1                   ; [73] + 3    > 70
  sta HMOVE                 ; [0] + 3
  sty PF0                   ; [3] + 3     >= 76

  ; Finished Kernel
  jmp KernelExit1           ; [6] + 3

  DC.B  "PARTG"

; -----------------------------------------------------------------------------
; PART 9 - REPOSITIONING KERNELS
; -----------------------------------------------------------------------------
    
  ALIGN   256
  
A_ReposFunctions0
  ; Not Enough Space In This Page For These
A_Repos6
  jmp A_Repos66             ; [11] + 3    = 14
A_Repos7
  jmp A_Repos77             ; [11] + 3
A_Repos8
  jmp A_Repos88             ; [11] + 3
A_Repos9
  jmp A_Repos99             ; [11] + 3
  
A_NoLine0
  SLEEP 5                   ; [17] + 5
  sta RESP1                 ; [22] + 3    = 25
  nop                       ; [25] + 2
  jmp A_EndLine0            ; [27] + 3
  
A_Repos0
  ; Check Line
  cpy LINE                  ; [11] + 3
  bcs A_NoLine0             ; [14] + 2/3

  ; Draw Line
  lda #A_GRIDCOL7           ; [16] + 2
  sta.w COLUBK              ; [18] + 4    < 23

  ; Reposition Sprite (Coarse)
  sta RESP1                 ; [22] + 3    = 25

  ; Clear Line Counter
  lda #0                    ; [25] + 2
  sta LINE                  ; [27] + 3
A_EndLine0
  
  ; Calculate Sprite Offset
  txa                       ; [30] + 2
  sec                       ; [32] + 2
  sbc #15                   ; [34] + 2
  ; 4 CYCLE GAP

A_Continue0
  ; Reposition Sprite (Fine)
  tax                       ; [40] + 2
  lda A_FineTuneEnd,X       ; [42] + 5
  sta HMP1                  ; [47] + 3

A_Continue1
  ; Decrease Line Counter
  dey                       ; [50] + 2

A_Continue2
  ; Preload Ship Data
  lda (SPTR),Y              ; [52] + 5
  sta GRP0                  ; [57] + 3    VDEL
  
  ; Preload Ship Colour
  lda (CPTR),Y              ; [60] + 5 

  ; Check Line
  cpy LINE                  ; [65] + 3
  bcc A_DrawLine3           ; [68] + 2/3

A_Continue3
  ; Set Line Colour
  ldx #0                    ; [71] + 2
  
  ; Fine Tune Sprite Position
  sta WSYNC                 ; [73] + 3
A_Continue4
  sta HMOVE                 ; [0] + 3

  ; Draw Ship
  sta COLUP0                ; [3] + 3    > 75 < 23
  stx GRP1                  ; [6] + 3    < 23

  ; Clear Background
  stx COLUBK                ; [9] + 3    < 23

  ; Draw Bullets
  A_DRAWBULLETS             ; [12] + 16
  ldx #ENAM1                ; [28] + 2
  txs                       ; [30] + 2
  
  ; Resume Kernel
  jmp A_Resume3             ; [32] + 3

A_DrawLine3
  ; Set Line Colour
  ldx #A_GRIDCOL7           ; [71] + 2
  
  ; Fine Tune Sprite Position
  sta WSYNC                 ; [73] + 3 
A_Continue5
  sta HMOVE                 ; [0] + 3
  
  ; Set Ship Colour 
  sta COLUP0                ; [3] + 3    > 75 < 23

A_Continue6
  ; Draw Line
  stx COLUBK                ; [6] + 3    < 23

  ; Draw Ship
  lda #0                    ; [9] + 2
  sta GRP1                  ; [11] + 3    < 23

  ; Resume
  SLEEP 4                   ; [14] + 4
  jmp A_Resume5             ; [18] + 3
  
A_NoLine1
  SLEEP 6                   ; [17] + 6
  bcs A_EndLine1            ; [23] + 3

A_Repos1
  ; Check Line
  cpy LINE                  ; [11] + 3
  bcs A_NoLine1             ; [14] + 2/3

  ; Draw Line
  lda #A_GRIDCOL7           ; [16] + 2
  sta COLUBK                ; [18] + 3    < 23

  ; Clear Line Counter
  lda #0                    ; [21] + 2
  sta LINE                  ; [23] + 3
A_EndLine1
  
  ; Reposition Sprite (Coarse)
  sta.w RESP1               ; [26] + 4    = 30

  ; Calculate Sprite Offset
  txa                       ; [30] + 2
  sec                       ; [32] + 2
  sbc #30                   ; [34] + 2
  jmp A_Continue0           ; [36] + 3    <= 40

A_NoLine2
  SLEEP 6                   ; [17] + 6
  bcs A_EndLine2            ; [23] + 3
  
A_Repos2
  ; Check Line
  cpy LINE                  ; [11] + 3
  bcs A_NoLine2             ; [14] + 2/3

  ; Draw Line
  lda #A_GRIDCOL7           ; [16] + 2
  sta COLUBK                ; [18] + 3    < 23

  ; Clear Line Counter
  lda #0                    ; [21] + 2
  sta LINE                  ; [23] + 3
A_EndLine2
  
  ; Calculate Sprite Offset
  txa                       ; [26] + 2
  sec                       ; [28] + 2
  sbc #45                   ; [30] + 2

  ; Reposition Sprite (Coarse)
  sta RESP1                 ; [32] + 3    = 35
  jmp A_Continue0           ; [35] + 3    <= 40

A_NoLine3
  SLEEP 6                   ; [17] + 6
  bcs A_EndLine3            ; [23] + 3
  
A_Repos3
  ; Check Line
  cpy LINE                  ; [11] + 3
  bcs A_NoLine3             ; [14] + 2/3

  ; Draw Line
  lda #A_GRIDCOL7           ; [16] + 2
  sta COLUBK                ; [18] + 3    < 23

  ; Clear Line Counter
  lda #0                    ; [21] + 2
  sta LINE                  ; [23] + 3
A_EndLine3
  
  ; Reposition Sprite (Fine)
  txa                       ; [26] + 2
  sec                       ; [28] + 2
  sbc #60                   ; [30] + 2
  tax                       ; [32] + 2

  ; Decrease Line Counter
  dey                       ; [34] + 2

  ; Reposition Sprite (Coarse)
  sta.w RESP1               ; [36] + 4    = 40

  ; Finish Fine Tuning
  lda A_FineTuneEnd,X       ; [40] + 5
  sta HMP1                  ; [45] + 3
  jmp A_Continue2           ; [48] + 3    <= 52

A_NoLine4
  SLEEP 6                   ; [17] + 6
  bcs A_EndLine4            ; [23] + 3
  
A_Repos4
  ; Check Line
  cpy LINE                  ; [11] + 3
  bcs A_NoLine4             ; [14] + 2/3

  ; Draw Line
  lda #A_GRIDCOL7           ; [16] + 2
  sta COLUBK                ; [18] + 3    < 23

  ; Clear Line Counter
  lda #0                    ; [21] + 2
  sta LINE                  ; [23] + 3
A_EndLine4
  
  ; Reposition Sprite (Fine)
  txa                       ; [26] + 2
  sec                       ; [28] + 2
  sbc #75                   ; [30] + 2
  tax                       ; [32] + 2
  lda A_FineTuneEnd,X       ; [34] + 5
  sta HMP1                  ; [39] + 3

  ; Reposition Sprite (Coarse)
  sta RESP1                 ; [42] + 3    = 45
  jmp A_Continue1           ; [45] + 3    <= 50

A_NoLine5
  SLEEP 6                   ; [17] + 6
  bcs A_EndLine5            ; [23] + 3    

A_Repos5
  ; Check Line
  cpy LINE                  ; [11] + 3
  bcs A_NoLine5             ; [14] + 2/3

  ; Draw Line
  lda #A_GRIDCOL7           ; [16] + 2
  sta COLUBK                ; [18] + 3    < 23

  ; Clear Line Counter
  lda #0                    ; [21] + 2
  sta LINE                  ; [23] + 3
A_EndLine5
  
  ; Reposition Sprite (Fine)
  txa                       ; [26] + 2
  sec                       ; [28] + 2
  sbc #90                   ; [30] + 2
  tax                       ; [32] + 2
  lda A_FineTuneEnd,X       ; [34] + 5
  sta HMP1                  ; [39] + 3

  ; Decrease Line Counter
  dey                       ; [42] + 2

  ; Continue in Next Page
  jmp A_Repos5Cont          ; [44] + 3
A_EndReposFunctions0
  if (>A_ReposFunctions0 != >A_EndReposFunctions0)
    echo "WARNING: Repositioning0 Crosses Page Boundary!"
  endif
  
  ; DC.B  "PARTH"
  
  ALIGN   256

A_ReposFunctions1

A_Repos5Cont
  ; Reposition Sprite (Coarse)
  sta RESP1                 ; [47] + 3    = 50

  ; Preload Ship Data
  lda (SPTR),Y              ; [50] + 5
  sta GRP0                  ; [55] + 3    VDEL
  
  ; Preload Ship Colour
  lda (CPTR),Y              ; [58] + 5 

  ; Check Line
  cpy LINE                  ; [63] + 3
  bcc A_DrawLine4           ; [66] + 2/3
  jmp A_Continue3           ; [68] + 3     <= 71

A_DrawLine4
  ldx #A_GRIDCOL7           ; [69] + 2
  nop                       ; [71] + 2
  jmp A_Continue5           ; [73] + 3    = 76
      
A_NoLine66
  SLEEP 6                   ; [20] + 6
  bcs A_EndLine66           ; [26] + 3    
  
A_Repos66
  ; Check Line
  cpy LINE                  ; [14] + 3
  bcs A_NoLine66            ; [17] + 2/3

  ; Draw Line
  lda #A_GRIDCOL7           ; [19] + 2
  sta COLUBK                ; [21] + 3    < 23!

  ; Clear Line Counter
  lda #0                    ; [24] + 2
  sta LINE                  ; [26] + 3
A_EndLine66
  
  ; Reposition Sprite (Fine)
  txa                       ; [29] + 2
  sec                       ; [31] + 2
  sbc #105                  ; [33] + 2
  tax                       ; [35] + 2
  lda A_FineTuneEnd,X       ; [37] + 5
  sta HMP1                  ; [42] + 3

  ; Decrease Line Counter
  dey                       ; [45] + 2
  
  ; Preload Ship Data
  lda (SPTR),Y              ; [47] + 5

  ; Reposition Sprite (Coarse)    
  sta RESP1                 ; [52] + 3    = 55

  ; Store Ship Data
  sta GRP0                  ; [55] + 3    VDEL

  ; Preload Ship Colour
  lda (CPTR),Y              ; [58] + 5 

  ; Check Line
  cpy LINE                  ; [63] + 3
  bcc A_DrawLine5           ; [66] + 2/3
  jmp A_Continue3           ; [68] + 3     <= 71

A_DrawLine5
  ldx #A_GRIDCOL7           ; [69] + 2
  nop                       ; [71] + 2
  jmp A_Continue5           ; [73] + 3    = 76
    
A_NoLine77
  SLEEP 6                   ; [20] + 6
  bcs A_EndLine77           ; [26] + 3
  
A_Repos77
  ; Check Line
  cpy LINE                  ; [14] + 3
  bcs A_NoLine77            ; [17] + 2/3

  ; Draw Line
  lda #A_GRIDCOL7           ; [19] + 2
  sta COLUBK                ; [21] + 3    < 23!

  ; Clear Line Counter
  lda #0                    ; [24] + 2
  sta LINE                  ; [26] + 3
A_EndLine77
  
  ; Reposition Sprite (Fine)
  txa                       ; [29] + 2
  sec                       ; [31] + 2
  sbc #120                  ; [33] + 2
  tax                       ; [35] + 2
  lda A_FineTuneEnd,X       ; [37] + 5
  sta HMP1                  ; [42] + 3

  ; Decrease Line Counter
  dey                       ; [45] + 2
  
  ; Preload Ship Data
  lda (SPTR),Y              ; [47] + 5
  sta GRP0                  ; [52] + 3    VDEL

  ; Set Line Colour
  ldx #0                    ; [55] + 2
      
  ; Reposition Sprite (Coarse)
  sta RESP1                 ; [57] + 3    = 60

  ; Preload Ship Colour
  lda (CPTR),Y              ; [60] + 5 
  cpy LINE                  ; [65] + 3
  bcc A_DrawLine6           ; [68] + 2/3
  SLEEP 3                   ; [70] + 3
  jmp A_Continue4           ; [73] + 3

A_DrawLine6
  ; Set Line Colour
  ldx #A_GRIDCOL7           ; [71] + 2
  jmp A_Continue5           ; [73] + 3

A_NoLine88
  SLEEP 6                   ; [20] + 6
  bcs A_EndLine88           ; [26] + 3

A_Repos88
  ; Check Line
  cpy LINE                  ; [14] + 3
  bcs A_NoLine88            ; [17] + 2/3

  ; Draw Line
  lda #A_GRIDCOL7           ; [19] + 2
  sta COLUBK                ; [21] + 3    < 23!

  ; Clear Line Counter
  lda #0                    ; [24] + 2
  sta LINE                  ; [26] + 3
A_EndLine88
    
  ; Reposition Sprite (Fine)
  txa                       ; [29] + 2
  sec                       ; [31] + 2
  sbc #135                  ; [33] + 2
  tax                       ; [35] + 2
  lda A_FineTuneEnd,X       ; [37] + 5
  sta HMP1                  ; [42] + 3

  ; Decrease Line Counter
  dey                       ; [45] + 2
  
  ; Preload Ship Data
  lda (SPTR),Y              ; [47] + 5
  sta GRP0                  ; [52] + 3    VDEL

  ; Preload Ship Colour
  lda (CPTR),Y              ; [55] + 5

  ; Set Line Colour
  ldx #0                    ; [60] + 2

  ; Reposition Sprite (Coarse)
  sta RESP1                 ; [62] + 3    = 65

  ; Check Line
  cpy LINE                  ; [65] + 3
  bcc A_DrawLine6           ; [68] + 2/3
  SLEEP 3                   ; [70] + 3
  jmp A_Continue4           ; [73] + 3

A_NoLine99
  SLEEP 6                   ; [20] + 6
  bcs A_EndLine99           ; [26] + 3    

A_Repos99
  ; Check Line
  cpy LINE                  ; [14] + 3
  bcs A_NoLine99            ; [17] + 2/3

  ; Draw Line
  lda #A_GRIDCOL7           ; [19] + 2
  sta COLUBK                ; [21] + 3    < 23!

  ; Clear Line Counter
  lda #0                    ; [24] + 2
  sta LINE                  ; [26] + 3
A_EndLine99
    
  ; Reposition Sprite (Fine)
  txa                       ; [29] + 2
  sec                       ; [31] + 2
  sbc #150                  ; [33] + 2
  tax                       ; [35] + 2
  lda A_FineTuneEnd,X       ; [37] + 5
  sta HMP1                  ; [42] + 3

  ; Decrease Line Counter
  dey                       ; [45] + 2

  ; Preload Ship Data
  lda (SPTR),Y              ; [47] + 5
  sta GRP0                  ; [52] + 3    VDEL

  ; Preload Ship Colour
  lda (CPTR),Y              ; [55] + 5

  ; Check Line
  cpy LINE                  ; [60] + 3
  bcc A_DrawLine7           ; [63] + 2/3

  ; Set Line Colour
  ldx #0                    ; [65] + 2

  ; Reposition Sprite (Coarse)
  sta RESP1                 ; [67] + 3    = 70
  SLEEP 3                   ; [70] + 3
  jmp A_Continue4           ; [73] + 3
    
A_DrawLine7
  ; Reposition Sprite (Coarse)    
  sta.w RESP1               ; [66] + 4    = 70

  ; Set Line Colour
  ldx #A_GRIDCOL7           ; [70] + 2
  sta.w COLUP0              ; [72] + 4
  
  ; Reposition & Continue
  sta HMOVE                 ; [0] + 3
  jmp A_Continue6           ; [3] + 3
A_EndReposFunctions1
  if (>A_ReposFunctions1 != >A_EndReposFunctions1)
    echo "WARNING: Repositioning1 Crosses Page Boundary!"
  endif

  DC.B  "PARTI"

; -----------------------------------------------------------------------------
; PART 10 - KERNEL DATA (LOOKUP TABLES, SPRITES, etc.)
; -----------------------------------------------------------------------------

  ALIGN   256

  ; Include Alien Sprite Data
A_Aliens
A_Empty                     ; Empty Sprite
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000           ; Extra Bit
A_BigUFO_1                  ; UFO Big
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00111000
  DC.B  %01111100
  DC.B  %10101010
  DC.B  %01111100
  DC.B  %00111000
A_BigUFO_2                  ; UFO Medium
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00010000
  DC.B  %00111000
  DC.B  %01010100
  DC.B  %00111000
  DC.B  %00010000
A_BigUFO_3                  ; UFO Small
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000    
  DC.B  %00011000
  DC.B  %00111100
  DC.B  %00011000
A_BigUFO_4
  DC.B  %00011000           ; UFO Tiny
  DC.B  %00011000
A_SmallUFO_1                ; UFO Big
  DC.B  %00000000
  DC.B  %00000000    
  DC.B  %00111000
  DC.B  %01111100
  DC.B  %10101010
  DC.B  %01111100
  DC.B  %00101000
  DC.B  %00010000
A_SmallUFO_2                ; UFO Medium
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00010000
  DC.B  %00111000
  DC.B  %01010100
  DC.B  %00111000
  DC.B  %00010000
A_SmallUFO_3                ; UFO Small
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000    
  DC.B  %00010000
  DC.B  %00111000
  DC.B  %00010000
A_SmallUFO_4
  DC.B  %00011000           ; UFO Tiny
  DC.B  %00011000    
A_Alien1_1                  ; Alien1 Big (Frame 1)
  DC.B  %10011001
  DC.B  %01011010
  DC.B  %00111100
  DC.B  %01111110
  DC.B  %10101001
  DC.B  %01101010
  DC.B  %00111100
  DC.B  %00011000
A_Alien1_2                  ; Alien1 Big (Frame 2)
  DC.B  %10011001
  DC.B  %01011010
  DC.B  %00111100
  DC.B  %01111110
  DC.B  %10010101
  DC.B  %01010110
  DC.B  %00111100
  DC.B  %00011000
A_Alien1_3                  ; Alien 1 Medium
  DC.B  %00000000
  DC.B  %00000000    
  DC.B  %01010100
  DC.B  %00111000
  DC.B  %00111000
  DC.B  %01010100
  DC.B  %00111000
  DC.B  %00010000
A_Alien1_4                  ; Alien 1 Small
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00101000
  DC.B  %00010000
  DC.B  %00111000
  DC.B  %00010000
A_Alien1_5
  DC.B  %00011000           ; Alien 1 Tiny
  DC.B  %00011000
A_Alien2_1                  ; Alien 2 Big
  DC.B  %00101000
  DC.B  %00101000
  DC.B  %01111100
  DC.B  %10111010
  DC.B  %11111110
  DC.B  %01000100
  DC.B  %00111000
  DC.B  %00010000
A_Alien2_2                  ; Alien 2 Big (Frame 2)
  DC.B  %10010010
  DC.B  %01010100
  DC.B  %01111100
  DC.B  %10111010
  DC.B  %11111110
  DC.B  %01000100
  DC.B  %00111000
  DC.B  %00010000
A_Alien2_3                  ; Alien 2 Medium (Frame 1)
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00101000
  DC.B  %00111000
  DC.B  %01010100
  DC.B  %01111100
  DC.B  %00101000
  DC.B  %00010000
A_Alien2_4                  ; Alien 2 Medium (Frame 2)
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %01010100
  DC.B  %00111000
  DC.B  %01010100
  DC.B  %01111100
  DC.B  %00101000
  DC.B  %00010000
A_Alien2_5                  ; Alien 2 Small
  DC.B  %00000000
  DC.B  %00000000    
  DC.B  %00000000    
  DC.B  %00000000
  DC.B  %00101000
  DC.B  %00010000
  DC.B  %00101000
  DC.B  %00010000
A_Alien2_6
  DC.B  %00011000           ; Alien 2 Tiny
  DC.B  %00011000
A_BugAlien_1                ; Bug Alien Big
  DC.B  %01010100
  DC.B  %00111000
  DC.B  %11111110
  DC.B  %01101100
  DC.B  %01101100
  DC.B  %00101000
  DC.B  %01010100
  DC.B  %00010000
A_BugAlien_2                ; Bug Alien Medium
  DC.B  %00000000
  DC.B  %00000000    
  DC.B  %00100100
  DC.B  %01111110
  DC.B  %00111100
  DC.B  %00111100
  DC.B  %00011000
  DC.B  %00100100
A_BugAlien_3                ; Bug Alien Small
  DC.B  %00000000
  DC.B  %00000000    
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00111000
  DC.B  %00111000
  DC.B  %00010000
  DC.B  %00101000
A_BugAlien_4
  DC.B  %00011000           ; Bug Alien Tiny
  DC.B  %00011000
A_Sphere_1                  ; Sphere Big
  DC.B  %00111000
  DC.B  %01001100
  DC.B  %10010110
  DC.B  %11110010
  DC.B  %11001110
  DC.B  %10101010
  DC.B  %01110100
  DC.B  %00111000
A_Sphere_2                  ; Sphere Medium
  DC.B  %00000000
  DC.B  %00011000
  DC.B  %00110100
  DC.B  %01011010
  DC.B  %01110110
  DC.B  %01011010
  DC.B  %00101100
  DC.B  %00011000
A_Sphere_3                  ; Sphere Small
  DC.B  %00000000    
  DC.B  %00000000
  DC.B  %00010000
  DC.B  %00111000
  DC.B  %01110100
  DC.B  %01011100
  DC.B  %00101000
  DC.B  %00010000
A_Sphere_4
  DC.B  %00011000           ; Sphere Tiny
  DC.B  %00011000
A_Astronaut_1               ; Astronaut Big
  DC.B  %00101000
  DC.B  %00101000
  DC.B  %00111000
  DC.B  %01111100
  DC.B  %01010100
  DC.B  %00101000
  DC.B  %00101000
  DC.B  %00010000
A_Astronaut_2               ; Astronaut Medium
  DC.B  %00000000
  DC.B  %00000000    
  DC.B  %00101000
  DC.B  %00111000
  DC.B  %00010000
  DC.B  %00111000
  DC.B  %00101000
  DC.B  %00010000
A_Astronaut_3               ; Astronaut Small
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000    
  DC.B  %00101000
  DC.B  %00010000
  DC.B  %00111000
  DC.B  %00010000
A_Explosion_1               ; Alien Explosion (Small)
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00010100
  DC.B  %00011000
  DC.B  %00100100
  DC.B  %00001000
  DC.B  %00000000
  DC.B  %00000000
A_Explosion_2               ; Alien Explosion (Medium)
  DC.B  %00000000
  DC.B  %00010100
  DC.B  %00101000
  DC.B  %00011010
  DC.B  %01010100
  DC.B  %00101000
  DC.B  %00001000
  DC.B  %00000000
A_Explosion_3               ; Alien Explosion (Big1)
  DC.B  %00011000
  DC.B  %00100100
  DC.B  %00011100
  DC.B  %10101010
  DC.B  %00111100
  DC.B  %01011010
  DC.B  %00000100
  DC.B  %00010000
A_Explosion_4               ; Alien Explosion (Big2)
  DC.B  %00101000
  DC.B  %01100101
  DC.B  %00010000
  DC.B  %01001010
  DC.B  %00000101
  DC.B  %10001110
  DC.B  %00100010
  DC.B  %01010100
A_Explosion_5               ; Alien Explosion (Big3)
  DC.B  %00100001
  DC.B  %00000000
  DC.B  %10000000
  DC.B  %00000000
  DC.B  %00000100
  DC.B  %10000001
  DC.B  %00100000
  DC.B  %00000001
A_EndAliens
  if (>A_Aliens != >A_EndAliens)
    echo "WARNING: Alien Sprite Data crosses a page boundary!"
  endif
  
  ALIGN   256
  
  ; Include Alien Sprite Colours
A_AlienCols
  IF (PALCOLS)
A_EmptyCol
  DC.B  $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
A_BigUFOCol_1
  DC.B  $0E, $0E, $0E, $66, $88, $88, $88, $66
A_BigUFOCol_2
  DC.B  $0E, $0E, $0E, $66, $88, $88, $88, $66
A_BigUFOCol_3
  DC.B  $0E, $0E, $0E, $0E, $0E, $66, $88, $66
A_BigUFOCol_4
  DC.B  $88, $66
A_SmallUFOCol_1
  DC.B  $0E, $0E, $36, $64, $64, $28, $28, $28
A_SmallUFOCol_2
  DC.B  $0E, $0E, $0E, $36, $64, $28, $28, $28
A_SmallUFOCol_3
  DC.B  $0E, $0E, $0E, $0E, $0E, $36, $28, $64
A_SmallUFOCol_4
  DC.B  $64, $36
A_Alien1Col_1
  DC.B  $64, $64, $62, $28, $28, $64, $28, $28
A_Alien1Col_2
  DC.B  $64, $64, $62, $28, $28, $64, $28, $28
A_Alien1Col_3
  DC.B  $0E, $0E, $64, $62, $28, $28, $64, $28
A_Alien1Col_4
  DC.B  $0E, $0E, $0E, $0E, $64, $62, $28, $28
A_Alien1Col_5
  DC.B  $28, $64
A_Alien2Col_1
  DC.B  $64, $64, $D6, $D6, $9A, $9A, $9A, $9A
A_Alien2Col_2
  DC.B  $64, $64, $D6, $D6, $9A, $9A, $9A, $9A
A_Alien2Col_3
  DC.B  $0E, $0E, $64, $D6, $D6, $9A, $9A, $9A
A_Alien2Col_4
  DC.B  $0E, $0E, $64, $D6, $D6, $9A, $9A, $9A
A_Alien2Col_5
  DC.B  $0E, $0E, $0E, $0E, $64, $D6, $9A, $9A
A_Alien2Col_6
  DC.B  $9A, $64
A_BugAlienCol_1
  DC.B  $36, $36, $36, $88, $88, $88, $36, $28
A_BugAlienCol_2
  DC.B  $0E, $0E, $36, $36, $88, $88, $88, $36
A_BugAlienCol_3
  DC.B  $0E, $0E, $0E, $0E, $36, $88, $88, $36
A_BugAlienCol_4
  DC.B  $88, $36
A_SphereCol_1
  DC.B  $D6, $D6, $D6, $9A, $9A, $9A, $9A, $9A
A_SphereCol_2
  DC.B  $0E, $D6, $D6, $D6, $9A, $9A, $9A, $9A
A_SphereCol_3
  DC.B  $0E, $0E, $D6, $D6, $9A, $9A, $9A, $9A
A_SphereCol_4
  DC.B  $D6, $9A
A_AstronautCol_1
  DC.B  $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
A_AstronautCol_2
  DC.B  $0E, $0E, $0C, $0C, $0C, $0C, $0C, $0C
A_AstronautCol_3
  DC.B  $0E, $0E, $0E, $0E, $0A, $0A, $0A, $0A
A_ExplosionCol_1
  DC.B  $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
A_ExplosionCol_2    
  DC.B  $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
A_ExplosionCol_3        
  DC.B  $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
A_ExplosionCol_4    
  DC.B  $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
A_ExplosionCol_5    
  DC.B  $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
  ELSE
A_EmptyCol
  DC.B  $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
A_BigUFOCol_1
  DC.B  $0E, $0E, $0E, $46, $58, $58, $58, $46
A_BigUFOCol_2
  DC.B  $0E, $0E, $0E, $46, $58, $58, $58, $46
A_BigUFOCol_3
  DC.B  $0E, $0E, $0E, $0E, $0E, $46, $58, $46
A_BigUFOCol_4
  DC.B  $58, $46
A_SmallUFOCol_1
  DC.B  $0E, $0E, $D6, $44, $44, $28, $28, $28
A_SmallUFOCol_2
  DC.B  $0E, $0E, $0E, $D6, $44, $28, $28, $28
A_SmallUFOCol_3
  DC.B  $0E, $0E, $0E, $0E, $0E, $D6, $28, $44
A_SmallUFOCol_4
  DC.B  $44, $D6
A_Alien1Col_1
  DC.B  $44, $44, $42, $28, $28, $44, $28, $28
A_Alien1Col_2
  DC.B  $44, $44, $42, $28, $28, $44, $28, $28
A_Alien1Col_3
  DC.B  $0E, $0E, $44, $42, $28, $28, $44, $28
A_Alien1Col_4
  DC.B  $0E, $0E, $0E, $0E, $44, $42, $28, $28
A_Alien1Col_5
  DC.B  $28, $44
A_Alien2Col_1
  DC.B  $44, $44, $86, $86, $AA, $AA, $AA, $AA
A_Alien2Col_2
  DC.B  $44, $44, $86, $86, $AA, $AA, $AA, $AA
A_Alien2Col_3
  DC.B  $0E, $0E, $44, $86, $86, $AA, $AA, $AA
A_Alien2Col_4
  DC.B  $0E, $0E, $44, $86, $86, $AA, $AA, $AA
A_Alien2Col_5
  DC.B  $0E, $0E, $0E, $0E, $44, $86, $AA, $AA
A_Alien2Col_6
  DC.B  $AA, $44
A_BugAlienCol_1
  DC.B  $D6, $D6, $D6, $58, $58, $58, $D6, $18
A_BugAlienCol_2
  DC.B  $0E, $0E, $D6, $D6, $58, $58, $58, $D6
A_BugAlienCol_3
  DC.B  $0E, $0E, $0E, $0E, $D6, $58, $58, $D6
A_BugAlienCol_4
  DC.B  $58, $D6
A_SphereCol_1
  DC.B  $86, $86, $86, $AA, $AA, $AA, $AA, $AA
A_SphereCol_2
  DC.B  $0E, $86, $86, $86, $AA, $AA, $AA, $AA
A_SphereCol_3
  DC.B  $0E, $0E, $86, $86, $AA, $AA, $AA, $AA
A_SphereCol_4
  DC.B  $86, $AA
A_AstronautCol_1
  DC.B  $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
A_AstronautCol_2
  DC.B  $0E, $0E, $0C, $0C, $0C, $0C, $0C, $0C
A_AstronautCol_3
  DC.B  $0E, $0E, $0E, $0E, $0A, $0A, $0A, $0A
A_ExplosionCol_1
  DC.B  $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
A_ExplosionCol_2    
  DC.B  $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
A_ExplosionCol_3        
  DC.B  $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
A_ExplosionCol_4    
  DC.B  $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
A_ExplosionCol_5    
  DC.B  $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
  ENDIF
A_EndAlienCols
  if (>A_AlienCols != >A_EndAlienCols)
    echo "WARNING: Alien Colour Data crosses a page boundary!"
  endif

  ALIGN   256

  ; Include Ship Sprite Data
A_Ship
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00111000
  DC.B  #%11111110
  DC.B  #%11010110
  DC.B  #%11000110
  DC.B  #%11010110
  DC.B  #%01111100
  DC.B  #%01111100
  DC.B  #%01101100
  DC.B  #%00101000
  DC.B  #%00101000
  DC.B  #%00111000
  DC.B  #%00010000
  DC.B  #%00010000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
A_EmptyShip
  DS.B  40,0
A_ForwardFlame1
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000 
  DC.B  #%00010000
  DC.B  #%00010000
  DC.B  #%00111000
  DC.B  #%00101000
  DC.B  #%01000100
  DC.B  #%01010100
  DC.B  #%00101000
  DC.B  #%00010000
  DC.B  #%00111000
  DC.B  #%11111110
  DC.B  #%11010110
  DC.B  #%11000110
  DC.B  #%11010110
  DC.B  #%01111100
  DC.B  #%01111100
  DC.B  #%01101100
  DC.B  #%00101000
  DC.B  #%00101000
  DC.B  #%00111000
  DC.B  #%00010000
  DC.B  #%00010000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
A_ForwardFlame2
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00010000
  DC.B  #%00010000
  DC.B  #%00111000
  DC.B  #%00101000
  DC.B  #%00101000
  DC.B  #%01000100
  DC.B  #%01010100
  DC.B  #%01010100
  DC.B  #%00111000
  DC.B  #%00101000
  DC.B  #%00010000
  DC.B  #%00111000
  DC.B  #%11111110
  DC.B  #%11010110
  DC.B  #%11000110
  DC.B  #%11010110
  DC.B  #%01111100
  DC.B  #%01111100
  DC.B  #%01101100
  DC.B  #%00101000
  DC.B  #%00101000
  DC.B  #%00111000
  DC.B  #%00010000
  DC.B  #%00010000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
A_ReverseFlame1
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00111000
  DC.B  #%11111110
  DC.B  #%11010110
  DC.B  #%11000110
  DC.B  #%11010110
  DC.B  #%01111100
  DC.B  #%01111100
  DC.B  #%01101100
  DC.B  #%00101000
  DC.B  #%00101000
  DC.B  #%00111000
  DC.B  #%00010000
  DC.B  #%00010000
  DC.B  #%01000100
  DC.B  #%01000100
  DC.B  #%01101100
  DC.B  #%00101000
  DC.B  #%00101000
  DC.B  #%00000000
  DC.B  #%00101000
  DC.B  #%00000000
A_ReverseFlame2
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00111000
  DC.B  #%11111110
  DC.B  #%11010110
  DC.B  #%11000110
  DC.B  #%11010110
  DC.B  #%01111100
  DC.B  #%01111100
  DC.B  #%01101100
  DC.B  #%00101000
  DC.B  #%00101000
  DC.B  #%00111000
  DC.B  #%00010000
  DC.B  #%00010000
  DC.B  #%01000100
  DC.B  #%01000100
  DC.B  #%01000100
  DC.B  #%01101100
  DC.B  #%00101000
  DC.B  #%00101000
  DC.B  #%00000000
  DC.B  #%00101000
A_EndShip
  if (>A_Ship != >A_EndShip)
    echo "WARNING: Ship Sprite Data crosses a page boundary!"
  endif

  ALIGN   256

  ; Include Ship Explosion Data
A_ShipExplosion0
  DC.B  40,0
A_ShipExplosion1
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00001000
  DC.B  #%00000000
  DC.B  #%00100000
  DC.B  #%11000100
  DC.B  #%11010110
  DC.B  #%11000110
  DC.B  #%10000010
  DC.B  #%01101000
  DC.B  #%01001000
  DC.B  #%01001100
  DC.B  #%00101000
  DC.B  #%00101000
  DC.B  #%00111010
  DC.B  #%00010000
  DC.B  #%01010000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
A_ShipExplosion2
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00001000
  DC.B  #%00000000
  DC.B  #%01010000
  DC.B  #%01010010
  DC.B  #%10100100
  DC.B  #%10000110
  DC.B  #%00010010
  DC.B  #%00001010
  DC.B  #%10000000
  DC.B  #%01101000
  DC.B  #%00000000
  DC.B  #%00000100
  DC.B  #%10101000
  DC.B  #%00001000
  DC.B  #%00100010
  DC.B  #%00000100
  DC.B  #%00010100
  DC.B  #%00010000
  DC.B  #%10010000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
A_ShipExplosion3
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00001000
  DC.B  #%00000000
  DC.B  #%01010000
  DC.B  #%11000110
  DC.B  #%00000010
  DC.B  #%00000010
  DC.B  #%01101000
  DC.B  #%01000100
  DC.B  #%00000000
  DC.B  #%11000100
  DC.B  #%00000000
  DC.B  #%00010000
  DC.B  #%00000010
  DC.B  #%01000000
  DC.B  #%00010000
  DC.B  #%00000010
  DC.B  #%11000100
  DC.B  #%00000100
  DC.B  #%01000000
  DC.B  #%00000100
  DC.B  #%00010100
  DC.B  #%00010000
  DC.B  #%10010000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
A_ShipExplosion4
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%01010010
  DC.B  #%10000010
  DC.B  #%01000000
  DC.B  #%10000000
  DC.B  #%00001000
  DC.B  #%00000000
  DC.B  #%01000000
  DC.B  #%00000010
  DC.B  #%00000000
  DC.B  #%10000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000010
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00010000
  DC.B  #%00000000
  DC.B  #%11000000
  DC.B  #%00000000
  DC.B  #%10010000
  DC.B  #%00000100
  DC.B  #%00000000
  DC.B  #%00010010
  DC.B  #%10010000
  DC.B  #%00000000
  DC.B  #%00000000
A_ShipExplosion5
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%10000010
  DC.B  #%10000000
  DC.B  #%00000000
  DC.B  #%00001000
  DC.B  #%10000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%00000000
  DC.B  #%10010000
  DC.B  #%00000000
  DC.B  #%00010010
  DC.B  #%00000000
  DC.B  #%00000000
A_EndShipExplosion
  if (>A_ShipExplosion0 != >A_EndShipExplosion)
    echo "WARNING: Ship Explosion Data crosses a page boundary!"
  endif

  ALIGN   256

  ; Laser Size Table (66 Entries)
A_LaserSize
  DS.B  32, 32
  DS.B  16, 16
  DS.B  8, 8
  DS.B  4, 4
  DS.B  2, 2
  DS.B  2, 1
  DS.B  2, 0
A_EndLaserSize
  if (>A_LaserSize != >A_EndLaserSize)
    echo "WARNING: LaserSize Table Crosses Page Boundary!"
  endif
  
A_ReposTable
  ; Repositioning Kernel Pointers (150 Entries)
  DS.B  15, #<A_Repos0
  DS.B  15, #<A_Repos1
  DS.B  15, #<A_Repos2
  DS.B  15, #<A_Repos3
  DS.B  15, #<A_Repos4
  DS.B  15, #<A_Repos5
  DS.B  15, #<A_Repos6
  DS.B  15, #<A_Repos7
  DS.B  15, #<A_Repos8
  DS.B  15, #<A_Repos9
A_EndReposTable
  if (>A_ReposTable != >A_EndReposTable)
    echo "WARNING: Repositioning Table Crosses Page Boundary!"
  endif

  ALIGN   256

  ; Include Ship Sprite Colours - (96 Entries)
  IF (PALCOLS)
A_DarkCol
  DC.B  $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
  DC.B  $4C,$4A,$48,$46,$44,$24,$64,$22
  DC.B  $62,$62,$60,$90,$B0,$90,$B0,$90
  DC.B  $B0,$90,$B0,$B0,$D0,$B0,$D0,$B0
  DC.B  $60,$62,$62,$42,$64,$64,$44,$46
A_ShipCol
  DC.B  $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
  DC.B  $4C,$4A,$48,$46,$44,$24,$64,$22
  DC.B  $62,$62,$60,$9E,$BE,$9C,$BC,$9A
  DC.B  $BA,$98,$B8,$B6,$D6,$B4,$D4,$B2
  DC.B  $60,$62,$62,$42,$64,$64,$44,$46
A_ExplosionCol
  DC.B  $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
  DC.B  $BE,$BE,$BE,$BE,$BE,$BE,$BE,$BE
  DC.B  $BE,$BE,$BE,$BE,$BE,$9C,$BC,$9A
  DC.B  $BA,$98,$B8,$B6,$D6,$B4,$D4,$B2
  DC.B  $D2,$D0,$D0,$D0,$D0,$D0,$D0,$D0
  ELSE
A_DarkCol
  DC.B  $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
  DC.B  $2C,$2A,$28,$26,$24,$34,$44,$32
  DC.B  $42,$42,$40,$A0,$90,$A0,$90,$A0
  DC.B  $90,$A0,$90,$90,$80,$90,$80,$90
  DC.B  $40,$42,$42,$32,$44,$34,$24,$26
A_ShipCol
  DC.B  $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
  DC.B  $2C,$2A,$28,$26,$24,$34,$44,$32
  DC.B  $42,$42,$40,$AE,$9E,$AC,$9C,$AA
  DC.B  $9A,$A8,$98,$96,$86,$94,$84,$92
  DC.B  $40,$42,$42,$32,$44,$34,$24,$26
A_ExplosionCol
  DC.B  $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
  DC.B  $AE,$AE,$AE,$AE,$AE,$AE,$AE,$AE
  DC.B  $AE,$AE,$AE,$AE,$9E,$AC,$9C,$AA
  DC.B  $9A,$A8,$98,$96,$86,$94,$84,$92
  DC.B  $82,$90,$80,$80,$80,$80,$80,$80
  ENDIF
A_EndShipCol
  if (>A_ShipCol != >A_EndShipCol)
    echo "WARNING: Ship Colour Data crosses a page boundary!"
  endif

A_TimeBarData1
A_PF1BTime
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000 ;
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000 ;
A_PF2ATime
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000 ;
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000001
  DC.B  %00000011
  DC.B  %00000111 ;
  DC.B  %00001111
  DC.B  %00011111
  DC.B  %00111111
  DC.B  %01111111
  DC.B  %11111111
  DC.B  %11111111
  DC.B  %11111111
  DC.B  %11111111 ;
  DC.B  %11111111
  DC.B  %11111111
  DC.B  %11111111
  DC.B  %11111111
  DC.B  %11111111
  DC.B  %11111111
  DC.B  %11111111
  DC.B  %11111111 ;
  DC.B  %11111111
  DC.B  %11111111
  DC.B  %11111111
  DC.B  %11111111
  DC.B  %11111111
  DC.B  %11111111
  DC.B  %11111111
  DC.B  %11111111 ;
  DC.B  %11111111
  DC.B  %11111111
A_EndTimeBarData1
  if (>A_TimeBarData1 != >A_EndTimeBarData1)
    echo "WARNING: TimeBar Data 1 crosses a page boundary!"
  endif

  ; Time Bar Colours
A_TimeCol
  IF (PALCOLS)
  DC.B  $00, $0E, $28, $46, $64, $64, $64, $64
  DC.B  $A4, $A4, $A4, $A4, $A4, $A4, $A4, $A4
  DC.B  $A4, $A4, $A4, $A4, $A4, $A4, $A4, $A4
  DC.B  $A4, $A4, $A4, $A4, $A4, $A4, $A4, $A4
  DC.B  $A4, $A4, $A4, $A4, $A4, $A4, $A4, $A4, $A4, $A4
  ELSE
  DC.B  $00, $0E, $18, $26, $44, $44, $44, $44
  DC.B  $64, $64, $64, $64, $64, $64, $64, $64
  DC.B  $64, $64, $64, $64, $64, $64, $64, $64
  DC.B  $64, $64, $64, $64, $64, $64, $64, $64
  DC.B  $64, $64, $64, $64, $64, $64, $64, $64, $64, $64
  ENDIF
A_EndTimeCol
  if (>A_TimeCol != >A_EndTimeCol)
    echo "WARNING: TimeCol crosses a page boundary!"
  endif
  
  ALIGN 256
  
A_TimeBarData2
A_PF0ATime
  DC.B  %00000000
  DC.B  %00010000
  DC.B  %00110000
  DC.B  %01110000
  DC.B  %11110000
  DC.B  %11110000
  DC.B  %11110000
  DC.B  %11110000 ;
  DC.B  %11110000
  DC.B  %11110000
  DC.B  %11110000
  DC.B  %11110000
  DC.B  %11110000
  DC.B  %11110000
  DC.B  %11110000
  DC.B  %11110000 ;
  DC.B  %11110000
  DC.B  %11110000
  DC.B  %11110000
  DC.B  %11110000
  DC.B  %11110000
  DC.B  %11110000
  DC.B  %11110000
  DC.B  %11110000 ;
  DC.B  %11110000
  DC.B  %11110000
  DC.B  %11110000
  DC.B  %11110000
  DC.B  %11110000
  DC.B  %11110000
  DC.B  %11110000
  DC.B  %11110000 ;
  DC.B  %11110000
  DC.B  %11110000
  DC.B  %11110000
  DC.B  %11110000
  DC.B  %11110000
  DC.B  %11110000
  DC.B  %11110000
  DC.B  %11110000 ;
  DC.B  %11110000
  DC.B  %11110000

A_PF0BTime
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000 ;
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000 ;
A_PF2BTime
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000 ;
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000 ;
A_PF1ATime
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %10000000
  DC.B  %11000000
  DC.B  %11100000 ;
  DC.B  %11110000
  DC.B  %11111000
  DC.B  %11111100
  DC.B  %11111110
  DC.B  %11111111
  DC.B  %11111111
  DC.B  %11111111
  DC.B  %11111111 ;
  DC.B  %11111111
  DC.B  %11111111
  DC.B  %11111111
  DC.B  %11111111
  DC.B  %11111111
  DC.B  %11111111
  DC.B  %11111111
  DC.B  %11111111 ;
  DC.B  %11111111
  DC.B  %11111111
  DC.B  %11111111
  DC.B  %11111111
  DC.B  %11111111
  DC.B  %11111111
  DC.B  %11111111
  DC.B  %11111111 ;
  DC.B  %11111111
  DC.B  %11111111
  DC.B  %11111111
  DC.B  %11111111
  DC.B  %11111111
  DC.B  %11111111
  DC.B  %11111111
  DC.B  %11111111 ;
  DC.B  %11111111
  DC.B  %11111111
A_EndTimeBarData2
  if (>A_TimeBarData2 != >A_EndTimeBarData2)
    echo "WARNING: TimeBar Data 2 crosses a page boundary!"
  endif

  echo "----",($FFF0 - *) , "bytes left (BANK 2 - KERNEL A)"

  ORG     $9FF0
  RORG    $FFF0
  DC.B    0, 0, 0, 255
  DC.B    0, 0, 0, 0, 0, 0
  DC.W    (PlusAPI - $E000)
  DC.W    Init2, Init2

