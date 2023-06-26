; -----------------------------------------------------------------------------
; BANK 3 - KERNEL 2
; -----------------------------------------------------------------------------

; Grid Colours (PAL/NTSC)
  IF (PALCOLS)
B_GRIDCOL0 = $60
B_GRIDCOL1 = $62
B_GRIDCOL2 = $64
B_GRIDCOL3 = $66
B_GRIDCOL4 = $68
B_GRIDCOL5 = $6A
B_GRIDCOL6 = $6C
B_GRIDCOL7 = $6E
  ELSE
B_GRIDCOL0 = $40
B_GRIDCOL1 = $42
B_GRIDCOL2 = $44
B_GRIDCOL3 = $46
B_GRIDCOL4 = $48
B_GRIDCOL5 = $4A
B_GRIDCOL6 = $4C
B_GRIDCOL7 = $4E
  ENDIF

  ; Sprite Positioning Macro (BattleZone Divide By 15 Algorithm)
  MAC B_XPOSITION
.RES   SET {1}
.HM    SET {2}
.Div15
  sbc #15                   ; [11] + 2
  bcs .Div15                ; [13/18/23/28/33/38/43/48/53/58] + 2/3
  tax                       ; [15/20/25/30/35/40/45/50/55/60] + 2
  lda B_FineTuneEnd,X       ; [17/22/27/32/37/42/47/52/57/62] + 5
  sta .RES                  ; [22/27/32/37/42/47/52/57/62/67] + 3
  sta .HM                   ; [25/30/35/40/45/50/55/60/65/70] + 3
  ENDM                      ; WORST CASE = 73

  ; Grid-line Offset Calculation
  MAC B_GRIDOFFSET
.LINE    SET {1}
  lsr DVD                   ; [0] + 5
  sec                       ; [5] + 2
  lda B_LineTable0+.LINE    ; [7] + 3
  sbc DVD                   ; [10] + 3
  sta DVD+.LINE             ; [13] + 3 = 16
  ENDM
  
  ; Bullet Drawing Macro (Combat PHP Stack Trick)
  MAC B_DRAWBULLETS
  tya                       ; [0] + 2
  lsr                       ; [2] + 2
  cmp B2                    ; [4] + 3
  php                       ; [7] + 3
  cmp B1                    ; [10] + 3
  php                       ; [13] + 3 = 16
  ENDM

  SEG     BANK3
  ORG     $A000
  RORG    $F000

Init3
  ; Switch to Bank 8
  nop     $FFFB
  nop
  nop
  nop
  nop
  nop
  nop
KernelExit2
  ; Switch to Bank4
  nop     $FFF7
  nop
  nop
  nop

; Start Of Kernel Here
Kernel2

; -----------------------------------------------------------------------------
; PART 0 - REPOSITION SPRITES & CALCULATE GRIDLINE POSITIONS
; -----------------------------------------------------------------------------

B_MoveSprites
  ; Delay P0 Sprite
  sta WSYNC                 ; [0]
  lda #%00000001            ; [0] + 2
  sta.w VDELP0              ; [2] + 4
  
  ; Position Player Sprite
  lda PLAYERX               ; [6] + 3
  sec                       ; [9] + 2
  B_XPOSITION RESP0,HMP0    ; [11]
  sta WSYNC                 ; [0]
  
  ; Position Bullet Sprite 1
  ldx B1                    ; [0] + 3
  lda B1X,X                 ; [3] + 4
  asl                       ; [7] + 2
  sec                       ; [9] + 2
  B_XPOSITION RESM0,HMM0    ; [11]
  sta WSYNC                 ; [0]

  ; Position Bullet Sprite 2
  ldx B2                    ; [0] + 3
  lda B2X,X                 ; [3] + 4
  asl                       ; [7] + 2
  sec                       ; [9] + 2
  B_XPOSITION RESM1,HMM1    ; [11]
  sta WSYNC                 ; [0]

  ; Set Laser Colour
  IF (PALCOLS)
  lda #$24                  ; [0] + 2
  ELSE
  lda #$14                  ; [0] + 2
  ENDIF
  sta.w COLUPF              ; [2] + 4

  ; Move Laser Beam Sprite
  lda LASERX                ; [6] + 3
  sec                       ; [9] + 2
  B_XPOSITION RESBL,HMBL    ; [11]
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
  lda #>B_ReposFunctions0   ; [23] + 2
  sta JPTR+1                ; [25] + 3 = 28
  
  ; Calculate Grid Line Offsets (Unrolled)
  lda START                 ; [0] + 3
  lsr                       ; [3] + 2
  lsr                       ; [5] + 2
  sta DVD                   ; [7] + 3
  sec                       ; [10] + 2
  lda B_LineTable0+1        ; [12] + 3
  sbc DVD                   ; [15] + 3
  sta DVD+1                 ; [18] + 3 = 21
GINDEX    SET 2
  REPEAT 6
  B_GRIDOFFSET GINDEX       ; [0] + 16
GINDEX    SET GINDEX+1
  REPEND                    ; RUNTIME = 21 + (16*6) = 117 (1.5 SCANLINES)
  
  ; End of Vertical Blank
  WAIT_VBLANK               ; [0]

  ; Set Sky Colour
  ldy SKYCOL                ; [0] + 3
  sty.w COLUBK              ; [3] + 4

  ; Calculate Laser Beam Size
  ldy LASERY                ; [7] + 3
  lda B_LaserSize,Y         ; [10] + 4
  sta LASERS                ; [14] + 3

  ; Set Scanner Height    
  ldy #SCANH                ; [17] + 2
  
  ; Start Scanner Kernel
  jmp B_ScannerLoop         ; [19] + 3
B_EndMoveSprites
  if (>B_MoveSprites != >B_EndMoveSprites)
    echo "WARNING: Sprite Movement Code Crosses Page Boundary!"
  endif

  ; Grid Line Dividers (8 Entries)
B_LineTable0
  DC.B  128-128, 128-64, 128-32, 128-16, 128-8, 128-4, 128-2, 128-1
B_EndLineTable0
  if (>B_LineTable0 != >B_EndLineTable0)
    echo "WARNING: Line Table Crosses Page Boundary!"
  endif
  
  DC.B  "PARTA"
    
; -----------------------------------------------------------------------------
; PART 1 - DISPLAY ALIEN SCANNER AT TOP OF SCREEN
; -----------------------------------------------------------------------------

  ALIGN   256
  
  ; Skip The Rest Of The Scanner If No Sprites Are Displayed
B_FinishLoop
  sta WSYNC                 ; [0]
  ; Clear Alien Sprite 
  lda #0                    ; [0]
  sta GRP1                  ; [0] + 3    > 75 < 23
  sta WSYNC                 ; [0]
  sta WSYNC                 ; [0]
  ; Check If We Are Done
  dey                       ; [0] + 2
  bpl B_FinishLoop          ; [2] + 2/3
  jmp B_EndScannerLoop      ; [4] + 3

  ; Skip Scanner Region If It Contains No Sprites
B_SkipLine
  ; Clear Alien Sprite
  lda #0                    ; [9] + 2
  sta GRP1                  ; [11] + 3    < 23
  ; Skip 3 Rows
  sta WSYNC                 ; [0]
  sta WSYNC                 ; [0]
  sta WSYNC                 ; [0]
  ; Check If We Are Done
  dey                       ; [0] + 2
  bpl B_NextScannerLine     ; [2] + 2/3
  jmp B_EndScannerLoop      ; [4] + 3

  ; Main Scanner Kernel Loop
B_ScannerLoop
  ; Fetch Index Of Next Alien
  ldx INDEX                 ; [22] + 3
  
  ; Clear Sprite Movements
  sta.w HMCLR               ; [25] + 4    > 24(+3?)

  ; Check If We Are Finished Drawing Sprites
  bmi B_FinishLoop          ; [29] + 2/3
  
  ; Fetch Y Position Of Alien
  lda INVY,X                ; [31] + 4
  
  ; Scanner Shows Positions 255->128 only
  bpl B_FinishLoop          ; [35] + 2/3
  
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
  lda B_AlienSprites+3,X    ; [60] + 4
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
  B_XPOSITION RESP1,HMP1    ; [11]
  sta WSYNC                 ; [0] 
  sta HMOVE                 ; [0] + 3

B_NextScannerLine
  ; Check If We Have Reached Alien Position
  cpy ALIENY                ; [3] + 3
  bne B_SkipLine            ; [6] + 2/3

  ; Draw Alien (Line 0)
  ldx TYPE                  ; [8] + 3
  lda B_Aliens+1,X          ; [11] + 4
  sta GRP1                  ; [15] + 3    < 23
  lda B_AlienCols+1,X       ; [18] + 4
  sta COLUP1                ; [22] + 3    < 23 !!!

  ; Draw Alien (Line 1)
  sta WSYNC                 ; [0]
  ldx TYPE                  ; [0] + 3
  lda B_Aliens+2,X          ; [3] + 4
  sta GRP1                  ; [7] + 3    < 23
  lda B_AlienCols+2,X       ; [10] + 4
  sta COLUP1                ; [14] + 3    < 23

  ; Check If We Are Done
  dey                       ; [17] + 2
  bpl B_ScannerLoop         ; [19] + 2/3
B_EndScannerLoop

  ; Reset Sprite Collisions
  sta CXCLR                 ; [21] + 3

  ; Set Stack To M1 For Bullet Drawing
  ldx #ENAM1                ; [24] + 2
  txs                       ; [26] + 2
  
  ; Set Initial Alien Counters
  ldx INDEX                 ; [28] + 3
  bmi B_NoMoreSprites       ; [31] + 2/3
  ldy INVY,X                ; [33] + 4
  sty ALIENY                ; [37] + 3
  lda INVT,X                ; [40] + 4
  and #%00111100            ; [44] + 2
  clc                       ; [46] + 2
  adc B_AlienSize,Y         ; [48] + 4
  tay                       ; [52] + 2
  lda B_AlienSprites,Y      ; [54] + 4
  sta TYPE                  ; [58] + 3
  
  ; Clear Alien Sprite
  sta WSYNC                 ; [0]
  lda #0                    ; [0] + 2
  sta GRP1                  ; [2] + 3    < 23

  ; Set Initial Alien Sprite Position
  lda INVX,X                ; [5] + 4 
  sec                       ; [9] + 2
  B_XPOSITION RESP1,HMP1    ; [11]
  sta WSYNC                 ; [0]
  sta HMOVE                 ; [0] + 3
  
  ; Decrease Alien Index
  ldx INDEX                 ; [3] + 3
  lda INVS,X                ; [6] + 4
  sta INDEX                 ; [10] + 3

B_FinishScanner
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
  jmp B_Resume1             ; [37] + 3

  ; No More Sprites To Display
B_NoMoreSprites
  sta WSYNC                 ; [0]
  ; Clear Graphics & Alien Counter
  lda #0                    ; [0] + 2
  sta GRP1                  ; [2] + 3    < 23
  sta ALIENY                ; [5] + 3
  sta WSYNC                 ; [0]
  SLEEP 10                  ; [0] + 10
  jmp B_FinishScanner       ; [10] + 3
B_EndScannerKernel

  if (>B_FinishLoop != >B_EndScannerKernel)
    echo "WARNING: Scanner Kernel Crosses Page Boundary!"
  endif

  DC.B  "PARTB"

  ALIGN   256

  ; HMOVE Finetune Table (15 Entries)
B_FineTune
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
B_FineTuneEnd = B_FineTune - 241

; -----------------------------------------------------------------------------
; PART 2 - GRID KERNEL: DISPLAYS ALIENS, GRID LINES, LASER BEAM, AND BULLETS
; -----------------------------------------------------------------------------
        
B_DrawLine0
  ; Display Gridline (By Changing PF Background Colour)
  ldx DVD                   ; [12] + 3
  lda B_GridCols,X          ; [15] + 4
  sta COLUBK                ; [19] + 3    < 23
  
  ; Calculate Next Gridline Position
  lda DVD,X                 ; [22] + 4
  sta LINE                  ; [26] + 3
  dec DVD                   ; [29] + 5
  
  ; Resume Kernel
  SLEEP 3                   ; [34] + 3
  jmp B_Resume1             ; [37] + 3

B_DrawLaser0
  ; Draw Laser Beam
  sbc LASERY                ; [23] + 3
  adc LASERS                ; [26] + 3
  rol                       ; [29] + 2
  rol                       ; [31] + 2
  sta.w ENABL               ; [33] + 4
  jmp B_Resume1             ; [37] + 3

  ; Main Grid Kernel Loop
B_GridLoop
  ; Draw Alien (Variant of Thomas Jentzsch SkipDraw Routine)
  SLEEP 3                   ; [65] + 3    
  lda B_Aliens,X            ; [68] + 4
  sta GRP1                  ; [72] + 3    > 75  !!!!
  lda B_AlienCols,X         ; [75] + 4
  sta COLUP1                ; [3] + 3    > 75 < 23

  ; Check If Grid Line Should Be Displayed
B_Resume0
  cpy LINE                  ; [6] + 3
  bcc B_DrawLine0           ; [9] + 2/3

  ; Clear Background
  lda #0                    ; [11] + 2
  sta COLUBK                ; [13] + 3    < 23

  ; Draw Bullets/Laser
  tya                       ; [16] + 2
  lsr                       ; [18] + 2
  bcs B_DrawLaser0          ; [20] + 2/3
  cmp B2                    ; [22] + 3
  php                       ; [25] + 3
  cmp B1                    ; [28] + 3
  php                       ; [31] + 3
  ldx #ENAM1                ; [34] + 2
  txs                       ; [36] + 2
  nop                       ; [38] + 2
B_Resume1
  ; Check If Alien Should Be Drawn Next Line
  sec                       ; [40] + 2
  tya                       ; [42] + 2
  sbc ALIENY                ; [44] + 3
  bcs B_NoDraw0             ; [47] + 2/3
  
  ; Check For New Alien
  adc #8                    ; [49] + 2
  bcc B_NewSprite0          ; [51] + 2/3

  ; Calculate Alien Sprite Pointer
  adc TYPE                  ; [53] + 3
  tax                       ; [56] + 2
  
  ; Decrease Row  Counter
  dey                       ; [58] + 2
  cpy #SHIPH                ; [60] + 2
  bcs B_GridLoop            ; [62] + 2/3
  
  ; Disable Laser Beam
  lda #0                    ; [64] + 2
  sta ENABL                 ; [66] + 3

  ; Draw Alien
  lda B_Aliens,X            ; [69] + 4
  sta GRP1                  ; [73] + 3    > 75 < 23
  lda B_AlienCols,X         ; [0] + 4 
  sta COLUP1                ; [4] + 3    > 75 < 23

  ; Start Ship Kernel
  jmp B_ShipKernel          ; [7] + 3

B_NoDraw0
  ; Cycle Padding    Code
  SLEEP 12                  ; [50] + 12    
  lda #0                    ; [62] + 2
  beq B_Cont0               ; [64] + 3
B_NoNewSprite0
  ; Set Alien Coordinate to 0
  nop                       ; [60] + 2
  lda #0                    ; [62] + 2
  sta ALIENY                ; [64] + 3
B_Cont0
  ; Decrease Row Counter
  dey                       ; [67] + 2
  cpy #SHIPH                ; [69] + 2
  bcs B_Cont1               ; [71] + 2/3
  ; Clear Alien Sprite, Disable Laser Beam, and Start Ship Kernel
  SLEEP 4                   ; [73] + 4    
  sta GRP1                  ; [1] + 3    > 75 < 23
  sta ENABL                 ; [4] + 3
  jmp B_ShipKernel          ; [7] + 3
B_Cont1
  ; Resume Grid Kernel
  nop                       ; [74] + 2    
  sta GRP1                  ; [0] + 3    > 75 < 23
  jmp B_Resume0             ; [3] + 3

; -----------------------------------------------------------------------------
; PART 3 - REPOSITION ALIEN DURING GRID KERNEL
; -----------------------------------------------------------------------------

B_NewSprite0
  ; Fetch Current Alien Index
  ldx INDEX                 ; [54] + 3
  bmi B_NoNewSprite0        ; [57] + 2/3

  ; Update Next Alien Index Counter
  lda INVS,X                ; [59] + 4
  
  ; Check If Grid Line Should Be Drawn Now
  dey                       ; [63] + 2
  cpy LINE                  ; [65] + 3
  bcc B_LineFirst0          ; [68] + 2/4*

B_RepositionFirst0
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
  B_XPOSITION RESP1,HMP1    ; [11]
  sta WSYNC                 ; [0] 
  sta HMOVE                 ; [0] + 3
          
  ; Check Grid Line
  dey                       ; [3] + 2
  cpy LINE                  ; [5] + 3
  bcs B_SkipLine2           ; [8] + 2/3
  
  ; Display Grid Line
  ldx DVD                   ; [10] + 3
  lda B_GridCols,X          ; [13] + 4    
  sta COLUBK                ; [17] + 3    < 23 

  ; Calculate Next Line Position
  lda DVD,X                 ; [20] + 4
  sta LINE                  ; [24] + 3
  dec DVD                   ; [27] + 5
  nop                       ; [32] + 2
B_EndSkip2

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
  adc B_AlienSize,Y         ; [61] + 4
  tay                       ; [65] + 2
  
  ; Fetch Sprite Offset (& Store For Later)
  lda B_AlienSprites,Y      ; [67] + 4
  sta TYPE                  ; [71] + 3
  
  ; Restore Row Counter
  ldy TEMP                  ; [74] + 3
  dey                       ; [1] + 2
  
  ; Resume Grid Kernel
  jmp B_Resume0             ; [3] + 3 

B_SkipLine2
  ; Draw Bullets
  B_DRAWBULLETS             ; [11] + 16
  ldx #ENAM1                ; [27] + 2
  txs                       ; [29] + 2
  jmp B_EndSkip2            ; [31] + 3

B_EndGridKernel
  if (>B_DrawLine0 != >B_EndGridKernel)
    echo "WARNING: Grid Kernel Crosses Page Boundary!"
  endif

  ; DC.B  "PARTC"

; -----------------------------------------------------------------------------
; PART 4 - DELAYED ALIEN REPOSITIONING (WHEN GRIDLINE NEEDS TO BE DRAWN FIRST)
; -----------------------------------------------------------------------------

  ALIGN   256

B_LineFirst0
  ; Store Index & Row Counter
  sta INDEX                 ; [72] + 3
  sty TEMP                  ; [75] + 3
  
  ; Fetch Grid Line Index
  ldy DVD                   ; [2] + 3
      
  ; Clear Alien Sprite
  lda #0                    ; [5] + 2
  sta GRP1                  ; [7] + 3    > 75 < 23

  ; Display Grid Line
  lda B_GridCols,Y          ; [10] + 4
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
  adc B_AlienSize,Y         ; [42] + 4
  tay                       ; [46] + 2
  lda B_AlienSprites,Y      ; [48] + 4
  sta TYPE                  ; [52] + 3
  
  ; Restore Row Counter
  ldy TEMP                  ; [55] + 3

  ; Draw Bullets
  B_DRAWBULLETS             ; [58] + 16

  ; Decrease Row
  dey                       ; [74] + 2
      
  ; Fetch Sprite X Position
  lda INVX,X                ; [0] + 4

  ; Clear Background
  ldx #0                    ; [4] + 2
  stx COLUBK                ; [6] + 3    > 75 < 23 

  ; Reposition Alien Sprite
  sec                       ; [9] + 2
  B_XPOSITION RESP1,HMP1    ; [11]
  sta WSYNC                 ; [0]
  sta HMOVE                 ; [0] + 3

  ; Check Grid Line
  dey                       ; [3] + 2
  cpy LINE                  ; [5] + 3
  bcs B_SkipLine3           ; [8] + 2/3
  
  ; Display Grid Line
  ldx DVD                   ; [10] + 3
  lda B_GridCols,X          ; [13] + 4
  sta COLUBK                ; [17] + 3    < 23

  ; Calculate Next Grid Line Position
  lda DVD,X                 ; [20] + 4
  sta.w LINE                ; [24] + 4
  dec DVD                   ; [28] + 5
      
  ; Reset Stack
  ldx #ENAM1                ; [33] + 2
  txs                       ; [35] + 2
  jmp B_Resume1             ; [37] + 3

B_SkipLine3
  ; Draw Bullets
  ldx #ENAM1                ; [11] + 2
  txs                       ; [13] + 2
  B_DRAWBULLETS             ; [15] + 16
  ldx #ENAM1                ; [31] + 2
  txs                       ; [33] + 2
  nop                       ; [35] + 2
  jmp B_Resume1             ; [37] + 3

  ; Include Alien Sprite Pointers
B_AlienSprites
  ; Sprite Type/Size Table
  ; 0 = Empty
  DC.B  #<B_Empty, #<B_Empty, #<B_Empty, #<B_Empty
  ; 1 = Alien 1 (Frame 1)
  DC.B  #<B_Alien1_1-1, #<B_Alien1_3-1, #<B_Alien1_4-1, #<B_Alien1_5-1
  ; 2 = Alien 2 (Frame 1)
  DC.B  #<B_Alien2_1-1, #<B_Alien2_3-1, #<B_Alien2_5-1, #<B_Alien2_6-1
  ; 3 = Bug Alien
  DC.B  #<B_BugAlien_1-1, #<B_BugAlien_2-1, #<B_BugAlien_3-1, #<B_BugAlien_4-1
  ; 4 = Sphere
  DC.B  #<B_Sphere_1-1, #<B_Sphere_2-1, #<B_Sphere_3-1, #<B_Sphere_4-1
  ; 5 = Big UFO
  DC.B  #<B_BigUFO_1-1, #<B_BigUFO_2-1, #<B_BigUFO_3-1, #<B_BigUFO_4-1
  ; 6 = Small UFO
  DC.B  #<B_SmallUFO_1-1, #<B_SmallUFO_2-1, #<B_SmallUFO_3-1, #<B_SmallUFO_4-1
  ; 7 = Alien 1 (Frame 2)
  DC.B  #<B_Alien1_2-1, #<B_Alien1_3-1, #<B_Alien1_4-1, #<B_Alien1_5-1
  ; 8 = Alien 2 (Frame 2)
  DC.B  #<B_Alien2_2-1, #<B_Alien2_4-1, #<B_Alien2_5-1, #<B_Alien2_6-1
  ; 9 = Astronaut
  DC.B  #<B_Astronaut_1-1, #<B_Astronaut_2-1, #<B_Astronaut_3-1, #<B_Empty
  ; 10->12 = Warp
  DC.B  #<B_Explosion_3-1, #<B_Explosion_2-1, #<B_Explosion_1-1, #<B_Empty
  DC.B  #<B_Explosion_4-1, #<B_Explosion_3-1, #<B_Explosion_2-1, #<B_Empty
  DC.B  #<B_Explosion_5-1, #<B_Explosion_5-1, #<B_Explosion_5-1, #<B_Empty
  ; 13->15 = Explosion
  DC.B  #<B_Explosion_5-1, #<B_Explosion_5-1, #<B_Explosion_5-1, #<B_Empty
  DC.B  #<B_Explosion_4-1, #<B_Explosion_3-1, #<B_Explosion_2-1, #<B_Empty
  DC.B  #<B_Explosion_3-1, #<B_Explosion_2-1, #<B_Explosion_1-1, #<B_Empty
B_EndAlienSprites
  if (>B_AlienSprites != >B_EndAlienSprites)
    echo "WARNING: Alien Sprite Table crosses a page boundary!"
  endif
    
; -----------------------------------------------------------------------------
; PART 5 - SHIP KERNEL - DISPLAYS SHIP, ALIENS, BULLETS, AND FINAL GRID LINE
; -----------------------------------------------------------------------------

B_DrawLine1
  ; Draw Line
  lda #B_GRIDCOL7           ; [16] + 2
  sta.w COLUBK              ; [18] + 4    < 23
  jmp B_Resume6             ; [22] + 3    = 25
  
  ; Ship Kernel Start Code
B_ShipKernel
  ; Check Grid Line
  cpy LINE                  ; [10] + 3
  bcc B_DrawLine1           ; [13] + 2/3

  ; Clear Background
  lda #0                    ; [15] + 2
  sta COLUBK                ; [17] + 3    < 23
  
  ; Display Bullets
  B_DRAWBULLETS             ; [20] + 16
  ldx #ENAM1                ; [36] + 2
  txs                       ; [38] + 2

  ; Start Ship Kernel
  sec                       ; [40] + 2
  jmp B_ShipLoop            ; [42] + 3

B_NoDraw2
  ; Preload Ship Data
  lda (SPTR),Y              ; [54] + 5
  sta GRP0                  ; [59] + 3    VDEL
  SLEEP 13                  ; [62] + 13
  
  ; Display Ship (By Setting GRP1)
  lda #0                    ; [75] + 2
  sta GRP1                  ; [1] + 3     > 75 < 23
  jmp B_Resume2             ; [4] + 3

B_EndLineFirst0
  if (>B_LineFirst0 != >B_EndLineFirst0)
    echo "WARNING: Repositioning Code Crosses Page Boundary!"
  endif

  DC.B  "PARTD"
  
  ALIGN   256

B_NewSprite1
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
  bmi B_Resume2             ; [4] + 2/3

    ; Update Ship Colour
  lda (CPTR),Y              ; [6] + 5
  sta COLUP0                ; [11] + 3    > 75 < 23

  ; Calculate Alien Variables
  lda INVY,X                ; [14] + 4
  sta ALIENY                ; [18] + 3
  lda INVT,X                ; [21] + 4
  and #%00111100            ; [25] + 2
  tax                       ; [27] + 2
  lda B_AlienSprites,X      ; [29] + 4
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
  lda B_ReposTable,X        ; [62] + 4
  sta JPTR                  ; [66] + 3

  ; Set Ship Colour
  lda (CPTR),Y              ; [69] + 5
  sta COLUP0                ; [74] + 3    > 75 < 23
  
  ; Draw Ship
  lda #0                    ; [1] + 2
  sta GRP1                  ; [3] + 3     < 23

  ; Jump To Repositioning Kernel
  jmp (JPTR)                ; [6] + 5     = 11

B_ShipLoop
  ; Calculate Alien Row
  ; sec (Carry Already Set)
  tya                       ; [45] + 2
  sbc ALIENY                ; [47] + 3
  bcs B_NoDraw2             ; [50] + 2/4*
  adc #8                    ; [52] + 2
  bcc B_NewSprite1          ; [54] + 2/3
  adc TYPE                  ; [56] + 3
  tax                       ; [59] + 2
  
  ; Preload Ship Data
  lda (SPTR),Y              ; [61] + 5
  sta GRP0                  ; [66] + 3    VDEL
      
  ; Draw Alien & Ship
  lda B_Aliens,X            ; [69] + 4
  sta GRP1                  ; [73] + 3    > 75
  lda B_AlienCols,X         ; [0] + 4
  sta COLUP1                ; [4] + 3     > 75 < 23
B_Resume2
  ; Set Ship Colour
  lda (CPTR),Y              ; [7] + 5
  sta COLUP0                ; [12] + 3    < 23
  
  ; Draw Bullets
  B_DRAWBULLETS             ; [15] + 16
  ldx #ENAM1                ; [31] + 2
  txs                       ; [33] + 2
B_Resume3
  ; Check End of Kernel
  dey                       ; [35] + 2
  bmi B_EndShipKernel       ; [37] + 2/3
  
  ; Check Line
  cpy LINE                  ; [39] + 3
  bcs B_ShipLoop            ; [42] + 2/3
  
; -----------------------------------------------------------------------------
; PART 6 - DRAW GRID LINE DURING SHIP KERNEL
; -----------------------------------------------------------------------------

B_DrawLine2
  ; Calculate Alien Row
  sec                       ; [44] + 2
  tya                       ; [46] + 2
  sbc ALIENY                ; [48] + 3
  bcs B_NoDraw3             ; [51] + 2/3
  adc #8                    ; [53] + 2
  bcc B_NewSprite3          ; [55] + 2/3
  adc TYPE                  ; [57] + 3
  tax                       ; [60] + 2
  
  ; Preload Ship Data
  lda (SPTR),Y              ; [62] + 5
  sta GRP0                  ; [67] + 3    VDEL
      
  ; Draw Alien & Ship
  lda B_Aliens,X            ; [70] + 4
  sta GRP1                  ; [74] + 3    > 75
  lda B_AlienCols,X         ; [1] + 4
  sta COLUP1                ; [5] + 3     > 75 < 23
B_Resume4
  ; Set Ship Colour
  lda (CPTR),Y              ; [8] + 5
  sta COLUP0                ; [13] + 3    < 23

  ; Draw Grid Line
  lda #B_GRIDCOL7           ; [16] + 2
  sta COLUBK                ; [18] + 3    < 23

B_Resume5            
  ; Decrease Row Counter
  dey                       ; [21] + 2
  bmi B_EndShipKernel       ; [23] + 2/3
B_Resume6
  ; Clear LINE To Prevent More Grid Lines
  lda #0                    ; [25] + 2
  sta LINE                  ; [27] + 3
  
  ; Calculate Alien Row
  sec                       ; [30] + 2
  tya                       ; [32] + 2
  sbc ALIENY                ; [34] + 3
  bcs B_NoDraw4             ; [37] + 2/4*
  adc #8                    ; [39] + 2
  bcc B_NewSprite2          ; [41] + 2/3
  adc TYPE                  ; [43] + 3
  tax                       ; [46] + 2

  ; Draw Bullets
  B_DRAWBULLETS             ; [48] + 16

  ; Preload Ship Data
  lda (SPTR),Y              ; [64] + 5
  sta GRP0                  ; [69] + 3    VDEL
      
  ; Draw Alien & Ship
  lda B_Aliens,X            ; [72] + 4
  sta GRP1                  ; [0] + 3     > 75
  lda B_AlienCols,X         ; [3] + 4
  sta COLUP1                ; [7] + 3     > 75 < 23
B_Resume7
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
  jmp B_Resume3             ; [32] + 3

  ; Finish Ship Kernel
B_EndShipKernel
  jmp B_TimeBar             ; [40] + 3 (Worst Case)
    
B_NewSprite3
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
  bpl B_NewSprite5          ; [1] + 2/4*
  nop                       ; [3] + 2
  jmp B_Resume4             ; [5] + 3
    
B_NoDraw3
  ; Preload Ship Data
  lda (SPTR),Y              ; [54] + 5
  sta GRP0                  ; [59] + 3    VDEL
  SLEEP 14                  ; [62] + 14
  
  ; Display Ship (By Setting GRP1)
  lda #0                    ; [0] + 2
  sta GRP1                  ; [2] + 3     > 75 < 23
  jmp B_Resume4             ; [5] + 3

B_NewSprite2
  ; No Line (Ever)
  SLEEP 10                  ; [44] + 10
  jmp B_NewSprite1          ; [54] + 3

B_EndSK
  if (>B_NewSprite1 != >B_EndSK)
    echo "WARNING: Ship Kernel Crosses Page Boundary!"
  endif

  ; DC.B  "PARTE"

; -----------------------------------------------------------------------------
; PART 7 - REPOSITION ALIEN SPRITE AND DRAW LINE DURING SHIP KERNEL
; -----------------------------------------------------------------------------
    
  ALIGN   256
  
B_NoDraw4
  ; Draw Bullets
  B_DRAWBULLETS             ; [41] + 16
  ldx #ENAM1                ; [57] + 2
  txs                       ; [59] + 2
      
  ; Preload Ship Data
  lda (SPTR),Y              ; [61] + 5
  sta GRP0                  ; [66] + 3    VDEL    
  SLEEP 9                   ; [69] + 9
  
  ; Display Ship (By Setting GRP1)
  lda #0                    ; [2] + 2
  sta GRP1                  ; [4] + 3    > 75 < 23
  jmp B_Resume7             ; [7] + 3
      
B_NewSprite5
  ; Change Ship Colour
  lda (CPTR),Y              ; [5] + 5
  sta COLUP0                ; [10] + 3    > 75 < 23

  ; Draw Gridline
  lda #B_GRIDCOL7           ; [13] + 2
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
  lda B_AlienSprites,Y      ; [43] + 4
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
  B_XPOSITION RESP1,HMP1    ; [11]
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
  jmp B_Resume3             ; [32] + 3
B_EndNewSprite5
  if (>B_NewSprite5 != >B_EndNewSprite5)
    echo "WARNING: Ship Kernel Repositioning Crosses Page Boundary!"
  endif    

; Alien Size Table (128 Entries)
B_AlienSize
  DS.B  96, 0             ; 0 -> 96
  DS.B  24, 1             ; 96 -> 120
  DS.B  8,  2             ; 120 -> 128
B_EndAlienSize
  if (>B_AlienSize != >B_EndAlienSize)
    echo "WARNING: Alien Size Table Crosses Page Boundary!"
  endif

; Grid Line Colours (8 Entries)
B_GridCols
  DC.B  B_GRIDCOL7, B_GRIDCOL6, B_GRIDCOL5, B_GRIDCOL4
  DC.B  B_GRIDCOL3, B_GRIDCOL2, B_GRIDCOL1, B_GRIDCOL0
B_EndGridCols
  if (>B_GridCols != >B_EndGridCols)
    echo "WARNING: Grid Colours Cross Page Boundary!"
  endif

  DC.B  "PARTF"

; -----------------------------------------------------------------------------
; PART 8 - DISPLAY TIME (FUEL) REMAINING
; -----------------------------------------------------------------------------

  ALIGN 256

B_TimeBar
  ; Calculate Time Remaining
  lda TIME                  ; [43] + 3
  and #%00111111            ; [46] + 2
  tax                       ; [48] + 2

   ; Set Timebar Colour & Preload Data
  lda B_TimeCol,X           ; [50] + 4
  sta COLUPF                ; [54] + 3
  lda B_PF0ATime,X          ; [57] + 4

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
  lda B_PF1ATime,X          ; [18] + 4
  sta PF1                   ; [22] + 3    < 28
  lda B_PF2ATime,X          ; [25] + 4
  sta PF2                   ; [29] + 3    < 38
  lda B_PF2BTime,X          ; [32] + 4

  ; Set Coarse Sprite Positions
  nop                       ; [36] + 2
  sta RESP0                 ; [38] + 3    = 41 EXACT
  sta RESP1                 ; [41] + 3    = 44 EXACT

  ; Display Time Bar (Third Part)
  sta.w PF2                 ; [44] + 4    = 48 EXACT
  lda B_PF1BTime,X          ; [48] + 4
  sta PF1                   ; [52] + 3    > 38 < 60
  lda B_PF0BTime,X          ; [55] + 4
  sta PF0                   ; [59] + 3    > 28 < 70

  ; Set Sprite Offsets For Score (Wont Change Until HMOVE)
  lda #%00010000            ; [62] + 2
  sta HMP1                  ; [64] + 3
  
  ; Set Delay and Three Copies For Score
  lda #%00000011            ; [67] + 2
  sta VDELP1                ; [69] + 3
  sta NUSIZ0                ; [72] + 3
  sta NUSIZ1                ; [75] + 3
  
  lda B_PF0ATime,X          ; [2] + 4
  sta PF0                   ; [6] + 3     > 0 < 23
  lda B_PF1ATime,X          ; [9] + 4
  sta PF1                   ; [13] + 3    < 28
  lda B_PF2ATime,X          ; [16] + 4
  sta PF2                   ; [20] + 3    < 38
  nop                       ; [23] + 2
  nop                       ; [25] + 2
  lda B_PF0BTime,X          ; [27] + 4
  sta PF0                   ; [31] + 3    > 28 < 70
  lda B_PF1BTime,X          ; [34] + 4
  sta PF1                   ; [38] + 3    > 38 < 60
  lda B_PF2BTime,X          ; [41] + 4
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
  jmp KernelExit2           ; [6] + 3

  DC.B  "PARTG"

; -----------------------------------------------------------------------------
; PART 9 - REPOSITIONING KERNELS
; -----------------------------------------------------------------------------
    
  ALIGN   256
  
B_ReposFunctions0
  ; Not Enough Space In This Page For These
B_Repos6
  jmp B_Repos66             ; [11] + 3    = 14
B_Repos7
  jmp B_Repos77             ; [11] + 3
B_Repos8
  jmp B_Repos88             ; [11] + 3
B_Repos9
  jmp B_Repos99             ; [11] + 3
  
B_NoLine0
  SLEEP 5                   ; [17] + 5
  sta RESP1                 ; [22] + 3    = 25
  nop                       ; [25] + 2
  jmp B_EndLine0            ; [27] + 3
  
B_Repos0
  ; Check Line
  cpy LINE                  ; [11] + 3
  bcs B_NoLine0             ; [14] + 2/3

  ; Draw Line
  lda #B_GRIDCOL7           ; [16] + 2
  sta.w COLUBK              ; [18] + 4    < 23

  ; Reposition Sprite (Coarse)
  sta RESP1                 ; [22] + 3    = 25

  ; Clear Line Counter
  lda #0                    ; [25] + 2
  sta LINE                  ; [27] + 3
B_EndLine0
  
  ; Calculate Sprite Offset
  txa                       ; [30] + 2
  sec                       ; [32] + 2
  sbc #15                   ; [34] + 2
  ; 4 CYCLE GAP

B_Continue0
  ; Reposition Sprite (Fine)
  tax                       ; [40] + 2
  lda B_FineTuneEnd,X       ; [42] + 5
  sta HMP1                  ; [47] + 3

B_Continue1
  ; Decrease Line Counter
  dey                       ; [50] + 2

B_Continue2
  ; Preload Ship Data
  lda (SPTR),Y              ; [52] + 5
  sta GRP0                  ; [57] + 3    VDEL
  
  ; Preload Ship Colour
  lda (CPTR),Y              ; [60] + 5 

  ; Check Line
  cpy LINE                  ; [65] + 3
  bcc B_DrawLine3           ; [68] + 2/3

B_Continue3
  ; Set Line Colour
  ldx #0                    ; [71] + 2
  
  ; Fine Tune Sprite Position
  sta WSYNC                 ; [73] + 3
B_Continue4
  sta HMOVE                 ; [0] + 3

  ; Draw Ship
  sta COLUP0                ; [3] + 3    > 75 < 23
  stx GRP1                  ; [6] + 3    < 23

  ; Clear Background
  stx COLUBK                ; [9] + 3    < 23

  ; Draw Bullets
  B_DRAWBULLETS             ; [12] + 16
  ldx #ENAM1                ; [28] + 2
  txs                       ; [30] + 2
  
  ; Resume Kernel
  jmp B_Resume3             ; [32] + 3

B_DrawLine3
  ; Set Line Colour
  ldx #B_GRIDCOL7           ; [71] + 2
  
  ; Fine Tune Sprite Position
  sta WSYNC                 ; [73] + 3 
B_Continue5
  sta HMOVE                 ; [0] + 3
  
  ; Set Ship Colour 
  sta COLUP0                ; [3] + 3    > 75 < 23

B_Continue6
  ; Draw Line
  stx COLUBK                ; [6] + 3    < 23

  ; Draw Ship
  lda #0                    ; [9] + 2
  sta GRP1                  ; [11] + 3    < 23

  ; Resume
  SLEEP 4                   ; [14] + 4
  jmp B_Resume5             ; [18] + 3
  
B_NoLine1
  SLEEP 6                   ; [17] + 6
  bcs B_EndLine1            ; [23] + 3

B_Repos1
  ; Check Line
  cpy LINE                  ; [11] + 3
  bcs B_NoLine1             ; [14] + 2/3

  ; Draw Line
  lda #B_GRIDCOL7           ; [16] + 2
  sta COLUBK                ; [18] + 3    < 23

  ; Clear Line Counter
  lda #0                    ; [21] + 2
  sta LINE                  ; [23] + 3
B_EndLine1
  
  ; Reposition Sprite (Coarse)
  sta.w RESP1               ; [26] + 4    = 30

  ; Calculate Sprite Offset
  txa                       ; [30] + 2
  sec                       ; [32] + 2
  sbc #30                   ; [34] + 2
  jmp B_Continue0           ; [36] + 3    <= 40

B_NoLine2
  SLEEP 6                   ; [17] + 6
  bcs B_EndLine2            ; [23] + 3
  
B_Repos2
  ; Check Line
  cpy LINE                  ; [11] + 3
  bcs B_NoLine2             ; [14] + 2/3

  ; Draw Line
  lda #B_GRIDCOL7           ; [16] + 2
  sta COLUBK                ; [18] + 3    < 23

  ; Clear Line Counter
  lda #0                    ; [21] + 2
  sta LINE                  ; [23] + 3
B_EndLine2
  
  ; Calculate Sprite Offset
  txa                       ; [26] + 2
  sec                       ; [28] + 2
  sbc #45                   ; [30] + 2

  ; Reposition Sprite (Coarse)
  sta RESP1                 ; [32] + 3    = 35
  jmp B_Continue0           ; [35] + 3    <= 40

B_NoLine3
  SLEEP 6                   ; [17] + 6
  bcs B_EndLine3            ; [23] + 3
  
B_Repos3
  ; Check Line
  cpy LINE                  ; [11] + 3
  bcs B_NoLine3             ; [14] + 2/3

  ; Draw Line
  lda #B_GRIDCOL7           ; [16] + 2
  sta COLUBK                ; [18] + 3    < 23

  ; Clear Line Counter
  lda #0                    ; [21] + 2
  sta LINE                  ; [23] + 3
B_EndLine3
  
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
  lda B_FineTuneEnd,X       ; [40] + 5
  sta HMP1                  ; [45] + 3
  jmp B_Continue2           ; [48] + 3    <= 52

B_NoLine4
  SLEEP 6                   ; [17] + 6
  bcs B_EndLine4            ; [23] + 3
  
B_Repos4
  ; Check Line
  cpy LINE                  ; [11] + 3
  bcs B_NoLine4             ; [14] + 2/3

  ; Draw Line
  lda #B_GRIDCOL7           ; [16] + 2
  sta COLUBK                ; [18] + 3    < 23

  ; Clear Line Counter
  lda #0                    ; [21] + 2
  sta LINE                  ; [23] + 3
B_EndLine4
  
  ; Reposition Sprite (Fine)
  txa                       ; [26] + 2
  sec                       ; [28] + 2
  sbc #75                   ; [30] + 2
  tax                       ; [32] + 2
  lda B_FineTuneEnd,X       ; [34] + 5
  sta HMP1                  ; [39] + 3

  ; Reposition Sprite (Coarse)
  sta RESP1                 ; [42] + 3    = 45
  jmp B_Continue1           ; [45] + 3    <= 50

B_NoLine5
  SLEEP 6                   ; [17] + 6
  bcs B_EndLine5            ; [23] + 3    

B_Repos5
  ; Check Line
  cpy LINE                  ; [11] + 3
  bcs B_NoLine5             ; [14] + 2/3

  ; Draw Line
  lda #B_GRIDCOL7           ; [16] + 2
  sta COLUBK                ; [18] + 3    < 23

  ; Clear Line Counter
  lda #0                    ; [21] + 2
  sta LINE                  ; [23] + 3
B_EndLine5
  
  ; Reposition Sprite (Fine)
  txa                       ; [26] + 2
  sec                       ; [28] + 2
  sbc #90                   ; [30] + 2
  tax                       ; [32] + 2
  lda B_FineTuneEnd,X       ; [34] + 5
  sta HMP1                  ; [39] + 3

  ; Decrease Line Counter
  dey                       ; [42] + 2

  ; Continue in Next Page
  jmp B_Repos5Cont          ; [44] + 3
B_EndReposFunctions0
  if (>B_ReposFunctions0 != >B_EndReposFunctions0)
    echo "WARNING: Repositioning0 Crosses Page Boundary!"
  endif
  
  ; DC.B  "PARTH"
  
  ALIGN   256

B_ReposFunctions1

B_Repos5Cont
  ; Reposition Sprite (Coarse)
  sta RESP1                 ; [47] + 3    = 50

  ; Preload Ship Data
  lda (SPTR),Y              ; [50] + 5
  sta GRP0                  ; [55] + 3    VDEL
  
  ; Preload Ship Colour
  lda (CPTR),Y              ; [58] + 5 

  ; Check Line
  cpy LINE                  ; [63] + 3
  bcc B_DrawLine4           ; [66] + 2/3
  jmp B_Continue3           ; [68] + 3     <= 71

B_DrawLine4
  ldx #B_GRIDCOL7           ; [69] + 2
  nop                       ; [71] + 2
  jmp B_Continue5           ; [73] + 3    = 76
      
B_NoLine66
  SLEEP 6                   ; [20] + 6
  bcs B_EndLine66           ; [26] + 3    
  
B_Repos66
  ; Check Line
  cpy LINE                  ; [14] + 3
  bcs B_NoLine66            ; [17] + 2/3

  ; Draw Line
  lda #B_GRIDCOL7           ; [19] + 2
  sta COLUBK                ; [21] + 3    < 23!

  ; Clear Line Counter
  lda #0                    ; [24] + 2
  sta LINE                  ; [26] + 3
B_EndLine66
  
  ; Reposition Sprite (Fine)
  txa                       ; [29] + 2
  sec                       ; [31] + 2
  sbc #105                  ; [33] + 2
  tax                       ; [35] + 2
  lda B_FineTuneEnd,X       ; [37] + 5
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
  bcc B_DrawLine5           ; [66] + 2/3
  jmp B_Continue3           ; [68] + 3     <= 71

B_DrawLine5
  ldx #B_GRIDCOL7           ; [69] + 2
  nop                       ; [71] + 2
  jmp B_Continue5           ; [73] + 3    = 76
    
B_NoLine77
  SLEEP 6                   ; [20] + 6
  bcs B_EndLine77           ; [26] + 3
  
B_Repos77
  ; Check Line
  cpy LINE                  ; [14] + 3
  bcs B_NoLine77            ; [17] + 2/3

  ; Draw Line
  lda #B_GRIDCOL7           ; [19] + 2
  sta COLUBK                ; [21] + 3    < 23!

  ; Clear Line Counter
  lda #0                    ; [24] + 2
  sta LINE                  ; [26] + 3
B_EndLine77
  
  ; Reposition Sprite (Fine)
  txa                       ; [29] + 2
  sec                       ; [31] + 2
  sbc #120                  ; [33] + 2
  tax                       ; [35] + 2
  lda B_FineTuneEnd,X       ; [37] + 5
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
  bcc B_DrawLine6           ; [68] + 2/3
  SLEEP 3                   ; [70] + 3
  jmp B_Continue4           ; [73] + 3

B_DrawLine6
  ; Set Line Colour
  ldx #B_GRIDCOL7           ; [71] + 2
  jmp B_Continue5           ; [73] + 3

B_NoLine88
  SLEEP 6                   ; [20] + 6
  bcs B_EndLine88           ; [26] + 3

B_Repos88
  ; Check Line
  cpy LINE                  ; [14] + 3
  bcs B_NoLine88            ; [17] + 2/3

  ; Draw Line
  lda #B_GRIDCOL7           ; [19] + 2
  sta COLUBK                ; [21] + 3    < 23!

  ; Clear Line Counter
  lda #0                    ; [24] + 2
  sta LINE                  ; [26] + 3
B_EndLine88
    
  ; Reposition Sprite (Fine)
  txa                       ; [29] + 2
  sec                       ; [31] + 2
  sbc #135                  ; [33] + 2
  tax                       ; [35] + 2
  lda B_FineTuneEnd,X       ; [37] + 5
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
  bcc B_DrawLine6           ; [68] + 2/3
  SLEEP 3                   ; [70] + 3
  jmp B_Continue4           ; [73] + 3

B_NoLine99
  SLEEP 6                   ; [20] + 6
  bcs B_EndLine99           ; [26] + 3    

B_Repos99
  ; Check Line
  cpy LINE                  ; [14] + 3
  bcs B_NoLine99            ; [17] + 2/3

  ; Draw Line
  lda #B_GRIDCOL7           ; [19] + 2
  sta COLUBK                ; [21] + 3    < 23!

  ; Clear Line Counter
  lda #0                    ; [24] + 2
  sta LINE                  ; [26] + 3
B_EndLine99
    
  ; Reposition Sprite (Fine)
  txa                       ; [29] + 2
  sec                       ; [31] + 2
  sbc #150                  ; [33] + 2
  tax                       ; [35] + 2
  lda B_FineTuneEnd,X       ; [37] + 5
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
  bcc B_DrawLine7           ; [63] + 2/3

  ; Set Line Colour
  ldx #0                    ; [65] + 2

  ; Reposition Sprite (Coarse)
  sta RESP1                 ; [67] + 3    = 70
  SLEEP 3                   ; [70] + 3
  jmp B_Continue4           ; [73] + 3
    
B_DrawLine7
  ; Reposition Sprite (Coarse)    
  sta.w RESP1               ; [66] + 4    = 70

  ; Set Line Colour
  ldx #B_GRIDCOL7           ; [70] + 2
  sta.w COLUP0              ; [72] + 4
  
  ; Reposition & Continue
  sta HMOVE                 ; [0] + 3
  jmp B_Continue6           ; [3] + 3
B_EndReposFunctions1
  if (>B_ReposFunctions1 != >B_EndReposFunctions1)
    echo "WARNING: Repositioning1 Crosses Page Boundary!"
  endif

  DC.B  "PARTI"

; -----------------------------------------------------------------------------
; PART 10 - KERNEL DATA (LOOKUP TABLES, SPRITES, etc.)
; -----------------------------------------------------------------------------

  ALIGN   256

  ; Include Alien Sprite Data
B_Aliens
B_Empty                     ; Empty Sprite
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000           ; Extra Bit
B_BigUFO_1                  ; UFO Big
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00111000
  DC.B  %01111100
  DC.B  %10101010
  DC.B  %01111100
  DC.B  %00111000
B_BigUFO_2                  ; UFO Medium
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00010000
  DC.B  %00111000
  DC.B  %01010100
  DC.B  %00111000
  DC.B  %00010000
B_BigUFO_3                  ; UFO Small
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000    
  DC.B  %00011000
  DC.B  %00111100
  DC.B  %00011000
B_BigUFO_4
  DC.B  %00011000           ; UFO Tiny
  DC.B  %00011000
B_SmallUFO_1                ; UFO Big
  DC.B  %00000000
  DC.B  %00000000    
  DC.B  %00111000
  DC.B  %01111100
  DC.B  %10101010
  DC.B  %01111100
  DC.B  %00101000
  DC.B  %00010000
B_SmallUFO_2                ; UFO Medium
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00010000
  DC.B  %00111000
  DC.B  %01010100
  DC.B  %00111000
  DC.B  %00010000
B_SmallUFO_3                ; UFO Small
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000    
  DC.B  %00010000
  DC.B  %00111000
  DC.B  %00010000
B_SmallUFO_4
  DC.B  %00011000           ; UFO Tiny
  DC.B  %00011000    
B_Alien1_1                  ; Alien1 Big (Frame 1)
  DC.B  %10011001
  DC.B  %01011010
  DC.B  %00111100
  DC.B  %01111110
  DC.B  %10101001
  DC.B  %01101010
  DC.B  %00111100
  DC.B  %00011000
B_Alien1_2                  ; Alien1 Big (Frame 2)
  DC.B  %10011001
  DC.B  %01011010
  DC.B  %00111100
  DC.B  %01111110
  DC.B  %10010101
  DC.B  %01010110
  DC.B  %00111100
  DC.B  %00011000
B_Alien1_3                  ; Alien 1 Medium
  DC.B  %00000000
  DC.B  %00000000    
  DC.B  %01010100
  DC.B  %00111000
  DC.B  %00111000
  DC.B  %01010100
  DC.B  %00111000
  DC.B  %00010000
B_Alien1_4                  ; Alien 1 Small
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00101000
  DC.B  %00010000
  DC.B  %00111000
  DC.B  %00010000
B_Alien1_5
  DC.B  %00011000           ; Alien 1 Tiny
  DC.B  %00011000
B_Alien2_1                  ; Alien 2 Big
  DC.B  %00101000
  DC.B  %00101000
  DC.B  %01111100
  DC.B  %10111010
  DC.B  %11111110
  DC.B  %01000100
  DC.B  %00111000
  DC.B  %00010000
B_Alien2_2                  ; Alien 2 Big (Frame 2)
  DC.B  %10010010
  DC.B  %01010100
  DC.B  %01111100
  DC.B  %10111010
  DC.B  %11111110
  DC.B  %01000100
  DC.B  %00111000
  DC.B  %00010000
B_Alien2_3                  ; Alien 2 Medium (Frame 1)
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00101000
  DC.B  %00111000
  DC.B  %01010100
  DC.B  %01111100
  DC.B  %00101000
  DC.B  %00010000
B_Alien2_4                  ; Alien 2 Medium (Frame 2)
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %01010100
  DC.B  %00111000
  DC.B  %01010100
  DC.B  %01111100
  DC.B  %00101000
  DC.B  %00010000
B_Alien2_5                  ; Alien 2 Small
  DC.B  %00000000
  DC.B  %00000000    
  DC.B  %00000000    
  DC.B  %00000000
  DC.B  %00101000
  DC.B  %00010000
  DC.B  %00101000
  DC.B  %00010000
B_Alien2_6
  DC.B  %00011000           ; Alien 2 Tiny
  DC.B  %00011000
B_BugAlien_1                ; Bug Alien Big
  DC.B  %01010100
  DC.B  %00111000
  DC.B  %11111110
  DC.B  %01101100
  DC.B  %01101100
  DC.B  %00101000
  DC.B  %01010100
  DC.B  %00010000
B_BugAlien_2                ; Bug Alien Medium
  DC.B  %00000000
  DC.B  %00000000    
  DC.B  %00100100
  DC.B  %01111110
  DC.B  %00111100
  DC.B  %00111100
  DC.B  %00011000
  DC.B  %00100100
B_BugAlien_3                ; Bug Alien Small
  DC.B  %00000000
  DC.B  %00000000    
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00111000
  DC.B  %00111000
  DC.B  %00010000
  DC.B  %00101000
B_BugAlien_4
  DC.B  %00011000           ; Bug Alien Tiny
  DC.B  %00011000
B_Sphere_1                  ; Sphere Big
  DC.B  %00111000
  DC.B  %01001100
  DC.B  %10010110
  DC.B  %11110010
  DC.B  %11001110
  DC.B  %10101010
  DC.B  %01110100
  DC.B  %00111000
B_Sphere_2                  ; Sphere Medium
  DC.B  %00000000
  DC.B  %00011000
  DC.B  %00110100
  DC.B  %01011010
  DC.B  %01110110
  DC.B  %01011010
  DC.B  %00101100
  DC.B  %00011000
B_Sphere_3                  ; Sphere Small
  DC.B  %00000000    
  DC.B  %00000000
  DC.B  %00010000
  DC.B  %00111000
  DC.B  %01110100
  DC.B  %01011100
  DC.B  %00101000
  DC.B  %00010000
B_Sphere_4
  DC.B  %00011000           ; Sphere Tiny
  DC.B  %00011000
B_Astronaut_1               ; Astronaut Big
  DC.B  %00101000
  DC.B  %00101000
  DC.B  %00111000
  DC.B  %01111100
  DC.B  %01010100
  DC.B  %00101000
  DC.B  %00101000
  DC.B  %00010000
B_Astronaut_2               ; Astronaut Medium
  DC.B  %00000000
  DC.B  %00000000    
  DC.B  %00101000
  DC.B  %00111000
  DC.B  %00010000
  DC.B  %00111000
  DC.B  %00101000
  DC.B  %00010000
B_Astronaut_3               ; Astronaut Small
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00000000    
  DC.B  %00101000
  DC.B  %00010000
  DC.B  %00111000
  DC.B  %00010000
B_Explosion_1               ; Alien Explosion (Small)
  DC.B  %00000000
  DC.B  %00000000
  DC.B  %00010100
  DC.B  %00011000
  DC.B  %00100100
  DC.B  %00001000
  DC.B  %00000000
  DC.B  %00000000
B_Explosion_2               ; Alien Explosion (Medium)
  DC.B  %00000000
  DC.B  %00010100
  DC.B  %00101000
  DC.B  %00011010
  DC.B  %01010100
  DC.B  %00101000
  DC.B  %00001000
  DC.B  %00000000
B_Explosion_3               ; Alien Explosion (Big1)
  DC.B  %00011000
  DC.B  %00100100
  DC.B  %00011100
  DC.B  %10101010
  DC.B  %00111100
  DC.B  %01011010
  DC.B  %00000100
  DC.B  %00010000
B_Explosion_4               ; Alien Explosion (Big2)
  DC.B  %00101000
  DC.B  %01100101
  DC.B  %00010000
  DC.B  %01001010
  DC.B  %00000101
  DC.B  %10001110
  DC.B  %00100010
  DC.B  %01010100
B_Explosion_5               ; Alien Explosion (Big3)
  DC.B  %00100001
  DC.B  %00000000
  DC.B  %10000000
  DC.B  %00000000
  DC.B  %00000100
  DC.B  %10000001
  DC.B  %00100000
  DC.B  %00000001
B_EndAliens
  if (>B_Aliens != >B_EndAliens)
    echo "WARNING: Alien Sprite Data crosses a page boundary!"
  endif
  
  ALIGN   256
  
  ; Include Alien Sprite Colours
B_AlienCols
  IF (PALCOLS)
B_EmptyCol
  DC.B  $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
B_BigUFOCol_1
  DC.B  $0E, $0E, $0E, $66, $48, $48, $48, $66
B_BigUFOCol_2
  DC.B  $0E, $0E, $0E, $66, $48, $48, $48, $66
B_BigUFOCol_3
  DC.B  $0E, $0E, $0E, $0E, $0E, $66, $48, $66
B_BigUFOCol_4
  DC.B  $48, $66
B_SmallUFOCol_1
  DC.B  $0E, $0E, $A6, $D6, $D6, $9A, $9A, $9A
B_SmallUFOCol_2
  DC.B  $0E, $0E, $0E, $A6, $D6, $9A, $9A, $9A
B_SmallUFOCol_3
  DC.B  $0E, $0E, $0E, $0E, $0E, $D6, $9A, $9A
B_SmallUFOCol_4
  DC.B  $D6, $9A
B_Alien1Col_1
  DC.B  $64, $64, $72, $58, $58, $72, $58, $58
B_Alien1Col_2
  DC.B  $64, $64, $72, $58, $58, $72, $58, $58
B_Alien1Col_3
  DC.B  $0E, $0E, $64, $72, $58, $58, $72, $58
B_Alien1Col_4
  DC.B  $0E, $0E, $0E, $0E, $64, $72, $58, $58
B_Alien1Col_5
  DC.B  $58, $64
B_Alien2Col_1
  DC.B  $64, $64, $A6, $A6, $AA, $AA, $AA, $AA
B_Alien2Col_2
  DC.B  $64, $64, $A6, $A6, $AA, $AA, $AA, $AA
B_Alien2Col_3
  DC.B  $0E, $0E, $64, $A6, $A6, $AA, $AA, $AA
B_Alien2Col_4
  DC.B  $0E, $0E, $64, $A6, $A6, $AA, $AA, $AA
B_Alien2Col_5
  DC.B  $0E, $0E, $0E, $0E, $64, $A6, $AA, $AA
B_Alien2Col_6
  DC.B  $AA, $64
B_BugAlienCol_1
  DC.B  $36, $36, $36, $28, $28, $28, $36, $28
B_BugAlienCol_2
  DC.B  $0E, $0E, $36, $36, $28, $28, $28, $36
B_BugAlienCol_3
  DC.B  $0E, $0E, $0E, $0E, $36, $28, $28, $36
B_BugAlienCol_4
  DC.B  $28, $36
B_SphereCol_1
  DC.B  $60, $60, $60, $62, $62, $62, $62, $62
B_SphereCol_2
  DC.B  $0E, $60, $60, $60, $62, $62, $62, $62
B_SphereCol_3
  DC.B  $0E, $0E, $60, $60, $62, $62, $62, $62
B_SphereCol_4
  DC.B  $60, $62
B_AstronautCol_1
  DC.B  $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
B_AstronautCol_2
  DC.B  $0E, $0E, $0C, $0C, $0C, $0C, $0C, $0C
B_AstronautCol_3
  DC.B  $0E, $0E, $0E, $0E, $0A, $0A, $0A, $0A
B_ExplosionCol_1
  DC.B  $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
B_ExplosionCol_2    
  DC.B  $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
B_ExplosionCol_3        
  DC.B  $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
B_ExplosionCol_4    
  DC.B  $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
B_ExplosionCol_5    
  DC.B  $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
  ELSE
B_EmptyCol
  DC.B  $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
B_BigUFOCol_1
  DC.B  $0E, $0E, $0E, $46, $28, $28, $28, $46
B_BigUFOCol_2
  DC.B  $0E, $0E, $0E, $46, $28, $28, $28, $46
B_BigUFOCol_3
  DC.B  $0E, $0E, $0E, $0E, $0E, $46, $28, $46
B_BigUFOCol_4
  DC.B  $28, $46
B_SmallUFOCol_1
  DC.B  $0E, $0E, $66, $86, $86, $AA, $AA, $AA
B_SmallUFOCol_2
  DC.B  $0E, $0E, $0E, $66, $86, $AA, $AA, $AA
B_SmallUFOCol_3
  DC.B  $0E, $0E, $0E, $0E, $0E, $86, $AA, $AA
B_SmallUFOCol_4
  DC.B  $86, $AA
B_Alien1Col_1
  DC.B  $44, $44, $B2, $C8, $C8, $B2, $C8, $C8
B_Alien1Col_2
  DC.B  $44, $44, $B2, $C8, $C8, $B2, $C8, $C8
B_Alien1Col_3
  DC.B  $0E, $0E, $44, $B2, $C8, $C8, $B2, $C8
B_Alien1Col_4
  DC.B  $0E, $0E, $0E, $0E, $44, $B2, $C8, $C8
B_Alien1Col_5
  DC.B  $C8, $44
B_Alien2Col_1
  DC.B  $44, $44, $66, $66, $6A, $6A, $6A, $6A
B_Alien2Col_2
  DC.B  $44, $44, $66, $66, $6A, $6A, $6A, $6A
B_Alien2Col_3
  DC.B  $0E, $0E, $44, $66, $66, $6A, $6A, $6A
B_Alien2Col_4
  DC.B  $0E, $0E, $44, $66, $66, $6A, $6A, $6A
B_Alien2Col_5
  DC.B  $0E, $0E, $0E, $0E, $44, $66, $6A, $6A
B_Alien2Col_6
  DC.B  $6A, $44
B_BugAlienCol_1
  DC.B  $D6, $D6, $D6, $28, $28, $28, $D6, $18
B_BugAlienCol_2
  DC.B  $0E, $0E, $D6, $D6, $28, $28, $28, $D6
B_BugAlienCol_3
  DC.B  $0E, $0E, $0E, $0E, $D6, $28, $28, $D6
B_BugAlienCol_4
  DC.B  $28, $D6
B_SphereCol_1
  DC.B  $40, $40, $40, $42, $42, $42, $42, $42
B_SphereCol_2
  DC.B  $0E, $40, $40, $40, $42, $42, $42, $42
B_SphereCol_3
  DC.B  $0E, $0E, $40, $40, $42, $42, $42, $42
B_SphereCol_4
  DC.B  $40, $42
B_AstronautCol_1
  DC.B  $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
B_AstronautCol_2
  DC.B  $0E, $0E, $0C, $0C, $0C, $0C, $0C, $0C
B_AstronautCol_3
  DC.B  $0E, $0E, $0E, $0E, $0A, $0A, $0A, $0A
B_ExplosionCol_1
  DC.B  $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
B_ExplosionCol_2    
  DC.B  $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
B_ExplosionCol_3        
  DC.B  $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
B_ExplosionCol_4    
  DC.B  $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
B_ExplosionCol_5    
  DC.B  $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
  ENDIF
B_EndAlienCols
  if (>B_AlienCols != >B_EndAlienCols)
    echo "WARNING: Alien Colour Data crosses a page boundary!"
  endif

  ALIGN   256

  ; Include Ship Sprite Data
B_Ship
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
B_EmptyShip
  DS.B  40,0
B_ForwardFlame1
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
B_ForwardFlame2
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
B_ReverseFlame1
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
B_ReverseFlame2
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
B_EndShip
  if (>B_Ship != >B_EndShip)
    echo "WARNING: Ship Sprite Data crosses a page boundary!"
  endif

  ALIGN   256

  ; Include Ship Explosion Data
B_ShipExplosion0
  DS.B  40,0
B_ShipExplosion1
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
B_ShipExplosion2
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
B_ShipExplosion3
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
B_ShipExplosion4
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
B_ShipExplosion5
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
B_EndShipExplosion
  if (>B_ShipExplosion0 != >B_EndShipExplosion)
    echo "WARNING: Ship Explosion Data crosses a page boundary!"
  endif

  ALIGN   256

  ; Laser Size Table (66 Entries)
B_LaserSize
  DS.B  32, 32
  DS.B  16, 16
  DS.B  8, 8
  DS.B  4, 4
  DS.B  2, 2
  DS.B  2, 1
  DS.B  2, 0
B_EndLaserSize
  if (>B_LaserSize != >B_EndLaserSize)
    echo "WARNING: LaserSize Table Crosses Page Boundary!"
  endif
  
B_ReposTable
  ; Repositioning Kernel Pointers (150 Entries)
  DS.B  15, #<B_Repos0
  DS.B  15, #<B_Repos1
  DS.B  15, #<B_Repos2
  DS.B  15, #<B_Repos3
  DS.B  15, #<B_Repos4
  DS.B  15, #<B_Repos5
  DS.B  15, #<B_Repos6
  DS.B  15, #<B_Repos7
  DS.B  15, #<B_Repos8
  DS.B  15, #<B_Repos9
B_EndReposTable
  if (>B_ReposTable != >B_EndReposTable)
    echo "WARNING: Repositioning Table Crosses Page Boundary!"
  endif

  ALIGN   256

  ; Include Ship Sprite Colours - (96 Entries)
  IF (PALCOLS)
B_DarkCol
  DC.B  $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
  DC.B  $4C,$4A,$48,$46,$44,$24,$64,$22
  DC.B  $62,$62,$60,$A0,$A0,$A0,$A0,$A0
  DC.B  $A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0
  DC.B  $60,$62,$62,$42,$64,$64,$44,$46
B_ShipCol
  DC.B  $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
  DC.B  $4C,$4A,$48,$46,$44,$24,$64,$22
  DC.B  $62,$62,$60,$AE,$AE,$AC,$AC,$AA
  DC.B  $AA,$A8,$A8,$A6,$A6,$A4,$A4,$A2
  DC.B  $60,$62,$62,$42,$64,$64,$44,$46
B_ExplosionCol
  DC.B  $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
  DC.B  $AE,$AE,$AE,$AE,$AE,$AE,$AE,$AE
  DC.B  $AE,$AE,$AE,$AE,$AE,$AC,$AC,$AA
  DC.B  $AA,$A8,$A8,$A6,$A6,$A4,$A4,$A2
  DC.B  $A2,$A0,$A0,$A0,$A0,$A0,$A0,$A0
  ELSE
B_DarkCol
  DC.B  $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
  DC.B  $2C,$2A,$28,$26,$24,$34,$44,$32
  DC.B  $42,$42,$40,$60,$60,$60,$60,$60
  DC.B  $60,$60,$60,$60,$60,$60,$60,$60
  DC.B  $40,$42,$42,$32,$44,$34,$24,$26
B_ShipCol
  DC.B  $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
  DC.B  $2C,$2A,$28,$26,$24,$34,$44,$32
  DC.B  $42,$42,$40,$6E,$6E,$6C,$6C,$6A
  DC.B  $6A,$68,$68,$66,$66,$64,$64,$62
  DC.B  $40,$42,$42,$32,$44,$34,$24,$26
B_ExplosionCol
  DC.B  $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
  DC.B  $6E,$6E,$6E,$6E,$6E,$6E,$6E,$6E
  DC.B  $6E,$6E,$6E,$6E,$6E,$6C,$6C,$6A
  DC.B  $6A,$68,$68,$66,$66,$64,$64,$62
  DC.B  $62,$60,$60,$60,$60,$60,$60,$60
  ENDIF
B_EndShipCol
  if (>B_ShipCol != >B_EndShipCol)
    echo "WARNING: Ship Colour Data crosses a page boundary!"
  endif

B_TimeBarData1
B_PF1BTime
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
B_PF2ATime
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
B_EndTimeBarData1
  if (>B_TimeBarData1 != >B_EndTimeBarData1)
    echo "WARNING: TimeBar Data 1 crosses a page boundary!"
  endif

  ; Time Bar Colours
B_TimeCol
  IF (PALCOLS)
  DC.B  $00, $0E, $28, $46, $64, $64, $64, $64
  DC.B  $84, $84, $84, $84, $84, $84, $84, $84
  DC.B  $84, $84, $84, $84, $84, $84, $84, $84
  DC.B  $84, $84, $84, $84, $84, $84, $84, $84
  DC.B  $84, $84, $84, $84, $84, $84, $84, $84, $84, $84
  ELSE
  DC.B  $00, $0E, $18, $26, $44, $44, $44, $44
  DC.B  $54, $54, $54, $54, $54, $54, $54, $54
  DC.B  $54, $54, $54, $54, $54, $54, $54, $54
  DC.B  $54, $54, $54, $54, $54, $54, $54, $54
  DC.B  $54, $54, $54, $54, $54, $54, $54, $54, $54, $54
  ENDIF
B_EndTimeCol
  if (>B_TimeCol != >B_EndTimeCol)
    echo "WARNING: TimeCol crosses a page boundary!"
  endif
  
  ALIGN 256
  
B_TimeBarData2
B_PF0ATime
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

B_PF0BTime
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
B_PF2BTime
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
B_PF1ATime
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
B_EndTimeBarData2
  if (>B_TimeBarData2 != >B_EndTimeBarData2)
    echo "WARNING: TimeBar Data 2 crosses a page boundary!"
  endif

  echo "----",($FFF0 - *) , "bytes left (BANK 3 - KERNEL B)"

  ORG     $AFF0
  RORG    $FFF0
  DC.B    0, 0, 0, 255
  DC.B    0, 0, 0, 0, 0, 0
  DC.W    (PlusAPI - $E000)
  DC.W    Init3, Init3

