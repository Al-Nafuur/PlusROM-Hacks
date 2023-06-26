; -----------------------------------------------------------------------------
; BANK 5 - ALIEN SPAWNING
; -----------------------------------------------------------------------------

  SEG     BANK5
  ORG     $C000
  RORG    $F000

Init5
  ; Switch to Bank 8
  nop     $FFFB
  nop
  nop
  nop
EndWaveSpawn
  ; Switch to Bank 4
  nop     $FFF7
 
  ; Spawn New Aliens
StartWaveSpawn
  ; Don't Spawn If Paused
  bit WAVE                  ; [0] + 3
  bmi EndSpawn              ; [3] + 2/3 = 5
      
  ; Calculate Spawn Table Pointer Based On Wave
  lda SpawnTable+0,Y        ; [0] + 4
  sta SPTR+0                ; [4] + 3 
  lda SpawnTable+1,Y        ; [7] + 4
  sta SPTR+1                ; [11] + 3 = 14

  ; Find First Free Alien Slot
  ldx #INVMAX
FindLoop
  dex                       ; [0] + 2
  bmi EndSpawn              ; [2] + 2/3
  lda INVT,X                ; [4] + 4
  bne FindLoop              ; [8] + 2/3 = 11
  txs                       ; WORST CASE = 11 * 16 + 2 = 178 (2.5 SCANLINES)

  ; NEW FORMAT
  ; 0 - Type (0-2), Spawn Time (3-7)
  ; 1 - XPOS (0-3), YPOS (4-7)
  ; X = XMIN + XPOS<<3 (Range = XMIN -> XMIN + 120, 0 = RANDOM)
  ; Y = INVYMIN + YPOS<<3 (Range = INVYMIN -> INVYMIN + 120)
  
  ; Check If We Are Finished Spawning
  lda SPAWN                 ; [0] + 3
  bmi EndSpawn              ; [3] + 2/3
  ; Get Offset Into Spawn Table
  asl                       ; [5] + 2
  tay                       ; [7] + 2
  ; Check Spawn Time
  lax (SPTR),Y              ; [9] + 5
  and #%11111000            ; [14] + 2
  lsr                       ; [16] + 2
  lsr                       ; [18] + 2
  lsr                       ; [20] + 2
  cmp STIME                 ; [22] + 3
  bcc NoSpawn               ; [25] + 2/3
  ; Get Alien Type
  txa                       ; [27] + 2
  and #%00000111            ; [29] + 2
  tax                       ; [31] + 2
  lda TypeTable,X           ; [33] + 4
  tsx                       ; [37] + 2
  sta INVT,X                ; [39] + 4
  ; Get X Position
  iny                       ; [43] + 2
  lda (SPTR),Y              ; [45] + 5
  and #%00001111            ; [50] + 2
  bne NotRandomX            ; [52] + 2/3
  lda RANDOM                ; [54] + 3
  and #%01111100            ; [57] + 2
  clc                       ; [59] + 2
  adc #XMIN                 ; [61] + 2
  jmp StoreX                ; [63] + 3
NotRandomX
  asl                       ; [55] + 2
  asl                       ; [57] + 2
  asl                       ; [59] + 2
StoreX
  sta INVX,X                ; [66] + 4
  ; Get Y Position
  lda (SPTR),Y              ; [70] + 5
  and #%11110000            ; [75] + 2
  lsr                       ; [77] + 2
  adc #INVYMIN              ; [79] + 2
  sta INVY,X                ; [81] + 4
  ; Play Spawn Sound
  lda SND0                  ; [85] + 3
  beq PlaySpawnSound        ; [88] + 2/3
  and #%11100000            ; [90] + 2
  cmp #5<<5                 ; [92] + 2
  bcc EndSpawnSound         ; [94] + 2/3
PlaySpawnSound
  lda #((5<<5)|17)          ; [96] + 2
  sta SND0                  ; [98] + 3
EndSpawnSound
  ; Decrease Spawn Counter
  dec SPAWN                 ; [101] + 5
EndSpawn
  ; Return to game logic
  jmp EndWaveSpawn          ; [106] + 3
                            ; WORST CASE = 109 CYCLES (1.5 SCANLINES)

NoSpawn
  ; Advance Spawn Time If No Aliens On Screen!
  lda STIME
  beq EndSpawn
  lda KILLS
  cmp SPAWN
  bne EndSpawn
  dec STIME
  jmp EndWaveSpawn

  ALIGN 256
  
; -----------------------------------------------------------------------------
; GAME WAVE DATA
; -----------------------------------------------------------------------------

  ; Alien Type Table (For Spawning)
TypeTable
  ; Bit 7 = Alien Type Bit 1
  ; Bit 6 = Alien Type Bit 0
  ; Bit 5->2 = 12 (Start of Warp Effect)
  ; Bit 1 = Alien Type Bit 3
  ; Bit 0 = Alien Type Bit 2
  DC.B  %00000000        ; 0
  DC.B  %01110000        ; 1
  DC.B  %10110000        ; 2
  DC.B  %11110000        ; 3
  DC.B  %00110001        ; 4
  DC.B  %01110001        ; 5
  DC.B  %10110001        ; 6
EndTypeTable

  ; Spawn Table (2 Bytes Per Wave)
  ; 0 - Table LSB
  ; 1 - Table MSB
SpawnTable
  DC.B  <Wave1, >Wave1
  DC.B  <Wave2, >Wave2
  DC.B  <Wave3, >Wave3
  DC.B  <Wave4, >Wave4
  DC.B  <Wave5, >Wave5
  DC.B  <Wave6, >Wave6
  DC.B  <Wave7, >Wave7
  DC.B  <Wave8, >Wave8
  DC.B  <Wave9, >Wave9
  DC.B  <Wave10, >Wave10
  DC.B  <Wave11, >Wave11
  DC.B  <Wave12, >Wave12
  DC.B  <Wave13, >Wave13
  DC.B  <Wave14, >Wave14
  DC.B  <Wave15, >Wave15
  DC.B  <Wave16, >Wave16
  DC.B  <Wave17, >Wave17
  DC.B  <Wave18, >Wave18
  DC.B  <Wave19, >Wave19
  DC.B  <Wave20, >Wave20
  DC.B  <Wave21, >Wave21
  DC.B  <Wave22, >Wave22
  DC.B  <Wave23, >Wave23
  DC.B  <Wave24, >Wave24
  DC.B  <Wave25, >Wave25
  DC.B  <Wave26, >Wave26
  DC.B  <Wave27, >Wave27
  DC.B  <Wave28, >Wave28
  DC.B  <Wave29, >Wave29
  DC.B  <Wave30, >Wave30
  DC.B  <Wave31, >Wave31
  DC.B  <Wave32, >Wave32
  DC.B  <Wave1, >Wave1
  DC.B  <Wave2, >Wave2
  DC.B  <Wave3, >Wave3
  DC.B  <Wave4, >Wave4
  DC.B  <Wave5, >Wave5
  DC.B  <Wave6, >Wave6
  DC.B  <Wave7, >Wave7
  DC.B  <Wave8, >Wave8
  DC.B  <Wave9, >Wave9
  DC.B  <Wave10, >Wave10
  DC.B  <Wave11, >Wave11
  DC.B  <Wave12, >Wave12
  DC.B  <Wave13, >Wave13
  DC.B  <Wave14, >Wave14
  DC.B  <Wave15, >Wave15
  DC.B  <Wave16, >Wave16
  DC.B  <Wave17, >Wave17
  DC.B  <Wave18, >Wave18
  DC.B  <Wave19, >Wave19
  DC.B  <Wave20, >Wave20
  DC.B  <Wave21, >Wave21
  DC.B  <Wave22, >Wave22
  DC.B  <Wave23, >Wave23
  DC.B  <Wave24, >Wave24
  DC.B  <Wave25, >Wave25
  DC.B  <Wave26, >Wave26
  DC.B  <Wave27, >Wave27
  DC.B  <Wave28, >Wave28
  DC.B  <Wave29, >Wave29
  DC.B  <Wave30, >Wave30
  DC.B  <Wave31, >Wave31
  DC.B  <Wave32, >Wave32
EndSpawnTable
  if (>SpawnTable != >EndSpawnTable)
    echo "WARNING: Spawn Table Cross Page Boundary!"
  endif
  
  ; Wave Format
  ; 0 - Type (0-2), Spawn Time (3-7)
  ; 1 - XPOS (0-3), YPOS (4-7)
  ; X = XPOS<<3 (Range = 0 -> 120, 0=Random)
  ; Y = 100 + YPOS<<3 (Range = 100 -> 220)
  
  ; Include Wave Data
  ALIGN 256
  WAVE1
  WAVE2
  WAVE3
  WAVE4

  ALIGN 256
  WAVE5
  WAVE6

  ALIGN 256
  WAVE7
  WAVE8
  
  ALIGN 256
  WAVE9
  WAVE10
  WAVE11
  
  ALIGN 256
  WAVE13
  WAVE14
  
  ALIGN 256
  WAVE12
  WAVE15
  WAVE16
  
  ALIGN 256
  WAVE17
  WAVE18
  
  ALIGN 256
  WAVE19
  WAVE20
  
  ALIGN 256
  WAVE21
  WAVE22
  
  ALIGN 256
  WAVE23
  WAVE24
  
  ALIGN 256
  WAVE25
  WAVE26
  
  ALIGN 256
  WAVE27
  WAVE28
  
  ALIGN 256
  WAVE29
  WAVE30
  
  ALIGN 256
  WAVE31
  WAVE32

  echo "----",($FFF0 - *) , "bytes left (BANK 5 - WAVE DATA)"

  ORG     $CFF0
  RORG    $FFF0
  DC.B    0, 0, 0, 255
  DC.B    0, 0, 0, 0, 0, 0
  DC.W    (PlusAPI - $E000)
  DC.W    Init5, Init5

