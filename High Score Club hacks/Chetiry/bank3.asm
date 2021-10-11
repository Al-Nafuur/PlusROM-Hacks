; -----------------------------------------------------------------------------
; CHETIRY - Atari 2600 - (C) Copyright 2011 - AtariAge
; BANK 3 - SHUTTLE A
; -----------------------------------------------------------------------------

  SEG     BANK3
  ORG     $B000
  RORG    $F000

  ; Space for DPC registers
  DS.B    256,0
  DC.B    "BAIKONUR"

; -------------------------- LAUNCH PAD ---------------------------------------

LaunchPadLoop
  jmp LaunchPadStart      ; [65] + 3
LaunchPad
  ldy #21                 ; [63] + 2
  sty TLINE               ; [65] + 3
LaunchPadStart
  ldx LaunchPad0,Y        ; [68] + 4
  stx TEMP                ; [72] + 3
  PLAY_MUSIC              ; [75] + 5
  stx GRP0                ; [4] + 3
  lda LaunchPadPF,Y       ; [7] + 4
  sta PF2                 ; [11] + 3
  ora #%11111100          ; [14] + 2
  sta PF1                 ; [16] + 3
  lda LaunchPad1,Y        ; [19] + 4
  sta GRP1                ; [23] + 3
  lax LaunchPad4,Y        ; [26] + 4
  lda LaunchPad2,Y        ; [30] + 4
  sta GRP0                ; [34] + 3
  nop                     ; [37] + 2
  ldy TEMP                ; [39] + 3
  sta GRP1                ; [42] + 3  >= 44 < 47
  stx GRP0                ; [45] + 3  > 46 < 50
  sty GRP1                ; [48] + 3  > 49 <= 52
  sta GRP0                ; [51] + 3  >= 52 < 55
  dec TLINE               ; [54] + 5
  ldy TLINE               ; [59] + 3
  bpl LaunchPadLoop       ; [62] + 2/3
  sty PF1                 ; [64] + 3
  sty PF2                 ; [67] + 3
  ldy #0                  ; [70] + 2
  sty GRP1                ; [72] + 3
  PLAY_MUSIC              ; [75] + 5
  sty GRP0                ; [4] + 3
  sty GRP1                ; [7] + 3
  jmp FinishScreen        ; [10] + 3

; -------------------------- TAKE OFF -----------------------------------------

TakeOffLoop
  PLAY_MUSIC              ; [61] + 5
  jmp TakeOffStart        ; [66] + 3
  nop

  ; Entry Point
  ORG     $B15C
  RORG    $F15C

TakeOffStart
  ldy TLINE               ; [69] + 3
  lda (TDATA0),Y          ; [72] + 5
  sta GRP0                ; [1] + 3
  lda (TDATA1),Y          ; [4] + 5
  sta GRP1                ; [9] + 3
  lda (TDATA2),Y          ; [12] + 5
  sta GRP0                ; [17] + 3
  lax (TDATA4),Y          ; [20] + 5
  lda (TDATA5),Y          ; [25] + 5
  sta TEMP                ; [30] + 3
  lda (TDATA3),Y          ; [33] + 5
  ldy TEMP                ; [38] + 3
  sta GRP1                ; [41] + 3  >= 44 < 47
  stx GRP0                ; [44] + 3  > 46 < 50
  sty GRP1                ; [47] + 3  > 49 <= 52
  sta GRP0                ; [50] + 3  >= 52 < 55
  dec TLINE               ; [53] + 5
  bpl TakeOffLoop         ; [58] + 2/3
  jmp LaunchPad           ; [60] + 3

FinishScreen
  START_OVERSCAN_MUSIC
  
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  
  ; Clear Playfield
  ldy #0                  ; [5] + 2
  sty COLUBK              ; [7] + 3
  sty COLUPF              ; [10] + 3
  sty PF0                 ; [13] + 3
  sty PF1                 ; [16] + 3
  sty PF2                 ; [19] + 3
  
  WAIT_OVERSCAN_MUSIC
  jmp ExitLaunch

  DC.B    "ENERGIA"
  CHECKPAGE LaunchPadLoop, "LaunchPadLoop"

; -------------------------- LAUNCH DATA --------------------------------------

  ALIGN 256
  LAUNCH0
  ALIGN 256
  LAUNCH1
  ALIGN 256
  LAUNCH2
  ALIGN 256
  LAUNCH3
  ALIGN 256
  LAUNCH4
  ALIGN 256
  LAUNCH5
  ALIGN 256
  LAUNCH6
  ALIGN 256
  LAUNCH7
  ALIGN 256
  LAUNCH8
  ALIGN 256
  LAUNCH9
  ALIGN 256
  LAUNCH10
  ALIGN 256
  LAUNCH11
  ALIGN 256
  LAUNCH12
  ALIGN 256
  LAUNCH13

  DC.B    "ROSCOSMOS"

  echo "----",($FFE6 - *) , "bytes left (BANK 3 - SHUTTLE A)"

  ; Switch Points
  ORG     $BFE6
  RORG    $FFE6
Init3
  ; Switch to Bank 7
  nop     $FFFB
ExitLaunch
  ; Switch To Bank 4
  nop     $FFF8
  nop
  nop
  nop

  ORG     $BFF4
  RORG    $FFF4
  DC.B    "BANK3", 0
  DC.W    (PlusROM_API - $D000)
  DC.W    Init3, Init3
