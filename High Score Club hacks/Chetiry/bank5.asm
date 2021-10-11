; -----------------------------------------------------------------------------
; CHETIRY - Atari 2600 - (C) Copyright 2011 - AtariAge
; BANK 5 - TITLE SCREEN
; -----------------------------------------------------------------------------

  ; Logo Colours
  IF (PALMODE)
ORANGET   = $46
LGREENT   = $58
DGREENT   = $54
REDT      = $64
YELLOWT   = $3C
  ELSE
ORANGET   = $38
LGREENT   = $C8
DGREENT   = $C4
REDT      = $44
YELLOWT   = $DC
  ENDIF

  ; Logo Height
TITLEH    = 200

  ; Snare Drum Sounds
SNARE1    = %01100000
SNARE2    = %01100001
SLOW      = $FF
FAST      = $FE

  SEG     BANK5
  ORG     $D000
  RORG    $F000

  ; Space for DPC registers
  DS.B    256,0

Init5
  ; Switch to Bank 7
  nop     $FFFB
  ; Entry Point
  jmp     TitleScreen
EndTitleScreen
  ; Switch To Bank 7
  nop     $FFFB
EndTitleMusic
  ; Switch To Bank 7
  nop     $FFFB
  nop
  nop
  nop     ;
  nop
  nop
  nop     ;
  nop
  nop
  nop     ;
  nop
  nop
  nop     ;
  
; -----------------------------------------------------------------------------
; TITLE MUSIC
; -----------------------------------------------------------------------------

TitleMusic
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; Clear Percussion
  ldx #0                  ; [5] + 2
  stx AUDV1               ; [7] + 3
  
  ; Store Tune Type
  lax MUSIC               ; [10] + 3
  and #%11100000          ; [13] + 2
  sta TEMP                ; [15] + 3
  
  ; Decrement Tempo
  txa                     ; [18] + 2
  and #%00011111          ; [20] + 2
  sec                     ; [22] + 2
  sbc #1                  ; [24] + 2
  bmi PlayTitleNotes      ; [26] + 2/3
  ora TEMP                ; [28] + 3
  sta MUSIC               ; [31] + 3
  jmp EndTitleMusic       ; [34] + 3
  
PlayTitleNotes
  ; Reset Tempo
  lda TEMP                ; [29] + 3
  ora TEMPO               ; [32] + 2
  sta MUSIC               ; [34] + 3

  ; Advance Music
  sta TUNESTEP_W          ; [37] + 3
  nop                     ; [40] + 2
  
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; Drum Table
  ldx #>DrumTab0          ; [5] + 2
  stx DPTR+1              ; [7] + 3
  
  ; Drum Position
  IF (MELODY)
  lda TUNEHI_R            ; [10] + 4
  ELSE
  lda #0
  ENDIF
  cmp #$FF                ; [14] + 2      Wrap Marker
  beq SkipDrums           ; [16] + 2/3
  lsr                     ; [18] + 2
  sta TEMP                ; [20] + 3
  IF (MELODY)
  lda TUNELO_R            ; [23] + 3
  ELSE
  lda #0
  ENDIF
  tax                     ; [26] + 2
  and #%00000011          ; [28] + 2
  tay                     ; [30] + 2
  txa                     ; [32] + 2
  ror                     ; [34] + 2
  lsr TEMP                ; [36] + 5
  ror                     ; [41] + 2
  tax                     ; [43] + 2
  lda DrumTable,X         ; [45] + 4
  sta DPTR                ; [49] + 3

  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; Play Drums
  lda (DPTR),Y            ; [5] + 5
  beq SkipDrums           ; [10] + 2/3
  cmp #SLOW               ; [12] + 2
  beq SlowTempo           ; [14] + 2/3
  cmp #FAST               ; [16] + 2
  beq FastTempo           ; [18] + 2/3
  sta AUDF1               ; [20] + 3
  lda #8                  ; [23] + 2
  sta AUDC1               ; [25] + 3
  lsr                     ; [28] + 2
  sta AUDV1               ; [30] + 3
  jmp SkipDrums           ; [33] + 3
SlowTempo
  ldx #SLOWTEMPO          ; [17] + 2
  stx TEMPO               ; [19] + 3
  jmp SkipDrums           ; [22] + 3
FastTempo
  ldx #TITLETEMPO         ; [21] + 2
  stx TEMPO               ; [23] + 3
SkipDrums

  jmp EndTitleMusic       ; [36] + 3

  DC.B  "NIKITA"

; -----------------------------------------------------------------------------
; TITLE SCREEN
; -----------------------------------------------------------------------------

  ALIGN 256

TitleScreen
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5
  
  ; Set Three Sprite Copies
  ldx #%00000110          ; [5] + 2
  stx NUSIZ0              ; [7] + 3
  stx NUSIZ1              ; [10] + 3

  ; Delay P0 & P1
  ldx #%00000001          ; [13] + 2
  stx VDELP0              ; [15] + 3
  stx VDELP1              ; [18] + 3

  ; Swap Kernels on Alternate Cycles
  lda CYCLE               ; [21] + 3
  lsr                     ; [24] + 2
  bcs TitleKernel1        ; [26] + 2/3
  jmp TitleKernel2        ; [28] + 3
  
; -----------------------------------------------------------------------------
; TITLE KERNEL 1
; -----------------------------------------------------------------------------

TitleKernel1
  START_FRAME_MUSIC
  
  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; Sprite Positioning (Fine Tuning)
  ldx #%10010000          ; [5] + 2
  stx HMP0                ; [7] + 3
  ldx #%10000000          ; [10] + 2
  stx HMP1                ; [12] + 3

  ; Set Lines To Display
  ldy #TITLEH             ; [15] + 2

  ; Set Sprite Colours
  lda TitleCols,Y         ; [17] + 4
  sta COLUP0              ; [21] + 3
  sta COLUP1              ; [24] + 3

  ; Sprite Positioning (Coarse)
  nop                     ; [27] + 2
  sta RESP0               ; [29] + 3      = 32
  nop                     ; [32] + 2
  sta RESP1               ; [34] + 3      = 37

TitleLoop1A
  sta WSYNC               ; [0]
  sta HMOVE               ; [0] + 3       P0A P1A P0B P1B
  PLAY_MUSIC              ; [3] + 5
  lda #0                  ; [8] + 2
  sta GRP0                ; [10] + 3      - - 0 -
  sta GRP1                ; [13] + 3      0 - - 3   < 36
  lda Title5,Y            ; [16] + 4
  sta GRP0                ; [20] + 3      0 3 5 -   < 42
  sta HMCLR               ; [23] + 3      > 24
  SLEEP 6                 ; [26] + 6
  lda Title7,Y            ; [32] + 4
  sta GRP1                ; [36] + 3      5 3 - 7   > 38 < 47
  lda Title9,Y            ; [39] + 4
  sta GRP0                ; [43] + 3      5 7 9 -   > 44 < 52 
  lda #0                  ; [46] + 2
  sta GRP1                ; [48] + 3      9 7 - 11  > 49 < 58
  dey                     ; [51] + 2
  sta GRP0                ; [53] + 3      9 11 - -  > 54 < 63
  lda TitleCols,Y         ; [56] + 4
  sta COLUP0              ; [60] + 3
  sta COLUP1              ; [63] + 3
  SLEEP 5                 ; [66] + 5
  sta HMOVE               ; [71] + 3      = 74!
  PLAY_MUSIC              ; [74] + 5
  lda #0                  ; [3] + 2       P0A P1A P0B P1B  
  sta GRP0                ; [5] + 3       - - 0 -
  lda Title2,Y            ; [8] + 4
  sta GRP1                ; [12] + 3      0 - - 2   < 34
  lda Title4,Y            ; [15] + 4
  sta GRP0                ; [19] + 3      0 2 4 -   < 39
  lda #%10000000          ; [22] + 2
  sta HMP0                ; [24] + 3      > 24
  sta HMP1                ; [27] + 3      > 24
  lda Title6,Y            ; [30] + 4
  sta GRP1                ; [34] + 3      4 2 - 6   > 36 < 44
  lda #0                  ; [37] + 2
  sta GRP0                ; [39] + 3      4 6 8 -   > 41 < 50
  SLEEP 3                 ; [42] + 3
  sta GRP1                ; [45] + 3      8 6 - 10  > 47 < 55
  lda TitleCols-1,Y       ; [48] + 4
  sta GRP0                ; [52] + 3      8 11 - -  > 52 < 60
  sta COLUP0              ; [55] + 3
  sta COLUP1              ; [58] + 3
  dey                     ; [61] + 2
  bmi TitleLoop1A         ; [63] + 2/3
  jmp Title1B             ; [65] + 3
EndTitleKernel1A
  if (>TitleKernel1 != >EndTitleKernel1A)
    echo "WARNING: Title Kernel1A Crosses Page Boundary!"
  endif
  DC.B   "T0"
  
  ALIGN 256

  ; Display Sprites On Alternate Lines
Title1B
  sta WSYNC               ; [0]
TitleLoop1B
  sta HMOVE               ; [0] + 3       P0A P1A P0B P1B
  PLAY_MUSIC              ; [3] + 5
  nop                     ; [8] + 2
  lda Title1,Y            ; [10] + 4
  sta GRP0                ; [14] + 3       - - 0 -
  lda Title3,Y            ; [17] + 4
  sta GRP1                ; [21] + 3      0 - - 3   < 36
  lda Title5,Y            ; [24] + 4
  sta GRP0                ; [28] + 3      0 3 5 -   < 42
  sta HMCLR               ; [31] + 3      > 24
  lda Title7,Y            ; [34] + 4
  sta GRP1                ; [38] + 3      5 3 - 7   > 38 < 47
  lda Title9,Y            ; [41] + 4
  sta GRP0                ; [45] + 3      5 7 9 -   > 44 < 52 
  lda Title11,Y           ; [48] + 4
  sta GRP1                ; [52] + 3      9 7 - 11  > 49 < 58
  sta GRP0                ; [55] + 3      9 11 - -  > 54 < 63
  dey                     ; [58] + 2
  lda TitleCols,Y         ; [60] + 4
  sta COLUP0              ; [64] + 3
  ldx #%10000000          ; [67] + 2
  nop                     ; [69] + 2
  sta HMOVE               ; [71] + 3      = 74!
  sta COLUP1              ; [3] + 3
  PLAY_MUSIC              ; [74] + 5
  lda Title0,Y            ; [6] + 4       P0A P1A P0B P1B  
  sta GRP0                ; [10] + 3        - - 0 -
  lda Title2,Y            ; [13] + 4
  sta GRP1                ; [17] + 3      0 - - 2   < 34
  lda Title4,Y            ; [20] + 4
  sta GRP0                ; [24] + 3      0 2 4 -   < 39
  stx HMP1                ; [27] + 3      > 24
  lda Title6,Y            ; [30] + 4
  sta GRP1                ; [34] + 3      4 2 - 6   > 36 < 44
  lda Title8,Y            ; [37] + 4
  sta GRP0                ; [41] + 3      4 6 8 -   > 41 < 50 
  lda Title10,Y           ; [44] + 4
  sta GRP1                ; [48] + 3      8 6 - 10  > 47 < 55
  sta GRP0                ; [51] + 3      8 11 - -  > 52 < 60
  stx HMP0                ; [54] + 3      > 24
  SLEEP 4                 ; [57] + 4
  lda TitleCols-1,Y       ; [61] + 4
  sta COLUP0              ; [65] + 3
  sta COLUP1              ; [68] + 3
  dey                     ; [71] + 2
  bne TitleLoop1B         ; [73] + 2/3
  PLAY_MUSIC              ; [75] + 5
  ; Clear Sprite Data
  lda #0                  ; [4] + 2
  sta GRP0                ; [6] + 3
  sta GRP1                ; [9] + 3
  sta GRP0                ; [12] + 3
  jmp EndTitleScreen      ; [15] + 3
EndTitleKernel1B
  if (>Title1B != >EndTitleKernel1B)
    echo "WARNING: Title Kernel1B Crosses Page Boundary!"
  endif

  ; Title Data
  TITLE0
  DC.B   "T1"

; -----------------------------------------------------------------------------
; TITLE KERNEL 2
; -----------------------------------------------------------------------------

  ALIGN 256

TitleKernel2
  START_FRAME_MUSIC

  sta WSYNC               ; [0]
  PLAY_MUSIC              ; [0] + 5

  ; Position Sprites
  SLEEP 24                ; [5] + 24
  sta RESP0               ; [29] + 3  = 32
  nop                     ; [32] + 2
  sta RESP1               ; [34] + 3  = 37
  ldx #%10010000          ; [37] + 2
  stx HMP0                ; [39] + 3
  ldx #%10000000          ; [42] + 2
  stx HMP1                ; [44] + 3

  ; Title Height
  ldy #TITLEH             ; [47] + 2

  ; Set Lines To Display
  SLEEP 6                 ; [49] + 6

TitleKernel2B
  ; Set Sprite Colours
  lda TitleCols,Y         ; [55] + 4
  sta COLUP0              ; [59] + 3
  sta COLUP1              ; [62] + 3

TitleLoop2A
  SLEEP 6                 ; [65] + 6
  sta HMOVE               ; [71] + 3      = 74!
  ldx #0                  ; [74] + 2      P0A P1A P0B P1B  
  PLAY_MUSIC              ; [0] + 5
  stx GRP0                ; [5] + 3       - - 0 -
  lda Title2,Y            ; [8] + 4
  sta GRP1                ; [12] + 3      0 - - 2   < 34
  lda Title4,Y            ; [15] + 4
  sta GRP0                ; [19] + 3      0 2 4 -   < 39
  lda #%10000000          ; [22] + 2
  sta HMP0                ; [24] + 3      > 24
  sta HMP1                ; [27] + 3      > 24
  lda Title6,Y            ; [30] + 4
  sta GRP1                ; [34] + 3      4 2 - 6   > 36 < 44
  lda #0                  ; [37] + 2
  sta GRP0                ; [39] + 3      4 6 8 -   > 41 < 50
  ldx TitleCols-1,Y       ; [42] + 4
  sta GRP1                ; [46] + 3      8 6 - 10  > 47 < 55
  dey                     ; [49] + 2
  sta GRP0                ; [51] + 3      8 11 - -  > 52 < 60
  stx COLUP0              ; [54] + 3
  stx COLUP1              ; [57] + 3
  sta WSYNC               ; [0]
  sta HMOVE               ; [0] + 3       P0A P1A P0B P1B
  PLAY_MUSIC              ; [3] + 5
  lda #0                  ; [8] + 2
  sta GRP0                ; [10] + 3      - - 0 -
  sta GRP1                ; [13] + 3      0 - - 3    < 36
  lda Title5,Y            ; [16] + 4
  sta GRP0                ; [20] + 3      0 3 5 -    < 42
  nop                     ; [23] + 2
  ldx TitleCols-1,Y       ; [25] + 4
  sta HMCLR               ; [29] + 3      > 24
  lda Title7,Y            ; [32] + 4
  sta GRP1                ; [36] + 3      5 3 - 7    > 38 < 47
  lda Title9,Y            ; [39] + 4
  sta GRP0                ; [43] + 3      5 7 9 -   > 44 < 52
  lda #0                  ; [46] + 2
  sta GRP1                ; [48] + 3      9 7 - 11  > 49 < 58
  dey                     ; [51] + 2
  sta GRP0                ; [53] + 3      9 11 - -  > 54 < 63
  stx COLUP0              ; [56] + 3
  stx COLUP1              ; [59] + 3
  bmi TitleLoop2A         ; [62] + 2/3
  SLEEP 4                 ; [64] + 4
  jmp TitleLoop2B         ; [68] + 3
EndTitleKernel2A
  if (>TitleKernel2 != >EndTitleKernel2A)
    echo "WARNING: Title Kernel2A Crosses Page Boundary!"
  endif
  DC.B   "T2"

  ALIGN 256

Title2B
TitleLoop2B
  sta HMOVE               ; [71] + 3  = 74!
  nop                     ; [74] + 2
  PLAY_MUSIC              ; [0] + 5
  SLEEP 3                 ; [5] + 3
  lda Title0,Y            ; [8] + 4       P0A P1A P0B P1B  
  sta GRP0                ; [12] + 3      - - 0 -
  lda Title2,Y            ; [15] + 4
  sta GRP1                ; [19] + 3      0 - - 2    < 34
  lda Title4,Y            ; [22] + 4
  sta.w GRP0              ; [26] + 4      0 2 4 -    < 39
  lda Title6,Y            ; [30] + 4  
  sta GRP1                ; [34] + 3      4 2 - 6    > 36 < 44
  lda Title8,Y            ; [37] + 4
  sta GRP0                ; [41] + 3      4 6 8 -    > 41 < 50 
  lda Title10,Y           ; [44] + 4
  sta GRP1                ; [48] + 3      8 6 - 10  > 47 < 55
  sta GRP0                ; [51] + 3      8 11 - -  > 52 < 60
  dey                     ; [54] + 2
  lda TitleCols,Y         ; [56] + 4
  sta COLUP0              ; [60] + 3
  sta COLUP1              ; [63] + 3
  lda #%10000000          ; [66] + 2
  sta HMP0                ; [68] + 3
  sta HMP1                ; [71] + 3
  nop                     ; [74] + 2
  sta HMOVE               ; [0] + 3       P0A P1A P0B P1B
  PLAY_MUSIC              ; [3] + 5
  lda Title1,Y            ; [8] + 4
  sta GRP0                ; [12] + 3      - - 0 -
  lda Title3,Y            ; [15] + 4
  sta GRP1                ; [19] + 3      0 - - 3    < 36
  lda Title5,Y            ; [22] + 4
  sta GRP0                ; [26] + 3      0 3 5 -    < 42
  ldx Title11,Y           ; [29] + 4  
  lda Title7,Y            ; [33] + 4
  sta GRP1                ; [37] + 3      5 3 - 7    > 38 < 47
  lda Title9,Y            ; [40] + 4  
  sta GRP0                ; [44] + 3      5 7 9 -    > 44 < 52
  stx GRP1                ; [47] + 3      9 7 - 11  > 49 < 58
  sta HMCLR               ; [50] + 3  
  sta GRP0                ; [53] + 3      9 11 - -  > 54 < 63
  lda TitleCols-1,Y       ; [56] + 4
  sta COLUP0              ; [60] + 3
  sta COLUP1              ; [63] + 3
  dey                     ; [66] + 2
  bne TitleLoop2B         ; [68] + 2/3
  SLEEP 5                 ; [70] + 5
  PLAY_MUSIC              ; [75] + 5
  ; Clear Sprite Data
  lda #0                  ; [4] + 2
  sta GRP0                ; [6] + 3
  sta GRP1                ; [9] + 3
  sta GRP0                ; [12] + 3
  jmp EndTitleScreen      ; [15] + 3
EndTitleKernel2B
  if (>Title2B != >EndTitleKernel2B)
    echo "WARNING: Title Kernel2B Crosses Page Boundary!"
  endif

  ; Title Data
  TITLE1
  DC.B   "T3"

; -----------------------------------------------------------------------------
; TITLE DATA
; -----------------------------------------------------------------------------

  ; Title Image Data
  ALIGN 256
  TITLE2
  ALIGN 256
  TITLE3
  TITLE8
  ALIGN 256
  TITLE4
  ALIGN 256
  TITLE5
  ALIGN 256
  TITLE6
  ALIGN 256
  TITLE7
  ALIGN 256
  TITLE9
  ALIGN 256
  TITLE10
  TITLE11
  ALIGN 256
  TITLECOLS

  ; Tune Drum Data
  ALIGN 256
  DRUMDATA

  echo "----",($FFF4 - *) , "bytes left (BANK 5 - TITLE SCREEN)"

  ORG     $DFF4
  RORG    $FFF4
  DC.B    "BANK5", 0
  DC.W    (PlusROM_API - $D000)
  DC.W    Init5, Init5

