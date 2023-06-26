; -----------------------------------------------------------------------------
; BANK 7 - GAME SPEECH & TITLE MUSIC
; -----------------------------------------------------------------------------

  SEG     BANK7
  ORG     $E000
  RORG    $F000

Init7
  ; Switch to Bank 8
  nop     $FFFB
  nop
  nop
  nop
ExitMusic
  ; Switch to Bank 8
  nop     $FFFB
StartMusic
  jmp Player
  nop
  nop
  nop
ExitSpeech
  ; Switch To Bank 1
  nop     $FFF4

; -----------------------------------------------------------------------------
; PART 1 - ATARIVOX SPEECH PLAYER
; -----------------------------------------------------------------------------

  ; Speech Player
  ; On Entry - A contains SPEECH value, Y contains counter
StartSpeech
  ; Get Speech Type
  and #%11000000            ; [0] + 2
  ; Play Wave Number  If Type = 0
  beq WaveSpeech            ; [2] + 2/3
  ; Store Speech Counter
  sty TEMP                  ; [4] + 3
  ora TEMP                  ; [7] + 3
  sta SPEECH                ; [10] + 3
  ; Make Pointer To Speech Data
  clc                       ; [13] + 2
  rol                       ; [15] + 2
  rol                       ; [17] + 2
  rol                       ; [19] + 2
  and #%00000011            ; [21] + 2
  tax                       ; [23] + 2
  lda GameSpeechLo,X        ; [25] + 4
  sta JPTR                  ; [29] + 3
  lda #>JunoGameSpeech      ; [32] + 2
  sta JPTR+1                ; [34] + 3
  ; Fetch Speech Data
  lda (JPTR),Y              ; [37] + 5 = 42
  ; Copy Speech To AtariVox
WriteOutSpeech
  ; Invert and Store Speech Bits
  eor #$FF                  ; [0] + 2
  sta TEMP                  ; [2] + 3
  ; Reset Byte Counter
  sec                       ; [5] + 2
  ldy #0                    ; [7] + 2 = 9 
ByteOutLoop0
  ; Copy Carry Into Bit 0 Of SWACNT
  lda SWACNT                ; [0] + 3
  and #$FE                  ; [3] + 2
  adc #0                    ; [5] + 2
  sta SWACNT                ; [7] + 3
  ; Check If Finished
  cpy #9                    ; [10] + 2
  beq ExitSpeech            ; [12] + 2/3
  iny                       ; [14] + 2
  ; Waste Cycles
  SLEEP 36                  ; [16] + 36
  ; Put Next Byte Into Carry
  lsr TEMP                  ; [52] + 5
  jmp ByteOutLoop0          ; [57] + 3 
                            ; TOTAL = (60*9) + 15 = 555 (7.3 SCANLINES)

WaveSpeech
  ; Check If Finished Setup Speech (bit 5 set = setup)
  cpy #32                   ; [5] + 2
  beq ReloadLength          ; [7] + 2/3
  bcs SetupSpeech           ; [9] + 2/3
NormalWaveSpeech
  ; Store Speech Counter
  sty SPEECH                ; [11] + 3
  ; Get Wave Number
  lda WAVE                  ; [14] + 3
  and #%00111111            ; [17] + 2
  tax                       ; [19] + 2
  jmp WaveNumberSpeech      ; [21] + 3
ReloadLength
  ; Get Wave Number
  lda WAVE                  ; [10] + 3
  and #%00111111            ; [13] + 2
  tax                       ; [15] + 2
  ; Set Length Of Speech
  ldy SpeechLength,X        ; [17] + 4
  sty SPEECH                ; [21] + 3
WaveNumberSpeech
  ; Make Pointer To Speech Data
  lda SpeechTableLo,X       ; [24] + 4
  sta JPTR                  ; [28] + 3
  lda SpeechTableHi,X       ; [31] + 4
  sta JPTR+1                ; [35] + 3
  ; Fetch Speech Data
  lda (JPTR),Y              ; [38] + 5
  jmp WriteOutSpeech        ; [43] + 3 = 46
SetupSpeech
  ; Store Speech Counter
  tya                       ; [12] + 2
  sta SPEECH                ; [14] + 3
  ; Mask Bit 5 To Get Pointer
  and #%00011111            ; [17] + 2
  tay                       ; [19] + 2
  ; Calculate Setup Based on Wave
  lda WAVE                  ; [21] + 3
  and #%00011110            ; [24] + 2
  lsr                       ; [26] + 2
  tax                       ; [28] + 2
  ; Get Setup Pointer
  lda SetupSpeechLo,X       ; [30] + 4
  sta JPTR                  ; [34] + 3
  lda #>JunoSetupSpeech     ; [37] + 2
  sta JPTR+1                ; [39] + 3
  ; Fetch Speech Data
  lda (JPTR),Y              ; [42] + 5
  jmp WriteOutSpeech        ; [47] + 3 = 50

; -----------------------------------------------------------------------------
; PART 2 - MUSIC PLAYER (player adapted from Paul Slocum's Music Kit 2.0)
; -----------------------------------------------------------------------------

  ALIGN 256

  ; Music Player
Player
  ; Handle Tempo
  inc TEMPOCOUNT
  lda TEMPOCOUNT
  eor #TEMPODELAY
  bne QuitTempo
  sta TEMPOCOUNT

  ; Handle Beat
  inc BEAT
  lda BEAT
  eor #32
  bne QuitTempo
  sta BEAT
  ; Increment Song Pointer
  inc MEASURE
QuitTempo

  ; Fetch Song Data (Channel 0)
  ldx #0
  ldy MEASURE
  lda song1,Y

  ; Check If End Of Song
  cmp #255
  bne ContinueSong

  ; Go Reset to Beginning
  stx MEASURE
  lda song1,X
ContinueSong

  ; Play Pattern
  jsr PlayPattern

  ; Fetch Song Data (Channel 1)
  ldy MEASURE
  lda song2,Y

  ; Set Channel
  ldx #1
  jsr PlayPattern
  jmp ExitMusic
  
PlayPattern
  ; Save Channel Number
  stx CHAN

  ; Save Pattern Data
  sta TEMP16L

  ; Extract patternArray Offset
  asl
  asl
  asl
  sta TEMP16H

  ; Use Beat To Determine Extra Offset Within patternArray
  lda BEAT
  and #%00011000
  lsr
  lsr

  ; Add Original Offset
  adc TEMP16H
  tax

  ; Check 32
  lda TEMP16L
  and #%00100000
  bne PlayPattern32

PlayPattern0
  ; Pattern offsets greater than 128 read from a different array and play lower
  lda TEMP16L
  bmi LowPattern0

  ; Loud Version
  ; Get Pattern Address
  lda patternArrayH,X
  ldy patternArrayH+1,X

  ; Set 0 Attenuation
  ldx #0
  beq EndGetPattern

LowPattern0
  ; Soft Version
  ; Get Pattern Address
  lda patternArrayL,X
  ldy patternArrayL+1,X

  ; Set -6 Attenuation
  ldx #2
  jmp EndGetPattern
    
PlayPattern32
  lda TEMP16L
  bmi LowPattern32

  ; Loud version
  ; Get Pattern Address
  lda patternArrayH+256,X
  ldy patternArrayH+257,X

  ; Set 0 Attenuation
  ldx #0
  beq EndGetPattern

LowPattern32
  ; Soft Version
  ; Get address of selected pattern
  lda patternArrayL,X
  ldy patternArrayL+1,X

  ; Set -6 Attenuation
  ldx #4
  ; jmp endGetPattern

EndGetPattern
  sta TEMP16L
  sty TEMP16H
  stx ATTEN

  ; BEAT contains the 32nd note that the beat is currently on
  lda BEAT

  ; Modification for 1 Quarter Per Measure (Thrust)
  and #%00000111
  tay

  ; Get sound/note data
  lda (TEMP16L),Y
  eor #255
  beq MuteNote
  eor #255

; Each byte of pattern data contains the frequency and
; sound type data.  This function separates and decodes them.
; The encoding is: the 3 high bits contain the encoded sound
; type and the lower 5 bits contain the freq data.
; - ACC must contain pattern byte
; = ACC will return the freq
; = X will return the sound type

  ; Extract Frequency
  tax
  and #%00011111
  sta FREQ
  
  ; Extract Sound Type
  txa
  lsr
  lsr
  lsr
  lsr
  lsr
  tax

  ; Sound Type Attenuation
  lda ATTEN
  clc
  adc soundTurnArray,X
  sta ATTEN

  ; Get Sound Type
  lda soundTypeArray,X

  ; Get Channel
  ldx CHAN

  ; Play Sound!
  sta AUDC0,X
  lda FREQ
  sta AUDF0,X

  ; Restore Beat & #%111
  tya
  tax

; Each set of pattern data is followed by 4 accept bytes.
; Each bit in order represents the accent (on or off)
; of its corresponding 32nd note.  This function
; returns the attenuation of a note in a pattern.
; - TEMP16 must contain an indirect pointer to the pattern data
; - X must contain BEAT && %00000111
; = will return the volume in ACC

  ; Accent offset is always 8 for Thrust mod
  ldy #8

  lda (TEMP16L),Y
  and BitMaskArray,X
  beq NoAccent

  ; It's an Accent, so don't attenuate
  lda #15

NoAccent
  ; No accent, so use a lower volume
  ora #13
  sbc ATTEN
  
MuteNote
  ldy CHAN
  sta AUDV0,Y

; Plays high hat sound on the first frame of each beat indicated in hatPattern
  ldy CHAN
  beq NoHat

  ; Repeat high hat pattern
  lda MEASURE
  cmp #HATSTART
  bmi NoHat
  lda BEAT
  and #%00000111
  tax
  lda BEAT
  lsr
  lsr
  lsr
  tay
  lda hatPattern,Y
  and BitMaskArray,X
  beq NoHat

  ; Only Play On First Frame
  lda TEMPOCOUNT
  bne NoHat

  ; Play High Hat
  lda #HATPITCH
  sta AUDF1
  lda #HATSOUND
  sta AUDC1
  lda #HATVOLUME
  sta AUDV1
NoHat

  ; End Player
  rts
  
; -----------------------------------------------------------------------------
; PART 3 - SPEECH DATA
; -----------------------------------------------------------------------------

  ALIGN 256

  ; Wave Setup
JunoSetupSpeech

  ; Setup Speech Pointers
SetupSpeechLo
  DC.B  <SetupSpeech0, <SetupSpeech1, <SetupSpeech2, <SetupSpeech3
  DC.B  <SetupSpeech4, <SetupSpeech5, <SetupSpeech6, <SetupSpeech7
  DC.B  <SetupSpeech8, <SetupSpeech9, <SetupSpeech10, <SetupSpeech11
  DC.B  <SetupSpeech12, <SetupSpeech13, <SetupSpeech14, <SetupSpeech15
  
  ; Setup Speech
SetupSpeech0
  DC.B  0, 0, 166, 154, 147
  DC.B  4, 23, 64, 22, 118, 21, 96, 20      ; 12 Length
SetupSpeech1
  DC.B  0, 0, 166, 154, 147
  DC.B  4, 23, 72, 22, 118, 21, 96, 20      ; 12 Length
SetupSpeech2
  DC.B  0, 0, 166, 154, 147
  DC.B  4, 23, 80, 22, 118, 21, 96, 20      ; 12 Length
SetupSpeech3
  DC.B  0, 0, 166, 154, 147
  DC.B  4, 23, 88, 22, 118, 21, 96, 20      ; 12 Length
SetupSpeech4
  DC.B  0, 0, 166, 154, 147
  DC.B  4, 23, 96, 22, 118, 21, 96, 20      ; 12 Length
SetupSpeech5
  DC.B  0, 0, 166, 154, 147
  DC.B  4, 23, 104, 22, 118, 21, 96, 20     ; 12 Length
SetupSpeech6
  DC.B  0, 0, 166, 154, 147
  DC.B  4, 23, 112, 22, 118, 21, 96, 20     ; 12 Length
SetupSpeech7
  DC.B  0, 0, 166, 154, 147
  DC.B  4, 23, 120, 22, 118, 21, 96, 20     ; 12 Length
SetupSpeech8
  DC.B  0, 0, 166, 154, 147
  DC.B  4, 23, 128, 22, 118, 21, 96, 20     ; 12 Length
SetupSpeech9
  DC.B  0, 0, 166, 154, 147
  DC.B  4, 23, 136, 22, 118, 21, 96, 20     ; 12 Length
SetupSpeech10
  DC.B  0, 0, 166, 154, 147
  DC.B  4, 23, 144, 22, 118, 21, 96, 20     ; 12 Length
SetupSpeech11
  DC.B  0, 0, 166, 154, 147
  DC.B  4, 23, 152, 22, 118, 21, 96, 20     ; 12 Length
SetupSpeech12
  DC.B  0, 0, 166, 154, 147
  DC.B  4, 23, 160, 22, 118, 21, 96, 20     ; 12 Length
SetupSpeech13
  DC.B  0, 0, 166, 154, 147
  DC.B  4, 23, 168, 22, 118, 21, 96, 20     ; 12 Length
SetupSpeech14
  DC.B  0, 0, 166, 154, 147
  DC.B  4, 23, 176, 22, 118, 21, 96, 20     ; 12 Length
SetupSpeech15
  DC.B  0, 0, 166, 154, 147
  DC.B  4, 23, 184, 22, 118, 21, 96, 20     ; 12 Length
EndJunoSetupSpeech
  if (>JunoSetupSpeech != >EndJunoSetupSpeech)
    echo "WARNING: Setup Speech Crosses Page Boundary!"
  endif

  ; Speech Data
  SPEECH1
  SPEECH2
  SPEECH3

  ; Non-Wave Speech
JunoGameSpeech

  ; Speech Pointers
GameSpeechLo
  DC.B  0, <Speech1, <Speech2, <Speech3

  ; Hyperspace
Speech1
  DC.B  187, 50, 21, 128, 70, 22, 128, 80
  DC.B  22, 128, 90, 22, 127, 21, 154, 198
  DC.B  187, 100, 22, 148, 148, 199, 15, 150
  DC.B  22, 154, 135, 184, 5, 23, 127, 20
  DC.B  200, 22, 110, 21
  DC.B  5, 23, 80, 22, 114, 21, 122, 20     ; 43 Length

  ; Bonus Ship
Speech2
  DC.B  2, 198, 129, 189, 150, 22, 110, 21
  DC.B  188, 8, 134, 7, 141, 105, 22, 164
  DC.B  171, 5, 23, 127, 20, 120, 21, 85, 22
  DC.B  5, 23, 80, 22, 114, 21, 122, 20     ; 32 Length

  ; Fuel Low
Speech3
  DC.B  164, 146, 100, 22, 100, 21, 145, 160
  DC.B  186, 114, 21, 120, 22, 5, 23, 127, 20
  DC.B  5, 23, 88, 22, 114, 21, 127, 20     ; 24 Length

EndJunoGameSpeech
  if (>JunoGameSpeech != >EndJunoGameSpeech)
    echo "WARNING: Game Speech Crosses Page Boundary!"
  endif

  ALIGN 256

  ; Include Wave Speech Pointers
  SPEECHTABLE

; -----------------------------------------------------------------------------
; PART 3 - MUSIC DATA
; -----------------------------------------------------------------------------

  ALIGN 256

  ; Include Song Data
  INCLUDE "song.h"

  ; Mask Table (used by Song Player)
BitMaskArray
  DC.B  #%10000000
  DC.B  #%01000000
  DC.B  #%00100000
  DC.B  #%00010000
  DC.B  #%00001000
  DC.B  #%00000100
  DC.B  #%00000010
  DC.B  #%00000001
EndBitMaskArray
  if (>BitMaskArray != >EndBitMaskArray)
    echo "WARNING: Bit Mask Array Crosses Page Boundary!"
  endif

  echo "----",($FFF0 - *) , "bytes left (BANK 7 - MUSIC)"
  
  ORG     $EFF0
  RORG    $FFF0
  DC.B    0, 0, 0, 255
  DC.B    0, 0, 0, 0, 0, 0
  DC.W    (PlusAPI - $E000)
  DC.W    Init7, Init7

