;
; Lunokhod 1
;
; Screen Layout:
;
;     Lines     Cycles      Description
;     -----------------------------------
;     40        3040        VSYNC/VBLANK
;     10        760         Score
;     32        2432        Sky
;     130       9880        Playfield
;     20        1520        Radar
;     30        2280        Overscan

    PROCESSOR 6502
    include "vcs.h"
    include "macro.h"

PLUSROM     = 1
PAL         = 1

    SEG
    ORG $F000 ; set the ROM page

; constants

  IF PLUSROM == 1
WriteToBuffer           = $1FF0
WriteSendBuffer         = $1FF1
ReceiveBuffer           = $1FF2
ReceiveBufferSize       = $1FF3

HIGHSCORE_ID            = 89         ; Lunokhod 1 game ID in Highscore DB
  ENDIF

    IF PAL
CALOR_00 = $00
CALOR_02 = $02
CALOR_04 = $04
CALOR_06 = $06
CALOR_08 = $08
CALOR_0A = $0A
CALOR_0C = $0C
CALOR_0E = $0E
CALOR_10 = $20
CALOR_12 = $22
CALOR_14 = $24
CALOR_16 = $26
CALOR_18 = $28
CALOR_1A = $2A
CALOR_1C = $2C
CALOR_1E = $2E
CALOR_20 = $40
CALOR_22 = $42
CALOR_24 = $44
CALOR_26 = $46
CALOR_28 = $48
CALOR_2A = $4A
CALOR_2C = $4C
CALOR_2E = $4E
CALOR_30 = $40
CALOR_32 = $42
CALOR_34 = $44
CALOR_36 = $46
CALOR_38 = $48
CALOR_3A = $4A
CALOR_3C = $4C
CALOR_3E = $4E
CALOR_40 = $60
CALOR_42 = $62
CALOR_44 = $64
CALOR_46 = $66
CALOR_48 = $68
CALOR_4A = $6A
CALOR_4C = $6C
CALOR_4E = $6E
CALOR_50 = $80
CALOR_52 = $82
CALOR_54 = $84
CALOR_56 = $86
CALOR_58 = $88
CALOR_5A = $8A
CALOR_5C = $8C
CALOR_5E = $8E
CALOR_60 = $A0
CALOR_62 = $A2
CALOR_64 = $A4
CALOR_66 = $A6
CALOR_68 = $A8
CALOR_6A = $AA
CALOR_6C = $AC
CALOR_6E = $AE
CALOR_70 = $C0
CALOR_72 = $C2
CALOR_74 = $C4
CALOR_76 = $C6
CALOR_78 = $C8
CALOR_7A = $CA
CALOR_7C = $CC
CALOR_7E = $CE
CALOR_80 = $D0
CALOR_82 = $D2
CALOR_84 = $D4
CALOR_86 = $D6
CALOR_88 = $D8
CALOR_8A = $DA
CALOR_8C = $DC
CALOR_8E = $DE
CALOR_90 = $B0
CALOR_92 = $B2
CALOR_94 = $B4
CALOR_96 = $B6
CALOR_98 = $B8
CALOR_9A = $BA
CALOR_9C = $BC
CALOR_9E = $BE
CALOR_A0 = $90
CALOR_A2 = $92
CALOR_A4 = $94
CALOR_A6 = $96
CALOR_A8 = $98
CALOR_AA = $9A
CALOR_AC = $9C
CALOR_AE = $9E
CALOR_B0 = $70
CALOR_B2 = $72
CALOR_B4 = $74
CALOR_B6 = $76
CALOR_B8 = $78
CALOR_BA = $7A
CALOR_BC = $7C
CALOR_BE = $7E
CALOR_C0 = $50
CALOR_C2 = $52
CALOR_C4 = $54
CALOR_C6 = $56
CALOR_C8 = $58
CALOR_CA = $5A
CALOR_CC = $5C
CALOR_CE = $5E
CALOR_D0 = $30
CALOR_D2 = $32
CALOR_D4 = $34
CALOR_D6 = $36
CALOR_D8 = $38
CALOR_DA = $3A
CALOR_DC = $3C
CALOR_DE = $3E
CALOR_E0 = $20
CALOR_E2 = $22
CALOR_E4 = $24
CALOR_E6 = $26
CALOR_E8 = $28
CALOR_EA = $2A
CALOR_EC = $2C
CALOR_EE = $2E
CALOR_F0 = $40
CALOR_F2 = $42
CALOR_F4 = $44
CALOR_F6 = $46
CALOR_F8 = $48
CALOR_FA = $4A
CALOR_FC = $4C
CALOR_FE = $4E

    ELSE
CALOR_00 = $00
CALOR_02 = $02
CALOR_04 = $04
CALOR_06 = $06
CALOR_08 = $08
CALOR_0A = $0A
CALOR_0C = $0C
CALOR_0E = $0E
CALOR_10 = $10
CALOR_12 = $12
CALOR_14 = $14
CALOR_16 = $16
CALOR_18 = $18
CALOR_1A = $1A
CALOR_1C = $1C
CALOR_1E = $1E
CALOR_20 = $20
CALOR_22 = $22
CALOR_24 = $24
CALOR_26 = $26
CALOR_28 = $28
CALOR_2A = $2A
CALOR_2C = $2C
CALOR_2E = $2E
CALOR_30 = $30
CALOR_32 = $32
CALOR_34 = $34
CALOR_36 = $36
CALOR_38 = $38
CALOR_3A = $3A
CALOR_3C = $3C
CALOR_3E = $3E
CALOR_40 = $40
CALOR_42 = $42
CALOR_44 = $44
CALOR_46 = $46
CALOR_48 = $48
CALOR_4A = $4A
CALOR_4C = $4C
CALOR_4E = $4E
CALOR_50 = $50
CALOR_52 = $52
CALOR_54 = $54
CALOR_56 = $56
CALOR_58 = $58
CALOR_5A = $5A
CALOR_5C = $5C
CALOR_5E = $5E
CALOR_60 = $60
CALOR_62 = $62
CALOR_64 = $64
CALOR_66 = $66
CALOR_68 = $68
CALOR_6A = $6A
CALOR_6C = $6C
CALOR_6E = $6E
CALOR_70 = $70
CALOR_72 = $72
CALOR_74 = $74
CALOR_76 = $76
CALOR_78 = $78
CALOR_7A = $7A
CALOR_7C = $7C
CALOR_7E = $7E
CALOR_80 = $80
CALOR_82 = $82
CALOR_84 = $84
CALOR_86 = $86
CALOR_88 = $88
CALOR_8A = $8A
CALOR_8C = $8C
CALOR_8E = $8E
CALOR_90 = $90
CALOR_92 = $92
CALOR_94 = $94
CALOR_96 = $96
CALOR_98 = $98
CALOR_9A = $9A
CALOR_9C = $9C
CALOR_9E = $9E
CALOR_A0 = $A0
CALOR_A2 = $A2
CALOR_A4 = $A4
CALOR_A6 = $A6
CALOR_A8 = $A8
CALOR_AA = $AA
CALOR_AC = $AC
CALOR_AE = $AE
CALOR_B0 = $B0
CALOR_B2 = $B2
CALOR_B4 = $B4
CALOR_B6 = $B6
CALOR_B8 = $B8
CALOR_BA = $BA
CALOR_BC = $BC
CALOR_BE = $BE
CALOR_C0 = $C0
CALOR_C2 = $C2
CALOR_C4 = $C4
CALOR_C6 = $C6
CALOR_C8 = $C8
CALOR_CA = $CA
CALOR_CC = $CC
CALOR_CE = $CE
CALOR_D0 = $D0
CALOR_D2 = $D2
CALOR_D4 = $D4
CALOR_D6 = $D6
CALOR_D8 = $D8
CALOR_DA = $DA
CALOR_DC = $DC
CALOR_DE = $DE
CALOR_E0 = $E0
CALOR_E2 = $E2
CALOR_E4 = $E4
CALOR_E6 = $E6
CALOR_E8 = $E8
CALOR_EA = $EA
CALOR_EC = $EC
CALOR_EE = $EE
CALOR_F0 = $F0
CALOR_F2 = $F2
CALOR_F4 = $F4
CALOR_F6 = $F6
CALOR_F8 = $F8
CALOR_FA = $FA
CALOR_FC = $FC
CALOR_FE = $FE

    ENDIF


MODE_ATTRACT            equ #0
MODE_RESET              equ #1
MODE_RESET_DOWN         equ #2
MODE_GAME_OVER          equ #4
MODE_GAME_ENDED         equ #5
MODE_START              equ #6
MODE_CRASH              equ #129
MODE_IN_PROGRESS        equ #130
MODE_END_WAVE           equ #131
MODE_TRANSITION         equ #132

DEBRIS_HIDDEN           equ #160
MISSILE_FOLLOWS_SHIP    equ #0      ;
NEW_MOVE_DEBRIS         equ #0
MAXSPEED                equ #2    ; the max speed of the sled
MINSPEED                equ #-2    ; the min speed of the sled
MINXPOS                 equ #76-30  ; minimum screen positionof the sled (32)
MAXXPOS                 equ #76+30  ; maximum screen position of the sled  (112)
POSDELTA                equ #MAXXPOS-MINXPOS        ; this must be equal to maxpos-minpos (80)
P0CENTER                equ #4      ; offset to center of player 0
MISSILESPEED            equ #6
MISSILESIZE             equ #10
SHOT_TIMING             equ #6
MISSILECOLOR            equ #CALOR_0E
;MINXSCREEN              equ #8
;MAXXSCREEN              equ #154
MINXSCREEN              equ #1
MAXXSCREEN              equ #160
GAUGECOLOR              equ #CALOR_32
SCORECOLOR              equ #CALOR_0E
SCOREBGCOLOR            equ #CALOR_00
SKYCOLOR                equ #CALOR_00
MTNCOLOR                equ #CALOR_04
MTNCAPSCOLOR            equ #CALOR_04
GRADIENT1               equ #$04
GRADIENT2               equ #$04
GRADIENT3               equ #$02
RADARCOLOR              equ #CALOR_14
RADARCOLOR2             equ #CALOR_C8
GROUNDCOLOR             equ #CALOR_00
SNOWCOLOR               equ #CALOR_06
RAILCOLOR               equ #CALOR_02
PLATFORMCOLOR           equ #CALOR_04
PLAYFIELDSZ             equ #140
ENEMYHEIGHT             equ #17
SPRITEINIT              equ #PLAYFIELDSZ+ENEMYHEIGHT+1
ME_VOL0                 equ #0      ; music engine volume event
ME_PITCH0               equ #255      ; music engine pitch event
ME_TONE0                equ #1      ; music engine tone event
ME_END                  equ #2      ; music engine end event
SONGTONE                equ #6

; variable assignments.

score           equ  $80    ; 80-81
p0x             equ  $82    ; screen x position of player 0 [20,132]
p0s             equ  $83    ; x speed of player 0 [-4,4]
p0vxlo          equ  $84    ; player 0 virtual x position low byte
temp1lo         equ  $85    ; temporary 16 bit var
temp1hi         equ  $86    ; temporary 16 bit var
temp2lo         equ  $87    ; temporary 16 bit var
temp2hi         equ  $88    ; temporary 16 bit var
randomSeed      equ  $89    ;
direction       equ  $8a    ;
songPos         equ  $8b    ; points to the current song position
hitRatio        equ  $8c    ;
waveCounter     equ  $8d    ; number of debris created in the current wave
nexty           equ  $8e    ;
m0vxlo          equ  $8f    ; virtual x position of missile 0
m0ys            equ  $90    ; y position of missile 0
m1vxlo          equ  $91    ; virtual x position of missile 1
m1ys            equ  $92    ; y position of missile 1
m2vxlo          equ  $93    ; virtual x position of missile 2
m2ys            equ  $94    ; y position of missile 2
m3vxlo          equ  $95    ; virtual x position of missile 3
m3ys            equ  $96    ; y position of missile 3
debrisvx        equ  $97    ; 97-9E virtual x position of debris
debrisinfo      equ  $9f    ; 9F-A6 ; info about debris aaxxxyyy
                            ; (aa = movespeed xxx = debris index,
                            ; yyy = condemned)
debrisx         equ  $a7    ; A7-AE screen x position of debris
debrisy         equ  $af    ; AF-B6 screen y position of debris
frameCount      equ  $b7    ; count the current frame, rolls over at 255
debrisCount     equ  $b8    ; number of current debris
scrollpos       equ  $b9    ; scroll position on the virtual playfield
p1dataptrlo     equ  $ba    ;
p1dataptrhi     equ  $bb    ;
wave            equ  $bc    ; 0-31, incremented by one. The first five
                            ; bits of the wave are used like this:
                            ;    bit 0 & 1    : horizontal speed
                            ;    bit 2 & 3    : vertical speed
                            ;    bit 5        : pace
p1colorlo       equ  $bd    ;
p1colorhi       equ  $be    ;
UNUSED2         equ  $bf    ;
tempIdx         equ  $c0    ; C0-C7 pointers into spriteram
tempIdx0        equ  $c0    ; C0-C7 pointers into spriteram
tempIdx1        equ  $c1    ; C0-C7 pointers into spriteram
tempIdx2        equ  $c2    ; C0-C7 pointers into spriteram
tempIdx3        equ  $c3    ; C0-C7 pointers into spriteram
tempIdx4        equ  $c4    ; C0-C7 pointers into spriteram
tempIdx5        equ  $c5    ; C0-C7 pointers into spriteram
tempIdx6        equ  $c6    ; C0-C7 pointers into spriteram
tempIdx7        equ  $c7    ; C0-C7 pointers into spriteram

spriteram       equ  $c8    ; C8-D7 16 bytes of ram organized in 2 byte
                            ; chunks, each chunk containing display
                            ; data for a given debris sprite. this is
                            ; loaded with the current animation frames

effectState    equ  $D8     ; holds state information for current
                            ; audio/video effect: XXXYYYYY, where XXX
                            ; is the event type and YYYYY is the event
                            ; duration countdown.
shotcount       equ  $D9    ;
gameMode        equ  $DA    ; see MODES_* below for possible values
radlevel        equ  $DB    ;
musicEngineCount equ  $DC   ;
vxoffsetlo      equ  $DD    ;
radarRam        equ  $DE    ; DE-FD 16 bytes to store radar display data
temp3           equ  $FE

; sysinit
;
; Initialize system (Andrew Davie's 8 byte init)

    echo "------", [*], [* - $F000]d, "sysinit"

sysinit:
    ldx #0
    txa
sysinit_clear
    dex

    txs
    pha
    bne sysinit_clear
    jsr non_system_init
    jmp main_loop
sysinit_end

; non_system_init
;
; Non-system initialization

    echo "------", [*], [* - $F000]d, "non_system_init"

non_system_init:

    lda #0                          ; zero out memory locations $80,$FD
    ldx #$7c                        ;
clear_loop                          ;
    sta #$80,x                      ;
    dex                             ;
    cpx #2                          ;
    bne clear_loop                  ;

    lda #$FF                        ; set initial missile y positions
    sta m0ys                        ; $FF means hidden
    sta m1ys                        ;
    sta m2ys                        ;
    sta m3ys                        ;

    lda #<sprite1Frame1             ; initialize the player sprite
    sta p1dataptrlo                 ;
    lda #>sprite1Frame1             ;
    sta p1dataptrhi                 ;

    lda #$4d                        ; initialize initial player screen
    sta p0x                         ; and virtual positions
    lda #77                         ;
    sta p0vxlo                      ;

    lda #32
    sta songPos

    lda #1
    sta musicEngineCount

    lda #MODE_ATTRACT
    sta gameMode

    rts
non_system_init_end

; main_loop:

    echo "------", [*], [* - $F000]d, "main_loop"

main_loop:

;
; Start the vertical blank interrupt
; TIME: ~4 scanlines
; ENDS: 8
;

    echo "------", [*], [* - $F000]d, "vblank_start"

vblank_start:
    lda #2                          ; wait for horizontal sync
    sta  WSYNC                      ; ""
    sta  VSYNC                      ; turn on vsync
    sta  WSYNC                      ; wait three lines
    sta  WSYNC                      ; ""
    lda #0                          ; ""
    sta  WSYNC                      ; ""
    sta  VSYNC                      ; turn off vsync
    lda  #45                        ; Set the timer for 2812 cycles
    sta  TIM64T                     ; ""
vblank_start_end


    inc frameCount      ; 5, 13

; do_audio
;
; perform audio processing

    echo "------", [*], [* - $F000]d, "do_audio"

do_audio:                   ; 35

    lda shotcount           ; 3     ; if there are no active shots skip
    beq skip_shot_audio     ; 2/3   ;

    dec shotcount           ; 5     ; decriment the shot counter.

    lda effectState                 ; skip shot audio if there are other
    cmp #0                          ; noises to be made
    bne skip_shot_audio             ;

    lda shotcount                   ; adjust the shot sound parameters
    adc #1                          ;
    sta AUDV1                       ;
    lda #15                 ; 2     ;
    sec                     ; 2     ;
    sbc shotcount           ; 2     ;
    clc                     ; 2     ;
    adc #24                 ; 2     ; tweak to alter timbre of shot
    sta AUDF1               ; 3     ;
    lda #8                  ; 2     ; tweak to alter shot distortion
    sta AUDC1               ; 3     ;
    jmp do_audio_end

skip_shot_audio

    lda effectState                 ; if the effect counter is > 0 set
    and #%00011111                  ; the volume for this effect, skip
    cmp #0                          ; otherwise
    bne skipAudioEnd                ;
    sta AUDV1                       ;
    sta effectState                 ;
    jmp do_audio_end

skipAudioEnd

    lsr                             ; set the volume and decriment the
    sta AUDV1                       ; counter
    dec effectState                 ;

    lda effectState                 ; jump to the correct effect handler
    cmp #%10000000                  ;
    bcs radioactive_effect          ;
    cmp #%01000000                  ;
    bcs absorber_effect             ;
    cmp #%00100000                  ;
    bcs bomb_effect                 ;
    lda #0                          ; no audio events, skip
    sta AUDV1                       ;
    jmp do_audio_end                ;

bomb_effect

    lda frameCount
    and #%00100110
    clc
    adc #%00110100
    sta AUDF1
    lda #15
    jmp noRandomAudc

absorber_effect

    lda frameCount
    lda effectState
    and #%00011111
    asl
    sta AUDF1
    lda #12
    jmp noRandomAudc

radioactive_effect

    lda frameCount
    and #%00100110
    clc
    adc #%10000100
    sta AUDF1
    lda #8

noRandomAudc
    sta AUDC1

do_audio_end

music_engine:

    ldy songPos
;    cmp #$FF
;    beq music_engine_end

    dec musicEngineCount    ; 5     ; decriment the counter, if it is 0 load the next event
    beq music_next_event    ; 2-4   ; ""
    jmp music_engine_end    ; 3, 22 ; nothing to do this go around.

music_next_event            ;       ; load the next event

    lda songLoopIntro,y             ; load the pitch, if it is 255 this
    cmp #255                        ; is a jump
    beq music_engine_jump           ;
    sta AUDF0                       ;

    iny
    lda songLoopIntro,y           ; load the tone
    sta AUDC0

    iny
    lda songLoopIntro,y            ; load the volume
    sta AUDV0

    iny
    lda songLoopIntro,y           ; load the duration


;; MULTIPLY
    ; adjust for tempo. multiply by delta then divide by 8.

    sta musicEngineCount
    cmp #1
    beq endMusicEngineDivide;

    ldx wave
    lda tempoTable,x
    sta temp3
    lda #0
    beq enterMultiplyLoop
doAdd
    clc
    adc temp3
theLoop
    asl temp3
enterMultiplyLoop
    lsr musicEngineCount
    bcs doAdd
    bne theLoop
end
    lsr
    lsr
    lsr
    sta musicEngineCount

endMusicEngineDivide
;
    iny
    sty songPos
    jmp music_engine_end

music_engine_jump

    iny
    lda songLoopIntro,y
    tay
    jmp music_next_event

music_engine_end


; process_gameMode
;
; detect game mode changes, take appropriate action

    echo "------", [*], [* - $F000]d, "process_gameMode"

process_gameMode:

    lda gameMode                    ; load gameMode, choose appropriate
    cmp #MODE_IN_PROGRESS           ;
    bne process_gameMode_2          ;
    jmp process_gameMode_end
process_gameMode_2
    cmp #MODE_START
    beq do_mode_start
    cmp #MODE_END_WAVE              ;
    beq do_end_wave                 ;
    cmp #MODE_RESET                 ; action
    beq do_reset                    ;
    cmp #MODE_GAME_OVER             ;
    beq do_game_over                ;
    cmp #MODE_GAME_ENDED            ;
    beq do_game_ended               ;
    cmp #MODE_TRANSITION            ;
    beq do_transition               ;
    cmp #MODE_CRASH                 ;
    beq do_crash                    ;
    jmp process_gameMode_end        ;

do_mode_start

    dec waveCounter
    bne process_gameMode_end
    lda #MODE_TRANSITION
    sta gameMode
    jmp process_gameMode_end

do_reset

    jsr non_system_init
    lda #MODE_START
    sta gameMode
    lda #1
    sta musicEngineCount
    lda #0
    sta score
    sta score+1
    sta songPos
    lda #32
    sta waveCounter
    jmp process_gameMode_end

do_game_over

    dec waveCounter
    bne process_gameMode_end
    lda #MODE_ATTRACT
    sta gameMode
    lda #32
    sta songPos
    lda #1
    sta musicEngineCount
    jmp process_gameMode_end

do_game_ended

  IF PLUSROM
    jsr SendPlusROMScore
  ELSE
    jsr non_system_init
  ENDIF
    lda #MODE_GAME_OVER
    sta gameMode
    lda #127
    sta waveCounter
    jmp process_gameMode_end

do_end_wave

   lda #MODE_TRANSITION
   sta gameMode
   jmp process_gameMode_end

do_transition

   dec waveCounter
   bne process_gameMode_end
   lda #MODE_IN_PROGRESS
   sta gameMode
   jmp process_gameMode_end

do_crash

   dec waveCounter
   bne process_gameMode_end
   lda #MODE_IN_PROGRESS
   sta gameMode
   jmp process_gameMode_end

process_gameMode_end


; adjust_missiles
;
; adjust missile y positions.

    echo "------", [*], [* - $F000]d, "adjust_missiles"

adjust_missiles:

    lda m0ys
    cmp #$FF
    beq adjust_1
    cmp #PLAYFIELDSZ
    bcs reset_missile_0
    clc
    adc #MISSILESPEED
    sta m0ys
    jmp adjust_1
reset_missile_0
    lda #$FF
    sta m0ys
adjust_1
    lda m1ys
    cmp #$FF
    beq adjust_2
    cmp #PLAYFIELDSZ
    bcs reset_missile_1
    clc
    adc #MISSILESPEED
    sta m1ys
    jmp adjust_2
reset_missile_1
    lda #$FF
    sta m1ys
adjust_2
    lda m2ys
    cmp #$FF
    beq adjust_3
    cmp #PLAYFIELDSZ
    bcs reset_missile_2
    clc
    adc #MISSILESPEED
    sta m2ys
    jmp adjust_3
reset_missile_2
    lda #$FF
    sta m2ys
adjust_3
    lda m3ys
    cmp #$FF
    beq adjust_missiles_end
    cmp #PLAYFIELDSZ
    bcs reset_missile_3
    clc
    adc #MISSILESPEED
    sta m3ys
    jmp adjust_missiles_end
reset_missile_3
    lda #$FF
    sta m3ys
adjust_missiles_end

    sta HMCLR           ; clear the ball init

; move_missiles
;
; move move missile sprites. there can be up to four visible missiles,
; two 'odd' and two 'even'. They are moved in alternating passes every
; other frame.

    echo "------", [*], [* - $F000]d, "move_missiles"

move_missiles:

    lda frameCount
    and #%0000001
;    cmp #0
    beq move_missiles_even
    jmp move_missiles_odd

move_missiles_even
    lda m0ys
    cmp #$FF
    beq move_missiles_even_1
    lda m0vxlo
;    sta temp2lo
    jsr convert_virtual_xpos
    clc
    adc #P0CENTER       ; add to align with center of ship
#if MISSILE_FOLLOWS_SHIP = 1
    lda p0x             ; missle follows ship
    clc                 ; ""
    adc #4              ; ""
#endif
    ldx #3
    jsr do_sprite_move
move_missiles_even_1
    lda m1ys
    cmp #$FF
    beq move_missiles_even_end
    lda m1vxlo
;    sta temp2lo
    jsr convert_virtual_xpos
    clc
    adc #P0CENTER       ; add to align with center of ship
#if MISSILE_FOLLOWS_SHIP = 1
    lda p0x             ; missle follows ship
    clc                 ; ""
    adc #4              ; ""
#endif
    ldx #2
    sta HMCLR
    jsr do_sprite_move
move_missiles_even_end
    jmp move_missiles_odd_end

move_missiles_odd
    lda m2ys
    cmp #$FF
    beq move_missiles_odd_3
    lda m2vxlo
;    sta temp2lo
    jsr convert_virtual_xpos
    clc
    adc #P0CENTER       ; add to align with center of ship
#if MISSILE_FOLLOWS_SHIP = 1
    lda p0x             ; missle follows ship
    clc                 ; ""
    adc #4              ; ""
#endif
    ldx #3
    jsr do_sprite_move
move_missiles_odd_3
    lda m3ys
    cmp #$FF
    beq move_missiles_odd_end
    lda m3vxlo
;    sta temp2lo
    jsr convert_virtual_xpos
    clc
    adc #P0CENTER       ; add to align with center of ship
#if MISSILE_FOLLOWS_SHIP = 1
    lda p0x             ; missle follows ship
    clc                 ; ""
    adc #4              ; ""
#endif
    ldx #2
    sta HMCLR
    jsr do_sprite_move
move_missiles_odd_end

move_missiles_end

; swap_missiles1
;
; We are keeping state for 4 missiles. Every other frame we
; map even or odd missiles to M0/M1.

    echo "------", [*], [* - $F000]d, "swap_missiles1"

swap_missiles1:             ; 32

    lda frameCount          ; 3
    and #%0000001           ; 2
    beq swap_missiles1_end  ; 3
    ldx m0ys                ; 3
    lda m2ys                ; 3
    sta m0ys                ; 3
    stx m2ys                ; 3
    ldx m1ys                ; 3
    lda m3ys                ; 3
    sta m1ys                ; 3
    stx m3ys                ; 3

swap_missiles1_end

    nop                 ; waste time before the HMCLR
    sta HMCLR


; remove_stale_debris
;
; Check the bottom debris moved and see if it needs to be removed,
; perform appropriate actions if we remove an absorber or radioactive
; elementl

    echo "------", [*], [* - $F000]d, "remove_stale_debris"

remove_stale_debris

    ldx debrisCount                 ; load the debris count into x
    cpx #0                          ; compare x to 0
    beq remove_stale_debris_end     ; if it = 0 then skip remove debris

    dex                             ; decriment x so that it equals
                                    ; the lowest debris index

    lda debrisy,x                   ; load the debris y position into a
    cmp #4                          ; compare a to 4
    bcs remove_stale_debris_end     ; if a >= 4 then skip remove debris
    dec debrisCount                 ; else decriment debrisCount


    lda debrisinfo,x                ; if the debris is radioactive and
    and #%00011111                  ; it is not condemned (011, 111) we
    cmp #%00011000                  ; trigger an effect and update the
    bne check_for_absorber          ; radLevel (else skip to absorber
                                    ; check

    lda #%10010111                  ; play radioactiveaudio
    sta effectState                 ;

    lda radlevel                    ; update radlevel
    lsr                             ;
    clc                             ;
    adc #128                        ;
    sta radlevel                    ;

    cmp #$FF                        ; if radLevel has reached maximum
    bne remove_stale_debris_end     ; then the game is over, set
    lda #MODE_GAME_ENDED            ; gameMode to MODE_GAME_OVER and the
    sta gameMode                    ; debrisCount to 0
    lda #0                          ;
    sta debrisCount                 ;

    jmp remove_stale_debris_end

check_for_absorber


remove_stale_debris_end

; move_debris
;
; Move the debris according to the current speed and hDirectionTable entries.

    echo "------", [*], [* - $F000]d, "move_debris"

move_debris:

    lda debrisCount               ; 3 ; if there is no debris, skip section
    cmp #0                        ; 2
    bne no_move_debris_end        ; 2
    jmp move_debris_end           ; 3

no_move_debris_end

    ldy wave                        ; use vSpeedTable and frameCount
    lda vSpeedTable,y               ; to calculate the speed of vertical
    sta tempIdx0                    ; movement for this frame.
    lda frameCount                  ; ...
    and #%00000011                  ; ...
    tay                             ; ...
    lda tempIdx0                    ; ...
shiftLoop1                  ; +1    ; ...
    cpy #0                  ; 2     ; ...
    beq doneShiftLoop1      ; 2     ; ...
    dey                     ; 2     ; ...
    lsr                     ; 2     ; ...
    lsr                     ; 2     ; ...
    jmp shiftLoop1          ; 3     ; ...
doneShiftLoop1              ; +1    ; ...
    and #%00000011          ; 2     ; ...
    sta tempIdx0                    ; ...

    ldy wave                        ; use hSpeedTable and frameCount
    lda hSpeedTable,y               ; to calculate the speed of horiz
    sta tempIdx1                    ; movement for this frame.
    lda frameCount                  ; ...
    and #%00000011                  ; ...
    tay                             ; ...
    lda tempIdx1                    ; ...
shiftLoop2                  ; +1    ; ...
    cpy #0                  ; 2     ; ...
    beq doneShiftLoop2      ; 2     ; ...
    dey                     ; 2     ; ...
    lsr                     ; 2     ; ...
    lsr                     ; 2     ; ...
    jmp shiftLoop2          ; 3     ; ...
doneShiftLoop2              ; +1    ; ...
    and #%00000011          ; 2     ; ...
    sta tempIdx1                    ; ...

    ldx #0                  ; 2     ; initialize the loop counter

move_debris_loop


; begin_horizontal_move
;
; move debris left or right. Degree of movement is detrermined by
; settings in the hDirectionTable and by the current wave.

begin_horizontal_move

    lda debrisinfo,x
    lsr
    lsr
    lsr
    tay
    lda hDirectionTable,y
    beq end_horizontal_move         ; this is not a mover
    cmp #1
    beq skip_left_move

    lda debrisvx,x
    sec
    sbc tempIdx1
    sta debrisvx,x
    jmp end_horizontal_move

skip_left_move

    lda debrisvx,x
    clc
    adc tempIdx1
    sta debrisvx,x

end_horizontal_move

; begin_vertical_move
;

begin_vertial_move

    lda debrisy,x
    sec
    sbc tempIdx0
    sta debrisy,x

end_vertical_move


    lda debrisinfo,x                ; check to see if debris is fully
    and #%00000111                  ; condemned, if not we go ahead with
    cmp #%00000111                  ; virtual position conversion
    bne continue_with_convert       ;

    lda #DEBRIS_HIDDEN                        ; debris is condemned, skip further
    sta debrisx,x                   ; debris processing
    jmp move_debris_skip            ;

continue_with_convert               ; debris not fully condemned,
    lda debrisvx,x                  ; convert virtual x position of the
    jsr convert_virtual_xpos        ; debris to a screen position
    sta debrisx,x                   ;
set_to_ff
;    cmp #$FF                        ; if convert_virtual_xpos returns
;    beq move_debris_skip            ; FF, the sprite is off screen, skip

    stx temp3                       ; store x, our debris index, so we
                                    ; can use x to look up details
                                    ; in the sprite table


    lda debrisinfo,x                ; increment the condemned counter
    and #%00000111                  ; if the counter is non-zero.
    beq spriteselect                ;
    sta temp1lo                     ;
    inc temp1lo                     ;
    lda debrisinfo,x                ;
    and #%11111000                  ;
    ora temp1lo                     ;
    sta debrisinfo,x                ;

    lda #<explosionFrameTable       ; Load the memory location of the
    sta temp2lo                     ; explosion data for use in
    lda #>explosionFrameTable       ; 'spritemove'
    sta temp2hi                     ;
    jmp spritemove            ;

spriteselect

    lda debrisinfo,x                ; load the correct sprite. we are
    and #%00111000                  ; looking up the memory location for
    lsr                             ; this sprites data in spriteTable
    lsr                             ; and storing it in temp2lo/temp2hi
    tax                             ;
    lda spriteTable,x               ;
    sta temp2lo                     ;
    lda spriteTable+1,x             ;
    sta temp2hi                     ;

spritemove

    ldx temp3
    lda frameCount      ; 3         ; use framecount to choose which
    and #%00001100      ; 2         ; sprite animation frame to show.
    lsr                 ; 2         ; we use the memory location grabbed
    tay                             ; spritetable in "spriteselect"
    lda (temp2lo),y                 ; above to look up the sprite frame
    sta spriteram,x                 ; data.
    iny                             ;
    lda (temp2lo),y                 ;
    sta spriteram+8,x                     ;

move_debris_skip

    inx                             ; increment x and tee up the next
    cpx debrisCount                 ; debris index, loop if more debris
    beq move_debris_end             ; left to move.
    jmp move_debris_loop

move_debris_end

    echo "------", [*], [* - $F000]d, "load_score"

load_wave:

    lda gameMode
    cmp #MODE_TRANSITION
    bne load_score

    lda wave            ; load half the player score
    and #$0F            ; and out the low nibble
    tax
    lda scoreTable,x    ;
    sta tempIdx    ;
    lda scoreTable+10,x ;
    sta tempIdx+1  ;

    lda wave           ; load half the player score
    lsr                 ; extract the high nibble
    lsr
    lsr
    lsr
    tax
    lda scoreTable,x    ;
    sta tempIdx+2      ;
    lda scoreTable+10,x  ;
    sta tempIdx+3    ;

    lda #<scoreA
    sta tempIdx+4    ;
    lda #>scoreA
    sta tempIdx+5  ;

    lda #<scoreW
    sta tempIdx+6    ;
    lda #>scoreW
    sta tempIdx+7  ;

    jmp init_first_sprite


; load_score
;
; load the score pointers to be used later in the kernel

    echo "------", [*], [* - $F000]d, "load_score"

load_score:

    lda score           ; load half the player score
    and #$0F            ; and out the low nibble
    tax
    lda scoreTable,x    ;
    sta tempIdx    ;
    lda scoreTable+10,x ;
    sta tempIdx+1  ;

    lda score           ; load half the player score
    lsr                 ; extract the high nibble
    lsr
    lsr
    lsr
    tax
    lda scoreTable,x    ;
    sta tempIdx+2      ;
    lda scoreTable+10,x  ;
    sta tempIdx+3    ;

    lda score+1           ; load half the player score
    and #$0F            ; and out the low nibble
    tax
    lda scoreTable,x    ;
    sta tempIdx+4      ;
    lda scoreTable+10,x  ;
    sta tempIdx+5    ;

    lda score+1           ; load half the player score
    lsr                 ; extract the high nibble
    lsr
    lsr
    lsr
    tax
    lda scoreTable,x    ;
    sta tempIdx+6      ;
    lda scoreTable+10,x  ;
    sta tempIdx+7    ;


    echo "------", [*], [* - $F000]d, "map_player"


; init_first_sprite
;
; Initialize the first sprite position, this triggers finding
; the next sprite in the kernel.
; TIME: 5 cycles

    echo "------", [*], [* - $F000]d, "init_first_sprite"

init_first_sprite:

    lda #SPRITEINIT     ; 2
    sta nexty           ; 3

init_first_sprite_end

; init_ball
;
; set up the ball, we use it to hide HMOVE bars

    echo "------", [*], [* - $F000]d, "init_ball"

init_ball:              ; WSYNC + 19

;    lda gameMode
;    cmp #MODE_ATTRACT
;    beq skipInitBall

    sta WSYNC           ; 3, 0
    STA RESBL           ; 3, 3  ; Reset the ball
    LDA #$22            ; 2, 5  ; Set the horizontal ball motion (+2)
    STA HMBL            ; 3, 8  ;
skipInitBall
    STA ENABL           ; 3, 11 ;Enable the ball (02)
    lda #%00110100      ; 2, 13
    sta CTRLPF          ; 3, 16

init_ball_end

;
; End the vertical blank
;

    echo "------", [*], [* - $F000]d, "vblank_end"

vblank_end:

waitOnVblank

    lda INTIM           ; wait for timeout to expire
    bne waitOnVblank

    sta WSYNC           ; wait for the next line.
    sta HMOVE           ; this HMOVE is paired with the init_ball routine above
    sta VBLANK          ; ""

vblank_end_end

; draw_score
;
; Draw the score

    echo "------", [*], [* - $F000]d, "main loop - draw score"

draw_score:

    lda #SCOREBGCOLOR   ; 2, 4
    sta COLUBK          ; 3, 7
    lda #SCORECOLOR
    ldy gameMode
    cpy #MODE_TRANSITION
    bne skipScoreBlink
    lda frameCount
    and #%00001111
    ora #%00000011
skipScoreBlink
    sta WSYNC           ; 3, -
    sta COLUP0          ; 3, 3
    sta COLUP1          ; 3, 6
    lda #1              ; 2, 8
    sta HMCLR           ; 3, 11     ; clear the motion register
    sta NUSIZ0          ; 3, 14     ; two copies of each player
    sta NUSIZ1          ; 3, 17     ;
    nop                 ; 2, 19
    SLEEP 20            ; 20, 39    ; waste cycles here.
    sta RESP0           ; 3, 42     ; start drawing
    sta RESP1           ; 3, 45
    lda #%11110000      ; 2, 47
    sta HMP0            ; 3, 50
    lda #%00000000      ; 2, 52
    sta HMP1            ; 3, 55
    sty nexty           ; 3, 58
    ldy #7              ; 2, 60
    sta WSYNC           ; 3, 3
    sta HMOVE           ; 3, 3
    nop                 ; 2, 5
score_loop              ; +1, 5
    dey                 ; 2, 7
    lda (tempIdx+6),y  ; 6, 13
    sta GRP0            ; 3, 16
    lda (tempIdx+4),y ; 6, 22
    sta GRP1            ; 3, 25
    lda (tempIdx),y ; 6, 31
    tax                 ; 2, 33
    lda (tempIdx+2),y ; 6, 39
    sleep 8             ; 8, 47
    sta GRP0            ; 3, 50
    stx GRP1            ; 3, 53
    sta WSYNC           ; 3, 66
    cpy #0              ; 2, 2
    bne score_loop      ; 2, 4
    lda #0              ; 2, 6
    sta GRP1            ; 3, 9
    sta GRP0            ; 3, 12
    lda  #%00010000     ; 2, 14

    lda #MISSILECOLOR   ; 2, 16
    sta COLUP0          ; 2, 18
    sta COLUP1          ; 2, 20

; init_player_position
;
; Initialize the player horizontal position, for use later in the
; kernel.

    echo "------", [*], [* - $F000]d, "init_player_position"

init_player_position

    lda p0x             ; 2, 22
    ldx #0              ; 2, 24
    jsr do_sprite_move  ; -,

init_player_position_end

; init_playfield
;
; Between drawing the score but before the playfield, initialize our
; sprite list: load the sprite ram index  and then preload the first
; debris sprite. This ends up taking a variable amount of time
; so we set a timer here and do the things we need then wait for
; it to expire.

    echo "------", [*], [* - $F000]d, "main loop kernel - init playfield"

init_playfied:

    sta WSYNC
    sta HMCLR                   ; so HMOVES do not effect sled position
    lda #7              ; 2, 22 ;
    sta TIM64T          ; 3, 25 ;

; preload pointers for the first debris sprite to be used later
; in the kernel.

preload_sprite

    lda #$EF            ; 2, 2  ; Yes, this is $EF
    sta nexty           ; 3, 5
    lda debrisCount     ; 3, 8
    beq preload_sprite_end ; 2, 10

    ldx #0              ; 2, 12
    lda spriteram,x     ; 4, 16
    sta p1dataptrlo     ; 3, 19
    lda spriteram+8,x   ; 4, 23
    sta p1dataptrhi     ; 3, 26

    ldy #17             ; 2, 28 ;
    lda (p1dataptrlo),y ; 6, 34 ;
    sta p1colorlo       ; 3, 37 ;
    iny                 ; 2, 39 ;
    lda (p1dataptrlo),y ; 6, 45 ;
    sta p1colorhi       ; 3, 48 ;

    ldx #0
    lda debrisy,x       ; 4, 58
    sta nexty           ; 3, 61
    lda debrisx,x       ; 4, 65
    ldx #1              ; 2, 67
    jsr do_sprite_move  ; ~91, 158

preload_sprite_end

    echo "------", [*], [* - $F000]d, "sky_wait"

init_playfield_wait
    lda INTIM           ; 3, --
    bne init_playfield_wait        ; 2, --


    lda  #%00010000     ; 2, 14
    sta  NUSIZ0         ; 3, 17
    sta  NUSIZ1         ; 3, 20

    sta WSYNC

init_playfield_end

; draw_top_line
;
; We draw the top line, and have a few additional cycles to set the
; playfield background color (and related visual effects). This MUST
; fit in a single scanline.
; MAX CYCLES: 46

    echo "------", [*], [* - $F000]d, "draw_top_line"

draw_top_line

    lda #CALOR_0C               ; 2, 2   ; line color between score and sky
    sta COLUBK             ; 3, 5   ;

    lda effectState        ; 3, 8
    cmp #%10000000         ; 2, 10   ; corresponding action
    bcs radioactiveEffect  ; 2, 12  ;
    cmp #%01000000         ; 2, 14  ;
    bcs absorberEffect     ; 2, 16  ;
    jmp resetBackground    ; 3, 19  ;

radioactiveEffect          ; 1, 13
    lda #$20               ; 2, 15
    jmp doneSetEffectColor ; 3, 18

absorberEffect             ; 1, 17
    lda #$80               ; 2, 19

doneSetEffectColor         ; 0, 18
    sta temp2hi            ; 3, 22
    lda effectState        ; 3, 25  ; if effectState is 0 no jump to
    beq resetBackground    ; 2, 27  ; resetBackground (done with effect)
    lsr                    ; 2, 29
    lsr                    ; 2, 31
    cmp #0                 ; 2, 33  ; if a is now zero go to set zero
    beq resetBackground    ; 2, 35
    clc                    ; 2, 37
    adc temp2hi            ; 3, 40
    jmp setBackground      ; 3, 43

resetBackground            ; +0, 19
    lda #SKYCOLOR          ; 2, 21

setBackground              ;
    sta WSYNC              ; 3, 23/46
    sta COLUBK             ; 3, 3

end_draw_top_line

; DO NOT DELETE THIS COMMENT SECTION!!!!!!!!
;    jmp draw_playfield
;AdjustCodeOrg1
;    .byte #0
;    .byte #0

    echo "------", [*], [* - $F000]d, "draw_playfield"

draw_playfield

    lda gameMode
    cmp #MODE_ATTRACT
    bne notTransition

draw_title_screen

    lda #0
    sta ENABL
    ldx #16
drawingTheTitlescreen1
    sta WSYNC
    dex                     ; 2, 2
    bne drawingTheTitlescreen1 ; 2, 4

    lda #SNOWCOLOR          ; 2, 6     ; change playfield color
    sta COLUPF              ; 3, 9     ;
    ldx #6                  ; 2, 11

    stx temp3               ; 3, 14
    lda #9                  ; 2, 16

draw_title_screen_loop           ; +1, 65

    ldy #4                  ; 2, 18,67

draw_title_inner_loop       ; +1, 58

    sta WSYNC               ; 3, 24,64,73
    sta COLUPF
    lda TScreenLeft1-1,X    ; 4, 4
    sta PF0                 ; 3, 7
    lda TScreenLeft2-1,X    ; 4, 11
    sta PF1                 ; 3, 14
    lda TScreenLeft3-1,X    ; 4, 18
    sta PF2                 ; 3, 21

    nop                     ; 2, 23
    nop                     ; 2, 25
    nop                     ; 2, 27

    lda TScreenRight1-1,X   ; 4, 31
    sta PF0                 ; 3, 34
    lda TScreenRight2-1,X   ; 4, 38
    sta PF1                 ; 3, 41
    lda TScreenRight3-1,X   ; 4, 45
    sta PF2                 ; 3, 48


    tya                     ; 2, 50
    adc temp3               ; 3, 53

    dey                     ; 2, 55

    bne draw_title_inner_loop ; 2, 57

    dex                     ; 2, 59
    stx temp3               ; 3, 62
    bne draw_title_screen_loop   ; 2, 64

    sta WSYNC                       ; clear the playfield registers
    sta ENABL
    lda #0
    sta COLUPF
    sta PF0
    sta PF1
    sta PF2

draw_title_screen_end

    ldx #99
drawingTheTitlescreen
    sta WSYNC
    dex
    bne drawingTheTitlescreen
    jmp draw_playfield_end

notTransition


    lda #1                  ; 2, 5     ;
    sta temp3               ; 3, 8     ;
    ldx #PLAYFIELDSZ        ; 2, 10    ;
do_playfield                ;+1, 5 21

    sec                     ; 2, 12 23 ; ---------------------------------------------
    txa                     ; 2, 14 25 ;
    sbc m1ys                ; 3, 17 28 ; draw missile 0, max 16 cycles
    adc #MISSILESIZE        ; 2, 19 30 ;
    lda #2                  ; 2, 21 32 ;
    adc #$ff                ; 2, 23 34 ;
    sta ENAM0               ; 3, 26 37 ; ---------------------------------------------
    sec                     ; 2, 28 39
    txa                     ; 2, 30 41 ; ---------------------------------------------
    sbc m0ys                ; 3, 33 44 ; draw missile 1, max 16 cycles
    adc #MISSILESIZE        ; 2, 35 46 ;
    lda #2                  ; 2, 37 48 ;
    adc #$ff                ; 2, 39 50 ;
    sta ENAM1               ; 3, 42 53 ; ---------------------------------------------
    txa                     ; 2, 44 55 ; ---------------------------------------------
    sec                     ; 2, 46 57 ; set up player1 - max  16 cycles
    sbc nexty               ; 3, 49 60 ;
    adc #ENEMYHEIGHT        ; 2, 51 62 ;
    bcc finishedSprite      ; 2, 53 64 ;
    tay                     ; 2, 55 66 ;
    sta WSYNC               ; 3, 58 69 ; 7 free cycles
    lda (p1dataptrlo),y     ; 5, 5     ; ---------------------------------------------
    sta GRP1                ; 3, 8     ; write player graphics & color - max 17 cyles
    lda (p1colorlo),y       ; 5, 13    ;
    sta COLUP1              ; 3, 16    ; ---------------------------------------------
    dex                     ; 2, 18    ;
    bne do_playfield        ; 2, 20    ;
    jmp draw_playfield_end  ; 3, 23    ;
finishedSprite              ;+1, 54 65 ;
    cmp #$FF                ; 2, 56 67 ; we just finished the last line of a sprite
    beq load_next_sprite    ; 2, 58 69 ; need to load the next one
    sta WSYNC               ; 3, 61 72 ; 4 FREE CYCLES
    dex                     ; 2, 2     ;
    bne do_playfield        ; 2, 4     ;
    jmp draw_playfield_end  ; 3, 7     ;
load_next_sprite            ;+1, 59 70 ;
    sta WSYNC               ; 3, 62 73 ; 1 FREE CYCLE

    lda temp3               ; 3, 3     ;
    cmp debrisCount         ; 3, 6     ;
    bcs no_more_sprites     ; 2, 8     ;

    stx temp1lo             ; 3, 11    ; load sprite data
    ldx temp3               ; 2, 13

    lda spriteram,x         ; 4, 17    ; ""
    sta p1dataptrlo         ; 3, 20    ; ""
    lda spriteram+8,x       ; 4, 24    ; ""
    sta p1dataptrhi         ; 3, 27    ; ""

    ldy #17                 ; 2, 29    ;
    lda (p1dataptrlo),y     ; 6*, 35   ;
    sta p1colorlo           ; 3, 38    ;
    iny                     ; 2, 40    ;
    lda (p1dataptrlo),y     ; 6*, 46   ;
    sta p1colorhi           ; 3, 49    ;

    inc temp3               ; 3, 59    ; ""
    lda debrisy,x           ; 4, 63    ; ""
    sta nexty               ; 3, 66    ; ""

    lda debrisx,x           ; 4, 70    ; setup A and X for do_sprite_move

    sta WSYNC               ; 3, 73    ; -----------------------------------
    sec                     ; 2, 2     ; inline skipdraw
DivideLoop1                 ;+1  7  52 ; This loop MAX 54 cycles if A is < 160, MIN 4
    sbc #15                 ; 2, 4  54 ;
    bcs DivideLoop1         ; 2, 6  56 ;
    tay                     ; 2, 8  58 ;
    lda FineAdjustTableEnd,Y; 5, 13 63 ;
    ldx #1                  ; 2, 15 65 ;
    sta HMP0,X              ; 4, 19 69 ;
    sta RESP0,X             ; 4, 23 73 ;
    sta WSYNC               ; 3, 26 76 ;
    sta HMOVE               ; 3, 3     ;

    ldx temp1lo             ; 3, 6     ;
    dex                     ; 2, 8     ;
    dex                     ; 2, 10    ;
    dex                     ; 2, 12    ;
    jmp do_playfield        ; 3, 15    ; checking dex is not required here - we know there is at least one more sprite
no_more_sprites             ;+1, 9     ;
    lda #$ef                ; 2, 11
    sta nexty               ; 3, 14
    dex                     ; 2, 16    ;
    beq draw_playfield_end  ; 2, 18    ;
    jmp do_playfield        ; 3, 21    ;
draw_playfield_end          ;+1, 7 19 23 ;


; draw_hills
;
; draw the hills just above the lunokhod. we have at most 52 cycles
; before the next line draws. p1dataptr and p1color were populated in
; draw_playfield for  us.
;
; 4 scan lines
;
    echo "------", [*], [* - $F000]d, "draw_hills"

draw_hills
    sta HMCLR           ; 3, 26 ; so HMOVE's will not reposition debris

    lda scrollpos       ; 3, 29     ; calculate the PFData offset and
    and #%11111100      ; 2, 31     ; put it in x
    clc                 ; 2, 33     ;
    adc #4              ; 2, 35     ;
    tax                 ; 2, 37     ;

    lda #SNOWCOLOR      ; 2, 39     ; change playfield color
    sta COLUPF          ; 3, 42     ;

    lda #$FF            ; 2, 44     ; initialize the debris counter at
    sta temp1lo         ; 3, 47     ; FF, this allows us to continue
                                    ; using skipdraw with 'nexty' from
                                    ; draw_playfield

hills_loop              ; +1, 42 47

    sec                 ; 2, 49 ;
    sbc nexty           ; 3, 52 ;
    adc #ENEMYHEIGHT    ; 2, 54 ;
    bcs doDrawSprite    ; 2, 56 ;
    sta WSYNC           ; 3, 59      ;
    sta HMOVE           ; 3, 3  ;
    lda PFData0-1,X     ; 4, 7
    sta PF0             ; 3, 10
    jmp overSprite      ; 3, 13
doDrawSprite            ; 1, 57 ; draw the sprite
    tay                 ; 2, 59
    lda (p1dataptrlo),y ; 6, 16
    sta WSYNC           ; 3, 62
    sta HMOVE           ; 3, 3
    sta GRP1            ; 3, 19
    lda (p1colorlo),y   ; 6, 16
    sta COLUP1          ; 3, 19
    lda PFData0-1,X     ; 4, 7
    sta PF0             ; 3, 10
overSprite              ;+1, 13
    lda PFData1-1,X     ; 4, 17
    sta PF1             ; 3, 20
    lda PFData2-1,X     ; 4, 24
    sta PF2             ; 3, 27
;    sleep 9
    dex                 ; 2, 29
    dec temp1lo         ; 5, 34
    lda temp1lo         ; 3, 37
    cmp #$FB            ; 2, 39
    bne hills_loop      ; 2, 41

draw_hills_end


;
; Draw the sled and rail
;

    echo "------", [*], [* - $F000]d, "draw_sled_and_rail"

draw_sled_and_rail


    lda #0              ; 2, 43
    sta ENAM0           ; 3, 46  ; turn off missiles
    sta ENAM1           ; 3, 49  ;

    sta WSYNC
;    sta HMOVE           ; 3, 3 ;

    sta COLUPF          ; 3, 6  ; set pf color to black for HMOVE hiding
    sta PF0             ; 3, 9  ; clear playfield data
    sta PF1             ; 3, 12 ;
    sta PF2             ; 3, 15 ;

    lda #SNOWCOLOR      ; 2, 17 ; change the background color to white
    sta COLUBK          ; 3, 20 ; ""

    ldx #10

    lda temp1lo         ;

do_sled

    sec                 ; 2
    sbc nexty           ; 3,  ;
    adc #ENEMYHEIGHT    ; 2,  ;
    bcs doDrawSprite2; 2,  ;
    sta WSYNC
    lda PlayerSprite,X
    sta GRP0                   ; 45
    lda PlayerColor,X
    sta COLUP0

    jmp overSprite2
doDrawSprite2
    tay
    lda (p1dataptrlo),y ; 6,        ;
    sta WSYNC
    sta GRP1
;    lda (p1colorlo),y   ; 6, 16
;    sta COLUP1          ; 3, 19    ; 3,

    lda PlayerSprite,X
    sta GRP0                   ; 45
    lda PlayerColor,X
    sta COLUP0    ;

overSprite2
    dec temp1lo
    lda temp1lo
    dex
    cpx #0
    bne do_sled
;; TEMP
    stx GRP1
;; END TEMP

;    lda #RAILCOLOR
;    sta COLUBK
    lda PlayerSprite,X
    sta WSYNC
    sta GRP0

    lda #PLATFORMCOLOR
    sta COLUBK
    lda #0
    sta GRP0

    ldy #172

draw_sled_and_rail_end

    echo "------", [*], [* - $F000]d, "draw_guage"

draw_gauge
    lda #%00000111      ; 2
    sta NUSIZ0          ; 3
    lda #63             ; 2
    ldx #0              ; 2
    jsr do_sprite_move  ; -, 9
    lda radlevel       ; 3, 12
    sta GRP0            ; 3, 15

    lda frameCount
;    and #%00000111
;    adc #$14

;    lda #GAUGECOLOR     ; 2, 17
    sta COLUP0          ; 3, 20
    sta WSYNC
    sta HMCLR
    sta WSYNC
    sta HMOVE
    lda #0
    sta GRP0
    lda #%11110000      ; 2
    sta PF2             ; 3
    lda #%00110001      ; 2
    sta CTRLPF          ; 3
    lda #PLATFORMCOLOR
    sta COLUPF
draw_gauge_end



; draw_radar
;
; draw the radar. this is extremely tight and needs to close to
; page boundary, hence the adjustment below.

    echo "------", [*], [* - $F000]d, "draw_radar BEFORE ALIGN"

;    jmp draw_radar
;    ALIGN #255,#0

; DO NOT DELETE THIS COMMENT SECTION!!!!!!!!
;    jmp draw_radar
;AdjustCodeOrg1
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0
;    .byte #0


    echo "------", [*], [* - $F000]d, "draw_radar AFTER ALIGN"

draw_radar
    sta WSYNC
    sta HMOVE
    lda #1              ; 2, 2
    sta HMCLR           ; 3, 5
    sta NUSIZ0          ; 3, 8
    sta NUSIZ1          ; 3, 11

    nop
    nop
    nop
    nop
    nop
    nop
    lda #$8e
    jmp setColor

; old radar color code - leave this here for now
;    lda radarColor      ; 3, 14
;    asl                 ; 2, 16
;    sta radarColor      ; 3, 19
;    and #%10000000      ; 2, 21
;    beq normalRadarColor; 2, 23
;    lda #$1e             ; 2, 25
;    jmp setColor        ; 3, 28
;normalRadarColor        ; +1, 24
;    lda #RADARCOLOR     ; 2, 26
;    nop                 ; 2, 28

setColor
    sta COLUP0          ; 3, 31
    sta COLUP1          ; 3, 34
    nop                 ; 2, 36

;    SLEEP 17            ; 20, 39
    sta RESP0           ; 3, 42
    sta RESP1           ; 3, 45
    lda #%11110000      ; 2, 47
    sta HMP0            ; 3, 50
    lda #%00000000      ; 2, 52
    sta HMP1            ; 3, 55
;    sty nexty           ; 3, 58
    ldx #32             ; 2, 60
    lda #0
    sta COLUPF          ; 3, 6
    sta WSYNC           ; 3, 0
    sta HMOVE           ; 3, 3
    nop                 ; 2, 5
radar_loop              ; +1, 5
    dex                 ; 2, 7
    dex                 ; 2, 9
    dex                 ; 2  11
    dex                 ; 2, 13
    lda radarRam,x      ; 4, 17
    sta GRP0            ; 3, 20
    lda radarRam+1,x    ; 4, 24
    sta GRP1            ; 3, 27
    ldy radarRam+3,x    ; 4, 31
    lda radarRam+2,x    ; 4, 35
    sleep 9             ; 9, 44
    sta GRP0            ; 3, 47
    sty GRP1            ; 3, 50

;    nop
    nop
    nop
    nop
    nop
    nop
    lda #$8e
    jmp setColor2
; old radar color code - leave here for now
;    lda radarColor      ; 3, 53
;    asl                 ; 2, 55
;    sta radarColor      ; 3, 58
;
;    and #%10000000      ; 2, 60
;    beq normalRadarColor2 ; 2, 62
;    lda #$1e            ; 2, 64
;    jmp setColor2       ; 3, 67
;normalRadarColor2       ; +1, 63
;    lda #RADARCOLOR     ; 2, 65
;    nop                 ; 2, 67
setColor2
    sta COLUP0          ; 3, 70
    sta COLUP1          ; 3, 73

    sta WSYNC           ; 3, 75
    cpx #4              ; 2, 2
    bne radar_loop      ; 2, 4

    lda #RADARCOLOR2    ; 2, 6
    sta COLUP1          ; 3, 9
    sta COLUP0          ; 3, 12
    lda radarRam        ; 3, 15
    nop                 ; 2, 17
    sta GRP0            ; 3, 20 ; this must be on cycle 19 or 20
    lda radarRam+1      ; 3, 23
    sta GRP1            ; 3, 26 ; this must be on cycle 26 or 27
    ldy radarRam+3      ; 3, 29
    lda radarRam+2      ; 3, 32
    sleep 12            ; 12, 44
    sta GRP0            ; 3, 47 ; these three must start on cycle 47
    sty GRP1            ; 3, 50
    sta WSYNC

    sleep 10            ; 10, 10
    ldx #0              ; 2, 12
    lda radarRam,x      ; 4, 16
    sta GRP0            ; 3, 19 ; this must be on cycle 19 or 20
    lda radarRam+1,x    ; 4, 23
    sta GRP1            ; 3, 26 ; this must be on cycle 26 or 27
    ldy radarRam+3,x    ; 4, 30
    lda radarRam+2,x    ; 4, 34
;    stx temp1lo         ; 3, 37
;    ldx #0              ; 2, 39
    sleep 10             ; 10, 44
    sta GRP0            ; 3, 47 ; these three must start on cycle 47
    sty GRP1            ; 3, 50
;    stx GRP0            ; 3, 53
;    stx GRP1            ; 3, 56

    sta WSYNC
    lda #$0e     ; 2, 16
    sta COLUP0          ; 3, 19
    sta COLUP1          ; 3, 14
    lda #0              ; 2, 6
    sta GRP1            ; 3, 9
    sta GRP0            ; 3, 12
    sta PF2
    sta NUSIZ0         ; 3, 26
    sta NUSIZ1         ; 3, 29
    sta WSYNC
    sta WSYNC
draw_radar_end

    echo "------", [*], [* - $F000]d, "overscan_start"

overscan_start
    lda #2              ; turn on the vblank
    sta VBLANK          ; ""
    lda  #34          ; wait for 4544 cycles then sync
    sta  TIM64T         ; ""
overscan_start_end

;
; Process collisions
;

    echo "------", [*], [* - $F000]d, "do_collisions"

do_collisions

    lda gameMode
    cmp #MODE_IN_PROGRESS
    bne do_collisions_end
    lda CXM0P           ; 3     ; Check if any collisions were measured with missile 0
    and #%10000000      ; 2     ; ""
;    cmp #0              ; 2     ; ""
    beq no_m0_collision ; 2     ; ""

    ldx #0              ; 2     ; Walk through the debris list checking for collisions
m0_collision_loop       ; +1    ; ""
    lda debrisy,x        ; 4     ; ""
    sec                 ; 2     ; ""
    sbc m1ys            ; 3     ; ""
    cmp #$FD            ; 2     ; ""
    bcs found_m0        ; 2     ; ""
    cmp #ENEMYHEIGHT    ; 2     ;
    bcc found_m0        ; 2     ; ""
    jmp no_pm0_collision ; 3    ; ""
found_m0
    lda debrisinfo,x  ; if the debris is in condemned state this is not a collision
    and #%00000111      ; ""
;    cmp #0              ; ""
    bne no_pm0_collision ; ""
    jsr remove_shot_debris
    lda #$FF
    sta m1ys
    jmp no_m0_collision
no_pm0_collision
    inx
    cpx debrisCount
    bcc m0_collision_loop

no_m0_collision
    lda CXM1P           ; 3     ; Check if missile 1 has a collision
    and #%01000000      ;       ; ""
;    cmp #0              ;       ; ""
    beq no_m1_collision ;       ; ""

    ldx #0
m1_collision_loop
    lda debrisy,x
    sec
    sbc m0ys
    cmp #$FD
    bcs found_m1
    cmp #ENEMYHEIGHT
    bcc found_m1
    jmp no_pm1_collision
found_m1
    lda debrisinfo,x  ; if the debris is in a condemned state this is not a collision
    and #%00000111      ; ""
;    cmp #0              ; ""
    bne no_pm1_collision ; ""
    jsr remove_shot_debris
    lda #$FF
    sta m0ys
    jmp no_m1_collision
no_pm1_collision
    inx
    cpx debrisCount
    bcc m1_collision_loop

no_m1_collision

    lda gameMode
    cmp #MODE_CRASH
    beq do_collisions_end
    ; check for player/player collision
    lda CXPPMM
    and #%10000000
    beq do_collisions_end
    ldx debrisCount
    dex
    jsr remove_shot_debris

    lda radlevel                    ; update radlevel
    lsr                             ;
    clc                             ;
    adc #128                        ;
    sta radlevel                    ;

    lda #16
    sta waveCounter
    lda #MODE_CRASH
    sta gameMode

do_collisions_end
    sta CXCLR


; swap_missiles2
;
; every other frame we swap even/odd missiles

    echo "------", [*], [* - $F000]d, "swap_missiles"

swap_missiles2:

    lda frameCount          ; 3
    and #%0000001           ; 2
    beq swap_missiles2_end  ; 3-4
    ldx m0ys                ; 3
    lda m2ys                ; 3
    sta m0ys                ; 3
    stx m2ys                ; 3
    ldx m1ys                ; 3
    lda m3ys                ; 3
    sta m1ys                ; 3
    stx m3ys                ; 3

swap_missiles2_end

; check console input

    echo "------", [*], [* - $F000]d, "check_console_input"

check_console_input

    lda SWCHB                   ; if the reset button is not pressed
    lsr                         ; skip the down check
    bcs resetNotPressed         ;

    lda #MODE_RESET_DOWN        ; set state to MODE_RESET_DOWN
    sta gameMode                ;
    jmp do_joystick_end

resetNotPressed

    lda gameMode                ; reset button is up and we are currently
    cmp #MODE_RESET_DOWN        ; not in
    bne do_joystick

    lda #MODE_RESET             ; set state to MODE_RESET_DOWN
    sta gameMode                ;
    jmp do_joystick_end

do_joystick

    lda gameMode
    cmp #0
    bne no_attract_mode

    lda INPT4           ; first check the trigger
    bmi do_joystick_end
    lda #MODE_RESET
    sta gameMode
    jmp do_joystick_end


no_attract_mode
    cmp #127
    bcc do_joystick_end

    lda INPT4           ; first check the trigger
    bmi no_button
    jmp create_missile
create_missile_cb
no_button
    lda #%01000000      ; Left?
    bit SWCHA           ;
    bne do_joystick_1   ; Nope, check Right
    dec p0s
    jmp check_bounds
do_joystick_1
    lda #%10000000      ; Right?
    bit SWCHA           ;
    bne do_slowdown     ; Nope, start slowing down
    inc p0s
    jmp check_bounds
do_slowdown

;    lda frameCount
;    and #%00000011
;    bne do_joystick_end

    lda p0s
    beq do_joystick_end
    bmi do_left_slowdown
    dec p0s
    jmp check_bounds
do_left_slowdown
    inc p0s
check_bounds
    lda p0s
    cmp #MINSPEED
    bmi less_than_1
    cmp #MAXSPEED+1
    bpl greater_than_1
    jmp do_joystick_end
less_than_1
    lda #MINSPEED
    sta p0s
    jmp do_joystick_end
greater_than_1
    lda #MAXSPEED
    sta p0s
do_joystick_end

;
; If there was no joystick movement, then skip the positioning routines.
;
    lda p0s
    bne p0_position
    jmp skip_positioning

;
; p0_position determine the position for player 0, which also
; determines the virtual screen offset
;

    echo "------", [*], [* - $F000]d, "p0_position"

p0_position
    lda p0s
    clc
    adc p0vxlo
    sta p0vxlo
p0_position_end

;
; Compute the screen position of the player and scroll offset
;

    echo "------", [*], [* - $F000]d, "calc_offset"

calc_offset
    lda vxoffsetlo
    sta temp1lo
    lda p0s
    clc
    adc p0x
    cmp #MINXPOS
    bcc adjustOffsetDown
    cmp #MAXXPOS+1
    bcs adjustOffsetUp
    jmp noAdjustOffset
adjustOffsetDown
    sta temp3
    lda #MINXPOS
    sec
    sbc temp3
    sta temp3
    lda vxoffsetlo
    sec
    sbc temp3
    sta vxoffsetlo

    lda scrollpos
    sec
    sbc temp3
    sta scrollpos
    cmp #200
    bcs wrapUp
    jmp scrollDone
wrapUp
    clc
    adc #80
    sta scrollpos
scrollDone


    jmp noAdjustOffset
adjustOffsetUp
    sec
    sbc #MAXXPOS
    sta temp3
    lda vxoffsetlo
    clc
    adc temp3
    sta vxoffsetlo

    lda scrollpos
    clc
    adc temp3
    sta scrollpos
    cmp #80
    bcs wrapDown
    jmp scrollDone2
wrapDown
    sec
    sbc #80
    sta scrollpos
scrollDone2


noAdjustOffset

    lda p0vxlo
    jsr convert_virtual_xpos
    sta p0x

calc_offset_end

    echo "------", [*], [* - $F000]d, "create_debris"

skip_positioning

create_debris

    lda gameMode                    ; only create new debris if the game
    cmp #MODE_IN_PROGRESS           ; is in progress
    beq ce_continue                 ;
    jmp create_debris_end           ;
ce_continue                         ;

    lda #20                         ; load waveLimit into a
                                    ; todo: multiply by vertical speed
    cmp waveCounter                 ; compare a to waveCounter
    bne skip_gameMode_change2           ; if a != limit, skip the rest

    cmp hitRatio
    bne skipHitRatioBonus

    clc
    lda score                  ; adjust the score
    sed
    adc #160
    sta score
    bcc skipChainedAdd
    inc score+1
skipChainedAdd
    cld
    lda #0
    sta hitRatio

    lda #%01011111
    sta effectState               ; currently handling a radioactive
;    lda #%01000000                 ; event.
;skip_absorber

;    lda radlevel                   ; adjust the radlevel
;    asl                            ;
;    sta radlevel                   ;


skipHitRatioBonus


    lda debrisCount                 ; load debrisCount into a
    cmp #0                          ; compare a to 0
    bne skip_gameMode_change1           ; if a != 0 skip

    lda #MODE_END_WAVE              ; if there is then we set gameMode to
    sta gameMode                        ; MODE_END_WAVE and set the waveCounter
    sed
    clc
    lda wave                        ; using adc instead of inc for
    adc #1                          ; decimal mode
    sta wave
    cld
    lda #0                          ; back to 0
    sta waveCounter                 ;
    sta hitRatio

skip_gameMode_change1                   ;
    jmp create_debris_end           ;
skip_gameMode_change2                   ;

    lda debrisCount                 ; make sure there is a slot left
    cmp #8                          ; ""
    beq create_debris_end           ; ""

    lda debrisy                     ; make sure there is enough space
    cmp #PLAYFIELDSZ-8              ; for new debris.
    bcs create_debris_end           ;

    ldx #6                          ; shift the list down (this can take
shift_down                  ; +1    ; up to 350 cycles - ouch!)
    lda debrisvx,x          ; 4     ;
    sta debrisvx+1,x        ; 4     ;
    lda debrisinfo,x        ; 4     ;
    sta debrisinfo+1,x      ; 4     ;
    lda debrisy,x           ; 4     ;
    sta debrisy+1,x         ; 4     ;
    cpx #0                  ; 2     ;
    beq done_shift_down     ; 2     ;
    dex                     ;       ;
    jmp shift_down          ;       ;
done_shift_down                     ;

    lda #PLAYFIELDSZ+11             ; set the y position for the new
    sta debrisy                     ; debris

    jsr update_random_seed

    and #%00111000                  ; use the random number generated
    sta debrisinfo                  ; for position to set the debris
                                    ; info (type of sprite, fall
                                    ; direction)

    lda randomSeed
    cmp #128
    bcs skipDirectionChange

    lda direction
    eor #$FF
    sta direction

skipDirectionChange

    lda direction
    cmp #0
    beq rightDirection

    lda randomSeed
    ora #%0010000
    and #%0011111
    clc
    adc debrisvx,1
    sta debrisvx
    jmp doneDirection

rightDirection

    lda randomSeed
    ora #%0010000
    and #%0011111
    eor #$FF
    sec
    adc debrisvx,1
    sta debrisvx

doneDirection

    inc waveCounter
    inc debrisCount
    lda debrisCount
    cmp #8
    bcc create_debris_end
    lda #8
    sta debrisCount

create_debris_end

    lda #0
    sta COLUBK          ; so the first line on the top will be black

;
; Clear the RAM that we will use to build the radar.
; Unrolled to reduce cycle use.  (110 cycles)
;

    echo "------", [*], [* - $F000]d, "clear ram for radar"

    lda #$00            ; 2, 11
    sta radarRam        ; 3, 14
    sta radarRam+1      ; 3, 17
    sta radarRam+2      ; 3, 20
    sta radarRam+3      ; 3, 23
    sta radarRam+4      ; 3, 26
    sta radarRam+5      ; 3, 29
    sta radarRam+6      ; 3, 32
    sta radarRam+7      ; 3, 35
    sta radarRam+8      ; 3, 38
    sta radarRam+9      ; 3, 41
    sta radarRam+10     ; 3, 44
    sta radarRam+11     ; 3, 47
    sta radarRam+12     ; 3, 50
    sta radarRam+13     ; 3, 53
    sta radarRam+14     ; 3, 56
    sta radarRam+15     ; 3, 59
    sta radarRam+16     ; 3, 62
    sta radarRam+17     ; 3, 65
    sta radarRam+18     ; 3, 68
    sta radarRam+19     ; 3, 71
    sta radarRam+20     ; 3, 74
    sta radarRam+21     ; 3, 77 ----- scanline + 1
    sta radarRam+22     ; 3, 2
    sta radarRam+23     ; 3, 5
    sta radarRam+24     ; 3, 8
    sta radarRam+25     ; 3, 11
    sta radarRam+26     ; 3, 14
    sta radarRam+27     ; 3, 17
    sta radarRam+28     ; 3, 20
    sta radarRam+29     ; 3, 23
    sta radarRam+30     ; 3, 26
    sta radarRam+31     ; 3, 29
    iny                 ; 2, 31
    iny                 ; 2, 33

;
; Draw our debris onto the radar. radar memory is a 4x8 byte area arranged
; by row:
;
;                            column
;                 00       01       02       03
;        00    00000000 00000000 00000000 00000000
;        01    00000000 00000000 00000000 00000000
;      r 02    00000000 00000000 00000000 00000000
;      o 03    00000000 00000000 00000000 00000000
;      w 04    00000000 00000000 00000000 00000000
;        05    00000000 00000000 00000000 00000000
;        06    00000000 00000000 00000000 00000000
;        07    00000000 00000000 00000000 00000000
;

    echo "------", [*], [* - $F000]d, "map_debris"

map_debris
    lda debrisCount
;    cmp #0
    beq map_debris_done

    ldx #0
;    stx radarColor
    lda debrisy         ; calculate what will be the starting y coord on the map
    sec                 ; "" divide by 21
DivideBy22              ; +1
    inx                 ; 2
    sbc #22             ; 2
    bcs DivideBy22      ; 2 repeat up to 6 times, result will be in [0,6]
;    inx                 ; add 1 to make it [1,7]
    stx temp1hi
    ldx #0
map_debris_loop

;    lda radarColor
;    asl
;    sta radarColor

    lda debrisinfo,x    ; skip the enmy if it has been condemned
    and #%00000001      ; ""
    cmp #%00000001      ; ""
    beq skip_debris     ; ""

;    lda debrisinfo,x
;    and #%00011000
;    cmp #%00011000
;    bne notRadioactive
;    inc radarColor
;notRadioactive

    ldy #0
    lda debrisvx,x
    cmp #64
    bcc no1a
    iny
no1a
    cmp #128
    bcc no2a
    iny
no2a
    cmp #192
    bcc no3a
    iny
no3a
    sty temp1lo

    lda temp1hi
    asl                 ; "" (multiple result by 4)
    asl                 ; ""

    clc                 ; Add row and column offsets together to
    adc temp1lo          ; get our insert offset
    sta temp1lo          ; ""

    lda debrisvx,x       ; calculate the remainder put it in Y
    lsr                 ; ""
    lsr                 ; ""
    lsr                 ; ""
    tay                 ; ""

    lda map_debris_table,y
    sta temp2lo

    lda temp1lo
    tay
    lda radarRam,y
    ora temp2lo
    sta radarRam,y
skip_debris
    dec temp1hi
    beq map_debris_done
    inx
    cpx debrisCount
    bcc map_debris_loop
map_debris_done

;
; Map the player onto the radar
;
    echo "------", [*], [* - $F000]d, "map_player"

map_player

    ldy #$FF
    lda p0vxlo          ; calculate the x coord for the map
    sec                 ; "" divide by 65
DivideBy64              ; +1
    iny                 ; 2
    sbc #64             ; 2
    bcs DivideBy64      ; 2 repeat up to 3 times, result will be in [0,3]

    lda p0vxlo          ; divide by 8 and lookup bit offset
    lsr
    lsr
    lsr
    tax
    lda map_debris_table,x
    sta radarRam,y
map_player_end

    echo "------", [*], [* - $F000]d, "overscan_end"

overscan_end
waitOnOverscan          ; wait for overscan to finish
    lda INTIM           ; ""
    bne waitOnOverscan  ; ""
overscan_end_end

    jmp main_loop

;
; The trigger was pressed, create a missile if we can
;
; if (shotcount > 0) skip
; x = 8l
; while (x != 0) {
;    x = x - 2;
;    if (missileVirtualXPostion[x] == 256) {
;        shotcount = SHOT_TIMING
;        missileVirtualXPostion[x] = shipVirtualXPostion;
;        break;
;    }
; }
;

    echo "------", [*], [* - $F000]d, "routine: create_missile"

create_missile

    lda shotcount                   ; if shotcount is
;    cmp #0
    bne no_free_missiles
    ldx #8
check_missile_n
    cpx #0
    beq no_free_missiles
    dex
    dex
    lda m0vxlo+1,x
    cmp #$FF
    bne check_missile_n
    lda #SHOT_TIMING
    sta shotcount
    lda #0
    sta m0vxlo+1,x
    lda p0vxlo          ; add the P0center to align with the ship
    sta m0vxlo,x
no_free_missiles
    jmp create_missile_cb

; remove_shot_debris
;
; remove debris that was shot, x is assumed to be set to the debrisinfo
; index.
;
;     * Set the condemned countdown timer in debrisinfo (00000xxx) to 001
;     * Set the sprite index in debrisinfo (00xxx000) to 100

    echo "------", [*], [* - $F000]d, "routine: remove_shot_debris"

remove_shot_debris

    inc hitRatio
    lda effectState             ; play the explosion audio, but only
    cmp #%01000000              ; when there are other events playing
    bcs skipNoise

    lda #%00111111            ; 2     ;
    sta effectState   ; 3     ;

skipNoise

    lda debrisinfo,x   ; 4     ; set the condemned counter to 1 on this
    ora #%00000001     ; 2     ; debris
    sta debrisinfo,x   ; 4     ;

    lda #1                     ; adjust the score
    sed                        ;
    clc                        ;
    adc score                  ;
    sta score                  ;
    lda #0                     ;
    adc score+1                ;
    sta score+1                ;
    cld                        ;
    rts                ; 6

remove_shot_debris_end

; Horizontal position routine. Found by members of the Stella mailing list
; in code for Atari Battlezone. HMOVE needs to be called some time after
; invoking this routine.
;
; Inputs:
;   A = Desired position.
;   X = Desired object to be positioned (0-5).
; scanlines:
;   If control comes on or before cycle 73 then 1 scanline is consumed.
;   If control comes after cycle 73 then 2 scanlines are consumed.
; Outputs:
;   X = unchanged
;   A = Fine Adjustment value.
;   Y = the "remainder" of the division by 15 minus an additional 15.
;       control is returned on cycle 6 of the next scanline.
;
; TIME: 85 + whatever time was left in the current line

    echo "------", [*], [* - $F000]d, "routine: do_sprite_move"

do_sprite_move
    sta WSYNC           ; 3, ?
    sec                 ; 2, 2
DivideLoop              ; This loop MAX 54 cycles if A is < 160, MIN 4
    sbc #15             ; 2, [4
    bcs DivideLoop      ; 2/3 [6,56]
    tay                 ; 2, [8,58]
    lda FineAdjustTableEnd,Y    ; 5, [13,63]
    nop                 ; 2, [15,65]
    sta HMP0,X          ; 4  [19,69]
    sta RESP0,x         ; 4  [23,73]
    sta WSYNC           ; 3  [26,76]
    sta HMOVE           ; 3, 3
    rts                 ; 6, 9

;
; Input:
;     A : least significat byte of vx position to convert
;     vxoffsetlo : least significant byte of scroll offset
; Output:
;     A : xposition or $FF if would be off screen

    echo "------", [*], [* - $F000]d, "routine: convert_virtual_xpos"

convert_virtual_xpos
    sec                 ; 2, 2
    sbc vxoffsetlo      ; 3, 5
    cmp #MINXSCREEN              ; 2, 7
    bcc off_screen      ; 2, 9
    cmp #MAXXSCREEN            ; 2, 11
    bcs off_screen      ; 2, 13
    rts                 ; 6, 19
off_screen              ; +1, 14
    lda #DEBRIS_HIDDEN            ; 2, 16
    rts                 ; 6, 22
convert_virtual_xpos_end

; update our random seed

    echo "------", [*], [* - $F000]d, "update_random_seed"

update_random_seed:

    lda randomSeed
    beq doEor
    asl
    beq noEor
    bcc noEor
doEor
    eor #$1d
noEor
    sta randomSeed
    rts

update_random_seed_end


    echo "------", [*], [* - $F000]d, "code done, before ALIGN"

;    ALIGN #256,#0

    echo "------", [*], [* - $F000]d, "code done, after ALIGN"

; This table converts the "remainder" of the division by 15 (-1 to -15) to the correct
; fine adjustment value. This table is on a page boundary to guarantee the processor
; will cross a page boundary and waste a cycle in order to be at the precise position
; for a RESP0,x write

    echo "------", [*], [* - $F000]d, "FineAdjustTable"

FineAdjustTableBegin
        .byte %01100000 ;left 6
        .byte %01010000
        .byte %01000000
        .byte %00110000
        .byte %00100000
        .byte %00010000
        .byte %00000000 ;left/right 0
        .byte %11110000
        .byte %11100000
        .byte %11010000
        .byte %11000000
        .byte %10110000
        .byte %10100000
        .byte %10010000
        .byte %10000000 ;right 8
FineAdjustTableEnd      =       FineAdjustTableBegin - 241

    echo "------", [*], [* - $F000]d, "map_debris_table"

map_debris_table
    .byte #%10000000
    .byte #%01000000
    .byte #%00100000
    .byte #%00010000
    .byte #%00001000
    .byte #%00000100
    .byte #%00000010
    .byte #%00000001
    .byte #%10000000
    .byte #%01000000
    .byte #%00100000
    .byte #%00010000
    .byte #%00001000
    .byte #%00000100
    .byte #%00000010
    .byte #%00000001
    .byte #%10000000
    .byte #%01000000
    .byte #%00100000
    .byte #%00010000
    .byte #%00001000
    .byte #%00000100
    .byte #%00000010
    .byte #%00000001
    .byte #%10000000
    .byte #%01000000
    .byte #%00100000
    .byte #%00010000
    .byte #%00001000
    .byte #%00000100
    .byte #%00000010
    .byte #%00000001

; offset 48

; look up table controls vertical speed, indexed by 'wave'. bits:

vSpeedTable
    .byte #%00010101 ; add 1 in 3/4 frames
    .byte #%00010101 ; add 1 in 3/4 frames
    .byte #%00010101 ; add 1 in 3/4 frames
    .byte #%00010101 ; add 1 in 3/4 frames

    .byte #%01010101 ; add 1 in 4/4 frames
    .byte #%01010101 ; add 1 in 4/4 frames
    .byte #%01010101 ; add 1 in 4/4 frames
    .byte #%01010101 ; add 1 in 4/4 frames

    .byte #%01010101 ; add 1 in 4/4 frames
    .byte #%01010101 ; add 1 in 4/4 frames
    .byte #%01010101 ; add 1 in 4/4 frames
    .byte #%01010101 ; add 1 in 4/4 frames

    .byte #%01010110 ; add 1 in 3/4 frames, 2 in 1/4 frames
    .byte #%01010110 ; add 1 in 3/4 frames, 2 in 1/4 frames
    .byte #%01010110 ; add 1 in 3/4 frames, 2 in 1/4 frames
    .byte #%01010110 ; add 1 in 3/4 frames, 2 in 1/4 frames

    .byte #%01010110 ; add 1 in 3/4 frames, 2 in 1/4 frames
    .byte #%01010110 ; add 1 in 3/4 frames, 2 in 1/4 frames
    .byte #%01010110 ; add 1 in 3/4 frames, 2 in 1/4 frames
    .byte #%01010110 ; add 1 in 3/4 frames, 2 in 1/4 frames

    .byte #%01100110 ; add 1 in 2/4 frames, 2 in 2/4 frames
    .byte #%01100110 ; add 1 in 2/4 frames, 2 in 2/4 frames
    .byte #%01100110 ; add 1 in 2/4 frames, 2 in 2/4 frames
    .byte #%01100110 ; add 1 in 2/4 frames, 2 in 2/4 frames

    .byte #%01100110 ; add 1 in 2/4 frames, 2 in 2/4 frames
    .byte #%01100110 ; add 1 in 2/4 frames, 2 in 2/4 frames
    .byte #%01100110 ; add 1 in 2/4 frames, 2 in 2/4 frames
    .byte #%01100110 ; add 1 in 2/4 frames, 2 in 2/4 frames

    .byte #%01101010 ; add 1 in 1/4 frames, 2 in 3/4 frames
    .byte #%01101010 ; add 1 in 1/4 frames, 2 in 3/4 frames
    .byte #%01101010 ; add 1 in 1/4 frames, 2 in 3/4 frames
    .byte #%01101010 ; add 1 in 1/4 frames, 2 in 3/4 frames

; one entry per wave, controls game behavior
;
;     0 - 5 horizontal speed mask (11111 special case)
;     5      horizontal speed multiplier
;     6-8    vertical speed mask
;
;

hSpeedTable
    .byte #%00000000 ; no movement
    .byte #%00000000 ; no movement
    .byte #%00000000 ; no movement
    .byte #%00000000 ; no movement
    .byte #%01000000 ; add 1 in 1/4 frames
    .byte #%01000100 ; add 1 in 2/4 frames
    .byte #%01010100 ; add 1 in 3/4 frames

    .byte #%00000000 ; no movement
    .byte #%01000000 ; add 1 in 1/4 frames
    .byte #%01000000 ; add 1 in 3/4 frames
    .byte #%01000100 ; add 1 in 2/4 frames

    .byte #%00000000 ; no movement
    .byte #%01000000 ; add 1 in 1/4 frames
    .byte #%01000000 ; add 1 in 3/4 frames
    .byte #%01000100 ; add 1 in 2/4 frames


tempoTable
    .byte #9
    .byte #9
    .byte #9
    .byte #9

    .byte #8
    .byte #8
    .byte #8
    .byte #8

    .byte #8
    .byte #8
    .byte #8
    .byte #8

    .byte #7
    .byte #7
    .byte #7
    .byte #7

    .byte #7
    .byte #7
    .byte #7
    .byte #7

    .byte #7
    .byte #7
    .byte #7
    .byte #7

    .byte #6
    .byte #6
    .byte #6
    .byte #6

    .byte #6
    .byte #6
    .byte #6
    .byte #6


; moveTable bits
;   0 = unused
;   1 = move left
;   2 = move right
;   3 = unused
;   4 = unused
;   5 = unused
;   6 & 7 = move adder
hDirectionTable       ; if and table is 3 then horizontal movement is slower
    .byte #%00000000  ; 000 absorber    down
    .byte #%00000001  ; 001 medium rock right
    .byte #%00000001  ; 010 small rock  right
    .byte #%00000001  ; 011 radioactive right
    .byte #%00000010  ; 100 medium rock left
    .byte #%00000000  ; 101 rock        down
    .byte #%00000010  ; 110 small       left
    .byte #%00000010  ; 111 radioactive left


    echo "------", [*], [* - $F000]d, "before player sprites"

PlayerSprite
    .byte #%01000010
    .byte #%10100101
    .byte #%01000010
    .byte #%00111100
    .byte #%01111110
    .byte #%01111110
    .byte #%11111111
    .byte #%11111111
    .byte #%00011000
    .byte #%00011000
    .byte #%00011000

PlayerColor
    .byte #CALOR_0A
    .byte #CALOR_0A
    .byte #CALOR_0A
    .byte #CALOR_0A
    .byte #CALOR_0A
    .byte #CALOR_0A
    .byte #CALOR_0C
    .byte #CALOR_0E
    .byte #CALOR_02
    .byte #CALOR_42
    .byte #CALOR_42

spriteTable
    .byte #<sprite3FrameTable  ; 000 Absorber
    .byte #>sprite3FrameTable
    .byte #<sprite7FrameTable  ; 001 Right
    .byte #>sprite7FrameTable
    .byte #<sprite5FrameTable  ; 010 Small Right
    .byte #>sprite5FrameTable
    .byte #<sprite1FrameTable  ; 011 Radioactive
    .byte #>sprite1FrameTable
    .byte #<sprite6FrameTable  ; 100 Left
    .byte #>sprite6FrameTable
    .byte #<sprite2FrameTable  ; 101 Down
    .byte #>sprite2FrameTable
    .byte #<sprite4FrameTable  ; 110 Small Left
    .byte #>sprite4FrameTable
    .byte #<sprite1FrameTable  ; 111 Radioactive
    .byte #>sprite1FrameTable

sprite1FrameTable
    .byte #<sprite1Frame1
    .byte #>sprite1Frame1
    .byte #<sprite1Frame3
    .byte #>sprite1Frame3
    .byte #<sprite1Frame5
    .byte #>sprite1Frame5
    .byte #<sprite1Frame7
    .byte #>sprite1Frame7

sprite2FrameTable
    .byte #<sprite2Frame1
    .byte #>sprite2Frame1
    .byte #<sprite2Frame1
    .byte #>sprite2Frame1
    .byte #<sprite2Frame2
    .byte #>sprite2Frame2
    .byte #<sprite2Frame2
    .byte #>sprite2Frame2

sprite3FrameTable
    .byte #<sprite3Frame1
    .byte #>sprite3Frame1
    .byte #<sprite3Frame1
    .byte #>sprite3Frame1
    .byte #<sprite3Frame1
    .byte #>sprite3Frame1
    .byte #<sprite3Frame1
    .byte #>sprite3Frame1

sprite4FrameTable           ; left left
    .byte #<sprite4Frame1
    .byte #>sprite4Frame1
    .byte #<sprite4Frame2
    .byte #>sprite4Frame2
    .byte #<sprite4Frame3
    .byte #>sprite4Frame3
    .byte #<sprite4Frame2
    .byte #>sprite4Frame2


sprite5FrameTable           ; right right
    .byte #<sprite5Frame1
    .byte #>sprite5Frame1
    .byte #<sprite5Frame2
    .byte #>sprite5Frame2
    .byte #<sprite5Frame3
    .byte #>sprite5Frame3
    .byte #<sprite5Frame2
    .byte #>sprite5Frame2

sprite6FrameTable
    .byte #<sprite6Frame1
    .byte #>sprite6Frame1
    .byte #<sprite6Frame1
    .byte #>sprite6Frame1
    .byte #<sprite6Frame2
    .byte #>sprite6Frame2
    .byte #<sprite6Frame2
    .byte #>sprite6Frame2

sprite7FrameTable
    .byte #<sprite7Frame1
    .byte #>sprite7Frame1
    .byte #<sprite7Frame1
    .byte #>sprite7Frame1
    .byte #<sprite7Frame2
    .byte #>sprite7Frame2
    .byte #<sprite7Frame2
    .byte #>sprite7Frame2

explosionFrameTable
    .byte #<explosionSprite1
    .byte #>explosionSprite1
    .byte #<explosionSprite1
    .byte #>explosionSprite1
    .byte #<explosionSprite2
    .byte #>explosionSprite2
    .byte #<explosionSprite2
    .byte #>explosionSprite2

sprite1Frame1
        .byte #%00000000
        .byte #%00000000
        .byte #%00000001
        .byte #%00000010
        .byte #%00000110
        .byte #%00001100
        .byte #%00011100
        .byte #%00111000
        .byte #%01111100
        .byte #%00111110
        .byte #%00011100
        .byte #%00111000
        .byte #%00110000
        .byte #%01100000
        .byte #%01000000
        .byte #%10000000
        .byte #%00000000
    .byte #<boltColors1
    .byte #>boltColors1
sprite1Frame3
        .byte #%00000000
        .byte #%00000000
        .byte #%00000001
        .byte #%00000010
        .byte #%00000110
        .byte #%00001100
        .byte #%00011100
        .byte #%00111000
        .byte #%01111100
        .byte #%00111110
        .byte #%00011100
        .byte #%00111000
        .byte #%00110000
        .byte #%01100000
        .byte #%01000000
        .byte #%10000000
        .byte #%00000000
    .byte #<boltColors3
    .byte #>boltColors3
sprite1Frame5
        .byte #%00000000
        .byte #%00000000
        .byte #%00000001
        .byte #%00000010
        .byte #%00000110
        .byte #%00001100
        .byte #%00011100
        .byte #%00111000
        .byte #%01111100
        .byte #%00111110
        .byte #%00011100
        .byte #%00111000
        .byte #%00110000
        .byte #%01100000
        .byte #%01000000
        .byte #%10000000
        .byte #%00000000
    .byte #<boltColors5
    .byte #>boltColors5
sprite1Frame7
        .byte #%00000000
        .byte #%00000000
        .byte #%00000001
        .byte #%00000010
        .byte #%00000110
        .byte #%00001100
        .byte #%00011100
        .byte #%00111000
        .byte #%01111100
        .byte #%00111110
        .byte #%00011100
        .byte #%00111000
        .byte #%00110000
        .byte #%01100000
        .byte #%01000000
        .byte #%10000000
        .byte #%00000000
    .byte #<boltColors7
    .byte #>boltColors7

sprite2Frame1
    .byte #%00000000
    .byte #%00111000
    .byte #%01100110
    .byte #%11111011
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%01111111
    .byte #%01111110
    .byte #%00011100
    .byte #%10000001
    .byte #%0100100
    .byte #%01000100
    .byte #%00010000
    .byte #%00000000
    .byte #%00000000
    .byte #<DebrisColor1
    .byte #>DebrisColor1

sprite2Frame2
    .byte #%00000000
    .byte #%00111000
    .byte #%01100110
    .byte #%11111011
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%01111111
    .byte #%01111110
    .byte #%00011100
    .byte #%00000000
    .byte #%00100100
    .byte #%00010000
    .byte #%00100100
    .byte #%00010000
    .byte #%00000000
    .byte #<DebrisColor1
    .byte #>DebrisColor1

sprite3Frame1
    .byte #%00000000
    .byte #%01111100
    .byte #%11111110
    .byte #%01111100
    .byte #%01111100
    .byte #%00111000
    .byte #%00111000
    .byte #%00010000
    .byte #%00111000
    .byte #%00111000
    .byte #%01111100
    .byte #%01111100
    .byte #%11111110
    .byte #%01111100
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #<absorberColor1
    .byte #>absorberColor1

sprite4Frame1
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%01110000
    .byte #%01011000
    .byte #%11111000
    .byte #%11111000
    .byte #%11101000
    .byte #%01110000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #<DebrisColor2
    .byte #>DebrisColor2

sprite4Frame2
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%01110000
    .byte #%01011000
    .byte #%11111000
    .byte #%11111000
    .byte #%11101000
    .byte #%01110000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #<DebrisColor2
    .byte #>DebrisColor2

sprite4Frame3
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%01110000
    .byte #%01011000
    .byte #%11111000
    .byte #%11111000
    .byte #%11101000
    .byte #%01110000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #<DebrisColor2
    .byte #>DebrisColor2


sprite5Frame1
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00001110
    .byte #%00001011
    .byte #%00011111
    .byte #%00011111
    .byte #%00011101
    .byte #%00001110
    .byte #%01010000
    .byte #%00000000
    .byte #%10000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #<DebrisColor2
    .byte #>DebrisColor2

sprite5Frame2
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00001110
    .byte #%00001011
    .byte #%00011111
    .byte #%00011111
    .byte #%00011101
    .byte #%00001110
    .byte #%10100000
    .byte #%00000000
    .byte #%01000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #<DebrisColor2
    .byte #>DebrisColor2

sprite5Frame3
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00001110
    .byte #%00001011
    .byte #%00011111
    .byte #%00011111
    .byte #%00011101
    .byte #%00001110
    .byte #%01000000
    .byte #%00000000
    .byte #%10000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #<DebrisColor2
    .byte #>DebrisColor2

sprite6Frame1
    .byte #%00000000
    .byte #%00000000
    .byte #%01110000
    .byte #%11011000
    .byte #%10111000
    .byte #%11111000
    .byte #%11111000
    .byte #%11111010
    .byte #%11111000
    .byte #%11111010
    .byte #%01110010
    .byte #%00000100
    .byte #%01011001
    .byte #%00000010
    .byte #%00001000
    .byte #%00000000
    .byte #%00000000
    .byte #<DebrisColor1
    .byte #>DebrisColor1

sprite6Frame2
    .byte #%00000000
    .byte #%00000000
    .byte #%01110000
    .byte #%11011000
    .byte #%10111000
    .byte #%11111000
    .byte #%11111000
    .byte #%11111000
    .byte #%11111001
    .byte #%11111000
    .byte #%01110001
    .byte #%00000001
    .byte #%00000010
    .byte #%00101100
    .byte #%00000001
    .byte #%00000100
    .byte #%00000000
    .byte #<DebrisColor1
    .byte #>DebrisColor1

sprite7Frame1
    .byte #%00000000
    .byte #%00000000
    .byte #%00001110
    .byte #%00011011
    .byte #%00011101
    .byte #%00011111
    .byte #%00010111
    .byte #%01011111
    .byte #%00011111
    .byte #%01011111
    .byte #%01001110
    .byte #%00100000
    .byte #%10011010
    .byte #%01000000
    .byte #%00010000
    .byte #%00000000
    .byte #%00000000
    .byte #<DebrisColor1
    .byte #>DebrisColor1



sprite7Frame2
    .byte #%00000000
    .byte #%00000000
    .byte #%00001110
    .byte #%00011011
    .byte #%00011101
    .byte #%00011111
    .byte #%00010111
    .byte #%00011111
    .byte #%10011111
    .byte #%00011111
    .byte #%10001110
    .byte #%10000000
    .byte #%01000000
    .byte #%00110100
    .byte #%10000000
    .byte #%00100000
    .byte #%00000000
    .byte #<DebrisColor1
    .byte #>DebrisColor1


absorberColor1
    .byte #CALOR_0E
    .byte #CALOR_98
    .byte #CALOR_90
    .byte #CALOR_9A
    .byte #CALOR_90
    .byte #CALOR_9C
    .byte #CALOR_90
    .byte #CALOR_9E
    .byte #CALOR_90
    .byte #CALOR_9C
    .byte #CALOR_90
    .byte #CALOR_9A
    .byte #CALOR_90
    .byte #CALOR_98
    .byte #CALOR_90
    .byte #CALOR_98
    .byte #CALOR_90

DebrisColor1
    .byte #CALOR_0E
    .byte #CALOR_02
    .byte #CALOR_02
    .byte #CALOR_02
    .byte #CALOR_04
    .byte #CALOR_06
    .byte #CALOR_08
    .byte #CALOR_0A
    .byte #CALOR_0E
    .byte #CALOR_0C
    .byte #CALOR_0A
    .byte #CALOR_08
    .byte #CALOR_06
    .byte #CALOR_04
    .byte #CALOR_02
    .byte #CALOR_02
    .byte #CALOR_02

DebrisColor2
    .byte #CALOR_0E
    .byte #CALOR_02
    .byte #CALOR_02
    .byte #CALOR_02
    .byte #CALOR_04
    .byte #CALOR_06
    .byte #CALOR_08
    .byte #CALOR_08
    .byte #CALOR_0A
    .byte #CALOR_3A
    .byte #CALOR_3E
    .byte #CALOR_3C
    .byte #CALOR_3A
    .byte #CALOR_38
    .byte #CALOR_36
    .byte #CALOR_34
    .byte #CALOR_32

boltColors7
    .byte #CALOR_0E
    .byte #CALOR_18
    .byte #CALOR_1A
    .byte #CALOR_1E
    .byte #CALOR_1A
    .byte #CALOR_18
    .byte #CALOR_16
    .byte #CALOR_14
    .byte #CALOR_12
    .byte #CALOR_12
    .byte #CALOR_12
    .byte #CALOR_12
    .byte #CALOR_12
    .byte #CALOR_12
    .byte #CALOR_12
    .byte #CALOR_12
    .byte #CALOR_12
boltColors5
    .byte #CALOR_0E
    .byte #CALOR_12
    .byte #CALOR_12
    .byte #CALOR_14
    .byte #CALOR_16
    .byte #CALOR_18
    .byte #CALOR_1A
    .byte #CALOR_1E
    .byte #CALOR_1A
    .byte #CALOR_18
    .byte #CALOR_16
    .byte #CALOR_14
    .byte #CALOR_12
    .byte #CALOR_12
    .byte #CALOR_12
    .byte #CALOR_12
    .byte #CALOR_12
boltColors3
    .byte #CALOR_0E
    .byte #CALOR_12
    .byte #CALOR_12
    .byte #CALOR_12
    .byte #CALOR_12
    .byte #CALOR_12
    .byte #CALOR_12
    .byte #CALOR_14
    .byte #CALOR_16
    .byte #CALOR_18
    .byte #CALOR_1A
    .byte #CALOR_1E
    .byte #CALOR_1A
    .byte #CALOR_18
    .byte #CALOR_16
    .byte #CALOR_14
    .byte #CALOR_12
boltColors1
    .byte #CALOR_0E
    .byte #CALOR_12
    .byte #CALOR_12
    .byte #CALOR_12
    .byte #CALOR_12
    .byte #CALOR_12
    .byte #CALOR_12
    .byte #CALOR_12
    .byte #CALOR_12
    .byte #CALOR_12
    .byte #CALOR_12
    .byte #CALOR_14
    .byte #CALOR_16
    .byte #CALOR_18
    .byte #CALOR_1A
    .byte #CALOR_1E
    .byte #CALOR_1A

explosionSprite1
        .byte #%00000000
        .byte #%00000000
        .byte #%00101000
        .byte #%10000100
        .byte #%00101000
        .byte #%10010010
        .byte #%00101001
        .byte #%01000000
        .byte #%10001001
        .byte #%00100000
        .byte #%10001010
        .byte #%01001001
        .byte #%00000100
        .byte #%00100000
        .byte #%01001000
        .byte #%00010000
        .byte #%00000000
    .byte #<explosionColors
    .byte #>explosionColors

explosionSprite2
        .byte #%00000000
        .byte #%00000000
        .byte #%00101000
        .byte #%10010010
        .byte #%01000000
        .byte #%10001001
        .byte #%00100000
        .byte #%00101001
        .byte #%10001010
        .byte #%01001001
        .byte #%00000100
        .byte #%00100000
        .byte #%01001001
        .byte #%00000100
        .byte #%00010000
        .byte #%00100000
        .byte #%00000000
    .byte #<explosionColors
    .byte #>explosionColors

explosionColors
    .byte #$0e
    .byte #$02
    .byte #$08
    .byte #$0a
    .byte #$0a
    .byte #$0e
    .byte #$0e
    .byte #$0e
    .byte #$0e
    .byte #$0e
    .byte #$0e
    .byte #$0e
    .byte #$0e
    .byte #$0a
    .byte #$0a
    .byte #$08
    .byte #$08

    echo "------", [*], [* - $F000]d, "score sprites"

scoreTable
    .byte #<scoreZero
    .byte #<scoreOne
    .byte #<scoreTwo
    .byte #<scoreThree
    .byte #<scoreFour
    .byte #<scoreFive
    .byte #<scoreSix
    .byte #<scoreSeven
    .byte #<scoreEight
    .byte #<scoreNine
    .byte #>scoreZero
    .byte #>scoreOne
    .byte #>scoreTwo
    .byte #>scoreThree
    .byte #>scoreFour
    .byte #>scoreFive
    .byte #>scoreSix
    .byte #>scoreSeven
    .byte #>scoreEight
    .byte #>scoreNine

scoreZero
    .byte #%00111100
    .byte #%01000110
    .byte #%01000110
    .byte #%01000110
    .byte #%01000110
    .byte #%01000110
    .byte #%00111100
    .byte #%00000000
scoreOne
    .byte #%00011000
    .byte #%00011000
    .byte #%00011000
    .byte #%00011000
    .byte #%00011000
    .byte #%00011000
    .byte #%00011000
    .byte #%00000000
scoreTwo
    .byte #%00111110
    .byte #%01000000
    .byte #%01000000
    .byte #%00111100
    .byte #%00000110
    .byte #%01000110
    .byte #%00111100
    .byte #%00000000
scoreThree
    .byte #%00111100
    .byte #%01000110
    .byte #%00000110
    .byte #%00111100
    .byte #%00000110
    .byte #%01000110
    .byte #%00111100
    .byte #%00000000
scoreFour
    .byte #%00000110
    .byte #%00000110
    .byte #%00000110
    .byte #%00111110
    .byte #%01000110
    .byte #%01000110
    .byte #%01000110
    .byte #%00000000
scoreFive
    .byte #%00111100
    .byte #%01000110
    .byte #%00000110
    .byte #%01111100
    .byte #%01000000
    .byte #%01000000
    .byte #%01111110
    .byte #%00000000
scoreSix
    .byte #%00111100
    .byte #%01000110
    .byte #%01000110
    .byte #%01111100
    .byte #%01000000
    .byte #%01000010
    .byte #%00111100
    .byte #%00000000
scoreSeven
    .byte #%00011000
    .byte #%00011000
    .byte #%00001100
    .byte #%00001100
    .byte #%00000110
    .byte #%00000110
    .byte #%01111110
    .byte #%00000000
scoreEight
    .byte #%00111100
    .byte #%01000110
    .byte #%01000110
    .byte #%00111100
    .byte #%01000110
    .byte #%01000110
    .byte #%00111100
    .byte #%00000000
scoreNine
    .byte #%00111100
    .byte #%01000110
    .byte #%00000110
    .byte #%00111110
    .byte #%01000110
    .byte #%01000110
    .byte #%00111100
    .byte #%00000000

scoreW
    .byte #%10101010
    .byte #%11101010
    .byte #%10101110
    .byte #%10101010
    .byte #%10100100
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000

scoreA
    .byte #%01001110
    .byte #%10101000
    .byte #%10101110
    .byte #%10101000
    .byte #%10101110
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000

    echo "------", [*], [* - $F000]d, "radarColorTable"

radarColorTable
    .byte #CALOR_AE  ; absorbing
    .byte #CALOR_08  ; inert debris
    .byte #CALOR_08  ; inert debris
    .byte #CALOR_1E  ; radioactive
    .byte #CALOR_08  ; inert debris
    .byte #CALOR_08  ; inert debris
    .byte #CALOR_08  ; inert debris
    .byte #CALOR_1E  ; radioactive

    echo "------", [*], [* - $F000]d, "songLoopIntro"

songLoopIntro

    .byte #0, #8, #8, #1
    .byte #0, #8, #0, #24
    .byte #0, #8, #8, #1
    .byte #0, #8, #0, #24
    .byte #0, #8, #8, #1
    .byte #0, #8, #0, #24
    .byte #0, #8, #8, #1
    .byte #0, #8, #0, #24

songLoop

    .byte #30, #SONGTONE, #15, #11
    .byte #30, #SONGTONE, #4, #8
    .byte #30, #SONGTONE, #0, #17

    .byte #25, #SONGTONE, #15, #9
    .byte #25, #SONGTONE, #4, #3

    .byte #25, #SONGTONE, #15, #9
    .byte #25, #SONGTONE, #4, #3

    .byte #29, #SONGTONE, #15, #9
    .byte #29, #SONGTONE, #4, #4

    .byte #25, #SONGTONE, #15, #9
    .byte #25, #SONGTONE, #4, #4

    .byte #23, #SONGTONE, #15, #3
    .byte #23, #SONGTONE, #12, #5
    .byte #23, #SONGTONE, #2, #5

    .byte #22, #SONGTONE, #15, #12
    .byte #22, #SONGTONE, #4, #8
    .byte #22, #SONGTONE, #0, #16

    .byte #18, #SONGTONE, #15, #9
    .byte #18, #SONGTONE, #4, #3

    .byte #18, #SONGTONE, #15, #9
    .byte #18, #SONGTONE, #4, #3

    .byte #16, #SONGTONE, #15, #9
    .byte #16, #SONGTONE, #4, #3

    .byte #16, #SONGTONE, #15, #9
    .byte #16, #SONGTONE, #4, #7
    .byte #16, #SONGTONE, #0, #9

    .byte #255, #32, #0, #0

    echo "------", [*], [* - $F000]d, "include pfdata.asm"

    include "pfdata.asm"

    echo "------", [*], [* - $F000]d, "TScreen data"

TScreenLeft1

        .byte #%00000000
        .byte #%00000000
        .byte #%00000000
        .byte #%00000000
        .byte #%00000000
        .byte #%00000000

TScreenLeft2

        .byte #%01101110
        .byte #%01001010
        .byte #%01001010
        .byte #%01001010
        .byte #%01000000
        .byte #%01000000

TScreenLeft3

        .byte #%01110111
        .byte #%01010111
        .byte #%01010111
        .byte #%01110111
        .byte #%00000000
        .byte #%00000000

TScreenRight1

        .byte #%01110000
        .byte #%01110000
        .byte #%01110000
        .byte #%01110000
        .byte #%00010000
        .byte #%00010000

TScreenRight2

        .byte #%10101110
        .byte #%10101010
        .byte #%10101010
        .byte #%11101110
        .byte #%10000000
        .byte #%10000000

TScreenRight3

        .byte #%00000111
        .byte #%00000101
        .byte #%00000101
        .byte #%00000111
        .byte #%00000100
        .byte #%00000100

  IF PLUSROM
    echo "------", [*], [* - $F000]d, "SendPlusROMScore"

SendPlusROMScore
        lda     score+1                 ;3
        sta     WriteToBuffer           ; BCD score hi
        lda     score                   ;3
        sta     WriteToBuffer           ; BCD score lo
        lda     #HIGHSCORE_ID           ; game id in Highscore DB
        sta     WriteSendBuffer
        jmp     non_system_init

    echo "------", [*], [* - $F000]d, "PlusROM_API"
PlusROM_API
       .byte "a", 0, "h.firmaplus.de"
       .byte 0
  ENDIF

;
; Check our free space
;

    echo "------", [*], [* - $F000]d, "end"
    echo "------", [$FFFA - *]d, "bytes free before End of Cartridge"

;
; The 2600 looks at this memory location to find the program entry point
;

    ORG $FFFA

  IF PLUSROM
    .word (PlusROM_API - $E000)
  ELSE 
    .word sysinit          ; NMI
  ENDIF
    .word sysinit          ; RESET
    .word sysinit          ; IRQ

END

;
; Each sprite has the following overhead:
;     frame table      : 16 bytes
;     color table      : 16 bytes
;     each frame       : 17 bytes
;     each color frame : 17 bytes
;     at least one entry in the index : 2 bytes
; So, a sprite with 8 full color frames takes 16 + 16 + 2 + 17x8 + 17x8 = 306 bytes
; A sprite with 2 full color frames = 102 bytes
; A sprite with 1 frame = 68 bytes
;