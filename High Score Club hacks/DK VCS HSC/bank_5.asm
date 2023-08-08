
; ********************************************************************
;  Donkey Kong VCS
;
;    ROM Bank 5
;
;    $Date: Thu, 05 Jan 2017 20:55:15 +0100 $
;    $Author: dietrich $
;    $Revision: 477 $
;
;  Copyright (C) 2017 Andreas Dietrich
; ********************************************************************

; ********************************************************************
;
;       Code Section
;
; ********************************************************************

; ====================================================================
;       Drive Audio
; ====================================================================

DriveAudio:     SUBROUTINE

                ; evaluate audio code

                and     DemoMode_R

                sta     Temp
                bit     Temp
                bne     .switch

                ; clear variables if command is 0

.clear          ldx     #7
.loop           sta     FXType,x
                dex
                bpl     .loop
                rts

                ; switch between sound and music

.switch         bmi     DriveSound

; --------------------------------------------------------------------

DriveMusic:     SUBROUTINE
                and     #AUDIO_ARG_MASK
                tay
                lda     Temp
                and     #AUDIO_CHN_MASK
                bvs     .play0

.init1          cmp     #AUDIO_CHANNEL_1
                bcc     .init0
                ldx     B5_Track1_L,y
                stx     NPTrackPtrL+1
                ldx     B5_Track1_H,y
                stx     NPTrackPtrH+1
                ldx     #0
                stx     NPNote+1
                inx
                stx     NPTimer+1

.init0          and     #AUDIO_CHANNEL_0
                beq     .return
                ldx     B5_Track0_L,y
                stx     NPTrackPtrL+0
                ldx     B5_Track0_H,y
                stx     NPTrackPtrH+0
                ldx     #0
                stx     NPNote+0
                inx
                stx     NPTimer+0

.return         rts

.play0          cmp     #AUDIO_CHANNEL_0
                bne     .play1
                ldx     #0
                jmp     B5_DriveNPlayer

.play1          cmp     #AUDIO_CHANNEL_1
                bne     .play2
                ldx     #1
                jmp     B5_DriveNPlayer

.play2          ldx     #0
                jsr     B5_DriveNPlayer
                ldx     #1
                jmp     B5_DriveNPlayer

; --------------------------------------------------------------------

DriveSound:     SUBROUTINE
                and     #AUDIO_CHN_MASK
                bvs     .play0

.init           lsr
                lsr
                lsr
                lsr
                tax
                dex
                lda     Temp
                and     #AUDIO_ARG_MASK
                tay

                DO_SOUND_FX B5_SFXAudC, B5_SFXAudF, B5_SFXAudV, B5_SFXLength
                rts

.play0          cmp     #AUDIO_CHANNEL_0
                bne     .play1
                ldx     #0
                jmp     B5_DriveSoundFX

.play1          cmp     #AUDIO_CHANNEL_1
                bne     .play2
                ldx     #1
                jmp     B5_DriveSoundFX

.play2          ldx     #0
                jsr     B5_DriveSoundFX
                ldx     #1
                jmp     B5_DriveSoundFX

; ====================================================================
;       Bank 5 Draw Kong
; ====================================================================

B5_KONG_THROW = 1

B5_DrawKong:    SUBROUTINE

                ; setup playfield and players

                ldx     #6
                sta     WSYNC
.loop           dex
                bpl     .loop
                sta     RESP0
                sta     RESP1
                sta     RESBL
                lda     #$80
                sta     HMP0
                lda     #$90
                sta     HMP1
                sta     WSYNC
                sta     HMOVE
                sta     WSYNC                   ; reserve

                lda     #DK_COL_WHITE
                sta     COLUPF
                lda     #%00010001
                sta     CTRLPF
                lda     #%00000011
                sta     NUSIZ0
                sta     NUSIZ1

                ; draw alternating Kong planes 0/1

                lda     FrameCtr
                lsr
                lda     #KONG_COL_0
                bcc     .write
                lda     #KONG_COL_1
.write          sta     COLUP0
                sta     COLUP1
                bcs     .plane1
.plane0         jmp     B5_DrawKongThrow_0
.plane1         jmp     B5_DrawKongThrow_1


; ********************************************************************
;
;       Imports
;
; ********************************************************************

; noise player

B5_DriveNPlayer:FUNC_DriveNoisePlayer

; 48 bit sprites - draw Kong throw 0

arg0 SET ENABL                  ; ball behind Kong
arg1 SET ENABL                  ;
arg2 SET B5_KongThrow_BL        ;
arg3 SET B5_KongThrow_GRP0_0
arg4 SET B5_KongThrow_GRP1_0
arg5 SET B5_KongThrow_GRP2_0
arg6 SET B5_KongThrow_GRP3_0
arg7 SET B5_KongThrow_GRP4_0
arg8 SET B5_KongThrow_GRP5_0
arg9 SET $FF

B5_DrawKongThrow_0: FUNC_Bitmap48Centered arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9

; 48 bit sprites - draw Kong throw 1

arg0 SET ENABL                  ; ball behind Kong
arg1 SET ENABL                  ;
arg2 SET B5_KongThrow_BL        ;
arg3 SET B5_KongThrow_GRP0_1
arg4 SET B5_KongThrow_GRP1_1
arg5 SET B5_KongThrow_GRP2_1
arg6 SET B5_KongThrow_GRP3_1
arg7 SET B5_KongThrow_GRP4_1
arg8 SET B5_KongThrow_GRP5_1
arg9 SET $FF

B5_DrawKongThrow_1: FUNC_Bitmap48Centered arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9

; sound effects driver

arg0 SET B5_SFXAudC
arg1 SET B5_SFXAudF
arg2 SET B5_SFXAudV
arg3 SET B5_SFXLength
arg4 SET B5_SFXAddF
arg5 SET B5_SFXAndF
arg6 SET B5_SFXOrF
arg7 SET B5_SFXAddV
arg8 SET B5_SFXSpawn

B5_DriveSoundFX:FUNC_DriveSoundFX arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8


; ********************************************************************
;
;       Data Section
;
; ********************************************************************

; audio code masks

AUDIO_CMD_MASK   = %11000000 ; command mask
AUDIO_CHN_MASK   = %00110000 ; channel mask
AUDIO_ARG_MASK   = %00001111 ; argument mask

; audio commands

AUDIO_MUSIC_INIT = %00000000
AUDIO_MUSIC_PLAY = %01000000
AUDIO_SOUND_INIT = %10000000
AUDIO_SOUND_PLAY = %11000000

; audio channels

AUDIO_CHANNEL_0  = %00010000
AUDIO_CHANNEL_1  = %00100000
AUDIO_CHANNEL_2  = %00110000

; audio codes

AUDIO_CLEAR      = %00000000

; --------------------------------------------------------------------
;       Macros
; --------------------------------------------------------------------

        MAC MAKE_MUSIC_TRACK

                IF {2} & %01
MUSIC_TRACK_{1}_0 SET {3}_Track0
                ELSE
MUSIC_TRACK_{1}_0 SET 0
                ENDIF

                IF {2} & %10
MUSIC_TRACK_{1}_1 SET {3}_Track1
                ELSE
MUSIC_TRACK_{1}_1 SET 0
                ENDIF

        ENDM

        MAC MAKE_MUSIC_LOBYTE_TABLE

                BYTE    <{1}00{2},<{1}01{2},<{1}02{2},<{1}03{2},<{1}04{2},<{1}05{2},<{1}06{2},<{1}07{2}
                BYTE    <{1}08{2},<{1}09{2},<{1}10{2},<{1}11{2},<{1}12{2},<{1}13{2}

        ENDM

        MAC MAKE_MUSIC_HIBYTE_TABLE

                BYTE    >{1}00{2},>{1}01{2},>{1}02{2},>{1}03{2},>{1}04{2},>{1}05{2},>{1}06{2},>{1}07{2}
                BYTE    >{1}08{2},>{1}09{2},>{1}10{2},>{1}11{2},>{1}12{2},>{1}13{2}

        ENDM

        MAC WHIRL

.HI SET  0
.LO SET 10

            REPEAT {1}

                BYTE    .HI, {2}, {4}
                BYTE    .HI, {3}, {5}
                BYTE    .LO, {2}, {4}
                BYTE    .LO, {3}, {5}

.HI SET .HI + 1
.LO SET .LO + 1

            REPEND

        ENDM

        MAC MAKE_SOUND_EFFECT

SOUND_EFFECT_{1}_AUDC SET  {2}
SOUND_EFFECT_{1}_AUDF SET  {3}
SOUND_EFFECT_{1}_AUDV SET  {4}
SOUND_EFFECT_{1}_LENG SET  {5}
SOUND_EFFECT_{1}_ADDF SET  {6}
SOUND_EFFECT_{1}_ANDF SET  {7}
SOUND_EFFECT_{1}_ORAF SET  {8}
SOUND_EFFECT_{1}_ADDV SET  {9}
SOUND_EFFECT_{1}_SPWN SET {10}

        ENDM

        MAC MAKE_SOUND_EFFECT_TABLE

                BYTE    SOUND_EFFECT_00_{1},SOUND_EFFECT_01_{1},SOUND_EFFECT_02_{1},SOUND_EFFECT_03_{1}
                BYTE    SOUND_EFFECT_04_{1},SOUND_EFFECT_05_{1},SOUND_EFFECT_06_{1},SOUND_EFFECT_07_{1}
                BYTE    SOUND_EFFECT_08_{1},SOUND_EFFECT_09_{1},SOUND_EFFECT_10_{1},SOUND_EFFECT_11_{1}
                BYTE    SOUND_EFFECT_12_{1},SOUND_EFFECT_13_{1},SOUND_EFFECT_14_{1},SOUND_EFFECT_15_{1}
                BYTE    SOUND_EFFECT_16_{1},SOUND_EFFECT_17_{1}

        ENDM

; --------------------------------------------------------------------
;       Music
; --------------------------------------------------------------------

MUSIC_INIT_STAGE_1      = AUDIO_MUSIC_INIT | AUDIO_CHANNEL_0 |  0
MUSIC_INIT_STAGE_2      = AUDIO_MUSIC_INIT | AUDIO_CHANNEL_0 |  1
MUSIC_INIT_STAGE_3      = AUDIO_MUSIC_INIT | AUDIO_CHANNEL_0 |  2
MUSIC_INIT_STAGE_4      = AUDIO_MUSIC_INIT | AUDIO_CHANNEL_0 |  3
MUSIC_INIT_HAMMER       = AUDIO_MUSIC_INIT | AUDIO_CHANNEL_0 |  4
MUSIC_INIT_TIME_OUT     = AUDIO_MUSIC_INIT | AUDIO_CHANNEL_0 |  5
MUSIC_INIT_MARIO_DEAD   = AUDIO_MUSIC_INIT | AUDIO_CHANNEL_2 |  6
MUSIC_INIT_MARIO_WIN    = AUDIO_MUSIC_INIT | AUDIO_CHANNEL_2 |  7
MUSIC_INIT_INTRO        = AUDIO_MUSIC_INIT | AUDIO_CHANNEL_2 |  8
MUSIC_INIT_INTERMISSION = AUDIO_MUSIC_INIT | AUDIO_CHANNEL_2 |  9
MUSIC_INIT_ENDING_INTRO = AUDIO_MUSIC_INIT | AUDIO_CHANNEL_2 | 10
MUSIC_INIT_ENDING_A     = AUDIO_MUSIC_INIT | AUDIO_CHANNEL_2 | 11
MUSIC_INIT_ENDING_B     = AUDIO_MUSIC_INIT | AUDIO_CHANNEL_2 | 12
MUSIC_INIT_ENDING_C     = AUDIO_MUSIC_INIT | AUDIO_CHANNEL_2 | 13

MUSIC_PLAY_0            = AUDIO_MUSIC_PLAY | AUDIO_CHANNEL_0
MUSIC_PLAY_1            = AUDIO_MUSIC_PLAY | AUDIO_CHANNEL_1
MUSIC_PLAY_2            = AUDIO_MUSIC_PLAY | AUDIO_CHANNEL_2
MUSIC_PLAY              = MUSIC_PLAY_2

; music track table

                MAKE_MUSIC_TRACK 00, 1, B5_Stage1
                MAKE_MUSIC_TRACK 01, 1, B5_Stage2
                MAKE_MUSIC_TRACK 02, 1, B5_Stage3
                MAKE_MUSIC_TRACK 03, 1, B5_Stage4
                MAKE_MUSIC_TRACK 04, 1, B5_Hammer
                MAKE_MUSIC_TRACK 05, 1, B5_TimeOut
                MAKE_MUSIC_TRACK 06, 3, B5_MarioDead
                MAKE_MUSIC_TRACK 07, 3, B5_MarioWin
                MAKE_MUSIC_TRACK 08, 3, B5_Intro
                MAKE_MUSIC_TRACK 09, 3, B5_Intermission
                MAKE_MUSIC_TRACK 10, 3, B5_EndingIntro
                MAKE_MUSIC_TRACK 11, 3, B5_EndingA
                MAKE_MUSIC_TRACK 12, 3, B5_EndingB
                MAKE_MUSIC_TRACK 13, 3, B5_EndingC

B5_Track0_L:    MAKE_MUSIC_LOBYTE_TABLE MUSIC_TRACK_, _0
B5_Track0_H:    MAKE_MUSIC_HIBYTE_TABLE MUSIC_TRACK_, _0
B5_Track1_L:    MAKE_MUSIC_LOBYTE_TABLE MUSIC_TRACK_, _1
B5_Track1_H:    MAKE_MUSIC_HIBYTE_TABLE MUSIC_TRACK_, _1

;
; Stage 1 music ------------------------------------------------------
;
                SUBROUTINE

B5_Stage1_Track0:
.track0
                WORD    .pattern0_0
                WORD    .track0
                WORD    0

                ;      AUDF   AUDC|V  Lenght

.pattern0_0     BYTE    31,    $C3,    1*5   ; E3
                BYTE     0,      0,    4*5+3 ; -
                BYTE    24,    $C3,    1*5   ; G3#
                BYTE     0,      0,    2*5+3 ; -
                BYTE    20,    $C3,    1*5   ; B3
                BYTE     0,      0,    1*5   ; -
                BYTE    18,    $C3,    1*5   ; C4#
                BYTE     0,      0,    1*5   ; -
                BYTE    20,    $C3,    1*5   ; B3
                BYTE     0,      0,    1*5   ; -
                BYTE    -1

;
; Stage 2 music ------------------------------------------------------
;
                SUBROUTINE

B5_Stage2_Track0:
.track0
                WORD    .pattern0_0
                WORD    .track0
                WORD    0

                ;      AUDF   AUDC|V  Lenght

.pattern0_0     BYTE    24,    $C3,    1*5   ; G3#
                BYTE     0,      0,    1*5+3 ; -
                BYTE    24,    $C3,    1*5   ; G3#
                BYTE     0,      0,    0*5+3 ; -
                BYTE    24,    $C3,    1*5   ; G3#
                BYTE     0,      0,    0*5+3 ; -
                BYTE    -1

;
; Stage 3 music ------------------------------------------------------
;
                SUBROUTINE

B5_Stage3_JackJump_Track0:

                WORD    .pattern1_0
                WORD    B5_Stage3_Track0
                WORD    0

B5_Stage3_JackFall_Track0:

                WORD    .pattern2_0

B5_Stage3_Track0:
.track0
                WORD    .pattern0_0
                WORD    .track0
                WORD    0

                ;      AUDF   AUDC|V  Lenght

.pattern0_0     BYTE     0,      0,      0   ; -
                BYTE    -1

.pattern1_0     BYTE    16,    $C6,      1
                BYTE    14,    $C6,      1
                BYTE    26,    $C6,      1
                BYTE    24,    $C5,      2
                BYTE    22,    $C4,      1
                BYTE    20,    $C3,      3
                BYTE    18,    $C2,      3
                BYTE    16,    $C3,      1
                BYTE    15,    $C2,      1
                BYTE    13,    $C1,      1
                BYTE    -1

.pattern2_0     BYTE    16,    $47,      3
                BYTE    17,    $47,      3
                BYTE    18,    $46,      3
                BYTE    19,    $46,      3
                BYTE    20,    $45,      3
                BYTE    21,    $45,      3
                BYTE    22,    $44,      3
                BYTE    23,    $44,      3
                BYTE    24,    $43,      3
                BYTE    25,    $43,      3
                BYTE    26,    $42,      3
                BYTE    27,    $42,      3
                BYTE    28,    $41,      3
                BYTE    29,    $41,      3
                BYTE    30,    $41,      3
                BYTE    31,    $41,      3
                BYTE    -1

;
; Stage 4 music ------------------------------------------------------
;
                SUBROUTINE

B5_Stage4_Track0:
.track0
                WORD    .pattern0_0
                WORD    .track0
                WORD    0

                ;      AUDF   AUDC|V  Lenght

.pattern0_0     BYTE    24,    $C3,    1*5   ; G3#
                BYTE     0,      0,    2*5   ; -
                BYTE    31,    $C3,    1*5   ; E3
                BYTE     0,      0,    0*5+3 ; -
                BYTE    31,    $C3,    1*5   ; E3
                BYTE     0,      0,    0*5+3 ; -
                BYTE    -1

;
; Hammer music -------------------------------------------------------
;
                SUBROUTINE

B5_Hammer_Track0:
.track0
                WORD    .pattern0_0
                WORD    .pattern1_0
                WORD    .pattern1_0
                WORD    .pattern0_0
                WORD    .pattern0_0
                WORD    .pattern2_0
                WORD    .pattern0_0
                WORD    .pattern2_0
                WORD    .pattern0_0
                WORD    .pattern2_0
                WORD    .pattern3_0
                WORD    .pattern3_0
                WORD    .pattern2_0
                WORD    .pattern2_0
                WORD    .pattern4_0
                WORD    .pattern2_0
                WORD    .pattern4_0
                WORD    .pattern2_0
                WORD    .track0
                WORD    0

                ;      AUDF   AUDC|V  Lenght

.pattern0_0     BYTE    31,    $C3,    2*2   ; E3
                BYTE     0,      0,    3*2   ; -
                BYTE    -1
.pattern1_0     BYTE    31,    $C3,    2*2   ; E3
                BYTE     0,      0,    1*2   ; -
                BYTE    -1
.pattern2_0     BYTE    24,    $C3,    2*2   ; G3#
                BYTE     0,      0,    3*2   ; -
                BYTE    -1
.pattern3_0     BYTE    24,    $C3,    2*2   ; G3#
                BYTE     0,      0,    1*2   ; -
                BYTE    -1
.pattern4_0     BYTE    20,    $C3,    2*2   ; B3
                BYTE     0,      0,    3*2   ; -
                BYTE    -1

;
; Time out music -----------------------------------------------------
;
                SUBROUTINE

B5_TimeOut_Track0:
.track0
                WORD    .pattern0_0
                WORD    .track0
                WORD    0

                ;      AUDF   AUDC|V  Lenght

.pattern0_0     BYTE    11,    $43,    1*5   ; E6
                BYTE    21,    $62,    1*5   ; -
                BYTE    15,    $43,    1*5   ; B5
                BYTE    21,    $62,    1*5   ; -
                BYTE    18,    $43,    1*5   ; G5#
                BYTE    21,    $62,    1*5   ; -
                BYTE    -1

;
; Mario dead music ---------------------------------------------------
;
                SUBROUTINE

B5_MarioDead_Track0:
.track0
                WORD    .pattern0_0

B5_MarioDead_Track1:
.track1
                WORD    .pattern0_1

                ;       AUDF  AUDC|V  Lenght

.pattern0_0     BYTE     1,      0,      4   ; delay whirl
                WHIRL   14, $46, $41, 2, 2   ; whirl
                 ;
                BYTE    18,    $46,    2*5   ; G5#
                BYTE    15,    $46,    4*5   ; B5
                BYTE    24,    $46,    4*5   ; D5#
                BYTE    23,    $46,    7*5   ; E5
                BYTE     0,      0,      0   ; - NPTimer = 256

.pattern0_1     WHIRL   14, $46, $41, 2, 2   ; whirl
                 ;
                BYTE     0,      0,    2*5+4 ; -
                BYTE    20,    $C5,    4*5   ; B3
                BYTE    24,    $C5,    4*5   ; G3#
                BYTE    31,    $C5,    7*5   ; E3
                BYTE     0,      0,      0   ; - NPTimer = 256

;
; Stage end music ----------------------------------------------------
;
                SUBROUTINE

B5_MarioWin_Track0:
.track0
                WORD    .pattern0_0
                WORD    .pattern0_0
                WORD    .pattern0_2

B5_MarioWin_Track1:
.track1
                WORD    .pattern0_1
                WORD    .pattern0_1
                WORD    .pattern0_2

                ;      AUDF   AUDC|V  Lenght

.pattern0_0     BYTE    21,    $45,    1*5   ; F5
                BYTE     0,      0,      2   ; -
                BYTE    19,    $45,    1*5   ; G5
                BYTE     0,      0,      2   ; -
                BYTE    17,    $45,    1*5   ; A5
                BYTE     0,      0,      2   ; -
                BYTE    21,    $45,      7   ; F5
                BYTE     0,      0,    5*5   ; -
                BYTE    -1

.pattern0_1     BYTE     0,      0,    1*5   ; -
                BYTE    21,    $45,    1*5   ; F5
                BYTE     0,      0,      2   ; -
                BYTE    19,    $45,    1*5   ; G5
                BYTE     0,      0,      2   ; -
                BYTE    17,    $45,    1*5   ; A5
                BYTE     0,      0,      2   ; -
                BYTE    21,    $45,      7   ; F5
                BYTE     0,      0,    4*5   ; -
                BYTE    -1

.pattern0_2     BYTE     0,      0,      0   ; - NPTimer = 256

;
; Intro music --------------------------------------------------------
;
                SUBROUTINE

B5_Intro_Track0:
.track0
                WORD    .pattern0_0
                WORD    .pattern1_0
                WORD    .pattern1_0
                WORD    .pattern1_0
                WORD    .pattern1_0
                WORD    .pattern1_0
                WORD    .pattern1_0
                WORD    .pattern1_0
                WORD    .pattern1_0
                WORD    .pattern2_0

B5_Intro_Track1:
.track1
                WORD    .pattern0_1

                ;      AUDF   AUDC|V  Lenght

.pattern0_0     BYTE    18,    $C5,   12*5   ; C4#
                BYTE    16,    $C5,    4*5   ; D4#
                BYTE    15,    $C5,    8*5   ; E4
                BYTE    18,    $C5,    8*5   ; C4#
                BYTE    -1
.pattern1_0     BYTE    29,    $46,    1*5   ; C5
                BYTE    27,    $46,    1*5   ; C5#
                BYTE    -1
.pattern2_0     BYTE    27,    $46,   16*5   ; C5#
                BYTE    -1

.pattern0_1     BYTE    27,    $C5,   12*5   ; F3#
                BYTE    24,    $C5,    4*5   ; G3#
                BYTE    23,    $C5,    8*5   ; A3
                BYTE    27,    $C5,    8*5   ; F3#
                BYTE    19,    $C5,   16*5   ; C4
                BYTE    18,    $C5,   16*5   ; C4#
                BYTE    -1

;
; Intermission music -------------------------------------------------
;
                SUBROUTINE

B5_Intermission_Track0:
.track0
                WORD    .pattern0_0

B5_Intermission_Track1:
.track1
                WORD    .pattern0_1

                ;      AUDF   AUDC|V  Lenght

.pattern0_0     BYTE    20,    $46,    4*6   ; F5#
                BYTE    18,    $46,      4   ; G5#
                BYTE    18,    $43,      2   ; |
                BYTE     0,      0,    1*6   ; -
                BYTE    15,    $46,    2*6+4 ; B5
                BYTE    15,    $43,      2   ; |
                BYTE     0,      0,    1*6   ; -
                BYTE    18,    $46,    2*6   ; G5#
                BYTE    20,    $46,    2*6   ; F5#
                BYTE    18,    $46,    2*6   ; G5#
                BYTE    23,    $46,    3*6   ; E5
.pattern0_0_end BYTE     0,      0,      0   ; - NPTimer = 256

.pattern0_1     BYTE    20,    $C5,    2*6+4 ; B3
                BYTE    20,    $C3,      2   ; |
                BYTE     0,      0,    1*6   ; -
                BYTE    27,    $C5,    2*6+4 ; F3#
                BYTE    27,    $C3,      2   ; |
                BYTE     0,      0,    1*6   ; -
                BYTE    20,    $C5,    2*6+4 ; B3
                BYTE    20,    $C3,      2   ; |
                BYTE     0,      0,    1*6   ; -
                BYTE    27,    $C5,    2*6+4 ; F3#
                BYTE    27,    $C3,      2   ; |
                BYTE     0,      0,    1*6   ; -
                BYTE    31,    $C5,    2*6   ; E3
                BYTE    27,    $C5,    1*6   ; F3#
                BYTE    24,    $C5,    1*6   ; G3#
                BYTE    23,    $C5,    1*6   ; A3
                BYTE    20,    $C5,    1*6   ; B3
                BYTE    18,    $C5,    1*6   ; C4#
                BYTE    16,    $C5,    1*6   ; D4#
                BYTE    15,    $C5,    3*6   ; E4
.pattern0_1_end BYTE     0,      0,    2*6   ; - time to start gameplay
                BYTE     0,      0,      0   ; - leave kernel
                BYTE    -1

NOTE_INTERMISSION_END_0 = .pattern0_0_end - .pattern0_0
NOTE_INTERMISSION_END_1 = .pattern0_1_end - .pattern0_1

;
; Ending music intro -------------------------------------------------
;
                SUBROUTINE

B5_EndingIntro_Track0:
.track0
                WORD    .pattern0_0
                WORD    .pattern1_0
                WORD    .pattern1_0
                WORD    .pattern1_0
                WORD    .pattern1_0
                WORD    .pattern1_0
                WORD    .pattern1_0
                WORD    .pattern0_2

B5_EndingIntro_Track1:
.track1
                WORD    .pattern0_1

                ;      AUDF   AUDC|V  Lenght

.pattern0_0     BYTE    20,    $C4,    1*5   ; B3
                BYTE    20,    $C1,      1   ; |
                BYTE    20,    $C4,    1*5   ; B3
                BYTE    20,    $C1,      1   ; |
                BYTE    20,    $C4,    1*5   ; B3
                BYTE    20,    $C3,      2   ; |
                BYTE    20,    $C2,      3   ; |
                BYTE    20,    $C1,      6   ; |
                BYTE     0,      0,    3*8+4 ; -
                BYTE    -1
.pattern1_0     BYTE    26,    $C4,    1*5   ; B3
                BYTE    26,    $C2,      1   ; |
                BYTE    26,    $C1,      2   ; |
                BYTE    31,    $C4,    1*5   ; G3#
                BYTE    31,    $C2,      1   ; |
                BYTE    31,    $C1,      2   ; |
                BYTE    -1

.pattern0_1     BYTE    15,    $C4,    1*5   ; E4
                BYTE    15,    $C1,      1   ; |
                BYTE    15,    $C4,    1*5   ; E4
                BYTE    15,    $C1,      1   ; |
                BYTE    15,    $C4,    1*5   ; E4
                BYTE    15,    $C3,      2   ; |
                BYTE    15,    $C2,      3   ; |
                BYTE    15,    $C1,      6   ; |

.pattern0_2     BYTE     0,      0,      0   ; - NPTimer = 256

;
; Ending music A -----------------------------------------------------
;
                SUBROUTINE

B5_EndingA_Track0:
.track0
                WORD    B5_Note_E3_2T
                WORD    B5_Note_B3_2T
                WORD    B5_Note_C4s_2T
                WORD    B5_Note_D4s_2T
                ;
                WORD    .pattern0_2
                WORD    .pattern0_0
                WORD    .pattern1_2

B5_EndingA_Track1:
.track1
                WORD    .pattern0_2
                ;
                WORD    B5_Note_D4s_2T
                WORD    B5_Note_E4_2T
                WORD    B5_Note_F4s_2T
                WORD    B5_Note_E4_2T
                WORD    B5_Note_B3_2T
                WORD    B5_Note_E4_2T
                WORD    B5_Note_B3_2T
                WORD    B5_Note_C4s_2T
                WORD    B5_Note_G3s_2T
                WORD    B5_Note_C4s_2T
                WORD    B5_Note_G3s_2T
                WORD    B5_Note_F3s_2T
                WORD    B5_Note_A3s_2T
                WORD    B5_Note_B3_2T
                WORD    B5_Note_F3s_2T
                ;
                WORD    .pattern1_2

                ;      AUDF   AUDC|V  Lenght

.pattern0_0     BYTE    18,    $46,    2*5   ; G5#
                BYTE    18,    $43,      2   ; |
                BYTE    18,    $42,      3   ; |
                BYTE     0,      0,    1*5   ; -
                BYTE    19,    $46,    1*5   ; G5
                BYTE    19,    $43,      2   ; |
                BYTE     0,      0,      3   ; -
                BYTE    18,    $46,    1*5   ; G5#
                BYTE    18,    $43,      2   ; |
                BYTE    18,    $42,      3   ; |
                BYTE     0,      0,    8*5   ; -
                BYTE    13,    $46,    2*5   ; C6#
                BYTE    13,    $43,      2   ; |
                BYTE    13,    $42,      3   ; |
                BYTE     0,      0,    1*5   ; -
                BYTE    15,    $46,    1*5   ; B5
                BYTE    15,    $43,      2   ; |
                BYTE     0,      0,      3   ; -
                BYTE    18,    $46,    1*5   ; G5#
                BYTE    18,    $43,      2   ; |
                BYTE    18,    $42,      3   ; |
                BYTE     0,      0,    4*5   ; -
                ;
                BYTE    20,    $46,    2*5   ; F5#
                BYTE    20,    $43,      2   ; |
                BYTE    20,    $42,      3   ; |
                BYTE     0,      0,    1*5   ; -
                BYTE    20,    $46,    2*5   ; F5#
                BYTE    20,    $43,      2   ; |
                BYTE    20,    $42,      3   ; |
                BYTE     0,      0,    1*5   ; -
                BYTE    15,    $46,    2*5   ; B5
                BYTE    15,    $43,      2   ; |
                BYTE     0,      0,      3   ; -
                BYTE    18,    $46,    1*5   ; G5#
                BYTE    20,    $46,    1*5   ; F5#
                BYTE    20,    $43,      2   ; |
                BYTE     0,      0,    1*5   ; -
                BYTE    23,    $46,    1*5   ; E3
                BYTE    23,    $43,      3   ; |
                BYTE    -1

.pattern0_2     BYTE     0,      0,    4*5   ; -
                BYTE    -1

.pattern1_2     BYTE    20,    $C4,    1*5   ; B3
                BYTE    20,    $C3,      2   ; |
                BYTE    20,    $C2,      3   ; |
                BYTE    18,    $C4,    1*5   ; C4#
                BYTE    18,    $C3,      2   ; |
                BYTE    18,    $C2,      3   ; |
                BYTE    18,    $C1,      5   ; |
                BYTE     0,      0,    3*5   ; -
                BYTE    17,    $C4,    1*5   ; D4
                BYTE    17,    $C3,      2   ; |
                BYTE    17,    $C2,      3   ; |
                BYTE    17,    $C1,      5   ; |
                BYTE     0,      0,      0   ; - NPTimer = 256

;
; Ending music B -----------------------------------------------------
;
                SUBROUTINE

B5_EndingB_Track0:
.track0
                WORD    B5_Note_B5_1T
                WORD    B5_Note_B5_1T
                WORD    B5_Note_A5_1T
                WORD    B5_Note_A5_1T
                WORD    B5_Note_G5s_1T
                WORD    B5_Note_G5s_1T
                WORD    B5_Note_F5s_1T
                WORD    B5_Note_F5s_1T
                ;
                WORD    .pattern0_0
                ;
                WORD    B5_Note_B5_1T
                WORD    B5_Note_C6s_1T
                WORD    B5_Note_B5_1T
                WORD    B5_Note_A5_1T
                WORD    B5_Note_G5s_1T
                WORD    B5_Note_B5_1T
                ;
                WORD    B5_Pattern_End

B5_EndingB_Track1:
.track1
                WORD    B5_Note_G5s_1T
                WORD    B5_Note_G5s_1T
                WORD    B5_Note_F5s_1T
                WORD    B5_Note_F5s_1T
                WORD    B5_Note_E5_1T
                WORD    B5_Note_E5_1T
                WORD    B5_Note_D5s_1T
                WORD    B5_Note_D5s_1T
                ;
                WORD    B5_Note_E4_2T
                WORD    B5_Note_B3_2T
                WORD    B5_Note_E4_2T
                WORD    B5_Note_B3_2T
                WORD    B5_Note_F4s_2T
                WORD    B5_Note_C4s_2T
                WORD    B5_Note_F4s_2T
                WORD    B5_Note_C4s_2T
                WORD    B5_Note_B3_2T
                WORD    B5_Note_F3s_2T
                WORD    B5_Note_B3_2T
                WORD    B5_Note_F3s_2T
                ;
                WORD    .pattern0_1
                ;
                WORD    B5_Pattern_End

                ;      AUDF   AUDC|V  Lenght

.pattern0_0     BYTE    23,    $46,    2*5   ; E5
                BYTE    23,    $43,      2   ; |
                BYTE    23,    $42,      3   ; |
                BYTE     0,      0,    3*5   ; -
                BYTE    23,    $46,    1*5   ; E5
                BYTE    20,    $46,    1*5   ; F5#
                BYTE    18,    $46,    2*5   ; G5#
                BYTE    18,    $43,      2   ; |
                BYTE    18,    $42,      3   ; |
                BYTE     0,      0,    1*5   ; -
                BYTE    23,    $46,    2*5   ; E5
                BYTE    23,    $43,      2   ; |
                BYTE    23,    $42,      3   ; |
                BYTE     0,      0,    1*5   ; -
                ;
                BYTE    20,    $46,    2*5   ; F5#
                BYTE    20,    $43,      2   ; |
                BYTE    20,    $42,      3   ; |
                BYTE     0,      0,    3*5   ; -
                BYTE    20,    $46,    1*5   ; F5#
                BYTE    18,    $46,    1*5   ; G5#
                BYTE    17,    $46,    2*5   ; A5
                BYTE    17,    $43,      2   ; |
                BYTE    17,    $42,      3   ; |
                BYTE     0,      0,    1*5   ; -
                BYTE    20,    $46,    2*5   ; F5#
                BYTE    20,    $43,      2   ; |
                BYTE    20,    $42,      3   ; |
                BYTE     0,      0,    1*5   ; -
                ;
                BYTE    15,    $46,    2*5   ; B5
                BYTE    15,    $43,      2   ; |
                BYTE    15,    $42,      3   ; |
                BYTE     0,      0,    3*5   ; -
                BYTE    -1

.pattern0_1     BYTE    31,    $C4,    1*5   ; E3
                BYTE    31,    $C3,      2   ; |
                BYTE    31,    $C2,      3   ; |
                BYTE    26,    $C4,    1*5   ; G3
                BYTE    26,    $C3,      2   ; |
                BYTE    26,    $C2,      3   ; |
                BYTE    26,    $C1,      5   ; |
                BYTE     0,      0,    3*5   ; -
                BYTE    31,    $C4,    1*5   ; E3
                BYTE    31,    $C3,      2   ; |
                BYTE    31,    $C2,      3   ; |
                BYTE    31,    $C1,      5   ; |
                BYTE    -1

;
; Ending music C -----------------------------------------------------
;

; Unused arcade ending tune. Variation of ending music B.

                SUBROUTINE

B5_EndingC_Track0:
.track0
                WORD    B5_Note_B5_1T
                WORD    B5_Note_B5_1T
                WORD    B5_Note_A5_1T
                WORD    B5_Note_A5_1T
                WORD    B5_Note_G5s_1T
                WORD    B5_Note_G5s_1T
                WORD    B5_Note_F5s_1T
                WORD    B5_Note_F5s_1T
                ;
                WORD    B5_Note_E5_1T
                WORD    B5_Note_E5_1T
                WORD    B5_Note_G5s_1T
                WORD    B5_Note_E5_1T
                WORD    B5_Note_G5s_1T
                WORD    B5_Note_E5_1T
                WORD    B5_Note_G5s_1T
                WORD    B5_Note_B5_1T
                WORD    B5_Note_C6s_2T
                WORD    B5_Note_B5_2T
                WORD    B5_Note_E6_2T
                ;
                WORD    B5_Pattern_End

B5_EndingC_Track1:
.track1
                WORD    B5_Note_G5s_1T
                WORD    B5_Note_G5s_1T
                WORD    B5_Note_F5s_1T
                WORD    B5_Note_F5s_1T
                WORD    B5_Note_E5_1T
                WORD    B5_Note_E5_1T
                WORD    B5_Note_D5s_1T
                WORD    B5_Note_D5s_1T
                ;
                WORD    B5_Note_E4_2T
                WORD    B5_Note_B3_2T
                WORD    B5_Note_E3_2T
                WORD    B5_Note_B3_2T
                WORD    B5_Note_A4_2T
                WORD    B5_Note_E4_2T
                WORD    B5_Note_E3_2T
                ;
                WORD    B5_Pattern_End

;
; Common notes and patterns ------------------------------------------------
;

                ;
                ; E3 major notes
                ;

                ;      AUDF   AUDC|V  Lenght

B5_Note_E3_2T:  BYTE    31,    $C4,    2*5   ; E3
                BYTE    31,    $C2,      2   ; |
                BYTE    31,    $C1,      3   ; |
                BYTE     0,      0,    1*5   ; -
                BYTE    -1

B5_Note_F3s_2T: BYTE    27,    $C4,    2*5   ; F3#
                BYTE    27,    $C2,      2   ; |
                BYTE    27,    $C1,      3   ; |
                BYTE     0,      0,    1*5   ; -
                BYTE    -1

B5_Note_G3s_2T  BYTE    24,    $C4,    2*5   ; G3#
                BYTE    24,    $C2,      2   ; |
                BYTE    24,    $C1,      3   ; |
                BYTE     0,      0,    1*5   ; -
                BYTE    -1

B5_Note_A3s_2T: BYTE    21,    $C4,    2*5   ; A3#
                BYTE    21,    $C2,      2   ; |
                BYTE    21,    $C1,      3   ; |
                BYTE     0,      0,    1*5   ; -
                BYTE    -1

B5_Note_B3_2T:  BYTE    20,    $C4,    2*5   ; B3
                BYTE    20,    $C2,      2   ; |
                BYTE    20,    $C1,      3   ; |
                BYTE     0,      0,    1*5   ; -
                BYTE    -1

B5_Note_C4s_2T: BYTE    18,    $C4,    2*5   ; C4#
                BYTE    18,    $C2,      2   ; |
                BYTE    18,    $C1,      3   ; |
                BYTE     0,      0,    1*5   ; -
                BYTE    -1

B5_Note_D4s_2T: BYTE    16,    $C4,    2*5   ; D4#
                BYTE    16,    $C2,      2   ; |
                BYTE    16,    $C1,      3   ; |
                BYTE     0,      0,    1*5   ; -
                BYTE    -1

B5_Note_E4_2T:  BYTE    15,    $C4,    2*5   ; E4
                BYTE    15,    $C2,      2   ; |
                BYTE    15,    $C1,      3   ; |
                BYTE     0,      0,    1*5   ; -
                BYTE    -1

B5_Note_F4s_2T: BYTE    13,    $C4,    2*5   ; F4#
                BYTE    13,    $C2,      2   ; |
                BYTE    13,    $C1,      3   ; |
                BYTE     0,      0,    1*5   ; -
                BYTE    -1

B5_Note_A4_2T:  BYTE    11,    $C4,    2*5   ; A4
                BYTE    11,    $C2,      2   ; |
                BYTE    11,    $C1,      3   ; |
                BYTE     0,      0,    1*5   ; -
                BYTE    -1

                ;
                ; E5 major notes
                ;

                ;      AUDF   AUDC|V  Lenght

B5_Note_D5s_1T: BYTE    24,    $46,    1*5   ; D5#
                BYTE    24,    $43,      2   ; |
                BYTE    24,    $42,      3   ; |
                BYTE    -1

B5_Note_E5_1T:  BYTE    23,    $46,    1*5   ; E5
                BYTE    23,    $43,      2   ; |
                BYTE    23,    $42,      3   ; |
                BYTE    -1

B5_Note_F5s_1T: BYTE    20,    $46,    1*5   ; F5#
                BYTE    20,    $43,      2   ; |
                BYTE    20,    $42,      3   ; |
                BYTE    -1

B5_Note_G5s_1T: BYTE    18,    $46,    1*5   ; G5#
                BYTE    18,    $43,      2   ; |
                BYTE    18,    $42,      3   ; |
                BYTE    -1

B5_Note_A5_1T:  BYTE    17,    $46,    1*5   ; A5
                BYTE    17,    $43,      2   ; |
                BYTE    17,    $42,      3   ; |
                BYTE    -1

B5_Note_B5_1T:  BYTE    15,    $46,    1*5   ; B5
                BYTE    15,    $43,      2   ; |
                BYTE    15,    $42,      3   ; |
                BYTE    -1

B5_Note_B5_2T:  BYTE    15,    $46,    2*5   ; B5
                BYTE    15,    $43,      2   ; |
                BYTE    15,    $42,      3   ; |
                BYTE     0,      0,    1*5   ; -
                BYTE    -1

B5_Note_C6s_1T: BYTE    13,    $46,    1*5   ; C6#
                BYTE    13,    $43,      2   ; |
                BYTE    13,    $42,      3   ; |
                BYTE    -1

B5_Note_C6s_2T: BYTE    13,    $46,    2*5   ; C6#
                BYTE    13,    $43,      2   ; |
                BYTE    13,    $42,      3   ; |
                BYTE     0,      0,    1*5   ; -
                BYTE    -1

B5_Note_E6_2T:  BYTE    11,    $46,    2*5   ; E6
                BYTE    11,    $43,      2   ; |
                BYTE    11,    $42,      3   ; |
                BYTE     0,      0,    1*5   ; -
                BYTE    -1

                ;
                ; End delay pattern
                ;

                ;      AUDF   AUDC|V  Lenght

B5_Pattern_End: BYTE     0,      0,      0   ; - NPTimer = 256

; --------------------------------------------------------------------
;       Sound
; --------------------------------------------------------------------

SOUND_INIT_GIRDER_IMPACT = AUDIO_SOUND_INIT | AUDIO_CHANNEL_2 |  0
SOUND_INIT_KONG_FALL     = AUDIO_SOUND_INIT | AUDIO_CHANNEL_2 |  1
SOUND_INIT_KONG_HIT      = AUDIO_SOUND_INIT | AUDIO_CHANNEL_2 |  2
SOUND_INIT_KONG_IMPACT   = AUDIO_SOUND_INIT | AUDIO_CHANNEL_2 |  3
SOUND_INIT_KONG_GRIN     = AUDIO_SOUND_INIT | AUDIO_CHANNEL_2 |  4 ; 4 - 6
SOUND_INIT_MARIO_WALK    = AUDIO_SOUND_INIT | AUDIO_CHANNEL_1 |  7
SOUND_INIT_MARIO_CLIMB   = AUDIO_SOUND_INIT | AUDIO_CHANNEL_1 |  7 ; same as walk
SOUND_INIT_MARIO_JUMP    = AUDIO_SOUND_INIT | AUDIO_CHANNEL_1 |  8
SOUND_INIT_MARIO_FALL    = AUDIO_SOUND_INIT | AUDIO_CHANNEL_1 |  9
SOUND_INIT_MARIO_HIT     = AUDIO_SOUND_INIT | AUDIO_CHANNEL_1 | 10
SOUND_INIT_MARIO_JINGLE  = AUDIO_SOUND_INIT | AUDIO_CHANNEL_1 | 11 ; 11 - 17

SOUND_INIT_HI_PRIORITY   = SOUND_INIT_MARIO_HIT

SOUND_PLAY_0             = AUDIO_SOUND_PLAY | AUDIO_CHANNEL_0
SOUND_PLAY_1             = AUDIO_SOUND_PLAY | AUDIO_CHANNEL_1
SOUND_PLAY_2             = AUDIO_SOUND_PLAY | AUDIO_CHANNEL_2
SOUND_PLAY               = SOUND_PLAY_2

                ;                 index   AUDC    AUDF    AUDV   lenght   addF  andF  orF  addV  spawn
                ; ------------------------------------------------------------------------------------
                ;
                MAKE_SOUND_EFFECT   00,     8,   0*8-1,  9*8-1,  4*8+0,  -128,  $FF,  $00,  -2,   -1    ; girder impact
                MAKE_SOUND_EFFECT   01,     4,  16*8-0,  8*8-0,  6*8+6,     2,  $FF,  $00,  -1,    2    ; Kong fall (1-2)
                MAKE_SOUND_EFFECT   02,    15,   0*8-1,  9*8-1,  4*8+3,     0,  $FF,  $00,  -2,   -1    ; Kong hit
                MAKE_SOUND_EFFECT   03,     8,   0*8-1,  9*8-1,  4*8+3,     0,  $FF,  $00,  -2,   -1    ; Kong impact
                MAKE_SOUND_EFFECT   04,     3,  13*8-1,  6*8-1,  5*8+0,     0,  $FF,  $00,  -1,    5    ; Kong grin (4-6)
                MAKE_SOUND_EFFECT   05,     3,  13*8-1,  6*8-1,  5*8+0,     0,  $FF,  $00,  -1,    6
                MAKE_SOUND_EFFECT   06,     3,  13*8-1,  6*8-1,  5*8+0,     0,  $FF,  $00,  -1,   -1
                MAKE_SOUND_EFFECT   07,    12,  17*8-1,  5*8-1,  1*8+0,    -8,  $FF,  $00,  -3,   -1    ; Mario walk
                MAKE_SOUND_EFFECT   08,    12,  16*8-1,  8*8-1,  2*8+5,   -10,  $38,  $40,  -3,   -1    ; Mario jump
                MAKE_SOUND_EFFECT   09,     4,  16*8-0,  8*8-0,  7*8+0,     2,  $FF,  $00,  -1,   -1    ; Mario fall
                MAKE_SOUND_EFFECT   10,    12,  31*8-1,  4*8-1, 12*8+2,    -2,  $FF,  $00,   0,   11    ; Mario hit
                MAKE_SOUND_EFFECT   11,     4,  23*8-0,  3*8-0,  0*8+4,     0,  $FF,  $00,   0,   12    ; Mario jingle (11-17)
                MAKE_SOUND_EFFECT   12,     4,  20*8-0,  3*8-0,  0*8+4,     0,  $FF,  $00,   0,   13
                MAKE_SOUND_EFFECT   13,     4,  18*8-0,  3*8-0,  0*8+4,     0,  $FF,  $00,   0,   14
                MAKE_SOUND_EFFECT   14,     4,  13*8-0,  3*8-0,  0*8+7,     0,  $FF,  $00,   0,   15
                MAKE_SOUND_EFFECT   15,     0,       0,      0,  0*8+1,     0,  $FF,  $00,   0,   16
                MAKE_SOUND_EFFECT   16,     4,  17*8-0,  3*8-0,  0*8+8,     0,  $FF,  $00,   0,   17
                MAKE_SOUND_EFFECT   17,     0,       0,      0,  0*8+1,     0,  $FF,  $00,   0,   -1

B5_SFXAudC:     MAKE_SOUND_EFFECT_TABLE AUDC
B5_SFXAudF:     MAKE_SOUND_EFFECT_TABLE AUDF
B5_SFXAudV:     MAKE_SOUND_EFFECT_TABLE AUDV
B5_SFXLength:   MAKE_SOUND_EFFECT_TABLE LENG
B5_SFXAddF:     MAKE_SOUND_EFFECT_TABLE ADDF
B5_SFXAndF:     MAKE_SOUND_EFFECT_TABLE ANDF
B5_SFXOrF:      MAKE_SOUND_EFFECT_TABLE ORAF
B5_SFXAddV:     MAKE_SOUND_EFFECT_TABLE ADDV
B5_SFXSpawn:    MAKE_SOUND_EFFECT_TABLE SPWN

; --------------------------------------------------------------------
;       Graphics
; --------------------------------------------------------------------

;
; Mario GRP
;

B5_MarioWalk0_GRP_0:

                BYTE    %01110111 ; | XXX XXX|  15
                BYTE    %00110011 ; |  XX  XX|  14
                BYTE    %00000000 ; |        |  13
                BYTE    %00000100 ; |     X  |  12
                BYTE    %00001100 ; |    XX  |  11
                BYTE    %00000111 ; |     XXX|  10
                BYTE    %00000011 ; |      XX|   9
                BYTE    %00110111 ; |  XX XXX|   8
                BYTE    %00011110 ; |   XXXX |   7
                BYTE    %00000000 ; |        |   6
                BYTE    %01110011 ; | XXX  XX|   5
                BYTE    %00101101 ; |  X XX X|   4
                BYTE    %00010101 ; |   X X X|   3
                BYTE    %00001110 ; |    XXX |   2
              ; BYTE    %00000000 ; |        |   1
              ; BYTE    %00000000 ; |        |   0

B5_MarioWalk0_GRP_1:

                BYTE    %00000000 ; |        |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %00110111 ; |  XX XXX|  13
                BYTE    %01111011 ; | XXXX XX|  12
                BYTE    %01110001 ; | XXX   X|  11
                BYTE    %01111000 ; | XXXX   |  10
                BYTE    %00111100 ; |  XXXX  |   9
                BYTE    %00001000 ; |    X   |   8
                BYTE    %00000000 ; |        |   7
                BYTE    %00111100 ; |  XXXX  |   6
                BYTE    %00001100 ; |    XX  |   5
                BYTE    %11010010 ; |XX X  X |   4
                BYTE    %01101010 ; | XX X X |   3
                BYTE    %00010000 ; |   X    |   2
                BYTE    %01111110 ; | XXXXXX |   1
                BYTE    %00011100 ; |   XXX  |   0

B5_MarioWalk1_GRP_0:

                BYTE    %00000110 ; |     XX |  15
                BYTE    %00000011 ; |      XX|  14
                BYTE    %10000011 ; |X     XX|  13
                BYTE    %10000000 ; |X       |  12
                BYTE    %10000000 ; |X       |  11
                BYTE    %10000000 ; |X       |  10
                BYTE    %10010001 ; |X  X   X|   9
                BYTE    %11000011 ; |XX    XX|   8
                BYTE    %10000001 ; |X      X|   7
                BYTE    %00000000 ; |        |   6
                BYTE    %01110011 ; | XXX  XX|   5
                BYTE    %00101101 ; |  X XX X|   4
                BYTE    %00010101 ; |   X X X|   3
                BYTE    %00001110 ; |    XXX |   2
              ; BYTE    %00000000 ; |        |   1
              ; BYTE    %00000000 ; |        |   0

B5_MarioWalk1_GRP_1:

                BYTE    %00000000 ; |        |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %01100100 ; | XX  X  |  13
                BYTE    %01111110 ; | XXXXXX |  12
                BYTE    %01111110 ; | XXXXXX |  11
                BYTE    %00111100 ; |  XXXX  |  10
                BYTE    %00101000 ; |  X X   |   9
                BYTE    %00011000 ; |   XX   |   8
                BYTE    %00111110 ; |  XXXXX |   7
                BYTE    %00111100 ; |  XXXX  |   6
                BYTE    %00001100 ; |    XX  |   5
                BYTE    %11010010 ; |XX X  X |   4
                BYTE    %01101010 ; | XX X X |   3
                BYTE    %00010000 ; |   X    |   2
                BYTE    %01111110 ; | XXXXXX |   1
                BYTE    %00011100 ; |   XXX  |   0

B5_MarioWalk2_GRP_0:

                BYTE    %00111000 ; |  XXX   |  15
                BYTE    %00011001 ; |   XX  X|  14
                BYTE    %00000001 ; |       X|  13
                BYTE    %00000001 ; |       X|  12
                BYTE    %00000001 ; |       X|  11
                BYTE    %00000100 ; |     X  |  10
                BYTE    %00011100 ; |   XXX  |   9
                BYTE    %00011100 ; |   XXX  |   8
                BYTE    %00000000 ; |        |   7
                BYTE    %01110011 ; | XXX  XX|   6
                BYTE    %00101101 ; |  X XX X|   5
                BYTE    %00010101 ; |   X X X|   4
                BYTE    %00001110 ; |    XXX |   3
                BYTE    %00000000 ; |        |   2
              ; BYTE    %00000000 ; |        |   1
              ; BYTE    %00000000 ; |        |   0

B5_MarioWalk2_GRP_1:

                BYTE    %00000000 ; |        |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %00011010 ; |   XX X |  13
                BYTE    %00111110 ; |  XXXXX |  12
                BYTE    %00111110 ; |  XXXXX |  11
                BYTE    %00100010 ; |  X   X |  10
                BYTE    %01100010 ; | XX   X |   9
                BYTE    %00100000 ; |  X     |   8
                BYTE    %00111100 ; |  XXXX  |   7
                BYTE    %00001100 ; |    XX  |   6
                BYTE    %11010010 ; |XX X  X |   5
                BYTE    %01101010 ; | XX X X |   4
                BYTE    %00010000 ; |   X    |   3
                BYTE    %01111110 ; | XXXXXX |   2
                BYTE    %00011100 ; |   XXX  |   1
              ; BYTE    %00000000 ; |        |   0

B5_MarioJump0_GRP_0:

                BYTE    %00000000 ; |        |  15
                BYTE    %00000001 ; |       X|  14
                BYTE    %10000011 ; |X     XX|  13
                BYTE    %10000010 ; |X     X |  12
                BYTE    %10000000 ; |X       |  11
                BYTE    %00010000 ; |   X    |  10
                BYTE    %00000010 ; |      X |   9
                BYTE    %10000011 ; |X     XX|   8
                BYTE    %10000000 ; |X       |   7
                BYTE    %01110011 ; | XXX  XX|   6
                BYTE    %00101101 ; |  X XX X|   5
                BYTE    %00010101 ; |   X X X|   4
                BYTE    %00001110 ; |    XXX |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

B5_MarioJump0_GRP_1:

                BYTE    %00000100 ; |     X  |  15
                BYTE    %00001110 ; |    XXX |  14
                BYTE    %01111100 ; | XXXXX  |  13
                BYTE    %01111100 ; | XXXXX  |  12
                BYTE    %00111100 ; |  XXXX  |  11
                BYTE    %00101000 ; |  X X   |  10
                BYTE    %00011000 ; |   XX   |   9
                BYTE    %01111100 ; | XXXXX  |   8
                BYTE    %00111100 ; |  XXXX  |   7
                BYTE    %00001100 ; |    XX  |   6
                BYTE    %11010010 ; |XX X  X |   5
                BYTE    %01101010 ; | XX X X |   4
                BYTE    %00010000 ; |   X    |   3
                BYTE    %01111110 ; | XXXXXX |   2
                BYTE    %00011100 ; |   XXX  |   1
                BYTE    %00000000 ; |        |   0

B5_MarioLand0_GRP_0:

                BYTE    %00101000 ; |  X X   |  15
                BYTE    %01111000 ; | XXXX   |  14
                BYTE    %11110000 ; |XXXX    |  13
                BYTE    %10100000 ; |X X     |  12
                BYTE    %00000000 ; |        |  11
                BYTE    %11000001 ; |XX     X|  10
                BYTE    %11010011 ; |XX X  XX|   9
                BYTE    %01101101 ; | XX XX X|   8
                BYTE    %00111111 ; |  XXXXXX|   7
                BYTE    %00000011 ; |      XX|   6
                BYTE    %01110010 ; | XXX  X |   5
                BYTE    %00101101 ; |  X XX X|   4
                BYTE    %00010101 ; |   X X X|   3
                BYTE    %00001110 ; |    XXX |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

B5_MarioLand0_GRP_1:

                BYTE    %00000101 ; |     X X|  15
                BYTE    %00000010 ; |      X |  14
                BYTE    %00001101 ; |    XX X|  13
                BYTE    %01011110 ; | X XXXX |  12
                BYTE    %00111110 ; |  XXXXX |  11
                BYTE    %00111110 ; |  XXXXX |  10
                BYTE    %00101000 ; |  X X   |   9
                BYTE    %00010000 ; |   X    |   8
                BYTE    %00001000 ; |    X   |   7
                BYTE    %00111100 ; |  XXXX  |   6
                BYTE    %00001100 ; |    XX  |   5
                BYTE    %11010010 ; |XX X  X |   4
                BYTE    %01101010 ; | XX X X |   3
                BYTE    %00010000 ; |   X    |   2
                BYTE    %01111110 ; | XXXXXX |   1
                BYTE    %00011100 ; |   XXX  |   0

B5_MarioClimb0_GRP_0:

                BYTE    %01111110 ; | XXXXXX |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %00000000 ; |        |  13
                BYTE    %00000000 ; |        |  12
                BYTE    %00000000 ; |        |  11
                BYTE    %10000001 ; |X      X|  10
                BYTE    %10000001 ; |X      X|   9
                BYTE    %11011011 ; |XX XX XX|   8
                BYTE    %01011010 ; | X XX X |   7
                BYTE    %00011000 ; |   XX   |   6
                BYTE    %00000000 ; |        |   5
                BYTE    %01111110 ; | XXXXXX |   4
                BYTE    %01111110 ; | XXXXXX |   3
                BYTE    %01111110 ; | XXXXXX |   2
                BYTE    %00100100 ; |  X  X  |   1
              ; BYTE    %00000000 ; |        |   0

B5_MarioClimb0_GRP_1:

                BYTE    %00000000 ; |        |  15
                BYTE    %00111100 ; |  XXXX  |  14
                BYTE    %01100110 ; | XX  XX |  13
                BYTE    %01111110 ; | XXXXXX |  12
                BYTE    %01111110 ; | XXXXXX |  11
                BYTE    %01111110 ; | XXXXXX |  10
                BYTE    %00111100 ; |  XXXX  |   9
                BYTE    %00100100 ; |  X  X  |   8
                BYTE    %00100100 ; |  X  X  |   7
                BYTE    %00100100 ; |  X  X  |   6
                BYTE    %00111100 ; |  XXXX  |   5
                BYTE    %00000000 ; |        |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00011000 ; |   XX   |   1
                BYTE    %00111100 ; |  XXXX  |   0

B5_MarioClimb1_GRP_0:

                BYTE    %00011100 ; |   XXX  |  15
                BYTE    %00011110 ; |   XXXX |  14
                BYTE    %00000100 ; |     X  |  13
                BYTE    %01110000 ; | XXX    |  12
                BYTE    %00000000 ; |        |  11
                BYTE    %00000000 ; |        |  10
                BYTE    %00000000 ; |        |   9
                BYTE    %00000000 ; |        |   8
                BYTE    %01011011 ; | X XX XX|   7
                BYTE    %01011011 ; | X XX XX|   6
                BYTE    %11000010 ; |XX    X |   5
                BYTE    %11111110 ; |XXXXXXX |   4
                BYTE    %11111110 ; |XXXXXXX |   3
                BYTE    %01100100 ; | XX  X  |   2
                BYTE    %01000000 ; | X      |   1
                BYTE    %01000000 ; | X      |   0

B5_MarioClimb1_GRP_1:

                BYTE    %00000000 ; |        |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %00001010 ; |    X X |  13
                BYTE    %00000110 ; |     XX |  12
                BYTE    %01111111 ; | XXXXXXX|  11
                BYTE    %11111111 ; |XXXXXXXX|  10
                BYTE    %11111111 ; |XXXXXXXX|   9
                BYTE    %01111110 ; | XXXXXX |   8
                BYTE    %00100100 ; |  X  X  |   7
                BYTE    %00100100 ; |  X  X  |   6
                BYTE    %00111100 ; |  XXXX  |   5
                BYTE    %00000000 ; |        |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00011000 ; |   XX   |   2
                BYTE    %00111100 ; |  XXXX  |   1
                BYTE    %00000000 ; |        |   0

B5_MarioClimb2_GRP_0:

                BYTE    %00011000 ; |   XX   |  15
                BYTE    %00011100 ; |   XXX  |  14
                BYTE    %00011100 ; |   XXX  |  13
                BYTE    %00001000 ; |    X   |  12
                BYTE    %01100000 ; | XX     |  11
                BYTE    %01100000 ; | XX     |  10
                BYTE    %00000000 ; |        |   9
                BYTE    %00000000 ; |        |   8
                BYTE    %00000000 ; |        |   7
                BYTE    %00000000 ; |        |   6
                BYTE    %10000000 ; |X       |   5
                BYTE    %10000001 ; |X      X|   4
                BYTE    %01011010 ; | X XX X |   3
                BYTE    %01011010 ; | X XX X |   2
                BYTE    %00100000 ; |  X     |   1
              ; BYTE    %00000000 ; |        |   0

B5_MarioClimb2_GRP_1:

                BYTE    %00000000 ; |        |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %00000000 ; |        |  13
                BYTE    %00010100 ; |   X X  |  12
                BYTE    %00011110 ; |   XXXX |  11
                BYTE    %00011110 ; |   XXXX |  10
                BYTE    %01101110 ; | XX XXX |   9
                BYTE    %11111111 ; |XXXXXXXX|   8
                BYTE    %11111111 ; |XXXXXXXX|   7
                BYTE    %11111111 ; |XXXXXXXX|   6
                BYTE    %01111111 ; | XXXXXXX|   5
                BYTE    %00111110 ; |  XXXXX |   4
                BYTE    %00100100 ; |  X  X  |   3
                BYTE    %00100100 ; |  X  X  |   2
                BYTE    %00011100 ; |   XXX  |   1
              ; BYTE    %00000000 ; |        |   0

B5_MarioClimb3_GRP_0:

                BYTE    %00000000 ; |        |  15
                BYTE    %00001100 ; |    XX  |  14
                BYTE    %00001110 ; |    XXX |  13
                BYTE    %00000100 ; |     X  |  12
                BYTE    %00000000 ; |        |  11
                BYTE    %00000000 ; |        |  10
                BYTE    %00110000 ; |  XX    |   9
                BYTE    %01000001 ; | X     X|   8
                BYTE    %00000001 ; |       X|   7
                BYTE    %00000001 ; |       X|   6
                BYTE    %00000001 ; |       X|   5
                BYTE    %00000010 ; |      X |   4
                BYTE    %00001000 ; |    X   |   3
              ; BYTE    %00000000 ; |        |   2
              ; BYTE    %00000000 ; |        |   1
              ; BYTE    %00000000 ; |        |   0

B5_MarioClimb3_GRP_1:

                BYTE    %00000000 ; |        |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %00000000 ; |        |  13
                BYTE    %00001010 ; |    X X |  12
                BYTE    %00001110 ; |    XXX |  11
                BYTE    %00000110 ; |     XX |  10
                BYTE    %00001110 ; |    XXX |   9
                BYTE    %00101110 ; |  X XXX |   8
                BYTE    %11111110 ; |XXXXXXX |   7
                BYTE    %11111110 ; |XXXXXXX |   6
                BYTE    %01111110 ; | XXXXXX |   5
                BYTE    %01111100 ; | XXXXX  |   4
                BYTE    %00110100 ; |  XX X  |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
              ; BYTE    %00000000 ; |        |   0

B5_MarioDead0_GRP_0:

                BYTE    %00000000 ; |        |  15
                BYTE    %01100010 ; | XX   X |  14
                BYTE    %11100011 ; |XXX   XX|  13
                BYTE    %11000011 ; |XX    XX|  12
                BYTE    %00000001 ; |       X|  11
                BYTE    %00000000 ; |        |  10
                BYTE    %00100100 ; |  X  X  |   9
                BYTE    %01011010 ; | X XX X |   8
                BYTE    %11011011 ; |XX XX XX|   7
                BYTE    %00001000 ; |    X   |   6
                BYTE    %01011100 ; | X XXX  |   5
                BYTE    %01010101 ; | X X X X|   4
                BYTE    %01000001 ; | X     X|   3
                BYTE    %00010100 ; |   X X  |   2
              ; BYTE    %00000000 ; |        |   1
              ; BYTE    %00000000 ; |        |   0

B5_MarioDead0_GRP_1:

                BYTE    %00000000 ; |        |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %00010100 ; |   X X  |  13
                BYTE    %00111100 ; |  XXXX  |  12
                BYTE    %01111110 ; | XXXXXX |  11
                BYTE    %00111100 ; |  XXXX  |  10
                BYTE    %00011000 ; |   XX   |   9
                BYTE    %00100100 ; |  X  X  |   8
                BYTE    %00100100 ; |  X  X  |   7
                BYTE    %10010101 ; |X  X X X|   6
                BYTE    %10100011 ; |X X   XX|   5
                BYTE    %00101010 ; |  X X X |   4
                BYTE    %00111110 ; |  XXXXX |   3
                BYTE    %00101010 ; |  X X X |   2
                BYTE    %00111110 ; |  XXXXX |   1
                BYTE    %01011100 ; | X XXX  |   0

B5_MarioDead1_GRP_0:

                BYTE    %00000000 ; |        |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %00010100 ; |   X X  |  13
                BYTE    %01000001 ; | X     X|  12
                BYTE    %01010101 ; | X X X X|  11
                BYTE    %01011100 ; | X XXX  |  10
                BYTE    %00001000 ; |    X   |   9
                BYTE    %11011011 ; |XX XX XX|   8
                BYTE    %01011010 ; | X XX X |   7
                BYTE    %00100100 ; |  X  X  |   6
                BYTE    %00000000 ; |        |   5
                BYTE    %00000001 ; |       X|   4
                BYTE    %11000011 ; |XX    XX|   3
                BYTE    %11100011 ; |XXX   XX|   2
                BYTE    %01100010 ; | XX   X |   1
                BYTE    %00000000 ; |        |   0

B5_MarioDead1_GRP_1:

                BYTE    %01011100 ; | X XXX  |  15
                BYTE    %00111110 ; |  XXXXX |  14
                BYTE    %00101010 ; |  X X X |  13
                BYTE    %00111110 ; |  XXXXX |  12
                BYTE    %00101010 ; |  X X X |  11
                BYTE    %10100011 ; |X X   XX|  10
                BYTE    %10010101 ; |X  X X X|   9
                BYTE    %00100100 ; |  X  X  |   8
                BYTE    %00100100 ; |  X  X  |   7
                BYTE    %00011000 ; |   XX   |   6
                BYTE    %00111100 ; |  XXXX  |   5
                BYTE    %01111110 ; | XXXXXX |   4
                BYTE    %00111100 ; |  XXXX  |   3
                BYTE    %00010100 ; |   X X  |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

B5_MarioDead2_GRP_0:

                BYTE    %01000000 ; | X      |  15
                BYTE    %11010000 ; |XX X    |  14
                BYTE    %11010100 ; |XX X X  |  13
                BYTE    %10010110 ; |X  X XX |  12
                BYTE    %00001100 ; |    XX  |  11
                BYTE    %00100110 ; |  X  XX |  10
                BYTE    %00000100 ; |     X  |   9
                BYTE    %00001110 ; |    XXX |   8
                BYTE    %00000100 ; |     X  |   7
                BYTE    %00100110 ; |  X  XX |   6
                BYTE    %00001100 ; |    XX  |   5
                BYTE    %11010110 ; |XX X XX |   4
                BYTE    %11010100 ; |XX X X  |   3
                BYTE    %01010000 ; | X X    |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

B5_MarioDead2_GRP_1:

                BYTE    %00001000 ; |    X   |  15
                BYTE    %00001000 ; |    X   |  14
                BYTE    %00000000 ; |        |  13
                BYTE    %01000000 ; | X      |  12
                BYTE    %01100001 ; | XX    X|  11
                BYTE    %01010001 ; | X X   X|  10
                BYTE    %01100001 ; | XX    X|   9
                BYTE    %01100001 ; | XX    X|   8
                BYTE    %01100001 ; | XX    X|   7
                BYTE    %01010001 ; | X X   X|   6
                BYTE    %00100001 ; |  X    X|   5
                BYTE    %00000000 ; |        |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00001000 ; |    X   |   2
                BYTE    %00001000 ; |    X   |   1
                BYTE    %00001000 ; |    X   |   0

B5_MarioDead3_GRP_0:

                BYTE    %11000000 ; |XX      |  15
                BYTE    %11001110 ; |XX  XXX |  14
                BYTE    %01001100 ; | X  XX  |  13
                BYTE    %01001010 ; | X  X X |  12
                BYTE    %00001010 ; |    X X |  11
                BYTE    %00001010 ; |    X X |  10
                BYTE    %00100010 ; |  X   X |   9
                BYTE    %00000010 ; |      X |   8
                BYTE    %00000000 ; |        |   7
                BYTE    %00001000 ; |    X   |   6
                BYTE    %00111000 ; |  XXX   |   5
                BYTE    %00110000 ; |  XX    |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

B5_MarioDead3_GRP_1:

                BYTE    %00111111 ; |  XXXXXX|  15
                BYTE    %00110001 ; |  XX   X|  14
                BYTE    %00110011 ; |  XX  XX|  13
                BYTE    %00100001 ; |  X    X|  12
                BYTE    %11110001 ; |XXXX   X|  11
                BYTE    %11100001 ; |XXX    X|  10
                BYTE    %11000001 ; |XX     X|   9
                BYTE    %11100000 ; |XXX     |   8
                BYTE    %01100000 ; | XX     |   7
                BYTE    %01100000 ; | XX     |   6
                BYTE    %01000000 ; | X      |   5
                BYTE    %00000110 ; |     XX |   4
                BYTE    %00001001 ; |    X  X|   3
                BYTE    %00001001 ; |    X  X|   2
                BYTE    %00001001 ; |    X  X|   1
                BYTE    %00000110 ; |     XX |   0

;
; Mario COL
;

B5_MarioWalk0_COL_0:

                BYTE    COL_82 ; |181AA7|  15
                BYTE    COL_82 ; |181AA7|  14
                BYTE    COL_00 ; |000000|  13
                BYTE    COL_3E ; |FCBC74|  12
                BYTE    COL_3E ; |FCBC74|  11
                BYTE    COL_82 ; |181AA7|  10
                BYTE    COL_82 ; |181AA7|   9
                BYTE    COL_82 ; |181AA7|   8
                BYTE    COL_82 ; |181AA7|   7
                BYTE    COL_00 ; |000000|   6
                BYTE    COL_82 ; |181AA7|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_82 ; |181AA7|   2
              ; BYTE    COL_00 ; |000000|   1
              ; BYTE    COL_00 ; |000000|   0

B5_MarioWalk0_COL_1:

                BYTE    COL_00 ; |000000|  15
                BYTE    COL_00 ; |000000|  14
                BYTE    COL_42 ; |A71A1A|  13
                BYTE    COL_42 ; |A71A1A|  12
                BYTE    COL_42 ; |A71A1A|  11
                BYTE    COL_42 ; |A71A1A|  10
                BYTE    COL_42 ; |A71A1A|   9
                BYTE    COL_42 ; |A71A1A|   8
                BYTE    COL_00 ; |000000|   7
                BYTE    COL_3E ; |FCBC74|   6
                BYTE    COL_3E ; |FCBC74|   5
                BYTE    COL_3E ; |FCBC74|   4
                BYTE    COL_3E ; |FCBC74|   3
                BYTE    COL_3E ; |FCBC74|   2
                BYTE    COL_42 ; |A71A1A|   1
                BYTE    COL_42 ; |A71A1A|   0

B5_MarioWalk1_COL_0:

                BYTE    COL_82 ; |181AA7|  15
                BYTE    COL_82 ; |181AA7|  14
                BYTE    COL_82 ; |181AA7|  13
                BYTE    COL_82 ; |181AA7|  12
                BYTE    COL_82 ; |181AA7|  11
                BYTE    COL_82 ; |181AA7|  10
                BYTE    COL_3E ; |FCBC74|   9
                BYTE    COL_3E ; |FCBC74|   8
                BYTE    COL_3E ; |FCBC74|   7
                BYTE    COL_00 ; |000000|   6
                BYTE    COL_82 ; |181AA7|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_82 ; |181AA7|   2
              ; BYTE    COL_00 ; |000000|   1
              ; BYTE    COL_00 ; |000000|   0

B5_MarioWalk1_COL_1:

                BYTE    COL_00 ; |000000|  15
                BYTE    COL_00 ; |000000|  14
                BYTE    COL_42 ; |A71A1A|  13
                BYTE    COL_42 ; |A71A1A|  12
                BYTE    COL_42 ; |A71A1A|  11
                BYTE    COL_42 ; |A71A1A|  10
                BYTE    COL_42 ; |A71A1A|   9
                BYTE    COL_42 ; |A71A1A|   8
                BYTE    COL_82 ; |181AA7|   7
                BYTE    COL_3E ; |FCBC74|   6
                BYTE    COL_3E ; |FCBC74|   5
                BYTE    COL_3E ; |FCBC74|   4
                BYTE    COL_3E ; |FCBC74|   3
                BYTE    COL_3E ; |FCBC74|   2
                BYTE    COL_42 ; |A71A1A|   1
                BYTE    COL_42 ; |A71A1A|   0

B5_MarioWalk2_COL_0:

                BYTE    COL_82 ; |181AA7|  15
                BYTE    COL_82 ; |181AA7|  14
                BYTE    COL_82 ; |181AA7|  13
                BYTE    COL_82 ; |181AA7|  12
                BYTE    COL_82 ; |181AA7|  11
                BYTE    COL_42 ; |A71A1A|  10
                BYTE    COL_82 ; |181AA7|   9
                BYTE    COL_82 ; |181AA7|   8
                BYTE    COL_00 ; |000000|   7
                BYTE    COL_82 ; |181AA7|   6
                BYTE    COL_82 ; |181AA7|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_00 ; |000000|   2
              ; BYTE    COL_00 ; |000000|   1
              ; BYTE    COL_00 ; |000000|   0

B5_MarioWalk2_COL_1:

                BYTE    COL_00 ; |000000|  15
                BYTE    COL_00 ; |000000|  14
                BYTE    COL_42 ; |A71A1A|  13
                BYTE    COL_42 ; |A71A1A|  12
                BYTE    COL_42 ; |A71A1A|  11
                BYTE    COL_3E ; |FCBC74|  10
                BYTE    COL_3E ; |FCBC74|   9
                BYTE    COL_3E ; |FCBC74|   8
                BYTE    COL_3E ; |FCBC74|   7
                BYTE    COL_3E ; |FCBC74|   6
                BYTE    COL_3E ; |FCBC74|   5
                BYTE    COL_3E ; |FCBC74|   4
                BYTE    COL_3E ; |FCBC74|   3
                BYTE    COL_42 ; |A71A1A|   2
                BYTE    COL_42 ; |A71A1A|   1
              ; BYTE    COL_00 ; |000000|   0

B5_MarioJump0_COL_0:

                BYTE    COL_00 ; |000000|  15
                BYTE    COL_82 ; |181AA7|  14
                BYTE    COL_82 ; |181AA7|  13
                BYTE    COL_82 ; |181AA7|  12
                BYTE    COL_82 ; |181AA7|  11
                BYTE    COL_3E ; |FCBC74|  10
                BYTE    COL_3E ; |FCBC74|   9
                BYTE    COL_3E ; |FCBC74|   8
                BYTE    COL_3E ; |FCBC74|   7
                BYTE    COL_82 ; |181AA7|   6
                BYTE    COL_82 ; |181AA7|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_00 ; |000000|   2
                BYTE    COL_00 ; |000000|   1
                BYTE    COL_00 ; |000000|   0

B5_MarioJump0_COL_1:

                BYTE    COL_42 ; |A71A1A|  15
                BYTE    COL_42 ; |A71A1A|  14
                BYTE    COL_42 ; |A71A1A|  13
                BYTE    COL_42 ; |A71A1A|  12
                BYTE    COL_42 ; |A71A1A|  11
                BYTE    COL_42 ; |A71A1A|  10
                BYTE    COL_42 ; |A71A1A|   9
                BYTE    COL_82 ; |181AA7|   8
                BYTE    COL_3E ; |FCBC74|   7
                BYTE    COL_3E ; |FCBC74|   6
                BYTE    COL_3E ; |FCBC74|   5
                BYTE    COL_3E ; |FCBC74|   4
                BYTE    COL_3E ; |FCBC74|   3
                BYTE    COL_42 ; |A71A1A|   2
                BYTE    COL_42 ; |A71A1A|   1
                BYTE    COL_00 ; |000000|   0

B5_MarioLand0_COL_0:

                BYTE    COL_82 ; |181AA7|  15
                BYTE    COL_82 ; |181AA7|  14
                BYTE    COL_82 ; |181AA7|  13
                BYTE    COL_82 ; |181AA7|  12
                BYTE    COL_00 ; |000000|  11
                BYTE    COL_3E ; |FCBC74|  10
                BYTE    COL_3E ; |FCBC74|   9
                BYTE    COL_82 ; |181AA7|   8
                BYTE    COL_82 ; |181AA7|   7
                BYTE    COL_82 ; |181AA7|   6
                BYTE    COL_82 ; |181AA7|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_82 ; |181AA7|   2
                BYTE    COL_00 ; |000000|   1
                BYTE    COL_00 ; |000000|   0

B5_MarioLand0_COL_1:

                BYTE    COL_3E ; |FCBC74|  15
                BYTE    COL_3E ; |FCBC74|  14
                BYTE    COL_42 ; |A71A1A|  13
                BYTE    COL_42 ; |A71A1A|  12
                BYTE    COL_42 ; |A71A1A|  11
                BYTE    COL_42 ; |A71A1A|  10
                BYTE    COL_42 ; |A71A1A|   9
                BYTE    COL_42 ; |A71A1A|   8
                BYTE    COL_42 ; |A71A1A|   7
                BYTE    COL_3E ; |FCBC74|   6
                BYTE    COL_3E ; |FCBC74|   5
                BYTE    COL_3E ; |FCBC74|   4
                BYTE    COL_3E ; |FCBC74|   3
                BYTE    COL_3E ; |FCBC74|   2
                BYTE    COL_42 ; |A71A1A|   1
                BYTE    COL_42 ; |A71A1A|   0

B5_MarioClimb0_COL_0:

                BYTE    COL_82 ; |181AA7|  15
                BYTE    COL_00 ; |000000|  14
                BYTE    COL_00 ; |000000|  13
                BYTE    COL_00 ; |000000|  12
                BYTE    COL_00 ; |000000|  11
                BYTE    COL_3E ; |FCBC74|  10
                BYTE    COL_3E ; |FCBC74|   9
                BYTE    COL_82 ; |181AA7|   8
                BYTE    COL_82 ; |181AA7|   7
                BYTE    COL_82 ; |181AA7|   6
                BYTE    COL_00 ; |000000|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_82 ; |181AA7|   2
                BYTE    COL_82 ; |181AA7|   1
              ; BYTE    COL_00 ; |000000|   0

B5_MarioClimb0_COL_1:

                BYTE    COL_00 ; |000000|  15
                BYTE    COL_42 ; |A71A1A|  14
                BYTE    COL_42 ; |A71A1A|  13
                BYTE    COL_42 ; |A71A1A|  12
                BYTE    COL_42 ; |A71A1A|  11
                BYTE    COL_42 ; |A71A1A|  10
                BYTE    COL_42 ; |A71A1A|   9
                BYTE    COL_42 ; |A71A1A|   8
                BYTE    COL_42 ; |A71A1A|   7
                BYTE    COL_42 ; |A71A1A|   6
                BYTE    COL_3E ; |FCBC74|   5
                BYTE    COL_00 ; |000000|   4
                BYTE    COL_00 ; |000000|   3
                BYTE    COL_00 ; |000000|   2
                BYTE    COL_42 ; |A71A1A|   1
                BYTE    COL_42 ; |A71A1A|   0

B5_MarioClimb1_COL_0:

                BYTE    COL_82 ; |181AA7|  15
                BYTE    COL_82 ; |181AA7|  14
                BYTE    COL_82 ; |181AA7|  13
                BYTE    COL_82 ; |181AA7|  12
                BYTE    COL_00 ; |000000|  11
                BYTE    COL_00 ; |000000|  10
                BYTE    COL_00 ; |000000|   9
                BYTE    COL_00 ; |000000|   8
                BYTE    COL_82 ; |181AA7|   7
                BYTE    COL_82 ; |181AA7|   6
                BYTE    COL_82 ; |181AA7|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_82 ; |181AA7|   2
                BYTE    COL_3E ; |FCBC74|   1
                BYTE    COL_3E ; |FCBC74|   0

B5_MarioClimb1_COL_1:

                BYTE    COL_00 ; |000000|  15
                BYTE    COL_00 ; |000000|  14
                BYTE    COL_42 ; |A71A1A|  13
                BYTE    COL_42 ; |A71A1A|  12
                BYTE    COL_42 ; |A71A1A|  11
                BYTE    COL_42 ; |A71A1A|  10
                BYTE    COL_42 ; |A71A1A|   9
                BYTE    COL_42 ; |A71A1A|   8
                BYTE    COL_42 ; |A71A1A|   7
                BYTE    COL_42 ; |A71A1A|   6
                BYTE    COL_3E ; |FCBC74|   5
                BYTE    COL_00 ; |000000|   4
                BYTE    COL_00 ; |000000|   3
                BYTE    COL_42 ; |A71A1A|   2
                BYTE    COL_42 ; |A71A1A|   1
                BYTE    COL_00 ; |000000|   0

B5_MarioClimb2_COL_0:

                BYTE    COL_82 ; |181AA7|  15
                BYTE    COL_82 ; |181AA7|  14
                BYTE    COL_82 ; |181AA7|  13
                BYTE    COL_82 ; |181AA7|  12
                BYTE    COL_82 ; |181AA7|  11
                BYTE    COL_82 ; |181AA7|  10
                BYTE    COL_00 ; |000000|   9
                BYTE    COL_00 ; |000000|   8
                BYTE    COL_00 ; |000000|   7
                BYTE    COL_00 ; |000000|   6
                BYTE    COL_3E ; |FCBC74|   5
                BYTE    COL_3E ; |FCBC74|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_82 ; |181AA7|   2
                BYTE    COL_82 ; |181AA7|   1
              ; BYTE    COL_00 ; |000000|   0

B5_MarioClimb2_COL_1:

                BYTE    COL_00 ; |000000|  15
                BYTE    COL_00 ; |000000|  14
                BYTE    COL_00 ; |000000|  13
                BYTE    COL_42 ; |A71A1A|  12
                BYTE    COL_42 ; |A71A1A|  11
                BYTE    COL_42 ; |A71A1A|  10
                BYTE    COL_42 ; |A71A1A|   9
                BYTE    COL_42 ; |A71A1A|   8
                BYTE    COL_42 ; |A71A1A|   7
                BYTE    COL_42 ; |A71A1A|   6
                BYTE    COL_42 ; |A71A1A|   5
                BYTE    COL_42 ; |A71A1A|   4
                BYTE    COL_42 ; |A71A1A|   3
                BYTE    COL_42 ; |A71A1A|   2
                BYTE    COL_3E ; |FCBC74|   1
              ; BYTE    COL_00 ; |000000|   0

B5_MarioClimb3_COL_0:

                BYTE    COL_00 ; |000000|  15
                BYTE    COL_82 ; |181AA7|  14
                BYTE    COL_82 ; |181AA7|  13
                BYTE    COL_82 ; |181AA7|  12
                BYTE    COL_00 ; |000000|  11
                BYTE    COL_00 ; |000000|  10
                BYTE    COL_82 ; |181AA7|   9
                BYTE    COL_3E ; |FCBC74|   8
                BYTE    COL_82 ; |181AA7|   7
                BYTE    COL_82 ; |181AA7|   6
                BYTE    COL_82 ; |181AA7|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_82 ; |181AA7|   3
              ; BYTE    COL_00 ; |000000|   2
              ; BYTE    COL_00 ; |000000|   1
              ; BYTE    COL_00 ; |000000|   0

B5_MarioClimb3_COL_1:

                BYTE    COL_00 ; |000000|  15
                BYTE    COL_00 ; |000000|  14
                BYTE    COL_00 ; |000000|  13
                BYTE    COL_42 ; |A71A1A|  12
                BYTE    COL_42 ; |A71A1A|  11
                BYTE    COL_42 ; |A71A1A|  10
                BYTE    COL_42 ; |A71A1A|   9
                BYTE    COL_42 ; |A71A1A|   8
                BYTE    COL_42 ; |A71A1A|   7
                BYTE    COL_42 ; |A71A1A|   6
                BYTE    COL_42 ; |A71A1A|   5
                BYTE    COL_42 ; |A71A1A|   4
                BYTE    COL_42 ; |A71A1A|   3
                BYTE    COL_00 ; |000000|   2
                BYTE    COL_00 ; |000000|   1
              ; BYTE    COL_00 ; |000000|   0

B5_MarioDead0_COL_0:

                BYTE    COL_00 ; |000000|  15
                BYTE    COL_82 ; |181AA7|  14
                BYTE    COL_82 ; |181AA7|  13
                BYTE    COL_82 ; |181AA7|  12
                BYTE    COL_82 ; |181AA7|  11
                BYTE    COL_00 ; |000000|  10
                BYTE    COL_3E ; |FCBC74|   9
                BYTE    COL_82 ; |181AA7|   8
                BYTE    COL_82 ; |181AA7|   7
                BYTE    COL_42 ; |A71A1A|   6
                BYTE    COL_82 ; |181AA7|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_82 ; |181AA7|   2
              ; BYTE    COL_00 ; |000000|   1
              ; BYTE    COL_00 ; |000000|   0

B5_MarioDead0_COL_1:

                BYTE    COL_00 ; |000000|  15
                BYTE    COL_00 ; |000000|  14
                BYTE    COL_42 ; |A71A1A|  13
                BYTE    COL_42 ; |A71A1A|  12
                BYTE    COL_42 ; |A71A1A|  11
                BYTE    COL_42 ; |A71A1A|  10
                BYTE    COL_42 ; |A71A1A|   9
                BYTE    COL_42 ; |A71A1A|   8
                BYTE    COL_42 ; |A71A1A|   7
                BYTE    COL_3E ; |FCBC74|   6
                BYTE    COL_3E ; |FCBC74|   5
                BYTE    COL_3E ; |FCBC74|   4
                BYTE    COL_3E ; |FCBC74|   3
                BYTE    COL_3E ; |FCBC74|   2
                BYTE    COL_42 ; |A71A1A|   1
                BYTE    COL_42 ; |A71A1A|   0

B5_MarioDead1_COL_0:

                BYTE    COL_00 ; |000000|  15
                BYTE    COL_00 ; |000000|  14
                BYTE    COL_82 ; |181AA7|  13
                BYTE    COL_82 ; |181AA7|  12
                BYTE    COL_82 ; |181AA7|  11
                BYTE    COL_82 ; |181AA7|  10
                BYTE    COL_42 ; |A71A1A|   9
                BYTE    COL_82 ; |181AA7|   8
                BYTE    COL_82 ; |181AA7|   7
                BYTE    COL_3E ; |FCBC74|   6
                BYTE    COL_00 ; |000000|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_82 ; |181AA7|   2
                BYTE    COL_82 ; |181AA7|   1
                BYTE    COL_00 ; |000000|   0

B5_MarioDead1_COL_1:

                BYTE    COL_42 ; |A71A1A|  15
                BYTE    COL_42 ; |A71A1A|  14
                BYTE    COL_3E ; |FCBC74|  13
                BYTE    COL_3E ; |FCBC74|  12
                BYTE    COL_3E ; |FCBC74|  11
                BYTE    COL_3E ; |FCBC74|  10
                BYTE    COL_3E ; |FCBC74|   9
                BYTE    COL_42 ; |A71A1A|   8
                BYTE    COL_42 ; |A71A1A|   7
                BYTE    COL_42 ; |A71A1A|   6
                BYTE    COL_42 ; |A71A1A|   5
                BYTE    COL_42 ; |A71A1A|   4
                BYTE    COL_42 ; |A71A1A|   3
                BYTE    COL_42 ; |A71A1A|   2
                BYTE    COL_00 ; |000000|   1
                BYTE    COL_00 ; |000000|   0

;
; Kong GRP
;

B5_KongThrow_GRP0_0:

                BYTE    %00001110 ; |    XXX |  31
                BYTE    %00011111 ; |   XXXXX|  30
                BYTE    %00011111 ; |   XXXXX|  29
                BYTE    %00001111 ; |    XXXX|  28
                BYTE    %00000111 ; |     XXX|  27
                BYTE    %00000110 ; |     XX |  26
                BYTE    %00011111 ; |   XXXXX|  25
                BYTE    %00011110 ; |   XXXX |  24
                BYTE    %00000000 ; |        |  23
                BYTE    %00000000 ; |        |  22
                BYTE    %00000000 ; |        |  21
                BYTE    %00000000 ; |        |  20
                BYTE    %00000000 ; |        |  19
                BYTE    %00000000 ; |        |  18
                BYTE    %00000000 ; |        |  17
                BYTE    %00000000 ; |        |  16
                BYTE    %00000000 ; |        |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %00000011 ; |      XX|  13
                BYTE    %00000111 ; |     XXX|  12
                BYTE    %00001111 ; |    XXXX|  11
                BYTE    %00010000 ; |   X    |  10
                BYTE    %00011111 ; |   XXXXX|   9
                BYTE    %00001111 ; |    XXXX|   8
                BYTE    %00001111 ; |    XXXX|   7
                BYTE    %00000111 ; |     XXX|   6
                BYTE    %00000000 ; |        |   5
                BYTE    %00000000 ; |        |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

B5_KongThrow_GRP1_0:

                BYTE    %00011111 ; |   XXXXX|  31
                BYTE    %10011101 ; |X  XXX X|  30
                BYTE    %11001110 ; |XX  XXX |  29
                BYTE    %11000000 ; |XX      |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %00000000 ; |        |  25
                BYTE    %00000000 ; |        |  24
                BYTE    %00000000 ; |        |  23
                BYTE    %00000000 ; |        |  22
                BYTE    %00000000 ; |        |  21
                BYTE    %00000000 ; |        |  20
                BYTE    %00000000 ; |        |  19
                BYTE    %00000000 ; |        |  18
                BYTE    %00000000 ; |        |  17
                BYTE    %00000000 ; |        |  16
                BYTE    %00000000 ; |        |  15
                BYTE    %01111000 ; | XXXX   |  14
                BYTE    %11111100 ; |XXXXXX  |  13
                BYTE    %11011100 ; |XX XXX  |  12
                BYTE    %10111110 ; |X XXXXX |  11
                BYTE    %01111011 ; | XXXX XX|  10
                BYTE    %11110001 ; |XXXX   X|   9
                BYTE    %11100000 ; |XXX     |   8
                BYTE    %11110001 ; |XXXX   X|   7
                BYTE    %11111001 ; |XXXXX  X|   6
                BYTE    %10011100 ; |X  XXX  |   5
                BYTE    %10011110 ; |X  XXXX |   4
                BYTE    %11111100 ; |XXXXXX  |   3
                BYTE    %01110000 ; | XXX    |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

B5_KongThrow_GRP2_0:

                BYTE    %11111111 ; |XXXXXXXX|  31
                BYTE    %11111110 ; |XXXXXXX |  30
                BYTE    %01001100 ; | X  XX  |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %00000001 ; |       X|  25
                BYTE    %00000111 ; |     XXX|  24
                BYTE    %00111111 ; |  XXXXXX|  23
                BYTE    %01111111 ; | XXXXXXX|  22
                BYTE    %01111110 ; | XXXXXX |  21
                BYTE    %00111000 ; |  XXX   |  20
                BYTE    %00011000 ; |   XX   |  19
                BYTE    %00011000 ; |   XX   |  18
                BYTE    %00001100 ; |    XX  |  17
                BYTE    %00000110 ; |     XX |  16
                BYTE    %00000011 ; |      XX|  15
                BYTE    %00000001 ; |       X|  14
                BYTE    %00000001 ; |       X|  13
                BYTE    %00000000 ; |        |  12
                BYTE    %00000000 ; |        |  11
                BYTE    %00000000 ; |        |  10
                BYTE    %10000000 ; |X       |   9
                BYTE    %10000000 ; |X       |   8
                BYTE    %11100001 ; |XXX    X|   7
                BYTE    %11100000 ; |XXX     |   6
                BYTE    %11000000 ; |XX      |   5
                BYTE    %00000011 ; |      XX|   4
                BYTE    %00011100 ; |   XXX  |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

;
; Mario COL
;

B5_MarioDead2_COL_0:

                BYTE    COL_82 ; |181AA7|  15
                BYTE    COL_82 ; |181AA7|  14
                BYTE    COL_82 ; |181AA7|  13
                BYTE    COL_82 ; |181AA7|  12
                BYTE    COL_3E ; |FCBC74|  11
                BYTE    COL_3E ; |FCBC74|  10
                BYTE    COL_3E ; |FCBC74|   9
                BYTE    COL_3E ; |FCBC74|   8
                BYTE    COL_3E ; |FCBC74|   7
                BYTE    COL_3E ; |FCBC74|   6
                BYTE    COL_3E ; |FCBC74|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_82 ; |181AA7|   2
                BYTE    COL_00 ; |000000|   1
                BYTE    COL_00 ; |000000|   0

B5_MarioDead2_COL_1:

                BYTE    COL_3E ; |FCBC74|  15
                BYTE    COL_3E ; |FCBC74|  14
                BYTE    COL_00 ; |000000|  13
                BYTE    COL_42 ; |A71A1A|  12
                BYTE    COL_42 ; |A71A1A|  11
                BYTE    COL_42 ; |A71A1A|  10
                BYTE    COL_42 ; |A71A1A|   9
                BYTE    COL_42 ; |A71A1A|   8
                BYTE    COL_42 ; |A71A1A|   7
                BYTE    COL_42 ; |A71A1A|   6
                BYTE    COL_42 ; |A71A1A|   5
                BYTE    COL_00 ; |000000|   4
                BYTE    COL_00 ; |000000|   3
                BYTE    COL_3E ; |FCBC74|   2
                BYTE    COL_3E ; |FCBC74|   1
                BYTE    COL_3E ; |FCBC74|   0

;
; Kong GRP
;

B5_KongThrow_GRP3_0:

                BYTE    %00000000 ; |        |  31
                BYTE    %00000000 ; |        |  30
                BYTE    %00000000 ; |        |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %01111100 ; | XXXXX  |  26
                BYTE    %11111100 ; |XXXXXX  |  25
                BYTE    %11111000 ; |XXXXX   |  24
                BYTE    %11100000 ; |XXX     |  23
                BYTE    %10000000 ; |X       |  22
                BYTE    %00000000 ; |        |  21
                BYTE    %00000000 ; |        |  20
                BYTE    %00000000 ; |        |  19
                BYTE    %00000000 ; |        |  18
                BYTE    %00000000 ; |        |  17
                BYTE    %00000000 ; |        |  16
                BYTE    %00000000 ; |        |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %10000001 ; |X      X|  13
                BYTE    %10000001 ; |X      X|  12
                BYTE    %10000001 ; |X      X|  11
                BYTE    %10000001 ; |X      X|  10
                BYTE    %10000010 ; |X     X |   9
                BYTE    %10000010 ; |X     X |   8
                BYTE    %10000100 ; |X    X  |   7
                BYTE    %00001000 ; |    X   |   6
                BYTE    %00110000 ; |  XX    |   5
                BYTE    %11000000 ; |XX      |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

B5_KongThrow_GRP4_0:

                BYTE    %00000111 ; |     XXX|  31
                BYTE    %00000011 ; |      XX|  30
                BYTE    %00000000 ; |        |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %00000000 ; |        |  25
                BYTE    %00000000 ; |        |  24
                BYTE    %00000001 ; |       X|  23
                BYTE    %00000000 ; |        |  22
                BYTE    %00000000 ; |        |  21
                BYTE    %00000000 ; |        |  20
                BYTE    %00000000 ; |        |  19
                BYTE    %00000000 ; |        |  18
                BYTE    %00010000 ; |   X    |  17
                BYTE    %00100000 ; |  X     |  16
                BYTE    %11000000 ; |XX      |  15
                BYTE    %10000000 ; |X       |  14
                BYTE    %10000000 ; |X       |  13
                BYTE    %00000000 ; |        |  12
                BYTE    %00000000 ; |        |  11
                BYTE    %00000000 ; |        |  10
                BYTE    %00000000 ; |        |   9
                BYTE    %00000000 ; |        |   8
                BYTE    %00000000 ; |        |   7
                BYTE    %00000000 ; |        |   6
                BYTE    %00000000 ; |        |   5
                BYTE    %00000000 ; |        |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

B5_KongThrow_GRP5_0:

                BYTE    %11110000 ; |XXXX    |  31
                BYTE    %10111000 ; |X XXX   |  30
                BYTE    %00101000 ; |  X X   |  29
                BYTE    %00001100 ; |    XX  |  28
                BYTE    %00011100 ; |   XXX  |  27
                BYTE    %00011100 ; |   XXX  |  26
                BYTE    %00001100 ; |    XX  |  25
                BYTE    %11001000 ; |XX  X   |  24
                BYTE    %10000000 ; |X       |  23
                BYTE    %00000000 ; |        |  22
                BYTE    %00000000 ; |        |  21
                BYTE    %00000000 ; |        |  20
                BYTE    %00000000 ; |        |  19
                BYTE    %00000000 ; |        |  18
                BYTE    %00000000 ; |        |  17
                BYTE    %00000000 ; |        |  16
                BYTE    %00000000 ; |        |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %00000000 ; |        |  13
                BYTE    %00000000 ; |        |  12
                BYTE    %00000000 ; |        |  11
                BYTE    %00000000 ; |        |  10
                BYTE    %00000000 ; |        |   9
                BYTE    %00000000 ; |        |   8
                BYTE    %00000000 ; |        |   7
                BYTE    %00000000 ; |        |   6
                BYTE    %00000000 ; |        |   5
                BYTE    %00000000 ; |        |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

B5_KongThrow_GRP0_1:

                BYTE    %00001110 ; |    XXX |  31
                BYTE    %00011011 ; |   XX XX|  30
                BYTE    %00011101 ; |   XXX X|  29
                BYTE    %00001111 ; |    XXXX|  28
                BYTE    %00000111 ; |     XXX|  27
                BYTE    %00000111 ; |     XXX|  26
                BYTE    %00011111 ; |   XXXXX|  25
                BYTE    %00011111 ; |   XXXXX|  24
                BYTE    %00000001 ; |       X|  23
                BYTE    %00000000 ; |        |  22
                BYTE    %00000000 ; |        |  21
                BYTE    %00000000 ; |        |  20
                BYTE    %00000000 ; |        |  19
                BYTE    %00000000 ; |        |  18
                BYTE    %00000000 ; |        |  17
                BYTE    %00000000 ; |        |  16
                BYTE    %00000000 ; |        |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %00000011 ; |      XX|  13
                BYTE    %00000111 ; |     XXX|  12
                BYTE    %00001111 ; |    XXXX|  11
                BYTE    %00011111 ; |   XXXXX|  10
                BYTE    %00011111 ; |   XXXXX|   9
                BYTE    %00001111 ; |    XXXX|   8
                BYTE    %00001111 ; |    XXXX|   7
                BYTE    %00000111 ; |     XXX|   6
                BYTE    %00000011 ; |      XX|   5
                BYTE    %00000000 ; |        |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

B5_KongThrow_GRP1_1:

                BYTE    %00010111 ; |   X XXX|  31
                BYTE    %10010111 ; |X  X XXX|  30
                BYTE    %11001011 ; |XX  X XX|  29
                BYTE    %11100001 ; |XXX    X|  28
                BYTE    %11110011 ; |XXXX  XX|  27
                BYTE    %11110011 ; |XXXX  XX|  26
                BYTE    %11111001 ; |XXXXX  X|  25
                BYTE    %11111101 ; |XXXXXX X|  24
                BYTE    %11111110 ; |XXXXXXX |  23
                BYTE    %11111111 ; |XXXXXXXX|  22
                BYTE    %11111111 ; |XXXXXXXX|  21
                BYTE    %01111111 ; | XXXXXXX|  20
                BYTE    %00111111 ; |  XXXXXX|  19
                BYTE    %00111111 ; |  XXXXXX|  18
                BYTE    %00011111 ; |   XXXXX|  17
                BYTE    %00011111 ; |   XXXXX|  16
                BYTE    %00001111 ; |    XXXX|  15
                BYTE    %01111111 ; | XXXXXXX|  14
                BYTE    %11111111 ; |XXXXXXXX|  13
                BYTE    %11111111 ; |XXXXXXXX|  12
                BYTE    %11111111 ; |XXXXXXXX|  11
                BYTE    %11111111 ; |XXXXXXXX|  10
                BYTE    %11111111 ; |XXXXXXXX|   9
                BYTE    %11111111 ; |XXXXXXXX|   8
                BYTE    %11111111 ; |XXXXXXXX|   7
                BYTE    %11111111 ; |XXXXXXXX|   6
                BYTE    %11011111 ; |XX XXXXX|   5
                BYTE    %10011111 ; |X  XXXXX|   4
                BYTE    %11111111 ; |XXXXXXXX|   3
                BYTE    %01111111 ; | XXXXXXX|   2
                BYTE    %01111111 ; | XXXXXXX|   1
                BYTE    %00111111 ; |  XXXXXX|   0

B5_KongThrow_GRP2_1:

                BYTE    %11111111 ; |XXXXXXXX|  31
                BYTE    %11111111 ; |XXXXXXXX|  30
                BYTE    %11111111 ; |XXXXXXXX|  29
                BYTE    %11111110 ; |XXXXXXX |  28
                BYTE    %11111110 ; |XXXXXXX |  27
                BYTE    %11111111 ; |XXXXXXXX|  26
                BYTE    %11111110 ; |XXXXXXX |  25
                BYTE    %11111100 ; |XXXXXX  |  24
                BYTE    %11111000 ; |XXXXX   |  23
                BYTE    %01100000 ; | XX     |  22
                BYTE    %11010001 ; |XX X   X|  21
                BYTE    %11111111 ; |XXXXXXXX|  20
                BYTE    %11111111 ; |XXXXXXXX|  19
                BYTE    %11100111 ; |XXX  XXX|  18
                BYTE    %11110011 ; |XXXX  XX|  17
                BYTE    %11111001 ; |XXXXX  X|  16
                BYTE    %11111100 ; |XXXXXX  |  15
                BYTE    %11111110 ; |XXXXXXX |  14
                BYTE    %11111110 ; |XXXXXXX |  13
                BYTE    %11111111 ; |XXXXXXXX|  12
                BYTE    %11111111 ; |XXXXXXXX|  11
                BYTE    %11111111 ; |XXXXXXXX|  10
                BYTE    %11111111 ; |XXXXXXXX|   9
                BYTE    %11111111 ; |XXXXXXXX|   8
                BYTE    %11111110 ; |XXXXXXX |   7
                BYTE    %11111111 ; |XXXXXXXX|   6
                BYTE    %11111111 ; |XXXXXXXX|   5
                BYTE    %11111100 ; |XXXXXX  |   4
                BYTE    %11100010 ; |XXX   X |   3
                BYTE    %11100000 ; |XXX     |   2
                BYTE    %11000000 ; |XX      |   1
                BYTE    %00000000 ; |        |   0

B5_KongThrow_GRP3_1:

                BYTE    %00000000 ; |        |  31
                BYTE    %00000000 ; |        |  30
                BYTE    %00000000 ; |        |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000001 ; |       X|  27
                BYTE    %10000011 ; |X     XX|  26
                BYTE    %00000011 ; |      XX|  25
                BYTE    %00000111 ; |     XXX|  24
                BYTE    %00011111 ; |   XXXXX|  23
                BYTE    %01111111 ; | XXXXXXX|  22
                BYTE    %11111111 ; |XXXXXXXX|  21
                BYTE    %11111111 ; |XXXXXXXX|  20
                BYTE    %11111111 ; |XXXXXXXX|  19
                BYTE    %11111111 ; |XXXXXXXX|  18
                BYTE    %11111111 ; |XXXXXXXX|  17
                BYTE    %11111111 ; |XXXXXXXX|  16
                BYTE    %11111111 ; |XXXXXXXX|  15
                BYTE    %11111111 ; |XXXXXXXX|  14
                BYTE    %01111110 ; | XXXXXX |  13
                BYTE    %01111110 ; | XXXXXX |  12
                BYTE    %01111110 ; | XXXXXX |  11
                BYTE    %01111110 ; | XXXXXX |  10
                BYTE    %01111100 ; | XXXXX  |   9
                BYTE    %01111100 ; | XXXXX  |   8
                BYTE    %01111000 ; | XXXX   |   7
                BYTE    %11110000 ; |XXXX    |   6
                BYTE    %11000000 ; |XX      |   5
                BYTE    %00000000 ; |        |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

;
; Mario COL
;

B5_MarioDead3_COL_1:

                BYTE    COL_82 ; |181AA7|  15
                BYTE    COL_82 ; |181AA7|  14
                BYTE    COL_82 ; |181AA7|  13
                BYTE    COL_42 ; |A71A1A|  12
                BYTE    COL_42 ; |A71A1A|  11
                BYTE    COL_42 ; |A71A1A|  10
                BYTE    COL_42 ; |A71A1A|   9
                BYTE    COL_42 ; |A71A1A|   8
                BYTE    COL_42 ; |A71A1A|   7
                BYTE    COL_42 ; |A71A1A|   6
                BYTE    COL_42 ; |A71A1A|   5
              ; BYTE    COL_3E ; |FCBC74|   4
              ; BYTE    COL_3E ; |FCBC74|   3
              ; BYTE    COL_3E ; |FCBC74|   2
              ; BYTE    COL_3E ; |FCBC74|   1
              ; BYTE    COL_3E ; |FCBC74|   0

B5_MarioDead3_COL_0:

                BYTE    COL_3E ; |FCBC74|  15
                BYTE    COL_3E ; |FCBC74|  14
                BYTE    COL_3E ; |FCBC74|  13
                BYTE    COL_3E ; |FCBC74|  12
                BYTE    COL_3E ; |FCBC74|  11
                BYTE    COL_3E ; |FCBC74|  10
                BYTE    COL_3E ; |FCBC74|   9
                BYTE    COL_3E ; |FCBC74|   8
                BYTE    COL_00 ; |000000|   7
                BYTE    COL_82 ; |181AA7|   6
                BYTE    COL_82 ; |181AA7|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_00 ; |000000|   3
                BYTE    COL_00 ; |000000|   2
                BYTE    COL_00 ; |000000|   1
                BYTE    COL_00 ; |000000|   0

;
; Kong GRP
;

B5_KongThrow_GRP4_1:

                BYTE    %00000101 ; |     X X|  31
                BYTE    %00000011 ; |      XX|  30
                BYTE    %00000001 ; |       X|  29
                BYTE    %11111111 ; |XXXXXXXX|  28
                BYTE    %11111111 ; |XXXXXXXX|  27
                BYTE    %11111111 ; |XXXXXXXX|  26
                BYTE    %11111111 ; |XXXXXXXX|  25
                BYTE    %11111111 ; |XXXXXXXX|  24
                BYTE    %11111110 ; |XXXXXXX |  23
                BYTE    %11111110 ; |XXXXXXX |  22
                BYTE    %11111110 ; |XXXXXXX |  21
                BYTE    %11111110 ; |XXXXXXX |  20
                BYTE    %11111100 ; |XXXXXX  |  19
                BYTE    %11111000 ; |XXXXX   |  18
                BYTE    %11100000 ; |XXX     |  17
                BYTE    %11000000 ; |XX      |  16
                BYTE    %00000000 ; |        |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %00000000 ; |        |  13
                BYTE    %00000000 ; |        |  12
                BYTE    %00000000 ; |        |  11
                BYTE    %00000000 ; |        |  10
                BYTE    %00000000 ; |        |   9
                BYTE    %00000000 ; |        |   8
                BYTE    %00000000 ; |        |   7
                BYTE    %00000000 ; |        |   6
                BYTE    %00000000 ; |        |   5
                BYTE    %00000000 ; |        |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

B5_KongThrow_GRP5_1:

                BYTE    %01110000 ; | XXX    |  31
                BYTE    %11111000 ; |XXXXX   |  30
                BYTE    %11111000 ; |XXXXX   |  29
                BYTE    %11111000 ; |XXXXX   |  28
                BYTE    %11111000 ; |XXXXX   |  27
                BYTE    %11111000 ; |XXXXX   |  26
                BYTE    %11111000 ; |XXXXX   |  25
                BYTE    %00111000 ; |  XXX   |  24
              ; BYTE    %00000000 ; |        |  23
              ; BYTE    %00000000 ; |        |  22
              ; BYTE    %00000000 ; |        |  21
              ; BYTE    %00000000 ; |        |  20
              ; BYTE    %00000000 ; |        |  19
              ; BYTE    %00000000 ; |        |  18
              ; BYTE    %00000000 ; |        |  17
              ; BYTE    %00000000 ; |        |  16
              ; BYTE    %00000000 ; |        |  15
              ; BYTE    %00000000 ; |        |  14
              ; BYTE    %00000000 ; |        |  13
              ; BYTE    %00000000 ; |        |  12
              ; BYTE    %00000000 ; |        |  11
              ; BYTE    %00000000 ; |        |  10
              ; BYTE    %00000000 ; |        |   9
              ; BYTE    %00000000 ; |        |   8
              ; BYTE    %00000000 ; |        |   7
              ; BYTE    %00000000 ; |        |   6
              ; BYTE    %00000000 ; |        |   5
              ; BYTE    %00000000 ; |        |   4
              ; BYTE    %00000000 ; |        |   3
              ; BYTE    %00000000 ; |        |   2
              ; BYTE    %00000000 ; |        |   1
              ; BYTE    %00000000 ; |        |   0

;
; Kong BL
;

B5_KongThrow_BL:BYTE    $00,$00,$00,$00,$00,$00,$00,$00
                BYTE    $00,$00,$00,$00,$00,$00,$00,$00
                BYTE    $00,$00,$00,$00,$00,$00,$00,$00
                BYTE    $00,$00,$02,$02,$00,$00,$00,$00

; ====================================================================
;       Copy Mario
; ====================================================================

                ALIGN_END Bank5Start, $0025

CopyMario:      SUBROUTINE

                ; copy Mario GRP and COL data

                lda     MarioVPos_R
                and     #FLOOR_HEIGHT-1
                clc
                adc     #MARIO_HEIGHT+8
                sta     Temp

                ldy     #MARIO_HEIGHT-1
.loop           dec     Temp
                lda     Temp
                cmp     #40
                bcc     .write
                sbc     #40
.write          tax
                lda     (ST_GRPPtrTmp),y
                sta     MarioGRPBuffer_W,x
                lda     (ST_COLPtrTmp),y
                sta     MarioCOLBuffer_W,y
                dey
                bpl     .loop
                rts

; ********************************************************************

#if PRINT_MESSAGES
                ECHO "---- Bank 5: ", (ROMJumpTable - *) + GapSize, "bytes of ROM left"
#endif
