
; ********************************************************************
;  Donkey Kong VCS
;
;    ROM Bank 7
;
;    $Date: Sat, 11 Feb 2017 21:03:15 +0100 $
;    $Author: dietrich $
;    $Revision: 486 $
;
;  Copyright (C) 2017 Andreas Dietrich
; ********************************************************************

;---------------------------------------------------------------------
;       CR Constants
;---------------------------------------------------------------------

CM_TEXT_CHROMA          = COL_20        ; base text hue |7C2C00|
CM_FADE_IN_START        = 30            ; start fading text in after 0.5 seconds
CM_FADE_OUT_START       = 3*60          ; start fading text out after 3 seconds
CM_FADE_END             = 4*60          ; stop fading and leave after 4 seconds

;---------------------------------------------------------------------
;       CR Variables
;---------------------------------------------------------------------

CM_TextCol              = $B0           ; current fading color

;---------------------------------------------------------------------
;       IM Constants
;---------------------------------------------------------------------

IM_TEXT_COL             = DK_COL_WHITE  ; goal and HOW HIGH color |ECECEC|

;---------------------------------------------------------------------
;       IM Variables
;---------------------------------------------------------------------

IM_GoalPtr0             = $E0 ; [2]     ; hundreds GPR pointer
IM_GoalPtr1             = $E2 ; [2]     ; tens     GRP pointer
IM_GoalPtr2             = $E4 ; [2]     ; ones     GRP pointer

IM_KongLineCtr          = $E6           ; Kong lines to draw
IM_KongSkipCtr          = $E7           ; line skiped above Kong
IM_KongScrlCtr          = $E8           ; scroll counter

IM_RoundNum             = $E9           ; current round ordinal
IM_Exiting              = $EA           ; early exit in progress

;---------------------------------------------------------------------
;       GO Constants
;---------------------------------------------------------------------

GO_TEXT_COL             = DK_COL_YELLOW ; GAME OVER text color (level 4 arcade)
GO_GIRDER_COL_0         = DK_COL_BLUE   ; dark girder color  |181AA7|
GO_GIRDER_COL_1         = DK_COL_CYAN   ; light girder color |84E0FC|
GO_TIME_END             = 210           ; leave after 3.5 seconds (3.5x60)

;---------------------------------------------------------------------
;       Score Constants
;---------------------------------------------------------------------

ONE_UP_FLASH            = 16            ; 1UP flash speed (power of two)
ONE_UP_COL              = DK_COL_RED    ; 1UP text color
POINTS_COL              = DK_COL_WHITE  ; score points color
LEVEL_COL               = DK_COL_BLUE   ; level number color
EXTRA_LIFE_A            = $02           ; extra Mario at 20,000 (pro)
EXTRA_LIFE_B            = $01           ; extra Mario at 10,000 (amateur)


; ********************************************************************
;
;       Code Section
;
; ********************************************************************

Start:          RESET

                ; debug mode settings

                STOREBYTE $20, Level_Round ; <------------------------
                STOREBYTE $81, Death_Lives ; <------------------------
                STOREBYTE $FF, DemoMode_W  ; disable demo mode

                ; init extra storage bits

                lda     #%00110100
                sta     SWBCNT
                sta     SWCHB

; ====================================================================
;       Copyright Message
; ====================================================================

CopyrightMessage:

; --------------------------------------------------------------------
;       CM VSync
; --------------------------------------------------------------------

CM_VSync:   IF (!DEBUG_MODE)

                jsr     B7_VerticalSync

            ELSE

                DEBUG DEBUG_DONKEY_KONG_VCS,  jmp  Bank0_DonkeyKongVCS
                DEBUG DEBUG_INTRO,            jmp  Bank1_Intro
                DEBUG DEBUG_INTERMISSION,     jmp  Bank7_Intermission
                DEBUG DEBUG_ENDING,           jmp  Bank6_Ending

            ENDIF

; --------------------------------------------------------------------
;       CM VBlank
; --------------------------------------------------------------------

CM_VBlank:      lda     INTIM
                bne     CM_VBlank

; --------------------------------------------------------------------
;       CM Kernel
; --------------------------------------------------------------------

CM_Kernel:      sta     WSYNC
                sta     VBLANK
                lda     #%00000110
                sta     NUSIZ0
                sta     NUSIZ1

                ; position text sprites

CM_PosText:     jsr     B7_HPosB96Centered

                ; draw interlaced text

                WAIT_LINES (192-CM_TEXT_LINES*13)/2-3

CM_DrawText:    SUBROUTINE
                ldy     #CM_TEXT_LINES*12-1
.loop0          ldx     #24-2
.loop1          lda     CM_Text,y
                asl
                sta     ScorePtr,x
                lda     #(>DigitFont)>>1
                rol
                sta     ScorePtr+1,x
                dey
                dex
                dex
                bpl     .loop1
                sty     Temp
                ldy     #7
                jsr     B7_Bitmap96
                ldy     Temp
                bpl     .loop0

                WAIT_LINES #(192-CM_TEXT_LINES*13)/2+2

; --------------------------------------------------------------------
;       CM Overscan
; --------------------------------------------------------------------

CM_Overscan:    jsr     B7_Overscan

                inc     FrameCtr

                ; go to Title kernel

                lda     FrameCtr
                cmp     #CM_FADE_END
                bcc     CM_FadeText
                jmp     Bank0_DonkeyKongVCS

                ; fade text in and out

CM_FadeText:    SUBROUTINE
                tax
                and     #%00000011
                bne     .end

                cpx     #CM_FADE_IN_START
                bcc     .end
                cpx     #CM_FADE_OUT_START
                lda     CM_TextCol                      ; affects S, Z (not C)
                bcs     .fadeout
.fadein         cmp     #7
                bcs     .write
                inc     CM_TextCol
                bne     .write
.fadeout        bpl     .skip
                lda     #((-CM_TEXT_CHROMA) & $F0) / 2  ; generates black
.skip           dec     CM_TextCol
.write          asl
                adc     #CM_TEXT_CHROMA
                sta     COLUP0
                sta     COLUP1
.end

CM_OverscanEnd: lda     INTIM
                bne     CM_OverscanEnd
                jmp     CM_VSync

; ====================================================================
;       Intermission
; ====================================================================

Intermission:   STOREBYTE 7, Bank

                ; check game over

                lda     Death_Lives
                and     #~EXTRA_MASK
                cmp     #%10000000
                bne     IM_Init
                jmp     GameOver

; --------------------------------------------------------------------
;       IM Init
; --------------------------------------------------------------------

IM_Init:        SUBROUTINE

                ; clear RAM and TIA registers

                CLEAR_RAM $E0, StackBottom
                CLEAR_TIA

                ; select next stage

                lda     SWCHB
                lsr
                lsr
                lsr
                lda     #$30                    ; Japanese screen order
                bcs     .round
                lda     Level_Round             ; US screen order
                and     #LEVEL_MASK
                cmp     #$50
                bcc     .round
                lda     #$50

.round          lsr
                tax
                lda     Level_Round
                and     #ROUND_MASK
.loop           dex
                cmp     IM_RoundCodes,x
                bne     .loop
                lda     Death_Lives             ; Mario death ?
                bpl     .skip                   ; no  -> progress
                dex                             ; yes -> keep round
.skip           txa
                and     #8-1
                sta     IM_RoundNum
                lda     Level_Round
                and     #LEVEL_MASK
                ora     IM_RoundCodes+1,x
                sta     Level_Round

                ; prepare stage variables

                jsr     PrepareStage

                ; noise player

                lda     #MUSIC_INIT_INTERMISSION
                jsr     Bank5_DriveAudio

.wait           lda     INTIM
                bne     .wait

; --------------------------------------------------------------------
;       IM VSync
; --------------------------------------------------------------------

IM_VSync:       jsr     B7_VerticalSync

; --------------------------------------------------------------------
;       IM VBlank
; --------------------------------------------------------------------

IM_VBlank:      lda     #MUSIC_PLAY
                jsr     Bank5_DriveAudio

                ; mute audio and prepare exit

IM_Exit:        SUBROUTINE
                lda     IM_Exiting
                bne     .end
                jsr     IM_CheckExit
.end

IM_SetupKongs:  SUBROUTINE

                ; scroll Kong

.scroll         lda     IM_RoundNum
                cmp     #4                      ; round >= 4 ?
                bcc     .kong
                lda     FrameCtr
                lsr
                bcs     .skip
                lda     IM_KongScrlCtr
                cmp     #32
                bcs     .skip
                inc     IM_KongScrlCtr
.skip           lda     #3                      ; display 4 Kongs

                ; calculate number of Kong lines

.kong           asl
                asl
                asl
                asl
                asl                             ; carry is clear
                adc     #32-1                   ; increase stage number
                sta     IM_KongLineCtr          ; Kongs x 32 - 1

                ; calculate number of lines above Kong

.blank          lda     #4*32+1                 ; carry still clear
                sbc     IM_KongLineCtr
                sta     IM_KongSkipCtr

IM_SetupGoal:   SUBROUTINE

                ; set digit hi-bytes

                lda     #>SpecialCharacterFont
                sta     IM_GoalPtr0+1
                sta     IM_GoalPtr1+1
                sta     IM_GoalPtr2+1

                ; setup lower two digits

                lda     IM_RoundNum
                and     #%00000011
                tax
                lda     IM_GoalDigit1_L,x
                sta     IM_GoalPtr1
                lda     IM_GoalDigit2_L,x
                sta     IM_GoalPtr2

                ; setup upper digit (hundreds)

                lda     IM_RoundNum
                asl
                adc     #%00000010
                and     #%00111000
                bne     .skip
                lda     #>Space
                sta     IM_GoalPtr0+1
                lda     #(<Space)-(<zero)
.skip           adc     #<zero
                sta     IM_GoalPtr0

IM_VBlankEnd:   lda     INTIM
                bne     IM_VBlankEnd

; --------------------------------------------------------------------
;       IM Kernel
; --------------------------------------------------------------------

;
;  Part 1 -- Score
;

IM_Score:       jsr     DrawScore

;
;  Part 2 -- Kong
;

IM_Kong:        SUBROUTINE
                lda     #DK_COL_WHITE
                sta     COLUPF
                lda     #%00000011
                sta     NUSIZ0
                sta     NUSIZ1

                ; skip lines above Kong

                ldy     IM_KongSkipCtr
                beq     IM_PosKong
.wait           sta     WSYNC
                dey
                bne     .wait

IM_PosKong:     jsr     B7_HPosB48Centered

                ; draw alternating Kong planes 0/1

IM_DrawKong     SUBROUTINE
                ldy     #5
.wait           dey
                bpl     .wait
                nop
                nop

                lda     FrameCtr
                lsr
                bcs     .plane1

                ; even plane

.plane0         lda     #KONG_COL_1
                sta     COLUP0
                sta     COLUP1
                clc
.loop0          lda     IM_KongLineCtr
                adc     IM_KongScrlCtr
                and     #%00011111
                tay
                lda     IM_KongPF2,y
                sta     PF2
                lda     IM_KongGRP0_0,y
                sta     GRP0
                lda     IM_KongGRP1_0,y
                sta     GRP1
                lda     IM_KongGRP2_0,y
                sta     GRP0
                lda     IM_KongGRP3_0,y
                sta     Temp
                ldx     IM_KongGRP4_0,y
                lda     IM_KongGRP5_0,y
                ldy     Temp
                sty     GRP1
                stx     GRP0
                sta     GRP1
                sta     GRP0
                dec     IM_KongLineCtr
                bpl     .loop0
                bmi     .clear

                ; odd plane

.plane1         lda     #KONG_COL_0
                sta     COLUP0
                sta     COLUP1
                clc
.loop1          lda     IM_KongLineCtr
                adc     IM_KongScrlCtr
                and     #%00011111
                tay
                lda     IM_KongPF2,y
                sta     PF2
                lda     IM_KongGRP0_1,y
                sta     GRP0
                lda     IM_KongGRP1_1,y
                sta     GRP1
                lda     IM_KongGRP2_1,y
                sta     GRP0
                lda     IM_KongGRP3_1,y
                sta     Temp
                ldx     IM_KongGRP4_1,y
                lda     IM_KongGRP5_1,y
                ldy     Temp
                sty     GRP1
                stx     GRP0
                sta     GRP1
                sta     GRP0
                dec     IM_KongLineCtr
                bpl     .loop1

                ; channels off

.clear          sta     WSYNC
                lda     #0
                sta     GRP0
                sta     GRP1
                sta     GRP0
                sta     PF2

;
;  Part 3 -- Goal
;

IM_Goal:        lda     #IM_TEXT_COL
                sta     COLUP0
                sta     COLUP1

                ; postion level meters on the left

IM_PosGoal:     SUBROUTINE
                sta     RESP0
                sta     RESP1
                lda     #$10
                sta     HMP0
                lda     #$20
                sta     HMP1
                ldx     #5
.loop           dex
                bne     .loop
                sta     HMOVE

                ; draw level meters

IM_DrawGoal:    SUBROUTINE
                ldy     #6
.loop           lda     (IM_GoalPtr0),y
                sta     WSYNC
                sta     GRP0
                lda     (IM_GoalPtr1),y
                sta     GRP1
                lda     (IM_GoalPtr2),y
                sta     GRP0
                lda     Space,y
                ldx     m,y
                sta     GRP1
                stx     GRP0
                stx     GRP1
                dey
                bpl     .loop

;
;  Part 4 -- How high
;

                ; position "HOW HIGH" at the center

IM_PosHowHigh:  jsr     B7_HPosB96Centered

IM_SetupHowHigh:lda     #%00000110
                sta     NUSIZ0
                sta     NUSIZ1
                sta     VDELP0
                sta     VDELP1

IM_DrawHowHigh: SUBROUTINE

                ; set hi-bytes for letters 6-11 (0-5 from DrawScore)

                lda     #>(Space+1)
                ldx     #12-1
.loop           sta     ScorePtr+12,x
                dex
                dex
                bpl     .loop

                ; draw "HOW HIGH" text line

                STOREBYTE <Space+1,    ScorePtr+$00
                sta                    ScorePtr+$02
                sta                    ScorePtr+$0A
                sta                    ScorePtr+$14
                sta                    ScorePtr+$16
                STOREBYTE <H+1,        ScorePtr+$04
                sta                    ScorePtr+$0C
                sta                    ScorePtr+$12
                STOREBYTE <O+1,        ScorePtr+$06
                STOREBYTE <W+1,        ScorePtr+$08
                STOREBYTE <I+1,        ScorePtr+$0E
                STOREBYTE <G+1,        ScorePtr+$10
                inc                    ScorePtr+$09     ; fix 'W' hi-byte

                ldy     #6
                jsr     B7_Bitmap96
                iny
                sty     GRP0

                ; draw "CAN YOU GET?" text line

                STOREBYTE <C+1,        ScorePtr+$00
                STOREBYTE <A+1,        ScorePtr+$02
                STOREBYTE <N+1,        ScorePtr+$04
                STOREBYTE <Space+1,    ScorePtr+$06
                sta                    ScorePtr+$0E
                STOREBYTE <Y+1,        ScorePtr+$08
                STOREBYTE <O+1,        ScorePtr+$0A
                STOREBYTE <U+1,        ScorePtr+$0C
                ; ---------------------------------
                lda     #<G+1
                ldx     #<E+1
                ldy     #<T+1
                ; *********************************
                jsr     IM_EasterEgg
                ; *********************************
                sta     ScorePtr+$10
                stx     ScorePtr+$12
                sty     ScorePtr+$14
                ; ---------------------------------
                STOREBYTE <Question+1, ScorePtr+$16
                inc                    ScorePtr+$17     ; fix '?' hi-byte

                ldy     #6
                jsr     B7_Bitmap96

; --------------------------------------------------------------------
;       IM Overscan
; --------------------------------------------------------------------

IM_Overscan:    lda     #%00000010
                sta     VBLANK                          ; VBLANK on

                iny                                     ; Y := 0
                sty     GRP0
                sty     GRP1
                sty     GRP0
                sty     PF2

                lda     #35                             ; set timer to skip OVERSCAN
                sta     TIM64T

                CHECK_RESET

                inc     FrameCtr

                ; exit to Stage kernel

                lda     NPNote+1
                bne     IM_OverscanEnd
                jmp     Bank2_Stage

IM_OverscanEnd: lda     INTIM
                bne     IM_OverscanEnd
                jmp     IM_VSync

; ====================================================================
;       Game Over
; ====================================================================

GameOver:

; --------------------------------------------------------------------
;       GO Init
; --------------------------------------------------------------------

GO_Init:        SUBROUTINE

                ; clear TIA and EventCtr

                CLEAR_TIA
                sta     EventCtr

                ; check if we were in demo mode

                jsr     GO_Skip

.wait           lda     INTIM
                bne     .wait

; --------------------------------------------------------------------
;       GO VSync
; --------------------------------------------------------------------

GO_VSync:       jsr     B7_VerticalSync

; --------------------------------------------------------------------
;       GO VBlank
; --------------------------------------------------------------------

GO_VBlank:      STOREWORD GO_GirderPF2+1, Ptr

GO_VBlankEnd:   lda     INTIM
                bne     GO_VBlankEnd

; --------------------------------------------------------------------
;       GO Kernel
; --------------------------------------------------------------------

;
;  -- Score
;

GO_Score:       jsr     DrawScore

;
;  -- Game Over
;

GO_PosOuterDots:lda     #(53+13*4)+1
                ldx     #2
                jsr     B7_HPosBZNoHMOVE
                lda     #(53+00*4)+1
                ldx     #4
                jsr     B7_HPosBZNoHMOVE
                sta     WSYNC
                sta     HMOVE

                WAIT_LINES 7

                sta     HMCLR

                ; draw Kong

GO_DrawKong:    lda     Random
                and     #3
                tax
                ldy     GO_KongAnimTab,x
                STOREBYTE 31, LineCtr
                jsr     Bank6_DrawKong

                ; draw first line of frame

GO_DrawFrameHi: ldx     #0
                stx     COLUPF
                stx     GRP0
                stx     GRP1
                stx     GRP0

                ldy     #%11111111
                sty     PF0
                sty     PF1
                ldy     #%00000001
                sty     PF2
                lda     #GO_GIRDER_COL_0
                sta     COLUBK

                ; draw second line of frame

                sta     WSYNC
                lda     #GO_GIRDER_COL_1
                sta     COLUBK
                stx     COLUP0                  ; X = 0
                stx     COLUP1
                stx     VDELP0
                stx     VDELP1
                stx     REFP0
                stx     REFP1

                ; draw higher part of grider frame

                iny                             ; Y := 2
                ldx     #5                      ; skip 2 lines
                jsr     GO_DrawFrame+4          ; jump to inner loop

                lda     #%00000001
                sta     VDELP0
                sta     VDELP1

                ; draw "GAME" text line

GO_DrawGame:    SUBROUTINE
                ldy     #7
.loop           lda     GO_GirderCOL,y
                ldx     #GO_TEXT_COL
                sta     COLUBK
                sta     WSYNC
                stx     COLUP0
                stx     COLUP1
                asl
                sta     ENAM0
                sta     ENABL
                lda     #0
                sta     GRP1
                lda     G,y
                sta     GRP0
                lda     A,y
                sta     GRP1
                lda     M,y
                ldx     E,y
                sta     GRP0
                stx     GRP1
                lda     #0
                sta     GRP0
                sta     COLUP0
                dey
                bpl     .loop

                ; draw "OVER" text line

GO_DrawOver:    SUBROUTINE
                ldy     #7
.loop           lda     GO_GirderCOL,y
                ldx     #GO_TEXT_COL
                sta     COLUBK
                sta     WSYNC
                stx     COLUP0
                stx     COLUP1
                asl
                sta     ENAM0
                sta     ENABL
                lda     #0
                sta     GRP1
                lda     O,y
                sta     GRP0
                lda     V,y
                sta     GRP1
                lda     E,y
                ldx     R,y
                sta     GRP0
                stx     GRP1
                lda     #0
                sta     GRP0
                sta     COLUP0
                dey
                bne     .loop                   ; draw only 7 lines

                ; draw lower part of girder frame

GO_DrawFrameLo: sty     VDELP0
                sty     VDELP1
                sty     COLUP0
                sty     COLUP1
                lda     #GO_GIRDER_COL_1
                sta     COLUBK

                dec     Ptr
                jsr     GO_DrawFrame

                sta     WSYNC
                sta     COLUBK
                WAIT_LINES 48

; --------------------------------------------------------------------
;       GO Overscan
; --------------------------------------------------------------------

GO_Overscan:    jsr     B7_Overscan

                CHECK_RESET

                inc     FrameCtr
                inc     EventCtr

                ; go to title

                lda     EventCtr
                cmp     #GO_TIME_END
                bcc     GO_OverscanEnd
                jmp     Bank0_DonkeyKongVCS

GO_OverscanEnd: lda     INTIM
                bne     GO_OverscanEnd
                jmp     GO_VSync

; --------------------------------------------------------------------
;       GO Subroutines
; --------------------------------------------------------------------

GO_DrawFrame:   SUBROUTINE
                ldy     #2
.loop0          ldx     #7
.loop1          lda     (Ptr),y
                sta     WSYNC
                sta     PF2
                lda     GO_GirderCOL,x
                sta     COLUBK
                lda     GO_DotGRP,x
                sta     GRP0
                sta     GRP1
                sta     ENABL
                sta     ENAM0
                dex
                bpl     .loop1
                dey
                bpl     .loop0
                rts

; ====================================================================
;       Draw Score
; ====================================================================

DrawScore:      sta     WSYNC
                sta     VBLANK
                sta     COLUBK
                sta     REFP0
                sta     REFP1
                lda     #%00100011
                sta     NUSIZ0
                sta     NUSIZ1
                sta     VDELP0
                sta     VDELP1
                ldy     #%00100001
                sty     CTRLPF

;
; Part 1 -- Score
;

                ; compute score GRP pointers

SetScore:       SUBROUTINE

                ; set pointer hi-bytes

                lda     #>(DigitFont)
                sta     ScorePtr+$01
                sta     ScorePtr+$03
                sta     ScorePtr+$05
                sta     ScorePtr+$07
                sta     ScorePtr+$09
                sta     ScorePtr+$0B

                ; compute pointer lo-bytes from Score

                ldx     #2                      ; Y = %00100001
.loop           lda     Score,x                 ; lo-nibble
                and     #$0F
                asl
                asl
                asl
                adc     #<(DigitFont)
                sta     ScorePtr-%00100001+10,y
                dey
                dey
                ;
                lda     Score,x                 ; hi-nibble
                and     #$F0
                lsr
                adc     #<(DigitFont)
                sta     ScorePtr-%00100001+10,y
                dey
                dey
                dex
                bpl     .loop

                ; ----------------------------------------------------

TimeLadder:     SUBROUTINE

                ; operate timer for the telescope ladder
                ; (not enough space in bank 0)

                lda     EventCtr                ; bonus timer (stops if needed)
                and     #%00000011              ; one tick = 4 frames
                bne     .end

                ldx     Stage2LadderState_R
                dex
                bne     .write
                ldx     #LADDER_STATE_TOP
.write          stx     Stage2LadderState_W
.end
                ; ----------------------------------------------------

                ; position score at the center

PosScore:       jsr     B7_HPosB48Centered

                ; turn "1UP" on and off

Flash1UP:       SUBROUTINE
                ldx     #$00
                lda     FrameCtr
                and     #ONE_UP_FLASH
                beq     .write
                ldx     #ONE_UP_COL
.write          stx     Temp

                ; award extra life

ExtraLife:      SUBROUTINE
                bit     Death_Lives
                bvs     .end
                lda     Score+0
                bit     SWCHB
                bpl     .extra_b
.extra_a        cmp     #EXTRA_LIFE_A
                bcc     .end
.extra_b        cmp     #EXTRA_LIFE_B
                bcc     .end
                lda     Death_Lives
                adc     #EXTRA_MASK             ; C = 1
                sta     Death_Lives
.end
                ; draw "1UP" text line

Draw1UP:        SUBROUTINE
                STOREBYTE 7, LineCtr
.loop           ldy     LineCtr
                sta     WSYNC
                lda     Temp
                sta     COLUP0
                sta     COLUP1
                lda     #0
                sta     GRP0
                sta     GRP1
                lda     One,y
                sta     GRP0
                nop
                nop
                nop
                nop
                lda     U,y
                ldx     P,y
                ldy     #0
                sta     GRP1
                stx     GRP0
                sty     GRP1
                sty     GRP0
                dec     LineCtr
                bne     .loop

                sty     GRP1                    ; blank next line

                ; ----------------------------------------------------

MoveLadder:     SUBROUTINE

                ; state function for moving the telescope ladder
                ; (not enough space in bank 0)

                lda     Level_Round             ; stage 2 ?
                and     #STAGE_MASK
                eor     #1                      ; A := 0 if stage 2
                bne     .end

                ldx     Stage2LadderState_R
.top            cpx     #LADDER_STATE_DOWN      ; ladder on top ?
                bcs     .write                  ; yes -> offset := 0
                ;
                lda     MarioFloor_R            ; Mario on top floor ?
                cmp     #3*4
                bne     .down
                lda     #4                      ; prevent climbing
                sta     Stage2LadderLimit_W
                ;
.down           txa
                lsr                             ; ladder moves down slowly
                sbc     #(LADDER_STATE_BOTTOM/2); compute offset
                eor     #7                      ; invert offset
                bcs     .write                  ; still going down ?
                ;
.bottom         lda     #8
                cpx     #LADDER_STATE_UP        ; ladder on bottom ?
                bcs     .write                  ; yes -> offset := 8
                ;
.up             txa                             ; ladder moves up

.write          sta     Stage2LadderOffset_W
.end
                ; ----------------------------------------------------

                ; draw score points

DrawPoints:     SUBROUTINE
                lda     #POINTS_COL
                sta     COLUP0
                sta     COLUP1

                STOREBYTE 7, LineCtr
.loop           ldy     LineCtr
                lda     (ScorePtr+$0),y
                sta     GRP0
                sta     WSYNC
                lda     (ScorePtr+$2),y
                sta     GRP1
                lda     (ScorePtr+$4),y
                sta     GRP0
                lda     (ScorePtr+$6),y
                sta     Temp
                lda     (ScorePtr+$8),y
                tax
                lda     (ScorePtr+$A),y
                tay
                lda     Temp
                sta     GRP1
                stx     GRP0
                sty     GRP1
                sta     GRP0
                dec     LineCtr
                bpl     .loop

;
; Part 2 -- Status
;

                ; setup sprites

Status:         SUBROUTINE
                sta     COLUP1
                lda     #LEVEL_COL
                sta     COLUPF

#if (LEVEL_COL & %00000011) != %00000010
                ECHO    "ERROR: Can't optimize Status"
#endif
                sta     ENAM1
                sta     VDELP0
                sta     VDELP1

                lda     Level_Round
                and     #LEVEL_MASK
                lsr
                sta     ScorePtr

PosStatus:      SUBROUTINE

                ; clamp lives value to [0,3]

                lda     Death_Lives
                and     #LIVES_MASK
                cmp     #3
                bcc     .skip
                lda     #3
.skip           tax
                ldy     #10

                ; position lives on the left (or on the far right if invisible)

                lda     LivesHMP0,x
                sta     HMP0
                lda     LivesHMM1,x
                sta     HMM1
                and     #$0F
                sta     WSYNC
                tax
.loop0          dex
                bne     .loop0
                sta     RESP0
                sta     RESM1

                ; position level on the right

                sta     WSYNC
.loop1          dey
                bne     .loop1
                sta     RESP1
                sta     RESBL
                lda     #$D0
                sta     HMP1
                lda     #$B0
                sta     HMBL

                ; draw remaining lives and level number

DrawStatus:     SUBROUTINE
                ldy     #7
.loop           sta     WSYNC
                sta     HMOVE
                ldx     B7_MarioHMOVE-1,y
                lda     B7_MarioGRP0,y
                sta     GRP0
                lda     B7_MarioCOL0,y
                sta     COLUP0
                lda     B7_MarioCOL1,y
                sta     COLUP1
                stx     HMCLR
                stx     HMM1
                stx     ENABL
                lda     #DK_COL_BLUE
                sta.w   COLUP1
                lda     L,y
                sta     GRP1
                lda     (ScorePtr),y
                sta     GRP1
                ldx     #0
                stx     COLUP1
                stx     COLUP0
                dey
                bpl     .loop

                sty     VDELP0
                sty     VDELP1

                lda     Bank
                cmp     #2
                beq     SetupBonus
                jmp     ScoreEnd

;
; Part 3 -- Bonus timer
;

SetupBonus:     SUBROUTINE

                ; setup playfield

                sty     VBLANK
                lda     #%11000000
                sta     PF1
                lda     #%00001100
                sta     PF2

                ; lower Bonus digits always 0

                stx     ScorePtr+$4             ; X = 0
                stx     ScorePtr+$6

                ; ----------------------------------------------------

                ; check for Pauline's hat in elevator stage
                ; (not enough space in bank 2)

                lda     ItemIndex_R             ; stage 3 ?
                cmp     #3*2
                bne     .skip
                lda     MarioFloor_R            ; Mario on top floor ?
                cmp     #4*4
                bcc     .skip
                lda     MarioHPos_R             ; Mario over hat ?
                cmp     #126
                bcc     .skip
                stx     ItemIndex_W
.skip
                ; ----------------------------------------------------

                ; flip color and clear top digit

                dex                             ; X := -1
              ; ldy     #%11111111              ; Y = -1
                lda     Bonus
                cmp     #$10
                bcs     .write
                ldx     #<Space
                stx     ScorePtr+$0
                ldx     #%11000000
                ldy     #%11111100
.write          stx     Ptr+0
                sty     Ptr+1

                ; compute Bonus pointers

                and     #$F0
                lsr
                sta     ScorePtr+$0
                lda     Bonus
                and     #$0F
                asl
                asl
                asl
                sta     ScorePtr+$2

                ; loop counters

                lda     #8-1
                sta     ScorePtr+$A
                lda     #5-1
                sta     ScorePtr+$8
                lsr                             ; A := 3-1
                sta     ScorePtr+$9
                sta     ScorePtr+$B

                ; choose frame and text colors per stage

                lda     Level_Round
                and     #STAGE_MASK
                tax
                lda     BonusCOLTab0,x
                sta     COLUPF
                lda     BonusCOLTab1,x
                sta     COLUBK

                ; position bitmap on the right

PosBonus:       SUBROUTINE
                sta     WSYNC
                ldx     #7
.loop0          dex
                bne     .loop0
                sta     RESM1
                ldx     #2
.loop1          dex
                bne     .loop1
                sta     RESP0
                sta     RESP1
                sta     HMCLR
                lda     #$10
                sta     HMP1
                lda     #$90
                sta     HMM1

                ; sync circular kernels

DelayTimer:     SUBROUTINE
                sta     WSYNC
                sta.w   HMOVE
                ldx     #14
.loop           dex
                bpl     .loop

                ; draw 'BONUS' graphics

DrawBonus:      SUBROUTINE
.loop           ldy     ScorePtr+$8
                lda     B7_Bonus_GRP0+6,y
                sta     GRP0
                lda     B7_Bonus_GRP1+6,y
                sta     GRP1
                lda     B7_Bonus_GRP2+0,y
                sta     GRP0
                lda     B7_Bonus_GRP3+0,y
                sta     Temp
                ldx     B7_Bonus_GRP4+0,y
                lda     B7_Bonus_GRP5+6,y
                nop
                nop
                ldy     Temp
                sty     VBLANK
                nop
                sty     GRP1
                stx     GRP0
                sta     GRP1
                sta     GRP0
                sta     VBLANK
                dec     ScorePtr+$8
                bmi     .end
                bpl     .loop
.end            nop

                ; draw upper part of frame

DrawFrameHi:    SUBROUTINE
.loop           ldy     ScorePtr+$9
                lda     B7_Bonus_GRP0+3,y
                sta     GRP0
                lda     B7_Bonus_GRP1+3,y
                sta     GRP1
                sta     GRP0
                lda     B7_BonusPF1+3,y
                and     Ptr+0
                sta     PF1
                lda     B7_BonusPF2+3,y
                and     Ptr+1
                sta     PF2
                lda     B7_Bonus_GRP5+3,y
                ldx     #%00000000
                stx     VBLANK
                nop
                sta     GRP1
                nop
                nop
                nop
                sta     GRP0
                sta     VBLANK
                dec     ScorePtr+$9
                bmi     .end
                bpl     .loop
.end            nop

                ; draw remaining time

DrawTime:       SUBROUTINE
.loop           ldy     ScorePtr+$A
                lda     #%11101011
                sta     GRP0
                lda     (ScorePtr+$0),y
                eor     #%11111111
                sta     GRP1
                lda     (ScorePtr+$2),y
                eor     #%11111111
                sta     GRP0
                lda     (ScorePtr+$4),y
                eor     #%11111111
                tax
                lda     (ScorePtr+$6),y
                eor     #%11111111
                ldy     #%00000000
                sty     VBLANK
                ldy     #%11010111
                stx     GRP1
                sta     GRP0
                sty     GRP1
                sty     GRP0
                sty     VBLANK
                dec     ScorePtr+$A
                bmi     .end
                bpl     .loop
.end            nop

                ; draw lower part of frame

DrawFrameLo:    SUBROUTINE
.loop           ldy     ScorePtr+$B
                lda     B7_Bonus_GRP0+0,y
                sta     GRP0
                lda     B7_Bonus_GRP1+0,y
                sta     GRP1
                sta     GRP0
                lda     B7_Bonus_GRP5+0,y
                ldx     B7_BonusPF1,y
                stx     PF1
                ldx     B7_BonusPF2,y
                stx     PF2
                ldx     #%00000000
                nop
                stx     ST_PFOffset,y   ; ST_PFOffset[0-2] := 0
                stx     VBLANK
                sta     GRP1
                stx     ST_PFOffset+3,y ; ST_PFOffset[3-5] := 0
                stx     ST_PFOffset+6,y ; ST_PFOffset[6-8] := 0
                sta     GRP0
                sta     VBLANK
                dec     ScorePtr+$B,x   ; burn one cycle (X = 0)
                bpl     .loop

#if ((>ScoreEnd) - (>DrawFrameLo)) != 1
                ECHO    "ERROR: Can't optimize DrawFrameLo"
#endif

                ; clear channels

ScoreEnd:       txa                     ; two reserve cycles
                sta     GRP0
                sta     GRP1
                sta     GRP0
                sta     ENAM1
                sta     PF1
                sta     PF2
                sta     COLUBK

                sta     CXCLR
                sta     VBLANK
                rts


; ********************************************************************
;
;       Imports
;
; ********************************************************************

; display

B7_VerticalSync:        FUNC_VerticalSync
B7_Overscan:            FUNC_Overscan

; sprite positioning

B7_HPosBZNoHMOVE:       FUNC_HPosBZNoHMOVE
B7_HPosB48Centered:     FUNC_HPosB48Centered
B7_HPosB96Centered:     FUNC_HPosB96Centered

; 96 bit sprites

B7_Bitmap96:            FUNC_Bitmap96 CXCLR, $F800, ScorePtr


; ********************************************************************
;
;       Data Section
;
; ********************************************************************

; --------------------------------------------------------------------
;       CR Text
; --------------------------------------------------------------------

; Encode character graphics address in one byte

        MAC CHAR
                BYTE ((>({1}))<<7)|((<({1}))>>1)
        ENDM

CM_Text:        INCLUDE "copyright.asm"
CM_TextEnd:

CM_TEXT_LINES = (CM_TextEnd-CM_Text) / 12

; --------------------------------------------------------------------
;       IM Tables
; --------------------------------------------------------------------

; stage goal lower digits

IM_GoalDigit1_L:BYTE    <two,  <five, <seven, <zero
IM_GoalDigit2_L:BYTE    <five, <zero, <five,  <zero

; round progression table (US)
                ;                        25m        50m        75m       100m       125m       150m
IM_RoundCodes:  BYTE    ROUND_NEW, ROUND_0S1, ROUND_0S4, ROUND_NOP, ROUND_NOP, ROUND_NOP, ROUND_NOP, ROUND_NOP ; level 1
                BYTE    ROUND_NEW, ROUND_0S1, ROUND_0S3, ROUND_0S4, ROUND_NOP, ROUND_NOP, ROUND_NOP, ROUND_NOP ; level 2
                BYTE    ROUND_NEW, ROUND_0S1, ROUND_0S2, ROUND_0S3, ROUND_0S4, ROUND_NOP, ROUND_NOP, ROUND_NOP ; level 3 (JAP)
                BYTE    ROUND_NEW, ROUND_0S1, ROUND_0S2, ROUND_1S1, ROUND_0S3, ROUND_0S4, ROUND_NOP, ROUND_NOP ; level 4
                BYTE    ROUND_NEW, ROUND_0S1, ROUND_0S2, ROUND_1S1, ROUND_0S3, ROUND_2S1, ROUND_0S4, ROUND_NOP ; level 5

; --------------------------------------------------------------------
;       GO Tables
; --------------------------------------------------------------------

; Kong animation codes

GO_KongAnimTab: BYTE    EN_KONG_DRUM0, EN_KONG_DRUM1, EN_KONG_STAND, EN_KONG_GRIN

; --------------------------------------------------------------------
;       GO Graphics
; --------------------------------------------------------------------

GO_GirderCOL:   BYTE    GO_GIRDER_COL_1     ;         7
                BYTE    GO_GIRDER_COL_0     ;         6
                BYTE    GO_GIRDER_COL_0 + 1 ; ENABL   5
                BYTE    GO_GIRDER_COL_0 + 1 ; ENABL   4
                BYTE    GO_GIRDER_COL_0 + 1 ; ENABL   3
                BYTE    GO_GIRDER_COL_0 + 1 ; ENABL   2
                BYTE    GO_GIRDER_COL_1     ;         1
                BYTE    GO_GIRDER_COL_0     ;         0

GO_GirderPF2:   BYTE    %00000001 ; |       X|   3
                BYTE    %11111101 ; |XXXXXX X|   2
                BYTE    %11111101 ; |XXXXXX X|   1
                BYTE    %00000001 ; |       X|   1

GO_DotGRP:      BYTE    %00000000 ; |        |   7
                BYTE    %00000000 ; |        |   6
                BYTE    %01100110 ; | XX  XX |   5
                BYTE    %01100110 ; | XX  XX |   4
                BYTE    %01100110 ; | XX  XX |   3
                BYTE    %01100110 ; | XX  XX |   2
              ; BYTE    %00000000 ; |        |   1
              ; BYTE    %00000000 ; |        |   0

; --------------------------------------------------------------------
;       Score and Item Tables
; --------------------------------------------------------------------

; lives to NUSIZ table

LivesHMP0:      BYTE    $00,$00,$30,$90
LivesHMM1:      BYTE    $7C,$7D,$13,$04 ; [HM|delay]

; bonus frame colors
                ;       stage 1         stage 2        stage 3         stage 4
BonusCOLTab0:   BYTE    DK_COL_CYAN,    DK_COL_WHITE,  DK_COL_CYAN,    DK_COL_YELLOW
BonusCOLTab1:   BYTE    DK_COL_MAGENTA, DK_COL_ORANGE, DK_COL_MAGENTA, DK_COL_BLUE

; bonus timer tables
                ;       level 1  level 2  level 3  level 4
BonusStartTab:  BYTE    $50,     $60,     $70,     $80
BonusTimerTab:  BYTE    120,     100,     80,      60

; item position and graphics
                ;       stage 1          stage2               stage 3          stage 4
ItemPosTab:     BYTE    $E1,             $A4,                 $87,             $35
ItemGRP_H:      BYTE    >(B3_Oil_GRP_0), >(B4_Handbag_GRP_0), >(B3_Hat_GRP_0), >(B4_Umbrella_GRP_0)
ItemCOL_H:      BYTE    >(B3_Oil_COL_0), >(B4_Handbag_COL_0), >(B3_Hat_COL_0), >(B4_Umbrella_COL_0)

; --------------------------------------------------------------------
;       IM Graphics
; --------------------------------------------------------------------

;
; Kong
;
                ALIGN   $0100

IM_KongGRP0_0:  BYTE    %00000111 ; |     XXX|  31
                BYTE    %00000111 ; |     XXX|  30
                BYTE    %00000011 ; |      XX|  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000001 ; |       X|  26
                BYTE    %00000011 ; |      XX|  25
                BYTE    %00000111 ; |     XXX|  24
                BYTE    %00001111 ; |    XXXX|  23
                BYTE    %00011111 ; |   XXXXX|  22
                BYTE    %00111111 ; |  XXXXXX|  21
                BYTE    %00111111 ; |  XXXXXX|  20
                BYTE    %01111111 ; | XXXXXXX|  19
                BYTE    %01111111 ; | XXXXXXX|  18
                BYTE    %01111111 ; | XXXXXXX|  17
                BYTE    %01111111 ; | XXXXXXX|  16
                BYTE    %01111111 ; | XXXXXXX|  15
                BYTE    %00111111 ; |  XXXXXX|  14
                BYTE    %00111111 ; |  XXXXXX|  13
                BYTE    %00011111 ; |   XXXXX|  12
                BYTE    %00011111 ; |   XXXXX|  11
                BYTE    %00001111 ; |    XXXX|  10
                BYTE    %00001111 ; |    XXXX|   9
                BYTE    %00000111 ; |     XXX|   8
                BYTE    %00000111 ; |     XXX|   7
                BYTE    %00000011 ; |      XX|   6
                BYTE    %00000011 ; |      XX|   5
                BYTE    %00000001 ; |       X|   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

IM_KongGRP1_0:  BYTE    %11111111 ; |XXXXXXXX|  31
                BYTE    %11111111 ; |XXXXXXXX|  30
                BYTE    %11111111 ; |XXXXXXXX|  29
                BYTE    %01111111 ; | XXXXXXX|  28
                BYTE    %11111111 ; |XXXXXXXX|  27
                BYTE    %11111111 ; |XXXXXXXX|  26
                BYTE    %11111111 ; |XXXXXXXX|  25
                BYTE    %11111111 ; |XXXXXXXX|  24
                BYTE    %11111111 ; |XXXXXXXX|  23
                BYTE    %11111111 ; |XXXXXXXX|  22
                BYTE    %11111111 ; |XXXXXXXX|  21
                BYTE    %11011111 ; |XX XXXXX|  20
                BYTE    %10111111 ; |X XXXXXX|  19
                BYTE    %10111111 ; |X XXXXXX|  18
                BYTE    %10111111 ; |X XXXXXX|  17
                BYTE    %10010111 ; |X  X XXX|  16
                BYTE    %11001111 ; |XX  XXXX|  15
                BYTE    %11101111 ; |XXX XXXX|  14
                BYTE    %11111111 ; |XXXXXXXX|  13
                BYTE    %11111111 ; |XXXXXXXX|  12
                BYTE    %11111111 ; |XXXXXXXX|  11
                BYTE    %11111111 ; |XXXXXXXX|  10
                BYTE    %11111111 ; |XXXXXXXX|   9
                BYTE    %11111111 ; |XXXXXXXX|   8
                BYTE    %11111111 ; |XXXXXXXX|   7
                BYTE    %11111111 ; |XXXXXXXX|   6
                BYTE    %11111111 ; |XXXXXXXX|   5
                BYTE    %11111111 ; |XXXXXXXX|   4
                BYTE    %11111110 ; |XXXXXXX |   3
                BYTE    %01111111 ; | XXXXXXX|   2
                BYTE    %00111111 ; |  XXXXXX|   1
                BYTE    %00011110 ; |   XXXX |   0

IM_KongGRP2_0:  BYTE    %11110000 ; |XXXX    |  31
                BYTE    %11111000 ; |XXXXX   |  30
                BYTE    %11110000 ; |XXXX    |  29
                BYTE    %11111111 ; |XXXXXXXX|  28
                BYTE    %11111111 ; |XXXXXXXX|  27
                BYTE    %11111111 ; |XXXXXXXX|  26
                BYTE    %11111111 ; |XXXXXXXX|  25
                BYTE    %11111111 ; |XXXXXXXX|  24
                BYTE    %11111111 ; |XXXXXXXX|  23
                BYTE    %11111111 ; |XXXXXXXX|  22
                BYTE    %11111111 ; |XXXXXXXX|  21
                BYTE    %11111111 ; |XXXXXXXX|  20
                BYTE    %11111111 ; |XXXXXXXX|  19
                BYTE    %11111111 ; |XXXXXXXX|  18
                BYTE    %11111111 ; |XXXXXXXX|  17
                BYTE    %11110000 ; |XXXX    |  16
                BYTE    %00111000 ; |  XXX   |  15
                BYTE    %11110001 ; |XXXX   X|  14
                BYTE    %11111100 ; |XXXXXX  |  13
                BYTE    %11111111 ; |XXXXXXXX|  12
                BYTE    %11111111 ; |XXXXXXXX|  11
                BYTE    %11111111 ; |XXXXXXXX|  10
                BYTE    %11111111 ; |XXXXXXXX|   9
                BYTE    %11111100 ; |XXXXXX  |   8
                BYTE    %11111111 ; |XXXXXXXX|   7
                BYTE    %11110011 ; |XXXX  XX|   6
                BYTE    %11110001 ; |XXXX   X|   5
                BYTE    %11111111 ; |XXXXXXXX|   4
                BYTE    %11111111 ; |XXXXXXXX|   3
                BYTE    %01111111 ; | XXXXXXX|   2
                BYTE    %00111111 ; |  XXXXXX|   1
                BYTE    %00011110 ; |   XXXX |   0

IM_KongGRP3_0:  BYTE    %00001111 ; |    XXXX|  31
                BYTE    %00011111 ; |   XXXXX|  30
                BYTE    %00001111 ; |    XXXX|  29
                BYTE    %11111111 ; |XXXXXXXX|  28
                BYTE    %11111111 ; |XXXXXXXX|  27
                BYTE    %11111111 ; |XXXXXXXX|  26
                BYTE    %11111111 ; |XXXXXXXX|  25
                BYTE    %11111111 ; |XXXXXXXX|  24
                BYTE    %11111111 ; |XXXXXXXX|  23
                BYTE    %11111111 ; |XXXXXXXX|  22
                BYTE    %11111111 ; |XXXXXXXX|  21
                BYTE    %11111111 ; |XXXXXXXX|  20
                BYTE    %11111111 ; |XXXXXXXX|  19
                BYTE    %11111111 ; |XXXXXXXX|  18
                BYTE    %11111111 ; |XXXXXXXX|  17
                BYTE    %00011111 ; |   XXXXX|  16
                BYTE    %10000011 ; |X     XX|  15
                BYTE    %11000100 ; |XX   X  |  14
                BYTE    %10001110 ; |X   XXX |  13
                BYTE    %00000100 ; |     X  |  12
                BYTE    %10000001 ; |X      X|  11
                BYTE    %11000011 ; |XX    XX|  10
                BYTE    %11111111 ; |XXXXXXXX|   9
                BYTE    %01111111 ; | XXXXXXX|   8
                BYTE    %11111111 ; |XXXXXXXX|   7
                BYTE    %10011111 ; |X  XXXXX|   6
                BYTE    %00011111 ; |   XXXXX|   5
                BYTE    %11111111 ; |XXXXXXXX|   4
                BYTE    %11111111 ; |XXXXXXXX|   3
                BYTE    %11111110 ; |XXXXXXX |   2
                BYTE    %11111100 ; |XXXXXX  |   1
                BYTE    %01111000 ; | XXXX   |   0

IM_KongGRP4_0:  BYTE    %11111111 ; |XXXXXXXX|  31
                BYTE    %11111111 ; |XXXXXXXX|  30
                BYTE    %11111111 ; |XXXXXXXX|  29
                BYTE    %11111110 ; |XXXXXXX |  28
                BYTE    %11111111 ; |XXXXXXXX|  27
                BYTE    %11111111 ; |XXXXXXXX|  26
                BYTE    %11111111 ; |XXXXXXXX|  25
                BYTE    %11111111 ; |XXXXXXXX|  24
                BYTE    %11111111 ; |XXXXXXXX|  23
                BYTE    %11111111 ; |XXXXXXXX|  22
                BYTE    %11111111 ; |XXXXXXXX|  21
                BYTE    %11111011 ; |XXXXX XX|  20
                BYTE    %11111101 ; |XXXXXX X|  19
                BYTE    %11111101 ; |XXXXXX X|  18
                BYTE    %11111101 ; |XXXXXX X|  17
                BYTE    %11100011 ; |XXX   XX|  16
                BYTE    %11111011 ; |XXXXX XX|  15
                BYTE    %11110111 ; |XXXX XXX|  14
                BYTE    %11111111 ; |XXXXXXXX|  13
                BYTE    %11111111 ; |XXXXXXXX|  12
                BYTE    %11111111 ; |XXXXXXXX|  11
                BYTE    %11111111 ; |XXXXXXXX|  10
                BYTE    %11111111 ; |XXXXXXXX|   9
                BYTE    %11111111 ; |XXXXXXXX|   8
                BYTE    %11111111 ; |XXXXXXXX|   7
                BYTE    %11111111 ; |XXXXXXXX|   6
                BYTE    %11111111 ; |XXXXXXXX|   5
                BYTE    %11111111 ; |XXXXXXXX|   4
                BYTE    %01111111 ; | XXXXXXX|   3
                BYTE    %11111110 ; |XXXXXXX |   2
                BYTE    %11111100 ; |XXXXXX  |   1
                BYTE    %01111000 ; | XXXX   |   0

IM_KongGRP5_0:  BYTE    %11100000 ; |XXX     |  31
                BYTE    %11100000 ; |XXX     |  30
                BYTE    %11000000 ; |XX      |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %10000000 ; |X       |  26
                BYTE    %11000000 ; |XX      |  25
                BYTE    %11100000 ; |XXX     |  24
                BYTE    %11110000 ; |XXXX    |  23
                BYTE    %11111000 ; |XXXXX   |  22
                BYTE    %11111100 ; |XXXXXX  |  21
                BYTE    %11111100 ; |XXXXXX  |  20
                BYTE    %11111110 ; |XXXXXXX |  19
                BYTE    %11111110 ; |XXXXXXX |  18
                BYTE    %11111110 ; |XXXXXXX |  17
                BYTE    %11111110 ; |XXXXXXX |  16
                BYTE    %11111110 ; |XXXXXXX |  15
                BYTE    %11111100 ; |XXXXXX  |  14
                BYTE    %11111100 ; |XXXXXX  |  13
                BYTE    %11111100 ; |XXXXXX  |  12
                BYTE    %11111000 ; |XXXXX   |  11
                BYTE    %11111000 ; |XXXXX   |  10
                BYTE    %11110000 ; |XXXX    |   9
                BYTE    %11110000 ; |XXXX    |   8
                BYTE    %11100000 ; |XXX     |   7
                BYTE    %11100000 ; |XXX     |   6
                BYTE    %11000000 ; |XX      |   5
                BYTE    %10000000 ; |X       |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

IM_KongGRP0_1:  BYTE    %00000111 ; |     XXX|  31
                BYTE    %00000110 ; |     XX |  30
                BYTE    %00000011 ; |      XX|  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %00000001 ; |       X|  25
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

IM_KongGRP1_1:  BYTE    %11111000 ; |XXXXX   |  31
                BYTE    %10000000 ; |X       |  30
                BYTE    %00111100 ; |  XXXX  |  29
                BYTE    %01111110 ; | XXXXXX |  28
                BYTE    %01111000 ; | XXXX   |  27
                BYTE    %10110000 ; |X XX    |  26
                BYTE    %01110000 ; | XXX    |  25
                BYTE    %10111110 ; |X XXXXX |  24
                BYTE    %00111100 ; |  XXXX  |  23
                BYTE    %00010001 ; |   X   X|  22
                BYTE    %00000111 ; |     XXX|  21
                BYTE    %00001111 ; |    XXXX|  20
                BYTE    %00011010 ; |   XX X |  19
                BYTE    %00010101 ; |   X X X|  18
                BYTE    %00000100 ; |     X  |  17
                BYTE    %00000001 ; |       X|  16
                BYTE    %00000011 ; |      XX|  15
                BYTE    %00001111 ; |    XXXX|  14
                BYTE    %00011111 ; |   XXXXX|  13
                BYTE    %00011111 ; |   XXXXX|  12
                BYTE    %00011111 ; |   XXXXX|  11
                BYTE    %00001111 ; |    XXXX|  10
                BYTE    %00000111 ; |     XXX|   9
                BYTE    %00001010 ; |    X X |   8
                BYTE    %00110110 ; |  XX XX |   7
                BYTE    %00111010 ; |  XXX X |   6
                BYTE    %00111010 ; |  XXX X |   5
                BYTE    %00011100 ; |   XXX  |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

IM_KongGRP4_1:  BYTE    %00011111 ; |   XXXXX|  31
                BYTE    %00000001 ; |       X|  30
                BYTE    %00111100 ; |  XXXX  |  29
                BYTE    %01111110 ; | XXXXXX |  28
                BYTE    %00011110 ; |   XXXX |  27
                BYTE    %00001101 ; |    XX X|  26
                BYTE    %00001110 ; |    XXX |  25
                BYTE    %01111101 ; | XXXXX X|  24
                BYTE    %00111100 ; |  XXXX  |  23
                BYTE    %00001000 ; |    X   |  22
                BYTE    %10000000 ; |X       |  21
                BYTE    %11100000 ; |XXX     |  20
                BYTE    %10110000 ; |X XX    |  19
                BYTE    %01010000 ; | X X    |  18
                BYTE    %10100000 ; |X X     |  17
                BYTE    %00000000 ; |        |  16
                BYTE    %10000000 ; |X       |  15
                BYTE    %10000000 ; |X       |  14
                BYTE    %11000000 ; |XX      |  13
                BYTE    %11100000 ; |XXX     |  12
                BYTE    %11100000 ; |XXX     |  11
                BYTE    %11100000 ; |XXX     |  10
                BYTE    %11100000 ; |XXX     |   9
                BYTE    %01010000 ; | X X    |   8
                BYTE    %01101100 ; | XX XX  |   7
                BYTE    %01011100 ; | X XXX  |   6
                BYTE    %01011100 ; | X XXX  |   5
                BYTE    %00111000 ; |  XXX   |   4
                BYTE    %00000000 ; |        |   3
              ; BYTE    %00000000 ; |        |   2
              ; BYTE    %00000000 ; |        |   1
              ; BYTE    %00000000 ; |        |   0

IM_KongGRP2_1:  BYTE    %00000000 ; |        |  31
                BYTE    %00000000 ; |        |  30
                BYTE    %00000000 ; |        |  29
                BYTE    %00000101 ; |     X X|  28
                BYTE    %00111011 ; |  XXX XX|  27
                BYTE    %01010101 ; | X X X X|  26
                BYTE    %10101010 ; |X X X X |  25
                BYTE    %01010101 ; | X X X X|  24
                BYTE    %00000111 ; |     XXX|  23
                BYTE    %11111000 ; |XXXXX   |  22
                BYTE    %11001100 ; |XX  XX  |  21
                BYTE    %11111100 ; |XXXXXX  |  20
                BYTE    %11011011 ; |XX XX XX|  19
                BYTE    %01100000 ; | XX     |  18
                BYTE    %00011111 ; |   XXXXX|  17
                BYTE    %11110000 ; |XXXX    |  16
                BYTE    %00111000 ; |  XXX   |  15
                BYTE    %11110001 ; |XXXX   X|  14
                BYTE    %11111100 ; |XXXXXX  |  13
                BYTE    %11111111 ; |XXXXXXXX|  12
                BYTE    %11111111 ; |XXXXXXXX|  11
                BYTE    %11111111 ; |XXXXXXXX|  10
                BYTE    %01111111 ; | XXXXXXX|   9
                BYTE    %00101100 ; |  X XX  |   8
                BYTE    %00010000 ; |   X    |   7
                BYTE    %00110001 ; |  XX   X|   6
                BYTE    %01110001 ; | XXX   X|   5
                BYTE    %01111111 ; | XXXXXXX|   4
                BYTE    %00111111 ; |  XXXXXX|   3
                BYTE    %00011110 ; |   XXXX |   2
              ; BYTE    %00000000 ; |        |   1
              ; BYTE    %00000000 ; |        |   0

IM_KongGRP3_1:  BYTE    %00000000 ; |        |  31
                BYTE    %00000000 ; |        |  30
                BYTE    %00000000 ; |        |  29
                BYTE    %01100000 ; | XX     |  28
                BYTE    %11011100 ; |XX XXX  |  27
                BYTE    %01101010 ; | XX X X |  26
                BYTE    %10110101 ; |X XX X X|  25
                BYTE    %01011010 ; | X XX X |  24
                BYTE    %11000000 ; |XX      |  23
                BYTE    %00111111 ; |  XXXXXX|  22
                BYTE    %01100111 ; | XX  XXX|  21
                BYTE    %01111111 ; | XXXXXXX|  20
                BYTE    %10101011 ; |X X X XX|  19
                BYTE    %00000101 ; |     X X|  18
                BYTE    %11111000 ; |XXXXX   |  17
                BYTE    %00011110 ; |   XXXX |  16
                BYTE    %10000011 ; |X     XX|  15
                BYTE    %11000100 ; |XX   X  |  14
                BYTE    %10001110 ; |X   XXX |  13
                BYTE    %00000100 ; |     X  |  12
                BYTE    %10000001 ; |X      X|  11
                BYTE    %11000011 ; |XX    XX|  10
                BYTE    %11111110 ; |XXXXXXX |   9
                BYTE    %01101100 ; | XX XX  |   8
                BYTE    %00011000 ; |   XX   |   7
                BYTE    %00011100 ; |   XXX  |   6
                BYTE    %00011110 ; |   XXXX |   5
                BYTE    %11111110 ; |XXXXXXX |   4
                BYTE    %11111100 ; |XXXXXX  |   3
                BYTE    %01111000 ; | XXXX   |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

IM_KongGRP5_1:  BYTE    %11100000 ; |XXX     |  31
                BYTE    %01100000 ; | XX     |  30
                BYTE    %11000000 ; |XX      |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %10000000 ; |X       |  25
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

; playfield bitmap behind Kong

IM_KongPF2:     BYTE    $00,$00,$00,$00,$00,$00,$00,$00
                BYTE    $00,$00,$00,$00,$00,$00,$00,$80
                BYTE    $C0,$C0,$C0,$C0,$C0,$C0,$00,$00
                BYTE    $00,$80,$80,$00,$00,$00,$00,$00

; --------------------------------------------------------------------

IM_CheckExit:   SUBROUTINE

                ; check both buttons

                lda     INPT4
                and     INPT5
                bmi     .end

                ; fast forward music

                lda     #NOTE_INTERMISSION_END_0        ; end track 0
                sta     NPNote
                lda     #NOTE_INTERMISSION_END_1        ; end track 1
                sta     NPNote+1
                lda     #1                              ; jump to next note
                sta     NPTimer
                sta     NPTimer+1
                sta     IM_Exiting                      ; avoid deadlock
.end            rts

; --------------------------------------------------------------------
;       B7 Graphics
; --------------------------------------------------------------------

;
; Mario
;

B7_MarioGRP0:   BYTE    %01100110 ; | XX  XX |   7
                BYTE    %00111100 ; |  XXXX  |   6
                BYTE    %00111100 ; |  XXXX  |   5
                BYTE    %00100100 ; |  X  X  |   4
                BYTE    %01100000 ; | XX     |   3
                BYTE    %01110100 ; | XXX X  |   2
                BYTE    %00101110 ; |  X XXX |   1
                BYTE    %00111100 ; |  XXXX  |   0

B7_MarioCOL0:   BYTE    COL_82 ; |181AA7|   7
                BYTE    COL_42 ; |A71A1A|   6
                BYTE    COL_42 ; |A71A1A|   5
                BYTE    COL_42 ; |A71A1A|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_3E ; |FCBC74|   2
                BYTE    COL_82 ; |181AA7|   1
                BYTE    COL_42 ; |A71A1A|   0

B7_MarioCOL1:   BYTE    COL_00 ; |000000|   7
                BYTE    COL_3E ; |FCBC74|   6
                BYTE    COL_3E ; |FCBC74|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_3E ; |FCBC74|   3
                BYTE    COL_82 ; |181AA7|   2
                BYTE    COL_3E ; |FCBC74|   1
                BYTE    COL_00 ; |000000|   0   ; B7_MarioHMOVE-1 must contain 0

B7_MarioHMOVE:  BYTE    $00     ;  0         7
                BYTE    $D0     ; +3         6
                BYTE    $20 + 2 ; -2 ENABL   5
                BYTE    $F0     ; +1         4
                BYTE    $20 + 2 ; -2 ENABL   3
                BYTE    $D0     ; +3         2
                BYTE    $00     ;  0         1
                BYTE    $00     ;  0         0

;
; Bonus
;

B7_Bonus_GRP0:  BYTE    %11110000 ; |XXXX    |  18
                BYTE    %11101111 ; |XXX XXXX|  17
                BYTE    %11101000 ; |XXX X   |  16
                ;
                BYTE    %11101011 ; |XXX X XX|   7
                BYTE    %11101000 ; |XXX X   |   6
                BYTE    %11101111 ; |XXX XXXX|   5
                ;
                BYTE    %11101111 ; |XXX XXXX|   4
                BYTE    %11101111 ; |XXX XXXX|   3
                BYTE    %11101111 ; |XXX XXXX|   2
                BYTE    %11101111 ; |XXX XXXX|   1
                BYTE    %11110001 ; |XXXX   X|   0

B7_Bonus_GRP1:  BYTE    %00000000 ; |        |  18
                BYTE    %11111111 ; |XXXXXXXX|  17
                BYTE    %00000000 ; |        |  16
                ;
                BYTE    %11111111 ; |XXXXXXXX|   7
                BYTE    %00000000 ; |        |   6
                BYTE    %11111111 ; |XXXXXXXX|   5
                ;
                BYTE    %00000111 ; |     XXX|   4
                BYTE    %00011010 ; |   XX X |   3
                BYTE    %00000110 ; |     XX |   2
                BYTE    %00011010 ; |   XX X |   1
                BYTE    %00000111 ; |     XXX|   0

B7_Bonus_GRP2:  BYTE    %00001100 ; |    XX  |   4
                BYTE    %00110100 ; |  XX X  |   3
                BYTE    %00110100 ; |  XX X  |   2
                BYTE    %00110100 ; |  XX X  |   1
                BYTE    %00001100 ; |    XX  |   0

B7_Bonus_GRP3:  BYTE    %11011000 ; |XX XX   |   4
                BYTE    %10010001 ; |X  X   X|   3
                BYTE    %01010001 ; | X X   X|   2
                BYTE    %11010001 ; |XX X   X|   1
                BYTE    %11010001 ; |XX X   X|   0

B7_Bonus_GRP4:  BYTE    %01100001 ; | XX    X|   4
                BYTE    %10111100 ; |X XXXX  |   3
                BYTE    %10100000 ; |X X     |   2
                BYTE    %10100111 ; |X X  XXX|   1
                BYTE    %10110000 ; |X XX    |   0

B7_Bonus_GRP5:  BYTE    %00001111 ; |     XXX|  18
                BYTE    %11110111 ; |XXXXX XX|  17
                BYTE    %00010111 ; |    X XX|  16
                ;
                BYTE    %11010111 ; |XXX X XX|   7
                BYTE    %00010111 ; |    X XX|   6
                BYTE    %11110111 ; |XXXXX XX|   5
                ;
                BYTE    %11110111 ; |XXXXX XX|   4
                BYTE    %11110111 ; |XXXXX XX|   3
                BYTE    %11110111 ; |XXXXX XX|   2
                BYTE    %11110111 ; |XXXXX XX|   1
                BYTE    %10001111 ; |XX   XXX|   0

B7_BonusPF1:    BYTE    %11111111 ; |XXXXXXXX|  18
                BYTE    %10000000 ; |X       |  17
                BYTE    %10000000 ; |X       |  16
                ;
                BYTE    %10111111 ; |X XXXXXX|   7
                BYTE    %10000000 ; |X       |   6
                BYTE    %10000000 ; |X       |   5

B7_BonusPF2:    BYTE    %00001111 ; |    XXXX|  18
                BYTE    %00001000 ; |    X   |  17
                BYTE    %00001000 ; |    X   |  16
                ;
                BYTE    %00001011 ; |    X XX|   7
                BYTE    %00001000 ; |    X   |   6
                BYTE    %00001000 ; |    X   |   5

;
; Font
;
                ALIGN   $0200

B7_Font:        DATA_Font

;
; Mario GRP
;

B7_MarioWalk0_Hammer0_GRP_0:

                BYTE    %01110111 ; | XXX XXX|  15
                BYTE    %00110011 ; |  XX  XX|  14
                BYTE    %00000000 ; |        |  13
                BYTE    %00000000 ; |        |  12
                BYTE    %00000000 ; |        |  11
                BYTE    %00000000 ; |        |  10
                BYTE    %00000110 ; |     XX |   9
                BYTE    %00101110 ; |  X XXX |   8
                BYTE    %00011110 ; |   XXXX |   7
                BYTE    %00011100 ; |   XXX  |   6
                BYTE    %01111011 ; | XXXX XX|   5
                BYTE    %00111101 ; |  XXXX X|   4
                BYTE    %00011101 ; |   XXX X|   3
                BYTE    %00000110 ; |     XX |   2
                BYTE    %01100110 ; | XX  XX |   1
                BYTE    %00000100 ; |     X  |   0

B7_MarioWalk0_Hammer0_GRP_1:

                BYTE    %00000000 ; |        |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %00110111 ; |  XX XXX|  13
                BYTE    %01111111 ; | XXXXXXX|  12
                BYTE    %01111111 ; | XXXXXXX|  11
                BYTE    %01111110 ; | XXXXXX |  10
                BYTE    %00111000 ; |  XXX   |   9
                BYTE    %00010000 ; |   X    |   8
                BYTE    %00000000 ; |        |   7
                BYTE    %00100000 ; |  X     |   6
                BYTE    %00000100 ; |     X  |   5
                BYTE    %11000010 ; |XX    X |   4
                BYTE    %01100010 ; | XX   X |   3
                BYTE    %00011000 ; |   XX   |   2
                BYTE    %00011000 ; |   XX   |   1
                BYTE    %00011000 ; |   XX   |   0

B7_MarioWalk0_Hammer1_GRP_0:

                BYTE    %01110111 ; | XXX XXX|  15
                BYTE    %00110011 ; |  XX  XX|  14
                BYTE    %00000000 ; |        |  13
                BYTE    %00000000 ; |        |  12
                BYTE    %00000000 ; |        |  11
                BYTE    %00000010 ; |      X |  10
                BYTE    %00111111 ; |  XXXXXX|   9
                BYTE    %00111111 ; |  XXXXXX|   8
                BYTE    %00010110 ; |   X XX |   7
                BYTE    %00000000 ; |        |   6
                BYTE    %01110011 ; | XXX  XX|   5
                BYTE    %00101101 ; |  X XX X|   4
                BYTE    %00010101 ; |   X X X|   3
                BYTE    %00001110 ; |    XXX |   2
              ; BYTE    %00000000 ; |        |   1
              ; BYTE    %00000000 ; |        |   0

B7_MarioWalk0_Hammer1_GRP_1:

                BYTE    %00000000 ; |        |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %00110111 ; |  XX XXX|  13
                BYTE    %01111111 ; | XXXXXXX|  12
                BYTE    %01111111 ; | XXXXXXX|  11
                BYTE    %01111101 ; | XXXXX X|  10
                BYTE    %11000000 ; |XX      |   9
                BYTE    %11000000 ; |XX      |   8
                BYTE    %00001000 ; |    X   |   7
                BYTE    %00111100 ; |  XXXX  |   6
                BYTE    %00001100 ; |    XX  |   5
                BYTE    %11010010 ; |XX X  X |   4
                BYTE    %01101010 ; | XX X X |   3
                BYTE    %00010000 ; |   X    |   2
                BYTE    %01111110 ; | XXXXXX |   1
                BYTE    %00011100 ; |   XXX  |   0

B7_MarioWalk1_Hammer0_GRP_0:

                BYTE    %00000110 ; |     XX |  15
                BYTE    %00000011 ; |      XX|  14
                BYTE    %10000011 ; |X     XX|  13
                BYTE    %10000000 ; |X       |  12
                BYTE    %10000000 ; |X       |  11
                BYTE    %10000000 ; |X       |  10
                BYTE    %00000110 ; |     XX |   9
                BYTE    %00000110 ; |     XX |   8
                BYTE    %00011110 ; |   XXXX |   7
                BYTE    %00011100 ; |   XXX  |   6
                BYTE    %01111111 ; | XXXXXXX|   5
                BYTE    %00111101 ; |  XXXX X|   4
                BYTE    %00011101 ; |   XXX X|   3
                BYTE    %00000110 ; |     XX |   2
                BYTE    %01100110 ; | XX  XX |   1
                BYTE    %00000100 ; |     X  |   0

B7_MarioWalk1_Hammer0_GRP_1:

                BYTE    %00000000 ; |        |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %01100100 ; | XX  X  |  13
                BYTE    %01111110 ; | XXXXXX |  12
                BYTE    %01111110 ; | XXXXXX |  11
                BYTE    %00111100 ; |  XXXX  |  10
                BYTE    %00111000 ; |  XXX   |   9
                BYTE    %00011000 ; |   XX   |   8
                BYTE    %00000000 ; |        |   7
                BYTE    %00100000 ; |  X     |   6
                BYTE    %00000000 ; |        |   5
                BYTE    %11000010 ; |XX    X |   4
                BYTE    %01100010 ; | XX   X |   3
                BYTE    %00011000 ; |   XX   |   2
                BYTE    %00011000 ; |   XX   |   1
                BYTE    %00011000 ; |   XX   |   0

B7_MarioWalk1_Hammer1_GRP_0:

                BYTE    %00000110 ; |     XX |  15
                BYTE    %00000011 ; |      XX|  14
                BYTE    %10000011 ; |X     XX|  13
                BYTE    %10000000 ; |X       |  12
                BYTE    %10000000 ; |X       |  11
                BYTE    %10000100 ; |X    X  |  10
                BYTE    %00111110 ; |  XXXXX |   9
                BYTE    %00111110 ; |  XXXXX |   8
                BYTE    %00010100 ; |   X X  |   7
                BYTE    %00000000 ; |        |   6
                BYTE    %01110011 ; | XXX  XX|   5
                BYTE    %00101101 ; |  X XX X|   4
                BYTE    %00010101 ; |   X X X|   3
                BYTE    %00001110 ; |    XXX |   2
              ; BYTE    %00000000 ; |        |   1
              ; BYTE    %00000000 ; |        |   0

B7_MarioWalk1_Hammer1_GRP_1:

                BYTE    %00000000 ; |        |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %01100100 ; | XX  X  |  13
                BYTE    %01111110 ; | XXXXXX |  12
                BYTE    %01111110 ; | XXXXXX |  11
                BYTE    %00111000 ; |  XXX   |  10
                BYTE    %11000000 ; |XX      |   9
                BYTE    %11000000 ; |XX      |   8
                BYTE    %00001000 ; |    X   |   7
                BYTE    %00111100 ; |  XXXX  |   6
                BYTE    %00001100 ; |    XX  |   5
                BYTE    %11010010 ; |XX X  X |   4
                BYTE    %01101010 ; | XX X X |   3
                BYTE    %00010000 ; |   X    |   2
                BYTE    %01111110 ; | XXXXXX |   1
                BYTE    %00011100 ; |   XXX  |   0

B7_MarioWalk2_Hammer0_GRP_0:

                BYTE    %00111000 ; |  XXX   |  15
                BYTE    %00011001 ; |   XX  X|  14
                BYTE    %00000001 ; |       X|  13
                BYTE    %00000001 ; |       X|  12
                BYTE    %00000001 ; |       X|  11
                BYTE    %00001000 ; |    X   |  10
                BYTE    %00001110 ; |    XXX |   9
                BYTE    %00011110 ; |   XXXX |   8
                BYTE    %00011100 ; |   XXX  |   7
                BYTE    %01111011 ; | XXXX XX|   6
                BYTE    %00111101 ; |  XXXX X|   5
                BYTE    %00011101 ; |   XXX X|   4
                BYTE    %00011110 ; |   XXXX |   3
                BYTE    %01100110 ; | XX  XX |   2
                BYTE    %00000100 ; |     X  |   1
              ; BYTE    %00000000 ; |        |   0

B7_MarioWalk2_Hammer0_GRP_1:

                BYTE    %00000000 ; |        |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %00011010 ; |   XX X |  13
                BYTE    %00111110 ; |  XXXXX |  12
                BYTE    %00111110 ; |  XXXXX |  11
                BYTE    %00110100 ; |  XX X  |  10
                BYTE    %00010000 ; |   X    |   9
                BYTE    %00000000 ; |        |   8
                BYTE    %00100000 ; |  X     |   7
                BYTE    %00000100 ; |     X  |   6
                BYTE    %11000010 ; |XX    X |   5
                BYTE    %01100010 ; | XX   X |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00011000 ; |   XX   |   2
                BYTE    %00011000 ; |   XX   |   1
                BYTE    %00011000 ; |   XX   |   0

B7_MarioWalk2_Hammer1_GRP_0:

                BYTE    %00111000 ; |  XXX   |  15
                BYTE    %00011001 ; |   XX  X|  14
                BYTE    %00000001 ; |       X|  13
                BYTE    %00000001 ; |       X|  12
                BYTE    %00000001 ; |       X|  11
                BYTE    %00111000 ; |  XXX   |  10
                BYTE    %00111110 ; |  XXXXX |   9
                BYTE    %00011110 ; |   XXXX |   8
                BYTE    %00000000 ; |        |   7
                BYTE    %01110011 ; | XXX  XX|   6
                BYTE    %00101101 ; |  X XX X|   5
                BYTE    %00010101 ; |   X X X|   4
                BYTE    %00001110 ; |    XXX |   3
                BYTE    %00000000 ; |        |   2
              ; BYTE    %00000000 ; |        |   1
              ; BYTE    %00000000 ; |        |   0

B7_MarioWalk2_Hammer1_GRP_1:

                BYTE    %00000000 ; |        |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %00011010 ; |   XX X |  13
                BYTE    %00111110 ; |  XXXXX |  12
                BYTE    %00111110 ; |  XXXXX |  11
                BYTE    %00000100 ; |     X  |  10
                BYTE    %11000000 ; |XX      |   9
                BYTE    %11000000 ; |XX      |   8
                BYTE    %00111100 ; |  XXXX  |   7
                BYTE    %00001100 ; |    XX  |   6
                BYTE    %11010010 ; |XX X  X |   5
                BYTE    %01101010 ; | XX X X |   4
                BYTE    %00010000 ; |   X    |   3
                BYTE    %01111110 ; | XXXXXX |   2
                BYTE    %00011100 ; |   XXX  |   1
                BYTE    %00000000 ; |        |   0

;
; Mario COL
;

B7_MarioWalk0_Hammer0_COL_0:

                BYTE    COL_82 ; |181AA7|  15
                BYTE    COL_82 ; |181AA7|  14
                BYTE    COL_00 ; |000000|  13
                BYTE    COL_00 ; |000000|  12
                BYTE    COL_00 ; |000000|  11
                BYTE    COL_00 ; |000000|  10
                BYTE    COL_82 ; |181AA7|   9
                BYTE    COL_82 ; |181AA7|   8
                BYTE    COL_82 ; |181AA7|   7
                BYTE    COL_82 ; |181AA7|   6
                BYTE    COL_82 ; |181AA7|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_82 ; |181AA7|   2
                BYTE    COL_42 ; |A71A1A|   1
                BYTE    COL_42 ; |A71A1A|   0

B7_MarioWalk0_Hammer0_COL_1:

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
                BYTE    COL_3E ; |FCBC74|   1
                BYTE    COL_3E ; |FCBC74|   0

B7_MarioWalk0_Hammer1_COL_0:

                BYTE    COL_82 ; |181AA7|  15
                BYTE    COL_82 ; |181AA7|  14
                BYTE    COL_00 ; |000000|  13
                BYTE    COL_00 ; |000000|  12
                BYTE    COL_00 ; |000000|  11
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

B7_MarioWalk0_Hammer1_COL_1:

                BYTE    COL_00 ; |000000|  15
                BYTE    COL_00 ; |000000|  14
                BYTE    COL_42 ; |A71A1A|  13
                BYTE    COL_42 ; |A71A1A|  12
                BYTE    COL_42 ; |A71A1A|  11
                BYTE    COL_42 ; |A71A1A|  10
                BYTE    COL_3E ; |FCBC74|   9
                BYTE    COL_3E ; |FCBC74|   8
                BYTE    COL_42 ; |A71A1A|   7
                BYTE    COL_3E ; |FCBC74|   6
                BYTE    COL_3E ; |FCBC74|   5
                BYTE    COL_3E ; |FCBC74|   4
                BYTE    COL_3E ; |FCBC74|   3
                BYTE    COL_3E ; |FCBC74|   2
                BYTE    COL_42 ; |A71A1A|   1
                BYTE    COL_42 ; |A71A1A|   0

B7_MarioWalk1_Hammer0_COL_0:

                BYTE    COL_82 ; |181AA7|  15
                BYTE    COL_82 ; |181AA7|  14
                BYTE    COL_82 ; |181AA7|  13
                BYTE    COL_82 ; |181AA7|  12
                BYTE    COL_82 ; |181AA7|  11
                BYTE    COL_82 ; |181AA7|  10
                BYTE    COL_82 ; |181AA7|   9
                BYTE    COL_82 ; |181AA7|   8
                BYTE    COL_82 ; |181AA7|   7
                BYTE    COL_82 ; |181AA7|   6
                BYTE    COL_82 ; |181AA7|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_82 ; |181AA7|   2
                BYTE    COL_42 ; |A71A1A|   1
                BYTE    COL_42 ; |A71A1A|   0

B7_MarioWalk1_Hammer0_COL_1:

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
                BYTE    COL_00 ; |000000|   5
                BYTE    COL_3E ; |FCBC74|   4
                BYTE    COL_3E ; |FCBC74|   3
                BYTE    COL_3E ; |FCBC74|   2
                BYTE    COL_3E ; |FCBC74|   1
                BYTE    COL_3E ; |FCBC74|   0

B7_MarioWalk1_Hammer1_COL_0:

                BYTE    COL_82 ; |181AA7|  15
                BYTE    COL_82 ; |181AA7|  14
                BYTE    COL_82 ; |181AA7|  13
                BYTE    COL_82 ; |181AA7|  12
                BYTE    COL_82 ; |181AA7|  11
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

B7_MarioWalk1_Hammer1_COL_1:

                BYTE    COL_00 ; |000000|  15
                BYTE    COL_00 ; |000000|  14
                BYTE    COL_42 ; |A71A1A|  13
                BYTE    COL_42 ; |A71A1A|  12
                BYTE    COL_42 ; |A71A1A|  11
                BYTE    COL_42 ; |A71A1A|  10
                BYTE    COL_3E ; |FCBC74|   9
                BYTE    COL_3E ; |FCBC74|   8
                BYTE    COL_42 ; |A71A1A|   7
                BYTE    COL_3E ; |FCBC74|   6
                BYTE    COL_3E ; |FCBC74|   5
                BYTE    COL_3E ; |FCBC74|   4
                BYTE    COL_3E ; |FCBC74|   3
                BYTE    COL_3E ; |FCBC74|   2
                BYTE    COL_42 ; |A71A1A|   1
                BYTE    COL_42 ; |A71A1A|   0

B7_MarioWalk2_Hammer0_COL_0:

                BYTE    COL_82 ; |181AA7|  15
                BYTE    COL_82 ; |181AA7|  14
                BYTE    COL_82 ; |181AA7|  13
                BYTE    COL_82 ; |181AA7|  12
                BYTE    COL_82 ; |181AA7|  11
                BYTE    COL_82 ; |181AA7|  10
                BYTE    COL_82 ; |181AA7|   9
                BYTE    COL_82 ; |181AA7|   8
                BYTE    COL_82 ; |181AA7|   7
                BYTE    COL_82 ; |181AA7|   6
                BYTE    COL_82 ; |181AA7|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_42 ; |A71A1A|   2
                BYTE    COL_42 ; |A71A1A|   1
              ; BYTE    COL_00 ; |000000|   0

B7_MarioWalk2_Hammer0_COL_1:

                BYTE    COL_00 ; |000000|  15
                BYTE    COL_00 ; |000000|  14
                BYTE    COL_42 ; |A71A1A|  13
                BYTE    COL_42 ; |A71A1A|  12
                BYTE    COL_42 ; |A71A1A|  11
                BYTE    COL_42 ; |A71A1A|  10
                BYTE    COL_42 ; |A71A1A|   9
                BYTE    COL_00 ; |000000|   8
                BYTE    COL_3E ; |FCBC74|   7
                BYTE    COL_3E ; |FCBC74|   6
                BYTE    COL_3E ; |FCBC74|   5
                BYTE    COL_3E ; |FCBC74|   4
                BYTE    COL_00 ; |000000|   3
                BYTE    COL_3E ; |FCBC74|   2
                BYTE    COL_3E ; |FCBC74|   1
                BYTE    COL_3E ; |FCBC74|   0

B7_MarioWalk2_Hammer1_COL_0:

                BYTE    COL_82 ; |181AA7|  15
                BYTE    COL_82 ; |181AA7|  14
                BYTE    COL_82 ; |181AA7|  13
                BYTE    COL_82 ; |181AA7|  12
                BYTE    COL_82 ; |181AA7|  11
                BYTE    COL_82 ; |181AA7|  10
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

B7_MarioWalk2_Hammer1_COL_1:

                BYTE    COL_00 ; |000000|  15
                BYTE    COL_00 ; |000000|  14
                BYTE    COL_42 ; |A71A1A|  13
                BYTE    COL_42 ; |A71A1A|  12
                BYTE    COL_42 ; |A71A1A|  11
                BYTE    COL_42 ; |A71A1A|  10
                BYTE    COL_3E ; |FCBC74|   9
                BYTE    COL_3E ; |FCBC74|   8
                BYTE    COL_3E ; |FCBC74|   7
                BYTE    COL_3E ; |FCBC74|   6
                BYTE    COL_3E ; |FCBC74|   5
                BYTE    COL_3E ; |FCBC74|   4
                BYTE    COL_3E ; |FCBC74|   3
                BYTE    COL_42 ; |A71A1A|   2
                BYTE    COL_42 ; |A71A1A|   1
                BYTE    COL_00 ; |000000|   0

; --------------------------------------------------------------------
;       GO Skip
; --------------------------------------------------------------------

                ; skip Game Over screen in demo mode

GO_Skip:        lda     DemoMode_R
                bne     IM_EasterEggEnd
                txs                             ; won't use RTS (X = $FF)
                jmp     Bank0_DonkeyKongVCS     ; directly go to title

; ********************************************************************
;       Intermission Easter Egg
; ********************************************************************

IM_EasterEgg:   SUBROUTINE

                ; check if joystick pushed left

                bit     SWCHA
                bvs     IM_EasterEggEnd

                ; write "TRY" instead of "GET"

                tya                             ; Y = <T+1
                ldx     #<R+1
                ldy     #<Y+1
                inc     ScorePtr+$15            ; fix 'Y' hi-byte

IM_EasterEggEnd:rts

; ====================================================================
;       Prepare Stage
; ====================================================================

PrepareStage:   SUBROUTINE

                ; remember level (A = Level_Round)

                lsr
                lsr
                lsr
                lsr
                sta     TempLevel_W

                ; setup Bonus

                tax
                cpx     #5
                bcc     .skip
                ldx     #4
.skip           lda     BonusStartTab-1,x
                sta     Bonus
                lda     BonusTimerTab-1,x
                sta     BonusTimer_W
                sta     EventCtr

                ; set item variables

                lda     Level_Round
                and     #STAGE_MASK
                sta     TempStage_W
                tax
                lda     ItemPosTab,x
                sta     ItemPos_W
                lda     ItemGRP_H,x
                sta     ItemGRPH_W
                lda     ItemCOL_H,x
                sta     ItemCOLH_W
                inx                     ; item index must be > 0
                txa
                asl
                sta     ItemIndex_W
                rts

; ====================================================================
;       Copy Mario
; ====================================================================

                ALIGN_END Bank7Start, $0025

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
                ECHO "---- Bank 7: ", (ROMJumpTable - *) + GapSize, "bytes of ROM left"
#endif
