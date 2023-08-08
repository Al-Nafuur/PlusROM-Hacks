
; ********************************************************************
;  Donkey Kong VCS
;
;    ROM Bank 0
;
;    $Date: Sat, 14 Jan 2017 19:34:58 +0100 $
;    $Author: dietrich $
;    $Revision: 478 $
;
;  Copyright (C) 2017 Andreas Dietrich
; ********************************************************************

; --------------------------------------------------------------------
;       DK Constants
; --------------------------------------------------------------------

DK_EXIT_TIME            = 30            ; exit after 15 seconds
DK_FLASH_START          =  6            ; start title flashing after 3 seconds
DK_FLASH_END            =  9            ; stop title flashing after 4.5 seconds

DK_NINTENDO_COL_0       = COL_2A        ; yellow Nintendo color
DK_NINTENDO_COL_1       = COL_BC        ; cyan Nintendo color

; --------------------------------------------------------------------
;       DK Variables
; --------------------------------------------------------------------

DK_VarStart = $90

DK_VcsColCtr            = $90           ; VCS rainbow color index
DK_VcsCycleCtr          = $91           ; one cycle relates to 0.5 seconds

DK_JapUsMask            = $92           ; hide JAP US display
DK_JapUsSelect          = $93           ; select switch shadow

DK_NintendoCol          = $94           ; flashing Nintendo color

DK_BlockSwing           = $95           ; current oscillation state
DK_BlockSrc             = $96           ; title line source
DK_BlockDst             = $97           ; title line destination
DK_BlockColPtr          = $98 ; [2]     ; flashing title color
DK_BlockBuffer          = $A0 ; [48]    ; title playfield buffer

; --------------------------------------------------------------------
;       SH Constants
; --------------------------------------------------------------------

S1_BARREL_FALL_PROB_0   = $20           ; Mario on same floor
S1_BARREL_FALL_PROB_1   = $B0           ; Mario on different floor
S1_FIREBALL_CLIMB_PROB  = $80           ; fireball follows Mario
S1_FIREBALL_H_MIN       =  12           ; fireball left limit
S1_FIREBALL_H_MAX       = 108           ; fireball right limit
S1_FIREBALL_SPAWN_DELAY =  64           ; wait before initially spawning (>60)

S2_CEMENT_SPAWN_DELAY   =  16           ; wait before spawning
S2_CEMENT_MOVE_MIN      =   8           ; min move duration in one direction (>2)
S2_CEMENT_MOVE_MAX_F1   = 256           ; max move duration floor 1
S2_CEMENT_MOVE_MAX_F3   = 128           ; max move duration floor 3
S2_CEMENT_CLOSE_L       = $30           ; first level with close spacing
S2_FIREBALL_H_MIN       =  40           ; fireball left limit
S2_FIREBALL_H_MAX       =  84           ; fireball right limit
S2_LADDER_TOP_TIME      =  40           ; time telescope ladder is on top
S2_LADDER_DOWN_TIME     =  16           ; time telescope ladder is going down
S2_LADDER_BOTTOM_TIME   =  20           ; time telescope ladder is on bottom
S2_LADDER_UP_TIME       =   8           ; time telescope ladder is going up

S3_JACK_V_LETHAL        =  12           ; jack hits if less high
S3_ELEVATOR_FAST_L      = $20           ; first level with fast elevator

S4_FLAME_SPAWN_DELAY    =  40           ; wait before spawning
S4_FLAME_MOVE_MIN       =   4           ; min move duration
S4_FLAME_MOVE_MAX       =  64           ; max move duration
S4_FLAME_CLIMB_PROB     = $80           ; flame follows Mario


; ********************************************************************
;
;       Code Section
;
; ********************************************************************

; ====================================================================
;       Donkey Kong VCS
; ====================================================================

DonkeyKongVCS:

; --------------------------------------------------------------------
;       DK Init
; --------------------------------------------------------------------

DK_Init:        SUBROUTINE

                ; clear RAM and TIA registers

                CLEAR_RAM_TIA RAMBase, RAMTop

                ; demo mode off (enables sound)

                stx     DemoMode_W                      ; X = $FF

                ; non-zero variables

                inc                     DK_BlockSwing   ; = 1
                STOREBYTE           12, DK_BlockDst
                STOREWORD DK_BlockCol0, DK_BlockColPtr

.wait           lda     INTIM
                bne     .wait

; --------------------------------------------------------------------
;       DK VSync
; --------------------------------------------------------------------

DK_VSync:       VERTICAL_SYNC_SMALL

; --------------------------------------------------------------------
;       DK VBlank
; --------------------------------------------------------------------

DK_VBlank:      ldx     #43
                stx     TIM64T

                ; cycle rainbow colors

DK_CycleVcs:    SUBROUTINE
                ldx     DK_VcsColCtr
                inx
                cpx     #15*2                   ; 15 double-line colors
                bne     .store
                inc     DK_VcsCycleCtr
                tax                             ; A = 0
.store          stx     DK_VcsColCtr

                ; flash DONKEY KONG and NINTENDO text

DK_FlashLetters:SUBROUTINE
                lda     DK_VcsCycleCtr
                cmp     #DK_FLASH_START
                bcc     .end
                cmp     #DK_FLASH_END
                bcs     .end
.blue           ldx     #<DK_BlockCol0
                ldy     #DK_NINTENDO_COL_0
                lda     FrameCtr
                lsr
                lsr
                bcc     .write
.pink           ldx     #<DK_BlockCol1
                ldy     #DK_NINTENDO_COL_1
.write          stx     DK_BlockColPtr
                sty     DK_NintendoCol
.end
                ; drop DONKEY KONG letter blocks

DK_DropLetters: SUBROUTINE

                ; advance vibration

                lda     DK_BlockSwing
                beq     .copy
                dec     DK_BlockSwing
                bpl     .end

                ; copy block graphics into block buffer

.copy           ldy     DK_BlockSrc
                ldx     DK_BlockDst

                cpy     #12
                beq     .end
                lda     DK_TitlePF1_L,y
                sta     DK_BlockBuffer+00-1,x
                lda     DK_TitlePF2_L,y
                sta     DK_BlockBuffer+12-1,x
                lda     DK_TitlePF0_R,y
                sta     DK_BlockBuffer+24-1,x
                lda     DK_TitlePF1_R,y
                sta     DK_BlockBuffer+36-1,x

                ; clear old row position

                cpx     #12
                beq     .next
                lda     #0
                sta     DK_BlockBuffer+00,x
                sta     DK_BlockBuffer+12,x
                sta     DK_BlockBuffer+24,x
                sta     DK_BlockBuffer+36,x

                ; move block down until it hits something

.next           dec     DK_BlockDst
                cpy     DK_BlockDst
                bne     .end

                lda     #12
                sta     DK_BlockDst
                iny
                sty     DK_BlockSrc
                lda     DK_TitlePF1_L-1,y
                beq     .end
                lda     #8
                sta     DK_BlockSwing
                lda     #SOUND_INIT_GIRDER_IMPACT
                jsr     Bank5_DriveAudio
.end
                ; keep playing sounds

DK_SoundPlay:   lda     #SOUND_PLAY
                jsr     Bank5_DriveAudio

; --------------------------------------------------------------------
;       DK Kernel
; --------------------------------------------------------------------

DK_Kernel:      ldx     INTIM
                bne     DK_Kernel

DK_FirstLine:   stx     WSYNC
                stx     VBLANK
                stx     COLUP0
                stx     COLUP1
                stx     VDELP1
                lda     #%00010011
                sta     NUSIZ0
                sta     NUSIZ1

;
; First part -- Donkey Kong title
;

                ; position dots

DK_PosDots:     lda     #82
                jsr     SH_HPosBZNoHMOVE        ; M0
                inx
                lda     #86
                jsr     SH_HPosBZNoHMOVE        ; M1
                sta     WSYNC
                sta     HMOVE

                ; delay start of title graphics

DK_TitleSwingHi:SUBROUTINE
                ldx     DK_BlockSwing
                ldy     DK_BlockSwngTab,x
.loop           sta     WSYNC
                dey
                bpl     .loop

                ; draw title girders

DK_Title:       SUBROUTINE

                ; setup dot graphics

                lda     #%00110011
                sta     GRP0
                asl
                sta     GRP1
                sta     ENAM0
                sta     ENAM1
                sta     CTRLPF

                ; draw block lines 1 and 2

                ldx     #11
.loop0          ldy     #7
                jsr     DK_Line
                jsr     DK_Line
                lda     (DK_BlockColPtr),y
                sta     COLUPF

                ; draw block lines 3 - 6

.loop1          lda     #%00000000
                sta     CTRLPF
                sty     Temp
                lda     DK_BlockBuffer+00,x
                sta     PF1
                lda     DK_BlockBuffer+12,x
                sta     PF2
                lda     DK_BlockBuffer+24,x
                ldy     DK_BlockBuffer+36,x
                sta     RESP0
                sta     RESP1
                sta     RESP0
                sta     RESP1
                sta     PF0
                sty     PF1
                lda     #0
                sta     RESP0
                ldy     Temp
                sta     RESP0
                sta     PF2
                sta     PF0
                cmp     $C5
                dey
                cpy     #1
                bne    .loop1

                ; draw block lines 7 and 8

                lda     #%00000100
                sta     CTRLPF
                jsr     DK_LineNoWSync
                jsr     DK_Line
                dex
                bpl     .loop0

                sta     WSYNC
                sta     PF1
                sta     ENAM0
                sta     ENAM1

                ; insert fill lines if vibrating

DK_TitleSwingLo:SUBROUTINE
                lda     DK_VcsCycleCtr
                cmp     #DK_FLASH_START
                bcs     DK_PosVCS
                ldx     DK_BlockSwing
                lda     #90+1                   ; C = 0
                sbc     DK_BlockSwngTab,x
                tay
.loop           sta     WSYNC
                dey
                bpl     .loop
                jmp     DK_KernelEnd

;
; Second part -- VCS
;

                ; position VCS sprites

DK_PosVCS:      SUBROUTINE
                ldx     #10
                sta     WSYNC
.loop           dex
                bpl     .loop
                sta     RESP0
                sta     RESP1
                lda     #$A0
                sta     HMP0
                lda     #$B0+%0001
                sta     HMP1
                sta     VDELP0
                sta     VDELP1
                sta     WSYNC
                sta     HMOVE

                ; draw VCS sprites

DK_VCS:         SUBROUTINE
                ldy     #14
.loop           tya
                clc
                adc     DK_VcsColCtr
                lsr
                tax
                lda     DK_VcsCol,x
                sta     COLUP0
                sta     COLUP1
                lda     DK_VcsGRP0,y
                ldx     DK_VcsGRP1,y
                sta     GRP0
                stx     GRP1
                lda     DK_VcsGRP2,y
                ldx     DK_VcsGRP3,y
                sta     GRP0
                nop
                nop
                nop
                nop
                lda     #0
                stx     GRP1
                sta     GRP0
                sta     GRP1
                sta     GRP0
                cmp     $C5
                dey
                bpl     .loop

                ; position JAP US sprites

DK_PosJapUs:    SUBROUTINE
                ldy     #4
.loop           dey
                bne     .loop
                lda     SWCHB
                and     #SWCHB_JAPUS_MASK
                asl
                ora     #7
                tax
                sty     NUSIZ0
                sty     NUSIZ1
                sta     RESP0
                sta     RESP1
                lda     #$30
                sta     HMP0
                lda     #$40
                sta     HMP1

                ; draw JAP US sprites

DK_JapUs:       SUBROUTINE
                ldy     #7
.loop           sta     WSYNC
                sta     HMOVE
                lda     DK_JapUsGRP0,x
                and     DK_JapUsMask
                sta     GRP0
                lda     DK_JapUsGRP1,x
                and     DK_JapUsMask
                sta     GRP1
                sta     GRP0
                dex
                dey
                sta     HMCLR
                bpl     .loop

;
; Third part -- Kong
;

DK_Kong:        STOREBYTE 31, LineCtr
                ldy     #EN_KONG_DRUM0
                jsr     Bank6_DrawKong

                stx     COLUP0
                stx     COLUP1
                stx     REFP0
                stx     REFP1

                WAIT_LINES 9

;
; Fourth part -- Nintendo
;

DK_Nintendo:    SUBROUTINE
                STOREBYTE 15, LineCtr
.loop           ldy     LineCtr
                lda     DK_NintendoGRP0,y
                sta     GRP1
                sta     WSYNC
                lda     DK_NintendoCol
                sta     COLUP0
                sta     COLUP1
                lda     DK_NintendoGRP1,y
                sta     GRP0
                lda     DK_NintendoGRP2,y
                sta     GRP1
                lda     DK_NintendoGRP3,y
                sta     Temp
                lda     DK_NintendoGRP4,y
                ldx     DK_NintendoGRP5,y
                ldy     Temp
                sty     GRP0
                sta     GRP1
                stx     GRP0
                stx     GRP1
                dec     LineCtr
                bpl     .loop

DK_KernelEnd:

; --------------------------------------------------------------------
;       DK Overscan
; --------------------------------------------------------------------

DK_Overscan:    lda     #%00000010
                sta     WSYNC                   ; VBLANK on
                sta     VBLANK

                lda     #36                     ; set timer to skip OVERSCAN
                sta     TIM64T

                inc     FrameCtr

DK_CheckReset:  SUBROUTINE

                ; check for reset switch pressed

.reset          lda     SWCHB
                lsr
                bcs     .exit
                jmp     DK_Init

                ; check for exit time reached

.exit           ldx     DK_VcsCycleCtr
                cpx     #DK_EXIT_TIME
                bcs     DK_Exit

                ; check for select switch pressed

DK_CheckSelect: SUBROUTINE
                and     #%00000001
                tax
                bne     .write
                lda     DK_JapUsSelect
                beq     .write
                lda     SWCHB
                eor     #SWCHB_JAPUS_MASK
                sta     SWCHB
                lda     #%11111111
                sta     DK_JapUsMask
.write          stx     DK_JapUsSelect

                ; check for either trigger pressed

DK_CheckStart:  lda     INPT4
                and     INPT5
                bmi     DK_OverscanEnd
DK_Exit:        jmp     Bank1_Intro

DK_OverscanEnd: lda     INTIM
                bne     DK_OverscanEnd
                jmp     DK_VSync

; --------------------------------------------------------------------
;       DK Subroutines
; --------------------------------------------------------------------

                ; draw a single line of title text

DK_Line:        sta     WSYNC
DK_LineNoWSync: lda     DK_BlockBuffer+00,x
                sta     PF1
                lda     DK_BlockBuffer+12,x
                sta     PF2
                lda     (DK_BlockColPtr),y
                sta     COLUPF
                dey
                nop
                lda     DK_BlockBuffer+24,x
                sta     PF0
                lda     DK_BlockBuffer+36,x
                sta     PF1
                dec     Temp                    ; sleep 5
                lda     #0
                sta     PF2
                sta     PF0
                rts

; ====================================================================
;       Stage Handler
; ====================================================================

; forward declarations

ST_AudioCode    = ScorePtr + 0
ST_GapBorder    = ScorePtr + 4
ST_GapMask      = ScorePtr + 8
ST_M0Mask       = $F0
ST_FrameSwitch  = $F1

StageHandler:   SUBROUTINE

;
; Hammer -------------------------------------------------------------
;

SH_Hammer:      SUBROUTINE

                ; hammer available ?

                STOREBYTE 0, ST_M0Mask

                ldy     TempStage_R
                beq     .floor
                cpy     #3
                bne     SH_HammerEnd
.floor          lda     MarioFloor_R
                cmp     #2*4
                bcs     SH_HammerEnd

                ; hammer flickering

                lda     ST_FrameSwitch
                asl
                asl
                sta     ST_M0Mask

                ; switch state

                ldx     HammerState_R
                beq     SH_HammerAvail
                cpx     #HAMMER_STATE_OFF
                bcc     SH_HammerSnag
                bne     SH_HammerOn
SH_HammerEnd:   jmp     SH_HammerOff

                ;
                ; AVAIL state ----------------------------------------
                ;

SH_HammerAvail: SUBROUTINE

                ; Mario snags hammer ?

                lda     MarioFloor_R
                cmp     #1*4                    ; Mario on floor 1 ?
                bne     .pos                    ; no -> display hammer
                lda     MarioState_R
                cmp     #MARIO_STATE_JUMP       ; Mario jumping ?
                bne     .pos                    ; no -> display hammer
                sec
                lda     MarioHPos_R
                sbc     SH_HammerPosTab,y
                adc     #3-1
                cmp     #6                      ; horizontal overlap ?
                bcc     SH_HammerSnagSet        ; yes -> snag hammer

                ; position hammer

.pos            lda     ST_M0Mask
                ora     #%00000010
                sta     ST_M0Mask               ; hammer visible
                lda     ST_FrameSwitch
                ora     #%00000100
                tax                             ; relative position index
                lda     SH_HammerPosTab,y       ; absolute position
                jmp     SH_HammerPos

                ;
                ; SNAG state -----------------------------------------
                ;

SH_HammerSnagSet: SUBROUTINE

                inx
                stx     HammerState_W
                lda     #SOUND_INIT_MARIO_JINGLE
                sta     ST_AudioCode

SH_HammerSnag:  SUBROUTINE

                lda     MarioState_R
                cmp     #MARIO_STATE_WALK
                bne     SH_HammerEnd

                ;
                ; ON state -------------------------------------------
                ;

SH_HammerOnSet: SUBROUTINE

                ldx     #HAMMER_STATE_ON
                lda     #MUSIC_INIT_HAMMER
                sta     ST_AudioCode

SH_HammerOn:    SUBROUTINE

                ; animate hammer

                lda     FXType+1
                cmp     #(SOUND_INIT_MARIO_HIT & AUDIO_ARG_MASK) ; hammer hitting enemy ?
                beq     .write                                   ; yes -> don't move hammer
                ldy     MarioState_R
                cpy     #MARIO_STATE_JUMP                        ; Mario falling ?
                beq     .off                                     ; yes -> hammer off
                cpy     #MARIO_STATE_DEAD                        ; Mario dead ?
                bne     .animate                                 ; no -> keep animating
                lda     MarioTimer_R
                cmp     #4*30                                    ; Mario rotating ?
                bcs     .write                                   ; no -> don't move hammer
.off            ldx     #HAMMER_STATE_OFF                        ; yes -> hammer off
                bne     .write

.animate        lda     ST_FrameSwitch
                beq     .music
                dex

                ; restore stage music

.music          cpx     #HAMMER_STATE_OFF
                bne     .hit
                lda     #MUSIC_INIT_TIME_OUT
                ldy     Bonus
                cpy     #$10
                bcc     .skip
                lda     TempStage_R
                ora     #(AUDIO_MUSIC_INIT | AUDIO_CHANNEL_0)
.skip           sta     ST_AudioCode
                bne     .write

                ; check if enemy hit

.hit            bit     CXM0P
                bvc     .write

                ldy     #4
.loop           lda     EnemyIndex_R-1,y        ; enemy slot used ?
                beq     .next
                lda     MarioFloor_R            ; enemy and Mario on same floor ?
                cmp     EnemyFloor_R-1,y
                bne     .next
                ;
                lda     #SOUND_INIT_MARIO_HIT
                sta     ST_AudioCode
                lda     #0
                sta     EnemyIndex_W-1,y        ; remove enemy
                sta     EnemyAnimTimer_W-1,y    ; delay respawn in stage 1
                ;
.next           dey
                bne     .loop

                ; compute missile mask

.write          stx     HammerState_W
                txa
                asl
                and     #%00001000              ; hammer up/down
                ora     #%00000010              ; hammer on
                ora     ST_M0Mask
                sta     ST_M0Mask

                ; position missile

.position       lda     MarioDir_R
                and     #%00001000
                asl
                ora     ST_M0Mask
                lsr
                lsr
                tax
                lda     MarioHPos_R
SH_HammerPos:   clc
                adc     SH_HammerRelTab,x

                ldx     #0                      ; M0
                jsr     SH_HPosBZNoHMOVE
                sta     WSYNC
                sta     HMOVE

                ;
                ; OFF state ------------------------------------------
                ;

SH_HammerOff:   SUBROUTINE

; --------------------------------------------------------------------

SH_StageCode:   SUBROUTINE

                ; execute stage code ?

                lda     TempStage_R
                cmp     #2                                       ; elevator stage ?
                beq     .random                                  ; yes -> execute stage code (flicker)
                ;
                lda     MarioState_R
                cmp     #MARIO_STATE_DEAD                        ; Mario dead ?
                beq     SH_End                                   ; yes -> don't move enemies
                ;
                lda     FXType+1
                cmp     #(SOUND_INIT_MARIO_HIT & AUDIO_ARG_MASK) ; hammer hitting enemy ?
                bne     .random                                  ; no -> move enemies
                lda     FXTimer+1                                ; jingle about to start ?
                bne     SH_End                                   ; no -> don't move enemies
                lda     #ST_SCORE_ENEMY_KILL                     ; yes -> increase score
                jmp     SH_AddToScore

                ; randomize with input

.random         lda     Random
                eor     SWCHA
                sta     Random

                ; activate all gaps

                lda     #0
                sta     ST_GapMask+0
                sta     ST_GapMask+1
                sta     ST_GapMask+2
                sta     ST_GapMask+3

                ; check extra score (picking up items and jumping over enemies)

                jsr     SH_ExtraScore

                ; stage code dispatcher

SH_Dispatch:    SUBROUTINE
                ldx     TempStage_R
.switch         lda     SH_StageHdlr_H,x
                pha
                lda     SH_StageHdlr_L,x
                pha
SH_End:         rts

; --------------------------------------------------------------------
;       Stage 1
; --------------------------------------------------------------------

Stage1:         SUBROUTINE

                ; check stage goal

                jsr     SH_Stage123Goal

                ; speed progression

                lda     ST_FrameSwitch          ; min. every 2nd frame
                beq     S1_Enemies
                lda     StageTimer_R            ; max. every frame
                beq     SH_End

                ; handle enemies

S1_Enemies:     SUBROUTINE
                ldx     #3
.loop           lda     EnemyIndex_R,x
                beq     .next
                jsr     SH_State
                jsr     SH_Animate
.next           dex
                bpl     .loop

; --------------------------------------------------------------------

S1_FireballSpawn:SUBROUTINE

                ; animate fireball

                ldy     EnemyAnimTimer_R+3
                dey
                sty     EnemyAnimTimer_W+3

                ; spawn new fireball

                bne     .end                    ; delay respawn
                lda     EnemyIndex_R+3          ; check slot
                bne     .end
                lda     MarioState_R            ; wait for Mario
                beq     .end
                ldx     #3
                ldy     #ENEMY_INIT_FIREBALL_0
                jsr     SH_EnemyInit
.end

; --------------------------------------------------------------------

S1_BarrelSpawn: SUBROUTINE

                ; check if there is room for a new barrel

                ldx     #2
.loop0          lda     EnemyFloor_R,x
                ldy     EnemyState_R,x
                cmp     #5*4                            ; floor 5 (top) free ?
                bcs     .hide                           ; no -> don't spawn
                cmp     #4*4                            ; floor 4 free ?
                bcc     .next0                          ; yes -> check next slot
                cpy     #ENEMY_STATE_BARREL_ROLL        ; barrel on floor 4 falling from 5 ?
                bne     .end
                lda     EnemyHPos_R,x                   ; barrel far enough away ?
                cmp     #86
                bcs     .end
.next0          dex
                bpl     .loop0

                ; find a free slot

                ldx     #2
.loop1          ldy     EnemyIndex_R,x
                beq     .spawn
                dex
                bpl     .loop1
                rts

                ; set Kong animation and insert initial barrel data

.spawn          sty     KongAnimState_W                 ; Y = 0
                sty     KongAnimTimer_W
                jmp     SH_EnemyInit

                ; hide a newly spawned barrel for a few frames

.hide           cpy     #ENEMY_STATE_BARREL_EDGE
                bne     .end
                lda     #ENEMY_BARREL_ROLL_1
                ldy     EnemyVPos_R,x
                cpy     #10+1
                bcc     .write
                lda     #ENEMY_EMPTY_GRP
.write          sta     EnemyIndex_W,x
.end            rts

; --------------------------------------------------------------------

                ;
                ; ROLL state -----------------------------------------
                ;

S1_BarrelRollSet:SUBROUTINE

                STOREBYTE_X ENEMY_STATE_BARREL_ROLL, EnemyState_W

                lda     EnemyDir_R,x
                eor     #%11110000
                sta     EnemyDir_W,x

S1_BarrelRoll:  SUBROUTINE

                ; check if barrel finished

                lda     EnemyHPos_R,x
                ldy     EnemyFloor_R,x
                cpy     #2*4                    ; never go below floor 2
                bne     .ladder
                cmp     #16                     ; stop on left edge
                bcs     .skip

                ; make barrel disappear

                cmp     #13
                lda     #2                      ; freeze animation timer
                sta     EnemyAnimTimer_W,x
                lda     #ENEMY_VANISH           ; directly set vanish graphics
                bcs     .write                  ; barrel X-position < 13 ?
                lda     #ENEMY_EMPTY_SLOT       ; yes -> clear slot
.write          sta     EnemyIndex_W,x
.skip           jmp     .minus

                ; barrel takes shortcut ?

.ladder         cmp     S1_LadderTab-(3*4)+0,y  ; barrel over ladder ?
                beq     .random
                cmp     S1_LadderTab-(3*4)+1,y
                beq     .random
                cmp     S1_LadderTab-(3*4)+2,y
                bne     .roll
.random         GET_RANDOM                      ; randomly take ladder
                cpy     MarioFloor_R
                bne     .p1
.p0             cmp     #S1_BARREL_FALL_PROB_0  ; Mario on same floor
                bcs     .roll
.p1             cmp     #S1_BARREL_FALL_PROB_1  ; Mario on different floor
                bcs     .roll

                ; avoid falling barrels in JAP setting

                lda     SWCHB
                and     #SWCHB_JAPUS_MASK       ; JAP only
                beq     .fall
                lda     MarioState_R
                cmp     #MARIO_STATE_CLIMB      ; Mario climbing ?
                bne     .fall
                lda     MarioVPos_R
                and     #%00011111
                cmp     #16                     ; Mario's hand aligns with grider
                beq     .roll

                ; check if there is room below

.fall           ldy     #2
.loop           lda     EnemyIndex_R,y          ; slot empty ?
                beq     .next
                ;
                lda     EnemyFloor_R,x
                sec
                sbc     #4
                cmp     EnemyFloor_R,y          ; floor -1 free ?
                beq     .roll                   ; no -> keep rolling
                sbc     #4
                cmp     EnemyFloor_R,y          ; floor -2 free ?
                bne     .next                   ; yes -> check next slot
                lda     EnemyState_R,y
                cmp     #ENEMY_STATE_BARREL_ROLL; barrel on floor -2 is falling ?
                bne     .roll                   ; yes -> keep rolling
                ;
                sec                             ; barrel on floor -2 far enough away ?
                lda     #128                    ; hpos_Y < (w - hpos_X) or hpos_Y > (w - hpos_X)
                sbc     EnemyHPos_R,x
                cmp     EnemyHPos_R,y
                ror
                eor     EnemyDir_R,x
                bmi     .roll
.next           dey
                bpl     .loop
                bmi     S1_BarrelLadderSet

                ; check if barrel reaches borders

.roll           ldy     EnemyHPos_R,x
                lda     EnemyDir_R,x
                bmi     .left
.right          cpy     #116-2-4
                bcs     S1_BarrelEdgeSet
.plus           lda     #1
                bne     .move
.left           cpy     #13+2+4
                bcc     S1_BarrelEdgeSet
.minus          lda     #-1

                ; update position and animation

.move           tay
                clc
                adc     EnemyHPos_R,x
                sta     EnemyHPos_W,x
                tya
                clc
                adc     EnemyAnimTimer_R,x
                sta     EnemyAnimTimer_W,x
                rts

                ;
                ; LADDER state ---------------------------------------
                ;

S1_BarrelLadderSet:SUBROUTINE

                STOREBYTE_X ENEMY_STATE_BARREL_LADDER, EnemyState_W
                STOREBYTE_X                        16, EnemyTimer_W,x
                STOREBYTE_X                         0, EnemyAnimTimer_W,x
                STOREBYTE_X                12+(1*2)+0, EnemyAnimState_W,x
                rts

S1_BarrelLadder:SUBROUTINE

                ; next step

                ldy     EnemyTimer_R,x
                dey
                bpl     .skip
                lda     #0+(1*2)+2
                sta     EnemyAnimState_W,x
                lda     #1
                sta     EnemyAnimTimer_W,x
                jmp     S1_BarrelRollSet
.skip           tya
                sta     EnemyTimer_W,x

                ; update vertical position

                ldy     EnemyAnimTimer_R,x
                dey
                tya
                sta     EnemyAnimTimer_W,x
                lda     #-2
                bne     SH_EnemyMoveV

                ;
                ; EDGE state -----------------------------------------
                ;

S1_BarrelEdgeSet:SUBROUTINE

                STOREBYTE_X ENEMY_STATE_BARREL_EDGE, EnemyState_W
                STOREBYTE_X                   16+12, EnemyTimer_W,x

S1_BarrelEdge:  SUBROUTINE

                ; next step

                ldy     EnemyTimer_R,x
                dey
                bpl     .skip
                jmp     S1_BarrelRollSet
.skip           tya
                sta     EnemyTimer_W,x

                ; update horizontal position

                lda     EnemyDir_R,x
                rol
                lda     S1_BarrelFallX,y
                bcc     .move
                eor     #$FF
                adc     #0
.move           sta     Temp
                clc
                adc     EnemyHPos_R,x
                sta     EnemyHPos_W,x
                lda     Temp
                clc
                adc     EnemyAnimTimer_R,x
                sta     EnemyAnimTimer_W,x

                ; update vertical position

                lda     S1_BarrelFallY,y

; --------------------------------------------------------------------

SH_EnemyMoveV:  SUBROUTINE
                clc
                adc     EnemyVPos_R,x
                tay
                and     #%00011111
                sta     EnemyVPos_W,x
                tya
                and     #%11100000
                cmp     #$80
                ror
                cmp     #$80
                ror
                cmp     #$80
                ror
                adc     EnemyFloor_R,x
                sta     EnemyFloor_W,x
                rts

; --------------------------------------------------------------------

                ;
                ; JUMP state -----------------------------------------
                ;

S1_FireballJump:SUBROUTINE

                ; freeze animation

                lda     #3
                sta     EnemyAnimTimer_W,x

                ; move fireball

                lda     EnemyTimer_R,x
                lsr
                bcc     .vertical
.horizontal     lda     #0
                adc     EnemyHPos_R,x
                sta     EnemyHPos_W,x
.vertical       lda     #-1
                adc     EnemyTimer_R,x
                sta     EnemyTimer_W,x
                tay
                lda     S1_FireballJmpY,y
                bcs     SH_EnemyMoveV

                ;
                ; MOVE state -----------------------------------------
                ;

S1_FireballMoveSet:SUBROUTINE

                STOREBYTE_X ENEMY_STATE_FIREBALL_MOVE, EnemyState_W,x
                STOREBYTE_X                        64, EnemyTemp_W,x

S1_FireballMove:SUBROUTINE

                ; vertical jumping

                ldy     EnemyTimer_R,x
                iny
                tya
                and     #16-1
                sta     EnemyTimer_W,x
                tay
                lsr
                lda     S1_FireballMovY,y
                sta     EnemyVPos_W,x
                bcc     .end

                ; follow Mario on ladder ?

                lda     MarioState_R            ; Mario climbing ?
                cmp     #MARIO_STATE_CLIMB
                bne     .horizontal
                lda     MarioFloor_R            ; Mario on same floor ?
                cmp     EnemyFloor_R,x
                bne     .horizontal
                lda     MarioHPos_R             ; Mario above Fireball ?
                sbc     #12
                cmp     EnemyHPos_R,x
                bne     .horizontal

                lda     Random                  ; follow Mario ?
                cmp     #S1_FIREBALL_CLIMB_PROB
                bcc     S1_FireballClimbSet

                ; horizontal movement

.horizontal     ldy     EnemyHPos_R,x
                tya
                cmp     EnemyTemp_R,x
                bne     .left

                ; get new destination

                lda     EnemyFloor_R,x
                lsr
                lsr
                tay

                GET_RANDOM
                sec
.modulo         sbc     SH_FireblRange-1,y
                bcs     .modulo
                adc     SH_FireblStart-1,y
                sta     EnemyTemp_W,x           ; A in [H_MIN,H_MAX]
                bne     .horizontal

                ; increase/decrease horizontal position

.left           dey
                lda     #%00000000
                bcs     .write
                iny
.right          iny
                lda     #%11111000
.write          sta     EnemyDir_W,x
                tya
                sta     EnemyHPos_W,x
.end            rts

                ;
                ; CLIMB state ----------------------------------------
                ;

S1_FireballClimbSet:SUBROUTINE

                STOREBYTE_X ENEMY_STATE_FIREBALL_CLIMB, EnemyState_W,x
                STOREBYTE_X                          1, EnemyTemp_W,x

S1_FireballClimb:SUBROUTINE

                ; climb up then down, then leave

                lda     EnemyVPos_R,x
                bne     .climb
                jmp     S1_FireballMoveSet

.climb          ldy     EnemyAnimTimer_R,x
                cpy     #1
                bne     S1_End
                clc
                adc     EnemyTemp_R,x
                sta     EnemyVPos_W,x
                cmp     #7
                bne     S1_End
                lda     #-1
                sta     EnemyTemp_W,x
S1_End:         rts

; --------------------------------------------------------------------
;       Stage 2
; --------------------------------------------------------------------

EnemyCementTimer_R = EnemyTimer_R
EnemyCementTimer_W = EnemyTimer_W
EnemyCementType_R  = EnemyAnimTimer_R
EnemyCementType_W  = EnemyAnimTimer_W
EnemyCementSpeed_R = EnemyAnimState_R
EnemyCementSpeed_W = EnemyAnimState_W
EnemyCementPos_R   = EnemyTemp_R
EnemyCementPos_W   = EnemyTemp_W

Stage2:         SUBROUTINE

                ; check stage goal

                jsr     SH_Stage123Goal

                ; handle enemies

S2_Enemies:     lda     MarioState_R            ; wait until Mario appears
                beq     S1_End

; --------------------------------------------------------------------

S2_Fireball:    SUBROUTINE

                ; speed progression

                lda     StageTimer_R
                beq     S2_Cement

                ; animate fireball

                ldy     EnemyAnimTimer_R+0
                dey
                sty     EnemyAnimTimer_W+0

                ldx     #0
                lda     EnemyIndex_R+0
                beq     .spawn
                jsr     SH_State
                jsr     SH_Animate
                jmp     S2_Cement

                ; spawn new fireball

.spawn          ldy     #ENEMY_INIT_FIREBALL_1
                jsr     SH_EnemyInit

; --------------------------------------------------------------------

S2_Cement:      SUBROUTINE

                ; speed progression

                lda     ST_FrameSwitch          ; max. every 2nd frame
                beq     S1_End
                lda     StageTimer_R            ; min. every 4th frame
                beq     S1_End

                ; move cement

.animate        ldx     #2
.loop           jsr     S2_ConveyorMove
                lda     EnemyIndex_R,x
                bne     .skip
                jsr     S2_CementSpawn
.skip           jsr     SH_State
                dex
                bne     .loop

; --------------------------------------------------------------------

S2_Mario:       SUBROUTINE

                ; find relvant conveyor

                lda     MarioState_R
                cmp     #MARIO_STATE_WALK
                bne     S1_End
                lda     #0
                ldy     MarioFloor_R
.floor1         ldx     #1
                cpy     #1*4
                beq     .move
.floor3         inx
                cpy     #3*4
                bne     S1_End

                ; invert direction if Mario and Cement are on different sides

                ldy     MarioHPos_R
                cpy     #(ST_MARIO_H_MIN+ST_MARIO_H_MAX)/2
                ror
                ldy     EnemyCementType_R,x
                cpy     #5
                bcs     .move
                eor     #%10000000

                ; drag Mario

.move           eor     EnemyCementSpeed_R,x
                bpl     .right
.left           jmp     SH_MarioLeft
.right          jmp     SH_MarioRight

; --------------------------------------------------------------------

S2_ConveyorMove:SUBROUTINE

                ; change conveyor direction

                ldy     EnemyCementTimer_R,x
                bne     .next
                GET_RANDOM                      ; get new duration
                and     S2_CementMovMax-1,x     ; maximum per floor
                ora     #S2_CEMENT_MOVE_MIN     ; minimum
                tay
                sec
                and     #2                      ; bit 1 : direction
                sbc     #1                      ; A in [-1,+1]
                sta     EnemyCementSpeed_W,x
.next           dey
                tya
                sta     EnemyCementTimer_W,x
                rts

; --------------------------------------------------------------------

S2_CementSpawn: SUBROUTINE

                ; set basic enemy attributes

                STOREBYTE_X ENEMY_STATE_CEMENT_MOVE, EnemyState_W
                STOREBYTE_X ENEMY_CEMENT,            EnemyIndex_W

                lda     S2_CementFloor-1,x
                sta     EnemyFloor_W,x

                ; randomly determine Cement type

                GET_RANDOM

                cpx     #1
                bne     .floor3

                ; set Cement type on floor 1

.floor1         and     #4-1                    ; type 0, 1, 2, 3
                bne     .setup
                ldy     Level_Round             ; avoid type 0 in L = {1,2}
                cpy     #S2_CEMENT_CLOSE_L
                bcs     .setup
                txa                             ; X = 1
                bne     .setup

                ; set Cement type and left or right conveyor on floor 3

.floor3         and     #2-1                    ; A := 0 or 1
                tay
                asl                             ; A := A * 2 (C := 0)
                sbc     #0                      ; A := A - 1
                sta     EnemyCementSpeed_W,x
                tya
                clc
                adc     #4                      ; type 4 or 5

                ; set horizontal start position

.setup          sta     EnemyCementType_W,x
                tay
                lda     EnemyCementSpeed_R,x
                bpl     .right
.left           lda     S2_CementMax,y
                bne     .write
.right          lda     S2_CementMin,y
.write          sta     EnemyCementPos_W,x

                ; ensure minimum movement duration

                lda     EnemyCementTimer_R,x
                ora     #S2_CEMENT_MOVE_MIN
                sta     EnemyCementTimer_W,x
                rts

; --------------------------------------------------------------------

                ;
                ; MOVE state -----------------------------------------
                ;

S2_CementMove:  SUBROUTINE

                ; update cement position

                ldy     EnemyCementType_R,x
                lda     EnemyCementSpeed_R,x
                clc
                adc     EnemyCementPos_R,x
                sta     EnemyCementPos_W,x

                ; end position reached ?

                bcc     .right
.left           cmp     S2_CementMin,y
                jmp     .burn
.right          cmp     S2_CementMax,y
.burn           beq     S2_CementBurnSet

                ; calculate actual HPOS and NUSIZ values

.move           lda     S2_CementNum,y          ; get interval list
                tay
                lda     EnemyCementPos_R,x      ; find current interval
.loop           dey
                cmp     S2_NumLimitTab,y
                bcc     .loop
                adc     S2_NumOffsetTab,y       ; shift HPos
                sta     EnemyHPos_W,x
                lda     S2_NumTab,y             ; adjust NUSIZ
                sta     EnemyDir_W,x
.end            rts

                ;
                ; BURN state -----------------------------------------
                ;

S2_CementBurnSet:SUBROUTINE

                STOREBYTE_X ENEMY_STATE_CEMENT_BURN, EnemyState_W

                ; determine delay time

                lda     #S2_CEMENT_SPAWN_DELAY
                ldy     EnemyHPos_R,x
                cpy     #64                     ; Cement stopped over barrel ?
                beq     .write                  ; yes -> use delay
                lsr                             ; no -> halve delay
.write          sta     EnemyTemp_W,x

S2_CementBurn:  SUBROUTINE

                ; count down delay time

                ldy     EnemyTemp_R,x
                dey
                tya
                sta     EnemyTemp_W,x           ; timer down to 0 ?
                beq     .write                  ; yes -> clear enemy slot

                ; set graphics index

                lda     #ENEMY_EMPTY_GRP
                ldy     EnemyHPos_R,x
                cpy     #64                     ; Cement stopped over barrel ?
                bne     .write                  ; no -> make invisible
                GET_RANDOM                      ; yes -> show random Blaze
                lda     #ENEMY_BLAZE_0
                bcc     .write
                lda     #ENEMY_BLAZE_1
.write          sta     EnemyIndex_W,x
S2_End:         rts

; --------------------------------------------------------------------
;       Stage 3
; --------------------------------------------------------------------

Stage3:         SUBROUTINE

                ; handle enemies

S3_Enemies:     lda     MarioState_R            ; wait until Mario appears
                beq     S2_End
.dead           cmp     #MARIO_STATE_DEAD
                beq     S3_Display

                ; check stage goal

S3_Goal:        jsr     SH_Stage123Goal

; --------------------------------------------------------------------

S3_Elevator:    SUBROUTINE

                ; set initial speed and direction

                ldx     #2
                lda     EnemyTemp_R,x
                bne     .skip
                lda     #-1
                sta     EnemyTemp_W,x
.skip
                ; advance timer

                lda     FrameCtr
                and     StageSpeedMask_R
                bne     .end

                ldy     EnemyTimer_R,x
                dey
                tya
                sta     EnemyTimer_W,x
                bpl     .move

                ; reset position and direction

                STOREBYTE_X ENEMY_ELEVATOR, EnemyAnimState_W
                STOREBYTE_X      (3*32)+14, EnemyTimer_W

                ldy     #28                     ; left elevator
                sec
                lda     #0
                sbc     EnemyTemp_R,x           ; invert direction
                sta     EnemyTemp_W,x
                bpl     .write
                ldy     #28+32                  ; right elevator
.write          tya
                sta     EnemyHPos_W,x
                bne     .end                    ; don't move when switching

                ; move elevator

.move           lda     EnemyTemp_R,x
                jsr     SH_EnemyMoveV
.end

; --------------------------------------------------------------------

S3_Jack:        SUBROUTINE

                ; spawn new jack

                dex                             ; X := 1
.loop           lda     EnemyAnimState_R,x      ; slot free ?
                bne     .move                   ; no -> keep moving
                txa
                eor     #%00000001              ; check other slot
                tay
                lda     EnemyFloor_R,y
                cmp     #2*4                    ; floor 3 and 4 free ?
                bcs     .next                   ; no -> check next slot

                stx     LineCtr
                ldy     #ENEMY_INIT_JACK
                jsr     SH_EnemyInit            ; insert new jack
                ldx     LineCtr

                ; move jacks

.move           jsr     SH_State
.next           dex
                bpl     .loop

; --------------------------------------------------------------------

S3_Display:     SUBROUTINE

                ; switch display between jacks and elevators

                lda     ST_FrameSwitch
                asl
                tay                             ; [0,1]: jacks [2,3]: elevators

                ; clear all display slots and relax collision

                ldx     #2
.loop           lda     #ENEMY_EMPTY_SLOT
                sta     EnemyIndex_W,x

                tya                             ; elevators were shown this frame ?
                beq     .write                  ; yes -> ignore collision
                lda     MarioVPos_R
                and     #%00011111              ; Mario on ground ?
                bne     .next
                lda     EnemyFloor_R,x
                cmp     #4*4                    ; jack on top floor ?
                bne     .next
                lda     EnemyVPos_R,x
                cmp     #S3_JACK_V_LETHAL       ; jack high enough ?
                bcc     .next
.write          sta     CXCLR                   ; ignore collision

.next           dex
                bpl     .loop

                ; display active slots

                lda     EnemyAnimState_R+0,y
                sta     EnemyIndex_W+0,y
                lda     EnemyAnimState_R+1,y
                sta     EnemyIndex_W+1,y
                rts

; --------------------------------------------------------------------

                ;
                ; JUMP state -----------------------------------------
                ;

S3_JackJump:    SUBROUTINE

                ldy     #2
                lda     StageTimer_R
                lsr                             ; slow speed ramping
                bne     .write
                dey
.write          sty     Temp

                ; move horizontally

                clc
                lda     EnemyHPos_R,x
                adc     Temp
                sta     EnemyHPos_W,x
                cmp     #96                     ; over gap ?
                bcc     .jump                   ; no -> keep jumping
                lda     EnemyVPos_R,x           ; jump finished ?
                beq     S3_JackFallSet          ; yes -> start fall

                ; move vertically

.jump           sec
                lda     EnemyTimer_R,x
                sbc     Temp
                bpl     .write0
                lda     #24                     ; repeat cycle
.write0         tay
                sta     EnemyTimer_W,x
                lda     S3_JackJumpY,y
                sta     EnemyVPos_W,x

                ; animate graphics

                cmp     #7
                lda     #ENEMY_JACK_0
                bcs     .write1
                lda     #ENEMY_JACK_2
.write1         sta     EnemyAnimState_W,x

                ; trigger bounce sound

                cpy     #24
                bne     .end
                ldy     #1

S3_JackSound:   lda     Bonus
                cmp     #$10
                bcc     .end                    ; don't interrupt time out music

                lda     #1
                sta     NPTimer+0
                lsr                             ; A := 0
                sta     NPNote+0
                lda     S3_TrackTabL,y
                sta     NPTrackPtrL+0
                lda     #>(B5_Stage3_JackFall_Track0)
                sta     NPTrackPtrH+0
.end            rts

#if (>B5_Stage3_JackFall_Track0) != (>B5_Stage3_JackJump_Track0)
                ECHO    "ERROR: Can't optimize S3_JackSound"
#endif

                ;
                ; FALL state -----------------------------------------
                ;

S3_JackFallSet: SUBROUTINE

                STOREBYTE_X ENEMY_STATE_JACK_FALL, EnemyState_W
                STOREBYTE_X                     0, EnemyTimer_W

                tay                             ; A = 0
                beq     S3_JackSound

S3_JackFall:    SUBROUTINE

                ; animate graphics

                ldy     EnemyTimer_R,x
                dey
                tya
                sta     EnemyTimer_W,x
                and     #16
                bne     .skip
                lda     #ENEMY_JACK_1
                BYTE    $2C                     ; skip two bytes
.skip           lda     #ENEMY_JACK_2
                sta     EnemyAnimState_W,x

                ; check if jack finished

                lda     EnemyFloor_R,x
                bpl     .move
                lda     #ENEMY_EMPTY_SLOT
                sta     EnemyAnimState_W,x      ; index backup
                rts

                ; move vertically

.move           lda     #-3
                jmp     SH_EnemyMoveV

; --------------------------------------------------------------------
;       Stage 4
; --------------------------------------------------------------------

Stage4:         SUBROUTINE

                ; rivets

S4_Rivets:      SUBROUTINE

                ; check for Mario rivet collision

                lda     MarioFloor_R
                beq     .end

                ldx     MarioHPos_R
                cpx     #ST_MARIO_H_MIN-4+( 7*4)+1
                beq     .rivet_hit
                cpx     #ST_MARIO_H_MIN-4+( 7*4)+2
                beq     .rivet_hit
                cpx     #ST_MARIO_H_MIN-4+(20*4)+1
                beq     .rivet_hit
                cpx     #ST_MARIO_H_MIN-4+(20*4)+2
                beq     .rivet_hit

                ; update rivet mask

.rivet_miss     ldx     Stage4RivetHit_R
                beq     .rivet_gaps
                lsr
                cpx     #(ST_MARIO_H_MIN+ST_MARIO_H_MAX)/2
                adc     #0
                tax

                lda     Stage4RivetMask_R
                tay
                ora     S4_RivetTab-2,x         ; update rivet bits
                sta     Stage4RivetMask_W
                ldx     #0
                cpy     Stage4RivetMask_R       ; have we already removed the rivet ?
                beq     .rivet_hit
                cmp     #%11111111
                beq     .rivet_score            ; avoid jingle for last rivet

                lda     #SOUND_INIT_MARIO_JINGLE
                sta     ST_AudioCode

.rivet_score    lda     #ST_SCORE_RIVET
                jsr     SH_AddToScore
.rivet_hit      stx     Stage4RivetHit_W

                ; compute gap masks

.rivet_gaps     lda     MarioFloor_R
                lsr
                tax
                ldy     #$FF
.rivet_left     lda     Stage4RivetMask_R
                and     S4_RivetTab-2+0,x
                bne     .rivet_right
                sty     ST_GapMask+2
.rivet_right    lda     Stage4RivetMask_R
                and     S4_RivetTab-2+1,x
                bne     .end
                sty     ST_GapMask+1
.end

; --------------------------------------------------------------------

S4_Enemies:     lda     MarioState_R            ; wait until Mario appears
                beq     S4_End
                lda     ST_FrameSwitch          ; max. every 2nd frame
                beq     S4_End                  ; spare cycles in even frames

; --------------------------------------------------------------------

S4_Flame:       SUBROUTINE

                ; animate flames

                ldx     #0                      ; fill floors from bottom
.loop           ldy     EnemyAnimTimer_R,x
                dey
                tya
                sta     EnemyAnimTimer_W,x

                ; spawn new flame

                lda     EnemyIndex_R,x          ; floor free ?
                bne     .animate
                txa                             ; hammer floor ?
                bne     .spawn                  ; no -> always spawn
                lda     HammerState_R           ; hammer available ?
                beq     .spawn                  ; yes -> spawn
                cmp     #HAMMER_STATE_OFF       ; hammer used ?
                beq     .insert                 ; yes -> immediately respawn
                bne     .next                   ; no -> don't respawn while hammer in use

.spawn          ldy     EnemyTimer_R,x          ; wait before spawning
                iny
                tya
                sta     EnemyTimer_W,x
                cmp     #S4_FLAME_SPAWN_DELAY
                bne     S4_End

                ; insert enemy

.insert         stx     LineCtr                 ; common data
                ldy     #ENEMY_INIT_FLAME
                jsr     SH_EnemyInit
                ldx     LineCtr
                lda     S4_HMinTab,x            ; horizontal position
                ldy     MarioHPos_R
                cpy     #(ST_MARIO_H_MIN+ST_MARIO_H_MAX)/2
                bcs     .write
                lda     S4_HMaxTab,x
.write          sta     EnemyHPos_W,x
                inx                             ; floor
                txa
                asl
                asl
                sta     EnemyFloor_W-1,x
                rts

                ; move flames

.animate        jsr     SH_State
                jsr     SH_Animate
.next           inx
                cpx     #4
                bne     .loop
S4_End:         rts

; --------------------------------------------------------------------

                ;
                ; CLIMB state ----------------------------------------
                ;

S4_FlameClimbSet:SUBROUTINE

                STOREBYTE_X ENEMY_STATE_FLAME_CLIMB, EnemyState_W,x
                STOREBYTE_X                       1, EnemyTimer_W,x

S4_FlameClimb:  SUBROUTINE

                ; climb up then down, then leave

                lda     EnemyVPos_R,x
                beq     S4_FlameMoveSet

.climb          ldy     EnemyAnimTimer_R,x
                cpy     #1
                bne     .end
                clc
                adc     EnemyTimer_R,x
                sta     EnemyVPos_W,x
                cmp     #7
                bne     .end
                lda     #-1
                sta     EnemyTimer_W,x
.end            rts

                ;
                ; MOVE state -----------------------------------------
                ;

S4_FlameMoveSet:SUBROUTINE

                STOREBYTE_X ENEMY_STATE_FLAME_MOVE, EnemyState_W,x

S4_FlameMove:   SUBROUTINE

                ; vertical jumping

.jump           ldy     EnemyTimer_R,x
                iny
                tya
                and     #16-1
                sta     EnemyTimer_W,x
                tay
                lda     S4_FlameMovY,y
                sta     EnemyVPos_W,x

                lda     StageTimer_R
                beq     S4_End

                ; follow Mario on ladder ?

.climb          lda     MarioState_R            ; Mario climbing ?
                cmp     #MARIO_STATE_CLIMB
                bne     .rivet
                lda     MarioFloor_R            ; Mario on same floor ?
                cmp     EnemyFloor_R,x
                bne     .rivet
                lda     MarioHPos_R             ; Mario above enemy ?
                sbc     #12
                cmp     EnemyHPos_R,x
                bne     .rivet
                lda     Random                  ; follow Mario ?
                cmp     #S4_FLAME_CLIMB_PROB
                bcc     S4_FlameClimbSet

                ; setup rivet gap borders

.rivet          STOREBYTE 36, ST_GapBorder+0    ; left border of left gap
                STOREBYTE 40, ST_GapBorder+1    ; right border of left gap
                STOREBYTE 88, ST_GapBorder+2    ; left border of right gap
                STOREBYTE 92, ST_GapBorder+3    ; right border of right gap

                lda     EnemyFloor_R,x
                lsr
                tay
.rivet_left     lda     Stage4RivetMask_R
                and     S4_RivetTab-2+0,y
                bne     .rivet_right
                lda     #0
                sta     ST_GapBorder+0
                sta     ST_GapBorder+1
.rivet_right    lda     Stage4RivetMask_R
                and     S4_RivetTab-2+1,y
                bne     .horizontal
                lda     #0
                sta     ST_GapBorder+2
                sta     ST_GapBorder+3

                ; horizontal movement

.horizontal     tya
                lsr
                tay
                lda     EnemyTemp_R,x
                bmi     .right
                bne     .left

                ; get new direction and duration

.reset          GET_RANDOM
                asl                             ; S -> C
                and     #S4_FLAME_MOVE_MAX-1
                ora     #S4_FLAME_MOVE_MIN
                sta     EnemyTemp_W,x
                bcc     .end

                ; invert direction

.flip           sec
                lda     #0
                sbc     EnemyTemp_R,x
                tay
                bne     .write_tmp

                ; update horizonal position

.left           lda     EnemyHPos_R,x
                cmp     ST_GapBorder+1          ; right border of left gap
                beq     .flip
                cmp     ST_GapBorder+3          ; right border of right gap
                beq     .flip
                cmp     S4_HMinTab-1,y          ; left border
                bcc     .flip
                sbc     #1
                ldy     EnemyTemp_R,x
                dey
                bcs     .write_hpos
.right          lda     EnemyHPos_R,x
                cmp     ST_GapBorder+0          ; left border of left gap
                beq     .flip
                cmp     ST_GapBorder+2          ; left border of right gap
                beq     .flip
                cmp     S4_HMaxTab-1,y          ; right border
                bcs     .flip
                adc     #1
                ldy     EnemyTemp_R,x
                iny

.write_hpos     sta     EnemyHPos_W,x
.write_tmp      tya
                sta     EnemyTemp_W,x
                lsr
                lsr
                lsr
                lsr
                and     #%00001000
                sta     EnemyDir_W,x
.end            rts

; --------------------------------------------------------------------
;       SH Subroutines
; --------------------------------------------------------------------

SH_Stage123Goal:SUBROUTINE
                lda     MarioState_R
                cmp     #MARIO_STATE_WIN        ; goal already reached ?
                beq     .exit
                cmp     #MARIO_STATE_CLIMB      ; CLIMB state ?
                bne     .end

                ldx     TempStage_R
                lda     MarioFloor_R
                cmp     SH_FloorMaxTab,x        ; top floor ?
                bcc     .end

                lda     MarioVPos_R
                and     #%00011111
                cmp     #14                     ; top of ladder ?
                bcc     .end

                STOREBYTE MARIO_STATE_WIN, MarioState_W

.exit           pla
                pla
.end            rts

; --------------------------------------------------------------------

SH_MarioLeft:   SUBROUTINE
                ldx     MarioHPos_R
                cpx     #ST_MARIO_H_MIN
                beq     .end
                dex
                stx     MarioHPos_W
.end            rts

SH_MarioRight:  SUBROUTINE
                ldx     MarioHPos_R
                cpx     #ST_MARIO_H_MAX
                beq     .end
                inx
                stx     MarioHPos_W
.end            rts

; --------------------------------------------------------------------

SH_State:       SUBROUTINE
                ldy     EnemyState_R,x
                lda     SH_EnemyState_L,y
                sta     Ptr
                lda     SH_EnemyState_H,y
                sta     Ptr+1
                jmp     (Ptr)

; --------------------------------------------------------------------

SH_Animate:     SUBROUTINE

                ; check if anim timer reached top or bottom (step end)

                ldy     EnemyAnimState_R,x
.timer_lo       lda     EnemyAnimTimer_R,x
                bne     .timer_hi
                iny
                iny
                bne     .anim_command
.timer_hi       lda     SH_EnemyAnimTab-2,y
                and     #ANIM_TIM_MASK
                cmp     EnemyAnimTimer_R,x
                bcs     .end
                dey
                dey

                ; check anim command (next step)

.anim_command   lda     SH_EnemyAnimTab-2,y
                bpl     .update
                and     #ANIM_CMD_MASK
                cmp     #ANIM_FLIP
                beq     .anim_cmd_flip
                cmp     #ANIM_REF1
                beq     .anim_cmd_ref1
                cmp     #ANIM_REF0
                beq     .anim_cmd_ref0
.anim_cmd_jump  lda     SH_EnemyAnimTab-1,y
                tay
                bcs     .anim_command
.anim_cmd_ref0  lda     EnemyDir_R,x
                and     #%11110111
                bcs     .anim_write
.anim_cmd_ref1  lda     EnemyDir_R,x
                ora     #%00001000
                bcs     .anim_write
.anim_cmd_flip  lda     EnemyDir_R,x
                eor     #%00001000
.anim_write     sta     EnemyDir_W,x

                ; update state and timer

.update         tya
                sta     EnemyAnimState_W,x

                lda     SH_EnemyAnimTab-1,y
                sta     EnemyIndex_W,x

                lda     SH_EnemyAnimTab-2,y
                and     #ANIM_TIM_MASK
                ldy     EnemyAnimTimer_R,x
                beq     .write
                lda     #1
.write          sta     EnemyAnimTimer_W,x
.end            rts

; --------------------------------------------------------------------

SH_EnemyInit:   SUBROUTINE
                lda     #10
                sta     Temp
.loop           lda     SH_EnemyInitTab,y
                sta     EnemyHPos_W,x
                inx
                inx
                inx
                inx
                iny
                dec     Temp
                bne     .loop
                rts

; --------------------------------------------------------------------

SH_SCORE_100 = $01 ; 000100 points
SH_SCORE_300 = $03 ; 000300 points
SH_SCORE_500 = $05 ; 000500 points

SH_ExtraScore:  SUBROUTINE

                ; check Pauline's items

                ldx     ItemIndex_R             ; item picked if index = 0
                bne     SH_JumpOver             ; no -> check enemies
                dex
                stx     ItemIndex_W
                lda     #ST_SCORE_ITEM

                ; score and jingle

SH_Scored:      ldy     #SOUND_INIT_MARIO_JINGLE
                sty     ST_AudioCode

                ; add to digits 2 and 3

SH_AddToScore:  sed
                clc
                adc     Score+1
                sta     Score+1
                lda     #0
                adc     Score+0
                sta     Score+0
                cld

                ; clamp score to 999999

                bcc     .end
                lda     #$99
                sta     Score+0
                sta     Score+1
                sta     Score+2
.end            rts

; --------------------------------------------------------------------

SH_JumpOver:    SUBROUTINE

                ; jump over conditions

                ldy     #4
.loop           lda     EnemyIndex_R-1,y                ; enemy slot used ?
                beq     .next
                lda     MarioFloor_R                    ; enemy and Mario on same floor ?
                cmp     EnemyFloor_R-1,y
                bne     .next

                ; check horizontal overlap

                lda     EnemyDir_R-1,y
                and     #%00000111
                tax
                sec
                lda     MarioHPos_R
                sbc     #8-(4-ST_MARIO_H_OVERLAP)+1
                sbc     EnemyHPos_R-1,y
                cmp     #8+2*(4-ST_MARIO_H_OVERLAP)-1   ; compare main copy
                bcc     .hit
                sbc     SH_Copy2Offset,x
                cmp     #8+2*(4-ST_MARIO_H_OVERLAP)-1   ; compare second copy
                bcc     .hit
                sbc     SH_Copy3Offset,x
                cmp     #8+2*(4-ST_MARIO_H_OVERLAP)-1   ; compare third copy
                bcs     .miss
.hit            lda     EnemyIndex_R-1,y                ; store enemy index -> collision
                sta     EnemyCollision_W
                rts

                ; trigger scoring

.miss           lda     EnemyCollision_R                ; going from hit to miss ?
                beq     .end
                ldx     #0                              ; clear enemy index -> no collision
                stx     EnemyCollision_W
                cmp     #ENEMY_NOJUMP_START             ; enemy type Mario can jump over ?
                bcs     .end
                lda     #ST_SCORE_ENEMY_JUMP
                ldy     MarioState_R
                cpy     #MARIO_STATE_JUMP               ; Mario jumping ?
                beq     SH_Scored
                rts

.next           dey
                bne     .loop
.end            rts

; --------------------------------------------------------------------

SH_HPosBZNoHMOVE:SUBROUTINE
                sec
                sta     WSYNC
.loop           sbc     #15
                bcs     .loop
                eor     #7
                asl
                asl
                asl
                asl
                sta.wx  HMM0,x
                sta     RESM0,x
                rts


; ********************************************************************
;
;       Data Section
;
; ********************************************************************

                ALIGN_END Bank0Start, $02AD

; --------------------------------------------------------------------
;       SH Tables
; --------------------------------------------------------------------

; newly spawned enemies

ENEMY_INIT_BARREL     = 0 * 10 ; must be 0
ENEMY_INIT_FIREBALL_0 = 1 * 10
ENEMY_INIT_FIREBALL_1 = 2 * 10
ENEMY_INIT_JACK       = 3 * 10
ENEMY_INIT_FLAME      = 4 * 10

SH_EnemyInitTab:

                ; barrel

                BYTE    40                         ; EnemyHPos
                BYTE    28                         ; EnemyVPos
                BYTE    5*4                        ; EnemyFloor
                BYTE    %11100000                  ; EnemyDir
                BYTE    ENEMY_EMPTY_GRP            ; EnemyIndex
                BYTE    11+12                      ; EnemyTimer
                BYTE    ENEMY_STATE_BARREL_EDGE    ; EnemyState
                BYTE    4                          ; EnemyAnimTimer
                BYTE    0+1*2+2                    ; EnemyAnimState
                BYTE    0                          ; EnemyTemp

                ; fireball stage 1

                BYTE    20                         ; EnemyHPos
                BYTE    24                         ; EnemyVPos
                BYTE    0*4                        ; EnemyFloor
                BYTE    %00000000                  ; EnemyDir
                BYTE    ENEMY_FIREBALL_0           ; EnemyIndex
                BYTE    26                         ; EnemyTimer
                BYTE    ENEMY_STATE_FIREBALL_JUMP  ; EnemyState
                BYTE    1                          ; EnemyAnimTimer
                BYTE    24+0*2+2                   ; EnemyAnimState
                BYTE    20                         ; EnemyTemp

                ; fireball stage 2

                BYTE    50                         ; EnemyHPos
                BYTE    31                         ; EnemyVPos
                BYTE    2*4                        ; EnemyFloor
                BYTE    %00000000                  ; EnemyDir
                BYTE    ENEMY_FIREBALL_0           ; EnemyIndex
                BYTE    0                          ; EnemyTimer
                BYTE    ENEMY_STATE_FIREBALL_CLIMB ; EnemyState
                BYTE    2                          ; EnemyAnimTimer
                BYTE    24+0*2+2                   ; EnemyAnimState
                BYTE    -1                         ; EnemyTemp

                ; jack

                BYTE    12-1                       ; EnemyHPos
                BYTE    0                          ; EnemyVPos
                BYTE    4*4                        ; EnemyFloor
                BYTE    %00000000                  ; EnemyDir
                BYTE    0                          ; EnemyIndex
                BYTE    17                         ; EnemyTimer
                BYTE    ENEMY_STATE_JACK_JUMP      ; EnemyState
                BYTE    0                          ; EnemyAnimTimer
                BYTE    ENEMY_JACK_1               ; EnemyAnimState
                BYTE    0                          ; EnemyTemp

                ; flame

                BYTE    0                          ; EnemyHPos
                BYTE    0                          ; EnemyVPos
                BYTE    0                          ; EnemyFloor
                BYTE    %00000000                  ; EnemyDir
                BYTE    ENEMY_FLAME_0              ; EnemyIndex
                BYTE    0                          ; EnemyTimer
                BYTE    ENEMY_STATE_FLAME_MOVE     ; EnemyState
                BYTE    2                          ; EnemyAnimTimer
                BYTE    30+0*2+2                   ; EnemyAnimState
                BYTE    0                          ; EnemyTemp

; barrel animation tables

SH_EnemyAnimTab:; barrel roll ( 0 )

                BYTE    ANIM_JUMP  , 0+4*2+2
                BYTE    ANIM_REF0+3, ENEMY_BARREL_ROLL_1
                BYTE    ANIM_REF1+3, ENEMY_BARREL_ROLL_1
                BYTE    ANIM_REF1+3, ENEMY_BARREL_ROLL_0
                BYTE    ANIM_REF0+3, ENEMY_BARREL_ROLL_0
                BYTE    ANIM_JUMP  , 0+1*2+2

                ; barrel fall ( 12 )

                BYTE    ANIM_JUMP  , 12+4*2+2
                BYTE    ANIM_REF0+2, ENEMY_BARREL_FALL_1
                BYTE    ANIM_REF1+2, ENEMY_BARREL_FALL_0
                BYTE    ANIM_REF1+2, ENEMY_BARREL_FALL_1
                BYTE    ANIM_REF0+2, ENEMY_BARREL_FALL_0
                BYTE    ANIM_JUMP  , 12+1*2+2

                ; fireball ( 24 )

                BYTE              3, ENEMY_FIREBALL_0
                BYTE              3, ENEMY_FIREBALL_1
                BYTE    ANIM_JUMP  , 24+0*2+2

                ; flame ( 30 )

                BYTE              3, ENEMY_FLAME_0
                BYTE              3, ENEMY_FLAME_1
                BYTE    ANIM_JUMP  , 30+0*2+2

; enemy attrirbutes

SH_FireblRange: BYTE    (S1_FIREBALL_H_MAX-S1_FIREBALL_H_MIN)+1                   ; stage 1, floor 1
                BYTE    (S2_FIREBALL_H_MAX-S2_FIREBALL_H_MIN)+1                   ; stage 2, floor 2

SH_FireblStart: BYTE    (S1_FIREBALL_H_MAX-S1_FIREBALL_H_MIN)+1+S1_FIREBALL_H_MIN ; stage 1, floor 1
                BYTE    (S2_FIREBALL_H_MAX-S2_FIREBALL_H_MIN)+1+S2_FIREBALL_H_MIN ; stage 2, floor 2

; stage handler jump table

ENEMY_STATE_BARREL_ROLL    =  0 ; must be 0
ENEMY_STATE_BARREL_LADDER  =  1
ENEMY_STATE_BARREL_EDGE    =  2
ENEMY_STATE_FIREBALL_JUMP  =  3
ENEMY_STATE_FIREBALL_MOVE  =  4
ENEMY_STATE_FIREBALL_CLIMB =  5
ENEMY_STATE_CEMENT_MOVE    =  6
ENEMY_STATE_CEMENT_BURN    =  7
ENEMY_STATE_JACK_JUMP      =  8
ENEMY_STATE_JACK_FALL      =  9
ENEMY_STATE_FLAME_MOVE     = 10
ENEMY_STATE_FLAME_CLIMB    = 11

SH_StageHdlr_L: BYTE    <(Stage1-1), <(Stage2-1), <(Stage3-1), <(Stage4-1)
SH_StageHdlr_H: BYTE    >(Stage1-1), >(Stage2-1), >(Stage3-1), >(Stage4-1)

SH_EnemyState_L:BYTE    <(S1_BarrelRoll)  , <(S1_BarrelLadder), <(S1_BarrelEdge)    ; barrel
                BYTE    <(S1_FireballJump), <(S1_FireballMove), <(S1_FireballClimb) ; fireball
                BYTE    <(S2_CementMove)  , <(S2_CementBurn)                        ; cement
                BYTE    <(S3_JackJump)    , <(S3_JackFall)                          ; jack
                BYTE    <(S4_FlameMove)   , <(S4_FlameClimb)                        ; flame
SH_EnemyState_H:BYTE    >(S1_BarrelRoll)  , >(S1_BarrelLadder), >(S1_BarrelEdge)    ; barrel
                BYTE    >(S1_FireballJump), >(S1_FireballMove), >(S1_FireballClimb) ; fireball
                BYTE    >(S2_CementMove)  , >(S2_CementBurn)                        ; cement
                BYTE    >(S3_JackJump)    , >(S3_JackFall)                          ; jack
                BYTE    >(S4_FlameMove)   , >(S4_FlameClimb)                        ; flame

; stage parameters
                ;       stage 1  stage 2  stage 3  stage 4
SH_FloorMaxTab: BYTE        5*4,     3*4,     4*4
SH_HammerPosTab:BYTE        106,       0,       0,      74

; hammer relative missile position
                ;       up     down
SH_HammerRelTab:BYTE    5, 3 , -3, -7 ; left
                BYTE    4, 3 ,  9, 13 ; right

; copy distance
                ;     000  001  010  011  100  101  110  NUSIZ
SH_Copy2Offset: BYTE    0, 2*8, 4*8, 2*8, 8*8, 1*8, 4*8
SH_Copy3Offset: BYTE    0,   0,   0, 2*8,   0,   0, 4*8

; --------------------------------------------------------------------
;       S1 Tables
; --------------------------------------------------------------------

; barrel fall deltas

S1_BarrelFallY: BYTE    -1, 0, 0, 1,-1,-1, 0,-1, 0, 0, 1, 2             ; bounce
                BYTE    -3,-4,-3,-4,-2,-3,-2,-3,-1,-2,-1,-2, 0,-1, 0    ; fall
              ; BYTE    -1

S1_BarrelFallX: BYTE    -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1             ; bounce
                BYTE     0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0    ; fall
              ; BYTE     1

; fireball move absolute Y-positions

S1_FireballMovY:BYTE     1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 0

; fireball jump delta Y-values

S1_FireballJmpY:BYTE    -1,-1,-1,-1,-1,-1, 0,-1, 0, 0, 0, 0, 0, 1, 0, 1
                BYTE     1, 1, 1, 1, 1, 1, 1, 2, 2, 2

; ladder plan

S1_LadderTab:   BYTE     8*4+10, 14*4+10, 23*4+10, 0 ; floor 3
                BYTE     4*4+10,  9*4+10, 21*4+10, 0 ; floor 4
                BYTE    11*4+10,       0, 23*4+10, 0 ; floor 5

; --------------------------------------------------------------------
;       S2 Tables
; --------------------------------------------------------------------

; cement limit tables

S2_CementMin:   BYTE    12-1,     12-1,     12-1,     12-1,     64-1,  12-1
S2_CementMax:   BYTE    116+1+32, 116+1+32, 116+1+64, 116+1+64, 116+1, 64+1
S2_CementNum:   BYTE    ENEMY_CEMENT_NUM_0, ENEMY_CEMENT_NUM_1, ENEMY_CEMENT_NUM_2, ENEMY_CEMENT_NUM_3
                BYTE    ENEMY_CEMENT_NUM_4, ENEMY_CEMENT_NUM_4

; cement attributes

S2_CementFloor: BYTE    1*4, 3*4
S2_CementMovMax:BYTE    S2_CEMENT_MOVE_MAX_F1-1, S2_CEMENT_MOVE_MAX_F3-1

; cement number control tables

ENEMY_CEMENT_NUM_0 = (0*5) + 5
ENEMY_CEMENT_NUM_1 = (1*5) + 5
ENEMY_CEMENT_NUM_2 = (2*5) + 5
ENEMY_CEMENT_NUM_3 = (3*5) + 5
ENEMY_CEMENT_NUM_4 = (4*5) + 5

S2_NumOffsetTab:BYTE    0-1, -16-1, -32-1, -32-1, -32-1
                BYTE    0-1,   0-1, -32-1, -32-1, -32-1
                BYTE    0-1, -32-1, -64-1, -64-1, -64-1
                BYTE    0-1,   0-1, -64-1, -64-1, -64-1
                BYTE    0-1,   0-1,   0-1,   0-1,   0-1

S2_NumLimitTab: BYTE    12+0, 12+16, 12+32, 116+1+0, 116+1+16
                BYTE    12+0, 12+16, 12+32, 116+1+0, 116+1+16
                BYTE    12+0, 12+32, 12+64, 116+1+0, 116+1+32
                BYTE    12+0, 12+32, 12+64, 116+1+0, 116+1+32
                BYTE       0,     0,     0,       0
              ; BYTE       0

S2_NumTab:      BYTE    %00000000, %00000001, %00000011, %00000001, %00000000
                BYTE    %00000000, %00000000, %00000010, %00000000, %00000000
                BYTE    %00000000, %00000010, %00000110, %00000010, %00000000
                BYTE    %00000000, %00000000, %00000100, %00000000, %00000000
                BYTE    %00000000, %00000000, %00000000, %00000000
              ; BYTE    %00000000

; --------------------------------------------------------------------
;       S3 Tables
; --------------------------------------------------------------------

; jack jump absolute Y-values

S3_JackJumpY:   BYTE    00, 3, 6, 8,10,12,13,14,15,15,16,16,16
                BYTE       16,16,15,15,14,13,12,10, 8, 6, 3, 0

; jack sound effect track table (only lo-bytes)

S3_TrackTabL:   BYTE    <(B5_Stage3_JackFall_Track0), <(B5_Stage3_JackJump_Track0)

; --------------------------------------------------------------------
;       S4 Tables
; --------------------------------------------------------------------

; rivet mask table

S4_RivetTab:    BYTE    %00000010, %00000001 ; floor 1
                BYTE    %00001000, %00000100 ; floor 2
                BYTE    %00100000, %00010000 ; floor 3
                BYTE    %10000000, %01000000 ; floor 4

; flame move absolute Y-positions

S4_FlameMovY = S1_FireballMovY

; horizontal floor limits

                ;       floor 1  floor 2  floor 3  floor 4
S4_HMinTab:     BYTE     16-4+1,  20-4+1,  24-4+1,  28-4+1
S4_HMaxTab:     BYTE    112+4  , 108+4  , 104+4  , 100+4

; --------------------------------------------------------------------
;       DK Tables
; --------------------------------------------------------------------

; title girder line colors (blue/pink)

DK_BlockCol0:   BYTE    COL_AE,COL_82,COL_82,COL_82,COL_82,COL_82,COL_AE,COL_82
DK_BlockCol1:   BYTE    COL_40,COL_56,COL_56,COL_56,COL_56,COL_56,COL_40,COL_56

; VCS rainbow color cycle table

#if TV_SYSTEM == "NTSC"
DK_VcsCol:      BYTE    $58,$68,$78,$88,$98,$A8,$B8,$C8
                BYTE    $D8,$E8,$18,$F8,$28,$38,$48,$58 ; NTSC $1X and $FX swapped
                BYTE    $68,$78,$88,$98,$A8,$B8
#else
DK_VcsCol:      BYTE    $88,$A8,$C8,$D8,$B8,$98,$78,$58
                BYTE    $38,$38,$28,$28,$48,$48,$68,$88 ; NTSC $2X mapped to PAL $4X
                BYTE    $A8,$C8,$D8,$B8,$98,$78
#endif

; title oscillation delay lines (approximate damped sine)

DK_BlockSwngTab:BYTE    3,2,1,2,3,5,8,5
              ; BYTE    3

; --------------------------------------------------------------------
;       DK Graphics
; --------------------------------------------------------------------

;
; Title
;

DK_TitlePF1_L:  BYTE    %00000011
                BYTE    %00000011
                BYTE    %00000011
                BYTE    %00000011
                BYTE    %00000011
                BYTE    %00000000
                BYTE    %00000000
                BYTE    %00011001
                BYTE    %00010101
                BYTE    %00010101
                BYTE    %00010101
                BYTE    %00011001

DK_TitlePF2_L:  BYTE    %01111010
                BYTE    %01011010
                BYTE    %01011001
                BYTE    %01011011
                BYTE    %01111010
                BYTE    %00000000
                BYTE    %00000000
                BYTE    %01001011
                BYTE    %01101010
                BYTE    %01111010
                BYTE    %01011010
                BYTE    %01001011

DK_TitlePF0_R:  BYTE    %10010000
                BYTE    %11010000
                BYTE    %11110000
                BYTE    %10110000
                BYTE    %10010000
                BYTE    %00000000
                BYTE    %00000000
                BYTE    %10010000
                BYTE    %01010000
                BYTE    %01110000
                BYTE    %01010000
                BYTE    %10010000

DK_TitlePF1_R:  BYTE    %00111000
                BYTE    %01100100
                BYTE    %01101100
                BYTE    %01100000
                BYTE    %00111000
                BYTE    %00000000
                BYTE    %00000000
                BYTE    %01110010
                BYTE    %01000010
                BYTE    %01110111
                BYTE    %01000101
                BYTE    %01110101

;
; VCS - GRP 0-2
;

DK_VcsGRP0:     BYTE    %00001111 ; 14
                BYTE    %00011111 ; 13
                BYTE    %00011111 ; 12
                BYTE    %00111111 ; 11
                BYTE    %00111001 ; 10
                BYTE    %00111001 ;  9
                BYTE    %01110000 ;  8
                BYTE    %01110000 ;  7
                BYTE    %01110000 ;  6
                BYTE    %01110000 ;  5
                BYTE    %01110000 ;  4
                BYTE    %11100000 ;  3
                BYTE    %11100000 ;  2
                BYTE    %11100000 ;  1
                BYTE    %11100000 ;  0

DK_VcsGRP1:     BYTE    %00000000 ; 14
                BYTE    %10000001 ; 13
                BYTE    %10000011 ; 12
                BYTE    %11000011 ; 11
                BYTE    %11000111 ; 10
                BYTE    %11000111 ;  9
                BYTE    %11100111 ;  8
                BYTE    %11100111 ;  7
                BYTE    %11100111 ;  6
                BYTE    %11100111 ;  5
                BYTE    %11100111 ;  4
                BYTE    %01110011 ;  3
                BYTE    %01110011 ;  2
                BYTE    %01110001 ;  1
                BYTE    %01110000 ;  0

DK_VcsGRP2:     BYTE    %01111101 ; 14
                BYTE    %11111101 ; 13
                BYTE    %11111101 ; 12
                BYTE    %11000000 ; 11
                BYTE    %10000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00000000 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %10000001 ;  4
                BYTE    %11000001 ;  3
                BYTE    %11111101 ;  2
                BYTE    %11111101 ;  1
                BYTE    %01111100 ;  0

;
; JAP US - GRP 0
;

DK_JapUsGRP0:   BYTE    %00000000 ; |        |  15
                BYTE    %00001110 ; |    XXX |  14
                BYTE    %00011011 ; |   XX XX|  13
                BYTE    %00011011 ; |   XX XX|  12
                BYTE    %00011011 ; |   XX XX|  11
                BYTE    %00011011 ; |   XX XX|  10
                BYTE    %00011011 ; |   XX XX|   9
                BYTE    %00011011 ; |   XX XX|   8
                BYTE    %00000000 ; |        |   7
                BYTE    %01100110 ; | XX  XX |   6
                BYTE    %10110110 ; |X XX XX |   5
                BYTE    %00110111 ; |  XX XXX|   4
                BYTE    %00110110 ; |  XX XX |   3
                BYTE    %00110110 ; |  XX XX |   2
                BYTE    %00110011 ; |  XX  XX|   1
                BYTE    %00110001 ; |  XX   X|   0

;
; Dot
;

DK_DotGRP:      BYTE    %00000000
                BYTE    %00000000
                BYTE    %01100110
                BYTE    %01100110
                BYTE    %01100110
                BYTE    %01100110
                BYTE    %00000000
              ; BYTE    %00000000

;
; JAP US - GRP 1
;

DK_JapUsGRP1:   BYTE    %00000000 ; |        |  15
                BYTE    %00111000 ; |  XXX   |  14
                BYTE    %01101100 ; | XX XX  |  13
                BYTE    %00001100 ; |    XX  |  12
                BYTE    %00111000 ; |  XXX   |  11
                BYTE    %01100000 ; | XX     |  10
                BYTE    %01101100 ; | XX XX  |   9
                BYTE    %00111000 ; |  XXX   |   8
                BYTE    %00000000 ; |        |   7
                BYTE    %11011000 ; |XX XX   |   6
                BYTE    %11011000 ; |XX XX   |   5
                BYTE    %11011110 ; |XX XXXX |   4
                BYTE    %11011011 ; |XX XX XX|   3
                BYTE    %11011011 ; |XX XX XX|   2
                BYTE    %10011011 ; |X  XX XX|   1
                BYTE    %00011110 ; |   XXXX |   0

;
; Nintendo - GRP 0-3
;

DK_NintendoGRP0:BYTE    %11001101 ; |XX  XX X|  15
                BYTE    %11001100 ; |XX  XX  |  14
                BYTE    %11011100 ; |XX XXX  |  13
                BYTE    %11111100 ; |XXXXXX  |  12
                BYTE    %11111100 ; |XXXXXX  |  11
                BYTE    %11101100 ; |XXX XX  |  10
                BYTE    %11001101 ; |XX  XX X|   9
                BYTE    %00000000 ; |        |   8
                BYTE    %00001111 ; |    XXXX|   7
                BYTE    %00010000 ; |   X    |   6
                BYTE    %00100111 ; |  X  XXX|   5
                BYTE    %00100100 ; |  X  X  |   4
                BYTE    %00100100 ; |  X  X  |   3
                BYTE    %00100111 ; |  X  XXX|   2
                BYTE    %00010000 ; |   X    |   1
                BYTE    %00001111 ; |    XXXX|   0

DK_NintendoGRP1:BYTE    %11101100 ; |XXX XX  |  15
                BYTE    %11001100 ; |XX  XX  |  14
                BYTE    %11001101 ; |XX  XX X|  13
                BYTE    %11001111 ; |XX  XXXX|  12
                BYTE    %11001111 ; |XX  XXXX|  11
                BYTE    %11001110 ; |XX  XXX |  10
                BYTE    %11101100 ; |XXX XX  |   9
                BYTE    %00000000 ; |        |   8
                BYTE    %10000000 ; |X       |   7
                BYTE    %01000000 ; | X      |   6
                BYTE    %00100000 ; |  X     |   5
                BYTE    %00100000 ; |  X     |   4
                BYTE    %00100000 ; |  X     |   3
                BYTE    %00100000 ; |  X     |   2
                BYTE    %01000000 ; | X      |   1
                BYTE    %10000000 ; |X       |   0

DK_NintendoGRP2:BYTE    %11001100 ; |XX  XX  |  15
                BYTE    %11001100 ; |XX  XX  |  14
                BYTE    %11001100 ; |XX  XX  |  13
                BYTE    %11001100 ; |XX  XX  |  12
                BYTE    %11001100 ; |XX  XX  |  11
                BYTE    %11001100 ; |XX  XX  |  10
                BYTE    %11011110 ; |XX XXXX |   9
                BYTE    %00000000 ; |        |   8
                BYTE    %00000000 ; |        |   7
                BYTE    %11111100 ; |XXXXXX  |   6
                BYTE    %00110000 ; |  XX    |   5
                BYTE    %00110000 ; |  XX    |   4
                BYTE    %00110000 ; |  XX    |   3
                BYTE    %00110001 ; |  XX   X|   2
                BYTE    %01110001 ; | XXX   X|   1
                BYTE    %00110000 ; |  XX    |   0

DK_NintendoGRP3:BYTE    %11111011 ; |XXXXX XX|  15
                BYTE    %11000011 ; |XX    XX|  14
                BYTE    %11000011 ; |XX    XX|  13
                BYTE    %11110011 ; |XXXX  XX|  12
                BYTE    %11000011 ; |XX    XX|  11
                BYTE    %11000011 ; |XX    XX|  10
                BYTE    %11111011 ; |XXXXX XX|   9
                BYTE    %00000000 ; |        |   8
                BYTE    %00000000 ; |        |   7
                BYTE    %11110000 ; |XXXX    |   6
                BYTE    %00011001 ; |   XX  X|   5
                BYTE    %00001101 ; |    XX X|   4
                BYTE    %11111100 ; |XXXXXX  |   3
                BYTE    %10001101 ; |X   XX X|   2
                BYTE    %10001101 ; |X   XX X|   1
              ; BYTE    %11111000 ; |XXXXX   |   0

;
; VCS - GRP 3
;

DK_VcsGRP3:     BYTE    %11111000 ; 14
                BYTE    %11111110 ; 13
                BYTE    %11111111 ; 12
                BYTE    %00001111 ; 11
                BYTE    %00000111 ; 10
                BYTE    %00001111 ;  9
                BYTE    %00011110 ;  8
                BYTE    %01111100 ;  7
                BYTE    %11111000 ;  6
                BYTE    %11110000 ;  5
                BYTE    %11100000 ;  4
                BYTE    %11000000 ;  3
                BYTE    %11111110 ;  2
                BYTE    %11111110 ;  1
                BYTE    %01111110 ;  0

;
; Nintendo - GRP 4-5
;

DK_NintendoGRP4:BYTE    %00110111 ; |  XX XXX|  15
                BYTE    %00110110 ; |  XX XX |  14
                BYTE    %01110110 ; | XXX XX |  13
                BYTE    %11110110 ; |XXXX XX |  12
                BYTE    %11110110 ; |XXXX XX |  11
                BYTE    %10110110 ; |X XX XX |  10
                BYTE    %00110111 ; |  XX XXX|   9
                BYTE    %00000000 ; |        |   8
                BYTE    %00000000 ; |        |   7
                BYTE    %11111000 ; |XXXXX   |   6
                BYTE    %00001100 ; |    XX  |   5
                BYTE    %00111100 ; |  XXXX  |   4
                BYTE    %11110000 ; |XXXX    |   3
                BYTE    %11001000 ; |XX  X   |   2
                BYTE    %10001000 ; |X   X   |   1
                BYTE    %11110000 ; |XXXX    |   0

DK_NintendoGRP5:BYTE    %10001110 ; |X   XXX |  15
                BYTE    %11011011 ; |XX XX XX|  14
                BYTE    %11011011 ; |XX XX XX|  13
                BYTE    %11011011 ; |XX XX XX|  12
                BYTE    %11011011 ; |XX XX XX|  11
                BYTE    %11011011 ; |XX XX XX|  10
                BYTE    %10001110 ; |X   XXX |   9
                BYTE    %00000000 ; |        |   8
                BYTE    %00000000 ; |        |   7
                BYTE    %11111100 ; |XXXXXX  |   6
                BYTE    %00110000 ; |  XX    |   5
                BYTE    %00110000 ; |  XX    |   4
                BYTE    %00110000 ; |  XX    |   3
                BYTE    %00110000 ; |  XX    |   2
                BYTE    %01110000 ; | XXX    |   1
                BYTE    %00110000 ; |  XX    |   0

; ********************************************************************

#if PRINT_MESSAGES
                ECHO "---- Bank 0: ", (ROMJumpTable - *) + GapSize, "bytes of ROM left"
#endif
