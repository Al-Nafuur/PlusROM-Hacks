
; ********************************************************************
;  Donkey Kong VCS
;
;    ROM Bank 2
;
;    $Date: Sat, 21 Jan 2017 21:40:21 +0100 $
;    $Author: dietrich $
;    $Revision: 479 $
;
;  Copyright (C) 2017 Andreas Dietrich
; ********************************************************************

;---------------------------------------------------------------------
;       ST Constants
;---------------------------------------------------------------------

ST_MARIO_H_MIN          =  24                           ; leftmost position of Mario
ST_MARIO_H_MAX          = 128                           ; rightmost position of Mario
ST_MARIO_H_OVERLAP      =   3                           ; collision detection overlap (3 pixels left and right)
ST_MARIO_V_JUMPOVER     =   8                           ; no collision if Mario jumps high enough
ST_MARIO_EARLY_JUMP     =  12                           ; button debounce interval for ladders
ST_MARIO_START_DELAY    =  30                           ; delay Mario's appearance by 1 second

ST_SCORE_ENEMY_JUMP     = SH_SCORE_100                  ; jumping an enemy
ST_SCORE_ENEMY_KILL     = SH_SCORE_500                  ; smashing an enemy
ST_SCORE_ITEM           = SH_SCORE_300                  ; collecting one of Pauline's items
ST_SCORE_LADDER         = SH_SCORE_500                  ; taking the left ladder to Kong
ST_SCORE_RIVET          = SH_SCORE_100                  ; eliminating a rivet

;---------------------------------------------------------------------
;       ST Variables
;---------------------------------------------------------------------

;
; RIOT RAM -----------------------------------------------------------
;

; temporary variables (vertical blank or overscan) -------------------

ST_AudioCode            = ScorePtr +  0                 ; music/sound code for DriveAudio
ST_ShadowSWCHA          = ScorePtr +  1                 ; retain a consistent SWCHA state
ST_ShadowINPT           = ScorePtr +  2                 ; retain a consistent INPT state

ST_PtrOffset            = ScorePtr +  2                 ; offset for GRP and COL pointers
ST_GRPPtrTmp            = ScorePtr +  3 ; [2]           ; temporary GPR pointer
ST_COLPtrTmp            = ScorePtr +  5 ; [2]           ; temporary COL pointer
ST_VPosTmp              = ScorePtr +  7                 ; temporary enemy vertical position
ST_RefNuSizTmp          = ScorePtr +  8                 ; temporary enemy REF/NUSIZ value
ST_CullTmp              = ScorePtr +  9                 ; temporary enemy cull table entry
ST_HMPosTmp             = ScorePtr + 10                 ; temporary horizontal motion fine position
ST_HMDelayTmp           = ScorePtr + 11                 ; temporary horizontal motion coarse position

ST_GapBorder            = ScorePtr +  4 ; [4]           ; left and right borders of rivet gaps
ST_GapMask              = ScorePtr +  8 ; [4]           ; dynamic gaps for rivets

; temporary variables (kernel after score) ---------------------------

ST_PFOffset             = ScorePtr +  0 ; [9]           ; dynamic playfield offsets (rivets, ladders)

ST_COLTmp               = ScorePtr +  0                 ; transfer color from display bank
ST_PF1Tmp               = ScorePtr +  1                 ; transfer PF1 from display bank
ST_PF2Tmp               = ScorePtr +  2                 ; transfer PF2 from display bank
ST_PtrTmp               = ScorePtr +  3 ; [2]           ; temporary pointer (enemy calculations, sub-kernel jump)

ST_PFCol                = ScorePtr +  5 ; [4]           ; 4 bands, kernel playfield color for corridors

ST_DisplayBank          = ScorePtr +  9                 ; switch between display banks (bank 3 or 4)

; kernel variables ---------------------------------------------------

ST_VarStart = $9A

ST_MarioGRPPtr          = ST_VarStart+$00 ; [8*2]       ; 8 bands Mario graphics
ST_MarioCOLPtr          = ST_VarStart+$10 ; [8*2]       ; 8 bands Mario line colors
ST_EnemyGRPPtr          = ST_VarStart+$20 ; [9*2]       ; 8+1 bands (4 floors + offscreen pointer)
ST_EnemyCOLPtr          = ST_VarStart+$32 ; [5*2]       ; 4+1 bands (line colors only for corridors)
ST_PFIdx                = ST_VarStart+$3C ; [8*1]       ; playfield index must come before ST_HMPos (writes to ST_HMPos-1)

ST_HMPos                = ST_VarStart+$44 ; [5]         ; horizontal fine positioning (HMP)
ST_HMDelay              = ST_VarStart+$49 ; [5]         ; horizontal coarse positioning (RESP delay loop)
ST_RefNuSiz             = ST_VarStart+$4E ; [5]         ; enemy REF/NUSIZ

ST_Floor                = ST_VarStart+$53               ; base floor of scrolling window
ST_ScrollCtr0           = ST_VarStart+$54               ; lines to draw of top band
ST_ScrollCtr1           = ST_VarStart+$55               ; lines to draw of bottom band
ST_M0Mask               = ST_VarStart+$56               ; mask for hammer missile
ST_FrameSwitch          = ST_VarStart+$57               ; LSB of FrameCtr (even/odd frame)
                        ; $F1 -- end of kernel specific variables

;
; Super Chip RAM -----------------------------------------------------
;

        ; generate R/W address labels

        MAC SC_RAM

{1}_W = SCRAMWrite + {2}
{1}_R = SCRAMRead  + {2}

        ENDM

SC_RAM_CLR_FIRST = MarioTimer_W
SC_RAM_CLR_LAST  = KongAnimState_W
SC_RAM_CLR_RANGE = SC_RAM_CLR_LAST - SC_RAM_CLR_FIRST + 1

        ; SARA memory map

        SC_RAM  MarioGRPBuffer,     $00 ; [16+24]       ; GRP data plus empty space
        SC_RAM  MarioCOLBuffer,     $28 ; [16]          ; COL data
        ;
        ; $38 - $39 unused
        ;

        SC_RAM  Stage2LadderState,  $3A                 ; telescope ladder state
        SC_RAM  Stage2LadderLimit,  $3B                 ; dynamic limit of how high Mario can climb
        SC_RAM  Stage2LadderOffset, $3C                 ; scrolling ladder playfield pointer offset

        SC_RAM  ButtonDebounce,     $3D                 ; store button state from previous frame
        SC_RAM  StageSpeedMask,     $3E                 ; AND mask for FrameCtr
        SC_RAM  BonusTimer,         $3F                 ; speed of bonus countdown
        SC_RAM  DemoMode,           $40                 ; demo mode (on: $00, off: $FF)

        SC_RAM  ItemIndex,          $41                 ; extra items (Pauline's stuff and oil barrel)
        SC_RAM  ItemPos,            $42                 ; item horizontal position
        SC_RAM  ItemGRPH,           $43                 ; GRP pointer hi-byte
        SC_RAM  ItemCOLH,           $44                 ; COL poinyer hi-byte

        SC_RAM  TempLevel,          $45                 ; current level (from Level_Round)
        SC_RAM  TempStage,          $46                 ; current stage (from Level_Round)

        SC_RAM  MarioHPos,          $47                 ; horizontal position
        SC_RAM  MarioVPos,          $48                 ; vertical position relative to bottom band
        SC_RAM  MarioYPos,          $49                 ; vertical position relative to bottom window border
        SC_RAM  MarioFloor,         $4A                 ; current floor x4
        SC_RAM  MarioDir,           $4B                 ; SWCHA: [%XX------], REFP: [%----X---]
        ;
        ; ----- cleared from here on -----
        ;
        SC_RAM  MarioTimer,         $4C                 ; general purpose state timer
        SC_RAM  MarioState,         $4D                 ; current state (walking, climbing, ...)
        SC_RAM  MarioAnimTimer,     $4E                 ; animation timer
        SC_RAM  MarioAnimState,     $4F                 ; animation state (frame)

        SC_RAM  EnemyHPos,          $50 ; [4]           ; horizontal position
        SC_RAM  EnemyVPos,          $54 ; [4]           ; vertical position relative to floor
        SC_RAM  EnemyFloor,         $58 ; [4]           ; floor x4
        SC_RAM  EnemyDir,           $5C ; [4]           ; pos/neg: [%XXXX----], REFP: [%----X---], NUSIZ: [%-----XXX]
        SC_RAM  EnemyIndex,         $60 ; [4]           ; enemy slots (0: empty, 1: invisible, index)
        SC_RAM  EnemyTimer,         $64 ; [4]           ; general purpose state timer
        SC_RAM  EnemyState,         $68 ; [4]           ; current state (barrel rolling, fireball climbing, ...)
        SC_RAM  EnemyAnimTimer,     $6C ; [4]           ; animation timer
        SC_RAM  EnemyAnimState,     $70 ; [4]           ; animation state (frame)
        SC_RAM  EnemyTemp,          $74 ; [4]           ; general purpose storage depending on enemy type

        SC_RAM  EnemyCollision,     $78                 ; enemy index if software collision detected
        SC_RAM  HammerState,        $79                 ; hammer state (avail, snag, off, on)

        SC_RAM  StageTimer,         $7A                 ; level progression countdown, used to skip code if 0
        SC_RAM  Stage3Elevator,     $7B                 ; signals if Mario did hit elevator (0/1)
        SC_RAM  Stage4RivetMask,    $7C                 ; rivets on/off, floor 1: [%------XX], floor 2: [%----XX--], ...
        SC_RAM  Stage4RivetHit,     $7D                 ; store Mario's horizontal position if rivet is touched

        SC_RAM  KongAnimTimer,      $7E                 ; Kong animation timer
        SC_RAM  KongAnimState,      $7F                 ; Kong animation state (frame)


; ********************************************************************
;
;       Code Section
;
; ********************************************************************

; --------------------------------------------------------------------
;       Stage
; --------------------------------------------------------------------

Stage:          STOREBYTE 2, Bank

; --------------------------------------------------------------------
;       ST Init
; --------------------------------------------------------------------

ST_Init:        SUBROUTINE

                ; clear RAM and TIA registers

                CLEAR_RAM_TIA ST_VarStart, StackBottom

                ; Super Chip

                ldx     #(SC_RAM_CLR_RANGE/2)-1
.loop0          sta     SC_RAM_CLR_FIRST+0*SC_RAM_CLR_RANGE/2,x
                sta     SC_RAM_CLR_FIRST+1*SC_RAM_CLR_RANGE/2,x
                dex
                bpl     .loop0

                ; non-zero variables

                STOREBYTE %00001000,               MarioDir_W         ; Mario faces right
                STOREBYTE S1_FIREBALL_SPAWN_DELAY, EnemyAnimTimer_W+3 ; initial fireball delay in stage 1

                ; preset elevator speed

                ldy     #%00000011
                lda     Level_Round
                cmp     #S3_ELEVATOR_FAST_L
                bcc     .write
                ldy     #%00000001
.write          sty     StageSpeedMask_W

                ; Mario's initial position

                ldx     #3
                and     #STAGE_MASK             ; A = Level_Round
                cmp     #2
                bne     .skip
                ldx     #7
.skip           ldy     #3
.loop1          lda     ST_MarioSetTab,x
                sta     MarioHPos_W,y
                dex
                dey
                bpl     .loop1

                jsr     ST_MarioClrPtrs

                ; noise player

                lda     TempStage_R
                ora     #(AUDIO_MUSIC_INIT | AUDIO_CHANNEL_0)
                jsr     Bank5_DriveAudio

.wait           lda     INTIM
                bne     .wait

; --------------------------------------------------------------------
;       ST VSync
; --------------------------------------------------------------------

ST_VSync:       VERTICAL_SYNC

; --------------------------------------------------------------------
;       ST VBlank
; --------------------------------------------------------------------

ST_VBlank:

;
; Mario --------------------------------------------------------------
;

ST_MarioAnim:   SUBROUTINE

                ; progress Mario animation state (timer counted in state machine)

                ldx     MarioAnimState_R
.timer_lo       lda     MarioAnimTimer_R        ; timer decreased to 0 ?
                bne     .timer_hi               ; no -> check upper bound
                inx                             ; yes -> next command
                inx
                bne     .anim_command
.timer_hi       lda     ST_MarioAnimTab-2,x
                and     #ANIM_TIM_MASK
                cmp     MarioAnimTimer_R        ; timer increased to table value ?
                bcs     .load                   ; no -> set pointers
                dex                             ; yes -> previous command
                dex

                ; animation commands

.anim_command   lda     ST_MarioAnimTab-2,x
                bpl     .next
                cmp     #ANIM_JUMP
                bne     .anim_cmd_flip
.anim_cmd_jump  lda     ST_MarioAnimTab-1,x
                tax
                bcs     .anim_command
.anim_cmd_flip  tay
                lda     MarioDir_R
                eor     #%00001000
                sta     MarioDir_W
                tya
                and     #ANIM_TIM_MASK

                ; progress animation state

.next           ldy     MarioAnimTimer_R        ; did we count down ?
                beq     .write                  ; yes -> set new start value
                lda     #1                      ; no -> start with 1
.write          sta     MarioAnimTimer_W
                stx     MarioAnimState_W

                ; prepare GRP and COL pointers

.load           ldy     ST_MarioAnimTab-1,x

                lda     HammerState_R
                cmp     #HAMMER_STATE_OFF+1     ; hammer in use ?
                bcc     .switch                 ; no -> Mario walking
                iny                             ; yes -> Mario holds hammer
                iny

                lda     ST_M0Mask
                and     #%00001000
                beq     .switch                 ; hands up/down
                iny
                iny

.switch         lda     FrameCtr
                lsr
                bcc     .store
                iny

.store          lda     ST_MarioGRP_L,y
                sta     ST_GRPPtrTmp
                lda     ST_MarioGRP_H,y
                sta     ST_GRPPtrTmp+1
                lda     ST_MarioCOL_L,y
                sta     ST_COLPtrTmp
                lda     ST_MarioCOL_H,y
                sta     ST_COLPtrTmp+1

; --------------------------------------------------------------------

ST_MarioScroll: SUBROUTINE

                bcs     .end                    ; only scroll every second frame

.loop           ldy     MarioYPos_R

                ; scroll screen up

.up             cpy     #(1*32)
                bcs     .down

.scroll_up      ldx     ST_ScrollCtr0
                bne     .move_up
                lda     ST_Floor
                cmp     #1
                bcc     .end
.move_up        iny
                sty     MarioYPos_W
                dex
                bpl     .write_up
                clc
                lda     #32
                adc     MarioVPos_R
                sta     MarioVPos_W
                clc
                lda     #-4
                adc     ST_Floor
                sta     ST_Floor
                ldx     #31
.write_up       stx     ST_ScrollCtr0

                ; loop to match Mario's position when we fall

                lda     MarioState_R
                cmp     #MARIO_STATE_JUMP
                beq     .loop
                bne     .end

                ; scroll screen down

.down           cpy     #(1*32)+1
                bcc     .end

                ; always scroll when Mario is out of focus

                lda     MarioFloor_R
                sbc     ST_Floor                ; C = 1
                cmp     #2*4
                bcs     .scroll_down

                ; don't scroll when we jump or hit an enemy

                lda     MarioState_R
                cmp     #MARIO_STATE_JUMP
                beq     .end
                cmp     #MARIO_STATE_DEAD
                beq     .end

.scroll_down    ldx     ST_ScrollCtr0
                bne     .move_down
                ldx     TempStage_R
                lda     ST_Floor
                cmp     ST_FloorMaxTab,x
                bcs     .end
                ldx     #0
.move_down      dey
                sty     MarioYPos_W
                inx
                cpx     #32
                bcc     .write_down
                clc
                lda     #-32
                adc     MarioVPos_R
                sta     MarioVPos_W
                clc
                lda     #4
                adc     ST_Floor
                sta     ST_Floor
                ldx     #0
.write_down     stx     ST_ScrollCtr0
.end

; --------------------------------------------------------------------

ST_MarioCopy:   SUBROUTINE

                ; copy Mario from ROM to Super Chip RAM

                ldx     #5

                lda     HammerState_R
                cmp     #HAMMER_STATE_OFF+1
                bcc     .copy

                ldx     #7

.copy           jsr     BankX_CopyMario

;
; Enemies ------------------------------------------------------------
;
                ; clear enemy GRP and COL pointers

ST_EnemyClrPtrs:SUBROUTINE
                ldx     #(8*2)
                ldy     #<(EmptyGRP-1)
                lda     #>(EmptyGRP-1)
.loop           sta     ST_EnemyGRPPtr-1,x
                dex
                sty     ST_EnemyGRPPtr-1,x
                dex
                bne     .loop

                sty     ST_EnemyCOLPtr+4*2
                sta     ST_EnemyCOLPtr+4*2+1

                ; ----------------------------------------------------

                ; clear enemy HMPos, HMDelay, RefNuSiz attributes

ST_EnemyClrAttr:SUBROUTINE
                ldy     #(3*5)-1                ; X = 0
.loop           stx     ST_HMPos,y
                dey
                bpl     .loop

                ; ----------------------------------------------------

                ; setup enemy pointers

ST_EnemySetPtrs:SUBROUTINE

                ; enemies off ?

                lda     MarioState_R
                beq     .finish
                cmp     #MARIO_STATE_WIN
                beq     .finish
                cmp     #MARIO_STATE_DEAD
                bne     .skip
                lda     MarioTimer_R
                cmp     #4*30
                bcc     .finish
.skip
                ; loop over enemies

                STOREBYTE 4, LineCtr
.loop           dec     LineCtr
                ldy     LineCtr
                bpl     .cull
.finish         jmp     .end

                ; cull enemy sprites

.cull           lda     EnemyIndex_R,y          ; skip empty slots
                cmp     #ENEMY_INDEX_START
                bcc     .loop
                lda     EnemyFloor_R,y          ; skip off-screen enemies
                sec
                sbc     ST_Floor
                cmp     #4*4
                bcc     .load
                cmp     #-1*4
                bne     .loop
.load           pha

                ; load parameters

                ldx     EnemyHPos_R,y
                jsr     ST_PrecalcHPos
                lda     EnemyVPos_R,y
                sta     ST_VPosTmp
                lda     EnemyDir_R,y
                sta     ST_RefNuSizTmp

                ; load and switch index

                lda     EnemyIndex_R,y
                ora     ST_FrameSwitch
                tax

                ; adjust base pointers according to vpos

.adjust         sec
                lda     ST_EnemyGRP_L-2,x
                sbc     ST_VPosTmp
                sta     ST_GRPPtrTmp
                lda     ST_EnemyGRP_H-2,x
                sbc     #0
                sta     ST_GRPPtrTmp+1
                sec
                lda     ST_EnemyCOL_L-2,x
                sbc     ST_VPosTmp
                sta     ST_COLPtrTmp
                lda     ST_EnemyCOL_H-2,x
                sbc     #0
                sta     ST_COLPtrTmp+1

                lda     ST_EnemyCull-2,x
                sta     ST_CullTmp

                ; set pointers in current floor band

.band0          pla
                tay
                tax

                lda     ST_VPosTmp
                cmp     #BAND_24_SIZE           ; completly above floor band ?
                bcs     .band1
                cpy     #-1*4                   ; floor band off screen ?
                beq     .b0skip

.b0             STOREBYTE 0, ST_PtrOffset
                jsr     ST_EnemySetGRP
                jsr     ST_EnemySetCA

.b0skip         lda     ST_VPosTmp
                cmp     ST_CullTmp              ; completly inside floor band ?
                bcc     .next

                lda     #0                      ; HMPos needs to be set in next floor, not here
                sta     ST_HMPos,x                      ; beware! X can be negative (-1*4)

                ; set pointers in girder band

.band1          iny                             ; next band
                iny
                tya
                and     #(4*4)-1
                tax

                lda     ST_ScrollCtr0
.b1lo           cpy     #(-1*4)+2               ; skip bottom girder band ?
                bne     .b1hi
                cmp     #8
                bcs     .band2
.b1hi           cpy     #(3*4)+2                ; skip top girder band ?
                bne     .b1
                cmp     #8
                bcc     .band2

.b1             STOREBYTE 0+BAND_24_SIZE, ST_PtrOffset
                jsr     ST_EnemySetGRP

                ; set pointers in next floor band

.band2          iny                             ; next band
                iny
                tya
                tax

                cpy     #5*4                    ; floor band off screen ?
                bcs     .next

.b2             STOREBYTE 0+BAND_24_SIZE+BAND_08_SIZE, ST_PtrOffset
                lda     ST_VPosTmp
                sec
                sbc     #BAND_08_SIZE+1
                cmp     ST_CullTmp              ; inside next floor band ?
                bcc     .b2skip
                jsr     ST_EnemySetGRP
.b2skip         jsr     ST_EnemySetCA

.next           jmp     .loop
.end

;
; Items --------------------------------------------------------------
;

ST_ItemSetup:   SUBROUTINE

                ; item index holds state (>0 = index, 0 = jingle, <0 = done)

                ldy     ItemIndex_R
                beq     .end
                bmi     .end
                tya
                ora     ST_FrameSwitch
                tax

                ; check if item visible

                cpy     #3*2
                beq     .stage_3
.stage_124      ldy     ST_Floor                ; item on ground floor in stages 1/2/4
                bne     .end
                beq     .animate                ; Y = 0
.stage_3        lsr                             ; display in sync with elevator
                bcc     .end
                lda     #4*4                    ; C = 1
                sbc     ST_Floor
                tay

                ; fire animation

.animate        lda     EnemyDir_R+2            ; fake animation
                eor     EnemyDir_R+1            ; don't use GET_RANDOM/Random
                asl                             ; C := S
                lda     ST_ItemGRP_L-2,x
                bcc     .write
                cpx     #2*2                    ; animate only oil barrel
                bcs     .write
                adc     #B3_OIL_HEIGHT*2        ; C = 0
.write          sta     ST_EnemyGRPPtr,y

                ; set remaining GRP and COL pointers

                lda     ItemGRPH_R
                sta     ST_EnemyGRPPtr+1,y
                tya
                lsr                             ; GRP -> COL index
                tay
                lda     ST_ItemCOL_L-2,x
                sta     ST_EnemyCOLPtr,y
                lda     ItemCOLH_R
                sta     ST_EnemyCOLPtr+1,y

                ; set horizontal position

                tya
                lsr                             ; COL -> HM index
                tay
                lda     ItemPos_R
                sta     ST_HMPos,y
                and     #$0F
                sta     ST_HMDelay,y
.end

; --------------------------------------------------------------------
;       ST Kernel
; --------------------------------------------------------------------

ST_Kernel:      lda     INTIM
                bne     ST_Kernel

                ldy     #255
                sty     Stage2LadderLimit_W
                sta     Stage2LadderOffset_W

;
; Score --------------------------------------------------------------
;
                ; draw score and bonus

ST_Score:       jsr     Bank7_DrawScore

                ldy     #12
                sty     TIM64T

;
; Playfield ----------------------------------------------------------
;
                ; set playfield offsets (dynamically change background)

ST_SetPFOffsets:SUBROUTINE
                lda     ST_Floor
                lsr
                sta     Temp

                ; calculate offset for stage 2 ladder

                lda     #(4*2)+1                ; C = 0
                sbc     Temp                    ; Temp = Floor/2
                tax
                ldy     Stage2LadderOffset_R
                sty     ST_PFOffset-1,x

                ; calculate offsets for stage 4 rivets

                sec
                sbc     #3*2                    ; A := 1*2 - Floor/2
                tax

                lda     Stage4RivetMask_R
                ldy     #3
.loop           pha
                asl
                asl
                asl
                and     #%00011000
                cpx     #0
                bmi     .skip
                sta     ST_PFOffset,x
.skip           pla
                lsr
                lsr
                inx
                inx
                dey
                bpl     .loop

                ; ----------------------------------------------------

                ; set playfield indices

ST_SetPFIdx:    SUBROUTINE
                lda     TempStage_R
                pha                             ; remember stage
                tax
                lda     Temp                    ; Temp = Floor/2
                clc
                adc     ST_FloorPFIdxOffsets,x
                tax

                ldy     #8-1
.loop           lda     ST_FloorPFIdx,x
                adc     ST_PFOffset+1,y
                sta     ST_PFIdx,y
                dex
                dey
                bpl     .loop

                lda     ST_ScrollCtr0
                cmp     #8
                bcs     .end
                lda     ST_FloorPFIdx,x
                adc     ST_PFOffset+0
                sta     ST_PFIdx+7
.end
                ; ----------------------------------------------------

                ; set playfield colors

ST_SetPFCol:    SUBROUTINE
                pla                             ; retrieve stage
                tax
                lda     Temp                    ; Temp = Floor/2
                lsr
                adc     ST_FloorPFColOffsets,x  ; C = 0
                tax

                ldy     #4-1
.loop           lda     ST_FloorPFCol,x
                sta     ST_PFCol,y
                dex
                dey
                bpl     .loop

;
; Scrolling ----------------------------------------------------------
;

ST_Scrolling:   SUBROUTINE

                ; adjust scroll counters

                lda     ST_ScrollCtr0
                and     #32-1
                sta     ST_ScrollCtr0
                sec
                sbc     #8
                bpl     .write
                lda     #0
.write          sta     ST_ScrollCtr1

                ; adjust top bands

                lda     ST_ScrollCtr0
                cmp     #23
                bcs     .band1

.band0          lda     ST_PFIdx+6
                sec
                sbc     #22
                clc
                adc     ST_ScrollCtr0
                sta     ST_PFIdx+6
                jmp     .end

.band1          lda     ST_PFIdx+7
                sec
                sbc     #32
                clc
                adc     ST_ScrollCtr0
                sta     ST_PFIdx+7
.end

;
; Mario --------------------------------------------------------------
;

ST_MarioSetPtrs:SUBROUTINE

                ; skip displaying Mario when the game starts up

                lda     MarioState_R
                beq     .end

                ; get floor Mario is on

                lda     MarioVPos_R
                lsr
                lsr
                lsr
                and     #255-3
                tax

                ; get Mario's Y-position relative to floor

                lda     MarioVPos_R
                and     #FLOOR_HEIGHT-1
                tay
                sty     ST_PtrOffset

                ; preset COL buffer lo-bytes

                lda     #<(MarioCOLBuffer_R)
                sec
                sbc     ST_PtrOffset
                sta     ST_MarioCOLPtr+0*2,x
                adc     #BAND_24_SIZE-1
                sta     ST_MarioCOLPtr+1*2,x
                adc     #BAND_08_SIZE
                sta     ST_MarioCOLPtr+2*2,x

                ; set relevant GRP and COL band pointers

.band0          cpy     #BAND_24_SIZE
                bcs     .band1
                lda     #<(MarioGRPBuffer_R+8)
                sta     ST_MarioGRPPtr+0*2,x
                lda     #>SCRAMRead
                sta     ST_MarioGRPPtr+0*2+1,x
                sta     ST_MarioCOLPtr+0*2+1,x

.band1          lda     #<(MarioGRPBuffer_R+8+24)
                sta     ST_MarioGRPPtr+1*2,x
                lda     #>SCRAMRead
                sta     ST_MarioGRPPtr+1*2+1,x
                sta     ST_MarioCOLPtr+1*2+1,x

.band2          cpy     #MARIO_HEIGHT
                bcc     .end
                lda     #<(MarioGRPBuffer_R)
                sta     ST_MarioGRPPtr+2*2,x
                lda     #>SCRAMRead
                sta     ST_MarioGRPPtr+2*2+1,x
                sta     ST_MarioCOLPtr+2*2+1,x
.end

;
; Hammer -------------------------------------------------------------
;

ST_Hammer:      SUBROUTINE

                lda     ST_M0Mask
                lsr
                lsr
                bcc     .end
                tax

                lda     ST_RefNuSiz+1
                and     #%11001111
                ora     ST_HammerNUSIZ,x
                sta     ST_RefNuSiz+1

                lda     ST_HammerCOL_L,x
                sta     ST_EnemyCOLPtr+1*2
                lda     #>B3_Hammer1_COL_0
                sta     ST_EnemyCOLPtr+1*2+1
.end

;
; Top ----------------------------------------------------------------
;

ST_SetupTop:    SUBROUTINE

                ; precalc Mario position

                ldx     MarioHPos_R
                jsr     ST_PrecalcHPos

                ; sync beam again

.loop           lda     INTIM
                bne     .loop
                sta     WSYNC

                ; set Kong colors

                ldx     ST_FrameSwitch
                lda     ST_KongCol,x
                sta     COLUP0
                sta     COLUP1

                ; prepare display bank

                lda     TempStage_R
                lsr                             ; LSB -> C

                ; fine positon Kong (and ladder)

                lda     #$80
                sta     HMP0
                lda     #$90
                sta     HMP1

                ; set display bank

                adc     #-$90+3                 ; A := 3 + C
                sta     ST_DisplayBank

                ; coarse positon Kong (and ladder)

                sta     RESP0
                sta     RESP1
                sta     WSYNC
                sta     HMOVE

                ; preset top girder where Kong is completely visible

                ldy     #32-1
                STOREWORD Floor1_3_Cnt, ST_PtrTmp

                ; Kong visible at all ?

                ldx     TempStage_R
                lda     ST_Floor
                cmp     ST_FloorMaxTab,x
                beq     ST_SetTopGirder
                cmp     ST_FloorKongTab,x
                bcc     ST_SetTopEnemy
                ldy     ST_ScrollCtr0
                beq     ST_SetTopEnemy

                ; set top girder where Kong is partially visible

                dey
                STOREWORD Floor2_3_Cnt, ST_PtrTmp

                ; ----------------------------------------------------

ST_SetTopGirder:SUBROUTINE

                ; pre-fetch data for first girder line

#if (DrawTop & $FF00) != (GetTopGirder & $FF00) - $0100
                ECHO    "ERROR: Can't optimize ST_SetTopGirder"
#endif
                STOREWORD GetTopGirder, Ptr

                ldx     ST_DisplayBank
                jsr     BankX_DrawFloors

                lda     #<DrawTop
                sta     Ptr
                dec     Ptr+1

                ; set Mario Position

                lda     ST_HMPosTmp
                sta     ST_HMPos+4
                lda     ST_HMDelayTmp
                sta     ST_HMDelay+4

                ; draw Kong on top

                jsr     ST_DrawKong

                ; first girder line -- reset Mario

                lda     ST_COLTmp
                sta     COLUPF
                lda     ST_PF1Tmp
                sta     PF1
                lda     ST_PF2Tmp
                sta     PF2

                lda     #0
                sta     GRP0
                sta     GRP1
                sta     GRP0
                sta     NUSIZ1

                ldx     ST_DisplayBank
                jsr     BankX_DrawFloors
                jmp     ST_KernelEnd

                ; ----------------------------------------------------

ST_SetTopEnemy: SUBROUTINE

                STOREWORD SwitchFloors, Ptr

                ; set Mario attributes and position

                ldx     #%00000000
                stx     VDELP1
                stx     NUSIZ1
                lda     MarioDir_R
                sta     REFP1

                pha                             ; one reserve scanline
                pla                             ; sleep 3+4

                lda     MarioHPos_R
                sec
                sta     WSYNC
.loop           sbc     #15
                bcs     .loop
                eor     #7
                asl
                asl
                asl
                asl
                inx                             ; X := 1
                sta     HMP1
                sta     RESP0,x

                ; set enemy attributes and position

                sta     WSYNC                   ; X = 1
                lda     ST_ScrollCtr0
                cmp     #BAND_24_SIZE+1
                bcs     .setattr
                lda     ST_HMPos+3-1,x
                beq     .setattr
                dex
.setattr        lda     ST_RefNuSiz+3,x
                sta     WSYNC
                sta     REFP0
                sta     NUSIZ0
                nop
                lda     ST_HMPos+3,x
                sta     HMP0
                ldy     ST_HMDelay+3,x
.resdelay       dey
                bpl     .resdelay
                sta     RESP0
                sta     WSYNC
                sta     HMOVE

                ; prepare GPR0 and COLUP0 adjustment

                ldx     #0
                stx     ST_PtrTmp
                stx     ST_PtrTmp+1
                ldy     ST_ScrollCtr0
                cpy     #BAND_24_SIZE
                bcc     .write
                ldx     #2
                lda     #BAND_24_SIZE
                sta     ST_PtrTmp
                sty     ST_PtrTmp+1

                ; set first line of enemy GRP0 and COLUP0

.write          sec
                lda     ST_EnemyGRPPtr+6*2,x
                sbc     ST_PtrTmp
                sta     ST_EnemyGRPPtr+8*2
                lda     ST_EnemyGRPPtr+6*2+1,x
                sbc     #0
                sta     ST_EnemyGRPPtr+8*2+1
                sec
                lda     ST_EnemyCOLPtr+3*2,x
                sbc     ST_PtrTmp+1
                sta     ST_EnemyCOLPtr+4*2
                lda     ST_EnemyCOLPtr+3*2+1,x
                sbc     #0
                sta     ST_EnemyCOLPtr+4*2+1

                sta     HMCLR

                ldx     ST_DisplayBank
                jsr     BankX_DrawFloors

ST_KernelEnd:

; --------------------------------------------------------------------
;       ST Overscan
; --------------------------------------------------------------------

ST_Overscan:    ldy     #0                      ; clear channels
                sty     GRP0
                sty     GRP1
                sty     GRP0
                sty     PF1
                sty     PF2

                lda     #52                     ; set timer to skip OVERSCAN
                sta     TIM64T

                CHECK_RESET

                inc     FrameCtr
                lda     FrameCtr
                and     #%00000001
                sta     ST_FrameSwitch

;
; Ending -------------------------------------------------------------
;
                ; exit as soon as possible when going to ending

ST_Ending:      SUBROUTINE
                lda     TempStage_R
                cmp     #3
                bne     .end
                ldx     Stage4RivetMask_R
                inx
                bne     .end
                jmp     Bank6_Ending
.end

;
; Stage --------------------------------------------------------------
;

ST_StageHandler:SUBROUTINE

                ; call stage handler

                sty     ST_AudioCode            ; Y = 0
                jsr     Bank0_StageHandler

                sta     HMCLR                   ; clear M0 HPos

                ; ----------------------------------------------------

ST_StageSpeed:  SUBROUTINE

                ; determine level speed

                lda     ST_FrameSwitch
                beq     .end

                ldx     TempLevel_R
                cpx     #9                      ; Level >= 9 ?
                bcs     .write                  ; yes -> never decrease timer

                lda     TempStage_R
                bne     .skip
                dex                             ; speed = L-1 in Stage 1
.skip           txa

                ; count down and reset stage timer

.count          ldx     StageTimer_R
                dex
                bpl     .write
                tax
.write          stx     StageTimer_W
.end

;
; Mario --------------------------------------------------------------
;

ST_Mario:       SUBROUTINE

                ; extended death check

                lda     MarioState_R
#if CHEAT_INVINCIBILITY == 0
                cmp     #MARIO_STATE_DEAD
#else            ;
                lda     #0
#endif           ;
                beq     .elevator

                lda     TempStage_R
                cmp     #2
                bne     .elevator
                lda     MarioFloor_R
                ora     MarioVPos_R
                bne     .elevator
                jsr     ST_MarioDeadSet

                ; elevator drag Mario

.elevator       lda     FrameCtr
                and     StageSpeedMask_R
                bne     .state
                lda     Stage3Elevator_R
                beq     .state
                lda     EnemyTemp_R+2
                jsr     ST_MarioMoveV

                ; update Mario's state

.state          jsr     ST_MarioState

                ; apply audio code

                lda     ST_AudioCode
                beq     .end

                cmp     #AUDIO_SOUND_INIT                               ; always init music
                bcc     .audio
                cmp     #SOUND_INIT_HI_PRIORITY                         ; always init high priority sounds
                bcs     .audio
                ldx     FXType+1                                        ; don't interrupt
                cpx     #(SOUND_INIT_HI_PRIORITY & AUDIO_ARG_MASK)
                bcc     .audio
                cpx     #(SOUND_INIT_HI_PRIORITY & AUDIO_ARG_MASK)+7
                bcc     .end
.audio          jsr     Bank5_DriveAudio
.end

;
; Kong ---------------------------------------------------------------
;

ST_Kong:        SUBROUTINE

                ; count down Kong anim timer if Mario is not dead

                lda     MarioState_R
                cmp     #MARIO_STATE_DEAD
                beq     .end
                ldy     KongAnimTimer_R
                dey
                bpl     .write

                ; advance Kong state

                ldx     KongAnimState_R
                inx
                lda     ST_KongGrfxTab-1,x
                bpl     .skip
                lda     ST_KongMoveTab-1,x
                tax
.skip           stx     KongAnimState_W
                ldy     ST_KongTimeTab-1,x
.write          sty     KongAnimTimer_W
.end

;
; Finisehd -----------------------------------------------------------
;

ST_Finished:    SUBROUTINE

                ; Mario dead or winning ?

                lda     MarioState_R
                cmp     #MARIO_STATE_DEAD       ; Mario still active ?
                bcc     .end                    ; yes -> go on
                sta     EventCtr                ; stop telescope ladders in stage 2 (A % 4 != 0)
                bne     ST_MarioClear           ; A = MARIO_STATE_WIN -> skip

                ; Mario is dead

                lda     MarioTimer_R
                bne     ST_MarioClear           ; still spinning

                lda     Death_Lives
                ora     #%10000000
                sta     Death_Lives

                jmp     Bank7_Intermission
.end
                ; ----------------------------------------------------

ST_Bonus:       SUBROUTINE

                ; decrease bonus timer

                dec     EventCtr
                bne     .end
                lda     BonusTimer_R
                sta     EventCtr
                lda     Bonus
                beq     ST_MarioDeath

                ; decrease bonus

                sed
                sec
#if CHEAT_FREEZE_BONUS_TIMER == 0
                sbc     #$01
#else            ;
                sbc     #$00
#endif           ;
                sta     Bonus
                cld
                cmp     #$09
                bne     .end
                lda     HammerState_R
                cmp     #HAMMER_STATE_OFF+1
                bcs     .end
                lda     #MUSIC_INIT_TIME_OUT
                jsr     Bank5_DriveAudio
.end
                ; ----------------------------------------------------

ST_CollEnemy:   SUBROUTINE

                ; hardware collision check

                bit     CXPPMM
                bpl     ST_MarioClear

                ; relaxed collision check

.climb          lda     MarioState_R            ; no relaxation in CLIMB state
                cmp     #MARIO_STATE_CLIMB
                bne     .item
                lda     MarioHPos_R             ; except in stage 3 on ladder leading to jacks
                cmp     #102
                beq     ST_MarioClear
                bne     ST_MarioDeath

.item           lda     MarioFloor_R            ; check oil and Pauline's items on first floor
                bne     .overlap
                ldx     ItemIndex_R             ; oil barrel kills
                cpx     #1*2
                beq     ST_MarioDeath
                sta     ItemIndex_W             ; item picked up

.overlap        lda     EnemyCollision_R        ; horizontal overlap ?
                beq     ST_MarioClear

.jump           cmp     #ENEMY_NOJUMP_START     ; enemy type Mario can jump over ?
                bcs     ST_MarioDeath
                lda     MarioVPos_R
                and     #%00011111
                cmp     #ST_MARIO_V_JUMPOVER    ; jumping high enough ?
                bcs     ST_MarioClear

                ; ----------------------------------------------------

ST_MarioDeath:  SUBROUTINE
#if CHEAT_INVINCIBILITY == 0
                jsr     ST_MarioDeadSet
#else            ;
                jmp     ST_MarioClear
#endif           ;
                lda     #AUDIO_CLEAR
                sta     AUDV0
                sta     AUDV1
                jsr     Bank5_DriveAudio

                ; ----------------------------------------------------

ST_MarioClear:  SUBROUTINE
                lda     #0
                ldx     #28-1
.loop           sta     MarioGRPBuffer_W,x
                sta     MarioGRPBuffer_W+28,x   ; unrolled
                dex
                bpl     .loop
                jsr     ST_MarioClrPtrs

                ; ----------------------------------------------------

ST_MarioWinning:SUBROUTINE
                lda     MarioState_R
                cmp     #MARIO_STATE_WIN
                bne     .end

                ; init Mario win music

.win            ldx     KongAnimState_R
                cpx     #KONG_ANIM_CLIMB
                bcs     .bonus

                lda     #MUSIC_INIT_MARIO_WIN
                jsr     Bank5_DriveAudio
                lda     #104-1
                sta     KongAnimTimer_W
                ldx     #KONG_ANIM_CLIMB
                stx     KongAnimState_W

                lda     #ST_SCORE_LADDER
                ldy     MarioHPos_R
                cpy     #58                     ; Mario took left ladder ?
                beq     .add                    ; yes -> add score

                ; add remaining bonus

.bonus          cpx     #KONG_ANIM_CLIMB+9
                bne     .exit

                ldy     KongAnimTimer_R
                cpy     #110-62
                bne     .end

                lda     Bonus
.add            sed
                clc
                adc     Score+1
                sta     Score+1
                lda     #0
                adc     Score+0
                sta     Score+0
                cld
                bcc     .exit
                lda     #$99
                sta     Score+0
                sta     Score+1
                sta     Score+2

                ; advance to next stage

.exit           cpx     #KONG_ANIM_CLIMB+10
                bcc     .end
                jmp     Bank7_Intermission
.end

;
; Audio --------------------------------------------------------------
;

ST_Audio:       SUBROUTINE

                lda     MarioState_R

                ; Mario has reached top ?

.win            cmp     #MARIO_STATE_WIN
                bne     .dead

                ldx     KongAnimState_R
                cpx     #KONG_ANIM_CLIMB+1
                bcc     .music

                ; clear sound timer variables and init grin

                cpx     #KONG_ANIM_CLIMB+2
                bcs     .sound

                lda     #AUDIO_CLEAR
                jsr     Bank5_DriveAudio
                lda     #SOUND_INIT_KONG_GRIN
                bne     .drive

                ; Mario dead ?

.dead           cmp     #MARIO_STATE_DEAD
                bne     .play

                lda     MarioTimer_R
                cmp     #4*30
                bcc     .music

                ; play music and sound

.play           lda     FXType+1
                cmp     #(SOUND_INIT_MARIO_FALL & AUDIO_ARG_MASK)
                bne     .mplay0
                lda     #0                      ; turn off music when Mario falls
                sta     AUDV0
                beq     .splay1

                ; drive audio

.sound          lda     #SOUND_PLAY
                bne     .drive
.music          lda     #MUSIC_PLAY
                bne     .drive

.mplay0         lda     #MUSIC_PLAY_0
                jsr     Bank5_DriveAudio
.splay1         lda     #SOUND_PLAY_1
.drive          jsr     Bank5_DriveAudio

; --------------------------------------------------------------------

ST_OverscanEnd: lda     INTIM
                bne     ST_OverscanEnd
                jmp     ST_VSync

; --------------------------------------------------------------------
;       ST Subroutines
; --------------------------------------------------------------------

;
; Mario state machinery ----------------------------------------------
;

ST_MarioState:  SUBROUTINE

                ; state function dispatcher

                lda     ST_FrameSwitch
                beq     .switch
                rts                             ; save one cycle

.switch         ldx     MarioState_R
                lda     ST_MarioState_H,x
                pha
                lda     ST_MarioState_L,x
                pha
ST_MarioEnd:    rts

                ;
                ; START state ----------------------------------------
                ;

                ; initial state, don't need MarioStartSet

ST_MarioStart:  SUBROUTINE

                ; wait before starting

                ldx     MarioTimer_R
                cpx     #ST_MARIO_START_DELAY
                bcs     .life
                inx
                stx     MarioTimer_W
                stx     ButtonDebounce_W        ; clear bit 7 (prevent jump on startup)
                rts

                ; take one life

.life           lda     Death_Lives
                bpl     ST_MarioWalkSet
#if CHEAT_INFINITE_LIVES == 0
                sbc     #DEATH_MASK+1           ; C = 1
#else            ;
                sbc     #DEATH_MASK
#endif           ;
                sta     Death_Lives

                ;
                ; WALK state -----------------------------------------
                ;

ST_MarioWalkSet:SUBROUTINE

                ; init state

                lda     #MARIO_STATE_WALK
                sta     MarioState_W

ST_MarioWalk:   SUBROUTINE

                ; don't move if Mario go hit

                lda     ST_AudioCode            ; hit audio code was set in stage handler ?
                cmp     #SOUND_INIT_MARIO_HIT
                beq     ST_MarioEnd
                lda     FXType+1                ; hit audio effect is playing ?
                cmp     #(SOUND_INIT_MARIO_HIT & AUDIO_ARG_MASK)
                beq     ST_MarioEnd

                ; check if Mario is dragged by the elevator

                lda     #-3                     ; Mario Y-speed if falling
                jsr     ST_CheckElevator        ; standing on elevator ?
                bne     .trigger                ; yes -> skip gap test

                ; check if falling

                lda     MarioVPos_R
                and     #%00011111              ; Mario in the air ?
                bne     ST_MarioFallSet

                jsr     ST_CheckGap             ; Mario over a gap ?
                bcc     ST_MarioFallSet

                ; check jump trigger

.trigger        lda     HammerState_R
                cmp     #HAMMER_STATE_OFF+1     ; Hammer in use ?
                bcs     .horizontal             ; yes -> cannot jump

                bit     ButtonDebounce_R        ; button already released ?
                bmi     .press                  ; yes -> check if pressed
.release        lda     ST_ShadowINPT           ; check if button released
                bpl     .horizontal
                sta     ButtonDebounce_W
.press          lda     ST_ShadowINPT           ; check if button pressed
                bpl     ST_MarioJumpSet

                ; horizontal movement

.horizontal     lda     ST_ShadowSWCHA
                and     #%11000000
                eor     #%11000000              ; stick pushed left or right ?
                beq     .vertical               ; no -> go to vertical handling
                lda     MarioAnimState_R
                cmp     #10
                bcc     .switch
                lda     #2                      ; reset walk animation
                sta     MarioAnimTimer_W
                sta     MarioAnimState_W

.switch         bit     ST_ShadowSWCHA          ; switch left/right handling
                bpl     .right
                bvs     .vertical               ; can this still happen ?

.left           ldx     #%00000000              ; clear REFP (don't need to store SWCHA)
                stx     MarioDir_W
                jsr     ST_MarioLeft            ; move Mario left
                bne     .anim                   ; are we at the left border (Z = 0) ?
                rts                             ; yes -> no animation

.right          ldx     #%00001000              ; set REFP (don't need to store SWCHA)
                stx     MarioDir_W
                jsr     ST_MarioRight           ; move Mario right
                bne     .anim                   ; are we at the right border (Z = 0) ?
                rts                             ; yes -> no animation

                ; vertical movement

.vertical       lda     HammerState_R
                cmp     #HAMMER_STATE_OFF+1     ; Hammer in use ?
                bcs     .end                    ; yes -> cannot climb

                lda     ST_ShadowSWCHA
                and     #%00110000
                eor     #%00110000              ; stick pushed up or down ?
                beq     .end
                jmp     ST_MarioClimbSet        ; check if we are close to a ladder (switch to climb state)

                ; walk animation and sound

.anim           ldx     MarioAnimTimer_R
                dex
                stx     MarioAnimTimer_W

                bne     .end                    ; only trigger sound if animation phase has changed
                cmp     #2                      ; A = animation phase
                bne     .end                    ; only trigger sound once each cycle
                STOREBYTE SOUND_INIT_MARIO_WALK, ST_AudioCode

.end            rts

                ;
                ; JUMP state -----------------------------------------
                ;

                ; reuse jump state as fall state

ST_MarioFallSet:SUBROUTINE

                ; init state

                lda     #MARIO_STATE_JUMP
                sta     MarioState_W

                STOREBYTE 0, MarioTimer_W       ; no jump animation

                ; ensure a straight fall when running into gaps

                lda     MarioDir_R
                ora     #%11000000              ; set as SWCHA left/right bits
                sta     MarioDir_W
                bne     ST_MarioJumpH           ; jump to jump state

                ; regular jump state

ST_MarioJumpSet:SUBROUTINE

                ; init state

                lda     #MARIO_STATE_JUMP
                sta     MarioState_W

                STOREBYTE 20, MarioTimer_W
                lsr
                sta           MarioAnimState_W  ; A = 10
                STOREBYTE  0, MarioAnimTimer_W

                ; clear elevator

                sta     Stage3Elevator_W        ; A = 0

                ; remember jump direction from SWCHA

                lda     MarioDir_R
                and     #%00111111
                sta     MarioDir_W
                lda     ST_ShadowSWCHA
                and     #%11000000
                ora     MarioDir_R
                sta     MarioDir_W

                STOREBYTE SOUND_INIT_MARIO_JUMP, ST_AudioCode

                bne     ST_MarioJumpH           ; always move at t=20 (ensure we jump 16 pixels far)

ST_MarioJump:   SUBROUTINE

                ; shorten jump

                lda     MarioTimer_R            ; don't move horizontally at t=16, 12, 8, 4
                and     #%00000011
                beq     .vertical

                ; horizontal movement
ST_MarioJumpH:
.horizontal     bit     MarioDir_R              ; switch based on saved SWCHA flags
                bpl     .right
                bvs     .vertical

.left           jsr     ST_MarioLeft            ; move left and check border (Z = 0)
                bne     .vertical               ; no -> skip bounce
                lda     MarioDir_R
                eor     #%11000000              ; flip move direction
                ora     #%00001000              ; Mario faces to the right
                sta     MarioDir_W
                bne     .vertical

.right          jsr     ST_MarioRight           ; move right and check border (Z = 0)
                bne     .vertical               ; no -> skip bounce
                lda     MarioDir_R
                eor     #%11000000              ; flip move direction
                and     #%11110111              ; Mario faces to the left
                sta     MarioDir_W

                ; vertical movement
ST_MarioJumpV:
.vertical       ldx     MarioTimer_R
                dex
                beq     .zero                   ; X = 0 : jump animation ended
                bmi     .minus                  ; X < 0 : contains fall speed (fixed point)
                bpl     .plus                   ; X > 0 : contains animation timer

.zero           ldx     #(-3<<2)+3              ; fall speed after jump
.minus          cpx     #(-4<<2)+2              ; fall speed reaches threshold ?
                bne     .shift
                lda     #SOUND_INIT_MARIO_FALL  ; yes -> play falling sound
                sta     ST_AudioCode
.shift          txa                             ; compute pixel move delta from fall speed
                lsr
                lsr
                ora     #%11000000              ; arithmetic shift (keep sign)
                bmi     .switch
.plus           lda     ST_MarioJumpTab,x       ; get pixel move delta from table

.switch         stx     MarioTimer_W
                pha                             ; save delta
                bmi     .down

                ; Mario moving up

.up             jsr     ST_CheckElevator        ; check for horizontal collision
                pla                             ; restore delta
.move           jmp     ST_MarioMoveV           ; continue vertical movement

                ; check if falling through gap

.gap            jsr     ST_CheckGap             ; C = 0 if within gap
                pla                             ; restore delta
                bcc     .move                   ; within gap -> keep falling
                jsr     ST_MarioFallV           ; platform hit ?
                beq     ST_MarioLandSet         ; yes -> go to land state
                rts                             ; no -> keep state

                ; Mario moving down

.down           jsr     ST_CheckElevator        ; landing on elevator ?
                beq     .gap                    ; no -> check gaps
                jsr     ST_MarioMoveV           ; drop on elevator (A = distance)
                pla                             ; restore stack

                ;
                ; LAND state -----------------------------------------
                ;

ST_MarioLandSet:SUBROUTINE

                ; check if Mario fell to his death

                ldx     MarioTimer_R
                cpx     #(-4<<2)+2+1            ; landing speed greater than threshold ?
                bcs     .init
                cpx     #20                     ; ignore if jump animation is on (can hit elevator during jump)
                bcc     .init

                STOREBYTE 12, MarioAnimState_W  ; set land pose
                STOREBYTE  0, MarioAnimTimer_W  ; show immediately next frame

                jmp     ST_MarioDeadSet         ; directly go to dead state

                ; init state

.init           lda     #MARIO_STATE_LAND
                sta     MarioState_W

                STOREBYTE 4, MarioTimer_W       ; wait time before Mario can move on

                rts                             ; keep jump pose for one frame

ST_MarioLand:   SUBROUTINE

                ; wait some time then continue with walk state

                ldx     MarioTimer_R
                dex
                stx     MarioTimer_W
                bne     .land
                stx     ButtonDebounce_W        ; avoid continuous jumping (X = 0)
                jmp     ST_MarioWalkSet

                ; landing animation

.land           cpx     #3                      ; init land animation here
                bne     .anim

                STOREBYTE 12, MarioAnimState_W  ; set land pose
                STOREBYTE  1, MarioAnimTimer_W  ; will be decreased to 0 below

.anim           ldx     MarioAnimTimer_R
                dex
                stx     MarioAnimTimer_W
                rts

                ;
                ; CLIMB state ----------------------------------------
                ;

ST_MarioClimbSet:SUBROUTINE

                ; init animation

                lda     ST_ShadowSWCHA
                and     #%00010000              ; stick pushed up ?
                beq     .bottom                 ; yes -> start from bottom

.top            jsr     ST_CheckLadder          ; ladder going down (S = 1) ?
                bpl     .end                    ; no -> keep old state

                STOREBYTE 36, MarioAnimState_W
                STOREBYTE  2, MarioAnimTimer_W
                STOREBYTE 16, MarioTimer_W
                bne     .state

.bottom         jsr     ST_CheckLadder          ; ladder going up (V = 1) ?
                bvc     .end                    ; no -> keep old state

                STOREBYTE 18, MarioAnimState_W
                STOREBYTE  1, MarioAnimTimer_W
                STOREBYTE  0, MarioTimer_W

                ; init remaining state

.state          lda     Temp
                and     #LADDER_BROKEN
                ora     MarioTimer_R            ; store broken ladder flag in unused timer bits
                sta     MarioTimer_W
                jsr     ST_MarioAlign           ; align Mario with ladder

                lda     #MARIO_STATE_CLIMB
                sta     MarioState_W

                bne     .climb                  ; climb first step

                ; leave or advance state

ST_MarioClimb:  lda     FrameCtr
                and     #%00000011              ; advance every four frames
                bne     .end

                lda     MarioTimer_R
                and     #~LADDER_BROKEN
                tay
                dey                             ; leave state if timer reaches 0 or 16
                cpy     #15
                bcc     .debounce
                lda     MarioDir_R
                and     #%11110111              ; face left in case we jump after geting off a ladder
                sta     MarioDir_W
                jmp     ST_MarioWalkSet

                ; relax strict button debounce (can be pressed shortly before reaching top/bottom)

.debounce       dey                             ; check if Y = 0, 13 or 14
                cpy     #ST_MARIO_EARLY_JUMP
                bcs     .climb                  ; yes -> ignore button
                lda     ST_ShadowINPT           ; no -> record if button released
                sta     ButtonDebounce_W

                ; trigger climb sound

.climb          lda     MarioVPos_R
                and     #8-1                    ; every two ladder steps (2*4 lines)
                cmp     #4
                bne     .switch
                STOREBYTE SOUND_INIT_MARIO_CLIMB, ST_AudioCode

                ; vertical movement

.switch         lda     ST_ShadowSWCHA
                and     #%00010000
                beq     .up
                lda     ST_ShadowSWCHA
                and     #%00100000
                beq     .down
.off            STOREBYTE 0, ST_AudioCode       ; don't replay sound if not moving
.end            rts

.up             ldx     MarioTimer_R
                cpx     #LADDER_BROKEN+7        ; limit on broken ladder
                beq     .end
                cpx     Stage2LadderLimit_R     ; limit on telescope ladder
                bcs     .off
                inx
                stx     MarioTimer_W
                ldx     MarioAnimTimer_R
                dex
                stx     MarioAnimTimer_W
                lda     #2
                jmp     ST_MarioMoveV

.down           ldx     MarioTimer_R
                cpx     #LADDER_BROKEN+8        ; limit on broken ladder
                beq     .end
                dex
                stx     MarioTimer_W
                ldx     MarioAnimTimer_R
                inx
                stx     MarioAnimTimer_W
                lda     #-2
                jmp     ST_MarioMoveV

                ;
                ; DEAD state -----------------------------------------
                ;

ST_MarioDeadSet:SUBROUTINE

                ; init state

                lda     #MARIO_STATE_DEAD
                sta     MarioState_W

                STOREBYTE (4+1)*30, MarioTimer_W
                STOREBYTE        0, Stage3Elevator_W

                rts

ST_MarioDead:   SUBROUTINE

                ; start animation after 1 second

                lda     MarioTimer_R
                cmp     #4*30
                bne     .skip

                STOREBYTE MUSIC_INIT_MARIO_DEAD, ST_AudioCode
                STOREBYTE                    36, MarioAnimState_W
                STOREBYTE                     0, MarioAnimTimer_W

                ; death animation

.skip           bcs     .next
                ldx     MarioAnimTimer_R
                dex
                stx     MarioAnimTimer_W
.next           ldx     MarioTimer_R
                dex
                stx     MarioTimer_W

                ;
                ; WIN state ------------------------------------------
                ;

ST_MarioWin:    rts                             ; nothing to do anymore

;
; Mario helper functions ---------------------------------------------
;
                ; check border and move Mario left

ST_MarioLeft:   SUBROUTINE
                ldx     MarioHPos_R
                cpx     #ST_MARIO_H_MIN
                beq     .end
                dex
                stx     MarioHPos_W
.end            rts

                ; check border and move Mario right

ST_MarioRight:  SUBROUTINE
                ldx     MarioHPos_R
                cpx     #ST_MARIO_H_MAX
                beq     .end
                inx
                stx     MarioHPos_W
.end            rts

; --------------------------------------------------------------------

                ; incrementally let Mario fall on a platform (avoid falling through)

ST_MarioFallV:  SUBROUTINE
                tax                             ; fall speed (A < 0)
.loop           ldy     MarioYPos_R
                dey
                sty     MarioYPos_W
                ldy     MarioVPos_R
                dey
                sty     MarioVPos_W
                tya
                and     #%00011111              ; are we at the bottom of a corriror ?
                beq     ST_MarioMoveV           ; yes -> set return values (A = 0)
                inx                             ; no -> move Mario one more line (if X < 0)
                bne     .loop
                txa                             ; A := 0

                ; move Mario up/down

ST_MarioMoveV:  tax
                clc
                adc     MarioYPos_R
                sta     MarioYPos_W
                txa
                clc
                adc     MarioVPos_R
                sta     MarioVPos_W
                and     #%11100000
                lsr                             ; divide by 8
                lsr
                lsr
                adc     ST_Floor                ; factor scrolling in (C = 0)
                sta     MarioFloor_W            ; floor x4
                lda     MarioVPos_R
                and     #%00011111              ; A := Y-position within corridor
                rts

; --------------------------------------------------------------------

                ; align Mario with ladder

ST_MarioAlign:  lda     MarioHPos_R
                and     #256-4                  ; - %4
                ora     #2                      ; + 2
                sta     MarioHPos_W
                rts

; --------------------------------------------------------------------

                ; set all GRP and COL pointers to empty space

ST_MarioClrPtrs:SUBROUTINE
                ldx     #(8*2)-1
                ldy     #<EmptyGRP
                lda     #>EmptyGRP
.loop           sta     ST_MarioGRPPtr,x
                sta     ST_MarioCOLPtr,x
                dex
                sty     ST_MarioGRPPtr,x
                sty     ST_MarioCOLPtr,x
                dex
                bpl     .loop
                rts

;
; Stage layout query functions ---------------------------------------
;

ST_CheckLadder: SUBROUTINE

                ; adjust Mario position

                lda     MarioHPos_R
                sec
                sbc     #16-4
                lsr
                lsr
                sta     Temp

                ; get laddder plan

                ldx     TempStage_R
                lda     MarioFloor_R
                lsr
                lsr
                adc     MarioFloor_R
                adc     ST_LadderPlanOffsets,x
                tax

                ; check ladders

                ldy     #10
.loop           lda     ST_LadderPlan-5,x
                and     #LADDER_POS_MASK
                cmp     Temp
                beq     .write
                inx
                dey
                bne     .loop
                clv
                rts

                ; determine ladder type

.write          lda     ST_LadderPlan-5,x
                cpy     #6
                bcc     .up
.down           cmp     ST_LadderPlan,x
                php
                ora     #LADDER_DOWN
                plp
                bne     .end
.up             ora     #LADDER_UP
.end            sta     Temp
                bit     Temp
                rts

; --------------------------------------------------------------------

ST_CheckGap:    SUBROUTINE

                ; adjust Mario position

                lda     MarioHPos_R
                sec
                sbc     #16-4
                sta     Temp

                ; get gap plan

                ldx     TempStage_R
                lda     MarioFloor_R
                beq     .end                    ; no gaps on floor 0 (C = 1)
                adc     ST_GapStartPlanOffsets,x
                tax

                ; check gaps

                ldy     #4-1
.loop           lda     Temp
                ora     ST_GapMask,y
                sec
                sbc     ST_GapStartPlan,x
                cmp     ST_GapWidthPlan,x
                bcc     .end
                inx
                dey
                bpl     .loop
.end            rts

 ; --------------------------------------------------------------------

ST_CheckElevator:SUBROUTINE

                ; elevator stage ?

                ldy     #-1                     ; prepare boolean result
                tax                             ; save Mario Y-speed
                lda     TempStage_R
                cmp     #2
                bne     .miss

                ; check horizontal overlap

                lda     MarioHPos_R
                sbc     #12-5                   ; C = 1
                sbc     EnemyHPos_R+2
                cmp     #10+1
                bcs     .miss

                ; Mario absolute Y-position

                lda     MarioVPos_R
                and     #%00011111
                sta     Temp
                lda     MarioFloor_R
                asl
                asl
                asl
                adc     Temp
                sta     Temp

                ; elevator absolute Y-position

                lda     EnemyFloor_R+2
                asl
                asl
                asl
                adc     EnemyVPos_R+2
                adc     #8+1                    ; adjust for C = 0

                ; check for exact hit from above

                sbc     Temp                    ; distance := elevator Y - Mario Y
                sta     Temp
                beq     .hit                    ; distance = 0 -> hit

                ; check for vertical overlap and block

                cmp     #8+MARIO_HEIGHT         ; vertical overlap ?
                bcs     .catch                  ; no -> check if Mario is above elevator
                lda     MarioDir_R
                ora     #%11000000              ; block jump
                sta     MarioDir_W
                bne     .miss                   ; no hit

                ; elevator catches Mario

.catch          dex                             ; X = speed, Temp = distance
                bpl     .miss                   ; avoid comparison if speed is positive
                cpx     Temp                    ; speed-1 >= distance (both negative)
                bcs     .miss                   ; yes -> miss

                ; result = hit ? 1 : 0

.hit            iny
.miss           iny
                sty     Stage3Elevator_W
                rts

;
; Enemy functions ----------------------------------------------------
;

ST_EnemySetGRP: SUBROUTINE

                ; set GRP pointers

                clc
                lda     ST_PtrOffset
                adc     ST_GRPPtrTmp
                sta     ST_EnemyGRPPtr,x
                lda     #0
                adc     ST_GRPPtrTmp+1
                sta     ST_EnemyGRPPtr+1,x
                rts

 ; --------------------------------------------------------------------

ST_EnemySetCA:  SUBROUTINE

                ; set COL pointers

                txa
                lsr
                tax
                lda     ST_PtrOffset
                adc     ST_COLPtrTmp
                sta     ST_EnemyCOLPtr,x
                lda     #0
                adc     ST_COLPtrTmp+1
                sta     ST_EnemyCOLPtr+1,x

                ; set attributes

                txa
                lsr
                tax
                lda     ST_HMPosTmp
                sta     ST_HMPos,x
                lda     ST_HMDelayTmp
                sta     ST_HMDelay,x
                lda     ST_RefNuSizTmp
                sta     ST_RefNuSiz,x
                rts

;
; Misc functions -----------------------------------------------------
;

ST_DrawKong:    SUBROUTINE

                ; Kong standing

.stand          ldx     KongAnimState_R
                cpx     #KONG_ANIM_CLIMB+1
                bcs     .climb

                sty     LineCtr
                ldy     ST_KongGrfxTab-1,x
                cpy     #B5_KONG_THROW
                bne     .b6

.b5             jmp     Bank5_DrawKong
.b6             jmp     Bank6_DrawKong

                ; Kong climbing

.climb          ldy     ST_KongGrfxTab-1,x
                lda     ST_KongSizeTab-1,x
                sta     LineCtr

                ; Kong only

.kong0          cpx     #KONG_ANIM_CLIMB+2
                bcs     .kong1
                jmp     Bank1_DrawKong

                ; Kong plus ladder

.kong1          cpx     #KONG_ANIM_CLIMB+9
                bcc     .kong2
                WAIT_LINES 5
                beq     .ladder
.kong2          jsr     Bank1_DrawKong

                ; draw ladder

.ladder         ldx     KongAnimState_R
                ldy     ST_KongMoveTab-1,x
.loop           lda     #%00000000
                sta     GRP0
                sta     VDELP1
                ldx     #%10000001
                stx     NUSIZ1
                tya
                and     #3
                bne     .skip
                ldx     #%11111111
.skip           stx     GRP1
                sta     WSYNC
                dey
                cpy     #1
                bne     .loop
                sta     WSYNC
                rts

; --------------------------------------------------------------------

ST_PrecalcHPos: SUBROUTINE

                ; kernel adjustment

                txa
                clc
                adc     #3

                ; calculate delay (divide by 15)

                sta     Temp
                lsr
                adc     #4
                lsr
                lsr
                lsr
                adc     Temp
                ror
                lsr
                lsr
                lsr
                sta     ST_HMDelayTmp           ; A = [X/15]

                ; calculate HMP (remainder)

                asl
                asl
                asl
                asl                             ; A := [X/15] * 16
                sbc     ST_HMDelayTmp           ; A := [X/15] * 15 - 1 ; C = 0
                sbc     Temp                    ; A := -remainder - 1  ; C = 1
                adc     #7                      ; A := -(remainder-6)  ; C = 0 (map remainder to +6,...,-8)
                asl
                asl
                asl
                asl
                ora     ST_HMDelayTmp
                sta     ST_HMPosTmp
                rts


; ********************************************************************
;
;       Data Section
;
; ********************************************************************

; --------------------------------------------------------------------
;       Macros
; --------------------------------------------------------------------

        MAC MAKE_MARIO_LABEL_SEQUENCE

{1}{2}_GRP_0 = {3}_{4}_GRP_0
{1}{2}_GRP_1 = {3}_{4}_GRP_1
{1}{2}_COL_0 = {3}_{4}_COL_0
{1}{2}_COL_1 = {3}_{4}_COL_1

        ENDM

        MAC MAKE_ENEMY_LABEL_SEQUENCE

{1}{2}_GRP_0  = {3}_{4}_GRP_0
{1}{2}_GRP_1  = {3}_{4}_GRP_1
{1}{2}_COL_0  = {3}_{4}_COL_0
{1}{2}_COL_1  = {3}_{4}_COL_1
{1}{2}_HEIGHT = {3}_{5}_HEIGHT

        ENDM

        MAC MAKE_ITEM_LABEL_SEQUENCE

{1}{2}_GRP_0  = {3}_{4}_GRP_0
{1}{2}_GRP_1  = {3}_{4}_GRP_1
{1}{2}_COL_0  = {3}_{4}_COL_0
{1}{2}_COL_1  = {3}_{4}_COL_1

        ENDM

        MAC MAKE_MARIO_LOBYTE_TABLE

                BYTE <({1}00{2}_0-{3}),<({1}00{2}_1-{3})
                BYTE <({1}01{2}_0-{3}),<({1}01{2}_1-{3})
                BYTE <({1}02{2}_0-{3}),<({1}02{2}_1-{3})
                BYTE <({1}03{2}_0-{3}),<({1}03{2}_1-{3})
                BYTE <({1}04{2}_0-{3}),<({1}04{2}_1-{3})
                BYTE <({1}05{2}_0-{3}),<({1}05{2}_1-{3})
                BYTE <({1}06{2}_0-{3}),<({1}06{2}_1-{3})
                BYTE <({1}07{2}_0-{3}),<({1}07{2}_1-{3})
                BYTE <({1}08{2}_0-{3}),<({1}08{2}_1-{3})
                BYTE <({1}09{2}_0-{3}),<({1}09{2}_1-{3})
                BYTE <({1}10{2}_0-{3}),<({1}10{2}_1-{3})
                BYTE <({1}11{2}_0-{3}),<({1}11{2}_1-{3})
                BYTE <({1}12{2}_0-{3}),<({1}12{2}_1-{3})
                BYTE <({1}13{2}_0-{3}),<({1}13{2}_1-{3})
                BYTE <({1}14{2}_0-{3}),<({1}14{2}_1-{3})
                BYTE <({1}15{2}_0-{3}),<({1}15{2}_1-{3})
                BYTE <({1}16{2}_0-{3}),<({1}16{2}_1-{3})
                BYTE <({1}17{2}_0-{3}),<({1}17{2}_1-{3})
                BYTE <({1}18{2}_0-{3}),<({1}18{2}_1-{3})
        ENDM

        MAC MAKE_MARIO_HIBYTE_TABLE

                BYTE >({1}00{2}_0-{3}),>({1}00{2}_1-{3})
                BYTE >({1}01{2}_0-{3}),>({1}01{2}_1-{3})
                BYTE >({1}02{2}_0-{3}),>({1}02{2}_1-{3})
                BYTE >({1}03{2}_0-{3}),>({1}03{2}_1-{3})
                BYTE >({1}04{2}_0-{3}),>({1}04{2}_1-{3})
                BYTE >({1}05{2}_0-{3}),>({1}05{2}_1-{3})
                BYTE >({1}06{2}_0-{3}),>({1}06{2}_1-{3})
                BYTE >({1}07{2}_0-{3}),>({1}07{2}_1-{3})
                BYTE >({1}08{2}_0-{3}),>({1}08{2}_1-{3})
                BYTE >({1}09{2}_0-{3}),>({1}09{2}_1-{3})
                BYTE >({1}10{2}_0-{3}),>({1}10{2}_1-{3})
                BYTE >({1}11{2}_0-{3}),>({1}11{2}_1-{3})
                BYTE >({1}12{2}_0-{3}),>({1}12{2}_1-{3})
                BYTE >({1}13{2}_0-{3}),>({1}13{2}_1-{3})
                BYTE >({1}14{2}_0-{3}),>({1}14{2}_1-{3})
                BYTE >({1}15{2}_0-{3}),>({1}15{2}_1-{3})
                BYTE >({1}16{2}_0-{3}),>({1}16{2}_1-{3})
                BYTE >({1}17{2}_0-{3}),>({1}17{2}_1-{3})
                BYTE >({1}18{2}_0-{3}),>({1}18{2}_1-{3})
        ENDM

        MAC MAKE_ENEMY_LOBYTE_TABLE

                BYTE <({1}00{2}_0-{3}),<({1}00{2}_1-{3})
                BYTE <({1}01{2}_0-{3}),<({1}01{2}_1-{3})
                BYTE <({1}02{2}_0-{3}),<({1}02{2}_1-{3})
                BYTE <({1}03{2}_0-{3}),<({1}03{2}_1-{3})
                BYTE <({1}04{2}_0-{3}),<({1}04{2}_1-{3})
                BYTE <({1}05{2}_0-{3}),<({1}05{2}_1-{3})
                BYTE <({1}06{2}_0-{3}),<({1}06{2}_1-{3})
                BYTE <({1}07{2}_0-{3}),<({1}07{2}_1-{3})
                BYTE <({1}08{2}_0-{3}),<({1}08{2}_1-{3})
                BYTE <({1}09{2}_0-{3}),<({1}09{2}_1-{3})
                BYTE <({1}10{2}_0-{3}),<({1}10{2}_1-{3})
                BYTE <({1}11{2}_0-{3}),<({1}11{2}_1-{3})
                BYTE <({1}12{2}_0-{3}),<({1}12{2}_1-{3})
                BYTE <({1}13{2}_0-{3}),<({1}13{2}_1-{3})
                BYTE <({1}14{2}_0-{3}),<({1}14{2}_1-{3})
                BYTE <({1}15{2}_0-{3}),<({1}15{2}_1-{3})

        ENDM

        MAC MAKE_ENEMY_HIBYTE_TABLE

                BYTE >({1}00{2}_0-{3}),>({1}00{2}_1-{3})
                BYTE >({1}01{2}_0-{3}),>({1}01{2}_1-{3})
                BYTE >({1}02{2}_0-{3}),>({1}02{2}_1-{3})
                BYTE >({1}03{2}_0-{3}),>({1}03{2}_1-{3})
                BYTE >({1}04{2}_0-{3}),>({1}04{2}_1-{3})
                BYTE >({1}05{2}_0-{3}),>({1}05{2}_1-{3})
                BYTE >({1}06{2}_0-{3}),>({1}06{2}_1-{3})
                BYTE >({1}07{2}_0-{3}),>({1}07{2}_1-{3})
                BYTE >({1}08{2}_0-{3}),>({1}08{2}_1-{3})
                BYTE >({1}09{2}_0-{3}),>({1}09{2}_1-{3})
                BYTE >({1}10{2}_0-{3}),>({1}10{2}_1-{3})
                BYTE >({1}11{2}_0-{3}),>({1}11{2}_1-{3})
                BYTE >({1}12{2}_0-{3}),>({1}12{2}_1-{3})
                BYTE >({1}13{2}_0-{3}),>({1}13{2}_1-{3})
                BYTE >({1}14{2}_0-{3}),>({1}14{2}_1-{3})
                BYTE >({1}15{2}_0-{3}),>({1}15{2}_1-{3})

        ENDM

        MAC MAKE_ENEMY_CULL_TABLE

                BYTE {3}-{1}00{2}-1,{3}-{1}00{2}-1
                BYTE {3}-{1}01{2}-1,{3}-{1}01{2}-1
                BYTE {3}-{1}02{2}-1,{3}-{1}02{2}-1
                BYTE {3}-{1}03{2}-1,{3}-{1}03{2}-1
                BYTE {3}-{1}04{2}-1,{3}-{1}04{2}-1
                BYTE {3}-{1}05{2}-1,{3}-{1}05{2}-1
                BYTE {3}-{1}06{2}-1,{3}-{1}06{2}-1
                BYTE {3}-{1}07{2}-1,{3}-{1}07{2}-1
                BYTE {3}-{1}08{2}-1,{3}-{1}08{2}-1
                BYTE {3}-{1}09{2}-1,{3}-{1}09{2}-1
                BYTE {3}-{1}10{2}-1,{3}-{1}10{2}-1
                BYTE {3}-{1}11{2}-1,{3}-{1}11{2}-1
                BYTE {3}-{1}12{2}-1,{3}-{1}12{2}-1
                BYTE {3}-{1}13{2}-1,{3}-{1}13{2}-1
                BYTE {3}-{1}14{2}-1,{3}-{1}14{2}-1
                BYTE {3}-{1}15{2}-1,{3}-{1}15{2}-1

        ENDM

        MAC MAKE_ITEM_LOBYTE_TABLE

                BYTE <({1}00{2}_0-{3}),<({1}00{2}_1-{3})
                BYTE <({1}01{2}_0-{3}),<({1}01{2}_1-{3})
                BYTE <({1}02{2}_0-{3}),<({1}02{2}_1-{3})
                BYTE <({1}03{2}_0-{3}),<({1}03{2}_1-{3})

        ENDM

        MAC MAKE_STAGE_OFFSET_TABLE

                BYTE (S1_{1}-ST_{1})+{2}
                BYTE (S2_{1}-ST_{1})+{2}
                BYTE (S3_{1}-ST_{1})+{2}
                BYTE (S4_{1}-ST_{1})+{2}

        ENDM

        MAC MAKE_KONG_ANIM_STEP

KONG_ANIM_{1}_SVAL SET {2}
KONG_ANIM_{1}_MVAL SET {3}
KONG_ANIM_{1}_TVAL SET {4}
KONG_ANIM_{1}_GVAL SET {5}

        ENDM

        MAC MAKE_KONG_ANIM_TABLE

                BYTE KONG_ANIM_00_{1},KONG_ANIM_01_{1},KONG_ANIM_02_{1},KONG_ANIM_03_{1},KONG_ANIM_04_{1}
                BYTE KONG_ANIM_05_{1},KONG_ANIM_06_{1},KONG_ANIM_07_{1},KONG_ANIM_08_{1},KONG_ANIM_09_{1}
                BYTE KONG_ANIM_10_{1},KONG_ANIM_11_{1},KONG_ANIM_12_{1},KONG_ANIM_13_{1},KONG_ANIM_14_{1}
                BYTE KONG_ANIM_15_{1},KONG_ANIM_16_{1}

        ENDM

; --------------------------------------------------------------------
;       ST Tables
; --------------------------------------------------------------------

MARIO_HEIGHT = 16
FLOOR_HEIGHT = 32

BAND_08_SIZE =  8
BAND_24_SIZE = 24

; Mario state engine

MARIO_STATE_START = 0 ; START must be 0
MARIO_STATE_WALK  = 1
MARIO_STATE_JUMP  = 2
MARIO_STATE_LAND  = 3
MARIO_STATE_CLIMB = 4
MARIO_STATE_DEAD  = 5 ; must be second to last entry
MARIO_STATE_WIN   = 6 ; must be last entry

;
; Mario animation tables ---------------------------------------------
;

; Mario animation sequence tables

ANIM_CMD_MASK   = %11100000
ANIM_TIM_MASK   = %00011111

ANIM_REF0       = %10000000
ANIM_REF1       = %10100000
ANIM_FLIP       = %11000000
ANIM_JUMP       = %11100000

MARIO_WALK_0    =  0 * 2
MARIO_WALK_1    =  3 * 2
MARIO_WALK_2    =  6 * 2
MARIO_JUMP_0    =  9 * 2
MARIO_LAND_0    = 10 * 2
MARIO_CLIMB_0   = 11 * 2
MARIO_CLIMB_1   = 12 * 2
MARIO_CLIMB_2   = 13 * 2
MARIO_CLIMB_3   = 14 * 2
MARIO_DEAD_0    = 15 * 2
MARIO_DEAD_1    = 16 * 2
MARIO_DEAD_2    = 17 * 2
MARIO_DEAD_3    = 18 * 2

ST_MarioAnimTab:SUBROUTINE

                ; walk ( 0 )

                BYTE              2, MARIO_WALK_0  ;  0
                BYTE              2, MARIO_WALK_1  ;  2
                BYTE              2, MARIO_WALK_0  ;  4
                BYTE              2, MARIO_WALK_2  ;  6
                BYTE    ANIM_JUMP  , 0+2           ;  8

                ; jump ( 10 )

                BYTE              2, MARIO_JUMP_0  ; 10

                ; land ( 12 )

                BYTE              2, MARIO_LAND_0  ; 12
                BYTE              1, MARIO_WALK_0  ; 14

                ; climb ( 16 )

                BYTE              1, MARIO_CLIMB_0 ; 16
                BYTE              1, MARIO_CLIMB_0 ; 18
                BYTE    ANIM_FLIP+2, MARIO_CLIMB_1 ; 20
                BYTE    ANIM_FLIP+2, MARIO_CLIMB_1 ; 22
                BYTE    ANIM_FLIP+2, MARIO_CLIMB_1 ; 24
                BYTE    ANIM_FLIP+2, MARIO_CLIMB_1 ; 26
                BYTE    ANIM_FLIP+2, MARIO_CLIMB_2 ; 28
                BYTE    ANIM_FLIP+2, MARIO_CLIMB_3 ; 30
                BYTE    ANIM_FLIP+2, MARIO_CLIMB_1 ; 32
                BYTE              1, MARIO_CLIMB_0 ; 34

                ; dead ( 36 )

                BYTE              4, MARIO_DEAD_0  ; 36
                BYTE              4, MARIO_DEAD_2  ; 38
                BYTE    ANIM_FLIP+4, MARIO_DEAD_1  ; 40
                BYTE              4, MARIO_DEAD_2  ; 42
                BYTE    ANIM_FLIP+4, MARIO_DEAD_0  ; 44
                BYTE              4, MARIO_DEAD_2  ; 46
                BYTE    ANIM_FLIP+4, MARIO_DEAD_1  ; 48
                BYTE              4, MARIO_DEAD_2  ; 50
                BYTE    ANIM_FLIP+4, MARIO_DEAD_0  ; 52
                BYTE              4, MARIO_DEAD_2  ; 54
                BYTE    ANIM_FLIP+4, MARIO_DEAD_1  ; 56
                BYTE              4, MARIO_DEAD_2  ; 58
                BYTE    ANIM_FLIP+4, MARIO_DEAD_0  ; 60
                BYTE              4, MARIO_DEAD_3  ; 62
                BYTE    ANIM_JUMP  , 36+13*2+2     ; 64

; Mario jump animation Y-table

                ; -------------------------------------------------
                ;
                ; original arcade 60Hz jump Y-offsets ( 40 frames )
                ;
                ;  -2,-1,-2,-1,-1,-1,-1, 0,-1,-1 ; 39 - 30
                ;   0,-1, 0,-1, 0, 0, 0,-1, 0, 0 ; 29 - 20
                ;   0, 1, 0, 0, 0, 1, 0, 1, 0, 1 ; 19 - 10
                ;   1, 0, 1, 1, 1, 1, 1, 1, 1, 2 ;  9 -  0
                ;
                ; -------------------------------------------------

                ; 30Hz combined jump Y-offsets ( 20 frames )

ST_MarioJumpTab:BYTE  -3,-3,-2,-1,-2 ; 19 - 15
                BYTE  -1,-1, 0,-1, 0 ; 14 - 10
                BYTE   1, 0, 1, 1, 1 ;  9 -  5
                BYTE   1, 2, 2, 2, 3 ;  4 -  0

; Mario animation graphics tables

                MAKE_MARIO_LABEL_SEQUENCE ST_Mario, 00, B5, MarioWalk0
                MAKE_MARIO_LABEL_SEQUENCE ST_Mario, 01, B7, MarioWalk0_Hammer0
                MAKE_MARIO_LABEL_SEQUENCE ST_Mario, 02, B7, MarioWalk0_Hammer1
                MAKE_MARIO_LABEL_SEQUENCE ST_Mario, 03, B5, MarioWalk1
                MAKE_MARIO_LABEL_SEQUENCE ST_Mario, 04, B7, MarioWalk1_Hammer0
                MAKE_MARIO_LABEL_SEQUENCE ST_Mario, 05, B7, MarioWalk1_Hammer1
                MAKE_MARIO_LABEL_SEQUENCE ST_Mario, 06, B5, MarioWalk2
                MAKE_MARIO_LABEL_SEQUENCE ST_Mario, 07, B7, MarioWalk2_Hammer0
                MAKE_MARIO_LABEL_SEQUENCE ST_Mario, 08, B7, MarioWalk2_Hammer1
                MAKE_MARIO_LABEL_SEQUENCE ST_Mario, 09, B5, MarioJump0
                MAKE_MARIO_LABEL_SEQUENCE ST_Mario, 10, B5, MarioLand0
                MAKE_MARIO_LABEL_SEQUENCE ST_Mario, 11, B5, MarioClimb0
                MAKE_MARIO_LABEL_SEQUENCE ST_Mario, 12, B5, MarioClimb1
                MAKE_MARIO_LABEL_SEQUENCE ST_Mario, 13, B5, MarioClimb2
                MAKE_MARIO_LABEL_SEQUENCE ST_Mario, 14, B5, MarioClimb3
                MAKE_MARIO_LABEL_SEQUENCE ST_Mario, 15, B5, MarioDead0
                MAKE_MARIO_LABEL_SEQUENCE ST_Mario, 16, B5, MarioDead1
                MAKE_MARIO_LABEL_SEQUENCE ST_Mario, 17, B5, MarioDead2
                MAKE_MARIO_LABEL_SEQUENCE ST_Mario, 18, B5, MarioDead3

ST_MarioGRP_L:  MAKE_MARIO_LOBYTE_TABLE ST_Mario, _GRP, 0
ST_MarioGRP_H:  MAKE_MARIO_HIBYTE_TABLE ST_Mario, _GRP, 0
ST_MarioCOL_L:  MAKE_MARIO_LOBYTE_TABLE ST_Mario, _COL, 0
ST_MarioCOL_H:  MAKE_MARIO_HIBYTE_TABLE ST_Mario, _COL, 0

; Mario animation state jump tables

ST_MarioState_L:BYTE    <(ST_MarioStart-1), <(ST_MarioWalk-1),  <(ST_MarioJump-1)
                BYTE    <(ST_MarioLand-1),  <(ST_MarioClimb-1), <(ST_MarioDead-1), <(ST_MarioWin-1)
ST_MarioState_H:BYTE    >(ST_MarioStart-1), >(ST_MarioWalk-1),  >(ST_MarioJump-1)
                BYTE    >(ST_MarioLand-1),  >(ST_MarioClimb-1), >(ST_MarioDead-1), >(ST_MarioWin-1)

;
; Kong animation tables ----------------------------------------------
;

KONG_ANIM_THROW = 0 + 1
KONG_ANIM_DRUM  = 1 + 1
KONG_ANIM_CLIMB = 7 + 1

                ;                   step       size        move     time       graphics
                ;
                ; - [ throw ] --------------------------------------------------------------
                MAKE_KONG_ANIM_STEP  00,            0,       0,      26-1,     B5_KONG_THROW
                ;
                ; - [ drum ] ---------------------------------------------------------------
                MAKE_KONG_ANIM_STEP  01,            0,       0, (158/2)-1,     EN_KONG_STAND
                MAKE_KONG_ANIM_STEP  02,            0,       0,      32-1,     EN_KONG_DRUM0
                MAKE_KONG_ANIM_STEP  03,            0,       0,      32-1,     EN_KONG_DRUM1
                MAKE_KONG_ANIM_STEP  04,            0,       0,      32-1,     EN_KONG_DRUM0
                MAKE_KONG_ANIM_STEP  05,            0,       0, (158/2)-1,     EN_KONG_STAND
                MAKE_KONG_ANIM_STEP  06,            0,     1+1,         0,     EN_KONG_LOOP
                ;
                ; - [ climb ] --------------------------------------------------------------
                MAKE_KONG_ANIM_STEP  07,            0,       0,     104-1,     EN_KONG_STAND
                ;
                MAKE_KONG_ANIM_STEP  08,   (32-1)*8+4,       0,       8-1,     IN_KONG_CLIMB1
                MAKE_KONG_ANIM_STEP  09,   (28-1)*8+0,       4,       8-1,     IN_KONG_CLIMB0
                MAKE_KONG_ANIM_STEP  10,   (24-1)*8+4,       8,       8-1,     IN_KONG_CLIMB3
                MAKE_KONG_ANIM_STEP  11,   (20-1)*8+0,      12,       8-1,     IN_KONG_CLIMB0
                MAKE_KONG_ANIM_STEP  12,   (16-1)*8+4,      16,       8-1,     IN_KONG_CLIMB1
                MAKE_KONG_ANIM_STEP  13,   (12-1)*8+0,      20,       8-1,     IN_KONG_CLIMB2
                MAKE_KONG_ANIM_STEP  14,    (8-1)*8+4,      24,       8-1,     IN_KONG_CLIMB3
                MAKE_KONG_ANIM_STEP  15,    (4-1)*8+0,      28,       8-1,     IN_KONG_CLIMB0
                ;
                MAKE_KONG_ANIM_STEP  16,            0,      32,     110-1,     IN_KONG_OFF

ST_KongSizeTab: MAKE_KONG_ANIM_TABLE SVAL
ST_KongMoveTab: MAKE_KONG_ANIM_TABLE MVAL
ST_KongTimeTab: MAKE_KONG_ANIM_TABLE TVAL
ST_KongGrfxTab: MAKE_KONG_ANIM_TABLE GVAL

ST_KongCol      BYTE    KONG_COL_0, KONG_COL_1

;
; Hammer tables ------------------------------------------------------
;

HAMMER_STATE_AVAIL =   0 ; must be 0
HAMMER_STATE_SNAG  =   1 ; must be 1
HAMMER_STATE_OFF   =   2
HAMMER_STATE_ON    = 255

ST_HammerNUSIZ: BYTE    %00000000,         %00100000,         %00100000,         %00100000
ST_HammerCOL_L: BYTE    <B3_Hammer0_COL_0, <B3_Hammer0_COL_1, <B3_Hammer1_COL_0, <B3_Hammer1_COL_1

;
; Stage gap plan -----------------------------------------------------
;

;
; Gap start plan
;

ST_GapStartPlan:SUBROUTINE

; stage 1

S1_GapStartPlan:; BYTE      0,      0,      0,      0 ; floor 0
                  BYTE      0,      0,      0, 28*4+2 ; floor 1
                  BYTE  0*4+2,      0,      0,      0 ; floor 2
                  BYTE      0,      0,      0, 28*4+2 ; floor 3
                  BYTE  0*4+2,      0,      0,      0 ; floor 4
                  BYTE      0,      0,      0, 28*4+2 ; floor 5

; stage 2

S2_GapStartPlan:; BYTE      0,      0,      0,      0 ; floor 0
                  BYTE      0,      0,      0,      0 ; floor 1
                  BYTE  0*4+2,  8*4+2, 21*4+2, 29*4+2 ; floor 2
                  BYTE      0, 15*4+2,      0,      0 ; floor 3
                  BYTE      0,      0,      0,      0 ; floor 4

; stage 3

S3_GapStartPlan:; BYTE      0,      0,      0,      0 ; floor 0
                  BYTE  5*4+2, 13*4+2, 21*4+2, 24*4+2 ; floor 1
                  BYTE  5*4+2, 11*4+2, 13*4+2, 24*4+2 ; floor 2
                  BYTE  5*4+2, 13*4+2, 21*4+2, 24*4+2 ; floor 3
                  BYTE      0,      0, 23*4+2,      0 ; floor 4

; stage 4

S4_GapStartPlan:; BYTE      0,      0,      0,      0 ; floor 0
                  BYTE  0*4+2,  9*4+2, 22*4+2, 29*4+2 ; floor 1
                  BYTE  0*4+2,  9*4+2, 22*4+2, 28*4+2 ; floor 2
                  BYTE  0*4+2,  9*4+2, 22*4+2, 27*4+2 ; floor 3
                  BYTE  0*4+2,  9*4+2, 22*4+2, 26*4+2 ; floor 4

;
; Gap width plan
;

ST_GapWidthPlan:SUBROUTINE

; stage 1

S1_GapWidthPlan:; BYTE      0,      0,      0,      0 ; floor 0
                  BYTE      0,      0,      0,  4*4-3 ; floor 1
                  BYTE  4*4-3,      0,      0,      0 ; floor 2
                  BYTE      0,      0,      0,  4*4-3 ; floor 3
                  BYTE  4*4-3,      0,      0,      0 ; floor 4
                  BYTE      0,      0,      0,  4*4-3 ; floor 5

; stage 2

S2_GapWidthPlan:; BYTE      0,      0,      0,      0 ; floor 0
                  BYTE      0,      0,      0,      0 ; floor 1
                  BYTE  3*4-3,  2*4-3,  2*4-3,  3*4-3 ; floor 2
                  BYTE      0,  2*4-3,      0,      0 ; floor 3
                  BYTE      0,      0,      0,      0 ; floor 4

; stage 3

S3_GapWidthPlan:; BYTE      0,      0,      0,      0 ; floor 0
                  BYTE  5*4-3,  6*4-3,  1*4-3,  1*4-3 ; floor 1
                  BYTE  5*4-3,  1*4-3,  6*4-3,  1*4-3 ; floor 2
                  BYTE  5*4-3,  5*4-3,  1*4-3,  1*4-3 ; floor 3
                  BYTE      0,      0,  5*4-3,      0 ; floor 4

; stage 4

S4_GapWidthPlan:; BYTE      0,      0,      0,      0 ; floor 0
                  BYTE  3*4-3,  1*4-3,  1*4-3,  3*4-3 ; floor 1
                  BYTE  4*4-3,  1*4-3,  1*4-3,  4*4-3 ; floor 2
                  BYTE  5*4-3,  1*4-3,  1*4-3,  5*4-3 ; floor 3
                  BYTE  6*4-3,  1*4-3,  1*4-3,  6*4-3 ; floor 4

;
; Stage ladder plan --------------------------------------------------
;

; Donkey Kong
; -----------
; Stage 2 ladder timing (left ladder)
;
; Ladder on top     : 258 frames
; Ladder going down : 119 frames
; Ladder on bottom  :  85 frames
; Ladder going up   :  59 frames

; static ladders

LADDER_TYP_MASK = %11100000
LADDER_POS_MASK = %00011111

LADDER_BROKEN   = %00100000
LADDER_UP       = %01000000
LADDER_DOWN     = %10000000

LADDER_B        = %00100000 ; broken
LADDER_F        = %00000000 ; full

; telescope ladders

LADDER_STATE_TOP    = S2_LADDER_TOP_TIME + S2_LADDER_DOWN_TIME + S2_LADDER_BOTTOM_TIME + S2_LADDER_UP_TIME
LADDER_STATE_DOWN   =                      S2_LADDER_DOWN_TIME + S2_LADDER_BOTTOM_TIME + S2_LADDER_UP_TIME
LADDER_STATE_BOTTOM =                                            S2_LADDER_BOTTOM_TIME + S2_LADDER_UP_TIME
LADDER_STATE_UP     =                                                                    S2_LADDER_UP_TIME

; ladder setup

                BYTE              0,           0,           0,           0,           0

ST_LadderPlan:  SUBROUTINE

; stage 1

S1_LadderPlan:  BYTE              0, LADDER_B+12,           0, LADDER_F+25,           0 ; floor 0
                BYTE    LADDER_F+06,           0, LADDER_F+14,           0,           0 ; floor 1
                BYTE              0, LADDER_B+10, LADDER_F+16, LADDER_F+25,           0 ; floor 2
                BYTE    LADDER_F+06, LADDER_F+11,           0, LADDER_B+23,           0 ; floor 3
                BYTE              0, LADDER_B+13,           0, LADDER_F+25,           0 ; floor 4
                BYTE              0, LADDER_F+11, LADDER_F+20,           0,           0 ; floor 5
                BYTE              0,           0,           0,           0,           0 ; floor 6

; stage 2

S2_LadderPlan:  BYTE    LADDER_F+05, LADDER_F+12, LADDER_F+19, LADDER_F+26,           0 ; floor 0
                BYTE              0, LADDER_F+10, LADDER_F+20,           0,           0 ; floor 1
                BYTE    LADDER_F+05, LADDER_F+12, LADDER_F+19, LADDER_F+26,           0 ; floor 2
                BYTE    LADDER_F+04,           0,           0, LADDER_F+27,           0 ; floor 3
              ; BYTE              0,           0,           0,           0,           0 ; floor 4 ; (unused in stage *3*)

; stage 3

S3_LadderPlan:  BYTE              0,           0,           0,           0,           0 ; floor 0
                BYTE    LADDER_F+03, LADDER_F+10, LADDER_F+12, LADDER_F+28,           0 ; floor 1
                BYTE    LADDER_F+04, LADDER_F+10, LADDER_F+12, LADDER_F+19, LADDER_F+25 ; floor 2
                BYTE              0, LADDER_F+22,           0, LADDER_F+28,           0 ; floor 3
                BYTE              0, LADDER_F+11, LADDER_F+20,           0,           0 ; floor 4
                BYTE              0,           0,           0,           0,           0 ; floor 5

; stage 4

S4_LadderPlan:  BYTE    LADDER_F+03, LADDER_F+15, LADDER_F+28,           0,           0 ; floor 0
                BYTE    LADDER_F+04, LADDER_F+11, LADDER_F+20, LADDER_F+27,           0 ; floor 1
                BYTE    LADDER_F+05, LADDER_F+15, LADDER_F+26,           0,           0 ; floor 2
                BYTE    LADDER_F+06, LADDER_F+10, LADDER_F+21, LADDER_F+25,           0 ; floor 3
                BYTE              0,           0,           0,           0,           0 ; floor 4

;
; Stage floor playfield ----------------------------------------------
;

;
; Girders
;

; stage 1
S1_GIRDER_0     = 0*8 + 7
S1_GIRDER_1     = 1*8 + 7
S1_GIRDER_2     = 2*8 + 7
S1_GIRDER_3     = 3*8 + 7

; stage 3
S3_GIRDER_0     = 4*8 + 7
S3_GIRDER_1     = 5*8 + 7
S3_GIRDER_2     = 6*8 + 7
S3_GIRDER_3     = 7*8 + 7
S3_GIRDER_4     = 8*8 + 7
S3_GIRDER_5     = 3*8 + 7

; stage 4
S4_GIRDER_0     =  0*8 + 7
;
S4_GIRDER_1_00  =  1*8 + 7
S4_GIRDER_1_01  =  2*8 + 7
S4_GIRDER_1_10  =  3*8 + 7
S4_GIRDER_1_11  =  4*8 + 7
;
S4_GIRDER_2_00  =  5*8 + 7
S4_GIRDER_2_01  =  6*8 + 7
S4_GIRDER_2_10  =  7*8 + 7
S4_GIRDER_2_11  =  8*8 + 7
;
S4_GIRDER_3_00  =  9*8 + 7
S4_GIRDER_3_01  = 10*8 + 7
S4_GIRDER_3_10  = 11*8 + 7
S4_GIRDER_3_11  = 12*8 + 7
;
S4_GIRDER_4_00  = 13*8 + 7
S4_GIRDER_4_01  = 14*8 + 7
S4_GIRDER_4_10  = 15*8 + 7
S4_GIRDER_4_11  = 16*8 + 7
;
S4_GIRDER_5     = 17*8 + 7

; stage 2
S2_GIRDER_0     = 18*8 + 7
S2_GIRDER_1     = 19*8 + 7
S2_GIRDER_2     = 20*8 + 7
S2_GIRDER_3     = 21*8 + 7

;
; Corridors
;

; stage 1
S1_CORRIDOR_0   =  0*22 + 21
S1_CORRIDOR_1   =  1*22 + 21
S1_CORRIDOR_2   =  2*22 + 21
S1_CORRIDOR_3   =  3*22 + 21
S1_CORRIDOR_4   =  4*22 + 21
S1_CORRIDOR_5   =  5*22 + 21

; stage 3
S3_CORRIDOR_0   =  6*22 + 21
S3_CORRIDOR_1   =  7*22 + 21
S3_CORRIDOR_2   =  8*22 + 21
S3_CORRIDOR_3   =  9*22 + 21
S3_CORRIDOR_4   = 10*22 + 21

; stage 4
S4_CORRIDOR_0   =  0*22 + 21
S4_CORRIDOR_1   =  1*22 + 21
S4_CORRIDOR_2   =  2*22 + 21
S4_CORRIDOR_3   =  3*22 + 21
S4_CORRIDOR_4   =  4*22 + 21

; stage 2
S2_CORRIDOR_0   =  5*22 + 21
S2_CORRIDOR_1   =  6*22 + 21
S2_CORRIDOR_2   =  7*22 + 21
S2_CORRIDOR_3   =  8*22 + 21

;
; Floor playfield index
;

ST_FloorPFIdx:  SUBROUTINE

; stage 1

S1_FloorPFIdx:  BYTE                   S1_GIRDER_0    ; ground
                BYTE    S1_CORRIDOR_0, S1_GIRDER_1    ; floor 0
                BYTE    S1_CORRIDOR_1, S1_GIRDER_2    ; floor 1
                BYTE    S1_CORRIDOR_2, S1_GIRDER_1    ; floor 2
                BYTE    S1_CORRIDOR_3, S1_GIRDER_2    ; floor 3
                BYTE    S1_CORRIDOR_4, S1_GIRDER_1    ; floor 4
                BYTE    S1_CORRIDOR_5, S1_GIRDER_3    ; floor 5
                BYTE    S1_CORRIDOR_5, S1_GIRDER_3    ; floor 6 ; Kong

; stage 2

S2_FloorPFIdx:  BYTE                   S2_GIRDER_0    ; ground
                BYTE    S2_CORRIDOR_0, S2_GIRDER_1    ; floor 0
                BYTE    S2_CORRIDOR_1, S2_GIRDER_2    ; floor 1
                BYTE    S2_CORRIDOR_2, S2_GIRDER_3    ; floor 2
                BYTE    S2_CORRIDOR_3, S2_GIRDER_1    ; floor 3
                BYTE    S2_CORRIDOR_3, S2_GIRDER_1    ; floor 4 ; Kong

; stage 3

S3_FloorPFIdx:  BYTE                   S3_GIRDER_0    ; ground
                BYTE    S3_CORRIDOR_0, S3_GIRDER_1    ; floor 0
                BYTE    S3_CORRIDOR_1, S3_GIRDER_2    ; floor 1
                BYTE    S3_CORRIDOR_2, S3_GIRDER_3    ; floor 2
                BYTE    S3_CORRIDOR_3, S3_GIRDER_4    ; floor 3
                BYTE    S3_CORRIDOR_4, S3_GIRDER_5    ; floor 4
                BYTE    S3_CORRIDOR_4, S3_GIRDER_5    ; floor 5 ; Kong

; stage 4

S4_FloorPFIdx:  BYTE                   S4_GIRDER_0    ; ground
                BYTE    S4_CORRIDOR_0, S4_GIRDER_1_00 ; floor 0
                BYTE    S4_CORRIDOR_1, S4_GIRDER_2_00 ; floor 1
                BYTE    S4_CORRIDOR_2, S4_GIRDER_3_00 ; floor 2
                BYTE    S4_CORRIDOR_3, S4_GIRDER_4_00 ; floor 3
                BYTE    S4_CORRIDOR_4, S4_GIRDER_5    ; floor 4
                BYTE    S4_CORRIDOR_4, S4_GIRDER_5    ; floor 5 ; Kong

;
; Floor playfield color
;

ST_FloorPFCol:  SUBROUTINE

; stage 1

S1_FloorPFCol:  BYTE    DK_COL_CYAN   ; floor 0
                BYTE    DK_COL_CYAN   ; floor 1
                BYTE    DK_COL_CYAN   ; floor 2
                BYTE    DK_COL_CYAN   ; floor 3
                BYTE    DK_COL_CYAN   ; floor 4
                BYTE    DK_COL_CYAN   ; floor 5
              ; BYTE    DK_COL_BLACK  ; floor 6 ; Kong

; stage 2

S2_FloorPFCol:  BYTE    DK_COL_WHITE  ; floor 0
                BYTE    DK_COL_WHITE  ; floor 1
                BYTE    DK_COL_WHITE  ; floor 2
                BYTE    DK_COL_WHITE  ; floor 3
              ; BYTE    DK_COL_BLACK  ; floor 4 ; Kong

; stage 3

S3_FloorPFCol = S1_FloorPFCol

; stage 4

S4_FloorPFCol:  BYTE    DK_COL_YELLOW ; floor 0
                BYTE    DK_COL_YELLOW ; floor 1
                BYTE    DK_COL_YELLOW ; floor 2
                BYTE    DK_COL_YELLOW ; floor 3
                BYTE    DK_COL_CYAN   ; floor 4
              ; BYTE    DK_COL_BLACK  ; floor 5 ; Kong

;
; Stage offset tables ------------------------------------------------
;

ST_FloorPFIdxOffsets:   MAKE_STAGE_OFFSET_TABLE FloorPFIdx,   8+1-1
ST_FloorPFColOffsets:   MAKE_STAGE_OFFSET_TABLE FloorPFCol,   4-1
ST_LadderPlanOffsets:   MAKE_STAGE_OFFSET_TABLE LadderPlan,   0
ST_GapStartPlanOffsets: MAKE_STAGE_OFFSET_TABLE GapStartPlan, -4-1

;
; Stage parameters ---------------------------------------------------
;
                ;       stage 1  stage 2  stage 3  stage 4
ST_FloorMaxTab: BYTE        4*4,     2*4,     3*4,     3*4
ST_FloorKongTab:BYTE        3*4,     1*4,     2*4,     2*4

                ;       h-pos  v-pos  y-pos  floor
ST_MarioSetTab: BYTE    24+22,  0*32,  0*32,   0*4 ; stage 1,2,4
                BYTE    24+01,  1*32,  1*32,   1*4 ; stage 3

;
; Enemy data ---------------------------------------------------------
;

; Crazy Kong
; ----------
; Enemy speed in pixels per frame
;
;           L=1   L=2   L=3   L=4   L=5
; --------------------------------------
; Barrel    1.00  1.00  1.00  1.00  1.00
; Fireball  0.50  1.00  1.00  1.00  1.00
; Cement    0.50  1.00  1.00  1.00  1.00
; Elevator  0.25  0.50  0.50  0.50  0.50
; Jack      2.00  2.00  2.00  2.00  2.00

; enemy animation graphics tables

ENEMY_EMPTY_SLOT    = 0 ; free slot
ENEMY_EMPTY_GRP     = 1 ; enemy invisible
ENEMY_INDEX_START   = 2 ; first index

ENEMY_FIREBALL_0    =  1 * 2 ; must be 2
ENEMY_FIREBALL_1    =  2 * 2
ENEMY_FLAME_0       =  3 * 2
ENEMY_FLAME_1       =  4 * 2
ENEMY_BLAZE_0       =  5 * 2
ENEMY_BLAZE_1       =  6 * 2
ENEMY_BARREL_ROLL_0 =  7 * 2
ENEMY_BARREL_ROLL_1 =  8 * 2
ENEMY_BARREL_FALL_0 =  9 * 2
ENEMY_BARREL_FALL_1 = 10 * 2
ENEMY_CEMENT        = 11 * 2
ENEMY_JACK_0        = 12 * 2
ENEMY_JACK_1        = 13 * 2
ENEMY_JACK_2        = 14 * 2
ENEMY_ELEVATOR      = 15 * 2
ENEMY_VANISH        = 16 * 2

ENEMY_NOJUMP_START  = ENEMY_JACK_0 ; no points for jumping over

                MAKE_ENEMY_LABEL_SEQUENCE ST_Enemy, 00, B3, Fireball0,   FIREBALL_0
                MAKE_ENEMY_LABEL_SEQUENCE ST_Enemy, 01, B3, Fireball1,   FIREBALL_1
                MAKE_ENEMY_LABEL_SEQUENCE ST_Enemy, 02, B4, Flame0,      FLAME_0
                MAKE_ENEMY_LABEL_SEQUENCE ST_Enemy, 03, B4, Flame1,      FLAME_1
                MAKE_ENEMY_LABEL_SEQUENCE ST_Enemy, 04, B4, Blaze0,      BLAZE_0
                MAKE_ENEMY_LABEL_SEQUENCE ST_Enemy, 05, B4, Blaze1,      BLAZE_1
                MAKE_ENEMY_LABEL_SEQUENCE ST_Enemy, 06, B3, BarrelRoll0, BARREL_ROLL_0
                MAKE_ENEMY_LABEL_SEQUENCE ST_Enemy, 07, B3, BarrelRoll1, BARREL_ROLL_1
                MAKE_ENEMY_LABEL_SEQUENCE ST_Enemy, 08, B3, BarrelFall0, BARREL_FALL_0
                MAKE_ENEMY_LABEL_SEQUENCE ST_Enemy, 09, B3, BarrelFall1, BARREL_FALL_1
                MAKE_ENEMY_LABEL_SEQUENCE ST_Enemy, 10, B4, Cement,      CEMENT
                MAKE_ENEMY_LABEL_SEQUENCE ST_Enemy, 11, B3, Jack0,       JACK_0
                MAKE_ENEMY_LABEL_SEQUENCE ST_Enemy, 12, B3, Jack1,       JACK_1
                MAKE_ENEMY_LABEL_SEQUENCE ST_Enemy, 13, B3, Jack2,       JACK_2
                MAKE_ENEMY_LABEL_SEQUENCE ST_Enemy, 14, B3, Elevator,    ELEVATOR
                MAKE_ENEMY_LABEL_SEQUENCE ST_Enemy, 15, B3, Vanish,      VANISH

ST_EnemyGRP_L:  MAKE_ENEMY_LOBYTE_TABLE ST_Enemy, _GRP, 1
ST_EnemyGRP_H:  MAKE_ENEMY_HIBYTE_TABLE ST_Enemy, _GRP, 1
ST_EnemyCOL_L:  MAKE_ENEMY_LOBYTE_TABLE ST_Enemy, _COL, 0
ST_EnemyCOL_H:  MAKE_ENEMY_HIBYTE_TABLE ST_Enemy, _COL, 0

ST_EnemyCull:   MAKE_ENEMY_CULL_TABLE ST_Enemy, _HEIGHT, BAND_24_SIZE

; item graphics tables

                MAKE_ITEM_LABEL_SEQUENCE ST_Item, 00, B3, Oil
                MAKE_ITEM_LABEL_SEQUENCE ST_Item, 01, B4, Handbag
                MAKE_ITEM_LABEL_SEQUENCE ST_Item, 02, B3, Hat
                MAKE_ITEM_LABEL_SEQUENCE ST_Item, 03, B4, Umbrella

ST_ItemGRP_L:   MAKE_ITEM_LOBYTE_TABLE ST_Item, _GRP, 1
ST_ItemCOL_L:   MAKE_ITEM_LOBYTE_TABLE ST_Item, _COL, 0

; ********************************************************************

#if PRINT_MESSAGES
                ECHO "---- Bank 2: ", (ROMJumpTable - *), "bytes of ROM left"
#endif
