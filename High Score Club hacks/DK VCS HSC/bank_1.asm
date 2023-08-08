
; ********************************************************************
;  Donkey Kong VCS
;
;    ROM Bank 1
;
;    $Date: Sat, 14 Jan 2017 19:34:58 +0100 $
;    $Author: dietrich $
;    $Revision: 478 $
;
;  Copyright (C) 2017 Andreas Dietrich
; ********************************************************************

;---------------------------------------------------------------------
;       IN Constants
;---------------------------------------------------------------------

IN_GIRDER_COL_0         = COL_40        ; dark girder color  |940000|
IN_GIRDER_COL_1         = COL_56        ; light girder color |B846A2|

;---------------------------------------------------------------------
;       IN Variables
;---------------------------------------------------------------------

IN_VarStart = $D4

IN_RAMCode              = $A0 ; [$34]   ; RAM for Bitmap48 code

IN_KongHPos             = $D4           ; Kong horizontal position
IN_KongVPos             = $D5           ; Kong vertical position
IN_KongVPosOld          = $D6           ; Kong vertical position from previous frame
IN_KongAnimState        = $D7           ; Kong animation state
IN_KongAnimTimer        = $D8           ; Kong animation timer
IN_KongAnimLoop         = $D9           ; Kong animation loop counter
IN_KongSkipLines        = $DA           ; blank lines above Kong

IN_PaulineMask          = $DB           ; switch Pauline on/off

IN_LadderLinesLo        = $DC           ; ladder lines below Kong
IN_LadderLinesHi        = $DD           ; ladder lines above Kong
IN_LadderUp             = $DE           ; invisible ladder lines
IN_LadderEnd            = $DF           ; scrolling ladder end

IN_GirderColor          = $E0           ; color of line below Kong
IN_GirderStart          = $E1           ; draw girder start line
IN_GirderEnd            = $E2           ; draw girder end line
IN_GirderLoEnd          = $E3           ; end of bottom scrolling girder
IN_GirderHiStart        = $E4           ; start of top scrolling girder
IN_GirderOffset         = $E5           ; position of lowered girder

IN_LevelSelect          = $E6           ; debounce select switch

;---------------------------------------------------------------------
;       B1 Variables
;---------------------------------------------------------------------

B1_KongPtr              = $AA           ; WARNING: must be same as ST_MarioCOLPtr


; ********************************************************************
;
;       Code Section
;
; ********************************************************************

; --------------------------------------------------------------------
;       Intro
; --------------------------------------------------------------------

Intro:          STOREBYTE 1, Bank

; --------------------------------------------------------------------
;       IN Init
; --------------------------------------------------------------------

IN_Init:        SUBROUTINE

                ; reset level/stage parameters

                lda     #(1*16)+0               ; level: 1 | round: 1
                sta     Level_Round

                ; check demo mode

                jsr     IN_SetDemoMode

                ; reset player parameters

                lda     #(1*128)+3              ; death: 1 | lives: 3 (B)
                bit     SWCHB
                bvc     .write
                lda     #(1*128)+1              ; death: 1 | lives: 1 (A)
.write          sta     Death_Lives

                ; clear RAM and TIA registers

IN_CLR_RANGE = StackBottom-1-IN_VarStart ; RAM clear range
IN_CLR_RANGE = RESMP1-NUSIZ0             ; TIA clear range (must be same)

                lda     #0
                ldx     #IN_CLR_RANGE
.loop0          sta     IN_VarStart,x
                sta     NUSIZ0,x
                dex
                bpl     .loop0

                ; non-zero variables

                STOREBYTE -6,        IN_LadderUp
                STOREBYTE 16+(3*15), IN_KongHPos

                ; copy Bitmap48 code to RAM

                ldx     #(IN_Bitmap48End-IN_Bitmap48)-1
.loop1          lda     IN_Bitmap48,x
                sta     IN_RAMCode,x
                dex
                bpl     .loop1

                ; noise player

                lda     #MUSIC_INIT_INTRO
                jsr     Bank5_DriveAudio

.wait           lda     INTIM
                bne     .wait

; --------------------------------------------------------------------
;       IN VSync
; --------------------------------------------------------------------

IN_VSync:       VERTICAL_SYNC

; --------------------------------------------------------------------
;       IN VBlank
; --------------------------------------------------------------------

IN_VBlank:      SUBROUTINE

                ; Kong animation

IN_MoveKong:    SUBROUTINE
                ldy     IN_KongAnimState

                ; progress animation time

                lda     FrameCtr
                lsr
                bcs     .end
                dec     IN_KongAnimTimer
                bpl     .move

                ; progress animation state

.command        iny
                lda     IN_KongGfxTab-1,y
                bpl     .write
                dec     IN_KongAnimLoop
                beq     .command
                bpl     .jump
                lda     IN_KongTimeTab-1,y
                sta     IN_KongAnimLoop
.jump           lda     IN_KongVTab-1,y
                tay
.write          sty     IN_KongAnimState
.timer          lda     IN_KongTimeTab-1,y
                sta     IN_KongAnimTimer

                ; update position

.move           clc
                lda     IN_KongHTab-1,y
                adc     IN_KongHPos
                sta     IN_KongHPos
                clc
                lda     IN_KongVTab-1,y
                adc     IN_KongVPos
                sta     IN_KongVPos
.end
                ; Pauline animation

IN_MovePauline: SUBROUTINE
                cpy     #31+1
                bcc     .end

                STOREBYTE $FF, IN_PaulineMask
.end

; --------------------------------------------------------------------

IN_Audio:       SUBROUTINE

                ; no sound if there's no Kong

                cpy     #1+1
                bcc     .end

                ; play music while Kong is climbing

.music          cpy     #29+1
                bcs     .sound_clear

                lda     #MUSIC_PLAY
                bne     .drive

                ; clear sound timer variables

.sound_clear    cpy     #30+1
                bcs     .sound_impact

                lda     #AUDIO_CLEAR
                beq     .drive

                ; Kong hits floor

.sound_impact   cpy     #43+1
                bcs     .sound_grin

                lda     IN_KongVPos
                cmp     #6
                bne     .play
                lda     IN_KongVPosOld
                cmp     #5
                bne     .play

                inc     IN_GirderOffset

                lda     #SOUND_INIT_KONG_IMPACT
                jsr     Bank5_DriveAudio

                ; Kong grins

.sound_grin     lda     IN_KongAnimTimer
                cmp     #60-1
                bne     .play

                lda     #SOUND_INIT_KONG_GRIN
                jsr     Bank5_DriveAudio

                ; keep driving sounds

.play           lda     #SOUND_PLAY
.drive          jsr     Bank5_DriveAudio

.end            lda     IN_KongVPos
                sta     IN_KongVPosOld

; --------------------------------------------------------------------

IN_SetupLadder: SUBROUTINE

                ; X = max(VPos-1, 0)

                ldx     IN_KongVPos
                dex
                bpl     .steps
                inx

                ; steps above and below Kong

.steps          txa
                and     #%00000111
                bne     .raise
                stx     IN_LadderLinesLo
                sec
                lda     #28*4+2
                sbc     IN_LadderLinesLo
                sta     IN_LadderLinesHi

                ; raise ladder

.raise          txa
                sec
                sbc     IN_LadderUp
                cmp     #(4*4)-1+6
                bne     .end
                clc
                lda     IN_LadderUp
                adc     #8
                sta     IN_LadderUp
.end

; --------------------------------------------------------------------

IN_Scrolling:   SUBROUTINE

                ; scroll bottom girder

.girder         sec
                txa
                sbc     #(10*4)-1
                bcc     .ladder
                sta     IN_GirderLoEnd
                sta     IN_GirderHiStart

                ; scroll ladder below Kong

.ladder         sec
                txa
                sbc     #(10*4)+8-1
                bcc     .end
                sta     IN_LadderEnd
.end

; --------------------------------------------------------------------

IN_SetupB48:    SUBROUTINE

IN_RAM_B48BGTab =   8
IN_RAM_B48BG    =  11
IN_RAM_B48PF1   =  46
IN_RAM_B48Char0 =   3
IN_RAM_B48Char1 =  15
IN_RAM_B48Char2 =  20
IN_RAM_B48Char3 =  25
IN_RAM_B48Char4 =  30
IN_RAM_B48Char5 =  34

                ldy     IN_KongAnimState
                ldx     IN_KongGfxTab-1,y

                ; patch RAM code based on animation

.stand          cpx     #IN_KONG_STAND
                bne     .grin
                lda     #ENABL
                sta     IN_RAMCode+IN_RAM_B48BG
                STOREWORD IN_KongENABL, IN_RAMCode+IN_RAM_B48BGTab

.grin           cpx     #IN_KONG_GRIN
                bne     .switch
                lda     #PF1
                sta     IN_RAMCode+IN_RAM_B48PF1
                sta     IN_RAMCode+IN_RAM_B48BG
                STOREWORD IN_KongPF1, IN_RAMCode+IN_RAM_B48BGTab

                ; update GRP pointers

.switch         lda     FrameCtr
                lsr
                bcc     .write
                txa
                adc     #6-1
                tax

.write          STOREBYTE 5, Temp
.loop           ldy     Temp
                lda     IN_B48Offsets,y
                tay
                lda     IN_KongGRP_L,x
                sta     IN_RAMCode,y
                lda     IN_KongGRP_H,x
                sta     IN_RAMCode+1,y
                dex
                dec     Temp
                bpl     .loop

IN_VBlankEnd:   lda     INTIM
                bne     IN_VBlankEnd

; --------------------------------------------------------------------
;       IN Kernel
; --------------------------------------------------------------------

;
;  Part 1 -- Score
;

IN_Score:       jsr     Bank7_DrawScore

                lda     #%00110001
                sta     CTRLPF

; - Switch -----------------------------------------------------------

IN_Switch:      lda     IN_KongAnimState
                cmp     #27+1
                bcc     IN_Kernel_0
                jmp     IN_Kernel_1

; - Kernel 0 ---------------------------------------------------------

IN_Kernel_0:    SUBROUTINE

                ; setup Kong

IN_SetKong_0:   jsr     IN_KongColors

                ; position Kong

IN_PosKong_0:   lda     #$A0
                sta     HMP0
                lda     #$B0
                sta     HMP1
                nop
                sta     RESP0
                sta     RESP1
                sta     WSYNC
                sta     HMOVE

;
; Part 2 -- Girders above Kong
;

IN_GirdersHi_0: lda     #0
                sta     IN_GirderEnd
                lda     IN_GirderHiStart
                sta     IN_GirderStart
                jsr     IN_DrawGirders

;
; Part 3 -- Ladder
;

                ; ladder above Kong

IN_LadderHi_0:  SUBROUTINE
                ldy     IN_LadderLinesHi
.loop           sta     WSYNC
                ldx     #%10000001
                stx     NUSIZ1

                tya
                and     #3
                bne     .write
                ldx     #%11111111
.write          stx     GRP1
                lda     #0
                sta     GRP0
                sta     PF1
                sta     PF2
                dey
                cpy     #1
                bne     .loop

                ; skip Kong in the beginning

IN_SkipKong_0:  ldy     #36+1
                lda     IN_KongAnimState
                cmp     #1+1
                bcc     IN_LadderLoLoop_0

                ; draw alternating Kong planes 0/1

IN_DrawKong_0:  SUBROUTINE
                STOREBYTE 35, LineCtr
                sta     WSYNC
                ldx     #9
.loop           dex
                bpl     .loop
                lda     #%00000011
                sta     NUSIZ1

                jsr     IN_RAMCode

                ; ladder below Kong

IN_LadderLo_0:  SUBROUTINE
                ldy     IN_LadderLinesLo
                beq     .end

IN_LadderLoLoop_0:
.loop           sta     WSYNC
                lda     #%00000000
                sta     VDELP1
                ldx     #%10000001
                stx     NUSIZ1

                cpy     IN_LadderUp
                bpl     .next
                sta     COLUP1

.next           tya
                and     #3
                bne     .write
                ldx     #%11111111
.write          stx     GRP1
                dey
                cpy     IN_LadderEnd
                bne     .loop

                asl     Temp
                asl     Temp
.end

;
; Part 4 -- Girder below Kong
;

IN_GirderLo_0:  SUBROUTINE
                ldx     #1*8-1
                cpx     IN_GirderLoEnd
                bcc     .end
                stx     IN_GirderStart
                lda     IN_GirderLoEnd
                sta     IN_GirderEnd

                lda     #IN_GIRDER_COL_1
                sta     COLUPF
                ldx     #%11111111
                stx     PF1
                stx     PF2
                inx
                stx     GRP1

                jsr     IN_DrawGirders
.end

IN_KernelEnd_0: jmp     IN_Overscan

; - Kernel 1 ---------------------------------------------------------

IN_Kernel_1:    SUBROUTINE

;
; Part 2 -- Pauline
;

                ; setup Pauline

IN_SetPauline_1:lda     #%00000000
                sta     NUSIZ0
                sta     NUSIZ1

                ; position Pauline

IN_PosPauline_1:SUBROUTINE
                lda     #$90
                ldy     #$20
                sta     WSYNC
                ldx     #6
.loop           dex
                bne     .loop
                sta     HMP0
                sty     HMP1
                sta     RESP0
                sta     RESP1

                ; determine number of blank lines above Kong

IN_SetSkip_1:   SUBROUTINE
                lda     IN_GirderOffset
                asl
                adc     IN_KongVPos             ; C = 0
                sta     IN_KongSkipLines

                ; position Kong's teeth

IN_PosBall_1:   SUBROUTINE
                lda     IN_KongHPos
                adc     #18+9+1                 ; 18:offset, +9:RESBL; +1:SEC (C = 0)
                sta     WSYNC
.loop           sbc     #15
                bcs     .loop
                eor     #7
                asl
                asl
                asl
                asl
                sta     HMBL
                sta     RESBL
                sta     WSYNC
                sta     HMOVE

                ; draw Pauline

IN_DrwPauline_1:SUBROUTINE
                ldy     #22-1
.loop           sta     WSYNC
.plane0         lda     IN_Pauline_GRP_0,y
                and     IN_PaulineMask
                sta     GRP0
                lda     IN_Pauline_COL_0,y
                sta     COLUP0
.plane1         lda     IN_Pauline_GRP_1,y
                and     IN_PaulineMask
                sta     GRP1
                lda     IN_Pauline_COL_1,y
                sta     COLUP1
                sta     GRP0
                dey
                bpl     .loop

;
; Part 3 -- Girder above Kong
;

IN_GirderHi_1:  STOREBYTE 7*8-2, IN_GirderStart
                STOREBYTE 6*8+2, IN_GirderEnd
                lda     #0
                sta     GRP0
                sta     GRP1
                sta     GRP0

                ; draw first line of girder

                sta     WSYNC
                lda     #IN_GIRDER_COL_1
                sta     COLUPF
                lda     #%11110000
                sta     PF2

                ; Kong size

                lda     #%00000011
                sta     NUSIZ0
                ldy     IN_KongAnimState
                ldx     IN_KongGfxTab-1,y
                cpx     #IN_KONG_JUMP
                beq     .write0
                lsr
.write0         sta     NUSIZ1

                ; Kong and girder colors

                jsr     IN_KongColors
                lda     #IN_GIRDER_COL_0
                sta     COLUPF

                ; position Kong in the second girder line

                lda     IN_KongHPos
                sec
                sta     HMCLR
                sta     WSYNC
.loop0          sbc     #15
                bcs     .loop0
                eor     #7
                asl
                asl
                asl
                asl
                sta     HMP0
                sta     HMP1
                sta     RESP0
                sta     RESP1

                ; draw middle part of girder

                jsr     IN_DrawGirders

                ; draw second to last line of girder

                sta     WSYNC
                sta     HMOVE
                lda     #%11110000
                sta     PF2

                ; setup girder below Kong

                sec
                lda     #1*8+5
                sbc     IN_KongVPos
                sta     IN_GirderStart
                lda     #0*8
                sta     IN_GirderEnd

                ; color of line below Kong

                ldx     #IN_GIRDER_COL_1
                ldy     IN_KongVPos
                cpy     #6
                beq     .write1
                tax
.write1         stx     IN_GirderColor

                ; adjust player 1

                lda     #$10
                sta     HMCLR
                sta     HMP1

                ; draw last line of girder

                sta     WSYNC
                sta     HMOVE
                lda     #IN_GIRDER_COL_0
                sta     COLUPF
                ldx     #9
.loop1          dex
                bne     .loop1
                stx     PF2
                lda     #DK_COL_WHITE
                sta     COLUPF

;
; Part 4 -- Kong
;

                STOREBYTE 31, LineCtr

                ; skip lines above Kong

IN_KongSkipHi_1:SUBROUTINE
                ldy     IN_KongSkipLines
.loop           sta     WSYNC
                dey
                bpl     .loop

                ; draw Kong

IN_DrawKong_1:  ldx     IN_KongHPos
                B48_DELAY

                jsr     IN_RAMCode

;
; Part 5 -- Girder below Kong
;

IN_GirderLo_1:  lda     IN_GirderColor
                sta     COLUPF
                ldx     #0
                stx     GRP0
                stx     GRP1
                stx     GRP0
                dex
                stx     PF1
                stx     PF2

                jsr     IN_DrawGirders

;
; Part 6 -- Ladder below Kong
;

IN_Ladder_1:    SUBROUTINE

                ; adjust player 1

                lda     #$10
                sta     HMP1
                sta     WSYNC
                sta     HMOVE

                ; draw ladder

                ldy     #46
.loop           lda     #%00000000
                sta     VDELP1
                ldx     #%10000001
                stx     NUSIZ1
                sta     PF1
                sta     PF2

                tya
                and     #3
                bne     .mask
                ldx     #%11111111
.mask           lda     IN_PaulineMask
                beq     .write
                ldx     #%00000000
.write          stx     GRP1
                sta     WSYNC
                dey
                cpy     #1
                bne     .loop

                sta     COLUP1

                ; clear remaining lines

                sec
                lda     #32/2
                sbc     IN_GirderOffset
                asl
                tay

                WAIT_Y_LINES

; --------------------------------------------------------------------
;       IN Overscan
; --------------------------------------------------------------------

IN_Overscan:    lda     #%00000010
                sta     WSYNC                   ; VBLANK on
                sta     VBLANK

                lda     #0                      ; clear channels
                sta     GRP0
                sta     GRP1
                sta     PF1
                sta     PF2

                lda     #35                     ; set timer to skip OVERSCAN
                sta     TIM64T

                CHECK_RESET

                inc     FrameCtr

                ; check for select switch pressed

IN_CheckSelect: SUBROUTINE
                and     #%00000001              ; A = SWCHA>>1
                tax
                bne     .write
                lda     IN_LevelSelect
                beq     .write
                lda     Level_Round             ; Round = 0
                cmp     #$90
                bcs     .skip
                adc     #$10
.skip           sta     Level_Round
.write          stx     IN_LevelSelect

                ; exit to intermission

IN_CheckExit:   lda     IN_KongAnimState
                cmp     #45+1
                bcc     IN_OverscanEnd
                jmp     Bank7_Intermission

IN_OverscanEnd: lda     INTIM
                bne     IN_OverscanEnd
                jmp     IN_VSync

; --------------------------------------------------------------------
;       IN Subroutines
; --------------------------------------------------------------------

                ; moved to data section

;IN_KongColors: SUBROUTINE
;               lda     FrameCtr
;               and     #%00000001
;               tax
;               lda     .KongCol,x
;               sta     COLUP0
;               sta     COLUP1
;               rts
;
;.KongCol       BYTE    KONG_COL_0, KONG_COL_1

; --------------------------------------------------------------------

IN_DrawGirders: SUBROUTINE
                ldy     IN_GirderStart
                dey
                bmi     .end
                cpy     IN_GirderEnd
                bmi     .end

.loop           sta     WSYNC
                tya
                and     #7
                clc
                adc     IN_GirderTab,y
                tax
                lda     IN_GirderCOL,x
                sta     COLUPF
                lda     IN_GirderPF1_L,x
                sta     PF1
                lda     IN_GirderPF2_L,x
                sta     PF2
                dey
                nop
                nop
                nop
                lda     IN_GirderPF2_R,x
                sta     PF2
                lda     IN_GirderPF1_R,x
                sta     PF1
                dex
                cpy     IN_GirderEnd
                bpl     .loop
.end            rts

; --------------------------------------------------------------------

IN_Bitmap48:    SUBROUTINE
.loop           ldy     LineCtr

                lda     $0000,y                 ; 1. character
                sta     GRP0

                lda     $0000,y                 ; background
                sta     CXCLR                   ; disabled
                cmp     $C5

                lda     $0000,y                 ; 2. character
                sta     GRP1
                lda     $0000,y                 ; 3. character
                sta     GRP0
                lda     $0000,y                 ; 4. character
                sta     Temp
                lda     $0000,y                 ; 5. character
                tax
                lda     $0000,y                 ; 6. character
                nop
                ldy     Temp
                sty     GRP1
                stx     GRP0
                sta     GRP1
                sta     GRP0

                dec     LineCtr
                bpl     .loop
                rts
IN_Bitmap48End:


; ********************************************************************
;
;       Data Section
;
; ********************************************************************

; --------------------------------------------------------------------
;       Macros
; --------------------------------------------------------------------

        MAC IN_MAKE_ANIM_STEP

IN_ANIM_{1}_HVAL SET {2}
IN_ANIM_{1}_VVAL SET {3}
IN_ANIM_{1}_TVAL SET {4}
IN_ANIM_{1}_GVAL SET {5}

        ENDM

        MAC IN_MAKE_ANIM_ARRAY

                BYTE IN_ANIM_00_{1},IN_ANIM_01_{1},IN_ANIM_02_{1},IN_ANIM_03_{1}
                BYTE IN_ANIM_04_{1},IN_ANIM_05_{1},IN_ANIM_06_{1},IN_ANIM_07_{1}
                BYTE IN_ANIM_08_{1},IN_ANIM_09_{1},IN_ANIM_10_{1},IN_ANIM_11_{1}
                BYTE IN_ANIM_12_{1},IN_ANIM_13_{1},IN_ANIM_14_{1},IN_ANIM_15_{1}
                BYTE IN_ANIM_16_{1},IN_ANIM_17_{1},IN_ANIM_18_{1},IN_ANIM_19_{1}
                BYTE IN_ANIM_20_{1},IN_ANIM_21_{1},IN_ANIM_22_{1},IN_ANIM_23_{1}
                BYTE IN_ANIM_24_{1},IN_ANIM_25_{1},IN_ANIM_26_{1},IN_ANIM_27_{1}
                BYTE IN_ANIM_28_{1},IN_ANIM_29_{1},IN_ANIM_30_{1},IN_ANIM_31_{1}
                BYTE IN_ANIM_32_{1},IN_ANIM_33_{1},IN_ANIM_34_{1},IN_ANIM_35_{1}
                BYTE IN_ANIM_36_{1},IN_ANIM_37_{1},IN_ANIM_38_{1},IN_ANIM_39_{1}
                BYTE IN_ANIM_40_{1},IN_ANIM_41_{1},IN_ANIM_42_{1},IN_ANIM_43_{1}
                BYTE IN_ANIM_44_{1},IN_ANIM_45_{1}

        ENDM

        MAC IN_MAKE_B48_LOBYTE_TABLE

                BYTE <{1}_GRP0_0,<{1}_GRP1_0,<{1}_GRP2_0,<{1}_GRP3_0,<{1}_GRP4_0,<{1}_GRP5_0
                BYTE <{1}_GRP0_1,<{1}_GRP1_1,<{1}_GRP2_1,<{1}_GRP3_1,<{1}_GRP4_1,<{1}_GRP5_1

        ENDM

        MAC IN_MAKE_B48_HIBYTE_TABLE

                BYTE >{1}_GRP0_0,>{1}_GRP1_0,>{1}_GRP2_0,>{1}_GRP3_0,>{1}_GRP4_0,>{1}_GRP5_0
                BYTE >{1}_GRP0_1,>{1}_GRP1_1,>{1}_GRP2_1,>{1}_GRP3_1,>{1}_GRP4_1,>{1}_GRP5_1

        ENDM

; --------------------------------------------------------------------
;       IN Tables
; --------------------------------------------------------------------

; Kong animation graphics tables

IN_KongGRP_L:   ; lo-bytes
IN_KongClimb0_L:IN_MAKE_B48_LOBYTE_TABLE IN_KongClimb0
IN_KongClimb1_L:IN_MAKE_B48_LOBYTE_TABLE IN_KongClimb1
IN_KongClimb2_L:IN_MAKE_B48_LOBYTE_TABLE IN_KongClimb2
IN_KongClimb3_L:IN_MAKE_B48_LOBYTE_TABLE IN_KongClimb3
IN_KongJump_L:  IN_MAKE_B48_LOBYTE_TABLE IN_KongJump
IN_KongStand_L: IN_MAKE_B48_LOBYTE_TABLE IN_KongStand
IN_KongGrin_L:  IN_MAKE_B48_LOBYTE_TABLE IN_KongGrin

IN_KongGRP_H:   ; hi-bytes
IN_KongClimb0_H:IN_MAKE_B48_HIBYTE_TABLE IN_KongClimb0
IN_KongClimb1_H:IN_MAKE_B48_HIBYTE_TABLE IN_KongClimb1
IN_KongClimb2_H:IN_MAKE_B48_HIBYTE_TABLE IN_KongClimb2
IN_KongClimb3_H:IN_MAKE_B48_HIBYTE_TABLE IN_KongClimb3
IN_KongJump_H:  IN_MAKE_B48_HIBYTE_TABLE IN_KongJump
IN_KongStand_H: IN_MAKE_B48_HIBYTE_TABLE IN_KongStand
IN_KongGrin_H:  IN_MAKE_B48_HIBYTE_TABLE IN_KongGrin

; Kong animation tables

IN_KONG_CLIMB0  = (0*12)+5
IN_KONG_CLIMB1  = (1*12)+5
IN_KONG_CLIMB2  = (2*12)+5
IN_KONG_CLIMB3  = (3*12)+5
IN_KONG_JUMP    = (4*12)+5
IN_KONG_STAND   = (5*12)+5
IN_KONG_GRIN    = (6*12)+5

IN_KONG_OFF     = %00000000
IN_KONG_LOOP    = %11111111

                ; --------------------------------------------------------------------
                ;
                ; original arcade 60Hz jump (left) Y-offsets ( 32 frames )
                ;
                ;   1, 0, 1, 0, 1, 0, 1, 0 ; 31 - 24
                ;   1, 0, 0, 0, 1, 0, 0, 0 ; 23 - 16
                ;   0, 0,-1, 0, 0, 0,-1, 0 ; 15 -  8
                ;  -1, 0,-1, 0,-1, 0,-1, 0 ;  7 -  0
                ;
                ; 30Hz adapted jump (left) Y-offsets ( 16 double frames air time )
                ;
                ;   1, 1, 1, 1, 1, 0, 1, 0 ;  15 - 8
                ;   0,-1, 0,-1,-1,-1,-1,-1 ;   7 - 0
                ;
                ; --------------------------------------------------------------------

                ;                 step   h-move   v-move     time      graphics
                ;
                ; - [ start ] --------------------------------------------------------
                IN_MAKE_ANIM_STEP  00,      0,       0,     32-1,     IN_KONG_OFF
                ;
                ; - [ climb ] --------------------------------------------------------
                IN_MAKE_ANIM_STEP  01,      0,       1,      4-1,     IN_KONG_CLIMB0 ;     <-+
                IN_MAKE_ANIM_STEP  02,      0,       1,      4-1,     IN_KONG_CLIMB1 ;       |
                IN_MAKE_ANIM_STEP  03,      0,     1+1,      2-1,     IN_KONG_LOOP   ; loop -+ (x2)
                ;
                IN_MAKE_ANIM_STEP  04,      0,       1,      4-1,     IN_KONG_CLIMB2
                IN_MAKE_ANIM_STEP  05,      0,       1,      4-1,     IN_KONG_CLIMB3
                IN_MAKE_ANIM_STEP  06,      0,       1,      4-1,     IN_KONG_CLIMB0
                ;
                IN_MAKE_ANIM_STEP  07,      0,       1,      4-1,     IN_KONG_CLIMB3 ;     <-+
                IN_MAKE_ANIM_STEP  08,      0,       1,      4-1,     IN_KONG_CLIMB2 ;       |
                IN_MAKE_ANIM_STEP  09,      0,     7+1,      2-1,     IN_KONG_LOOP   ; loop -+ (x2)
                ;
                IN_MAKE_ANIM_STEP  10,      0,       1,      4-1,     IN_KONG_CLIMB1 ;     <-+
                IN_MAKE_ANIM_STEP  11,      0,       1,      4-1,     IN_KONG_CLIMB2 ;       |
                IN_MAKE_ANIM_STEP  12,      0,    10+1,      2-1,     IN_KONG_LOOP   ; loop -+ (x2)
                ;
                IN_MAKE_ANIM_STEP  13,      0,       1,      4-1,     IN_KONG_CLIMB1 ;     <-+
                IN_MAKE_ANIM_STEP  14,      0,       1,      4-1,     IN_KONG_CLIMB0 ;       |
                IN_MAKE_ANIM_STEP  15,      0,    13+1,      2-1,     IN_KONG_LOOP   ; loop -+ (x2)
                ;
                IN_MAKE_ANIM_STEP  16,      0,       1,      4-1,     IN_KONG_CLIMB3
                IN_MAKE_ANIM_STEP  17,      0,       1,      4-1,     IN_KONG_CLIMB0
                IN_MAKE_ANIM_STEP  18,      0,       1,      4-1,     IN_KONG_CLIMB1
                ;
                IN_MAKE_ANIM_STEP  19,      0,       1,      4-1,     IN_KONG_CLIMB2 ;     <-+
                IN_MAKE_ANIM_STEP  20,      0,       1,      4-1,     IN_KONG_CLIMB3 ;       |
                IN_MAKE_ANIM_STEP  21,      0,    19+1,      2-1,     IN_KONG_LOOP   ; loop -+ (x2)
                ;
                IN_MAKE_ANIM_STEP  22,      0,       1,      4-1,     IN_KONG_CLIMB0
                IN_MAKE_ANIM_STEP  23,      0,       1,      4-1,     IN_KONG_CLIMB1
                IN_MAKE_ANIM_STEP  24,      0,       1,      4-1,     IN_KONG_CLIMB2
                IN_MAKE_ANIM_STEP  25,      0,       1,      4-1,     IN_KONG_CLIMB1
                ;
                ; - [ wait ] ---------------------------------------------------------
                IN_MAKE_ANIM_STEP  26,      0,       0,     17-1,     IN_KONG_CLIMB1
                ;
                ; - [ jump up ] ------------------------------------------------------
                IN_MAKE_ANIM_STEP  27,      0,    -114,      1-1,     IN_KONG_JUMP
                IN_MAKE_ANIM_STEP  28,      0,      -1,      4-1,     IN_KONG_JUMP
                IN_MAKE_ANIM_STEP  29,      0,       0,      2-1,     IN_KONG_JUMP
                IN_MAKE_ANIM_STEP  30,      0,       1,      3-1,     IN_KONG_JUMP
                IN_MAKE_ANIM_STEP  31,      0,       1,      1-1,     IN_KONG_STAND
                ;
                ; - [ wait ] ---------------------------------------------------------
                IN_MAKE_ANIM_STEP  32,      0,       0,     16-1,     IN_KONG_STAND
                ;
                ; - [ jump left ] ----------------------------------------------------
                IN_MAKE_ANIM_STEP  33,     -1,      -1,      5-1,     IN_KONG_STAND  ;     <-+
                IN_MAKE_ANIM_STEP  34,     -1,       0,      1-1,     IN_KONG_STAND  ;       |
                IN_MAKE_ANIM_STEP  35,     -1,      -1,      1-1,     IN_KONG_STAND  ;       |
                IN_MAKE_ANIM_STEP  36,     -1,       0,      1-1,     IN_KONG_STAND  ;       |
                IN_MAKE_ANIM_STEP  37,     -1,       1,      1-1,     IN_KONG_STAND  ;       |
                IN_MAKE_ANIM_STEP  38,     -1,       0,      1-1,     IN_KONG_STAND  ;       |
                IN_MAKE_ANIM_STEP  39,     -1,       1,      5-1,     IN_KONG_STAND  ;       |
                IN_MAKE_ANIM_STEP  40,      0,       0,      1-1,     IN_KONG_STAND  ;       |
                IN_MAKE_ANIM_STEP  41,      0,    33+1,      3-1,     IN_KONG_LOOP   ; loop -+ (x3)
                ;
                ; - [ grin ] ---------------------------------------------------------
                IN_MAKE_ANIM_STEP  42,      0,       0,     16-1,     IN_KONG_STAND
                IN_MAKE_ANIM_STEP  43,      0,       0,     60-1,     IN_KONG_GRIN
                IN_MAKE_ANIM_STEP  44,      0,       0,     11-1,     IN_KONG_STAND
                ;
                ; - [ end ] ----------------------------------------------------------
                IN_MAKE_ANIM_STEP  45,      0,       0,      255,     IN_KONG_STAND

IN_KongHTab:    IN_MAKE_ANIM_ARRAY HVAL
IN_KongVTab:    IN_MAKE_ANIM_ARRAY VVAL
IN_KongTimeTab: IN_MAKE_ANIM_ARRAY TVAL
IN_KongGfxTab:  IN_MAKE_ANIM_ARRAY GVAL

; 48bit bitmap GRP load instruction offsets

IN_B48Offsets:  BYTE    IN_RAM_B48Char0,IN_RAM_B48Char1,IN_RAM_B48Char2,IN_RAM_B48Char3,IN_RAM_B48Char4,IN_RAM_B48Char5

; per line girder index

IN_GirderTab:   DS.B    8, 1*8
                DS.B    8, 2*8
                DS.B    8, 2*8
                DS.B    8, 2*8
                DS.B    8, 2*8
                DS.B    8, 2*8
                DS.B    8, 0*8
                DS.B    8, 2*8
                DS.B    8, 2*8
                DS.B    8, 2*8

; --------------------------------------------------------------------
;       IN Graphics
; --------------------------------------------------------------------

                ; ALIGN $0010

IN_GirderPF1_L: BYTE    %00000000 ; |        |  7
                BYTE    %00000000 ; |        |  6
                BYTE    %00000000 ; |        |  5
                BYTE    %00000000 ; |        |  4
                BYTE    %00000000 ; |        |  3
                BYTE    %00000000 ; |        |  2
                BYTE    %00000000 ; |        |  1
                BYTE    %00000000 ; |        |  0

                BYTE    %11111111 ; |XXXXXXXX|  7
                BYTE    %11111111 ; |XXXXXXXX|  6
                BYTE    %10001000 ; |X   X   |  5
                BYTE    %11011101 ; |XX XXX X|  4
                BYTE    %01110111 ; | XXX XXX|  3
                BYTE    %00100010 ; |  X   X |  2
                BYTE    %11111111 ; |XXXXXXXX|  1
                BYTE    %11111111 ; |XXXXXXXX|  0

IN_GirderPF2_L: BYTE    %11110000 ; |XXXX    |  7
                BYTE    %11110000 ; |XXXX    |  6
                BYTE    %00010000 ; |   X    |  5
                BYTE    %10110000 ; |X XX    |  4
                BYTE    %11100000 ; |XXX     |  3
                BYTE    %01000000 ; | X      |  2
                BYTE    %11110000 ; |XXXX    |  1
                BYTE    %11110000 ; |XXXX    |  0

                BYTE    %11111111 ; |XXXXXXXX|  7
                BYTE    %11111111 ; |XXXXXXXX|  6
                BYTE    %00010001 ; |   X   X|  5
                BYTE    %10111011 ; |X XXX XX|  4
                BYTE    %11101110 ; |XXX XXX |  3
                BYTE    %01000100 ; | X   X  |  2
                BYTE    %11111111 ; |XXXXXXXX|  1
                BYTE    %11111111 ; |XXXXXXXX|  0

IN_GirderPF2_R: BYTE    %11110000 ; |XXXX    |  7
                BYTE    %11110000 ; |XXXX    |  6
                BYTE    %10000000 ; |X       |  5
                BYTE    %11010000 ; |XX X    |  4
                BYTE    %01110000 ; | XXX    |  3
                BYTE    %00100000 ; |  X     |  2
                BYTE    %11110000 ; |XXXX    |  1
                BYTE    %11110000 ; |XXXX    |  0

                BYTE    %11111111 ; |XXXXXXXX|  7
                BYTE    %11111111 ; |XXXXXXXX|  6
                BYTE    %10001000 ; |X   X   |  5
                BYTE    %11011101 ; |XX XXX X|  4
                BYTE    %01110111 ; | XXX XXX|  3
                BYTE    %00100010 ; |  X   X |  2
                BYTE    %11111111 ; |XXXXXXXX|  1
                BYTE    %11111111 ; |XXXXXXXX|  0

IN_GirderPF1_R: BYTE    %00000000 ; |        |  7
                BYTE    %00000000 ; |        |  6
                BYTE    %00000000 ; |        |  5
                BYTE    %00000000 ; |        |  4
                BYTE    %00000000 ; |        |  3
                BYTE    %00000000 ; |        |  2
                BYTE    %00000000 ; |        |  1
                BYTE    %00000000 ; |        |  0

                BYTE    %11111111 ; |XXXXXXXX|  7
                BYTE    %11111111 ; |XXXXXXXX|  6
                BYTE    %00010001 ; |   X   X|  5
                BYTE    %10111011 ; |X XXX XX|  4
                BYTE    %11101110 ; |XXX XXX |  3
                BYTE    %01000100 ; | X   X  |  2
                BYTE    %11111111 ; |XXXXXXXX|  1
                BYTE    %11111111 ; |XXXXXXXX|  0

IN_GirderCOL:   BYTE    IN_GIRDER_COL_0 ; 7
                BYTE    IN_GIRDER_COL_1 ; 6
                BYTE    IN_GIRDER_COL_1 ; 5
                BYTE    IN_GIRDER_COL_1 ; 4
                BYTE    IN_GIRDER_COL_1 ; 3
                BYTE    IN_GIRDER_COL_1 ; 2
                BYTE    IN_GIRDER_COL_0 ; 1
                BYTE    IN_GIRDER_COL_1 ; 0

                BYTE    IN_GIRDER_COL_0 ; 7
                BYTE    IN_GIRDER_COL_1 ; 6
                BYTE    IN_GIRDER_COL_1 ; 5
                BYTE    IN_GIRDER_COL_1 ; 4
                BYTE    IN_GIRDER_COL_1 ; 3
                BYTE    IN_GIRDER_COL_1 ; 2
                BYTE    IN_GIRDER_COL_0 ; 1
                BYTE    IN_GIRDER_COL_1 ; 0

IN_EmptySpace:  DS.B    32-24, $00 ; 24 bytes from next table

; ball behind Kong

IN_KongENABL:   BYTE    $00,$00,$00,$00,$00,$00,$00,$00
                BYTE    $00,$00,$00,$00,$00,$00,$00,$00
                BYTE    $00,$00,$00,$00,$00,$00,$00,$00
                BYTE    $00,$00,$02,$02
              ; BYTE    $00,$00,$00,$00

; playfield bitmap behind Kong

IN_KongPF1:     BYTE    $00,$00,$00,$00,$00,$00,$00,$00
                BYTE    $00,$00,$00,$00,$00,$00,$00,$00
                BYTE    $00,$00,$00,$0C,$1E,$1E,$1E,$1E
                BYTE    $00,$00,$0C,$0C,$00,$00,$00,$00

;
; Pauline
;

IN_Pauline_GRP_0:

                BYTE    %00000110 ; |     XX |  23
                BYTE    %01100100 ; | XX  X  |  22
                BYTE    %01000000 ; | X      |  21
                BYTE    %11100000 ; |XXX     |  20
                BYTE    %00001110 ; |    XXX |  19
                BYTE    %00010000 ; |   X    |  18
                BYTE    %00100000 ; |  X     |  17
                BYTE    %01000000 ; | X      |  16
                BYTE    %00000001 ; |       X|  15
                BYTE    %00000001 ; |       X|  14
                BYTE    %00011000 ; |   XX   |  13
                BYTE    %00000001 ; |       X|  12
                BYTE    %00000001 ; |       X|  11
                BYTE    %11000000 ; |XX      |  10
                BYTE    %01100000 ; | XX     |   9
                BYTE    %11110000 ; |XXXX    |   8
                BYTE    %01110000 ; | XXX    |   7
                BYTE    %01001000 ; | X  X   |   6
                BYTE    %10010010 ; |X  X  X |   5
                BYTE    %00011000 ; |   XX   |   4
                BYTE    %00011111 ; |   XXXXX|   3
                BYTE    %00001110 ; |    XXX |   2
              ; BYTE    %00000000 ; |        |   1
              ; BYTE    %00000000 ; |        |   0

IN_Pauline_GRP_1:

                BYTE    %00000000 ; |        |  23
                BYTE    %00000000 ; |        |  22
                BYTE    %00001110 ; |    XXX |  21
                BYTE    %00011110 ; |   XXXX |  20
                BYTE    %00110000 ; |  XX    |  19
                BYTE    %01101110 ; | XX XXX |  18
                BYTE    %11011110 ; |XX XXXX |  17
                BYTE    %10111110 ; |X XXXXX |  16
                BYTE    %01111110 ; | XXXXXX |  15
                BYTE    %00111110 ; |  XXXXX |  14
                BYTE    %00000100 ; |     X  |  13
                BYTE    %00011110 ; |   XXXX |  12
                BYTE    %00011110 ; |   XXXX |  11
                BYTE    %00011100 ; |   XXX  |  10
                BYTE    %00011100 ; |   XXX  |   9
                BYTE    %00001100 ; |    XX  |   8
                BYTE    %00001110 ; |    XXX |   7
                BYTE    %00000110 ; |     XX |   6
                BYTE    %00001101 ; |    XX X|   5
                BYTE    %00000110 ; |     XX |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

IN_Pauline_COL_0:

                BYTE    COL_82 ; |181AA7|  23
                BYTE    COL_82 ; |181AA7|  22
                BYTE    COL_82 ; |181AA7|  21
                BYTE    COL_82 ; |181AA7|  20
                BYTE    COL_0E ; |ECECEC|  19
                BYTE    COL_0E ; |ECECEC|  18
                BYTE    COL_0E ; |ECECEC|  17
                BYTE    COL_0E ; |ECECEC|  16
                BYTE    COL_0E ; |ECECEC|  15
                BYTE    COL_0E ; |ECECEC|  14
                BYTE    COL_82 ; |181AA7|  13
                BYTE    COL_0E ; |ECECEC|  12
                BYTE    COL_0E ; |ECECEC|  11
                BYTE    COL_36 ; |C66C3A|  10
                BYTE    COL_36 ; |C66C3A|   9
                BYTE    COL_36 ; |C66C3A|   8
                BYTE    COL_36 ; |C66C3A|   7
                BYTE    COL_36 ; |C66C3A|   6
                BYTE    COL_36 ; |C66C3A|   5
                BYTE    COL_36 ; |C66C3A|   4
                BYTE    COL_36 ; |C66C3A|   3
                BYTE    COL_36 ; |C66C3A|   2
              ; BYTE    COL_00 ; |000000|   1
              ; BYTE    COL_00 ; |000000|   0

IN_Pauline_COL_1:

                BYTE    COL_00 ; |000000|  23
                BYTE    COL_00 ; |000000|  22
                BYTE    COL_5A ; |D46CC3|  21
                BYTE    COL_5A ; |D46CC3|  20
                BYTE    COL_5A ; |D46CC3|  19
                BYTE    COL_5A ; |D46CC3|  18
                BYTE    COL_5A ; |D46CC3|  17
                BYTE    COL_5A ; |D46CC3|  16
                BYTE    COL_5A ; |D46CC3|  15
                BYTE    COL_5A ; |D46CC3|  14
                BYTE    COL_5A ; |D46CC3|  13
                BYTE    COL_5A ; |D46CC3|  12
                BYTE    COL_5A ; |D46CC3|  11
                BYTE    COL_5A ; |D46CC3|  10
                BYTE    COL_5A ; |D46CC3|   9
                BYTE    COL_0E ; |ECECEC|   8
                BYTE    COL_0E ; |ECECEC|   7
                BYTE    COL_0E ; |ECECEC|   6
                BYTE    COL_0E ; |ECECEC|   5
                BYTE    COL_0E ; |ECECEC|   4
                BYTE    COL_00 ; |000000|   3
                BYTE    COL_00 ; |000000|   2
                BYTE    COL_00 ; |000000|   1
                BYTE    COL_00 ; |000000|   0

;
; Kong climbing -- pose 0
;

                ALIGN   $0100

IN_KongClimb0_GRP4_0:

                BYTE    %00000000 ; |        |  35
                BYTE    %00000000 ; |        |  34
                BYTE    %00000000 ; |        |  33
                BYTE    %00000000 ; |        |  32
                BYTE    %11000000 ; |XX      |  31
                BYTE    %10000000 ; |X       |  30
                BYTE    %10000000 ; |X       |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000111 ; |     XXX|  27
                BYTE    %00001111 ; |    XXXX|  26
                BYTE    %00000111 ; |     XXX|  25
                BYTE    %00100011 ; |  X   XX|  24
                BYTE    %00100011 ; |  X   XX|  23
                BYTE    %01100111 ; | XX  XXX|  22
                BYTE    %11111111 ; |XXXXXXXX|  21
                BYTE    %11111111 ; |XXXXXXXX|  20
                BYTE    %11111111 ; |XXXXXXXX|  19
                BYTE    %11111111 ; |XXXXXXXX|  18
                BYTE    %11111111 ; |XXXXXXXX|  17
                BYTE    %11111111 ; |XXXXXXXX|  16
                BYTE    %11111110 ; |XXXXXXX |  15
                BYTE    %00000010 ; |      X |  14
                BYTE    %00000000 ; |        |  13
                BYTE    %00000000 ; |        |  12
                BYTE    %00000111 ; |     XXX|  11
                BYTE    %00111000 ; |  XXX   |  10
                BYTE    %11000000 ; |XX      |   9
                BYTE    %00000000 ; |        |   8
                BYTE    %00000000 ; |        |   7
                BYTE    %00000000 ; |        |   6
                BYTE    %00000000 ; |        |   5
               ;BYTE    %00000000 ; |        |   4
               ;BYTE    %00000000 ; |        |   3
               ;BYTE    %00000000 ; |        |   2
               ;BYTE    %00000000 ; |        |   1
               ;BYTE    %00000000 ; |        |   0

IN_KongClimb0_GRP5_0:

                BYTE    %00000000 ; |        |  35
                BYTE    %00000000 ; |        |  34
                BYTE    %00000000 ; |        |  33
                BYTE    %00000000 ; |        |  32
                BYTE    %00000000 ; |        |  31
                BYTE    %00000000 ; |        |  30
                BYTE    %00000000 ; |        |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %10000000 ; |X       |  25
                BYTE    %10000000 ; |X       |  24
                BYTE    %10000000 ; |X       |  23
                BYTE    %11100000 ; |XXX     |  22
                BYTE    %11100000 ; |XXX     |  21
                BYTE    %11000000 ; |XX      |  20
                BYTE    %11000000 ; |XX      |  19
                BYTE    %10000000 ; |X       |  18
                BYTE    %10000000 ; |X       |  17
                BYTE    %00000000 ; |        |  16
                BYTE    %00000000 ; |        |  15
                BYTE    %00010000 ; |   X    |  14
                BYTE    %00100000 ; |  X     |  13
                BYTE    %11000000 ; |XX      |  12
                BYTE    %00000000 ; |        |  11
                BYTE    %00000000 ; |        |  10
                BYTE    %00000000 ; |        |   9
                BYTE    %00000000 ; |        |   8
              ; BYTE    %00000000 ; |        |   7
              ; BYTE    %00000000 ; |        |   6
              ; BYTE    %00000000 ; |        |   5
              ; BYTE    %00000000 ; |        |   4
              ; BYTE    %00000000 ; |        |   3
              ; BYTE    %00000000 ; |        |   2
              ; BYTE    %00000000 ; |        |   1
              ; BYTE    %00000000 ; |        |   0

IN_KongClimb0_GRP0_1:

                BYTE    %00000000 ; |        |  35
                BYTE    %00000000 ; |        |  34
                BYTE    %00000000 ; |        |  33
                BYTE    %00000000 ; |        |  32
                BYTE    %00000000 ; |        |  31
                BYTE    %00000000 ; |        |  30
                BYTE    %00000000 ; |        |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000001 ; |       X|  27
                BYTE    %00000011 ; |      XX|  26
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
                BYTE    %00000001 ; |       X|  13
                BYTE    %00000001 ; |       X|  12
                BYTE    %00000011 ; |      XX|  11
                BYTE    %00000011 ; |      XX|  10
                BYTE    %00000011 ; |      XX|   9
                BYTE    %00000011 ; |      XX|   8
                BYTE    %00000011 ; |      XX|   7
                BYTE    %00000001 ; |       X|   6
                BYTE    %00000001 ; |       X|   5
              ; BYTE    %00000000 ; |        |   4
              ; BYTE    %00000000 ; |        |   3
              ; BYTE    %00000000 ; |        |   2
              ; BYTE    %00000000 ; |        |   1
              ; BYTE    %00000000 ; |        |   0

IN_KongClimb0_GRP0_0:

                BYTE    %00000000 ; |        |  35
                BYTE    %00000000 ; |        |  34
                BYTE    %00000000 ; |        |  33
                BYTE    %00000000 ; |        |  32
                BYTE    %00000000 ; |        |  31
                BYTE    %00000000 ; |        |  30
                BYTE    %00000000 ; |        |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %00000010 ; |      X |  25
                BYTE    %00000011 ; |      XX|  24
                BYTE    %00000001 ; |       X|  23
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

; --------------------------------------------------------------------

                ; moved from pose 2

IN_KongClimb2_GRP5_1:

                BYTE    %00000000 ; |        |  35
                BYTE    %00000000 ; |        |  34
                BYTE    %00000000 ; |        |  33
                BYTE    %00000000 ; |        |  32
                BYTE    %00000000 ; |        |  31
                BYTE    %00000000 ; |        |  30
                BYTE    %00000000 ; |        |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %00000000 ; |        |  25
                BYTE    %00000000 ; |        |  24
                BYTE    %00000000 ; |        |  23
                BYTE    %00000000 ; |        |  22
                BYTE    %00000000 ; |        |  21
                BYTE    %01100000 ; | XX     |  20
                BYTE    %00000000 ; |        |  19
                BYTE    %00000000 ; |        |  18
                BYTE    %11000000 ; |XX      |  17
                BYTE    %11100000 ; |XXX     |  16
                BYTE    %11110000 ; |XXXX    |  15
                BYTE    %11100000 ; |XXX     |  14
                BYTE    %11000000 ; |XX      |  13
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

; --------------------------------------------------------------------

IN_KongClimb0_GRP1_0:

                BYTE    %11111111 ; |XXXXXXXX|  35
                BYTE    %01111111 ; | XXXXXXX|  34
                BYTE    %10011111 ; |X  XXXXX|  33
                BYTE    %11000111 ; |XX   XXX|  32
                BYTE    %10000000 ; |X       |  31
                BYTE    %10000000 ; |X       |  30
                BYTE    %00000000 ; |        |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000001 ; |       X|  26
                BYTE    %00000011 ; |      XX|  25
                BYTE    %00000100 ; |     X  |  24
                BYTE    %00001000 ; |    X   |  23
                BYTE    %10010000 ; |X  X    |  22
                BYTE    %10000000 ; |X       |  21
                BYTE    %11000000 ; |XX      |  20
                BYTE    %10000000 ; |X       |  19
                BYTE    %10000000 ; |X       |  18
                BYTE    %10000000 ; |X       |  17
                BYTE    %10000000 ; |X       |  16
                BYTE    %10000000 ; |X       |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %00000000 ; |        |  13
                BYTE    %00000000 ; |        |  12
                BYTE    %00111001 ; |  XXX  X|  11
                BYTE    %01011110 ; | X XXXX |  10
                BYTE    %01001100 ; | X  XX  |   9
                BYTE    %00000110 ; |     XX |   8
                BYTE    %00000010 ; |      X |   7
                BYTE    %00000011 ; |      XX|   6
                BYTE    %00000001 ; |       X|   5
                BYTE    %00000001 ; |       X|   4
                BYTE    %10000001 ; |X      X|   3
                BYTE    %10000011 ; |X     XX|   2
                BYTE    %10000001 ; |X      X|   1
                BYTE    %11110111 ; |XXXX XXX|   0

IN_KongClimb0_GRP2_0:

                BYTE    %00000000 ; |        |  35
                BYTE    %10000000 ; |X       |  34
                BYTE    %11000000 ; |XX      |  33
                BYTE    %11000000 ; |XX      |  32
                BYTE    %00000000 ; |        |  31
                BYTE    %00000000 ; |        |  30
                BYTE    %00000000 ; |        |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %11001111 ; |XX  XXXX|  26
                BYTE    %11111111 ; |XXXXXXXX|  25
                BYTE    %00110000 ; |  XX    |  24
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
                BYTE    %10000001 ; |X      X|  12
                BYTE    %00000000 ; |        |  11
                BYTE    %00000000 ; |        |  10
                BYTE    %00000000 ; |        |   9
                BYTE    %00000000 ; |        |   8
                BYTE    %00000000 ; |        |   7
                BYTE    %00000000 ; |        |   6
                BYTE    %11000001 ; |XX     X|   5
                BYTE    %01100010 ; | XX   X |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %10000000 ; |X       |   2
                BYTE    %10000000 ; |X       |   1
                BYTE    %00000000 ; |        |   0

IN_KongClimb0_GRP3_0:

                BYTE    %10000001 ; |X      X|  35
                BYTE    %10000001 ; |X      X|  34
                BYTE    %10000001 ; |X      X|  33
                BYTE    %11111111 ; |XXXXXXXX|  32
                BYTE    %10011111 ; |X  XXXXX|  31
                BYTE    %10111111 ; |X XXXXXX|  30
                BYTE    %11111111 ; |XXXXXXXX|  29
                BYTE    %11111110 ; |XXXXXXX |  28
                BYTE    %00111000 ; |  XXX   |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %11000000 ; |XX      |  25
                BYTE    %00100000 ; |  X     |  24
                BYTE    %00010110 ; |   X XX |  23
                BYTE    %00001111 ; |    XXXX|  22
                BYTE    %00001111 ; |    XXXX|  21
                BYTE    %00011111 ; |   XXXXX|  20
                BYTE    %00011111 ; |   XXXXX|  19
                BYTE    %00001111 ; |    XXXX|  18
                BYTE    %00000111 ; |     XXX|  17
                BYTE    %00000001 ; |       X|  16
                BYTE    %00000000 ; |        |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %00000000 ; |        |  13
                BYTE    %00000000 ; |        |  12
                BYTE    %10000000 ; |X       |  11
                BYTE    %01100000 ; | XX     |  10
                BYTE    %00011111 ; |   XXXXX|   9
                BYTE    %00111111 ; |  XXXXXX|   8
                BYTE    %01000001 ; | X     X|   7
                BYTE    %01000001 ; | X     X|   6
                BYTE    %10000001 ; |X      X|   5
                BYTE    %11111111 ; |XXXXXXXX|   4
                BYTE    %10000001 ; |X      X|   3
                BYTE    %10000001 ; |X      X|   2
                BYTE    %10000001 ; |X      X|   1
                BYTE    %11111111 ; |XXXXXXXX|   0

;
; pose 0 GRP4_0, GRP5_0, GRP0_1 moved to top
;

                ALIGN   $0100

IN_KongClimb0_GRP1_1:

                BYTE    %11100000 ; |XXX     |  35
                BYTE    %11110000 ; |XXXX    |  34
                BYTE    %11111100 ; |XXXXXX  |  33
                BYTE    %11111111 ; |XXXXXXXX|  32
                BYTE    %10111111 ; |X XXXXXX|  31
                BYTE    %11111111 ; |XXXXXXXX|  30
                BYTE    %11111111 ; |XXXXXXXX|  29
                BYTE    %11111111 ; |XXXXXXXX|  28
                BYTE    %11111111 ; |XXXXXXXX|  27
                BYTE    %11111110 ; |XXXXXXX |  26
                BYTE    %11111100 ; |XXXXXX  |  25
                BYTE    %11111011 ; |XXXXX XX|  24
                BYTE    %11110111 ; |XXXX XXX|  23
                BYTE    %01101111 ; | XX XXXX|  22
                BYTE    %01111111 ; | XXXXXXX|  21
                BYTE    %10111111 ; |X XXXXXX|  20
                BYTE    %11111111 ; |XXXXXXXX|  19
                BYTE    %11111111 ; |XXXXXXXX|  18
                BYTE    %10111111 ; |X XXXXXX|  17
                BYTE    %11111111 ; |XXXXXXXX|  16
                BYTE    %11111111 ; |XXXXXXXX|  15
                BYTE    %11111111 ; |XXXXXXXX|  14
                BYTE    %11111111 ; |XXXXXXXX|  13
                BYTE    %11111111 ; |XXXXXXXX|  12
                BYTE    %11000110 ; |XX   XX |  11
                BYTE    %10100001 ; |X X    X|  10
                BYTE    %10110011 ; |X XX  XX|   9
                BYTE    %11111001 ; |XXXXX  X|   8
                BYTE    %11111101 ; |XXXXXX X|   7
                BYTE    %11111100 ; |XXXXXX  |   6
                BYTE    %11111110 ; |XXXXXXX |   5
                BYTE    %11111111 ; |XXXXXXXX|   4
                BYTE    %11111111 ; |XXXXXXXX|   3
                BYTE    %10111110 ; |X XXXXX |   2
                BYTE    %10111111 ; |X XXXXXX|   1
                BYTE    %11111111 ; |XXXXXXXX|   0

IN_KongClimb0_GRP2_1:

                BYTE    %00000000 ; |        |  35
                BYTE    %00000000 ; |        |  34
                BYTE    %01000000 ; | X      |  33
                BYTE    %11100000 ; |XXX     |  32
                BYTE    %11100000 ; |XXX     |  31
                BYTE    %11000000 ; |XX      |  30
                BYTE    %10000000 ; |X       |  29
                BYTE    %10000000 ; |X       |  28
                BYTE    %10000000 ; |X       |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %00000000 ; |        |  25
                BYTE    %11001111 ; |XX  XXXX|  24
                BYTE    %11111111 ; |XXXXXXXX|  23
                BYTE    %11111111 ; |XXXXXXXX|  22
                BYTE    %11111111 ; |XXXXXXXX|  21
                BYTE    %11111111 ; |XXXXXXXX|  20
                BYTE    %11111111 ; |XXXXXXXX|  19
                BYTE    %11111111 ; |XXXXXXXX|  18
                BYTE    %11111111 ; |XXXXXXXX|  17
                BYTE    %11111111 ; |XXXXXXXX|  16
                BYTE    %11111111 ; |XXXXXXXX|  15
                BYTE    %11111111 ; |XXXXXXXX|  14
                BYTE    %11111111 ; |XXXXXXXX|  13
                BYTE    %01111110 ; | XXXXXX |  12
                BYTE    %11111111 ; |XXXXXXXX|  11
                BYTE    %11111111 ; |XXXXXXXX|  10
                BYTE    %11111111 ; |XXXXXXXX|   9
                BYTE    %11111111 ; |XXXXXXXX|   8
                BYTE    %11111111 ; |XXXXXXXX|   7
                BYTE    %11111111 ; |XXXXXXXX|   6
                BYTE    %00111110 ; |  XXXXX |   5
                BYTE    %00011100 ; |   XXX  |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %10000000 ; |X       |   2
                BYTE    %10000000 ; |X       |   1
                BYTE    %00000000 ; |        |   0

IN_KongClimb0_GRP3_1:

                BYTE    %10000001 ; |X      X|  35
                BYTE    %10000001 ; |X      X|  34
                BYTE    %10000001 ; |X      X|  33
                BYTE    %11111111 ; |XXXXXXXX|  32
                BYTE    %10000000 ; |X       |  31
                BYTE    %10000001 ; |X      X|  30
                BYTE    %10000011 ; |X     XX|  29
                BYTE    %11000111 ; |XX   XXX|  28
                BYTE    %11111111 ; |XXXXXXXX|  27
                BYTE    %11111111 ; |XXXXXXXX|  26
                BYTE    %00111111 ; |  XXXXXX|  25
                BYTE    %11011111 ; |XX XXXXX|  24
                BYTE    %11101001 ; |XXX X  X|  23
                BYTE    %11110000 ; |XXXX    |  22
                BYTE    %11110000 ; |XXXX    |  21
                BYTE    %11111000 ; |XXXXX   |  20
                BYTE    %11100001 ; |XXX    X|  19
                BYTE    %11110000 ; |XXXX    |  18
                BYTE    %11111111 ; |XXXXXXXX|  17
                BYTE    %11111110 ; |XXXXXXX |  16
                BYTE    %11111111 ; |XXXXXXXX|  15
                BYTE    %11111111 ; |XXXXXXXX|  14
                BYTE    %11111111 ; |XXXXXXXX|  13
                BYTE    %11111111 ; |XXXXXXXX|  12
                BYTE    %01111111 ; | XXXXXXX|  11
                BYTE    %10011111 ; |X  XXXXX|  10
                BYTE    %11100000 ; |XXX     |   9
                BYTE    %11011111 ; |XX XXXXX|   8
                BYTE    %10000001 ; |X      X|   7
                BYTE    %10000001 ; |X      X|   6
                BYTE    %00000001 ; |       X|   5
                BYTE    %11111111 ; |XXXXXXXX|   4
                BYTE    %10000001 ; |X      X|   3
                BYTE    %10000001 ; |X      X|   2
                BYTE    %10000001 ; |X      X|   1
                BYTE    %11111111 ; |XXXXXXXX|   0

IN_KongClimb0_GRP4_1:

                BYTE    %00000000 ; |        |  35
                BYTE    %00000000 ; |        |  34
                BYTE    %00000000 ; |        |  33
                BYTE    %00000000 ; |        |  32
                BYTE    %11000000 ; |XX      |  31
                BYTE    %10000000 ; |X       |  30
                BYTE    %10000000 ; |X       |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %10000000 ; |X       |  27
                BYTE    %11000000 ; |XX      |  26
                BYTE    %11100000 ; |XXX     |  25
                BYTE    %11000000 ; |XX      |  24
                BYTE    %11000000 ; |XX      |  23
                BYTE    %11100100 ; |XXX  X  |  22
                BYTE    %11111000 ; |XXXXX   |  21
                BYTE    %11111000 ; |XXXXX   |  20
                BYTE    %01100001 ; | XX    X|  19
                BYTE    %00000010 ; |      X |  18
                BYTE    %11111100 ; |XXXXXX  |  17
                BYTE    %00000000 ; |        |  16
                BYTE    %00000001 ; |       X|  15
                BYTE    %11111101 ; |XXXXXX X|  14
                BYTE    %11111111 ; |XXXXXXXX|  13
                BYTE    %11111111 ; |XXXXXXXX|  12
                BYTE    %11111000 ; |XXXXX   |  11
                BYTE    %11000000 ; |XX      |  10
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

IN_KongClimb0_GRP5_1:

                BYTE    %00000000 ; |        |  35
                BYTE    %00000000 ; |        |  34
                BYTE    %00000000 ; |        |  33
                BYTE    %00000000 ; |        |  32
                BYTE    %00000000 ; |        |  31
                BYTE    %00000000 ; |        |  30
                BYTE    %00000000 ; |        |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %00000000 ; |        |  25
                BYTE    %00000000 ; |        |  24
                BYTE    %00000000 ; |        |  23
                BYTE    %11100000 ; |XXX     |  22
                BYTE    %00000000 ; |        |  21
                BYTE    %00000000 ; |        |  20
                BYTE    %11000000 ; |XX      |  19
                BYTE    %00000000 ; |        |  18
                BYTE    %01000000 ; | X      |  17
                BYTE    %11100000 ; |XXX     |  16
                BYTE    %11110000 ; |XXXX    |  15
                BYTE    %11100000 ; |XXX     |  14
                BYTE    %11000000 ; |XX      |  13
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

;
; Kong climbing -- pose 1
;

IN_KongClimb1_GRP0_0:

                BYTE    %00000000 ; |        |  35
                BYTE    %00000000 ; |        |  34
                BYTE    %00000000 ; |        |  33
                BYTE    %00000000 ; |        |  32
                BYTE    %00000000 ; |        |  31
                BYTE    %00000000 ; |        |  30
                BYTE    %00000000 ; |        |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000011 ; |      XX|  27
                BYTE    %00000001 ; |       X|  26
                BYTE    %00000001 ; |       X|  25
                BYTE    %00000000 ; |        |  24
                BYTE    %00000000 ; |        |  23
                BYTE    %00000000 ; |        |  22
                BYTE    %00000000 ; |        |  21
                BYTE    %00000100 ; |     X  |  20
                BYTE    %00000100 ; |     X  |  19
                BYTE    %00000010 ; |      X |  18
                BYTE    %00000001 ; |       X|  17
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
                BYTE    %00000011 ; |      XX|   6
                BYTE    %00000110 ; |     XX |   5
                BYTE    %00000000 ; |        |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

IN_KongClimb1_GRP1_0:

                BYTE    %10000001 ; |X      X|  35
                BYTE    %10000001 ; |X      X|  34
                BYTE    %10000001 ; |X      X|  33
                BYTE    %11111111 ; |XXXXXXXX|  32
                BYTE    %10000001 ; |X      X|  31
                BYTE    %10000001 ; |X      X|  30
                BYTE    %10000001 ; |X      X|  29
                BYTE    %11111111 ; |XXXXXXXX|  28
                BYTE    %11111001 ; |XXXXX  X|  27
                BYTE    %11111101 ; |XXXXXX X|  26
                BYTE    %11111111 ; |XXXXXXXX|  25
                BYTE    %01111111 ; | XXXXXXX|  24
                BYTE    %00011100 ; |   XXX  |  23
                BYTE    %00000000 ; |        |  22
                BYTE    %00000011 ; |      XX|  21
                BYTE    %00000100 ; |     X  |  20
                BYTE    %00001000 ; |    X   |  19
                BYTE    %00010000 ; |   X    |  18
                BYTE    %00000000 ; |        |  17
                BYTE    %10000000 ; |X       |  16
                BYTE    %10000000 ; |X       |  15
                BYTE    %10000000 ; |X       |  14
                BYTE    %10000000 ; |X       |  13
                BYTE    %10000000 ; |X       |  12
                BYTE    %00000000 ; |        |  11
                BYTE    %00000000 ; |        |  10
                BYTE    %00000000 ; |        |   9
                BYTE    %00000000 ; |        |   8
                BYTE    %00111001 ; |  XXX  X|   7
                BYTE    %11011110 ; |XX XXXX |   6
                BYTE    %00001100 ; |    XX  |   5
                BYTE    %00001110 ; |    XXX |   4
                BYTE    %00000010 ; |      X |   3
                BYTE    %00000111 ; |     XXX|   2
                BYTE    %00001101 ; |    XX X|   1
                BYTE    %00110111 ; |  XX XXX|   0

                ALIGN   $0100

IN_KongClimb1_GRP2_0:

                BYTE    %00000000 ; |        |  35
                BYTE    %00000000 ; |        |  34
                BYTE    %00000000 ; |        |  33
                BYTE    %00000000 ; |        |  32
                BYTE    %00000000 ; |        |  31
                BYTE    %00000001 ; |       X|  30
                BYTE    %00000011 ; |      XX|  29
                BYTE    %00000011 ; |      XX|  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %00000000 ; |        |  25
                BYTE    %00000000 ; |        |  24
                BYTE    %00000000 ; |        |  23
                BYTE    %11110011 ; |XXXX  XX|  22
                BYTE    %11111111 ; |XXXXXXXX|  21
                BYTE    %00001100 ; |    XX  |  20
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
                BYTE    %10000001 ; |X      X|   8
                BYTE    %00000000 ; |        |   7
                BYTE    %00000000 ; |        |   6
                BYTE    %00000000 ; |        |   5
                BYTE    %00000000 ; |        |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %11000001 ; |XX     X|   1
                BYTE    %01100010 ; | XX   X |   0

IN_KongClimb1_GRP3_0:

                BYTE    %10000001 ; |X      X|  35
                BYTE    %10000001 ; |X      X|  34
                BYTE    %10000001 ; |X      X|  33
                BYTE    %11111111 ; |XXXXXXXX|  32
                BYTE    %11111111 ; |XXXXXXXX|  31
                BYTE    %11111110 ; |XXXXXXX |  30
                BYTE    %11111001 ; |XXXXX  X|  29
                BYTE    %11100011 ; |XXX   XX|  28
                BYTE    %00000001 ; |       X|  27
                BYTE    %00000001 ; |       X|  26
                BYTE    %00000000 ; |        |  25
                BYTE    %00000000 ; |        |  24
                BYTE    %00000000 ; |        |  23
                BYTE    %10000000 ; |X       |  22
                BYTE    %11000000 ; |XX      |  21
                BYTE    %00100000 ; |  X     |  20
                BYTE    %00010110 ; |   X XX |  19
                BYTE    %00001111 ; |    XXXX|  18
                BYTE    %00001111 ; |    XXXX|  17
                BYTE    %00011111 ; |   XXXXX|  16
                BYTE    %00011111 ; |   XXXXX|  15
                BYTE    %00001111 ; |    XXXX|  14
                BYTE    %00000111 ; |     XXX|  13
                BYTE    %00000001 ; |       X|  12
                BYTE    %00000000 ; |        |  11
                BYTE    %00000000 ; |        |  10
                BYTE    %00000000 ; |        |   9
                BYTE    %00000000 ; |        |   8
                BYTE    %10000000 ; |X       |   7
                BYTE    %01100000 ; | XX     |   6
                BYTE    %00011111 ; |   XXXXX|   5
                BYTE    %00111111 ; |  XXXXXX|   4
                BYTE    %01000001 ; | X     X|   3
                BYTE    %01000001 ; | X     X|   2
                BYTE    %10000001 ; |X      X|   1
                BYTE    %11111111 ; |XXXXXXXX|   0

IN_KongClimb1_GRP4_0:

                BYTE    %00000000 ; |        |  35
                BYTE    %00000000 ; |        |  34
                BYTE    %00000000 ; |        |  33
                BYTE    %00000000 ; |        |  32
                BYTE    %00000000 ; |        |  31
                BYTE    %00000000 ; |        |  30
                BYTE    %00000000 ; |        |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %00000000 ; |        |  25
                BYTE    %00000000 ; |        |  24
                BYTE    %00000111 ; |     XXX|  23
                BYTE    %00001111 ; |    XXXX|  22
                BYTE    %01000111 ; | X   XXX|  21
                BYTE    %11000011 ; |XX    XX|  20
                BYTE    %10000011 ; |X     XX|  19
                BYTE    %01100111 ; | XX  XXX|  18
                BYTE    %11111111 ; |XXXXXXXX|  17
                BYTE    %11111111 ; |XXXXXXXX|  16
                BYTE    %11111111 ; |XXXXXXXX|  15
                BYTE    %11111111 ; |XXXXXXXX|  14
                BYTE    %11111111 ; |XXXXXXXX|  13
                BYTE    %11111111 ; |XXXXXXXX|  12
                BYTE    %11111110 ; |XXXXXXX |  11
                BYTE    %00000010 ; |      X |  10
                BYTE    %00000000 ; |        |   9
                BYTE    %00000000 ; |        |   8
                BYTE    %00000111 ; |     XXX|   7
                BYTE    %00111000 ; |  XXX   |   6
                BYTE    %11000000 ; |XX      |   5
                BYTE    %00000000 ; |        |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

IN_KongClimb1_GRP5_0 = IN_KongClimb0_GRP5_0 - 4

IN_KongClimb1_GRP0_1:

                BYTE    %00000000 ; |        |  35
                BYTE    %00000000 ; |        |  34
                BYTE    %00000000 ; |        |  33
                BYTE    %00000000 ; |        |  32
                BYTE    %00000000 ; |        |  31
                BYTE    %00000000 ; |        |  30
                BYTE    %00000000 ; |        |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000011 ; |      XX|  27
                BYTE    %00000001 ; |       X|  26
                BYTE    %00000001 ; |       X|  25
                BYTE    %00000000 ; |        |  24
                BYTE    %00000001 ; |       X|  23
                BYTE    %00000011 ; |      XX|  22
                BYTE    %00000111 ; |     XXX|  21
                BYTE    %00000011 ; |      XX|  20
                BYTE    %00000011 ; |      XX|  19
                BYTE    %00000001 ; |       X|  18
                BYTE    %00000000 ; |        |  17
                BYTE    %00000000 ; |        |  16
                BYTE    %00000000 ; |        |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %00000000 ; |        |  13
                BYTE    %00000000 ; |        |  12
                BYTE    %00000011 ; |      XX|  11
                BYTE    %00001111 ; |    XXXX|  10
                BYTE    %00011111 ; |   XXXXX|   9
                BYTE    %00111111 ; |  XXXXXX|   8
                BYTE    %01111111 ; | XXXXXXX|   7
                BYTE    %01111100 ; | XXXXX  |   6
                BYTE    %01111001 ; | XXXX  X|   5
                BYTE    %01111111 ; | XXXXXXX|   4
                BYTE    %00111111 ; |  XXXXXX|   3
                BYTE    %00011111 ; |   XXXXX|   2
                BYTE    %00000111 ; |     XXX|   1
                BYTE    %00000000 ; |        |   0

IN_KongClimb1_GRP1_1:

                BYTE    %10000001 ; |X      X|  35
                BYTE    %10000001 ; |X      X|  34
                BYTE    %10000001 ; |X      X|  33
                BYTE    %11111111 ; |XXXXXXXX|  32
                BYTE    %10000001 ; |X      X|  31
                BYTE    %10000001 ; |X      X|  30
                BYTE    %10000001 ; |X      X|  29
                BYTE    %11111111 ; |XXXXXXXX|  28
                BYTE    %00000001 ; |       X|  27
                BYTE    %10000001 ; |X      X|  26
                BYTE    %11000001 ; |XX     X|  25
                BYTE    %11100011 ; |XXX   XX|  24
                BYTE    %11111111 ; |XXXXXXXX|  23
                BYTE    %11111111 ; |XXXXXXXX|  22
                BYTE    %11111100 ; |XXXXXX  |  21
                BYTE    %11111011 ; |XXXXX XX|  20
                BYTE    %11110111 ; |XXXX XXX|  19
                BYTE    %11101111 ; |XXX XXXX|  18
                BYTE    %11111111 ; |XXXXXXXX|  17
                BYTE    %01111111 ; | XXXXXXX|  16
                BYTE    %11111111 ; |XXXXXXXX|  15
                BYTE    %11111111 ; |XXXXXXXX|  14
                BYTE    %10111111 ; |X XXXXXX|  13
                BYTE    %11111111 ; |XXXXXXXX|  12
                BYTE    %11111111 ; |XXXXXXXX|  11
                BYTE    %11111111 ; |XXXXXXXX|  10
                BYTE    %11111111 ; |XXXXXXXX|   9
                BYTE    %11111111 ; |XXXXXXXX|   8
                BYTE    %11000110 ; |XX   XX |   7
                BYTE    %00000001 ; |       X|   6
                BYTE    %11100011 ; |XXX   XX|   5
                BYTE    %11111001 ; |XXXXX  X|   4
                BYTE    %11111001 ; |XXXXX  X|   3
                BYTE    %11111100 ; |XXXXXX  |   2
                BYTE    %11111100 ; |XXXXXX  |   1
                BYTE    %11111111 ; |XXXXXXXX|   0

IN_KongClimb1_GRP2_1:

                BYTE    %00000000 ; |        |  35
                BYTE    %00000000 ; |        |  34
                BYTE    %00000000 ; |        |  33
                BYTE    %00000000 ; |        |  32
                BYTE    %00000000 ; |        |  31
                BYTE    %00000000 ; |        |  30
                BYTE    %00000010 ; |      X |  29
                BYTE    %00000111 ; |     XXX|  28
                BYTE    %00000111 ; |     XXX|  27
                BYTE    %00000011 ; |      XX|  26
                BYTE    %00000001 ; |       X|  25
                BYTE    %00000001 ; |       X|  24
                BYTE    %00000001 ; |       X|  23
                BYTE    %00000000 ; |        |  22
                BYTE    %00000000 ; |        |  21
                BYTE    %11110011 ; |XXXX  XX|  20
                BYTE    %11111111 ; |XXXXXXXX|  19
                BYTE    %11111111 ; |XXXXXXXX|  18
                BYTE    %11111111 ; |XXXXXXXX|  17
                BYTE    %11111111 ; |XXXXXXXX|  16
                BYTE    %11111111 ; |XXXXXXXX|  15
                BYTE    %11111111 ; |XXXXXXXX|  14
                BYTE    %11111111 ; |XXXXXXXX|  13
                BYTE    %11111111 ; |XXXXXXXX|  12
                BYTE    %11111111 ; |XXXXXXXX|  11
                BYTE    %11111111 ; |XXXXXXXX|  10
                BYTE    %11111111 ; |XXXXXXXX|   9
                BYTE    %01111110 ; | XXXXXX |   8
                BYTE    %11111111 ; |XXXXXXXX|   7
                BYTE    %11111111 ; |XXXXXXXX|   6
                BYTE    %11111111 ; |XXXXXXXX|   5
                BYTE    %11111111 ; |XXXXXXXX|   4
                BYTE    %11111111 ; |XXXXXXXX|   3
                BYTE    %11111111 ; |XXXXXXXX|   2
                BYTE    %00111110 ; |  XXXXX |   1
                BYTE    %00011100 ; |   XXX  |   0

IN_KongClimb1_GRP3_1:

                BYTE    %10000001 ; |X      X|  35
                BYTE    %10000001 ; |X      X|  34
                BYTE    %10000001 ; |X      X|  33
                BYTE    %11111111 ; |XXXXXXXX|  32
                BYTE    %00000111 ; |     XXX|  31
                BYTE    %00001111 ; |    XXXX|  30
                BYTE    %00111111 ; |  XXXXXX|  29
                BYTE    %11111111 ; |XXXXXXXX|  28
                BYTE    %11111101 ; |XXXXXX X|  27
                BYTE    %11111111 ; |XXXXXXXX|  26
                BYTE    %11111111 ; |XXXXXXXX|  25
                BYTE    %11111111 ; |XXXXXXXX|  24
                BYTE    %11111111 ; |XXXXXXXX|  23
                BYTE    %01111111 ; | XXXXXXX|  22
                BYTE    %00111111 ; |  XXXXXX|  21
                BYTE    %11011111 ; |XX XXXXX|  20
                BYTE    %11101001 ; |XXX X  X|  19
                BYTE    %11110000 ; |XXXX    |  18
                BYTE    %11110000 ; |XXXX    |  17
                BYTE    %11111000 ; |XXXXX   |  16
                BYTE    %11100001 ; |XXX    X|  15
                BYTE    %11110000 ; |XXXX    |  14
                BYTE    %11111111 ; |XXXXXXXX|  13
                BYTE    %11111110 ; |XXXXXXX |  12
                BYTE    %11111111 ; |XXXXXXXX|  11
                BYTE    %11111111 ; |XXXXXXXX|  10
                BYTE    %11111111 ; |XXXXXXXX|   9
                BYTE    %11111111 ; |XXXXXXXX|   8
                BYTE    %01111111 ; | XXXXXXX|   7
                BYTE    %10011111 ; |X  XXXXX|   6
                BYTE    %11100000 ; |XXX     |   5
                BYTE    %11011111 ; |XX XXXXX|   4
                BYTE    %10000001 ; |X      X|   3
                BYTE    %10000001 ; |X      X|   2
                BYTE    %00000001 ; |       X|   1
                BYTE    %11111111 ; |XXXXXXXX|   0

                ALIGN   $0100

IN_KongClimb1_GRP4_1:

                BYTE    %00000000 ; |        |  35
                BYTE    %00000000 ; |        |  34
                BYTE    %00000000 ; |        |  33
                BYTE    %00000000 ; |        |  32
                BYTE    %00000000 ; |        |  31
                BYTE    %00000000 ; |        |  30
                BYTE    %00000000 ; |        |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %00000000 ; |        |  25
                BYTE    %00000000 ; |        |  24
                BYTE    %10000000 ; |X       |  23
                BYTE    %11000000 ; |XX      |  22
                BYTE    %10000000 ; |X       |  21
                BYTE    %00000000 ; |        |  20
                BYTE    %00000000 ; |        |  19
                BYTE    %01100100 ; | XX  X  |  18
                BYTE    %11111000 ; |XXXXX   |  17
                BYTE    %11111000 ; |XXXXX   |  16
                BYTE    %01100001 ; | XX    X|  15
                BYTE    %00000010 ; |      X |  14
                BYTE    %11111100 ; |XXXXXX  |  13
                BYTE    %00000000 ; |        |  12
                BYTE    %00000001 ; |       X|  11
                BYTE    %11111101 ; |XXXXXX X|  10
                BYTE    %11111111 ; |XXXXXXXX|   9
                BYTE    %11111111 ; |XXXXXXXX|   8
                BYTE    %11111000 ; |XXXXX   |   7
                BYTE    %11000000 ; |XX      |   6
                BYTE    %00000000 ; |        |   5
                BYTE    %00000000 ; |        |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

IN_KongClimb1_GRP5_1 = IN_KongClimb0_GRP5_1 - 4

;
; Kong climbing -- pose 2
;

IN_KongClimb2_GRP0_0 = IN_KongClimb0_GRP0_0

IN_KongClimb2_GRP1_0 = IN_KongClimb0_GRP1_0

IN_KongClimb2_GRP2_0 = IN_KongClimb0_GRP2_0

IN_KongClimb2_GRP3_0:

                BYTE    %10000001 ; |X      X|  35
                BYTE    %10000001 ; |X      X|  34
                BYTE    %10000001 ; |X      X|  33
                BYTE    %11111111 ; |XXXXXXXX|  32
                BYTE    %10011111 ; |X  XXXXX|  31
                BYTE    %10111111 ; |X XXXXXX|  30
                BYTE    %11111111 ; |XXXXXXXX|  29
                BYTE    %11111110 ; |XXXXXXX |  28
                BYTE    %00111011 ; |  XXX XX|  27
                BYTE    %00000011 ; |      XX|  26
                BYTE    %11000111 ; |XX   XXX|  25
                BYTE    %00100111 ; |  X  XXX|  24
                BYTE    %00010111 ; |   X XXX|  23
                BYTE    %00011111 ; |   XXXXX|  22
                BYTE    %00011111 ; |   XXXXX|  21
                BYTE    %00001111 ; |    XXXX|  20
                BYTE    %00001111 ; |    XXXX|  19
                BYTE    %00000111 ; |     XXX|  18
                BYTE    %00000111 ; |     XXX|  17
                BYTE    %00000011 ; |      XX|  16
                BYTE    %00000001 ; |       X|  15
                BYTE    %00000000 ; |        |  14
                BYTE    %00000000 ; |        |  13
                BYTE    %00000000 ; |        |  12
                BYTE    %10000000 ; |X       |  11
                BYTE    %01100000 ; | XX     |  10
                BYTE    %00011111 ; |   XXXXX|   9
                BYTE    %00111111 ; |  XXXXXX|   8
                BYTE    %01000001 ; | X     X|   7
                BYTE    %01000001 ; | X     X|   6
                BYTE    %10000001 ; |X      X|   5
                BYTE    %11111111 ; |XXXXXXXX|   4
                BYTE    %10000001 ; |X      X|   3
                BYTE    %10000001 ; |X      X|   2
                BYTE    %10000001 ; |X      X|   1
                BYTE    %11111111 ; |XXXXXXXX|   0

IN_KongClimb2_GRP4_0:

                BYTE    %00000000 ; |        |  35
                BYTE    %00000000 ; |        |  34
                BYTE    %00000000 ; |        |  33
                BYTE    %00000000 ; |        |  32
                BYTE    %11000000 ; |XX      |  31
                BYTE    %10000000 ; |X       |  30
                BYTE    %10000000 ; |X       |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %10000000 ; |X       |  27
                BYTE    %11000000 ; |XX      |  26
                BYTE    %10000000 ; |X       |  25
                BYTE    %00100000 ; |  X     |  24
                BYTE    %00100001 ; |  X    X|  23
                BYTE    %11011011 ; |XX XX XX|  22
                BYTE    %11111111 ; |XXXXXXXX|  21
                BYTE    %11111111 ; |XXXXXXXX|  20
                BYTE    %11111111 ; |XXXXXXXX|  19
                BYTE    %11111111 ; |XXXXXXXX|  18
                BYTE    %11111111 ; |XXXXXXXX|  17
                BYTE    %11111110 ; |XXXXXXX |  16
                BYTE    %11111100 ; |XXXXXX  |  15
                BYTE    %00000010 ; |      X |  14
                BYTE    %00000000 ; |        |  13
                BYTE    %00000000 ; |        |  12
                BYTE    %00000111 ; |     XXX|  11
                BYTE    %00111000 ; |  XXX   |  10
                BYTE    %11000000 ; |XX      |   9
                BYTE    %00000000 ; |        |   8
                BYTE    %00000000 ; |        |   7
                BYTE    %00000000 ; |        |   6
                BYTE    %00000000 ; |        |   5
                BYTE    %00000000 ; |        |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

IN_KongClimb2_GRP5_0:

                BYTE    %00000000 ; |        |  35
                BYTE    %00000000 ; |        |  34
                BYTE    %00000000 ; |        |  33
                BYTE    %00000000 ; |        |  32
                BYTE    %00000000 ; |        |  31
                BYTE    %00000000 ; |        |  30
                BYTE    %00000000 ; |        |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %00000000 ; |        |  25
                BYTE    %00000000 ; |        |  24
                BYTE    %10000000 ; |X       |  23
                BYTE    %11000000 ; |XX      |  22
                BYTE    %11000000 ; |XX      |  21
                BYTE    %11100000 ; |XXX     |  20
                BYTE    %11100000 ; |XXX     |  19
                BYTE    %11000000 ; |XX      |  18
                BYTE    %10000000 ; |X       |  17
                BYTE    %00000000 ; |        |  16
                BYTE    %00000000 ; |        |  15
                BYTE    %00010000 ; |   X    |  14
                BYTE    %00100000 ; |  X     |  13
                BYTE    %11000000 ; |XX      |  12
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

IN_KongClimb2_GRP0_1 = IN_KongClimb0_GRP0_1

IN_KongClimb2_GRP1_1 = IN_KongClimb0_GRP1_1

IN_KongClimb2_GRP2_1 = IN_KongClimb0_GRP2_1

IN_KongClimb2_GRP3_1:

                BYTE    %10000001 ; |X      X|  35
                BYTE    %10000001 ; |X      X|  34
                BYTE    %10000001 ; |X      X|  33
                BYTE    %11111111 ; |XXXXXXXX|  32
                BYTE    %10000000 ; |X       |  31
                BYTE    %10000001 ; |X      X|  30
                BYTE    %10000011 ; |X     XX|  29
                BYTE    %11000111 ; |XX   XXX|  28
                BYTE    %11111100 ; |XXXXXX  |  27
                BYTE    %11111100 ; |XXXXXX  |  26
                BYTE    %00111000 ; |  XXX   |  25
                BYTE    %11011000 ; |XX XX   |  24
                BYTE    %11101000 ; |XXX X   |  23
                BYTE    %11111100 ; |XXXXXX  |  22
                BYTE    %11100000 ; |XXX     |  21
                BYTE    %11110000 ; |XXXX    |  20
                BYTE    %11111110 ; |XXXXXXX |  19
                BYTE    %11111001 ; |XXXXX  X|  18
                BYTE    %11111000 ; |XXXXX   |  17
                BYTE    %11111100 ; |XXXXXX  |  16
                BYTE    %11111110 ; |XXXXXXX |  15
                BYTE    %11111111 ; |XXXXXXXX|  14
                BYTE    %11111111 ; |XXXXXXXX|  13
                BYTE    %11111111 ; |XXXXXXXX|  12
                BYTE    %01111111 ; | XXXXXXX|  11
                BYTE    %10011111 ; |X  XXXXX|  10
                BYTE    %11100000 ; |XXX     |   9
                BYTE    %11011111 ; |XX XXXXX|   8
                BYTE    %10000001 ; |X      X|   7
                BYTE    %10000001 ; |X      X|   6
                BYTE    %00000001 ; |       X|   5
                BYTE    %11111111 ; |XXXXXXXX|   4
                BYTE    %10000001 ; |X      X|   3
                BYTE    %10000001 ; |X      X|   2
                BYTE    %10000001 ; |X      X|   1
                BYTE    %11111111 ; |XXXXXXXX|   0

IN_KongClimb2_GRP4_1:

                BYTE    %00000000 ; |        |  35
                BYTE    %00000000 ; |        |  34
                BYTE    %00000000 ; |        |  33
                BYTE    %00000000 ; |        |  32
                BYTE    %11000000 ; |XX      |  31
                BYTE    %10000000 ; |X       |  30
                BYTE    %10000000 ; |X       |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %01100000 ; | XX     |  25
                BYTE    %11000000 ; |XX      |  24
                BYTE    %11000000 ; |XX      |  23
                BYTE    %10011000 ; |X  XX   |  22
                BYTE    %01111100 ; | XXXXX  |  21
                BYTE    %01111100 ; | XXXXX  |  20
                BYTE    %00011010 ; |   XX X |  19
                BYTE    %00000000 ; |        |  18
                BYTE    %11111111 ; |XXXXXXXX|  17
                BYTE    %00000001 ; |       X|  16
                BYTE    %00000011 ; |      XX|  15
                BYTE    %11111101 ; |XXXXXX X|  14
                BYTE    %11111111 ; |XXXXXXXX|  13
                BYTE    %11111111 ; |XXXXXXXX|  12
                BYTE    %11111000 ; |XXXXX   |  11
                BYTE    %11000000 ; |XX      |  10
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

; --------------------------------------------------------------------

                ; moved to pose 0

;IN_KongClimb2_GRP5_1:
;
;               BYTE    %00000000 ; |        |  35
;               BYTE    %00000000 ; |        |  34
;               BYTE    %00000000 ; |        |  33
;               BYTE    %00000000 ; |        |  32
;               BYTE    %00000000 ; |        |  31
;               BYTE    %00000000 ; |        |  30
;               BYTE    %00000000 ; |        |  29
;               BYTE    %00000000 ; |        |  28
;               BYTE    %00000000 ; |        |  27
;               BYTE    %00000000 ; |        |  26
;               BYTE    %00000000 ; |        |  25
;               BYTE    %00000000 ; |        |  24
;               BYTE    %00000000 ; |        |  23
;               BYTE    %00000000 ; |        |  22
;               BYTE    %00000000 ; |        |  21
;               BYTE    %01100000 ; | XX     |  20
;               BYTE    %00000000 ; |        |  19
;               BYTE    %00000000 ; |        |  18
;               BYTE    %11000000 ; |XX      |  17
;               BYTE    %11100000 ; |XXX     |  16
;               BYTE    %11110000 ; |XXXX    |  15
;               BYTE    %11100000 ; |XXX     |  14
;               BYTE    %11000000 ; |XX      |  13
;               BYTE    %00000000 ; |        |  12
;               BYTE    %00000000 ; |        |  11
;               BYTE    %00000000 ; |        |  10
;               BYTE    %00000000 ; |        |   9
;               BYTE    %00000000 ; |        |   8
;               BYTE    %00000000 ; |        |   7
;               BYTE    %00000000 ; |        |   6
;               BYTE    %00000000 ; |        |   5
;               BYTE    %00000000 ; |        |   4
;               BYTE    %00000000 ; |        |   3
;               BYTE    %00000000 ; |        |   2
;               BYTE    %00000000 ; |        |   1
;               BYTE    %00000000 ; |        |   0

; --------------------------------------------------------------------

IN_SetDemoMode: SUBROUTINE

                ; check if demo mode active

                ldx     DK_VcsCycleCtr
                cpx     #DK_EXIT_TIME           ; automatic exit ?
                bcs     .demo                   ; yes -> enter demo
                rts

                ; setup parameters for demo play

.demo           ldx     #7
.loop           lda     IN_DemoConfTab,x
                sta     BonusTimer_W,x
                dex
                bpl     .loop
                sta     EventCtr                ; EventCtr := BonusTimer
                lda     #$50                    ; bonus for L=1
                sta     Bonus
                lda     #(1*128)+1              ; death: 1 | lives: 1
                sta     Death_Lives

                ; skip intro and intermission

                txs                             ; won't use RTS (X = $FF)
                jmp     Bank2_Stage             ; directly go to stage screen

; --------------------------------------------------------------------

;
; Kong climbing -- pose 3
;

                ALIGN   $0100

IN_KongClimb3_GRP0_0 = IN_KongClimb1_GRP0_0

IN_KongClimb3_GRP1_0 = IN_KongClimb1_GRP1_0

IN_KongClimb3_GRP2_0 = IN_KongClimb1_GRP2_0

IN_KongClimb3_GRP3_0:

                BYTE    %10000001 ; |X      X|  35
                BYTE    %10000001 ; |X      X|  34
                BYTE    %10000001 ; |X      X|  33
                BYTE    %11111111 ; |XXXXXXXX|  32
                BYTE    %11111111 ; |XXXXXXXX|  31
                BYTE    %11111110 ; |XXXXXXX |  30
                BYTE    %11111001 ; |XXXXX  X|  29
                BYTE    %11100011 ; |XXX   XX|  28
                BYTE    %00000001 ; |       X|  27
                BYTE    %00000001 ; |       X|  26
                BYTE    %00000000 ; |        |  25
                BYTE    %00000000 ; |        |  24
                BYTE    %00000011 ; |      XX|  23
                BYTE    %10000011 ; |X     XX|  22
                BYTE    %11000111 ; |XX   XXX|  21
                BYTE    %00100111 ; |  X  XXX|  20
                BYTE    %00010111 ; |   X XXX|  19
                BYTE    %00011111 ; |   XXXXX|  18
                BYTE    %00011111 ; |   XXXXX|  17
                BYTE    %00001111 ; |    XXXX|  16
                BYTE    %00001111 ; |    XXXX|  15
                BYTE    %00000111 ; |     XXX|  14
                BYTE    %00000111 ; |     XXX|  13
                BYTE    %00000011 ; |      XX|  12
                BYTE    %00000001 ; |       X|  11
                BYTE    %00000000 ; |        |  10
                BYTE    %00000000 ; |        |   9
                BYTE    %00000000 ; |        |   8
                BYTE    %10000000 ; |X       |   7
                BYTE    %01100000 ; | XX     |   6
                BYTE    %00011111 ; |   XXXXX|   5
                BYTE    %00111111 ; |  XXXXXX|   4
                BYTE    %01000001 ; | X     X|   3
                BYTE    %01000001 ; | X     X|   2
                BYTE    %10000001 ; |X      X|   1
                BYTE    %11111111 ; |XXXXXXXX|   0

IN_KongClimb3_GRP4_0:

                BYTE    %00000000 ; |        |  35
                BYTE    %00000000 ; |        |  34
                BYTE    %00000000 ; |        |  33
                BYTE    %00000000 ; |        |  32
                BYTE    %00000000 ; |        |  31
                BYTE    %00000000 ; |        |  30
                BYTE    %00000000 ; |        |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %00000000 ; |        |  25
                BYTE    %00000000 ; |        |  24
                BYTE    %10000000 ; |X       |  23
                BYTE    %11000000 ; |XX      |  22
                BYTE    %11000000 ; |XX      |  21
                BYTE    %11000000 ; |XX      |  20
                BYTE    %10000001 ; |X      X|  19
                BYTE    %10011011 ; |X  XX XX|  18
                BYTE    %11111111 ; |XXXXXXXX|  17
                BYTE    %11111111 ; |XXXXXXXX|  16
                BYTE    %11111111 ; |XXXXXXXX|  15
                BYTE    %11111111 ; |XXXXXXXX|  14
                BYTE    %11111111 ; |XXXXXXXX|  13
                BYTE    %11111110 ; |XXXXXXX |  12
                BYTE    %11111100 ; |XXXXXX  |  11
                BYTE    %00000010 ; |      X |  10
                BYTE    %00000000 ; |        |   9
                BYTE    %00000000 ; |        |   8
                BYTE    %00000111 ; |     XXX|   7
                BYTE    %00111000 ; |  XXX   |   6
                BYTE    %11000000 ; |XX      |   5
                BYTE    %00000000 ; |        |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

IN_KongClimb3_GRP5_0 = IN_KongClimb2_GRP5_0 - 4

IN_KongClimb3_GRP0_1 = IN_KongClimb1_GRP0_1

IN_KongClimb3_GRP1_1 = IN_KongClimb1_GRP1_1

IN_KongClimb3_GRP2_1 = IN_KongClimb1_GRP2_1

IN_KongClimb3_GRP3_1:

                BYTE    %10000001 ; |X      X|  35
                BYTE    %10000001 ; |X      X|  34
                BYTE    %10000001 ; |X      X|  33
                BYTE    %11111111 ; |XXXXXXXX|  32
                BYTE    %00000111 ; |     XXX|  31
                BYTE    %00001111 ; |    XXXX|  30
                BYTE    %00111111 ; |  XXXXXX|  29
                BYTE    %11111111 ; |XXXXXXXX|  28
                BYTE    %11111101 ; |XXXXXX X|  27
                BYTE    %11111111 ; |XXXXXXXX|  26
                BYTE    %11111111 ; |XXXXXXXX|  25
                BYTE    %11111111 ; |XXXXXXXX|  24
                BYTE    %11111100 ; |XXXXXX  |  23
                BYTE    %01111100 ; | XXXXX  |  22
                BYTE    %00111000 ; |  XXX   |  21
                BYTE    %11011000 ; |XX XX   |  20
                BYTE    %11101000 ; |XXX X   |  19
                BYTE    %11111100 ; |XXXXXX  |  18
                BYTE    %11100000 ; |XXX     |  17
                BYTE    %11110000 ; |XXXX    |  16
                BYTE    %11111110 ; |XXXXXXX |  15
                BYTE    %11111001 ; |XXXXX  X|  14
                BYTE    %11111000 ; |XXXXX   |  13
                BYTE    %11111100 ; |XXXXXX  |  12
                BYTE    %11111110 ; |XXXXXXX |  11
                BYTE    %11111111 ; |XXXXXXXX|  10
                BYTE    %11111111 ; |XXXXXXXX|   9
                BYTE    %11111111 ; |XXXXXXXX|   8
                BYTE    %01111111 ; | XXXXXXX|   7
                BYTE    %10011111 ; |X  XXXXX|   6
                BYTE    %11100000 ; |XXX     |   5
                BYTE    %11011111 ; |XX XXXXX|   4
                BYTE    %10000001 ; |X      X|   3
                BYTE    %10000001 ; |X      X|   2
                BYTE    %00000001 ; |       X|   1
                BYTE    %11111111 ; |XXXXXXXX|   0

IN_KongClimb3_GRP4_1:

                BYTE    %00000000 ; |        |  35
                BYTE    %00000000 ; |        |  34
                BYTE    %00000000 ; |        |  33
                BYTE    %00000000 ; |        |  32
                BYTE    %00000000 ; |        |  31
                BYTE    %00000000 ; |        |  30
                BYTE    %00000000 ; |        |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %00000000 ; |        |  25
                BYTE    %00000000 ; |        |  24
                BYTE    %00000000 ; |        |  23
                BYTE    %00000000 ; |        |  22
                BYTE    %00000000 ; |        |  21
                BYTE    %00000000 ; |        |  20
                BYTE    %00000000 ; |        |  19
                BYTE    %10011000 ; |X  XX   |  18
                BYTE    %01111100 ; | XXXXX  |  17
                BYTE    %01111100 ; | XXXXX  |  16
                BYTE    %00011010 ; |   XX X |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %11111111 ; |XXXXXXXX|  13
                BYTE    %00000001 ; |       X|  12
                BYTE    %00000011 ; |      XX|  11
                BYTE    %11111101 ; |XXXXXX X|  10
                BYTE    %11111111 ; |XXXXXXXX|   9
                BYTE    %11111111 ; |XXXXXXXX|   8
                BYTE    %11111000 ; |XXXXX   |   7
                BYTE    %11000000 ; |XX      |   6
                BYTE    %00000000 ; |        |   5
                BYTE    %00000000 ; |        |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

IN_KongClimb3_GRP5_1 = IN_KongClimb2_GRP5_1 - 4

; --------------------------------------------------------------------

                ; moved from subroutines section

IN_KongColors:  SUBROUTINE
                lda     FrameCtr
                and     #%00000001
                tax
                lda     .KongCol,x
                sta     COLUP0
                sta     COLUP1
                rts

.KongCol        BYTE    KONG_COL_0, KONG_COL_1

; --------------------------------------------------------------------

;
; Kong standing
;

                ALIGN   $0020

IN_KongStand_GRP0_0:

                BYTE    %11111111 ; |XXXXXXXX|  31
                BYTE    %01111011 ; | XXXX XX|  30
                BYTE    %00101100 ; |  X XX  |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00010000 ; |   X    |  26
                BYTE    %00010000 ; |   X    |  25
                BYTE    %00010000 ; |   X    |  24
                BYTE    %00001000 ; |    X   |  23
                BYTE    %00000100 ; |     X  |  22
                BYTE    %00000010 ; |      X |  21
                BYTE    %00000001 ; |       X|  20
                BYTE    %00000000 ; |        |  19
                BYTE    %00000000 ; |        |  18
                BYTE    %00000000 ; |        |  17
                BYTE    %00000000 ; |        |  16
                BYTE    %00000000 ; |        |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %00000001 ; |       X|  13
                BYTE    %00000010 ; |      X |  12
                BYTE    %00100000 ; |  X     |  11
                BYTE    %00011000 ; |   XX   |  10
                BYTE    %00000100 ; |     X  |   9
                BYTE    %00000011 ; |      XX|   8
                BYTE    %00000000 ; |        |   7
                BYTE    %00000000 ; |        |   6
                BYTE    %00000000 ; |        |   5
                BYTE    %00000000 ; |        |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

IN_KongStand_GRP1_0:

                BYTE    %11110000 ; |XXXX    |  31
                BYTE    %11011000 ; |XX XX   |  30
                BYTE    %01101000 ; | XX X   |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %00000010 ; |      X |  25
                BYTE    %00000001 ; |       X|  24
                BYTE    %00000011 ; |      XX|  23
                BYTE    %00000011 ; |      XX|  22
                BYTE    %00010111 ; |   X XXX|  21
                BYTE    %00101111 ; |  X XXXX|  20
                BYTE    %00000010 ; |      X |  19
                BYTE    %00000001 ; |       X|  18
                BYTE    %00000010 ; |      X |  17
                BYTE    %00000111 ; |     XXX|  16
                BYTE    %01110111 ; | XXX XXX|  15
                BYTE    %11100000 ; |XXX     |  14
                BYTE    %10000000 ; |X       |  13
                BYTE    %00000011 ; |      XX|  12
                BYTE    %00000011 ; |      XX|  11
                BYTE    %00011101 ; |   XXX X|  10
                BYTE    %00011110 ; |   XXXX |   9
                BYTE    %00011111 ; |   XXXXX|   8
                BYTE    %10001111 ; |X   XXXX|   7
                BYTE    %00111000 ; |  XXX   |   6
                BYTE    %00111000 ; |  XXX   |   5
                BYTE    %00011001 ; |   XX  X|   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

IN_KongStand_GRP2_0:

                BYTE    %00000000 ; |        |  31
                BYTE    %00000000 ; |        |  30
                BYTE    %00000000 ; |        |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %01010010 ; | X X  X |  25
                BYTE    %11111011 ; |XXXXX XX|  24
                BYTE    %11111111 ; |XXXXXXXX|  23
                BYTE    %11111111 ; |XXXXXXXX|  22
                BYTE    %11111111 ; |XXXXXXXX|  21
                BYTE    %11111111 ; |XXXXXXXX|  20
                BYTE    %11111111 ; |XXXXXXXX|  19
                BYTE    %11111111 ; |XXXXXXXX|  18
                BYTE    %11111111 ; |XXXXXXXX|  17
                BYTE    %11100111 ; |XXX  XXX|  16
                BYTE    %00000000 ; |        |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %11111111 ; |XXXXXXXX|  13
                BYTE    %11111111 ; |XXXXXXXX|  12
                BYTE    %11111111 ; |XXXXXXXX|  11
                BYTE    %11111111 ; |XXXXXXXX|  10
                BYTE    %00000000 ; |        |   9
                BYTE    %11111111 ; |XXXXXXXX|   8
                BYTE    %11100011 ; |XXX   XX|   7
                BYTE    %11111111 ; |XXXXXXXX|   6
                BYTE    %11001001 ; |XX  X  X|   5
                BYTE    %11001001 ; |XX  X  X|   4
                BYTE    %11110111 ; |XXXX XXX|   3
                BYTE    %01100011 ; | XX   XX|   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

IN_KongStand_GRP3_0:

                BYTE    %00001111 ; |    XXXX|  31
                BYTE    %00011011 ; |   XX XX|  30
                BYTE    %00010110 ; |   X XX |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %01000000 ; | X      |  25
                BYTE    %10000000 ; |X       |  24
                BYTE    %11000000 ; |XX      |  23
                BYTE    %11000000 ; |XX      |  22
                BYTE    %11101000 ; |XXX X   |  21
                BYTE    %11110100 ; |XXXX X  |  20
                BYTE    %01000000 ; | X      |  19
                BYTE    %10000000 ; |X       |  18
                BYTE    %01000000 ; | X      |  17
                BYTE    %11100000 ; |XXX     |  16
                BYTE    %11101110 ; |XXX XXX |  15
                BYTE    %00000111 ; |     XXX|  14
                BYTE    %10000001 ; |X      X|  13
                BYTE    %11100000 ; |XXX     |  12
                BYTE    %11100000 ; |XXX     |  11
                BYTE    %11011000 ; |XX XX   |  10
                BYTE    %00111000 ; |  XXX   |   9
                BYTE    %11111000 ; |XXXXX   |   8
                BYTE    %11110001 ; |XXXX   X|   7
                BYTE    %10011100 ; |X  XXX  |   6
                BYTE    %10011100 ; |X  XXX  |   5
                BYTE    %11011000 ; |XX XX   |   4
                BYTE    %10000000 ; |X       |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

IN_KongStand_GRP4_0:

                BYTE    %11111111 ; |XXXXXXXX|  31
                BYTE    %11011110 ; |XX XXXX |  30
                BYTE    %00110100 ; |  XX X  |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00001000 ; |    X   |  26
                BYTE    %00001000 ; |    X   |  25
                BYTE    %00001000 ; |    X   |  24
                BYTE    %00010000 ; |   X    |  23
                BYTE    %00100000 ; |  X     |  22
                BYTE    %01000000 ; | X      |  21
                BYTE    %10000000 ; |X       |  20
                BYTE    %00000000 ; |        |  19
                BYTE    %00000000 ; |        |  18
                BYTE    %00000000 ; |        |  17
                BYTE    %00000000 ; |        |  16
                BYTE    %00000000 ; |        |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %10000000 ; |X       |  13
                BYTE    %01000000 ; | X      |  12
                BYTE    %00000100 ; |     X  |  11
                BYTE    %00011000 ; |   XX   |  10
                BYTE    %00100000 ; |  X     |   9
                BYTE    %11000000 ; |XX      |   8
                BYTE    %00000000 ; |        |   7
                BYTE    %00000000 ; |        |   6
                BYTE    %00000000 ; |        |   5
                BYTE    %00000000 ; |        |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

IN_KongStand_GRP5_0 = IN_EmptySpace

IN_KongStand_GRP0_1:

                BYTE    %10111111 ; |X XXXXXX|  31
                BYTE    %01011111 ; | X XXXXX|  30
                BYTE    %00111111 ; |  XXXXXX|  29
                BYTE    %00001111 ; |    XXXX|  28
                BYTE    %00001111 ; |    XXXX|  27
                BYTE    %00001111 ; |    XXXX|  26
                BYTE    %00001111 ; |    XXXX|  25
                BYTE    %00001111 ; |    XXXX|  24
                BYTE    %00000111 ; |     XXX|  23
                BYTE    %00000011 ; |      XX|  22
                BYTE    %00000001 ; |       X|  21
                BYTE    %00000000 ; |        |  20
                BYTE    %00000001 ; |       X|  19
                BYTE    %00001111 ; |    XXXX|  18
                BYTE    %00011111 ; |   XXXXX|  17
                BYTE    %00111111 ; |  XXXXXX|  16
                BYTE    %01111111 ; | XXXXXXX|  15
                BYTE    %01111111 ; | XXXXXXX|  14
                BYTE    %01111110 ; | XXXXXX |  13
                BYTE    %01111101 ; | XXXXX X|  12
                BYTE    %00011111 ; |   XXXXX|  11
                BYTE    %00000111 ; |     XXX|  10
                BYTE    %00000011 ; |      XX|   9
                BYTE    %00000000 ; |        |   8
                BYTE    %00000000 ; |        |   7
                BYTE    %00000000 ; |        |   6
                BYTE    %00000000 ; |        |   5
                BYTE    %00000000 ; |        |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

IN_KongStand_GRP1_1:

                BYTE    %11110000 ; |XXXX    |  31
                BYTE    %11111000 ; |XXXXX   |  30
                BYTE    %11111000 ; |XXXXX   |  29
                BYTE    %11111100 ; |XXXXXX  |  28
                BYTE    %11111110 ; |XXXXXXX |  27
                BYTE    %11111111 ; |XXXXXXXX|  26
                BYTE    %11111101 ; |XXXXXX X|  25
                BYTE    %11111110 ; |XXXXXXX |  24
                BYTE    %11111101 ; |XXXXXX X|  23
                BYTE    %11111101 ; |XXXXXX X|  22
                BYTE    %11101000 ; |XXX X   |  21
                BYTE    %11010111 ; |XX X XXX|  20
                BYTE    %11111111 ; |XXXXXXXX|  19
                BYTE    %11111111 ; |XXXXXXXX|  18
                BYTE    %11111111 ; |XXXXXXXX|  17
                BYTE    %11111111 ; |XXXXXXXX|  16
                BYTE    %10001111 ; |X   XXXX|  15
                BYTE    %00011111 ; |   XXXXX|  14
                BYTE    %01111111 ; | XXXXXXX|  13
                BYTE    %11111111 ; |XXXXXXXX|  12
                BYTE    %11111111 ; |XXXXXXXX|  11
                BYTE    %11111111 ; |XXXXXXXX|  10
                BYTE    %11111111 ; |XXXXXXXX|   9
                BYTE    %11111111 ; |XXXXXXXX|   8
                BYTE    %01111111 ; | XXXXXXX|   7
                BYTE    %00111111 ; |  XXXXXX|   6
                BYTE    %00111111 ; |  XXXXXX|   5
                BYTE    %00011111 ; |   XXXXX|   4
                BYTE    %00000011 ; |      XX|   3
                BYTE    %00000001 ; |       X|   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

IN_KongStand_GRP2_1:

                BYTE    %00000000 ; |        |  31
                BYTE    %00000000 ; |        |  30
                BYTE    %00000000 ; |        |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %10101101 ; |X X XX X|  25
                BYTE    %10101101 ; |X X XX X|  24
                BYTE    %01011010 ; | X XX X |  23
                BYTE    %11111111 ; |XXXXXXXX|  22
                BYTE    %00111100 ; |  XXXX  |  21
                BYTE    %11011011 ; |XX XX XX|  20
                BYTE    %11100111 ; |XXX  XXX|  19
                BYTE    %11100111 ; |XXX  XXX|  18
                BYTE    %11100111 ; |XXX  XXX|  17
                BYTE    %11011011 ; |XX XX XX|  16
                BYTE    %11111111 ; |XXXXXXXX|  15
                BYTE    %11111111 ; |XXXXXXXX|  14
                BYTE    %11111111 ; |XXXXXXXX|  13
                BYTE    %11111111 ; |XXXXXXXX|  12
                BYTE    %11111111 ; |XXXXXXXX|  11
                BYTE    %11111111 ; |XXXXXXXX|  10
                BYTE    %11111111 ; |XXXXXXXX|   9
                BYTE    %11111111 ; |XXXXXXXX|   8
                BYTE    %11111111 ; |XXXXXXXX|   7
                BYTE    %11111111 ; |XXXXXXXX|   6
                BYTE    %11011101 ; |XX XXX X|   5
                BYTE    %11001001 ; |XX  X  X|   4
                BYTE    %11111111 ; |XXXXXXXX|   3
                BYTE    %11111111 ; |XXXXXXXX|   2
                BYTE    %11111111 ; |XXXXXXXX|   1
                BYTE    %01111111 ; | XXXXXXX|   0

IN_KongStand_GRP3_1:

                BYTE    %00001111 ; |    XXXX|  31
                BYTE    %00011111 ; |   XXXXX|  30
                BYTE    %00011111 ; |   XXXXX|  29
                BYTE    %00111111 ; |  XXXXXX|  28
                BYTE    %01111111 ; | XXXXXXX|  27
                BYTE    %11111111 ; |XXXXXXXX|  26
                BYTE    %10111111 ; |X XXXXXX|  25
                BYTE    %01111111 ; | XXXXXXX|  24
                BYTE    %10111111 ; |X XXXXXX|  23
                BYTE    %10111111 ; |X XXXXXX|  22
                BYTE    %00010111 ; |   X XXX|  21
                BYTE    %11101011 ; |XXX X XX|  20
                BYTE    %11111111 ; |XXXXXXXX|  19
                BYTE    %11111111 ; |XXXXXXXX|  18
                BYTE    %11111111 ; |XXXXXXXX|  17
                BYTE    %11111111 ; |XXXXXXXX|  16
                BYTE    %11110001 ; |XXXX   X|  15
                BYTE    %11111000 ; |XXXXX   |  14
                BYTE    %11111110 ; |XXXXXXX |  13
                BYTE    %11111111 ; |XXXXXXXX|  12
                BYTE    %11111111 ; |XXXXXXXX|  11
                BYTE    %11111111 ; |XXXXXXXX|  10
                BYTE    %11111111 ; |XXXXXXXX|   9
                BYTE    %11111111 ; |XXXXXXXX|   8
                BYTE    %11111110 ; |XXXXXXX |   7
                BYTE    %11111100 ; |XXXXXX  |   6
                BYTE    %11111100 ; |XXXXXX  |   5
                BYTE    %11111000 ; |XXXXX   |   4
                BYTE    %11100000 ; |XXX     |   3
                BYTE    %11000000 ; |XX      |   2
                BYTE    %10000000 ; |X       |   1
                BYTE    %00000000 ; |        |   0

IN_KongStand_GRP4_1:

                BYTE    %11111101 ; |XXXXXX X|  31
                BYTE    %11111010 ; |XXXXX X |  30
                BYTE    %11111100 ; |XXXXXX  |  29
                BYTE    %11110000 ; |XXXX    |  28
                BYTE    %11110000 ; |XXXX    |  27
                BYTE    %11110000 ; |XXXX    |  26
                BYTE    %11110000 ; |XXXX    |  25
                BYTE    %11110000 ; |XXXX    |  24
                BYTE    %11100000 ; |XXX     |  23
                BYTE    %11000000 ; |XX      |  22
                BYTE    %10000000 ; |X       |  21
                BYTE    %00000000 ; |        |  20
                BYTE    %10000000 ; |X       |  19
                BYTE    %11110000 ; |XXXX    |  18
                BYTE    %11111000 ; |XXXXX   |  17
                BYTE    %11111100 ; |XXXXXX  |  16
                BYTE    %11111110 ; |XXXXXXX |  15
                BYTE    %11111110 ; |XXXXXXX |  14
                BYTE    %01111110 ; | XXXXXX |  13
                BYTE    %10111110 ; |X XXXXX |  12
                BYTE    %11111000 ; |XXXXX   |  11
                BYTE    %11100000 ; |XXX     |  10
                BYTE    %11000000 ; |XX      |   9
                BYTE    %00000000 ; |        |   8
                BYTE    %00000000 ; |        |   7
                BYTE    %00000000 ; |        |   6
                BYTE    %00000000 ; |        |   5
                BYTE    %00000000 ; |        |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

IN_KongStand_GRP5_1 = IN_EmptySpace

;
; Kong grinning
;

IN_KongGrin_GRP0_0 = IN_KongStand_GRP0_0

IN_KongGrin_GRP1_0:

                BYTE    %11110000 ; |XXXX    |  31
                BYTE    %11011000 ; |XX XX   |  30
                BYTE    %01101000 ; | XX X   |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %00000010 ; |      X |  25
                BYTE    %00000001 ; |       X|  24
                BYTE    %00000011 ; |      XX|  23
                BYTE    %00000011 ; |      XX|  22
                BYTE    %00010111 ; |   X XXX|  21
                BYTE    %00101111 ; |  X XXXX|  20
                BYTE    %00000010 ; |      X |  19
                BYTE    %00000001 ; |       X|  18
                BYTE    %00000010 ; |      X |  17
                BYTE    %00000111 ; |     XXX|  16
                BYTE    %01110111 ; | XXX XXX|  15
                BYTE    %11100000 ; |XXX     |  14
                BYTE    %10000000 ; |X       |  13
                BYTE    %00000111 ; |     XXX|  12
                BYTE    %00001100 ; |    XX  |  11
                BYTE    %00011101 ; |   XXX X|  10
                BYTE    %00011000 ; |   XX   |   9
                BYTE    %00011100 ; |   XXX  |   8
                BYTE    %10001111 ; |X   XXXX|   7
                BYTE    %00111000 ; |  XXX   |   6
                BYTE    %00111000 ; |  XXX   |   5
                BYTE    %00011001 ; |   XX  X|   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

IN_KongGrin_GRP2_0:

                BYTE    %00000000 ; |        |  31
                BYTE    %00000000 ; |        |  30
                BYTE    %00000000 ; |        |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %01010010 ; | X X  X |  25
                BYTE    %11111011 ; |XXXXX XX|  24
                BYTE    %11111111 ; |XXXXXXXX|  23
                BYTE    %11111111 ; |XXXXXXXX|  22
                BYTE    %11111111 ; |XXXXXXXX|  21
                BYTE    %11111111 ; |XXXXXXXX|  20
                BYTE    %11111111 ; |XXXXXXXX|  19
                BYTE    %11111111 ; |XXXXXXXX|  18
                BYTE    %11111111 ; |XXXXXXXX|  17
                BYTE    %11100111 ; |XXX  XXX|  16
                BYTE    %00000000 ; |        |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %11111111 ; |XXXXXXXX|  13
                BYTE    %10001000 ; |X   X   |  12
                BYTE    %10001000 ; |X   X   |  11
                BYTE    %11011101 ; |XX XXX X|  10
                BYTE    %10001000 ; |X   X   |   9
                BYTE    %11111111 ; |XXXXXXXX|   8
                BYTE    %11100011 ; |XXX   XX|   7
                BYTE    %11111111 ; |XXXXXXXX|   6
                BYTE    %11001001 ; |XX  X  X|   5
                BYTE    %11001001 ; |XX  X  X|   4
                BYTE    %11110111 ; |XXXX XXX|   3
                BYTE    %01100011 ; | XX   XX|   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

IN_KongGrin_GRP3_0:

                BYTE    %00001111 ; |    XXXX|  31
                BYTE    %00011011 ; |   XX XX|  30
                BYTE    %00010110 ; |   X XX |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %01000000 ; | X      |  25
                BYTE    %10000000 ; |X       |  24
                BYTE    %11000000 ; |XX      |  23
                BYTE    %11000000 ; |XX      |  22
                BYTE    %11101000 ; |XXX X   |  21
                BYTE    %11110100 ; |XXXX X  |  20
                BYTE    %01000000 ; | X      |  19
                BYTE    %10000000 ; |X       |  18
                BYTE    %01000000 ; | X      |  17
                BYTE    %11100000 ; |XXX     |  16
                BYTE    %11101110 ; |XXX XXX |  15
                BYTE    %00000111 ; |     XXX|  14
                BYTE    %10000001 ; |X      X|  13
                BYTE    %11100000 ; |XXX     |  12
                BYTE    %10010000 ; |X  X    |  11
                BYTE    %11011000 ; |XX XX   |  10
                BYTE    %10001000 ; |X   X   |   9
                BYTE    %10011000 ; |X  XX   |   8
                BYTE    %11110001 ; |XXXX   X|   7
                BYTE    %10011100 ; |X  XXX  |   6
                BYTE    %10011100 ; |X  XXX  |   5
                BYTE    %11011000 ; |XX XX   |   4
                BYTE    %10000000 ; |X       |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

IN_KongGrin_GRP4_0 = IN_KongStand_GRP4_0

IN_KongGrin_GRP5_0 = IN_EmptySpace

IN_KongGrin_GRP0_1 = IN_KongStand_GRP0_1

IN_KongGrin_GRP1_1:

                BYTE    %11110000 ; |XXXX    |  31
                BYTE    %11111000 ; |XXXXX   |  30
                BYTE    %11111000 ; |XXXXX   |  29
                BYTE    %11111100 ; |XXXXXX  |  28
                BYTE    %11111110 ; |XXXXXXX |  27
                BYTE    %11111111 ; |XXXXXXXX|  26
                BYTE    %11111101 ; |XXXXXX X|  25
                BYTE    %11111110 ; |XXXXXXX |  24
                BYTE    %11111101 ; |XXXXXX X|  23
                BYTE    %11111101 ; |XXXXXX X|  22
                BYTE    %11101000 ; |XXX X   |  21
                BYTE    %11010111 ; |XX X XXX|  20
                BYTE    %11111111 ; |XXXXXXXX|  19
                BYTE    %11111111 ; |XXXXXXXX|  18
                BYTE    %11111111 ; |XXXXXXXX|  17
                BYTE    %11111111 ; |XXXXXXXX|  16
                BYTE    %10001111 ; |X   XXXX|  15
                BYTE    %00011111 ; |   XXXXX|  14
                BYTE    %01111111 ; | XXXXXXX|  13
                BYTE    %11111111 ; |XXXXXXXX|  12
                BYTE    %11111100 ; |XXXXXX  |  11
                BYTE    %11111101 ; |XXXXXX X|  10
                BYTE    %11111000 ; |XXXXX   |   9
                BYTE    %11111100 ; |XXXXXX  |   8
                BYTE    %01111111 ; | XXXXXXX|   7
                BYTE    %00111111 ; |  XXXXXX|   6
                BYTE    %00111111 ; |  XXXXXX|   5
                BYTE    %00011111 ; |   XXXXX|   4
                BYTE    %00000011 ; |      XX|   3
                BYTE    %00000001 ; |       X|   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

IN_KongGrin_GRP2_1:

                BYTE    %00000000 ; |        |  31
                BYTE    %00000000 ; |        |  30
                BYTE    %00000000 ; |        |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %10101101 ; |X X XX X|  25
                BYTE    %10101101 ; |X X XX X|  24
                BYTE    %01011010 ; | X XX X |  23
                BYTE    %11111111 ; |XXXXXXXX|  22
                BYTE    %00111100 ; |  XXXX  |  21
                BYTE    %11011011 ; |XX XX XX|  20
                BYTE    %11100111 ; |XXX  XXX|  19
                BYTE    %11100111 ; |XXX  XXX|  18
                BYTE    %11100111 ; |XXX  XXX|  17
                BYTE    %11011011 ; |XX XX XX|  16
                BYTE    %11111111 ; |XXXXXXXX|  15
                BYTE    %11111111 ; |XXXXXXXX|  14
                BYTE    %11111111 ; |XXXXXXXX|  13
                BYTE    %10001000 ; |X   X   |  12
                BYTE    %10001000 ; |X   X   |  11
                BYTE    %11011101 ; |XX XXX X|  10
                BYTE    %10001000 ; |X   X   |   9
                BYTE    %11111111 ; |XXXXXXXX|   8
                BYTE    %11111111 ; |XXXXXXXX|   7
                BYTE    %11111111 ; |XXXXXXXX|   6
                BYTE    %11011101 ; |XX XXX X|   5
                BYTE    %11001001 ; |XX  X  X|   4
                BYTE    %11111111 ; |XXXXXXXX|   3
                BYTE    %11111111 ; |XXXXXXXX|   2
                BYTE    %11111111 ; |XXXXXXXX|   1
                BYTE    %01111111 ; | XXXXXXX|   0

IN_KongGrin_GRP3_1:

                BYTE    %00001111 ; |    XXXX|  31
                BYTE    %00011111 ; |   XXXXX|  30
                BYTE    %00011111 ; |   XXXXX|  29
                BYTE    %00111111 ; |  XXXXXX|  28
                BYTE    %01111111 ; | XXXXXXX|  27
                BYTE    %11111111 ; |XXXXXXXX|  26
                BYTE    %10111111 ; |X XXXXXX|  25
                BYTE    %01111111 ; | XXXXXXX|  24
                BYTE    %10111111 ; |X XXXXXX|  23
                BYTE    %10111111 ; |X XXXXXX|  22
                BYTE    %00010111 ; |   X XXX|  21
                BYTE    %11101011 ; |XXX X XX|  20
                BYTE    %11111111 ; |XXXXXXXX|  19
                BYTE    %11111111 ; |XXXXXXXX|  18
                BYTE    %11111111 ; |XXXXXXXX|  17
                BYTE    %11111111 ; |XXXXXXXX|  16
                BYTE    %11110001 ; |XXXX   X|  15
                BYTE    %11111000 ; |XXXXX   |  14
                BYTE    %11111110 ; |XXXXXXX |  13
                BYTE    %11111111 ; |XXXXXXXX|  12
                BYTE    %10011111 ; |X  XXXXX|  11
                BYTE    %11011111 ; |XX XXXXX|  10
                BYTE    %10001111 ; |X   XXXX|   9
                BYTE    %10011111 ; |X  XXXXX|   8
                BYTE    %11111110 ; |XXXXXXX |   7
                BYTE    %11111100 ; |XXXXXX  |   6
                BYTE    %11111100 ; |XXXXXX  |   5
                BYTE    %11111000 ; |XXXXX   |   4
                BYTE    %11100000 ; |XXX     |   3
                BYTE    %11000000 ; |XX      |   2
                BYTE    %10000000 ; |X       |   1
                BYTE    %00000000 ; |        |   0

IN_KongGrin_GRP4_1 = IN_KongStand_GRP4_1

IN_KongGrin_GRP5_1 = IN_EmptySpace

;
; Kong jumping
;

IN_KongJump_GRP0_0 = IN_KongClimb1_GRP0_0 + 4

IN_KongJump_GRP1_0:

                BYTE    %00000000 ; |        |  31
                BYTE    %00000000 ; |        |  30
                BYTE    %00000000 ; |        |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %11111000 ; |XXXXX   |  27
                BYTE    %11111100 ; |XXXXXX  |  26
                BYTE    %11111110 ; |XXXXXXX |  25
                BYTE    %01111110 ; | XXXXXX |  24
                BYTE    %00011100 ; |   XXX  |  23
                BYTE    %00000000 ; |        |  22
                BYTE    %00000011 ; |      XX|  21
                BYTE    %00000100 ; |     X  |  20
                BYTE    %00001000 ; |    X   |  19
                BYTE    %00010000 ; |   X    |  18
                BYTE    %00000000 ; |        |  17
                BYTE    %10000000 ; |X       |  16
                BYTE    %00000000 ; |        |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %00000000 ; |        |  13
                BYTE    %00000000 ; |        |  12
                BYTE    %00000000 ; |        |  11
                BYTE    %00000000 ; |        |  10
                BYTE    %00000000 ; |        |   9
                BYTE    %00000000 ; |        |   8
                BYTE    %00111001 ; |  XXX  X|   7
                BYTE    %11011110 ; |XX XXXX |   6
                BYTE    %00001100 ; |    XX  |   5
                BYTE    %00000110 ; |     XX |   4
                BYTE    %00000010 ; |      X |   3
                BYTE    %00000111 ; |     XXX|   2
                BYTE    %00001101 ; |    XX X|   1
                BYTE    %00110110 ; |  XX XX |   0

IN_KongJump_GRP2_0 = IN_KongClimb1_GRP2_0 + 4

IN_KongJump_GRP3_0:

                BYTE    %11111111 ; |XXXXXXXX|  31
                BYTE    %11111110 ; |XXXXXXX |  30
                BYTE    %11111000 ; |XXXXX   |  29
                BYTE    %11100000 ; |XXX     |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %00000000 ; |        |  25
                BYTE    %00000000 ; |        |  24
                BYTE    %00000000 ; |        |  23
                BYTE    %10000000 ; |X       |  22
                BYTE    %11000000 ; |XX      |  21
                BYTE    %00100000 ; |  X     |  20
                BYTE    %00010110 ; |   X XX |  19
                BYTE    %00001111 ; |    XXXX|  18
                BYTE    %00001111 ; |    XXXX|  17
                BYTE    %00011111 ; |   XXXXX|  16
                BYTE    %00011111 ; |   XXXXX|  15
                BYTE    %00001111 ; |    XXXX|  14
                BYTE    %00000111 ; |     XXX|  13
                BYTE    %00000001 ; |       X|  12
                BYTE    %00000000 ; |        |  11
                BYTE    %00000000 ; |        |  10
                BYTE    %00000000 ; |        |   9
                BYTE    %00000000 ; |        |   8
                BYTE    %10000000 ; |X       |   7
                BYTE    %01100000 ; | XX     |   6
                BYTE    %00011111 ; |   XXXXX|   5
                BYTE    %00100000 ; |  X     |   4
                BYTE    %01000000 ; | X      |   3
                BYTE    %01000000 ; | X      |   2
                BYTE    %10000000 ; |X       |   1
                BYTE    %00000000 ; |        |   0

IN_KongJump_GRP4_0 = IN_KongClimb1_GRP4_0 + 4

IN_KongJump_GRP5_0 = IN_KongClimb1_GRP5_0 + 4

IN_KongJump_GRP0_1 = IN_KongClimb1_GRP0_1 + 4

IN_KongJump_GRP1_1:

                BYTE    %00000000 ; |        |  31
                BYTE    %00000000 ; |        |  30
                BYTE    %00000000 ; |        |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %10000000 ; |X       |  26
                BYTE    %11000000 ; |XX      |  25
                BYTE    %11100010 ; |XXX   X |  24
                BYTE    %11111111 ; |XXXXXXXX|  23
                BYTE    %11111111 ; |XXXXXXXX|  22
                BYTE    %11111100 ; |XXXXXX  |  21
                BYTE    %11111011 ; |XXXXX XX|  20
                BYTE    %11110111 ; |XXXX XXX|  19
                BYTE    %11101111 ; |XXX XXXX|  18
                BYTE    %11111111 ; |XXXXXXXX|  17
                BYTE    %01111111 ; | XXXXXXX|  16
                BYTE    %01111111 ; | XXXXXXX|  15
                BYTE    %01111111 ; | XXXXXXX|  14
                BYTE    %00111111 ; |  XXXXXX|  13
                BYTE    %01111111 ; | XXXXXXX|  12
                BYTE    %11111111 ; |XXXXXXXX|  11
                BYTE    %11111111 ; |XXXXXXXX|  10
                BYTE    %11111111 ; |XXXXXXXX|   9
                BYTE    %11111111 ; |XXXXXXXX|   8
                BYTE    %11000110 ; |XX   XX |   7
                BYTE    %00000001 ; |       X|   6
                BYTE    %11100011 ; |XXX   XX|   5
                BYTE    %11110001 ; |XXXX   X|   4
                BYTE    %11111001 ; |XXXXX  X|   3
                BYTE    %11111100 ; |XXXXXX  |   2
                BYTE    %11111100 ; |XXXXXX  |   1
                BYTE    %11111110 ; |XXXXXXX |   0

IN_KongJump_GRP2_1 = IN_KongClimb1_GRP2_1 + 4

IN_KongJump_GRP3_1:

                BYTE    %00000111 ; |     XXX|  31
                BYTE    %00001111 ; |    XXXX|  30
                BYTE    %00111110 ; |  XXXXX |  29
                BYTE    %11111100 ; |XXXXXX  |  28
                BYTE    %11111100 ; |XXXXXX  |  27
                BYTE    %11111110 ; |XXXXXXX |  26
                BYTE    %11111111 ; |XXXXXXXX|  25
                BYTE    %11111111 ; |XXXXXXXX|  24
                BYTE    %11111111 ; |XXXXXXXX|  23
                BYTE    %01111111 ; | XXXXXXX|  22
                BYTE    %00111111 ; |  XXXXXX|  21
                BYTE    %11011111 ; |XX XXXXX|  20
                BYTE    %11101001 ; |XXX X  X|  19
                BYTE    %11110000 ; |XXXX    |  18
                BYTE    %11110000 ; |XXXX    |  17
                BYTE    %11111000 ; |XXXXX   |  16
                BYTE    %11100001 ; |XXX    X|  15
                BYTE    %11110000 ; |XXXX    |  14
                BYTE    %11111111 ; |XXXXXXXX|  13
                BYTE    %11111110 ; |XXXXXXX |  12
                BYTE    %11111111 ; |XXXXXXXX|  11
                BYTE    %11111111 ; |XXXXXXXX|  10
                BYTE    %11111111 ; |XXXXXXXX|   9
                BYTE    %11111111 ; |XXXXXXXX|   8
                BYTE    %01111111 ; | XXXXXXX|   7
                BYTE    %10011111 ; |X  XXXXX|   6
                BYTE    %11100000 ; |XXX     |   5
                BYTE    %11000000 ; |XX      |   4
                BYTE    %10000000 ; |X       |   3
                BYTE    %10000000 ; |X       |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

IN_KongJump_GRP4_1 = IN_KongClimb1_GRP4_1 + 4

IN_KongJump_GRP5_1 = IN_KongClimb1_GRP5_1 + 4

; --------------------------------------------------------------------

; configuration for stage 1 and L=1

IN_DemoConfTab: BYTE    120                     ; BonusTimer
                BYTE    $00                     ; DemoMode
                BYTE    2                       ; ItemIndex
                BYTE    $E1                     ; ItemPos
                BYTE    >(B3_Oil_GRP_0)         ; ItemGRPH
                BYTE    >(B3_Oil_COL_0)         ; ItemCOLH
                BYTE    $01                     ; TempLevel
              ; BYTE    $00                     ; TempStage

; ====================================================================
;       Bank 1 Draw Kong
; ====================================================================

B1_KongOffsets: BYTE    0*2, 1*2, 4*2, 5*2, 6*2, 7*2

B1_DrawKong:    SUBROUTINE

.switch         lda     FrameCtr
                lsr
                tya
                bcc     .skip
                adc     #6-1
.skip           tay

#if (B1_KongPtr) != (ST_MarioCOLPtr)
                ECHO    "ERROR: Wrong B1_KongPtr address"
#endif

                STOREBYTE 5, Temp
                clc
.loop0          ldx     Temp
                lda     B1_KongOffsets,x
                tax
                lda     LineCtr
                and     #%00000111
                adc     IN_KongGRP_L,y
                sta     B1_KongPtr,x
                lda     IN_KongGRP_H,y
                sta     B1_KongPtr+1,x
                dey
                dec     Temp
                bpl     .loop0

                sta     WSYNC
                lda     LineCtr
                lsr
                lsr
                lsr
                sta     LineCtr

.loop1          ldy     LineCtr
                lda     (B1_KongPtr+0*2),y
                sta     GRP0
                sta     WSYNC
                lda     (B1_KongPtr+1*2),y
                sta     GRP1
                lda     (B1_KongPtr+4*2),y
                sta     GRP0
                lda     (B1_KongPtr+5*2),y
                sta     Temp
                lda     (B1_KongPtr+6*2),y
                tax
                lda     (B1_KongPtr+7*2),y
                tay
                lda     Temp
                sta     GRP1
                stx     GRP0
                sty     GRP1
                sta     GRP0
                dec     LineCtr
                bpl     .loop1
                rts

; ********************************************************************

#if PRINT_MESSAGES
                ECHO "---- Bank 1: ", (ROMJumpTable - *), "bytes of ROM left"
#endif
