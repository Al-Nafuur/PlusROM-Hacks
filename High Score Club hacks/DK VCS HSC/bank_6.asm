
; ********************************************************************
;  Donkey Kong VCS
;
;    ROM Bank 6
;
;    $Date: Thu, 05 Jan 2017 20:55:15 +0100 $
;    $Author: dietrich $
;    $Revision: 477 $
;
;  Copyright (C) 2017 Andreas Dietrich
; ********************************************************************

;---------------------------------------------------------------------
;       EN Constants
;---------------------------------------------------------------------

EN_GIRDER_COL_0         = DK_COL_BLUE   ; dark girder color  |181AA7|
EN_GIRDER_COL_1         = DK_COL_CYAN   ; light girder color |84E0FC|
EN_HEART_COL            = DK_COL_PINK   ; heart color

EN_MARIO_START          = 4             ; show Mario 32 frames after Kong hit (4 anim steps)
EN_HEART_START          = 8             ; show heart 64 frames after Kong hit (8 anim steps)

;---------------------------------------------------------------------
;       EN Variables
;---------------------------------------------------------------------

EN_VarStart = $E0

EN_RAMCode              = $A0 ; [$40]   ; RAM for Bitmap48 code

EN_FrameIdx             = $E0           ; Frame index (0=even, 1=odd)

EN_PF1                  = $E1           ; PF1 shadow register
EN_PF2                  = $E2           ; PF2 shadow register
EN_GRP0                 = $E3           ; GRP0 shadow register
EN_GRP1                 = $E4           ; GRP1 shadow register

EN_KongAnimState        = $E5           ; Kong animation state
EN_KongAnimTimer        = $E6           ; Kong animation timer
EN_KongAnimLoop         = $E7           ; Kong animation loop counter
EN_KongStarIdx          = $E8           ; Kong star position index

EN_LinesLo              = $E9           ; lines below Kong
EN_LinesHi              = $EA           ; lines above Kong

EN_HeartColor           = $EB           ; heart color
EN_MarioCol0Ptr         = $EC ; [2]     ; Mario color 0 pointer
EN_MarioCol1Ptr         = $EE ; [2]     ; Mario color 1 pointer


; ********************************************************************
;
;       Code Section
;
; ********************************************************************

; ====================================================================
;       Ending
; ====================================================================

Ending:         STOREBYTE 6, Bank

; --------------------------------------------------------------------
;       EN Init
; --------------------------------------------------------------------

EN_Init:        SUBROUTINE

                ; clear RAM and TIA registers

                CLEAR_RAM EN_VarStart, StackBottom
                CLEAR_TIA

                ; zero variables

                sta     EventCtr

                ; non-zero variables

                lda     #<EN_Empty16
                sta     EN_MarioCol0Ptr
                sta     EN_MarioCol1Ptr
                lda     #>EN_Empty16
                sta     EN_MarioCol0Ptr+1
                sta     EN_MarioCol1Ptr+1

                ; copy Bitmap48 code to RAM

                ldx     #(EN_Bitmap48End-EN_Bitmap48)-1
.loop           lda     EN_Bitmap48,x
                sta     EN_RAMCode,x
                dex
                bpl     .loop

                ; noise player

                lda     #MUSIC_INIT_ENDING_INTRO
                jsr     Bank5_DriveAudio

.wait           lda     INTIM
                bne     .wait

; --------------------------------------------------------------------
;       EN VSync
; --------------------------------------------------------------------

EN_VSync:       VERTICAL_SYNC

; --------------------------------------------------------------------
;       EN VBlank
; --------------------------------------------------------------------

EN_VBlank:      lda     FrameCtr
                and     #%00000001
                sta     EN_FrameIdx

; --------------------------------------------------------------------

EN_AnimKong:    SUBROUTINE

                ldy     EN_KongAnimState

                ; progress animation time

                dec     EN_KongAnimTimer
                bpl     .move

                ; progress animation state

.command        iny
                lda     EN_KongGfxTab-1,y
                bpl     .write
                dec     EN_KongAnimLoop
                beq     .command
                bpl     .jump
                lda     EN_KongTimeTab-1,y
                sta     EN_KongAnimLoop
.jump           lda     EN_KongVTab-1,y
                tay
.write          sty     EN_KongAnimState
.timer          lda     EN_KongTimeTab-1,y
                sta     EN_KongAnimTimer
                inc     EventCtr

                ; update position

.move           ldx     EventCtr
                clc
                lda     EN_KongVTab-1,y
                and     #EN_KONG_VMOVE_MASK
                adc     EN_LinesHi
                sta     EN_LinesHi
                sec
                lda     #81
                sbc     EN_LinesHi
                sta     EN_LinesLo

; --------------------------------------------------------------------

EN_AnimStar:    SUBROUTINE

                ; animate star as long as Kong rolls eyes

                cpy     #14+1
                bcs     .end

                ; move star left and right

                txa
                and     #%00000001
                sta     EN_KongStarIdx
.end

; --------------------------------------------------------------------

EN_AnimMP:      SUBROUTINE

                ; start Mario and Pauline's sequence

                cpy     #8+1
                bcc     .end

                ; show Mario

.mario          cpx     #20+EN_MARIO_START
                bne     .heart

                lda     #<EN_Mario_COL0
                sta     EN_MarioCol0Ptr
                lda     #<EN_Mario_COL1
                sta     EN_MarioCol1Ptr
                lda     #>EN_Mario_COL0
                sta     EN_MarioCol0Ptr+1
                sta     EN_MarioCol1Ptr+1

                ; show heart

.heart          cpx     #20+EN_HEART_START
                bne     .end
                lda     #EN_HEART_COL
                sta     EN_HeartColor
.end

; --------------------------------------------------------------------

EN_Audio:       SUBROUTINE

                lda     #MUSIC_PLAY             ; default

                ; Kong falls

.sound_fall     cpy     #6+1
                bcc     .drive
                bne     .music_part2

                lda     #AUDIO_CLEAR
                jsr     Bank5_DriveAudio
                lda     #SOUND_INIT_KONG_FALL
                bne     .drive

                ; play music once heart appears

.music_part2    cpx     #20+EN_HEART_START
                bcc     .sound
                bne     .drive

.ending_c       ldx     #MUSIC_INIT_ENDING_C    ; ending music C (easter egg)
                lda     Level_Round
                and     #LEVEL_MASK
                cmp     #$90
                bcc     .ending_b
                ldy     Score+0
                cpy     #$10
                bcs     .set
.ending_b       dex                             ; ending music B
                and     #ROUND_MASK+1
                beq     .set
.ending_a       dex                             ; ending music A
.set            txa
                bne     .drive

                ; keep driving sounds

.sound          lda     #SOUND_PLAY
.drive          jsr     Bank5_DriveAudio

; --------------------------------------------------------------------

EN_SetupB48:    SUBROUTINE

                ldy     EN_KongAnimState
                ldx     EN_KongGfxTab-1,y

                ; alternate between even and odd frames

.switch         lda     EN_FrameIdx
                beq     .write
                txa
                clc
                adc     #7
                tax

                ; update GRP pointers

.write          STOREBYTE 7-1, Temp
.loop           ldy     Temp
                lda     EN_B48Offsets,y
                tay
                lda     EN_KongGRP_L,x
                sta     EN_RAMCode,y
                lda     EN_KongGRP_H,x
                sta     EN_RAMCode+1,y
                dex
                dec     Temp
                bpl     .loop

EN_VBlankEnd:   lda     INTIM
                bne     EN_VBlankEnd

; --------------------------------------------------------------------
;       EN Kernel
; --------------------------------------------------------------------

;
; Part 1 -- Score
;

EN_Score:       jsr     Bank7_DrawScore

; - Switch -----------------------------------------------------------

EN_Switch:      lda     EN_KongAnimState
                cmp     #8+1
                bcs     EN_Kernel_1

; - Kernel 0 ---------------------------------------------------------

EN_Kernel_0:    SUBROUTINE

;
; Part 2 -- Kong
;

                ; skip lines above Kong

EN_KongSkipHi_0:SUBROUTINE
                ldx     EN_LinesHi
.loop           sta     WSYNC
                dex
                bpl     .loop

                ; draw Kong

EN_DrawKong_0:  jsr     EN_DrawKong

                ; skip lines below Kong

EN_KongSkipLo_0:SUBROUTINE
                ldx     EN_LinesLo
.loop           sta     WSYNC
                lda     #0
                sta     GRP0
                sta     GRP1
                sta     GRP0
                dex
                bpl     .loop

EN_KernelEnd_0: ldx     #4
                jmp     EN_GirdersBot

; - Kernel 1 ---------------------------------------------------------

EN_Kernel_1:    SUBROUTINE

;
; Part 2 -- Mario & Pauline
;

                ; position Mario and Pauline

EN_PosMP_1:     ldx     #4                      ; BL
                lda     #86
                jsr     EN_HPosBZNoHMOVE
                dex                             ; M1
                lda     #70
                jsr     EN_HPosBZNoHMOVE
                ldx     #1                      ; P1
                lda     #84
                jsr     EN_HPosBZNoHMOVE
                dex
                lda     #76                     ; P0
                jsr     EN_HPosBZNoHMOVE
                sta     WSYNC
                sta     HMOVE

                ; setup Mario and Pauline

EN_SetupMP_1:   stx     COLUPF                  ; X = %00000000
                stx     NUSIZ0
                stx     VDELP1                  ; OK to delay P0
                stx     EN_PF2
                dex                             ; X := %11111111
                stx     ENABL
                ldx     #%11111100
                stx     EN_PF1
                stx     HMCLR

                ; draw Mario and Pauline

EN_DrawMP_1:    SUBROUTINE

                ; draw heart and upper part of Pauline

                ldy     #13
.loop0          lda     EN_Pauline_HMBL+16,y
                sta     HMBL
                lda     EN_MarioPauline_HMP0_HMM1_NUSIZ+16,y
                sta     HMP0
                asl
                asl
                sta     WSYNC
                sta     HMOVE
                sta     NUSIZ1
                sta     CTRLPF
                lda     EN_Mario_GRP+16,y
                sta     GRP0
                lda     EN_HeartColor
                sta     COLUP0
                lda     EN_Pauline_GRP+16,y
                sta     GRP1
                lda     EN_Pauline_COL0+16,y
                sta     COLUPF
                lda     EN_Pauline_COL1+16,y
                sta     COLUP1
                dey
                bpl     .loop0

                ; draw Mario and lower part of Pauline

                lda     #%00000010
                sta     ENAM1

                ldy     #15
.loop1          lda     EN_Pauline_HMBL,y
                sta     HMBL
                lda     EN_MarioPauline_HMP0_HMM1_NUSIZ,y
                sta     HMM1
                asl
                asl
                sta     NUSIZ1
                sta     CTRLPF
                sta     HMOVE
                lda     EN_Mario_GRP,y
                sta     GRP0
                lda     (EN_MarioCol0Ptr),y
                sta     COLUP0
                lda     (EN_MarioCol1Ptr),y
                sta     COLUP1
                lda     EN_Pauline_GRP,y
                sta     GRP1
                lda     EN_Pauline_COL0,y
                sta     COLUPF
                lda     EN_Pauline_COL1,y
                sta     COLUP1
                dey
                bpl     .loop1

;
; Part 3 -- Girder below Mario
;

                ; draw girder

EN_GirderTop_1: jsr     EN_DrawGirder

                sta     WSYNC
                sty     COLUBK
                sty     PF0
                sty     PF1

                ; position ball behind star

                ldx     #4
                ldy     EN_KongStarIdx
                lda     EN_KongStarPos,y
                jsr     EN_HPosBZNoHMOVE
                sta     WSYNC
                sta     HMOVE

;
; Part 4 -- Kong
;

                ; skip lines above Kong

EN_KongSkipHi_1:SUBROUTINE
                ldy     #36
.loop           sta     WSYNC
                sta     HMCLR
                dey
                bne     .loop

                ; draw Kong

EN_DrawKong_1:  jsr     EN_DrawKong

;
; Part 5 -- Star and girder below Kong
;

EN_DrawStar_1:  SUBROUTINE

                ; draw first girder line (no star)

                lda     #EN_GIRDER_COL_0
                sta     COLUP0
                sta     COLUP1
                lda     #DK_COL_YELLOW
                sta     COLUPF
                lda     #%00110001
                sta     CTRLPF
                dex                             ; X := %11111111
                stx     GRP0
                stx     GRP1
                stx     GRP0
                stx     ENABL

                ; draw remaining girder lines (plus star)

                ldy     #6
.loop           ldx     EN_KongStarIdx
                lda     EN_KongStar_GRP,y
                sta     EN_GRP0,x
                txa
                sta     WSYNC
                eor     #1
                tax
                lda     EN_Girder_COL,y
                sta     COLUP0
                sta     COLUP1
                lda     EN_Girder_GRP,y
                sta     EN_GRP0,x
                sta     GRP1
                sta     GRP0
                ldx     EN_GRP1
                stx     GRP1
                nop
                nop
                ldx     EN_GRP0
                stx     GRP0
                sta     GRP1
                sta     GRP0
                sta     GRP1
                dey
                bpl     .loop

EN_KernelEnd_1: ldx     #3

; - Kernel common ----------------------------------------------------

;
; Part 3/6 -- Girders below Kong
;

EN_GirdersBot:  SUBROUTINE

                ; draw short girder stack

.loop0          ldy     #7
.loop1          sta     WSYNC
                lda     #0
                sta     COLUP0
                sta     COLUP1
                lda     #%11111100
                sta     PF2
                lda     EN_Girder_COL,y
                sta     COLUPF
                lda     EN_Girder_GRP,y
                eor     #$FF
                sta     GRP0
                sta     GRP1
                sta     GRP0
                dey
                bpl     .loop1
                dex
                bne     .loop0

                ; draw long girder

                STOREBYTE %11000000, EN_PF1
                STOREBYTE %00000000, EN_PF2

                jsr     EN_DrawGirder

; --------------------------------------------------------------------
;       EN Overscan
; --------------------------------------------------------------------

EN_Overscan:    OVERSCAN                        ; Standard overscan

                CHECK_RESET

                inc     FrameCtr

                ; increase level

.level          ldy     EN_KongAnimState        ; Kong stopped rolling eyes
                cpy     #13+1
                bne     .intermission
                ldx     EN_KongAnimTimer        ; wait 8 frames
                cpx     #114-1-8
                bne     .intermission

                lda     Level_Round
                and     #LEVEL_MASK
                cmp     #$90
                bcs     .write
                adc     #$10
.write          ora     #ROUND_NEW
                sta     Level_Round

                ; add bonus to score

.bonus          sed
                clc
                lda     Bonus
                adc     Score+1
                sta     Score+1
                lda     #0
                adc     Score+0
                sta     Score+0
                cld
                bcc     .intermission
                lda     #$99
                sta     Score+0
                sta     Score+1
                sta     Score+2

                ; go to intermission

.intermission   cpy     #15+1
                bcc     EN_OverscanEnd
                jmp     Bank7_Intermission

EN_OverscanEnd: lda     INTIM
                bne     EN_OverscanEnd
                jmp     EN_VSync

; --------------------------------------------------------------------
;       EN Subroutines
; --------------------------------------------------------------------

EN_RAM_B48Count  =  9
EN_RAM_B48Comp   = 48
EN_RAM_B48Branch = 59

EN_DrawKong:    SUBROUTINE

                ; setup playfield and players

                jsr     EN_CenterKong

                lda     #KONG_COL_0
                ldx     EN_FrameIdx
                beq     .write
                lda     #KONG_COL_1
.write          sta     COLUP0
                sta     COLUP1
                lda     #DK_COL_WHITE
                sta     COLUPF
                ldx     #%00000011
                stx     NUSIZ0
                stx     NUSIZ1
                stx     VDELP0
                stx     VDELP1

                ; mirror mode

                ldy     EN_KongAnimState
                lda     EN_KongVTab-1,y

.horizontal     asl
                bcc     .hwrite
                ldx     #%00001000
.hwrite         stx     REFP0
                stx     REFP1

.vertical       asl
                lda     #$C6                    ; dec
                ldx     #1                      ; last line
                ldy     #$B0                    ; bcs
                bcc     .vwrite0
                lda     #$E6                    ; inc
                ldx     #31                     ; last line
                ldy     #$90                    ; bcs
.vwrite0        sta     EN_RAMCode+EN_RAM_B48Count
                stx     EN_RAMCode+EN_RAM_B48Comp
                sty     EN_RAMCode+EN_RAM_B48Branch
                lda     #31
                bcc     .vwrite1
                lda     #-2
.vwrite1        sta     LineCtr

                ; draw Kong

                jmp     IN_RAMCode

; --------------------------------------------------------------------

EN_DrawGirder:  SUBROUTINE

                lda     #%00010001
                sta     CTRLPF
                ldx     #0
                stx     COLUPF

                ; draw girder line 0

                sta     WSYNC
                sty     PF0                     ; Y = %11111111
                lda     EN_PF1
                sta     PF1
                lda     EN_PF2
                sta     PF2
                lda     #EN_GIRDER_COL_0
                sta.w   COLUBK
                stx     COLUP0
                stx     COLUP1
                stx     ENABL
                stx     ENAM1
                sta     RESM0                   ; position M0
                stx     GRP0
                stx     GRP1
                stx     GRP0
                sta     RESBL                   ; position BL
                sty     HMM0                    ; Y = $FF
                sty     HMBL

                ; draw girder line 1

                sta     WSYNC
                sta     HMOVE
                lda     #EN_GIRDER_COL_1
                sta     COLUBK
                lda     #%00010011
                sta     NUSIZ0
                sta     NUSIZ1
                sta     REFP0

                ldx     #6
.delay          dex
                bne     .delay

                sty     HMCLR
                sty     HMM1

                ; draw girder lines 2 - 5

                ldy     #4
.loop           lda     #%00000010
                sta     ENABL
                sta     ENAM0
                sta     ENAM1
                sta     RESM1

                sta     WSYNC
                sta     HMOVE
                lda     #EN_GIRDER_COL_0
                sta     COLUBK
                lda     #%00110011
                sta     GRP0
                asl
                sta     GRP1
                sta     GRP0

                sta     RESP0
                sta     RESP1
                sta     RESP0
                sta     RESP1
                cmp     $FF
                sta     RESM1
                nop
                sta     RESP0
                sta     RESP1
                sta     RESP0
                sta     RESP1
                dey
                bne     .loop

                ; draw girder line 6

                sta     WSYNC
                lda     #EN_GIRDER_COL_0
                sta     COLUBK
                sty     ENAM0
                sty     ENAM1
                sty     ENABL
                sty     GRP0
                sty     GRP1
                sty     GRP0

                ; draw girder line 7

                sta     WSYNC
                lda     #EN_GIRDER_COL_1
                sta     COLUBK
                rts

; --------------------------------------------------------------------

EN_CenterKong:  SUBROUTINE
                ldx     #6
                sta     WSYNC
.loop           dex
                bpl     .loop
                sta     RESP1
                sta     RESP0
                lda     #$80
                sta     HMP1
                lda     #$90
                sta     HMP0
                sta     WSYNC
                sta     HMOVE
                sta     WSYNC                   ; reserve
                rts

; --------------------------------------------------------------------

; 48 bit sprites

EN_RAM_B48TIA0  = 17
EN_RAM_B48TIA1  = 19
EN_RAM_B48BG    = 14
EN_RAM_B48Char0 = 21
EN_RAM_B48Char1 = 26
EN_RAM_B48Char2 = 31
EN_RAM_B48Char3 = 36
EN_RAM_B48Char4 = 41
EN_RAM_B48Char5 = 45

        MAC EN_BITMAP_48

                sta     WSYNC
                ldx     #9
.loop0          dex
                bpl     .loop0

                inc     LineCtr
.loop1          dec     LineCtr
                ldy     LineCtr
                lda     {3},y                   ; background
                sta     {1}                     ; 1. TIA register
                sta     {2}                     ; 2. TIA register
                lda     {4},y                   ; 1. character
                sta     GRP1
                lda     {5},y                   ; 2. character
                sta     GRP0
                lda     {6},y                   ; 3. character
                sta     GRP1
                lda     {7},y                   ; 4. character
                sta     Temp
                lda     {8},y                   ; 5. character
                tax
                lda     {9},y                   ; 6. character
                cpy     #1
                ldy     Temp
                sty     GRP0
                stx     GRP1
                sta     GRP0
                sta     GRP1

                bcs     .loop1
                dec     LineCtr
                rts

        ENDM

        MAC MAKE_BITMAP_48

arg0 SET PF2
arg1 SET PF2
arg2 SET {2}_PF2_0
arg3 SET {2}_GRP0_0
arg4 SET {2}_GRP1_0
arg5 SET {2}_GRP2_0
arg6 SET {2}_GRP3_0
arg7 SET {2}_GRP4_0
arg8 SET {2}_GRP5_0
arg9 SET $FF

{1}_0:          EN_BITMAP_48 arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9

arg0 SET PF2
arg1 SET PF2
arg2 SET {2}_PF2_1
arg3 SET {2}_GRP0_1
arg4 SET {2}_GRP1_1
arg5 SET {2}_GRP2_1
arg6 SET {2}_GRP3_1
arg7 SET {2}_GRP4_1
arg8 SET {2}_GRP5_1
arg9 SET $FF

{1}_1:          EN_BITMAP_48 arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9

        ENDM

EN_Bitmap48    = EN_DrawKongDrum1_0
EN_Bitmap48End = EN_DrawKongDrum1_1

                ALIGN   $0100

                MAKE_BITMAP_48 EN_DrawKongDrum0, EN_KongDrum0
                MAKE_BITMAP_48 EN_DrawKongDrum1, EN_KongDrum1

                ALIGN   $0100

                MAKE_BITMAP_48 EN_DrawKongStand, EN_KongStand
                MAKE_BITMAP_48 EN_DrawKongGrin,  EN_KongGrin

; ====================================================================
;       Bank 6 Draw Kong
; ====================================================================

B6_DrawKong:    SUBROUTINE

                ; setup playfield and players

                jsr     EN_CenterKong

                lda     #DK_COL_WHITE
                sta     COLUPF
                lda     #%00010001
                sta     CTRLPF
                lda     #%00010011
                sta     NUSIZ0
                sta     NUSIZ1

                ; draw alternating Kong planes 0/1

                ldx     #KONG_COL_0
                lda     FrameCtr
                and     #%00000001
                bne     .write
                ldx     #KONG_COL_1
.write          stx     COLUP0
                stx     COLUP1

                cpy     #EN_KONG_DRUM0
                beq     B6_DrwKongDrum0
                cpy     #EN_KONG_DRUM1
                beq     B6_DrwKongDrum1
                cpy     #EN_KONG_STAND
                beq     B6_DrwKongStand

B6_DrwKongGrin: SUBROUTINE
                lsr
                bcc     .plane1
.plane0         jmp     EN_DrawKongGrin_0
.plane1         jmp     EN_DrawKongGrin_1

B6_DrwKongStand:SUBROUTINE
                lsr
                bcc     .plane1
.plane0         jmp     EN_DrawKongStand_0
.plane1         jmp     EN_DrawKongStand_1

B6_DrwKongDrum1:SUBROUTINE
                lsr
                bcc     .plane1
.plane0         jmp     EN_DrawKongDrum1_0
.plane1         jmp     EN_DrawKongDrum1_1

B6_DrwKongDrum0:SUBROUTINE
                ldx     #%00001000
                stx     REFP0
                stx     REFP1
                lsr
                bcc     .plane1
.plane0         jmp     EN_DrawKongDrum0_0
.plane1         jmp     EN_DrawKongDrum0_1


; ********************************************************************
;
;       Imports
;
; ********************************************************************

; sprite positioning

EN_HPosBZNoHMOVE: FUNC_HPosBZNoHMOVE


; ********************************************************************
;
;       Data Section
;
; ********************************************************************

; --------------------------------------------------------------------
;       EN Macros
; --------------------------------------------------------------------

        MAC EN_MAKE_ANIM_STEP

EN_ANIM_{1}_VVAL SET {2}
EN_ANIM_{1}_TVAL SET {3}
EN_ANIM_{1}_GVAL SET {4}

        ENDM

        MAC EN_MAKE_ANIM_ARRAY

                BYTE EN_ANIM_00_{1},EN_ANIM_01_{1},EN_ANIM_02_{1},EN_ANIM_03_{1}
                BYTE EN_ANIM_04_{1},EN_ANIM_05_{1},EN_ANIM_06_{1},EN_ANIM_07_{1}
                BYTE EN_ANIM_08_{1},EN_ANIM_09_{1},EN_ANIM_10_{1},EN_ANIM_11_{1}
                BYTE EN_ANIM_12_{1},EN_ANIM_13_{1},EN_ANIM_14_{1},EN_ANIM_15_{1}

        ENDM

        MAC EN_MAKE_B48_LOBYTE_TABLE

                BYTE <{1}_PF2_0,<{1}_GRP0_0,<{1}_GRP1_0,<{1}_GRP2_0,<{1}_GRP3_0,<{1}_GRP4_0,<{1}_GRP5_0
                BYTE <{1}_PF2_1,<{1}_GRP0_1,<{1}_GRP1_1,<{1}_GRP2_1,<{1}_GRP3_1,<{1}_GRP4_1,<{1}_GRP5_1

        ENDM

        MAC EN_MAKE_B48_HIBYTE_TABLE

                BYTE >{1}_PF2_0,>{1}_GRP0_0,>{1}_GRP1_0,>{1}_GRP2_0,>{1}_GRP3_0,>{1}_GRP4_0,>{1}_GRP5_0
                BYTE >{1}_PF2_1,>{1}_GRP0_1,>{1}_GRP1_1,>{1}_GRP2_1,>{1}_GRP3_1,>{1}_GRP4_1,>{1}_GRP5_1

        ENDM

; --------------------------------------------------------------------
;       EN Tables
; --------------------------------------------------------------------

; Kong animation tables

EN_KONG_HIT0       = (0*14)+6
EN_KONG_HIT1       = (1*14)+6
EN_KONG_HIT2       = (2*14)+6
EN_KONG_DRUM0      = (3*14)+6
EN_KONG_DRUM1      = (4*14)+6
EN_KONG_STAND      = (5*14)+6
EN_KONG_GRIN       = (6*14)+6

EN_KONG_OFF        = %00000000
EN_KONG_LOOP       = %11111111
EN_KONG_MIRROR_X   = %10000000
EN_KONG_MIRROR_Y   = %01000000
EN_KONG_VMOVE_MASK = %00111111

                ;                 step          v-move             time      graphics
                ;
                ; - [ start ] -------------------------------------------------------------
                EN_MAKE_ANIM_STEP  00,                     0,     40-1,     EN_KONG_STAND
                ;
                ; - [ drum ] --------------------------------------------------------------
                EN_MAKE_ANIM_STEP  01,   EN_KONG_MIRROR_X| 0,      8-1,     EN_KONG_DRUM0 ;     <-+
                EN_MAKE_ANIM_STEP  02,                     0,      8-1,     EN_KONG_DRUM1 ;       |
                EN_MAKE_ANIM_STEP  03,                   1+1,      7-1,     EN_KONG_LOOP  ; loop -+ (x7)
                ;
                EN_MAKE_ANIM_STEP  04,   EN_KONG_MIRROR_X| 0,      8-1,     EN_KONG_DRUM0
                ;
                ; - [ wait ] --------------------------------------------------------------
                EN_MAKE_ANIM_STEP  05,                     0,     32-1,     EN_KONG_STAND
                ;
                ; - [ fall ] --------------------------------------------------------------
                EN_MAKE_ANIM_STEP  06,   EN_KONG_MIRROR_Y|32,      1-1,     EN_KONG_GRIN
                EN_MAKE_ANIM_STEP  07,   EN_KONG_MIRROR_Y| 1,     49-1,     EN_KONG_GRIN
                ;
                ; - [ hit ] ---------------------------------------------------------------
                EN_MAKE_ANIM_STEP  08,   EN_KONG_MIRROR_Y| 0,      8-1,     EN_KONG_HIT0  ;     <-+
                EN_MAKE_ANIM_STEP  09,   EN_KONG_MIRROR_Y| 0,      8-1,     EN_KONG_HIT1  ;       |
                EN_MAKE_ANIM_STEP  10,   EN_KONG_MIRROR_Y| 0,      8-1,     EN_KONG_HIT2  ;       |
                EN_MAKE_ANIM_STEP  11,                   8+1,     10-1,     EN_KONG_LOOP  ; loop -+ (x10)
                ;
                EN_MAKE_ANIM_STEP  12,   EN_KONG_MIRROR_Y| 0,      8-1,     EN_KONG_HIT0
                EN_MAKE_ANIM_STEP  13,   EN_KONG_MIRROR_Y| 0,    114-1,     EN_KONG_HIT1
                EN_MAKE_ANIM_STEP  14,   EN_KONG_MIRROR_Y| 0,    114-1,     EN_KONG_HIT1
                ;
                ; - [ end ] ---------------------------------------------------------------
                EN_MAKE_ANIM_STEP  15,   EN_KONG_MIRROR_Y| 0,      255,     EN_KONG_HIT1

EN_KongVTab:    EN_MAKE_ANIM_ARRAY VVAL
EN_KongTimeTab: EN_MAKE_ANIM_ARRAY TVAL
EN_KongGfxTab:  EN_MAKE_ANIM_ARRAY GVAL

; Kong animation graphics tables

EN_KongGRP_L:   ; lo-bytes
EN_KongHit0_L:  EN_MAKE_B48_LOBYTE_TABLE EN_KongHit0
EN_KongHit1_L:  EN_MAKE_B48_LOBYTE_TABLE EN_KongHit1
EN_KongHit2_L:  EN_MAKE_B48_LOBYTE_TABLE EN_KongHit2
EN_KongDrum0_L: EN_MAKE_B48_LOBYTE_TABLE EN_KongDrum0
EN_KongDrum1_L: EN_MAKE_B48_LOBYTE_TABLE EN_KongDrum1
EN_KongStand_L: EN_MAKE_B48_LOBYTE_TABLE EN_KongStand
EN_KongGrin_L:  EN_MAKE_B48_LOBYTE_TABLE EN_KongGrin

EN_KongGRP_H:   ; hi-bytes
EN_KongHit0_H:  EN_MAKE_B48_HIBYTE_TABLE EN_KongHit0
EN_KongHit1_H:  EN_MAKE_B48_HIBYTE_TABLE EN_KongHit1
EN_KongHit2_H:  EN_MAKE_B48_HIBYTE_TABLE EN_KongHit2
EN_KongDrum0_H: EN_MAKE_B48_HIBYTE_TABLE EN_KongDrum0
EN_KongDrum1_H: EN_MAKE_B48_HIBYTE_TABLE EN_KongDrum1
EN_KongStand_H: EN_MAKE_B48_HIBYTE_TABLE EN_KongStand
EN_KongGrin_H:  EN_MAKE_B48_HIBYTE_TABLE EN_KongGrin

; Kong star X-positions

EN_KongStarPos: BYTE    73+8,73

; 48bit bitmap GRP load instruction offsets

EN_B48Offsets:  BYTE    EN_RAM_B48BG
                BYTE    EN_RAM_B48Char0
                BYTE    EN_RAM_B48Char1
                BYTE    EN_RAM_B48Char2
                BYTE    EN_RAM_B48Char3
                BYTE    EN_RAM_B48Char4
                BYTE    EN_RAM_B48Char5

; --------------------------------------------------------------------
;       EN Graphics
; --------------------------------------------------------------------

;
; Girder
;

EN_Girder_GRP:  BYTE    %11111111
                BYTE    %11111111
                BYTE    %10011001
                BYTE    %10011001
                BYTE    %10011001
                BYTE    %10011001
                BYTE    %11111111
                BYTE    %11111111

EN_Girder_COL:  BYTE    EN_GIRDER_COL_1
                BYTE    EN_GIRDER_COL_0
                BYTE    EN_GIRDER_COL_0
                BYTE    EN_GIRDER_COL_0
                BYTE    EN_GIRDER_COL_0
                BYTE    EN_GIRDER_COL_0
                BYTE    EN_GIRDER_COL_1
                BYTE    EN_GIRDER_COL_0

;
; Mario GRP ( + heart )
;

EN_Mario_GRP:

                BYTE    %11101110 ; |XXX XXX |  15
                BYTE    %11001100 ; |XX  XX  |  14
                BYTE    %11101100 ; |XXX XX  |  13
                BYTE    %11011110 ; |XX XXXX |  12
                BYTE    %10001110 ; |X   XXX |  11
                BYTE    %00011110 ; |   XXXX |  10
                BYTE    %00111100 ; |  XXXX  |   9
                BYTE    %11101100 ; |XXX XX  |   8
                BYTE    %01111000 ; | XXXX   |   7
                BYTE    %00111100 ; |  XXXX  |   6
                BYTE    %11001110 ; |XX  XXX |   5
                BYTE    %01001011 ; | X  X XX|   4
                BYTE    %11101000 ; |XXX X   |   3
                BYTE    %01110000 ; | XXX    |   2
                BYTE    %01111110 ; | XXXXXX |   1
                BYTE    %00111000 ; |  XXX   |   0
                ;
                BYTE    %00000000 ; |        |  13
                BYTE    %00000000 ; |        |  12
                BYTE    %00000100 ; |     X  |  11
                BYTE    %00001000 ; |    X   |  10
                BYTE    %00011100 ; |   XXX  |   9
                BYTE    %00111110 ; |  XXXXX |   8
                BYTE    %00111110 ; |  XXXXX |   7
                BYTE    %01111111 ; | XXXXXXX|   6
                BYTE    %01111111 ; | XXXXXXX|   5
                BYTE    %01111111 ; | XXXXXXX|   4
                BYTE    %01111111 ; | XXXXXXX|   3
                BYTE    %01111111 ; | XXXXXXX|   2
                BYTE    %01110111 ; | XXX XXX|   1
                BYTE    %00110110 ; |  XX XX |   0

EN_Mario_COL0:

                BYTE    COL_82 ; |181AA7|  15
                BYTE    COL_82 ; |181AA7|  14
                BYTE    COL_42 ; |A71A1A|  13
                BYTE    COL_42 ; |A71A1A|  12
                BYTE    COL_42 ; |A71A1A|  11
                BYTE    COL_42 ; |A71A1A|  10
                BYTE    COL_42 ; |A71A1A|   9
                BYTE    COL_82 ; |181AA7|   8
                BYTE    COL_82 ; |181AA7|   7
                BYTE    COL_3E ; |FCBC74|   6
                BYTE    COL_82 ; |181AA7|   5
                BYTE    COL_3E ; |FCBC74|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_82 ; |181AA7|   2
                BYTE    COL_42 ; |A71A1A|   1
                BYTE    COL_42 ; |A71A1A|   0

EN_Mario_COL1:

                BYTE    COL_00 ; |000000|  15
                BYTE    COL_00 ; |000000|  14
                BYTE    COL_00 ; |000000|  13
                BYTE    COL_3E ; |FCBC74|  12
                BYTE    COL_3E ; |FCBC74|  11
                BYTE    COL_82 ; |181AA7|  10
                BYTE    COL_82 ; |181AA7|   9
                BYTE    COL_42 ; |A71A1A|   8
                BYTE    COL_00 ; |000000|   7
                BYTE    COL_00 ; |000000|   6
                BYTE    COL_3E ; |FCBC74|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_3E ; |FCBC74|   3
                BYTE    COL_3E ; |FCBC74|   2
                BYTE    COL_00 ; |000000|   1
                BYTE    COL_00 ; |000000|   0

EN_MarioPauline_HMP0_HMM1_NUSIZ:

                BYTE    $00 + %0000 ;  15
                BYTE    $00 + %0000 ;  14
                BYTE    $00 + %0000 ;  13
                BYTE    $00 + %1000 ;  12
                BYTE    $E0 + %0100 ;  11
                BYTE    $00 + %1000 ;  10
                BYTE    $30 + %0100 ;   9
                BYTE    $F0 + %0000 ;   8
                BYTE    $00 + %0000 ;   7
                BYTE    $00 + %0100 ;   6
                BYTE    $E0 + %1000 ;   5
                BYTE    $30 + %1100 ;   4
                BYTE    $10 + %1000 ;   3
                BYTE    $D0 + %0000 ;   2
                BYTE    $00 + %1000 ;   1
                BYTE    $00 + %0100 ;   0
                ;
                BYTE    $00 + %1000 ;  13
                BYTE    $70 + %0100 ;  12
                BYTE    $10 + %1000 ;  11
                BYTE    $00 + %0100 ;  10
                BYTE    $00 + %0000 ;   9
                BYTE    $00 + %0000 ;   8
                BYTE    $00 + %0000 ;   7
                BYTE    $00 + %0000 ;   6
                BYTE    $00 + %0000 ;   5
                BYTE    $00 + %0000 ;   4
                BYTE    $00 + %0000 ;   3
                BYTE    $00 + %0000 ;   2
                BYTE    $00 + %0000 ;   1
                BYTE    $00 + %0000 ;   0

EN_Pauline_GRP:

                BYTE    %01101100 ; | XX XX  |  15
                BYTE    %00101000 ; |  X X   |  14
                BYTE    %01111110 ; | XXXXXX |  13
                BYTE    %11000011 ; |XX    XX|  12
                BYTE    %01100110 ; | XX  XX |  11
                BYTE    %01111110 ; | XXXXXX |  10
                BYTE    %01111110 ; | XXXXXX |   9
                BYTE    %01111110 ; | XXXXXX |   8
                BYTE    %00111100 ; |  XXXX  |   7
                BYTE    %01000010 ; | X    X |   6
                BYTE    %11000011 ; |XX    XX|   5
                BYTE    %01111110 ; | XXXXXX |   4
                BYTE    %00011000 ; |   XX   |   3
                BYTE    %00111000 ; |  XXX   |   2
                BYTE    %00000110 ; |     XX |   1
                BYTE    %00001111 ; |    XXXX|   0
                ;
                BYTE    %00001110 ; |    XXX |  13
                BYTE    %00010010 ; |   X  X |  12
                BYTE    %01001001 ; | X  X  X|  11
                BYTE    %00011000 ; |   XX   |  10
                BYTE    %11111000 ; |XXXXX   |   9
                BYTE    %01110000 ; | XXX    |   8

EN_Pauline_COL0:

                BYTE    COL_00 ; |000000|  15
                BYTE    COL_00 ; |000000|  14
                BYTE    COL_00 ; |000000|  13
                BYTE    COL_0E ; |ECECEC|  12
                BYTE    COL_5A ; |D46CC3|  11
                BYTE    COL_00 ; |000000|  10
                BYTE    COL_00 ; |000000|   9
                BYTE    COL_00 ; |000000|   8
                BYTE    COL_00 ; |000000|   7
                BYTE    COL_5A ; |D46CC3|   6
                BYTE    COL_5A ; |D46CC3|   5
                BYTE    COL_00 ; |000000|   4
                BYTE    COL_5A ; |D46CC3|   3
                BYTE    COL_36 ; |C66C3A|   2
                BYTE    COL_5A ; |D46CC3|   1
                BYTE    COL_0E ; |ECECEC|   0
                ;
                BYTE    COL_0E ; |ECECEC|  13
                BYTE    COL_0E ; |ECECEC|  12
                BYTE    COL_0E ; |ECECEC|  11
                BYTE    COL_0E ; |ECECEC|  10
                BYTE    COL_00 ; |000000|   9
                BYTE    COL_00 ; |000000|   8
                BYTE    COL_00 ; |000000|   7
                BYTE    COL_00 ; |000000|   6
                BYTE    COL_00 ; |000000|   5
                BYTE    COL_00 ; |000000|   4
                BYTE    COL_00 ; |000000|   3
                BYTE    COL_00 ; |000000|   2
                BYTE    COL_00 ; |000000|   1
                BYTE    COL_00 ; |000000|   0

EN_Pauline_COL1:

                BYTE    COL_82 ; |181AA7|  15
                BYTE    COL_82 ; |181AA7|  14
                BYTE    COL_5A ; |D46CC3|  13
                BYTE    COL_5A ; |D46CC3|  12
                BYTE    COL_0E ; |ECECEC|  11
                BYTE    COL_5A ; |D46CC3|  10
                BYTE    COL_5A ; |D46CC3|   9
                BYTE    COL_5A ; |D46CC3|   8
                BYTE    COL_5A ; |D46CC3|   7
                BYTE    COL_0E ; |ECECEC|   6
                BYTE    COL_0E ; |ECECEC|   5
                BYTE    COL_5A ; |D46CC3|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_5A ; |D46CC3|   2
                BYTE    COL_36 ; |C66C3A|   1
                BYTE    COL_36 ; |C66C3A|   0
                ;
                BYTE    COL_36 ; |C66C3A|  13
                BYTE    COL_36 ; |C66C3A|  12
                BYTE    COL_36 ; |C66C3A|  11
                BYTE    COL_36 ; |C66C3A|  10
                BYTE    COL_36 ; |C66C3A|   9
                BYTE    COL_36 ; |C66C3A|   8
                BYTE    COL_00 ; |000000|   7
                BYTE    COL_00 ; |000000|   6
                BYTE    COL_00 ; |000000|   5
                BYTE    COL_00 ; |000000|   4
                BYTE    COL_00 ; |000000|   3
                BYTE    COL_00 ; |000000|   2
                BYTE    COL_00 ; |000000|   1
                BYTE    COL_00 ; |000000|   0

EN_Pauline_HMBL:

                BYTE    $00 ;  15
                BYTE    $00 ;  14
                BYTE    $00 ;  13
                BYTE    $10 ;  12
                BYTE    $00 ;  11
                BYTE    $00 ;  10
                BYTE    $00 ;   9
                BYTE    $00 ;   8
                BYTE    $00 ;   7
                BYTE    $F0 ;   6
                BYTE    $00 ;   5
                BYTE    $00 ;   4
                BYTE    $50 ;   3
                BYTE    $B0 ;   2
                BYTE    $00 ;   1
                BYTE    $F0 ;   0
                ;
                BYTE    $00 ;  13
                BYTE    $F0 ;  12
                BYTE    $10 ;  11
                BYTE    $00 ;  10
                BYTE    $00 ;   9
                BYTE    $00 ;   8
                BYTE    $00 ;   7
                BYTE    $00 ;   6
                BYTE    $00 ;   5
                BYTE    $00 ;   4
                BYTE    $00 ;   3
                BYTE    $00 ;   2
                BYTE    $00 ;   1
                BYTE    $00 ;   0

EN_KongStar_GRP:

                BYTE    %10110110
                BYTE    %11010101
                BYTE    %11110111
                BYTE    %10000000
                BYTE    %11110111
                BYTE    %11010101
                BYTE    %10110110
                BYTE    %11111111

;
; Kong
;
                ALIGN   $0020

EN_KongHit0_GRP0_0:

                BYTE    %00001111 ; |    XXXX|  31
                BYTE    %00000111 ; |     XXX|  30
                BYTE    %00000010 ; |      X |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000001 ; |       X|  26
                BYTE    %00000001 ; |       X|  25
                BYTE    %00000001 ; |       X|  24
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
                BYTE    %00000010 ; |      X |  11
                BYTE    %00000001 ; |       X|  10
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

EN_KongHit0_GRP1_0:

                BYTE    %11111111 ; |XXXXXXXX|  31
                BYTE    %10111101 ; |X XXXX X|  30
                BYTE    %11000110 ; |XX   XX |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %00000000 ; |        |  25
                BYTE    %00000000 ; |        |  24
                BYTE    %10000000 ; |X       |  23
                BYTE    %01000000 ; | X      |  22
                BYTE    %00100001 ; |  X    X|  21
                BYTE    %00010010 ; |   X  X |  20
                BYTE    %00000000 ; |        |  19
                BYTE    %00000000 ; |        |  18
                BYTE    %00000000 ; |        |  17
                BYTE    %00000000 ; |        |  16
                BYTE    %00000111 ; |     XXX|  15
                BYTE    %00001110 ; |    XXX |  14
                BYTE    %00011000 ; |   XX   |  13
                BYTE    %00100000 ; |  X     |  12
                BYTE    %00000000 ; |        |  11
                BYTE    %10000001 ; |X      X|  10
                BYTE    %01000001 ; | X     X|   9
                BYTE    %00110000 ; |  XX    |   8
                BYTE    %00001000 ; |    X   |   7
                BYTE    %00000010 ; |      X |   6
                BYTE    %00000010 ; |      X |   5
                BYTE    %00000001 ; |       X|   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

EN_KongHit0_GRP2_0:

                BYTE    %00000000 ; |        |  31
                BYTE    %10000000 ; |X       |  30
                BYTE    %10000000 ; |X       |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %00100101 ; |  X  X X|  25
                BYTE    %00011111 ; |   XXXXX|  24
                BYTE    %00111111 ; |  XXXXXX|  23
                BYTE    %00111111 ; |  XXXXXX|  22
                BYTE    %01111111 ; | XXXXXXX|  21
                BYTE    %11111111 ; |XXXXXXXX|  20
                BYTE    %00101111 ; |  X XXXX|  19
                BYTE    %00011111 ; |   XXXXX|  18
                BYTE    %00101111 ; |  X XXXX|  17
                BYTE    %01111110 ; | XXXXXX |  16
                BYTE    %01111111 ; | XXXXXXX|  15
                BYTE    %01110000 ; | XXX    |  14
                BYTE    %11001000 ; |XX  X   |  13
                BYTE    %11111111 ; |XXXXXXXX|  12
                BYTE    %11011100 ; |XX XXX  |  11
                BYTE    %11001000 ; |XX  X   |  10
                BYTE    %11100000 ; |XXX     |   9
                BYTE    %01111110 ; | XXXXXX |   8
                BYTE    %11110011 ; |XXXX  XX|   7
                BYTE    %11100001 ; |XXX    X|   6
                BYTE    %11100001 ; |XXX    X|   5
                BYTE    %11110011 ; |XXXX  XX|   4
                BYTE    %11111111 ; |XXXXXXXX|   3
                BYTE    %00011110 ; |   XXXX |   2
                BYTE    %00000100 ; |     X  |   1
                BYTE    %00000000 ; |        |   0

EN_KongHit0_GRP3_0:

                BYTE    %00000000 ; |        |  31
                BYTE    %00000001 ; |       X|  30
                BYTE    %00000001 ; |       X|  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %00100100 ; |  X  X  |  25
                BYTE    %10111000 ; |X XXX   |  24
                BYTE    %11111100 ; |XXXXXX  |  23
                BYTE    %11111100 ; |XXXXXX  |  22
                BYTE    %11111110 ; |XXXXXXX |  21
                BYTE    %11111111 ; |XXXXXXXX|  20
                BYTE    %11110100 ; |XXXX X  |  19
                BYTE    %11111000 ; |XXXXX   |  18
                BYTE    %11110100 ; |XXXX X  |  17
                BYTE    %01111110 ; | XXXXXX |  16
                BYTE    %11111110 ; |XXXXXXX |  15
                BYTE    %00001110 ; |    XXX |  14
                BYTE    %10001111 ; |X   XXXX|  13
                BYTE    %11111011 ; |XXXXX XX|  12
                BYTE    %11111111 ; |XXXXXXXX|  11
                BYTE    %10001001 ; |X   X  X|  10
                BYTE    %00000011 ; |      XX|   9
                BYTE    %01111110 ; | XXXXXX |   8
                BYTE    %11000011 ; |XX    XX|   7
                BYTE    %10000001 ; |X      X|   6
                BYTE    %10000001 ; |X      X|   5
                BYTE    %10000001 ; |X      X|   4
                BYTE    %11000011 ; |XX    XX|   3
                BYTE    %11111100 ; |XXXXXX  |   2
                BYTE    %01100000 ; | XX     |   1
                BYTE    %00000000 ; |        |   0

EN_KongHit0_GRP4_0:

                BYTE    %11111111 ; |XXXXXXXX|  31
                BYTE    %10111101 ; |X XXXX X|  30
                BYTE    %01100011 ; | XX   XX|  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %00000000 ; |        |  25
                BYTE    %00000000 ; |        |  24
                BYTE    %00000001 ; |       X|  23
                BYTE    %00000010 ; |      X |  22
                BYTE    %10000100 ; |X    X  |  21
                BYTE    %01001000 ; | X  X   |  20
                BYTE    %00000000 ; |        |  19
                BYTE    %00000000 ; |        |  18
                BYTE    %00000000 ; |        |  17
                BYTE    %00000000 ; |        |  16
                BYTE    %11100000 ; |XXX     |  15
                BYTE    %01110000 ; | XXX    |  14
                BYTE    %00011000 ; |   XX   |  13
                BYTE    %00000100 ; |     X  |  12
                BYTE    %00000000 ; |        |  11
                BYTE    %10000001 ; |X      X|  10
                BYTE    %10000010 ; |X     X |   9
                BYTE    %00001100 ; |    XX  |   8
                BYTE    %00010000 ; |   X    |   7
                BYTE    %01000000 ; | X      |   6
                BYTE    %01000000 ; | X      |   5
                BYTE    %00000000 ; |        |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

EN_KongHit0_GRP5_0:

                BYTE    %11110000 ; |XXXX    |  31
                BYTE    %11100000 ; |XXX     |  30
                BYTE    %01000000 ; | X      |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %10000000 ; |X       |  26
                BYTE    %10000000 ; |X       |  25
                BYTE    %10000000 ; |X       |  24
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
                BYTE    %01000000 ; | X      |  11
                BYTE    %10000000 ; |X       |  10
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

EN_KongHit0_GRP0_1:

                BYTE    %00001011 ; |    X XX|  31
                BYTE    %00000101 ; |     X X|  30
                BYTE    %00000011 ; |      XX|  29
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
                BYTE    %00000000 ; |        |  18
                BYTE    %00000001 ; |       X|  17
                BYTE    %00000011 ; |      XX|  16
                BYTE    %00000111 ; |     XXX|  15
                BYTE    %00000111 ; |     XXX|  14
                BYTE    %00000111 ; |     XXX|  13
                BYTE    %00000111 ; |     XXX|  12
                BYTE    %00000001 ; |       X|  11
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

EN_KongHit0_GRP1_1:

                BYTE    %11111111 ; |XXXXXXXX|  31
                BYTE    %11111111 ; |XXXXXXXX|  30
                BYTE    %11111111 ; |XXXXXXXX|  29
                BYTE    %11111111 ; |XXXXXXXX|  28
                BYTE    %11111111 ; |XXXXXXXX|  27
                BYTE    %11111111 ; |XXXXXXXX|  26
                BYTE    %11111111 ; |XXXXXXXX|  25
                BYTE    %11111111 ; |XXXXXXXX|  24
                BYTE    %01111111 ; | XXXXXXX|  23
                BYTE    %00111111 ; |  XXXXXX|  22
                BYTE    %00011110 ; |   XXXX |  21
                BYTE    %00001101 ; |    XX X|  20
                BYTE    %00011111 ; |   XXXXX|  19
                BYTE    %11111111 ; |XXXXXXXX|  18
                BYTE    %11111111 ; |XXXXXXXX|  17
                BYTE    %11111111 ; |XXXXXXXX|  16
                BYTE    %11111000 ; |XXXXX   |  15
                BYTE    %11110001 ; |XXXX   X|  14
                BYTE    %11100111 ; |XXX  XXX|  13
                BYTE    %11011111 ; |XX XXXXX|  12
                BYTE    %11111111 ; |XXXXXXXX|  11
                BYTE    %01111111 ; | XXXXXXX|  10
                BYTE    %00111111 ; |  XXXXXX|   9
                BYTE    %00001111 ; |    XXXX|   8
                BYTE    %00000111 ; |     XXX|   7
                BYTE    %00000011 ; |      XX|   6
                BYTE    %00000011 ; |      XX|   5
                BYTE    %00000001 ; |       X|   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

EN_KongHit0_GRP2_1:

                BYTE    %00000000 ; |        |  31
                BYTE    %10000000 ; |X       |  30
                BYTE    %10000000 ; |X       |  29
                BYTE    %11000000 ; |XX      |  28
                BYTE    %11100000 ; |XXX     |  27
                BYTE    %11110000 ; |XXXX    |  26
                BYTE    %11011010 ; |XX XX X |  25
                BYTE    %11101010 ; |XXX X X |  24
                BYTE    %11010101 ; |XX X X X|  23
                BYTE    %11011111 ; |XX XXXXX|  22
                BYTE    %10000011 ; |X     XX|  21
                BYTE    %01111101 ; | XXXXX X|  20
                BYTE    %11111110 ; |XXXXXXX |  19
                BYTE    %11111110 ; |XXXXXXX |  18
                BYTE    %11111110 ; |XXXXXXX |  17
                BYTE    %11111101 ; |XXXXXX X|  16
                BYTE    %11111111 ; |XXXXXXXX|  15
                BYTE    %11110000 ; |XXXX    |  14
                BYTE    %11001000 ; |XX  X   |  13
                BYTE    %11111111 ; |XXXXXXXX|  12
                BYTE    %11011100 ; |XX XXX  |  11
                BYTE    %11001000 ; |XX  X   |  10
                BYTE    %11100000 ; |XXX     |   9
                BYTE    %11111111 ; |XXXXXXXX|   8
                BYTE    %11110011 ; |XXXX  XX|   7
                BYTE    %11100101 ; |XXX  X X|   6
                BYTE    %11100001 ; |XXX    X|   5
                BYTE    %11110011 ; |XXXX  XX|   4
                BYTE    %11111111 ; |XXXXXXXX|   3
                BYTE    %00111111 ; |  XXXXXX|   2
                BYTE    %00111111 ; |  XXXXXX|   1
                BYTE    %00011111 ; |   XXXXX|   0

EN_KongHit0_GRP3_1:

                BYTE    %00000000 ; |        |  31
                BYTE    %00000001 ; |       X|  30
                BYTE    %00000001 ; |       X|  29
                BYTE    %00000011 ; |      XX|  28
                BYTE    %00000111 ; |     XXX|  27
                BYTE    %00001111 ; |    XXXX|  26
                BYTE    %11011011 ; |XX XX XX|  25
                BYTE    %11010111 ; |XX X XXX|  24
                BYTE    %10101011 ; |X X X XX|  23
                BYTE    %11111011 ; |XXXXX XX|  22
                BYTE    %11000001 ; |XX     X|  21
                BYTE    %10111110 ; |X XXXXX |  20
                BYTE    %01111111 ; | XXXXXXX|  19
                BYTE    %01111111 ; | XXXXXXX|  18
                BYTE    %01111111 ; | XXXXXXX|  17
                BYTE    %10111111 ; |X XXXXXX|  16
                BYTE    %11111111 ; |XXXXXXXX|  15
                BYTE    %00001111 ; |    XXXX|  14
                BYTE    %10001111 ; |X   XXXX|  13
                BYTE    %11111011 ; |XXXXX XX|  12
                BYTE    %11111111 ; |XXXXXXXX|  11
                BYTE    %10001001 ; |X   X  X|  10
                BYTE    %00000011 ; |      XX|   9
                BYTE    %11111111 ; |XXXXXXXX|   8
                BYTE    %11000011 ; |XX    XX|   7
                BYTE    %10100001 ; |X X    X|   6
                BYTE    %10000001 ; |X      X|   5
                BYTE    %10000001 ; |X      X|   4
                BYTE    %11000011 ; |XX    XX|   3
                BYTE    %11111110 ; |XXXXXXX |   2
                BYTE    %11111110 ; |XXXXXXX |   1
                BYTE    %11111100 ; |XXXXXX  |   0

EN_KongHit0_GRP4_1:

                BYTE    %11111111 ; |XXXXXXXX|  31
                BYTE    %11111111 ; |XXXXXXXX|  30
                BYTE    %11111111 ; |XXXXXXXX|  29
                BYTE    %11111111 ; |XXXXXXXX|  28
                BYTE    %11111111 ; |XXXXXXXX|  27
                BYTE    %11111111 ; |XXXXXXXX|  26
                BYTE    %11111111 ; |XXXXXXXX|  25
                BYTE    %11111111 ; |XXXXXXXX|  24
                BYTE    %11111110 ; |XXXXXXX |  23
                BYTE    %11111100 ; |XXXXXX  |  22
                BYTE    %01111000 ; | XXXX   |  21
                BYTE    %10110000 ; |X XX    |  20
                BYTE    %11111000 ; |XXXXX   |  19
                BYTE    %11111111 ; |XXXXXXXX|  18
                BYTE    %11111111 ; |XXXXXXXX|  17
                BYTE    %11111111 ; |XXXXXXXX|  16
                BYTE    %00011111 ; |   XXXXX|  15
                BYTE    %10001111 ; |X   XXXX|  14
                BYTE    %11100111 ; |XXX  XXX|  13
                BYTE    %11111011 ; |XXXXX XX|  12
                BYTE    %11111111 ; |XXXXXXXX|  11
                BYTE    %11111110 ; |XXXXXXX |  10
                BYTE    %11111100 ; |XXXXXX  |   9
                BYTE    %11110000 ; |XXXX    |   8
                BYTE    %11100000 ; |XXX     |   7
                BYTE    %11000000 ; |XX      |   6
                BYTE    %11000000 ; |XX      |   5
                BYTE    %10000000 ; |X       |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

EN_KongHit0_GRP5_1:

                BYTE    %11010000 ; |XX X    |  31
                BYTE    %10100000 ; |X X     |  30
                BYTE    %11000000 ; |XX      |  29
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
                BYTE    %00000000 ; |        |  18
                BYTE    %10000000 ; |X       |  17
                BYTE    %11000000 ; |XX      |  16
                BYTE    %11100000 ; |XXX     |  15
                BYTE    %11100000 ; |XXX     |  14
                BYTE    %11100000 ; |XXX     |  13
                BYTE    %11100000 ; |XXX     |  12
                BYTE    %10000000 ; |X       |  11
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

EN_KongHit1_GRP0_0 = EN_KongHit0_GRP0_0

EN_KongHit1_GRP1_0 = EN_KongHit0_GRP1_0

EN_KongHit1_GRP2_0 = EN_KongHit0_GRP2_0

EN_KongHit1_GRP3_0 = EN_KongHit0_GRP3_0

EN_KongHit1_GRP4_0 = EN_KongHit0_GRP4_0

EN_KongHit1_GRP5_0 = EN_KongHit0_GRP5_0

EN_KongHit1_GRP0_1 = EN_KongHit0_GRP0_1

EN_KongHit1_GRP1_1 = EN_KongHit0_GRP1_1

EN_KongHit1_GRP2_1:

                BYTE    %00000000 ; |        |  31
                BYTE    %10000000 ; |X       |  30
                BYTE    %10000000 ; |X       |  29
                BYTE    %11000000 ; |XX      |  28
                BYTE    %11100000 ; |XXX     |  27
                BYTE    %11110000 ; |XXXX    |  26
                BYTE    %11011010 ; |XX XX X |  25
                BYTE    %11101010 ; |XXX X X |  24
                BYTE    %11010101 ; |XX X X X|  23
                BYTE    %11011111 ; |XX XXXXX|  22
                BYTE    %10000011 ; |X     XX|  21
                BYTE    %01111101 ; | XXXXX X|  20
                BYTE    %11111110 ; |XXXXXXX |  19
                BYTE    %11111110 ; |XXXXXXX |  18
                BYTE    %11111110 ; |XXXXXXX |  17
                BYTE    %11111101 ; |XXXXXX X|  16
                BYTE    %11111111 ; |XXXXXXXX|  15
                BYTE    %11110000 ; |XXXX    |  14
                BYTE    %11001000 ; |XX  X   |  13
                BYTE    %11111111 ; |XXXXXXXX|  12
                BYTE    %11011100 ; |XX XXX  |  11
                BYTE    %11001000 ; |XX  X   |  10
                BYTE    %11100000 ; |XXX     |   9
                BYTE    %11111111 ; |XXXXXXXX|   8
                BYTE    %11110011 ; |XXXX  XX|   7
                BYTE    %11100001 ; |XXX    X|   6
                BYTE    %11110001 ; |XXXX   X|   5
                BYTE    %11110011 ; |XXXX  XX|   4
                BYTE    %11111111 ; |XXXXXXXX|   3
                BYTE    %00111111 ; |  XXXXXX|   2
                BYTE    %00111111 ; |  XXXXXX|   1
                BYTE    %00011111 ; |   XXXXX|   0

EN_KongHit1_GRP3_1:

                BYTE    %00000000 ; |        |  31
                BYTE    %00000001 ; |       X|  30
                BYTE    %00000001 ; |       X|  29
                BYTE    %00000011 ; |      XX|  28
                BYTE    %00000111 ; |     XXX|  27
                BYTE    %00001111 ; |    XXXX|  26
                BYTE    %11011011 ; |XX XX XX|  25
                BYTE    %11010111 ; |XX X XXX|  24
                BYTE    %10101011 ; |X X X XX|  23
                BYTE    %11111011 ; |XXXXX XX|  22
                BYTE    %11000001 ; |XX     X|  21
                BYTE    %10111110 ; |X XXXXX |  20
                BYTE    %01111111 ; | XXXXXXX|  19
                BYTE    %01111111 ; | XXXXXXX|  18
                BYTE    %01111111 ; | XXXXXXX|  17
                BYTE    %10111111 ; |X XXXXXX|  16
                BYTE    %11111111 ; |XXXXXXXX|  15
                BYTE    %00001111 ; |    XXXX|  14
                BYTE    %10001111 ; |X   XXXX|  13
                BYTE    %11111011 ; |XXXXX XX|  12
                BYTE    %11111111 ; |XXXXXXXX|  11
                BYTE    %10001001 ; |X   X  X|  10
                BYTE    %00000011 ; |      XX|   9
                BYTE    %11111111 ; |XXXXXXXX|   8
                BYTE    %11000011 ; |XX    XX|   7
                BYTE    %10000001 ; |X      X|   6
                BYTE    %10000001 ; |X      X|   5
                BYTE    %10100001 ; |X X    X|   4
                BYTE    %11000011 ; |XX    XX|   3
                BYTE    %11111110 ; |XXXXXXX |   2
                BYTE    %11111110 ; |XXXXXXX |   1
                BYTE    %11111100 ; |XXXXXX  |   0

EN_KongHit1_GRP4_1 = EN_KongHit0_GRP4_1

EN_KongHit1_GRP5_1 = EN_KongHit0_GRP5_1

EN_KongHit2_GRP0_0 = EN_KongHit0_GRP0_0

EN_KongHit2_GRP1_0 = EN_KongHit0_GRP1_0

EN_KongHit2_GRP2_0 = EN_KongHit0_GRP2_0

EN_KongHit2_GRP3_0 = EN_KongHit0_GRP3_0

EN_KongHit2_GRP4_0 = EN_KongHit0_GRP4_0

EN_KongHit2_GRP5_0 = EN_KongHit0_GRP5_0

EN_KongHit2_GRP0_1 = EN_KongHit0_GRP0_1

EN_KongHit2_GRP1_1 = EN_KongHit0_GRP1_1

EN_KongHit2_GRP2_1:

                BYTE    %00000000 ; |        |  31
                BYTE    %10000000 ; |X       |  30
                BYTE    %10000000 ; |X       |  29
                BYTE    %11000000 ; |XX      |  28
                BYTE    %11100000 ; |XXX     |  27
                BYTE    %11110000 ; |XXXX    |  26
                BYTE    %11011010 ; |XX XX X |  25
                BYTE    %11101010 ; |XXX X X |  24
                BYTE    %11010101 ; |XX X X X|  23
                BYTE    %11011111 ; |XX XXXXX|  22
                BYTE    %10000011 ; |X     XX|  21
                BYTE    %01111101 ; | XXXXX X|  20
                BYTE    %11111110 ; |XXXXXXX |  19
                BYTE    %11111110 ; |XXXXXXX |  18
                BYTE    %11111110 ; |XXXXXXX |  17
                BYTE    %11111101 ; |XXXXXX X|  16
                BYTE    %11111111 ; |XXXXXXXX|  15
                BYTE    %11110000 ; |XXXX    |  14
                BYTE    %11001000 ; |XX  X   |  13
                BYTE    %11111111 ; |XXXXXXXX|  12
                BYTE    %11011100 ; |XX XXX  |  11
                BYTE    %11001000 ; |XX  X   |  10
                BYTE    %11100000 ; |XXX     |   9
                BYTE    %11111111 ; |XXXXXXXX|   8
                BYTE    %11110011 ; |XXXX  XX|   7
                BYTE    %11100001 ; |XXX    X|   6
                BYTE    %11100001 ; |XXX    X|   5
                BYTE    %11110111 ; |XXXX XXX|   4
                BYTE    %11111111 ; |XXXXXXXX|   3
                BYTE    %00111111 ; |  XXXXXX|   2
                BYTE    %00111111 ; |  XXXXXX|   1
                BYTE    %00011111 ; |   XXXXX|   0

EN_KongHit2_GRP3_1:

                BYTE    %00000000 ; |        |  31
                BYTE    %00000001 ; |       X|  30
                BYTE    %00000001 ; |       X|  29
                BYTE    %00000011 ; |      XX|  28
                BYTE    %00000111 ; |     XXX|  27
                BYTE    %00001111 ; |    XXXX|  26
                BYTE    %11011011 ; |XX XX XX|  25
                BYTE    %11010111 ; |XX X XXX|  24
                BYTE    %10101011 ; |X X X XX|  23
                BYTE    %11111011 ; |XXXXX XX|  22
                BYTE    %11000001 ; |XX     X|  21
                BYTE    %10111110 ; |X XXXXX |  20
                BYTE    %01111111 ; | XXXXXXX|  19
                BYTE    %01111111 ; | XXXXXXX|  18
                BYTE    %01111111 ; | XXXXXXX|  17
                BYTE    %10111111 ; |X XXXXXX|  16
                BYTE    %11111111 ; |XXXXXXXX|  15
                BYTE    %00001111 ; |    XXXX|  14
                BYTE    %10001111 ; |X   XXXX|  13
                BYTE    %11111011 ; |XXXXX XX|  12
                BYTE    %11111111 ; |XXXXXXXX|  11
                BYTE    %10001001 ; |X   X  X|  10
                BYTE    %00000011 ; |      XX|   9
                BYTE    %11111111 ; |XXXXXXXX|   8
                BYTE    %11000011 ; |XX    XX|   7
                BYTE    %10000001 ; |X      X|   6
                BYTE    %10000101 ; |X    X X|   5
                BYTE    %10000001 ; |X      X|   4
                BYTE    %11000011 ; |XX    XX|   3
                BYTE    %11111110 ; |XXXXXXX |   2
                BYTE    %11111110 ; |XXXXXXX |   1
                BYTE    %11111100 ; |XXXXXX  |   0

EN_KongHit2_GRP4_1 = EN_KongHit0_GRP4_1

EN_KongHit2_GRP5_1 = EN_KongHit0_GRP5_1

;
; Kong drumming
;

EN_KongDrum0_GRP0_0 = EN_KongDrum1_GRP5_0

EN_KongDrum0_GRP1_0 = EN_KongDrum1_GRP4_0

EN_KongDrum0_GRP2_0:

                BYTE    %00000000 ; |        |  31
                BYTE    %00000000 ; |        |  30
                BYTE    %00000000 ; |        |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %10100100 ; |X X  X  |  25
                BYTE    %11111000 ; |XXXXX   |  24
                BYTE    %11111100 ; |XXXXXX  |  23
                BYTE    %11111100 ; |XXXXXX  |  22
                BYTE    %11111110 ; |XXXXXXX |  21
                BYTE    %11111111 ; |XXXXXXXX|  20
                BYTE    %11110100 ; |XXXX X  |  19
                BYTE    %11111000 ; |XXXXX   |  18
                BYTE    %11110100 ; |XXXX X  |  17
                BYTE    %01111110 ; | XXXXXX |  16
                BYTE    %00001110 ; |    XXX |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %11110000 ; |XXXX    |  13
                BYTE    %00011110 ; |   XXXX |  12
                BYTE    %00010011 ; |   X  XX|  11
                BYTE    %10111011 ; |X XXX XX|  10
                BYTE    %00010001 ; |   X   X|   9
                BYTE    %11110011 ; |XXXX  XX|   8
                BYTE    %01111111 ; | XXXXXXX|   7
                BYTE    %11110001 ; |XXXX   X|   6
                BYTE    %00110001 ; |  XX   X|   5
                BYTE    %00111001 ; |  XXX  X|   4
                BYTE    %11110000 ; |XXXX    |   3
                BYTE    %01100000 ; | XX     |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

EN_KongDrum0_GRP3_0:

                BYTE    %00000000 ; |        |  31
                BYTE    %10000000 ; |X       |  30
                BYTE    %10000000 ; |X       |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %00100100 ; |  X  X  |  25
                BYTE    %00011101 ; |   XXX X|  24
                BYTE    %00111111 ; |  XXXXXX|  23
                BYTE    %00111111 ; |  XXXXXX|  22
                BYTE    %01111111 ; | XXXXXXX|  21
                BYTE    %11111111 ; |XXXXXXXX|  20
                BYTE    %11101111 ; |XXX XXXX|  19
                BYTE    %11111111 ; |XXXXXXXX|  18
                BYTE    %01111111 ; | XXXXXXX|  17
                BYTE    %01111110 ; | XXXXXX |  16
                BYTE    %00000000 ; |        |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %00011111 ; |   XXXXX|  13
                BYTE    %01110001 ; | XXX   X|  12
                BYTE    %10010001 ; |X  X   X|  11
                BYTE    %10111011 ; |X XXX XX|  10
                BYTE    %00010001 ; |   X   X|   9
                BYTE    %10011111 ; |X  XXXXX|   8
                BYTE    %11111100 ; |XXXXXX  |   7
                BYTE    %10011111 ; |X  XXXXX|   6
                BYTE    %10011001 ; |X  XX  X|   5
                BYTE    %10111001 ; |X XXX  X|   4
                BYTE    %00011110 ; |   XXXX |   3
                BYTE    %00001100 ; |    XX  |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

EN_KongDrum0_GRP4_0 = EN_KongDrum1_GRP1_0

EN_KongDrum0_GRP5_0 = EN_KongDrum1_GRP0_0

EN_KongDrum0_GRP0_1 = EN_KongDrum1_GRP5_1

EN_KongDrum0_GRP1_1 = EN_KongDrum1_GRP4_1

EN_KongDrum0_GRP2_1:

                BYTE    %00000000 ; |        |  31
                BYTE    %00000000 ; |        |  30
                BYTE    %00000000 ; |        |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000011 ; |      XX|  26
                BYTE    %01011011 ; | X XX XX|  25
                BYTE    %01010111 ; | X X XXX|  24
                BYTE    %10101011 ; |X X X XX|  23
                BYTE    %11111011 ; |XXXXX XX|  22
                BYTE    %11000001 ; |XX     X|  21
                BYTE    %10111110 ; |X XXXXX |  20
                BYTE    %01111111 ; | XXXXXXX|  19
                BYTE    %01111111 ; | XXXXXXX|  18
                BYTE    %01111111 ; | XXXXXXX|  17
                BYTE    %10111111 ; |X XXXXXX|  16
                BYTE    %11111111 ; |XXXXXXXX|  15
                BYTE    %11111111 ; |XXXXXXXX|  14
                BYTE    %11111111 ; |XXXXXXXX|  13
                BYTE    %00011111 ; |   XXXXX|  12
                BYTE    %00010011 ; |   X  XX|  11
                BYTE    %10111011 ; |X XXX XX|  10
                BYTE    %00010001 ; |   X   X|   9
                BYTE    %11110011 ; |XXXX  XX|   8
                BYTE    %11111111 ; |XXXXXXXX|   7
                BYTE    %11111111 ; |XXXXXXXX|   6
                BYTE    %10111111 ; |X XXXXXX|   5
                BYTE    %00111111 ; |  XXXXXX|   4
                BYTE    %11111100 ; |XXXXXX  |   3
                BYTE    %11111000 ; |XXXXX   |   2
                BYTE    %11110000 ; |XXXX    |   1
                BYTE    %11100000 ; |XXX     |   0

EN_KongDrum0_GRP3_1:

                BYTE    %00000000 ; |        |  31
                BYTE    %10000000 ; |X       |  30
                BYTE    %10000000 ; |X       |  29
                BYTE    %11000000 ; |XX      |  28
                BYTE    %11100000 ; |XXX     |  27
                BYTE    %11110000 ; |XXXX    |  26
                BYTE    %11011011 ; |XX XX XX|  25
                BYTE    %11101011 ; |XXX X XX|  24
                BYTE    %11010101 ; |XX X X X|  23
                BYTE    %11011111 ; |XX XXXXX|  22
                BYTE    %10000011 ; |X     XX|  21
                BYTE    %01111101 ; | XXXXX X|  20
                BYTE    %01111110 ; | XXXXXX |  19
                BYTE    %01111110 ; | XXXXXX |  18
                BYTE    %10111110 ; |X XXXXX |  17
                BYTE    %11010101 ; |XX X X X|  16
                BYTE    %11111111 ; |XXXXXXXX|  15
                BYTE    %11111111 ; |XXXXXXXX|  14
                BYTE    %11111111 ; |XXXXXXXX|  13
                BYTE    %11110001 ; |XXXX   X|  12
                BYTE    %10010001 ; |X  X   X|  11
                BYTE    %10111011 ; |X XXX XX|  10
                BYTE    %00010001 ; |   X   X|   9
                BYTE    %10011111 ; |X  XXXXX|   8
                BYTE    %11111111 ; |XXXXXXXX|   7
                BYTE    %11111111 ; |XXXXXXXX|   6
                BYTE    %11111011 ; |XXXXX XX|   5
                BYTE    %11111001 ; |XXXXX  X|   4
                BYTE    %01111111 ; | XXXXXXX|   3
                BYTE    %00111111 ; |  XXXXXX|   2
                BYTE    %00011111 ; |   XXXXX|   1
                BYTE    %00001111 ; |    XXXX|   0

EN_KongDrum0_GRP4_1 = EN_KongDrum1_GRP1_1

EN_KongDrum0_GRP5_1 = EN_KongDrum1_GRP0_1

EN_KongDrum1_GRP0_0:

                BYTE    %00001111 ; |    XXXX|  31
                BYTE    %00000111 ; |     XXX|  30
                BYTE    %00000010 ; |      X |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000001 ; |       X|  26
                BYTE    %00000001 ; |       X|  25
                BYTE    %00000001 ; |       X|  24
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
                BYTE    %00001000 ; |    X   |   9
                BYTE    %00000100 ; |     X  |   8
                BYTE    %00000110 ; |     XX |   7
                BYTE    %00000110 ; |     XX |   6
                BYTE    %00011111 ; |   XXXXX|   5
                BYTE    %00111111 ; |  XXXXXX|   4
                BYTE    %00111111 ; |  XXXXXX|   3
                BYTE    %00111111 ; |  XXXXXX|   2
                BYTE    %00001111 ; |    XXXX|   1
                BYTE    %00000011 ; |      XX|   0

EN_KongDrum1_GRP1_0:

                BYTE    %11111111 ; |XXXXXXXX|  31
                BYTE    %10111101 ; |X XXXX X|  30
                BYTE    %11000110 ; |XX   XX |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %00000000 ; |        |  25
                BYTE    %00000000 ; |        |  24
                BYTE    %10000000 ; |X       |  23
                BYTE    %01000000 ; | X      |  22
                BYTE    %00100001 ; |  X    X|  21
                BYTE    %00010010 ; |   X  X |  20
                BYTE    %00000001 ; |       X|  19
                BYTE    %00000001 ; |       X|  18
                BYTE    %00000010 ; |      X |  17
                BYTE    %00000001 ; |       X|  16
                BYTE    %00000000 ; |        |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %00000000 ; |        |  13
                BYTE    %00000000 ; |        |  12
                BYTE    %00000000 ; |        |  11
                BYTE    %00000001 ; |       X|  10
                BYTE    %00000001 ; |       X|   9
                BYTE    %00000001 ; |       X|   8
                BYTE    %00000000 ; |        |   7
                BYTE    %00000011 ; |      XX|   6
                BYTE    %11000011 ; |XX    XX|   5
                BYTE    %11100001 ; |XXX    X|   4
                BYTE    %11100000 ; |XXX     |   3
                BYTE    %11110000 ; |XXXX    |   2
                BYTE    %11100000 ; |XXX     |   1
                BYTE    %11000000 ; |XX      |   0

EN_KongDrum1_GRP2_0:

                BYTE    %00000000 ; |        |  31
                BYTE    %10000000 ; |X       |  30
                BYTE    %10000000 ; |X       |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %00100101 ; |  X  X X|  25
                BYTE    %00011111 ; |   XXXXX|  24
                BYTE    %00111111 ; |  XXXXXX|  23
                BYTE    %00111111 ; |  XXXXXX|  22
                BYTE    %01111111 ; | XXXXXXX|  21
                BYTE    %11111111 ; |XXXXXXXX|  20
                BYTE    %11101111 ; |XXX XXXX|  19
                BYTE    %11111111 ; |XXXXXXXX|  18
                BYTE    %01111111 ; | XXXXXXX|  17
                BYTE    %01111110 ; | XXXXXX |  16
                BYTE    %00000000 ; |        |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %00001111 ; |    XXXX|  13
                BYTE    %01111000 ; | XXXX   |  12
                BYTE    %11001000 ; |XX  X   |  11
                BYTE    %11011101 ; |XX XXX X|  10
                BYTE    %10001000 ; |X   X   |   9
                BYTE    %11001111 ; |XX  XXXX|   8
                BYTE    %11111110 ; |XXXXXXX |   7
                BYTE    %10001111 ; |X   XXXX|   6
                BYTE    %10001100 ; |X   XX  |   5
                BYTE    %10011100 ; |X  XXX  |   4
                BYTE    %00001111 ; |    XXXX|   3
                BYTE    %00000110 ; |     XX |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

EN_KongDrum1_GRP3_0:

                BYTE    %00000000 ; |        |  31
                BYTE    %00000000 ; |        |  30
                BYTE    %00000000 ; |        |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %00100100 ; |  X  X  |  25
                BYTE    %10111000 ; |X XXX   |  24
                BYTE    %11111100 ; |XXXXXX  |  23
                BYTE    %11111100 ; |XXXXXX  |  22
                BYTE    %11111110 ; |XXXXXXX |  21
                BYTE    %11111111 ; |XXXXXXXX|  20
                BYTE    %11110100 ; |XXXX X  |  19
                BYTE    %11111000 ; |XXXXX   |  18
                BYTE    %11110100 ; |XXXX X  |  17
                BYTE    %01111110 ; | XXXXXX |  16
                BYTE    %00001110 ; |    XXX |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %11111000 ; |XXXXX   |  13
                BYTE    %10001110 ; |X   XXX |  12
                BYTE    %10001001 ; |X   X  X|  11
                BYTE    %11011101 ; |XX XXX X|  10
                BYTE    %10001000 ; |X   X   |   9
                BYTE    %11111001 ; |XXXXX  X|   8
                BYTE    %00111111 ; |  XXXXXX|   7
                BYTE    %11111001 ; |XXXXX  X|   6
                BYTE    %10011001 ; |X  XX  X|   5
                BYTE    %10011101 ; |X  XXX X|   4
                BYTE    %01111000 ; | XXXX   |   3
                BYTE    %00110000 ; |  XX    |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

EN_KongDrum1_GRP4_0:

                BYTE    %00000000 ; |        |  31
                BYTE    %00000000 ; |        |  30
                BYTE    %00011110 ; |   XXXX |  29
                BYTE    %00000001 ; |       X|  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %00000000 ; |        |  25
                BYTE    %00000000 ; |        |  24
                BYTE    %10000000 ; |X       |  23
                BYTE    %00000000 ; |        |  22
                BYTE    %00000001 ; |       X|  21
                BYTE    %10011110 ; |X  XXXX |  20
                BYTE    %00000000 ; |        |  19
                BYTE    %00000000 ; |        |  18
                BYTE    %00000000 ; |        |  17
                BYTE    %00000000 ; |        |  16
                BYTE    %11100000 ; |XXX     |  15
                BYTE    %01110000 ; | XXX    |  14
                BYTE    %00011000 ; |   XX   |  13
                BYTE    %00000100 ; |     X  |  12
                BYTE    %00000000 ; |        |  11
                BYTE    %10000001 ; |X      X|  10
                BYTE    %10000010 ; |X     X |   9
                BYTE    %10001100 ; |X   XX  |   8
                BYTE    %00010000 ; |   X    |   7
                BYTE    %11000000 ; |XX      |   6
                BYTE    %11000000 ; |XX      |   5
                BYTE    %10000000 ; |X       |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

EN_KongDrum1_GRP5_0:

                BYTE    %00000000 ; |        |  31
                BYTE    %00000000 ; |        |  30
                BYTE    %11111100 ; |XXXXXX  |  29
                BYTE    %11011100 ; |XX XXX  |  28
                BYTE    %00111000 ; |  XXX   |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %00000000 ; |        |  25
                BYTE    %00000000 ; |        |  24
                BYTE    %01000000 ; | X      |  23
                BYTE    %01000000 ; | X      |  22
                BYTE    %10000000 ; |X       |  21
                BYTE    %00000000 ; |        |  20
                BYTE    %00000000 ; |        |  19
                BYTE    %00000000 ; |        |  18
                BYTE    %00000000 ; |        |  17
                BYTE    %00000000 ; |        |  16
                BYTE    %00000000 ; |        |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %00000000 ; |        |  13
                BYTE    %00000000 ; |        |  12
                BYTE    %01000000 ; | X      |  11
                BYTE    %10000000 ; |X       |  10
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

EN_KongDrum1_GRP0_1:

                BYTE    %00001011 ; |    X XX|  31
                BYTE    %00000101 ; |     X X|  30
                BYTE    %00000011 ; |      XX|  29
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
                BYTE    %00000000 ; |        |  18
                BYTE    %00000000 ; |        |  17
                BYTE    %00000000 ; |        |  16
                BYTE    %00000000 ; |        |  15
                BYTE    %00000001 ; |       X|  14
                BYTE    %00000111 ; |     XXX|  13
                BYTE    %00001111 ; |    XXXX|  12
                BYTE    %00111111 ; |  XXXXXX|  11
                BYTE    %01111111 ; | XXXXXXX|  10
                BYTE    %11110111 ; |XXXX XXX|   9
                BYTE    %11111011 ; |XXXXX XX|   8
                BYTE    %11111001 ; |XXXXX  X|   7
                BYTE    %01111000 ; | XXXX   |   6
                BYTE    %01100111 ; | XX  XXX|   5
                BYTE    %01101110 ; | XX XXX |   4
                BYTE    %00111101 ; |  XXXX X|   3
                BYTE    %00111011 ; |  XXX XX|   2
                BYTE    %00001111 ; |    XXXX|   1
                BYTE    %00000011 ; |      XX|   0

EN_KongDrum1_GRP1_1:

                BYTE    %11111111 ; |XXXXXXXX|  31
                BYTE    %11111111 ; |XXXXXXXX|  30
                BYTE    %11111111 ; |XXXXXXXX|  29
                BYTE    %11111111 ; |XXXXXXXX|  28
                BYTE    %11111111 ; |XXXXXXXX|  27
                BYTE    %11111111 ; |XXXXXXXX|  26
                BYTE    %11111111 ; |XXXXXXXX|  25
                BYTE    %11111111 ; |XXXXXXXX|  24
                BYTE    %01111111 ; | XXXXXXX|  23
                BYTE    %00111111 ; |  XXXXXX|  22
                BYTE    %00011110 ; |   XXXX |  21
                BYTE    %00001101 ; |    XX X|  20
                BYTE    %00011110 ; |   XXXX |  19
                BYTE    %00011110 ; |   XXXX |  18
                BYTE    %00011101 ; |   XXX X|  17
                BYTE    %00111110 ; |  XXXXX |  16
                BYTE    %01111111 ; | XXXXXXX|  15
                BYTE    %11111111 ; |XXXXXXXX|  14
                BYTE    %11111111 ; |XXXXXXXX|  13
                BYTE    %11111111 ; |XXXXXXXX|  12
                BYTE    %11111111 ; |XXXXXXXX|  11
                BYTE    %11111111 ; |XXXXXXXX|  10
                BYTE    %11111111 ; |XXXXXXXX|   9
                BYTE    %11111111 ; |XXXXXXXX|   8
                BYTE    %11111111 ; |XXXXXXXX|   7
                BYTE    %00000111 ; |     XXX|   6
                BYTE    %11000011 ; |XX    XX|   5
                BYTE    %11100001 ; |XXX    X|   4
                BYTE    %00000000 ; |        |   3
                BYTE    %10110000 ; |X XX    |   2
                BYTE    %01100000 ; | XX     |   1
                BYTE    %11000000 ; |XX      |   0

EN_KongDrum1_GRP2_1:

                BYTE    %00000000 ; |        |  31
                BYTE    %10000000 ; |X       |  30
                BYTE    %10000000 ; |X       |  29
                BYTE    %11000000 ; |XX      |  28
                BYTE    %11100000 ; |XXX     |  27
                BYTE    %11110000 ; |XXXX    |  26
                BYTE    %11011010 ; |XX XX X |  25
                BYTE    %11101010 ; |XXX X X |  24
                BYTE    %11010101 ; |XX X X X|  23
                BYTE    %11011111 ; |XX XXXXX|  22
                BYTE    %10000011 ; |X     XX|  21
                BYTE    %01111101 ; | XXXXX X|  20
                BYTE    %01111110 ; | XXXXXX |  19
                BYTE    %01111110 ; | XXXXXX |  18
                BYTE    %10111110 ; |X XXXXX |  17
                BYTE    %11010101 ; |XX X X X|  16
                BYTE    %11111111 ; |XXXXXXXX|  15
                BYTE    %11111111 ; |XXXXXXXX|  14
                BYTE    %11111111 ; |XXXXXXXX|  13
                BYTE    %11111000 ; |XXXXX   |  12
                BYTE    %11001000 ; |XX  X   |  11
                BYTE    %11011101 ; |XX XXX X|  10
                BYTE    %10001000 ; |X   X   |   9
                BYTE    %11001111 ; |XX  XXXX|   8
                BYTE    %11111111 ; |XXXXXXXX|   7
                BYTE    %11111111 ; |XXXXXXXX|   6
                BYTE    %11111101 ; |XXXXXX X|   5
                BYTE    %11111100 ; |XXXXXX  |   4
                BYTE    %00111111 ; |  XXXXXX|   3
                BYTE    %00011111 ; |   XXXXX|   2
                BYTE    %00001111 ; |    XXXX|   1
                BYTE    %00000111 ; |     XXX|   0

EN_KongDrum1_GRP3_1:

                BYTE    %00000000 ; |        |  31
                BYTE    %00000000 ; |        |  30
                BYTE    %00000000 ; |        |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000011 ; |      XX|  26
                BYTE    %11011011 ; |XX XX XX|  25
                BYTE    %11010111 ; |XX X XXX|  24
                BYTE    %10101011 ; |X X X XX|  23
                BYTE    %11111011 ; |XXXXX XX|  22
                BYTE    %11000001 ; |XX     X|  21
                BYTE    %10111110 ; |X XXXXX |  20
                BYTE    %01111111 ; | XXXXXXX|  19
                BYTE    %01111111 ; | XXXXXXX|  18
                BYTE    %01111111 ; | XXXXXXX|  17
                BYTE    %10111111 ; |X XXXXXX|  16
                BYTE    %11111111 ; |XXXXXXXX|  15
                BYTE    %11111111 ; |XXXXXXXX|  14
                BYTE    %11111111 ; |XXXXXXXX|  13
                BYTE    %10001111 ; |X   XXXX|  12
                BYTE    %10001001 ; |X   X  X|  11
                BYTE    %11011101 ; |XX XXX X|  10
                BYTE    %10001000 ; |X   X   |   9
                BYTE    %11111001 ; |XXXXX  X|   8
                BYTE    %11111111 ; |XXXXXXXX|   7
                BYTE    %11111111 ; |XXXXXXXX|   6
                BYTE    %11011111 ; |XX XXXXX|   5
                BYTE    %10011111 ; |X  XXXXX|   4
                BYTE    %11111110 ; |XXXXXXX |   3
                BYTE    %11111100 ; |XXXXXX  |   2
                BYTE    %11111000 ; |XXXXX   |   1
                BYTE    %11110000 ; |XXXX    |   0

EN_KongDrum1_GRP4_1:

                BYTE    %00000000 ; |        |  31
                BYTE    %00000000 ; |        |  30
                BYTE    %00011111 ; |   XXXXX|  29
                BYTE    %00011111 ; |   XXXXX|  28
                BYTE    %00111111 ; |  XXXXXX|  27
                BYTE    %11111111 ; |XXXXXXXX|  26
                BYTE    %11111111 ; |XXXXXXXX|  25
                BYTE    %11111111 ; |XXXXXXXX|  24
                BYTE    %01111111 ; | XXXXXXX|  23
                BYTE    %11111111 ; |XXXXXXXX|  22
                BYTE    %11111110 ; |XXXXXXX |  21
                BYTE    %01100000 ; | XX     |  20
                BYTE    %11111000 ; |XXXXX   |  19
                BYTE    %11111111 ; |XXXXXXXX|  18
                BYTE    %11111111 ; |XXXXXXXX|  17
                BYTE    %11111111 ; |XXXXXXXX|  16
                BYTE    %00011111 ; |   XXXXX|  15
                BYTE    %10001111 ; |X   XXXX|  14
                BYTE    %11100111 ; |XXX  XXX|  13
                BYTE    %11111011 ; |XXXXX XX|  12
                BYTE    %11111111 ; |XXXXXXXX|  11
                BYTE    %11111110 ; |XXXXXXX |  10
                BYTE    %11111100 ; |XXXXXX  |   9
                BYTE    %11110000 ; |XXXX    |   8
                BYTE    %11100000 ; |XXX     |   7
                BYTE    %11000000 ; |XX      |   6
                BYTE    %11000000 ; |XX      |   5
                BYTE    %10000000 ; |X       |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

EN_KongDrum1_GRP5_1:

                BYTE    %00000000 ; |        |  31
                BYTE    %00000000 ; |        |  30
                BYTE    %11111100 ; |XXXXXX  |  29
                BYTE    %11111100 ; |XXXXXX  |  28
                BYTE    %11111000 ; |XXXXX   |  27
                BYTE    %11100000 ; |XXX     |  26
                BYTE    %11000000 ; |XX      |  25
                BYTE    %11000000 ; |XX      |  24
                BYTE    %10000000 ; |X       |  23
                BYTE    %10000000 ; |X       |  22
                BYTE    %00000000 ; |        |  21
                BYTE    %00000000 ; |        |  20
                BYTE    %00000000 ; |        |  19
                BYTE    %00000000 ; |        |  18
                BYTE    %10000000 ; |X       |  17
                BYTE    %11000000 ; |XX      |  16
                BYTE    %11100000 ; |XXX     |  15
                BYTE    %11100000 ; |XXX     |  14
                BYTE    %11100000 ; |XXX     |  13
                BYTE    %11100000 ; |XXX     |  12
                BYTE    %10000000 ; |X       |  11
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
; Kong standing
;

EN_KongStand_GRP0_0 = EN_KongHit0_GRP0_0

EN_KongStand_GRP1_0:

                BYTE    %11111111 ; |XXXXXXXX|  31
                BYTE    %10111101 ; |X XXXX X|  30
                BYTE    %11000110 ; |XX   XX |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %00000000 ; |        |  25
                BYTE    %00000000 ; |        |  24
                BYTE    %10000000 ; |X       |  23
                BYTE    %01000000 ; | X      |  22
                BYTE    %00100001 ; |  X    X|  21
                BYTE    %00010010 ; |   X  X |  20
                BYTE    %00000000 ; |        |  19
                BYTE    %00000000 ; |        |  18
                BYTE    %00000000 ; |        |  17
                BYTE    %00000000 ; |        |  16
                BYTE    %00000111 ; |     XXX|  15
                BYTE    %00001110 ; |    XXX |  14
                BYTE    %00011000 ; |   XX   |  13
                BYTE    %00100000 ; |  X     |  12
                BYTE    %00000000 ; |        |  11
                BYTE    %10000001 ; |X      X|  10
                BYTE    %01000001 ; | X     X|   9
                BYTE    %00110001 ; |  XX   X|   8
                BYTE    %00001000 ; |    X   |   7
                BYTE    %00000011 ; |      XX|   6
                BYTE    %00000011 ; |      XX|   5
                BYTE    %00000001 ; |       X|   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

EN_KongStand_GRP2_0:

                BYTE    %00000000 ; |        |  31
                BYTE    %10000000 ; |X       |  30
                BYTE    %10000000 ; |X       |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %00100101 ; |  X  X X|  25
                BYTE    %00011111 ; |   XXXXX|  24
                BYTE    %00111111 ; |  XXXXXX|  23
                BYTE    %00111111 ; |  XXXXXX|  22
                BYTE    %01111111 ; | XXXXXXX|  21
                BYTE    %11111111 ; |XXXXXXXX|  20
                BYTE    %00101111 ; |  X XXXX|  19
                BYTE    %00011111 ; |   XXXXX|  18
                BYTE    %00101111 ; |  X XXXX|  17
                BYTE    %01111110 ; | XXXXXX |  16
                BYTE    %01110000 ; | XXX    |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %00001111 ; |    XXXX|  13
                BYTE    %00111111 ; |  XXXXXX|  12
                BYTE    %00111111 ; |  XXXXXX|  11
                BYTE    %11011111 ; |XX XXXXX|  10
                BYTE    %11100000 ; |XXX     |   9
                BYTE    %11111111 ; |XXXXXXXX|   8
                BYTE    %11111110 ; |XXXXXXX |   7
                BYTE    %10001111 ; |X   XXXX|   6
                BYTE    %10001100 ; |X   XX  |   5
                BYTE    %10011100 ; |X  XXX  |   4
                BYTE    %00001111 ; |    XXXX|   3
                BYTE    %00000110 ; |     XX |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

EN_KongStand_GRP3_0:

                BYTE    %00000000 ; |        |  31
                BYTE    %00000001 ; |       X|  30
                BYTE    %00000001 ; |       X|  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %00100100 ; |  X  X  |  25
                BYTE    %10111000 ; |X XXX   |  24
                BYTE    %11111100 ; |XXXXXX  |  23
                BYTE    %11111100 ; |XXXXXX  |  22
                BYTE    %11111110 ; |XXXXXXX |  21
                BYTE    %11111111 ; |XXXXXXXX|  20
                BYTE    %11110100 ; |XXXX X  |  19
                BYTE    %11111000 ; |XXXXX   |  18
                BYTE    %11110100 ; |XXXX X  |  17
                BYTE    %01111110 ; | XXXXXX |  16
                BYTE    %00001110 ; |    XXX |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %11111000 ; |XXXXX   |  13
                BYTE    %11111110 ; |XXXXXXX |  12
                BYTE    %11111110 ; |XXXXXXX |  11
                BYTE    %11111101 ; |XXXXXX X|  10
                BYTE    %00000011 ; |      XX|   9
                BYTE    %11111111 ; |XXXXXXXX|   8
                BYTE    %00111111 ; |  XXXXXX|   7
                BYTE    %11111001 ; |XXXXX  X|   6
                BYTE    %10011001 ; |X  XX  X|   5
                BYTE    %10011101 ; |X  XXX X|   4
                BYTE    %01111000 ; | XXXX   |   3
                BYTE    %00110000 ; |  XX    |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

EN_KongStand_GRP4_0:

                BYTE    %11111111 ; |XXXXXXXX|  31
                BYTE    %10111101 ; |X XXXX X|  30
                BYTE    %01100011 ; | XX   XX|  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %00000000 ; |        |  25
                BYTE    %00000000 ; |        |  24
                BYTE    %00000001 ; |       X|  23
                BYTE    %00000010 ; |      X |  22
                BYTE    %10000100 ; |X    X  |  21
                BYTE    %01001000 ; | X  X   |  20
                BYTE    %00000000 ; |        |  19
                BYTE    %00000000 ; |        |  18
                BYTE    %00000000 ; |        |  17
                BYTE    %00000000 ; |        |  16
                BYTE    %11100000 ; |XXX     |  15
                BYTE    %01110000 ; | XXX    |  14
                BYTE    %00011000 ; |   XX   |  13
                BYTE    %00000100 ; |     X  |  12
                BYTE    %00000000 ; |        |  11
                BYTE    %10000001 ; |X      X|  10
                BYTE    %10000010 ; |X     X |   9
                BYTE    %10001100 ; |X   XX  |   8
                BYTE    %00010000 ; |   X    |   7
                BYTE    %11000000 ; |XX      |   6
                BYTE    %11000000 ; |XX      |   5
                BYTE    %10000000 ; |X       |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

EN_KongStand_GRP5_0 = EN_KongHit0_GRP5_0

EN_KongStand_GRP0_1 = EN_KongHit0_GRP0_1

EN_KongStand_GRP1_1:

                BYTE    %11111111 ; |XXXXXXXX|  31
                BYTE    %11111111 ; |XXXXXXXX|  30
                BYTE    %11111111 ; |XXXXXXXX|  29
                BYTE    %11111111 ; |XXXXXXXX|  28
                BYTE    %11111111 ; |XXXXXXXX|  27
                BYTE    %11111111 ; |XXXXXXXX|  26
                BYTE    %11111111 ; |XXXXXXXX|  25
                BYTE    %11111111 ; |XXXXXXXX|  24
                BYTE    %01111111 ; | XXXXXXX|  23
                BYTE    %00111111 ; |  XXXXXX|  22
                BYTE    %00011110 ; |   XXXX |  21
                BYTE    %00001101 ; |    XX X|  20
                BYTE    %00011111 ; |   XXXXX|  19
                BYTE    %11111111 ; |XXXXXXXX|  18
                BYTE    %11111111 ; |XXXXXXXX|  17
                BYTE    %11111111 ; |XXXXXXXX|  16
                BYTE    %11111000 ; |XXXXX   |  15
                BYTE    %11110001 ; |XXXX   X|  14
                BYTE    %11100111 ; |XXX  XXX|  13
                BYTE    %11011111 ; |XX XXXXX|  12
                BYTE    %11111111 ; |XXXXXXXX|  11
                BYTE    %01111111 ; | XXXXXXX|  10
                BYTE    %00111111 ; |  XXXXXX|   9
                BYTE    %00001111 ; |    XXXX|   8
                BYTE    %00000111 ; |     XXX|   7
                BYTE    %00000011 ; |      XX|   6
                BYTE    %00000011 ; |      XX|   5
                BYTE    %00000001 ; |       X|   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

EN_KongStand_GRP2_1:

                BYTE    %00000000 ; |        |  31
                BYTE    %10000000 ; |X       |  30
                BYTE    %10000000 ; |X       |  29
                BYTE    %11000000 ; |XX      |  28
                BYTE    %11100000 ; |XXX     |  27
                BYTE    %11110000 ; |XXXX    |  26
                BYTE    %11011010 ; |XX XX X |  25
                BYTE    %11101010 ; |XXX X X |  24
                BYTE    %11010101 ; |XX X X X|  23
                BYTE    %11011111 ; |XX XXXXX|  22
                BYTE    %10000011 ; |X     XX|  21
                BYTE    %01111101 ; | XXXXX X|  20
                BYTE    %11111110 ; |XXXXXXX |  19
                BYTE    %11111110 ; |XXXXXXX |  18
                BYTE    %11111110 ; |XXXXXXX |  17
                BYTE    %11111101 ; |XXXXXX X|  16
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
                BYTE    %11111101 ; |XXXXXX X|   5
                BYTE    %11111100 ; |XXXXXX  |   4
                BYTE    %00111111 ; |  XXXXXX|   3
                BYTE    %00011111 ; |   XXXXX|   2
                BYTE    %00001111 ; |    XXXX|   1
                BYTE    %00000111 ; |     XXX|   0

EN_KongStand_GRP3_1:

                BYTE    %00000000 ; |        |  31
                BYTE    %00000001 ; |       X|  30
                BYTE    %00000001 ; |       X|  29
                BYTE    %00000011 ; |      XX|  28
                BYTE    %00000111 ; |     XXX|  27
                BYTE    %00001111 ; |    XXXX|  26
                BYTE    %11011011 ; |XX XX XX|  25
                BYTE    %11010111 ; |XX X XXX|  24
                BYTE    %10101011 ; |X X X XX|  23
                BYTE    %11111011 ; |XXXXX XX|  22
                BYTE    %11000001 ; |XX     X|  21
                BYTE    %10111110 ; |X XXXXX |  20
                BYTE    %01111111 ; | XXXXXXX|  19
                BYTE    %01111111 ; | XXXXXXX|  18
                BYTE    %01111111 ; | XXXXXXX|  17
                BYTE    %10111111 ; |X XXXXXX|  16
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
                BYTE    %11011111 ; |XX XXXXX|   5
                BYTE    %10011111 ; |X  XXXXX|   4
                BYTE    %11111110 ; |XXXXXXX |   3
                BYTE    %11111100 ; |XXXXXX  |   2
                BYTE    %11111000 ; |XXXXX   |   1
                BYTE    %11110000 ; |XXXX    |   0

EN_KongStand_GRP4_1:

                BYTE    %11111111 ; |XXXXXXXX|  31
                BYTE    %11111111 ; |XXXXXXXX|  30
                BYTE    %11111111 ; |XXXXXXXX|  29
                BYTE    %11111111 ; |XXXXXXXX|  28
                BYTE    %11111111 ; |XXXXXXXX|  27
                BYTE    %11111111 ; |XXXXXXXX|  26
                BYTE    %11111111 ; |XXXXXXXX|  25
                BYTE    %11111111 ; |XXXXXXXX|  24
                BYTE    %11111110 ; |XXXXXXX |  23
                BYTE    %11111100 ; |XXXXXX  |  22
                BYTE    %01111000 ; | XXXX   |  21
                BYTE    %10110000 ; |X XX    |  20
                BYTE    %11111000 ; |XXXXX   |  19
                BYTE    %11111111 ; |XXXXXXXX|  18
                BYTE    %11111111 ; |XXXXXXXX|  17
                BYTE    %11111111 ; |XXXXXXXX|  16
                BYTE    %00011111 ; |   XXXXX|  15
                BYTE    %10001111 ; |X   XXXX|  14
                BYTE    %11100111 ; |XXX  XXX|  13
                BYTE    %11111011 ; |XXXXX XX|  12
                BYTE    %11111111 ; |XXXXXXXX|  11
                BYTE    %11111110 ; |XXXXXXX |  10
                BYTE    %11111100 ; |XXXXXX  |   9
                BYTE    %11110000 ; |XXXX    |   8
                BYTE    %11100000 ; |XXX     |   7
                BYTE    %11000000 ; |XX      |   6
                BYTE    %11000000 ; |XX      |   5
                BYTE    %10000000 ; |X       |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

EN_KongStand_GRP5_1 = EN_KongHit0_GRP5_1

;
; Kong grinning
;

EN_KongGrin_GRP0_0 = EN_KongStand_GRP0_0

EN_KongGrin_GRP1_0 = EN_KongStand_GRP1_0

EN_KongGrin_GRP2_0:

                BYTE    %00000000 ; |        |  31
                BYTE    %10000000 ; |X       |  30
                BYTE    %10000000 ; |X       |  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %00100101 ; |  X  X X|  25
                BYTE    %00011111 ; |   XXXXX|  24
                BYTE    %00111111 ; |  XXXXXX|  23
                BYTE    %00111111 ; |  XXXXXX|  22
                BYTE    %01111111 ; | XXXXXXX|  21
                BYTE    %11111111 ; |XXXXXXXX|  20
                BYTE    %00101111 ; |  X XXXX|  19
                BYTE    %00011111 ; |   XXXXX|  18
                BYTE    %00101111 ; |  X XXXX|  17
                BYTE    %01111110 ; | XXXXXX |  16
                BYTE    %01110000 ; | XXX    |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %00001111 ; |    XXXX|  13
                BYTE    %01111000 ; | XXXX   |  12
                BYTE    %11001000 ; |XX  X   |  11
                BYTE    %11011101 ; |XX XXX X|  10
                BYTE    %10001000 ; |X   X   |   9
                BYTE    %11001111 ; |XX  XXXX|   8
                BYTE    %11111110 ; |XXXXXXX |   7
                BYTE    %10001111 ; |X   XXXX|   6
                BYTE    %10001100 ; |X   XX  |   5
                BYTE    %10011100 ; |X  XXX  |   4
                BYTE    %00001111 ; |    XXXX|   3
                BYTE    %00000110 ; |     XX |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

EN_KongGrin_GRP3_0:

                BYTE    %00000000 ; |        |  31
                BYTE    %00000001 ; |       X|  30
                BYTE    %00000001 ; |       X|  29
                BYTE    %00000000 ; |        |  28
                BYTE    %00000000 ; |        |  27
                BYTE    %00000000 ; |        |  26
                BYTE    %00100100 ; |  X  X  |  25
                BYTE    %10111000 ; |X XXX   |  24
                BYTE    %11111100 ; |XXXXXX  |  23
                BYTE    %11111100 ; |XXXXXX  |  22
                BYTE    %11111110 ; |XXXXXXX |  21
                BYTE    %11111111 ; |XXXXXXXX|  20
                BYTE    %11110100 ; |XXXX X  |  19
                BYTE    %11111000 ; |XXXXX   |  18
                BYTE    %11110100 ; |XXXX X  |  17
                BYTE    %01111110 ; | XXXXXX |  16
                BYTE    %00001110 ; |    XXX |  15
                BYTE    %00000000 ; |        |  14
                BYTE    %11111000 ; |XXXXX   |  13
                BYTE    %10001110 ; |X   XXX |  12
                BYTE    %10001001 ; |X   X  X|  11
                BYTE    %11011101 ; |XX XXX X|  10
                BYTE    %10001000 ; |X   X   |   9
                BYTE    %11111001 ; |XXXXX  X|   8
                BYTE    %00111111 ; |  XXXXXX|   7
                BYTE    %11111001 ; |XXXXX  X|   6
                BYTE    %10011001 ; |X  XX  X|   5
                BYTE    %10011101 ; |X  XXX X|   4
                BYTE    %01111000 ; | XXXX   |   3
                BYTE    %00110000 ; |  XX    |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

EN_KongGrin_GRP4_0 = EN_KongStand_GRP4_0

EN_KongGrin_GRP5_0 = EN_KongStand_GRP5_0

EN_KongGrin_GRP0_1 = EN_KongStand_GRP0_1

EN_KongGrin_GRP1_1 = EN_KongStand_GRP1_1

EN_KongGrin_GRP2_1:

                BYTE    %00000000 ; |        |  31
                BYTE    %10000000 ; |X       |  30
                BYTE    %10000000 ; |X       |  29
                BYTE    %11000000 ; |XX      |  28
                BYTE    %11100000 ; |XXX     |  27
                BYTE    %11110000 ; |XXXX    |  26
                BYTE    %11011010 ; |XX XX X |  25
                BYTE    %11101010 ; |XXX X X |  24
                BYTE    %11010101 ; |XX X X X|  23
                BYTE    %11011111 ; |XX XXXXX|  22
                BYTE    %10000011 ; |X     XX|  21
                BYTE    %01111101 ; | XXXXX X|  20
                BYTE    %11111110 ; |XXXXXXX |  19
                BYTE    %11111110 ; |XXXXXXX |  18
                BYTE    %11111110 ; |XXXXXXX |  17
                BYTE    %11111101 ; |XXXXXX X|  16
                BYTE    %11111111 ; |XXXXXXXX|  15
                BYTE    %11111111 ; |XXXXXXXX|  14
                BYTE    %11111111 ; |XXXXXXXX|  13
                BYTE    %11111000 ; |XXXXX   |  12
                BYTE    %11001000 ; |XX  X   |  11
                BYTE    %11011101 ; |XX XXX X|  10
                BYTE    %10001000 ; |X   X   |   9
                BYTE    %11001111 ; |XX  XXXX|   8
                BYTE    %11111111 ; |XXXXXXXX|   7
                BYTE    %11111111 ; |XXXXXXXX|   6
                BYTE    %11111101 ; |XXXXXX X|   5
                BYTE    %11111100 ; |XXXXXX  |   4
                BYTE    %00111111 ; |  XXXXXX|   3
                BYTE    %00011111 ; |   XXXXX|   2
                BYTE    %00001111 ; |    XXXX|   1
                BYTE    %00000111 ; |     XXX|   0

EN_KongGrin_GRP3_1:

                BYTE    %00000000 ; |        |  31
                BYTE    %00000001 ; |       X|  30
                BYTE    %00000001 ; |       X|  29
                BYTE    %00000011 ; |      XX|  28
                BYTE    %00000111 ; |     XXX|  27
                BYTE    %00001111 ; |    XXXX|  26
                BYTE    %11011011 ; |XX XX XX|  25
                BYTE    %11010111 ; |XX X XXX|  24
                BYTE    %10101011 ; |X X X XX|  23
                BYTE    %11111011 ; |XXXXX XX|  22
                BYTE    %11000001 ; |XX     X|  21
                BYTE    %10111110 ; |X XXXXX |  20
                BYTE    %01111111 ; | XXXXXXX|  19
                BYTE    %01111111 ; | XXXXXXX|  18
                BYTE    %01111111 ; | XXXXXXX|  17
                BYTE    %10111111 ; |X XXXXXX|  16
                BYTE    %11111111 ; |XXXXXXXX|  15
                BYTE    %11111111 ; |XXXXXXXX|  14
                BYTE    %11111111 ; |XXXXXXXX|  13
                BYTE    %10001111 ; |X   XXXX|  12
                BYTE    %10001001 ; |X   X  X|  11
                BYTE    %11011101 ; |XX XXX X|  10
                BYTE    %10001000 ; |X   X   |   9
                BYTE    %11111001 ; |XXXXX  X|   8
                BYTE    %11111111 ; |XXXXXXXX|   7
                BYTE    %11111111 ; |XXXXXXXX|   6
                BYTE    %11011111 ; |XX XXXXX|   5
                BYTE    %10011111 ; |X  XXXXX|   4
                BYTE    %11111110 ; |XXXXXXX |   3
                BYTE    %11111100 ; |XXXXXX  |   2
                BYTE    %11111000 ; |XXXXX   |   1
                BYTE    %11110000 ; |XXXX    |   0

EN_KongGrin_GRP4_1 = EN_KongStand_GRP4_1

EN_KongGrin_GRP5_1 = EN_KongStand_GRP5_1

; playfield bitmap behind Kong

;
; Kong drumming
;

EN_KongDrum0_PF2_0:

                BYTE    $00,$00,$00,$00,$00,$00,$00,$00
                BYTE    $00,$00,$00,$00,$00,$00,$00,$00
                BYTE    $00,$00,$00,$80,$C0,$C0,$C0,$C0
                BYTE    $00,$00,$80,$80,$00,$00,$00,$00

EN_KongDrum0_PF2_1 = EN_KongDrum0_PF2_0
EN_KongDrum1_PF2_0 = EN_KongDrum0_PF2_0
EN_KongDrum1_PF2_1 = EN_KongDrum0_PF2_0

;
; Kong standing
;

EN_KongStand_PF2_0:

                BYTE    $00,$00,$00,$00,$00,$00,$00,$00
                BYTE    $00,$00,$00,$00,$00,$00,$00,$00
                BYTE    $00,$00,$00,$00,$00,$00,$00,$00
                BYTE    $00,$00,$80,$80,$00,$00,$00,$00

EN_KongStand_PF2_1 = EN_KongStand_PF2_0

;
; Kong grinning
;

EN_KongGrin_PF2_0 = EN_KongDrum0_PF2_0
EN_KongGrin_PF2_1 = EN_KongDrum0_PF2_0

;
; Kong hitting floor
;

EN_KongHit0_PF2_0:

                BYTE    $00,$00,$00,$00,$00,$00,$00,$00
                BYTE    $00,$00,$00,$00,$00,$00,$00,$00
                BYTE    $00,$80,$C0,$C0,$C0,$C0,$C0,$00
                BYTE    $C0,$C0,$C0,$C0,$C0,$00,$00,$00

EN_KongHit0_PF2_1 = EN_KongHit0_PF2_0
EN_KongHit1_PF2_0 = EN_KongHit0_PF2_0
EN_KongHit1_PF2_1 = EN_KongHit0_PF2_0
EN_KongHit2_PF2_0 = EN_KongHit0_PF2_0
EN_KongHit2_PF2_1 = EN_KongHit0_PF2_0

; 16 0-bytes

EN_Empty16 = EN_KongHit0_PF2_0

; ********************************************************************

#if PRINT_MESSAGES
                ECHO "---- Bank 6: ", (ROMJumpTable - *), "bytes of ROM left"
#endif
