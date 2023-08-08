
; ********************************************************************
;  libvcs
;
;    Atari VCS 2600 support library
;
;    $Date: Sat, 14 Jan 2017 19:34:58 +0100 $
;    $Author: dietrich $
;    $Revision: 478 $
;
;  Copyright (C) 2017 Andreas Dietrich
; ********************************************************************

; ********************************************************************
;
;       Generic Macros
;
; ********************************************************************

        MAC STOREBYTE
                lda     #({1})
                sta     {2}
        ENDM

;---------------------------------------------------------------------

        MAC STOREBYTE_X
                lda     #({1})
                sta     {2},x
        ENDM

;---------------------------------------------------------------------

        MAC STOREWORD
                lda     #<({1})
                sta     {2}
                lda     #>({1})
                sta     {2}+1
        ENDM

;---------------------------------------------------------------------

        MAC FILL
                REPEAT {1}
                {2}
                REPEND
        ENDM


; ********************************************************************
;
;       Subroutine Macros
;
; ********************************************************************

; --------------------------------------------------------------------
;       Color Subroutines
; --------------------------------------------------------------------

; Input
;   A   : time
;   X   : phase
;   Y   : chroma
;   Ptr : color table (luma)
;
; Uses
;   Temp
;
; Parameters
;   {1} : sine table address
;   {2} : sine table size

        MAC WRITE_SINE_BAR

                stx     Temp
                clc
                adc     Temp
                cmp     #{2}
                bcc     .modulo
                sec
                sbc     #{2}
.modulo         tax

                sty     Temp
                lda     {1},x
                clc
                adc     #15
                tax
                ldy     #15
.loop           lda     (Ptr),y
                ora     Temp
                sta     LineColorBuffer,x
                dex
                dey
                bpl     .loop

        ENDM

; --------------------------------------------------------------------

; Input
;   Y : number of lines - 1
;
; Parameters
;   {1} : color table
;   {2} : color register

        MAC COLORBAR

.loop           lda     {1},y
                sta     WSYNC
                sta     {2}
                dey
                bpl     .loop

        ENDM

; --------------------------------------------------------------------
;       Display Subroutines
; --------------------------------------------------------------------

; ROM space efficient vertical sync
;   by Edwin Blink

        MAC VERTICAL_SYNC_SMALL

                lda     #%00001110      ; each '1' bit generates a VSYNC ON line (bits 1..3)
.loop           sta     WSYNC           ; 1st '0' bit resets VSYNC, 2nd '0' bit exits loop
                sta     VSYNC
                lsr
                bne     .loop

        ENDM

; --------------------------------------------------------------------

        MAC VERTICAL_SYNC

                lda     #%00000010
                sta     WSYNC           ; line 0 -- VSYNC on
                sta     VSYNC
                sta     WSYNC           ; line 1
                sta     WSYNC           ; line 2

                lda     #44             ; set timer to skip VBLANK
                sta     TIM64T

                sta     WSYNC           ; line 3 -- VSYNC off
                sta     VSYNC

        ENDM

; --------------------------------------------------------------------

        MAC OVERSCAN

                lda     #%00000010
                sta     WSYNC           ; VBLANK on
                sta     VBLANK

                lda     #0              ; clear all channels
                sta     ENAM0
                sta     ENAM1
                sta     ENABL
                sta     GRP0
                sta     GRP1
                sta     GRP0
                sta     PF0
                sta     PF1
                sta     PF2

                lda     #35             ; set timer to skip OVERSCAN
                sta     TIM64T

        ENDM

; --------------------------------------------------------------------

; Input
;   Y : number of lines

        MAC WAIT_Y_LINES

.loop           sta     WSYNC
                dey
                bne     .loop

        ENDM

; Parameters
;   {1} : number of lines

        MAC WAIT_LINES

                ldy     #{1}
                WAIT_Y_LINES

        ENDM

; --------------------------------------------------------------------
;       Sprite Subroutines
; --------------------------------------------------------------------

; Input
;   A : horizontal position (0 - 159)
;   X : object ID (0 - 4) for P0,P1,M0,M1,BL
;
;   For X = M0,M1,BL, A must be increased by 1 due to different RES timing.
;
; Parameters
;   {1} : HPosTab

        MAC H_POS

                sta     HMCLR

                H_POS_NO_HMOVE {1}

                sta     WSYNC
                sta     HMOVE

        ENDM

; --------------------------------------------------------------------

; Input
;   A : horizontal position (0 - 159)
;   X : object ID (0 - 4) for P0,P1,M0,M1,BL
;
;   For X = M0,M1,BL, A must be increased by 1 due to different RES timing.
;
; Parameters
;   {1} : HPosTab

        MAC H_POS_NO_HMOVE

                tay
                lda     {1},y
                sta     HMP0,x
                and     #$0F
                tay

                sta     WSYNC
                nop
                nop
                nop
                nop
                nop
                nop
                nop
.loop           dey
                bpl     .loop
                sta     RESP0,x

        ENDM

; --------------------------------------------------------------------

; Horizontal sprite positioning
;   from Battlezone by Michael Feinstein
;
; Input
;   A : horizontal position (0 - 159)
;   X : object ID (0 - 4) for P0,P1,M0,M1,BL
;
;   For X = M0,M1,BL, A must be increased by 1 due to different RES timing.

        MAC H_POS_BZ

                sta     HMCLR

                H_POS_BZ_NO_HMOVE

                sta     WSYNC
                sta     HMOVE

        ENDM

; --------------------------------------------------------------------

; Horizontal sprite positioning
;   from Battlezone by Michael Feinstein
;
; Input
;   A : horizontal position (0 - 159)
;   X : object ID (0 - 4) for P0,P1,M0,M1,BL
;
;   For X = M0,M1,BL, A must be increased by 1 due to different RES timing.

        MAC H_POS_BZ_NO_HMOVE

                sec
                sta     WSYNC
.loop           sbc     #15
                bcs     .loop
                eor     #7
                asl
                asl
                asl
                asl
                sta.wx  HMP0,x
                sta     RESP0,x

        ENDM

; --------------------------------------------------------------------

        MAC H_POS_B48_CENTERED

                ldx     #6
                sta     WSYNC
.loop           dex
                bpl     .loop
                sta     RESP0
                sta     RESP1
                lda     #$80
                sta     HMP0
                lda     #$90
                sta     HMP1
                sta     WSYNC
                sta     HMOVE

        ENDM

; --------------------------------------------------------------------

        MAC H_POS_B96_CENTERED

                ldx     #5
                sta     WSYNC
.loop           dex
                bpl     .loop
                sta     RESP0
                sta     RESP1
                lda     #$10
                sta     HMP0
                lda     #$A0
                sta     HMP1
                sta     WSYNC
                sta     HMOVE

        ENDM

; --------------------------------------------------------------------

; Input
;   LineCtr : number of lines - 1
;
; Uses
;   Temp
;
; Parameters
;   {1} : foreground color table
;   {2} : background color table
;   {3} : character 1 bitmap
;   {4} : character 2 bitmap
;   {5} : character 3 bitmap
;   {6} : character 4 bitmap
;   {7} : character 5 bitmap

        MAC BITMAP_40

.loop           ldy     LineCtr
                lda     {1},y           ; foreground color
                sta     COLUP0
                sta     WSYNC
                sta     COLUP1
                lda     {2},y           ; background color
                sta     COLUBK
                lda     {3},y           ; 1. character
                ldx     {4},y           ; 2. character
                sta     GRP0
                stx     GRP1
                lda     {5},y           ; 3. character
                ldx     {6},y           ; 4. character
                sta     GRP0
                lda     {7},y           ; 5. character
                ldy     Temp
                stx     GRP1
                sta     GRP0
                sty     GRP1
                sta     GRP0

                dec     LineCtr
                bpl     .loop

        ENDM

; Offsets for table addresses when used as RAM code

RAM_B40FGCol = 03
RAM_B40BGCol = 12
RAM_B40Char0 = 17
RAM_B40Char1 = 20
RAM_B40Char2 = 27
RAM_B40Char3 = 30
RAM_B40Char4 = 35

; --------------------------------------------------------------------

; Input
;   LineCtr : number of lines - 1
;
; Uses
;   Temp
;
; Parameters
;    {1} : first color register (e.g., COLUP0)
;    {2} : second color register (e.g., COLUP1)
;    {3} : color table
;    {4} : character 1 bitmap
;    {5} : character 2 bitmap
;    {6} : character 3 bitmap
;    {7} : character 4 bitmap
;    {8} : character 5 bitmap
;    {9} : character 6 bitmap
;   {10} : border mask

        MAC BITMAP_48

.loop           ldy     LineCtr
                lda     {3},y           ; color table
                sta     {1}             ; 1. color register
                sta     {2}             ; 2. color register
                lda     {4},y           ; 1. character
                sta     GRP0
                lda     {5},y           ; 2. character
                sta     GRP1
                lda     {6},y           ; 3. character
                sta     GRP0
                lda     {7},y           ; 4. character
                sta     Temp
                lda     {8},y           ; 5. character
                tax
                lda     {9},y           ; 6. character
                and     #{10}           ; border mask
                ldy     Temp
                sty     GRP1
                stx     GRP0
                sta     GRP1
                sta     GRP0

                dec     LineCtr
                bpl     .loop

        ENDM

; Offsets for table addresses when used as RAM code

RAM_B48FGCol =   3
RAM_B48Col0  =   6
RAM_B48Col1  =   8
RAM_B48Char0 =  10
RAM_B48Char1 =  15
RAM_B48Char2 =  20
RAM_B48Char3 =  25
RAM_B48Char4 =  30
RAM_B48Char5 =  34
RAM_B48Mask  =  37

; --------------------------------------------------------------------

; Input
;   LineCtr : number of lines - 1
;
; Uses
;   Temp
;
; Parameters
;    {1} : first color register (e.g., COLUP0)
;    {2} : second color register (e.g., COLUP1)
;    {3} : color table
;    {4} : character 1 bitmap
;    {5} : character 2 bitmap
;    {6} : character 3 bitmap
;    {7} : character 4 bitmap
;    {8} : character 5 bitmap
;    {9} : character 6 bitmap
;   {10} : border mask

        MAC BITMAP_48_CENTERED

                sta     WSYNC
                ldx     #11
.loop           dex
                bpl     .loop

                BITMAP_48 {1}, {2}, {3}, {4}, {5}, {6}, {7}, {8}, {9}, {10}

        ENDM

; --------------------------------------------------------------------

; Input
;   LineCtr : number of lines - 1
;
; Uses
;   ScorePtr[12]
;   Temp

        MAC BITMAP_48_SCORE

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

        ENDM

; --------------------------------------------------------------------

; Input
;   X : leftmost X-position of 48bit bitmap (0 - 160)
;
; Uses
;   Ptr
;   Temp

        MAC B48_DELAY

                ; adjust X delay to account for div 3 rounding errors

                inx
                inx

                ; divide by 3 (Bob Sander-Cederlof, Apple Assembly Line)

                clc
                txa
                sta     Temp
                lsr
                lsr
                adc     Temp
                ror
                lsr
                adc     Temp
                ror
                lsr
                adc     Temp
                ror
                lsr
                adc     Temp
                ror
                lsr
                sta     Temp

                ; delay (based on Eckhard Stolberg's BigMove Demo)

                sec
                lda     #<.delay_end
                sbc     Temp
                sta     Ptr
                lda     #>.delay_end
                sbc     #0
                sta     Ptr+1

                sta     WSYNC
                ldy     #4
.loop           dey
                bpl     .loop
                jmp     (Ptr)

.delay          REPEAT 53               ; 159 / 3
                BYTE    $C9             ; CMP
                REPEND
.delay_end      nop
                nop
                nop

                ;
                ; jsr BITMAP_48
                ;

        ENDM

; --------------------------------------------------------------------

; Input
;   A : leftmost X-position of 48bit bitmap (0 - 160)
;
; Parameters
;   {1} : space character (table of 0-bytes)

        MAC B48_SHIFT_SPRITES

                ldx     #1

                cmp     #160-4*8
                bcc     .shift1
                cmp     #160-3*8
                bcc     .shift2
                cmp     #160-2*8
                bcc     .shift3
                cmp     #160-1*8
                bcc     .shift4
                cmp     #160-0*8
                bcc     .shift5
                bcs     .clear6

.shift1         lda     RAM_B48Char5,x
                sta     RAM_B48Char6,x
                lda     RAM_B48Char4,x
                sta     RAM_B48Char5,x
                lda     RAM_B48Char3,x
                sta     RAM_B48Char4,x
                lda     RAM_B48Char2,x
                sta     RAM_B48Char3,x
                lda     RAM_B48Char1,x
                sta     RAM_B48Char2,x
                dex
                bpl     .shift1
                bmi     .clear1

.shift2         lda     RAM_B48Char4,x
                sta     RAM_B48Char6,x
                lda     RAM_B48Char3,x
                sta     RAM_B48Char5,x
                lda     RAM_B48Char2,x
                sta     RAM_B48Char4,x
                lda     RAM_B48Char1,x
                sta     RAM_B48Char3,x
                dex
                bpl     .shift2
                bmi     .clear2

.shift3         lda     RAM_B48Char3,x
                sta     RAM_B48Char6,x
                lda     RAM_B48Char2,x
                sta     RAM_B48Char5,x
                lda     RAM_B48Char1,x
                sta     RAM_B48Char4,x
                dex
                bpl     .shift3
                bmi     .clear3

.shift4         lda     RAM_B48Char2,x
                sta     RAM_B48Char6,x
                lda     RAM_B48Char1,x
                sta     RAM_B48Char5,x
                dex
                bpl     .shift4
                bmi     .clear4

.shift5         lda     RAM_B48Char1,x
                sta     RAM_B48Char6,x
                dex
                bpl     .shift5
                bmi     .clear5

.clear6         STOREWORD {1},RAM_B48Char6
.clear5         STOREWORD {1},RAM_B48Char5
.clear4         STOREWORD {1},RAM_B48Char4
.clear3         STOREWORD {1},RAM_B48Char3
.clear2         STOREWORD {1},RAM_B48Char2
.clear1         STOREWORD {1},RAM_B48Char1

        ENDM

; --------------------------------------------------------------------

; Input
;   Y : number of lines - 1
;
; Uses
;   FrameCtr
;
; Parameters
;   {1} : color register (e.g., COLUBK)
;   {2} : color table
;   {3} : character bitmap pointer [12*2]

        MAC BITMAP_96

                lda     #$80
                sta     HMP0
                sta     HMP1

                ; interlace switch

                lda     FrameCtr
                lsr
                bcs     .odd
                sta     WSYNC

                ; draw even columns

.even           lda     {2},y           ; color
                sta     {1}
                lda     ({3}+$10),y     ; column 8
                tax
                lda     ({3}+$00),y     ; column 0
                sta     GRP0
                lda     ({3}+$04),y     ; column 2
                sta     GRP1
                lda     ({3}+$08),y     ; column 4
                sta     GRP0
                lda     ({3}+$0C),y     ; column 6
                sta     GRP1
                stx     GRP0
                lda     ({3}+$14),y     ; column 10
                sta     GRP1

                lda     #$80            ; move +8 pixels
                sta     HMP0
                sta     HMP1

                dey
                bmi     .end

                ; draw odd columns

.odd            sta     WSYNC
                sta     HMOVE

                lda     {2},y           ; color
                sta     {1}
                lda     ({3}+$16),y     ; column 11
                tax
                lda     ({3}+$02),y     ; column 1
                sta     GRP0
                lda     ({3}+$06),y     ; column 3
                sta     GRP1
                lda     ({3}+$0A),y     ; column 5
                sta     GRP0
                lda     ({3}+$0E),y     ; column 7
                sta     GRP1
                lda     ({3}+$12),y     ; column 9
                sta     GRP0
                stx     GRP1

                nop
                nop
                nop
                sta.w   HMCLR
                sta     HMOVE           ; at cycle 74 to move -8 pixels

                dey
                bpl     .even
.end

        ENDM

; --------------------------------------------------------------------
;       Sound Subroutines
; --------------------------------------------------------------------

; Input
;   X : track lo-byte
;   Y : track hi-byte
;
; Uses
;   NPTrackPtrL[2]
;   NPTrackPtrH[2]
;   NPNote[2]
;   NPTimer[2]
;
; Parameters
;   {1} : audio channel (0, 1)

        MAC INIT_NOISE_PLAYER

                stx     NPTrackPtrL+{1}
                sty     NPTrackPtrH+{1}
                ldx     #0
                stx     NPNote+{1}
                inx
                stx     NPTimer+{1}

        ENDM

; Input
;   X : audio channel (0, 1)
;
; Uses
;   NPTrackPtrL[2]
;   NPTrackPtrH[2]
;   NPNote[2]
;   NPTimer[2]
;   Ptr
;   Temp

        MAC DRIVE_NOISE_PLAYER

                dec     NPTimer,x
                bne     .end

.track          lda     NPTrackPtrL,x
                sta     Ptr
                lda     NPTrackPtrH,x
                sta     Ptr+1

                ldy     #3
                lda     (Ptr),y
                dey
                ora     (Ptr),y
                bne     .play
                dey
                lda     (Ptr),y
                sta     NPTrackPtrH,x
                dey
                lda     (Ptr),y
                sta     NPTrackPtrL,x
                jmp     .track

.play           dey
                lda     (Ptr),y
                sta     Temp
                dey
                lda     (Ptr),y
                sta     Ptr
                lda     Temp
                sta     Ptr+1

.pattern        ldy     NPNote,x
                lda     (Ptr),y
                sta     AUDF0,x
                iny
                lda     (Ptr),y
                sta     AUDV0,x
                lsr
                lsr
                lsr
                lsr
                sta     AUDC0,x
                iny
                lda     (Ptr),y
                sta     NPTimer,x
                iny
                lda     (Ptr),y
                bpl     .next

                clc
                lda     NPTrackPtrL,x
                adc     #2
                sta     NPTrackPtrL,x
                lda     NPTrackPtrH,x
                adc     #0
                sta     NPTrackPtrH,x
                ldy     #0

.next           sty     NPNote,x
.end
        ENDM

; --------------------------------------------------------------------

;
; INIT_SOUND_FX, DO_SOUND_FX, DRIVE_SOUND_FX
;   original code by Manuel Rotschkar
;

; Input
;   X : audio channel (0, 1)
;   Y : sound effect index
;
; Uses
;   FXFreq[2]
;   FXVol[2]
;   FXTimer[2]
;   FXType[2]
;
; Parameters
;   {1} : audio control table
;   {2} : audio frequency start table
;   {3} : audio volume start table
;   {4} : effect length table

        MAC INIT_SOUND_FX

                lda     {1},y
                sta     AUDC0,x
                lda     {2},y
                sta     FXFreq,x
                lda     {3},y
                sta     FXVol,x
                lda     {4},y
                sta     FXTimer,x
                sty     FXType,x

        ENDM

; Input
;   X : audio channel (0, 1, 2=both)
;   Y : sound effect index
;
; Uses
;   FXFreq[2]
;   FXVol[2]
;   FXTimer[2]
;   FXType[2]
;
; Parameters
;   {1} : audio control table
;   {2} : audio frequency start table
;   {3} : audio volume start table
;   {4} : effect length table

        MAC DO_SOUND_FX

                cpx     #2
                bcc     .init

                ldx     #1
                lda     FXTimer
                cmp     FXTimer+1
                bcs     .init
                ldx     #0

.init           INIT_SOUND_FX {1}, {2}, {3}, {4}

        ENDM

; Input
;   X : audio channel (0, 1)
;
; Uses
;   FXFreq[2]
;   FXVol[2]
;   FXTimer[2]
;   FXType[2]
;
; Parameters
;   {1} : audio control table
;   {2} : audio frequency start table
;   {3} : audio volume start table
;   {4} : effect length table
;   {5} : audio frequency add table
;   {6} : audio frequency AND table
;   {7} : audio frequency OR table
;   {8} : audio volume add table
;   {9} : spawned effect table

        MAC DRIVE_SOUND_FX

                ldy     FXType,x
                lda     FXTimer,x
                beq     .over

.continue       dec     FXTimer,x
.freq           lda     FXFreq,x
                clc
                adc     {5},y
                sta     FXFreq,x
                and     {6},y
                ora     {7},y
                lsr
                lsr
                lsr
                sta     AUDF0,x
.vol            lda     FXVol,x
                clc
                adc     {8},y
                and     #$7F
                sta     FXVol,x
                lsr
                lsr
                lsr
                sta     AUDV0,x
                bpl     .end

.over           sta     AUDV0,x
                lda     {9},y
                bmi     .end
                tay

                INIT_SOUND_FX {1}, {2}, {3}, {4}
.end
        ENDM

; --------------------------------------------------------------------
;       Score Subroutines
; --------------------------------------------------------------------

; Input
;   X : Score table index (SCORE_1, ...)
;
; Uses
;   Score[3]
;
; Parameters
;   {1} : BCD score table (e.g., DATA_ScoreTab)

; Score table indices (used with DATA_ScoreTab)

SCORE_1         =  0
SCORE_10        =  2
SCORE_50        =  4
SCORE_100       =  6

        MAC ADD_TO_SCORE

                sed
                clc
                lda     {1}+1,x
                adc     Score+2
                sta     Score+2
                lda     {1}+0,x
                adc     Score+1
                sta     Score+1
                lda     #0
                adc     Score+0
                sta     Score+0
                cld

                bcc     .end
                lda     #$99
                sta     Score+0
                sta     Score+1
                sta     Score+2
.end
        ENDM

; --------------------------------------------------------------------

; Uses
;   Score[3]
;   ScorePtr[12]
;
; Parameters
;   {1} : digit font bitmap (8 bytes/character), 0-9 Must be within a page

        MAC SET_SCORE_PTRS

                lda     #>({1})
                sta     ScorePtr+$01
                sta     ScorePtr+$03
                sta     ScorePtr+$05
                sta     ScorePtr+$07
                sta     ScorePtr+$09
                sta     ScorePtr+$0B

                ldx     #2
                ldy     #10
                clc

.loop           lda     Score,x
                and     #$0F
                asl
                asl
                asl
                adc     #<({1})
                sta     ScorePtr,y
                dey
                dey

                lda     Score,x
                and     #$F0
                lsr
                adc     #<({1})
                sta     ScorePtr,y
                dey
                dey

                dex
                bpl     .loop

        ENDM

; --------------------------------------------------------------------
;       Math Subroutines
; --------------------------------------------------------------------

; Uses
;   Random

        MAC GET_RANDOM

                lda     Random
                beq     .xseed
                lsr
                bcc     .srand
.xseed          eor     #$AF
.srand          sta     Random

        ENDM

; --------------------------------------------------------------------
;       Memory Subroutines
; --------------------------------------------------------------------

; ROM space efficient reset
;   by Andrew Davie

        MAC RESET

                sei
                cld
                ldx     #0
                txa
.loop           dex
                txs
                pha
                bne     .loop

        ENDM

; --------------------------------------------------------------------

; Parameters
;   {1} : start address
;   {2} : end address

        MAC CLEAR_RAM

                lda     #0
                ldx     #{2}-{1}
.loop           sta     {1},x
                dex
                bpl     .loop

        ENDM

; --------------------------------------------------------------------

        MAC CLEAR_TIA

                CLEAR_RAM NUSIZ0, RESMP1

        ENDM

; --------------------------------------------------------------------

; Parameters
;   {1} : start address
;   {2} : end address

        MAC CLEAR_RAM_TIA

                CLEAR_RAM {1}, {2}

                ldx     #RESMP1-NUSIZ0
.loop           sta     NUSIZ0,x
                dex
                bpl     .loop

        ENDM

; --------------------------------------------------------------------

; Input
;   Ptr : pointer to source data
;   A   : end of target RAM space (start + Y)
;   Y   : size of RAM block - 1
;
; Uses
;   Temp

        MAC COPY_TO_RAM_FAST

                tsx
                stx     Temp
                tax
                txs
.loop           lda     (Ptr),y
                pha
                dey
                bpl     .loop
                ldx     Temp
                txs

        ENDM

; --------------------------------------------------------------------

; Input
;   A   : end of target RAM space (start + Y)
;   Y   : size of RAM block - 1
;
; Uses
;   Temp

        MAC CLEAR_RAM_FAST

                tsx
                stx     Temp
                tax
                txs
                lda     #0
.loop           pha
                dey
                bpl     .loop
                ldx     Temp
                txs

        ENDM


; ********************************************************************
;
;       Subroutine Wrappers
;
; ********************************************************************

; color subroutines

        MAC FUNC_WriteSineBar
                WRITE_SINE_BAR {1}, {2}
                rts
        ENDM

        MAC FUNC_Colorbar
                COLORBAR {1}, {2}
                rts
        ENDM

; display subroutines

        MAC FUNC_VerticalSyncSmall
                VERTICAL_SYNC_SMALL
                rts
        ENDM

        MAC FUNC_VerticalSync
                VERTICAL_SYNC
                rts
        ENDM

        MAC FUNC_Overscan
                OVERSCAN
                rts
        ENDM

        MAC FUNC_WaitYLines
                WAIT_Y_LINES
                rts
        ENDM

        MAC FUNC_WaitLines
                WAIT_LINES
                rts
        ENDM

; sprite subrouines

        MAC FUNC_HPos
                H_POS {1}
                rts
        ENDM

        MAC FUNC_HPosNoHMOVE
                H_POS_NO_HMOVE {1}
                rts
        ENDM

        MAC FUNC_HPosBZ
                H_POS_BZ
                rts
        ENDM

        MAC FUNC_HPosBZNoHMOVE
                H_POS_BZ_NO_HMOVE
                rts
        ENDM

        MAC FUNC_HPosB48Centered
                H_POS_B48_CENTERED
                rts
        ENDM

        MAC FUNC_HPosB96Centered
                H_POS_B96_CENTERED
                rts
        ENDM

        MAC FUNC_Bitmap40
                BITMAP_40 {1}, {2}, {3}, {4}, {5}, {6}, {7}
                rts
        ENDM

        MAC FUNC_Bitmap48
                BITMAP_48 {1}, {2}, {3}, {4}, {5}, {6}, {7}, {8}, {9}, {10}
                rts
        ENDM

        MAC FUNC_Bitmap48Centered
                BITMAP_48_CENTERED {1}, {2}, {3}, {4}, {5}, {6}, {7}, {8}, {9}, {10}
                rts
        ENDM

        MAC FUNC_Bitmap48Score
                BITMAP_48_SCORE
                rts
        ENDM

        MAC FUNC_B48Delay
                B48_DELAY
                rts
        ENDM

        MAC FUNC_B48ShiftSprites
                B48_SHIFT_SPRITES {1}
                rts
        ENDM

        MAC FUNC_Bitmap96
                BITMAP_96 {1}, {2}, {3}
                rts
        ENDM

; sound subroutines

        MAC FUNC_InitNoisePlayer
                INIT_NOISE_PLAYER {1}
                rts
        ENDM

        MAC FUNC_DriveNoisePlayer
                DRIVE_NOISE_PLAYER
                rts
        ENDM

        MAC FUNC_InitSoundFX
                INIT_SOUND_FX {1}, {2}, {3}, {4}
                rts
        ENDM

        MAC FUNC_DoSoundFX
                DO_SOUND_FX {1}, {2}, {3}, {4}
                rts
        ENDM

        MAC FUNC_DriveSoundFX
                DRIVE_SOUND_FX {1}, {2}, {3}, {4}, {5}, {6}, {7}, {8}, {9}
                rts
        ENDM

; score subroutines

        MAC FUNC_AddToScore
                ADD_TO_SCORE {1}
                rts
        ENDM

        MAC FUNC_SetScorePtrs
                SET_SCORE_PTRS {1}
                rts
        ENDM

; math subroutines

        MAC FUNC_GetRandom
                GET_RANDOM
                rts
        ENDM

; memory subroutines

        MAC FUNC_ClearRAM
                CLEAR_RAM {1}, {2}
                rts
        ENDM

        MAC FUNC_ClearTIA
                CLEAR_TIA
                rts
        ENDM

        MAC FUNC_ClearRAMTIA
                CLEAR_RAM_TIA {1}, {2}
                rts
        ENDM

        MAC FUNC_CopyToRAMFast
                COPY_TO_RAM_FAST
                rts
        ENDM

        MAC FUNC_ClearRAMFast
                CLEAR_RAM_FAST
                rts
        ENDM


; ********************************************************************
;
;       Data Macros
;
; ********************************************************************

; Generic score BCD values
;   2 bytes per value (BCD digits 0 - 3)
;   upper 2 digits are assumed as 0 (6 digits total)

        MAC DATA_ScoreTab

                ;      1
                BYTE    $00,$01
                ;     10
                BYTE    $00,$10
                ;     50
                BYTE    $00,$50
                ;    100
                BYTE    $01,$00

        ENDM

; --------------------------------------------------------------------

; Horizontal positioning table (X = 0-159)
;   upper half : horizontal motion value
;   lower half : 15-pixel jumps
;   table is shifted so that the first entry relates to X=0

        MAC DATA_HPosTab

                BYTE                 $30,$20,$10,$00,$F0,$E0,$D0,$C0,$B0,$A0,$90,$80
                BYTE     $61,$51,$41,$31,$21,$11,$01,$F1,$E1,$D1,$C1,$B1,$A1,$91,$81
                BYTE     $62,$52,$42,$32,$22,$12,$02,$F2,$E2,$D2,$C2,$B2,$A2,$92,$82
                BYTE     $63,$53,$43,$33,$23,$13,$03,$F3,$E3,$D3,$C3,$B3,$A3,$93,$83
                BYTE     $64,$54,$44,$34,$24,$14,$04,$F4,$E4,$D4,$C4,$B4,$A4,$94,$84
                BYTE     $65,$55,$45,$35,$25,$15,$05,$F5,$E5,$D5,$C5,$B5,$A5,$95,$85
                BYTE     $66,$56,$46,$36,$26,$16,$06,$F6,$E6,$D6,$C6,$B6,$A6,$96,$86
                BYTE     $67,$57,$47,$37,$27,$17,$07,$F7,$E7,$D7,$C7,$B7,$A7,$97,$87
                BYTE     $68,$58,$48,$38,$28,$18,$08,$F8,$E8,$D8,$C8,$B8,$A8,$98,$88
                BYTE     $69,$59,$49,$39,$29,$19,$09,$F9,$E9,$D9,$C9,$B9,$A9,$99,$89
                BYTE     $6A,$5A,$4A,$3A,$2A,$1A,$0A,$FA,$EA,$DA,$CA,$BA,$AA,$9A,$8A
                BYTE     $60,$50,$40

        ENDM

; --------------------------------------------------------------------

; Sine table
;   120 values in 3-degree steps
;   amplitude = 16 (range 0 - 32)

        MAC DATA_SineTab32

                ; 0 - 87
                BYTE     $10,$11,$12,$13,$13,$14,$15,$16,$17,$17
                BYTE     $18,$19,$19,$1A,$1B,$1B,$1C,$1C,$1D,$1D
                BYTE     $1E,$1E,$1F,$1F,$1F,$1F,$20,$20,$20,$20
                ; 90 - 177
                BYTE     $20,$20,$20,$20,$20,$1F,$1F,$1F,$1F,$1E
                BYTE     $1E,$1D,$1D,$1C,$1C,$1B,$1B,$1A,$19,$19
                BYTE     $18,$17,$17,$16,$15,$14,$13,$13,$12,$11
                ; 180 - 267
                BYTE     $10,$0F,$0E,$0D,$0D,$0C,$0B,$0A,$09,$09
                BYTE     $08,$07,$07,$06,$05,$05,$04,$04,$03,$03
                BYTE     $02,$02,$01,$01,$01,$01,$00,$00,$00,$00
                ; 270 - 357
                BYTE     $00,$00,$00,$00,$00,$01,$01,$01,$01,$02
                BYTE     $02,$03,$03,$04,$04,$05,$05,$06,$07,$07
                BYTE     $08,$09,$09,$0A,$0B,$0C,$0D,$0D,$0E,$0F

        ENDM

; --------------------------------------------------------------------

; Mask table
;   masks out 0 - 7 rightmost bits

        MAC DATA_MaskTab

                BYTE    %11111111,%11111110,%11111100,%11111000
                BYTE    %11110000,%11100000,%11000000,%10000000

        ENDM

; --------------------------------------------------------------------

; Example sound effects table
;   from Seawolf by Manuel Rotschkar
;
; Parameters
;   {1} : label prefix

        MAC DATA_SFXTab_Seawolf

                ; shot, bink, mineexp, shipsink, shipexp, subsink, restock, charge, ...

{1}AudC:        BYTE      8   ,    4   ,    8   ,    8   ,    8   ,   12   ,    8   ,    8   ,    8   ,    4   ,   12
{1}AudF:        BYTE   20*8-1 ,  6*8-1 ,  8*8-1 ,  0*8-1 , 20*8-1 , 32*8-1 ,  8*8-1 ,  0*8-1 , 20*8-1 , 16*8-1 , 20*8-1
{1}AudV:        BYTE    2*8-1 , 15*8-1 , 15*8-1 , 15*8-1 , 15*8-1 , 15*8-1 ,  5*8-1 ,  9*8-1 ,  5*8-1 , 15*8-1 ,  2*8-1
{1}Length:      BYTE    3*8   ,  1*8   ,  2*8   ,  5*8   ,  3*8   ,  7*8   ,  2*8   ,  4*8   ,  2*8   ,  2*8   ,  3*8
{1}AddF:        BYTE      0   ,    0   ,    1   , -128   ,    2   ,  -32   ,    1   , -128   ,    2   ,   -4   ,    0
{1}AndF:        BYTE    $FF   ,  $FF   ,  $FF   ,  $FF   ,  $FF   ,  $FF   ,  $FF   ,  $FF   ,  $FF   ,  $FF   ,  $FF
{1}OrF:         BYTE    $00   ,  $00   ,  $00   ,  $00   ,  $00   ,  $00   ,  $00   ,  $00   ,  $00   ,  $00   ,  $00
{1}AddV:        BYTE      1   ,    0   ,    0   ,   -2   ,    0   ,   -4   ,    0   ,   -2   ,    0   ,    0   ,    1
{1}Spawn:       BYTE     -1   ,   -1   ,    6   ,    7   ,    8   ,   -1   ,   -1   ,   -1   ,   -1   ,   -1   ,   -1

        ENDM

; --------------------------------------------------------------------

; Example music track
;   from Donkey Kong by Yukio Kaneoka

        MAC DATA_NPTrack_DonkeyKong

.track          WORD    .pattern             ; pattern pointer
                WORD    .track               ; jump destination
                WORD    0                    ; jump command

                ;      AUDF   AUDC|V  Lenght

.pattern        BYTE    31,     $C2,     6   ; E3
                BYTE     0,       0,    23   ; -
                BYTE    24,     $C2,     6   ; G3#
                BYTE     0,       0,    15   ; -
                BYTE    20,     $C2,     6   ; B3
                BYTE     0,       0,     4   ; -
                BYTE    18,     $C2,     6   ; C4#
                BYTE     0,       0,     4   ; -
                BYTE    20,     $C2,     6   ; B3
                BYTE     0,       0,     4   ; -
                BYTE    -1

        ENDM
