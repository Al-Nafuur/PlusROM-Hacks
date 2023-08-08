
; ********************************************************************
;  Donkey Kong VCS
;
;    ROM Bank 4
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

; --------------------------------------------------------------------
;       Bank 4 Draw Floors
; --------------------------------------------------------------------

DrawFloors:     jmp     (Ptr)

SwitchFloors:   SUBROUTINE

                lda     (ST_EnemyGRPPtr+8*2),y
                sta     GRP0
                lda     (ST_EnemyCOLPtr+4*2),y
                sta     COLUP0

                cpy     #0
                bne     .switch
                jmp     B4_DrawFloor2

.switch         lda     #0                      ; before branching to shorten distance
                cpy     #23
                bcc     B4_Floor3_0
                cpy     #24
                bcc     B4_Floor3_1
                cpy     #25
                bcs     .skip
                sta     WSYNC
                sta     ST_HMPos+3
                bcc     B4_Floor3_2

.skip           tya
                sec
                sbc     #25
                tay

;
; Floor 3 ------------------------------------------------------------
;

B4_DrawFloor3:  SUBROUTINE

B4_Floor3_3:    ldx     ST_PFIdx+7

.loop0          lda     B4_GirderCOL,x
                sta     WSYNC
                sta     COLUPF

                lda     (ST_MarioGRPPtr+7*2),y
                sta     GRP1
                lda     (ST_MarioCOLPtr+7*2),y
                sta     COLUP1

                lda     B4_GirderPF1_L,x
                sta     PF1
                lda     B4_GirderPF2_L,x
                sta     PF2

                lda     (ST_EnemyGRPPtr+7*2),y
                sta     GRP0

                lda     B4_GirderPF2_R,x
                sta     PF2
                lda     B4_GirderPF1_R,x
                sta     PF1

                dex
                dey
                bpl     .loop0

                ; ----------------------------------------------------

B4_Floor3_2:    ldy     #23

                lda     (ST_MarioGRPPtr+6*2),y
                sta     GRP1
                lda     (ST_MarioCOLPtr+6*2),y
                sta     COLUP1

                lda     #0
                sta     COLUPF

                lda     ST_HMPos+3
                sta     HMP0
                beq     .skip
                ldx     ST_HMDelay+3
.resdelay       dex
                bpl     .resdelay
                sta     RESP0
.skip
                ; ----------------------------------------------------

B4_Floor3_1:    lda     (ST_EnemyGRPPtr+6*2),y
                sta     WSYNC
                sta     HMOVE
                sta     GRP0
                dey

                lda     (ST_MarioGRPPtr+6*2),y
                sta     GRP1
                lda     (ST_MarioCOLPtr+6*2),y
                sta     COLUP1

                lda     (ST_EnemyGRPPtr+6*2),y
                sta     GRP0

                lda     #0
                sta     PF1
                sta     PF2
                sta     HMCLR
                lda     ST_RefNuSiz+3
                sta     REFP0
                sta     NUSIZ0

                ; ----------------------------------------------------

B4_Floor3_0:    ldx     ST_PFIdx+6
                lda     ST_PFCol+3
                sta     COLUPF
                dey

.loop1          lda     (ST_EnemyCOLPtr+3*2),y
                sta     WSYNC
                sta     COLUP0

                lda     (ST_MarioGRPPtr+6*2),y
                sta     GRP1
                lda     (ST_MarioCOLPtr+6*2),y
                sta     COLUP1

                lda     B4_FloorPF1_L,x
                sta     PF1
                lda     B4_FloorPF2_L,x
                sta     PF2

                lda     (ST_EnemyGRPPtr+6*2),y
                sta     GRP0

                lda     B4_FloorPF2_R,x
                sta     PF2
                lda     B4_FloorPF1_R,x
                sta     PF1

                dex
                dey
                bpl     .loop1

;
; Floor 2 ------------------------------------------------------------
;

B4_DrawFloor2:  SUBROUTINE

B4_Floor2_3:    ldx     ST_PFIdx+5
                ldy     #7

.loop0          lda     B4_GirderCOL,x
                sta     WSYNC
                sta     COLUPF

                lda     (ST_MarioGRPPtr+5*2),y
                sta     GRP1
                lda     (ST_MarioCOLPtr+5*2),y
                sta     COLUP1

                lda     B4_GirderPF1_L,x
                sta     PF1
                lda     B4_GirderPF2_L,x
                sta     PF2

                lda     (ST_EnemyGRPPtr+5*2),y
                sta     GRP0

                lda     B4_GirderPF2_R,x
                sta     PF2
                lda     B4_GirderPF1_R,x
                sta     PF1

Floor2_3_Cnt:   dex
                dey
                bpl     .loop0

                ; ----------------------------------------------------

B4_Floor2_2:    ldy     #23

                lda     (ST_MarioGRPPtr+4*2),y
                sta     GRP1
                lda     (ST_MarioCOLPtr+4*2),y
                sta     COLUP1

                lda     #0
                sta     COLUPF

                lda     ST_HMPos+2
                sta     HMP0
                beq     .skip
                ldx     ST_HMDelay+2
.resdelay       dex
                bpl     .resdelay
                sta     RESP0

.skip           lda     (ST_EnemyGRPPtr+4*2),y

                ; ----------------------------------------------------

B4_Floor2_1:    sta     WSYNC
                sta     HMOVE

                sta     GRP0
                dey

                lda     (ST_MarioGRPPtr+4*2),y
                sta     GRP1
                lda     (ST_MarioCOLPtr+4*2),y
                sta     COLUP1

                lda     (ST_EnemyGRPPtr+4*2),y
                sta     GRP0

                lda     #0
                sta     PF1
                sta     PF2
                sta     HMCLR
                lda     ST_RefNuSiz+2
                sta     REFP0
                sta     NUSIZ0

                ; ----------------------------------------------------

B4_Floor2_0:    ldx     ST_PFIdx+4
                lda     ST_PFCol+2
                sta     COLUPF
                dey

.loop1          lda     (ST_EnemyCOLPtr+2*2),y
                sta     WSYNC
                sta     COLUP0

                lda     (ST_MarioGRPPtr+4*2),y
                sta     GRP1
                lda     (ST_MarioCOLPtr+4*2),y
                sta     COLUP1

                lda     B4_FloorPF1_L,x
                sta     PF1
                lda     B4_FloorPF2_L,x
                sta     PF2

                lda     (ST_EnemyGRPPtr+4*2),y
                sta     GRP0

                lda     B4_FloorPF2_R,x
                sta     PF2
                lda     B4_FloorPF1_R,x
                sta     PF1

                dex
                dey
                bpl     .loop1

;
; Floor 1 ------------------------------------------------------------
;

B4_DrawFloor1:  SUBROUTINE

B4_Floor1_3:    ldx     ST_PFIdx+3
                ldy     #7

.loop0          lda     B4_GirderCOL,x
                sta     WSYNC
                sta     COLUPF

                lda     (ST_MarioGRPPtr+3*2),y
                sta     GRP1
                lda     (ST_MarioCOLPtr+3*2),y
                sta     COLUP1

                lda     B4_GirderPF1_L,x
                sta     PF1
                lda     B4_GirderPF2_L,x
                sta     PF2

                lda     (ST_EnemyGRPPtr+3*2),y
                sta     GRP0

                lda     B4_GirderPF2_R,x
                sta     PF2
                lda     B4_GirderPF1_R,x
                sta     PF1

Floor1_3_Cnt:   dex
                dey
                bpl     .loop0

                ; ----------------------------------------------------

B4_Floor1_2:    ldy     #23

                lda     (ST_MarioGRPPtr+2*2),y
                sta     GRP1
                lda     (ST_MarioCOLPtr+2*2),y
                sta     COLUP1

                lda     #0
                sta     COLUPF

                lda     ST_HMPos+1
                sta     HMP0
                beq     .skip
                ldx     ST_HMDelay+1
.resdelay       dex
                bpl     .resdelay
                sta     RESP0

.skip           lda     (ST_EnemyGRPPtr+2*2),y

                ; ----------------------------------------------------

B4_Floor1_1:    sta     WSYNC
                sta     HMOVE

                sta     GRP0
                dey

                lda     (ST_MarioGRPPtr+2*2),y
                sta     GRP1
                lda     (ST_MarioCOLPtr+2*2),y
                sta     COLUP1

                lda     (ST_EnemyGRPPtr+2*2),y
                sta     GRP0

                lda     #0
                sta     PF1
                sta     PF2
                sta     HMCLR
                lda     ST_RefNuSiz+1
                sta     REFP0
                sta     NUSIZ0

                ; ----------------------------------------------------

B4_Floor1_0:    ldx     ST_PFIdx+2
                lda     ST_PFCol+1
                sta     COLUPF
                dey

.loop1          lda     (ST_EnemyCOLPtr+1*2),y
                sta     COLUP0
                asl
                and     ST_M0Mask
                sta     ENAM0

                lda     (ST_MarioGRPPtr+2*2),y
                sta     GRP1
                lda     (ST_MarioCOLPtr+2*2),y
                sta     COLUP1

                lda     B4_FloorPF1_L,x
                sta     PF1
                lda     B4_FloorPF2_L,x
                sta     PF2

                lda     (ST_EnemyGRPPtr+2*2),y
                sta     GRP0

                lda     B4_FloorPF2_R,x
                sta     PF2
                lda     B4_FloorPF1_R,x
                sta     PF1

                dex
                dey
                bpl     .loop1

;
; Floor 0 ------------------------------------------------------------
;

B4_DrawFloor0:  SUBROUTINE

B4_Floor0_3:    ldx     ST_PFIdx+1
                ldy     #7

.loop0          lda     B4_GirderCOL,x
                sta     WSYNC
                sta     COLUPF

                lda     (ST_MarioGRPPtr+1*2),y
                sta     GRP1
                lda     (ST_MarioCOLPtr+1*2),y
                sta     COLUP1

                lda     B4_GirderPF1_L,x
                sta     PF1
                lda     B4_GirderPF2_L,x
                sta     PF2

                lda     (ST_EnemyGRPPtr+1*2),y
                sta     GRP0

                lda     B4_GirderPF2_R,x
                sta     PF2
                lda     B4_GirderPF1_R,x
                sta     PF1

                dex
                dey
                bpl     .loop0

                ; ----------------------------------------------------

B4_Floor0_2:    ldy     #23

                lda     (ST_MarioGRPPtr+0*2),y
                sta     GRP1
                lda     (ST_MarioCOLPtr+0*2),y
                sta     COLUP1

                lda     #0
                sta     COLUPF

                lda     ST_HMPos+0
                sta     HMP0
                beq     .skip
                ldx     ST_HMDelay+0
.resdelay       dex
                bpl     .resdelay
                sta     RESP0

.skip           ldx     ST_ScrollCtr1
                cpx     #23
                bcs     B4_SkipGround

                lda     (ST_EnemyGRPPtr+0*2),y

                ; ----------------------------------------------------

B4_Floor0_1:    sta     WSYNC
                sta     HMOVE

                sta     GRP0
                dey

                lda     (ST_MarioGRPPtr+0*2),y
                sta     GRP1
                lda     (ST_MarioCOLPtr+0*2),y
                sta     COLUP1

                lda     (ST_EnemyGRPPtr+0*2),y
                sta     GRP0

                lda     #0
                sta     PF1
                sta     HMCLR
                lda     ST_RefNuSiz+0
                sta     REFP0
                sta     NUSIZ0

                ldx     ST_ScrollCtr1
                cpx     #22
                bcs     B4_SkipGround

                ; ----------------------------------------------------

B4_Floor0_0:    ldx     ST_PFIdx+0
                lda     ST_PFCol+0
                sta     COLUPF
                dey

.loop1          lda     (ST_EnemyCOLPtr+0*2),y
                sta     WSYNC
                sta     COLUP0

                lda     (ST_MarioGRPPtr+0*2),y
                sta     GRP1
                lda     (ST_MarioCOLPtr+0*2),y
                sta     COLUP1

                lda     B4_FloorPF1_L,x
                sta     PF1
                lda     B4_FloorPF2_L,x
                sta     PF2

                lda     (ST_EnemyGRPPtr+0*2),y
                sta     GRP0

                lda     B4_FloorPF2_R,x
                sta     PF2
                lda     B4_FloorPF1_R,x
                sta     PF1

                dex
                dey

                cpy     ST_ScrollCtr1
                bpl     .loop1

;
; Ground -------------------------------------------------------------
;

B4_DrawGround:  SUBROUTINE
                ldx     ST_PFIdx+7              ; reuse from floor 3 girder
                ldy     #7
B4_SkipGround:
.loop           sta     WSYNC
                lda     #0
                sta     GRP1
                lda     ST_ScrollCtr0
                cmp     #8
                bcs     .blank

                lda     B4_GirderCOL,x
                sta     COLUPF

                lda     B4_GirderPF1_L,x
                sta     PF1
                lda     B4_GirderPF2_L,x
                sta     PF2

                lda     (ST_EnemyGRPPtr+7*2),y
                sta     GRP0

                lda     B4_GirderPF2_R,x
                sta     PF2
                lda     B4_GirderPF1_R,x
                sta     PF1

                dex
                dey
                cpy     ST_ScrollCtr0
                bpl     .loop

                sta     WSYNC
.blank          lda     #%00000010
                sta     VBLANK

                jmp     B4_ReadJoystick

;
; Top ----------------------------------------------------------------
;

DrawTop:        SUBROUTINE

                ; second girder line -- set coarse Mario position

                ldx     ST_PFIdx+5
                dex

                lda     B4_GirderPF1_L,x
                sta     PF1
                lda     B4_GirderPF2_L,x
                sta     PF2

                lda     B4_GirderCOL,x
                sta     COLUPF
                ldy     ST_HMDelay+4
.loop           dey
                bpl     .loop
                sta     RESP1

                ; third girder line -- set fine Mario position

                lda     ST_HMPos+4
                sta     HMP1
                sta     WSYNC
                sta     HMOVE
                dex
                lda     B4_GirderCOL,x
                sta     COLUPF

                lda     B4_GirderPF1_L,x
                sta     PF1
                lda     B4_GirderPF2_L,x
                sta     PF2

                lda     MarioDir_R
                sta     REFP1
                sta     VDELP1
                ldy     #5
                sta     HMCLR

                lda     B4_GirderPF2_R,x
                sta     PF2
                lda     B4_GirderPF1_R,x
                sta     PF1

                ; continue with girder in floor 1 or 2

                jmp     (ST_PtrTmp)

; --------------------------------------------------------------------
;       B4 Data Fetch Functions
; --------------------------------------------------------------------

                ; fetch data for first girder line, used in another bank

GetTopGirder:   ldx     ST_PFIdx+5
                lda     B4_GirderCOL,x
                sta     ST_COLTmp
                lda     B4_GirderPF1_L,x
                sta     ST_PF1Tmp
                lda     B4_GirderPF2_L,x
                sta     ST_PF2Tmp
                rts


; ********************************************************************
;
;       Data Section
;
; ********************************************************************

; --------------------------------------------------------------------
;       B4 Graphics
; --------------------------------------------------------------------

        MAC GRP_PADDING

                DS.B    24, 0

        ENDM

;
; Enemy GRP (part 1)
;
                ; Address of EmptyGRP should be the same in all display banks.
                ; DASM will complain if not.

                DS.B    2, 0

                GRP_PADDING
EmptyGRP:       GRP_PADDING

; fireball -----------------------------------------------------------

B4_FIREBALL_0_HEIGHT = B4_Fireball0_GRP_0_End - B4_Fireball0_GRP_0

Fireball0_GRP:
B4_Fireball0_GRP_0:

                BYTE    %00000000 ; |        |  14
                BYTE    %00111100 ; |  XXXX  |  13
                BYTE    %01111110 ; | XXXXXX |  12
                BYTE    %01010110 ; | X X XX |  11
                BYTE    %01010110 ; | X X XX |  10
                BYTE    %01010110 ; | X X XX |   9
                BYTE    %01111110 ; | XXXXXX |   8
                BYTE    %00111100 ; |  XXXX  |   7
                BYTE    %00010100 ; |   X X  |   6
                BYTE    %00000000 ; |        |   5
                BYTE    %01000000 ; | X      |   4
                BYTE    %00000010 ; |      X |   3
                BYTE    %00000100 ; |     X  |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

B4_Fireball0_GRP_0_End:

                GRP_PADDING

B4_Fireball0_GRP_1:

                BYTE    %00111100 ; |  XXXX  |  14
                BYTE    %01000010 ; | X    X |  13
                BYTE    %10111001 ; |X XXX  X|  12
                BYTE    %11111101 ; |XXXXXX X|  11
                BYTE    %11111101 ; |XXXXXX X|  10
                BYTE    %11111101 ; |XXXXXX X|   9
                BYTE    %10111001 ; |X XXX  X|   8
                BYTE    %01000010 ; | X    X |   7
                BYTE    %00101000 ; |  X X   |   6
                BYTE    %00011110 ; |   XXXX |   5
                BYTE    %00000101 ; |     X X|   4
                BYTE    %00001000 ; |    X   |   3
                BYTE    %00010000 ; |   X    |   2
                BYTE    %00001001 ; |    X  X|   1
                BYTE    %00000000 ; |        |   0

                GRP_PADDING

; umbrella -----------------------------------------------------------

B4_UMBRELLA_HEIGHT = B4_Umbrella_GRP_1 - B4_Umbrella_GRP_0

B4_Umbrella_GRP_0:

                BYTE    %00001000 ; |    X   |  14
                BYTE    %00010100 ; |   X X  |  13
                BYTE    %00010000 ; |   X    |  12
                BYTE    %00010000 ; |   X    |  11
                BYTE    %00000000 ; |        |  10
                BYTE    %00000000 ; |        |   9
                BYTE    %10000000 ; |X       |   8
                BYTE    %10101010 ; |X X X X |   7
                BYTE    %10111010 ; |X XXX X |   6
                BYTE    %10010010 ; |X  X  X |   5
                BYTE    %10010010 ; |X  X  X |   4
                BYTE    %01010100 ; | X X X  |   3
                BYTE    %01010100 ; | X X X  |   2
                BYTE    %00111000 ; |  XXX   |   1
                BYTE    %00010000 ; |   X    |   0

                DS.B    9, 0

B4_Umbrella_GRP_1:

                BYTE    %00000000 ; |        |  14
                BYTE    %00000000 ; |        |  13
                BYTE    %00000000 ; |        |  12
                BYTE    %00000000 ; |        |  11
                BYTE    %00010000 ; |   X    |  10
                BYTE    %00010000 ; |   X    |   9
                BYTE    %00010010 ; |   X  X |   8
                BYTE    %01010100 ; | X X X  |   7
                BYTE    %01000100 ; | X   X  |   6
                BYTE    %01101100 ; | XX XX  |   5
                BYTE    %01101100 ; | XX XX  |   4
                BYTE    %00101000 ; |  X X   |   3
                BYTE    %00101000 ; |  X X   |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

                DS.B    9, 0

;
; Playfield
;
                ALIGN   $0100

B4_GirderPF1_L: ;  0    0
                BYTE    %00111111 ; |  XXXXXX|  7
                BYTE    %00111111 ; |  XXXXXX|  6
                BYTE    %00101010 ; |  X X X |  5
                BYTE    %00101010 ; |  X X X |  4
                BYTE    %00101010 ; |  X X X |  3
                BYTE    %00101010 ; |  X X X |  2
                BYTE    %00111111 ; |  XXXXXX|  1
                BYTE    %00111111 ; |  XXXXXX|  0
                ;  1    1_00
                BYTE    %00011111 ; |   XXXXX|  7
                BYTE    %00011111 ; |   XXXXX|  6
                BYTE    %00001010 ; |    X X |  5
                BYTE    %00001010 ; |    X X |  4
                BYTE    %00001010 ; |    X X |  3
                BYTE    %00001010 ; |    X X |  2
                BYTE    %00011111 ; |   XXXXX|  1
                BYTE    %00011111 ; |   XXXXX|  0
                ;  2    1_01
                BYTE    %00011111 ; |   XXXXX|  7
                BYTE    %00011111 ; |   XXXXX|  6
                BYTE    %00001010 ; |    X X |  5
                BYTE    %00001010 ; |    X X |  4
                BYTE    %00001010 ; |    X X |  3
                BYTE    %00001010 ; |    X X |  2
                BYTE    %00011111 ; |   XXXXX|  1
                BYTE    %00011111 ; |   XXXXX|  0
                ;  3    1_10
                BYTE    %00011111 ; |   XXXXX|  7
                BYTE    %00011111 ; |   XXXXX|  6
                BYTE    %00001010 ; |    X X |  5
                BYTE    %00001010 ; |    X X |  4
                BYTE    %00001010 ; |    X X |  3
                BYTE    %00001010 ; |    X X |  2
                BYTE    %00011111 ; |   XXXXX|  1
                BYTE    %00011111 ; |   XXXXX|  0
                ;  4    1_11
                BYTE    %00011111 ; |   XXXXX|  7
                BYTE    %00011111 ; |   XXXXX|  6
                BYTE    %00001010 ; |    X X |  5
                BYTE    %00001010 ; |    X X |  4
                BYTE    %00001010 ; |    X X |  3
                BYTE    %00001010 ; |    X X |  2
                BYTE    %00011111 ; |   XXXXX|  1
                BYTE    %00011111 ; |   XXXXX|  0
                ;  5    2_00
                BYTE    %00001111 ; |    XXXX|  7
                BYTE    %00001111 ; |    XXXX|  6
                BYTE    %00001010 ; |    X X |  5
                BYTE    %00001010 ; |    X X |  4
                BYTE    %00001010 ; |    X X |  3
                BYTE    %00001010 ; |    X X |  2
                BYTE    %00001111 ; |    XXXX|  1
                BYTE    %00001111 ; |    XXXX|  0
                ;  6    2_01
                BYTE    %00001111 ; |    XXXX|  7
                BYTE    %00001111 ; |    XXXX|  6
                BYTE    %00001010 ; |    X X |  5
                BYTE    %00001010 ; |    X X |  4
                BYTE    %00001010 ; |    X X |  3
                BYTE    %00001010 ; |    X X |  2
                BYTE    %00001111 ; |    XXXX|  1
                BYTE    %00001111 ; |    XXXX|  0
                ;  7    2_10
                BYTE    %00001111 ; |    XXXX|  7
                BYTE    %00001111 ; |    XXXX|  6
                BYTE    %00001010 ; |    X X |  5
                BYTE    %00001010 ; |    X X |  4
                BYTE    %00001010 ; |    X X |  3
                BYTE    %00001010 ; |    X X |  2
                BYTE    %00001111 ; |    XXXX|  1
                BYTE    %00001111 ; |    XXXX|  0
                ;  8    2_11
                BYTE    %00001111 ; |    XXXX|  7
                BYTE    %00001111 ; |    XXXX|  6
                BYTE    %00001010 ; |    X X |  5
                BYTE    %00001010 ; |    X X |  4
                BYTE    %00001010 ; |    X X |  3
                BYTE    %00001010 ; |    X X |  2
                BYTE    %00001111 ; |    XXXX|  1
                BYTE    %00001111 ; |    XXXX|  0
                ;  9    3_00
                BYTE    %00000111 ; |     XXX|  7
                BYTE    %00000111 ; |     XXX|  6
                BYTE    %00000010 ; |      X |  5
                BYTE    %00000010 ; |      X |  4
                BYTE    %00000010 ; |      X |  3
                BYTE    %00000010 ; |      X |  2
                BYTE    %00000111 ; |     XXX|  1
                BYTE    %00000111 ; |     XXX|  0
                ; 10    3_01
                BYTE    %00000111 ; |     XXX|  7
                BYTE    %00000111 ; |     XXX|  6
                BYTE    %00000010 ; |      X |  5
                BYTE    %00000010 ; |      X |  4
                BYTE    %00000010 ; |      X |  3
                BYTE    %00000010 ; |      X |  2
                BYTE    %00000111 ; |     XXX|  1
                BYTE    %00000111 ; |     XXX|  0
                ; 11    3_10
                BYTE    %00000111 ; |     XXX|  7
                BYTE    %00000111 ; |     XXX|  6
                BYTE    %00000010 ; |      X |  5
                BYTE    %00000010 ; |      X |  4
                BYTE    %00000010 ; |      X |  3
                BYTE    %00000010 ; |      X |  2
                BYTE    %00000111 ; |     XXX|  1
                BYTE    %00000111 ; |     XXX|  0
                ; 12    3_11
                BYTE    %00000111 ; |     XXX|  7
                BYTE    %00000111 ; |     XXX|  6
                BYTE    %00000010 ; |      X |  5
                BYTE    %00000010 ; |      X |  4
                BYTE    %00000010 ; |      X |  3
                BYTE    %00000010 ; |      X |  2
                BYTE    %00000111 ; |     XXX|  1
                BYTE    %00000111 ; |     XXX|  0
                ; 13    4_00
                BYTE    %00000011 ; |      XX|  7
                BYTE    %00000011 ; |      XX|  6
                BYTE    %00000010 ; |      X |  5
                BYTE    %00000010 ; |      X |  4
                BYTE    %00000010 ; |      X |  3
                BYTE    %00000010 ; |      X |  2
                BYTE    %00000011 ; |      XX|  1
                BYTE    %00000011 ; |      XX|  0
                ; 14    4_01
                BYTE    %00000011 ; |      XX|  7
                BYTE    %00000011 ; |      XX|  6
                BYTE    %00000010 ; |      X |  5
                BYTE    %00000010 ; |      X |  4
                BYTE    %00000010 ; |      X |  3
                BYTE    %00000010 ; |      X |  2
                BYTE    %00000011 ; |      XX|  1
                BYTE    %00000011 ; |      XX|  0
                ; 15    4_10
                BYTE    %00000011 ; |      XX|  7
                BYTE    %00000011 ; |      XX|  6
                BYTE    %00000010 ; |      X |  5
                BYTE    %00000010 ; |      X |  4
                BYTE    %00000010 ; |      X |  3
                BYTE    %00000010 ; |      X |  2
                BYTE    %00000011 ; |      XX|  1
                BYTE    %00000011 ; |      XX|  0
                ; 16    4_11
                BYTE    %00000011 ; |      XX|  7
                BYTE    %00000011 ; |      XX|  6
                BYTE    %00000010 ; |      X |  5
                BYTE    %00000010 ; |      X |  4
                BYTE    %00000010 ; |      X |  3
                BYTE    %00000010 ; |      X |  2
                BYTE    %00000011 ; |      XX|  1
                BYTE    %00000011 ; |      XX|  0
                ; 17    5
                BYTE    %00000000 ; |        |  7
                BYTE    %00000000 ; |        |  6
                BYTE    %00000000 ; |        |  5
                BYTE    %00000000 ; |        |  4
                BYTE    %00000000 ; |        |  3
                BYTE    %00000000 ; |        |  2
                BYTE    %00000000 ; |        |  1
                BYTE    %00000000 ; |        |  0
                ; 18    6
                BYTE    %00111111 ; |  XXXXXX|  7
                BYTE    %00111111 ; |  XXXXXX|  6
                BYTE    %00100010 ; |  X   X |  5
                BYTE    %00110111 ; |  XX XXX|  4
                BYTE    %00011101 ; |   XXX X|  3
                BYTE    %00001000 ; |    X   |  2
                BYTE    %00111111 ; |  XXXXXX|  1
                BYTE    %00111111 ; |  XXXXXX|  0
                ; 19    7
                BYTE    %00011111 ; |   XXXXX|  7
                BYTE    %00111111 ; |  XXXXXX|  6
                BYTE    %00001010 ; |    X X |  5
                BYTE    %00101010 ; |  X X X |  4
                BYTE    %00101010 ; |  X X X |  3
                BYTE    %00001010 ; |    X X |  2
                BYTE    %00111111 ; |  XXXXXX|  1
                BYTE    %00011111 ; |   XXXXX|  0
                ; 20    8
                BYTE    %00011111 ; |   XXXXX|  7
                BYTE    %00011111 ; |   XXXXX|  6
                BYTE    %00000010 ; |      X |  5
                BYTE    %00010111 ; |   X XXX|  4
                BYTE    %00011101 ; |   XXX X|  3
                BYTE    %00001000 ; |    X   |  2
                BYTE    %00011111 ; |   XXXXX|  1
                BYTE    %00011111 ; |   XXXXX|  0
                ; 21    9
                BYTE    %00111111 ; |  XXXXXX|  7
                BYTE    %00111111 ; |  XXXXXX|  6
                BYTE    %00101010 ; |  X X X |  5
                BYTE    %00101010 ; |  X X X |  4
                BYTE    %00101010 ; |  X X X |  3
                BYTE    %00101010 ; |  X X X |  2
                BYTE    %00111111 ; |  XXXXXX|  1
                BYTE    %00111111 ; |  XXXXXX|  0

; blaze ---------------------------------------------------------------

                ; blaze 0

                DS.B    1, 0

B4_BLAZE_0_HEIGHT = B4_Blaze0_GRP_0_End - B4_Blaze0_GRP_0

B4_Blaze0_GRP_0:

                BYTE    %00111100 ; |  XXXX  |  15
                BYTE    %01011110 ; | X XXXX |  14
                BYTE    %00011100 ; |   XXX  |  13
                BYTE    %00111100 ; |  XXXX  |  12
                BYTE    %00111110 ; |  XXXXX |  11
                BYTE    %00111110 ; |  XXXXX |  10
                BYTE    %01111101 ; | XXXXX X|   9
                BYTE    %00111100 ; |  XXXX  |   8
                BYTE    %00011100 ; |   XXX  |   7
                BYTE    %00101100 ; |  X XX  |   6
                BYTE    %00001010 ; |    X X |   5
                BYTE    %00101010 ; |  X X X |   4
                BYTE    %10000001 ; |X      X|   3
                BYTE    %00001000 ; |    X   |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000001 ; |       X|   0

B4_Blaze0_GRP_0_End:

                DS.B    8, 0

B4_Blaze0_GRP_1:

                BYTE    %01100110 ; | XX  XX |  15
                BYTE    %00010100 ; |   X X  |  14
                BYTE    %11110111 ; |XXXX XXX|  13
                BYTE    %11010111 ; |XX X XXX|  12
                BYTE    %10011101 ; |X  XXX X|  11
                BYTE    %01101000 ; | XX X   |  10
                BYTE    %10001010 ; |X   X X |   9
                BYTE    %10010010 ; |X  X  X |   8
                BYTE    %01100010 ; | XX   X |   7
                BYTE    %01010001 ; | X X   X|   6
                BYTE    %01110100 ; | XXX X  |   5
                BYTE    %01010101 ; | X X X X|   4
                BYTE    %10101110 ; |X X XXX |   3
                BYTE    %00100010 ; |  X   X |   2
                BYTE    %01001010 ; | X  X X |   1
                BYTE    %00010001 ; |   X   X|   0

                DS.B    8, 0

; ---------------------------------------------------------------------

B4_ReadJoystick:SUBROUTINE

                ; set shadow Registers

                ldx     SWCHA
                lda     INPT4
                and     INPT5                   ; use either button
                stx     ST_ShadowSWCHA
                sta     ST_ShadowINPT

                ; finish last line

                sta     WSYNC
                rts

; ---------------------------------------------------------------------

                ALIGN   $0100

B4_GirderPF2_L: ;  0    0
                BYTE    %11111111 ; |XXXXXXXX|  7
                BYTE    %11111111 ; |XXXXXXXX|  6
                BYTE    %01010101 ; | X X X X|  5
                BYTE    %01010101 ; | X X X X|  4
                BYTE    %01010101 ; | X X X X|  3
                BYTE    %01010101 ; | X X X X|  2
                BYTE    %11111111 ; |XXXXXXXX|  1
                BYTE    %11111111 ; |XXXXXXXX|  0
                ;  1    1_00
                BYTE    %11111101 ; |XXXXXX X|  7
                BYTE    %11111101 ; |XXXXXX X|  6
                BYTE    %01010101 ; | X X X X|  5
                BYTE    %01010111 ; | X X XXX|  4
                BYTE    %01010111 ; | X X XXX|  3
                BYTE    %01010111 ; | X X XXX|  2
                BYTE    %11111101 ; |XXXXXX X|  1
                BYTE    %11111111 ; |XXXXXXXX|  0
                ;  2    1_01
                BYTE    %11111101 ; |XXXXXX X|  7
                BYTE    %11111101 ; |XXXXXX X|  6
                BYTE    %01010101 ; | X X X X|  5
                BYTE    %01010111 ; | X X XXX|  4
                BYTE    %01010111 ; | X X XXX|  3
                BYTE    %01010111 ; | X X XXX|  2
                BYTE    %11111101 ; |XXXXXX X|  1
                BYTE    %11111111 ; |XXXXXXXX|  0
                ;  3    1_10
                BYTE    %11111101 ; |XXXXXX X|  7
                BYTE    %11111101 ; |XXXXXX X|  6
                BYTE    %01010101 ; | X X X X|  5
                BYTE    %01010101 ; | X X X X|  4
                BYTE    %01010101 ; | X X X X|  3
                BYTE    %01010101 ; | X X X X|  2
                BYTE    %11111101 ; |XXXXXX X|  1
                BYTE    %11111101 ; |XXXXXX X|  0
                ;  4    1_11
                BYTE    %11111101 ; |XXXXXX X|  7
                BYTE    %11111101 ; |XXXXXX X|  6
                BYTE    %01010101 ; | X X X X|  5
                BYTE    %01010101 ; | X X X X|  4
                BYTE    %01010101 ; | X X X X|  3
                BYTE    %01010101 ; | X X X X|  2
                BYTE    %11111101 ; |XXXXXX X|  1
                BYTE    %11111101 ; |XXXXXXXX|  0
                ;  5    2_00
                BYTE    %11111101 ; |XXXXXX X|  7
                BYTE    %11111101 ; |XXXXXX X|  6
                BYTE    %01010101 ; | X X X X|  5
                BYTE    %01010111 ; | X X XXX|  4
                BYTE    %01010111 ; | X X XXX|  3
                BYTE    %01010111 ; | X X XXX|  2
                BYTE    %11111101 ; |XXXXXX X|  1
                BYTE    %11111111 ; |XXXXXXXX|  0
                ;  6    2_01
                BYTE    %11111101 ; |XXXXXX X|  7
                BYTE    %11111101 ; |XXXXXX X|  6
                BYTE    %01010101 ; | X X X X|  5
                BYTE    %01010111 ; | X X XXX|  4
                BYTE    %01010111 ; | X X XXX|  3
                BYTE    %01010111 ; | X X XXX|  2
                BYTE    %11111101 ; |XXXXXX X|  1
                BYTE    %11111111 ; |XXXXXXXX|  0
                ;  7    2_10
                BYTE    %11111101 ; |XXXXXX X|  7
                BYTE    %11111101 ; |XXXXXX X|  6
                BYTE    %01010101 ; | X X X X|  5
                BYTE    %01010101 ; | X X X X|  4
                BYTE    %01010101 ; | X X X X|  3
                BYTE    %01010101 ; | X X X X|  2
                BYTE    %11111101 ; |XXXXXX X|  1
                BYTE    %11111101 ; |XXXXXX X|  0
                ;  8    2_11
                BYTE    %11111101 ; |XXXXXX X|  7
                BYTE    %11111101 ; |XXXXXX X|  6
                BYTE    %01010101 ; | X X X X|  5
                BYTE    %01010101 ; | X X X X|  4
                BYTE    %01010101 ; | X X X X|  3
                BYTE    %01010101 ; | X X X X|  2
                BYTE    %11111101 ; |XXXXXX X|  1
                BYTE    %11111101 ; |XXXXXX X|  0
                ;  9    3_00
                BYTE    %11111101 ; |XXXXXX X|  7
                BYTE    %11111101 ; |XXXXXX X|  6
                BYTE    %01010101 ; | X X X X|  5
                BYTE    %01010111 ; | X X XXX|  4
                BYTE    %01010111 ; | X X XXX|  3
                BYTE    %01010111 ; | X X XXX|  2
                BYTE    %11111101 ; |XXXXXX X|  1
                BYTE    %11111111 ; |XXXXXXXX|  0
                ; 10    3_01
                BYTE    %11111101 ; |XXXXXX X|  7
                BYTE    %11111101 ; |XXXXXX X|  6
                BYTE    %01010101 ; | X X X X|  5
                BYTE    %01010111 ; | X X XXX|  4
                BYTE    %01010111 ; | X X XXX|  3
                BYTE    %01010111 ; | X X XXX|  2
                BYTE    %11111101 ; |XXXXXX X|  1
                BYTE    %11111111 ; |XXXXXXXX|  0
                ; 11    3_10
                BYTE    %11111101 ; |XXXXXX X|  7
                BYTE    %11111101 ; |XXXXXX X|  6
                BYTE    %01010101 ; | X X X X|  5
                BYTE    %01010101 ; | X X X X|  4
                BYTE    %01010101 ; | X X X X|  3
                BYTE    %01010101 ; | X X X X|  2
                BYTE    %11111101 ; |XXXXXX X|  1
                BYTE    %11111101 ; |XXXXXX X|  0
                ; 12    3_11
                BYTE    %11111101 ; |XXXXXX X|  7
                BYTE    %11111101 ; |XXXXXX X|  6
                BYTE    %01010101 ; | X X X X|  5
                BYTE    %01010101 ; | X X X X|  4
                BYTE    %01010101 ; | X X X X|  3
                BYTE    %01010101 ; | X X X X|  2
                BYTE    %11111101 ; |XXXXXX X|  1
                BYTE    %11111101 ; |XXXXXX X|  0
                ; 13    4_00
                BYTE    %11111101 ; |XXXXXX X|  7
                BYTE    %11111101 ; |XXXXXX X|  6
                BYTE    %01010101 ; | X X X X|  5
                BYTE    %01010111 ; | X X XXX|  4
                BYTE    %01010111 ; | X X XXX|  3
                BYTE    %01010111 ; | X X XXX|  2
                BYTE    %11111101 ; |XXXXXX X|  1
                BYTE    %11111111 ; |XXXXXXXX|  0
                ; 14    4_01
                BYTE    %11111101 ; |XXXXXX X|  7
                BYTE    %11111101 ; |XXXXXX X|  6
                BYTE    %01010101 ; | X X X X|  5
                BYTE    %01010111 ; | X X XXX|  4
                BYTE    %01010111 ; | X X XXX|  3
                BYTE    %01010111 ; | X X XXX|  2
                BYTE    %11111101 ; |XXXXXX X|  1
                BYTE    %11111111 ; |XXXXXXXX|  0
                ; 15    4_10
                BYTE    %11111101 ; |XXXXXX X|  7
                BYTE    %11111101 ; |XXXXXX X|  6
                BYTE    %01010101 ; | X X X X|  5
                BYTE    %01010101 ; | X X X X|  4
                BYTE    %01010101 ; | X X X X|  3
                BYTE    %01010101 ; | X X X X|  2
                BYTE    %11111101 ; |XXXXXX X|  1
                BYTE    %11111101 ; |XXXXXX X|  0
                ; 16    4_11
                BYTE    %11111101 ; |XXXXXX X|  7
                BYTE    %11111101 ; |XXXXXX X|  6
                BYTE    %01010101 ; | X X X X|  5
                BYTE    %01010101 ; | X X X X|  4
                BYTE    %01010101 ; | X X X X|  3
                BYTE    %01010101 ; | X X X X|  2
                BYTE    %11111101 ; |XXXXXX X|  1
                BYTE    %11111101 ; |XXXXXX X|  0
                ; 17    5
                BYTE    %11111110 ; |XXXXXXX |  7
                BYTE    %11111110 ; |XXXXXXX |  6
                BYTE    %01010100 ; | X X X  |  5
                BYTE    %01010100 ; | X X X  |  4
                BYTE    %01010100 ; | X X X  |  3
                BYTE    %01010100 ; | X X X  |  2
                BYTE    %11111110 ; |XXXXXXX |  1
                BYTE    %11111110 ; |XXXXXXX |  0
                ; 18    6
                BYTE    %11111111 ; |XXXXXXXX|  7
                BYTE    %11111111 ; |XXXXXXXX|  6
                BYTE    %01000100 ; | X   X  |  5
                BYTE    %11101110 ; |XXX XXX |  4
                BYTE    %10111011 ; |X XXX XX|  3
                BYTE    %00010001 ; |   X   X|  2
                BYTE    %11111111 ; |XXXXXXXX|  1
                BYTE    %11111111 ; |XXXXXXXX|  0
                ; 19    7
                BYTE    %11111111 ; |XXXXXXXX|  7
                BYTE    %11111111 ; |XXXXXXXX|  6
                BYTE    %01010101 ; | X X X X|  5
                BYTE    %01010101 ; | X X X X|  4
                BYTE    %01010101 ; | X X X X|  3
                BYTE    %01010101 ; | X X X X|  2
                BYTE    %11111111 ; |XXXXXXXX|  1
                BYTE    %11111111 ; |XXXXXXXX|  0
                ; 20    8
                BYTE    %11111100 ; |XXXXXX  |  7
                BYTE    %11111100 ; |XXXXXX  |  6
                BYTE    %01000100 ; | X   X  |  5
                BYTE    %11101100 ; |XXX XX  |  4
                BYTE    %10111000 ; |X XXX   |  3
                BYTE    %00010000 ; |   X    |  2
                BYTE    %11111100 ; |XXXXXX  |  1
                BYTE    %11111100 ; |XXXXXX  |  0
                ; 21    9
                BYTE    %10111111 ; |X XXXXXX|  7
                BYTE    %11111111 ; |XXXXXXXX|  6
                BYTE    %10010101 ; |X  X X X|  5
                BYTE    %11010101 ; |XX X X X|  4
                BYTE    %11010101 ; |XX X X X|  3
                BYTE    %10010101 ; |X  X X X|  2
                BYTE    %11111111 ; |XXXXXXXX|  1
                BYTE    %10111111 ; |X XXXXXX|  0

; blaze ---------------------------------------------------------------

                ; blaze 1

                DS.B    1, 0

B4_BLAZE_1_HEIGHT = B4_Blaze1_GRP_0_End - B4_Blaze1_GRP_0

B4_Blaze1_GRP_0:

                BYTE    %00111110 ; |  XXXXX |  15
                BYTE    %01011110 ; | X XXXX |  14
                BYTE    %00111110 ; |  XXXXX |  13
                BYTE    %00111110 ; |  XXXXX |  12
                BYTE    %01011110 ; | X XXXX |  11
                BYTE    %00011110 ; |   XXXX |  10
                BYTE    %00011100 ; |   XXX  |   9
                BYTE    %00101000 ; |  X X   |   8
                BYTE    %01001001 ; | X  X  X|   7
                BYTE    %00010010 ; |   X  X |   6
                BYTE    %00010000 ; |   X    |   5
                BYTE    %00000000 ; |        |   4
                BYTE    %10100000 ; |X X     |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000001 ; |       X|   1
                BYTE    %00000000 ; |        |   0

B4_Blaze1_GRP_0_End:

                DS.B    8, 0

B4_Blaze1_GRP_1:

                BYTE    %01110011 ; | XXX  XX|  15
                BYTE    %00110010 ; |  XX  X |  14
                BYTE    %10010101 ; |X  X X X|  13
                BYTE    %01011101 ; | X XXX X|  12
                BYTE    %00111101 ; |  XXXX X|  11
                BYTE    %10101101 ; |X X XX X|  10
                BYTE    %00101010 ; |  X X X |   9
                BYTE    %01010110 ; | X X XX |   8
                BYTE    %10110010 ; |X XX  X |   7
                BYTE    %11001000 ; |XX  X   |   6
                BYTE    %01000000 ; | X      |   5
                BYTE    %00010101 ; |   X X X|   4
                BYTE    %00000000 ; |        |   3
                BYTE    %01000000 ; | X      |   2
                BYTE    %10001000 ; |X   X   |   1
                BYTE    %01000101 ; | X   X X|   0

                DS.B    8, 0

; ---------------------------------------------------------------------

                ALIGN   $0100

B4_GirderPF2_R: ;  0    0
                BYTE    %11111111 ; |XXXXXXXX|  7
                BYTE    %11111111 ; |XXXXXXXX|  6
                BYTE    %10101010 ; |X X X X |  5
                BYTE    %10101010 ; |X X X X |  4
                BYTE    %10101010 ; |X X X X |  3
                BYTE    %10101010 ; |X X X X |  2
                BYTE    %11111111 ; |XXXXXXXX|  1
                BYTE    %11111111 ; |XXXXXXXX|  0
                ;  1    1_00
                BYTE    %11111101 ; |XXXXXX X|  7
                BYTE    %11111101 ; |XXXXXX X|  6
                BYTE    %10101001 ; |X X X  X|  5
                BYTE    %10101011 ; |X X X XX|  4
                BYTE    %10101011 ; |X X X XX|  3
                BYTE    %10101011 ; |X X X XX|  2
                BYTE    %11111101 ; |XXXXXX X|  1
                BYTE    %11111111 ; |XXXXXXXX|  0
                ;  2    1_01
                BYTE    %11111101 ; |XXXXXX X|  7
                BYTE    %11111101 ; |XXXXXX X|  6
                BYTE    %10101001 ; |X X X  X|  5
                BYTE    %10101001 ; |X X X  X|  4
                BYTE    %10101001 ; |X X X  X|  3
                BYTE    %10101001 ; |X X X  X|  2
                BYTE    %11111101 ; |XXXXXX X|  1
                BYTE    %11111101 ; |XXXXXX X|  0
                ;  3    1_10
                BYTE    %11111101 ; |XXXXXX X|  7
                BYTE    %11111101 ; |XXXXXX X|  6
                BYTE    %10101001 ; |X X X  X|  5
                BYTE    %10101011 ; |X X X XX|  4
                BYTE    %10101011 ; |X X X XX|  3
                BYTE    %10101011 ; |X X X XX|  2
                BYTE    %11111101 ; |XXXXXX X|  1
                BYTE    %11111111 ; |XXXXXXXX|  0
                ;  4    1_11
                BYTE    %11111101 ; |XXXXXX X|  7
                BYTE    %11111101 ; |XXXXXX X|  6
                BYTE    %10101001 ; |X X X  X|  5
                BYTE    %10101001 ; |X X X  X|  4
                BYTE    %10101001 ; |X X X  X|  3
                BYTE    %10101001 ; |X X X  X|  2
                BYTE    %11111101 ; |XXXXXX X|  1
                BYTE    %11111101 ; |XXXXXX X|  0
                ;  5    2_00
                BYTE    %11111101 ; |XXXXXX X|  7
                BYTE    %11111101 ; |XXXXXX X|  6
                BYTE    %10101001 ; |X X X  X|  5
                BYTE    %10101011 ; |X X X XX|  4
                BYTE    %10101011 ; |X X X XX|  3
                BYTE    %10101011 ; |X X X XX|  2
                BYTE    %11111101 ; |XXXXXX X|  1
                BYTE    %11111111 ; |XXXXXXXX|  0
                ;  6    2_01
                BYTE    %11111101 ; |XXXXXX X|  7
                BYTE    %11111101 ; |XXXXXX X|  6
                BYTE    %10101001 ; |X X X  X|  5
                BYTE    %10101001 ; |X X X  X|  4
                BYTE    %10101001 ; |X X X  X|  3
                BYTE    %10101001 ; |X X X  X|  2
                BYTE    %11111101 ; |XXXXXX X|  1
                BYTE    %11111101 ; |XXXXXX X|  0
                ;  7    2_10
                BYTE    %11111101 ; |XXXXXX X|  7
                BYTE    %11111101 ; |XXXXXX X|  6
                BYTE    %10101001 ; |X X X  X|  5
                BYTE    %10101011 ; |X X X XX|  4
                BYTE    %10101011 ; |X X X XX|  3
                BYTE    %10101011 ; |X X X XX|  2
                BYTE    %11111101 ; |XXXXXX X|  1
                BYTE    %11111111 ; |XXXXXXXX|  0
                ;  8    2_11
                BYTE    %11111101 ; |XXXXXX X|  7
                BYTE    %11111101 ; |XXXXXX X|  6
                BYTE    %10101001 ; |X X X  X|  5
                BYTE    %10101001 ; |X X X  X|  4
                BYTE    %10101001 ; |X X X  X|  3
                BYTE    %10101001 ; |X X X  X|  2
                BYTE    %11111101 ; |XXXXXX X|  1
                BYTE    %11111101 ; |XXXXXX X|  0
                ;  9    3_00
                BYTE    %11111101 ; |XXXXXX X|  7
                BYTE    %11111101 ; |XXXXXX X|  6
                BYTE    %10101001 ; |X X X  X|  5
                BYTE    %10101011 ; |X X X XX|  4
                BYTE    %10101011 ; |X X X XX|  3
                BYTE    %10101011 ; |X X X XX|  2
                BYTE    %11111101 ; |XXXXXX X|  1
                BYTE    %11111111 ; |XXXXXXXX|  0
                ; 10    3_01
                BYTE    %11111101 ; |XXXXXX X|  7
                BYTE    %11111101 ; |XXXXXX X|  6
                BYTE    %10101001 ; |X X X  X|  5
                BYTE    %10101001 ; |X X X  X|  4
                BYTE    %10101001 ; |X X X  X|  3
                BYTE    %10101001 ; |X X X  X|  2
                BYTE    %11111101 ; |XXXXXX X|  1
                BYTE    %11111101 ; |XXXXXX X|  0
                ; 11    3_10
                BYTE    %11111101 ; |XXXXXX X|  7
                BYTE    %11111101 ; |XXXXXX X|  6
                BYTE    %10101001 ; |X X X  X|  5
                BYTE    %10101011 ; |X X X XX|  4
                BYTE    %10101011 ; |X X X XX|  3
                BYTE    %10101011 ; |X X X XX|  2
                BYTE    %11111101 ; |XXXXXX X|  1
                BYTE    %11111111 ; |XXXXXXXX|  0
                ; 12    3_11
                BYTE    %11111101 ; |XXXXXX X|  7
                BYTE    %11111101 ; |XXXXXX X|  6
                BYTE    %10101001 ; |X X X  X|  5
                BYTE    %10101001 ; |X X X  X|  4
                BYTE    %10101001 ; |X X X  X|  3
                BYTE    %10101001 ; |X X X  X|  2
                BYTE    %11111101 ; |XXXXXX X|  1
                BYTE    %11111101 ; |XXXXXX X|  0
                ; 13    4_00
                BYTE    %11111101 ; |XXXXXX X|  7
                BYTE    %11111101 ; |XXXXXX X|  6
                BYTE    %10101001 ; |X X X  X|  5
                BYTE    %10101011 ; |X X X XX|  4
                BYTE    %10101011 ; |X X X XX|  3
                BYTE    %10101011 ; |X X X XX|  2
                BYTE    %11111101 ; |XXXXXX X|  1
                BYTE    %11111111 ; |XXXXXXXX|  0
                ; 14    4_01
                BYTE    %11111101 ; |XXXXXX X|  7
                BYTE    %11111101 ; |XXXXXX X|  6
                BYTE    %10101001 ; |X X X  X|  5
                BYTE    %10101001 ; |X X X  X|  4
                BYTE    %10101001 ; |X X X  X|  3
                BYTE    %10101001 ; |X X X  X|  2
                BYTE    %11111101 ; |XXXXXX X|  1
                BYTE    %11111101 ; |XXXXXX X|  0
                ; 15    4_10
                BYTE    %11111101 ; |XXXXXX X|  7
                BYTE    %11111101 ; |XXXXXX X|  6
                BYTE    %10101001 ; |X X X  X|  5
                BYTE    %10101011 ; |X X X XX|  4
                BYTE    %10101011 ; |X X X XX|  3
                BYTE    %10101011 ; |X X X XX|  2
                BYTE    %11111101 ; |XXXXXX X|  1
                BYTE    %11111111 ; |XXXXXXXX|  0
                ; 16    4_11
                BYTE    %11111101 ; |XXXXXX X|  7
                BYTE    %11111101 ; |XXXXXX X|  6
                BYTE    %10101001 ; |X X X  X|  5
                BYTE    %10101001 ; |X X X  X|  4
                BYTE    %10101001 ; |X X X  X|  3
                BYTE    %10101001 ; |X X X  X|  2
                BYTE    %11111101 ; |XXXXXX X|  1
                BYTE    %11111101 ; |XXXXXX X|  0
                ; 17    5
                BYTE    %11111110 ; |XXXXXXX |  7
                BYTE    %11111110 ; |XXXXXXX |  6
                BYTE    %10101010 ; |X X X X |  5
                BYTE    %10101010 ; |X X X X |  4
                BYTE    %10101010 ; |X X X X |  3
                BYTE    %10101010 ; |X X X X |  2
                BYTE    %11111110 ; |XXXXXXX |  1
                BYTE    %11111110 ; |XXXXXXX |  0
                ; 18    6
                BYTE    %11111111 ; |XXXXXXXX|  7
                BYTE    %11111111 ; |XXXXXXXX|  6
                BYTE    %00100010 ; |  X   X |  5
                BYTE    %01110111 ; | XXX XXX|  4
                BYTE    %11011101 ; |XX XXX X|  3
                BYTE    %10001000 ; |X   X   |  2
                BYTE    %11111111 ; |XXXXXXXX|  1
                BYTE    %11111111 ; |XXXXXXXX|  0
                ; 19    7
                BYTE    %11111111 ; |XXXXXXXX|  7
                BYTE    %11111111 ; |XXXXXXXX|  6
                BYTE    %10101010 ; |X X X X |  5
                BYTE    %10101010 ; |X X X X |  4
                BYTE    %10101010 ; |X X X X |  3
                BYTE    %10101010 ; |X X X X |  2
                BYTE    %11111111 ; |XXXXXXXX|  1
                BYTE    %11111111 ; |XXXXXXXX|  0
                ; 20    8
                BYTE    %11111001 ; |XXXXX  X|  7
                BYTE    %11111001 ; |XXXXX  X|  6
                BYTE    %00100000 ; |  X     |  5
                BYTE    %01110001 ; | XXX   X|  4
                BYTE    %11011001 ; |XX XX  X|  3
                BYTE    %10001000 ; |X   X   |  2
                BYTE    %11111001 ; |XXXXX  X|  1
                BYTE    %11111001 ; |XXXXX  X|  0
                ; 21    9
                BYTE    %10111111 ; |X XXXXXX|  7
                BYTE    %11111111 ; |XXXXXXXX|  6
                BYTE    %10010101 ; |X  X X X|  5
                BYTE    %11010101 ; |XX X X X|  4
                BYTE    %11010101 ; |XX X X X|  3
                BYTE    %10010101 ; |X  X X X|  2
                BYTE    %11111111 ; |XXXXXXXX|  1
                BYTE    %10111111 ; |X XXXXXX|  0

; cement -------------------------------------------------------------

                DS.B    1, 0

B4_CEMENT_HEIGHT = B4_Cement_GRP_0_End - B4_Cement_GRP_0

B4_Cement_GRP_0:

                BYTE    %01111110 ; | XXXXXX |   7
                BYTE    %11111111 ; |XXXXXXXX|   6
                BYTE    %11111111 ; |XXXXXXXX|   5
                BYTE    %11111111 ; |XXXXXXXX|   4
                BYTE    %11111111 ; |XXXXXXXX|   3
                BYTE    %01111110 ; | XXXXXX |   2
                BYTE    %00111100 ; |  XXXX  |   1
                BYTE    %00001000 ; |    X   |   0

B4_Cement_GRP_0_End:

                DS.B    16, 0

B4_Cement_GRP_1:

                BYTE    %01111110 ; | XXXXXX |   7
                BYTE    %11111111 ; |XXXXXXXX|   6
                BYTE    %11111111 ; |XXXXXXXX|   5
                BYTE    %11010111 ; |XX X XXX|   4
                BYTE    %01101011 ; | XX X XX|   3
                BYTE    %00110100 ; |  XX X  |   2
                BYTE    %00011100 ; |   XXX  |   1
                BYTE    %00000000 ; |        |   0

                DS.B    16, 0

; --------------------------------------------------------------------

                ALIGN   $0100

B4_GirderPF1_R: ;  0    0
                BYTE    %00111111 ; |  XXXXXX|  7
                BYTE    %00111111 ; |  XXXXXX|  6
                BYTE    %00010101 ; |   X X X|  5
                BYTE    %00010101 ; |   X X X|  4
                BYTE    %00010101 ; |   X X X|  3
                BYTE    %00010101 ; |   X X X|  2
                BYTE    %00111111 ; |  XXXXXX|  1
                BYTE    %00111111 ; |  XXXXXX|  0
                ;  1    1_00
                BYTE    %00011111 ; |   XXXXX|  7
                BYTE    %00011111 ; |   XXXXX|  6
                BYTE    %00001010 ; |    X X |  5
                BYTE    %00001010 ; |    X X |  4
                BYTE    %00001010 ; |    X X |  3
                BYTE    %00001010 ; |    X X |  2
                BYTE    %00011111 ; |   XXXXX|  1
                BYTE    %00011111 ; |   XXXXX|  0
                ;  2    1_01
                BYTE    %00011111 ; |   XXXXX|  7
                BYTE    %00011111 ; |   XXXXX|  6
                BYTE    %00001010 ; |    X X |  5
                BYTE    %00001010 ; |    X X |  4
                BYTE    %00001010 ; |    X X |  3
                BYTE    %00001010 ; |    X X |  2
                BYTE    %00011111 ; |   XXXXX|  1
                BYTE    %00011111 ; |   XXXXX|  0
                ;  3    1_10
                BYTE    %00011111 ; |   XXXXX|  7
                BYTE    %00011111 ; |   XXXXX|  6
                BYTE    %00001010 ; |    X X |  5
                BYTE    %00001010 ; |    X X |  4
                BYTE    %00001010 ; |    X X |  3
                BYTE    %00001010 ; |    X X |  2
                BYTE    %00011111 ; |   XXXXX|  1
                BYTE    %00011111 ; |   XXXXX|  0
                ;  4    1_11
                BYTE    %00011111 ; |   XXXXX|  7
                BYTE    %00011111 ; |   XXXXX|  6
                BYTE    %00001010 ; |    X X |  5
                BYTE    %00001010 ; |    X X |  4
                BYTE    %00001010 ; |    X X |  3
                BYTE    %00001010 ; |    X X |  2
                BYTE    %00011111 ; |   XXXXX|  1
                BYTE    %00011111 ; |   XXXXX|  0
                ;  5    2_00
                BYTE    %00001111 ; |    XXXX|  7
                BYTE    %00001111 ; |    XXXX|  6
                BYTE    %00001010 ; |    X X |  5
                BYTE    %00001010 ; |    X X |  4
                BYTE    %00001010 ; |    X X |  3
                BYTE    %00001010 ; |    X X |  2
                BYTE    %00001111 ; |    XXXX|  1
                BYTE    %00001111 ; |    XXXX|  0
                ;  6    2_01
                BYTE    %00001111 ; |    XXXX|  7
                BYTE    %00001111 ; |    XXXX|  6
                BYTE    %00001010 ; |    X X |  5
                BYTE    %00001010 ; |    X X |  4
                BYTE    %00001010 ; |    X X |  3
                BYTE    %00001010 ; |    X X |  2
                BYTE    %00001111 ; |    XXXX|  1
                BYTE    %00001111 ; |    XXXX|  0
                ;  7    2_10
                BYTE    %00001111 ; |    XXXX|  7
                BYTE    %00001111 ; |    XXXX|  6
                BYTE    %00001010 ; |    X X |  5
                BYTE    %00001010 ; |    X X |  4
                BYTE    %00001010 ; |    X X |  3
                BYTE    %00001010 ; |    X X |  2
                BYTE    %00001111 ; |    XXXX|  1
                BYTE    %00001111 ; |    XXXX|  0
                ;  8    2_11
                BYTE    %00001111 ; |    XXXX|  7
                BYTE    %00001111 ; |    XXXX|  6
                BYTE    %00001010 ; |    X X |  5
                BYTE    %00001010 ; |    X X |  4
                BYTE    %00001010 ; |    X X |  3
                BYTE    %00001010 ; |    X X |  2
                BYTE    %00001111 ; |    XXXX|  1
                BYTE    %00001111 ; |    XXXX|  0
                ;  9    3_00
                BYTE    %00000111 ; |     XXX|  7
                BYTE    %00000111 ; |     XXX|  6
                BYTE    %00000010 ; |      X |  5
                BYTE    %00000010 ; |      X |  4
                BYTE    %00000010 ; |      X |  3
                BYTE    %00000010 ; |      X |  2
                BYTE    %00000111 ; |     XXX|  1
                BYTE    %00000111 ; |     XXX|  0
                ; 10    3_01
                BYTE    %00000111 ; |     XXX|  7
                BYTE    %00000111 ; |     XXX|  6
                BYTE    %00000010 ; |      X |  5
                BYTE    %00000010 ; |      X |  4
                BYTE    %00000010 ; |      X |  3
                BYTE    %00000010 ; |      X |  2
                BYTE    %00000111 ; |     XXX|  1
                BYTE    %00000111 ; |     XXX|  0
                ; 11    3_10
                BYTE    %00000111 ; |     XXX|  7
                BYTE    %00000111 ; |     XXX|  6
                BYTE    %00000010 ; |      X |  5
                BYTE    %00000010 ; |      X |  4
                BYTE    %00000010 ; |      X |  3
                BYTE    %00000010 ; |      X |  2
                BYTE    %00000111 ; |     XXX|  1
                BYTE    %00000111 ; |     XXX|  0
                ; 12    3_11
                BYTE    %00000111 ; |     XXX|  7
                BYTE    %00000111 ; |     XXX|  6
                BYTE    %00000010 ; |      X |  5
                BYTE    %00000010 ; |      X |  4
                BYTE    %00000010 ; |      X |  3
                BYTE    %00000010 ; |      X |  2
                BYTE    %00000111 ; |     XXX|  1
                BYTE    %00000111 ; |     XXX|  0
                ; 13    4_00
                BYTE    %00000011 ; |      XX|  7
                BYTE    %00000011 ; |      XX|  6
                BYTE    %00000010 ; |      X |  5
                BYTE    %00000010 ; |      X |  4
                BYTE    %00000010 ; |      X |  3
                BYTE    %00000010 ; |      X |  2
                BYTE    %00000011 ; |      XX|  1
                BYTE    %00000011 ; |      XX|  0
                ; 14    4_01
                BYTE    %00000011 ; |      XX|  7
                BYTE    %00000011 ; |      XX|  6
                BYTE    %00000010 ; |      X |  5
                BYTE    %00000010 ; |      X |  4
                BYTE    %00000010 ; |      X |  3
                BYTE    %00000010 ; |      X |  2
                BYTE    %00000011 ; |      XX|  1
                BYTE    %00000011 ; |      XX|  0
                ; 15    4_10
                BYTE    %00000011 ; |      XX|  7
                BYTE    %00000011 ; |      XX|  6
                BYTE    %00000010 ; |      X |  5
                BYTE    %00000010 ; |      X |  4
                BYTE    %00000010 ; |      X |  3
                BYTE    %00000010 ; |      X |  2
                BYTE    %00000011 ; |      XX|  1
                BYTE    %00000011 ; |      XX|  0
                ; 16    4_11
                BYTE    %00000011 ; |      XX|  7
                BYTE    %00000011 ; |      XX|  6
                BYTE    %00000010 ; |      X |  5
                BYTE    %00000010 ; |      X |  4
                BYTE    %00000010 ; |      X |  3
                BYTE    %00000010 ; |      X |  2
                BYTE    %00000011 ; |      XX|  1
                BYTE    %00000011 ; |      XX|  0
                ; 17    5
                BYTE    %00000000 ; |        |  7
                BYTE    %00000000 ; |        |  6
                BYTE    %00000000 ; |        |  5
                BYTE    %00000000 ; |        |  4
                BYTE    %00000000 ; |        |  3
                BYTE    %00000000 ; |        |  2
                BYTE    %00000000 ; |        |  1
                BYTE    %00000000 ; |        |  0
                ; 18    6
                BYTE    %00111111 ; |  XXXXXX|  7
                BYTE    %00111111 ; |  XXXXXX|  6
                BYTE    %00000100 ; |     X  |  5
                BYTE    %00101110 ; |  X XXX |  4
                BYTE    %00111011 ; |  XXX XX|  3
                BYTE    %00010001 ; |   X   X|  2
                BYTE    %00111111 ; |  XXXXXX|  1
                BYTE    %00111111 ; |  XXXXXX|  0
                ; 19    7
                BYTE    %00011111 ; |   XXXXX|  7
                BYTE    %00111111 ; |  XXXXXX|  6
                BYTE    %00010101 ; |   X X X|  5
                BYTE    %00110101 ; |  XX X X|  4
                BYTE    %00110101 ; |  XX X X|  3
                BYTE    %00010101 ; |   X X X|  2
                BYTE    %00111111 ; |  XXXXXX|  1
                BYTE    %00011111 ; |   XXXXX|  0
                ; 20    8
                BYTE    %00011111 ; |   XXXXX|  7
                BYTE    %00011111 ; |   XXXXX|  6
                BYTE    %00000100 ; |     X  |  5
                BYTE    %00001110 ; |    XXX |  4
                BYTE    %00011011 ; |   XX XX|  3
                BYTE    %00010001 ; |   X   X|  2
                BYTE    %00011111 ; |   XXXXX|  1
                BYTE    %00011111 ; |   XXXXX|  0
                ; 21    9
                BYTE    %00111111 ; |  XXXXXX|  7
                BYTE    %00111111 ; |  XXXXXX|  6
                BYTE    %00101010 ; |  X X X |  5
                BYTE    %00101010 ; |  X X X |  4
                BYTE    %00101010 ; |  X X X |  3
                BYTE    %00101010 ; |  X X X |  2
                BYTE    %00111111 ; |  XXXXXX|  1
                BYTE    %00111111 ; |  XXXXXX|  0

; umbrella ------------------------------------------------------------

B4_Umbrella_COL_0:

                BYTE    COL_5A ; |D46CC3|  14
                BYTE    COL_5A ; |D46CC3|  13
                BYTE    COL_5A ; |D46CC3|  12
                BYTE    COL_5A ; |D46CC3|  11
                BYTE    COL_00 ; |000000|  10
                BYTE    COL_00 ; |000000|   9
                BYTE    COL_5A ; |D46CC3|   8
                BYTE    COL_5A ; |D46CC3|   7
                BYTE    COL_5A ; |D46CC3|   6
                BYTE    COL_5A ; |D46CC3|   5
                BYTE    COL_5A ; |D46CC3|   4
                BYTE    COL_5A ; |D46CC3|   3
                BYTE    COL_5A ; |D46CC3|   2
                BYTE    COL_5A ; |D46CC3|   1
                BYTE    COL_82 ; |181AA7|   0

B4_Umbrella_COL_1:

                BYTE    COL_00 ; |000000|  14
                BYTE    COL_00 ; |000000|  13
                BYTE    COL_00 ; |000000|  12
                BYTE    COL_00 ; |000000|  11
                BYTE    COL_0E ; |ECECEC|  10
                BYTE    COL_0E ; |ECECEC|   9
                BYTE    COL_0E ; |ECECEC|   8
                BYTE    COL_0E ; |ECECEC|   7
                BYTE    COL_0E ; |ECECEC|   6
                BYTE    COL_0E ; |ECECEC|   5
                BYTE    COL_0E ; |ECECEC|   4
                BYTE    COL_0E ; |ECECEC|   3
                BYTE    COL_0E ; |ECECEC|   2
                BYTE    COL_00 ; |000000|   1
                BYTE    COL_00 ; |000000|   0

; handbag -------------------------------------------------------------

B4_HANDBAG_HEIGHT = B4_Handbag_GRP_1 - B4_Handbag_GRP_0

B4_Handbag_GRP_0:

                BYTE    %01111110 ; | XXXXXX |   8
                BYTE    %01100110 ; | XX  XX |   7
                BYTE    %01100110 ; | XX  XX |   6
                BYTE    %01011010 ; | X XX X |   5
                BYTE    %00111100 ; |  XXXX  |   4
                BYTE    %01011010 ; | X XX X |   3
                BYTE    %01000010 ; | X    X |   2
                BYTE    %00100100 ; |  X  X  |   1
                BYTE    %00011000 ; |   XX   |   0

                DS.B    15, 0

B4_Handbag_GRP_1:

                BYTE    %00000000 ; |        |   8
                BYTE    %00011000 ; |   XX   |   7
                BYTE    %00011000 ; |   XX   |   6
                BYTE    %00100100 ; |  X  X  |   5
                BYTE    %00000000 ; |        |   4
                BYTE    %00000000 ; |        |   3
                BYTE    %00000000 ; |        |   2
                BYTE    %00000000 ; |        |   1
                BYTE    %00000000 ; |        |   0

                DS.B    15, 0

; ---------------------------------------------------------------------

                ALIGN   $0100

B4_GirderCOL:   ;  0    0
                BYTE    COL_AE ; |84E0FC|   7
                BYTE    COL_82 ; |181AA7|   6
                BYTE    COL_82 ; |181AA7|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_82 ; |181AA7|   2
                BYTE    COL_AE ; |84E0FC|   1
                BYTE    COL_82 ; |181AA7|   0
                ;  1    1_00
                BYTE    COL_AE ; |84E0FC|   7
                BYTE    COL_82 ; |181AA7|   6
                BYTE    COL_82 ; |181AA7|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_82 ; |181AA7|   2
                BYTE    COL_AE ; |84E0FC|   1
                BYTE    COL_82 ; |181AA7|   0
                ;  2    1_01
                BYTE    COL_AE ; |84E0FC|   7
                BYTE    COL_82 ; |181AA7|   6
                BYTE    COL_82 ; |181AA7|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_82 ; |181AA7|   2
                BYTE    COL_AE ; |84E0FC|   1
                BYTE    COL_82 ; |181AA7|   0
                ;  3    1_10
                BYTE    COL_AE ; |84E0FC|   7
                BYTE    COL_82 ; |181AA7|   6
                BYTE    COL_82 ; |181AA7|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_82 ; |181AA7|   2
                BYTE    COL_AE ; |84E0FC|   1
                BYTE    COL_82 ; |181AA7|   0
                ;  4    1_11
                BYTE    COL_AE ; |84E0FC|   7
                BYTE    COL_82 ; |181AA7|   6
                BYTE    COL_82 ; |181AA7|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_82 ; |181AA7|   2
                BYTE    COL_AE ; |84E0FC|   1
                BYTE    COL_82 ; |181AA7|   0
                ;  5    2_00
                BYTE    COL_AE ; |84E0FC|   7
                BYTE    COL_82 ; |181AA7|   6
                BYTE    COL_82 ; |181AA7|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_82 ; |181AA7|   2
                BYTE    COL_AE ; |84E0FC|   1
                BYTE    COL_82 ; |181AA7|   0
                ;  6    2_01
                BYTE    COL_AE ; |84E0FC|   7
                BYTE    COL_82 ; |181AA7|   6
                BYTE    COL_82 ; |181AA7|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_82 ; |181AA7|   2
                BYTE    COL_AE ; |84E0FC|   1
                BYTE    COL_82 ; |181AA7|   0
                ;  7    2_10
                BYTE    COL_AE ; |84E0FC|   7
                BYTE    COL_82 ; |181AA7|   6
                BYTE    COL_82 ; |181AA7|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_82 ; |181AA7|   2
                BYTE    COL_AE ; |84E0FC|   1
                BYTE    COL_82 ; |181AA7|   0
                ;  8    2_11
                BYTE    COL_AE ; |84E0FC|   7
                BYTE    COL_82 ; |181AA7|   6
                BYTE    COL_82 ; |181AA7|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_82 ; |181AA7|   2
                BYTE    COL_AE ; |84E0FC|   1
                BYTE    COL_82 ; |181AA7|   0
                ;  9    3_00
                BYTE    COL_AE ; |84E0FC|   7
                BYTE    COL_82 ; |181AA7|   6
                BYTE    COL_82 ; |181AA7|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_82 ; |181AA7|   2
                BYTE    COL_AE ; |84E0FC|   1
                BYTE    COL_82 ; |181AA7|   0
                ; 10    3_01
                BYTE    COL_AE ; |84E0FC|   7
                BYTE    COL_82 ; |181AA7|   6
                BYTE    COL_82 ; |181AA7|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_82 ; |181AA7|   2
                BYTE    COL_AE ; |84E0FC|   1
                BYTE    COL_82 ; |181AA7|   0
                ; 11    3_10
                BYTE    COL_AE ; |84E0FC|   7
                BYTE    COL_82 ; |181AA7|   6
                BYTE    COL_82 ; |181AA7|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_82 ; |181AA7|   2
                BYTE    COL_AE ; |84E0FC|   1
                BYTE    COL_82 ; |181AA7|   0
                ; 12    3_11
                BYTE    COL_AE ; |84E0FC|   7
                BYTE    COL_82 ; |181AA7|   6
                BYTE    COL_82 ; |181AA7|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_82 ; |181AA7|   2
                BYTE    COL_AE ; |84E0FC|   1
                BYTE    COL_82 ; |181AA7|   0
                ; 13    4_00
                BYTE    COL_AE ; |84E0FC|   7
                BYTE    COL_82 ; |181AA7|   6
                BYTE    COL_82 ; |181AA7|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_82 ; |181AA7|   2
                BYTE    COL_AE ; |84E0FC|   1
                BYTE    COL_82 ; |181AA7|   0
                ; 14    4_01
                BYTE    COL_AE ; |84E0FC|   7
                BYTE    COL_82 ; |181AA7|   6
                BYTE    COL_82 ; |181AA7|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_82 ; |181AA7|   2
                BYTE    COL_AE ; |84E0FC|   1
                BYTE    COL_82 ; |181AA7|   0
                ; 15    4_10
                BYTE    COL_AE ; |84E0FC|   7
                BYTE    COL_82 ; |181AA7|   6
                BYTE    COL_82 ; |181AA7|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_82 ; |181AA7|   2
                BYTE    COL_AE ; |84E0FC|   1
                BYTE    COL_82 ; |181AA7|   0
                ; 16    4_11
                BYTE    COL_AE ; |84E0FC|   7
                BYTE    COL_82 ; |181AA7|   6
                BYTE    COL_82 ; |181AA7|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_82 ; |181AA7|   2
                BYTE    COL_AE ; |84E0FC|   1
                BYTE    COL_82 ; |181AA7|   0
                ; 17
                BYTE    COL_AE ; |84E0FC|   7
                BYTE    COL_82 ; |181AA7|   6
                BYTE    COL_82 ; |181AA7|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_82 ; |181AA7|   3
                BYTE    COL_82 ; |181AA7|   2
                BYTE    COL_AE ; |84E0FC|   1
                BYTE    COL_82 ; |181AA7|   0
                ; 18
                BYTE    COL_3E ; |FCBC74|   7
                BYTE    COL_36 ; |C66C3A|   6
                BYTE    COL_36 ; |C66C3A|   5
                BYTE    COL_36 ; |C66C3A|   4
                BYTE    COL_36 ; |C66C3A|   3
                BYTE    COL_36 ; |C66C3A|   2
                BYTE    COL_3E ; |FCBC74|   1
                BYTE    COL_36 ; |C66C3A|   0
                ; 19
                BYTE    COL_36 ; |C66C3A|   7
                BYTE    COL_3E ; |FCBC74|   6
                BYTE    COL_3E ; |FCBC74|   5
                BYTE    COL_3E ; |FCBC74|   4
                BYTE    COL_3E ; |FCBC74|   3
                BYTE    COL_3E ; |FCBC74|   2
                BYTE    COL_3E ; |FCBC74|   1
                BYTE    COL_36 ; |C66C3A|   0
                ; 20
                BYTE    COL_3E ; |FCBC74|   7
                BYTE    COL_36 ; |C66C3A|   6
                BYTE    COL_36 ; |C66C3A|   5
                BYTE    COL_36 ; |C66C3A|   4
                BYTE    COL_36 ; |C66C3A|   3
                BYTE    COL_36 ; |C66C3A|   2
                BYTE    COL_3E ; |FCBC74|   1
                BYTE    COL_36 ; |C66C3A|   0
                ; 21
                BYTE    COL_36 ; |C66C3A|   7
                BYTE    COL_3E ; |FCBC74|   6
                BYTE    COL_3E ; |FCBC74|   5
                BYTE    COL_3E ; |FCBC74|   4
                BYTE    COL_3E ; |FCBC74|   3
                BYTE    COL_3E ; |FCBC74|   2
                BYTE    COL_3E ; |FCBC74|   1
                BYTE    COL_36 ; |C66C3A|   0

; handbag -------------------------------------------------------------

B4_Handbag_COL_0:

                BYTE    COL_5A ; |D46CC3|   8
                BYTE    COL_5A ; |D46CC3|   7
                BYTE    COL_5A ; |D46CC3|   6
                BYTE    COL_5A ; |D46CC3|   5
                BYTE    COL_5A ; |D46CC3|   4
                BYTE    COL_5A ; |D46CC3|   3
                BYTE    COL_5A ; |D46CC3|   2
                BYTE    COL_5A ; |D46CC3|   1
                BYTE    COL_5A ; |D46CC3|   0

B4_Handbag_COL_1:

                BYTE    COL_00 ; |000000|   8
                BYTE    COL_0E ; |ECECEC|   7
                BYTE    COL_0E ; |ECECEC|   6
                BYTE    COL_0E ; |ECECEC|   5
                BYTE    COL_00 ; |000000|   4
                BYTE    COL_00 ; |000000|   3
                BYTE    COL_00 ; |000000|   2
                BYTE    COL_00 ; |000000|   1
                BYTE    COL_00 ; |000000|   0

; ---------------------------------------------------------------------

                ALIGN   $0100

B4_FloorPF1_L:  ;  0
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000000 ; 21
                BYTE    %00010000 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00010000 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %00010000 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00010000 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00010000 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00010000 ;  0
                ;  1
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000000 ; 21
                BYTE    %00001000 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00001000 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %00001000 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00001000 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00001000 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00001000 ;  0
                ;  2
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000000 ; 21
                BYTE    %00000100 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00000100 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %00000100 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00000100 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00000100 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00000100 ;  0
                ;  3
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000000 ; 21
                BYTE    %00000010 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00000010 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %00000010 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00000010 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00000010 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00000010 ;  0
                ;  4
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000000 ; 21
                BYTE    %00000000 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00000000 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %00000000 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00000000 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00000000 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00000000 ;  0
                ;  5
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000000 ; 21
                BYTE    %00000100 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00000100 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %00000100 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00000100 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00000100 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00000100 ;  0
                ;  6
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000000 ; 21
                BYTE    %00000000 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00000000 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %00000000 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00000000 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00000000 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00000000 ;  0
                ;  7
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000000 ; 21
                BYTE    %00000100 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00000100 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %00000100 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00000100 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00000100 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00000100 ;  0
                ;  8
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000000 ; 21
                BYTE    %00001000 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00001000 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %00001000 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00001000 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00001000 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00001000 ;  0
                ;
                BYTE    %00000000 ;  7 ; scrolling extension
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00000000 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
                BYTE    %00000000 ;  1
                BYTE    %00000000 ;  0

                ALIGN   $0100

B4_FloorPF2_L:  ;  0
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000000 ; 21
                BYTE    %10000000 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %10000000 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %10000000 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %10000000 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %10000000 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %10000000 ;  0
                ;  1
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000000 ; 21
                BYTE    %00001000 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00001000 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %00001000 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00001000 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00001000 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00001000 ;  0
                ;  2
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000000 ; 21
                BYTE    %10000000 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %10000000 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %10000000 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %10000000 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %10000000 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %10000000 ;  0
                ;  3
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000000 ; 21
                BYTE    %00000100 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00000100 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %00000100 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00000100 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00000100 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00000100 ;  0
                ;  4
                BYTE    %00001000 ; 23
                BYTE    %00001000 ; 22
                BYTE    %00001000 ; 21
                BYTE    %00001000 ; 20
                BYTE    %00001000 ; 19
                BYTE    %00001000 ; 18
                BYTE    %00001000 ; 17
                BYTE    %00001000 ; 16
                BYTE    %00001000 ; 15
                BYTE    %00001000 ; 14
                BYTE    %00001000 ; 13
                BYTE    %00001000 ; 12
                BYTE    %00001000 ; 11
                BYTE    %00001000 ; 10
                BYTE    %00001000 ;  9
                BYTE    %00001000 ;  8
                BYTE    %00001000 ;  7
                BYTE    %00001000 ;  6
                BYTE    %00001000 ;  5
                BYTE    %00001000 ;  4
                BYTE    %00001000 ;  3
                BYTE    %00001000 ;  2
              ; BYTE    %00001000 ;  1
              ; BYTE    %00001000 ;  0
                ;  5
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000000 ; 21
                BYTE    %00010000 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00010000 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %00010000 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00010000 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00010000 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00010000 ;  0
                ;  6
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000000 ; 21
                BYTE    %00000100 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00000100 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %00000100 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00000100 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00000100 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00000100 ;  0
                ;  7
                BYTE    %01000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %10000000 ; 21
                BYTE    %00010000 ; 20
                BYTE    %01000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %10000000 ; 17
                BYTE    %00010000 ; 16
                BYTE    %01000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %10000000 ; 13
                BYTE    %00010000 ; 12
                BYTE    %01000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %10000000 ;  9
                BYTE    %00010000 ;  8
                BYTE    %10000000 ;  7
                BYTE    %10000000 ;  6
                BYTE    %10000000 ;  5
                BYTE    %10010000 ;  4
                BYTE    %10000000 ;  3
                BYTE    %10000000 ;  2
              ; BYTE    %10000000 ;  1
              ; BYTE    %10010000 ;  0
                ;  8
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000000 ; 21
                BYTE    %00000000 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00000000 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %00000000 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00000000 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00000000 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00000000 ;  0
                ;
                BYTE    %00000000 ;  7 ; scrolling extension
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00000000 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
                BYTE    %00000000 ;  1
                BYTE    %00000000 ;  0

                ALIGN   $0100

B4_FloorPF2_R:  ;  0
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000000 ; 21
                BYTE    %00000000 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00000000 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %00000000 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00000000 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00000000 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00000000 ;  0
                ;  1
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000000 ; 21
                BYTE    %00001000 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00001000 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %00001000 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00001000 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00001000 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00001000 ;  0
                ;  2
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000000 ; 21
                BYTE    %00000000 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00000000 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %00000000 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00000000 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00000000 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00000000 ;  0
                ;  3
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000000 ; 21
                BYTE    %00000100 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00000100 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %00000100 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00000100 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00000100 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00000100 ;  0
                ;  4
                BYTE    %00001000 ; 23
                BYTE    %00001000 ; 22
                BYTE    %00001000 ; 21
                BYTE    %00001000 ; 20
                BYTE    %00001000 ; 19
                BYTE    %00001000 ; 18
                BYTE    %00001000 ; 17
                BYTE    %00001000 ; 16
                BYTE    %00001000 ; 15
                BYTE    %00001000 ; 14
                BYTE    %00001000 ; 13
                BYTE    %00001000 ; 12
                BYTE    %00001000 ; 11
                BYTE    %00001000 ; 10
                BYTE    %00001000 ;  9
                BYTE    %00001000 ;  8
                BYTE    %00001000 ;  7
                BYTE    %00001000 ;  6
                BYTE    %00001000 ;  5
                BYTE    %00001000 ;  4
                BYTE    %00001000 ;  3
                BYTE    %00001000 ;  2
              ; BYTE    %00001000 ;  1
              ; BYTE    %00001000 ;  0
                ;  5
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000000 ; 21
                BYTE    %00010000 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00010000 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %00010000 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00010000 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00010000 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00010000 ;  0
                ;  6
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000000 ; 21
                BYTE    %00001000 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00001000 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %00001000 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00001000 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00001000 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00001000 ;  0
                ;  7
                BYTE    %10000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %01000000 ; 21
                BYTE    %00010000 ; 20
                BYTE    %10000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %01000000 ; 17
                BYTE    %00010000 ; 16
                BYTE    %10000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %01000000 ; 13
                BYTE    %00010000 ; 12
                BYTE    %10000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %01000000 ;  9
                BYTE    %00010000 ;  8
                BYTE    %10000000 ;  7
                BYTE    %10000000 ;  6
                BYTE    %10000000 ;  5
                BYTE    %10010000 ;  4
                BYTE    %10000000 ;  3
                BYTE    %10000000 ;  2
              ; BYTE    %10000000 ;  1
              ; BYTE    %10010000 ;  0
                ;  8
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000000 ; 21
                BYTE    %00000000 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00000000 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %00000000 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00000000 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00000000 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00000000 ;  0
                ;
                BYTE    %00000000 ;  7 ; scrolling extension
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00000000 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
                BYTE    %00000000 ;  1
                BYTE    %00000000 ;  0

                ALIGN   $0100

B4_FloorPF1_R:  ;  0
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000000 ; 21
                BYTE    %00010000 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00010000 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %00010000 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00010000 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00010000 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00000000 ;  0
                ;  1
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000000 ; 21
                BYTE    %00001000 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00001000 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %00001000 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00001000 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00001000 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00001000 ;  0
                ;  2
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000000 ; 21
                BYTE    %00000100 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00000100 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %00000100 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00000100 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00000100 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00000000 ;  0
                ;  3
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000000 ; 21
                BYTE    %00000010 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00000010 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %00000010 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00000010 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00000010 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00000000 ;  0
                ;  4
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000000 ; 21
                BYTE    %00000000 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00000000 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %00000000 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00000000 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00000000 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00000000 ;  0
                ;  5
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000000 ; 21
                BYTE    %00000100 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00000100 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %00000100 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00000100 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00000100 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00000100 ;  0
                ;  6
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000000 ; 21
                BYTE    %00000000 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00000000 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %00000000 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00000000 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00000000 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00000000 ;  0
                ;  7
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000000 ; 21
                BYTE    %00000100 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00000100 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %00000100 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00000100 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00000100 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00000100 ;  0
                ;  8
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000000 ; 21
                BYTE    %00001000 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00001000 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %00001000 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00001000 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00001000 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00001000 ;  0
                ;
                BYTE    %00000000 ;  7 ; scrolling extension
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00000000 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
                BYTE    %00000000 ;  1
                BYTE    %00000000 ;  0

;
; Enemy GRP (part 2)
;
                ALIGN   $0100

; fireball -----------------------------------------------------------

                GRP_PADDING

B4_FIREBALL_1_HEIGHT = B4_Fireball1_GRP_0_End - B4_Fireball1_GRP_0

Fireball1_GRP:
B4_Fireball1_GRP_0:

                BYTE    %00000000 ; |        |  14
                BYTE    %00011000 ; |   XX   |  13
                BYTE    %00111100 ; |  XXXX  |  12
                BYTE    %01111110 ; | XXXXXX |  11
                BYTE    %01010110 ; | X X XX |  10
                BYTE    %01010110 ; | X X XX |   9
                BYTE    %01111110 ; | XXXXXX |   8
                BYTE    %00111100 ; |  XXXX  |   7
                BYTE    %00011100 ; |   XXX  |   6
                BYTE    %10101000 ; |X X X   |   5
                BYTE    %00010000 ; |   X    |   4
                BYTE    %00000001 ; |       X|   3
                BYTE    %00000000 ; |        |   2
                BYTE    %01000100 ; | X   X  |   1
                BYTE    %00000000 ; |        |   0

B4_Fireball1_GRP_0_End:

                GRP_PADDING

B4_Fireball1_GRP_1:

                BYTE    %00111100 ; |  XXXX  |  14
                BYTE    %01100110 ; | XX  XX |  13
                BYTE    %11000011 ; |XX    XX|  12
                BYTE    %10111001 ; |X XXX  X|  11
                BYTE    %11111101 ; |XXXXXX X|  10
                BYTE    %11111101 ; |XXXXXX X|   9
                BYTE    %10111101 ; |X XXXX X|   8
                BYTE    %01011010 ; | X XX X |   7
                BYTE    %01100011 ; | XX   XX|   6
                BYTE    %00010110 ; |   X XX |   5
                BYTE    %00101110 ; |  X XXX |   4
                BYTE    %00100100 ; |  X  X  |   3
                BYTE    %00010010 ; |   X  X |   2
                BYTE    %00010000 ; |   X    |   1
                BYTE    %00001010 ; |    X X |   0

; flame ---------------------------------------------------------------

                GRP_PADDING

B4_FLAME_0_HEIGHT = B4_Flame0_GRP_0_End - B4_Flame0_GRP_0

B4_Flame0_GRP_0:

                BYTE    %00000000 ; |        |  11
                BYTE    %00111000 ; |  XXX   |  10
                BYTE    %01111100 ; | XXXXX  |   9
                BYTE    %01111010 ; | XXXX X |   8
                BYTE    %01111000 ; | XXXX   |   7
                BYTE    %01111100 ; | XXXXX  |   6
                BYTE    %01111100 ; | XXXXX  |   5
                BYTE    %00101100 ; |  X XX  |   4
                BYTE    %00101000 ; |  X X   |   3
                BYTE    %01111000 ; | XXXX   |   2
                BYTE    %01110000 ; | XXX    |   1
                BYTE    %00000000 ; |        |   0

B4_Flame0_GRP_0_End:

                GRP_PADDING

B4_Flame0_GRP_1:

                BYTE    %00111000 ; |  XXX   |  11
                BYTE    %01000100 ; | X   X  |  10
                BYTE    %10000010 ; |X     X |   9
                BYTE    %10110101 ; |X XX X X|   8
                BYTE    %11111111 ; |XXXXXXXX|   7
                BYTE    %11111001 ; |XXXXX  X|   6
                BYTE    %11111001 ; |XXXXX  X|   5
                BYTE    %01111010 ; | XXXX X |   4
                BYTE    %11111100 ; |XXXXXX  |   3
                BYTE    %11110100 ; |XXXX X  |   2
                BYTE    %10001000 ; |X   X   |   1
                BYTE    %01110000 ; | XXX    |   0

                GRP_PADDING

B4_FLAME_1_HEIGHT = B4_Flame1_GRP_0_End - B4_Flame1_GRP_0

B4_Flame1_GRP_0:

                BYTE    %00000000 ; |        |  11
                BYTE    %00001000 ; |    X   |  10
                BYTE    %00011100 ; |   XXX  |   9
                BYTE    %01111100 ; | XXXXX  |   8
                BYTE    %01111010 ; | XXXX X |   7
                BYTE    %01110000 ; | XXX    |   6
                BYTE    %01111000 ; | XXXX   |   5
                BYTE    %00101000 ; |  X X   |   4
                BYTE    %00101000 ; |  X X   |   3
                BYTE    %01111000 ; | XXXX   |   2
                BYTE    %01110000 ; | XXX    |   1
                BYTE    %00000000 ; |        |   0

B4_Flame1_GRP_0_End:

                GRP_PADDING

B4_Flame1_GRP_1:

                BYTE    %00011000 ; |   XX   |  11
                BYTE    %01110101 ; | XXX X X|  10
                BYTE    %11100010 ; |XXX   X |   9
                BYTE    %10000010 ; |X     X |   8
                BYTE    %10100101 ; |X X  X X|   7
                BYTE    %11111011 ; |XXXXX XX|   6
                BYTE    %11110011 ; |XXXX  XX|   5
                BYTE    %01110010 ; | XXX  X |   4
                BYTE    %11110100 ; |XXXX X  |   3
                BYTE    %11110101 ; |XXXX X X|   2
                BYTE    %10001010 ; |X   X X |   1
                BYTE    %01110000 ; | XXX    |   0

                GRP_PADDING
;
; Enemy COL
;
                ALIGN   $0100

; cement -------------------------------------------------------------

B4_Cement_COL_0:

                BYTE    COL_82 ; |181AA7|   7
                BYTE    COL_82 ; |181AA7|   6
                BYTE    COL_82 ; |181AA7|   5
                BYTE    COL_3E ; |FCBC74|   4
                BYTE    COL_3E ; |FCBC74|   3
                BYTE    COL_3E ; |FCBC74|   2
                BYTE    COL_3E ; |FCBC74|   1
                BYTE    COL_3E ; |FCBC74|   0

B4_Cement_COL_1:

                BYTE    COL_82 ; |181AA7|   7
                BYTE    COL_82 ; |181AA7|   6
                BYTE    COL_82 ; |181AA7|   5
                BYTE    COL_3E ; |FCBC74|   4
                BYTE    COL_3E ; |FCBC74|   3
                BYTE    COL_3E ; |FCBC74|   2
                BYTE    COL_3E ; |FCBC74|   1
                BYTE    COL_00 ; |000000|   0

                DS.B    8, 0

; fireball & hammer --------------------------------------------------

Hammer1_COL_0:
B4_Hammer1_COL_0:

                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0 | 1
                BYTE    FIRE_COL_0 | 1

                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0

                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0

Fireball0_COL_0:
B4_Fireball0_COL_0:
B4_Hammer0_COL_0:

                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0

                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0 | 1
                BYTE    FIRE_COL_0 | 1
                BYTE    FIRE_COL_0 | 1
                BYTE    FIRE_COL_0 | 1

                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0 | 1
                BYTE    FIRE_COL_0
                BYTE    FIRE_COL_0

Fireball0_COL_1:
B4_Fireball0_COL_1:
B4_Hammer0_COL_1:

                BYTE    FIRE_COL_1
                BYTE    FIRE_COL_1
                BYTE    FIRE_COL_1
                BYTE    FIRE_COL_1
                BYTE    FIRE_COL_1
                BYTE    FIRE_COL_1
                BYTE    FIRE_COL_1
                BYTE    FIRE_COL_1

                BYTE    FIRE_COL_1
                BYTE    FIRE_COL_1
                BYTE    FIRE_COL_1
                BYTE    FIRE_COL_1
                BYTE    FIRE_COL_1
                BYTE    FIRE_COL_1
                BYTE    FIRE_COL_1
                BYTE    FIRE_COL_1

                BYTE    FIRE_COL_1 | 1
                BYTE    FIRE_COL_1 | 1
                BYTE    FIRE_COL_1 | 1
                BYTE    FIRE_COL_1 | 1
                BYTE    FIRE_COL_1 | 1
                BYTE    FIRE_COL_1
                BYTE    FIRE_COL_1
                BYTE    FIRE_COL_1

Hammer1_COL_1:
B4_Hammer1_COL_1:

                BYTE    FIRE_COL_1
                BYTE    FIRE_COL_1
                BYTE    FIRE_COL_1
                BYTE    FIRE_COL_1 | 1
                BYTE    FIRE_COL_1 | 1
                BYTE    FIRE_COL_1 | 1
                BYTE    FIRE_COL_1 | 1
                BYTE    FIRE_COL_1 | 1

                BYTE    FIRE_COL_1 | 1
                BYTE    FIRE_COL_1 | 1
                BYTE    FIRE_COL_1 | 1
                BYTE    FIRE_COL_1
                BYTE    FIRE_COL_1
                BYTE    FIRE_COL_1
                BYTE    FIRE_COL_1
                BYTE    FIRE_COL_1

                BYTE    FIRE_COL_1
                BYTE    FIRE_COL_1
                BYTE    FIRE_COL_1
                BYTE    FIRE_COL_1
                BYTE    FIRE_COL_1
                BYTE    FIRE_COL_1
                BYTE    FIRE_COL_1
                BYTE    FIRE_COL_1

B4_Fireball1_COL_0 = B4_Fireball0_COL_0
B4_Fireball1_COL_1 = B4_Fireball0_COL_1

; flame --------------------------------------------------------------

B4_Flame0_COL_0 = B4_Fireball0_COL_0
B4_Flame0_COL_1 = B4_Fireball0_COL_1

B4_Flame1_COL_0 = B4_Fireball0_COL_0
B4_Flame1_COL_1 = B4_Fireball0_COL_1

; blaze --------------------------------------------------------------

B4_Blaze0_COL_0 = B4_Fireball0_COL_0
B4_Blaze0_COL_1 = B4_Fireball0_COL_1

B4_Blaze1_COL_0 = B4_Fireball0_COL_0
B4_Blaze1_COL_1 = B4_Fireball0_COL_1

; ********************************************************************

#if PRINT_MESSAGES
                ECHO "---- Bank 4: ", (ROMJumpTable - *), "bytes of ROM left"
#endif
