
; ********************************************************************
;  Donkey Kong VCS
;
;    ROM Bank 3
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
;       Bank 3 Draw Floors
; --------------------------------------------------------------------

DrawFloors:     jmp     (Ptr)

SwitchFloors:   SUBROUTINE

                lda     (ST_EnemyGRPPtr+8*2),y
                sta     GRP0
                lda     (ST_EnemyCOLPtr+4*2),y
                sta     COLUP0

                cpy     #0
                bne     .switch
                jmp     B3_DrawFloor2

.switch         lda     #0                      ; before branching to shorten distance
                cpy     #23
                bcc     B3_Floor3_0
                cpy     #24
                bcc     B3_Floor3_1
                cpy     #25
                bcs     .skip
                sta     WSYNC
                sta     ST_HMPos+3
                bcc     B3_Floor3_2

.skip           tya
                sec
                sbc     #25
                tay

;
; Floor 3 ------------------------------------------------------------
;

B3_DrawFloor3:  SUBROUTINE

B3_Floor3_3:    ldx     ST_PFIdx+7

.loop0          lda     B3_GirderCOL,x
                sta     WSYNC
                sta     COLUPF

                lda     (ST_MarioGRPPtr+7*2),y
                sta     GRP1
                lda     (ST_MarioCOLPtr+7*2),y
                sta     COLUP1

                lda     B3_GirderPF1_L,x
                sta     PF1
                lda     B3_GirderPF2_L,x
                sta     PF2

                lda     (ST_EnemyGRPPtr+7*2),y
                sta     GRP0

                lda     B3_GirderPF2_R,x
                sta     PF2
                lda     B3_GirderPF1_R,x
                sta     PF1

                dex
                dey
                bpl     .loop0

                ; ----------------------------------------------------

B3_Floor3_2:    ldy     #23

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

B3_Floor3_1:    lda     (ST_EnemyGRPPtr+6*2),y
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

B3_Floor3_0:    ldx     ST_PFIdx+6
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

                lda     B3_FloorPF1_L,x
                sta     PF1
                lda     B3_FloorPF2_L,x
                sta     PF2

                lda     (ST_EnemyGRPPtr+6*2),y
                sta     GRP0

                lda     B3_FloorPF2_R,x
                sta     PF2
                lda     B3_FloorPF1_R,x
                sta     PF1

                dex
                dey
                bpl     .loop1

;
; Floor 2 ------------------------------------------------------------
;

B3_DrawFloor2:  SUBROUTINE

B3_Floor2_3:    ldx     ST_PFIdx+5
                ldy     #7

.loop0          lda     B3_GirderCOL,x
                sta     WSYNC
                sta     COLUPF

                lda     (ST_MarioGRPPtr+5*2),y
                sta     GRP1
                lda     (ST_MarioCOLPtr+5*2),y
                sta     COLUP1

                lda     B3_GirderPF1_L,x
                sta     PF1
                lda     B3_GirderPF2_L,x
                sta     PF2

                lda     (ST_EnemyGRPPtr+5*2),y
                sta     GRP0

                lda     B3_GirderPF2_R,x
                sta     PF2
                lda     B3_GirderPF1_R,x
                sta     PF1

Floor2_3_Cnt:   dex
                dey
                bpl     .loop0

                ; ----------------------------------------------------

B3_Floor2_2:    ldy     #23

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

B3_Floor2_1:    sta     WSYNC
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

B3_Floor2_0:    ldx     ST_PFIdx+4
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

                lda     B3_FloorPF1_L,x
                sta     PF1
                lda     B3_FloorPF2_L,x
                sta     PF2

                lda     (ST_EnemyGRPPtr+4*2),y
                sta     GRP0

                lda     B3_FloorPF2_R,x
                sta     PF2
                lda     B3_FloorPF1_R,x
                sta     PF1

                dex
                dey
                bpl     .loop1

;
; Floor 1 ------------------------------------------------------------
;

B3_DrawFloor1:  SUBROUTINE

B3_Floor1_3:    ldx     ST_PFIdx+3
                ldy     #7

.loop0          lda     B3_GirderCOL,x
                sta     WSYNC
                sta     COLUPF

                lda     (ST_MarioGRPPtr+3*2),y
                sta     GRP1
                lda     (ST_MarioCOLPtr+3*2),y
                sta     COLUP1

                lda     B3_GirderPF1_L,x
                sta     PF1
                lda     B3_GirderPF2_L,x
                sta     PF2

                lda     (ST_EnemyGRPPtr+3*2),y
                sta     GRP0

                lda     B3_GirderPF2_R,x
                sta     PF2
                lda     B3_GirderPF1_R,x
                sta     PF1

Floor1_3_Cnt:   dex
                dey
                bpl     .loop0

                ; ----------------------------------------------------

B3_Floor1_2:    ldy     #23

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

B3_Floor1_1:    sta     WSYNC
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

B3_Floor1_0:    ldx     ST_PFIdx+2
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

                lda     B3_FloorPF1_L,x
                sta     PF1
                lda     B3_FloorPF2_L,x
                sta     PF2

                lda     (ST_EnemyGRPPtr+2*2),y
                sta     GRP0

                lda     B3_FloorPF2_R,x
                sta     PF2
                lda     B3_FloorPF1_R,x
                sta     PF1

                dex
                dey
                bpl     .loop1

;
; Floor 0 ------------------------------------------------------------
;

B3_DrawFloor0:  SUBROUTINE

B3_Floor0_3:    ldx     ST_PFIdx+1
                ldy     #7

.loop0          lda     B3_GirderCOL,x
                sta     WSYNC
                sta     COLUPF

                lda     (ST_MarioGRPPtr+1*2),y
                sta     GRP1
                lda     (ST_MarioCOLPtr+1*2),y
                sta     COLUP1

                lda     B3_GirderPF1_L,x
                sta     PF1
                lda     B3_GirderPF2_L,x
                sta     PF2

                lda     (ST_EnemyGRPPtr+1*2),y
                sta     GRP0

                lda     B3_GirderPF2_R,x
                sta     PF2
                lda     B3_GirderPF1_R,x
                sta     PF1

                dex
                dey
                bpl     .loop0

                ; ----------------------------------------------------

B3_Floor0_2:    ldy     #23

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
                bcs     B3_SkipGround

                lda     (ST_EnemyGRPPtr+0*2),y

                ; ----------------------------------------------------

B3_Floor0_1:    sta     WSYNC
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
                bcs     B3_SkipGround

                ; ----------------------------------------------------

B3_Floor0_0:    ldx     ST_PFIdx+0
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

                lda     B3_FloorPF1_L,x
                sta     PF1
                lda     B3_FloorPF2_L,x
                sta     PF2

                lda     (ST_EnemyGRPPtr+0*2),y
                sta     GRP0

                lda     B3_FloorPF2_R,x
                sta     PF2
                lda     B3_FloorPF1_R,x
                sta     PF1

                dex
                dey

                cpy     ST_ScrollCtr1
                bpl     .loop1

;
; Ground -------------------------------------------------------------
;

B3_DrawGround:  SUBROUTINE
                ldx     ST_PFIdx+7              ; reuse from floor 3 girder
                ldy     #7
B3_SkipGround:
.loop           sta     WSYNC
                lda     #0
                sta     GRP1
                lda     ST_ScrollCtr0
                cmp     #8
                bcs     .blank

                lda     B3_GirderCOL,x
                sta     COLUPF

                lda     B3_GirderPF1_L,x
                sta     PF1
                lda     B3_GirderPF2_L,x
                sta     PF2

                lda     (ST_EnemyGRPPtr+7*2),y
                sta     GRP0

                lda     B3_GirderPF2_R,x
                sta     PF2
                lda     B3_GirderPF1_R,x
                sta     PF1

                dex
                dey
                cpy     ST_ScrollCtr0
                bpl     .loop

                sta     WSYNC
.blank          lda     #%00000010
                sta     VBLANK

                jmp     B3_ReadJoystick

;
; Top ----------------------------------------------------------------
;

DrawTop:        SUBROUTINE

                ; second girder line -- set coarse Mario position

                ldx     ST_PFIdx+5
                dex

                lda     B3_GirderPF1_L,x
                sta     PF1
                lda     B3_GirderPF2_L,x
                sta     PF2

                lda     B3_GirderCOL,x
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
                lda     B3_GirderCOL,x
                sta     COLUPF

                lda     B3_GirderPF1_L,x
                sta     PF1
                lda     B3_GirderPF2_L,x
                sta     PF2

                lda     MarioDir_R
                sta     REFP1
                sta     VDELP1
                ldy     #5
                sta     HMCLR

                lda     B3_GirderPF2_R,x
                sta     PF2
                lda     B3_GirderPF1_R,x
                sta     PF1

                ; continue with girder in floor 1 or 2

                jmp     (ST_PtrTmp)

; --------------------------------------------------------------------
;       B3 Data Fetch Functions
; --------------------------------------------------------------------

                ; fetch data for first girder line, used in another bank

GetTopGirder:   ldx     ST_PFIdx+5
                lda     B3_GirderCOL,x
                sta     ST_COLTmp
                lda     B3_GirderPF1_L,x
                sta     ST_PF1Tmp
                lda     B3_GirderPF2_L,x
                sta     ST_PF2Tmp
                rts


; ********************************************************************
;
;       Data Section
;
; ********************************************************************

; --------------------------------------------------------------------
;       B3 Graphics
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

B3_FIREBALL_0_HEIGHT = B3_Fireball0_GRP_0_End - B3_Fireball0_GRP_0

Fireball0_GRP:
B3_Fireball0_GRP_0:

                BYTE    %00000000 ; |        |  14
                BYTE    %00000000 ; |        |  13
                BYTE    %00111000 ; |  XXX   |  12
                BYTE    %01111100 ; | XXXXX  |  11
                BYTE    %01010100 ; | X X X  |  10
                BYTE    %01010100 ; | X X X  |   9
                BYTE    %01111100 ; | XXXXX  |   8
                BYTE    %00111100 ; |  XXXX  |   7
                BYTE    %00011000 ; |   XX   |   6
                BYTE    %00000000 ; |        |   5
                BYTE    %01000000 ; | X      |   4
                BYTE    %00000010 ; |      X |   3
                BYTE    %00000001 ; |       X|   2
                BYTE    %00010000 ; |   X    |   1
                BYTE    %00000000 ; |        |   0

B3_Fireball0_GRP_0_End:

                GRP_PADDING

B3_Fireball0_GRP_1:

                BYTE    %00111000 ; |  XXX   |  14
                BYTE    %01111100 ; | XXXXX  |  13
                BYTE    %11000110 ; |XX   XX |  12
                BYTE    %10010010 ; |X  X  X |  11
                BYTE    %10111010 ; |X XXX X |  10
                BYTE    %10111010 ; |X XXX X |   9
                BYTE    %10011010 ; |X  XX X |   8
                BYTE    %01000010 ; | X    X |   7
                BYTE    %01100100 ; | XX  X  |   6
                BYTE    %00111110 ; |  XXXXX |   5
                BYTE    %00011101 ; |   XXX X|   4
                BYTE    %00110100 ; |  XX X  |   3
                BYTE    %00101000 ; |  X X   |   2
                BYTE    %01000000 ; | X      |   1
                BYTE    %00100101 ; |  X  X X|   0

                GRP_PADDING

; hat ----------------------------------------------------------------

B3_HAT_HEIGHT = B3_Hat_GRP_0 - B3_Hat_COL_0

B3_Hat_GRP_0:

                BYTE    %01011010 ; | X XX X |   7
                BYTE    %11111111 ; |XXXXXXXX|   6
                BYTE    %01111110 ; | XXXXXX |   5
                BYTE    %00111100 ; |  XXXX  |   4
                BYTE    %00111100 ; |  XXXX  |   3
                BYTE    %00111100 ; |  XXXX  |   2
                BYTE    %00011110 ; |   XXXX |   1
                BYTE    %00011000 ; |   XX   |   0

                DS.B    16, 0

B3_Hat_GRP_1 = B3_Hat_GRP_0

B3_Hat_COL_0:

                BYTE    COL_5A ; |D46CC3|   7
                BYTE    COL_5A ; |D46CC3|   6
                BYTE    COL_0E ; |ECECEC|   5
                BYTE    COL_82 ; |181AA7|   4
                BYTE    COL_0E ; |ECECEC|   3
                BYTE    COL_0E ; |ECECEC|   2
                BYTE    COL_0E ; |ECECEC|   1
                BYTE    COL_0E ; |ECECEC|   0

                DS.B    16, 0

B3_Hat_COL_1 = B3_Hat_COL_0

;
; Playfield
;
                ALIGN   $0100

B3_GirderPF1_L: ;  0
                BYTE    %00111111 ; |  XXXXXX|  7
                BYTE    %00111111 ; |  XXXXXX|  6
                BYTE    %00100010 ; |  X   X |  5
                BYTE    %00110111 ; |  XX XXX|  4
                BYTE    %00011101 ; |   XXX X|  3
                BYTE    %00001000 ; |    X   |  2
                BYTE    %00111111 ; |  XXXXXX|  1
                BYTE    %00111111 ; |  XXXXXX|  0
                ;  1
                BYTE    %00111111 ; |  XXXXXX|  7
                BYTE    %00111111 ; |  XXXXXX|  6
                BYTE    %00100010 ; |  X   X |  5
                BYTE    %00110111 ; |  XX XXX|  4
                BYTE    %00011101 ; |   XXX X|  3
                BYTE    %00001000 ; |    X   |  2
                BYTE    %00111111 ; |  XXXXXX|  1
                BYTE    %00111111 ; |  XXXXXX|  0
                ;  2
                BYTE    %00001111 ; |    XXXX|  7
                BYTE    %00001111 ; |    XXXX|  6
                BYTE    %00000010 ; |      X |  5
                BYTE    %00000111 ; |     XXX|  4
                BYTE    %00001101 ; |    XX X|  3
                BYTE    %00001000 ; |    X   |  2
                BYTE    %00001111 ; |    XXXX|  1
                BYTE    %00001111 ; |    XXXX|  0
                ;  3
                BYTE    %00000000 ; |        |  7
                BYTE    %00000000 ; |        |  6
                BYTE    %00000000 ; |        |  5
                BYTE    %00000000 ; |        |  4
                BYTE    %00000000 ; |        |  3
                BYTE    %00000000 ; |        |  2
                BYTE    %00000000 ; |        |  1
                BYTE    %00000000 ; |        |  0
                ;  4
                BYTE    %00111100 ; |  XXXX  |  7
                BYTE    %00111111 ; |  XXXXXX|  6
                BYTE    %00100011 ; |  X   XX|  5
                BYTE    %00110111 ; |  XX XXX|  4
                BYTE    %00011111 ; |   XXXXX|  3
                BYTE    %00001011 ; |    X XX|  2
                BYTE    %00111100 ; |  XXXX  |  1
                BYTE    %00111111 ; |  XXXXXX|  0
                ;  5
                BYTE    %00111000 ; |  XXX   |  7
                BYTE    %00111000 ; |  XXX   |  6
                BYTE    %00100000 ; |  X     |  5
                BYTE    %00110000 ; |  XX    |  4
                BYTE    %00011000 ; |   XX   |  3
                BYTE    %00001000 ; |    X   |  2
                BYTE    %00111000 ; |  XXX   |  1
                BYTE    %00111000 ; |  XXX   |  0
                ;  6
                BYTE    %00111000 ; |  XXX   |  7
                BYTE    %00111000 ; |  XXX   |  6
                BYTE    %00100000 ; |  X     |  5
                BYTE    %00110000 ; |  XX    |  4
                BYTE    %00011000 ; |   XX   |  3
                BYTE    %00001000 ; |    X   |  2
                BYTE    %00111000 ; |  XXX   |  1
                BYTE    %00111000 ; |  XXX   |  0
                ;  7
                BYTE    %00111000 ; |  XXX   |  7
                BYTE    %00111000 ; |  XXX   |  6
                BYTE    %00100000 ; |  X     |  5
                BYTE    %00110000 ; |  XX    |  4
                BYTE    %00011000 ; |   XX   |  3
                BYTE    %00001000 ; |    X   |  2
                BYTE    %00111000 ; |  XXX   |  1
                BYTE    %00111000 ; |  XXX   |  0
                ;  8
                BYTE    %00111100 ; |  XXXX  |  7
                BYTE    %00111111 ; |  XXXXXX|  6
                BYTE    %00100011 ; |  X   XX|  5
                BYTE    %00110111 ; |  XX XXX|  4
                BYTE    %00011111 ; |   XXXXX|  3
                BYTE    %00001011 ; |    X XX|  2
                BYTE    %00111100 ; |  XXXX  |  1
                BYTE    %00111111 ; |  XXXXXX|  0

B3_GirderPF2_L: ;  0
                BYTE    %11111111 ; |XXXXXXXX|  7
                BYTE    %11111111 ; |XXXXXXXX|  6
                BYTE    %01000100 ; | X   X  |  5
                BYTE    %11101110 ; |XXX XXX |  4
                BYTE    %10111011 ; |X XXX XX|  3
                BYTE    %00010001 ; |   X   X|  2
                BYTE    %11111111 ; |XXXXXXXX|  1
                BYTE    %11111111 ; |XXXXXXXX|  0
                ;  1
                BYTE    %11111111 ; |XXXXXXXX|  7
                BYTE    %11111111 ; |XXXXXXXX|  6
                BYTE    %01000100 ; | X   X  |  5
                BYTE    %11101110 ; |XXX XXX |  4
                BYTE    %10111011 ; |X XXX XX|  3
                BYTE    %00010001 ; |   X   X|  2
                BYTE    %11111111 ; |XXXXXXXX|  1
                BYTE    %11111111 ; |XXXXXXXX|  0
                ;  2
                BYTE    %11111111 ; |XXXXXXXX|  7
                BYTE    %11111111 ; |XXXXXXXX|  6
                BYTE    %01000100 ; | X   X  |  5
                BYTE    %11101110 ; |XXX XXX |  4
                BYTE    %10111011 ; |X XXX XX|  3
                BYTE    %00010001 ; |   X   X|  2
                BYTE    %11111111 ; |XXXXXXXX|  1
                BYTE    %11111111 ; |XXXXXXXX|  0
                ;  3
                BYTE    %11111000 ; |XXXXX   |  7
                BYTE    %11111000 ; |XXXXX   |  6
                BYTE    %01000000 ; | X      |  5
                BYTE    %11101000 ; |XXX X   |  4
                BYTE    %10111000 ; |X XXX   |  3
                BYTE    %00010000 ; |   X    |  2
                BYTE    %11111000 ; |XXXXX   |  1
                BYTE    %11111000 ; |XXXXX   |  0
                ;  4
                BYTE    %00111111 ; |  XXXXXX|  7
                BYTE    %11111111 ; |XXXXXXXX|  6
                BYTE    %11000100 ; |XX   X  |  5
                BYTE    %11101110 ; |XXX XXX |  4
                BYTE    %11111011 ; |XXXXX XX|  3
                BYTE    %11010001 ; |XX X   X|  2
                BYTE    %00111111 ; |  XXXXXX|  1
                BYTE    %11111111 ; |XXXXXXXX|  0
                ;  5
                BYTE    %00011100 ; |   XXX  |  7
                BYTE    %00011100 ; |   XXX  |  6
                BYTE    %00000100 ; |     X  |  5
                BYTE    %00001100 ; |    XX  |  4
                BYTE    %00011000 ; |   XX   |  3
                BYTE    %00010000 ; |   X    |  2
                BYTE    %00011100 ; |   XXX  |  1
                BYTE    %00011100 ; |   XXX  |  0
                ;  6
                BYTE    %00010100 ; |   X X  |  7
                BYTE    %00010100 ; |   X X  |  6
                BYTE    %00000000 ; |        |  5
                BYTE    %00010100 ; |   X X  |  4
                BYTE    %00010100 ; |   X X  |  3
                BYTE    %00000000 ; |        |  2
                BYTE    %00010100 ; |   X X  |  1
                BYTE    %00010100 ; |   X X  |  0
                ;  7
                BYTE    %00011100 ; |   XXX  |  7
                BYTE    %00011100 ; |   XXX  |  6
                BYTE    %00000100 ; |     X  |  5
                BYTE    %00001100 ; |    XX  |  4
                BYTE    %00011000 ; |   XX   |  3
                BYTE    %00010000 ; |   X    |  2
                BYTE    %00011100 ; |   XXX  |  1
                BYTE    %00011100 ; |   XXX  |  0
                ;  8
                BYTE    %00111111 ; |  XXXXXX|  7
                BYTE    %11111111 ; |XXXXXXXX|  6
                BYTE    %11000100 ; |XX   X  |  5
                BYTE    %11101110 ; |XXX XXX |  4
                BYTE    %11111011 ; |XXXXX XX|  3
                BYTE    %11010001 ; |XX X   X|  2
                BYTE    %00111111 ; |  XXXXXX|  1
                BYTE    %11111111 ; |XXXXXXXX|  0

; ---------------------------------------------------------------------

B3_ReadJoystick:SUBROUTINE

                ; read ports

                ldx     SWCHA
                lda     INPT4
                and     INPT5                                           ; use either button

                ; demo mode "AI"

                ldy     DemoMode_R
                bne     .write
                lda     #%10000000                                      ; don't jump
.left           ldx     #%10111111                                      ; preset joystick left
                ldy     MarioFloor_R
                cpy     #1*4                                            ; Mario on floor 1 ?
                beq     .write                                          ; yes -> walk left
.up             ldx     #%11101111                                      ; preset joystick up
                ldy     MarioHPos_R
                cpy     #114                                            ; Mario has reached ladder ?
                beq     .write                                          ; yes -> climb up
.right          ldx     #%01111111                                      ; else walk right
                ldy     EnemyAnimTimer_R+3
                cpy     #S1_FIREBALL_SPAWN_DELAY-ST_MARIO_START_DELAY   ; Mario has appeared (and jumped) ?
                bcc     .write                                          ; yes -> walk right
.jump           sta     ButtonDebounce_W                                ; debounce (A = %10000000)
                asl                                                     ; jump (A := 0)
                ldx     #%11111111                                      ; don't walk

                ; set shadow registers

.write          stx     ST_ShadowSWCHA
                sta     ST_ShadowINPT

                ; finish line

                sta     WSYNC
                rts

; ---------------------------------------------------------------------

                ALIGN   $0100

B3_GirderPF2_R: ;  0
                BYTE    %11111111 ; |XXXXXXXX|  7
                BYTE    %11111111 ; |XXXXXXXX|  6
                BYTE    %00100010 ; |  X   X |  5
                BYTE    %01110111 ; | XXX XXX|  4
                BYTE    %11011101 ; |XX XXX X|  3
                BYTE    %10001000 ; |X   X   |  2
                BYTE    %11111111 ; |XXXXXXXX|  1
                BYTE    %11111111 ; |XXXXXXXX|  0
                ;  1
                BYTE    %11111111 ; |XXXXXXXX|  7
                BYTE    %11111111 ; |XXXXXXXX|  6
                BYTE    %00100010 ; |  X   X |  5
                BYTE    %01110111 ; | XXX XXX|  4
                BYTE    %11011101 ; |XX XXX X|  3
                BYTE    %10001000 ; |X   X   |  2
                BYTE    %11111111 ; |XXXXXXXX|  1
                BYTE    %11111111 ; |XXXXXXXX|  0
                ;  2
                BYTE    %11111111 ; |XXXXXXXX|  7
                BYTE    %11111111 ; |XXXXXXXX|  6
                BYTE    %00100010 ; |  X   X |  5
                BYTE    %01110111 ; | XXX XXX|  4
                BYTE    %11011101 ; |XX XXX X|  3
                BYTE    %10001000 ; |X   X   |  2
                BYTE    %11111111 ; |XXXXXXXX|  1
                BYTE    %11111111 ; |XXXXXXXX|  0
                ;  3
                BYTE    %11111000 ; |XXXXX   |  7
                BYTE    %11111000 ; |XXXXX   |  6
                BYTE    %00100000 ; |  X     |  5
                BYTE    %01110000 ; | XXX    |  4
                BYTE    %11011000 ; |XX XX   |  3
                BYTE    %10001000 ; |X   X   |  2
                BYTE    %11111000 ; |XXXXX   |  1
                BYTE    %11111000 ; |XXXXX   |  0
                ;  4
                BYTE    %11111111 ; |XXXXXXXX|  7
                BYTE    %11111111 ; |XXXXXXXX|  6
                BYTE    %00100010 ; |  X   X |  5
                BYTE    %01110111 ; | XXX XXX|  4
                BYTE    %11011101 ; |XX XXX X|  3
                BYTE    %10001000 ; |X   X   |  2
                BYTE    %11111111 ; |XXXXXXXX|  1
                BYTE    %11111111 ; |XXXXXXXX|  0
                ;  5
                BYTE    %00011011 ; |   XX XX|  7
                BYTE    %00011011 ; |   XX XX|  6
                BYTE    %00000010 ; |      X |  5
                BYTE    %00010011 ; |   X  XX|  4
                BYTE    %00011001 ; |   XX  X|  3
                BYTE    %00001000 ; |    X   |  2
                BYTE    %00011011 ; |   XX XX|  1
                BYTE    %00011011 ; |   XX XX|  0
                ;  6
                BYTE    %00011111 ; |   XXXXX|  7
                BYTE    %00011111 ; |   XXXXX|  6
                BYTE    %00000010 ; |      X |  5
                BYTE    %00010111 ; |   X XXX|  4
                BYTE    %00011101 ; |   XXX X|  3
                BYTE    %00001000 ; |    X   |  2
                BYTE    %00011111 ; |   XXXXX|  1
                BYTE    %00011111 ; |   XXXXX|  0
                ;  7
                BYTE    %00111011 ; |  XXX XX|  7
                BYTE    %00111011 ; |  XXX XX|  6
                BYTE    %00100010 ; |  X   X |  5
                BYTE    %00110011 ; |  XX  XX|  4
                BYTE    %00011001 ; |   XX  X|  3
                BYTE    %00001000 ; |    X   |  2
                BYTE    %00111011 ; |  XXX XX|  1
                BYTE    %00111011 ; |  XXX XX|  0
                ;  8
                BYTE    %11111110 ; |XXXXXXX |  7
                BYTE    %11111110 ; |XXXXXXX |  6
                BYTE    %00100010 ; |  X   X |  5
                BYTE    %01110110 ; | XXX XX |  4
                BYTE    %11011100 ; |XX XXX  |  3
                BYTE    %10001000 ; |X   X   |  2
                BYTE    %11111110 ; |XXXXXXX |  1
                BYTE    %11111110 ; |XXXXXXX |  0

B3_GirderPF1_R: ;  0
                BYTE    %00111111 ; |  XXXXXX|  7
                BYTE    %00111111 ; |  XXXXXX|  6
                BYTE    %00000100 ; |     X  |  5
                BYTE    %00101110 ; |  X XXX |  4
                BYTE    %00111011 ; |  XXX XX|  3
                BYTE    %00010001 ; |   X   X|  2
                BYTE    %00111111 ; |  XXXXXX|  1
                BYTE    %00111111 ; |  XXXXXX|  0
                ;  1
                BYTE    %00001111 ; |    XXXX|  7
                BYTE    %00001111 ; |    XXXX|  6
                BYTE    %00000100 ; |     X  |  5
                BYTE    %00001110 ; |    XXX |  4
                BYTE    %00001011 ; |    X XX|  3
                BYTE    %00000001 ; |       X|  2
                BYTE    %00001111 ; |    XXXX|  1
                BYTE    %00001111 ; |    XXXX|  0
                ;  2
                BYTE    %00111111 ; |  XXXXXX|  7
                BYTE    %00111111 ; |  XXXXXX|  6
                BYTE    %00000100 ; |     X  |  5
                BYTE    %00101110 ; |  X XXX |  4
                BYTE    %00111011 ; |  XXX XX|  3
                BYTE    %00010001 ; |   X   X|  2
                BYTE    %00111111 ; |  XXXXXX|  1
                BYTE    %00111111 ; |  XXXXXX|  0
                ;  3
                BYTE    %00000000 ; |        |  7
                BYTE    %00000000 ; |        |  6
                BYTE    %00000000 ; |        |  5
                BYTE    %00000000 ; |        |  4
                BYTE    %00000000 ; |        |  3
                BYTE    %00000000 ; |        |  2
                BYTE    %00000000 ; |        |  1
                BYTE    %00000000 ; |        |  0
                ;  4
                BYTE    %00111111 ; |  XXXXXX|  7
                BYTE    %00111111 ; |  XXXXXX|  6
                BYTE    %00000100 ; |     X  |  5
                BYTE    %00101110 ; |  X XXX |  4
                BYTE    %00111011 ; |  XXX XX|  3
                BYTE    %00010001 ; |   X   X|  2
                BYTE    %00111111 ; |  XXXXXX|  1
                BYTE    %00111111 ; |  XXXXXX|  0
                ;  5
                BYTE    %00111110 ; |  XXXXX |  7
                BYTE    %00111110 ; |  XXXXX |  6
                BYTE    %00000100 ; |     X  |  5
                BYTE    %00101110 ; |  X XXX |  4
                BYTE    %00111010 ; |  XXX X |  3
                BYTE    %00010000 ; |   X    |  2
                BYTE    %00111110 ; |  XXXXX |  1
                BYTE    %00111110 ; |  XXXXX |  0
                ;  6
                BYTE    %00111110 ; |  XXXXX |  7
                BYTE    %00111110 ; |  XXXXX |  6
                BYTE    %00000100 ; |    XX  |  5
                BYTE    %00101110 ; |  X XXX |  4
                BYTE    %00111010 ; |  XXX X |  3
                BYTE    %00010000 ; |   XX   |  2
                BYTE    %00111110 ; |  XXXXX |  1
                BYTE    %00111110 ; |  XXXXX |  0
                ;  7
                BYTE    %00111110 ; |  XXXXX |  7
                BYTE    %00111110 ; |  XXXXX |  6
                BYTE    %00000100 ; |     X  |  5
                BYTE    %00101110 ; |  X XXX |  4
                BYTE    %00111010 ; |  XXX X |  3
                BYTE    %00010000 ; |   X    |  2
                BYTE    %00111110 ; |  XXXXX |  1
                BYTE    %00111110 ; |  XXXXX |  0
                ;  8
                BYTE    %00110000 ; |  XX    |  7
                BYTE    %00110000 ; |  XX    |  6
                BYTE    %00000000 ; |        |  5
                BYTE    %00100000 ; |  X     |  4
                BYTE    %00110000 ; |  XX    |  3
                BYTE    %00010000 ; |   X    |  2
                BYTE    %00110000 ; |  XX    |  1
                BYTE    %00110000 ; |  XX    |  0

; vanish --------------------------------------------------------------

                DS.B    1, 0

B3_VANISH_HEIGHT = B3_Vanish_GRP_0_End - B3_Vanish_GRP_0

B3_Vanish_GRP_0:

                BYTE    %01001001 ; | X  X  X|  12
                BYTE    %01001001 ; | X  X  X|  11
                BYTE    %00101010 ; |  X X X |  10
                BYTE    %00101010 ; |  X X X |   9
                BYTE    %00010100 ; |   X X  |   8
                BYTE    %00000000 ; |        |   7
                BYTE    %01101011 ; | XX X XX|   6
                BYTE    %00000000 ; |        |   5
                BYTE    %00010100 ; |   X X  |   4
                BYTE    %00101010 ; |  X X X |   3
                BYTE    %00101010 ; |  X X X |   2
                BYTE    %01001001 ; | X  X  X|   1
                BYTE    %01001001 ; | X  X  X|   0

B3_Vanish_GRP_0_End:

                DS.B    11, 0

B3_Vanish_GRP_1 = B3_Vanish_GRP_0

B3_Vanish_COL_0:

                BYTE    COL_0E ; |ECECEC|  12
                BYTE    COL_82 ; |181AA7|  11
                BYTE    COL_AA ; |65B7D9|  10
                BYTE    COL_AA ; |65B7D9|   9
                BYTE    COL_AA ; |65B7D9|   8
                BYTE    COL_00 ; |000000|   7
                BYTE    COL_0E ; |ECECEC|   6
                BYTE    COL_00 ; |000000|   5
                BYTE    COL_AA ; |65B7D9|   4
                BYTE    COL_AA ; |65B7D9|   3
                BYTE    COL_AA ; |65B7D9|   2
                BYTE    COL_82 ; |181AA7|   1
                BYTE    COL_0E ; |ECECEC|   0

                DS.B    11, 0

B3_Vanish_COL_1 = B3_Vanish_COL_0

; ---------------------------------------------------------------------

                ALIGN   $0100

B3_GirderCOL:   ;  0
                BYTE    COL_40 ; |940000|   7
                BYTE    COL_56 ; |B846A2|   6
                BYTE    COL_56 ; |B846A2|   5
                BYTE    COL_56 ; |B846A2|   4
                BYTE    COL_56 ; |B846A2|   3
                BYTE    COL_56 ; |B846A2|   2
                BYTE    COL_40 ; |940000|   1
                BYTE    COL_56 ; |B846A2|   0
                ;  1
                BYTE    COL_40 ; |940000|   7
                BYTE    COL_56 ; |B846A2|   6
                BYTE    COL_56 ; |B846A2|   5
                BYTE    COL_56 ; |B846A2|   4
                BYTE    COL_56 ; |B846A2|   3
                BYTE    COL_56 ; |B846A2|   2
                BYTE    COL_40 ; |940000|   1
                BYTE    COL_56 ; |B846A2|   0
                ;  2
                BYTE    COL_40 ; |940000|   7
                BYTE    COL_56 ; |B846A2|   6
                BYTE    COL_56 ; |B846A2|   5
                BYTE    COL_56 ; |B846A2|   4
                BYTE    COL_56 ; |B846A2|   3
                BYTE    COL_56 ; |B846A2|   2
                BYTE    COL_40 ; |940000|   1
                BYTE    COL_56 ; |B846A2|   0
                ;  3
                BYTE    COL_40 ; |940000|   7
                BYTE    COL_56 ; |B846A2|   6
                BYTE    COL_56 ; |B846A2|   5
                BYTE    COL_56 ; |B846A2|   4
                BYTE    COL_56 ; |B846A2|   3
                BYTE    COL_56 ; |B846A2|   2
                BYTE    COL_40 ; |940000|   1
                BYTE    COL_56 ; |B846A2|   0
                ;  4
                BYTE    COL_40 ; |940000|   7
                BYTE    COL_56 ; |B846A2|   6
                BYTE    COL_56 ; |B846A2|   5
                BYTE    COL_56 ; |B846A2|   4
                BYTE    COL_56 ; |B846A2|   3
                BYTE    COL_56 ; |B846A2|   2
                BYTE    COL_40 ; |940000|   1
                BYTE    COL_56 ; |B846A2|   0
                ;  5
                BYTE    COL_40 ; |940000|   7
                BYTE    COL_56 ; |B846A2|   6
                BYTE    COL_56 ; |B846A2|   5
                BYTE    COL_56 ; |B846A2|   4
                BYTE    COL_56 ; |B846A2|   3
                BYTE    COL_56 ; |B846A2|   2
                BYTE    COL_40 ; |940000|   1
                BYTE    COL_56 ; |B846A2|   0
                ;  6
                BYTE    COL_40 ; |940000|   7
                BYTE    COL_56 ; |B846A2|   6
                BYTE    COL_56 ; |B846A2|   5
                BYTE    COL_56 ; |B846A2|   4
                BYTE    COL_56 ; |B846A2|   3
                BYTE    COL_56 ; |B846A2|   2
                BYTE    COL_40 ; |940000|   1
                BYTE    COL_56 ; |B846A2|   0
                ;  7
                BYTE    COL_40 ; |940000|   7
                BYTE    COL_56 ; |B846A2|   6
                BYTE    COL_56 ; |B846A2|   5
                BYTE    COL_56 ; |B846A2|   4
                BYTE    COL_56 ; |B846A2|   3
                BYTE    COL_56 ; |B846A2|   2
                BYTE    COL_40 ; |940000|   1
                BYTE    COL_56 ; |B846A2|   0
                ;  8
                BYTE    COL_40 ; |940000|   7
                BYTE    COL_56 ; |B846A2|   6
                BYTE    COL_56 ; |B846A2|   5
                BYTE    COL_56 ; |B846A2|   4
                BYTE    COL_56 ; |B846A2|   3
                BYTE    COL_56 ; |B846A2|   2
                BYTE    COL_40 ; |940000|   1
                BYTE    COL_56 ; |B846A2|   0

                ALIGN   $0100

B3_FloorPF1_L:  ;  0
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
              ; BYTE    %00000001 ;  0
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
              ; BYTE    %00000001 ;  0
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
                BYTE    %00110110 ; 22
                BYTE    %00000000 ; 21
                BYTE    %00110110 ; 20
                BYTE    %00110110 ; 19
                BYTE    %00110110 ; 18
                BYTE    %00110110 ; 17
                BYTE    %00110110 ; 16
                BYTE    %00110110 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00110110 ; 13
                BYTE    %00000000 ; 12
                BYTE    %00110110 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00110110 ;  9
                BYTE    %00110110 ;  8
                BYTE    %00110110 ;  7
                BYTE    %00110110 ;  6
                BYTE    %00110110 ;  5
                BYTE    %00110110 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00110110 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00000000 ;  0
                ;  6
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000011 ; 21
                BYTE    %00000011 ; 20
                BYTE    %00000011 ; 19
                BYTE    %00000011 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00000011 ; 16
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
                ;  9
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
                BYTE    %00000011 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000011 ;  5
                BYTE    %00000011 ;  4
                BYTE    %00000011 ;  3
                BYTE    %00000011 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00000000 ;  0
                ; 10
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

                ALIGN   $0100

B3_FloorPF2_L:  ;  0
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
                BYTE    %00000000 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00000000 ;  8
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
                BYTE    %01000000 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %01000000 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %01000000 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %01000000 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %01000000 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %01000000 ;  0
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
                BYTE    %00000000 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00000000 ;  8
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
                ;  4
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000000 ; 21
                BYTE    %00100000 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00100000 ; 16
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
                BYTE    %00100000 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00100000 ;  0
                ;  5
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
                ;  6
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %11000000 ; 21
                BYTE    %11000000 ; 20
                BYTE    %11000000 ; 19
                BYTE    %11000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %11000000 ; 16
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
                BYTE    %00010100 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00010100 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %00010100 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00010100 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00010100 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00010100 ;  0
                ;  8
                BYTE    %00000000 ; 23
                BYTE    %00000000 ; 22
                BYTE    %00000000 ; 21
                BYTE    %00010100 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00010100 ; 16
                BYTE    %00000000 ; 15
                BYTE    %00000000 ; 14
                BYTE    %00000000 ; 13
                BYTE    %00010100 ; 12
                BYTE    %00000000 ; 11
                BYTE    %00000000 ; 10
                BYTE    %00000000 ;  9
                BYTE    %00010100 ;  8
                BYTE    %00000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %00000000 ;  5
                BYTE    %00010100 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00010100 ;  0
                ;  9
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
                BYTE    %11000000 ;  7
                BYTE    %00000000 ;  6
                BYTE    %11000000 ;  5
                BYTE    %11000000 ;  4
                BYTE    %11000000 ;  3
                BYTE    %11000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00000000 ;  0
                ; 10
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

                ALIGN   $0100

B3_FloorPF2_R:  ;  0
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
                BYTE    %00000001 ; 20
                BYTE    %00000000 ; 19
                BYTE    %00000000 ; 18
                BYTE    %00000000 ; 17
                BYTE    %00000001 ; 16
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
                BYTE    %00000001 ;  4
                BYTE    %00000000 ;  3
                BYTE    %00000000 ;  2
              ; BYTE    %00000000 ;  1
              ; BYTE    %00000001 ;  0
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
                ;  8
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
                ;  9
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
                ; 10
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

                ALIGN   $0100

B3_FloorPF1_R:  ;  0
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
                ;  1
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
                ;  2
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
                ;  3
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
                ;  4
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
                ;  5
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
                ;  8
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
                ;  9
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
                ; 10
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
; Enemy GRP (part 2)
;
                ALIGN   $0100

; barrel -------------------------------------------------------------

                GRP_PADDING

                ; roll 0

B3_BARREL_ROLL_0_HEIGHT = B3_BarrelRoll0_GRP_0_End - B3_BarrelRoll0_GRP_0

B3_BarrelRoll0_GRP_0:

                BYTE    %00111100 ; |  XXXX  |  10
                BYTE    %01111110 ; | XXXXXX |   9
                BYTE    %01111110 ; | XXXXXX |   8
                BYTE    %11110111 ; |XXXX XXX|   7
                BYTE    %11101111 ; |XXX XXXX|   6
                BYTE    %11011111 ; |XX XXXXX|   5
                BYTE    %11111011 ; |XXXXX XX|   4
                BYTE    %11111011 ; |XXXXX XX|   3
                BYTE    %01111110 ; | XXXXXX |   2
                BYTE    %01111110 ; | XXXXXX |   1
                BYTE    %00111100 ; |  XXXX  |   0

B3_BarrelRoll0_GRP_0_End:

                GRP_PADDING

B3_BarrelRoll0_GRP_1:

                BYTE    %00000000 ; |        |  10
                BYTE    %00111100 ; |  XXXX  |   9
                BYTE    %00111100 ; |  XXXX  |   8
                BYTE    %01111110 ; | XXXXXX |   7
                BYTE    %01111110 ; | XXXXXX |   6
                BYTE    %01111110 ; | XXXXXX |   5
                BYTE    %01111110 ; | XXXXXX |   4
                BYTE    %01111110 ; | XXXXXX |   3
                BYTE    %00111100 ; |  XXXX  |   2
                BYTE    %00111100 ; |  XXXX  |   1
                BYTE    %00000000 ; |        |   0

                GRP_PADDING

                ; roll 1

B3_BARREL_ROLL_1_HEIGHT = B3_BARREL_ROLL_0_HEIGHT

B3_BarrelRoll1_GRP_0:

                BYTE    %00111100 ; |  XXXX  |  10
                BYTE    %01111110 ; | XXXXXX |   9
                BYTE    %01111110 ; | XXXXXX |   8
                BYTE    %11111011 ; |XXXXX XX|   7
                BYTE    %11111011 ; |XXXXX XX|   6
                BYTE    %11011111 ; |XX XXXXX|   5
                BYTE    %11101111 ; |XXX XXXX|   4
                BYTE    %11110111 ; |XXXX XXX|   3
                BYTE    %01111110 ; | XXXXXX |   2
                BYTE    %01111110 ; | XXXXXX |   1
                BYTE    %00111100 ; |  XXXX  |   0

B3_BarrelRoll1_GRP_1 = B3_BarrelRoll0_GRP_1

                GRP_PADDING

                ; fall 0

B3_BARREL_FALL_0_HEIGHT = B3_BarrelFall0_GRP_0_End - B3_BarrelFall0_GRP_0

B3_BarrelFall0_GRP_0:

                BYTE    %00111100 ; |  XXXX  |   9
                BYTE    %00111100 ; |  XXXX  |   8
                BYTE    %10111101 ; |X XXXX X|   7
                BYTE    %10111101 ; |X XXXX X|   6
                BYTE    %10111101 ; |X XXXX X|   5
                BYTE    %10111101 ; |X XXXX X|   4
                BYTE    %10111101 ; |X XXXX X|   3
                BYTE    %10111101 ; |X XXXX X|   2
                BYTE    %00111100 ; |  XXXX  |   1
                BYTE    %00111100 ; |  XXXX  |   0

B3_BarrelFall0_GRP_0_End:

                GRP_PADDING

B3_BarrelFall0_GRP_1:

                BYTE    %00111100 ; |  XXXX  |   9
                BYTE    %01100110 ; | XX  XX |   8
                BYTE    %11000011 ; |XX    XX|   7
                BYTE    %01111110 ; | XXXXXX |   6
                BYTE    %11111111 ; |XXXXXXXX|   5
                BYTE    %11111111 ; |XXXXXXXX|   4
                BYTE    %01111110 ; | XXXXXX |   3
                BYTE    %11000011 ; |XX    XX|   2
                BYTE    %01100110 ; | XX  XX |   1
                BYTE    %00111100 ; |  XXXX  |   0

                GRP_PADDING

                ; fall 1

B3_BARREL_FALL_1_HEIGHT = B3_BARREL_FALL_0_HEIGHT

B3_BarrelFall1_GRP_0 = B3_BarrelFall0_GRP_0

B3_BarrelFall1_GRP_1:

                BYTE    %00000000 ; |        |   9
                BYTE    %01011010 ; | X XX X |   8
                BYTE    %01111110 ; | XXXXXX |   7
                BYTE    %11000011 ; |XX    XX|   6
                BYTE    %01000010 ; | X    X |   5
                BYTE    %01000010 ; | X    X |   4
                BYTE    %11000011 ; |XX    XX|   3
                BYTE    %01111110 ; | XXXXXX |   2
                BYTE    %01011010 ; | X XX X |   1
                BYTE    %00000000 ; |        |   0

                GRP_PADDING

; jack ---------------------------------------------------------------

                ; Jack 0

B3_JACK_0_HEIGHT = B3_Jack0_GRP_0_End - B3_Jack0_GRP_0

B3_Jack0_GRP_0:

                BYTE    %11111110 ; |XXXXXXX |   5
                BYTE    %01111100 ; | XXXXX  |   4
                BYTE    %00010000 ; |   X    |   3
                BYTE    %00010000 ; |   X    |   2
                BYTE    %01111100 ; | XXXXX  |   1
                BYTE    %11111110 ; |XXXXXXX |   0

B3_Jack0_GRP_0_End:

B3_Jack0_GRP_1 = B3_Jack0_GRP_0

                ALIGN   $0100

                GRP_PADDING

                ; Jack 1

B3_JACK_1_HEIGHT = B3_Jack1_GRP_0_End - B3_Jack1_GRP_0

B3_Jack1_GRP_0:

                BYTE    %11111110 ; |XXXXXXX |  10
                BYTE    %01111100 ; | XXXXX  |   9
                BYTE    %00010000 ; |   X    |   8
                BYTE    %00101000 ; |  X X   |   7
                BYTE    %01010100 ; | X X X  |   6
                BYTE    %10010010 ; |X  X  X |   5
                BYTE    %01010100 ; | X X X  |   4
                BYTE    %00101000 ; |  X X   |   3
                BYTE    %00010000 ; |   X    |   2
                BYTE    %01111100 ; | XXXXX  |   1
                BYTE    %11111110 ; |XXXXXXX |   0

B3_Jack1_GRP_0_End:

B3_Jack1_GRP_1 = B3_Jack1_GRP_0

                GRP_PADDING

                ; Jack 2

B3_JACK_2_HEIGHT = B3_Jack2_GRP_0_End - B3_Jack2_GRP_0

B3_Jack2_GRP_0:

                BYTE    %11111110 ; |XXXXXXX |  14
                BYTE    %01111100 ; | XXXXX  |  13
                BYTE    %00010000 ; |   X    |  12
                BYTE    %00101000 ; |  X X   |  11
                BYTE    %00111000 ; |  XXX   |  10
                BYTE    %01010100 ; | X X X  |   9
                BYTE    %01010100 ; | X X X  |   8
                BYTE    %10010010 ; |X  X  X |   7
                BYTE    %01010100 ; | X X X  |   6
                BYTE    %01010100 ; | X X X  |   5
                BYTE    %00111000 ; |  XXX   |   4
                BYTE    %00101000 ; |  X X   |   3
                BYTE    %00010000 ; |   X    |   2
                BYTE    %01111100 ; | XXXXX  |   1
                BYTE    %11111110 ; |XXXXXXX |   0

B3_Jack2_GRP_0_End:

B3_Jack2_GRP_1 = B3_Jack2_GRP_0

                DS.B    8, 0
                GRP_PADDING

; elevator -----------------------------------------------------------

B3_ELEVATOR_HEIGHT = B3_Elevator_GRP_0_End - B3_Elevator_GRP_0

B3_Elevator_GRP_0:

                BYTE    %11111111 ; |XXXXXXXX|   7
                BYTE    %11111111 ; |XXXXXXXX|   6
                BYTE    %10001000 ; |X   X   |   5
                BYTE    %11011101 ; |XX XXX X|   4
                BYTE    %01110111 ; | XXX XXX|   3
                BYTE    %00100010 ; |  X   X |   2
                BYTE    %11111111 ; |XXXXXXXX|   1
                BYTE    %11111111 ; |XXXXXXXX|   0

B3_Elevator_GRP_0_End:

B3_Elevator_GRP_1 = B3_Elevator_GRP_0

                GRP_PADDING

; oil ----------------------------------------------------------------

                ; Oil (single entry in animation table)

B3_OIL_HEIGHT = B3_Oil_GRP_1 - B3_Oil_GRP_0

B3_Oil_GRP_0 = B3_Oil0_GRP_0
B3_Oil_GRP_1 = B3_Oil0_GRP_1

                ; Oil 0

B3_OIL_0_HEIGHT = B3_Oil_GRP_1 - B3_Oil_GRP_0

B3_Oil0_GRP_0:

                BYTE    %01111111 ; | XXXXXXX|  22
                BYTE    %00111110 ; |  XXXXX |  21
                BYTE    %10100101 ; |X X  X X|  20
                BYTE    %10111111 ; |X XXXXXX|  19
                BYTE    %10111111 ; |X XXXXXX|  18
                BYTE    %11111111 ; |XXXXXXXX|  17
                BYTE    %00010100 ; |   X X  |  16
                BYTE    %01010101 ; | X X X X|  15
                BYTE    %00010101 ; |   X X X|  14
                BYTE    %11111111 ; |XXXXXXXX|  13
                BYTE    %10111111 ; |X XXXXXX|  12
                BYTE    %10111111 ; |X XXXXXX|  11
                BYTE    %00111110 ; |  XXXXX |  10
                BYTE    %01111111 ; | XXXXXXX|   9
                BYTE    %01011010 ; | X XX X |   8
                BYTE    %01011010 ; | X XX X |   7
                BYTE    %00001000 ; |    X   |   6
                BYTE    %10001100 ; |X   XX  |   5
                BYTE    %00001000 ; |    X   |   4
                BYTE    %00101000 ; |  X X   |   3
              ; BYTE    %00000000 ; |        |   2 ; should get this line back
                BYTE    %00000010 ; |      X |   1
                BYTE    %00000101 ; |     X X|   0

                DS.B    2, 0

B3_Oil0_GRP_1:

                BYTE    %10000000 ; |X       |  22
                BYTE    %01000000 ; | X      |  21
                BYTE    %01011010 ; | X XX X |  20
                BYTE    %01000000 ; | X      |  19
                BYTE    %01000000 ; | X      |  18
                BYTE    %00000000 ; |        |  17
                BYTE    %11101011 ; |XXX X XX|  16
                BYTE    %10101010 ; |X X X X |  15
                BYTE    %11101010 ; |XXX X X |  14
                BYTE    %00000000 ; |        |  13
                BYTE    %01000000 ; | X      |  12
                BYTE    %01000000 ; | X      |  11
                BYTE    %01000000 ; | X      |  10
                BYTE    %10000000 ; |X       |   9
                BYTE    %01110110 ; | XXX XX |   8
                BYTE    %10110101 ; |X XX X X|   7
                BYTE    %11011110 ; |XX XXXX |   6
                BYTE    %01010010 ; | X X  X |   5
                BYTE    %01000101 ; | X   X X|   4
                BYTE    %00000101 ; |     X X|   3
              ; BYTE    %00010101 ; |   X X X|   2 ; should get this line back
                BYTE    %00100010 ; |  X   X |   1
                BYTE    %00010100 ; |   X X  |   0

                DS.B    2, 0

                ; Oil 1

B3_OIL_1_HEIGHT = B3_Oil1_GRP_1 - B3_Oil1_GRP_0

B3_Oil1_GRP_0:

                BYTE    %01111111 ; | XXXXXXX|  22
                BYTE    %00111110 ; |  XXXXX |  21
                BYTE    %10100101 ; |X X  X X|  20
                BYTE    %10111111 ; |X XXXXXX|  19
                BYTE    %10111111 ; |X XXXXXX|  18
                BYTE    %11111111 ; |XXXXXXXX|  17
                BYTE    %00010100 ; |   X X  |  16
                BYTE    %01010101 ; | X X X X|  15
                BYTE    %00010101 ; |   X X X|  14
                BYTE    %11111111 ; |XXXXXXXX|  13
                BYTE    %10111111 ; |X XXXXXX|  12
                BYTE    %10111111 ; |X XXXXXX|  11
                BYTE    %00111110 ; |  XXXXX |  10
                BYTE    %01111111 ; | XXXXXXX|   9
                BYTE    %01011010 ; | X XX X |   8
                BYTE    %11011110 ; |XX XXXX |   7
                BYTE    %00001011 ; |    X XX|   6
                BYTE    %00100101 ; |  X  X X|   5
                BYTE    %00000010 ; |      X |   4
                BYTE    %00000100 ; |     X  |   3
              ; BYTE    %01001010 ; | X  X X |   2
                BYTE    %00000001 ; |       X|   1
                BYTE    %00010000 ; |   X    |   0

                DS.B    2, 0

B3_Oil1_GRP_1:

                BYTE    %10000000 ; |X       |  22
                BYTE    %01000000 ; | X      |  21
                BYTE    %01011010 ; | X XX X |  20
                BYTE    %01000000 ; | X      |  19
                BYTE    %01000000 ; | X      |  18
                BYTE    %00000000 ; |        |  17
                BYTE    %11101011 ; |XXX X XX|  16
                BYTE    %10101010 ; |X X X X |  15
                BYTE    %11101010 ; |XXX X X |  14
                BYTE    %00000000 ; |        |  13
                BYTE    %01000000 ; | X      |  12
                BYTE    %01000000 ; | X      |  11
                BYTE    %01000000 ; | X      |  10
                BYTE    %10000000 ; |X       |   9
                BYTE    %10101100 ; |X X XX  |   8
                BYTE    %00111011 ; |  XXX XX|   7
                BYTE    %01010010 ; | X X  X |   6
                BYTE    %00111010 ; |  XXX X |   5
                BYTE    %00001000 ; |    X   |   4
                BYTE    %00100010 ; |  X   X |   3
              ; BYTE    %00000000 ; |        |   2
                BYTE    %00000100 ; |     X  |   1
                BYTE    %00010000 ; |   X    |   0

                DS.B    2, 0

                ALIGN   $0100

; fireball -----------------------------------------------------------

                GRP_PADDING

B3_FIREBALL_1_HEIGHT = B3_Fireball1_GRP_0_End - B3_Fireball1_GRP_0

Fireball1_GRP:
B3_Fireball1_GRP_0:

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

B3_Fireball1_GRP_0_End:

                GRP_PADDING

B3_Fireball1_GRP_1:

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

                GRP_PADDING

;
; Enemy COL
;

; jack ---------------------------------------------------------------

B3_Jack0_COL_0:

                DS.B    24+1, COL_8E ; |8490FC|

B3_Jack0_COL_1 = B3_Jack0_COL_0

B3_Jack1_COL_0 = B3_Jack0_COL_0
B3_Jack1_COL_1 = B3_Jack0_COL_0

B3_Jack2_COL_0 = B3_Jack0_COL_0
B3_Jack2_COL_1 = B3_Jack0_COL_0

; elevator -----------------------------------------------------------

B3_Elevator_COL_0:

                DS.B    24+8, COL_54 ; |A8308F|

B3_Elevator_COL_1 = B3_Elevator_COL_0

; barrel -------------------------------------------------------------

B3_BarrelRoll0_COL_0:

                DS.B    24, BARREL_COL_0

B3_BarrelRoll0_COL_1:

                DS.B    24, BARREL_COL_1

B3_BarrelRoll1_COL_0 = B3_BarrelRoll0_COL_0
B3_BarrelRoll1_COL_1 = B3_BarrelRoll0_COL_1

B3_BarrelFall0_COL_0 = B3_BarrelRoll0_COL_0
B3_BarrelFall0_COL_1 = B3_BarrelRoll0_COL_1

B3_BarrelFall1_COL_0 = B3_BarrelRoll0_COL_0
B3_BarrelFall1_COL_1 = B3_BarrelRoll0_COL_1

; oil ----------------------------------------------------------------

B3_Oil_COL_0:

                BYTE    COL_82     ; |181AA7|  22
                BYTE    COL_82     ; |181AA7|  21
                BYTE    COL_82     ; |181AA7|  20
                BYTE    COL_82     ; |181AA7|  19
                BYTE    COL_AA     ; |65B7D9|  18
                BYTE    COL_82     ; |181AA7|  17
                BYTE    COL_82     ; |181AA7|  16
                BYTE    COL_82     ; |181AA7|  15
                BYTE    COL_82     ; |181AA7|  14
                BYTE    COL_82     ; |181AA7|  13
                BYTE    COL_AA     ; |65B7D9|  12
                BYTE    COL_82     ; |181AA7|  11
                BYTE    COL_82     ; |181AA7|  10
                BYTE    COL_82     ; |181AA7|   9
                BYTE    FIRE_COL_0 ;            8
                BYTE    FIRE_COL_0 ;            7
                BYTE    FIRE_COL_0 ;            6
                BYTE    FIRE_COL_0 ;            5
                BYTE    FIRE_COL_0 ;            4
                BYTE    FIRE_COL_0 ;            3
                BYTE    FIRE_COL_0 ;            2
                BYTE    FIRE_COL_0 ;            1
                BYTE    FIRE_COL_0 ;            0

B3_Oil_COL_1:

                BYTE    COL_0E     ; |ECECEC|  22
                BYTE    COL_0E     ; |ECECEC|  21
                BYTE    COL_0E     ; |ECECEC|  20
                BYTE    COL_0E     ; |ECECEC|  19
                BYTE    COL_0E     ; |ECECEC|  18
                BYTE    COL_00     ; |000000|  17
                BYTE    COL_AA     ; |65B7D9|  16
                BYTE    COL_AA     ; |65B7D9|  15
                BYTE    COL_AA     ; |65B7D9|  14
                BYTE    COL_00     ; |000000|  13
                BYTE    COL_0E     ; |ECECEC|  12
                BYTE    COL_0E     ; |ECECEC|  11
                BYTE    COL_0E     ; |ECECEC|  10
                BYTE    COL_0E     ; |ECECEC|   9
                BYTE    FIRE_COL_1 ;            8
                BYTE    FIRE_COL_1 ;            7
                BYTE    FIRE_COL_1 ;            6
                BYTE    FIRE_COL_1 ;            5
                BYTE    FIRE_COL_1 ;            4
                BYTE    FIRE_COL_1 ;            3
                BYTE    FIRE_COL_1 ;            2
                BYTE    FIRE_COL_1 ;            1
                BYTE    FIRE_COL_1 ;            0

; fireball & hammer --------------------------------------------------

                DS.B    3+24,0 ; ensure Fireball address matches bank 4

Hammer1_COL_0:
B3_Hammer1_COL_0:

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
B3_Fireball0_COL_0:
B3_Hammer0_COL_0:

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
B3_Fireball0_COL_1:
B3_Hammer0_COL_1:

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
B3_Hammer1_COL_1:

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

B3_Fireball1_COL_0 = B3_Fireball0_COL_0
B3_Fireball1_COL_1 = B3_Fireball0_COL_1

; ********************************************************************

#if PRINT_MESSAGES
                ECHO "---- Bank 3: ", (ROMJumpTable - *), "bytes of ROM left"
#endif
