
; ********************************************************************
;  Donkey Kong VCS
;
;    Main source file
;
;    $Date: Sun, 05 Mar 2017 19:54:20 +0100 $
;    $Author: dietrich $
;    $Revision: 487 $
;
;  Copyright (C) 2017 Andreas Dietrich
; ********************************************************************

                PROCESSOR 6502

VERSION                  SET "1.0"
TV_SYSTEM                SET "NTSC"
ROM_TYPE                 SET "STELLA"
PRINT_MESSAGES           SET 1
CHEAT_FREEZE_BONUS_TIMER SET 0
CHEAT_INFINITE_LIVES     SET 0
CHEAT_INVINCIBILITY      SET 0
DEBUG_DONKEY_KONG_VCS    SET 0
DEBUG_INTRO              SET 0
DEBUG_INTERMISSION       SET 0
DEBUG_ENDING             SET 0

; --------------------------------------------------------------------
;       Includes
; --------------------------------------------------------------------

                INCLUDE vcs.h
                INCLUDE libvcs.asm
                INCLUDE printf.asm
                INCLUDE font.asm
                INCLUDE colors.asm

; --------------------------------------------------------------------
;       Global Constants
; --------------------------------------------------------------------

; address equates

ROMStart        = $F000
ROMSize         = $1000

RAMBase         = $80
RAMTop          = $FF

SCRAMWrite      = ROMStart + 0 * $80
SCRAMRead       = ROMStart + 1 * $80
CodeStart       = ROMStart + 2 * $80

Bank0Start      = $0000
Bank1Start      = $1000
Bank2Start      = $2000
Bank3Start      = $3000
Bank4Start      = $4000
Bank5Start      = $5000
Bank6Start      = $6000
Bank7Start      = $7000

; game constants

#if TV_SYSTEM == "NTSC"
KONG_COL_0      = $28           ; Kong plane 0 color
KONG_COL_1      = $30           ; Kong plane 1 color
BARREL_COL_0    = $3E           ; Barrel plane 0 color
BARREL_COL_1    = $32           ; Barrel plane 1 color
#else
KONG_COL_0      = $46           ; Kong plane 0 color
KONG_COL_1      = $42           ; Kong plane 1 color
BARREL_COL_0    = $48           ; Barrel plane 0 color
BARREL_COL_1    = $46           ; Barrel plane 1 color
#endif

FIRE_COL_0      = COL_FE        ; Fire plane 0 color
FIRE_COL_1      = COL_44        ; Fire plane 1 color

; DK arcade main colors

DK_COL_BLACK    = COL_00
DK_COL_BLUE     = COL_82
DK_COL_BROWN    = COL_3E
DK_COL_CYAN     = COL_AE
DK_COL_MAGENTA  = COL_56
DK_COL_ORANGE   = COL_36
DK_COL_PINK     = COL_5A
DK_COL_RED      = COL_42
DK_COL_WHITE    = COL_0E
DK_COL_YELLOW   = COL_2A

;---------------------------------------------------------------------
;       Global RAM Layout
;---------------------------------------------------------------------

; standard global variables

Bank            = $80
LineCtr         = $81
EventCtr        = $82
FrameCtr        = $83
Random          = $84
Temp            = $85
Ptr             = $86 ; [2]

; game state

SWCHB_JAPUS_MASK = %00000100

LEVEL_MASK = %11110000
ROUND_MASK = %00001111
STAGE_MASK = %00000011
DEATH_MASK = %10000000
EXTRA_MASK = %01000000
LIVES_MASK = %00111111

ROUND_NEW = %1111
ROUND_NOP = %0111
ROUND_0S1 = %0000
ROUND_1S1 = %0100
ROUND_2S1 = %1000
ROUND_0S2 = %0001
ROUND_0S3 = %0010
ROUND_0S4 = %0011

Level_Round     = $88 ;       level: [%XXXX----], round: [%----XXXX] ( stage: [%------XX] )
Death_Lives     = $89 ;       Mario death: [%X-------], extra life: [%-X------], lives: [%--XXXXXX]
Bonus           = $8A ;       upper two timer digits [$XX]00
Score           = $8B ; [3]   six-digit BCD score [$XX][$XX][$XX]
ScorePtr        = $8E ; [12]  digit graphics pointer

; kernel specific variables

                ; $9A - $F1

; sound player (same as music player)

FXType          = $F2 ; [2]
FXFreq          = $F4 ; [2]
FXVol           = $F6 ; [2]
FXTimer         = $F8 ; [2]

; music player (same as sound player)

NPTrackPtrL     = $F2 ; [2]
NPTrackPtrH     = $F4 ; [2]
NPNote          = $F6 ; [2]
NPTimer         = $F8 ; [2]

; stack (3 levels)

StackBottom     = $FA
StackTop        = $FF

; --------------------------------------------------------------------
;       Bank layout
; --------------------------------------------------------------------
;
; F4SC -- 32k bank switching scheme + 'Super Chip' RAM
;

#ifconst ROM_EXTERN
#if ROM_EXTERN == 1
ROM_TYPE SET "CART"
#endif
#endif

ROMJumpTableSize = (7*9)+(7*6)+1
ROMJumpTable     = $FFF4 - ROMJumpTableSize

        MAC BANK

                ; bank content ---------------------------------------

                ORG {1}
                RORG ROMStart

                ; RAM ; $F000
                IF (ROM_TYPE != "CART") || ({3} == 0)
                        DS.B 128,0 ; write area
                        DS.B 128,0 ; read area
                ELSE
                        ; only use if bank switching not auto-detected
                        INCLUDE {3}
                ENDIF

                ; ROM ; $F100
                cmp     SELECT_BANK_7
                INCLUDE {2}

                ; common area ----------------------------------------

                ORG  {1} + ( ROMJumpTable - ROMStart )
                RORG ROMJumpTable

                ; jump table

.CommonCodeStart
Bank0_DonkeyKongVCS
; 6 bytes
                cmp     SELECT_BANK_0
                jmp     DonkeyKongVCS

Bank1_Intro
; 6 bytes
                cmp     SELECT_BANK_1
                jmp     Intro

Bank7_Intermission
; 6 bytes
                cmp     SELECT_BANK_7
                jmp     Intermission

Bank2_Stage
; 6 bytes
                cmp     SELECT_BANK_2
                jmp     Stage

Bank6_Ending
; 6 bytes
                cmp     SELECT_BANK_6
                jmp     Ending

Bank5_DriveAudio
; 9 bytes
                cmp     SELECT_BANK_5
                jsr     DriveAudio
                jmp     ReturnFromBank

Bank0_StageHandler
; 9 bytes
                cmp     SELECT_BANK_0
                jsr     StageHandler
                jmp     ReturnFromBank

Bank1_DrawKong
; 9 bytes
                cmp     SELECT_BANK_1
                jsr     B1_DrawKong
                jmp     ReturnFromBank

Bank5_DrawKong
; 9 bytes
                cmp     SELECT_BANK_5
                jsr     B5_DrawKong
                jmp     ReturnFromBank

Bank6_DrawKong
; 9 bytes
                cmp     SELECT_BANK_6
                jsr     B6_DrawKong
                jmp     ReturnFromBank

BankX_CopyMario
; 9 bytes
                cmp     SELECT_BANK_0,x
                jsr     CopyMario
                jmp     ReturnFromBank

BankX_DrawFloors
; 9 bytes
                cmp     SELECT_BANK_0,x
                jsr     DrawFloors
                jmp     ReturnFromBank

Bank7_DrawScore
; 6 bytes
                cmp     SELECT_BANK_7
                jsr     DrawScore

ReturnFromBank:
; 6 bytes
                ldx     Bank
                cmp     SELECT_BANK_0,x
                rts
; 1 byte
                nop     ; padding to prevent access of $FFF4
.CommonCodeEnd

                ; bank switching hotspots
; $FFF4
SELECT_BANK_0   BYTE    $F4
SELECT_BANK_1   BYTE    $F5
SELECT_BANK_2   BYTE    $F6
SELECT_BANK_3   BYTE    $F7
SELECT_BANK_4   BYTE    $F8
SELECT_BANK_5   BYTE    $F9
SELECT_BANK_6   BYTE    $FA ; same address as NMI vector
SELECT_BANK_7   BYTE    $FB ;

                ; CPU vectors
; $FFFC
Reset:          WORD    CodeStart ; $F100
IRQ:            WORD    CodeStart ; $F100

        ENDM

; --------------------------------------------------------------------
;       Macros
; --------------------------------------------------------------------

DEBUG_MODE = (DEBUG_DONKEY_KONG_VCS | DEBUG_INTRO | DEBUG_INTERMISSION | DEBUG_ENDING)

        MAC DEBUG

                IF {1}
                        {2}
                ENDIF

        ENDM

; --------------------------------------------------------------------

        MAC CHECK_RESET

                lda     SWCHB
                lsr
                bcs     .end
                jmp     Bank0_DonkeyKongVCS
.end
        ENDM

; --------------------------------------------------------------------

        MAC ALIGN_END

GapSize SET *

                ORG     {1} + ( ROMJumpTable - ROMStart ) - {2}
                RORG    ROMJumpTable                      - {2}

GapSize SET * - GapSize

        ENDM

; ********************************************************************
;
;       ROM Code
;
; ********************************************************************

                BANK Bank0Start, "bank_0.asm", 0
                BANK Bank1Start, "bank_1.asm", 0
                BANK Bank2Start, "bank_2.asm", 0
                BANK Bank3Start, "bank_3.asm", 0
                BANK Bank4Start, "bank_4.asm", 0
                BANK Bank5Start, "bank_5.asm", 0
                BANK Bank6Start, "bank_6.asm", 0
                BANK Bank7Start, "bank_7.asm", "ikegami.asm"

#if PRINT_MESSAGES
                ECHO "===================================================="
#endif

; --- Final Checklist ---
;
; [X] Check ALL page borders in all banks.
; [X] Check for SuperChip read/write collisions.
; [X] Check automatically generated colors.
; [X] Check NTSC/PAL colors.
; [X] Check music ADSR curves.
; [X] Check reset code in all kernels.
; [X] Check for consistent Mario dead.
