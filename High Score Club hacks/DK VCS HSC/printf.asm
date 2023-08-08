
; ********************************************************************
;  printf
;
;    Stella / ATARI VCS 2600 printf support macros
;
;    $Date: Thu, 05 Jan 2017 20:55:15 +0100 $
;    $Author: dietrich $
;    $Revision: 477 $
;
;  Copyright (C) 2017 Andreas Dietrich
; ********************************************************************

; ********************************************************************
;
;       Pseudo Instruction Macros
;
; ********************************************************************

        MAC PRINTF_POC
                BYTE    $00,$00 ; BRK,0
        ENDM

        MAC PRINTF0
                PRINTF_POC      ; printf pseudo op-code
                BYTE    {1},0   ; 0-terminated format string
                BYTE    0       ; number of addresses
        ENDM

        MAC PRINTF1
                PRINTF_POC      ; printf pseudo op-code
                BYTE    {1},0   ; 0-terminated format string
                BYTE    1       ; number of addresses
                WORD    {2}     ; address #1
        ENDM

        MAC PRINTF2
                PRINTF_POC      ; printf pseudo op-code
                BYTE    {1},0   ; 0-terminated format string
                BYTE    2       ; number of addresses
                WORD    {2}     ; address #1
                WORD    {3}     ; address #2
        ENDM

        MAC PRINTF3
                PRINTF_POC      ; printf pseudo op-code
                BYTE    {1},0   ; 0-terminated format string
                BYTE    3       ; number of addresses
                WORD    {2}     ; address #1
                WORD    {3}     ; address #2
                WORD    {4}     ; address #3
        ENDM

        MAC PRINTF4
                PRINTF_POC      ; printf pseudo op-code
                BYTE    {1},0   ; 0-terminated format string
                BYTE    4       ; number of addresses
                WORD    {2}     ; address #1
                WORD    {3}     ; address #2
                WORD    {4}     ; address #3
                WORD    {5}     ; address #4
        ENDM
