;-----------------------------------------------------------
; Kung-Fu Master PlusROM HSC: HSC table screen (bank 3)
;
; Enter: joystick left on the start/game over screen (only with a PlusROM device)
; Exit:  joystick right or RESET (cold start of the game)
;
; Request: WriteToBuffer 0, WriteSendBuffer HIGHSCORE_ID
; Answer:  3 lines with 24 chars (72 bytes), char codes of the 1942 bB font
;          (L1CHARS.., index * TEXTHEIGHT)
;
; The PlusCart needs some frames between reading the receive buffer and the next
; request: old bytes are removed from the receive buffer on entry, the request is
; sent HSC_REQUEST_DELAY frames later.
;
; Uses the 24 char text kernel of 1942 bB (by c-dw). Differences to 1942:
; - no SC-RAM: the table is stored in the RIOT RAM, the game is cold started
;   when leaving the screen
; - no TEXT buffer: TextCopy reads the chars via TPTR and writes BUFF1 with pha
; - the static rows use a second font with "-"
;-----------------------------------------------------------

; RAM while the HSC screen is shown
HSC_TABLE       = $80   ; 72  3 lines with 24 chars, received from the backend
BUFF1           = $c8   ; 42  bitmap of the current text line
TPTR            = $f2   ;  2  pointer to the chars of the current text line
CYCLE           = $f4   ;  1
LOOP            = $f5   ;  1
TIDX            = $f6   ;  1
HSC_WAIT        = $f7   ;  1  frames until the request is sent, 0 = sent
                        ;     stack $fe..$ff (1 jsr level)

HSC_REQUEST_DELAY = 16  ; frames between reading the receive buffer and the request

HSC_LINE_LEN    = 24
HSC_LINES       = 3
HSC_TABLE_SIZE  = HSC_LINE_LEN * HSC_LINES

; HSC screen colors
HSC_COL_TITLE   = YELLOW|$c
HSC_COL_TOP3    = RED|$6
HSC_COL_TABLE   = BLACK|$e

; Char codes (same as 1942 bB)
TEXTHEIGHT  = 4
_COMMA  = 0
_DOT    = 1
__      = (1 * TEXTHEIGHT)
_A      = (2 * TEXTHEIGHT)
_B      = (3 * TEXTHEIGHT)
_C      = (4 * TEXTHEIGHT)
_D      = (5 * TEXTHEIGHT)
_E      = (6 * TEXTHEIGHT)
_F      = (7 * TEXTHEIGHT)
_G      = (8 * TEXTHEIGHT)
_H      = (9 * TEXTHEIGHT)
_I      = (10 * TEXTHEIGHT)
_J      = (11 * TEXTHEIGHT)
_K      = (12 * TEXTHEIGHT)
_L      = (13 * TEXTHEIGHT)
_M      = (14 * TEXTHEIGHT)
_N      = (15 * TEXTHEIGHT)
_O      = (16 * TEXTHEIGHT)
_P      = (17 * TEXTHEIGHT)
_Q      = (18 * TEXTHEIGHT)
_R      = (19 * TEXTHEIGHT)
_S      = (20 * TEXTHEIGHT)
_T      = (21 * TEXTHEIGHT)
_U      = (22 * TEXTHEIGHT)
_V      = (23 * TEXTHEIGHT)
_W      = (24 * TEXTHEIGHT)
_X      = (25 * TEXTHEIGHT)
_Y      = (26 * TEXTHEIGHT)
_Z      = (27 * TEXTHEIGHT)
_a      = (28 * TEXTHEIGHT)
_b      = (29 * TEXTHEIGHT)
_c      = (30 * TEXTHEIGHT)
_d      = (31 * TEXTHEIGHT)
_e      = (32 * TEXTHEIGHT)
_f      = (33 * TEXTHEIGHT)
_g      = (34 * TEXTHEIGHT)
_h      = (35 * TEXTHEIGHT)
_i      = (36 * TEXTHEIGHT)
_j      = (37 * TEXTHEIGHT)
_k      = (38 * TEXTHEIGHT)
_l      = (39 * TEXTHEIGHT)
_m      = (40 * TEXTHEIGHT)
_n      = (41 * TEXTHEIGHT)
_o      = (42 * TEXTHEIGHT)
_p      = (43 * TEXTHEIGHT)
_q      = (44 * TEXTHEIGHT)
_r      = (45 * TEXTHEIGHT)
_s      = (46 * TEXTHEIGHT)
_t      = (47 * TEXTHEIGHT)
_u      = (48 * TEXTHEIGHT)
_v      = (49 * TEXTHEIGHT)
_w      = (50 * TEXTHEIGHT)
_x      = (51 * TEXTHEIGHT)
_y      = (52 * TEXTHEIGHT)
_z      = (53 * TEXTHEIGHT)
_0      = (54 * TEXTHEIGHT)
_1      = (55 * TEXTHEIGHT)
_2      = (56 * TEXTHEIGHT)
_3      = (57 * TEXTHEIGHT)
_4      = (58 * TEXTHEIGHT)
_5      = (59 * TEXTHEIGHT)
_6      = (60 * TEXTHEIGHT)
_7      = (61 * TEXTHEIGHT)
_8      = (62 * TEXTHEIGHT)
_9      = (63 * TEXTHEIGHT)
_DASH   = _q                    ; static font only!

; SLEEP macro (like macro.h with NO_ILLEGAL_OPCODES)
    MAC SLEEP
.CYCLES SET {1}
    IF .CYCLES & 1
      bit VSYNC
.CYCLES SET .CYCLES - 3
    ENDIF
    REPEAT .CYCLES / 2
      nop
    REPEND
    ENDM


;-----------------------------------------------------------
; Entry from HSCCheck (joystick left, PlusROM device present)
;-----------------------------------------------------------
EnterHSCScreen
    SUBROUTINE
    ldx     #0
    txa
.clear
    sta     $00,x                   ; clear TIA and RAM, the game is cold started later
    inx
    bne     .clear
    dex
    txs
    inx                             ; x = 0, max. 256 bytes
.drainOld
    lda     ReceiveBufferSize       ; remove old bytes from the receive buffer
    beq     .empty
    lda     ReceiveBuffer
    dex
    bne     .drainOld
.empty
    lda     #HSC_REQUEST_DELAY      ; send the request later
    sta     HSC_WAIT
    lda     #__
    ldx     #HSC_TABLE_SIZE
.clrTable
    sta     HSC_TABLE-1,x
    dex
    bne     .clrTable
    ldx     #HSCLoadingMsgEnd-HSCLoadingMsg
.loadingMsg
    lda     HSCLoadingMsg-1,x
    sta     HSC_TABLE+HSC_LINE_LEN+[HSC_LINE_LEN-[HSCLoadingMsgEnd-HSCLoadingMsg]]/2-1,x
    dex
    bne     .loadingMsg
    lda     #%00000110              ; 3 copies medium
    sta     NUSIZ0
    sta     NUSIZ1
    lda     #%00000001
    sta     VDELP0
    sta     VDELP1
    sta     CTRLPF                  ; reflect PF
    lda     #$30
    sta     PF0                     ; COLUPF, COLUBK = black

;-----------------------------------------------------------
HSCMainLoop
    SUBROUTINE
    lda     #2
    sta     WSYNC
    sta     VSYNC
    sta     WSYNC
    sta     WSYNC
    lsr
    sta     WSYNC
    sta     VSYNC
    ldy     #HSC_VBLANK_TIM
    sty     TIM64T
    dec     CYCLE
    lda     #0
    sta     GRP0
    sta     GRP1
    sta     GRP0
    sta     GRP1
    lda     #HSC_ROWS-1
    sta     LOOP
    lda     #<HSCMsgBlank           ; first row
    sta     TPTR
    lda     #>HSCMsgBlank
    sta     TPTR+1
    jsr     TextPosition
    jsr     TextCopyS
.waitVblank
    lda     INTIM
    bne     .waitVblank
    sta     WSYNC
    sta     VBLANK
    sta     WSYNC
    jsr     TextKernel
.textLoop
    lda     #0
    sta     GRP1
    sta     GRP0
    sta     GRP1
    ldx     LOOP
    lda     HSCRowColor,x
    sta     COLUP0
    sta     COLUP1
    lda     HSCRowPtrLo,x
    sta     TPTR
    lda     HSCRowPtrHi,x
    sta     TPTR+1
    beq     .tableRow               ; RAM -> table font
    jsr     TextCopyS
    jmp     .copied

.tableRow
    jsr     TextCopy
.copied
    jsr     TextKernel
    dec     LOOP
    bpl     .textLoop

    lda     #2                      ; overscan
    sta     WSYNC
    sta     WSYNC
    sta     WSYNC
    sta     WSYNC
    sta     WSYNC
    sta     WSYNC
    sta     VBLANK
    ldy     #HSC_OVERSCAN_TIM
    sty     TIM64T

    lda     HSC_WAIT                ; request already sent?
    beq     .waitTable              ;   yes
    dec     HSC_WAIT                ; no, don't touch the receive buffer until then
    bne     .noTable
    lda     #$00                    ; request the HSC table
    sta     WriteToBuffer
    lda     #HIGHSCORE_ID
    sta     WriteSendBuffer
    jmp     .noTable

.waitTable
    lda     ReceiveBufferSize       ; table received?
    cmp     #HSC_TABLE_SIZE
    bcc     .noTable
    ldx     #0
.copyTable
    lda     ReceiveBuffer
    sta     HSC_TABLE,x
    inx
    cpx     #HSC_TABLE_SIZE
    bne     .copyTable
.drain
    lda     ReceiveBufferSize       ; ignore more bytes
    beq     .noTable
    lda     ReceiveBuffer
    jmp     .drain

.noTable
    bit     SWCHA                   ; joystick right?
    bpl     .exit
    lda     SWCHB                   ; RESET?
    lsr
    bcc     .exit
.waitOverscan
    lda     INTIM
    bne     .waitOverscan
    jmp     HSCMainLoop

.exit
    jmp     Start_3                 ; cold start of the game

;-----------------------------------------------------------
; Copy the 24 chars at TPTR as bitmap into BUFF1 (from BUFF1+41 down to BUFF1+0).
; Same result as TextCopy1/TextCopy2 of 1942 bB, but BUFF1 is written with pha.
; Must be called with SP = $ff.
;-----------------------------------------------------------
    MAC TEXTCOPY_ODD4           ; {1}..{4} = L1, L2, R1, R2 font tables
.loop
    lda     (TPTR),y            ; right char of the odd pair
    tax
    dey
    lda     (TPTR),y            ; left char
    sty     TIDX
    tay
    lda     {2}+3,y             ; 4 rows of L2/R2
    ora     {4}+3,x
    pha
    lda     {2}+2,y
    ora     {4}+2,x
    pha
    lda     {2}+1,y
    ora     {4}+1,x
    pha
    lda     {2}+0,y
    ora     {4}+0,x
    pha
    ldy     TIDX
    dey
    lda     (TPTR),y            ; right char of the even pair
    tax
    dey
    lda     (TPTR),y
    sty     TIDX
    tay
    lda     {1}+3,y             ; 3 rows of L1/R1
    ora     {3}+3,x
    pha
    lda     {1}+2,y
    ora     {3}+2,x
    pha
    lda     {1}+1,y
    ora     {3}+1,x
    pha
    ldy     TIDX
    dey
    bpl     .loop
    ENDM

    MAC TEXTCOPY_ODD3           ; {1}..{4} = L1, L2, R1, R2 font tables
.loop
    lda     (TPTR),y            ; right char of the odd pair
    tax
    dey
    lda     (TPTR),y            ; left char
    sty     TIDX
    tay
    lda     {1}+3,y             ; 3 rows of L1/R1
    ora     {3}+3,x
    pha
    lda     {1}+2,y
    ora     {3}+2,x
    pha
    lda     {1}+1,y
    ora     {3}+1,x
    pha
    ldy     TIDX
    dey
    lda     (TPTR),y            ; right char of the even pair
    tax
    dey
    lda     (TPTR),y
    sty     TIDX
    tay
    lda     {2}+3,y             ; 4 rows of L2/R2
    ora     {4}+3,x
    pha
    lda     {2}+2,y
    ora     {4}+2,x
    pha
    lda     {2}+1,y
    ora     {4}+1,x
    pha
    lda     {2}+0,y
    ora     {4}+0,x
    pha
    ldy     TIDX
    dey
    bpl     .loop
    ENDM

TextCopy                        ; font of the HSC table
    SUBROUTINE
    ldx     #<(BUFF1+41)
    txs
    ldy     #HSC_LINE_LEN-1
    lda     CYCLE
    lsr
    bcc     .phase2
    TEXTCOPY_ODD4 L1CHARS, L2CHARS, R1CHARS, R2CHARS
    bcs     .done                   ; always (C = 1 from lsr)
.phase2
    TEXTCOPY_ODD3 L1CHARS, L2CHARS, R1CHARS, R2CHARS
.done
    ldx     #$fd                    ; SP after jsr from the main loop
    txs
    rts

TextCopyS                       ; static font (with "-")
    SUBROUTINE
    ldx     #<(BUFF1+41)
    txs
    ldy     #HSC_LINE_LEN-1
    lda     CYCLE
    lsr
    bcc     .phase2
    TEXTCOPY_ODD4 SL1CHARS, SL2CHARS, SR1CHARS, SR2CHARS
    bcs     .done                   ; always (C = 1 from lsr)
.phase2
    TEXTCOPY_ODD3 SL1CHARS, SL2CHARS, SR1CHARS, SR2CHARS
.done
    ldx     #$fd                    ; SP after jsr from the main loop
    txs
    rts

;-----------------------------------------------------------
; Rows from bottom (0) to top (HSC_ROWS-1), the first row is always blank.
; Pointers with hi byte 0 point to the HSC table in RAM (table font).
;-----------------------------------------------------------
HSC_ROWS        = 10
HSC_VBLANK_TIM  = 36            ; 3 + 31 + 204 + 24 = 262 lines
HSC_OVERSCAN_TIM = 28

HSCRowPtrLo
    .byte   <HSCMsgBlank, <HSCMsgBlank, <HSCMsgBlank
    .byte   <(HSC_TABLE+2*HSC_LINE_LEN), <(HSC_TABLE+HSC_LINE_LEN), <HSC_TABLE
    .byte   <HSCMsgBlank, <HSCMsgTop3, <HSCMsgTitle, <HSCMsgBlank
HSCRowPtrHi
    .byte   >HSCMsgBlank, >HSCMsgBlank, >HSCMsgBlank
    .byte   0, 0, 0
    .byte   >HSCMsgBlank, >HSCMsgTop3, >HSCMsgTitle, >HSCMsgBlank
HSCRowColor
    .byte   HSC_COL_TABLE, HSC_COL_TABLE, HSC_COL_TABLE
    .byte   HSC_COL_TABLE, HSC_COL_TABLE, HSC_COL_TABLE
    .byte   HSC_COL_TABLE, HSC_COL_TOP3, HSC_COL_TITLE, HSC_COL_TABLE

HSCMsgTitle
    .byte   __, __, __, __, __, _K, _u, _n, _g, _DASH, _F, _u, __, _M, _a, _s, _t, _e, _r, __, __, __, __, __
HSCMsgTop3
    .byte   __, __, __, __, __, __, __, __, __, __, _T, _o, _p, __, _3, __, __, __, __, __, __, __, __, __
HSCMsgBlank
    .byte   __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __
HSCLoadingMsg                       ; centered in the middle line of the table
    .byte   _L, _o, _a, _d, _i, _n, _g, _DOT, _DOT, _DOT
HSCLoadingMsgEnd

    include "HSC_TextKernel.asm"
    include "HSC_Font.asm"
