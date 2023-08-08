;
; Unused code snippets
;

        MAC BITMAP_48_SPREAD_CENTERED

                SUBROUTINE
                sta     WSYNC
                ldx     #13
.loop           dex
                bpl     .loop
                cmp     $FF

                SUBROUTINE
.loop           ldy     LineCtr

                lda     {4},y   ; foreground color
                sta     {1}
                sta     {2}
                lda     {5},y   ; background color
                sta     {3}
                lda     {6},y   ; 1. character
                sta     GRP0
                lda     {7},y   ; 2. character
                sta     GRP1
                lda     {8},y   ; 3. character
                sta     GRP0
                lda     {9},y   ; 4. character
                sta     GRP1
                lda     {10},y  ; 5. character
                sta     GRP0
                lda     {11},y  ; 6. character
                sta     GRP1

                nop
                nop
                nop

                dec     LineCtr
                bpl     .loop

        ENDM
                STOREBYTE 7, LineCtr
                BITMAP_48_SPREAD_CENTERED COLUP0, COLUP1, COLUBK, TestColFG, TestColBG, One, Two, Three, Four, Five, Six

                rts

TestColFG:      BYTE    $80,$82,$84,$86,$88,$8A,$8C,$8E
TestColBG:      BYTE    $60,$62,$64,$66,$68,$6A,$6C,$6E


DrawScreen SUBROUTINE
    ldx     #227
.waitTim:
    lda     INTIM
    bne     .waitTim
    sta     WSYNC
    sta     VBLANK
    stx     TIM64T

; save stack pointer
    tsx
    stx     tmpVar

; enable vertical delay:
    lda     #%1
    sta     VDELP0
    sta     VDELP1

    SLEEP   33

    ldy     #8
    ldx     #$c3
    lda     #$03
    stx     PF2                 ; 3     @62
    sta     PF1                 ; 3     @65


.loopScore:

    lda     Color1,y
    sta.w   COLUPF

    lda     Three,y
    sta     GRP0
    lda     Four,y
    sta     GRP1
    lda     Five,y
    sta     GRP0

    lda     Two,y

    ldx     Six,y
    txs
    ldx     Color2,y

    stx     COLUBK
    tsx
    stx     GRP1
    sta     GRP0

    lda     Color3,y
    ldx     One,y
    stx     GRP1
    sta     COLUPF
    sta     GRP0

    dey
    bpl     .loopScore
