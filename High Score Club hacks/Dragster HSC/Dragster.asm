   processor 6502
  LIST OFF
    include "vcs.h"
  LIST ON

PLUSROM     = 1
PAL50       = 0
PAL_COLORS  = 0

MAX_GEAR    = 4
MAX_TACH    = $1f

CLUTCH_FLAG = $80


variation   = $80
frameCnt    = $81
random      = $82

countDown   = $8d

framePlayer = $8f

ptrLst      = $90       ;..$9b
pfLst       = $9c       ;..$a7

tachLst     = $a8
leftTach    = tachLst
rightTach   = tachLst+1

yPosLst     = $ab

joyLst      = $ad
leftJoy     = joyLst
rightJoy    = joyLst+1

timerLst    = $b3
timerHiLst  = timerLst
timerMedLst = timerLst+2
timerLoLst  = timerLst+4

distLst     = $ba
speedLst    = $c0
distLoLst   = $c2
pfIdxLst    = $c4

gearLst     = $cc

msgPtr      = $d4
lastJoyLst  = $d6
temp1       = $d8

   IF PLUSROM
WriteToBuffer     = $1ff0
WriteSendBuffer   = $1ff1
ReceiveBuffer     = $1ff2
ReceiveBufferSize = $1ff3
HIGHSCORE_ID      = 52	 ; Dragster game ID in Highscore DB
   ENDIF

;===============================================================================
; R O M - C O D E (Part 1)
;===============================================================================
  IF PLUSROM
LF6F2   =   $F7F2
  ELSE
LF6F2   =   $F6F2
  ENDIF
    ORG     $F000

START
    sei                     ; 2
    cld                     ; 2
    ldx     #$00            ; 2
Reset
    lda     #$00            ; 2
LF006
    sta     VSYNC,x         ; 4
    txs                     ; 2
    inx                     ; 2
    bne     LF006           ; 2
    lda     random          ; 3
    bne     LF013           ; 2
    jmp     LF2FD           ; 3

LF013
    jsr     InitGame        ; 6
MainLoop
    ldx     #$06            ; 2
LF018
    lda     LF6CA,x         ; 4
    eor     $84             ; 3
    and     $85             ; 3
    sta     $86,x           ; 4
    dex                     ; 2
    bpl     LF018           ; 2
    nop                     ; 2
    nop                     ; 2
    nop                     ; 2

  IF PAL50 = 0
    nop                     ; 2
    nop                     ; 2
  ENDIF

    ldx     framePlayer     ; 3
    sec                     ; 2
    ldy     #$00            ; 2
    lda     distLst,x       ; 4
LF030
    iny                     ; 2
    sbc     #$03            ; 2
    bpl     LF030           ; 2
    dey                     ; 2
    sec                     ; 2
    tya                     ; 2
    ldy     #$00            ; 2
LF03A
    iny                     ; 2
    sbc     #$05            ; 2
    bpl     LF03A           ; 2
    dey                     ; 2
    sty     $BC,x           ; 4
    adc     #$05            ; 2
    sta     $BE,x           ; 4
LF046
    lda     INTIM           ; 4
    bne     LF046           ; 2
    sta     WSYNC           ; 3
;---------------------------------------
    sta     VBLANK          ; 3
    sta     $AA             ; 3
LF051
    ldx     $AA             ; 3
    lda     distLst,x       ; 4
    jsr     LF4E5           ; 6
    ldx     $AA             ; 3
    lda     #$03            ; 2
    sta     NUSIZ0          ; 3
    sta     NUSIZ1          ; 3
    ldy     pfIdxLst,x      ; 4
    lda     LF6C0,y         ; 4
    sta     WSYNC           ; 3
;---------------------------------------
    sta     PF0             ; 3
    sta     PF2             ; 3
    lda     LF6C4,y         ; 4
    sta     PF1             ; 3
    jsr     LF7D0           ; 6
    ldy     #$05            ; 2
LF075
    sta     WSYNC           ; 3
;---------------------------------------
    dey                     ; 2
    bpl     LF075           ; 2
    ldx     $AA             ; 3
    ldy     $BC,x           ; 4
LF07E
    dey                     ; 2
    bpl     LF07E           ; 2
    ldy     $BE,x           ; 4
    cpy     #$04            ; 2
    beq     LF095           ; 2
    cpy     #$03            ; 2
    beq     LF098           ; 2
    cpy     #$02            ; 2
    beq     LF09B           ; 2
    cpy     #$01            ; 2
    beq     LF09E           ; 2
    bne     LF0A0           ; 2
LF095
    nop                     ; 2
    lda     temp1           ; 3
LF098
    nop                     ; 2
    lda     temp1           ; 3
LF09B
    nop                     ; 2
    lda     temp1           ; 3
LF09E
    lda     temp1           ; 3
LF0A0
    nop                     ; 2
    nop                     ; 2
    lda     #$16            ; 2
    sta     $8E             ; 3
    clc                     ; 2
LF0A7
    ldy     $8E             ; 3
    lda     (ptrLst),y      ; 5
    sta     GRP0            ; 3
    lda     (ptrLst+2),y    ; 5
    sta     GRP1            ; 3
    lda     (ptrLst+4),y    ; 5
    sta     GRP0            ; 3
    lda     (ptrLst+10),y   ; 5
    sta     temp1           ; 3
    lda     (ptrLst+8),y    ; 5
    tax                     ; 2
    lda     (ptrLst+6),y    ; 5
    ldy     temp1           ; 3
    sta     GRP1            ; 3
    stx     GRP0            ; 3
    sty     GRP1            ; 3
    sta     GRP0            ; 3
    lda     temp1           ; 3
    nop                     ; 2
    lda     $8E             ; 3
    lsr                     ; 2
    lsr                     ; 2
    lsr                     ; 2
    tay                     ; 2
    lda     LF6D0,y         ; 4
    sta     PF0             ; 3
    sta     PF1             ; 3
    sta     PF2             ; 3
    ldy     $8E             ; 3
    lda     (ptrLst),y      ; 5
    sta     GRP0            ; 3
    lda     (ptrLst+2),y    ; 5
    sta     GRP1            ; 3
    lda     (ptrLst+4),y    ; 5
    sta     GRP0            ; 3
    lda     (ptrLst+6),y    ; 5
    ldy     temp1           ; 3
    sta     GRP1            ; 3
    stx     GRP0            ; 3
    sty     GRP1            ; 3
    sta     GRP0            ; 3
    nop                     ; 2
    nop                     ; 2
    nop                     ; 2
    dec     $8E             ; 5
    bpl     LF0A7           ; 2
    ldx     #$01            ; 2
LF0FD
    ldy     #$00            ; 2
    sty     GRP0            ; 3
    sty     GRP1            ; 3
    dex                     ; 2
    bpl     LF0FD           ; 2
    stx     WSYNC           ; 3
;---------------------------------------
    lda     $88             ; 3
    sta     COLUBK          ; 3
    ldx     #$09            ; 2
LF10E
    sta     WSYNC           ; 3
;---------------------------------------
    lda     #>InfoLineTbl   ; 2
    sta     ptrLst,x        ; 4
    dex                     ; 2
    dex                     ; 2
    bpl     LF10E           ; 2
    lda     $89             ; 3
    sta     COLUBK          ; 3
    lda     #$02            ; 2
    sta     CTRLPF          ; 3
    lda     $8A             ; 3
    sta     COLUP0          ; 3
    lda     $8B             ; 3
    sta     COLUP1          ; 3
    ldx     $AA             ; 3
    ldy     #$07            ; 2
LF12C
    sta     WSYNC           ; 3
;---------------------------------------
    lda     pfLst,x         ; 4
    sta     PF0             ; 3
    lda     pfLst+2,x       ; 4
    sta     PF1             ; 3
    lda     pfLst+4,x       ; 4
    sta     PF2             ; 3
    lda     temp1           ; 3
    lda     pfLst+6,x       ; 4
    sta     PF0             ; 3
    lda     temp1           ; 3
    lda     pfLst+8,x       ; 4
    sta     PF1             ; 3
    lda     temp1           ; 3
    lda     pfLst+10,x      ; 4
    sta     PF2             ; 3
    dey                     ; 2
    bpl     LF12C           ; 2
    sta     WSYNC           ; 3
;---------------------------------------
    iny                     ; 2
    sty     PF0             ; 3
    sty     PF1             ; 3
    sty     PF2             ; 3
    lda     #$10            ; 2
    sta     CTRLPF          ; 3
    lda     $86             ; 3
    sta     COLUP0          ; 3
    sta     COLUP1          ; 3
    lda     #$0F            ; 2
    jsr     LF4E5           ; 6
    lda     #$06            ; 2
    sta     NUSIZ0          ; 3
    lda     #$01            ; 2
    sta     NUSIZ1          ; 3
    ldx     $AA             ; 3
    ldy     msgPtr,x        ; 4
    sta     WSYNC           ; 3
;---------------------------------------
    beq     LF181           ; 2
    jsr     LF53B           ; 6
    sta     WSYNC           ; 3
;---------------------------------------
    sta     WSYNC           ; 3
;---------------------------------------
    jmp     LF209           ; 3

LF181
    lda     $8C             ; 3
    sta     COLUPF          ; 3
    ldx     $AA             ; 3
    ldy     #$68            ; 2
    lda     timerHiLst,x    ; 4
    beq     LF195           ; 2
    ldy     #$50            ; 2
    and     #$F0            ; 2
    beq     LF195           ; 2
    lsr                     ; 2
    tay                     ; 2
LF195
    sta     WSYNC           ; 3
;---------------------------------------
    tya                     ; 2
    sta     ptrLst          ; 3
    lda     timerHiLst,x    ; 4
    and     #$0F            ; 2
    asl                     ; 2
    asl                     ; 2
    asl                     ; 2
    sta     ptrLst+2        ; 3
    lda     timerMedLst,x   ; 4
    and     #$F0            ; 2
    lsr                     ; 2
    sta     ptrLst+6        ; 3
    lda     countDown       ; 3
    beq     LF1B6           ; 2
    and     #$F0            ; 2
    lsr                     ; 2
    adc     #$08            ; 2
    jmp     LF1BD           ; 3

LF1B6
    lda     timerMedLst,x   ; 4
    and     #$0F            ; 2
    asl                     ; 2
    asl                     ; 2
    asl                     ; 2
LF1BD
    sta     ptrLst+4        ; 3
    lda     #$0C            ; 2
    ldy     gearLst,x       ; 4     CLUTCH_FLAG?
    bmi     LF1CA           ; 2
    tya                     ; 2
    bne     LF1CA           ; 2
    lda     #$0B            ; 2
LF1CA
    asl                     ; 2
    asl                     ; 2
    asl                     ; 2
    sta     $98             ; 3
    lda     #$07            ; 2
    ldy     timerMedLst,x   ; 4
    cpy     #$AA            ; 2
    bne     LF1D9           ; 2
    lda     #$0A            ; 2
LF1D9
    tax                     ; 2
    ldy     #$07            ; 2
LF1DC
    sta     WSYNC           ; 3
;---------------------------------------
    nop                     ; 2
    lda     ($92),y         ; 5
    sta     GRP1            ; 3
    lda     ($90),y         ; 5
    sta     GRP0            ; 3
    lda     ($96),y         ; 5
    sta     GRP1            ; 3
    lda     ($94),y         ; 5
    sta     GRP0            ; 3
    sta     GRP1            ; 3
    lda     ($98),y         ; 5
    sta     GRP0            ; 3
    sta     GRP1            ; 3
    lda     LF56A,x         ; 4
    sta     ENABL           ; 3
    dex                     ; 2
    dey                     ; 2
    bpl     LF1DC           ; 2
    iny                     ; 2
    sty     GRP0            ; 3
    sty     GRP1            ; 3
    sty     GRP0            ; 3
    sty     ENABL           ; 3
LF209
    lda     $88             ; 3
    sta     COLUPF          ; 3
    inc     $AA             ; 5
    lda     $AA             ; 3
    lsr                     ; 2
    bcc     LF217           ; 2
    jmp     LF051           ; 3

LF217
    lda     #$0F            ; 2
    jsr     LF4E5           ; 6
    ldy     #$39            ; 2
    jsr     LF53B           ; 6
  IF PAL50
    sta     WSYNC                   ;3   =  19
;---------------------------------------
    lda     #$37                    ;2        
    sta     TIM64T          ; 4
    sta     VBLANK                  ;3        
  ELSE
    lda     #$21            ; 2
    sta     TIM64T          ; 4
  ENDIF
    lda     frameCnt        ; 3
    and     #$01            ; 2
    tax                     ; 2
    stx     framePlayer     ; 3
    ldy     #$00            ; 2
    lda     $B9             ; 3
    bmi     LF27F           ; 2
    lda     $D2,x           ; 4
    bne     LF27F           ; 2
    lda     $D0,x           ; 4
    beq     LF242           ; 2
    ldy     #$08            ; 2
    dec     $D0,x           ; 6
    jmp     LF24E           ; 3

LF242
    lda     countDown       ; 3
    beq     LF257           ; 2
    and     #$0F            ; 2
    bne     LF257           ; 2
    ldy     #$0C            ; 2
    lda     #$18            ; 2
LF24E
    sta     AUDV0,x         ; 4
    sty     AUDF0,x         ; 4
    sty     AUDC0,x         ; 4
    jmp     LF285           ; 3

LF257
    lda     $CE,x           ; 4
    bne     LF27F           ; 2
    lda     frameCnt        ; 3
    and     #$02            ; 2
    beq     LF271           ; 2
    ldy     #$09            ; 2
    lda     #$01            ; 2
    sta     AUDF0,x         ; 4
    lda     speedLst,x      ; 4
    beq     LF271           ; 2
    lda     $C8,x           ; 4
    ora     $CA,x           ; 4
    bne     LF27F           ; 2/3
LF271
    lda     tachLst,x       ; 4
    cmp     #MAX_TACH+1     ; 2     32
    bcc     LF279           ; 2
    lda     #$1F            ; 2
LF279
    eor     #$1F            ; 2
    sta     AUDF0,x         ; 4
    ldy     #$03            ; 2
LF27F
    sty     AUDC0,x         ; 4
    lda     $B1,x           ; 4
    sta     AUDV0,x         ; 4
LF285
    lda     INTIM           ; 4
    bne     LF285           ; 2
    ldy     #$82            ; 2
    sty     WSYNC           ; 3
;---------------------------------------
  IF PAL50 = 0
    sty     VBLANK          ; 3
  ENDIF
    sty     VSYNC           ; 3
    sty     WSYNC           ; 3
    sty     WSYNC           ; 3
    sty     WSYNC           ; 3
;---------------------------------------
    sta     VSYNC           ; 3
    inc     frameCnt        ; 5
    bne     LF2A5           ; 2
    inc     $B9             ; 5
    bne     LF2A5           ; 2
    sec                     ; 2
    ror     $B9             ; 5
LF2A5
    ldy     #$FF            ; 2
    lda     SWCHB           ; 4
    and     #$08            ; 2
    bne     LF2B0           ; 2
    ldy     #$0F            ; 2
LF2B0
    lda     #$00            ; 2
    bit     $B9             ; 3
    bpl     LF2BD           ; 2
    tya                     ; 2
    and     #$F7            ; 2
    tay                     ; 2
    lda     $B9             ; 3
    asl                     ; 2
LF2BD
    sta     $84             ; 3
    sty     $85             ; 3
  IF PAL50 = 1
    lda     #$3e                    ;2        
  ELSE
    lda     #$19            ; 2
  ENDIF
    sta     WSYNC           ; 3
;---------------------------------------
    sta     TIM64T          ; 4
    lda     SWCHA           ; 4
    tay                     ; 2
    and     #$0F            ; 2
    sta     rightJoy        ; 3
    tya                     ; 2
    lsr                     ; 2
    lsr                     ; 2
    lsr                     ; 2
    lsr                     ; 2
    sta     leftJoy         ; 3
    lda     leftTach        ; 3
    ora     rightTach       ; 3
    bne     LF2E3           ; 2
    lda     joyLst,x        ; 4
    cmp     #$07            ; 2     RIGHT?
    beq     .doReset        ; 2/3
LF2E3
    lda     SWCHB           ; 4
    lsr                     ; 2
    bcs     LF2EE           ; 2
.doReset
    ldx     #$B9            ; 2
    jmp     Reset           ; 3

LF2EE
    ldy     #$00            ; 2
    lsr                     ; 2
    bcs     .skipSelect     ; 2/3
    lda     $B0             ; 3
    beq     LF2FB           ; 2
    dec     $B0             ; 5
    bpl     LF31E           ; 2
LF2FB
    inc     variation       ; 5
LF2FD
    lda     variation       ; 3
    and     #$01            ; 2
    sta     variation       ; 3
    sta     $B9             ; 3
    tay                     ; 2
    iny                     ; 2
    sty     $CC             ; 3
    jsr     InitGame        ; 6
    lda     #$0A            ; 2
    sta     $CD             ; 3
    lda     #$00            ; 2
    sta     countDown       ; 3
    sta     msgPtr          ; 3
    ldy     #$1E            ; 2
    sty     $D2             ; 3
    sty     $D3             ; 3
.skipSelect
    sty     $B0             ; 3
LF31E
    lda     countDown       ; 3
    beq     LF32E           ; 2
    dec     countDown       ; 5
    bne     LF32E           ; 2
; reset clock to start game:
    ldx     #6-1            ; 2
    lsr                     ; 2
LF329
    sta     timerLst,x      ; 4
    dex                     ; 2
    bpl     LF329           ; 2
LF32E
; the following code is executed alternating between players
    ldx     framePlayer     ; 3
    lda     $B9             ; 3
    bmi     LF341           ; 2
    lda     $D2,x           ; 4
    beq     LF344           ; 2
LF338
    ldy     #$01            ; 2
    sty     $D2,x           ; 4
    dey                     ; 2
    sty     tachLst,x       ; 4
    sty     $C8,x           ; 4
LF341
    jmp     .skipVariation2 ; 3

LF344
    lda     countDown       ; 3
    bne     .skipTimer      ; 2/3
    sed                     ; 2
    clc                     ; 2
    lda     timerLoLst,x    ; 4
    adc     #$34            ; 2
    sta     timerLoLst,x    ; 4
    lda     timerMedLst,x   ; 4
    adc     #$03            ; 2
    sta     timerMedLst,x   ; 4
    lda     timerHiLst,x    ; 4
    adc     #$00            ; 2
    sta     timerHiLst,x    ; 4
    cld                     ; 2
    bcc     .skipTimer      ; 2/3
    lda     #$99            ; 2
    sta     timerHiLst,x    ; 4
    sta     timerMedLst,x   ; 4
LF365
    bne     LF338           ; 3

.skipTimer
    lda     speedLst,x      ; 4
    beq     LF3A0           ; 2
    clc                     ; 2
    adc     distLoLst,x     ; 4
    sta     distLoLst,x     ; 4
    bcc     LF374           ; 2
    inc     distLst,x       ; 6
LF374
    lda     speedLst,x      ; 4
    rol                     ; 2
    rol                     ; 2
    rol                     ; 2
    and     #$03            ; 2
    tay                     ; 2
    lda     LF6C8,y         ; 4
    and     frameCnt        ; 3
    bne     LF392           ; 2
    inc     pfIdxLst,x      ; 6
    clc                     ; 2
    lda     $C6,x           ; 4
    adc     #$17            ; 2
    cmp     #$2F            ; 2
    bcc     LF390           ; 2
    lda     #$00            ; 2
LF390
    sta     $C6,x           ; 4
LF392
    lda     pfIdxLst,x      ; 4
    and     #$03            ; 2
    sta     pfIdxLst,x      ; 4
; finish distance reached?
    lda     distLst,x       ; 4
    cmp     #$60            ; 2
    bcc     LF3A0           ; 2
  IF PLUSROM
    bne     LF3CFx           ; 2 ; LF3CFx
  ELSE
    bne     LF365           ; 2
  ENDIF
LF3A0
    lda     $CE,x           ; 4
    bne     LF3CF           ; 2
    lda     frameCnt        ; 3
    ldy     gearLst,x       ; 4     CLUTCH_FLAG
    bpl     .noClutch       ; 2/3
    ldy     #0              ; 2
.noClutch
    and     AccelMask,y     ; 4     0, 0, 2, 6, $E
    bne     .skipAccel      ; 2/3
    lda     INPT4,x         ; 4
    bmi     .noFire         ; 2
    lda     $CA,x           ; 4
    beq     LF3BF           ; 2
    lda     frameCnt        ; 3
    and     #$02            ; 2
    beq     .noFire         ; 2
LF3BF
    clc                     ; 2
    lda     tachLst,x       ; 4
    adc     TachTbl,y       ; 4
    sta     tachLst,x       ; 4
    lda     #$0C            ; 2
    sta     $B1,x           ; 4
    sta     $B9             ; 3
    bne     LF3E3           ; 3
  IF PLUSROM
LF3CFx
    jmp SendPlusROMScore
  ENDIF
LF3CF
    bne     .skipSpeed      ; 3

.noFire
    sec                     ; 2
    lda     tachLst,x       ; 4
    sbc     TachTbl,y       ; 4
    sta     tachLst,x       ; 4
    dec     $B1,x           ; 6
    lda     #$04            ; 2
    cmp     $B1,x           ; 4
    bcc     LF3E3           ; 2
    sta     $B1,x           ; 4
LF3E3
    lda     tachLst,x       ; 4     clutch enabled?
    bpl     LF3E9           ; 2/3    no
    lda     #$00            ; 2      yes, no acceleration
LF3E9
    cmp     #MAX_TACH+1     ; 2
    bcc     .notBlown       ; 2/3
; engine blown:
    lda     #$0f            ; 2
    sta     $d0,x           ; 4
    lda     #$01            ; 2
    sta     msgPtr,x        ; 4     display "BLOWN"
    lda     #$04            ; 2
    sta     yPosLst,x       ; 4
    lda     #$1a            ; 2
    sta     $ce,x           ; 4
    lda     #$00            ; 2
.notBlown                   ;
    sta     tachLst,x       ;
.skipAccel
    lda     #$00            ; 2
    sta     $C8,x           ; 4
; calculate tire speed (Y = gear or 0):
; max = 31, 63, 126, 252
    tya                     ; 2
    beq     .skipSpeed      ; 2     skip when clutch pressed
    lda     tachLst,x       ; 4
    cmp     #20             ; 2     if tach >= 20: 0,1,2,4 extra
LF40C
    dey                     ; 2
    beq     LF413           ; 2
    rol                     ; 2
    jmp     LF40C           ; 3

LF413
    sta     temp1           ; 3     tach << gear ("speed limit")
    cmp     speedLst,x      ; 4
    beq     .skipSpeed      ; 2/3
    bcs     .accellerate    ; 2/3
; slow down:
    lda     speedLst,x      ; 4
    beq     .skipSpeed      ; 2
    dec     speedLst,x      ; 6
    jmp     .skipSpeed      ; 3

.accellerate
    lda     temp1           ; 3
    sec                     ; 2
    sbc     speedLst,x      ; 4
    inc     speedLst,x      ; 6
    inc     speedLst,x      ; 6
    cmp     #16             ; 2     tires squealing?
    bcc     .skipSpeed      ; 2/3    no
    lda     $CE,x           ; 4
    bne     .skipSpeed      ; 2/3
    lda     #$17            ; 2     enable squealing sound
    sta     $C8,x           ; 4
    dec     tachLst,x       ; 6     decrease tach
.skipSpeed
; *** check the clutch: ***
    lda     joyLst,x        ; 4
    and     #$04            ; 2     mask LEFT
    cmp     lastJoyLst,x    ; 4
    sta     lastJoyLst,x    ; 4     joystick changed?
    beq     .endClutch      ; 2/3    no
    cmp     #$00            ; 2     clutch pressed?
    bne     .shiftGear      ; 2/3    no, clutch released!
    asl     gearLst,x       ; 6      yes, enable clutch
    sec                     ; 2     CLUTCH_FLAG
    ror     gearLst,x       ; 6
    bmi     .endClutch      ; 3

.shiftGear
    lda     countDown       ; 3
    beq     LF458           ; 2
    lda     #$1D            ; 2     display "EARLY"
    sta     msgPtr,x        ; 4
LF458
    inc     gearLst,x       ; 6
    lda     gearLst,x       ; 4
    and     #~CLUTCH_FLAG   ; 2
    cmp     #MAX_GEAR       ; 2
    bcc     LF464           ; 2/3
    lda     #MAX_GEAR       ; 2
LF464
    sta     gearLst,x       ; 4
.endClutch
; *** check game variation: ***
    lda     variation       ; 3
    lsr                     ; 2
    bcc     .skipVariation2 ; 2/3
    lda     $CE,x           ; 4
    bne     .skipVariation2 ; 2/3
    lda     speedLst,x      ; 4
    beq     .skipVariation2 ; 2/3
    lda     frameCnt        ; 3
    and     #$06            ; 2
    bne     .skipVariation2 ; 2/3
    lda     joyLst,x        ; 4
    lsr                     ; 2
    bcs     LF480           ; 2
    dec     yPosLst,x       ; 6
LF480
    lsr                     ; 2
    bcs     LF485           ; 2
    inc     yPosLst,x       ; 6
LF485
    lda     random          ; 3
    bpl     LF48D           ; 2
    inc     yPosLst,x       ; 6
    inc     yPosLst,x       ; 6
LF48D
    dec     yPosLst,x       ; 6
    lda     #$00            ; 2
    sta     $CA,x           ; 4
    ldy     yPosLst,x       ; 4
    bpl     LF49A           ; 2
    tay                     ; 2
    inc     $CA,x           ; 6
LF49A
    cpy     #$08            ; 2
    bcc     LF4A2           ; 2
    ldy     #$08            ; 2
    inc     $CA,x           ; 6
LF4A2
    sty     yPosLst,x       ; 4
.skipVariation2
    lda     random          ; 3
    asl                     ; 2
    asl                     ; 2
    asl                     ; 2
    eor     random          ; 3
    asl                     ; 2
    rol     random          ; 5
    txa                     ; 2
    ora     #$0A            ; 2
    tay                     ; 2
    lda     #$00            ; 2
LF4B4
    sta.wy  pfLst,y         ; 5     ..$a7
    dey                     ; 2
    dey                     ; 2
    bpl     LF4B4           ; 2
    ldy     tachLst,x       ; 4
    cpy     #19             ; 2
    bcc     .loop           ; 2
    tya                     ; 2
    sbc     #$13            ; 2
    tay                     ; 2
    lda     #$FF            ; 2
    sta     pfLst,x         ; 4
    sta     pfLst+2,x       ; 4
    sta     pfLst+4,x       ; 4
    txa                     ; 2
    ora     #$06            ; 2
    tax                     ; 2
.loop
    dey                     ; 2
    bmi     LF4E2           ; 2
    lda     pfLst,x         ; 4
    ora     #$08            ; 2
    asl                     ; 2
    sta     pfLst,x         ; 4
    ror     pfLst+2,x       ; 6
    rol     pfLst+4,x       ; 6
    jmp     .loop           ; 3

LF4E2
    jmp     MainLoop        ; 3

LF4E5 SUBROUTINE
    sta     $D9             ; 3
    ldx     #$00            ; 2
LF4E9
    sta     HMCLR           ; 3
LF4EB
    ldy     $8C             ; 3
    sty     WSYNC           ; 3
;---------------------------------------
    sty     COLUBK          ; 3
    clc                     ; 2
    adc     #$2E            ; 2
    tay                     ; 2
    and     #$0F            ; 2
    sta     temp1           ; 3
    tya                     ; 2
    lsr                     ; 2
    lsr                     ; 2
    lsr                     ; 2
    lsr                     ; 2
    tay                     ; 2
    clc                     ; 2
    adc     temp1           ; 3
    cmp     #$0F            ; 2
    bcc     LF509           ; 2
    sbc     #$0F            ; 2
    iny                     ; 2
LF509
    eor     #$07            ; 2
    asl                     ; 2
    asl                     ; 2
    asl                     ; 2
    asl                     ; 2
    sta     HMP0,x          ; 4
    sta     WSYNC           ; 3
;---------------------------------------
LF513
    dey                     ; 2
    bpl     LF513           ; 2
    sta     RESP0,x         ; 4
    lda     $D9             ; 3
    clc                     ; 2
    adc     #$08            ; 2
    inx                     ; 2
    cpx     #$02            ; 2
    bcc     LF4EB           ; 2
    sta     WSYNC           ; 3
;---------------------------------------
    sta     HMOVE           ; 3
    lda     $89             ; 3
    sta     WSYNC           ; 3
;---------------------------------------
    sta     COLUBK          ; 3
    rts                     ; 6

LF52D
    lda     LF7C5,y         ; 4
    sta.wy  ptrLst+1,y      ; 5
    dey                     ; 2
    dey                     ; 2
    bmi     LF53A           ; 2
    jmp     LF7D2           ; 3

LF53A
    rts                     ; 6

LF53B SUBROUTINE
    lda     #$01            ; 2
    sta     NUSIZ0          ; 3
    sta     WSYNC           ; 3
;---------------------------------------
    ldx     #$06            ; 2
LF543
    sta     WSYNC           ; 3
;---------------------------------------
    iny                     ; 2
    lda     LF76E,y         ; 4
    sta     GRP0            ; 3
    lda     LF775,y         ; 4
    sta     GRP1            ; 3
    lda     LF77C,y         ; 4
    sta     GRP0            ; 3
    lda     LF783,y         ; 4
    nop                     ; 2
    sta     GRP1            ; 3
    sta     GRP0            ; 3
    dex                     ; 2
    bpl     LF543           ; 2
    inx                     ; 2
    stx     GRP0            ; 3
    stx     GRP1            ; 3
    stx     GRP0            ; 3
    sta     WSYNC           ; 3
;---------------------------------------
    rts                     ; 6
  IF PLUSROM
    ORG $f66a
  ENDIF
LF56A
    .byte $02 ; |      X | $F56A
    .byte $02 ; |      X | $F56B
    .byte $02 ; |      X | $F56C
    .byte $02 ; |      X | $F56D
    .byte $00 ; |        | $F56E
    .byte $00 ; |        | $F56F
    .byte $00 ; |        | $F570
    .byte $00 ; |        | $F571
    .byte $00 ; |        | $F572
    .byte $00 ; |        | $F573
    .byte $00 ; |        | $F574
    .byte $00 ; |        | $F575
    .byte $07 ; |     XXX| $F576
    .byte $1F ; |   XXXXX| $F577
    .byte $3F ; |  XXXXXX| $F578
    .byte $7E ; | XXXXXX | $F579
    .byte $7D ; | XXXXX X| $F57A
    .byte $FD ; |XXXXXX X| $F57B
    .byte $EF ; |XXX XXXX| $F57C
    .byte $F7 ; |XXXX XXX| $F57D
    .byte $FE ; |XXXXXXX | $F57E
    .byte $7E ; | XXXXXX | $F57F
    .byte $7D ; | XXXXX X| $F580
    .byte $3F ; |  XXXXXX| $F581
    .byte $1F ; |   XXXXX| $F582
    .byte $07 ; |     XXX| $F583
    .byte $01 ; |       X| $F584
    .byte $00 ; |        | $F585
    .byte $00 ; |        | $F586
    .byte $00 ; |        | $F587
    .byte $00 ; |        | $F588
    .byte $00 ; |        | $F589
    .byte $00 ; |        | $F58A
    .byte $00 ; |        | $F58B
    .byte $00 ; |        | $F58C
    .byte $07 ; |     XXX| $F58D
    .byte $1F ; |   XXXXX| $F58E
    .byte $3F ; |  XXXXXX| $F58F
    .byte $77 ; | XXX XXX| $F590
    .byte $77 ; | XXX XXX| $F591
    .byte $FB ; |XXXXX XX| $F592
    .byte $FF ; |XXXXXXXX| $F593
    .byte $FF ; |XXXXXXXX| $F594
    .byte $FB ; |XXXXX XX| $F595
    .byte $77 ; | XXX XXX| $F596
    .byte $7F ; | XXXXXXX| $F597
    .byte $3F ; |  XXXXXX| $F598
    .byte $1F ; |   XXXXX| $F599
    .byte $07 ; |     XXX| $F59A
    .byte $01 ; |       X| $F59B
    .byte $00 ; |        | $F59C
    .byte $00 ; |        | $F59D
    .byte $00 ; |        | $F59E
    .byte $00 ; |        | $F59F
    .byte $00 ; |        | $F5A0
    .byte $00 ; |        | $F5A1
    .byte $00 ; |        | $F5A2
    .byte $00 ; |        | $F5A3
    .byte $07 ; |     XXX| $F5A4
    .byte $1F ; |   XXXXX| $F5A5
    .byte $3F ; |  XXXXXX| $F5A6
    .byte $7F ; | XXXXXXX| $F5A7
    .byte $6F ; | XX XXXX| $F5A8
    .byte $F6 ; |XXXX XX | $F5A9
    .byte $FB ; |XXXXX XX| $F5AA
    .byte $FF ; |XXXXXXXX| $F5AB
    .byte $FD ; |XXXXXX X| $F5AC
    .byte $7B ; | XXXX XX| $F5AD
    .byte $77 ; | XXX XXX| $F5AE
    .byte $3F ; |  XXXXXX| $F5AF
    .byte $1F ; |   XXXXX| $F5B0
    .byte $07 ; |     XXX| $F5B1
    .byte $01 ; |       X| $F5B2
    .byte $00 ; |        | $F5B3
    .byte $00 ; |        | $F5B4
    .byte $00 ; |        | $F5B5
    .byte $00 ; |        | $F5B6
    .byte $00 ; |        | $F5B7
    .byte $00 ; |        | $F5B8
    .byte $00 ; |        | $F5B9
    .byte $00 ; |        | $F5BA
    .byte $80 ; |X       | $F5BB
    .byte $E0 ; |XXX     | $F5BC
    .byte $F7 ; |XXXX XXX| $F5BD
    .byte $FB ; |XXXXX XX| $F5BE
    .byte $FB ; |XXXXX XX| $F5BF
    .byte $FF ; |XXXXXXXX| $F5C0
    .byte $BF ; |X XXXXXX| $F5C1
    .byte $DF ; |XX XXXXX| $F5C2
    .byte $FD ; |XXXXXX X| $F5C3
    .byte $FA ; |XXXXX X | $F5C4
    .byte $FA ; |XXXXX X | $F5C5
    .byte $F6 ; |XXXX XX | $F5C6
    .byte $EC ; |XXX XX  | $F5C7
    .byte $B8 ; |X XXX   | $F5C8
    .byte $E0 ; |XXX     | $F5C9
    .byte $00 ; |        | $F5CA
    .byte $00 ; |        | $F5CB
    .byte $00 ; |        | $F5CC
    .byte $00 ; |        | $F5CD
    .byte $00 ; |        | $F5CE
    .byte $00 ; |        | $F5CF
    .byte $00 ; |        | $F5D0
    .byte $00 ; |        | $F5D1
    .byte $80 ; |X       | $F5D2
    .byte $E0 ; |XXX     | $F5D3
    .byte $F7 ; |XXXX XXX| $F5D4
    .byte $FB ; |XXXXX XX| $F5D5
    .byte $BB ; |X XXX XX| $F5D6
    .byte $7F ; | XXXXXXX| $F5D7
    .byte $FF ; |XXXXXXXX| $F5D8
    .byte $FF ; |XXXXXXXX| $F5D9
    .byte $7D ; | XXXXX X| $F5DA
    .byte $BA ; |X XXX X | $F5DB
    .byte $BA ; |X XXX X | $F5DC
    .byte $F6 ; |XXXX XX | $F5DD
    .byte $EC ; |XXX XX  | $F5DE
    .byte $B8 ; |X XXX   | $F5DF
    .byte $E0 ; |XXX     | $F5E0
    .byte $00 ; |        | $F5E1
    .byte $00 ; |        | $F5E2
    .byte $00 ; |        | $F5E3
    .byte $00 ; |        | $F5E4
    .byte $00 ; |        | $F5E5
    .byte $00 ; |        | $F5E6
    .byte $00 ; |        | $F5E7
    .byte $00 ; |        | $F5E8
    .byte $80 ; |X       | $F5E9
    .byte $E0 ; |XXX     | $F5EA
    .byte $F7 ; |XXXX XXX| $F5EB
    .byte $BB ; |X XXX XX| $F5EC
    .byte $7B ; | XXXX XX| $F5ED
    .byte $FF ; |XXXXXXXX| $F5EE
    .byte $FF ; |XXXXXXXX| $F5EF
    .byte $7F ; | XXXXXXX| $F5F0
    .byte $BD ; |X XXXX X| $F5F1
    .byte $DA ; |XX XX X | $F5F2
    .byte $FA ; |XXXXX X | $F5F3
    .byte $F6 ; |XXXX XX | $F5F4
    .byte $EC ; |XXX XX  | $F5F5
    .byte $B8 ; |X XXX   | $F5F6
    .byte $E0 ; |XXX     | $F5F7
    .byte $00 ; |        | $F5F8
    .byte $00 ; |        | $F5F9
    .byte $00 ; |        | $F5FA
    .byte $00 ; |        | $F5FB
    .byte $00 ; |        | $F5FC
    .byte $00 ; |        | $F5FD
    .byte $00 ; |        | $F5FE
    .byte $00 ; |        | $F5FF
LF600
    .byte $00 ; |        | $F600
    .byte $00 ; |        | $F601
    .byte $00 ; |        | $F602
    .byte $00 ; |        | $F603
    .byte $00 ; |        | $F604
    .byte $00 ; |        | $F605
    .byte $00 ; |        | $F606
    .byte $00 ; |        | $F607
    .byte $44 ; | X   X  | $F608
    .byte $22 ; |  X   X | $F609
    .byte $EE ; |XXX XXX | $F60A
    .byte $F7 ; |XXXX XXX| $F60B
    .byte $FB ; |XXXXX XX| $F60C
    .byte $FD ; |XXXXXX X| $F60D
    .byte $FF ; |XXXXXXXX| $F60E
    .byte $EF ; |XXX XXXX| $F60F
    .byte $E8 ; |XXX X   | $F610
    .byte $F8 ; |XXXXX   | $F611
    .byte $00 ; |        | $F612
    .byte $00 ; |        | $F613
    .byte $00 ; |        | $F614
    .byte $00 ; |        | $F615
    .byte $00 ; |        | $F616
    .byte $00 ; |        | $F617
    .byte $00 ; |        | $F618
    .byte $00 ; |        | $F619
    .byte $00 ; |        | $F61A
    .byte $00 ; |        | $F61B
    .byte $00 ; |        | $F61C
    .byte $00 ; |        | $F61D
    .byte $00 ; |        | $F61E
    .byte $00 ; |        | $F61F
    .byte $00 ; |        | $F620
    .byte $20 ; |  X     | $F621
    .byte $92 ; |X  X  X | $F622
    .byte $E1 ; |XXX    X| $F623
    .byte $F6 ; |XXXX XX | $F624
    .byte $FB ; |XXXXX XX| $F625
    .byte $FB ; |XXXXX XX| $F626
    .byte $FF ; |XXXXXXXX| $F627
    .byte $EF ; |XXX XXXX| $F628
    .byte $E0 ; |XXX     | $F629
    .byte $00 ; |        | $F62A
    .byte $00 ; |        | $F62B
    .byte $00 ; |        | $F62C
    .byte $00 ; |        | $F62D
    .byte $00 ; |        | $F62E
    .byte $00 ; |        | $F62F
    .byte $00 ; |        | $F630
    .byte $00 ; |        | $F631
    .byte $00 ; |        | $F632
    .byte $00 ; |        | $F633
    .byte $00 ; |        | $F634
    .byte $00 ; |        | $F635
    .byte $40 ; | X      | $F636
    .byte $20 ; |  X     | $F637
    .byte $EF ; |XXX XXXX| $F638
    .byte $74 ; | XXX X  | $F639
    .byte $BA ; |X XXX X | $F63A
    .byte $DD ; |XX XXX X| $F63B
    .byte $FF ; |XXXXXXXX| $F63C
    .byte $FF ; |XXXXXXXX| $F63D
    .byte $00 ; |        | $F63E
    .byte $00 ; |        | $F63F
    .byte $00 ; |        | $F640
    .byte $00 ; |        | $F641
    .byte $00 ; |        | $F642
    .byte $00 ; |        | $F643
    .byte $00 ; |        | $F644
    .byte $00 ; |        | $F645
    .byte $00 ; |        | $F646
    .byte $00 ; |        | $F647
    .byte $00 ; |        | $F648
    .byte $00 ; |        | $F649
    .byte $00 ; |        | $F64A
    .byte $00 ; |        | $F64B
    .byte $00 ; |        | $F64C
    .byte $00 ; |        | $F64D
    .byte $00 ; |        | $F64E
    .byte $00 ; |        | $F64F
    .byte $00 ; |        | $F650
    .byte $20 ; |  X     | $F651
    .byte $10 ; |   X    | $F652
    .byte $60 ; | XX     | $F653
    .byte $B6 ; |X XX XX | $F654
    .byte $BB ; |X XXX XX| $F655
    .byte $FA ; |XXXXX X | $F656
    .byte $FF ; |XXXXXXXX| $F657
    .byte $0F ; |    XXXX| $F658
    .byte $00 ; |        | $F659
    .byte $00 ; |        | $F65A
    .byte $00 ; |        | $F65B
    .byte $00 ; |        | $F65C
    .byte $00 ; |        | $F65D
    .byte $00 ; |        | $F65E
    .byte $00 ; |        | $F65F
    .byte $00 ; |        | $F660
    .byte $00 ; |        | $F661
    .byte $00 ; |        | $F662
    .byte $00 ; |        | $F663
    .byte $00 ; |        | $F664
    .byte $00 ; |        | $F665
    .byte $FF ; |XXXXXXXX| $F666
    .byte $00 ; |        | $F667
    .byte $FF ; |XXXXXXXX| $F668
    .byte $80 ; |X       | $F669
    .byte $80 ; |X       | $F66A
    .byte $80 ; |X       | $F66B
    .byte $00 ; |        | $F66C
    .byte $00 ; |        | $F66D
    .byte $00 ; |        | $F66E
    .byte $00 ; |        | $F66F
    .byte $00 ; |        | $F670
    .byte $00 ; |        | $F671
    .byte $00 ; |        | $F672
    .byte $00 ; |        | $F673
    .byte $00 ; |        | $F674
    .byte $00 ; |        | $F675
    .byte $00 ; |        | $F676
    .byte $00 ; |        | $F677
    .byte $00 ; |        | $F678
    .byte $00 ; |        | $F679
    .byte $00 ; |        | $F67A
    .byte $00 ; |        | $F67B
    .byte $00 ; |        | $F67C
    .byte $00 ; |        | $F67D
    .byte $00 ; |        | $F67E
    .byte $00 ; |        | $F67F
    .byte $00 ; |        | $F680
    .byte $00 ; |        | $F681
    .byte $00 ; |        | $F682
    .byte $F0 ; |XXXX    | $F683
    .byte $0E ; |    XXX | $F684
    .byte $E1 ; |XXX    X| $F685
    .byte $1F ; |   XXXXX| $F686
    .byte $00 ; |        | $F687
    .byte $00 ; |        | $F688
    .byte $00 ; |        | $F689
    .byte $00 ; |        | $F68A
    .byte $00 ; |        | $F68B
    .byte $00 ; |        | $F68C
    .byte $00 ; |        | $F68D
    .byte $00 ; |        | $F68E
    .byte $00 ; |        | $F68F
    .byte $00 ; |        | $F690
    .byte $00 ; |        | $F691
    .byte $30 ; |  XX    | $F692
    .byte $78 ; | XXXX   | $F693
    .byte $FC ; |XXXXXX  | $F694
    .byte $FE ; |XXXXXXX | $F695
    .byte $FA ; |XXXXX X | $F696
    .byte $34 ; |  XX X  | $F697
    .byte $18 ; |   XX   | $F698
    .byte $00 ; |        | $F699
    .byte $00 ; |        | $F69A
    .byte $00 ; |        | $F69B
    .byte $00 ; |        | $F69C
    .byte $00 ; |        | $F69D
    .byte $00 ; |        | $F69E
    .byte $00 ; |        | $F69F
    .byte $00 ; |        | $F6A0
    .byte $00 ; |        | $F6A1
    .byte $00 ; |        | $F6A2
    .byte $00 ; |        | $F6A3
    .byte $00 ; |        | $F6A4
    .byte $00 ; |        | $F6A5
    .byte $00 ; |        | $F6A6
    .byte $00 ; |        | $F6A7
    .byte $00 ; |        | $F6A8
    .byte $00 ; |        | $F6A9
    .byte $00 ; |        | $F6AA
    .byte $00 ; |        | $F6AB
    .byte $00 ; |        | $F6AC
    .byte $00 ; |        | $F6AD
    .byte $00 ; |        | $F6AE
    .byte $00 ; |        | $F6AF
    .byte $00 ; |        | $F6B0
    .byte $60 ; | XX     | $F6B1
    .byte $F0 ; |XXXX    | $F6B2
    .byte $F8 ; |XXXXX   | $F6B3
    .byte $FC ; |XXXXXX  | $F6B4
    .byte $F4 ; |XXXX X  | $F6B5
    .byte $68 ; | XX X   | $F6B6
    .byte $30 ; |  XX    | $F6B7
    .byte $00 ; |        | $F6B8
    .byte $00 ; |        | $F6B9
    .byte $00 ; |        | $F6BA
    .byte $00 ; |        | $F6BB
    .byte $00 ; |        | $F6BC
    .byte $00 ; |        | $F6BD
    .byte $00 ; |        | $F6BE
    .byte $00 ; |        | $F6BF
LF6C0
    .byte $77,$BB,$DD,$EE
LF6C4
    .byte $EE,$DD,$BB,$77
LF6C8
    .byte $06,$02
LF6CA
  IF PAL_COLORS = 1
    .byte $00,$00,$b8,$3c,$58,$66
  ELSE
    .byte $00,$00,$88,$CC,$D8,$46
  ENDIF

LF6D0
    .byte $00,$00,$FF

InitGame SUBROUTINE
    lda     #$9F            ; 2
    sta     countDown       ; 3
    ldx     #$01            ; 2
LF6D9
    lda     #$01            ; 2
    sta     VDELP0,x        ; 4
    sta     random          ; 3
    lda     #$AA            ; 2
    sta     timerHiLst,x    ; 4
    sta     timerMedLst,x   ; 4
    lda     #$04            ; 2     LEFT!
    sta     yPosLst,x       ; 4
    sta     lastJoyLst,x    ; 4
    sta     $B1,x           ; 4
    dex                     ; 2
    bpl     LF6D9           ; 2
    tax                     ; 2
    lda     #$23            ; 2
    jmp     LF4E9           ; 3

AccelMask
    .byte   0, 0, 2, 6, $E
TachTbl
    .byte   3, 1, 1, 1, 1

    ALIGN 256
InfoLineTbl
    .byte $3C ; |  XXXX  | $F700
    .byte $66 ; | XX  XX | $F701
    .byte $66 ; | XX  XX | $F702
    .byte $66 ; | XX  XX | $F703
    .byte $66 ; | XX  XX | $F704
    .byte $66 ; | XX  XX | $F705
    .byte $66 ; | XX  XX | $F706
    .byte $3C ; |  XXXX  | $F707
    .byte $7E ; | XXXXXX | $F708
    .byte $18 ; |   XX   | $F709
    .byte $18 ; |   XX   | $F70A
    .byte $18 ; |   XX   | $F70B
    .byte $18 ; |   XX   | $F70C
    .byte $78 ; | XXXX   | $F70D
    .byte $38 ; |  XXX   | $F70E
    .byte $18 ; |   XX   | $F70F
    .byte $7E ; | XXXXXX | $F710
    .byte $60 ; | XX     | $F711
    .byte $60 ; | XX     | $F712
    .byte $3C ; |  XXXX  | $F713
    .byte $06 ; |     XX | $F714
    .byte $06 ; |     XX | $F715
    .byte $46 ; | X   XX | $F716
    .byte $3C ; |  XXXX  | $F717
    .byte $3C ; |  XXXX  | $F718
    .byte $46 ; | X   XX | $F719
    .byte $06 ; |     XX | $F71A
    .byte $0C ; |    XX  | $F71B
    .byte $0C ; |    XX  | $F71C
    .byte $06 ; |     XX | $F71D
    .byte $46 ; | X   XX | $F71E
    .byte $3C ; |  XXXX  | $F71F
    .byte $0C ; |    XX  | $F720
    .byte $0C ; |    XX  | $F721
    .byte $0C ; |    XX  | $F722
    .byte $7E ; | XXXXXX | $F723
    .byte $4C ; | X  XX  | $F724
    .byte $2C ; |  X XX  | $F725
    .byte $1C ; |   XXX  | $F726
    .byte $0C ; |    XX  | $F727
    .byte $7C ; | XXXXX  | $F728
    .byte $46 ; | X   XX | $F729
    .byte $06 ; |     XX | $F72A
    .byte $06 ; |     XX | $F72B
    .byte $7C ; | XXXXX  | $F72C
    .byte $60 ; | XX     | $F72D
    .byte $60 ; | XX     | $F72E
    .byte $7E ; | XXXXXX | $F72F
    .byte $3C ; |  XXXX  | $F730
    .byte $66 ; | XX  XX | $F731
    .byte $66 ; | XX  XX | $F732
    .byte $66 ; | XX  XX | $F733
    .byte $7C ; | XXXXX  | $F734
    .byte $60 ; | XX     | $F735
    .byte $62 ; | XX   X | $F736
    .byte $3C ; |  XXXX  | $F737
    .byte $18 ; |   XX   | $F738
    .byte $18 ; |   XX   | $F739
    .byte $18 ; |   XX   | $F73A
    .byte $18 ; |   XX   | $F73B
    .byte $0C ; |    XX  | $F73C
    .byte $06 ; |     XX | $F73D
    .byte $42 ; | X    X | $F73E
    .byte $7E ; | XXXXXX | $F73F
    .byte $3C ; |  XXXX  | $F740
    .byte $66 ; | XX  XX | $F741
    .byte $66 ; | XX  XX | $F742
    .byte $3C ; |  XXXX  | $F743
    .byte $3C ; |  XXXX  | $F744
    .byte $66 ; | XX  XX | $F745
    .byte $66 ; | XX  XX | $F746
    .byte $3C ; |  XXXX  | $F747
    .byte $3C ; |  XXXX  | $F748
    .byte $46 ; | X   XX | $F749
    .byte $06 ; |     XX | $F74A
    .byte $3E ; |  XXXXX | $F74B
    .byte $66 ; | XX  XX | $F74C
    .byte $66 ; | XX  XX | $F74D
    .byte $66 ; | XX  XX | $F74E
    .byte $3C ; |  XXXX  | $F74F
    .byte $00 ; |        | $F750
    .byte $00 ; |        | $F751
    .byte $00 ; |        | $F752
    .byte $00 ; |        | $F753
    .byte $00 ; |        | $F754
    .byte $00 ; |        | $F755
    .byte $00 ; |        | $F756
    .byte $00 ; |        | $F757
    .byte $C3 ; |XX    XX| $F758
    .byte $C7 ; |XX   XXX| $F759
    .byte $CF ; |XX  XXXX| $F75A
    .byte $DF ; |XX XXXXX| $F75B
    .byte $FB ; |XXXXX XX| $F75C
    .byte $F3 ; |XXXX  XX| $F75D
    .byte $E3 ; |XXX   XX| $F75E
    .byte $C3 ; |XX    XX| $F75F
    .byte $7E ; | XXXXXX | $F760
    .byte $C3 ; |XX    XX| $F761
    .byte $C0 ; |XX      | $F762
    .byte $C0 ; |XX      | $F763
    .byte $C0 ; |XX      | $F764
    .byte $C0 ; |XX      | $F765
    .byte $C3 ; |XX    XX| $F766
    .byte $7E ; | XXXXXX | $F767
    .byte $7E ; | XXXXXX | $F768
    .byte $C3 ; |XX    XX| $F769
    .byte $C3 ; |XX    XX| $F76A
    .byte $CF ; |XX  XXXX| $F76B
    .byte $C0 ; |XX      | $F76C
    .byte $C0 ; |XX      | $F76D
LF76E
    .byte $C3 ; |XX    XX| $F76E
    .byte $7E ; | XXXXXX | $F76F
    .byte $F2 ; |XXXX  X | $F770
    .byte $4A ; | X  X X | $F771
    .byte $4A ; | X  X X | $F772
    .byte $72 ; | XXX  X | $F773
    .byte $4A ; | X  X X | $F774
LF775
    .byte $4A ; | X  X X | $F775
    .byte $F3 ; |XXXX  XX| $F776
    .byte $0E ; |    XXX | $F777
    .byte $11 ; |   X   X| $F778
    .byte $11 ; |   X   X| $F779
    .byte $11 ; |   X   X| $F77A
    .byte $11 ; |   X   X| $F77B
LF77C
    .byte $11 ; |   X   X| $F77C
    .byte $CE ; |XX  XXX | $F77D
    .byte $45 ; | X   X X| $F77E
    .byte $45 ; | X   X X| $F77F
    .byte $45 ; | X   X X| $F780
    .byte $45 ; | X   X X| $F781
    .byte $55 ; | X X X X| $F782
LF783
    .byte $6D ; | XX XX X| $F783
    .byte $45 ; | X   X X| $F784
    .byte $10 ; |   X    | $F785
    .byte $90 ; |X  X    | $F786
    .byte $50 ; | X X    | $F787
    .byte $30 ; |  XX    | $F788
    .byte $10 ; |   X    | $F789
    .byte $10 ; |   X    | $F78A
    .byte $10 ; |   X    | $F78B
    .byte $F8 ; |XXXXX   | $F78C
    .byte $81 ; |X      X| $F78D
    .byte $82 ; |X     X | $F78E
    .byte $E2 ; |XXX   X | $F78F
    .byte $83 ; |X     XX| $F790
    .byte $82 ; |X     X | $F791
    .byte $FA ; |XXXXX X | $F792
    .byte $8F ; |X   XXXX| $F793
    .byte $48 ; | X  X   | $F794
    .byte $28 ; |  X X   | $F795
    .byte $2F ; |  X XXXX| $F796
    .byte $EA ; |XXX X X | $F797
    .byte $29 ; |  X X  X| $F798
    .byte $28 ; |  X X   | $F799
    .byte $21 ; |  X    X| $F79A
    .byte $A1 ; |X X    X| $F79B
    .byte $A0 ; |X X     | $F79C
    .byte $20 ; |  X     | $F79D
    .byte $20 ; |  X     | $F79E
    .byte $20 ; |  X     | $F79F
    .byte $BE ; |X XXXXX | $F7A0
    .byte $10 ; |   X    | $F7A1
    .byte $10 ; |   X    | $F7A2
    .byte $A0 ; |X X     | $F7A3
    .byte $40 ; | X      | $F7A4
    .byte $40 ; | X      | $F7A5
    .byte $40 ; | X      | $F7A6
    .byte $40 ; | X      | $F7A7
    .byte $0F ; |    XXXX| $F7A8
    .byte $41 ; | X     X| $F7A9
    .byte $ED ; |XXX XX X| $F7AA
    .byte $A9 ; |X X X  X| $F7AB
    .byte $E9 ; |XXX X  X| $F7AC
    .byte $A9 ; |X X X  X| $F7AD
    .byte $AD ; |X X XX X| $F7AE
    .byte $F0 ; |XXXX    | $F7AF
    .byte $11 ; |   X   X| $F7B0
    .byte $53 ; | X X  XX| $F7B1
    .byte $56 ; | X X XX | $F7B2
    .byte $5C ; | X XXX  | $F7B3
    .byte $58 ; | X XX   | $F7B4
    .byte $50 ; | X X    | $F7B5
    .byte $FE ; |XXXXXXX | $F7B6
    .byte $80 ; |X       | $F7B7
    .byte $3A ; |  XXX X | $F7B8
    .byte $A2 ; |X X   X | $F7B9
    .byte $BA ; |X XXX X | $F7BA
    .byte $8A ; |X   X X | $F7BB
    .byte $BA ; |X XXX X | $F7BC
    .byte $00 ; |        | $F7BD
    .byte $00 ; |        | $F7BE
    .byte $E9 ; |XXX X  X| $F7BF
    .byte $AD ; |X X XX X| $F7C0
    .byte $AF ; |X X XXXX| $F7C1
    .byte $AB ; |X X X XX| $F7C2
    .byte $E9 ; |XXX X  X| $F7C3

LF7C4
    .byte $6E
LF7C5
    .byte #>LF56A
    .byte $B3, #>LF56A
    .byte $00, #>LF600
    .byte $2E, #>LF600
    .byte $5C, #>LF600
    .byte $8A, #>LF600

LF7D0 SUBROUTINE
    ldy     #$0A            ; 2
LF7D2
    lda     LF7C4,y         ; 4
    clc                     ; 2
    adc     yPosLst,x       ; 4
    cpy     #$04            ; 2
    bcc     LF7F1           ; 2
    clc                     ; 2
    adc     $C8,x           ; 4
    cpy     #$08            ; 2
    bcs     LF7F4           ; 2
    sta     temp1           ; 3
    lda     $CE,x           ; 4
    beq     LF7EC           ; 2
    adc     LF6F2,y         ; 4
LF7EC
    adc     temp1           ; 3
    jmp     LF7F4           ; 3

LF7F1
    clc                     ; 2
    adc     $C6,x           ; 4
LF7F4
    sta.wy  ptrLst,y        ; 5
    jmp     LF52D           ; 3

  IF PLUSROM = 1
PlusROM_API:
    .byte "a", 0, "h.firmaplus.de", 0
SendPlusROMScore:
    lda framePlayer                 ; Check for player 1
    bne EndSendPlusROMScore 

    lda msgPtr                      ; check for player 1 "BLOWN" or "EARLY"
    bne EndSendPlusROMScore 

    lda variation
    sta WriteToBuffer
    lda timerHiLst
    sta WriteToBuffer
    lda timerMedLst
    sta WriteToBuffer
    lda timerLoLst
    sta WriteToBuffer
    lda #HIGHSCORE_ID               ; game id in Highscore DB
    sta WriteSendBuffer             ; send request to backend..
EndSendPlusROMScore
    jmp LF338

    ORG     $fffa
    .word   ( PlusROM_API - $e000 )
    .word   START
    .word   0
  ELSE
LF7FA
    .byte   $00,$00,$00,$F0,$00,$00
  ENDIF
