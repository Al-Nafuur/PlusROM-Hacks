;Copyright 2025 Cisano Carmelo
;
;This file is part of Stop die Glocke
;
;    Stop die Glocke is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    Stop Die Glocke is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with Stop die Glocke.  If not, see <http://www.gnu.org/licenses/>.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MACRO draw Tanks remained and Time
        MAC DRAW_SCORE_MACRO
.setScore
	lda #2
	sta NUSIZ0
        lda #3
        sta NUSIZ1
        lda #LIGHT_BLUE
	sta COLUP0
	sta COLUP1
	lda #COORD_X_SCORE
	ldy #0
	jsr roundPosition

        lda #COORD_X_SCORE+9
	ldy #1
	jsr roundPosition

        ldy DamageTank
        lda TablePointerNumbersLow,y
	sta PointerMultiUseOneLow
	lda TablePointerNumbersHi,y
	sta PointerMultiUseOneHi

        lda Minutes
        and #%00001111
        tay 
        lda TablePointerNumbersLow,y
	sta PointerMultiUseTwoLow
	lda TablePointerNumbersHi,y
	sta PointerMultiUseTwoHi

        sta WSYNC
	sta HMOVE

        lda Minutes
        lsr
        lsr
        lsr
        lsr
        tay
        lda TablePointerNumbersLow,y
	sta PointerMultiUseThreeLow
	lda TablePointerNumbersHi,y
	sta PointerMultiUseThreeHi

        ldy #6
.drawScore
	sta WSYNC
        SLEEP 2
        lda (PointerMultiUseTwoLow),y ;second minute
        sta TempX
        lda LogoMinutes,y
        sta TempY
        lda (PointerMultiUseThreeLow),y ;first minute
        tax
        lda (PointerMultiUseOneLow),y ;tank damage
        sta GRP0
        lda Shield,y
        sta GRP1
        lda TempX
        sta GRP0        
        stx GRP1
        lda TempY
        sta GRP1
        dey
        bpl .drawScore

        lda #0
        sta GRP0
        sta GRP1
        ENDM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;DRAW VICTORY MESSAGE;;;;;;;;;;;;;;;;;;;;;;
        MAC DRAW_MESSAGE
        lda #1
	sta NUSIZ0
        lda #0
        sta NUSIZ1
	lda #COORD_X_SCORE+11
        ldy #0
	jsr roundPosition
        lda #YELLOW
	sta COLUP0
	sta COLUP1
        
        lda #COORD_X_SCORE+19
	ldy #1
	jsr roundPosition

        sta WSYNC
	sta HMOVE

        ldy #6
.drawMessage
	sta WSYNC
        lda Vicotry_message_2,y
        tax
        lda Vicotry_message_0,y
        sta GRP0
        lda Vicotry_message_1,y
        sta GRP1
        SLEEP 26
        stx GRP0
       
        dey
        bpl .drawMessage

        lda #0
        sta GRP0
        sta GRP1

        ENDM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MACRO sound generator
    MAC PLAY_SOUND_MACRO
;if Victory Song, stop all sounds and play it
.victorySongCH0
    lda GameOver
    and #WON_PLAY_VICTORY_SONG ;cmp #%10000111
    beq .chanel0 ;bne .chanel0
    ldy #ID_VICTORY_SONG_CH0
    jmp .playCH0
;CHANEL 0
.chanel0
    ;ldx #0 ;chanel
    lda TempoCH0 ;sound until the tempo not reach the zero
    beq .introCH0
    dec TempoCH0 ;end of any sound effect
    bne .endCH0
.continuePlayCH0
    jsr SoundEffectCH0
    jmp .endCH0
;inizialize one of the sounds
.introCH0
    lda GameOver
    cmp #%10000000 ;case 7
    bne .myFireCH0
    ;in CH1
    ;lda #0
    ;sta GameOver
    ldy #ID_INTRO_SONG_CH0 ;#00 ;IntroSongCh0
    jmp .playCH0
.myFireCH0
    lda MyFire
    beq .otherSfxCH0
    lda #0
    sta MyFire
    ldy #ID_SOUND_M0
    jmp .playCH0
.otherSfxCH0
    lda #$ff
    cmp IdSfx
    beq .endCH0
    ldy IdSfx
    sta IdSfx ;store #$ff
.playCH0
    lda TableSfxLow,y
    sta SoundDataLow0
    lda TableSfxHi,y
    sta SoundDataHi0
    lda #0
    sta CursorSound0
    jsr SoundEffectCH0
.endCH0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;CHANEL 1
.victorySongCH1
    lda GameOver
    and #WON_PLAY_VICTORY_SONG 
    beq .chanel1
    lda #AFTER_VICTORY_SONG
    sta GameOver
    ldy #ID_VICTORY_SONG_CH1
    jmp .playCH1
.chanel1
    ;ldx #1 ;chanel
    lda TempoCH1 ;sound until the tempo not reach the zero
    beq .introCH1
    dec TempoCH1 ;end of any sound effect
    bne .endCH1
.continuePlayCH1
    jsr SoundEffectCH1
    jmp .endCH1
;inizialize one of the sounds
.introCH1
    lda GameOver
    cmp #START_JINGLE ;case 7
    bne .myFireCH1
    lsr GameOver ;become case 6
    ldy #ID_INTRO_SONG_CH1 ;#01 ;IntroSongCh1
    jmp .playCH1
.myFireCH1
    lda MyFire
    beq .otherSfxCH1
    lda #0
    sta MyFire
    ldy #ID_SOUND_M0
    jmp .playCH1
.otherSfxCH1
    lda #$ff
    cmp IdSfx
    beq .endCH1
    ldy IdSfx
    sta IdSfx ;store #$ff
.playCH1
    lda TableSfxLow,y
    sta SoundDataLow1
    lda TableSfxHi,y
    sta SoundDataHi1
    lda #0
    sta CursorSound1
    jsr SoundEffectCH1
.endCH1
    lda #0
    sta MyFire
    ENDM
