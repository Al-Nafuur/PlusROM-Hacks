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
        MAC JOYSTICK_MACRO
        lda #%00001110 ;case 3,4,5
        bit GameOver
        bne EndJoy0
        lda #TITLE_SCREEN ;case 1
        bit GameOver
        beq CheckJoy0Left ;if zero means in game
;check fire for start game
        lda #%10000000 ;fire?
	bit INPT4
        bne EndJoy0
        jsr initVariables

CheckJoy0Left
        lda CoordXMyTank
        cmp #LEFT_LIMIT_MY_TANK
        beq CheckJoy0Right
        ;half speed
        lda TimerForAll
        and #%00000001
        beq CheckJoy0Right
        ;end half speed
        lda #%01000000 ;left?
	bit SWCHA
        bne CheckJoy0Right
        dec CoordXMyTank
CheckJoy0Right
        lda CoordXMyTank 
        cmp #RIGHT_LIMIT_MY_TANK
        beq CheckFire

	lda #%10000000 ;right?
	bit SWCHA
        bne CheckFire

        inc CoordXMyTank
CheckFire
        lda GameOver
        and #MASK_SIGNAL_START_JINGLE_BEFORE_SHOOT ;both case 6 and 7
        bne EndJoy0 ;wait until you can shoot

        lda #%10000000 ;fire?
        bit INPT4
        bne preEndJoy0 ;no pressed then set EventFireButtonUp to false
    
        lda BooleanM0Runs ;check if missile0 is running
        ;bne EndJoy0 ;if already running then exit
        ora EventFireButtonUp
        bne EndJoy0
    
        ;lda #0
        ;sta RESMP0 ;missile0 unlocked from P0
        ;lda CoordXMyRobot ;missile starts where the robot IS
        ;sta CoordXMyMissile
        lda #TRUE ;if not running..
        sta BooleanM0Runs ;set it is running now
        sta EventFireButtonUp ;set true button pressed
        sta MyFire ;set Sfx
        
        
        clc
        lda CoordXMyTank
        adc #RANGE_MISSILE_0
        sta Missile0Range ;how long can go

        jmp EndJoy0
preEndJoy0
        lda BooleanM0Runs ;set EventFireButtonUp
        bne EndJoy0 ;only with no missile on screen
        lda #0 ;no missile then
        sta EventFireButtonUp ;set false because no pressed

EndJoy0
        ENDM