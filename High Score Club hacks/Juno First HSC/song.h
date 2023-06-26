; Silkworm title theme - chip version
; adapted for the Atari 2600 by Erik Ehrling (Sweden)
; email: "erik_ehrling 'at' hotmail 'dot' com"
;
; Last updated 15th November 2005
;-------------------------------------------------------------
; SONG DATA FILE
;-------------------------------------------------------------
; This song uses a music driver by Paul Slocum (from Music 
; Kit v2.0). The music driver is used here with a minor 
; modification in songplay.h to allow for more than 32
;
;-------------------------------------------------------------
; Tempo
;-------------------------------------------------------------

TEMPODELAY equ 5 ; 4 for PAL, 5 for NTSC

;-------------------------------------------------------------
; Sound Attenuation Array
;-------------------------------------------------------------
;
; 000 = Square  001 = Pitfall  010 = Saw  011 = Noise
; 100 = Buzz    101 = Bass     110 = Saw  111 = Lead

soundTurnArray
	byte 8, 6, 8, 6
	byte 5, 6, 7, 10

;-------------------------------------------------------------
; Sound Type Array
;-------------------------------------------------------------
;
; Sound Type Setup:
; 000 0 Square  = 4
; 001 1 Pitfall = 7
; 010 2 Saw     = 1 (lower volume)
; 011 3 Noise   = 8
;
; 100 4 Buzz    = 15
; 101 5 Bass    = 6
; 110 6 Saw     = 1 (higher volume)
; 111 7 Lead    = 12

soundTypeArray
    byte 4,7,1,8
    byte 15,6,1,12

;-------------------------------------------------------------
; Auto High Hat
;-------------------------------------------------------------

hatPattern
	byte %10101010
	byte %10101010
	byte %10101010
	byte %10101010

HATSTART equ 8

HATVOLUME equ 2
HATPITCH equ 1
HATSOUND equ 8

;-------------------------------------------------------------
; Song Data
;-------------------------------------------------------------

song1
	byte 1, 2
	byte 1, 5

	byte 6, 7
	byte 6, 8

	byte 9, 15
	byte 16, 18

	byte 9, 15
	byte 16, 18

	byte 9, 15
	byte 16, 18

	byte 28, 29
	byte 30, 31

	byte 36, 37
	byte 38, 39

	byte 48, 49
	byte 50, 51

	byte 48, 49
	byte 50, 51

	byte 52, 53
	byte 54, 55

	byte 48, 49
	byte 50, 51

	byte 6, 7
	byte 6, 8

	byte 9, 15
	byte 16, 18

	byte 28, 29
	byte 30, 31

	byte 36, 37
	byte 38, 39

	byte 60, 61
	byte 62, 63

	; End of song marker
	byte 255

song2
	byte 0, 0
	byte 0, 10

	byte 11, 12
	byte 11, 13

	byte 14, 14
	byte 17, 19

	byte 20, 21
	byte 22, 23

	byte 24, 25
	byte 26, 27

	byte 32, 33
	byte 34, 128

	byte 40, 41
	byte 42, 43

	byte 44, 45
	byte 46, 47

	byte 44, 45
	byte 46, 47

	byte 56, 57
	byte 58, 59

	byte 44, 45
	byte 46, 47

	byte 11, 12
	byte 11, 13

	byte 24, 25
	byte 26, 27

	byte 32, 33
	byte 34, 128

	byte 40, 41
	byte 42, 43

	byte 3, 4
	byte 35, 129
		
	; End of song marker
	byte 255

;-------------------------------------------------------------
; Pattern Arrays
;-------------------------------------------------------------

	; Higher volume patterns, starts at 0
patternArrayH
	word mute,mute,mute,mute			    ; 0 Muted pattern
	word perc1, perc2, perc1, perc2			    ; 1
	word perc1, perc2, perc7, perc8			    ; 2
	word stickB16, stickB17, stickB18, stickB19	    ; 3

	word stickB20, stickB17, stickB22, stickB23	    ; 4
	word perc1, perc2, melody15, melody16		    ; 5  
	word melody17, melody18, melody19, melody18         ; 6
	word melody17, melody18, melody23, melody24         ; 7

	word melody17, melody18, melody23, melody25         ; 8
	word melody26a, melody27a, melody26a, melody27a	    ; 9
	word mute, mute, melodyB15, melodyB16		    ; 10
	word melodyB17, melodyB18, melodyB19, melodyB18     ; 11

	word melodyB17, melodyB18, melodyB23, melodyB24     ; 12
	word melodyB17, melodyB18, melodyB23, melodyB25     ; 13
	word melodyB26, melodyB27, melodyB26, melodyB27	    ; 14
	word melody26a, melody27a, melody26d, melody27d	    ; 15

	word melody28a, melody29a, melody28a, melody29a     ; 16
	word melodyB28, melodyB29, melodyB28, melodyB29     ; 17
	word melody30a, melody31a, melody30b, melody31b     ; 18
	word melodyB30, melodyB31, melodyB30, melodyB31     ; 19

	word melodyB32, melodyB27, melodyB34, melodyB35     ; 20
	word melodyB32, melodyB27, melodyB38, melodyB39     ; 21
	word melodyB40, melodyB41, melodyB42, melodyB43     ; 22
	word melodyB44, melodyB31, melodyB46, melodyB47     ; 23

	word melodyB48, melodyB27, melodyB50, melodyB51     ; 24
	word melodyB48, melodyB27, melodyB54, melodyB55     ; 25
	word melodyB56, melodyB29, melodyB58, melodyB59     ; 26
	word melodyB60, melodyB31, melodyB62, melodyB31     ; 27

	word melody64,  melody65,  melody64,  melody65      ; 28
	word melody64,  melody65,  melody70,  melody71      ; 29
	word melody72,  melody73,  melody72,  melody73      ; 30
	word melody76,  melody77,  melody78,  melody79      ; 31

	word melodyB64, melodyB65, melodyB50, melodyB51     ; 32
	word melodyB68, melodyB65, melodyB70, melodyB51     ; 33
	word melodyB72, melodyB73, melodyB74, melodyB75     ; 34
	word melodyB80, stickB25,  stickB26,  stickB27	    ; 35

	word melody80,  melody81,  melody82,  melody83      ; 36
	word melody84,  melody85,  melody86,  melody87      ; 37
	word melody88,  melody89,  melody90,  melody91      ; 38
	word melody92,  melody93,  melody94,  melody95      ; 39
	
	word melodyB80, melodyB81, melodyB82, melodyB83     ; 40
	word melodyB84, melodyB85, melodyB86, melodyB87     ; 41
	word melodyB88, melodyB89, melodyB90, melodyB91     ; 42
	word melodyB92, melodyB93, melodyB94, melodyB95     ; 43
	
	word melodyB96,  melodyB97,  melodyB98,  melodyB99  ; 44
	word melodyB100, melodyB101, melodyB100, melodyB27  ; 45
	word melodyB104, melodyB105, melodyB106, melodyB107 ; 46
	word melodyB108, melodyB109, melodyB110, melodyB31  ; 47		
	
	word melody26a,  melody27a,  melody26a,  melody27a  ; 48
	word melody26a,  melody27a,  melody26d,  melody27d  ; 49
	word melody28a,  melody29a,  melody28a,  melody29a  ; 50
	word melody30a,  melody31a,  melody30b,  melody31b  ; 51		

	word stick00, stick01, stick02, stick03  	    ; 52
	word stick04, stick05, stick06, stick07  	    ; 53
	word stick08, stick09, stick10, stick11  	    ; 54
	word stick12, stick13, stick14, stick15  	    ; 55

	word stickB00, stickB01, stickB02, stickB03  	    ; 56
	word stickB04, stickB05, stickB06, stickB07  	    ; 57
	word stickB08, stickB09, stickB08, stickB09  	    ; 58
	word stickB12, stickB13, stickB14, stickB15  	    ; 59

	word stick16, stick17, stick18, stick19		    ; 60
	word stick16, stick17, stick22, stick23		    ; 61
	word stick16, stick17, stick26, stick27		    ; 62
	word stick28, stick17, stick22, stick31		    ; 63

	
	; Lower volume patterns, start at 128
patternArrayL 					
	word melodyB76,  melodyB77,  melodyB78,  melodyB79  ; 128
	word stickB28, stickB29, stickB30, mute	            ; 129
	
;-------------------------------------------------------------
; Pattern Data
;-------------------------------------------------------------

perc1 ; p0
	byte %10011001, 255
	byte 255, 255
	byte %01100100, 255
	byte %10011001, 255
	
	byte %00000000
	
perc2 ; p1
	byte 255, 255	
	byte %10011001, 255
	byte %01100100, 255
	byte %10011001, 255

	byte %00000000

perc7 ; p2
	byte %10011001, 255
	byte 255, 255
	byte %01100100, 255
	byte 255, 255
	
	byte %00000000

perc8 ; p3
	byte 255, 255
	byte 255, 255
	byte %01100100, 255
	byte 255, 255
	
	byte %00000000

perc15 ; p4
	byte %10011001, 255
	byte %10011001, 255
	byte %01100100, 255
	byte %10011001, 255

	byte %00000000

melody15 ; p5
        byte %10011001, %10110001
	byte %10011001, 255 
	byte %01100100, 255 
	byte %10011001, %10110001

	byte %00000000

melody16 ; p6
	byte %10011001, 255 
	byte %10011001, 255 
        byte %01100100, %10110001
	byte %10011001, 255 

	byte %00000000

melody17 ; p7
        byte %10011001, %10110011
        byte %10110001, %10110001
	byte %01100100, 255
	byte %10011001, 255 
	
	byte %00000000

melody18 ; p8
        byte %10101110, %10101110
        byte %10011001, %10110001
	byte %01100100, 255 
	byte %10011001, 255

	byte %00000000

melody19 ; p9
        byte %10011001, %10110011
        byte %10110001, %10110001
        byte %01100100, %10110011
        byte %10011001, %10110001
	
	byte %00000000

melody23 ; p10
        byte %10011001, %10110011
	byte %10011001, 255
        byte %01100100, %10110011
	byte %10011001, 255
	
	byte %00000000

melody24 ; p11
	byte %10110001, %10110001
	byte %10011001, 255
	byte %01100100, 255
	byte %10011001, 255

	byte %11000000
	
melody25 ; p12
	byte %10011001, %10110001
	byte %10011001, %10110001
	byte %01100100, %10110001
	byte %10011001, %10110001

	byte %11000000

melody26a ; p13
        byte %10011001, %10110011
	byte %10110001, %10110001
	byte %01100100, %10110001
	byte %10011001, %10110001

	byte %00000000

melody27a ; p14 
        byte %10101110, %10101110
	byte %10011001, %10110001
	byte %01100100, %10110001
	byte %10011001, %10110001

	byte %00000000

melody26d ; p15
        byte %10011001, %10110011
	byte %10110001, %10110001
	byte %01100100, %10110001
	byte %10110001, %10110001

	byte %00000000

melody27d ; p16
        byte %10101110, %10101110
	byte %10110001, %10110001
	byte %01100100, %10110001
	byte %10110001, %10110001

	byte %00000000

melody28a ; p17
	byte %10011001, %10110101
        byte %10110011, %10110011
        byte %01100100, %10110011
        byte %10011001, %10110011

	byte %00000000

melody29a ; p18
	byte %10110000, %10110000
        byte %10011001, %10110011
        byte %01100100, %10110011
        byte %10011001, %10110011

	byte %00000000
	
melody30a ; p19
        byte %10011001, %10111101
        byte %10111010, %10111010
        byte %01100100, %10111010
        byte %10011001, %10111010
        
	byte %00000000        
	
melody31a ; p20
        byte %10110101, %10110101
        byte %10011001, %10111010
        byte %01100100, %10111010
        byte %10011001, %10111010
        
	byte %00000000        

melody30b ; p21
        byte %10011001, %10111101
        byte %10011001, %10111010
        byte %01100100, %10111010
        byte %10011001, %10111010
        
	byte %00000000        

melody31b ; p22
        byte %10011001, %10110101
        byte %10011001, %10111010
        byte %01100100, %10111010
        byte %10011001, %10111010
        
	byte %00000000        

melody64 ; p23
	byte %10011001, %10110011
	byte %10110001, %10110001
	byte %01100100, 255
	byte %10011001, %10110001
	
	byte %00000000

melody65 ; p24
	byte 255, 255	
	byte %10011001, %10110001
	byte %01100100, %10110001
	byte %10011001, %10110001

	byte %00000000

melody70 ; p25
	byte %10011001, %10110011
	byte %10110001, %10110001
	byte %01100100, 255
	byte %10110001, %10110001
	
	byte %00000000

melody71 ; p26
	byte 255, 255
	byte %10110001, %10110001
	byte %01100100, %10110001
	byte %10110001, %10110001
	
	byte %00000000

melody72 ; p27
	byte %10011001, %10110101
	byte %10110011, %10110011
	byte %01100100, 255
	byte %10011001, %10110011

	byte %00000000

melody73 ; p28
	byte 255, 255	
	byte %10011001, %10110011
	byte %01100100, %10110011
	byte %10011001, %10110011

	byte %00000000

melody76 ; p29
	byte %10011001, %10111010
	byte 255, 255	
	byte %01100100, 255
	byte %10011001, %10111010

	byte %00000000

melody77 ; p30
	byte 255, 255	
	byte %10011001, 255
	byte %01100100, %10111010
	byte %10011001, %11101001

	byte %00000001

melody80 ; p33

	byte %10110011, %10110011
	byte %10101001, %10101001
	byte %01100100, %10110011
	byte %10011001, %10110011
	
	byte %11000000
	
melody81 ; p34
	byte %10101001, %10101001
	byte %10011001, %10110011
	byte %01100100, %10110011
	byte %10110011, %10110011

	byte %00000111
	
melody82 ; p35
	byte %10110011, %10110011
	byte %10101001, %10101001
	byte %01100100, %10101001
	byte %10111010, %10111010

	byte %11000011

melody83 ; p36
	byte %10101001, %10101001
	byte %10011001, %10110011
	byte %01100100, %10110011
	byte %10011001, %10110011

	byte %00000100

melody84 ; p37
	byte %10110000, %10110000
	byte %10101001, %10101001
	byte %01100100, %10110011
	byte %10011001, %10110011

	byte %11000000

melody85 ; p38
	byte %10101001, %10101001
	byte %10011001, %10110011
	byte %01100100, %10110000
	byte %10110000, %10110000

	byte %00000111

melody86 ; p39

	byte %10110000, %10110000
	byte %10101001, %10101001
	byte %01100100, %10101001
	byte %10110110, %10110110
	
	byte %11000011

melody87 ; p40

	byte %10101001, %10101001
	byte %10110011, %10110011
	byte %01100100, %10110000
	byte %10110011, %10110011
	
	byte %00000100

melody88 ; p41
	byte %10101110, %10101110
        byte %10101010, %10101010
	byte %01100100, %10110101
	byte %10011001, %10110101

	byte %11000000

melody90 ; p43
	byte %10101110, %10101110
        byte %10101010, %10101010
	byte %01100100, %10101010
	byte %10101110, %10101110

	byte %11000011

melody92 ; p45
	byte %10101011, %10101011
        byte %10101110, %10101110
	byte %01100100, %10111101
	byte %10011001, %10111101

	byte %11000000

melody93 ; p46
	byte %10101110, %10101110
	byte %10011001, %10111101
	byte %01100100, %10111101
	byte %10011001, %10101110

	byte %00000000

melody94 ; p47
	byte %10101110, %10101110
	byte %10011001, %10101110
	byte %01100100, %10101110
	byte %10011001, %10111101

	byte %11000000

melody95 ; p48
	byte %10011001, %10101110
	byte %10011001, %10111101
	byte %01100100, %10101110
	byte %10011001, %10111101

	byte %00000000

stick00 ; p49
	byte %10011001, %00010001
	byte %10110011, 255
	byte %10110011, 255
	byte 255, 255
	
	byte %00101000
	
stick01 ; p50
	byte %01100100, 255
	byte 255, 255
	byte %10011001, %00010011
	byte 255, 255
	
	byte %00000000	

stick02 ; p51
	byte %10101001, %00010001
	byte %10101001, 255
	byte %10110011, %00010011 
	byte 255, 255
	
	byte %10101000

stick03 ; p52
	byte %01100100, %00010111
	byte %10101001, 255
	byte %10110101, %00011101
	byte %10101001, 255
	
	byte %00101010	

stick04 ; p53
	byte %10110011, %00010001
	byte 255, 255
	byte %10110011, 255
	byte %10110011, 255
	
	byte %11001010

stick05 ; p54
	byte %01100100, 255
	byte 255, 255
	byte %10011001, %00001110
	byte %10011001, 255
	
	byte %00000000

stick06 ; p55
	byte %10101001, %00010001
	byte %10101001, 255
	byte %10110000, %00001011
	byte %10011001, %00001100
	
	byte %10101000

stick07 ; p56
	byte %01100100, %00001110
	byte %10101001, %00001100
	byte %01100100, %00001110
	byte %10101001, %00001111
	
	byte %00100010

stick08 ; p57
	byte %10110101, %00010011
	byte 255, %00001111
	byte %01100100, %00001100
	byte %10110101, %00010011

	byte %10000010

stick09 ; p58
	byte 255, %00001111
	byte %10011001, %00001001
	byte %01100100, %00010011
	byte 255, %00001111

	byte %00000000

stick10 ; p59
	byte %10011001, %00010011
	byte 255, %00001111
	byte %10011001, %00001100
	byte 255, %00010011

	byte %10001000

stick11 ; p60
	byte %10011001, %00001111
	byte 255, %00001001
	byte %10011001, %00010011
	byte 255, %00001111

	byte %10001000

stick12 ; p61
	byte %10011001, %00001100
	byte %10111101, 255
	byte %01100100, 255
	byte %10011001, %00001100

	byte %10100010

stick13 ; p62
	byte 255, 255
	byte %10011001, 255
	byte %01100100, %00001100
	byte %10011001, 255

	byte %00100100

stick14 ; p63
	byte %10011001, %00001010
	byte %10101110, 255            	     
	byte %01100100, 255
	byte %10101110, %00001010

	byte %10100011

stickB00 ; p65

	byte %00010001, %11110001
	byte %11010011, 255
	byte %11010011, 255
	byte 255, 255
	
	byte %10001000	

stickB01 ; p66
	byte 255, 255
	byte 255, 255
	byte %00010011, %11110011
	byte %11110011, 255
	
	byte %00001000

stickB02 ; p67
	byte %00010001, %11110001
	byte %11110001, 255
	byte %00010011, %11110011
	byte %11110011, 255
	
	byte %10001100

stickB03 ; p68
	byte %00010111, %11110111
	byte %11110111, 255
	byte %00011101, %11111101
	byte %11111101, 255
	
	byte %10001000

stickB04 ; p69
	byte %00010001, %11110001
	byte %11110001, 255
	byte %11010011, 255
	byte %11010011, 255
	
	byte %10001010

stickB05 ; p70
	byte 255, 255
	byte 255, 255
	byte %00001110, %11101110
	byte %11101110, 255
	
	byte %00001000

stickB06 ; p71
	byte %00010001, %11110001
	byte %11110001, 255
	byte %00001011, %11101011
	byte %00001100, %11101100
	
	byte %10001111

stickB08 ; p73
	byte %00010011, %11110011
        byte %00001111, %11101111
	byte %00001100, %11101100
	byte %00010011, %11110011
	
	byte %01010101

stickB09 ; p74 
	byte %00001111, %11101111
	byte %00001001, %11101001
	byte %00010011, %11110011
	byte %00001111, %11101111
	
	byte %01010101

stick15 ; p64
	byte %01100100, 255
	byte %01100100, 255
	byte %10101110, %00001010
	byte %01100100, 255

stickB12 ; p75
	byte %00001100

	byte %11101100
	byte %11101100, 255
	byte 255, 255
	byte %00001100, %11101100
	
	byte %11000011

stickB13 ; p76
	byte %11101100, 255
	byte %10101110, 255
	byte %00001100, %11101100
	byte %11101100, 255

stickB15 ; p78	
	byte %10101110

	byte 255
	byte %10111101, 255
stickB14 ; p77	
	byte %00001010, %11101010
	byte %11101010, 255
	
	byte %10101110
	
	byte 255
	byte %00001010, %11101010
	
	byte %11101011

melodyB15 ; p79
        byte %01010001, %01010001
	byte 255, 255
	byte 255, 255
	byte %01010001, %01010001

	byte %10000010

melodyB16 ; p80
	byte 255, 255
	byte 255, 255
        byte %01010001, %01010001
	byte 255, 255

	byte %00000000

melodyB17 ; p81
        byte %01010011, %01010011
        byte %01010001, %01010001
	byte 255, 255
	byte 255, 255
	
	byte %00000000
	
melodyB18 ; p82
        byte %01001110, %01001110
        byte %01010001, %01010001
	byte 255, 255
	byte 255, 255

	byte %00000000

melodyB19 ; p83
        byte %01010011, %01010011
        byte %01010001, %01010001
        byte %01010011, %01010011
        byte %01010001, %01010001
	
	byte %00000000
	
melodyB23 ; p84	
        byte %01010011, %01010011
	byte 255, 255
        byte %01010011, %01010011
	byte 255, 255
	
	byte %10000000

melodyB24 ; p85
	byte %01010001, %01010001
	byte 255, 255
	byte 255, 255
	byte 255, 255

	byte %10000000
	
melodyB25 ; p86
	byte %01010001, 255
	byte %01010001, 255
	byte %01010001, 255
	byte %01010001, 255

	byte %10000000

melodyB26 ; p87
        byte %01010011, 255
	byte %01010001, 255
	byte %01010001, 255
	byte %01010001, 255

	byte %00000000

melodyB27 ; p88
        byte %01001110, 255
	byte %01010001, 255
	byte %01010001, 255
	byte %01010001, 255

	byte %00000000

melody78 ; p31
	byte %10011001, %11101100
	byte %10011001, %11110011
	byte %01100100, %11101100
	byte %10011001, %11101001

melodyB28 ; p89
	byte %01010101

	byte 255
        byte %01010011, 255
        byte %01010011, 255
        byte %01010011, 255

	byte %00000000

melodyB29 ; p90
	byte %01010000, 255
        byte %01010011, 255
        byte %01010011, 255
        byte %01010011, 255

	byte %00000000

melodyB30 ; p91
        byte %01011101, 255
        byte %01011010, 255
        byte %01011010, 255
        byte %01011010, 255
        
	byte %00000000        

melody79 ; p32
	byte %10011001, %11101100
	byte %10011001, %11110011
	byte %01100100, %11101100
	byte %10011001, %11110011

melodyB31 ; p92
	byte %01010101

        byte 255
        byte %01011010, 255
        byte %01011010, 255
        byte %01011010, 255
        
	byte %00000000        

melodyB32 ; p93
	byte %01010001, %01010001
	byte %01010001, 255
	byte %01010001, 255
	byte %01010001, 255
	
	byte %11000000        

melodyB34 ; p94
        byte %01010011, 255
	byte %01010001, 255
	byte %01010001, 255
        byte %01010011, %01010011
	
	byte %00000011        	

melodyB38 ; p96
        byte %01010011, 255
	byte %01010001, 255
	byte %01010001, 255
        byte %01001110, %01001110

	byte %00000011        	

melodyB40 ; p98
        byte %01010011, %01010011
        byte %01010011, 255
        byte %01010011, 255
        byte %01010011, 255
	
	byte %11000000

melodyB42 ; p100
        byte %01010011, %01010011
        byte %01010011, %01010011
	byte %01010011, 255
        byte %01010011, %01010011

	byte %10100010

melodyB43 ; p101	
	byte %01010000, 255
        byte %01010011, %01010011
        byte %01010011, %01010011
        byte %01010011, %01010011

	byte %00101010

melodyB44 ; p102
	byte %01011010, %01011010
        byte %01011010, 255
        byte %01011010, 255
        byte %01011010, 255

	byte %11000000
	
melodyB46 ; p103
        byte %00011000, 255
        byte %11110000, 255
        byte %11110000, 255
        byte %00011000, 255

	byte %10100010        
	

melodyB48 ; p105
        byte %11010001, %01010001
	byte %01010001, 255
	byte %01010001, 255
	byte %01010001, 255

	byte %11000000        
	
melodyB50 ; p106
        byte %01010011, 255
	byte %01010001, 255
        byte %11010001, %01010001
	byte %01010001, 255

	byte %00001100        

melodyB51 ; p107
        byte %11010011, %01010011
	byte %01010001, 255
        byte %11010011, %01010011
	byte %01010001, 255

	byte %11001100        

melodyB54 ; p108
        byte %01010011, 255
	byte %01010001, 255
        byte %11001110, %01001110
	byte %01010001, 255

	byte %00001100        

melodyB55 ; p109
	byte %11001110, %01001110
	byte %01010001, 255
        byte %11001110, %01001110
	byte %01010001, 255

	byte %11001100        

melodyB56 ; p110
	byte %11010011, %01010011
        byte %01010011, 255
        byte %01010011, 255
        byte %01010011, 255

	byte %11000000        

stickB07 ; p72
        byte %00001110, %11101110
	byte %00001100, %11101100
        byte %00001110, %11101110
        byte %00001111, %11101111

melodyB58 ; p111
	
	byte %01010101

	byte 255
        byte %01010011, 255
       	byte %11010011, %01010011
        byte %01010011, 255

	byte %00001100        

melodyB59 ; p112
       	byte %11010011, %01010011
        byte %01010011, 255
	byte %11010011, %01010011
        byte %01010011, 255

	byte %11001100        

melodyB60 ; p113
        byte %11011010, %01011010
        byte %01011010, 255
        byte %01011010, 255
        byte %01011010, 255

	byte %11000000        

melodyB62 ; p114
        byte %00101100, %00101100
        byte %01011010, 255
        byte %01011010, 255
        byte %01011010, 255

	byte %10000000        

melodyB64 ; p115
        byte %11010001, %01010001
	byte %01010001, 255
	byte 255, 255 
	byte %01010001, 255
	
	byte %11000000

melodyB65 ; p116
	byte 255, 255	
	byte %01010001, 255
	byte %01010001, 255
	byte %01010001, 255

	byte %00000000

melodyB68 ; p117
	byte %11010111, %01010111
	byte %01010001, 255
	byte 255, 255	
	byte %01010001, 255

	byte %11000000

melodyB70 ; p118
	byte %01010011, 255
	byte %01010001, 255
	byte %11010111, %01010111
	byte %01010001, 255
	
	byte %00001100

melodyB72 ; p119
	byte %11010101, %01010101
	byte %11010011, %01010011
	byte 255, 255
	byte %01010011, 255

melodyB47 ; p104
	byte %11110000

        byte 255
        byte %11110000, 255
       	byte %00011000, 255
        byte %11110000, 255
        
	byte %10001000        

melodyB73 ; p120
	byte 255, 255	
	byte %01010011, 255
	byte %01010011, 255
	byte %01010011, 255

	byte %00000000

melodyB74 ; p121
	byte %11010101, %01010101
	byte %11010011, %01010011
	byte 255, 255  
	byte %11010101, %01010101

	byte %11001111

melodyB75 ; p122
	byte %11010011, %01010011
	byte %01010011, 255
	byte %11010101, %01010101
	byte %11010011, %01010011

	byte %11001111

melodyB76 ; p123
        byte %11011010, %01011010
	byte 255, 255	
	byte 255, 255	
	byte %01011010, 255

	byte %11000010

melodyB35 ; p95
        byte %01001110, 255
	byte %01010001, 255
        byte %01010011, %01010011
	byte %01010001, 255

melodyB78 ; p125
	byte %00001100        	

	byte %01011010
	byte %00010011, %01011010
	byte %00001100, %11011010
	byte %00001001, %01011010

	byte %00000010

melodyB39 ; p97
        byte %01001110, 255
	byte %01010001, 255
        byte %01001110, %01001110
	byte %01010001, 255

melodyB79 ; p126
	byte %00001100        	

	byte %01011010
	byte %00010011, %11011010
	byte %00001100, %01011010
	byte %00010011, %01011010

	byte %00000010

melodyB80 ; p127
	byte %11010011, %11010011
	byte %01001001, 255
	byte %01010011, 255
	byte %01010011, 255
	
	byte %11000000
	
	
melodyB82 ; p129
	byte %11010011, %11010011
	byte %01001001, 255
	byte %01010011, 255
	byte %11011010, %11011010

	byte %11000011

melodyB83 ; p130
	byte %01001001, 255
	byte %01010011, 255
	byte %11010011, %11010011
	byte %01010011, 255

	byte %00001100

melodyB84 ; p131
	byte %11010000, %11010000
	byte %01001001, 255
	byte %01010011, 255
	byte %01010011, 255

	byte %11000000

melodyB85 ; p132
	byte %01001001, 255
	byte %01010011, 255
	byte %11010000, %11010000
	byte %11010000, %11010000

	byte %00001010

melodyB86 ; p133
	byte %11010000, %11010000
	byte %01001001, 255
	byte %01001001, 255
	byte %11010110, %11010110
	
	byte %11000011

melodyB87 ; p134
	byte %01001001, 255
	byte %01010011, 255
	byte %11010000, %11010000
	byte %01010011, 255
	
	byte %00001100

melodyB88 ; p135
	byte %11001110, %11001110
        byte %01001010, 255
	byte %01010101, 255
	byte %01010101, 255

	byte %11000000

melodyB90 ; p137
	byte %11001110, %11001110
        byte %01001010, 255
        byte %01001010, 255
	byte %11001110, %11001110

	byte %11000011

melodyB91 ; p138
        byte %01001010, 255
	byte %01010101, 255
        byte %11001110, %11001110
	byte %01010101, 255

	byte %00001100

melodyB92 ; p139
	byte %11001011, %11001011
	byte %01001110, 255
	byte %01011101, 255
	byte %01011101, 255

	byte %11000000
	
melodyB93 ; p140
	byte %01001110, 255
	byte %01011101, 255
	byte %01011101, 255
	byte %01001110, 255

	byte %00000000

melodyB94 ; p141
	byte %11001110, %11001110
	byte %01001110, 255
	byte %01001110, 255
	byte %01011101, 255

	byte %11000000

melodyB95 ; p142
	byte %01001110, 255
	byte %01011101, 255
	byte %01001110, 255
	byte %01011101, 255

	byte %00000000

melodyB96 ; p143
	byte %00001001, %00001001
	byte %00001110, %00001110
	byte %00001001, %00001001
	byte %00001110, %00001110

melody89 ; p42	
	byte %10101010

        byte %10101010
	byte %10011001, %10110101
	byte %01100100, %10101110
	byte %10101110, %10101110

	byte %00000111

melodyB97 ; p144
	byte %00010011, %00010011
	byte %00010011, 255
	byte %01010001, 255
        byte %01010001, 255
	
	byte %01000000

melodyB98 ; p145
        byte %00010000, %00010000
        byte %01010001, 255
	byte %01010001, 255
        byte %01010001, 255

	byte %10000000

melodyB99 ; p146
	byte %00001110, %00001110
        byte %01010001, 255
	byte %00010011, %00010011
        byte %01010001, 255

melody91 ; p44

	byte %10101010

        byte %10101010
	byte %10011001, %10110101
	byte %01100100, %10101110
	byte %10011001, %10110101

	byte %00000100

melodyB100 ; p147
	byte %00011101, %00011101
	byte %00011101, 255
	byte %01010001, 255
        byte %01010001, 255

melodyB89 ; p136

	byte %01001010

        byte 255
	byte %01010101, 255
	byte %11001110, %11001110
        byte %11001110, %11001110

	byte %00001010

melodyB101 ; p148
	byte %01001110, 255
        byte %01010001, 255
	byte %11101010, %11101010
	byte %11101010, 255

	byte %00001100

melodyB104 ; p149
	byte %00010000, %00010000
	byte %00010101, %00010101
	byte %00010000, %00010000
	byte %00010101, %00010101

	byte %10101010

melodyB41 ; p99
	byte %01010000, 255
        byte %01010011, 255 
        byte %01010011, %01010011 				
        byte %01010011, 255 

melodyB105 ; p150	
	byte %00001100

	byte %00001100
	byte %00010000, %00010000
	byte %00010101, %00010101
	byte %00010000, %00010000

	byte %10101010

melodyB77 ; p124
	byte 255, 255	
	byte 255, 255	
	byte %01011010, 255
	byte %00001001, %11011010	

melodyB106 ; p151

	byte %00001010

	byte %00001010
	byte %00001000, %00001000
	byte %00001010, %00001010	
	byte %00001100, %00001100

	byte %10001010

melodyB81 ; p128
	byte %01001001, 255 
	byte %01010011, 255 
	byte %11010011, %11010011
	byte %11010011, %11010011

melodyB107 ; p152

	byte %00001010

	byte %00001010	
	byte %00001100, %00001100
	byte %00010000, %00010000
	byte %01010011, 255

	byte %10101010

melodyB108 ; p153
	byte %00010001, %00010001
	byte %00010001, 255
        byte %01011010, 255
	byte %00010001, %00010001

	byte %11000011

melodyB109 ; p154
	byte %00010001, 255
        byte %01011010, 255
	byte %00010001, %00010001
        byte %01011010, 255

	byte %00001000

melodyB110 ; p155
	byte %00010101, %00010101
	byte %00010101, 255
        byte %01011010, 255
        byte %01011010, 255

	byte %11000000

stick16 ; p156
	byte %01100100, %10110011  
	byte %01100100, 255  
	byte %01100100, 255  
	byte 255, 255

	byte %01000000

stick17 ; p157
	byte 255, 255
	byte 255, 255
	byte %10011001, 255 
	byte 255, 255

	byte %00000000

stick18 ; p158
	byte %10011001, 255 
	byte 255, 255
	byte 255, 255
	byte %10011001, 255 

	byte %00000000

stick19 ; p159
	byte 255, 255
	byte %10011001, 255 
	byte %10011001, %10110000 
	byte %10011001, 255 

	byte %00000100

stick22 ; p160
	byte %10011001, 255 
	byte 255, 255
	byte 255, 255
	byte 255, 255

	byte %00000000

stick23 ; p161
	byte 255, 255
	byte 255, 255
	byte %10101100, 255
	byte 255, 255

	byte %00000000

stick26 ; p162
	byte %10011001, 255
	byte %10011001, 255
	byte 255, 255
	byte %10011001, %10101100

	byte %00000000

stick27; p163
	byte 255, 255
	byte %10011001, 255 
	byte %10011001, %10110011 
	byte %10011001, 255 

	byte %00000000

stick28; p164
	byte %01100100, %10110011  
	byte %01100100, 255  
	byte %01100100, 255  
	byte 255, 255

	byte %00000000

stick31; p165
	byte %01100100, 255  
	byte %01100100, 255  
	byte %01100100, 255  
	byte %01100100, 255  

	byte %00000000

stickB16; p166
	byte %11010011, %11010011
	byte %01010011, 255
	byte %01010011, 255
	byte %01010011, 255

	byte %11000000

stickB17; p167
	byte %01010011, 255
	byte %01001001, 255
	byte %01010011, 255
	byte %01001001, 255

	byte %00000000

stickB18; p168 
	byte %01001001, 255
	byte 255, 255
	byte %01001001, 255
	byte 255, 255

	byte %00000000

stickB19; p169 
	byte %01001001, 255
	byte 255, 255
        byte %11010000, %11010000
	byte 255, 255

	byte %00001100

stickB20; p170
	byte %11010011, %11010011
	byte %01010011, 255
	byte %01010011, 255
	byte %01010011, 255

	byte %10000000

stickB22; p171 
	byte 255, 255
	byte %01001001, 255
	byte %01001001, 255
	byte 255, 255

	byte %00000000

stickB23; p172
	byte %01001001, 255
	byte 255, 255
        byte %00101100, %00101100
	byte 255, 255

	byte %00000000

stickB25; p173
	byte %01010011, 255
	byte %01001001, 255
	byte 255, 255
	byte 255, 255

	byte %00000000

stickB26; p174
        byte %00101001, %01001001
	byte 255, 255
	byte 255, 255
        byte %00101100, %00101100

	byte %00000000

stickB27; p175
	byte 255, 255
	byte 255, 255
	byte %11010011, %11010011
	byte 255, 255

	byte %00001100

stickB28; p176
	byte %11010011, %11010011
	byte %11010011, 255
	byte %11010011, 255
	byte %11001001, 255

	byte %11101010

stickB29; p177
	byte 255, 255
	byte 255, 255
	byte %11001001, 255
	byte 255, 255

	byte %00001000

stickB30; p178
	byte %11001001, 255
	byte 255, 255
	byte 255, 255
	byte 255, 255

	byte %10000000

mute; p179

	byte 255, 255, 255, 255
	byte 255, 255, 255, 255

	byte 255
