
 ;*** The height of the displayed data...
bmp_48x2_3_window = 10

 ;*** The height of the bitmap data. This can be larger than 
 ;*** the displayed data height, if you're scrolling or animating 
 ;*** the data...
bmp_48x2_3_height = 10

   if >. != >[.+(bmp_48x2_3_height)]
      align 256
   endif
 BYTE 0 ; leave this here!


 ;*** The color of each line in the bitmap, in reverse order...
bmp_48x2_3_colors 
	BYTE _0E
	BYTE _0E
	BYTE _0E
	BYTE _0E
	BYTE _0E
	BYTE _0E
	BYTE _0E
	BYTE _0E
	BYTE _0E
	BYTE _0E

; ifnconst bmp_48x2_3_PF1
;bmp_48x2_3_PF1
; endif
;        BYTE %00001111
; ifnconst bmp_48x2_3_PF2
;bmp_48x2_3_PF2
; endif
;        BYTE %11111111
; ifnconst bmp_48x2_3_background
;bmp_48x2_3_background
; endif
;        BYTE $c2

   if >. != >[.+bmp_48x2_3_height]
      align 256
   endif


bmp_48x2_3_00
	BYTE %00111111
	BYTE %01100000
	BYTE %11000001
	BYTE %11000010
	BYTE %11000010
	BYTE %11000010
	BYTE %11000001
	BYTE %11000000
	BYTE %01100000
	BYTE %00111111
	BYTE %11000001

   if >. != >[.+(bmp_48x2_3_height)]
      align 256
   endif

bmp_48x2_3_01
	BYTE %11111111
	BYTE %00000000
	BYTE %00010000
	BYTE %01001000
	BYTE %11101000
	BYTE %01001000
	BYTE %00010000
	BYTE %00000000
	BYTE %00000000
	BYTE %11111111
	BYTE %00010000

   if >. != >[.+(bmp_48x2_3_height)]
      align 256
   endif

bmp_48x2_3_02
	BYTE %11111111
	BYTE %00000000
	BYTE %00110010
	BYTE %01001010
	BYTE %01001010
	BYTE %01001011
	BYTE %00110010
	BYTE %00000001
	BYTE %00000000
	BYTE %11111111
	BYTE %00110010

   if >. != >[.+(bmp_48x2_3_height)]
      align 256
   endif

bmp_48x2_3_03
	BYTE %11111111
	BYTE %00000000
	BYTE %01001101
	BYTE %01001001
	BYTE %01001001
	BYTE %01101001
	BYTE %01001000
	BYTE %00101001
	BYTE %00000000
	BYTE %11111111
	BYTE %01001000

   if >. != >[.+(bmp_48x2_3_height)]
      align 256
   endif

bmp_48x2_3_04
	BYTE %11111111
	BYTE %00000000
	BYTE %01001001
	BYTE %01001010
	BYTE %01001011
	BYTE %01001010
	BYTE %01110001
	BYTE %00000000
	BYTE %00000000
	BYTE %11111111
	BYTE %01110001

   if >. != >[.+(bmp_48x2_3_height)]
      align 256
   endif

bmp_48x2_3_05
	BYTE %11111100
	BYTE %00000110
	BYTE %10000011
	BYTE %00000011
	BYTE %11000011
	BYTE %01000011
	BYTE %10000011
	BYTE %00000011
	BYTE %00000110
	BYTE %11111100
	BYTE %10000011

