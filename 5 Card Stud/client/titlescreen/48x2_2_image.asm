
 ;*** The height of the displayed data...
bmp_48x2_2_window = 10

 ;*** The height of the bitmap data. This can be larger than 
 ;*** the displayed data height, if you're scrolling or animating 
 ;*** the data...
bmp_48x2_2_height = 10

   if >. != >[.+(bmp_48x2_2_height)]
      align 256
   endif
 BYTE 0 ; leave this here!


 ;*** The color of each line in the bitmap, in reverse order...
bmp_48x2_2_colors 
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

; ifnconst bmp_48x2_2_PF1
;bmp_48x2_2_PF1
; endif
;        BYTE %00000000
; ifnconst bmp_48x2_2_PF2
;bmp_48x2_2_PF2
; endif
;        BYTE %11111000
; ifnconst bmp_48x2_2_background
;bmp_48x2_2_background
; endif
;        BYTE _00


   if >. != >[.+bmp_48x2_2_height]
      align 256
   endif


bmp_48x2_2_00
	BYTE %00111111
	BYTE %01100000
	BYTE %11000000
	BYTE %11000001
	BYTE %11000001
	BYTE %11000001
	BYTE %11000000
	BYTE %11000000
	BYTE %01100000
	BYTE %00111111
	BYTE %11000000

   if >. != >[.+(bmp_48x2_2_height)]
      align 256
   endif

bmp_48x2_2_01
	BYTE %11111111
	BYTE %00000000
	BYTE %10001000
	BYTE %00100100
	BYTE %01110100
	BYTE %00100100
	BYTE %10001000
	BYTE %00000000
	BYTE %00000000
	BYTE %11111111
	BYTE %10001000

   if >. != >[.+(bmp_48x2_2_height)]
      align 256
   endif

bmp_48x2_2_02
	BYTE %11111111
	BYTE %00000000
	BYTE %00110010
	BYTE %01001010
	BYTE %01001010
	BYTE %01001010
	BYTE %00110011
	BYTE %00000000
	BYTE %00000000
	BYTE %11111111
	BYTE %00110011

   if >. != >[.+(bmp_48x2_2_height)]
      align 256
   endif

bmp_48x2_2_03
	BYTE %11111111
	BYTE %00000000
	BYTE %01011010
	BYTE %01010010
	BYTE %01010010
	BYTE %01010010
	BYTE %10010000
	BYTE %00010010
	BYTE %00000000
	BYTE %11111111
	BYTE %10010000

   if >. != >[.+(bmp_48x2_2_height)]
      align 256
   endif

bmp_48x2_2_04
	BYTE %11111111
	BYTE %00000000
	BYTE %10010011
	BYTE %10010100
	BYTE %10010111
	BYTE %10010100
	BYTE %11100011
	BYTE %00000000
	BYTE %00000000
	BYTE %11111111
	BYTE %11100011

   if >. != >[.+(bmp_48x2_2_height)]
      align 256
   endif

bmp_48x2_2_05
	BYTE %11111100
	BYTE %00000110
	BYTE %00000011
	BYTE %00000011
	BYTE %10000011
	BYTE %10000011
	BYTE %00000011
	BYTE %00000011
	BYTE %00000110
	BYTE %11111100
	BYTE %00000011

