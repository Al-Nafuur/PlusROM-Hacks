
 ;*** The height of the displayed data...
bmp_48x1_2_window = 11

 ;*** The height of the bitmap data. This can be larger than 
 ;*** the displayed data height, if you're scrolling or animating 
 ;*** the data...
bmp_48x1_2_height = 11

 ifnconst bmp_48x1_2_PF1
bmp_48x1_2_PF1
 endif
	BYTE %00000000
 ifnconst bmp_48x1_2_PF2
bmp_48x1_2_PF2
 endif
	BYTE %00000000
 ifnconst bmp_48x1_2_background
bmp_48x1_2_background
 endif
	BYTE $00
 
 ifnconst bmp_48x1_2_color
bmp_48x1_2_color
 endif
 ; *** this is the bitmap color. If you want to change it in a 
 ; *** variable instead, dim one in bB called "bmp_48x1_2_color"
	BYTE $0f


   if >. != >[.+bmp_48x1_2_height]
	align 256
   endif

bmp_48x1_2_00
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000100
	BYTE %00000100
	BYTE %00000111
	BYTE %00000101
	BYTE %00000111
	BYTE %00000000

   if >. != >[.+(bmp_48x1_2_height)]
      align 256
   endif

bmp_48x1_2_01
	BYTE %01001110
	BYTE %01001010
	BYTE %01001010
	BYTE %01001010
	BYTE %11101110
	BYTE %00000000
	BYTE %01010111
	BYTE %01100100
	BYTE %01110110
	BYTE %01010100
	BYTE %01110111
	BYTE %01001010

   if >. != >[.+(bmp_48x1_2_height)]
      align 256
   endif

bmp_48x1_2_02
	BYTE %00001110
	BYTE %00000010
	BYTE %00001110
	BYTE %00001000
	BYTE %00001110
	BYTE %00000000
	BYTE %01110111
	BYTE %00010001
	BYTE %01110111
	BYTE %01000100
	BYTE %01110111
	BYTE %00001000

   if >. != >[.+(bmp_48x1_2_height)]
      align 256
   endif

bmp_48x1_2_03
	BYTE %01001010
	BYTE %01001010
	BYTE %01001110
	BYTE %01001010
	BYTE %11101110
	BYTE %00000000
	BYTE %00000100
	BYTE %00000100
	BYTE %00000110
	BYTE %00000100
	BYTE %00000111
	BYTE %01001010

   if >. != >[.+(bmp_48x1_2_height)]
      align 256
   endif

bmp_48x1_2_04
	BYTE %10100100
	BYTE %11000100
	BYTE %11100100
	BYTE %10100100
	BYTE %11101110
	BYTE %00000000
	BYTE %01010101
	BYTE %01011001
	BYTE %01011101
	BYTE %01010101
	BYTE %01011101
	BYTE %10100100

   if >. != >[.+(bmp_48x1_2_height)]
      align 256
   endif

bmp_48x1_2_05
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %11000000
	BYTE %00000000
	BYTE %10000000
	BYTE %00000000
	BYTE %11000000
	BYTE %00000000


