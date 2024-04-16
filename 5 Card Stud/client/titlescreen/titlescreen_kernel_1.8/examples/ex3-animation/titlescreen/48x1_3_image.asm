

 ; *** if you want to modify the bitmap color on the fly, just dim a
 ; *** variable in bB called "bmp_48x1_3_color", and use it to set the
 ; *** color.


 ;*** this is the height of the displayed data
bmp_48x1_3_window = 11

 ;*** this is the height of the bitmap data
bmp_48x1_3_height = 11

 ifnconst bmp_48x1_3_color
bmp_48x1_3_color
 endif
	.byte $0f

 ifnconst bmp_48x1_3_PF1
bmp_48x1_3_PF1
 endif
        BYTE %00000000
 ifnconst bmp_48x1_3_PF2
bmp_48x1_3_PF2
 endif
        BYTE %00000000
 ifnconst bmp_48x1_3_background
bmp_48x1_3_background
 endif
        BYTE $00


   if >. != >[.+bmp_48x1_3_height]
      align 256
   endif

bmp_48x1_3_00

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

   if >. != >[.+bmp_48x1_3_height]
      align 256
   endif

bmp_48x1_3_01

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

   if >. != >[.+bmp_48x1_3_height]
      align 256
   endif

bmp_48x1_3_02

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



   if >. != >[.+bmp_48x1_3_height]
      align 256
   endif

bmp_48x1_3_03

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

   if >. != >[.+bmp_48x1_3_height]
      align 256
   endif

bmp_48x1_3_04

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


   if >. != >[.+bmp_48x1_3_height]
      align 256
   endif

bmp_48x1_3_05

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


