

 ; *** if you want to modify the bitmap color on the fly, just dim a
 ; *** variable in bB called "bmp_48x1_2_color", and use it to set the
 ; *** color.


 ;*** this is the height of the displayed data
bmp_48x1_2_window = 11

 ;*** this is the height of the bitmap data
bmp_48x1_2_height = 11

 ifnconst bmp_48x1_2_color
bmp_48x1_2_color
 endif
	.byte $4a

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

   if >. != >[.+bmp_48x1_2_height]
      align 256
   endif

bmp_48x1_2_00

	BYTE %00001110
	BYTE %00001000
	BYTE %00001000
	BYTE %00001000
	BYTE %00001110
	BYTE %00000000
	BYTE %11101110
	BYTE %10001010
	BYTE %10001010
	BYTE %10001010
	BYTE %11101110

   if >. != >[.+bmp_48x1_2_height]
      align 256
   endif

bmp_48x1_2_01

	BYTE %11101000
	BYTE %10101000
	BYTE %10101110
	BYTE %10101010
	BYTE %11101110
	BYTE %00000000
	BYTE %10000100
	BYTE %10000100
	BYTE %11101110
	BYTE %10101010
	BYTE %11101010

   if >. != >[.+bmp_48x1_2_height]
      align 256
   endif

bmp_48x1_2_02

	BYTE %10001110
	BYTE %10001000
	BYTE %11101100
	BYTE %10101000
	BYTE %11101110
	BYTE %00000000
	BYTE %10101011
	BYTE %11001010
	BYTE %11101011
	BYTE %10101010
	BYTE %11101011


   if >. != >[.+bmp_48x1_2_height]
      align 256
   endif

bmp_48x1_2_03

	BYTE %10100100
	BYTE %10100100
	BYTE %11000100
	BYTE %10100100
	BYTE %11101110
	BYTE %00000000
	BYTE %10101001
	BYTE %10101001
	BYTE %10111001
	BYTE %00101001
	BYTE %10101011

   if >. != >[.+bmp_48x1_2_height]
      align 256
   endif

bmp_48x1_2_04

	BYTE %11101001
	BYTE %10101011
	BYTE %10101111
	BYTE %10101101
	BYTE %11101001
	BYTE %00000000
	BYTE %00011101
	BYTE %00010001
	BYTE %00011101
	BYTE %00000101
	BYTE %10011101


   if >. != >[.+bmp_48x1_2_height]
      align 256
   endif

bmp_48x1_2_05

	BYTE %01110000
	BYTE %01000000
	BYTE %01100000
	BYTE %01000000
	BYTE %01110000
	BYTE %00000000
	BYTE %11010111
	BYTE %01010101
	BYTE %01010101
	BYTE %01010101
	BYTE %11010111



