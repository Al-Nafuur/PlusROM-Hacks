

 ; *** if you want to modify the bitmap color on the fly, just dim a
 ; *** variable in bB called "bmp_48x1_1_color", and use it to set the
 ; *** color.


 ;*** this is the height of the displayed data
bmp_48x1_1_window = 27

 ;*** this is the height of the bitmap data
bmp_48x1_1_height = 27

 ifnconst bmp_48x1_1_color
bmp_48x1_1_color
 endif
	.byte $1a

   if >. != >[.+bmp_48x1_1_height]
      align 256
   endif

 ifnconst bmp_48x1_1_PF1
bmp_48x1_1_PF1
 endif
        BYTE %00000000
 ifnconst bmp_48x1_1_PF2
bmp_48x1_1_PF2
 endif
        BYTE %00000000
 ifnconst bmp_48x1_1_background
bmp_48x1_1_background
 endif
        BYTE $00

bmp_48x1_1_00
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %11111111
	BYTE %11111111
	BYTE %01111111
	BYTE %00111100
	BYTE %00011110
	BYTE %00001111
	BYTE %00000111
	BYTE %00000011
	BYTE %00000001
	BYTE %00000000
	BYTE %00000000
	BYTE %00001100
	BYTE %00000111
	BYTE %00000111
	BYTE %00000011
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000


   if >. != >[.+bmp_48x1_1_height]
      align 256
   endif

bmp_48x1_1_01
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %00011111
	BYTE %00000000
	BYTE %00000000
	BYTE %11111100
	BYTE %11111100
	BYTE %11111100
	BYTE %00011100
	BYTE %00011100
	BYTE %00011100
	BYTE %11111100
	BYTE %11111100
	BYTE %11111100
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000




   if >. != >[.+bmp_48x1_1_height]
      align 256
   endif

bmp_48x1_1_02
	BYTE %00000010
	BYTE %00000110
	BYTE %00001110
	BYTE %00011110
	BYTE %00111110
	BYTE %01111110
	BYTE %01110110
	BYTE %01100110
	BYTE %01100110
	BYTE %01100110
	BYTE %01100110
	BYTE %01100110
	BYTE %01100110
	BYTE %01100110
	BYTE %01100110
	BYTE %01100110
	BYTE %01100110
	BYTE %01100110
	BYTE %01100110
	BYTE %01100110
	BYTE %01100110
	BYTE %01110110
	BYTE %00111110
	BYTE %00011110
	BYTE %00001110
	BYTE %00000110
	BYTE %00000010




   if >. != >[.+bmp_48x1_1_height]
      align 256
   endif

bmp_48x1_1_03
	BYTE %10000000
	BYTE %11000000
	BYTE %11100000
	BYTE %11110000
	BYTE %11111000
	BYTE %11111100
	BYTE %11011110
	BYTE %11001110
	BYTE %11000110
	BYTE %11000110
	BYTE %11000110
	BYTE %11000110
	BYTE %01001100
	BYTE %01101100
	BYTE %01101100
	BYTE %11001100
	BYTE %11000110
	BYTE %11000110
	BYTE %11000110
	BYTE %11000110
	BYTE %11001110
	BYTE %11011100
	BYTE %11111000
	BYTE %11110000
	BYTE %11100000
	BYTE %11000000
	BYTE %10000000


   if >. != >[.+bmp_48x1_1_height]
      align 256
   endif

bmp_48x1_1_04
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000111
	BYTE %00000111
	BYTE %00000111
	BYTE %00000111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11000111
	BYTE %11000111
	BYTE %11100111
	BYTE %01110111
	BYTE %00111111
	BYTE %00011111
	BYTE %00001111
	BYTE %00000111
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000


   if >. != >[.+bmp_48x1_1_height]
      align 256
   endif

bmp_48x1_1_05
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %11111110
	BYTE %11111100
	BYTE %11111000
	BYTE %00110000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000



