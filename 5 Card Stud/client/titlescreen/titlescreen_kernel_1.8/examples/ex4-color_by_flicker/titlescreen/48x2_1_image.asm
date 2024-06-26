
 ; *** if you want to modify the bitmap color on the fly, just dim a
 ; *** variable in bB called "bmp_48x2_1_color", and use it to set the
 ; *** color.


 ;*** this is the height of the displayed data
bmp_48x2_1_window = 22

 ;*** this is the height of the bitmap data
bmp_48x2_1_height = 22

   if >. != >[.+bmp_48x2_1_height+1]
      align 256
   endif
 BYTE 0

 ;*** this is the color of each line in the bitmap data
bmp_48x2_1_colors 
	BYTE $b6
	BYTE $a6
	BYTE $a8
	BYTE $a8
	BYTE $9a
	BYTE $9a
	BYTE $9a
	BYTE $8a
	BYTE $8c
	BYTE $8c
	BYTE $7c
	BYTE $7c
	BYTE $7c
	BYTE $6a
	BYTE $6a
	BYTE $6a
	BYTE $5a
	BYTE $5a
	BYTE $58
	BYTE $48
	BYTE $46
	BYTE $46

 ifnconst bmp_48x2_1_PF1
bmp_48x2_1_PF1
 endif
        BYTE %00000000
 ifnconst bmp_48x2_1_PF2
bmp_48x2_1_PF2
 endif
        BYTE %00000000
 ifnconst bmp_48x2_1_background
bmp_48x2_1_background
 endif
        BYTE $00

   if >. != >[.+bmp_48x2_1_height]
      align 256
   endif



   if >. != >[.+bmp_48x2_1_height]
      align 256
   endif

bmp_48x2_1_00

	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000001
	BYTE %00000001
	BYTE %00000001
	BYTE %00000001
	BYTE %00000001
	BYTE %00000001
	BYTE %00000011
	BYTE %00000011
	BYTE %00000111
	BYTE %00000111
	BYTE %00001111
	BYTE %00001111
	BYTE %00011111
	BYTE %00111111
	BYTE %00111111
	BYTE %01111110
	BYTE %01111110
	BYTE %00000000


   if >. != >[.+bmp_48x2_1_height]
      align 256
   endif

bmp_48x2_1_01

	BYTE %11101111
	BYTE %11011111
	BYTE %10111111
	BYTE %10111111
	BYTE %01001000
	BYTE %01011010
	BYTE %01001000
	BYTE %01101011
	BYTE %01001011
	BYTE %10111111
	BYTE %10111111
	BYTE %11011111
	BYTE %11011111
	BYTE %11101111
	BYTE %10110111
	BYTE %10111011
	BYTE %00011101
	BYTE %00011110
	BYTE %00011111
	BYTE %00011111
	BYTE %00011111
	BYTE %00011111

   if >. != >[.+bmp_48x2_1_height]
      align 256
   endif

bmp_48x2_1_02

	BYTE %11111100
	BYTE %11111110
	BYTE %11111111
	BYTE %11111111
	BYTE %10001000
	BYTE %10101010
	BYTE %10101010
	BYTE %10101010
	BYTE %10001000
	BYTE %11111111
	BYTE %11111111
	BYTE %11111110
	BYTE %11111110
	BYTE %11111101
	BYTE %11111101
	BYTE %11111011
	BYTE %11110011
	BYTE %00000011
	BYTE %00000001
	BYTE %00000000
	BYTE %10000000
	BYTE %10000000

   if >. != >[.+bmp_48x2_1_height]
      align 256
   endif

bmp_48x2_1_03

	BYTE %11110000
	BYTE %01111000
	BYTE %00111000
	BYTE %00011100
	BYTE %10011100
	BYTE %10011100
	BYTE %10111100
	BYTE %10111000
	BYTE %10111000
	BYTE %01110000
	BYTE %01100000
	BYTE %11100000
	BYTE %11000000
	BYTE %11000000
	BYTE %11000000
	BYTE %11000000
	BYTE %11110000
	BYTE %11111100
	BYTE %11111100
	BYTE %01111000
	BYTE %00000000
	BYTE %00000000


   if >. != >[.+bmp_48x2_1_height]
      align 256
   endif

bmp_48x2_1_04

	BYTE %00000010
	BYTE %10000110
	BYTE %10000110
	BYTE %11000110
	BYTE %11000111
	BYTE %11000111
	BYTE %11000011
	BYTE %11001111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11100001
	BYTE %11100001
	BYTE %11110001
	BYTE %11110001
	BYTE %11110000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000

   if >. != >[.+bmp_48x2_1_height]
      align 256
   endif

bmp_48x2_1_05

	BYTE %00001000
	BYTE %00011100
	BYTE %00011100
	BYTE %00011100
	BYTE %00011100
	BYTE %00001100
	BYTE %00001100
	BYTE %10001100
	BYTE %10001110
	BYTE %10001110
	BYTE %11001110
	BYTE %11001110
	BYTE %11001111
	BYTE %11101111
	BYTE %11101111
	BYTE %11001111
	BYTE %00000100
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000




