

 ;*** the height of this mini-kernel.
bmp_player_window = 10

 ;*** how many scanlines per pixel. 
bmp_player_kernellines = 1

 ;*** the height of each player.
bmp_player0_height = 10
bmp_player1_height = 10

 ;*** NUSIZ0 value. If you want to change it in a variable
 ;*** instead, dim on in bB called "bmp_player0_nusiz"
 ifnconst bmp_player0_nusiz
bmp_player0_nusiz
 endif
 BYTE 0

 ;*** NUSIZ1 value. If you want to change it in a variable
 ;*** instead, dim on in bB called "bmp_player1_nusiz"
 ifnconst bmp_player1_nusiz
bmp_player1_nusiz
 endif
 BYTE 0

 ;*** REFP0 value. If you want to change it in a variable
 ;*** instead, dim on in bB called "bmp_player0_refp"
 ifnconst bmp_player0_refp
bmp_player0_refp
 endif
 BYTE 0

 ;*** REFP1 value. If you want to change it in a variable
 ;*** instead, dim on in bB called "bmp_player1_refp"
 ifnconst bmp_player1_refp
bmp_player1_refp
 endif
 BYTE 0

 ;*** the bitmap data for player0
bmp_player0
	BYTE %01000010
	BYTE %01100100
	BYTE %00011100
	BYTE %00111100
	BYTE %00011100
	BYTE %00011000
	BYTE %00001100
	BYTE %00001100
	BYTE %00001100
	BYTE %00000000

	BYTE %00000110
	BYTE %01100100
	BYTE %00011100
	BYTE %01011010
	BYTE %00111100
	BYTE %00011000
	BYTE %00001100
	BYTE %00001100
	BYTE %00001100
	BYTE %00000000

	BYTE %00011000
	BYTE %00111000
	BYTE %00011000
	BYTE %00011000
	BYTE %00011000
	BYTE %00011000
	BYTE %00001100
	BYTE %00001100
	BYTE %00001100
	BYTE %00000000

	BYTE %01000000
	BYTE %01010000
	BYTE %01001000
	BYTE %00111000
	BYTE %01111000
	BYTE %00111000
	BYTE %00110000
	BYTE %00011000
	BYTE %00011000
	BYTE %00011000

 ;*** the color data for player0
bmp_color_player0
	BYTE $86 
	BYTE $86 
	BYTE $86 
	BYTE $a6 
	BYTE $a6 
	BYTE $a6 
	BYTE $4a 
	BYTE $4a
	BYTE $f4
	BYTE $00

	BYTE $86
	BYTE $86
	BYTE $86
	BYTE $a6
	BYTE $a6
	BYTE $a6
	BYTE $4a
	BYTE $4a
	BYTE $f4
	BYTE $00

	BYTE $86
	BYTE $86
	BYTE $86
	BYTE $a6
	BYTE $a6
	BYTE $a6
	BYTE $4a
	BYTE $4a
	BYTE $f4
	BYTE $00

	BYTE $86
	BYTE $86
	BYTE $86
	BYTE $86
	BYTE $a6
	BYTE $a6
	BYTE $a6
	BYTE $4a
	BYTE $4a
	BYTE $f4

 ;*** the bitmap data for player1
bmp_player1
	BYTE %00111100
	BYTE %01100110
	BYTE %11000011
	BYTE %11011011
	BYTE %11111111
	BYTE %11111111
	BYTE %11011011
	BYTE %11011011
	BYTE %01111110
	BYTE %00111100

 ;*** the color data for player1
bmp_color_player1
	BYTE $FA
	BYTE $FA
	BYTE $FA
	BYTE $FA
	BYTE $FA
	BYTE $FA
	BYTE $FA
	BYTE $FA
	BYTE $FA
	BYTE $FA
