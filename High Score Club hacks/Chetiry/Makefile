###############################################################################
# CHETIRY - Atari 2600 - Copyright (C) 2011 Chris Walton
###############################################################################

compile:
	dasm chetiry.asm -f3 -DMELODY=0 -ochetiry.bin

list:
	dasm chetiry.asm -lchetiry.lst -f3 -DMELODY=0 -ochetiry.bin

img:
	dasm chetiry.asm -f3 -DMELODY=1 -ochetiry.bin
	./makeimg/makeimg -b bankswitch2/bootloader.bin -g chetiry.bin -o chetiry.img

flash: img
	lpc21isp -bin -wipe -verify chetiry.img /dev/ttyUSB0 38400 10000

flashnew: img
	lpc21isp -bin -wipe -verify -control -controlswap chetiry.img /dev/ttyUSB0 38400 10000

clean:
	rm chetiry.bin chetiry.img

