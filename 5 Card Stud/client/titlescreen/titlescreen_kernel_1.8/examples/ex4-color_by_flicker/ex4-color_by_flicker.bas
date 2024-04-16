
 rem *** any romsize should work
 set romsize 16k

 rem *** a variable to switch frames of the yoshi picture
 dim bmp_48x1_1_index = a

 rem *** a variable to color each frame of the yoshi picture
 dim bmp_48x1_1_color = b

 rem *** a variable to color cycle the "press fire" text
 dim bmp_48x1_2_color = c

 rem *** a frame counter
 dim frame = d

 scorecolor=$0a

titlepage
 frame=frame+1
 bmp_48x1_1_color=$4b :bmp_48x1_1_index=0
 if frame{0} then bmp_48x1_1_color=$ac : bmp_48x1_1_index=87
 gosub titledrawscreen bank2

 rem *** make the "press fire to start" color-cycle
 bmp_48x1_2_color=bmp_48x1_2_color+1
 if joy0fire then goto gamestart

 goto titlepage

 rem *** Our fake game start. If you move the joystick it goes back to the
 rem *** title screen.
gamestart
 bmp_48x1_1_index=0
 drawscreen
 if joy0left || joy0right then goto titlepage
 if joy0up || joy0down then goto titlepage
 goto gamestart

 bank 2
 rem *** We're putting the title kernel here, but it can go in any bank you
 rem *** like. Just don't let your program flow accidentally fall into the
 rem *** line below.
 asm
 include "titlescreen/asm/titlescreen.asm"
end
 
