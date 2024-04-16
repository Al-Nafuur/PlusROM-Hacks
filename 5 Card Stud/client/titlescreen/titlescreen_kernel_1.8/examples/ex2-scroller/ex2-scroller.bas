
 rem *** any romsize should work
 set romsize 16k

 dim bmp_48x1_2_index=a
 dim frame=b

 scorecolor=$1a

resettitlepage
 frame=0
 bmp_48x1_2_index=0

titlepage
 gosub titledrawscreen bank2
 frame=frame+1
 temp1=frame&%00000111
 if temp1=0 && bmp_48x1_2_index<190 then bmp_48x1_2_index=bmp_48x1_2_index+1
 if joy0fire || switchreset then goto gamestart
 goto titlepage

 rem *** Our fake game start. If you move the joystick it goes back to the
 rem *** title screen.
gamestart
 drawscreen
 if joy0left || joy0right then goto resettitlepage
 if joy0up || joy0down then goto resettitlepage
 goto gamestart

 bank 2
 rem *** We're putting the title kernel here, but it can go in any bank you
 rem *** like. Just don't let your program flow accidentally fall into the
 rem *** line below.
 asm
 include "titlescreen/asm/titlescreen.asm"
end
