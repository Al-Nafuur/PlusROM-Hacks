
 rem *** any romsize should work
 set romsize 16k

 rem *** The selected game number. The game selection minikernel displays 
 rem *** this variable
 dim gamenumber=a

 rem *** this debounce variable is used to slow down the game number selection
 dim swdebounce=b

 rem *** this turns on the score fading effect. it looks especially pretty
 rem *** if you do a "scorecolor=scorecolor+1" every 2nd or 4th frame.
 const scorefade=1

 scorecolor=$1a

 swdebounce=0
 gamenumber=1

titlepage
 gosub titledrawscreen bank2
 if joy0fire || switchreset then goto gamestart
 if !switchselect then swdebounce=0
 if swdebounce>0  then swdebounce=swdebounce-1: goto titlepage
 if switchselect then swdebounce=30: gamenumber=gamenumber+1:if gamenumber=21 then gamenumber=1
 goto titlepage

 rem *** Our fake game start. If you move the joystick it goes back to the
 rem *** title screen.
gamestart
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
 
 bank 3
