
 rem *** any romsize should work
 set romsize 16k

 dim frame=a
 dim addvalue=b

 rem ** we define this because player 0 has multiple frames...
 dim bmp_player0_index=c

 scorecolor=$8a

resettitlepage
 frame=0
 player0x=0
 player0y=11
 player1x=159
 player1y=25
 addvalue=1

titlepage
 gosub titledrawscreen bank2
 frame=frame+1
 temp1=frame&3
 if temp1>0 then skipplayeranim 
 bmp_player0_index=bmp_player0_index+10
 player0x=player0x+1
 player1x=player1x-1
 player0y=player0y+addvalue
 if player0x>159 then player0x=0
 if player1x>200 then player1x=159
 if player0y=40 || player0y=10 then addvalue=addvalue^254
 if bmp_player0_index>30 then bmp_player0_index=0
skipplayeranim

 if joy0fire || switchreset then player0y=200:player1y=200:goto gamestart
 goto titlepage

 rem *** Our fake game start. If you move the joystick it goes back to the
 rem *** title screen.
gamestart

 rem *** if you used the player kernel, you'll need to hide the players or 
 rem *** redefine them, or else you'll see garbage on the screen.
 player0:
 0
end
 player1:
 0
end
 missile0height=1
 missile1height=1

gameloop
 drawscreen
 if joy0left || joy0right then goto resettitlepage
 if joy0up || joy0down then goto resettitlepage
 goto gameloop

 bank 2
 rem *** We're putting the title kernel here, but it can go in any bank you
 rem *** like. Just don't let your program flow accidentally fall into the
 rem *** line below.
 asm
 include "titlescreen/asm/titlescreen.asm"
end
