
 ; To use a minikernel, just list it below. They'll be drawn on the screen in
 ; in the order they were listed.
 ; If a minikernel isn't listed, it won't be compiled into your program, and
 ; it won't use any rom space.

 MAC titlescreenlayout
	draw_48x2_1
	draw_48x1_1
	draw_space 6
	draw_48x1_2
	draw_score
 ENDM
