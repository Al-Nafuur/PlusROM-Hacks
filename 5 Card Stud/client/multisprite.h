; Provided under the CC0 license. See the included LICENSE.txt for details.


LEFT_PLAYERS_XPOS = 28
RIGHT_PLAYERS_XPOS = 92

ROUND_MASK         = %00000111
ACTIVE_PLAYER_MASK = %01111000
ACTIVE_P_COL_MASK  = %00011000
ACTIVE_P_ROW_MASK  = %00100000
NO_ACTIVE_P_MASK   = %01000000
VIEWING_MASK       = %10000000

EARLY_HMOVE_M00 = $80    ;  0
EARLY_HMOVE_M01 = $90    ; -1
EARLY_HMOVE_M02 = $A0    ; -2
EARLY_HMOVE_M03 = $B0    ; -3
EARLY_HMOVE_M04 = $C0    ; -4
EARLY_HMOVE_M05 = $D0    ; -5
EARLY_HMOVE_M06 = $E0    ; -6
EARLY_HMOVE_M07 = $F0    ; -7
EARLY_HMOVE_M08 = $00    ; -8
EARLY_HMOVE_M09 = $10    ; -9
EARLY_HMOVE_M10 = $20    ; -10
EARLY_HMOVE_M11 = $30    ; -11
EARLY_HMOVE_M12 = $40    ; -12
EARLY_HMOVE_M13 = $50    ; -13
EARLY_HMOVE_M14 = $60    ; -14
EARLY_HMOVE_M15 = $70    ; -15


; player_data:
; name: not used
; status: not used
; bet: (not used?)
; move: (not used)
; hand: 5 bytes
; purse: 2 bytes
player_data             equ $80 ; 8x7 = 56 bytes !

player_purse            equ $A8
player1_purse_lo        equ $A8
player1_purse_hi        equ $A9
player2_purse_lo        equ $AA
player2_purse_hi        equ $AB
player3_purse_lo        equ $AC
player3_purse_hi        equ $AD
player4_purse_lo        equ $AE
player4_purse_hi        equ $AF

player5_purse_lo        equ $B0
player5_purse_hi        equ $B1
player6_purse_lo        equ $B2
player6_purse_hi        equ $B3
player7_purse_lo        equ $B4
player7_purse_hi        equ $B5
player8_purse_lo        equ $B6
player8_purse_hi        equ $B7

; game_data:
; lastResult: not used e.g. "Thom won with Full House, Eights full of Sixes",
; pot: 2 bytes
; round: 3 bits (0-5)
; activePlayer: 4 bits (0-7) -1 for none!
; moveTime: not used (0-25)
; viewing: 1 bit (0-1)
game_data               equ $B8 ; 3 bytes
pot_lo_id               equ $B8 ; id of 0-99 (with 00-09) sign 
pot_hi_id               equ $B9 ; id of 0-99 (with 00-09) sign
game_state              equ $BA ; round 

; valid_moves
valid_moves             equ $BB ; if active player == 0 then 1 byte ()
last_move               equ $BB ; 1 byte ()


; End of PlusROM delivered data

row_counter             equ $BC ; 1 byte
current_row             equ $BD ; 8 bytes
current_color           equ $C5 ; 8 bytes, only every second used (0,2,4,6)
current_color_1         equ $C5 ; 1 byte
temp1                   equ $C6 ; 1 byte
current_color_2         equ $C7 ; 1 byte
temp2                   equ $C8 ; 1 byte
current_color_3         equ $C9 ; 1 byte
temp3                   equ $CA ; 1 byte
pf_offset               equ $CA ; 1 byte
current_color_4         equ $CB ; 1 byte
current_move            equ $CC ; 1 byte

; end of kernel 

scorepointers = $80 ; 11 bytes used in title screen
missile0x = $d4     ;  1 byte used in title screen as framecounter
framecounter = $d4
missile0y = $d6     ;  1 byte used in title screen as temp var


A = $d7
a = $d7
B = $d8
b = $d8
C = $d9
c = $d9
D = $da
d = $da
E = $db
e = $db
F = $dc
f = $dc
G = $dd
g = $dd
H = $de
h = $de
I = $df
i = $df
J = $e0
j = $e0
K = $e1
k = $e1
L = $e2
l = $e2
M = $e3
m = $e3
N = $e4
n = $e4
O = $e5
o = $e5
P = $e6
p = $e6
Q = $e7
q = $e7
R = $e8
r = $e8
S = $e9
s = $e9
T = $ea
t = $ea
U = $eb
u = $eb
V = $ec
v = $ec
W = $ed
w = $ed
X = $ee
x = $ee
Y = $ef
y = $ef
Z = $f0
z = $f0


temp7 = $F1 ; This is used to aid in bankswitching
aux2 = $F2
aux5 = $F3
aux6 = $F4


stack1 = $f6
stack2 = $f7
stack3 = $f8
stack4 = $f9
; the stack bytes above may be used in the kernel
; stack = F6-F7, F8-F9, FA-FB, FC-FD, FE-FF

 MAC RETURN	; auto-return from either a regular or bankswitched module
   ifnconst bankswitch
     rts
   else
     jmp BS_return
   endif
 ENDM
