   inline PlusROM_functions.asm

   includesfile multisprite_superchip.inc
   set smartbranching on
   set optimization inlinerand
   set optimization noinlinedata
   set kernel multisprite
   set romsize 32kSC

   ; In game score font
   const font                      = custom
   const noscore = 1

   const IS_NTSC = 1                ; 1 for NTSC colors or 1 for PAL colors
   inline NTSC_PAL_colors.asm       ; Color constants are defined in external ASM file

   const LOAD_LOBBY  = 0
   const JOIN_TABLE  = 1
   const LEAVE_TABLE = 2
   const LOAD_STATE  = 3
   const SEND_MOVE   = 4

   ; To use the ASM defined colors in bB assignments they have to be redefined,
   ; otherwise bB is using the ZP memory instead!
   const _Color_Background          = _C2
   const _Color_Titlescreen_BG      = _00
   const _Color_Titlescreen_PF      = _C2
   const _Color_Lobby_BG            = _C2
   const _Color_Lobby_Row           = _0E
   const _Color_Lobby_Active_Row    = _EA
   const _Color_Card                = _0E
   const _Color_Purse               = _00
   const _Color_Purse_BG            = _0E


   dim selected_valid_move_id = x
   dim activePlayer           = y
   dim game_flags             = z
   dim _Bit0_Request_pending  = z
   dim _Bit1_reset_restrainer = z
   dim _Bit2_joy_restrainer   = z
   dim _Bit3_mute_bg_music    = z
   dim _Bit4_genesispad       = z
   dim _Bit5_PlusROM          = z
   dim _Bit6_p0_explosion     = z
   dim _Bit7_hide_sidefighter = z  ; hide sidefighter and Playfield mirrored

   bank 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;#region "Bank 1 Game Logic"

   game_flags = 0
   if INPT1{7} then _Bit4_genesispad{4} = 1
   if ReceiveBufferSize = 0 then _Bit5_PlusROM{5} = 1

   goto titlescreen_start bank7

start
  WriteToBuffer = ActiveRow
  WriteSendBuffer = JOIN_TABLE

   asm
; Load CARD_FACE_DOWN into player cards
        ldx #55
        lda #CARD_FACE_DOWN_ID
fill_player_data
        sta player_data,x
        dex
        bpl fill_player_data
end
  goto set_request_pending

refresh
  WriteSendBuffer = LOAD_STATE

set_request_pending
  _Bit0_Request_pending{0} = 1

  asm
   sei
   cld
end

   NUSIZ0 = 2 : NUSIZ1 = 2
   CTRLPF = 49
   VDELP0 = 0 : VDELP1 = 0 : VDELBL = 0

   COLUBK = _Color_Background
   COLUPF = _Color_Card

main
  drawscreen

  if !joy0fire && SWCHA > 239 then _Bit1_reset_restrainer{1} = 0

  if _Bit1_reset_restrainer{1} then skip_player_move
  if (game_state & #ACTIVE_PLAYER_MASK) then skip_player_move

  if joy0up && selected_valid_move_id < 6 then _Bit1_reset_restrainer{1} = 1 : goto select_next_move
  if joy0down && selected_valid_move_id > 0 then _Bit1_reset_restrainer{1} = 1 : goto select_prev_move
  if joy0fire then _Bit1_reset_restrainer{1} = 1 : goto send_move

skip_player_move
  if joy0fire && joy0right then leave_table_to_lobby
  if switchselect then leave_table_to_lobby
  if switchreset then leave_table_to_titlescreen
  if !_Bit0_Request_pending{0} then check_for_reload

  if ReceiveBufferSize < 60 then main

  _Bit0_Request_pending{0} = 0
  framecounter = 1

  asm
   LDX #0
.copy_player_data_loop
   LDA	ReceiveBuffer   		    ; 4
   STA	player_data,x		        ; 5   
   INX					                ; 2   
   LDA	ReceiveBufferSize       ; 4
   BNE	.copy_player_data_loop  ; 2/3 
end

  if (game_state & #ACTIVE_PLAYER_MASK) then current_move = last_move : goto main

  asm
   LDA valid_moves              ; is also last_move
   ldx #$ff
.find_first_valid_move
   inx
   lsr
   bcc .find_first_valid_move
.store_move_pointer
   stx selected_valid_move_id
   lda move_pointer_list,x
   sta current_move
end

check_for_reload
  if !framecounter then goto refresh
  goto main
; end of main loop

select_next_move
  asm
  lda valid_moves
  ldx selected_valid_move_id
.find_cur_move_loop
  lsr
  dex
  bpl .find_cur_move_loop
  ; now cur move is in C
  ; find next valid up.
  ldx selected_valid_move_id
.find_next_valid_move
  inx
  cpx #7
  beq .no_next_or_prev_valid_move   ; branch to "skip_player_move" if possible
  lsr
  bcc .find_next_valid_move
  jmp .store_move_pointer
end

select_prev_move
  asm
  lda valid_moves
  ldx selected_valid_move_id
.find_cur_move_loop2
  clc
  rol
  inx
  cpx #8
  bne .find_cur_move_loop2
  ; now cur move is in C
  ; find next valid up.
  ldx selected_valid_move_id
.find_prev_valid_move
  dex
  bmi .no_next_or_prev_valid_move   ; branch to "skip_player_move" if possible
  clc
  rol
  bcc .find_prev_valid_move
  jmp .store_move_pointer

.no_next_or_prev_valid_move
end
  goto skip_player_move

send_move
  ; todo: Check for "_Bit0_Request_pending{0} == 1"
  ; and delay our request until previous request has been received
  WriteToBuffer = selected_valid_move_id
  WriteSendBuffer = SEND_MOVE
  goto set_request_pending


leave_table_to_titlescreen
  WriteSendBuffer = LEAVE_TABLE
  _Bit0_Request_pending{0} = 1 ; todo read the response before next request

  goto titlescreen_start bank7

leave_table_to_lobby
  WriteSendBuffer = LEAVE_TABLE
  _Bit0_Request_pending{0} = 1 ; todo read the response before next request

  goto _prepare_Lobby_bank5 bank5

; data table bank 1
  asm
move_pointer_list
  .byte MOVE_FO_ID, MOVE_CH_ID, MOVE_BB_ID, MOVE_BL_ID, MOVE_BH_ID, MOVE_CA_ID, MOVE_RA_ID
end

;#endregion

   bank 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;#region "Bank 2 Title Screen Music"

  asm
Channel_1Duration = $90
Channel_1Counter  = $91 ; This is a pointer, needs 16 bits.
Channel_2Duration = $93
Channel_2Counter  = $94 ; This is a pointer, needs 16 bits.
end

RestartMusic
  asm
	LDA	#1
	STA	Channel_1Duration
	STA	Channel_2Duration
	LDA	#<.MusicChannel_1
	STA	Channel_1Counter
	LDA	#>.MusicChannel_1
	STA	Channel_1Counter+1
	LDA	#<.MusicChannel_2
	STA	Channel_2Counter
	LDA	#>.MusicChannel_2
	STA	Channel_2Counter+1
end
Music_Player
  asm
	DEC	Channel_1Duration
	LDA	Channel_1Duration
	BNE	.LoadNext2
	LDX	#Channel_1Counter
	LDA	(0,x)
	INC	0,x
	BNE	*+4
	INC	1,x
	CMP	#255
	BNE	.Not255_1
	LDA	#<.MusicChannel_1
	STA	Channel_1Counter
	LDA	#>.MusicChannel_1
	STA	Channel_1Counter+1
	LDX	#Channel_1Counter
	LDA	(0,x)
	INC	0,x
	BNE	*+4
	INC	1,x
.Not255_1
	STA	AUDV0
	CMP	#0
	BEQ	.GotoDuration1
	lsr
	lsr
	lsr
	lsr
	STA	AUDC0
	LDX	#Channel_1Counter
	LDA	(0,x)
	INC	0,x
	BNE	*+4
	INC	1,x
	STA	AUDF0
.GotoDuration1
	LDX	#Channel_1Counter
	LDA	(0,x)
	INC	0,x
	BNE	*+4
	INC	1,x
	STA	Channel_1Duration
.LoadNext2
	DEC	Channel_2Duration
	LDA	Channel_2Duration
	BNE	.LoadEnd
	LDX	#Channel_2Counter
	LDA	(0,x)
	INC	0,x
	BNE	*+4
	INC	1,x
	CMP	#255
	BNE	.Not255_2
	LDA	#<.MusicChannel_2
	STA	Channel_2Counter
	LDA	#>.MusicChannel_2
	STA	Channel_2Counter+1
	LDX	#Channel_2Counter
	LDA	(0,x)
	INC	0,x
	BNE	*+4
	INC	1,x
.Not255_2
	STA	AUDV1
	CMP	#0
	BEQ	.GotoDuration2
	lsr
	lsr
	lsr
	lsr
	STA	AUDC1
	LDX	#Channel_2Counter
	LDA	(0,x)
	INC	0,x
	BNE	*+4
	INC	1,x
	STA	AUDF1
.GotoDuration2
	LDX	#Channel_2Counter
	LDA	(0,x)
	INC	0,x
	BNE	*+4
	INC	1,x
	STA	Channel_2Duration
.LoadEnd
end
  goto return_from_music bank7

  asm
.MusicChannel_1
	.BYTE	#%01001000
	.BYTE	#6
	.BYTE	#1
	.BYTE	#%01001000
	.BYTE	#5
	.BYTE	#7
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%01001000
	.BYTE	#5
	.BYTE	#8
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%01001000
	.BYTE	#6
	.BYTE	#8
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%01001000
	.BYTE	#8
	.BYTE	#17
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%01001000
	.BYTE	#7
	.BYTE	#8
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%01001000
	.BYTE	#9
	.BYTE	#17
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%01001000
	.BYTE	#27
	.BYTE	#1
	.BYTE	#%01001000
	.BYTE	#26
	.BYTE	#7
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%01001000
	.BYTE	#23
	.BYTE	#7
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%01001000
	.BYTE	#29
	.BYTE	#8
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%11001000
	.BYTE	#11
	.BYTE	#17
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%01001000
	.BYTE	#31
	.BYTE	#8
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%11001000
	.BYTE	#12
	.BYTE	#17
	.BYTE	#0
	.BYTE	#2
	.BYTE	#0
	.BYTE	#7
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%11001000
	.BYTE	#31
	.BYTE	#8
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%00010010
	.BYTE	#15
	.BYTE	#8
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%00010010
	.BYTE	#18
	.BYTE	#17
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%00010010
	.BYTE	#16
	.BYTE	#8
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%00010010
	.BYTE	#18
	.BYTE	#8
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%00010010
	.BYTE	#18
	.BYTE	#7
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%00010010
	.BYTE	#19
	.BYTE	#3
	.BYTE	#0
	.BYTE	#33
	.BYTE	#%01001010
	.BYTE	#19
	.BYTE	#17
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%11000110
	.BYTE	#17
	.BYTE	#8
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%11000110
	.BYTE	#16
	.BYTE	#8
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%11000110
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%11000110
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%11000110
	.BYTE	#15
	.BYTE	#4
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#51
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01000110
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#21
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#20
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#11
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#11
	.BYTE	#13
	.BYTE	#0
	.BYTE	#2
	.BYTE	#0
	.BYTE	#2
	.BYTE	#%01000110
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%01000110
	.BYTE	#14
	.BYTE	#14
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#23
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#19
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%11000110
	.BYTE	#17
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%11000110
	.BYTE	#16
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%11000110
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%11000110
	.BYTE	#15
	.BYTE	#4
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%11000110
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#58
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#17
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#19
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#17
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#23
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#20
	.BYTE	#14
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#20
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#20
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#20
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#12
	.BYTE	#52
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%11000110
	.BYTE	#17
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%11000110
	.BYTE	#16
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%11000110
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%11000110
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%11000110
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#52
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%01000110
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#11
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000111
	.BYTE	#11
	.BYTE	#14
	.BYTE	#0
	.BYTE	#2
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%01000111
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000111
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%01000111
	.BYTE	#14
	.BYTE	#14
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000111
	.BYTE	#23
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000111
	.BYTE	#19
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%01001000
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#19
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#23
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#21
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#19
	.BYTE	#14
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#19
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#19
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#19
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#11
	.BYTE	#4
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#11
	.BYTE	#14
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#11
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#11
	.BYTE	#14
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#12
	.BYTE	#4
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%01001000
	.BYTE	#14
	.BYTE	#52
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%11000110
	.BYTE	#17
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%11000110
	.BYTE	#16
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%11000110
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%11000110
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%11000110
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#51
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01000110
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#21
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#20
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#11
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#12
	.BYTE	#4
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#11
	.BYTE	#14
	.BYTE	#0
	.BYTE	#2
	.BYTE	#0
	.BYTE	#2
	.BYTE	#%01000110
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%01000110
	.BYTE	#14
	.BYTE	#14
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#23
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#19
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%11000110
	.BYTE	#17
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%11000110
	.BYTE	#16
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%11000110
	.BYTE	#15
	.BYTE	#4
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%11000110
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%11000110
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#58
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#17
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#19
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#17
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#23
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#20
	.BYTE	#14
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#20
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#20
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#20
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#12
	.BYTE	#52
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%11000110
	.BYTE	#17
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%11000110
	.BYTE	#16
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%11000110
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%11000110
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%11000110
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#52
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%01000110
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#11
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#11
	.BYTE	#14
	.BYTE	#0
	.BYTE	#2
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%01000110
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%01000110
	.BYTE	#14
	.BYTE	#14
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000111
	.BYTE	#23
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000111
	.BYTE	#19
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000111
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%01001000
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#19
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#23
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#21
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#19
	.BYTE	#14
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#19
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#19
	.BYTE	#4
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#19
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#11
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#11
	.BYTE	#14
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#11
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#11
	.BYTE	#13
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%01001000
	.BYTE	#14
	.BYTE	#40
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#29
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#21
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#24
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%11001000
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%01001000
	.BYTE	#17
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#23
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01001000
	.BYTE	#29
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#26
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#24
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%11001000
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%01001000
	.BYTE	#23
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01000111
	.BYTE	#23
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000111
	.BYTE	#29
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%11000111
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%11000110
	.BYTE	#11
	.BYTE	#4
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#31
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#26
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%11000110
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#26
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#26
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%11000110
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#23
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000111
	.BYTE	#21
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000111
	.BYTE	#19
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000111
	.BYTE	#17
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000111
	.BYTE	#19
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#29
	.BYTE	#4
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#26
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%11001000
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%01001000
	.BYTE	#17
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#23
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01001000
	.BYTE	#29
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#26
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#24
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#23
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%01001000
	.BYTE	#17
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#23
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01001000
	.BYTE	#19
	.BYTE	#4
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#24
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#16
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#15
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01001000
	.BYTE	#15
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01001000
	.BYTE	#17
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#20
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#26
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#19
	.BYTE	#42
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11001000
	.BYTE	#15
	.BYTE	#4
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%11001000
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%11001000
	.BYTE	#13
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#23
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%01001000
	.BYTE	#17
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#23
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01001000
	.BYTE	#29
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#21
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#20
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%11001000
	.BYTE	#12
	.BYTE	#4
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%01001000
	.BYTE	#17
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#23
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01000111
	.BYTE	#23
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000111
	.BYTE	#29
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%11000111
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%11000110
	.BYTE	#11
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#31
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#26
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%11000110
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#26
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#4
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#26
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%01000110
	.BYTE	#23
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#19
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%11000110
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%11000110
	.BYTE	#13
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%11000110
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%11000110
	.BYTE	#11
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#14
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#11
	.BYTE	#4
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%11000110
	.BYTE	#11
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#19
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#11
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01000110
	.BYTE	#23
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%11000110
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#17
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#4
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%01000110
	.BYTE	#11
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#26
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01000110
	.BYTE	#23
	.BYTE	#51
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01000110
	.BYTE	#11
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#10
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#10
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#9
	.BYTE	#4
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%01000110
	.BYTE	#8
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#9
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01000110
	.BYTE	#11
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#10
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#10
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#9
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01000110
	.BYTE	#8
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#9
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01000110
	.BYTE	#10
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#14
	.BYTE	#4
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#19
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#17
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#11
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#19
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#11
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#10
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#9
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#8
	.BYTE	#4
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#9
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#11
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#10
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#9
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%01000110
	.BYTE	#8
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#9
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01000110
	.BYTE	#11
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#10
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#10
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#9
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01000110
	.BYTE	#8
	.BYTE	#4
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#9
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01000110
	.BYTE	#9
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#8
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#7
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#7
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#7
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01000110
	.BYTE	#7
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01000110
	.BYTE	#8
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#10
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#0
	.BYTE	#1
	.BYTE	#0
	.BYTE	#1
	.BYTE	#0
	.BYTE	#1
	.BYTE	#0
	.BYTE	#2
	.BYTE	#0
	.BYTE	#1
	.BYTE	#0
	.BYTE	#2
	.BYTE	#0
	.BYTE	#1
	.BYTE	#0
	.BYTE	#2
	.BYTE	#0
	.BYTE	#1
	.BYTE	#0
	.BYTE	#2
	.BYTE	#0
	.BYTE	#1
	.BYTE	#0
	.BYTE	#2
	.BYTE	#0
	.BYTE	#1
	.BYTE	#0
	.BYTE	#2
	.BYTE	#0
	.BYTE	#1
	.BYTE	#0
	.BYTE	#2
	.BYTE	#0
	.BYTE	#1
	.BYTE	#0
	.BYTE	#2
	.BYTE	#0
	.BYTE	#1
	.BYTE	#0
	.BYTE	#2
	.BYTE	#0
	.BYTE	#1
	.BYTE	#0
	.BYTE	#2
	.BYTE	#0
	.BYTE	#1
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%01000110
	.BYTE	#9
	.BYTE	#1
	.BYTE	#0
	.BYTE	#1
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%01000110
	.BYTE	#9
	.BYTE	#1
	.BYTE	#0
	.BYTE	#1
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%01000110
	.BYTE	#9
	.BYTE	#1
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01000110
	.BYTE	#11
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#10
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#10
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#9
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%01000110
	.BYTE	#8
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#9
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01000110
	.BYTE	#11
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#10
	.BYTE	#4
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#10
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#9
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01000110
	.BYTE	#8
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#9
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01000110
	.BYTE	#11
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#19
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#17
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#11
	.BYTE	#4
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%01000110
	.BYTE	#11
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#9
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#6
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#9
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#10
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#9
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#6
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%01000110
	.BYTE	#8
	.BYTE	#4
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#6
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01000110
	.BYTE	#8
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#6
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#8
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#9
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#6
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#5
	.BYTE	#5
	.BYTE	#0
	.BYTE	#2
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%01000110
	.BYTE	#4
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01000110
	.BYTE	#5
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#6
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#9
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#8
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%01000110
	.BYTE	#6
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%01000110
	.BYTE	#5
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#5
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01000110
	.BYTE	#6
	.BYTE	#52
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%11000110
	.BYTE	#17
	.BYTE	#6
	.BYTE	#0
	.BYTE	#5
	.BYTE	#%11000110
	.BYTE	#16
	.BYTE	#6
	.BYTE	#0
	.BYTE	#5
	.BYTE	#%11000110
	.BYTE	#15
	.BYTE	#6
	.BYTE	#0
	.BYTE	#5
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%11000110
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%11000110
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#51
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01000110
	.BYTE	#14
	.BYTE	#4
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#21
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#20
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#11
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#11
	.BYTE	#14
	.BYTE	#0
	.BYTE	#2
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%01000110
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%01000110
	.BYTE	#14
	.BYTE	#14
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#23
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#19
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%11000110
	.BYTE	#17
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%11000110
	.BYTE	#16
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%11000110
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%11000110
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%11000110
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#58
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#17
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#19
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#17
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#23
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#20
	.BYTE	#14
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#20
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#20
	.BYTE	#4
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#20
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#12
	.BYTE	#52
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%11000110
	.BYTE	#17
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%11000110
	.BYTE	#16
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%11000110
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%11000110
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%11000110
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#29
	.BYTE	#52
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%01000110
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#11
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000110
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000111
	.BYTE	#11
	.BYTE	#14
	.BYTE	#0
	.BYTE	#2
	.BYTE	#0
	.BYTE	#2
	.BYTE	#%01000111
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000111
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%01000111
	.BYTE	#14
	.BYTE	#14
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000111
	.BYTE	#23
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01000111
	.BYTE	#19
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#13
	.BYTE	#%01001000
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#19
	.BYTE	#4
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#23
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#21
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#19
	.BYTE	#14
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#19
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#19
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#19
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#11
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#12
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#11
	.BYTE	#14
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#12
	.BYTE	#4
	.BYTE	#0
	.BYTE	#4
	.BYTE	#%01001000
	.BYTE	#14
	.BYTE	#5
	.BYTE	#0
	.BYTE	#4
	.BYTE	#255

.MusicChannel_2
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%01001000
	.BYTE	#12
	.BYTE	#7
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%01001000
	.BYTE	#11
	.BYTE	#8
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%01001000
	.BYTE	#14
	.BYTE	#8
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%01001000
	.BYTE	#17
	.BYTE	#17
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%01001000
	.BYTE	#15
	.BYTE	#8
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%01001000
	.BYTE	#19
	.BYTE	#17
	.BYTE	#0
	.BYTE	#1
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%11001000
	.BYTE	#17
	.BYTE	#7
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%11001000
	.BYTE	#15
	.BYTE	#7
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%11001000
	.BYTE	#19
	.BYTE	#8
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%11001000
	.BYTE	#23
	.BYTE	#17
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%11001000
	.BYTE	#20
	.BYTE	#8
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%11001000
	.BYTE	#26
	.BYTE	#17
	.BYTE	#0
	.BYTE	#1
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%00010010
	.BYTE	#28
	.BYTE	#7
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%00010010
	.BYTE	#25
	.BYTE	#8
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%00010010
	.BYTE	#31
	.BYTE	#8
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%01100010
	.BYTE	#18
	.BYTE	#17
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%01100010
	.BYTE	#16
	.BYTE	#8
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%01100010
	.BYTE	#18
	.BYTE	#8
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%01100010
	.BYTE	#19
	.BYTE	#7
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%00010010
	.BYTE	#19
	.BYTE	#2
	.BYTE	#0
	.BYTE	#34
	.BYTE	#%11001010
	.BYTE	#26
	.BYTE	#8
	.BYTE	#0
	.BYTE	#28
	.BYTE	#%00010001
	.BYTE	#15
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#22
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#23
	.BYTE	#15
	.BYTE	#0
	.BYTE	#2
	.BYTE	#%11000110
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#25
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#26
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#20
	.BYTE	#15
	.BYTE	#0
	.BYTE	#2
	.BYTE	#%00010001
	.BYTE	#31
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#31
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#20
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#15
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#26
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#2
	.BYTE	#%11000110
	.BYTE	#26
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#23
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#31
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#12
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#13
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#2
	.BYTE	#%00010001
	.BYTE	#13
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#13
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#26
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#18
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#16
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#15
	.BYTE	#15
	.BYTE	#0
	.BYTE	#2
	.BYTE	#%11000110
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#26
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#23
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#25
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#26
	.BYTE	#15
	.BYTE	#0
	.BYTE	#2
	.BYTE	#%00010001
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000111
	.BYTE	#26
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#31
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000111
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11001000
	.BYTE	#26
	.BYTE	#15
	.BYTE	#0
	.BYTE	#20
	.BYTE	#%00010010
	.BYTE	#31
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11001000
	.BYTE	#15
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01100010
	.BYTE	#17
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11001000
	.BYTE	#26
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01100010
	.BYTE	#18
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11001000
	.BYTE	#14
	.BYTE	#15
	.BYTE	#0
	.BYTE	#2
	.BYTE	#%01100010
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11001000
	.BYTE	#24
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01100010
	.BYTE	#20
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11001000
	.BYTE	#15
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01100010
	.BYTE	#20
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11001000
	.BYTE	#29
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010010
	.BYTE	#31
	.BYTE	#15
	.BYTE	#0
	.BYTE	#2
	.BYTE	#%11000111
	.BYTE	#26
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#18
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#20
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#15
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#22
	.BYTE	#15
	.BYTE	#0
	.BYTE	#2
	.BYTE	#%00010001
	.BYTE	#23
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#25
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#26
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#2
	.BYTE	#%11000110
	.BYTE	#20
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#31
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#31
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#20
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#15
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#26
	.BYTE	#15
	.BYTE	#0
	.BYTE	#2
	.BYTE	#%00010001
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#26
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#23
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#31
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#12
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#13
	.BYTE	#15
	.BYTE	#0
	.BYTE	#2
	.BYTE	#%11000110
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#13
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#13
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#26
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#18
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#16
	.BYTE	#15
	.BYTE	#0
	.BYTE	#2
	.BYTE	#%00010001
	.BYTE	#15
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#26
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#23
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#25
	.BYTE	#15
	.BYTE	#0
	.BYTE	#2
	.BYTE	#%11000110
	.BYTE	#26
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#26
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#31
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000111
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#2
	.BYTE	#%11000111
	.BYTE	#26
	.BYTE	#15
	.BYTE	#0
	.BYTE	#21
	.BYTE	#%00010010
	.BYTE	#31
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11001000
	.BYTE	#15
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01100010
	.BYTE	#17
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11001000
	.BYTE	#26
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01100010
	.BYTE	#18
	.BYTE	#15
	.BYTE	#0
	.BYTE	#2
	.BYTE	#%11001000
	.BYTE	#14
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01100010
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11001000
	.BYTE	#24
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01100010
	.BYTE	#20
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11001000
	.BYTE	#15
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%01100010
	.BYTE	#20
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11001000
	.BYTE	#29
	.BYTE	#15
	.BYTE	#0
	.BYTE	#2
	.BYTE	#%00010010
	.BYTE	#15
	.BYTE	#15
	.BYTE	#0
	.BYTE	#1
	.BYTE	#0
	.BYTE	#1
	.BYTE	#0
	.BYTE	#1
	.BYTE	#%11001000
	.BYTE	#26
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010010
	.BYTE	#15
	.BYTE	#5
	.BYTE	#0
	.BYTE	#31
	.BYTE	#%00010010
	.BYTE	#31
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11001000
	.BYTE	#15
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010010
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#2
	.BYTE	#%11001000
	.BYTE	#26
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010010
	.BYTE	#31
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11001000
	.BYTE	#15
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000111
	.BYTE	#31
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#23
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#2
	.BYTE	#%00010001
	.BYTE	#23
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#29
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#25
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000111
	.BYTE	#31
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11001000
	.BYTE	#15
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010010
	.BYTE	#31
	.BYTE	#15
	.BYTE	#0
	.BYTE	#2
	.BYTE	#%11001000
	.BYTE	#15
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010010
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11001000
	.BYTE	#26
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010010
	.BYTE	#31
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11001000
	.BYTE	#15
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11001000
	.BYTE	#31
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11001000
	.BYTE	#16
	.BYTE	#15
	.BYTE	#0
	.BYTE	#2
	.BYTE	#%00010010
	.BYTE	#13
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11001000
	.BYTE	#17
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11001000
	.BYTE	#27
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11001000
	.BYTE	#17
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11001000
	.BYTE	#26
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010010
	.BYTE	#23
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11001000
	.BYTE	#31
	.BYTE	#15
	.BYTE	#0
	.BYTE	#2
	.BYTE	#%00010010
	.BYTE	#28
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010010
	.BYTE	#31
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11001000
	.BYTE	#26
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010010
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11001000
	.BYTE	#15
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010010
	.BYTE	#31
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11001000
	.BYTE	#26
	.BYTE	#15
	.BYTE	#0
	.BYTE	#2
	.BYTE	#%00010001
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000111
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#23
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#23
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#29
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#25
	.BYTE	#15
	.BYTE	#0
	.BYTE	#2
	.BYTE	#%11000110
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#31
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#31
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#23
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#29
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#22
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#12
	.BYTE	#15
	.BYTE	#0
	.BYTE	#2
	.BYTE	#%11000110
	.BYTE	#26
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#26
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#26
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#26
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#13
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#23
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#2
	.BYTE	#%11000110
	.BYTE	#20
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#15
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#26
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#25
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#13
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#31
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#15
	.BYTE	#15
	.BYTE	#0
	.BYTE	#2
	.BYTE	#%00010001
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#15
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#15
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#23
	.BYTE	#15
	.BYTE	#0
	.BYTE	#2
	.BYTE	#%11000110
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#23
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#29
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#25
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#15
	.BYTE	#15
	.BYTE	#0
	.BYTE	#2
	.BYTE	#%00010001
	.BYTE	#31
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#15
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#15
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#15
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#31
	.BYTE	#15
	.BYTE	#0
	.BYTE	#2
	.BYTE	#%00010001
	.BYTE	#12
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#13
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#17
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#27
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#19
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%11000110
	.BYTE	#26
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#23
	.BYTE	#15
	.BYTE	#0
	.BYTE	#2
	.BYTE	#%11000110
	.BYTE	#31
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#28
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3
	.BYTE	#%00010001
	.BYTE	#31
	.BYTE	#15
	.BYTE	#0
	.BYTE	#3

	.BYTE	#255


end

;#endregion

   bank 3
   bank 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;#region "Bank 4 Text Screens"

   asm
  ; 24 Char textkernel by AtariAge member c-dw
  ; https://atariage.com/forums/blogs/entry/4523-13-lines/

NO_ILLEGAL_OPCODES = 0
TEXTHEIGHT  = 4

  ; Alphabet Constants
_COMMA  = 0
_DOT    = 1
__      = (1 * TEXTHEIGHT)
_A      = (2 * TEXTHEIGHT)
_B      = (3 * TEXTHEIGHT)
_C      = (4 * TEXTHEIGHT)
_D      = (5 * TEXTHEIGHT)
_E      = (6 * TEXTHEIGHT)
_F      = (7 * TEXTHEIGHT)
_G      = (8 * TEXTHEIGHT)
_H      = (9 * TEXTHEIGHT)
_I      = (10 * TEXTHEIGHT)
_J      = (11 * TEXTHEIGHT)
_K      = (12 * TEXTHEIGHT)
_L      = (13 * TEXTHEIGHT)
_M      = (14 * TEXTHEIGHT)
_N      = (15 * TEXTHEIGHT)
_O      = (16 * TEXTHEIGHT)
_P      = (17 * TEXTHEIGHT)
_Q      = (18 * TEXTHEIGHT)
_R      = (19 * TEXTHEIGHT)
_S      = (20 * TEXTHEIGHT)
_T      = (21 * TEXTHEIGHT)
_U      = (22 * TEXTHEIGHT)
_V      = (23 * TEXTHEIGHT)
_W      = (24 * TEXTHEIGHT)
_X      = (25 * TEXTHEIGHT)
_Y      = (26 * TEXTHEIGHT)
_Z      = (27 * TEXTHEIGHT)
_a      = (28 * TEXTHEIGHT)
_b      = (29 * TEXTHEIGHT)
_c      = (30 * TEXTHEIGHT)
_d      = (31 * TEXTHEIGHT)
_e      = (32 * TEXTHEIGHT)
_f      = (33 * TEXTHEIGHT)
_g      = (34 * TEXTHEIGHT)
_h      = (35 * TEXTHEIGHT)
_i      = (36 * TEXTHEIGHT)
_j      = (37 * TEXTHEIGHT)
_k      = (38 * TEXTHEIGHT)
_l      = (39 * TEXTHEIGHT)
_m      = (40 * TEXTHEIGHT)
_n      = (41 * TEXTHEIGHT)
_o      = (42 * TEXTHEIGHT)
_p      = (43 * TEXTHEIGHT)
_q      = (44 * TEXTHEIGHT)
_r      = (45 * TEXTHEIGHT)
_s      = (46 * TEXTHEIGHT)
_t      = (47 * TEXTHEIGHT)
_u      = (48 * TEXTHEIGHT)
_v      = (49 * TEXTHEIGHT)
_w      = (50 * TEXTHEIGHT)
_x      = (51 * TEXTHEIGHT)
_y      = (52 * TEXTHEIGHT)
_z      = (53 * TEXTHEIGHT)
_0      = (54 * TEXTHEIGHT)
_1      = (55 * TEXTHEIGHT)
_2      = (56 * TEXTHEIGHT)
_3      = (57 * TEXTHEIGHT)
_4      = (58 * TEXTHEIGHT)
_5      = (59 * TEXTHEIGHT)
_6      = (60 * TEXTHEIGHT)
_7      = (61 * TEXTHEIGHT)
_8      = (62 * TEXTHEIGHT)
_9      = (63 * TEXTHEIGHT)
;_BANG   = (64 * TEXTHEIGHT)
;_QMARK  = (65 * TEXTHEIGHT)
;_DASH   = (66 * TEXTHEIGHT)
;_COLON  = (67 * TEXTHEIGHT)


; Text screen ZP-RAM layout
TEXT             = $80 ; 24
BUFF1            = $98 ; 42
CYCLE            = $C2 ;  1
LOOP             = $C3 ;  1
FontColorsLobby  = $C4 ; 10 
ActiveRow        = $CE ;  1
SelectRestrainer = $CF ;  1


SC_RAM_Row_1 = 0
SC_RAM_Row_2 = 24
SC_RAM_Row_3 = 48
SC_RAM_Row_4 = 72
SC_RAM_Row_5 = 96

ROM_Row_1 = 128
ROM_Row_2 = 152
ROM_Row_3 = 176
ROM_Row_4 = 200
ROM_Row_5 = 224


RAM_Messages=r000

ROM_Messages
Message0
  DC.B  __, _C, _H, _O, _O, _S, _E, __, _A, __, _T, _A, _B, _L, _E, __, _T, _O, __, _J, _O, _I, _N, __
Message1
  DC.B  _T, _A, _B, _L, _E, __, __, __, __, __, __, __, __, __, __, __, __, _P, _L, _A, _Y, _E, _R, _S
Message2
  DC.B  __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __
Message3
  DC.B  __, __, __, __, __, __, __, __, _G, _A, _M, _E, __, _O, _V, _E, _R, __, __, __, __, __, __, __

  ; The rows below are not accessible on SC-RAM/ROM mixed Screens
Message4
  DC.B  __, __, __, __, __, __, __, _W, _E, __, _G, _I, _V, _E, __, _U, _P, __, __, __, __, __, __, __
Message5
  DC.B  __, __, __, __, __, __, _S, _P, _E, _C, _I, _A, _L, __, _B, _O, _N, _U, _S, __, __, __, __, __
Message6
  DC.B  __, __, __, __, __, _1, _0, _COMMA, _0, _0, _0, _COMMA, _0, _0, _0, __, _P, _T, _S, __, __, __, __, __
Message7
  DC.B  __, __, __, __, __, _P, _R, _E, _S, _E, _N, _T, _E, _D, __, _B, _Y, __, _N, _N, __, __, __, __

EndMessages
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;#region rem "Lobby Text Screen"

   ;batariBasic entry point to asm bank
_bB_Lobby_entry_bank4
   asm


  lda #_Color_Lobby_Row   ; text color first row
  ldx #_Color_Lobby_BG    ; background color
  jsr _Prepare_Text_Screen

MainLobbyLoop
  ; Do Vertical Sync (VSync Routine by Manuel Polik)
  lda #2
  sta WSYNC
  sta VSYNC
  sta WSYNC
  sta WSYNC
  lsr
  sta WSYNC
  sta VSYNC

  ; Set Vertical Blank Timer 
  ldy #43
  sty TIM64T
  
  ; Update Game Cycle
  dec CYCLE

  ; Clear Sprites
  lda #0
  sta GRP0
  sta GRP1
  sta GRP0
  sta GRP1
  
  ; Set Loop Iterations
  lda #9
  sta LOOP
  
  ; Set Pointer for First Message
  lda #48
  jsr LoadTextROM
  
  ; Set Sprite Positions and Preload First Text Line
  jsr TextPosition
  jsr TextCopy

  ; Wait for Vertical Blank End
WaitVblank
  lda INTIM
  bne WaitVblank
  sta WSYNC
  sta VBLANK
  sta WSYNC

  ; Display First Line
  jsr TextKernel

TextLoop
  
  ; Clear Sprite Data
  lda #0
  sta GRP1
  sta GRP0
  sta GRP1
  
  ; Set Next Message Pointer
  ldx LOOP
  lda FontColorsLobby,X
  sta COLUP0
  sta COLUP1
  lda OffsetLobby,X
  jsr LoadText
  
  ; Copy and Display Message
  jsr TextCopy
  jsr TextKernel
  
  ; Decrement Loop
  dec LOOP
  bpl TextLoop
EndKernel


  ; Start Vertical Blank
  lda #2
  sta WSYNC
  sta WSYNC
  sta WSYNC
  sta WSYNC
  sta WSYNC
  sta WSYNC
  sta VBLANK
  
  ; Set Timer for Overscan
  ldy #35
  sty TIM64T
end

   if ReceiveBufferSize < 120 then _skip_copy_response_to_SC_RAM
  _Bit0_Request_pending{0}  = 0

   asm
   LDX #0
.copy_loop
   LDA	ReceiveBuffer   		; 4
   STA	w000,x		         ; 5   
   INX					         ; 2   
   LDA	ReceiveBufferSize		; 4
   BNE	.copy_loop			   ; 2/3 
end
; Lobby select code
_skip_copy_response_to_SC_RAM
   if _Bit0_Request_pending{0} then skipSelect
   if !joy0fire then _Bit1_reset_restrainer{1} = 0
   if switchreset || joy0right then goto _prepare_Lobby_bank5 bank5
   if joy0fire && !_Bit1_reset_restrainer{1} then _Bit1_reset_restrainer{1} = 1 : goto start bank1
   if SelectRestrainer then skipSelect
   if joy0down && ActiveRow > 2 then decActiveRow
   if joy0up && ActiveRow < 6 then incActiveRow
skipSelect
   if !joy0down && !joy0up then SelectRestrainer = 0

   asm
  ; Finish Overscan
WaitOverscanEnd
  lda INTIM
  bne WaitOverscanEnd
  
  ; Loop To Beginning
  jmp MainLobbyLoop
end

incActiveRow
   asm
   ldx ActiveRow
   lda #_Color_Lobby_Row
   sta FontColorsLobby,x
   inx
setActiveRow
   lda #_Color_Lobby_Active_Row
   sta FontColorsLobby,x
   stx ActiveRow
   stx SelectRestrainer
   jmp WaitOverscanEnd
end

decActiveRow
   asm
   ldx ActiveRow
   lda #_Color_Lobby_Row
   sta FontColorsLobby,x
   dex
   jmp setActiveRow


;#endregion

  ALIGN 256
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;#region rem "Text Screen Kernel"

TextKernel
  ; Alternate each frame
  lda CYCLE
  lsr
  bcs Kernel1A
  jmp Kernel2A
Kernel1A
  sta WSYNC                     ; [0]
  sta HMOVE                     ; [0] + 3       P0A P1A P0B P1B
  lda BUFF1+6                   ; [3] + 3
  sta GRP0                      ; [6] + 3       - - 0 -
  lda BUFF1+13                  ; [9] + 3
  sta GRP1                      ; [12] + 3      0 - - 3   < 36
  lda BUFF1+20                  ; [15] + 3
  sta GRP0                      ; [18] + 3      0 3 5 -   < 42
  SLEEP 6                       ; [21] + 6
  lda #0                        ; [27] + 2
  sta HMP0                      ; [29] + 3
  sta HMP1                      ; [32] + 3
  lda BUFF1+27                  ; [35] + 3
  sta GRP1                      ; [38] + 3      5 3 - 7   > 38 < 47
  lda BUFF1+34                  ; [41] + 3
  sta GRP0                      ; [44] + 3      5 7 9 -   > 44 < 52 
  lda BUFF1+41                  ; [47] + 3
  sta GRP1                      ; [50] + 3      9 7 - 11  > 49 < 58
  sta GRP0                      ; [53] + 3      9 11 - -  > 54 < 63
  SLEEP 15                      ; [56] + 15
  sta HMOVE                     ; [71] + 3      = 74!
  lda BUFF1+2                   ; [74] + 3      P0A P1A P0B P1B 
  sta GRP0                      ; [1] + 3       - - 0 -
  lda BUFF1+9                   ; [4] + 3
  sta GRP1                      ; [7] + 3       0 - - 2   < 34
  lda BUFF1+16                  ; [10] + 3
  sta GRP0                      ; [13] + 3      0 2 4 -   < 39
  SLEEP 9                       ; [16] + 9
  lda #%10000000                ; [25] + 2
  sta HMP0                      ; [27] + 3
  sta HMP1                      ; [30] + 3
  lda BUFF1+23                  ; [33] + 3      
  sta GRP1                      ; [36] + 3      4 2 - 6   > 36 < 44
  lda BUFF1+30                  ; [39] + 3
  sta GRP0                      ; [42] + 3      4 6 8 -   > 41 < 50 
  lda BUFF1+37                  ; [45] + 3
  sta GRP1                      ; [48] + 3      8 6 - 10  > 47 < 55
  sta GRP0                      ; [51] + 3      8 11 - -  > 52 < 60
  ; SPARE 22 CYCLES
  sta WSYNC                     ; [0]
  sta HMOVE                     ; [0] + 3       P0A P1A P0B P1B
  lda BUFF1+5                   ; [3] + 3
  sta GRP0                      ; [6] + 3       - - 0 -
  lda BUFF1+12                  ; [9] + 3
  sta GRP1                      ; [12] + 3      0 - - 3   < 36
  lda BUFF1+19                  ; [15] + 3
  sta GRP0                      ; [18] + 3      0 3 5 -   < 42
  SLEEP 6                       ; [21] + 6
  lda #0                        ; [27] + 2
  sta HMP0                      ; [29] + 3
  sta HMP1                      ; [32] + 3
  lda BUFF1+26                  ; [35] + 3
  sta GRP1                      ; [38] + 3      5 3 - 7   > 38 < 47
  lda BUFF1+33                  ; [41] + 3
  sta GRP0                      ; [44] + 3      5 7 9 -   > 44 < 52 
  lda BUFF1+40                  ; [47] + 3
  sta GRP1                      ; [50] + 3      9 7 - 11  > 49 < 58
  sta GRP0                      ; [53] + 3      9 11 - -  > 54 < 63
  SLEEP 15                      ; [56] + 15
  sta HMOVE                     ; [71] + 3      = 74!
  lda BUFF1+1                   ; [74] + 3      P0A P1A P0B P1B 
  sta GRP0                      ; [1] + 3       - - 0 -
  lda BUFF1+8                   ; [4] + 3
  sta GRP1                      ; [7] + 3       0 - - 2   < 34
  lda BUFF1+15                  ; [10] + 3
  sta GRP0                      ; [13] + 3      0 2 4 -   < 39
  SLEEP 9                       ; [16] + 9
  lda #%10000000                ; [25] + 2
  sta HMP0                      ; [27] + 3
  sta HMP1                      ; [30] + 3
  lda BUFF1+22                  ; [33] + 3      
  sta GRP1                      ; [36] + 3      4 2 - 6   > 36 < 44
  lda BUFF1+29                  ; [39] + 3
  sta GRP0                      ; [42] + 3      4 6 8 -   > 41 < 50 
  lda BUFF1+36                  ; [45] + 3
  sta GRP1                      ; [48] + 3      8 6 - 10  > 47 < 55
  sta GRP0                      ; [51] + 3      8 11 - -  > 52 < 60
  sta WSYNC                     ; [0]
  sta HMOVE                     ; [0] + 3       P0A P1A P0B P1B
  lda BUFF1+4                   ; [3] + 3
  sta GRP0                      ; [6] + 3       - - 0 -
  lda BUFF1+11                  ; [9] + 3
  sta GRP1                      ; [12] + 3      0 - - 3   < 36
  lda BUFF1+18                  ; [15] + 3
  sta GRP0                      ; [18] + 3      0 3 5 -   < 42
  SLEEP 6                       ; [21] + 6
  lda #0                        ; [27] + 2
  sta HMP0                      ; [29] + 3
  sta HMP1                      ; [32] + 3
  lda BUFF1+25                  ; [35] + 3
  sta GRP1                      ; [38] + 3      5 3 - 7   > 38 < 47
  lda BUFF1+32                  ; [41] + 3
  sta GRP0                      ; [44] + 3      5 7 9 -   > 44 < 52 
  lda BUFF1+39                  ; [47] + 3
  sta GRP1                      ; [50] + 3      9 7 - 11  > 49 < 58
  sta GRP0                      ; [53] + 3      9 11 - -  > 54 < 63
  SLEEP 15                      ; [56] + 15
  sta HMOVE                     ; [71] + 3      = 74!
  lda BUFF1+0                   ; [74] + 3      P0A P1A P0B P1B 
  sta GRP0                      ; [1] + 3       - - 0 -
  lda BUFF1+7                   ; [4] + 3
  sta GRP1                      ; [7] + 3       0 - - 2   < 34
  lda BUFF1+14                  ; [10] + 3
  sta GRP0                      ; [13] + 3      0 2 4 -   < 39
  SLEEP 6                       ; [16] + 6
  jmp Kernel1B                  ; [22] + 3
EndKernel1A

  if (>TextKernel != >EndKernel1A)
    echo "WARNING: Kernel1A Crosses Page Boundary!"
  endif

  ALIGN 256
  
Kernel1B
  lda #%10000000                ; [25] + 2
  sta HMP0                      ; [27] + 3
  sta HMP1                      ; [30] + 3
  lda BUFF1+21                  ; [33] + 3      
  sta GRP1                      ; [36] + 3      4 2 - 6   > 36 < 44
  lda BUFF1+28                  ; [39] + 3
  sta GRP0                      ; [42] + 3      4 6 8 -   > 41 < 50 
  lda BUFF1+35                  ; [45] + 3
  sta GRP1                      ; [48] + 3      8 6 - 10  > 47 < 55
  sta GRP0                      ; [51] + 3      8 11 - -  > 52 < 60
  ; SPARE 22 CYCLES
  sta WSYNC                     ; [0]
  sta HMOVE                     ; [0] + 3       P0A P1A P0B P1B
  lda BUFF1+3                   ; [3] + 3
  sta GRP0                      ; [6] + 3       - - 0 -
  lda BUFF1+10                  ; [9] + 3
  sta GRP1                      ; [12] + 3      0 - - 3   < 36
  lda BUFF1+17                  ; [15] + 3
  sta GRP0                      ; [18] + 3      0 3 5 -   < 42
  SLEEP 6                       ; [21] + 6
  lda #0                        ; [27] + 2
  sta HMP0                      ; [29] + 3
  sta HMP1                      ; [32] + 3
  lda BUFF1+24                  ; [35] + 3
  sta GRP1                      ; [38] + 3      5 3 - 7   > 38 < 47
  lda BUFF1+31                  ; [41] + 3
  sta GRP0                      ; [44] + 3      5 7 9 -   > 44 < 52 
  lda BUFF1+38                  ; [47] + 3
  sta GRP1                      ; [50] + 3      9 7 - 11  > 49 < 58
  sta GRP0                      ; [53] + 3      9 11 - -  > 54 < 63
EndKernel1B
  rts
  
Kernel2A
  sta WSYNC                     ; [0]
  lda BUFF1+3                   ; [0] + 3      P0A P1A P0B P1B 
  sta GRP0                      ; [3] + 3       - - 0 -
  lda BUFF1+10                  ; [6] + 3
  sta GRP1                      ; [9] + 3       0 - - 2   < 34
  lda BUFF1+17                  ; [12] + 3
  sta GRP0                      ; [15] + 3      0 2 4 -   < 39
  SLEEP 7                       ; [18] + 7
  lda #%10000000                ; [25] + 2
  sta HMP0                      ; [27] + 3
  sta HMP1                      ; [30] + 3
  lda BUFF1+24                  ; [33] + 3
  sta GRP1                      ; [36] + 3      4 2 - 6   > 36 < 44
  lda BUFF1+31                  ; [39] + 3
  sta GRP0                      ; [42] + 3      4 6 8 -   > 41 < 50 
  lda BUFF1+38                  ; [45] + 3
  sta GRP1                      ; [48] + 3      8 6 - 10  > 47 < 55
  sta GRP0                      ; [51] + 3      8 11 - -  > 52 < 60
  sta WSYNC                     ; [0]
  sta HMOVE                     ; [0] + 3       P0A P1A P0B P1B
  lda BUFF1+6                   ; [3] + 3
  sta GRP0                      ; [6] + 3       - - 0 -
  lda BUFF1+13                  ; [9] + 3
  sta GRP1                      ; [12] + 3      0 - - 3   < 36
  lda BUFF1+20                  ; [15] + 3
  sta GRP0                      ; [18] + 3      0 3 5 -   < 42
  SLEEP 15                      ; [21] + 15
  lda BUFF1+27                  ; [36] + 3
  sta GRP1                      ; [39] + 3      5 3 - 7   > 38 < 47
  lda BUFF1+34                  ; [42] + 3
  sta GRP0                      ; [45] + 3      5 7 9 -   > 44 < 52
  lda BUFF1+41                  ; [48] + 3
  sta GRP1                      ; [51] + 3      9 7 - 11  > 49 < 58
  sta GRP0                      ; [54] + 3      9 11 - -  > 54 < 63
  lda #0                        ; [57] + 2
  sta HMP0                      ; [59] + 3
  sta HMP1                      ; [62] + 3
  SLEEP 6                       ; [65] + 6
  sta HMOVE                     ; [71] + 3      = 74!   
  lda BUFF1+2                   ; [74] + 3      P0A P1A P0B P1B 
  sta GRP0                      ; [1] + 3       - - 0 -
  lda BUFF1+9                   ; [4] + 3
  sta GRP1                      ; [7] + 3       0 - - 2   < 34
  lda BUFF1+16                  ; [10] + 3
  sta GRP0                      ; [13] + 3      0 2 4 -   < 39
  SLEEP 9                       ; [16] + 9
  lda #%10000000                ; [25] + 2
  sta HMP0                      ; [27] + 3
  sta HMP1                      ; [30] + 3
  lda BUFF1+23                  ; [33] + 3
  sta GRP1                      ; [36] + 3      4 2 - 6   > 36 < 44
  lda BUFF1+30                  ; [39] + 3
  sta GRP0                      ; [42] + 3      4 6 8 -   > 41 < 50 
  lda BUFF1+37                  ; [45] + 3
  sta GRP1                      ; [48] + 3      8 6 - 10  > 47 < 55
  sta GRP0                      ; [51] + 3      8 11 - -  > 52 < 60
  sta WSYNC                     ; [0]
  sta HMOVE                     ; [0] + 3       P0A P1A P0B P1B
  lda BUFF1+5                   ; [3] + 3
  sta GRP0                      ; [6] + 3       - - 0 -
  lda BUFF1+12                  ; [9] + 3
  sta GRP1                      ; [12] + 3      0 - - 3   < 36
  lda BUFF1+19                  ; [15] + 3
  sta GRP0                      ; [18] + 3      0 3 5 -   < 42
  SLEEP 15                      ; [21] + 15
  lda BUFF1+26                  ; [36] + 3
  sta GRP1                      ; [39] + 3      5 3 - 7   > 38 < 47
  lda BUFF1+33                  ; [42] + 3
  sta GRP0                      ; [45] + 3      5 7 9 -   > 44 < 52
  lda BUFF1+40                  ; [48] + 3
  sta GRP1                      ; [51] + 3      9 7 - 11  > 49 < 58
  sta GRP0                      ; [54] + 3      9 11 - -  > 54 < 63
  lda #0                        ; [57] + 2
  sta HMP0                      ; [59] + 3
  sta HMP1                      ; [62] + 3
  SLEEP 3                       ; [65] + 3
  jmp Kernel2B                  ; [68] + 3
EndKernel2A
  
  if (>Kernel1B != >EndKernel2A)
    echo "WARNING: Kernel1B/2A Crosses Page Boundary!"
  endif

  ALIGN 256

Kernel2B
  sta HMOVE                     ; [71] + 3      = 74!   
  lda BUFF1+1                   ; [74] + 3      P0A P1A P0B P1B 
  sta GRP0                      ; [1] + 3       - - 0 -
  lda BUFF1+8                   ; [4] + 3
  sta GRP1                      ; [7] + 3       0 - - 2   < 34
  lda BUFF1+15                  ; [10] + 3
  sta GRP0                      ; [13] + 3      0 2 4 -   < 39
  SLEEP 9                       ; [16] + 9
  lda #%10000000                ; [25] + 2
  sta HMP0                      ; [27] + 3
  sta HMP1                      ; [30] + 3
  lda BUFF1+22                  ; [33] + 3
  sta GRP1                      ; [36] + 3      4 2 - 6   > 36 < 44
  lda BUFF1+29                  ; [39] + 3
  sta GRP0                      ; [42] + 3      4 6 8 -   > 41 < 50 
  lda BUFF1+36                  ; [45] + 3
  sta GRP1                      ; [48] + 3      8 6 - 10  > 47 < 55
  sta GRP0                      ; [51] + 3      8 11 - -  > 52 < 60
  sta WSYNC                     ; [0]
  sta HMOVE                     ; [0] + 3       P0A P1A P0B P1B
  lda BUFF1+4                   ; [3] + 3
  sta GRP0                      ; [6] + 3       - - 0 -
  lda BUFF1+11                  ; [9] + 3
  sta GRP1                      ; [12] + 3      0 - - 3   < 36
  lda BUFF1+18                  ; [15] + 3
  sta GRP0                      ; [18] + 3      0 3 5 -   < 42
  SLEEP 15                      ; [21] + 15
  lda BUFF1+25                  ; [36] + 3
  sta GRP1                      ; [39] + 3      5 3 - 7   > 38 < 47
  lda BUFF1+32                  ; [42] + 3
  sta GRP0                      ; [45] + 3      5 7 9 -   > 44 < 52
  lda BUFF1+39                  ; [48] + 3
  sta GRP1                      ; [51] + 3      9 7 - 11  > 49 < 58
  sta GRP0                      ; [54] + 3      9 11 - -  > 54 < 63
  lda #0                        ; [57] + 2
  sta HMP0                      ; [59] + 3
  sta HMP1                      ; [62] + 3
  SLEEP 6                       ; [65] + 6
  sta HMOVE                     ; [71] + 3      = 74!   
  lda BUFF1+0                   ; [74] + 3      P0A P1A P0B P1B 
  sta GRP0                      ; [1] + 3       - - 0 -
  lda BUFF1+7                   ; [4] + 3
  sta GRP1                      ; [7] + 3       0 - - 2   < 34
  lda BUFF1+14                  ; [10] + 3
  sta GRP0                      ; [13] + 3      0 2 4 -   < 39
  SLEEP 9                       ; [16] + 9
  lda #%10000000                ; [25] + 2
  sta HMP0                      ; [27] + 3
  sta HMP1                      ; [30] + 3
  lda BUFF1+21                  ; [33] + 3
  sta GRP1                      ; [36] + 3      4 2 - 6   > 36 < 44
  lda BUFF1+28                  ; [39] + 3
  sta GRP0                      ; [42] + 3      4 6 8 -   > 41 < 50 
  lda BUFF1+35                  ; [45] + 3
  sta GRP1                      ; [48] + 3      8 6 - 10  > 47 < 55
  sta GRP0                      ; [51] + 3      8 11 - -  > 52 < 60
EndKernel2B
  rts
  
  ; Set Initial Sprite Positions
TextPosition
  lda CYCLE
  lsr
  bcs Position1
  jmp Position2
Position1
  sta WSYNC
  lda #%10010000                ; [0] + 2
  sta HMP0                      ; [2] + 3
  lda #%10000000                ; [5] + 2
  sta HMP1                      ; [7] + 3
  SLEEP 19                      ; [10] + 19
  sta RESP0                     ; [29] + 3 = 32
  nop                           ; [32] + 2
  sta RESP1                     ; [34] + 3 = 37
  rts
Position2
  sta WSYNC
  SLEEP 29                      ; [0] + 29
  sta RESP0                     ; [29] + 3 = 32
  nop                           ; [32] + 2
  sta RESP1                     ; [34] + 3 = 37
  lda #%10010000                ; [37] + 2
  sta HMP0                      ; [39] + 3
  lda #%10000000                ; [42] + 2
  sta HMP1                      ; [44] + 3 
  SLEEP 24                      ; [47] + 21
  sta HMOVE                     ; [71] + 3
  rts
EndPosition

  if (>Kernel2B != >EndPosition)
    echo "WARNING: Kernel2B Crosses Page Boundary!"
  endif

  ALIGN 256

  ; Copy Text Into Buffer
TextCopy
  lda CYCLE
  lsr
  bcs TextCopy1
  jmp TextCopy2
TextCopy1
  ldx TEXT+23         ; [0] + 3
  ldy TEXT+22         ; [3] + 3
  lda L2CHARS+3,Y     ; [6] + 4
  ora R2CHARS+3,X     ; [10] + 4
  sta BUFF1+41        ; [14] + 3
  lda L2CHARS+2,Y     ; [17] + 4
  ora R2CHARS+2,X     ; [21] + 4
  sta BUFF1+40        ; [25] + 3
  lda L2CHARS+1,Y     ; [28] + 4
  ora R2CHARS+1,X     ; [32] + 4
  sta BUFF1+39        ; [36] + 3
  lda L2CHARS+0,Y     ; [39] + 4
  ora R2CHARS+0,X     ; [43] + 4
  sta BUFF1+38        ; [47] + 3 = 50
  ldx TEXT+21
  ldy TEXT+20
  lda L1CHARS+3,Y
  ora R1CHARS+3,X
  sta BUFF1+37
  lda L1CHARS+2,Y
  ora R1CHARS+2,X
  sta BUFF1+36
  lda L1CHARS+1,Y
  ora R1CHARS+1,X
  sta BUFF1+35
  ldx TEXT+19
  ldy TEXT+18
  lda L2CHARS+3,Y
  ora R2CHARS+3,X
  sta BUFF1+34
  lda L2CHARS+2,Y
  ora R2CHARS+2,X
  sta BUFF1+33
  lda L2CHARS+1,Y
  ora R2CHARS+1,X
  sta BUFF1+32
  lda L2CHARS+0,Y
  ora R2CHARS+0,X
  sta BUFF1+31
  ldx TEXT+17
  ldy TEXT+16
  lda L1CHARS+3,Y
  ora R1CHARS+3,X
  sta BUFF1+30
  lda L1CHARS+2,Y
  ora R1CHARS+2,X
  sta BUFF1+29
  lda L1CHARS+1,Y
  ora R1CHARS+1,X
  sta BUFF1+28
  ldx TEXT+15
  ldy TEXT+14
  lda L2CHARS+3,Y
  ora R2CHARS+3,X
  sta BUFF1+27
  lda L2CHARS+2,Y
  ora R2CHARS+2,X
  sta BUFF1+26
  lda L2CHARS+1,Y
  ora R2CHARS+1,X
  sta BUFF1+25
  lda L2CHARS+0,Y
  ora R2CHARS+0,X
  sta BUFF1+24
  ldx TEXT+13
  ldy TEXT+12
  lda L1CHARS+3,Y
  ora R1CHARS+3,X
  sta BUFF1+23
  lda L1CHARS+2,Y
  ora R1CHARS+2,X
  sta BUFF1+22
  lda L1CHARS+1,Y
  ora R1CHARS+1,X
  sta BUFF1+21
  ldx TEXT+11
  ldy TEXT+10
  lda L2CHARS+3,Y
  ora R2CHARS+3,X
  sta BUFF1+20
  lda L2CHARS+2,Y
  ora R2CHARS+2,X
  sta BUFF1+19
  lda L2CHARS+1,Y
  ora R2CHARS+1,X
  sta BUFF1+18
  lda L2CHARS+0,Y
  ora R2CHARS+0,X
  sta BUFF1+17
  ldx TEXT+9
  ldy TEXT+8
  lda L1CHARS+3,Y
  ora R1CHARS+3,X
  sta BUFF1+16
  lda L1CHARS+2,Y
  ora R1CHARS+2,X
  sta BUFF1+15
  lda L1CHARS+1,Y
  ora R1CHARS+1,X
  sta BUFF1+14
  ldx TEXT+7
  ldy TEXT+6
  lda L2CHARS+3,Y
  ora R2CHARS+3,X
  sta BUFF1+13
  lda L2CHARS+2,Y
  ora R2CHARS+2,X
  sta BUFF1+12
  lda L2CHARS+1,Y
  ora R2CHARS+1,X
  sta BUFF1+11
  lda L2CHARS+0,Y
  ora R2CHARS+0,X
  sta BUFF1+10
  ldx TEXT+5
  ldy TEXT+4
  lda L1CHARS+3,Y
  ora R1CHARS+3,X
  sta BUFF1+9
  lda L1CHARS+2,Y
  ora R1CHARS+2,X
  sta BUFF1+8
  lda L1CHARS+1,Y
  ora R1CHARS+1,X
  sta BUFF1+7
  ldx TEXT+3
  ldy TEXT+2
  lda L2CHARS+3,Y
  ora R2CHARS+3,X
  sta BUFF1+6
  lda L2CHARS+2,Y
  ora R2CHARS+2,X
  sta BUFF1+5
  lda L2CHARS+1,Y
  ora R2CHARS+1,X
  sta BUFF1+4
  lda L2CHARS+0,Y
  ora R2CHARS+0,X
  sta BUFF1+3
  ldx TEXT+1
  ldy TEXT+0
  lda L1CHARS+3,Y
  ora R1CHARS+3,X
  sta BUFF1+2
  lda L1CHARS+2,Y
  ora R1CHARS+2,X
  sta BUFF1+1
  lda L1CHARS+1,Y
  ora R1CHARS+1,X
  sta BUFF1+0
  rts
EndTextCopy1

TextCopy2
  ldx TEXT+23       ; [0] + 3
  ldy TEXT+22       ; [3] + 3
  lda L1CHARS+3,Y   ; [6] + 4
  ora R1CHARS+3,X   ; [10] + 4
  sta BUFF1+41      ; [14] + 3
  lda L1CHARS+2,Y   ; [17] + 4
  ora R1CHARS+2,X   ; [21] + 4
  sta BUFF1+40      ; [25] + 3
  lda L1CHARS+1,Y   ; [28] + 4
  ora R1CHARS+1,X   ; [32] + 4
  sta BUFF1+39      ; [36] + 3 = 39
  ldx TEXT+21
  ldy TEXT+20
  lda L2CHARS+3,Y
  ora R2CHARS+3,X
  sta BUFF1+38
  lda L2CHARS+2,Y
  ora R2CHARS+2,X
  sta BUFF1+37
  lda L2CHARS+1,Y
  ora R2CHARS+1,X
  sta BUFF1+36
  lda L2CHARS+0,Y
  ora R2CHARS+0,X
  sta BUFF1+35
  ldx TEXT+19
  ldy TEXT+18
  lda L1CHARS+3,Y
  ora R1CHARS+3,X
  sta BUFF1+34
  lda L1CHARS+2,Y
  ora R1CHARS+2,X
  sta BUFF1+33
  lda L1CHARS+1,Y
  ora R1CHARS+1,X
  sta BUFF1+32
  ldx TEXT+17
  ldy TEXT+16
  lda L2CHARS+3,Y
  ora R2CHARS+3,X
  sta BUFF1+31
  lda L2CHARS+2,Y
  ora R2CHARS+2,X
  sta BUFF1+30
  lda L2CHARS+1,Y
  ora R2CHARS+1,X
  sta BUFF1+29
  lda L2CHARS+0,Y
  ora R2CHARS+0,X
  sta BUFF1+28
  ldx TEXT+15
  ldy TEXT+14
  lda L1CHARS+3,Y
  ora R1CHARS+3,X
  sta BUFF1+27
  lda L1CHARS+2,Y
  ora R1CHARS+2,X
  sta BUFF1+26
  lda L1CHARS+1,Y
  ora R1CHARS+1,X
  sta BUFF1+25
  ldx TEXT+13
  ldy TEXT+12
  lda L2CHARS+3,Y
  ora R2CHARS+3,X
  sta BUFF1+24
  lda L2CHARS+2,Y
  ora R2CHARS+2,X
  sta BUFF1+23
  lda L2CHARS+1,Y
  ora R2CHARS+1,X
  sta BUFF1+22
  lda L2CHARS+0,Y
  ora R2CHARS+0,X
  sta BUFF1+21
  ldx TEXT+11
  ldy TEXT+10
  lda L1CHARS+3,Y
  ora R1CHARS+3,X
  sta BUFF1+20
  lda L1CHARS+2,Y
  ora R1CHARS+2,X
  sta BUFF1+19
  lda L1CHARS+1,Y
  ora R1CHARS+1,X
  sta BUFF1+18
  ldx TEXT+9
  ldy TEXT+8
  lda L2CHARS+3,Y
  ora R2CHARS+3,X
  sta BUFF1+17
  lda L2CHARS+2,Y
  ora R2CHARS+2,X
  sta BUFF1+16
  lda L2CHARS+1,Y
  ora R2CHARS+1,X
  sta BUFF1+15
  lda L2CHARS+0,Y
  ora R2CHARS+0,X
  sta BUFF1+14
  ldx TEXT+7
  ldy TEXT+6
  lda L1CHARS+3,Y
  ora R1CHARS+3,X
  sta BUFF1+13
  lda L1CHARS+2,Y
  ora R1CHARS+2,X
  sta BUFF1+12
  lda L1CHARS+1,Y
  ora R1CHARS+1,X
  sta BUFF1+11
  ldx TEXT+5
  ldy TEXT+4
  lda L2CHARS+3,Y
  ora R2CHARS+3,X
  sta BUFF1+10
  lda L2CHARS+2,Y
  ora R2CHARS+2,X
  sta BUFF1+9
  lda L2CHARS+1,Y
  ora R2CHARS+1,X
  sta BUFF1+8
  lda L2CHARS+0,Y
  ora R2CHARS+0,X
  sta BUFF1+7
  ldx TEXT+3
  ldy TEXT+2
  lda L1CHARS+3,Y
  ora R1CHARS+3,X
  sta BUFF1+6
  lda L1CHARS+2,Y
  ora R1CHARS+2,X
  sta BUFF1+5
  lda L1CHARS+1,Y
  ora R1CHARS+1,X
  sta BUFF1+4
  ldx TEXT+1
  ldy TEXT+0
  lda L2CHARS+3,Y
  ora R2CHARS+3,X
  sta BUFF1+3
  lda L2CHARS+2,Y
  ora R2CHARS+2,X
  sta BUFF1+2 
  lda L2CHARS+1,Y
  ora R2CHARS+1,X
  sta BUFF1+1
  lda L2CHARS+0,Y
  ora R2CHARS+0,X
  sta BUFF1+0
  rts
EndTextCopy2
;#endregion

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;#region rem "Text Kernel Subroutines"
LoadText
  bpl	LoadTextRAM
  and #%01111111
LoadTextROM
  tay
  lda ROM_Messages+23,Y
  sta TEXT+23
  lda ROM_Messages+22,Y
  sta TEXT+22
  lda ROM_Messages+21,Y
  sta TEXT+21
  lda ROM_Messages+20,Y
  sta TEXT+20
  lda ROM_Messages+19,Y
  sta TEXT+19
  lda ROM_Messages+18,Y
  sta TEXT+18
  lda ROM_Messages+17,Y
  sta TEXT+17
  lda ROM_Messages+16,Y
  sta TEXT+16
  lda ROM_Messages+15,Y
  sta TEXT+15
  lda ROM_Messages+14,Y
  sta TEXT+14
  lda ROM_Messages+13,Y
  sta TEXT+13
  lda ROM_Messages+12,Y
  sta TEXT+12
  lda ROM_Messages+11,Y
  sta TEXT+11
  lda ROM_Messages+10,Y
  sta TEXT+10
  lda ROM_Messages+9,Y
  sta TEXT+9
  lda ROM_Messages+8,Y
  sta TEXT+8
  lda ROM_Messages+7,Y
  sta TEXT+7
  lda ROM_Messages+6,Y
  sta TEXT+6
  lda ROM_Messages+5,Y
  sta TEXT+5
  lda ROM_Messages+4,Y
  sta TEXT+4
  lda ROM_Messages+3,Y
  sta TEXT+3
  lda ROM_Messages+2,Y
  sta TEXT+2
  lda ROM_Messages+1,Y
  sta TEXT+1
  lda ROM_Messages+0,Y
  sta TEXT+0
  rts

LoadTextRAM
  tay
  lda RAM_Messages+23,Y
  sta TEXT+23
  lda RAM_Messages+22,Y
  sta TEXT+22
  lda RAM_Messages+21,Y
  sta TEXT+21
  lda RAM_Messages+20,Y
  sta TEXT+20
  lda RAM_Messages+19,Y
  sta TEXT+19
  lda RAM_Messages+18,Y
  sta TEXT+18
  lda RAM_Messages+17,Y
  sta TEXT+17
  lda RAM_Messages+16,Y
  sta TEXT+16
  lda RAM_Messages+15,Y
  sta TEXT+15
  lda RAM_Messages+14,Y
  sta TEXT+14
  lda RAM_Messages+13,Y
  sta TEXT+13
  lda RAM_Messages+12,Y
  sta TEXT+12
  lda RAM_Messages+11,Y
  sta TEXT+11
  lda RAM_Messages+10,Y
  sta TEXT+10
  lda RAM_Messages+9,Y
  sta TEXT+9
  lda RAM_Messages+8,Y
  sta TEXT+8
  lda RAM_Messages+7,Y
  sta TEXT+7
  lda RAM_Messages+6,Y
  sta TEXT+6
  lda RAM_Messages+5,Y
  sta TEXT+5
  lda RAM_Messages+4,Y
  sta TEXT+4
  lda RAM_Messages+3,Y
  sta TEXT+3
  lda RAM_Messages+2,Y
  sta TEXT+2
  lda RAM_Messages+1,Y
  sta TEXT+1
  lda RAM_Messages+0,Y
  sta TEXT+0
  rts
end
;#endregion


   asm
_Prepare_Text_Screen

  ; Set P0/P1 Colours
  sta COLUP0
  sta COLUP1

  ; Set Background
  stx COLUBK

  ; Set 3 sprite copies
  lda #%00000110
  sta NUSIZ0
  sta NUSIZ1

  ; Delay P0 & P1
  lda #%00000001
  sta VDELP0
  sta VDELP1
  

  ; Reflect PF
  lda #1
  sta CTRLPF
  
  lda #_00
  sta COLUPF
  sta PF1
  sta PF2
  lda #$30
  sta PF0
  rts

; Colors and rows tables  
OffsetLobby
  DC.B  ROM_Row_3, ROM_Row_3, SC_RAM_Row_5, SC_RAM_Row_4, SC_RAM_Row_3, SC_RAM_Row_2, SC_RAM_Row_1, ROM_Row_3, ROM_Row_2, ROM_Row_1

  ; Include Font Data
  INCLUDE "font.h"
end

;#endregion

   bank 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;#region "Bank 5 prepare SC-RAM for textscreens"

_prepare_Lobby_bank5
  ; Send request for FujiNet lobby
  WriteSendBuffer = LOAD_LOBBY
  _Bit0_Request_pending{0} = 1

  gosub _copy_loading_msg
  goto _bB_Lobby_entry_bank4 bank4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;#region "Subroutine and data bank 5"

_copy_loading_msg
  asm
  ; Copy loading message to SC-RAM
  lda #__
  ldx #119
clear_zp_ram_loop
  sta w000,x
  dex
  bpl clear_zp_ram_loop

  ldx #6
loading_msg_copy_loop
  lda loading_message,x
  sta w033,x
  lda #_Color_Lobby_Row
  sta FontColorsLobby+2,x
  dex
  bpl loading_msg_copy_loop
  sta FontColorsLobby+9
  lda #6
  sta ActiveRow
  lda #_Color_Lobby_Active_Row
  sta FontColorsLobby+6
end
  return

   data loading_message
   _L, _o, _a, _d, _i, _n, _g
end



;#endregion


;#endregion

   bank 6
   bank 7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;#region "Bank 7 Titlescreen"

titlescreen_start
   COLUBK = _Color_Titlescreen_BG
   COLUPF = _Color_Titlescreen_PF
   PF0 = $C0
   PF1 = $FF : PF2 = $FF
   _Bit1_reset_restrainer{1} = 1

  goto RestartMusic bank2

titlescreen            

   goto Music_Player bank2
return_from_music
   gosub titledrawscreen


   if !_Bit5_PlusROM{5} then titlescreen
   if !joy0fire then _Bit1_reset_restrainer{1} = 0
   if joy0fire && !_Bit1_reset_restrainer{1} then gosub _clean_RAM : goto _prepare_Lobby_bank5 bank5

   asm
   lda SWCHA
   lda SWCHB
end
   goto titlescreen

_clean_RAM
   _Bit1_reset_restrainer{1} = 1
   AUDV0 = 0 : AUDV1 = 0
   return thisbank  

   asm
   include "titlescreen/asm/titlescreen.asm"
end
;#endregion


   asm
   ; 5 Card Stud PlusROM API definition.
   SET_PLUSROM_API "a", "5cs.firmaplus.de"
end

   bank 8
