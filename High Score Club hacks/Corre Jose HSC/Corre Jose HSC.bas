
 inline PlusROM_functions.asm

 set kernel_options pfheights ;  no_blank_lines 

 ; PlusROM HSC Id (https://highscore.firmaplus.de/index.php?game_id=88)
 const HighScoreDB_ID = 88

 pfheights:
   3
   39
   7
   2
   2
   3
   2
   12
   8
   1
   9
end


       playfield:
................................
................................
................................
................................
................................
................................
................................
................................
................................
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
................................
end

   ;***************************************************************
   ;
   ;  Variable aliases go here (DIMs).
   ;
   ;  You can have more than one alias for each variable.
   ;  If you use different aliases for bit operations,
   ;  it's easier to understand and remember what they do.
   ;
   ;  I start variable aliases with one underscore so I won't
   ;  have to worry that I might be using bB keywords by mistake.
   ;  I also start labels with two underscores for the same
   ;  reason. The second underscore also makes labels stand out 
   ;  so I can tell at a glance that they are labels and not
   ;  variables.
   ;
   ;  Use bit operations any time you need a simple off/on
   ;  variable. One variable essentially becomes 8 smaller
   ;  variables when you use bit operations.
   ;
   ;  I start my bit aliases with "_Bit" then follow that
   ;  with the bit number from 0 to 7, then another underscore
   ;  and the name. Example: _Bit0_Reset_Restrainer 
   ;
   ;```````````````````````````````````````````````````````````````
   ;  Player0 fixed point variables for more flexibility in
   ;  gameplay mechanics.
   ;
   dim _P0_L_R = player0x.a
   ;dim missel_x = missile0x.b
   dim frame = c

   dim incrementa_score = d
   dim conta_ate10 = e
   ;dim f
   dim conta_ate6 = g

   dim aceleracao = h.i
   dim hora_de_acelerar = j

   dim cor_fundo = k
   dim cor_boneco = l

   dim ufo = m
   m = 0

   dim modo_demo = o
   o = 0


   dim pulo = p
   dim segura_no_ar = q

   ; usado para o som da moeda
   dim Duration = r
   r = 1
   dim som_pegou_item = s
   s = 0 
  dim SoundDataLoc = t ; u also used
  t = 0
  u = 0

  dim sprite = v
  v = 0
 
   ;```````````````````````````````````````````````````````````````
   ;  Bits for various jobs.
   ;
   dim _BitOp_01 = y
   dim _Bit0_Reset_Restrainer = y
   dim _bit_escuro_claro = y

   ;```````````````````````````````````````````````````````````````
   ;  Makes better random numbers.
   ;
   dim rand16 = z



   ;***************************************************************
   ;
   ;  Defines the edges of the playfield for an 8 x 8 sprite.
   ;  If your sprite is a different size, you`ll need to adjust
   ;  the numbers.
   ;
   const _P_Edge_Top = 9
   const _P_Edge_Bottom = 88
   const _P_Edge_Left = 1
   const _P_Edge_Right = 153



   ;***************************************************************
   ;
   ;  Disables the score. (We don`t need it in this program.)
   ;

_Reset

 ;ativo o modo de demonstração ao ligar o jogo ou dar reset
 modo_demo = 1


   ;***************************************************************
   ;***************************************************************
   ;
   ;  PROGRAM START/RESTART
   ;
   ;
__Start_Restart


 
   ;***************************************************************
   ;
   ;  Mutes volume of both sound channels.
   ;
   AUDV0 = 0 : AUDV1 = 0
   score = 0

   ;***************************************************************
   ;
   ;  Clears 25 of the normal 26 variables (fastest way).
   ;  The variable z is used for random numbers in this program
   ;  and clearing it would mess up those random numbers.
   ;
   a = 0 : b = 0 : c = 0 : d = 0 : e = 0 : f = 0 : g = 0 : h = 0 : i = 0
   j = 0 : k = 0 : l = 0 : m = 0 : n = 0 : p = 0 : q = 0 : r = 0
   s = 0 : t = 0 : u = 0 : v = 0 : w = 0 : x = 0 : y = 0


   ;***************************************************************
   ;
   ;  Starting position of Player0.
   ;
   player0x = 130 : player0y = 78
   player1x = 40 : player1y = 78
   ;missile0x = 80 : missile0y = 80





   ;***************************************************************
   ;
   ;  Restrains the reset switch for the main loop.
   ;
   ;  This bit fixes it so the reset switch becomes inactive if
   ;  it hasn't been released after being pressed once.
   ;
   _Bit0_Reset_Restrainer{0} = 1


   ;***************************************************************
   ;
   ;  Defines shape of player0 sprite.
   ;
 player0:
        %00011000
        %00011000
        %00011000
        %00011000
        %00011000
        %01111110
        %01011010
        %01011010
        %01011010
        %01011010
        %01011010
        %01011010
        %00011010
        %00011000
        %00011000
end

   ;defino a aceleração inicial
   aceleracao = 1.0

    gosub Init_gotthecoin
   
   drawscreen

   ;***************************************************************
   ;***************************************************************
   ;
   ;  MAIN LOOP (MAKES THE PROGRAM GO)
   ;
   ;

   dim _sc1 = score
   dim _sc2 = score+1
   dim _sc3 = score+2

    scorecolor = $02

   cor_boneco = $00
   cor_fundo = $0E

   ;cor_boneco = $0A
   ;cor_fundo = $00

   missile0x = 120 
   missile1x = 122

__Main_Loop

 if modo_demo = 1 && joy0fire then modo_demo = 0 : goto __Start_Restart


 NUSIZ0 = $30

 if _bit_escuro_claro{2} then gosub _ceu_estrelado else bally = 0 : missile0y = 0 : missile1y = 0

    ;escurecer no modo demo
    if modo_demo = 1 && incrementa_score = 123 then incrementa_score = 0
    if modo_demo = 1 && incrementa_score >= 100 && !_bit_escuro_claro{2} then gosub _escurecer
    if modo_demo = 1 && incrementa_score >= 100 && _bit_escuro_claro{2} then gosub _escurecer
    if modo_demo = 1 && conta_ate6 = 1 then incrementa_score = incrementa_score + 1

    ;if modo_demo = 1 && cor_fundo = cor_boneco then  cor_boneco = cor_boneco + 2
    

    temp6 = _sc2
    if temp6=$03 && !_bit_escuro_claro{2} then gosub _escurecer : ufo = (rand&7) : missile0x = 130 : missile1x = 132
    if temp6=$06 && _bit_escuro_claro{2} then gosub _escurecer
    if temp6=$09 && !_bit_escuro_claro{2} then gosub _escurecer : ufo = (rand&3) : missile0x = 130 : missile1x = 132
    if temp6=$12 && _bit_escuro_claro{2} then gosub _escurecer
    if temp6=$15 && !_bit_escuro_claro{2} then gosub _escurecer : ufo = (rand&3) : missile0x = 130 : missile1x = 132
    if temp6=$18 && _bit_escuro_claro{2} then gosub _escurecer
    if temp6=$21 && !_bit_escuro_claro{2} then gosub _escurecer : ufo = (rand&1) : missile0x = 130 : missile1x = 132
    if temp6=$24 && _bit_escuro_claro{2} then gosub _escurecer
    if temp6=$27 && !_bit_escuro_claro{2} then gosub _escurecer : ufo = (rand&1) : missile0x = 130 : missile1x = 132
    if temp6=$30 && _bit_escuro_claro{2} then gosub _escurecer
    if temp6=$33 && !_bit_escuro_claro{2} then gosub _escurecer : ufo = 1 : missile0x = 130 : missile1x = 132
    if temp6=$36 && _bit_escuro_claro{2} then gosub _escurecer
    if temp6=$39 && !_bit_escuro_claro{2} then gosub _escurecer : ufo = 1 : missile0x = 130 : missile1x = 132


 f =f + 1
 if f = 10 then f = 0 
/*
  player1:
        %01101100
        %01001000
        %01001000
        %01111000
        %01111000
        %01111010
        %11111110
        %11111000
        %10111000
        %10011110
        %10011000
        %00011111
        %00011111
        %00011011
        %00001110
end
*/

  if f = 0 then player1:
        %01100000
        %01001100
        %01001000
        %01111000
        %01111000
        %01111010
        %11111110
        %11111000
        %10111000
        %10011110
        %10011000
        %00011111
        %00011111
        %00011011
        %00001110
end
 if f = 5 then player1:
        %00001100
        %01101000
        %01001000
        %01111000
        %01111000
        %01111010
        %11111110
        %11111000
        %10111000
        %10011110
        %10011000
        %00011111
        %00011111
        %00011011
        %00001110
end





   ;***************************************************************
   ;
   ;  Sets color of player0 sprite.
   ;
   
   ;noturno
   ;COLUP0 = $0A
   ;COLUP1 = $0A

   ;dia
   ;COLUP0 = $02
   ;COLUP1 = $02

   COLUP0 = cor_boneco
   COLUP1 = cor_boneco

   ;***************************************************************
   ;
   ;  Sets playfield color.
   ;
   ;noite
   ;COLUPF = $0A
   ;dia
   ;COLUPF = $02

   COLUPF = cor_boneco 


   ;***************************************************************
   ;
   ;  Sets background color.
   ;
   ;noite
   ;COLUBK = $00
   ;dia
   ;COLUBK = $0E
   
   COLUBK = cor_fundo

  

   if modo_demo = 0 && collision(player0,player1) then goto _prepara_fim_do_jogo
   if sprite >= 6 then gosub _passaro : NUSIZ0 = $35


   ;até 500 pontos acelera em 0.2
   if player0x <=14 && hora_de_acelerar = 1 && _sc1 = $00 && _sc2 < $05 then aceleracao = aceleracao + 0.2 : hora_de_acelerar = 0 : goto _pula_acelera
   ;depois dos 500 acelera em 0.1
   if player0x <=14 && hora_de_acelerar = 1 then aceleracao = aceleracao + 0.1 : hora_de_acelerar = 0
_pula_acelera

   if player0x <=14 then gosub _troca_sprite : player0x = 140
   _P0_L_R = _P0_L_R - aceleracao

   ;sujeira
   ;missel_x = missel_x - aceleracao
   ;if missile0x <=14 then missile0x = 140

  if modo_demo = 1 && player1x = 100 then p = 1

  ; PULO
  if modo_demo = 0 && joy0fire && p = 0 && player1y = 78 then p = 1 
  if p = 1 && player1y > 56 then player1y = player1y - 2
 
  if p = 1 && player1y > 66 then AUDF0 = 30 : AUDV0 = 5 : AUDC0 = 4
  if p = 1 && player1y = 66 then AUDV0 = 0
 
  if player1y = 56 && segura_no_ar < 10 && p=1 then segura_no_ar = segura_no_ar + 1 : AUDV0 = 0
  if player1y = 56 && segura_no_ar = 10 then p=2
  if p = 2 && player1y < 78 then player1y = player1y + 2
  if player1y = 78 && p =2 then p = 0 : segura_no_ar = 0

 if modo_demo = 1 then goto _pula_score

   ; incrementa score
   if incrementa_score < 100 && conta_ate6 = 0 then score = score + 1 : incrementa_score = incrementa_score + 1
   
   if incrementa_score = 100 then som_pegou_item = 5
     
   if incrementa_score >= 100 && conta_ate10 = 0 then scorecolor = $0A : incrementa_score = incrementa_score + 1
   if incrementa_score >= 100 && conta_ate10 = 5 then scorecolor = $02
   if incrementa_score = 110 then incrementa_score = 0 : scorecolor = $02 : hora_de_acelerar = 1

   if som_pegou_item = 5 then gosub Get_Music_or_Sound

_pula_score

   conta_ate10 = conta_ate10 + 1
   if conta_ate10 = 10 then conta_ate10 = 0

   conta_ate6 = conta_ate6 + 1
   if conta_ate6 = 6 then conta_ate6 = 0


 if p <> 0 then player1:
        %01101100
        %01001000
        %01001000
        %01111000
        %01111000
        %01111010
        %11111110
        %11111000
        %10111000
        %10011110
        %10011000
        %00011111
        %00011111
        %00011011
        %00001110
end


   ;***************************************************************
   ;
   ;  Displays the screen.
   ;
   drawscreen

  ;gosub __Snowbank


   ;***************************************************************
   ;
   ;  Reset switch check and end of main loop.
   ;
   ;  Any Atari 2600 program should restart when the reset  
   ;  switch is pressed. It is part of the usual standards
   ;  and procedures.
   ;
   ;```````````````````````````````````````````````````````````````
   ;  Turns off reset restrainer bit and jumps to beginning of
   ;  main loop if the reset switch is not pressed.
   ;
   if !switchreset then _Bit0_Reset_Restrainer{0} = 0 : goto __Main_Loop

   ;```````````````````````````````````````````````````````````````
   ;  Jumps to beginning of main loop if the reset switch hasn't
   ;  been released after being pressed.
   ;
   if _Bit0_Reset_Restrainer{0} then goto __Main_Loop

   ;```````````````````````````````````````````````````````````````
   ;  Restarts the program.
   ;
   goto _Reset

_prepara_fim_do_jogo

 f = 10
 AUDV0 = 0
 AUDV1 = 0

 ;esconde o ufo caso esteja em tela
 missile0y = 0 : missile1y = 0

 ;send score to PlusROM HSC
 WriteToBuffer = _sc1
 WriteToBuffer = _sc2
 WriteToBuffer = _sc3
 WriteSendBuffer = HighScoreDB_ID

_fim_do_jogo

   ;scorecolor = scorecolor + 1

   if sprite >= 6 then NUSIZ0 = $35

  player1:
        %01101100
        %01001000
        %01001000
        %01111000
        %01111000
        %01111010
        %11111110
        %11111000
        %10111110
        %10011000
        %10011111
        %00011011
        %00010001
        %00011011
        %00001110
end

 ;player0y = 0
 AUDV0 = 0
 
 if f <= 29 then f=f+1

 if f<29 then AUDF0 = f : AUDV0 = 5 : AUDC0 = 1

 COLUP0 = 2
 COLUP1 = 2

 ;segura alguns ciclos antes de poder apertar o botão de continue
 if f >= 29 && joy0fire then AUDV0 = 0 : f=0 : goto __Start_Restart

 ;ceu continua estrelado no game over a noite
 if _bit_escuro_claro{2} then gosub _ceu_estrelado else bally = 0
   conta_ate6 = conta_ate6 + 1
   if conta_ate6 = 6 then conta_ate6 = 0

 drawscreen

   ;***************************************************************
   ;
   ;  Reset switch check and end of main loop.
   ;
   ;  Any Atari 2600 program should restart when the reset  
   ;  switch is pressed. It is part of the usual standards
   ;  and procedures.
   ;
   ;```````````````````````````````````````````````````````````````
   ;  Turns off reset restrainer bit and jumps to beginning of
   ;  main loop if the reset switch is not pressed.
   ;
   if !switchreset then _Bit0_Reset_Restrainer{0} = 0 : goto _fim_do_jogo

   ;```````````````````````````````````````````````````````````````
   ;  Jumps to beginning of main loop if the reset switch hasn't
   ;  been released after being pressed.
   ;
   if _Bit0_Reset_Restrainer{0} then goto _fim_do_jogo

   ;```````````````````````````````````````````````````````````````
   ;  Restarts the program.
   ;
   goto _Reset
 
Get_Music_or_Sound

   
   ; com 255 no Duration sai do loop pois ja tocou todas as notas
   if Duration = 255 then return

   ;Check for end of current note
   Duration = Duration - 1
   if Duration>0 then return



   rem  '  Retrieve channel 0 data.
   temp4 = sread(musicData)
   temp5 = sread(musicData)
   temp6 = sread(musicData)

   rem  '  Check for end of data.
   ;Pegou o valor 255 na sdata, coloco 255 no Duration para não decrementar mais la em cima.
   ;som_pegou_item=5 quer dizer o som da moeda quando o et pega o cartucho no buraco. esse será reiniciado a cada ver que tocar.
   if temp4=255 && som_pegou_item = 5 then gosub Init_gotthecoin : AUDV0 = 0 : AUDV1 = 0 : som_pegou_item = 0 : return
   if temp4=255 then Duration = 255 : AUDV0 = 0 : AUDV1 = 0 : som_pegou_item = 0 : return


   rem  '  Play channel 0.
   AUDV1 = temp4
   AUDC1 = temp5
   AUDF1 = temp6

   rem  '  Set Duration.
   Duration = sread(musicData)
   
   return

Init_gotthecoin
    sdata musicData = SoundDataLoc
    $f, $c, $04
    2
    $e, $4, $0a
    1
    $f, $4, $0a
    2
    $d, $4, $0a
    1
    $9, $4, $0a
    2
    $8, $4, $0a
    1
    $7, $4, $0a
    1
    $6, $4, $0a
    1
    $5, $4, $0a
    1
    $3, $4, $0a
    1
    $7, $4, $0a
    1
    $9, $4, $0a
    1
    $3, $4, $0a
    1
    $1, $4, $0a
    3
    $0, $4, $0a
    1
    $1, $4, $0a
    3
    255
end
   Duration = 1
   return

_troca_sprite

  player0y = 78

 sprite = (rand&7)

 if _sc1 = $00 && _sc2 < $03 && sprite = 6 then sprite = 7

 NUSIZ0 = $30


 if sprite <= 2 then player0:
        %00011000
        %00011000
        %00011000
        %00011000
        %00011000
        %01111110
        %01011010
        %01011010
        %01011010
        %01011010
        %01011010
        %01011010
        %00011010
        %00011000
        %00011000
end



  if sprite = 3 then player0:
        %00100000
        %00100000
        %11111000
        %10101000
        %10101000
        %00101000
        %00100000
end

  if sprite = 4 then player0:
        %01000100
        %01000100
        %11011111
        %10010101
        %10010101
        %10010101
        %10000101
        %00000001
end
  if sprite = 5 then player0:
        %01000100
        %01000100
        %01100111
        %01110101
        %01010101
        %01010101
        %01010101
        %01000001
end
 if sprite >= 6 then gosub _passaro
 return

_passaro
  
  if sprite = 7 then player0y = 63

  if f <= 5 then player0:
        %00000000
        %00000000
        %00000000
        %00000010
        %00011100
        %00111111
        %00111110
        %11011100
        %01011000
        %00011000
        %00010000
end
  if f > 5 then player0:
        %00010000
        %00010000
        %00011000
        %00011010
        %00011100
        %00111111
        %00111110
        %11000000
        %01000000
        %00000000
        %00000000
end
 return


_escurecer

   if !_bit_escuro_claro{2} && cor_boneco <> $0E && conta_ate10 = 0 then cor_boneco = cor_boneco + 1 : cor_fundo = cor_fundo - 1 
   
   if _bit_escuro_claro{2} && cor_boneco <> $00 && conta_ate10 = 0 then cor_boneco = cor_boneco - 1 : cor_fundo = cor_fundo + 1

   if cor_boneco = $0E then _bit_escuro_claro{2} = 1
   if cor_boneco = $00 then _bit_escuro_claro{2} = 0


;   cor_boneco = $02
   ;cor_fundo = $0E

;   cor_boneco = $0A
   ;cor_fundo = $00

    return

_ceu_estrelado

  if conta_ate6 = 0 then ballx = 30  : bally = 30
  if conta_ate6 = 2 then ballx = 70  : bally = 20
  if conta_ate6 = 4 then ballx = 110 : bally = 30
  if conta_ate6 = 5 then ballx = 130 : bally = 20

  if ufo = 1 then  missile0y = 10 : missile1y = 10 : missile0x = missile0x - 2 : missile1x = missile1x - 2
 
 if missile0x <= 20 then ufo = 0 : missile0y = 0 : missile1y = 0


 NUSIZ1 = $20

 return

 ; don't let your program flow run into this code
   asm
   ; HSC PlusROM API definition.
   SET_PLUSROM_API "a", "h.firmaplus.de"
end


/*

__Snowbank

  frame=frame+1
  ballheight = 255

  ENAM0=2 : rem ** set ENAM0=0 when you want the stars to be gone

  vblank
  rem ** Enable the TIA bug that causes missile0 to be repeated
  asm
 sta HMCLR
 sta WSYNC
 lda #$ff
 sta HMM0
 lda #$c8
 sta WSYNC
 sta HMOVE
 sleep 5
 sta HMM0
end
  return

*/