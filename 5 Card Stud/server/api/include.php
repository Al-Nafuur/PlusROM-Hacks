<?php
date_default_timezone_set('CET');
spl_autoload_register(function ($class_name){
    include './api/classes/'.$class_name . '.php';
});

$api_routes = [
 "load_lobby",       // 0
 "join_table",       // 1
 "leave_table",      // 2
 "load_state",       // 3
 "send_move"         // 4
];

function logging($Request){
    file_put_contents ("../extras/clog/5cspoker.log", date("Y-m-d H:i:s").": PS-ID: ".$Request->plusrom_header_raw." LENGTH: ".$Request->data_length." BYTES: ".implode ( ", " , $Request->data ).", ".$Request->api_route."\n", FILE_APPEND );
}

function parse_fujinet_2_plusrom($data){
    $card_ids = [];

    for($p=0; $p<8; $p++){
        $hand = array_key_exists($p, $data['players']) ? str_pad($data['players'][$p]['hand'], 10) : "          ";
        $cards = str_split($hand, 2);
        $card_ids[$p] = [];
        foreach($cards AS $card){
            $card_ids[$p][] = get_card_id($card);
        }
    }
    $raw_data = [];

    // add top 4 players hands (or 2 spaces)
    for($i=0; $i<5; $i++){
        for($p=0; $p<4;$p++){
            $raw_data[] = $card_ids[$p][$i];
        }
    }

    // add bottom 4 players hands (or 2 spaces)
    for($i=0; $i<5; $i++){
        for($p=4; $p<8;$p++){
            $raw_data[] = $card_ids[$p][$i];
        }
    }

    // add players purses (or 4 spaces)
    for($p=0; $p<8;$p++){
        if(array_key_exists($p, $data['players'])){
            $purse_hi = intval( $data['players'][$p]['purse'] / 100);
            $purse_lo = $data['players'][$p]['purse'] - ($purse_hi * 100);

            $raw_data[] = $purse_hi > 0 ? (54 + $purse_hi) : 53;
            $raw_data[] = $purse_lo + (($purse_lo < 10 && $purse_hi > 0 ) ? 154 : 54);
        }else{
            $raw_data[] = 53;
            $raw_data[] = 53;
        }
    }

    // add pot (2 bytes)
    $pot_hi = intval( $data['pot'] / 100);
    $pot_lo = $data['pot'] - ($pot_hi * 100);

    $raw_data[] = $pot_hi > 0 ? (54 + $pot_hi) : 53;
    $raw_data[] = $pot_lo + (($pot_lo < 10 && $pot_hi > 0 ) ? 154 : 54);

    // add Round (3 bits), activePlayer (4 bits), viewing (1 bit)
    $raw_data[] = $data['round'] | ($data['activePlayer'] << 3) | ($data['viewing'] << 7);

    // validMoves (array -> 1 byte )
/*
    "FO": "FOLD",
	"CH": "CHECK",
	"BB": "POST",
	"BL": "BET", // BET LOW (e.g. 5 of 5/10, or 2 of 2/5 first round)
	"BH": "BET", // BET HIGH (e.g. 10)
	"CA": "CALL",
	"RA": "RAISE",
*/
    if(is_array($data['validMoves'])){
        $validMovesFlags = 0b10000000;
        $moveLookup = ["FO","CH","BB","BL","BH","CA","RA"];
        foreach($data['validMoves'] AS $value){
            $key = array_search($value['move'], $moveLookup);
            if($key !== false){
                $validMovesFlags |= 1 << $key;
            }
        }
        $raw_data[] = $validMovesFlags;
    } else {
        $player = $data['activePlayer'];
        $max = count($data['players']);
        if($player < $max ){
            $moveLookup = ["FOLD","CHECK","POST","BET","BET2","CALL","RAISE"]; // todo check if there is more!
            $sp = $player;
            do{
                $key = array_search($data['players'][$sp]['move'], $moveLookup);
                $sp--;
                if($sp == -1)
                    $sp = $max - 1;
            } while ($key === false && $sp != $player);
            if($key !== false){
                $raw_data[] = 164 + $key;
            }else{
                $raw_data[] = 53;
            }
        }else{
            $raw_data[] = 53;
        }
    }
    return $raw_data;
}

function get_card_id($card_str){
    if($card_str == "  "){
        return 53;
    }
    if($card_str == "??"){
        return 26;
    }
    $im = "AKQJT98765432";
    $de = "CS-------------------------HD";
    $id = strpos($im, $card_str[0]) * 2 + strpos($de, $card_str[1]);
    return $id;
}