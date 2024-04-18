<?php
/*
 * Make a move.
 * 
 * response:
 *  the data for a table
 * 
 */


 if (array_key_exists('table', $game_status) && !empty($game_status['table']) ){
    $move_id = $Request->data[0];
    $moveLookup = ["FO","CH","BB","BL","BH","CA","RA"];
    $json = file_get_contents($game_status['table']['url']['base'].'/move/'.$moveLookup[$move_id].'?'.$game_status['table']['url']['param_table'].'&'.$game_status['table']['url']['param_player']);

    // Decode the JSON file 
    $game_status['table']['fuji_data'] = json_decode($json,true);
    if($game_status['table']['fuji_data']['activePlayer'] === -1){
        $game_status['table']['fuji_data']['activePlayer'] = 8;
    }
    $game_status['table']['plusrom_data'] = parse_fujinet_2_plusrom($game_status['table']['fuji_data']);

    // debug log !!
    file_put_contents ("../extras/clog/5cspoker.log", "send_move:".print_r($game_status,true), FILE_APPEND);

    foreach($game_status['table']['plusrom_data'] AS $key => $byte){
        $Response->add_byte($byte);
    }

}else{ //
    $Response->add_byte(1);
}

  