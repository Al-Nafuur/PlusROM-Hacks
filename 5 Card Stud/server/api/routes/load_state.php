<?php
/*
 * State of a table.
 * 
 * response:
 *  the data for a table
 * 
 */
if (array_key_exists('table', $game_status) && !empty($game_status['table']) ){
    $json = file_get_contents($game_status['table']['url']['base'].'state?'.$game_status['table']['url']['param_table'].'&'.$game_status['table']['url']['param_player']); 
  
    // Decode the JSON file 
    $game_status['table']['fuji_data'] = json_decode($json,true);
    if($game_status['table']['fuji_data']['activePlayer'] === -1){
        $game_status['table']['fuji_data']['activePlayer'] = 8;
    }
    $game_status['table']['plusrom_data'] = parse_fujinet_2_plusrom($game_status['table']['fuji_data']);

    // debug log !!
    file_put_contents ("../extras/clog/5cspoker.log", "load_state:".print_r($game_status,true), FILE_APPEND);

    foreach($game_status['table']['plusrom_data'] AS $key => $byte){
        $Response->add_byte($byte);
    }

}else{ // Unknown/Invalid table.
    // toDo send "empty" table with no valid moves
    $Response->add_byte(3);
}