<?php
/*
 * Join a table.
 * 
 * response:
 *  the data for a table
 * 
 */

if (array_key_exists('lobby', $game_status) && !empty($game_status['lobby']) ){
    $id = 6 - $Request->data[0] ; // 6 = 0, 5 = 1, 4=2, 3=3, 2 = 4
    $table_url = $game_status['lobby'][$id]['u'];
    $url_parts = explode("?", $table_url );
    $game_status['table'] = [];
    $game_status['table']['id'] = $id;
    $game_status['table']['url'] = [];
    $game_status['table']['url']['base'] = $url_parts[0];
    $game_status['table']['url']['param_table'] = $url_parts[1];
    $game_status['table']['url']['param_player'] = "player=".urlencode($Request->device["nick"]);
    $game_status['table']['url']['param_mock_table'] = "table=PlusCart123";
    $game_status['table']['url']['param_mock_player'] = "player=Al_Nafuur&count=8"; // get from PlusStore openAPI like HSC does;
    $json = file_get_contents($game_status['table']['url']['base'].'state?'.$game_status['table']['url']['param_table'].'&'.$game_status['table']['url']['param_player']); 
  
    // Decode the JSON file 
    $game_status['table']['fuji_data'] = json_decode($json,true);
    if($game_status['table']['fuji_data']['activePlayer'] === -1){
        $game_status['table']['fuji_data']['activePlayer'] = 8;
    }
    $game_status['table']['plusrom_data'] = parse_fujinet_2_plusrom($game_status['table']['fuji_data']);

    // debug log !!
    file_put_contents ("../extras/clog/5cspoker.log", "join_table:".print_r($game_status,true), FILE_APPEND);

    foreach($game_status['table']['plusrom_data'] AS $key => $byte){
        $Response->add_byte($byte);
    }

}else{ // Unknown/Invalid table.
    // toDo send "empty" table with no valid moves
    $Response->add_byte(3);
}
