<?php
/*
 * Leave table request from client.
 * 
 * response:
 *  0: success
 *  1: failed
 */


if (array_key_exists('table', $game_status) && !empty($game_status['table']) ){
    $json = file_get_contents($game_status['table']['url']['base'].'leave?'.$game_status['table']['url']['param_table'].'&'.$game_status['table']['url']['param_player']); 
    $Response->add_byte(0);
}else{ //
    $Response->add_byte(1);
}
