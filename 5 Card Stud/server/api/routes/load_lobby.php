<?php
/*
 * Load Lobby Data
 * 
 * responses:
 *  120 bytes of textpage
 * 
 */
// Read the JSON file  
$json = file_get_contents('http://fujinet.online:8080/view?platform=atari'); 
  
// Decode the JSON file 
$lobby_data = json_decode($json,true); 
  
// Display data 
$count=0;
$game_status['lobby'] = [];
//if(!array_key_exists('lobby', $game_status)){
//}
foreach($lobby_data AS $server){
    if($count < 5 && $server['g'] == "5 Card Stud" && $server['o'] = 1 && $server['p'] < $server['m']){
        // Server is 5cs and online and a seat is available.
        $game_status['lobby'][$count] = $server;
        $display_name = str_replace(["- "," bots"],["","bots"], $server['s']);

//        echo str_pad(substr($server['s'], 0, 20), 20)." ".$server['p']."/".$server['m']."\n";
        $append_srv = str_pad(substr($display_name, 0, 20), 20)." ".$server['p']."/".$server['m'];

        for ($i = 0; $i < 24; $i++){
            $Response->add_byte( getCharId($append_srv[$i]) );
        }
        $count++;
    }
}

while($count++ < 5 ){ // wipe out rest of table!
    for ($i = 0; $i < 24; $i++) {
        $Response->add_byte( 4 ); // getCharId(' ')
    }
}


function getCharId($c){
    $o = ord($c);
    if($o > 64 && $o < 91 ){
        return ($o - 63) * 4;
    }
    if($o > 96 && $o < 123 ){
        return ($o - 69 ) * 4;
    }
    if($o > 47 && $o < 58 ){
        return ($o + 6 ) * 4;
    }
    $map = [32 => 4, 46 => 1, 45 => 0];
    if(array_key_exists($o, $map ) ){
        return $map[$o];
    }
    return 4;
}
