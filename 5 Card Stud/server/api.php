<?php
/*
* 5 Card Stud PlusROM backend for FujiNet
* Response status codes:
*  0: OK
*  1: Unknow function request
*  2: Too few parameters
*/

if ($_SERVER['REQUEST_METHOD'] === 'POST' && ( array_key_exists('HTTP_PLUSSTORE_ID', $_SERVER) || array_key_exists('HTTP_PLUSROM_INFO', $_SERVER) )) {
    include("./api/include.php");

    $Request = new PlusROMrequest();
    $Response = new PlusROMresponse();

    session_id($Request->device["id"]);
    session_start(["use_cookies" => false, "use_only_cookies" => false, "use_trans_sid" => false]);

    $game_status = $_SESSION;
    $save_session = true;

    logging($Request );

    if($Request->data_length > 0 ){
        if(array_key_exists($Request->api_route, $api_routes) ){
            include("./api/routes/".$api_routes[$Request->api_route].".php");
        } else { // Unknow function request
            $Response->add_byte(1);
        }
    } else { // Too few parameters
        $Response->add_byte(2);
    }

    $Response->send( );

    if($save_session)
        $_SESSION = $game_status; // todo caching time?

}else if ($_SERVER['REQUEST_METHOD'] === 'OPTIONS') {
        header('Access-Control-Allow-Origin: '.$_SERVER['HTTP_ORIGIN']);
        header('Access-Control-Allow-Headers: PlusRom-Info,PlusStore-ID,Content-Type');
}else{
    echo "Wrong Request Method!\r\n";
}
?>