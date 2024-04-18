<?php
class PlusROMrequest {
    protected array $data;
    protected string $payload;
    protected int $api_route;
    protected int $data_length;
    protected string $plusrom_header_raw;
    protected array $device;

    function __construct() {
        $this->parse_request_payload();
        $this->api_route = (int)array_pop($this->data);

        if(array_key_exists('HTTP_PLUSROM_INFO', $_SERVER)){
            $this->plusrom_header_raw = $_SERVER['HTTP_PLUSROM_INFO'];
            $this->parse_plusrom_header();
        }else{
            $this->plusrom_header_raw = $_SERVER['HTTP_PLUSSTORE_ID'];
            $plusrom_header_raw_parts  = explode(" ", $this->plusrom_header_raw);
            $this->device["id"] = array_pop( $plusrom_header_raw_parts );
            if( substr( $this->device["id"], 0, 2 ) == "WE" || strlen($this->device["id"]) != 24 || !ctype_xdigit($this->device["id"]) ){
                $this->device["agent"] = "Emulator";
                $this->device["nick"] = implode( " ", $plusrom_header_raw_parts );
                if($this->device["nick"] == "v0.6.0"){
                    $this->device["nick"] = "unknown";
                }
            }else{
                $this->device["agent"] = "PlusCart";
                $this->device["nick"] = "unknown";
            }
        }

        
        $response_obj = false;
        if($this->device["agent"] == "PlusCart" ){
            $response_obj = $this->open_api_request();
        }

        if($response_obj && $response_obj->user_id !== false){
            $this->device["nick"] = $response_obj->user_id;
        }else{
            $this->generate_unique_user_name($response_obj);
        }
    }

    public function &__get($var){
        if(isset($this->$var))
            return $this->$var;
        else
            return false;
    }

    private function open_api_request(){
        $options = array(
            'http' => array(
                'header'  => "Authorization: Bearer XXX_YOUR_PLUSSTORE_OPENAPI_TOKEN_HERE_XXXXX\r\n",
                'method'  => 'GET'
            )
        );
        $context = stream_context_create($options);
        $result  = file_get_contents("https://plusstore.firmaplus.de/openAPI.php?device_id=".$this->device["id"], false, $context);

        return json_decode($result);
    }

    private function generate_unique_user_name($openAPI_resp){
        if($openAPI_resp !== false){
            $user_id = $openAPI_resp['plusstore_id'];
        }else{
            $user_id = "";
        }
        $this->device["nick"] .= " ".$this->device["agent"]." user".$user_id;
    }

    private function parse_request_payload(){
        $post_data = file_get_contents("php://input");
        $post_data_len = strlen($post_data);
        $this->data = [];
        for ( $pos=0; $pos < $post_data_len; $pos ++ ) {
            $this->data[] = ord(substr($post_data, $pos));
        }
        if ( $post_data_len > 3 ){
            $this->payload = substr($post_data, 0, -3);
        }
        $this->data_length = count($this->data);
    }


    private function parse_plusrom_header(){
        // Explode the plusrom header string using a series of semicolons
        $pieces = array_filter(array_map('trim', explode(';', $this->plusrom_header_raw )));

        if (empty($pieces) ) {
            return false;
        }

        // Add the plusrom header pieces into the parsed data array
        foreach ($pieces as $part) {
            $plusrom_header_parts = explode('=', $part, 2);
            $key = trim($plusrom_header_parts[0]);

            if (count($plusrom_header_parts) == 2) {
                // Be sure to strip wrapping quotes
                $this->device[$key] = trim($plusrom_header_parts[1], " \n\r\t\0\x0B\"");
            }
        }
        if(empty($this->device["nick"]) ){
            $this->device["nick"] = "unknown";
        }
    }
}