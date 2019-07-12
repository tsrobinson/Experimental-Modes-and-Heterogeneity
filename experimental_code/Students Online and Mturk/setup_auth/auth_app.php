<?php # CESS Online SQL Connect
ini_set( 'display_errors', 1 );
// Set the database access information as constants:
DEFINE ('DB_USER', 'amatsuon_cessaki');
DEFINE ('DB_PASSWORD', 'c6KDMsUBC7Wv');
DEFINE ('DB_HOST', 'localhost');
#DEFINE ('DB_HOST', 'amatsuo.net');
DEFINE ('DB_NAME', 'amatsuon_CESSinteractive');

//$base_url = 'http://localhost:8080/taxCompliance/auth/';
$base_url = 'http://212.71.245.173:8080/CESS_TC/auth/';
if(isset($_GET['t'])){
    $base_url = 'http://localhost:8080/CESS_TC/auth/';    
}

// Make the connection:
$mysqli = new mysqli(DB_HOST, DB_USER, DB_PASSWORD, DB_NAME) OR die ('Could not connect to MySQL: ' . mysqli_connect_error() );


if (mysqli_connect_errno()) {
    printf("Connect failed: %s\n", mysqli_connect_error());
    exit();
}
// Set the encoding...
//mysqli_set_charset($dbc, 'utf8');

#$q = "UPDATE users SET demoq_complete = 1 WHERE user_name = '$un'";
function redirect($url, $statusCode = 303)
{
   header('Location: ' . $url, true, $statusCode);
   die();
}


if(isset($_GET['mid'])){
    
    $trimmed = array_map('trim', $_GET);
    $mid = $trimmed['mid'];
    $q1 = sprintf("select * from interactive_tax_auth where mid = '%s';", $mid);
    //echo($q1);
    $result = $mysqli->query($q1);
    $row_cnt = $result->num_rows;

    //printf("Result set has %d rows.\n", $row_cnt);

    /* close result set */
    
    if($row_cnt >0 ){
        $row = mysqli_fetch_row($result);
        $auth = $row[0];
        $mysqli->query(sprintf("update interactive_tax_auth set access_time = now() where auth_id = '%s';", $auth));
        
    } else {
        $q2 = sprintf("select * from interactive_tax_auth where mid is null limit 1;");
        $result = $mysqli->query($q2);
        $row = mysqli_fetch_row($result);
        $auth = $row[0];
        $mysqli->query(sprintf("update interactive_tax_auth set mid = '%s', access_time = now() where auth_id = '%s';", $mid, $auth));
    }
    //printf("The URL will be: %s%s", $base_url, $auth);
    redirect(sprintf("%s%s", $base_url, $auth));
} else {
    echo("please come back later");
}
/*    if($count >0){
        $q2 = "";
    }
    else {
        $q2 = "select auth_id from `interactive_tax_auth` where mid is NULL limit 1;";
        
    }
    $row = mysqli_fetch_assoc($r);
    $writer = new XMLWriter();
    $writer->openURI('php://output');  
    $writer->startDocument('1.0','UTF-8');  
    $writer->setIndent(4);
    $writer->startElement("grdata");
    foreach($row as $key => $value){
       $writer->writeElement($key,$value);  
    }
    $writer->writeElement("auditRate",10);
    $writer->writeElement("groupId",$groupid);
    $writer->endElement();
    $writer->endDocument(); 
    $writer->flush();
    mysqli_close($dbc);

}*/