let 
  facadeIP = "167.172.133.184"; 
in 
{ publicKey = "cInHQG7ns2Hvq7HW6kqVGoRXvoZALNZ00pvjH1bPTmM=";
  allowedIPs = [ "10.100.0.1/32" ];
  endpoint = "${facadeIP}:51820";
  persistentKeepalive = 25;
}