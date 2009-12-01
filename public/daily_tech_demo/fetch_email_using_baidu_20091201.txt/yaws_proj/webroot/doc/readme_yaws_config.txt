
<1> 
logdir = "C:\webroot/log"


<2>
ebin_dir = "C:\webroot/ebin"

<3>
include_dir = "C:\webroot/include"

<4>
<server localhost>
        port = 8080
        listen = 0.0.0.0
        docroot = "C:\webroot"
        appmods = <cgi-bin, yaws_appmod_cgi>        
</server>



