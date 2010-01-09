$(document).ready(function() {

  $('div#nav-login-button > input').hover(
    function() {
      $(this).attr("src", "/static/image/button-login-hover.gif");
    },
    function() {
      $(this).attr("src", "/static/image/button-login.gif");
    }
  );

  $('a#nav-reg-button>img').hover(
    function() {
      $(this).attr("src", "/static/image/button-reg-hover.gif");
    },
    function() {
      $(this).attr("src", "/static/image/button-reg.gif");
    }
  );

})

/*
 * 用户登录
 */
function header_login() {
  $('#nav-login-button').hide();
  $('#nav-login-ajax').show();
  $.post("/login/ajax", $('#header_login_form').serialize(), validate_header_login, "json");
}

function validate_header_login(data) {
  if(data.result == "error") {
    $('#nav-login-button').show();
    $('#nav-login-ajax').hide();
    $.achtung({message:data.content, 
               timeout:3,
	       className:'achtungFail'});
  } 
  /**
   * 登录成功, 直接跳转
   */
  else if (data.result == "ok") {
    location.href = "user/" + data.content;
  }
}

/*
 * Cookie Helpers
 */
function createCookie(name,value,days)
{
  if (days) {
    var date = new Date();
    date.setTime(date.getTime()+(days*24*60*60*1000));
    var expires = "; expires="+date.toGMTString();
  }
  else var expires = "";
  document.cookie = name+"="+value+expires+"; path=/";
}

function readCookie(name) {
  var nameEQ = name + "=";
  var ca = document.cookie.split(';');
  for(var i=0;i < ca.length;i++) {
    var c = ca[i];
    while (c.charAt(0)==' ') c = c.substring(1,c.length);
    if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length,c.length);
  }
  return null;
}

function eraseCookie(name) {
  createCookie(name,"",-1);
} 






