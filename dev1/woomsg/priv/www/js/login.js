$(document).ready(function() {

  $('div#login-button-done>input').hover(
    function() {
      $(this).attr("src", "/static/image/button-login-hover-done.gif");
    },
    function() {
      $(this).attr("src", "/static/image/button-login-done.gif");
    }
  );

})

/**
 * 用户登录
 */
function do_login() {
  $('#login-button-done').hide();
  $('#login-ajax').show();
  $.post("login/ajax", $('#login_form').serialize(), validate_do_login, "json");
}


function validate_do_login(data) {
  if(data.result == "error") {
    $('#login-button-done').show();
    $('#login-ajax').hide()
    clear_login_form();
    $.achtung({message:data.content,
	       timeout:3,
	       className:'achtungFail'});
  }
  else if(data.result == "ok") {
    location.href = "user/" + data.content;
  }
}

function clear_login_form() {
  $('input.reg-input').val('');
}
