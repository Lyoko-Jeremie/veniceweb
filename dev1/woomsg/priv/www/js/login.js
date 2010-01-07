
/**
 * 用户登录
 */
function do_login() {
  $('#login_ajax').show();
  $.post("login/ajax", $('#login_form').serialize(), validate_do_login, "json");
}


function validate_do_login(data) {
  $('#login_ajax').hide();
  if(data.result == "error") {
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
