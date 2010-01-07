/**
 * 用户注册
 */
function register_user() {
  $('#register_ajax').show();
  $.post("register/ajax", $('#register_form').serialize(), validate_register_user, "json");
}

function validate_register_user(data) {
  $('#register_ajax').hide();
  if(data.result == "error") {
    $.achtung({message:data.content,
	       timeout:3,
	       className:'achtungFail'});
    clear_register_form();
  }
  
}

function clear_register_form() {
  $('input.reg-input').val('');
}
