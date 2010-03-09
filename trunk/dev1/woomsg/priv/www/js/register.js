$(document).ready(function() {

  $('div#reg-button-done>input').hover(
    function() {
      $(this).attr("src", "/static/image/button-reg-hover-done.gif");
    },
    function() {
      $(this).attr("src", "/static/image/button-reg-done.gif");
    }
  );

})

/**
 * 用户注册
 */
function register_user() {
  $('#reg-button-done').hide();
  $('#register-ajax').show();
  $.post("/register/ajax", $('#register_form').serialize(), validate_register_user, "json");
}

function validate_register_user(data) {
  if(data.result == "error") {
    $('#reg-button-done').show();
    $('#register-ajax').hide();
    show_error(data.content);
    clear_register_form();
  }
  /**
   * 登录成功, 跳转到login页面
   */
  else if (data.result == "ok") {
    location.href = "/login";
  }
  
}

function clear_register_form() {
  $('input.reg-input').val('');
}
