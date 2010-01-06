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

