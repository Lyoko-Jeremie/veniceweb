
/**
 * add following
 */
function add_following(fusername) {
  $.post("/following/add/ajax", {"username":fusername}, validate_add_following, "json");
}


function validate_add_following(data) {
  if(data.result == "error") {
    $.achtung({message:data.content,
	       timeout:3,
	       className:'achtungFail'});
  }
  else if(data.result == "ok") {
    $('div#profile-username>a').text("取消关注:" + data.content);
    $('div#profile-username>a').removeAttr("onclick");
    $('div#profile-username>a').unbind("click");
    $('div#profile-username>a').bind("click", function() {remove_following(data.content); return false;});
    $.achtung({message:"添加关注成功: " + data.content,
	       timeout:3,
	       className:'achtungSuccess'});
  }
}

/**
 * remove following
 */
function remove_following(fusername) {
  $.post("/following/remove/ajax", {"username":fusername}, validate_remove_following, "json");
}


function validate_remove_following(data) {
  if(data.result == "error") {
    $.achtung({message:data.content,
	       timeout:3,
	       className:'achtungFail'});
  }
  else if(data.result == "ok") {
    $('div#profile-username>a').text("关注:" + data.content);
    $('div#profile-username>a').removeAttr("onclick");
    $('div#profile-username>a').unbind("click");
    $('div#profile-username>a').bind("click", function() {add_following(data.content); return false;});
    $.achtung({message:"取消关注成功:" + data.content,
	       timeout:3,
	       className:'achtungSuccess'});
  }
}