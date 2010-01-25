
/**
 * 添加关注
 */
function add_following(fusername) {
  $.post("/following/add/ajax", {"username":fusername}, validate_add_following, "json");
}


function validate_add_following(data) {
  if(data.result == "error") {
    show_error(data.content);
  }
  else if(data.result == "ok") {
    $('div#profile-username>a').text("取消关注:" + data.content);
    $('div#profile-username>a').removeAttr("onclick");
    $('div#profile-username>a').unbind("click");
    $('div#profile-username>a').bind("click", function() {remove_following(data.content); return false;});
    show_message("添加关注成功" + data.content);
  }
}

/**
 * 删除关注
 */
function remove_following(fusername) {
  $.post("/following/remove/ajax", {"username":fusername}, validate_remove_following, "json");
}


function validate_remove_following(data) {
  if(data.result == "error") {
    show_error(data.content);
  }
  else if(data.result == "ok") {
    $('div#profile-username>a').text("关注:" + data.content);
    $('div#profile-username>a').removeAttr("onclick");
    $('div#profile-username>a').unbind("click");
    $('div#profile-username>a').bind("click", function() {add_following(data.content); return false;});
    show_message("取消关注成功" + data.content);
  }
}


/**
 * 删除照片
 */
function remove_pic(guid) {
  $.post("/pic/remove/ajax", {"pic-guid":guid}, validate_remove_pic, "json");
}


function validate_remove_pic(data) {
  if(data.result == "error") {
    show_error(data.content);
  }
  else if(data.result == "ok") {
    show_message("删除照片成功");
    removediv = '#' + data.content;
    $(removediv).remove();
  }
}
