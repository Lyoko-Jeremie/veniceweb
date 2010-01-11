/**
 * add comment
 */
function add_comment() {
  $.post("/comment/add/ajax", {"pic-guid":"",
			       "pic-comment": ""}, validate_add_comment, "json");
}


function validate_add_comment(data) {
  if(data.result == "error") {
    $.achtung({message:data.content,
	       timeout:3,
	       className:'achtungFail'});
  }
  else if(data.result == "ok") {
    $.achtung({message:"添加关注成功: " + data.content,
	       timeout:3,
	       className:'achtungSuccess'});
  }
}

/**
 * remove comment
 */
function remove_comment() {
  $.post("/comment/remove/ajax", {"comment-guid":""}, validate_remove_comment, "json");
}


function validate_remove_comment(data) {
  if(data.result == "error") {
    $.achtung({message:data.content,
	       timeout:3,
	       className:'achtungFail'});
  }
  else if(data.result == "ok") {
    $.achtung({message:"取消关注成功:" + data.content,
	       timeout:3,
	       className:'achtungSuccess'});
  }
}