/*
 * add comment
 */
function add_comment(picguid) {
  piccomment = $('.leave-message').val();
  if(check_empty(piccomment)) {
    show_error("评论不能为空");
  }
  else {
    $.post("/comment/add/ajax", {"pic-guid":picguid,
			         "pic-comment":piccomment}, validate_add_comment, "json");
  }
}


function validate_add_comment(data) {
  if(data.result == "error") {
    $('.leave-message').val('');
    show_error(data.content);
  }
  else if(data.result == "ok") {
    $('.leave-message').val('');
    show_message("添加评论成功");
    $('#photo-comments-form').after(new_comment_div(data.content.guid,
						    data.content.username,
                                                    data.content.comment,
                                                    data.content.photosrc,
						    data.content.createdate))
  }
}

function new_comment_div(guid, username, comment, photosrc, createdate) {
  return "<div id='" + guid + "' class='photo-comment'>" +
           "<div class='photo-comment-avatar'>" +
             "<img class='avatar' height=48 width=48 src='" + photosrc + "'/>" +
           "</div>" +
	 "<div class='photo-comment-body'>" +
           "<div class='photo-comment-info'>" +
             "<a class='nav' href='/user/"+ username  +"'>" + username + "</a>" +
	     "<span class='photo-comment-date'>" + createdate  +"</span>" +
           "</div>" +
	   "<div style='float: right;'>" +
             "<form onsubmit='remove_comment(\"" + guid + "\");return false' action=''>" +
		"<input type='image' src='/static/image/icon_trash.gif'/>" +
             "</form>" +
           "</div>" +
           "<div class='photo-comment-message'>" + comment + "</div>" + 
         "</div>" +  
         "<div style='clear:both;'></div>" +
         "</div>";
}

/**
 * remove comment
 */
function remove_comment(guid) {
  $.post("/comment/remove/ajax", {"comment-guid":guid}, validate_remove_comment, "json");
}


function validate_remove_comment(data) {
  if(data.result == "error") {
    show_error(data.content);
  }
  else if(data.result == "ok") {
    show_message("删除评论成功");
    removediv = '#' + data.content;
    $(removediv).remove();
  }
}
