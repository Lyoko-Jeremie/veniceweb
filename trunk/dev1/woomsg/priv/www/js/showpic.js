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
    var removediv = '#' + data.content;
    $(removediv).remove();
  }
}

/*
 * add tags
 */
function add_tags(picguid) {
  var pictags = $('#pic-tags-id').val();
  if(check_empty(pictags)) {
    show_error("标签不能为空");
  }
  else {
    $.post("/tag/add/ajax", {"pic-guid":picguid,
			     "pic-tags":pictags}, validate_add_tags, "json");
  }
}

function validate_add_tags(data) {
  if(data.result == "error") {
    $('#pic-tags-id').val('');
    show_error(data.content);
  }
  else if(data.result == "ok") {
    $('#pic-tags-id').val('');
    show_message("添加标签成功");
    $('#scope-tags').prepend(new_tags_scope(data.content.guid,
					    data.content.count,
                                            data.content.tags))
  }
}

function new_tags_scope(guid, count, tags) {
  var res = "";
  for (var index = 0; index < count; index++ ) {
    var tmpres = "<span id='scope1-" + tags[index] +"' style='font-size:10px; color:#000000;'>+</span>" +
	         "<a id='scope2-" + tags[index] + "' class='nav' style='font-size:12px; text-decoration:none;' href='/tag/" + tags[index] + "'>" + tags[index] +"</a>" +
	         "<span id='scope3-" + tags[index] + "' style='color:#4e4e4e; font-size:10px;'>" +
	         "<form onsubmit='remove_tag(\"" + guid + "\", \"" + tags[index] + "\"); return false;' action='' style='display:inline;'>" +
	           "<input type='image' src='/static/image/icon_trash.gif'/>" +
	         "</form>" +
	         "</span>";
    res += tmpres;
  }
  return res;
}

/*
 * remove tag
 */
function remove_tag(picguid, pictag) {
  $.post("/tag/remove/ajax", {"pic-guid":picguid,
			      "pic-tag":pictag}, validate_remove_tag, "json");
}

function validate_remove_tag(data) {
  if(data.result == "error") {
    show_error(data.content);
  }
  else if(data.result == "ok") {
    show_message("删除标签成功");
    var removescope1 = '#' + "scope1-" + data.content;
    var removescope2 = '#' + "scope2-" + data.content;
    var removescope3 = '#' + "scope3-" + data.content;
    $(removescope1).remove();
    $(removescope2).remove();
    $(removescope3).remove();
  }
}
