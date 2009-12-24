%% woomsg schema struct

-record(user, {
          username,
          password,
	  email,
	  photo,
	  create_data
        }).

-record(user_ext, {
          username,
	  fullname,
	  sex,
	  location,
	  web,
	  describe
	}).

-record(following, {
	  username,
	  usernamelist
        }).

-record(pic, {
	  guid,
	  owner,
	  path,
	  msg,
	  count,
	  taglist,
	  spam,
	  create_data
        }).

-record(pic_comment, {
	  guid,
	  owner,
	  comment,
	  create_data
        }).

-record(pic_tag, {
	   tag,
	   guid
	}).

-record(session, {
           username,
	   session_id
       }).
