%% woomsg schema struct

-record(user, {
          username,
          password,
	  email,
	  photo_guid,
          photo_path,
          photo_type,
	  create_date
        }).

-record(user_ext, {
          username,
	  fullname,
	  sex,
	  location,
	  web,
	  describe,
          public
	}).

-record(following, {
	  username1,
	  username2
        }).

-record(pic, {
	  guid,
	  owner,
	  path,
          type,
	  msg,
	  count,
          dig,
	  taglist,
	  spam,
	  create_date
        }).

-record(pic_comment, {
	  guid,
          pic_guid,
	  owner,
	  comment,
	  create_date
        }).

-record(pic_tag, {
	   tag,
	   guid
	}).

-record(session, {
           username,
	   session_id,
	   create_date
       }).

-record(nfs_cache, {
           key,    
           path
       }).
