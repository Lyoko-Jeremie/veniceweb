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
	  username1,
	  username2
        }).

-record(pic, {
	  guid,
	  owner,
	  path,
	  msg,
	  count,
	  spam,
	  create_data
        }).

-record(pic_comment, {
	  guid,
	  owner,
	  comment,
	  create_data
        }).
