-module(frag_db).

-export([set/3, dec/3, inc/3]).    %% set/increase/decrease field value
-export([get/3]).                  %% retrive field value
-export([find/1, find/2, find/3]). %% retrive rows
-export([delete/1, delete/2]).     %% delete rows
