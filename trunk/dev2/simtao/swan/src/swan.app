{application, swan,
 [{description, "swan"},
  {vsn, "0.01"},
  {modules, [
    swan,
    swan_app,
    swan_sup,
    swan_web,
    swan_deps
  ]},
  {registered, []},
  {mod, {swan_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.