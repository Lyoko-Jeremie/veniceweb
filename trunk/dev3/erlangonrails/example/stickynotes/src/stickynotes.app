{application, stickynotes,
 [{description, "stickynotes"},
  {vsn, "0.01"},
  {modules, [
    stickynotes,
    stickynotes_app,
    stickynotes_sup,
    stickynotes_web,
    stickynotes_deps
  ]},
  {registered, []},
  {mod, {stickynotes_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
