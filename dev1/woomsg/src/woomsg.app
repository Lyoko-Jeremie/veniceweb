{application, woomsg,
 [{description, "woomsg"},
  {vsn, "0.01"},
  {modules, [
    woomsg,
    woomsg_app,
    woomsg_sup,
    woomsg_web,
    woomsg_deps
  ]},
  {registered, []},
  {mod, {woomsg_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
