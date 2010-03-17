{application, tiny_app,
  [{description, "tiny app 2.0"},
   {vsn, "2.0"},
   {modules, [tiny, tiny_server]},
   {registered, [tiny_sup, tiny_server]},
   {applications, [kernel, stdlib, sasl]},
   {mod, {tiny, []}}]
}.