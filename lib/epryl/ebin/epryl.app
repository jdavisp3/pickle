{application, epryl,
 [{description, "A library for connecting Erlang and Python"},
  {vsn, "0.1.3"},
  {modules, [epryl_pickle, epryl_amp, epryl_amp_server]},
  {registered, []},
  {applications, [kernel, stdlib, gen_socket]},
  {start_phases, []}
 ]
}.
