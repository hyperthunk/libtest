{application, libtest,
 [
  {description, ""},
  {vsn, "1"},
  {modules, [
             'libtest_matchers'
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  %% {mod, { libtest, []}},
  {env, []}
 ]}.
