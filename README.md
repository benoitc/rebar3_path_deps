rebar3_path_deps
=====

A rebar plugin to specify path dependencies. 

Over time, your project is growing significantly in size! It’s gotten to the point that 
you probably want to split out a separate OTP applications for others to use. 

This plugin add to rebar the  supports of path dependencies which are typically sub-applications 
that live within one repository. 

Build
-----

    $ rebar3 compile

Use
---

To use it add to your rebar.config dependencies the path : 

```erlang
{deps, [
  {mysubapp, {path, "mysubapp"}},
  ..
]

```

This tells Rebar that we depend on an application called mysubapp which is found in the mysubapp 
folder (relative to the `rebar.config` file it’s written in).


and then add the plugin to your rebar config:

    {plugins, [
        rebar3_path_deps
    ]}.

Then just compile your application

    $ rebar3 compile
    ===> Compiling rebar3_path_deps
    ===> Verifying dependencies...
    ===> Fetching mysubapp ({path,"./mysubapp",
                                        {mtime,<<"2018-10-17T11:21:18Z">>}})
    ===> Compiling hackney
    ===> Compiling myapp
