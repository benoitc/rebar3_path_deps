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

Let’s start off by making a new OTP application `hello_utils` inside of your  project `hello-world`:


    # inside of hello-world/
    $ rebar3 new app hello_utils

This will create a new folder hello_utils inside of which a `rebar.config` and `src` folder are ready to be useed. 
In order to tell rebar3 about this, open up `hello_world/rebar.config` and add hello_utils to your dependencies:

```erlang
{deps, [
  {hello_utils, {path, "hello_utils"}},
  ..
]

```

This tells Rebar that we depend on an application called `hello_utils` which is found in the hello_utils 
folder (relative to the `rebar.config` file it’s written in).


and then add the plugin to your rebar config:

    {plugins, [
        rebar3_path_deps
    ]}.

Then just compile your application

    $ rebar3 compile
    ===> Compiling rebar3_path_deps
    ===> Verifying dependencies...
    ===> Fetching hello_utils ({path,"hello_utils",
                                        {mtime,<<"2018-10-17T11:21:18Z">>}})
    ===> Compiling mysubapp
    ===> Compiling myapp
