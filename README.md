rebar3_path_dep
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { rebar3_path_dep, ".*", {git, "git@host:user/rebar3_path_dep.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar3_path_dep
    ===> Fetching rebar3_path_dep
    ===> Compiling rebar3_path_dep
    <Plugin Output>
