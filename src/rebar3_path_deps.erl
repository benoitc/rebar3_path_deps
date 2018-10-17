-module(rebar3_path_deps).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok,rebar_state:add_resource(State, {path, rebar_path_resource})}.
