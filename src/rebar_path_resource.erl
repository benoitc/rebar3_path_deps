-module(rebar_path_resource).
-behaviour(rebar_resource).

-export([lock/2,
         download/3,
         needs_update/2,
         make_vsn/1]).

lock(_Dir, {path, Path}) ->
  Vsn = hash_files(Path),
  {path, Path, binary_to_list(Vsn)};
lock(_Dir, {path, Path, _VSN}) ->
  Vsn = hash_files(Path),
  {path, Path, binary_to_list(Vsn)}.

download(Dir, {path, Path}, State) ->
  download_(Dir, Path, State);
download(Dir, {path, Path, _Ref}, State) ->
  download_(Dir, Path, State).

make_vsn(_Dir) ->
  {error, "Replacing version of type path not supported."}.

needs_update(Dir, {path, Path, _}) ->
  {ok, Cwd} = file:get_cwd(),
  Source = filename:join([Cwd, Path]),
  OldHash = read_hash(Dir),
  Need = hash_files(Source) /= OldHash,
  Need.


download_(Dir, Path, State) ->
  ok = filelib:ensure_dir(Dir),
  {ok, Cwd} = file:get_cwd(),
  Source = filename:join([Cwd, Path]),
  ec_file:copy(Source, Dir, [recursive, {file_info, [mode, time, owner, group]}]),
  Hash = hash_files(Source),
  ok = store_hash(Dir, Hash),
  {ok, State}.

hashname(Dir) -> filename:join([Dir, "._rebar_shah1sum"]).

store_hash(Dir, Hash) ->
  file:write_file(hashname(Dir), Hash).

read_hash(Dir) ->
  {ok, Hash} = file:read_file(hashname(Dir)),
  Hash.

to_timestamp({{Year,Month,Day},{Hours,Minutes,Seconds}}) ->
  (calendar:datetime_to_gregorian_seconds(
     {{Year,Month,Day},{Hours,Minutes,Seconds}}
    ) - 62167219200)*1000000.


hash_files(Source) ->
  Files = lists:sort(filter_files(dir_files(Source))),
  State = crypto:hash_init(sha),

  State2= lists:foldl(
            fun(Path, State1) ->
                LastModified = to_timestamp(filelib:last_modified(Path)),
                crypto:hash_update(State1, iolist_to_binary([Path, integer_to_binary(LastModified)]))
            end,
            State,
            Files
           ),

  Digest = crypto:hash_final(State2),
  to_hex(Digest).

to_hex(Bin) ->
    << <<(to_digit(H)),(to_digit(L))>> || <<H:4,L:4>> <= Bin >>.

to_digit(N) when N < 10 -> $0 + N;
to_digit(N)             -> $a + N-10.


dir_files(Path) ->
  case filelib:is_dir(Path) of
    true ->
      filelib:wildcard(filename:join(Path, "**"));
    false ->
      [Path]
  end.


filter_files(Files) ->
    lists:filter(fun is_excluded/1, [filename:absname(F) || F <- Files]).


is_excluded(Path) ->
      KnownExcludes = [
                     "^.",
                     "~$"
                      ],

      lists:foldl(fun(_, true) -> true;
                     (RE, false) ->
                      (re:run(Path, RE) =/= nomatch) orelse (filelib:is_regular (Path) /= true)
                  end, false, KnownExcludes).





