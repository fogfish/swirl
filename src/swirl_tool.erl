%% @doc
%%   command line tool
-module(swirl_tool).

-export([main/1]).

%%
%% swirl -o Dir INPUT
main(["-o", Dir, Input]) ->
   % compile template into beam file
   cc(Input, [{outdir, Dir}]);

main(["-j", Json, Input]) ->
   % bind a template with json
   {ok, T} = file:read_file(Input),
   {ok, J} = file:read_file(Json),
   R = swirl:apply(T, jsx:decode(J, [{labels, atom}])),
   io:format("~s", [R]);

main([]) ->
   io:format("Usage: swirl -o Dir INPUT\n").


%%
%% compiles template from file
cc(File, Opts) ->
   {ok, T} = file:read_file(File),
   Uid = list_to_atom(uid(File, Opts)),
   {ok, Mod, Bin} = swirl:c(Uid, #{f => T}),
   {module,  Uid} = code:load_binary(Mod, [], Bin),
   case beam(File, Opts) of
      undefined -> 
         ok;
      Beam      -> 
         file:write_file(Beam, Bin)
   end.

%%
%%
uid(File, Opts) ->
   case proplists:get_value(module, Opts) of
      % name space is not defined, uid is filename
      undefined -> 
         filename:basename(File, filename:extension(File));
      Mod when is_atom(Mod) -> 
         atom_to_list(Mod);
      Mod when is_tuple(Mod) ->
         string:join(
            lists:map(
               fun
               (X) when is_atom(X) -> atom_to_list(X);
               (X) when is_list(X) -> X
               end,
               tuple_to_list(Mod)
            ),
            "_"
         )
   end.

%%
%%
beam(File, Opts) ->
   case proplists:get_value(outdir, Opts) of
      undefined -> undefined;
      Dir       -> filename:join([Dir, uid(File, Opts) ++ ".beam"])
   end.
