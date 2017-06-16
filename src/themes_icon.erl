-module(themes_icon).
-export([ lookup/5
        , lookup_fallback/3
        , cache/2
        ]).

-on_load(init_nifs/0).

-include("themes.hrl").
-include_lib("kernel/include/file.hrl").

-define(MAX_SIZE,(2 bsl 31)).
-define(ICON_XPM,1).
-define(ICON_SVG,2).
-define(ICON_PNG,4).
-define(HAS_ICON_FILE,8).
-define(ICON_SYMBOLIC_PNG,16).


lookup(#theme{cache=Cache}=Theme, Id, Size, Scale, Types) ->
    case lookup_cache(Cache, Id, Size, Scale, Types) of
        undefined ->
            case lookup_file(Theme, Id, Size, Scale, Types) of
                {ThemeDir, Type, IconFile} ->
                    icon_data(ThemeDir, Id, Type, IconFile);
                undefined when Theme#theme.inherits /= undefined ->
                    lookup(Theme#theme.inherits, Id, Size, Scale, Types);
                undefined ->
                    undefined
            end;
        Result ->
            Result
    end.


lookup_fallback(Id, Dirs, Types) ->
    lookup_fallback(Id, Types, Dirs, Types).

lookup_fallback(_, _, [], _) ->
    undefined;

lookup_fallback(Id, Types, [_|Dirs], []) ->
    lookup_fallback(Id, Types, Dirs, Types);

lookup_fallback(Id, Types, [Dir|_] = Dirs, [T|Ts]) ->
    Filename = filename:join([Dir, Id ++ wildcard(T)]),
    case icon_file(Filename) of
        undefined -> lookup_fallback(Id, Types, Dirs, Ts);
        IconFile  -> icon_fallback_data(Id, T, IconFile)
    end.


cache(Roots, ThemeDirs) ->
    lists:filtermap(fun (Root) -> init_cache(Root, ThemeDirs) end, Roots).


lookup_file(Theme, Id, Size, Scale, Types) ->
    case lookup_icon_size_match(Theme, Id, Size, Scale, Types) of
        undefined -> lookup_icon_size_distance(Theme, Id, Size, Scale, Types);
        Result    -> Result
    end.


lookup_icon_size_match(Theme, Id, Size, Scale, Types) ->
    find(fun (Root, Dir, Type) ->
             case icon_size_match(Dir, Size, Scale) of
                 true  -> icon_directory_file(Root, Dir, Id, Type);
                 false -> undefined
             end
         end, Theme, Types).


lookup_icon_size_distance(Theme, Id, Size, Scale, Types) ->
    {Result, _} = fold(fun (Root, Dir, Type, {_, MinSize} = Acc) ->
                           case icon_directory_file(Root, Dir, Id, Type) of
                               undefined ->
                                   Acc;
                               DirFile ->
                                   case icon_size_distance(Dir, Size, Scale) of
                                       NewSize when NewSize < MinSize ->
                                           {DirFile, NewSize};
                                       _ ->
                                           Acc
                                   end
                           end
                       end, {undefined, ?MAX_SIZE}, Theme, Types),
    Result.


icon_size_match(#theme_directory{scale=Scale}=Dir, IconSize, Scale) ->
    #theme_directory{ size = Size
                    , min_size = MinSize
                    , max_size = MaxSize
                    , threshold = Threshold } = Dir,
    case Dir#theme_directory.type of
        fixed when Size == IconSize -> true;
        scalable when MinSize =< IconSize
                    , MaxSize >= IconSize -> true;
        threshold when (Size - Threshold) =< IconSize
                     , (Size + Threshold) >= IconSize -> true;
        _ -> false
    end;

icon_size_match(_, _, _) ->
    false.


icon_size_distance(Dir, IconSize, IconScale) ->
    #theme_directory{ size = Size
                    , scale = Scale
                    , min_size = MinSize
                    , max_size = MaxSize
                    , threshold = Threshold } = Dir,
    case Dir#theme_directory.type of
        fixed ->
            erlang:abs(Size * Scale - IconSize * IconScale);

        scalable when IconSize * IconScale < MinSize * Scale ->
            MinSize * Scale - IconSize * IconScale;

        scalable when IconSize * IconScale > MaxSize * Scale ->
           IconSize * IconScale -  MaxSize * Scale;

        threshold when IconSize * IconScale < (Size - Threshold) * Scale ->
            MinSize * Scale - IconSize * IconScale;

        threshold when IconSize * IconScale > (Size + Threshold) * Scale ->
            IconSize * IconScale - MaxSize * Scale;

        _ ->
            0
    end.


icon_directory_file(Root, #theme_directory{directory=Dir}=D, Id, Type) ->
    Filename = filename:join([Root, Dir, Id ++ wildcard(Type)]),
    case icon_file(Filename) of
        undefined -> undefined;
        IconFile  -> {D, Type, IconFile}
    end.


icon_file(Wildcard) ->
    Files = filelib:wildcard(Wildcard),
    case lists:sort(fun (F1,F2) -> length(F1) =< length(F2) end, Files) of
        [File | _] -> File;
        []         -> undefined
    end.


icon_data(#theme_directory{context=Ctx,size=Sz,min_size=MinSz,max_size=MaxSz}
         ,Id, Type, IconFile) ->
    case icon_binary(IconFile) of
        undefined ->
            undefined;
        Data ->
            #icon{ id = Id
                 , size = Sz
                 , max_size = MaxSz
                 , min_size = MinSz
                 , context = Ctx
                 , type = Type
                 , data = Data
                 }
    end.


icon_fallback_data(Id, Type, IconFile) ->
    case icon_binary(IconFile) of
        undefined ->
            undefined;
        Data ->
            #icon_fallback{ id = Id
                          , type = Type
                          , data = Data
                          }
    end.


icon_binary(Filename) ->
    case file:read_file(Filename) of
        {ok, Data} ->
            Data;
        {error, Error} ->
            error_logger:error_report([{module, ?MODULE}
                                      ,{load_icon, Filename}
                                      ,{error, Error}]),
            undefined
    end.


find(Fun, #theme{directories=Dirs, roots=Roots}, Types) ->
    find(Fun, Roots, Types, Dirs, Roots, Types).

find(Fun, Roots, Types, [Dir | _] = Dirs, [R | _] = Rs, [T | Ts]) ->
    case Fun(R, Dir, T) of
        undefined -> find(Fun, Roots, Types, Dirs, Rs, Ts);
        Result    -> Result
    end;

find(Fun, Roots, Types, Dirs, [_ | Rs], []) ->
    find(Fun, Roots, Types, Dirs,  Rs, Types);

find(Fun, Roots, Types, [_|Dirs], [], _) ->
    find(Fun, Roots, Types, Dirs, Roots, Types);

find(_, _, _, [], _, _) ->
    undefined.


fold(Fun, Acc, #theme{directories=Dirs, roots=Roots}, Types) ->
    fold(Fun, Acc, Roots, Types, Dirs, Roots, Types).

fold(Fun, Acc0, Roots, Types, [Dir | _] = Dirs, [R | _] = Rs, [T | Ts]) ->
    Acc1 = Fun(R, Dir, T, Acc0),
    fold(Fun, Acc1, Roots, Types, Dirs, Rs, Ts);

fold(Fun, Acc, Roots, Types, Dirs, [_ | Rs], []) ->
    fold(Fun, Acc, Roots, Types, Dirs,  Rs, Types);

fold(Fun, Acc, Roots, Types, [_|Dirs], [], _) ->
    fold(Fun, Acc, Roots, Types, Dirs, Roots, Types);

fold(_, Acc, _, _, [], _, _) ->
    Acc.


init_cache(Root, ThemeDirs) ->
    Dirs = [Dir || #theme_directory{directory=Dir} <- ThemeDirs],
    case cache_available(Root) of
        {true, CacheFile} ->
            try init_cache_nif(CacheFile, Dirs) of
                {Cache, DirIdx} ->
                    {true, {Root, {Cache, cache_directories(DirIdx, ThemeDirs)}}}
            catch
                error:Error when Error /= badarg ->
                    error_logger:error_report([{module, ?MODULE}
                                              ,{load_icon_cache, CacheFile}
                                              ,{error, Error}]),
                    false
            end;
        false ->
            false
    end.


cache_available(Root) ->
    CacheFile = filename:join(Root, "icon-theme.cache"),
    CacheFileInfo = file:read_file_info(CacheFile),
    DirInfo = file:read_file_info(Root),
    case cache_out_of_date(DirInfo, CacheFileInfo) of
        true  -> false;
        false -> {true, CacheFile}
    end.


cache_out_of_date({ok,#file_info{mtime=T1}}, {ok,#file_info{mtime=T2}}) ->
    T1 > T2;

cache_out_of_date(_, _) ->
    true.


cache_directories(DirIdx, ThemeDirs) ->
    lists:foldl(fun cache_directory/2, #{}, lists:zip(DirIdx, ThemeDirs)).

cache_directory({undefined, _}, Acc)  -> Acc;
cache_directory({Idx, ThemeDir}, Acc) -> Acc#{Idx => ThemeDir}.


lookup_cache([], _Id, _Size, _Scale, _Exts) ->
    undefined;

lookup_cache([{Root, {Cache, DirIdx}} | Rest], Id, Size, Scale, Types) ->
    case lookup_cache_nif(Cache, Id) of
        undefined ->
            lookup_cache(Rest, Id, Size, Scale, Types);
        Result ->
            case lookup_cache_result(Result, DirIdx, Size, Scale, Types) of
                undefined ->
                    lookup_cache(Rest, Id, Size, Scale, Types);
                {ThemeDir, Type} ->
                    Filename = cache_file(Root, ThemeDir, Id, Type),
                    icon_data(ThemeDir, Id, Type, Filename)
            end
    end.


lookup_cache_result(Result, DirIdx, Size, Scale, Types) ->
    ResultDir = lookup_cache_result_directories(Result, DirIdx),
    case lookup_cache_size_match(ResultDir, Size, Scale, Types) of
        undefined ->
            lookup_cache_size_distance(ResultDir, Size, Scale, Types);
        Match ->
            Match
    end.


lookup_cache_result_directories(Result, DirIdx) ->
    lists:filtermap(fun ({Idx, Flags, _}) ->
                        case maps:get(Idx, DirIdx, undefined) of
                            undefined -> false;
                            Dir       -> {true, {Dir, Flags}}
                        end
                    end, Result).


lookup_cache_size_match([],  _Size, _Scale, _Types) ->
    undefined;

lookup_cache_size_match([{Dir, Flags} | Rest], Size, Scale, Types) ->
    case icon_size_match(Dir, Size, Scale) of
        true ->
            case cache_type_match(Flags, Types) of
                {true, Type} ->
                    {Dir, Type};
                false ->
                    lookup_cache_size_match(Rest, Size, Scale, Types)
            end;
        false ->
            lookup_cache_size_match(Rest, Size, Scale, Types)
    end.


lookup_cache_size_distance(ResultDir, Size, Scale, Types) ->
    {Result, _} = lists:foldl(fun ({Dir, Flags}, {_, MinSize} = Acc) ->
                           case cache_type_match(Flags, Types) of
                               false ->
                                   Acc;
                               {true, Type} ->
                                   case icon_size_distance(Dir, Size, Scale) of
                                       NewSize when NewSize < MinSize ->
                                           {{Dir, Type}, NewSize};
                                       _ ->
                                           Acc
                                   end
                           end
                       end, {undefined, ?MAX_SIZE}, ResultDir),
    Result.

cache_file(Root, #theme_directory{directory=Dir}, Id, Type) ->
    filename:join([Root, Dir, Id ++ extension(Type)]).


cache_type_match(_Flags, []) ->
    false;

cache_type_match(Flags, [Type | Types]) ->
    case Type of
        png when (Flags band ?ICON_PNG) /= 0 -> {true, png};
        svg when (Flags band ?ICON_SVG) /= 0 -> {true, svg};
        xpm when (Flags band ?ICON_XPM) /= 0 -> {true, xpm};
        _ -> cache_type_match(Flags, Types)
    end.


extension(png) -> ".png";
extension(svg) -> ".svg";
extension(xpm) -> ".xpm".


wildcard(png) -> "*.png";
wildcard(svg) -> "*.svg";
wildcard(xpm) -> "*.xpm".


init_nifs() ->
    erlang:load_nif(filename:join(code:priv_dir(themes), "icon_cache"), 0).

init_cache_nif(CacheFile, Dirs) ->
    erlang:nif_error(not_loaded, [CacheFile, Dirs]).

lookup_cache_nif(Cache, Id) ->
    erlang:nif_error(not_loaded, [Cache, Id]).
