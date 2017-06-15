-module(themes_icon).
-export([lookup/5, lookup_fallback/3]).

-include("themes.hrl").

-define(MAX_SIZE,(2 bsl 31)).

lookup(Theme, Id, Size, Scale, Exts) ->
    case lookup_icon(Theme, Id, Size, Scale, Exts) of
        {ThemeDir, Filename} ->
            icon_data(ThemeDir, Filename);
        undefined when Theme#theme.inherits /= undefined ->
            lookup(Theme#theme.inherits, Id, Size, Scale, Exts);
        undefined ->
            undefined
    end.


lookup_fallback(Id, Dirs, Exts) ->
    lookup_fallback(Id, Exts, Dirs, Exts).

lookup_fallback(_, _, [], _) ->
    undefined;

lookup_fallback(Id, Exts, [_|Dirs], []) ->
    lookup_fallback(Id, Exts, Dirs, Exts);

lookup_fallback(Id, Exts, [Dir|_] = Dirs, [E|Es]) ->
    case icon_file(Dir, Id, E) of
        undefined -> lookup_fallback(Id, Exts, Dirs, Es);
        Result    -> icon_fallback_data(Result)
    end.


lookup_icon(Theme, Id, Size, Scale, Exts) ->
    case lookup_icon_size_match(Theme, Id, Size, Scale, Exts) of
        undefined -> lookup_icon_size_distance(Theme, Id, Size, Scale, Exts);
        Result    -> Result
    end.


lookup_icon_size_match(Theme, Id, Size, Scale, Exts) ->
    find(fun (Root, Dir, Ext) ->
             case icon_size_match(Dir, Size, Scale) of
                 true  -> icon_directory_file(Root, Dir, Id, Ext);
                 false -> undefined
             end
         end, Theme, Exts).


lookup_icon_size_distance(Theme, Id, Size, Scale, Exts) ->
    {Result, _} = fold(fun (Root, Dir, Ext, {_, MinSize} = Acc) ->
                           case icon_directory_file(Root, Dir, Id, Ext) of
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
                       end, {undefined, ?MAX_SIZE}, Theme, Exts),
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


icon_directory_file(Root, #theme_directory{directory=Dir}=D, Id, Ext) ->
    Filename = filename:join([Root, Dir, Id ++ wildcard(Ext)]),
    case icon_file(Filename) of
        undefined -> undefined;
        IconFile  -> {D, IconFile}
    end.


icon_file(Dir, Id, Ext) ->
    Filename = filename:join([Dir, Id ++ wildcard(Ext)]),
    icon_file(Filename).


icon_file(Wildcard) ->
    Files = filelib:wildcard(Wildcard),
    case lists:sort(fun (F1,F2) -> length(F1) =< length(F2) end, Files) of
        [File | _] -> File;
        []         -> undefined
    end.



icon_data(#theme_directory{context=Ctx,size=Sz,min_size=MinSz,max_size=MaxSz}
         ,Filename) ->
    case icon_binary(Filename) of
        undefined ->
            undefined;
        Data ->
            {Id, Type} = icon_id_type(Filename),
            #icon{ id = Id
                 , size = Sz
                 , max_size = MaxSz
                 , min_size = MinSz
                 , context = Ctx
                 , type = Type
                 , data = Data
                 }
    end.


icon_fallback_data(Filename) ->
    case file:read_file(Filename) of
        undefined ->
            undefined;
        Data ->
            {Id, Type} = icon_id_type(Filename),
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


icon_id_type(Filename) ->
    Basename = filename:basename(Filename),
    Id = filename:rootname(Basename),
    Type = extension_to_type(filename:extension(Basename)),
    {Id, Type}.


extension_to_type(".png") -> png;
extension_to_type(".svg") -> svg;
extension_to_type(".xpm") -> xpm.


find(Fun, #theme{directories=Dirs, roots=Roots}, Exts) ->
    find(Fun, Roots, Exts, Dirs, Roots, Exts).

find(Fun, Roots, Exts, [Dir | _] = Dirs, [R | _] = Rs, [E | Es]) ->
    case Fun(R, Dir, E) of
        undefined -> find(Fun, Roots, Exts, Dirs, Rs, Es);
        Result    -> Result
    end;

find(Fun, Roots, Exts, Dirs, [_ | Rs], []) ->
    find(Fun, Roots, Exts, Dirs,  Rs, Exts);

find(Fun, Roots, Exts, [_|Dirs], [], _) ->
    find(Fun, Roots, Exts, Dirs, Roots, Exts);

find(_, _, _, [], _, _) ->
    undefined.


fold(Fun, Acc, #theme{directories=Dirs, roots=Roots}, Exts) ->
    fold(Fun, Acc, Roots, Exts, Dirs, Roots, Exts).

fold(Fun, Acc0, Roots, Exts, [Dir | _] = Dirs, [R | _] = Rs, [E | Es]) ->
    Acc1 = Fun(R, Dir, E, Acc0),
    fold(Fun, Acc1, Roots, Exts, Dirs, Rs, Es);

fold(Fun, Acc, Roots, Exts, Dirs, [_ | Rs], []) ->
    fold(Fun, Acc, Roots, Exts, Dirs,  Rs, Exts);

fold(Fun, Acc, Roots, Exts, [_|Dirs], [], _) ->
    fold(Fun, Acc, Roots, Exts, Dirs, Roots, Exts);

fold(_, Acc, _, _, [], _, _) ->
    Acc.


wildcard(png) -> "*.png";
wildcard(svg) -> "*.svg";
wildcard(xpm) -> "*.xpm".
