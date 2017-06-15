-module(themes_cursor).
-export([ load/2, load/3 ]).

-include("themes.hrl").
-include_lib("kernel/include/file.hrl").

-define(MAX_CURSOR_NAME,30).

-define(FILE_MAJOR,1).
-define(FILE_MINOR,0).
-define(FILE_VER,((?FILE_MAJOR bsl 16) bor ?FILE_MINOR)).
-define(FILE_HDR_LEN,(4 * 4)).
-define(FILE_TOC_LEN,(3 * 4)).

-define(CHUNK_HDR_LEN,(4 * 4)).

-define(IMAGE_TYPE,16#fffd0002).
-define(IMAGE_HDR_LEN,(?CHUNK_HDR_LEN + (5*4))).
-define(IMAGE_VER,1).
-define(IMAGE_MAX_DIM,32767).

-define(LU32(V),V:32/little-unsigned).


load(Theme, Size) ->
    Dirs = cursors_directories(Theme),
    Acc = lists:foldl(fun (D, Acc0) -> add_cursors(D,Size,Acc0) end, #{}, Dirs),
    lists:keysort(#cursor.id, maps:values(Acc)).


load(Theme, Id, Size) ->
    Dirs = cursors_directories(Theme),
    try
        lists:foreach(fun (D) -> find_cursor(D, Id, Size) end, Dirs),
        undefined
    catch
        throw:{found, Cursor} -> Cursor
    end.


add_cursors(Dir, Size, Acc) ->
    {Files, Aliases} = cursors_files(Dir),
    lists:foldl(fun (File, Acc0) -> add_cursor(File, Size, Aliases, Acc0) end,
                Acc, [filename:join([Dir, F]) || F <- Files]).


add_cursor(File, Size, Aliases, Acc) ->
    try load_cursor(File, Size, Aliases) of
        #cursor{id=Id}=Cursor -> Acc#{Id => Cursor};
        false                 -> Acc
    catch
        throw:Error ->
            error_logger:error_report([{module, ?MODULE}
                                      ,{load_cursor, File}
                                      ,{error, Error}]),
            Acc
    end.


find_cursor(Dir, Id, Size) ->
    {Files, Aliases} = cursors_files(Dir),
    case lists:member(Id, Files) of
        true  -> find_cursor_by_id(Dir, Id, Size, Aliases);
        false -> find_cursor_by_alias(Dir, Id, Size, Aliases)
    end.


find_cursor_by_id(Dir, Id, Size, Aliases) ->
    try load_cursor(filename:join([Dir, Id]), Size, Aliases) of
        false  -> undefined;
        Cursor -> throw({found, Cursor})
    catch
        throw:_ -> undefined
    end.


find_cursor_by_alias(Dir, Id, Size, Aliases) ->
    lists:foreach(fun ({K, Vs}) ->
                      case lists:member(Id, Vs) of
                          true  -> find_cursor_by_id(Dir, K, Size, Aliases);
                          false -> undefined
                      end
                  end, maps:to_list(Aliases)).


load_cursor(File, Size, Aliases) ->
    case file:open(File, [raw, binary]) of
        {ok, IoDev} ->
            try load_images(IoDev, Size) of
                {ImgSize, Imgs} ->
                    Id = filename:basename(File),
                    #cursor{ id = Id
                           , size = ImgSize
                           , images = Imgs
                           , aliases = maps:get(Id, Aliases, [])
                           };
                false -> false
            catch
                _:_ -> throw(invalid_cursor_file)
            after
                file:close(IoDev)
            end;
        {error, Reason} ->
            throw(Reason)
    end.


load_images(IoDev, Size) ->
    case best_images(load_tocs(IoDev), Size) of
        {_, []} ->
            false;
        {Size1, Pos} ->
            {Size1, lists:map(fun (P) -> load_image(IoDev, Size1, P) end, Pos)}
    end.


load_image(IoDev, Size, Pos) ->
    {ok,
     <<?LU32(?IMAGE_HDR_LEN),?LU32(?IMAGE_TYPE),?LU32(Size),?LU32(?IMAGE_VER)
       ,?LU32(W),?LU32(H),?LU32(Xhot),?LU32(Yhot),?LU32(Delay)>>
    } = file:pread(IoDev, Pos, ?IMAGE_HDR_LEN),

    true = check_image(W,H,Xhot,Yhot),
    DataPos = Pos + ?IMAGE_HDR_LEN,
    {ok, Data} = file:pread(IoDev, DataPos, W*H*4),
    #cursor_image{ width=W
                 , height=H
                 , x_hot=Xhot
                 , y_hot=Yhot
                 , delay=Delay
                 , data=Data
                 }.


check_image(W,H,Xhot,Yhot) ->
        (W > 0) and (W < ?IMAGE_MAX_DIM)
    and (H > 0) and (H < ?IMAGE_MAX_DIM)
    and (Xhot =< W) and (Yhot =< H).

load_tocs(IoDev) ->
    {ok, <<"Xcur",?LU32(?FILE_HDR_LEN),?LU32(?FILE_VER),?LU32(NToc)>>}
        = file:read(IoDev, ?FILE_HDR_LEN),
    {ok, TocsBin} = file:read(IoDev, NToc * 12),
    [{Sz,Pos} || <<?LU32(?IMAGE_TYPE),?LU32(Sz),?LU32(Pos)>> <= TocsBin].


best_images([], Size) ->
    {Size, []};
best_images([{HSz, HPos} | Rest], Size) ->
    lists:foldr(fun (I,Acc) -> best_image(I,Acc,Size) end, {HSz, [HPos]}, Rest).


best_image({Sz,Pos}, {Sz, Acc}, _) ->
    {Sz, [Pos| Acc]};
best_image({Sz1,Pos}, {Sz2, Acc}, Size) ->
    case erlang:abs(Sz1-Size) < erlang:abs(Sz2-Size) of
        true  -> {Sz1, [Pos]};
        false -> {Sz2, Acc}
    end.


cursors_directories(#theme{roots = Dirs, inherits = undefined}) ->
    cursors_directories_1(lists:reverse(Dirs));

cursors_directories(#theme{roots = Dirs, inherits = Theme}) ->
    cursors_directories(Theme) ++ cursors_directories_1(lists:reverse(Dirs)).


cursors_directories_1(Dirs) ->
    lists:filtermap(fun cursors_directory/1, Dirs).


cursors_directory(Root) ->
    Dir = filename:join(Root, "cursors"),
    case filelib:is_dir(Dir) of
        true -> {true, Dir};
        false -> false
    end.


cursors_files(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            lists:foldl(fun (F, Acc) -> cursors_file(Dir, F, Acc) end, {[],#{}}, Files);
        _ ->
            {[],#{}}
    end.


cursors_file(_, File, Acc) when length(File) > ?MAX_CURSOR_NAME ->
    Acc;

cursors_file(Dir, File, {Files, Aliases} = Acc) ->
    Filename = filename:join([Dir, File]),
    case file:read_link_info(Filename) of
        {ok, #file_info{type=regular}} ->
            {[File | Files], Aliases};

        {ok, #file_info{type=symlink}} ->
            {Files, cursors_alias(File, file:read_link(Filename), Aliases)};

        _ ->
            Acc
    end.

cursors_alias(Alias, {ok, File}, Aliases) ->
    case filename:basename(File) of
        File when length(File) =< ?MAX_CURSOR_NAME ->
            maps:update_with(File, fun (Vs) -> [Alias|Vs] end, [Alias], Aliases);
        _ ->
            Aliases
    end;

cursors_alias(_, _, Aliases) ->
    Aliases.
