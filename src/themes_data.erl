-module(themes_data).
-export([index/3]).

-include("themes.hrl").

-define(ICON_THEME,<<"Icon Theme">>).
-define(NAME,<<"Name">>).
-define(INHERITS,<<"Inherits">>).
-define(COMMENT,<<"Comment">>).
-define(EXAMPLE,<<"Example">>).
-define(HIDDEN,<<"Hidden">>).
-define(DIRECTORIES,<<"Directories">>).
-define(SIZE,<<"Size">>).
-define(SCALE,<<"Scale">>).
-define(CONTEXT,<<"Context">>).
-define(TYPE,<<"Type">>).
-define(MAX_SIZE,<<"MaxSize">>).
-define(MIN_SIZE,<<"MinSize">>).
-define(THRESHOLD,<<"Threshold">>).


index(Id, IdxFile, LoadFun) ->
    try
        Data = parse_file(IdxFile),
        IconTheme = section(?ICON_THEME, Data),
        case value(?NAME, IconTheme, undefined) of
            undefined ->
                Inherits = value(?INHERITS, IconTheme),
                LoadFun(unicode:characters_to_list(Inherits));
            Name ->
                index(Id, Name, Data, LoadFun)
        end
    catch
        throw:Error -> {error, [{index, IdxFile} | Error]}
    end.


index(Id, Name, Data, LoadFun) ->
    IconTheme = section(?ICON_THEME, Data),
    {ok, #theme{ id = Id
               , name = Name
               , comment = value(?COMMENT, IconTheme)
               , example = value(?EXAMPLE, IconTheme, undefined)
               , hidden = hidden(IconTheme)
               , directories = directories(IconTheme, Data)
               , inherits = inherits(Id, IconTheme, LoadFun)
               }}.


inherits(?FALLBACK_THEME, _, _) ->
    undefined;
inherits(_Id, IconTheme, LoadFun) ->
    case value(?INHERITS, IconTheme, undefined) of
        undefined ->
            inherits(LoadFun(?FALLBACK_THEME));
        Inherits ->
            case inherits(LoadFun(unicode:characters_to_list(Inherits))) of
                undefined -> inherits(LoadFun(?FALLBACK_THEME));
                Result    -> Result
            end
    end.

inherits({ok, Theme}) -> Theme;
inherits(_) -> undefined.


hidden(IconTheme) ->
    case value(?HIDDEN, IconTheme, undefined) of
        <<"true">> -> true;
        _          -> false
    end.


directories(IconTheme, Data) ->
    Dirs = value(?DIRECTORIES, IconTheme),
    lists:foldr(fun (V,Acc) -> directories(V, Data, Acc) end, [],
                string:lexemes(Dirs, [$,])).

directories(Dir, Data, Acc) ->
    DirData = section(Dir, Data),
    Size = directory_size(DirData),
    [#theme_directory{ directory = unicode:characters_to_list(Dir)
                     , size  = Size
                     , scale = directory_scale(DirData)
                     , context = directory_context(DirData)
                     , type = directory_type(DirData)
                     , max_size = directory_max_size(DirData, Size)
                     , min_size = directory_min_size(DirData, Size)
                     , threshold = directory_threshold(DirData)
                     } | Acc].


directory_size(DirData) ->
    binary_to_integer(value(?SIZE, DirData)).


directory_scale(DirData) ->
    case value(?SCALE, DirData, undefined) of
        undefined -> 1;
        Value     -> binary_to_integer(Value)
    end.


directory_context(DirData) ->
    case value(?CONTEXT, DirData, undefined) of
        <<"Actions">>       -> actions;
        <<"Animations">>    -> animations;
        <<"Applications">>  -> applications;
        <<"Categories">>    -> categories;
        <<"Devices">>       -> devices;
        <<"Emblems">>       -> emblems;
        <<"Emotes">>        -> emotes;
        <<"FileSystems">>   -> file_systems;
        <<"International">> -> international;
        <<"MimeTypes">>     -> mime_types;
        <<"Places">>        -> places;
        <<"Status">>        -> status;
        <<"Stock">>         -> stock;
        _                   -> other
    end.


directory_type(DirData) ->
    case value(?TYPE, DirData, undefined) of
        <<"Fixed">>     -> fixed;
        <<"Scalable">>  -> scalable;
        <<"Threshold">> -> threshold;
        undefined       -> threshold
    end.


directory_max_size(DirData, Size) ->
    case value(?MAX_SIZE,DirData,undefined) of
        undefined -> Size;
        Value     -> binary_to_integer(Value)
    end.


directory_min_size(DirData, Size) ->
    case value(?MIN_SIZE,DirData,undefined) of
        undefined -> Size;
        Value     -> binary_to_integer(Value)
    end.


directory_threshold(DirData) ->
    case value(?THRESHOLD, DirData, undefined) of
        undefined -> 2;
        Value     -> binary_to_integer(Value)
    end.


parse_file(File) ->
    case file:read_file(File) of
        {ok, Data} ->
            parse_lines(string:lexemes(Data, [$\n]), []);
        {error, Error} ->
            throw([{read_file, Error}])
    end.


parse_lines([], Acc) -> lists:reverse(Acc);
parse_lines(Lines, Acc) -> parse_section(Lines, Acc).


parse_section([Line | Lines], Acc) ->
    Line1 = string:trim(Line),
    case string:is_empty(Line1) of
        true -> parse_section(Lines, Acc);
        false -> parse_section(Line1, Lines, Acc)
    end;

parse_section([], Acc) ->
    parse_lines([], Acc).

parse_section(<<$[,Rest/binary>>=Line, Lines, Acc) ->
    case string:split(Rest, [$]]) of
        [Section, <<>>] -> parse_values(Section, Lines, [], Acc);
        _ -> throw([{invalid_section, Line}])
    end;

parse_section(<<$#,_/binary>>, Lines, Acc) ->
    parse_section(Lines, Acc);

parse_section(_, _, _) ->
    throw([{invalid_format, missing_section}]).


parse_values(Section, [Line | Lines], Values, Acc) ->
    Line1 = string:trim(Line),
    case string:is_empty(Line1) of
        true -> parse_values(Section, Lines, Values, Acc);
        false -> parse_values(Line1, Section, Lines, Values, Acc)
    end;

parse_values(Section, [], Values, Acc) ->
    parse_lines([], [{Section, lists:reverse(Values)} | Acc]).


parse_values(<<$#,_/binary>>, Section, Lines, Values, Acc) ->
    parse_values(Section, Lines, Values, Acc);

parse_values(Line, Section, Lines, Values, Acc) ->
    case string:split(Line, [$=]) of
        [Name, Value] ->
            parse_values(Section, Lines, [{Name, Value} | Values], Acc);
        _ ->
            parse_section(Line, Lines, [{Section, lists:reverse(Values)} | Acc])
    end.


section(Name, Data) ->
    case lists:keyfind(Name, 1, Data) of
        false -> throw([{missing_section, Name}]);
        {_, Values} -> Values
    end.


value(Name, Section) ->
    case lists:keyfind(Name, 1, Section) of
        false -> throw([{missing_value, Name}]);
        {_, Value} -> Value
    end.


value(Name, Section, Default) ->
    case lists:keyfind(Name, 1, Section) of
        false -> Default;
        {_, Value} -> Value
    end.
