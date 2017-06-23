-module(themes_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("themes/include/themes.hrl").


example_theme_test_() ->
  {setup, fun () -> setup(), themes:load("birch") end,
    {with, [
       fun (V) -> ?assertMatch({ok, #theme{name = <<"Birch">>}}, V) end,

       fun ({ok, #theme{comment=C}}) ->
           ?assertEqual(<<"Icon theme with a wooden look">>, C)
       end,

       fun ({ok, #theme{directories=Ds}}) -> ?assertEqual(7, length(Ds)) end,

       fun ({ok, #theme{directories=[D|_]}}) ->
           ?assertMatch(#theme_directory{directory = "48x48/apps"}, D),
           ?assertMatch(#theme_directory{size = 48}, D),
           ?assertMatch(#theme_directory{scale = 1}, D),
           ?assertMatch(#theme_directory{context = applications}, D),
           ?assertMatch(#theme_directory{type = fixed}, D),
           ?assertMatch(#theme_directory{min_size = 48}, D),
           ?assertMatch(#theme_directory{max_size = 48}, D)
       end,

       fun ({ok, T}) ->
           ?assertMatch(#icon{id="mozilla", size=32, type=png}
                       ,themes:icon(T, "mozilla", 32, 1)),

           ?assertMatch(#icon{id="mozilla", size=48, type=png}
                       ,themes:icon(T, "mozilla", 48, 1)),

           ?assertMatch(#icon{id="mozilla", size=48, max_size=256, type=svg}
                       ,themes:icon(T, "mozilla", 96, 1))
       end
  ]}}.


setup() ->
    {_, _, ModuleFile} = code:get_object_code(?MODULE),
    Base = filename:dirname(ModuleFile),
    IconDir = filename:join([Base, "icons"]),
    {ok, _} = application:ensure_all_started(themes),
    ok = application:set_env(themes, directories, [IconDir]).
