-type context() :: actions
                 | animations
                 | applications
                 | categories
                 | devices
                 | emblems
                 | emotes
                 | file_systems
                 | international
                 | mime_types
                 | places
                 | status
                 | stock
                 | other.


-record(cursor_image,{ width :: integer()
                     , height :: integer()
                     , x_hot :: integer()
                     , y_hot :: integer()
                     , delay :: integer()
                     , data :: binary()
                     }).

-record(cursor,{ id :: string()
               , size :: integer()
               , images :: [#cursor_image{}]
               , aliases = [] :: [string()]
               }).


-record(icon,{ id :: string()
             , size :: integer()
             , max_size :: integer()
             , min_size :: integer()
             , context :: context()
             , type :: png | svg | xpm
             , data :: binary()
             }).

-record(icon_fallback,{ id :: string()
                      , type :: png | svg | xpm
                      , data :: binary()
                      }).


-record(theme_directory,{ directory :: file:filename()
                        , size :: integer()
                        , scale :: integer()
                        , context :: context()
                        , type :: fixed | scalable | threshold
                        , max_size :: integer()
                        , min_size :: integer()
                        , threshold :: integer()
                        }).

-record(theme,{ id :: string()
              , name :: binary()
              , comment :: binary()
              , example :: binary() | undefined
              , hidden :: boolean()
              , roots = [] :: [file:filename()]
              , directories :: [#theme_directory{}]
              , inherits :: #theme{} | undefined
              , cache = [] :: [{file:filename(), term()}]
              }).

-define(DEFAULT_THEME,"default").
-define(FALLBACK_THEME,"hicolor").
