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
              }).

-define(DEFAULT_THEME,"default").
-define(FALLBACK_THEME,"hicolor").
