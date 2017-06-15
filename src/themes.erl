-module(themes).

-export([ available/0
        , load/0
        , load/1
        , cursors/2
        , cursor/3
        , directories/0
        , standard_icon_names/1
        ]).

-include("themes.hrl").
-include_lib("kernel/include/file.hrl").

-export_type([context/0]).


-spec available() -> [string()].
available() ->
    [Id || {Id, _, _} <- themes()].


-spec load() -> {ok, #theme{}} | {error, Reason :: any}.
load() ->
    Themes = themes(),
    case load(?DEFAULT_THEME, Themes) of
        {ok, _} = Result -> Result;
        _                -> load(?FALLBACK_THEME, Themes)
    end.


-spec load(Id :: string()) -> {ok, #theme{}} | {error, Reason :: any}.
load(Id) ->
    load(Id, themes()).


-spec cursors(Theme :: #theme{}, Size :: integer()) -> [#cursor{}].
cursors(Theme, Size) ->
    themes_cursor:load(Theme, Size).


-spec cursor(Theme :: #theme{}, Id :: string(), Size :: integer())
            -> #cursor{} | undefined.
cursor(Theme, Id, Size) ->
    themes_cursor:load(Theme, Id, Size).


-spec directories() -> [file:name()].
directories() ->
    config_dirs() ++ home_dir() ++ xdg_data_dirs() ++ ["/usr/share/pixmaps"].


-spec standard_icon_names(context()) -> [Name :: binary()].
standard_icon_names(actions) ->
    [ <<"address-book-new">>
    , <<"application-exit">>
    , <<"appointment-new">>
    , <<"call-start">>
    , <<"call-stop">>
    , <<"contact-new">>
    , <<"document-new">>
    , <<"document-open">>
    , <<"document-open-recent">>
    , <<"document-page-setup">>
    , <<"document-print">>
    , <<"document-print-preview">>
    , <<"document-properties">>
    , <<"document-revert">>
    , <<"document-save">>
    , <<"document-save-as">>
    , <<"document-send">>
    , <<"edit-clear">>
    , <<"edit-copy">>
    , <<"edit-cut">>
    , <<"edit-delete">>
    , <<"edit-find">>
    , <<"edit-find-replace">>
    , <<"edit-paste">>
    , <<"edit-redo">>
    , <<"edit-select-all">>
    , <<"edit-undo">>
    , <<"folder-new">>
    , <<"format-indent-less">>
    , <<"format-indent-more">>
    , <<"format-justify-center">>
    , <<"format-justify-fill">>
    , <<"format-justify-left">>
    , <<"format-justify-right">>
    , <<"format-text-direction-ltr">>
    , <<"format-text-direction-rtl">>
    , <<"format-text-bold">>
    , <<"format-text-italic">>
    , <<"format-text-underline">>
    , <<"format-text-strikethrough">>
    , <<"go-bottom">>
    , <<"go-down">>
    , <<"go-first">>
    , <<"go-home">>
    , <<"go-jump">>
    , <<"go-last">>
    , <<"go-next">>
    , <<"go-previous">>
    , <<"go-top">>
    , <<"go-up">>
    , <<"help-about">>
    , <<"help-contents">>
    , <<"help-faq">>
    , <<"insert-image">>
    , <<"insert-link">>
    , <<"insert-object">>
    , <<"insert-text">>
    , <<"list-add">>
    , <<"list-remove">>
    , <<"mail-forward">>
    , <<"mail-mark-important">>
    , <<"mail-mark-junk">>
    , <<"mail-mark-notjunk">>
    , <<"mail-mark-read">>
    , <<"mail-mark-unread">>
    , <<"mail-message-new">>
    , <<"mail-reply-all">>
    , <<"mail-reply-sender">>
    , <<"mail-send">>
    , <<"mail-send-receive">>
    , <<"media-eject">>
    , <<"media-playback-pause">>
    , <<"media-playback-start">>
    , <<"media-playback-stop">>
    , <<"media-record">>
    , <<"media-seek-backward">>
    , <<"media-seek-forward">>
    , <<"media-skip-backward">>
    , <<"media-skip-forward">>
    , <<"object-flip-horizontal">>
    , <<"object-flip-vertical">>
    , <<"object-rotate-left">>
    , <<"object-rotate-right">>
    , <<"process-stop">>
    , <<"system-lock-screen">>
    , <<"system-log-out">>
    , <<"system-run">>
    , <<"system-search">>
    , <<"system-reboot">>
    , <<"system-shutdown">>
    , <<"tools-check-spelling">>
    , <<"view-fullscreen">>
    , <<"view-refresh">>
    , <<"view-restore">>
    , <<"view-sort-ascending">>
    , <<"view-sort-descending">>
    , <<"window-close">>
    , <<"window-new">>
    , <<"zoom-fit-best">>
    , <<"zoom-in">>
    , <<"zoom-original">>
    , <<"zoom-out">> ];

standard_icon_names(animations) ->
    [ <<"process-working">> ];

standard_icon_names(applications) ->
    [ <<"accessories-calculator">>
    , <<"accessories-character-map">>
    , <<"accessories-dictionary">>
    , <<"accessories-text-editor">>
    , <<"help-browser">>
    , <<"multimedia-volume-control">>
    , <<"preferences-desktop-accessibility">>
    , <<"preferences-desktop-font">>
    , <<"preferences-desktop-keyboard">>
    , <<"preferences-desktop-locale">>
    , <<"preferences-desktop-multimedia">>
    , <<"preferences-desktop-screensaver">>
    , <<"preferences-desktop-theme">>
    , <<"preferences-desktop-wallpaper">>
    , <<"system-file-manager">>
    , <<"system-software-install">>
    , <<"system-software-update">>
    , <<"utilities-system-monitor">>
    , <<"utilities-terminal">> ];

standard_icon_names(categories) ->
    [ <<"applications-accessories">>
    , <<"applications-development">>
    , <<"applications-engineering">>
    , <<"applications-games">>
    , <<"applications-graphics">>
    , <<"applications-internet">>
    , <<"applications-multimedia">>
    , <<"applications-office">>
    , <<"applications-other">>
    , <<"applications-science">>
    , <<"applications-system">>
    , <<"applications-utilities">>
    , <<"preferences-desktop">>
    , <<"preferences-desktop-peripherals">>
    , <<"preferences-desktop-personal">>
    , <<"preferences-other">>
    , <<"preferences-system">>
    , <<"preferences-system-network">>
    , <<"system-help">> ];

standard_icon_names(devices) ->
    [ <<"audio-card">>
    , <<"audio-input-microphone">>
    , <<"battery">>
    , <<"camera-photo">>
    , <<"camera-video">>
    , <<"camera-web">>
    , <<"computer">>
    , <<"drive-harddisk">>
    , <<"drive-optical">>
    , <<"drive-removable-media">>
    , <<"input-gaming">>
    , <<"input-keyboard">>
    , <<"input-mouse">>
    , <<"input-tablet">>
    , <<"media-flash">>
    , <<"media-floppy">>
    , <<"media-optical">>
    , <<"media-tape">>
    , <<"modem">>
    , <<"multimedia-player">>
    , <<"network-wired">>
    , <<"network-wireless">>
    , <<"pda">>
    , <<"phone">>
    , <<"printer">>
    , <<"scanner">>
    , <<"video-display">> ];

standard_icon_names(emblems) ->
    [ <<"emblem-default">>
    , <<"emblem-documents">>
    , <<"emblem-downloads">>
    , <<"emblem-favorite">>
    , <<"emblem-important">>
    , <<"emblem-mail">>
    , <<"emblem-photos">>
    , <<"emblem-readonly">>
    , <<"emblem-shared">>
    , <<"emblem-symbolic-link">>
    , <<"emblem-synchronized">>
    , <<"emblem-system">>
    , <<"emblem-unreadable">> ];

standard_icon_names(emotes) ->
    [ <<"face-angel">>
    , <<"face-angry">>
    , <<"face-cool">>
    , <<"face-crying">>
    , <<"face-devilish">>
    , <<"face-embarrassed">>
    , <<"face-kiss">>
    , <<"face-laugh">>
    , <<"face-monkey">>
    , <<"face-plain">>
    , <<"face-raspberry">>
    , <<"face-sad">>
    , <<"face-sick">>
    , <<"face-smile">>
    , <<"face-smile-big">>
    , <<"face-smirk">>
    , <<"face-surprise">>
    , <<"face-tired">>
    , <<"face-uncertain">>
    , <<"face-wink">>
    , <<"face-worried">> ];

standard_icon_names(file_systems) ->
    [
    ];

standard_icon_names(international) ->
    [ <<"flag-">> ]; % ++ ISO 3166 two-letter country code in lowercase form

standard_icon_names(mime_types) ->
    [ <<"application-x-executable">>
    , <<"audio-x-generic">>
    , <<"font-x-generic">>
    , <<"image-x-generic">>
    , <<"package-x-generic">>
    , <<"text-html">>
    , <<"text-x-generic">>
    , <<"text-x-generic-template">>
    , <<"text-x-script">>
    , <<"video-x-generic">>
    , <<"x-office-address-book">>
    , <<"x-office-calendar">>
    , <<"x-office-document">>
    , <<"x-office-presentation">>
    , <<"x-office-spreadsheet">> ];

standard_icon_names(places) ->
    [ <<"folder">>
    , <<"folder-remote">>
    , <<"network-server">>
    , <<"network-workgroup">>
    , <<"start-here">>
    , <<"user-bookmarks">>
    , <<"user-desktop">>
    , <<"user-home">>
    , <<"user-trash">> ];

standard_icon_names(status) ->
    [ <<"appointment-missed">>
    , <<"appointment-soon">>
    , <<"audio-volume-high">>
    , <<"audio-volume-low">>
    , <<"audio-volume-medium">>
    , <<"audio-volume-muted">>
    , <<"battery-caution">>
    , <<"battery-low">>
    , <<"dialog-error">>
    , <<"dialog-information">>
    , <<"dialog-password">>
    , <<"dialog-question">>
    , <<"dialog-warning">>
    , <<"folder-drag-accept">>
    , <<"folder-open">>
    , <<"folder-visiting">>
    , <<"image-loading">>
    , <<"image-missing">>
    , <<"mail-attachment">>
    , <<"mail-unread">>
    , <<"mail-read">>
    , <<"mail-replied">>
    , <<"mail-signed">>
    , <<"mail-signed-verified">>
    , <<"media-playlist-repeat">>
    , <<"media-playlist-shuffle">>
    , <<"network-error">>
    , <<"network-idle">>
    , <<"network-offline">>
    , <<"network-receive">>
    , <<"network-transmit">>
    , <<"network-transmit-receive">>
    , <<"printer-error">>
    , <<"printer-printing">>
    , <<"security-high">>
    , <<"security-medium">>
    , <<"security-low">>
    , <<"software-update-available">>
    , <<"software-update-urgent">>
    , <<"sync-error">>
    , <<"sync-synchronizing">>
    , <<"task-due">>
    , <<"task-past-due">>
    , <<"user-available">>
    , <<"user-away">>
    , <<"user-idle">>
    , <<"user-offline">>
    , <<"user-trash-full">>
    , <<"weather-clear">>
    , <<"weather-clear-night">>
    , <<"weather-few-clouds">>
    , <<"weather-few-clouds-night">>
    , <<"weather-fog">>
    , <<"weather-overcast">>
    , <<"weather-severe-alert">>
    , <<"weather-showers">>
    , <<"weather-showers-scattered">>
    , <<"weather-snow">>
    , <<"weather-storm">> ];

standard_icon_names(stock) ->
    [];

standard_icon_names(other) ->
    [];

standard_icon_names(_) ->
    error(badarg).


config_dirs() ->
    application:get_env(themes, dirs, []).


home_dir() ->
    case os:getenv("HOME") of
        false -> [];
        V -> [filename:join([V, ".icons"])]
    end.


xdg_data_dirs() ->
    case os:getenv("XDG_DATA_DIRS") of
        false -> [];
        Vs -> [filename:join([V, "icons"]) || V <- string:lexemes(Vs, [$:])]
    end.


themes() ->
    Acc = lists:foldl(fun themes/2, #{}, directories()),
    Themes = [{Id, Idx, Ds} || {Id, {Idx, Ds}} <- maps:to_list(Acc)
                             , Idx /= undefined],
    lists:keysort(1, Themes).


themes(Dir, Acc) ->
    case file:list_dir(Dir) of
        {ok, Ids} ->
            Vs0 = [{Id, IdDir} || Id <- Ids, begin
                                                 IdDir = filename:join([Dir,Id]),
                                                 filelib:is_dir(IdDir)
                                             end],
            lists:foldl(fun themes_add/2, Acc, Vs0);
        {error, _} ->
            Acc
    end.


themes_add({Name, Dir}, Acc) ->
    IdxFile = filename:join([Dir, "index.theme"]),
    Idx0 = case filelib:is_regular(IdxFile) of
               true -> IdxFile;
               false -> undefined
           end,
    case maps:get(Name, Acc, undefined) of
        undefined ->
            Acc#{Name => {Idx0, [Dir]}};
        {undefined, Dirs} ->
            Acc#{Name := {Idx0, Dirs ++ [Dir]}};
        {Idx1, Dirs} ->
            Acc#{Name := {Idx1, Dirs ++ [Dir]}}
    end.


load(Id, Themes) ->
    case lists:keytake(Id, 1, Themes) of
        {value, {_, Idx, Dirs}, Themes1} ->
            case themes_data:index(Id, Idx, fun (V) -> load(V, Themes1) end) of
                {ok, #theme{roots = Roots}=Theme} ->
                    {ok, Theme#theme{roots = Dirs ++ Roots}};
                Error ->
                    Error
            end;
        false ->
            {error, not_found}
    end.
