-module(parser_SUITE).
-export([all/0, parse_system_class/1]).

all() ->
    [parse_system_class].

parse_system_class(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Data =
        case file:read_file(DataDir ++ "java.base/java/lang/System.class") of
            {ok, Binary} -> Binary;
            {error, Reason} -> ct:fail("Failed reading System.class: ~p", [Reason])
        end,

    parser:parse(Data).
