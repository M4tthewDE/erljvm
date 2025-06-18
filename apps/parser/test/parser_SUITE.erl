-module(parser_SUITE).
-export([all/0, parse_system_class/1]).
-include_lib("eunit/include/eunit.hrl").
-include("parser.hrl").

all() ->
    [parse_system_class].

parse_system_class(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Data =
        case file:read_file(DataDir ++ "java.base/java/lang/System.class") of
            {ok, Binary} -> Binary;
            {error, Reason} -> ct:fail("Failed reading System.class: ~p", [Reason])
        end,

    ClassFile = parser:parse(Data),
    ?assertEqual(16#CAFEBABE, ClassFile#class_file.magic),
    ?assertEqual(0, ClassFile#class_file.minor_version),
    ?assertEqual(61, ClassFile#class_file.major_version),
    ?assertEqual(802, length(ClassFile#class_file.constant_pool)).
