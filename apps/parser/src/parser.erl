-module(parser).
-include_lib("eunit/include/eunit.hrl").
-include("parser.hrl").

-export([parse/1]).

parse(<<Magic:32, MinorVersion:16, MajorVersion:16, ConstantPoolCount:16, Rest/binary>>) ->
    ConstantPool = constant_pool(ConstantPoolCount, Rest),
    #class_file{
        magic = Magic,
        minor_version = MinorVersion,
        major_version = MajorVersion,
        constant_pool = ConstantPool
    }.

constant_pool(0, _) ->
    [];
constant_pool(Count, Data) when Count > 0 ->
    {Item, Size} = constant_pool_item(Data),
    io:format("item: ~p, size: ~p~n", [Item, Size]),
    RemainingData = binary_part(Data, Size, byte_size(Data) - Size),
    [Item | constant_pool(Count - 1, RemainingData)].

constant_pool_item(<<Tag:8, Rest/binary>>) ->
    case Tag of
        10 -> method_ref(Rest);
        true -> exit(unknown_constantpool_tag)
    end.

method_ref(<<ClassIndex:16, NameAndTypeIndex:16, _/binary>>) ->
    {#method_ref{class_index = ClassIndex, name_and_type_index = NameAndTypeIndex}, 5}.
