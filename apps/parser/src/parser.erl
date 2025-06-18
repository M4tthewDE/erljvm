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
        1 -> utf8_pool_item(Rest);
        7 -> class_pool_item(Rest);
        10 -> method_ref_pool_item(Rest);
        12 -> name_and_type_pool_item(Rest);
        true -> exit(unknown_constantpool_tag)
    end.

method_ref_pool_item(<<ClassIndex:16, NameAndTypeIndex:16, _/binary>>) ->
    {#method_ref_pool_item{class_index = ClassIndex, name_and_type_index = NameAndTypeIndex}, 5}.

class_pool_item(<<NameIndex:16, _/binary>>) ->
    {#class_pool_item{name_index = NameIndex}, 3}.

name_and_type_pool_item(<<NameIndex:16, DescriptorIndex:16, _/binary>>) ->
    {#name_and_type_pool_item{name_index = NameIndex, descriptor_index = DescriptorIndex}, 5}.

utf8_pool_item(<<Length:16, Rest/binary>>) ->
    {#utf8_pool_item{bytes = binary_part(Rest, 0, Length)}, Length + 3}.
