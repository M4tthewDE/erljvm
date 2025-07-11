-module(pool).
-include("parser.hrl").

-export([constant_pool/2, utf8/2]).

constant_pool(Count, Data) ->
    constant_pool(Count, [], Data).

% constant pool is 1 indexed!
constant_pool(1, Pool, Data) ->
    {lists:reverse(Pool), Data};
constant_pool(Count, Pool, Data) when Count > 1 ->
    {Item, RemainingData} = constant_pool_item(Data),
    constant_pool(Count - 1, [Item | Pool], RemainingData).

constant_pool_item(<<Tag:8, Data/binary>>) ->
    case Tag of
        1 -> utf8_pool_item(Data);
        3 -> integer_pool_item(Data);
        7 -> class_pool_item(Data);
        8 -> string_pool_item(Data);
        9 -> field_ref_pool_item(Data);
        10 -> method_ref_pool_item(Data);
        11 -> interface_method_ref_pool_item(Data);
        12 -> name_and_type_pool_item(Data);
        15 -> method_handle_pool_item(Data);
        16 -> method_type_pool_item(Data);
        18 -> invoke_dynamic_pool_item(Data);
        true -> exit(unknown_constantpool_tag)
    end.

method_ref_pool_item(<<ClassIndex:16, NameAndTypeIndex:16, Data/binary>>) ->
    {#method_ref_pool_item{class_index = ClassIndex, name_and_type_index = NameAndTypeIndex}, Data}.

class_pool_item(<<NameIndex:16, Data/binary>>) ->
    {#class_pool_item{name_index = NameIndex}, Data}.

name_and_type_pool_item(<<NameIndex:16, DescriptorIndex:16, Data/binary>>) ->
    {#name_and_type_pool_item{name_index = NameIndex, descriptor_index = DescriptorIndex}, Data}.

utf8_pool_item(<<Length:16, Data/binary>>) ->
    {
        #utf8_pool_item{bytes = binary_part(Data, 0, Length)},
        binary_part(Data, Length, byte_size(Data) - Length)
    }.

field_ref_pool_item(<<ClassIndex:16, NameAndTypeIndex:16, Data/binary>>) ->
    {#field_ref_pool_item{class_index = ClassIndex, name_and_type_index = NameAndTypeIndex}, Data}.

interface_method_ref_pool_item(<<ClassIndex:16, NameAndTypeIndex:16, Data/binary>>) ->
    {
        #interface_method_ref_pool_item{
            class_index = ClassIndex, name_and_type_index = NameAndTypeIndex
        },
        Data
    }.

string_pool_item(<<StringIndex:16, Data/binary>>) ->
    {#string_pool_item{string_index = StringIndex}, Data}.

invoke_dynamic_pool_item(<<BootstrapMethodAttrIndex:16, NameAndTypeIndex:16, Data/binary>>) ->
    {
        #invoke_dynamic_pool_item{
            bootstrap_method_attr_index = BootstrapMethodAttrIndex,
            name_and_type_index = NameAndTypeIndex
        },
        Data
    }.

integer_pool_item(<<Bytes:32, Data/binary>>) ->
    {#integer_pool_item{bytes = Bytes}, Data}.

method_handle_pool_item(<<ReferenceKind:8, ReferenceIndex:16, Data/binary>>) ->
    {
        #method_handle_pool_item{reference_kind = ReferenceKind, reference_index = ReferenceIndex},
        Data
    }.

method_type_pool_item(<<DescriptorIndex:16, Data/binary>>) ->
    {#method_type_pool_item{descriptor_index = DescriptorIndex}, Data}.

utf8(ConstantPool, Index) ->
    Item = lists:nth(Index, ConstantPool),
    Item#utf8_pool_item.bytes.
