-module(parser).
-include_lib("eunit/include/eunit.hrl").
-include("parser.hrl").

-export([parse/1]).

parse(<<Magic:32, MinorVersion:16, MajorVersion:16, ConstantPoolCount:16, Data/binary>>) ->
    ConstantPool = constant_pool(ConstantPoolCount, Data),
    #class_file{
        magic = Magic,
        minor_version = MinorVersion,
        major_version = MajorVersion,
        constant_pool = ConstantPool
    }.

% constant pool is 1 indexed!
constant_pool(1, _) ->
    [];
constant_pool(Count, Data) when Count > 1 ->
    {Item, RemainingData} = constant_pool_item(Data),
    [Item | constant_pool(Count - 1, RemainingData)].

constant_pool_item(<<Tag:8, Rest/binary>>) ->
    case Tag of
        1 -> utf8_pool_item(Rest);
        3 -> integer_pool_item(Rest);
        7 -> class_pool_item(Rest);
        8 -> string_pool_item(Rest);
        9 -> field_ref_pool_item(Rest);
        10 -> method_ref_pool_item(Rest);
        11 -> interface_method_ref_pool_item(Rest);
        12 -> name_and_type_pool_item(Rest);
        15 -> method_handle_pool_item(Rest);
        16 -> method_type_pool_item(Rest);
        18 -> invoke_dynamic_pool_item(Rest);
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
