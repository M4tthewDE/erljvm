-module(parser).
-include("parser.hrl").

-export([parse/1]).

parse(<<Magic:32, MinorVersion:16, MajorVersion:16, ConstantPoolCount:16, Data/binary>>) ->
    {ConstantPool, Data1} = pool:constant_pool(ConstantPoolCount, Data),
    {AccessFlags, Data2} = access_flags(Data1),
    {ThisClass, Data3} = this_class(Data2),
    {SuperClass, Data4} = super_class(Data3),
    {Interfaces, Data5} = interfaces(Data4),
    {Fields, _} = fields(ConstantPool, Data5),

    #class_file{
        magic = Magic,
        minor_version = MinorVersion,
        major_version = MajorVersion,
        constant_pool = ConstantPool,
        access_flags = AccessFlags,
        this_class = ThisClass,
        super_class = SuperClass,
        interfaces = Interfaces,
        fields = Fields
    }.

fields(ConstantPool, <<FieldsCount:16, Data/binary>>) ->
    fields(ConstantPool, FieldsCount, [], Data).

fields(_, 0, Items, Data) ->
    {lists:reverse(Items), Data};
fields(
    ConstantPool,
    Count,
    Items,
    <<AccessFlags:16, NameIndex:16, DescriptorIndex:16, AttributesCount:16, Data/binary>>
) ->
    {Attributes, Data1} = attributes:attributes(ConstantPool, AttributesCount, [], Data),
    Field = #field{
        access_flags = AccessFlags,
        name_index = NameIndex,
        descriptor_index = DescriptorIndex,
        attributes = Attributes
    },
    fields(ConstantPool, Count - 1, [Field | Items], Data1).

interfaces(<<InterfacesCount:16, Data/binary>>) ->
    interfaces(InterfacesCount, [], Data).

interfaces(0, Items, Data) ->
    {lists:reverse(Items), Data};
interfaces(Count, Items, <<Index:16, Data/binary>>) ->
    interfaces(Count - 1, [Index | Items], Data).

access_flags(<<AccessFlags:16, Data/binary>>) ->
    {AccessFlags, Data}.

this_class(<<ThisClass:16, Data/binary>>) ->
    {ThisClass, Data}.

super_class(<<SuperClass:16, Data/binary>>) ->
    {SuperClass, Data}.
