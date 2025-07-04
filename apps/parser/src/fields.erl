-module(fields).
-include("parser.hrl").

-export([fields/2]).

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
