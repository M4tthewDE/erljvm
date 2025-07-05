-module(fields).
-include("parser.hrl").

-export([fields/2]).

fields(ConstantPool, <<FieldsCount:16, Data/binary>>) ->
    fields(ConstantPool, FieldsCount, [], Data).

fields(_, 0, Fields, Data) ->
    {lists:reverse(Fields), Data};
fields(
    ConstantPool,
    Count,
    Fields,
    <<AccessFlags:16, NameIndex:16, DescriptorIndex:16, Data/binary>>
) ->
    {Attributes, Data1} = attributes:attributes(ConstantPool, Data),
    Field = #field{
        access_flags = AccessFlags,
        name_index = NameIndex,
        descriptor_index = DescriptorIndex,
        attributes = Attributes
    },
    fields(ConstantPool, Count - 1, [Field | Fields], Data1).
