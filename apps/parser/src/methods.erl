-module(methods).
-include("parser.hrl").
-export([methods/2]).

methods(ConstantPool, <<MethodsCount:16, Data/binary>>) ->
    methods(ConstantPool, MethodsCount, [], Data).

methods(_, 0, Methods, Data) ->
    {lists:reverse(Methods), Data};
methods(ConstantPool, Count, Methods, Data) ->
    {Method, RemainingData} = method(ConstantPool, Data),
    methods(ConstantPool, Count - 1, [Method | Methods], RemainingData).

method(
    ConstantPool,
    <<AccessFlags:16, NameIndex:16, DescriptorIndex:16, Data/binary>>
) ->
    {Attributes, RemainingData} = attributes:attributes(ConstantPool, Data),
    {
        #method{
            access_flags = AccessFlags,
            name_index = NameIndex,
            descriptor_index = DescriptorIndex,
            attributes = Attributes
        },
        RemainingData
    }.
