-module(attributes).
-include("parser.hrl").
-export([attributes/2]).

attributes(
    ConstantPool, <<AttributesCount:16, Data/binary>>
) ->
    attributes(ConstantPool, AttributesCount, [], Data).

attributes(_, 0, Attributes, Data) ->
    {lists:reverse(Attributes), Data};
attributes(
    ConstantPool, Count, Attributes, <<AttributeNameIndex:16, _:32, Data/binary>>
) ->
    AttributeName = pool:utf8(ConstantPool, AttributeNameIndex),
    {Attribute, RemainingData} = attribute(ConstantPool, AttributeName, Data),
    attributes(ConstantPool, Count - 1, [Attribute | Attributes], RemainingData).

attribute(ConstantPool, Name, Data) ->
    case Name of
        <<"ConstantValue">> -> constant_value(Data);
        <<"RuntimeVisibleAnnotations">> -> runtime_visible_annotations(Data);
        <<"Code">> -> code(ConstantPool, Data);
        <<"LineNumberTable">> -> line_number_table(Data);
        <<"LocalVariableTable">> -> local_variable_table(Data);
        true -> exit(unknown_attribute_name)
    end.

constant_value(<<ConstantValueIndex:16, Data/binary>>) ->
    {#constant_value{constant_value_index = ConstantValueIndex}, Data}.

runtime_visible_annotations(<<NumAnnotations:16, Data/binary>>) ->
    {Annotations, RemainingData} = annotations(NumAnnotations, [], Data),
    {#runtime_visible_annotations{annotations = Annotations}, RemainingData}.

annotations(0, Annotations, Data) ->
    {lists:reverse(Annotations), Data};
annotations(Count, Annotations, Data) ->
    {Annotation, RemainingData} = annotation(Data),
    annotations(Count - 1, [Annotation | Annotations], RemainingData).

annotation(<<TypeIndex:16, NumElementValuePairs:16, Data/binary>>) ->
    {ElementValuePairs, RemainingData} = element_value_pairs(NumElementValuePairs, [], Data),
    {#annotation{type_index = TypeIndex, element_value_pairs = ElementValuePairs}, RemainingData}.

element_value_pairs(0, Pairs, Data) ->
    {lists:reverse(Pairs), Data};
element_value_pairs(Count, Pairs, Data) ->
    {Pair, RemainingData} = element_value_pair(Data),
    element_value_pairs(Count - 1, [Pair | Pairs], RemainingData).

element_value_pair(<<_:16, Tag:8, _/binary>>) ->
    case Tag of
        true -> exit(unknown_element_value_tag)
    end.

code(
    ConstantPool,
    <<MaxStack:16, MaxLocals:16, CodeLength:32, Data/binary>>
) ->
    Code = binary_part(Data, 0, CodeLength),
    Data1 = binary_part(Data, CodeLength, byte_size(Data) - CodeLength),
    {ExceptionTable, Data2} = exception_table(Data1),
    {Attributes, Data3} = attributes(ConstantPool, Data2),
    CodeAttribute = #code{
        max_stack = MaxStack,
        max_locals = MaxLocals,
        code = Code,
        exception_table = ExceptionTable,
        attributes = Attributes
    },
    {CodeAttribute, Data3}.

exception_table(<<ExceptionTableLength:16, Data/binary>>) ->
    exception_table(ExceptionTableLength, [], Data).

exception_table(0, Exceptions, Data) ->
    {lists:reverse(Exceptions), Data};
exception_table(
    Count, Exceptions, <<StartPc:16, EndPc:16, HandlerPc:16, CatchType:16, RemainingData/binary>>
) ->
    Entry = #exception_table_entry{
        start_pc = StartPc, end_pc = EndPc, handler_pc = HandlerPc, catch_type = CatchType
    },
    exception_table(Count - 1, [Entry | Exceptions], RemainingData).

line_number_table(<<LineNumberTableLength:16, Data/binary>>) ->
    line_number_table(LineNumberTableLength, [], Data).

line_number_table(0, LineNumberTable, Data) ->
    {lists:reverse(LineNumberTable), Data};
line_number_table(Count, LineNumberTable, <<StartPc:16, LineNumber:16, Data/binary>>) ->
    Entry = #line_number_table_entry{start_pc = StartPc, line_number = LineNumber},
    line_number_table(Count - 1, [Entry | LineNumberTable], Data).

local_variable_table(<<Length:16, Data/binary>>) ->
    local_variable_table(Length, [], Data).

local_variable_table(0, LocalVariableTable, Data) ->
    {lists:reverse(LocalVariableTable), Data};
local_variable_table(
    Count,
    LocalVariableTable,
    <<StartPc:16, Length:16, NameIndex:16, DescriptorIndex:16, Index:16, Data/binary>>
) ->
    Entry = #local_variable_table_entry{
        start_pc = StartPc,
        length = Length,
        name_index = NameIndex,
        descriptor_index = DescriptorIndex,
        index = Index
    },
    local_variable_table(Count - 1, [Entry | LocalVariableTable], Data).
