-module(attributes).
-include("parser.hrl").
-export([attributes/2]).

attributes(
    ConstantPool, <<AttributesCount:16, Data/binary>>
) ->
    attributes(ConstantPool, AttributesCount, [], Data).

attributes(_, 0, Items, Data) ->
    {lists:reverse(Items), Data};
attributes(
    ConstantPool, Count, Attributes, <<AttributeNameIndex:16, _:32, Data/binary>>
) ->
    {Attribute, RemainingData} = attribute(
        ConstantPool, pool:utf8(ConstantPool, AttributeNameIndex), Data
    ),
    attributes(ConstantPool, Count - 1, [Attribute | Attributes], RemainingData).

attribute(ConstantPool, Name, Data) ->
    {Attribute, RemainingData} =
        case Name of
            <<"ConstantValue">> -> constant_value(Data);
            <<"RuntimeVisibleAnnotations">> -> runtime_visible_annotations(Data);
            <<"Code">> -> code(ConstantPool, Data);
            <<"LineNumberTable">> -> line_number_table(Data);
            <<"LocalVariableTable">> -> local_variable_table(Data);
            true -> exit(unknown_attribute_name)
        end,
    {Attribute, RemainingData}.

constant_value(<<ConstantValueIndex:16, Data/binary>>) ->
    {#constant_value{constant_value_index = ConstantValueIndex}, Data}.

runtime_visible_annotations(<<NumAnnotations:16, Data/binary>>) ->
    {Annotations, RemainingData} = annotations(NumAnnotations, [], Data),
    {#runtime_visible_annotations{annotations = Annotations}, RemainingData}.

annotations(0, Items, Data) ->
    {lists:reverse(Items), Data};
annotations(Count, Items, Data) ->
    {Annotation, RemainingData} = annotation(Data),
    annotations(Count - 1, [Annotation | Items], RemainingData).

annotation(<<TypeIndex:16, NumElementValuePairs:16, Data/binary>>) ->
    {ElementValuePairs, RemainingData} = element_value_pairs(NumElementValuePairs, [], Data),
    {#annotation{type_index = TypeIndex, element_value_pairs = ElementValuePairs}, RemainingData}.

element_value_pairs(0, Items, Data) ->
    {lists:reverse(Items), Data};
element_value_pairs(Count, Items, Data) ->
    {ElementValuePair, RemainingData} = element_value_pair(Data),
    element_value_pairs(Count - 1, [ElementValuePair | Items], RemainingData).

element_value_pair(<<_:16, Tag:8, _/binary>>) ->
    case Tag of
        true -> exit(unknown_element_value_tag)
    end.

code(
    ConstantPool,
    <<MaxStack:16, MaxLocals:16, CodeLength:32, Data/binary>>
) ->
    ct:pal("CodeLength: ~p~n", [CodeLength]),
    Code = binary_part(Data, 0, CodeLength),
    {ExceptionTable, RemainingData} = exception_table(
        binary_part(Data, CodeLength, byte_size(Data) - CodeLength)
    ),
    {Attributes, RemainingData1} = attributes(ConstantPool, RemainingData),
    {
        #code{
            max_stack = MaxStack,
            max_locals = MaxLocals,
            code = Code,
            exception_table = ExceptionTable,
            attributes = Attributes
        },
        RemainingData1
    }.

exception_table(<<ExceptionTableLength:16, Data/binary>>) ->
    exception_table(ExceptionTableLength, [], Data).

exception_table(0, Exceptions, Data) ->
    {lists:reverse(Exceptions), Data};
exception_table(
    Count, Exceptions, <<StartPc:16, EndPc:16, HandlerPc:16, CatchType:16, RemainingData/binary>>
) ->
    exception_table(
        Count - 1,
        [
            #exception_table_item{
                start_pc = StartPc, end_pc = EndPc, handler_pc = HandlerPc, catch_type = CatchType
            }
            | Exceptions
        ],
        RemainingData
    ).

line_number_table(0, LineNumberTable, Data) ->
    {lists:reverse(LineNumberTable), Data};
line_number_table(Count, LineNumberTable, <<StartPc:16, LineNumber:16, Data/binary>>) ->
    line_number_table(
        Count - 1,
        [#line_number_table_entry{start_pc = StartPc, line_number = LineNumber} | LineNumberTable],
        Data
    ).

line_number_table(<<LineNumberTableLength:16, Data/binary>>) ->
    line_number_table(LineNumberTableLength, [], Data).

local_variable_table(<<LocalVariableTableLength:16, Data/binary>>) ->
    local_variable_table(LocalVariableTableLength, [], Data).

local_variable_table(0, LocalVariableTable, Data) ->
    {lists:reverse(LocalVariableTable), Data};
local_variable_table(
    Count,
    LocalVariableTable,
    <<StartPc:16, Length:16, NameIndex:16, DescriptorIndex:16, Index:16, Data/binary>>
) ->
    local_variable_table(
        Count - 1,
        [
            #local_variable_table_entry{
                start_pc = StartPc,
                length = Length,
                name_index = NameIndex,
                descriptor_index = DescriptorIndex,
                index = Index
            }
            | LocalVariableTable
        ],
        Data
    ).
