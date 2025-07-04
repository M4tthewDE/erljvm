-module(attributes).
-include("parser.hrl").
-export([attributes/4]).

attributes(_, 0, Items, Data) ->
    {lists:reverse(Items), Data};
attributes(
    ConstantPool, Count, Attributes, <<AttributeNameIndex:16, _:32, Data/binary>>
) ->
    {Attribute, RemainingData} = attribute(pool:utf8(ConstantPool, AttributeNameIndex), Data),
    attributes(ConstantPool, Count - 1, [Attribute | Attributes], RemainingData).

attribute(Name, Data) ->
    {Attribute, RemainingData} =
        case Name of
            <<"ConstantValue">> -> constant_value(Data);
            <<"RuntimeVisibleAnnotations">> -> runtime_visible_annotations(Data);
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
