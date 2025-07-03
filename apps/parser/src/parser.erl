-module(parser).
-include("parser.hrl").

-export([parse/1]).

parse(<<Magic:32, MinorVersion:16, MajorVersion:16, ConstantPoolCount:16, Data/binary>>) ->
    {ConstantPool, Data1} = constant_pool(ConstantPoolCount, Data),
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

utf8(ConstantPool, Index) ->
    Item = lists:nth(Index, ConstantPool),
    Item#utf8_pool_item.bytes.

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
    {Attributes, Data1} = attributes(ConstantPool, AttributesCount, [], Data),
    Field = #field{
        access_flags = AccessFlags,
        name_index = NameIndex,
        descriptor_index = DescriptorIndex,
        attributes = Attributes
    },
    fields(ConstantPool, Count - 1, [Field | Items], Data1).

attributes(_, 0, Items, Data) ->
    {lists:reverse(Items), Data};
attributes(
    ConstantPool, Count, Attributes, <<AttributeNameIndex:16, _:32, Data/binary>>
) ->
    {Attribute, RemainingData} = attribute(utf8(ConstantPool, AttributeNameIndex), Data),
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

constant_pool(Count, Data) ->
    constant_pool(Count, [], Data).

% constant pool is 1 indexed!
constant_pool(1, Items, Data) ->
    {lists:reverse(Items), Data};
constant_pool(Count, Items, Data) when Count > 1 ->
    {Item, RemainingData} = constant_pool_item(Data),
    constant_pool(Count - 1, [Item | Items], RemainingData).

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
