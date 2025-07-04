-record(class_file, {
    magic,
    minor_version,
    major_version,
    constant_pool,
    access_flags,
    this_class,
    super_class,
    interfaces,
    fields,
    methods
}).
-record(field, {access_flags, name_index, descriptor_index, attributes}).
-record(method_ref_pool_item, {class_index, name_and_type_index}).
-record(class_pool_item, {name_index}).
-record(name_and_type_pool_item, {name_index, descriptor_index}).
-record(utf8_pool_item, {bytes}).
-record(field_ref_pool_item, {class_index, name_and_type_index}).
-record(interface_method_ref_pool_item, {class_index, name_and_type_index}).
-record(string_pool_item, {string_index}).
-record(invoke_dynamic_pool_item, {bootstrap_method_attr_index, name_and_type_index}).
-record(integer_pool_item, {bytes}).
-record(method_handle_pool_item, {reference_kind, reference_index}).
-record(method_type_pool_item, {descriptor_index}).
-record(constant_value, {constant_value_index}).
-record(runtime_visible_annotations, {annotations}).
-record(annotation, {type_index, element_value_pairs}).
-record(method, {access_flags, name_index, descriptor_index, attributes}).
-record(exception_table_item, {start_pc, end_pc, handler_pc, catch_type}).
-record(code, {max_stack, max_locals, code, exception_table, attributes}).
