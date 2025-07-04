-module(parser).
-include("parser.hrl").

-export([parse/1]).

parse(<<Magic:32, MinorVersion:16, MajorVersion:16, ConstantPoolCount:16, Data/binary>>) ->
    {ConstantPool, Data1} = pool:constant_pool(ConstantPoolCount, Data),
    {AccessFlags, Data2} = access_flags(Data1),
    {ThisClass, Data3} = this_class(Data2),
    {SuperClass, Data4} = super_class(Data3),
    {Interfaces, Data5} = interfaces:interfaces(Data4),
    {Fields, _} = fields:fields(ConstantPool, Data5),

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

access_flags(<<AccessFlags:16, Data/binary>>) ->
    {AccessFlags, Data}.

this_class(<<ThisClass:16, Data/binary>>) ->
    {ThisClass, Data}.

super_class(<<SuperClass:16, Data/binary>>) ->
    {SuperClass, Data}.
