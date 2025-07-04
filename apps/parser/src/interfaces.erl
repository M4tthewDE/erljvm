-module(interfaces).

-export([interfaces/1]).

interfaces(<<InterfacesCount:16, Data/binary>>) ->
    interfaces(InterfacesCount, [], Data).

interfaces(0, Items, Data) ->
    {lists:reverse(Items), Data};
interfaces(Count, Items, <<Index:16, Data/binary>>) ->
    interfaces(Count - 1, [Index | Items], Data).
