-module(interfaces).

-export([interfaces/1]).

interfaces(<<InterfacesCount:16, Data/binary>>) ->
    interfaces(InterfacesCount, [], Data).

interfaces(0, Interfaces, Data) ->
    {lists:reverse(Interfaces), Data};
interfaces(Count, Interfaces, <<Index:16, Data/binary>>) ->
    interfaces(Count - 1, [Index | Interfaces], Data).
