-module(parser_SUITE).
-export([all/0, mod_exists/1]).

all() ->
    [mod_exists].

mod_exists(_) ->
    parser:parse(ok).
