-module(snp_registry).

-export([registry/2]).

-import(wol_strings, [template/3]).

registry(Mod, Id) ->
    list_to_atom(template("~s-~s", [Mod, Id], [{return, list}])).
