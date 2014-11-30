-module(db).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-spec test() -> term().
-endif.

-opaque key() :: {atom(), atom()}.
-opaque db() :: list( key()).
-export_type([key/0, db/0]).
-export[ new/0,
         read/2,
         write/3,
         delete/2,
         match/2,
         destroy/1
       ].

%% db:new() ⇒ Db.
-spec new() -> db().
new() -> [].

%% db:destroy(Db) ⇒ ok.
-spec destroy(db()) -> ok.
destroy(_) -> ok.

%% db:read(Key, Db) ⇒ {ok, Element} | {error, instance}.
-spec read(atom(), db()) -> {ok, atom()} | {error, instance}.
read(_Key, []) -> {error,instance}; % end of list and no key found
read(Key, [{Key, Element} | _Rest]) -> {ok, Element}; % matched, return element
read(Key, [_ | Rest]) -> read(Key, Rest).

%% db:write(Key, Element, Db) ⇒ NewDb.
-spec write(atom(), atom(), db()) -> db().
write(Key, Element, []) -> [{Key, Element}];
write(Key, Element, [{Key, _ } | Rest]) ->
    [{Key, Element} | Rest];
write(Key, Element, [ Head | Rest]) ->
    [Head | write(Key, Element, Rest)].

%% db:delete(Key, Db) ⇒ NewDb.
-spec delete(atom(), db()) -> db().
delete(_, []) -> [];
delete(Key, [{Key, _ } | Rest]) -> Rest;
delete(Key, [ Head | Rest]) ->
    [Head | delete(Key, Rest)].

%% db:match(Element, Db) ⇒ [Key1, ..., KeyN].
-spec match(atom(), db()) -> list(atom()).
match(_, []) -> [];
match(Element, [{Key, Element } | Rest]) -> [ Key | match(Element, Rest)];
match(Element, [ _ | Rest]) -> match(Element, Rest).

-ifdef(TEST).
new_test() ->
    ?assertEqual([], new()).

write_to_empty_db_test() ->
    ?assertEqual([{francesco,london}], write(francesco,london, [])).

write_to_existing_db_test() ->
    ?assertEqual([{francesco,london}, {lelle, stockholm}],
                 write(lelle, stockholm, [{francesco,london}])).

read_existing_key_from_db_test() ->
    ?assertEqual({ok, london}, read(francesco, [{lelle, stockholm},
                                                {francesco,london}])).

read_missing_key_from_db_test() ->
    ?assertEqual({error, instance}, read(ola,[{lelle, stockholm},
                                              {francesco,london}])).

match_multiple_keys_test() ->
    ?assertEqual([joern, lelle],
                 match(stockholm,
                       [{joern,stockholm},{lelle,stockholm},{francesco,london}])).

delete_existing_key_test() ->
   ?assertEqual([{joern,stockholm},{francesco,london}],
    delete(lelle, [{joern,stockholm},{lelle,stockholm},{francesco,london}])).

match_after_deleting_key_test() ->
    ?assertEqual([joern],
                 match(stockholm, [{joern,stockholm},{francesco,london}])).
-endif.

