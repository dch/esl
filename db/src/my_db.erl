-module(my_db).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-spec test() -> term().
-endif.

-ifdef(TEST).
%% my_db:stop() ⇒ ok.
stop_test() ->
    my_db:start(),
    ?assertEqual(ok, my_db:stop()).

%% my_db:start() ⇒ ok.
start_test() ->
    ?assertEqual(ok, my_db:start()).

%% my_db:write(Key, Element) ⇒ ok.
write_test() ->
    ?assertEqual(ok, my_db:write(key, element)).

%% my_db:delete(Key) ⇒ ok.
delete_test() ->
    my_db:write(key, element),
    ?assertEqual(ok, my_db:delete(key)).

%% my_db:read(Key) ⇒ {ok, Element} | {error, instance}.
read_existing_key_test() ->
    my_db:write(key, read),
    ?assertEqual({ok, read}, my_db:read(key)).
read_missing_key_test() ->
    ?assertEqual({error, instance}, my_db:read(missing_key)).

%% my_db:match(Element) ⇒ [Key1, ..., KeyN].
match_multiple_keys_test() ->
    my_db:write(beer, tasty),
    my_db:write(schnitzel, tasty),
    my_db:write(würstel, tasty),
    ?assertEqual([beer, schnitzel, würstel],
                 my_db_match(tasty)).
-endif.

