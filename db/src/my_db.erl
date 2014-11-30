-module(my_db).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-spec test() -> term().
-endif.

-export([
         start/0,
         init/0,
         stop/0
        ]).

-spec init() -> any().
init() -> loop(db:new()).

-spec start() -> ok | {error, already_started}.
start() ->
    case whereis(my_db) of
        undefined ->
            Pid = proc_lib:spawn(my_db, init, []),
            register(my_db, Pid),
            ok;
        Pid when is_pid(Pid) -> {error, already_started}
    end.

-spec stop() -> ok.
stop() ->
    case whereis(my_db) of
        undefined -> ok;
        Pid when is_pid(Pid) -> my_db ! stop,
                                ok
    end.

%% Internal

-spec return(reference()) -> any().
return(Ref) ->
    receive {Ref, Response} -> Response end.

-spec loop(any()) -> ok.
loop(Db) -> receive
                stop -> ok;
                {write, K, V} ->
                    loop(db:write(K, V, Db));
                {From, Ref, read, K} ->
                    From ! {Ref, db:read(K, Db)},
                    loop(Db);
                {From, Ref, match, V} ->
                    From ! {Ref, db:match(V, Db)},
                    loop(Db);
                {delete, K} ->
                    loop(db:delete(K, Db)),
                    loop(Db)
            end.

%% Tests

-ifdef(TEST).
%% my_db:start() ⇒ ok.
start_test() ->
    my_db:stop(),
    my_db:start(),
    ?assertEqual(true, is_pid(whereis(my_db))).

%% my_db:stop() ⇒ ok.
stop_test() ->
    my_db:start(),
    ?assertEqual(ok, my_db:stop()),
    ?assertEqual(false, is_pid(whereis(my_db))).

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

