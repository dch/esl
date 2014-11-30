-module(db).
-include("db.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-spec test() -> term().
-endif.

%-opaque key() :: {atom(), atom()}.
%-opaque db() :: list( key()).
%-export_type([key/0, db/0]).
-export[ new/0,
         read/1,
         write/2,
         delete/1,
         match/1,
         destroy/0
       ].

%% db:new() ⇒ Db.
% -spec new() -> db().
new() ->
    ets:new(db, [named_table, {keypos, 2}]).

%% db:destroy(Db) ⇒ ok.
% -spec destroy(db()) -> ok.
destroy() ->
    case ets:delete(db) of
        true -> ok
    end.

%% db:read(Key, Db) ⇒ {ok, Element} | {error, instance}.
%-spec read(atom(), db()) -> {ok, atom()} | {error, instance}.
read(Key) -> case ets:lookup(db, Key) of
                 [] -> {error,instance};
                 [#data{key=Key, data=Value}] -> {ok, Value}
             end.

%% db:write(Key, Element, Db) ⇒ NewDb.
%-spec write(atom(), atom(), db()) -> db().
write(Key, Element) -> ets:insert(db, #data{key=Key, data=Element}).

%% db:delete(Key, Db) ⇒ NewDb.
%-spec delete(atom(), db()) -> db().
delete(Key) -> ets:delete(db, Key).

%% db:match(Element, Db) ⇒ [Key1, ..., KeyN].
%-spec match(atom(), db()) -> list(atom()).
match(Element) -> lists:flatten(ets:match(db, #data{key='$1', data=Element})).

-ifdef(TEST).
-endif.

