-module(db).

-opaque item() :: {atom(), atom()}.
-opaque db() :: list( item()).
-export_type([item/0, db/0]).
-export[ new/0,
         read/2,
         write/3,
         delete/2,
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


%% db:match(Element, Db) ⇒
%% [Key1, ..., KeyN].
