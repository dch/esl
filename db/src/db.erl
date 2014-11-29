-module(db).

-opaque item() :: {atom(), atom()}.
-opaque db() :: list( item()).
-export_type([item/0, db/0]).
-export[ new/0,
         read/2,
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
%% [Key1, ..., KeyN].
