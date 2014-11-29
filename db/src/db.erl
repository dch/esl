-module(db).

-opaque item() :: {atom(), atom()}.
-opaque db() :: list( item()).
-export_type([item/0, db/0]).
-export[
        new/0
       ].

-spec new() -> db().
new() -> [].


%% db:new() ⇒ Db.
%% db:destroy(Db) ⇒ ok.
%% db:write(Key, Element, Db) ⇒ NewDb. db:delete(Key, Db) ⇒ NewDb.
%% db:read(Key, Db) ⇒{ok, Element} | {error, instance}. db:match(Element, Db) ⇒
%% [Key1, ..., KeyN].
