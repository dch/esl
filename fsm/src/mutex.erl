-module(mutex).
-export([start/0, wait/0, signal/0, init/0,where/0]).

start() ->
    spawn(?MODULE, init, []).

signal() ->
    mutex ! {self(), Ref=make_ref(), release},
    receive
        {mutex, Ref, released} -> ok;
        _ -> {error, not_owner}
    end.

wait() ->
    mutex ! {self(), Ref=make_ref(), acquire},
    receive
        {mutex, Ref, acquired} -> ok
    end.

where() ->
    case whereis(mutex) of
        undefined -> start(), timer:sleep(1);
        _ -> ok
    end,
    mutex ! {self(), Ref=make_ref(), where},
    receive
        {mutex, Ref, Owner} -> Owner
    end.
init() ->
    process_flag(trap_exit, true),
    register(mutex, self()),
    loop(free).

loop(free) ->
    receive
        {From, Ref, where} ->
            From ! {mutex, Ref, free},
            loop(free);
        {From, Ref, acquire} ->
            link(From),
            From ! {mutex, Ref, acquired},
            loop({busy, From})
    end;
loop({busy, Owner}) ->
    receive
        {From, Ref, where} ->
            From ! {mutex, Ref, Owner},
        loop({busy, Owner});
        {Owner, Ref, release} ->
            unlink(Owner),
            Owner ! {mutex, Ref, released},
            loop(free);
        {'EXIT', Owner, _ } ->
            loop(free)
    end.

%% mutex:start() ⇒ ok.
%% mutex:wait() ⇒ ok.
%% mutex:signal() ⇒ ok.

