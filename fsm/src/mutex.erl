-module(mutex).
-export([start/0, wait/0, signal/0, init/0]).

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

init() ->
    register(mutex, self()),
    loop(free).

loop(free) ->
    Owner = receive {From, Ref, acquire} ->
                        From ! {mutex, Ref, acquired},
                        From
            end,
    loop({busy, Owner});
loop({busy, Owner}) ->
    receive {Owner, Ref, release} ->
                Owner ! {mutex, Ref, released}
    end,
    loop(free).

%% mutex:start() ⇒ ok.
%% mutex:wait() ⇒ ok.
%% mutex:signal() ⇒ ok.

