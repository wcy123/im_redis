-module(im_redis).
-author('wcy123@gmail.com').
-compile([{parse_transform, lager_transform}]).
-export([q/2,pq/2]).


is_block() ->
    true.
timeout() ->
    infinity.

q(Name, Q) ->
    request(Name, fun eredis:q/2, [Q]).
pq(Name, PQ) ->
    request(Name, fun eredis:pq/2, [PQ]).

request(Name, Fun, Args) ->
    try
        Worker = poolboy:checkout(Name, is_block(), timeout()),
        maybe_do_work(Name, Worker, Fun, Args)
    catch
        Class:Reason ->
            {error, {Class, Reason}}
    end.

maybe_do_work(Name, Pid, Fun, Args)
  when is_pid(Pid) ->
    do_work(Name, Pid, Fun, Args);
maybe_do_work(_Name, full, _Fun, _Args) ->
    {error, full}.

do_work(Name, Pid, Fun, Args) ->
    try
        apply(Fun, [Pid | Args])
    catch
        Class:Reason ->
            {error, {Class, Reason}}
    after
        ok = poolboy:checkin(Name, Pid)
    end.
