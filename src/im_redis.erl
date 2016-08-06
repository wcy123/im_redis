-module(im_redis).
-author('wcy123@gmail.com').
-compile([{parse_transform, lager_transform}]).
-export([q/2,pq/2]).


is_block() ->
    true.
timeout() ->
    1000.

q(Name, Q) ->
    request(Name, fun eredis:q/2, [Q]).
pq(Name, PQ) ->
    request(Name, fun eredis:pq/2, [PQ]).

request(Name, Fun, Args) ->
    try
        Worker = poolboy:checkout(Name, is_block(), timeout()),
        im_alarm_handler:clear_alarm({Name, checkout}),
        maybe_do_work(Name, Worker, Fun, Args)
    catch
        Class:Reason ->
            im_alarm_handler:set_alarm({{Name, checkout}, {Class, Reason, erlang:get_stacktrace()}}),
            {error, {Class, Reason}}
    end.

maybe_do_work(Name, Pid, Fun, Args)
  when is_pid(Pid) ->
    im_alarm_handler:clear_alarm({Name, full}),
    do_work(Name, Pid, Fun, Args);
maybe_do_work(Name, full, _Fun, _Args) ->
    im_alarm_handler:set_alarm({{Name, full}, "please increase worker pool size"}),
    {error, full}.

do_work(Name, Pid, Fun, Args) ->
    try
        Ret = apply(Fun, [Pid | Args]),
        im_alarm_handler:clear_alarm({Name, do_work}),
        Ret
    catch
        Class:Reason ->
            im_alarm_handler:set_alarm({{Name, do_work}, {Class, Reason, erlang:get_stacktrace()}})
    after
        ok = poolboy:checkin(Name, Pid)
    end.
