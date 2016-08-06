-module(im_redis).
-author('wcy123@gmail.com').
-compile([{parse_transform, lager_transform}]).
-export([q/2,qp/2, list2plist/1]).


is_block() ->
    true.
timeout() ->
    infinity.

q(Name, Q) ->
    request(Name, fun my_q/2, [Q]).
qp(Name, QP) ->
    request(Name, fun my_qp/2, [QP]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
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

my_q(Client, Q) ->
    try eredis:q(Client, Q) of
        {error, no_connection} ->
            lager:error("redis op: input = ~p, error reason:~p", [Q, no_connection]),
            {error, no_connection};
        {error, Reason} ->
            lager:error("redis op: input = ~p, error reason:~p", [Q, Reason]),
            {error, Reason};
        {ok, Value} ->
            lager:debug("redis op: ~p => ~p~n", [Q, Value]),
            {ok, Value}
    catch
        Class:Exception ->
            lager:debug("redis op ~p:~p input = ~w~n       stack = ~p~n",
                        [Class, Exception, erlang:get_stacktrace()]),
            {error, {Class, Exception}}
    end.

my_qp(Client, QP) ->
    try eredis:qp(Client, QP) of
        RetList when is_list(RetList) ->
            qp_check(QP, RetList),
            RetList;
        {error, no_connection} ->
            lager:error("redis op: input = ~w, error reason:~w", [QP, no_connection]),
            {error, no_connection};
        {error, Reason} ->
            lager:error("redis op: input = ~w, error reason:~w", [QP, Reason]),
            {error, Reason}
    catch
        Class:Exception ->
            lager:error("redis op ~p:~p input = ~w~n       stack = ~p~n",
                        [Class, Exception, erlang:get_stacktrace()]),
            {error, {Class, Exception}}
    end.

qp_check(QP, RetList) ->
    lists:foreach(
      fun({Q, Ret}) ->
              case Ret of
                  {error, no_connection} ->
                      lager:error("error reason:no_connection, input = ~p~n", [Q]),
                      {error, no_connection};
                  {error, Reason} ->
                      lager:error("error reason:~p, Q = ~p~n", [Q, Reason]),
                      {error, Reason};
                  {ok, Value} ->
                      lager:debug("redis op: ~p => ~p~n", [Q, Value]),
                      {ok, Value}
              end
      end,
      lists:zip(QP, RetList)).


list2plist(L) ->
    list2plist(L,[]).
list2plist([], Acc) ->
    lists:reverse(Acc);
list2plist([K], Acc) ->
    lists:reverse([{K,undefined} | Acc]);
list2plist([K,V|T], Acc) ->
    list2plist(T, [{K,V} | Acc]).
