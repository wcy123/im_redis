-module(benchemark_im_redis).
-compile([export_all]).


start() ->
    Length = application:get_env(im_redis, benchmark_pq_size, 1000),
    ListRefs =
        lists:map(
          fun(I) ->
                  PID = spawn(?MODULE, do_work, [self(), I]),
                  monitor(process, PID)
          end, lists:seq(1,Length)),
    wait_for(ListRefs, []).

do_work(_Pid, I) ->
    X = lists:foreach(
          fun(N) ->
                  Q = [set
                      , io_lib:format("~p_~p",[I,N])
                      , io_lib:format("this is ~p~n",[I])],
                  Result =
                      case application:get_env(im_redis, solution) of
                          {ok, cuesport} ->
                              Worker = cuesport:get_worker(index),
                              eredis:q(Worker, Q);
                          {ok, im_redis} ->
                              im_redis:q(index, Q)
                      end,
                  case Result of
                      {ok, _Anything} -> ok;
                      Error ->
                          io:format("error ~p -> ~p~n", [ Q, Error ])
                  end
          end,
          lists:seq(1,application:get_env(im_redis, benchmark_length, 4000))),
    exit(X).

wait_for([], Acc) ->
    Acc;
wait_for(ListRefs, Acc) ->
    receive
        {'DOWN', Ref, process, _Pid, Reason} ->
            wait_for(ListRefs -- [Ref], [Reason | Acc])
    end.
