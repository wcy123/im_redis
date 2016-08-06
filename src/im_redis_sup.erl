-module(im_redis_sup).
-compile([{parse_transform, lager_transform}]).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    PoolNames = application:get_env(message_store, redis,[]),
    PropLists = lists:map(fun read_env/1, PoolNames),
    PoolSpecs = lists:map(fun plist_to_pool_spec/1,PropLists),
    {ok, { {one_for_one, 5, 10}, PoolSpecs} }.


read_env(Name) ->
    read_env_plist(Name, application:get_env(message_store, Name)).

read_env_plist(Name, {ok, PropList}) ->
    [{name, Name} | PropList];
read_env_plist(Name, _) ->
    lager:critical("application env ~p is not defined~n", [Name]).

plist_to_pool_spec(PropList) ->
    Name = proplists:get_value(name, PropList),
    PoolArgs = [{ name, {local, Name} },
                { worker_module, im_redis_client },
                { size , proplists:get_value(pool_size, PropList, 10)},
                { max_overflow , proplists:get_value(max_overflow, PropList, 20)}
               ],
    poolboy:child_spec(Name, PoolArgs, PropList).
