-module(brod_SUITE).

-export([ init_per_suite/1
        , end_per_suite/1
        , all/0
        , suite/0
        ]).

-export([ t_create_topics/1
        , t_describe_configs_before_alter/1
        , t_alter_configs/1
        , t_describe_configs_after_alter/1
        , t_delete_topics/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl"). % Eunit macros for convenience

-type config() :: [{atom(), term()}].

-define(HOSTS, [{"localhost", 9092}]).
-define(TIMEOUT, 280000).

-define(TOPIC, <<"test-topic">>).
-define(RESOURCE_TYPE_TOPIC, 2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  Config.

-spec all() -> [atom()].
-ifdef(CURRENT_DEPS).
all() -> [F || {F, _A} <- module_info(exports),
               case atom_to_list(F) of
                 "t_" ++ _ -> true;
                 _         -> false
               end].
-else.
all() -> [].
-endif.

-spec suite() -> [atom()].
suite() -> [{timetrap, {minutes, 5}}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t_create_topics(Config) when is_list(Config) ->
  TopicConfig = [
                 #{
                   config_entries => [
                                      [ {config_name, "cleanup.policy"}
                                      , {config_value, <<"compact">>}]
                                     ]
                  , num_partitions => 1
                  , replica_assignment => []
                  , replication_factor => 1
                  , topic => ?TOPIC
                  }
                ],
  ?assertEqual(ok,
               brod:create_topics(?HOSTS, TopicConfig, #{timeout => ?TIMEOUT},
                                  #{connect_timeout => ?TIMEOUT})).


t_describe_configs_before_alter(Config) when is_list(Config) ->
  DescribeConfigsArgs =
    #{ resource_type => ?RESOURCE_TYPE_TOPIC
     , resource_name => ?TOPIC
     , config_names => ["cleanup.policy"]
     },
  Vsn = 0, % max api version
  Body = #{ resources => [DescribeConfigsArgs]
          },
  {ok, Rsp} = with_conn(kpro:connect_controller(?HOSTS, #{connect_timeout => ?TIMEOUT}),
                        fun(Pid) ->
                            Req = kpro_req_lib:make(describe_configs, Vsn, Body),
                            brod_utils:request_sync(Pid, Req)
                        end),
  [Resource] = kpro:find(resources, Rsp),
  [Entry] = kpro:find(config_entries, Resource),
  ConfigValue = kpro:find(config_value, Entry),
  ?assertEqual(<<"compact">>, ConfigValue).


t_alter_configs(Config) when is_list(Config) ->
  AlterConfigsArgs =
    #{ resource_type => ?RESOURCE_TYPE_TOPIC
     , resource_name => ?TOPIC
     , config_entries => [
                          [ {config_name, "cleanup.policy"}
                          , {config_value, <<"delete">>}]
                         ]
     },
  Vsn = 0, % max api version
  Body = #{ resources => [AlterConfigsArgs]
          , validate_only => false
          },
  with_conn(kpro:connect_controller(?HOSTS, #{connect_timeout => ?TIMEOUT}),
            fun(Pid) ->
                Request = kpro_req_lib:make(alter_configs, Vsn, Body),
                brod_utils:request_sync(Pid, Request)
            end).


t_describe_configs_after_alter(Config) when is_list(Config) ->
  DescribeConfigsArgs =
    #{ resource_type => ?RESOURCE_TYPE_TOPIC
     , resource_name => ?TOPIC
     , config_names => ["cleanup.policy"]
     },
  Vsn = 0, % max api version
  Body = #{ resources => [DescribeConfigsArgs]
          },
  {ok, Rsp} = with_conn(kpro:connect_controller(?HOSTS, #{connect_timeout => ?TIMEOUT}),
                        fun(Pid) ->
                            Req = kpro_req_lib:make(describe_configs, Vsn, Body),
                            brod_utils:request_sync(Pid, Req)
                        end),
  [Resource] = kpro:find(resources, Rsp),
  [Entry] = kpro:find(config_entries, Resource),
  ConfigValue = kpro:find(config_value, Entry),
  ?assertEqual(<<"delete">>, ConfigValue).


t_delete_topics(Config) when is_list(Config) ->
  ?assertEqual(ok, brod:delete_topics(?HOSTS, [?TOPIC], ?TIMEOUT,
                                      #{connect_timeout => ?TIMEOUT})).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

with_conn({ok, Pid}, Fun) ->
  try
    Fun(Pid)
  after
    kpro:close_connection(Pid)
  end;
with_conn({error, Reason}, _Fun) ->
  {error, Reason}.
