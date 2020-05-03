-module(brod_old_SUITE).

-export([ init_per_suite/1
        , end_per_suite/1
        , all/0
        , suite/0
        ]).

-export([ t_create_topics/1
        , t_delete_topics/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl"). % Eunit macros for convenience

-include_lib("kafka_protocol/include/kpro.hrl").
-include_lib("kafka_protocol/include/kpro_error_codes.hrl").

-type config() :: [{atom(), term()}].

-define(HOSTS, [{"localhost", 9092}]).
-define(TIMEOUT, 280000).

-define(RESOURCE_TYPE_TOPIC, 2).
-define(TOPIC, <<"test-topic1">>).

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
-ifdef(OLD_DEPS).
all() -> [ t_create_topics
         , t_delete_topics
         ].
-else.
all() -> [].
-endif.

-spec suite() -> [atom()].
suite() -> [{timetrap, {minutes, 5}}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%% Also see:
%% https://github.com/klarna/brod/commit/71e403a2de16e36d78e101be01ee675b501f25a9
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t_create_topics(Config) when is_list(Config) ->
  Hosts = ?HOSTS,
  TopicConfigs = [
                  #{ config_entries => [
                                        [ {config_name, "cleanup.policy"}
                                        , {config_value, <<"compact">>}],
                                        [ {config_name, "retention.ms"}
                                        , {config_value, <<"5000">>}]
                                       ]
                   , num_partitions => 2
                   , replica_assignment => []
                   , replication_factor => 2
                   , topic => ?TOPIC
                   }
                 ],
  RequestConfigs = #{ timeout => ?TIMEOUT
                    , validate_only => false},
  ConnCfg = #{timeout => ?TIMEOUT},
  {ok, Rsp} = with_conn(kpro:connect_controller(Hosts, ConnCfg),
                        fun(Pid) ->
                            Vsn = 0, % API version used
                            Req = kpro_req_lib:create_topics(Vsn, TopicConfigs, RequestConfigs),
                            kpro:request_sync(Pid, Req, infinity) %% Default RequestTimeout is used
                        end),
  ok = parse_rsp(Rsp).

%% Rsp looks like:
%% {kpro_rsp,#Ref<0.234127075.906231816.175858>,create_topics,0,
%%           #{topic_errors =>
%%                 [#{error_code => no_error,topic => <<"test-topic">>}]}}


t_delete_topics(Config) when is_list(Config) ->
  Hosts = ?HOSTS,
  Topics = [?TOPIC],
  Timeout = ?TIMEOUT, %% Controller will wait this time for topic to be deleted
  {ok, Rsp} = with_conn(kpro:connect_controller(Hosts, #{connect_timeout => ?TIMEOUT}),
                        fun(Pid) ->
                            Vsn = 0, % API version used
                            Req = kpro_req_lib:delete_topics(Vsn, Topics, #{timeout => Timeout}),
                            kpro:request_sync(Pid, Req, infinity) %% Default RequestTimeout is used
                        end),
  ok = parse_rsp(Rsp).

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

%% Make sure the response is ok
parse_rsp(#kpro_rsp{ api = create_topics
                   , msg = Msg
                   }) ->
  error_if_any(kpro:find(topic_errors, Msg));
parse_rsp(#kpro_rsp{ api = delete_topics
                   , msg = Msg
                   }) ->
  error_if_any(kpro:find(topic_error_codes, Msg)).

%% Return ok if all error codes are 'no_error'
error_if_any(Errors) ->
  Pred = fun(Struct) -> kpro:find(error_code, Struct) =/= ?no_error end,
  case lists:filter(Pred, Errors) of
    [] -> ok;
    Errs -> erlang:error(Errs)
  end.
