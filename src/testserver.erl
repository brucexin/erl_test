
-module(testserver).

-behaviour(gen_fsm).

-define(WAIT_TIMEOUT, 10000).

-define(LOGGER(Info), io:format(Info)).
-define(LOGGER(Format, Info), io:format(Format, Info)).

-compile(export_all).

%%client functions
start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_fsm:cast(?MODULE, stop).

query_state() ->
    gen_fsm:sync_send_all_state_event(?MODULE, query_state).


%% server functions
init([]) ->
    ?LOGGER("testserver started! ~p~n", [application:get_all_key()]),
    {Name, Desc, Vsn} = get_app_vsn(),
    {ok, state_print, [], ?WAIT_TIMEOUT}.

terminate(Reason, StateName, _LoopData) ->
    ?LOGGER("testserver terminate ~p for ~p~n!", [Reason, StateName]).

get_app_vsn() ->
    {ok, AppName} = application:get_application(self()),
    lists:keyfind(AppName, 1, application:loaded_applications()).

state_print(timeout, []) ->
    ?LOGGER("None state ~n"),
    {next_state, state_print, [], ?WAIT_TIMEOUT};
state_print(timeout, [{AppName, Desc, Vsn}]) ->
    ?LOGGER("testserver for ~p-~p alive at ~p~n", [AppName, Vsn, erlang:now()]),
    {next_state, state_print, [{AppName, Desc, Vsn}], ?WAIT_TIMEOUT}.

handle_info(stop, _StateName, LoopData) ->
    {stop, normal, LoopData}.

handle_sync_event(query_state, _From, StateName, LoopData) ->
    {reply, LoopData, StateName, ?WAIT_TIMEOUT}.

code_change({down, _Vsn}, StateName, LoopData, _Extra) ->
    ?LOGGER("code change down for testserver ~p ~p ~n", [_Vsn, StateName]),
    {ok, StateName, []};
code_change(_Vsn, StateName, LoopData, _Extra) ->
    ?LOGGER("code change upgrade for testserver ~p ~p ~n", [_Vsn, StateName]),
    {Name, Desc, Vsn} = get_app_vsn(),
    {ok, StateName, [{Name, Desc, Vsn}]}.


