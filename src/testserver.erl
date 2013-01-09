
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


%% server functions
init([]) ->
    ?LOGGER("testserver started! ~p~n", [application:get_all_key()]),
    {Name, Desc, Vsn} = get_app_vsn(),
    {ok, state_print, [Name, Vsn], ?WAIT_TIMEOUT}.

terminate(Reason, StateName, _LoopData) ->
    ?LOGGER("testserver terminate ~p for ~p~n!", [Reason, StateName]).

get_app_vsn() ->
    {ok, AppName} = application:get_application(self()),
    lists:keyfind(AppName, 1, application:loaded_applications()).

state_print(timeout, LoopData) ->
    %AppName = lists:nth(1, LoopData),
    %Vsn = lists:nth(2, LoopData),
    {AppName, Desc, Vsn} = get_app_vsn(),
    ?LOGGER("testserver for ~p-~p alive at ~p~n", [AppName, Vsn, erlang:now()]),
    {next_state, state_print, LoopData, ?WAIT_TIMEOUT}.

handle_info(stop, _StateName, LoopData) ->
    {stop, normal, LoopData}.


