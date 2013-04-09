-module(onewire_therm_manager).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(SUBSCRIBERS_TO_THERMS_TAB, onewire_therm_subscribers_to_therms).
-define(THERMS_TO_SUBSCRIBERS_TAB, onewire_therm_therms_to_subscribers).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/1,
    subscribe/2,
    publish/2
  ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([
    init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
  ]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-record(s, {
    app_pid,
    therms,
    subscribers_to_therms
  }).

start_link(App) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [App], []).

subscribe(Wire, Device) ->
  gen_server:call(?SERVER, {subscribe, self(), Wire, Device}).

publish(Subscribers, Message) ->
  ets:foldl(
    fun({Subscriber}, true) ->
        Subscriber ! Message
    end,
    true,
    Subscribers
  ).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([App]) ->
  Therms = ets:new(?MODULE, [set]),
  case ets:info(?SUBSCRIBERS_TO_THERMS_TAB) of
    undefined ->
      ets:new(?SUBSCRIBERS_TO_THERMS_TAB, [bag, named_table, public]),
      ets:new(?THERMS_TO_SUBSCRIBERS_TAB, [set, named_table, public]),
      ets:give_away(?SUBSCRIBERS_TO_THERMS_TAB, App, no_data),
      ets:give_away(?THERMS_TO_SUBSCRIBERS_TAB, App, no_data);
    _ ->
      ets:foldl(
        fun({{Wire, Device} = Key, Subscribers}, _) ->
            lager:info("Restarting onewire_therm ~p ~p ~p", [Wire, Device, ets:tab2list(Subscribers)]),
            {ok, ThermPid} = onewire_therm_sup_sup:start_child(Subscribers, Wire, Device),
            monitor(process, ThermPid),
            ets:insert(Therms, {ThermPid, Key, Subscribers}),
            ets:insert(Therms, {Key, ThermPid})
        end,
        true,
        ?THERMS_TO_SUBSCRIBERS_TAB
      )
  end,
  {ok, #s{
      app_pid = App,
      therms = Therms
    }}.

handle_call({subscribe, Subscriber, Wire, Device}, _From, #s{
    app_pid = App,
    therms = Therms
  } = State) ->
  Key = {Wire, Device},
  ThermPid =
    case ets:lookup(Therms, Key) of
      [{Key, ThermPid0}] ->
        ThermPid0;
      [] ->
        Subscribers =
          case ets:lookup(?THERMS_TO_SUBSCRIBERS_TAB, Key) of
            [{Key, Subscribers0}] ->
              Subscribers0;
            [] ->
              Subscribers0 = ets:new(onewire_therm_subscribers, [set, public]),
              ets:give_away(Subscribers0, App, no_data),
              ets:insert(?THERMS_TO_SUBSCRIBERS_TAB, {Key, Subscribers0}),
              Subscribers0
          end,
        {ok, ThermPid0} = onewire_therm_sup_sup:start_child(Subscribers, Wire, Device),
        monitor(process, ThermPid0),
        ets:insert(?SUBSCRIBERS_TO_THERMS_TAB, {Subscriber, Key}),
        ets:insert(Subscribers, {Subscriber}),
        ets:insert(Therms, {ThermPid0, Key, Subscribers}),
        ets:insert(Therms, {Key, ThermPid0}),
        ThermPid0
    end,

  monitor(process, Subscriber),
  {reply, onewire_therm:temperature(ThermPid), State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _}, #s{
    therms = Therms
  } = State) ->
  case ets:lookup(?SUBSCRIBERS_TO_THERMS_TAB, Pid) of
    [{Pid, Key}] ->
      [{Key, ThermPid}] = ets:lookup(Therms, Key),
      [{ThermPid, Key, Subscribers}] = ets:lookup(Therms, ThermPid),
      ets:delete(?SUBSCRIBERS_TO_THERMS_TAB, Pid),
      ets:delete(Subscribers, Pid);
    [] ->
      case ets:lookup(Therms, Pid) of
        [{Pid, {Wire, Device} = Key, Subscribers}] ->
          {ok, ThermPid} = onewire_therm_sup_sup:start_child(Subscribers, Wire, Device),
          monitor(process, ThermPid),
          ets:delete(Therms, Pid),
          ets:insert(Therms, {ThermPid, Key});
        _ ->
          ok
      end
  end,
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
