-module(onewire_therm_manager).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
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
    therms,
    subscribers_to_therms
  }).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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

init([]) ->
  {ok, #s{
      therms = ets:new(?MODULE, [set]),
      subscribers_to_therms = ets:new(onewire_therm_subscribers_to_therms, [bag])
    }}.

handle_call({subscribe, Subscriber, Wire, Device}, _From, #s{
    therms = Therms,
    subscribers_to_therms = Subscribers2Therms
  } = State) ->
  Key = {Wire, Device},
  ThermPid =
    case ets:lookup(Therms, Key) of
      [{Key, ThermPid0}] ->
        ThermPid0;
      [] ->
        Subscribers = ets:new(onewire_therm_subscribers, [set, protected]),
        {ok, ThermPid0} = onewire_therm_sup_sup:start_child(Subscribers, Wire, Device),
        monitor(process, ThermPid0),
        ets:insert(Subscribers2Therms, {Subscriber, Key}),
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
    therms = Therms,
    subscribers_to_therms = Subscribers2Therms
  } = State) ->
  case ets:lookup(Subscribers2Therms, Pid) of
    [{Pid, Key}] ->
      [{Key, ThermPid}] = ets:lookup(Therms, Key),
      [{ThermPid, Key, Subscribers}] = ets:lookup(Therms, ThermPid),
      ets:delete(Subscribers2Therms, Pid),
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
