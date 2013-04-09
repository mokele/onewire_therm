-module(onewire_therm_sup_sup).
-define(SERVER, ?MODULE).

-behaviour(supervisor).

%% API
-export([
    start_link/0,
    start_child/3
  ]).

%% Supervisor callbacks
-export([
    init/1
  ]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Subscribers, Wire, Device) ->
  supervisor:start_child(?SERVER, [Subscribers, Wire, Device]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, { {simple_one_for_one, 5, 10}, [
        {undefined, {onewire_therm, start_link, []},
          temporary, brutal_kill, worker,
          [onewire_therm]}
      ]} }.
