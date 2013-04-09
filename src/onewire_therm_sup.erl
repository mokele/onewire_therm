-module(onewire_therm_sup).
-behaviour(supervisor).
-define(SERVER, ?MODULE).

%% API
-export([
    start_link/0
  ]).

%% Supervisor callbacks
-export([
    init/1
  ]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, { {one_for_all, 5, 10}, [
        ?CHILD(onewire_therm_sup_sup, supervisor),
        ?CHILD(onewire_therm_manager, worker)
      ]} }.
