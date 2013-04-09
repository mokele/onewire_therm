-module(onewire_therm_sup).
-behaviour(supervisor).
-define(SERVER, ?MODULE).

%% API
-export([
    start_link/1
  ]).

%% Supervisor callbacks
-export([
    init/1
  ]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Args, Type), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(App) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [App]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([App]) ->
  {ok, { {one_for_all, 5, 10}, [
        ?CHILD(onewire_therm_sup_sup, [], supervisor),
        ?CHILD(onewire_therm_manager, [App], worker)
      ]} }.
