-module(onewire_therm).
-behaviour(gen_server).
-define(TIMER, timer).

-record(therm, {
    message,
    crc,
    temperature,
    timestamp
  }).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start/0,
    start_link/3,
    temperature/1
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
    subscribers,
    wire,
    device,
    path,
    timeout,
    last_value = #therm{}
  }).

start() ->
  application:start(?MODULE).

start_link(Subscribers, Wire, Device) ->
  gen_server:start_link(?MODULE, [Subscribers, Wire, Device], []).

temperature(Pid) ->
  gen_server:call(Pid, temperature).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Subscribers, Wire, Device]) ->
  State = #s{
    subscribers = Subscribers,
    wire = Wire,
    device = Device,
    path = path(Wire, Device),
    timeout = 2000
  },
  timer(State),
  {ok, State}.

handle_call(temperature, _From, #s{
    last_value = #therm{temperature = T, timestamp = TS}
  } = State) ->
  {reply, {ok, T, TS}, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(?TIMER, #s{
    wire = Wire,
    device = Device,
    subscribers = Subscribers,
    path = Path,
    last_value = Therm0
  } = State) ->
  T0 = Therm0#therm.temperature,
  Therm = 
    case read(Path) of
      {error, temperature_not_found} ->
        Therm0;
      {ok, T0, TS} ->
        Therm0#therm{timestamp = TS};
      {ok, T1, TS} ->
        onewire_therm_manager:publish(Subscribers, {therm, {Wire, Device}, T1, TS}),
        Therm0#therm{temperature = T1, timestamp = TS};
      Error ->
        Error
    end,
  timer(State),
  case Therm of
    {error, _} ->
      {noreply, State};
    _ ->
      {noreply, State#s{last_value = Therm}}
  end;
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

path(Wire, Device) ->
  filename:join(["", "sys", "bus", Wire, "devices", Device, Wire ++ "_slave"]).

timer(#s{timeout = Timeout}) ->
  timer:send_after(Timeout, ?TIMER).

read(Path) when is_list(Path) ->
  case file:read_file(Path) of
    {ok, Binary} ->
      read(Binary);
    Else ->
      Else
  end;

read(Binary) when is_binary(Binary) ->
  read(Binary, #therm{
      timestamp = erlang:now()
    }).

read(<<A1:16,_,A2:16,_,A3:16,_,A4:16,_,A5:16,_,A6:16,_,A7:16,_,A8:16,_,CRC:16,_,$:,_,"crc=",CRC:16,_,"YES",Rest0/binary>>,
  #therm{
    message = undefined
  } = OneWire) ->
  [_, Rest] = to_nl(Rest0),
  read(Rest, OneWire#therm{
      message = <<A1:16,A2:16,A3:16,A4:16,A5:16,A6:16,A7:16,A8:16>>,
      crc = <<CRC:16>>
    });

read(<<A1:16,_,A2:16,_,A3:16,_,A4:16,_,A5:16,_,A6:16,_,A7:16,_,A8:16,_,CRC:16," t=",Rest/binary>>,
  #therm{
    message = <<A1:16,A2:16,A3:16,A4:16,A5:16,A6:16,A7:16,A8:16>>,
    crc = <<CRC:16>>
  } = OneWire) ->
  [TemperatureBinary|_] = to_nl(Rest),
  read(Rest, OneWire#therm{
      temperature = list_to_integer(binary_to_list(TemperatureBinary)) / 1000
    });

read(_, #therm{
    message = Message,
    crc = CRC,
    temperature = Temperature
  }) when Message =:= undefined; CRC =:= undefined; Temperature =:= undefined ->
  {error, temperature_not_found};

read(_, #therm{
    temperature = Temperature,
    timestamp = Timestamp
  }) ->
  {ok, Temperature, Timestamp}.

to_nl(Binary) ->
  binary:split(Binary, <<10>>).
