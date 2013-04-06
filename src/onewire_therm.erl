-module(onewire_therm).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(TIMER, timer).

-record(onewire_therm, {
    message,
    crc,
    temperature
  }).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start/0,
    start_link/2
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
    wire,
    device,
    path,
    timeout
  }).

start() ->
  application:start(?MODULE).

start_link(Wire, Device) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Wire, Device], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Wire, Device]) ->
  State = #s{
    wire = Wire,
    device = Device,
    path = path(Wire, Device),
    timeout = 1000
  },
  timer(State),
  {ok, State}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(?TIMER, #s{path = Path} = State) ->
  Read = read(Path),
  lager:info("Read ~p", [Read]),
  timer(State),
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

path(Wire, Device) ->
  filename:join(["", "sys", "bus", Wire, "devices", Device, Wire ++ "_slave"]).

timer(#s{timeout = Timeout}) ->
  timer:send_after(Timeout, ?TIMER).

read(Path) when is_list(Path) ->
  {ok, Binary} = file:read_file(Path),
  read(Binary);

read(Binary) when is_binary(Binary) ->
  read(Binary, #onewire_therm{}).

read(<<A1:16,_,A2:16,_,A3:16,_,A4:16,_,A5:16,_,A6:16,_,A7:16,_,A8:16,_,CRC:16,Rest0/binary>>,
  #onewire_therm{
    message = undefined
  } = OneWire) ->
  [_, Rest] = to_nl(Rest0),
  read(Rest, OneWire#onewire_therm{
      message = <<A1:16,A2:16,A3:16,A4:16,A5:16,A6:16,A7:16,A8:16>>,
      crc = <<CRC:16>>
    });

read(<<A1:16,_,A2:16,_,A3:16,_,A4:16,_,A5:16,_,A6:16,_,A7:16,_,A8:16,_,CRC:16," t=",Rest/binary>>,
  #onewire_therm{
    message = <<A1:16,A2:16,A3:16,A4:16,A5:16,A6:16,A7:16,A8:16>>,
    crc = <<CRC:16>>
  } = OneWire) ->
  [TemperatureBinary|_] = to_nl(Rest),
  read(Rest, OneWire#onewire_therm{
      temperature = list_to_integer(binary_to_list(TemperatureBinary)) / 1000
    });

read(_, #onewire_therm{
    message = Message,
    crc = CRC,
    temperature = Temperature
  } = OneWire) when Message =:= undefined; CRC =:= undefined; Temperature =:= undefined ->
  {error, {temperature_not_found, OneWire}};

read(_, #onewire_therm{
    message = Message0,
    crc = CRC0,
    temperature = Temperature
  }) ->

  Message = onewire_therm_hex:hexstr_to_bin(binary_to_list(Message0)),
  CRC = onewire_therm_hex:hexstr_to_bin(binary_to_list(CRC0)),

  case onewire_therm_crc:crc8(Message) of
    CRC ->
      {ok, Temperature};
    CRC0 ->
      lager:info("Incorrect CRC ~p ~p", [CRC, CRC0]),
      {error, incorrect_crc}
  end.

to_nl(Binary) ->
  binary:split(Binary, <<10>>).
