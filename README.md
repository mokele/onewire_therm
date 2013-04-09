On your Raspberry-PI with Raspian Wheezy OS, if you've wired up your
OneWire thermistors correctly via pin 4 they will show up in your
devices list.
```bash
ls -lrt /sys/bus/w1/devices
total 0
lrwxrwxrwx 1 root root 0 Apr  9 11:22 w1_bus_master1 -> ../../../devices/w1_bus_master1
lrwxrwxrwx 1 root root 0 Apr  9 11:22 28-000004753e42 -> ../../../devices/w1_bus_master1/28-000004753e42
lrwxrwxrwx 1 root root 0 Apr  9 11:22 28-000004752948 -> ../../../devices/w1_bus_master1/28-000004752948
lrwxrwxrwx 1 root root 0 Apr  9 11:22 28-000004749085 -> ../../../devices/w1_bus_master1/28-000004749085
lrwxrwxrwx 1 root root 0 Apr  9 11:33 28-0000043792e5 -> ../../../devices/w1_bus_master1/28-0000043792e5
```

# And in Erlang
```erlang
1> onewire_therm_manager:subscribe("w1", "28-0000043792e5").
{ok,undefined,undefined} % undefined temperature and timestamp after
initial start
2> flush().
Shell got {therm,{"w1","28-0000043792e5"},15.437,{1365,511902,564105}}
ok
3> flush().
Shell got {therm,{"w1","28-0000043792e5"},15.375,{1365,511914,599490}}
Shell got {therm,{"w1","28-0000043792e5"},15.437,{1365,511917,439586}}
ok
```

You do not need to monitor or link to any processes in onewire_therm,
even if they crash during normal operation they will not affect your
subscription, meaning you will eventually receive more messages once the
relevant crashed processes come back up.
