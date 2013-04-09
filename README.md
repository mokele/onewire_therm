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
