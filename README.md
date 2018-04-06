InfluxDB client
---------------

InfluxDB client to drop data to InfluxDB using TCP or UDP protocol. The line encoder is shamelessly
ripped from https://github.com/palkan/influx_udp.

Compatible with telegraf socket_listener.

It has no external dependencies, and is dead simple.

To use you need to include the application into rebar dependencies as follows:
```
...
{deps, [
...
    {influx, ".*", {git, "https://github.com/netdalek/stenographer.git", "master"}}
]}.
...
```

There are two application parameters, protocol, host and port, with default set to udp,
"127.0.0.1" and 4444, respectively.

API
---

```
stenographer:send(cpu, [{value,80}]).
stenographer:send(cpu, [{value,80},{another,123}]).
```
will become an influx "measurement" string "cpu value=80" and "cpu value=80,another=123".

```
influx:send(cpu, [{tag1,smth}], [{value,80}]).
```
