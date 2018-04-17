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

There are two application parameters, protocol, host and port, with default set to tcp,
"127.0.0.1" and 8094, respectively.

You could also specify default tags in application parameters.
```
{stenographer, [
  {protocol, tcp},
  {tags, [{app, myapp}]}
]}
```

It adds hostname to tags by default
```
{stenographer, [
  {host_as_tag, true}
]}
```

You could also add node name to tags
```
{stenographer, [
  {node_name_as_tag, true}
]}
```

API
---

```
stenographer:send(cpu, [{value,80}]).
```
will become an influx "measurement" string "cpu value=80"

```
stenographer:send(cpu, [{value,80},{another,123}]).
```
will become an influx "measurement" string "cpu value=80,another=123".

```
influx:send(cpu, [{value,80}], [{tag1,smth}]).
```
will become an influx "measurement" string "cpu,tag1=smth value=80".
