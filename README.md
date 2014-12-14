### GitHub Events -> InfluxDB

This little binary implements a [GitHub webhook][webhook] which forwards
events to [InfluxDB][influx].


#### Configuration

Configuration is done according to the Twelve-Factor App [config
guideliens][12factor-config] in the environment.

 - **INFLUXDB**: The URL to the Influx database. The path component of the URL
   specifies the database. Example: `http://root:root@influx.example.com:8086/dbname`.
   Minimal complete definition MUST include username, password, hostname and
   dbname.

 - **HOOKPATH**: The path where the hook accepts events. This is optional,
   when not specified then this default is used: `webhook`.


Example:

```shell
INFLUXDB=http://... HOOKPATH=github-events ./github-influx-bridge
```

On the GitHub side, go to the repository settings and add a new webhook. Make
sure the path component of the *Payload URL* matches the `HOOKPATH`. You can
safely send all events to this hook (select the *Send me everything.* option).


[webhook]: https://developer.github.com/webhooks/
[influx]: http://influxdb.com/
[12factor-config]: http://12factor.net/config
