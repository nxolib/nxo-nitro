nxoapp
=====

A rebar3 template to instantiate a Docker stack comprising an Erlang
OTP application server and a PostgreSQL database.

The purpose of this template is to augment the batteries included
`app` template with Docker swarm friendly artifacts.  Note that out of
the box Docker will configure and start a PostgreSQL server.  If this
isn't useful, the appropriate section of `docker-compose.yml` can be
removed.

Usage
-----

The helper script `bin/AppName` provides a few useful commands
(AppName is the actual name of your application):

| Command | Description |
| --- | --- |
| start | Create a Docker stack |
| shell | Attach to the Docker FE instance |
| db    | Connect to the Docker DB instance |
| ps    | List the running containers |
| stop  | Stop the Docker stack |

Note that `start` does not start the Erlang application.  In
non-development environments it is preferable to create a release and
run that in the foreground.  That's left as an exercise to the reader
(although see the sample Dockerfile included).

To start the rebar3 shell for development:

``` shell
$ bin/AppName start
$ bin/AppName shell
# rebar3 shell
```

Configuration
-------

A trivial `config/sys.config.src` file and corresponding
`environment/dev.env` are included along with the rebar.conf
configuration to make use of it.

Release Building
-------

A sample Dockerfile is included for convenience.  This is only a
starting point!  In fact, it's not going to run as-is:
`config/sys.config.src` requires at least one environment variable to
be set.  How you do that is really based on what makes sense in your
environment.

Author
-----
Bunny Lushington
