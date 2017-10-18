Dampf
=====

dampf is a tool for managing the DANPF stack:

* **D**ocker: your application code is specified as services running in docker containers, so you can use any language you like!
* **A**MQP message queues: for transient communication between services
* **N**GINX: domains and static serving by NGINX
* **P**ostgreSQL: dampf manages your relational databases, including migrations if you want it to
* **F**iles: persistent filesystems accessible to services

By extension any service that can run as a docker container (e.g. Redis) can be accommodated in this framework

Table of Contents
-----------------

* [**Goals**](#goals)
* [**Status**](#status)
* [**Application file**](#application-file)
* [**Configuration file**](#configuration-file)
* [**Command line tool**](#command-line-tool)
* [**Environment variables and host names**](#environment-variables-and-host-names)

## Goals

The goals of dampf in managing the DANPF stack are:

* Simplicity: focus on building your insanely awesome application instead of learning Kubernetes the hard way. Dampf is for developers
  who want to spend as little time as possible on system administration and DevOps.
* DevOps at the application level: describe applications, not servers
* Multiple interpreters: one dampf application can be deployed in many different ways: locally as a development environment,
  directly to a dedicated server, or to any number of cloud solutions or hosting environments
* as many common tasks as possible are built in and available through a common interface
* dampf avoids vendor lock-in doubly: it is open source, MIT licenced; and the same application description can be executed in multiple environments.
* Full stack integration testing is built-in from the beginning, integrating with your database.
* Monitoring and testing [are the same thing.](https://plus.google.com/+RipRowan/posts/eVeouesvaVX)

Dampf was created out of the conviction that too many teams and start-ups solve the same problems over and over again for stacks
that are very similar. Dampf is intended to get a new project into production quickly but also scale in the growth phase.

## Status

The above is mostly wishful thinking. Dampf can currently deploy images, containers, domains and databases onto the local machine.
Testing and multiple interpreters are TODO.

## Application file

Your application is specified in the dampf application YAML file, typically named `dampf.yaml`. The sections in this file describe docker images,
containers for services, databases and domains. An example application file is given in `dampf.yaml.example` in this repository.

The application file specifies *what* your application is, not *how* it should be deployed. If it is not possible to describe your application entirely
using this format, you should not be using dampf.

This file should be checked into version control together with your application code.

### Image section

Syntax:

```
image {image name}:
  dockerFile: {relative path to Dockerfile}
```

Example:

```
image myimage:
  dockerFile: .
```

Declares that an image should be built from the specified Dockerfile and may be used in services using the specified image name.

### Container section

Syntax:

```
container: {container name}:
  image: {image name}
  expose: [port number, ...]
  command: {command to run container in image} # optional
```

Example:

```
container mycontainer1:
  image: myimage
  expose: [3002]
  command: serve
```

Declares that a container should be running from the specified image, optionally using the specified command (otherwise the
default command for the image will be used). The list of ports will be exposed internally in the container. Dampf should manage the ports
those are mapped to externally, so different containers can expose the same port and they will be mapped to different ports externally
(although this is not currently implemented).

### Domain section

Syntax:

```
domain {domain}:
  proxyContainer: {container name: port number} # optional
  static: {path to static content} # optional
  letsEncrypt: true|false # optional
```

Example:

```
domain mydomain.com
  proxyContainer: mycontainer:3102
  static: www/
  letsEncrypt: true
```

Declares that the server should listen on a domain and serve static content and/or forward requests to a container.

If a path to static content specified,
then this content should be served first if the request matches a file in that path.

If a proxy container is specified, and if the request does not match
file in a static path specified, then the request should be proxied to the running container, to the port specified (matching the internal port on the container).

If `letsEncrypt` is true, then the Let's Encrypt service will be used to provide SSL certificates.

### Database section

Syntax:

```
postgresdb {database name}:
  migrations: {path to migrations directory} # optional
  user: {username for the owner of this database}
  extensions: [extension, ...] # Optional
```

Example:
```
postgresdb mydb:
  migrations: migrations/
  user: myuser
  extensions: ["hstore"]
```

Declares that a PostgreSQL database is part of the application.

Dampf can manage your migrations. The migrations must be set to a relative path which contains the SQL files for individual migrations. In this directory, each migration should be a file with a name of the form YYYYMMDDHHMMSS_{migration name}.sql containing SQL to perform the migration. Files with names of this form can be generated with the `new_migration` command (see below).

*TODO* Alternatively, migrations can be managed by invoking the command inside a container.

The database will be owned by the given user and the specified extensions should be present.

### Test section

Syntax:

```
test {test name}:
  when: [AtBuild|AtDeploy|Hourly|Daily|Frequently, ...]
  units:
    - unit 1
    - unit 2
```
Unit syntax:

```
run {image_name} {command} {args..}
get {url}
get {url} =~ {regex}
```

Example:

```
test mytest:
  when: [AtDeploy, Daily]
  units:
    - run myimage mycommand arg1
    - get http://foo.com =~ "Foo"
```

Specifies a test from a list of test units (commands to run in images, or URLs to probe).

## Configuration file

The configuration file specifies *how* applications should be deployed, independently of *what* those applications are. An example configuration file ([.dampfcfg.yaml.example](https://github.com/diffusionkinetics/open/blob/master/dampf/.dampfcfg.yaml.example))is provided in this repository.

This file should *not* be checked into version control as it contains secrets that can be used to compromise your setup.

Syntax:

```
liveCertificate: {part to Let's encrypt certificate file}

postgres:
    host: {host address or name}
    port: {port number}

    users:
        postgres: {postgres password}
        {user name}: {user password}
        ...
```

## Command line tool

`dampf` is a commandline executable that can perform various tasks related to applications specified in an application file. dampf can build images, deploy the application and manage databases.

`dampf` presents a hierarchy with global options, commands and options for each command similar to `git`. To see a list of commands run `dampf --help`. For instance, to run the `build` command using the application file named `myapp.yaml` run `dampf -a myapp.yaml build`. To see options and arguments specific to each command, run `dampf {command} --help`.

### Global options

* **a**ppFile: give the name of the file that contains the application specification. This defaults to `dampf.yaml` in the current directory.
* **c**onfigFile: give the name of the file that contains the configuration specification. This defaults to `.dampfcfg.yaml` in the home directory.
* **p**rofile: select the profile to use out of the ones specified in the configuration file. This defaults to ???

### `backup` command

Backup databases. If invoked with no arguments, this will backup all databases listed in the application file. If one or more arguments follow the `backup` command, only databases with names matching those arguments will be backed up.

### `build` command

Build all the docker images in the application.

### `deploy` command

Build all the docker images in the application, (re)start the containers, run migrations and deploy the specified domains.

### `newmigration` command

Syntax: `dampf newmigration {migration name}`

Create a new migration file in the migrations directory using the current time as the timestamp

### `monitor` command

Syntax: `dampf monitor {test name}`

Run specified test (if non specified, run all tests except those only marked
"AtBuild") against the live production environment.

Status: Not implemented

## Environment variables and host names

dampf will set environmental variables in your running containers. These can be used to connect to databases and other containers. You should use these because they will be set correctly during testing so your test containers will connect to a test database.

TODO: list environment variables