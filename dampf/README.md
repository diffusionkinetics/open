Dampf
=====

dampf is a tool for managing the DANPF stack:

* **D**ocker: your application code is specified as services running in docker containers, so you can use any language you like!
* **A**MQP message queues: for transient communication between services
* **N**GINX: domains and static serving by NGINX
* **P**ostgreSQL: dampf managers your relational databases, including migrations if you want it to
* **F**iles: persistent filesystems accessible to services

Note that by extension any service that can run as a docker container (e.g. Redis) can be accommodated in this framework

The goals of dampf in managing this stack are:

* Simplicity: focus on building your insanely awesome application instead of learning Kubernetes the hard way. Dampf is for developers 
  who want to spend as little time as possible on system administration and DevOps.
* DevOps at the application level: describe applications, not servers
* Multiple interpreters: one dampf application can be deployed in many different ways: locally as a development environment, 
  directly to a dedicated server, or to any number of cloud solutions or hosting environments
* as many common tasks as possible are built in and available through a common interface
* dampf avoids vendor lock-in doubly: it is open source, MIT licenced; and the same application description can be executed in multiple environments.
* Full stack integration testing is built-in from the beginning, integrating with your database.

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
  migrations: {path to migrations directory}
  user: {username for the owner of this database}
  extensions: [extension, ...]
```

Example:
```
postgresdb mydb:
  migrations: migrations/
  user: myuser
  extensions: ["hstore"]
```

