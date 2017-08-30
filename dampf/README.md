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

* DevOps at the application level: describe applications, not servers
* Multiple interpreters: one dampf application can be deployed in many different ways: locally as a development environment, 
  directly to a dedicated server, or to any number of cloud solutions or hosting environments
* as many common tasks as possible are built in and available through a common interface
* dampf avoids vendor lock-in doubly: it is open source, MIT licenced; and the same application description can be executed in multiple environments.
* Full stack integration testing is built-in from the beginning, integrating with your database.

Dampf was created out of the conviction that too many teams and start-ups solve the same problems over and over again for stacks 
that are very similar. Dampf is intended to get a new project into production quickly and without much work, but also scale in the growth phase.

