# ShivaKumarT
ShivaKumarT Interview Project

Create a server application that users can treat as cloud backup.
The server understands 3 cmds, `post`, `get` and `list`.
Define and document the packet protocol for the same.

A light weight client application can be invoked from any client machine.
This should connect to the server and help perform the 3 operations listed above.

Both the server and client application needs to have unit tests (probably use googletest or anything of your choice).

Both server and client should not have any parameters hardcoded. If parameters are needed, they
should be defined part of a configuration file and source from it.

A top level makefile should exist, with below targets.
* make server
* make client
* make clean/clean_server/clean_client/
* make server_tests // should run unit test framework on server
* make client_tests // should run unit test framework on clients.

Proper documentation of server/client/unit test frameworks and how to run them are essential. You can edit this readme to add your own content like build instruction, dependencies to be installed and instruction to test and run the program.
