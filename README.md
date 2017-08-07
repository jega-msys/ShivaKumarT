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

#Tiny Notes:
  Tiny Notes server was implemented using http protocol and JSON data-interchange format. It takes commands `post`, `get` and `list` as functions. Server requires module and funtion parameters to execute commands. Notes are uniquely identified using user name and note title.

Requirements:
* Erlang 19+
* rebar3 3.3.5+

Steps To Run Server:
* make // Compiles Server and Client applications
* make server_tests // It initates common_test framework to test server application
* make server_shell // It runs the server in foreground

Steps To Run Client:
* make // Compiles Server and Client applications
* make server_shell // It starts a server application
* make client_tests // It initates common_test framework to test client application
* make client_shell // It runs the client in foreground

Adding a note in client shell
  notes_store_client:post(<<"notes_store">>, User, Title).

Listing all notes of a specific user
  notes_store_client:list(<<"notes_store">>, User).

Get a specific note using note Id
  notes_store_client:get(<<"notes_store">>, Id).

If the client is other than notes_client application, then use below configuration to initiate a http request:
ContentType: <<"application/json">>,
Method: Post,
URL: <<"ServerIp:8080">>,

Payload to add a note
  {"module":"notes_store","function":"post","user":"abc","title":"Title","note":"Note"}.

Payload to get a retrive note
  {"module":"notes_store","function":"get","id":"Id"}.

Payload to list all notes of a specific user
  {"module":"notes_store","function":"list","user":"User"}.
