.PHONY: default server client clean clean_server clean_client server_tests client_tests server_shell
SERVER = notes_server
CLIENT = notes_client

default:
	$(MAKE) -C $(SERVER) -sw clean compile
	$(MAKE) -C $(CLIENT) -sw clean compile
server:
	$(MAKE) -C $(SERVER) -sw compile

client:
	$(MAKE) -C $(CLIENT) -sw compile

clean:
	$(MAKE) -C  $(SERVER) -sw clean
	$(MAKE) -C  $(CLIENT) -sw clean

clean_server:
	$(MAKE) -C $(SERVER) -sw clean

clean_client:
	$(MAKE) -C $(CLIENT) -sw clean

server_tests:
	$(MAKE) -C $(SERVER) -sw test

client_tests:
	$(MAKE) -C $(CLIENT) -sw test

server_shell:
	$(MAKE) -C $(SERVER) shell

client_shell:
	$(MAKE) -C $(CLIENT) shell


