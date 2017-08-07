-module(notes_server_util).

-define(APP, notes_server).
-export([get_config/1,
         get_config/2,
         get_config/3,

         to_binary/1
        ]
       ).

get_config(Key) ->
  get_config(?APP, Key, undefined).

get_config(Key, Default) ->
  get_config(?APP, Key, Default).

get_config(App, Key, Default) ->
  case application:get_env(App, Key) of
    undefined ->
      Default;
    {ok, Val} ->
      Val
  end.

to_binary(Data) when is_list(Data) ->
  list_to_binary(Data);
to_binary(Data) when is_binary(Data) ->
  Data;
to_binary(Data) ->
  list_to_binary(io_lib:print(Data)).
