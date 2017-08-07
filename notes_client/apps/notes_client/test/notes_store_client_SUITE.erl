-module(notes_store_client_SUITE).

%% Common Test callbacks
-export([all/0,
         suite/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         group/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% Test cases
-export([add_note/1,
         get_note/1,
         list_notes/1,
         invalid_user/1,
         invalid_id/1,
         invalid_module/1
        ]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
  [add_note,
   get_note,
   list_notes,
   invalid_user,
   invalid_id,
   invalid_module
  ].

suite() ->
  [{timetrap, {seconds, 30}}].

groups() ->
  [].

init_per_suite(_Config) ->
  application:ensure_all_started(notes_client),
  [].

end_per_suite(_Config) ->
  application:stop(notes_client),
  ok.

group(_GroupName) ->
  [].

init_per_group(_GroupName, Config) ->
  Config.

end_per_group(_GroupName, _Config) ->
  ok.

init_per_testcase(_TestCase, Config) ->
  Config.

end_per_testcase(_TestCase, _Config) ->
  ok.

%%%===================================================================
%%% Test cases
%%%===================================================================
%%% This test case adds a note to notes storage database
add_note(_Config) ->
  Module = <<"notes_store">>,
  User = <<"shiva">>,
  Title = title(),
  {ok, Data} = notes_store_client:post(Module, User, Title),
  Id = proplists:get_value(<<"id">>, Data),
  ct:pal("Title:~p, Id:~p",[Title, Id]),
  {save_config,[{user,User},{title, Title},{id, Id}]}.

%%% This test case retrives a note based on Id
get_note(Config) ->
  {add_note,SavedConfig} = ?config(saved_config,Config),
  Id = ?config(id, SavedConfig),
  Title = ?config(title, SavedConfig),
  User = ?config(user, SavedConfig),
  {ok, Data} = notes_store_client:get(<<"notes_store">>, Id),
  ct:pal("Data:~p",[Data]),
  User = proplists:get_value(<<"user">>, Data),
  Title = proplists:get_value(<<"title">>, Data),
  ct:pal("Get Note Success"),
  {save_config,SavedConfig}.

%%% This test case retrives all notes of a particular user
list_notes(Config) ->
  {get_note,SavedConfig} = ?config(saved_config,Config),
  Id = ?config(id, SavedConfig),
  User = ?config(user, SavedConfig),
  {ok, Data} = notes_store_client:list(<<"notes_store">>, User),
  found = check_note_id(Data, Id),
  ct:pal("List Success"),
  {save_config,SavedConfig}.

%%% This test case tries to list notes of invalid user
invalid_user(Config) ->
  {list_notes,SavedConfig} = ?config(saved_config,Config),
  {ok, Data} = notes_store_client:list(<<"notes_store">>, <<"abcde">>),
  [] = Data,
  ct:pal("Invalid user gets empty list of notes"),
  ct:comment("Invalid User Notes:~p",[Data]),
  {save_config,SavedConfig}.

%%% This test case tries get a note with invalid note Id
invalid_id(Config) ->
  {invalid_user,SavedConfig} = ?config(saved_config,Config),
  {error, Reason} = notes_store_client:get(<<"notes_store">>, <<"abcde">>),
  ct:pal("Reason:~p",[Reason]),
  ct:comment("Invalid Id Response:~p",[Reason]),
  {save_config,SavedConfig}.

%%% This test case inputs invalid module name
invalid_module(Config) ->
  {invalid_id,SavedConfig} = ?config(saved_config,Config),
  {error, Reason} = notes_store_client:get(<<"notes">>, <<"abcde">>),
  ct:pal("Reason:~p",[Reason]),
  ct:comment("Invalid Module Response:~p",[Reason]),
  {save_config,SavedConfig}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

title() ->
  {{Y,M,D},{H,Mi,S}} = erlang:localtime(),
  YBin = int_to_bin(Y),
  MBin = int_to_bin(M),
  DBin = int_to_bin(D),
  HBin = int_to_bin(H),
  MiBin = int_to_bin(Mi),
  SBin = int_to_bin(S),
  Str = <<YBin/binary,":",MBin/binary,":",DBin/binary," ",HBin/binary,"-",MiBin/binary,"-",SBin/binary>>,
  <<"Title ",Str/binary>>.


int_to_bin(Int) ->
  erlang:integer_to_binary(Int).

check_note_id(Data, Id) ->
  lists:foldl(fun(_List, found) ->
                  found;
                (List, Status)->
                case proplists:get_value(<<"id">>, List) of
                  Id ->
                    found;
                  _ ->
                    Status
                end
              end, not_found, Data).
