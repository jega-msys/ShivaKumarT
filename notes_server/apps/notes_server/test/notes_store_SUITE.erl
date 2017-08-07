-module(notes_store_SUITE).

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
         modify_note/1,
         verify_modification/1,
         invalid_user/1,
         invalid_id/1,
         missing_title_in_post/1,
         missing_user_in_post/1,
         missing_user_in_list/1,
         missing_id_in_get/1
        ]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
  [add_note,
   get_note,
   list_notes,
   modify_note,
   verify_modification,
   invalid_user,
   invalid_id,
   missing_title_in_post,
   missing_user_in_post,
   missing_user_in_list,
   missing_id_in_get
  ].

suite() ->
  [{timetrap, {seconds, 30}}].

groups() ->
  [].

init_per_suite(_Config) ->
  application:ensure_all_started(notes_server),
  [].

end_per_suite(_Config) ->
  application:stop(notes_server),
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
  User = <<"shiva">>,
  Title = title(),
  List = [{<<"user">>,User},{<<"title">>, Title}],
  {ok, <<"application/json">>, Data} = notes_store:post(List),
  DecodedData = decode(Data),
  Id = proplists:get_value(<<"id">>, DecodedData),
  ct:pal("Title:~p, Id:~p",[Title, Id]),
  {save_config,[{user,User},{title, Title},{id, Id}]}.

%%% This test case retrives a note based on Id
get_note(Config) ->
  {add_note,SavedConfig} = ?config(saved_config,Config),
  Id = ?config(id, SavedConfig),
  Title = ?config(title, SavedConfig),
  User = ?config(user, SavedConfig),
  List = [{<<"id">>, Id}],
  {ok, <<"application/json">>, Data} = notes_store:get(List),
  DecodedData = decode(Data),
  ct:pal("Data:~p",[Data]),
  User = proplists:get_value(<<"user">>, DecodedData),
  Title = proplists:get_value(<<"title">>, DecodedData),
  ct:pal("Get Note Success"),
  {save_config,SavedConfig}.

%%% This test case retrives all notes of a particular user
list_notes(Config) ->
  {get_note,SavedConfig} = ?config(saved_config,Config),
  Id = ?config(id, SavedConfig),
  User = ?config(user, SavedConfig),
  List = [{<<"user">>,User}],
  {ok, <<"application/json">>, Data} = notes_store:list(List),
  DecodedData = decode(Data),
  found = check_note_id(DecodedData, Id),
  ct:pal("List Success"),
  {save_config,SavedConfig}.

%%% This test case modifies the existing note
modify_note(Config) ->
  {list_notes,SavedConfig} = ?config(saved_config,Config),
  Id = ?config(id, SavedConfig),
  User = ?config(user, SavedConfig),
  Title = ?config(title, SavedConfig),
  Note = <<"Test Note .....">>,
  List = [{<<"user">>,User},{<<"title">>, Title},{<<"id">>,Id},{<<"note">>,Note}],
  {ok, <<"application/json">>, _Data} = notes_store:post(List),
  ct:pal("Modify Success"),
  {save_config,[{note,Note}|SavedConfig]}.

%%% This test case verifies modification to note
verify_modification(Config) ->
  {modify_note,SavedConfig} = ?config(saved_config,Config),
  Id = ?config(id, SavedConfig),
  Title = ?config(title, SavedConfig),
  Note = ?config(note, SavedConfig),
  List = [{<<"id">>, Id}],
  {ok, <<"application/json">>, Data} = notes_store:get(List),
  DecodedData = decode(Data),
  ct:pal("Data:~p",[Data]),
  Note = proplists:get_value(<<"note">>, DecodedData),
  Title = proplists:get_value(<<"title">>, DecodedData),
  ct:pal("Get Note Success"),
  {save_config,SavedConfig}.

%%% This test case tries to list notes of invalid user
invalid_user(Config) ->
  {verify_modification,SavedConfig} = ?config(saved_config,Config),
  List = [{<<"user">>,<<"aghhh">>}],
  {ok, <<"application/json">>, Data} = notes_store:list(List),
  DecodedData = decode(Data),
  [] = DecodedData,
  ct:pal("Invalid user gets empty list of notes"),
  ct:comment("Invalid User Notes:~p",[Data]),
  {save_config,SavedConfig}.

%%% This test case tries get a note with invalid note Id
invalid_id(Config) ->
  {invalid_user,SavedConfig} = ?config(saved_config,Config),
  List = [{<<"id">>,<<"123455">>}],
  {error, Reason} = notes_store:get(List),
  ct:pal("Reason:~p",[Reason]),
  ct:comment("Invalid Id Response:~p",[Reason]),
  {save_config,SavedConfig}.

%%% This test case validate title parameter for post function
missing_title_in_post(Config) ->
  {invalid_id,SavedConfig} = ?config(saved_config,Config),
  List = [{<<"user">>,<<"abc">>}],
  {error,Reason} = notes_store:post(List),
  ct:pal("Reason:~p",[Reason]),
  ct:comment("Missing Title Response:~p",[Reason]),
  {saved_config, SavedConfig}.

%%% This test case validate user parameter for post function
missing_user_in_post(_Config) ->
  List = [{<<"title">>,<<"abc">>}],
  {error,Reason} = notes_store:post(List),
  ct:pal("Reason:~p",[Reason]),
  ct:comment("Missing User Response:~p",[Reason]),
  {comment,""}.

%%% This test case validate id parameter for get function
missing_id_in_get(_Config) ->
  List = [],
  {error,Reason} = notes_store:get(List),
  ct:pal("Reason:~p",[Reason]),
  ct:comment("Missing Id Response:~p",[Reason]),
  {comment, ""}.

%%% This test case validate uset parameter for list function
missing_user_in_list(_Config) ->
  List = [],
  {error,Reason} = notes_store:list(List),
  ct:pal("Reason:~p",[Reason]),
  ct:comment("Missing Id Response:~p",[Reason]),
  {comment,""}.

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

decode(Data) ->
  decode1(jiffy:decode(Data)).

decode1({Data}) ->
  Data;
decode1(List) ->
  lists:map(fun({Data})->Data end, List).
