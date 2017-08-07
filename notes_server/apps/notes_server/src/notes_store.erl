%%%-------------------------------------------------------------------
%%% @author shiva
%%% @copyright (C) 2017, shiva
%%% @doc
%%%
%%% @end
%%% Created : 2017-08-06 23:53:03.500479
%%%-------------------------------------------------------------------
-module(notes_store).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([get/1,
         list/1,
         post/1
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(notes,{id,user,title,note}).
-record(state, {}).

%%%===================================================================
%%% API Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Retries completed record of notes based on Id
%%
%% @spec get(List) -> {ok, ContentType, Data} |{error, Error}
%% @end
%%--------------------------------------------------------------------

get(List) ->
  gen_server:call(?SERVER, {get, List}).

%%--------------------------------------------------------------------
%% @doc
%% Retries all notes of specific user from store DB
%%
%% @spec list(List) -> {ok, ContentType, Data} |{error, Error}
%% @end
%%--------------------------------------------------------------------

list(List) ->
  gen_server:call(?SERVER, {list, List}).

%%--------------------------------------------------------------------
%% @doc
%% Stores a note into db
%%
%% @spec post(List) -> {ok, ContentType, Data} |{error, Error}
%% @end
%%--------------------------------------------------------------------

post(List) ->
  gen_server:call(?SERVER, {post, List}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
  init_mnesia_schema(),
  create_table(),
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get, List}, _From, State) ->
  Reply = handle_get(List),
  {reply, Reply, State};
handle_call({list, List}, _From, State) ->
  Reply = handle_list(List),
  {reply, Reply, State};
handle_call({post, List}, _From, State) ->
  Reply = handle_post(List),
  {reply, Reply, State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates disc based mnesia schema to persist schema definitions
%%
%% @spec init_mnesia_schema() -> ok| {'EXIT', Reason}.
%% @end
%%--------------------------------------------------------------------

init_mnesia_schema() ->
  mnesia:change_table_copy_type(schema, node(), disc_copies).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates disc based notes table in mnesia database
%%
%% @spec create_table() -> ok| {'EXIT', Reason}.
%% @end
%%--------------------------------------------------------------------

create_table() ->
  mnesia:create_table(notes,[{attributes,record_info(fields, notes)},
                             {disc_copies,[node()]},
                             {index,[user]}]).

handle_get(List) ->
  case proplists:get_value(<<"id">>, List) of
    undefined ->
      {error, <<"id parameter is required">>};
    Id ->
      case mnesia:dirty_read(notes, Id) of
        [] ->
          {error, <<"Invalid Note Id">>};
        [Rec] ->
          KeyValList = rec_to_list(Rec),
          Encoded = jiffy:encode({KeyValList}),
          {ok, <<"application/json">>, Encoded}
      end
  end.

handle_list(List) ->
  case proplists:get_value(<<"user">>, List) of
    undefined ->
      {error, <<"user parameter is required">>};
    User  ->
      case mnesia:dirty_index_read(notes, User, #notes.user) of
        [] ->
          Encoded = jiffy:encode({[]}),
          {ok, <<"application/json">>, Encoded};
        Recs ->
          KeyValList = list_recs(Recs),
          Encoded = jiffy:encode(KeyValList),
          {ok, <<"application/json">>, Encoded}
      end
  end.

handle_post(List) ->
  case proplists:get_value(<<"user">>, List) of
    undefined ->
      {error, <<"user parameter is required">>};
    User ->
      case proplists:get_value(<<"title">>, List) of
        undefined ->
          {error, <<"title parameter is required">>};
        Title ->
          Note = proplists:get_value(<<"note">>, List, []),
          Id = proplists:get_value(<<"id">>, List, undefined),
          store_note(Id, User, Title, Note)
      end
  end.

rec_to_list(Rec) ->
  Attributes = record_info(fields, notes),
  [_|Values] = tuple_to_list(Rec),
  KeyValList = lists:zip(Attributes, Values),
  KeyValList.

list_recs(Recs) ->
  lists:map(fun rec_to_keyval/1, Recs).

rec_to_keyval(Rec) ->
  List = [{id, Rec#notes.id},{title, Rec#notes.title}],
  {List}.

store_note(undefined, User, Title, Note) ->
  IdBin = erlang:integer_to_binary(erlang:system_time()),
  Rec = #notes{
           id = IdBin,
           user = User,
           title = Title,
           note = Note
          },
  ok = mnesia:dirty_write(Rec),
  List = [{id, IdBin}],
  Encoded = jiffy:encode({List}),
  {ok, <<"application/json">>, Encoded};
store_note(Id, _User, Title, Note) ->
  case mnesia:dirty_read(notes, Id) of
    [] ->
      {error, <<"Invalid Note Id">>};
    [Rec] ->
      RecU = Rec#notes{title = Title,
                  note = Note
                 },
      ok = mnesia:dirty_write(RecU),
      List = [{id, Id}],
      Encoded = jiffy:encode({List}),
      {ok, <<"application/json">>, Encoded}
  end.


