-module(notes_store_client).
-export([get/2,
         list/2,
         post/3,
         post/4
        ]).

-include_lib("eunit/include/eunit.hrl").
-define(APP, notes_client).


%%%===================== API Functions ===============================
%%--------------------------------------------------------------------
%% @doc
%% Reads complete note details from store based on note Id
%%
%% @spec get(Module, Id) -> {ok, List} | {error, Error}
%% @end
%%--------------------------------------------------------------------
get(Module,Id) ->
  Request = [{<<"module">>,Module},
             {<<"id">>, Id},
             {<<"function">>,<<"get">>}
            ],
  request(Request).

%%--------------------------------------------------------------------
%% @doc
%% Lists all notes of a specific user
%%
%% @spec list(Module, User) -> {ok, List} |{error, Error}
%% @end
%%--------------------------------------------------------------------

list(Module, User) ->
   Request = [{<<"module">>,Module},
             {<<"user">>,User},
             {<<"function">>,<<"list">>}
            ],
  request(Request).

%%--------------------------------------------------------------------
%% @doc
%% Adds a note to notes storage
%%
%% @spec post(Module, User, Title) -> {ok, List} |{error, Error}
%% @spec post(Module, User, Title, Note) -> {ok, List}| {error, Error}
%% @end
%%--------------------------------------------------------------------

post(Module, User, Title)  ->
  post(Module, User, Title, <<"">>).

post(Module, User, Title, Note) ->
  Request = [{<<"module">>,Module},
             {<<"user">>, User},
             {<<"function">>,<<"post">>},
             {<<"title">>,Title},
             {<<"note">>, Note}
            ],
  request(Request).

%%%========================== Internal Functions ====================
request(Request) ->
  case catch jiffy:encode({Request}) of
    {'EXIT', Reason} ->
      {error, Reason};
    {error, Reason} ->
      {error, Reason};
    Encoded ->
      Host = get_config(host,"localhost"),
      Port = get_config(port,"8080"),
      Url = list_to_binary(Host++":"++Port),
      Options = [],
      Method = "post",
      Headers = [{<<"Content-Type">>,<<"application/json">>}],
      case invoke_request(Method,Url,Headers,Encoded,Options) of
        {ok, 200, RespHeaders, RespBody} ->
          Data = parse_response_body(RespHeaders, RespBody),
          {ok, Data};
        {ok, 500, RespHeaders, RespBody} ->
          Data = parse_response_body(RespHeaders, RespBody),
          {error, Data};
        {ok, StatusCode, Headers, Body} ->
          io:format("~n StatusCode:~p , Headers:~p, Body:~p",
                   [StatusCode, Headers, Body]),
          {error, <<"ClientInternalError">>};
        {error, Reason} ->
          io:format("~nInternalError::Reason:~p",[Reason]),
          {error, <<"ClientInternalError">>}
      end
  end.

invoke_request(Method, Url, Headers, Payload, Options) ->
  case hackney:request(Method, Url, Headers, Payload, Options) of
    {ok, StatusCode, RespHeaders, ClientRef} ->
      case hackney:body(ClientRef) of
        {ok, Body} ->
          {ok, StatusCode, RespHeaders, Body};
        Error ->
          Error
      end;
    Error ->
      Error
  end.

parse_response_body(RespHeaders, RespBody) ->
  case proplists:get_value(<<"content_type">>, RespHeaders) of
    <<"application/json">> ->
      Data = jiffy:decode(RespBody),
      format_jiffy_list(Data);
    _ ->
      RespBody
  end.

get_config(Key, Default) ->
  case application:get_env(?APP, Key) of
    undefined ->
      Default;
    {ok, Val} ->
      Val
  end.

format_jiffy_list({List}) ->
  List;
format_jiffy_list(List) ->
  lists:map(fun({KeyValL})->
                KeyValL;
               (KeyVal) ->
                KeyVal
            end, List).


