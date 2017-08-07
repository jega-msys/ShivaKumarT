-module(api_handler).

-export([init/3,
         handle/2,
         terminate/3
        ]).

-record(state, {}).

init(_, Req, _Opts) ->
  {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
  {Method, Req2} = cowboy_req:method(Req),
  case process_request(Method, Req2) of
    {ok, ContnentType, Body} ->
      {ok, Req3} = cowboy_req:reply(200,[{<<"content_type">>,ContnentType}],Body,Req2),
      {ok, Req3, State};
    {error, Reason} ->
      Bin = notes_server_util:to_binary(Reason),
      {ok, Req3} = cowboy_req:reply(500,[{<<"content_type">>,<<"text/html">>}],
                                    Bin,Req2),
      {ok, Req3, State}
  end.

%% POST request handler
process_request(<<"POST">>,Req) ->
  case cowboy_req:header(<<"content-type">>,Req) of
    {Form,Req0} when Form=:= <<"application/json; charset=UTF-8">>;
                     Form =:= <<"application/json">>->
      case cowboy_req:body(Req0) of
        {error,Reason} ->
          io:format("~n~p",[Reason]),
          {error, <<"Internal Error">>};
        {ok,JSONBody, _Req1} ->
          case catch jiffy:decode(JSONBody) of
            {JSONList} ->
              process_request(JSONList);
            Any ->
              io:format("~nJSON Error:~p",[Any]),
              {error, <<"Internal Error">>}
          end
      end;
    _Other ->
      {error, <<"Internal Error">>}
  end;
process_request(_, _Req) ->
  {error, <<"Service Not Implemented">>}.

terminate(_Reason, _Req, _State) ->
  ok.
%%===================== Internal Functions ================================
process_request(List) ->
  case proplists:get_value(<<"module">>,List,undefine) of
    undefined ->
      {error,<<"Module Parameter Missing">>};
    Mod ->
      Module = erlang:list_to_atom(binary_to_list(Mod)),
      case proplists:get_value(<<"function">>, List, undefined) of
        undefined ->
          {error,<<"Function Parameter Missing">>};
        Fun ->
          Function = erlang:list_to_atom(binary_to_list(Fun)),
          case catch Module:Function(List) of
            {ok, ContentType, Response} ->
              {ok, ContentType, Response};
            {'EXIT',{undef,_}} ->
              {error, <<"Invalid Funtion/Module">>};
            {'EXIT', Reason} ->
              {error, Reason};
            {error,{undef, _}} ->
              {error, <<"Invalid Funtion/Module">>};
            {error, Reason} ->
              {error, Reason}
          end
     end
  end.
 
