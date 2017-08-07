%%%-------------------------------------------------------------------
%% @doc notes_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(notes_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
 
%%Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->

  AChild = {cowboy_plugin,{cowboy_plugin,start_link,[]},
            permanent,2000,worker,[cowboy_plugin]},
  BChild = {notes_store,{notes_store,start_link,[]},
            permanent,2000,worker,[notes_store]},

  {ok, { {one_for_one, 10, 10}, [AChild, BChild]} }.

%%====================================================================
%% Internal functions
%%====================================================================
