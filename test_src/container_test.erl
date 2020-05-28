%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(container_test). 
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("common_macros.hrl").
%% --------------------------------------------------------------------
      
-define(SOURCE,"https://github.com/joq62/").
-export([start/0]).



%% ====================================================================
%% External functions

%------------------ ceate and delete Pods and containers -------
% create Pod, start container - test application running - delete container
% delete Pod

start()->
    ?debugMsg("create_delete_ok_test"),   
    ?assertEqual(ok,create_delete_ok_test()),
    ?debugMsg("create_delete_NOK_test"),   
    ?assertEqual(ok,create_delete_NOK_test()),
    ok.


create_delete_ok_test()->
    application:stop(list_to_atom("")),
    application:unload(list_to_atom("test_ok_service")),		     
    os:cmd("rm -rf "++"test_ok_service"),
    os:cmd("rm -rf "++"include"),

    ?assertEqual(ok,container:clone("include",?SOURCE)),
    ?assertEqual(ok,container:create("test_ok_service",git,?SOURCE)),
    ?assertEqual(42,test_ok_service:add(20,22)),
    ?assertEqual(ok,container:delete("test_ok_service")),
    ?assertMatch({badrpc,{'EXIT',_}},rpc:call(node(),test_ok_service,add,[20,22])),
    application:stop(list_to_atom("")),
    application:unload(list_to_atom("test_ok_service")),		     
    os:cmd("rm -rf "++"test_ok_service"),    
    os:cmd("rm -rf "++"include"),
    ok.
		 
    

create_delete_NOK_test()->
    application:unload(list_to_atom("test_NOK_service")),		     
    os:cmd("rm -rf "++"test_NOK_service"),
    os:cmd("rm -rf "++"include"),
    ?assertEqual(ok,container:clone("include",?SOURCE)),
    ?assertMatch({error,
		  {error,
		   [compiler_error,
		    [{error,"test_NOK_service/src/test_NOK_service.erl"}],
		    container,_]}},container:create("test_NOK_service",git,?SOURCE)),
    application:unload(list_to_atom("test_NOK_service")),		     
    os:cmd("rm -rf "++"test_NOK_service"),
    os:cmd("rm -rf "++"include"),
    ok.
