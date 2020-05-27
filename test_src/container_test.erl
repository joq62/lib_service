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
    os:cmd("rm -rf adder_service"),
    ?assertEqual(ok,create_delete_test()),
    ok.


create_delete_test()->
    {PodId,_}=misc_lib:get_vm_id(),
    ?assertEqual(glurk,container:create("adder_service",git,?SOURCE)),
    ?assertEqual(42,adder_service:add(20,22)),
    ?assertEqual(glurk,container:delete("adder_service")),
    ?assertEqual(42,adder_service:add(20,22)),
    ok.
		 
    
