%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lib_service_tests). 
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("common_macros.hrl").
%% --------------------------------------------------------------------

%% External exports
-export([start/0]).



%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function:tes cases
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
start()->
    spawn(fun()->eunit:test({timeout,60,lib_service}) end).

cases_test()->
    ?debugMsg("Test system setup"),
    setup(),
    %% Start application tests
    ?debugMsg("container service"),    
    ?assertEqual(ok,container_test:start()),
    ?debugMsg("get call"),   
 
    ?debugMsg("delete call"),   

    ?debugMsg("clear call"),   

    ?debugMsg("Start stop_test_system:start"),
    %% End application tests
    cleanup(),
    ok.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
setup()->
    ?assertEqual(ok,application:start(lib_service)),
    Node=node(),
    ?assertEqual({pong,Node,lib_service},lib_service:ping()),    
    ?assertEqual(ok,application:stop(lib_service)),  
    ok.


cleanup()->
    init:stop().




