%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(dns_service_tests). 
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("common_macros.hrl").
%% --------------------------------------------------------------------

-define(TEST_VECTOR,[{"s1","ip1",1},{"s11","ip1",2},
		     {"s2","ip2",1},{"s21","ip1",1},
		     {"s3","ip1",2},{"s21","ip1",1},
		     {"s1","ip2",1},{"s21","ip2",1}]).

%% External exports
%-export([start/0]).
-compile(export_all).


%% ====================================================================
%% External functions
%% ====================================================================
start()->
    spawn(fun()->eunit:test({timeout,2*60,dns_service}) end).


cases_test()->
    ?debugMsg("Test system setup"),
    setup(),
    ?debugMsg("add and all calls"),    
    add_all_services(),
    ?debugMsg("get call"),   
    get_services(),
    ?debugMsg("delete call"),   
    delete_services(),    

    ?debugMsg("clear call"),   
    clear_services(),  
    ?debugMsg("Start stop_test_system:start"),
    cleanup(),
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
setup()->
    ?assertEqual(ok,application:start(dns_service)),
    Node=node(),
    ?assertEqual({pong,Node,dns_service},dns_service:ping()),    
    ?assertEqual(ok,application:stop(dns_service)),  
    ok.


cleanup()->
    init:stop().



%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
add_all_services()->
    ?assertEqual(ok,application:start(dns_service)),
    Node=node(),
    ?assertEqual({pong,Node,dns_service},dns_service:ping()),    
    [dns_service:add(S,I,P)||{S,I,P}<-?TEST_VECTOR],
    [dns_service:add(S,I,P)||{S,I,P}<-?TEST_VECTOR],
    ?assertEqual([{"s21","ip2",1},
		  {"s1","ip2",1},
		  {"s21","ip1",1},
		  {"s3","ip1",2},
		  {"s2","ip2",1},
		  {"s11","ip1",2},
		  {"s1","ip1",1}],dns_service:all()),
    ?assertEqual(ok,application:stop(dns_service)),  
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
get_services()->
    ?assertEqual(ok,application:start(dns_service)),
    Node=node(),
    ?assertEqual({pong,Node,dns_service},dns_service:ping()),   
    [dns_service:add(S,I,P)||{S,I,P}<-?TEST_VECTOR],
    [dns_service:add(S,I,P)||{S,I,P}<-?TEST_VECTOR],
    ?assertEqual([{"ip2",1},
		  {"ip1",1}],dns_service:get("s21")),
    ?assertEqual([{"ip1",2}],dns_service:get("s3")),
    ?assertEqual([],dns_service:get("glurk")),
    ?assertEqual(ok,application:stop(dns_service)),  
    ok.    
  
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
delete_services()->
    ?assertEqual(ok,application:start(dns_service)),
    Node=node(),
    ?assertEqual({pong,Node,dns_service},dns_service:ping()),   
    [dns_service:add(S,I,P)||{S,I,P}<-?TEST_VECTOR],
    [dns_service:add(S,I,P)||{S,I,P}<-?TEST_VECTOR],

    ?assertEqual([{"ip2",1},
		  {"ip1",1}],dns_service:get("s21")),    
    ?assertEqual(ok,dns_service:delete("s21","ip2",1)),
    ?assertEqual([{"ip1",2}],dns_service:get("s3")),
    dns_service:delete("s3","ip1",2),
    dns_service:delete("s1","glurk",1),  
    
    ?assertEqual([{"ip1",1}],dns_service:get("s21")),
    ?assertEqual([],dns_service:get("s3")),
    ?assertEqual([],dns_service:get("glurk")),
    ?assertEqual(ok,application:stop(dns_service)),  
    ok.    
  


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

clear_services()->
    ?assertEqual(ok,application:start(dns_service)),
    Node=node(),
    ?assertEqual({pong,Node,dns_service},dns_service:ping()),    
    [dns_service:add(S,I,P)||{S,I,P}<-?TEST_VECTOR],
    [dns_service:add(S,I,P)||{S,I,P}<-?TEST_VECTOR],
    ?assertEqual([{"s21","ip2",1},
		  {"s1","ip2",1},
		  {"s21","ip1",1},
		  {"s3","ip1",2},
		  {"s2","ip2",1},
		  {"s11","ip1",2},
		  {"s1","ip1",1}],dns_service:all()),
    ?assertEqual(ok,dns_service:clear()),
    ?assertEqual([],dns_service:all()),
    ?assertEqual(ok,application:stop(dns_service)),  
    ok.
