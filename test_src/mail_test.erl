%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(mail_test). 
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("common_macros.hrl").
%% --------------------------------------------------------------------


-export([start/0]).
%-compile(start/0).



%% ====================================================================
%% External functions


start()->
    send("Test1","Hej","joakim.leche@gmail.com",
	 "service.varmdo@gmail.com","service.varmdo@gmail.com","Festum01"),

    ok.



send(Subject,Msg,Receiver,Sender,UserId,PassWd)->
    mail:send_mail(Subject,Msg,Receiver,Sender,UserId,PassWd).
