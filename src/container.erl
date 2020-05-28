%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(container). 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("common_macros.hrl").

-include_lib("eunit/include/eunit.hrl").
%% --------------------------------------------------------------------


%% External exports

-export([create/3,
	 delete/1,
	 clone/2
	]).


%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function:init 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function:clone_compile(Service,BoardNode)
%% Description:
%% Returns: ok|{erro,compile_info}|{error,nodedown}
%% --------------------------------------------------------------------
delete(ServiceId)->
    delete_container(ServiceId).	

delete_container(ServiceId)->
    Result=case application:stop(list_to_atom(ServiceId)) of
	       ok->
		   PathServiceEbin=filename:join([ServiceId,"ebin"]),
		   case code:del_path(PathServiceEbin) of
		       true->
			   case os:cmd("rm -rf "++ServiceId) of
			       []->
				   ok;
			       Err ->
				   {error,[undefined_error,ServiceId,Err,?MODULE,?LINE]}
			   end;
		       false->
			   {error,[directory_not_found,ServiceId,?MODULE,?LINE]};
		       {error,Err}->
			   {error,[ServiceId,Err,?MODULE,?LINE]};
		       Err ->
			   {error,[undefined_error,ServiceId,Err,?MODULE,?LINE]}
		   end;
	       {error,{not_started,Err}}->
		   {error,[eexists,ServiceId,Err,?MODULE,?LINE]};
	       Err ->
		   {error,[undefined_error,ServiceId,Err,?MODULE,?LINE]}
	   end,
    Result.

%% --------------------------------------------------------------------
%% Function:clone_compile(Service,BoardNode)
%% Description:
%% Returns: ok|{erro,compile_info}|{error,nodedown}
%%
%% PodId/Service
%% --------------------------------------------------------------------
create(ServiceId,Type,Source)->
    case create_container(ServiceId,Type,Source) of
	{ok,ServiceId}->
	    ok;
	Error->
	    {error,Error}
    end.
    
create_container(ServiceId,Type,Source)->
    Result =case filelib:is_dir(ServiceId) of
		true->
		    {error,[service_already_loaded,ServiceId,?MODULE,?LINE]};
		false ->
		    case Type of
			git->
			    ok=clone(ServiceId,Source),
			    case compile(ServiceId) of
				{error,Err}->
				    {error,Err};
				ok->
				    case start(ServiceId) of
					{error,Err}->
					    {error,Err};
					ok->
					    {ok,ServiceId}
				    end
			    end;
			dir->
			    glurk
						%clone(Pod,PodId,{ServiceId,Source})
		    end
	    end,
    timer:sleep(2000),
    Result.


clone(ServiceId,Source)->
    application:stop(list_to_atom(ServiceId)),
    application:unload(list_to_atom(ServiceId)),		     
    os:cmd("rm -rf "++ServiceId),
    ServiceIdGit=ServiceId++".git",
    ServiceUrl=Source++ServiceIdGit,
    os:cmd("git clone "++ServiceUrl),
    ok.

%% --------------------------------------------------------------------
%% Function:clone_compile(Service,BoardNode)
%% Description:
%% Returns: ok|{erro,compile_info}|{error,nodedown}
%% --------------------------------------------------------------------
compile(ServiceId)->
    PathSrc=filename:join(ServiceId,"src"),
    PathEbin=filename:join(ServiceId,"ebin"),
 %   PathTestSrc=filename:join([PodId,ServiceId,"test_src"]),
 %   PathTestEbin=filename:join([PodId,ServiceId,"test_ebin"]),

    PathInclude="include",
    Result=do_compile(ServiceId,PathSrc,PathEbin,PathInclude),
    Result.

do_compile(ServiceId,PathSrc,PathEbin,PathInclude)->
    Result=case file:list_dir(PathSrc) of
	       {ok,Files}->
		   FilesToCompile=[filename:join(PathSrc,File)||File<-Files,filename:extension(File)==".erl"],
		   % clean up ebin dir
		   case os:cmd("rm -rf "++PathEbin++"/*") of
		       []->
			   CompileResult=[{c:c(ErlFile,[{outdir,PathEbin},{i,PathInclude}]),ErlFile}||ErlFile<-FilesToCompile],  
			   case [{R,File}||{R,File}<-CompileResult,error==R] of
			       []->
				   AppFileSrc=filename:join(PathSrc,ServiceId++".app"),
				   AppFileDest=filename:join(PathEbin,ServiceId++".app"),
				   case os:cmd("cp "++AppFileSrc++" "++AppFileDest) of
				       []->
					 %  io:format("~p~n",[{AppFileSrc,AppFileDest,?FILE,?LINE}]),
					   ok;
				       Err ->
					   {error,[undefined_error,ServiceId,Err,?MODULE,?LINE]}
				   end;
			       CompilerErrors->
				   {error,[compiler_error,CompilerErrors,?MODULE,?LINE]}
			   end;
		       Err ->
			   {error,[undefined_error,ServiceId,Err,?MODULE,?LINE]}
		   end;
	       Err ->
		   {error,[undefined_error,ServiceId,Err,?MODULE,?LINE]}
	   end,
    Result.

%% --------------------------------------------------------------------
%% Function:clone_compile(Service,BoardNode)
%% Description:
%% Returns: ok|{erro,compile_info}|{error,nodedown}
%% --------------------------------------------------------------------
start(ServiceId)->
    PathServiceEbin=filename:join([ServiceId,"ebin"]),
    Result = case code:add_path(PathServiceEbin) of
		 true->
		     Service=list_to_atom(ServiceId),
		     case application:start(Service) of
			 ok->
			     ok;
			 {error,{already_started,Service}}->
			     ok;  % Needs to be checked 
			 Err->
			     {error,[undefined_error,ServiceId,Err,?MODULE,?LINE]}
		     end;
		 Err ->
		     {error,[undefined_error,ServiceId,Err,?MODULE,?LINE]}
	     end,
    Result.
