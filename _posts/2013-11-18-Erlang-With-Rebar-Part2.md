---
layout: post
published: true
---

## Purpose

Part 1 described how to setup a new application and release with rebar.  This Part describes how to add functionality for a server and then make it **distributed**.

## Step 1 - Add a **gen_server** to respond to an action

1. Create a new module using the behaviour(gen_server)

```erlang
-module(poke_server).
-behaviour (gen_server).

-export([start_link/0]).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	{ok,[]}.

handle_call(Atom, From, State) ->
    {reply, [], State}.

handle_cast(Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    ok.

code_change(OldVsn, State, Extra) ->
    {ok, State}.
```
2. Update to include functionality to handle *poke* and to get *numberOfPokes*.
Note: I have also cleaned up the code to remove unused variables

```erlang
-module(poke_server).
-behaviour (gen_server).

-export([poke/0,numberOfPokes/0]).

-export([start_link/0,init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-record(state,{numberOfPokes = 0}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	{ok,[]}.

poke() -> 
	gen_server:call(?MODULE,poke).

numberOfPokes() ->
	gen_server:call(?MODULE,numberOfPokes).

% Callbacks

%handle_call(Atom, From, State) ->
handle_call(poke, _, State) ->
	NewNumberOfPokes = State#state.numberOfPokes+1,
	NewState = State#state{numberOfPokes = NewNumberOfPokes},
	Reply = {ok, NewNumberOfPokes},
    {reply, Reply, NewState};

handle_call(numberOfPokes, _, State) ->
	Reply = State#state.numberOfPokes,
    {reply, Reply, State}.

%handle_cast(Msg, State) ->
handle_cast(_, State) ->
    {noreply, State}.

%handle_info(Info, State) ->
handle_info(_, State) ->
    {noreply, State}.

%terminate(Reason, State) ->
terminate(_, _) ->
    ok.

%code_change(OldVsn, State, Extra) ->
code_change(_, State, _) ->
    {ok, State}.
```
3. Compile using rebar to make sure everything is working
```
rebar compile
```
4. Now we need to add the server as a worker process under the supervisor.
The supervisor was created as part of the rebar template and uses the standard naming convention <appName>_sup
Edit the poke_sup.erl supervisor to start up the new poke server(poke_server.erl).  Here is how the supervisor needs to be updated
From:

```erlang
-module(poke_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

```
To:

```erlang
-module(poke_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Children = [?CHILD(poke_server,worker)],
    {ok, { {one_for_one, 5, 10}, Children} }.

```
5. Recompile and generate the release

```
rebar compile generate
```
6. Now run the application in a console

```
.\rel\poke\bin\poke console
%-> In the erlang console you can use the poke_server like:
%-> poke_server:poke().
%-> {ok,1}
%-> poke_server:numberOfPokes().
%-> 1
```

## Step 2 - Make the process distributed

Now that we have a server responding to requests on a single process we can move to the next step and make it distributed using another process on the same machine.  Before we do this there are a couple of things to note:

1. Rebar release console
  - When rebar creates the release it provides scripts in the bin folder for you to execute
  - When you run *.\rel\poke\bin\poke console* this runs the script *.\rel\poke\bin\poke.cmd*
  	- To see the command that gets executed go into the poke.cmd and see what gets run
  - When sending the console command to your release it executes the following:
  
  ```
  @start "%node_name% console" %werl% -boot "%node_boot_script%" -config "%sys_config%" -args_file "%vm_args%" -sname %node_name%
  ```
  - The importnant thing to node is it uses *short names* to spin up the erlang shell(-sname %node_name%).  For distributed erlang you will have to spin up the other erlang shell using -sname inorder to communicate
  -The other important thing to look at is the vm.args file (rel/releases/1/vm.args).  This specifies the arguments to be sent into the erlang console. The one important thing to note is that the *cookie* has been set to the name of the app(-setcookie poke)

Now that you know where to find the information that rebar creates for you we can create a distributed app

1. Start up the application 

```
.\rel\poke\bin\poke console
```
2. Open up another command with an erlang process

```
erl -sname poke_client -setcookie poke
```
3. Now type in the following to send a command to your other process

```
% rpc:call(%host%,%module%,%method%,%args%)
rpc:call('poke@localhost',poke_server,poke,[]).
```

That is it now two separate erlang processes are communicating to each other independent of one another.  As a note if you type `nodes()` into the command prompt on each process you should see a reference to one another

## References
* [github repository of the poke application](https://github.com/tjchaplin/poke)
* [Erlang gen-server behavior](http://www.erlang.org/doc/design_principles/gen_server_concepts.html)
* [Richard Jones - Tutorial on hot swapping](http://www.metabrew.com/article/erlang-rebar-tutorial-generating-releases-upgrades)