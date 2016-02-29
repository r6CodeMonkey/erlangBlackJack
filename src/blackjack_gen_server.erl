-module(blackjack_gen_server).
-behaviour(gen_server).

-export([join_table/0, enter_game/2, hit/1, stand/1, double_down/2, surrender/1, split/1, leave/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

join_table() ->
gen_server:start_link(?MODULE, [], []).

%%% start game by placing a wager...a sync call wait for dealer to start 
enter_game(Pid, Wager) ->
gen_server:cast(Pid, {enter, wager}).

%%% we want a card 
hit(Pid) ->
gen_server:call(Pid, {hit}).

%%% we are sticking, asynch call
stand(Pid) ->
gen_server:cast(Pid, {stand}).

%%% increase wager on bet, receive 1 more card
double_down(Pid, Wager) ->
gen_server:call(Pid, {double_down, wager}).

%%% give up and get half stake back
surrender(Pid) ->
gen_server:call(Pid,{surrender}).

%%% if we have the same card twice, we can split the deck
split(Pid) ->
gen_server:call(Pid, {split}).

leave(Pid) -> gen_server:call(Pid, terminate).



%%% Server functions
init([]) -> {ok, []}.

handle_call({hit, wager}, _From, Cards) -> 
io:format("hit call ~n", []);
handle_call({surrender, wager}, _From, Cards) -> 
io:format("surrender call ~n", []);
handle_call({split, wager}, _From, Cards) -> 
io:format("split call ~n", []);
handle_call({double_down, wager}, _From, Cards) -> 
io:format("double down call ~n", []).

handle_cast({enter, Wager}, Cards) ->
io:format("enter cast ~n", []);
handle_cast({stand, Wager}, Cards) ->
io:format("stand cast ~n", []).

handle_info(Msg, Wager) ->
io:format("unexpected message: ~p~n", [Msg]).

code_change(_OldVsn, State, _Extra) ->
    %% No change planned. The function is there for the behaviour,
    %% but will not be used. Only a version on the next
    {ok, State}. 

terminate(normal, Wager) ->
[io:format("Player has left  table~n",[])],
ok.


%%% private server functions to get cards / manage cards...ideally we have a seperate engine for the cards and table.