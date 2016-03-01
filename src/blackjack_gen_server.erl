-module(blackjack_gen_server).
-behaviour(gen_server).

-export([join_table/0, enter_game/2, hit/1, stand/1, double_down/2, surrender/1, split/1, leave/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("blackjack_records.hrl").

join_table() ->
gen_server:start_link(?MODULE, [], []).

%%% start game by placing a wager and receive 2 cards..
enter_game(Pid, Wager) ->
gen_server:call(Pid, {enter, Wager}).

%%% we want a card 
hit(Pid) ->
gen_server:call(Pid, {hit}).

%%% we are sticking, asynch call
stand(Pid) ->
gen_server:call(Pid, {stand}).

%%% increase wager on bet, receive 1 more card
double_down(Pid, Wager) ->
gen_server:call(Pid, {double_down, Wager}).

%%% give up and get half stake back
surrender(Pid) ->
gen_server:call(Pid,{surrender}).

%%% if we have the same card twice, we can split the deck
split(Pid) ->
gen_server:call(Pid, {split}).

leave(Pid) -> gen_server:call(Pid, terminate).

%% make one player game for time being...

%%% Server functions
init([]) -> {ok, blackjack_table:start()}.

handle_call({enter, Wager}, _From, {Cards, Dealer, Player}) ->
   [D1,D2, D3, D4|Deck] = Cards,
   NewPlayer = blackjack_player:create_player(_From, Wager),
   UpdatedPlayer = NewPlayer#player{handValue=blackjack_deck:get_card_value(D1#card.value)+blackjack_deck:get_card_value(D2#card.value)
	, alternateValue=blackjack_deck:get_alternate_card_value(D1#card.value)+blackjack_deck:get_alternate_card_value(D2#card.value)},
   {reply,io:format("Your cards ~p,~p~nDealer cards ~p,~p~n", [D1, D2, D3, D4]), {Deck,[D3|D4], UpdatedPlayer}};

handle_call({hit}, _From, {Cards, Dealer, Player}) -> 
 Card = hd(Cards),
 Value = blackjack_deck:get_card_value(Card#card.value)+Player#player.handValue,
 Value2 = blackjack_deck:get_alternate_card_value(Card#card.value)+Player#player.alternateValue,
	UpdatedPlayer = Player#player{handValue=Value, alternateValue=Value2},
 if UpdatedPlayer#player.handValue > 21, UpdatedPlayer#player.alternateValue > 21  -> {reply, "Bust - Dealer Wins", {Cards,Dealer, UpdatedPlayer}};
    UpdatedPlayer#player.handValue == 21; UpdatedPlayer#player.alternateValue == 21 -> {reply, io:format("BlackJack - You Win ~p Winnings~n",[UpdatedPlayer#player.balance*2]), {Cards, Dealer, UpdatedPlayer}};
   true -> {reply, {io:format("hand value ~p, alternate value ~p ~n",[UpdatedPlayer#player.handValue,UpdatedPlayer#player.alternateValue]), Card}, {tl(Cards),Dealer, UpdatedPlayer}}
end;

handle_call({surrender}, _From, {Cards, Dealer, Player}) -> 
 {reply, io:format("Returned ~p ~n", [Player#player.balance/2]), {Cards, Dealer, Player}};

handle_call({split, wager}, _From, {Cards, Dealer, Player}) -> 
 io:format("split call ~n", []);

handle_call({double_down, wager}, _From, {Cards, Dealer, Player}) -> 
 io:format("double down call ~n", []);
 
handle_call({stand}, _From, {Cards, Dealer, Player}) ->
 %% dealer now needs to deal two cards...in fact they should of done that first...
 io:format("stand cast ~n", []);
 
handle_call(terminate,_From, {Cards, Dealer, Player}) ->
 {stop, normal, ok, Cards}.
  
handle_cast({value}, {Cards, Dealer, Player}) ->  io:format("stand cast ~n", []) .  


handle_info(Msg, Wager) ->
io:format("unexpected message: ~p~n", [Msg]).

code_change(_OldVsn, State, _Extra) ->
    %% No change planned. The function is there for the behaviour,
    %% but will not be used. Only a version on the next
    {ok, State}. 

terminate(normal, Cards) ->
[io:format("Player has left  table~n",[])],
ok.


%%% private server functions to get cards / manage cards...ideally we have a seperate engine for the cards and table.