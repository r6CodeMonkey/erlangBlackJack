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

leave(Pid) -> gen_server:cast(Pid, terminate).

%% make one player game for time being...

%%% Server functions
init([]) -> {ok, blackjack_table:start()}.

handle_call({enter, Wager}, _From, {Cards, Dealer, Player}) ->
   [D1,D2, D3, D4|Deck] = Cards,
   NewPlayer = blackjack_player:create_player(_From, Wager),
   NewDealer = blackjack_player:create_player("Dealer", Wager),
   UpdatedPlayer = blackjack_player:update_cards(NewPlayer, D1, D2),
   UpdatedDealer = blackjack_player:update_cards(NewDealer, D3, D4),  
   
 if	UpdatedPlayer#player.handValue == 21; UpdatedPlayer#player.alternateValue == 21 -> {reply, io:format("Your Cards ~p,~p~nBlackJack - You Win ~p Winnings~n",[D1,D2,UpdatedPlayer#player.balance*2]), {Deck, Dealer, UpdatedPlayer}};
   true -> {reply,io:format("Your cards ~p,~p~nDealer cards ~p,~p~n", [D1, D2, D3, D4]), {Deck,UpdatedDealer, UpdatedPlayer}}
 end;  

handle_call({hit}, _From, {Cards, Dealer, Player}) -> 
 Card = hd(Cards),
	UpdatedPlayer = blackjack_player:update_card(Player, Card, Player#player.balance), 
 if UpdatedPlayer#player.handValue > 21, UpdatedPlayer#player.alternateValue > 21  -> {reply, io:format("Your Card ~p~nBust - Dealer Wins~n",[Card]), {Cards,Dealer, UpdatedPlayer}};
    UpdatedPlayer#player.handValue == 21; UpdatedPlayer#player.alternateValue == 21 -> {reply, io:format("Your Card ~p~nBlackJack - You Win ~p Winnings~n",[Card, UpdatedPlayer#player.balance*2]), {Cards, Dealer, UpdatedPlayer}};
   true -> {reply,{io:format("hand value ~p, alternate value ~p ~n",[UpdatedPlayer#player.handValue,UpdatedPlayer#player.alternateValue]), Card}, {tl(Cards),Dealer, UpdatedPlayer}}
end;

handle_call({surrender}, _From, {Cards, Dealer, Player}) -> 
 {reply, io:format("Returned ~p ~n", [Player#player.balance/2]), {Cards, Dealer, Player}};

handle_call({split}, _From, {Cards, Dealer, Player}) -> 
%% the tricky one,  firstly are the cards the same....
 [Card1, Card2 | Rest] = Player#player.cards,
 if Card1#card.value == Card2#card.value -> 
 UpdatedPlayer = blackjack_player:split_cards(Player),
 {reply, io:format("You can split deck~p~n~p~n", [UpdatedPlayer#player.cards, UpdatedPlayer#player.split_cards]), {Cards, Dealer, UpdatedPlayer}};       
  true -> {reply, io:format("You can not split deck~n", []), {Cards, Dealer, Player}}
  end;

handle_call({double_down, Wager}, _From, {Cards, Dealer, Player}) ->

 [PlayerCard, DealerCard|Rest] = Cards,
 UpdatedPlayer = blackjack_player:update_card(Player,PlayerCard, Player#player.balance+Wager),
 UpdatedDealer = blackjack_player:update_card(Dealer,DealerCard, Dealer#player.balance+Wager),
 
 process_result(UpdatedPlayer, UpdatedDealer, Rest);
  
handle_call({stand}, _From, {Cards, Dealer, Player}) ->

 UpdatedPlayer = player_stand(Player),
 %% dealer plays.
 UpdatedDealer = dealer_twist(Cards, Dealer, UpdatedPlayer),
 
 FinalDealer = dealer_stand(UpdatedDealer),
  
 process_result(UpdatedPlayer,FinalDealer, Cards);
 
handle_call(terminate,_From, {Cards, Dealer, Player}) ->
 {stop, normal, ok, Cards}.
  
handle_cast({leave}, {Cards, Dealer, Player}) ->  io:format("stand cast ~n", []) .  

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
dealer_twist(Cards, Dealer,  Player) when Dealer#player.handValue > 16, Dealer#player.alternateValue > 16 -> Dealer;
dealer_twist(Cards, Dealer,  Player) when Dealer#player.handValue > Player#player.handValue, Dealer#player.alternateValue > Player#player.handValue -> Dealer;
dealer_twist(Cards, Dealer,  Player) -> 
  Card = hd(Cards),
   UpdatedDealer = blackjack_player:update_card(Dealer, Card, Dealer#player.balance),
  dealer_twist(tl(Cards),UpdatedDealer,  Player).

player_stand(Player) -> if Player#player.handValue > 21 -> UpdatedPlayer = Player#player{handValue=Player#player.alternateValue};
   Player#player.alternateValue > 21 -> UpdatedPlayer = Player#player{alternateValue=Player#player.handValue};
   Player#player.handValue > Player#player.alternateValue -> UpdatedPlayer = Player#player{alternateValue=Player#player.handValue};
   Player#player.alternateValue > Player#player.handValue -> UpdatedPlayer = Player#player{handValue=Player#player.alternateValue};
   true -> UpdatedPlayer = Player
  end.
  
dealer_stand(Dealer) ->   if Dealer#player.handValue > 21,Dealer#player.alternateValue > 21 -> FinalDealer = Dealer#player{handValue=Dealer#player.alternateValue};
   Dealer#player.handValue > 21 -> FinalDealer = Dealer#player{handValue=Dealer#player.alternateValue};
   Dealer#player.alternateValue > 21 -> FinalDealer = Dealer#player{alternateValue=Dealer#player.handValue};
   Dealer#player.handValue > Dealer#player.alternateValue -> FinalDealer = Dealer#player{alternateValue=Dealer#player.handValue};
   Dealer#player.alternateValue > Dealer#player.handValue -> FinalDealer = Dealer#player{handValue=Dealer#player.alternateValue};
   true -> FinalDealer = Dealer
  end.
  
process_result(Player, Dealer, Cards) ->  
  if Dealer#player.handValue > 21 -> {reply, io:format("You win ~p winnings~nYour Cards~p~nDealer Cards ~p~nDealer went bust~n", [Player#player.balance*2, Player#player.cards, Dealer#player.cards]), {Cards, Dealer, Player}};
     Player#player.handValue > Dealer#player.handValue -> {reply, io:format("Your Cards~p~nDealer Cards ~p~nYou win ~p winnings, Dealer has ~p~n", [Player#player.cards,Dealer#player.cards,Player#player.balance*2, Dealer#player.handValue]), {Cards, Dealer, Player}};
     Dealer#player.handValue > Player#player.handValue -> {reply, io:format("Your Cards~p~nDealer Cards ~p~nDealer wins, has ~p~n", [Player#player.cards,Dealer#player.cards,Dealer#player.handValue]), {Cards, Dealer, Player}};
	 true ->  {reply, io:format("Tie,Your Cards~p~nDealer Cards ~p~nyour stake ~p returned~n", [Player#player.cards,Dealer#player.cards, Player#player.balance]), {Cards, Dealer, Player}}
  end.

