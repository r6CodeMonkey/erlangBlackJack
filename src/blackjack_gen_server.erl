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
   
   HandValue = blackjack_player:get_hand_value(UpdatedPlayer#player.cards,0),
   AltValue = blackjack_player:get_alternate_hand_value(UpdatedPlayer#player.cards,0),
   DealerHandValue = blackjack_player:get_hand_value(UpdatedDealer#player.cards,0),
   DealerAltValue = blackjack_player:get_alternate_hand_value(UpdatedDealer#player.cards,0),
   
   
 if	HandValue == 21; AltValue == 21 -> 
   {reply, io:format("Your Cards ~p,~p~nBlackJack - You Win ~p Winnings~n",[D1,D2,UpdatedPlayer#player.balance*2]), {Deck, Dealer, UpdatedPlayer}};
   DealerHandValue == 21; DealerAltValue == 21 -> 
   {reply, io:format("Your Cards ~p,~p~Dealer Has BlackJack - Stake Returned ~p~n",[D1,D2,UpdatedPlayer#player.balance]), {Deck, Dealer, UpdatedPlayer}};
   true -> {reply,io:format("Your cards ~p,~p~nDealer cards ~p,~p~n", [D1, D2, D3, D4]), {Deck,UpdatedDealer, UpdatedPlayer}}
 end;  

handle_call({hit}, _From, {Cards, Dealer, Player}) -> 
 Card = hd(Cards),
if Player#player.split_cards == [] ->
	UpdatedPlayer = blackjack_player:update_card(Player, Card, Player#player.balance), 
	HandValue = blackjack_player:get_hand_value(UpdatedPlayer#player.cards,0),
	AltValue = blackjack_player:get_alternate_hand_value(UpdatedPlayer#player.cards,0),
 if HandValue > 21, AltValue > 21  -> 
     {reply, io:format("Your Card ~p~nBust - Dealer Wins~n",[Card]), {Cards,Dealer, UpdatedPlayer}};
    HandValue == 21; AltValue == 21 -> 
	 {reply, io:format("Your Card ~p~nBlackJack - You Win ~p Winnings~n",[Card, UpdatedPlayer#player.balance*2]), {Cards, Dealer, UpdatedPlayer}};
   true -> {reply,io:format("Cards ~p ~n",[UpdatedPlayer#player.cards]), {tl(Cards),Dealer, UpdatedPlayer}}
end;
 true -> 
  if length(Player#player.cards) == length(Player#player.split_cards) -> 
     UpdatedPlayer = blackjack_player:update_card(Player, Card, Player#player.balance),
	 handle_call({hit}, _From, {tl(Cards), Dealer, UpdatedPlayer});
  true -> UpdatedPlayer = blackjack_player:update_split_card(Player, Card, Player#player.balance),
    {reply,io:format("Cards ~p~n~p ~n",[UpdatedPlayer#player.cards,UpdatedPlayer#player.split_cards]), {tl(Cards),Dealer, UpdatedPlayer}}
  end
end;

handle_call({surrender}, _From, {Cards, Dealer, Player}) -> 
 {reply, io:format("Returned ~p ~n", [Player#player.balance/2]), {Cards, Dealer, Player}};

handle_call({split}, _From, {Cards, Dealer, Player}) -> 
%% the tricky one,  firstly are the cards the same....
 [Card1, Card2 | Rest] = Player#player.cards,
 if Card1#card.value == Card2#card.value -> 
 UpdatedPlayer = blackjack_player:split_cards(Player),
 {reply, io:format("~p~n~p~n", [UpdatedPlayer#player.cards, UpdatedPlayer#player.split_cards]), {Cards, Dealer, UpdatedPlayer}};       
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
 
 FinalDealer = player_stand(UpdatedDealer),
  
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


dealer_twist(Cards, Dealer,  Player) -> 
  Card = hd(Cards),
   UpdatedDealer = blackjack_player:update_card(Dealer, Card, Dealer#player.balance),
   
   HandValue = blackjack_player:get_hand_value(UpdatedDealer#player.cards,0),
   AltValue = blackjack_player:get_alternate_hand_value(UpdatedDealer#player.cards,0),
   
   if HandValue > 16,AltValue > 16 -> UpdatedDealer;
      HandValue < 21, HandValue >= Player#player.handValue; AltValue < 21, AltValue >= Player#player.handValue -> UpdatedDealer;
	  true -> dealer_twist(tl(Cards),UpdatedDealer,  Player)
end.

player_stand(Player) -> 

HandValue = blackjack_player:get_hand_value(Player#player.cards,0),
AltValue = blackjack_player:get_alternate_hand_value(Player#player.cards,0),
if HandValue > 21,AltValue > 21 -> 
       UpdatedPlayer = Player#player{handValue=AltValue};
   HandValue > 21 -> 
       UpdatedPlayer = Player#player{handValue=AltValue};
   AltValue > 21 -> 
       UpdatedPlayer = Player#player{handValue=HandValue};
   HandValue > AltValue -> 
       UpdatedPlayer = Player#player{handValue=HandValue};
   AltValue > HandValue -> 
       UpdatedPlayer = Player#player{handValue=AltValue};
   true -> UpdatedPlayer = Player#player{handValue=HandValue}
  end.
  
  
process_result(Player, Dealer, Cards) ->  
  if Dealer#player.handValue > 21  -> 
      {reply, io:format("You win ~p winnings~nYour Cards~p~nDealer Cards ~p~nDealer went bust~n", [Player#player.balance*2, Player#player.cards, Dealer#player.cards]), {Cards, Dealer, Player}};
     Player#player.handValue > Dealer#player.handValue -> 
	  {reply, io:format("Your Cards~p~nDealer Cards ~p~nYou win ~p winnings, Dealer has ~p~n", [Player#player.cards,Dealer#player.cards,Player#player.balance*2, Dealer#player.handValue]), {Cards, Dealer, Player}};
     Dealer#player.handValue > Player#player.handValue -> 
	  {reply, io:format("Your Cards~p~nDealer Cards ~p~nDealer wins, has ~p~n", [Player#player.cards,Dealer#player.cards,Dealer#player.handValue]), {Cards, Dealer, Player}};
	 true ->  {reply, io:format("Tie,Your Cards~p~nDealer Cards ~p~nyour stake ~p returned~n", [Player#player.cards,Dealer#player.cards, Player#player.balance]), {Cards, Dealer, Player}}
  end.

