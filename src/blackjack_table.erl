-module(blackjack_table).
-compile(export_all).

-include("blackjack_records.hrl").

%% contains deck
start() ->
Decks = blackjack_deck:create_decks(lists:seq(1,4), []),
{blackjack_deck:shuffle(lists:seq(1,1000), Decks),"",""}.

dealer_twist(Cards, Dealer,  Player) -> 
  Card = hd(Cards),
   UpdatedDealer = blackjack_player:update_player(Dealer, blackjack_player:update_cards([Card],Dealer#player.cards), [], Dealer#player.balance),
   
   HandValues = blackjack_player:get_hand_values(UpdatedDealer#player.cards,[]),
   
   if HandValue > 16 -> UpdatedDealer;
      HandValue < 21, HandValue >= Player#player.handValue -> UpdatedDealer;
	  true -> dealer_twist(tl(Cards),UpdatedDealer,  Player)
end.

player_stand(Player) -> 
if Player#player.split_cards == [] ->
HandValues = blackjack_player:get_hand_values(Player#player.cards,[]),
Player#player{handValue=HandValue};
  
true -> 
DeckValues = blackjack_player:get_hand_values(Player#player.cards,[]),
SplitValues = blackjack_player:get_hand_values(Player#player.split_cards,[]),
Max = blackjack_player:get_max_value([DeckValue,SplitValue],0),
Player#player{handValue=Max}
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




