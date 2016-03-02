-module(blackjack_player).
-compile(export_all).

-include("blackjack_records.hrl").

create_player(Pid, Balance) -> #player{id=Pid, balance=Balance}.

update_cards(Player, Card, Card2) ->
Player#player{cards=[Card,Card2|Player#player.cards]}.

update_card(Player, Card, Wager) ->
Player#player{balance=Wager
	,cards=[Card|Player#player.cards]}.

update_split_card(Player, Card, Wager) ->
Player#player{balance=Wager
   ,split_cards=[Card|Player#player.split_cards]}.	
	
split_cards(Player) ->
[Card, Card2|Rest] = Player#player.cards,
Player#player{cards=[Card], split_cards=[Card2]}.

get_alternate_hand_value([], Value) -> Value;
get_alternate_hand_value([Card|Hand], Value) -> 
 get_alternate_hand_value(Hand, Value+blackjack_deck:get_alternate_card_value(Card#card.value)).


get_hand_value([], Value) -> Value; 
get_hand_value([Card|Hand], Value) -> 
get_hand_value(Hand, Value + blackjack_deck:get_card_value(Card#card.value)).


get_max_value([], Max) -> Max;
get_max_value(Values, Max) ->
 TempMax = lists:max(Values),
 if TempMax =< 21 -> TempMax;
 true -> get_max_value(lists:delete(TempMax, Values),TempMax)
 end.
 
 


