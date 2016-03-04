-module(blackjack_player).
-compile(export_all).

-include("blackjack_records.hrl").

create_player(Pid, Balance) -> #player{id=Pid, balance=Balance}.

update_player(Player, Cards, SplitCards, Wager) ->
Player#player{cards=Cards, split_cards=SplitCards, balance=Wager}.


update_cards([], Cards) -> Cards;
update_cards([NewCard|Rest], Cards) ->
 update_cards(Rest, [NewCard|Cards]).
	
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
 
 


