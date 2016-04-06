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

get_hand_value([], Acc) -> lists:reverse(Acc); 
get_hand_value([Card|Hand], Acc) -> 
{Value,AltValue} = Card#card.numericValue,
 if Value /= AltValue ->
 %% if our acc is empty, then need to add a value to it.
  HandValues = lists:append(add_hand_value(Acc, Value, []),add_hand_value(Acc, AltValue, []));
 true -> HandValues = add_hand_value(Acc, Value, [])
  end,
get_hand_value(Hand, HandValues).

add_hand_value([],_, Acc) -> lists:reverse(Acc);
add_hand_value([Hand|Alt], Value, Acc) ->
V = Hand#handValue.value + Value,
add_hand_value(Alt, Value, lists:append(Acc, [#handValue{value=V}])).

%% we do need it on split.
get_max_value([], Max) -> Max;
get_max_value(Values, Max) ->
 TempMax = lists:max(Values),
 if TempMax =< 21 -> TempMax;
 true -> get_max_value(lists:delete(TempMax, Values),TempMax)
 end.
 
 


