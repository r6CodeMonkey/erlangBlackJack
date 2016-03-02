-module(blackjack_player).
-compile(export_all).

-include("blackjack_records.hrl").

create_player(Pid, Balance) -> #player{id=Pid, balance=Balance}.

update_cards(Player, Card, Card2) ->
Player#player{handValue=blackjack_deck:get_card_value(Card#card.value)+blackjack_deck:get_card_value(Card2#card.value)
	, alternateValue=blackjack_deck:get_alternate_card_value(Card#card.value)+blackjack_deck:get_alternate_card_value(Card2#card.value)
	, cards=[Card,Card2|Player#player.cards]}.

update_card(Player, Card, Wager) ->
Player#player{handValue=blackjack_deck:get_card_value(Card#card.value)+Player#player.handValue
	, alternateValue=blackjack_deck:get_alternate_card_value(Card#card.value)+Player#player.alternateValue
	,balance=Wager
	,cards=[Card|Player#player.cards]}.

split_cards(Player) ->
Player#player{cards=[hd(Player#player.cards)], split_cards=tl(Player#player.cards)}.

