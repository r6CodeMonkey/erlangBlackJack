-module(blackjack_table).
-compile(export_all).

%% contains deck
start() ->
Decks = blackjack_deck:create_decks(lists:seq(1,4), []),
{blackjack_deck:shuffle(lists:seq(1,1000), Decks),[],[]}.




