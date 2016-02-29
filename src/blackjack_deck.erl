-module(blackjack_deck).
-compile(export_all).

-record(card, {suit, value}).

create([], Acc) -> lists:reverse(Acc);
create([Suit|Tail], Acc) ->
 create(Tail, create_suit(lists:seq(2,10),Suit, Acc)).

create_suit([],Suit, Acc) -> 
%% add in our face cards and ace.
   lists:reverse([#card{suit=Suit, value="Jack"},#card{suit=Suit, value="Queen"},#card{suit=Suit, value="King"},#card{suit=Suit, value="Ace"}|Acc]);
create_suit([Value|Rest], Suit, Acc) ->
 create_suit(Rest, Suit, [#card{suit=Suit, value=Value}|Acc]).

shuffle([],Deck) -> lists:reverse(Deck);
shuffle([Shuffle|Tail], Deck) ->
%% need to randomly shuffle cards..we dont want to split at 52 clearly. hence take 1 off.
{First, Second} = lists:split(random:uniform(length(Deck)-1), Deck),
{Third, Fourth} = lists:split(random:uniform(length(First)), First),
{Fifth, Sixth} = lists:split(random:uniform(length(Second)), Second),

shuffle(Tail, lists:append(lists:append(Sixth, Third),lists:append(Fifth, Fourth))).