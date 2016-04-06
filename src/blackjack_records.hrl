-record(player, {id, balance, cards=[], split_cards=[], handValue=0}).

-record(card, {suit, value, numericValue}).

-record(handValue, {value}).