Notes for me (as i did this at work lol)

Need to remove the update_card / update_cards.  have 1 recursive routine that takes a list of current, and new cards.

Secondly, due to 2 aces, need to remove the alternate value shit.

Instead, one routine.  that routine will simply keep a list of scores (and remove duplicates)
and simply work with the max assuming its <= 21.  


To play the game simply

{ok,Pid} = blackjack_gen_server:join_table().

blackjack_gen_server:enter_game(Pid, 100).

To Hit:

blackjack_gen_server:hit(Pid).

To Surrender:
blackjack_gen_server:surrender(Pid).

To Stand:
blackjack_gen_server:stand(Pid).

To leave:
blackjack_gen_server:leave(Pid).
