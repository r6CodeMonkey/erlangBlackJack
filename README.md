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
