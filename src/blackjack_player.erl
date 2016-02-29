-module(blackjack_player).
-compile(export_all).

create_player(Pid, Balance) -> {Pid, Balance}.

update_balance({Pid, Balance}, Amount) -> {Pid, Balance + Amount}. 
