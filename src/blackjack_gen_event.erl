-module(blackjack_gen_event)
-behaviour(gen_event)

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
         terminate/2]).
		 
init([]) -> {ok, blackjack_table:start()}.

handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.		 