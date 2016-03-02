-module(blackjack_gen_event)
-behaviour(gen_event)
%% also need a module to abstract this behind....like our other gen_server.
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
         terminate/2]).
		 
init([]) -> {ok, blackjack_table:start()}.

%%%%% now we really need to handle calls, and handle events.  events we can send to everyone, ie the card actions...etc.  it

%%%%% ie we add players to game.

%%%%% subscribe them.

%%%%% start game on timer (ie count down 30 seconds from player added, if no new player then start)


handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.		 