-module(checker).

-export([start/2, display/0]).

display() -> 
	fun (ID,_,good)    -> io:format("Piece ~p was OK~n",[ID]);
	    (ID,_,missing) -> io:format("Piece ~p was BAD~n", [ID]);
	    (_,_,done)     -> io:format("done.") end.

start(Torrent,Hook) -> 
	crypto:start(),
	Pieces = torrent:pieces(Torrent),
	spawn_link(fun () -> loop(Pieces,Hook,0) end).


loop([Hash|Tail], Hook, Id) ->
	receive { piece, Id, Data } ->
		case crypto:sha(Data) == Hash of
			true -> Hook(Id,Data,good);
			false -> Hook(Id,Data,missing)
		end,
		loop(Tail,Hook,Id+1);
	_ ->
		loop([Hash|Tail], Hook, Id)
	end;

loop([],Hook,_) -> Hook([],[],done).

