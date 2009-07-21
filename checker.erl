-module(checker).

-export([start/2, display/0]).

display() -> {
	fun (ID) -> io:format("Piece ~p was OK~n",[ID]) end,
	fun (ID) -> io:format("Piece ~p was BAD~n",[ID]) end
	}.

start(Torrent,{Good,Bad}) -> 
	Pieces = torrent:pieces(Torrent),
	spawn_link(fun () -> loop(Pieces,Good,Bad,0) end).


loop([Hash|Tail], Good, Bad, Id) ->
	receive { piece, Id, Data } ->
		case crypto:sha(Data) == Hash of
			true -> Good(Id);
			false -> Bad(Id)
		end,
		loop(Tail,Good,Bad,Id+1);
	_ ->
		loop([Hash|Tail], Good, Bad, Id)
	end;

loop([],_,_,_) -> ok.

