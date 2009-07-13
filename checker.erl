-module(checker).

-export([start/2]).

start(PieceData,{Good,Bad}) -> 
	Pieces = split_pieces(PieceData),
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


%%%%%%%%%%%%%%%%
% Split the blob of hashes in a list of valid hashes

split_pieces(<<>>) ->
	[];
split_pieces(<<Piece:20/binary, Rest/binary>>) ->
	[ Piece | split_pieces(Rest) ].
