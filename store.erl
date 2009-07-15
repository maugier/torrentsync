-module(store).

-export([write/2]).

write(Torrent, { piece, Id, Data }) ->
	Files = torrent:files(Torrent),
	PieceLength = torrent:piece_length(Torrent),
	_Slices = slice_piece(Files, Id * PieceLength, Data).


slice_piece(_,_,<<>>) -> [];
slice_piece([{File,Size}|Tail], Offset, Data) ->
	if Size < Offset ->
		slice_piece(Tail, Offset - Size, Data);
	true ->
		case (Size - Offset) of N when N > size(Data) ->
			[{File, Offset, Data}];
		N ->
			<<Slice:N/binary,Rest/binary>> = Data,
			[{File, Offset, Slice} |
			 slice_piece(Tail, 0, Rest) ]
		end
	end.
