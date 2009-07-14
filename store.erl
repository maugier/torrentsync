-module(store).

-export([write/2]).

write(Info, { piece, Id, Data }) ->
	FileData = proplists:get_value(<<"files">>, Info),
	PieceSize = proplists:get_value(<<"piece length">>, Info),
	_Slices = slice_piece(FileData, Id * PieceSize, Data).


slice_piece(_,_,<<>>) -> [];
slice_piece([File|Tail], Offset, Data) ->
	Size = proplists:get_value(<<"length">>, File),
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
