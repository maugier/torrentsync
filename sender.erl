-module(sender).

-export([start/1]).

start(Dest) -> loop(Dest).

loop(Dest) -> receive
	{ need, complete } -> 
		Dest ! complete;
	{ need, Id } -> receive
		{ piece_good, Id, Piece } ->
			Dest ! { piece, Id, Piece },
			loop(Dest);
		{ piece_bad, Id } ->
			loop(Dest)
end.

