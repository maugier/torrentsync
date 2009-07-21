-module(differ).

-export([start/1]).


start(Store) -> spawn_link(fun () -> loop(Store) end).

loop(Store) -> receive
	Piece = { piece,_,_} ->
		process_piece(Piece,Store),
		loop(Store);	
	load_done -> 
		gen_server:call(Store, done),
		ok
end.

process_piece(Piece = {piece,Id,_},Store) -> receive
	{ piece_status, Id, good } ->
		loop(Store);
	{ piece_status, Id, missing } ->
		gen_server:cast(Store, Piece),
		loop(Store);
	{ piece_status, OtherId, _ } when OtherId < Id ->
		process_piece(Piece,Store);
	{ piece_status, _, _} ->
		throw(unordered_piece_id)
end.
