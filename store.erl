-module(store).

-export([start/1, write/2]).

-behaviour(gen_server).
-record(st, {piece_length, files, writer}).
-export([init/1, handle_call/3, handle_cast/2]).

init([Torrent]) -> 
	Files = torrent:files(Torrent),
	PieceLength = torrent:piece_length(Torrent),
	PID = spawn_link(fun writer/0),
	{ok, #st{piece_length=PieceLength,
		 files=Files,
		 writer=PID}}.


handle_cast({piece, Id, Data}, ST) ->
	Slices = slice_piece(ST#st.files, ID * ST#st.piece_length, Data),
	PID = ST#st.writer,
	[PID ! S || Slices ],
	{noreply, ST}.


slice_piece(_,_,<<>>) -> [];
slice_piece([{File,Size}|Tail], Offset, Data) ->
	if Size < Offset ->
		slice_piece(Tail, Offset - Size, Data);
	true ->
		case (Size - Offset) of N when N > size(Data) ->
			[{write_piece, File, Offset, Data}];
		N ->
			<<Slice:N/binary,Rest/binary>> = Data,
			[{write_piece, File, Offset, Slice} |
			 slice_piece(Tail, 0, Rest) ]
		end
	end.
writer() -> writer(null,null).
writer(FileName,FD) ->
	receive 
	    {write_piece, NewFileName, Offset, Slice} ->
		if 
		    FileName == NewFileName ->
			write_to_fd(FD,Offset,Slice),
			writer(FileName,FD);
		    true ->
		    	ok = case FD of null -> ok; _ -> file:close(FD) end,
			case file:open(NewFileName,[write,binary]) of
			    {ok, NewFD} ->
			    	write_to_fd(NewFD,Offset,Slice),
				writer(NewFileName, NewFD);
			    Err -> 
				error_logger:error_report
					([opening_fd, {err,Err}]),
				writer(null,null)
			end
	        end;
	    done ->
	        ok
end.

write_to_fd(FD, Offset, Slice) ->
	ok = file:pwrite(FD,[{{bof,Offset},Slice}]).
