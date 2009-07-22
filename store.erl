-module(store).

-export([start/1, start/2, write/2]).

-behaviour(gen_server).
-record(st, {piece_length, files, writer, locbase}).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,code_change/3]).

start(Torrent) -> start(Torrent, undefined).
start(Torrent,Loc) -> gen_server:start_link(store,[Torrent,Loc],[]).

write(PID, Req = {piece,_,_}) ->
	gen_server:cast(PID,Req).

init([Torrent,undefined]) ->
	Loc = binary_to_list(torrent:basename(Torrent)),
	init([Torrent,Loc]);

init([Torrent,Loc]) -> 
	Files = torrent:files(Torrent,Loc),
	PieceLength = torrent:piece_length(Torrent),
	PID = spawn_link(fun () -> writer(Loc) end),
	{ok, #st{piece_length=PieceLength,
		 files=Files,
		 writer=PID,
		 locbase=Loc}}.

handle_call(done,_From,ST) ->
	ST#st.writer ! {self(), done},
	receive writer_terminating ->
		{stop, normal, ok, []}
	end.

handle_cast({piece, Id, Data}, ST) ->
	Slices = slice_piece(ST#st.files, Id * ST#st.piece_length, Data),
	PID = ST#st.writer,
	[PID ! S || S <- Slices ],
	{noreply, ST}.

handle_info(_,ST) -> {noreply,ST}.

terminate(Reason,State) -> 
	io:format("Store terminating (~p) ~p~n",[Reason,State]).

code_change(_Old,St,_Ex) -> {ok, St}.


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
writer(Loc) -> writer(null,null,Loc).
writer(FileName,FD,Loc) ->
	receive 
	    {write_piece, NewFileName, Offset, Slice} ->
		if 
		    FileName == NewFileName ->
			write_to_fd(FD,Offset,Slice),
			writer(FileName,FD,Loc);
		    true ->
		    	ok = case FD of null -> ok; _ -> file:close(FD) end,
			case file:open(NewFileName,[read,write,binary]) of
			    {ok, NewFD} ->
			    	write_to_fd(NewFD,Offset,Slice),
				writer(NewFileName, NewFD,Loc);
			    Err -> 
				error_logger:error_report
					([opening_fd, {err,Err},{path,NewFileName}]),
				writer(null,null,Loc)
			end
	        end;
	    {PID, done} ->
	    	PID ! writer_terminating,
	        ok
end.

write_to_fd(FD, Offset, Slice) ->
	io:format("Writing ~p bytes at ~p~n",[byte_size(Slice),Offset]),
	ok = file:pwrite(FD,[{{bof,Offset},Slice}]).
