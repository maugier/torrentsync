-module(loader).

-export([start/3, load/3, pieces/1, pieces/2]).

pieces(Torrent) -> pieces(Torrent, default).
pieces(Torrent, Base) ->
	PID = self(),
	spawn_link(fun () -> load(Torrent,Base,PID) end),
	gather().

gather() -> receive
	load_done -> [];
	Piece = {piece,_,_} -> [Piece | gather()]
end.

start(Torrent,Base,Dest) ->
	spawn_link(fun () -> load(Torrent,Base,Dest) end).

load(Torrent,Base,Dest) ->
	PieceLength = torrent:piece_length(Torrent),
	FileList = torrent:files(Torrent,Base),
	load_pieces(Dest, FileList, PieceLength),
	Dest ! load_done.


%%%%%%%%%%%%%%%%%
% Load the pieces data from a torrent, given a list of files and piece size
load_pieces(Dest,FileList,PSize) -> load_pieces(Dest,FileList,PSize,0,<<>>).

%
% load_pieces(Dest,Files,PS,Cnt,Last)
% Dest = process of recipient
% Files = list of {Filename, Size}
% PS = piece size
% Cnt = piece index
% Last = fragment of data read from last file


% no more files, create last undersized tail piece
load_pieces(Dest,[],_,Cnt,Last) ->
	case size(Last) of 
		0 -> ok; 
		_ -> Dest ! {piece,Cnt,Last}, ok %send 
	end;

% read from a new file
load_pieces(Dest,[{File,Len}|FT],PS,Cnt,Last) -> 
	{ok, FD} = pad_read:open(File,Len),
	{ok, NewCnt, NewLast} = load_pieces_file(Dest,FD,PS,Cnt,Last),
	ok = pad_read:close(FD),
	load_pieces(Dest,FT,PS,NewCnt,NewLast).

% read piece data from a file, return {ok, NewCnt, NewLast}
% NewCnt = new piece index
% NewLast = new incomplete piece at end of file
load_pieces_file(Dest,FD,PS,Cnt,Last) ->
	
	%how much we need for a complete piece
	ToRead = PS - size(Last),

	{ok, Data} = pad_read:read(FD,ToRead),
	if size(Data) < ToRead ->
		% file was too short to make a complete file
		{ok, Cnt, <<Last/binary,Data/binary>>};
	true -> 
		Dest ! {piece, Cnt, <<Last/binary,Data/binary>>},
		load_pieces_file(Dest,FD,PS,Cnt+1,<<>>)
	end.
	
	


