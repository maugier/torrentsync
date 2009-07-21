-module(loader).

-export([check/3, display/0]).


display() -> { fun(_Id) -> io:format("+") end,
               fun(_Id) -> io:format(".") end }.

check(Torrent,Base,Dest) -> 
	check(Torrent, Base, Dest, torrent:multi(Torrent)).

check(_Torrent,_B,_D,single) -> {error, not_impl, check_single};
check(Torrent,Base,Dest,multi) -> 
	PieceData = torrent:piece_data(Torrent),
	PieceLength = torrent:piece_length(Torrent),
	FileList = torrent:files(Torrent,Base),
	
	Checker = checker:start(PieceData, Dest),

	load_pieces(Checker, FileList, PieceLength).

%%%%%%%%%%%%%%%%%
% Load the pieces data from a torrent
load_pieces(Dest,FileList,PSize) -> load_pieces(Dest,FileList,PSize,0,<<>>).

load_pieces(Dest,[],_,Cnt,Last) ->
	case size(Last) of 0 -> ok; _ -> Dest ! {piece,Cnt,Last}, ok end;

load_pieces(Dest,[{File,Len}|FT],PS,Cnt,Last) -> 
	{ok, FD} = pad_read:open(File,Len),
	{ok, NewCnt, NewLast} = load_pieces_file(Dest,FD,PS,Cnt,Last),
	ok = pad_read:close(FD),
	load_pieces(Dest,FT,PS,NewCnt,NewLast).

load_pieces_file(Dest,FD,PS,Cnt,Last) ->
	ToRead = PS - size(Last),
	{ok, Data} = pad_read:read(FD,ToRead),
	if size(Data) < ToRead ->
		{ok, Cnt, <<Last/binary,Data/binary>>};
	true -> 
		Dest ! {piece, Cnt, <<Last/binary,Data/binary>>},
		load_pieces_file(Dest,FD,PS,Cnt+1,<<>>)
	end.
	
	


