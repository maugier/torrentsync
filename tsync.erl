-module(tsync).

-export([check/1]).

-export([load_pieces/3]).

check(TorrentName) -> 
	crypto:start(),
	{ok, TData} = file:read_file(TorrentName),
	{MetaInfo, <<>>} = benc:decode(TData),
	Info = proplists:get_value(<<"info">>, MetaInfo),

	case proplists:is_defined(<<"length">>, Info) of
		true -> check_single(Info);
		_    -> check_multi(Info) end.


check_single(_Info) -> {error, not_impl, check_single}.
check_multi(Info) -> 
	FileData = proplists:get_value(<<"files">>, Info),
	PieceData = proplists:get_value(<<"pieces">>, Info),
	PieceLength = proplists:get_value(<<"piece length">>, Info),
	FileList = [ read_file_item(I) || I <- FileData ],
	
	Good = fun (Id) -> io:format("good piece ~p~n", [Id]) end,
	Bad = fun (Id) -> io:format("bad piece ~p~n", [Id]) end,

	Checker = checker:start(PieceData, {Good,Bad}),

	load_pieces(Checker, FileList, PieceLength).

%%%%%%%%%%%%%%%%%
% Read a file item 

read_file_item(Item) ->
	Len = proplists:get_value(<<"length">>, Item),
	PathList = proplists:get_value(<<"path">>, Item),
	Path = string:join([binary_to_list(P) || P <- PathList] , "/"),
	{ Path, Len }.

%%%%%%%%%%%%%%%%%
% Load the pieces data from a torrent
load_pieces(Dest,FileList,PSize) -> load_pieces(Dest,FileList,PSize,0,<<>>).

load_pieces(Dest,[],_,Cnt,Last) ->
	Dest ! {piece,Cnt,Last},
	ok;

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
	
	


