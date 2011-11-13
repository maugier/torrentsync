-module(torrent).

-export([
	parse/1,
	multi/1,
	files/1,
	files/2,
	basename/1,
	piece_length/1,
	pieces/1
]).

parse({Node,FileName}) ->
	rpc:call(Node,?MODULE,parse,[FileName]);

parse(FileName) ->
	{ok, TData} = file:read_file(FileName),
	{MetaInfo, <<>>} = benc:decode(TData),
	{torrent, MetaInfo}.

multi({torrent,MetaInfo}) ->
	Info = proplists:get_value(<<"info">>, MetaInfo),
	case proplists:is_defined(<<"length">>, Info) of
		true -> single;
		_    -> multi
	end.

piece_length({torrent,MetaInfo}) ->
	I = proplists:get_value(<<"info">>,MetaInfo),
	proplists:get_value(<<"piece length">>, I).

pieces({torrent,MetaInfo}) ->
	I = proplists:get_value(<<"info">>,MetaInfo),
	Data = proplists:get_value(<<"pieces">>, I),
	split_pieces(Data).

split_pieces(<<>>) ->
        [];
split_pieces(<<Piece:20/binary, Rest/binary>>) ->
        [ Piece | split_pieces(Rest) ].

files(T={torrent,_}) -> files(T, default).

files(T={torrent,MetaInfo}, default) -> 
	Info = proplists:get_value(<<"info">>, MetaInfo),
	Base = proplists:get_value(<<"name">>, Info),
	files(T, Base);
files(T={torrent,_}, Base) when is_list(Base) ->
	files(T, list_to_binary(Base));
files({torrent,MetaInfo}, Base) ->
	Info = proplists:get_value(<<"info">>, MetaInfo),
	Files = proplists:get_value(<<"files">>, Info),
	[ read_file_item(Item,Base) || Item <- Files ].

basename({torrent,MetaInfo}) ->
	I = proplists:get_value(<<"info">>,MetaInfo),
	proplists:get_value(<<"name">>, I).

read_file_item(Item,Base) ->
	Len = proplists:get_value(<<"length">>, Item),
	PathList = proplists:get_value(<<"path">>, Item),
	Path = string:join([binary_to_list(P) || P <- [Base|PathList]] , "/"),
	{ Path, Len }.


