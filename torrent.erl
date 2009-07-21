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

parse(FileName) ->
	{ok, TData} = file:read_file(FileName),
	{MetaInfo, <<>>} = benc:decode(TData),
	MetaInfo.

multi(MetaInfo) ->
	Info = proplists:get_value(<<"info">>, MetaInfo),
	case proplists:is_defined(<<"length">>, Info) of
		true -> single;
		_    -> multi
	end.

piece_length(MetaInfo) ->
	I = proplists:get_value(<<"info">>,MetaInfo),
	proplists:get_value(<<"piece length">>, I).

pieces(MetaInfo) ->
	I = proplists:get_value(<<"info">>,MetaInfo),
	Data = proplists:get_value(<<"pieces">>, I),
	split_pieces(Data).

split_pieces(<<>>) ->
        [];
split_pieces(<<Piece:20/binary, Rest/binary>>) ->
        [ Piece | split_pieces(Rest) ].

files(MetaInfo) -> files(MetaInfo, default).

files(MetaInfo, default) -> 
	Info = proplists:get_value(<<"info">>, MetaInfo),
	Base = proplists:get_value(<<"name">>, Info),
	files(MetaInfo, Base);
files(MetaInfo, Base) when is_list(Base) ->
	files(MetaInfo, list_to_binary(Base));
files(MetaInfo, Base) ->
	Info = proplists:get_value(<<"info">>, MetaInfo),
	Files = proplists:get_value(<<"files">>, Info),
	[ read_file_item(Item,Base) || Item <- Files ].

basename(MetaInfo) ->
	I = proplists:get_value(<<"info">>,MetaInfo),
	proplists:get_value(<<"name">>, I).

read_file_item(Item,Base) ->
	Len = proplists:get_value(<<"length">>, Item),
	PathList = proplists:get_value(<<"path">>, Item),
	Path = string:join([binary_to_list(P) || P <- [Base|PathList]] , "/"),
	{ Path, Len }.


