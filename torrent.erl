-module(torrent).

-export([
	parse/1,
	multi/1,
	files/1,
	piece_length/1
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

files(MetaInfo) ->
	Info = proplists:get_value(<<"info">>, MetaInfo),
	Files = proplists:get_value(<<"files">>, Info),
	BaseName = proplists:get_value(<<"name">>, Info),
	[ read_file_item(BaseName, Item) || Item <- Files ].

read_file_item(BaseName, Item) ->
	Len = proplists:get_value(<<"length">>, Item),
	PathList = proplists:get_value(<<"path">>, Item),
	Path = string:join([binary_to_list(P) || P <- [BaseName|PathList]] , "/"),
	{ Path, Len }.


