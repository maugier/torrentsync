-module(torrent).

-export([
	parse/1,
	multi/1,
	files/1,
	basename/1,
	piece_length/1,
	piece_data/1
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

piece_data(MetaInfo) ->
	proplists:get_value(<<"pieces">>, MetaInfo).

files(MetaInfo) ->
	Info = proplists:get_value(<<"info">>, MetaInfo),
	Files = proplists:get_value(<<"files">>, Info),
	[ read_file_item(Item) || Item <- Files ].

basename(MetaInfo) ->
	proplists:get_value(<<"name">>, MetaInfo).

read_file_item(Item) ->
	Len = proplists:get_value(<<"length">>, Item),
	PathList = proplists:get_value(<<"path">>, Item),
	Path = string:join([binary_to_list(P) || P <- PathList] , "/"),
	{ Path, Len }.


