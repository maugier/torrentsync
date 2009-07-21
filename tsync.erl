-module(tsync).

-export([check/1, check/2, sync/3]).

check(TorrentFile) -> check(TorrentFile, default).


check(TorrentFile, Base) ->
	T = torrent:parse(TorrentFile),
	PID = self(),
	CK = checker:start(T, {
		fun (Id) -> PID ! { good, Id } end,
		fun (Id) -> PID ! { missing, Id } end
	}),
	loader:load(T,Base,CK),
	PID ! done,
	display_good([]).

display_good(Acc) -> receive
	{ good, _ } -> display_good([$+|Acc]);
	{ missing, _ } -> display_good([$-|Acc]);
	done -> lists:reverse(Acc)
end.


sync(Torrent,From,From) -> {error, same_source_and_dest}.
sync(Torrent,From,To) ->
	T = torrent:parse(Torrent),
	Store = store:start(Torrent,To),
