-module(tsync).

-export([check/1, check/2, sync/3]).

check(TorrentFile) -> check(TorrentFile, default).


check(TorrentFile, Base) ->
	T = torrent:parse(TorrentFile),
	PID = self(),
	CK = checker:start(T, fun (_,_,Stat) -> PID ! {check, Stat} end),
	loader:load(T,Base,CK),
	display_good([]).

display_good(Acc) -> receive
	{ check, good} -> display_good([$+|Acc]);
	{ check, missing } -> display_good([$-|Acc]);
	{ check, done } -> lists:reverse(Acc)
end.

remote_check_hook(PID) ->
	fun (Id,_,Stat) -> PID ! { piece_status, Id, Stat } end.

sync(_,From,From) -> {error, same_source_and_dest};
sync(Torrent,{Node,From},To) -> rpc:call(Node,?MODULE,sync,[Torrent,From,To]);
sync(Torrent,From,{Node,To}) -> 
	T = torrent:parse(Torrent),
	{ok, Store} = run_maybe_local(Node,{store,start,[T,To]}),
	Diff = differ:start(Store),
	RemoteChecker = run_maybe_local(Node,{checker, start, [
		T, remote_check_hook(Diff)]}),
	_RemoteLoader = run_maybe_local(Node,{loader,start,[
		T,To,RemoteChecker]}),
	LocalChecker = checker:start(T,fun
		(Id,Data,good) -> Diff ! { piece, Id, Data };
		(_,_,missing) -> ok;
		(_,_,done)    -> Diff ! load_done
	end),
	loader:load(T,From,LocalChecker);
	
sync(Torrent,From,To) -> sync(Torrent,From,{local,To}).

run_maybe_local(local,{Mod,Fun,Args}) -> apply(Mod,Fun,Args);
run_maybe_local(Node,{Mod,Fun,Args}) -> rpc:call(Node,Mod,Fun,Args).

