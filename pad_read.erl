-module(pad_read).

-export([open/2, read/2, close/1]).


open(Filename,Expect) ->
	case file:open(Filename, [binary,read]) of
		{ok, FD} -> 
			{ok, spawn_link(fun () -> reader(FD,Expect) end)};
		{error, enoent} ->
			{ok, spawn_link(fun () -> reader_eof(Expect) end)};
		Error -> Error
	end.

close(PID) ->
	PID ! close,
	ok.

read(PID, Len) ->
	PID ! { self(), read, Len },
	receive { PID, Reply } -> Reply end.

reader(FD,Expect) -> receive
	close ->
		ok = file:close(FD);
	{PID, read, Len} ->
		case file:read(FD, Len) of
		{ ok, Data } -> 
			Read = size(Data),
		 	if Read == Len ->
				PID ! { self(), {ok, Data} },
				reader(FD, Expect - Len);
			Len < Expect ->
				Pad = 8*(Len - Read),
				PID ! { self(), {ok, <<Data/binary,0:Pad>>} },
				ok = file:close(FD),
				reader_eof(Expect - Len);
			true -> 
				Pad = 8*(Expect - Read),
				PID ! { self(), {ok, <<Data/binary,0:Pad>>} },
				ok = file:close(FD),
				real_eof()
			end;
		eof -> 
			ok = file:close(FD),
			if Len < Expect ->
				PID ! { self(), {ok, <<0:Len/unit:8>>} },
				reader_eof(Expect - Len);
			true ->
				PID ! { self(), {ok, <<0:Expect/unit:8>>} },
				real_eof()
			end;
		Other -> 
			PID ! { self(), Other }
		end
	end.

reader_eof(Expect) -> receive
	close -> ok;
	{PID, read, Len} ->
		if Len >= Expect ->
			PID ! { self(), {ok, <<0:Expect/unit:8>>} },
			real_eof();
		true ->
			PID ! { self(), {ok, <<0:Len/unit:8>>} },
			reader_eof(Expect - Len)
		end
	end.

real_eof() -> receive
	close -> ok;
	{PID, read, _Len} -> 
		PID ! { self(), eof },
		real_eof()
	end.

