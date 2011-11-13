-module(pad_read).

-export([open/2, read/2, close/1]).

% returns {ok, PID} 
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

% try to read Len bytes from the file. If the expected size is smaller that what was requested,
% pad with zeroes.
% returns { ok, Data }, or "eof".
read(PID, Len) ->
	PID ! { self(), read, Len },
	receive { PID, Reply } -> Reply end.


% read bytes from a file with Expect remaining bytes
reader(FD,Expect) -> receive
	close ->
		ok = file:close(FD);
	{PID, read, Len} ->
		case file:read(FD, Len) of
		{ ok, Data } -> 
			Read = size(Data),
		 	if Read == Len -> % we managed to read what was requested
				PID ! { self(), {ok, Data} },
				reader(FD, Expect - Len);
			Len < Expect -> % the read was not supposed to reach eof. Pad up to Len.
				Pad = Len - Read,
				PID ! { self(), {ok, <<Data/binary,0:Pad/unit:8>>} },
				ok = file:close(FD),
				reader_eof(Expect - Len);
			true -> % the read was supposed to reach eof. Pad up to expected file length.
				Pad = Expect - Read,
				PID ! { self(), {ok, <<Data/binary,0:Pad/unit:8>>} },
				ok = file:close(FD),
				real_eof()
			end;
		eof -> 
			ok = file:close(FD),
			if Len < Expect -> % we expected more than Len, pad up to Len
				PID ! { self(), {ok, <<0:Len/unit:8>>} },
				reader_eof(Expect - Len);
			true -> % expected the file to fall short, only pad up to expected length.
				PID ! { self(), {ok, <<0:Expect/unit:8>>} },
				real_eof()
			end;
		Other -> 
			PID ! { self(), Other }
		end
	end.

% from now on, only return blocks of zeroes as the file was cut short.
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
% expected eof has been reached, from now on only return eof.
real_eof() -> receive
	close -> ok;
	{PID, read, _Len} -> 
		PID ! { self(), eof },
		real_eof()
	end.

