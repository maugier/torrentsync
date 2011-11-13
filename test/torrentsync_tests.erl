-module(torrentsync_tests).
-include_lib("eunit/include/eunit.hrl").


tsync_check_test() ->
	"++++++++" = tsync:check("../test/blob.torrent", "../test/blob").
