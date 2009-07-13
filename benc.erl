-module(benc).
-export([decode/1]).

%-spec decode(S::string() -> {any(), string}).

decode([$i,$-|Tail]) -> {Int,T2} = decode_int(Tail,0), {-Int,T2};
decode([$i|Tail])   -> decode_int(Tail,0);
decode([$l|Tail])   -> decode_list(Tail,[]);
decode([$d|Tail])   -> decode_dict(Tail,[]);
decode([L|_] = Tail) when L >= $0 andalso L =< $9 -> decode_string_len(Tail,0).


decode_int([$e|Tail], Acc) -> {Acc,Tail};
decode_int([N |Tail], Acc) when N >= $0 andalso N =< $9 ->
	decode_int(Tail, Acc*10 + (N - $0)).

decode_list([$e|Tail], Acc) -> {lists:reverse(Acc), Tail};
decode_list(Tail, Acc) -> 
	{Next, T2} = decode(Tail),
	decode_list(T2, [Next|Acc]).

decode_dict([$e|Tail], Acc) -> {Acc,Tail};
decode_dict(Tail,Acc) ->
	{Key, T2} = decode(Tail),
	{Val, T3} = decode(T2),
	decode_dict(T3, [{Key,Val}|Acc]).

decode_string_len([$:|Tail], Acc) ->
        lists:split(Acc,Tail);
decode_string_len([N | Tail], Acc) when N >= $0 andalso N =< $9 ->
	decode_string_len(Tail, Acc*10 + (N - $0)).
