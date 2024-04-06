%%% Copyright 2023 Oleksandr Chumachenko <ledest@gmail.com>
%%%
%%% This file is part of Emsgpack.
%%%
%%% Emsgpack is free software: you can redistribute it and/or modify it
%%% under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% Emsgpack is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
%%% See the GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with Emsgpack. If not, see <http://www.gnu.org/licenses/>.
-module(emsgpack).

-export([encode/1, encode/2, decode/1, decode/2]).
-export([enc/1, enc/2, dec/1, dec/2]).

-define(NIL, 16#C0).
-define(FALSE, 16#C2).
-define(TRUE, 16#C3).
-define(BIN1, 16#C4).
-define(BIN2, 16#C5).
-define(BIN4, 16#C6).
-define(EXT1, 16#C7).
-define(EXT2, 16#C8).
-define(EXT4, 16#C9).
-define(FLOAT4, 16#CA).
-define(FLOAT8, 16#CB).
-define(UINT0, 0:1).
-define(UINT1, 16#CC).
-define(UINT2, 16#CD).
-define(UINT4, 16#CE).
-define(UINT8, 16#CF).
-define(SINT1, 16#D0).
-define(SINT2, 16#D1).
-define(SINT4, 16#D2).
-define(SINT8, 16#D3).
-define(FIXEXT1, 16#D4).
-define(FIXEXT2, 16#D5).
-define(FIXEXT4, 16#D6).
-define(FIXEXT8, 16#D7).
-define(FIXEXT16, 16#D8).
-define(STR0, 2#101:3).
-define(STR1, 16#D9).
-define(STR2, 16#DA).
-define(STR4, 16#DB).
-define(ARRAY2, 16#DC).
-define(ARRAY4, 16#DD).
-define(MAP2, 16#DE).
-define(MAP4, 16#DF).
-define(MAP0, 16#8:4).
-define(ARRAY0, 16#9:4).
-define(TIMESTAMP, 16#FF).
-define(TERM, 16#83).
-define(BITSTR, 16#4D).

-define(ARRAY0(S), ?ARRAY0, S:4).
-define(ARRAY02(S), ?ARRAY2, 0, S:1/unit:8).
-define(ARRAY04(S), ?ARRAY4, 0:2/unit:8, S:2/unit:8).
-define(ARRAY2(S), ?ARRAY2, S:2/unit:8).
-define(ARRAY4(S), ?ARRAY4, S:4/unit:8).
-define(BIN02(S), ?BIN2, 0, S:1/unit:8).
-define(BIN04(S), ?BIN4, 0:2/unit:8, S:2/unit:8).
-define(BIN1(S), ?BIN1, S:1/unit:8).
-define(BIN2(S), ?BIN2, S:2/unit:8).
-define(BIN4(S), ?BIN4, S:4/unit:8).
-define(BIN02(S, B), ?BIN02(S), B:S/binary).
-define(BIN04(S, B), ?BIN04(S), B:S/binary).
-define(BIN1(S, B), ?BIN1(S), B:S/binary).
-define(BIN2(S, B), ?BIN2(S), B:S/binary).
-define(BIN4(S, B), ?BIN4(S), B:S/binary).
-define(EXT1(S), ?EXT1, S:1/unit:8).
-define(EXT2(S), ?EXT2, S:2/unit:8).
-define(EXT4(S), ?EXT4, S:4/unit:8).
-define(FLOAT4(F), ?FLOAT4, F:32/float).
-define(FLOAT8(F), ?FLOAT8, F/float).
-define(MAP0(S), ?MAP0, S:4).
-define(MAP2(S), ?MAP2, S:2/unit:8).
-define(MAP4(S), ?MAP4, S:4/unit:8).
-define(SINT1(I), ?SINT1, I:1/signed-unit:8).
-define(SINT2(I), ?SINT2, I:2/signed-unit:8).
-define(SINT4(I), ?SINT4, I:4/signed-unit:8).
-define(SINT8(I), ?SINT8, I:8/signed-unit:8).
-define(FIXEXT4(T, I), ?FIXEXT4, T, I:4/unit:8).
-define(FIXEXT8(T), ?FIXEXT8, T).
-define(FIXEXT8(T, I), ?FIXEXT8(T), I:8/unit:8).
-define(STR0(S), ?STR0, S:5).
-define(STR01(S), ?STR1, 0:3, S:5).
-define(STR02(S), ?STR2, 0, S:1/unit:8).
-define(STR04(S), ?STR4, 0:2/unit:8, S:2/unit:8).
-define(STR1(S), ?STR1, S:1/unit:8).
-define(STR2(S), ?STR2, S:2/unit:8).
-define(STR4(S), ?STR4, S:4/unit:8).
-define(STR0(S, B), ?STR0(S), B:S/binary).
-define(STR01(S, B), ?STR01(S), B:S/binary).
-define(STR02(S, B), ?STR02(S), B:S/binary).
-define(STR04(S, B), ?STR04(S), B:S/binary).
-define(STR1(S, B), ?STR1(S), B:S/binary).
-define(STR2(S, B), ?STR2(S), B:S/binary).
-define(STR4(S, B), ?STR4(S), B:S/binary).
-define(UINT0(I), ?UINT0, I:7).
-define(UINT1(I), ?UINT1, I:1/unit:8).
-define(UINT2(I), ?UINT2, I:2/unit:8).
-define(UINT4(I), ?UINT4, I:4/unit:8).
-define(UINT8(I), ?UINT8, I:8/unit:8).
-define(TIMESTAMP4(S), ?FIXEXT4(?TIMESTAMP, S)).
-define(TIMESTAMP8(S, N), ?FIXEXT8(?TIMESTAMP), N:30, S:34).
-define(TIMESTAMP12(S, N), ?EXT1(12), ?TIMESTAMP, N:4/unit:8, S:8/unit:8).

-record(opt, {safe = false :: boolean(), compat = false :: boolean(), bitstr :: undefined|ext|binary|term}).

-spec encode(T::term()) -> binary().
encode(T) -> iolist_to_binary(enc(T)).

-spec encode(T::term(), O::proplists:proplist()) -> binary().
encode(T, O) -> iolist_to_binary(enc(T, O)).

-spec decode(B::binary()) -> term().
decode(B) ->
    {T, _} = dec(B),
    T.

-spec decode(B::binary(), O::proplists:proplist()) -> term().
decode(B, O) ->
    {T, _} = dec(B, O),
    T.

-spec enc(T::term()) -> iodata().
enc(T) -> enc_(T, #opt{}).

-spec enc(T::term(), O::proplists:proplist()) -> iodata().
enc(T, O) ->
    is_list(O) orelse error(badarg, [T, O]),
    enc_(T, options(O)).

-spec dec(B::binary()) -> {term(), binary()}.
dec(B) -> dec_(B, #opt{}).

-spec dec(B::binary(), O::proplists:proplist()) -> {term(), binary()}.
dec(T, O) ->
    is_list(O) orelse error(badarg, [T, O]),
    dec_(T, options(O)).

-spec enc_(T::term(), O::#opt{}) -> iodata().
enc_([], #opt{compat = false}) -> <<?NIL>>;
enc_(null, #opt{compat = true}) -> <<?NIL>>;
enc_(false, _) -> <<?FALSE>>;
enc_(true, _) -> <<?TRUE>>;
enc_(A, O) when is_atom(A) -> enc_atom(A, O);
enc_(I, _) when is_integer(I) -> enc_int(I);
enc_(F, _) when is_float(F) -> enc_float(F);
enc_(B, _) when is_binary(B) -> enc_binary(B);
enc_(T, O) when is_tuple(T) -> enc_tuple(T, O);
enc_(L, O) when is_list(L) -> enc_list(L, O);
enc_(M, O) when is_map(M) -> enc_map(M, O);
enc_(B, O) when is_bitstring(B) -> enc_bitstr(B, O);
enc_(T, _) -> enc_term(T).

-spec dec_(B::binary(), O::#opt{}) -> {term(), binary()}.
dec_(<<?UINT0(I), R/binary>>, _) -> {I, R};
dec_(<<I:1/signed-unit:8, R/binary>>, _) when I >= -32 -> {I, R};
dec_(<<?MAP0(S), B/binary>>, O) -> dec_map(S, O, B);
dec_(<<?ARRAY0(S), B/binary>>, O) -> dec_tuple(S, O, B);
dec_(<<?NIL, R/binary>>, #opt{compat = true}) -> {null, R};
dec_(<<?NIL, R/binary>>, _) -> {[], R};
dec_(<<?FALSE, R/binary>>, _) -> {false, R};
dec_(<<?TRUE, R/binary>>, _) -> {true, R};
dec_(<<?BIN1(S, B), R/binary>>, _) -> {B, R};
dec_(<<?BIN02(S, B), R/binary>>, O) -> {dec_term(B, O), R};
dec_(<<?BIN2(S, B), R/binary>>, _) -> {B, R};
dec_(<<?BIN04(S, B), R/binary>>, O) -> {dec_term(B, O), R};
dec_(<<?BIN4(S, B), R/binary>>, _) -> {B, R};
dec_(<<?TIMESTAMP12(S, N), R/binary>>, _) -> {dec_timestamp(S, N), R};
dec_(<<?FLOAT4(F), R/binary>>, _) -> {F, R};
dec_(<<?FLOAT8(F), R/binary>>, _) -> {F, R};
dec_(<<?UINT1(I), R/binary>>, _) -> {I, R};
dec_(<<?UINT2(I), R/binary>>, _) -> {I, R};
dec_(<<?UINT4(I), R/binary>>, _) -> {I, R};
dec_(<<?UINT8(I), R/binary>>, _) -> {I, R};
dec_(<<?SINT1(I), R/binary>>, _) -> {I, R};
dec_(<<?SINT2(I), R/binary>>, _) -> {I, R};
dec_(<<?SINT4(I), R/binary>>, _) -> {I, R};
dec_(<<?SINT8(I), R/binary>>, _) -> {I, R};
dec_(<<?TIMESTAMP4(S), R/binary>>, _) -> {S, R};
dec_(<<?TIMESTAMP8(S, N), R/binary>>, _) -> {dec_timestamp(S, N), R};
dec_(<<?STR0(S, B), R/binary>>, _) -> {unicode:characters_to_list(B), R};
dec_(<<?STR01(S, B), R/binary>>, O) -> {dec_atom(B, O), R};
dec_(<<?STR1(S, B), R/binary>>, _) -> {unicode:characters_to_list(B), R};
dec_(<<?STR02(S, B), R/binary>>, O) -> {dec_atom(B, O), R};
dec_(<<?STR2(S, B), R/binary>>, _) -> {unicode:characters_to_list(B), R};
dec_(<<?STR4(S, B), R/binary>>, _) -> {unicode:characters_to_list(B), R};
dec_(<<?ARRAY02(S), B/binary>>, O) -> dec_list(S, O, B);
dec_(<<?ARRAY2(S), B/binary>>, O) -> dec_tuple(S, O, B);
dec_(<<?ARRAY04(S), B/binary>>, O) -> dec_list(S, O, B);
dec_(<<?ARRAY4(S), B/binary>>, O) -> dec_tuple(S, O, B);
dec_(<<?MAP2(S), B/binary>>, O) -> dec_map(S, O, B);
dec_(<<?MAP4(S), B/binary>>, O) -> dec_map(S, O, B);
dec_(<<?EXT1(S), T, B/binary>>, _) -> dec_ext(S, T, B);
dec_(<<?EXT2(S), T, B/binary>>, _) -> dec_ext(S, T, B);
dec_(<<?EXT4(S), T, B/binary>>, _) -> dec_ext(S, T, B);
dec_(<<?FIXEXT1, _, B:1/binary, R/binary>>, _) -> {B, R};
dec_(<<?FIXEXT2, _, B:2/binary, R/binary>>, _) -> {B, R};
dec_(<<?FIXEXT4, _, B:4/binary, R/binary>>, _) -> {B, R};
dec_(<<?FIXEXT8, _, B:8/binary, R/binary>>, _) -> {B, R};
dec_(<<?FIXEXT16, _, B:16/binary, R/binary>>, _) -> {B, R};
dec_(_, _) -> error(undefined).

-compile({inline, enc_int/1}).
-spec enc_int(I::integer()) -> binary()|[binary()].
enc_int(I) when I >= 0 -> enc_uint(I);
enc_int(I) -> enc_sint(I).

-compile({inline, enc_uint/1}).
-spec enc_uint(I::non_neg_integer()) -> binary()|[binary()].
enc_uint(I) when I < 1 bsl 7 -> <<I>>;
enc_uint(I) when I < 1 bsl 8 -> <<?UINT1(I)>>;
enc_uint(I) when I < 1 bsl 16 -> <<?UINT2(I)>>;
enc_uint(I) when I < 1 bsl 32 -> <<?UINT4(I)>>;
enc_uint(I) when I < 1 bsl 64 -> <<?UINT8(I)>>;
enc_uint(I) -> enc_term(I).

-compile({inline, enc_sint/1}).
-spec enc_sint(I::neg_integer()) -> binary()|[binary()].
enc_sint(I) when I >= -1 bsl 5 -> <<2#111:3, I:5/signed>>;
enc_sint(I) when I >= -1 bsl 7 -> <<?SINT1(I)>>;
enc_sint(I) when I >= -1 bsl 15 -> <<?SINT2(I)>>;
enc_sint(I) when I >= -1 bsl 31 -> <<?SINT4(I)>>;
enc_sint(I) when I >= -1 bsl 63 -> <<?SINT8(I)>>;
enc_sint(I) -> enc_term(I).

-compile({inline, enc_float/1}).
-spec enc_float(F::float()) -> binary().
enc_float(F) ->
    case <<F:32/float>> of
        <<F:32/float>> = F4 -> <<?FLOAT4, F4/binary>>;
        _ -> <<?FLOAT8(F)>>
    end.

-spec enc_binary(B::binary()) -> [binary()].
enc_binary(B) ->
    [case byte_size(B) of
         S when S < 1 bsl 8 -> <<?BIN1(S)>>;
         S when S < 1 bsl 16 -> <<?BIN2(S)>>;
         S when S < 1 bsl 32 -> <<?BIN4(S)>>;
         _ -> error(undefined)
     end,
     B].

-compile({inline, enc_tuple/2}).
-spec enc_tuple(T::tuple(), O::#opt{}) -> iodata().
enc_tuple(T, #opt{compat = true} = O) ->
    S = tuple_size(T),
    [if
         S < 1 bsl 4 -> <<?ARRAY2(S)>>;
         S < 1 bsl 16 -> <<?ARRAY4(S)>>;
         true -> error(undefined)
     end|enc_array(T, O, S)];
enc_tuple(T, O) ->
    S = tuple_size(T),
    [if
         S < 1 bsl 4 -> <<?ARRAY0(S)>>;
         S < 1 bsl 16 -> <<?ARRAY2(S)>>;
         S < 1 bsl 32 -> <<?ARRAY4(S)>>;
         true -> error(undefined)
     end|enc_array(T, O, S)].

-spec enc_array(T::tuple(), O::#opt{}, S::non_neg_integer()) -> iodata().
enc_array(T, O, S) -> enc_array(T, O, S, []).

-spec enc_array(T::tuple(), O::#opt{}, S::non_neg_integer(), L::[iodata()]) -> [iodata()].
enc_array(_, _, 0, L) -> L;
enc_array(T, O, S, L) -> enc_array(T, O, S - 1, [enc_(element(S, T), O)|L]).

-compile({inline, enc_map/2}).
-spec enc_map(M::map(), O::#opt{}) -> [iodata()].
enc_map(M, O) ->
    [case map_size(M) of
         S when S < 1 bsl 4 -> <<?MAP0(S)>>;
         S when S < 1 bsl 16 -> <<?MAP2, S:2/unit:8>>;
         S when S < 1 bsl 32 -> <<?MAP4, S:4/unit:8>>;
         _ -> error(undefined)
     end|maps:fold(fun(K, V, A) -> [enc_(K, O), enc_(V, O)|A] end, [], M)].

-compile({inline, enc_atom/2}).
-spec enc_atom(A::atom(), O::#opt{}) -> [iodata()].
enc_atom(A, O) ->
    B = atom_to_binary(A, utf8),
    S = byte_size(B),
    [case O of
         #opt{compat = true} -> <<?BIN1(S)>>;
         _ when S < 1 bsl 5 -> <<?STR1(S)>>;
         _ -> <<?STR2(S)>>
     end,
     B].

-compile({inline, enc_list/2}).
-spec enc_list(L::list(), O::#opt{}) -> [iodata()].
enc_list(L, O) ->
    case io_lib:char_list(L) of
        true -> enc_string(L, O);
        _false -> enc_list_(L, O)
    end.

-compile({inline, enc_string/2}).
enc_string(L, O) ->
    B = unicode:characters_to_binary(L),
    case byte_size(B) of
        S when S < 1 bsl 5 -> [<<?STR0(S)>>, B];
        S when S < 1 bsl 8 -> [<<?STR1(S)>>, B];
        S when S < 1 bsl 16 -> [<<?STR2(S)>>, B];
        S when S < 1 bsl 32 -> [<<?STR4(S)>>, B];
        _ -> enc_list_(L, O)
    end.

-spec enc_list_(L::list(), O::#opt{}) -> [iodata()].
enc_list_(L, #opt{compat = C} = O) ->
    try lists:mapfoldl(fun(E, A) -> {enc_(E, O), A + 1} end, 0, L) of
        {R, S} when C, S < 1 bsl 4 -> [<<?ARRAY0(S)>>|R];
        {R, S} when C, S < 1 bsl 16; S < 1 bsl 4 -> [<<?ARRAY2(S)>>|R];
        {R, S} when S < 1 bsl 32 -> [<<?ARRAY4(S)>>|R];
        _ -> error(undefined)
    catch
        error:undefined -> error(undefined);
        error:_ -> enc_term(L)
    end.

-compile({inline, enc_bitstr/2}).
-spec enc_bitstr(B::bitstring(), O::#opt{}) -> [binary()].
enc_bitstr(B, #opt{bitstr = term}) -> enc_term(B);
enc_bitstr(B, #opt{bitstr = binary}) -> enc_binary(<<B/bitstring, 0:(8 - bit_size(B) rem 8)>>);
enc_bitstr(B, _) ->
    BS = bit_size(B) rem 8,
    EB = <<B/bitstring, 0:(8 - BS)>>,
    [case byte_size(EB) of
         S when S < 1 bsl 8 -> <<?EXT1(S), (?BITSTR + BS)>>;
         S when S < 1 bsl 16 -> <<?EXT2(S), (?BITSTR + BS)>>;
         S when S < 1 bsl 32 -> <<?EXT4(S), (?BITSTR + BS)>>;
         _ -> error(undefined)
     end|EB].

-spec enc_term(T::term()) -> [iodata()].
enc_term(T) ->
    B = term_to_binary(T),
    [case byte_size(B) of
         S when S < 1 bsl 8 -> <<?BIN2(S)>>;
         S when S < 1 bsl 16 -> <<?BIN4(S)>>;
         _ -> error(undefined)
     end,
     B].

-spec dec_atom(B, O::#opt{}) -> atom()|B when B::binary().
dec_atom(B, #opt{compat = true}) -> B;
dec_atom(B, #opt{safe = true}) ->
    try
        binary_to_existing_atom(B, utf8)
    catch
        _:_ -> B
    end;
dec_atom(B, _) -> binary_to_atom(B, utf8).

-spec dec_tuple(S::non_neg_integer(), O::#opt{}, B::binary()) -> {tuple()|list(), binary()}.
dec_tuple(S, #opt{compat = true} = O, B) -> dec_list(S, O, B);
dec_tuple(S, O, B) ->
    {L, R} = dec_tuple_(S, O, B),
    {list_to_tuple(L), R}.

-spec dec_tuple_(S::non_neg_integer(), O::#opt{}, B::binary()) -> {list(), binary()}.
dec_tuple_(0, _, B) -> {[], B};
dec_tuple_(S, O, B) ->
    {T, R} = dec_(B, O),
    {L, X} = dec_tuple_(S - 1, O, R),
    {[T|L], X}.

-spec dec_map(S::non_neg_integer(), O::#opt{}, B::binary()) -> {map(), binary()}.
dec_map(S, O, B) -> dec_map(S, O, B, #{}).

-spec dec_map(S::non_neg_integer(), O::#opt{}, B::binary(), M::map()) -> {map(), binary()}.
dec_map(0, _, B, M) -> {M, B};
dec_map(S, O, B, M) ->
    {K, R0} = dec_(B, O),
    {V, R} = dec_(R0, O),
    dec_map(S - 1, O, R, M#{K => V}).

-spec dec_list(S::non_neg_integer(), O::#opt{}, B::binary()) -> {list(), binary()}.
dec_list(0, _, B) -> {[], B};
dec_list(S, O, B) ->
    {T, R} = dec_(B, O),
    {L, X} = dec_list(S - 1, O, R),
    {[T|L], X}.

-spec dec_term(B, O::#opt{}) -> term()|B when B::binary().
dec_term(B, #opt{compat = true}) -> B;
dec_term(<<?TERM, _/binary>> = B, #opt{safe = true}) ->
    try
        binary_to_term(B, [safe])
    catch
        error:badarg -> B
    end;
dec_term(<<?TERM, _/binary>> = B, _) -> binary_to_term(B);
dec_term(B, _) -> B.

-spec dec_timestamp(S::integer(), N::integer()) -> integer().
dec_timestamp(S, N) when N < 1000000000 -> S * 1000000000 + N;
dec_timestamp(_, _) -> error(undefined).

-spec dec_ext(S::non_neg_integer(), T::byte(), B::binary()) -> {term(), binary()}.
dec_ext(S, T, B) when T > ?BITSTR, T < ?BITSTR + 8 ->
    BS = (S - 1) * 8 + (T - ?BITSTR),
    {<<Bits:BS/bitstring, _/bits>>, R} = split_binary(B, S),
    {Bits, R};
dec_ext(S, _, B) -> split_binary(B, S).

-spec options(L::proplists:proplist()) -> #opt{}.
options(L) ->
    lists:foldl(fun(compat, A) -> A#opt{compat = true};
                   ({compat, V}, A) when is_boolean(V) -> A#opt{compat = V};
                   (safe, A) -> A#opt{safe = true};
                   ({safe, V}, A) when is_boolean(V) -> A#opt{safe = V};
                   ({bitstr, V}, A) when V =:= binary; V =:= term -> A#opt{bitstr = V};
                   (_, _) -> error(badarg, [L])
                end,
                #opt{}, L).
