-module(mudslide_manifest).
-export([get_checksum/1]).
-export([get_file_hashes/1]).
-export([parse_manifest/1]).

-record(file, {filehash, filename}).
-record(checksum, {hash}).
-record(manifest, {cards}).

get_checksum(#manifest{cards=Cards}) ->
    {checksum, Checksum} = proplists:lookup(checksum, Cards),
    Checksum.

get_file_hashes(#manifest{cards=Cards}) ->
    lists:filtermap(fun
            (#file{filehash=Hash}) -> {true, Hash};
            (_) -> false
        end,
        Cards
    ).

parse_manifest(Text) ->
    {Cards, Digested} = lists:foldl(
        fun collect_card/2,
        {[], mudslide_util:get_digest()},
        string:tokens(Text, "\n")
    ),
    ExpectedChecksum = mudslide_util:hex_string(crypto:hash_final(Digested)),
    {checksum, ExpectedChecksum} = proplists:lookup(checksum, Cards),
    #manifest{cards=Cards}.

collect_card("Z " ++ Rest, {Cards, Digest}) ->
    {[#checksum{hash=Rest}| Cards], Digest};
collect_card(("F " ++ Rest) = Line, {Cards, CurrentDigest}) ->
    MoreDigest = crypto:hash_update(CurrentDigest, [Line, $\n]),
    [Checksum, Name] = string:split(Rest, " "),
    {[#file{filehash=Checksum, filename=Name} | Cards], MoreDigest}.
