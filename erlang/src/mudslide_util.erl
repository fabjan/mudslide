-module(mudslide_util).
-compile(export_all). % it's what utils are for

-define(CHECKSUM_ALGO, sha). % sha is sha1

get_digest() ->
    crypto:hash_init(?CHECKSUM_ALGO).

hex_string(Bytes) ->
    lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B:8>> <= Bytes]).

checksum(Bytes) ->
    HashBytes = crypto:hash(?CHECKSUM_ALGO, Bytes),
    hex_string(HashBytes).

long_str(Strings) ->
    string:join(Strings, "\n").

example_manifest() ->
    long_str([
        "F e242ed3bffccdf271b7fbaf34ed72d089537b42f bar.txt",
        "F f1d2d2f924e986ac86fdf7b36c94bcdf32beec15 foo.txt",
        "Z 1fb2be2881c60c77abc59aa4b7ae5fabb5dbe96d"
    ]).