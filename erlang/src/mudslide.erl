-module(mudslide).
-export([main/1]).

action(["stats"], Options) ->
    Prefix = option(prefix, Options),
    println("prefix: ~s", [Prefix]),
    mudslide_blobs:start_link(Prefix),
    println("stored manifests:"),
    [println(M) || M <- mudslide_blobs:list_manifests()],
    println("stored files:"),
    [println(M) || M <- mudslide_blobs:list_files()];

action(["store", Data], Options) ->
    mudslide_blobs:start_link(option(prefix, Options)),
    Name = mudslide_blobs:save_file(Data),
    println("stored ~s", [Name]);

action(["dump", Hash], Options) ->
    mudslide_blobs:start_link(option(prefix, Options)),
    {ok, Data} = mudslide_blobs:get_file_content(Hash),
    println(Data);

action(_, _) -> usage(1).

options() ->
    [{prefix, $d, "prefix", {string, "/tmp/mudslide"},
     "File store root prefix"},
     {help, $h, "help", boolean, "Print usage and exit"}].

usage(ExitCode) ->
    getopt:usage(
        options(), "mudslide", "[action ...]",
        [{"", ""},
        {"Actions:", ""},
        {"stats", "Prints information about the storage"},
        {"store STR", "Store the string STR"},
        {"dump HASH", "Prints the contents of file HASH"}]
    ),
    halt(ExitCode).

option(Name, Options) ->
    {Name, Value} = proplists:lookup(Name, Options),
    Value.

println(F, [Args]) -> io:format(F ++ "~n", [Args]).
println(S)         -> println("~s", [S]).

main(Args) ->
    case getopt:parse(options(), Args) of
        {ok, {Options, Action}} -> action(Action, Options);
        _ -> usage(1)
    end.
