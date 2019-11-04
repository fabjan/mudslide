-module(mudslide_http).

-export([routes/1]).
-export([init/2]).

routes(Prefix) ->
    [
        {Prefix ++ "/files/:checksum", ?MODULE, file_request},
        {Prefix ++ "/manifests/[:checksum]", ?MODULE, manifest_request}
    ].

init(Req, State) ->
    Method = cowboy_req:method(Req),
    Checksum = cowboy_req:binding(checksum, Req),
    HasBody = cowboy_req:has_body(Req),
    Rep = handle(State, Method, Checksum, HasBody, Req),
    {ok, Rep, State}.

handle(_, <<"PUT">>, _, false, Req) ->
    cowboy_req:reply(400, #{}, "missing body\n", Req);

handle(file_request, <<"PUT">>, Checksum, true, Req0) ->
    {FileData, Req} = get_body(Req0),
    case mudslide_blobs:save_file(Checksum, FileData) of
        {ok, Name} ->
            cowboy_req:reply(200, #{}, [Name, $\n], Req);
        {error, {incorrect_file_name, _, Expected}} ->
            Message = ["incorrect checksum, expected ", Expected, $\n],
            cowboy_req:reply(400, #{}, Message, Req)
    end;
handle(file_request, <<"GET">>, Checksum, _, Req) ->
    case mudslide_blobs:get_file_content(Checksum) of
        {ok, FileContent} ->
            cowboy_req:reply(200, #{}, FileContent, Req);
        {error, not_found} ->
            cowboy_req:reply(404, #{}, "not found\n", Req)
    end;

handle(manifest_request, <<"PUT">>, Checksum, true, Req0) ->
    {ManifestData, Req} = get_body(Req0),
    case mudslide_blobs:save_manifest(Checksum, ManifestData) of
        {ok, Name} ->
            cowboy_req:reply(200, #{}, [Name, $\n] , Req);
        {error, malformed_manifest} ->
            cowboy_req:reply(400, #{}, ["malformed manifest", $\n], Req);
        {error, {missing_files, Missing}} ->
            Message = ["missing files: ", lists:join(",", Missing), $\n],
            cowboy_req:reply(400, #{}, Message, Req);
        {error, {incorrect_manifest_name, _, Expected}} ->
            Message = ["incorrect checksum, expected ", Expected, $\n],
            cowboy_req:reply(400, #{}, Message, Req)
    end;
handle(manifest_request, <<"GET">>, undefined, _, Req) ->
    ManifestNames = mudslide_blobs:list_manifests(),
    cowboy_req:reply(200, #{}, [lists:join("\n", ManifestNames), $\n], Req);
handle(manifest_request, <<"GET">>, Checksum, _, Req) ->
    case mudslide_blobs:get_manifest(Checksum) of
        {ok, Manifest} ->
            cowboy_req:reply(200, #{}, Manifest, Req);
        {error, not_found} ->
            cowboy_req:reply(404, #{}, "not found\n", Req)
    end.

get_body(Req0) ->
    {ok, [{Body, true}], Req} = cowboy_req:read_urlencoded_body(Req0),
    {Body, Req}.