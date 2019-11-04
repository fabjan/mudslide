-module(mudslide_blobs).
-behaviour(gen_server).

-export([save_file/2]).
-export([list_files/0]).
-export([get_file_content/1]).
-export([save_manifest/2]).
-export([list_manifests/0]).
-export([get_manifest/1]).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(SERVER_NAME, ?MODULE).

%%% Client API %%%

save_file(Name, Data) ->
    gen_server:call(?SERVER_NAME, {save_file, str(Name), Data}).
list_files() ->
    gen_server:call(?SERVER_NAME, {list_files}).
get_file_content(Name) ->
    gen_server:call(?SERVER_NAME, {get_file_content, str(Name)}).

save_manifest(Name, Data) ->
    Text = str(Data),
    case catch mudslide_manifest:parse_manifest(Text) of
        {'EXIT', _} -> {error, malformed_manifest};
        _ -> gen_server:call(?SERVER_NAME, {save_manifest, str(Name), Text})
    end.
list_manifests() ->
    gen_server:call(?SERVER_NAME, {list_manifests}).
get_manifest(Name) ->
    gen_server:call(?SERVER_NAME, {get_manifest, str(Name)}).

%%% Helpers %%%

str(IoData) ->
    lists:flatten(io_lib:format("~s", [IoData])).

files_root(StorePrefix) ->
    StorePrefix ++ "/files".
manifests_root(StorePrefix) ->
    StorePrefix ++ "/manifests".
file_store_path(StorePrefix, Name) ->
    files_root(StorePrefix) ++ "/" ++ Name.
manifest_store_path(StorePrefix, Name) ->
    manifests_root(StorePrefix) ++ "/" ++ Name.

name_file(Data) ->
    mudslide_util:checksum(Data).
name_manifest(Manifest) ->
    mudslide_manifest:get_checksum(Manifest).

write_file_if_not_exists(Name, Bytes) ->
    case filelib:is_file(Name) of
        true -> ok;
        false -> file:write_file(Name, Bytes)
    end.

missing_file_checker(StorePrefix) ->
    fun (FileHash) ->
        not filelib:is_file(file_store_path(StorePrefix, FileHash))
    end.

%%% gen_server %%%

start_link(Prefix) ->
    gen_server:start_link({local, ?SERVER_NAME}, ?MODULE, [Prefix], []).

init([Prefix]) ->
    StorePrefix = Prefix ++ "/mudslide_blobs.store",
    ok = filelib:ensure_dir(files_root(StorePrefix) ++ "/."),
    ok = filelib:ensure_dir(manifests_root(StorePrefix) ++ "/."),
    {ok, StorePrefix}.

handle_call({save_file, Name, Data}, _From, StorePrefix) ->
    case name_file(Data) of
        Name ->
            Filename = file_store_path(StorePrefix, Name),
            ok = write_file_if_not_exists(Filename, Data),
            {reply, {ok, Name}, StorePrefix};
        Expected ->
            {reply, {error, {incorrect_file_name, Name, Expected}}, StorePrefix}
    end;

handle_call({list_files}, _From, StorePrefix) ->
    {ok, FileHashes} = file:list_dir(files_root(StorePrefix)),
    {reply, FileHashes, StorePrefix};

handle_call({get_file_content, Name}, _From, StorePrefix) ->
    case file:read_file(file_store_path(StorePrefix, Name)) of
        {ok, Data} ->
            {reply, {ok, Data}, StorePrefix};
        {error, enoent} ->
            {reply, {error, not_found}, StorePrefix}
    end;

handle_call({save_manifest, Name, Text}, _From, StorePrefix) ->
    Manifest = mudslide_manifest:parse_manifest(Text),
    FileHashes = mudslide_manifest:get_file_hashes(Manifest),
    case lists:filter(missing_file_checker(StorePrefix), FileHashes) of
        [] ->
            case name_manifest(Manifest) of
                Name ->
                    Filename = manifest_store_path(StorePrefix, Name),
                    ok = write_file_if_not_exists(Filename, Text),
                    {reply, {ok, Name}, StorePrefix};
                Expected ->
                    Error = {incorrect_manifest_name, Name, Expected},
                    {reply, {error, Error}, StorePrefix}
            end;
        Missing ->
            {reply, {error, {missing_files, Missing}}, StorePrefix}
    end;

handle_call({list_manifests}, _From, StorePrefix) ->
    {ok, ManifestNames} = file:list_dir(manifests_root(StorePrefix)),
    {reply, ManifestNames, StorePrefix};

handle_call({get_manifest, Name}, _From, StorePrefix) ->
    case file:read_file(manifest_store_path(StorePrefix, Name)) of
        {ok, Text} ->
            {reply, {ok, Text}, StorePrefix};
        {error, enoent} ->
            {reply, {error, not_found}, StorePrefix}
    end;

handle_call(_, From, StorePrefix) ->
    logger:warning(io_lib:format("unexpected call from ~w", [From])),
    {noreply, StorePrefix}.

handle_cast(_, StorePrefix) ->
    logger:warning("unexpected cast"),
    {noreply, StorePrefix}.

handle_info(_, StorePrefix) ->
    logger:warning("unexpected info"),
    {noreply, StorePrefix}.