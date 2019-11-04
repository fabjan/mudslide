-module(mudslide_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(BlobStorePrefix) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [BlobStorePrefix]).

init([BlobStorePrefix]) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [
        #{
            id => mudslide_blobs,
            start => {mudslide_blobs, start_link, [BlobStorePrefix]}
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
