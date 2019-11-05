-module(mudslide_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    {ok, BlobStorePrefix} = application:get_env(mudslide, blob_store_prefix),
    {ok, HttpPort} = application:get_env(mudslide, http_port),
    application:ensure_all_started(cowboy), % :shruggie:
    ApiRoutes = [{"/", mudslide_http, homepage_request}] ++
                [{"/api" ++ P, H, S} || {P, H, S} <- mudslide_http:routes()],
    Dispatch = cowboy_router:compile([{'_', ApiRoutes}]),
    {ok, _} = cowboy:start_clear(mudslide_cowboy,
        [{port, HttpPort}],
        #{env => #{dispatch => Dispatch}}
    ),
    mudslide_sup:start_link(BlobStorePrefix).

stop(_State) ->
    ok.
