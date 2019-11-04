## Installation

You obviously need [Erlang] here (OTP 20 tested), and [Erlang.mk] for building.

Run `make rel SFX=1` to create a self-extracting release that you can just run.

You can also use `make run` to start the app together with an interactive shell.

The application can be configured by editing `config/sys.config` and rebuilding
the release.

[Erlang]: https://www.erlang.org/downloads
[Erlang.mk]: https://erlang.mk
