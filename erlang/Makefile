PROJECT = mudslide
PROJECT_DESCRIPTION = Store files and sets of files
PROJECT_VERSION = 0.1.0

DEPS = cowboy getopt
dep_cowboy_commit = 2.6.3

BUILD_DEPS = elvis_mk
dep_elvis_mk = git https://github.com/inaka/elvis.mk.git 1.0.0

DEP_PLUGINS = elvis_mk

ifdef PROD
SFX=1
RELX_CONFIG = $(CURDIR)/relx.prod.config
else
RELX_OPTS = -d true
RELX_CONFIG = $(CURDIR)/relx.dev.config
DEPS += sync
endif

include erlang.mk

ERLC_OPTS := +nowarn_export_all
