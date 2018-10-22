PROJECT = snp
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = wol

dep_wol = git git@github.com:iosport/wol_commons.git master

SHELL_OPTS = \
  -s $(PROJECT) \
  -config dev.config \
  -eval "snp_demo:run()"

include erlang.mk
