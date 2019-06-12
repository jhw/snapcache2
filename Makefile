PROJECT = snp
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = wol lager

dep_wol = git git@github.com:jhw/wol_commons.git master

SHELL_OPTS = \
  -s $(PROJECT) \
  -config dev.config \
  -eval "snp_demo:run()"

include erlang.mk

ERLC_COMPILE_OPTS= +'{parse_transform, lager_transform}'
ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)
