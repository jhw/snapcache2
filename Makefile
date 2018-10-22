PROJECT = snp
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

SHELL_OPTS = \
  -s $(PROJECT) \
  -config dev.config \
  -eval "snp_demo:run()"

include erlang.mk
