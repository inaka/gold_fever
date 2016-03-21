PROJECT = gold_fever

CONFIG ?= config/generic.config
NODEIP ?= 127.0.0.1

RELX_URL := https://github.com/erlware/relx/releases/download/v2.0.0/relx

DEPS = goldrush lager katana shotgun
SHELL_DEPS = eper sync
TEST_DEPS = mixer
BUILD_DEPS = inaka_mk hexer_mk

dep_lager       = hex 3.0.2
dep_goldrush    = hex 0.1.7
dep_katana      = hex 0.2.22
dep_shotgun     = hex 0.2.3
dep_eper        = git https://github.com/massemanet/eper.git     0.97.3
dep_sync        = git https://github.com/rustyio/sync.git        11df81d
dep_mixer       = git https://github.com/inaka/mixer.git         0.1.5
dep_inaka_mk    = git https://github.com/inaka/inaka.mk.git      1.0.0
dep_hexer_mk    = git https://github.com/inaka/hexer.mk.git      1.1.0

DEP_PLUGINS = inaka_mk hexer_mk

include erlang.mk

ERLC_OPTS := +'{parse_transform, lager_transform}'
ERLC_OPTS += +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

CT_OPTS = -cover test/cover.spec -vvv -erl_args -config ${CONFIG}

SHELL_OPTS += -name ${PROJECT}@${NODEIP} -config ${CONFIG} -s lager -s sync -s ${PROJECT}
