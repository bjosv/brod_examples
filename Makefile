.PHONY: all test test-env profile clean

all: clean profile
	@echo Available profiles: [old, current, latest]
	@echo Set a profile using:
	@echo export PROFILE=xxxx
	@echo
	@rebar3 as ${PROFILE} compile
	@rebar3 as ${PROFILE} tree

test: tree
	@rebar3 as ${PROFILE} ct -v

tree:
	@rebar3 as ${PROFILE} tree

dialyzer:
	@rebar3 as ${PROFILE} dialyzer

test-env:
	@scripts/setup-test-env.sh

profile:
	@echo ============================================
	@echo Current profile: ${PROFILE}
	@echo ============================================

clean:
	@rebar3 clean
	@rm -rf _build rebar.lock

# old
# │
# └─ brod─3.7.5 (hex package)
#    ├─ kafka_protocol─2.2.7 (hex package)
#    │  ├─ crc32cer─0.1.3 (hex package)
#    │  └─ snappyer─1.2.4 (hex package)
#    └─ supervisor3─1.1.8 (hex package)

# current
# │
# └─ brod─3.9.5 (hex package)
#    ├─ kafka_protocol─2.3.3 (hex package)
#    │  ├─ crc32cer─0.1.3 (hex package)
#    │  └─ snappyer─1.2.4 (hex package)
#    └─ supervisor3─1.1.8 (hex package)

# latest
# │
# ├─ brod─3.9.5 (hex package)
# │  └─ supervisor3─1.1.8 (hex package)
# └─ kafka_protocol─2.4.0 (git repo)
#    ├─ crc32cer─0.1.4 (hex package)
#    └─ snappyer─1.2.5 (hex package)
