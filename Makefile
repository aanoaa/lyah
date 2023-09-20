TEST_APP	:= $(shell which doctest)
HS_FILES	:= $(wildcard src/*.hs)

.PHONY: test
test:
	$(TEST_APP) --fast $(HS_FILES)
