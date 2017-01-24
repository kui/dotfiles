BIN = $(shell npm bin)
BMKS = $(patsubst %.js, %.bookmarklet.js, $(filter-out %.bookmarklet.js, $(wildcard *.js)))

all: $(BMKS)

%.bookmarklet.js: %.js
	$(BIN)/babili < $< > $@

.PHONY: clean
clean:
	rm *.bookmarklet.js
