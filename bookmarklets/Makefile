BIN = $(shell npm bin)
BMKS = $(patsubst %.js, %.bookmarklet.js, $(filter-out %.bookmarklet.js, $(wildcard *.js)))

all: $(BMKS)

%.bookmarklet.js: %.js node_modules
	$(BIN)/babili < $< > $@

node_modules: package.js
	npm install

.PHONY: clean
clean:
	rm *.bookmarklet.js
