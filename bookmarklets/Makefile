BMKS = $(patsubst %.js, %.bookmarklet.js, $(filter-out %.bookmarklet.js, $(wildcard *.js)))

all: $(BMKS)

%.bookmarklet.js: %.js node_modules
	npx esbuild --minify < $< > $@

node_modules: package.json
	npm install

.PHONY: clean
clean:
	rm *.bookmarklet.js
