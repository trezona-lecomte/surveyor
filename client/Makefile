default: all

ELM_FILES = $(shell find src -type f -name '*.elm' -not -name '.\#*')

all: app.js surveyor.css

app.js: $(ELM_FILES) elm-stuff/.stamp
	elm-make --yes src/Main.elm --output app.js

surveyor.css:
	sass css/surveyor.scss css/compiled/surveyor.css

elm-stuff/.stamp: elm-package.json
	elm-package install --yes && touch ./elm-stuff/.stamp

clean-deps:
	rm -rf ./elm-stuff

clean:
	rm -f app.js
	rm -rf elm-stuff/build-artifacts

debug:
	elm-make --debug --yes src/Main.elm --output app.js

realclean: clean clean-deps

watch:
	@make js || true
	@echo "Watching for changes..."
	@fswatch src/ css/surveyor.scss | grep --line-buffered -v '.\#' | while read -r changed; do date; echo "MODIFIED: $$changed"; make all || true; done

.PHONY: js
.PHONY: realclean
