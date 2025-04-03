.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY" | sed 's/.PHONY: /- /'


public: static templates content config.toml
	zola build


.PHONY: build
build: public


.PHONY: serve  # Start the Zola development server
serve:
	zola serve


.PHONY: deploy  # Deploy website with Netlify
deploy: public
	netlify deploy \
		--dir=$< \
		--site=5984a2df-f835-4e6c-87cd-12aa26ec33f3 \
		--prod


.PHONY: clean
clean:
	-rm -r public
