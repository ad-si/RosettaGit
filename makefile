.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY" | sed 's/.PHONY: /- /'


public: static templates content config.toml
	RAYON_NUM_THREADS=4 zola build


.PHONY: test
test:
	zola check


.PHONY: format
format:
	echo "TODO"


.PHONY: build
build: public


.PHONY: serve  # Start the Zola development server
serve:
	zola serve


.PHONY: prune-public  # Remove source files from public/ before deploy
prune-public: public
	find public -type f -name '*.md' -delete
	find public/tasks public/drafts -mindepth 2 -type f \
		! -name 'index.html' \
		! -iname '*.png' ! -iname '*.jpg' ! -iname '*.jpeg' \
		! -iname '*.gif' ! -iname '*.svg' ! -iname '*.webp' \
		-delete


.PHONY: deploy  # Deploy website with Netlify
deploy: public prune-public
	netlify deploy \
		--dir=public \
		--site=5984a2df-f835-4e6c-87cd-12aa26ec33f3 \
		--prod


.PHONY: check-links  # Check links with Lychee (https://github.com/lycheeverse/lychee)
check-links: public
	lychee --no-progress --root-dir "$(CURDIR)/public" public


.PHONY: clean
clean:
	-rm -r public
