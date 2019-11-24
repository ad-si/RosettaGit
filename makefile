public: static templates content config.toml
	zola build


.PHONY: serve
serve:
	zola serve


.PHONY: deploy
deploy: public
	netlify deploy \
		--dir=$< \
		--site=5984a2df-f835-4e6c-87cd-12aa26ec33f3 \
		--prod

.PHONY: clean
clean:
	-rm -r public
