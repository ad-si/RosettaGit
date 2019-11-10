public: static templates content config.toml
	zola build

.PHONY: serve
serve:
	zola serve
