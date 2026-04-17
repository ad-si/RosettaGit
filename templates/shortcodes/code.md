{%- set content = load_data(path=src, format="plain") -%}
```{{ lang }}
{{ content | trim }}
```
