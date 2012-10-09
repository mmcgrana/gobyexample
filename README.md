## Go by Example

Source for [Go by Example](https://gobyexample.com), a
hands-on guide to Go.


### Overview

This repo contains:

* `src`: Go and Bash source code for the site
* `meta`: metadata used to generate the site
* `templates`: HTML templates and CSS for the site
* `tool`: toolchain used to generate the site

The site is built by by extracting the code & comments
from the .go and .sh source files in `src`, and rendering
that data according to `meta` and `templates` via the
programs in `tool`.


### Usage

Generation requires the [`pygmentize`](http://pygments.org/)
binary for syntax highlighting.

To validate the source, generate the site, and open the
home page in your browser:

```console
$ tool/build
$ open site/index.html
```


### License

This work is licensed under a [Creative Commons Attribution 3.0 Unported License](http://creativecommons.org/licenses/by/3.0/).
