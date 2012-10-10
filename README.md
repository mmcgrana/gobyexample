## Go by Example

Content and toolchain source for the
[Go by Example](https://gobyexample.com) site.


### Overview

This repo contains:

* `src`: Go and Bash source code for the site
* `meta`: metadata used to generate the site
* `tool`: toolchain used to generate the site
* `template`: HTML templates and CSS for the site

The site is built by extracting the code & comments from
the `.go` and `.sh` source files in `src` and rendering
that data according to `meta` and `templates` via programs
in `tool`.

The generated contented is served by the a
[lightweight server](https://github.com/mmcgrana/gobyexample-server).


### Usage

To validate the source, generate the site, and open the
home page in your browser:

```console
$ tool/build
$ open site/index.html
```

To build continuously in a loop:

```console
$ tool/build-loop
```

Generation requires the [`pygmentize`](http://pygments.org/)
binary for syntax highlighting.


### Serving

The site is served by the [gobyexample-server](https://github.com/mmcgrana/gobyexample-server)
tool. To export to this tool:

```console
$ SITEDIR = ../gobyexample-server/public tool/build
```


### License

This work is licensed under a [Creative Commons Attribution 3.0 Unported License](http://creativecommons.org/licenses/by/3.0/).


### Thanks

Thanks to [Jeremy Ashkenas](https://github.com/jashkenas)
for [Docco](http://jashkenas.github.com/docco/), which
inspired this project.
