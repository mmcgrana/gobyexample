## Go by Example

Content and toolchain for [Go by Example](https://gobyexample.com).


### Overview

The Go by Example site is built by extracting code &
comments from source files in `examples` and rendering
that data via the site `templates`. The programs
implementing this build process are in `tools`.

The build process produces a directory of static files
suitable for serving by any modern HTTP server. The
Go by Example site is served by a
[lightweight Go server](https://github.com/mmcgrana/gobyexample-server).


### Building

To build the site:

```console
$ tools/build
$ open site/index.html
```

To build continuously in a loop:

```console
$ tools/build-loop
```

Builds require the [`pygmentize`](http://pygments.org/)
binary for syntax highlighting. We suggesting using the
most recent version from Bitbucket with e.g.:

```console
$ export PATH="$HOME/repos/pygments:$PATH"
```


### Serving

The site is served by the [gobyexample-server](https://github.com/mmcgrana/gobyexample-server)
tool. To export to this tool:

```console
$ SITEDIR=../gobyexample-server/public tool/build
```


### License

This work is licensed under a
[Creative Commons Attribution 3.0 Unported License](http://creativecommons.org/licenses/by/3.0/).


### Thanks

Thanks to [Jeremy Ashkenas](https://github.com/jashkenas)
for [Docco](http://jashkenas.github.com/docco/), which
inspired this project.
