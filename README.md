## Go by Example

Content, toolchain, and web server for [Go by Example](https://dlintw.github.io/gobyexample)

The original source come from [gobyexample.com](https://gobyexample.com).

### Overview

The Go by Example site is built by extracting code &
comments from source files in `examples` and rendering
that data via the site `templates`. The programs
implementing this build process are in `tools`.

The build process produces a directory of static files -
`public` - suitable for serving by any modern HTTP server.
We include a lightweight Go server in `server.go`.


### Building

To build the site:

```console
# install python-pygments package on your linux distribution.
$ go get github.com/russross/blackfriday  # markdown processor
$ rm public/*  # optional step for force re-generate
$ cd tools ; $ ./buildexe ; cd ..        # optional for reduce build time
$ tools/build
$ open index.html or public/index.html
```

To build continuously in a loop:

```console
$ tools/build-loop
```

### License

This work is copyright Mark McGranaghan and licensed under a
[Creative Commons Attribution 3.0 Unported License](http://creativecommons.org/licenses/by/3.0/).

The Go Gopher is copyright [Renée French](http://reneefrench.blogspot.com/) and licensed under a
[Creative Commons Attribution 3.0 Unported License](http://creativecommons.org/licenses/by/3.0/).


### Translations

Contributor translations of the Go by Example site are available in:

* [Chinese](http://everyx.github.io/gobyexample/) by [everyx](https://github.com/everyx)


### Thanks

Thanks to [Jeremy Ashkenas](https://github.com/jashkenas)
for [Docco](http://jashkenas.github.com/docco/), which
inspired this project.
