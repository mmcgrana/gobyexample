## Go by Example

Content and build toolchain for [Go by Example](https://gobyexample.com),
a site that teaches Go via annotated example programs.


### Overview

The Go by Example site is built by extracting code and
comments from source files in `examples` and rendering
them via the `templates` into a static `public`
directory. The programs implementing this build process
are in `tools`, along with some vendor'd dependencies
in `vendor`.

The built `public` directory can be served by any
static content system. The production site uses S3 and
CloudFront, for example.


### Building

To build the site you'll need Go and Python installed. Run:

```console
$ go get github.com/russross/blackfriday
$ tools/build
$ open public/index.html
```

To build continuously in a loop:

```console
$ tools/build-loop
```

### Publishing

To upload the site:

```console
$ gem install aws-sdk
$ export AWS_ACCESS_KEY_ID=...
$ export AWS_SECRET_ACCESS_KEY=...
$ tools/upload-site
```

### License

This work is copyright Mark McGranaghan and licensed under a
[Creative Commons Attribution 3.0 Unported License](http://creativecommons.org/licenses/by/3.0/).

The Go Gopher is copyright [Renée French](http://reneefrench.blogspot.com/) and licensed under a
[Creative Commons Attribution 3.0 Unported License](http://creativecommons.org/licenses/by/3.0/).


### Translations

Contributor translations of the Go by Example site are available in:

* [Chinese](https://gobyexample.xgwang.me/) by [xg-wang](https://github.com/xg-wang/gobyexample)
* [French](http://le-go-par-l-exemple.keiruaprod.fr) by [keirua](https://github.com/keirua/gobyexample)
* [Italian](http://gobyexample.it) by the [Go Italian community](https://github.com/golangit/gobyexample-it)
* [Korean](https://mingrammer.com/gobyexample/) by [mingrammer](https://github.com/mingrammer)
* [Spanish](http://goconejemplos.com) by the [Go Mexico community](https://github.com/dabit/gobyexample)
* [Ukrainian](http://gobyexample.com.ua/) by [butuzov](https://github.com/butuzov/gobyexample)

### Thanks

Thanks to [Jeremy Ashkenas](https://github.com/jashkenas)
for [Docco](http://jashkenas.github.com/docco/), which
inspired this project.
