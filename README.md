# Go by Example

Content and build toolchain for [Go by Example](https://gobyexample.com),
a site that teaches Go via annotated example programs.

### Overview

The Go by Example site is built by extracting code and
comments from source files in `examples` and rendering
them using `templates` into a static `public`
directory. The programs implementing this build process
are in `tools`, along with dependencies specified in
the `go.mod`file.

The built `public` directory can be served by any
static content system. The production site uses S3 and
CloudFront, for example.

### Building

[![test](https://github.com/mmcgrana/gobyexample/actions/workflows/test.yml/badge.svg)](https://github.com/mmcgrana/gobyexample/actions/workflows/test.yml)

To build the site you'll need Go installed. Run:

```console
$ tools/build
```

To build continuously in a loop:

```console
$ tools/build-loop
```

To see the site locally:

```console
$ tools/serve
```

and open `http://127.0.0.1:8000/` in your browser.

### Publishing

To upload the site:

```console
$ export AWS_ACCESS_KEY_ID=...
$ export AWS_SECRET_ACCESS_KEY=...
$ tools/upload
```

### License

This work is copyright Mark McGranaghan and licensed under a
[Creative Commons Attribution 3.0 Unported License](http://creativecommons.org/licenses/by/3.0/).

The Go Gopher is copyright [Renée French](https://reneefrench.blogspot.com/) and licensed under a
[Creative Commons Attribution 3.0 Unported License](http://creativecommons.org/licenses/by/3.0/).


### Translations

Contributor translations of the Go by Example site are available in:

* [Chinese](https://gobyexample-cn.github.io/) by [gobyexample-cn](https://github.com/gobyexample-cn)
* [French](http://le-go-par-l-exemple.keiruaprod.fr) by [keirua](https://github.com/keirua/gobyexample)
* [Italian](https://gobyexample.it) by the [Go Italian community](https://github.com/golangit/gobyexample-it)
* [Japanese](http://spinute.org/go-by-example) by [spinute](https://github.com/spinute)
* [Korean](https://mingrammer.com/gobyexample/) by [mingrammer](https://github.com/mingrammer)
* [Russian](https://gobyexample.com.ru/) by [badkaktus](https://github.com/badkaktus)
* [Ukrainian](https://butuzov.github.io/gobyexample/) by [butuzov](https://github.com/butuzov/gobyexample)
* [Brazilian Portuguese](https://lcslitx.github.io/GoEmExemplos/) by [lcslitx](https://github.com/LCSLITX)
* [Vietnamese](https://gobyexample.vn/) by [s6k Gopher](https://github.com/s6k-gopher/gobyexample-vn)
* [Burmese](https://setkyar.github.io/gobyexample) by [Set Kyar Wa Lar](https://github.com/setkyar/gobyexample)

### Thanks

Thanks to [Jeremy Ashkenas](https://github.com/jashkenas)
for [Docco](http://jashkenas.github.io/docco/), which
inspired this project.

### FAQ

#### I found a problem with the examples; what do I do?

We're very happy to fix problem reports and accept contributions! Please submit
[an issue](https://github.com/mmcgrana/gobyexample/issues) or send a Pull Request.
See `CONTRIBUTING.md` for more details.

#### What version of Go is required to run these examples?

Given Go's strong [backwards compatibility guarantees](https://go.dev/doc/go1compat),
we expect the vast majority of examples to work on the latest released version of Go
as well as many older releases going back years.

That said, some examples show off new features added in recent releases; therefore,
it's recommended to try running examples with the latest officially released Go version
(see Go's [release history](https://go.dev/doc/devel/release) for details).

#### I'm getting output in a different order from the example. Is the example wrong?

Some of the examples demonstrate concurrent code which has a non-deterministic
execution order. It depends on how the Go runtime schedules its goroutines and
may vary by operating system, CPU architecture, or even Go version.

Similarly, examples that iterate over maps may produce items in a different order
from what you're getting on your machine. This is because the order of iteration
over maps in Go is [not specified and is not guaranteed to be the same from one
iteration to the next](https://go.dev/ref/spec#RangeClause).

It doesn't mean anything is wrong with the example. Typically the code in these
examples will be insensitive to the actual order of the output; if the code is
sensitive to the order - that's probably a bug - so feel free to report it.




