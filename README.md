## Go by Example

Content, toolchain, and web server for [Go by Example](https://gobyexample.com).


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
$ tools/build
$ open public/index.html
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


### Local Deploy

```bash
$ mkdir -p $GOPATH/src/github.com/mmcgrana
$ cd $GOPATH/src/github.com/mmcgrana
$ git clone git@github.com:mmcgrana/gobyexaple.git
$ cd gobyexample
$ go get
$ foreman start
$ foreman open
```


### Platform Deploy

Basic setup:

```bash
$ export DEPLOY=$USER
$ heroku create gobyexample-$DEPLOY -r $DEPLOY
$ heroku config:add -r $DEPLOY \
    BUILDPACK_URL=https://github.com/mmcgrana/heroku-buildpack-go.git -r $DEPLOY \
    CANONICAL_HOST=gobyexample-$DEPLOY.herokuapp.com -r $DEPLOY \
    FORCE_HTTPS=1 \
    AUTH=go:byexample
$ git push $DEPLOY master
$ heroku open -r $DEPLOY
```

Add a domain + SSL:

```bash
$ heroku domains:add $DOMAIN
$ heroku addons:add ssl -r $DEPLOY
# order ssl cert for domain
$ cat > /tmp/server.key
$ cat > /tmp/server.crt.orig
$ cat /tmp/server.crt.orig /tmp/rapidssl_bundle.pem > /tmp/server.crt
$ heroku certs:add /tmp/server.crt /tmp/server.key -r $DEPLOY
# add ALIAS record from domain to ssl endpoint dns
$ heroku config:add CANONICAL_HOST=$DOMAIN -r $DEPLOY
$ heroku open -r $DEPLOY
```




### License

This work is licensed under a
[Creative Commons Attribution 3.0 Unported License](http://creativecommons.org/licenses/by/3.0/).


### Thanks

Thanks to [Jeremy Ashkenas](https://github.com/jashkenas)
for [Docco](http://jashkenas.github.com/docco/), which
inspired this project.
