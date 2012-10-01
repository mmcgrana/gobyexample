## gbe-book

Go by Example book source.


### Number'ing

```console
$ mate tool/index.txt
$ go run tool/number.go
```


### Gofmt'ing

```console
$ tool/gofmt
```


### Generating

```console
$ cd tool
$ go build generate.go
$ cd ..
$ tool/generate src/063-line-filters/line-filters.go 'Line Filters' > build/line-filters.html
$ prince build/line-filters.html -o build/line-filters.pdf