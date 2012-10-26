# Running our URL parsing program shows all the different
# pieces that we extracted.
$ go run url-parsing.go 
postgres
user:pass
user
pass
host.com:5432
host.com
5432
/path
f
k=v
map[k:[v]]
v
