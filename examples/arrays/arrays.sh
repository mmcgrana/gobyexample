# Note that arrays appear in the form `[v1 v2 v3 ...]`
# when printed with `fmt.Println`.
$ go run arrays.go
emp: [0 0 0 0 0]
set: [0 0 0 0 100]
get: 100
len: 5
dcl: [1 2 3 4 5]
2d:  [[0 1 2] [1 2 3]]
Element 0 had value 1
Element 1 had value 2
Element 2 had value 3
Element 3 had value 4
Element 4 had value 5

# You'll see _slices_ much more often than arrays in
# typical Go. We'll look at slices next.
