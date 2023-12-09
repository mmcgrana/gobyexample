$ go run generics.go
keys: [4 1 2]
list: [10 13 23]

# Note: The order of iteration over map keys is not defined in Go,
# so different invocations may result in different orders.
# The output need not necessarily be keys: [1 2 4].