# First, be sure that you've installed Postgres
# and have a server running locally at port 5432.

# Then create an example database.
$ createdb gobyexample

# Now install the dependencies for the postgres
# example and try running it.
$ go get github.com/bmizerany/pq
$ go run postgres.go
