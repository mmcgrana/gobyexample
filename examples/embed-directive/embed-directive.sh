# Use these commands to run the example.
# (Note: due to limitation on go playground,
# this example can only be run on your local machine.)
$ mkdir -p folder
$ echo "hello go" > folder/single_file.txt
$ echo "123" > folder/file1.hash
$ echo "456" > folder/file2.hash

$ go run embed-directive.go
hello go
hello go
123
456

