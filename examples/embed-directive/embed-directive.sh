# Use these commands to run the example.
# (Note: due to limitation on go playground,
# this example can only be run on your local machine.)
$ mkdir -p example_folder
$ echo "hello go" > example_folder/single_file.txt
$ echo "123" > example_folder/multi_file1.hash
$ echo "456" > example_folder/multi_file2.hash

$ go run embed-directive.go
hello go
hello go
123
456

