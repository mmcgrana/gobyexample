package main                        // Use `os.Args` to access command-line arguments and
                                    // the name of the program.
import ("os"; "fmt")

func main() {
	argsWithProg := os.Args         // `os.Args` includes the program name as the first
	argsWithoutProg := os.Args[1:]  // value.

	arg := os.Args[3]               // `Args` are a slice, you can get individual args
                                    // with normal indexing.

	fmt.Println(argsWithProg)       
	fmt.Println(argsWithoutProg)
	fmt.Println(arg)
}

/*
$ go build command-line-args        // Build a `command-line-args` binary so that we have
                                    // the expected program name.

$ ./command-line-args a b c d
[command-line-args a b c d]       
[a b c d]
c
*/
