// _Enumerated types_ (enums) are a special case of
// [sum types](https://en.wikipedia.org/wiki/Algebraic_data_type).
// An enum is a type that has a fixed number of possible
// values, each with a distinct name. Go doesn't have an
// enum type as a distinct language feature, but enums
// are simple to implement using existing language idioms.

package main

import "fmt"

// Our enum type `ServerState` has an underlying `int` type.
type ServerState int

// The possible values for `ServerState` are defined as
// constants. The special keyword [iota](https://go.dev/ref/spec#Iota)
// generates successive constant values automatically; in this
// case 0, 1, 2 and so on.
const (
	StateIdle ServerState = iota
	StateConnected
	StateError
	StateRetrying
)

// By implementing the [fmt.Stringer](https://pkg.go.dev/fmt#Stringer)
// interface, values of `ServerState` can be printed out or converted
// to strings.
//
// This can get cumbersome if there are many possible values. In such
// cases the [stringer tool](https://pkg.go.dev/golang.org/x/tools/cmd/stringer)
// can be used in conjunction with `go:generate` to automate the
// process. See [this post](https://eli.thegreenplace.net/2021/a-comprehensive-guide-to-go-generate)
// for a longer explanation.
var stateName = map[ServerState]string{
	StateIdle:      "idle",
	StateConnected: "connected",
	StateError:     "error",
	StateRetrying:  "retrying",
}

func (ss ServerState) String() string {
	return stateName[ss]
}

func main() {
	ns := transition(StateIdle)
	fmt.Println(ns)
	// If we have a value of type `int`, we cannot pass it to `transition` - the
	// compiler will complain about type mismatch. This provides some degree of
	// compile-time type safety for enums.

	ns2 := transition(ns)
	fmt.Println(ns2)
}

// transition emulates a state transition for a
// server; it takes the existing state and returns
// a new state.
func transition(s ServerState) ServerState {
	switch s {
	case StateIdle:
		return StateConnected
	case StateConnected, StateRetrying:
		// Suppose we check some predicates here to
		// determine the next state...
		return StateIdle
	case StateError:
		return StateError
	default:
		panic(fmt.Errorf("unknown state: %s", s))
	}
}
