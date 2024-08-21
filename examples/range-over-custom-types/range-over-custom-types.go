// Starting with version 1.23, Go has added support for
// [iterators](https://go.dev/blog/range-functions),
// which lets us range over custom types.

package main

import (
	"fmt"
	"iter"
	"slices"
)

// Let's look at the `List` type from the
// [previous example](generics) again. In that example
// we had an `AllElements` method that returned a slice
// of all elements in the list. With Go iterators, we
// can do it better - as shown below.
type List[T any] struct {
	head, tail *element[T]
}

type element[T any] struct {
	next *element[T]
	val  T
}

func (lst *List[T]) Push(v T) {
	if lst.tail == nil {
		lst.head = &element[T]{val: v}
		lst.tail = lst.head
	} else {
		lst.tail.next = &element[T]{val: v}
		lst.tail = lst.tail.next
	}
}

// All returns an _iterator_, which in Go is a function
// with a special signature.
func (lst *List[T]) All() iter.Seq[T] {
	return func(yield func(T) bool) {
		// The iterator function takes another function as
		// a parameter, called `yield` by convention (but
		// the name can be arbitrary). It will call `yield` for
		// every element we want to iterate over, and note `yield`'s
		// return value for a potential early termination.
		for e := lst.head; e != nil; e = e.next {
			if !yield(e.val) {
				return
			}
		}
	}
}

func main() {
	lst := List[int]{}
	lst.Push(10)
	lst.Push(13)
	lst.Push(23)

	// Since `List.All` returns an interator, it can be used
	// in a regular `range` loop!
	for e := range lst.All() {
		fmt.Println(e)
	}

	// Packages like [slices](https://pkg.go.dev/slices) have
	// a number of useful functions to work with iterators.
	// For example, `Collect` takes any iterator and collects
	// all its values into a slice.
	all := slices.Collect(lst.All())
	fmt.Println("all:", all)
}
