// Starting with version 1.18, Go has added support for
// _generics_, also known as _type parameters_.

package main

import "fmt"

type MyInt16 int16
type MyInt int

func (m MyInt16) String() string {
	return fmt.Sprintf("{MyInt16: %d}", m)
}

func (m MyInt) String() string {
	return fmt.Sprintf("{MyInt: %d}", m)
}

// `~` the underlying type of T must be itself, and T cannot be an interface.
type Number interface {
	int | ~int16
}

// An interface representing all types with underlying type int that implement the String method.
type IntString interface {
	~int16
	String() string
}

func SumNumber[T Number](a, b T) T {
	return a + b
}

func SumIntString[T IntString](a, b T) (T, string) {
	return a + b, a.String() + ", " + b.String()
}

// As an example of a generic function, `MapKeys` takes
// a map of any type and returns a slice of its keys.
// This function has two type parameters - `K` and `V`;
// `K` has the `comparable` _constraint_, meaning that
// we can compare values of this type with the `==` and
// `!=` operators. This is required for map keys in Go.
// `V` has the `any` constraint, meaning that it's not
// restricted in any way (`any` is an alias for `interface{}`).
func MapKeys[K comparable, V any](m map[K]V) []K {
	r := make([]K, 0, len(m))
	for k := range m {
		r = append(r, k)
	}
	return r
}

// As an example of a generic type, `List` is a
// singly-linked list with values of any type.
type List[T any] struct {
	head, tail *element[T]
}

type element[T any] struct {
	next *element[T]
	val  T
}

// We can define methods on generic types just like we
// do on regular types, but we have to keep the type
// parameters in place. The type is `List[T]`, not `List`.
func (lst *List[T]) Push(v T) {
	if lst.tail == nil {
		lst.head = &element[T]{val: v}
		lst.tail = lst.head
	} else {
		lst.tail.next = &element[T]{val: v}
		lst.tail = lst.tail.next
	}
}

func (lst *List[T]) GetAll() []T {
	var elems []T
	for e := lst.head; e != nil; e = e.next {
		elems = append(elems, e.val)
	}
	return elems
}

func main() {
	var m = map[int]string{1: "2", 2: "4", 4: "8"}

	// When invoking generic functions, we can often rely
	// on _type inference_. Note that we don't have to
	// specify the types for `K` and `V` when
	// calling `MapKeys` - the compiler infers them
	// automatically.
	fmt.Println("keys:", MapKeys(m))

	// ... though we could also specify them explicitly.
	_ = MapKeys[int, string](m)

	lst := List[int]{}
	lst.Push(10)
	lst.Push(13)
	lst.Push(23)
	fmt.Println("list:", lst.GetAll())

	// We can use `MyInt16` as the parameter of the function `SumNumber`, because its underlying type is int16.
	fmt.Println(SumNumber(MyInt16(1), MyInt16(2)))
	fmt.Println(SumNumber(1, 2))

	// We can't use `MyInt` as the parameter of the function `SumInString`, because underlying type of `MyInt` is int not int16.
	// also int16 does not implement `IntString` (missing method String).
	result, str := SumIntString(MyInt16(1), MyInt16(2))
	fmt.Printf("result: %d, output: %s\n", result, str)
}
