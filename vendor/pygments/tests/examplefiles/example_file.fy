class Person {
  def initialize: @name age: @age {
    """
    This is a docstring for the Person constructor method.
    Docstrings usually are multi-line, like this one.
    """
  }

  def to_s {
    # return is optional in this case, but we use it nontheless
    return "Person with name: #{@name inspect} and age: #{@age}"
  }
}

class PersonWithCity : Person {
  def initialize: @name age: @age city: @city {
  }

  def to_s {
    super to_s ++ " living in: #{@city inspect}"
  }
}

p1 = Person new: "Johnny Jackson" age: 42
p1 println # prints: Person with name: "Johnny Jackson" and age: 42

p2 = PersonWithCity new: "John Appleseed" age: 55 city: "New York"
p2 println # prints: Person with name: "John Appleseed" age: 55 living in: "New York"

array = [1,2,3, "foo", 'bar]
hash = <['foo => "bar", 'bar => 42]>
tuple = (1,2,"hello","world")
block = |x, y| {
  x + y println
}
block call: [4,2]

0b010101 & 0b00101 to_s: 2 . println
0xFF & 0xAB to_s: 16 . println
0o77 > 0o76 println
123.123 + 0.222 println

x = 0
try {
  10 / x println
} catch ZeroDivisionError => e {
  x = 3
  retry
} finally {
  "Finally, done!" println
}

def a_method: arg1 with_default_arg: arg2 (42) {
  arg1 * arg2 println
}

a_method: 42
a_method: 42 with_default_arg: 85

class ClassWithClassMethod {
  def self class_method1 {
    'works
  }

  def ClassWithClassMethod class_method2 {
    'this_as_well
  }
}

ClassWithClassMethod class_method1 println
ClassWithClassMethod class_method2 println

def another_method: block {
  1 upto: 10 . map: block
}

# local returns
another_method: |x| { return_local x * 2 } . inspect println


# pattern matching:
class PatternMatching {
  def match_it: obj {
    match obj {
      case String -> "It's a String!" println
      case Fixnum -> "It's a Number!" println
      case _ -> "Aything else!" println
    }
  }

  def match_with_extract: str {
    match str {
      # m holds the MatchData object, m1 & m2 the first and second matches
      case /^(.*) : (.*)$/ -> |m, m1, m2|
        "First match: #{m1}" println
        "Second match: #{m2}" println
    }
  }
}

pm = PatternMatching new
pm match_it: "foo"
pm match_it: 42
pm match_it: 'foo

pm match_with_extract: "Hello : World!"


# calling ruby methods:
[3, 2, 1] reverse() each() |a| { puts(a) }
"Hello" sub("ll", "y") println
[3, 2, 1] map() |a| { a * 2 } inject(0) |s i| { s + i } println

# test symbol highlighting
['foo]
['foo?!]
{'foo}
{'foo!?}
{'foo:bar?!=&/:}
('foo)

# future sends
42 @ to_s class println
42 @ to_s: 16 . value println

# async sends
42 @@ println
42 @@ upto: 100
