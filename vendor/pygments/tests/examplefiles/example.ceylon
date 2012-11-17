doc "A top-level function,
     with multi-line documentation."
void topLevel(String? a, Integer b=5, String... seqs) {
    function nested(String s) {
        print(s[1..2]);
        return true;
    }
    for (s in seqs.filter((String x) x.size > 2)) {
        nested(s);
    }
    value uppers = seqs.sequence[].uppercased;
    String|Nothing z = a;
    Sequence<Integer> ints = { 1, 2, 3, 4, 5 };
}

shared class Example<Element>(name, element) satisfies Comparable<Example<Element>>
        given Element satisfies Comparable<Element> {
    shared String name;
    shared Element element;

    shared actual Comparison compare(Example<Element> other) {
        return element <=> other.element;
    }

    shared actual String string {
        return "Example with " + element.string;
    }
}

Example<Integer> instance = Example {
    name = "Named args call";
    element = 5;
};
