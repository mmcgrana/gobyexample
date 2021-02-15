import ceylon.language { parseInteger }

doc "A top-level function,
     with multi-line documentation."
void topLevel(String? a, Integer b=5, String* seqs) {
    function nested(String s) {
        print(s[1..2]);
        return true;
    }
    for (s in seqs.filter((String x) => x.size > 2)) {
        nested(s);
    }
    value uppers = seqs.map((String x) {
        return x.uppercased;
    });
    String|Null z = a;
    {Integer+} ints = { 1, 2, 3, 4, 5 };
    value numbers = [ 1, #ffff, #ffff_ffff, $10101010, $1010_1010_1010_1010,
        123_456_789 ];
    value chars = ['a', '\{#ffff}' ];
}

shared class Example_1<Element>(name, element) satisfies Comparable<Example_1<Element>>
        given Element satisfies Comparable<Element> {
    shared String name;
    shared Element element;
    shared [Integer,String] tuple = [1, "2"];
    shared late String lastName;
    variable Integer cnt = 0;

    shared Integer count => cnt;
    assign count {
        assert(count >= cnt);
        cnt = count;
    }

    shared actual Comparison compare(Example_1<Element> other) {
        return element <=> other.element;
    }

    shared actual String string {
        return "Example with ``element.string``";
    }
}

Example_1<Integer> instance = Example_1 {
    element = 5;
    name = "Named args call \{#0060}";
};

object example1 extends Example_1<Integer>("object", 5) {
}