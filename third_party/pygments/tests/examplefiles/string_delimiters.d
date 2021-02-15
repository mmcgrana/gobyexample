import std.stdio;

void main() {
    // Nesting delimited strings
    auto a = q"{foo " {bar} baz}";
    auto b = q"[foo [bar] " baz]";
    auto c = q"(foo " (bar) baz)";
    auto d = q"<foo <bar> " baz>";
    // Non-nesting delimited strings
    auto e = q"/foo " bar/";
    auto f = q"-Another " string-";
    // "heredoc" strings
    auto g = q"FOO
        This is a string!
FOO";
    // Token strings (only the q{} should be highlighted as a string)
    auto h = q{
        int i;
        void foo() { writefln("Hello, world!"); }
    };
}
