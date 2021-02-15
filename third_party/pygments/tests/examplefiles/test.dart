// Greeter example from
// <http://www.dartlang.org/docs/getting-started/interface.html>
class Greeter implements Comparable {
  String prefix = 'Hello,';
  Greeter() {}
  Greeter.withPrefix(this.prefix);
  greet(String name) => print('$prefix $name');

  int compareTo(Greeter other) => prefix.compareTo(other.prefix);
}

void main() {
  Greeter greeter = new Greeter();
  Greeter greeter2 = new Greeter.withPrefix('Hi,');

  num result = greeter2.compareTo(greeter);
  if (result == 0) {
    greeter2.greet('you are the same.');
  } else {
    greeter2.greet('you are different.');
  }
}

