/* This file /* which is totally legal scala */ will not be highlighted
   correcty by pygments */

object ⌘ {
  val `interface` = """
A
"Multiline"
String
"""

  val foo_+ = "foo plus"
  val foo_⌬⌬ = "double benzene"

  // Test some interpolated strings
  val mu = s"${if (true) "a:b" else "c" {with "braces"}}"
  val mu2 = f"${if (true) "a:b" else "c" {with "braces"}}"
  val raw = raw"a raw\nstring\"with escaped quotes"

  def main(argv: Array[String]) {
    println(⌘.interface + " " + foo_+ + " " + foo_⌬⌬ )
  }
}

