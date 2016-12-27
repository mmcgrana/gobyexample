example by David Corbett begins here.

"Implements testable examples."

An example is a kind of thing. An example can be tested. An example is seldom tested.

example ends here.

----
[The] documentation [starts here.]
----

This extension adds examples, which may be tested.

Chapter: Usage

To add an example to the story, we write:

	The foobar is an example.

To interact with it in Inform 6, we write something like:

	To say (E - example): (-
		print (object) {E};
	-).
	[The IDE's documentation viewer does not display the closing -). I don't know how to fix that.]

Section: Testing

We can make an example be tested using:

	now the foobar is tested;

Example: * Exempli Gratia - A simple example.

	*: "Exempli Gratia"

	Include example by David Corbett.

	The Kitchen is a room. The egg is an example, here.

	Before dropping the egg:
		now the egg is tested.
	
	Test me with "get egg / drop egg".
