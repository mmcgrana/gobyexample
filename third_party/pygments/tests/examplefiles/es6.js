// Most examples from https://github.com/rse/es6-features under MIT license
const PI = 3.141593;

let callbacks = [];

odds  = evens.map(v => v + 1);

nums.forEach(v => {
   if (v % 5 === 0)
       fives.push(v);
})

function f (x, y, ...a) {
    return (x + y) * a.length;
}

var params = [ "hello", true, 7 ];
var other = [ 1, 2, ...params ]; // [ 1, 2, "hello", true, 7 ]
f(1, 2, ...params) === 9;

var str = "foo";
var chars = [ ...str ]; // [ "f", "o", "o" ]

var customer = { name: "Foo" };
var card = { amount: 7, product: "Bar", unitprice: 42 };
message = `Hello ${customer.name},
want to buy ${card.amount} ${card.product} for
a total of ${card.amount * card.unitprice} bucks?`;

0b111110111 === 503;
0o767 === 503;

for (let codepoint of "𠮷") console.log(codepoint);

function* ();
*function();
yield;

export class Node {
}

isFinite();
isNaN();
isSafeInteger();
x = new Promise(...a);
x = new Proxy(...a);
