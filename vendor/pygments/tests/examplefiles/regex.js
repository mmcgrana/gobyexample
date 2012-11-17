// regex

blah(/abc/);
x = /abc/;
x = /abc/.match;

// math

blah(1/2); //comment
x = 1 / 2 / 3;
x = 1/1/.1;

// broken

x=/1/;
x=1/a/g;
x=a/a/g;

// real-world

var x = 1/(1+Math.sqrt(sum)); // convert to number between 1-0
return Math.round((num / den) * 100)/100;
