/regexp/.test(foo) || x = [/regexp/,/regexp/, /regexp/, // comment
// comment
/regexp/];
if (/regexp/.test(string))
{/regexp/.test(string);};
x =/regexp/;
x = /regexp/;
if (0</regexp/.exec(string) || 1>/regexp/.exec(string))
x = { u:/regexp/, v: /regexp/ };
foo();/regexp/.test(string); /regexp/.test(string);
if (!/regexp/) foobar();
x = u %/regexp/.exec(string) */regexp/.exec(string) / /regexp/.exec(string);
x = u?/regexp/.exec(string) : v +/regexp/.exec(string) -/regexp/.exec(string);
a = u^/regexp/.exec(string) &/regexp/.exec(string) |/regexp/.exec(string) +~/regexp/.exec(string);
x = /regexp/ /* a comment */ ;
x = /[reg/exp]/;
x = 4/2/i;
x = (a == b) ?/* this is a comment */ c : d;
/// a comment //
a = /regex//2/1; //syntactically correct, returns NaN




/* original examples */

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
