module: sample
comment: for make sure that does not highlight per word.
         and it continues on to the next line.

define class <car> (<object>)
  slot serial-number :: <integer> = unique-serial-number();
  constant slot model-name :: <string>,
    required-init-keyword: model:;
  each-subclass slot has-sunroof? :: <boolean>,
    init-keyword: sunroof?:,
    init-value: #f;
  keyword foo:;
  required keyword bar:;
end class <car>;

define class <flying-car> (<car>)
end class <flying-car>;

let flying-car = make(<flying-car>);
let car? :: <car?> = #f;
let prefixed-car :: <vehicles/car> = #f;
let model :: <car-911> = #f;

define constant $empty-string = "";
define constant $escaped-backslash = '\\';
define constant $escaped-single-quote = '\'';

define variable *unique-serial-number* = 0;

define function unique-serial-number() => (usn :: <integer>)
  let serial = *unique-serial-number*;
  *unique-serial-number* := *unique-serial-number* + 1;
  serial;
end function;

define constant $blue-car = make(<car>, model: "Viper");
define constant $black-car = make(<car>, model: "Town Car", sunroof?: #t);
define constant $red-car = make(<car>, model: "F40", sunroof?: #f);

define method foo() => _ :: <boolean>
  #t
end method;

define method foo() => _ :: <boolean>;
  #t
end method;

define method \+
    (offset1 :: <time-offset>, offset2 :: <time-offset>)
 => (sum :: <time-offset>)
  let sum = offset1.total-seconds + offset2.total-seconds;
  make(<time-offset>, total-seconds: sum);
end method \+;

define method bar ()
  1 | 2 & 3
end

if (bar)
  1
elseif (foo)
  2
else
  3
end if;

select (foo by instance?)
  <integer> => 1
  otherwise => 3
end select;

/* multi
   line
   comment
*/

/* multi line comments
  /* can be */
  nested */

define constant $symbol = #"hello";
define variable *vector* = #[3.5, 5]
define constant $list = #(1, 2);
define constant $pair = #(1 . "foo")

let octal-number = #o238;
let hex-number = #x3890ADEF;
let binary-number = #b1010;
let float-exponent = 3.5e10;

block (return)
  with-lock (lock)
    return();
  end;
exception (e :: <error>)
    format-out("Oh no");
cleanup
    return();
afterwards
    format-out("Hello");
end;

define macro repeat
  { repeat ?:body end }
   => { block (?=stop!)
          local method again() ?body; again() end;
          again();
        end }
end macro repeat;

define macro with-decoded-seconds
  {
    with-decoded-seconds
      (?max:variable, ?min:variable, ?sec:variable = ?time:expression)
      ?:body
    end
  }
    => {
         let (?max, ?min, ?sec) = decode-total-seconds(?time);
         ?body
       }
end macro;

let x = "This size call should be seen as a builtin despite the odd case.".siZe;

