define class <car> (<object>)
  slot serial-number :: <integer> = unique-serial-number();
  slot model-name :: <string>,
    required-init-keyword: model:;
  slot has-sunroof? :: <boolean>,
    init-keyword: sunroof?:,
    init-value: #f;
end class <car>;

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

define method \+()
end;

define constant $symbol = #"hello";
define variable *vector* = #[3.5, 5]
define constant $list = #(1, 2);
define constant $pair = #(1 . "foo")
