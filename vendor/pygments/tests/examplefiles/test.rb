a.each{|el|anz[el]=anz[el]?anz[el]+1:1}
while x<10000
#a bis f dienen dazu die Nachbarschaft festzulegen. Man stelle sich die #Zahl von 1 bis 64 im Binärcode vor 1 bedeutet an 0 aus
  b=(p[x]%32)/16<1 ? 0 : 1

  (x-102>=0? n[x-102].to_i : 0)*a+(x-101>=0?n[x-101].to_i : 0)*e+n[x-100].to_i+(x-99>=0? n[x-99].to_i : 0)*f+(x-98>=0? n[x-98].to_i : 0)*a+
  n[x+199].to_i*b+n[x+200].to_i*d+n[x+201].to_i*b

#und die Ausgabe folgt
g=%w{}
x=0

#leere regex
test //, 123

while x<100
 puts"#{g[x]}"
 x+=1
end

puts""
sleep(10)

1E1E1
puts 30.send(:/, 5) # prints 6

# fun with class attributes
class Foo
  def self.blub x
    if not x.nil?
      self.new
    end
  end
  def another_way_to_get_class
    self.class
  end
end

# ruby 1.9 "call operator"
a = Proc.new { 42 }
a.()

"instance variables can be #@included, #@@class_variables\n and #$globals as well."
`instance variables can be #@included, #@@class_variables\n and #$globals as well.`
'instance variables can be #@included, #@@class_variables\n and #$globals as well.'
/instance variables can be #@included, #@@class_variables\n and #$globals as well./mousenix
:"instance variables can be #@included, #@@class_variables\n and #$globals as well."
:'instance variables can be #@included, #@@class_variables\n and #$globals as well.'
%'instance variables can be #@included, #@@class_variables\n and #$globals as well.'
%q'instance variables can be #@included, #@@class_variables\n and #$globals as well.'
%Q'instance variables can be #@included, #@@class_variables\n and #$globals as well.'
%w'instance variables can be #@included, #@@class_variables\n and #$globals as well.'
%W'instance variables can be #@included, #@@class_variables\n and #$globals as well.'
%s'instance variables can be #@included, #@@class_variables\n and #$globals as well.'
%r'instance variables can be #@included, #@@class_variables\n and #$globals as well.'
%x'instance variables can be #@included, #@@class_variables\n and #$globals as well.'

#%W[ but #@0illegal_values look strange.]

%s#ruby allows strange#{constructs}
%s#ruby allows strange#$constructs
%s#ruby allows strange#@@constructs

##################################################################
# HEREDOCS
foo(<<-A, <<-B)
this is the text of a
A
and this is the text of b
B

a = <<"EOF"
This is a multiline #$here document
terminated by EOF on a line by itself
EOF

a = <<'EOF'
This is a multiline #$here document
terminated by EOF on a line by itself
EOF

b=(p[x] %32)/16<1 ? 0 : 1

<<""
#{test}
#@bla
#die suppe!!!
\xfffff


super <<-EOE % [
    foo
EOE

<<X
X
X

%s(uninter\)pre\ted)            # comment here
%q(uninter\)pre\ted)            # comment here
%Q(inter\)pre\ted)              # comment here
:"inter\)pre\ted"               # comment here
:'uninter\'pre\ted'             # comment here

%q[haha! [nesting [rocks] ! ] ] # commeht here


##################################################################
class                                                  NP
def  initialize a=@p=[], b=@b=[];                      end
def +@;@b<<1;b2c end;def-@;@b<<0;b2c                   end
def  b2c;if @b.size==8;c=0;@b.each{|b|c<<=1;c|=b};send(
     'lave'.reverse,(@p.join))if c==0;@p<< c.chr;@b=[] end
     self end end ; begin _ = NP.new                   end


# Regexes
/
this is a
mutliline
regex
/

this /is a
multiline regex too/

also /4
is one/

this(/
too
/)

# this not
2 /4
asfsadf/


#from: http://coderay.rubychan.de/rays/show/383
class Object
  alias  :xeq :`
  def `(cmd, p2)
    self.method(cmd.to_sym).call(p2)
  end
end
p [1,2,3].`('concat', [4,5,6]) # => [1, 2, 3, 4, 5, 6]
p [1,2,3].`(:concat, [4,5,6]) # => [1, 2, 3, 4, 5, 6]
p "Hurra! ".`(:*, 3) # => "Hurra! Hurra! Hurra! "
p "Hurra! ".`('*', 3) # => "Hurra! Hurra! Hurra! "
# Leider geht nicht die Wunschform
# [1,2,3] `concat` [4,5,6]

class Object
  @@infixops = []
  alias :xeq :`
  def addinfix(operator)
    @@infixops << operator
  end
  def `(expression)
    @@infixops.each{|op|break if expression.match(/^(.*?) (#{op}) (.*)$/)}
    raise "unknown infix operator in expression: #{expression}" if $2 == nil
    eval($1).method($2.to_sym).call(eval($3))
  end
end
addinfix("concat")
p `[1,2,3] concat [4,5,6]` # => [1, 2, 3, 4, 5, 6]


# HEREDOC FUN!!!!!!!1111
foo(<<A, <<-B, <<C)
this is the text of a
   A!!!!
A
and this is text of B!!!!!!111
   B
and here some C
C
