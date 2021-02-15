# -*- ruby -*-

# Local variables:
#  indent-tabs-mode: nil
#  ruby-indent-level: 4
# End:

# @@PLEAC@@_NAME
# @@SKIP@@ Ruby

# @@PLEAC@@_WEB
# @@SKIP@@ http://www.ruby-lang.org


# @@PLEAC@@_1.0
string = '\n'                     # two characters, \ and an n
string = 'Jon \'Maddog\' Orwant'  # literal single quotes

string = "\n"                     # a "newline" character
string = "Jon \"Maddog\" Orwant"  # literal double quotes

string = %q/Jon 'Maddog' Orwant/  # literal single quotes

string = %q[Jon 'Maddog' Orwant]  # literal single quotes
string = %q{Jon 'Maddog' Orwant}  # literal single quotes
string = %q(Jon 'Maddog' Orwant)  # literal single quotes
string = %q<Jon 'Maddog' Orwant>  # literal single quotes

a = <<"EOF"
This is a multiline here document
terminated by EOF on a line by itself
EOF


# @@PLEAC@@_1.1
value = string[offset,count]
value = string[offset..-1]

string[offset,count] = newstring
string[offset..-1]   = newtail

# in Ruby we can also specify intervals by their two offsets
value = string[offset..offs2]
string[offset..offs2] = newstring

leading, s1, s2, trailing = data.unpack("A5 x3 A8 A8 A*")

fivers = string.unpack("A5" * (string.length/5))

chars = string.unpack("A1" * string.length)

string = "This is what you have"
#        +012345678901234567890  Indexing forwards  (left to right)
#         109876543210987654321- Indexing backwards (right to left)
#          note that 0 means 10 or 20, etc. above

first  = string[0, 1]       # "T"
start  = string[5, 2]       # "is"
rest   = string[13..-1]     # "you have"
last   = string[-1, 1]      # "e"
end_   = string[-4..-1]     # "have"
piece  = string[-8, 3]      # "you"

string[5, 2] = "wasn't"     # change "is" to "wasn't"
string[-12..-1] = "ondrous" # "This wasn't wondrous"
string[0, 1] = ""           # delete first character
string[-10..-1]  = ""       # delete last 10 characters

if string[-10..-1] =~ /pattern/
    puts "Pattern matches in last 10 characters"
end

string[0, 5].gsub!(/is/, 'at')

a = "make a hat"
a[0, 1], a[-1, 1] = a[-1, 1], a[0, 1]

a = "To be or not to be"
b = a.unpack("x6 A6")

b, c = a.unpack("x6 A2 X5 A2")
puts "#{b}\n#{c}\n"

def cut2fmt(*args)
    template = ''
    lastpos  = 1
    for place in args
        template += "A" + (place - lastpos).to_s + " "
        lastpos   = place
    end
    template += "A*"
    return template
end

fmt = cut2fmt(8, 14, 20, 26, 30)


# @@PLEAC@@_1.2
# careful! "b is true" doesn't mean "b != 0" (0 is true in Ruby)
# thus no problem of "defined" later since only nil is false
# the following sets to `c' if `b' is nil or false
a = b || c

# if you need Perl's behaviour (setting to `c' if `b' is 0) the most
# effective way is to use Numeric#nonzero? (thanks to Dave Thomas!)
a = b.nonzero? || c

# you will still want to use defined? in order to test
# for scope existence of a given object
a = defined?(b) ? b : c

dir = ARGV.shift || "/tmp"


# @@PLEAC@@_1.3
v1, v2 = v2, v1

alpha, beta, production = %w(January March August)
alpha, beta, production = beta, production, alpha


# @@PLEAC@@_1.4
num = char[0]
char = num.chr

# Ruby also supports having a char from character constant
num = ?r

char = sprintf("%c", num)
printf("Number %d is character %c\n", num, num)

ascii = string.unpack("C*")
string = ascii.pack("C*")

hal = "HAL"
ascii = hal.unpack("C*")
# We can't use Array#each since we can't mutate a Fixnum
ascii.collect! { |i|
    i + 1                         # add one to each ASCII value
}                
ibm = ascii.pack("C*")
puts ibm


# @@PLEAC@@_1.5
array = string.split('')

array = string.unpack("C*")

string.scan(/./) { |b|
    # do something with b
}

string = "an apple a day"
print "unique chars are: ", string.split('').uniq.sort, "\n"

sum = 0
for ascval in string.unpack("C*") # or use Array#each for a pure OO style :)
    sum += ascval
end
puts "sum is #{sum & 0xffffffff}" # since Ruby will go Bignum if necessary

# @@INCLUDE@@ include/ruby/slowcat.rb


# @@PLEAC@@_1.6
revbytes = string.reverse

revwords = string.split(" ").reverse.join(" ")

revwords = string.split(/(\s+)/).reverse.join

# using the fact that IO is Enumerable, you can directly "select" it
long_palindromes = File.open("/usr/share/dict/words").
    select { |w| w.chomp!; w.reverse == w && w.length > 5 }


# @@PLEAC@@_1.7
while string.sub!("\t+") { ' ' * ($&.length * 8 - $`.length % 8) }
end


# @@PLEAC@@_1.8
'You owe #{debt} to me'.gsub(/\#{(\w+)}/) { eval($1) }

rows, cols = 24, 80
text = %q(I am #{rows} high and #{cols} long)
text.gsub!(/\#{(\w+)}/) { eval("#{$1}") }
puts text

'I am 17 years old'.gsub(/\d+/) { 2 * $&.to_i }


# @@PLEAC@@_1.9
e = "bo peep".upcase
e.downcase!
e.capitalize!

"thIS is a loNG liNE".gsub!(/\w+/) { $&.capitalize }


# @@PLEAC@@_1.10
"I have #{n+1} guanacos."
print "I have ", n+1, " guanacos."


# @@PLEAC@@_1.11
var = <<'EOF'.gsub(/^\s+/, '')
    your text
    goes here
EOF


# @@PLEAC@@_1.12
string = "Folding and splicing is the work of an editor,\n"+
    "not a mere collection of silicon\n"+ 
    "and\n"+
    "mobile electrons!"

def wrap(str, max_size)
    all = []
    line = ''
    for l in str.split
        if (line+l).length >= max_size
            all.push(line)
            line = ''
        end
        line += line == '' ? l : ' ' + l
    end
    all.push(line).join("\n")
end

print wrap(string, 20)
#=> Folding and 
#=> splicing is the 
#=> work of an editor, 
#=> not a mere 
#=> collection of 
#=> silicon and mobile 
#=> electrons!


# @@PLEAC@@_1.13
string = %q(Mom said, "Don't do that.")
string.gsub(/['"]/) { '\\'+$& }
string.gsub(/['"]/, '\&\&')
string.gsub(/[^A-Z]/) { '\\'+$& }
"is a test!".gsub(/\W/) { '\\'+$& }  # no function like quotemeta?


# @@PLEAC@@_1.14
string.strip!


# @@PLEAC@@_1.15
def parse_csv(text)
    new = text.scan(/"([^\"\\]*(?:\\.[^\"\\]*)*)",?|([^,]+),?|,/)
    new << nil if text[-1] == ?,
    new.flatten.compact
end  

line = %q<XYZZY,"","O'Reilly, Inc","Wall, Larry","a \"glug\" bit,",5,"Error, Core Dumped">
fields = parse_csv(line)
fields.each_with_index { |v,i|
    print "#{i} : #{v}\n";
}


# @@PLEAC@@_1.16
# Use the soundex.rb Library from Michael Neumann.
# http://www.s-direktnet.de/homepages/neumann/rb_prgs/Soundex.rb
require 'Soundex'

code = Text::Soundex.soundex(string)
codes = Text::Soundex.soundex(array)

# substitution function for getpwent():
# returns an array of user entries,
# each entry contains the username and the full name
def login_names
    result = []
    File.open("/etc/passwd") { |file|
        file.each_line { |line|
            next if line.match(/^#/)
            cols = line.split(":")
            result.push([cols[0], cols[4]])
        }
    }
    result
end

puts "Lookup user: "
user = STDIN.gets
user.chomp!
exit unless user
name_code = Text::Soundex.soundex(user)

splitter = Regexp.new('(\w+)[^,]*\b(\w+)')
for username, fullname in login_names do
    firstname, lastname = splitter.match(fullname)[1,2]
    if name_code == Text::Soundex.soundex(username)
        || name_code == Text::Soundex.soundex(firstname)
        || name_code == Text::Soundex.soundex(lastname)
    then
        puts "#{username}: #{firstname} #{lastname}"
    end
end


# @@PLEAC@@_1.17
# @@INCLUDE@@ include/ruby/fixstyle.rb


# @@PLEAC@@_1.18
# @@INCLUDE@@ include/ruby/psgrep.rb


# @@PLEAC@@_2.1
# Matz tells that you can use Integer() for strict checked conversion.
Integer("abc")
#=> `Integer': invalid value for Integer: "abc" (ArgumentError)
Integer("567")
#=> 567

# You may use Float() for floating point stuff
Integer("56.7")
#=> `Integer': invalid value for Integer: "56.7" (ArgumentError)
Float("56.7")
#=> 56.7

# You may also use a regexp for that
if string =~ /^[+-]?\d+$/
    p 'is an integer'
else
    p 'is not'
end

if string =~ /^-?(?:\d+(?:\.\d*)?|\.\d+)$/
    p 'is a decimal number'
else
    p 'is not'
end


# @@PLEAC@@_2.2
# equal(num1, num2, accuracy) : returns true if num1 and num2 are
#   equal to accuracy number of decimal places
def equal(i, j, a)
    sprintf("%.#{a}g", i) == sprintf("%.#{a}g", j)
end

wage = 536                        # $5.36/hour
week = 40 * wage                  # $214.40
printf("One week's wage is: \$%.2f\n", week/100.0)


# @@PLEAC@@_2.3
num.round                         # rounds to integer

a = 0.255
b = sprintf("%.2f", a)
print  "Unrounded: #{a}\nRounded: #{b}\n"
printf "Unrounded: #{a}\nRounded: %.2f\n", a

print "number\tint\tfloor\tceil\n"
a = [ 3.3 , 3.5 , 3.7, -3.3 ]
for n in a
    printf("% .1f\t% .1f\t% .1f\t% .1f\n",  # at least I don't fake my output :)
           n, n.to_i, n.floor, n.ceil)
end


# @@PLEAC@@_2.4
def dec2bin(n)
    [n].pack("N").unpack("B32")[0].sub(/^0+(?=\d)/, '')
end

def bin2dec(n)
    [("0"*32+n.to_s)[-32..-1]].pack("B32").unpack("N")[0]
end


# @@PLEAC@@_2.5
for i in x .. y
    # i is set to every integer from x to y, inclusive
end

x.step(y,7) { |i|
    # i is set to every integer from x to y, stepsize = 7
}

print "Infancy is: "
(0..2).each { |i|
    print i, " "
}
print "\n"


# @@PLEAC@@_2.6
# We can add conversion methods to the Integer class,
# this makes a roman number just a representation for normal numbers.
class Integer
    
    @@romanlist = [["M", 1000],
                   ["CM", 900],
                   ["D",  500],
                   ["CD", 400],
                   ["C",  100],
                   ["XC",  90],
                   ["L",   50],
                   ["XL",  40],
                   ["X",   10],
                   ["IX",   9],
                   ["V",    5],
                   ["IV",   4],
                   ["I",    1]]
    
    def to_roman
        remains = self
        roman = ""
        for sym, num in @@romanlist
            while remains >= num
                remains -= num
                roman << sym
            end
        end
        roman
    end
    
    def Integer.from_roman(roman)
        ustr = roman.upcase
        sum = 0
        for entry in @@romanlist
            sym, num = entry[0], entry[1]
            while sym == ustr[0, sym.length]
                sum += num
                ustr.slice!(0, sym.length)
            end
        end
        sum
    end
    
end


roman_fifteen = 15.to_roman
puts "Roman for fifteen is #{roman_fifteen}"
i = Integer.from_roman(roman_fifteen)
puts "Converted back, #{roman_fifteen} is #{i}"

# check
for i in (1..3900)
    r = i.to_roman
    j = Integer.from_roman(r)
    if i != j
        puts "error: #{i} : #{r} - #{j}"
    end
end


# @@PLEAC@@_2.7
random = rand(y-x+1)+x

chars = ["A".."Z","a".."z","0".."9"].collect { |r| r.to_a }.join + %q(!@$%^&*)
password = (1..8).collect { chars[rand(chars.size)] }.pack("C*")


# @@PLEAC@@_2.8
srand        # uses a combination of the time, the process id, and a sequence number
srand(val)   # for repeatable behaviour


# @@PLEAC@@_2.9
# from the randomr lib: 
# http://raa.ruby-lang.org/project/randomr/
----> http://raa.ruby-lang.org/project/randomr/

require 'random/mersenne_twister'
mers = Random::MersenneTwister.new 123456789
puts mers.rand(0)    # 0.550321932544541
puts mers.rand(10)   # 2

# using online sources of random data via the realrand package:
# http://raa.ruby-lang.org/project/realrand/
# **Note**
# The following online services are used in this package:
#   http://www.random.org - source: atmospheric noise 
#   http://www.fourmilab.ch/hotbits - source: radioactive decay timings
#   http://random.hd.org - source: entropy from local and network noise
# Please visit the sites and respect the rules of each service.

require 'random/online'

generator1 = Random::RandomOrg.new
puts generator1.randbyte(5).join(",")
puts generator1.randnum(10, 1, 6).join(",")  # Roll dice 10 times.

generator2 = Random::FourmiLab.new
puts generator2.randbyte(5).join(",")
# randnum is not supported.

generator3 = Random::EntropyPool.new
puts generator3.randbyte(5).join(",")
# randnum is not supported.


# @@PLEAC@@_2.10
def gaussian_rand
    begin
        u1 = 2 * rand() - 1
        u2 = 2 * rand() - 1
        w = u1*u1 + u2*u2
    end while (w >= 1)
    w = Math.sqrt((-2*Math.log(w))/w)
    [ u2*w, u1*w ]
end

mean = 25
sdev = 2
salary = gaussian_rand[0] * sdev + mean
printf("You have been hired at \$%.2f\n", salary)


# @@PLEAC@@_2.11
def deg2rad(d)
    (d/180.0)*Math::PI
end

def rad2deg(r)
    (r/Math::PI)*180
end


# @@PLEAC@@_2.12
sin_val = Math.sin(angle)
cos_val = Math.cos(angle)
tan_val = Math.tan(angle)

# AFAIK Ruby's Math module doesn't provide acos/asin
# While we're at it, let's also define missing hyperbolic functions
module Math
    def Math.asin(x)
        atan2(x, sqrt(1 - x**2))
    end
    def Math.acos(x)
        atan2(sqrt(1 - x**2), x)
    end
    def Math.atan(x)
        atan2(x, 1)
    end
    def Math.sinh(x)
        (exp(x) - exp(-x)) / 2
    end
    def Math.cosh(x)
        (exp(x) + exp(-x)) / 2
    end
    def Math.tanh(x)
        sinh(x) / cosh(x)
    end
end

# The support for Complex numbers is not built-in
y = Math.acos(3.7)
#=> in `sqrt': square root for negative number (ArgumentError)

# There is an implementation of Complex numbers in 'complex.rb' in current
# Ruby distro, but it doesn't support atan2 with complex args, so it doesn't
# solve this problem.


# @@PLEAC@@_2.13
log_e = Math.log(val)
log_10 = Math.log10(val)

def log_base(base, val)
    Math.log(val)/Math.log(base)
end

answer = log_base(10, 10_000)
puts "log10(10,000) = #{answer}"


# @@PLEAC@@_2.14
require 'matrix.rb'

a = Matrix[[3, 2, 3], [5, 9, 8]]
b = Matrix[[4, 7], [9, 3], [8, 1]]
c = a * b

a.row_size
a.column_size

c.det
a.transpose


# @@PLEAC@@_2.15
require 'complex.rb'
require 'rational.rb'

a = Complex(3, 5)              # 3 + 5i
b = Complex(2, -2)             # 2 - 2i
puts "c = #{a*b}"

c = a * b
d = 3 + 4*Complex::I

printf "sqrt(#{d}) = %s\n", Math.sqrt(d)


# @@PLEAC@@_2.16
number = hexadecimal.hex
number = octal.oct

print "Gimme a number in decimal, octal, or hex: "
num = gets.chomp
exit unless defined?(num)
num = num.oct if num =~ /^0/  # does both oct and hex  
printf "%d %x %o\n", num, num, num

print "Enter file permission in octal: "
permissions = gets.chomp
raise "Exiting ...\n" unless defined?(permissions)
puts "The decimal value is #{permissions.oct}"


# @@PLEAC@@_2.17
def commify(n)
    n.to_s =~ /([^\.]*)(\..*)?/
    int, dec = $1.reverse, $2 ? $2 : ""
    while int.gsub!(/(,|\.|^)(\d{3})(\d)/, '\1\2,\3')
    end
    int.reverse + dec
end


# @@PLEAC@@_2.18
printf "It took %d hour%s\n", time, time == 1 ? "" : "s"

# dunno if an equivalent to Lingua::EN::Inflect exists...


# @@PLEAC@@_2.19
#-----------------------------
#!/usr/bin/ruby
# bigfact - calculating prime factors
def factorize(orig)
    factors = {}
    factors.default = 0     # return 0 instead nil if key not found in hash
    n = orig
    i = 2
    sqi = 4                 # square of i
    while sqi <= n do
        while n.modulo(i) == 0 do
            n /= i
            factors[i] += 1
            # puts "Found factor #{i}"
        end
        # we take advantage of the fact that (i +1)**2 = i**2 + 2*i +1
        sqi += 2 * i + 1
        i += 1
    end
    
    if (n != 1) && (n != orig)
        factors[n] += 1
    end
    factors
end

def printfactorhash(orig, factorcount)
    print format("%-10d ", orig)
    if factorcount.length == 0
        print "PRIME"
    else
        # sorts after number, because the hash keys are numbers
        factorcount.sort.each { |factor,exponent|
            print factor
            if exponent > 1
                print "**", exponent
            end
            print " "
        }
    end
    puts
end

for arg in ARGV
    n = arg.to_i
    mfactors = factorize(n)
    printfactorhash(n, mfactors)
end
#-----------------------------


# @@PLEAC@@_3.0
puts Time.now

print "Today is day ", Time.now.yday, " of the current year.\n"
print "Today is day ", Time.now.day, " of the current month.\n"


# @@PLEAC@@_3.1
day, month, year = Time.now.day, Time.now.month, Time.now.year
# or
day, month, year = Time.now.to_a[3..5]

tl = Time.now.localtime
printf("The current date is %04d %02d %02d\n", tl.year, tl.month, tl.day)

Time.now.localtime.strftime("%Y-%m-%d")


# @@PLEAC@@_3.2
Time.local(year, month, day, hour, minute, second).tv_sec
Time.gm(year, month, day, hour, minute, second).tv_sec


# @@PLEAC@@_3.3
sec, min, hour, day, month, year, wday, yday, isdst, zone = Time.at(epoch_secs).to_a


# @@PLEAC@@_3.4
when_ = now + difference         # now -> Time ; difference -> Numeric (delta in seconds)
then_ = now - difference


# @@PLEAC@@_3.5
bree = 361535725
nat  =  96201950

difference = bree - nat
puts "There were #{difference} seconds between Nat and Bree"

seconds    =  difference % 60
difference = (difference - seconds) / 60
minutes    =  difference % 60
difference = (difference - minutes) / 60
hours      =  difference % 24
difference = (difference - hours)   / 24
days       =  difference % 7
weeks      = (difference - days)    /  7

puts "(#{weeks} weeks, #{days} days, #{hours}:#{minutes}:#{seconds})"


# @@PLEAC@@_3.6
monthday, weekday, yearday = date.mday, date.wday, date.yday

# AFAIK the week number is not just a division since week boundaries are on sundays
weeknum = d.strftime("%U").to_i + 1

year  = 1981
month = "jun"                     # or `6' if you want to emulate a broken language
day   = 16
t = Time.mktime(year, month, day)
print "#{month}/#{day}/#{year} was a ", t.strftime("%A"), "\n"


# @@PLEAC@@_3.7
yyyy, mm, dd = $1, $2, $3 if "1998-06-25" =~ /(\d+)-(\d+)-(\d+)/

epoch_seconds = Time.mktime(yyyy, mm, dd).tv_sec

# dunno an equivalent to Date::Manip#ParseDate


# @@PLEAC@@_3.8
string = Time.at(epoch_secs)
Time.at(1234567890).gmtime        # gives: Fri Feb 13 23:31:30 UTC 2009

time = Time.mktime(1973, "jan", 18, 3, 45, 50)
print "In localtime it gives: ", time.localtime, "\n"


# @@PLEAC@@_3.9
# Ruby provides micro-seconds in Time object
Time.now.usec

# Ruby gives the seconds in floating format when substracting two Time objects
before = Time.now
line = gets
elapsed = Time.now - before
puts "You took #{elapsed} seconds."

# On my Celeron-400 with Linux-2.2.19-14mdk, average for three execs are:
#   This Ruby version:       average 0.00321 sec
#   Cookbook's Perl version: average 0.00981 sec
size = 500
number_of_times = 100
total_time = 0
number_of_times.times {
    # populate array
    array = []
    size.times { array << rand }
    # sort it
    begin_ = Time.now
    array.sort!
    time = Time.now - begin_
    total_time += time
}
printf "On average, sorting %d random numbers takes %.5f seconds\n",
    size, (total_time/Float(number_of_times))


# @@PLEAC@@_3.10
sleep(0.005)                      # Ruby is definitely not as broken as Perl :)
# (may be interrupted by sending the process a SIGALRM)


# @@PLEAC@@_3.11
#!/usr/bin/ruby -w
# hopdelta - feed mail header, produce lines
#            showing delay at each hop.
require 'time'
class MailHopDelta

    def initialize(mail)
        @head = mail.gsub(/\n\s+/,' ')
        @topline = %w-Sender Recipient Time Delta-
        @start_from = mail.match(/^From.*\@([^\s>]*)/)[1]
        @date = Time.parse(mail.match(/^Date:\s+(.*)/)[1])
    end

    def out(line)
         "%-20.20s %-20.20s %-20.20s  %s" % line
    end

    def hop_date(day)
        day.strftime("%I:%M:%S %Y/%m/%d")
    end

    def puts_hops
        puts out(@topline) 
        puts out(['Start', @start_from, hop_date(@date),''])
        @head.split(/\n/).reverse.grep(/^Received:/).each do |hop|
            hop.gsub!(/\bon (.*?) (id.*)/,'; \1')
            whence = hop.match(/;\s+(.*)$/)[1]
            unless whence
                warn "Bad received line: #{hop}"
                next
            end
            from = $+ if hop =~ /from\s+(\S+)|\((.*?)\)/
            by   = $1 if hop =~ /by\s+(\S+\.\S+)/
            next unless now = Time.parse(whence).localtime
            delta = now - @date
            puts out([from, by, hop_date(now), hop_time(delta)])
            @date = now
        end
    end

    def hop_time(secs)
        sign = secs < 0 ? -1 : 1
        days, secs = secs.abs.divmod(60 * 60 * 24)
        hours,secs = secs.abs.divmod(60 * 60)
        mins, secs = secs.abs.divmod(60)
        rtn =  "%3ds" % [secs  * sign]
        rtn << "%3dm" % [mins  * sign] if mins  != 0
        rtn << "%3dh" % [hours * sign] if hours != 0
        rtn << "%3dd" % [days  * sign] if days  != 0 
        rtn
    end
end

$/ = ""
mail = MailHopDelta.new(ARGF.gets).puts_hops


# @@PLEAC@@_4.0
single_level = [ "this", "that", "the", "other" ]

# Ruby directly supports nested arrays
double_level = [ "this", "that", [ "the", "other" ] ]
still_single_level = [ "this", "that", [ "the", "other" ] ].flatten


# @@PLEAC@@_4.1
a = [ "quick", "brown", "fox" ]
a = %w(Why are you teasing me?)

lines = <<"END_OF_HERE_DOC".gsub(/^\s*(.+)/, '\1')
    The boy stood on the burning deck,
    It was as hot as glass.
END_OF_HERE_DOC

bigarray = IO.readlines("mydatafile").collect { |l| l.chomp }

name = "Gandalf"
banner = %Q(Speak, #{name}, and welcome!)

host_info  = `host #{his_host}`

%x(ps #{$$})

banner = 'Costs only $4.95'.split(' ')

rax = %w! ( ) < > { } [ ] !


# @@PLEAC@@_4.2
def commify_series(arr)
    return '' if not arr
    case arr.size
        when 0 then ''
        when 1 then arr[0]
        when 2 then arr.join(' and ')
        else arr[0..-2].join(', ') + ', and ' + arr[-1]
    end
end

array = [ "red", "yellow", "green" ]

print "I have ", array, " marbles\n"
# -> I have redyellowgreen marbles

# But unlike Perl:
print "I have #{array} marbles\n"
# -> I have redyellowgreen marbles
# So, needs:
print "I have #{array.join(' ')} marbles\n"
# -> I have red yellow green marbles

#!/usr/bin/ruby
# communify_series - show proper comma insertion in list output

def commify_series(arr)
    return '' if not arr
    sepchar = arr.find { |p| p =~ /,/ } ? '; ' : ', '
    case arr.size
        when 0 then ''
        when 1 then arr[0]
        when 2 then arr.join(' and ')
        else arr[0..-2].join(sepchar) + sepchar + 'and ' + arr[-1]
    end
end

lists = [
    [ 'just one thing' ],
    %w(Mutt Jeff),
    %w(Peter Paul Mary),
    [ 'To our parents', 'Mother Theresa', 'God' ],
    [ 'pastrami', 'ham and cheese', 'peanut butter and jelly', 'tuna' ],
    [ 'recycle tired, old phrases', 'ponder big, happy thoughts' ],
    [ 'recycle tired, old phrases',
      'ponder big, happy thoughts',
      'sleep and dream peacefully' ],
]

for list in lists do
    puts "The list is: #{commify_series(list)}."
end


# @@PLEAC@@_4.3
#   (note: AFAIK Ruby doesn't allow gory change of Array length)
# grow the array by assigning nil to past the end of array
ary[new_size-1] = nil
# shrink the array by slicing it down
ary.slice!(new_size..-1)
# init the array with given size
Array.new(number_of_elems)
# assign to an element past the original end enlarges the array
ary[index_new_last_elem] = value

def what_about_that_array(a)
    print "The array now has ", a.size, " elements.\n"
    # Index of last element is not really interesting in Ruby
    print "Element #3 is `#{a[3]}'.\n"
end
people = %w(Crosby Stills Nash Young)
what_about_that_array(people)


# @@PLEAC@@_4.4
# OO style
bad_users.each { |user|
    complain(user)
}
# or, functional style
for user in bad_users
    complain(user)
end

for var in ENV.keys.sort
    puts "#{var}=#{ENV[var]}"
end

for user in all_users
    disk_space = get_usage(user)
    if (disk_space > MAX_QUOTA)
        complain(user)
    end
end

for l in IO.popen("who").readlines
    print l if l =~ /^gc/ 
end

# we can mimic the obfuscated Perl way
while fh.gets               # $_ is set to the line just read
    chomp                   # $_ has a trailing \n removed, if it had one
    split.each { |w|        # $_ is split on whitespace
                            # but $_ is not set to each chunk as in Perl
        print w.reverse
    }
end
# ...or use a cleaner way
for l in fh.readlines
    l.chomp.split.each { |w| print w.reverse }
end

# same drawback as in problem 1.4, we can't mutate a Numeric...
array.collect! { |v| v - 1 }

a = [ .5, 3 ]; b = [ 0, 1 ]
for ary in [ a, b ]
    ary.collect! { |v| v * 7 }
end
puts "#{a.join(' ')} #{b.join(' ')}"

# we can mutate Strings, cool; we need a trick for the scalar
for ary in [ [ scalar ], array, hash.values ]
    ary.each { |v| v.strip! }     # String#strip rules :)
end


# @@PLEAC@@_4.5
# not relevant in Ruby since we have always references
for item in array
    # do somethingh with item
end


# @@PLEAC@@_4.6
unique = list.uniq

# generate a list of users logged in, removing duplicates
users = `who`.collect { |l| l =~ /(\w+)/; $1 }.sort.uniq
puts("users logged in: #{commify_series(users)}")  # see 4.2 for commify_series


# @@PLEAC@@_4.7
a - b
# [ 1, 1, 2, 2, 3, 3, 3, 4, 5 ] - [ 1, 2, 4 ]  ->  [3, 5]


# @@PLEAC@@_4.8
union = a | b
intersection = a & b
difference = a - b


# @@PLEAC@@_4.9
array1.concat(array2)
# if you will assign to another object, better use:
new_ary = array1 + array2

members = [ "Time", "Flies" ]
initiates =  [ "An", "Arrow" ]
members += initiates

members = [ "Time", "Flies" ]
initiates = [ "An", "Arrow" ]
members[2,0] = [ "Like", initiates ].flatten

members[0] = "Fruit"
members[3,2] = "A", "Banana"


# @@PLEAC@@_4.10
reversed = ary.reverse

ary.reverse_each { |e|
    # do something with e
}

descending = ary.sort.reverse
descending = ary.sort { |a,b| b <=> a }


# @@PLEAC@@_4.11
# remove n elements from front of ary (shift n)
front = ary.slice!(0, n)

# remove n elements from the end of ary (pop n)
end_ = ary.slice!(-n .. -1)

# let's extend the Array class, to make that useful
class Array
    def shift2()
        slice!(0 .. 1)     # more symetric with pop2...
    end
    def pop2()
        slice!(-2 .. -1)
    end
end

friends = %w(Peter Paul Mary Jim Tim)
this, that = friends.shift2

beverages = %w(Dew Jolt Cola Sprite Fresca)
pair = beverages.pop2


# @@PLEAC@@_4.12
# use Enumerable#detect (or the synonym Enumerable#find)
highest_eng = employees.detect { |emp| emp.category == 'engineer' }


# @@PLEAC@@_4.13
# use Enumerable#select (or the synonym Enumerable#find_all)
bigs = nums.select { |i| i > 1_000_000 }
pigs = users.keys.select { |k| users[k] > 1e7 }

matching = `who`.select { |u| u =~ /^gnat / }

engineers = employees.select { |e| e.position == 'Engineer' }

secondary_assistance = applicants.select { |a|
    a.income >= 26_000 && a.income < 30_000
}


# @@PLEAC@@_4.14
# normally you would have an array of Numeric (Float or
# Fixnum or Bignum), so you would use:
sorted = unsorted.sort
# if you have strings representing Integers or Floats
# you may specify another sort method:
sorted = unsorted.sort { |a,b| a.to_f <=> b.to_f }

# let's use the list of my own PID's
`ps ux`.split("\n")[1..-1].
    select { |i| i =~ /^#{ENV['USER']}/ }.
    collect { |i| i.split[1] }.
    sort { |a,b| a.to_i <=> b.to_i }.each { |i| puts i }
puts "Select a process ID to kill:"
pid = gets.chomp
raise "Exiting ... \n" unless pid && pid =~ /^\d+$/
Process.kill('TERM', pid.to_i)
sleep 2
Process.kill('KILL', pid.to_i)

descending = unsorted.sort { |a,b| b.to_f <=> a.to_f }


# @@PLEAC@@_4.15
ordered = unordered.sort { |a,b| compare(a,b) }

precomputed = unordered.collect { |e| [compute, e] }
ordered_precomputed = precomputed.sort { |a,b| a[0] <=> b[0] }
ordered = ordered_precomputed.collect { |e| e[1] }

ordered = unordered.collect { |e| [compute, e] }.
    sort { |a,b| a[0] <=> b[0] }.
    collect { |e| e[1] }

for employee in employees.sort { |a,b| a.name <=> b.name }
    print employee.name, " earns \$ ", employee.salary, "\n"
end

# Beware! `0' is true in Ruby.
# For chaining comparisons, you may use Numeric#nonzero?, which
# returns num if num is not zero, nil otherwise
sorted = employees.sort { |a,b| (a.name <=> b.name).nonzero? || b.age <=> a.age }

users = []
# getpwent is not wrapped in Ruby... let's fallback
IO.readlines('/etc/passwd').each { |u| users << u.split(':') }
users.sort! { |a,b| a[0] <=> b[0] }
for user in users
    puts user[0]
end

sorted = names.sort { |a,b| a[1, 1] <=> b[1, 1] }
sorted = strings.sort { |a,b| a.length <=> b.length }

# let's show only the compact version
ordered = strings.collect { |e| [e.length, e] }.
    sort { |a,b| a[0] <=> b[0] }.
    collect { |e| e[1] }

ordered = strings.collect { |e| [/\d+/.match(e)[0].to_i, e] }.
    sort { |a,b| a[0] <=> b[0] }.
    collect { |e| e[1] }

print `cat /etc/passwd`.collect { |e| [e, e.split(':').indexes(3,2,0)].flatten }.
    sort { |a,b| (a[1] <=> b[1]).nonzero? || (a[2] <=> b[2]).nonzero? || a[3] <=> b[3] }.
    collect { |e| e[0] }


# @@PLEAC@@_4.16
circular.unshift(circular.pop)        # the last shall be first
circular.push(circular.shift)         # and vice versa

def grab_and_rotate(l)
    l.push(ret = l.shift)
    ret
end

processes = [1, 2, 3, 4, 5]
while (1)
    process = grab_and_rotate(processes)
    puts "Handling process #{process}"
    sleep 1
end


# @@PLEAC@@_4.17
def fisher_yates_shuffle(a)
    (a.size-1).downto(1) { |i|
        j = rand(i+1)
        a[i], a[j] = a[j], a[i] if i != j
    }
end

def naive_shuffle(a)
    for i in 0...a.size
        j = rand(a.size)
        a[i], a[j] = a[j], a[i]
    end
end


