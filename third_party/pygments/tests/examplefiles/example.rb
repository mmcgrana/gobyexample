module CodeRay
	module Scanners

class Ruby < Scanner

	RESERVED_WORDS = [
		'and', 'def', 'end', 'in', 'or', 'unless', 'begin',
		'defined?', 'ensure', 'module', 'redo', 'super', 'until',
		'BEGIN', 'break', 'do', 'next', 'rescue', 'then',
		'when', 'END', 'case', 'else', 'for', 'retry',
		'while', 'alias', 'class', 'elsif', 'if', 'not', 'return',
		'undef', 'yield',
	]

	DEF_KEYWORDS = ['def']
	MODULE_KEYWORDS = ['class', 'module']
	DEF_NEW_STATE = WordList.new(:initial).
		add(DEF_KEYWORDS, :def_expected).
		add(MODULE_KEYWORDS, :module_expected)

	WORDS_ALLOWING_REGEXP = [
		'and', 'or', 'not', 'while', 'until', 'unless', 'if', 'elsif', 'when'
	]
	REGEXP_ALLOWED = WordList.new(false).
		add(WORDS_ALLOWING_REGEXP, :set)

	PREDEFINED_CONSTANTS = [
		'nil', 'true', 'false', 'self',
		'DATA', 'ARGV', 'ARGF', '__FILE__', '__LINE__',
	]

	IDENT_KIND = WordList.new(:ident).
		add(RESERVED_WORDS, :reserved).
		add(PREDEFINED_CONSTANTS, :pre_constant)

	METHOD_NAME = / #{IDENT} [?!]? /xo
	METHOD_NAME_EX = /
	 #{METHOD_NAME}  # common methods: split, foo=, empty?, gsub!
	 | \*\*?         # multiplication and power
	 | [-+~]@?       # plus, minus
	 | [\/%&|^`]     # division, modulo or format strings, &and, |or, ^xor, `system`
	 | \[\]=?        # array getter and setter
	 | <=?>? | >=?   # comparison, rocket operator
	 | << | >>       # append or shift left, shift right
	 | ===?          # simple equality and case equality
	/ox
	GLOBAL_VARIABLE = / \$ (?: #{IDENT} | \d+ | [~&+`'=\/,;_.<>!@0$?*":F\\] | -[a-zA-Z_0-9] ) /ox

	DOUBLEQ = / "  [^"\#\\]*  (?: (?: \#\{.*?\} | \#(?:$")?  | \\. ) [^"\#\\]*  )* "?  /ox
	SINGLEQ = / '  [^'\\]*    (?:                              \\.   [^'\\]*    )* '?  /ox
	STRING  = / #{SINGLEQ} | #{DOUBLEQ} /ox
	SHELL   = / `  [^`\#\\]*  (?: (?: \#\{.*?\} | \#(?:$`)?  | \\. ) [^`\#\\]*  )* `?  /ox
	REGEXP  = / \/ [^\/\#\\]* (?: (?: \#\{.*?\} | \#(?:$\/)? | \\. ) [^\/\#\\]* )* \/? /ox

	DECIMAL = /\d+(?:_\d+)*/  # doesn't recognize 09 as octal error
	OCTAL = /0_?[0-7]+(?:_[0-7]+)*/
	HEXADECIMAL = /0x[0-9A-Fa-f]+(?:_[0-9A-Fa-f]+)*/
	BINARY = /0b[01]+(?:_[01]+)*/

	EXPONENT = / [eE] [+-]? #{DECIMAL} /ox
	FLOAT = / #{DECIMAL} (?: #{EXPONENT} | \. #{DECIMAL} #{EXPONENT}? ) /
	INTEGER = /#{OCTAL}|#{HEXADECIMAL}|#{BINARY}|#{DECIMAL}/

	def reset
		super
		@regexp_allowed = false
	end

	def next_token
		return if @scanner.eos?

		kind = :error
		if @scanner.scan(/\s+/)  # in every state
			kind = :space
			@regexp_allowed = :set if @regexp_allowed or @scanner.matched.index(?\n)  # delayed flag setting

		elsif @state == :def_expected
			if @scanner.scan(/ (?: (?:#{IDENT}(?:\.|::))* | (?:@@?|$)? #{IDENT}(?:\.|::) ) #{METHOD_NAME_EX} /ox)
				kind = :method
				@state = :initial
			else
				@scanner.getch
			end
			@state = :initial

		elsif @state == :module_expected
			if @scanner.scan(/<</)
				kind = :operator
			else
				if @scanner.scan(/ (?: #{IDENT} (?:\.|::))* #{IDENT} /ox)
					kind = :method
				else
					@scanner.getch
				end
				@state = :initial
			end

		elsif # state == :initial
			# IDENTIFIERS, KEYWORDS
			if @scanner.scan(GLOBAL_VARIABLE)
				kind = :global_variable
			elsif @scanner.scan(/ @@ #{IDENT} /ox)
				kind = :class_variable
			elsif @scanner.scan(/ @ #{IDENT} /ox)
				kind = :instance_variable
			elsif @scanner.scan(/ __END__\n ( (?!\#CODE\#) .* )? | \#[^\n]* | =begin(?=\s).*? \n=end(?=\s|\z)(?:[^\n]*)? /mx)
				kind = :comment
			elsif @scanner.scan(METHOD_NAME)
				if @last_token_dot
					kind = :ident
				else
					matched = @scanner.matched
					kind = IDENT_KIND[matched]
					if kind == :ident and matched =~ /^[A-Z]/
						kind = :constant
					elsif kind == :reserved
						@state = DEF_NEW_STATE[matched]
						@regexp_allowed = REGEXP_ALLOWED[matched]
					end
				end

			elsif @scanner.scan(STRING)
				kind = :string
			elsif @scanner.scan(SHELL)
				kind = :shell
			elsif @scanner.scan(/<<
				(?:
					([a-zA-Z_0-9]+)
						(?: .*? ^\1$ | .* )
				|
					-([a-zA-Z_0-9]+)
						(?: .*? ^\s*\2$ | .* )
				|
					(["\'`]) (.+?) \3
						(?: .*? ^\4$ | .* )
				|
					- (["\'`]) (.+?) \5
						(?: .*? ^\s*\6$ | .* )
				)
			/mxo)
				kind = :string
			elsif @scanner.scan(/\//) and @regexp_allowed
				@scanner.unscan
				@scanner.scan(REGEXP)
				kind = :regexp
/%(?:[Qqxrw](?:\([^)#\\\\]*(?:(?:#\{.*?\}|#|\\\\.)[^)#\\\\]*)*\)?|\[[^\]#\\\\]*(?:(?:#\{.*?\}|#|\\\\.)[^\]#\\\\]*)*\]?|\{[^}#\\\\]*(?:(?:#\{.*?\}|#|\\\\.)[^}#\\\\]*)*\}?|<[^>#\\\\]*(?:(?:#\{.*?\}|#|\\\\.)[^>#\\\\]*)*>?|([^a-zA-Z\\\\])(?:(?!\1)[^#\\\\])*(?:(?:#\{.*?\}|#|\\\\.)(?:(?!\1)[^#\\\\])*)*\1?)|\([^)#\\\\]*(?:(?:#\{.*?\}|#|\\\\.)[^)#\\\\]*)*\)?|\[[^\]#\\\\]*(?:(?:#\{.*?\}|#|\\\\.)[^\]#\\\\]*)*\]?|\{[^}#\\\\]*(?:(?:#\{.*?\}|#|\\\\.)[^}#\\\\]*)*\}?|<[^>#\\\\]*(?:(?:#\{.*?\}|#|\\\\.)[^>#\\\\]*)*>?|([^a-zA-Z\s\\\\])(?:(?!\2)[^#\\\\])*(?:(?:#\{.*?\}|#|\\\\.)(?:(?!\2)[^#\\\\])*)*\2?|\\\\[^#\\\\]*(?:(?:#\{.*?\}|#)[^#\\\\]*)*\\\\?)/
			elsif @scanner.scan(/:(?:#{GLOBAL_VARIABLE}|#{METHOD_NAME_EX}|#{STRING})/ox)
				kind = :symbol
			elsif @scanner.scan(/
				\? (?:
					[^\s\\]
				|
					\\ (?:M-\\C-|C-\\M-|M-\\c|c\\M-|c|C-|M-))? (?: \\ (?: . | [0-7]{3} | x[0-9A-Fa-f][0-9A-Fa-f] )
				)
			/mox)
				kind = :integer

			elsif @scanner.scan(/ [-+*\/%=<>;,|&!()\[\]{}~?] | \.\.?\.? | ::? /x)
				kind = :operator
				@regexp_allowed = :set if @scanner.matched[-1,1] =~ /[~=!<>|&^,\(\[+\-\/\*%]\z/
			elsif @scanner.scan(FLOAT)
				kind = :float
			elsif @scanner.scan(INTEGER)
				kind = :integer
			else
				@scanner.getch
			end
		end

		token = Token.new @scanner.matched, kind

		if kind == :regexp
			token.text << @scanner.scan(/[eimnosux]*/)
		end

		@regexp_allowed = (@regexp_allowed == :set)  # delayed flag setting

		token
	end
end

register Ruby, 'ruby', 'rb'

	end
end
class Set
  include Enumerable

  # Creates a new set containing the given objects.
  def self.[](*ary)
    new(ary)
  end

  # Creates a new set containing the elements of the given enumerable
  # object.
  #
  # If a block is given, the elements of enum are preprocessed by the
  # given block.
  def initialize(enum = nil, &block) # :yields: o
    @hash ||= Hash.new

    enum.nil? and return

    if block
      enum.each { |o| add(block[o]) }
    else
      merge(enum)
    end
  end

  # Copy internal hash.
  def initialize_copy(orig)
    @hash = orig.instance_eval{@hash}.dup
  end

  # Returns the number of elements.
  def size
    @hash.size
  end
  alias length size

  # Returns true if the set contains no elements.
  def empty?
    @hash.empty?
  end

  # Removes all elements and returns self.
  def clear
    @hash.clear
    self
  end

  # Replaces the contents of the set with the contents of the given
  # enumerable object and returns self.
  def replace(enum)
    if enum.class == self.class
      @hash.replace(enum.instance_eval { @hash })
    else
      enum.is_a?(Enumerable) or raise ArgumentError, "value must be enumerable"
      clear
      enum.each { |o| add(o) }
    end

    self
  end

  # Converts the set to an array.  The order of elements is uncertain.
  def to_a
    @hash.keys
  end

  def flatten_merge(set, seen = Set.new)
    set.each { |e|
      if e.is_a?(Set)
	if seen.include?(e_id = e.object_id)
	  raise ArgumentError, "tried to flatten recursive Set"
	end

	seen.add(e_id)
	flatten_merge(e, seen)
	seen.delete(e_id)
      else
	add(e)
      end
    }

    self
  end
  protected :flatten_merge

  # Returns a new set that is a copy of the set, flattening each
  # containing set recursively.
  def flatten
    self.class.new.flatten_merge(self)
  end

  # Equivalent to Set#flatten, but replaces the receiver with the
  # result in place.  Returns nil if no modifications were made.
  def flatten!
    if detect { |e| e.is_a?(Set) }
      replace(flatten())
    else
      nil
    end
  end

  # Returns true if the set contains the given object.
  def include?(o)
    @hash.include?(o)
  end
  alias member? include?

  # Returns true if the set is a superset of the given set.
  def superset?(set)
    set.is_a?(Set) or raise ArgumentError, "value must be a set"
    return false if size < set.size
    set.all? { |o| include?(o) }
  end

  # Returns true if the set is a proper superset of the given set.
  def proper_superset?(set)
    set.is_a?(Set) or raise ArgumentError, "value must be a set"
    return false if size <= set.size
    set.all? { |o| include?(o) }
  end

  # Returns true if the set is a subset of the given set.
  def subset?(set)
    set.is_a?(Set) or raise ArgumentError, "value must be a set"
    return false if set.size < size
    all? { |o| set.include?(o) }
  end

  # Returns true if the set is a proper subset of the given set.
  def proper_subset?(set)
    set.is_a?(Set) or raise ArgumentError, "value must be a set"
    return false if set.size <= size
    all? { |o| set.include?(o) }
  end

  # Calls the given block once for each element in the set, passing
  # the element as parameter.
  def each
    @hash.each_key { |o| yield(o) }
    self
  end

  # Adds the given object to the set and returns self.  Use +merge+ to
  # add several elements at once.
  def add(o)
    @hash[o] = true
    self
  end
  alias << add

  # Adds the given object to the set and returns self.  If the
  # object is already in the set, returns nil.
  def add?(o)
    if include?(o)
      nil
    else
      add(o)
    end
  end

  # Deletes the given object from the set and returns self.  Use +subtract+ to
  # delete several items at once.
  def delete(o)
    @hash.delete(o)
    self
  end

  # Deletes the given object from the set and returns self.  If the
  # object is not in the set, returns nil.
  def delete?(o)
    if include?(o)
      delete(o)
    else
      nil
    end
  end

  # Deletes every element of the set for which block evaluates to
  # true, and returns self.
  def delete_if
    @hash.delete_if { |o,| yield(o) }
    self
  end

  # Do collect() destructively.
  def collect!
    set = self.class.new
    each { |o| set << yield(o) }
    replace(set)
  end
  alias map! collect!

  # Equivalent to Set#delete_if, but returns nil if no changes were
  # made.
  def reject!
    n = size
    delete_if { |o| yield(o) }
    size == n ? nil : self
  end

  # Merges the elements of the given enumerable object to the set and
  # returns self.
  def merge(enum)
    if enum.is_a?(Set)
      @hash.update(enum.instance_eval { @hash })
    else
      enum.is_a?(Enumerable) or raise ArgumentError, "value must be enumerable"
      enum.each { |o| add(o) }
    end

    self
  end

  # Deletes every element that appears in the given enumerable object
  # and returns self.
  def subtract(enum)
    enum.is_a?(Enumerable) or raise ArgumentError, "value must be enumerable"
    enum.each { |o| delete(o) }
    self
  end

  # Returns a new set built by merging the set and the elements of the
  # given enumerable object.
  def |(enum)
    enum.is_a?(Enumerable) or raise ArgumentError, "value must be enumerable"
    dup.merge(enum)
  end
  alias + |		##
  alias union |		##

  # Returns a new set built by duplicating the set, removing every
  # element that appears in the given enumerable object.
  def -(enum)
    enum.is_a?(Enumerable) or raise ArgumentError, "value must be enumerable"
    dup.subtract(enum)
  end
  alias difference -	##

  # Returns a new array containing elements common to the set and the
  # given enumerable object.
  def &(enum)
    enum.is_a?(Enumerable) or raise ArgumentError, "value must be enumerable"
    n = self.class.new
    enum.each { |o| n.add(o) if include?(o) }
    n
  end
  alias intersection &	##

  # Returns a new array containing elements exclusive between the set
  # and the given enumerable object.  (set ^ enum) is equivalent to
  # ((set | enum) - (set & enum)).
  def ^(enum)
    enum.is_a?(Enumerable) or raise ArgumentError, "value must be enumerable"
    n = dup
    enum.each { |o| if n.include?(o) then n.delete(o) else n.add(o) end }
    n
  end

  # Returns true if two sets are equal.  The equality of each couple
  # of elements is defined according to Object#eql?.
  def ==(set)
    equal?(set) and return true

    set.is_a?(Set) && size == set.size or return false

    hash = @hash.dup
    set.all? { |o| hash.include?(o) }
  end

  def hash	# :nodoc:
    @hash.hash
  end

  def eql?(o)	# :nodoc:
    return false unless o.is_a?(Set)
    @hash.eql?(o.instance_eval{@hash})
  end

  # Classifies the set by the return value of the given block and
  # returns a hash of {value => set of elements} pairs.  The block is
  # called once for each element of the set, passing the element as
  # parameter.
  #
  # e.g.:
  #
  #   require 'set'
  #   files = Set.new(Dir.glob("*.rb"))
  #   hash = files.classify { |f| File.mtime(f).year }
  #   p hash    # => {2000=>#<Set: {"a.rb", "b.rb"}>,
  #             #     2001=>#<Set: {"c.rb", "d.rb", "e.rb"}>,
  #             #     2002=>#<Set: {"f.rb"}>}
  def classify # :yields: o
    h = {}

    each { |i|
      x = yield(i)
      (h[x] ||= self.class.new).add(i)
    }

    h
  end

  # Divides the set into a set of subsets according to the commonality
  # defined by the given block.
  #
  # If the arity of the block is 2, elements o1 and o2 are in common
  # if block.call(o1, o2) is true.  Otherwise, elements o1 and o2 are
  # in common if block.call(o1) == block.call(o2).
  #
  # e.g.:
  #
  #   require 'set'
  #   numbers = Set[1, 3, 4, 6, 9, 10, 11]
  #   set = numbers.divide { |i,j| (i - j).abs == 1 }
  #   p set     # => #<Set: {#<Set: {1}>,
  #             #            #<Set: {11, 9, 10}>,
  #             #            #<Set: {3, 4}>,
  #             #            #<Set: {6}>}>
  def divide(&func)
    if func.arity == 2
      require 'tsort'

      class << dig = {}		# :nodoc:
	include TSort

	alias tsort_each_node each_key
	def tsort_each_child(node, &block)
	  fetch(node).each(&block)
	end
      end

      each { |u|
	dig[u] = a = []
	each{ |v| func.call(u, v) and a << v }
      }

      set = Set.new()
      dig.each_strongly_connected_component { |css|
	set.add(self.class.new(css))
      }
      set
    else
      Set.new(classify(&func).values)
    end
  end

  InspectKey = :__inspect_key__         # :nodoc:

  # Returns a string containing a human-readable representation of the
  # set. ("#<Set: {element1, element2, ...}>")
  def inspect
    ids = (Thread.current[InspectKey] ||= [])

    if ids.include?(object_id)
      return sprintf('#<%s: {...}>', self.class.name)
    end

    begin
      ids << object_id
      return sprintf('#<%s: {%s}>', self.class, to_a.inspect[1..-2])
    ensure
      ids.pop
    end
  end

  def pretty_print(pp)	# :nodoc:
    pp.text sprintf('#<%s: {', self.class.name)
    pp.nest(1) {
      pp.seplist(self) { |o|
	pp.pp o
      }
    }
    pp.text "}>"
  end

  def pretty_print_cycle(pp)	# :nodoc:
    pp.text sprintf('#<%s: {%s}>', self.class.name, empty? ? '' : '...')
  end
end

# SortedSet implements a set which elements are sorted in order.  See Set.
class SortedSet < Set
  @@setup = false

  class << self
    def [](*ary)	# :nodoc:
      new(ary)
    end

    def setup	# :nodoc:
      @@setup and return

      begin
	require 'rbtree'

	module_eval %{
	  def initialize(*args, &block)
	    @hash = RBTree.new
	    super
	  end
	}
      rescue LoadError
	module_eval %{
	  def initialize(*args, &block)
	    @keys = nil
	    super
	  end

	  def clear
	    @keys = nil
	    super
	  end

	  def replace(enum)
	    @keys = nil
	    super
	  end

	  def add(o)
	    @keys = nil
	    @hash[o] = true
	    self
	  end
	  alias << add

	  def delete(o)
	    @keys = nil
	    @hash.delete(o)
	    self
	  end

	  def delete_if
	    n = @hash.size
	    @hash.delete_if { |o,| yield(o) }
	    @keys = nil if @hash.size != n
	    self
	  end

	  def merge(enum)
	    @keys = nil
	    super
	  end

	  def each
	    to_a.each { |o| yield(o) }
	  end

	  def to_a
	    (@keys = @hash.keys).sort! unless @keys
	    @keys
	  end
	}
      end

      @@setup = true
    end
  end

  def initialize(*args, &block)	# :nodoc:
    SortedSet.setup
    initialize(*args, &block)
  end
end

module Enumerable
  # Makes a set from the enumerable object with given arguments.
  def to_set(klass = Set, *args, &block)
    klass.new(self, *args, &block)
  end
end

# =begin
# == RestricedSet class
# RestricedSet implements a set with restrictions defined by a given
# block.
#
# === Super class
#     Set
#
# === Class Methods
# --- RestricedSet::new(enum = nil) { |o| ... }
# --- RestricedSet::new(enum = nil) { |rset, o| ... }
#     Creates a new restricted set containing the elements of the given
#     enumerable object.  Restrictions are defined by the given block.
#
#     If the block's arity is 2, it is called with the RestrictedSet
#     itself and an object to see if the object is allowed to be put in
#     the set.
#
#     Otherwise, the block is called with an object to see if the object
#     is allowed to be put in the set.
#
# === Instance Methods
# --- restriction_proc
#     Returns the restriction procedure of the set.
#
# =end
#
# class RestricedSet < Set
#   def initialize(*args, &block)
#     @proc = block or raise ArgumentError, "missing a block"
#
#     if @proc.arity == 2
#       instance_eval %{
# 	def add(o)
# 	  @hash[o] = true if @proc.call(self, o)
# 	  self
# 	end
# 	alias << add
#
# 	def add?(o)
# 	  if include?(o) || !@proc.call(self, o)
# 	    nil
# 	  else
# 	    @hash[o] = true
# 	    self
# 	  end
# 	end
#
# 	def replace(enum)
# 	  enum.is_a?(Enumerable) or raise ArgumentError, "value must be enumerable"
# 	  clear
# 	  enum.each { |o| add(o) }
#
# 	  self
# 	end
#
# 	def merge(enum)
# 	  enum.is_a?(Enumerable) or raise ArgumentError, "value must be enumerable"
# 	  enum.each { |o| add(o) }
#
# 	  self
# 	end
#       }
#     else
#       instance_eval %{
# 	def add(o)
#         if @proc.call(o)
# 	    @hash[o] = true
#         end
# 	  self
# 	end
# 	alias << add
#
# 	def add?(o)
# 	  if include?(o) || !@proc.call(o)
# 	    nil
# 	  else
# 	    @hash[o] = true
# 	    self
# 	  end
# 	end
#       }
#     end
#
#     super(*args)
#   end
#
#   def restriction_proc
#     @proc
#   end
# end

if $0 == __FILE__
  eval DATA.read, nil, $0, __LINE__+4
end

# = rweb - CGI Support Library
#
# Author:: Johannes Barre (mailto:rweb@igels.net)
# Copyright:: Copyright (c) 2003, 04 by Johannes Barre
# License:: GNU Lesser General Public License (COPYING, http://www.gnu.org/copyleft/lesser.html)
# Version:: 0.1.0
# CVS-ID:: $Id: example.rb 39 2005-11-05 03:33:55Z murphy $
#
# == What is Rweb?
# Rweb is a replacement for the cgi class included in the ruby distribution.
#
# == How to use
#
# === Basics
#
# This class is made to be as easy as possible to use. An example:
#
# 	require "rweb"
#
# 	web = Rweb.new
# 	web.out do
# 		web.puts "Hello world!"
# 	end
#
# The visitor will get a simple "Hello World!" in his browser. Please notice,
# that won't set html-tags for you, so you should better do something like this:
#
# 	require "rweb"
#
# 	web = Rweb.new
# 	web.out do
# 		web.puts "<html><body>Hello world!</body></html>"
# 	end
#
# === Set headers
# Of course, it's also possible to tell the browser, that the content of this
# page is plain text instead of html code:
#
# 	require "rweb"
#
# 	web = Rweb.new
# 	web.out do
# 		web.header("content-type: text/plain")
# 		web.puts "Hello plain world!"
# 	end
#
# Please remember, headers can't be set after the page content has been send.
# You have to set all nessessary headers before the first puts oder print. It's
# possible to cache the content until everything is complete. Doing it this
# way, you can set headers everywhere.
#
# If you set a header twice, the second header will replace the first one. The
# header name is not casesensitive, it will allways converted in to the
# capitalised form suggested by the w3c (http://w3.org)
#
# === Set cookies
# Setting cookies is quite easy:
# 	include 'rweb'
#
# 	web = Rweb.new
# 	Cookie.new("Visits", web.cookies['visits'].to_i +1)
# 	web.out do
# 		web.puts "Welcome back! You visited this page #{web.cookies['visits'].to_i +1} times"
# 	end
#
# See the class Cookie for more details.
#
# === Get form and cookie values
# There are four ways to submit data from the browser to the server and your
# ruby script: via GET, POST, cookies and file upload. Rweb doesn't support
# file upload by now.
#
# 	include 'rweb'
#
# 	web = Rweb.new
# 	web.out do
# 		web.print "action: #{web.get['action']} "
# 		web.puts "The value of the cookie 'visits' is #{web.cookies['visits']}"
# 		web.puts "The post parameter 'test['x']' is #{web.post['test']['x']}"
# 	end

RWEB_VERSION = "0.1.0"
RWEB = "rweb/#{RWEB_VERSION}"

#require 'rwebcookie' -> edit by bunny :-)

class Rweb
    # All parameter submitted via the GET method are available in attribute
		# get. This is Hash, where every parameter is available as a key-value
		# pair.
		#
		# If your input tag has a name like this one, it's value will be available
		# as web.get["fieldname"]
		#  <input name="fieldname">
		# You can submit values as a Hash
		#  <input name="text['index']">
		#  <input name="text['index2']">
		# will be available as
		#  web.get["text"]["index"]
		#  web.get["text"]["index2"]
		# Integers are also possible
		#  <input name="int[2]">
		#  <input name="int[3]['hi']>
		# will be available as
		#  web.get["int"][2]
		#  web.get["int"][3]["hi"]
		# If you specify no index, the lowest unused index will be used:
		#  <input name="int[]"><!-- First Field -->
		#  <input name="int[]"><!-- Second one -->
		# will be available as
		#  web.get["int"][0] # First Field
		#  web.get["int"][1] # Second one
		# Please notice, this doesn'd work like you might expect:
		#  <input name="text[index]">
		# It will not be available as web.get["text"]["index"] but
		#  web.get["text[index]"]
    attr_reader :get

    # All parameters submitted via POST are available in the attribute post. It
		# works like the get attribute.
		#  <input name="text[0]">
		# will be available as
		#  web.post["text"][0]
		attr_reader :post

    # All cookies submitted by the browser are available in cookies. This is a
		# Hash, where every cookie is a key-value pair.
		attr_reader :cookies

    # The name of the browser identification is submitted as USER_AGENT and
		# available in this attribute.
		attr_reader :user_agent

    # The IP address of the client.
		attr_reader :remote_addr

    # Creates a new Rweb object. This should only done once. You can set various
    # options via the settings hash.
    #
    # "cache" => true: Everything you script send to the client will be cached
    # until the end of the out block or until flush is called. This way, you
    # can modify headers and cookies even after printing something to the client.
    #
    # "safe" => level: Changes the $SAFE attribute. By default, $SAFE will be set
    # to 1. If $SAFE is already higher than this value, it won't be changed.
    #
    # "silend" => true: Normaly, Rweb adds automaticly a header like this
    # "X-Powered-By: Rweb/x.x.x (Ruby/y.y.y)". With the silend option you can
    # suppress this.
    def initialize (settings = {})
        # {{{
        @header = {}
        @cookies = {}
        @get = {}
        @post = {}

        # Internal attributes
        @status = nil
        @reasonPhrase = nil
        @setcookies = []
        @output_started = false;
        @output_allowed = false;

        @mod_ruby = false
        @env = ENV.to_hash

        if defined?(MOD_RUBY)
            @output_method = "mod_ruby"
            @mod_ruby = true
        elsif @env['SERVER_SOFTWARE'] =~ /^Microsoft-IIS/i
            @output_method = "nph"
        else
            @output_method = "ph"
        end

        unless settings.is_a?(Hash)
            raise TypeError, "settings must be a Hash"
        end
        @settings = settings

        unless @settings.has_key?("safe")
            @settings["safe"] = 1
        end

        if $SAFE < @settings["safe"]
            $SAFE = @settings["safe"]
        end

        unless @settings.has_key?("cache")
            @settings["cache"] = false
        end

        # mod_ruby sets no QUERY_STRING variable, if no GET-Parameters are given
        unless @env.has_key?("QUERY_STRING")
            @env["QUERY_STRING"] = ""
        end

        # Now we split the QUERY_STRING by the seperators & and ; or, if
        # specified, settings['get seperator']
        unless @settings.has_key?("get seperator")
            get_args = @env['QUERY_STRING'].split(/[&;]/)
        else
            get_args = @env['QUERY_STRING'].split(@settings['get seperator'])
        end

        get_args.each do | arg |
            arg_key, arg_val = arg.split(/=/, 2)
            arg_key = Rweb::unescape(arg_key)
            arg_val = Rweb::unescape(arg_val)

            # Parse names like name[0], name['text'] or name[]
            pattern = /^(.+)\[("[^\]]*"|'[^\]]*'|[0-9]*)\]$/
            keys = []
            while match = pattern.match(arg_key)
                arg_key = match[1]
                keys = [match[2]] + keys
            end
            keys = [arg_key] + keys

            akt = @get
            last = nil
            lastkey = nil
            keys.each do |key|
                if key == ""
                    # No key specified (like in "test[]"), so we use the
                    # lowerst unused Integer as key
                    key = 0
                    while akt.has_key?(key)
                        key += 1
                    end
                elsif /^[0-9]*$/ =~ key
                    # If the index is numerical convert it to an Integer
                    key = key.to_i
                elsif key[0].chr == "'" || key[0].chr == '"'
                    key = key[1, key.length() -2]
                end
                if !akt.has_key?(key) || !akt[key].class == Hash
                    # create an empty Hash if there isn't already one
                    akt[key] = {}
                end
                last = akt
                lastkey = key
                akt = akt[key]
            end
            last[lastkey] = arg_val
        end

        if @env['REQUEST_METHOD'] == "POST"
            if @env.has_key?("CONTENT_TYPE") && @env['CONTENT_TYPE'] == "application/x-www-form-urlencoded" && @env.has_key?('CONTENT_LENGTH')
                unless @settings.has_key?("post seperator")
                    post_args = $stdin.read(@env['CONTENT_LENGTH'].to_i).split(/[&;]/)
                else
                    post_args = $stdin.read(@env['CONTENT_LENGTH'].to_i).split(@settings['post seperator'])
                end
                post_args.each do | arg |
                    arg_key, arg_val = arg.split(/=/, 2)
                    arg_key = Rweb::unescape(arg_key)
                    arg_val = Rweb::unescape(arg_val)

                    # Parse names like name[0], name['text'] or name[]
                    pattern = /^(.+)\[("[^\]]*"|'[^\]]*'|[0-9]*)\]$/
                    keys = []
                    while match = pattern.match(arg_key)
                        arg_key = match[1]
                        keys = [match[2]] + keys
                    end
                    keys = [arg_key] + keys

                    akt = @post
                    last = nil
                    lastkey = nil
                    keys.each do |key|
                        if key == ""
                            # No key specified (like in "test[]"), so we use
                            # the lowerst unused Integer as key
                            key = 0
                            while akt.has_key?(key)
                                key += 1
                            end
                        elsif /^[0-9]*$/ =~ key
                            # If the index is numerical convert it to an Integer
                            key = key.to_i
                        elsif key[0].chr == "'" || key[0].chr == '"'
                            key = key[1, key.length() -2]
                        end
                        if !akt.has_key?(key) || !akt[key].class == Hash
                            # create an empty Hash if there isn't already one
                            akt[key] = {}
                        end
                        last = akt
                        lastkey = key
                        akt = akt[key]
                    end
                    last[lastkey] = arg_val
                end
            else
                # Maybe we should print a warning here?
                $stderr.print("Unidentified form data recived and discarded.")
            end
        end

        if @env.has_key?("HTTP_COOKIE")
            cookie = @env['HTTP_COOKIE'].split(/; ?/)
            cookie.each do | c |
                cookie_key, cookie_val = c.split(/=/, 2)

                @cookies [Rweb::unescape(cookie_key)] = Rweb::unescape(cookie_val)
            end
        end

        if defined?(@env['HTTP_USER_AGENT'])
            @user_agent = @env['HTTP_USER_AGENT']
        else
            @user_agent = nil;
        end

        if defined?(@env['REMOTE_ADDR'])
            @remote_addr = @env['REMOTE_ADDR']
        else
            @remote_addr = nil
        end
        # }}}
    end

    # Prints a String to the client. If caching is enabled, the String will
    # buffered until the end of the out block ends.
    def print(str = "")
        # {{{
        unless @output_allowed
            raise "You just can write to output inside of a Rweb::out-block"
        end

        if @settings["cache"]
            @buffer += [str.to_s]
        else
            unless @output_started
                sendHeaders
            end
            $stdout.print(str)
        end
        nil
        # }}}
    end

    # Prints a String to the client and adds a line break at the end. Please
		# remember, that a line break is not visible in HTML, use the <br> HTML-Tag
		# for this. If caching is enabled, the String will buffered until the end
		# of the out block ends.
    def puts(str = "")
        # {{{
        self.print(str + "\n")
        # }}}
    end

		# Alias to print.
    def write(str = "")
        # {{{
        self.print(str)
        # }}}
    end

    # If caching is enabled, all cached data are send to the cliend and the
		# cache emptied.
    def flush
        # {{{
        unless @output_allowed
            raise "You can't use flush outside of a Rweb::out-block"
        end
        buffer = @buffer.join

        unless @output_started
            sendHeaders
        end
        $stdout.print(buffer)

        @buffer = []
        # }}}
    end

    # Sends one or more header to the client. All headers are cached just
		# before body data are send to the client. If the same header are set
		# twice, only the last value is send.
		#
		# Example:
		#  web.header("Last-Modified: Mon, 16 Feb 2004 20:15:41 GMT")
		#  web.header("Location: http://www.ruby-lang.org")
		#
		# You can specify more than one header at the time by doing something like
		# this:
		#  web.header("Content-Type: text/plain\nContent-Length: 383")
		# or
		#  web.header(["Content-Type: text/plain", "Content-Length: 383"])
    def header(str)
        # {{{
        if @output_started
            raise "HTTP-Headers are already send. You can't change them after output has started!"
        end
        unless @output_allowed
            raise "You just can set headers inside of a Rweb::out-block"
        end
        if str.is_a?Array
            str.each do | value |
                self.header(value)
            end

        elsif str.split(/\n/).length > 1
            str.split(/\n/).each do | value |
                self.header(value)
            end

        elsif str.is_a? String
            str.gsub!(/\r/, "")

            if (str =~ /^HTTP\/1\.[01] [0-9]{3} ?.*$/) == 0
                pattern = /^HTTP\/1.[01] ([0-9]{3}) ?(.*)$/

                result = pattern.match(str)
                self.setstatus(result[0], result[1])
            elsif (str =~ /^status: [0-9]{3} ?.*$/i) == 0
                pattern = /^status: ([0-9]{3}) ?(.*)$/i

                result = pattern.match(str)
                self.setstatus(result[0], result[1])
            else
                a = str.split(/: ?/, 2)

                @header[a[0].downcase] = a[1]
            end
        end
        # }}}
    end

    # Changes the status of this page. There are several codes like "200 OK",
		# "302 Found", "404 Not Found" or "500 Internal Server Error". A list of
		# all codes is available at
		# http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10
		#
		# You can just send the code number, the reason phrase will be added
		# automaticly with the recommendations from the w3c if not specified. If
		# you set the status twice or more, only the last status will be send.
		# Examples:
		#  web.status("401 Unauthorized")
		#  web.status("410 Sad but true, this lonely page is gone :(")
		#  web.status(206)
		#  web.status("400")
		#
		# The default status is "200 OK". If a "Location" header is set, the
		# default status is "302 Found".
    def status(str)
        # {{{
        if @output_started
            raise "HTTP-Headers are already send. You can't change them after output has started!"
        end
        unless @output_allowed
            raise "You just can set headers inside of a Rweb::out-block"
        end
        if str.is_a?Integer
            @status = str
        elsif str.is_a?String
            p1 = /^([0-9]{3}) ?(.*)$/
            p2 = /^HTTP\/1\.[01] ([0-9]{3}) ?(.*)$/
            p3 = /^status: ([0-9]{3}) ?(.*)$/i

            if (a = p1.match(str)) == nil
                if (a = p2.match(str)) == nil
                    if (a = p3.match(str)) == nil
                        raise ArgumentError, "Invalid argument", caller
                    end
                end
            end
            @status = a[1].to_i
            if a[2] != ""
                @reasonPhrase = a[2]
            else
                @reasonPhrase = getReasonPhrase(@status)
            end
        else
            raise ArgumentError, "Argument of setstatus must be integer or string", caller
        end
        # }}}
    end

    # Handles the output of your content and rescues all exceptions. Send all
		# data in the block to this method. For example:
		#  web.out do
		#      web.header("Content-Type: text/plain")
		#      web.puts("Hello, plain world!")
		#  end
    def out
        # {{{
        @output_allowed = true
        @buffer = []; # We use an array as buffer, because it's more performant :)

        begin
            yield
        rescue Exception => exception
            $stderr.puts "Ruby exception rescued (#{exception.class}): #{exception.message}"
            $stderr.puts exception.backtrace.join("\n")

            unless @output_started
                self.setstatus(500)
                @header = {}
            end

            unless (@settings.has_key?("hide errors") and @settings["hide errors"] == true)
                unless @output_started
                    self.header("Content-Type: text/html")
                    self.puts "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Strict//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">"
                    self.puts "<html>"
                    self.puts "<head>"
                    self.puts "<title>500 Internal Server Error</title>"
                    self.puts "</head>"
                    self.puts "<body>"
                end
                if @header.has_key?("content-type") and (@header["content-type"] =~ /^text\/html/i) == 0
                    self.puts "<h1>Internal Server Error</h1>"
                    self.puts "<p>The server encountered an exception and was unable to complete your request.</p>"
                    self.puts "<p>The exception has provided the following information:</p>"
                    self.puts "<pre style=\"background: #FFCCCC; border: black solid 2px; margin-left: 2cm; margin-right: 2cm; padding: 2mm;\"><b>#{exception.class}</b>: #{exception.message} <b>on</b>"
                    self.puts
                    self.puts "#{exception.backtrace.join("\n")}</pre>"
                    self.puts "</body>"
                    self.puts "</html>"
                else
                    self.puts "The server encountered an exception and was unable to complete your request"
                    self.puts "The exception has provided the following information:"
                    self.puts "#{exception.class}: #{exception.message}"
                    self.puts
                    self.puts exception.backtrace.join("\n")
                end
            end
        end

        if @settings["cache"]
            buffer = @buffer.join

            unless @output_started
                unless @header.has_key?("content-length")
                    self.header("content-length: #{buffer.length}")
                end

                sendHeaders
            end
            $stdout.print(buffer)
        elsif !@output_started
            sendHeaders
        end
        @output_allowed = false;
        # }}}
    end

    # Decodes URL encoded data, %20 for example stands for a space.
    def Rweb.unescape(str)
        # {{{
        if defined? str and str.is_a? String
            str.gsub!(/\+/, " ")
            str.gsub(/%.{2}/) do | s |
                s[1,2].hex.chr
            end
        end
        # }}}
    end

    protected
    def sendHeaders
        # {{{

        Cookie.disallow # no more cookies can be set or modified
        if !(@settings.has_key?("silent") and @settings["silent"] == true) and !@header.has_key?("x-powered-by")
            if @mod_ruby
                header("x-powered-by: #{RWEB} (Ruby/#{RUBY_VERSION}, #{MOD_RUBY})");
            else
                header("x-powered-by: #{RWEB} (Ruby/#{RUBY_VERSION})");
            end
        end

        if @output_method == "ph"
            if ((@status == nil or @status == 200) and !@header.has_key?("content-type") and !@header.has_key?("location"))
                header("content-type: text/html")
            end

            if @status != nil
                $stdout.print "Status: #{@status} #{@reasonPhrase}\r\n"
            end

            @header.each do |key, value|
                key = key *1 # "unfreeze" key :)
                key[0] = key[0,1].upcase![0]

                key = key.gsub(/-[a-z]/) do |char|
                    "-" + char[1,1].upcase
                end

                $stdout.print "#{key}: #{value}\r\n"
            end
            cookies = Cookie.getHttpHeader # Get all cookies as an HTTP Header
            if cookies
                $stdout.print cookies
            end

            $stdout.print "\r\n"

        elsif @output_method == "nph"
        elsif @output_method == "mod_ruby"
            r = Apache.request

            if ((@status == nil or @status == 200) and !@header.has_key?("content-type") and !@header.has_key?("location"))
                header("text/html")
            end

            if @status != nil
                r.status_line = "#{@status} #{@reasonPhrase}"
            end

            r.send_http_header
            @header.each do |key, value|
                key = key *1 # "unfreeze" key :)

                key[0] = key[0,1].upcase![0]
                key = key.gsub(/-[a-z]/) do |char|
                    "-" + char[1,1].upcase
                end
                puts "#{key}: #{value.class}"
                #r.headers_out[key] = value
            end
        end
        @output_started = true
        # }}}
    end

    def getReasonPhrase (status)
        # {{{
        if status == 100
            "Continue"
        elsif status == 101
            "Switching Protocols"
        elsif status == 200
            "OK"
        elsif status == 201
            "Created"
        elsif status == 202
            "Accepted"
        elsif status == 203
            "Non-Authoritative Information"
        elsif status == 204
            "No Content"
        elsif status == 205
            "Reset Content"
        elsif status == 206
            "Partial Content"
        elsif status == 300
            "Multiple Choices"
        elsif status == 301
            "Moved Permanently"
        elsif status == 302
            "Found"
        elsif status == 303
            "See Other"
        elsif status == 304
            "Not Modified"
        elsif status == 305
            "Use Proxy"
        elsif status == 307
            "Temporary Redirect"
        elsif status == 400
            "Bad Request"
        elsif status == 401
            "Unauthorized"
        elsif status == 402
            "Payment Required"
        elsif status == 403
            "Forbidden"
        elsif status == 404
            "Not Found"
        elsif status == 405
            "Method Not Allowed"
        elsif status == 406
            "Not Acceptable"
        elsif status == 407
            "Proxy Authentication Required"
        elsif status == 408
            "Request Time-out"
        elsif status == 409
            "Conflict"
        elsif status == 410
            "Gone"
        elsif status == 411
            "Length Required"
        elsif status == 412
            "Precondition Failed"
        elsif status == 413
            "Request Entity Too Large"
        elsif status == 414
            "Request-URI Too Large"
        elsif status == 415
            "Unsupported Media Type"
        elsif status == 416
            "Requested range not satisfiable"
        elsif status == 417
            "Expectation Failed"
        elsif status == 500
            "Internal Server Error"
        elsif status == 501
            "Not Implemented"
        elsif status == 502
            "Bad Gateway"
        elsif status == 503
            "Service Unavailable"
        elsif status == 504
            "Gateway Time-out"
        elsif status == 505
            "HTTP Version not supported"
        else
            raise "Unknown Statuscode. See http://www.w3.org/Protocols/rfc2616/rfc2616-sec6.html#sec6.1 for more information."
        end
        # }}}
    end
end

class Cookie
	attr_reader :name, :value, :maxage, :path, :domain, :secure, :comment

	# Sets a cookie. Please see below for details of the attributes.
	def initialize (name, value = nil, maxage = nil, path = nil, domain = nil, secure = false)
		# {{{
		# HTTP headers (Cookies are a HTTP header) can only set, while no content
		# is send. So an exception will be raised, when @@allowed is set to false
		# and a new cookie has set.
		unless defined?(@@allowed)
			@@allowed = true
		end
		unless @@allowed
			raise "You can't set cookies after the HTTP headers are send."
		end

		unless defined?(@@list)
			@@list = []
		end
		@@list += [self]

		unless defined?(@@type)
			@@type = "netscape"
		end

		unless name.class == String
			raise TypeError, "The name of a cookie must be a string", caller
		end
		if value.class.superclass == Integer || value.class == Float
			value = value.to_s
		elsif value.class != String && value != nil
			raise TypeError, "The value of a cookie must be a string, integer, float or nil", caller
		end
		if maxage.class == Time
			maxage = maxage - Time.now
		elsif !maxage.class.superclass == Integer  || !maxage == nil
			raise TypeError, "The maxage date of a cookie must be an Integer or Time object or nil.", caller
		end
		unless path.class == String  || path == nil
			raise TypeError, "The path of a cookie must be nil or a string", caller
		end
		unless domain.class == String  || domain == nil
			raise TypeError, "The value of a cookie must be nil or a string", caller
		end
		unless secure == true  || secure == false
			raise TypeError, "The secure field of a cookie must be true or false", caller
		end

		@name, @value, @maxage, @path, @domain, @secure = name, value, maxage, path, domain, secure
		@comment = nil
		# }}}
	end

	# Modifies the value of this cookie. The information you want to store. If the
	# value is nil, the cookie will be deleted by the client.
	#
	# This attribute can be a String, Integer or Float object or nil.
	def value=(value)
		# {{{
		if value.class.superclass == Integer || value.class == Float
			value = value.to_s
		elsif value.class != String && value != nil
			raise TypeError, "The value of a cookie must be a string, integer, float or nil", caller
		end
		@value = value
		# }}}
	end

	# Modifies the maxage of this cookie. This attribute defines the lifetime of
	# the cookie, in seconds. A value of 0 means the cookie should be discarded
	# imediatly. If it set to nil, the cookie will be deleted when the browser
	# will be closed.
	#
	# Attention: This is different from other implementations like PHP, where you
	# gives the seconds since 1/1/1970 0:00:00 GMT.
	#
	# This attribute must be an Integer or Time object or nil.
	def maxage=(maxage)
		# {{{
		if maxage.class == Time
			maxage = maxage - Time.now
		elsif maxage.class.superclass == Integer  || !maxage == nil
			raise TypeError, "The maxage of a cookie must be an Interger or Time object or nil.", caller
		end
		@maxage = maxage
		# }}}
	end

	# Modifies the path value of this cookie. The client will send this cookie
	# only, if the requested document is this directory or a subdirectory of it.
	#
	# The value of the attribute must be a String object or nil.
	def path=(path)
		# {{{
		unless path.class == String  || path == nil
			raise TypeError, "The path of a cookie must be nil or a string", caller
		end
		@path = path
		# }}}
	end

	# Modifies the domain value of this cookie. The client will send this cookie
	# only if it's connected with this domain (or a subdomain, if the first
	# character is a dot like in ".ruby-lang.org")
	#
	# The value of this attribute must be a String or nil.
	def domain=(domain)
		# {{{
		unless domain.class == String  || domain == nil
			raise TypeError, "The domain of a cookie must be a String or nil.", caller
		end
		@domain = domain
		# }}}
	end

	# Modifies the secure flag of this cookie. If it's true, the client will only
	# send this cookie if it is secured connected with us.
	#
	# The value od this attribute has to be true or false.
	def secure=(secure)
		# {{{
		unless secure == true  || secure == false
			raise TypeError, "The secure field of a cookie must be true or false", caller
		end
		@secure = secure
		# }}}
	end

	# Modifies the comment value of this cookie. The comment won't be send, if
	# type is "netscape".
	def comment=(comment)
		# {{{
		unless comment.class == String || comment == nil
			raise TypeError, "The comment of a cookie must be a string or nil", caller
		end
		@comment = comment
		# }}}
	end

	# Changes the type of all cookies.
	# Allowed values are RFC2109 and netscape (default).
	def Cookie.type=(type)
		# {{{
		unless @@allowed
			raise "The cookies are allready send, so you can't change the type anymore."
		end
		unless type.downcase == "rfc2109" && type.downcase == "netscape"
			raise "The type of the cookies must be \"RFC2109\" or \"netscape\"."
		end
		@@type = type;
		# }}}
	end

	# After sending this message, no cookies can be set or modified. Use it, when
	# HTTP-Headers are send. Rweb does this for you.
	def Cookie.disallow
		# {{{
		@@allowed = false
		true
		# }}}
	end

	# Returns a HTTP header (type String) with all cookies. Rweb does this for
	# you.
	def Cookie.getHttpHeader
		# {{{
		if defined?(@@list)
			if @@type == "netscape"
				str = ""
				@@list.each do |cookie|
					if cookie.value == nil
						cookie.maxage = 0
						cookie.value = ""
					end
					# TODO: Name and value should be escaped!
					str += "Set-Cookie: #{cookie.name}=#{cookie.value}"
					unless cookie.maxage == nil
						expire = Time.now + cookie.maxage
						expire.gmtime
						str += "; Expire=#{expire.strftime("%a, %d-%b-%Y %H:%M:%S %Z")}"
					end
					unless cookie.domain == nil
						str += "; Domain=#{cookie.domain}"
					end
					unless cookie.path == nil
						str += "; Path=#{cookie.path}"
					end
					if cookie.secure
						str += "; Secure"
					end
					str += "\r\n"
				end
				return str
			else # type == "RFC2109"
				str = "Set-Cookie: "
				comma = false;

				@@list.each do |cookie|
					if cookie.value == nil
						cookie.maxage = 0
						cookie.value = ""
					end
					if comma
						str += ","
					end
					comma = true

					str += "#{cookie.name}=\"#{cookie.value}\""
					unless cookie.maxage == nil
						str += "; Max-Age=\"#{cookie.maxage}\""
					end
					unless cookie.domain == nil
						str += "; Domain=\"#{cookie.domain}\""
					end
					unless cookie.path == nil
						str += "; Path=\"#{cookie.path}\""
					end
					if cookie.secure
						str += "; Secure"
					end
					unless cookie.comment == nil
						str += "; Comment=\"#{cookie.comment}\""
					end
					str += "; Version=\"1\""
				end
				str
			end
		else
			false
		end
		# }}}
	end
end

require 'strscan'

module BBCode
	DEBUG = true

	use 'encoder', 'tags', 'tagstack', 'smileys'

=begin
	The Parser class takes care of the encoding.
	It scans the given BBCode (as plain text), finds tags
	and smilies and also makes links of urls in text.

	Normal text is send directly to the encoder.

	If a tag was found, an instance of a Tag subclass is created
	to handle the case.

	The @tagstack manages tag nesting and ensures valid HTML.
=end

	class Parser
		class Attribute
			# flatten and use only one empty_arg
			def self.create attr
				attr = flatten attr
				return @@empty_attr if attr.empty?
				new attr
			end

			private_class_method :new

			# remove leading and trailing whitespace; concat lines
			def self.flatten attr
				attr.strip.gsub(/\n/, ' ')
				# -> ^ and $ can only match at begin and end now
			end

			ATTRIBUTE_SCAN = /
				(?!$)  # don't match at end
				\s*
				( # $1 = key
					[^=\s\]"\\]*
					(?:
						(?: \\. | "[^"\\]*(?:\\.[^"\\]*)*"? )
						[^=\s\]"\\]*
					)*
				)
				(?:
					=
					( # $2 = value
						[^\s\]"\\]*
						(?:
							(?: \\. | "[^"\\]*(?:\\.[^"\\]*)*"? )
							[^\s\]"\\]*
						)*
					)?
				)?
				\s*
			/x

			def self.parse source
				source = source.dup
				# empty_tag: the tag looks like [... /]
				# slice!: this deletes the \s*/] at the end
				# \s+ because [url=http://rubybb.org/forum/] is NOT an empty tag.
				# In RubyBBCode, you can use [url=http://rubybb.org/forum/ /], and this has to be
				# interpreted correctly.
				empty_tag = source.sub!(/^:/, '=') or source.slice!(/\/$/)
				debug 'PARSE: ' + source.inspect + ' => ' + empty_tag.inspect
				#-> we have now an attr that's EITHER empty OR begins and ends with non-whitespace.

				attr = Hash.new
				attr[:flags] = []
				source.scan(ATTRIBUTE_SCAN) { |key, value|
					if not value
						attr[:flags] << unescape(key)
					else
						next if value.empty? and key.empty?
						attr[unescape(key)] = unescape(value)
					end
				}
				debug attr.inspect

				return empty_tag, attr
			end

			def self.unescape_char esc
				esc[1]
			end

			def self.unquote qt
				qt[1..-1].chomp('"').gsub(/\\./) { |esc| unescape_char esc }
			end

			def self.unescape str
				str.gsub(/ (\\.) | (" [^"\\]* (?:\\.[^"\\]*)* "?) /x) {
					if $1
						unescape_char $1
					else
						unquote $2
					end
				}
			end

			include Enumerable
			def each &block
				@args.each(&block)
			end

			attr_reader :source, :args, :value

			def initialize source
				@source = source
				debug 'Attribute#new(%p)' % source
				@empty_tag, @attr = Attribute.parse source
				@value = @attr[''].to_s
			end

			def empty?
				self == @@empty_attr
			end

			def empty_tag?
				@empty_tag
			end

			def [] *keys
				res = @attr[*keys]
			end

			def flags
				attr[:flags]
			end

			def to_s
				@attr
			end

			def inspect
				'ATTR[' + @attr.inspect + (@empty_tag ? ' | empty tag' : '') + ']'
			end
		end
		class Attribute
			@@empty_attr = new ''
		end
	end

