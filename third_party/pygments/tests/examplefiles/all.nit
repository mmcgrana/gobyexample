# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Copyright 2013 Alexis Laferrière <alexis.laf@xymus.net>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

import gtk

class CalculatorContext
	var result : nullable Float = null

	var last_op : nullable Char = null

	var current : nullable Float = null
	var after_point : nullable Int = null

	fun push_op( op : Char )
	do
		apply_last_op_if_any
		if op == 'C' then
			self.result = 0.0
			last_op = null
		else
			last_op = op # store for next push_op
		end

		# prepare next current
		after_point = null
		current = null
	end

	fun push_digit( digit : Int )
	do
		var current = current
		if current == null then current = 0.0

		var after_point = after_point
		if after_point == null then
			current = current * 10.0 + digit.to_f
		else
			current = current + digit.to_f * 10.0.pow(after_point.to_f)
			self.after_point -= 1
		end

		self.current = current
	end

	fun switch_to_decimals
	do
		if self.current == null then current = 0.0
		if after_point != null then return

		after_point = -1
	end

	fun apply_last_op_if_any
	do
		var op = last_op

		var result = result
		if result == null then result = 0.0

		var current = current
		if current == null then current = 0.0

		if op == null then
			result = current
		else if op == '+' then
			result = result + current
		else if op == '-' then
			result = result - current
		else if op == '/' then
			result = result / current
		else if op == '*' then
			result = result * current
		end
		self.result = result
		self.current = null
	end
end

class CalculatorGui
	super GtkCallable

	var win : GtkWindow
	var container : GtkGrid

	var lbl_disp : GtkLabel
	var but_eq : GtkButton
	var but_dot : GtkButton

	var context = new CalculatorContext

	redef fun signal( sender, user_data )
	do
		var after_point = context.after_point
		if after_point == null then 
		    after_point = 0
		else
		    after_point = (after_point.abs)
		end
		
		if user_data isa Char then # is an operation
			var c = user_data
			if c == '.' then
				but_dot.sensitive= false
				context.switch_to_decimals
				lbl_disp.text = "{context.current.to_i}."
			else
				but_dot.sensitive= true
				context.push_op( c )
				
				var s = context.result.to_precision_native(6)
				var index : nullable Int = null
				for i in s.length.times do
				    var chiffre = s.chars[i]
				    if chiffre == '0' and index == null then
					index = i
				    else if chiffre != '0' then
					index = null
				    end
				end
				if index != null then
					s = s.substring(0, index)
					if s.chars[s.length-1] == ',' then s = s.substring(0, s.length-1)
				end
				lbl_disp.text = s
			end
		else if user_data isa Int then # is a number
			var n = user_data
			context.push_digit( n )
			lbl_disp.text = context.current.to_precision_native(after_point)
		end
	end

	init
	do
		init_gtk

		win = new GtkWindow( 0 )

		container = new GtkGrid(5,5,true)
		win.add( container )

		lbl_disp = new GtkLabel( "_" )
		container.attach( lbl_disp, 0, 0, 5, 1 )

		# digits
		for n in [0..9] do
			var but = new GtkButton.with_label( n.to_s )
			but.request_size( 64, 64 )
			but.signal_connect( "clicked", self, n )
			if n == 0 then
				container.attach( but, 0, 4, 1, 1 )
			else container.attach( but, (n-1)%3, 3-(n-1)/3, 1, 1 )
		end

		# operators
		var r = 1
		for op in ['+', '-', '*', '/' ] do
			var but = new GtkButton.with_label( op.to_s )
			but.request_size( 64, 64 )
			but.signal_connect( "clicked", self, op )
			container.attach( but, 3, r, 1, 1 )
			r+=1
		end

		# =
		but_eq = new GtkButton.with_label( "=" )
		but_eq.request_size( 64, 64 )
		but_eq.signal_connect( "clicked", self, '=' )
		container.attach( but_eq, 4, 3, 1, 2 )

		# .
		but_dot = new GtkButton.with_label( "." )
		but_dot.request_size( 64, 64 )
		but_dot.signal_connect( "clicked", self, '.' )
		container.attach( but_dot, 1, 4, 1, 1 )

		#C
		var but_c =  new GtkButton.with_label( "C" )
		but_c.request_size( 64, 64 )
		but_c.signal_connect("clicked", self, 'C')
		container.attach( but_c, 2, 4, 1, 1 )

		win.show_all
	end
end

# context tests
var context = new CalculatorContext
context.push_digit( 1 )
context.push_digit( 2 )
context.push_op( '+' )
context.push_digit( 3 )
context.push_op( '*' )
context.push_digit( 2 )
context.push_op( '=' )
var r = context.result.to_precision( 2 )
assert r == "30.00" else print r

context = new CalculatorContext
context.push_digit( 1 )
context.push_digit( 4 )
context.switch_to_decimals
context.push_digit( 1 )
context.push_op( '*' )
context.push_digit( 3 )
context.push_op( '=' )
r = context.result.to_precision( 2 )
assert r == "42.30" else print r

context.push_op( '+' )
context.push_digit( 1 )
context.push_digit( 1 )
context.push_op( '=' )
r = context.result.to_precision( 2 )
assert r == "53.30" else print r

context = new CalculatorContext
context.push_digit( 4 )
context.push_digit( 2 )
context.switch_to_decimals
context.push_digit( 3 )
context.push_op( '/' )
context.push_digit( 3 )
context.push_op( '=' )
r = context.result.to_precision( 2 )
assert r == "14.10" else print r

#test multiple decimals
context = new CalculatorContext
context.push_digit( 5 )
context.push_digit( 0 )
context.switch_to_decimals
context.push_digit( 1 )
context.push_digit( 2 )
context.push_digit( 3 )
context.push_op( '+' )
context.push_digit( 1 )
context.push_op( '=' )
r = context.result.to_precision( 3 )
assert r == "51.123" else print r

#test 'C' button
context = new CalculatorContext
context.push_digit( 1 )
context.push_digit( 0 )
context.push_op( '+' )
context.push_digit( 1 )
context.push_digit( 0 )
context.push_op( '=' )
context.push_op( 'C' )
r = context.result.to_precision( 1 )
assert r == "0.0" else print r

# graphical application

if "NIT_TESTING".environ != "true" then
	var app = new CalculatorGui
	run_gtk
end
# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Copyright 2013 Matthieu Lucas <lucasmatthieu@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# This sample has been implemented to show you how simple is it to play 
# with native callbacks (C) through an high level with NIT program.

module callback_chimpanze
import callback_monkey

class Chimpanze
	super MonkeyActionCallable

	fun create
	do
		var monkey = new Monkey
		print "Hum, I'm sleeping ..."
		# Invoking method which will take some time to compute, and 
		# will be back in wokeUp method with information.
		# - Callback method defined in MonkeyActionCallable Interface
		monkey.wokeUpAction(self, "Hey, I'm awake.")
	end

	# Inherit callback method, defined by MonkeyActionCallable interface
	# - Back of wokeUpAction method 
	redef fun wokeUp( sender:Monkey, message:Object )
	do
		print message
	end
end

var m = new Chimpanze
m.create
# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Copyright 2013 Matthieu Lucas <lucasmatthieu@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# This sample has been implemented to show you how simple is it to play 
# with native callbacks (C) through an high level with NIT program.

module callback_monkey

in "C header" `{
	#include <stdio.h>
	#include <stdlib.h>

	typedef struct { 
		int id;
		int age;
	} CMonkey;

	typedef struct {
		MonkeyActionCallable toCall;
		Object message;
	} MonkeyAction;
`}

in "C body" `{
	// Method which reproduce a callback answer
	// Please note that a function pointer is only used to reproduce the callback
	void cbMonkey(CMonkey *mkey, void callbackFunc(CMonkey*, MonkeyAction*), MonkeyAction *data)
	{
		sleep(2);
		callbackFunc( mkey, data );
	}

	// Back of background treatment, will be redirected to callback function
	void nit_monkey_callback_func( CMonkey *mkey, MonkeyAction *data )
	{
		// To call a your method, the signature must be written like this :
		// <Interface Name>_<Method>...
		MonkeyActionCallable_wokeUp( data->toCall, mkey, data->message );
	}
`}

# Implementable interface to get callback in defined methods
interface MonkeyActionCallable
	fun wokeUp( sender:Monkey, message: Object) is abstract
end

# Defining my object type Monkey, which is, in a low level, a pointer to a C struct (CMonkey)
extern class Monkey `{ CMonkey * `}
	
	new `{
		CMonkey *monkey = malloc( sizeof(CMonkey) );
		monkey->age = 10;
		monkey->id = 1;
		return monkey;
	`}
	
	# Object method which will get a callback in wokeUp method, defined in MonkeyActionCallable interface
	# Must be defined as Nit/C method because of C call inside
	fun wokeUpAction( toCall: MonkeyActionCallable, message: Object ) is extern import MonkeyActionCallable.wokeUp `{

		// Allocating memory to keep reference of received parameters :
		// - Object receiver
		// - Message 
		MonkeyAction *data = malloc( sizeof(MonkeyAction) );

		// Incrementing reference counter to prevent from releasing
		MonkeyActionCallable_incr_ref( toCall );
		Object_incr_ref( message );
		
		data->toCall = toCall;
		data->message = message;
		
		// Calling method which reproduce a callback by passing :
		// - Receiver
		// - Function pointer to object return method
		// - Datas
		cbMonkey( recv, &nit_monkey_callback_func, data );
	`}
end
# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Implementation of circular lists
# This example shows the usage of generics and somewhat a specialisation of collections.
module circular_list

# Sequences of elements implemented with a double-linked circular list
class CircularList[E]
	# Like standard Array or LinkedList, CircularList is a Sequence.
	super Sequence[E]

	# The first node of the list if any
	# The special case of an empty list is handled by a null node
	private var node: nullable CLNode[E] = null

	redef fun iterator do return new CircularListIterator[E](self)

	redef fun first do return self.node.item

	redef fun push(e)
	do
		var new_node = new CLNode[E](e)
		var n = self.node
		if n == null then
			# the first node
			self.node = new_node
		else
			# not the first one, so attach nodes correctly.
			var old_last_node = n.prev
			new_node.next = n
			new_node.prev = old_last_node
			old_last_node.next = new_node
			n.prev = new_node
		end
	end

	redef fun pop
	do
		var n = self.node
		assert n != null
		var prev = n.prev
		if prev == n then
			# the only node
			self.node = null
			return n.item
		end
		# not the only one do detach nodes correctly.
		var prev_prev = prev.prev
		n.prev = prev_prev
		prev_prev.next = n
		return prev.item
	end

	redef fun unshift(e)
	do
		# Circularity has benefits.
		push(e)
		self.node = self.node.prev
	end

	redef fun shift
	do
		# Circularity has benefits.
		self.node = self.node.next
		return self.pop
	end

	# Move the first at the last position, the second at the first, etc.
	fun rotate
	do
		var n = self.node
		if n == null then return
		self.node = n.next
	end

	# Sort the list using the Josephus algorithm.
	fun josephus(step: Int)
	do
		var res = new CircularList[E]
		while not self.is_empty do
			# count 'step'
			for i in [1..step[ do self.rotate
			# kill
			var x = self.shift
			res.add(x)
		end
		self.node = res.node
	end
end

# Nodes of a CircularList
private class CLNode[E]
	# The current item
	var item: E

	# The next item in the circular list.
	# Because of circularity, there is always a next;
	# so by default let it be self
	var next: CLNode[E] = self

	# The previous item in the circular list.
	# Coherence between next and previous nodes has to be maintained by the
	# circular list.
	var prev: CLNode[E] = self
end

# An iterator of a CircularList.
private class CircularListIterator[E]
	super IndexedIterator[E]

	redef var index: Int

	# The current node pointed.
	# Is null if the list is empty.
	var node: nullable CLNode[E]

	# The list iterated.
	var list: CircularList[E]

	redef fun is_ok
	do
		# Empty lists are not OK.
		# Pointing again the first node is not OK.
		return self.node != null and (self.index == 0 or self.node != self.list.node)
	end

	redef fun next
	do
		self.node = self.node.next
		self.index += 1
	end

	redef fun item do return self.node.item

	init(list: CircularList[E])
	do
		self.node = list.node
		self.list = list
		self.index = 0
	end
end

var i = new CircularList[Int]
i.add_all([1, 2, 3, 4, 5, 6, 7])
print i.first
print i.join(":")

i.push(8)
print i.shift
print i.pop
i.unshift(0)
print i.join(":")

i.josephus(3)
print i.join(":")
# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# This module beef up the clock module by allowing a clock to be comparable.
# It show the usage of class refinement
module clock_more

import clock

redef class Clock
	# Clock are now comparable
	super Comparable

	# Comparaison of a clock make only sense with an other clock
	redef type OTHER: Clock

	redef fun <(o)
	do
		# Note: < is the only abstract method of Comparable.
		#       All other operators and methods rely on < and ==.
		return self.total_minutes < o.total_minutes
	end
end

var c1 = new Clock(8, 12)
var c2 = new Clock(8, 13)
var c3 = new Clock(9, 13)

print "{c1}<{c2}? {c1<c2}"
print "{c1}<={c2}? {c1<=c2}"
print "{c1}>{c2}? {c1>c2}"
print "{c1}>={c2}? {c1>=c2}"
print "{c1}<=>{c2}? {c1<=>c2}"
print "{c1},{c2}? max={c1.max(c2)} min={c1.min(c2)}"
print "{c1}.is_between({c2}, {c3})? {c1.is_between(c2, c3)}"
print "{c2}.is_between({c1}, {c3})? {c2.is_between(c1, c3)}"

print "-"

c1.minutes += 1

print "{c1}<{c2}? {c1<c2}"
print "{c1}<={c2}? {c1<=c2}"
print "{c1}>{c2}? {c1>c2}"
print "{c1}>={c2}? {c1>=c2}"
print "{c1}<=>{c2}? {c1<=>c2}"
print "{c1},{c2}? max={c1.max(c2)} min={c1.min(c2)}"
print "{c1}.is_between({c2}, {c3})? {c1.is_between(c2, c3)}"
print "{c2}.is_between({c1}, {c3})? {c2.is_between(c1, c3)}"
# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# This module provide a simple wall clock.
# It is an example of getters and setters.
# A beefed-up module is available in clock_more
module clock

# A simple wall clock with 60 minutes and 12 hours.
class Clock
	# total number of minutes from 0 to 719
	var total_minutes: Int
	# Note: only the read acces is public, the write access is private.

	# number of minutes in the current hour (from 0 to 59)
	fun minutes: Int do return self.total_minutes % 60
	
	# set the number of minutes in the current hour.
	# if m < 0 or m >= 60, the hour will be changed accordinlgy
	fun minutes=(m: Int) do self.total_minutes = self.hours * 60 + m

	# number of hours (from 0 to 11)
	fun hours: Int do return self.total_minutes / 60

	# set the number of hours
	# the minutes will not be updated
	fun hours=(h: Int) do self.total_minutes = h * 60 + minutes

	# the position of the hour arrow in the [0..60[ interval
	fun hour_pos: Int do return total_minutes / 12

	# replace the arrow of hours (from 0 to 59).
	# the hours and the minutes will be updated.
	fun hour_pos=(h: Int) do self.total_minutes = h * 12

	redef fun to_s do return "{hours}:{minutes}"

	fun reset(hours, minutes: Int) do self.total_minutes = hours*60 + minutes

	init(hours, minutes: Int) do self.reset(hours, minutes)

	redef fun ==(o)
	do
		# Note: o is a nullable Object, a type test is required
		# Thanks to adaptive typing, there is no downcast
		# i.e. the code is safe!
		return o isa Clock and self.total_minutes == o.total_minutes
	end
end

var c = new Clock(10,50)
print "It's {c} o'clock."

c.minutes += 22
print "Now it's {c} o'clock."

print "The short arrow in on the {c.hour_pos/5} and the long arrow in on the {c.minutes/5}."

c.hours -= 2
print "Now it's {c} o'clock."

var c2 = new Clock(9, 11)
print "It's {c2} on the second clock."
print "The two clocks are synchronized: {c == c2}."
c2.minutes += 1
print "It's now {c2} on the second clock."
print "The two clocks are synchronized: {c == c2}."
# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Copyright 2013 Matthieu Lucas <lucasmatthieu@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Sample of the Curl module.
module curl_http

import curl

# Small class to represent an Http Fetcher
class MyHttpFetcher
	super CurlCallbacks

	var curl: Curl
	var our_body: String = ""

	init(curl: Curl) do self.curl = curl

	# Release curl object
	fun destroy do self.curl.destroy

	# Header callback
	redef fun header_callback(line: String) do
		# We keep this callback silent for testing purposes
		#if not line.has_prefix("Date:") then print "Header_callback : {line}"
	end

	# Body callback
	redef fun body_callback(line: String) do self.our_body = "{self.our_body}{line}"

	# Stream callback - Cf : No one is registered
	redef fun stream_callback(buffer: String, size: Int, count: Int) do print "Stream_callback : {buffer} - {size} - {count}"
end


# Program
if args.length < 2 then
	print "Usage: curl_http <method wished [POST, GET, GET_FILE]> <target url>"
else
	var curl = new Curl
	var url = args[1]
	var request = new CurlHTTPRequest(url, curl)

	# HTTP Get Request
	if args[0] == "GET" then
		request.verbose = false
		var getResponse = request.execute

		if getResponse isa CurlResponseSuccess then
			print "Status code : {getResponse.status_code}"
			print "Body : {getResponse.body_str}"
		else if getResponse isa CurlResponseFailed then
			print "Error code : {getResponse.error_code}"
			print "Error msg : {getResponse.error_msg}"
		end

	# HTTP Post Request
	else if args[0] == "POST" then
		var myHttpFetcher = new MyHttpFetcher(curl)
		request.delegate = myHttpFetcher

		var postDatas = new HeaderMap
		postDatas["Bugs Bunny"] = "Daffy Duck"
		postDatas["Batman"] = "Robin likes special characters @#ùà!è§'(\"é&://,;<>∞~*"
		postDatas["Batman"] = "Yes you can set multiple identical keys, but APACHE will consider only once, the last one"
		request.datas = postDatas
		request.verbose = false
		var postResponse = request.execute

		print "Our body from the callback : {myHttpFetcher.our_body}"

		if postResponse isa CurlResponseSuccess then
			print "*** Answer ***"
			print "Status code : {postResponse.status_code}"
			print "Body should be empty, because we decided to manage callbacks : {postResponse.body_str.length}"
		else if postResponse isa CurlResponseFailed then
			print "Error code : {postResponse.error_code}"
			print "Error msg : {postResponse.error_msg}"
		end

	# HTTP Get to file Request
	else if args[0] == "GET_FILE" then
		var headers = new HeaderMap
		headers["Accept"] = "Moo"
		request.headers = headers
		request.verbose = false
		var downloadResponse = request.download_to_file(null)

		if downloadResponse isa CurlFileResponseSuccess then
			print "*** Answer ***"
			print "Status code : {downloadResponse.status_code}"
			print "Size downloaded : {downloadResponse.size_download}"
		else if downloadResponse isa CurlResponseFailed then
			print "Error code : {downloadResponse.error_code}"
			print "Error msg : {downloadResponse.error_msg}"
		end
	# Program logic
	else
		print "Usage : Method[POST, GET, GET_FILE]"
	end
end
# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Copyright 2013 Matthieu Lucas <lucasmatthieu@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Mail sender sample using the Curl module
module curl_mail

import curl

var curl = new Curl
var mail_request = new CurlMailRequest(curl)

# Networks
var response = mail_request.set_outgoing_server("smtps://smtp.example.org:465", "user@example.org", "mypassword")
if response isa CurlResponseFailed then
	print "Error code : {response.error_code}"
	print "Error msg : {response.error_msg}"
end

# Headers
mail_request.from = "Billy Bob"
mail_request.to = ["user@example.org"]
mail_request.cc = ["bob@example.org"]
mail_request.bcc = null

var headers_body = new HeaderMap
headers_body["Content-Type:"] = "text/html; charset=\"UTF-8\""
headers_body["Content-Transfer-Encoding:"] = "quoted-printable"
mail_request.headers_body = headers_body

# Content
mail_request.body = "<h1>Here you can write HTML stuff.</h1>"
mail_request.subject = "Hello From My Nit Program"

# Others
mail_request.verbose = false

# Send mail
response = mail_request.execute
if response isa CurlResponseFailed then
	print "Error code : {response.error_code}"
	print "Error msg : {response.error_msg}"
else if response isa CurlMailResponseSuccess then
	print "Mail Sent"
else
	print "Unknown Curl Response type"
end
# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Copyright 2012-2013 Alexis Laferrière <alexis.laf@xymus.net>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Draws an arithmetic operation to the terminal
module draw_operation

redef enum Int
	fun n_chars: Int `{
		int c;
		if ( abs(recv) >= 10 )
			c = 1+(int)log10f( (float)abs(recv) );
		else
			c = 1;
		if ( recv < 0 ) c ++;
		return c;
	`}
end

redef enum Char
	fun as_operator(a, b: Int): Int
	do
		if self == '+' then return a + b
		if self == '-' then return a - b
		if self == '*' then return a * b
		if self == '/' then return a / b
		if self == '%' then return a % b
		abort
	end

	fun override_dispc: Bool
	do
		return self == '+' or self == '-' or self == '*' or self == '/' or self == '%'
	end

	fun lines(s: Int): Array[Line]
	do
		if self == '+' then
			return [new Line(new P(0,s/2),1,0,s), new Line(new P(s/2,1),0,1,s-2)]
		else if self == '-' then
			return [new Line(new P(0,s/2),1,0,s)]
		else if self == '*' then
			var lines = new Array[Line]
			for y in [1..s-1[ do
				lines.add( new Line(new P(1,y), 1,0,s-2) )
			end
			return lines
		else if self == '/' then
			return [new Line(new P(s-1,0), -1,1, s )]
		else if self == '%' then
			var q4 = s/4
			var lines = [new Line(new P(s-1,0),-1,1,s)]
			for l in [0..q4[ do
				lines.append([ new Line( new P(0,l), 1,0,q4), new Line( new P(s-1,s-1-l), -1,0,q4) ])
			end
			return lines
		else if self == '1' then
			return [new Line(new P(s/2,0), 0,1,s),new Line(new P(0,s-1),1,0,s),
				new Line( new P(s/2,0),-1,1,s/2)]
		else if self == '2' then
			return [new Line(new P(0,0), 1,0,s),new Line(new P(s-1,0),0,1,s/2),
				new Line( new P(0,s-1),1,0,s), new Line( new P(0,s/2), 0,1,s/2),
				new Line( new P(0,s/2), 1,0,s)]
		else if self == '3' then
			return [new Line(new P(0,0), 1,0,s),new Line(new P(s-1,0),0,1,s),
				new Line( new P(0,s-1),1,0,s), new Line( new P(0,s/2), 1,0,s)]
		else if self == '4' then
			return [new Line(new P(s-1,0),0,1,s), new Line( new P(0,0), 0,1,s/2),
				new Line( new P(0,s/2), 1,0,s)]
		else if self == '5' then
			return [new Line(new P(0,0), 1,0,s),new Line(new P(s-1,s/2),0,1,s/2),
				new Line( new P(0,s-1),1,0,s), new Line( new P(0,0), 0,1,s/2),
				new Line( new P(0,s/2), 1,0,s)]
		else if self == '6' then
			return [new Line(new P(0,0), 1,0,s),new Line(new P(s-1,s/2),0,1,s/2),
				new Line( new P(0,s-1),1,0,s), new Line( new P(0,0), 0,1,s),
				new Line( new P(0,s/2), 1,0,s)]
		else if self == '7' then
			var tl = new P(0,0)
			var tr = new P(s-1,0)
			return [new Line(tl, 1,0,s), new Line(tr,-1,1,s)]
		else if self == '8' then
			return [new Line(new P(0,0), 1,0,s),new Line(new P(s-1,0),0,1,s),
				new Line( new P(0,s-1),1,0,s), new Line( new P(0,0), 0,1,s),
				new Line( new P(0,s/2), 1,0,s)]
		else if self == '9' then
			return [new Line(new P(0,0), 1,0,s),new Line(new P(s-1,0),0,1,s),
				new Line( new P(0,s-1),1,0,s), new Line( new P(0,0), 0,1,s/2),
				new Line( new P(0,s/2), 1,0,s)]
		else if self == '0' then
			return [new Line(new P(0,0), 1,0,s),new Line(new P(s-1,0),0,1,s),
				new Line( new P(0,s-1),1,0,s), new Line( new P(0,0), 0,1,s)]
		end
		return new Array[Line]
	end
end

class P
	var x : Int
	var y : Int
end

redef class String
	# hack is to support a bug in the evaluation software
	fun draw(dispc: Char, size, gap: Int, hack: Bool)
	do
		var w = size * length +(length-1)*gap
		var h = size
		var map = new Array[Array[Char]]
		for x in [0..w[ do
			map[x] = new Array[Char].filled_with( ' ',  h )
		end

		var ci = 0
		for c in self.chars do
			var local_dispc
			if c.override_dispc then
				local_dispc = c
			else
				local_dispc = dispc
			end

			var lines = c.lines( size )
			for line in lines do
				var x = line.o.x+ci*size
					x += ci*gap
				var y = line.o.y
				for s in [0..line.len[ do
					assert map.length > x and map[x].length > y else print "setting {x},{y} as {local_dispc}"
					map[x][y] = local_dispc
					x += line.step_x
					y += line.step_y
				end
			end

			ci += 1
		end

		if hack then
			for c in [0..size[ do
				map[c][0] = map[map.length-size+c][0]
				map[map.length-size+c][0] = ' '
			end
		end

		for y in [0..h[ do
			for x in [0..w[ do
				printn map[x][y]
			end
			print ""
		end
	end
end

class Line
	var o : P
	var step_x : Int
	var step_y : Int
	var len : Int
end

var a
var b
var op_char
var disp_char
var disp_size
var disp_gap

if "NIT_TESTING".environ == "true" then
	a = 567
	b = 13
	op_char = '*'
	disp_char = 'O'
	disp_size = 8
	disp_gap = 1
else
	printn "Left operand: "
	a = gets.to_i

	printn "Right operand: "
	b = gets.to_i

	printn "Operator (+, -, *, /, %): "
	op_char = gets.chars[0]

	printn "Char to display: "
	disp_char = gets.chars[0]

	printn "Size of text: "
	disp_size = gets.to_i

	printn "Space between digits: "
	disp_gap = gets.to_i
end

var result = op_char.as_operator( a, b )

var len_a = a.n_chars
var len_b = b.n_chars
var len_res = result.n_chars
var max_len = len_a.max( len_b.max( len_res ) ) + 1

# draw first line
var d = max_len - len_a
var line_a = ""
for i in [0..d[ do line_a += " "
line_a += a.to_s
line_a.draw( disp_char, disp_size, disp_gap, false )

print ""
# draw second line
d = max_len - len_b-1
var line_b = op_char.to_s
for i in [0..d[ do line_b += " "
line_b += b.to_s
line_b.draw( disp_char, disp_size, disp_gap, false )

# draw -----
print ""
for i in [0..disp_size*max_len+(max_len-1)*disp_gap] do
	printn "_"
end
print ""
print ""

# draw result
d = max_len - len_res
var line_res = ""
for i in [0..d[ do line_res += " "
line_res += result.to_s
line_res.draw( disp_char, disp_size, disp_gap, false )
# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Copyright 2013 Alexis Laferrière <alexis.laf@xymus.net>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Example using the privileges module to drop privileges from root
module drop_privileges

import privileges

# basic command line options
var opts = new OptionContext
var opt_ug = new OptionUserAndGroup.for_dropping_privileges
opt_ug.mandatory = true
opts.add_option(opt_ug)

# parse and check command line options
opts.parse(args)
if not opts.errors.is_empty then
	print opts.errors
	print "Usage: drop_privileges [options]"
	opts.usage
	exit 1
end

# original user
print "before {sys.uid}:{sys.gid}"

# make the switch
var user_group = opt_ug.value
assert user_group != null
user_group.drop_privileges

# final user
print "after {sys.uid}:{sys.egid}"
# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Copyright 2012-2013 Alexis Laferrière <alexis.laf@xymus.net>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# This module illustrates some uses of the FFI, specifically
# how to use extern methods. Which means to implement a Nit method in C.
module extern_methods

redef enum Int
	# Returns self'th fibonnaci number
	# implemented here in C for optimization purposes
	fun fib : Int import fib `{
		if ( recv < 2 )
			return recv;
		else
			return Int_fib( recv-1 ) + Int_fib( recv-2 );
	`}

	# System call to sleep for "self" seconds
	fun sleep `{
		sleep( recv );
	`}

	# Return atan2l( self, x ) from libmath
	fun atan_with( x : Int ) : Float `{
		return atan2( recv, x );
	`}

	# This method callback to Nit methods from C code
	# It will use from C code:
	# * the local fib method
	# * the + operator, a method of Int
	# * to_s, a method of all objects
	# * String.to_cstring, a method of String to return an equivalent char*
	fun foo import fib, +, to_s, String.to_cstring `{
		long recv_fib = Int_fib( recv );
		long recv_plus_fib = Int__plus( recv, recv_fib );

		String nit_string = Int_to_s( recv_plus_fib );
		char *c_string = String_to_cstring( nit_string );

		printf( "from C: self + fib(self) = %s\n", c_string );
	`}

	# Equivalent to foo but written in pure Nit
	fun bar do print "from Nit: self + fib(self) = {self+self.fib}"
end

print 12.fib

print "sleeping 1 second..."
1.sleep

print 100.atan_with( 200 )
8.foo
8.bar

# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Copyright 2004-2008 Jean Privat <jean@pryen.org>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# A simple exemple of refinement where a method is added to the integer class.
module fibonacci

redef class Int
	# Calculate the self-th element of the fibonacci sequence.
	fun fibonacci: Int
	do
		if self < 2 then
			return 1
		else
			return (self-2).fibonacci + (self-1).fibonacci
		end
	end 
end

# Print usage and exit.
fun usage
do
	print "Usage: fibonnaci <integer>" 
	exit 0 
end

# Main part
if args.length != 1 then
	usage
end
print args.first.to_i.fibonacci
print "hello world"
# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

import html

class NitHomepage
	super HTMLPage

	redef fun head do
		add("meta").attr("charset", "utf-8")
		add("title").text("Nit")
		add("link").attr("rel", "icon").attr("href", "http://nitlanguage.org/favicon.ico").attr("type", "image/x-icon")
		add("link").attr("rel", "stylesheet").attr("href", "http://nitlanguage.org/style.css").attr("type", "text/css")
		add("link").attr("rel", "stylesheet").attr("href", "http://nitlanguage.org/local.css").attr("type", "text/css")
	end

	redef fun body do
		open("article").add_class("page")
			open("section").add_class("pageheader")
				add_html("<a id='toptitle_first' class='toptitle'>the</a><a id='toptitle_second' class='toptitle' href=''>Nit</a><a id='toptitle_third' class='toptitle' href=''>Programming Language</a>")
				open("header").add_class("header")
					open("div").add_class("topsubtitle")
						add("p").text("A Fun Language for Serious Programming")
					close("div")
				close("header")
			close("section")

			open("div").attr("id", "pagebody")
				open("section").attr("id", "content")
					add("h1").text("# What is Nit?")
					add("p").text("Nit is an object-oriented programming language. The goal of Nit is to propose a robust statically typed programming language where structure is not a pain.")
					add("p").text("So, what does the famous hello world program look like, in Nit?")
					add_html("<pre><tt><span class='normal'>print </span><span class='string'>'Hello, World!'</span></tt></pre>")

					add("h1").text("# Feature Highlights")
					add("h2").text("Usability")
					add("p").text("Nit's goal is to be usable by real programmers for real projects")

					open("ul")
						open("li")
						add("a").attr("href", "http://en.wikipedia.org/wiki/KISS_principle").text("KISS principle")
						close("li")
						add("li").text("Script-like language without verbosity nor cryptic statements")
						add("li").text("Painless static types: static typing should help programmers")
						add("li").text("Efficient development, efficient execution, efficient evolution.")
					close("ul")

					add("h2").text("Robustness")
					add("p").text("Nit will help you to write bug-free programs")

					open("ul")
						add("li").text("Strong static typing")
						add("li").text("No more NullPointerException")
					close("ul")

					add("h2").text("Object-Oriented")
					add("p").text("Nit's guideline is to follow the most powerful OO principles")

					open("ul")
						open("li")
						add("a").attr("href", "./everything_is_an_object/").text("Everything is an object")
						close("li")
						open("li")
						add("a").attr("href", "./multiple_inheritance/").text("Multiple inheritance")
						close("li")
						open("li")
						add("a").attr("href", "./refinement/").text("Open classes")
						close("li")
						open("li")
						add("a").attr("href", "./virtual_types/").text("Virtual types")
						close("li")
					close("ul")


					add("h1").text("# Getting Started")
					add("p").text("Get Nit from its Git repository:")

					add_html("<pre><code>$ git clone http://nitlanguage.org/nit.git</code></pre>")
					add("p").text("Build the compiler (may be long):")
					add_html("<pre><code>$ cd nit\n")
					add_html("$ make</code></pre>")
					add("p").text("Compile a program:")
					add_html("<pre><code>$ bin/nitc examples/hello_world.nit</code></pre>")
					add("p").text("Execute the program:")
					add_html("<pre><code>$ ./hello_world</code></pre>")
				close("section")
			close("div")
		close("article")
	end
end

var page = new NitHomepage
page.write_to stdout
page.write_to_file("nit.html")
# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# An example that defines and uses stacks of integers.
# The implementation is done with a simple linked list.
# It features: free constructors, nullable types and some adaptive typing.
module int_stack

# A stack of integer implemented by a simple linked list.
# Note that this is only a toy class since a real linked list will gain to use
# generics and extends interfaces, like Collection, from the standard library.
class IntStack
	# The head node of the list.
	# Null means that the stack is empty.
	private var head: nullable ISNode = null

	# Add a new integer in the stack.
	fun push(val: Int)
	do
		self.head = new ISNode(val, self.head)
	end

	# Remove and return the last pushed integer.
	# Return null if the stack is empty.
	fun pop: nullable Int
	do
		var head = self.head
		if head == null then return null
		# Note: the followings are statically safe because of the
		# previous 'if'.
		var val = head.val
		self.head = head.next
		return val
	end

	# Return the sum of all integers of the stack.
	# Return 0 if the stack is empty.
	fun sumall: Int
	do
		var sum = 0
		var cur = self.head
		while cur != null do
			# Note: the followings are statically safe because of
			# the condition of the 'while'.
			sum += cur.val
			cur = cur.next
		end
		return sum
	end

	# Note: Because all attributes have a default value, a free constructor
	# "init()" is implicitly defined.
end

# A node of a IntStack
private class ISNode
	# The integer value stored in the node.
	var val: Int

	# The next node, if any.
	var next: nullable ISNode

	# Note: A free constructor "init(val: Int, next: nullable ISNode)" is
	# implicitly defined.
end

var l = new IntStack
l.push(1)
l.push(2)
l.push(3)

print l.sumall

# Note: the 'for' control structure cannot be used on IntStack in its current state.
# It requires a more advanced topic.
# However, why not using the 'loop' control structure?
loop
	var i = l.pop
	if i == null then break
	# The following is statically safe because of the previous 'if'.
	print i * 10
end

# Note: 'or else' is used to give an alternative of a null expression.
l.push(5)
print l.pop or else 0 # l.pop gives 5, so print 5
print l.pop or else 0 # l.pop gives null, so print the alternative: 0


# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Copyright 2014 Alexis Laferrière <alexis.laf@xymus.net>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Basic example of OpenGL ES 2.0 usage from the book OpenGL ES 2.0 Programming Guide.
#
# Code reference:
# https://code.google.com/p/opengles-book-samples/source/browse/trunk/LinuxX11/Chapter_2/Hello_Triangle/Hello_Triangle.c 
module opengles2_hello_triangle

import glesv2
import egl
import mnit_linux # for sdl
import x11

if "NIT_TESTING".environ == "true" then exit(0)

var window_width = 800
var window_height = 600

#
## SDL
#
var sdl_display = new SDLDisplay(window_width, window_height)
var sdl_wm_info = new SDLSystemWindowManagerInfo
var x11_window_handle = sdl_wm_info.x11_window_handle

#
## X11
#
var x_display = x_open_default_display
assert x_display != 0 else print "x11 fail"

#
## EGL
#
var egl_display = new EGLDisplay(x_display)
assert egl_display.is_valid else print "EGL display is not valid"
egl_display.initialize

print "EGL version: {egl_display.version}"
print "EGL vendor: {egl_display.vendor}"
print "EGL extensions: {egl_display.extensions.join(", ")}"
print "EGL client APIs: {egl_display.client_apis.join(", ")}"

assert egl_display.is_valid else print egl_display.error

var config_chooser = new EGLConfigChooser
#config_chooser.surface_type_egl
config_chooser.blue_size = 8
config_chooser.green_size = 8
config_chooser.red_size = 8
#config_chooser.alpha_size = 8
#config_chooser.depth_size = 8
#config_chooser.stencil_size = 8
#config_chooser.sample_buffers = 1
config_chooser.close

var configs = config_chooser.choose(egl_display)
assert configs != null else print "choosing config failed: {egl_display.error}"
assert not configs.is_empty else print "no EGL config"

print "{configs.length} EGL configs available"
for config in configs do
	var attribs = config.attribs(egl_display)
	print "* caveats: {attribs.caveat}"
	print "  conformant to: {attribs.conformant}"
	print "  size of RGBA: {attribs.red_size} {attribs.green_size} {attribs.blue_size} {attribs.alpha_size}"
	print "  buffer, depth, stencil: {attribs.buffer_size} {attribs.depth_size} {attribs.stencil_size}"
end

var config = configs.first

var format = config.attribs(egl_display).native_visual_id

# TODO android part
# Opengles1Display_midway_init(recv, format);

var surface = egl_display.create_window_surface(config, x11_window_handle, [0])
assert surface.is_ok else print egl_display.error

var context = egl_display.create_context(config)
assert context.is_ok else print egl_display.error

var make_current_res = egl_display.make_current(surface, surface, context)
assert make_current_res

var width = surface.attribs(egl_display).width
var height = surface.attribs(egl_display).height
print "Width: {width}"
print "Height: {height}"

assert egl_bind_opengl_es_api else print "eglBingAPI failed: {egl_display.error}"

#
## GLESv2
#

print "Can compile shaders? {gl_shader_compiler}"
assert_no_gl_error

assert gl_shader_compiler else print "Cannot compile shaders"

# gl program
print gl_error.to_s
var program = new GLProgram
if not program.is_ok then
	print "Program is not ok: {gl_error.to_s}\nLog:"
	print program.info_log
	abort
end
assert_no_gl_error

# vertex shader
var vertex_shader = new GLVertexShader
assert vertex_shader.is_ok else print "Vertex shader is not ok: {gl_error}"
vertex_shader.source = """
attribute vec4 vPosition;   
void main()                 
{                           
  gl_Position = vPosition;  
}                           """
vertex_shader.compile
assert vertex_shader.is_compiled else print "Vertex shader compilation failed with: {vertex_shader.info_log} {program.info_log}"
assert_no_gl_error

# fragment shader
var fragment_shader = new GLFragmentShader
assert fragment_shader.is_ok else print "Fragment shader is not ok: {gl_error}"
fragment_shader.source = """
precision mediump float;
void main()
{
	gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0);
}
"""
fragment_shader.compile
assert fragment_shader.is_compiled else print "Fragment shader compilation failed with: {fragment_shader.info_log}"
assert_no_gl_error

program.attach_shader vertex_shader
program.attach_shader fragment_shader
program.bind_attrib_location(0, "vPosition")
program.link
assert program.is_linked else print "Linking failed: {program.info_log}"
assert_no_gl_error

# draw!
var vertices = [0.0, 0.5, 0.0, -0.5, -0.5, 0.0, 0.5, -0.5, 0.0]
var vertex_array = new VertexArray(0, 3, vertices)
vertex_array.attrib_pointer
gl_clear_color(0.5, 0.0, 0.5, 1.0)
for i in [0..10000[ do
	printn "."
	assert_no_gl_error
	gl_viewport(0, 0, width, height)
	gl_clear_color_buffer
	program.use
	vertex_array.enable
	vertex_array.draw_arrays_triangles
	egl_display.swap_buffers(surface)
end

# delete
program.delete
vertex_shader.delete
fragment_shader.delete

#
## EGL
#
# close
egl_display.make_current(new EGLSurface.none, new EGLSurface.none, new EGLContext.none)
egl_display.destroy_context(context)
egl_display.destroy_surface(surface)

#
## SDL
#
# close
sdl_display.destroy
# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Copyright 2004-2008 Jean Privat <jean@pryen.org>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# How to print arguments of the command line.
module print_arguments

for a in args do
	print a
end
# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Copyright 2004-2008 Jean Privat <jean@pryen.org>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# A procedural program (without explicit class definition).
# This program manipulates arrays of integers.
module procedural_array

# The sum of the elements of `a'.
# Uses a 'for' control structure.
fun array_sum(a: Array[Int]): Int
do
	var sum = 0
	for i in a do
		sum = sum + i
	end
	return sum
end

# The sum of the elements of `a' (alternative version).
# Uses a 'while' control structure.
fun array_sum_alt(a: Array[Int]): Int
do
	var sum = 0
	var i = 0
	while i < a.length do
		sum = sum + a[i]
		i = i + 1
	end
	return sum
end

# The main part of the program.
var a = [10, 5, 8, 9]
print(array_sum(a))
print(array_sum_alt(a))
# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Copyright 2013 Matthieu Lucas <lucasmatthieu@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Client sample using the Socket module which connect to the server sample.
module socket_client

import socket

if args.length < 2 then
	print "Usage : socket_client <host> <port>"
	return
end

var s = new Socket.client(args[0], args[1].to_i)
print "[HOST ADDRESS] : {s.address}"
print "[HOST] : {s.host}"
print "[PORT] : {s.port}"
print "Connecting ... {s.connected}"
if s.connected then
	print "Writing ... Hello server !"
	s.write("Hello server !")
	print "[Response from server] : {s.read(100)}"
	print "Closing ..."
	s.close
end
# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Copyright 2013 Matthieu Lucas <lucasmatthieu@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Server sample using the Socket module which allow client to connect
module socket_server

import socket

if args.is_empty then
	print "Usage : socket_server <port>"
	return
end

var socket = new Socket.server(args[0].to_i, 1)
print "[PORT] : {socket.port.to_s}"

var clients = new Array[Socket]
var max = socket
loop
	var fs = new SocketObserver(true, true, true)
	fs.readset.set(socket)

	for c in clients do fs.readset.set(c)

	if fs.select(max, 4, 0) == 0 then
		print "Error occured in select {sys.errno.strerror}"
		break
	end

	if fs.readset.is_set(socket) then
		var ns = socket.accept
		print "Accepting {ns.address} ... "
		print "[Message from {ns.address}] : {ns.read(100)}"
		ns.write("Goodbye client.")
		print "Closing {ns.address} ..."
		ns.close
	end
end

# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

import template

### Here, definition of the specific templates

# The root template for composers
class TmplComposers
	super Template

	# Short list of composers
	var composers = new Array[TmplComposer]

	# Detailled list of composers
	var composer_details = new Array[TmplComposerDetail]

	# Add a composer in both lists
	fun add_composer(firstname, lastname: String, birth, death: Int)
	do
		composers.add(new TmplComposer(lastname))
		composer_details.add(new TmplComposerDetail(firstname, lastname, birth, death))
	end

	redef fun rendering do
		add """
COMPOSERS
=========
"""
		add_all composers
		add """

DETAILS
=======
"""
		add_all composer_details
	end
end

# A composer in the short list of composers
class TmplComposer
	super Template

	# Short name
	var name: String

	init(name: String) do self.name = name

	redef fun rendering do add "- {name}\n"
end

# A composer in the detailled list of composers
class TmplComposerDetail
	super Template

	var firstname: String
	var lastname: String
	var birth: Int
	var death: Int

	init(firstname, lastname: String, birth, death: Int) do
		self.firstname = firstname
		self.lastname = lastname
		self.birth = birth
		self.death = death
	end

	redef fun rendering do add """

COMPOSER: {{{firstname}}} {{{lastname}}}
BIRTH...: {{{birth}}}
DEATH...: {{{death}}}
"""

end

### Here a simple usage of the templates

var f = new TmplComposers
f.add_composer("Johann Sebastian", "Bach", 1685, 1750)
f.add_composer("George Frideric", "Handel", 1685, 1759)
f.add_composer("Wolfgang Amadeus", "Mozart", 1756, 1791)
f.write_to(stdout)
# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Copyright 2014 Lucas Bajolet <r4pass@hotmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Sample module for a minimal chat server using Websockets on port 8088
module websocket_server

import websocket

var sock = new WebSocket(8088, 1)

var msg: String

if sock.listener.eof then
	print sys.errno.strerror
end

sock.accept

while not sock.listener.eof do
	if not sock.connected then sock.accept
	if sys.stdin.poll_in then
		msg = gets
		printn "Received message : {msg}"
		if msg == "exit" then sock.close
		if msg == "disconnect" then sock.disconnect_client
		sock.write(msg)
	end
	if sock.can_read(10) then
		msg = sock.read_line
		if msg != "" then print msg
	end
end

