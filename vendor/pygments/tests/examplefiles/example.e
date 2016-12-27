note
	description : "[
						This is use to have almost every language element."
						
						That way, I can correctly test the lexer. %]"
						
						Don't try to understand what it does. It's not even compilling.
					]"
	date        : "August 6, 2013"
	revision    : "0.1"

class
	SAMPLE

inherit
	ARGUMENTS
		rename
			Command_line as Caller_command,
			command_name as Application_name
		undefine
			out
		end
	ANY
		export
			{ANY} out
		redefine
			out
		end



create
	make

convert
	as_boolean: {BOOLEAN}

feature {NONE} -- Initialization

	make
			-- Run application.
		local
			i1_:expanded INTEGER
			f_1:REAL_64
			l_char:CHARACTER_8
		do
			l_char:='!'
			l_char:='%''
			l_char:='%%'
			i1_:=80 - 0x2F0C // 0C70 \\ 0b10110 * 1;
			f_1:=0.1 / .567
			f_1:=34.
			f_1:=12345.67890
			inspect i1_
			when 1 then
				io.output.put_integer (i1_)		-- Comment
			else
				io.output.put_real (f_1.truncated_to_real)
			end
			io.output.put_string (CuRrEnt.out)		-- Comment
			(agent funct_1).call([1,2,"Coucou"])
		end

feature -- Access

	funct_1(x,y:separate INTEGER;a_text:READABLE_STRING_GENERAL):detachable BOOLEAN
		obsolete "This function is obsolete"
		require
			Is_Attached: AttAched a_text
		local
			l_list:LIST[like x]
		do
			if (NOT a_text.is_empty=TrUe or elSe ((x<0 aNd x>10) oR (y>0 and then y<10))) xor True thEn
				ResuLT := FalSe
			elseif (acROss l_list as la_list SoMe	la_list.item<0 end) implies a_text.is_boolean then
				ResuLT := FalSe
			else
				Result := TruE
			eND
			from
				l_list.start
			until
				l_list.exhausted
			loop
				l_list.forth
			variant
				l_list.count - l_list.index
			end
			check Current /= Void end
			debug print("%"Here%"%N") end
		ensure
			Is_Cool_Not_Change: is_cool = old is_cool
		end

	is_cool:BOOLEAN
		attribute
			Result:=False
		end

	froZen c_malloc: POINTER is
		exTErnal
			"C inline use <stdlib.h>"
		alIAs
			"malloc (1)"
		end

	as_boolean:BOOLEAN
		do
			Result:=True
		rescue
			retry
		end

feature {ANY} -- The redefine feature

	out:STRING_8
		once
			reSUlt:=PrecursOr {ANY}
			Result := "Hello Worl"+('d').out
		end

invariant
	Always_Cool: is_cool
end
