
;foobar!

;Include "blurg/blurg.bb"

Const ca = $10000000 ; Hex
Const cb = %10101010 ; Binary
Global ga$ = "blargh"
Local a = 124, b$ = "abcdef"

Function name_123#(zorp$, ll = False, blah#, waffles% = 100)
	Return 235.7804 ; comment
End Function
Function TestString$()
End Function

Function hub(blah$, abc = Pi)
End Function
Function Blar%()
	Local aa %, ab # ,ac #, ad# ,ae$,af% ; Intentional mangling
	Local ba#, bb.TBlarf , bc%,bd#,be. TFooBar,ff = True
End Function

abc()

Function abc()
	Print "abc"	; I cannot find a way to parse these as function calls without messing something up
	Print		; Anyhow, they're generally not used in this way
	Goto Eww_Goto
	.Eww_Goto
End Function

Type TBlarf
End Type

Type TFooBar
End Type

Local myinst.MyClass = New MyClass
TestMethod(myinst)

Type MyClass
	
	Field m_foo.MyClass
	Field m_bar.MyClass
	
;	abc
;	def
End Type

Function TestMethod(self.MyClass) ; foobar
	self\m_foo = self
	self\m_bar = Object.MyClass(Handle self\m_foo)
	Yell self\m_foo\m_bar\m_foo\m_bar
End Function

Function Yell(self.MyClass)
	Print("huzzah!")
End Function

Function Wakka$(foo$)
	Return foo + "bar"
End Function


Print("blah " + "blah " + "blah.")

Local i : For i = 0 To 10 Step 1
	Print("Index: " + i)
Next
Local array$[5]
array[0] = "foo": array[1] = "bar":array[2] = "11":array[3] = "22":array[4] = "33"
For i = 0 To 4
	Local value$ = array[i]
	Print("Value: " + value)
Next

Local foobar = Not (1 Or (2 And (4 Shl 5 Shr 6)) Sar 7) Mod (8+2)
Local az = 1234567890
az = az + 1
az = az - 2
az  = az* 3
az = az/ 4
az = az And 5
az = az Or 6
az= ~ 7
az  = az Shl 8
az= az  Shr 9
az  = az Sar 10
az = az Mod 11
az = ((10-5+2/4*2)>(((8^2)) < 2)) And 12 Or 2


;~IDEal Editor Parameters:
;~C#Blitz3D