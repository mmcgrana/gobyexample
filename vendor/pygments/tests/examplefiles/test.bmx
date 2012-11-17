
Rem
foobar
EndRem

Rem
	foobar!
End Rem

Rem
End Rem

SuperStrict

Framework brl.blitz
Import brl.standardio

'Import "blah.bmx"
'Import "blah/blah.bmx"
'Include "blurg/blurg.bmx"

Const ca:Long = $10000000 ' Hex
Const cb:Int = %10101010 ' Binary
Global ga:String = "blargh"
Local a:Int = 124, b$ = "abcdef"
?Not Debug
Print(_name123(ga, a, 100.2))
?

Function _name123  : Float  (zorp:String, ll:Int = False, blah#, waffles% = 100)
	Return 235.7804 ' Single-line comment
End Function
Function TestString:String()
End Function
Function TestByte:Byte()
End Function

Function hub(blah:String, ..
				abc:Int = Pi)
End Function
Function Blar%()
	Local aa !, ab @ ,ac @@, ad# ,ae$,af% ' Intentional mangling
	Local ba:Double, bb :Byte, bc: Short,bd:Float,be: String,ff:Int = True
End Function

?Win32
abc()
?Linux
abc()
?

Function abc()
	Print "abc"	' I cannot find a way to parse these as function calls without messing something up
	Print		' Anyhow, they're generally not used in this way
	Goto Eww_Goto
	#Eww_Goto
End Function

Type TBlarf Abstract
End Type

Type TFooBar
End Type

New MyClass.TestMethod()
New(MyClass).TestMethod()
Local myinst:MyClass = New MyClass
myinst.TestMethod()

Type MyClass Extends TFooBar
	
	Field m_foo:MyClass
	Field m_bar:MyClass
	
	Rem
		abc
		def
	End Rem
	Method New()
		Rem
			abcdef
		endrem
	End Method
	
	Method TestMethod() ' foobar
		m_foo = Self
		m_bar = MyClass(m_foo)
		m_foo.m_bar.m_foo.m_bar.Yell()
	End Method
	
	Method Yell()
		Print("huzzah!")
	End Method
	
	Function Wakka$(foo:String)
		Return foo + "bar"
	End Function
	
End Type

Extern "c"
	Function vesper!(a:Int) = "vesper@4"
	Function bubbles@@(a%)
End Extern

Print("blah " + ..
	"blah " + ..
	"blah.")

Try
	Throw("blar!")
Catch exception:String
	Print("Caught: " + exception)
End Try

For Local i:Int = 0 To 10 Step 1
	Print("Index: " + i)
Next
Local array:String[] = ["foo", "bar", "11", "22", "33"]
For Local value:String = EachIn array
	Print("Value: " + value)
Next

Local foobar:Int = Not (1 Or (2 And (4 Shl 5 Shr 6)) Sar 7) Mod (8+2)
Local az:Int = 1234567890
az : + 1
az: - 2
az :* 3
az:/ 4
az:& 5
az:| 6
az: ~ 7
az : Shl 8
az:  Shr 9
az :Sar 10
az:Mod 11
az = ((10-5+2/4*2)>(((8^2)) < 2)) & 12|2

Function flub(fah Ptr, eah:Int Ptr, blu@@ Ptr)
End Function
Function Foob:Int Ptr(blar:Byte Ptr, Saffon@Ptr, blaus#Ptr)
End Function
Function zauus@Ptr()
End Function

