/**
 * This is not really a valid Haxe file, but just an demo...
 */

package;
package net.onthewings;

import net.onthewings.Test;
import net.onthewings.*;

using Lambda;
using net.onthewings.Test;

#if flash8
// Haxe code specific for flash player 8
#elseif flash
// Haxe code specific for flash platform (any version)
#elseif js
// Haxe code specific for javascript plaform
#elseif neko
// Haxe code specific for neko plaform
#else 
// do something else
    #error  // will display an error "Not implemented on this platform"
    #error "Custom error message" // will display an error "Custom error message"
#end

0; // Int
-134; // Int
0xFF00; // Int

123.0; // Float
.14179; // Float
13e50; // Float
-1e-99; // Float

"hello"; // String
"hello \"world\" !"; // String
'hello "world" !'; // String

true; // Bool
false; // Bool

null; // Unknown<0>

~/[a-z]+/i; // EReg : regular expression

var point = { "x" : 1, "y" : -5 };

{
    var x;
    var y = 3;
    var z : String;
    var w : String = "";
    var a, b : Bool, c : Int = 0;
}

//haxe3 pattern matching
switch(e.expr) {
	case EConst(CString(s)) if (StringTools.startsWith(s, "foo")):
		"1";
	case EConst(CString(s)) if (StringTools.startsWith(s, "bar")):
		"2";
	case EConst(CInt(i)) if (switch(Std.parseInt(i) * 2) { case 4: true; case _: false; }):
		"3";
	case EConst(_):
		"4";
	case _:
		"5";
}

switch [true, 1, "foo"] {
	case [true, 1, "foo"]: "0";
	case [true, 1, _]: "1";
	case _: "_";
}


class Test <T:Void->Void> {
	private function new():Void {
		inline function innerFun(a:Int, b:Int):Int {
			return readOnlyField = a + b;
		}
		
		_innerFun(1, 2.3);
	}
	
	static public var instance(get,null):Test;
	static function get_instance():Test {
		return instance != null ? instance : instance = new Test();
	}
}

@:native("Test") private class Test2 {}

extern class Ext {}

@:macro class M {
	@:macro static function test(e:Array<Expr>):ExprOf<String> {
		return macro "ok";
	}
}

enum Color {
    Red;
    Green;
    Blue;
    Grey( v : Int );
    Rgb( r : Int, g : Int, b : Int );
    Alpha( a : Int, col : Color );
}

class Colors {
    static function toInt( c : Color ) : Int {
        return switch( c ) {
            case Red: 0xFF0000;
            case Green: 0x00FF00;
            case Blue: 0x0000FF;
            case Grey(v): (v << 16) | (v << 8) | v;
            case Rgb(r,g,b): (r << 16) | (g << 8) | b;
            case Alpha(a,c): (a << 24) | (toInt(c) & 0xFFFFFF);
        }
    }
}

class EvtQueue<T : (Event, EventDispatcher)> {
    var evt : T;
}

typedef DS = Dynamic<String>;
typedef Pt = {
	var x:Float;
	var y:Float;
	@:optional var z:Float; /* optional z */
	function add(pt:Pt):Void;
}
typedef Pt2 = {
	x:Float,
	y:Float,
	?z:Float, //optional z
	add : Point -> Void,
}


//top-level class members
public function test();
private var attr(get, set) = 1;


//pre-proc number
public static inline function indexOf<T>(arr:Array<T>, v:T) : Int
{
	#if (haxe_ver >= 3.1) 
	return arr.indexOf(v);
	#else
		#if (flash || js)
		return untyped arr.indexOf(v);
		#else
		return std.Lambda.indexOf(arr, v);
		#end
	#end
}

//macro reification
var e = macro var $myVar = 0;
var e = macro ${v}.toLowerCase();
var e = macro o.$myField;
var e = macro { $myField : 0 };
var e = macro $i{varName}++;
var e = macro $v{myStr};
var args = [macro "sub", macro 3];
var e = macro "Hello".toLowerCase($a{args});
(macro $i{tmp}.addAtom($v{name}, $atom)).finalize(op.pos);

var c = macro class MyClass {
    public function new() { }
    public function $funcName() {
        trace($v{funcName} + " was called");
    }
}

var c = macro interface IClass {};

//macro class could have no name...
var def = macro class {
	private inline function new(loader) this = loader;
	private var loader(get,never) : $loaderType;
	inline private function get_loader() : $loaderType return this;
};

//ECheckType
var f = (123:Float);