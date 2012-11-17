import System
import Boo.Lang.Interpreter from Boo.Lang.Interpreter

class ObjectInterpreter(AbstractInterpreter):

        _context as object

        [getter(Value)]
        _value as object

        def constructor(context):
            _context = context
            self.RememberLastValue = true

        override def Lookup(name as string):
            property = _context.GetType().GetProperty(name)
            return property.PropertyType if property is not null

        override def GetValue(name as string):
            return _context.GetType().GetProperty(name).GetValue(
                                          _context, null)

        override def SetLastValue(value):
            _value = value

        override def SetValue(name as string, value):
            raise InvalidOperationException()

        override def Declare(name as string, type as Type):
            raise InvalidOperationException()

class Person:
        [property(FirstName)]
        _fname as string = ""

p = Person(FirstName: "Homer")
i = ObjectInterpreter(p)
i.Eval('"Hello, ${FirstName.ToUpper()}!"')
print i.Value
