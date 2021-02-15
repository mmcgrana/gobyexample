&& This is a concatenation of all VFP examples on Wikipedia.
&& Copyright 2013 Wikimedia, under the GFDL.

FOR i = 1 to 10
   x = x + 6.5
ENDFOR
 
IF i = 25
   i = i + 1
ELSE
   i = i + 3
ENDIF
 
x = 1
DO WHILE x < 50
   x =  x + 1
ENDDO
 
x = 1
DO WHILE .T.
  x = x + 1
  IF x < 50
   LOOP
  ELSE
   EXIT
  ENDIF
ENDDO
 
nMonth = MONTH(DATE())
DO CASE
      CASE nMonth <= 3
               MESSAGEBOX("Q1")
 
      CASE nMonth <= 6
               MESSAGEBOX("Q2")
 
      CASE nMonth <= 9
               MESSAGEBOX("Q3")
 
      OTHERWISE
               MESSAGEBOX("Q4")
ENDCASE
 
FOR EACH oControl IN THISFORM.Controls
  MESSAGEBOX(oControl.Name)
ENDFOR
 
f = Factorial(10)
 
FUNCTION Factorial(n)
 LOCAL i,r
 r = 1
 FOR i = n TO 1 STEP -1
  r = r * n
 ENDFOR
 RETURN r
ENDFUNC

loForm = CREATEOBJECT("HiForm")
loForm.Show(1)
 
DEFINE CLASS HiForm AS Form
  AutoCenter = .T.
  Caption = "Hello, World"
 
  ADD OBJECT lblHi as Label WITH ;
    Caption = "Hello, World!"
ENDDEFINE

loMine = CREATEOBJECT("MyClass")
? loMine.cProp1   && This will work. (Double-ampersand marks an end-of-line comment)
? loMine.cProp2   && Program Error: Property CPROP2 is not found.
 
? loMine.MyMethod1()  && This will work.
? loMine.MyMethod2()  && Program Error: Property MYMETHOD2 is not found.
 
DEFINE CLASS MyClass AS Custom
  cProp1 = "My Property"    && This is a public property
  HIDDEN cProp2     && This is a private (hidden) property
  dProp3 = {}     && Another public property
 
  PROCEDURE Init()    && Class constructor
    This.cProp2 = "This is a hidden property."
  ENDPROC
 
  PROCEDURE dProp3_Access    && Property Getter
   RETURN DATE()
  ENDPROC
  PROCEDURE dProp3_Assign(vNewVal)   && Property Setter
    IF VARTYPE(vNewVal) = "D"
     THIS.dProp3 = vNewVal
    ENDIF
  ENDPROC
 
  PROCEDURE MyMethod1()
    * This is a public method, calling a hidden method that returns
    * the value of a hidden property.
    RETURN This.MyMethod2()
  ENDPROC
 
  HIDDEN PROCEDURE MyMethod2()  && This is a private (hidden) method
    RETURN This.cProp2
  ENDPROC
ENDDEFINE

&& Create a table
CREATE TABLE randData (iData I)
 
&& Populate with random data using xBase and SQL DML commands
FOR i = 1 TO 50
    APPEND BLANK
    REPLACE iData WITH (RAND() * 100)
 
    INSERT INTO randData (iData) VALUES (RAND() * 100)
ENDFOR
 
&& Place a structural index on the data
INDEX ON iData TAG iData
CLOSE ALL
 
&& Display ordered data using xBase-style commands
USE randData
SET ORDER TO iData
GO TOP
LIST NEXT 10  && First 10 
GO BOTTOM
SKIP -10
LIST REST     && Last 10
CLOSE ALL
 
&& Browse ordered data using SQL DML commands
SELECT * ;
  FROM randData ;
  ORDER BY iData DESCENDING


&& Connect to an ODBC data source
LOCAL nHnd
nHnd = SQLCONNECT ("ODBCDSN", "user", "pwd")
 
&& Execute a SQL command
LOCAL nResult
nResult = SQLEXEC (nHnd, "USE master")
IF nResult < 0
  MESSAGEBOX ("MASTER database does not exist!")
  RETURN
ENDIF
 
&& Retrieve data from the remote server and stores it in
&& a local data cursor
nResult = SQLEXEC (nHnd, "SELECT * FROM authors", "QAUTHORS")
 
&& Update a record in a remote table using parameters
PRIVATE cAuthorID, cAuthorName
cAuthorID = "1001"
cAuthorName = "New name"
nResult = SQLEXEC (nHnd, "UPDATE authors SET auth_name = ?cAuthorName WHERE auth_id = ?cAuthorID")
 
&& Close the connection
SQLDISCONNECT(nHnd)

