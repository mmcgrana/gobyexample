<!--- cfcomment --->
<!-- html comment -->
<html>
<head>
<title>Date Functions</title>
</head>
<body>
<cfset RightNow = Now()>
<cfoutput>
 #RightNow#<br />
 #DateFormat(RightNow)#<br />
 #DateFormat(RightNow,"mm/dd/yy")#<br />
 #TimeFormat(RightNow)#<br />
 #TimeFormat(RightNow,"hh:mm tt")#<br />
 #IsDate(RightNow)#<br />
 #IsDate("January 31, 2007")#<br />
 #IsDate("foo")#<br />
 #DaysInMonth(RightNow)#
</cfoutput>
<cfoutput group="x">
    #x#
    <cfoutput>#y#</cfoutput>
    #z#
</cfoutput>
</body>
</html>

<cfset person = "Paul">
<cfset greeting = "Hello #person#">

<cfset greeting = "Hello" & " world!">
<cfset c = a^b>
<cfset c = a MOD b>
<cfset c = a / b>
<cfset c = a * b>
<cfset c = a + b>
<cfset c = a - b>

