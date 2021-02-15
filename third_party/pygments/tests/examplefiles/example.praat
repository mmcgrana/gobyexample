form Highlighter test
  sentence Blank
  sentence My_sentence This should all be a string
  text My_text This should also all be a string
  word My_word Only the first word is a string, the rest is invalid
  boolean Binary 1
  boolean Text no
  boolean Quoted "yes"
  comment This should be a string
  real left_Range -123.6
  positive right_Range_max 3.3
  integer Int 4
  natural Nat 4
endform

# External scripts
include /path/to/file
runScript: "/path/to/file"
execute /path/to/file

# Predefined variables
a  = praatVersion
a  = e
a  = pi
a$ = homeDirectory$ + tab$ + newline$
a$ = temporaryDirectory$
a$ = praatVersion$
a$ = shellDirectory$
a$ = homeDirectory$
a$ = preferencesDirectory$
a$ = defaultDirectory$
nocheck selectObject: undefined

# Arrays are not comments
a# = zero# (5, 6)
a [3], 5 = 7
printline 'a[3,5]', 'a[3]'
a [1] = 2
b [a [1]] = 3
assert b [a [1]] = 3
printline 'b[2]'

# if-block with built-in variables
if windows
  # We are on Windows
elsif unix = 1 or !macintosh
  exitScript: "We are on Linux"
else macintosh == 1
  exit We are on Mac
endif

string$ = "Strings can be 'interpolated'"
string$ = "But don't interpolate everything!"

Text... 1 Right 0.2 Half many----hyphens
Text... 1 Right -0.4 Bottom aحبيبa
Text... 1 Right -0.6 Bottom 日本
Draw circle (mm)... 0.5 0.5 i
x=1

rows   = Object_'table'.nrow
value$ = Table_'table'$[25, "f0"]
fixed  = Sound_10.xmin
fixed  = Object_foo.xmin
fixed  = Procrustes_foo.nx

# old-style procedure call
call oldStyle "quoted" 2 unquoted string
assert oldStyle.local = 1

# New-style procedure call with parens
@newStyle("quoted", 2, "quoted string")
if praatVersion >= 5364
  # New-style procedure call with colon
  @newStyle: "quoted", 2, "quoted string"
endif

# inline if with inline comment
var = if macintosh = 1 then 0 else 1 fi ; This is an inline comment

# for-loop with explicit from using local variable
# and paren-style function calls and variable interpolation
n = numberOfSelected("Sound")
for i from newStyle.local to n
  name = selected$(extractWord$(selected$(), " "))
  sound'i' = selected("Sound", i)
  sound[i] = sound'i'
endfor

for i from 1 to n
  # Different styles of object selection
  select sound'i'
  sound = selected()
  sound$ = selected$("Sound")
  select Sound 'sound$'
  selectObject(sound[i])
  selectObject: sound

  # Pause commands
  beginPause("Viewing " + sound$)
  if i > 1
    button = endPause("Stop", "Previous",
      ...if i = total_sounds then "Finish" else "Next" fi,
      ...3, 1)
  else
    button = endPause("Stop",
      ...if i = total_sounds then "Finish" else "Next" fi,
      ...2, 1)
  endif
  editor_name$ = if total_textgrids then "TextGrid " else "Sound " fi + name$
  nocheck editor Sound 'editor_name$'
    nocheck Close
  nocheck endeditor
  editor_id = editor: editor_name$
    Close
  endeditor

  # New-style standalone command call
  Rename: "SomeName"

  # Command call with assignment
  duration = Get total duration

  # Multi-line command with modifier
  pitch = noprogress To Pitch (ac): 0, 75, 15, "no",
    ...0.03, 0.45, 0.01, 0.35, 0.14, 600

  # do-style command with assignment
  minimum = do("Get minimum...", 0, 0, "Hertz", "Parabolic")

  # New-style multi-line command call with broken strings
  table = Create Table with column names: "table", 0,
    ..."file subject speaker 
    ...f0 f1 f2 f3 " +
    ..."duration response"

  # Function call with trailing space
  removeObject: pitch, table 

  # Picture window commands
  selectObject: sound
  # do-style command
  do("Select inner viewport...", 1, 6, 0.5, 1.5)
  Black
  Draw... 0 0 0 0 "no" Curve
  Draw inner box
  Text bottom: "yes", sound$
  Erase all

  # Demo window commands
  demo Erase all
  demo Select inner viewport... 0 100 0 100
  demo Axes... 0 100 0 100
  demo Paint rectangle... white 0 100 0 100
  demo Text... 50 centre 50 half Click to finish
  demoWaitForInput ( )
  demo Erase all
  demo Text: 50, "centre", 50, "half", "Finished"
endfor

switch$ = if switch == 1 then "a" else
  ...     if switch == 2 then "b" else
  ...     if switch == 3 then "c" else
  ...     if switch == 4 then "d" else
  ...     "default" fi fi fi fi

# An old-style sendpraat block
# All these lines should be a string!
sendpraat Praat
  ...'newline$' Create Sound as pure tone... "tone" 1 0 0.4 44100 440 0.2 0.01 0.01
  ...'newline$' Play
  ...'newline$' Remove

# A new-style sendpraat block
beginSendPraat: "Praat"
  Create Sound as pure tone: "tone", 1, 0, 0.4, 44100, 440, 0.2, 0.01, 0.01
  duration = Get total duration
  Remove
endSendPraat: "duration"
appendInfoLine: "The generated sound lasted for ", duration, "seconds"

# Number types
a =   10%
a =  -10
a =  +10
a =   10.4
a =  294e12
a =    2.94e12

# Operators
a = 2 ^ -6
a = -(1+1)^6
a = 4^3 ^ 2
a = 54 div 5.1
a = 54.3 mod 5
a = 3 ** 8 - 7
a = 3 / (8 + 7)
a = (7 * (3 + 5)) / ((2 + 3) - 1)

# Logical operators
assert (a =   b) and c
assert  a == (b  or  c)
assert  a <=  b  not c
assert  a >=  b     !c
assert  a !=  b  &   c
assert  a !=  b  &&  c
assert  a <>  b  ||  c
assert  a <   b  |   c
assert  a >   b
assert "hello" =  "he" + "llo"
assert "hello" == "hello world" - " world"

stopwatch
time = stopwatch
clearinfo
echo This script took
print 'time' seconds to
printline execute.

# Old-style procedure declaration
procedure oldStyle .str1$ .num .str2$
  .local = 1
endproc

# New-style procedure declaration with parentheses
procedure newStyle (.str1$, .num, .str2$)
  # Command with "local" variable
  .local = Get total duration
  .local = Get 'some' duration
  .local = Get 'some[1]' value... hello 10 p[i]
  .local = Get 'some[1,3]' value: "hello", 10, 'p[i]'
  .local = Get 'some$' duration
  .local = Get 'some$[1]' duration
endproc

# New-style procedure declaration with colon
procedure _new_style: .str1$, .num, .str2$
  # Command with "local" variable
  # Initial underscores in variables not allowed (unless interpolated)
  _new_style.local = Get total duration
endproc

asserterror Unknown symbol:'newline$'« _
assert '_new_style.local'

