/* REXX example. */

/* Some basic constructs. */
almost_pi = 0.1415 + 3
if almost_pi < 3 then
   say 'huh?'
else do
   say 'almost_pi=' almost_pi || " - ok"
end
x = '"' || "'" || '''' || """" /* quotes */

/* A comment
 * spawning multiple
   lines. /* / */

/* Built-in functions. */
line = 'line containing some short text'
say WordPos(line, 'some')
say Word(line, 4)

/* Labels and procedures. */
some_label :

divide: procedure
    parse arg some other
    return some / other

call divide(5, 2)

/* Loops */
do i = 1 to 5
    do j = -3 to -9 by -3
        say i '+' j '=' i + j
    end j
end i

do forever
  leave
end

/* Print a text file on MVS. */
ADDRESS TSO
"ALLOC F(TEXTFILE) DSN('some.text.dsn') SHR REU"
"EXECIO * DISKR TEXTFILE ( FINIS STEM LINES."
"FREE F(TEXTFILE)"
I = 1
DO WHILE I <= LINES.0
    SAY ' LINE ' I ' : ' LINES.I
    I = I + 1
END
