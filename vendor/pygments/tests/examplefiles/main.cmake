SET( SOURCES back.c io.c main.c )
MESSAGE( ${SOURCES}   )      # three arguments, prints "back.cio.cmain.c"
MESSAGE( "${SOURCES}" )      # one argument,    prints "back.c;io.c;main.c"
MESSAGE( "" )                # one argument,    prints "" an empty line
MESSAGE( "${EMPTY_STRING}" ) # one argument,    prints "" an empty line
MESSAGE( ${EMPTY_STRING} )   # zero arguments,  causes CMake Error
                             # "MESSAGE called with incorrect number of arguments"
MESSAGE( \\\"\ \(\)\#\$\^ ) # this message contains literal characters

MESSAGE( "This is practice." )  # prints "This is practice."
MESSAGE( "This;is;practice." )  # prints "This;is;practice."
MESSAGE( "Hi. ) MESSAGE( x )" ) # prints "Hi. ) MESSAGE( x )"

MESSAGE( "Welc"ome ) # rule 1
MESSAGE( Welc"ome" ) # rule 3
MESSAGE( Welc"ome)" ) # rule 2
MESSAGE( ""Thanks ) # rule 1
MESSAGE( Thanks"" ) # rule 3

SET( x y A B C )              # stores "y;A;B;C" in x (without quote)
SET( ${x} )                   # => SET( y;A;B;C ) => SET( y A B C)
MESSAGE( ${y} )               # prints "ABC" to stdout (without quotes)
SET( y x )                    # stores "x" in y (without quotes)
SET( ${y} y = x )             # => SET( x y )
MESSAGE( "\${x} = '${x}'" )   # prints "${x} = 'y;=;x'" to stdout (without quotes)
SET( y ${x} )                 # => SET( y y = x ) => stores "y;=;x" in y (without quotes)
MESSAGE( ${y} )               # prints "y=x" to stdout (without quotes)

SET( x a b c   ) # stores "a;b;c" in x      (without quotes)
SET( y "a b c" ) # stores "a b c" in y      (without quotes)
MESSAGE( a b c ) # prints "abc"   to stdout (without quotes)
MESSAGE( ${x} )  # prints "abc"   to stdout (without quotes)
MESSAGE("${x}")  # prints "a;b;c" to stdout (without quotes)
MESSAGE( ${y} )  # prints "a b c" to stdout (without quotes)
MESSAGE("${y}")  # prints "a b c" to stdout (without quotes)

# This is a comment.
COMMAND( arguments go here )
ANOTHER_COMMAND() # this command has no arguments
YET_ANOTHER_COMMAND( these
  arguments are spread         # another comment
  over several lines )
