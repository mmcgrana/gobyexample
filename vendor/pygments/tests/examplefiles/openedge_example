{include.i}
{nested.i {include.i}}

&SCOPED-DEFINE MY_NAME "Abe"

DEF VAR i AS INT NO-UNDO.
i = 0xABE + 1337 / (1 * 1.00)

def var clowercasetest as char no-undo.
DEF VAR vardashtest AS DATETIME-TZ NO-UNDO.

DEFINE TEMP-TABLE ttNames NO-UNDO
  FIELD cName AS CHAR
  INDEX IXPK_ttNames IS PRIMARY UNIQUE cName.

/* One-line comment */
/* Two-line
   Comment  */
/*
  Nested
  /*
    Multiline
    /*
      Comment
    */
  */
*/

CREATE ttNames.
ASSIGN ttNames.cName = {&MY_NAME}.

FOR EACH ttNames:
  MESSAGE "Hello, " + ttNames.cName + '!' VIEW-AS ALERT-BOX.
END.
