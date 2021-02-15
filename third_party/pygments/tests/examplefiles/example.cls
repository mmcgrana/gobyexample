USING Progress.Lang.*.

CLASS Test INHERITS Progress.Sucks:

  DEFINE PRIVATE VARIABLE cTest AS CHAR NO-UNDO.

  CONSTRUCTOR PUBLIC Test():
    SUPER().
    MESSAGE "Why are you punishing yourself by coding in this language?".
  END CONSTRUCTOR.

  METHOD PUBLIC LOGICAL Blowup(INPUT iTime AS INT):
  END.

END CLASS.
