       IDENTIFICATION DIVISION.
       PROGRAM-ID. OCic.
      *****************************************************************
      ** This program provides a Textual User Interface (TUI) to the **
      ** process of compiling and (optionally) executing an OpenCOBOL**
      ** program.                                                    **
      **                                                             **
      ** This programs execution syntax is as follows:               **
      **                                                             **
      ** ocic <program-path-and-filename> [ <switch>... ]            **
      **                                                             **
      ** Once executed, a display screen will be presented showing   **
      ** the compilation options that will be used.  The user will   **
      ** have the opportunity to change options, specify new ones    **
      ** and specify any program execution arguments to be used if   **
      ** you select the "Execute" option.  When you press the Enter  **
      ** key the program will be compiled.                           **
      **                                                             **
      ** The SCREEN SECTION contains an image of the screen.         **
      **                                                             **
      ** The "010-Parse-Args" section in the PROCEDURE DIVISION has  **
      ** documentation on switches and their function.               **
      *****************************************************************
      **                                                             **
      ** AUTHOR:       GARY L. CUTLER                                **
      **               CutlerGL@gmail.com                            **
      **               Copyright (C) 2009-2010, Gary L. Cutler, GPL  **
      **                                                             **
      ** DATE-WRITTEN: June 14, 2009                                 **
      **                                                             **
      *****************************************************************
      ** Note: Depending on which extended DISPLAY handler you're    **
      **       using (PDCurses, Curses, ...), you may need to un-    **
      **       comment any source lines tagged with "SCROLL" in cols **
      **       1-6 in order to have error messages scroll properly   **
      **       in the OCic shell window.                             **
      *****************************************************************
      **  DATE  CHANGE DESCRIPTION                                   **
      ** ====== ==================================================== **
      ** GC0609 Don't display compiler messages file if compilation  **
      **        Is successful.  Also don't display messages if the   **
      **        output file is busy (just put a message on the       **
      **        screen, leave the OC screen up & let the user fix    **
      **        the problem & resubmit.                              **
      ** GC0709 When 'EXECUTE' is selected, a 'FILE BUSY' error will **
      **        still cause the (old) executable to be launched.     **
      **        Also, the 'EXTRA SWITCHES' field is being ignored.   **
      **        Changed the title bar to lowlighted reverse video &  **
      **        the message area to highlighted reverse-video.       **
      ** GC0809 Add a SPACE in from of command-line args when        **
      **        executing users program.  Add a SPACE after the      **
      **        -ftraceall switch when building cobc command.        **
      ** GC0909 Convert to work on Cygwin/Linux as well as MinGW     **
      ** GC0310 Virtualized the key codes for S-F1 thru S-F7 as they **
      **        differ depending upon whether PDCurses or NCurses is **
      **        being used.                                          **
      ** GC0410 Introduced the cross-reference and source listing    **
      **        features.  Also fixed a bug in @EXTRA switch proces- **
      **        sing where garbage will result if more than the      **
      **        @EXTRA switch is specified.                          **
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT Bat-File             ASSIGN TO Bat-File-Name
                                       ORGANIZATION IS LINE SEQUENTIAL.

           SELECT Cobc-Output          ASSIGN TO Cobc-Output-File
                                       ORGANIZATION IS LINE SEQUENTIAL.

           SELECT Source-Code          ASSIGN TO File-Name
                                       ORGANIZATION IS LINE SEQUENTIAL
                                       FILE STATUS IS FSM-Status.
       DATA DIVISION.
       FILE SECTION.
       FD  Bat-File.
       01  Bat-File-Rec                PIC X(2048).

       FD  Cobc-Output.
       01  Cobc-Output-Rec             PIC X(256).

       FD  Source-Code.
       01  Source-Code-Record          PIC X(80).

       WORKING-STORAGE SECTION.
       COPY screenio.

       01  Bat-File-Name               PIC X(256).

GC0909 01  Cmd                         PIC X(512).

       01  Cobc-Cmd                    PIC X(256).

       01  Cobc-Output-File            PIC X(256).

       01  Command-Line-Args           PIC X(256).

       01  Config-File                 PIC X(12).

GC0310 01  Config-Keys.
GC0310     05 CK-S-F1                  PIC 9(4).
GC0310     05 CK-S-F2                  PIC 9(4).
GC0310     05 CK-S-F3                  PIC 9(4).
GC0310     05 CK-S-F4                  PIC 9(4).
GC0310     05 CK-S-F5                  PIC 9(4).
GC0310     05 CK-S-F6                  PIC 9(4).
GC0310     05 CK-S-F7                  PIC 9(4).

GC0909 01  Dir-Char                    PIC X(1).

       01  Dummy                       PIC X(1).

       01  Env-TEMP                    PIC X(256).

       01  File-Name.
           05 FN-Char                  OCCURS 256 TIMES PIC X(1).

       01  File-Status-Message.
           05 FILLER                   PIC X(13) VALUE 'Status Code: '.
           05 FSM-Status               PIC 9(2).
           05 FILLER                   PIC X(11) VALUE ', Meaning: '.
           05 FSM-Msg                  PIC X(25).

       01  Flags.
           05 F-Compilation-Succeeded  PIC X(1).
              88 88-Compile-OK         VALUE 'Y'.
GC0909        88 88-Compile-OK-Warn    VALUE 'W'.
              88 88-Compile-Failed     VALUE 'N'.
GC0609     05 F-Complete               PIC X(1).
GC0609        88 88-Complete           VALUE 'Y'.
GC0609        88 88-Not-Complete       VALUE 'N'.
GC0809     05 F-IDENT-DIVISION         PIC X(1).
GC0809        88 88-1st-Prog-Complete  VALUE 'Y'.
GC0809        88 88-More-To-1st-Prog   VALUE 'N'.
           05 F-LINKAGE-SECTION        PIC X(1).
              88 88-Compile-As-Subpgm  VALUE 'Y'.
              88 88-Compile-As-Mainpgm VALUE 'N'.
           05 F-No-Switch-Changes      PIC X(1).
              88 88-No-Switch-Changes  VALUE 'Y'.
              88 88-Switch-Changes     VALUE 'N'.
GC0709     05 F-Output-File-Busy       PIC X(1).
GC0709        88 88-Output-File-Busy   VALUE 'Y'.
GC0709        88 88-Output-File-Avail  VALUE 'N'.
GC0809     05 F-Source-Record-Type     PIC X(1).
GC0809        88 88-Source-Rec-Linkage VALUE 'L'.
GC0809        88 88-Source-Rec-Ident   VALUE 'I'.
GC0809        88 88-Source-Rec-IgnoCOB-COLOR-RED VALUE ' '.
           05 F-Switch-Error           PIC X(1).
              88 88-Switch-Is-Bad      VALUE 'Y'.
              88 88-Switch-Is-Good     VALUE 'N'.

GC0909 01  Horizontal-Line             PIC X(80).
GC0909
       01  I                           USAGE BINARY-LONG.

       01  J                           USAGE BINARY-LONG.

GC0909 01  MS                          USAGE BINARY-LONG.

GC0909 01  ML                          USAGE BINARY-LONG.

       01  OC-Compiled                 PIC XXXX/XX/XXBXX/XX.

GC0909 01  OS-Type                     USAGE BINARY-LONG.
GC0909     88 OS-Unknown               VALUE 0.
GC0909     88 OS-Windows               VALUE 1.
GC0909     88 OS-Cygwin                VALUE 2.
GC0909     88 OS-UNIX                  VALUE 3.

GC0909 01  OS-Type-Literal             PIC X(7).

       01  Output-Message              PIC X(80).

       01  Path-Delimiter              PIC X(1).

       01  Prog-Folder                 PIC X(256).

       01  Prog-Extension              PIC X(30).

       01  Prog-File-Name              PIC X(40).

       01  Prog-Name                   PIC X(31).

       78  Selection-Char              VALUE '>'.

       01  Switch-Display.
           05 SD-Switch-And-Value      PIC X(19).
           05 FILLER                   PIC X(1).
           05 SD-Description           PIC X(60).

       01  Switch-Keyword              PIC X(12).
GC0410     88 Switch-Is-CONFIG     VALUE '@CONFIG', '@C'.
GC0410     88 Switch-Is-DEBUG      VALUE '@DEBUG', '@D'.
GC0410     88 Switch-Is-DLL        VALUE '@DLL'.
GC0410     88 Switch-Is-EXECUTE    VALUE '@EXECUTE', '@E'.
GC0410     88 Switch-Is-EXTRA      VALUE '@EXTRA', '@EX'.
GC0410     88 Switch-Is-NOTRUNC    VALUE '@NOTRUNC', '@N'.
GC0410     88 Switch-Is-TRACE      VALUE '@TRACE', '@T'.
GC0410     88 Switch-Is-SOURCE     VALUE '@SOURCE', '@S'.
GC0410     88 Switch-Is-XREF       VALUE '@XREF', '@X'.

       01  Switch-Keyword-And-Value    PIC X(256).

       01  Switch-Value.
           05 SV-1                     PIC X(1).
           05 FILLER                   PIC X(255).
       01  Switch-Value-Alt            REDEFINES Switch-Value
                                       PIC X(256).
           88 Valid-Config-Filename
              VALUE 'BS2000', 'COBOL85', 'COBOL2002', 'DEFAULT',
                    'IBM',    'MF',      'MVS'.

       01  Switches.
           05 S-ARGS                   PIC X(75) VALUE SPACES.
           05 S-CfgS.
              10 S-Cfg-BS2000          PIC X(1)  VALUE ' '.
              10 S-Cfg-COBOL85         PIC X(1)  VALUE ' '.
              10 S-Cfg-COBOL2002       PIC X(1)  VALUE ' '.
              10 S-Cfg-DEFAULT         PIC X(1)  VALUE Selection-Char.
              10 S-Cfg-IBM             PIC X(1)  VALUE ' '.
              10 S-Cfg-MF              PIC X(1)  VALUE ' '.
              10 S-Cfg-MVS             PIC X(1)  VALUE ' '.
           05 S-EXTRA                  PIC X(75) VALUE SPACES.
           05 S-Yes-No-Switches.
              10 S-DEBUG               PIC X(1)  VALUE 'N'.
              10 S-DLL                 PIC X(1)  VALUE 'N'.
GC0410        10 S-XREF                PIC X(1)  VALUE 'N'.
GC0410        10 S-SOURCE              PIC X(1)  VALUE 'N'.
              10 S-EXECUTE             PIC X(1)  VALUE 'N'.
              10 S-NOTRUNC             PIC X(1)  VALUE 'Y'.
              10 S-SUBROUTINE          PIC X(1)  VALUE 'A'.
              10 S-TRACE               PIC X(1)  VALUE 'N'.
              10 S-TRACEALL            PIC X(1)  VALUE 'N'.

       01  Tally                       USAGE BINARY-LONG.

         SCREEN SECTION.
      *>
      *> Here is the layout of the OCic screen.
      *>
      *> Note that this program can utilize the traditional PC line-drawing characters,
      *> if they are available.
      *>
      *> If this program is run on Windows, it must run with codepage 437 activated to
      *> display the line-drawing characters.  With a native Windows build or a
      *> Windows/MinGW build, one could use the command "chcp 437" to set that codepage
      *> for display within a Windows console window (that should be the default, though).
      *> With a Windows/Cygwin build, set the environment variable CYGWIN to a value of
      *> "codepage:oem" (this cannot be done from within the program though - you will
      *> have to use the "Computer/Advanced System Settings/Environment Variables" (Vista or
      *> Windows 7) function to define the variable.  XP Users: use "My Computer/Properties/
      *> Advanced/Environment Variables".
      *>
      *> To use OCic without the line-drawing characters, comment-out the first set of
      *> 78 "LD" items and uncomment the second.
      *>
      *> The following sample screen layout shows how the screen looks with line-drawing
      *> characters disabled.
      *>
      *>===================================================================================
      *> OCic (2010/04/02 11:36) - OpenCOBOL V1.1 Interactive Compilation        Windows 01
      *> +-----------------------------------------------------------------------------+ 02
      *> | Program:  OCic                                            F-Key: Select Opt | 03
      *> | Folder:   E:\OpenCOBOL\Samples                            Enter: Compile    | 04
      *> | Filename: OCic.cbl                                        Esc:   Quit       | 05
      *> +-----------------------------------------------------------------------------+ 06
      *>   On/Off Switches:                                          Configuration:      07
      *> +---------------------------------------------------------+-------------------+ 08
      *> | F1   Compile debug lines    F8   Produce source listing | S-F1   BS2000     | 09
      *> | F2   Always make DLLs       F9   Produce xref listing   | S-F2   COBOL85    | 10
      *> | F3   Pgm is a SUBROUTINE                                | S-F3   COBOL2002  | 11
      *> | F4   Execute if compile OK                              | S-F4 > Default    | 12
      *> | F5 > No COMP/BINARY trunc                               | S-F5   IBM        | 13
      *> | F6   Trace procedures                                   | S-F6   MicroFocus | 14
      *> | F7   Trace proc + stmnts                                | S-F7   MVS        | 15
      *> +---------------------------------------------------------+-------------------+ 16
      *>   Additional "cobc" Switches (if any):                                          17
      *> +-----------------------------------------------------------------------------+ 18
      *> | -O2________________________________________________________________________ | 19
      *> +-----------------------------------------------------------------------------+ 20
      *>   Program Execution Arguments (if any):                                         21
      *> +-----------------------------------------------------------------------------+ 22
      *> | ___________________________________________________________________________ | 23
      *> +-----------------------------------------------------------------------------+ 24
      *> OCic Copyright (C) 2009-2010, Gary L. Cutler, GPL                               25
      *>===================================================================================
      *>12345678901234567890123456789012345678901234567890123456789012345678901234567890
      *>         1         2         3         4         5         6         7         8
      *>
      *> USE THESE CHARS FOR LINE-DRAWING IF YOU HAVE ACCESS TO PC-DOS CODEPAGE 437:
      *>
       78 LD-UL-Corner                 VALUE X"DA".
       78 LD-LL-Corner                 VALUE X"C0".
       78 LD-UR-Corner                 VALUE X"BF".
       78 LD-LR-Corner                 VALUE X"D9".
       78 LD-Upper-T                   VALUE X"C2".
       78 LD-Lower-T                   VALUE X"C1".
       78 LD-Horiz-Line                VALUE X"C4".
       78 LD-Vert-Line                 VALUE X"B3".
      *>
      *> USE THESE CHARS FOR LINE-DRAWING IF YOU DO NOT HAVE ACCESS TO PC-DOS CODEPAGE 437:
      *>
      *> 78 LD-UL-Corner                          VALUE '+'.
      *> 78 LD-LL-Corner                          VALUE '+'.
      *> 78 LD-UR-Corner                          VALUE '+'.
      *> 78 LD-LR-Corner                          VALUE '+'.
      *> 78 LD-Upper-T                            VALUE '+'.
      *> 78 LD-Lower-T                            VALUE '+'.
      *> 78 LD-Horiz-Line                         VALUE '-'.
      *> 78 LD-Vert-Line                          VALUE '|'.
      *>
       01 Blank-Screen LINE 1 COLUMN 1 BLANK SCREEN.

       01 Switches-Screen BACKGROUND-COLOR COB-COLOR-BLACK
                          FOREGROUND-COLOR COB-COLOR-WHITE AUTO.
      *>
      *> GENERAL SCREEN FRAMEWORK
      *>
          03 BACKGROUND-COLOR COB-COLOR-BLACK
             FOREGROUND-COLOR COB-COLOR-BLUE  HIGHLIGHT.
             05 LINE 02 COL 02           VALUE LD-UL-Corner.
             05                PIC X(77) FROM  Horizontal-Line.
             05         COL 80           VALUE LD-UR-Corner.

             05 LINE 03 COL 02           VALUE LD-Vert-Line.
             05         COL 80           VALUE LD-Vert-Line.

             05 LINE 04 COL 02           VALUE LD-Vert-Line.
             05         COL 80           VALUE LD-Vert-Line.

             05 LINE 05 COL 02           VALUE LD-Vert-Line.
             05         COL 80           VALUE LD-Vert-Line.

             05 LINE 06 COL 02           VALUE LD-LL-Corner.
             05                PIC X(77) FROM  Horizontal-Line.
             05         COL 80           VALUE LD-LR-Corner.

             05 LINE 08 COL 02           VALUE LD-UL-Corner.
             05                PIC X(57) FROM  Horizontal-Line.
             05         COL 60           VALUE LD-Upper-T.
             05                PIC X(19) FROM  Horizontal-Line.
             05         COL 80           VALUE LD-UR-Corner.

             05 LINE 09 COL 02           VALUE LD-Vert-Line.
             05         COL 60           VALUE LD-Vert-Line.
             05         COL 80           VALUE LD-Vert-Line.

             05 LINE 10 COL 02           VALUE LD-Vert-Line.
             05         COL 60           VALUE LD-Vert-Line.
             05         COL 80           VALUE LD-Vert-Line.

             05 LINE 11 COL 02           VALUE LD-Vert-Line.
             05         COL 60           VALUE LD-Vert-Line.
             05         COL 80           VALUE LD-Vert-Line.

             05 LINE 12 COL 02           VALUE LD-Vert-Line.
             05         COL 60           VALUE LD-Vert-Line.
             05         COL 80           VALUE LD-Vert-Line.

             05 LINE 13 COL 02           VALUE LD-Vert-Line.
             05         COL 60           VALUE LD-Vert-Line.
             05         COL 80           VALUE LD-Vert-Line.

             05 LINE 14 COL 02           VALUE LD-Vert-Line.
             05         COL 60           VALUE LD-Vert-Line.
             05         COL 80           VALUE LD-Vert-Line.

             05 LINE 15 COL 02           VALUE LD-Vert-Line.
             05         COL 60           VALUE LD-Vert-Line.
             05         COL 80           VALUE LD-Vert-Line.

             05 LINE 16 COL 02           VALUE LD-LL-Corner.
             05                PIC X(57) FROM  Horizontal-Line.
             05         COL 60           VALUE LD-Lower-T.
             05                PIC X(19) FROM  Horizontal-Line.
             05         COL 80           VALUE LD-LR-Corner.

             05 LINE 18 COL 02           VALUE LD-UL-Corner.
             05                PIC X(77) FROM  Horizontal-Line.
             05         COL 80           VALUE LD-UR-Corner.

             05 LINE 19 COL 02           VALUE LD-Vert-Line.
             05         COL 80           VALUE LD-Vert-Line.

             05 LINE 20 COL 02           VALUE LD-LL-Corner.
             05                PIC X(77) FROM  Horizontal-Line.
             05         COL 80           VALUE LD-LR-Corner.

             05 LINE 22 COL 02           VALUE LD-UL-Corner.
             05                PIC X(77) FROM  Horizontal-Line.
             05         COL 80           VALUE LD-UR-Corner.

             05 LINE 23 COL 02           VALUE LD-Vert-Line.
             05         COL 80           VALUE LD-Vert-Line.

             05 LINE 24 COL 02           VALUE LD-LL-Corner.
             05                PIC X(77) FROM  Horizontal-Line.
             05         COL 80           VALUE LD-LR-Corner.
      *>
      *> TOP AND BOTTOM LINES
      *>
          03 BACKGROUND-COLOR COB-COLOR-BLUE  BLINK
             FOREGROUND-COLOR COB-COLOR-WHITE HIGHLIGHT.
GC0410       05 LINE 01 COL 01 VALUE ' OCic ('.
GC0410       05                PIC X(16) FROM OC-Compiled.
GC0410       05                VALUE ') OpenCOBOL V1.1 06FEB2009 ' &
GC0410                               'Interactive Compilation         '.
GC0410       05 LINE 25 COL 01 PIC X(81) FROM Output-Message.
      *>
      *> LABELS
      *>
          03 BACKGROUND-COLOR COB-COLOR-BLACK
             FOREGROUND-COLOR COB-COLOR-CYAN  HIGHLIGHT.
             05 LINE 07 COL 04 VALUE 'On/Off Switches:'.
             05         COL 62 VALUE 'Configuration:'.
             05 LINE 17 COL 04 VALUE 'Additional "cobc" Switches (if any
      -                              '):'.
             05 LINE 21 COL 04 VALUE 'Program Execution Arguments (if an
      -                              'y):'.
      *>
      *> TOP SECTION BACKGROUND
      *>
          03 BACKGROUND-COLOR COB-COLOR-BLACK
             FOREGROUND-COLOR COB-COLOR-CYAN LOWLIGHT.
             05 LINE 03 COL 04 VALUE 'Program:  '.
             05 LINE 04 COL 04 VALUE 'Folder:   '.
             05 LINE 05 COL 04 VALUE 'Filename: '.

             05 LINE 03 COL 62 VALUE 'F-Key: Select Opt'.
             05 LINE 04 COL 62 VALUE 'Enter: Compile   '.
             05 LINE 05 COL 62 VALUE 'Esc:   Quit      '.
      *>
      *> TOP SECTION PROGRAM INFO
      *>
          03 BACKGROUND-COLOR COB-COLOR-BLACK
             FOREGROUND-COLOR COB-COLOR-WHITE HIGHLIGHT.
             05 LINE 03 COL 14 PIC X(47) FROM Prog-Name.
             05 LINE 04 COL 14 PIC X(47) FROM Prog-Folder.
             05 LINE 05 COL 14 PIC X(47) FROM Prog-File-Name.
      *>
      *> MIDDLE LEFT SECTION F-KEYS
      *>
          03 BACKGROUND-COLOR COB-COLOR-BLACK
             FOREGROUND-COLOR COB-COLOR-WHITE HIGHLIGHT.
             05 LINE 09 COL 04 VALUE 'F1'.
             05 LINE 10 COL 04 VALUE 'F2'.
             05 LINE 11 COL 04 VALUE 'F3'.
             05 LINE 12 COL 04 VALUE 'F4'.
             05 LINE 13 COL 04 VALUE 'F5'.
             05 LINE 14 COL 04 VALUE 'F6'.
             05 LINE 15 COL 04 VALUE 'F7'.
             05 LINE 09 COL 32 VALUE 'F8'.
             05 LINE 10 COL 32 VALUE 'F9'.
      *>
      *> MIDDLE LEFT SECTION SWITCHES
      *>
          03 BACKGROUND-COLOR COB-COLOR-BLACK
             FOREGROUND-COLOR COB-COLOR-RED   HIGHLIGHT.
             05 LINE 09 COL 07 PIC X(1) FROM S-DEBUG.
             05 LINE 10 COL 07 PIC X(1) FROM S-DLL.
             05 LINE 11 COL 07 PIC X(1) FROM S-SUBROUTINE.
             05 LINE 12 COL 07 PIC X(1) FROM S-EXECUTE.
             05 LINE 13 COL 07 PIC X(1) FROM S-NOTRUNC.
             05 LINE 14 COL 07 PIC X(1) FROM S-TRACE.
             05 LINE 15 COL 07 PIC X(1) FROM S-TRACEALL.
             05 LINE 09 COL 35 PIC X(1) FROM S-SOURCE.
             05 LINE 10 COL 35 PIC X(1) FROM S-XREF.
      *>
      *> MIDDLE LEFT SECTION BACKGROUND
      *>
          03 BACKGROUND-COLOR COB-COLOR-BLACK
             FOREGROUND-COLOR COB-COLOR-CYAN  LOWLIGHT.
             05 LINE 09 COL 09 VALUE 'Compile debug lines   '.
             05 LINE 10 COL 09 VALUE 'Always make DLLs      '.
             05 LINE 11 COL 09 VALUE 'Pgm is a SUBROUTINE   '.
             05 LINE 12 COL 09 VALUE 'Execute if compile OK '.
             05 LINE 13 COL 09 VALUE 'No COMP/BINARY trunc  '.
             05 LINE 14 COL 09 VALUE 'Trace procedures      '.
             05 LINE 15 COL 09 VALUE 'Trace proc + stmnts   '.
             05 LINE 09 COL 37 VALUE 'Produce source listing'.
             05 LINE 10 COL 37 VALUE 'Produce xref listing  '.
      *>
      *> MIDDLE RIGHT SECTION F-KEYS
      *>
          03 BACKGROUND-COLOR COB-COLOR-BLACK
             FOREGROUND-COLOR COB-COLOR-WHITE HIGHLIGHT.
             05 LINE 09 COL 62 VALUE 'S-F1'.
             05 LINE 10 COL 62 VALUE 'S-F2'.
             05 LINE 11 COL 62 VALUE 'S-F3'.
             05 LINE 12 COL 62 VALUE 'S-F4'.
             05 LINE 13 COL 62 VALUE 'S-F5'.
             05 LINE 14 COL 62 VALUE 'S-F6'.
             05 LINE 15 COL 62 VALUE 'S-F7'.
      *>
      *> MIDDLE RIGHT SECTION SWITCHES
      *>
          03 BACKGROUND-COLOR COB-COLOR-BLACK
             FOREGROUND-COLOR COB-COLOR-RED HIGHLIGHT.
             05 LINE 09 COL 67 PIC X(1) FROM S-Cfg-BS2000.
             05 LINE 10 COL 67 PIC X(1) FROM S-Cfg-COBOL85.
             05 LINE 11 COL 67 PIC X(1) FROM S-Cfg-COBOL2002.
             05 LINE 12 COL 67 PIC X(1) FROM S-Cfg-DEFAULT.
             05 LINE 13 COL 67 PIC X(1) FROM S-Cfg-IBM.
             05 LINE 14 COL 67 PIC X(1) FROM S-Cfg-MF.
             05 LINE 15 COL 67 PIC X(1) FROM S-Cfg-MVS.
      *>
      *> MIDDLE RIGHT SECTION BACKGROUND
      *>
          03 BACKGROUND-COLOR COB-COLOR-BLACK
             FOREGROUND-COLOR COB-COLOR-CYAN LOWLIGHT.
             05 LINE 09 COL 69 VALUE 'BS2000    '.
             05 LINE 10 COL 69 VALUE 'COBOL85   '.
             05 LINE 11 COL 69 VALUE 'COBOL2002 '.
             05 LINE 12 COL 69 VALUE 'Default   '.
             05 LINE 13 COL 69 VALUE 'IBM       '.
             05 LINE 14 COL 69 VALUE 'MicroFocus'.
             05 LINE 15 COL 69 VALUE 'MVS       '.
      *>
      *> FREE-FORM OPTIONS FIELDS
      *>
          03 BACKGROUND-COLOR COB-COLOR-BLACK
             FOREGROUND-COLOR COB-COLOR-WHITE HIGHLIGHT.
             05 LINE 19 COL 04 PIC X(75) USING S-EXTRA.
             05 LINE 23 COL 04 PIC X(75) USING S-ARGS.
      /
       PROCEDURE DIVISION.
      *****************************************************************
      ** Legend to procedure names:                                  **
      **                                                             **
      ** 00x-xxx   All MAIN driver procedures                        **
      ** 0xx-xxx   All GLOBAL UTILITY procedures                     **
      ** 1xx-xxx   All INITIALIZATION procedures                     **
      ** 2xx-xxx   All CORE PROCESSING procedures                    **
      ** 9xx-xxx   All TERMINATION procedures                        **
      *****************************************************************
       DECLARATIVES.
       000-File-Error SECTION.
           USE AFTER STANDARD ERROR PROCEDURE ON Source-Code.
       000-Handle-Error.
           COPY FileStat-Msgs
               REPLACING STATUS BY FSM-Status
                         MSG    BY FSM-Msg.
           MOVE SPACES TO Output-Message
           IF FSM-Status = 35
               DISPLAY
                   'File not found: "'
                   TRIM(File-Name,TRAILING)
                   '"'
               END-DISPLAY
           ELSE
               DISPLAY
                   'Error accessing file: "'
                   TRIM(File-Name,TRAILING)
                   '"'
               END-DISPLAY
           END-IF
           GOBACK
           .
       END DECLARATIVES.
      /
       000-Main SECTION.

           PERFORM 100-Initialization
GC0609     SET 88-Not-Complete TO TRUE
GC0609     PERFORM UNTIL 88-Complete
GC0609         PERFORM 200-Let-User-Set-Switches
GC0609         PERFORM 210-Run-Compiler
GC0410         IF (88-Compile-OK OR 88-Compile-OK-Warn)
GC0410         AND (S-XREF NOT = SPACE OR S-SOURCE NOT = SPACE)
GC0410             PERFORM 220-Make-Listing
GC0410         END-IF
GC0709         IF  (S-EXECUTE NOT = SPACES)
GC0709         AND (88-Output-File-Avail)
GC0609             PERFORM 230-Run-Program
GC0609         END-IF
GC0609     END-PERFORM
           .

       009-Done.
           PERFORM 900-Terminate
           .
      * -- Control will NOT return
      /
       010-Parse-Args SECTION.
      *****************************************************************
      ** Process a sequence of KEYWORD=VALUE items.  These are items **
      ** specified on the command-line to provide the initial        **
      ** options shown selected on the screen.  When integrating     **
      ** OCic into an edirot or framework, include these switches on **
      ** the ocic.exe command the editor/framework executes.  Any    **
      ** underlined choice is the default value for that switch.     **
      **                                                             **
      ** @CONFIG=BS2000|COBOL85|COBOL2002|DEFAULT|IBM|MF|MVS         **
      **                                  =======                    **
      ** This switch specifies the default cobc compiler configura-  **
      ** tion file to be used                                        **
      **                                                             **
      ** @DEBUG=YES|NO                                               **
      **            ==                                               **
      ** This switch specifies whether (YES) or not (NO) debugging   **
      ** lines (those with a "D" in column 7) will be compiled.      **
      **                                                             **
      ** @DLL=YES|NO                                                 **
      **          ==                                                 **
      ** Use this switch to force ALL compiled programs to be built  **
      ** as DLLs ("@DLL=YES").  When main programs are built as DLLs **
      ** they must be executed using the cobcrun utility.  When      **
      ** "@DLL=NO" is in effect, main programs are generated as      **
      ** actual "exe" files and only subprograms will be generated   **
      ** as DLLs.                                                    **
      **                                                             **
      ** @EXECUTE=YES|NO                                             **
      **              ==                                             **
      ** This switch specifies whether ("@EXECUTE=YES") or not       **
      ** ("@EXECUTE=NO") the program will be executed after it is    **
      ** successfully compiled.                                      **
      **                                                             **
      ** @EXTRA=extra cobc argument(s)                               **
      **                                                             **
      ** This switch allows you to specify additional cobc arguments **
      ** that aren't managed by the other OC switches.  If used,     **
      ** this must be the last switch specified on the command line, **
      ** as everything that follows the "=" will be placed on the    **
      ** cobc command generated by OC.                               **
      **                                                             **
      ** @NOTRUNC=YES|NO                                             **
      **          ===                                                **
      ** This switch specifies whether (YES) or not (NO) the sup-    **
      ** pression of binary field truncation will occur.  If a PIC   **
      ** 99 COMP field (one byte of storage), for example, is given  **
      ** the value 123, it may have its value truncated to 23 when   **
      ** DISPLAYed.  Regardless of the NOTRUNC setting, internally   **
      ** the full precision of the field (allowing a maximum value   **
      ** of 255) will be preserved.  Even though truncation - if it  **
      ** does occur - would appear to have a minimal disruption on   **
      ** program operation, it has a significant effect on program   **
      ** run-time speed.                                             **
      **                                                             **
      ** @TRACE=YES|NO|ALL                                           **
      **            ==                                               **
      ** This switch controls whether or not code will be added to   **
      ** the object program to produce execution-time logic traces.  **
      ** A specification of "@TRACE=NO" means no such code will be   **
      ** produced.  By specifying "@TRACE=YES", code will be genera- **
      ** ted to display procedure names as they are entered.  A      **
      ** "@TRACE=ALL" specification will generate not only procedure **
      ** traces (as "@TRACE=YES" would) but also statement-level     **
      ** traces too!  All trace output is written to STDERR, so      **
      ** adding a "2>file" to the execution of the program will pipe **
      ** the trace output to a file.  You may find it valuable to    **
      ** add your own DISPLAY statements to the debugging output via **
      ** "DISPLAY xx UPON SYSERR"  The SYSERR device corresponds to  **
      ** the Windows or UNIX STDERR device and will therefore honor  **
      ** any "2>file" placed at the end of your program's execution. **
      ** Add a "D" in column 7 and you can control the generation or **
      ** ignoring of these DISPLAY statements via the "@DEBUG"       **
      ** switch.                                                     **
      **                                                             **
GC0410** @SOURCE=YES|NO                                              **
GC0410**           ==                                                **
GC0410** Use this switch to produce a source listing of the program, **
GC0410** PROVIDED it compiles without errors.                        **
      **                                                             **
GC0410** @XREF=YES|NO                                                **
GC0410**           ==                                                **
GC0410** Use this switch to produce a cross-reference listing of the **
GC0410** program, PROVIDED it compiles without errors.               **
      *****************************************************************

       011-Init.
           MOVE 1 TO I
           .

       012-Extract-Kwd-And-Value.
           PERFORM UNTIL I NOT < LENGTH(Command-Line-Args)
               MOVE I TO J
               UNSTRING Command-Line-Args
                   DELIMITED BY ALL SPACES
                   INTO Switch-Keyword-And-Value
                   WITH POINTER I
               END-UNSTRING
               IF Switch-Keyword-And-Value NOT = SPACES
                   UNSTRING Switch-Keyword-And-Value
                       DELIMITED BY '='
                       INTO Switch-Keyword, Switch-Value
                   END-UNSTRING
                   PERFORM 030-Process-Keyword
               END-IF
           END-PERFORM
           .

       019-Done.
           EXIT.

      *****************************************************************
      ** Since this program uses the SCREEN SECTION, it cannot do    **
      ** conventional console DISPLAY operations.  This routine      **
      ** (which, I admit, is like using an H-bomb to hunt rabbits)   **
      ** will submit an "ECHO" command to the system to simulate a   **
      ** DISPLAY.                                                    **
      *****************************************************************
       021-Build-And-Issue-Command.
           DISPLAY
               Output-Message
           END-DISPLAY
           .

       029-Done.
           EXIT.
      /
       030-Process-Keyword SECTION.
      *****************************************************************
      ** Process a single KEYWORD=VALUE item.                        **
      *****************************************************************

       031-Init.
           MOVE UPPER-CASE(Switch-Keyword) TO Switch-Keyword
           SET 88-Switch-Is-Good TO TRUE
           .

       032-Process.
           EVALUATE TRUE
               WHEN Switch-Is-EXTRA
GC0410             MOVE J TO I
                   UNSTRING Command-Line-Args DELIMITED BY '='
                       INTO Dummy, S-EXTRA
GC0410                 WITH POINTER I
GC0410             END-UNSTRING
                   MOVE LENGTH(Command-Line-Args) TO I
               WHEN Switch-Is-CONFIG
                   MOVE 'CONFIG' TO Switch-Keyword
                   MOVE UPPER-CASE(Switch-Value)
                     TO Switch-Value
                   EVALUATE Switch-Value
                       WHEN 'BS2000'
                           MOVE SPACES TO S-CfgS
                           MOVE Selection-Char    TO S-Cfg-BS2000
                       WHEN 'COBOL85'
                           MOVE SPACES TO S-CfgS
                           MOVE Selection-Char    TO S-Cfg-COBOL85
                       WHEN 'COBOL2002'
                           MOVE SPACES TO S-CfgS
                           MOVE Selection-Char    TO S-Cfg-COBOL2002
                       WHEN 'DEFAULT'
                           MOVE SPACES TO S-CfgS
                           MOVE Selection-Char    TO S-Cfg-DEFAULT
                       WHEN 'IBM'
                           MOVE SPACES TO S-CfgS
                           MOVE Selection-Char    TO S-Cfg-IBM
                       WHEN 'MF'
                           MOVE SPACES TO S-CfgS
                           MOVE Selection-Char    TO S-Cfg-MF
                       WHEN 'MVS'
                           MOVE SPACES TO S-CfgS
                           MOVE Selection-Char    TO S-Cfg-MVS
                       WHEN OTHER
                           MOVE 'An invalid /CONFIG switch value ' &
                                'was specified on the command line ' &
                                '- ignored'
                             TO Output-Message
                   END-EVALUATE
               WHEN Switch-Is-DEBUG
                   MOVE 'DEBUG' TO Switch-Keyword
                   MOVE UPPER-CASE(Switch-Value)
                     TO Switch-Value
                   PERFORM 040-Process-Yes-No-Value
                   IF 88-Switch-Is-Good
                       MOVE SV-1 TO S-DEBUG
                   END-IF
GC0410         WHEN Switch-Is-DLL
GC0410             MOVE 'DLL' TO Switch-Keyword
GC0410             MOVE UPPER-CASE(Switch-Value)
GC0410               TO Switch-Value
GC0410             PERFORM 040-Process-Yes-No-Value
GC0410             IF 88-Switch-Is-Good
GC0410                 MOVE SV-1 TO S-DLL
GC0410             END-IF
               WHEN Switch-Is-EXECUTE
                   MOVE 'EXECUTE' TO Switch-Keyword
                   MOVE UPPER-CASE(Switch-Value)
                     TO Switch-Value
                   PERFORM 040-Process-Yes-No-Value
                   IF 88-Switch-Is-Good
                       MOVE SV-1 TO S-EXECUTE
                   END-IF
               WHEN Switch-Is-NOTRUNC
                   MOVE 'NOTRUNC' TO Switch-Keyword
                   MOVE UPPER-CASE(Switch-Value)
                     TO Switch-Value
                   PERFORM 040-Process-Yes-No-Value
                   IF 88-Switch-Is-Good
                       MOVE SV-1 TO S-NOTRUNC
                   END-IF
GC0410         WHEN Switch-Is-SOURCE
GC0410             MOVE 'SOURCE' TO Switch-Keyword
GC0410             MOVE UPPER-CASE(Switch-Value)
GC0410               TO Switch-Value
GC0410             PERFORM 050-Process-Yes-No-All
GC0410             IF 88-Switch-Is-Good
GC0410                 MOVE SV-1 TO S-SOURCE
GC0410             END-IF
               WHEN Switch-Is-TRACE
                   MOVE 'TRACE' TO Switch-Keyword
                   MOVE UPPER-CASE(Switch-Value)
                     TO Switch-Value
                   PERFORM 050-Process-Yes-No-All
                   IF 88-Switch-Is-Good
                       MOVE SV-1 TO S-TRACE
                   END-IF
GC0410         WHEN Switch-Is-XREF
GC0410             MOVE 'XREF' TO Switch-Keyword
GC0410             MOVE UPPER-CASE(Switch-Value)
GC0410               TO Switch-Value
GC0410             PERFORM 050-Process-Yes-No-All
GC0410             IF 88-Switch-Is-Good
GC0410                 MOVE SV-1 TO S-XREF
GC0410             END-IF
               WHEN OTHER
                   MOVE SPACES TO Output-Message
                   STRING '"'
                          TRIM(Switch-Keyword)
                          '" is not a valid switch ' &
                                         '- ignored'
                          DELIMITED SIZE
                          INTO Output-Message
                   END-STRING
                   SET 88-Switch-Is-Bad TO TRUE
           END-EVALUATE
           .

       039-Done.
           EXIT.
      /
       040-Process-Yes-No-Value SECTION.
      *****************************************************************
      ** Process a switch value of YES or NO                         **
      *****************************************************************

       042-Process.
           EVALUATE SV-1
               WHEN 'Y'
                   MOVE 'YES' TO Switch-Value
               WHEN 'N'
                   MOVE 'NO'  To Switch-Value
               WHEN OTHER
                   MOVE SPACES TO Output-Message
                   STRING '*ERROR: "' TRIM(Switch-Value)
                           '" is not a valid value for the "'
                           TRIM(Switch-Keyword) '" switch'
                           DELIMITED SPACES
                           INTO Output-Message
                   END-STRING
                   SET 88-Switch-Is-Bad TO TRUE
           END-EVALUATE
           .

       049-Done.
           EXIT.
      /
       050-Process-Yes-No-All SECTION.
      *****************************************************************
      ** Process a switch value of YES, NO or ALL                    **
      *****************************************************************

       052-Process.
           IF SV-1 = 'A'
               MOVE 'ALL' TO Switch-Value
           ELSE
               PERFORM 040-Process-Yes-No-Value
           END-IF
           .

       059-Done.
           EXIT.
      /
       060-Process-Yes-No-Auto SECTION.
      *****************************************************************
      ** Process a switch value of YES, NO or AUTO                   **
      *****************************************************************

       061-Init.
           IF SV-1 = 'A'
               PERFORM 070-Find-LINKAGE-SECTION
               IF 88-Compile-As-Subpgm
                   MOVE 'Y' TO Switch-Value
               ELSE
                   MOVE 'N' TO Switch-Value
               END-IF
           ELSE
               PERFORM 040-Process-Yes-No-Value
           END-IF
           .
      /
       070-Find-LINKAGE-SECTION SECTION.
      *****************************************************************
      ** Determine if the program being compiled is a MAIN program   **
      *****************************************************************

       071-Init.
           OPEN INPUT Source-Code
           SET 88-Compile-As-Mainpgm TO TRUE
           SET 88-More-To-1st-Prog   TO TRUE
           PERFORM UNTIL 88-1st-Prog-Complete
               READ Source-Code AT END
                   CLOSE Source-Code
                   EXIT SECTION
               END-READ
               CALL 'CHECKSOURCE' USING Source-Code-Record
                                       F-Source-Record-Type
               END-CALL
               IF 88-Source-Rec-Ident
                   SET 88-1st-Prog-Complete TO TRUE
               END-IF
           END-PERFORM
           .

       072-Process-Source.
           SET 88-Source-Rec-IgnoCOB-COLOR-RED TO TRUE
           PERFORM UNTIL 88-Source-Rec-Linkage
                      OR 88-Source-Rec-Ident
               READ Source-Code AT END
                   CLOSE Source-Code
                   EXIT SECTION
               END-READ
               CALL 'CHECKSOURCE' USING Source-Code-Record
                                       F-Source-Record-Type
               END-CALL
           END-PERFORM
           CLOSE Source-Code
           IF 88-Source-Rec-Linkage
               SET 88-Compile-As-Subpgm TO TRUE
           END-IF
           .

       079-Done.
           EXIT.
      /
       100-Initialization SECTION.
      *****************************************************************
      ** Perform all program-wide initialization operations          **
      *****************************************************************


GC0909 101-Determine-OS-Type.
GC0909     CALL 'GETOSTYPE'
GC0909     END-CALL
GC0909     MOVE RETURN-CODE TO OS-Type
GC0909     EVALUATE TRUE
GC0909         WHEN OS-Unknown
GC0909             MOVE '\'         TO Dir-Char
GC0909             MOVE 'Unknown'   TO OS-Type-Literal
GC0310             MOVE COB-SCR-F11 TO CK-S-F1
GC0310             MOVE COB-SCR-F12 TO CK-S-F2
GC0310             MOVE COB-SCR-F13 TO CK-S-F3
GC0310             MOVE COB-SCR-F14 TO CK-S-F4
GC0310             MOVE COB-SCR-F15 TO CK-S-F5
GC0310             MOVE COB-SCR-F16 TO CK-S-F6
GC0310             MOVE COB-SCR-F17 TO CK-S-F7
GC0909         WHEN OS-Windows
GC0909             MOVE '\'         TO Dir-Char
GC0909             MOVE 'Windows'   TO OS-Type-Literal
GC0310             MOVE COB-SCR-F13 TO CK-S-F1
GC0310             MOVE COB-SCR-F14 TO CK-S-F2
GC0310             MOVE COB-SCR-F15 TO CK-S-F3
GC0310             MOVE COB-SCR-F16 TO CK-S-F4
GC0310             MOVE COB-SCR-F17 TO CK-S-F5
GC0310             MOVE COB-SCR-F18 TO CK-S-F6
GC0310             MOVE COB-SCR-F19 TO CK-S-F7
GC0909         WHEN OS-Cygwin
GC0909             MOVE '/'         TO Dir-Char
GC0410             MOVE 'Cygwin'    TO OS-Type-Literal
GC0310             MOVE COB-SCR-F11 TO CK-S-F1
GC0310             MOVE COB-SCR-F12 TO CK-S-F2
GC0310             MOVE COB-SCR-F13 TO CK-S-F3
GC0310             MOVE COB-SCR-F14 TO CK-S-F4
GC0310             MOVE COB-SCR-F15 TO CK-S-F5
GC0310             MOVE COB-SCR-F16 TO CK-S-F6
GC0310             MOVE COB-SCR-F17 TO CK-S-F7
GC0909         WHEN OS-UNIX
GC0909             MOVE '/'         TO Dir-Char
GC0410             MOVE 'UNIX   '   TO OS-Type-Literal
GC0310             MOVE COB-SCR-F11 TO CK-S-F1
GC0310             MOVE COB-SCR-F12 TO CK-S-F2
GC0310             MOVE COB-SCR-F13 TO CK-S-F3
GC0310             MOVE COB-SCR-F14 TO CK-S-F4
GC0310             MOVE COB-SCR-F15 TO CK-S-F5
GC0310             MOVE COB-SCR-F16 TO CK-S-F6
GC0310             MOVE COB-SCR-F17 TO CK-S-F7
GC0909     END-EVALUATE
GC0909     .

       102-Set-Environment-Vars.
           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'
           SET ENVIRONMENT 'COB_SCREEN_ESC'        TO 'Y'
           .

       103-Generate-Cobc-Output-Fn.
           ACCEPT Env-TEMP
               FROM ENVIRONMENT "TEMP"
           END-ACCEPT
           MOVE SPACES TO Cobc-Output-File
           STRING TRIM(Env-TEMP,TRAILING)
GC0909            Dir-Char
GC0909            'OC-Messages.TXT'
                  DELIMITED SIZE
                  INTO Cobc-Output-File
           END-STRING
           .

       104-Generate-Banner-Line-Info.
           MOVE WHEN-COMPILED (1:12) TO OC-Compiled
           INSPECT OC-Compiled
               REPLACING ALL '/' BY ':'
               AFTER INITIAL SPACE
           .

       105-Establish-Switch-Settings.
           ACCEPT Command-Line-Args
               FROM COMMAND-LINE
           END-ACCEPT
           MOVE TRIM(Command-Line-Args, Leading)
             TO Command-Line-Args
           MOVE 0 TO Tally
GC0410     INSPECT Command-Line-Args TALLYING Tally FOR ALL '@'
           IF Tally = 0
               MOVE Command-Line-Args TO File-Name
               MOVE SPACES            TO Command-Line-Args
           ELSE
GC0410         UNSTRING Command-Line-Args DELIMITED BY '@'
                   INTO File-Name, Dummy
               END-UNSTRING
               INSPECT Command-Line-Args
GC0410             REPLACING FIRST '@' BY LOW-VALUES
               UNSTRING Command-Line-Args
                   DELIMITED BY LOW-VALUES
                   INTO Dummy, Cmd
               END-UNSTRING
               MOVE SPACES TO Command-Line-Args
GC0410         STRING '@' Cmd DELIMITED SIZE
                   INTO Command-Line-Args
               END-STRING
           END-IF
           IF File-Name = SPACES
               DISPLAY
                   'No program filename was specified'
               END-DISPLAY
               PERFORM 900-Terminate
           END-IF
           PERFORM 010-Parse-Args
           IF S-SUBROUTINE = 'A'
               MOVE 'S' TO Switch-Keyword
               MOVE 'A' TO Switch-Value
               PERFORM 070-Find-LINKAGE-SECTION
               IF 88-Compile-As-Subpgm
                   MOVE 'Y' TO S-SUBROUTINE
               ELSE
                   MOVE 'N' TO S-SUBROUTINE
               END-IF
           END-IF
           INSPECT S-Yes-No-Switches REPLACING ALL 'Y' BY Selection-Char
           INSPECT S-Yes-No-Switches REPLACING ALL 'N' BY ' '
           .

       106-Determine-Folder-Path.
           Move 256 TO I
GC0909     IF OS-Cygwin AND File-Name (2:1) = ':'
GC0909         MOVE '\' TO Dir-Char
GC0909     END-IF
           PERFORM UNTIL I = 0 OR FN-Char (I) = Dir-Char
               SUBTRACT 1 FROM I
           END-PERFORM
           IF I = 0
               MOVE SPACES    TO Prog-Folder
               MOVE File-Name TO Prog-File-Name
           ELSE
               MOVE '*' TO FN-Char (I)
               UNSTRING File-Name DELIMITED BY '*'
                   INTO Prog-Folder
                        Prog-File-Name
               END-UNSTRING
               MOVE Dir-Char TO FN-Char (I)
           END-IF
           UNSTRING Prog-File-Name DELIMITED BY '.'
               INTO Prog-Name, Prog-Extension
           END-UNSTRING
           IF Prog-Folder = SPACES
               ACCEPT Prog-Folder
                   FROM ENVIRONMENT 'CD'
               END-ACCEPT
GC0909     ELSE
GC0909         CALL "CBL_CHANGE_DIR"
GC0909             USING TRIM(Prog-Folder,TRAILING)
GC0909         END-CALL
           END-IF
GC0909     IF OS-Cygwin AND File-Name (2:1) = ':'
GC0909         MOVE '/' TO Dir-Char
GC0909     END-IF
           .

GC0909 107-Other.
GC0909     MOVE ALL LD-Horiz-Line TO Horizontal-Line.
GC0410     MOVE CONCATENATE(' OCic for ',
GC0410                      TRIM(OS-Type-Literal,Trailing),
GC0410                      ' Copyright (C) 2009-2010, Gary L. Cutler,',
GC0410                      ' GPL')
GC0410       TO Output-Message.
GC0909     .
GC0909
       109-Done.
           EXIT.
      /
       200-Let-User-Set-Switches SECTION.
      *****************************************************************
      ** Show the user the current switch settings and allow them to **
      ** be changed.                                                 **
      *****************************************************************

       201-Init.
           SET 88-Switch-Changes TO TRUE
           .

       202-Show-And-Change-Switches.
           PERFORM UNTIL 88-No-Switch-Changes
               ACCEPT
                   Switches-Screen
               END-ACCEPT
               IF COB-CRT-STATUS > 0
                   EVALUATE COB-CRT-STATUS
                       WHEN COB-SCR-F1
                           IF S-DEBUG = SPACE
                               MOVE Selection-Char TO S-DEBUG
                           ELSE
                               MOVE ' ' TO S-DEBUG
                           END-IF
                       WHEN COB-SCR-F2
                           IF S-DLL = SPACE
                               MOVE Selection-Char TO S-DLL
                           ELSE
                               MOVE ' ' TO S-DLL
                           END-IF
                       WHEN COB-SCR-F3
                           IF S-SUBROUTINE = SPACE
                               MOVE Selection-Char TO S-SUBROUTINE
                               MOVE ' ' TO S-EXECUTE
                           ELSE
                               MOVE ' ' TO S-SUBROUTINE
                           END-IF
                       WHEN COB-SCR-F4
                           IF  S-EXECUTE = SPACE
                           AND S-SUBROUTINE = SPACE
                               MOVE Selection-Char TO S-EXECUTE
                           ELSE
                               MOVE ' ' TO S-EXECUTE
                           END-IF
                       WHEN COB-SCR-F5
                           IF  S-NOTRUNC = SPACE
                               MOVE Selection-Char TO S-NOTRUNC
                           ELSE
                               MOVE ' ' TO S-NOTRUNC
                           END-IF
                       WHEN COB-SCR-F6
                           IF  S-TRACE = SPACE
                               MOVE Selection-Char TO S-TRACE
                               MOVE ' ' TO S-TRACEALL
                           ELSE
                               MOVE ' ' TO S-TRACE
                           END-IF
                       WHEN COB-SCR-F7
                           IF  S-TRACEALL = SPACE
                               MOVE Selection-Char TO S-TRACEALL
                               MOVE ' ' TO S-TRACE
                           ELSE
                               MOVE ' ' TO S-TRACEALL
                           END-IF
GC0410                 WHEN COB-SCR-F8
GC0410                     IF S-SOURCE = SPACE
GC0410                         MOVE Selection-Char TO S-SOURCE
GC0410                     ELSE
GC0410                         MOVE ' ' TO S-SOURCE
GC0410                     END-IF
GC0410                 WHEN COB-SCR-F9
GC0410                     IF S-XREF = SPACE
GC0410                         MOVE Selection-Char TO S-XREF
GC0410                     ELSE
GC0410                         MOVE ' ' TO S-XREF
GC0410                     END-IF
                       WHEN COB-SCR-ESC
                           PERFORM 900-Terminate
GC0310                 WHEN CK-S-F1
                           MOVE SPACES         TO S-CfgS
                           MOVE Selection-Char TO S-Cfg-BS2000
GC0310                 WHEN CK-S-F2
                           MOVE SPACES         TO S-CfgS
                           MOVE Selection-Char TO S-Cfg-COBOL85
GC0310                 WHEN CK-S-F3
                           MOVE SPACES         TO S-CfgS
                           MOVE Selection-Char TO S-Cfg-COBOL2002
GC0310                 WHEN CK-S-F4
                           MOVE SPACES         TO S-CfgS
                           MOVE Selection-Char TO S-Cfg-DEFAULT
GC0310                 WHEN CK-S-F5
                           MOVE SPACES         TO S-CfgS
                           MOVE Selection-Char TO S-Cfg-IBM
GC0310                 WHEN CK-S-F6
                           MOVE SPACES         TO S-CfgS
                           MOVE Selection-Char TO S-Cfg-MF
GC0310                 WHEN CK-S-F7
                           MOVE SPACES         TO S-CfgS
                           MOVE Selection-Char TO S-Cfg-MVS
                       WHEN OTHER
                           MOVE 'An unsupported key was pressed'
                             TO Output-Message
                   END-EVALUATE
               ELSE
                   SET 88-No-Switch-Changes TO TRUE
               END-IF
           END-PERFORM
           .

       209-Done.
           EXIT.
      /
       210-Run-Compiler SECTION.
      *****************************************************************
      ** Run the compiler using the switch settings we've prepared.  **
      *****************************************************************

       211-Init.
           MOVE SPACES TO Cmd
                          Cobc-Cmd
                          Output-Message
           DISPLAY
               Switches-Screen
           END-DISPLAY
           MOVE 1 TO I
           EVALUATE TRUE
               WHEN S-Cfg-BS2000 NOT = SPACES
                   MOVE 'bs2000' TO Config-File
               WHEN S-Cfg-COBOL85  NOT = SPACES
                   MOVE 'cobol85' TO Config-File
               WHEN  S-Cfg-COBOL2002  NOT = SPACES
                   MOVE 'cobol2002' TO Config-File
               WHEN  S-Cfg-IBM  NOT = SPACES
                   MOVE 'ibm' TO Config-File
               WHEN  S-Cfg-MF  NOT = SPACES
                   MOVE 'mf' TO Config-File
               WHEN  S-Cfg-MVS  NOT = SPACES
                   MOVE 'mvs' TO Config-File
               WHEN OTHER
                   MOVE 'default' TO Config-File
           END-EVALUATE
           .

       212-Build-Compile-Command.
GC0909    MOVE SPACES TO Cobc-Cmd
GC0909     STRING 'cobc -std='
GC0909         TRIM(Config-File,TRAILING)
GC0909         ' '
GC0909         INTO Cobc-Cmd
GC0909         WITH POINTER I
GC0909     END-STRING
           IF S-SUBROUTINE NOT = ' '
               STRING '-m '
                   DELIMITED SIZE INTO Cobc-Cmd
                   WITH POINTER I
               END-STRING
           ELSE
               STRING '-x '
                   DELIMITED SIZE INTO Cobc-Cmd
                   WITH POINTER I
               END-STRING
           END-IF
           IF S-DEBUG NOT = ' '
               STRING '-fdebugging-line '
                   DELIMITED SIZE INTO Cobc-Cmd
                   WITH POINTER I
               END-STRING
           END-IF
           IF S-NOTRUNC NOT = ' '
               STRING '-fnotrunc '
                   DELIMITED SIZE INTO Cobc-Cmd
                   WITH POINTER I
               END-STRING
           END-IF
           IF S-TRACEALL NOT = ' '
GC0809         STRING '-ftraceall '
                   DELIMITED SIZE INTO Cobc-Cmd
                   WITH POINTER I
               END-STRING
           END-IF
           IF S-TRACE NOT = ' '
               STRING '-ftrace '
                   DELIMITED SIZE INTO Cobc-Cmd
                   WITH POINTER I
               END-STRING
           END-IF

GC0709     IF S-EXTRA > SPACES
GC0709         STRING ' '
GC0709                TRIM(S-Extra,TRAILING)
GC0709                ' '
GC0709                DELIMITED SIZE INTO Cobc-Cmd
GC0709                WITH POINTER I
GC0709         END-STRING
GC0709     END-IF
GC0909     STRING TRIM(Prog-File-Name,TRAILING)
GC0909         DELIMITED SIZE INTO Cobc-Cmd
GC0909         WITH POINTER I
GC0909     END-STRING
           .

       213-Run-Compiler.
GC0410     MOVE ' Compiling...' TO Output-Message
GC0410     DISPLAY
GC0410         Switches-Screen
GC0410     END-DISPLAY
GC0609     SET 88-Output-File-Avail TO TRUE
           MOVE SPACES TO Cmd
           STRING TRIM(Cobc-Cmd,TRAILING)
                  ' 2>'
                  TRIM(Cobc-Output-File,TRAILING)
                  DELIMITED SIZE
                  INTO Cmd
           END-STRING
           CALL 'SYSTEM'
               USING TRIM(Cmd,TRAILING)
           END-CALL
GC0909     IF RETURN-CODE = 0
GC0909         SET 88-Compile-OK TO TRUE
GC0909     ELSE
GC0909         SET 88-Compile-Failed TO TRUE
GC0909     END-IF
GC0909     IF 88-Compile-OK
GC0909         OPEN INPUT Cobc-Output
GC0909         READ Cobc-Output
GC0909             AT END
GC0909                 CONTINUE
GC0909             NOT AT END
GC0909                 SET 88-Compile-OK-Warn TO TRUE
GC0909         END-READ
GC0909         CLOSE Cobc-Output
GC0909     END-IF
GC0909     MOVE SPACES TO Output-Message
           IF 88-Compile-OK
GC0909         MOVE ' Compilation Was Successful' TO Output-Message
GC0909         DISPLAY
GC0909             Switches-Screen
GC0909         END-DISPLAY
GC0909         CALL 'C$SLEEP'
GC0909             USING 2
GC0909         END-CALL
GC0909         MOVE SPACES TO Output-Message
GC0609         SET 88-Complete TO TRUE
           ELSE
GC0909         DISPLAY
GC0909             Blank-Screen
GC0909         END-DISPLAY
GC0909         IF 88-Compile-OK-Warn
GC0909             DISPLAY ' Compilation was successful, but ' &
GC0909                     'warnings were generated:'
SCROLL*                AT LINE 24 COLUMN 1
SCROLL*                WITH SCROLL UP 1 LINE
GC0909             END-DISPLAY
GC0909         ELSE
GC0909             DISPLAY 'Compilation Failed:'
SCROLL*                AT LINE 24 COLUMN 1
SCROLL*                WITH SCROLL UP 1 LINE
GC0909             END-DISPLAY
GC0909         END-IF
GC0609         SET 88-Compile-Failed TO TRUE
GC0609         SET 88-Complete TO TRUE
GC0909         DISPLAY ' '
SCROLL*            AT LINE 24 COLUMN 1
SCROLL*            WITH SCROLL UP 1 LINE
GC0909         END-DISPLAY
GC0909         OPEN INPUT Cobc-Output
GC0909         PERFORM FOREVER
GC0909             READ Cobc-Output AT END
GC0909                 EXIT PERFORM
GC0909             END-READ
GC0909             DISPLAY TRIM(Cobc-Output-Rec,TRAILING)
SCROLL*                AT LINE 24 COLUMN 1
SCROLL*                WITH SCROLL UP 1 LINE
GC0909             END-DISPLAY
GC0909         END-PERFORM
GC0909         CLOSE Cobc-Output
GC0909         DISPLAY ' '
SCROLL*            AT LINE 24 COLUMN 1
SCROLL*            WITH SCROLL UP 2 LINES
GC0909         END-DISPLAY
GC0909         DISPLAY 'Press ENTER to close:'
SCROLL*            AT LINE 24 COLUMN 1
SCROLL*            WITH SCROLL UP 1 LINE
GC0909         END-DISPLAY
GC0909         ACCEPT Dummy
GC0909             FROM CONSOLE
GC0909         END-ACCEPT
GC0909         DISPLAY
GC0909             Blank-Screen
GC0909         END-DISPLAY
           END-IF
           .

       219-Done.
           IF 88-Compile-Failed
               PERFORM 900-Terminate
           END-IF
           .
      /
GC0410 220-Make-Listing SECTION.
GC0410*****************************************************************
GC0410** Generate a source and/or xref listing using XREF            **
GC0410*****************************************************************
GC0410
GC0410 221-Init.
GC0410     MOVE ' Generating cross-reference listing...'
GC0410       TO Output-Message
GC0410     DISPLAY
GC0410         Switches-Screen
GC0410     END-DISPLAY
GC0410     CALL "CBL_DELETE_FILE"
GC0410         USING CONCATENATE(TRIM(Prog-Name,Trailing),".lst")
GC0410     END-CALL
GC0410     MOVE 0 TO RETURN-CODE
GC0410     .
GC0410
GC0410 213-Run-OCXref.
GC0410     MOVE SPACES TO Output-Message
GC0410     CALL 'LISTING'
GC0410         USING S-SOURCE
GC0410               S-XREF
GC0410               File-Name
GC0410         ON EXCEPTION
GC0410             MOVE ' LISTING module is not available'
GC0410               TO Output-Message
GC0410             MOVE 1 TO RETURN-CODE
GC0410     END-CALL
GC0410     IF RETURN-CODE = 0
GC0410         MOVE ' Listing generated'
GC0410           TO Output-Message
GC0410         IF OS-Windows OR OS-Cygwin
GC0410             MOVE SPACES TO Cmd
GC0410             STRING
GC0410                 'cmd /c '
GC0410                 TRIM(Prog-Name,TRAILING)
GC0410                 '.lst'
GC0410                 DELIMITED SIZE INTO Cmd
GC0410             END-STRING
GC0410             CALL 'SYSTEM'
GC0410                 USING TRIM(Cmd,TRAILING)
GC0410             END-CALL
GC0410         END-IF
GC0410     ELSE
GC0410         IF Output-Message = SPACES
GC0410             MOVE ' Listing generation failed'
GC0410               TO Output-Message
GC0410         END-IF
GC0410     END-IF
GC0410     DISPLAY
GC0410         Switches-Screen
GC0410     END-DISPLAY
GC0410     CALL 'C$SLEEP'
GC0410         USING 2
GC0410     END-CALL
GC0410     .
      /
       230-Run-Program SECTION.
      *****************************************************************
      ** Run the compiled program                                    **
      *****************************************************************

       232-Build-Command.
GC0909     MOVE SPACES TO Cmd
GC0909     MOVE 1 TO I
           IF S-SUBROUTINE NOT = ' '
           OR S-DLL NOT = ' '
               STRING 'cobcrun ' DELIMITED SIZE
                      INTO Cmd
                      WITH POINTER I
               END-STRING
           END-IF
           IF Prog-Folder NOT = SPACES
GC0909         IF OS-Cygwin AND Prog-Folder (2:1) = ':'
GC0909             STRING '/cygdrive/'
GC0909                 INTO Cmd
GC0909                 WITH POINTER I
GC0909             END-STRING
GC0909             STRING LOWER-CASE(Prog-Folder (1:1))
GC0909                 INTO Cmd
GC0909                 WITH POINTER I
GC0909             END-STRING
GC0909             PERFORM VARYING J FROM 3 BY 1
GC0909                       UNTIL J > LENGTH(TRIM(Prog-Folder))
GC0909                 IF Prog-Folder (J:1) = '\'
GC0909                     STRING '/'
GC0909                         INTO Cmd
GC0909                         WITH POINTER I
GC0909                     END-STRING
GC0909                 ELSE
GC0909                     STRING Prog-Folder (J:1)
GC0909                         INTO Cmd
GC0909                         WITH POINTER I
GC0909                     END-STRING
GC0909                 END-IF
GC0909             END-PERFORM
GC0909         ELSE
GC0410             STRING '"' TRIM(Prog-Folder,TRAILING)
GC0909                 INTO Cmd
GC0909                 WITH POINTER I
GC0909             END-STRING
GC0909         END-IF
GC0909         STRING Dir-Char
GC0909             INTO Cmd
GC0909             WITH POINTER I
GC0909         END-STRING
GC0909     ELSE
GC0909         IF OS-Cygwin OR OS-UNIX
GC0909             STRING './'
GC0909                 INTO Cmd
GC0909                 WITH POINTER I
GC0909             END-STRING
GC0909         END-IF
           END-IF
GC0909     STRING TRIM(Prog-Name,TRAILING)
GC0909         INTO Cmd
GC0909         WITH POINTER I
GC0909     END-STRING
GC0909     IF S-SUBROUTINE = ' '
GC0909     AND S-DLL NOT = ' '
GC0909         STRING '.exe' DELIMITED SIZE
                      INTO Cmd
                      WITH POINTER I
               END-STRING
           END-IF
           IF S-ARGS NOT = SPACES
GC0809         STRING ' ' TRIM(S-ARGS,TRAILING)
                   INTO Cmd
                   WITH POINTER I
               END-STRING
           END-IF
           IF OS-Unknown OR OS-Windows
GC0410         STRING '"&&pause'
                   INTO Cmd
                   WITH POINTER I
               END-STRING
           ELSE
               STRING ';echo "Press ENTER to close...";read'
                   INTO Cmd
                   WITH POINTER I
               END-STRING
           END-IF
           .

       233-Run-Program.
GC0909     DISPLAY
GC0909         Blank-Screen
GC0909     END-DISPLAY

           CALL 'SYSTEM'
               USING TRIM(Cmd,TRAILING)
           END-CALL
           PERFORM 900-Terminate
           .

       239-Done.
           EXIT.
      /
       900-Terminate SECTION.
      *****************************************************************
      ** Display a message and halt the program                      **
      *****************************************************************

       901-Display-Message.
GC0909     IF Output-Message > SPACES
GC0909         DISPLAY
GC0909             Switches-Screen
GC0909         END-DISPLAY
GC0909         CALL 'C$SLEEP'
GC0909             USING 2
GC0909         END-CALL
GC0909     END-IF
           DISPLAY
               Blank-Screen
           END-DISPLAY
           .

       909-Done.
           GOBACK
           .

       END PROGRAM OCic.

       IDENTIFICATION DIVISION.
       PROGRAM-ID.  GETOSTYPE.
      *****************************************************************
      ** This subprogram determine the OS type the program is run-   **
      ** ning under, passing that result back in RETURN-CODE as fol- **
      ** lows:                                                       **
      **                                                             **
      ** 0:   Cannot be determined                                   **
      ** 1:   Native Windows or Windows/MinGW                        **
      ** 2:   Cygwin                                                 **
      ** 3:   UNIX/Linux/MacOS                                       **
      *****************************************************************
      **  DATE  CHANGE DESCRIPTION                                   **
      ** ====== ==================================================== **
      ** GC0909 Initial coding.                                      **
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  Env-Path                    PIC X(1024).
       01  Tally                       USAGE BINARY-LONG.
       PROCEDURE DIVISION.
       000-Main SECTION.
       010-Get-TEMP-Var.
           MOVE SPACES TO Env-Path
           ACCEPT Env-Path
               FROM ENVIRONMENT "PATH"
               ON EXCEPTION
                   MOVE 0 TO RETURN-CODE
                   GOBACK
           END-ACCEPT
           IF Env-Path = SPACES
               MOVE 0 TO RETURN-CODE
           ELSE
               MOVE 0 TO Tally
               INSPECT Env-Path
                   TALLYING Tally FOR ALL ";"
               IF Tally = 0 *> Must be some form of UNIX
                   MOVE 0 TO Tally
                   INSPECT Env-Path
                       TALLYING TALLY FOR ALL "/cygdrive/"
                   IF Tally = 0 *> UNIX/MacOS
                       MOVE 3 TO RETURN-CODE
                   ELSE *> Cygwin
                       MOVE 2 TO RETURN-CODE
                   END-IF
               ELSE *> Assume Windows[/MinGW]
                   MOVE 1 TO RETURN-CODE
               END-IF
           END-IF
           GOBACK
           .
       END PROGRAM GETOSTYPE.

       IDENTIFICATION DIVISION.
       PROGRAM-ID.  CHECKSOURCE.
      *****************************************************************
      ** This subprogram will scan a line of source code it is given **
      ** looking for "LINKAGE SECTION" or "IDENTIFICATION DIVISION". **
      **                                                             **
      **  ****NOTE****   ****NOTE****    ****NOTE****   ****NOTE***  **
      **                                                             **
      ** These two strings must be found IN THEIR ENTIRETY within    **
      ** the 1st 80 columns of program source records, and cannot    **
      ** follow either a "*>" sequence OR a "*" in col 7.            **
      *****************************************************************
      **  DATE  CHANGE DESCRIPTION                                   **
      ** ====== ==================================================== **
      ** GC0809 Initial coding.                                      **
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  Compressed-Src.
           05 CS-Char                  OCCURS 80 TIMES PIC X(1).

       01  Flags.
           05 F-Found-SPACE            PIC X(1).
              88 88-Skipping-SPACE     VALUE 'Y'.
              88 88-Not-Skipping-SPACE VALUE 'N'.

       01  I                           USAGE BINARY-CHAR.

       01  J                           USAGE BINARY-CHAR.
       LINKAGE SECTION.
       01  Argument-1.
           02 A1-Char                  OCCURS 80 TIMES PIC X(1).

       01  Argument-2                  PIC X(1).
           88 88-A2-LINKAGE-SECTION         VALUE 'L'.
           88 88-A2-IDENTIFICATION-DIVISION VALUE 'I'.
           88 88-A2-Nothing-Special         VALUE ' '.
       PROCEDURE DIVISION USING Argument-1, Argument-2.
       000-Main SECTION.

       010-Initialize.
           SET 88-A2-Nothing-Special TO TRUE
           IF A1-Char (7) = '*'
               GOBACK
           END-IF
           .

       020-Compress-Multiple-SPACES.
           SET 88-Not-Skipping-SPACE TO TRUE
           MOVE 0 TO J
           MOVE SPACES TO Compressed-Src
           PERFORM VARYING I FROM 1 BY 1
                     UNTIL I > 80
               IF A1-Char (I) = SPACE
                   IF 88-Not-Skipping-SPACE
                       ADD 1 TO J
                       MOVE UPPER-CASE(A1-Char (I)) TO CS-Char (J)
                       SET 88-Skipping-SPACE TO TRUE
                   END-IF
               ELSE
                   SET 88-Not-Skipping-SPACE TO TRUE
                   ADD 1 TO J
                   MOVE A1-Char (I) TO CS-Char (J)
               END-IF
           END-PERFORM
           .

       030-Scan-Compressed-Src.
           PERFORM VARYING I FROM 1 BY 1
                     UNTIL I > 66
               EVALUATE TRUE
                   WHEN CS-Char (I) = '*'
                       IF Compressed-Src (I : 2) = '*>'
                           GOBACK
                       END-IF
                   WHEN (CS-Char (I) = 'L') AND (I < 66)
                       IF Compressed-Src (I : 15) = 'LINKAGE SECTION'
                           SET 88-A2-LINKAGE-SECTION TO TRUE
                           GOBACK
                       END-IF
                   WHEN (CS-Char (I) = 'I') AND (I < 58)
                       IF Compressed-Src (I : 23) = 'IDENTIFICATION ' &
                                                       'DIVISION'
                           SET 88-A2-IDENTIFICATION-DIVISION TO TRUE
                           GOBACK
                       END-IF
               END-EVALUATE
           END-PERFORM
           .

       099-Never-Found-Either-One.
           GOBACK
           .
       END PROGRAM CHECKSOURCE.

       IDENTIFICATION DIVISION.
       PROGRAM-ID.  LISTING.
      *****************************************************************
      ** This subprogram generates a cross-reference listing of an   **
      ** OpenCOBOL program.                                          **
      **                                                             **
      ** Linkage:      CALL "LISTING" USING <source>                 **
      **                                    <xref>                   **
      **                                    <filename>               **
      **                                                             **
      **               Where:                                        **
      **                  <source>   is a PIC X(1) flag indicating   **
      **                             whether or not a source listing **
      **                             should be produced (space=NO,   **
      **                             non-space=yes)                  **
      **                  <xref>     is a PIC X(1) flag indicating   **
      **                             whether or not an xref listing  **
      **                             should be produced (space=NO,   **
      **                             non-space=yes)                  **
      **                  <filename> is the [path]filename of the    **
      **                             program being listed and/or     **
      **                             xreffed in a PIC X(256) form.   **
      *****************************************************************
      **                                                             **
      ** AUTHOR:       GARY L. CUTLER                                **
      **               CutlerGL@gmail.com                            **
      **               Copyright (C) 2010, Gary L. Cutler, GPL       **
      **                                                             **
      ** DATE-WRITTEN: April 1, 2010                                 **
      **                                                             **
      *****************************************************************
      **  DATE  CHANGE DESCRIPTION                                   **
      ** ====== ==================================================== **
      ** GC0410 Initial coding                                       **
      ** GC0710 Handle duplicate data names (i.e. "CORRESPONDING" or **
      **        qualified items) better; ignore "END PROGRAM" recs   **
      **        so program name doesn't appear in listing.           **
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT Expand-Code          ASSIGN TO Expanded-Src-Filename
                                       ORGANIZATION IS LINE SEQUENTIAL.
           SELECT Report-File          ASSIGN TO Report-Filename
                                       ORGANIZATION IS LINE SEQUENTIAL.
           SELECT Sort-File            ASSIGN TO DISK.
           SELECT Source-Code          ASSIGN TO Src-Filename
                                       ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  Expand-Code.
       01  Expand-Code-Rec.
           05 ECR-1                    PIC X.
           05 ECR-2-256                PIC X(256).
       01  Expand-Code-Rec-Alt.
           05 ECR-1-128                PIC X(128).
           05 ECR-129-256              PIC X(128).

       FD  Report-File.
       01  Report-Rec                  PIC X(135).

       SD  Sort-File.
       01  Sort-Rec.
           05 SR-Prog-ID               PIC X(15).
           05 SR-Token-UC              PIC X(32).
           05 SR-Token                 PIC X(32).
           05 SR-Section               PIC X(15).
           05 SR-Line-No-Def           PIC 9(6).
           05 SR-Reference.
              10 SR-Line-No-Ref        PIC 9(6).
              10 SR-Ref-Flag           PIC X(1).

       FD  Source-Code.
       01  Source-Code-Rec.
GC0410     05 SCR-1-128.
GC0410        10 FILLER                PIC X(6).
GC0410        10 SCR-7                 PIC X(1).
GC0410        10 FILLER                PIC X(121).
           05 SCR-129-256              PIC X(128).

       WORKING-STORAGE SECTION.
       78  Line-Nos-Per-Rec            VALUE 8.

       01  Cmd                         PIC X(256).

       01  Delim                       PIC X(2).

       01  Detail-Line-S.
           05 DLS-Line-No              PIC ZZZZZ9.
           05 FILLER                   PIC X(1).
           05 DLS-Statement            PIC X(128).

       01  Detail-Line-X.
           05 DLX-Prog-ID              PIC X(15).
           05 FILLER                   PIC X(1).
           05 DLX-Token                PIC X(32).
           05 FILLER                   PIC X(1).
           05 DLX-Line-No-Def          PIC ZZZZZ9.
           05 FILLER                   PIC X(1).
           05 DLX-Section              PIC X(15).
           05 FILLER                   PIC X(1).
           05 DLX-Reference            OCCURS Line-Nos-Per-Rec TIMES.
              10 DLX-Line-No-Ref       PIC ZZZZZ9.
              10 DLX-Ref-Flag          PIC X(1).
              10 FILLER                PIC X(1).

       01  Dummy                       PIC X(1).

       01  Env-TEMP                    PIC X(256).

       01  Expanded-Src-Filename       PIC X(256).

       01  Filename                    PIC X(256).

       01  Flags.
GC0710     05 F-Duplicate              PIC X(1).
           05 F-First-Record           PIC X(1).
           05 F-In-Which-Pgm           PIC X(1).
              88 In-Main-Module        VALUE 'M'.
              88 In-Copybook           VALUE 'C'.
           05 F-Last-Token-Ended-Sent  PIC X(1).
           05 F-Processing-PICTURE     PIC X(1).
           05 F-Token-Ended-Sentence   PIC X(1).
GC0710     05 F-Verb-Has-Been-Found    PIC X(1).

       01  Group-Indicators.
           05 GI-Prog-ID               PIC X(15).
           05 GI-Token                 PIC X(32).

       01  Heading-1S.
           05 FILLER                   PIC X(125) VALUE
              "OpenCOBOL 1.1 06FEB2009 Source Listing - " &
              "OCic Copyright (C) 2009-2010, Gary L. Cutler, GPL".
           05 H1S-Date                 PIC 9999/99/99.

       01  Heading-1X.
           05 FILLER                   PIC X(125) VALUE
              "OpenCOBOL 1.1 06FEB2009 Cross-Reference Listing - " &
              "OCic Copyright (C) 2009-2010, Gary L. Cutler, GPL".
           05 H1X-Date                 PIC 9999/99/99.

       01  Heading-2                   PIC X(135).

       01  Heading-4S                  PIC X(16) VALUE
           "Line   Statement".

       01  Heading-4X                  PIC X(96) VALUE
           "PROGRAM-ID      Identifier/Register/Function     Defn   Wher
      -    "e Defined   References (* = Updated)".

       01  Heading-5S                  PIC X(135) VALUE
           "====== =====================================================
      -    "============================================================
      -    "===============".

       01  Heading-5X                  PIC X(135) VALUE
           "=============== ================================ ====== ====
      -    "=========== ================================================
      -    "===============".

       01  Held-Reference              PIC X(100).

       01  I                           USAGE BINARY-LONG.

       01  J                           USAGE BINARY-LONG.

       01  Lines-Left                  USAGE BINARY-LONG.

       01  Lines-Per-Page              USAGE BINARY-LONG.

       01  Lines-Per-Page-ENV          PIC X(256).

       01  Num-UserNames               USAGE BINARY-LONG.

       01  PIC-X10                     PIC X(10).

       01  PIC-X32                     PIC X(32).

       01  PIC-X256                    PIC X(256).

       01  Program-Path                PIC X(256).

       01  Report-Filename             PIC X(256).

       01  Reserved-Words.
           05 FILLER PIC X(33) VALUE "IABS".
           05 FILLER PIC X(33) VALUE "VACCEPT".
           05 FILLER PIC X(33) VALUE " ACCESS".
           05 FILLER PIC X(33) VALUE "IACOS".
           05 FILLER PIC X(33) VALUE " ACTIVE-CLASS".
           05 FILLER PIC X(33) VALUE "VADD".
           05 FILLER PIC X(33) VALUE " ADDRESS".
           05 FILLER PIC X(33) VALUE " ADVANCING".
           05 FILLER PIC X(33) VALUE "KAFTER".
           05 FILLER PIC X(33) VALUE " ALIGNED".
           05 FILLER PIC X(33) VALUE " ALL".
           05 FILLER PIC X(33) VALUE "VALLOCATE".
           05 FILLER PIC X(33) VALUE " ALPHABET".
           05 FILLER PIC X(33) VALUE " ALPHABETIC".
           05 FILLER PIC X(33) VALUE " ALPHABETIC-LOWER".
           05 FILLER PIC X(33) VALUE " ALPHABETIC-UPPER".
           05 FILLER PIC X(33) VALUE " ALPHANUMERIC".
           05 FILLER PIC X(33) VALUE " ALPHANUMERIC-EDITED".
           05 FILLER PIC X(33) VALUE " ALSO".
           05 FILLER PIC X(33) VALUE "VALTER".
           05 FILLER PIC X(33) VALUE " ALTERNATE".
           05 FILLER PIC X(33) VALUE " AND".
           05 FILLER PIC X(33) VALUE "IANNUITY".
           05 FILLER PIC X(33) VALUE " ANY".
           05 FILLER PIC X(33) VALUE " ANYCASE".
           05 FILLER PIC X(33) VALUE " ARE".
           05 FILLER PIC X(33) VALUE " AREA".
           05 FILLER PIC X(33) VALUE " AREAS".
           05 FILLER PIC X(33) VALUE " ARGUMENT-NUMBER".
           05 FILLER PIC X(33) VALUE " ARGUMENT-VALUE".
           05 FILLER PIC X(33) VALUE " AS".
           05 FILLER PIC X(33) VALUE " ASCENDING".
           05 FILLER PIC X(33) VALUE "IASIN".
           05 FILLER PIC X(33) VALUE " ASSIGN".
           05 FILLER PIC X(33) VALUE " AT".
           05 FILLER PIC X(33) VALUE "IATAN".
           05 FILLER PIC X(33) VALUE " AUTHOR".
           05 FILLER PIC X(33) VALUE " AUTO".
           05 FILLER PIC X(33) VALUE " AUTO-SKIP".
           05 FILLER PIC X(33) VALUE " AUTOMATIC".
           05 FILLER PIC X(33) VALUE " AUTOTERMINATE".
           05 FILLER PIC X(33) VALUE " BACKGROUND-COLOR".
           05 FILLER PIC X(33) VALUE " BASED".
           05 FILLER PIC X(33) VALUE " BEEP".
           05 FILLER PIC X(33) VALUE " BEFORE".
           05 FILLER PIC X(33) VALUE " BELL".
           05 FILLER PIC X(33) VALUE " BINARY".
           05 FILLER PIC X(33) VALUE " BINARY-C-LONG".
           05 FILLER PIC X(33) VALUE " BINARY-CHAR".
           05 FILLER PIC X(33) VALUE " BINARY-DOUBLE".
           05 FILLER PIC X(33) VALUE " BINARY-LONG".
           05 FILLER PIC X(33) VALUE " BINARY-SHORT".
           05 FILLER PIC X(33) VALUE " BIT".
           05 FILLER PIC X(33) VALUE " BLANK".
           05 FILLER PIC X(33) VALUE " BLINK".
           05 FILLER PIC X(33) VALUE " BLOCK".
           05 FILLER PIC X(33) VALUE " BOOLEAN".
           05 FILLER PIC X(33) VALUE " BOTTOM".
           05 FILLER PIC X(33) VALUE "YBY".
           05 FILLER PIC X(33) VALUE "IBYTE-LENGTH".
           05 FILLER PIC X(33) VALUE "MC01".
           05 FILLER PIC X(33) VALUE "MC02".
           05 FILLER PIC X(33) VALUE "MC03".
           05 FILLER PIC X(33) VALUE "MC04".
           05 FILLER PIC X(33) VALUE "MC05".
           05 FILLER PIC X(33) VALUE "MC06".
           05 FILLER PIC X(33) VALUE "MC07".
           05 FILLER PIC X(33) VALUE "MC08".
           05 FILLER PIC X(33) VALUE "MC09".
           05 FILLER PIC X(33) VALUE "MC10".
           05 FILLER PIC X(33) VALUE "MC11".
           05 FILLER PIC X(33) VALUE "MC12".
           05 FILLER PIC X(33) VALUE "VCALL".
           05 FILLER PIC X(33) VALUE "VCANCEL".
           05 FILLER PIC X(33) VALUE " CF".
           05 FILLER PIC X(33) VALUE " CH".
           05 FILLER PIC X(33) VALUE " CHAINING".
           05 FILLER PIC X(33) VALUE "ICHAR".
           05 FILLER PIC X(33) VALUE " CHARACTER".
           05 FILLER PIC X(33) VALUE " CHARACTERS".
           05 FILLER PIC X(33) VALUE " CLASS".
           05 FILLER PIC X(33) VALUE " CLASS-ID".
           05 FILLER PIC X(33) VALUE "VCLOSE".
           05 FILLER PIC X(33) VALUE "ICOB-CRT-STATUS".
           05 FILLER PIC X(33) VALUE " CODE".
           05 FILLER PIC X(33) VALUE " CODE-SET".
           05 FILLER PIC X(33) VALUE " COL".
           05 FILLER PIC X(33) VALUE " COLLATING".
           05 FILLER PIC X(33) VALUE " COLS".
           05 FILLER PIC X(33) VALUE " COLUMN".
           05 FILLER PIC X(33) VALUE " COLUMNS".
           05 FILLER PIC X(33) VALUE "ICOMBINED-DATETIME".
           05 FILLER PIC X(33) VALUE " COMMA".
           05 FILLER PIC X(33) VALUE " COMMAND-LINE".
           05 FILLER PIC X(33) VALUE "VCOMMIT".
           05 FILLER PIC X(33) VALUE " COMMON".
           05 FILLER PIC X(33) VALUE " COMP".
           05 FILLER PIC X(33) VALUE " COMP-1".
           05 FILLER PIC X(33) VALUE " COMP-2".
           05 FILLER PIC X(33) VALUE " COMP-3".
           05 FILLER PIC X(33) VALUE " COMP-4".
           05 FILLER PIC X(33) VALUE " COMP-5".
           05 FILLER PIC X(33) VALUE " COMP-X".
           05 FILLER PIC X(33) VALUE " COMPUTATIONAL".
           05 FILLER PIC X(33) VALUE " COMPUTATIONAL-1".
           05 FILLER PIC X(33) VALUE " COMPUTATIONAL-2".
           05 FILLER PIC X(33) VALUE " COMPUTATIONAL-3".
           05 FILLER PIC X(33) VALUE " COMPUTATIONAL-4".
           05 FILLER PIC X(33) VALUE " COMPUTATIONAL-5".
           05 FILLER PIC X(33) VALUE " COMPUTATIONAL-X".
           05 FILLER PIC X(33) VALUE "VCOMPUTE".
           05 FILLER PIC X(33) VALUE "ICONCATENATE".
           05 FILLER PIC X(33) VALUE " CONDITION".
           05 FILLER PIC X(33) VALUE "KCONFIGURATION".
           05 FILLER PIC X(33) VALUE "MCONSOLE".
           05 FILLER PIC X(33) VALUE " CONSTANT".
           05 FILLER PIC X(33) VALUE " CONTAINS".
           05 FILLER PIC X(33) VALUE " CONTENT".
           05 FILLER PIC X(33) VALUE "VCONTINUE".
           05 FILLER PIC X(33) VALUE " CONTROL".
           05 FILLER PIC X(33) VALUE " CONTROLS".
           05 FILLER PIC X(33) VALUE "KCONVERTING".
           05 FILLER PIC X(33) VALUE " COPY".
           05 FILLER PIC X(33) VALUE " CORR".
           05 FILLER PIC X(33) VALUE " CORRESPONDING".
           05 FILLER PIC X(33) VALUE "ICOS".
           05 FILLER PIC X(33) VALUE "KCOUNT".
           05 FILLER PIC X(33) VALUE " CRT".
           05 FILLER PIC X(33) VALUE " CURRENCY".
           05 FILLER PIC X(33) VALUE "ICURRENT-DATE".
           05 FILLER PIC X(33) VALUE " CURSOR".
           05 FILLER PIC X(33) VALUE " CYCLE".
           05 FILLER PIC X(33) VALUE "KDATA".
           05 FILLER PIC X(33) VALUE " DATA-POINTER".
           05 FILLER PIC X(33) VALUE " DATE".
           05 FILLER PIC X(33) VALUE " DATE-COMPILED".
           05 FILLER PIC X(33) VALUE " DATE-MODIFIED".
           05 FILLER PIC X(33) VALUE "IDATE-OF-INTEGER".
           05 FILLER PIC X(33) VALUE "IDATE-TO-YYYYMMDD".
           05 FILLER PIC X(33) VALUE " DATE-WRITTEN".
           05 FILLER PIC X(33) VALUE " DAY".
           05 FILLER PIC X(33) VALUE "IDAY-OF-INTEGER".
           05 FILLER PIC X(33) VALUE " DAY-OF-WEEK".
           05 FILLER PIC X(33) VALUE "IDAY-TO-YYYYDDD".
           05 FILLER PIC X(33) VALUE " DE".
           05 FILLER PIC X(33) VALUE " DEBUGGING".
           05 FILLER PIC X(33) VALUE " DECIMAL-POINT".
           05 FILLER PIC X(33) VALUE " DECLARATIVES".
           05 FILLER PIC X(33) VALUE " DEFAULT".
           05 FILLER PIC X(33) VALUE "VDELETE".
           05 FILLER PIC X(33) VALUE " DELIMITED".
           05 FILLER PIC X(33) VALUE "KDELIMITER".
           05 FILLER PIC X(33) VALUE " DEPENDING".
           05 FILLER PIC X(33) VALUE " DESCENDING".
           05 FILLER PIC X(33) VALUE " DESTINATION".
           05 FILLER PIC X(33) VALUE " DETAIL".
           05 FILLER PIC X(33) VALUE " DISABLE".
           05 FILLER PIC X(33) VALUE " DISK".
           05 FILLER PIC X(33) VALUE "VDISPLAY".
           05 FILLER PIC X(33) VALUE "VDIVIDE".
           05 FILLER PIC X(33) VALUE "KDIVISION".
           05 FILLER PIC X(33) VALUE "KDOWN".
           05 FILLER PIC X(33) VALUE " DUPLICATES".
           05 FILLER PIC X(33) VALUE " DYNAMIC".
           05 FILLER PIC X(33) VALUE "IE".
           05 FILLER PIC X(33) VALUE " EBCDIC".
           05 FILLER PIC X(33) VALUE " EC".
           05 FILLER PIC X(33) VALUE "VELSE".
GC0710     05 FILLER PIC X(33) VALUE "KEND".
           05 FILLER PIC X(33) VALUE " END-ACCEPT".
           05 FILLER PIC X(33) VALUE " END-ADD".
           05 FILLER PIC X(33) VALUE " END-CALL".
           05 FILLER PIC X(33) VALUE " END-COMPUTE".
           05 FILLER PIC X(33) VALUE " END-DELETE".
           05 FILLER PIC X(33) VALUE " END-DISPLAY".
           05 FILLER PIC X(33) VALUE " END-DIVIDE".
           05 FILLER PIC X(33) VALUE " END-EVALUATE".
           05 FILLER PIC X(33) VALUE " END-IF".
           05 FILLER PIC X(33) VALUE " END-MULTIPLY".
           05 FILLER PIC X(33) VALUE " END-OF-PAGE".
           05 FILLER PIC X(33) VALUE " END-PERFORM".
           05 FILLER PIC X(33) VALUE " END-READ".
           05 FILLER PIC X(33) VALUE " END-RETURN".
           05 FILLER PIC X(33) VALUE " END-REWRITE".
           05 FILLER PIC X(33) VALUE " END-SEARCH".
           05 FILLER PIC X(33) VALUE " END-START".
           05 FILLER PIC X(33) VALUE " END-STRING".
           05 FILLER PIC X(33) VALUE " END-SUBTRACT".
           05 FILLER PIC X(33) VALUE " END-UNSTRING".
           05 FILLER PIC X(33) VALUE " END-WRITE".
           05 FILLER PIC X(33) VALUE "VENTRY".
           05 FILLER PIC X(33) VALUE "KENVIRONMENT".
           05 FILLER PIC X(33) VALUE " ENVIRONMENT-NAME".
           05 FILLER PIC X(33) VALUE " ENVIRONMENT-VALUE".
           05 FILLER PIC X(33) VALUE " EO".
           05 FILLER PIC X(33) VALUE " EOL".
           05 FILLER PIC X(33) VALUE " EOP".
           05 FILLER PIC X(33) VALUE " EOS".
           05 FILLER PIC X(33) VALUE " EQUAL".
           05 FILLER PIC X(33) VALUE "KEQUALS".
           05 FILLER PIC X(33) VALUE " ERASE".
           05 FILLER PIC X(33) VALUE " ERROR".
           05 FILLER PIC X(33) VALUE " ESCAPE".
           05 FILLER PIC X(33) VALUE "VEVALUATE".
           05 FILLER PIC X(33) VALUE " EXCEPTION".
           05 FILLER PIC X(33) VALUE "IEXCEPTION-FILE".
           05 FILLER PIC X(33) VALUE "IEXCEPTION-LOCATION".
           05 FILLER PIC X(33) VALUE " EXCEPTION-OBJECT".
           05 FILLER PIC X(33) VALUE "IEXCEPTION-STATEMENT".
           05 FILLER PIC X(33) VALUE "IEXCEPTION-STATUS".
           05 FILLER PIC X(33) VALUE " EXCLUSIVE".
           05 FILLER PIC X(33) VALUE "VEXIT".
           05 FILLER PIC X(33) VALUE "IEXP".
           05 FILLER PIC X(33) VALUE "IEXP10".
           05 FILLER PIC X(33) VALUE " EXTEND".
           05 FILLER PIC X(33) VALUE " EXTERNAL".
           05 FILLER PIC X(33) VALUE "IFACTORIAL".
           05 FILLER PIC X(33) VALUE " FACTORY".
           05 FILLER PIC X(33) VALUE " FALSE".
           05 FILLER PIC X(33) VALUE "KFD".
           05 FILLER PIC X(33) VALUE "KFILE".
           05 FILLER PIC X(33) VALUE " FILE-CONTROL".
           05 FILLER PIC X(33) VALUE " FILE-ID".
           05 FILLER PIC X(33) VALUE " FILLER".
           05 FILLER PIC X(33) VALUE " FINAL".
           05 FILLER PIC X(33) VALUE " FIRST".
           05 FILLER PIC X(33) VALUE " FLOAT-BINARY-16".
           05 FILLER PIC X(33) VALUE " FLOAT-BINARY-34".
           05 FILLER PIC X(33) VALUE " FLOAT-BINARY-7".
           05 FILLER PIC X(33) VALUE " FLOAT-DECIMAL-16".
           05 FILLER PIC X(33) VALUE " FLOAT-DECIMAL-34".
           05 FILLER PIC X(33) VALUE " FLOAT-EXTENDED".
           05 FILLER PIC X(33) VALUE " FLOAT-LONG".
           05 FILLER PIC X(33) VALUE " FLOAT-SHORT".
           05 FILLER PIC X(33) VALUE " FOOTING".
           05 FILLER PIC X(33) VALUE " FOR".
           05 FILLER PIC X(33) VALUE " FOREGROUND-COLOR".
           05 FILLER PIC X(33) VALUE " FOREVER".
           05 FILLER PIC X(33) VALUE " FORMAT".
           05 FILLER PIC X(33) VALUE "MFORMFEED".
           05 FILLER PIC X(33) VALUE "IFRACTION-PART".
           05 FILLER PIC X(33) VALUE "VFREE".
           05 FILLER PIC X(33) VALUE " FROM".
           05 FILLER PIC X(33) VALUE " FULL".
           05 FILLER PIC X(33) VALUE " FUNCTION".
           05 FILLER PIC X(33) VALUE " FUNCTION-ID".
           05 FILLER PIC X(33) VALUE " FUNCTION-POINTER".
           05 FILLER PIC X(33) VALUE "VGENERATE".
           05 FILLER PIC X(33) VALUE " GET".
           05 FILLER PIC X(33) VALUE "KGIVING".
           05 FILLER PIC X(33) VALUE " GLOBAL".
           05 FILLER PIC X(33) VALUE "VGO".
           05 FILLER PIC X(33) VALUE "VGOBACK".
           05 FILLER PIC X(33) VALUE " GREATER".
           05 FILLER PIC X(33) VALUE " GROUP".
           05 FILLER PIC X(33) VALUE " GROUP-USAGE".
           05 FILLER PIC X(33) VALUE " HEADING".
           05 FILLER PIC X(33) VALUE " HIGH-VALUE".
           05 FILLER PIC X(33) VALUE " HIGH-VALUES".
           05 FILLER PIC X(33) VALUE " HIGHLIGHT".
           05 FILLER PIC X(33) VALUE " I-O".
           05 FILLER PIC X(33) VALUE " I-O-CONTROL".
           05 FILLER PIC X(33) VALUE "KID".
           05 FILLER PIC X(33) VALUE "KIDENTIFICATION".
           05 FILLER PIC X(33) VALUE "VIF".
           05 FILLER PIC X(33) VALUE " IGNORE".
           05 FILLER PIC X(33) VALUE " IGNORING".
           05 FILLER PIC X(33) VALUE " IN".
           05 FILLER PIC X(33) VALUE " INDEX".
           05 FILLER PIC X(33) VALUE "KINDEXED".
           05 FILLER PIC X(33) VALUE " INDICATE".
           05 FILLER PIC X(33) VALUE " INFINITY".
           05 FILLER PIC X(33) VALUE " INHERITS".
           05 FILLER PIC X(33) VALUE " INITIAL".
           05 FILLER PIC X(33) VALUE " INITIALISED".
           05 FILLER PIC X(33) VALUE "VINITIALIZE".
           05 FILLER PIC X(33) VALUE " INITIALIZED".
           05 FILLER PIC X(33) VALUE "VINITIATE".
           05 FILLER PIC X(33) VALUE " INPUT".
           05 FILLER PIC X(33) VALUE "KINPUT-OUTPUT".
           05 FILLER PIC X(33) VALUE "VINSPECT".
           05 FILLER PIC X(33) VALUE " INSTALLATION".
           05 FILLER PIC X(33) VALUE "IINTEGER".
           05 FILLER PIC X(33) VALUE "IINTEGER-OF-DATE".
           05 FILLER PIC X(33) VALUE "IINTEGER-OF-DAY".
           05 FILLER PIC X(33) VALUE "IINTEGER-PART".
           05 FILLER PIC X(33) VALUE " INTERFACE".
           05 FILLER PIC X(33) VALUE " INTERFACE-ID".
           05 FILLER PIC X(33) VALUE "KINTO".
           05 FILLER PIC X(33) VALUE " INTRINSIC".
           05 FILLER PIC X(33) VALUE " INVALID".
           05 FILLER PIC X(33) VALUE " INVOKE".
           05 FILLER PIC X(33) VALUE " IS".
           05 FILLER PIC X(33) VALUE " JUST".
           05 FILLER PIC X(33) VALUE " JUSTIFIED".
           05 FILLER PIC X(33) VALUE " KEY".
           05 FILLER PIC X(33) VALUE " LABEL".
           05 FILLER PIC X(33) VALUE " LAST".
           05 FILLER PIC X(33) VALUE " LEADING".
           05 FILLER PIC X(33) VALUE " LEFT".
           05 FILLER PIC X(33) VALUE " LEFT-JUSTIFY".
           05 FILLER PIC X(33) VALUE "ILENGTH".
           05 FILLER PIC X(33) VALUE " LESS".
           05 FILLER PIC X(33) VALUE " LIMIT".
           05 FILLER PIC X(33) VALUE " LIMITS".
           05 FILLER PIC X(33) VALUE " LINAGE".
           05 FILLER PIC X(33) VALUE "ILINAGE-COUNTER".
           05 FILLER PIC X(33) VALUE " LINE".
           05 FILLER PIC X(33) VALUE " LINE-COUNTER".
           05 FILLER PIC X(33) VALUE " LINES".
           05 FILLER PIC X(33) VALUE "KLINKAGE".
           05 FILLER PIC X(33) VALUE "KLOCAL-STORAGE".
           05 FILLER PIC X(33) VALUE " LOCALE".
           05 FILLER PIC X(33) VALUE "ILOCALE-DATE".
           05 FILLER PIC X(33) VALUE "ILOCALE-TIME".
           05 FILLER PIC X(33) VALUE "ILOCALE-TIME-FROM-SECONDS".
           05 FILLER PIC X(33) VALUE " LOCK".
           05 FILLER PIC X(33) VALUE "ILOG".
           05 FILLER PIC X(33) VALUE "ILOG10".
           05 FILLER PIC X(33) VALUE " LOW-VALUE".
           05 FILLER PIC X(33) VALUE " LOW-VALUES".
           05 FILLER PIC X(33) VALUE " LOWER".
           05 FILLER PIC X(33) VALUE "ILOWER-CASE".
           05 FILLER PIC X(33) VALUE " LOWLIGHT".
           05 FILLER PIC X(33) VALUE " MANUAL".
           05 FILLER PIC X(33) VALUE "IMAX".
           05 FILLER PIC X(33) VALUE "IMEAN".
           05 FILLER PIC X(33) VALUE "IMEDIAN".
           05 FILLER PIC X(33) VALUE " MEMORY".
           05 FILLER PIC X(33) VALUE "VMERGE".
           05 FILLER PIC X(33) VALUE " METHOD".
           05 FILLER PIC X(33) VALUE " METHOD-ID".
           05 FILLER PIC X(33) VALUE "IMIDRANGE".
           05 FILLER PIC X(33) VALUE "IMIN".
           05 FILLER PIC X(33) VALUE " MINUS".
           05 FILLER PIC X(33) VALUE "IMOD".
           05 FILLER PIC X(33) VALUE " MODE".
           05 FILLER PIC X(33) VALUE "VMOVE".
           05 FILLER PIC X(33) VALUE " MULTIPLE".
           05 FILLER PIC X(33) VALUE "VMULTIPLY".
           05 FILLER PIC X(33) VALUE " NATIONAL".
           05 FILLER PIC X(33) VALUE " NATIONAL-EDITED".
           05 FILLER PIC X(33) VALUE " NATIVE".
           05 FILLER PIC X(33) VALUE " NEGATIVE".
           05 FILLER PIC X(33) VALUE " NESTED".
           05 FILLER PIC X(33) VALUE "VNEXT".
           05 FILLER PIC X(33) VALUE " NO".
           05 FILLER PIC X(33) VALUE " NOT".
           05 FILLER PIC X(33) VALUE " NULL".
           05 FILLER PIC X(33) VALUE " NULLS".
           05 FILLER PIC X(33) VALUE " NUMBER".
           05 FILLER PIC X(33) VALUE "INUMBER-OF-CALL-PARAMETERS".
           05 FILLER PIC X(33) VALUE " NUMBERS".
           05 FILLER PIC X(33) VALUE " NUMERIC".
           05 FILLER PIC X(33) VALUE " NUMERIC-EDITED".
           05 FILLER PIC X(33) VALUE "INUMVAL".
           05 FILLER PIC X(33) VALUE "INUMVAL-C".
           05 FILLER PIC X(33) VALUE " OBJECT".
           05 FILLER PIC X(33) VALUE " OBJECT-COMPUTER".
           05 FILLER PIC X(33) VALUE " OBJECT-REFERENCE".
           05 FILLER PIC X(33) VALUE " OCCURS".
           05 FILLER PIC X(33) VALUE " OF".
           05 FILLER PIC X(33) VALUE " OFF".
           05 FILLER PIC X(33) VALUE " OMITTED".
           05 FILLER PIC X(33) VALUE " ON".
           05 FILLER PIC X(33) VALUE " ONLY".
           05 FILLER PIC X(33) VALUE "VOPEN".
           05 FILLER PIC X(33) VALUE " OPTIONAL".
           05 FILLER PIC X(33) VALUE " OPTIONS".
           05 FILLER PIC X(33) VALUE " OR".
           05 FILLER PIC X(33) VALUE "IORD".
           05 FILLER PIC X(33) VALUE "IORD-MAX".
           05 FILLER PIC X(33) VALUE "IORD-MIN".
           05 FILLER PIC X(33) VALUE " ORDER".
           05 FILLER PIC X(33) VALUE " ORGANIZATION".
           05 FILLER PIC X(33) VALUE " OTHER".
           05 FILLER PIC X(33) VALUE " OUTPUT".
           05 FILLER PIC X(33) VALUE " OVERFLOW".
           05 FILLER PIC X(33) VALUE " OVERLINE".
           05 FILLER PIC X(33) VALUE " OVERRIDE".
           05 FILLER PIC X(33) VALUE " PACKED-DECIMAL".
           05 FILLER PIC X(33) VALUE " PADDING".
           05 FILLER PIC X(33) VALUE " PAGE".
           05 FILLER PIC X(33) VALUE " PAGE-COUNTER".
           05 FILLER PIC X(33) VALUE " PARAGRAPH".
           05 FILLER PIC X(33) VALUE "VPERFORM".
           05 FILLER PIC X(33) VALUE " PF".
           05 FILLER PIC X(33) VALUE " PH".
           05 FILLER PIC X(33) VALUE "IPI".
           05 FILLER PIC X(33) VALUE "KPIC".
           05 FILLER PIC X(33) VALUE "KPICTURE".
           05 FILLER PIC X(33) VALUE " PLUS".
           05 FILLER PIC X(33) VALUE "KPOINTER".
           05 FILLER PIC X(33) VALUE " POSITION".
           05 FILLER PIC X(33) VALUE " POSITIVE".
           05 FILLER PIC X(33) VALUE " PRESENT".
           05 FILLER PIC X(33) VALUE "IPRESENT-VALUE".
           05 FILLER PIC X(33) VALUE " PREVIOUS".
           05 FILLER PIC X(33) VALUE "MPRINTER".
           05 FILLER PIC X(33) VALUE " PRINTING".
           05 FILLER PIC X(33) VALUE "KPROCEDURE".
           05 FILLER PIC X(33) VALUE " PROCEDURE-POINTER".
           05 FILLER PIC X(33) VALUE " PROCEDURES".
           05 FILLER PIC X(33) VALUE " PROCEED".
           05 FILLER PIC X(33) VALUE " PROGRAM".
           05 FILLER PIC X(33) VALUE "KPROGRAM-ID".
           05 FILLER PIC X(33) VALUE " PROGRAM-POINTER".
           05 FILLER PIC X(33) VALUE " PROMPT".
           05 FILLER PIC X(33) VALUE " PROPERTY".
           05 FILLER PIC X(33) VALUE " PROTOTYPE".
           05 FILLER PIC X(33) VALUE " QUOTE".
           05 FILLER PIC X(33) VALUE " QUOTES".
           05 FILLER PIC X(33) VALUE " RAISE".
           05 FILLER PIC X(33) VALUE " RAISING".
           05 FILLER PIC X(33) VALUE "IRANDOM".
           05 FILLER PIC X(33) VALUE "IRANGE".
           05 FILLER PIC X(33) VALUE " RD".
           05 FILLER PIC X(33) VALUE "VREAD".
           05 FILLER PIC X(33) VALUE "VREADY".
           05 FILLER PIC X(33) VALUE " RECORD".
           05 FILLER PIC X(33) VALUE " RECORDING".
           05 FILLER PIC X(33) VALUE " RECORDS".
           05 FILLER PIC X(33) VALUE " RECURSIVE".
           05 FILLER PIC X(33) VALUE "KREDEFINES".
           05 FILLER PIC X(33) VALUE " REEL".
           05 FILLER PIC X(33) VALUE " REFERENCE".
           05 FILLER PIC X(33) VALUE " RELATIVE".
           05 FILLER PIC X(33) VALUE "VRELEASE".
           05 FILLER PIC X(33) VALUE "IREM".
           05 FILLER PIC X(33) VALUE " REMAINDER".
           05 FILLER PIC X(33) VALUE " REMARKS".
           05 FILLER PIC X(33) VALUE " REMOVAL".
           05 FILLER PIC X(33) VALUE "KRENAMES".
           05 FILLER PIC X(33) VALUE "KREPLACING".
           05 FILLER PIC X(33) VALUE "KREPORT".
           05 FILLER PIC X(33) VALUE " REPORTING".
           05 FILLER PIC X(33) VALUE " REPORTS".
           05 FILLER PIC X(33) VALUE " REPOSITORY".
           05 FILLER PIC X(33) VALUE " REPRESENTS-NOT-A-NUMBER".
           05 FILLER PIC X(33) VALUE " REQUIRED".
           05 FILLER PIC X(33) VALUE " RESERVE".
           05 FILLER PIC X(33) VALUE " RESUME".
           05 FILLER PIC X(33) VALUE " RETRY".
           05 FILLER PIC X(33) VALUE "VRETURN".
           05 FILLER PIC X(33) VALUE "IRETURN-CODE".
           05 FILLER PIC X(33) VALUE "KRETURNING".
           05 FILLER PIC X(33) VALUE "IREVERSE".
           05 FILLER PIC X(33) VALUE " REVERSE-VIDEO".
           05 FILLER PIC X(33) VALUE " REWIND".
           05 FILLER PIC X(33) VALUE "VREWRITE".
           05 FILLER PIC X(33) VALUE " RF".
           05 FILLER PIC X(33) VALUE " RH".
           05 FILLER PIC X(33) VALUE " RIGHT".
           05 FILLER PIC X(33) VALUE " RIGHT-JUSTIFY".
           05 FILLER PIC X(33) VALUE "VROLLBACK".
           05 FILLER PIC X(33) VALUE " ROUNDED".
           05 FILLER PIC X(33) VALUE " RUN".
           05 FILLER PIC X(33) VALUE " SAME".
           05 FILLER PIC X(33) VALUE "KSCREEN".
           05 FILLER PIC X(33) VALUE " SCROLL".
           05 FILLER PIC X(33) VALUE "KSD".
           05 FILLER PIC X(33) VALUE "VSEARCH".
           05 FILLER PIC X(33) VALUE "ISECONDS-FROM-FORMATTED-TIME".
           05 FILLER PIC X(33) VALUE "ISECONDS-PAST-MIDNIGHT".
           05 FILLER PIC X(33) VALUE "KSECTION".
           05 FILLER PIC X(33) VALUE " SECURE".
           05 FILLER PIC X(33) VALUE " SECURITY".
           05 FILLER PIC X(33) VALUE " SEGMENT-LIMIT".
           05 FILLER PIC X(33) VALUE " SELECT".
           05 FILLER PIC X(33) VALUE " SELF".
           05 FILLER PIC X(33) VALUE " SENTENCE".
           05 FILLER PIC X(33) VALUE " SEPARATE".
           05 FILLER PIC X(33) VALUE " SEQUENCE".
           05 FILLER PIC X(33) VALUE " SEQUENTIAL".
           05 FILLER PIC X(33) VALUE "VSET".
           05 FILLER PIC X(33) VALUE " SHARING".
           05 FILLER PIC X(33) VALUE "ISIGN".
           05 FILLER PIC X(33) VALUE " SIGNED".
           05 FILLER PIC X(33) VALUE " SIGNED-INT".
           05 FILLER PIC X(33) VALUE " SIGNED-LONG".
           05 FILLER PIC X(33) VALUE " SIGNED-SHORT".
           05 FILLER PIC X(33) VALUE "ISIN".
           05 FILLER PIC X(33) VALUE " SIZE".
           05 FILLER PIC X(33) VALUE "VSORT".
           05 FILLER PIC X(33) VALUE " SORT-MERGE".
           05 FILLER PIC X(33) VALUE "ISORT-RETURN".
           05 FILLER PIC X(33) VALUE " SOURCE".
           05 FILLER PIC X(33) VALUE " SOURCE-COMPUTER".
           05 FILLER PIC X(33) VALUE " SOURCES".
           05 FILLER PIC X(33) VALUE " SPACE".
           05 FILLER PIC X(33) VALUE " SPACE-FILL".
           05 FILLER PIC X(33) VALUE " SPACES".
           05 FILLER PIC X(33) VALUE " SPECIAL-NAMES".
           05 FILLER PIC X(33) VALUE "ISQRT".
           05 FILLER PIC X(33) VALUE " STANDARD".
           05 FILLER PIC X(33) VALUE " STANDARD-1".
           05 FILLER PIC X(33) VALUE " STANDARD-2".
           05 FILLER PIC X(33) VALUE "ISTANDARD-DEVIATION".
           05 FILLER PIC X(33) VALUE "VSTART".
           05 FILLER PIC X(33) VALUE " STATUS".
           05 FILLER PIC X(33) VALUE "VSTOP".
           05 FILLER PIC X(33) VALUE "ISTORED-CHAR-LENGTH".
           05 FILLER PIC X(33) VALUE "VSTRING".
           05 FILLER PIC X(33) VALUE "ISUBSTITUTE".
           05 FILLER PIC X(33) VALUE "ISUBSTITUTE-CASE".
           05 FILLER PIC X(33) VALUE "VSUBTRACT".
           05 FILLER PIC X(33) VALUE "ISUM".
           05 FILLER PIC X(33) VALUE " SUPER".
           05 FILLER PIC X(33) VALUE "VSUPPRESS".
           05 FILLER PIC X(33) VALUE "MSWITCH-1".
           05 FILLER PIC X(33) VALUE "MSWITCH-2".
           05 FILLER PIC X(33) VALUE "MSWITCH-3".
           05 FILLER PIC X(33) VALUE "MSWITCH-4".
           05 FILLER PIC X(33) VALUE "MSWITCH-5".
           05 FILLER PIC X(33) VALUE "MSWITCH-6".
           05 FILLER PIC X(33) VALUE "MSWITCH-7".
           05 FILLER PIC X(33) VALUE "MSWITCH-8".
           05 FILLER PIC X(33) VALUE " SYMBOLIC".
           05 FILLER PIC X(33) VALUE " SYNC".
           05 FILLER PIC X(33) VALUE " SYNCHRONIZED".
           05 FILLER PIC X(33) VALUE "MSYSERR".
           05 FILLER PIC X(33) VALUE "MSYSIN".
           05 FILLER PIC X(33) VALUE "MSYSIPT".
           05 FILLER PIC X(33) VALUE "MSYSLIST".
           05 FILLER PIC X(33) VALUE "MSYSLST".
           05 FILLER PIC X(33) VALUE "MSYSOUT".
           05 FILLER PIC X(33) VALUE " SYSTEM-DEFAULT".
           05 FILLER PIC X(33) VALUE " TABLE".
           05 FILLER PIC X(33) VALUE "KTALLYING".
           05 FILLER PIC X(33) VALUE "ITAN".
           05 FILLER PIC X(33) VALUE " TAPE".
           05 FILLER PIC X(33) VALUE "VTERMINATE".
           05 FILLER PIC X(33) VALUE " TEST".
           05 FILLER PIC X(33) VALUE "ITEST-DATE-YYYYMMDD".
           05 FILLER PIC X(33) VALUE "ITEST-DAY-YYYYDDD".
           05 FILLER PIC X(33) VALUE " THAN".
           05 FILLER PIC X(33) VALUE " THEN".
           05 FILLER PIC X(33) VALUE " THROUGH".
           05 FILLER PIC X(33) VALUE " THRU".
           05 FILLER PIC X(33) VALUE " TIME".
           05 FILLER PIC X(33) VALUE " TIMES".
           05 FILLER PIC X(33) VALUE "KTO".
           05 FILLER PIC X(33) VALUE " TOP".
           05 FILLER PIC X(33) VALUE " TRAILING".
           05 FILLER PIC X(33) VALUE " TRAILING-SIGN".
           05 FILLER PIC X(33) VALUE "VTRANSFORM".
           05 FILLER PIC X(33) VALUE "ITRIM".
           05 FILLER PIC X(33) VALUE " TRUE".
           05 FILLER PIC X(33) VALUE " TYPE".
           05 FILLER PIC X(33) VALUE " TYPEDEF".
           05 FILLER PIC X(33) VALUE " UNDERLINE".
           05 FILLER PIC X(33) VALUE " UNIT".
           05 FILLER PIC X(33) VALUE " UNIVERSAL".
           05 FILLER PIC X(33) VALUE "VUNLOCK".
           05 FILLER PIC X(33) VALUE " UNSIGNED".
           05 FILLER PIC X(33) VALUE " UNSIGNED-INT".
           05 FILLER PIC X(33) VALUE " UNSIGNED-LONG".
           05 FILLER PIC X(33) VALUE " UNSIGNED-SHORT".
           05 FILLER PIC X(33) VALUE "VUNSTRING".
           05 FILLER PIC X(33) VALUE " UNTIL".
           05 FILLER PIC X(33) VALUE "KUP".
           05 FILLER PIC X(33) VALUE " UPDATE".
           05 FILLER PIC X(33) VALUE " UPON".
           05 FILLER PIC X(33) VALUE " UPPER".
           05 FILLER PIC X(33) VALUE "IUPPER-CASE".
           05 FILLER PIC X(33) VALUE " USAGE".
           05 FILLER PIC X(33) VALUE "VUSE".
           05 FILLER PIC X(33) VALUE " USER-DEFAULT".
           05 FILLER PIC X(33) VALUE "KUSING".
           05 FILLER PIC X(33) VALUE " VAL-STATUS".
           05 FILLER PIC X(33) VALUE " VALID".
           05 FILLER PIC X(33) VALUE " VALIDATE".
           05 FILLER PIC X(33) VALUE " VALIDATE-STATUS".
           05 FILLER PIC X(33) VALUE " VALUE".
           05 FILLER PIC X(33) VALUE " VALUES".
           05 FILLER PIC X(33) VALUE "IVARIANCE".
           05 FILLER PIC X(33) VALUE "KVARYING".
           05 FILLER PIC X(33) VALUE " WAIT".
           05 FILLER PIC X(33) VALUE "VWHEN".
           05 FILLER PIC X(33) VALUE "IWHEN-COMPILED".
           05 FILLER PIC X(33) VALUE " WITH".
           05 FILLER PIC X(33) VALUE " WORDS".
           05 FILLER PIC X(33) VALUE "KWORKING-STORAGE".
           05 FILLER PIC X(33) VALUE "VWRITE".
           05 FILLER PIC X(33) VALUE "IYEAR-TO-YYYY".
           05 FILLER PIC X(33) VALUE " YYYYDDD".
           05 FILLER PIC X(33) VALUE " YYYYMMDD".
           05 FILLER PIC X(33) VALUE " ZERO".
           05 FILLER PIC X(33) VALUE " ZERO-FILL".
           05 FILLER PIC X(33) VALUE " ZEROES".
           05 FILLER PIC X(33) VALUE " ZEROS".
       01  Reserved-Word-Table         REDEFINES Reserved-Words.
           05 Reserved-Word            OCCURS 591 TIMES
                                       ASCENDING KEY RW-Word
                                       INDEXED RW-Idx.
              10 RW-Type               PIC X(1).
              10 RW-Word               PIC X(32).

       01  Saved-Section               PIC X(15).

       01  Search-Token                PIC X(32).

       01  Source-Line-No              PIC 9(6).

       01  Src-Ptr                     USAGE BINARY-LONG.

       01  Syntax-Parsing-Items.
           05 SPI-Current-Char         PIC X(1).
              88 Current-Char-Is-Punct VALUE "=", "(", ")", "*", "/",
                                             "&", ";", ",", "<", ">",
                                             ":".
              88 Current-Char-Is-Quote VALUE '"', "'".
              88 Current-Char-Is-X     VALUE "x", "X".
              88 Current-Char-Is-Z     VALUE "z", "Z".
           05 SPI-Current-Division     PIC X(1).
              88 In-IDENTIFICATION-DIVISION VALUE "I", "?".
              88 In-ENVIRONMENT-DIVISION    VALUE "E".
              88 In-DATA-DIVISION           VALUE "D".
              88 In-PROCEDURE-DIVISION      VALUE "P".
           05 SPI-Current-Line-No      PIC 9(6).
           05 SPI-Current-Program-ID.
              10 FILLER                PIC X(12).
              10 SPI-CP-13-15          PIC X(3).
           05 SPI-Current-Section.
              10 SPI-CS-1              PIC X(1).
              10 SPI-CS-2-14.
                 15 FILLER             PIC X(10).
                 15 SPI-CS-11-14       PIC X(3).
              10 SPI-CS-15             PIC X(1).
           05 SPI-Current-Token        PIC X(32).
           05 SPI-Current-Token-UC     PIC X(32).
           05 SPI-Current-Verb         PIC X(12).
           05 SPI-Next-Char            PIC X(1).
              88 Next-Char-Is-Quote    VALUE '"', "'".
           05 SPI-Prior-Token          PIC X(32).
           05 SPI-Token-Type           PIC X(1).
              88 Token-Is-EOF             VALUE HIGH-VALUES.
              88 Token-Is-Identifier      VALUE "I".
              88 Token-Is-Key-Word        VALUE "K", "V".
              88 Token-Is-Literal-Alpha   VALUE "L".
              88 Token-Is-Literal-Number  VALUE "N".
              88 Token-Is-Verb            VALUE "V".
GC0710        88 Token-Is-Reserved-Word   VALUE " ".

       01  Tally                       USAGE BINARY-LONG.

       01  Todays-Date                 PIC 9(8).

       LINKAGE SECTION.
       01  Produce-Source-Listing      PIC X(1).
       01  Produce-Xref-Listing        PIC X(1).
       01  Src-Filename                PIC X(256).
      /
       PROCEDURE DIVISION USING Produce-Source-Listing
                                Produce-Xref-Listing
                                Src-Filename.
       000-Main SECTION.
       001-Init.
           PERFORM 100-Initialization
           PERFORM 200-Execute-cobc
           OPEN OUTPUT Report-File
           IF Produce-Source-Listing NOT = SPACE
               PERFORM 500-Produce-Source-Listing
           END-IF
           IF Produce-Xref-Listing NOT = SPACE
               SORT Sort-File
                   ASCENDING KEY    SR-Prog-ID
                                    SR-Token-UC
                                    SR-Line-No-Ref
                   INPUT PROCEDURE  300-Tokenize-Source
                   OUTPUT PROCEDURE 400-Produce-Xref-Listing
           END-IF
           CLOSE Report-File
           GOBACK
           .
      /
       100-Initialization SECTION.
      *****************************************************************
      ** Perform all program-wide initialization operations          **
      *****************************************************************
       END PROGRAM LISTING.
