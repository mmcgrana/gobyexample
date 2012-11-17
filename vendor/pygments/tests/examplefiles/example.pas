// vim:ft=pascal

unit YTools;

{===============================================================================

   cYcnus.YTools 1.0.3 Beta for Delphi 4+
   by licenser and Murphy

   ©2000-2003 by cYcnus
   visit www.cYcnus.de

   licenser@cYcnus.de (Heinz N. Gies)
   murphy@cYcnus.de (Kornelius Kalnbach)
   
   this unit is published under the terms of the GPL

===============================================================================}

interface

uses
  Windows, SysUtils, Classes, YTypes;

const
  BackSpace = #8;
  Tab = #9;
  LF = #10; //Line Feed
  CR = #13; //Carriage Return
  Space = #32;
  EOLChars = [CR, LF];
{$IFNDEF VER140}
  sLineBreak = #13#10;
  SwitchChars = ['/', '-'];
{$ENDIF}
  EOL = sLineBreak;
  MaxCard = High(Cardinal);
  AllChars = [#0..#255];
  Alphabetical = ['A'..'Z', 'a'..'z'];
  DecimalChars = ['0'..'9'];
  AlphaNumerical = Alphabetical + DecimalChars;
  StrangeChars = [#0..#31, #127, #129, #141..#144, #157, #158];

  HexadecimalChars = DecimalChars + ['A'..'F', 'a'..'f'];
  OctalChars = ['0'..'7'];
  BinaryChars = ['0', '1'];

  QuoteChars = ['''', '"'];
  WildCards = ['*', '?'];
  FileNameEnemies = WildCards + ['\', '/', ':', '<', '>', '|'];

  HexChar: array[THex] of Char = (
    '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
  LowerHexChar: array[THex] of Char = (
    '0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f');
  BaseNChar: array[TBaseN] of Char = (
    '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H',
    'I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z');

  cYcnusOverlayColor = $050001;

  faFindEveryFile = faReadOnly + faHidden + faSysFile + faArchive;

  platWin9x = [VER_PLATFORM_WIN32s, VER_PLATFORM_WIN32_WINDOWS];


{ Debugging }
procedure ClearReport(const ReportName: string);
procedure Report(const ReportName, Text: string);
procedure ReportFmt(const ReportName, Fmt: string; const Args: array of const);

{ Params }
procedure GetParams(Strings: TStrings); overload;
function GetParams(const Separator: string = ' '): string; overload;

function ParamNum(const S: string): Integer;
function ParamPrefixNum(const Prefix: string): Integer;
function Param(const S: string): Boolean;
function ParamPrefix(const Prefix: string): Boolean;

function Switch(const Switch: string; const PrefixChars: TCharSet = SwitchChars;
  IgnoreCase: Boolean = True): Boolean;
function GetParam(const Prefix: string = ''; const Default: string = ''): string;

{ Dirs & UserName}
function GetMyDir(FullPath: Boolean = False): string;
function WinDir: string;
function SysDir: string;
function UserName: string;

{ Strings & Chars}
function FirstChar(const S: string): Char;
function LastChar(const S: string): Char;

function CharPos(C: Char; const S: string; Offset: Integer = 1): Integer; overload;
function CharPos(C: TCharSet; const S: string; Offset: Integer = 1): Integer; overload;
function CharPosR(C: Char; const S: string; Offset: Integer = -1): Integer;
function PosEx(const SubStr, S: string; Offset: Integer = 1): Integer;
function PosExText(const SubStr, S: string; Offset: Integer = 1): Integer;
function PosExAnsiText(const SubStr, S: string; Offset: Integer = 1): Integer;

function UntilChar(const S: string; Brake: Char): string; overload;
function UntilChar(const S: string; Brake: TCharSet): string; overload;
function UntilLastChar(const S: string; Brake: Char;
  IgnoreNoBrake: Boolean = True): string;

function FromChar(const S: string; Brake: Char): string; overload;
function FromChar(const S: string; Brake: TCharSet): string; overload;
function FromLastChar(const S: string; Brake: Char;
  IgnoreNoBrake: Boolean = False): string;

function BetweenChars(const S: string; Start, Finish: Char;
  Inclusive: Boolean = False): string;

function UntilStr(const S: string; Brake: string): string;
function FromStr(const S: string; Brake: string): string;

function StringWrap(const S: string; Width: Integer; const LineEnd: string = EOL): string;

{ Splitting & Combining }
function Split(const S, Separator: string; IgnoreMultiSep: Boolean = True;
  MinCount: Integer = 0): TStrA; overload;
procedure Split(const S, Separator: string; Strings: TStrings;
  IgnoreMultiSep: Boolean = True); overload;
function Split(const S: string; Separators: TCharSet;
  IgnoreMultiSep: Boolean = True; MinCount: Integer = 0): TStrA; overload;

procedure TileStr(const S: string; BrakeStart: Integer; BrakeEnd: Integer;
  out Left, Right: string);

function Join(Strings: TStrings; Separator: string = ' '): string; overload;
function Join(StrA: TStrA; Separator: string = ' '): string; overload;

function MulStr(const S: string; Count: Integer): string;

{ Strings ausrichten }
function AlignR(const S: string; Width: Integer; Filler: Char = ' '): string;
function MaxStr(const S: string; MaxLen: Integer): string;

{ Stringing }
function TrimAll(const S: string): string;

function ControlChar(C: Char): Boolean;
function FriendlyChar(C: Char): Char;

function FriendlyStr(const S: string): string; overload;
function FriendlyStr(a: TByteA): string; overload;

function Quote(const S: string; Quoter: Char = '"'): string;
function UnQuote(const S: string): string;
function DeQuote(const S: string): string;

function StrNumerus(const Value: Integer; const Singular, Plural: string;
  const Zero: string = '0'): string;

function MakeStr(const Items: array of const; Separator: string = ''): string;
procedure ShowText(const Items: array of const; Separator: string = '');

{ Delete }
function DeleteChars(const S: string; C: Char): string; overload;
function DeleteChars(const S: string; C: TCharSet): string; overload;
function ExtractChars(const S: string; C: TCharSet): string;

{ Find }
function CharCount(const S: string; C: Char): Integer;

function CharIn(const S: string; C: Char): Boolean; overload;
function CharIn(const S: string; C: TCharSet): Boolean; overload;

function StrAtPos(const S: string; Pos: Integer; const Str: string): Boolean;
function StrAtBegin(const S, Str: string): Boolean;
function StrIn(const S, SubStr: string): Boolean; overload;
function StrIn(A: TStrA; const S: string): Boolean; overload;
function StrIn(SL: TStrings; const S: string): Boolean; overload;
function StrIndex(A: TStrA; const S: string): Integer; overload;
function StrIndex(SL: TStrings; const S: string): Integer; overload;

function TextAtPos(const S: string; Pos: Integer; const Text: string): Boolean;
function TextAtBegin(const S, Text: string): Boolean;
function TextIn(const S, Text: string): Boolean; overload;
function TextIn(A: TStrA; const Text: string): Boolean; overload;
function TextIn(SL: TStrings; const Text: string): Boolean; overload;
function TextIndex(A: TStrA; const Text: string): Integer; overload;
function TextIndex(SL: TStrings; const Text: string): Integer; overload;

{ Replace }
function ReplaceChars(const S: string; Old, New: Char): string; overload;
function ReplaceChars(const S: string; Old: TCharSet; New: Char): string; overload;

function Replace(const S, Old, New: string): string;

{ TStrings }
function SLOfFile(const FileName: string): TStringList;
function ContainsEmptyLines(SL: TStrings): Boolean;
procedure DeleteEmptyLines(SL: TStrings);
procedure DeleteCommentLines(SL: TStrings; const CommentSign: string = '//');
procedure WriteSL(Strings: TStrings; const Prefix: string = '';
  const Suffix: string = '');

function FindLine(SL: TStrings; const S: string): Integer;

procedure QuickSortSL(SL: TStringList);

{ TStrA }
function IncStrA(StrA: TStrA): Integer;

{ TByteA }
function StrOfByteA(a: TByteA): string;
function ByteAOfStr(const S: string): TByteA;
function ByteAOfInt(i: Integer): TByteA;
function IntOfByteA(A: TByteA): Integer;
function ByteAOfHex(const Hex: string): TByteA;

function SameByteA(const A, B: TByteA): Boolean;
function Reverse(a: TByteA): TByteA;
function SaveByteA(Data: TByteA; const FileName: string; Overwrite: Boolean = True): Boolean;
function LoadByteA(const FileName: string): TByteA;

function Endian(i: Integer): Integer;

{ Files }
function SizeOfFile(const FileName: string): Integer;
function FileEx(const FileName: string; AllowFolders: Boolean = False): Boolean;
function LWPSolve(const Dir: string): string;
function LWPSlash(const Dir: string): string;

function ExtractDrive(const FileName: string): string;
function ExtractPath(const FileName: string): string;
function ExtractPrefix(const FileName: string): string;
function ExtractSuffix(const FileName: string): string;

function IsValidFileName(const FileName: string): Boolean;
function MakeValidFileName(FileName: string; const Default: string = 'File'): string;

{ Converting }
function IsValidInteger(const S: string): Boolean;
function IsValidCardinal(const S: string): Boolean;

function StrOfBool(flag: Boolean; const TrueStr: string = 'True';
  const FalseStr: string = 'False'): string;
function StrOfInt(i: Integer): string;
function CardOfStr(const S: string): Cardinal;

function HexOrd(Hex: Char): THex;
function ByteOfHex(Hex: THexByteStr): Byte;

function DecOfHex(const Hex: string): string;
function HexOfByte(b: Byte): THexByteStr;
function HexOfCard(i: Cardinal): string; overload;
function HexOfCard(i: Cardinal; Digits: Integer): string; overload;

function PascalHexArray(a: TByteA; Name: string): string;

function HexOfByteA(a: TByteA; Blocks: Integer = 1;
  const Splitter: string = ' '): string;
function BinOfByteA(a: TByteA; Blocks: Integer = 4;
  const Splitter: string = ' '): string;

function CardOfHex(Hex: string): Cardinal;
function IntOfBin(Bin: string): Cardinal;

function BinOfIntFill(n: cardinal; MinCount: Integer = 8): string;
function BinOfInt(n: cardinal): string;

function BaseNOfInt(I: Cardinal; B: TBaseN): string;
function IntOfBaseN(V: string; B: TBaseN): Cardinal;

{ Ranges }
function KeepIn(i, Bottom, Top: Variant): Variant;
function InRange(Value, Bottom, Top: Variant): Boolean;
function InStrictRange(Value, Bottom, Top: Variant): Boolean;
function Min(const A, B: Integer): Integer; overload;
function Min(const A: TIntA): Integer; overload;
function Max(const A, B: Integer): Integer; overload;
function Max(const A: TIntA): Integer; overload;

const
  RangesSeparator = ',';
  RangeInnerSeparator = '-';
  RangeInfinite = '*';
  RangeSpecialChars = [RangesSeparator, RangeInnerSeparator, RangeInfinite];

function RangesOfStr(const S: string): TRanges;
function InRanges(Ranges: TRanges; TestValue: Cardinal): Boolean;

function Success(Res: Integer; ResultOnSuccess: Integer = ERROR_SUCCESS): Boolean;
function Failure(Res: Integer; ResultOnSuccess: Integer = ERROR_SUCCESS): Boolean;

function ExpandString(const S: string): string;

{ Files }
procedure DeleteFiles(const Mask: string; ScanSubDirs: Boolean = True;
  Attributes: Integer = faFindEveryFile);
procedure FileNew(const FileName: string);
function DateTimeOfFileTime(const FileTime: TFileTime): TDateTime;

{ FileNames }
function GetFileNew(FileName: string; NoFloppyDrives: Boolean = True): string;

{ Finding Files }
function FindAll(Strings: TStrings; const Mask: string;
  ScanSubDirs: Boolean = True; Attributes: Integer = faFindEveryFile;
  FileReturn: TFileNameFunc = nil): Boolean;
function FindAllFirst(const Mask: string; ScanSubDirs: Boolean = True;
  Attributes: Integer = faFindEveryFile): string;

function FullOSInfo: string;
function Win32PlatformStr: string;
function Win9x: Boolean;
function WinNT: Boolean;
function Win2000: Boolean;
function WinXP: Boolean;

var
  MyDir: string = '';
  LastSuccessRes: Integer = 0;
  
{ Backward compatibility }
{$IFNDEF VER130}
function SameText(const S1, S2: string): Boolean;
{$ENDIF}

implementation
{$IFNDEF VER140}
uses FileCtrl;
{$ENDIF}

{$IFNDEF VER130}
function SameText(const S1, S2: string): Boolean;
begin
  Result := CompareText(S1, S2) = 0;
end;
{$ENDIF}

procedure Report(const ReportName, Text: string);
var
  F: TextFile;
  FileName: string;
begin
  FileName := MyDir + ReportName + '.rep';
  Assign(F, FileName);
  try
    if not FileExists(FileName) then
      Rewrite(F)
    else
      Append(F);
    WriteLn(F, Text);
  finally
    Close(F);
  end;
end;

procedure ClearReport(const ReportName: string);
var
  FileName: string;
begin
  FileName := MyDir + ReportName + '.rep';
  DeleteFile(FileName);
end;

procedure ReportFmt(const ReportName, Fmt: string; const Args: array of const);
begin
  Report(ReportName, Format(Fmt, Args));
end;

procedure GetParams(Strings: TStrings);
var
  P: PChar;
  Param: string;

  function GetParamStr(var P: PChar; var Param: string): Boolean;
  var
    Quoted: Boolean;
  begin
    Param := '';

    repeat
      while (P[0] <> #0) and (P[0] <= ' ') do
        Inc(P);

      Quoted := False;
      while P[0] <> #0 do begin
        if P[0] = '"' then begin
          Quoted := not Quoted;
          Inc(P);
        Continue; end;
        if (P[0] <= ' ') and not Quoted then
          Break;
        Param := Param + P[0];
        Inc(P);
      end;
    until (Param <> '') or (P[0] = #0);

    Result := Param <> '';
  end;

begin
  Strings.Clear;
  P := GetCommandLine;
  GetParamStr(P, Param);
  while GetParamStr(P, Param) do
    Strings.Add(Param);
end;

function GetParams(const Separator: string = ' '): string;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  GetParams(SL);
  Result := Join(SL, Separator);
  SL.Free;
end;

function Switch(const Switch: string; const PrefixChars: TCharSet = SwitchChars;
  IgnoreCase: Boolean = True): Boolean;
//= SysUtils.FindCmdLineSwitch
var
  i: Integer;
  s: string;
begin
  Result := True;

  for i := 1 to ParamCount do begin
    s := ParamStr(i);

    if (s <> '') and (s[1] in PrefixChars) then begin
    //i know that always s <> '', but this is saver
      s := Copy(s, 2, MaxInt);
      if (s = Switch) or (IgnoreCase and (0=AnsiCompareText(s, Switch))) then
        Exit;
    end;
  end;

  Result := False;
end;

function ParamNum(const S: string): Integer;
begin
  for Result := 1 to ParamCount do
    if 0=AnsiCompareText(ParamStr(Result), S) then
      Exit;

  Result := 0;
end;

function ParamPrefixNum(const Prefix: string): Integer;
var
  Len: Integer;
begin
  Len := Length(Prefix);
  for Result := 1 to ParamCount do
    if 0=AnsiCompareText(Copy(ParamStr(Result), 1, Len), Prefix) then
      Exit;

  Result := 0;
end;

function Param(const S: string): Boolean;
begin
  Result := ParamNum(S) > 0;
end;

function ParamPrefix(const Prefix: string): Boolean;
begin
  Result := ParamPrefixNum(Prefix) > 0;
end;

function GetParam(const Prefix: string = ''; const Default: string = ''): string;
var
  i: Integer;
begin
  Result := Default;

  if Prefix = '' then begin
    Result := ParamStr(1);
  Exit; end;

  i := ParamPrefixNum(Prefix);
  if i > 0 then
    Result := Copy(ParamStr(i), Length(Prefix) + 1, MaxInt);
end;

function GetMyDir(FullPath: Boolean = False): string;
var
  Buffer: array[0..260] of Char;
begin
  Result := '';
  SetString(Result, Buffer, GetModuleFileName(0, Buffer, SizeOf(Buffer)));
  if FullPath then
    Result := GetFileNew(Result);
  Result := ExtractPath(Result);
end;

function WinDir: string;
var
  Res: PChar;
begin
  Result := '\';
  GetMem(Res, MAX_PATH);
  GetWindowsDirectory(Res, MAX_PATH);
  Result := Res + '\';
  FreeMem(Res, MAX_PATH);
end;

function SysDir: string;
var
  Res: PChar;
begin
  Result := '\';
  GetMem(Res, MAX_PATH);
  GetSystemDirectory(Res, MAX_PATH);
  Result := Res + '\';
  FreeMem(Res, MAX_PATH);
end;

function UserName: string;
var
  Len: Cardinal;
  Res: PChar;
begin
  Result := '';
  GetMem(Res, MAX_PATH);
  Len := MAX_PATH;
  GetUserName(Res, Len);
  Result := Res;
  FreeMem(Res, MAX_PATH);
end;

function FirstChar(const S: string): Char;
begin
  if s = '' then
    Result := #0
  else
    Result := s[1];
end;

function LastChar(const S: string): Char;
begin
  if s = '' then
    Result := #0
  else
    Result := s[Length(s)];
end;

function CharPos(C: Char; const S: string; Offset: Integer = 1): Integer;
var
  MaxPosToSearch: Integer;
begin
  Result := Offset;
  MaxPosToSearch := Length(S);

  while Result <= MaxPosToSearch do begin
    if S[Result] = C then
      Exit;
    Inc(Result);
  end;

  Result := 0;
end;

function CharPos(C: TCharSet; const S: string; Offset: Integer = 1): Integer;
var
  MaxPosToSearch: Integer;
begin
  Result := Offset;
  MaxPosToSearch := Length(S);

  while Result <= MaxPosToSearch do begin
    if S[Result] in C then
      Exit;
    Inc(Result);
  end;

  Result := 0;
end;

function CharPosR(C: Char; const S: string; Offset: Integer = -1): Integer;
begin
  if Offset < 0 then
    Result := Length(S) + 1 - Offset
  else
    Result := Offset;
  if Result > Length(S) then
    Result := Length(S);

  while Result > 0 do begin
    if S[Result] = C then
      Exit;
    Dec(Result);
  end;
end;

function PosEx(const SubStr, S: string; Offset: Integer = 1): Integer;
var
  MaxPosToSearch, LenSubStr, i: Integer;
begin
  if SubStr = '' then begin
    Result := 0;
  Exit; end;

  if Offset < 1 then
    Result := 1
  else
    Result := Offset;

  LenSubStr := Length(SubStr);
  MaxPosToSearch := Length(S) - LenSubStr + 1;

  while Result <= MaxPosToSearch do begin
    if S[Result] = SubStr[1] then begin
      i := 1;

      while (i < LenSubStr)
       and (S[Result + i] = SubStr[i + 1]) do
        Inc(i);

      if i = LenSubStr then
        Exit;
    end;
    Inc(Result);
  end;

  Result := 0;
end;

function PosExText(const SubStr, S: string; Offset: Integer = 1): Integer;
var
  MaxPosToSearch, LenSubStr, i: Integer;

  function SameChar(a, b: Char): Boolean;
  begin
    Result := UpCase(a) = UpCase(b)
  end;

begin
  if SubStr = '' then begin
    Result := 0;
  Exit; end;

  if Offset < 1 then
    Result := 1
  else
    Result := Offset;

  LenSubStr := Length(SubStr);
  MaxPosToSearch := Length(S) - LenSubStr + 1;

  while Result <= MaxPosToSearch do begin
    if SameChar(S[Result], SubStr[1]) then begin
      i := 1;

      while (i < LenSubStr)
       and (SameChar(S[Result + i], SubStr[i + 1])) do
        Inc(i);

      if i = LenSubStr then
        Exit;
    end;
    Inc(Result);
  end;

  Result := 0;
end;

function PosExAnsiText(const SubStr, S: string; Offset: Integer = 1): Integer;
var
  MaxPosToSearch, LenSubStr, i: Integer;

  function SameChar(a, b: Char): Boolean;
  begin
    Result := CharLower(PChar(a)) = CharLower(PChar(b));
  end;

begin
  if SubStr = '' then begin
    Result := 0;
  Exit; end;

  if Offset < 1 then
    Result := 1
  else
    Result := Offset;

  LenSubStr := Length(SubStr);
  MaxPosToSearch := Length(S) - LenSubStr + 1;

  while Result <= MaxPosToSearch do begin
    if SameChar(S[Result], SubStr[1]) then begin
      i := 1;

      while (i < LenSubStr)
       and (SameChar(S[Result + i], SubStr[i + 1])) do
        Inc(i);

      if i = LenSubStr then
        Exit;
    end;
    Inc(Result);
  end;

  Result := 0;
end;

function UntilChar(const S: string; Brake: Char): string;
var
  p: Integer;
begin
  p := CharPos(Brake, S);

  if p > 0 then
    Result := Copy(S, 1, p - 1)
  else
    Result := S;
end;

function UntilChar(const S: string; Brake: TCharSet): string;
var
  p: Integer;
begin
  Result := '';
  p := CharPos(Brake, S);

  if p > 0 then
    Result := Copy(S, 1, p - 1)
  else
    Result := S;
end;

function UntilLastChar(const S: string; Brake: Char;
  IgnoreNoBrake: Boolean = True): string;
var
  p: Integer;
begin
  Result := '';
  p := CharPosR(Brake, S);

  if p > 0 then
    Result := Copy(S, 1, p - 1)
  else if IgnoreNoBrake then
    Result := S;
end;

function FromChar(const S: string; Brake: Char): string;
var
  p: Integer;
begin
  Result := '';
  p := CharPos(Brake, S);

  if p > 0 then
    Result := Copy(S, p + 1, Length(S) - p);
end;

function FromChar(const S: string; Brake: TCharSet): string;
var
  p: Integer;
begin
  Result := '';
  p := CharPos(Brake, S);

  if p > 0 then
    Result := Copy(S, p + 1, Length(S) - p);
end;

function FromLastChar(const S: string; Brake: Char;
  IgnoreNoBrake: Boolean = False): string;
var
  p: Integer;
begin
  Result := '';
  p := CharPosR(Brake, S);

  if p > 0 then
    Result := Copy(S, p + 1, Length(S) - p)
  else if IgnoreNoBrake then
    Result := S;
end;

function BetweenChars(const S: string; Start, Finish: Char;
  Inclusive: Boolean = False): string;
var
  p, fin: Integer;
begin
  Result := '';

  p := CharPos(Start, S);
  if p = 0 then
    Exit;

  fin := CharPos(Finish, S, p + 1);
  if fin = 0 then
    Exit;

  if not Inclusive then begin
    Inc(p);
    Dec(fin);
  end;

  Result := Copy(S, p, fin - p + 1);
end;

function UntilStr(const S: string; Brake: string): string;
var
  p: Integer;
begin
  if Length(Brake) = 1 then begin
    Result := UntilChar(S, Brake[1]);
  Exit; end;

  p := PosEx(Brake, S);

  if p > 0 then
    Result := Copy(S, 1, p - 1)
  else
    Result := S;
end;

function FromStr(const S: string; Brake: string): string;
var
  p: Integer;
begin
  if Length(Brake) = 1 then begin
    Result := FromChar(S, Brake[1]);
  Exit; end;

  Result := '';
  p := PosEx(Brake, s);

  if p > 0 then begin
    Inc(p, Length(Brake));
    Result := Copy(S, p, Length(S) - p + 1);
  end;
end;

function StringWrap(const S: string; Width: Integer; const LineEnd: string = EOL): string;
var
  i: Integer;
begin
  Result := '';
  if (S = '') or (Width < 1) then
    Exit;

  i := 1;
  while True do begin
    Result := Result + Copy(S, i, Width);
    Inc(i, Width);
    if i <= Length(S) then
      Result := Result + LineEnd
    else
      Exit;
  end;
end;

function Split(const S, Separator: string; IgnoreMultiSep: Boolean = True;
  MinCount: Integer = 0): TStrA;
var
  p, fin, SepLen: Integer;

  procedure Add(const S: string);
  begin
    if IgnoreMultiSep and (S = '') then
      Exit;
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := S;
  end;

begin
  if S = '' then begin
    if Length(Result) < MinCount then
      SetLength(Result, MinCount);
  Exit; end;

  Result := nil;
  SepLen := Length(Separator);

  p := 1;
  fin := PosEx(Separator, S);
  while fin > 0 do begin
    Add(Copy(S, p, fin - p));
    p := fin + SepLen;
    fin := PosEx(Separator, S, p);
  end;
  Add(Copy(S, p, Length(S) - p + 1));

  if Length(Result) < MinCount then
    SetLength(Result, MinCount);
end;

procedure Split(const S, Separator: string; Strings: TStrings;
  IgnoreMultiSep: Boolean = True); 
var
  p, fin, SepLen: Integer;

  procedure Add(const S: string);
  begin
    if IgnoreMultiSep and (S = '') then
      Exit;
    Strings.Add(S);
  end;

begin
  if S = '' then
    Exit;

  Strings.BeginUpdate;
  SepLen := Length(Separator);
  p := 1;
  fin := PosEx(Separator, S);
  while fin > 0 do begin
    Add(Copy(S, p, fin - p));
    p := fin + SepLen;
    fin := PosEx(Separator, S, p);
  end;
  Add(Copy(S, p, Length(S) - p + 1));
  Strings.EndUpdate;
end;

function Split(const S: string; Separators: TCharSet;
  IgnoreMultiSep: Boolean = True; MinCount: Integer = 0): TStrA;
var
  p, fin: Integer;

  procedure Add(const S: string);
  begin
    if IgnoreMultiSep and (S = '') then
      Exit;
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := S;
  end;

begin
  if S = '' then begin
    if Length(Result) < MinCount then
      SetLength(Result, MinCount);
  Exit; end;

  Result := nil;

  p := 1;
  fin := CharPos(Separators, S);
  while fin > 0 do begin
    Add(Copy(S, p, fin - p));
    p := fin + 1;
    fin := CharPos(Separators, S, p);
  end;
  Add(Copy(S, p, Length(S) - p + 1));

  if Length(Result) < MinCount then
    SetLength(Result, MinCount);
end;

procedure TileStr(const S: string; BrakeStart: Integer; BrakeEnd: Integer;
  out Left, Right: string);
begin
  Left := Copy(S, 1, BrakeStart-1);
  Right := Copy(S, BrakeEnd + 1, MaxInt);
end;

function Join(Strings: TStrings; Separator: string = ' '): string;
var
  i, imax: Integer;
begin
  Result := '';
  imax := Strings.Count-1;
  for i := 0 to imax do begin
    Result := Result + Strings[i];
    if i < imax then
      Result := Result + Separator;
  end;
end;

function Join(StrA: TStrA; Separator: string = ' '): string; overload;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to High(StrA) do begin
    Result := Result + StrA[i];
    if i < High(StrA) then
      Result := Result + Separator;
  end;
end;

function MulStr(const S: string; Count: Integer): string;
var
  P: PChar;
  Len, i: Integer;
begin
  Result := '';
  if Count = 0 then
    Exit;

  Len := Length(S);
  SetLength(Result, Len * Count);

  P := Pointer(Result);
  for i := 1 to Count do begin
    Move(Pointer(S)^, P^, Len);
    Inc(P, Len);
  end;
end;

function AlignR(const S: string; Width: Integer; Filler: Char = ' '): string;
begin
  Result := MulStr(Filler, Width - Length(S)) + S;
end;

function MaxStr(const S: string; MaxLen: Integer): string;
var
  Len: Integer;
begin
  Len := Length(S);
  if Len <= MaxLen then begin
    Result := S;
  Exit end;

  Result := Copy(S, 1, MaxLen - 3) + '...';
end;

function TrimAll(const S: string): string;
var
  i: Integer;
begin
  for i := 1 to Length(S) do
    if S[i] > #32 then
      Result := Result + S[i];
end;

function ControlChar(C: Char): Boolean;
begin
  Result := C in StrangeChars;
end;

function FriendlyChar(C: Char): Char;
begin
  case C of
    #0: Result := '.';
    #1..#31: Result := '?';
    #255: Result := '#';
  else
    Result := C;
  end;
end;

function FriendlyStr(const S: string): string;
var
  i: Integer;
begin
  SetLength(Result, Length(S));
  for i := 1 to Length(S) do
    Result[i] := FriendlyChar(S[i]);
end;

function FriendlyStr(a: TByteA): string;
var
  i: Integer;
begin
  SetLength(Result, Length(a));
  for i := 0 to High(a) do
    Result[i + 1] := FriendlyChar(Char(a[i]));
end;

function Quote(const S: string; Quoter: Char = '"'): string;
begin
  Result := S;

  if FirstChar(S) <> Quoter then
    Result := Quoter + Result;

  if LastChar(S) <> Quoter then
    Result := Result + Quoter;
end;

function DeQuote(const S: string): string;
begin
  Result := '';
  if Length(S) > 2 then
    Result := Copy(S, 2, Length(S) - 2);
end;

function UnQuote(const S: string): string;
var
  Start, Len: Integer;
begin
  Start := 1;
  Len := Length(S);

  if (S <> '') and (S[1] in ([#0..#32] + QuoteChars)) then begin
    if (LastChar(S) = S[1]) then
      Dec(Len);
    Inc(Start);
  end;

  Result := Copy(S, Start, Len - Start + 1);
end;

function StrNumerus(const Value: Integer; const Singular, Plural: string;
  const Zero: string = '0'): string;
begin
  if Abs(Value) = 1 then
    Result := IntToStr(Value) + ' ' + Singular
  else if Value = 0 then
    Result := Zero + ' ' + Plural
  else
    Result := IntToStr(Value) + ' ' + Plural;
end;

function MakeStr(const Items: array of const; Separator: string = ''): string;
const
  BoolStrings: array[Boolean] of string = ('False', 'True');

var
  i: Integer;

  function StrOfP(P: Pointer): string;
  begin
    if P = nil then
      Result := '[nil]'
    else
      Result := '[' + IntToStr(Cardinal(P)) + ']';
  end;

  procedure Add(const S: string);
  begin
    Result := Result + s + Separator;
  end;

begin
  Result := '';
  for i := 0 to High(Items) do
    with Items[i] do
      case VType of
        vtString:     Add(VString^);
        vtInteger:    Add(IntToStr(VInteger));
        vtBoolean:    Add(BoolStrings[VBoolean]);
        vtChar:       Add(VChar);
        vtPChar:      Add(VPChar);
        vtExtended:   Add(FloatToStr(VExtended^));
        vtObject:     if VObject is TComponent then
                        Add(TComponent(VObject).Name)
                      else
                        Add(VObject.ClassName);
        vtClass:      Add(VClass.ClassName);
        vtAnsiString: Add(string(VAnsiString));
        vtCurrency:   Add(CurrToStr(VCurrency^));
        vtInt64:      Add(IntToStr(VInt64^));
        vtVariant:    Add(string(VVariant^));

        vtWideChar:   Add(VWideChar);
        vtPWideChar:  Add(VPWideChar);
        vtInterface:  Add(StrOfP(VInterface));
        vtPointer:    Add(StrOfP(VPointer));
        vtWideString: Add(WideString(VWideString));
      end;
  if Result <> '' then
    SetLength(result, Length(Result) - Length(Separator));
end;

procedure ShowText(const Items: array of const; Separator: string = '');
var
  Text: string;
begin
  Text := MakeStr(Items, Separator);

  MessageBox(0, PChar(Text), 'Info', MB_OK and MB_APPLMODAL);
end;

function DeleteChars(const S: string; C: Char): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(S) do
    if S[i] <> C then
      Result := Result + S[i];
end;

function DeleteChars(const S: string; C: TCharSet): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(S) do
    if not (S[i] in C) then
      Result := Result + S[i];
end;

function ExtractChars(const S: string; C: TCharSet): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(S) do
    if S[i] in C then
      Result := Result + S[i];
end;

function CharCount(const S: string; C: Char): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(S) do
    if S[i] = C then
      Inc(Result);
end;

function StrAtPos(const S: string; Pos: Integer; const Str: string): Boolean;
begin
  Result := (Str <> '') and (Str = Copy(S, Pos, Length(Str)));
end;

function TextAtPos(const S: string; Pos: Integer; const Text: string): Boolean;
begin
  Result := (Text <> '') and SameText(Text, Copy(S, Pos, Length(Text)));
end;

function StrAtBegin(const S, Str: string): Boolean;
begin
  Result := StrAtPos(S, 1, Str);
end;

function TextAtBegin(const S, Text: string): Boolean;
begin
  Result := TextAtPos(S, 1, Text);
end;

function CharIn(const S: string; C: Char): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 1 to Length(S) do
    if S[i] = C then Exit;
  Result := False;
end;

function CharIn(const S: string; C: TCharSet): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to Length(S) do begin
    Result := S[i] in C;
    if Result then
      Exit;
  end;
end;

function StrIn(const S, SubStr: string): Boolean;
begin
  Result := PosEx(SubStr, S) > 0;
end;

function StrIn(SL: TStrings; const S: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to SL.Count-1 do begin
    Result := (S = SL[i]);
    if Result then
      Exit;
  end;
end;

function StrIn(A: TStrA; const S: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low(A) to High(A) do begin
    Result := (S = A[i]);
    if Result then
      Exit;
  end;
end;

function TextIn(const S, Text: string): Boolean;
begin
  Result := PosExText(Text, S) > 0;
end;

function TextIn(SL: TStrings; const Text: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to SL.Count-1 do begin
    Result := SameText(Text, SL[i]);
    if Result then
      Exit;
  end;
end;

function TextIn(A: TStrA; const Text: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low(A) to High(A) do begin
    Result := SameText(Text, A[i]);
    if Result then
      Exit;
  end;
end;

function StrIndex(SL: TStrings; const S: string): Integer;
begin
  for Result := 0 to SL.Count-1 do
    if S = SL[Result] then
      Exit;
  Result := -1;
end;

function StrIndex(A: TStrA; const S: string): Integer;
begin
  for Result := Low(A) to High(A) do
    if S = A[Result] then
      Exit;
  Result := -1;
end;

function TextIndex(SL: TStrings; const Text: string): Integer;
begin
  for Result := 0 to SL.Count-1 do
    if SameText(Text, SL[Result]) then
      Exit;
  Result := -1;
end;

function TextIndex(A: TStrA; const Text: string): Integer;
begin
  for Result := Low(A) to High(A) do
    if SameText(Text, A[Result]) then
      Exit;
  Result := -1;
end;

function ReplaceChars(const S: string; Old, New: Char): string;
var
  i: Integer;
begin
  Result := S;
  for i := 1 to Length(Result) do
    if Result[i] = Old then
      Result[i] := New;
end;

function ReplaceChars(const S: string; Old: TCharSet; New: Char): string;
var
  i: Integer;
begin
  Result := S;
  for i := 1 to Length(Result) do
    if Result[i] in Old then
      Result[i] := New;
end;

function Replace(const S, Old, New: string): string;
var
  oldp, ps: Integer;
begin
  ps := 1;
  Result := '';
  while True do begin
    oldp := ps;
    ps := PosEx(Old, S, oldp);
    if ps = 0 then begin
      Result := Result + Copy(S, oldp, Length(S) - oldp + 1);
    Exit; end;
    Result := Result + Copy(S, oldp, ps - oldp) + New;
    Inc(ps, Length(Old));
  end;
end;

function SLOfFile(const FileName: string): TStringList;
begin
  Result := TStringList.Create;
  if FileExists(FileName) then
    Result.LoadFromFile(FileName);
end;

function ContainsEmptyLines(SL: TStrings): Boolean;
begin
  Result := StrIn(SL, '');
end;

procedure DeleteEmptyLines(SL: TStrings);
var
  i: Integer;
begin
  i := 0;
  while i < SL.Count do begin
    if SL[i] = '' then
      SL.Delete(i)
    else
      Inc(i);
  end;
end;

procedure DeleteCommentLines(SL: TStrings; const CommentSign: string = '//');
var
  i: Integer;
begin
  i := 0;
  while i < SL.Count do begin
    if (SL[i] = '') or (StrAtBegin(TrimLeft(SL[i]), CommentSign)) then
      SL.Delete(i)
    else
      Inc(i);
  end;
end;

function FindLine(SL: TStrings; const S: string): Integer;
begin
  for Result := 0 to SL.Count-1 do
    if TextAtBegin(SL[Result], S) then
      Exit;
  Result := -1;
end;

procedure QuickSortSL(SL: TStringList);

  procedure Sort(l, r: Integer);
  var
    i,j: Integer;
    z,x: string;
  begin
    i := l;
    j := r;
    x := SL[(j + i) div 2];
    repeat
      while SL[i] < x do Inc(i);
      while SL[j] > x do Dec(j);
      if i <= j then begin
        z := SL[i];
        SL[i] := SL[j];
        SL[j] := z;
        Inc(i); Dec(j);
      end;
    until i > j;
    if j > l then Sort(l, j);
    if i < r then Sort(i, r);
  end;

begin
  if SL.Count > 0 then
    Sort(0, SL.Count-1);
end;

function IncStrA(StrA: TStrA): Integer;
begin
  SetLength(StrA, Length(StrA) + 1);
  Result := High(StrA);
end;

function StrOfByteA(a: TByteA): string;
begin
  Result := string(Copy(a, 0, Length(a)));
end;

function ByteAOfStr(const S: string): TByteA;
begin
  Result := TByteA(Copy(S, 1, Length(s)));
end;

function ByteAOfInt(i: Integer): TByteA;
begin
  SetLength(Result, SizeOf(Integer));
  Move(i, Pointer(Result)^, SizeOf(Integer));
end;

function IntOfByteA(A: TByteA): Integer;
begin
  Result := 0;
  Move(Pointer(A)^, Result, Min(Length(A), SizeOf(Integer)));
end;

function ByteAOfHex(const Hex: string): TByteA;
var
  i: Integer;
  h: string;
begin
  h := ExtractChars(Hex, HexadecimalChars);
  SetLength(Result, Length(h) div 2);
  for i := 0 to High(Result) do
    Result[i] := ByteOfHex(Copy(h, (i shl 1) + 1, 2));
end;

function SizeOfFile(const FileName: string): Integer;
var
  F: file;
begin
  AssignFile(F, FileName);
  {$I-}Reset(F, 1);{$I+}
  if IOResult = 0 then begin
    Result := FileSize(F);
    CloseFile(F);
  end else
    Result := 0;
end;

function FileEx(const FileName: string; AllowFolders: Boolean = False): Boolean;
var
  FindData: TWin32FindData;
begin
  if FileName = '' then begin
    Result := False;
  Exit; end;

  Result := (AllowFolders and DirectoryExists(FileName)) or
    (FindFirstFile(PChar(FileName), FindData) <> INVALID_HANDLE_VALUE);
  Result := Result and not CharIn(FileName, WildCards);
  Result := Result and (AllowFolders
    or ((FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0));
end;

function LWPSolve(const Dir: string): string;
begin
  if (Dir <> '') and (Dir[Length(Dir)] = '\') then begin
    Result := Copy(Dir, 1, Length(Dir) - 1);
  end else
    Result := Dir;
end;

function LWPSlash(const Dir: string): string;
begin
  if (Dir <> '') and (Dir[Length(Dir)] = '\') then begin
    Result := Copy(Dir, 1, Length(Dir));
  end else
    Result := Dir + '\';
end;

function ExtractDrive(const FileName: string): string;
begin
  Result := '';
  if (Length(FileName) >= 2) and (FileName[2] = ':') then
    Result := UpperCase(FileName[1] + ':\');
end;

function ExtractPath(const FileName: string): string;
var
  p: Integer;
begin
  p := CharPosR('\', FileName);
  if P > 0 then
    Result := Copy(FileName, 1, p)
  else
    Result := FileName;
end;

function ExtractPrefix(const FileName: string): string;
begin
  Result := UntilLastChar(ExtractFileName(FileName), '.');
end;

function ExtractSuffix(const FileName: string): string;
begin
  Result := FromLastChar(ExtractFileName(FileName), '.');
end;

function SameByteA(const A, B: TByteA): Boolean;
begin
  Result := (A = B) or ((Length(A) = Length(B)) and CompareMem(A, B, Length(A)));
end;

function Reverse(A: TByteA): TByteA;
var
  i: Integer;
begin
  SetLength(Result, Length(A));

  for i := 0 to High(A) do
    Result[High(Result) - i] := A[i];
end;

function Endian(i: Integer): Integer;
type
  EndianArray = packed array[0..3] of Byte;
var
  a, b: EndianArray;
begin
  a := EndianArray(i);
  b[0] := a[3];
  b[1] := a[2];
  b[2] := a[1];
  b[3] := a[0];
  Result := Integer(b);
end;

function SaveByteA(Data: TByteA; const FileName: string;
  Overwrite: Boolean = True): Boolean;
var
  F: file;
begin
  if FileExists(FileName) and not Overwrite then begin
    Result := False;
  Exit end;

  AssignFile(F, FileName);
  {$I-}Rewrite(F, 1);{$I+}
  if IOResult = 0 then begin
    if Length(Data) > 0 then
      BlockWrite(F, Data[0], Length(Data));
    CloseFile(F);
    Result := True;
  end else
    Result := False;
end;

function LoadByteA(const FileName: string): TByteA;
var
  F: file;
begin
  AssignFile(F, FileName);
  {$I-}Reset(F, 1);{$I+}
  if IOResult = 0 then begin
    SetLength(Result, FileSize(F));
    if Length(Result) > 0 then
      BlockRead(F, Result[0], FileSize(F));
    CloseFile(F);
  end else
    SetLength(Result, 0);
end;

function IsValidFileName(const FileName: string): Boolean;
begin
  Result := (FileName <> '') and not CharIn(FileName, FileNameEnemies)
    and CharIn(Trim(FileName), AllChars - ['.']);
end;

function MakeValidFileName(FileName: string; const Default: string = 'File'): string;
begin
  if FileName = '' then
    FileName := Default;

  if CharIn(FileName, FileNameEnemies) then
    Result := ReplaceChars(FileName, FileNameEnemies, '_')
  else if not CharIn(Trim(FileName), AllChars - ['.']) then
    Result := Default
  else
    Result := FileName;
end;

function IsValidInteger(const S: string): Boolean;
{const
  LowInt = '2147483648';
  HighInt = '2147483647';
var
  len, RealLen, i, o: Integer;
  c: Char;
begin
  Result := False;
  if S = '' then
    Exit;

  len := Length(S);
  o := 1;
  
  if S[1] = '-' then begin
    if len = 1 then
      Exit;
    Inc(o);
    while (o <= len) and (S[o] = '0') do
      Inc(o);
    if o > len then
      Exit;
    if o < len then begin
      RealLen := len - o + 1;
      if RealLen > Length(LowInt) then
        Exit
      else if RealLen = Length(LowInt) then begin
        for i := 1 to Length(LowInt) do begin
          c := S[i + o - 1];
          if (c < '0') or (c > LowInt[i]) then
            Exit;
          if c in ['0'..Char((Byte(LowInt[i])-1))] then
            Break;
        end;
        Inc(o, i);
      end;
    end;
  end else begin
    while (o <= len) and (S[o] = '0') do
      Inc(o);
    if o <= len then begin
      RealLen := len - o + 1;
      if RealLen > Length(HighInt) then
        Exit
      else if RealLen = Length(HighInt) then begin
        for i := 1 to Length(HighInt) do begin
          c := S[i + o - 1];
          if (c < '0') or (c > HighInt[i]) then
            Exit;
          if c in ['0'..Char((Byte(HighInt[i])-1))] then
            Break;
        end;
        Inc(o, i);
      end;
    end;
  end;

  for i := o to len do
    if not (S[i] in ['0'..'9']) then
      Exit;

  Result := True;  }
var
  i: Int64;
begin
  i := StrToInt64Def(S, High(Int64));
  Result := (i >= Low(Integer)) and (i <= High(Integer));
end;

function IsValidCardinal(const S: string): Boolean;
{const
  HighCard = '4294967295';
var
  len, RealLen, i, o: Integer;
begin
  Result := False;
  if S = '' then
    Exit;

  len := Length(S);
  o := 1;
  
  while (o <= len) and (S[o] = '0') do
    Inc(o);
  if o <= len then begin
    RealLen := len - o + 1;
    if RealLen > Length(HighCard) then
      Exit
    else if RealLen = Length(HighCard) then begin
      for i := 1 to Length(HighCard) do begin
        if S[i + o - 1] > HighCard[i] then
          Exit;
        if S[i + o - 1] in ['0'..Char((Byte(HighCard[i])-1))] then
          Break;
      end;
      Inc(o, i);
    end;
  end;

  for i := o to len do
    if not (S[i] in ['0'..'9']) then
      Exit;

  Result := True;  }
var
  i: Int64;
begin
  i := StrToInt64Def(S, -1);
  Result := (i >= 0) and (i <= High(Cardinal));
end;

function StrOfBool(flag: Boolean; const TrueStr: string = 'True';
  const FalseStr: string = 'False'): string;
begin
  if Flag then
    Result := TrueStr
  else
    Result := FalseStr;
end;

function StrOfInt(i: Integer): string;
begin
{  if i = 0 then begin
    Result := '0';
  Exit end;

  while i > 0 do begin
    Result := Char(Byte('0') + (i mod 10)) + Result;
    i := i div 10;
  end;}
  Result := IntToStr(i);
end;

function CardOfStr(const S: string): Cardinal;
var
  Res: Int64;
begin
  Res := StrToInt64Def(S, -1);
  if Res > High(Cardinal) then
    Res := High(Cardinal)
  else if Res < 0 then
    Res := 0;
  Result := Cardinal(Res);
end;

function HexOrd(Hex: Char): THex;
begin
  case Hex of
    '0'..'9':
      Result := Byte(Hex) - 48;
    'A'..'F':
      Result := Byte(Hex) - 55;
    'a'..'f':
      Result := Byte(Hex) - 87;
    else
      Result := 0;
  end;
end;

function ByteOfHex(Hex: THexByteStr): Byte;
begin
  Result := (HexOrd(Hex[1]) shl 4) + HexOrd(Hex[2]);
end;

function DecOfHex(const Hex: string): string;
begin
  Result := IntToStr(CardOfHex(Hex));
end;

function HexOfByte(b: Byte): THexByteStr;
begin
  Result := HexChar[(b and $F0) shr 4]
          + HexChar[ b and $0F       ];
end;

{function HexOfCard2(c: Cardinal): string;
var
  Data: array[0..(1 shl 4) - 1] of Char;
  i: Integer;
begin
  for i := 0 to (1 shl 4) - 1 do
    if i < 10 then
      Data[i] := Char(Ord('0') + i)
    else
      Data[i] := Char(Ord('A') + i - 10);

  Result := Data[(c and (((1 shl (1 shl 2)) - 1) shl (7 shl 2))) shr (7 shl 2)]
          + Data[(c and (((1 shl (1 shl 2)) - 1) shl (6 shl 2))) shr (6 shl 2)]
          + Data[(c and (((1 shl (1 shl 2)) - 1) shl (5 shl 2))) shr (5 shl 2)]
          + Data[(c and (((1 shl (1 shl 2)) - 1) shl (4 shl 2))) shr (4 shl 2)]
          + Data[(c and (((1 shl (1 shl 2)) - 1) shl (3 shl 2))) shr (3 shl 2)]
          + Data[(c and (((1 shl (1 shl 2)) - 1) shl (2 shl 2))) shr (2 shl 2)]
          + Data[(c and (((1 shl (1 shl 2)) - 1) shl (1 shl 2))) shr (1 shl 2)]
          + Data[(c and (((1 shl (1 shl 2)) - 1) shl (0 shl 2))) shr (0 shl 2)];
end; }

function HexOfCard(i: Cardinal): string;
var
  a: Cardinal;
begin
  Result := '';
  while i > 0 do begin
    a := i and $F;
    Result := HexChar[a] + Result;
    i := i shr 4;
  end;
end;

function HexOfCard(i: Cardinal; Digits: Integer): string;
var
  a: Cardinal;
begin
  Result := '';
  while i > 0 do begin
    a := i and $F;
    Result := HexChar[a] + Result;
    i := i shr 4;
  end;
  Result := MulStr('0', Digits - Length(Result)) + Result;
end;

function PascalHexArray(a: TByteA; Name: string): string;
var
  i, len: Integer;
begin
  Result := 'const' + EOL +
    '  ' + Name + ': array[0..' + IntToStr(High(a)) + '] of Byte = (';

  len := Length(a);
  for i := 0 to len-1 do begin
    if (i mod 19) = 0 then
      Result := Result + EOL + '  ' + '  ';
    Result := Result + '$' + HexOfByte(a[i]);
    if i < len-1 then
      Result := Result + ',';
  end;
  Result := Result + EOL + '  );';
end;

function HexOfByteA(a: TByteA; Blocks: Integer = 1;
  const Splitter: string = ' '): string;
var
  i: Integer;
begin
  Result := '';

  if Blocks > 0 then
    for i := 0 to High(a) do begin
      Result := Result + HexOfByte(a[i]);
      if i < High(a) then
        if ((i+1) mod Blocks) = 0 then
          Result := Result + Splitter;
    end
  else
    for i := 0 to High(a) do
      Result := Result + HexOfByte(a[i]);
end;

function BinOfByteA(a: TByteA; Blocks: Integer = 4;
  const Splitter: string = ' '): string;
var
  i, max: Integer;
  Bit: Boolean;
begin
  Result := '';

  if Blocks > 0 then begin
    max := 8 * (High(a)) + 7;
    for i := 0 to max do begin
      Bit := 7-(i mod 8) in TBitSet(a[i div 8]);
      Result := Result + Char(Byte('0') + Byte(Bit));
      if i < max then
        if ((i+1) mod Blocks) = 0 then
          Result := Result + Splitter;
    end;
  end else
    for i := 0 to High(a) do
      Result := Result + Char(Byte('0') + a[i] shr (i and 8));
end;

function CardOfHex(Hex: string): Cardinal;
var
  i: Integer;
begin
  Result := 0;
  Hex := Copy(ExtractChars(Hex, HexadecimalChars), 1, 8);

  for i := 1 to Length(Hex) do
    if Hex[i] <> '0' then
      Inc(Result, HexOrd(Hex[i]) shl ((Length(Hex) - i) shl 2));
end;

function IntOfBin(Bin: string): Cardinal;
var
  i: Integer;
begin
  Result := 0;
  Bin := Copy(ExtractChars(Bin, BinaryChars), 1, 32);

  for i := Length(Bin) downto 1 do
    if Bin[i] = '1' then
      Inc(Result, 1 shl (Length(Bin) - i));
end;

function BinOfInt(n: Cardinal): string;
var
  a: Integer;
begin
  if n = 0 then begin
    Result := '0';
  exit; end;

  Result := '';
  while n > 0 do begin
    a := n and 1;
    Result := Char(a + Byte('0')) + Result;
    n := n shr 1;
  end;
end;

function BinOfIntFill(n: Cardinal; MinCount: Integer = 8): string;
var
  a: Integer;
begin
  if n = 0 then begin
    Result := MulStr('0', MinCount);
  Exit; end;

  Result := '';
  while n > 0 do begin
    a := n and 1;
    Result := Char(a + Byte('0')) + Result;
    n := n shr 1;
  end;
  Result := MulStr('0', MinCount - Length(Result)) + Result;
end;

function BaseNOfInt(I: Cardinal; B: TBaseN): string;
var
  a: Integer;
begin
  if (B < 2) or (i = 0) then begin
    Result := '0';
  Exit; end;

  Result := '';
  while i > 0 do begin
    a := i mod B;
    Result := BaseNChar[a] + Result;
    i := i div B;
  end;
end;

function IntOfBaseN(V: string; B: TBaseN): Cardinal;
var
  i: Integer;
  F: Cardinal;
  c: Byte;
begin
  Result := 0;
  V := TrimAll(V);
  F := 1;
  for i := Length(V) downto 1 do begin
    c := Byte(UpCase(V[i]));
    case Char(c) of
      '0'..'9': c := c - 48;
      'A'..'Z': c := c - 55;
    end;
    if c < B then
      Result := Result + Byte(c) * F;
    F := F * B;
  end;
end;

function KeepIn(i, Bottom, Top: Variant): Variant;
begin
  Result := i;
  if Result > Top then
    Result := Top
  else if Result < Bottom then
    Result := Bottom;
end;

function InRange(Value, Bottom, Top: Variant): Boolean;
begin
  Result := (Value >= Bottom) and (Value <= Top);
end;

function InStrictRange(Value, Bottom, Top: Variant): Boolean;
begin
  Result := (Value > Bottom) and (Value < Top);
end;

function Min(const A, B: Integer): Integer;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Min(const A: TIntA): Integer;
var
  i: Integer;
begin
  Result := 0;
  if Length(A) = 0 then
    Exit;

  Result := A[0];
  for i := 1 to High(A) do
    if A[i] < Result then
      Result := A[i];
end;

function Max(const A, B: Integer): Integer;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function Max(const A: TIntA): Integer;
var
  i: Integer;
begin
  Result := 0;
  if Length(A) = 0 then
    Exit;

  Result := A[0];
  for i := 1 to High(A) do
    if A[i] > Result then
      Result := A[i];
end;

function RangesOfStr(const S: string): TRanges;
var
  SL: TStringList;
  r, b, t: string;
  i, p: Integer;

  function TryStrToCard(const S: string; out Value: Cardinal): Boolean;
  var
    E: Integer;
  begin
    Val(S, Value, E);
    Result := E = 0;
  end;

begin
  Result := nil;
  SL := TStringList.Create;
  try
    Split(S, RangesSeparator, SL);
    SetLength(Result, SL.Count);
    for i := 0 to SL.Count-1 do begin
      r := SL[i];
      with Result[i] do begin
        p := CharPos(RangeInnerSeparator, r);
        Simple := p = 0; // no '-' found
        if Simple then begin
          if r = RangeInfinite then begin // * --> *-*
            Simple := False;
            Bottom := Low(Bottom);
            Top := High(Top);
          end else if not TryStrToCard(r, Value) then
            Break;

        end else begin
          TileStr(r, p, p, b, t);

          if b = RangeInfinite then
            Bottom := Low(Bottom)
          else if not TryStrToCard(b, Bottom) then
            Break;

          if t = RangeInfinite then
            Top := High(Top)
          else if not TryStrToCard(t, Top) then
            Break;
          if Bottom > Top then begin
            p := Bottom; Bottom := Top; Top := p;
          end;
        end;
      end;
    end;

    if i <> SL.Count then
      Result := nil;

  finally
    SL.Free;
  end;
end;

function InRanges(Ranges: TRanges; TestValue: Cardinal): Boolean;
var
  i: Integer;
begin
  Result := True;

  for i := 0 to High(Ranges) do
    with Ranges[i] do
      if Simple then begin
        if TestValue = Value then
          Exit;
      end else begin
        if InRange(TestValue, Bottom, Top) then
          Exit;
      end;

  Result := False;
end;

procedure WriteSL(Strings: TStrings; const Prefix: string = '';
  const Suffix: string = '');
var
  i: Integer;
begin
  for i := 0 to Strings.Count-1 do
    WriteLn(Prefix + Strings[i] + Suffix);
end;

function Success(Res: Integer; ResultOnSuccess: Integer = ERROR_SUCCESS): Boolean;
begin
  Result := (Res = ResultOnSuccess);
  LastSuccessRes := Res;
end;

function Failure(Res: Integer; ResultOnSuccess: Integer = ERROR_SUCCESS): Boolean;
begin
  Result := not Success(Res, ResultOnSuccess);
end;

function ExpandString(const S: string): string;
var
  Len: Integer;
  P, Res: PChar;
begin
  Result := '';
  P := PChar(S);
  Len := ExpandEnvironmentStrings(P, nil, 0);
  if Len = 0 then
    Exit;

  GetMem(Res, Len);
  ExpandEnvironmentStrings(P, Res, Len);

  Result := Res;
  FreeMem(Res, Len);
end;

function FindAll(Strings: TStrings; const Mask: string;
  ScanSubDirs: Boolean = True; Attributes: Integer = faFindEveryFile;
  FileReturn: TFileNameFunc = nil): Boolean;
var
  Path, FileName: string;

  procedure ScanDir(const Path, FileName: string);
  var
    PSR: TSearchRec;
    Res: Integer;

    procedure Add(const S: string);
    begin
      if S <> '' then
        Strings.Add(S);
    end;

  begin
    Res := FindFirst(Path + FileName, Attributes, PSR);
    while Success(Res, 0) do begin
      if Assigned(FileReturn) then
        Add(FileReturn(Path + PSR.Name))
      else
        Add(Path + PSR.Name);
      Res := FindNext(PSR);
    end;
    FindClose(PSR);
    if not ScanSubDirs then
      Exit;

    Res := FindFirst(Path + '*', faDirectory, PSR);
    while Success(Res, 0) do begin
      if (PSR.Attr and faDirectory > 0)
       and (PSR.Name <> '.') and (PSR.Name <> '..') then
        ScanDir(Path + PSR.Name + '\', FileName);
      Res := FindNext(PSR);
    end;
    FindClose(PSR);
  end;

begin
  Strings.Clear;
  Path := ExtractPath(Mask);
  FileName := ExtractFileName(Mask);
  ScanDir(Path, FileName);
  Result := Strings.Count > 0;
end;

function FindAllFirst(const Mask: string; ScanSubDirs: Boolean = True;
  Attributes: Integer = faFindEveryFile): string;
var
  Path, FileName: string;

  function ScanDir(const Path, FileName: string): Boolean;
  var
    PSR: TSearchRec;
    Res: Integer;
  begin
    Result := False;
    if Success(FindFirst(Path + FileName, Attributes, PSR), 0) then begin
      FindAllFirst := Path + PSR.Name;
      Result := True;
      FindClose(PSR);
    Exit; end;
    if not ScanSubDirs then
      Exit;

    Res := FindFirst(Path + '*', faDirectory, PSR);
    while not Result and Success(Res, 0) do begin
      if (PSR.Attr and faDirectory > 0)
       and (PSR.Name <> '.') and (PSR.Name <> '..') then
        Result := ScanDir(Path + PSR.Name + '\', FileName);
      Res := FindNext(PSR);
    end;
    FindClose(PSR);
  end;
begin
  Result := '';
  Path := ExtractPath(Mask);
  FileName := ExtractFileName(Mask);
  ScanDir(Path, FileName);
end;

procedure DeleteFiles(const Mask: string; ScanSubDirs: Boolean = True;
  Attributes: Integer = faFindEveryFile);
var
  Path, FileName: string;

  procedure ScanDir(const Path, FileName: string);
  var
    PSR: TSearchRec;
    Res: Integer;

    procedure TryDeleteFile(const FileName: string);
    begin
      try
        DeleteFile(Path + PSR.Name);
      except
      end;
    end;

  begin
    Res := FindFirst(Path + FileName, Attributes, PSR);
    while Success(Res, 0) do begin
      TryDeleteFile(Path + PSR.Name);
      Res := FindNext(PSR);
    end;
    FindClose(PSR);
    if not ScanSubDirs then
      Exit;

    Res := FindFirst(Path + '*', faDirectory, PSR);
    while Success(Res, 0) do begin
      if (PSR.Attr and faDirectory > 0)
       and (PSR.Name <> '.') and (PSR.Name <> '..') then begin
        ScanDir(Path + PSR.Name + '\', FileName);
        TryDeleteFile(Path + PSR.Name);
      end;
      Res := FindNext(PSR);
    end;
    FindClose(PSR);
  end;
begin
  Path := ExtractPath(Mask);
  FileName := ExtractFileName(Mask);
  ScanDir(Path, FileName);
end;

function GetFileNew(FileName: string; NoFloppyDrives: Boolean = True): string;
var
  Drive: string;
  pf, pd, Len: Integer;
  PSR: TSearchRec;
begin
  Result := '';
  FileName := Trim(FileName);
  if Length(FileName) < 2 then
    Exit;

  Drive := ExtractDrive(FileName);
  if not DirectoryExists(Drive) then
    Exit;

  if NoFloppyDrives and (Drive[1] in ['A', 'B']) then
    Exit;

  Len := Length(FileName);
  Result := Drive;
  pf := Length(Drive) + 1;
  while pf <= Len do begin
    if FileName[pf] = '\' then begin
      Result := Result + '\';
      Inc(pf);
    Continue; end;

    pd := CharPos('\', FileName, pf);
    if pd = 0 then begin
      if 0=FindFirst(Result + Copy(FileName, pf, MaxInt), faFindEveryFile, PSR) then begin
        Result := Result + PSR.Name;
      Break; end else begin
        FindClose(PSR);
        if 0=FindFirst(Result + Copy(FileName, pf, MaxInt), faDirectory, PSR) then
          Result := Result + PSR.Name + '\'
        else
          Result := '';
        FindClose(PSR);
        if Result = '' then
          Break;
      end;
    end;

    if 0=FindFirst(Result + Copy(FileName, pf, pd - pf), faDirectory, PSR) then
      Result := Result + PSR.Name + '\'
    else
      Result := '';
    FindClose(PSR);
    if Result = '' then
      Break;

    pf := pd + 1;
  end;

  if (Result <> '') and not FileEx(Result, True) then
    Result := '';
end;

function DateTimeOfFileTime(const FileTime: TFileTime): TDateTime;
var
  LocalFileTime: TFileTime;
  Res: Integer;
begin
  Result := 0;

  FileTimeToLocalFileTime(FileTime, LocalFileTime);
  if not FileTimeToDosDateTime(LocalFileTime, LongRec(Res).Hi,
   LongRec(Res).Lo) then
    Res := -1;

  if (Res = -1) or (Res = 0) then
    Exit;
  try
    Result := FileDateToDateTime(Res);
  except
  end;
end;

procedure FileNew(const FileName: string);
var
  Handle: Integer;
begin
  Handle := FileCreate(FileName);
  FileClose(Handle);
end;

function Win32PlatformStr: string;
const
  PlatformStrings: array[VER_PLATFORM_WIN32s..VER_PLATFORM_WIN32_NT] of string =
    ('VER_PLATFORM_WIN32s', 'VER_PLATFORM_WIN32_WINDOWS', 'VER_PLATFORM_WIN32_NT');
begin
  Result := PlatformStrings[Win32Platform];
end;

function FullOSInfo: string;
begin
  Result := Format(
    'Platform: %s' + EOL +
    'Version: %d.%d Build %d' + EOL +
    'CSD: %s',
    [
      Win32PlatformStr,
      Win32MajorVersion, Win32MinorVersion, Win32BuildNumber,
      Win32CSDVersion
    ]
  );
end;

function Win9x: Boolean;
begin
  Result := Win32Platform = VER_PLATFORM_WIN32_WINDOWS;
end;

function WinNT: Boolean;
begin
  Result := Win32Platform = VER_PLATFORM_WIN32_NT;
end;

function Win2000: Boolean;
begin
  Result := (Win32Platform = VER_PLATFORM_WIN32_NT)
            and (Win32MajorVersion = 4);
end;

function WinXP: Boolean;
begin
  Result := Win32MajorVersion >= 5;
end;

initialization
  MyDir := GetMyDir;

end.

unit FifoStream;

interface

uses Classes, windows, Dialogs;

const
  DefaultChunksize = 32768; // 32kb per chunk as default.

type
  PMemChunk = ^TMemChunk;
  TMemChunk = record
    Filled: Longword;
    Read: Longword;
    Data: pointer;
  end;

  TFifo = class
  private
    FBuffers: TList;
    FChunksize: Longword;
    FCritSect: TRTLCriticalSection;
    FIsWinNT: boolean;
    FBytesInFifo: LongWord;
  protected
    function GetBytesInFifo: LongWord;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Write(Data: pointer; Size: LongWord);
    procedure Read(Buff: pointer; var ReqSize: LongWord);
    procedure PeekData(Buff: pointer; var ReqSize: LongWord);
  published
    property BytesInFifo: LongWord read FBytesInFifo;
  end;

implementation

constructor TFifo.Create;
begin
  inherited;
  FBuffers := TList.Create;
  // set default chunksize...
  FChunksize := DefaultChunksize;
  InitializeCriticalSection(FCritSect);
end;

destructor TFifo.Destroy;
var
  I: Integer;
begin
  EnterCriticalSection(FCritSect);
  for I := 0 to FBuffers.count - 1 do
  begin
    FreeMem(PMemChunk(Fbuffers[I]).Data);
    Dispose(PMemChunk(Fbuffers[I]));
  end;
  FBuffers.Clear;
  FBuffers.Free;
  LeaveCriticalSection(FCritSect);

  DeleteCriticalSection(FCritSect);
  inherited;
end;

function TFifo.GetBytesInFifo: LongWord;
begin
  Result := 0;
  if FBuffers.Count = 0 then
  begin
    exit;
  end
  else
  begin
    if FBuffers.Count > 1 then
      Inc(Result, (FBuffers.Count - 1) * FChunkSize);
    Inc(Result, PMemChunk(FBuffers[Fbuffers.Count - 1]).Filled);
    Dec(Result, PMemChunk(FBuffers[0]).Read);
  end;
end;

procedure TFifo.Write(Data: pointer; Size: LongWord);
var
  Privpointer: pointer;
  PrivSize: LongWord;
  Chunk: PMemChunk;
  PosInChunk: pointer;
begin
  if LongWord(Data) = 0 then
  begin
    // null pointer? somebody is trying to fool us, get out...
    Exit;
  end;
  EnterCriticalSection(FCritSect);
  PrivPointer := Data;
  PrivSize := 0;
  // are already buffers there?
  if FBuffers.count > 0 then
  begin
    // is the last one of them not completely filled?
    if PMemChunk(FBuffers[FBuffers.count - 1]).filled < FChunksize then
      // not completely filled, so fill up the buffer.
    begin
      Chunk := PMemChunk(FBuffers[FBuffers.count - 1]);
      // fetch chunkdata.
      PosInChunk := Chunk.Data;
      // move to current fill pos...
      Inc(LongWord(PosInChunk), Chunk.Filled);
      // can we fill the chunk completely?
      if Size > FChunksize - Chunk.Filled then
      begin
        // yes we can.
        Move(PrivPointer^, PosInChunk^, FChunksize - Chunk.Filled);
        Inc(PrivSize, FChunksize - Chunk.Filled);
        Inc(LongWord(PrivPointer), FChunksize - Chunk.Filled);
        Chunk.Filled := FChunkSize;
      end
      else
        // we have to less data for filling the chunk completely,
        // just put everything in.
      begin
        Move(PrivPointer^, PosInChunk^, Size);
        Inc(PrivSize, Size);
        Inc(Chunk.Filled, Size);
      end;
    end;
  end;
  // as long as we have remaining stuff put it into new chunks.
  while (PrivSize < Size) do
  begin
    new(Chunk);
    GetMem(Chunk.Data, FChunksize);
    Chunk.Read := 0;
    // can we fill an entire chunk with the remaining data?
    if Privsize + FChunksize < Size then
    begin
      // yes we can, so put the stuff in.
      Move(Privpointer^, Chunk.Data^, FChunksize);
      Inc(LongWord(PrivPointer), FChunksize);
      Inc(PrivSize, FChunksize);
      Chunk.Filled := FChunksize;
    end
    else // we have to less data to fill the entire chunk, just put the remaining stuff in.
    begin
      Move(Privpointer^, Chunk.Data^, Size - Privsize);
      Chunk.Filled := Size - Privsize;
      Inc(PrivSize, Size - Privsize);
    end;
    Fbuffers.Add(Chunk);
  end;
  if Size <> Privsize then
    Showmessage('miscalculation in TFifo.write');
  FBytesInFifo := GetBytesInFifo;
  LeaveCriticalSection(FCritSect);
end;

procedure TFifo.Read(Buff: pointer; var ReqSize: LongWord);
var
  PrivSize: Integer;
  Privpos: pointer;
  Chunk: PMemChunk;
  ChunkPos: pointer;
begin
  if LongWord(Buff) = 0 then
  begin
    // null pointer? somebody is trying to fool us, get out...
    Exit;
  end;
  EnterCriticalSection(FCritSect);
  PrivSize := 0;
  Privpos := Buff;
  while FBuffers.Count > 0 do
  begin
    Chunk := PMemChunk(FBuffers[0]);
    ChunkPos := Chunk.data;
    Inc(LongWord(ChunkPos), Chunk.Read);
    // does the remaining part of the chunk fit into the buffer?
    if PrivSize + (Chunk.Filled - Chunk.read) < ReqSize then
    begin // yep, it fits
      Move(ChunkPos^, Privpos^, Chunk.Filled - Chunk.read);
      Inc(PrivSize, Chunk.Filled - Chunk.read);
      FreeMem(Chunk.Data);
      Dispose(Chunk);
      FBuffers.Delete(0);
    end
    else // remaining part didn't fit, get as much as we can and increment the
      // read attribute.
    begin
      Move(ChunkPos^, Privpos^, ReqSize - PrivSize);
      Inc(Chunk.read, ReqSize - PrivSize);
      Inc(PrivSize, ReqSize - PrivSize);
      // as we filled the buffer, we'll have to break here.
      break;
    end;
  end;
  FBytesInFifo := GetBytesInFifo;
  LeaveCriticalSection(FCritSect);
  ReqSize := PrivSize;
end;

// read Data from Stream without removing it from the Stream...

procedure TFifo.PeekData(Buff: pointer; var ReqSize: LongWord);
var
  PrivSize: Integer;
  Privpos: pointer;
  Chunk: PMemChunk;
  ChunkPos: pointer;
  ChunkNr: Integer;
begin
  if LongWord(Buff) = 0 then
  begin
    // null pointer? somebody is trying to fool us, get out...
    Exit;
  end;
  EnterCriticalSection(FCritSect);
  PrivSize := 0;
  Privpos := Buff;
  ChunkNr := 0;
  while FBuffers.Count > ChunkNr do
  begin
    Chunk := PMemChunk(FBuffers[ChunkNr]);
    ChunkPos := Chunk.data;
    Inc(LongWord(ChunkPos), Chunk.Read);
    // does the remaining part of the chunk fit into the buffer?
    if PrivSize + (Chunk.Filled - Chunk.read) < ReqSize then
    begin // yep, it fits
      Move(ChunkPos^, Privpos^, Chunk.Filled - Chunk.read);
      Inc(PrivSize, Chunk.Filled - Chunk.read);
      Inc(ChunkNr);
    end
    else // remaining part didn't fit, get as much as we can and increment the
      // read attribute.
    begin
      Move(ChunkPos^, Privpos^, ReqSize - PrivSize);
      Inc(PrivSize, ReqSize - PrivSize);
      // as we filled the buffer, we'll have to break here.
      break;
    end;
  end;
  LeaveCriticalSection(FCritSect);
  ReqSize := PrivSize;
end;

end.
