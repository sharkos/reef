{*****************************************************************************}
{                   _                                                         }
{       /\/\   ___ | | ____ _ _ __ _ __ __ _                                  }
{      /    \ / _ \| |/ / _` | '__| '__/ _` |                                 }
{     / /\/\ \ (_) |   < (_| | |  | | | (_| |                                 }
{     \/    \/\___/|_|\_\__,_|_|  |_|  \__,_|                                 }
{                                                                             }
{ Mokarra                                                                     }
{   (C)opyright 2011-2015 - LeafScale Systems, Inc.                           }
{                                                                             }
{ This software is protected under the LeafScale Software License             }
{                                                                             }
{*****************************************************************************}
{ /lib/io/textutils.pas                                                       }
{ Library file for handling text strings & various conversions                }
{*****************************************************************************}
{$H+}
unit TextUtils;

interface

uses
  Classes, SysUtils, BaseUnix, Crt;

Procedure UpperCase(Var S: String);
Procedure UpperChar(Var C: Char);
Function GetTextLineBySubStr(FileName, SubString: String) : STRING;
Function GetTextLineByNum(FileName: String; LineNum: Integer) : STRING;
Function ParseTextLineVariable(InputLine: String; Separator: Char):STRING;
Function Str2Int(Str: String): Integer;
Function Int2Str(I: Integer): STRING;
Function Int642AnsiStr(I: Int64): AnsiString;
Function AnsiStr2Int64(Str: AnsiString): Int64;
Function Bool2Str(B: Boolean): String;
Function CurrentTimestamp(): String;

implementation

{ Make all Chars in S upper case}
Procedure UpperCase(Var S: String);

Var
  I: Integer;
begin
  For I := 1 to Length(S) do
    S[I] := Upcase(S[I]);
end;

{ Make Char in C upper case}
Procedure UpperChar(Var C: Char);
begin
    C := Upcase(C);
end;


Function GetTextLineBySubStr(Filename, SubString: String) : STRING;
  var
    FileVar: Text;
    OneLine, Temporary: String;
    LineNumber: Integer;
 begin
  Assign(FileVar, FileName);
  Reset(FileVar);
  if Length(SubString) > 0 then
  begin
    UpperCase(SubString);
    LineNumber := 0;
    While not Eof(FileVar) do
    begin
      Readln(FileVar, OneLine);
      Inc(LineNumber);
      Temporary := OneLine;
      UpperCase(Temporary);
      if Pos(SubString, Temporary) >0 then
        begin
          Close(FileVar);
          Exit(OneLine);
        end
    end
  end;
  Close(FileVar);
  Result := 'Unknown';
end;

{* Get a text line by Line Number *}
Function GetTextLineByNum(FileName: String; LineNum: Integer) : STRING;
  var
    FileVar: Text;
    CurLine: Integer = 1; // Set to 1 as lines in txt files start with 1 not 0
    Temporary: String;
begin
  Assign(FileVar, FileName);
  Reset(FileVar);
  While not Eof(FileVar) do
    begin
      Readln(FileVar, Temporary);
      if (CurLine = LineNum) then
        begin
          Close(FileVar);
          Exit(Temporary);
        end;
      Inc(CurLine);
    end;
    Close(FileVar);
    Result := '';
end;

{* Parse a text line variable (ie: Bash Variable File) *}
Function ParseTextLineVariable(InputLine: String; Separator: Char):STRING;
  var
    NewString : STRING = '';
    I : INTEGER; // Current Iterator
    J : INTEGER; // Current Pointer to Separator Position
    L : INTEGER; // Length of String
begin
   L := Length(InputLine);
   For I := 1 to L do
    begin
      If InputLine[I] = Separator then
        begin
          J := (I+1);
          While J <= L do
            begin
              NewString := NewString + InputLine[J];
              J := (J+1);
            end;
          Exit(NewString);
        end;
    end;
    Result := NewString;
end;

Function Str2Int(Str: String): Integer;
VAR I: Integer;

BEGIN
   VAL(Str,I);
   Str2Int := I;
END;

Function Int2Str(I: Integer): String;
VAR S: String;

BEGIN
   STR(I,S);
   Int2Str := S;
END;

Function Int642AnsiStr(I: Int64): AnsiString;
VAR S: AnsiString;

BEGIN
   STR(I,S);
   Int642AnsiStr := S;
END;

Function AnsiStr2Int64(Str: AnsiString): Int64;
VAR I: Integer;

BEGIN
   VAL(Str,I);
   AnsiStr2Int64 := I;
END;

Function Bool2Str(B: Boolean): String;
  var
    S: String = '';

begin
   If B = TRUE then
     S := 'True'
   else
     S := 'False';
   Bool2Str := S;
end;

// Returns the current timestamp as a string (YearMonthDayHourMinute)
Function CurrentTimestamp(): String;
Var Curtime : TDateTime;
Begin
  CurTime := Now;
  result := FormatDateTime('YYYYMMDDhhnn',CurTime);
end;


end.

