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
{ /lib/system/filesystem.pp                                                   }
{ Smattering of Filesystem and Directory routines                             }
{*****************************************************************************}

{$mode objfpc}
{$h+}

Unit Filesystem;

Interface

uses
  Classes, SysUtils, StrUtils,
  {$IFDEF UNIX}
  BaseUnix
  {$ENDIF}
  ;

type DirList = Array of String;  // Dynamic array of Strings to hold a directory listing

function GetSubDirectories(BaseDirName : String) : DirList;
function StripParentDir(PathName : String) : String;
procedure SaveStringToFile(FileName : String; S : String);

implementation

(* GetSubDirectories - returns a listing of directory names located in the
    specified Base Directory. (this is a non recursive listing)
*)
function GetSubDirectories(BaseDirName : String) : DirList;
Var
  BaseDir     : PDir;
  DirEntries  : PDirent;
  Entry       : Longint = 0;
  CurEntry    : String;      // Current dir listing record to scan
  FileType    : Longint;     // FileType
  D           : DirList;     // Dynamic array to store discovered directories
  L           : Integer =0;  // Current Length of dynamic array or number of dirs added

begin
  // BUG - TODO Check for a trailing / and add if missing
  BaseDir:=fpOpenDir(BaseDirName);

  Repeat
    DirEntries:=fpReadDir (BaseDir^);
    If DirEntries<>Nil then
      With DirEntries^ do
        begin
        if (pchar(@d_name[0]) = '.') or (pchar(@d_name[0]) = '..') then
          continue
        else
          begin
            CurEntry := BaseDirName + pchar(@d_name[0]);
            FileType := FileGetAttr(CurEntry);
            If FileType<> (-1) then
              begin
                If (FileType and faDirectory)<>0  then
                  begin
                    L := L + 1;
                    SetLength(D,(L));
                    D[L-1] := CurEntry;
                  end
              end;
            Entry := Entry + 1;
          end;
        end;
  Until DirEntries=Nil;
  fpCloseDir (BaseDir^);
  result := D;
end;

// Strip out all parts of a filename path before the last slash
function StripParentDir(PathName : String) : String;
begin
  result := ExtractWord(WordCount(PathName,['/']), PathName, ['/']);
end;

// Save filestream
// Output JSON file to disc
procedure SaveStringToFile(FileName : String; S : String);
var
 FS : TFileStream;              // Filename Handler
 B : Boolean;
begin
  try
    FS:=TFileStream.create(FileName,fmOpenWrite or fmCreate);
    FS.Write(PChar(S)^, Length(S));
  finally
    FS.Free;
  end;
end;


end.
