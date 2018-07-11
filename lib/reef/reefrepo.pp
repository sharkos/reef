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
{ /lib/reef/reefrepo.pp                                                       }
{ Reef Master Index & Reef Repo Index handlers                                }
{*****************************************************************************}
{$mode objfpc}{$H+}

unit ReefRepo;

interface

uses
  Classes, SysUtils, fpJSON, JSONparser, ErrorMsg, Filesystem, ReefIndex;


Procedure CreateReefRepo(RepoDirName : AnsiString);

implementation

// This function scans the pkgsrc files and creates reponame.repo
Procedure CreateReefRepo(RepoDirName : Ansistring);
var
  I : Integer;                  // Iterator
  Dirs : Array of Ansistring;   // Array of Directory Names (repodirs)
  IDXFile : String;             // Index File Name
begin
  IDXFile  := GetReefRootDir() + ReefIndexFileName;
end;



end.

