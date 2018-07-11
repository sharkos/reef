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
{ /lib/system/errormsg.pp                                                     }
{ Error Message & Exit Codes for Mokarra library and program                  }
{*****************************************************************************}

{$mode objfpc}{$H+}

unit ErrorMsg;

interface

uses Classes, SysUtils;

// Define Program Specific Error Message Types
Type EMokarraRuntime     = Class(Exception);  // Generic Runtime Error.
Type EMokarraFileNotFound= Class(Exception);  // Generic File Not Found
Type EReefIndexNotFound  = Class(Exception);  // Missing Index File ([ReefRoot]/reef.idx).
Type EReefIndexInvalid   = Class(Exception);  // Invalid Index File format.
Type EReefRootNotFound   = Class(Exception);  // Reef Root directory not found as configured
Type EReefCatDirNotFound = Class(Exception);  // Reef Category Directroy not found
Type EReefCatNotIndexed  = Class(Exception);  // Reef Category not indexed
Type EPkgSrcNotFound     = Class(Exception);  // pkgsrc file specified not found
Type EPkgSrcInvalid      = Class(Exception);  // PkgSrc file does not seem valid

procedure ErrorMessage( ExitCode : Integer);

implementation

// This procedure will create an error message & exception class to be returned via Raise call.
procedure ErrorMessage( ExitCode : Integer);
begin
      case ExitCode of
        // 1  -  9 : General Runtime
        1: Raise EMokarraRuntime.Create('a runtime error occurred.');
        2: Raise EMokarraFileNotFound.Create('File not found.');
        // 10 - 19 : REEF
        10: Raise EReefIndexNotFound.Create('Reef index file not found.');
        11: Raise EReefIndexInvalid.Create('Reef index does not appear valid.');
        12: Raise EReefRootNotFound.Create('Reef root directory not found.');
        13: Raise EReefCatDirNotFound.Create('Reef category directory not found.');
        14: Raise EReefCatNotIndexed.Create ('Specified category name does not exist in the master index.');
        // 20 - 29 : PKGSRC
        20: Raise EPkgSrcNotFound.Create ('Package source record file not found.');
        21: Raise EPkgSrcInvalid.Create ('Package source does not appear valid.');
        // 30 - 39
        // 40 - 49
        // 50 - 59
        else writeln ('An unknown/unspecified error occurred.');
      end;
  Abort; // If this is called and all else fails, Abort execution here.
end;

end.