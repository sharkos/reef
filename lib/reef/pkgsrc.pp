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
{ /lib/reef/pkgsrc.pp                                                         }
{ PkgSrc library                                                              }
{*****************************************************************************}
{$mode objfpc}{$H+}

unit PkgSrc;  // also known as REEF

interface

uses Classes, SysUtils, StrUtils, fpjson, jsonparser, ErrorMsg, typinfo;
// Filename constant
const
  PkgSrcIndexFileName = 'reef.idx';

type MPkgSrcArrayItem  = Array of Ansistring;

// PkgSrc file record defines the fields in the .pkgsrc for a Reef package
type MPkgSrc = Record
  name        : String;           // Package name
  version     : String;           // Software version
  revision    : Integer;          // Package revision
  arch        : MPkgSrcArrayItem; // Architecture this package builds for
  description : String;           // Description
  owner       : String;           // Package owner
  category    : String;           // Category
  builddeps   : MPkgSrcArrayItem; // Packages required to build (Should be an unitialized array)
  depends     : MPkgSrcArrayItem; // Packages required to install (Should be array)
  projecturl  : String;           // URL of the project main information site
  projectsrc  : String;           // URL of the project src download
  distfiles   : MPkgSrcArrayItem; // List of files required to build
end;

// Unit header declarations.
Function ParsePkgSrcFile(JSFile : String) : TJSONData;
Function IsPkgSrc( J : TJSONData ) : Boolean;
Function GetPkgSrcArrayItem( J : TJSONData ) : MPkgSrcArrayItem;
Function GetPkgSrcFilename(F : String) : String;
Function GetPkgSrcRecord(F : String) : MPkgSrc;

Function ConvertMPkgSrcArrayItemToJSON( P : MPkgSrcArrayItem ) : TJSONData;


implementation


// This function loads a JSON File are returns it as type TJSONData
Function ParsePkgSrcFile(JSFile : String) : TJSONData;
var
  F : TFileStream;
  P : TJSONParser;
  D : TJSONData;
begin
  if FileExists(JSFile) <> TRUE then   // Test file exists or is not 0
    begin
      ErrorMessage(2);                 // refer to unit ErrorMsg for code
    end
  else
    begin
      F:=TFileStream.Create(JSFile,fmopenRead);
      try
        P:=TJSONParser.Create(F);      // Create Parser Object from the file
      except
        Raise;
      end;
      F.Free;                          // Unload the file Handle
    end;
    // Now parse the data
    try
      D := P.Parse;             // Return a TJSONData object via Parse
    finally
      FreeAndNil(P);
    end;
      result := D;
end;


// Verify if first entry is "mokarra". Probably not the best way to validate...
Function IsPkgSrc( J : TJSONData ) : Boolean;
begin
  if TJSONObject(J).Names[0] <> 'pkgsrc' then result := FALSE else result := TRUE;
end;


// Get the categories Array - Iterates over the TJSONArray and Converts it to
// Type MPkgSrcArrayItem which is an Array of AnsiString
Function GetPkgSrcArrayItem( J : TJSONData ) : MPkgSrcArrayItem;
var
  I : Integer = 0;                 // Iterator
  A : MPkgSrcArrayItem;            // Dynamic Array for storing the categories
  K : AnsiString;                  // Key name from JSON Data (reefindex)
begin
  For I:=0 to J.Count-1 do
      begin
        K := J.Items[I].AsString; // Get key name @ iterator
      SetLength(A,I+1);               // Increase Dynamic array size to current iteartor count
      A[I] := K;                      // Add the keyname to the array @ iterator
  end; // end-for
  result := A;                        // return result intended for storage in MReefIndex.categories
end;


// Resolves the pkgsrc filename based on its pathname
Function GetPkgSrcFilename(F : AnsiString) : AnsiString;
begin
  if AnsiEndsStr('/',F) = FALSE then F := AddCharR('/',F,Length(F)+1);
  result := F+'pkgsrc';
end;


// Parses the contents of a pkgsrc file and returns it as an MPkgSrc Record
function GetPkgSrcRecord(F : String) : MPkgSrc;
var
  J : TJSONData;
//  JA: TJSONData;
  PkgSrcInfo : MPkgSrc;
begin
     J := ParsePkgSrcFile(F);
   Try
     If Assigned(J) then         // Check if JSON Parse was successful
       begin                     // Check if top level is 'mokarra'
         if IsPkgSrc(J) = FALSE then
           begin
             ErrorMessage(21); // EReefIndexInvalid
             Exit;
           end;
         // Drill down into path 'pkgsrc'
         J := TJSONObject(J).Find('pkgsrc');

         // Populate record from JSON object
         PkgSrcInfo.name        := TJSONObject(J).get('name');
         PkgSrcInfo.version     := TJSONObject(J).get('version');
         PkgSrcInfo.revision    := TJSONObject(J).get('revision');
         PkgSrcInfo.arch        := GetPkgSrcArrayItem(TJSONObject(J).find('arch'));
         PkgSrcInfo.description := TJSONObject(J).get('description');
         PkgSrcInfo.owner       := TJSONObject(J).get('owner');
         PkgSrcInfo.category    := TJSONObject(J).get('category');
         PkgSrcInfo.builddeps   := GetPkgSrcArrayItem(TJSONObject(J).find('build_deps'));
         PkgSrcInfo.depends     := GetPkgSrcArrayItem(TJSONObject(J).find('depends'));
         PkgSrcInfo.projecturl  := TJSONObject(J).get('project_url');
         PkgSrcInfo.projectsrc  := TJSONObject(J).get('project_src');
         PkgSrcInfo.distfiles   := GetPkgSrcArrayItem(TJSONObject(J).find('distfiles'));
       end;
   finally
     J.Destroy;
   end;
   result := PkgSrcInfo;
end;


// Converts type MPkgArrayItem (which is Array of Ansistring) into a JSON Data
Function ConvertMPkgSrcArrayItemToJSON( P : MPkgSrcArrayItem ) : TJSONData;
var
  I: Integer;
  K: AnsiString;
  J: TJSONArray;
begin
  J:=TJSONArray.Create([]);
  For I:=0 to (Length(P) - 1) do
      begin
        K := P[I]; // Get key name @ iterator
        J.Add(K); // Add K to the JSON Object
  end; // end-for
  result := J;                        // return result as TJSONData
end;


end. // unit PkgSrc
