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
{ /lib/reef/reefindex.pp                                                      }
{ Reef Master Index & Reef Repo Index handlers                                }
{*****************************************************************************}
{$mode objfpc}{$H+}

unit ReefIndex;

interface

uses
  Classes, SysUtils, fpJSON, JSONparser, StrUtils,
  ConfigFile, ErrorMsg, Filesystem, TextUtils, Version, PkgSrc;

// TYPE: MCategories - used to store the parsed category names.
type MCategories = Array of AnsiString; // Dynamic Array

// TYPE: MCategories - used to store the parsed package names.
type MPackages = Array of AnsiString; // Dynamic Array

// RECORD: MReefPkgSrcSummary - Custom type that holds a PkgSrcSummary index record.
type MReefPkgSrcSummary = Record
  name         : AnsiString;
  version      : AnsiString;
  revision     : Integer;
  description  : Ansistring;
  arch         : Array of AnsiString;
end;

// RECORD: MReefCategory - Custom type that holds the category index record.
type MReefCategory = Record
  name : AnsiString;               // Category Name
  pkgsummary : Array of MReefPkgSrcSummary;    // Package Sources
end;

// RECORD: MReefIndex - Custom type that holds the master index record.
type MReefIndex = Record
  mokarraversion : Int64;               // Version of Mokarra
  reefversion    : AnsiString;          // Reef Revision
  categories     : Array of MReefCategory; // Category List
end;


const
  ReefIndexFileName = 'reef.idx';      // Index name constant

Function ParseReefIndexFile(JSFile : String) : TJSONData;
Function IsMokarraIndex( IDX : TJSONData ) : Boolean;
Function GetMokarraIndexVersion( IDX : TJSONData ) : Int64;
Function GetReefRootDir() : String;
Function GetReefCategoryDir(C : String) : String;
Function GetReefIndexVersion( IDX : TJSONData ) : AnsiString;
Function GetReefCategories( IDX : TJSONData ) : MCategories;
// BUGS: The following 2 functions require refactoring to correct changes in the Types
//Function CategoryExists(C : String) : BOOLEAN;
//Function ReadReefIndex() : MReefIndex;

Procedure DisplayReefCategories();
Procedure BuildReefIndex();
Function GenerateReefCategoryIndex(CatName : String) : TJSONData;


implementation

// This function loads a JSON File are returns it as type TJSONData
Function ParseReefIndexFile(JSFile : String) : TJSONData;
var
  F : TFileStream;
  P : TJSONParser;
begin
  if FileExists(JSFile) <> TRUE then   // Test file exists
    begin
      ErrorMessage(2);                 // refer to unit ErrorMsg for code
    end
  else
    begin
      F:=TFileStream.Create(JSFile,fmopenRead);
      try
        P:=TJSONParser.Create(F);      // Create Parser Object from the file
      finally
        F.Free;                        // Unload the file Handle
      end;
    end;
  result := P.Parse;                   // Return a TJSONData object via Parse
end;


// Verify if first entry is "mokarra". Probably not the best way to validate...
Function IsMokarraIndex( IDX : TJSONData ) : Boolean;
begin
  if TJSONObject(IDX).Names[0] <> 'mokarra' then result := FALSE else result := TRUE;
end;


// Retrives Mokarra Compat Version from index
Function GetMokarraIndexVersion( IDX : TJSONData ) : Int64;
begin
  result := TJSONObject(IDX).Find('version').AsInt64;
end;


// Returns the ReefRootDirectory (appends trailing / if needed)
Function GetReefRootDir() : String;
var
  cfg : MasterConfig;
  R : String; // Root Dir
begin
  cfg:=LoadMasterConfig();  // Load Configuration and set ReefRoot variable
  R := cfg.ReefRoot; // Set ReefRoot to configuration directory
  if AnsiEndsStr('/',R) = FALSE then R := AddCharR('/',R,Length(R)+1);
  result := R;
end;


// Returns the Reef Category Directory (appends trailing / if needed)
Function GetReefCategoryDir(C : String) : String;
var
  cfg : MasterConfig;
  R : String; // Root Dir
begin
  cfg:=LoadMasterConfig();  // Load Configuration and set ReefRoot variable
  R := cfg.ReefRoot; // Set ReefRoot to configuration directory
  if AnsiEndsStr('/',R) = FALSE then R := AddCharR('/',R,Length(R)+1);
  if AnsiEndsStr('/',C) = FALSE then C := AddCharR('/',C,Length(C)+1);
  result := R+C;
end;


// Retrieve the Reef Index Version from index
Function GetReefIndexVersion( IDX : TJSONData ) : String;
begin
  result := TJSONObject(IDX).Find('reefversion').AsString;
end;

// Get the categories Array
Function GetReefCategories( IDX : TJSONData ) : MCategories;
var
  I : Integer = 0;                 // Iterator
  A : MCategories;                 // Dynamic Array for storing the categories
  K : AnsiString;                  // Key name from JSON Data (reefindex)
begin
  For I:=0 to IDX.Count-1 do
      begin
      K := TJSONObject(IDX).Names[i]; // Get category key name @ iterator
      SetLength(A,I+1);               // Increase Dynamic array size to current iteartor count
      A[I] := K;                      // Add the keyname to the array @ iterator
  end; // end-for
  result := A;                        // return result intended for storage in MReefIndex.categories
end;

(* THIS FUNCTION IS BROKEN AND NEEDS TO BE REWORKED DUE TO CHANGES IN TYPES

// Returns TRUE/FALSE if named category 'C' exists in the master index
Function CategoryExists(C : String) : BOOLEAN ;
var
  ReefIDX : MReefIndex;
  I       : Integer;
  AL      : Integer;
  R       : BOOLEAN = FALSE;
begin
  // Retrieve the idex
  ReefIDX := ReadReefIndex();
  AL := Length(ReefIDX.categories);
  For I:=0 to (AL-1) do
    if ReefIDX.categories[I] = C then R:=TRUE;
  result := R;
end;
*)

// Load the Reef IDX file and return an index record
Function ReadReefIndex() : MReefIndex;
// TODO: Add exceptions and error checking

Var
  J : TJSONData;                       // J = JSON Dataset
  ReefIDX : MReefIndex;                // Index Dataset Record
  IDXFile : String = '/sharkos/reef/reef.idx';  // Set default filename
begin
    IDXFile := GetReefRootDir() + ReefIndexFileName;
    If FileExists(IDXFile) then
    begin
       Try
         // Get the TJSONData object from the the index file
         J:=ParseReefIndexFile(IDXFile);
         Try
           If Assigned(J) then         // Check if JSON Parse was successful
             begin                     // Check if top level is 'mokarra'
               if IsMokarraIndex(J) = FALSE then
                 ErrorMessage(11); // EReefIndexInvalid

               // Drill down into path 'mokarra'
             J := TJSONObject(J).Find('mokarra');
               // Fetch version information and set into result record
               ReefIDX.mokarraversion := GetMokarraIndexVersion(J);
               ReefIDX.reefversion    := TJSONObject(J).Find('reefversion').AsString;

             // Drill down into path 'reefindex' (categories)
             J := TJSONObject(J).Find('reefindex');
               // Fetch categories array
//              ReefIDX.categories     := GetReefCategories(J);
              end
           else // If JSON file not parsed correctly, then else
             Writeln('No JSON data available. Perhaps a bad or missing reef index file?');
         Finally
           FreeAndNil(J);
         end;
       except
         On E : Exception do
           begin
             Writeln('An Exception occurred when parsing : ',E.Message);
             Abort;
           end;
       end;
    end
    else
      ErrorMessage(10);
  // Return the Index to the caller
  result := ReefIDX;
end;


// Displays a formatted list of Reef Repositories
// TODO: This is not based on the index, only what is found in the FS. Unverified data
Procedure DisplayReefCategories();
var
  ReefDirs : Dirlist; // directory listing type
  AL : Integer = 0 ; //array length
  I : Integer;
begin
 ReefDirs := GetSubDirectories(GetReefRootDir());
 AL := Length(ReefDirs);
 writeln('Discovered Catgegories : (', AL, ') unverified');
 writeln('------------------------------------------------------------------');
 for I:=0 to AL do
   writeln(ReefDirs[I]);
end;


// This procedure scans dirs/files in cfg.PkgSrc and creates reef.idx JSON file
Procedure BuildReefIndex();
var
  I        : Integer;       // Iterator (of ReefDirs)
  AL       : Integer = 0;   // Array Length (of ReefDirs)
  ReefDirs : DirList;       // directory listing type
  IDXFile  : String;        // Index filename
  ReefIDX  : MReefIndex;    // Index Dataset Record
  IndexJSON: TJSONObject;   // JSON Object for index file output
  CatName  : String;        // Category name resolved from pathname
  CatJSON  : TJSONObject;   // JSON Object for storing category
begin
  writeln('Starting CreateReefIndex...');
//  ReefRoot := GetReefRootDir(); // Set ReefRoot to configuration directory
  IDXFile  := GetReefRootDir() + ReefIndexFileName;

  // TODO Backup Existing File for rollback if something fails
  if FileExists(IDXFile) = TRUE then
    begin
      // Read the old idx file and rename the file with the reefversion appended.
      ReefIDX := ReadReefIndex();
      write('Backup  v', ReefIDX.reefversion,': ');
      RenameFile(IDXFile,IDXFile+'.'+ReefIDX.reefversion);
      writeln('[OK]');
    end;

  // Check for the existence of the ReefRoot directory first and foremost.
  if DirectoryExists(GetReefRootDir()) <> TRUE then
    begin
      ErrorMessage(12); //
      Abort;
    end
  else
    begin
      // Start directory scans
      //ReefIDX.categories :=
      ReefDirs := GetSubDirectories(GetReefRootDir());
      AL := Length(ReefDirs);
      // Create a blank object to store the Category Data
      CatJSON:=TJSONObject.create([]);
      for I:=0 to (AL-1) do
        begin
          // Resolve the category name from path
          CatName:=StripParentDir(ReefDirs[I]);
          // Generate the current Category's pkgsrc index into a JSON Array
          CatJSON.add(CatName,GenerateReefCategoryIndex(CatName));
        end; // End 'for I'
        // Set Mokarra Version
        ReefIDX.mokarraversion:=MOKARRA_RELEASE;
        // Set Reef Version from Timestamp
        ReefIDX.reefversion:=CurrentTimeStamp;
        // Create the final JSON object
        IndexJSON := TJSONObject.Create(['mokarra',
                       TJSONObject.Create(['version', ReefIDX.mokarraversion,
                                           'reefversion',CurrentTimeStamp,
                                           'reefindex', CatJSON])]);
        write('Saving  v', ReefIDX.reefversion, ': ');
        SaveStringToFile(IDXFile, IndexJSON.asJSON);
        WriteLn('[OK]');
    end;

    // Cleanup
    // BUG: For some reason, issuing a .destroy to these causes an exception fault
    CatJSON.clear;
    IndexJSON.clear;
    // remove any backup files?
end; // CreateReefIndex


// Creates the index data for the specified category name
Function GenerateReefCategoryIndex(CatName : String) : TJSONData;
var
  I          : Integer;          // Iterator (of SubDirs)
  L          : Integer = 0;      // Array Length (of ScanDirs)
  CatDir     : String;           // Resolved Category Directory name
  ScanDirs   : DirList;          // Subdirectories to scan
  PkgArray   : TJSONArray;       // JSON Array for parsing package object
  PkgRecord  : MPkgSrc;          // Pkg Record retrieved from pkgsrc file
  PO         : TJSONObject;      // Pkg Object to be assembled from PkgRecord
  PF         : String;           // PackageSrc File
begin
  // Resolve the category path
  CatDir := GetReefCategoryDir(CatName);

  // Check for the existence of the Category directory first and foremost.
  if DirectoryExists(CatDir) <> TRUE then
    begin
      ErrorMessage(13); //
      Abort;
    end
  else
    begin
      writeln('Scanning: ',CatDir);
      // Step 1: Get the directory listing for the category
      ScanDirs := GetSubDirectories(CatDir);
      L := Length(ScanDirs);
      PkgArray:=TJSONArray.create([]); // Create a blank package array
      // Process Package dirs
        for I:=0 to (L-1) do
          begin
            // Get the package record from a resolved pkgsrc filename
            PF := GetPkgSrcFileName(ScanDirs[I]);
            writeln((I+1),'/',L,': Processing ', PF);
            PkgRecord := GetPkgSrcRecord(PF);
            PO:=TJSONObject.Create(['description', PkgRecord.description,
                                    'version',     PkgRecord.version,
                                    'revision',    PkgRecord.revision,
                                    'arch', ConvertMPkgSrcArrayItemToJSON(PkgRecord.arch)
                                    ]);
            PO:=TJSONObject.Create([PkgRecord.name,PO]);
            //writeln(PO.AsJSON);
            PkgArray.add(PO);
          end; // End 'for I'
    end;
    result:=PkgArray;
end;


// unit end
end.
