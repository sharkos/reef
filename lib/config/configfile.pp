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
{ /lib/config/configfile.pas                                                  }
{ Library file for the handling INI style configuration file                  }
{*****************************************************************************}

unit ConfigFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseUnix, InputQuestions, INIfiles;

const
  MasterConfigFilename = '/etc/mokarra/mokarra.conf';
  MTimeStampFormat = 'YYYYMMDDhhnn';

// Defines the master configuration file record
type MasterConfig = RECORD
       // [PKG]
       PkgMirrorURI     : STRING;   // Mirror Site for binary packages
       PkgMirrorSSL     : BOOLEAN;  // Mirror Site uses SSL?
       // [REEF]
       ReefRoot         : STRING; // Package Source ROOT (reef)
       ReefBuild        : STRING;  // Package Build Root - where packages are built
       ReefBinary       : STRING;  // Binary Package output directory
end;

var
  INI: TCustomINIFile;

function LoadMasterConfig() : MasterConfig;
function SaveMasterConfig(ConfigData: MasterConfig) : BOOLEAN;
function InitMasterConfig() : MasterConfig;

implementation

{* Load Master Configuration from a file *}
function LoadMasterConfig() : MasterConfig;
  var
    info : stat;
    cfg  : MasterConfig;
    INI  : TINIFile;
begin
  // Test if file exists
  if fpstat(MasterConfigFilename,info) = 0 then
    begin
      INI := TINIFile.Create(MasterConfigFilename);
      // [PKG]
      cfg.PkgMirrorURI := INI.ReadString('PKG','mirror_uri','');
      cfg.PkgMirrorSSL := INI.ReadBool('PKG','use_ssl',TRUE);
      // [REEF]
      cfg.ReefRoot       := INI.ReadString('REEF','root','');
      cfg.ReefBuild     := INI.ReadString('REEF','build','');
      cfg.ReefBinary    := INI.ReadString('REEF','binary','');
    end
  else
    begin
      if AskYN('Unable to locate configuration file, create one? [Y/N]: ') = TRUE then
        cfg := InitMasterConfig()
      else
        Halt; // Abort Program here. (unclean? probably not - Assumes OS cleaned)
    end;
    Result := cfg;
end;

{* Save Master Configuration to file - returns TRUE or FALSE *}
function SaveMasterConfig(ConfigData: MasterConfig) : BOOLEAN;
  var
    INI  : TINIFile;
begin
         INI := TINIFile.Create(MasterConfigFilename);
         // [PKG]
         INI.WriteString('PKG','mirror_uri', ConfigData.PkgMirrorURI);
         INI.WriteBool('PKG','use_ssl', ConfigData.PkgMirrorSSL);
         // [REEF]
         INI.WriteString('REEF','root', ConfigData.ReefRoot);
         INI.WriteString('REEF','build', ConfigData.ReefBuild);
         INI.WriteString('REEF','binary', ConfigData.ReefBinary);

         Result:=TRUE;
end;

{* Initialize an empty configuration with Defaults *}
function InitMasterConfig() : MasterConfig;
  var
    cfg  : MasterConfig;
begin
// Defaults
  // [PKG]
  cfg.PkgMirrorURI    := 'https://sources.leafscale.org/sharkos/binary';
  cfg.PkgMirrorSSL    := TRUE;
  // [REEF}
  cfg.ReefRoot        := '/sharkos/reef';
  cfg.ReefBuild       := '/sharkos/buildroot';
  cfg.ReefBinary      := '/sharkos/packages';

  if SaveMasterConfig(cfg) = TRUE then
    writeln('Successfully created default configuration...')
  else
  begin
    writeln('!! Unable to create default configuration !!');
    Exit;
  end;
  Result := cfg;
end;

end.

