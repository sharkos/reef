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
{   Author: Chris Tusa <chris.tusa@leafscale.com                              }
{                                                                             }
{ This software is protected under the LeafScale Software License.            }
{                                                                             }
{*****************************************************************************}
{ /lib/config/configmenu.pas                                                  }
{ Library file for the main configuration menu system                         }
{*****************************************************************************}

unit ConfigMenu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ConfigFile, Menu, TextUtils, InputQuestions, SystemInfo, ReefIndex;

//procedure ConfigMenuMain(cfg: MasterConfig);
procedure ConfigMenuMain();
procedure ConfigMenuPackageManager(cfg: MasterConfig);
procedure ConfigMenuPackageSources(cfg: MasterConfig);
procedure ConfigMenuSystemInfo();

implementation

//procedure ConfigMenuMain(cfg: MasterConfig);
procedure ConfigMenuMain();
var
 Cfg : MasterConfig;
 M : MMenu;
 selection : CHAR;
 loop : BOOLEAN = TRUE;
begin
 M.name := 'Mokarra Config - Main Menu';
 M.items[1].key   := '1';
 M.items[1].descr := 'Package Manager';
 M.items[1].value := ' ';
 M.items[2].key   := '2';
 M.items[2].descr := 'Reef Configuration';
 M.items[2].value := ' ';
 M.items[3].key   := '3';
 M.items[3].descr := 'System Information';
 M.items[3].value := ' ';

 M.items[20].key := 'X';
 M.items[20].descr := 'Exit';
 while loop = TRUE do
   begin
     // Load config after each menu to ensure we have a current copy
     cfg:=LoadMasterConfig();
     selection := DisplayMenu(M);
     case selection of
       'X': Exit;
       '1': ConfigMenuPackageManager(cfg);
       '2': ConfigMenuPackageSources(cfg);
       '3': ConfigMenuSystemInfo();
     end;
   end;
end;

procedure ConfigMenuPackageManager(cfg: MasterConfig);
var
 M : MMenu;
 selection : CHAR;
 loop : BOOLEAN = TRUE;
begin
     M.name := 'Package Manager Settings';
     M.items[1].key := '1';
     M.items[1].descr := 'Mirror Site:  ';
     M.items[1].value := cfg.PkgMirrorURI;

     M.items[2].key := '2';
     M.items[2].descr := 'Require SSL:  ';
     M.items[2].value := Bool2Str(cfg.PkgMirrorSSL);

     M.items[20].key := 'X';
     M.items[20].descr := 'Return to Main';

 while loop = TRUE do
   begin
     selection := DisplayMenu(M);
     writeln();
     case selection of
       'X':
         begin
           SaveMasterConfig(cfg);
           Exit;
         end;
       '1':
         begin
           cfg.PkgMirrorURI := AskString('Enter a Mokarra Server URI:',60);
         end;
       '2':
         begin
           cfg.PkgMirrorSSL := AskYN('Does the server require SSL?');
         end;
       (*
       '3':
         begin
           cfg.ServerPort:= Str2Int(AskString('Enter the TCP Server Port:',5));
         end;
         *)
     end;
   end;
end;

procedure ConfigMenuPackageSources(cfg: MasterConfig);
var
 M : MMenu;
 selection : CHAR;
 loop : BOOLEAN = TRUE;
begin
    M.name := 'REEF: Package Source Settings';
    M.items[1].key := '1';
    M.items[1].descr := 'REEF Root :  ';
    M.items[1].value := cfg.ReefRoot;

    M.items[2].key := '2';
    M.items[2].descr := 'Build dir :  ';
    M.items[2].value := cfg.ReefBuild;

    M.items[3].key := '3';
    M.items[3].descr := 'Binary dir:  ';
    M.items[3].value := cfg.ReefBinary;

    M.items[10].key := 'B';
    M.items[10].descr := 'Browse Package Sources';

    M.items[11].key := 'R';
    M.items[11].descr := 'Rebuild REEF Index';

    M.items[20].key := 'X';
    M.items[20].descr := 'Return to Main';


 while loop = TRUE do
   begin

     selection := DisplayMenu(M);
     writeln();
     case selection of
       'X':
         begin
           SaveMasterConfig(cfg);
           Exit;
         end;
       'B':
         begin
           writeln('Browse feature is not implemented. Press a key to continue');
           InputSpinKey();
         end;
       'R':
         begin
           BuildReefIndex();
           InputSpinKey();
         end;
       '1':
         begin
           cfg.ReefRoot := AskString('Enter top level dir for reef \n (ex: /sharkos/reef ) :',40);
         end;
       '2':
         begin
           cfg.ReefBuild := AskString('Enter build temp dir: ',40);
         end;
       '3':
         begin
           cfg.ReefBinary := AskString('Enter pkg output dir: ',40);
         end;
     end;
   end;
end;



procedure ConfigMenuSystemInfo();
begin
 SystemDisplayInfo();
 write ('-- press space to continue --');
 InputSpinKey();
end;

//END
end.

