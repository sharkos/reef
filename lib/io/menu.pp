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
{ /lib/io/menu.pas                                                            }
{ Library file for rendering standarized menus                                }
{*****************************************************************************}

unit Menu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Crt, InputQuestions;

type MItem = RECORD
     key  : Char;   // Character Key for input
     descr: String; // Description Field
     value: String; // Current Value
end;

type MMenu = RECORD
     name : STRING; // Name for this menu (Used as Header Text)
     items: Array[1..20] of MItem; // Menu Items for Display (max 20)
end;

procedure PrintMenuItem(Key: CHAR; Descr, Value: STRING; Spaces : WORD);
function DisplayMenu(M: MMenu) : CHAR;

implementation

{* Ouput a standard/color Menu
     Key   = Input character submitted by user
     Value = Description for Item Text
     Spaces = Number of Spaces to use at end of line (for formatting)
*}
procedure PrintMenuItem(Key: CHAR; Descr, Value: STRING; Spaces : WORD);
const
  KeyColor = LightBlue;
  KeySepColor = Blue;
  KeySep = ')';
  DescrColor = White;
  ValueColor = LightCyan;
var
 count : Word = 0;

begin
 TextColor(KeyColor);
 write(' ',Key);
 TextColor(KeySepColor);
 write(KeySep, ' ');
 TextColor(DescrColor);
 write(Descr);
 if Value <> '' then
   begin
     TextColor(ValueColor);
     write(Value);
   end;
 TextColor(LightGray);

 if spaces = 0 then
   writeln()
 else
   begin
     while count < Spaces do
     begin
       write(' ');
       count := count + 1;
     end;
   end;
end;

{* Display a menu using Type MMenu, then ask for item selection *}
function DisplayMenu(M: MMenu) : CHAR;
  var
  i : INTEGER;
  validkeys : Set of CHAR = [];
  selection : CHAR;
  loop      : BOOLEAN = TRUE;
  posX   : WORD;
  posY   : WORD;
begin
  clrscr;
  TextColor(Blue);
  write('[');
  TextColor(Yellow);
  write(M.name);
  TextColor(Blue);
  writeln(']');
  writeln();

  for i:=Low(M.items) to high(M.items) do
    begin
      if M.items[i].descr <> '' then
        begin
          PrintMenuItem(M.items[i].key, M.items[i].descr, M.items[i].value, 0);
          Include(validkeys, M.items[i].key);
        end;
    end;
    writeln();
    TextColor(LightGray);
    write('Select: ');
    posX := WhereX;
    posY := WhereY;
    TextBackground(Blue);
    write(' ');
    GotoXY(posX, posY);
   while loop = TRUE do
     begin
       selection := InputSpinKey;
       TextBackground(Black);
       if UpCase(selection) in validkeys then
         begin
           loop := FALSE;
           Exit(Upcase(selection));
         end
       else
         TextColor(Red);
         write('  Invalid Selection');
         GotoXY(posX, posY);
         TextBackground(Blue);
         TextColor(White);
         write(' ');
         GotoXY(posX, posY);
     end;
end;

end.

