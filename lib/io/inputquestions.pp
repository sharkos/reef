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
{ /lib/io/inputquestions.pas                                                  }
{ Library file for getting input from users                                   }
{*****************************************************************************}

unit InputQuestions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Crt;

function InputSpinKey : CHAR;
function InputSpinStr : STRING;
function InputSpinLimit(Limit : INTEGER) : STRING;
function AskYN(Question: STRING) : BOOLEAN;
function AskString(Question: STRING; Limit: INTEGER) : STRING;
function ReadIn(Len : BYTE) : STRING;

implementation

Const
  SpinChar : Array [1..4] of Char = ('-','/','|','\');

function InputSpinKey : CHAR;
Var
  X,Y  : Byte;
  Num  : Byte;
  Ch   : Char;
  Wait : Byte = 40;
begin
  Num := 1;                               (* initialize SpinChars  *)
  X   := WhereX;                          (* Where am I ??         *)
  Y   := WhereY;
  Repeat
    Write(SpinChar[Num]);           (* Spin the Cursor       *)
    GotoXY(X, Y);                   (* Go back               *)
    Delay(Wait);                    (* Wait, it's to fast!   *)
    Write(#32);                     (* Clean Screen          *)
    GotoXY(X, Y);                   (* Go back               *)
    Inc(Num);                       (* Next SpinChar, please *)
    if Num = 5 then Num := 1;       (* I have only 5 Chars   *)
  Until KeyPressed;
  Ch := ReadKey;                        (* Get the pressed Key   *)
  Write(Ch);                            (* and Write it to screen*)
  InputSpinKey := Ch;                    (* give a result         *)
end;

function InputSpinStr : STRING;
Var
  Concat : String = '';
  Ch   : Char;
begin
  Repeat
    Ch := InputSpinKey;
    if Ch = #8 then Ch := #0; // Ignore Backspace
    if Ch <> #13 then Concat := Concat + Ch;
  Until Ch = #13;
  InputSpinStr := Concat;
  WriteLn;
end;

{* Input a string of limited length *}
function InputSpinLimit(Limit : INTEGER) : STRING;
Var
  Concat : String = '';
  Ch   : Char;
  Count: Integer = 0;
begin
  Repeat
    Ch := InputSpinKey;
    // Test for [backspace]
    if Ch = #8 then
      begin
       Ch := #0; // set Ch to [null] for safety
       // Decrease limitcounter by 1 on backspace
       if Count >= 0 then // Make sure we are not going negative
         begin
          Delete(Concat, Count, 1); // Delete the last character in the set
          Count := Count -1;
         end;
      end
    else
      begin
        // test if not [enter]
        if Ch <> #13 then
        begin
          Concat := Concat + Ch; // Add the new character to the string array
          Count := Count +1;
        end;
        if Ch = #13 then Count := Limit;
      end;
  Until Count >= Limit;
  InputSpinLimit := Concat;
  WriteLn;
end;

{* Asks a YES/NO question returning TRUE/FALSE *}
function AskYN(Question: STRING) : BOOLEAN;
  var
    Ch : Char;
    IsValid : Integer = 0;
    Xc : Integer; // Screen coordinate
    Yc : Integer; // Screen coordinate
begin
 While isValid < 1 do
   begin
     // Print Question
     write(Question+' [Y/N]: ');
     // Create Input Box
     Xc := WhereX;
     Yc := WhereY;
     TextColor(White);
     TextBackground(Blue);
     write(' ');
     GotoXY(Xc,Yc);
     // Get value
     Ch := InputSpinKey;
     TextBackground(Black);
     case Ch of
       'Y', 'y' :
         begin
           isValid := 1;
           Exit(TRUE);
         end;
       'N', 'n' :
         begin
           isValid := 1;
           Exit(FALSE);
         end;
       else
         writeln('Invalid Key');
     end;

   end;
end;

{* Ask a question returning String *}
function AskString(Question: STRING; Limit: INTEGER) : STRING;
  var
    Reply : String;  // Answer
    L : Integer = 0; // Length
    Xc : Integer; // Screen coordinate
    Yc : Integer; // Screen coordinate
begin
    writeln(Question);
    Xc := WhereX;
    Yc := WhereY;
    TextColor(White);
    TextBackground(Blue);
    Repeat
      write(' ');
      L := L +1;
    Until L >= Limit;
    GotoXY(Xc,Yc);
    Reply := InputSpinLimit(Limit);
    //Reply := InputSpinStr;
    TextBackground(Black);
    AskString := Reply;
end;

{* Read String of Limited Length *}
Function ReadIn(Len : BYTE) : STRING;
  var
    Inkey   : Char;
    InString: String[255] ='';
Begin
  Repeat
    Inkey:=ReadKey;
    If (Inkey=#8) and (Length(InString)>0) then
      Begin
        Dec(InString[0]);
        Write(#8#32#8);
      End;
     If (Inkey<>#13) and (Inkey<>#8) then
       If Length(InString)<Len  then
       Begin
         InString:=InString+InKey;
         Write(InKey);
       End
     Else
       Begin
         Write(#7);
         Instring[Length(Instring)]:=Inkey;
         Write(#8,Inkey);
       End;
  Until Inkey=#13;
  WriteLn;
  ReadIn:=InString;
End;


end.
