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
{ /lib/io/ansicrt.pp                                                          }
{ Library file for ANSI colorization of the TTY                               }
{*****************************************************************************}

unit AnsiCrt;

{$mode objfpc}{$H+}


interface

Const
  Blk = #27#91#52#48#109;
  Blu = #27#91#52#52#109;

  { Foreground and background color Constants }
  Black         = 0;
  Blue          = 1;
  Green         = 2;
  Cyan          = 3;
  Red           = 4;
  Magenta       = 5;
  Brown         = 6;
  LightGray     = 7;

  { Foreground color Constants }
  DarkGray      = 8;
  LightBlue     = 9;
  LightGreen    = 10;
  LightCyan     = 11;
  LightRed      = 12;
  LightMagenta  = 13;
  Yellow        = 14;
  White         = 15;

  { Add-in For blinking }
  Blink         = 128;


  procedure TextBackGround(Color : Byte);
  procedure TextColor(Color : Byte);

implementation

uses
  Classes, SysUtils;


Procedure TextBackGround(Color : Byte);
begin
 Case color of
   0 : Write(#27#91#52#48#109);
   1 : Write(#27#91#52#52#109);
   2 : Write(#27#91#52#50#109);
   3 : Write(#27#91#52#54#109);
   4 : Write(#27#91#52#49#109);
   5 : Write(#27#91#52#53#109);
   6 : Write(#27#91#52#51#109);
   7 : Write(#27#91#52#55#109);
  end;
end;

Procedure TextColor(Color : Byte);
 begin
  Case color of
     0 : Write(#27#91#51#48#109);
     1 : Write(#27#91#51#52#109);
     2 : Write(#27#91#51#50#109);
     3 : Write(#27#91#51#54#109);
     4 : Write(#27#91#51#49#109);
     5 : Write(#27#91#51#53#109);
     6 : Write(#27#91#51#51#109);
     7 : Write(#27#91#51#55#109);
     8 : Write(#27#91#49#59#51#48#109);
     9 : Write(#27#91#49#59#51#52#109);
    10 : Write(#27#91#49#59#51#50#109);
    11 : Write(#27#91#49#59#51#54#109);
    12 : Write(#27#91#49#59#51#49#109);
    13 : Write(#27#91#49#59#51#53#109);
    14 : Write(#27#91#49#59#51#51#109);
    15 : Write(#27#91#49#59#51#55#109);
   128 : Write(#27#91#53#109);
  end;
end;

Procedure NormVideo;
begin
  Write(#27#91#48#109);
end;


end.

