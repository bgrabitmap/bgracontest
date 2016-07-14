{
 *****************************************************************************
  This file is part of ShakingPix
  Copyright (c) 2016 by Michael W Vogel

  See the file COPYING.LGPL.txt, included in this distribution,
  for details about the license.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 *****************************************************************************

  ShakingPix - next picture slideshow, by Michael W Vogel, forum alias Michl

  A cross platform sliedeshow, made with
  FreePascal: http://www.freepascal.org/
  Lazarus: http://www.lazarus-ide.org/
  BGRABitmap (package BGLControls): https://github.com/bgrabitmap/lazpaint/tree/master/bgrabitmap
  for the Graphics Contest 2016 http://forum.lazarus.freepascal.org/index.php/topic,32626

  To compile ShakingPix, you need at least:
  Lazarus 1.6, BGRABitmap 8.9.2
  Linux OpenGL libraries, see: https://en.wikibooks.org/wiki/OpenGL_Programming/Installation/Linux

  Steps to compile the project:
  - Start Lazarus
  - Before you open the project, you have to install package LazOpenGLContext 0.0.1 (from
    Lazarus repository) and BGLControls from BGRABitmap
  - Open the project and compile it, hope it work for you ;)

 *****************************************************************************
}

{set debugger option: --eval-command="set auto-solib-add off"}

program ShakingPix;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF USEHEAPTRC}
  heaptrc,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uMain, uImageLoader, uImagesMover
  { you can add units after this };

{$R *.res}

begin
  {$IF FPC_FULLVERSION > 30000}
  {$IFDEF USEHEAPTRC}
  GlobalSkipIfNoLeaks := True;
  {$ENDIF}
  {$ENDIF}
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

