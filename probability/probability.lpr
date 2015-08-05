program probability;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, MainUnit, LazOpenGLContext, URope, UClouds, ugame,
  utexture, ULines, UText, UStars;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

