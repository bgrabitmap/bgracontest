program SmileyTunnel;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  {$IFDEF USEHEAPTRC}
  heaptrc,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, SmileyTunnelMain
  { you can add units after this };

{$R *.res}

begin
  {$IF FPC_FULLVERSION > 30000}
  {$IFDEF USEHEAPTRC}
  GlobalSkipIfNoLeaks := True;
  {$ENDIF}
  {$ENDIF}
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

