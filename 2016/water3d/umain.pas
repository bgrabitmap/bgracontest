unit umain;
{ Water3D demo by circular }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  BGLVirtualScreen, BGRAOpenGL, UWaterScene;

type

  { TForm1 }

  TForm1 = class(TForm)
    BGLVirtualScreen1: TBGLVirtualScreen;
    Timer1: TTimer;
    procedure BGLVirtualScreen1Elapse(Sender: TObject; BGLContext: TBGLContext;
      ElapsedMs: integer);
    procedure BGLVirtualScreen1LoadTextures(Sender: TObject;
      BGLContext: TBGLContext);
    procedure BGLVirtualScreen1Redraw(Sender: TObject; BGLContext: TBGLContext);
    procedure BGLVirtualScreen1UnloadTextures(Sender: TObject;
      BGLContext: TBGLContext);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    scene: TWaterScene3D;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.BGLVirtualScreen1LoadTextures(Sender: TObject;
  BGLContext: TBGLContext);
begin
  scene := TWaterScene3D.Create;
end;

procedure TForm1.BGLVirtualScreen1Elapse(Sender: TObject;
  BGLContext: TBGLContext; ElapsedMs: integer);
begin
  scene.Elapse(ElapsedMs);
end;

procedure TForm1.BGLVirtualScreen1Redraw(Sender: TObject;
  BGLContext: TBGLContext);
begin
  if Assigned(scene) then
    scene.RenderGL(BGLContext.Canvas);
end;

procedure TForm1.BGLVirtualScreen1UnloadTextures(Sender: TObject;
  BGLContext: TBGLContext);
begin
  FreeAndNil(scene);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if Assigned(scene) then BGLVirtualScreen1.Invalidate;
end;

end.

