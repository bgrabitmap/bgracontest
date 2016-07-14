unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  BGRAVirtualScreen, BGRABitmap, BGRABitmapTypes, BCTypes, Math;

type

  { TForm1 }

  TForm1 = class(TForm)
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    Timer1: TTimer;
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    points: array [0..255] of TPoint;
    current: integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  i, j: integer;
begin
  j := 0;
  if current > 1 then
  begin
    for i := current-1 downto 1 do
    begin
      Inc(j);
      Bitmap.DrawLineAntialias(points[i].x, points[i].y, points[i-1].x, points[i-1].y, BGRA(0, 0, RandomRange(0,256), 255-j), 5, False);
    end;
  end;

  for i := Length(points)-1 downto current+1 do
  begin
    Inc(j);
    Bitmap.DrawLineAntialias(points[i].x, points[i].y, points[i-1].x, points[i-1].y, BGRA(0, 0, RandomRange(0,256), 255-j), 5, False);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
  p: TPoint;
begin
  current := 0;
  p := ScreenToClient(Mouse.CursorPos);
  // Initialize array
  for i := 0 to Length(points)-1 do
  begin
    points[i] := Point(p.x, p.y);
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  p: TPoint;
begin
  if current > Length(points)-1 then
  begin
    current := 0;
  end;
  p := ScreenToClient(Mouse.CursorPos);
  points[current] := Point(p.x, p.y);
  BGRAVirtualScreen1.DiscardBitmap;
  Inc(current);
end;

end.

