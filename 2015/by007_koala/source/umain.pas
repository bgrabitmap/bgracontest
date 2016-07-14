unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  BGRABitmap, BGRABitmapTypes, ukoala;

const
  TimerInterval = 33; // 33 by default

type

  { TMainForm }

  TMainForm = class(TForm)
    Chronos: TTimer;
    procedure ChronosTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    Koala: TKoala;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  { Create Koala object with specified resolution }
  Koala := TKoala.Create(320, 240);

  { Time }
  Chronos.Interval := TimerInterval;
end;

procedure TMainForm.ChronosTimer(Sender: TObject);
begin
  { Compute the next frame to be later drawn }
  Koala.ComputeNextFrame;
  { Redraw }
  FormPaint(Self);

  if Koala.NextFrame = Koala.MaxFrameID then
    Chronos.Enabled := False;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  { Bye bye Koala }
  Koala.Free;
end;

procedure TMainForm.FormPaint(Sender: TObject);
begin
  { Stretch draw computed frame to client }
  Koala.Draw(Canvas, ClientRect);
  { Display NextFrame ID }
  // Canvas.TextOut(1, 1, IntToStr(Koala.NextFrame));
end;

end.

