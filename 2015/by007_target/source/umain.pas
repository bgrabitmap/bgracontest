unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  utarget, LCLType;

const
  TimerInterval = 33; // 33 by default

type

  { TMainForm }

  TMainForm = class(TForm)
    Chronos: TTimer;
    procedure ChronosTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
  private
    Target: TTarget;
    SaveFolder: string;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  { Create Target object with specified resolution }
  Target := TTarget.Create(Width, Height);

  SaveFolder := ExtractFilePath(ParamStr(0)) + 'out\';
  CreateDir(SaveFolder);

  { Time }
  Chronos.Interval := TimerInterval;
end;

procedure TMainForm.ChronosTimer(Sender: TObject);
var
  p: TPoint;
begin
  p := ScreenToClient(Mouse.CursorPos);
  Target.MousePos := p;

  { Compute the next frame to be later drawn }
  Target.ComputeNextFrame;

  { Save }
  //Target.SaveFrame(SaveFolder);

  { Redraw }
  FormPaint(Self);

  if Target.NextFrame = Target.MaxFrameID then
    Chronos.Enabled := False;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  { Bye bye Target }
  Target.Free;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Application.Terminate;
end;

procedure TMainForm.FormPaint(Sender: TObject);
begin
  { Stretch draw computed frame to client }
  Target.Draw(Canvas, ClientRect);
  { Display NextFrame ID }
  //Canvas.TextOut(1, 1, IntToStr(Target.NextFrame));
end;

end.


