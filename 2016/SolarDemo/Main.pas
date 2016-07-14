unit Main;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, Forms, Controls, Dialogs, LCLType, Graphics,
  nxGL, nxTypes, GraphicsUnit, nxMath;

type

  { TForm1 }

  TForm1 = class(TForm)
    AppProperties: TApplicationProperties;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure AppPropertiesIdle(Sender: TObject; var Done: Boolean);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure FormMouseWheel(Sender: TObject; {%H-}Shift: TShiftState; {%H-}WheelDelta: Integer; {%H-}MousePos: TPoint; var {%H-}Handled: Boolean);
  private
    game: TGraphicalGame;
  public
  end;

var Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  clientHeight:=screen.Height*4 div 5;
  clientWidth:=clientHeight*4 div 3;

  if not nx.CreateGlWindow(self) then begin
    showmessage('Failed to initialize OpenGL!');
  end;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  if (game=nil) and nx.AllOK then begin
    game:=TGraphicalGame.Create;
    if not game.Initialized then begin
      // Failed to initialize game
      FreeAndNil(game);
    end;
    onPaint:=nil; // No need to trigger this event again
  end;
end;

procedure TForm1.AppPropertiesIdle(Sender: TObject; var Done: Boolean);
begin
  if game<>nil then begin
    Done:=false;
    game.Idle;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if game<>nil then FreeAndNil(game);
  nx.KillGLWindow;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if game<>nil then
    with game do begin
      KeyDown(key, shift);
      case key of
        VK_SPACE: moving:=not moving;
        VK_ESCAPE: Close;
      end;
    end;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if game<>nil then
    with game do begin
      KeyUp(key, shift);

    end;
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if game<>nil then
    with game do begin
      MouseDown(button, shift);

    end;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if game<>nil then
    with game do begin
      MouseMove(x, y, shift);

    end;
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if game<>nil then
    with game do begin
      MouseUp(button, shift);

    end;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var delta: single;
begin
  if game<>nil then
    with game do begin
      delta:=wheeldelta/abs(wheeldelta);
      toZoom:=toZoom*(1-0.08/delta);
      toZoom:=minmax(toZoom, 0.05, 900);
    end;
end;

end.

