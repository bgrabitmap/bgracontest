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
}

unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, Spin, StdCtrls, uImageLoader, uImagesMover,
  BGRAOpenGL, BGLVirtualScreen, BGRABitmapTypes,
  uDefinitions;

type

  { TForm1 }

  TForm1 = class(TForm)
    BGLVirtualScreen1: uImageLoader.TBGLVirtualScreen;
    Label1: TLabel;
    Label2: TLabel;
    LoadingTimer: TIdleTimer;
    Panel1: TPanel;
    RepaintTimer: TTimer;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    SpinEdit1: TSpinEdit;
    procedure BGLVirtualScreen1DblClick(Sender: TObject);
    procedure BGLVirtualScreen1Elapse(Sender: TObject; BGLContext: TBGLContext; ElapsedMs: integer);
    procedure BGLVirtualScreen1LoadTextures(Sender: TObject; BGLContext: TBGLContext);
    procedure BGLVirtualScreen1MouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure BGLVirtualScreen1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BGLVirtualScreen1Redraw(Sender: TObject; Ctx: TBGLContext);
    procedure BGLVirtualScreen1UnloadTextures(Sender: TObject; BGLContext: TBGLContext);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LoadingTimerTimer(Sender: TObject);
    procedure RepaintTimerTimer(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
  private
    Initialized: Boolean;
    FullScreen: Boolean;
    CurrentSpriteCount: Integer;
    InfoText: IBGLRenderedFont;
    SpeedArray: array[0..99] of Integer;
    Speed: Integer;
    ImagesMover: TMovedImages;
    OldBounds: TMBounds;
    procedure SetFullScreen(AValue: Boolean);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Position := poScreenCenter;
  ImagesMover := TMovedImages.Create;
  ImagesMover.Count := 10;
  Images.GLScreen := BGLVirtualScreen1;
  FillByte(SpeedArray[0], Length(SpeedArray), 0);
  Randomize;
  Initialized := False;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ImagesMover.Free;
end;

procedure TForm1.FormMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  Images.ZoomPoint := ClientToModel(Point(X, Y), ClientWidth, ClientHeight);
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  BGLVirtualScreen1.Repaint;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  Images.MBounds := MBounds(0, 0, ClientWidth, ClientHeight);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  if Initialized then Exit;
  OldBounds.Left        := Left;
  OldBounds.Top         := Top;
  OldBounds.Width       := Width;
  OldBounds.Height      := Height;
  OldBounds.WindowState := wsNormal;
  FormResize(nil);
  AppState := asRunning;
  Images.LoadFromDir(BasePath);
  RepaintTimer.Enabled := True;
  LoadingTimer.Enabled := True;
  FullScreen := False;
  Initialized := True;
end;

procedure TForm1.LoadingTimerTimer(Sender: TObject);
begin
  if ThreadCnt < MaxImageLoadThreads then Images.LoadNeededImage;
end;

procedure TForm1.RepaintTimerTimer(Sender: TObject);
begin
  ImagesMover.Count := ImagesMover.Count + 1;
  ImagesMover.Invalidate;
  Repaint;
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
  MaxBitmapSize := SpinEdit1.Value;
end;

procedure TForm1.SetFullScreen(AValue: Boolean);
begin
  FullScreen := AValue;
  ImagesMover.Clear;
  if FullScreen then
  begin
    OldBounds.Left        := Left;
    OldBounds.Top         := Top;
    OldBounds.Width       := Width;
    OldBounds.Height      := Height;
    OldBounds.WindowState := WindowState;
    BorderStyle := bsNone;
    WindowState := wsFullScreen;
  end
  else
  begin
    WindowState := OldBounds.WindowState;
    BorderStyle := bsSizeable;
    Application.ProcessMessages;
    SetBounds(OldBounds.Left, OldBounds.Top, OldBounds.Width, OldBounds.Height);
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  AppState := asDestroying;
  BGLVirtualScreen1.UnloadTextures;
  LoadingTimer.Enabled := False;
  RepaintTimer.Enabled := False;
end;

procedure TForm1.BGLVirtualScreen1DblClick(Sender: TObject);
begin
  SetFullScreen(not FullScreen);
end;

procedure TForm1.BGLVirtualScreen1Elapse(Sender: TObject;
  BGLContext: TBGLContext; ElapsedMs: integer);
var
  i: Integer;
begin
  Move(SpeedArray[0], SpeedArray[1], SizeOf(Integer) * High(SpeedArray));
  SpeedArray[0] := ElapsedMs;
  Speed := 0;
  for i := 0 to High(SpeedArray) do
    Speed := Speed + SpeedArray[i];
  Speed := Speed div Length(SpeedArray);
  if (Speed < 38) and (CurrentSpriteCount + 10 > MaxImages) then Inc(MaxImages);
  if (Speed > 44) and (MaxImages > 10) and (CurrentSpriteCount - 10 < MaxImages) then Dec(MaxImages);
end;

procedure TForm1.BGLVirtualScreen1LoadTextures(
  Sender: TObject; BGLContext: TBGLContext);
begin
  Infotext := BGLFont('Arial', 16);
end;

procedure TForm1.BGLVirtualScreen1MouseMove(
  Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  Images.ZoomPoint := Point(X, Y);
end;

procedure TForm1.BGLVirtualScreen1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then begin
    RepaintTimer.Enabled := False;
    LoadingTimer.Enabled := False;
    if FullScreen then
      SetFullScreen(False);
    BGLVirtualScreen1.Align := alNone;
    BGLVirtualScreen1.SetBounds(0, 0, 0, 0);
    if SelectDirectoryDialog1.Execute then
      Images.LoadFromDir(SelectDirectoryDialog1.FileName);
    BGLVirtualScreen1.Align := alClient;
    RepaintTimer.Enabled := True;
    LoadingTimer.Enabled := True;
  end;
end;

procedure TForm1.BGLVirtualScreen1Redraw(Sender: TObject; Ctx: TBGLContext);
var
  i: Integer;
  s: string;
begin
  Ctx.Canvas.Fill(CSSBlack);

  for i := Ctx.Sprites.Count - 1 downto 0 do
    if Ctx.Sprites.Sprite[i].Layer < 0 then
      Ctx.Sprites.Delete(i);

  Ctx.Sprites.OnDraw;

  CurrentSpriteCount := ctx.Sprites.Count;
  s := IntToStr(Speed) + ' - ' + IntToStr(CurrentSpriteCount) + ' - ' + IntToStr(ImagesMover.Count) + ' - ' + IntToStr(Images.SpriteObjects.Count) + ' - ' + IntToStr(Images.Count);
  if FullScreen then
  begin
    Ctx.Canvas.RoundRect(5, 5, 15 + InfoText.TextWidth(s), 31, 10, 10 , CSSRed, BGRA(0, 0, 0, 128));
    InfoText.SetGradientColors(CSSWhite, CSSWhite, CSSYellow, CSSYellow);
    InfoText.TextOut(10, 10, s, taLeftJustify, tlTop, BGRABlack);
  end
  else
    Caption := s;
end;

procedure TForm1.BGLVirtualScreen1UnloadTextures(Sender: TObject; BGLContext: TBGLContext);
begin
  BGLContext.Sprites.Clear;
end;

end.

