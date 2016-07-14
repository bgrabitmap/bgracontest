unit Unit1;
//A source by Ara aradeonas@operamail.com
//If you nee any help or want to use it in real app please let me know.
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, BGRABitmap, BGRABitmapTypes, LCLType, WallOfTiles, wcButtons;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure PlayButtonOnClick(Sender: TObject);
    procedure BackButtonOnClick(Sender: TObject);
  private
    Wall: TWallOfTiles;
    PlayButton, NextButton, PreviousButton, BackButton, ShuffleButton, RepeatButton, VolumeButton: TWCButton;
    Cover: TImage;
    ArtisLabel, AlbumLabel, TitleLabel: TLabel;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
  s: string;
  bmp, cov: TBGRABitmap;
begin
  Randomize;
  Wall := TWallOfTiles.Create(Self);
  with Wall do
  begin
    Wall.Parent := Self;
    Align := alClient;
    TileSize := 75;
    Margin := 1;
    Color := clBlack;
    Interval := 1500;
    AnimateTime := 2500;
    BlackAndWhite := True;
    MakeFullScreen := True;
    MovingCamera := True;
  end;

  for i := 1 to 49 do
  begin
    s := ExtractFilePath(ParamStr(0)) + 'Covers' + PathDelim + 'Cover' + IntToStr(i) + '.jpg';
    if FileExists(s) then
      bmp := TBGRABitmap.Create(s)
    else
      bmp := TBGRABitmap.Create(100, 100, BGRA(Random(255), Random(255), Random(255)));
    Wall.Tiles.Add(bmp);
  end;

  PlayButton := TWCButton.Create(Self);
  with PlayButton do
  begin
    Parent := Wall;
    Background := Wall.Bitmap;
    ButtonType := twPlay;
    Width := 55;
    Height := 55;
    OnClick := @PlayButtonOnClick;
  end;
  NextButton := TWCButton.Create(Self);
  with NextButton do
  begin
    Parent := Wall;
    Background := Wall.Bitmap;
    ButtonType := twNext;
    Width := 47;
    Height := 47;
  end;
  PreviousButton := TWCButton.Create(Self);
  with PreviousButton do
  begin
    Parent := Wall;
    Background := Wall.Bitmap;
    ButtonType := twPrevious;
    Width := 47;
    Height := 47;
  end;
  BackButton := TWCButton.Create(Self);
  with BackButton do
  begin
    Parent := Wall;
    Background := Wall.Bitmap;
    ButtonType := twLeft;
    Width := 80;
    Height := 80;
    OnClick := @BackButtonOnClick;
  end;
  ShuffleButton := TWCButton.Create(Self);
  with ShuffleButton do
  begin
    Parent := Wall;
    Background := Wall.Bitmap;
    ButtonType := twShuffle;
    Width := 25;
    Height := 25;
  end;
  RepeatButton := TWCButton.Create(Self);
  with RepeatButton do
  begin
    Parent := Wall;
    Background := Wall.Bitmap;
    ButtonType := twRepeat;
    Width := 25;
    Height := 25;
  end;
  VolumeButton := TWCButton.Create(Self);
  with VolumeButton do
  begin
    Parent := Wall;
    Background := Wall.Bitmap;
    ButtonType := twVolume;
    Width := 25;
    Height := 25;
  end;


  Cover := TImage.Create(Self);
  with Cover do
  begin
    Parent := Wall;
    Width := 160;
    Height := 160;
  end;
  s := ExtractFilePath(ParamStr(0)) + 'Cover.jpg';
  if FileExists(s) then
    bmp := TBGRABitmap.Create(s)
  else
    bmp := TBGRABitmap.Create(100, 100, BGRA(Random(255), Random(255), Random(255)));
  BGRAReplace(bmp, bmp.Resample(150, 150));
  cov := TBGRABitmap.Create(bmp.Width + 10, bmp.Height + 10, BGRAWhite);
  cov.PutImage(5, 5, bmp, dmDrawWithTransparency);
  Cover.Picture.Bitmap.Assign(cov.Bitmap);


  ArtisLabel := TLabel.Create(Self);
  with ArtisLabel do
  begin
    Parent := Wall;
    AutoSize := True;
    Transparent := True;
    Font.Style := [fsBold];
    Font.Size := 24;
    Font.Color := clWhite;
    Caption := 'Finch';
  end;
  AlbumLabel := TLabel.Create(Self);
  with AlbumLabel do
  begin
    Parent := Wall;
    AutoSize := True;
    Transparent := True;
    Font.Size := 20;
    Font.Color := clWhite;
    Caption := 'Back to Oblivion';
  end;
  TitleLabel := TLabel.Create(Self);
  with TitleLabel do
  begin
    Parent := Wall;
    AutoSize := True;
    Transparent := True;
    Font.Style := [fsBold];
    Font.Size := 16;
    Font.Color := clWhite;
    Caption := 'Us vs. Them';
  end;

  WindowState := wsMaximized;
  BorderStyle := bsNone;

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Wall.Free;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if key = VK_ESCAPE then
    Close;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  with PlayButton do
  begin
    Left := round(Wall.Width * 0.9);
    Top := round(Wall.Height * 0.95);
  end;
  with NextButton do
  begin
    Left := PlayButton.Left + PlayButton.Width - 5;
    Top := PlayButton.Top + 4;
  end;
  with PreviousButton do
  begin
    Left := PlayButton.Left - PlayButton.Width + 11;
    Top := PlayButton.Top + 4;
  end;
  with BackButton do
  begin
    Left := round(BackButton.Width * 0.4);
    Top := round(-BackButton.Height * 0.3);
  end;
  with ShuffleButton do
  begin
    Left := PreviousButton.Left - NextButton.Width + 9;
    Top := PreviousButton.Top + 10;
  end;
  with RepeatButton do
  begin
    Left := ShuffleButton.Left - ShuffleButton.Width + 2;
    Top := PreviousButton.Top + 10;
  end;
  with VolumeButton do
  begin
    Left := NextButton.Left + NextButton.Width + 4;
    Top := PreviousButton.Top + 10;
  end;


  with Cover do
  begin
    Left := round(Wall.Width * 0.07);
    Top := round(Wall.Height * 0.75);
  end;


  with ArtisLabel do
  begin
    Left := Cover.Left + Cover.Width + 15;
    Top := Cover.Top + 30;
  end;
  with AlbumLabel do
  begin
    Left := ArtisLabel.Left;
    Top := ArtisLabel.Top + 40;
  end;
  with TitleLabel do
  begin
    Left := ArtisLabel.Left;
    Top := AlbumLabel.Top + 56;
  end;


  Wall.Active := True;
end;

procedure TForm1.PlayButtonOnClick(Sender: TObject);
begin
  if PlayButton.ButtonType = twPlay then
    PlayButton.ButtonType := twPause
  else
    PlayButton.ButtonType := twPlay;
end;

procedure TForm1.BackButtonOnClick(Sender: TObject);
begin
  Close;
end;

end.
