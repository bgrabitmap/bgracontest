unit Main;
//A source by Ara aradeonas@operamail.com
//If you nee any help or want to use it in real app please let me know.
//Source idea is http://www.humanssince1982.com/
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, TikanUnit;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Edit1: TEdit;
    Panel1: TPanel;
    Timer1: TTimer;
    Timer2: TTimer;
    procedure Button10Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
  private
    { private declarations }
  public
    Tikan: TTikan;
    se: integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption := 'Tikan';
  Position := poScreenCenter;
  Tikan := TTikan.Create(Self);
  Tikan.Left := 0;
  Tikan.Top := 0;
  Tikan.TikSize := 50;
  Tikan.SetGridSize(12, 24);
  {$IFDEF debug}
  Panel1.Visible := True;
  Panel1.BringToFront;
  {$ELSE}
  Panel1.Visible := False;
  {$ENDIF}
  AutoSize := True;
  BorderIcons := [biSystemMenu, biMinimize];
  se := 0;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Tikan.SetClear;
end;

procedure TForm1.Button10Click(Sender: TObject);
begin
  Tikan.SetEffect(e5);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Tikan.Mode := tmFastClock;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Tikan.SetText('');
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  //Tikan.SetText('01:47');
  Tikan.SetText(Edit1.Text);
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  Tikan.SetEffect(e1);
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  Tikan.SetEffect(e2);
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  Timer1.Enabled := not Timer1.Enabled;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  Tikan.SetEffect(e3);
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
  Tikan.SetEffect(e4);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Tikan.Initialise;
  Button1.Click;
end;

procedure TForm1.Panel1Click(Sender: TObject);
begin

end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if StrToInt(FormatDateTime('SS', Now)) mod 5 = 0 then
    Tikan.SetText(FormatDateTime('NN:SS', Now));
end;

procedure TForm1.Timer2Timer(Sender: TObject);
begin
  se += 1;
  case se of
    5: Tikan.SetEffect(e1);
    10: Tikan.SetText('01:47');
    15: Tikan.SetText('18:26');
    20: Tikan.SetText('03:29');
    27: Tikan.SetEffect(e2);
    32: Tikan.SetText('23:59');
    39: Tikan.SetText('-');
    45: Tikan.SetEffect(e5);
    49: Timer1.Enabled := True;
  end;
end;

end.

