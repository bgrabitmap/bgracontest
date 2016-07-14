unit TikanUnit;
//A source by Ara aradeonas@operamail.com
//If you nee any help or want to use it in real app please let me know.
//Source idea is http://www.humanssince1982.com/
{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
interface

uses
  Classes, SysUtils, Controls, ExtCtrls, Math, OpenGLContext, GL, BGRABitmap, BGRABitmapTypes, BGRAOpenGL;

type
  TTikTime = record
    Hour, Minute, Second, MilliSecond: single;
  end;
  TTikSpeed = TTikTime;
  TTikanMode = (tmFix, tmClock, tmFastClock, tmEffect);
  TTikTimeType = (tn, t030, t915, t015, t330, t930, t045);
  TTikanEffect = (e1, e2, e3, e4, e5, e6);
  TTikan = class;
  { TTik }

  TTik = record
    Index: integer;
    Tikan: TTikan;
    Time, SourceTime, DestinationTime: TTikTime;
    Speed, DestinationSpeed: TTikSpeed;
    Delay: integer;
    Reversed: boolean;
    function Row: integer;
    function Column: integer;
  end;
  { TTikan }

  TTikan = class(TOpenGLControl)
  private
    FAnimating: boolean;
    FInitialised: boolean;
    FMode: TTikanMode;
    FTikSize: integer;
    Tiks: array of TTik;
    DiskBmp, HourBmp, MinuteBmp: TBGLBitmap;
    MarginX, MarginY, RowCount, ColumnCount: integer;
    Timer: TTimer;
    FEffect: TTikanEffect;
    Finished: boolean;
    procedure OnTimer(Sender: TObject);
    procedure SetMode(AValue: TTikanMode);
    procedure SetTikSize(AValue: integer);
  protected
    procedure Resize; override;
    procedure PrepareTiks;
    procedure PrepareBitmaps;
    procedure Draw;
    procedure DrawTik(Index: integer);
    procedure SetTikTime(ARow, AColumn: integer; ATime: TTikTime);
    function MoveValue(Reversed: boolean; V1, V2: single): single;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialise;
    procedure Paint; override;
    procedure SetGridSize(ARowCount, AColumnCount: integer);
    procedure SetAllTiksTime(ATime: TTikTime);
    procedure SetText(AText: string);
    procedure SetEffect(AEffect: TTikanEffect);
    procedure SetClear;
  public
    property TikSize: integer read FTikSize write SetTikSize;
    property Mode: TTikanMode read FMode write SetMode;
    property Animating: boolean read FAnimating write FAnimating;
  end;

function TikTime(AHour: single; AMinute: single; ASecond: single = 0; AMilliSecond: single = 0): TTikTime; overload;
function TikTime(ATime: TTime): TTikTime; overload;
function TikTime(AType: TTikTimeType): TTikTime; overload;
function TikSpeed(AHour, AMinute, ASecond, AMilliSecond: single): TTikSpeed;

implementation

function TikTime(AHour, AMinute, ASecond, AMilliSecond: single): TTikTime;
begin
  with Result do
  begin
    Hour := AHour;
    Minute := AMinute;
    Second := ASecond;
    MilliSecond := AMilliSecond;
  end;
end;

function TikSpeed(AHour, AMinute, ASecond, AMilliSecond: single): TTikSpeed;
begin
  with Result do
  begin
    Hour := AHour;
    Minute := AMinute;
    Second := ASecond;
    MilliSecond := AMilliSecond;
  end;
end;

function TikTime(ATime: TTime): TTikTime;
var
  Hour, Minute, Second, MilliSecond: word;
begin
  DecodeTime(ATime, Hour, Minute, Second, MilliSecond);
  Result := TikTime(Hour, Minute, Second, MilliSecond);
end;

function TikTime(AType: TTikTimeType): TTikTime;
begin
  case AType of
    tn: Result := TikTime(t915{7.5, 37.5});
    t030: Result := TikTime(0, 30);
    t915: Result := TikTime(9, 15);
    t015: Result := TikTime(0, 15);
    t330: Result := TikTime(3, 30);
    t930: Result := TikTime(9, 30);
    t045: Result := TikTime(0, 45);
  end;
end;

{ TTik }
function TTik.Row: integer;
begin
  Result := Index div Tikan.ColumnCount;
end;

function TTik.Column: integer;
begin
  Result := Index mod Tikan.ColumnCount;
end;

{ TTikan }

procedure TTikan.SetTikSize(AValue: integer);
begin
  if FTikSize = AValue then
    Exit;
  FTikSize := AValue;
  PrepareBitmaps;
  PrepareTiks;
end;

procedure TTikan.OnTimer(Sender: TObject);
var
  Hour, Minute, Second, MilliSecond: word;
  i, fi: integer;
  t, dt: TTikTime;
  st, dst: TTikSpeed;
  b, r: boolean;
  a: integer;
begin
  fi := 0;
  for i := 0 to Length(Tiks) - 1 do
  begin
    case Mode of
      tmFix:
      begin
        if Animating then
        begin
          t := Tiks[i].Time;
          dt := Tiks[i].DestinationTime;
          st := Tiks[i].Speed;
          r := Tiks[i].Reversed;
          if not (t.Hour = dt.Hour) then
            if r then
              t.Hour := Max(dt.Hour, t.Hour - ((st.Hour * 12) * (Timer.Interval / 1000)))
            else
              t.Hour := Min(dt.Hour, t.Hour + ((st.Hour * 12) * (Timer.Interval / 1000)));
          if not (t.Minute = dt.Minute) then
            if r then
              t.Minute := Max(dt.Minute, t.Minute - ((st.Minute * 60) * (Timer.Interval / 1000)))
            else
              t.Minute := Min(dt.Minute, t.Minute + ((st.Minute * 60) * (Timer.Interval / 1000)));
          if not (t.Second = dt.Second) then
            if r then
              t.Second := Max(dt.Second, t.Second - ((st.Second * 60) * (Timer.Interval / 1000)))
            else
              t.Second := Min(dt.Second, t.Second + ((st.Second * 60) * (Timer.Interval / 1000)));
          if not (t.MilliSecond = dt.MilliSecond) then
            if r then
              t.MilliSecond := Max(dt.MilliSecond, t.MilliSecond - ((st.MilliSecond * 1000) * (Timer.Interval / 1000)))
            else
              t.MilliSecond := Min(dt.MilliSecond, t.MilliSecond + ((st.MilliSecond * 1000) * (Timer.Interval / 1000)));
          Tiks[i].Time := t;

          if (t.Hour = dt.Hour) and (t.Minute = dt.Minute) and (t.Second = dt.Second) and (t.MilliSecond = dt.MilliSecond) then
          begin
            Tiks[i].Reversed := False;
            Inc(fi);
          end;
        end;
      end;
      tmClock: Tiks[i].Time := TikTime(Now);
      tmFastClock:
      begin
        DecodeTime(Now, Hour, Minute, Second, MilliSecond);
        Tiks[i].Time := TikTime(Minute, Second, 0, 0);
      end;
      tmEffect:
      begin
        t := Tiks[i].Time;
        st := Tiks[i].Speed;
        dst := Tiks[i].DestinationSpeed;
        case FEffect of
          e1, e2, e3, e4, e5, e6:
          begin
            b := False;
            Tiks[i].Delay := Tiks[i].Delay - Timer.Interval;
            if (st.Hour = dst.Hour) and (st.Minute = dst.Minute) and (st.Second = dst.Second) and (st.MilliSecond = dst.MilliSecond) then
            begin
              if Tiks[i].Delay <= 0 then
                b := True;
            end
            else
            begin
              a := 1;
              if not (st.Hour = dst.Hour) then
                st.Hour := Min(dst.Hour, st.Hour + ((st.Hour * a) * (Timer.Interval / 1000)));
              if not (st.Minute = dst.Minute) then
                st.Minute := Min(dst.Minute, st.Minute + ((st.Minute * a) * (Timer.Interval / 1000)));
              if not (st.Second = dst.Second) then
                st.Second := Min(dst.Second, st.Second + ((st.Second * a) * (Timer.Interval / 1000)));
              if not (st.MilliSecond = dst.MilliSecond) then
                st.MilliSecond := Min(dst.MilliSecond, st.MilliSecond + ((st.MilliSecond * a) * (Timer.Interval / 1000)));
              Tiks[i].Speed := st;
              b := True;
            end;
            if b then
            begin
              t.Hour := MoveValue(Tiks[i].Reversed, t.Hour, ((st.Hour * 12) * (Timer.Interval / 1000)));
              t.Minute := MoveValue(Tiks[i].Reversed, t.Minute, ((st.Minute * 60) * (Timer.Interval / 1000)));
              t.Second := MoveValue(Tiks[i].Reversed, t.Second, ((st.Second * 60) * (Timer.Interval / 1000)));
              t.MilliSecond := MoveValue(Tiks[i].Reversed, t.MilliSecond, ((st.MilliSecond * 1000) * (Timer.Interval / 1000)));
            end;
          end;
        end;
        Tiks[i].Time := t;
      end;
    end;
  end;
  Refresh;
  if fi = Length(Tiks) then
  begin
    Timer.Enabled := False;
    for i := 0 to Length(Tiks) - 1 do
    begin
      Tiks[i].Reversed := False;
    end;
    Finished := True;
  end;
end;

procedure TTikan.SetMode(AValue: TTikanMode);
var
  i: integer;
  st: TTikSpeed;
begin
  FMode := AValue;
  Finished := False;
  case Mode of
    tmFix:
    begin
      if Animating then
      begin
        for i := 0 to Length(Tiks) - 1 do
        begin
          st := Tiks[i].Speed;
          Tiks[i].Speed := TikSpeed(Max(0.25, st.Hour), Max(0.25, st.Minute), Max(0.25, st.Second), Max(0.25, st.MilliSecond));
        end;
        Timer.Interval := 25;
        Timer.Enabled := True;
      end
      else
      begin
        Timer.Enabled := False;
        for i := 0 to Length(Tiks) - 1 do
        begin
          Tiks[i].Time := TikTime(0, 15, 30, 750);
        end;
      end;
    end;
    tmClock, tmFastClock:
    begin
      Timer.Interval := 100;
      Timer.Enabled := True;
    end;
    tmEffect:
    begin
      Timer.Interval := 25;
      Timer.Enabled := True;
    end;
  end;
end;

procedure TTikan.Paint;
begin
  inherited Paint;
  if not FInitialised then
    Exit;
  BGLViewPort(Width, Height, BGRAWhite);
  Draw;
  SwapBuffers;
end;

procedure TTikan.SetGridSize(ARowCount, AColumnCount: integer);
begin
  Width := (AColumnCount + 2) * TikSize;
  Height := (ARowCount + 2) * TikSize;
end;

procedure TTikan.SetAllTiksTime(ATime: TTikTime);
var
  i: integer;
begin
  for i := 0 to Length(Tiks) - 1 do
  begin
    Tiks[i].DestinationTime := ATime;
  end;
  case Mode of
    tmFix: Refresh;
  end;
end;

procedure TTikan.SetText(AText: string);
var
  i, j: integer;
  c: string;
  arr: array of array of TTikTime;
  xl, yl: integer;
  t, dt: TTikTime;

  procedure SetTikTimes(tt: array of TTikTimeType);
  var
    k, l, n: integer;
  begin
    for l := 0 to yl - 1 do
    begin
      n := 0;
      for k := Length(arr) - xl to Length(arr) - 1 do
      begin
        arr[k][l] := TikTime(tt[(l * xl) + n]);
        Inc(n);
      end;
    end;
  end;

begin
  Mode := tmFix;
  if AText = '' then
  begin
    for i := 0 to Length(Tiks) - 1 do
    begin
      Tiks[i].Speed := TikSpeed(0.25, 0.25, 0.25, 0.25);
      if (Tiks[i].Column = 0) then
        Tiks[i].DestinationTime := TikTime(3, 15, 0, 0)
      else
      if (Tiks[i].Column = ColumnCount - 1) then
        Tiks[i].DestinationTime := TikTime(9, 15, 0, 0)
      else
        Tiks[i].DestinationTime := TikTime(3, 45, 0, 0);
    end;
  end
  else
  if AText = '-' then
  begin
    for i := 0 to Length(Tiks) - 1 do
    begin
      Tiks[i].Speed := TikSpeed(0.25, 0.25, 0.25, 0.25);
      Tiks[i].DestinationTime := TikTime(3, 45, 0, 0);
    end;
  end
  else
  begin
    for i := 0 to Length(Tiks) - 1 do
    begin
      Tiks[i].DestinationTime := TikTime(tn);
    end;
    yl := 6;
    SetLength(arr, 0, yl);
    for i := 0 to Length(AText) - 1 do
    begin
      c := AText[i + 1];
      case c of
        '0':
        begin
          xl := 5;
          SetLength(arr, Length(arr) + xl, yl);
          SetTikTimes([{0}t330, t915, t915, t915, t930,{1} t030, t330, t915, t930, t030,{2}t030, t030, tn, t030, t030,
            {3}t030, t030, tn, t030, t030{4}, t030, t015, t915, t045, t030 {5}, t015, t915, t915, t915, t045]);
        end;
        '1':
        begin
          xl := 5;
          SetLength(arr, Length(arr) + xl, yl);
          SetTikTimes([{0}tn, tn, t330, t915, t930,{1} tn, tn, t015, t930, t030,{2}tn, tn, tn, t030, t030,
            {3}tn, tn, tn, t030, t030 {4}, tn, tn, tn, t030, t030 {5}, tn, tn, tn, t015, t045]);
        end;
        '2':
        begin
          xl := 5;
          SetLength(arr, Length(arr) + xl, yl);
          SetTikTimes([{0}t330, t915, t915, t915, t930,{1}t015, t915, t915, t930, t030,{2}t330, t915, t915, t045, t030,
            {3}t030, t330, t915, t915, t045,{4}t030, t015, t915, t915, t930,{5}t015, t915, t915, t915, t045]);
        end;
        '3':
        begin
          xl := 5;
          SetLength(arr, Length(arr) + xl, yl);
          SetTikTimes([{0}t330, t915, t915, t915, t930,{1}t015, t915, t915, t930, t030,{2}t330, t915, t915, t045, t030,
            {3}t015, t915, t915, t930, t030{4}, t330, t915, t915, t045, t030{5}, t015, t915, t915, t915, t045]);
        end;
        '4':
        begin
          xl := 5;
          SetLength(arr, Length(arr) + xl, yl);
          SetTikTimes([{0}t330, t930, tn, t330, t930,{1}t030, t030, tn, t030, t030,{2}t030, t015, t915, t045, t030,
            {3}t015, t915, t915, t930, t030{4}, tn, tn, tn, t030, t030{5}, tn, tn, tn, t015, t045]);
        end;
        '5':
        begin
          xl := 5;
          SetLength(arr, Length(arr) + xl, yl);
          SetTikTimes([{0}t330, t915, t915, t915, t930,{1} t030, t330, t915, t915, t045,{2}t030, t015, t915, t915, t930,
            {3}t015, t915, t915, t930, t030 {4}, t330, t915, t915, t045, t030 {5}, t015, t915, t915, t915, t045]);
        end;
        '6':
        begin
          xl := 5;
          SetLength(arr, Length(arr) + xl, yl);
          SetTikTimes([{0}t330, t915, t915, t915, t930,{1} t030, t330, t915, t915, t045,{2}t030, t015, t915, t915, t930,
            {3}t030, t330, t915, t930, t030 {4}, t030, t015, t915, t045, t030 {5}, t015, t915, t915, t915, t045]);
        end;
        '7':
        begin
          xl := 5;
          SetLength(arr, Length(arr) + xl, yl);
          SetTikTimes([{0}t330, t915, t915, t915, t930,{1} t015, t915, t915, t930, t030,{2}tn, tn, tn, t030, t030,
            {3}tn, tn, tn, t030, t030 {4}, tn, tn, tn, t030, t030 {5}, tn, tn, tn, t015, t045]);
        end;
        '8':
        begin
          xl := 5;
          SetLength(arr, Length(arr) + xl, yl);
          SetTikTimes([{0}t330, t915, t915, t915, t930,{1} t030, t330, t915, t930, t030,{2}t030, t015, t915, t045, t030,
            {3}t030, t330, t915, t930, t030 {4}, t030, t015, t915, t045, t030 {5}, t015, t915, t915, t915, t045]);
        end;
        '9':
        begin
          xl := 5;
          SetLength(arr, Length(arr) + xl, yl);
          SetTikTimes([{0}t330, t915, t915, t915, t930,{1} t030, t330, t915, t930, t030,{2}t030, t015, t915, t045, t030,
            {3}t015, t915, t915, t045, t030 {4}, t330, t915, t915, t045, t030 {5}, t015, t915, t915, t915, t045]);
        end;
        ':':
        begin
          xl := 2;
          SetLength(arr, Length(arr) + xl, yl);
          SetTikTimes([{0}tn, tn{1}, t330, t930{2}, t015, t045{3}, t330, t930{4}, t015, t045{5}, tn, tn]);
        end;
        ' ':
        begin
          xl := 2;
          SetLength(arr, Length(arr) + xl, yl);
          SetTikTimes([{0}tn, tn{1}, tn, tn{2}, tn, tn{3}, tn, tn{4}, tn, tn{5}, tn, tn]);
          //SetTikTimes([{0}t915, t915, t915, t915, t915,{1} t915, t915, t915, t915, t915,{2}t915, t915, t915, t915, t915,
          //  {3}t915, t915, t915, t915, t915 {4}, t915, t915, t915, t915, t915 {5}, t915, t915, t915, t915, t915]);
        end;
      end;
    end;
    for i := 0 to Length(arr) - 1 do
    begin
      for j := 0 to Length(arr[i]) - 1 do
      begin
        SetTikTime(j + 3, i + 1, arr[i][j]);
      end;
    end;
  end;
  for i := 0 to Length(Tiks) - 1 do
  begin

    t := Tiks[i].Time;
    dt := Tiks[i].DestinationTime;

    if Tiks[i].Reversed then
    begin
      while dt.Hour > t.Hour do
        dt.Hour := dt.Hour - 12;
      while dt.Minute > t.Minute do
        dt.Minute := dt.Minute - 60;
      while dt.Second > t.Second do
        dt.Second := dt.Second - 60;
      while dt.MilliSecond > t.MilliSecond do
        dt.MilliSecond := dt.MilliSecond - 1000;
    end
    else
    begin
      while dt.Hour < t.Hour do
        dt.Hour := dt.Hour + 12;
      while dt.Minute < t.Minute do
        dt.Minute := dt.Minute + 60;
      while dt.Second < t.Second do
        dt.Second := dt.Second + 60;
      while dt.MilliSecond < t.MilliSecond do
        dt.MilliSecond := dt.MilliSecond + 1000;
    end;


    Tiks[i].DestinationTime := dt;
  end;

  if not Animating then
    Refresh;
end;

procedure TTikan.SetEffect(AEffect: TTikanEffect);

  procedure DoEffect1;
  var
    i: integer;
    ts: TTikSpeed;
  begin
    for i := 0 to Length(Tiks) - 1 do
    begin
      if Mode <> tmEffect then
        Tiks[i].Delay := (ColumnCount - Tiks[i].Column) * 100;
      ts := TikSpeed(0.25, 0.25, 0.25, 0.25);
      if Mode <> tmEffect then
      begin
        Tiks[i].Speed := ts;
        Tiks[i].DestinationSpeed := ts;
      end
      else
      begin
        Tiks[i].DestinationSpeed := ts;
      end;
    end;
  end;

  procedure DoEffect2;
  var
    i: integer;
    sa: array of single;
    ts: TTikSpeed;
  begin
    SetLength(sa, 5);
    sa[0] := 0.15;
    sa[1] := 0.25;
    sa[2] := 0.5;
    sa[3] := 0.75;
    sa[4] := 1;
    for i := 0 to Length(Tiks) - 1 do
    begin
      Tiks[i].Delay := (ColumnCount - Tiks[i].Column) * 100;
      ts := TikSpeed(sa[Random(Length(sa))], sa[Random(Length(sa))], sa[Random(Length(sa))], sa[Random(Length(sa))]);
      if Mode <> tmEffect then
      begin
        Tiks[i].Speed := ts;
        Tiks[i].DestinationSpeed := ts;
      end
      else
      begin
        Tiks[i].DestinationSpeed := ts;
      end;
    end;
  end;

  procedure DoEffect3;
  var
    i: integer;
  begin
    DoEffect1;
    for i := 0 to Length(Tiks) - 1 do
    begin
      Tiks[i].Reversed := Tiks[i].Column < ColumnCount div 2;
    end;
  end;

  procedure DoEffect4;
  var
    i: integer;
  begin
    DoEffect3;
    for i := 0 to Length(Tiks) - 1 do
    begin
      Tiks[i].Delay := 0;
    end;
  end;

  procedure DoEffect5;
  var
    i, x, y: integer;
  begin
    DoEffect1;
    for i := 0 to Length(Tiks) - 1 do
    begin
      x := Tiks[i].Column - (ColumnCount div 2);
      y := Tiks[i].Row - (RowCount div 2);
      if ((x < 0) and (y < 0)) or ((x >= 0) and (y >= 0)) then
        Tiks[i].Reversed := True;
      if ((x >= 0) and (y < 0)) or ((x < 0) and (y >= 0)) then
        Tiks[i].Reversed := False;
      Tiks[i].Delay := abs(x) * abs(y) * 50;
    end;
  end;

  procedure DoEffect6;
  begin

  end;

begin
  FEffect := AEffect;
  case AEffect of
    e1: DoEffect1;
    e2: DoEffect2;
    e3: DoEffect3;
    e4: DoEffect4;
    e5: DoEffect5;
    e6: DoEffect6;
  end;
  Mode := tmEffect;
end;

procedure TTikan.SetClear;
begin
  Mode := tmFix;
  SetAllTiksTime(TikTime(tn));
end;

procedure TTikan.Resize;
begin
  inherited Resize;
  if not FInitialised then
    Exit;
  PrepareTiks;
end;

procedure TTikan.PrepareTiks;
var
  tc, i: integer;
begin
  ColumnCount := (Width div TikSize) - 2;
  RowCount := (Height div TikSize) - 2;
  MarginX := TikSize;
  MarginY := TikSize;
  tc := RowCount * ColumnCount;
  if tc <= 0 then
    Exit;
  SetLength(Tiks, tc);
  for i := 0 to Length(Tiks) - 1 do
  begin
    Tiks[i].Index := i;
    Tiks[i].Tikan := Self;
  end;
  Mode := Mode;
end;

procedure TTikan.PrepareBitmaps;
var
  r, w, h: single;
  col: TBGRAPixel;
begin
  if not FInitialised then
    Exit;
  r := TikSize / 2;
  DiskBmp.SetSize(TikSize, TikSize);
  col := BGRA(210, 210, 210);
  DiskBmp.EllipseAntialias(DiskBmp.Width / 2, DiskBmp.Height / 2, r - 1, r - 1, col, 2);
  h := r;
  w := 0.2 * r;
  w := Max(w, 6);
  col := BGRABlack;
  HourBmp.SetSize(round(w), round(h));
  HourBmp.Rectangle(1, 1, HourBmp.Width - 1, HourBmp.Height - 1, BGRABlack, BGRABlack, dmSet);
  h := r + 2;
  w := 0.2 * r;
  w := Max(w, 6);
  col := BGRABlack;
  MinuteBmp.SetSize(round(w), round(h));
  MinuteBmp.Rectangle(1, 1, MinuteBmp.Width - 1, MinuteBmp.Height - 1, col, col, dmSet);
end;

procedure TTikan.Draw;
var
  i: integer;
begin
  for i := 0 to Length(Tiks) - 1 do
    DrawTik(i);
end;

procedure TTikan.DrawTik(Index: integer);
var
  cx, cy, x, y: single;
  t: TTik;

  procedure DrawHand(Bmp: TBGLBitmap; Angle: single);
  begin
    x := round(cx - ((Bmp.Width - 1) / 2));
    y := round(cy - ((Bmp.Width - 1) / 2));
    Bmp.Texture.DrawAngle(x, y, Angle - 180, PointF((Bmp.Width) / 2, (Bmp.Width) / 2), True);
  end;

begin
  t := Tiks[Index];
  cx := MarginX + (TikSize div 2) + (t.Column * TikSize);
  cy := MarginY + (TikSize div 2) + (t.Row * TikSize);

  x := cx - TikSize div 2;
  y := cy - TikSize div 2;
  DiskBmp.Texture.Draw(x, y);
  DrawHand(HourBmp, t.Time.Hour / 12 * 360);
  DrawHand(MinuteBmp, t.Time.Minute / 60 * 360);
end;

procedure TTikan.SetTikTime(ARow, AColumn: integer; ATime: TTikTime);
var
  i: integer;
begin
  i := ARow * ColumnCount + AColumn;
  if i >= Length(Tiks) then
    Exit;
  if Animating then
    Tiks[i].DestinationTime := ATime
  else
    Tiks[i].Time := ATime;
end;

function TTikan.MoveValue(Reversed: boolean; V1, V2: single): single;
begin
  if Reversed then
    Result := V1 - V2
  else
    Result := V1 + V2;
end;

constructor TTikan.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Parent := TWinControl(TheOwner);
  AutoResizeViewport := True;
  DiskBmp := TBGLBitmap.Create;
  HourBmp := TBGLBitmap.Create;
  MinuteBmp := TBGLBitmap.Create;
  FTikSize := 10;
  FMode := tmFix;
  Timer := TTimer.Create(Self);
  Timer.Enabled := False;
  Timer.OnTimer := @OnTimer;
  FAnimating := True;
  Finished := True;
end;

destructor TTikan.Destroy;
begin
  Timer.Free;
  DiskBmp.Free;
  HourBmp.Free;
  MinuteBmp.Free;
  inherited Destroy;
end;

procedure TTikan.Initialise;
begin
  FInitialised := True;
  PrepareBitmaps;
  PrepareTiks;
end;

end.
