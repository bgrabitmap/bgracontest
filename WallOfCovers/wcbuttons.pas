unit wcButtons;
//A source by Ara aradeonas@operamail.com
//If you nee any help or want to use it in real app please let me know.

//Icons:
//<div>Icons made by <a href="http://www.flaticon.com/authors/appzgear" title="Appzgear">Appzgear</a> from <a href="http://www.flaticon.com" title="Flaticon">www.flaticon.com</a>             is licensed by <a href="http://creativecommons.org/licenses/by/3.0/" title="Creative Commons BY 3.0">CC BY 3.0</a></div>
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, BGRACanvas2D, BCImageButton;

type
  TWCButtonType = (twPlay, twPause, twNext, twPrevious, twShuffle, twLeft, twRepeat, twVolume);
  { TWCButton }

  TWCButton = class(TBCGraphicButton)
  private
    ctx: TBGRACanvas2D;
    FButtonType: TWCButtonType;
    zoom: single;
    margin: single;
    function GetButtonType: TWCButtonType;
    procedure SetButtonType(AValue: TWCButtonType);
    procedure DrawPlayButton;
    procedure DrawPauseButton;
    procedure DrawNextButton;
    procedure DrawPreviousButton;
    procedure DrawShuffleButton;
    procedure DrawLeftButton;
    procedure DrawRepeatButton;
    procedure DrawVolumeButton;
  protected
    procedure DrawControl; override;
  public
    Background: TBGRABitmap;
    property ButtonType: TWCButtonType read GetButtonType write SetButtonType;
  end;

implementation

{ TWCButton }

function TWCButton.GetButtonType: TWCButtonType;
begin
  Result := FButtonType;
end;

procedure TWCButton.SetButtonType(AValue: TWCButtonType);
begin
  FButtonType := AValue;
  Invalidate;
end;

procedure TWCButton.DrawControl;
var
  Bitmap, content, Blur, back: TBGRABitmap;
  nb: integer;
  r: TRect;
begin
  r := Rect(Left, Top, Left + Width, Top + Height);
  if Assigned(Background.Data) then
    back := Background.GetPart(r) as TBGRABitmap
  else
    back := TBGRABitmap.Create(Width, Height, BGRAWhite);
  Bitmap := TBGRABitmap.Create(back);
  back.Free;
  content := TBGRABitmap.Create(Width, Height);
  margin := Width / 8;

  ctx := content.Canvas2D;
  ctx.antialiasing := True;
  ctx.translate(margin, margin);
  ctx.pixelCenteredCoordinates := True;
  case FButtonType of
    twPlay: DrawPlayButton;
    twPause: DrawPauseButton;
    twNext: DrawNextButton;
    twPrevious: DrawPreviousButton;
    twShuffle: DrawShuffleButton;
    twLeft: DrawLeftButton;
    twRepeat: DrawRepeatButton;
    twVolume: DrawVolumeButton;
  end;

  if Enabled then
    case FState of
      gbsNormal: ctx.fillStyle('#EDEDED');
      gbsActive, gbsHover:
      begin
        ctx.fillStyle('#FFFFFF');
        if FState = gbsActive then
          nb := 2
        else
          nb := 0;
      end;
    end
  else
    ctx.fillStyle('#5E5E5E');
  ctx.fill();
  ctx.restore();
  case FState of
    gbsActive, gbsHover:
    begin
      Blur := TBGRABitmap.Create(Width, Height);
      //Blur.PutImage(0,0,content.FilterBlurRadial(Rect(0, 0, content.Width, content.Height), 10 + nb, rbFast),dmDrawWithTransparency);
      BGRAReplace(Blur, content.FilterBlurRadial(Rect(0, 0, content.Width, content.Height), 3 + nb, rbFast));
      Bitmap.PutImage(0, 0, Blur, dmDrawWithTransparency);
      Blur.Free;
    end;
  end;
  Bitmap.PutImage(0, 0, content, dmDrawWithTransparency);
  Bitmap.Draw(Canvas, 0, 0);
  content.Free;
  Bitmap.Free;
end;

procedure TWCButton.DrawPlayButton;
begin
  zoom := ((ctx.Width - (2 * margin)) / 40);
  ctx.scale(zoom, zoom);
  ctx.beginPath();
  ctx.moveTo(0, 0);
  ctx.lineTo(40.007, 0);
  ctx.lineTo(40.007, 40.007);
  ctx.lineTo(0, 40.007);
  ctx.closePath();
  ctx.clip();
  ctx.translate(0, 0);
  ctx.translate(0, 0);
  ctx.scale(1, 1);
  ctx.translate(0, 0);
  ctx.strokeStyle('rgba(0,0,0,0)');
  ctx.miterLimit := 4;
  ctx.save();
  ctx.beginPath();
  ctx.moveTo(37.324, 10.004);
  ctx.bezierCurveTo(31.802, 0.43799999999999883, 19.569, -2.84, 10.003999999999998, 2.6839999999999993);
  ctx.bezierCurveTo(0.438, 8.206, -2.84, 20.438, 2.684, 30.004);
  ctx.bezierCurveTo(8.206, 39.569, 20.438000000000002, 42.847, 30.004, 37.324);
  ctx.bezierCurveTo(39.569, 31.801, 42.848, 19.569, 37.324, 10.004);
  ctx.closePath();
  ctx.moveTo(28.004, 33.859);
  ctx.bezierCurveTo(20.352, 38.278, 10.566000000000003, 35.656, 6.148, 28.003);
  ctx.bezierCurveTo(1.729, 20.351, 4.351999999999999, 10.565000000000001, 12.004, 6.148);
  ctx.bezierCurveTo(19.656, 1.729, 29.442, 4.351, 33.86, 12.004);
  ctx.bezierCurveTo(38.278, 19.656, 35.656, 29.441, 28.004, 33.859);
  ctx.closePath();
  ctx.moveTo(27.204, 19.191);
  ctx.lineTo(16.731, 13.082999999999998);
  ctx.bezierCurveTo(15.777000000000001, 12.525999999999998, 15.007000000000001, 12.970999999999998, 15.012000000000002, 14.075);
  ctx.lineTo(15.066000000000003, 26.198999999999998);
  ctx.bezierCurveTo(15.071000000000003, 27.302999999999997, 15.850000000000003, 27.752, 16.807000000000002, 27.198999999999998);
  ctx.lineTo(27.200000000000003, 21.198999999999998);
  ctx.bezierCurveTo(28.156, 20.647, 28.158, 19.748, 27.204, 19.191);
  ctx.closePath();
end;

procedure TWCButton.DrawPauseButton;
begin
  zoom := ((ctx.Width - (2 * margin)) / 314);
  ctx.scale(zoom, zoom);
  ctx.beginPath();
  ctx.moveTo(0, 0);
  ctx.lineTo(314.065, 0);
  ctx.lineTo(314.065, 314.064);
  ctx.lineTo(0, 314.064);
  ctx.closePath();
  ctx.clip();
  ctx.translate(0, 0);
  ctx.translate(0, 0);
  ctx.scale(1, 1);
  ctx.translate(0, 0);
  ctx.strokeStyle('rgba(0,0,0,0)');
  ctx.lineCap := 'butt';
  ctx.lineJoin := 'miter';
  ctx.miterLimit := 4;
  ctx.save();
  ctx.beginPath();
  ctx.moveTo(125.632, 109.931);
  ctx.bezierCurveTo(116.96600000000001, 109.931, 109.93100000000001, 116.958, 109.93100000000001, 125.632);
  ctx.lineTo(109.93100000000001, 188.425);
  ctx.bezierCurveTo(109.93100000000001, 197.09300000000002, 116.96600000000001, 204.126, 125.632, 204.126);
  ctx.bezierCurveTo(134.298, 204.126, 141.333, 197.09300000000002, 141.333, 188.425);
  ctx.lineTo(141.333, 125.632);
  ctx.bezierCurveTo(141.333, 116.95, 134.298, 109.931, 125.632, 109.931);
  ctx.closePath();
  ctx.moveTo(293.002, 78.529);
  ctx.bezierCurveTo(249.646, 3.434, 153.618, -22.298, 78.529, 21.066);
  ctx.bezierCurveTo(3.434, 64.417, -22.298, 160.44, 21.066, 235.532);
  ctx.bezierCurveTo(64.416, 310.627, 160.441, 336.362, 235.531, 293.002);
  ctx.bezierCurveTo(310.627, 249.638, 336.363, 153.618, 293.002, 78.529);
  ctx.closePath();
  ctx.moveTo(219.834, 265.8);
  ctx.bezierCurveTo(159.767, 300.492, 82.94, 279.906, 48.25800000000001, 219.827);
  ctx.bezierCurveTo(13.568, 159.76, 34.161, 82.933, 94.23, 48.259);
  ctx.bezierCurveTo(154.301, 13.569000000000003, 231.12400000000002, 34.153, 265.808, 94.22999999999999);
  ctx.bezierCurveTo(300.493, 154.305, 279.906, 231.115, 219.834, 265.8);
  ctx.closePath();
  ctx.moveTo(188.432, 109.931);
  ctx.bezierCurveTo(179.768, 109.931, 172.731, 116.958, 172.731, 125.632);
  ctx.lineTo(172.731, 188.425);
  ctx.bezierCurveTo(172.731, 197.09300000000002, 179.768, 204.13400000000001, 188.432, 204.13400000000001);
  ctx.bezierCurveTo(197.1, 204.13400000000001, 204.13299999999998, 197.10100000000003, 204.13299999999998, 188.425);
  ctx.lineTo(204.13299999999998, 125.632);
  ctx.bezierCurveTo(204.133, 116.95, 197.1, 109.931, 188.432, 109.931);
  ctx.closePath();
end;

procedure TWCButton.DrawNextButton;
begin
  zoom := ((ctx.Width - (2 * margin)) / 314);
  ctx.scale(zoom, zoom);
  ctx.beginPath();
  ctx.moveTo(0, 0);
  ctx.lineTo(314.065, 0);
  ctx.lineTo(314.065, 314.064);
  ctx.lineTo(0, 314.064);
  ctx.closePath();
  ctx.clip();
  ctx.translate(0, 0);
  ctx.translate(0, 0);
  ctx.scale(1, 1);
  ctx.translate(0, 0);
  ctx.strokeStyle('rgba(0,0,0,0)');
  ctx.lineCap := 'butt';
  ctx.lineJoin := 'miter';
  ctx.miterLimit := 4;
  ctx.save();
  ctx.beginPath();
  ctx.moveTo(222.047, 149.318);
  ctx.lineTo(170.463, 118.10500000000002);
  ctx.bezierCurveTo(163.049, 113.61400000000002, 157.064, 116.99700000000001, 157.09799999999998, 125.67000000000002);
  ctx.lineTo(157.17399999999998, 143.311);
  ctx.lineTo(115.51199999999997, 118.10400000000001);
  ctx.bezierCurveTo(108.09499999999997, 113.61300000000001, 102.11199999999997, 116.99600000000001, 102.14599999999997, 125.66900000000001);
  ctx.lineTo(102.41799999999998, 188.46800000000002);
  ctx.bezierCurveTo(102.44799999999998, 197.14000000000001, 108.51099999999998, 200.55800000000002, 115.95299999999997, 196.11);
  ctx.lineTo(157.30199999999996, 171.35600000000002);
  ctx.lineTo(157.37799999999996, 188.47600000000003);
  ctx.bezierCurveTo(157.40799999999996, 197.14800000000002, 163.47299999999996, 200.56700000000004, 170.91199999999995, 196.11900000000003);
  ctx.lineTo(222.02599999999995, 165.51800000000003);
  ctx.bezierCurveTo(229.453, 161.062, 229.46, 153.807, 222.047, 149.318);
  ctx.closePath();
  ctx.moveTo(293.002, 78.529);
  ctx.bezierCurveTo(249.646, 3.434, 153.618, -22.298, 78.529, 21.066);
  ctx.bezierCurveTo(3.434, 64.417, -22.298, 160.44, 21.066, 235.532);
  ctx.bezierCurveTo(64.416, 310.627, 160.441, 336.362, 235.531, 293.002);
  ctx.bezierCurveTo(310.627, 249.638, 336.363, 153.618, 293.002, 78.529);
  ctx.closePath();
  ctx.moveTo(219.834, 265.8);
  ctx.bezierCurveTo(159.75900000000001, 300.492, 82.94, 279.906, 48.25800000000001, 219.827);
  ctx.bezierCurveTo(13.568, 159.76, 34.153, 82.933, 94.23, 48.259);
  ctx.bezierCurveTo(154.301, 13.569000000000003, 231.12400000000002, 34.153, 265.808, 94.22999999999999);
  ctx.bezierCurveTo(300.493, 154.305, 279.906, 231.115, 219.834, 265.8);
  ctx.closePath();
end;

procedure TWCButton.DrawPreviousButton;
begin
  zoom := ((ctx.Width - (2 * margin)) / 314);
  ctx.scale(zoom, zoom);
  ctx.moveTo(0, 0);
  ctx.lineTo(314.063, 0);
  ctx.lineTo(314.063, 314.063);
  ctx.lineTo(0, 314.063);
  ctx.closePath();
  ctx.clip();
  ctx.translate(0, 0);
  ctx.translate(0, 0);
  ctx.scale(1, 1);
  ctx.translate(0, 0);
  ctx.strokeStyle('rgba(0,0,0,0)');
  ctx.lineCap := 'butt';
  ctx.lineJoin := 'miter';
  ctx.miterLimit := 4;
  ctx.save();
  ctx.beginPath();
  ctx.moveTo(235.532, 21.063);
  ctx.bezierCurveTo(160.437, -22.289, 64.413, 3.431, 21.062, 78.53);
  ctx.bezierCurveTo(-22.294, 153.615, 3.43, 249.643, 78.527, 293.007);
  ctx.bezierCurveTo(153.614, 336.352, 249.64600000000002, 310.624, 293.003, 235.537);
  ctx.bezierCurveTo(336.363, 160.438, 310.619, 64.417, 235.532, 21.063);
  ctx.closePath();
  ctx.moveTo(265.8, 219.832);
  ctx.bezierCurveTo(231.11200000000002, 279.911, 154.29700000000003, 300.489, 94.226, 265.797);
  ctx.bezierCurveTo(34.157, 231.116, 13.564, 154.306, 48.249, 94.231);
  ctx.bezierCurveTo(82.932, 34.148999999999994, 159.756, 13.564999999999998, 219.827, 48.25599999999999);
  ctx.bezierCurveTo(279.898, 82.932, 300.484, 159.756, 265.8, 219.832);
  ctx.closePath();
  ctx.moveTo(198.551, 118.113);
  ctx.lineTo(156.88, 143.32);
  ctx.lineTo(156.96, 125.679);
  ctx.bezierCurveTo(156.99800000000002, 117.017, 151.007, 113.618, 143.597, 118.113);
  ctx.lineTo(92.01500000000001, 149.322);
  ctx.bezierCurveTo(84.59300000000002, 153.806, 84.60900000000001, 161.058, 92.05300000000001, 165.514);
  ctx.lineTo(143.156, 196.11100000000002);
  ctx.bezierCurveTo(150.594, 200.56300000000002, 156.661, 197.15300000000002, 156.699, 188.473);
  ctx.lineTo(156.77900000000002, 171.353);
  ctx.lineTo(198.11400000000003, 196.103);
  ctx.bezierCurveTo(205.55200000000002, 200.555, 211.62300000000005, 197.137, 211.65900000000002, 188.465);
  ctx.lineTo(211.92800000000003, 125.662);
  ctx.bezierCurveTo(211.952, 117.009, 205.958, 113.619, 198.551, 118.113);
  ctx.closePath();
end;

procedure TWCButton.DrawShuffleButton;
begin
  zoom := ((ctx.Width - (2 * margin)) / 124.7);
  ctx.scale(zoom, zoom);
  ctx.beginPath();
  ctx.moveTo(0, 0);
  ctx.lineTo(124.701, 0);
  ctx.lineTo(124.701, 124.701);
  ctx.lineTo(0, 124.701);
  ctx.closePath();
  ctx.clip();
  ctx.translate(0, 0);
  ctx.translate(0, 0);
  ctx.scale(1, 1);
  ctx.translate(0, 0);
  ctx.strokeStyle('rgba(0,0,0,0)');
  ctx.lineCap := 'butt';
  ctx.lineJoin := 'miter';
  ctx.miterLimit := 4;
  ctx.save();
  ctx.beginPath();
  ctx.moveTo(0, 30.805);
  ctx.bezierCurveTo(0, 30.801, 0, 30.798, 0, 30.789);
  ctx.bezierCurveTo(0, 30.786, 0, 30.782, 0, 30.779);
  ctx.lineTo(0, 30.805);
  ctx.closePath();
  ctx.moveTo(118.277, 79.685);
  ctx.bezierCurveTo(117.339, 75.934, 114.369, 75.10300000000001, 111.631, 77.84400000000001);
  ctx.lineTo(107.06, 82.412);
  ctx.lineTo(87.008, 62.338);
  ctx.lineTo(107.072, 42.274);
  ctx.lineTo(111.644, 46.846000000000004);
  ctx.bezierCurveTo(114.381, 49.584, 117.35900000000001, 48.757000000000005, 118.29, 45.003);
  ctx.lineTo(124.491, 20.095);
  ctx.bezierCurveTo(125.425, 16.339, 123.136, 14.046, 119.378, 14.982);
  ctx.lineTo(94.44800000000001, 21.171);
  ctx.bezierCurveTo(90.69000000000001, 22.102, 89.858, 25.073999999999998, 92.599, 27.816);
  ctx.lineTo(97.161, 32.370999999999995);
  ctx.lineTo(77.105, 52.426);
  ctx.lineTo(49.103, 24.392);
  ctx.lineTo(49.103, 23.797);
  ctx.lineTo(48.514, 23.797);
  ctx.lineTo(48.478, 23.761);
  ctx.lineTo(48.443000000000005, 23.797);
  ctx.lineTo(7.009, 23.797);
  ctx.bezierCurveTo(3.14, 23.797, 0.007000000000000561, 26.928, 0, 30.79);
  ctx.bezierCurveTo(0.007, 34.659, 3.14, 37.785, 7.009, 37.785);
  ctx.lineTo(42.594, 37.785);
  ctx.lineTo(67.153, 62.376);
  ctx.lineTo(42.654, 86.872);
  ctx.lineTo(7.009, 86.872);
  ctx.bezierCurveTo(3.14, 86.872, 0, 90.01, 0, 93.88);
  ctx.lineTo(0, 93.884);
  ctx.bezierCurveTo(0, 97.75, 3.14, 100.892, 7.009, 100.892);
  ctx.lineTo(49.06, 100.892);
  ctx.lineTo(49.06, 100.28399999999999);
  ctx.lineTo(77.053, 72.294);
  ctx.lineTo(97.104, 92.365);
  ctx.lineTo(92.58, 96.886);
  ctx.bezierCurveTo(89.846, 99.62299999999999, 90.667, 102.597, 94.425, 103.532);
  ctx.lineTo(119.365, 109.72);
  ctx.bezierCurveTo(123.119, 110.655, 125.41199999999999, 108.365, 124.478, 104.608);
  ctx.lineTo(118.277, 79.685);
  ctx.closePath();
end;

procedure TWCButton.DrawLeftButton;
begin
  zoom := ((ctx.Width - (2 * margin)) / 314);
  ctx.scale(zoom, zoom);
  ctx.beginPath();
  ctx.moveTo(0, 0);
  ctx.lineTo(314.069, 0);
  ctx.lineTo(314.069, 314.069);
  ctx.lineTo(0, 314.069);
  ctx.closePath();
  ctx.clip();
  ctx.translate(0, 0);
  ctx.translate(0, 0);
  ctx.scale(1, 1);
  ctx.translate(0, 0);
  ctx.strokeStyle('rgba(0,0,0,0)');
  ctx.lineCap := 'butt';
  ctx.lineJoin := 'miter';
  ctx.miterLimit := 4;
  ctx.save();
  ctx.beginPath();
  ctx.moveTo(293.004, 78.525);
  ctx.bezierCurveTo(249.64, 3.436, 153.62, -22.295, 78.531, 21.061);
  ctx.bezierCurveTo(3.436, 64.411, -22.296, 160.443, 21.068, 235.542);
  ctx.bezierCurveTo(64.418, 310.629, 160.443, 336.36400000000003, 235.53300000000002, 293.009);
  ctx.bezierCurveTo(310.629, 249.648, 336.365, 153.621, 293.004, 78.525);
  ctx.closePath();
  ctx.moveTo(219.836, 265.802);
  ctx.bezierCurveTo(159.76100000000002, 300.487, 82.94200000000001, 279.916, 48.26000000000002, 219.83300000000003);
  ctx.bezierCurveTo(13.57, 159.762, 34.155, 82.936, 94.232, 48.253);
  ctx.bezierCurveTo(154.303, 13.57, 231.126, 34.153999999999996, 265.81, 94.232);
  ctx.bezierCurveTo(300.495, 154.308, 279.908, 231.118, 219.836, 265.802);
  ctx.closePath();
  ctx.moveTo(211.986, 141.328);
  ctx.lineTo(146.495, 141.328);
  ctx.lineTo(164.094, 123.725);
  ctx.bezierCurveTo(170.218, 117.59599999999999, 170.218, 107.649, 164.094, 101.52799999999999);
  ctx.bezierCurveTo(157.965, 95.395, 148.016, 95.395, 141.887, 101.52799999999999);
  ctx.lineTo(97.485, 145.928);
  ctx.bezierCurveTo(91.356, 152.059, 91.356, 162.006, 97.485, 168.141);
  ctx.lineTo(141.887, 212.543);
  ctx.bezierCurveTo(148.016, 218.671, 157.965, 218.671, 164.094, 212.543);
  ctx.bezierCurveTo(170.218, 206.412, 170.218, 196.466, 164.094, 190.342);
  ctx.lineTo(146.488, 172.741);
  ctx.lineTo(211.987, 172.741);
  ctx.bezierCurveTo(220.656, 172.741, 227.684, 165.70000000000002, 227.684, 157.04000000000002);
  ctx.lineTo(227.684, 157.032);
  ctx.bezierCurveTo(227.683, 148.353, 220.655, 141.328, 211.986, 141.328);
  ctx.closePath();
end;

procedure TWCButton.DrawRepeatButton;
begin
  zoom := ((ctx.Width - (2 * margin)) / 140);
  ctx.scale(zoom, zoom);
  ctx.beginPath();
  ctx.moveTo(0, 0);
  ctx.lineTo(140.171, 0);
  ctx.lineTo(140.171, 140.171);
  ctx.lineTo(0, 140.171);
  ctx.closePath();
  ctx.clip();
  ctx.translate(0, 0);
  ctx.translate(0, 0);
  ctx.scale(1, 1);
  ctx.translate(0, 0);
  ctx.strokeStyle('rgba(0,0,0,0)');
  ctx.lineCap := 'butt';
  ctx.lineJoin := 'miter';
  ctx.miterLimit := 4;
  ctx.save();
  ctx.beginPath();
  ctx.moveTo(31.987, 90.618);
  ctx.bezierCurveTo(20.939, 85.49799999999999, 14.017, 78.181, 14.017, 70.054);
  ctx.bezierCurveTo(14.017, 56.999, 31.899, 46.05800000000001, 56.068, 42.944);
  ctx.lineTo(56.068, 28.848);
  ctx.bezierCurveTo(24.078, 32.746, 0, 49.713, 0, 70.054);
  ctx.bezierCurveTo(0, 81.783, 8.021, 92.357, 20.925, 99.974);
  ctx.bezierCurveTo(35.043, 108.604, 42.051, 94.588, 31.987, 90.618);
  ctx.closePath();
  ctx.moveTo(116.155, 38.416);
  ctx.bezierCurveTo(108.634, 31.509999999999998, 98.12, 42.022999999999996, 105.29, 48.238);
  ctx.bezierCurveTo(118.01400000000001, 53.378, 126.155, 61.242, 126.155, 70.054);
  ctx.bezierCurveTo(126.155, 83.116, 108.27, 94.058, 84.10300000000001, 97.171);
  ctx.lineTo(84.10300000000001, 111.26100000000001);
  ctx.bezierCurveTo(116.093, 107.36900000000001, 140.171, 90.403, 140.171, 70.054);
  ctx.bezierCurveTo(140.171, 57.423, 130.853, 46.116, 116.155, 38.416);
  ctx.closePath();
  ctx.moveTo(62.261, 52.461);
  ctx.lineTo(85.075, 38.8);
  ctx.bezierCurveTo(88.394, 36.812, 88.401, 33.574, 85.093, 31.571999999999996);
  ctx.lineTo(62.068, 17.637);
  ctx.bezierCurveTo(58.756, 15.638, 56.083, 17.148, 56.099, 21.018);
  ctx.lineTo(56.223, 49.052);
  ctx.bezierCurveTo(56.233, 52.923, 58.938, 54.449, 62.261, 52.461);
  ctx.closePath();
  ctx.moveTo(78.104, 87.723);
  ctx.lineTo(55.08, 101.657);
  ctx.bezierCurveTo(51.769, 103.65899999999999, 51.775999999999996, 106.89699999999999, 55.099, 108.886);
  ctx.lineTo(77.91, 122.547);
  ctx.bezierCurveTo(81.229, 124.53099999999999, 83.94, 123.009, 83.957, 119.13499999999999);
  ctx.lineTo(84.077, 91.101);
  ctx.bezierCurveTo(84.089, 87.233, 81.413, 85.725, 78.104, 87.723);
  ctx.closePath();
end;

procedure TWCButton.DrawVolumeButton;
begin
  zoom := ((ctx.Width - (2 * margin)) / 282.6);
  ctx.scale(zoom, zoom);
  ctx.beginPath();
  ctx.moveTo(0, 0);
  ctx.lineTo(282.611, 0);
  ctx.lineTo(282.611, 282.612);
  ctx.lineTo(0, 282.612);
  ctx.closePath();
  ctx.clip();
  ctx.translate(0, 0);
  ctx.translate(0, 0);
  ctx.scale(1, 1);
  ctx.translate(0, 0);
  ctx.strokeStyle('rgba(0,0,0,0)');
  ctx.lineCap := 'butt';
  ctx.lineJoin := 'miter';
  ctx.miterLimit := 4;
  ctx.save();
  ctx.beginPath();
  ctx.moveTo(31.4, 180.553);
  ctx.lineTo(15.701, 180.553);
  ctx.bezierCurveTo(7.033, 180.553, 0, 187.583, 0, 196.254);
  ctx.lineTo(0, 227.656);
  ctx.bezierCurveTo(0, 236.324, 7.033, 243.357, 15.701, 243.357);
  ctx.lineTo(31.4, 243.357);
  ctx.bezierCurveTo(40.068, 243.357, 47.101, 236.324, 47.101, 227.656);
  ctx.lineTo(47.101, 196.25400000000002);
  ctx.bezierCurveTo(47.101, 187.583, 40.068, 180.553, 31.4, 180.553);
  ctx.closePath();
  ctx.moveTo(109.905, 133.45);
  ctx.lineTo(94.204, 133.45);
  ctx.bezierCurveTo(85.536, 133.45, 78.50299999999999, 140.48499999999999, 78.50299999999999, 149.15099999999998);
  ctx.lineTo(78.50299999999999, 227.65699999999998);
  ctx.bezierCurveTo(78.50299999999999, 236.325, 85.53599999999999, 243.35799999999998, 94.20399999999998, 243.35799999999998);
  ctx.lineTo(109.90499999999997, 243.35799999999998);
  ctx.bezierCurveTo(118.57099999999997, 243.35799999999998, 125.60399999999997, 236.325, 125.60399999999997, 227.65699999999998);
  ctx.lineTo(125.60399999999997, 149.15099999999998);
  ctx.bezierCurveTo(125.604, 140.485, 118.571, 133.45, 109.905, 133.45);
  ctx.closePath();
  ctx.moveTo(188.409, 86.349);
  ctx.lineTo(172.708, 86.349);
  ctx.bezierCurveTo(164.044, 86.349, 157.007, 93.382, 157.007, 102.05000000000001);
  ctx.lineTo(157.007, 227.657);
  ctx.bezierCurveTo(157.007, 236.32500000000002, 164.044, 243.358, 172.708, 243.358);
  ctx.lineTo(188.409, 243.358);
  ctx.bezierCurveTo(197.07299999999998, 243.358, 204.10999999999999, 236.32500000000002, 204.10999999999999, 227.657);
  ctx.lineTo(204.10999999999999, 102.05);
  ctx.bezierCurveTo(204.11, 93.382, 197.073, 86.349, 188.409, 86.349);
  ctx.closePath();
  ctx.moveTo(266.91, 39.253);
  ctx.lineTo(251.20900000000003, 39.253);
  ctx.bezierCurveTo(242.54900000000004, 39.253, 235.51200000000003, 46.28, 235.51200000000003, 54.954);
  ctx.lineTo(235.51200000000003, 227.656);
  ctx.bezierCurveTo(235.51200000000003, 236.324, 242.54900000000004, 243.357, 251.20900000000003, 243.357);
  ctx.lineTo(266.91, 243.357);
  ctx.bezierCurveTo(275.574, 243.357, 282.61100000000005, 236.324, 282.61100000000005, 227.656);
  ctx.lineTo(282.61100000000005, 54.955);
  ctx.bezierCurveTo(282.611, 46.28, 275.574, 39.253, 266.91, 39.253);
  ctx.closePath();
end;

end.

