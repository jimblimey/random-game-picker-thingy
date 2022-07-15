unit zxgfxw;
{$mode objfpc}{$H+}

interface

uses SysUtils, Classes, ptc, crt;

type
  ZXPaperData = array of array of Uint32;
  ZXEvent = IPTCEvent;
  ZXKeyEvent = IPTCKeyEvent;
  ZXTimer = IPTCTimer;
  ZXPaper = record
    top: Integer;
    left: Integer;
    bottom: Integer;
    right: Integer;
    width: Integer;
    height: Integer;
  end;

var
  console: IPTCConsole;
  surface: IPTCSurface;
  format: IPTCFormat;
  paper: IPTCArea;
  ZXColours: array[0..14] of IPTCColor;
  WinScale: Integer = 2;
  PaperDimensions: ZXPaper;
  LastKey: IPTCKeyEvent; 
  CurrentPaper: Integer;
  TextCursor: array[0..7] of Byte;
  ZXAnyEvent: TPTCEventMask;
  g1: array[0..7] of Byte = (15, 15, 15, 15, 0, 0, 0, 0);
  g2: array[0..7] of Byte = (240, 240, 240, 240, 0, 0, 0, 0);
  g3: array[0..7] of Byte = (255, 255, 255, 255, 0, 0, 0, 0);
  g4: array[0..7] of Byte = (0, 0, 0, 0, 15, 15, 15, 15);
  g5: array[0..7] of Byte = (15, 15, 15, 15, 15, 15, 15, 15);
  g6: array[0..7] of Byte = (240, 240, 240, 240, 15, 15, 15, 15);
  g7: array[0..7] of Byte = (255, 255, 255, 255, 15, 15, 15, 15);
  g8: array[0..7] of Byte = (0, 0, 0, 0, 0, 0, 0, 0);
  g9: array[0..7] of Byte = (255, 255, 255, 255, 255, 255, 255, 255);
  g10: array[0..7] of Byte = (170, 85, 170, 85, 170, 85, 170, 85); 
{$I specfont.inc}

const
  WINWIDTH = 640;
  WINHEIGHT = 480;
  ZXLEFT = PTCKEY_LEFT;
  ZXRIGHT = PTCKEY_RIGHT;
  ZXUP = PTCKEY_UP;
  ZXDOWN = PTCKEY_DOWN;
  BLACK = 0;
  BLUE = 1;
  RED = 2;
  MAGENTA = 3;
  GREEN = 4;
  CYAN = 5;
  YELLOW = 6;
  WHITE = 7;
  BRIGHTBLUE = 8;
  BRIGHTRED = 9;
  BRIGHTMAGENTA = 10;
  BRIGHTGREEN = 11;
  BRIGHTCYAN = 12;
  BRIGHTYELLOW = 13;
  BRIGHTWHITE = 14;

procedure SaveDebug(s: String);
function GetBit(a: Byte; b: Byte): Byte;
function RGB(cl: IPTCColor): Uint32;
procedure UpdateScreen;
procedure SaveScreen(t: Integer; l: Integer; w: Integer; h: Integer; var screendata: ZXPaperData);
procedure RestoreScreen(t: Integer; l: Integer; w: Integer; h: Integer; screendata: ZXPaperData);
procedure ClearArea(x: Integer; y: Integer; w: Integer; h: Integer; cl: Integer);
procedure Plot(x, y: Integer; colour: Uint32);
procedure DrawLine(x0: Integer; y0: Integer; x1: Integer; y1: Integer; cl: Integer; d: integer = 0);
procedure DrawCircle(xc: Integer; yc: Integer; r: Integer; cl: Integer);
procedure DrawGraphic(x: Integer; y: Integer; w: Integer; h: Integer; cl: Integer; gdata: array of Byte; over: Boolean = false; inv: Boolean = false);
procedure DrawGraphicI(x: Integer; y: Integer; w: Integer; h: Integer; gdata: array of Byte);
procedure PrintAt(x: Integer; y: Integer; s: String; cl: Integer; over: Boolean = true; inv: Boolean = false);
procedure Cls(c: Integer);
procedure Border(c: Integer);
function Input(x: Integer; y: Integer; cl: Integer; showcursor: Boolean = true): String;
function Inkey: Char;
function InkeyW: Char;
procedure InitialiseColours;
procedure SetupPaper;
procedure InitialiseWindow(wtitle: String);
procedure CloseWindow;
function CreateTimer: ZXTimer;

implementation

procedure SaveDebug(s: String);
var
  fo: TStrings;
begin
  fo := TStringList.Create;
  if FileExists('debug.txt') then fo.LoadFromFile('debug.txt');
  fo.Add(s);
  fo.SaveToFile('debug.txt');
  fo.Free;
end;

function GetBit(a: Byte; b: Byte): Byte;
begin
  Result := (a shr b) and 1;
end;

function RGB(cl: IPTCColor): Uint32;
var
  r,g,b: Integer;
begin
  r := round(cl.R * 255);
  g := round(cl.G * 255);
  b := round(cl.B * 255);
  Result := (r shl 16) or (g shl 8) or b;
end;

procedure UpdateScreen;
begin
  surface.copy(console);
  console.update;
end;

procedure SaveScreen(t: Integer; l: Integer; w: Integer; h: Integer; var screendata: ZXPaperData);
var
  x,y: Integer;
  pixels: PUint32;
begin
  SetLength(screendata,w*WinScale,h*WinScale);
  pixels := surface.lock;
  for x := l*WinScale to w*WinScale-1 do
  begin
    for y := t*WinScale to h*WinScale-1 do
    begin
      screendata[x,y] := pixels[(x+PaperDimensions.left) + (y+PaperDimensions.top) * surface.width];
    end;
  end;
  surface.unlock;
end;

procedure RestoreScreen(t: Integer; l: Integer; w: Integer; h: Integer; screendata: ZXPaperData);
var
  x,y: Integer;
  pixels: PUint32;
begin
  pixels := surface.lock;
  for x := l*WinScale to w*WinScale-1 do
  begin
    for y := t*WinScale to h*WinScale-1 do
    begin
      pixels[(x+PaperDimensions.left) + (y+PaperDimensions.top) * surface.width] := screendata[x,y];
    end;
  end;
  surface.unlock;
  UpdateScreen;  
end;

procedure ClearArea(x: Integer; y: Integer; w: Integer; h: Integer; cl: Integer);
var
  i,j: Integer;
begin
  for i := x to x+w do
  begin
    for j := y to y+h do
    begin
      Plot(i,j,cl);
    end;
  end;
end;

procedure Plot(x, y: Integer; colour: Uint32);
var
  i,j: Integer;
  t,l: Integer;
  pixels: PUint32;
begin
  pixels := surface.lock;
  try
    t := y * WinScale;
    l := x * WinScale;
    for i := 0 to WinScale-1 do
    begin
      for j := 0 to WinScale-1 do
      begin
        pixels[(l+PaperDimensions.left+i) + (t+PaperDimensions.top+j) * surface.width] := RGB(ZXColours[colour]);
      end;
    end;
  finally
    surface.unlock;
  end;
end;

procedure DrawLine(x0: Integer; y0: Integer; x1: Integer; y1: Integer; cl: Integer; d: integer = 0);
var
  sx, sy, err, e2, dx, dy: Integer;
  i,j: Integer;
  pixels: PUint32;
  t,l: Integer;
begin
  x0 := x0 * WinScale;
  x1 := x1 * WinScale;
  y0 := y0 * WinScale;
  y1 := y1 * WinScale;
  dx := abs(x1-x0);
  if x0 < x1 then sx := 1*WinScale
  else sx := -1*WinScale;  
  dy := -abs(y1-y0);
  if y0 < y1 then sy := 1*WinScale
  else sy := -1*WinScale;
  err := dx+dy;
  pixels := surface.lock;
  while true do
  begin    
    for i := 0 to WinScale-1 do
    begin
      for j := 0 to WinScale-1 do
      begin
        t := y0+PaperDimensions.top+j;
        l := x0+PaperDimensions.left+i;
        pixels[l + t * surface.width] := RGB(ZXColours[cl]);
      end;
    end;
    if d > 0 then
    begin
      UpdateScreen;
      //sleep(d);
      delay(d);
    end;
    if (x0 = x1) and (y0 = y1) then break;
    e2 := 2*err;
    if e2 >= dy then
    begin
      err += dy;
      x0 += sx;
    end;
    if e2 <= dx then
    begin
      err += dx;
      y0 += sy;
    end;
  end;
  surface.unlock;
  //UpdateScreen;
end;

procedure DrawCircle(xc: Integer; yc: Integer; r: Integer; cl: Integer);
  procedure _DC(xc: Integer; yc: Integer; x: Integer; y: Integer; cl: Integer; var pixels: PUint32);
  var
    i,j: Integer;    
    x1,y1: Integer;
  begin
    
    for i := 0 to WinScale-1 do
    begin
      for j := 0 to WinScale-1 do
      begin  
        x1 := xc+x+i;
        y1 := yc+y+j;
        pixels[x1 + y1 * surface.width] := RGB(ZXColours[cl]);
        x1 := xc-x+i;
        y1 := yc+y+j;
        pixels[x1 + y1 * surface.width] := RGB(ZXColours[cl]);
        x1 := xc+x+i;
        y1 := yc-y+j;
        pixels[x1 + y1 * surface.width] := RGB(ZXColours[cl]);
        x1 := xc-x+i;
        y1 := yc-y+j;
        pixels[x1 + y1 * surface.width] := RGB(ZXColours[cl]);
        x1 := xc+y+i;
        y1 := yc+x+j;
        pixels[x1 + y1 * surface.width] := RGB(ZXColours[cl]);
        x1 := xc-y+i;
        y1 := yc+x+j;
        pixels[x1 + y1 * surface.width] := RGB(ZXColours[cl]);
        x1 := xc+y+i;
        y1 := yc-x+j;
        pixels[x1 + y1 * surface.width] := RGB(ZXColours[cl]);
        x1 := xc-y+i;
        y1 := yc-x+j;
        pixels[x1 + y1 * surface.width] := RGB(ZXColours[cl]);
      end;
    end;         
  end;
var
  x,y,d: Integer;
  pixels: PUint32;
begin
  pixels := surface.lock;
  x := 0;
  y := r * WinScale;
  d := 3 - 2 * r;
  _DC(PaperDimensions.left+xc,PaperDimensions.top+yc,x,y,cl,pixels);
  while y >= x do
  begin
    inc(x,WinScale);
    if d > 0 then
    begin
      dec(y,WinScale);
      d := d + 4 * (x - y) + 10;
    end
    else d := d + 4 * x + 6;
    _DC(PaperDimensions.left+xc,PaperDimensions.top+yc,x,y,cl,pixels);
  end;  
  surface.unlock;
  //UpdateScreen;
end;

procedure DrawGraphic(x: Integer; y: Integer; w: Integer; h: Integer; cl: Integer; gdata: array of Byte; over: Boolean = false; inv: Boolean = false);
  procedure PlotRow(l: Integer; t: Integer; d: Byte; cl: Integer; inv: Boolean; var p: PUint32);
  var
    z, x1, y1, i, j: Integer;
    b: Byte;
  begin
    if inv then b := 0
    else b := 1;
    for z := 0 to 7 do
    begin
      if GetBit(d,7-z) = b then
      begin
        for i := 0 to WinScale-1 do
        begin
          for j := 0 to WinScale-1 do
          begin
            x1 := l+(z*WinScale)+i;
            y1 := t+j;
            p[x1 + y1 * surface.width] := RGB(ZXColours[cl]);
          end;
        end;
      end
      else
      begin
        if over then
        begin
        for i := 0 to WinScale-1 do
        begin
          for j := 0 to WinScale-1 do
          begin
            x1 := l+(z*WinScale)+i;
            y1 := t+j;
            p[x1 + y1 * surface.width] := RGB(ZXColours[CurrentPaper]);
          end;
        end;        
        end;
      end;
    end;  
  end;
var
  c,l,t: Integer;
  i,j,z: Integer;
  pixels: PUint32;
begin
  if (x < 0) or (x + (w*8) > 255) or (y < 0) or (y + (h*8) > 192) then exit;
  pixels := surface.lock;  
  z := 0;
  for i := 0 to w -1 do
  begin
    l := (x*WinScale)+PaperDimensions.left+(i*(8*WinScale));  
    for j := 0 to h -1 do
    begin
      t := (y*WinScale)+PaperDimensions.top+(j*(8*WinScale));  
      for c := 0 to 7 do
      begin      
        PlotRow(l, t+(c*WinScale), gdata[z+c], cl, inv, pixels);
      end;
      inc(z,8);
    end;
  end;
  surface.unlock;
end;

procedure DrawGraphicI(x: Integer; y: Integer; w: Integer; h: Integer; gdata: array of Byte);
var
  i,j,l,t,c,f,g: Integer;
  pixels: PUint32;
begin
  c := 0;
  pixels := surface.lock;
  for i := 0 to w -1 do
  begin
    l := (x*WinScale)+PaperDimensions.left+(i*WinScale);
    for j := 0 to h-1 do
    begin
      t := (y*WinScale)+PaperDimensions.top+(j*WinScale);
      for f := 0 to WinScale-1 do
      begin
        for g := 0 to WinScale-1 do
        begin
         //PutPixel(l+f,t+g,gdata[c]);
         pixels[(l+f) + (t+g) * surface.width] := RGB(ZXColours[gdata[c]]);
        end;
      end;
      inc(c);
    end;
  end;
  surface.unlock;
end;

procedure PrintAt(x: Integer; y: Integer; s: String; cl: Integer; over: Boolean = true; inv: Boolean = false);
var
  i,j,t,l: Integer;
  tmp: array[0..7] of Byte;
begin
  l := x * 8;
  t := y * 8;
  for i := 1 to Length(s) do
  begin
    DrawGraphic(l,t,1,1,cl,ZXFont[Ord(s[i])], over);
    inc(l,8);
  end;
  UpdateScreen;
end;

procedure Cls(c: Integer);
begin
  CurrentPaper := c;
  surface.clear(ZXColours[c], paper);
  surface.copy(console);
  console.update;
end;

procedure Border(c: Integer);
var
  sc: ZXPaperData;
begin
  SaveScreen(0,0,256,192,sc);
  surface.clear(ZXColours[c],surface.Area);
  surface.copy(console);
  RestoreScreen(0,0,256,192,sc);
  console.update;
end;

function Input(x: Integer; y: Integer; cl: Integer; showcursor: Boolean = true): String;
var
  l: Integer;
  s: String;
begin
  l := x;
  s := '';
  repeat
    console.ReadKey(LastKey);
    if (LastKey.Code >= 32) and (LastKey.Code <= 127) then 
    begin
      PrintAt(l,y,Chr(LastKey.Code),cl);
      if showcursor then PrintAt(l+1,y,'_',cl);
      s := s + Chr(LastKey.Code);
      inc(l);
    end;
    if (LastKey.Code = 8) and (Length(s) > 0) then
    begin
      dec(l);
      if showcursor then PrintAt(l,y,'_ ',cl)
      else PrintAt(l,y,' ',cl);
      s := Copy(s,1,Length(s)-1);
    end;
  until (LastKey.Code = 13) or (LastKey.Code = 10);
  PrintAt(l,y,' ',cl);
  Result := s;
end;

function Inkey: Char;
var
  key: IPTCKeyEvent;
begin
  Result := #0;
  if console.PeekKey(key) then
  begin
    if (key.Code >= 32) and (key.Code <= 127) then Result := Chr(key.Code)
    else Result := Char(key.Code);
  end;
end;

function InkeyW: Char;
var
  key: IPTCKeyEvent;
begin
  Result := #0;
  {if console.PeekKey(key) then
  begin
    if (key.Code >= 32) and (key.Code <= 127) then Result := Chr(key.Code)
    else Result := Char(key.Code);
  end;}
  console.ReadKey(key);
  if (key.Code >= 32) and (key.Code <= 127) then Result := Chr(key.Code)
  else Result := Char(key.Code);
end;

procedure InitialiseColours;
var
  f: single = $AA / 255;
  h: single = $55 / 255;
begin
  ZXColours[0] := TPTCColorFactory.CreateNew(0, 0, 0);
  ZXColours[1] := TPTCColorFactory.CreateNew(0, 0, 0.75);
  ZXColours[2] := TPTCColorFactory.CreateNew(0.75, 0, 0);
  ZXColours[3] := TPTCColorFactory.CreateNew(0.75, 0, 0.75);
  ZXColours[4] := TPTCColorFactory.CreateNew(0, 0.75, 0);
  ZXColours[5] := TPTCColorFactory.CreateNew(0, 0.75, 0.75);
  ZXColours[6] := TPTCColorFactory.CreateNew(0.75, 0.75, 0);
  ZXColours[7] := TPTCColorFactory.CreateNew(0.75, 0.75, 0.75);
  ZXColours[8] := TPTCColorFactory.CreateNew(0, 0, 1);
  ZXColours[9] := TPTCColorFactory.CreateNew(1, 0, 0);
  ZXColours[10] := TPTCColorFactory.CreateNew(1, 0, 1);
  ZXColours[11] := TPTCColorFactory.CreateNew(0, 1, 0);
  ZXColours[12] := TPTCColorFactory.CreateNew(0, 1, 1);
  ZXColours[13] := TPTCColorFactory.CreateNew(1, 1, 0);
  ZXColours[14] := TPTCColorFactory.CreateNew(1, 1, 1);
end;

procedure SetupPaper;
begin
  PaperDimensions.width := 256 * WinScale;
  PaperDimensions.height := 192 * WinScale;
  PaperDimensions.left := (WINWIDTH div 2) - (PaperDimensions.width div 2);
  PaperDimensions.top := (WINHEIGHT div 2) - (PaperDimensions.height div 2);
  PaperDimensions.right := PaperDimensions.left + PaperDimensions.width;
  PaperDimensions.bottom := PaperDimensions.top + PaperDimensions.height;  
end;

procedure InitialiseWindow(wtitle: String);
var
  i: Integer;
begin
  ZXAnyEvent := PTCAnyEvent;
  for i := 0 to 6 do TextCursor[i] := 0;
  TextCursor[7] := 255;
  InitialiseColours;
  console := TPTCConsoleFactory.CreateNew;
  console.option('DirectX');  // this is required!
  console.option('windowed output');
  console.option('default width 640');
  console.option('default height 480');
  console.option('fixed window');
  console.option('frequency 60');
  format := TPTCFormatFactory.CreateNew(32, $00FF0000, $0000FF00, $000000FF);
  //format := TPTCFormatFactory.CreateNew(8);
  console.open(wtitle, WINWIDTH, WINHEIGHT, format);
  surface := TPTCSurfaceFactory.CreateNew(WINWIDTH, WINHEIGHT, format);
  SetupPaper;
  paper := TPTCAreaFactory.CreateNew(PaperDimensions.left, PaperDimensions.top, PaperDimensions.right, PaperDimensions.bottom);
  Border(7);
  Cls(7);
end;

procedure CloseWindow;
begin
  console.close;
end;

function CreateTimer: ZXTimer;
begin
  Result := TPTCTimerFactory.CreateNew;
end;

end.
