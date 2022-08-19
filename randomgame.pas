program randomgame;
{$mode objfpc}{$H+}
uses Classes, SysUtils, math, StrUtils, LCLIntF, httpsend, ssl_openssl, Zipper,
  zxgfxw;

type
  PGameEntry = ^TGameEntry;
  TGameEntry = record
    id: Integer;
    title: String;
    publisher: String;
    year: Integer;
    genre: String;
    filelink: String;
    filetype: Integer;
  end;
  
const
  BASEURL = 'https://spectrumcomputing.co.uk/';

procedure CentreText(y: Integer; s: String; cl: Integer);
var
  x: Integer;
begin
  x := 16-(Length(s) div 2);
  PrintAt(x,y,s,cl);
end;

function RandomString(maxlen: Integer): String;
var
  i: Integer;
  c: Char;
begin
  Result := '';
  for i := 0 to maxlen do
  begin
    c := Chr(RandomRange(32,127));
    Result := Result + c;
  end;
end;

procedure ExtractFile(fname: String);
var
  uz: TUnZipper;
begin
  uz := TUnZipper.Create;
  uz.FileName := fname;
  uz.OutputPath := '.\tapes';
  uz.Examine;
  uz.UnZipAllFiles;
  uz.Free;
end;

procedure DownloadFile(url: String);
var
  http: THTTPSend;
begin
  http := THTTPSend.Create;
  try
    http.UserAgent := 'Mozilla/5.0 (X11; FreeBSD amd64; rv:103.0) Engine:Blink Firefox/103.0';
    http.HTTPMethod('GET', URL);
    if (http.ResultCode >= 100) and (http.ResultCode<=299) then
    begin
      http.Document.SaveToFile('tmp.zip');
      ExtractFile('tmp.zip');
      DeleteFile('tmp.zip');
    end;
  finally
    http.Free;
  end;
end;

procedure main;
var
  fi: TStrings;
  games: TList;
  st: TStringArray;
  gp: PGameEntry;
  i,r: Integer;
  running: Boolean;
  c: Char;
  keys: set of Char = ['1','Q',' '];
begin
  if not DirectoryExists('.\tapes') then mkdir('.\tapes');
  games := TList.Create;
  fi := TStringList.Create;
  fi.LoadFromFile('zxdbdump.txt');
  for i := 0 to fi.Count -1 do
  begin
    st := fi[i].Split('|');
    if High(st) = 6 then
    begin
      new(gp);
      gp^.id := StrToInt(st[0]);
      gp^.title := st[1];
      gp^.publisher := st[2];
      gp^.year := StrToIntDef(st[3],0);
      gp^.genre := st[4];
      gp^.filelink := st[5];
      gp^.filetype := StrToInt(st[6]);
      games.Add(gp);
    end;
  end;

  running := true;
  while running do
  begin
    Border(MAGENTA);
    Cls(BLACK);
    CentreText(0,'JIM BLIMEY''S',BRIGHTMAGENTA);
    CentreText(1,'RANDOM',BRIGHTYELLOW);
    CentreText(2,'GAME',BRIGHTCYAN);
    CentreText(3,'PICKER THINGY',BRIGHTGREEN);
    CentreText(10,'PRESS SPACE TO PICK A GAME', BRIGHTWHITE);
    c := #0;
    while c <> ' ' do
    begin
      c := InkeyW;
    end;
    CentreText(10,Padleft('',30),BRIGHTCYAN);
    r := Random(fi.Count);
    for i := 0 to 99 do
    begin
      CentreText(10,RandomString(14),Random(15));
      UpdateScreen;
      sleep(10);
    end;
    CentreText(10,Padleft('',16),BRIGHTCYAN);
    gp := games[r];
    CentreText(10,gp^.title,CYAN);
    CentreText(11,gp^.genre,YELLOW);
    CentreText(13,'1 TO DOWNLOAD',BRIGHTWHITE);
    CentreText(22,'PRESS SPACE TO PICK AGAIN', BRIGHTGREEN);
    CentreText(23,'OR Q TO QUIT',BRIGHTRED);
    c := #0;
    while not (c in keys) do c := InkeyW;
    if Uppercase(c) = 'Q' then running := false;
    if c = '1' then
    begin
      DownloadFile(BASEURL + gp^.filelink);
    end;
  end;

  fi.Free;
  games.Free;
end;

begin
  Randomize;
  InitialiseWindow('Jim Blimey''s Random Game Picker Thingy');
  main;
  CloseWindow;
end.
