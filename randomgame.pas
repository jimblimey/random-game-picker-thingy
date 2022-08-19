program randomgame;
{$mode objfpc}{$H+}
uses Classes, SysUtils, fgl, math, StrUtils, LCLIntF, httpsend, ssl_openssl3,
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
  TIntegerList = specialize TFPGList<Integer>;
  
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

procedure DownloadFile(url: String);
var
  fo: TFileStream;
begin
  writeln(url);
  with THTTPSend.Create do
  begin
    if HTTPMethod('GET',url) then
    try
      fo := TFileStream.Create('/tmp/tmp.zip',fmCreate);
      writeln(Document.Size);
      fo.CopyFrom(Document,Document.Size);
      fo.Free;
    except
      writeln('poo');
    end;
    Free;
  end;
end;

procedure main;
var
  fi: TStrings;
  UsedIDs: TIntegerList;
  games: TList;
  st: TStringArray;
  gp: PGameEntry;
  i,j,r: Integer;
  running: Boolean;
  c: Char;
  s: String;
  keys: set of Char = ['1','Q',' '];
begin
  UsedIDs := TIntegerList.Create;
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
    Cls(CYAN);
    CentreText(0,'JIM BLIMEY''S',BLUE);
    CentreText(1,'RANDOM',YELLOW);
    CentreText(2,'GAME',RED);
    CentreText(3,'PICKER THINGY',MAGENTA);
    CentreText(10,'PRESS SPACE TO PICK A GAME', BLACK);
    c := #0;
    while c <> ' ' do
    begin
      c := InkeyW;
    end;
    CentreText(10,Padleft('',30),CYAN);
    r := Random(fi.Count);
    j := 0;
    repeat
      for i := 0 to UsedIDs.Count -1 do
      begin
        if r = UsedIDs[i] then j := 1;
      end;
    until j = 0;
    UsedIDs.Add(r);
    for i := 0 to 99 do
    begin
      CentreText(10,RandomString(14),Random(15));
      UpdateScreen;
      sleep(10);
    end;
    CentreText(10,Padleft('',16),CYAN);
    gp := games[r];
    CentreText(10,gp^.title,RED);
    CentreText(11,gp^.genre,BLUE);
    CentreText(13,'1 TO DOWNLOAD',WHITE);
    CentreText(22,'PRESS SPACE TO PICK AGAIN', BLACK);
    CentreText(23,'OR Q TO QUIT',BLACK);
    c := #0;
    while not (c in keys) do c := InkeyW;
    if Uppercase(c) = 'Q' then running := false;
    if c = '1' then
    begin
      //OpenURL(BASEURL + IntToStr(gp^.id));
      DownloadFile(BASEURL + gp^.filelink);
    end;
  end;

  fi.Free;
  UsedIDs.Free;
  games.Free;
end;

begin
  Randomize;
  InitialiseWindow('Jim Blimey''s Random Game Picker Thingy');
  main;
  CloseWindow;
end.
