program randomgame;
{$mode objfpc}{$H+}
uses Classes, SysUtils, math, StrUtils, LCLIntF, IniFiles, opensslsockets,
  ssockets, sslsockets, fphttpclient, Zipper, zxgfxw;

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

var
  history: TStrings;
  TapeDir: String;
  ConfigDir: String;
  // ~/Library/Application Support/MyApp/
{$I 4x8font.inc}
  
const
  BASEURL = 'https://spectrumcomputing.co.uk/';
  ZXDBUPD = '1.0.157';

procedure PrintAt64(x: Integer; y: Integer; s: String; cl: Integer);
var
  i,t,l: Integer;
begin
  l := x * 4;
  t := y * 8;
  for i := 1 to Length(s) do
  begin
    DrawGraphic(l, t, 1, 1, cl, font64[Ord(s[i])], true);
    inc(l,4);
  end;
  UpdateScreen;
end;

procedure CentreText(y: Integer; s: String; cl: Integer);
var
  x: Integer;
begin
  x := 16-(Length(s) div 2);
  PrintAt(x,y,s,cl);
end;

procedure CentreText64(y: Integer; s: String; cl: Integer);
var
  x: Integer;
begin
  x := 32-(Length(s) div 2);
  PrintAt64(x,y,s,cl);
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
  uz.OutputPath := TapeDir;
  uz.Examine;
  uz.UnZipAllFiles;
  uz.Free;
end;

function CheckHistory(id: Integer): Boolean;
var
  i: Integer;
begin
  //Result := true;
  if id = 0 then
    Result := true
  else
  begin
    Result := true;
    for i := 0 to history.Count -1 do
    begin
      if StrToIntDef(history[i],0) = id then Result := false;
    end;
  end;
end;

procedure AddHistory(id: Integer);
begin
  if CheckHistory(id) then
  begin
    history.Add(id.ToString);
    history.SaveToFile(ConfigDir+'history.txt');
  end;
end;

procedure DownloadFile(url: String);
var
  http: TFPHttpClient;
begin
  Cls(5);
  writeln(url);
  PrintAt64(0,0,'Downloading '+ExtractFilename(url)+'...',BRIGHTGREEN);

  http := TFPHttpClient.Create(nil);
  http.RequestHeaders.Add('User-Agent: Mozilla/5.0 (X11; FreeBSD amd64; rv:103.0) Engine:Blink Firefox/103.0');
  http.Get(url,TapeDir+'tmp.zip');
  if (http.ResponseStatusCode >= 100) and (http.ResponseStatusCode <= 299) then
  begin
    ExtractFile(TapeDir+'tmp.zip');
    DeleteFile(TapeDir+'tmp.zip');
    CentreText(11,'OK!',BRIGHTGREEN);
  end
  else
  begin
    PrintAt64(0,2,http.ResponseStatusText,BRIGHTRED);
    CentreText(14,http.ResponseStatusCode.ToString,BLACK);
    CentreText(11,'Press a key',BLACK);
    InkeyW;
  end;
  http.Free;
end;

procedure SaveLog(s: String);
var
  fo: TStrings;
  fn: String;
  t: String;
begin
  fo := TStringList.Create;
  DateTimeToString(t,'yyyy-mm-dd',Now);
  fn := ConfigDir+'log-' + t + '.txt';
  if FileExists(fn) then fo.LoadFromFile(fn);
  fo.Add(s);
  fo.SaveToFile(fn);
  fo.Free;
end;

procedure ShowDatabaseOptions;
begin
  Border(0);
  Cls(0);
  CentreText(0,'DATABASE OPTIONS',GREEN);
  PrintAt(0,2,'(A) Start year       []',GREEN);
  PrintAt(0,3,'(B) End year         []',GREEN);
  UpdateScreen;
  InkeyW;
end;

procedure ShowOptions;
var
  c: Char;
begin
  while true do
  begin
    Border(WHITE);
    CLS(WHITE);
    CentreText(0,'PROGRAM OPTIONS',BLACK);

    PrintAt(0, 2,'(A) Border colour      [3]',BLACK);
    PrintAt(0, 3,'(B) Paper colour       [0]',BLACK);
    PrintAt(0, 4,'(C) Title 1 colour    [10]',BLACK);
    PrintAt(0, 5,'(D) Title 2 colour    [13]',BLACK);
    PrintAt(0, 6,'(E) Title 3 colour    [12]',BLACK);
    PrintAt(0, 7,'(F) Title 4 colour    [11]',BLACK);
    PrintAt(0, 8,'(G) Command colour    [15]',BLACK);
    PrintAt(0, 9,'(H) Game title colour  [5]',BLACK);
    PrintAt(0,10,'(I) Genre colour       [6]',BLACK);
    PrintAt(0,11,'(J) Year colour        [1]',BLACK);
    PrintAt(0,11,'(K) Game count colour [14]',BLACK);
    PrintAt(0,12,'(K) ZXDB date  colour  [4]',BLACK);
    PrintAt(0,14,'(M) Save tapes location', BLACK);
    PrintAt(0,15,'['+TapeDir+']',BLACK);
    PrintAt(0,23,'(X) Exit (2) Games options',BLACK,false,true);
    UpdateScreen;
    c := InkeyW;
    if c = 'X' then break;
    if c = '2' then ShowDatabaseOptions;
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
  s: String;
  keys: set of Char = ['1','Q',' ','O','D'];
begin
  if not FileExists('zxdbdump.txt') then
  begin
    Border(2);
    Cls(0);
    CentreText(0,'ERROR',BRIGHTRED);
    CentreText(11,'Database file not found!',BRIGHTWHITE);
    CentreText(23,'Press any key to exit',BRIGHTWHITE);
    c := InkeyW;
    exit;
  end;
  if FileExists(ConfigDir+'history.txt') then history.LoadFromFile(ConfigDir+'history.txt');
  games := TList.Create;
  fi := TStringList.Create;
  fi.LoadFromFile('zxdbdump.txt');
  r := 0;
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
    PrintAt64(0,23, fi.Count.ToString + ' games',BRIGHTYELLOW);
    s := 'ZXDB ver ' + ZXDBUPD;
    PrintAt64(63-Length(s),23,s,GREEN);
    c := #0;
    while c <> ' ' do
    begin
      if Uppercase(C) = 'Q' then exit;
      c := InkeyW;
    end;
    CentreText(10,Padleft('',30),BRIGHTCYAN);
    repeat
      for i := 0 to 99 do
      begin
        r := Random(fi.Count);
        CentreText(10,RandomString(14),Random(15));
        UpdateScreen;
        sleep(10);
      end;
    until CheckHistory(r);
    AddHistory(r);
    CentreText(10,Padleft('',16),BRIGHTCYAN);
    gp := games[r];
    if Length(gp^.title) < 32 then CentreText(10,gp^.title,CYAN)
    else CentreText64(10,gp^.title,CYAN);
    CentreText(11,gp^.genre,YELLOW);
    if gp^.year > 1900 then CentreText(12,gp^.year.ToString,BRIGHTBLUE);
    CentreText(18,'(D)ownload, (Q)uit or SPACE  ',BRIGHTWHITE);
    c := InkeyW;
    if Uppercase(c) = 'Q' then running := false;
    if Uppercase(c) = 'D' then
    begin
      DownloadFile(BASEURL + gp^.filelink);
      SaveLog(gp^.title);
      AddHistory(gp^.id);
    end;
    //if Uppercase(c) = 'O' then ShowOptions;
  end;

  fi.Free;
  games.Free;
end;

begin
  Randomize;
{$IFDEF Darwin}
  TapeDir := GetUserDir + 'RandomTapes/';
{$ELSE}
  TapeDir := '.' + PathDelim + 'tapes' + PathDelim;
{$ENDIF}
  ConfigDir := GetAppConfigDir(false);
  if not DirectoryExists(TapeDir) then mkdir(TapeDir);
  if not DirectoryExists(ConfigDir) then mkdir(ConfigDir);
  history := TStringList.Create;
  InitialiseWindow('Jim Blimey''s Random Game Picker Thingy');
  main;
  CloseWindow;
  history.Free;
end.
