unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, ExtCtrls, acs_audio, acs_file,
  {$IfDef WINDOWS}
  acs_stdaudio,
  {$Else}
  acs_alsaaudio,
  {$EndIf}
  About;

type

  { TRingThread }

  TRingThread = class(TThread)
  Player: TAcsAudioOut;
  SoundFile: TAcsFileIn;
  private
    BellsToRing: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(NumberOfBells: Integer; AudioOut: TAcsAudioOut; FileIn: TAcsFileIn);
  end;

  { TFormMain }

  TFormMain = class(TForm)
    AcsAudioOut: TAcsAudioOut;
    AcsFileIn: TAcsFileIn;
    ButtonQuit: TButton;
    ButtonAbout: TButton;
    LabelWatch: TLabel;
    LabelShipsBells: TLabel;
    MenuItemOpen: TMenuItem;
    MenuItemQuit: TMenuItem;
    PopupTrayMenu: TPopupMenu;
    TimerRing: TTimer;
    TrayIcon: TTrayIcon;
    procedure ButtonAboutClick(Sender: TObject);
    procedure ButtonQuitClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure MenuItemOpenClick(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemQuitClick(Sender: TObject);
    procedure TimerRingTimer(Sender: TObject);
    procedure TrayIconClick(Sender: TObject);
  private
    RingThread: TRingThread;
    function Bells: Integer;
    function GetInterval: Cardinal;
    function Watch: Integer;
    function WatchName: String;
    procedure CreateWavFiles;
    procedure DeleteWavFiles;
    procedure Ring(NumberOfBells: Integer);
    procedure LabelWatchUpdate;

  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

const
  BellNames: array of String = ('no?!?!?!', 'One', 'Two', 'Three', 'Four', 'Five', 'Six', 'Seven', 'Eight');

var
  TempDir: String;

{ TRingThread }

procedure TRingThread.Execute;
begin
  while BellsToRing > 0 do
  begin
    if BellsToRing > 1 then
      SoundFile.FileName := TempDir + 'tangtang.wav'
    else
      SoundFile.FileName := TempDir + 'tang.wav';
    if SoundFile.Valid then
      Player.Run;
    while Player.Active do
      Sleep(500);
    Dec(BellsToRing, 2);
  end;
end;

constructor TRingThread.Create(NumberOfBells: Integer; AudioOut: TAcsAudioOut; FileIn: TAcsFileIn);
begin
  BellsToRing := NumberOfBells;
  Player := AudioOut;
  SoundFile := FileIn;
  FreeOnTerminate := True;
  inherited Create(False);
end;

{ TFormMain }

procedure TFormMain.ButtonQuitClick(Sender: TObject);
begin
  DeleteWavFiles;
  Application.Terminate;
end;

procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ButtonQuitClick(Sender);
end;

procedure TFormMain.ButtonAboutClick(Sender: TObject);
begin
  FormAbout.Show;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  {$IfDef LINUX}
  AcsAudioOut.DriverName := 'Alsa';
  {$EndIf}
  CreateWavFiles;
  LabelWatchUpdate;
  TimerRing.Interval := GetInterval;
  TimerRing.Enabled := True;
end;

procedure TFormMain.FormWindowStateChange(Sender: TObject);
begin
  if FormMain.WindowState = wsMinimized then
  begin
    FormMain.WindowState := wsNormal; // needed to make form redraw normally
    FormMain.Hide;
  end;
end;

procedure TFormMain.MenuItemOpenClick(Sender: TObject);
begin
  FormMain.Show;
end;

procedure TFormMain.MenuItemAboutClick(Sender: TObject);
begin
  FormAbout.Show;
end;

procedure TFormMain.MenuItemQuitClick(Sender: TObject);
begin
  DeleteWavFiles;
  Application.Terminate;
end;

procedure TFormMain.TimerRingTimer(Sender: TObject);
begin
  TimerRing.Enabled := False;
  LabelWatchUpdate;
  Ring(Bells);
  TimerRing.Interval := GetInterval;
  TimerRing.Enabled := True;
end;

procedure TFormMain.TrayIconClick(Sender: TObject);
begin
  FormMain.Show;
end;

function TFormMain.Bells: Integer;
var
  w: Integer;
begin
  w := Watch;
  if w = 39 then Result := 3
  else if w = 38 then Result := 2
  else if w = 37 then Result := 1
  else begin
    Result := w mod 8;
    if Result = 0 then Result := 8;
  end;
end;

function TFormMain.GetInterval: Cardinal;
var
  Hours, Minutes, Seconds, Milliseconds: Word;
begin
  DecodeTime(Time, Hours, Minutes, Seconds, Milliseconds);
  Minutes := (59 - Minutes) mod 30;
  Seconds := 59 - Seconds;
  Milliseconds := 1000 - Milliseconds;
  Result := Cardinal((Minutes * 60 * 1000) + (Seconds * 1000) + Milliseconds);
end;

function TFormMain.Watch: Integer;
var
  Hour, Minutes, Discard: Word;
begin
  DecodeTime(Time, Hour, Minutes, Discard, Discard);
  Result := Hour * 2;
  if Minutes >= 45 then
    Result := Result + 2
  else if Minutes >= 15 then
    Result := Result + 1;
end;

function TFormMain.WatchName: String;
const
  WatchNames: array of String = ('First', 'Middle', 'Morning', 'Forenoon', 'Afternoon', 'First Dog', 'Second Dog');
var
  W: Integer;
begin
  W := Watch mod 48;
  if W = 0 then
    Result := WatchNames[0]
  else if W <= 8 then
    Result := WatchNames[1]
  else if W <= 16 then
    Result := WatchNames[2]
  else if W <= 24 then
    Result := WatchNames[3]
  else if W <= 32 then
    Result := WatchNames[4]
  else if W <= 36 then
    Result := WatchNames[5]
  else if W <= 40 then
    Result := WatchNames[6]
  else
    Result := WatchNames[0];
end;

procedure TFormMain.CreateWavFiles;
var
  F: TFileStream;
  R: TResourceStream;
begin
  {$IfDef LINUX}
  TempDir := '/tmp/shipsbells/';
  {$Else}
  TempDir := GetEnvironmentVariable('TEMP') + '\shipsbells\';
  {$EndIf}
  if not DirectoryExists(TempDir) then
    CreateDir(TempDir);
  R := TResourceStream.Create(HINSTANCE, 'TANG', RT_RCDATA);
  try
    F := TFileStream.Create(TempDir + 'tang.wav', fmCreate);
    try
      F.CopyFrom(R, R.Size);
    finally
      F.Free;
    end;
  finally
    R.Free;
  end;
  R := TResourceStream.Create(HINSTANCE, 'TANGTANG', RT_RCDATA);
  try
    F := TFileStream.Create(TempDir + 'tangtang.wav', fmCreate);
    try
      F.CopyFrom(R, R.Size);
    finally
      F.Free;
    end;
  finally
    R.Free;
  end;
end;

procedure TFormMain.DeleteWavFiles;
begin
  DeleteFile(TempDir + 'tang.wav');
  DeleteFile(TempDir + 'tangtang.wav');
  RemoveDir(TempDir);
end;

procedure TFormMain.Ring(NumberOfBells: Integer);
begin
  RingThread := TRingThread.Create(NumberOfBells, AcsAudioOut, AcsFileIn);
end;

procedure TFormMain.LabelWatchUpdate;
begin
  if Bells = 1 then
    LabelWatch.Caption := Format('%s bell in the %s Watch', [BellNames[Bells], WatchName])
  else
    LabelWatch.Caption := Format('%s bells in the %s Watch', [BellNames[Bells], WatchName]);
  TrayIcon.Hint := LabelWatch.Caption;
end;

end.

