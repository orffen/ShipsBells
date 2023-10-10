unit BellRinger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, ExtCtrls, uplaysound, About;

type

  { TRingThread }

  TRingThread = class(TThread)
  PlaySound: Tplaysound;
  private
    BellsToRing: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(player: Tplaysound; numberOfBells: Integer);
  end;

  { TFormMain }

  TFormMain = class(TForm)
    ButtonQuit: TButton;
    ButtonAbout: TButton;
    LabelWatch: TLabel;
    LabelShipsBells: TLabel;
    MenuItemAbout: TMenuItem;
    MenuItemQuit: TMenuItem;
    PlaySound : Tplaysound;
    PopupTrayMenu: TPopupMenu;
    TimerRing : TTimer;
    TrayIcon: TTrayIcon;
    procedure ButtonAboutClick(Sender: TObject);
    procedure ButtonQuitClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemQuitClick(Sender: TObject);
    procedure TimerRingTimer(Sender: TObject);
    procedure TrayIconClick(Sender: TObject);
  private
    RingThread: TRingThread;
    function Bells: Integer;
    function GetInterval: Cardinal;
    function Watch: Integer;
    function WatchName(aWatch: Integer): String;
    procedure CreateWavFiles;
    procedure DeleteWavFiles;
    procedure Ring(numberOfBells: Integer);
    procedure UpdateLabelWatch;

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
    if BellsToRing > 1 then PlaySound.SoundFile := TempDir + 'tangtang.wav'
    else PlaySound.SoundFile := TempDir + 'tang.wav';
    PlaySound.Execute;
    Dec(BellsToRing, 2);
  end;
end;

constructor TRingThread.Create(player: Tplaysound; numberOfBells: Integer);
begin
  PlaySound := player;
  BellsToRing := numberOfBells;
  inherited Create(False);
  FreeOnTerminate := True;
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
  CreateWavFiles;
  UpdateLabelWatch;
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

procedure TFormMain.MenuItemAboutClick(Sender: TObject);
begin
  FormAbout.Show;
end;

procedure TFormMain.MenuItemQuitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TFormMain.TimerRingTimer(Sender: TObject);
begin
  begin
    TimerRing.Enabled := False;
    UpdateLabelWatch;
    Ring(Bells);
    TimerRing.Interval := GetInterval;
    TimerRing.Enabled := True;
  end;
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
  if Minutes >= 45
  then Result := Result + 2
  else if Minutes >= 15
  then Result := Result + 1;
end;

function TFormMain.WatchName(aWatch: Integer): String;
const
  WatchNames: array of String = ('First', 'Middle', 'Morning', 'Forenoon', 'Afternoon', 'First Dog', 'Second Dog');
var
  w: Integer;
begin
  w := aWatch mod 48;
  if w = 0 then Result := WatchNames[0]
  else if w <= 8 then Result := WatchNames[1]
  else if w <= 16 then Result := WatchNames[2]
  else if w <= 24 then Result := WatchNames[3]
  else if w <= 32 then Result := WatchNames[4]
  else if w <= 36 then Result := WatchNames[5]
  else if w <= 40 then Result := WatchNames[6]
  else Result := WatchNames[0];
end;

procedure TFormMain.CreateWavFiles;
var
  F: TFileStream;
  R: TResourceStream;
begin
  {$IfDef UNIX}
  TempDir := '/tmp/shipsbells/';
  {$Else}
  TempDir := GetEnvironmentVariable('TEMP') + '\shipsbells\';
  {$EndIf}
  if not DirectoryExists(TempDir) then CreateDir(TempDir);
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

procedure TFormMain.Ring(numberOfBells: Integer);
begin
  RingThread := TRingThread.Create(PlaySound, numberOfBells);
end;

procedure TFormMain.UpdateLabelWatch;
begin
  if Bells = 1
  then LabelWatch.Caption := Format('%s bell in the %s Watch', [BellNames[Bells], WatchName(Watch)])
  else LabelWatch.Caption := Format('%s bells in the %s Watch', [BellNames[Bells], WatchName(Watch)]);
  TrayIcon.Hint := LabelWatch.Caption;
end;

end.

