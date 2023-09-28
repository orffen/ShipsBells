unit BellRinger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ExtCtrls, About;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonQuit: TButton;
    ButtonAbout: TButton;
    LabelWatch: TLabel;
    LabelShipsBells: TLabel;
    MenuItemAbout: TMenuItem;
    MenuItemQuit: TMenuItem;
    PopupTrayMenu: TPopupMenu;
    TrayIcon: TTrayIcon;
    procedure ButtonAboutClick(Sender: TObject);
    procedure ButtonQuitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormWindowStateChange(Sender : TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemQuitClick(Sender: TObject);
    procedure TrayIconClick(Sender: TObject);
  private
    function Bells: Integer;
    function Watch: Integer;
    function WatchName(aWatch: Integer): String;
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

{ TFormMain }

procedure TFormMain.ButtonQuitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TFormMain.ButtonAboutClick(Sender: TObject);
begin
  FormAbout.Show;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  UpdateLabelWatch;
end;

procedure TFormMain.FormWindowStateChange(Sender : TObject);
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

procedure TFormMain.Ring(numberOfBells: Integer);
begin
  UpdateLabelWatch;
  //TODO
end;

procedure TFormMain.UpdateLabelWatch;
begin
  if Bells = 1
  then LabelWatch.Caption := Format('%s bell in the %s Watch', [BellNames[Bells], WatchName(Watch)])
  else LabelWatch.Caption := Format('%s bells in the %s Watch', [BellNames[Bells], WatchName(Watch)]);
  TrayIcon.Hint := LabelWatch.Caption;
end;

end.

