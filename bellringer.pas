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
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemQuitClick(Sender: TObject);
    procedure TrayIconClick(Sender: TObject);
  private

  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

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
  LabelWatch.Caption := 'Eight bells in the Afternoon Watch';
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

end.

