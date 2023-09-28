program ShipsBells;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, BellRinger, About;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='Ship''s Bells';
  Application.Scaled:=True;
  Application.Initialize;
  Application.ShowMainForm:=False;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormAbout, FormAbout);
  Application.Run;
end.

