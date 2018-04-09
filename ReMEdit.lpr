program ReMEdit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uGraphicsForm, uRegisterMachine, umanualform
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='ReMEdit';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TReMEditForm, ReMEditForm);
  Application.CreateForm(TmanualForm, manualForm);
  Application.Run;
end.

