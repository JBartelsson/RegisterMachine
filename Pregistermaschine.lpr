program Pregistermaschine;

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
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TReMEdit, ReMEdit);
  Application.CreateForm(TmanualForm, manualForm);
  Application.Run;
end.

