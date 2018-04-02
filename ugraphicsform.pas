unit ugraphicsform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, Grids, windows, strutils, uregistermachine;

type

  { TForm1 }

  TForm1 = class(TForm)
    ExecuteBtn: TButton;
    CreateBtnWrite: TButton;
    CloseBtn: TButton;
    Editor: TMemo;
    InfoLabel: TLabel;
    LoadFile: TButton;
    OpenRegister: TOpenDialog;
    Pages: TPageControl;
    HeadingLeft: TPanel;
    RegisterSG: TStringGrid;
    ExecuteSG: TStringGrid;
    TabSetUp: TTabSheet;
    TabWrite: TTabSheet;
    TabExecute: TTabSheet;
    procedure CloseBtnClick(Sender: TObject);
    procedure CreateMachine(Sender: TObject);
    procedure ExecuteBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LoadFileClick(Sender: TObject);
    procedure initializeExecuteSG(registerAmount : Integer);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  loadedList : TStringList;
  regM : RegisterMachine;

implementation

{$R *.lfm}

{ TForm1 }
procedure TForm1.LoadFileClick(Sender: TObject);
begin
    if OpenRegister.Execute then
       begin
         Editor.Clear;
         Editor.Lines.LoadFromFile(OpenRegister.FileName);
       end
    else ShowMessage('Kein File ausgewählt');

end;

procedure TForm1.initializeExecuteSG(registerAmount: Integer);
var
  i : Integer;
begin
    with ExecuteSG do
    begin
      Cells[0, 0] := 'b';
      Cells[1, 0] := 'Zeile';
      Cells[2, 0] := 'Effekt';
      ColCount:= 3 + registerAmount;
      RowCount := Length(regM.GetExecuteLog) + 1;
    end;
    for i := 0 to registerAmount - 1 do
    begin
      ExecuteSG.Cells[i + 3, 0 ]:= 'c(' + IntToStr(i) + ')';
    end;
end;

procedure TForm1.CloseBtnClick(Sender: TObject);
begin
  Close;
end;


//CREATE
procedure TForm1.CreateMachine(Sender: TObject);
var
  i : Integer;
begin
  loadedList := TStringList.Create;
  loadedList.AddStrings(Editor.Lines);
  regM := RegisterMachine.Create(loadedList);
  //ERROR HANDLING NECESSARY
  if regM.GetErrorMessage <> '' then
     ShowMessage(regM.GetErrorMessage)
  else
  begin
    with  RegisterSG do
    begin
      ColCount:= High(regM.GetRegisterData);
      RowCount:= 2;
      FixedRows:= 1;
      //Height := RowCount * DefaultRowHeight;
      //Width := ColCount * DefaultColWidth;
    end;
    for i := 0 to High(regM.GetRegisterData) - 1 do
    begin
        RegisterSG.Cells[i, 0] := 'c(' + IntToStr(i + 1) + ')';
        RegisterSg.Cells[i, 1] := '0';
    end;
  end;

  TabSetUp.TabVisible:= true;
  Pages.ActivePage := TabSetUp;

end;


//EXECUTE
procedure TForm1.ExecuteBtnClick(Sender: TObject);
var
  values : array of Integer;
  i,j : Integer;
  h : commandLine;
begin
  SetLength(values, RegisterSG.ColCount - 1 );
  for i := 0 to RegisterSG.ColCount - 1 do
  begin
    values[i] := StrtoInt(RegisterSG.Cells[i, 1]);
  end;
  regM.Execute(values);
  TabExecute.TabVisible:= true;
  Pages.ActivePage := TabExecute;

  initializeExecuteSG(Length(regM.GetRegisterData));
  for i := 1 to Length(regM.GetExecuteLog) do
  begin
    with ExecuteSG do
    begin
      Cells[0, i] := regM.GetExecuteLog[i - 1].b;
      h := regM.GetExecuteLog[i - 1].command;
      case h.command of
           'END' : Cells[1, i] := h.command;
           '' :   Cells[1, i] := '';
           else Cells[1, i] := h.command + ' ' + IntToStr(h.value);
      end;
      Cells[2, i] := regM.GetExecuteLog[i - 1].sysOutput;
    end;
    for j := 0 to High(regM.GetExecuteLog[i - 1].registers) do
    ExecuteSG.Cells[3 + j, i] := IntToStr(regM.GetExecuteLog[i -1].registers[j]);
  end;


end;

procedure TForm1.FormCreate(Sender: TObject);
var
  s : TTextStyle;
begin
  Editor.Lines.LoadFromFile('Beispiele/Zahlenvergleich.txt');
  Pages.ActivePage := TabWrite;
  TabSetUp.TabVisible:= false;
  TabExecute.TabVisible:= false;
  s := RegisterSG.DefaultTextStyle;
  s.Alignment:=taCenter;
  RegisterSG.DefaultTextStyle := s;
end;

end.

