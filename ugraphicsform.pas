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
    Memo1: TMemo;
    OpenRegister: TOpenDialog;
    Pages: TPageControl;
    HeadingLeft: TPanel;
    RegisterSG: TStringGrid;
    TabSetUp: TTabSheet;
    TabWrite: TTabSheet;
    TabExecute: TTabSheet;
    procedure CloseBtnClick(Sender: TObject);
    procedure CreateMachine(Sender: TObject);
    procedure ExecuteBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LoadFileClick(Sender: TObject);
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

procedure TForm1.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

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

procedure TForm1.ExecuteBtnClick(Sender: TObject);
var
  values : array of Integer;
  i : Integer;
begin
  SetLength(values, RegisterSG.ColCount - 1 );
  for i := 0 to RegisterSG.ColCount - 1 do
  begin
    values[i] := StrtoInt(RegisterSG.Cells[i, 1]);
  end;
  regM.Execute(values);
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

