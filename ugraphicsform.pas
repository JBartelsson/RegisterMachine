unit ugraphicsform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, Grids, windows, strutils, uregistermachine;

type

  { TForm1 }

  TForm1 = class(TForm)
    CreateBtnWrite: TButton;
    CloseBtn: TButton;
    CreateBtnLoad: TButton;
    FileDisplay: TLabel;
    HeadingRight: TPanel;
    Editor: TMemo;
    Label1: TLabel;
    LoadFile: TButton;
    Memo1: TMemo;
    OpenRegister: TOpenDialog;
    Pages: TPageControl;
    HeadingLeft: TPanel;
    RegisterSG: TStringGrid;
    TabSetUp: TTabSheet;
    TabWrite: TTabSheet;
    TabLoad: TTabSheet;
    TabExecute: TTabSheet;
    procedure CloseBtnClick(Sender: TObject);
    procedure CreateMachine(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LoadFileClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  FileSource : String;
  loadedList : TStringList;
  regM : RegisterMachine;

implementation

{$R *.lfm}

{ TForm1 }
procedure TForm1.LoadFileClick(Sender: TObject);
begin
    if OpenRegister.Execute then
       begin
         FileSource := OpenRegister.FileName;
         FileDisplay.Caption:= Copy(FileSource, LastDelimiter('\',FileSource) + 1, Length(FileSource));
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
  if Sender = CreateBtnWrite then
  loadedList.AddStrings(Editor.Lines)
  else
    loadedList.LoadFromFile(FileSource);
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
    end;
    for i := 0 to High(regM.GetRegisterData) - 1 do
    begin

    end;
  end;

  Pages.Page[2].TabVisible:= true;
  Pages.ActivePage := Pages.Pages[2];

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Pages.ActivePage := TabWrite;
  TabSetUp.TabVisible:= false;
  TabExecute.TabVisible:= false;
  FileSource := '';
end;

end.

