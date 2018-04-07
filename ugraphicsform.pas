unit ugraphicsform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,  Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Grids, Buttons, ValEdit, Menus, Windows,
  strutils, uregistermachine;

type

  { TReMEdit }

  TReMEdit = class(TForm)
    CloseBtn: TBitBtn;
    Editor: TMemo;
    fillIndicesPop: TPopupMenu;
    fillIndices: TMenuItem;
    ErrorOutput: TMemo;
    HeadingError: TPanel;
    SpeedContainer: TPanel;
    SaveRegister: TSaveDialog;
    SaveFile: TButton;
    CancelExecuteBtn: TBitBtn;
    CreateBtnWrite: TButton;
    ExecuteBtn: TBitBtn;
    InfoLabel: TLabel;
    SpeedLabel: TLabel;
    LoadFile: TButton;
    OpenRegister: TOpenDialog;
    Pages: TPageControl;
    HeadingLeft: TPanel;
    RegisterSG: TStringGrid;
    ExecuteSG: TStringGrid;
    TabSetUp: TTabSheet;
    TabError: TTabSheet;
    TabWrite: TTabSheet;
    SpeedTrackBar: TTrackBar;
    procedure CancelExecuteBtnClick(Sender: TObject);
    procedure fillIndicesClick(Sender: TObject);
    procedure SaveFileClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure CreateMachine(Sender: TObject);
    procedure ExecuteBtnClick(Sender: TObject);
    procedure ExecuteSGDrawCell(Sender: TObject; aCol, aRow: integer;
      aRect: TRect; aState: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure LoadFileClick(Sender: TObject);
    procedure initializeExecuteSG(registerAmount: integer);
    procedure Delay;
    procedure goToLine(X, Y: integer);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  ReMEdit: TReMEdit;
  loadedList: TStringList;
  regM: RegisterMachine;
  markedCells: array of array[0..1] of integer;
  executeFinished: boolean;
  cancelExecute : boolean;
  actualFile : String;

implementation

{$R *.lfm}
{DELAY FUNCTION}

{ TReMEdit }
procedure TReMEdit.LoadFileClick(Sender: TObject);
begin
  if (Editor.Lines.Count <> 0) and
    (MessageDlg('', 'Alle nicht gespeicherten Fortschritte gehen verloren. Fortfahren?',
    mtConfirmation, mbOKCancel, -1) = mrOk) then
  begin
    if OpenRegister.Execute then
    begin
      Editor.Clear;
      Editor.Lines.LoadFromFile(OpenRegister.FileName);
    end
    else
      ShowMessage('Kein File ausgewählt');
  end;

end;

procedure TReMEdit.initializeExecuteSG(registerAmount: integer);
var
  i: integer;
begin
  executeFinished := False;
  RegisterSG.Enabled := False;
  ExecuteBtn.Enabled := False;
  CancelExecuteBtn.Enabled := True;
  with ExecuteSG do
  begin
    Enabled := True;
    Visible := True;
    ColCount := 4 + registerAmount;
    RowCount := 1;
    Cells[0,0] := 'i';
    Cells[1, 0] := 'b';
    Cells[2, 0] := 'Zeile';
    Cells[3, 0] := 'Effekt';
  end;
  for i := 0 to registerAmount - 1 do
  begin
    ExecuteSG.Cells[i + 4, 0] := 'c(' + IntToStr(i) + ')';
  end;
end;

procedure TReMEdit.Delay;
var
  tc: DWORD;
begin
  tc := GetTickCount;
  while (GetTickCount < tc + ((SpeedTrackBar.Max + SpeedTrackBar.Min) -
      SpeedTrackBar.Position)) and (not Application.Terminated) do
    Application.ProcessMessages;
end;

procedure TReMEdit.goToLine(X, Y: integer);
var
  s: string;
  actLineNumber, i, cPos: integer;
begin
  actLineNumber := 0;
  s := Editor.Text;
  cPos := 0;
  for i := 0 to Length(s) do
  begin
    cPos := cPos + 1;
    if Copy(s, i, 1) = #10 then
      actLineNumber := actLineNumber + 1;
    if actLineNumber = Y then
    begin
      Editor.SetFocus;
      Editor.SelStart := cPos + X;
      Break;
    end;
  end;

end;


procedure TReMEdit.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TReMEdit.SaveFileClick(Sender: TObject);
begin
  if SaveRegister.Execute then
  begin
    Editor.Lines.SaveToFile(SaveRegister.FileName);
    actualFile := SaveRegister.FileName;
  end;

end;

procedure TReMEdit.fillIndicesClick(Sender: TObject);
var
  i, b, oldStringLength: integer;
  pos: TPOINT;
  line: string;
  setB: boolean;
begin
  pos := editor.CaretPos;
  b := 0;
  for i := 0 to Editor.Lines.Count - 1 do
  begin
    setB := False;
    line := trim(Editor.Lines[i]);
    oldStringLength := Length(line);
    if RegisterMachine.verifyCommand(line) = '' then
      setB := True
    else
    begin
      line := '0 ' + line;
      if RegisterMachine.verifyCommand(line) = '' then
        setB := True;
    end;
    line := ExtractDelimited(2, line, [' ']) + ' ' + ExtractDelimited(3, line, [' ']);
    if setB = True then
    begin
      line := IntToStr(b) + ' ' + line;
      b := b + 1;
      Editor.Lines[i] := UpperCase(line);
      if i = pos.y then
        pos.x := pos.x + Length(line) - oldStringLength;
    end;
  end;
  goToLine(pos.x - 1, pos.y);
end;

procedure TReMEdit.CancelExecuteBtnClick(Sender: TObject);
begin
   cancelExecute:= true;
   CancelExecuteBtn.Enabled:= false;
   ExecuteBtn.Enabled:= true;
   RegisterSG.Enabled:= true;
end;

//CREATE
procedure TReMEdit.CreateMachine(Sender: TObject);
var
  i: integer;
begin
  loadedList := TStringList.Create;
  loadedList.AddStrings(Editor.Lines);
  regM := RegisterMachine.Create(loadedList);
  //ERROR HANDLING NECESSARY
  if regM.GetErrorMessage <> '' then
  begin
    TabError.TabVisible := True;
    Pages.ActivePage := TabError;
    ErrorOutput.Clear;
    ErrorOutput.Lines.Add(regM.GetErrorMessage);
  end
  else
  begin
    if actualFile <> '' then
    Editor.Lines.SaveToFile(actualFile);
    TabError.TabVisible:=false;
    with RegisterSG do
    begin
      RowCount := 2;
      FixedRows := 1;
      ColCount := High(regM.GetRegisterData);
    end;
    for i := 0 to High(regM.GetRegisterData) - 1 do
    begin
      RegisterSG.Cells[i, 0] := 'c(' + IntToStr(i + 1) + ')';
      RegisterSG.Cells[i, 1] := '0';
    end;
    ExecuteSG.Visible := False;
    TabSetUp.TabVisible := True;
    Pages.ActivePage := TabSetUp;

  end;
end;

//EXECUTE
procedure TReMEdit.ExecuteBtnClick(Sender: TObject);
var
  values: registerArray;
  i, j, colorRegister: integer;
  h: commandLine;

begin
  cancelExecute:= false;
  SetLength(values, RegisterSG.ColCount);
  for i := 0 to RegisterSG.ColCount - 1 do
  begin
    values[i] := StrToInt(RegisterSG.Cells[i, 1]);
  end;
  if not regM.Execute(values) then
  begin
    if MessageDlg('Warnung','Möglicherweise wird die Registermaschine nie beendet. Ein manueller Abbruch ist bei Fortfahren eventuell erforderlich.', mtWarning, mbOKCancel, 0) = mrCancel then
    exit;
  end;


  initializeExecuteSG(Length(regM.GetRegisterData));
  for i := 1 to Length(regM.GetExecuteLog) do
  begin
    if cancelExecute then
    exit;
    with ExecuteSG do
    begin
      RowCount := RowCount + 1;
      Row := RowCount;
      Cells[0, i] := IntToStr(i);
      Cells[1, i] := regM.GetExecuteLog[i - 1].b;
      h := regM.GetExecuteLog[i - 1].command;
      case h.command of
        'END': Cells[2, i] := h.command;
        '': Cells[2, i] := '';
        else
          Cells[2, i] := h.command + ' ' + IntToStr(h.Value);
      end;
      Cells[3, i] := regM.GetExecuteLog[i - 1].sysOutput;
    end;
    for j := 0 to High(regM.GetExecuteLog[i - 1].registers) do
    begin
      ExecuteSG.Cells[4 + j, i] := IntToStr(regM.GetExecuteLog[i - 1].registers[j]);
    end;

    colorRegister := -1;
    if AnsiMatchText(regM.GetExecuteLog[i - 1].command.command,
      ['LOAD', 'CLOAD', 'CADD', 'CSUB', 'CMULT', 'CDIV', 'MULT', 'ADD', 'SUB', 'DIV']) then
      colorRegister := 0
    else if regM.GetExecuteLog[i - 1].command.command = 'STORE' then
      colorRegister := regM.GetExecuteLog[i - 1].command.Value;

    if colorRegister <> -1 then
    begin
      SetLength(markedCells, Length(markedCells) + 1);
      markedCells[High(markedCells)][0] := 3 + colorRegister;
      markedCells[High(markedCells)][1] := i;
    end;
    Delay;
  end;
  CancelExecuteBtnClick(nil);

end;

procedure TReMEdit.ExecuteSGDrawCell(Sender: TObject; aCol, aRow: integer;
  aRect: TRect; aState: TGridDrawState);
var
  i: integer;

  procedure colorCell(aRect: TRect; cellText: String; brushColor: TColor);
  begin
    with ExecuteSG do
    begin
    Canvas.Brush.Color := brushColor;
    Canvas.FillRect(aRect);
    Canvas.TextOut(aRect.Left + 2, aRect.Top + 2, cellText);
    end;
  end;

begin
  for i := 0 to High(markedCells) do
  begin
    if (ACol = markedCells[i][0]) and (ARow = markedCells[i][1]) then
    begin
       colorCell(aRect, ExecuteSG.Cells[ACol, ARow], clSilver);
    end;
  end;

  if ARow = Length(regM.GetExecuteLog) then
  begin
    colorCell(aRect, ExecuteSG.Cells[ACol, ARow], clGreen);
  end;

end;

procedure TReMEdit.FormCreate(Sender: TObject);
var
  s: TTextStyle;
begin
  //VARIABLE INITIALIZATION
  actualFile:= '';
  cancelExecute:= false;
  SetLength(markedCells, 0);
  Editor.Lines.LoadFromFile('Beispiele/Zahlenvergleich.txt');

  //LAYOUT INITIALIZATION
  Pages.ActivePage := TabWrite;
  TabSetUp.TabVisible := False;
  TabError.TabVisible:= false;
  Pages.Color := clGray;

  //


end;

end.
