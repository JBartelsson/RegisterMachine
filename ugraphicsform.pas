unit ugraphicsform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Grids, Buttons, ValEdit, Menus, Windows,
  strutils, uregistermachine, umanualform;

type

  { TReMEditForm }

  TReMEditForm = class(TForm)
    openWriteBtn: TButton;
    CloseBtn: TBitBtn;
    Editor: TMemo;
    fillIndicesPop: TPopupMenu;
    fillIndices: TMenuItem;
    ErrorOutput: TMemo;
    HeadingError: TPanel;
    HeadingStart: TPanel;
    openManualBtn: TButton;
    saveMenu: TMenuItem;
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
    TabStart: TTabSheet;
    TabWrite: TTabSheet;
    SpeedTrackBar: TTrackBar;
    procedure openManualBtnClick(Sender: TObject);
    procedure openWriteBtnClick(Sender: TObject);
    procedure save(fileSource: string);
    procedure CancelExecuteBtnClick(Sender: TObject);
    procedure fillIndicesClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
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
    procedure saveMenuClick(Sender: TObject);
    procedure updateCaption;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  ReMEditForm: TReMEditForm;
  loadedList: TStringList;
  regM: RegisterMachine;
  markedCells: array of array[0..1] of integer;
  cancelExecute: boolean;
  actualFile: string;

implementation

{$R *.lfm}
{DELAY FUNCTION}

{ TReMEditForm }
procedure TReMEditForm.LoadFileClick(Sender: TObject);
begin
  if (Editor.Lines.Count <> 0) and
    (MessageDlg('', 'Alle nicht gespeicherten Fortschritte gehen verloren. Fortfahren?',
    mtConfirmation, mbYesNo, -1) = mrYes) then
  begin
    if OpenRegister.Execute then
    begin
      Editor.Clear;
      actualFile := OpenRegister.FileName;
      Editor.Lines.LoadFromFile(actualFile);
      updateCaption;
    end
    else
      ShowMessage('Kein File ausgewählt');
  end;

end;

procedure TReMEditForm.initializeExecuteSG(registerAmount: integer);
var
  i: integer;
begin
  RegisterSG.Enabled := False;
  ExecuteBtn.Enabled := False;
  CancelExecuteBtn.Enabled := True;
  with ExecuteSG do
  begin
    Enabled := True;
    Visible := True;
    ColCount := 4 + registerAmount;
    RowCount := 1;
    Cells[0, 0] := 'i';
    Cells[1, 0] := 'b';
    Cells[2, 0] := 'Zeile';
    Cells[3, 0] := 'Effekt';
  end;
  for i := 0 to registerAmount - 1 do
  begin
    ExecuteSG.Cells[i + 4, 0] := 'c(' + IntToStr(i) + ')';
  end;
end;

procedure TReMEditForm.Delay;
var
  tc: DWORD;
begin
  tc := GetTickCount;
  while (GetTickCount < tc + ((SpeedTrackBar.Max + SpeedTrackBar.Min) -
      SpeedTrackBar.Position)) and (not Application.Terminated) do
    Application.ProcessMessages;
end;

procedure TReMEditForm.goToLine(X, Y: integer);
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

procedure TReMEditForm.saveMenuClick(Sender: TObject);
begin
  if actualFile <> '' then
  save(actualFile)
  else
    SaveFileClick(nil);
end;

procedure TReMEditForm.updateCaption;
begin
  Caption := 'ReMEdit 1.0 ' + ExtractFileName(actualFile);
end;


procedure TReMEditForm.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TReMEditForm.SaveFileClick(Sender: TObject);
begin
  if SaveRegister.Execute then
  begin
    save(SaveRegister.FileName);
  end;

end;

procedure TReMEditForm.fillIndicesClick(Sender: TObject);
var
  i, j, b, oldLineLength: integer;
  pos: TPOINT;
  line, refLine, referrer: string;
  forwardC, backwardC : array of array[0..1] of String;

  procedure fillVerifiedCommands; //firstly changes all commands to legal commands
  var
    k : Integer;
  begin
    for k := 0 to Editor.Lines.Count - 1 do
    begin
    line := trim(Editor.Lines[k]);
    if RegisterMachine.verifyCommand('0 ' + line) = '' then  //else it adds an artificial index to check if the user hasn't written any
         Editor.Lines[k] := '0 ' + line;
    end;
  end;

  procedure findIndexChanges; //finds lines which need a value change
  var
    l, m, refIndex, referrerIndex: Integer;
  begin
     for l:= 0 to Editor.Lines.Count - 1 do
     begin
       if (AnsiMatchText(ExtractDelimited(2, Editor.Lines[l], [' ']), ['GOTO', 'JZERO', 'JNZERO'])) AND (RegisterMachine.verifyCommand(Editor.Lines[l]) = '') then
       //if command is GOTO, JZERO or JNZERO
      begin
        //find referred line
        referrerIndex := StrToInt(ExtractDelimited(3, Editor.Lines[l], [' ']));
        for m := 0 to Editor.Lines.Count - 1 do
        begin
        if RegisterMachine.verifyCommand(Editor.Lines[m]) = '' then
        begin
        refIndex:= StrToInt(ExtractDelimited(1, Editor.Lines[m], [' ']));
        //if the referred line's index is bigger than the referrer line it's a forward Index Change otherwise it's backward
        if referrerIndex = refIndex  then
         begin
           if l < m then
           begin
           SetLength(forwardC, Length(forwardC) + 1);
           forwardC[High(forwardC)][0] := Editor.Lines[l];
           forwardC[High(forwardC)][1] := Editor.Lines[m];
           end
           else
           begin
           SetLength(backwardC, Length(backwardC) + 1);
           backwardC[High(backwardC)][0] := Editor.Lines[l];
           backwardC[High(backwardC)][1] := Editor.Lines[m];
           end;
           break;
         end;
        end;
        end;
      end;
     end;
  end;

begin
  pos := editor.CaretPos;
  oldLineLength:= Length(Editor.Lines[pos.y]); //necessary for recalculation of cursor position in memo
  b := 0;
  fillVerifiedCommands;
  findIndexChanges;
  for i := 0 to Editor.Lines.Count - 1 do
  begin
    if RegisterMachine.verifyCommand(Editor.Lines[i]) = '' then  //if command legal
    begin
      line := Editor.Lines[i];
      line := UpperCase(IntToStr(b) + ' ' + ExtractDelimited(2, line, [' ']) + ' ' + ExtractDelimited(3, line, [' '])); //updates index
      //implementation of forward Index Changes
      for j := 0 to High(forwardC) do
      begin
      if forwardC[j][0] = Editor.Lines[i] then  //if the actual line will be changed, the string in forwardC will be too
      begin
        forwardC[j][0] := line;
      end;
      if forwardC[j][1] = Editor.Lines[i] then
      begin
      referrer := forwardC[j][0];
      Editor.Lines[Editor.Lines.IndexOf(referrer)] := ExtractDelimited(1, referrer, [' ']) + ' ' + ExtractDelimited(2, referrer, [' ']) + ' ' + IntToStr(b) ;
      end;
      end;
      //implementation of backward Index Changes
      for j := 0 to High(backwardC) do
      begin
      if backwardC[j][1] = Editor.Lines[i] then
      begin
        backwardC[j][1] := line;
      end;
      if backwardC[j][0] = Editor.Lines[i] then
      begin
      refLine:= backwardC[j][1];
      line := ExtractDelimited(1, line, [' ']) + ' ' + ExtractDelimited(2, line, [' ']) + ' ' + ExtractDelimited(1, refLine, [' ']) ;
      end;
      end;
      b := b + 1;
      Editor.Lines[i] := line;
    end;
  end;
  goToLine(pos.x + Length(Editor.Lines[pos.y]) - oldLineLength, pos.y); //recalculation of cursor position
end;

procedure TReMEditForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  i: integer;
  fileSaved: boolean;
  oldFile, newFile: TStringList;
begin
  fileSaved := True;
  oldFile := TStringList.Create;
  newFile := TStringList.Create;
  newFile.AddStrings(Editor.Lines);
  //Determines, if file has changed
  try
    oldFile.LoadFromFile(actualFile);
    for i := 0 to newFile.Count - 1 do
    begin
      try
        if trim(newFile.Strings[i]) <> trim(oldFile.Strings[i]) then
        begin
          fileSaved := False;
          break;
        end;
      except
        fileSaved := False;
        break;
      end;
    end;
  except
    fileSaved := False;
  end;

  if not fileSaved then
  begin
    if MessageDlg('Warnung', 'Sie haben ungespeicherte Änderungen. Wollen Sie die Anwendung wirklich schließen?',
      mtWarning, mbYesNo, 0) = mrYes then
      CanClose := True
    else
      CanClose := False;
  end;
end;

procedure TReMEditForm.save(fileSource: string);
begin
  actualFile := fileSource;
  Editor.Lines.SaveToFile(fileSource);
  updateCaption;
end;

procedure TReMEditForm.openWriteBtnClick(Sender: TObject);
begin
  TabWrite.TabVisible:= true;
  Pages.ActivePage := TabWrite;
end;

procedure TReMEditForm.openManualBtnClick(Sender: TObject);
begin
  manualform.Show;
end;

procedure TReMEditForm.CancelExecuteBtnClick(Sender: TObject);
begin
  cancelExecute := True;
  CancelExecuteBtn.Enabled := False;
  ExecuteBtn.Enabled := True;
  RegisterSG.Enabled := True;
end;

//CREATE
procedure TReMEditForm.CreateMachine(Sender: TObject);
var
  i: integer;
begin
  fillIndicesClick(nil);
  loadedList := TStringList.Create;
  loadedList.AddStrings(Editor.Lines);
  regM := RegisterMachine.Create(loadedList);

  if regM.GetErrorMessage <> '' then
  begin
    //ERROR HANDLING
    TabError.TabVisible := True;
    Pages.ActivePage := TabError;
    ErrorOutput.Clear;
    ErrorOutput.Lines.Add(regM.GetErrorMessage);
  end
  else
  begin
    //INITIALIZATION for TabExecute
    if actualFile <> '' then
      Editor.Lines.SaveToFile(actualFile);
    TabError.TabVisible := False;
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
procedure TReMEditForm.ExecuteBtnClick(Sender: TObject);
var
  values: registerArray;
  i, j, colorRegister: integer;
  h: commandLine;

begin
  cancelExecute := False;
  SetLength(values, RegisterSG.ColCount);
  for i := 0 to RegisterSG.ColCount - 1 do
  begin
    values[i] := StrToInt(RegisterSG.Cells[i, 1]);
  end;
  if not regM.Execute(values) then
  begin
    if MessageDlg('Warnung',
      'Möglicherweise wird die Registermaschine nie beendet. Ein manueller Abbruch ist bei Fortfahren eventuell erforderlich.',
      mtWarning, mbOKCancel, 0) = mrCancel then
      exit;
  end;


  initializeExecuteSG(Length(regM.GetRegisterData));
  for i := 1 to Length(regM.GetExecuteLog) do
  begin
    if cancelExecute then  //Cancel button is pressed stop procedure
      exit;
    with ExecuteSG do
    begin
      //Adds new line in ExecuteSG
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

    //colors Registers
    colorRegister := -1;
    if AnsiMatchText(regM.GetExecuteLog[i - 1].command.command,
      ['LOAD', 'CLOAD', 'CADD', 'CSUB', 'CMULT', 'CDIV', 'MULT',
      'ADD', 'SUB', 'DIV']) then
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

procedure TReMEditForm.ExecuteSGDrawCell(Sender: TObject; aCol, aRow: integer;
  aRect: TRect; aState: TGridDrawState);
var
  i: integer;

  procedure colorCell(aRect: TRect; cellText: string; brushColor: TColor);
  begin
    with ExecuteSG do
    begin
      Canvas.Brush.Color := brushColor;
      Canvas.FillRect(aRect);
      Canvas.TextOut(aRect.Left + 2, aRect.Top + 2, cellText);
    end;
  end;

begin
  //changes color of ExecuteSG cells
  //grey cells
  for i := 0 to High(markedCells) do
  begin
    if (ACol = markedCells[i][0]) and (ARow = markedCells[i][1]) then
    begin
      colorCell(aRect, ExecuteSG.Cells[ACol, ARow], clSilver);
    end;
  end;
  //green cells
  if ARow = Length(regM.GetExecuteLog) then
  begin
    colorCell(aRect, ExecuteSG.Cells[ACol, ARow], clGreen);
  end;

end;

procedure TReMEditForm.FormCreate(Sender: TObject);
begin
  //VARIABLE INITIALIZATION
  actualFile := 'Beispiele/Zahlenvergleich.txt';
  cancelExecute := False;
  SetLength(markedCells, 0);
  Editor.Lines.LoadFromFile(actualFile);

  //LAYOUT INITIALIZATION
  Pages.ActivePage := TabStart;
  TabWrite.TabVisible:= false;
  TabSetUp.TabVisible := False;
  TabError.TabVisible := False;
  Pages.Color := clGray;

  updateCaption;

end;

end.
