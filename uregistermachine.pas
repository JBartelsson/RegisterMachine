unit uRegisterMachine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, strutils;

type
  commandLine = record
    command: string;
    Value: integer;
  end;

type
  registerArray = array of integer;
  commandList = array of commandLine;

  commandLog = record
    b : String;
    command: commandLine;
    sysOutput : String;
    registers : registerArray;
end;

type
   commandLogSum = array of commandLog;

  { RegisterMachine }

  RegisterMachine = class

  private
    registerData: registerArray;
    programData: commandList;
    rawStringData: TStringList;
    executeLog: commandLogSum;
    errorMessage: string;
    function LOAD(c : Integer) : String;
    function STORE(c : Integer) : String;
    function ADD(c : Integer) : String;
    function SUB(c : Integer) : String;
    function MULT(c : Integer) : String;
    function &DIV(c : Integer) : String;
    function CLOAD(c : Integer) : String;
    function CADD(c : Integer) : String;
    function CSUB(c : Integer) : String;
    function CMULT(c : Integer) : String;
    function CDIV(c : Integer) : String;
    function JZERO(var j, b : Integer) : String;
    function JNZERO(var j, b : Integer) : String;
    function &GOTO(var j, b : Integer) : String;
    function &END : String;

  public
    constructor Create(rawStringList: TStringList);
    function GetRegisterData: registerArray;
    procedure SetRegisterData(NewValue: integer; Index: integer);
    procedure SetRegisterData(NewArray: registerArray);
    function GetProgramData: commandList;
    procedure SetProgramData(NewValue: commandLine; Index: integer);
    function GetRawStringData: TStringList;
    procedure DeleteComments(var s: string; CommentSymbol: string);
    function GetErrorMessage: string;
    function GetExecuteLog: commandLogSum;
    procedure SetErrorMessage(NewValue: string);
    procedure ProcessFile;
    procedure RenderRegisters;
    procedure RenderCommands;
    procedure Execute(regs : registerArray);
  const
    commands: array[0..14] of string =
      ('LOAD', 'STORE', 'ADD', 'SUB', 'MULT', 'DIV', 'GOTO', 'END', 'CLOAD',
      'CADD', 'CMULT', 'CSUB', 'CDIV', 'JZERO', 'JNZERO');
    RegisterCommands: array[0..5] of string =
      ('LOAD', 'STORE', 'ADD', 'SUB', 'MULT', 'DIV');
    moveCommands : array[0..3] of String = ('GOTO', 'JZERO', 'JNZERO', 'END');


  end;

implementation

//REGISTER MACHINE FUNCTIONS
function RegisterMachine.LOAD(c: Integer): String;
begin
  SetRegisterData(GetRegisterData[c], 0);
  Result := 'A lädt Zahl ' + IntToStr(GetRegisterData[c]) + ' aus c(' + IntToStr(c) + ')';
end;

function RegisterMachine.STORE(c: Integer): String;
begin
   SetRegisterData(GetRegisterData[0], c);
     Result := 'A speichert Zahl ' + IntToStr(GetRegisterData[c]) + ' in c(' + IntToStr(c) + ')';
end;

function RegisterMachine.ADD(c: Integer): String;
begin
   SetRegisterData(GetRegisterData[0] + GetRegisterData[c], 0);
   Result := 'A wird um ' + inttoStr(GetRegisterData[c]) + ' aus c(' + IntToStr(c) + ') erhöht';
end;

function RegisterMachine.SUB(c: Integer): String;
begin
   SetRegisterData(GetRegisterData[0] - GetRegisterData[c], 0);
   if GetRegisterData[0] < 0 then
   begin
   GetRegisterData[0] := 0;
     Result := 'A wird um ' + inttoStr(GetRegisterData[c]) + ' aus c(' + IntToStr(c) + ') bis auf 0 verringert';
   end
   else
     Result := 'A wird um ' + inttostr(GetRegisterData[c]) + ' aus c(' + IntToStr(c) + ') verringert';

end;

function RegisterMachine.MULT(c: Integer): String;
begin
   SetRegisterData(GetRegisterData[0] * GetRegisterData[c], 0);
   Result := 'A wird mit ' + inttostr(GetRegisterData[c]) + ' aus c(' + IntToStr(c) + ') multipliziert';
end;

function RegisterMachine.&div(c: Integer): String;
begin
   if GetRegisterData[c] <> 0 then
   begin
   SetRegisterData(GetRegisterData[0] div GetRegisterData[c], 0);
   Result := 'A wird durch ' + inttostr(GetRegisterData[c]) + ' aus c(' + IntToStr(c) + ') dividiert';
   end
   else Result := 'ERROR';
end;

function RegisterMachine.CLOAD(c: Integer): String;
begin
   SetRegisterData(c, 0);
   Result := 'A lädt Konstante ' + IntToStr(c);
end;

function RegisterMachine.CADD(c: Integer): String;
begin
    SetRegisterData(GetRegisterData[0] + c, 0);
    Result := 'A um ' + IntToStr(c) + ' erhöht';
end;

function RegisterMachine.CSUB(c: Integer): String;
begin
    SetRegisterData(GetRegisterData[0] - c, 0);
    Result := 'A um ' + IntToStr(c) + ' verringert';
end;

function RegisterMachine.CMULT(c: Integer): String;
begin
    SetRegisterData(GetRegisterData[0] * c, 0);
    Result := 'A mit ' + IntToStr(c) + ' multipliziert';
end;

function RegisterMachine.CDIV(c: Integer): String;
begin
   if c <> 0 then
   begin
   SetRegisterData(GetRegisterData[0] div c, 0);
   Result := 'A wird durch ' + IntToStr(c) + ' dividiert';
   end
   else Result := 'ERROR';
end;

function RegisterMachine.JZERO(var j, b: Integer): String;
begin
    if GetRegisterData[0] = 0 then
    begin
    b := j;
    Result := 'A ist 0 -> Sprung zur' + IntToStr(j) + '. Programmzeile';
    end
    else
    begin
    Result := 'A ist nicht 0 -> Sprung zur nächsten Programmzeile';
    b := b +1;
    end;
end;

function RegisterMachine.JNZERO(var j, b: Integer): String;
begin
  if GetRegisterData[0] <> 0 then
    begin
    b := j;
    Result := 'A ist nicht 0 -> Sprung zur ' + IntToStr(j) + '. Programmzeile';
    end
    else
    begin
    Result := 'A ist  0 -> Sprung zur nächsten Programmzeile';
    b := b +1;
    end;
end;

function RegisterMachine.&goto(var j, b: Integer): String;
begin
   b := j;
   Result := 'Sprung zur ' + IntToStr(j) + '. Programmzeile' ;
end;

function RegisterMachine.&end: String;
begin
    Result := 'Ende des Ablaufs';
end;


//FORMATTING AND INITIALIZE

constructor RegisterMachine.Create(rawStringList: TStringList);
begin
  SetErrorMessage('');
  rawStringData := rawStringList;
  ProcessFile;
  RenderCommands;
  RenderRegisters;
end;

function RegisterMachine.GetRegisterData: registerArray;
begin
  Result := registerData;
end;

procedure RegisterMachine.SetRegisterData(NewValue: integer; Index: integer);
begin
  registerData[Index] := NewValue;
end;

procedure RegisterMachine.SetRegisterData(NewArray: registerArray);
begin
  registerData := NewArray;
end;

function RegisterMachine.GetProgramData: commandList;
begin
  Result := programData;
end;

procedure RegisterMachine.SetProgramData(NewValue: commandLine; Index: integer);
begin
  if index + 1 > Length(programData) then
    SetLength(programData, index + 1);
  programData[Index] := NewValue;
end;

function RegisterMachine.GetRawStringData: TStringList;
begin
  Result := rawStringData;
end;

procedure RegisterMachine.DeleteComments(var s: string; CommentSymbol: string);
var
  position: integer;
begin
  position := Pos(CommentSymbol, s);
  if position <> 0 then
    Delete(s, position, Length(s) - position + 1);
end;

function RegisterMachine.GetErrorMessage: string;
begin
  Result := errorMessage;
end;

function RegisterMachine.GetExecuteLog: commandLogSum;
begin
  Result := executeLog;
end;

procedure RegisterMachine.SetErrorMessage(NewValue: string);
begin
  if errorMessage <> '' then
  errorMessage := GetErrorMessage + sLineBreak + NewValue
  else
    errorMessage := NewValue;
end;

procedure RegisterMachine.ProcessFile;
var
  tempStr: string;
  i: integer;
begin
  GetRawStringData.Sort;
  i := 0;
  while i <> GetRawStringData.Count do
  begin
    tempStr := GetRawStringData.Strings[i];
    DeleteComments(tempStr, '//');
    GetRawStringData.Strings[i] := tempStr;
    Trim(GetRawStringData.Strings[i]);
    if GetRawStringData.Strings[i] = '' then
    begin
      GetRawStringData.Delete(i);
    end
    else
      i := i + 1;
  end;
end;

procedure RegisterMachine.RenderRegisters;
var
  amount, i: integer;
begin
  amount := 0;
  for i := 0 to Length(GetProgramData) - 1 do
  begin
    if AnsiMatchText(GetProgramData[i].command, RegisterCommands) and
      (GetProgramData[i].Value > amount) then
      amount := GetProgramData[i].Value;
  end;
  SetLength(registerData, amount + 1); //one extra for the c(0)

end;

procedure RegisterMachine.RenderCommands;
var
  i, index: integer;
  actualLine: commandLine;
begin
  for i := 0 to GetRawStringData.Count - 1 do
  begin
    try
      index := StrToInt(ExtractDelimited(1, GetRawStringData.Strings[i], [' ']));
    except
      SetErrorMessage('illegal index ' + IntToStr(index));
    end;

    actualLine.command := ExtractDelimited(2, GetRawStringData.Strings[i], [' ']);
    if not AnsiMatchText(actualLine.command, commands) then
    begin
      SetErrorMessage('illegal command on index ' + IntToStr(index));
    end;

    try
      if UpperCase(actualLine.command) <> 'END' then
      begin
        actualLine.Value :=
          StrToInt(ExtractDelimited(3, GetRawStringData.Strings[i], [' ']));
          if actualLine.Value < 0 then
          SetErrorMessage('illegal value on index ' + IntToStr(index));
      end
      else
        actualLine.Value := -1;
    except
      SetErrorMessage('illegal value on index ' + IntToStr(index));
    end;

    SetProgramData(actualLine, index);
  end;

  for i := 0 to Length(GetProgramData) do
  begin
    if GetProgramData[i].Value = 0 then
    begin
      SetErrorMessage('index ' + IntToStr(i) + ' is not initialized');
    end;
  end;
end;

procedure RegisterMachine.Execute(regs: registerArray);
var
  line : commandLine;
  b, i : Integer;
begin
  SetLength(executeLog, 1);
  SetRegisterData(0, 0);
  for i := 0 to High(regs) do
  begin
  SetRegisterData(regs[i], i + 1);
  end;

  i:= 1;
  b := 0;
  line.command:= ' ';
  line.Value:= 0;
  with GetExecuteLog[0] do
  begin
  sysOutput:= 'Anfangszustand';
  registers := Copy(GetRegisterData, 0, Length(GetRegisterData));
  command.command := '';
  command.value := 0;
  b := '';
  end;
    while line.command <> 'END' do
    begin
      line := GetProgramData[b];
         SetLength(executeLog, i + 1);
         GetExecuteLog[i].b := IntToStr(b);
         case line.command of
         'LOAD' : GetExecuteLog[i].sysOutput:= LOAD(line.value);
         'STORE' : GetExecuteLog[i].sysOutput:= STORE(line.value);
         'ADD' : GetExecuteLog[i].sysOutput:= ADD(line.value);
         'SUB' : GetExecuteLog[i].sysOutput:= SUB(line.value);
         'MULT' : GetExecuteLog[i].sysOutput:= MULT(line.value);
         'DIV' : GetExecuteLog[i].sysOutput:= &DIV(line.value);
         'CLOAD' : GetExecuteLog[i].sysOutput:= CLOAD(line.value);
         'CADD' : GetExecuteLog[i].sysOutput:= CADD(line.value);
         'CSUB' : GetExecuteLog[i].sysOutput:= CSUB(line.value);
         'CMULT' : GetExecuteLog[i].sysOutput:= CMULT(line.value);
         'CDIV' : GetExecuteLog[i].sysOutput:= CDIV(line.value);
         'END' : GetExecuteLog[i].sysOutput:= &END;
         'GOTO' : GetExecuteLog[i].sysOutput:= &GOTO(line.value, b);
         'JZERO' : GetExecuteLog[i].sysOutput:= JZERO(line.value, b);
         'JNZERO' : GetExecuteLog[i].sysOutput:= JNZERO(line.value, b);
         end;

         GetExecuteLog[i].command := line;
         GetExecuteLog[i].registers := Copy(GetRegisterData, 0, Length(GetRegisterData));
         if not AnsiMatchText(line.command, moveCommands) then
         b := b + 1;
         i := i + 1;
    end;
end;

end.
