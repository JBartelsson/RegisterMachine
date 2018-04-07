unit uRegisterMachine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, strutils;

type
  commandLine = record
    command: string;
    Value: Word;
  end;

type
  registerArray = array of Longword;
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
    function LOAD(c: Word): String;
    function STORE(c : Word) : String;
    function ADD(c : Word) : String;
    function SUB(c : Word) : String;
    function MULT(c : Word) : String;
    function &DIV(c : Word) : String;
    function CLOAD(c: Word): String;
    function CADD(c : Longword) : String;
    function CSUB(c : Longword) : String;
    function CMULT(c : Longword) : String;
    function CDIV(c : Longword) : String;
    function JZERO(var j, b : Word) : String;
    function JNZERO(var j, b : Word) : String;
    function &GOTO(var j, b : Word) : String;
    function &END : String;

  public
    constructor Create(rawStringList: TStringList);
    class function verifyCommand(s: String): String;
    function GetRegisterData: registerArray;
    procedure SetRegisterData(NewValue: Longword; Index: integer);
    procedure SetRegisterData(NewArray: registerArray);
    function GetProgramData: commandList;
    procedure SetProgramData(NewValue: commandLine; Index: integer);
    function GetRawStringData: TStringList;
    class procedure DeleteComments(var s: string; CommentSymbol: string);
    function GetErrorMessage: string;
    function GetExecuteLog: commandLogSum;
    procedure SetErrorMessage(NewValue: string);
    procedure ProcessFile;
    procedure RenderRegisters;
    procedure RenderCommands;
    function Execute(regs : registerArray): boolean;
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
function RegisterMachine.LOAD(c: Word): String;
begin
  SetRegisterData(GetRegisterData[c], 0);
  Result := 'A lädt Zahl ' + IntToStr(GetRegisterData[c]) + ' aus c(' + IntToStr(c) + ')';
end;

function RegisterMachine.STORE(c: Word): String;
begin
   SetRegisterData(GetRegisterData[0], c);
     Result := 'A speichert Zahl ' + IntToStr(GetRegisterData[c]) + ' in c(' + IntToStr(c) + ')';
end;

function RegisterMachine.ADD(c: Word): String;
begin
   SetRegisterData(GetRegisterData[0] + GetRegisterData[c], 0);
   Result := 'A wird um ' + inttoStr(GetRegisterData[c]) + ' aus c(' + IntToStr(c) + ') erhöht';
end;

function RegisterMachine.SUB(c: Word): String;
var
  res : Integer;
begin
   res := GetRegisterData[0] - GetRegisterData[c];
   if res < 0 then
   begin
   GetRegisterData[0] := 0;
     Result := 'A wird um ' + inttoStr(GetRegisterData[c]) + ' aus c(' + IntToStr(c) + ') bis auf 0 verringert';
   end
   else
   begin
     SetRegisterData(res, 0);
     Result := 'A wird um ' + inttostr(GetRegisterData[c]) + ' aus c(' + IntToStr(c) + ') verringert';
   end;

end;

function RegisterMachine.MULT(c: Word): String;
begin
   SetRegisterData(GetRegisterData[0] * GetRegisterData[c], 0);
   Result := 'A wird mit ' + inttostr(GetRegisterData[c]) + ' aus c(' + IntToStr(c) + ') multipliziert';
end;

function RegisterMachine.&div(c: Word): String;
begin
   if GetRegisterData[c] <> 0 then
   begin
   SetRegisterData(GetRegisterData[0] div GetRegisterData[c], 0);
   Result := 'A wird durch ' + inttostr(GetRegisterData[c]) + ' aus c(' + IntToStr(c) + ') dividiert';
   end
   else Result := 'ERROR';
end;

function RegisterMachine.CLOAD(c: Word): String;
begin
   SetRegisterData(c, 0);
   Result := 'A lädt Konstante ' + IntToStr(c);
end;

function RegisterMachine.CADD(c: Longword): String;
begin
    SetRegisterData(GetRegisterData[0] + c, 0);
    Result := 'A um ' + IntToStr(c) + ' erhöht';
end;

function RegisterMachine.CSUB(c: Longword): String;
begin
    SetRegisterData(GetRegisterData[0] - c, 0);
    Result := 'A um ' + IntToStr(c) + ' verringert';
end;

function RegisterMachine.CMULT(c: Longword): String;
begin
    SetRegisterData(GetRegisterData[0] * c, 0);
    Result := 'A mit ' + IntToStr(c) + ' multipliziert';
end;

function RegisterMachine.CDIV(c: Longword): String;
begin
   if c <> 0 then
   begin
   SetRegisterData(GetRegisterData[0] div c, 0);
   Result := 'A wird durch ' + IntToStr(c) + ' dividiert';
   end
   else Result := 'ERROR';
end;

function RegisterMachine.JZERO(var j, b: Word): String;
begin
    if GetRegisterData[0] = 0 then
    begin
    b := j;
    Result := 'A ist 0 -> Sprung zur' + IntToStr(j) + ' . Programmzeile';
    end
    else
    begin
    Result := 'A ist nicht 0 -> Sprung zur nächsten Programmzeile';
    b := b +1;
    end;
end;

function RegisterMachine.JNZERO(var j, b: Word): String;
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

function RegisterMachine.&goto(var j, b: Word): String;
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

{checks if command is legal
0: legal
ERRORS:
1: illegal index
2: illegal command
3: illegal value
}
class function RegisterMachine.verifyCommand(s: String): String;
var
  temp : Integer;
  line, errM: String;

  procedure addToResult(var s: String; const addS: String);
  begin
    if s <> '' then
      s := s +  sLineBreak + addS
    else
      s := addS;
  end;

begin
  errM := '';
  DeleteComments(s, '//');
  s:= trim(s);
  line := UpperCase(ExtractDelimited(2, s, [' ']));
  if not AnsiMatchText(line, commands) then
    addToResult(errM, s + ': illegal command ' + line);
  if not TryStrToInt(ExtractDelimited(1, s, [' ']), temp) then
    addToResult(errM, s + ': illegal index ' + ExtractDelimited(1, s, [' ']));
  if (line <> 'END') AND (not TryStrToInt(ExtractDelimited(3, s, [' ']), temp)) then
    addToResult(errM, s + ': illegal value ' + ExtractDelimited(3, s, [' ']));
  if (line = 'END') AND (Pos('END',s) <> Length(s) - 2) then
    addToResult(errM, s + ': illegal value ' + ExtractDelimited(3, s, [' ']));
  Result := errM;
end;

function RegisterMachine.GetRegisterData: registerArray;
begin
  Result := registerData;
end;

procedure RegisterMachine.SetRegisterData(NewValue: Longword; Index: integer);
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

class procedure RegisterMachine.DeleteComments(var s: string;
  CommentSymbol: string);
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
  i: integer;
  actualLine: commandLine;
  error : String;
begin
  for i := 0 to GetRawStringData.Count - 1 do
  begin
       error:= verifyCommand(GetRawStringData.Strings[i]);
     if error = '' then
     begin
        actualLine.command := UpperCase(ExtractDelimited(2, GetRawStringData.Strings[i], [' ']));
        if actualLine.command <> 'END' then
        actualLine.Value :=
          StrToInt(ExtractDelimited(3, GetRawStringData.Strings[i], [' ']))
        else
        actualLine.Value:= 0;
        SetProgramData(actualLine, StrtoInt(ExtractDelimited(1, GetRawStringData.Strings[i], [' '])));
     end
     else
     SetErrorMessage(error);
  end;
  for i := 0 to High(GetProgramData) do
  begin
     if GetProgramData[i].command = 'END' then
     exit;
  end;
  SetErrorMessage('no END command found');
end;

function RegisterMachine.Execute(regs: registerArray): boolean;
var
  line : commandLine;
  b : Word;
  i : Integer;
begin
  Result := true;
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

         if i > 1000 then
         begin
         Result := false;
         Break;
         end;
    end;
end;

end.
