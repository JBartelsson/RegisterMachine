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



  { RegisterMachine }

  RegisterMachine = class

  private
    registerData: registerArray;
    programData: commandList;
    rawStringData: TStringList;
    errorMessage: string;

  public
    constructor Create(rawStringList: TStringList);
    function GetRegisterData: registerArray;
    procedure SetRegisterData(NewValue: integer; Index: integer);
    function GetProgramData: commandList;
    procedure SetProgramData(NewValue: commandLine; Index: integer);
    function GetRawStringData: TStringList;
    procedure DeleteComments(var s: string; CommentSymbol: string);
    function GetErrorMessage: string;
    procedure SetErrorMessage(NewValue: string);
    procedure ProcessFile;
    procedure RenderRegisters;
    procedure RenderCommands;
  const
    commands: array[0..14] of string =
      ('LOAD', 'STORE', 'ADD', 'SUB', 'MULT', 'DIV', 'GOTO', 'END', 'CLOAD',
      'CADD', 'CMULT', 'CSUB', 'CDIV', 'JZERO', 'JNZERO');
    RegisterCommands: array[0..5] of string =
      ('LOAD', 'STORE', 'ADD', 'SUB', 'MULT', 'DIV');


  end;

implementation

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
        actualLine.Value :=
          StrToInt(ExtractDelimited(3, GetRawStringData.Strings[i], [' ']))
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

end.
