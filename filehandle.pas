unit FileHandle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Process;

procedure SaveTextFile(const filename: string; const s: TStrings);
function OpenTextFile(const filename: string) : TStrings;
procedure StartProcess(const ExecutableFilename, DataFilename: string);

implementation

procedure StartProcess(const ExecutableFilename, DataFilename: string);
var Pr: TProcess;
begin
  Pr := TProcess.Create(nil);
  Pr.Executable := ExecutableFilename;
  Pr.Parameters.Add('"' + DataFilename + '"');
  Pr.ShowWindow := swoShowNormal;
  Pr.Execute;
  Pr.Free;
end;

procedure SaveTextFile(const filename: string; const s: TStrings);
var
  tfOut: TextFile;
  i: integer;
begin
  AssignFile(tfOut, filename);
  try
    rewrite(tfOut);
    for i := 0 to s.Count - 1 do
        writeln(tfOut, s[i]);
    CloseFile(tfOut);
  except
    on E: EInOutError do
      ShowMessage('File handling error occurred. Details: ' + E.ClassName + '/' + E.Message);
  end;
end;

function OpenTextFile(const Filename: string) : TStrings;
var
  tfIn: TextFile;
  LineIn: string;
begin
  OpenTextFile := TStringList.Create();
  OpenTextFile.Clear;
  AssignFile(tfIn, Filename);
  try
    reset(tfIn);
    while not eof(tfIn) do
    begin
      readln(tfIn, LineIn);
      OpenTextFile.Add(LineIn);
    end;
    CloseFile(tfIn);
  except
    on E: EInOutError do
      ShowMessage('File handling error occurred. Details: ' + E.ClassName + '/' + E.Message);
  end;
end;


end.
