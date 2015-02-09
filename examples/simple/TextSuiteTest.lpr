program TextSuiteTest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, sysutils, Forms, uMainForm,
  utsFontCreatorGDI, utsUtils, utsTypes, utsTtfUtils, utsTextSuite, utsRendererOpenGL;

{$R *.res}

var
  HeapTraceLogFile: String;
begin
  HeapTraceLogFile := ExtractFilePath(Application.ExeName) + 'heaptrace.log';
  if FileExists(HeapTraceLogFile) then
    DeleteFile(HeapTraceLogFile);
  SetHeapTraceOutput(HeapTraceLogFile);

  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

