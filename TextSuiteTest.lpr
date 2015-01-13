program TextSuiteTest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uMainForm, TextSuite, TextSuiteClasses, TextSuiteImports, TextSuitePostProcess,
  TextSuiteTTFUtils, TextSuiteVersion, TextSuiteWideUtils, utsTextSuite;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

