unit uMainForm;

{$mode objfpc}{$H+}

{.$DEFINE USE_OLD_TS}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, uglcContext, TextSuite, uglcTypes,
  utsTextSuite, utsTypes, utsFontCreatorGDI, utsRendererOpenGL;

type
  TMainForm = class(TForm)
    ApplicationProperties: TApplicationProperties;
    procedure ApplicationPropertiesIdle(Sender: TObject; var Done: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    fFrameTime: QWord;
    fFrameCount: Integer;
    fSecTime: QWord;

    fContext: TglcContext;
    {$IFDEF USE_OLD_TS}
    fTextSuiteContext: tsContextID;
    fFontID: tsFontID;
    {$ELSE}
    ftsContext:   TtsContext;
    ftsRenderer:  TtsRendererOpenGL;
    ftsGenerator: TtsFontGeneratorGDI;
    ftsFont:      TtsFont;
    {$ENDIF}
    procedure Render;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  dglOpenGL;

const
  TEST_STRING = 'Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.';

procedure TMainForm.FormCreate(Sender: TObject);
var
  pf: TglcContextPixelFormatSettings;
begin
  pf := TglcContext.MakePF();
  fContext := TglcContext.GetPlatformClass.Create(self, pf);
  fContext.BuildContext;
  {$IFDEF USE_OLD_TS}
  tsInit(TS_INIT_TEXTSUITE or TS_INIT_OPENGL or TS_INIT_GDI);
  tsContextCreate(@fTextSuiteContext);
  tsSetParameteri(TS_RENDERER, TS_RENDERER_OPENGL);
  tsSetParameteri(TS_CREATOR, TS_CREATOR_GDI_FACENAME);
  tsContextBind(fTextSuiteContext);
  tsFontCreateCreatorA('Calibri', 25, 0, TS_ANTIALIASING_NORMAL, TS_DEFAULT, @fFontID);
  tsFontBind(fFontID);
  {$ELSE}
  ftsContext   := TtsContext.Create;
  ftsRenderer  := TtsRendererOpenGL.Create(ftsContext, tsFormatRGBA8);
  ftsGenerator := TtsFontGeneratorGDI.Create;
  ftsFont      := ftsGenerator.GetFontByName('Calibri', ftsRenderer, 25, [], tsAANormal);
  ftsFont.LineSpacing := 0;
  {$ENDIF}
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  {$IFNDEF USE_OLD_TS}
  FreeAndNil(ftsFont);
  FreeAndNil(ftsGenerator);
  FreeAndNil(ftsRenderer);
  FreeAndNil(ftsContext);
  {$ENDIF}
end;

procedure TMainForm.FormPaint(Sender: TObject);
begin
  Render;
end;

procedure TMainForm.Render;
var
  block: TtsTextBlock;
  t: QWord;
  dif: Integer;
begin
  t := GetTickCount64;
  if (fFrameTime <> 0) then begin
    dif := t - fFrameTime;
    inc(fFrameCount, 1);
    inc(fSecTime, dif);
    if (fSecTime > 1000) then begin
      Caption := IntToStr(fFrameCount) + ' FPS';
      fFrameCount := 0;
      dec(fSecTime, 1000);
    end;
  end;
  fFrameTime := t;

  glViewport(0, 0, ClientWidth, ClientHeight);
  glClearColor(0, 0, 0, 0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glOrtho(0, ClientWidth, ClientHeight, 0, -10, 10);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;

  glEnable(GL_BLEND);
  glcBlendFunc(TglcBlendMode.bmAdditiveAlphaBlend);

  {$IFDEF USE_OLD_TS}
  tsTextBeginBlock(0, 0, ClientWidth, ClientHeight, TS_ALIGN_BLOCK);
  tsTextOutA(TEST_STRING);
  tsTextEndBlock;
  {$ELSE}
  block := ftsRenderer.BeginBlock(0, 0, ClientWidth, ClientHeight, [tsBlockFlagWordWrap]);
  try
    block.ChangeFont(ftsFont);
    block.TextOutW(TEST_STRING);
  finally
    ftsRenderer.EndBlock(block);
  end;
  {$ENDIF}
  fContext.SwapBuffers;
end;

procedure TMainForm.ApplicationPropertiesIdle(Sender: TObject; var Done: Boolean);
begin
  Render;
  Done := false;
end;

end.

