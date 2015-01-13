unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, uglcContext, TextSuite, uglcTypes, utsTextSuite;

type
  TMainForm = class(TForm)
    ApplicationProperties: TApplicationProperties;
    procedure ApplicationPropertiesIdle(Sender: TObject; var Done: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    fContext: TglcContext;
    fTextSuiteContext: tsContextID;
    fFontID: tsFontID;

    ftsContext:  TtsContext;
    ftsRenderer: TtsRenderer;
    ftsCreator:  TtsFontCreator;
    ftsFont:     TtsFont;

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

  tsInit(TS_INIT_TEXTSUITE or TS_INIT_OPENGL or TS_INIT_GDI);
  tsContextCreate(@fTextSuiteContext);
  tsSetParameteri(TS_RENDERER, TS_RENDERER_OPENGL);
  tsSetParameteri(TS_CREATOR, TS_CREATOR_GDI);
  tsContextBind(fTextSuiteContext);
  tsFontCreateCreatorA('ttf/calibri.ttf', 24, 0, TS_ANTIALIASING_NORMAL, TS_DEFAULT, @fFontID);
  tsFontBind(fFontID);

  ftsContext  := TtsContext.Create;
  ftsRenderer := TtsRenderer.Create(ftsContext, tsFormatRGBA8);
  ftsCreator  := TtsFontCreator.Create;
  ftsFont     := TtsFont.Create(ftsRenderer, ftsCreator, '', '', '', '', 12, 0, 0, [], tsAANormal);
end;

procedure TMainForm.FormPaint(Sender: TObject);
begin
  Render;
end;

procedure TMainForm.Render;
var
  block: TtsTextBlock;
begin
  glViewport(0, 0, ClientWidth, ClientHeight);
  glClearColor(0, 0, 0, 0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glOrtho(0, ClientWidth, ClientHeight, 0, -10, 10);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;

  glDisable(GL_CULL_FACE);
  glDisable(GL_DEPTH_TEST);
  glEnable(GL_BLEND);

  glcBlendFunc(TglcBlendMode.bmAdditiveAlphaBlend);
  //tsTextBeginBlock(10, 10, ClientWidth-10, ClientHeight-10, TS_ALIGN_BLOCK);
  //tsTextOutA(TEST_STRING);
  //tsTextEndBlock;

  block := ftsRenderer.BeginBlock(10, 10, ClientWidth-10, ClientHeight-10, [tsBlockFlagWordWrap]);
  try
    block.ChangeFont(ftsFont);
    block.TextOutW('test'#13#10#13#10'test'#13#13'test'#10#10'test'#13#10#10'test'#13#13#10);
  finally
    ftsRenderer.EndBlock(block);
  end;

  fContext.SwapBuffers;
end;

procedure TMainForm.ApplicationPropertiesIdle(Sender: TObject; var Done: Boolean);
begin
  Render;
  Done := false;
end;

end.

