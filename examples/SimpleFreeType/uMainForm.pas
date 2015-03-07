unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  uglcContext,
  utsTextSuite, utsRendererOpenGL, utsFontCreatorFreeType, utsTypes;

type
  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    fContext: TglcContext;
    ftsContext: TtsContext;
    ftsRenderer: TtsRendererOpenGL;
    ftsCreator: TtsFontGeneratorFreeType;
    ftsFont: TtsFont;
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
  TEST_TEXT = 'Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.';

procedure TMainForm.FormCreate(Sender: TObject);
var
  pf: TglcContextPixelFormatSettings;
begin
  pf := TglcContext.MakePF();
  fContext := TglcContext.GetPlatformClass.Create(self, pf);
  fContext.BuildContext;

  ftsContext  := TtsContext.Create;
  ftsRenderer := TtsRendererOpenGL.Create(ftsContext, tsFormatAlpha8);
  ftsCreator  := TtsFontGeneratorFreeType.Create(ftsContext);
  ftsFont     := ftsCreator.GetFontByFile(ExtractFilePath(Application.ExeName) + '../Prototype.ttf', ftsRenderer, 20, [], tsAANormal);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ftsFont);
  FreeAndNil(ftsCreator);
  FreeAndNil(ftsRenderer);
  FreeAndNil(ftsContext);
  FreeAndNil(fContext);
end;

procedure TMainForm.FormPaint(Sender: TObject);
begin
  if Assigned(fContext) then begin
    Render;
    fContext.SwapBuffers;
  end;
end;

procedure TMainForm.Render;
var
  block: TtsTextBlock;
begin
  glClearColor(0, 0, 0, 0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  glViewport(0, 0, ClientWidth, ClientHeight);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glOrtho(0, ClientWidth, ClientHeight, 0, -10, 10);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;

  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  block := ftsRenderer.BeginBlock(10, 10, ClientWidth-20, ClientHeight-20, [tsBlockFlagWordWrap]);
  try
    block.HorzAlign := tsHorzAlignJustify;
    block.ChangeFont(ftsFont);
    block.ChangeColor(tsColor4f(1.0, 1.0, 1.0, 1.0));
    block.TextOutW(TEST_TEXT);
  finally
    ftsRenderer.EndBlock(block);
  end;
end;

end.

