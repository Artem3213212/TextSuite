unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  uglcContext,
  utsTextSuite, utsUtils, utsConstants;

type
  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    fContext: TglcContext;
    ftsContext: TtsContext;
    ftsRenderer: TtsRendererOpenGL;
    ftsCreator: TtsFontCreatorGDI;
    ftsPostProcessor1: TtsPostProcessorList;
    ftsPostProcessor2: TtsPostProcessorList;
    ftsFont1: TtsFont;
    ftsFont2: TtsFont;
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
  TEST_TEXT = 'Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum.';

procedure TMainForm.FormCreate(Sender: TObject);
var
  pf: TglcContextPixelFormatSettings;
  pp: TtsPostProcessor;
  img: TtsImage;

const
  PATTER_DATA: array[0..15] of Byte = (
    $FF, $BF, $7F, $BF,
    $BF, $FF, $BF, $7F,
    $7F, $BF, $FF, $BF,
    $BF, $7F, $BF, $FF);

begin
  pf := TglcContext.MakePF();
  fContext := TglcContext.GetPlatformClass.Create(self, pf);
  fContext.BuildContext;

  ftsContext  := TtsContext.Create;
  ftsRenderer := TtsRendererOpenGL.Create(ftsContext, TtsFormat.tsFormatRGBA8);

  // post processors
  ftsPostProcessor1 := TtsPostProcessorList.Create(ftsContext, true);
  ftsPostProcessor2 := TtsPostProcessorList.Create(ftsContext, true);

  pp := TtsPostProcessorFillColor.Create(ftsContext, tsColor4f(0, 0, 0, 1), TS_IMAGE_MODES_REPLACE_ALL, TS_COLOR_CHANNELS_RGB);
  pp.AddChars(TtsCharRangeUsage.tsUsageExclude, 'Lorem');
  ftsPostProcessor1.Add(pp);

  pp := TtsPostProcessorFillColor.Create(ftsContext, tsColor4f(1.0, 0.0, 0.0, 1.0), TS_IMAGE_MODES_MODULATE_ALL, TS_COLOR_CHANNELS_RGB);
  pp.AddChars(TtsCharRangeUsage.tsUsageInclude, 'Lorem');
  ftsPostProcessor1.Add(pp);

  img := TtsImage.Create;
  img.CreateEmpty(TtsFormat.tsFormatAlpha8, 4, 4);
  Move(PATTER_DATA[0], img.Data^, 16);
  pp := TtsPostProcessorFillPattern.Create(ftsContext, img, true, tsPosition(0, 0), TS_IMAGE_MODES_MODULATE_ALL, TS_COLOR_CHANNELS_RGBA);
  pp.AddChars(TtsCharRangeUsage.tsUsageInclude, 'Lorem');
  ftsPostProcessor2.Add(pp);

  pp := TtsPostProcessorFillColor.Create(ftsContext, tsColor4f(0, 0, 0.5, 1), TS_IMAGE_MODES_REPLACE_ALL, TS_COLOR_CHANNELS_RGB);
  pp.AddChars(TtsCharRangeUsage.tsUsageExclude, 'e');
  ftsPostProcessor2.Add(pp);

  pp := TtsPostProcessorBorder.Create(ftsContext, 3.0, 0.5, tsColor4f(0.0, 0.5, 0.0, 1.0), true);
  pp.AddChars(TtsCharRangeUsage.tsUsageInclude, 'e');
  ftsPostProcessor2.Add(pp);

  // font creator and fonts
  ftsCreator := TtsFontCreatorGDI.Create(ftsContext);

  ftsFont1 := ftsCreator.GetFontByFile(ExtractFilePath(Application.ExeName) + '../Prototype.ttf', 40, [], TtsAntiAliasing.tsAANormal);
  ftsFont1.PostProcessor := ftsPostProcessor1;

  ftsFont2 := ftsCreator.GetFontByFile(ExtractFilePath(Application.ExeName) + '../Prototype.ttf', 40, [TtsFontStyle.tsStyleStrikeout], TtsAntiAliasing.tsAANormal);
  ftsFont2.PostProcessor := ftsPostProcessor2;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ftsFont2);
  FreeAndNil(ftsFont1);
  FreeAndNil(ftsCreator);
  FreeAndNil(ftsPostProcessor2);
  FreeAndNil(ftsPostProcessor1);
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
  glClearColor(1, 1, 1, 1);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  glViewport(0, 0, ClientWidth, ClientHeight);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glOrtho(0, ClientWidth, ClientHeight, 0, -10, 10);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;

  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  block := ftsRenderer.BeginBlock(10, 10, ClientWidth-20, ClientHeight-20, [TtsBlockFlag.tsBlockFlagWordWrap]);
  try
    block.HorzAlign := TtsHorzAlignment.tsHorzAlignJustify;
    block.ChangeFont(ftsFont1);
    block.ChangeColor(tsColor4f(1.0, 1.0, 1.0, 1.0));
    block.TextOutW(TEST_TEXT + sLineBreak);

    block.ChangeFont(ftsFont2);
    block.TextOutW(TEST_TEXT);
  finally
    ftsRenderer.EndBlock(block);
  end;
end;

end.

