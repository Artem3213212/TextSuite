unit utsTextSuite;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$I utsTextSuite.inc}

interface

uses
  Classes, SysUtils,
  {$IFDEF TS_ENABLE_OPENGL_SUPPORT}   utsRendererOpenGL,{$ENDIF}
  {$IFDEF TS_ENABLE_OPENGLES_SUPPORT} utsRendererOpenGLES,{$ENDIF}
  {$IFDEF TS_ENABLE_GDI_SUPPORT}      utsFontCreatorGDI,{$ENDIF}
  {$IFDEF TS_ENABLE_FREETYPE_SUPPORT} utsFontCreatorFreeType,{$ENDIF}
  utsContext, utsFont, utsTypes, utsTextBlock, utsChar, utsImage, utsPostProcessor;

type
  // TextSuite Classes
  TtsContext                  = utsContext.TtsContext;
  TtsFont                     = utsFont.TtsFont;
  TtsTextBlock                = utsTextBlock.TtsTextBlock;
  TtsChar                     = utsChar.TtsChar;
  TtsImage                    = utsImage.TtsImage;
  TtsImageFunc                = utsImage.TtsImageFunc;

  // Post Processor
  TtsCharRangeUsage           = utsPostProcessor.TtsCharRangeUsage;
  TtsPostProcessor            = utsPostProcessor.TtsPostProcessor;  
  TtsPostProcessorList        = utsPostProcessor.TtsPostProcessorList;
  TtsPostProcessorBorder      = utsPostProcessor.TtsPostProcessorBorder;
  TtsPostProcessorFillColor   = utsPostProcessor.TtsPostProcessorFillColor;
  TtsPostProcessorFillPattern = utsPostProcessor.TtsPostProcessorFillPattern;
  TtsPostProcessorShadow      = utsPostProcessor.TtsPostProcessorShadow;

  // Renderer
  {$IFDEF TS_ENABLE_OPENGL_SUPPORT}   TtsRendererOpenGL       = utsRendererOpenGL.TtsRendererOpenGL;{$ENDIF}
  {$IFDEF TS_ENABLE_OPENGLES_SUPPORT} TtsRendererOpenGLES     = utsRendererOpenGLES.TtsRendererOpenGLES;{$ENDIF}

  // FontCreators
  {$IFDEF TS_ENABLE_GDI_SUPPORT}      TtsFontCreatorGDI       = utsFontCreatorGDI.TtsFontCreatorGDI;{$ENDIF}
  {$IFDEF TS_ENABLE_FREETYPE_SUPPORT} TtsFontCreatorFreeType  = utsFontCreatorFreeType.TtsFontCreatorFreeType;{$ENDIF}

  // Utils
  TtsCodePage                 = utsTypes.TtsCodePage;
  TtsFormat                   = utsTypes.TtsFormat;
  TtsVertAlignment            = utsTypes.TtsVertAlignment;
  TtsHorzAlignment            = utsTypes.TtsHorzAlignment;
  TtsClipping                 = utsTypes.TtsClipping;
  TtsAntiAliasing             = utsTypes.TtsAntiAliasing;
  TtsBlockFlag                = utsTypes.TtsBlockFlag;
  TtsBlockFlags               = utsTypes.TtsBlockFlags;
  TtsFontStyle                = utsTypes.TtsFontStyle;
  TtsColorChannel             = utsTypes.TtsColorChannel;
  TtsColorChannels            = utsTypes.TtsColorChannels;
  TtsImageMode                = utsTypes.TtsImageMode;
  TtsImageModes               = utsTypes.TtsImageModes;
  TtsColor4f                  = utsTypes.TtsColor4f;
  PtsColor4f                  = utsTypes.PtsColor4f;
  TtsPosition                 = utsTypes.TtsPosition;
  PtsPosition                 = utsTypes.PtsPosition;
  TtsRect                     = utsTypes.TtsRect;
  PtsRect                     = utsTypes.PtsRect;
  TtsVector4f                 = utsTypes.TtsVector4f;
  TtsMatrix4f                 = utsTypes.TtsMatrix4f;
  TtsGlyphMetric              = utsTypes.TtsGlyphMetric;
  TtsTextMetric               = utsTypes.TtsTextMetric;
  TtsFontMetric               = utsTypes.TtsFontMetric;
  TtsBlendValueFunc           = utsTypes.TtsBlendValueFunc;
  TtsBlendColorFunc           = utsTypes.TtsBlendColorFunc;

implementation

end.

