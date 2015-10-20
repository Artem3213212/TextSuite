unit utsTypes;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

type
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Enumerations//////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  {$Z4}
  TtsCodePage = (
    tsUTF8,
    tsISO_8859_1,
    tsISO_8859_2,
    tsISO_8859_3,
    tsISO_8859_4,
    tsISO_8859_5,
    tsISO_8859_6,
    tsISO_8859_7,
    tsISO_8859_8,
    tsISO_8859_9,
    tsISO_8859_10,
    tsISO_8859_11,
    tsISO_8859_13,
    tsISO_8859_14,
    tsISO_8859_15,
    tsISO_8859_16,
    tsISO_037,
    tsISO_437,
    tsISO_500,
    tsISO_737,
    tsISO_775,
    tsISO_850,
    tsISO_852,
    tsISO_855,
    tsISO_857,
    tsISO_860,
    tsISO_861,
    tsISO_862,
    tsISO_863,
    tsISO_864,
    tsISO_865,
    tsISO_866,
    tsISO_869,
    tsISO_874,
    tsISO_875,
    tsISO_1026,
    tsISO_1250,
    tsISO_1251,
    tsISO_1252,
    tsISO_1253,
    tsISO_1254,
    tsISO_1255,
    tsISO_1256,
    tsISO_1257,
    tsISO_1258);

  TtsFormat = (
    tsFormatEmpty,
    tsFormatRGBA8,
    tsFormatLumAlpha8,
    tsFormatAlpha8,
    tsFormatLum8);

  TtsVertAlignment = (
    tsVertAlignTop,
    tsVertAlignCenter,
    tsVertAlignBottom);

  TtsHorzAlignment = (
    tsHorzAlignLeft,
    tsHorzAlignCenter,
    tsHorzAlignRight,
    tsHorzAlignJustify);

  TtsClipping = (
    tsClipNone,           // no clipping
    tsClipWordBorder,     // draw all words that have at least one pixel inside the box
    tsClipCharBorder,     // draw all chars that have at least one pixel inside the box
    tsClipWordComplete,   // draw all words that are completly inside the box
    tsClipCharComplete    // draw all chars that are completly inside the box
  );

  TtsAntiAliasing = (
    tsAANone,
    tsAANormal);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Flags/////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsBlockFlag = (
    tsBlockFlagWordWrap
  );
  TtsBlockFlags = set of TtsBlockFlag;

  TtsFontStyle = (
    tsStyleBold,
    tsStyleItalic,
    tsStyleUnderline,
    tsStyleStrikeout);
  TtsFontStyles = set of TtsFontStyle;

  TtsColorChannel = (
    tsChannelRed,
    tsChannelGreen,
    tsChannelBlue,
    tsChannelAlpha);
  TtsColorChannels = set of TtsColorChannel;

  TtsImageMode = (
    tsModeIgnore,
    tsModeReplace,
    tsModeModulate);
  TtsImageModes = array[TtsColorChannel] of TtsImageMode;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Structures////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsRenderRef = Pointer;
  PtsCodePageValues = ^TtsCodePageValues;
  TtsCodePageValues = array [AnsiChar] of word;

  PtsColor4f = ^TtsColor4f;
  TtsColor4f = packed record
  case Boolean of
    true:  (r, g, b, a: Single);
    false: (arr: array[0..3] of Single);
  end;

  PtsPosition = ^TtsPosition;
  TtsPosition = packed record
    x, y: Integer;
  end;

  PtsRect = ^TtsRect;
  TtsRect = packed record
  case Byte of
    0: (TopLeft: TtsPosition; BottomRight: TtsPosition);
    1: (Left, Top, Right, Bottom: Integer);
  end;

  TtsVector4f = array[0..3] of Single;
  TtsMatrix4f = array[0..3] of TtsVector4f;

  TtsGlyphMetric = packed record
    GlyphOrigin: TtsPosition;
    GlyphRect: TtsRect;
    Advance: Integer;
  end;

  TtsTextMetric = packed record
    Ascent: Integer;
    Descent: Integer;
    ExternalLeading: Integer;
    BaseLineOffset: Integer;
    CharSpacing: Integer;
    LineHeight: Integer;
    LineSpacing: Integer;
  end;

  TtsFontMetric = packed record
    Fontname: String;
    Copyright: String;
    FaceName: String;
    StyleName: String;
    FullName: String;

    Size: Integer;
    Style: TtsFontStyles;
    AntiAliasing: TtsAntiAliasing;
    DefaultChar: WideChar;

    Ascent: Integer;
    Descent: Integer;
    ExternalLeading: Integer;
    BaseLineOffset: Integer;

    UnderlinePos: Integer;
    UnderlineSize: Integer;
    StrikeoutPos: Integer;
    StrikeoutSize: Integer;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Callbacks/////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsBlendValueFunc = function(const aSrc, aDst: Single): Single;
  TtsBlendColorFunc = function(const aSrc, aDst: TtsColor4f): TtsColor4f;

implementation

end.

