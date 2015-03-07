unit utsTypes;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils;

type
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

  TtsFontStyle = (
    tsStyleBold,
    tsStyleItalic,
    tsStyleUnderline,
    tsStyleStrikeout);
  TtsFontStyles = set of TtsFontStyle;

  TtsVertAlignment = (
    tsVertAlignTop,
    tsVertAlignCenter,
    tsVertAlignBottom);

  TtsHorzAlignment = (
    tsHorzAlignLeft,
    tsHorzAlignCenter,
    tsHorzAlignRight,
    tsHorzAlignJustify);

  TtsFormat = (
    tsFormatEmpty,
    tsFormatRGBA8,
    tsFormatLumAlpha8,
    tsFormatAlpha8,
    tsFormatLum8);

  TtsAntiAliasing = (
    tsAANone,
    tsAANormal);

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
  TtsImageModeFunc = function(const aSource, aDest: Single): Single;

  TtsFontProperties = packed record
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

  TtsPosition = packed record
    x, y: Integer;
  end;
  PtsPosition = ^TtsPosition;

  TtsPositionF = packed record
    x, y: Single;
  end;
  PtsPositionF = ^TtsPositionF;

  TtsRect = packed record
    case Byte of
      0: (TopLeft: TtsPosition; BottomRight: TtsPosition);
      1: (Left, Top, Right, Bottom: Integer);
  end;
  PtsRect = ^TtsRect;

  TtsRectF = packed record
    case Byte of
      0: (TopLeft: TtsPositionF; BottomRight: TtsPositionF);
      1: (Left, Top, Right, Bottom: Single);
  end;
  PtsRectF = ^TtsRectF;

  TtsColor4f = packed record
    case Boolean of
      true:  (r, g, b, a: Single);
      false: (arr: array[0..3] of Single);
  end;
  PtsColor4f = ^TtsColor4f;

  TtsColor4ub = packed record
    case Boolean of
      true:  (r, g, b, a: Byte);
      false: (arr: array[0..3] of Byte);
  end;
  PtsColor4ub = ^TtsColor4ub;

  TtsVector4f = array[0..3] of Single;
  TtsMatrix4f = array[0..3] of TtsVector4f;

  TtsTextMetric = packed record
    Ascent: Integer;
    Descent: Integer;
    ExternalLeading: Integer;
    BaseLineOffset: Integer;
    CharSpacing: Integer;
    LineHeight: Integer;
    LineSpacing: Integer;
  end;

  TtsBlendFunc = function(const aSrc, aDst: TtsColor4f): TtsColor4f;

const
  TS_CHANNELS_RGB:  TtsColorChannels = [tsChannelRed, tsChannelGreen, tsChannelBlue];
  TS_CHANNELS_RGBA: TtsColorChannels = [tsChannelRed, tsChannelGreen, tsChannelBlue, tsChannelAlpha];

  TS_MODES_REPLACE_ALL:    TtsImageModes = (tsModeReplace,  tsModeReplace,  tsModeReplace,  tsModeReplace);
  TS_MODES_MODULATE_ALL:   TtsImageModes = (tsModeModulate, tsModeModulate, tsModeModulate, tsModeModulate);
  TS_MODES_MODULATE_ALPHA: TtsImageModes = (tsModeReplace,  tsModeReplace,  tsModeReplace,  tsModeModulate);

  TS_MATRIX_IDENTITY: TtsMatrix4f = ((1, 0, 0, 0), (0, 1, 0, 0), (0, 0, 1, 0), (0, 0, 0, 1));

function tsColor4f(r, g, b, a: Single): TtsColor4f;
function tsModes(r, g, b, a: TtsImageMode): TtsImageModes;
function tsRect(const l, t, r, b: Integer): TtsRect;
function tsPosition(const x, y: Integer): TtsPosition;
function tsPositionF(const x, y: Single): TtsPositionF;
function tsVector4f(X, Y, Z, W: Single): TtsVector4f;
function tsMatrix4f(X, Y, Z, P: TtsVector4f): TtsMatrix4f;

function  tsFormatSize(const aFormat: TtsFormat): Integer;
procedure tsFormatMap(const aFormat: TtsFormat; var aData: PByte; const aColor: TtsColor4f);
procedure tsFormatUnmap(const aFormat: TtsFormat; var aData: PByte; out aColor: TtsColor4f);

function tsImageModeFuncIgnore(const aSource, aDest: Single): Single;
function tsImageModeFuncReplace(const aSource, aDest: Single): Single;
function tsImageModeFuncModulate(const aSource, aDest: Single): Single;

function tsBlendFundAlpha(const aSrc, aDst: TtsColor4f): TtsColor4f;
function tsBlendFundAdditive(const aSrc, aDst: TtsColor4f): TtsColor4f;
function tsBlendFundAdditiveAlpha(const aSrc, aDst: TtsColor4f): TtsColor4f;

implementation

uses
  Math;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsColor4f(r, g, b, a: Single): TtsColor4f;
begin
  result.r := r;
  result.g := g;
  result.b := b;
  result.a := a;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsModes(r, g, b, a: TtsImageMode): TtsImageModes;
begin
  result[tsChannelRed]   := r;
  result[tsChannelGreen] := g;
  result[tsChannelBlue]  := b;
  result[tsChannelAlpha] := a;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsRect(const l, t, r, b: Integer): TtsRect;
begin
  result.Left   := l;
  result.Top    := t;
  result.Right  := r;
  result.Bottom := b;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsPosition(const x, y: Integer): TtsPosition;
begin
  result.x := x;
  result.y := y;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsPositionF(const x, y: Single): TtsPositionF;
begin
  result.x := x;
  result.y := y;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsVector4f(X, Y, Z, W: Single): TtsVector4f;
begin
  result[0] := X;
  result[1] := Y;
  result[2] := Z;
  result[3] := W;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsMatrix4f(X, Y, Z, P: TtsVector4f): TtsMatrix4f;
begin
  result[0] := X;
  result[1] := Y;
  result[2] := Z;
  result[3] := P;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsFormatSize(const aFormat: TtsFormat): Integer;
begin
  case aFormat of
    tsFormatRGBA8:      result := 4;
    tsFormatLumAlpha8:  result := 2;
    tsFormatAlpha8:     result := 1;
    tsFormatLum8:       result := 1;
  else
    result := 0;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure tsFormatMap(const aFormat: TtsFormat; var aData: PByte; const aColor: TtsColor4f);
var
  i: Integer;
  s: Single;
begin
  case aFormat of
    tsFormatRGBA8: begin
      for i := 0 to 3 do begin
        aData^ := Trunc($FF * min(aColor.arr[i], 1.0));
        inc(aData);
      end;
    end;

    tsFormatLumAlpha8: begin
      s := 0.30 * min(aColor.r, 1.0) +
           0.59 * min(aColor.g, 1.0) +
           0.11 * min(aColor.b, 1.0);
      aData^ := Trunc($FF * s);
      inc(aData);
      aData^ := Trunc($FF * min(aColor.a, 1.0));
      inc(aData);
    end;

    tsFormatAlpha8: begin
      aData^ := Trunc($FF * min(aColor.a, 1.0));
      inc(aData);
    end;

    tsFormatLum8: begin
      s := 0.30 * min(aColor.r, 1.0) +
           0.59 * min(aColor.g, 1.0) +
           0.11 * min(aColor.b, 1.0);
      aData^ := Trunc($FF * s);
      inc(aData);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure tsFormatUnmap(const aFormat: TtsFormat; var aData: PByte; out aColor: TtsColor4f);
var
  i: Integer;
begin
  case aFormat of
    tsFormatRGBA8: begin
      for i := 0 to 3 do begin
        aColor.arr[i] := aData^ / $FF;
        inc(aData);
      end;
    end;

    tsFormatLumAlpha8: begin
      aColor.r := aData^ / $FF;
      aColor.g := aData^ / $FF;
      aColor.b := aData^ / $FF;
      inc(aData);
      aColor.a := aData^ / $FF;
      inc(aData);
    end;

    tsFormatAlpha8: begin
      aColor.r := 1.0;
      aColor.g := 1.0;
      aColor.b := 1.0;
      aColor.a := aData^ / $FF;
      inc(aData);
    end;

    tsFormatLum8: begin
      aColor.r := aData^ / $FF;
      aColor.g := aData^ / $FF;
      aColor.b := aData^ / $FF;
      aColor.a := 1.0;
      inc(aData);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsImageModeFuncIgnore(const aSource, aDest: Single): Single;
begin
  result := aDest;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsImageModeFuncReplace(const aSource, aDest: Single): Single;
begin
  result := aSource;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsImageModeFuncModulate(const aSource, aDest: Single): Single;
begin
  result := aSource * aDest;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsBlendFundAlpha(const aSrc, aDst: TtsColor4f): TtsColor4f;
var
  i: Integer;
begin
  for i := 0 to 2 do
    result.arr[i] := aSrc.arr[i] * aSrc.a + aDst.arr[i] * (1 - aSrc.a);
  result.a := aSrc.a + aDst.a * (1 - aSrc.a);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsBlendFundAdditive(const aSrc, aDst: TtsColor4f): TtsColor4f;
var
  i: Integer;
begin
  for i := 0 to 3 do
    result.arr[i] := aSrc.arr[i] + aDst.arr[i];
end;

function tsBlendFundAdditiveAlpha(const aSrc, aDst: TtsColor4f): TtsColor4f;
var
  i: Integer;
begin
  for i := 0 to 2 do
    result.arr[i] := aSrc.arr[i] * aSrc.a + aDst.arr[i];
  result.a := aDst.a;
end;

end.

