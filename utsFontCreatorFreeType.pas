unit utsFontCreatorFreeType;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  utsFreeType, utsFontCreator, utsFont, utsTypes, utsImage, utsContext;

type
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsFreeTypeFaceHandle = class
  private
    fFace: FT_Face;
  public
    constructor Create(const aFace: FT_Face);
    destructor Destroy; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsFontFreeType = class(TtsFont)
  private
    fHandle: TtsFreeTypeFaceHandle;
  protected
    {%H-}constructor Create(const aHandle: TtsFreeTypeFaceHandle; const aCreator: TtsFontCreator; const aMetric: TtsFontMetric);
  public
    procedure GetCharImage(const aCharCode: WideChar; const aCharImage: TtsImage; const aFormat: TtsFormat); override;
    function GetGlyphMetrics(const aCharCode: WideChar; out aGlyphOrigin, aGlyphSize: TtsPosition; out aAdvance: Integer): Boolean; override;

    destructor Destroy; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsFontCreatorFreeType = class(TtsFontCreator)
  private
    fHandle: FT_Library;

    procedure LoadNames(const aFace: FT_Face; var aMetric: TtsFontMetric);
    function CreateFont(const aFace: FT_Face; const aSize: Integer; const aStyle: TtsFontStyles; const aAntiAliasing: TtsAntiAliasing): TtsFont;
  public
    function GetFontByFile(const aFilename: String; const aSize: Integer; const aStyle: TtsFontStyles; const aAntiAliasing: TtsAntiAliasing): TtsFont; overload;
    function GetFontByStream(const aStream: TStream; const aSize: Integer; const aStyle: TtsFontStyles; const aAntiAliasing: TtsAntiAliasing): TtsFont; overload;

    constructor Create(const aContext: TtsContext);
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  utsUtils;

const
  FT_SIZE_FACTOR  = 64;
  FT_SIZE_RES     = 72; //dpi

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsFreeTypeFaceHandle/////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsFreeTypeFaceHandle.Create(const aFace: FT_Face);
begin
  inherited Create;
  fFace := aFace;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsFreeTypeFaceHandle.Destroy;
begin
  FT_Done_Face(fFace);
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsFontFreeType///////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsFontFreeType.Create(const aHandle: TtsFreeTypeFaceHandle; const aCreator: TtsFontCreator; const aMetric: TtsFontMetric);
begin
  inherited Create(aCreator, aMetric);
  fHandle := aHandle;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsFontFreeType.GetCharImage(const aCharCode: WideChar; const aCharImage: TtsImage; const aFormat: TtsFormat);
var
  err: FT_Error;
  g: FT_GlyphSlot;
  b: PFT_Bitmap;

  procedure CopyGray;
  var
    x, y: Integer;
    src, dst: PByte;
    c: TtsColor4f;
  begin
    aCharImage.CreateEmpty(aFormat, b^.width, b^.rows);
    c := tsColor4f(1, 1, 1, 1);
    for y := 0 to b^.rows-1 do begin
      src := b^.buffer;
      inc(src, y * b^.pitch);
      dst := aCharImage.Scanline[y];
      for x := 0 to b^.width-1 do begin
        c.a := src^ / $FF;
        inc(src, 1);
        tsFormatMap(aCharImage.Format, dst, c);
      end;
    end;
  end;

  procedure CopyMono;
  var
    x, y, i, cnt: Integer;
    src, dst: PByte;
    tmp: Byte;
    c: TtsColor4f;
  begin
    aCharImage.CreateEmpty(aFormat, b^.width, b^.rows);
    c := tsColor4f(1, 1, 1, 1);
    for y := 0 to b^.rows-1 do begin
      src := b^.buffer;
      inc(src, y * b^.pitch);
      dst := aCharImage.Scanline[y];
      x := b^.width;
      while (x > 0) do begin
        cnt := min(8, x);
        tmp := src^;
        inc(src, 1);
        for i := 1 to cnt do begin
          if ((tmp and $80) > 0) then
            c.a := 1.0
          else
            c.a := 0.0;
          tmp := (tmp and not $80) shl 1;
          tsFormatMap(aCharImage.Format, dst, c);
        end;
        dec(x, cnt);
      end;
    end;
  end;

begin
  g := fHandle.fFace^.glyph;

  if not (Metric.AntiAliasing in [tsAANormal, tsAANone]) then
    raise Exception.Create('unknown anti aliasing');
  case Metric.AntiAliasing of
    tsAANormal:
      err := FT_Load_Char(fHandle.fFace, Ord(aCharCode), FT_LOAD_DEFAULT or FT_LOAD_RENDER);
    tsAANone:
      err := FT_Load_Char(fHandle.fFace, Ord(aCharCode), FT_LOAD_MONOCHROME or FT_LOAD_TARGET_MONO or FT_LOAD_RENDER);
  else
    exit;
  end;
  if (err <> 0) then
    raise EtsException.Create('unable to set glyph metrix: error=' + IntToStr(err));
  if (g^.format <> FT_GLYPH_FORMAT_BITMAP) then
    raise EtsException.Create('invalid glyph format');

  b := @g^.bitmap;
  case b^.pixel_mode of
    FT_PIXEL_MODE_MONO:
      CopyMono;
    FT_PIXEL_MODE_GRAY:
      CopyGray;
  else
    raise EtsException.Create('unknown glyph bitmap format');
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFontFreeType.GetGlyphMetrics(const aCharCode: WideChar; out aGlyphOrigin, aGlyphSize: TtsPosition; out aAdvance: Integer): Boolean;
var
  err: FT_Error;
begin
  result := false;

  aGlyphOrigin.x := 0;
  aGlyphOrigin.x := 0;
  aGlyphSize.x   := 0;
  aGlyphSize.y   := 0;
  aAdvance       := 0;

  case Metric.AntiAliasing of
    tsAANormal:
      err := FT_Load_Char(fHandle.fFace, Ord(aCharCode), FT_LOAD_DEFAULT);
    tsAANone:
      err := FT_Load_Char(fHandle.fFace, Ord(aCharCode), FT_LOAD_MONOCHROME);
  else
    raise EtsException.Create('unknown anti aliasing');
  end;
  case err of
    FT_ERR_None:
      { nop };
    FT_ERR_Invalid_Character_Code:
      exit;
  else
    raise EtsException.Create('unable to set glyph metrix: error=' + IntToStr(err));
  end;

  result := true;
  with fHandle.fFace^.glyph^.metrics do begin
    aAdvance        := horiAdvance  div FT_SIZE_FACTOR;
    aGlyphOrigin.x  := horiBearingX div FT_SIZE_FACTOR;
    aGlyphOrigin.y  := horiBearingY div FT_SIZE_FACTOR;
    aGlyphSize.x    := width        div FT_SIZE_FACTOR;
    aGlyphSize.y    := height       div FT_SIZE_FACTOR;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsFontFreeType.Destroy;
begin
  FreeAndNil(fHandle);
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsFontCreatorFreeType//////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsFontCreatorFreeType.LoadNames(const aFace: FT_Face; var aMetric: TtsFontMetric);
var
  i, cnt: FT_Int;
  err: FT_Error;
  name: FT_SfntName;

  function DecodeAnsi(const aCodePage: TtsCodePage): String;
  var
    tmp: WideString;
    len: Integer;
  begin
    SetLength(tmp, name.string_len);
    len := tsAnsiSBCDToWide(@tmp[1], name.string_len, PAnsiChar(name.string_), aCodePage, '?');
    SetLength(tmp, len);
    result := UTF8Encode(tmp);
  end;

  function Decode: String;
  var
    tmp: WideString;
    len: Integer;
  begin
    result := '';
    case name.platform_id of
      TT_PLATFORM_APPLE_UNICODE: begin
        case name.encoding_id of
          TT_APPLE_ID_DEFAULT,
          TT_APPLE_ID_UNICODE_1_1,
          TT_APPLE_ID_UNICODE_2_0: begin
            SetLength(tmp, name.string_len);
            len := tsUTFBE16ToWide(@tmp[1], name.string_len, name.string_, name.string_len, '?');
            SetLength(tmp, len);
            result := UTF8Encode(tmp);
          end;
        end;
      end;

      TT_PLATFORM_ISO: begin
        case name.encoding_id of
          TT_ISO_ID_8859_1:
            result := DecodeAnsi(tsISO_8859_1);
        end;
      end;
    end;
  end;

begin
  cnt := FT_Get_Sfnt_Name_Count(aFace);
  for i := 0 to cnt-1 do begin
    err := FT_Get_Sfnt_Name(aFace, i, @name);
    if (err <> 0) then
      continue;

    case name.name_id of
      TT_NAME_ID_COPYRIGHT:
        if (aMetric.Copyright = '') then
          aMetric.Copyright := Decode;

      TT_NAME_ID_FONT_FAMILY:
        if (aMetric.Fontname = '') then
          aMetric.Fontname := Decode;

      TT_NAME_ID_FULL_NAME:
        if (aMetric.FullName = '') then
          aMetric.FullName := Decode;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFontCreatorFreeType.CreateFont(const aFace: FT_Face; const aSize: Integer; const aStyle: TtsFontStyles;
  const aAntiAliasing: TtsAntiAliasing): TtsFont;
var
  err: FT_Error;
  metric: TtsFontMetric;
  os2: PTT_OS2;
  hz: PTT_HoriHeader;
begin
  err := FT_Set_Char_Size(aFace, 0, aSize * FT_SIZE_FACTOR, FT_SIZE_RES, FT_SIZE_RES);
  if (err <> 0) then
    raise EtsException.Create('unable to set char size: error=' + IntToStr(err));

  FillChar(metric{%H-}, SizeOf(metric), #0);
  metric.AntiAliasing := tsAANormal;
  metric.FaceName     := String(aFace^.family_name);
  metric.StyleName    := String(aFace^.style_name);
  LoadNames(aFace, metric);

  metric.Size         := aSize;
  metric.AntiAliasing := aAntiAliasing;
  metric.DefaultChar  := '?';
  metric.Style        := aStyle + [tsStyleBold, tsStyleItalic];
  if ((aFace^.style_flags and FT_STYLE_FLAG_BOLD) = 0) then
    Exclude(metric.Style, tsStyleBold);
  if ((aFace^.style_flags and FT_STYLE_FLAG_ITALIC) = 0) then
    Exclude(metric.Style, tsStyleItalic);

  metric.Ascent           :=  aFace^.size^.metrics.ascender  div FT_SIZE_FACTOR;
  metric.Descent          := -aFace^.size^.metrics.descender div FT_SIZE_FACTOR;
  metric.ExternalLeading  := 0;
  metric.BaseLineOffset   := 0;

  metric.UnderlinePos  := aFace^.underline_position  div FT_SIZE_FACTOR;
  metric.UnderlineSize := aFace^.underline_thickness div FT_SIZE_FACTOR;

  os2 := PTT_OS2(FT_Get_Sfnt_Table(aFace, FT_SFNT_OS2));
  if Assigned(os2) and (os2^.version <> $FFFF) then begin
    metric.StrikeoutPos  := os2^.yStrikeoutPosition div FT_SIZE_FACTOR;
    metric.StrikeoutSize := os2^.yStrikeoutSize     div FT_SIZE_FACTOR;
  end;

  hz := PTT_HoriHeader(FT_Get_Sfnt_Table(aFace, FT_SFNT_HHEA));
  if Assigned(hz) then begin
    metric.ExternalLeading := hz^.Line_Gap div FT_SIZE_FACTOR;
  end;

  result := TtsFontFreeType.Create(TtsFreeTypeFaceHandle.Create(aFace), self, metric);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFontCreatorFreeType.GetFontByFile(const aFilename: String; const aSize: Integer; const aStyle: TtsFontStyles; const aAntiAliasing: TtsAntiAliasing): TtsFont;
var
  face: FT_Face;
  err: FT_Error;
begin
  err := FT_New_Face(fHandle, PAnsiChar(aFilename), 0, @face);
  if (err <> 0) then
    raise EtsException.Create('unable to create free type face from file: ' + aFilename + ' error=' + IntToStr(err));
  result := CreateFont(face, aSize, aStyle, aAntiAliasing);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFontCreatorFreeType.GetFontByStream(const aStream: TStream; const aSize: Integer; const aStyle: TtsFontStyles; const aAntiAliasing: TtsAntiAliasing): TtsFont;
var
  face: FT_Face;
  err: FT_Error;
  ms: TMemoryStream;
  p: PBYte;
begin
  if (aStream is TMemoryStream) then begin
    ms := (aStream as TMemoryStream);
    p := ms.Memory;
    inc(p, ms.Position);
    err := FT_New_Memory_Face(fHandle, p, ms.Size - ms.Position, 0, @face);
  end else begin
    ms := TMemoryStream.Create;
    try
      ms.CopyFrom(aStream, aStream.Size - aStream.Position);
      err := FT_New_Memory_Face(fHandle, PByte(ms.Memory), ms.Size, 0, @face);
    finally
      FreeAndNil(ms);
    end;
  end;
  if (err <> 0) then
    raise EtsException.Create('unable to create free type face from stream: error=' + IntToStr(err));
  result := CreateFont(face, aSize, aStyle, aAntiAliasing);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsFontCreatorFreeType.Create(const aContext: TtsContext);
begin
  inherited Create(aContext);
  fHandle := InitFreeType;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsFontCreatorFreeType.Destroy;
begin
  inherited Destroy;  // first call interited
  QuitFreeType;       // QuitFreeType will free callpacks
end;

end.

