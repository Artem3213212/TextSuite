unit utsFontCreatorFreeType;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  utsTextSuite, utsTypes, utsFreeType;

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
  public
    constructor Create(const aHandle: TtsFreeTypeFaceHandle; const aRenderer: TtsRenderer;
      const aGenerator: TtsFontGenerator; const aProperties: TtsFontProperties);
    destructor Destroy; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsFontGeneratorFreeType = class(TtsFontGenerator)
  private
    fHandle: FT_Library;

    function ConvertFont(const aFont: TtsFont): TtsFontFreeType;
    procedure LoadNames(const aFace: FT_Face; var aProperties: TtsFontProperties);
    function CreateFont(const aFace: FT_Face; const aRenderer: TtsRenderer; const aSize: Integer;
      const aStyle: TtsFontStyles; const aAntiAliasing: TtsAntiAliasing): TtsFont;
  protected
    function GetGlyphMetrics(const aFont: TtsFont; const aCharCode: WideChar;
      out aGlyphOrigin, aGlyphSize: TtsPosition; out aAdvance: Integer): Boolean; override;
    procedure GetCharImage(const aFont: TtsFont; const aCharCode: WideChar;
      const aCharImage: TtsImage); override;
  public
    function GetFontByFile(const aFilename: String; const aRenderer: TtsRenderer; const aSize: Integer;
      const aStyle: TtsFontStyles; const aAntiAliasing: TtsAntiAliasing): TtsFont; overload;
    function GetFontByStream(const aStream: TStream;  const aRenderer: TtsRenderer; const aSize: Integer;
      const aStyle: TtsFontStyles; const aAntiAliasing: TtsAntiAliasing): TtsFont; overload;

    constructor Create(const aContext: TtsContext);
    destructor Destroy; override;
  end;

implementation

uses
  utsUtils, math;

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
constructor TtsFontFreeType.Create(const aHandle: TtsFreeTypeFaceHandle; const aRenderer: TtsRenderer;
  const aGenerator: TtsFontGenerator; const aProperties: TtsFontProperties);
begin
  inherited Create(aRenderer, aGenerator, aProperties);
  fHandle := aHandle;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsFontFreeType.Destroy;
begin
  FreeAndNil(fHandle);
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsFontGeneratorFreeType//////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFontGeneratorFreeType.ConvertFont(const aFont: TtsFont): TtsFontFreeType;
begin
  if not (aFont is TtsFontFreeType) then
    raise EtsException.Create('aFont need to be a TtsFontGDI object');
  result := (aFont as TtsFontFreeType);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsFontGeneratorFreeType.LoadNames(const aFace: FT_Face; var aProperties: TtsFontProperties);
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
        if (aProperties.Copyright = '') then
          aProperties.Copyright := Decode;

      TT_NAME_ID_FONT_FAMILY:
        if (aProperties.Fontname = '') then
          aProperties.Fontname := Decode;

      TT_NAME_ID_FULL_NAME:
        if (aProperties.FullName = '') then
          aProperties.FullName := Decode;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFontGeneratorFreeType.CreateFont(const aFace: FT_Face; const aRenderer: TtsRenderer; const aSize: Integer;
  const aStyle: TtsFontStyles; const aAntiAliasing: TtsAntiAliasing): TtsFont;
var
  err: FT_Error;
  prop: TtsFontProperties;
  os2: PTT_OS2;
  hz: PTT_HoriHeader;
begin
  err := FT_Set_Char_Size(aFace, 0, aSize * FT_SIZE_FACTOR, FT_SIZE_RES, FT_SIZE_RES);
  if (err <> 0) then
    raise EtsException.Create('unable to set char size: error=' + IntToStr(err));

  FillByte(prop{%H-}, SizeOf(prop), 0);
  prop.AntiAliasing := tsAANormal;
  prop.FaceName     := aFace^.family_name;
  prop.StyleName    := aFace^.style_name;
  LoadNames(aFace, prop);

  prop.Size         := aSize;
  prop.AntiAliasing := aAntiAliasing;
  prop.DefaultChar  := '?';
  prop.Style        := aStyle + [tsStyleBold, tsStyleItalic];
  if ((aFace^.style_flags and FT_STYLE_FLAG_BOLD) = 0) then
    Exclude(prop.Style, tsStyleBold);
  if ((aFace^.style_flags and FT_STYLE_FLAG_ITALIC) = 0) then
    Exclude(prop.Style, tsStyleItalic);

  prop.Ascent           :=  aFace^.size^.metrics.ascender  div FT_SIZE_FACTOR;
  prop.Descent          := -aFace^.size^.metrics.descender div FT_SIZE_FACTOR;
  prop.ExternalLeading  := 0;
  prop.BaseLineOffset   := 0;

  prop.UnderlinePos  := aFace^.underline_position  div FT_SIZE_FACTOR;
  prop.UnderlineSize := aFace^.underline_thickness div FT_SIZE_FACTOR;

  os2 := PTT_OS2(FT_Get_Sfnt_Table(aFace, FT_SFNT_OS2));
  if Assigned(os2) and (os2^.version <> $FFFF) then begin
    prop.StrikeoutPos  := os2^.yStrikeoutPosition div FT_SIZE_FACTOR;
    prop.StrikeoutSize := os2^.yStrikeoutSize     div FT_SIZE_FACTOR;
  end;

  hz := PTT_HoriHeader(FT_Get_Sfnt_Table(aFace, FT_SFNT_HHEA));
  if Assigned(hz) then begin
    prop.ExternalLeading := hz^.Line_Gap div FT_SIZE_FACTOR;
  end;

  result := TtsFontFreeType.Create(TtsFreeTypeFaceHandle.Create(aFace), aRenderer, self, prop);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFontGeneratorFreeType.GetGlyphMetrics(const aFont: TtsFont; const aCharCode: WideChar; out aGlyphOrigin, aGlyphSize: TtsPosition; out aAdvance: Integer): Boolean;
var
  font: TtsFontFreeType;
  err: FT_Error;
begin
  result := false;

  aGlyphOrigin.x := 0;
  aGlyphOrigin.x := 0;
  aGlyphSize.x   := 0;
  aGlyphSize.y   := 0;
  aAdvance       := 0;

  font := ConvertFont(aFont);
  case font.Properties.AntiAliasing of
    tsAANormal:
      err := FT_Load_Char(font.fHandle.fFace, Ord(aCharCode), FT_LOAD_DEFAULT);
    tsAANone:
      err := FT_Load_Char(font.fHandle.fFace, Ord(aCharCode), FT_LOAD_MONOCHROME);
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
  with font.fHandle.fFace^.glyph^.metrics do begin
    aAdvance        := horiAdvance  div FT_SIZE_FACTOR;
    aGlyphOrigin.x  := horiBearingX div FT_SIZE_FACTOR;
    aGlyphOrigin.y  := horiBearingY div FT_SIZE_FACTOR;
    aGlyphSize.x    := width        div FT_SIZE_FACTOR;
    aGlyphSize.y    := height       div FT_SIZE_FACTOR;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsFontGeneratorFreeType.GetCharImage(const aFont: TtsFont; const aCharCode: WideChar; const aCharImage: TtsImage);
var
  font: TtsFontFreeType;
  err: FT_Error;
  g: FT_GlyphSlot;
  b: PFT_Bitmap;

  procedure CopyGray;
  var
    x, y: Integer;
    src, dst: PByte;
    c: TtsColor4f;
  begin
    aCharImage.CreateEmpty(font.Renderer.Format, b^.width, b^.rows);
    c := tsColor4f(1, 1, 1, 1);
    for y := 0 to b^.rows-1 do begin
      src := b^.buffer + y * b^.pitch;
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
    aCharImage.CreateEmpty(font.Renderer.Format, b^.width, b^.rows);
    c := tsColor4f(1, 1, 1, 1);
    for y := 0 to b^.rows-1 do begin
      src := b^.buffer + y * b^.pitch;
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
  font := ConvertFont(aFont);
  g := font.fHandle.fFace^.glyph;

  if not (font.Properties.AntiAliasing in [tsAANormal, tsAANone]) then
    raise Exception.Create('unknown anti aliasing');
  case font.Properties.AntiAliasing of
    tsAANormal:
      err := FT_Load_Char(font.fHandle.fFace, Ord(aCharCode), FT_LOAD_DEFAULT or FT_LOAD_RENDER);
    tsAANone:
      err := FT_Load_Char(font.fHandle.fFace, Ord(aCharCode), FT_LOAD_MONOCHROME or FT_LOAD_TARGET_MONO or FT_LOAD_RENDER);
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
function TtsFontGeneratorFreeType.GetFontByFile(const aFilename: String; const aRenderer: TtsRenderer;
  const aSize: Integer; const aStyle: TtsFontStyles; const aAntiAliasing: TtsAntiAliasing): TtsFont;
var
  face: FT_Face;
  err: FT_Error;
begin
  err := FT_New_Face(fHandle, PAnsiChar(aFilename), 0, @face);
  if (err <> 0) then
    raise EtsException.Create('unable to create free type face from file: ' + aFilename + ' error=' + IntToStr(err));
  result := CreateFont(face, aRenderer, aSize, aStyle, aAntiAliasing);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFontGeneratorFreeType.GetFontByStream(const aStream: TStream; const aRenderer: TtsRenderer;
  const aSize: Integer; const aStyle: TtsFontStyles; const aAntiAliasing: TtsAntiAliasing): TtsFont;
var
  face: FT_Face;
  err: FT_Error;
  ms: TMemoryStream;
begin
  if (aStream is TMemoryStream) then begin
    ms := (aStream as TMemoryStream);
    err := FT_New_Memory_Face(fHandle, PByte(ms.Memory) + ms.Position, ms.Size - ms.Position, 0, @face);
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
  result := CreateFont(face, aRenderer, aSize, aStyle, aAntiAliasing);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsFontGeneratorFreeType.Create(const aContext: TtsContext);
begin
  inherited Create(aContext);
  fHandle := InitFreeType;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsFontGeneratorFreeType.Destroy;
begin
  inherited Destroy;  // first call interited
  QuitFreeType;       // QuitFreeType will free callpacks
end;

end.

