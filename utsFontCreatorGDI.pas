unit utsFontCreatorGDI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs, dynlibs,
  utsTextSuite, utsTypes;

type
  HDC = Cardinal;

  TFixed = packed record
    fract: Word;
    value: Smallint;
  end;

  TMat2 = packed record
    eM11: TFixed;
    eM12: TFixed;
    eM21: TFixed;
    eM22: TFixed;
  end;
  PMat2 = ^TMat2;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsFontGDI = class(TtsFont)
  private
    fHandle: THandle;
    fMat2: TMat2;
  protected
    constructor Create(const aRenderer: TtsRenderer; const aCreator: TtsFontGenerator; const aProperties: TtsFontProperties; const aHandle: THandle);
  public
    destructor Destroy; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsFontRegistration = class(TObject)
  protected
    fIsRegistered: Boolean;
    fFontname: String;
    procedure UnregisterFont; virtual; abstract;
  public
    property IsRegistered: Boolean read fIsRegistered;
    property Fontname:     String  read fFontname;

    destructor Destroy; override;
  end;

  TtsFontRegistrationFile = class(TtsFontRegistration)
  private
    fFilename: String;
  protected
    procedure UnregisterFont; override;
  public
    constructor Create(const aFilename: String);
  end;

  TtsFontRegistrationStream = class(TtsFontRegistration)
  private
    fHandle: THandle;
  protected
    procedure UnregisterFont; override;
  public
    constructor Create(const aStream: TStream);
  end;

  TtsRegistredFontGDI = class(TtsFontGDI)
  private
    fRegistration: TtsFontRegistration;
  public
    constructor Create(const aRenderer: TtsRenderer; const aCreator: TtsFontGenerator;
      const aRegistration: TtsFontRegistration; const aProperties: TtsFontProperties; const aHandle: THandle);
    destructor Destroy; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsFontGeneratorGDI = class(TtsFontGenerator)
  private
    function ConvertFont(const aFont: TtsFont): TtsFontGDI;
    function GetGlyphIndex(const aFont: TtsFontGDI; const aCharCode: WideChar): Integer;
    procedure GetCharImageAANone(const aDC: HDC; const aFont: TtsFontGDI; const aCharCode: WideChar; const aImage: TtsImage);
    procedure GetCharImageAANormal(const aDC: HDC; const aFont: TtsFontGDI; const aCharCode: WideChar; const aImage: TtsImage);

    function CreateFont(const aFontname: String; const aSize: Integer; const aStyle: TtsFontStyles; const aAntiAliasing: TtsAntiAliasing; out aProperties: TtsFontProperties): THandle;
  protected
    function GetGlyphMetrics(const aFont: TtsFont; const aCharCode: WideChar; out aGlyphOrigin, aGlyphSize: TtsPosition; out aAdvance: Integer): Boolean; override;
    procedure GetCharImage(const aFont: TtsFont; const aCharCode: WideChar; const aCharImage: TtsImage); override;
  public
    function GetFontByName(const aFontname: String; const aRenderer: TtsRenderer; const aSize: Integer; const aStyle: TtsFontStyles; const aAntiAliasing: TtsAntiAliasing): TtsFont; overload;
    function GetFontByFile(const aFilename: String; const aRenderer: TtsRenderer; const aSize: Integer; const aStyle: TtsFontStyles; const aAntiAliasing: TtsAntiAliasing): TtsFont; overload;
    function GetFontByStream(const aStream: TStream;  const aRenderer: TtsRenderer; const aSize: Integer; const aStyle: TtsFontStyles; const aAntiAliasing: TtsAntiAliasing): TtsFont; overload;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  math, utsTtfUtils;

const
  LIB_GDI32    = 'gdi32.dll';
  LIB_KERNEL32 = 'kernel32.dll';

  GDI_ERROR = DWORD($FFFFFFFF);

  FW_NORMAL = 400;
  FW_BOLD = 700;

  DEFAULT_CHARSET = 1;

  NONANTIALIASED_QUALITY = 3;
  ANTIALIASED_QUALITY = 4;

  GGO_METRICS = 0;
  GGO_BITMAP = 1;
  GGO_GRAY8_BITMAP = 6;
  GGO_GLYPH_INDEX = $80;

  FR_PRIVATE = $10;
  FR_NOT_ENUM = $20;

  LOCALE_USER_DEFAULT = $0400;
  LOCALE_ILANGUAGE = $1;

  GCP_MAXEXTENT = $100000;

  TMPF_FIXED_PITCH = 1;

type
  HFONT = Cardinal;
  HGDIOBJ = Cardinal;

  TLogFontA = record
    lfHeight: Longint;
    lfWidth: Longint;
    lfEscapement: Longint;
    lfOrientation: Longint;
    lfWeight: Longint;
    lfItalic: Byte;
    lfUnderline: Byte;
    lfStrikeOut: Byte;
    lfCharSet: Byte;
    lfOutPrecision: Byte;
    lfClipPrecision: Byte;
    lfQuality: Byte;
    lfPitchAndFamily: Byte;
    lfFaceName: array[0..31] of AnsiChar;
  end;
  PLogFontA = ^TLogFontA;

  TTextMetricW = record
    tmHeight: Longint;
    tmAscent: Longint;
    tmDescent: Longint;
    tmInternalLeading: Longint;
    tmExternalLeading: Longint;
    tmAveCharWidth: Longint;
    tmMaxCharWidth: Longint;
    tmWeight: Longint;
    tmOverhang: Longint;
    tmDigitizedAspectX: Longint;
    tmDigitizedAspectY: Longint;
    tmFirstChar: WideChar;
    tmLastChar: WideChar;
    tmDefaultChar: WideChar;
    tmBreakChar: WideChar;
    tmItalic: Byte;
    tmUnderlined: Byte;
    tmStruckOut: Byte;
    tmPitchAndFamily: Byte;
    tmCharSet: Byte;
  end;
  PTextMetricW = ^TTextMetricW;

  TGlyphMetrics = record
    gmBlackBoxX: Cardinal;
    gmBlackBoxY: Cardinal;
    gmptGlyphOrigin: TtsPosition;
    gmCellIncX: Smallint;
    gmCellIncY: Smallint;
  end;
  PGlyphMetrics = ^TGlyphMetrics;

  TGCPResultsW = record
    lStructSize: DWORD;
    lpOutString: PWideChar;
    lpOrder: PDWORD;
    lpDx: PInteger;
    lpCaretPos: PInteger;
    lpClass: PChar;
    lpGlyphs: PCardinal;
    nGlyphs: Cardinal;
    nMaxFit: Cardinal;
  end;
  PGCPResultsW = ^TGCPResultsW;

  TPanose = record
    bFamilyType: Byte;
    bSerifStyle: Byte;
    bWeight: Byte;
    bProportion: Byte;
    bContrast: Byte;
    bStrokeVariation: Byte;
    bArmStyle: Byte;
    bLetterform: Byte;
    bMidline: Byte;
    bXHeight: Byte;
  end;
  PPanose = ^TPanose;

  TOutlineTextmetricW = record
    otmSize: LongWord;
    otmTextMetrics: TTextMetricW;
    otmFiller: Byte;
    otmPanoseNumber: TPanose;
    otmfsSelection: LongWord;
    otmfsType: LongWord;
    otmsCharSlopeRise: Integer;
    otmsCharSlopeRun: Integer;
    otmItalicAngle: Integer;
    otmEMSquare: LongWord;
    otmAscent: Integer;
    otmDescent: Integer;
    otmLineGap: LongWord;
    otmsCapEmHeight: LongWord;
    otmsXHeight: LongWord;
    otmrcFontBox: TtsRect;
    otmMacAscent: Integer;
    otmMacDescent: Integer;
    otmMacLineGap: LongWord;
    otmusMinimumPPEM: LongWord;
    otmptSubscriptSize: TtsPosition;
    otmptSubscriptOffset: TtsPosition;
    otmptSuperscriptSize: TtsPosition;
    otmptSuperscriptOffset: TtsPosition;
    otmsStrikeoutSize: LongWord;
    otmsStrikeoutPosition: Integer;
    otmsUnderscoreSize: Integer;
    otmsUnderscorePosition: Integer;
    otmpFamilyName: PWideChar;
    otmpFaceName: PWideChar;
    otmpStyleName: PWideChar;
    otmpFullName: PWideChar;
  end;
  POutlineTextmetricW = ^TOutlineTextmetricW;

  TCreateFontIndirectA = function (const p1: TLogFontA): HFONT; stdcall;

  TAddFontResourceA = function(Filename: PAnsiChar): Integer; stdcall;
  TAddFontResourceExA = function(Filename: PAnsiChar; Flag: DWORD; pdv: Pointer): Integer; stdcall;
  TAddFontMemResourceEx = function(pbFont: Pointer; cbFont: DWORD; pdv: Pointer; pcFonts: PDWORD): THandle; stdcall;
  TRemoveFontResourceA = function(Filename: PAnsiChar): Boolean; stdcall;
  TRemoveFontResourceExA = function(filename: PAnsiChar; Flag: DWORD; pdv: Pointer): Boolean; stdcall;
  TRemoveFontMemResourceEx = function(fh: THandle): Boolean; stdcall;

  TGetTextMetricsW = function(DC: HDC; var TM: TTextMetricW): Boolean; stdcall;
  TGetGlyphOutlineA = function(DC: HDC; uChar, uFormat: Cardinal; lpgm: PGlyphMetrics; cbBuffer: DWORD; lpvBuffer: Pointer; lpmat2: PMat2): DWORD; stdcall;

  TGetCharacterPlacementW = function(DC: HDC; Str: PWideChar; Count, MaxExtent: Integer; Result: PGCPResultsW; Flags: DWORD): DWORD; stdcall;
  TGetFontData = function(DC: HDC; TableName, Offset: DWORD; Buffer: Pointer; Data: DWORD): DWORD; stdcall;

  TCreateCompatibleDC = function(DC: HDC): HDC; stdcall;
  TDeleteDC = function(DC: HDC): Boolean; stdcall;
  TSelectObject = function(DC: HDC; p2: HGDIOBJ): HGDIOBJ; stdcall;
  TDeleteObject = function(p1: HGDIOBJ): Boolean; stdcall;

  TGetOutlineTextMetricsW = function(DC: HDC; p2: LongWord; var OTMetricStructs: TOutlineTextmetricW): LongWord; stdcall;

  TGetLocaleInfoA = function(Locale: DWORD; LCType: DWORD; lpLCData: pAnsiChar; cchData: Integer): Integer; stdcall;

var
  gdiRefCount: Integer;
  gdiCritSec: TCriticalSection;
  gdiInitialized: Boolean;
  gdiLibHandle: TLibHandle = 0;
  kernel32LibHandle: TLibHandle = 0;

  CreateFontIndirectA: TCreateFontIndirectA;
  AddFontResourceA: TAddFontResourceA;
  AddFontResourceExA: TAddFontResourceExA;
  AddFontMemResourceEx: TAddFontMemResourceEx;
  RemoveFontResourceA: TRemoveFontResourceA;
  RemoveFontResourceExA: TRemoveFontResourceExA;
  RemoveFontMemResourceEx: TRemoveFontMemResourceEx;
  GetTextMetricsW: TGetTextMetricsW;
  GetGlyphOutlineA: TGetGlyphOutlineA;
  GetCharacterPlacementW: TGetCharacterPlacementW;
  GetFontData: TGetFontData;
  CreateCompatibleDC: TCreateCompatibleDC;
  DeleteDC: TDeleteDC;
  SelectObject: TSelectObject;
  DeleteObject: TDeleteObject;
  GetOutlineTextMetricsW: TGetOutlineTextMetricsW;

  GetLocaleInfoA: TGetLocaleInfoA;

procedure InitGDI;

  function GetProcAddr(const aLibHandle: TLibHandle; const aName: String): Pointer;
  begin
    result := GetProcAddress(aLibHandle, aName);
    if not Assigned(result) then
      raise EtsException.Create('unable to load procedure from library: ' + aName);
  end;

begin
  try
    if (gdiLibHandle = 0) then begin
      gdiLibHandle := LoadLibrary(LIB_GDI32);
      if (gdiLibHandle = 0) then
        raise EtsException.Create('unable to load gdi lib: ' + LIB_GDI32);
    end;

    if (kernel32LibHandle = 0) then begin
      kernel32LibHandle := LoadLibrary(LIB_KERNEL32);
      if (kernel32LibHandle = 0) then
        raise EtsException.Create('unable to load kernel lib: ' + LIB_KERNEL32);
    end;

    CreateFontIndirectA     := TCreateFontIndirectA(    GetProcAddr(gdiLibHandle, 'CreateFontIndirectA'));
    AddFontResourceA        := TAddFontResourceA(       GetProcAddr(gdiLibHandle, 'AddFontResourceA'));
    AddFontResourceExA      := TAddFontResourceExA(     GetProcAddr(gdiLibHandle, 'AddFontResourceExA'));
    AddFontMemResourceEx    := TAddFontMemResourceEx(   GetProcAddr(gdiLibHandle, 'AddFontMemResourceEx'));
    RemoveFontResourceA     := TRemoveFontResourceA(    GetProcAddr(gdiLibHandle, 'RemoveFontResourceA'));
    RemoveFontResourceExA   := TRemoveFontResourceExA(  GetProcAddr(gdiLibHandle, 'RemoveFontResourceExA'));
    RemoveFontMemResourceEx := TRemoveFontMemResourceEx(GetProcAddr(gdiLibHandle, 'RemoveFontMemResourceEx'));
    GetTextMetricsW         := TGetTextMetricsW(        GetProcAddr(gdiLibHandle, 'GetTextMetricsW'));
    GetGlyphOutlineA        := TGetGlyphOutlineA(       GetProcAddr(gdiLibHandle, 'GetGlyphOutlineA'));
    GetCharacterPlacementW  := TGetCharacterPlacementW( GetProcAddr(gdiLibHandle, 'GetCharacterPlacementW'));
    GetFontData             := TGetFontData(            GetProcAddr(gdiLibHandle, 'GetFontData'));
    CreateCompatibleDC      := TCreateCompatibleDC(     GetProcAddr(gdiLibHandle, 'CreateCompatibleDC'));
    DeleteDC                := TDeleteDC(               GetProcAddr(gdiLibHandle, 'DeleteDC'));
    SelectObject            := TSelectObject(           GetProcAddr(gdiLibHandle, 'SelectObject'));
    DeleteObject            := TDeleteObject(           GetProcAddr(gdiLibHandle, 'DeleteObject'));
    GetOutlineTextMetricsW  := TGetOutlineTextMetricsW( GetProcAddr(gdiLibHandle, 'GetOutlineTextMetricsW'));

    GetLocaleInfoA := TGetLocaleInfoA(GetProcAddr(kernel32LibHandle, 'GetLocaleInfoA'));

    gdiInitialized := true;
  except
    gdiInitialized := false;
    FreeLibrary(gdiLibHandle);
    FreeLibrary(kernel32LibHandle);
  end;
end;

procedure QuitGDI;
begin
  CreateFontIndirectA := nil;
  AddFontResourceA := nil;
  AddFontResourceExA := nil;
  RemoveFontResourceA := nil;
  RemoveFontResourceExA := nil;
  GetTextMetricsW := nil;
  GetGlyphOutlineA := nil;
  GetCharacterPlacementW := nil;
  GetFontData := nil;
  CreateCompatibleDC := nil;
  DeleteDC := nil;
  SelectObject := nil;
  DeleteObject := nil;

  GetLocaleInfoA := nil;

  if (gdiLibHandle <> 0) then begin
    FreeLibrary(gdiLibHandle);
    gdiLibHandle := 0;
  end;

  if (kernel32LibHandle <> 0) then begin
    FreeLibrary(kernel32LibHandle);
    kernel32LibHandle := 0;
  end;

  gdiInitialized := false;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsFontGDI////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsFontGDI.Create(const aRenderer: TtsRenderer; const aCreator: TtsFontGenerator; const aProperties: TtsFontProperties; const aHandle: THandle);
begin
  inherited Create(aRenderer, aCreator, aProperties);
  FillByte(fMat2, SizeOf(fMat2), 0);
  fMat2.eM11.value := 1;
  fMat2.eM22.value := 1;
  fHandle          := aHandle;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsFontGDI.Destroy;
begin
  DeleteObject(fHandle);
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsFontRegistration///////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsFontRegistration.Destroy;
begin
  if fIsRegistered then
    UnregisterFont;
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsFontRegistrationFile///////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsFontRegistrationFile.UnregisterFont;
begin
  if Assigned(RemoveFontResourceExA) then
    RemoveFontResourceExA(PAnsiChar(fFilename), 0, nil)
  else if Assigned(RemoveFontResourceA) then
    RemoveFontResourceA(PAnsiChar(fFilename));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsFontRegistrationFile.Create(const aFilename: String);
var
  lang: AnsiString;
begin
  inherited Create;
  fFilename := aFilename;

  // get Fontname
  SetLength(lang, 4);
  GetLocaleInfoA(LOCALE_USER_DEFAULT, LOCALE_ILANGUAGE, @lang[1], 4);
  fFontname := GetTTFontFullNameFromFile(aFilename, StrToInt('$' + String(lang)));

  // register font
  if Assigned(AddFontResourceExA) then
    fIsRegistered := (AddFontResourceExA(PAnsiChar(fFilename), 0, nil) > 0)
  else if Assigned(AddFontResourceA) then
    fIsRegistered := (AddFontResourceA(PAnsiChar(fFilename)) > 0)
  else
    fIsRegistered := false;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsFontRegistrationStream/////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsFontRegistrationStream.UnregisterFont;
begin
  if Assigned(RemoveFontMemResourceEx) then
    RemoveFontMemResourceEx(fHandle);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsFontRegistrationStream.Create(const aStream: TStream);
var
  lang: AnsiString;
  ms: TMemoryStream;
  cnt: DWORD;
begin
  inherited Create;
  fHandle       := 0;
  fIsRegistered := false;

  // get Fontname
  SetLength(Lang, 4);
  GetLocaleInfoA(LOCALE_USER_DEFAULT, LOCALE_ILANGUAGE, @lang[1], 4);
  fFontname := GetTTFontFullNameFromStream(aStream, StrToInt('$' + String(Lang)));

  // register font
  ms := TMemoryStream.Create;
  try
    ms.CopyFrom(aStream, 0);
    if Assigned(AddFontMemResourceEx) then
      fHandle := AddFontMemResourceEx(ms.Memory, ms.Size, nil, @cnt);
    fIsRegistered := (fHandle > 0);
  finally
    FreeAndNil(ms);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsRegistredFontGDI///////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsRegistredFontGDI.Create(const aRenderer: TtsRenderer; const aCreator: TtsFontGenerator;
  const aRegistration: TtsFontRegistration; const aProperties: TtsFontProperties; const aHandle: THandle);
begin
  inherited Create(aRenderer, aCreator, aProperties, aHandle);
  fRegistration := aRegistration;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsRegistredFontGDI.Destroy;
begin
  FreeAndNil(fRegistration);
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsFontCreatorGDIFontFace/////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFontGeneratorGDI.ConvertFont(const aFont: TtsFont): TtsFontGDI;
begin
  if not (aFont is TtsFontGDI) then
    raise EtsException.Create('aFont need to be a TtsFontGDI object');
  result := (aFont as TtsFontGDI);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFontGeneratorGDI.GetGlyphIndex(const aFont: TtsFontGDI; const aCharCode: WideChar): Integer;
var
  DC: HDC;
  GCPRes: TGCPResultsW;
begin
  result := -1;
  DC := CreateCompatibleDC(0);
  try
    SelectObject(DC, aFont.fHandle);
    if Assigned(GetCharacterPlacementW) then begin
      FillByte(GCPRes, SizeOf(GCPRes), 0);
      GetMem(GCPRes.lpGlyphs, SizeOf(Cardinal));
      try
        GCPRes.lStructSize := SizeOf(GCPRes);
        GCPRes.lpGlyphs^   := 0;
        GCPRes.nGlyphs     := 1;
        if (GetCharacterPlacementW(DC, @aCharCode, 1, GCP_MAXEXTENT, @GCPRes, 0) <> GDI_ERROR) and
           (GCPRes.nGlyphs = 1) and
           (GCPRes.lpGlyphs <> nil) then
        begin
          result := GCPRes.lpGlyphs^;
        end;
      finally
        FreeMem(GCPRes.lpGlyphs);
      end;
    end;
  finally
    DeleteDC(DC);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsFontGeneratorGDI.GetCharImageAANone(const aDC: HDC; const aFont: TtsFontGDI; const aCharCode: WideChar; const aImage: TtsImage);
var
  Metric: TGlyphMetrics;
  GlyphIndex, srcW, srcX, w, h, x, y: Integer;
  Size, OutlineRes: Cardinal;
  Buffer, pSrc, pDst: PByte;

  procedure ExpandByte;
  var
    i, cnt, srcCnt: Integer;
    c: TtsColor4f;
  begin
    srcCnt := min(8, srcX);
    cnt    := min(8, x);
    for i := 1 to cnt do begin
      c := tsColor4f(1, 1, 1, 1);
      if ((pSrc^ and $80) > 0) then
        c.a := 1
      else
        c.a := 0;
      pSrc^ := (pSrc^ and not $80) shl 1;
      tsFormatMap(aFont.Renderer.Format, pDst, c);
    end;
    dec(srcX, srcCnt);
    dec(x, cnt);
    inc(pSrc);
  end;

begin
  if (aFont.fMat2.eM11.value <> 1) then
    raise EtsException.Create('invalid value');
  FillByte(Metric, SizeOf(Metric), 0);

  GlyphIndex := GetGlyphIndex(aFont, aCharCode);
  if (GlyphIndex < 0) then
    exit;

  Size := GetGlyphOutlineA(aDC, GlyphIndex, GGO_BITMAP or GGO_GLYPH_INDEX, @Metric, 0, nil, @aFont.fMat2);
  if (Size = GDI_ERROR) or (Size = 0) then
    exit;

  GetMem(Buffer, Size);
  try
    OutlineRes := GetGlyphOutlineA(aDC, GlyphIndex, GGO_BITMAP or GGO_GLYPH_INDEX, @Metric, Size, Buffer, @aFont.fMat2);
    if (OutlineRes = GDI_ERROR) then
      exit;
    w    := Metric.gmBlackBoxX;
    h    := Metric.gmBlackBoxY;
    srcW := (Size div h) * 8;
    if (w <= 0) or (h <= 0) then
      exit;
    aImage.CreateEmpty(aFont.Renderer.Format, w, h);
    pSrc := Buffer;
    for y := 0 to h-1 do begin
      pDst := aImage.Scanline[y];
      srcX := srcW;
      while (srcX > 0) do
        ExpandByte;
    end;
  finally
    Freemem(Buffer);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsFontGeneratorGDI.GetCharImageAANormal(const aDC: HDC; const aFont: TtsFontGDI; const aCharCode: WideChar; const aImage: TtsImage);
var
  Metric: TGlyphMetrics;
  GlyphIndex, OutlineRes, tmp, Spacer, x, y, w, h: Integer;
  Size: Cardinal;
  Buffer, pSrc, pDst: PByte;

  procedure CopyPixel;
  var
    i: Integer;
    tmp, cnt: Cardinal;
    c: TtsColor4f;
  begin
    cnt := min(x, aFont.fMat2.eM11.value);
    tmp := 0;
    for i := 0 to cnt-1 do begin
      tmp := tmp + pSrc^;
      inc(pSrc, 1);
    end;
    dec(x, cnt);
    c := tsColor4f(1, 1, 1, tmp / ($40 * Cardinal(aFont.fMat2.eM11.value)));
    tsFormatMap(aFont.Renderer.Format, pDst, c);
  end;

begin
  FillByte(Metric, SizeOf(Metric), 0);

  GlyphIndex := GetGlyphIndex(aFont, aCharCode);
  if (GlyphIndex < 0) then
    exit;

  Size := GetGlyphOutlineA(aDC, GlyphIndex, GGO_GRAY8_BITMAP or GGO_GLYPH_INDEX, @Metric, 0, nil, @aFont.fMat2);
  if (Size = GDI_ERROR) or (Size = 0) then
    exit;

  GetMem(Buffer, Size);
  try
    OutlineRes := GetGlyphOutlineA(aDC, GlyphIndex, GGO_GRAY8_BITMAP or GGO_GLYPH_INDEX, @Metric, Size, Buffer, @aFont.fMat2);
    if (OutlineRes = GDI_ERROR) then
      exit;
    w   := Integer(Metric.gmBlackBoxX) div aFont.fMat2.eM11.value;
    h   := Metric.gmBlackBoxY;
    tmp := Integer(Metric.gmBlackBoxX) mod aFont.fMat2.eM11.value;
    if (tmp <> 0) then
      w := w + aFont.fMat2.eM11.value - tmp;
    if (w <= 0) or (h <= 0) then
      exit;

    // spacer
    Spacer := Metric.gmBlackBoxX mod 4;
    if (Spacer <> 0) then
      Spacer := 4 - Spacer;

    // copy image
    aImage.CreateEmpty(aFont.Renderer.Format, w, h);
    pSrc := Buffer;
    for y := 0 to h-1 do begin
      pDst := aImage.Scanline[y];
      x    := Metric.gmBlackBoxX;
      while (x > 0) do
        CopyPixel;
      inc(pSrc, Spacer);
    end;
  finally
    FreeMem(Buffer);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFontGeneratorGDI.CreateFont(const aFontname: String; const aSize: Integer; const aStyle: TtsFontStyles;
  const aAntiAliasing: TtsAntiAliasing; out aProperties: TtsFontProperties): THandle;
var
  LogFont: TLogFontA;
  i: Integer;
  DC: HDC;
  TableName, BufSize: Cardinal;
  Buffer: PByte;
  Lang: AnsiString;
  TextMetric: TTextMetricW;
  OutlineMetric: TOutlineTextmetricW;

  function _(e: Boolean; a, b: Integer): Integer;
  begin
    if e then
      result := a
    else
      result := b;
  end;

begin
  result := 0;

  FillByte(aProperties, SizeOf(aProperties), 0);
  aProperties.Size         := aSize;
  aProperties.Style        := aStyle;
  aProperties.AntiAliasing := aAntiAliasing;
  aProperties.Fontname     := aFontname;

  // prepare font attribs
  FillByte(LogFont, SizeOf(LogFont), 0);
  for i := 1 to min(Length(aFontname), Length(LogFont.lfFaceName)) do
    LogFont.lfFaceName[i-1] := aFontname[i];
  LogFont.lfCharSet   := DEFAULT_CHARSET;
  LogFont.lfHeight    := -aSize;
  LogFont.lfWeight    := _(tsStyleBold      in aStyle, FW_BOLD, FW_NORMAL);
  LogFont.lfItalic    := _(tsStyleItalic    in aStyle, 1, 0);
  LogFont.lfUnderline := _(tsStyleUnderline in aStyle, 1, 0);
  LogFont.lfQuality   := _(aAntiAliasing = tsAANormal, ANTIALIASED_QUALITY, NONANTIALIASED_QUALITY);

  result := CreateFontIndirectA(LogFont);
  DC     := CreateCompatibleDC(0);
  try try
    SelectObject(DC, result);
    TableName := MakeTTTableName('n', 'a', 'm', 'e');
    BufSize   := GetFontData(DC, TableName, 0, nil, 0);
    if (BufSize <> GDI_ERROR) then begin
      GetMem(Buffer, BufSize);
      try
        if (GetFontData(DC, TableName, 0, Buffer, BufSize) <> GDI_ERROR) then begin
          SetLength(Lang, 4);
          GetLocaleInfoA(LOCALE_USER_DEFAULT, LOCALE_ILANGUAGE, @Lang[1], 4);

          GetTTString(Buffer, BufSize, NAME_ID_COPYRIGHT,  StrToInt('$' + String(Lang)), aProperties.Copyright);
          GetTTString(Buffer, BufSize, NAME_ID_FACE_NAME,  StrToInt('$' + String(Lang)), aProperties.FaceName);
          GetTTString(Buffer, BufSize, NAME_ID_STYLE_NAME, StrToInt('$' + String(Lang)), aProperties.StyleName);
          GetTTString(Buffer, BufSize, NAME_ID_FULL_NAME,  StrToInt('$' + String(Lang)), aProperties.FullName);
        end;
      finally
        FreeMem(Buffer);
      end;
    end;

    if GetTextMetricsW(DC, TextMetric) then begin
      aProperties.Ascent          := TextMetric.tmAscent;
      aProperties.Descent         := TextMetric.tmDescent;
      aProperties.ExternalLeading := TextMetric.tmExternalLeading;
      aProperties.DefaultChar     := TextMetric.tmDefaultChar;
    end;

    if (GetOutlineTextMetricsW(DC, SizeOf(OutlineMetric), OutlineMetric) > 0) then begin
      aProperties.UnderlinePos  := OutlineMetric.otmsUnderscorePosition;
      aProperties.UnderlineSize := Min(1, OutlineMetric.otmsUnderscoreSize);
      aProperties.StrikeoutPos  := OutlineMetric.otmsStrikeoutPosition;
      aProperties.StrikeoutSize := Min(1, OutlineMetric.otmsStrikeoutSize);
    end;
  except
    DeleteObject(result);
    result := 0;
  end;
  finally
    DeleteDC(DC);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFontGeneratorGDI.GetGlyphMetrics(const aFont: TtsFont; const aCharCode: WideChar; out aGlyphOrigin, aGlyphSize: TtsPosition; out aAdvance: Integer): Boolean;
var
  GlyphIndex: Integer;
  font: TtsFontGDI;
  DC: HDC;
  Metric: TGlyphMetrics;
  Size: Cardinal;
begin
  result := false;

  aGlyphOrigin.x := 0;
  aGlyphOrigin.x := 0;
  aGlyphSize.x   := 0;
  aGlyphSize.y   := 0;
  aAdvance       := 0;

  font := ConvertFont(aFont);
  GlyphIndex := GetGlyphIndex(font, aCharCode);
  if (GlyphIndex < 0) then
    exit;

  DC := CreateCompatibleDC(0);
  try
    SelectObject(DC, font.fHandle);
    case font.Properties.AntiAliasing of
      tsAANone: begin
        Size := GetGlyphOutlineA(DC, GlyphIndex, GGO_BITMAP or GGO_GLYPH_INDEX, @Metric, 0, nil, @font.fMat2);
      end;
      tsAANormal: begin
        Size := GetGlyphOutlineA(DC, GlyphIndex, GGO_GRAY8_BITMAP or GGO_GLYPH_INDEX, @Metric, 0, nil, @font.fMat2);
      end;
    else
      Size := GDI_ERROR;
    end;

    if (Size = GDI_ERROR) then
      Size := GetGlyphOutlineA(DC, GlyphIndex, GGO_METRICS or GGO_GLYPH_INDEX, @Metric, 0, nil, @font.fMat2);

    if (Size <> GDI_ERROR) then begin
      aGlyphOrigin.x := Round(Metric.gmptGlyphOrigin.x / font.fMat2.eM11.value);
      aGlyphOrigin.y := Metric.gmptGlyphOrigin.y;
      aGlyphSize.x   := Round(Metric.gmBlackBoxX / font.fMat2.eM11.value);
      aGlyphSize.y   := Metric.gmBlackBoxY;
      aAdvance       := Round(Metric.gmCellIncX / font.fMat2.eM11.value);
      result         := true;
    end;
  finally
    DeleteDC(DC);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsFontGeneratorGDI.GetCharImage(const aFont: TtsFont; const aCharCode: WideChar; const aCharImage: TtsImage);
var
  DC: HDC;
  font: TtsFontGDI;
begin
  font := ConvertFont(aFont);
  DC := CreateCompatibleDC(0);
  try
    SelectObject(DC, font.fHandle);
    case font.Properties.AntiAliasing of
      tsAANone:
        GetCharImageAANone(DC, font, aCharCode, aCharImage);
      tsAANormal:
        GetCharImageAANormal(DC, font, aCharCode, aCharImage);
    end;
  finally
    DeleteDC(DC);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFontGeneratorGDI.GetFontByName(const aFontname: String; const aRenderer: TtsRenderer;
  const aSize: Integer; const aStyle: TtsFontStyles; const aAntiAliasing: TtsAntiAliasing): TtsFont;
var
  handle: THandle;
  prop: TtsFontProperties;
begin
  handle := CreateFont(aFontname, aSize, aStyle, aAntiAliasing, prop);
  if (handle = 0) then
    raise EtsException.Create('unable to create font from name: ' + aFontname);
  result := TtsFontGDI.Create(aRenderer, self, prop, handle);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFontGeneratorGDI.GetFontByFile(const aFilename: String; const aRenderer: TtsRenderer;
  const aSize: Integer; const aStyle: TtsFontStyles; const aAntiAliasing: TtsAntiAliasing): TtsFont;
var
  reg: TtsFontRegistrationFile;
  handle: THandle;
  prop: TtsFontProperties;
begin
  reg := TtsFontRegistrationFile.Create(aFilename);
  if not reg.IsRegistered then
    raise EtsException.Create('unable to register font file: ' + aFilename);
  handle := CreateFont(reg.Fontname, aSize, aStyle, aAntiAliasing, prop);
  if (handle = 0) then
    raise EtsException.Create('unable to create font from file: ' + aFilename);
  result := TtsRegistredFontGDI.Create(aRenderer, self, reg, prop, handle);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFontGeneratorGDI.GetFontByStream(const aStream: TStream; const aRenderer: TtsRenderer;
  const aSize: Integer; const aStyle: TtsFontStyles; const aAntiAliasing: TtsAntiAliasing): TtsFont;
var
  reg: TtsFontRegistrationStream;
  handle: THandle;
  prop: TtsFontProperties;
begin
  reg := TtsFontRegistrationStream.Create(aStream);
  if not reg.IsRegistered then
    raise EtsException.Create('unable to register font from stream');
  handle := CreateFont(reg.Fontname, aSize, aStyle, aAntiAliasing, prop);
  if (handle = 0) then
    raise EtsException.Create('unable to create font from stream: ' + reg.Fontname);
  result := TtsRegistredFontGDI.Create(aRenderer, self, reg, prop, handle);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsFontGeneratorGDI.Create;
begin
  inherited Create;
  gdiCritSec.Enter;
  try
    inc(gdiRefCount, 1);
    if not gdiInitialized then
      InitGDI;
  finally
    gdiCritSec.Leave;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsFontGeneratorGDI.Destroy;
begin
  gdiCritSec.Enter;
  try
    dec(gdiRefCount, 1);
    if (gdiRefCount <= 0) then
      QuitGDI;
  finally
    gdiCritSec.Leave;
  end;
  inherited Destroy;
end;

initialization
  gdiRefCount    := 0;
  gdiInitialized := false;
  gdiCritSec     := TCriticalSection.Create;

finalization
  if gdiInitialized then
    QuitGDI;
  FreeAndNil(gdiCritSec);

end.