unit utsGDI;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, utsTypes, syncobjs{$IFDEF FPC}, dynlibs{$ELSE}, Windows{$ENDIF};

type
  HDC = Cardinal;
{$IFNDEF FPC}
  DWORD = Cardinal;
  PDWORD = ^DWORD;
  TLibHandle = Cardinal;
{$ENDIF}  

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


const
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
procedure QuitGDI;

implementation

uses
  utsTextSuite;

const
  LIB_GDI32    = 'gdi32.dll';
  LIB_KERNEL32 = 'kernel32.dll';

var
  gdiRefCount: Integer;
  gdiCritSec: TCriticalSection;
  gdiInitialized: Boolean;
  gdiLibHandle: TLibHandle = 0;
  kernel32LibHandle: TLibHandle = 0;

procedure InitGDI;

  function GetProcAddr(const aLibHandle: TLibHandle; const aName: String): Pointer;
  begin
    result := GetProcAddress(aLibHandle, PAnsiChar(aName));
    if not Assigned(result) then
      raise EtsException.Create('unable to load procedure from library: ' + aName);
  end;

begin
  gdiCritSec.Enter;
  try try
    inc(gdiRefCount, 1);
    if gdiInitialized then
      exit;

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
  finally
    gdiCritSec.Leave;
  end;
end;

procedure QuitGDI;
begin
  gdiCritSec.Enter;
  try
    dec(gdiRefCount, 1);
    if (gdiRefCount > 0) then
      exit;

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
  finally
    gdiCritSec.Leave;
  end;
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
