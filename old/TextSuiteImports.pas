{
TextSuite (C) Steffen Xonna (aka Lossy eX)
http://www.opengl24.de/
-----------------------------------------------------------------------
For copyright informations see file copyright.txt.
}

{$I TextSuiteOptions.inc}

unit TextSuiteImports;

interface

uses
  TextSuite;

type
  DWORD = Cardinal;
  PDWORD = ^DWORD;

  
// *** Global Functions ***
{$IFDEF WINDOWS}
const
  Kernel32 = 'kernel32.dll';

  function LoadLibrary(lpFileName: pAnsiChar): Pointer; stdcall; external Kernel32 name 'LoadLibraryA';
  function FreeLibrary(hModule: Pointer): Pointer; stdcall; external Kernel32 name 'FreeLibrary';
  function GetProcAddress(hModule: Pointer; lpProcName: pAnsiChar): Pointer; stdcall; external Kernel32 name 'GetProcAddress';
{$ELSE}
const
  LibraryLib = {$IFDEF Linux} 'libdl.so.2'{$ELSE} 'c'{$ENDIF};

  RTLD_LAZY = $001;

  function dlopen(Name: pAnsiChar; Flags: LongInt): Pointer; cdecl; external LibraryLib name 'dlopen';
  function dlclose(Lib: Pointer): LongInt; cdecl; external LibraryLib name 'dlclose';
  function dlsym(Lib: Pointer; Name: pAnsiChar): Pointer; cdecl; external LibraryLib name 'dlsym';
{$ENDIF}


{$IFDEF WINDOWS}
  function GetCurrentThreadId: DWORD; stdcall; external Kernel32 name 'GetCurrentThreadId';
{$ENDIF}



// *** OpenGL ***
function Init_OpenGL: Boolean;
procedure Quit_OpenGL;

const
  {$IFDEF WINDOWS}
  LIB_OPENGL = 'opengl32.dll';
  {$ELSE}
  LIB_OPENGL = 'libGL.so.1';
  {$ENDIF}

  GL_TEXTURE_2D = $0DE1;
  GL_RGBA = $1908;
  GL_UNSIGNED_BYTE = $1401;
  GL_NEAREST = $2600;
  GL_LINEAR = $2601;
  GL_TEXTURE_MAG_FILTER = $2800;
  GL_TEXTURE_MIN_FILTER = $2801;

  GL_LINES = $0001;
  GL_QUADS = $0007;

  GL_COMPILE = $1300;


var
  OpenGL_initialized: Boolean;
  Library_OpenGL: Pointer;

  glEnable: procedure(cap: Cardinal); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  glDisable: procedure(cap: Cardinal); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}

  glColor4f: procedure(red, green, blue, alpha: Single); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}

  glGenTextures: procedure(n: Integer; textures: PCardinal); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  glDeleteTextures: procedure(n: Integer; const textures: PCardinal); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  glBindTexture: procedure(target: Cardinal; texture: Cardinal); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  glTexParameteri: procedure(target: Cardinal; pname: Cardinal; param: Integer); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  glTexImage2D: procedure(target: Cardinal; level: Integer; internalformat: Integer; width: Integer; height: Integer; border: Integer; format: Cardinal; _type: Cardinal; const pixels: Pointer); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  glTexSubImage2D: procedure(target: Cardinal; level: Integer; xoffset: Integer; yoffset: Integer; width: Integer; height: Integer; format: Cardinal; _type: Cardinal; const pixels: Pointer); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}

  glBegin: procedure(mode: Cardinal); {$IFNDEF CLR}{$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}{$ENDIF}
  glEnd: procedure(); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  glTexCoord2f: procedure(s: Single; t: Single); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  glTexCoord2fv: procedure(v: Pointer); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  glVertex2f: procedure(x: Single; y: Single); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  glVertex2fv: procedure(v: Pointer); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  glVertex2iv: procedure(v: Pointer); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}

  glGenLists: function(range: Integer): Cardinal; {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  glDeleteLists: procedure(list: Cardinal; range: Integer); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  glCallList: procedure(list: Cardinal); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  glNewList: procedure(list: Cardinal; mode: Cardinal); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  glEndList: procedure(); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}



// *** Windows GDI ***
function Init_GDI: Boolean;
procedure Quit_GDI;

type
  HDC = Cardinal;
  HFONT = Cardinal;
  HGDIOBJ = Cardinal;

  {$IFDEF CPU64}
  {$PACKRECORDS 8}
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
    gmptGlyphOrigin: tsPoint;
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
    otmrcFontBox: tsRect;
    otmMacAscent: Integer;
    otmMacDescent: Integer;
    otmMacLineGap: LongWord;
    otmusMinimumPPEM: LongWord;
    otmptSubscriptSize: tsPoint;
    otmptSubscriptOffset: tsPoint;
    otmptSuperscriptSize: tsPoint;
    otmptSuperscriptOffset: tsPoint;
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

  {$IFDEF CPU64}
  {$PACKRECORDS 4}
  {$ENDIF}

const
  LIB_GDI32 = 'gdi32.dll';
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


var
  GDI_initialized: Boolean;
  Library_GDI32: Pointer;
  Library_KERNEL32: Pointer;

  CreateFontIndirectA: function (const p1: TLogFontA): HFONT; stdcall;

  AddFontResourceA: function(Filename: PAnsiChar): Integer; stdcall;
  AddFontResourceExA: function(Filename: PAnsiChar; Flag: DWORD; pdv: Pointer): Integer; stdcall;
  AddFontMemResourceEx: function(pbFont: Pointer; cbFont: DWORD; pdv: Pointer; pcFonts: PDWORD): THandle; stdcall;
  RemoveFontResourceA: function(Filename: PAnsiChar): Boolean; stdcall;
  RemoveFontResourceExA: function(filename: PAnsiChar; Flag: DWORD; pdv: Pointer): Boolean; stdcall;
  RemoveFontMemResourceEx: function(fh: THandle): Boolean; stdcall;

  GetTextMetricsW: function(DC: HDC; var TM: TTextMetricW): Boolean; stdcall;
  //GetGlyphOutlineA: function(DC: HDC; uChar, uFormat: Word; const lpgm: TGlyphMetrics; cbBuffer: DWORD; lpvBuffer: Pointer; const lpmat2: TMat2): DWORD; stdcall;
  GetGlyphOutlineA: function(DC: HDC; uChar, uFormat: Cardinal; lpgm: PGlyphMetrics; cbBuffer: DWORD; lpvBuffer: Pointer; lpmat2: PMat2): DWORD; stdcall;

  GetCharacterPlacementW: function(DC: HDC; Str: PWideChar; Count, MaxExtent: Integer; Result: PGCPResultsW; Flags: DWORD): DWORD; stdcall;
  GetFontData: function(DC: HDC; TableName, Offset: DWORD; Buffer: Pointer; Data: DWORD): DWORD; stdcall;

  CreateCompatibleDC: function(DC: HDC): HDC; stdcall;
  DeleteDC: function(DC: HDC): Boolean; stdcall;
  SelectObject: function(DC: HDC; p2: HGDIOBJ): HGDIOBJ; stdcall;
  DeleteObject: function(p1: HGDIOBJ): Boolean; stdcall;

  GetLocaleInfoA: function(Locale: DWORD; LCType: DWORD; lpLCData: pAnsiChar; cchData: Integer): Integer; stdcall;

  GetOutlineTextMetricsW: function(DC: HDC; p2: LongWord; var OTMetricStructs: TOutlineTextmetricW): LongWord; stdcall;

  
// *** SDL globals ***
function Init_SDL: Boolean;
procedure Quit_SDL;

type
  PSDL_Color = ^TSDL_Color;
  TSDL_Color = record
    r: Byte;
    g: Byte;
    b: Byte;
    unused: Byte;
  end;

  TSDL_Rect = record
    X: Smallint;
    Y: Smallint;
    Width: Word;
    Height: Word;
  end;

  PSDL_ColorArray = ^TSDL_ColorArray;
  TSDL_ColorArray = array[0..65000] of TSDL_Color;

  PSDL_Palette = ^TSDL_Palette;
  TSDL_Palette = record
    ncolors: Integer;
    colors: PSDL_ColorArray;
  end;

  PSDL_PixelFormat = ^TSDL_PixelFormat;
  TSDL_PixelFormat = record
    Palette: PSDL_Palette;
    BitsPerPixel: Byte;
    BytesPerPixel: Byte;
    Rloss: Byte;
    Gloss: Byte;
    Bloss: Byte;
    Aloss: Byte;
    Rshift: Byte;
    Gshift: Byte;
    Bshift: Byte;
    Ashift: Byte;
    RMask: Cardinal;
    GMask: Cardinal;
    BMask: Cardinal;
    AMask: Cardinal;
    Colorkey: Cardinal;
    Alpha: Byte;
  end;


  PSDL_Surface = ^TSDL_Surface;
  TSDL_Surface = record
    Flags: Cardinal;
    Format: PSDL_PixelFormat;
    Width: Integer;
    Height: Integer;
    Pitch: Word;
    Pixels: Pointer;
    Offset: Integer;
    HWDdata: Pointer;
    ClipRect: TSDL_Rect;
    Unused1: Cardinal;
    Locked: Cardinal;
    Blitmap: Pointer;
    FormatVersion: Cardinal;
    RefCount: Integer;
  end;


const
  {$IFDEF WINDOWS}
  LIB_SDL = 'SDL.dll';
  {$ELSE}
  LIB_SDL = 'libSDL.so';
  LIB_SDL_VERSION = 'libSDL-1.2.so.0';
  {$ENDIF}

  SDL_SWSURFACE = $00000000;
  
var
  Library_SDL: Pointer;

  SDL_FreeSurface: procedure(surface: PSDL_Surface); cdecl;
  SDL_ConvertSurface: function(Source: PSDL_Surface; Format: PSDL_PixelFormat; flags: Cardinal): PSDL_Surface; cdecl;



// *** SDL_TTF ***
function Init_SDL_TTF: Boolean;
procedure Quit_SDL_TTF;

type
  PTTF_Font = ^TTTF_font;
  TTTF_Font = record end;


const
  {$IFDEF WINDOWS}
  LIB_SDL_TTF = 'SDL_ttf.dll';
  {$ELSE}
  LIB_SDL_TTF = 'libSDL_ttf.so';
  LIB_SDL_TTF_VERSION = 'libSDL_ttf-2.0.so.0';
  {$ENDIF}

  TTF_STYLE_NORMAL	  = $00;
  TTF_STYLE_BOLD      = $01;
  TTF_STYLE_ITALIC	  = $02;
//  TTF_STYLE_UNDERLINE	= $04;

// ZERO WIDTH NO-BREAKSPACE (Unicode byte order mark)
//  UNICODE_BOM_NATIVE  = $FEFF;
//  UNICODE_BOM_SWAPPED = $FFFE;

var
  SDL_TTF_initialized: Boolean;
  Library_SDL_TTF: Pointer;

  TTF_Init: function: Integer; cdecl;
  TTF_WasInit: function: Integer; cdecl;
  TTF_OpenFont: function(const Filename: pAnsiChar; PTSize: Integer): PTTF_Font; cdecl;
  TTF_CloseFont: procedure(Font: PTTF_Font); cdecl;

  TTF_GetFontStyle: function(Font: PTTF_Font): Integer; cdecl;
  TTF_SetFontStyle: procedure(Font: PTTF_Font; Style: Integer); cdecl;

  TTF_FontAscent: function(Font: PTTF_Font) : Integer; cdecl;
  TTF_FontDescent: function(Font: PTTF_Font) : Integer; cdecl;
  TTF_FontLineSkip: function(Font: PTTF_Font): Integer; cdecl;
  TTF_FontFaceIsFixedWidth: function(Font: PTTF_Font): Integer; cdecl;
  TTF_FontFaceFamilyName: function(Font: PTTF_Font): pAnsiChar; cdecl;
  TTF_FontFaceStyleName: function(Font : PTTF_Font): pAnsiChar; cdecl;
  TTF_GlyphMetrics: function(Font: PTTF_Font; CharCode: WORD; var MinX: Integer; var MaxX: Integer; var MinY: Integer; var MaxY: Integer; var Advance: Integer): Integer; cdecl;

  TTF_RenderGlyph_Solid: function(Font: PTTF_Font; Char: WORD; const ForeGround: TSDL_Color): PSDL_Surface; cdecl;
  TTF_RenderGlyph_Shaded: function(Font: PTTF_Font; Char: WORD; const ForeGround: TSDL_Color; const BackGround: TSDL_Color): PSDL_Surface; cdecl;



// *** SDL_IMAGE ***
function Init_SDL_IMAGE: Boolean;
procedure Quit_SDL_IMAGE;

const
  {$IFDEF WINDOWS}
  LIB_SDL_IMAGE = 'SDL_Image.dll';
  {$ELSE}
  LIB_SDL_IMAGE = 'libSDL_image.so';
  LIB_SDL_IMAGE_VERSION = 'libSDL_image-1.2.so.0';
  {$ENDIF}

var
  SDL_IMAGE_initialized: Boolean;
  Library_SDL_IMAGE: Pointer;

  IMG_Load: function(const _file: PAnsiChar): PSDL_Surface; cdecl;


implementation


function GetLibraryProc(hLibrary: Pointer; ProcName: pAnsiChar): Pointer;
begin
  {$IFDEF WINDOWS}
    Result := GetProcAddress(hLibrary, ProcName);
  {$ELSE}
    Result := dlsym(hLibrary, ProcName);
  {$ENDIF}
end;


function GetOpenGLLibraryProc(hLibrary: Pointer; ProcName: pAnsiChar): Pointer;
begin
  Result := GetLibraryProc(hLibrary, ProcName);
end;


// *** OpenGL ***

function Init_OpenGL: Boolean;
begin
  if Library_OpenGL = nil then begin
    {$IFDEF WINDOWS}
      Library_OpenGL := LoadLibrary(LIB_OPENGL);
    {$ELSE}
      Library_OpenGL := dlopen(LIB_OPENGL, RTLD_LAZY);
    {$ENDIF}
  end;

  if Library_OpenGL <>  nil then begin
    glEnable := GetOpenGLLibraryProc(Library_OpenGL, 'glEnable');
    glDisable := GetOpenGLLibraryProc(Library_OpenGL, 'glDisable');
    glColor4f := GetOpenGLLibraryProc(Library_OpenGL, 'glColor4f');
    glGenTextures := GetOpenGLLibraryProc(Library_OpenGL, 'glGenTextures');
    glDeleteTextures := GetOpenGLLibraryProc(Library_OpenGL, 'glDeleteTextures');
    glBindTexture := GetOpenGLLibraryProc(Library_OpenGL, 'glBindTexture');
    glTexParameteri := GetOpenGLLibraryProc(Library_OpenGL, 'glTexParameteri');
    glTexImage2D := GetOpenGLLibraryProc(Library_OpenGL, 'glTexImage2D');
    glTexSubImage2D := GetOpenGLLibraryProc(Library_OpenGL, 'glTexSubImage2D');
    glBegin := GetOpenGLLibraryProc(Library_OpenGL, 'glBegin');
    glEnd := GetOpenGLLibraryProc(Library_OpenGL, 'glEnd');
    glTexCoord2f := GetOpenGLLibraryProc(Library_OpenGL, 'glTexCoord2f');
    glTexCoord2fv := GetOpenGLLibraryProc(Library_OpenGL, 'glTexCoord2fv');
    glVertex2f := GetOpenGLLibraryProc(Library_OpenGL, 'glVertex2f');
    glVertex2fv := GetOpenGLLibraryProc(Library_OpenGL, 'glVertex2fv');
    glVertex2iv := GetOpenGLLibraryProc(Library_OpenGL, 'glVertex2iv');
    glGenLists := GetOpenGLLibraryProc(Library_OpenGL, 'glGenLists');
    glDeleteLists := GetOpenGLLibraryProc(Library_OpenGL, 'glDeleteLists');
    glCallList := GetOpenGLLibraryProc(Library_OpenGL, 'glCallList');
    glNewList := GetOpenGLLibraryProc(Library_OpenGL, 'glNewList');
    glEndList := GetOpenGLLibraryProc(Library_OpenGL, 'glEndList');
  end;

  OpenGL_initialized :=
    (Addr(glEnable) <> nil) and
    (Addr(glDisable) <> nil) and
    (Addr(glColor4f) <> nil) and
    (Addr(glGenTextures) <> nil) and
    (Addr(glDeleteTextures) <> nil) and
    (Addr(glBindTexture) <> nil) and
    (Addr(glTexParameteri) <> nil) and
    (Addr(glTexImage2D) <> nil) and
    (Addr(glTexSubImage2D) <> nil) and
    (Addr(glBegin) <> nil) and
    (Addr(glEnd) <> nil) and
    (Addr(glTexCoord2f) <> nil) and
    (Addr(glTexCoord2fv) <> nil) and
    (Addr(glVertex2f) <> nil) and
    (Addr(glVertex2fv) <> nil) and
    (Addr(glVertex2iv) <> nil) and
    (Addr(glGenLists) <> nil) and
    (Addr(glDeleteLists) <> nil) and
    (Addr(glCallList) <> nil) and
    (Addr(glNewList) <> nil) and
    (Addr(glEndList) <> nil);

  Result := OpenGL_initialized;
end;


procedure Quit_OpenGL;
begin
  glEnable := nil;
  glDisable := nil;
  glColor4f := nil;
  glGenTextures := nil;
  glDeleteTextures := nil;
  glBindTexture := nil;
  glTexParameteri := nil;
  glTexImage2D := nil;
  glTexSubImage2D := nil;
  glBegin := nil;
  glEnd := nil;
  glTexCoord2f := nil;
  glTexCoord2fv := nil;
  glVertex2f := nil;
  glVertex2fv := nil;
  glVertex2iv := nil;
  glGenLists := nil;
  glDeleteLists := nil;
  glCallList := nil;
  glNewList := nil;
  glEndList := nil;

  if Library_OpenGL <> nil then begin
    {$IFDEF WINDOWS}
      FreeLibrary(Library_OpenGL);
      Library_OpenGL := nil;
    {$ELSE}
      dlclose(Library_OpenGL);
      Library_OpenGL := nil;
    {$ENDIF}
  end;

  OpenGL_initialized := False;
end;


// *** Windows GDI globals ***
function Init_GDI: Boolean;
begin
  if Library_GDI32 = nil then begin
    {$IFDEF WINDOWS}
      Library_GDI32 := LoadLibrary(LIB_GDI32);
//    {$ELSE}
//      Library_GDI32 := nil; //dlopen(LIB_GDI, RTLD_LAZY);
    {$ENDIF}
  end;

  if Library_GDI32 <> nil then begin
    CreateFontIndirectA := GetLibraryProc(Library_GDI32, 'CreateFontIndirectA');

    AddFontResourceA := GetLibraryProc(Library_GDI32, 'AddFontResourceA');
    AddFontResourceExA := GetLibraryProc(Library_GDI32, 'AddFontResourceExA');
    AddFontMemResourceEx := GetLibraryProc(Library_GDI32, 'AddFontMemResourceEx');
    RemoveFontResourceA := GetLibraryProc(Library_GDI32, 'RemoveFontResourceA');
    RemoveFontResourceExA := GetLibraryProc(Library_GDI32, 'RemoveFontResourceExA');
    RemoveFontMemResourceEx := GetLibraryProc(Library_GDI32, 'RemoveFontMemResourceEx');

    GetTextMetricsW := GetLibraryProc(Library_GDI32, 'GetTextMetricsW');
    GetGlyphOutlineA := GetLibraryProc(Library_GDI32, 'GetGlyphOutlineA');

    GetCharacterPlacementW := GetLibraryProc(Library_GDI32, 'GetCharacterPlacementW');
    GetFontData := GetLibraryProc(Library_GDI32, 'GetFontData');

    CreateCompatibleDC := GetLibraryProc(Library_GDI32, 'CreateCompatibleDC');
    DeleteDC := GetLibraryProc(Library_GDI32, 'DeleteDC');
    SelectObject := GetLibraryProc(Library_GDI32, 'SelectObject');
    DeleteObject := GetLibraryProc(Library_GDI32, 'DeleteObject');

    GetOutlineTextMetricsW := GetLibraryProc(Library_GDI32, 'GetOutlineTextMetricsW');
  end;

  if Library_KERNEL32 = nil then begin
    {$IFDEF WINDOWS}
      Library_KERNEL32 := LoadLibrary(LIB_KERNEL32);
    {$ENDIF}
  end;

  if Library_KERNEL32 <> nil then begin
    GetLocaleInfoA := GetLibraryProc(Library_KERNEL32, 'GetLocaleInfoA');
  end;

  GDI_initialized :=
    (Addr(CreateFontIndirectA) <> nil) and

    ((Addr(AddFontResourceA) <> nil) or
     (Addr(AddFontResourceExA) <> nil)) and

    ((Addr(RemoveFontResourceA) <> nil) or
     (Addr(RemoveFontResourceExA) <> nil)) and

    (Addr(GetTextMetricsW) <> nil) and
    (Addr(GetGlyphOutlineA) <> nil) and

// under 9x GetCharacterPlacementW dosn't exist
    (Addr(GetCharacterPlacementW) <> nil) and
    (Addr(GetFontData) <> nil) and

    (Addr(CreateCompatibleDC) <> nil) and
    (Addr(DeleteDC) <> nil) and
    (Addr(SelectObject) <> nil) and
    (Addr(DeleteObject) <> nil) and

    (Addr(GetLocaleInfoA) <> nil) and

    (Addr(GetOutlineTextMetricsW) <> nil);

  Result := GDI_initialized;
end;


procedure Quit_GDI;
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

  if Library_GDI32 <> nil then begin
    {$IFDEF WINDOWS}
      FreeLibrary(Library_GDI32);
      Library_GDI32 := nil;
    {$ENDIF}
  end;

  GetLocaleInfoA := nil;

  if Library_KERNEL32 <> nil then begin
    {$IFDEF WINDOWS}
      FreeLibrary(Library_KERNEL32);
      Library_KERNEL32 := nil;
    {$ENDIF}
  end;

  GDI_initialized := False;
end;


// *** SDL globals ***
function Init_SDL: Boolean;
begin
  if Library_SDL = nil then begin
    {$IFDEF WINDOWS}
      Library_SDL := LoadLibrary(LIB_SDL);
    {$ELSE}
      Library_SDL := dlopen(LIB_SDL, RTLD_LAZY);

      if Library_SDL = nil then
        Library_SDL := dlopen(LIB_SDL_VERSION, RTLD_LAZY);
    {$ENDIF}
  end;

  if Library_SDL <> nil then begin
    SDL_FreeSurface := GetLibraryProc(Library_SDL, 'SDL_FreeSurface');
    SDL_ConvertSurface := GetLibraryProc(Library_SDL, 'SDL_ConvertSurface');
  end;

  Result :=
    (Addr(SDL_FreeSurface) <> nil) and
    (Addr(SDL_ConvertSurface) <> nil);
end;


procedure Quit_SDL;
begin
  SDL_FreeSurface := nil;
  SDL_ConvertSurface := nil;

  if Library_SDL <> nil then begin
    {$IFDEF WINDOWS}
      FreeLibrary(Library_SDL);
      Library_SDL := nil;
    {$ELSE}
      dlclose(Library_SDL);
      Library_SDL := nil;
    {$ENDIF}
  end;
end;


// *** SDL_TTF ***
function Init_SDL_TTF: Boolean;
begin
  if Library_SDL_TTF = nil then begin
    {$IFDEF WINDOWS}
      Library_SDL_TTF := LoadLibrary(LIB_SDL_TTF);
    {$ELSE}
      Library_SDL_TTF := dlopen(LIB_SDL_TTF, RTLD_LAZY);

      if Library_SDL_TTF = nil then      
        Library_SDL_TTF := dlopen(LIB_SDL_TTF_VERSION, RTLD_LAZY);
    {$ENDIF}
  end;

  if Library_SDL_TTF <> nil then begin
    TTF_Init := GetLibraryProc(Library_SDL_TTF, 'TTF_Init');
    TTF_WasInit := GetLibraryProc(Library_SDL_TTF, 'TTF_WasInit');
    TTF_OpenFont := GetLibraryProc(Library_SDL_TTF, 'TTF_OpenFont');
    TTF_CloseFont := GetLibraryProc(Library_SDL_TTF, 'TTF_CloseFont');
    TTF_GetFontStyle := GetLibraryProc(Library_SDL_TTF, 'TTF_GetFontStyle');
    TTF_SetFontStyle := GetLibraryProc(Library_SDL_TTF, 'TTF_SetFontStyle');
    TTF_FontAscent := GetLibraryProc(Library_SDL_TTF, 'TTF_FontAscent');
    TTF_FontDescent := GetLibraryProc(Library_SDL_TTF, 'TTF_FontDescent');
    TTF_FontLineSkip := GetLibraryProc(Library_SDL_TTF, 'TTF_FontLineSkip');
    TTF_FontFaceIsFixedWidth := GetLibraryProc(Library_SDL_TTF, 'TTF_FontFaceIsFixedWidth');
    TTF_FontFaceFamilyName := GetLibraryProc(Library_SDL_TTF, 'TTF_FontFaceFamilyName');
    TTF_FontFaceStyleName := GetLibraryProc(Library_SDL_TTF, 'TTF_FontFaceStyleName');
    TTF_GlyphMetrics := GetLibraryProc(Library_SDL_TTF, 'TTF_GlyphMetrics');
    TTF_RenderGlyph_Solid := GetLibraryProc(Library_SDL_TTF, 'TTF_RenderGlyph_Solid');
    TTF_RenderGlyph_Shaded := GetLibraryProc(Library_SDL_TTF, 'TTF_RenderGlyph_Shaded');
  end;

  SDL_TTF_initialized :=
    Init_SDL and
    (Addr(TTF_Init) <> nil) and
    (Addr(TTF_WasInit) <> nil) and
    (Addr(TTF_OpenFont) <> nil) and
    (Addr(TTF_CloseFont) <> nil) and
    (Addr(TTF_GetFontStyle) <> nil) and
    (Addr(TTF_SetFontStyle) <> nil) and
    (Addr(TTF_FontAscent) <> nil) and
    (Addr(TTF_FontDescent) <> nil) and
    (Addr(TTF_FontLineSkip) <> nil) and
    (Addr(TTF_FontFaceIsFixedWidth) <> nil) and
    (Addr(TTF_FontFaceFamilyName) <> nil) and
    (Addr(TTF_FontFaceStyleName) <> nil) and
    (Addr(TTF_GlyphMetrics) <> nil) and
    (Addr(TTF_RenderGlyph_Solid) <> nil) and
    (Addr(TTF_RenderGlyph_Shaded) <> nil);

  Result := SDL_TTF_initialized;
end;


procedure Quit_SDL_TTF;
begin
  TTF_Init := nil;
  TTF_WasInit := nil;
  TTF_OpenFont := nil;
  TTF_CloseFont := nil;
  TTF_GetFontStyle := nil;
  TTF_SetFontStyle := nil;
  TTF_FontAscent := nil;
  TTF_FontDescent := nil;
  TTF_FontLineSkip := nil;
  TTF_FontFaceIsFixedWidth := nil;
  TTF_FontFaceFamilyName := nil;
  TTF_FontFaceStyleName := nil;
  TTF_GlyphMetrics := nil;
  TTF_RenderGlyph_Solid := nil;
  TTF_RenderGlyph_Shaded := nil;

  if Library_SDL_TTF <> nil then begin
    {$IFDEF WINDOWS}
      FreeLibrary(Library_SDL_TTF);
      Library_SDL_TTF := nil;
    {$ELSE}
      dlclose(Library_SDl_TTF);
      Library_SDL_TTF := nil;
    {$ENDIF}
  end;

  SDL_TTF_initialized := False;
end;


// *** SDL_IMAGE ***
function Init_SDL_IMAGE: Boolean;
begin
  if Library_SDL_IMAGE = nil then begin
    {$IFDEF WINDOWS}
      Library_SDL_IMAGE := LoadLibrary(LIB_SDL_IMAGE);
    {$ELSE}
      Library_SDL_IMAGE := dlopen(LIB_SDL_IMAGE, RTLD_LAZY);

      if Library_SDL_IMAGE = nil then
        Library_SDL_IMAGE := dlopen(LIB_SDL_IMAGE_VERSION, RTLD_LAZY);
    {$ENDIF}
  end;

  if Library_SDL_IMAGE <> nil then begin
    IMG_Load := GetLibraryProc(Library_SDL_IMAGE, 'IMG_Load');
  end;

  SDL_IMAGE_initialized :=
    Init_SDL and
    (Addr(IMG_load) <> nil);

  Result := SDL_IMAGE_initialized;
end;


procedure Quit_SDL_IMAGE;
begin
  IMG_Load := nil;

  if Library_SDL_IMAGE <> nil then begin
    {$IFDEF WINDOWS}
      FreeLibrary(Library_SDL_IMAGE);
      Library_SDL_IMAGE := nil;
    {$ELSE}
      dlclose(Library_SDL_IMAGE);
      Library_SDL_IMAGE := nil;
    {$ENDIF}
  end;
end;

end.
