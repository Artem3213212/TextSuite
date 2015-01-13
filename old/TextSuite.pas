{
TextSuite (C) Steffen Xonna (aka Lossy eX)
http://www.opengl24.de/
-----------------------------------------------------------------------
For copyright informations see file copyright.txt.
}

{$I TextSuiteOptions.inc}

unit TextSuite;


interface


type
  tsBool = Cardinal;
  tsByte = Byte;
  tsInt = Integer;
  tsFloat = Single;
  tsEnum = Cardinal;
  tsBitmask = Cardinal;
  tsError = Cardinal;

  ptsBool = ^tsBool;
  ptsByte = ^tsByte;
  ptsInt = ^tsInt;
  ptsEnum = ^tsEnum;

  tsContextID = Cardinal;
  tsFontID = Cardinal;
  tsImageID = Cardinal;

  ptsContextID = ^tsContextID;
  ptsFontID = ^tsFontID;
  ptsImageID = ^tsImageID;

  PPointer = ^Pointer;

  tsPostProcessProc = procedure(ImageID: tsImageID; CharCode: WideChar; Data: Pointer); cdecl;


  // these types are more internally.

  ptsColor = ^tsColor;
  tsColor = packed record
    Red: Byte;
    Green: Byte;
    Blue: Byte;
    Alpha: Byte;
  end;

  ptsRect = ^tsRect;
  tsRect = packed record
    Left: Integer;
    Top: Integer;
    Right: Integer;
    Bottom: Integer;
  end;

  ptsPoint = ^tsPoint;
  tsPoint = packed record
    X: Integer;
    Y: Integer;
  end;

  
  
const
{$ifdef TS_EXTERN_STATIC}
  {$ifdef WINDOWS}
    TS_LIBRARY = 'libTextSuite.dll';
  {$else}
    {$ifdef darwin}
    TS_LIBRARY = 'libTextSuite.dylib';
    {$else}
    TS_LIBRARY = 'libTextSuite.so';
    {$endif}
  {$endif}
{$endif}

  // Booleans
  TS_TRUE = 1;
  TS_FALSE = 0;

  TS_DEFAULT = 0;

  // Inits
  TS_INIT_TEXTSUITE = $01;
  TS_INIT_SDL_TTF = $02;
  TS_INIT_SDL_IMAGE = $04;
  TS_INIT_GDI = $08;
  TS_INIT_OPENGL = $10;
//  TS_INIT_FREETYPE2 = $20;

  // library infos
  TS_INFO_MAYOR_VERSION = $01;
  TS_INFO_MINOR_VERSION = $02;
  TS_INFO_BUILD_NUMBER = $03;
  TS_INFO_VERSION = $04;
  TS_INFO_COPYRIGHT = $05;

  TS_GLOBAL_ANTIALIASING = $11;
  TS_GLOBAL_FORMAT = $12;

  // debug infos
  TS_DEBUG_DRAW_CHAR_RECTS = $30;

  // context
  TS_CONTEXT_BINDING = $40;

  // Renderer
  TS_RENDERER = $50;
  TS_RENDERER_NULL = $51;
  TS_RENDERER_OPENGL = $52;

  TS_RENDERER_NULL_SAVE_IMAGES = $60;

  TS_RENDERER_OPENGL_TEXTURE_SIZE = $70;
  TS_RENDERER_OPENGL_TEXTURE_SIZE_128 = $71;
  TS_RENDERER_OPENGL_TEXTURE_SIZE_256 = $72;
  TS_RENDERER_OPENGL_TEXTURE_SIZE_512 = $73;
  TS_RENDERER_OPENGL_TEXTURE_SIZE_1024 = $74;

  // Font
  TS_FONT_BINDING = $100;
  TS_FONT_COPYRIGHT = $101;
  TS_FONT_FACE_NAME = $102;
  TS_FONT_STYLE_NAME = $103;
  TS_FONT_FULL_NAME = $104;
  TS_FONT_SIZE = $105;
  TS_FONT_STYLE = $106;
  TS_FONT_FILE_STYLE = $107;
  TS_FONT_FIXED_WIDTH = $108;
  TS_FONT_ANTIALIASING = $109;
  TS_FONT_FORMAT = $10A;
  TS_FONT_ASCENT = $10B;
  TS_FONT_DESCENT = $113;
  TS_FONT_EXTERNAL_LEADING = $114;
  TS_FONT_LINESKIP = $10C;
  TS_FONT_CHAR_SPACING = $10D;
  TS_FONT_LINE_SPACING = $10E;
  TS_FONT_UNDERLINE_POSITION = $10F;
  TS_FONT_UNDERLINE_SIZE = $110;
  TS_FONT_STRIKEOUT_POSITION = $111;
  TS_FONT_STRIKEOUT_SIZE = $112;
  TS_FONT_BASELINE_OFFSET = $115;

  // Char parameters
  TS_CHAR_ADVANCE = $131;
  TS_CHAR_GLYPHORIGIN = $132;
  TS_CHAR_GLYPHORIGIN_X = $133;
  TS_CHAR_GLYPHORIGIN_Y = $134;
  TS_CHAR_GLYPHRECT = $135;
  TS_CHAR_GLYPHRECT_TOP = $136;
  TS_CHAR_GLYPHRECT_LEFT = $137;
  TS_CHAR_GLYPHRECT_RIGHT = $138;
  TS_CHAR_GLYPHRECT_BOTTOM = $139;

  // Creator
  TS_CREATOR = $200;
  TS_CREATOR_SDL = $201;
  TS_CREATOR_GDI = $202;
  TS_CREATOR_GDI_FACENAME = $203;
//  TS_CREATOR_FREETYPE2 = $204;
  TS_CREATOR_GDI_STREAM = $205;

  TS_CREATOR_CREATE_CHARS = $210;
  TS_CREATOR_ADD_RESIZING_BORDER = $211;

  // Font Style
  TS_STYLE_NORMAL = $00;
  TS_STYLE_BOLD = $01;
  TS_STYLE_ITALIC = $02;
  TS_STYLE_UNDERLINE = $04;
  TS_STYLE_STRIKEOUT = $08;

  // AntiAliasing
  TS_ANTIALIASING_NONE = $221;
  TS_ANTIALIASING_NORMAL = $222;

  // Formats
  TS_FORMAT_EMPTY = $231;
  TS_FORMAT_RGBA8 = $232;

  // PostProcessing usage type
  TS_POST_INDEX_ALL = -2;
  TS_POST_INDEX_LAST = -1;
  TS_POST_USAGE_INCLUDE = $291;
  TS_POST_USAGE_EXCLUDE = $292;

  // output consts
  // Single line
  TS_SINGLE_LINE = $300;
  TS_SINGLE_LINE_TOP = $301;
  TS_SINGLE_LINE_BASELINE = $302;

  // Textalign
  TS_ALIGN = $310;
  TS_ALIGN_LEFT = $311;
  TS_ALIGN_CENTER = $312;
  TS_ALIGN_RIGHT = $313;
  TS_ALIGN_BLOCK = $314;

  TS_VALIGN = $320;
  TS_VALIGN_TOP = $321;
  TS_VALIGN_CENTER = $322;
  TS_VALIGN_BOTTOM = $323;

  // Textblockflags
  TS_BLOCK_OFFSET_X = $331;
  TS_BLOCK_OFFSET_Y = $332;

  TS_BLOCKFLAG_NONE = $00;
  TS_BLOCKFLAG_NO_CLIP = $01;
  TS_BLOCKFLAG_CALC_SIZE = $02;
  TS_BLOCKFLAG_WORD_WRAP = $04;
//  TS_BF_SINGLE_LINE = $08;
//  TS_BF_END_ELLIPSIS = $10;

  TS_CLIP = $340;
  TS_CLIP_COMPLETE = $341;
  TS_CLIP_BORDER = $342;

{
  TS_TAB = $350;
  TS_TAB_FIXED = $351;
  TS_TAB_ABSOLUTE = $352;
  TS_TAB_FIXED_WIDTH = $353;
  TS_TAB_ABSOLUTE_POSITIONS = $354;
}

  // Code pages
  TS_EMPTY_CP_ENTRY = $1000;
  TS_EMPTY_CP_ENTRY_IGNORE = $1001;
  TS_EMPTY_CP_ENTRY_USE_DEFAULT = $1002;

  TS_CODEPAGE = $1100;
  TS_CODEPAGE_UTF8 = TS_CODEPAGE + 1;
  TS_CODEPAGE_8859_1 = TS_CODEPAGE + 11;
  TS_CODEPAGE_8859_2 = TS_CODEPAGE + 12;
  TS_CODEPAGE_8859_3 = TS_CODEPAGE + 13;
  TS_CODEPAGE_8859_4 = TS_CODEPAGE + 14;
  TS_CODEPAGE_8859_5 = TS_CODEPAGE + 15;
  TS_CODEPAGE_8859_6 = TS_CODEPAGE + 16;
  TS_CODEPAGE_8859_7 = TS_CODEPAGE + 17;
  TS_CODEPAGE_8859_8 = TS_CODEPAGE + 18;
  TS_CODEPAGE_8859_9 = TS_CODEPAGE + 19;
  TS_CODEPAGE_8859_10 = TS_CODEPAGE + 20;
  TS_CODEPAGE_8859_11 = TS_CODEPAGE + 21;
  TS_CODEPAGE_8859_13 = TS_CODEPAGE + 22;
  TS_CODEPAGE_8859_14 = TS_CODEPAGE + 23;
  TS_CODEPAGE_8859_15 = TS_CODEPAGE + 24;
  TS_CODEPAGE_8859_16 = TS_CODEPAGE + 25;
  TS_CODEPAGE_037 = TS_CODEPAGE + 31;
  TS_CODEPAGE_437 = TS_CODEPAGE + 32;
  TS_CODEPAGE_500 = TS_CODEPAGE + 33;
  TS_CODEPAGE_737 = TS_CODEPAGE + 34;
  TS_CODEPAGE_775 = TS_CODEPAGE + 35;
  TS_CODEPAGE_850 = TS_CODEPAGE + 36;
  TS_CODEPAGE_852 = TS_CODEPAGE + 37;
  TS_CODEPAGE_855 = TS_CODEPAGE + 38;
  TS_CODEPAGE_857 = TS_CODEPAGE + 39;
  TS_CODEPAGE_860 = TS_CODEPAGE + 40;
  TS_CODEPAGE_861 = TS_CODEPAGE + 41;
  TS_CODEPAGE_862 = TS_CODEPAGE + 42;
  TS_CODEPAGE_863 = TS_CODEPAGE + 43;
  TS_CODEPAGE_864 = TS_CODEPAGE + 44;
  TS_CODEPAGE_865 = TS_CODEPAGE + 45;
  TS_CODEPAGE_866 = TS_CODEPAGE + 46;
  TS_CODEPAGE_869 = TS_CODEPAGE + 47;
  TS_CODEPAGE_874 = TS_CODEPAGE + 48;
  TS_CODEPAGE_875 = TS_CODEPAGE + 49;
  TS_CODEPAGE_1026 = TS_CODEPAGE + 50;
  TS_CODEPAGE_1250 = TS_CODEPAGE + 51;
  TS_CODEPAGE_1251 = TS_CODEPAGE + 52;
  TS_CODEPAGE_1252 = TS_CODEPAGE + 53;
  TS_CODEPAGE_1253 = TS_CODEPAGE + 54;
  TS_CODEPAGE_1254 = TS_CODEPAGE + 55;
  TS_CODEPAGE_1255 = TS_CODEPAGE + 56;
  TS_CODEPAGE_1256 = TS_CODEPAGE + 57;
  TS_CODEPAGE_1257 = TS_CODEPAGE + 58;
  TS_CODEPAGE_1258 = TS_CODEPAGE + 59;

  // channel masks
  TS_CHANNEL_RED = $01;
  TS_CHANNEL_GREEN = $02;
  TS_CHANNEL_BLUE = $04;
  TS_CHANNEL_ALPHA = $08;
  TS_CHANNEL_LUMINANCE = $10;

  TS_CHANNELS_RGB = TS_CHANNEL_RED or TS_CHANNEL_GREEN or TS_CHANNEL_BLUE;
  TS_CHANNELS_RGBA = TS_CHANNELS_RGB or TS_CHANNEL_ALPHA;
  TS_CHANNELS_LUMINANCE_ALPHA = TS_CHANNEL_LUMINANCE or TS_CHANNEL_ALPHA;

  TS_IMAGE_RED_MODE = $401;
  TS_IMAGE_GREEN_MODE = $402;
  TS_IMAGE_BLUE_MODE = $403;
  TS_IMAGE_ALPHA_MODE = $404;
  TS_IMAGE_LUMINANCE_MODE = $405;

  TS_MODE_REPLACE = $411;
  TS_MODE_MODULATE = $412;

  // Imagelibrary
  TS_IMAGE_LIBRARY = $420;
  TS_IMAGE_LIBRARY_SDL = $421;


  // Errorcode
  TS_NO_ERROR = $0000;
  TS_ERROR = $8000;
  TS_NO_ACTIVE_CONTEXT = TS_ERROR + $1;
  TS_NO_ACTIVE_RENDERER = TS_ERROR + $2;
  TS_NO_ACTIVE_FONT = TS_ERROR + $3;
  TS_INVALID_OPERATION = TS_ERROR + $4;
  TS_INVALID_ENUM = TS_ERROR + $5;
  TS_INVALID_VALUE = TS_ERROR + $6;
  TS_OUT_OF_MEMORY = TS_ERROR + $7;
  TS_NOT_INITIALIZED = TS_ERROR + $8;

  TS_NO_FUNC = $0000;
  TS_FUNC = $4000;
  TS_FUNC_INIT = TS_FUNC + $01;
  TS_FUNC_QUIT = TS_FUNC + $02;
  TS_FUNC_SET_PARAMETER = TS_FUNC + $03;
  TS_FUNC_GET_PARAMETER = TS_FUNC + $04;
  TS_FUNC_GET_STRING = TS_FUNC + $05;
  TS_FUNC_CONTEXT_CREATE = TS_FUNC + $06;
  TS_FUNC_CONTEXT_DESTROY = TS_FUNC + $07;
  TS_FUNC_CONTEXT_BIND = TS_FUNC + $08;
  TS_FUNC_FONT_CREATE_CREATOR = TS_FUNC + $09;
  TS_FUNC_FONT_DESTROY = TS_FUNC + $0A;
  TS_FUNC_FONT_BIND = TS_FUNC + $0B;
  TS_FUNC_FONT_ADD_CHAR = TS_FUNC + $0C;
  TS_FUNC_FONT_DELETE_CHAR = TS_FUNC + $0D;
  TS_FUNC_FONT_SET_CHAR_PARAMETER = TS_FUNC + $0E;
  TS_FUNC_FONT_GET_CHAR_PARAMETER = TS_FUNC + $0F;
  TS_FUNC_POST_ADD_FILL_COLOR = TS_FUNC + $10;
  TS_FUNC_POST_ADD_FILL_PATTERN = TS_FUNC + $11;
  TS_FUNC_POST_ADD_BORDER = TS_FUNC + $12;
  TS_FUNC_POST_ADD_SHADOW = TS_FUNC + $13;
  TS_FUNC_POST_ADD_CUSTOM = TS_FUNC + $14;
  TS_FUNC_POST_DELETE = TS_FUNC + $15;
  TS_FUNC_POST_ADD_USAGE = TS_FUNC + $16;
  TS_FUNC_POST_CLEAR_USAGE = TS_FUNC + $17;
  TS_FUNC_STRING_ANSI_TO_WIDE = TS_FUNC + $18;
  TS_FUNC_STRING_ALLOC = TS_FUNC + $19;
  TS_FUNC_STRING_DISPOSE = TS_FUNC + $1A;
  TS_FUNC_TEXT_BEGIN_BLOCK = TS_FUNC + $1B;
  TS_FUNC_TEXT_END_BLOCK = TS_FUNC + $1C;
  TS_FUNC_TEXT_COLOR = TS_FUNC + $1D;
  TS_FUNC_TEXT_OUT = TS_FUNC + $1E;
  TS_FUNC_TEXT_GET_WIDTH = TS_FUNC + $1F;
  TS_FUNC_TEXT_GET_HEIGHT = TS_FUNC + $20;
  TS_FUNC_IMAGE_CREATE = TS_FUNC + $21;
  TS_FUNC_IMAGE_DESTROY = TS_FUNC + $22;
  TS_FUNC_IMAGE_LOAD = TS_FUNC + $23;
  TS_FUNC_IMAGE_ASSIGN_FROM = TS_FUNC + $24;
  TS_FUNC_IMAGE_NEW = TS_FUNC + $25;
  TS_FUNC_IMAGE_GET_INFO = TS_FUNC + $26;
  TS_FUNC_IMAGE_SCANLINE = TS_FUNC + $27;
  TS_FUNC_IMAGE_RESIZE = TS_FUNC + $28;
  TS_FUNC_IMAGE_BLEND = TS_FUNC + $29;
  TS_FUNC_IMAGE_BLUR = TS_FUNC + $2A;
  TS_FUNC_IMAGE_FILL_COLOR = TS_FUNC + $2B;
  TS_FUNC_IMAGE_FILL_PATTERN = TS_FUNC + $2C;



  // *** global functions ***
  function tsInit(Names: tsEnum): tsBool; cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsInit'; {$endif}
  function tsWasInit(Names: tsEnum): tsBool; cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsWasInit'; {$endif}
  procedure tsQuit; cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsQuit'; {$endif}
  function tsGetError: tsEnum; cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsGetError'; {$endif}
  function tsGetErrorFunction: tsEnum; cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsGetErrorFunction'; {$endif}
  function tsGetErrorStringA(ErrorCode: tsEnum): pAnsiChar; cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsGetErrorStringA'; {$endif}

  procedure tsSetParameteri(ParamName: tsEnum; Param: tsInt); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsSetParameteri'; {$endif}
  procedure tsSetParameteriv(ParamName: tsEnum; pParam: ptsInt); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsSetParameteriv'; {$endif}
  function tsGetParameteri(ParamName: tsEnum): tsInt; cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsGetParameteri'; {$endif}
  procedure tsGetParameteriv(ParamName: tsEnum; pParam: ptsInt); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsGetParameteriv'; {$endif}

  function tsGetStringA(ParamName: tsEnum): pAnsiChar; cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsGetStringA'; {$endif}

  // *** context functions ***
  procedure tsContextCreate(pContextID: ptsContextID); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsContextCreate'; {$endif}
  procedure tsContextDestroy(ContextID: tsContextID); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsContextDestroy'; {$endif}
  procedure tsContextBind(ContextID: tsContextID); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsContextBind'; {$endif}

  // *** font functions ***
//  procedure tsFontCreate(pFontID: ptsFontID); cdecl;
//    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name ; {$endif}
  procedure tsFontCreateCreatorA(Name: pAnsiChar; Size: tsInt; Style: tsBitmask; AntiAliasing: tsEnum; Format: tsEnum; pFontID: ptsFontID); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsFontCreateCreatorA'; {$endif}
  procedure tsFontDestroy(FontID: tsFontID); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsFontDestroy'; {$endif}
  procedure tsFontBind(FontID: tsFontID); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsFontBind'; {$endif}

  procedure tsFontAddCharRange(CharStart: WideChar; CharEnd: WideChar); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsFontAddCharRange'; {$endif}
  procedure tsFontAddChars(Chars: pWideChar); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsFontAddChars'; {$endif}
  procedure tsFontAddChar(Char: WideChar); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsFontAddChar'; {$endif}

  procedure tsFontDeleteCharRange(CharStart: WideChar; CharEnd: WideChar); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsFontDeleteCharRange'; {$endif}
  procedure tsFontDeleteChars(Chars: pWideChar); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsFontDeleteChars'; {$endif}
  procedure tsFontDeleteChar(Char: WideChar); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsFontDeleteChar'; {$endif}

  procedure tsFontSetCharParameteri(Char: WideChar; ParamName: tsEnum; Param: tsInt); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsFontSetCharParameteri'; {$endif}
  procedure tsFontSetCharParameteriv(Char: WideChar; ParamName: tsEnum; pParam: ptsInt); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsFontSetCharParameteri'; {$endif}
  function tsFontGetCharParameteri(Char: WideChar; ParamName: tsEnum): tsInt; cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsFontGetCharParameteri'; {$endif}
  procedure tsFontGetCharParameteriv(Char: WideChar; ParamName: tsEnum; pParam: ptsInt); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsFontGetCharParameteri'; {$endif}

  procedure tsPostAddFillColor3ub(Red, Green, Blue: tsByte; ChannelMask: tsBitmask); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsPostAddFillColor3ub'; {$endif}
  procedure tsPostAddFillColor3f(Red, Green, Blue: tsFloat; ChannelMask: tsBitmask); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsPostAddFillColor3f'; {$endif}
  procedure tsPostAddFillColor4ub(Red, Green, Blue, Alpha: tsByte; ChannelMask: tsBitmask); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsPostAddFillColor4ub'; {$endif}
  procedure tsPostAddFillColor4f(Red, Green, Blue, Alpha: tsFloat; ChannelMask: tsBitmask); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsPostAddFillColor4f'; {$endif}

  procedure tsPostAddFillPattern(PatternImageID: tsImageID; X, Y: tsInt; ChannelMask: tsBitmask); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsPostAddFillPattern'; {$endif}

  procedure tsPostAddBorder3ub(Width, Strength: tsFloat; Red, Green, Blue: tsByte); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsPostAddBorder3ub'; {$endif}
  procedure tsPostAddBorder3f(Width, Strength: tsFloat; Red, Green, Blue: tsFloat); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsPostAddBorder3f'; {$endif}
  procedure tsPostAddBorder4ub(Width, Strength: tsFloat; Red, Green, Blue, Alpha: tsByte); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsPostAddBorder4ub'; {$endif}
  procedure tsPostAddBorder4f(Width, Strength: tsFloat; Red, Green, Blue, Alpha: tsFloat); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsPostAddBorder4f'; {$endif}

  procedure tsPostAddShadow3ub(Radius: tsFloat; X, Y: tsInt; Red, Green, Blue: tsByte); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsPostAddShadow3ub'; {$endif}
  procedure tsPostAddShadow3f(Radius: tsFloat; X, Y: tsInt; Red, Green, Blue: tsFloat); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsPostAddShadow3f'; {$endif}
  procedure tsPostAddShadow4ub(Radius: tsFloat; X, Y: tsInt; Red, Green, Blue, Alpha: tsByte); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsPostAddShadow4ub'; {$endif}
  procedure tsPostAddShadow4f(Radius: tsFloat; X, Y: tsInt; Red, Green, Blue, Alpha: tsFloat); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsPostAddShadow4f'; {$endif}

//  procedure tsPostAddKerning; cdecl;
//    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsPostAddKerning'; {$endif}
  procedure tsPostAddCustom(PostProcessProc: tsPostProcessProc; Data: Pointer); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsPostAddCustom'; {$endif}

  procedure tsPostDelete(PostIndex: tsInt); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsPostDelete'; {$endif}

  procedure tsPostAddUsageRange(PostIndex: tsInt; UsageType: tsEnum; CharStart, CharEnd: WideChar); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsPostAddUsageRange'; {$endif}
  procedure tsPostAddUsageChars(PostIndex: tsInt; UsageType: tsEnum; Chars: pWideChar); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsPostAddUsageChars'; {$endif}

  procedure tsPostClearUsage(PostIndex: tsInt); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsPostClearUsage'; {$endif}

  // *** string functions ***
  function tsStringAnsiToWide(pText: pAnsiChar): pWideChar; cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsStringAnsiToWide'; {$endif}
  function tsStringAlloc(Size: tsInt): pWideChar; cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsStringAlloc'; {$endif}
  procedure tsStringDispose(pText: pWideChar); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsStringDispose'; {$endif}

  // *** drawing functions ***
  procedure tsTextBeginBlock(Left, Top, Width, Height: tsInt; Flags: tsBitmask); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsTextBeginBlock'; {$endif}
  procedure tsTextEndBlock; cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsTextEndBlock'; {$endif}

  procedure tsTextColor3ub(Red, Green, Blue: tsByte); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsTextColor3ub'; {$endif}
  procedure tsTextColor3f(Red, Green, Blue: tsFloat); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsTextColor3f'; {$endif}
  procedure tsTextColor4ub(Red, Green, Blue, Alpha: tsByte); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsTextColor4ub'; {$endif}
  procedure tsTextColor4f(Red, Green, Blue, Alpha: tsFloat); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsTextColor4f'; {$endif}

  procedure tsTextOutA(pText: pAnsiChar); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsTextOutA'; {$endif}
  procedure tsTextOutW(pText: pWideChar); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsTextOutW'; {$endif}
  function tsTextGetWidthA(pText: pAnsiChar): tsInt; cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsTextGetWidthA'; {$endif}
  function tsTextGetWidthW(pText: pWideChar): tsInt; cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsTextGetWidthW'; {$endif}
  function tsTextGetHeightA(pText: pAnsiChar): tsInt; cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsTextGetHeightA'; {$endif}
  function tsTextGetHeightW(pText: pWideChar): tsInt; cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsTextGetHeightW'; {$endif}

  procedure tsCharOutW(CharCode: WideChar); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsCharOutW'; {$endif}

  // *** Image functions ***
  procedure tsImageCreate(pImageID: ptsImageID); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsImageCreate'; {$endif}
  procedure tsImageDestroy(ImageID: tsImageID); cdecl; 
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsImageDestroy'; {$endif}
  procedure tsImageLoadA(ImageID: tsImageID; Filename: pAnsiChar); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsImageLoadA'; {$endif}
  procedure tsImageAssignFrom(ImageID: tsImageID; FromImageID: tsImageID); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsImageAssignFrom'; {$endif}
  procedure tsImageNew(ImageID: tsImageID; Width: tsInt; Height: tsInt; Format: tsEnum); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsImageNew'; {$endif}

  procedure tsImageGetInfo(ImageID: tsImageID; pisEmpty: ptsBool; pWidth: ptsInt; pHeight: ptsInt; pFormat: ptsEnum; pData: PPointer); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsImageGetInfo'; {$endif}
  function tsImageGetIsEmpty(ImageID: tsImageID): tsBool; cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsImageGetIsEmpty'; {$endif}
  function tsImageGetWidth(ImageID: tsImageID): tsInt; cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsImageGetWidth'; {$endif}
  function tsImageGetHeight(ImageID: tsImageID): tsInt; cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsImageGetHeight'; {$endif}
  function tsImageGetFormat(ImageID: tsImageID): tsEnum; cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsImageGetFormat'; {$endif}
  function tsImageGetData(ImageID: tsImageID): Pointer; cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsImageGetData'; {$endif}
  function tsImageScanline(ImageID: tsImageID; Scanline: tsInt): Pointer; cdecl; 
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsImageScanline'; {$endif}

  procedure tsImageResize(ImageID: tsImageID; Width, Height, X, Y: tsInt); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsImageResize'; {$endif}
  procedure tsImageBlend(ImageID, OverImageID: tsImageID; X, Y: tsInt; AutoExpand: tsBool); cdecl; 
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsImageBlend'; {$endif}
  procedure tsImageBlur(ImageID: tsImageID; X, Y: tsFloat; ChannelMask: tsBitmask; AutoExpand: tsBool; ExpandSizeX, ExpandSizeY: ptsInt); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsImageBlur'; {$endif}

  procedure tsImageFillColor3ub(ImageID: tsImageID; Red, Green, Blue: tsByte; ChannelMask: tsBitmask); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsImageFillColor3ub'; {$endif}
  procedure tsImageFillColor3f(ImageID: tsImageID; Red, Green, Blue: tsFloat; ChannelMask: tsBitmask); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsImageFillColor3f'; {$endif}
  procedure tsImageFillColor4ub(ImageID: tsImageID; Red, Green, Blue, Alpha: tsByte; ChannelMask: tsBitmask); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsImageFillColor4ub'; {$endif}
  procedure tsImageFillColor4f(ImageID: tsImageID; Red, Green, Blue, Alpha: tsFloat; ChannelMask: tsBitmask); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsImageFillColor4f'; {$endif}

  procedure tsImageFillPattern(ImageID, PatternImageID: tsImageID; X, Y: tsInt; ChannelMask: tsBitmask); cdecl;
    {$ifdef TS_EXTERN_STATIC} external TS_LIBRARY name 'tsImageFillPattern'; {$endif}



implementation

{ This define ignores implementations of all functions }
{$ifndef TS_EXTERN_STATIC}

uses
  Classes,
  SysUtils,
  SyncObjs,
  TextSuiteClasses,
  TextSuitePostProcess,
  TextSuiteImports,
  {$ifndef TS_PURE_PASCAL}
  TextSuiteCPUUtils,
  {$endif}
  TextSuiteVersion,
  TextSuiteWideUtils;


// string consts
const
  TS_COPYRIGHT_STR = 'TextSuite (c) 2007-2009 by Steffen Xonna';


// error consts
const
  TS_NO_ERROR_STR = 'no error';
  TS_ERROR_STR = 'unknown error';
  TS_NO_ACTIVE_CONTEXT_STR = 'no active context';
  TS_NO_ACTIVE_RENDERER_STR = 'no active renderer';
  TS_NO_ACTIVE_FONT_STR = 'no active font';
  TS_INVALID_OPERATION_STR = 'invalid operation';
  TS_INVALID_ENUM_STR = 'invalid enum';
  TS_INVALID_VALUE_STR = 'invalid value';
  TS_OUT_OF_MEMORY_STR = 'out of memory';
  TS_NOT_INITIALIZED_STR = 'not initialized';


// global variables
var
  TextSuite_initialized: Boolean;
  TextSuite_initialized_count: Integer; 

  gCriticalSection: TCriticalSection;
  gContexts: TtsHash;
  gStrings: TtsStringHash;


  
// globals for the threads
threadvar
  gError: tsEnum;
  gErrorFunction: tsEnum;

  gContext: TtsContext;



procedure SetError(Error, ErrorFunction: tsEnum);
begin
  gError := Error;
  gErrorFunction := ErrorFunction;
end;

  
procedure ClearContexts;
var
  List: TList;
  Context: TtsContext;
  Idx: Integer;
begin
  List := TList.Create;
  try
    gContexts.GetValues(List);
    gContexts.Clear;
    gContext := nil;

    for Idx := 0 to List.Count - 1 do begin
      Context := List[Idx];

      Context.Free;
    end;
  finally
    List.Free;
  end;
end;


function Init_TextSuite: boolean;
begin
  if (TextSuite_initialized_count = 0) or (not TextSuite_initialized) then begin
    {$ifndef TS_PURE_PASCAL}
    ReadCPUFlags;

    if supportFPU and supportCMOV and supportMMX then begin
    {$endif}
      // global section
      if gCriticalSection = nil then
        gCriticalSection := TCriticalSection.Create;

      // Context management
      if gContexts = nil then
        gContexts := TtsHash.Create(113);

      // string management
      if gStrings = nil then
        gStrings := TtsStringHash.Create(4219);

      TextSuite_initialized :=
        (gCriticalSection <> nil) and
        (gContexts <> nil) and
        (gStrings <> nil);
    {$ifndef TS_PURE_PASCAL}
    end;
    {$endif}
  end;

  Inc(TextSuite_initialized_count);

  Result := TextSuite_initialized;
end;


procedure Quit_TextSuite;
begin
  Dec(TextSuite_initialized_count);

  if TextSuite_initialized_count = 0 then begin
    // destroy all contexts
    ClearContexts;

    // destroy hash
    gStrings.Free;
    gStrings := nil;

    gContexts.Free;
    gContexts := nil;

    gCriticalSection.Free;
    gCriticalSection := nil;

    gContext := nil;

    TextSuite_initialized := False;
  end;
end;



// *** global functions
function tsInit(Names: tsEnum): tsBool;
var
  ResNames: tsEnum;
begin
  Result := TS_FALSE;
  try
    ResNames := 0;

    if (Names and TS_INIT_TEXTSUITE) > 0 then
      if Init_TextSuite then
        ResNames := ResNames or TS_INIT_TEXTSUITE;

    if (Names and TS_INIT_SDL_TTF) > 0 then
      if Init_SDL_TTF then
        ResNames := ResNames or TS_INIT_SDL_TTF;

    if (Names and TS_INIT_SDL_IMAGE) > 0 then
      if Init_SDL_IMAGE then
        ResNames := ResNames or TS_INIT_SDL_IMAGE;

    if (Names and TS_INIT_GDI) > 0 then
      if Init_GDI then
        ResNames := ResNames or TS_INIT_GDI;

    if (Names and TS_INIT_OPENGL) > 0 then
      if Init_OpenGL then
        ResNames := ResNames or TS_INIT_OPENGL;

    if ResNames = Names then
      Result := TS_TRUE;

    if Result = TS_FALSE then
      SetError(TS_NOT_INITIALIZED, TS_FUNC_INIT);
  except
    SetError(TS_ERROR, TS_FUNC_INIT);
  end;
end;


function tsWasInit(Names: tsEnum): tsBool;
var
  ResNames: tsEnum;
begin
  ResNames := 0;

  if (Names and TS_INIT_TEXTSUITE) > 0 then
    if TextSuite_initialized then
      ResNames := ResNames or TS_INIT_TEXTSUITE;

  if (Names and TS_INIT_SDL_TTF) > 0 then
    if SDL_TTF_initialized then
      ResNames := ResNames or TS_INIT_SDL_TTF;

  if (Names and TS_INIT_SDL_IMAGE) > 0 then
    if SDL_IMAGE_initialized then
      ResNames := ResNames or TS_INIT_SDL_IMAGE;

  if (Names and TS_INIT_GDI) > 0 then
    if GDI_initialized then
      ResNames := ResNames or TS_INIT_GDI;

  if (Names and TS_INIT_OPENGL) > 0 then
    if OpenGL_initialized then
      ResNames := ResNames or TS_INIT_OPENGL;

  if ResNames = Names then
    Result := TS_TRUE
  else
    Result := TS_FALSE;
end;


procedure tsQuit;
begin
  try
    // maybe quit library if count is zero 
    if TextSuite_initialized then
      Quit_TextSuite;

    // if count is zero then quit all other systems
    if TextSuite_initialized_count = 0 then begin
      if SDL_TTF_initialized then
        Quit_SDL_TTF;

      if SDL_IMAGE_initialized then
        Quit_SDL_IMAGE;

      if GDI_initialized then
        Quit_GDI;

      if OpenGL_initialized then
        Quit_OpenGL;

      Quit_SDL;
    end;
  except
    SetError(TS_ERROR, TS_FUNC_QUIT);
  end;
end;


function tsGetError: tsEnum;
begin
  Result := gError;

  gError := TS_NO_ERROR;
end;


function tsGetErrorFunction: tsEnum;
begin
  Result := gErrorFunction;

  gErrorFunction := TS_NO_FUNC;
end;


function tsGetErrorStringA(ErrorCode: tsEnum): pAnsiChar;
begin
  case ErrorCode of
    TS_NO_ERROR:
      Result := pAnsiChar(TS_NO_ERROR_STR);
    TS_ERROR:
      Result := pAnsiChar(TS_ERROR_STR);
    TS_NO_ACTIVE_CONTEXT:
      Result := pAnsiChar(TS_NO_ACTIVE_CONTEXT_STR);
    TS_NO_ACTIVE_RENDERER:
      Result := pAnsiChar(TS_NO_ACTIVE_RENDERER_STR);
    TS_NO_ACTIVE_FONT:
      Result := pAnsiChar(TS_NO_ACTIVE_FONT_STR);
    TS_INVALID_OPERATION:
      Result := pAnsiChar(TS_INVALID_OPERATION_STR);
    TS_INVALID_ENUM:
      Result := pAnsiChar(TS_INVALID_ENUM_STR);
    TS_INVALID_VALUE:
      Result := pAnsiChar(TS_INVALID_VALUE_STR);
    TS_OUT_OF_MEMORY:
      Result := pAnsiChar(TS_OUT_OF_MEMORY_STR);
    TS_NOT_INITIALIZED:
      Result := pAnsiChar(TS_NOT_INITIALIZED_STR);
  else
    if ErrorCode and TS_ERROR = TS_ERROR then
      Result := pAnsiChar(TS_ERROR_STR)
    else
      Result := pAnsiChar(TS_NO_ERROR);
  end;
end;


procedure SetCodePage(CodePage: tsEnum; CodePageFunc: TtsAnsiToWideCharFunc; pCodePage: Pointer);
var
  Context: TtsContext;
begin
  Context := gContext;

  Context.gCodePage     := CodePage;
  Context.gCodePageFunc := CodePageFunc;
  Context.gCodePagePtr  := pCodePage;
end;


procedure tsSetParameteri(ParamName: tsEnum; Param: tsInt);
begin
  tsSetParameteriv(ParamName, @Param);
end;


procedure tsSetParameteriv(ParamName: tsEnum; pParam: ptsInt);
var
  Context: TtsContext;
begin
  Context := gContext;

  if Context <> nil then begin
    if pParam <> nil then begin
      case ParamName of
        // font
        TS_FONT_ASCENT:
          if not Context.IsLocked then begin
            if Context.ActiveFont <> nil then begin
              Context.ActiveFont.Ascent := pParam^;
            end else
              SetError(TS_NO_ACTIVE_FONT, TS_FUNC_SET_PARAMETER);
          end else
            SetError(TS_INVALID_OPERATION, TS_FUNC_SET_PARAMETER);

        TS_FONT_DESCENT:
          if not Context.IsLocked then begin
            if Context.ActiveFont <> nil then begin
              Context.ActiveFont.Descent := pParam^;
            end else
              SetError(TS_NO_ACTIVE_FONT, TS_FUNC_SET_PARAMETER);
          end else
            SetError(TS_INVALID_OPERATION, TS_FUNC_SET_PARAMETER);

        TS_FONT_EXTERNAL_LEADING:
          if not Context.IsLocked then begin
            if Context.ActiveFont <> nil then begin
              Context.ActiveFont.ExternalLeading := pParam^;
            end else
              SetError(TS_NO_ACTIVE_FONT, TS_FUNC_SET_PARAMETER);
          end else
            SetError(TS_INVALID_OPERATION, TS_FUNC_SET_PARAMETER);

        TS_FONT_BASELINE_OFFSET:
          if not Context.IsLocked then begin
            if Context.ActiveFont <> nil then begin
              Context.ActiveFont.BaselineOffset := pParam^;
            end else
              SetError(TS_NO_ACTIVE_FONT, TS_FUNC_SET_PARAMETER);
          end else
            SetError(TS_INVALID_OPERATION, TS_FUNC_SET_PARAMETER);

        TS_FONT_CHAR_SPACING:
          if not Context.IsLocked then begin
            if Context.ActiveFont <> nil then begin
              Context.ActiveFont.CharSpacing := pParam^;
            end else
              SetError(TS_NO_ACTIVE_FONT, TS_FUNC_SET_PARAMETER);
          end else
            SetError(TS_INVALID_OPERATION, TS_FUNC_SET_PARAMETER);

        TS_FONT_LINE_SPACING:
          if not Context.IsLocked then begin
            if Context.ActiveFont <> nil then begin
              Context.ActiveFont.LineSpacing := pParam^;
            end else
              SetError(TS_NO_ACTIVE_FONT, TS_FUNC_SET_PARAMETER);
          end else
            SetError(TS_INVALID_OPERATION, TS_FUNC_SET_PARAMETER);

        TS_FONT_UNDERLINE_POSITION:
          if not Context.IsLocked then begin
            if Context.ActiveFont <> nil then begin
              Context.ActiveFont.UnderlinePosition := pParam^;
            end else
              SetError(TS_NO_ACTIVE_FONT, TS_FUNC_SET_PARAMETER);
          end else
            SetError(TS_INVALID_OPERATION, TS_FUNC_SET_PARAMETER);

        TS_FONT_UNDERLINE_SIZE:
          if not Context.IsLocked then begin
            if Context.ActiveFont <> nil then begin
              Context.ActiveFont.UnderlineSize := pParam^;
            end else
              SetError(TS_NO_ACTIVE_FONT, TS_FUNC_SET_PARAMETER);
          end else
            SetError(TS_INVALID_OPERATION, TS_FUNC_SET_PARAMETER);

        TS_FONT_STRIKEOUT_POSITION:
          if not Context.IsLocked then begin
            if Context.ActiveFont <> nil then begin
              Context.ActiveFont.StrikeoutPosition := pParam^;
            end else
              SetError(TS_NO_ACTIVE_FONT, TS_FUNC_SET_PARAMETER);
          end else
            SetError(TS_INVALID_OPERATION, TS_FUNC_SET_PARAMETER);

        TS_FONT_STRIKEOUT_SIZE:
          if not Context.IsLocked then begin
            if Context.ActiveFont <> nil then begin
              Context.ActiveFont.StrikeoutSize := pParam^;
            end else
              SetError(TS_NO_ACTIVE_FONT, TS_FUNC_SET_PARAMETER);
          end else
            SetError(TS_INVALID_OPERATION, TS_FUNC_SET_PARAMETER);

        // globals
        TS_GLOBAL_ANTIALIASING:
          case pParam^ of
            TS_ANTIALIASING_NONE:
              Context.gGlobalAntiAliasing := TS_ANTIALIASING_NONE;
            TS_ANTIALIASING_NORMAL:
              Context.gGlobalAntiAliasing := TS_ANTIALIASING_NORMAL;
          else
            SetError(TS_INVALID_VALUE, TS_FUNC_SET_PARAMETER);
          end;

        TS_GLOBAL_FORMAT:
          case pParam^ of
            TS_FORMAT_RGBA8:
              Context.gGlobalFormat := TS_FORMAT_RGBA8;
          else
            SetError(TS_INVALID_VALUE, TS_FUNC_SET_PARAMETER);
          end;

        // creator
        TS_CREATOR:
          case pParam^ of
            TS_CREATOR_SDL:
              Context.gCreator := TS_CREATOR_SDL;
            TS_CREATOR_GDI:
              Context.gCreator := TS_CREATOR_GDI;
            TS_CREATOR_GDI_FACENAME:
              Context.gCreator := TS_CREATOR_GDI_FACENAME;
            TS_CREATOR_GDI_STREAM:
              Context.gCreator := TS_CREATOR_GDI_STREAM;
          else
            SetError(TS_INVALID_VALUE, TS_FUNC_SET_PARAMETER);
          end;

        TS_CREATOR_CREATE_CHARS:
          if Context.ActiveFont <> nil then begin
            if Context.ActiveFont is TtsFontCreator then begin
              case pParam^ of
                TS_TRUE:
                  TtsFontCreator(Context.ActiveFont).CreateChars := True;
                TS_FALSE:
                  TtsFontCreator(Context.ActiveFont).CreateChars := False;
              else
                SetError(TS_INVALID_VALUE, TS_FUNC_SET_PARAMETER);
              end;
            end else
              SetError(TS_INVALID_OPERATION, TS_FUNC_SET_PARAMETER);
          end else
            SetError(TS_NO_ACTIVE_FONT, TS_FUNC_SET_PARAMETER);

        TS_CREATOR_ADD_RESIZING_BORDER:
          if Context.ActiveFont <> nil then begin
            if Context.ActiveFont is TtsFontCreator then begin
              case pParam^ of
                TS_TRUE:
                  TtsFontCreator(Context.ActiveFont).AddResizingBorder := True;
                TS_FALSE:
                  TtsFontCreator(Context.ActiveFont).AddResizingBorder := False;
              else
                SetError(TS_INVALID_VALUE, TS_FUNC_SET_PARAMETER);
              end;
            end else
              SetError(TS_INVALID_OPERATION, TS_FUNC_SET_PARAMETER);
          end else
            SetError(TS_NO_ACTIVE_FONT, TS_FUNC_SET_PARAMETER);

        // renderer
        TS_RENDERER:
          if not Context.IsLocked then begin
            if tsGetParameteri(TS_RENDERER) <> pParam^ then begin
              if (Context.FontCount = 0) and (Context.ImageCount = 0) then begin
                case pParam^ of
                  TS_RENDERER_NULL:
                    begin
                      if Context.Renderer <> nil then
                        Context.Renderer.Free;

                      Context.Renderer := TtsRendererNULL.Create(Context);
                    end;
                  TS_RENDERER_OPENGL:
                    begin
                      if OpenGL_initialized then begin
                        if Context.Renderer <> nil then
                          Context.Renderer.Free;

                        Context.Renderer := TtsRendererOpenGL.Create(Context);
                      end else
                        SetError(TS_NOT_INITIALIZED, TS_FUNC_SET_PARAMETER);
                    end
                else
                  SetError(TS_INVALID_VALUE, TS_FUNC_SET_PARAMETER);
                end;
              end else
                // Renderer only can set if no Images/Fonts exist
                SetError(TS_INVALID_OPERATION, TS_FUNC_SET_PARAMETER);
            end;
          end else
            SetError(TS_INVALID_OPERATION, TS_FUNC_SET_PARAMETER);

        // opengl texture size
        TS_RENDERER_OPENGL_TEXTURE_SIZE:
          if Context.Renderer <> nil then begin
            if Context.Renderer is TtsRendererOpenGL then begin
              case pParam^ of
                TS_RENDERER_OPENGL_TEXTURE_SIZE_128:
                  TtsRendererOpenGL(Context.Renderer).TextureSize := 128;

                TS_RENDERER_OPENGL_TEXTURE_SIZE_256:
                  TtsRendererOpenGL(Context.Renderer).TextureSize := 256;

                TS_RENDERER_OPENGL_TEXTURE_SIZE_512:
                  TtsRendererOpenGL(Context.Renderer).TextureSize := 512;

                TS_RENDERER_OPENGL_TEXTURE_SIZE_1024:
                  TtsRendererOpenGL(Context.Renderer).TextureSize := 1024;
              else
                SetError(TS_INVALID_VALUE, TS_FUNC_SET_PARAMETER);
              end;
            end else
              SetError(TS_INVALID_OPERATION, TS_FUNC_SET_PARAMETER);
          end else
            SetError(TS_NO_ACTIVE_RENDERER, TS_FUNC_SET_PARAMETER);

        // null renderer saves char images
        TS_RENDERER_NULL_SAVE_IMAGES:
          if Context.Renderer <> nil then begin
            if Context.Renderer is TtsRendererNULL then begin
              case pParam^ of
                TS_FALSE:
                  Context.Renderer.SaveImages := False;

                TS_TRUE:
                  Context.Renderer.SaveImages := True;
              else
                SetError(TS_INVALID_VALUE, TS_FUNC_SET_PARAMETER);
              end;
            end else
              SetError(TS_INVALID_OPERATION, TS_FUNC_SET_PARAMETER);
          end else
            SetError(TS_NO_ACTIVE_RENDERER, TS_FUNC_SET_PARAMETER);

        // Align
        TS_ALIGN:
          if not Context.IsLocked then begin
            case pParam^ of
              TS_ALIGN_LEFT:
                Context.gAlign := TS_ALIGN_LEFT;
              TS_ALIGN_CENTER:
                Context.gAlign := TS_ALIGN_CENTER;
              TS_ALIGN_RIGHT:
                Context.gAlign := TS_ALIGN_RIGHT;
              TS_ALIGN_BLOCK:
                Context.gAlign := TS_ALIGN_BLOCK;
            else
              SetError(TS_INVALID_VALUE, TS_FUNC_SET_PARAMETER);
            end;
          end else
            SetError(TS_INVALID_OPERATION, TS_FUNC_SET_PARAMETER);

        // Vertical Align
        TS_VALIGN:
          if not Context.IsLocked then begin
            case pParam^ of
              TS_VALIGN_TOP:
                Context.gVAlign := TS_VALIGN_TOP;
              TS_VALIGN_CENTER:
                Context.gVAlign := TS_VALIGN_CENTER;
              TS_VALIGN_BOTTOM:
                Context.gVAlign := TS_VALIGN_BOTTOM;
            else
              SetError(TS_INVALID_VALUE, TS_FUNC_SET_PARAMETER);
            end;
          end else
            SetError(TS_INVALID_OPERATION, TS_FUNC_SET_PARAMETER);

        // Clipping
        TS_CLIP:
          if not Context.IsLocked then begin
            case pParam^ of
              TS_CLIP_COMPLETE:
                Context.gClip := TS_CLIP_COMPLETE;
              TS_CLIP_BORDER:
                Context.gClip := TS_CLIP_BORDER;
            else
              SetError(TS_INVALID_VALUE, TS_FUNC_SET_PARAMETER);
            end;
          end else
            SetError(TS_INVALID_OPERATION, TS_FUNC_SET_PARAMETER);

        // block offset
        TS_BLOCK_OFFSET_X:
          if not Context.IsLocked then begin
            Context.gBlockOffsetX := pParam^;
          end else
            SetError(TS_INVALID_OPERATION, TS_FUNC_SET_PARAMETER);

        TS_BLOCK_OFFSET_Y:
          if not Context.IsLocked then begin
            Context.gBlockOffsetY := pParam^;
          end else
            SetError(TS_INVALID_OPERATION, TS_FUNC_SET_PARAMETER);

        // single line
        TS_SINGLE_LINE:
          case pParam^ of
            TS_SINGLE_LINE_TOP:
              Context.gSingleLine := TS_SINGLE_LINE_TOP;
            TS_SINGLE_LINE_BASELINE:
              Context.gSingleLine := TS_SINGLE_LINE_BASELINE;
          else
            SetError(TS_INVALID_VALUE, TS_FUNC_SET_PARAMETER);
          end;

  {
        TS_TAB:
          case pParam^ of
            TS_TAB_FIXED:
              gTab := TS_TAB_FIXED;
            TS_TAB_ABSOLUTE:
              gTab := TS_TAB_ABSOLUTE;
          else
            SetError(TS_INVALID_VALUE, TS_FUNC_SET_PARAMETER);
          end;

        TS_TAB_FIXED_WIDTH:
          gTabWidth := pParam^;
  }
        // image modes
        TS_IMAGE_RED_MODE:
          case pParam^ of
            TS_MODE_REPLACE:
              Context.gImageMode[tsModeRed] := TS_MODE_REPLACE;
            TS_MODE_MODULATE:
              Context.gImageMode[tsModeRed] := TS_MODE_MODULATE;
          else
            SetError(TS_INVALID_VALUE, TS_FUNC_SET_PARAMETER);
          end;

        TS_IMAGE_GREEN_MODE:
          case pParam^ of
            TS_MODE_REPLACE:
              Context.gImageMode[tsModeGreen] := TS_MODE_REPLACE;
            TS_MODE_MODULATE:
              Context.gImageMode[tsModeGreen] := TS_MODE_MODULATE;
          else
            SetError(TS_INVALID_VALUE, TS_FUNC_SET_PARAMETER);
          end;

        TS_IMAGE_BLUE_MODE:
          case pParam^ of
            TS_MODE_REPLACE:
              Context.gImageMode[tsModeBlue] := TS_MODE_REPLACE;
            TS_MODE_MODULATE:
              Context.gImageMode[tsModeBlue] := TS_MODE_MODULATE;
          else
            SetError(TS_INVALID_VALUE, TS_FUNC_SET_PARAMETER);
          end;

        TS_IMAGE_ALPHA_MODE:
          case pParam^ of
            TS_MODE_REPLACE:
              Context.gImageMode[tsModeAlpha] := TS_MODE_REPLACE;
            TS_MODE_MODULATE:
              Context.gImageMode[tsModeAlpha] := TS_MODE_MODULATE;
          else
            SetError(TS_INVALID_VALUE, TS_FUNC_SET_PARAMETER);
          end;

        TS_IMAGE_LUMINANCE_MODE:
          case pParam^ of
            TS_MODE_REPLACE:
              Context.gImageMode[tsModeLuminance] := TS_MODE_REPLACE;
            TS_MODE_MODULATE:
              Context.gImageMode[tsModeLuminance] := TS_MODE_MODULATE;
          else
            SetError(TS_INVALID_VALUE, TS_FUNC_SET_PARAMETER);
          end;

        TS_IMAGE_LIBRARY:
          case pParam^ of
            TS_IMAGE_LIBRARY_SDL:
              Context.gImageLibrary := TS_IMAGE_LIBRARY_SDL;
          else
            SetError(TS_INVALID_VALUE, TS_FUNC_SET_PARAMETER);
          end;

        // code pages
        TS_EMPTY_CP_ENTRY:
          case pParam^ of
            TS_EMPTY_CP_ENTRY_IGNORE:
              Context.gEmptyCodePageEntry := TS_EMPTY_CP_ENTRY_IGNORE;
            TS_EMPTY_CP_ENTRY_USE_DEFAULT:
              Context.gEmptyCodePageEntry := TS_EMPTY_CP_ENTRY_USE_DEFAULT;
          else
            SetError(TS_INVALID_VALUE, TS_FUNC_SET_PARAMETER);
          end;

        TS_CODEPAGE:
          case pParam^ of
            TS_CODEPAGE_UTF8:
              SetCodePage(TS_CODEPAGE_UTF8, nil, nil);
            TS_CODEPAGE_8859_1:
              SetCodePage(TS_CODEPAGE_8859_1, nil, nil);
            TS_CODEPAGE_8859_2:
              SetCodePage(TS_CODEPAGE_8859_2, tsAnsiSBCDToWide, @CP_8859_2);
            TS_CODEPAGE_8859_3:
              SetCodePage(TS_CODEPAGE_8859_3, tsAnsiSBCDToWide, @CP_8859_3);
            TS_CODEPAGE_8859_4:
              SetCodePage(TS_CODEPAGE_8859_4, tsAnsiSBCDToWide, @CP_8859_4);
            TS_CODEPAGE_8859_5:
              SetCodePage(TS_CODEPAGE_8859_5, tsAnsiSBCDToWide, @CP_8859_5);
            TS_CODEPAGE_8859_6:
              SetCodePage(TS_CODEPAGE_8859_6, tsAnsiSBCDToWide, @CP_8859_6);
            TS_CODEPAGE_8859_7:
              SetCodePage(TS_CODEPAGE_8859_7, tsAnsiSBCDToWide, @CP_8859_7);
            TS_CODEPAGE_8859_8:
              SetCodePage(TS_CODEPAGE_8859_8, tsAnsiSBCDToWide, @CP_8859_8);
            TS_CODEPAGE_8859_9:
              SetCodePage(TS_CODEPAGE_8859_9, tsAnsiSBCDToWide, @CP_8859_9);
            TS_CODEPAGE_8859_10:
              SetCodePage(TS_CODEPAGE_8859_10, tsAnsiSBCDToWide, @CP_8859_10);
            TS_CODEPAGE_8859_11:
              SetCodePage(TS_CODEPAGE_8859_11, tsAnsiSBCDToWide, @CP_8859_11);
            TS_CODEPAGE_8859_13:
              SetCodePage(TS_CODEPAGE_8859_13, tsAnsiSBCDToWide, @CP_8859_13);
            TS_CODEPAGE_8859_14:
              SetCodePage(TS_CODEPAGE_8859_14, tsAnsiSBCDToWide, @CP_8859_14);
            TS_CODEPAGE_8859_15:
              SetCodePage(TS_CODEPAGE_8859_15, tsAnsiSBCDToWide, @CP_8859_15);
            TS_CODEPAGE_8859_16:
              SetCodePage(TS_CODEPAGE_8859_16, tsAnsiSBCDToWide, @CP_8859_16);
            TS_CODEPAGE_037:
              SetCodePage(TS_CODEPAGE_037, tsAnsiSBCDToWide, @CP_037);
            TS_CODEPAGE_437:
              SetCodePage(TS_CODEPAGE_437, tsAnsiSBCDToWide, @CP_437);
            TS_CODEPAGE_500:
              SetCodePage(TS_CODEPAGE_500, tsAnsiSBCDToWide, @CP_500);
            TS_CODEPAGE_737:
              SetCodePage(TS_CODEPAGE_737, tsAnsiSBCDToWide, @CP_737);
            TS_CODEPAGE_775:
              SetCodePage(TS_CODEPAGE_775, tsAnsiSBCDToWide, @CP_775);
            TS_CODEPAGE_850:
              SetCodePage(TS_CODEPAGE_850, tsAnsiSBCDToWide, @CP_850);
            TS_CODEPAGE_852:
              SetCodePage(TS_CODEPAGE_852, tsAnsiSBCDToWide, @CP_852);
            TS_CODEPAGE_855:
              SetCodePage(TS_CODEPAGE_855, tsAnsiSBCDToWide, @CP_855);
            TS_CODEPAGE_857:
              SetCodePage(TS_CODEPAGE_857, tsAnsiSBCDToWide, @CP_857);
            TS_CODEPAGE_860:
              SetCodePage(TS_CODEPAGE_860, tsAnsiSBCDToWide, @CP_860);
            TS_CODEPAGE_861:
              SetCodePage(TS_CODEPAGE_861, tsAnsiSBCDToWide, @CP_861);
            TS_CODEPAGE_862:
              SetCodePage(TS_CODEPAGE_862, tsAnsiSBCDToWide, @CP_862);
            TS_CODEPAGE_863:
              SetCodePage(TS_CODEPAGE_863, tsAnsiSBCDToWide, @CP_863);
            TS_CODEPAGE_864:
              SetCodePage(TS_CODEPAGE_864, tsAnsiSBCDToWide, @CP_864);
            TS_CODEPAGE_865:
              SetCodePage(TS_CODEPAGE_865, tsAnsiSBCDToWide, @CP_865);
            TS_CODEPAGE_866:
              SetCodePage(TS_CODEPAGE_866, tsAnsiSBCDToWide, @CP_866);
            TS_CODEPAGE_869:
              SetCodePage(TS_CODEPAGE_869, tsAnsiSBCDToWide, @CP_869);
            TS_CODEPAGE_874:
              SetCodePage(TS_CODEPAGE_874, tsAnsiSBCDToWide, @CP_874);
            TS_CODEPAGE_875:
              SetCodePage(TS_CODEPAGE_875, tsAnsiSBCDToWide, @CP_875);
            TS_CODEPAGE_1026:
              SetCodePage(TS_CODEPAGE_1026, tsAnsiSBCDToWide, @CP_1026);
            TS_CODEPAGE_1250:
              SetCodePage(TS_CODEPAGE_1250, tsAnsiSBCDToWide, @CP_1250);
            TS_CODEPAGE_1251:
              SetCodePage(TS_CODEPAGE_1251, tsAnsiSBCDToWide, @CP_1251);
            TS_CODEPAGE_1252:
              SetCodePage(TS_CODEPAGE_1252, tsAnsiSBCDToWide, @CP_1252);
            TS_CODEPAGE_1253:
              SetCodePage(TS_CODEPAGE_1253, tsAnsiSBCDToWide, @CP_1253);
            TS_CODEPAGE_1254:
              SetCodePage(TS_CODEPAGE_1254, tsAnsiSBCDToWide, @CP_1254);
            TS_CODEPAGE_1255:
              SetCodePage(TS_CODEPAGE_1255, tsAnsiSBCDToWide, @CP_1255);
            TS_CODEPAGE_1256:
              SetCodePage(TS_CODEPAGE_1256, tsAnsiSBCDToWide, @CP_1256);
            TS_CODEPAGE_1257:
              SetCodePage(TS_CODEPAGE_1257, tsAnsiSBCDToWide, @CP_1257);
            TS_CODEPAGE_1258:
              SetCodePage(TS_CODEPAGE_1258, tsAnsiSBCDToWide, @CP_1258);
          else
            SetError(TS_INVALID_VALUE, TS_FUNC_SET_PARAMETER);
          end;

        TS_DEBUG_DRAW_CHAR_RECTS:
          case pParam^ of
            TS_TRUE:
              Context.gDebugDrawCharRects := True;
            TS_FALSE:
              Context.gDebugDrawCharRects := False;
          else
            SetError(TS_INVALID_VALUE, TS_FUNC_SET_PARAMETER);
          end;
      else
        SetError(TS_INVALID_ENUM, TS_FUNC_SET_PARAMETER);
      end;
    end else
      SetError(TS_INVALID_VALUE, TS_FUNC_SET_PARAMETER);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_SET_PARAMETER);
end;


function tsGetParameteri(ParamName: tsEnum): tsInt;
begin
  tsGetParameteriv(ParamName, @Result);
end;


procedure tsGetParameteriv(ParamName: tsEnum; pParam: ptsInt);
var
  Context: TtsContext;
begin
  Context := gContext;

  if pParam <> nil then begin
    pParam^ := 0;

    case ParamName of
      TS_INFO_MAYOR_VERSION:
        pParam^ := TS_MAYOR_VERSION;

      TS_INFO_MINOR_VERSION:
        pParam^ := TS_MINOR_VERSION;

      TS_INFO_BUILD_NUMBER:
        pParam^ := TS_BUILD_NUMBER;

      TS_CONTEXT_BINDING:
        if Context <> nil then begin
          pParam^ := Context.ContextID;
        end;

      // font
      TS_FONT_STYLE:
        if Context <> nil then begin
          if Context.ActiveFont <> nil then begin
            pParam^ := TS_STYLE_NORMAL;

            with Context.ActiveFont do begin
              if tsStyleBold in Style then
                pParam^ := pParam^ or TS_STYLE_BOLD;

              if tsStyleItalic in Style then
                pParam^ := pParam^ or TS_STYLE_ITALIC;

              if tsStyleUnderline  in Style then
                pParam^ := pParam^ or TS_STYLE_UNDERLINE;

              if tsStyleStrikeout  in Style then
                pParam^ := pParam^ or TS_STYLE_STRIKEOUT;
            end;
          end else
            SetError(TS_NO_ACTIVE_FONT, TS_FUNC_GET_PARAMETER);
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      TS_FONT_FILE_STYLE:
        if Context <> nil then begin
          if Context.ActiveFont <> nil then begin
            pParam^ := Context.ActiveFont.FontFileStyle;
          end else
            SetError(TS_NO_ACTIVE_FONT, TS_FUNC_GET_PARAMETER);
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      TS_FONT_FIXED_WIDTH:
        if Context <> nil then begin
          if Context.ActiveFont <> nil then begin
            if Context.ActiveFont.FixedWidth then
              pParam^ := TS_TRUE
            else
              pParam^ := TS_FALSE;
          end else
            SetError(TS_NO_ACTIVE_FONT, TS_FUNC_GET_PARAMETER);
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      TS_FONT_ASCENT:
        if Context <> nil then begin
          if Context.ActiveFont <> nil then begin
            pParam^ := Context.ActiveFont.Ascent;
          end else
            SetError(TS_NO_ACTIVE_FONT, TS_FUNC_GET_PARAMETER);
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      TS_FONT_DESCENT:
        if Context <> nil then begin
          if Context.ActiveFont <> nil then begin
            pParam^ := Context.ActiveFont.Descent;
          end else
            SetError(TS_NO_ACTIVE_FONT, TS_FUNC_GET_PARAMETER);
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      TS_FONT_EXTERNAL_LEADING:
        if Context <> nil then begin
          if Context.ActiveFont <> nil then begin
            pParam^ := Context.ActiveFont.ExternalLeading;
          end else
            SetError(TS_NO_ACTIVE_FONT, TS_FUNC_GET_PARAMETER);
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      TS_FONT_BASELINE_OFFSET:
        if Context <> nil then begin
          if Context.ActiveFont <> nil then begin
            pParam^ := Context.ActiveFont.BaselineOffset;
          end else
            SetError(TS_NO_ACTIVE_FONT, TS_FUNC_GET_PARAMETER);
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      TS_FONT_LINESKIP:
        if Context <> nil then begin
          if Context.ActiveFont <> nil then begin
            with Context.ActiveFont do
              pParam^ := Ascent + Descent + ExternalLeading;
          end else
            SetError(TS_NO_ACTIVE_FONT, TS_FUNC_GET_PARAMETER);
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      TS_FONT_ANTIALIASING:
        if Context <> nil then begin
          if Context.ActiveFont <> nil then begin
            case Context.ActiveFont.AntiAliasing of
              tsAANone:
                pParam^ := TS_ANTIALIASING_NONE;
              tsAANormal:
                pParam^ := TS_ANTIALIASING_NORMAL;
            end;
          end else
            SetError(TS_NO_ACTIVE_FONT, TS_FUNC_GET_PARAMETER);
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      TS_FONT_FORMAT:
        if Context <> nil then begin
          if Context.ActiveFont <> nil then begin
            case Context.ActiveFont.Format of
              tsFormatEmpty:
                pParam^ := TS_FORMAT_EMPTY;
              tsFormatRGBA8:
                pParam^ := TS_FORMAT_RGBA8;
            end;
          end else
            SetError(TS_NO_ACTIVE_FONT, TS_FUNC_GET_PARAMETER);
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      TS_FONT_CHAR_SPACING:
        if Context <> nil then begin
          if Context.ActiveFont <> nil then begin
            pParam^ := Context.ActiveFont.CharSpacing;
          end else
            SetError(TS_NO_ACTIVE_FONT, TS_FUNC_GET_PARAMETER);
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      TS_FONT_LINE_SPACING:
        if Context <> nil then begin
          if Context.ActiveFont <> nil then begin
            pParam^ := Context.ActiveFont.LineSpacing;
          end else
            SetError(TS_NO_ACTIVE_FONT, TS_FUNC_GET_PARAMETER);
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      TS_FONT_UNDERLINE_POSITION:
        if Context <> nil then begin
          if Context.ActiveFont <> nil then begin
            pParam^ := Context.ActiveFont.UnderlinePosition;
          end else
            SetError(TS_NO_ACTIVE_FONT, TS_FUNC_GET_PARAMETER);
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      TS_FONT_UNDERLINE_SIZE:
        if Context <> nil then begin
          if Context.ActiveFont <> nil then begin
            pParam^ := Context.ActiveFont.UnderlineSize;
          end else
            SetError(TS_NO_ACTIVE_FONT, TS_FUNC_GET_PARAMETER);
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      TS_FONT_STRIKEOUT_POSITION:
        if Context <> nil then begin
          if Context.ActiveFont <> nil then begin
            pParam^ := Context.ActiveFont.StrikeoutPosition;
          end else
            SetError(TS_NO_ACTIVE_FONT, TS_FUNC_GET_PARAMETER);
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      TS_FONT_STRIKEOUT_SIZE:
        if Context <> nil then begin
          if Context.ActiveFont <> nil then begin
            pParam^ := Context.ActiveFont.StrikeoutSize;
          end else
            SetError(TS_NO_ACTIVE_FONT, TS_FUNC_GET_PARAMETER);
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      // globals
      TS_GLOBAL_ANTIALIASING:
        if Context <> nil then begin
          pParam^ := Context.gGlobalAntiAliasing;
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      TS_GLOBAL_FORMAT:
        if Context <> nil then begin
          pParam^ := Context.gGlobalFormat;
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      // creator
      TS_CREATOR:
        if Context <> nil then begin
          pParam^ := Context.gCreator;
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      TS_CREATOR_CREATE_CHARS:
        if Context <> nil then begin
          if Context.ActiveFont <> nil then begin
            if Context.ActiveFont is TtsFontCreator then begin
              if TtsFontCreator(Context.ActiveFont).CreateChars then
                pParam^ := TS_TRUE
              else
                pParam^ := TS_FALSE;
            end else
              SetError(TS_INVALID_OPERATION, TS_FUNC_GET_PARAMETER);
          end else
            SetError(TS_NO_ACTIVE_FONT, TS_FUNC_GET_PARAMETER);
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      TS_CREATOR_ADD_RESIZING_BORDER:
        if Context <> nil then begin
          if Context.ActiveFont <> nil then begin
            if Context.ActiveFont is TtsFontCreator then begin
              if TtsFontCreator(Context.ActiveFont).AddResizingBorder then
                pParam^ := TS_TRUE
              else
                pParam^ := TS_FALSE;
            end else
              SetError(TS_INVALID_OPERATION, TS_FUNC_GET_PARAMETER);
          end else
            SetError(TS_NO_ACTIVE_FONT, TS_FUNC_GET_PARAMETER);
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      // Renderer
      TS_RENDERER:
        if Context <> nil then begin
          if Context.Renderer <> nil then begin
            if Context.Renderer is TtsRendererNULL then begin
              pParam^ := TS_RENDERER_NULL;
            end else
            if Context.Renderer is TtsRendererOpenGL then begin
              pParam^ := TS_RENDERER_OPENGL;
            end else
              SetError(TS_ERROR, TS_FUNC_GET_PARAMETER);
          end;
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      // opengl texture size
      TS_RENDERER_OPENGL_TEXTURE_SIZE:
        if Context <> nil then begin
          if Context.Renderer <> nil then begin
            if Context.Renderer is TtsRendererOpenGL then begin
              pParam^ := TtsRendererOpenGL(Context.Renderer).TextureSize;
            end else
              SetError(TS_INVALID_OPERATION, TS_FUNC_GET_PARAMETER);
          end else
            SetError(TS_NO_ACTIVE_RENDERER, TS_FUNC_GET_PARAMETER);
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      // null renderer must save images
      TS_RENDERER_NULL_SAVE_IMAGES:
        if Context <> nil then begin
          if Context.Renderer <> nil then begin
            if Context.Renderer is TtsRendererNULL then begin
              case Context.Renderer.SaveImages of
                True:
                  pParam^ := TS_TRUE;
                False:
                  pParam^ := TS_FALSE;
              end;
            end else
              SetError(TS_INVALID_OPERATION, TS_FUNC_GET_PARAMETER);
          end else
            SetError(TS_NO_ACTIVE_RENDERER, TS_FUNC_GET_PARAMETER);
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      TS_ALIGN:
        if Context <> nil then begin
          pParam^ := Context.gAlign;
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      TS_VALIGN:
        if Context <> nil then begin
          pParam^ := Context.gVAlign;
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      TS_CLIP:
        if Context <> nil then begin
          pParam^ := Context.gClip;
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      TS_BLOCK_OFFSET_X:
        if Context <> nil then begin
          pParam^ := Context.gBlockOffsetX;
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      TS_BLOCK_OFFSET_Y:
        if Context <> nil then begin
          pParam^ := Context.gBlockOffsetY;
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

{
      TS_TAB:
        pParam^ := gTab;

      TS_TAB_FIXED_WIDTH:
        pParam^ := gTabWidth;
}

      TS_SINGLE_LINE:
        if Context <> nil then begin
          pParam^ := Context.gSingleLine;
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      TS_IMAGE_RED_MODE:
        if Context <> nil then begin
          pParam^ := Context.gImageMode[tsModeRed];
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      TS_IMAGE_GREEN_MODE:
        if Context <> nil then begin
          pParam^ := Context.gImageMode[tsModeGreen];
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      TS_IMAGE_BLUE_MODE:
        if Context <> nil then begin
          pParam^ := Context.gImageMode[tsModeBlue];
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      TS_IMAGE_ALPHA_MODE:
        if Context <> nil then begin
          pParam^ := Context.gImageMode[tsModeAlpha];
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      TS_IMAGE_LUMINANCE_MODE:
        if Context <> nil then begin
          pParam^ := Context.gImageMode[tsModeLuminance];
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      TS_IMAGE_LIBRARY:
        if Context <> nil then begin
          pParam^ := Context.gImageLibrary;
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      TS_EMPTY_CP_ENTRY:
        if Context <> nil then begin
          pParam^ := Context.gEmptyCodePageEntry;
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      TS_CODEPAGE:
        if Context <> nil then begin
          pParam^ := Context.gCodePage;
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      TS_DEBUG_DRAW_CHAR_RECTS:
        if Context <> nil then begin
          case Context.gDebugDrawCharRects of
            True:
              pParam^ := TS_TRUE;
            False:
              pParam^ := TS_FALSE;
          end;
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);

      TS_FONT_BINDING:
        if Context <> nil then begin
          if Context.Renderer <> nil then begin
            pParam^ := Context.Renderer.ActiveFontID;
          end
            else SetError(TS_NO_ACTIVE_RENDERER, TS_FUNC_GET_PARAMETER);
        end else
          SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_PARAMETER);
    else
      SetError(TS_INVALID_ENUM, TS_FUNC_GET_PARAMETER);
    end;
  end else
    SetError(TS_INVALID_VALUE, TS_FUNC_GET_PARAMETER);
end;


function tsGetStringA(ParamName: tsEnum): pAnsiChar; 
var
  Context: TtsContext; 
begin
  Result := nil;

  Context := gContext;

  case ParamName of
    TS_INFO_VERSION:
      if TS_VERSION_STR <> '' then
        Result := pAnsiChar(TS_VERSION_STR);

    TS_INFO_COPYRIGHT:
      if TS_COPYRIGHT_STR <> '' then
        Result := pAnsiChar(TS_COPYRIGHT_STR);

    TS_FONT_COPYRIGHT:
      if Context <> nil then begin
        if Context.ActiveFont <> nil then begin
          if Context.ActiveFont.Copyright <> '' then
            Result := pAnsiChar(Context.ActiveFont.Copyright);
        end else
          SetError(TS_NO_ACTIVE_FONT, TS_FUNC_GET_STRING);
      end else
        SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_STRING);

    TS_FONT_FACE_NAME:
      if Context <> nil then begin
        if Context.ActiveFont <> nil then begin
          if Context.ActiveFont.FaceName <> '' then
            Result := pAnsiChar(Context.ActiveFont.FaceName);
        end else
          SetError(TS_NO_ACTIVE_FONT, TS_FUNC_GET_STRING);
      end else
        SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_STRING);

    TS_FONT_STYLE_NAME:
      if Context <> nil then begin
        if Context.ActiveFont <> nil then begin
          if Context.ActiveFont.StyleName <> '' then
            Result := pAnsiChar(Context.ActiveFont.StyleName);
        end else
          SetError(TS_NO_ACTIVE_FONT, TS_FUNC_GET_STRING);
      end else
        SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_STRING);

    TS_FONT_FULL_NAME:
      if Context <> nil then begin
        if Context.ActiveFont <> nil then begin
          if Context.ActiveFont.FullName <> '' then
            Result := pAnsiChar(Context.ActiveFont.FullName);
        end else
          SetError(TS_NO_ACTIVE_FONT, TS_FUNC_GET_STRING);
      end else
        SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_GET_STRING);
  else
    SetError(TS_INVALID_ENUM, TS_FUNC_GET_STRING);
  end;
end;


// *** Context functions ***
procedure tsContextCreate(pContextID: ptsContextID);
begin
  if TextSuite_initialized then begin
    if pContextID <> nil then begin
      try
        gCriticalSection.Enter;
        try
          // unbound current context
          if gContext <> nil then begin
            gContext.gBoundThreadID := 0;
            gContext := nil;
          end;

          // create Context and bound it
          gContext := TtsContext.Create;
          gContext.gBoundThreadID := GetCurrentThreadId;

          // add to list
          gContexts.Add(gContext.ContextID, gContext);

          // return context id
          pContextID^ := gContext.ContextID;
        finally
          gCriticalSection.Leave;
        end;
      except
        on E: Exception do begin
          if E is EOutOfMemory then
            SetError(TS_OUT_OF_MEMORY, TS_FUNC_CONTEXT_CREATE)
          else
            SetError(TS_ERROR, TS_FUNC_CONTEXT_CREATE);

          pContextID^ := 0;
        end;
      end;
    end else
      SetError(TS_INVALID_VALUE, TS_FUNC_CONTEXT_CREATE);
  end else
    SetError(TS_NOT_INITIALIZED, TS_FUNC_CONTEXT_CREATE);
end;


procedure tsContextDestroy(ContextID: tsContextID);
var
  Context: TtsContext;
begin
  if TextSuite_initialized then begin
    try
      gCriticalSection.Enter;
      try
        Context := gContexts.Get(ContextID);

        if Context <> nil then begin
          // if context isn't bound or to actual thread
          if (Context.gBoundThreadID = 0) or (Context.gBoundThreadID = GetCurrentThreadId) then begin
            // if it's bound to actual thread
            if Context.gBoundThreadID = GetCurrentThreadId then begin
              gContext.gBoundThreadID := 0; 
              gContext := nil;
            end;

            // delete from list
            gContexts.Delete(ContextID);

            // free them
            Context.Free;
          end else
            SetError(TS_INVALID_OPERATION, TS_FUNC_CONTEXT_DESTROY);
        end else
          SetError(TS_INVALID_VALUE, TS_FUNC_CONTEXT_DESTROY);
      finally
        gCriticalSection.Leave;
      end;
    except
      SetError(TS_ERROR, TS_FUNC_CONTEXT_DESTROY);
    end;
  end else
    SetError(TS_NOT_INITIALIZED, TS_FUNC_CONTEXT_DESTROY);
end;


procedure tsContextBind(ContextID: tsContextID);
var
  Context: TtsContext;
begin
  if TextSuite_initialized then begin
    gCriticalSection.Enter;
    try
      // if any context is bound then release him
      if gContext <> nil then begin
        gContext.gBoundThreadID := 0;
        gContext := nil;
      end;

      // bind new context if <> zero
      if ContextID <> 0 then begin
        Context := gContexts.Get(ContextID);

        if Context <> nil then begin
          // check if the context is bound or bound to actual context
          if (Context.gBoundThreadID = 0) or (Context.gBoundThreadID = GetCurrentThreadId) then begin
            gContext := Context;
            gContext.gBoundThreadID := GetCurrentThreadId;
          end else
            SetError(TS_INVALID_OPERATION, TS_FUNC_CONTEXT_BIND);
        end else
          SetError(TS_INVALID_VALUE, TS_FUNC_CONTEXT_BIND);
      end;
    finally
      gCriticalSection.Leave;
    end;
  end else
    SetError(TS_NOT_INITIALIZED, TS_FUNC_CONTEXT_BIND);
end;


// *** font functions ***
//procedure tsFontCreate(pFontID: ptsFontID); 
//begin

//end;


procedure tsFontCreateCreatorA(Name: pAnsiChar; Size: tsInt; Style: tsBitmask; AntiAliasing: tsEnum; Format: tsEnum; pFontID: ptsFontID);
var
  Context: TtsContext;

  TempStyle: TtsFontStyles;
  TempAntiAliasing: TtsAntiAliasing;
  TempFormat: TtsFormat;

  Creator: TtsFontCreator;
begin
  Context := gContext;

  if Context <> nil then begin
    if Context.Renderer <> nil then begin
      if pFontID <> nil then begin
        pFontID^ := 0;

        // Style
        TempStyle := [];

        if (Style and TS_STYLE_BOLD) > 0 then
          TempStyle := TempStyle + [tsStyleBold];

        if (Style and TS_STYLE_ITALIC) > 0 then
          TempStyle := TempStyle + [tsStyleItalic];

        if (Style and TS_STYLE_UNDERLINE) > 0 then
          TempStyle := TempStyle + [tsStyleUnderline];

        if (Style and TS_STYLE_STRIKEOUT) > 0 then
          TempStyle := TempStyle + [tsStyleStrikeout];

        // AntiAliasing
        if AntiAliasing = TS_DEFAULT then
          AntiAliasing := Context.gGlobalAntiAliasing;

        case AntiAliasing of
          TS_ANTIALIASING_NONE:
            TempAntiAliasing := tsAANone;
          TS_ANTIALIASING_NORMAL:
            TempAntiAliasing := tsAANormal;
        else
          SetError(TS_INVALID_ENUM, TS_FUNC_FONT_CREATE_CREATOR);

          Exit;
        end;

        // Format
        if Format = TS_DEFAULT then
          Format := Context.gGlobalFormat;

        case Format of
          TS_FORMAT_RGBA8:
            TempFormat := tsFormatRGBA8;
        else
          SetError(TS_INVALID_ENUM, TS_FUNC_FONT_CREATE_CREATOR);

          Exit;
        end;

        Creator := nil;

        // create font
        try
          case Context.gCreator of
            TS_CREATOR_SDL:
              begin
                if SDL_TTF_initialized then
                  Creator := TtsFontCreatorSDL.Create(Context.Renderer, Name, Size, TempStyle, TempFormat, TempAntiAliasing)
                else
                  SetError(TS_NOT_INITIALIZED, TS_FUNC_FONT_CREATE_CREATOR);
              end;
            TS_CREATOR_GDI:
              begin
                if GDI_initialized then
                  Creator := TtsFontCreatorGDIFile.Create(Context.Renderer, Name, Size, TempStyle, TempFormat, TempAntiAliasing)
                else
                  SetError(TS_NOT_INITIALIZED, TS_FUNC_FONT_CREATE_CREATOR);
              end;
            TS_CREATOR_GDI_FACENAME:
              begin
                if GDI_initialized then
                  Creator := TtsFontCreatorGDIFontFace.Create(Context.Renderer, Name, Size, TempStyle, TempFormat, TempAntiAliasing)
                else
                  SetError(TS_NOT_INITIALIZED, TS_FUNC_FONT_CREATE_CREATOR);
              end;
            TS_CREATOR_GDI_STREAM:
              begin
                if GDI_initialized then
                  Creator := TtsFontCreatorGDIStream.Create(Context.Renderer, TStream(Name), Size, TempStyle, TempFormat, TempAntiAliasing)
                else
                  SetError(TS_NOT_INITIALIZED, TS_FUNC_FONT_CREATE_CREATOR);
              end;
          else
            SetError(TS_INVALID_ENUM, TS_FUNC_FONT_CREATE_CREATOR);
          end;
        except
          on E: Exception do begin
            if E is EOutOfMemory then
              SetError(TS_OUT_OF_MEMORY, TS_FUNC_FONT_CREATE_CREATOR)
            else
              SetError(TS_ERROR, TS_FUNC_FONT_CREATE_CREATOR);
            Exit;
          end;
        end;

        // Bind font
        if Creator <> nil then begin
          pFontID^ := Context.FontAdd(Creator);

          tsFontBind(pFontID^);
        end;
      end else
        SetError(TS_INVALID_VALUE, TS_FUNC_FONT_CREATE_CREATOR);
    end else
      SetError(TS_NO_ACTIVE_RENDERER, TS_FUNC_FONT_CREATE_CREATOR);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_FONT_CREATE_CREATOR);
end;


procedure tsFontDestroy(FontID: tsFontID);
var
  Context: TtsContext;
  Font: TtsFont;
begin
  Context := gContext;

  if Context <> nil then begin
    if not Context.IsLocked then begin
      Font := Context.FontGet(FontID);

      if Font <> nil then begin
        Context.FontDelete(FontID);

        Font.Free;
      end else
        SetError(TS_INVALID_VALUE, TS_FUNC_FONT_DESTROY);
    end else
      SetError(TS_INVALID_OPERATION, TS_FUNC_FONT_DESTROY);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_FONT_DESTROY);
end;


procedure tsFontBind(FontID: tsFontID);
var
  Context: TtsContext;
begin
  Context := gContext;

  if Context <> nil then begin
    if Context.Renderer <> nil then begin
      if (FontID = 0) or (Context.FontGet(FontID) <> nil) then begin  
        Context.Renderer.FontActivate(FontID);
      end else
        SetError(TS_INVALID_VALUE, TS_FUNC_FONT_BIND);
    end else
      SetError(TS_NO_ACTIVE_RENDERER, TS_FUNC_FONT_BIND);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_FONT_BIND);
end;


procedure tsFontAddCharRange(CharStart: WideChar; CharEnd: WideChar);
var
  ActiveFont: TtsFont;
  CharIdx: WideChar;
  Context: TtsContext;
begin
  Context := gContext;

  if Context <> nil then begin
    ActiveFont := Context.ActiveFont;

    if ActiveFont <> nil then begin
      if ActiveFont is TtsFontCreator then begin
        for CharIdx := CharStart to CharEnd do
          TtsFontCreator(ActiveFont).AddChar(CharIdx);
      end else
        SetError(TS_INVALID_OPERATION, TS_FUNC_FONT_ADD_CHAR);
    end else
      SetError(TS_NO_ACTIVE_FONT, TS_FUNC_FONT_ADD_CHAR);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_FONT_ADD_CHAR);
end;


procedure tsFontAddChars(Chars: pWideChar);
var
  ActiveFont: TtsFont;
  Context: TtsContext;
begin
  Context := gContext;

  if Context <> nil then begin
    ActiveFont := Context.ActiveFont;

    if ActiveFont <> nil then begin
      if ActiveFont is TtsFontCreator then begin
        if Chars <> nil then begin
          while Chars^ <> #0 do begin
            TtsFontCreator(ActiveFont).AddChar(Chars^);

            Inc(Chars);
          end;
        end else
          SetError(TS_INVALID_VALUE, TS_FUNC_FONT_ADD_CHAR);
      end else
        SetError(TS_INVALID_OPERATION, TS_FUNC_FONT_ADD_CHAR);
    end else
      SetError(TS_NO_ACTIVE_FONT, TS_FUNC_FONT_ADD_CHAR);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_FONT_ADD_CHAR);
end;


procedure tsFontAddChar(Char: WideChar);
var
  ActiveFont: TtsFont;
  Context: TtsContext;
begin
  Context := gContext;

  if Context <> nil then begin
    ActiveFont := Context.ActiveFont;

    if ActiveFont <> nil then begin
      if ActiveFont is TtsFontCreator then begin
        TtsFontCreator(ActiveFont).AddChar(Char);
      end else
        SetError(TS_INVALID_OPERATION, TS_FUNC_FONT_ADD_CHAR);
    end else
      SetError(TS_NO_ACTIVE_FONT, TS_FUNC_FONT_ADD_CHAR);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_FONT_ADD_CHAR);
end;


procedure tsFontDeleteCharRange(CharStart: WideChar; CharEnd: WideChar);
var
  Char: WideChar;
  ActiveFont: TtsFont;
  Context: TtsContext;
begin
  Context := gContext;

  if Context <> nil then begin
    ActiveFont := Context.ActiveFont;

    if ActiveFont <> nil then begin
      if ActiveFont is TtsFontCreator then begin
        for Char := CharStart to CharEnd do
          TtsFontCreator(ActiveFont).DeleteChar(Char);
      end else
        SetError(TS_INVALID_OPERATION, TS_FUNC_FONT_DELETE_CHAR);
    end else
      SetError(TS_NO_ACTIVE_FONT, TS_FUNC_FONT_DELETE_CHAR);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_FONT_DELETE_CHAR);
end;


procedure tsFontDeleteChars(Chars: pWideChar);
var
  ActiveFont: TtsFont;
  Context: TtsContext;
begin
  Context := gContext;

  if Context <> nil then begin
    ActiveFont := Context.ActiveFont;

    if ActiveFont <> nil then begin
      if ActiveFont is TtsFontCreator then begin
        if Chars <> nil then begin
          while Chars^ <> #0 do begin
            TtsFontCreator(ActiveFont).DeleteChar(Chars^);

            Inc(Chars);
          end;
        end else
          SetError(TS_INVALID_VALUE, TS_FUNC_FONT_DELETE_CHAR);
      end else
        SetError(TS_INVALID_OPERATION, TS_FUNC_FONT_DELETE_CHAR);
    end else
      SetError(TS_NO_ACTIVE_FONT, TS_FUNC_FONT_DELETE_CHAR);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_FONT_DELETE_CHAR);
end;


procedure tsFontDeleteChar(Char: WideChar);
var
  ActiveFont: TtsFont;
  Context: TtsContext;
begin
  Context := gContext;

  if Context <> nil then begin
    ActiveFont := Context.ActiveFont;

    if ActiveFont <> nil then begin
      if ActiveFont is TtsFontCreator then begin
        TtsFontCreator(Context.ActiveFont).DeleteChar(Char);
      end else
        SetError(TS_INVALID_OPERATION, TS_FUNC_FONT_DELETE_CHAR);
    end else
      SetError(TS_NO_ACTIVE_FONT, TS_FUNC_FONT_DELETE_CHAR);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_FONT_DELETE_CHAR);
end;


procedure tsFontSetCharParameteri(Char: WideChar; ParamName: tsEnum; Param: tsInt);
begin
  if ((ParamName <> TS_CHAR_GLYPHORIGIN) and
      (ParamName <> TS_CHAR_GLYPHRECT)) then begin
    tsFontSetCharParameteriv(Char, ParamName, @Param);
  end else
    SetError(TS_INVALID_ENUM, TS_FUNC_FONT_SET_CHAR_PARAMETER);
end;


procedure tsFontSetCharParameteriv(Char: WideChar; ParamName: tsEnum; pParam: ptsInt);
var
  tsChar: TtsChar;
  Context: TtsContext;
begin
  Context := gContext;

  if Context <> nil then begin
    if Context.ActiveFont <> nil then begin
      if pParam <> nil then begin
        tsChar := Context.ActiveFont.Char[Char];

        if tsChar <> nil then begin
          case ParamName of
            TS_CHAR_ADVANCE:
              tsChar.Advance := pParam^;

            TS_CHAR_GLYPHORIGIN:
              begin
                tsChar.GlyphOriginX := ptsPoint(pParam)^.X;
                tsChar.GlyphOriginY := ptsPoint(pParam)^.Y;
              end;

            TS_CHAR_GLYPHORIGIN_X:
              tsChar.GlyphOriginX := pParam^;

            TS_CHAR_GLYPHORIGIN_Y:
              tsChar.GlyphOriginY := pParam^;

            TS_CHAR_GLYPHRECT:
              tsChar.GlyphRect := pTsRect(pParam)^;

            TS_CHAR_GLYPHRECT_TOP:
              tsChar.GlyphRect.Top := pParam^;

            TS_CHAR_GLYPHRECT_LEFT:
              tsChar.GlyphRect.Left := pParam^;

            TS_CHAR_GLYPHRECT_RIGHT:
              tsChar.GlyphRect.Right := pParam^;

            TS_CHAR_GLYPHRECT_BOTTOM:
              tsChar.GlyphRect.Bottom := pParam^;
          else
            SetError(TS_INVALID_ENUM, TS_FUNC_FONT_SET_CHAR_PARAMETER);
          end;
        end else
          SetError(TS_INVALID_VALUE, TS_FUNC_FONT_SET_CHAR_PARAMETER);
      end else
        SetError(TS_INVALID_VALUE, TS_FUNC_FONT_SET_CHAR_PARAMETER);
    end else
      SetError(TS_NO_ACTIVE_FONT, TS_FUNC_FONT_SET_CHAR_PARAMETER);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_FONT_SET_CHAR_PARAMETER);
end;


function tsFontGetCharParameteri(Char: WideChar; ParamName: tsEnum): tsInt;
begin
  if ((ParamName <> TS_CHAR_GLYPHORIGIN) and
      (ParamName <> TS_CHAR_GLYPHRECT)) then begin
    tsFontGetCharParameteriv(Char, ParamName, @Result);
  end else
    SetError(TS_INVALID_ENUM, TS_FUNC_FONT_GET_CHAR_PARAMETER);
end;


procedure tsFontGetCharParameteriv(Char: WideChar; ParamName: tsEnum; pParam: ptsInt);
var
  tsChar: TtsChar;
  Context: TtsContext;
begin
  pParam^ := 0;
  Context := gContext;

  if Context <> nil then begin
    if Context.ActiveFont <> nil then begin
      if pParam <> nil then begin
        tsChar := Context.ActiveFont.Char[Char];

        if tsChar <> nil then begin
          case ParamName of
            TS_CHAR_ADVANCE:
              pParam^ := tsChar.Advance;
            TS_CHAR_GLYPHORIGIN:
              begin
                ptsPoint(pParam)^.X := tsChar.GlyphOriginX;
                ptsPoint(pParam)^.Y := tsChar.GlyphOriginY;
              end;
            TS_CHAR_GLYPHORIGIN_X:
              pParam^ := tsChar.GlyphOriginX;
            TS_CHAR_GLYPHORIGIN_Y:
              pParam^ := tsChar.GlyphOriginY;
            TS_CHAR_GLYPHRECT:
              PtsRect(pParam)^ := tsChar.GlyphRect;
            TS_CHAR_GLYPHRECT_TOP:
              pParam^ := tsChar.GlyphRect.Top;
            TS_CHAR_GLYPHRECT_LEFT:
              pParam^ := tsChar.GlyphRect.Left;
            TS_CHAR_GLYPHRECT_RIGHT:
              pParam^ := tsChar.GlyphRect.Right;
            TS_CHAR_GLYPHRECT_BOTTOM:
              pParam^ := tsChar.GlyphRect.Bottom;
          else
            SetError(TS_INVALID_ENUM, TS_FUNC_FONT_GET_CHAR_PARAMETER);
          end;
        end else
          SetError(TS_INVALID_VALUE, TS_FUNC_FONT_GET_CHAR_PARAMETER);
      end else
        SetError(TS_INVALID_VALUE, TS_FUNC_FONT_GET_CHAR_PARAMETER);
    end else
      SetError(TS_NO_ACTIVE_FONT, TS_FUNC_FONT_GET_CHAR_PARAMETER);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_FONT_GET_CHAR_PARAMETER);
end;


procedure tsPostAddFillColor3ub(Red, Green, Blue: tsByte; ChannelMask: tsBitmask);
begin
  tsPostAddFillColor4f(Red / $FF, Green / $FF, Blue / $FF, 1, ChannelMask);
end;


procedure tsPostAddFillColor3f(Red, Green, Blue: tsFloat; ChannelMask: tsBitmask);
begin
  tsPostAddFillColor4f(Red, Green, Blue, 1, ChannelMask);
end;


procedure tsPostAddFillColor4ub(Red, Green, Blue, Alpha: tsByte; ChannelMask: tsBitmask);
begin
  tsPostAddFillColor4f(Red / $FF, Green / $FF, Blue / $FF, Alpha / $FF, ChannelMask);
end;


procedure tsPostAddFillColor4f(Red, Green, Blue, Alpha: tsFloat; ChannelMask: tsBitmask);
var
  Context: TtsContext;
begin
  Context := gContext;

  if Context <> nil then begin
    if Context.ActiveFont <> nil then begin
      if Context.ActiveFont is TtsFontCreator then begin
        TtsFontCreator(Context.ActiveFont).AddPostProcessStep(TtsPostFillColor.Create(Red, Green, Blue, Alpha, ChannelMask, Context.gImageMode));
      end else
        SetError(TS_INVALID_OPERATION, TS_FUNC_POST_ADD_FILL_COLOR);
    end else
      SetError(TS_NO_ACTIVE_FONT, TS_FUNC_POST_ADD_FILL_COLOR);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_POST_ADD_FILL_COLOR);
end;


procedure tsPostAddFillPattern(PatternImageID: tsImageID; X, Y: tsInt; ChannelMask: tsBitmask);
var
  Context: TtsContext;
  Image: TtsImage;
begin
  Context := gContext;

  if Context <> nil then begin
    if Context.ActiveFont <> nil then begin
      if Context.ActiveFont is TtsFontCreator then begin
        Image := Context.ImageGet(PatternImageID);

        if Image <> nil then
          TtsFontCreator(Context.ActiveFont).AddPostProcessStep(TtsPostFillPattern.Create(Image, X, Y, ChannelMask, Context.gImageMode))
        else
          SetError(TS_INVALID_VALUE, TS_FUNC_POST_ADD_FILL_PATTERN);
      end else
        SetError(TS_INVALID_OPERATION, TS_FUNC_POST_ADD_FILL_PATTERN);
    end else
      SetError(TS_NO_ACTIVE_FONT, TS_FUNC_POST_ADD_FILL_PATTERN);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_POST_ADD_FILL_PATTERN);
end;


procedure tsPostAddBorder3ub(Width, Strength: Single; Red, Green, Blue: tsByte);
begin
  tsPostAddBorder4f(Width, Strength, Red / $FF, Green / $FF, Blue / $FF, 1);
end;


procedure tsPostAddBorder3f(Width, Strength: Single; Red, Green, Blue: tsFloat);
begin
  tsPostAddBorder4f(Width, Strength, Red, Green, Blue, 1);
end;


procedure tsPostAddBorder4ub(Width, Strength: Single; Red, Green, Blue, Alpha: tsByte);
begin
  tsPostAddBorder4f(Width, Strength, Red / $FF, Green / $FF, Blue / $FF, Alpha / $FF);
end;


procedure tsPostAddBorder4f(Width, Strength: Single; Red, Green, Blue, Alpha: tsFloat);
var
  Context: TtsContext;
begin
  Context := gContext;

  if Context <> nil then begin
    if Context.ActiveFont <> nil then begin
      if Context.ActiveFont is TtsFontCreator then begin
        TtsFontCreator(Context.ActiveFont).AddPostProcessStep(TtsPostBorder.Create(Width, Strength, Red, Green, Blue, Alpha));
      end else
        SetError(TS_INVALID_OPERATION, TS_FUNC_POST_ADD_BORDER);
    end else
      SetError(TS_NO_ACTIVE_FONT, TS_FUNC_POST_ADD_BORDER);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_POST_ADD_BORDER);
end;


procedure tsPostAddShadow3ub(Radius: Single; X, Y: tsInt; Red, Green, Blue: tsByte);
begin
  tsPostAddShadow4f(Radius, X, Y, Red / $FF, Green / $FF, Blue / $FF, 1);
end;


procedure tsPostAddShadow3f(Radius: Single; X, Y: tsInt; Red, Green, Blue: tsFloat);
begin
  tsPostAddShadow4f(Radius, X, Y, Red, Green, Blue, 1);
end;


procedure tsPostAddShadow4ub(Radius: Single; X, Y: tsInt; Red, Green, Blue, Alpha: tsByte);
begin
  tsPostAddShadow4f(Radius, X, Y, Red / $FF, Green / $FF, Blue / $FF, Alpha / $FF);
end;


procedure tsPostAddShadow4f(Radius: Single; X, Y: tsInt; Red, Green, Blue, Alpha: tsFloat);
var
  Context: TtsContext;
begin
  Context := gContext;

  if Context <> nil then begin
    if Context.ActiveFont <> nil then begin
      if Context.ActiveFont is TtsFontCreator then begin
        TtsFontCreator(Context.ActiveFont).AddPostProcessStep(TtsPostShadow.Create(Radius, X, Y, Red, Green, Blue, Alpha));
      end else
        SetError(TS_INVALID_OPERATION, TS_FUNC_POST_ADD_SHADOW);
    end else
      SetError(TS_NO_ACTIVE_FONT, TS_FUNC_POST_ADD_SHADOW);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_POST_ADD_SHADOW);
end;


(*
procedure tsPostAddKerning;
var
  Context: TtsContext;
begin
  Context := gContext;

  if Context <> nil then begin
    if Context.ActiveFont <> nil then begin
      if Context.ActiveFont is TtsFontCreator then begin
        TtsFontCreator(Context.ActiveFont).AddPostProcessStep(TtsPostKerning.Create);
      end else
        SetError(TS_INVALID_OPERATION;
    end else
      SetError(TS_NO_ACTIVE_FONT;
  end else
    SetError(TS_NO_ACTIVE_CONTEXT;
end;
*)

procedure tsPostAddCustom(PostProcessProc: tsPostProcessProc; Data: Pointer);
var
  Context: TtsContext;
begin
  Context := gContext;

  if Context <> nil then begin
    if Context.ActiveFont <> nil then begin
      if Context.ActiveFont is TtsFontCreator then begin
        TtsFontCreator(Context.ActiveFont).AddPostProcessStep(TtsPostCustom.Create(Context, PostProcessProc, Data));
      end else
        SetError(TS_INVALID_OPERATION, TS_FUNC_POST_ADD_CUSTOM);
    end else
      SetError(TS_NO_ACTIVE_FONT, TS_FUNC_POST_ADD_CUSTOM);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_POST_ADD_CUSTOM);
end;


procedure tsPostDelete(PostIndex: tsInt);
var
  Context: TtsContext;
begin
  Context := gContext;

  if Context <> nil then begin
    if Context.ActiveFont <> nil then begin
      if Context.ActiveFont is TtsFontCreator then begin
        if PostIndex = TS_POST_INDEX_ALL then begin
          TtsFontCreator(Context.ActiveFont).ClearPostProcessSteps;
        end else

        begin
          // tranlate to direct index
          if PostIndex = TS_POST_INDEX_LAST then
            PostIndex := TtsFontCreator(Context.ActiveFont).PostProcessStepCount -1;

          TtsFontCreator(Context.ActiveFont).DeletePostProcessStep(PostIndex);
        end;
      end else
        SetError(TS_INVALID_OPERATION, TS_FUNC_POST_DELETE);
    end else
      SetError(TS_NO_ACTIVE_FONT, TS_FUNC_POST_DELETE);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_POST_DELETE);
end;


procedure tsPostAddUsageRange(PostIndex: tsInt; UsageType: tsEnum; CharStart, CharEnd: WideChar);
var
  Context: TtsContext;
  ActFont: TtsFont;
  Usage: TtsFontProcessStepUsage;
  Idx: Integer;


  procedure AssignUsage(Idx: Integer);
  var
    PostProcess: TtsPostProcessStep;
  begin
    PostProcess := TtsFontCreator(ActFont).PostProcessStep[Idx];

    if PostProcess <> nil then begin
      PostProcess.AddUsageRange(Usage, CharStart, CharEnd);
    end else
      SetError(TS_INVALID_VALUE, TS_FUNC_POST_ADD_USAGE);
  end;

begin
  Context := gContext;

  if Context <> nil then begin
    ActFont := Context.ActiveFont;
    if ActFont <> nil then begin
      if ActFont is TtsFontCreator then begin
        // getting usagetype
        case UsageType of
          TS_POST_USAGE_INCLUDE:
            Usage := tsUInclude;
          TS_POST_USAGE_EXCLUDE:
            Usage := tsUExclude;
        else
          SetError(TS_INVALID_ENUM, TS_FUNC_POST_ADD_USAGE);
          Exit;
        end;

        // add usage to post processors
        if PostIndex = TS_POST_INDEX_ALL then begin
          for Idx := 0 to TtsFontCreator(ActFont).PostProcessStepCount -1 do
            AssignUsage(Idx);
        end else

        begin
          if PostIndex = TS_POST_INDEX_LAST then
            PostIndex := TtsFontCreator(ActFont).PostProcessStepCount -1;

          AssignUsage(PostIndex);
        end;
      end else
        SetError(TS_INVALID_OPERATION, TS_FUNC_POST_ADD_USAGE);
    end else
      SetError(TS_NO_ACTIVE_FONT, TS_FUNC_POST_ADD_USAGE);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_POST_ADD_USAGE);
end;


procedure tsPostAddUsageChars(PostIndex: tsInt; UsageType: tsEnum; Chars: pWideChar);
var
  Context: TtsContext;
  ActFont: TtsFont;
  Usage: TtsFontProcessStepUsage;
  Idx: Integer;


  procedure AssignUsage(Idx: Integer);
  var
    PostProcess: TtsPostProcessStep;
  begin
    PostProcess := TtsFontCreator(ActFont).PostProcessStep[Idx];

    if PostProcess <> nil then begin
      PostProcess.AddUsageChars(Usage, Chars);
    end else
      SetError(TS_INVALID_VALUE, TS_FUNC_POST_ADD_USAGE);
  end;

begin
  Context := gContext;

  if Context <> nil then begin
    if Chars <> nil then begin
      ActFont := Context.ActiveFont;
      if ActFont <> nil then begin
        if ActFont is TtsFontCreator then begin
          // getting usagetype
          case UsageType of
            TS_POST_USAGE_INCLUDE:
              Usage := tsUInclude;
            TS_POST_USAGE_EXCLUDE:
              Usage := tsUExclude;
          else
            SetError(TS_INVALID_ENUM, TS_FUNC_POST_ADD_USAGE);
            Exit;
          end;

          // add usage to post processors
          if PostIndex = TS_POST_INDEX_ALL then begin
            for Idx := 0 to TtsFontCreator(ActFont).PostProcessStepCount -1 do
              AssignUsage(Idx);
          end else

          begin
            if PostIndex = TS_POST_INDEX_LAST then
              PostIndex := TtsFontCreator(ActFont).PostProcessStepCount -1;

            AssignUsage(PostIndex);
          end;
        end else
          SetError(TS_INVALID_OPERATION, TS_FUNC_POST_ADD_USAGE);
      end else
        SetError(TS_NO_ACTIVE_FONT, TS_FUNC_POST_ADD_USAGE);
    end else
      SetError(TS_INVALID_VALUE, TS_FUNC_POST_ADD_USAGE);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_POST_ADD_USAGE);
end;


procedure tsPostClearUsage(PostIndex: tsInt); cdecl;
var
  Context: TtsContext;
  ActFont: TtsFont;
  Idx: Integer;


  procedure ClearUsage(Idx: Integer);
  var
    PostProcess: TtsPostProcessStep;
  begin
    PostProcess := TtsFontCreator(ActFont).PostProcessStep[Idx];

    if PostProcess <> nil then begin
      PostProcess.ClearIncludeRange;
      PostProcess.ClearExcludeRange;
    end else
      SetError(TS_INVALID_VALUE, TS_FUNC_POST_CLEAR_USAGE);
  end;

begin
  Context := gContext;

  if Context <> nil then begin
    ActFont := Context.ActiveFont;
    if ActFont <> nil then begin
      if ActFont is TtsFontCreator then begin
        if PostIndex = TS_POST_INDEX_ALL then begin
          for Idx := 0 to TtsFontCreator(ActFont).PostProcessStepCount -1 do
            ClearUsage(Idx);
        end else

        begin
          if PostIndex = TS_POST_INDEX_LAST then
            PostIndex := TtsFontCreator(ActFont).PostProcessStepCount -1;

          ClearUsage(PostIndex);
        end;
      end else
        SetError(TS_INVALID_OPERATION, TS_FUNC_POST_CLEAR_USAGE);
    end else
      SetError(TS_NO_ACTIVE_FONT, TS_FUNC_POST_CLEAR_USAGE);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_POST_CLEAR_USAGE);
end;


function tsStringAnsiToWide(pText: pAnsiChar): pWideChar;
var
  Context: TtsContext;
begin
  Result := nil;

  if TextSuite_initialized then begin
    Context := gContext;

    try
      if Context <> nil then begin
        if pText <> nil then begin
          Result := Context.AnsiToWide(pText);

          if Result <> nil then begin
            gCriticalSection.Enter;
            try
              gStrings.Add(Result);
            finally
              gCriticalSection.Leave;
            end;
          end else
              SetError(TS_INVALID_OPERATION, TS_FUNC_STRING_ANSI_TO_WIDE);
        end else
          SetError(TS_INVALID_VALUE, TS_FUNC_STRING_ANSI_TO_WIDE);
      end else
        SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_STRING_ANSI_TO_WIDE);
    except
      on E: Exception do begin
        if E is EOutOfMemory then
          SetError(TS_OUT_OF_MEMORY, TS_FUNC_STRING_ANSI_TO_WIDE)
        else
          SetError(TS_ERROR, TS_FUNC_STRING_ANSI_TO_WIDE);
      end;
    end;
  end else
    SetError(TS_NOT_INITIALIZED, TS_FUNC_STRING_ANSI_TO_WIDE);
end;


function tsStringAlloc(Size: tsInt): pWideChar; cdecl;
begin
  Result := nil;

  if TextSuite_initialized then begin 
    try
      Result := tsStrAlloc(Size);

      gCriticalSection.Enter;
      try
        gStrings.Add(Result);
      finally
        gCriticalSection.Leave;
      end;
    except
      on E: Exception do begin
        if E is EOutOfMemory then
          SetError(TS_OUT_OF_MEMORY, TS_FUNC_STRING_ALLOC)
        else
          SetError(TS_ERROR, TS_FUNC_STRING_ALLOC);
      end;
    end;
  end else
    SetError(TS_NOT_INITIALIZED, TS_FUNC_STRING_ALLOC);
end;


procedure tsStringDispose(pText: pWideChar);
var
  ValidString: Boolean;
begin
  if TextSuite_initialized then begin 
    if pText <> nil then begin
      try
        // delete String from hash
        gCriticalSection.Enter;
        try
          ValidString := gStrings.Delete(pText);
        finally
          gCriticalSection.Leave;
        end;

        // if sting valid free them
        if ValidString then
          tsStrDispose(pText)
        else
          SetError(TS_INVALID_OPERATION, TS_FUNC_STRING_DISPOSE);
      except
        on E: Exception do begin
          if E is EAccessViolation then
            SetError(TS_INVALID_OPERATION, TS_FUNC_STRING_DISPOSE)
          else
            SetError(TS_ERROR, TS_FUNC_STRING_DISPOSE);
        end;
      end;
    end else
      SetError(TS_INVALID_VALUE, TS_FUNC_STRING_DISPOSE);
  end else
    SetError(TS_NOT_INITIALIZED, TS_FUNC_STRING_DISPOSE);
end;


procedure tsTextBeginBlock(Left, Top, Width, Height: tsInt; Flags: tsBitmask); 
var
  Context: TtsContext;
begin
  Context := gContext;

  if Context <> nil then begin
    if Context.Renderer <> nil then begin
      if not Context.IsLocked then begin
        Context.Renderer.BeginBlock(Left, Top, Width, Height, Flags);
      end else
        SetError(TS_INVALID_OPERATION, TS_FUNC_TEXT_BEGIN_BLOCK);
    end else
      SetError(TS_NO_ACTIVE_RENDERER, TS_FUNC_TEXT_BEGIN_BLOCK);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_TEXT_BEGIN_BLOCK);
end;


procedure tsTextEndBlock; 
var
  Context: TtsContext;
begin
  Context := gContext;

  if Context <> nil then begin
    if Context.Renderer <> nil then begin
      if Context.IsLocked then begin
        Context.Renderer.EndBlock;
      end else
        SetError(TS_INVALID_OPERATION, TS_FUNC_TEXT_END_BLOCK);
    end else
      SetError(TS_NO_ACTIVE_RENDERER, TS_FUNC_TEXT_END_BLOCK);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_TEXT_END_BLOCK);
end;


procedure tsTextColor3ub(Red, Green, Blue: tsByte);
begin
  tsTextColor4f(Red / $FF, Green / $FF, Blue / $FF, 1);
end;


procedure tsTextColor3f(Red, Green, Blue: tsFloat);
begin
  tsTextColor4f(Red, Green, Blue, 1);
end;


procedure tsTextColor4ub(Red, Green, Blue, Alpha: tsByte);
begin
  tsTextColor4f(Red / $FF, Green / $FF, Blue / $FF, Alpha / $FF);
end;


procedure tsTextColor4f(Red, Green, Blue, Alpha: tsFloat);
var
  Context: TtsContext;
begin
  Context := gContext;

  if Context <> nil then begin
    if Context.Renderer <> nil then begin
      Context.Renderer.Color(Red, Green, Blue, Alpha);
    end else
      SetError(TS_NO_ACTIVE_RENDERER, TS_FUNC_TEXT_COLOR);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_TEXT_COLOR);
end;


procedure tsTextOutA(pText: pAnsiChar);
var
  pTemp: PWideChar;
  Context: TtsContext;
begin
  Context := gContext;

  if Context <> nil then begin
    if Context.Renderer <> nil then begin
      if Context.ActiveFont <> nil then begin
        if pText <> nil then begin
          // convert text via codepage
          pTemp := Context.AnsiToWide(pText);
          if pTemp <> nil then begin
            try
              Context.Renderer.TextOut(pTemp);
            finally
              tsStrDispose(pTemp);
            end;
          end else
            SetError(TS_INVALID_OPERATION, TS_FUNC_TEXT_OUT);
        end else
          SetError(TS_INVALID_VALUE, TS_FUNC_TEXT_OUT);
      end else
        SetError(TS_NO_ACTIVE_FONT, TS_FUNC_TEXT_OUT);
    end else
      SetError(TS_NO_ACTIVE_RENDERER, TS_FUNC_TEXT_OUT);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_TEXT_OUT);
end;


procedure tsTextOutW(pText: pWideChar);
var
  Context: TtsContext;
begin
  Context := gContext;

  if Context <> nil then begin
    if Context.Renderer <> nil then begin
      if Context.ActiveFont <> nil then begin
        if pText <> nil then begin
          Context.Renderer.TextOut(pText);
        end else
          SetError(TS_INVALID_VALUE, TS_FUNC_TEXT_OUT);
      end else
        SetError(TS_NO_ACTIVE_FONT, TS_FUNC_TEXT_OUT);
    end else
      SetError(TS_NO_ACTIVE_RENDERER, TS_FUNC_TEXT_OUT);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_TEXT_OUT);
end;


function tsTextGetWidthA(pText: pAnsiChar): tsInt;
var
  pTemp: PWideChar;
  Context: TtsContext;
begin
  Result := 0;
  Context := gContext;

  if Context <> nil then begin
    pTemp := nil;

    // if pText is assigned convert it
    if pText <> nil then begin
      pTemp := Context.AnsiToWide(pText);

      if pTemp = nil then begin
        SetError(TS_INVALID_OPERATION, TS_FUNC_TEXT_GET_WIDTH);
        Exit;
      end;
    end;

    try
      Result := tsTextGetWidthW(pTemp);
    finally
      if pTemp <> nil then
        tsStrDispose(pTemp);
    end;
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_TEXT_GET_WIDTH);
end;


function tsTextGetWidthW(pText: pWideChar): tsInt;
var
  Context: TtsContext;
begin
  Result := 0;

  Context := gContext;

  if Context <> nil then begin
    if Context.Renderer <> nil then begin
      // Width from the text
      if pText <> nil then begin
        if Context.ActiveFont <> nil then begin
          Result := Context.Renderer.TextGetWidth(pText);
        end else
          SetError(TS_NO_ACTIVE_FONT, TS_FUNC_TEXT_GET_WIDTH);
      end else

      // Width from blockmode
      begin
        if Context.IsLocked then begin
          Result := Context.Renderer.TextGetDrawWidth;
        end else
          SetError(TS_INVALID_VALUE, TS_FUNC_TEXT_GET_WIDTH);
      end;
    end else
      SetError(TS_NO_ACTIVE_RENDERER, TS_FUNC_TEXT_GET_WIDTH);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_TEXT_GET_WIDTH);
end;


function tsTextGetHeightA(pText: pAnsiChar): tsInt;
var
  pTemp: PWideChar;
  Context: TtsContext;
begin
  Result := 0;
  Context := gContext;

  if Context <> nil then begin
    pTemp := nil;

    // if pText is assigned convert it
    if pText <> nil then begin
      pTemp := Context.AnsiToWide(pText);

      if pTemp = nil then begin      
        SetError(TS_INVALID_OPERATION, TS_FUNC_TEXT_GET_WIDTH);
        Exit;
      end;
    end;

    try
      Result := tsTextGetHeightW(pTemp);
    finally
      if pTemp <> nil then
        tsStrDispose(pTemp);
    end;
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_TEXT_GET_WIDTH);
end;


function tsTextGetHeightW(pText: pWideChar): tsInt; 
var
  Context: TtsContext;
begin
  Result := 0;

  Context := gContext;

  if Context <> nil then begin
    if Context.Renderer <> nil then begin
      // Height from the text
      if pText <> nil then begin
        if Context.ActiveFont <> nil then begin
          with Context.ActiveFont do
            Result := Ascent + Descent + ExternalLeading;
        end else
          SetError(TS_NO_ACTIVE_FONT, TS_FUNC_TEXT_GET_HEIGHT);
      end else

      // Height from blockmode
      begin
        if Context.IsLocked then begin
          Result := Context.Renderer.TextGetDrawHeight;
        end else
          SetError(TS_INVALID_VALUE, TS_FUNC_TEXT_GET_HEIGHT);
      end;
    end else
      SetError(TS_NO_ACTIVE_RENDERER, TS_FUNC_TEXT_GET_HEIGHT);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_TEXT_GET_HEIGHT);
end;


procedure tsCharOutW(CharCode: WideChar); cdecl;
var
  Context: TtsContext;
begin
  Context := gContext;

  if Context <> nil then begin
    if Context.Renderer <> nil then begin
      if not Context.IsLocked then begin
        if Context.Renderer.ActiveFont <> nil then begin
          if Context.Renderer.ActiveFont.Char[CharCode] <> nil then begin
            Context.Renderer.CharOut(CharCode);
          end else
            SetError(TS_INVALID_VALUE, TS_FUNC_TEXT_OUT);
        end else
          SetError(TS_NO_ACTIVE_FONT, TS_FUNC_TEXT_OUT);
      end else
        SetError(TS_INVALID_OPERATION, TS_FUNC_TEXT_OUT);
    end else
      SetError(TS_NO_ACTIVE_RENDERER, TS_FUNC_TEXT_OUT);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_TEXT_OUT);
end;


// *** image functions ***
procedure tsImageCreate(pImageID: ptsImageID);
var
  Context: TtsContext;
  Image: TtsImage;
begin
  Context := gContext;

  if Context <> nil then begin
    if pImageID <> nil then begin
      // create image
      try
        Image := TtsImage.Create;
      except
        on E: Exception do begin
          if E is EOutOfMemory then
            SetError(TS_OUT_OF_MEMORY, TS_FUNC_IMAGE_CREATE)
          else
            SetError(TS_ERROR, TS_FUNC_IMAGE_CREATE);
          Exit;
        end;
      end;

      // add image
      pImageID^ := Context.ImageAdd(Image);
    end else
      SetError(TS_INVALID_VALUE, TS_FUNC_IMAGE_CREATE);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_IMAGE_CREATE);
end;


procedure tsImageDestroy(ImageID: tsImageID);
var
  Context: TtsContext;
  Image: TtsImage;
begin
  Context := gContext;
  
  if Context <> nil then begin
    Image := Context.ImageGet(ImageID);

    if Image <> nil then begin
      Context.ImageDelete(ImageID);

      Image.Free;
    end else
      SetError(TS_INVALID_VALUE, TS_FUNC_IMAGE_DESTROY);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_IMAGE_DESTROY);
end;


procedure tsImageLoadA(ImageID: tsImageID; Filename: pAnsiChar);
var
  Context: TtsContext;
  Image: TtsImage;
begin
  Context := gContext;

  if Context <> nil then begin
    case gContext.gImageLibrary of
      // SDL_image
      TS_IMAGE_LIBRARY_SDL:
        begin
          if SDL_IMAGE_initialized then begin
            Image := Context.ImageGet(ImageID);

            if Image <> nil then begin
              try
                Image.LoadFromFile(Filename);
              except
                on E: Exception do begin
                  if E is EOutOfMemory then
                    SetError(TS_OUT_OF_MEMORY, TS_FUNC_IMAGE_LOAD)
                  else
                    SetError(TS_ERROR, TS_FUNC_IMAGE_LOAD);
                end;
              end;
            end else
              SetError(TS_INVALID_VALUE, TS_FUNC_IMAGE_LOAD);
          end else
            SetError(TS_NOT_INITIALIZED, TS_FUNC_IMAGE_LOAD);
        end;
    else
      SetError(TS_INVALID_OPERATION, TS_FUNC_IMAGE_LOAD);
    end;
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_IMAGE_LOAD);
end;


procedure tsImageAssignFrom(ImageID: tsImageID; FromImageID: tsImageID);
var
  Context: TtsContext;
  Image, ImageFrom: TtsImage;
begin
  Context := gContext;

  if Context <> nil then begin
    Image := Context.ImageGet(ImageID);
    ImageFrom := Context.ImageGet(FromImageID);

    if (Image <> nil) and (ImageFrom <> nil) then begin
      if not ImageFrom.Empty then begin
        try
          Image.AssignFrom(ImageFrom)
        except
          on E: Exception do begin
            if E is EOutOfMemory then
              SetError(TS_OUT_OF_MEMORY, TS_FUNC_IMAGE_ASSIGN_FROM)
            else
              SetError(TS_ERROR, TS_FUNC_IMAGE_ASSIGN_FROM);
            Exit;
          end;
        end;
      end else
        SetError(TS_INVALID_OPERATION, TS_FUNC_IMAGE_ASSIGN_FROM);
    end else
      SetError(TS_INVALID_VALUE, TS_FUNC_IMAGE_ASSIGN_FROM);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_IMAGE_ASSIGN_FROM);
end;


procedure tsImageNew(ImageID: tsImageID; Width: tsInt; Height: tsInt; Format: tsEnum);
var
  Context: TtsContext;
  Image: TtsImage;
  ImageFormat: TtsFormat;
begin
  Context := gContext;

  if Context <> nil then begin
    Image := Context.ImageGet(ImageID);

    if Image <> nil then begin
      // format
      if Format = TS_DEFAULT then
        Format := Context.gGlobalFormat;

      case Format of
        TS_FORMAT_RGBA8:
          ImageFormat := tsFormatRGBA8;
      else
        SetError(TS_INVALID_ENUM, TS_FUNC_IMAGE_NEW);

        // leave the function
        Exit;
      end;

      if (Width > 0) and (Height > 0) then begin
        try
          Image.CreateEmpty(ImageFormat, Width, Height);
        except
          on E: Exception do begin
            if E is EOutOfMemory then
              SetError(TS_OUT_OF_MEMORY, TS_FUNC_IMAGE_NEW)
            else
              SetError(TS_ERROR, TS_FUNC_IMAGE_NEW);
          end;
        end;
      end else
        SetError(TS_INVALID_VALUE, TS_FUNC_IMAGE_NEW);
    end else
      SetError(TS_INVALID_VALUE, TS_FUNC_IMAGE_NEW);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_IMAGE_NEW);
end;


procedure tsImageGetInfo(ImageID: tsImageID; pisEmpty: ptsBool; pWidth: ptsInt; pHeight: ptsInt; pFormat: ptsEnum; pData: PPointer);
var
  Context: TtsContext;
  Image: TtsImage;
begin
  Context := gContext;

  // defaults
  if pisEmpty <> nil then
    pisEmpty^ := TS_FALSE;

  if pWidth <> nil then
    pWidth^ := 0;

  if pHeight <> nil then
    pHeight^ := 0;

  if pFormat <> nil then
    pFormat^ := TS_FORMAT_EMPTY;

  if pData <> nil then
    pData^ := nil;

  // query values
  if Context <> nil then begin
    Image := Context.ImageGet(ImageID);

    if Image <> nil then begin
      // isEmpty
      if pisEmpty <> nil then
        if Image.Empty then
          pisEmpty^ := TS_TRUE
        else
          pisEmpty^ := TS_FALSE;

      // Width
      if pWidth <> nil then
        pWidth^ := Image.Width;

      // Height
      if pHeight <> nil then
        pHeight^ := Image.Height;

      // Format
      if pFormat <> nil then begin
        case Image.Format of
          tsFormatEmpty:
            pFormat^ := TS_FORMAT_EMPTY;
          tsFormatRGBA8:
            pFormat^ := TS_FORMAT_RGBA8;
        end;
      end;

      // Data
      if pData <> nil then
        pData^ := Image.Data;
    end else
      SetError(TS_INVALID_VALUE, TS_FUNC_IMAGE_GET_INFO);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_IMAGE_GET_INFO);
end;


function tsImageGetIsEmpty(ImageID: tsImageID): tsBool;
begin
  tsImageGetInfo(ImageID, @Result, nil, nil, nil, nil);
end;


function tsImageGetWidth(ImageID: tsImageID): tsInt;
begin
  tsImageGetInfo(ImageID, nil, @Result, nil, nil, nil);
end;


function tsImageGetHeight(ImageID: tsImageID): tsInt;
begin
  tsImageGetInfo(ImageID, nil, nil, @Result, nil, nil);
end;


function tsImageGetFormat(ImageID: tsImageID): tsEnum;
begin
  tsImageGetInfo(ImageID, nil, nil, nil, @Result, nil);
end;


function tsImageGetData(ImageID: tsImageID): Pointer;
begin
  tsImageGetInfo(ImageID, nil, nil, nil, nil, @Result);
end;


function tsImageScanline(ImageID: tsImageID; ScanLine: tsInt): Pointer;
var
  Context: TtsContext;
  Image: TtsImage;
begin
  Result := nil;

  Context := gContext;

  if Context <> nil then begin
    Image := Context.ImageGet(ImageID);

    if Image <> nil then begin
      if not Image.Empty then begin      
        Result := Image.ScanLine[ScanLine];
      end else
        SetError(TS_INVALID_OPERATION, TS_FUNC_IMAGE_SCANLINE);
    end else
      SetError(TS_INVALID_VALUE, TS_FUNC_IMAGE_SCANLINE);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_IMAGE_SCANLINE);
end;


procedure tsImageResize(ImageID: tsImageID; Width, Height, X, Y: tsInt);
var
  Context: TtsContext;
  Image: TtsImage;
begin
  Context := gContext;

  if Context <> nil then begin
    Image := Context.ImageGet(ImageID);

    if Image <> nil then begin
      if not Image.Empty then begin
        Image.Resize(Width, Height, X, Y);
      end else
        SetError(TS_INVALID_OPERATION, TS_FUNC_IMAGE_RESIZE);
    end else
      SetError(TS_INVALID_VALUE, TS_FUNC_IMAGE_RESIZE);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_IMAGE_RESIZE);
end;


procedure tsImageBlend(ImageID, OverImageID: tsImageID; X, Y: tsInt; AutoExpand: tsBool);
var
  Context: TtsContext;
  Image, ImageOver: TtsImage;
begin
  Context := gContext;

  if Context <> nil then begin
    Image := Context.ImageGet(ImageID);
    ImageOver := Context.ImageGet(OverImageID);

    if (Image <> nil) and (ImageOver <> nil) then begin
      if (not Image.Empty) and (not ImageOver.Empty) then begin
        Image.BlendImage(ImageOver, X, Y, AutoExpand = TS_TRUE);
      end else
        SetError(TS_INVALID_OPERATION, TS_FUNC_IMAGE_BLEND);
    end else
      SetError(TS_INVALID_VALUE, TS_FUNC_IMAGE_BLEND);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_IMAGE_BLEND);
end;


procedure tsImageBlur(ImageID: tsImageID; X, Y: tsFloat; ChannelMask: tsBitmask; AutoExpand: tsBool; ExpandSizeX, ExpandSizeY: ptsInt);
var
  Context: TtsContext;
  Image: TtsImage;

  HorzKernel, VertKernel: TtsKernel1D;
begin
  Context := gContext;

  if Context <> nil then begin
    Image := Context.ImageGet(ImageID);

    if Image <> nil then begin
      if not Image.Empty then begin
        // Creating kernels
        HorzKernel := TtsKernel1D.Create(X, 0);
        VertKernel := TtsKernel1D.Create(Y, 0);

        if AutoExpand = TS_TRUE then begin
          // resizing image
          Image.Resize(Image.Width + HorzKernel.Size * 2, Image.Height + VertKernel.Size * 2, HorzKernel.Size, VertKernel.Size);

          if ExpandSizeX <> nil then
            ExpandSizeX^ := HorzKernel.Size;

          if ExpandSizeY <> nil then
            ExpandSizeY^ := VertKernel.Size
        end else

        begin
          if ExpandSizeX <> nil then
            ExpandSizeX^ := 0;

          if ExpandSizeY <> nil then
            ExpandSizeY^ := 0;
        end;

        // bluring image
        Image.Blur(HorzKernel, VertKernel, ChannelMask);

        // freeing
        HorzKernel.Free;
        VertKernel.Free;
      end else
        SetError(TS_INVALID_OPERATION, TS_FUNC_IMAGE_BLUR);
    end else
      SetError(TS_INVALID_VALUE, TS_FUNC_IMAGE_BLUR);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_IMAGE_BLUR);
end;


procedure tsImageFillColor3ub(ImageID: tsImageID; Red, Green, Blue: tsByte; ChannelMask: tsBitmask);
begin
  tsImageFillColor4f(ImageID, Red / $FF, Green / $FF, Blue / $FF, 1, ChannelMask);
end;


procedure tsImageFillColor3f(ImageID: tsImageID; Red, Green, Blue: tsFloat; ChannelMask: tsBitmask);
begin
  tsImageFillColor4f(ImageID, Red, Green, Blue, 1, ChannelMask);
end;


procedure tsImageFillColor4ub(ImageID: tsImageID; Red, Green, Blue, Alpha: tsByte; ChannelMask: tsBitmask);
begin
  tsImageFillColor4f(ImageID, Red / $FF, Green / $FF, Blue / $FF, Alpha / $FF, ChannelMask);
end;


procedure tsImageFillColor4f(ImageID: tsImageID; Red, Green, Blue, Alpha: tsFloat; ChannelMask: tsBitmask);
var
  Context: TtsContext;
  Image: TtsImage;
begin
  Context := gContext;

  if Context <> nil then begin
    Image := Context.ImageGet(ImageID);

    if Image <> nil then begin
      if not Image.Empty then begin
        Image.FillColor(Red, Green, Blue, Alpha, ChannelMask, Context.gImageMode);
      end else
        SetError(TS_INVALID_OPERATION, TS_FUNC_IMAGE_FILL_COLOR);
    end else
      SetError(TS_INVALID_VALUE, TS_FUNC_IMAGE_FILL_COLOR);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_IMAGE_FILL_COLOR);
end;


procedure tsImageFillPattern(ImageID, PatternImageID: tsImageID; X, Y: tsInt; ChannelMask: tsBitmask);
var
  Context: TtsContext;
  Image, ImagePattern: TtsImage;
begin
  Context := gContext;

  if Context <> nil then begin
    Image := Context.ImageGet(ImageID);
    ImagePattern := Context.ImageGet(PatternImageID);

    if (Image <> nil) and (ImagePattern <> nil) then begin
      if (not Image.Empty) and (not ImagePattern.Empty) then begin
        Image.FillPattern(ImagePattern, X, Y, ChannelMask, Context.gImageMode);
      end else
        SetError(TS_INVALID_OPERATION, TS_FUNC_IMAGE_FILL_PATTERN);
    end else
      SetError(TS_INVALID_VALUE, TS_FUNC_IMAGE_FILL_PATTERN);
  end else
    SetError(TS_NO_ACTIVE_CONTEXT, TS_FUNC_IMAGE_FILL_PATTERN);
end;


{$endif}


end.
