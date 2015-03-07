unit utsFreeType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs, dynlibs, utsTextSuite;

type
  // Simple Types
  FT_Error    = Integer;
  FT_Library  = Pointer;
  FT_Short    = SmallInt;
  FT_Byte     = Byte;
  FT_Char     = AnsiChar;
  FT_UShort   = Word;
  FT_Int      = Integer;
  FT_Int32    = Integer;
  FT_UInt     = Cardinal;
  FT_Long     = LongInt;
  FT_ULong    = Cardinal;
  FT_Fixed    = LongInt;
  FT_Pos      = LongInt;
  FT_F26Dot6  = LongInt;
  FT_String   = AnsiChar;

  // Enums
  FT_Encoding     = Integer;
  FT_Glyph_Format = Integer;

  // Pointer
  FT_Face       = ^FT_FaceRec;
  FT_GlyphSlot  = ^FT_GlyphSlotRec;
  FT_Size       = ^FT_SizeRec;
  FT_CharMap    = ^FT_CharMapRec;

  PFT_Library    = ^FT_Library;
  PFT_Face       = ^FT_Face;
  PFT_String     = ^FT_String;
  PFT_SfntName   = ^FT_SfntName;
  PFT_Bitmap     = ^FT_Bitmap;
  PTT_OS2        = ^TT_OS2;
  PTT_Postscript = ^TT_Postscript;
  PTT_HoriHeader = ^TT_HoriHeader;

  // unneeded
  FT_Driver         = Pointer;
  FT_Memory         = Pointer;
  FT_Stream         = Pointer;
  FT_ListNode       = Pointer;
  FT_Face_Internal  = Pointer;
  FT_SubGlyph       = Pointer;
  FT_Slot_Internal  = Pointer;
  FT_Size_Internal  = Pointer;

  FT_Generic_Finalizer = procedure(aObject: Pointer);

  FT_Generic = record
    data:       Pointer;
    finalizer:  FT_Generic_Finalizer;
  end;

  FT_BBox = record
    xMin, yMin, xMax, yMax: FT_Pos;
  end;

  FT_Vector = record
    x, y: FT_Pos;
  end;

  FT_ListRec = record
    head: FT_ListNode;
    tail: FT_ListNode;
  end;

  FT_CharMapRec = record
    face:         FT_Face;
    encoding:     FT_Encoding;
    platform_id:  FT_UShort;
    encoding_id:  FT_UShort;
  end;

  FT_Size_Metrics = record
    x_ppem:       FT_UShort;
    y_ppem:       FT_UShort;

    x_scale:      FT_Fixed;
    y_scale:      FT_Fixed;

    ascender:     FT_Pos;
    descender:    FT_Pos;
    height:       FT_Pos;
    max_advance:  FT_Pos;
  end;

  FT_SizeRec = record
    face:     FT_Face;
    generic_: FT_Generic;
    metrics:  FT_Size_Metrics;
    internal: FT_Size_Internal;
  end;

  FT_Glyph_Metrics = record
    width:        FT_Pos;
    height:       FT_Pos;

    horiBearingX: FT_Pos;
    horiBearingY: FT_Pos;
    horiAdvance:  FT_Pos;

    vertBearingX: FT_Pos;
    vertBearingY: FT_Pos;
    vertAdvance:  FT_Pos;
  end;

  FT_Bitmap_Size = record
    height:   FT_Short;
    width:    FT_Short;

    size:     FT_Pos;

    x_ppem:   FT_Pos;
    y_ppem:   FT_Pos;
  end;

  FT_Bitmap = record
    rows:         Integer;
    width:        Integer;
    pitch:        Integer;
    buffer:       PByte;
    num_grays:    ShortInt;
    pixel_mode:   Byte;
    palette_mode: Byte;
    palette:      Pointer;
  end;

  FT_Outline = record
    n_contours:   ShortInt;
    n_points:     ShortInt;

    points:       ^FT_Vector;
    tags:         PByte;
    contours:     PShortInt;

    flags:        Integer;
  end;

  FT_GlyphSlotRec = record
    library_:           FT_Library;
    face:               FT_Face;
    next:               FT_GlyphSlot;
    reserved:           FT_UInt;
    generic_:           FT_Generic;

    metrics:            FT_Glyph_Metrics;
    linearHoriAdvance:  FT_Fixed;
    linearVertAdvance:  FT_Fixed;
    advance:            FT_Vector;

    format:             FT_Glyph_Format;

    bitmap:             FT_Bitmap;
    bitmap_left:        FT_Int;
    bitmap_top:         FT_Int;

    outline:            FT_Outline;

    num_subglyphs:      FT_UInt;
    subglyphs:          FT_SubGlyph;

    control_data:       Pointer;
    control_len:        LongInt;

    lsb_delta:          FT_Pos;
    rsb_delta:          FT_Pos;

    other:              Pointer;

    internal:           FT_Slot_Internal;
  end;

  FT_FaceRec = record
    num_faces:            FT_Long;
    face_index:           FT_Long;

    face_flags:           FT_Long;
    style_flags:          FT_Long;

    num_glyphs:           FT_Long;

    family_name:          PFT_String;
    style_name:           PFT_String;

    num_fixed_sizes:      FT_Int;
    available_sizes:      ^FT_Bitmap_Size;

    num_charmaps:         FT_Int;
    charmaps:             ^FT_CharMap;

    generic_:             FT_Generic;

    bbox:                 FT_BBox;

    units_per_EM:         FT_UShort;
    ascender:             FT_Short;
    descender:            FT_Short;
    height:               FT_Short;

    max_advance_width:    FT_Short;
    max_advance_height:   FT_Short;

    underline_position:   FT_Short;
    underline_thickness:  FT_Short;

    glyph:                FT_GlyphSlot;
    size:                 FT_Size;
    charmap:              FT_CharMap;

    { private }
    driver:               FT_Driver;
    memory:               FT_Memory;
    stream:               FT_Stream;
    sizes_list:           FT_ListRec;
    autohint:             FT_Generic;
    extensions:           Pointer;
    internal:             FT_Face_Internal;
    { private end }
  end;

  FT_SfntName = record
    platform_id:  FT_UShort;
    encoding_id:  FT_UShort;
    language_id:  FT_UShort;
    name_id:      FT_UShort;

    string_:      PByte;
    string_len:   FT_UInt;
  end;

  TT_OS2 = record
    version:        FT_UShort;
    xAvgCharWidth:  FT_Short;
    usWeightClass:  FT_UShort;
    usWidthClass:   FT_UShort;
    fsType:         FT_Short;

    ySubscriptXSize:      FT_Short;
    ySubscriptYSize:      FT_Short;
    ySubscriptXOffset:    FT_Short;
    ySubscriptYOffset:    FT_Short;
    ySuperscriptXSize:    FT_Short;
    ySuperscriptYSize:    FT_Short;
    ySuperscriptXOffset:  FT_Short;
    ySuperscriptYOffset:  FT_Short;
    yStrikeoutSize:       FT_Short;
    yStrikeoutPosition:   FT_Short;
    sFamilyClass:         FT_Short;

    panose: array[0..9] of FT_Byte;

    ulUnicodeRange1: FT_ULong;
    ulUnicodeRange3: FT_ULong;
    ulUnicodeRange2: FT_ULong;
    ulUnicodeRange4: FT_ULong;

    achVendID: array[0..3] of FT_Char;

    fsSelection:      FT_UShort;
    usFirstCharIndex: FT_UShort;
    usLastCharIndex:  FT_UShort;
    sTypoAscender:    FT_Short;
    sTypoDescender:   FT_Short;
    sTypoLineGap:     FT_Short;
    usWinAscent:      FT_UShort;
    usWinDescent:     FT_UShort;

    ulCodePageRange1: FT_ULong;
    ulCodePageRange2: FT_ULong;

    { only version 2 and higher: }
    sxHeight:       FT_Short;
    sCapHeight:     FT_Short;
    usDefaultChar:  FT_UShort;
    usBreakChar:    FT_UShort;
    usMaxContext:   FT_UShort;

    { only version 5 and higher: }
    usLowerOpticalPointSize: FT_UShort;
    usUpperOpticalPointSize: FT_UShort;
  end;

  TT_Postscript = record
    FormatType:         FT_Fixed;
    italicAngle:        FT_Fixed;
    underlinePosition:  FT_Short;
    underlineThickness: FT_Short;
    isFixedPitch:       FT_ULong;
    minMemType42:       FT_ULong;
    maxMemType42:       FT_ULong;
    minMemType1:        FT_ULong;
    maxMemType1:        FT_ULong;
  end;

  TT_HoriHeader = record
    Version:    FT_Fixed;
    Ascender:   FT_Short;
    Descender:  FT_Short;
    Line_Gap:   FT_Short;

    advance_Width_Max: FT_UShort;

    min_Left_Side_Bearin:   FT_Short;
    min_Right_Side_Bearing: FT_Short;
    xMax_Extent:            FT_Short;
    caret_Slope_Rise:       FT_Short;
    caret_Slope_Run:        FT_Short;
    caret_Offset:           FT_Short;

    Reserved: array[0..3] of FT_Short;

    metric_Data_Format: FT_Short;
    number_Of_HMetrics: FT_UShort;

    long_metrics: Pointer;
    short_metrics: Pointer;
  end;

  TFT_Init_FreeType   = function(aLibrary: PFT_Library): FT_Error;
  TFT_Done_FreeType   = function(aLibrary: FT_Library): FT_Error;
  TFT_New_Face        = function(aLibrary: FT_Library; const aFilename: PAnsiChar; aFaceIndex: FT_Long; aFace: PFT_Face): FT_Error;
  TFT_New_Memory_Face = function(aLibrary: FT_Library; aData: PByte; aSize: FT_Long; aFaceIndex: FT_Long; aFace: PFT_Face): FT_Error;
  TFT_Done_Face       = function(aFace: FT_Face): FT_Error;

  TFT_Get_Sfnt_Name_Count = function(aFace: FT_Face): FT_UInt;
  TFT_Get_Sfnt_Name       = function(aFace: FT_Face; aIndex: FT_UInt; aName: PFT_SfntName): FT_Error;

  TFT_Set_Char_Size  = function(aFace: FT_Face; aCharWidth: FT_F26Dot6; aCharHeight: FT_F26Dot6; aHorzDPI: FT_UInt; aVertDPI: FT_UInt): FT_Error;
  TFT_Load_Char      = function(aFace: FT_Face; aCharCode: FT_ULong; aLoadFlags: FT_Int32): FT_Error;
  TFT_Get_Sfnt_Table = function(aFace: FT_Face; aTag: Integer): Pointer;

var
  FT_Init_FreeType:   TFT_Init_FreeType;
  FT_Done_FreeType:   TFT_Done_FreeType;
  FT_New_Face:        TFT_New_Face;
  FT_New_Memory_Face: TFT_New_Memory_Face;
  FT_Done_Face:       TFT_Done_Face;

  FT_Get_Sfnt_Name_Count: TFT_Get_Sfnt_Name_Count;
  FT_Get_Sfnt_Name:       TFT_Get_Sfnt_Name;

  FT_Set_Char_Size:   TFT_Set_Char_Size;
  FT_Load_Char:       TFT_Load_Char;
  FT_Get_Sfnt_Table:  TFT_Get_Sfnt_Table;

const
  TT_NAME_ID_COPYRIGHT          = 0;
  TT_NAME_ID_FONT_FAMILY        = 1;
  TT_NAME_ID_FONT_SUBFAMILY     = 2;
  TT_NAME_ID_UNIQUE_ID          = 3;
  TT_NAME_ID_FULL_NAME          = 4;
  TT_NAME_ID_VERSION_STRING     = 5;
  TT_NAME_ID_PS_NAME            = 6;
  TT_NAME_ID_TRADEMARK          = 7;

  TT_PLATFORM_APPLE_UNICODE     = 0;
  TT_PLATFORM_MACINTOSH         = 1;
  TT_PLATFORM_ISO               = 2; // deprecated
  TT_PLATFORM_MICROSOFT         = 3;
  TT_PLATFORM_CUSTOM            = 4;
  TT_PLATFORM_ADOBE             = 7; // artificial

  TT_ISO_ID_7BIT_ASCII = 0;
  TT_ISO_ID_10646      = 1;
  TT_ISO_ID_8859_1     = 2;

  TT_APPLE_ID_DEFAULT           = 0; // Unicode 1.0
  TT_APPLE_ID_UNICODE_1_1       = 1; // specify Hangul at U+34xx
  TT_APPLE_ID_ISO_10646         = 2; // deprecated
  TT_APPLE_ID_UNICODE_2_0       = 3; // or later
  TT_APPLE_ID_UNICODE_32        = 4; // 2.0 or later, full repertoire

  TT_MAC_ID_ROMAN                =  0;
  TT_MAC_ID_JAPANESE             =  1;
  TT_MAC_ID_TRADITIONAL_CHINESE  =  2;
  TT_MAC_ID_KOREAN               =  3;
  TT_MAC_ID_ARABIC               =  4;
  TT_MAC_ID_HEBREW               =  5;
  TT_MAC_ID_GREEK                =  6;
  TT_MAC_ID_RUSSIAN              =  7;
  TT_MAC_ID_RSYMBOL              =  8;
  TT_MAC_ID_DEVANAGARI           =  9;
  TT_MAC_ID_GURMUKHI             = 10;
  TT_MAC_ID_GUJARATI             = 11;
  TT_MAC_ID_ORIYA                = 12;
  TT_MAC_ID_BENGALI              = 13;
  TT_MAC_ID_TAMIL                = 14;
  TT_MAC_ID_TELUGU               = 15;
  TT_MAC_ID_KANNADA              = 16;
  TT_MAC_ID_MALAYALAM            = 17;
  TT_MAC_ID_SINHALESE            = 18;
  TT_MAC_ID_BURMESE              = 19;
  TT_MAC_ID_KHMER                = 20;
  TT_MAC_ID_THAI                 = 21;
  TT_MAC_ID_LAOTIAN              = 22;
  TT_MAC_ID_GEORGIAN             = 23;
  TT_MAC_ID_ARMENIAN             = 24;
  TT_MAC_ID_MALDIVIAN            = 25;
  TT_MAC_ID_SIMPLIFIED_CHINESE   = 25;
  TT_MAC_ID_TIBETAN              = 26;
  TT_MAC_ID_MONGOLIAN            = 27;
  TT_MAC_ID_GEEZ                 = 28;
  TT_MAC_ID_SLAVIC               = 29;
  TT_MAC_ID_VIETNAMESE           = 30;
  TT_MAC_ID_SINDHI               = 31;
  TT_MAC_ID_UNINTERP             = 32;

  FT_LOAD_DEFAULT                     =   0;
  FT_LOAD_NO_SCALE                    = ( 1 shl  0 );
  FT_LOAD_NO_HINTING                  = ( 1 shl  1 );
  FT_LOAD_RENDER                      = ( 1 shl  2 );
  FT_LOAD_NO_BITMAP                   = ( 1 shl  3 );
  FT_LOAD_VERTICAL_LAYOUT             = ( 1 shl  4 );
  FT_LOAD_FORCE_AUTOHINT              = ( 1 shl  5 );
  FT_LOAD_CROP_BITMAP                 = ( 1 shl  6 );
  FT_LOAD_PEDANTIC                    = ( 1 shl  7 );
  FT_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH = ( 1 shl  9 );
  FT_LOAD_NO_RECURSE                  = ( 1 shl 10 );
  FT_LOAD_IGNORE_TRANSFORM            = ( 1 shl 11 );
  FT_LOAD_MONOCHROME                  = ( 1 shl 12 );
  FT_LOAD_LINEAR_DESIGN               = ( 1 shl 13 );
  FT_LOAD_NO_AUTOHINT                 = ( 1 shl 15 );
  FT_LOAD_COLOR                       = ( 1 shl 20 );

  FT_GLYPH_FORMAT_NONE        = 0;
  FT_GLYPH_FORMAT_COMPOSITE   = (Ord('c') shl 24) or
                                (Ord('o') shl 16) or
                                (Ord('m') shl  8) or
                                (Ord('p'));
  FT_GLYPH_FORMAT_BITMAP      = (Ord('b') shl 24) or
                                (Ord('i') shl 16) or
                                (Ord('t') shl  8) or
                                (Ord('s'));
  FT_GLYPH_FORMAT_OUTLINE     = (Ord('o') shl 24) or
                                (Ord('u') shl 16) or
                                (Ord('t') shl  8) or
                                (Ord('l'));
  FT_GLYPH_FORMAT_PLOTTER     = (Ord('p') shl 24) or
                                (Ord('l') shl 16) or
                                (Ord('o') shl  8) or
                                (Ord('t'));

  //FT_PIXEL_MODE_NONE  = 0;
  FT_PIXEL_MODE_MONO  = 0;
  FT_PIXEL_MODE_GRAY  = 1;
  FT_PIXEL_MODE_GRAY2 = 2;
  FT_PIXEL_MODE_GRAY4 = 3;
  FT_PIXEL_MODE_LCD   = 4;
  FT_PIXEL_MODE_LCD_V = 5;
  FT_PIXEL_MODE_BGRA  = 6;

  FT_ERR_Ok                                         = $00;
  FT_ERR_None                                       = $00;

  FT_ERR_Cannot_Open_Resource                       = $01;
  FT_ERR_Unknown_File_Format                        = $02;
  FT_ERR_Invalid_File_Format                        = $03;
  FT_ERR_Invalid_Version                            = $04;
  FT_ERR_Lower_Module_Version                       = $05;
  FT_ERR_Invalid_Argument                           = $06;
  FT_ERR_Unimplemented_Feature                      = $07;
  FT_ERR_Invalid_Table                              = $08;
  FT_ERR_Invalid_Offset                             = $09;
  FT_ERR_Array_Too_Large                            = $0A;

  { glyph/character errors }
  FT_ERR_Invalid_Glyph_Index                        = $10;
  FT_ERR_Invalid_Character_Code                     = $11;
  FT_ERR_Invalid_Glyph_Format                       = $12;
  FT_ERR_Cannot_Render_Glyph                        = $13;
  FT_ERR_Invalid_Outline                            = $14;
  FT_ERR_Invalid_Composite                          = $15;
  FT_ERR_Too_Many_Hints                             = $16;
  FT_ERR_Invalid_Pixel_Size                         = $17;

  { handle errors }
  FT_ERR_Invalid_Handle                             = $20;
  FT_ERR_Invalid_Library_Handle                     = $21;
  FT_ERR_Invalid_Driver_Handle                      = $22;
  FT_ERR_Invalid_Face_Handle                        = $23;
  FT_ERR_Invalid_Size_Handle                        = $24;
  FT_ERR_Invalid_Slot_Handle                        = $25;
  FT_ERR_Invalid_CharMap_Handle                     = $26;
  FT_ERR_Invalid_Cache_Handle                       = $27;
  FT_ERR_Invalid_Stream_Handle                      = $28;

  { driver errors }
  FT_ERR_Too_Many_Drivers                           = $30;
  FT_ERR_Too_Many_Extensions                        = $31;

  { memory errors }
  FT_ERR_Out_Of_Memory                              = $40;
  FT_ERR_Unlisted_Object                            = $41;

  { stream errors }
  FT_ERR_Cannot_Open_Stream                         = $51;
  FT_ERR_Invalid_Stream_Seek                        = $52;
  FT_ERR_Invalid_Stream_Skip                        = $53;
  FT_ERR_Invalid_Stream_Read                        = $54;
  FT_ERR_Invalid_Stream_Operation                   = $55;
  FT_ERR_Invalid_Frame_Operation                    = $56;
  FT_ERR_Nested_Frame_Access                        = $57;
  FT_ERR_Invalid_Frame_Read                         = $58;

  { raster errors }
  FT_ERR_Raster_Uninitialized                       = $60;
  FT_ERR_Raster_Corrupted                           = $61;
  FT_ERR_Raster_Overflow                            = $62;
  FT_ERR_Raster_Negative_Height                     = $63;

  { cache errors }
  FT_ERR_Too_Many_Caches                            = $70;

  { TrueType and SFNT errors }
  FT_ERR_Invalid_Opcode                             = $80;
  FT_ERR_Too_Few_Arguments                          = $81;
  FT_ERR_Stack_Overflow                             = $82;
  FT_ERR_Code_Overflow                              = $83;
  FT_ERR_Bad_Argument                               = $84;
  FT_ERR_Divide_By_Zero                             = $85;
  FT_ERR_Invalid_Reference                          = $86;
  FT_ERR_Debug_OpCode                               = $87;
  FT_ERR_ENDF_In_Exec_Stream                        = $88;
  FT_ERR_Nested_DEFS                                = $89;
  FT_ERR_Invalid_CodeRange                          = $8A;
  FT_ERR_Execution_Too_Long                         = $8B;
  FT_ERR_Too_Many_Function_Defs                     = $8C;
  FT_ERR_Too_Many_Instruction_Defs                  = $8D;
  FT_ERR_Table_Missing                              = $8E;
  FT_ERR_Horiz_Header_Missing                       = $8F;
  FT_ERR_Locations_Missing                          = $90;
  FT_ERR_Name_Table_Missing                         = $91;
  FT_ERR_CMap_Table_Missing                         = $92;
  FT_ERR_Hmtx_Table_Missing                         = $93;
  FT_ERR_Post_Table_Missing                         = $94;
  FT_ERR_Invalid_Horiz_Metrics                      = $95;
  FT_ERR_Invalid_CharMap_Format                     = $96;
  FT_ERR_Invalid_PPem                               = $97;
  FT_ERR_Invalid_Vert_Metrics                       = $98;
  FT_ERR_Could_Not_Find_Context                     = $99;
  FT_ERR_Invalid_Post_Table_Format                  = $9A;
  FT_ERR_Invalid_Post_Table                         = $9B;

  { CFF CID and Type 1 errors }
  FT_ERR_Syntax_Error                               = $A0;
  FT_ERR_Stack_Underflow                            = $A1;
  FT_ERR_Ignore                                     = $A2;

  { BDF errors }
  FT_ERR_Missing_Startfont_Field                    = $B0;
  FT_ERR_Missing_Font_Field                         = $B1;
  FT_ERR_Missing_Size_Field                         = $B2;
  FT_ERR_Missing_Chars_Field                        = $B3;
  FT_ERR_Missing_Startchar_Field                    = $B4;
  FT_ERR_Missing_Encoding_Field                     = $B5;
  FT_ERR_Missing_Bbx_Field                          = $B6;
  FT_ERR_Bbx_Too_Big                                = $B7;
  FT_ERR_Corrupted_Font_Header                      = $B8;
  FT_ERR_Corrupted_Font_Glyphs                      = $B9;

  FT_STYLE_FLAG_ITALIC = (1 shl 0);
  FT_STYLE_FLAG_BOLD   = (1 shl 1);

  FT_RENDER_MODE_NORMAL = 0;
  FT_RENDER_MODE_LIGHT  = 1;
  FT_RENDER_MODE_MONO   = 2;
  FT_RENDER_MODE_LCD    = 3;
  FT_RENDER_MODE_LCD_V  = 4;

  FT_LOAD_TARGET_NORMAL = FT_RENDER_MODE_NORMAL shl 16;
  FT_LOAD_TARGET_LIGHT  = FT_RENDER_MODE_LIGHT  shl 16;
  FT_LOAD_TARGET_MONO   = FT_RENDER_MODE_MONO   shl 16;
  FT_LOAD_TARGET_LCD    = FT_RENDER_MODE_LCD    shl 16;
  FT_LOAD_TARGET_LCD_V  = FT_RENDER_MODE_LCD_V  shl 16;

  FT_SFNT_HEAD  = 0;
  FT_SFNT_MAXP  = 1;
  FT_SFNT_OS2   = 2;
  FT_SFNT_HHEA  = 3;
  FT_SFNT_VHEA  = 4;
  FT_SFNT_POST  = 5;
  FT_SFNT_PCLT  = 6;

function InitFreeType: FT_Library;
procedure QuitFreeType;

implementation

{$IFDEF WINDOWS}
  {$IFDEF WIN32}
    {$DEFINE TS_FT_WIN32}
  {$ELSE}
    {$DEFINE TS_FT_WIN64}
  {$ENDIF}
{$ELSE}
  {$DEFINE TS_FT_LINUX}
{$ENDIF}

const
{$IF DEFINED(TS_FT_WIN32)}
  LIB_FREE_TYPE = 'freetype6-x86.dll';
{$ELSEIF DEFINED(TS_FT_WIN64)}
  LIB_FREE_TYPE = 'freetype6-x64.dll';
{$ELSEIF DEFINED(TS_FT_LINUX)}
  LIB_FREE_TYPE = 'libfreetype.so';
{$ELSE}
  {$ERROR 'unknown/unsupported OS'}
{$IFEND}

var
  FreeTypeInitialized: Boolean;
  FreeTypeRefCount: Integer;
  FreeTypeCritSec: TCriticalSection;
  FreeTypeLibHandle: TLibHandle = 0;

  ftLibrary: FT_Library;

function InitFreeType: FT_Library;

  function GetProcAddr(const aName: String): Pointer;
  begin
    result := GetProcAddress(FreeTypeLibHandle, aName);
    if not Assigned(result) then
      raise EtsException.Create('unable to load procedure from library: ' + aName);
  end;

var
  err: FT_Error;
begin
  result := nil;
  FreeTypeCritSec.Enter;
  try try
    inc(FreeTypeRefCount, 1);
    if FreeTypeInitialized then
      exit;

    if (FreeTypeLibHandle = 0) then begin
      FreeTypeLibHandle := LoadLibrary(LIB_FREE_TYPE);
      if (FreeTypeLibHandle = 0) then
        raise EtsException.Create('unable to load free type lib: ' + LIB_FREE_TYPE + ' error=' + IntToStr(GetLastOSError));
    end;

    FT_Init_FreeType   := TFT_Init_FreeType(  GetProcAddr('FT_Init_FreeType'));
    FT_Done_FreeType   := TFT_Done_FreeType(  GetProcAddr('FT_Done_FreeType'));
    FT_New_Face        := TFT_New_Face(       GetProcAddr('FT_New_Face'));
    FT_New_Memory_Face := TFT_New_Memory_Face(GetProcAddr('FT_New_Memory_Face'));
    FT_Done_Face       := TFT_Done_Face(      GetProcAddr('FT_Done_Face'));

    FT_Get_Sfnt_Name_Count := TFT_Get_Sfnt_Name_Count(GetProcAddr('FT_Get_Sfnt_Name_Count'));
    FT_Get_Sfnt_Name       := TFT_Get_Sfnt_Name(      GetProcAddr('FT_Get_Sfnt_Name'));

    FT_Set_Char_Size  := TFT_Set_Char_Size( GetProcAddr('FT_Set_Char_Size'));
    FT_Load_Char      := TFT_Load_Char(     GetProcAddr('FT_Load_Char'));
    FT_Get_Sfnt_Table := TFT_Get_Sfnt_Table(GetProcAddr('FT_Get_Sfnt_Table'));

    err := FT_Init_FreeType(@ftLibrary);
    if (err <> 0) then
      raise EtsException.Create('unable to create free type library handle: ' + IntToStr(err));

    FreeTypeInitialized := true;
    result := ftLibrary;
  except
    FreeTypeInitialized := false;
  end;
  finally
    FreeTypeCritSec.Leave;
  end;
end;

procedure QuitFreeType;
begin
  FreeTypeCritSec.Enter;
  try
    dec(FreeTypeRefCount, 1);
    if (FreeTypeRefCount > 0) then
      exit;

    FT_Done_FreeType(ftLibrary);

    FT_Init_FreeType   := nil;
    FT_Done_FreeType   := nil;
    FT_New_Face        := nil;
    FT_New_Memory_Face := nil;
    FT_Done_Face       := nil;

    FT_Get_Sfnt_Name_Count := nil;
    FT_Get_Sfnt_Name       := nil;

    FT_Set_Char_Size  := nil;
    FT_Load_Char      := nil;
    FT_Get_Sfnt_Table := nil;

    if (FreeTypeLibHandle <> 0) then begin
      FreeLibrary(FreeTypeLibHandle);
      FreeTypeLibHandle := 0;
    end;
    FreeTypeInitialized := false;
  finally
    FreeTypeCritSec.Leave;
  end;
end;

initialization
  FreeTypeRefCount    := 0;
  FreeTypeInitialized := false;
  FreeTypeCritSec     := TCriticalSection.Create;

finalization
  if FreeTypeInitialized then
    QuitFreeType;
  FreeAndNil(FreeTypeCritSec);

end.

