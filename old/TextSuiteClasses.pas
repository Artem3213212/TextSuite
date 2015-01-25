{
TextSuite (C) Steffen Xonna (aka Lossy eX)
http://www.opengl24.de/
-----------------------------------------------------------------------
For copyright informations see file copyright.txt.
}

{$WARNINGS OFF}
{$HINTS OFF}

{$I TextSuiteOptions.inc}

unit TextSuiteClasses;

interface


uses
  Classes,

  TextSuite,
  TextSuiteWideUtils,
  TextSuiteImports;


{ intern types for Renderer }
const
  TS_BLOCK_FONT = $1;
  TS_BLOCK_COLOR = $2;
  TS_BLOCK_WORD = $3;
  TS_BLOCK_SPACE = $4;
  TS_BLOCK_LINEBREAK = $5;
  TS_BLOCK_TAB = $6;


type
  TtsFontStyle = (tsStyleBold, tsStyleItalic, tsStyleUnderline, tsStyleStrikeout);
  TtsFontStyles = set of TtsFontStyle;

  TtsAntiAliasing = (tsAANone, tsAANormal);
  TtsFormat = (tsFormatEmpty, tsFormatRGBA8);

  TtsImageMode = (tsModeRed, tsModeGreen, tsModeBlue, tsModeAlpha, tsModeLuminance);
  TtsImageModes = array [TtsImageMode] of tsEnum;


  tsQuad = array[0..3] of tsPoint;

  tsPointFloat = packed record
    X: Single;
    Y: Single;
  end;
  tsQuadFloat = array[0..3] of tsPointFloat;


const
  cModesReplace : TtsImageModes = (TS_MODE_REPLACE, TS_MODE_REPLACE, TS_MODE_REPLACE, TS_MODE_REPLACE, TS_MODE_REPLACE);
  cModesNormal : TtsImageModes = (TS_MODE_REPLACE, TS_MODE_REPLACE, TS_MODE_REPLACE, TS_MODE_MODULATE, TS_MODE_REPLACE);

type
  PtsHashEntry = ^TtsHashEntry;
  TtsHashEntry = record
    Name: Integer;
    Value: Pointer;
    Next: PtsHashEntry;
  end;

  TtsHash = class(TObject)
  private
    fHashArray: array of PtsHashEntry;
    fHashEntrys: Integer;
    fCount: Integer;

    function IntToPos(Name: Integer): Integer;
  public
    property Count: Integer read fCount;

    constructor Create(HashEntrys: Integer);
    destructor Destroy; override;

    procedure Add(Name: Integer; Value: Pointer);
    procedure Delete(Name: Integer);
    procedure Clear;

    function Get(Name: Integer): Pointer;
    procedure GetNames(const NameList: TList);
    procedure GetValues(const ValueList: TList);
  end;


  PtsStringHashEntry = ^TtsStringHashEntry;
  TtsStringHashEntry = record
    pString: pWideChar;
    Next: PtsStringHashEntry;
  end;

  TtsStringHash = class(TObject)
  private
    fHashArray: array of PtsStringHashEntry;
    fHashEntrys: Cardinal;
  public
    constructor Create(HashEntrys: Integer);
    destructor Destroy; override;

    procedure Add(pString: pWideChar);
    function Delete(pString: pWideChar): Boolean;
  end;


  TtsKernel1DItem = packed record
    Offset: Integer;
    Value: Single;

    DataOffset: Integer;
  end;

  TtsKernel1D = class
  public
    Size: Integer;
    ValueSum: Double;

    Items: array of TtsKernel1DItem;
    ItemCount: Integer;

    constructor Create(Radius, Strength: Single);

    procedure UpdateDataOffset(DataSize: Integer);
  end;


  TtsKernel2DItem = packed record
    OffsetX: Integer;
    OffsetY: Integer;
    Value: Single;

    DataOffset: Integer;
  end;

  TtsKernel2D = class
  public
    SizeX: Integer;
    SizeY: Integer;

    MidSizeX: Integer;
    MidSizeY: Integer;

    ValueSum: Double;

    Items: array of TtsKernel2DItem;
    ItemCount: Integer;

    constructor Create(Radius, Strength: Single);

    procedure UpdateDataOffset(DataSizeX, DataSizeY: Integer);
  end;


  TtsImage = class;
  TtsRenderer = class;
  TtsRendererImageReference = class;
  TtsContext = class;
  TtsChar = class;


  TtsImageFunc = procedure(Image: TtsImage; X, Y: Integer; var Pixel: tsColor; Data: Pointer);

  TtsImage = class
  private
    fWidth: Integer;
    fHeight: Integer;
    fFormat: TtsFormat;

    fData: Pointer;
    fScanLinesValid: Boolean;
    fScanLines: array of Pointer;

    procedure SetDataPtr(aData: Pointer; aFormat: TtsFormat = tsFormatEmpty; aWidth: Integer = 0; aHeight: Integer = 0);

    function GetFormatSize(Format: TtsFormat): Integer;

    procedure UpdateScanLines;
    function GetScanLine(Index: Integer): pointer;
    function GetEmpty: Boolean;
  public
    procedure BeforeDestruction; override;

    procedure AssignFrom(Image: TtsImage);
    procedure CreateEmpty(Format: TtsFormat; aWidth, aHeight: Integer);
    procedure LoadFromFile(FileName: PAnsiChar);

    procedure Resize(NewWidth, NewHeight, X, Y: Integer);
    procedure FindMinMax(var MinMaxInfo: tsRect);

    procedure AddFunc(Func: TtsImageFunc; Data: Pointer);

    procedure FillColor(Red, Green, Blue, Alpha: Single; ChannelMask: tsBitmask; Modes: TtsImageModes);
    procedure FillPattern(Pattern: TtsImage; X, Y: Integer; ChannelMask: tsBitmask; Modes: TtsImageModes);

    procedure BlendImage(Image: TtsImage; X, Y: Integer; AutoExpand: Boolean = True);
    procedure Blur(HorzKernel, VertKernel: TtsKernel1D; ChannelMask: tsBitmask);

    procedure AddResizingBorder(tsChar: TtsChar);

    property Empty: Boolean read GetEmpty;

    property Data: Pointer read fData;
    property Width: Integer read fWidth;
    property Height: Integer read fHeight;
    property Format: TtsFormat read fFormat;

    property ScanLine[Index: Integer]: pointer read GetScanline;
  end;


  TtsChar = class
  protected
    // CharCode
    fCharCode: WideChar;
  public
    // Position of char
    GlyphOriginX: Smallint;
    GlyphOriginY: Smallint;
    Advance: SmallInt;
    GlyphRect: tsRect;

    HasResizingBorder: Boolean;

    // Kerning
//    KerningValuesLeft: array of WORD;
//    KerningValuesRight: array of WORD;

    // Renderer data for Imagehandling
    RendererImageReference: TtsRendererImageReference;

    constructor Create(CharCode: WideChar);
    destructor Destroy; override;

    procedure ExpandRect(Left, Top, Right, Bottom: Integer);

    // Kerning
//    procedure CalculateKerningData(CharImage: TtsImage);
//    function CalculateKerningValue(LastChar: TtsChar): Smallint;

    // CharCode
    property CharCode: WideChar read fCharCode;
  end;


  PtsFontCharArray = ^TtsFontCharArray;
  TtsFontCharArray = packed record
    Chars: array [Byte] of TtsChar;
    CharCount: Byte;
  end;


  TtsTextMetric = record
    Ascent: Integer;
    Descent: Integer;
    LineSkip: Integer;
    LineSkip_with_LineSpace: Integer;
  end;


  TtsFont = class
  private
    // Strings
    fCopyright: AnsiString;
    fFaceName: AnsiString;
    fStyleName: AnsiString;
    fFullName: AnsiString;

    // font styles
    fSize: Integer;
    fStyle: TtsFontStyles;
    fFormat: TtsFormat;
    fAntiAliasing: TtsAntiAliasing;

    // font settings
    fAscent: Integer;
    fDescent: Integer;
    fExternalLeading: Integer;
    fBaselineOffset: Integer;

    fDefaultChar: WideChar;

    fFontFileStyle: Integer;
    fFixedWidth: Boolean;

    fCharSpacing: Integer;
    fLineSpacing: Integer;

    fUnderlinePosition: Integer;
    fUnderlineSize: Integer;
    fStrikeoutPosition: Integer;
    fStrikeoutSize: Integer;

    // chars
    fChars: array [Byte] of PtsFontCharArray;
  protected
    fRenderer: TtsRenderer;

    function Validate(CharCode: WideChar): Boolean; virtual;

    procedure AddChar(CharCode: WideChar; Char: TtsChar);
    function GetChar(CharCode: WideChar): TtsChar;
  public
    // chars
    property Char[CharCode: WideChar]: TtsChar read GetChar;

    // strings
    property Copyright: AnsiString read fCopyright write fCopyright;
    property FaceName: AnsiString read fFaceName write fFaceName;
    property StyleName: AnsiString read fStyleName write fStyleName;
    property FullName: AnsiString read fFullName write fFullName;

    property Size: Integer read fSize write fSize;
    property Style: TtsFontStyles read fStyle write fStyle;
    property Format: TtsFormat read fFormat write fFormat;
    property AntiAliasing: TtsAntiAliasing read fAntiAliasing write fAntiAliasing;

    // Font propertys
    property Ascent: Integer read fAscent write fAscent;
    property Descent: Integer read fDescent write fDescent;
    property ExternalLeading: Integer read fExternalLeading write fExternalLeading;
    property BaselineOffset: Integer read fBaselineOffset write fBaselineOffset;

    property DefaultChar: WideChar read fDefaultChar write fDefaultChar;

    property FontFileStyle: Integer read fFontFileStyle write fFontFileStyle;
    property FixedWidth: Boolean read fFixedWidth write fFixedWidth;

    property CharSpacing: Integer read fCharSpacing write fCharSpacing;
    property LineSpacing: Integer read fLineSpacing write fLineSpacing;

    property UnderlinePosition: Integer read fUnderlinePosition write fUnderlinePosition;
    property UnderlineSize: Integer read fUnderlineSize write fUnderlineSize;
    property StrikeoutPosition: Integer read fStrikeoutPosition write fStrikeoutPosition;
    property StrikeoutSize: Integer read fStrikeoutSize write fStrikeoutSize;

    constructor Create(Renderer: TtsRenderer; Size: Integer; Style: TtsFontStyles; Format: TtsFormat; AntiAliasing: TtsAntiAliasing);
    destructor Destroy; override;

    procedure ClearChars;

    procedure DeleteChar(CharCode: WideChar);

    procedure GetTextMetric(var Metric: TtsTextMetric);
  end;


  PtsPostProcessStepRange = ^TtsPostProcessStepRange;
  TtsPostProcessStepRange = record
    StartChar: WideChar;
    EndChar: WideChar;
  end;

  TtsFontProcessStepUsage = (tsUInclude, tsUExclude);

  TtsPostProcessStep = class
  protected
    fIncludeCharRange: TList;
    fExcludeCharRange: TList;

    procedure ClearList(List: TList);

    procedure PostProcess(const CharImage: TtsImage; const Char: TtsChar); virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    function IsInRange(CharCode: WideChar): Boolean;
    procedure AddUsageRange(Usage: TtsFontProcessStepUsage; StartChar, EndChar: WideChar);
    procedure AddUsageChars(Usage: TtsFontProcessStepUsage; Chars: pWideChar);

    procedure ClearIncludeRange;
    procedure ClearExcludeRange;
  end;


  TtsFontCreator = class(TtsFont)
  private
    fPostProcessSteps: TList;

    function GetPostProcessStepCount: Integer;
    function GetPostProcessStep(Index: Integer): TtsPostProcessStep;
  protected
    fCreateChars: Boolean;
    fAddResizingBorder: Boolean;

    function Validate(CharCode: WideChar): Boolean; override;

    function GetGlyphMetrics(CharCode: WideChar; var GlyphOriginX, GlyphOriginY, GlyphWidth, GlyphHeight, Advance: Integer): Boolean; virtual; abstract;

    procedure GetCharImage(CharCode: WideChar; const CharImage: TtsImage); virtual; abstract;

    procedure DrawLine(Char: TtsChar; CharImage: TtsImage; LinePosition, LineSize: Integer);
    procedure DoPostProcess(var CharImage: TtsImage; const tsChar: TtsChar);
  public
    property CreateChars: Boolean read fCreateChars write fCreateChars;
    property AddResizingBorder: Boolean read fAddResizingBorder write fAddResizingBorder;

    constructor Create(Renderer: TtsRenderer; Size: Integer; Style: TtsFontStyles; Format: TtsFormat; AntiAliasing: TtsAntiAliasing);
    destructor Destroy; override;

    procedure AddChar(CharCode: WideChar); overload;

    function AddPostProcessStep(PostProcessStep: TtsPostProcessStep): TtsPostProcessStep;
    procedure DeletePostProcessStep(Index: Integer);
    procedure ClearPostProcessSteps;

    property PostProcessStepCount: Integer read GetPostProcessStepCount;
    property PostProcessStep[Index: Integer]: TtsPostProcessStep read GetPostProcessStep;
  end;


  TtsFontCreatorSDL = class(TtsFontCreator)
  protected
    fSDLFont: PTTF_Font;

    function GetGlyphMetrics(CharCode: WideChar; var GlyphOriginX, GlyphOriginY, GlyphWidth, GlyphHeight, Advance: Integer): Boolean; override;

    procedure GetCharImage(CharCode: WideChar; const CharImage: TtsImage); override;
  public
    constructor Create(Renderer: TtsRenderer; const Filename: AnsiString; Size: Integer; Style: TtsFontStyles; Format: TtsFormat; AntiAliasing: TtsAntiAliasing);
    destructor Destroy; override;
  end;


  TtsFontCreatorGDIFontFace = class(TtsFontCreator)
  protected
    fFontHandle: THandle;
    fMat2: TMat2;

    fFontname: AnsiString;

    function GetGlyphIndex(CharCode: WideChar): Integer;

    function GetGlyphMetrics(CharCode: WideChar; var GlyphOriginX, GlyphOriginY, GlyphWidth, GlyphHeight, Advance: Integer): Boolean; override;

    procedure GetCharImageAntialiased(DC: HDC; CharCode: WideChar; const CharImage: TtsImage);
    procedure GetCharImageNone(DC: HDC; CharCode: WideChar; const CharImage: TtsImage);

    procedure GetCharImage(CharCode: WideChar; const CharImage: TtsImage); override;
  public
    constructor Create(Renderer: TtsRenderer; const Fontname: AnsiString; Size: Integer; Style: TtsFontStyles; Format: TtsFormat; AntiAliasing: TtsAntiAliasing);
    destructor Destroy; override;
  end;


  TtsFontCreatorGDIFile = class(TtsFontCreatorGDIFontFace)
  protected
    fFilename: pAnsiChar;
    fFontRegistred: Boolean;

    function RegisterFont(Filename: pAnsiChar; RegisterPublic: Boolean): boolean;
    function UnRegisterFont(Filename: pAnsiChar; RegisterPublic: Boolean): boolean;

    function GetFaceName(Filename: pAnsiChar; var Face: AnsiString): boolean;
  public
    constructor Create(Renderer: TtsRenderer; const Filename: AnsiString; Size: Integer; Style: TtsFontStyles; Format: TtsFormat; AntiAliasing: TtsAntiAliasing);
    destructor Destroy; override;
  end;

  TtsFontCreatorGDIStream = class(TtsFontCreatorGDIFontFace)
  protected
    fFontRegistred: Boolean;
    fHandle: THandle;

    function RegisterFont(Data: TStream): boolean;
    function UnRegisterFont(): boolean;

    function GetFaceName(Stream: TStream; var Face: AnsiString): boolean;
  public
    constructor Create(Renderer: TtsRenderer; const Source: TStream; Size: Integer; Style: TtsFontStyles; Format: TtsFormat; AntiAliasing: TtsAntiAliasing);
    destructor Destroy; override;
  end;

  PtsLineItem = ^TtsLineItem;
  TtsLineItem = record
    NextItem: PtsLineItem;
    PrevItem: PtsLineItem;

    ItemType: Integer;
    case Integer of
      TS_BLOCK_FONT: (
        Font: TtsFont;
        FontID: tsFontID;
      );

      TS_BLOCK_COLOR: (
        Red: Single;
        Green: Single;
        Blue: Single;
        Alpha: Single;
      );

      TS_BLOCK_WORD, TS_BLOCK_SPACE: (
        Word: PWideChar;
        WordLength: Integer;
      );
  end;

  PtsLinesItem = ^TtsLinesItem;
  TtsLinesItem = record
    NextLine: PtsLinesItem;

    LineItemFirst: PtsLineItem;
    LineItemLast: PtsLineItem;

    LineLength: Integer;
    LineAutoBreak: Boolean;
  end;

  TtsTempLines = record
    Lines: PtsLinesItem;
    Empty: Boolean;
  end;


  { ***  *** }
  TtsRendererImageReference = class
  end;


  TtsRenderer = class
  private
    fContext: TtsContext;

    fSaveImages: Boolean;

    fisBlock: Boolean;
    fBlockLeft: Integer;
    fBlockTop: Integer;
    fBlockWidth: Integer;
    fBlockHeight: Integer;
    fFlags: Integer;

    fWordWrap: Boolean;
//    fSingleLine: Boolean;

    fActiveFont: TtsFont;
    fActiveFontID: Cardinal;
    fLastActiveFont: TtsFont;
    fLastActiveFontID: Cardinal;

    fLinesFirst: PtsLinesItem;
    fLinesLast: PtsLinesItem;
    fLinesTemp: TtsTempLines;

    // drawings
    fLineTop: Integer;
    fTextOffsetY: Integer;
    fTextOffsetX: Integer;

    function GetActiveFont: TtsFont;
    function GetActiveFontID: Cardinal;

    function SplitText(pText: PWideChar): PtsLineItem;
    procedure CalculateWordLength(Font: TtsFont; pWord: PtsLineItem);
    procedure SplitIntoLines(pItemList: PtsLineItem);

    procedure DrawLine(pLine: PtsLineItem; LineLength: Integer; LineBreak: Boolean);
    procedure DrawLines(pLinesItem: PtsLinesItem);
    function CalculateLinesHeight(pLinesItem: PtsLinesItem): Integer;

    procedure GetLineMetric(pLine: PtsLineItem; var Metric: TtsTextMetric);

    procedure PushLineItem(pLine: PtsLineItem);
    procedure FreeLineItems(var pLine: PtsLineItem);

    procedure PushTempLines;
    procedure FreeLines(var pLinesItem: PtsLinesItem);
    procedure TrimSpaces(pLinesItem: PtsLinesItem);
  protected
    procedure DrawChar(Font: TtsFont; Char: TtsChar); virtual; abstract;
    procedure DrawSetPosition(X, Y: Integer); virtual; abstract;
    procedure DrawSetPositionRelative(X, Y: Integer); virtual; abstract;
    procedure DrawSetColor(Red, Green, Blue, Alpha: Single); virtual; abstract;

    function AddImage(Char: TtsChar; CharImage: TtsImage): TtsRendererImageReference; virtual; abstract;
    procedure RemoveImageReference(ImageReference: TtsRendererImageReference); virtual; abstract;
  public
    property ActiveFont: TtsFont read GetActiveFont;
    property ActiveFontID: Cardinal read GetActiveFontID;

    property SaveImages: Boolean read fSaveImages write fSaveImages;

    property isBlock: Boolean read FisBlock;

    constructor Create(Context: TtsContext);
    destructor Destroy; override;

    procedure BeginBlock(Left, Top, Width, Height: Integer; Flags: tsBitmask); virtual;
    procedure EndBlock;

    procedure FontActivate(FontID: Cardinal);
    procedure Color(Red, Green, Blue, Alpha: Single);
    procedure TextOut(pText: pWideChar);

    function TextGetWidth(pText: pWideChar): Integer;
    function TextGetDrawWidth: Integer;
    function TextGetDrawHeight: Integer;

    procedure CharOut(CharCode: WideChar);
  end;


  TtsRendererNULLImageReference = class(TtsRendererImageReference)
    Image: TtsImage;
  end;


  TtsRendererNULL = class(TtsRenderer)
  protected
    procedure DrawChar(Font: TtsFont; Char: TtsChar); override;
    procedure DrawSetPosition(X, Y: Integer); override;
    procedure DrawSetPositionRelative(X, Y: Integer); override;
    procedure DrawSetColor(Red, Green, Blue, Alpha: Single); override;

    function AddImage(Char: TtsChar; CharImage: TtsImage): TtsRendererImageReference; override;
    procedure RemoveImageReference(ImageReference: TtsRendererImageReference); override;
  end;



  TtsRendererOpenGLImageReference = class(TtsRendererImageReference)
    TexID: Integer;
    Coordinates: tsRect;

    TexCoords: tsQuadFloat;
    Vertex: tsQuadFloat;
  end;


  PtsRendererOpenGLTexture = ^TtsRendererOpenGLTexture;
  TtsRendererOpenGLTexture = record
    glTextureID: Cardinal;

    Width: Integer;
    Height: Integer;
  end;


  PtsRendererOpenGLManagedEntry = ^TtsRendererOpenGLManagedEntry;
  TtsRendererOpenGLManagedEntry = record
    Start: Word;
    Count: Word;

    NextEntry: PtsRendererOpenGLManagedEntry;
  end;


  PtsRendererOpenGLTextureEntry = ^TtsRendererOpenGLTextureEntry;
  TtsRendererOpenGLTextureEntry = record
    ID: Integer;
    Texture: PtsRendererOpenGLTexture;

    Lines: array of PtsRendererOpenGLManagedEntry;
    Usage: Integer;
  end;


  TtsRendererOpenGL = class(TtsRenderer)
  private
    fPos: tsPoint;

    fTextureSize: Integer;

    // Texture
    fTextures: TList;

    procedure AllocSpace(var FirstManaged: PtsRendererOpenGLManagedEntry; Start, Count: Word);
    procedure FreeSpace(var FirstManaged: PtsRendererOpenGLManagedEntry; Start, Count: Word);

    function GetTextureByID(ID: Integer): PtsRendererOpenGLTexture;

    function AddImageToTexture(Texture: PtsRendererOpenGLTextureEntry; Image: TtsImage; var TextureID: Integer; var Coordinates: tsRect): boolean;
    function CreateNewTexture: PtsRendererOpenGLTextureEntry;

    procedure DeleteTexture(Idx: Integer);
    procedure ClearTextures;
  protected
    procedure DrawChar(Font: TtsFont; Char: TtsChar); override;
    procedure DrawSetPosition(X, Y: Integer); override;
    procedure DrawSetPositionRelative(X, Y: Integer); override;
    procedure DrawSetColor(Red, Green, Blue, Alpha: Single); override;

    function AddImage(Char: TtsChar; CharImage: TtsImage): TtsRendererImageReference; override;
    procedure RemoveImageReference(ImageReference: TtsRendererImageReference); override;
  public
    property TextureSize: Integer read fTextureSize write fTextureSize;

    procedure BeginBlock(Left, Top, Width, Height: Integer; Flags: tsBitmask); override;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;
  

  // context structures/types for use in unit TextSuite
  TtsContext = class
  private
    fContextID: Cardinal;

    // Fonts
    fFonts: TtsHash;
    fLastFontID: Cardinal;

    // Images
    fImages: TtsHash;
    fLastImageID: Cardinal;

    function GetIsLocked: boolean;

    procedure ClearFonts;
    procedure ClearImages;
    function GetActiveFont: TtsFont;
  public
    // ThreadID
    gBoundThreadID: Cardinal;

    // error
    Error: Cardinal;

    // globals settings
    Renderer: TtsRenderer;
    gCreator: tsEnum;
    gGlobalFormat: tsEnum;
    gGlobalAntiAliasing: tsEnum;

    gDebugDrawCharRects: Boolean;

    gEmptyCodePageEntry: tsEnum;
    gCodePage: tsEnum;
    gCodePagePtr: Pointer;
    gCodePageFunc: TtsAnsiToWideCharFunc;

    gSingleLine: tsEnum;
    gAlign: tsEnum;
    gVAlign: tsEnum;
    gClip: tsEnum;
    gBlockOffsetX: tsInt;
    gBlockOffsetY: tsInt;

    gImageMode: TtsImageModes;
    gImageLibrary: tsEnum;

{    Tab: tsEnum;
    TabWidth: tsInt; }

    // context specific / helper
    property ContextID: Cardinal read fContextID;

    property IsLocked: boolean read GetIsLocked;

    property ActiveFont: TtsFont read GetActiveFont;

    // helper functions
    function ImageAdd(Image: TtsImage): Cardinal;
    function ImageGet(Image: Cardinal): TtsImage;
    procedure ImageDelete(Image: Cardinal);
    function ImageCount: Cardinal;

    function FontAdd(Font: TtsFont): Cardinal;
    function FontGet(Font: Cardinal): TtsFont;
    procedure FontDelete(Font: Cardinal);
    function FontCount: Cardinal;

    function AnsiToWide(pText: pAnsiChar): pWideChar;

    constructor Create;
    destructor Destroy; override;
  end;


  PtsContextFontEntry = ^TtsContextFontEntry;
  TtsContextFontEntry = record
    FontID: tsFontID;
    Font: TtsFont;
  end;


  PtsContextImageEntry = ^TtsContextImageEntry;
  TtsContextImageEntry = record
    ImageID: tsImageID;
    Image: TtsImage;
  end;


// Helper
function MakeColor(Red: Byte = 0; Green: Byte = 0; Blue: Byte = 0; Alpha: Byte = 0): tsColor;


implementation


uses
  Math,
  SysUtils,
  SyncObjs,
  TextSuitePostProcess,
  TextSuiteTTFUtils;


var
  gLastContextID: Cardinal;


// Helper
function MakeColor(Red, Green, Blue, Alpha: Byte): tsColor;
begin
  Result.Red   := Red;
  Result.Green := Green;
  Result.Blue  := Blue;
  Result.Alpha := Alpha;
end;


procedure TranslateQuad(var Dest: tsQuadFloat; const Source: tsQuadFloat; const Translate: tsPoint);
begin
  Dest[0].X := Source[0].X + Translate.X;
  Dest[0].Y := Source[0].Y + Translate.Y;

  Dest[1].X := Source[1].X + Translate.X;
  Dest[1].Y := Source[1].Y + Translate.Y;

  Dest[2].X := Source[2].X + Translate.X;
  Dest[2].Y := Source[2].Y + Translate.Y;

  Dest[3].X := Source[3].X + Translate.X;
  Dest[3].Y := Source[3].Y + Translate.Y;
end;


{ TtsHash }

procedure TtsHash.Add(Name: Integer; Value: Pointer);
var
  Pos: Integer;
  Entry, HashEntry: PtsHashEntry;
begin
  if Name <> 0 then begin
    Pos := IntToPos(Name);
    HashEntry := fHashArray[Pos];
    Entry := fHashArray[Pos];

    if (HashEntry = nil) then begin
      if (Value = nil) then
        Exit;

      New(HashEntry);
      HashEntry^.Name := Name;
      HashEntry^.Value := Value;
      HashEntry^.Next := nil;
      fHashArray[Pos] := HashEntry;
      Inc(fCount);

      Exit;
    end;

    while HashEntry <> nil do begin
      if Name = HashEntry^.Name then begin
        if Value = nil then begin
          if (HashEntry = fHashArray[Pos]) then
            fHashArray[Pos] := fHashArray[Pos]^.Next
          else
            Entry^.Next := HashEntry^.Next;

          Dispose(HashEntry);
          Dec(fCount);
          Exit;
        end;

        HashEntry^.Value := Value;
        Exit;
      end;

      if HashEntry^.Next = nil
        then break;

      Entry := HashEntry;
      HashEntry := HashEntry^.Next;
    end;

    if (Value = nil)
      then Exit;

    New(Entry);
    Entry^.Name := Name;
    Entry^.Value := Value;
    Entry^.Next := nil;
    Inc(fCount);

    HashEntry^.Next := Entry;
  end;
end;


procedure TtsHash.Clear;
var
  Idx: Integer;
  TempEntry, Entry: PtsHashEntry;
begin
  for Idx := Low(fHashArray) to High(fHashArray) do begin
    Entry := fHashArray[Idx];

    while Entry <> nil do begin
      TempEntry := Entry;
      Entry := Entry^.Next;

      Dispose(TempEntry);
    end;

    fHashArray[Idx] := nil;
  end;

  fCount := 0;
end;


constructor TtsHash.Create(HashEntrys: Integer);
begin
  inherited Create;

  fHashEntrys := Max(1, HashEntrys);
  SetLength(fHashArray, fHashEntrys);
end;


procedure TtsHash.Delete(Name: Integer);
begin
  // Add with an empty value is enough 
  Add(Name, nil);
end;


destructor TtsHash.Destroy;
begin
  Clear;

  inherited;
end;


function TtsHash.Get(Name: Integer): Pointer;
var
  Pos: Integer;
  Entry: PtsHashEntry;
begin
  Result := nil;

  if Name <> 0 then begin
    Pos := IntToPos(Name);
    Entry := fHashArray[Pos];

    if Entry <> nil then begin
      while Entry <> nil do begin
        if Name = Entry^.Name then begin
          Result := Entry^.Value;

          Break;
        end;

        Entry := Entry^.Next;
      end;
    end;
  end;
end;


procedure TtsHash.GetNames(const NameList: TList);
var
  Idx: Integer;
  Entry: PtsHashEntry;
begin
  Assert(NameList <> nil, 'TtsHash.GetNames - NameList is undefined');

  NameList.Clear;

  for Idx := Low(fHashArray) to High(fHashArray) do begin
    Entry := fHashArray[Idx];

    while Entry <> nil do begin
      NameList.Add({%H-}Pointer(Entry^.Name));

      Entry := Entry^.Next;
    end;
  end;
end;


procedure TtsHash.GetValues(const ValueList: TList);
var
  Idx: Integer;
  Entry: PtsHashEntry;
begin
  Assert(ValueList <> nil, 'TtsHash.GetValues - ValuesList is undefined');

  ValueList.Clear;

  for Idx := Low(fHashArray) to High(fHashArray) do begin
    Entry := fHashArray[Idx];

    while Entry <> nil do begin
      ValueList.Add(Entry^.Value);

      Entry := Entry^.Next;
    end;
  end;
end;


function TtsHash.IntToPos(Name: Integer): Integer;
begin
  if Name < 0 then
    Result := -Name
  else
    Result := Name;

  Result := Result mod fHashEntrys;
end;


{ TtsStringHash }

procedure TtsStringHash.Add(pString: pWideChar);
var
  Pos: Integer;
  Entry, HashEntry: PtsStringHashEntry;
begin
  if pString <> nil then begin
    Pos := {%H-}Cardinal(pString) mod fHashEntrys;
    Entry := fHashArray[Pos];
    HashEntry := Entry;

    // is empty field
    if (Entry = nil) then begin
      New(Entry);
      Entry^.pString := pString;
      Entry^.Next := nil;
      fHashArray[Pos] := Entry;

      Exit;
    end;

    // search last
    while HashEntry <> nil do begin
      if HashEntry^.Next = nil
        then break;

      HashEntry := HashEntry^.Next;
    end;

    New(Entry);
    Entry^.pString := pString;
    Entry^.Next := nil;

    HashEntry^.Next := Entry;
  end;
end;


constructor TtsStringHash.Create(HashEntrys: Integer);
begin
  inherited Create;

  fHashEntrys := Max(1, HashEntrys);
  SetLength(fHashArray, fHashEntrys);
end;


function TtsStringHash.Delete(pString: pWideChar) : Boolean;
var
  Pos: Integer;
  Entry, HashEntry: PtsStringHashEntry;
begin
  Result := False;

  if pString <> nil then begin
    Pos := {%H-}Cardinal(pString) mod fHashEntrys;
    HashEntry := fHashArray[Pos];
    Entry := nil;

    while HashEntry <> nil do begin
      if pString = HashEntry^.pString then begin
        if (HashEntry = fHashArray[Pos]) then
          fHashArray[Pos] := fHashArray[Pos]^.Next
        else
          Entry^.Next := HashEntry^.Next;

        Dispose(HashEntry);

        Result := True;

        Exit;
      end;

      Entry := HashEntry;
      HashEntry := HashEntry^.Next;
    end;
  end;
end;


destructor TtsStringHash.Destroy;
var
  Idx: Integer;
  Temp: PtsStringHashEntry;
begin
  for Idx := Low(fHashArray) to High(fHashArray) do begin
    while fHashArray[Idx] <> nil do begin
      Temp := fHashArray[Idx];

      fHashArray[Idx] := fHashArray[Idx]^.Next;

      tsStrDispose(Temp^.pString);
      Dispose(Temp);
    end;
  end;

  SetLength(fHashArray, 0);

  inherited;
end;


{ TtsKernel1D }

constructor TtsKernel1D.Create(Radius, Strength: Single);
var
  TempRadius, SQRRadius, TempStrength, TempValue: Double;
  Idx: Integer;


  function CalcValue(Index: Integer): Single;
  var
    Temp: Double;
  begin
    Temp := Max(0, Abs(Index) - TempStrength);
    Temp := Sqr(Temp * TempRadius) / SQRRadius;

    Result := Exp(-Temp);
  end;

begin
  inherited Create;

  // calculate new radius and strength
  TempStrength := Min(Radius - 1, Radius * Strength);
  TempRadius := Radius - TempStrength;

  SQRRadius := sqr(TempRadius) * sqr(TempRadius);

  // caluculating size of the kernel
  Size := Round(TempRadius);
  while CalcValue(Size) > 0.001 do
    Inc(Size);
  Size := Size -1;

  ValueSum := 0;
  ItemCount := Size * 2 +1;
  SetLength(Items, ItemCount);

  // calculate Value (yes thats right. there is no -1)
  for Idx := 0 to Size do begin
    TempValue := CalcValue(Idx);

    with Items[Size + Idx] do begin
      Offset := Idx;
      Value := TempValue;
    end;

    with Items[Size - Idx] do begin
      Offset := -Idx;
      Value := TempValue;
    end;

    // sum
    ValueSum := ValueSum + TempValue;
    if Idx > 0 then
      ValueSum := ValueSum + TempValue;
  end;
end;


procedure TtsKernel1D.UpdateDataOffset(DataSize: Integer);
var
  Idx: Integer;
begin
  for Idx := 0 to ItemCount -1 do
    with Items[Idx] do
      DataOffset := Offset * DataSize;
end;



{ TtsKernel2D }

constructor TtsKernel2D.Create(Radius, Strength: Single);
var
  TempRadius, SQRRadius, TempStrength, TempValue: Double;
  X, Y, Height, Width: Integer;


  function CalcValue(Index: Single): Single;
  var
    Temp: Double;
  begin
    Temp := Max(0, Abs(Index) - TempStrength);
    Temp := Sqr(Temp * TempRadius) / SQRRadius;

    Result := Exp(-Temp);
  end;


  procedure QuickSort(L, R: Integer);
  var
    I, J: Integer;
    P, T: TtsKernel2DItem;

    function Compare(const Item1, Item2: TtsKernel2DItem): Integer;
    begin
      if Item1.Value = Item2.Value then
        Result := 0
      else
      if Item1.Value > Item2.Value then
        Result := -1
      else
        Result := 1;
    end;

  begin
    repeat
      I := L;
      J := R;
      P := Items[(L + R) shr 1];

      repeat
        while Compare(Items[I], P) < 0 do
          Inc(I);

        while Compare(Items[J], P) > 0 do
          Dec(J);

        if I <= J then begin
          T := Items[I];
          Items[I] := Items[J];
          Items[J] := T;
          Inc(I);
          Dec(J);
        end;
      until I > J;

      if L < J then
        QuickSort(L, J);

      L := I;
    until I >= R;
  end;


begin
  inherited Create;

  // calculate new radius and strength
  TempStrength := Min(Radius - 1, Radius * Strength);
  TempRadius := Radius - TempStrength;

  SQRRadius := sqr(TempRadius) * sqr(TempRadius);

  // caluculating X size of the kernel
  SizeX := 0;
  MidSizeX := SizeX;

  while CalcValue(SizeX) > 0.5 do begin
    Inc(SizeX);
    Inc(MidSizeX);
  end;

  while CalcValue(SizeX) > 0.001 do
    Inc(SizeX);

  // caluculating Y size of the kernel
  SizeY := 0;
  MidSizeY := SizeY;

  while CalcValue(SizeY) > 0.5 do begin
    Inc(SizeY);
    Inc(MidSizeY);
  end;

  while CalcValue(SizeY) > 0.001 do
    Inc(SizeY);

  ValueSum := 0;

  Width := SizeX * 2 + 1;
  Height := SizeY * 2 + 1;
  ItemCount := Height * Width;
  SetLength(Items, ItemCount);

  Width := SizeX * 2 + 1;
  Height := SizeY * 2 + 1;
  ItemCount := Height * Width;
  SetLength(Items, ItemCount);

  // calculate Value (yes thats right. there is no -1)
  for Y := 0 to SizeY do begin
    for X := 0 to SizeX do begin
      TempValue := CalcValue(Sqrt(Sqr(X) + Sqr(Y)));

      with Items[(SizeY + Y) * Width + (SizeX + X)] do begin
        OffsetX :=  X;
        OffsetY :=  Y;
        Value := TempValue;
      end;

      with Items[(SizeY + Y) * Width + (SizeX - X)] do begin
        OffsetX := -X;
        OffsetY :=  Y;
        Value := TempValue;
      end;

      with Items[(SizeY - Y) * Width + (SizeX + X)] do begin
        OffsetX :=  X;
        OffsetY := -Y;
        Value := TempValue;
      end;

      with Items[(SizeY - Y) * Width + (SizeX - X)] do begin
        OffsetX := -X;
        OffsetY := -Y;
        Value := TempValue;
      end;

      // sum
      ValueSum := ValueSum + TempValue;
      if (X > 0) and (Y > 0) then
        ValueSum := ValueSum + TempValue;
    end;
  end;

  // sort
  QuickSort(0, ItemCount -1);

  // cut small items
  while Items[ItemCount -1].Value < 0.001 do
    Dec(ItemCount);

  SetLength(Items, ItemCount);
end;


procedure TtsKernel2D.UpdateDataOffset(DataSizeX, DataSizeY: Integer);
var
  Idx: Integer;
begin
  for Idx := 0 to ItemCount -1 do
    with Items[Idx] do
      DataOffset := OffsetX * DataSizeX + OffsetY * DataSizeY;
end;


{ TtsChar }

(*
procedure TtsChar.CalculateKerningData(CharImage: TtsImage);
var
  Y: Integer;
  pLeft, pRight: PtsColor;


  function GetFirstPixel(pData: PtsColor; MinOpaque: Byte; IncValue, MaxSteps: Integer) : Integer;
  var
    CurStep: Integer;
  begin
    Result := MaxSteps;
    CurStep := 0;

    while CurStep < MaxSteps do begin
      if pData^.Alpha >= MinOpaque then begin
        Result := CurStep;
        Break;
      end;

      Inc(CurStep);
      Inc(pData, IncValue);
    end;
  end;

begin
  SetLength(KerningValuesLeft, CharImage.Height);
  SetLength(KerningValuesRight, CharImage.Height);

  for Y := 0 to CharImage.Height - 1 do begin
    pRight := CharImage.ScanLine[Y];
    Inc(pRight, CharImage.Width -1);
    KerningValuesRight[Y] := GetFirstPixel(pRight, $40, -1, CharImage.Width);

    pLeft:= CharImage.ScanLine[Y];
    KerningValuesLeft[Y] := GetFirstPixel(pLeft, $40, 1, CharImage.Width);
  end;
end;
*)


//function TtsChar.CalculateKerningValue(LastChar: TtsChar): Smallint;
//begin
//  Result := 0;
//var
//  TempHeight, TempLastHeight: Integer;
//  Y, YMin, YMax: Integer;
//  LeftYMin, LeftYMax, RightYMin, RightYMax: Integer;
//
//  Dist, TempDist: Integer;
//
//  function GetMinDistance(Row: Integer): Integer;
//  begin
////    Result :=
////          Self.KerningValuesLeft[Self.BaseLine - Self.GlyphRect.Top + Row] +
////      LastChar.KerningValuesRight[LastChar.BaseLine - LastChar.GlyphRect.Top + Row];
//  end;
//
//begin
//  Result := 0;
//
//  if Assigned(LastChar) then begin
//    TempLastHeight := Length(LastChar.KerningValuesRight);
//    TempHeight := Length(Self.KerningValuesLeft);
//
//    if (TempLastHeight > 0) and (TempHeight > 0) then begin
//      LeftYMin := Self.GlyphRect.Bottom - Self.BaseLine;
//      LeftYMax := Self.GlyphRect.Top - Self.BaseLine;
//
//      RightYMin := LastChar.GlyphRect.Bottom - LastChar.BaseLine;
//      RightYMax := LastChar.GlyphRect.Top - LastChar.BaseLine;
//
//      YMin := Min(LeftYMin, RightYMin);
//      YMax := Max(LeftYMax, RightYMax);
//
//      Dist := -1;
//
//      for Y := YMax to YMin -1 do begin
//        TempDist := GetMinDistance(Y);
//
//        if (Dist = -1) then
//          Dist := TempDist
//        else
//
//        if TempDist < Dist then
//          Dist := TempDist;
//      end;
//
//      // calculate advance of last char to diff
//      Dist := Dist + LastChar.Advance - (LastChar.GlyphRect.Right - LastChar.GlyphRect.Left);
//
//      Result := -Dist +3;
//    end;
//  end;
//end;


constructor TtsChar.Create(CharCode: WideChar);
begin
  inherited Create;

  fCharCode := CharCode;
end;


destructor TtsChar.Destroy;
begin
//  SetLength(KerningValuesLeft, 0);
//  SetLength(KerningValuesRight, 0);

  inherited;
end;


procedure TtsChar.ExpandRect(Left, Top, Right, Bottom: Integer);
begin
  Advance := Advance + Left + Right;
  GlyphOriginY := GlyphOriginY + Top + Bottom;

  GlyphRect.Right := GlyphRect.Right + Left + Right;
  GlyphRect.Bottom := GlyphRect.Bottom + Top + Bottom;
end;


{ TtsImage }
type
  TtsModeFunc = function(Source, Dest: Byte): Byte; register;


function ModeFuncIgnore(Source, Dest: Byte): Byte; register;
{$ifdef TS_PURE_PASCAL}
begin
  Result := Dest;
{$else}
asm
  mov al, dl
{$endif}
end;


function ModeFuncReplace(Source, Dest: Byte): Byte; register;
{$ifdef TS_PURE_PASCAL}
begin
  Result := Source;
{$else}
asm
{$endif}
end;


function ModeFuncModulate(Source, Dest: Byte): Byte; register;
{$ifdef TS_PURE_PASCAL}
begin
  Result := (Source * Dest) div $FF
{$else}
asm
//  inc   ax
//  inc   dx
  mul   dl
  shr   eax, 8
{$endif}
end;


procedure TtsImage.AddFunc(Func: TtsImageFunc; Data: Pointer);
var
  X, Y: Integer;
  pPix: PtsColor;
begin
  for Y := 0 to Height - 1 do begin
    pPix := ScanLine[Y];

    for X := 0 to Width - 1 do begin
      Func(Self, X, Y, pPix^, Data);

      Inc(pPix);
    end;
  end;
end;


procedure TtsImage.AddResizingBorder(tsChar: TtsChar);
var
  X, Y: Integer;
  pPix: PtsColor;

  pTemp: PtsColor;
  SumCount: Integer;
  SumColor: array [0..2] of integer;
begin
  SumColor[0] := 0;
  SumColor[1] := 0;
  SumColor[2] := 0;
  SumCount := 0;

  // settings of char
  tsChar.GlyphRect.Top := tsChar.GlyphRect.Top + 1;
  tsChar.GlyphRect.Left := tsChar.GlyphRect.Left + 1;
  tsChar.GlyphRect.Right := tsChar.GlyphRect.Right + 1;
  tsChar.GlyphRect.Bottom := tsChar.GlyphRect.Bottom + 1;

  // resize image
  Resize(Width + 4, Height + 4, 2, 2);

  // calculate color of invisible pixels
  for Y := 0 to Height -1 do begin
    pPix := ScanLine[Y];

    for X := 0 to Width -1 do begin
      if pPix^.Alpha = 0 then begin
        // row -1
        if Y > 0 then begin
          pTemp := pPix;
          Dec(pTemp, fWidth);

          // row -1 / col
          if pTemp^.Alpha > 0 then begin
            Inc(SumCount);
            Inc(SumColor[0], pTemp^.Red);
            Inc(SumColor[1], pTemp^.Green);
            Inc(SumColor[2], pTemp^.Blue);
          end;

          // row -1 / col -1
          if X > 0 then begin
            Dec(pTemp);

            if pTemp^.Alpha > 0 then begin
              Inc(SumCount);
              Inc(SumColor[0], pTemp^.Red);
              Inc(SumColor[1], pTemp^.Green);
              Inc(SumColor[2], pTemp^.Blue);
            end;

            Inc(pTemp);
          end;

          // row -1 / col +1
          if X < fWidth -1 then begin
            Inc(pTemp);

            if pTemp^.Alpha > 0 then begin
              Inc(SumCount);
              Inc(SumColor[0], pTemp^.Red);
              Inc(SumColor[1], pTemp^.Green);
              Inc(SumColor[2], pTemp^.Blue);
            end;
          end;
        end;

        // row +1
        if Y < fHeight -1 then begin
          pTemp := pPix;
          Inc(pTemp, fWidth);

          // row +1 / col
          if pTemp^.Alpha > 0 then begin
            Inc(SumCount);
            Inc(SumColor[0], pTemp^.Red);
            Inc(SumColor[1], pTemp^.Green);
            Inc(SumColor[2], pTemp^.Blue);
          end;

          // row +1 / col -1
          if X > 0 then begin
            Dec(pTemp);

            if pTemp^.Alpha > 0 then begin
              Inc(SumCount);
              Inc(SumColor[0], pTemp^.Red);
              Inc(SumColor[1], pTemp^.Green);
              Inc(SumColor[2], pTemp^.Blue);
            end;

            Inc(pTemp);
          end;

          // row +1 / col +1
          if X < fWidth -1 then begin
            Inc(pTemp);

            if pTemp^.Alpha > 0 then begin
              Inc(SumCount);
              Inc(SumColor[0], pTemp^.Red);
              Inc(SumColor[1], pTemp^.Green);
              Inc(SumColor[2], pTemp^.Blue);
            end;
          end;
        end;

        // row / col -1
        if X > 0 then begin
          pTemp := pPix;
          Dec(pTemp);

          if pTemp^.Alpha > 0 then begin
            Inc(SumCount);
            Inc(SumColor[0], pTemp^.Red);
            Inc(SumColor[1], pTemp^.Green);
            Inc(SumColor[2], pTemp^.Blue);
          end;
        end;

        // row / col +1
        if X < fWidth -1 then begin
          pTemp := pPix;
          Inc(pTemp);

          if pTemp^.Alpha > 0 then begin
            Inc(SumCount);
            Inc(SumColor[0], pTemp^.Red);
            Inc(SumColor[1], pTemp^.Green);
            Inc(SumColor[2], pTemp^.Blue);
          end;
        end;

        // any pixel next to the transparent pixel they are opaque?
        if SumCount > 0 then begin
          // calculate resulting pixel color
          pPix^.Red   := SumColor[0] div SumCount;
          pPix^.Green := SumColor[1] div SumCount;
          pPix^.Blue  := SumColor[2] div SumCount;

          // clearing values
          SumColor[0] := 0;
          SumColor[1] := 0;
          SumColor[2] := 0;
          SumCount := 0;
        end;
      end;

      Inc(pPix);
    end;
  end;
end;


procedure TtsImage.AssignFrom(Image: TtsImage);
var
  pImage: Pointer;
  ImageSize: Integer;
begin
  ImageSize := Image.Width * Image.Height * GetFormatSize(Image.Format);

  GetMem(pImage, ImageSize);

  if pImage <> nil then
    Move(Image.Data^, pImage^, ImageSize);

  SetDataPtr(pImage, Image.Format, Image.Width, Image.Height);
end;


procedure TtsImage.BeforeDestruction;
begin
  SetDataPtr(nil);

  inherited;
end;


procedure TtsImage.BlendImage(Image: TtsImage; X, Y: Integer; AutoExpand: Boolean);
var
  pImage, pDest: PtsColor;
  X1, X2, Y1, Y2, BX1, BX2, BY1, BY2, NewWidth, NewHeight: Integer;
  TempX, TempY: Integer;

  TempLines: array of PtsColor;
  pSource: PtsColor;

  // Blending
  pUnder, pOver: PtsColor;
  ResultAlpha, FaqUnder, FaqOver: Byte;
begin
  // Calculate new size
  X1 := Min(X, 0);
  X2 := Max(X + Image.Width, Width);

  Y1 := Min(Y, 0);
  Y2 := Max(Y + Image.Height, Height);

  BX1 := Max(X, 0);
  BX2 := Min(X + Image.Width, Width);

  BY1 := Max(Y, 0);
  BY2 := Min(Y + Image.Height, Height);

  NewWidth  := X2 - X1;
  NewHeight := Y2 - Y1;

  // Allocate new image
  GetMem(pImage, NewWidth * NewHeight * GetFormatSize(Format));
  try
     FillChar(pImage^, NewWidth * NewHeight * GetFormatSize(Format), #$00);

    // ScanLines
    SetLength(TempLines, NewHeight);

    for TempY := 0 to NewHeight - 1 do begin
      TempLines[TempY] := pImage;
      Inc(TempLines[TempY], NewWidth * TempY);
    end;

    // copy non overlapping data from underlaying Image
    for TempY := 0 to Height -1 do begin
      pDest := TempLines[TempY - Y1];
      Inc(pDest, - X1);

      pSource := ScanLine[TempY];

      for TempX := 0 to Width -1 do begin
        pDest^ := pSource^;

        Inc(pDest);
        Inc(pSource);
      end;
    end;

    // copy non overlapping data from overlaying Image
    for TempY := 0 to Image.Height -1 do begin
      pDest := TempLines[TempY + Y - Y1];
      Inc(pDest, X - X1);

      pSource := Image.ScanLine[TempY];

      for TempX := 0 to Image.Width -1 do begin
        pDest^ := pSource^;

        Inc(pDest);
        Inc(pSource);
      end;
    end;

    // Blend overlapped
    for TempY := BY1 to BY2 - 1 do begin
      pOver := Image.ScanLine[TempY - Min(BY1, Y)];
      Inc(pOver, BX1 - X);

      pUnder := ScanLine[TempY - Min(BY1, 0)];
      Inc(pUnder, BX1);

      pDest := TempLines[TempY - Min(Y, 0)];
      Inc(pDest, BX1 - Min(X, 0));

      for TempX := BX1 to BX2 - 1 do begin
        ResultAlpha := pOver^.Alpha + pUnder^.Alpha * ($FF - pOver^.Alpha) div $FF;

        if ResultAlpha > 0 then begin
          FaqUnder := (pUnder^.Alpha * ($FF - pOver^.Alpha) div $FF) * $FF div ResultAlpha;
          FaqOver := pOver^.Alpha * $FF div ResultAlpha;

          pDest^.Red   := (pOver^.Red   * FaqOver + pUnder^.Red   * FaqUnder) div $FF;
          pDest^.Green := (pOver^.Green * FaqOver + pUnder^.Green * FaqUnder) div $FF;
          pDest^.Blue  := (pOver^.Blue  * FaqOver + pUnder^.Blue  * FaqUnder) div $FF;
        end else begin
          pDest^.Red   := 0;
          pDest^.Green := 0;
          pDest^.Blue  := 0;
        end;

        pDest^.Alpha := ResultAlpha;

        Inc(pOver);
        Inc(pUnder);
        Inc(pDest);
      end;
    end;

    // Set new image
    SetDataPtr(pImage, Format, NewWidth, NewHeight);
  except
    FreeMem(pImage);
  end;
end;



type
  TtsImageBlurFuncData = packed record
    Kernel: TtsKernel1D;
    Pos, MaxPos: Integer;
  end;

  TBlurFunc = function(pSource: pByte; var Data: TtsImageBlurFuncData): Byte; register;


function BlurFuncKernel(pSource: pByte; var Data: TtsImageBlurFuncData): Byte; register;
var
  Idx: Integer;
  pTemp: pByte;
  TempSum, TempMax: Double;
begin
  TempSum := 0;
  TempMax := 0;

  with Data do begin
    for Idx := 0 to Kernel.ItemCount -1 do begin
      with Kernel.Items[Idx] do begin
        if (Pos + Offset >= 0) and (Pos + Offset < MaxPos) then begin
          pTemp := pSource;
          Inc(pTemp, DataOffset);

          TempSum := TempSum + pTemp^ * Value;
          TempMax := TempMax + Value;
        end;
      end;
    end;
  end;

  Result := Round(TempSum / TempMax);
end;


function BlurFuncIgnore(pSource: pByte; var Data: TtsImageBlurFuncData): Byte; register;
{$ifdef TS_PURE_PASCAL}
begin
  Result := pSource^;
{$else}
asm
  mov al, byte ptr [eax]
{$endif}
end;


procedure TtsImage.Blur(HorzKernel, VertKernel: TtsKernel1D; ChannelMask: tsBitmask);
var
  X, Y: Integer;
  Temp: TtsImage;

  pSource, pDest: ptsColor;

  FuncData: TtsImageBlurFuncData;
  RedFunc, GreenFunc, BlueFunc, AlphaFunc: TBlurFunc;


  procedure AssignFunc(var Func: TBlurFunc; MaskBit: Cardinal);
  begin
    if MaskBit and ChannelMask > 0 then
      Func := BlurFuncKernel
    else
      Func := BlurFuncIgnore;
  end;


begin
  // casing functions
  AssignFunc(RedFunc,   TS_CHANNEL_RED);
  AssignFunc(GreenFunc, TS_CHANNEL_GREEN);
  AssignFunc(BlueFunc,  TS_CHANNEL_BLUE);
  AssignFunc(AlphaFunc, TS_CHANNEL_ALPHA);


  Temp := TtsImage.Create;
  try
    Temp.CreateEmpty(Format, Width, Height);
    Temp.FillColor(1, 1, 1, 0, TS_CHANNELS_RGBA, cModesReplace);

    // blur horz from original to temp image
    HorzKernel.UpdateDataOffset(4);

    FuncData.Kernel := HorzKernel;
    FuncData.MaxPos := Temp.Width;

    for Y := 0 to Temp.Height - 1 do begin
      pSource := Self.ScanLine[Y];
      pDest := Temp.ScanLine[Y];

      for X := 0 to FuncData.MaxPos - 1 do begin
        FuncData.Pos := X;
        pDest^.Red   := RedFunc(@(pSource^.Red), FuncData);
        pDest^.Green := GreenFunc(@(pSource^.Green), FuncData);
        pDest^.Blue  := BlueFunc(@(pSource^.Blue), FuncData);
        pDest^.Alpha := AlphaFunc(@(pSource^.Alpha), FuncData);

        Inc(pDest);
        Inc(pSource);
      end;
    end;

    // blur vert from temp to original image
    VertKernel.UpdateDataOffset(Width * 4);

    FuncData.Kernel := VertKernel;
    FuncData.MaxPos := Temp.Height;

    for Y := 0 to Temp.Height - 1 do begin
      pSource := Temp.ScanLine[Y];
      pDest := Self.ScanLine[Y];

      FuncData.Pos := Y;

      for X := 0 to Temp.Width - 1 do begin
        pDest^.Red   := RedFunc(@(pSource^.Red), FuncData);
        pDest^.Green := GreenFunc(@(pSource^.Green), FuncData);
        pDest^.Blue  := BlueFunc(@(pSource^.Blue), FuncData);
        pDest^.Alpha := AlphaFunc(@(pSource^.Alpha), FuncData);

        Inc(pDest);
        Inc(pSource);
      end;
    end;

  finally
    Temp.Free;
  end;
end;


procedure TtsImage.CreateEmpty(Format: TtsFormat; aWidth, aHeight: Integer);
var
  pImage: pByte;
begin
  pImage := AllocMem(aWidth * aHeight * GetFormatSize(Format));

  SetDataPtr(pImage, Format, aWidth, aHeight);
end;


procedure TtsImage.FillColor(Red, Green, Blue, Alpha: Single; ChannelMask: tsBitmask; Modes: TtsImageModes);
//var
//  MaskColor: TtsFillcolorData;
//begin
//  // prepare mask
//  FillChar(MaskColor.Mask, 4, $FF);
//  if ChannelMask and TS_CHANNEL_RED = TS_CHANNEL_RED then
//    MaskColor.Mask[0] := $00;
//  if ChannelMask and TS_CHANNEL_GREEN = TS_CHANNEL_GREEN then
//    MaskColor.Mask[1] := $00;
//  if ChannelMask and TS_CHANNEL_BLUE = TS_CHANNEL_BLUE then
//    MaskColor.Mask[2] := $00;
//  if ChannelMask and TS_CHANNEL_ALPHA = TS_CHANNEL_ALPHA then
//    MaskColor.Mask[3] := $00;
//
//  pCardinal(@MaskColor.Mask[4])^  := pCardinal(@MaskColor.Mask[0])^;
//  pCardinal(@MaskColor.Mask[8])^  := pCardinal(@MaskColor.Mask[0])^;
//  pCardinal(@MaskColor.Mask[12])^ := pCardinal(@MaskColor.Mask[0])^;
//
//  // prepare color
//  MaskColor.Color[0] := Round($FF * Red);
//  MaskColor.Color[1] := Round($FF * Green);
//  MaskColor.Color[2] := Round($FF * Blue);
//  MaskColor.Color[3] := Round($FF * Alpha);
//  pCardinal(@MaskColor.Color[4])^  := pCardinal(@MaskColor.Color[0])^;
//  pCardinal(@MaskColor.Color[8])^  := pCardinal(@MaskColor.Color[0])^;
//  pCardinal(@MaskColor.Color[12])^ := pCardinal(@MaskColor.Color[0])^;
//
//  // image mode
//  FillChar(MaskColor.ModuloMask, 4, $00);
//  if (Modes[tsModeRed] = TS_MODE_MODULATE) and (MaskColor.Mask[0] > 0) then
//    MaskColor.ModuloMask[0] := $FF;
//  if (Modes[tsModeGreen] = TS_MODE_MODULATE) and (MaskColor.Mask[1] > 0) then
//    MaskColor.ModuloMask[1] := $FF;
//  if (Modes[tsModeBlue] = TS_MODE_MODULATE) and (MaskColor.Mask[2] > 0) then
//    MaskColor.ModuloMask[2] := $FF;
//  if (Modes[tsModeAlpha] = TS_MODE_MODULATE) and (MaskColor.Mask[3] > 0) then
//    MaskColor.ModuloMask[3] := $FF;
//  pCardinal(@MaskColor.ModuloMask[4])^  := pCardinal(@MaskColor.ModuloMask[0])^;
//
//  // fill with color
//  if pCardinal(@MaskColor.ModuloMask[0])^ = 0 then
//    Fillcolor_RGBA8(Data, @MaskColor, Width * Height)
//  else
//    Fillcolor_RGBA8_modulo(Data, @MaskColor, Width * Height);
//
//  {$IFNDEF TS_PURE_PASCAL}
////  if supportSSE then
////    Fillcolor_RGBA8_SSE(Data, @MaskColor, Width * Height)
////  else
//  {$ENDIF}
//
////    Fillcolor_RGBA8(Data, @MaskColor, Width * Height);
//end;
var
  _Red, _Green, _Blue, _Alpha: Byte;
  RedFunc, GreenFunc, BlueFunc, AlphaFunc, LuminanceFunc: TtsModeFunc;

  Y, X: Integer;
  pPix: PtsColor;


  procedure AssignFunc(var Func: TtsModeFunc; Mask, Mode: tsEnum);
  begin
    if ChannelMask and Mask = Mask then begin
      if Mode = TS_MODE_MODULATE then
        Func := ModeFuncModulate
      else
        Func := ModeFuncReplace
    end else
      Func := ModeFuncIgnore
  end;


begin
  _Red       := Round($FF * Red);
  _Green     := Round($FF * Green);
  _Blue      := Round($FF * Blue);
  _Alpha     := Round($FF * Alpha);

  AssignFunc(RedFunc,       TS_CHANNEL_RED,       Modes[tsModeRed]);
  AssignFunc(GreenFunc,     TS_CHANNEL_GREEN,     Modes[tsModeGreen]);
  AssignFunc(BlueFunc,      TS_CHANNEL_BLUE,      Modes[tsModeBlue]);
  AssignFunc(AlphaFunc,     TS_CHANNEL_ALPHA,     Modes[tsModeAlpha]);
  AssignFunc(LuminanceFunc, TS_CHANNEL_LUMINANCE, Modes[tsModeLuminance]);

  for Y := 0 to Height - 1 do begin
    pPix := ScanLine[Y];

    for X := 0 to Width - 1 do begin
      pPix^.Red   := RedFunc  (_Red,   pPix^.Red);
      pPix^.Green := GreenFunc(_Green, pPix^.Green);
      pPix^.Blue  := BlueFunc (_Blue,  pPix^.Blue);
      pPix^.Alpha := AlphaFunc(_Alpha, pPix^.Alpha);

      Inc(pPix);
    end;
  end;
end;


procedure TtsImage.FillPattern(Pattern: TtsImage; X, Y: Integer; ChannelMask: tsBitmask; Modes: TtsImageModes);
var
  TempX, TempY, RandX, RandY, PosX: Integer;
  RedFunc, GreenFunc, BlueFunc, AlphaFunc, LuminanceFunc: TtsModeFunc;
  pSrc, pDest: PtsColor;


  procedure AssignFunc(var Func: TtsModeFunc; Mask, Mode: tsEnum);
  begin
    if ChannelMask and Mask = Mask then begin
      if Mode = TS_MODE_MODULATE then
        Func := ModeFuncModulate
      else
        Func := ModeFuncReplace
    end else
      Func := ModeFuncIgnore
  end;


begin
  // Pattern position
  if X < 0 then
    RandX := Random(Pattern.Width)
  else
    RandX := X;

  if Y < 0 then
    RandY := Random(Pattern.Height)
  else
    RandY := Y;

  AssignFunc(RedFunc,       TS_CHANNEL_RED,       Modes[tsModeRed]);
  AssignFunc(GreenFunc,     TS_CHANNEL_GREEN,     Modes[tsModeGreen]);
  AssignFunc(BlueFunc,      TS_CHANNEL_BLUE,      Modes[tsModeBlue]);
  AssignFunc(AlphaFunc,     TS_CHANNEL_ALPHA,     Modes[tsModeAlpha]);
  AssignFunc(LuminanceFunc, TS_CHANNEL_LUMINANCE, Modes[tsModeLuminance]);

  // Copy data
  for TempY := 0 to Height - 1 do begin
    pDest := ScanLine[TempY];
    pSrc := Pattern.Scanline[(TempY + RandY) mod Pattern.Height];

    Inc(pSrc, RandX);
    PosX := RandX;

    for TempX := 0 to Width - 1 do begin
      if PosX >= Pattern.Width then begin
        pSrc := Pattern.Scanline[(TempY + RandY) mod Pattern.Height];
        PosX := 0;
      end;

      pDest^.Red   := RedFunc  (pSrc^.Red,   pDest^.Red);
      pDest^.Green := GreenFunc(pSrc^.Green, pDest^.Green);
      pDest^.Blue  := BlueFunc (pSrc^.Blue,  pDest^.Blue);
      pDest^.Alpha := AlphaFunc(pSrc^.Alpha, pDest^.Alpha);

      Inc(pDest);
      Inc(pSrc);
      Inc(PosX);
    end;
  end;
end;


procedure TtsImage.FindMinMax(var MinMaxInfo: tsRect);
var
  X, Y: Integer;
  pPix: PtsColor;
begin
  MinMaxInfo.Top := -1;
  MinMaxInfo.Left := -1;
  MinMaxInfo.Right := -1;
  MinMaxInfo.Bottom := -1;

  // Search for MinMax
  for Y := 0 to Height -1 do begin
    pPix := ScanLine[Y];

    for X := 0 to Width -1 do begin
      if pPix^.Alpha > 0 then begin
        if (X < MinMaxInfo.Left) or (MinMaxInfo.Left = -1) then
          MinMaxInfo.Left := X;

        if (X+1 > MinMaxInfo.Right) or (MinMaxInfo.Right = -1) then
          MinMaxInfo.Right := X +1;

        if (Y < MinMaxInfo.Top) or (MinMaxInfo.Top = -1) then
          MinMaxInfo.Top := Y;

        if (Y+1 > MinMaxInfo.Bottom) or (MinMaxInfo.Bottom = -1) then
          MinMaxInfo.Bottom := Y +1;
      end;

      Inc(pPix);
    end;
  end;
end;


function TtsImage.GetEmpty: Boolean;
begin
  Result := fData = nil;
end;


function TtsImage.GetFormatSize(Format: TtsFormat): Integer;
begin
  case Format of
    tsFormatRGBA8:           Result := 4;
  else
    Result := 0;
  end;
end;


function TtsImage.GetScanLine(Index: Integer): pointer;
begin
  if not fScanLinesValid then
    UpdateScanLines;

  if (fScanLinesValid) and (Index >= 0) and (Index <= High(fScanLines)) then
    Result := fScanLines[Index]
  else
    Result := nil;
end;


procedure TtsImage.LoadFromFile(FileName: PAnsiChar);
var
  Surface, ConvSurface: PSDL_Surface;
  Format: TSDL_PixelFormat;

  ImageSize: Integer;
  Image: pByte;
begin
  Surface := IMG_Load(FileName);

  if Surface <> nil then
    try
      FillChar(Format, SizeOf(TSDL_PixelFormat), 0);
      Format.BitsPerPixel := 32;
      Format.BytesPerPixel := 4;
      Format.RMask := $000000FF;
      Format.GMask := $0000FF00;
      Format.BMask := $00FF0000;
      Format.AMask := $FF000000;
      Format.Rshift :=  0;
      Format.Gshift :=  8;
      Format.Bshift := 16;
      Format.Ashift := 24;

      ConvSurface := SDL_ConvertSurface(Surface, @Format, SDL_SWSURFACE);
      if ConvSurface <> nil then
        try
          // Set Image Size
          ImageSize := ConvSurface^.Width * ConvSurface^.Height * 4;

          GetMem(Image, ImageSize);
          try
            // Copy image
            Move(ConvSurface^.pixels^, Image^, ImageSize);

            // Set new Data
            SetDataPtr(Image, tsFormatRGBA8, ConvSurface^.Width, ConvSurface^.Height);
          except
            FreeMem(Image);
          end;
        finally
          SDL_FreeSurface(ConvSurface);
        end;
    finally
      SDL_FreeSurface(Surface);
    end;
end;


procedure TtsImage.Resize(NewWidth, NewHeight, X, Y: Integer);
var
  pImage: PByte;
  PixSize, LineSize, ImageSize, OrgLineSize: Integer;

  pSource, pDest: PByte;
  YStart, YEnd, YPos, XStart, XEnd: Integer;
begin
  if (NewHeight = 0) or (NewWidth = 0) then begin
    SetDataPtr(nil);
  end else begin
    PixSize := GetFormatSize(Format);
    LineSize := PixSize * NewWidth;
    ImageSize := LineSize * NewHeight;

    OrgLineSize := PixSize * Width;

    GetMem(pImage, ImageSize);
    try
      FillChar(pImage^, ImageSize, 0);

      // positions
      YStart := Max(0, Y);
      YEnd   := Min(NewHeight, Y + Height);

      XStart := Max(0, X);
      XEnd   := Min(NewWidth, X + Width);

      // copy data
      for YPos := YStart to YEnd -1 do begin
        pDest := pImage;
        Inc(pDest, LineSize * YPos + PixSize * XStart);

        pSource := fData;
        Inc(pSource, OrgLineSize * (YPos - Y) + PixSize * (XStart - X));

        Move(pSource^, pDest^, (XEnd - XStart) * PixSize);
      end;

      // assign
      SetDataPtr(pImage, Format, NewWidth, NewHeight);
    except
      FreeMem(pImage);
    end;
  end;
end;


procedure TtsImage.SetDataPtr(aData: Pointer; aFormat: TtsFormat; aWidth, aHeight: Integer);
begin
  fScanLinesValid := False;

  if fData <> nil then
    FreeMemory(fData);

  fData := aData;
  if fData <> nil then begin
    fWidth  := aWidth;
    fHeight := aHeight;
    fFormat := aFormat;
  end else begin
    fWidth  := 0;
    fHeight := 0;
    fFormat := tsFormatEmpty;
  end;
end;


procedure TtsImage.UpdateScanLines;
var
  Idx, LineSize: Integer;
  Temp: pByte;
begin
  LineSize := fWidth * GetFormatSize(fFormat);

  SetLength(fScanLines, fHeight);
  for Idx := 0 to fHeight -1 do begin
    Temp := fData;
    Inc(Temp, Idx * LineSize);

    fScanLines[Idx] := Temp;
  end;

  fScanLinesValid := True;
end;


{ TtsFont }

procedure TtsFont.AddChar(CharCode: WideChar; Char: TtsChar);
var
  Idx1, Idx2: Integer;
  Chars: PtsFontCharArray;
begin
  Idx1 := Hi(Ord(CharCode));
  Chars := fChars[Idx1];

  if Chars = nil then begin
    New(Chars);
    FillChar(Chars^, SizeOf(TtsFontCharArray), 0);

    fChars[Idx1] := Chars;
  end;

  if Chars <> nil then begin
    Idx2 := Lo(Ord(CharCode));
    Chars^.Chars[Idx2] := Char;
    Chars^.CharCount := Chars^.CharCount + 1;    
  end;
end;


procedure TtsFont.ClearChars;
var
  Idx1, Idx2: Integer;
  Chars: PtsFontCharArray;

  Char: TtsChar;
begin
  // iterate first step
  for Idx1 := Low(fChars) to High(fChars) do begin
    Chars := fChars[Idx1];

    // iterate second step
    if Chars <> nil then begin
      for Idx2 := Low(Chars^.Chars) to High(Chars^.Chars) do begin
        Char := Chars^.Chars[Idx2];

        // free char
        if Char <> nil then begin
          if Char.RendererImageReference <> nil then begin
            if fRenderer <> nil then
              fRenderer.RemoveImageReference(Char.RendererImageReference);

            Char.RendererImageReference.Free;
          end;

          Char.Free;
        end;
      end;

      // dispose
      fChars[Idx1] := nil;
      dispose(Chars);
    end;
  end;
end;


constructor TtsFont.Create(Renderer: TtsRenderer; Size: Integer; Style: TtsFontStyles; Format: TtsFormat; AntiAliasing: TtsAntiAliasing);
begin
  inherited Create;

  fRenderer := Renderer;

  fSize := Size;
  fStyle := Style;
  fFormat := Format;
  fAntiAliasing := AntiAliasing;
end;


procedure TtsFont.DeleteChar(CharCode: WideChar);
var
  Idx1, Idx2: Integer;
  Chars: PtsFontCharArray;
  Char: TtsChar;
begin
  // first step
  Idx1 := Hi(Ord(CharCode));
  Chars := fChars[Idx1];

  if Chars <> nil then begin
    // second step
    Idx2 := Lo(Ord(CharCode));
    Char := Chars^.Chars[Idx2];

    if Char <> nil then begin
      Chars^.Chars[Idx2] := nil;
      Chars^.CharCount := Chars^.CharCount -1;

      // no chars so delete the subpage
      if Chars^.CharCount = 0 then begin
        fChars[Idx1] := nil;
        Dispose(Chars);
      end;

      if Char.RendererImageReference <> nil then begin
        if fRenderer <> nil then
          fRenderer.RemoveImageReference(Char.RendererImageReference);

        Char.RendererImageReference.Free;
      end;

      Char.Free;
    end;
  end;
end;


destructor TtsFont.Destroy;
begin
  // Chars
  ClearChars;

  inherited;
end;


function TtsFont.GetChar(CharCode: WideChar): TtsChar;
{$IFDEF TS_PURE_PASCAL}
var
  Chars: PtsFontCharArray;
begin
  // first step
  Chars := fChars[Hi(Ord(CharCode))];

  // second step
  if Chars <> nil then
    Result := Chars^.Chars[Lo(Ord(CharCode))]
  else
    Result := nil;
{$else}
asm
  add     eax, offset TtsFont.fChars        // add offset of fChars to self

  movzx   ecx, dh                           // extract high byte to ecx
  mov     eax, dword ptr [eax + ecx * 4]    // copy array element to eax

  test    eax, eax                          // subarray is empty
  jz    @@end

  movzx   edx, dl                           // extract lower byte to ed x
  mov     eax, dword ptr [eax + edx * 4]    // copy array element to eax

@@end:
{$endif}
end;


procedure TtsFont.GetTextMetric(var Metric: TtsTextMetric);
begin
  Metric.Ascent := Ascent;
  Metric.Descent := Descent;
  Metric.LineSkip := Ascent + Descent + ExternalLeading;
  Metric.LineSkip_with_LineSpace := Metric.LineSkip + LineSpacing;
end;


// May be fpc has problems because it's an virtual function 
function TtsFont.Validate(CharCode: WideChar): Boolean;
//{$IFDEF TS_PURE_PASCAL}
begin
  Result := GetChar(CharCode) <> nil;
//{$else}
//asm
//  // self is still in eax
//  // charcode is still is edx
//  call    TtsFont.GetChar
//  test    eax, eax
//  setnz   al
//{$endif}
end;


{ TtsFontCreator }

procedure TtsFontCreator.AddChar(CharCode: WideChar);
var
  tsChar: TtsChar;

  GlyphOriginX, GlyphOriginY, GlyphWidth, GlyphHeight, Advance: Integer;
  CharImage: TtsImage;
begin
  if fCreateChars and (Ord(CharCode) > 0) then begin
    tsChar := GetChar(CharCode);

    // Check if the char allready was added
    if tsChar = nil then begin
      // check if the Char exists in the font
      if GetGlyphMetrics(CharCode, GlyphOriginX, GlyphOriginY, GlyphWidth, GlyphHeight, Advance) then
        if (GlyphOriginX <> 0) or (GlyphOriginY <> 0) or (GlyphWidth <> 0) or (GlyphHeight <> 0) or (Advance <> 0) then begin
          // Getting Image of Char
          CharImage := TtsImage.Create;
          try
            if fRenderer.SaveImages then begin
              if (GlyphWidth > 0) and (GlyphHeight > 0) then begin
                // getting char image
                GetCharImage(CharCode, CharImage);
              end;
            end;

            if (tsStyleUnderline in Style) or (tsStyleStrikeout in Style) then begin
              if (CharImage.Width = 0) and (CharImage.Height = 0) then begin
                CharImage.CreateEmpty(tsFormatRGBA8, Advance, 1);
                GlyphOriginY := 1;
              end;
            end;

            // Create new Entry for Char
            tsChar := TtsChar.Create(CharCode);
            tsChar.GlyphOriginX := GlyphOriginX;
            tsChar.GlyphOriginY := GlyphOriginY;
            tsChar.Advance      := Advance;
            tsChar.GlyphRect.Left   := 0;
            tsChar.GlyphRect.Top    := 0;
            tsChar.GlyphRect.Right  := CharImage.Width;
            tsChar.GlyphRect.Bottom := CharImage.Height;

            AddChar(CharCode, tsChar);

            if fRenderer.SaveImages then begin
              try
                // apply underline style
                if tsStyleUnderline in Style then
                  DrawLine(tsChar, CharImage, UnderlinePosition, UnderlineSize);

                // apply strikeout stlye
                if tsStyleStrikeout in Style then
                  DrawLine(tsChar, CharImage, StrikeoutPosition, StrikeoutSize);
              except
                CharImage.FillColor(1, 0, 0, 0, TS_CHANNELS_RGB, cModesNormal);
              end;

              // PostProcessing
              DoPostProcess(CharImage, tsChar);

              // Add invisible border for resizing (at last before adding)
              if AddResizingBorder then begin
                tsChar.HasResizingBorder := True;

                CharImage.AddResizingBorder(tsChar);
              end;

              // Add Image to Renderer
              tsChar.RendererImageReference := fRenderer.AddImage(tsChar, CharImage);
            end;
          finally
            FreeAndNil(CharImage);
          end;
        end;
    end;
  end;
end;


function TtsFontCreator.AddPostProcessStep(PostProcessStep: TtsPostProcessStep): TtsPostProcessStep;
begin
  Result := PostProcessStep;

  fPostProcessSteps.Add(PostProcessStep);
end;


procedure TtsFontCreator.ClearPostProcessSteps;
var
  Idx: Integer;
begin
  for Idx := fPostProcessSteps.Count -1 downto 0 do
    DeletePostProcessStep(Idx);

  fPostProcessSteps.Clear;
end;


constructor TtsFontCreator.Create(Renderer: TtsRenderer; Size: Integer; Style: TtsFontStyles; Format: TtsFormat; AntiAliasing: TtsAntiAliasing);
begin
  inherited Create(Renderer, Size, Style, Format, AntiAliasing);

  fCreateChars := True;
  
  fPostProcessSteps := TList.Create;
end;


procedure TtsFontCreator.DeletePostProcessStep(Index: Integer);
var
  Entry: TtsPostProcessStep;
begin
  if (Index >= 0) and (Index < fPostProcessSteps.Count) then begin
    Entry := fPostProcessSteps[Index];
    Entry.Free;

    fPostProcessSteps.Delete(Index);
  end;
end;


destructor TtsFontCreator.Destroy;
begin
  if fPostProcessSteps <> nil then begin
    ClearPostProcessSteps;
    FreeAndNil(fPostProcessSteps);
  end;

  inherited;
end;


procedure TtsFontCreator.DoPostProcess(var CharImage: TtsImage; const tsChar: TtsChar);
var
  Idx: Integer;
  Entry: TtsPostProcessStep;
begin
  if not CharImage.Empty then begin
    for Idx := 0 to fPostProcessSteps.Count - 1 do begin
      Entry := fPostProcessSteps[Idx];

      if Entry.IsInRange(tsChar.CharCode) then
        Entry.PostProcess(CharImage, tsChar);
    end;
  end;
end;


procedure TtsFontCreator.DrawLine(Char: TtsChar; CharImage: TtsImage; LinePosition, LineSize: Integer);
var
  NewWidth, NewHeight, NewPosX, NewPosY, YOffset, Idx: Integer;

  
  procedure FillLine(pPix: ptsColor);
  var
    Idx: Integer;
  begin
    Idx := NewWidth;
    while Idx > 0 do begin
      pPix^.Red   := $FF;
      pPix^.Green := $FF;
      pPix^.Blue  := $FF;
      pPix^.Alpha := $FF;

      Inc(pPix);
      Dec(Idx);
    end;
  end;

begin
  if LineSize <= 0 then
    Exit;

  LinePosition := LinePosition - LineSize;

  // calculate width and height
  NewWidth := CharImage.Width;
  NewPosX := 0;
  NewHeight := CharImage.Height;
  NewPosY := 0;

  // expand image to the full advance
  if Char.Advance > CharImage.Width then
    NewWidth := Char.Advance;

  // add glyph position to image width and set position
  if Char.GlyphOriginX > Char.GlyphRect.Left then begin
    NewWidth := NewWidth + Char.GlyphOriginX;
    NewPosX := Char.GlyphOriginX;
  end;

  if Char.GlyphOriginX < 0 then
    NewWidth := NewWidth - Char.GlyphOriginX;

  // line is under the image
  if LinePosition < (Char.GlyphOriginY - CharImage.Height) then
    NewHeight := NewHeight + (Char.GlyphOriginY - CharImage.Height - LinePosition);

  // line is above the image
  if LinePosition + LineSize > Char.GlyphOriginY then begin
    NewPosY := ((LinePosition + LineSize) - Char.GlyphOriginY);
    NewHeight := NewHeight + NewPosY;
  end;

  // resize
  CharImage.Resize(NewWidth, NewHeight, NewPosX, NewPosY);

  // draw lines
  YOffset := (Char.GlyphOriginY + NewPosY) - LinePosition;
  for Idx := 1 to LineSize do
    FillLine(CharImage.ScanLine[YOffset - Idx]);

  // move glyph rect
  Char.GlyphRect.Left   := Char.GlyphRect.Left   + NewPosX;
  Char.GlyphRect.Right  := Char.GlyphRect.Right  + NewPosX;
  Char.GlyphRect.Top    := Char.GlyphRect.Top    + NewPosY;
  Char.GlyphRect.Bottom := Char.GlyphRect.Bottom + NewPosY;
end;


function TtsFontCreator.GetPostProcessStep(Index: Integer): TtsPostProcessStep;
begin
  if (Index >= 0) and (Index < fPostProcessSteps.Count) then
    Result := TtsPostProcessStep(fPostProcessSteps[Index])
  else
    Result := nil;
end;


function TtsFontCreator.GetPostProcessStepCount: Integer;
begin
  Result := fPostProcessSteps.Count;
end;


function TtsFontCreator.Validate(CharCode: WideChar): Boolean;
begin
  Result := Inherited Validate(CharCode);

  // if char wasnt found then create it.
  if not Result then begin
    AddChar(CharCode);

    // and test for creation
    Result := Inherited Validate(CharCode);
  end;
end;


{ TtsPostProcessStep }

procedure TtsPostProcessStep.AddUsageChars(Usage: TtsFontProcessStepUsage; Chars: pWideChar);
begin
  if Chars <> nil then
    while Chars^ <> #0 do begin
      AddUsageRange(Usage, Chars^, Chars^);

      Inc(Chars);
    end;
end;


procedure TtsPostProcessStep.AddUsageRange(Usage: TtsFontProcessStepUsage;
  StartChar, EndChar: WideChar);
var
  pItem: PtsPostProcessStepRange;
begin
  New(pItem);

  pItem^.StartChar := StartChar;
  pItem^.EndChar := EndChar;

  case Usage of
    tsUInclude:
      fIncludeCharRange.Add(pItem);
    tsUExclude:
      fExcludeCharRange.Add(pItem);
  end;
end;


procedure TtsPostProcessStep.ClearExcludeRange;
begin
  ClearList(fExcludeCharRange);
end;


procedure TtsPostProcessStep.ClearIncludeRange;
begin
  ClearList(fIncludeCharRange);
end;


procedure TtsPostProcessStep.ClearList(List: TList);
var
  Idx: Integer;
  pItem: PtsPostProcessStepRange;
begin
  for Idx := 0 to List.Count - 1 do begin
    pItem := List[Idx];
    Dispose(pItem);
  end;

  List.Clear;
end;


constructor TtsPostProcessStep.Create;
begin
  inherited Create;

  fIncludeCharRange := TList.Create;
  fExcludeCharRange := TList.Create;
end;


destructor TtsPostProcessStep.Destroy;
begin
  ClearIncludeRange;
  ClearExcludeRange;

  fIncludeCharRange.Free;
  fExcludeCharRange.Free;

  inherited;
end;


function TtsPostProcessStep.IsInRange(CharCode: WideChar): Boolean;
var
  Idx: Integer;
  pItem: PtsPostProcessStepRange;
begin
  // Look in include range
  if fIncludeCharRange.Count <> 0 then begin
    Result := False;

    for Idx := 0 to fIncludeCharRange.Count - 1 do begin
      pItem := fIncludeCharRange[Idx];

      if (CharCode >= pItem^.StartChar) and (CharCode <= pItem^.EndChar) then begin
        Result := True;
        Break;
      end;
    end;
  end else
    Result := True;

  // Look in exclude range but only if its included
  if Result then begin
    for Idx := 0 to fExcludeCharRange.Count - 1 do begin
      pItem := fExcludeCharRange[Idx];

      if (CharCode >= pItem^.StartChar) and (CharCode <= pItem^.EndChar) then begin
        Result := False;
        Break;
      end;
    end;
  end;
end;


{ TtsFontCreatorSDL }

constructor TtsFontCreatorSDL.Create(Renderer: TtsRenderer; const Filename: AnsiString; Size: Integer;
  Style: TtsFontStyles; Format: TtsFormat; AntiAliasing: TtsAntiAliasing);
var
  TempStyle: Integer;
begin
  inherited Create(Renderer, Size, Style, Format, AntiAliasing);

  // Init SDL_ttf
  if (TTF_WasInit = 0) then
    if (TTF_Init < 0) then
      raise Exception.Create('TtsFontCreator.Create: TTF_Init error');

  // Create FFT_Font
  fSDLFont := TTF_OpenFont(pAnsiChar(Filename), Size);

  // Getting style  - SDL_ttf dosn't support it. so we only have normal
  fFontFileStyle := TS_STYLE_NORMAL;

  // getting props
  Ascent := TTF_FontAscent(fSDLFont);
  Descent := -TTF_FontDescent(fSDLFont);
  ExternalLeading := TTF_FontLineSkip(fSDLFont) - (Ascent + Descent); 

  // SDL_ttf dosn't support it so we must calculate it by our self
  UnderlinePosition := - round(Ascent / 8);
  if UnderlinePosition > -1 then
    UnderlinePosition := -1;

  if tsStyleBold in Style then
    UnderlineSize := round(Ascent / 8)
  else
    UnderlineSize := round(Ascent / 13);
  if UnderlineSize < 1 then
    UnderlineSize := 1;

  StrikeoutPosition := round(Ascent / 3.5);
  if tsStyleBold in Style then
    StrikeoutSize := round(Ascent / 14)
  else
    StrikeoutSize := round(Ascent / 19);
  if StrikeoutSize < 1 then
    StrikeoutSize := 1;

  FixedWidth := TTF_FontFaceIsFixedWidth(fSDLFont) > 0;

  Copyright := '';
  FaceName := TTF_FontFaceFamilyName(fSDLFont);
  StyleName := TTF_FontFaceStyleName(fSDLFont);
  FullName := FaceName + #32 + StyleName;

  // Set style
  TempStyle := 0;

  if tsStyleBold in Style then
    TempStyle := TempStyle or TTF_STYLE_BOLD;
  if tsStyleItalic in Style then
    TempStyle := TempStyle or TTF_STYLE_ITALIC;
//  if tsStyleUnderline in Style then
//    TempStyle := TempStyle or TTF_STYLE_UNDERLINE;

  TTF_SetFontStyle(fSDLFont, TempStyle);
end;


destructor TtsFontCreatorSDL.Destroy;
begin
  // Destroy Font
  TTF_CloseFont(fSDLFont);
  fSDLFont := nil;

  inherited;
end;


procedure TtsFontCreatorSDL.GetCharImage(CharCode: WideChar; const CharImage: TtsImage);
const
	WHITE: TSDL_Color = (r: $FF; g: $FF; b: $FF; unused: 0);
	BLACK: TSDL_Color = (r: $00; g: $00; b: $00; unused: 0);

var
  CharSurface: PSDL_Surface;

  X, Y, TempWidth: Integer;
  pSource: pByte;
  pDest: PtsColor;


  function GetPaletteEntry(Index: Byte): Byte;
  begin
    Result := 0;

    with CharSurface^.format^ do begin
      if palette <> nil then
        if (palette^.ncolors > 0) and (Index < palette^.ncolors) then
          Result := palette^.colors[Index].r
    end;
  end;


begin
  //CharCode: Needs to use an widestring because of #0 endchar

  case AntiAliasing of
    tsAANone:
      CharSurface := TTF_RenderGlyph_Solid(fSDLFont, Ord(CharCode), WHITE);
    tsAANormal:
      CharSurface := TTF_RenderGlyph_Shaded(fSDLFont, Ord(CharCode), WHITE, BLACK);
  end;

  
  if CharSurface <> nil then
    try
      CharImage.CreateEmpty(fFormat, CharSurface^.Width, CharSurface^.Height);
      try
        TempWidth := CharSurface^.Width;
        if TempWidth mod 4 > 0 then
          TempWidth := (TempWidth div 4 + 1) * 4;

        for Y := 0 to CharSurface^.Height - 1 do begin
          pDest := CharImage.ScanLine[Y];
          pSource := CharSurface^.Pixels;
          Inc(pSource, Y * TempWidth);

          for X := 0 to CharSurface^.Width - 1 do begin
            pDest^.Red   := $FF;
            pDest^.Green := $FF;
            pDest^.Blue  := $FF;
            pDest^.Alpha := GetPaletteEntry(pSource^);

            Inc(pSource);
            Inc(pDest);
          end;
        end;
      except
        CharImage.Free;
      end;
    finally
      SDL_FreeSurface(CharSurface);
    end;
end;


function TtsFontCreatorSDL.GetGlyphMetrics(CharCode: WideChar; var GlyphOriginX, GlyphOriginY, GlyphWidth, GlyphHeight, Advance: Integer): Boolean;
var
  MinX, MaxX, MinY, MaxY: Integer;
begin
  if fSDLFont <> nil then begin
    Result := TTF_GlyphMetrics(fSDLFont, Ord(CharCode), MinX, MaxX, MinY, MaxY, Advance) = 0;
    GlyphWidth := MaxX - MinX;
    GlyphHeight := MaxY - MinY;

    GlyphOriginX := MinX;
    GlyphOriginY := GlyphHeight + MinY;
  end

  else
    Result := False;
end;


{ TtsFontCreatorGDIFontFace }

constructor TtsFontCreatorGDIFontFace.Create(Renderer: TtsRenderer; const Fontname: AnsiString;
  Size: Integer; Style: TtsFontStyles; Format: TtsFormat; AntiAliasing: TtsAntiAliasing);
var
  Idx: Integer;
  LogFont: TLogFontA;

  DC: HDC;

  TableName: Cardinal;
  Buffer: Pointer;
  BufferSize: Cardinal;
  Lang: AnsiString;

  TextMetric: TTextMetricW;
  OutTextMetric: TOutlineTextmetricW;
begin
  inherited Create (Renderer, Size, Style, Format, AntiAliasing);

  // setting up matrix
  FillChar(fMat2, SizeOf(TMat2), $00);

  fMat2.eM11.Value := 1;
  fMat2.eM22.Value := 1;

  fFontname := Fontname;

  // Creating Font
  FillChar(LogFont, SizeOf(LogFont), 0);

  // name
  fFontname := Fontname;
  for Idx := 1 to min(Length(Fontname), Length(LogFont.lfFaceName)) do
    LogFont.lfFaceName[Idx -1] := Fontname[Idx];

  // char set
  LogFont.lfCharSet := DEFAULT_CHARSET;
  
  // size
//  fPointSize := PointSize;
  LogFont.lfHeight := -Size; //-MulDiv(PointSize, GetDeviceCaps(Temp.Canvas.Handle, LOGPIXELSY), 72);

  // style
  if tsStyleBold in Style then
    LogFont.lfWeight := FW_BOLD
  else
    LogFont.lfWeight := FW_NORMAL;

  if tsStyleItalic in Style then
    LogFont.lfItalic := 1;

  if tsStyleUnderline in Style then
    LogFont.lfUnderline := 1;

  // smooth
  case AntiAliasing of
    tsAANone:
      LogFont.lfQuality := NONANTIALIASED_QUALITY;
    tsAANormal:
      LogFont.lfQuality := ANTIALIASED_QUALITY;
//    tsSmoothSmooth:
//      begin
//        if Smooth = tsSmoothSmooth then
//          fMat2.eM11.Value := 3;
//      end;
  end;

  // create font
  fFontHandle := CreateFontIndirectA(LogFont);

  // Getting informations about font
  DC := CreateCompatibleDC(0);
  try
    SelectObject(DC, fFontHandle);

    // find strings in text
    TableName := MakeTTTableName('n', 'a', 'm', 'e');
    BufferSize := GetFontData(DC, TableName, 0, nil, 0);

    if BufferSize <> GDI_ERROR then begin
      GetMem(Buffer, BufferSize);
      try
        if GetFontData(DC, TableName, 0, Buffer, BufferSize) <> GDI_ERROR then begin
          SetLength(Lang, 4);
          GetLocaleInfoA(LOCALE_USER_DEFAULT, LOCALE_ILANGUAGE, @Lang[1], 4);

          GetTTString(Buffer, BufferSize, NAME_ID_COPYRIGHT, StrToInt('$' + String(Lang)), fCopyright);
          GetTTString(Buffer, BufferSize, NAME_ID_FACE_NAME, StrToInt('$' + String(Lang)), fFaceName);
          GetTTString(Buffer, BufferSize, NAME_ID_STYLE_NAME, StrToInt('$' + String(Lang)), fStyleName);
          GetTTString(Buffer, BufferSize, NAME_ID_FULL_NAME, StrToInt('$' + String(Lang)), fFullName);
        end;
      finally
        FreeMem(Buffer);
      end;
    end;

    // Text Metric
    GetTextMetricsW(DC, TextMetric);

    Ascent := TextMetric.tmAscent;
    Descent := TextMetric.tmDescent;
    ExternalLeading := TextMetric.tmExternalLeading;

    DefaultChar := TextMetric.tmDefaultChar;

    // inverse logic of the bit. clear then fixed pitch
    FixedWidth := TextMetric.tmPitchAndFamily and TMPF_FIXED_PITCH = 0;

    // style
    FontFileStyle := TS_STYLE_NORMAL;

    if TextMetric.tmWeight > 400 then
      FontFileStyle := FontFileStyle or TS_STYLE_BOLD;

    if TextMetric.tmItalic > 0 then
      FontFileStyle := FontFileStyle or TS_STYLE_ITALIC;

    if TextMetric.tmUnderlined > 0 then
      FontFileStyle := FontFileStyle or TS_STYLE_UNDERLINE;

    if TextMetric.tmStruckOut > 0 then
      FontFileStyle := FontFileStyle or TS_STYLE_STRIKEOUT;

    // Outline Text Metric
    GetOutlineTextMetricsW(DC, SizeOf(OutTextMetric), OutTextMetric);

    UnderlinePosition := OutTextMetric.otmsUnderscorePosition;
    UnderlineSize := OutTextMetric.otmsUnderscoreSize;
    if UnderlineSize < 1 then
      UnderlineSize := 1;

    StrikeoutPosition := OutTextMetric.otmsStrikeoutPosition;
    StrikeoutSize := OutTextMetric.otmsStrikeoutSize;
    if StrikeoutSize < 1 then
      StrikeoutSize := 1;
  finally
    DeleteDC(DC);
  end;
end;


destructor TtsFontCreatorGDIFontFace.Destroy;
begin
  DeleteObject(fFontHandle);

  inherited;
end;


procedure TtsFontCreatorGDIFontFace.GetCharImage(CharCode: WideChar; const CharImage: TtsImage);
var
  DC: HDC;
begin
  DC := CreateCompatibleDC(0);
  try
    SelectObject(DC, fFontHandle);

    case AntiAliasing of
      tsAANone:
        GetCharImageNone(DC, CharCode, CharImage);
      tsAANormal:
        GetCharImageAntialiased(DC, CharCode, CharImage);
    end;
  finally
    DeleteDC(DC);
  end;
end;


procedure TtsFontCreatorGDIFontFace.GetCharImageAntialiased(DC: HDC; CharCode: WideChar; const CharImage: TtsImage);
var
  Metric: TGlyphMetrics;
  pBuffer: Pointer;
  Size, OutlineResult: Cardinal;
  GlyphIndex: Integer;
  X, Y, Height, Width, Spacer: Integer;
  pDest: PtsColor;
  pSrc: pByte;


  procedure CopyPixel;
  var
    Idx: Integer;
    Temp, Count: Cardinal;
  begin
    Count := Min(X, fMat2.eM11.Value);

    Temp := 0;
    for Idx := 0 to Count -1 do begin
      Temp := Temp + pSrc^;
      Inc(pSrc);
    end;

    Dec(X, Count);

    pDest^.Red   := $FF;
    pDest^.Green := $FF;
    pDest^.Blue  := $FF;
    pDest^.Alpha := $FF * Temp div ($40 * Cardinal(fMat2.eM11.Value));

    Inc(pDest);
  end;


begin
  FillChar(Metric, SizeOf(TGlyphMetrics), $00);

  // Translate Glyphindex
  GlyphIndex := GetGlyphIndex(CharCode);

  // size
//  if GlyphIndex <> 0 then
    Size := GetGlyphOutlineA(DC, GlyphIndex, GGO_GRAY8_BITMAP or GGO_GLYPH_INDEX, @Metric, 0, nil, @fMat2);
//  else
//    Size := GetGlyphOutlineA(DC, Ord(fDefaultChar), GGO_GRAY8_BITMAP, Metric, 0, nil, fMat2);

  if (Size <> GDI_ERROR) and (Size <> 0) then begin
    GetMem(pBuffer, Size);
    try
      // glyphdata
//      if GlyphIndex <> 0 then
        OutlineResult := GetGlyphOutlineA(DC, GlyphIndex, GGO_GRAY8_BITMAP or GGO_GLYPH_INDEX, @Metric, Size, pBuffer, @fMat2);
//      else
//        OutlineResult := GetGlyphOutlineA(DC, Ord(fDefaultChar), GGO_GRAY8_BITMAP, Metric, Size, pBuffer, fMat2);

      if OutlineResult <> GDI_ERROR then begin
        // Image size
        Height := Metric.gmBlackBoxY;
        Width := Integer(Metric.gmBlackBoxX) div fMat2.eM11.Value;
        if (Integer(Metric.gmBlackBoxX) mod fMat2.eM11.Value) <> 0 then
          Width := Width + fMat2.eM11.Value - (Integer(Metric.gmBlackBoxX) mod fMat2.eM11.Value);

        // spacer
        if (Metric.gmBlackBoxX mod 4) <> 0 then
          Spacer := 4 - (Metric.gmBlackBoxX mod 4)
        else
          Spacer := 0;

        // copy image
        if (Height > 0) and (Width > 0) then begin
          CharImage.CreateEmpty(fFormat, Width, Height);

          pSrc := pBuffer;

          for Y := 0 to Height -1 do begin
            pDest := CharImage.ScanLine[Y];

            X := Metric.gmBlackBoxX;
            while X > 0 do
              CopyPixel;

            if Spacer <> 0 then
              Inc(pSrc, Spacer);
          end;
        end;
      end;
    finally
      FreeMem(pBuffer);
    end;
  end;
end;


procedure TtsFontCreatorGDIFontFace.GetCharImageNone(DC: HDC; CharCode: WideChar; const CharImage: TtsImage);
var
  Metric: TGlyphMetrics;
  pBuffer: Pointer;
  Size, OutlineResult: Cardinal;
  GlyphIndex: Integer;
  X, Y, Height, Width, SourceX, SourceWidth: Integer;
  pDest: PtsColor;
  pSrc: pByte;


  procedure ExpandByte;
  var
    Idx, Count, SourceCount: Integer;
  begin
    SourceCount := Min(8, SourceX);
    Count := Min(8, X);

    for Idx := 1 to Count do begin
      pDest^.Red   := $FF;
      pDest^.Green := $FF;
      pDest^.Blue  := $FF;

      if (pSrc^ and $80) > 0 then
        pDest^.Alpha := $FF
      else
        pDest^.Alpha := $00;

      pSrc^ := (pSrc^ and not $80) shl 1;

      Inc(pDest);
    end;

    Dec(SourceX, SourceCount);
    Dec(X, Count);
  end;


begin
  // fMat2.eM11.Value must be 1
  Assert(fMat2.eM11.Value = 1);

  FillChar(Metric, SizeOf(TGlyphMetrics), $00);

  // Translate Glyphindex
  GlyphIndex := GetGlyphIndex(CharCode);

  // size
//  if GlyphIndex <> 0 then
    Size := GetGlyphOutlineA(DC, GlyphIndex, GGO_BITMAP or GGO_GLYPH_INDEX, @Metric, 0, nil, @fMat2);
//  else
//    Size := GetGlyphOutlineA(DC, Ord(fDefaultChar), GGO_BITMAP, Metric, 0, nil, fMat2);

  if (Size <> GDI_ERROR) and (Size <> 0) then begin
    GetMem(pBuffer, Size);
    try
      // glyphdata
//      if GlyphIndex <> 0 then
        OutlineResult := GetGlyphOutlineA(DC, GlyphIndex, GGO_BITMAP or GGO_GLYPH_INDEX, @Metric, Size, pBuffer, @fMat2);
//      else
//        OutlineResult := GetGlyphOutlineA(DC, Ord(fDefaultChar), GGO_BITMAP, Metric, Size, pBuffer, fMat2);

      if OutlineResult <> GDI_ERROR then begin
        SourceWidth := (Size div Metric.gmBlackBoxY) * 8;
        Width := Metric.gmBlackBoxX;
        Height := Metric.gmBlackBoxY;

        // copy image
        if (Height > 0) and (Width > 0) then begin
          CharImage.CreateEmpty(tsFormatRGBA8, Width, Height);

          pSrc := pBuffer;

          for Y := 0 to Height -1 do begin
            pDest := CharImage.ScanLine[Y];

            // copy data
            SourceX := SourceWidth;
            X := Width;
            while SourceX > 0 do begin
              ExpandByte;

              Inc(pSrc);
            end;
          end;
        end;
      end;
    finally
      FreeMem(pBuffer);
    end;
  end;
end;


function TtsFontCreatorGDIFontFace.GetGlyphIndex(CharCode: WideChar): Integer;
var
//  ReadRawData: Boolean;
  DC: HDC;
  GCPRes: TGCPResultsW;
begin
  Result := 0;

//  ReadRawData := True;

  DC := CreateCompatibleDC(0);
  try
    SelectObject(DC, fFontHandle);

    // windows nt
    if Addr(GetCharacterPlacementW) <> nil then begin
      FillChar(GCPRes, SizeOf(GCPRes), 0);
      GetMem(GCPRes.lpGlyphs, SizeOf(Cardinal));
      try
        GCPRes.lStructSize := SizeOf(GCPRes);
        GCPRes.lpGlyphs^ := 0;
        GCPRes.nGlyphs := 1;

        if GetCharacterPlacementW(DC, @CharCode, 1, GCP_MAXEXTENT, @GCPRes, 0) <> GDI_ERROR then begin
          if (GCPRes.nGlyphs = 1) and (GCPRes.lpGlyphs <> nil) then begin
            Result := GCPRes.lpGlyphs^;

//            ReadRawData := False;
          end;
        end;
      finally
        FreeMem(GCPRes.lpGlyphs);
      end;
    end;

    // windows 9x workaround
//    ReadRawData := True;

//    if ReadRawData then begin
//      if GetTTUnicodeCharCount(DC) > 0 then
//        Result := GetTTUnicodeGlyphIndex(DC, Ord(CharCode));
//    end;
  finally
    DeleteDC(DC);
  end;
end;


function TtsFontCreatorGDIFontFace.GetGlyphMetrics(CharCode: WideChar; var GlyphOriginX, GlyphOriginY, GlyphWidth, GlyphHeight, Advance: Integer): Boolean;
var
  DC: HDC;
  Metric: TGlyphMetrics;
  Size: Cardinal;
  GlyphIndex: Integer;
begin
  Result := False;

  // Set values to 0
  GlyphOriginX := 0;
  GlyphOriginY := 0;
  GlyphWidth   := 0;
  GlyphHeight  := 0;
  Advance := 0;

  // Translate Glyphindex
  GlyphIndex := GetGlyphIndex(CharCode);

  DC := CreateCompatibleDC(0);
  try
    SelectObject(DC, fFontHandle);

    // get value of resulting bitmaps
    case AntiAliasing of
      tsAANone: begin
//          if GlyphIndex <> 0 then
            Size := GetGlyphOutlineA(DC, GlyphIndex, GGO_BITMAP or GGO_GLYPH_INDEX, @Metric, 0, nil, @fMat2);
//          else
//            Size := GetGlyphOutlineA(DC, Ord(fDefaultChar), GGO_BITMAP, Metric, 0, nil, fMat2);
        end;
      tsAANormal: begin
//          if GlyphIndex <> 0 then
            Size := GetGlyphOutlineA(DC, GlyphIndex, GGO_GRAY8_BITMAP or GGO_GLYPH_INDEX, @Metric, 0, nil, @fMat2);
//          else
//            Size := GetGlyphOutlineA(DC, Ord(fDefaultChar), GGO_GRAY8_BITMAP, Metric, 0, nil, fMat2);
        end;
      else
        Size := 0;
    end;

    // dosn't work so get metric value
    if (Size = GDI_ERROR) or (Size = 0) then begin
//      if GlyphIndex <> 0 then
        Size := GetGlyphOutlineA(DC, GlyphIndex, GGO_METRICS or GGO_GLYPH_INDEX, @Metric, 0, nil, @fMat2);
//      else
//        Size := GetGlyphOutlineA(DC, Ord(fDefaultChar), GGO_METRICS, Metric, 0, nil, fMat2);
    end;

    // we have values?
    if (Size <> GDI_ERROR) and (Size > 0) then begin
      GlyphOriginX := Round(Metric.gmptGlyphOrigin.X / fMat2.eM11.value);
      GlyphOriginY := Metric.gmptGlyphOrigin.Y;
      GlyphWidth   := Round(Metric.gmBlackBoxX / fMat2.eM11.value);
      GlyphHeight  := Metric.gmBlackBoxY;

      Advance := Round(Metric.gmCellIncX / fMat2.eM11.value);

      Result := True;
    end;
  finally
    DeleteDC(DC)
  end;
end;


{ TtsFontCreatorGDIFile }

constructor TtsFontCreatorGDIFile.Create(Renderer: TtsRenderer; const Filename: AnsiString;
  Size: Integer; Style: TtsFontStyles; Format: TtsFormat; AntiAliasing: TtsAntiAliasing);
var
  FaceName: AnsiString;
begin
  // filename
  fFileName := StrNew(pAnsiChar(Filename));

  fFontRegistred := false;
  FaceName := '';

  if GetFaceName(fFilename, FaceName) then 
    fFontRegistred := RegisterFont(fFilename, False);

  // inherited
  inherited Create(Renderer, FaceName, Size, Style, Format, AntiAliasing);
end;


destructor TtsFontCreatorGDIFile.Destroy;
begin
  inherited;

  // unregister font
  if fFontRegistred then
    UnRegisterFont(fFilename, False);

  StrDispose(fFileName);
end;


function TtsFontCreatorGDIFile.GetFaceName(Filename: PAnsiChar; var Face: AnsiString): boolean;
var
  Lang: AnsiString;
begin
  SetLength(Lang, 4);
  GetLocaleInfoA(LOCALE_USER_DEFAULT, LOCALE_ILANGUAGE, @Lang[1], 4);

  Face := GetTTFontFullNameFromFile(Filename, StrToInt('$' + String(Lang)));

  Result := Face <> '';
end;


function TtsFontCreatorGDIFile.RegisterFont(Filename: pAnsiChar; RegisterPublic: Boolean): boolean;
var
  Flags: Cardinal;
begin
  Result := False;

  // Flags
  if not RegisterPublic then
    Flags := FR_PRIVATE or FR_NOT_ENUM
  else
    Flags := 0;

  // AddFontResource
  if Addr(AddFontResourceExA) <> nil then
    Result := AddFontResourceExA(FileName, Flags, nil) > 0
  else

  if Addr(AddFontResourceA) <> nil then
    Result := AddFontResourceA(FileName) > 0;
end;


function TtsFontCreatorGDIFile.UnRegisterFont(Filename: pAnsiChar; RegisterPublic: Boolean): boolean;
var
  Flags: Cardinal;
begin
  Result := False;

  // Flags
  if not RegisterPublic then
    Flags := FR_PRIVATE or FR_NOT_ENUM
  else
    Flags := 0;

  // RemoveFontResource
  if Addr(RemoveFontResourceExA) <> nil then
    Result := RemoveFontResourceExA(FileName, Flags, nil)
  else

  if Addr(RemoveFontResourceA) <> nil then
    Result := RemoveFontResourceA(FileName);
end;

{ TtsFontCreatorGDIFile }

constructor TtsFontCreatorGDIStream.Create(Renderer: TtsRenderer; const Source: TStream;
  Size: Integer; Style: TtsFontStyles; Format: TtsFormat; AntiAliasing: TtsAntiAliasing);
var
  FaceName: AnsiString;
begin
  fFontRegistred := false;
  FaceName := '';

  if GetFaceName(Source, FaceName) then
    fFontRegistred := RegisterFont(Source);

  // inherited
  inherited Create(Renderer, FaceName, Size, Style, Format, AntiAliasing);
end;


destructor TtsFontCreatorGDIStream.Destroy;
begin
  inherited;

  // unregister font
  if fFontRegistred then
    UnRegisterFont();
end;


function TtsFontCreatorGDIStream.GetFaceName(Stream: TStream; var Face: AnsiString): boolean;
var
  Lang: AnsiString;
begin
  SetLength(Lang, 4);
  GetLocaleInfoA(LOCALE_USER_DEFAULT, LOCALE_ILANGUAGE, @Lang[1], 4);

  Face := GetTTFontFullNameFromStream(Stream, StrToInt('$' + String(Lang)));

  Result := Face <> '';
end;


function TtsFontCreatorGDIStream.RegisterFont(Data: TStream): boolean;
var
  ms: TMemoryStream;
  cnt: DWORD;
begin
  Result := False;
  fHandle := 0;

  ms:= TMemoryStream.Create;
  try
    ms.CopyFrom(Data, 0);
    if Addr(AddFontMemResourceEx)<>nil then
      fHandle:= AddFontMemResourceEx(ms.Memory, ms.Size, nil, @cnt);
    Result:= fHandle > 0;
  finally
    ms.Free;
  end;
end;


function TtsFontCreatorGDIStream.UnRegisterFont(): boolean;
begin
  Result := RemoveFontMemResourceEx(fHandle);
end;

{ TtsRenderer }

procedure TtsRenderer.BeginBlock(Left, Top, Width, Height: Integer; Flags: tsBitmask);
begin
  fisBlock := True;

  fBlockLeft := Left;
  fBlockTop := Top;
  fBlockWidth := Width;
  fBlockHeight := Height;
  fFlags := Flags;

  fWordWrap := fFlags and TS_BLOCKFLAG_WORD_WRAP = TS_BLOCKFLAG_WORD_WRAP;
//  fSingleLine := fFlags and TS_BLOCKFLAG_SINGLE_LINE = TS_BLOCKFLAG_SINGLE_LINE;

  fLineTop := Top + tsGetParameteri(TS_BLOCK_OFFSET_Y);
  fTextOffsetY := 0;
  fTextOffsetX := 0;

  with fLinesTemp do begin
    New(Lines);

    with Lines^ do begin
      NextLine := nil;
      LineItemFirst := nil;
      LineItemLast := nil;
      LineLength := 0;
      LineAutoBreak := False;
    end;
    Empty := True;
  end;

  fLinesFirst := nil;
  fLinesLast := nil;

  // if font is active add to list
  if fActiveFont <> nil then
    FontActivate(fActiveFontID);
end;


function TtsRenderer.CalculateLinesHeight(pLinesItem: PtsLinesItem): Integer;
var
  pLine: PtsLineItem;
  Metric: TtsTextMetric;
begin
  Result := 0;

  while pLinesItem <> nil do begin
    pLine := pLinesItem^.LineItemFirst;

    GetLineMetric(pLine, Metric);
    Result := Result + Metric.LineSkip_with_LineSpace;

    pLinesItem := pLinesItem^.NextLine;
  end;

  // remove last linespace from the lines
  Result := Result - (Metric.LineSkip_with_LineSpace - Metric.LineSkip);
end;


procedure TtsRenderer.CalculateWordLength(Font: TtsFont; pWord: PtsLineItem);
var
  pTempWord: PWideChar;
  Char: TtsChar;
  CharSpacing: tsInt;
begin
  if pWord^.ItemType in [TS_BLOCK_WORD, TS_BLOCK_SPACE] then begin
    CharSpacing := fLastActiveFont.CharSpacing;

    pTempWord := pWord^.Word;
    pWord^.WordLength := 0;

    while pTempWord^ <> #0 do begin
      // normal char
      if Font.Validate(pTempWord^) then
        Char := Font.GetChar(pTempWord^)
      else

      // default char
      if Font.Validate(Font.DefaultChar) then
        Char := Font.GetChar(Font.DefaultChar)
      else
        Char := nil;

      if Char <> nil then begin
        pWord^.WordLength := pWord^.WordLength + Char.Advance + CharSpacing;
      end;

      Inc(pTempWord);
    end;
  end;
end;


procedure TtsRenderer.Color(Red, Green, Blue, Alpha: Single);
var
  LineItem: PtsLineItem;
begin
  if isBlock then begin
    New(LineItem);

    LineItem^.NextItem := nil;
    LineItem^.PrevItem := nil;
    LineItem^.ItemType := TS_BLOCK_COLOR;
    LineItem^.Red := Red;
    LineItem^.Green := Green;
    LineItem^.Blue := Blue;
    LineItem^.Alpha := Alpha;

    PushLineItem(LineItem);
  end else

  begin
    DrawSetColor(Red, Green, Blue, Alpha);    
  end;
end;


constructor TtsRenderer.Create(Context: TtsContext);
begin
  inherited Create;

  fContext := Context;

  fSaveImages := True;
end;


destructor TtsRenderer.Destroy;
begin
  if isBlock then
    EndBlock;

  inherited;
end;


procedure TtsRenderer.DrawLine(pLine: PtsLineItem; LineLength: Integer; LineBreak: Boolean);
var
  pText: PWideChar;
  Char: TtsChar;

  Metric: TtsTextMetric;

  TempLeft, Temp: Integer;
  DrawLeft, SpaceTemp: Single;
  DrawAscent, LineSkip: Integer;

  DrawText: Boolean;

  BlockSpaceCount: Integer;
  BlockSpaceWidth: Single;


  function CountSpaces(pLine: PtsLineItem): Integer;
  var
    pText: PWideChar;
  begin
    Result := 0;

    while pLine <> nil do begin
      case pLine^.ItemType of
        TS_BLOCK_SPACE: begin
          pText := pLine^.Word;

          // Enumerate Text
          while pText^ <> #0 do begin
            Inc(Result);
            Inc(pText);
          end;
        end;
      end;

      pLine := pLine^.NextItem;
    end;
  end;


begin
  if fFlags and TS_BLOCKFLAG_CALC_SIZE > 0 then
    Exit;

  BlockSpaceWidth := 0;
  DrawLeft := 0;
  TempLeft := 0;

  GetLineMetric(pLine, Metric);

  // set drawposition to new baseline
  DrawAscent := fLineTop + fTextOffsetY + Metric.Ascent;

  // increment linetop with height of line
  LineSkip := Metric.LineSkip;
  fLineTop := fLineTop + LineSkip;

  // clipping
  DrawText := True; 
  if fisBlock then begin 
    if not (fFlags and TS_BLOCKFLAG_NO_CLIP = TS_BLOCKFLAG_NO_CLIP) then begin
      case tsGetParameteri(TS_CLIP) of
        TS_CLIP_COMPLETE: begin
          if (fLineTop + fTextOffsetY < fBlockTop) or
             ((fLineTop + fTextOffsetY - LineSkip) > (fBlockTop + fBlockHeight)) then
            DrawText := False; 
        end;
        TS_CLIP_BORDER: begin
          if ((fLineTop + fTextOffsetY - LineSkip) < fBlockTop) or
             (fLineTop + fTextOffsetY > (fBlockTop + fBlockHeight)) then
            DrawText := False; 
        end;
      end;
    end;
  end;

  // TextBlock text alignment
  if isBlock then begin
    case tsGetParameteri(TS_ALIGN) of
      TS_ALIGN_CENTER:
        begin
          TempLeft := (fBlockWidth div 2) - (LineLength div 2);
        end;
      TS_ALIGN_RIGHT:
        begin
          TempLeft := fBlockWidth - LineLength;
        end;
      TS_ALIGN_BLOCK: begin
        if LineBreak then begin
          BlockSpaceCount := CountSpaces(pLine);

          if BlockSpaceCount > 0 then
            BlockSpaceWidth := (fBlockWidth - LineLength) / BlockSpaceCount;
        end;
      end;
    end;

    DrawSetPosition(fBlockLeft + TempLeft, DrawAscent);
  end else

  // Normal text alignment
  begin
    case tsGetParameteri(TS_ALIGN) of
      TS_ALIGN_CENTER:
        begin
          TempLeft := - (LineLength div 2);
        end;
      TS_ALIGN_RIGHT:
        begin
          TempLeft := - LineLength;
        end;
    end;

    DrawSetPositionRelative(TempLeft, 0);
  end;

  DrawSetPositionRelative(tsGetParameteri(TS_BLOCK_OFFSET_X), 0);

  // Enumerate LineItems
  while pLine <> nil do begin
    case pLine^.ItemType of
      TS_BLOCK_FONT: begin
        fActiveFont := pLine^.Font;
        fActiveFontID := pLine^.FontID;
      end;

      TS_BLOCK_COLOR: begin
        DrawSetColor(pLine^.Red, pLine^.Green, pLine^.Blue, pLine^.Alpha);
      end;

      TS_BLOCK_WORD: begin
        if DrawText then begin
          if fActiveFont <> nil then begin
            pText := pLine^.Word;

            // Enumerate Text
            while pText^ <> #0 do begin
              // normal char
              if fActiveFont.Validate(pText^) then
                Char := fActiveFont.GetChar(pText^)
              else

              // default char
              if fActiveFont.Validate(fActiveFont.DefaultChar) then
                Char := fActiveFont.GetChar(fActiveFont.DefaultChar)
              else
                Char := nil;

              if Char <> nil then begin
                DrawSetPositionRelative(Char.GlyphOriginX, -fActiveFont.fBaselineOffset);
                DrawChar(fActiveFont, Char);
                DrawSetPositionRelative(Char.Advance - Char.GlyphOriginX + fActiveFont.CharSpacing, fActiveFont.fBaselineOffset);
              end;

              Inc(pText);
            end;
          end;
        end;
      end;

      TS_BLOCK_SPACE: begin
        if DrawText then begin
          if fActiveFont <> nil then begin
            pText := pLine^.Word;

            // Enumerate Text
            while pText^ <> #0 do begin
              // normal char
              if fActiveFont.Validate(pText^) then
                Char := fActiveFont.GetChar(pText^)
              else

              // default char
              if fActiveFont.Validate(fActiveFont.DefaultChar) then
                Char := fActiveFont.GetChar(fActiveFont.DefaultChar)
              else
                Char := nil;

              if Char <> nil then begin
                // We have lines so we must repeat the "empty" space
                if (tsStyleUnderline in fActiveFont.Style) or (tsStyleStrikeout in fActiveFont.Style) then begin
                  // width we need to draw
                  SpaceTemp := Char.Advance + fActiveFont.CharSpacing + BlockSpaceWidth;
                  // set the position to the normal end. Following we decrease
                  // these value by the width of the drawn chars. So we get the
                  // difference of the last drawn space.
                  DrawLeft := DrawLeft + Char.Advance + fActiveFont.CharSpacing + BlockSpaceWidth;

                  Temp := Char.Advance - Char.GlyphOriginX + fActiveFont.CharSpacing;

                  while SpaceTemp > 0 do begin
                    // draw the char
                    DrawSetPositionRelative(Char.GlyphOriginX, 0);
                    DrawChar(fActiveFont, Char);
                    // set the position inside the drawer
                    DrawSetPositionRelative(Temp, 0);

                    // decrease need to draw width
                    SpaceTemp := SpaceTemp - Temp;
                    // decrease the drawwidth with the width of the char.
                    DrawLeft := DrawLeft - Temp;
                  end;
                end else
                  // no lines so only set the position
                  DrawLeft := DrawLeft + Char.Advance + fActiveFont.CharSpacing + BlockSpaceWidth;
              end;

              Inc(pText);
            end;

            DrawSetPositionRelative(Round(DrawLeft), 0);
            DrawLeft := DrawLeft - Round(DrawLeft);
          end;
        end;
      end;

      TS_BLOCK_LINEBREAK: begin

      end;
//      TS_BLOCK_TAB: begin
//        case tsGetParameteri(TS_TAB) of
//          TS_TAB_FIXED:
//            begin
//              Temp := tsGetParameteri(TS_TAB_FIXED_WIDTH);
//
////              if (DrawLeft - fBlockLeft) mod Temp > 0 then
//              DrawLeft := (Round(DrawLeft) mod Temp) + Temp;
//            end;
//          TS_TAB_ABSOLUTE:
//            begin
//
//            end;
//        end;
//      end;
    end;

    pLine := pLine^.NextItem;
  end;
end;


procedure TtsRenderer.DrawLines(pLinesItem: PtsLinesItem);
begin
  if fFlags and TS_BLOCKFLAG_CALC_SIZE = 0 then begin
    while pLinesItem <> nil do begin
      DrawLine(pLinesItem^.LineItemFirst, pLinesItem^.LineLength, pLinesItem^.LineAutoBreak);

      pLinesItem := pLinesItem^.NextLine;
    end;
  end;
end;


procedure TtsRenderer.EndBlock;
var
  LinesHeight: Integer;
  VerticalAlign: tsEnum;
begin
  // if temp line exist then push them
  with fLinesTemp do begin
    if Lines <> nil then
      if Lines^.LineItemFirst <> nil then
        PushTempLines;

    FreeLines(Lines);
  end;

  // if vertical align isn't top
  VerticalAlign := tsGetParameteri(TS_VALIGN);
  if (VerticalAlign = TS_VALIGN_CENTER) or
     (VerticalAlign = TS_VALIGN_BOTTOM) then begin
    // calculating height
    LinesHeight := CalculateLinesHeight(fLinesFirst);

    // setting offset
    case VerticalAlign of
      TS_VALIGN_CENTER:
        fTextOffsetY := fTextOffsetY + (fBlockHeight div 2 - LinesHeight div 2);
      TS_VALIGN_BOTTOM:
        fTextOffsetY := fTextOffsetY + (fBlockHeight - LinesHeight);
    end;

    // drawing lines
    DrawLines(fLinesFirst);
  end;

  // Free all lines
  FreeLines(fLinesFirst);
  fLinesLast := nil;

  fisBlock := False;
end;


procedure TtsRenderer.FontActivate(FontID: Cardinal);
var
  pLine: PtsLineItem;
begin
  if FontID <> 0 then begin
    fLastActiveFont := fContext.FontGet(FontID);
    fLastActiveFontID := FontID;
  end else
    fLastActiveFont := nil;

  // if in block then add blockitem
  if isBlock then begin
    New(pLine);

    pLine^.NextItem := nil;
    pLine^.PrevItem := nil;
    pLine^.ItemType := TS_BLOCK_FONT;
    pLine^.FontID := FontID;
    pLine^.Font := fLastActiveFont;

    if pLine^.Font <> nil then
      PushLineItem(pLine)
    else
      Dispose(pLine);
  end else

  // activate font
  begin
    fActiveFontID := FontID;
    fActiveFont := fLastActiveFont;
  end;
end;


procedure TtsRenderer.FreeLines(var pLinesItem: PtsLinesItem);
var
  pTemp: PtsLinesItem;
begin

  while pLinesItem <> nil do begin
    pTemp := pLinesItem;

    FreeLineItems(pLinesItem^.LineItemFirst);
    pLinesItem^.LineItemLast := pLinesItem^.LineItemFirst;

    pLinesItem := pLinesItem^.NextLine;
    Dispose(pTemp);
  end;
end;


procedure TtsRenderer.FreeLineItems(var pLine: PtsLineItem);
var
  pTemp: PtsLineItem;
begin
  while pLine <> nil do begin
    pTemp := pLine;

    case pLine^.ItemType of
      TS_BLOCK_WORD, TS_BLOCK_SPACE:
        tsStrDispose(pLine^.Word);
    end;

    pLine := pLine^.NextItem;
    Dispose(pTemp);
  end;
end;


function TtsRenderer.GetActiveFont: TtsFont;
begin
  if fisBlock then
    Result := fLastActiveFont
  else
    Result := fActiveFont;
end;


function TtsRenderer.GetActiveFontID: Cardinal;
begin
  if fisBlock then
    Result := fLastActiveFontID
  else
    Result := fActiveFontID;
end;


procedure TtsRenderer.GetLineMetric(pLine: PtsLineItem; var Metric: TtsTextMetric);
var
  Font: TtsFont;
  Temp: TtsTextMetric;
begin
  // Defaults
  Metric.Ascent := 0;
  Metric.Descent := 0;
  Metric.LineSkip := 0;
  Metric.LineSkip_with_LineSpace := 0;

  // calculating lines
  Font := fActiveFont;

  while pLine <> nil do begin
    case pLine^.ItemType of
      TS_BLOCK_FONT: begin
        Font := pLine^.Font;
      end;

      TS_BLOCK_WORD, TS_BLOCK_SPACE, TS_BLOCK_LINEBREAK: begin
        if Font <> nil then begin
          Font.GetTextMetric(Temp);

          if Temp.Ascent > Metric.Ascent then
            Metric.Ascent := Temp.Ascent;

          if Temp.Descent > Metric.Descent then
            Metric.Descent := Temp.Descent;

          if Temp.LineSkip > Metric.LineSkip then
            Metric.LineSkip := Temp.LineSkip;

          if Temp.LineSkip_with_LineSpace > Metric.LineSkip_with_LineSpace then
            Metric.LineSkip_with_LineSpace := Temp.LineSkip_with_LineSpace;

          // font was handled so we can remove the font to skip the following words.
          // because the value only will change if we change the font.
          Font := nil;
        end;
      end;
    end;

    pLine := pLine^.NextItem;
  end;
end;


procedure TtsRenderer.PushTempLines;
begin
  TrimSpaces(fLinesTemp.Lines);

  fLinesTemp.Lines^.LineLength := fLinesTemp.Lines^.LineLength - fLastActiveFont.CharSpacing;

  // add after last item
  if fLinesFirst <> nil then begin
    fLinesLast^.NextLine := fLinesTemp.Lines;
    fLinesLast := fLinesTemp.Lines;
  end;

  // set first item
  if fLinesFirst = nil then begin
    fLinesFirst := fLinesTemp.Lines;
    fLinesLast := fLinesTemp.Lines;
  end;

  // if vertical align is top then draw direktlly
  if tsGetParameteri(TS_VALIGN) = TS_VALIGN_TOP then
    DrawLine(fLinesLast^.LineItemFirst, fLinesLast^.LineLength, fLinesLast^.LineAutoBreak);

  // create new item
  with fLinesTemp do begin
    New(Lines);
    with Lines^ do begin
      NextLine := nil;
      LineItemFirst := nil;
      LineItemLast := nil;
      LineLength := 0;
      LineAutoBreak := False;
    end;

    Empty := True;
  end;
end;


procedure TtsRenderer.PushLineItem(pLine: PtsLineItem);
begin
  with fLinesTemp do begin
    if Lines <> nil then begin
      // add after last item
      if Lines^.LineItemLast <> nil then begin
        pLine^.PrevItem := Lines^.LineItemLast;
        Lines^.LineItemLast^.NextItem := pLine;
        Lines^.LineItemLast := pLine;
      end;

      // set first item
      if Lines^.LineItemFirst = nil then begin
        Lines^.LineItemFirst := pLine;
        Lines^.LineItemLast := pLine;
      end;
    end;
  end;
end;


procedure TtsRenderer.SplitIntoLines(pItemList: PtsLineItem);
var
  pExtractItem: PtsLineItem;


  procedure PushWord(pItem: PtsLineItem);
  begin
    if pItem <> nil then begin
      with fLinesTemp.Lines^ do begin
        // add after last item
        if LineItemLast <> nil then begin
          LineItemLast^.NextItem := pItem;
          pItem^.PrevItem := LineItemLast;
          LineItemLast := pItem;
        end;

        // set first item
        if LineItemFirst = nil then begin
          LineItemFirst := pItem;
          LineItemLast := pItem;
        end;
      end;
    end;
  end;
  

begin
  while pItemList <> nil do begin
    // extract word from list
    pExtractItem := pItemList;
    pItemList := pItemList^.NextItem;
    pExtractItem^.NextItem := nil;
    pExtractItem^.PrevItem := nil;

    case pExtractItem^.ItemType of
      TS_BLOCK_WORD, TS_BLOCK_SPACE: begin
        // calculate size
        CalculateWordLength(fLastActiveFont, pExtractItem);

        if fWordWrap {and not fSingleLine} then begin
          // if line + word is larger than draw width
          if fLinesTemp.Lines^.LineLength + pExtractItem^.WordLength > fBlockWidth then begin
            fLinesTemp.Lines^.LineAutoBreak := True;

            // if line is empty
            if fLinesTemp.Lines^.LineLength = 0 then begin
              // ### Split word into multiple lines
              PushWord(pExtractItem);

              pExtractItem := nil;
            end; // else

            PushTempLines;
          end;
        end;

        // add extracted word to intern small list
        if pExtractItem <> nil then begin
          // add word
          PushWord(pExtractItem);

          // add Length
          fLinesTemp.Lines^.LineLength := fLinesTemp.Lines^.LineLength + pExtractItem^.WordLength;
        end;
      end;

      TS_BLOCK_LINEBREAK: begin
//        if not fSingleLine then begin
          PushWord(pExtractItem);

          PushTempLines;
//        end;
      end;

      TS_BLOCK_TAB: begin
        PushWord(pExtractItem);
      end;
    end;
  end;
end;


function TtsRenderer.SplitText(pText: PWideChar): PtsLineItem;
var
  pLastItem: PtsLineItem;

  State: Integer;

  WordLength: Integer;
  pWordBegin: PWideChar;


  procedure ExtractWord;
  var
    pWord: PWideChar;
    pWordItem: PtsLineItem;


    procedure AddItem;
    begin
      // add item to list
      if Result <> nil then begin
        pLastItem^.NextItem := pWordItem;
        pWordItem^.PrevItem := pLastItem;
        pLastItem := pWordItem;
      end;

      if Result = nil then begin
        Result := pWordItem;
        pLastItem := pWordItem;
      end;
    end;

    
  begin
    if State <> 0 then begin
      // Create listitem
      New(pWordItem);
      pWordItem^.NextItem := nil;
      pWordItem^.PrevItem := nil;
      pWordItem^.ItemType := State;

      // only if space or text
      case State of
        TS_BLOCK_WORD, TS_BLOCK_SPACE: begin
          pWordItem^.Word := tsStrAlloc(WordLength);

          // copy chars
          WordLength := 0;
          pWord := pWordItem^.Word;

          while pWordBegin <> pText do begin
            pWord^ := pWordBegin^;

            Inc(pWord);
            Inc(pWordBegin);
          end;

          AddItem;
        end;

        TS_BLOCK_LINEBREAK: begin
          if pWordBegin <> pText then begin
            // Skip Linebreak
            while pWordBegin <> pText do
              Inc(pWordBegin);

//            if not fSingleLine then begin
              AddItem;
//            end else

//            begin
//              Dispose(pWordItem);
//              pWordItem := nil;
//            end;

          end else

          begin
            Dispose(pWordItem);
            pWordItem := nil;
          end;
        end;

        TS_BLOCK_TAB: begin
          AddItem;
        end;
      end;
    end;
  end;


begin
  Result := nil;
  pLastItem := nil;
  WordLength := 0;
  State := 0;

  pWordBegin := pText;

  // look for word breaks
  while pText^ <> #0 do begin
    case pText^ of
      // Tabulator
      #$0009: begin
        ExtractWord;
        Inc(pWordBegin);
        State := TS_BLOCK_TAB;
      end;

      // line breaks
      #$000D, #$000A: begin
        if State <> TS_BLOCK_LINEBREAK then  
          ExtractWord;

        if pWordBegin <> pText then begin
          ExtractWord;
          Inc(pWordBegin);
        end;

        State := TS_BLOCK_LINEBREAK;
      end;

      // Spaces
      #$0020: begin
        if State <> TS_BLOCK_SPACE then begin
          ExtractWord;

          State := TS_BLOCK_SPACE;
        end;
      end;
    else
      if State <> TS_BLOCK_WORD then begin
        ExtractWord;

        State := TS_BLOCK_WORD;
      end;
    end;

    Inc(pText);
    Inc(WordLength);
  end;

  // copy last word
  if pWordBegin <> pText then
    ExtractWord;  
end;


function TtsRenderer.TextGetDrawHeight: Integer;
var
  pLinesItem: PtsLinesItem;
  Metric: TtsTextMetric;
begin
  Result := 0;

  // all lines
  pLinesItem := fLinesFirst;

  while pLinesItem <> nil do begin
    GetLineMetric(pLinesItem^.LineItemFirst, Metric);

    Result := Result + Metric.LineSkip_with_LineSpace;

    pLinesItem := pLinesItem^.NextLine;
  end;

  // last if we had an templine
  if fLinesTemp.Lines <> nil then begin
    GetLineMetric(fLinesTemp.Lines^.LineItemFirst, Metric);

    Result := Result + Metric.LineSkip_with_LineSpace;
  end;
end;


function TtsRenderer.TextGetDrawWidth: Integer;
var
  pLinesItem: PtsLinesItem;
  Temp: Integer;
  {%H-}Font: TtsFont;


  function IntGetLineWidth(pLine: PtsLineItem): Integer;
  begin
    Result := 0;

    while pLine <> nil do begin
      case pLine^.ItemType of
        TS_BLOCK_FONT: begin
          Font := pLine^.Font;
        end;

        TS_BLOCK_WORD, TS_BLOCK_SPACE: begin
          Result := Result + pLine^.WordLength;
        end;
      end;

      pLine := pLine^.NextItem;
    end;
  end;

begin
  Result := 0;

  // all lines
  Font := fActiveFont;

  pLinesItem := fLinesFirst;
  while pLinesItem <> nil do begin
    Temp := IntGetLineWidth(pLinesItem^.LineItemFirst);
    if Temp > Result then
      Result := Temp;

    pLinesItem := pLinesItem^.NextLine;
  end;

  // last if we had an templine
  if fLinesTemp.Lines <> nil then begin
    Temp := IntGetLineWidth(fLinesTemp.Lines^.LineItemFirst);

    if Temp > Result then
      Result := Temp;
  end;
end;


function TtsRenderer.TextGetWidth(pText: pWideChar): Integer;
var
  pItemList: PtsLineItem;
  pTempItem: PtsLineItem;
begin
  Result := 0;

  pItemList := SplitText(pText);
  pTempItem := pItemList;

  while pTempItem <> nil do begin
    CalculateWordLength(fActiveFont, pTempItem);
    Result := Result + pTempItem^.WordLength;

    pTempItem := pTempItem^.NextItem;
  end;

  // Free Items
  FreeLineItems(pItemList);
end;


procedure TtsRenderer.TextOut(pText: pWideChar);
var
  pItemList: PtsLineItem;

  pTempItem: PtsLineItem;
  TempLength: Integer;
begin
  pItemList := SplitText(pText);

  if isBlock then begin
    SplitIntoLines(pItemList);
  end else

  begin
    DrawSetPosition(0, 0);

    // Calculate Word length
    TempLength := 0;
    pTempItem := pItemList;

    while pTempItem <> nil do begin
      CalculateWordLength(fActiveFont, pTempItem);
      TempLength := TempLength + pTempItem^.WordLength;

      pTempItem := pTempItem^.NextItem;
    end;

    // remove last Char Spacing
    TempLength := TempLength - fActiveFont.CharSpacing;

    // if single line is top then set the Position to the baseline
    if tsGetParameteri(TS_SINGLE_LINE) = TS_SINGLE_LINE_TOP then
      DrawSetPositionRelative(0, fActiveFont.Ascent);

    // draw
    DrawLine(pItemList, TempLength, False);

    // Free Items
    FreeLineItems(pItemList);
  end;
end;



procedure TtsRenderer.TrimSpaces(pLinesItem: PtsLinesItem);
var
  pTempLoopItem, pTempItem: PtsLineItem;
begin
  if pLinesItem <> nil then begin
    // delete all spaces at beginning
    while pLinesItem^.LineItemFirst <> nil do begin
      if pLinesItem^.LineItemFirst^.ItemType <> TS_BLOCK_SPACE then
        Break;

      // save first
      pTempItem := pLinesItem^.LineItemFirst;

      // remove first item fromlist
      pLinesItem^.LineItemFirst := pLinesItem^.LineItemFirst^.NextItem;
      if pLinesItem^.LineItemFirst = nil then
        pLinesItem^.LineItemLast := nil
      else
        pLinesItem^.LineItemFirst^.PrevItem := nil;

      pLinesItem^.LineLength := pLinesItem^.LineLength - pTempItem^.WordLength;

      // dispose item
      pTempItem^.NextItem := nil;
      FreeLineItems(pTempItem);
    end;

    // delete all spaces at the end
    while pLinesItem^.LineItemLast <> nil do begin
      if pLinesItem^.LineItemLast^.ItemType <> TS_BLOCK_SPACE then
        break;

      // save last item
      pTempItem := pLinesItem^.LineItemLast;

      // remove last item from list
      pLinesItem^.LineItemLast := pLinesItem^.LineItemLast^.PrevItem;
      if pLinesItem^.LineItemLast = nil then
        pLinesItem^.LineItemFirst := nil
      else
        pLinesItem^.LineItemLast^.NextItem := nil;

      pLinesItem^.LineLength := pLinesItem^.LineLength - pTempItem^.WordLength;

      // dispose item
      FreeLineItems(pTempItem);
    end;


    // delete all spaces until some text comes
    pTempLoopItem := pLinesItem^.LineItemFirst;
    while pTempLoopItem <> nil do begin
      // exit if we have an word
      if pTempLoopItem^.ItemType = TS_BLOCK_WORD then
        Break;

      pTempItem := pTempLoopItem;

      pTempLoopItem := pTempLoopItem^.NextItem;

      if pTempItem^.ItemType = TS_BLOCK_SPACE then begin
        pLinesItem^.LineLength := pLinesItem^.LineLength - pTempItem^.WordLength;

        // set new next/prev
        if pTempItem^.NextItem <> nil then
          pTempItem^.NextItem^.PrevItem := pTempItem^.PrevItem;

        if pTempItem^.PrevItem <> nil then
          pTempItem^.PrevItem^.NextItem := pTempItem^.NextItem;

        // remove item
        pTempItem^.PrevItem := nil;
        pTempItem^.NextItem := nil;

        FreeLineItems(pTempItem);
      end;
    end;

    // delete all spaces until some text comes
    pTempLoopItem := pLinesItem^.LineItemLast;
    while pTempLoopItem <> nil do begin
      // exit if we have an word
      if pTempLoopItem^.ItemType = TS_BLOCK_WORD then
        Break;

      pTempItem := pTempLoopItem;

      pTempLoopItem := pTempLoopItem^.PrevItem;

      if pTempItem^.ItemType = TS_BLOCK_SPACE then begin
        pLinesItem^.LineLength := pLinesItem^.LineLength - pTempItem^.WordLength;

        // set new next/prev
        if pTempItem^.PrevItem <> nil then
          pTempItem^.PrevItem^.NextItem := pTempItem^.NextItem;

        if pTempItem^.NextItem <> nil then
          pTempItem^.NextItem^.PrevItem := pTempItem^.PrevItem;

        // remove item
        pTempItem^.PrevItem := nil;
        pTempItem^.NextItem := nil;

        FreeLineItems(pTempItem);
      end;
    end;
  end;
end;


procedure TtsRenderer.CharOut(CharCode: WideChar);
var
  tsChar: TtsChar;
begin
  tsChar := fActiveFont.GetChar(CharCode);

  if tsChar <> nil then
    DrawChar(fActiveFont, tsChar);
end;


{ TtsRendererNULL }

function TtsRendererNULL.AddImage(Char: TtsChar; CharImage: TtsImage): TtsRendererImageReference;
begin
  Result := TtsRendererNULLImageReference.Create;

  if fSaveImages then
    with TtsRendererNULLImageReference(Result) do begin
      Image := TtsImage.Create;
      Image.AssignFrom(CharImage);
    end;
end;


procedure TtsRendererNULL.DrawChar(Font: TtsFont; Char: TtsChar);
begin
  // nothing
end;


procedure TtsRendererNULL.DrawSetColor(Red, Green, Blue, Alpha: Single);
begin
  // nothing
end;


procedure TtsRendererNULL.DrawSetPosition(X, Y: Integer);
begin
  // nothing
end;


procedure TtsRendererNULL.DrawSetPositionRelative(X, Y: Integer);
begin
  // nothing
end;


procedure TtsRendererNULL.RemoveImageReference(ImageReference: TtsRendererImageReference);
begin
  if (ImageReference is TtsRendererNULLImageReference) then
    with TtsRendererNULLImageReference(ImageReference) do
      if Image <> nil then
        Image.Free;
end;



{ TtsRendererOpenGL }

function TtsRendererOpenGL.AddImage(Char: TtsChar; CharImage: TtsImage): TtsRendererImageReference;
var
  Idx: Integer;
  TextureEntry: PtsRendererOpenGLTextureEntry;
  TextureAdded: Boolean;

  Texture: PtsRendererOpenGLTexture;
  CharHeight, CharWidth: Integer;

  W1, H1, TempBorder: Single;
begin
  Result := nil;

  if not CharImage.Empty then begin
    Result := TtsRendererOpenGLImageReference.Create;

    with TtsRendererOpenGLImageReference(Result) do begin
      Coordinates.Top := 0;
      Coordinates.Left := 0;
      Coordinates.Right := 0;
      Coordinates.Bottom := 0;

      TextureAdded := False;
      TextureEntry := nil;

      // look if we can add the image to an texture
      for Idx := 0 to fTextures.Count - 1 do begin
        if AddImageToTexture(fTextures[Idx], CharImage, TexID, Coordinates) then begin
          TextureEntry := fTextures[Idx];
          TextureAdded := True;

          Break;
        end;
      end;

      // could not added so create new texture
      if not TextureAdded then begin
        TextureEntry := CreateNewTexture;

        AddImageToTexture(TextureEntry, CharImage, TexID, Coordinates);
      end;

      // generating coords
      if TextureEntry <> nil then begin
        Texture := TextureEntry^.Texture;

        if Texture <> nil then begin
          with Char do begin
            CharHeight := Coordinates.Bottom - Coordinates.Top;
            CharWidth  := Coordinates.Right - Coordinates.Left;

            // Set Variables for resizing border 
            if HasResizingBorder then begin
              W1 := 1 / Texture^.Width;
              H1 := 1 / Texture^.Height;
              TempBorder := 2;
            end else begin
              W1 := 0;
              H1 := 0;
              TempBorder := 0;
            end;

            // Top Left
            TexCoords[0].X := Coordinates.Left / Texture^.Width  + W1;
            TexCoords[0].Y := Coordinates.Top  / Texture^.Height + H1;
//            Vertex[0].X := - GlyphRect.Left + Size1;
//            Vertex[0].Y := - GlyphRect.Top - GlyphOriginY + Size1;
            Vertex[0].X := - GlyphRect.Left;
            Vertex[0].Y := - GlyphRect.Top - GlyphOriginY;

            // Bottom Left
            TexCoords[1].X := Coordinates.Left   / Texture^.Width  + W1;
            TexCoords[1].Y := Coordinates.Bottom / Texture^.Height - H1;
//            Vertex[1].X := - GlyphRect.Left + Size1;
//            Vertex[1].Y := CharHeight - GlyphRect.Top - GlyphOriginY - Size1;
            Vertex[1].X := - GlyphRect.Left;
            Vertex[1].Y := CharHeight - GlyphRect.Top - GlyphOriginY - TempBorder;

            // Bottom Right
            TexCoords[2].X := Coordinates.Right  / Texture^.Width  - W1;
            TexCoords[2].Y := Coordinates.Bottom / Texture^.Height - H1;
//            Vertex[2].X := CharWidth - GlyphRect.Left - Size1;
//            Vertex[2].Y := CharHeight - GlyphRect.Top - GlyphOriginY - Size1;
            Vertex[2].X := CharWidth - GlyphRect.Left  - TempBorder;
            Vertex[2].Y := CharHeight - GlyphRect.Top - GlyphOriginY  - TempBorder;

            // Top Right
            TexCoords[3].X := Coordinates.Right / Texture^.Width  - W1;
            TexCoords[3].Y := Coordinates.Top   / Texture^.Height + H1;
//            Vertex[3].X := CharWidth - GlyphRect.Left - Size1;
//            Vertex[3].Y := - GlyphRect.Top - GlyphOriginY + Size1;
            Vertex[3].X := CharWidth - GlyphRect.Left  - TempBorder;
            Vertex[3].Y := - GlyphRect.Top - GlyphOriginY;
          end;
        end;
      end;
    end;
  end;
end;


function TtsRendererOpenGL.AddImageToTexture(Texture: PtsRendererOpenGLTextureEntry; Image: TtsImage; var TextureID: Integer; var Coordinates: tsRect): boolean;
var
  NeedX, NeedY: Word;
  Start: Word;
  Y, Y2: Integer;
  Managed: PtsRendererOpenGLManagedEntry;


  function CheckVertical(StartPos, EndPos: Integer): Boolean;
  var
    TempY: Integer;
    TempManaged: PtsRendererOpenGLManagedEntry;
    Found: Boolean;
  begin
    Result := False;

    for TempY := Y +1 to Y + NeedY -1 do begin
      TempManaged := Texture^.Lines[TempY];

      // Überprüfen ob der entsprechende Bereich noch frei ist.
      Found := False;

      while TempManaged <> nil do begin
        if (TempManaged^.Start <= StartPos) and (TempManaged^.Start + TempManaged^.Count >= EndPos) then
          Found := True;

        TempManaged := TempManaged^.NextEntry;
      end;

      if not Found then
        Exit;
    end;

    Result := True;
  end;


begin
  Result := False;

  NeedX := Image.Width shr 1;
  if (Image.Width and 1) > 0 then
    Inc(NeedX);

  NeedY := Image.Height shr 1;
  if (Image.Height and 1) > 0 then
    Inc(NeedY);

  // scan for free space
  for Y := Low(Texture^.Lines) to High(Texture^.Lines) - NeedY do begin
    Managed := Texture^.Lines[Y];

    while Managed <> nil do begin
      if Managed^.Count >= NeedX then begin
        if CheckVertical(Managed^.Start, Managed^.Start + NeedX) then begin
          Start := Managed^.Start;

          // allocating space
          for Y2 := Y to Y + NeedY -1 do
            AllocSpace(Texture^.Lines[Y2], Start, NeedX);

          // setting texturecoordinates values
          TextureID := Texture^.ID;

          Coordinates.Left   := Start shl 1;
          Coordinates.Top    := Y shl 1;
          Coordinates.Right  := Coordinates.Left + Image.Width;
          Coordinates.Bottom := Coordinates.Top  + Image.Height;

          Texture^.Usage := Texture^.Usage + NeedX * NeedY;

          // copy charimage
          with Texture^.Texture^ do begin
            glBindTexture(GL_TEXTURE_2D, Texture^.Texture^.glTextureID);
            glTexSubImage2D(GL_TEXTURE_2D, 0, Coordinates.Left, Coordinates.Top, Image.Width, Image.Height, GL_RGBA, GL_UNSIGNED_BYTE, Image.Data);
          end;

          Result := True;
          Exit;
        end;
      end;

      Managed := Managed^.NextEntry;
    end;
  end;
end;


procedure TtsRendererOpenGL.AfterConstruction;
begin
  inherited;

  fTextures := TList.Create;
  fTextureSize := 256;
end;


procedure TtsRendererOpenGL.AllocSpace(var FirstManaged: PtsRendererOpenGLManagedEntry; Start, Count: Word);
var
  Managed, TempManaged: PtsRendererOpenGLManagedEntry;


  procedure RemoveManagedItem(pItem: PtsRendererOpenGLManagedEntry);
  var
    pTemp, pTemp2: PtsRendererOpenGLManagedEntry;
  begin
    pTemp := FirstManaged;

    while pTemp <> nil do begin
      pTemp2 := pTemp^.NextEntry;

      if pTemp2 = pItem then begin
        pTemp^.NextEntry := pItem^.NextEntry;

        Break;
      end;

      pTemp := pTemp2;
    end;
  end;


begin
  // complete remove of the FIRST item (spezial handling for first item removal.)
  if (Start = FirstManaged^.Start) and (Count = FirstManaged^.Count) then begin
    TempManaged := FirstManaged;

    FirstManaged := FirstManaged^.NextEntry;

    Dispose(TempManaged);
  end else

  // look for matching item
  begin
    Managed := FirstManaged;

    while Managed <> nil do begin
      // matched item?
      if (Start >= Managed^.Start) and ((Start + Count) <= (Managed^.Start + Managed^.Count)) then begin

        // cut at start 
        if (Start = Managed^.Start) then begin

          // remove the whole item
          if (Count = Managed^.Count) then begin
            RemoveManagedItem(Managed);

            // no need to preserve Managed because we leaving the loop
            Dispose(Managed);
          end else

          // cut at start
          begin
            Managed^.Start := Managed^.Start + Count;
            Managed^.Count := Managed^.Count - Count;
          end;
        end else

        // cut at end
        if (Start + Count) = (Managed^.Start + Managed^.Count) then begin
          Managed^.Count := Managed^.Count - Count;
        end else

        // cut in the middle
        begin
          New(TempManaged);
          TempManaged^.NextEntry := Managed^.NextEntry;
          Managed^.NextEntry := TempManaged;

          TempManaged^.Start := Start + Count;
          TempManaged^.Count := (Managed^.Start + Managed^.Count) - TempManaged^.Start;

          Managed^.Count := Start - Managed^.Start;
        end;

        // we found an item so leave the loop
        Break;
      end;

      Managed := Managed^.NextEntry;
    end;
  end;
end;


procedure TtsRendererOpenGL.BeforeDestruction;
begin
  ClearTextures;
  fTextures.Free;

  inherited;
end;


procedure TtsRendererOpenGL.BeginBlock(Left, Top, Width, Height: Integer; Flags: tsBitmask);
begin
  fPos.X := 0;
  fPos.Y := 0;

  inherited;
end;


procedure TtsRendererOpenGL.ClearTextures;
var
  Idx: Integer;
begin
  // Disposing items
  for Idx := fTextures.Count - 1 downto 0 do
    DeleteTexture(Idx);

  // Clear list
  fTextures.Clear;
end;


function TtsRendererOpenGL.CreateNewTexture: PtsRendererOpenGLTextureEntry;
var
  Idx: Integer;
begin
  New (Result);
  with Result^ do begin
    ID := fTextures.Add(Result);
    Usage := 0;

    // create opengl texture
    New(Texture);
    with Texture^ do begin
      Width := TextureSize;
      Height := TextureSize;

      glGenTextures(1, @glTextureID);
      glBindTexture(GL_TEXTURE_2D, glTextureID);

      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, TextureSize, TextureSize, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
    end;

    // initiale memory manager value
    SetLength(Lines, Texture^.Height shr 1);

    for Idx := Low(Lines) to High(Lines) do begin
      New(Lines[Idx]);
      Lines[Idx]^.NextEntry := nil;
      Lines[Idx]^.Start := 0;
      Lines[Idx]^.Count := Texture^.Width shr 1;
    end;
  end;
end;


procedure TtsRendererOpenGL.DeleteTexture(Idx: Integer);
var
  pItem: PtsRendererOpenGLTextureEntry;

  LineIdx: Integer;
  pManaged, pTempManaged: PtsRendererOpenGLManagedEntry;
begin
  pItem := fTextures[Idx];

  fTextures.Delete(Idx);

  if pItem <> nil then begin
    with pItem^ do begin
      // Free opengl texture
      if Texture <> nil then begin
        glDeleteTextures(1, @(Texture^.glTextureID));

        Dispose(Texture);
      end;

      // free lines
      for LineIdx := Low(Lines) to High(Lines) do begin
        pManaged := Lines[LineIdx];
        Lines[LineIdx] := nil;

        while pManaged <> nil do begin
          pTempManaged := pManaged;
          pManaged := pManaged^.NextEntry;

          Dispose(pTempManaged);
        end;
      end;

      SetLength(Lines, 0);
    end;

    Dispose(pItem);
  end;
end;


procedure TtsRendererOpenGL.DrawChar(Font: TtsFont; Char: TtsChar);
var
  Texture: PtsRendererOpenGLTexture;
  TempVertex: tsQuadFloat;
begin
  if Char.RendererImageReference <> nil then begin
    with Char.RendererImageReference as TtsRendererOpenGLImageReference do begin
      Texture := GetTextureByID(TexID);

      if Texture <> nil then begin
        glBindTexture(GL_TEXTURE_2D, Texture^.glTextureID);
        glEnable(GL_TEXTURE_2D);

        // calculate new quad
        TranslateQuad(TempVertex, Vertex, fPos);

        glBegin(GL_QUADS);
          glTexCoord2fv(@TexCoords[0]);
          glVertex2fv(@TempVertex[0]);

          glTexCoord2fv(@TexCoords[1]);
          glVertex2fv(@TempVertex[1]);

          glTexCoord2fv(@TexCoords[2]);
          glVertex2fv(@TempVertex[2]);

          glTexCoord2fv(@TexCoords[3]);
          glVertex2fv(@TempVertex[3]);
        glEnd;

        // if debug is enabled
        if fContext.gDebugDrawCharRects then begin
          glDisable(GL_TEXTURE_2D);

          // image Rect
          glColor4f(0, 1, 0, 0.1);
          glBegin(GL_QUADS);
            glVertex2fv(@TempVertex[0]);
            glVertex2fv(@TempVertex[1]);
            glVertex2fv(@TempVertex[2]);
            glVertex2fv(@TempVertex[3]);
          glEnd;

          // glyph rect
          glColor4f(1, 0, 0, 0.1);
          glBegin(GL_QUADS);
            glVertex2f(TempVertex[0].X + Char.GlyphRect.Left,  TempVertex[0].Y + Char.GlyphRect.Top);
            glVertex2f(TempVertex[0].X + Char.GlyphRect.Left,  TempVertex[0].Y + Char.GlyphRect.Bottom);
            glVertex2f(TempVertex[0].X + Char.GlyphRect.Right, TempVertex[0].Y + Char.GlyphRect.Bottom);
            glVertex2f(TempVertex[0].X + Char.GlyphRect.Right, TempVertex[0].Y + Char.GlyphRect.Top);
          glEnd;

          // baseline
          glColor4f(0, 0, 1, 0.25);
          glBegin(GL_LINES);
            glVertex2f(TempVertex[0].X, 0);
            glVertex2f(TempVertex[2].X, 0);
          glEnd;

          glColor4f(1, 1, 1, 1);
        end;
      end;
    end;
  end;
end;


procedure TtsRendererOpenGL.DrawSetColor(Red, Green, Blue, Alpha: Single);
begin
  glColor4f(Red, Green, Blue, Alpha);
end;


procedure TtsRendererOpenGL.DrawSetPosition(X, Y: Integer);
begin
  fPos.X := X;
  fPos.Y := Y;
end;


procedure TtsRendererOpenGL.DrawSetPositionRelative(X, Y: Integer);
begin
  DrawSetPosition(fPos.X + X, fPos.Y + Y);
end;


procedure TtsRendererOpenGL.FreeSpace(var FirstManaged: PtsRendererOpenGLManagedEntry; Start, Count: Word);
var
  Last, Managed, Temp: PtsRendererOpenGLManagedEntry;
  AddItem: Boolean;
begin
  // if we have no space we can add item directly
  if FirstManaged = nil then begin
    New(Temp);
    Temp^.Start := Start;
    Temp^.Count := Count;
    Temp^.NextEntry := nil;

    FirstManaged := Temp;
  end else

  // Special handling for first Item
  if Start + Count < FirstManaged^.Start then begin
    New(Temp);
    Temp^.Start := Start;
    Temp^.Count := Count;
    Temp^.NextEntry := FirstManaged;

    FirstManaged := Temp;
  end else

  begin
    Managed := FirstManaged;
    Last := nil;

    while Managed <> nil do begin
      // block is in front of another
      if Start + Count = Managed^.Start then begin
        Managed^.Start := Managed^.Start - Count;
        Managed^.Count := Managed^.Count + Count;

        if Last <> nil then begin
          if Last^.Start + Last^.Count = Managed^.Start then begin
            // Remove Item
            Last^.Count := Last^.Count + Managed^.Count;
            Last^.NextEntry := Managed^.NextEntry;

            Dispose(Managed);
          end;
        end;

        Break;
      end else

      // block is behind another
      if Start = Managed^.Start + Managed^.Count then begin
        Managed^.Count := Managed^.Count + Count;

        Temp := Managed^.NextEntry;
        if Temp <> nil then begin
          if Managed^.Start + Managed^.Count = Temp^.Start then begin
            // Remove Item
            Managed^.Count := Managed^.Count + Temp^.Count;
            Managed^.NextEntry := Temp^.NextEntry;

            Dispose(Temp);
          end;
        end;

        Break;
      end else

      // the block dosn't border an other so we must create some other
      begin
        AddItem := False;

        if not (Managed^.NextEntry <> nil) then
          AddItem := True
        else

        if (Managed^.Start + Managed^.Count < Start) and (Managed^.NextEntry^.Start > Start + Count) then
          AddItem := True;

        if AddItem then begin
          New(Temp);
          Temp^.Start := Start;
          Temp^.Count := Count;
          Temp^.NextEntry := Managed^.NextEntry;
          Managed^.NextEntry := Temp;

          Break;
        end;
      end;

      Last := Managed;
      Managed := Managed^.NextEntry;
    end;
  end;
end;


function TtsRendererOpenGL.GetTextureByID(ID: Integer): PtsRendererOpenGLTexture;
var
  Idx: Integer;
  pTexture: PtsRendererOpenGLTextureEntry;
begin
  Result := nil;

  for Idx := 0 to fTextures.Count - 1 do begin
    pTexture := fTextures[Idx];

    if pTexture <> nil then
      if pTexture^.ID = ID then begin
        Result := pTexture^.Texture;
        Break;
      end;
  end;
end;



procedure TtsRendererOpenGL.RemoveImageReference(ImageReference: TtsRendererImageReference);
var
  OpenGLRef: TtsRendererOpenGLImageReference;
  pItem: PtsRendererOpenGLTextureEntry;

  Idx, TempIdx: Integer;
  TempWidth, TempHeight: Integer;
  NeedX, NeedY: Integer;
  LinesY, TempX, TempY: Integer;
begin
  OpenGLRef := TtsRendererOpenGLImageReference(ImageReference);

  // freeing texture
  for Idx := 0 to fTextures.Count - 1 do begin
    pItem := fTextures[Idx];

    if pItem <> nil then begin
      if pItem^.ID = OpenGLRef.TexID then begin
        TempWidth  := OpenGLRef.Coordinates.Right - OpenGLRef.Coordinates.Left;
        TempHeight := OpenGLRef.Coordinates.Bottom - OpenGLRef.Coordinates.Top;

        with pItem^ do begin
          // calc size
          NeedX := TempWidth shr 1;
          if (TempWidth and 1) > 0 then
            Inc(NeedX);

          NeedY := TempHeight shr 1;
          if (TempHeight and 1) > 0 then
            Inc(NeedY);

          TempY := OpenGLRef.Coordinates.Top shr 1;
          TempX := OpenGLRef.Coordinates.Left shr 1;

          Usage := Usage - NeedX * NeedY; 

          Assert(Usage >= 0);

          // Points
          for LinesY := 0 to NeedY - 1 do
            FreeSpace(Lines[TempY + LinesY], TempX, NeedX);

          // freeing opengltexture
          if Usage = 0 then begin
            for TempIdx := 0 to fTextures.Count - 1 do begin
              if PtsRendererOpenGLTextureEntry(fTextures[TempIdx])^.ID = pItem^.ID then begin
                DeleteTexture(TempIdx);

                Break;
              end;
            end;
          end;
        end;

        Break;
      end;
    end;
  end;
end;


{ TtsContext }

function TtsContext.AnsiToWide(pText: pAnsiChar): pWideChar;

  function GetDefaultChar: WideChar;
  begin
    Result := #0;

    if tsGetParameteri(TS_EMPTY_CP_ENTRY) = TS_EMPTY_CP_ENTRY_USE_DEFAULT then
      if ActiveFont <> nil then
        Result := ActiveFont.DefaultChar;
  end;

begin
  Result := nil;

  // UTF-8
  if gCodePage = TS_CODEPAGE_UTF8 then begin
    Result := tsStrAlloc(Length(pText));
    tsAnsiUTF8ToWide(Result, pText, GetDefaultChar);
  end else

  // ISO 8859-1
  if gCodePage = TS_CODEPAGE_8859_1 then begin
    Result := tsStrAlloc(Length(pText));
    tsAnsiISO_8859_1_ToWide(Result, pText);
  end else

  // single or double byte CodePage
  begin
    if (Addr(gCodePageFunc) <> nil) and (gCodePagePtr <> nil) then begin
      Result := tsStrAlloc(Length(pText));
      gCodePageFunc(Result, pText, gCodePagePtr, GetDefaultChar);
    end;
  end;
end;


procedure TtsContext.ClearFonts;
var
  List: TList;
  Idx: Integer;
  pItem: PtsContextFontEntry;
begin
  List := TList.Create;
  try
    fFonts.GetValues(List);
    fFonts.Clear;

    for Idx := 0 to List.Count - 1 do begin
      pItem := List[Idx];

      pItem^.Font.Free;
      Dispose(pItem);
    end;
  finally
    List.Free;
  end;
end;


procedure TtsContext.ClearImages;
var
  List: TList;
  Idx: Integer;
  pItem: PtsContextImageEntry;
begin
  List := TList.Create;
  try
    fImages.GetValues(List);
    fImages.Clear;

    for Idx := 0 to List.Count - 1 do begin
      pItem := List[Idx];

      pItem^.Image.Free;
      Dispose(pItem);
    end;
  finally
    List.Free;
  end;
end;


constructor TtsContext.Create;
begin
  inherited;

  Inc(gLastContextID);
  fContextID := gLastContextID;

  // hashes
  fFonts := TtsHash.Create(127);
  fImages := TtsHash.Create(127);

  // defaults
  gEmptyCodePageEntry  := TS_EMPTY_CP_ENTRY_USE_DEFAULT;
  gCodePage            := TS_CODEPAGE_8859_1;
  gCodePagePtr         := nil; //@CP_8859_1;
  gCodePageFunc        := nil; //tsAnsiSBCDToWide;

  gGlobalFormat        := TS_FORMAT_RGBA8;
  gGlobalAntiAliasing  := TS_ANTIALIASING_NORMAL;

  gSingleLine          := TS_SINGLE_LINE_BASELINE;
  gAlign               := TS_ALIGN_LEFT;
  gVAlign              := TS_VALIGN_TOP;
  gClip                := TS_CLIP_COMPLETE;

  gImageMode[tsModeRed]       := TS_MODE_REPLACE;
  gImageMode[tsModeGreen]     := TS_MODE_REPLACE;
  gImageMode[tsModeBlue]      := TS_MODE_REPLACE;
  gImageMode[tsModeAlpha]     := TS_MODE_MODULATE;
  gImageMode[tsModeLuminance] := TS_MODE_REPLACE;

  gImageLibrary := 0;
end;


destructor TtsContext.Destroy;
begin
  ClearFonts;
  fFonts.Free;

  ClearImages;
  fImages.Free;

  if Renderer <> nil then
    Renderer.Free;

  inherited;
end;


function TtsContext.FontAdd(Font: TtsFont): Cardinal;
var
  Entry: PtsContextFontEntry;
begin
  New(Entry);

  Inc(fLastFontID);

  Entry^.FontID := fLastFontID;
  Entry^.Font := Font;

  fFonts.Add(fLastFontID, Entry);

  Result := fLastFontID;
end;


function TtsContext.FontCount: Cardinal;
begin
  Result := fFonts.Count;
end;


procedure TtsContext.FontDelete(Font: Cardinal);
var
  Entry: PtsContextFontEntry;
begin
  if fLastFontID = Font then
    Renderer.FontActivate(0);

  Entry := fFonts.Get(Font);

  if Entry <> nil then begin
    fFonts.Delete(Entry^.FontID);

    Dispose(Entry);
  end;
end;


function TtsContext.FontGet(Font: Cardinal): TtsFont;
var
  Entry: PtsContextFontEntry;
begin
  Entry := fFonts.Get(Font);

  if Entry <> nil then
    Result := Entry^.Font
  else
    Result := nil;
end;


function TtsContext.GetActiveFont: TtsFont;
begin
  Result := nil;

  if Renderer <> nil then
    Result := Renderer.ActiveFont;
end;


function TtsContext.GetIsLocked: boolean;
begin
  if Renderer <> nil then
    Result := Renderer.isBlock
  else
    Result := False;
end;


function TtsContext.ImageAdd(Image: TtsImage): Cardinal;
var
  Entry: PtsContextImageEntry;
begin
  New(Entry);

  Inc(fLastImageID);

  Entry^.ImageID := fLastImageID;
  Entry^.Image := Image;

  fImages.Add(fLastImageID, Entry);

  Result := fLastImageID;
end;


function TtsContext.ImageCount: Cardinal;
begin
  Result := fImages.Count;
end;


procedure TtsContext.ImageDelete(Image: Cardinal);
var
  Entry: PtsContextImageEntry;
begin
  Entry := fImages.Get(Image);

  if Entry <> nil then begin
    fImages.Delete(Entry^.ImageID);

    Dispose(Entry);
  end;
end;


function TtsContext.ImageGet(Image: Cardinal): TtsImage;
var
  Entry: PtsContextImageEntry;
begin
  Entry := fImages.Get(Image);

  if Entry <> nil then
    Result := Entry^.Image
  else
    Result := nil;
end;

end.
