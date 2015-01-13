unit utsTextSuite;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, math, syncobjs;

type
  TtsRendererType = (
    rtNull,
    rtOpenGL);

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
    tsFormatLumAlpha8);

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

  TtsPosition = packed record
    x, y: Integer;
  end;
  PtsPosition = ^TtsPosition;

  TtsRect = packed record
    case Byte of
      0: (TopLeft: TtsPosition; BottomRight: TtsPosition);
      1: (Left, Top, Right, Bottom: Integer);
  end;
  PtsRect = ^TtsRect;

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

  TtsTextMetric = packed record
    Ascent: Integer;
    Descent: Integer;
    ExternalLeading: Integer;
    BaseLineOffset: Integer;
    CharSpacing: Integer;
    LineHeight: Integer;
    LineSpacing: Integer;
  end;

  TtsAnsiToWideCharFunc = procedure(aDst: PWideChar; const aSize: Integer; aSource: PAnsiChar; const aCodePage: TtsCodePage; const aDefaultChar: WideChar);

  TtsImage = class;
  TtsFont = class;
  TtsFontCreator = class;
  TtsRenderer = class;
  TtsContext = class;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsKernel1DItem = packed record
    Offset: Integer;
    Value: Single;
    DataOffset: Integer;
  end;

  TtsKernel1D = class
  public
    Size: Integer;
    Items: array of TtsKernel1DItem;
    ItemCount: Integer;
    constructor Create(const aRadius, aStrength: Single);
  end;

  TtsImageFunc = procedure(const aImage: TtsImage; X, Y: Integer; var aPixel: TtsColor4f; aArgs: Pointer);
  TtsImage = class(TObject)
  private
    fWidth: Integer;
    fHeight: Integer;
    fFormat: TtsFormat;

    fData: Pointer;
    fHasScanlines: Boolean;
    fScanlines: array of Pointer;

    function GetScanline(const aIndex: Integer): Pointer;
    function GetIsEmpty: Boolean;
    procedure SetData(const aData: Pointer; const aFormat: TtsFormat = tsFormatEmpty; const aWidth: Integer = 0; const aHeight: Integer = 0);
    procedure UpdateScanlines;
  public
    property IsEmpty: Boolean   read GetIsEmpty;
    property Width:   Integer   read fWidth;
    property Height:  Integer   read fHeight;
    property Format:  TtsFormat read fFormat;
    property Data:    Pointer   read fData;
    property Scanline[const aIndex: Integer]: Pointer    read GetScanline;

    function GetPixelAt(const x, y: Integer; out aColor: TtsColor4f): Boolean;

    procedure Assign(const aImage: TtsImage);
    procedure CreateEmpty(const aFormat: TtsFormat; const aWidth, aHeight: Integer);
    procedure LoadFromFunc(const aFunc: TtsImageFunc; const aArgs: Pointer);

    procedure Resize(const aNewWidth, aNewHeight, X, Y: Integer);
    procedure FindMinMax(out aRect: TtsRect);

    procedure FillColor(const aColor: TtsColor4f; const aChannelMask: TtsColorChannels; const aModes: TtsImageModes);
    procedure FillPattern(const aPattern: TtsImage; X, Y: Integer; const aChannelMask: TtsColorChannels; const aModes: TtsImageModes);
    procedure BlendImage(const aImage: TtsImage; const X, Y: Integer);
    procedure Blur(const aHorzKernel, aVertKernel: TtsKernel1D; const aChannelMask: TtsColorChannels);

    procedure AddResizingBorder;

    constructor Create;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsCharRenderRef = class(TObject);
  TtsChar = class(TObject)
  private
    fCharCode: WideChar;
    fGlyphOrigin: TtsPosition;
    fGlyphRect: TtsRect;
    fAdvance: Integer;
    fHasResisingBorder: Boolean;
    fRenderRef: TtsCharRenderRef;
  public
    property CharCode:          WideChar         read fCharCode;
    property GlyphOrigin:       TtsPosition      read fGlyphOrigin       write fGlyphOrigin;
    property GlyphRect:         TtsRect          read fGlyphRect         write fGlyphRect;
    property Advance:           Integer          read fAdvance           write fAdvance;
    property HasResisingBorder: Boolean          read fHasResisingBorder write fHasResisingBorder;
    property RenderRef:         TtsCharRenderRef read fRenderRef         write fRenderRef;

    constructor Create(const aCharCode: WideChar);
  end;

  TtsFontCharArray = packed record
    Chars: array [Byte] of TtsChar;
    CharCount: Byte;
  end;
  PtsFontCharArray = ^TtsFontCharArray;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsFont = class(TObject)
  private
    fCreateChars: Boolean;
    fDefaultChar: WideChar;

    fCopyright: String;
    fFaceName: String;
    fStyleName: String;
    fFullName: String;

    fSize: Integer;
    fStyle: TtsFontStyles;
    fAntiAliasing: TtsAntiAliasing;

    fAscent: Integer;
    fDescent: Integer;
    fExternalLeading: Integer;
    fBaseLineOffset: Integer;
    fCharSpacing: Integer;
    fLineSpacing: Integer;

    fUnderlinePos: Integer;
    fUnderlineSize: Integer;
    fStrikeoutPos: Integer;
    fStrikeoutSize: Integer;

    fChars: array[Byte] of PtsFontCharArray;

    fRenderer: TtsRenderer;
    fCreator: TtsFontCreator;

    function HasChar(const aCharCode: WideChar): Boolean;
    function GetChar(const aCharCode: WideChar): TtsChar;
    procedure AddChar(const aCharCode: WideChar; const aChar: TtsChar);
  public
    property CreateChars: Boolean read fCreateChars write fCreateChars;
    property Char[const aCharCode: WideChar]: TtsChar read GetChar;

    property Copyright: String read fCopyright;
    property FaceName:  String read fFaceName;
    property StyleName: String read fStyleName;
    property FullName:  String read fFullName;

    property Size:         Integer         read fSize;
    property Style:        TtsFontStyles   read fStyle;
    property AntiAliasing: TtsAntiAliasing read fAntiAliasing;
    property Renderer:     TtsRenderer     read fRenderer;

    property Ascent:          Integer read fAscent;
    property Descent:         Integer read fDescent;
    property ExternalLeading: Integer read fExternalLeading;
    property BaseLineOffset:  Integer read fBaseLineOffset;
    property CharSpacing:     Integer read fCharSpacing;
    property LineSpacing:     Integer read fLineSpacing;

    property DefaultChar:   WideChar read fDefaultChar   write fDefaultChar;
    property UnderlinePos:  Integer  read fUnderlinePos  write fUnderlinePos;
    property UnderlineSize: Integer  read fUnderlineSize write fUnderlineSize;
    property StrikeoutPos:  Integer  read fStrikeoutPos  write fStrikeoutPos;
    property StrikeoutSize: Integer  read fStrikeoutSize write fStrikeoutSize;

    procedure AddChar(const aCharCode: WideChar);
    procedure AddCharRange(const aCharCodeBeg, aCharCodeEnd: WideChar);
    procedure RemoveChar(const aCharCode: WideChar);
    procedure ClearChars;

    function GetTextWidthW(aText: PWideChar): Integer;
    procedure GetTextMetric(out aMetric: TtsTextMetric);

    constructor Create(const aRenderer: TtsRenderer; const aCreator: TtsFontCreator;
      const aCopyright, aFaceName, aStyleName, aFullName: String; const aSize, aCharSpacing, aLineSpacing: Integer;
      const aStyle: TtsFontStyles; const aAntiAliasing: TtsAntiAliasing);
    destructor Destroy; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsPostProcessStepRange = record
    StartChar: WideChar;
    EndChar: WideChar;
  end;
  PtsPostProcessStepRange = ^TtsPostProcessStepRange;

  TtsFontProcessStepUsage = (
    tsUsageInclude,
    tsUsageExclude);

  TtsPostProcessStep = class(TObject)
  private
    fIncludeCharRange: TList;
    fExcludeCharRange: TList;

    procedure ClearList(const aList: TList);
  public
    function IsInRange(const aCharCode: WideChar): Boolean;
    procedure Execute(const aChar: TtsChar; const aCharImage: TtsImage); virtual; abstract;

    procedure AddUsageRange(const aUsage: TtsFontProcessStepUsage; const aStartChar, aEndChar: WideChar);
    procedure AddUsageChars(const aUsage: TtsFontProcessStepUsage; aChars: PWideChar);

    procedure ClearIncludeRange;
    procedure ClearExcludeRange;

    constructor Create;
    destructor Destroy; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsFontCreator = class(TObject)
  private
    fPostProcessSteps: TObjectList;
    fAddResizingBorder: Boolean;

    function GetPostProcessStepCount: Integer;
    function GetPostProcessStep(const aIndex: Integer): TtsPostProcessStep;

    procedure DrawLine(const aChar: TtsChar; const aCharImage: TtsImage; aLinePosition, aLineSize: Integer);
    procedure DoPostProcess(const aChar: TtsChar; const aCharImage: TtsImage);

    function GetGlyphMetrics(const aCharCode: WideChar; out aGlyphOriginX, aGlyphOriginY, aGlyphWidth, aGlyphHeight, aAdvance: Integer): Boolean; virtual; abstract;
    procedure GetCharImage(const aCharCode: WideChar; const CharImage: TtsImage); virtual; abstract;
  public
    property PostProcessStepCount: Integer read GetPostProcessStepCount;
    property PostProcessStep[const aIndex: Integer]: TtsPostProcessStep read GetPostProcessStep;
    property AddResizingBorder: Boolean read fAddResizingBorder write fAddResizingBorder;

    function GenerateChar(const aCharCode: WideChar; const aFont: TtsFont; const aRenderer: TtsRenderer): TtsChar;

    function AddPostProcessStep(const aStep: TtsPostProcessStep): TtsPostProcessStep;
    function InsertPostProcessStep(const aIndex: Integer; const aStep: TtsPostProcessStep): TtsPostProcessStep;
    procedure DeletePostProcessStep(const aIndex: Integer);
    procedure ClearPostProcessSteps;

    constructor Create;
    destructor Destroy; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsLineItemType = (
    tsItemTypeUnknown,
    tsItemTypeFont,
    tsItemTypeColor,
    tsItemTypeText,
    tsItemTypeSpace,
    tsItemTypeLineBreak,
    tsItemTypeTab,
    tsItemTypeSpacing);

  PtsLineItem = ^TtsLineItem;
  TtsLineItem = packed record
    Next: PtsLineItem;
    Prev: PtsLineItem;
    ItemType: TtsLineItemType;
    case TtsLineItemType of
      tsItemTypeFont: (
        Font: TtsFont
      );
      tsItemTypeColor: (
        Color: TtsColor4f;
      );
      tsItemTypeText, tsItemTypeSpace: (
        Text: PWideChar;      // text of this item
        TextWidth: Integer;   // width of text (in pixel)
      );
      tsItemTypeSpacing: (
        Spacing: Integer;
      );
  end;

  TtsLineFlag = (
    tsLastItemIsSpace
  );
  TtsLineFlags = set of TtsLineFlag;
  PtsBlockLine = ^TtsBlockLine;
  TtsBlockLine = packed record
    Next: PtsBlockLine;
    First: PtsLineItem;
    Last: PtsLineItem;

    Flags: TtsLineFlags;
    Width: Integer;       // absolut width of this line
    Height: Integer;      // absolute height of this line
    Spacing: Integer;     // spacing between lines
    Ascent: Integer;      // text ascent
    SpaceCount: Integer;  // number of words in this line
    AutoBreak: Boolean;   // automatically set linebreak
  end;

  TtsBlockFlag = (
    tsBlockFlagWordWrap
  );
  TtsBlockFlags = set of TtsBlockFlag;

  TtsClipping = (
    tsClipNone,           // no clipping
    tsClipWordBorder,     // draw all words that have at least one pixel inside the box
    tsClipCharBorder,     // draw all chars that have at least one pixel inside the box
    tsClipWordComplete,   // draw all words that are completly inside the box
    tsClipCharComplete    // draw all chars that are completly inside the box
  );

  TtsTextBlock = class(TObject)
  private
    fRenderer: TtsRenderer;

    fTop: Integer;
    fLeft: Integer;
    fWidth: Integer;
    fHeight: Integer;
    fFlags: TtsBlockFlags;
    fVertAlign: TtsVertAlignment;
    fHorzAlign: TtsHorzAlignment;
    fClipping: TtsClipping;

    fTextMetric: TtsTextMetric;
    fCurrentColor: TtsColor4f;
    fCurrentFont: TtsFont;
    fFirstLine: PtsBlockLine;
    fLastLine: PtsBlockLine;

    function GetRect: TtsRect;

    procedure PushLineItem(const aItem: PtsLineItem; const aUpdateLineWidth: Boolean = true);
    procedure PushSpacing(const aWidth: Integer);
    procedure FreeLineItems(var aItem: PtsLineItem);

    procedure FreeLines(var aItem: PtsBlockLine);

    function SplitText(aText: PWideChar): PtsLineItem;
    procedure SplitIntoLines(aItem: PtsLineItem);
    procedure TrimSpaces(const aLine: PtsBlockLine);
  protected
    property Lines: PtsBlockLine read fFirstLine;
    procedure PushNewLine;
    constructor Create(const aRenderer: TtsRenderer; const aTop, aLeft, aWidth, aHeight: Integer; const aFlags: TtsBlockFlags);
  public
    property Renderer:     TtsRenderer   read fRenderer;
    property CurrentColor: TtsColor4f    read fCurrentColor;
    property CurrentFont:  TtsFont       read fCurrentFont;
    property Rect:         TtsRect       read GetRect;
    property Width:        Integer       read fWidth;
    property Height:       Integer       read fHeight;
    property Flags:        TtsBlockFlags read fFlags;

    property Top:       Integer          read fTop       write fTop;
    property Left:      Integer          read fLeft      write fLeft;
    property VertAlign: TtsVertAlignment read fVertAlign write fVertAlign;
    property HorzAlign: TtsHorzAlignment read fHorzAlign write fHorzAlign;
    property Clipping:  TtsClipping      read fClipping  write fClipping;

    procedure ChangeFont(const aFont: TtsFont);
    procedure ChangeColor(const aColor: TtsColor4f);

    function GetActualBlockHeight: Integer;

    procedure TextOutA(const aText: PAnsiChar);
    procedure TextOutW(const aText: PWideChar);

    destructor Destroy; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsRenderer = class(TObject)
  private
    fContext: TtsContext;
    fFormat: TtsFormat;
    fSaveImages: Boolean;
    fCritSec: TCriticalSection;
  protected
    function  AddRenderRef(const aChar: TtsChar; const aCharImage: TtsImage): TtsCharRenderRef; virtual; abstract;
    procedure RemoveRenderRef(const aCharRef: TtsCharRenderRef); virtual; abstract;

    procedure BeginRender; virtual;
    procedure EndRender; virtual;

    procedure SetDrawPos(const X, Y: Integer); virtual; abstract;
    procedure MoveDrawPos(const X, Y: Integer); virtual; abstract;
    procedure SetColor(const aColor: TtsColor4f); virtual; abstract;
    procedure Render(const aCharRef: TtsCharRenderRef); virtual; abstract;
  public
    property Context:    TtsContext read fContext;
    property Format:     TtsFormat  read fFormat;
    property SaveImages: Boolean    read fSaveImages write fSaveImages;

    function BeginBlock(const aTop, aLeft, aWidth, aHeight: Integer; const aFlags: TtsBlockFlags): TtsTextBlock;
    procedure EndBlock(var aBlock: TtsTextBlock);

    constructor Create(const aContext: TtsContext; const aFormat: TtsFormat);
    destructor Destroy; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsContext = class(TObject)
  private
    fID: Cardinal;

    fCodePage: TtsCodePage;
    fCodePagePtr: Pointer;
    fCodePageFunc: TtsAnsiToWideCharFunc;
    fCodePageDefault: WideChar;
  public
    function AnsiToWide(const aText: PAnsiChar): PWideChar;

    constructor Create;
    destructor Destroy; override;
  end;

  EtsException = class(Exception);
  EtsOutOfRange = class(EtsException)
  public
    constructor Create(const aMin, aMax, aIndex: Integer);
  end;

const
  IMAGE_MODES_REPLACE: TtsImageModes = (tsModeReplace, tsModeReplace, tsModeReplace, tsModeReplace);
  IMAGE_MODES_NORMAL:  TtsImageModes = (tsModeReplace, tsModeReplace, tsModeReplace, tsModeModulate);

  COLOR_CHANNELS_RGB:  TtsColorChannels = [tsChannelRed, tsChannelGreen, tsChannelBlue];
  COLOR_CHANNELS_RGBA: TtsColorChannels = [tsChannelRed, tsChannelGreen, tsChannelBlue, tsChannelAlpha];

function  tsStrAlloc(aSize: Cardinal): PWideChar;
function  tsStrNew(const aText: PWideChar): PWideChar;
procedure tsStrDispose(const aText: PWideChar);
function  tsStrLength(aText: PWideChar): Cardinal;
function  tsStrCopy(aDst, aSrc: PWideChar): PWideChar;
function  tsISO_8859_1ToWide(aDst: PWideChar; const aSize: Integer; aSrc: PAnsiChar): Integer;
function  tsUTF8ToWide(aDst: PWideChar; const aSize: Integer; const aSrc: PAnsiChar; const aDefaultChar: WideChar): Integer;

function tsColor4f(r, g, b, a: Single): TtsColor4f;
function tsRect(const l, t, r, b: Integer): TtsRect;
function tsPosition(const x, y: Integer): TtsPosition;

function  tsFormatSize(const aFormat: TtsFormat): Integer;
procedure tsFormatMap(const aFormat: TtsFormat; var aData: PByte; const aColor: TtsColor4f);
procedure tsFormatUnmap(const aFormat: TtsFormat; var aData: PByte; out aColor: TtsColor4f);

function tsImageModeFuncIgnore(const aSource, aDest: Single): Single;
function tsImageModeFuncReplace(const aSource, aDest: Single): Single;
function tsImageModeFuncModulate(const aSource, aDest: Single): Single;

implementation

var
  gLastContextID: Cardinal = 0;

const
  IMAGE_MODE_FUNCTIONS: array[TtsImageMode] of TtsImageModeFunc = (
    @tsImageModeFuncIgnore,
    @tsImageModeFuncReplace,
    @tsImageModeFuncModulate);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Helper////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsStrAlloc(aSize: Cardinal): PWideChar;
begin
  aSize := (aSize + 1) shl 1;
  GetMem(result, aSize);
  FillChar(result^, aSize, 0);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsStrNew(const aText: PWideChar): PWideChar;
begin
  result := tsStrAlloc(tsStrLength(aText));
  tsStrCopy(result, aText);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure tsStrDispose(const aText: PWideChar);
begin
  FreeMem(aText);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsStrLength(aText: PWideChar): Cardinal;
begin
  result := 0;
  if Assigned(aText) then
    while (ord(aText^) <> 0) do begin
      inc(result);
      inc(aText);
    end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsStrCopy(aDst, aSrc: PWideChar): PWideChar;
begin
  result := aDst;
  if Assigned(aDst) and Assigned(aSrc) then
    while ord(aSrc^) <> 0 do begin
      aDst^ := aSrc^;
      inc(aDst);
      inc(aSrc);
    end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsISO_8859_1ToWide(aDst: PWideChar; const aSize: Integer; aSrc: PAnsiChar): Integer;
begin
  result := 0;
  if Assigned(aDst) and Assigned(aSrc) then
    while (ord(aSrc^) <> 0) do begin
      aDst^ := WideChar(aSrc^);
      inc(aDst);
      inc(aSrc);
      inc(result);
    end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsUTF8ToWide(aDst: PWideChar; const aSize: Integer; const aSrc: PAnsiChar; const aDefaultChar: WideChar): Integer;

  procedure AddToDest(aCharCode: UInt64);
  begin
    if (aCharCode > $FFFF) then
      aCharCode := ord(aDefaultChar);

    PWord(aDst)^ := aCharCode;
    inc(aDst);
    result := result + 1;
  end;

const
  STATE_STARTBYTE  = 0;
  STATE_FOLLOWBYTE = 1;
var
  cc: QWord;
  len, state, c: Integer;
  p: PByte;
  tmp: Byte;
begin
  result := 0;
  if not Assigned(aDst) or not Assigned(aSrc) or (aSize <= 0) then
    exit;

  p     := PByte(aSrc);
  len   := Length(aSrc);
  state := STATE_STARTBYTE;
  while (len > 0) do begin
    case state of
      STATE_STARTBYTE: begin
        if (p^ and %10000000 = 0) then begin
          AddToDest(p^);
        end else if (p^ and %01000000 > 0) then begin
          tmp := p^;
          c   := 0;
          while (tmp and %10000000) > 0 do begin
            inc(c);
            tmp := tmp shl 1;
          end;
          cc    := p^ and ((1 shl (7 - c)) - 1);
          state := STATE_FOLLOWBYTE;
          c := c - 1;
        end;
      end;

      STATE_FOLLOWBYTE: begin
        if ((p^ and %11000000) = %10000000) then begin
          cc := (cc shl 6) or (p^ and %00111111);
          c := c - 1;
          if (c = 0) then begin
            AddToDest(cc);
            state := STATE_STARTBYTE;
          end;
        end else
          state := STATE_STARTBYTE;
      end;
    end;

    if (result >= aSize) then
      exit;
    inc(p);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsColor4f(r, g, b, a: Single): TtsColor4f;
begin
  result.r := r;
  result.g := g;
  result.b := b;
  result.a := a;
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
function tsFormatSize(const aFormat: TtsFormat): Integer;
begin
  case aFormat of
    tsFormatRGBA8:      result := 4;
    tsFormatLumAlpha8:  result := 2;
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
        aData^ := Trunc($FF * aColor.arr[i]);
        inc(aData);
      end;
    end;

    tsFormatLumAlpha8: begin
      s := 0.30 * aColor.r + 0.59 * aColor.g + 0.11 * aColor.b;
      aData^ := Trunc($FF * s);         inc(aData);
      aData^ := Trunc($FF * s);         inc(aData);
      aData^ := Trunc($FF * s);         inc(aData);
      aData^ := Trunc($FF * aColor.a);  inc(aData);
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
//TtsKernel1D///////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsKernel1D.Create(const aRadius, aStrength: Single);
var
  TempRadius, SQRRadius, TempStrength, TempValue: Double;
  Idx: Integer;

  function CalcValue(const aIndex: Integer): Single;
  var
    Temp: Double;
  begin
    Temp   := Max(0, Abs(aIndex) - TempStrength);
    Temp   := Sqr(Temp * TempRadius) / SQRRadius;
    result := Exp(-Temp);
  end;

begin
  inherited Create;

  // calculate new radius and strength
  TempStrength := Min(aRadius - 1, aRadius * aStrength);
  TempRadius   := aRadius - TempStrength;
  SQRRadius    := sqr(TempRadius) * sqr(TempRadius);

  // caluculating size of the kernel
  Size := Round(TempRadius);
  while CalcValue(Size) > 0.001 do
    Inc(Size);
  Size      := Size -1;
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
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsImage//////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsImage.GetScanline(const aIndex: Integer): Pointer;
begin
  if not fHasScanlines then
    UpdateScanlines;

  if fHasScanlines and (aIndex >= 0) and (aIndex <= High(fScanlines)) then
    result := fScanlines[aIndex]
  else
    result := nil;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsImage.GetIsEmpty: Boolean;
begin
  result := not Assigned(fData);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsImage.SetData(const aData: Pointer; const aFormat: TtsFormat; const aWidth: Integer; const aHeight: Integer);
begin
  fHasScanlines := false;
  if Assigned(fData) then
    FreeMemory(fData);

  fData := aData;
  if Assigned(fData) then begin
    fWidth  := aWidth;
    fHeight := aHeight;
    fFormat := aFormat;
  end else begin
    fWidth  := 0;
    fHeight := 0;
    fFormat := tsFormatEmpty;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsImage.UpdateScanlines;
var
  i, LineSize: Integer;
  tmp: PByte;
begin
  LineSize := fWidth * tsFormatSize(fFormat);
  SetLength(fScanlines, fHeight);
  for i := 0 to fHeight-1 do begin
    tmp := fData;
    inc(tmp, i * LineSize);
    fScanlines[i] := tmp;
  end;
  fHasScanlines := true;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsImage.GetPixelAt(const x, y: Integer; out aColor: TtsColor4f): Boolean;
var
  p: PByte;
begin
  result := (x >= 0) and (x < Width) and (y >= 0) and (y < Height);
  if result then begin
    p := Scanline[y];
    inc(p, x * tsFormatSize(Format));
    tsFormatUnmap(Format, p, aColor);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsImage.Assign(const aImage: TtsImage);
var
  ImgData: Pointer;
  ImgSize: Integer;
begin
  ImgSize := aImage.Width * aImage.Height * tsFormatSize(aImage.Format);
  GetMem(ImgData, ImgSize);
  if Assigned(ImgData) then
    Move(aImage.Data, ImgData, ImgSize);
  SetData(ImgData, aImage.Format, aImage.Width, aImage.Height);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsImage.CreateEmpty(const aFormat: TtsFormat; const aWidth, aHeight: Integer);
var
  ImgData: PByte;
begin
  ImgData := AllocMem(aWidth * aHeight * tsFormatSize(aFormat));
  SetData(ImgData, aFormat, aWidth, aHeight);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsImage.LoadFromFunc(const aFunc: TtsImageFunc; const aArgs: Pointer);
var
  X, Y: Integer;
  c: TtsColor4f;
  p, tmp: PByte;
begin
  for Y := 0 to Height - 1 do begin
    p := ScanLine[Y];
    for X := 0 to Width - 1 do begin
      tmp := p;
      tsFormatUnmap(fFormat, tmp, c);
      aFunc(Self, X, Y, c, aArgs);
      tsFormatMap(fFormat, p, c);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsImage.Resize(const aNewWidth, aNewHeight, X, Y: Integer);
var
  ImgData: PByte;
  PixSize, LineSize, ImageSize, OrgLineSize: Integer;

  src, dst: PByte;
  YStart, YEnd, YPos, XStart, XEnd: Integer;
begin
  if (aNewHeight = 0) or (aNewWidth = 0) then begin
    SetData(nil);
    exit;
  end;

  PixSize     := tsFormatSize(Format);
  LineSize    := PixSize  * aNewWidth;
  ImageSize   := LineSize * aNewHeight;
  OrgLineSize := PixSize  * Width;

  GetMem(ImgData, ImageSize);
  try
    FillChar(ImgData^, ImageSize, 0);

    // positions
    YStart := Max(0, Y);
    YEnd   := Min(aNewHeight, Y + Height);
    XStart := Max(0, X);
    XEnd   := Min(aNewWidth, X + Width);

    // copy data
    for YPos := YStart to YEnd -1 do begin
      dst := ImgData;
      Inc(dst, LineSize * YPos + PixSize * XStart);

      src := fData;
      Inc(src, OrgLineSize * (YPos - Y) + PixSize * (XStart - X));

      Move(src^, dst^, (XEnd - XStart) * PixSize);
    end;

    // assign
    SetData(ImgData, Format, aNewWidth, aNewHeight);
  except
    FreeMem(ImgData);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsImage.FindMinMax(out aRect: TtsRect);
var
  X, Y: Integer;
  c: TtsColor4f;
  p: PByte;
begin
  aRect.Top := -1;
  aRect.Left := -1;
  aRect.Right := -1;
  aRect.Bottom := -1;

  // Search for MinMax
  for Y := 0 to Height-1 do begin
    p := ScanLine[Y];
    for X := 0 to Width-1 do begin
      tsFormatUnmap(Format, p, c);
      if c.a > 0 then begin
        if (X < aRect.Left) or (aRect.Left = -1) then
          aRect.Left := X;

        if (X+1 > aRect.Right) or (aRect.Right = -1) then
          aRect.Right := X+1;

        if (Y < aRect.Top) or (aRect.Top = -1) then
          aRect.Top := Y;

        if (Y+1 > aRect.Bottom) or (aRect.Bottom = -1) then
          aRect.Bottom := Y+1;
      end;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsImage.FillColor(const aColor: TtsColor4f; const aChannelMask: TtsColorChannels; const aModes: TtsImageModes);
var
  x, y: Integer;
  p: PByte;
  c: TtsColor4f;
  ch: TtsColorChannel;
  i: Integer;
begin
  for y := 0 to Height-1 do begin
    p := Scanline[y];
    for x := 0 to Width-1 do begin
      tsFormatUnmap(Format, p, c);
      for i := 0 to 3 do begin
        ch := TtsColorChannel(i);
        if (ch in aChannelMask) then
          c.arr[i] := IMAGE_MODE_FUNCTIONS[aModes[ch]](aColor.arr[i], c.arr[i]);
      end;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsImage.FillPattern(const aPattern: TtsImage; X, Y: Integer; const aChannelMask: TtsColorChannels; const aModes: TtsImageModes);
var
  _x, _y, posX, i: Integer;
  src, dst, tmp: PByte;
  cSrc, cDst: TtsColor4f;
  ch: TtsColorChannel;
begin
  if x < 0 then
    x := Random(aPattern.Width);
  if y < 0 then
    y := Random(aPattern.Height);

  for _y := 0 to Height-1 do begin
    src := aPattern.Scanline[(y + _y) mod aPattern.Height];
    dst := Scanline[_y];

    inc(src, x);
    posX := x;

    for _x := 0 to Width-1 do begin
      if (posX >= aPattern.Width) then begin
        src  := aPattern.Scanline[(y + _y) mod aPattern.Height];
        posX := 0;
      end;

      tmp := dst;
      tsFormatUnmap(Format, src, cSrc);
      tsFormatUnmap(Format, tmp, cDst);
      for i := 0 to 3 do begin
        ch := TtsColorChannel(i);
        if (ch in aChannelMask) then
          cDst.arr[i] := IMAGE_MODE_FUNCTIONS[aModes[ch]](cSrc.arr[i], cDst.arr[i]);
      end;
      tsFormatMap(Format, dst, cDst);
      inc(posX);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsImage.BlendImage(const aImage: TtsImage; const X, Y: Integer);
var
  _x, _y, i: Integer;
  c, cOver, cUnder: TtsColor4f;
  FaqOver, FaqUnder: Single;
  UnionRect, IntersectRect: TtsRect;
  NewSize: TtsPosition;
  ImgSize: Integer;
  ImgData, dst, src, pOver, pUnder: PByte;
  tmpLines: array of Pointer;
begin
  UnionRect := tsRect(
    Min(X, 0),
    Min(Y, 0),
    Max(X + aImage.Width,  Width),
    Max(Y + aImage.Height, Height));
  IntersectRect := tsRect(
    Max(X, 0),
    Max(Y, 0),
    Min(X + aImage.Width, Width),
    Min(X + aImage.Height, Height));
  NewSize := tsPosition(
    UnionRect.Right  - UnionRect.Left,
    UnionRect.Bottom - UnionRect.Top);

  ImgSize := NewSize.x * NewSize.y * tsFormatSize(Format);
  GetMem(ImgData, ImgSize);
  try
    FillByte(ImgData^, ImgSize, $00);

    // temporary scanlines
    SetLength(tmpLines, NewSize.y);
    for _y := 0 to NewSize.y-1 do begin
      tmpLines[_y] := ImgData;
      inc(tmpLines[_y], _y * NewSize.y);
    end;

    // copy data from underlaying image
    for _y := 0 to Height-1 do begin
      src := Scanline[_y];
      dst := tmpLines[_y - UnionRect.Top];
      dec(dst, UnionRect.Left);
      for _x := 0 to Width-1 do begin
        dst^ := src^;
        inc(src);
        inc(dst);
      end;
    end;

    // copy data from overlaying image
    for _y := 0 to aImage.Height-1 do begin
      src := aImage.Scanline[_y];
      dst := tmpLines[_y + y - UnionRect.Top];
      inc(dst, X - UnionRect.Left);
      for _x := 0 to Width-1 do begin
        dst^ := src^;
        inc(src);
        inc(dst);
      end;
    end;

    // blend overlapped
    for _y := IntersectRect.Top to IntersectRect.Bottom-1 do begin
      pOver := aImage.Scanline[_y - Min(IntersectRect.Top, UnionRect.Top)];
      inc(pOver, IntersectRect.Left - UnionRect.Left);

      pUnder := Scanline[_y - Min(IntersectRect.Top, 0)];
      inc(pUnder, IntersectRect.Left);

      dst := tmpLines[_y - Min(Y, 0)];
      inc(dst, IntersectRect.Left - Min(X, 0));

      for _x := IntersectRect.Left to IntersectRect.Right-1 do begin
        tsFormatUnmap(aImage.Format, pOver,  cOver);
        tsFormatUnmap(Format,        pUnder, cUnder);
        c.a := cOver.a + cUnder.a * (1 - cOver.a);
        if (c.a > 0) then begin
          FaqUnder := (cUnder.a * (1 - cOver.a)) / c.a;
          FaqOver  :=  cOver.a                   / c.a;
          for i := 0 to 2 do
            c.arr[i] := cOver.arr[i] * FaqOver + cUnder.arr[i] * FaqUnder;
        end else begin
          c.r := 0;
          c.g := 0;
          c.b := 0;
        end;
        tsFormatMap(Format, dst, c);
      end;
    end;
  except
    FreeMem(ImgData);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsImage.Blur(const aHorzKernel, aVertKernel: TtsKernel1D; const aChannelMask: TtsColorChannels);
var
  tmpImage: TtsImage;

  procedure DoBlur(const aSrc, aDst: TtsImage; const aKernel: TtsKernel1D; const ShiftX, ShiftY: Integer);
  var
    x, y, i, j: Integer;
    src, dst: PByte;
    v: Single;
    c, tmp: TtsColor4f;
  begin
    for y := 0 to Height-1 do begin
      src := aSrc.Scanline[y];
      dst := aDst.Scanline[y];
      for x := 0 to Width-1 do begin

        // read color and clear channels
        v := 0;
        tsFormatUnmap(aSrc.Format, src, c);
        for j := 0 to 3 do
          if (TtsColorChannel(j) in aChannelMask) then
            c.arr[j] := 0;

        // do blur
        for i := 0 to aKernel.ItemCount-1 do with aKernel.Items[i] do begin
          if aSrc.GetPixelAt(x + Offset * ShiftX, y + Offset * ShiftY, tmp) then begin
            for j := 0 to 3 do begin
              if (TtsColorChannel(j) in aChannelMask) then
                c.arr[j] := c.arr[j] + tmp.arr[j] * Value;
            end;
            v := v + Value;
          end;
        end;

        // calc final color and write
        for j := 0 to 3 do
          if (TtsColorChannel(i) in aChannelMask) then
            c.arr[j] := c.arr[j] / v;
        tsFormatMap(aDst.Format, dst, c);
      end;
    end;
  end;

begin
  tmpImage := TtsImage.Create;
  try
    tmpImage.CreateEmpty(Format, Width, Height);
    tmpImage.FillColor(tsColor4f(1, 1, 1, 0), COLOR_CHANNELS_RGBA, IMAGE_MODES_REPLACE);

    DoBlur(self, tmpImage, aHorzKernel, 1, 0);
    DoBlur(tmpImage, self, aVertKernel, 0, 1);
  finally
    FreeAndNil(tmpImage);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsImage.AddResizingBorder;
var
  c, cTmp, cSum: TtsColor4f;
  x, y, cnt: Integer;
  p, tmp: PByte;

  procedure AddCol(const aColor: TtsColor4f);
  var
    i: Integer;
  begin
    if (aColor.a > 0) then begin
      inc(cnt);
      for i := 0 to 2 do
        cSum.arr[i] := cSum.arr[i] + cTmp.arr[i];
    end;
  end;

var
  i: Integer;
begin
  Resize(Width + 4, Height + 4, 2, 2);
  for y := 0 to Height-1 do begin
    p := Scanline[y];
    for x := 0 to Width-1 do begin
      FillByte(cSum, SizeOf(cSum), 0);
      cnt := 0;
      tmp := p;
      tsFormatUnmap(Format, tmp, c);
      if (c.a = 0) then begin

        // row - 1
        if (y > 0) then begin

          // row - 1 | col
          GetPixelAt(x, y-1, cTmp);
          AddCol(cTmp);

          //row - 1 | col - 1
          if (x > 0) then begin
            GetPixelAt(x-1, y-1, cTmp);
            AddCol(cTmp);
          end;

          // row - 1 | col + 1
          if (x < Width-1) then begin
            GetPixelAt(x+1, y-1, cTmp);
            AddCol(cTmp);
          end;
        end;

        // row + 1
        if (y < Height-1) then begin
          // row - 1 | col
          GetPixelAt(x, y+1, cTmp);
          AddCol(cTmp);

          //row + 1 | col - 1
          if (x > 0) then begin
            GetPixelAt(x-1, y+1, cTmp);
            AddCol(cTmp);
          end;

          // row + 1 | col + 1
          if (x < Width-1) then begin
            GetPixelAt(x+1, y+1, cTmp);
            AddCol(cTmp);
          end;
        end;

        //row | col - 1
        if (x > 0) then begin
          GetPixelAt(x-1, y+1, cTmp);
          AddCol(cTmp);
        end;

        // row | col + 1
        if (x < Width-1) then begin
          GetPixelAt(x+1, y+1, cTmp);
          AddCol(cTmp);
        end;

        // any pixel next to the transparent pixel they are opaque?
        if (cnt > 0) then begin
          for i := 0 to 2 do
            c.arr[i] := cSum.arr[i] / cnt;
        end;
      end;
      tsFormatMap(Format, p, c);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsImage.Create;
begin
  inherited Create;
  SetData(nil);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsChar///////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsChar.Create(const aCharCode: WideChar);
begin
  inherited Create;
  fCharCode := aCharCode;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsFont///////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFont.HasChar(const aCharCode: WideChar): Boolean;
begin
  result := Assigned(GetChar(aCharCode));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFont.GetChar(const aCharCode: WideChar): TtsChar;
var
  Chars: PtsFontCharArray;
begin
  Chars := fChars[(Ord(aCharCode) shr 8) and $FF];
  if Assigned(Chars) then
    result := Chars^.Chars[Ord(aCharCode) and $FF]
  else
    result := nil;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsFont.AddChar(const aCharCode: WideChar; const aChar: TtsChar);
var
  h, l: Integer;
  Chars: PtsFontCharArray;
begin
  h := (Ord(aCharCode) shr 8) and $FF;
  Chars := fChars[h];
  if not Assigned(Chars) then begin
    New(Chars);
    FillChar(Chars^, SizeOf(Chars^), 0);
    fChars[h] := Chars;
  end;

  if Assigned(Chars) then begin
    l := Ord(aCharCode) and $FF;
    Chars^.Chars[l] := aChar;
    inc(Chars^.CharCount);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsFont.AddChar(const aCharCode: WideChar);
var
  c: TtsChar;
begin
  if not fCreateChars or (Ord(aCharCode) > 0) then
    exit;

  c := GetChar(aCharCode);
  if Assigned(c) then
    exit;

  c := fCreator.GenerateChar(aCharCode, self, fRenderer);
  if Assigned(c) then
    AddChar(aCharCode, c);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsFont.AddCharRange(const aCharCodeBeg, aCharCodeEnd: WideChar);
var
  c: WideChar;
begin
  for c := aCharCodeBeg to aCharCodeEnd do
    AddChar(c);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsFont.RemoveChar(const aCharCode: WideChar);
var
  h, l: Integer;
  Chars: PtsFontCharArray;
  c: TtsChar;
begin
  // find char array
  h := (Ord(aCharCode) shr 8) and $FF;
  Chars := fChars[h];
  if not Assigned(Chars) then
    exit;

  // find char
  l := Ord(aCharCode) and $FF;
  c := Chars^.Chars[l];
  if not Assigned(c) then
    exit;

  // remove char
  Chars^.Chars[l] := nil;
  dec(Chars^.CharCount);
  if (Chars^.CharCount <= 0) then begin
    fChars[h] := nil;
    Dispose(Chars);
  end;

  if Assigned(c.RenderRef) then begin
    fRenderer.RemoveRenderRef(c.RenderRef);
    c.RenderRef.Free;
  end;
  FreeAndNil(c);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsFont.ClearChars;
var
  h, l: Integer;
  Chars: PtsFontCharArray;
  c: TtsChar;
begin
  for h := Low(fChars) to High(fChars) do begin
    Chars := fChars[h];
    if Assigned(Chars) then begin
      for l := Low(Chars^.Chars) to High(Chars^.Chars) do begin
        c := Chars^.Chars[l];
        if Assigned(c) then begin
          if Assigned(c.RenderRef) then begin
            fRenderer.RemoveRenderRef(c.RenderRef);
            c.RenderRef.Free;
          end;
          FreeAndNil(c);
        end;
      end;
      Dispose(Chars);
      fChars[h] := nil;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFont.GetTextWidthW(aText: PWideChar): Integer;
var
  c: TtsChar;
begin
  result := 0;
  if not Assigned(aText) then
    exit;

  while (aText^ <> #0) do begin
    c := GetChar(aText^);
    if not Assigned(c) then
      c := GetChar(fDefaultChar);
    if Assigned(c) then begin
      if (result > 0) then
        result := result + CharSpacing;
      result := result + c.Advance;
    end;
    inc(aText);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsFont.GetTextMetric(out aMetric: TtsTextMetric);
begin
  aMetric.Ascent            := Ascent;
  aMetric.Descent           := Descent;
  aMetric.ExternalLeading   := ExternalLeading;
  aMetric.BaseLineOffset    := BaseLineOffset;
  aMetric.CharSpacing       := CharSpacing;
  aMetric.LineHeight        := Ascent + Descent + ExternalLeading;
  aMetric.LineSpacing       := LineSpacing;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsFont.Create(const aRenderer: TtsRenderer; const aCreator: TtsFontCreator; const aCopyright, aFaceName,
  aStyleName, aFullName: String; const aSize, aCharSpacing, aLineSpacing: Integer; const aStyle: TtsFontStyles;
  const aAntiAliasing: TtsAntiAliasing);
begin
  inherited Create;
  fRenderer     := aRenderer;
  fCreator      := aCreator;
  fDefaultChar  := '?';
  fCopyright    := aCopyright;
  fFaceName     := aFaceName;
  fStyleName    := aStyleName;
  fFullName     := aFullName;
  fSize         := aSize;
  fStyle        := aStyle;
  fAntiAliasing := aAntiAliasing;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsFont.Destroy;
begin
  ClearChars;
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsPostProcessStep////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsPostProcessStep.ClearList(const aList: TList);
var
  i: Integer;
  p: PtsPostProcessStepRange;
begin
  for i := 0 to aList.Count-1 do begin
    p := aList[i];
    Dispose(p);
  end;
  aList.Clear;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsPostProcessStep.IsInRange(const aCharCode: WideChar): Boolean;
var
  i: Integer;
  p: PtsPostProcessStepRange;
begin
  result := (fIncludeCharRange.Count = 0);

  if not result then for i := 0 to fIncludeCharRange.Count-1 do begin
    p := fIncludeCharRange[i];
    if (aCharCode >= p^.StartChar) and (aCharCode <= p^.EndChar) then begin
      result := true;
      break;
    end;
  end;

  if result then for i := 0 to fExcludeCharRange.Count-1 do begin
    p := fExcludeCharRange[i];
    if (aCharCode >= p^.StartChar) and (aCharCode <= p^.EndChar) then begin
      result := false;
      break;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsPostProcessStep.AddUsageRange(const aUsage: TtsFontProcessStepUsage; const aStartChar, aEndChar: WideChar);
var
  p: PtsPostProcessStepRange;
begin
  New(p);

  p^.StartChar := aStartChar;
  p^.EndChar   := aEndChar;

  case aUsage of
    tsUsageInclude:
      fIncludeCharRange.Add(p);
    tsUsageExclude:
      fExcludeCharRange.Add(p);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsPostProcessStep.AddUsageChars(const aUsage: TtsFontProcessStepUsage; aChars: PWideChar);
begin
  if Assigned(aChars) then
    while (aChars^ <> #0) do begin
      AddUsageRange(aUsage, aChars^, aChars^);
      inc(aChars);
    end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsPostProcessStep.ClearIncludeRange;
begin
  ClearList(fIncludeCharRange);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsPostProcessStep.ClearExcludeRange;
begin
  ClearList(fExcludeCharRange);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsPostProcessStep.Create;
begin
  inherited Create;
  fIncludeCharRange := TList.Create;
  fExcludeCharRange := TList.Create;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsPostProcessStep.Destroy;
begin
  ClearList(fIncludeCharRange);
  ClearList(fExcludeCharRange);
  FreeAndNil(fIncludeCharRange);
  FreeAndNil(fExcludeCharRange);
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsFontCreator//////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFontCreator.GetPostProcessStepCount: Integer;
begin
  result := fPostProcessSteps.Count;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFontCreator.GetPostProcessStep(const aIndex: Integer): TtsPostProcessStep;
begin
  if (aIndex >= 0) and (aIndex < fPostProcessSteps.Count) then
    Result := TtsPostProcessStep(fPostProcessSteps[aIndex])
  else
    raise EtsOutOfRange.Create(0, fPostProcessSteps.Count-1, aIndex);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsFontCreator.DrawLine(const aChar: TtsChar; const aCharImage: TtsImage; aLinePosition, aLineSize: Integer);
var
  NewSize, NewPos: TtsPosition;
  YOffset, y: Integer;

  procedure FillLine(aData: PByte);
  var
    w, i: Integer;
    c: TtsColor4f;
    tmp: PByte;
  begin
    w := NewSize.x;
    while (w > 0) do begin
      tmp := aData;
      tsFormatUnmap(aCharImage.Format, tmp, c);
      for i := 0 to 3 do
        c.arr[i] := 1.0;
      tsFormatMap(aCharImage.Format, aData, c);
      dec(w);
    end;
  end;

begin
  if aLineSize <= 0 then
    exit;

  aLinePosition := aLinePosition - aLineSize;

  // calculate width and height
  NewPos.x  := 0;
  NewPos.y  := 0;
  NewSize.x := aCharImage.Width;
  NewSize.y := aCharImage.Height;

  // expand image to the full advance
  if aChar.Advance > aCharImage.Width then
    NewSize.x := aChar.Advance;

  // add glyph position to image width and set position
  if aChar.GlyphOrigin.x > aChar.GlyphRect.Left then begin
    NewSize.x := NewSize.x + aChar.GlyphOrigin.x;
    NewPos.x  := aChar.GlyphOrigin.x;
  end;
  if (aChar.GlyphOrigin.x < 0) then
    NewSize.x := NewSize.x - aChar.GlyphOrigin.x;

  // line is under the image
  if aLinePosition < (aChar.GlyphOrigin.y - aCharImage.Height) then
    NewSize.y := NewSize.y + (aChar.GlyphOrigin.y - aCharImage.Height - aLinePosition);

  // line is above the image
  if aLinePosition + aLineSize > aChar.GlyphOrigin.y then begin
    NewPos.y  := ((aLinePosition + aLineSize) - aChar.GlyphOrigin.y);
    NewSize.y := NewSize.y + NewPos.y;
  end;

  // resize
  aCharImage.Resize(NewSize.x, NewSize.y, NewPos.x, NewPos.y);

  // draw lines
  YOffset := (aChar.GlyphOrigin.y + NewPos.y) - aLinePosition;
  for y := 1 to aLineSize do
    FillLine(aCharImage.ScanLine[YOffset - y]);

  // move glyph rect
  aChar.GlyphRect := tsRect(
    aChar.GlyphRect.Left   + NewPos.x,
    aChar.GlyphRect.Right  + NewPos.x,
    aChar.GlyphRect.Top    + NewPos.y,
    aChar.GlyphRect.Bottom + NewPos.y);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsFontCreator.DoPostProcess(const aChar: TtsChar; const aCharImage: TtsImage);
var
  i: Integer;
  step: TtsPostProcessStep;
begin
  if not aCharImage.IsEmpty then begin
    for i := 0 to fPostProcessSteps.Count-1 do begin
      step := TtsPostProcessStep(fPostProcessSteps[i]);
      if step.IsInRange(aChar.CharCode) then
        step.Execute(aChar, aCharImage);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFontCreator.GenerateChar(const aCharCode: WideChar; const aFont: TtsFont; const aRenderer: TtsRenderer): TtsChar;
var
  GlyphOrigin, GlyphSize: TtsPosition;
  Advance: Integer;
  CharImage: TtsImage;
  Char: TtsChar;
begin
  result := nil;
  if (Ord(aCharCode) = 0) or
     not GetGlyphMetrics(aCharCode, GlyphOrigin.x, GlyphOrigin.y, GlyphSize.x, GlyphSize.y, Advance) or
     not ((GlyphOrigin.x <> 0) or (GlyphOrigin.y <> 0) or (GlyphSize.x <> 0) or (GlyphSize.y <> 0) or (Advance <> 0)) then
        exit;

  CharImage := TtsImage.Create;
  try
    if aRenderer.SaveImages then begin
      if (GlyphSize.x > 0) and (GlyphSize.y > 0) then begin
        GetCharImage(aCharCode, CharImage);
      end else if ([tsStyleUnderline, tsStyleStrikeout] * aFont.Style <> []) then begin
        CharImage.CreateEmpty(aRenderer.Format, Advance, 1);
        GlyphOrigin.y := 1;
      end;
    end;

    Char := TtsChar.Create(aCharCode);
    Char.GlyphOrigin := GlyphOrigin;
    Char.GlyphRect   := tsRect(0, 0, CharImage.Width, CharImage.Height);
    Char.Advance     := Advance;

    if (aRenderer.SaveImages) then begin
      try
        if (tsStyleUnderline in aFont.Style) then
          DrawLine(Char, CharImage, aFont.UnderlinePos, aFont.UnderlineSize);
        if (tsStyleUnderline in aFont.Style) then
          DrawLine(Char, CharImage, aFont.StrikeoutPos, aFont.StrikeoutSize);
      except
        CharImage.FillColor(tsColor4f(1, 0, 0, 0), COLOR_CHANNELS_RGB, IMAGE_MODES_NORMAL);
      end;

      DoPostProcess(Char, CharImage);

      if AddResizingBorder then begin
        Char.HasResisingBorder := true;
        Char.GlyphRect := tsRect(
          Char.GlyphRect.Left   + 1,
          Char.GlyphRect.Top    + 1,
          Char.GlyphRect.Right  + 1,
          Char.GlyphRect.Bottom + 1);
        CharImage.AddResizingBorder;
      end;

      Char.RenderRef := aRenderer.AddRenderRef(Char, CharImage);
    end;
  finally
    FreeAndNil(CharImage);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFontCreator.AddPostProcessStep(const aStep: TtsPostProcessStep): TtsPostProcessStep;
begin
  result := aStep;
  fPostProcessSteps.Add(aStep);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFontCreator.InsertPostProcessStep(const aIndex: Integer; const aStep: TtsPostProcessStep): TtsPostProcessStep;
begin
  result := aStep;
  fPostProcessSteps.Insert(aIndex, aStep);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsFontCreator.DeletePostProcessStep(const aIndex: Integer);
begin
  if (aIndex >= 0) and (aIndex < fPostProcessSteps.Count) then
    fPostProcessSteps.Delete(aIndex)
  else
    raise EtsOutOfRange.Create(0, fPostProcessSteps.Count-1, aIndex);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsFontCreator.ClearPostProcessSteps;
begin
  fPostProcessSteps.Clear;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsFontCreator.Create;
begin
  inherited Create;
  fPostProcessSteps := TObjectList.Create(true);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsFontCreator.Destroy;
begin
  ClearPostProcessSteps;
  FreeAndNil(fPostProcessSteps);
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsTextBlock//////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsTextBlock.GetRect: TtsRect;
begin
  result.Left   := fLeft;
  result.Top    := fTop;
  result.Right  := fLeft + fWidth;
  result.Bottom := fTop  + fHeight;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsTextBlock.PushLineItem(const aItem: PtsLineItem; const aUpdateLineWidth: Boolean);
begin
  if not Assigned(fLastLine) then
    PushNewLine;

  if Assigned(fLastLine^.Last) then begin
    aItem^.Prev           := fLastLine^.Last;
    aItem^.Next           := nil;
    fLastLine^.Last^.Next := aItem;
    fLastLine^.Last       := aItem;
  end;

  if not Assigned(fLastLine^.First) then begin
    fLastLine^.First := aItem;
    fLastLine^.Last  := aItem;
  end;

  case aItem^.ItemType of
    tsItemTypeSpace, tsItemTypeText:
      fLastLine^.Width := fLastLine^.Width + aItem^.TextWidth;
    tsItemTypeSpacing:
      fLastLine^.Width := fLastLine^.Width + aItem^.Spacing;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsTextBlock.PushSpacing(const aWidth: Integer);
var
  p: PtsLineItem;
begin
  new(p);
  FillByte(p^, SizeOf(p^), 0);
  p^.ItemType := tsItemTypeSpacing;
  p^.Spacing  := aWidth;
  PushLineItem(p);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsTextBlock.FreeLineItems(var aItem: PtsLineItem);
var
  p: PtsLineItem;
begin
  while Assigned(aItem) do begin
    p := aItem;
    case p^.ItemType of
      tsItemTypeText, tsItemTypeSpace:
        tsStrDispose(p^.Text);
    end;
    aItem := p^.Next;
    Dispose(p);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsTextBlock.FreeLines(var aItem: PtsBlockLine);
var
  p: PtsBlockLine;
begin
  while Assigned(aItem) do begin
    p := aItem;
    FreeLineItems(p^.First);
    p^.Last := p^.First;
    aItem := p^.Next;
    Dispose(p);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsTextBlock.SplitText(aText: PWideChar): PtsLineItem;
var
  TextBegin: PWideChar;
  TextLength: Integer;
  State: TtsLineItemType;
  LastItem: PtsLineItem;
  c: WideChar;

  procedure AddItem(const aItem: PtsLineItem);
  begin
    if Assigned(result) then begin
      LastItem^.Next := aItem;
      aItem^.Prev    := LastItem;
      aItem^.Next    := nil;
      LastItem       := aItem;
    end;

    if not Assigned(result) then begin
      result   := aItem;
      LastItem := aItem;
    end;
  end;

  procedure ExtractWord;
  var
    p: PtsLineItem;
    Text: PWideChar;
  begin
    if (State = tsItemTypeUnknown) then
      exit;

    new(p);
    FillByte(p^, SizeOf(p^), 0);
    p^.ItemType := State;

    case State of
      tsItemTypeText, tsItemTypeSpace: begin
        p^.Text    := tsStrAlloc(TextLength);
        TextLength := 0;
        Text       := p^.Text;
        while (TextBegin <> aText) do begin
          Text^ := TextBegin^;
          inc(Text,      1);
          inc(TextBegin, 1);
        end;
        AddItem(p);
      end;

      tsItemTypeLineBreak: begin
        if ((c = #13) and (aText^ = #10)) or not (aText^ in [#10, #13]) then begin
          Dispose(p);
          p := nil;
        end else begin
          AddItem(p);
        end;
        TextBegin := aText;
      end;

      tsItemTypeTab: begin
        AddItem(p);
      end;

    else
      Dispose(p);
      p := nil;
    end;
  end;

begin
  result     := nil;
  LastItem   := nil;
  TextBegin  := aText;
  TextLength := 0;
  State      := tsItemTypeUnknown;

  if not Assigned(aText) then
    exit;

  while (aText^ <> #0) do begin
    case aText^ of

      // tabulator
      #$0009: begin
        ExtractWord;
        inc(TextBegin, 1);
        State := tsItemTypeTab;
      end;

      // line breaks
      #$000D, #$000A: begin
        if (State <> tsItemTypeLineBreak) then begin
          ExtractWord;
          State := tsItemTypeLineBreak;
          c := #0;
        end;
        ExtractWord;
        c := aText^;
      end;

      // spaces
      #$0020: begin
        if (State <> tsItemTypeSpace) then
          ExtractWord;
        State := tsItemTypeSpace;
      end;

    else
      if (State <> tsItemTypeText) then
        ExtractWord;
      State := tsItemTypeText;
    end;

    inc(aText,      1);
    inc(TextLength, 1);
  end;

  if (TextBegin <> aText) then
    ExtractWord;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsTextBlock.SplitIntoLines(aItem: PtsLineItem);
var
  p: PtsLineItem;
begin
  if not Assigned(fCurrentFont) then
    exit;

  while Assigned(aItem) do begin
    p := aItem;
    aItem := aItem^.Next;
    p^.Next := nil;
    p^.Prev := nil;

    if not Assigned(fLastLine) then
      PushNewLine;

    case p^.ItemType of
      tsItemTypeText, tsItemTypeSpace: begin
        // increment word counter
        if (p^.ItemType = tsItemTypeSpace) then begin
          if not (tsLastItemIsSpace in fLastLine^.Flags) then
            inc(fLastLine^.SpaceCount, 1);
          Include(fLastLine^.Flags, tsLastItemIsSpace);
        end else
          Exclude(fLastLine^.Flags, tsLastItemIsSpace);

        // update and check line width
        p^.TextWidth := fCurrentFont.GetTextWidthW(p^.Text);
        if (tsBlockFlagWordWrap in fFlags) and
           (fLastLine^.Width + p^.TextWidth > fWidth) then
        begin
          fLastLine^.AutoBreak := true;
          if (fLastLine^.Width = 0) then begin
            PushLineItem(p, false); // if is first word, than add anyway
            p := nil;
          end;
          PushNewLine;
        end;

        // add item
        if Assigned(p) then begin
          PushLineItem(p);
          PushSpacing(fCurrentFont.CharSpacing);
        end;
      end;

      tsItemTypeLineBreak: begin
        PushLineItem(p);
        PushNewLine;
      end;

      tsItemTypeTab: begin
        PushLineItem(p);
      end;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsTextBlock.TrimSpaces(const aLine: PtsBlockLine);

  procedure Trim(p: PtsLineItem; const aMoveNext: Boolean);
  var
    tmp: PtsLineItem;
  begin
    while Assigned(p) do begin
      tmp := p;
      if aMoveNext then
        p := p^.Next
      else
        p := p^.Prev;

      case  tmp^.ItemType of
        tsItemTypeText: begin    //done
          break;
        end;

        tsItemTypeSpace,
        tsItemTypeSpacing: begin
          // delete item from list
          if Assigned(tmp^.Prev) then
            tmp^.Prev^.Next := tmp^.Next;
          if Assigned(tmp^.Next) then
            tmp^.Next^.Prev := tmp^.Prev;

          // update line width
          if (tmp^.ItemType = tsItemTypeSpace) then begin
            aLine^.Width := aLine^.Width - tmp^.TextWidth;
            dec(aLine^.SpaceCount, 1);
          end else
            aLine^.Width := aLine^.Width - tmp^.Spacing;

          Dispose(tmp);
        end;
      end;
    end;
  end;

begin
  if not Assigned(aLine) then
    exit;
  Trim(aLine^.First, true);
  Trim(aLine^.Last,  false);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsTextBlock.PushNewLine;
var
  p: PtsBlockLine;
begin
  TrimSpaces(fLastLine);

  new(p);
  FillByte(p^, SizeOf(p^), 0);

  if Assigned(fLastLine) then begin
    fLastLine^.Next := p;
    fLastLine       := p;
  end;

  if not Assigned(fFirstLine) then begin
    fFirstLine := p;
    fLastLine  := p;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsTextBlock.Create(const aRenderer: TtsRenderer; const aTop, aLeft, aWidth, aHeight: Integer; const aFlags: TtsBlockFlags);
begin
  inherited Create;

  fRenderer  := aRenderer;
  fTop       := aTop;
  fLeft      := aLeft;
  fWidth     := aWidth;
  fHeight    := aHeight;
  fFlags     := aFlags;
  fVertAlign := tsVertAlignTop;
  fHorzAlign := tsHorzAlignLeft;

  PushNewLine;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsTextBlock.ChangeFont(const aFont: TtsFont);
var
  p: PtsLineItem;
begin
  if not Assigned(aFont) then
    exit;

  New(p);
  FillByte(p^, SizeOf(p^), 0);
  p^.ItemType := tsItemTypeFont;
  p^.Font     := aFont;
  PushLineItem(p);

  fCurrentFont := aFont;
  if Assigned(fCurrentFont) then begin
    fCurrentFont.GetTextMetric(fTextMetric);
    if Assigned(fLastLine) then begin
      fLastLine^.Height := max(
        fLastLine^.Height,
        fTextMetric.LineHeight);
      fLastLine^.Spacing := max(
        fLastLine^.Spacing,
        fTextMetric.LineSpacing);
      fLastLine^.Ascent := max(
        fLastLine^.Ascent,
        fTextMetric.Ascent);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsTextBlock.ChangeColor(const aColor: TtsColor4f);
var
  p: PtsLineItem;
begin
  New(p);
  FillByte(p^, SizeOf(p^), 0);
  p^.ItemType := tsItemTypeColor;
  p^.Color    := aColor;
  PushLineItem(p);

  fCurrentColor := aColor;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsTextBlock.GetActualBlockHeight: Integer;
var
  line: PtsBlockLine;
begin
  result := 0;
  line   := fFirstLine;
  while Assigned(line) do begin
    result := result + line^.Height;
    line := line^.Next;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsTextBlock.TextOutA(const aText: PAnsiChar);
var
  tmp: PWideChar;
begin
  tmp := Renderer.Context.AnsiToWide(aText);
  try
    TextOutW(tmp);
  finally
    tsStrDispose(tmp);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsTextBlock.TextOutW(const aText: PWideChar);
var
  p: PtsLineItem;
begin
  p := SplitText(aText);
  SplitIntoLines(p);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsTextBlock.Destroy;
begin
  FreeLines(fFirstLine);
  fLastLine := nil;
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsRenderer///////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRenderer.BeginRender;
begin
  fCritSec.Enter;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRenderer.EndRender;
begin
  fCritSec.Leave;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsRenderer.BeginBlock(const aTop, aLeft, aWidth, aHeight: Integer; const aFlags: TtsBlockFlags): TtsTextBlock;
begin
  result := TtsTextBlock.Create(self, aTop, aLeft, aWidth, aHeight, aFlags);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRenderer.EndBlock(var aBlock: TtsTextBlock);
var
  c: PWideChar;
  x, y: Integer;
  ExtraWordSpaceTotal, ExtraWordSpaceCurrent: Single;
  rect: TtsRect;
  line: PtsBlockLine;
  item: PtsLineItem;
  font: TtsFont;
  char: TtsChar;
  metric: TtsTextMetric;
  DrawText: Boolean;

  function GetChar(const aCharCode: WideChar): TtsChar;
  begin
    result := font.GetChar(aCharCode);
    if not Assigned(result) then
      result := font.GetChar(font.DefaultChar);
  end;

  procedure DrawItem;
  begin
    case item^.ItemType of
      tsItemTypeFont: begin
        font := item^.Font;
        font.GetTextMetric(metric);
      end;

      tsItemTypeColor: begin
        SetColor(item^.Color);
      end;

      tsItemTypeText: begin
        if DrawText and Assigned(font) then begin
          c := item^.Text;
          while (c^ <> #0) do begin
            char := GetChar(c^);
            if Assigned(char) then begin
              MoveDrawPos(Char.GlyphOrigin.x, -metric.BaseLineOffset);
              Render(char.RenderRef);
              MoveDrawPos(char.Advance - char.GlyphOrigin.x + font.CharSpacing, metric.BaseLineOffset);
            end;
            inc(c);
          end;
        end;
      end;

      tsItemTypeSpace: begin
        if DrawText and Assigned(font) then begin
          c := item^.Text;
          while (c^ <> #0) do begin
            char := GetChar(c^);
            if Assigned(char) then begin
              if (font.Style * [tsStyleUnderline, tsStyleStrikeout] <> []) then begin

              end else begin

              end;
            end;
          end;
        end;
      end;

      tsItemTypeLineBreak: begin
      end;

      tsItemTypeTab: begin
      end;

      tsItemTypeSpacing: begin

      end;
    end;
  end;

  procedure DrawLine;
  begin
    // check vertical clipping
    case aBlock.Clipping of
      tsClipCharBorder, tsClipWordBorder:
        DrawText := (y + line^.Height > rect.Top) and (y < rect.Bottom);
      tsClipCharComplete, tsClipWordComplete:
        DrawText := (y > rect.Top) and (y + line^.Height < rect.Bottom);
    end;

    // check horizontal alignment
    x := rect.Left;
    ExtraWordSpaceTotal := 0;
    case aBlock.HorzAlign of
      tsHorzAlignCenter: begin
        x := rect.Left + (aBlock.Width div 2) - (line^.Width div 2);
      end;

      tsHorzAlignRight: begin
        x := rect.Right - line^.Width;
      end;

      tsHorzAlignJustify: begin
        ExtraWordSpaceTotal   := (aBlock.Width - line^.Width) / line^.SpaceCount;
        ExtraWordSpaceCurrent := ExtraWordSpaceTotal;
      end;
    end;

    if DrawText then
      SetDrawPos(x, y + line^.Ascent);
    item := line^.First;
    while Assigned(item) do begin
      DrawItem;
      item := item^.Next;
    end;
  end;

begin
  BeginRender;
  try
    // init variables
    y    := aBlock.Top;
    font := nil;
    line := aBlock.Lines;
    rect := aBlock.Rect;

    // check vertical alignment
    case aBlock.VertAlign of
      tsVertAlignCenter:
        y := y + (aBlock.Height div 2 - aBlock.GetActualBlockHeight div 2);
      tsVertAlignBottom:
        y := y + (aBlock.Height - aBlock.GetActualBlockHeight);
    end;

    while Assigned(line) do begin
      DrawLine;
      line := line^.Next;
    end;
  finally
    EndRender;
    FreeAndNil(aBlock);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsRenderer.Create(const aContext: TtsContext; const aFormat: TtsFormat);
begin
  inherited Create;
  fContext := aContext;
  fFormat  := aFormat;
  fCritSec := TCriticalSection.Create;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsRenderer.Destroy;
begin
  FreeAndNil(fCritSec);
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsContext////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsContext.AnsiToWide(const aText: PAnsiChar): PWideChar;
var
  len: Integer;
begin
  result := nil;

  if not Assigned(aText) then
    exit;
  len := Length(aText);

  // UTF-8
  if (fCodePage = tsUTF8) then begin
    result := tsStrAlloc(len);
    tsUTF8ToWide(result, len, aText, fCodePageDefault);

  // ISO 8859-1
  end else if (fCodePage = tsISO_8859_1) then begin
    result := tsStrAlloc(len);
    tsISO_8859_1ToWide(result, len, aText);

    // single or double byte CodePage
  end else if Assigned(fCodePageFunc) and Assigned(fCodePagePtr) then begin
    result := tsStrAlloc(len);
    fCodePageFunc(result, len, aText, fCodePage, fCodePageDefault);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsContext.Create;
begin
  inherited Create;

  fID := InterLockedIncrement(gLastContextID);

  fCodePage        := tsUTF8;
  fCodePageFunc    := nil;
  fCodePagePtr     := nil;
  fCodePageDefault := WideChar('?');
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsContext.Destroy;
begin
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Exceptions////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor EtsOutOfRange.Create(const aMin, aMax, aIndex: Integer);
begin
  inherited Create(Format('index (%d) is out of range (%d - %d)', [aIndex, aMin, aMax]));
end;

initialization
  Randomize;

end.

