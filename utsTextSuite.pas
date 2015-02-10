unit utsTextSuite;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, math, syncobjs,
  utsTypes, utsUtils;

type
  TtsImage = class;
  TtsFont = class;
  TtsFontGenerator = class;
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
    property Scanline[const aIndex: Integer]: Pointer read GetScanline;

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
    destructor Destroy; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsCharRenderRef = class(TObject);
  TtsChar = class(TObject)
  private
    fCharCode: WideChar;
    fGlyphOrigin: TtsPosition;
    fGlyphRect: TtsRect;
    fAdvance: Integer;
    fRenderRef: TtsCharRenderRef;
  public
    property CharCode:    WideChar         read fCharCode;
    property GlyphOrigin: TtsPosition      read fGlyphOrigin write fGlyphOrigin;
    property GlyphRect:   TtsRect          read fGlyphRect   write fGlyphRect;
    property Advance:     Integer          read fAdvance     write fAdvance;
    property RenderRef:   TtsCharRenderRef read fRenderRef   write fRenderRef;

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
    fRenderer: TtsRenderer;
    fGenerator: TtsFontGenerator;
    fProperties: TtsFontProperties;

    fCharSpacing: Integer;
    fTabWidth: Integer;
    fLineSpacing: Single;

    fChars: array[Byte] of PtsFontCharArray;
    fCreateChars: Boolean;

    function HasChar(const aCharCode: WideChar): Boolean;
    function GetChar(const aCharCode: WideChar): TtsChar;
    function GetCharCreate(const aCharCode: WideChar): TtsChar;
    procedure AddChar(const aCharCode: WideChar; const aChar: TtsChar); overload;
  protected
    constructor Create(const aRenderer: TtsRenderer; const aGenerator: TtsFontGenerator; const aProperties: TtsFontProperties);
  public
    property CreateChars: Boolean read fCreateChars write fCreateChars;
    property Char[const aCharCode: WideChar]: TtsChar read GetChar;

    property Renderer:   TtsRenderer       read fRenderer;
    property Generator:  TtsFontGenerator  read fGenerator;
    property Properties: TtsFontProperties read fProperties;

    property CharSpacing: Integer read fCharSpacing write fCharSpacing;
    property TabWidth:    Integer read fTabWidth    write fTabWidth;
    property LineSpacing: Single  read fLineSpacing write fLineSpacing;

    function AddChar(const aCharCode: WideChar): TtsChar; overload;
    procedure AddCharRange(const aCharCodeBeg, aCharCodeEnd: WideChar);
    procedure RemoveChar(const aCharCode: WideChar);
    procedure ClearChars;

    function GetTextWidthW(aText: PWideChar): Integer;
    function GetTextWidthA(aText: PAnsiChar): Integer;
    procedure GetTextMetric(out aMetric: TtsTextMetric);

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
  TtsFontGenerator = class(TObject)
  private
    fContext: TtsContext;
    fFonts: TObjectList;
    fPostProcessSteps: TObjectList;

    function GetPostProcessStepCount: Integer;
    function GetPostProcessStep(const aIndex: Integer): TtsPostProcessStep;

    procedure DrawLine(const aChar: TtsChar; const aCharImage: TtsImage; aLinePosition, aLineSize: Integer);
    procedure DoPostProcess(const aChar: TtsChar; const aCharImage: TtsImage);
  protected
    procedure RegisterFont(const aFont: TtsFont);
    procedure UnregisterFont(const aFont: TtsFont);

    function GetGlyphMetrics(const aFont: TtsFont; const aCharCode: WideChar; out aGlyphOrigin, aGlyphSize: TtsPosition; out aAdvance: Integer): Boolean; virtual; abstract;
    procedure GetCharImage(const aFont: TtsFont; const aCharCode: WideChar; const aCharImage: TtsImage); virtual; abstract;
  public
    property Context: TtsContext read fContext;
    property PostProcessStepCount: Integer read GetPostProcessStepCount;
    property PostProcessStep[const aIndex: Integer]: TtsPostProcessStep read GetPostProcessStep;

    function GenerateChar(const aCharCode: WideChar; const aFont: TtsFont; const aRenderer: TtsRenderer): TtsChar;

    function AddPostProcessStep(const aStep: TtsPostProcessStep): TtsPostProcessStep;
    function InsertPostProcessStep(const aIndex: Integer; const aStep: TtsPostProcessStep): TtsPostProcessStep;
    procedure DeletePostProcessStep(const aIndex: Integer);
    procedure ClearPostProcessSteps;

    constructor Create(const aContext: TtsContext);
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
    tsLastItemIsSpace,  // is set if the last item was a space item
    tsMetaValid,        // is set if the line meta data is valid
    tsAutoLineBreak     // is set if the linebreak was set automatically
  );
  TtsLineFlags = set of TtsLineFlag;
  PtsBlockLine = ^TtsBlockLine;
  TtsBlockLine = packed record
    Next: PtsBlockLine;
    First: PtsLineItem;
    Last: PtsLineItem;
    Flags: TtsLineFlags;

    meta: packed record
      Width: Integer;       // absolut width of this line
      Height: Integer;      // absolute height of this line
      Spacing: Integer;     // spacing between lines
      Ascent: Integer;      // text ascent
      SpaceCount: Integer;  // number of words in this line
    end;
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

    fCurrentColor: TtsColor4f;
    fCurrentFont: TtsFont;
    fFirstLine: PtsBlockLine;
    fLastLine: PtsBlockLine;

    function GetRect: TtsRect;

    function PushLineItem(const aItem: PtsLineItem; const aUpdateLineWidth: Boolean = true): Boolean;
    procedure PushSpacing(const aWidth: Integer);
    procedure FreeLineItem(var aItem: PtsLineItem);
    procedure FreeLineItems(var aItem: PtsLineItem);

    procedure FreeLines(var aItem: PtsBlockLine);

    function SplitText(aText: PWideChar): PtsLineItem;
    function SplitIntoLines(aItem: PtsLineItem): Boolean;
    procedure TrimSpaces(const aLine: PtsBlockLine);
    procedure UpdateLineMeta(const aLine: PtsBlockLine);
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
    fRenderCS: TCriticalSection;
    fBlocks: TObjectList;

    procedure RegisterBlock(const aBlock: TtsTextBlock);
    procedure UnregisterBlock(const aBlock: TtsTextBlock);
  protected
    function  CreateRenderRef(const aChar: TtsChar; const aCharImage: TtsImage): TtsCharRenderRef; virtual; abstract;
    procedure FreeRenderRef(const aCharRef: TtsCharRenderRef); virtual; abstract;

    procedure BeginRender; virtual;
    procedure EndRender; virtual;

    procedure SetDrawPos(const X, Y: Integer); virtual; abstract;
    function  GetDrawPos: TtsPosition; virtual; abstract;
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
    fCodePage: TtsCodePage;
    fCodePageDefault: WideChar;

    fRenderers: TObjectList;
    fGenerators: TObjectList;
  private
    procedure RegisterRenderer(const aRenderer: TtsRenderer);
    procedure UnregisterRenderer(const aRenderer: TtsRenderer);
    procedure RegisterGenerator(const aGenerator: TtsFontGenerator);
    procedure UnregisterGenerator(const aGenerator: TtsFontGenerator);
  public
    property CodePage:        TtsCodePage read fCodePage        write fCodePage;
    property CodePageDefault: WideChar    read fCodePageDefault write fCodePageDefault;

    function AnsiToWide(const aText: PAnsiChar): PWideChar;

    constructor Create;
    destructor Destroy; override;
  end;

  EtsException = class(Exception);
  EtsRenderer = class(EtsException);
  EtsOutOfRange = class(EtsException)
  public
    constructor Create(const aMin, aMax, aIndex: Integer);
  end;

const
  IMAGE_MODES_REPLACE: TtsImageModes = (tsModeReplace, tsModeReplace, tsModeReplace, tsModeReplace);
  IMAGE_MODES_NORMAL:  TtsImageModes = (tsModeReplace, tsModeReplace, tsModeReplace, tsModeModulate);

  COLOR_CHANNELS_RGB:  TtsColorChannels = [tsChannelRed, tsChannelGreen, tsChannelBlue];
  COLOR_CHANNELS_RGBA: TtsColorChannels = [tsChannelRed, tsChannelGreen, tsChannelBlue, tsChannelAlpha];

implementation

const
  IMAGE_MODE_FUNCTIONS: array[TtsImageMode] of TtsImageModeFunc = (
    @tsImageModeFuncIgnore,
    @tsImageModeFuncReplace,
    @tsImageModeFuncModulate);

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
  LineSize := LineSize + ((4 - (LineSize mod 4)) mod 4);
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
  ImgSize, LineSize: Integer;
begin
  LineSize := aImage.Width * tsFormatSize(aImage.Format);
  LineSize := LineSize + ((4 - (LineSize mod 4)) mod 4);
  ImgSize := LineSize * aImage.Height;
  GetMem(ImgData, ImgSize);
  if Assigned(ImgData) then
    Move(aImage.Data, ImgData, ImgSize);
  SetData(ImgData, aImage.Format, aImage.Width, aImage.Height);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsImage.CreateEmpty(const aFormat: TtsFormat; const aWidth, aHeight: Integer);
var
  ImgData: PByte;
  LineSize: Integer;
begin
  LineSize := aWidth * tsFormatSize(aFormat);
  LineSize := LineSize + ((4 - (LineSize mod 4)) mod 4);
  ImgData := AllocMem(aHeight * LineSize);
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
destructor TtsImage.Destroy;
begin
  SetData(nil);
  inherited Destroy;
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
  if (Ord(aCharCode) > 0) then begin
    Chars := fChars[(Ord(aCharCode) shr 8) and $FF];
    if Assigned(Chars) then
      result := Chars^.Chars[Ord(aCharCode) and $FF]
    else
      result := nil;
  end else
    result := nil;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFont.GetCharCreate(const aCharCode: WideChar): TtsChar;
begin
  result := GetChar(aCharCode);
  if not Assigned(result) then
    result := AddChar(aCharCode);
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
constructor TtsFont.Create(const aRenderer: TtsRenderer; const aGenerator: TtsFontGenerator; const aProperties: TtsFontProperties);
begin
  inherited Create;
  fRenderer     := aRenderer;
  fGenerator    := aGenerator;
  fProperties   := aProperties;
  fCharSpacing  := 0;
  fTabWidth     := 0;
  fLineSpacing  := 0.0;
  fCreateChars  := true;
  fGenerator.RegisterFont(self);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFont.AddChar(const aCharCode: WideChar): TtsChar;
begin
  result := GetChar(aCharCode);
  if not Assigned(result) and fCreateChars and (Ord(aCharCode) > 0) then begin
    result := fGenerator.GenerateChar(aCharCode, self, fRenderer);
    if Assigned(result) then
      AddChar(aCharCode, result);
  end;
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
    fRenderer.FreeRenderRef(c.RenderRef);
    c.RenderRef := nil;
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
          if Assigned(c.RenderRef) then
            fRenderer.FreeRenderRef(c.RenderRef);
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
    c := AddChar(aText^);
    if not Assigned(c) then
      c := AddChar(fProperties.DefaultChar);
    if Assigned(c) then begin
      if (result > 0) then
        result := result + CharSpacing;
      result := result + c.Advance;
    end;
    inc(aText);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFont.GetTextWidthA(aText: PAnsiChar): Integer;
var
  tmp: PWideChar;
begin
  tmp := fGenerator.Context.AnsiToWide(aText);
  try
    result := GetTextWidthW(tmp);
  finally
    tsStrDispose(tmp);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsFont.GetTextMetric(out aMetric: TtsTextMetric);
begin
  aMetric.Ascent            := fProperties.Ascent;
  aMetric.Descent           := fProperties.Descent;
  aMetric.ExternalLeading   := fProperties.ExternalLeading;
  aMetric.BaseLineOffset    := fProperties.BaseLineOffset;
  aMetric.CharSpacing       := CharSpacing;
  aMetric.LineHeight        := fProperties.Ascent + fProperties.Descent + fProperties.ExternalLeading;
  aMetric.LineSpacing       := Trunc(fProperties.Size * fLineSpacing);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsFont.Destroy;
begin
  fGenerator.UnregisterFont(self);
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
//TtsFontGenerator//////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFontGenerator.GetPostProcessStepCount: Integer;
begin
  result := fPostProcessSteps.Count;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFontGenerator.GetPostProcessStep(const aIndex: Integer): TtsPostProcessStep;
begin
  if (aIndex >= 0) and (aIndex < fPostProcessSteps.Count) then
    Result := TtsPostProcessStep(fPostProcessSteps[aIndex])
  else
    raise EtsOutOfRange.Create(0, fPostProcessSteps.Count-1, aIndex);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsFontGenerator.DrawLine(const aChar: TtsChar; const aCharImage: TtsImage; aLinePosition, aLineSize: Integer);
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
procedure TtsFontGenerator.DoPostProcess(const aChar: TtsChar; const aCharImage: TtsImage);
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
procedure TtsFontGenerator.RegisterFont(const aFont: TtsFont);
begin
  fFonts.Add(aFont);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsFontGenerator.UnregisterFont(const aFont: TtsFont);
begin
  if Assigned(fFonts) then
    fFonts.Remove(aFont);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFontGenerator.GenerateChar(const aCharCode: WideChar; const aFont: TtsFont; const aRenderer: TtsRenderer): TtsChar;
var
  GlyphOrigin, GlyphSize: TtsPosition;
  Advance: Integer;
  CharImage: TtsImage;
begin
  result := nil;
  if (Ord(aCharCode) = 0) or
     not GetGlyphMetrics(aFont, aCharCode, GlyphOrigin, GlyphSize, Advance) or
     not ((GlyphOrigin.x <> 0) or (GlyphOrigin.y <> 0) or (GlyphSize.x <> 0) or (GlyphSize.y <> 0) or (Advance <> 0)) then
        exit;

  CharImage := TtsImage.Create;
  try
    if aRenderer.SaveImages then begin
      if (GlyphSize.x > 0) and (GlyphSize.y > 0) then begin
        GetCharImage(aFont, aCharCode, CharImage);
      end else if ([tsStyleUnderline, tsStyleStrikeout] * aFont.Properties.Style <> []) then begin
        CharImage.CreateEmpty(aRenderer.Format, Advance, 1);
        GlyphOrigin.y := 1;
      end;
    end;

    result := TtsChar.Create(aCharCode);
    try
      result.GlyphOrigin := GlyphOrigin;
      result.GlyphRect   := tsRect(0, 0, CharImage.Width, CharImage.Height);
      result.Advance     := Advance;

      if (aRenderer.SaveImages) then begin
        try
          if (tsStyleUnderline in aFont.Properties.Style) then
            DrawLine(result, CharImage, aFont.Properties.UnderlinePos, aFont.Properties.UnderlineSize);
          if (tsStyleUnderline in aFont.Properties.Style) then
            DrawLine(result, CharImage, aFont.Properties.StrikeoutPos, aFont.Properties.StrikeoutSize);
        except
          CharImage.FillColor(tsColor4f(1, 0, 0, 0), COLOR_CHANNELS_RGB, IMAGE_MODES_NORMAL);
        end;

        DoPostProcess(result, CharImage);

        result.RenderRef := aRenderer.CreateRenderRef(result, CharImage);
      end;
    except
      FreeAndNil(result);
    end;
  finally
    FreeAndNil(CharImage);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFontGenerator.AddPostProcessStep(const aStep: TtsPostProcessStep): TtsPostProcessStep;
begin
  result := aStep;
  fPostProcessSteps.Add(aStep);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFontGenerator.InsertPostProcessStep(const aIndex: Integer; const aStep: TtsPostProcessStep): TtsPostProcessStep;
begin
  result := aStep;
  fPostProcessSteps.Insert(aIndex, aStep);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsFontGenerator.DeletePostProcessStep(const aIndex: Integer);
begin
  if (aIndex >= 0) and (aIndex < fPostProcessSteps.Count) then
    fPostProcessSteps.Delete(aIndex)
  else
    raise EtsOutOfRange.Create(0, fPostProcessSteps.Count-1, aIndex);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsFontGenerator.ClearPostProcessSteps;
begin
  fPostProcessSteps.Clear;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsFontGenerator.Create(const aContext: TtsContext);
begin
  inherited Create;
  fContext          := aContext;
  fFonts            := TObjectList.Create(false);
  fPostProcessSteps := TObjectList.Create(true);
  fContext.RegisterGenerator(self);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsFontGenerator.Destroy;
begin
  ClearPostProcessSteps;
  fContext.UnregisterGenerator(self);
  fFonts.OwnsObjects := true;
  FreeAndNil(fFonts);
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
function TtsTextBlock.PushLineItem(const aItem: PtsLineItem; const aUpdateLineWidth: Boolean): Boolean;
begin
  result := false;
  if not Assigned(fLastLine) then
    PushNewLine;

  if not Assigned(fLastLine^.First) and
     (aItem^.ItemType in [tsItemTypeSpace, tsItemTypeSpacing]) then
         exit; // di not add line space or line spacing if line is empty

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
      fLastLine^.meta.Width := fLastLine^.meta.Width + aItem^.TextWidth;
    tsItemTypeSpacing:
      fLastLine^.meta.Width := fLastLine^.meta.Width + aItem^.Spacing;
  end;
  result := true;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsTextBlock.PushSpacing(const aWidth: Integer);
var
  p: PtsLineItem;
begin
  if (aWidth <= 0) then
    exit;
  new(p);
  FillByte(p^, SizeOf(p^), 0);
  p^.ItemType := tsItemTypeSpacing;
  p^.Spacing  := aWidth;
  PushLineItem(p);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsTextBlock.FreeLineItem(var aItem: PtsLineItem);
begin
  if Assigned(aItem^.Prev) then
    aItem^.Prev^.Next := aItem^.Next;
  if Assigned(aItem^.Next) then
    aItem^.Next^.Prev := aItem^.Prev;
  case aItem^.ItemType of
    tsItemTypeText, tsItemTypeSpace:
      tsStrDispose(aItem^.Text);
  end;
  Dispose(aItem);
  aItem := nil;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsTextBlock.FreeLineItems(var aItem: PtsLineItem);
var
  p: PtsLineItem;
begin
  while Assigned(aItem) do begin
    p := aItem;
    aItem := aItem^.Next;
    FreeLineItem(p);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsTextBlock.FreeLines(var aItem: PtsBlockLine);
var
  p: PtsBlockLine;
begin
  while Assigned(aItem) do begin
    p := aItem;
    aItem := aItem^.Next;
    FreeLineItems(p^.First);
    p^.Last := nil;
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
        AddItem(p);
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

      // line breaks
      #$000D, #$000A: begin
        if (State <> tsItemTypeLineBreak) then begin
          ExtractWord;
          State := tsItemTypeLineBreak;
        end else if (TextBegin^ <> #13) or (aText^ <> #10) or (TextBegin + 1 < aText) then
          ExtractWord;
      end;

      // spaces
      #$0020: begin
        if (State <> tsItemTypeSpace) then
          ExtractWord;
        State := tsItemTypeSpace;
      end;

      // tabulator
      #$0009: begin
        if (State <> tsItemTypeTab) then
          ExtractWord;
        State := tsItemTypeTab;
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
function TtsTextBlock.SplitIntoLines(aItem: PtsLineItem): Boolean;
var
  p: PtsLineItem;
begin
  result := false;
  if not Assigned(fCurrentFont) then
    exit;

  result := true;
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
            inc(fLastLine^.meta.SpaceCount, 1);
          Include(fLastLine^.Flags, tsLastItemIsSpace);
        end else
          Exclude(fLastLine^.Flags, tsLastItemIsSpace);

        // update and check line width
        p^.TextWidth := fCurrentFont.GetTextWidthW(p^.Text);
        if (tsBlockFlagWordWrap in fFlags) and
           (fLastLine^.meta.Width + p^.TextWidth > fWidth) then
        begin
          if (fLastLine^.meta.Width = 0) then begin
            if not PushLineItem(p, false) then // if is first word, than add anyway
              FreeLineItem(p);
            p := nil;
          end;
          include(fLastLine^.Flags, tsAutoLineBreak);
          PushNewLine;
        end;

        // add item
        if Assigned(p) then begin
          if not PushLineItem(p) then
            FreeLineItem(p);
          PushSpacing(fCurrentFont.CharSpacing);
        end;
      end;

      tsItemTypeLineBreak: begin
        if not PushLineItem(p) then
          FreeLineItem(p);
        PushNewLine;
      end;

      tsItemTypeTab: begin
        if not PushLineItem(p) then
          FreeLineItem(p);
      end;

    else
      raise EtsException.Create('unexpected line item');
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsTextBlock.TrimSpaces(const aLine: PtsBlockLine);

  procedure Trim(var aItem: PtsLineItem; const aMoveNext: Boolean);
  var
    tmp, p: PtsLineItem;
    IsFirst: Boolean;
  begin
    IsFirst := true;
    p       := aItem;
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
          // update line meta
          if (tmp^.ItemType = tsItemTypeSpace) then begin
            aLine^.meta.Width := aLine^.meta.Width - tmp^.TextWidth;
            dec(aLine^.meta.SpaceCount, 1);
          end else
            aLine^.meta.Width := aLine^.meta.Width - tmp^.Spacing;

          FreeLineItem(tmp);
          if IsFirst then
            aItem := p;
        end;

      else
        IsFirst := false;
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
procedure TtsTextBlock.UpdateLineMeta(const aLine: PtsBlockLine);
var
  metric: TtsTextMetric;
begin
  if not Assigned(fCurrentFont) or
     not Assigned(aLine) then
        exit;

  fCurrentFont.GetTextMetric(metric);
  if (tsMetaValid in aLine^.Flags) then begin
    aLine^.meta.Height := max(
      aLine^.meta.Height,
      metric.LineHeight);
    aLine^.meta.Spacing := max(
      aLine^.meta.Spacing,
      metric.LineSpacing);
    aLine^.meta.Ascent := max(
      aLine^.meta.Ascent,
      metric.Ascent);
  end else begin
    Include(aLine^.Flags, tsMetaValid);
    aLine^.meta.Height  := metric.LineHeight;
    aLine^.meta.Spacing := metric.LineSpacing;
    aLine^.meta.Ascent  := metric.Ascent;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsTextBlock.PushNewLine;
var
  p: PtsBlockLine;
begin
  TrimSpaces(fLastLine);

  new(p);
  FillByte(p^, SizeOf(p^), 0);
  UpdateLineMeta(p);

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

  fRenderer.RegisterBlock(self);
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
  fCurrentFont := aFont;
  p^.ItemType  := tsItemTypeFont;
  p^.Font      := fCurrentFont;
  PushLineItem(p);
  UpdateLineMeta(fLastLine);
  fRenderer.UnregisterBlock(self);
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
    result := result + line^.meta.Height;
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
  if not SplitIntoLines(p) then
    FreeLineItems(p);
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
procedure TtsRenderer.RegisterBlock(const aBlock: TtsTextBlock);
begin
  fBlocks.Add(aBlock);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRenderer.UnregisterBlock(const aBlock: TtsTextBlock);
begin
  if Assigned(fBlocks) then
    fBlocks.Remove(aBlock);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRenderer.BeginRender;
begin
  fRenderCS.Enter;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRenderer.EndRender;
begin
  fRenderCS.Leave;
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
  pos: TtsPosition;
  x, y, tmp, tab: Integer;
  ExtraSpaceTotal, ExtraSpaceActual: Single;
  rect: TtsRect;
  line: PtsBlockLine;
  item: PtsLineItem;
  font: TtsFont;
  char: TtsChar;
  metric: TtsTextMetric;
  DrawText: Boolean;

  function GetChar(const aCharCode: WideChar): TtsChar;
  begin
    result := font.AddChar(aCharCode);
    if not Assigned(result) then
      result := font.AddChar(font.Properties.DefaultChar);
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
          ExtraSpaceActual := ExtraSpaceActual + ExtraSpaceTotal;
          c := item^.Text;
          while (c^ <> #0) do begin
            char := GetChar(c^);
            if Assigned(char) then begin
              if (font.Properties.Style * [tsStyleUnderline, tsStyleStrikeout] <> []) then begin
                MoveDrawPos(char.GlyphOrigin.x, -metric.BaseLineOffset);
                Render(char.RenderRef);
                MoveDrawPos(char.Advance - char.GlyphOrigin.x + font.CharSpacing, metric.BaseLineOffset);
              end else begin
                MoveDrawPos(char.Advance + font.CharSpacing, 0);
              end;
            end;
            inc(c);
          end;

          tmp := Trunc(ExtraSpaceActual);
          ExtraSpaceActual := ExtraSpaceActual - tmp;
          if (font.Properties.Style * [tsStyleUnderline, tsStyleStrikeout] <> []) then begin
            // TODO draw lines; maybe with a temporary created fake char or something like an empty char?
          end;
          MoveDrawPos(tmp, 0);
        end;
      end;

      tsItemTypeLineBreak: begin
        // because this should be the last item in a line, we have nothing to do here
      end;

      tsItemTypeTab: begin
        // get current x pos and round it to TabWidth
        pos := GetDrawPos;
        tab := font.TabWidth * font.Properties.Size;
        pos.x := Ceil(pos.x * tab) div tab;
        SetDrawPos(pos.x, pos.y);
      end;

      tsItemTypeSpacing: begin
        MoveDrawPos(item^.Spacing, 0);
      end;
    end;
  end;

  procedure DrawLine;
  begin
    // check vertical clipping
    case aBlock.Clipping of
      tsClipCharBorder, tsClipWordBorder:
        DrawText := (y + line^.meta.Height > rect.Top) and (y < rect.Bottom);
      tsClipCharComplete, tsClipWordComplete:
        DrawText := (y > rect.Top) and (y + line^.meta.Height < rect.Bottom);
    end;

    // check horizontal alignment
    x := rect.Left;
    ExtraSpaceTotal  := 0;
    ExtraSpaceActual := 0;
    case aBlock.HorzAlign of
      tsHorzAlignCenter:
        x := rect.Left + (aBlock.Width div 2) - (line^.meta.Width div 2);
      tsHorzAlignRight:
        x := rect.Right - line^.meta.Width;
      tsHorzAlignJustify:
        if (tsAutoLineBreak in line^.Flags) then
          ExtraSpaceTotal := (aBlock.Width - line^.meta.Width) / line^.meta.SpaceCount;
    end;

    if DrawText then
      SetDrawPos(x, y + line^.meta.Ascent);
    inc(y, line^.meta.Height + line^.meta.Spacing);
    item := line^.First;
    while Assigned(item) do begin
      DrawItem;
      item := item^.Next;
    end;
  end;

begin
  if (aBlock.Renderer <> self) then
    EtsException.Create('text block was created by other renderer');

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
  fContext    := aContext;
  fFormat     := aFormat;
  fSaveImages := true;
  fBlocks     := TObjectList.Create(false);
  fRenderCS   := TCriticalSection.Create;
  fContext.RegisterRenderer(self);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsRenderer.Destroy;
begin
  fContext.UnregisterRenderer(self);
  fBlocks.OwnsObjects := true;
  FreeAndNil(fBlocks);
  FreeAndNil(fRenderCS);
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsContext////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsContext.RegisterRenderer(const aRenderer: TtsRenderer);
begin
  fRenderers.Add(aRenderer);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsContext.UnregisterRenderer(const aRenderer: TtsRenderer);
begin
  if Assigned(fRenderers) then
    fRenderers.Remove(aRenderer);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsContext.RegisterGenerator(const aGenerator: TtsFontGenerator);
begin
  fGenerators.Add(aGenerator);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsContext.UnregisterGenerator(const aGenerator: TtsFontGenerator);
begin
  if Assigned(fGenerators) then
    fGenerators.Remove(aGenerator);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsContext.AnsiToWide(const aText: PAnsiChar): PWideChar;
var
  len: Integer;
begin
  result := nil;
  if not Assigned(aText) then
    exit;
  len := Length(aText);
  tsAnsiToWide(result, len, aText, fCodePage, fCodePageDefault);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsContext.Create;
begin
  inherited Create;

  fCodePage        := tsUTF8;
  fCodePageDefault := WideChar('?');

  fRenderers  := TObjectList.Create(false);
  fGenerators := TObjectList.Create(false);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsContext.Destroy;
begin
  fGenerators.OwnsObjects := true;
  fRenderers.OwnsObjects  := true;
  FreeAndNil(fGenerators);
  FreeAndNil(fRenderers);
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

