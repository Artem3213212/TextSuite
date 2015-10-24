unit utsCharCache;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  utsChar, utsFont, utsUtils, utsContext, utsTypes, utsImage;

type
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsCharArray = packed record
    Chars: array [Byte] of TtsChar;
    Count: Byte;
  end;
  PtsCharArray = ^TtsCharArray;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsRenderRefGenerator = class(TtsRefManager)
  private
    fContext: TtsContext;
    fFormat: TtsFormat;
  public
    property Context: TtsContext read fContext;
    property Format:  TtsFormat  read fFormat;

    function  CreateRenderRef(const aChar: TtsChar; const aImage: TtsImage): TtsRenderRef; virtual; abstract;
    procedure FreeRenderRef(const aRenderRef: TtsRenderRef); virtual; abstract;

    constructor Create(const aContext: TtsContext; const aFormat: TtsFormat);
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsChars = class(TObject)
  private
    fRenderRefGenerator: TtsRenderRefGenerator;
    fFont: TtsFont;
    fCanCreate: Boolean;
    fChars: array[Byte] of PtsCharArray;
    function GenerateChar(const aCharCode: WideChar): TtsChar;
  public
    function GetChar(const aCharCode: WideChar): TtsChar;
    function AddChar(const aCharCode: WideChar): TtsChar;
    procedure DelChar(const aCharCode: WideChar);
    procedure AddCharRange(const aStart, aStop: WideChar);
    procedure DelCharRange(const aStart, aStop: WideChar);
    procedure Clear;
  public
    property CanCreate: Boolean read fCanCreate write fCanCreate;
    property Char[const aCharCode: WideChar]: TtsChar read GetChar;

    function GetTextWidthW(aText: PWideChar): Integer;
    function GetTextWidthA(aText: PAnsiChar): Integer;

    constructor Create(const aRenderRefGen: TtsRenderRefGenerator; const aFont: TtsFont);
    destructor Destroy; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  PtsCharCacheItem = ^TtsCharCacheItem;
  TtsCharCacheItem = packed record
    key: TtsFont;
    val: TtsChars;
  end;

  TtsCharCache = class(TtsRefManager)
  private
    fRenderRefGenerator: TtsRenderRefGenerator;
    fItems: TList;
    function GetChars(const aKey: TtsFont): TtsChars;
    function Find(const aMin, aMax: Integer; const aKey: TtsFont; out aIndex: Integer): Integer;
  protected
    procedure DelSlave(const aSlave: TtsRefManager); override;
  public
    property Chars[const aKey: TtsFont]: TtsChars read GetChars;

    procedure Clear;

    constructor Create(const aRenderRefGen: TtsRenderRefGenerator);
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  utsConstants;

type
  TtsWritableChar = class(TtsChar)
  public
    property RenderRef: TtsRenderRef read fRenderRef write fRenderRef;
  end;


{$IFNDEF fpc}
  {$IFDEF WIN64}
  PtrUInt   = System.UInt64;
  {$ELSE}
  PtrUInt = Cardinal;
  {$ENDIF}
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsChars//////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsChars.GenerateChar(const aCharCode: WideChar): TtsChar;
var
  GlyphSize: TtsPosition;
  CharImage: TtsImage;
  m: TtsGlyphMetric;
  c: TtsWritableChar;

  procedure FillLine(aData: PByte);
  var
    w, i: Integer;
    c: TtsColor4f;
    tmp: PByte;
  begin
    w := CharImage.Width;
    while (w > 0) do begin
      tmp := aData;
      tsFormatUnmap(CharImage.Format, tmp, c);
      for i := 0 to 3 do
        c.arr[i] := 1.0;
      tsFormatMap(CharImage.Format, aData, c);
      dec(w);
    end;
  end;

  procedure DrawLine(aLinePosition, aLineSize: Integer);
  var
    ImgSize, ImgPos, Origin: TtsPosition;
    Rect: TtsRect;
    YOffset, y: Integer;
  begin
    if aLineSize <= 0 then
      exit;

    aLinePosition := aLinePosition - aLineSize;

    // calculate width and height
    ImgPos  := tsPosition(0, 0);
    ImgSize := tsPosition(CharImage.Width, CharImage.Height);
    Origin  := m.GlyphOrigin;
    Rect    := m.GlyphRect;

    // expand left rect border to origin
    if (Origin.x > 0) then begin
      dec(Rect.Left, Origin.x);
      Origin.x := 0;
    end;

    // expand right rect border to advanced
    if (Rect.Right - Rect.Left < m.Advance) then begin
      Rect.Right := Rect.Left + m.Advance;
    end;

    // expand bottom rect border
    if (Origin.y - aLinePosition > Rect.Bottom) then begin
      Rect.Bottom := Origin.y - aLinePosition;
    end;

    // expand top rect border
    if (Origin.y - aLinePosition - aLineSize < Rect.Top) then begin
      Rect.Top := Origin.y - aLinePosition - aLineSize;
      Origin.y := aLinePosition + aLineSize;
    end;

    // update image size
    if (Rect.Right - Rect.Left > ImgSize.x) then begin
      ImgSize.x := Rect.Right - Rect.Left;
      ImgPos.x  := Max(-Rect.Left, 0);
      inc(Rect.Left,  ImgPos.x);
      inc(Rect.Right, ImgPos.x);
    end;
    if (Rect.Bottom - Rect.Top > ImgSize.y) then begin
      ImgSize.y := Rect.Bottom - Rect.Top;
      ImgPos.y  := Max(-Rect.Top, 0);
      inc(Rect.Top,    ImgPos.y);
      inc(Rect.Bottom, ImgPos.y);
    end;
    CharImage.Resize(ImgSize.x, ImgSize.y, ImgPos.x, ImgPos.y);

    // draw lines
    YOffset := Rect.Top + Origin.y - aLinePosition;
    for y := 1 to aLineSize do
      FillLine(CharImage.ScanLine[YOffset - y]);

    // move glyph rect
    m.GlyphOrigin := Origin;
    m.GlyphRect   := Rect;
  end;

begin
  result := nil;
  if (aCharCode <> #0) and
     (not fFont.GetGlyphMetrics(aCharCode, m.GlyphOrigin, GlyphSize, m.Advance) or
      not ((m.GlyphOrigin.x <> 0) or
           (m.GlyphOrigin.y <> 0) or
           (GlyphSize.x <> 0) or
           (GlyphSize.y <> 0) or
           (m.Advance <> 0))) then
        exit;

  CharImage := TtsImage.Create(nil);
  try
    if (aCharCode = #0) then begin
      CharImage.CreateEmpty(fRenderRefGenerator.Format, 3, 1);
      m.GlyphOrigin := tsPosition(0, 1);
      m.Advance     := 1;
    end else if (GlyphSize.x > 0) and (GlyphSize.y > 0) then
      fFont.GetCharImage(aCharCode, CharImage, fRenderRefGenerator.Format);

    if CharImage.IsEmpty and ([tsStyleUnderline, tsStyleStrikeout] * fFont.Metric.Style <> []) then begin
      CharImage.CreateEmpty(fRenderRefGenerator.Format, max(m.Advance, 1), 1);
      m.GlyphOrigin.y := 1;
    end;

    c := TtsWritableChar.Create(aCharCode);
    try
      if (aCharCode = #0)
        then m.GlyphRect := tsRect(1, 0, 2, 1)
        else m.GlyphRect := tsRect(0, 0, CharImage.Width, CharImage.Height);

      try
        if (tsStyleUnderline in fFont.Metric.Style) then
          DrawLine(fFont.Metric.UnderlinePos, fFont.Metric.UnderlineSize);
        if (tsStyleStrikeout in fFont.Metric.Style) then
          DrawLine(fFont.Metric.StrikeoutPos, fFont.Metric.StrikeoutSize);
      except
        CharImage.FillColor(tsColor4f(1, 0, 0, 0), TS_COLOR_CHANNELS_RGB, TS_IMAGE_MODES_MODULATE_ALPHA);
      end;

      c.GlyphMetric := m;
      if Assigned(fFont.PostProcessor) then
        fFont.PostProcessor.Execute(c, CharImage);
      c.RenderRef := fRenderRefGenerator.CreateRenderRef(c, CharImage);
      result := c;
    except
      FreeAndNil(c);
    end;
  finally
    FreeAndNil(CharImage);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsChars.GetChar(const aCharCode: WideChar): TtsChar;
var
  arr: PtsCharArray;
begin
  arr := fChars[(Ord(aCharCode) shr 8) and $FF];
  if Assigned(arr) then
    result := arr^.Chars[Ord(aCharCode) and $FF]
  else
    result := nil;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsChars.AddChar(const aCharCode: WideChar): TtsChar;
var
  h, l: Integer;
  arr: PtsCharArray;
begin
  result := GetChar(aCharCode);
  if not Assigned(result) and fCanCreate then begin
    result := GenerateChar(aCharCode);
    if Assigned(result) then begin
      h := (Ord(aCharCode) shr 8) and $FF;
      arr := fChars[h];
      if not Assigned(arr) then begin
        New(arr);
        FillChar(arr^, SizeOf(arr^), 0);
        fChars[h] := arr;
      end;
      l := Ord(aCharCode) and $FF;
      arr^.Chars[l] := result;
      inc(arr^.Count);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsChars.DelChar(const aCharCode: WideChar);
var
  h, l: Integer;
  c: TtsChar;
  arr: PtsCharArray;
begin
  // find char array
  h := (Ord(aCharCode) shr 8) and $FF;
  arr := fChars[h];
  if not Assigned(arr) then
    exit;

  // find char
  l := Ord(aCharCode) and $FF;
  c := arr^.Chars[l];
  if not Assigned(c) then
    exit;

  // remove char
  arr^.Chars[l] := nil;
  dec(arr^.Count);
  if (arr^.Count <= 0) then begin
    fChars[h] := nil;
    Dispose(arr);
  end;

  if Assigned(c.RenderRef) then
    fRenderRefGenerator.FreeRenderRef(c.RenderRef);
  FreeAndNil(c);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsChars.AddCharRange(const aStart, aStop: WideChar);
var
  c: WideChar;
begin
  for c := aStart to aStop do
    AddChar(c);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsChars.DelCharRange(const aStart, aStop: WideChar);
var
  c: WideChar;
begin
  for c := aStart to aStop do
    DelChar(c);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsChars.Clear;
var
  h, l: Integer;
  c: TtsChar;
  arr: PtsCharArray;
begin
  for h := Low(fChars) to High(fChars) do begin
    arr := fChars[h];
    if Assigned(arr) then begin
      for l := Low(arr^.Chars) to High(arr^.Chars) do begin
        c := arr^.Chars[l];
        if Assigned(c) then begin
          if Assigned(c.RenderRef) then
            fRenderRefGenerator.FreeRenderRef(c.RenderRef);
          FreeAndNil(c);
        end;
      end;
      Dispose(arr);
      fChars[h] := nil;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsChars.GetTextWidthW(aText: PWideChar): Integer;
var
  c: TtsChar;
begin
  result := 0;
  if not Assigned(aText) then
    exit;

  while (aText^ <> #0) do begin
    c := AddChar(aText^);
    if not Assigned(c) then
      c := AddChar(fRenderRefGenerator.Context.DefaultChar);
    if Assigned(c) then begin
      if (result > 0) then
        result := result + fFont.CharSpacing;
      result := result + c.GlyphMetric.Advance;
    end;
    inc(aText);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsChars.GetTextWidthA(aText: PAnsiChar): Integer;
var
  tmp: PWideChar;
begin
  tmp := fRenderRefGenerator.Context.AnsiToWide(aText);
  try
    result := GetTextWidthW(tmp);
  finally
    tsStrDispose(tmp);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsChars.Create(const aRenderRefGen: TtsRenderRefGenerator; const aFont: TtsFont);
begin
  inherited Create;
  fRenderRefGenerator := aRenderRefGen;
  fFont               := aFont;
  fCanCreate          := true;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsChars.Destroy;
begin
  Clear;
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsRenderRefGenerator/////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsRenderRefGenerator.Create(const aContext: TtsContext; const aFormat: TtsFormat);
begin
  inherited Create(aContext);
  fContext := aContext;
  fFormat  := aFormat;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsCharCache//////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsCharCache.GetChars(const aKey: TtsFont): TtsChars;
var
  pos, index: Integer;
  p: PtsCharCacheItem;
begin
  pos := Find(0, fItems.Count-1, aKey, index);
  if (pos < 0) then begin
    result := TtsChars.Create(fRenderRefGenerator, aKey);
    aKey.AddMaster(self);
    new(p);
    p^.key := aKey;
    p^.val := result;
    fItems.Insert(index, p);
  end else
    result := PtsCharCacheItem(fItems[pos])^.val;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsCharCache.Find(const aMin, aMax: Integer; const aKey: TtsFont; out aIndex: Integer): Integer;
var
  i: Integer;
begin
  if (aMin <= aMax) then begin
    i := aMin + Trunc((aMax - aMin) / 2);
    if (aKey = PtsCharCacheItem(fItems[i])^.key) then
      result := i
    else if (PtrUInt(aKey) < PtrUInt(PtsCharCacheItem(fItems[i])^.key)) then
      result := Find(aMin, i-1, aKey, aIndex)
    else
      result := Find(i+1, aMax, aKey, aIndex);
  end else begin
    result := -1;
    aIndex := aMin;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsCharCache.DelSlave(const aSlave: TtsRefManager);
var
  pos, index: Integer;
  p: PtsCharCacheItem;
begin
  pos := Find(0, fItems.Count-1, aSlave as TtsFont, index);
  if (pos >= 0) then begin
    p := PtsCharCacheItem(fItems[pos]);
    fItems.Delete(pos);
    FreeAndNil(p^.val);
    Dispose(p);
  end;
  inherited DelSlave(aSlave);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsCharCache.Clear;
var
  p: PtsCharCacheItem;
  i: Integer;
begin
  for i := 0 to fItems.Count-1 do begin
    p := PtsCharCacheItem(fItems[i]);
    FreeAndNil(p^.val);
    Dispose(p);
  end;
  fItems.Clear;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsCharCache.Create(const aRenderRefGen: TtsRenderRefGenerator);
begin
  inherited Create(aRenderRefGen);
  fRenderRefGenerator := aRenderRefGen;
  fItems              := TList.Create;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsCharCache.Destroy;
begin
  Clear;
  FreeAndNil(fItems);
  inherited Destroy;
end;

end.

