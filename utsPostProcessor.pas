unit utsPostProcessor;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, contnrs,
  utsUtils, utsChar, utsImage, utsContext, utsTypes;

type
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsCharRange = record
    Start: WideChar;
    Stop:  WideChar;
  end;
  PtsCharRange = ^TtsCharRange;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsCharRangeUsage = (
    tsUsageInclude,
    tsUsageExclude);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsPostProcessor = class(TtsRefManager)
  private
    fContext: TtsContext;
    fIncludeCharRanges: TList;
    fExcludeCharRanges: TList;

    procedure ClearList(const aList: TList);
  public
    property Context: TtsContext read fContext;

    function IsInRange(const aCharCode: WideChar): Boolean;

    procedure AddRange(const aUsage: TtsCharRangeUsage; const aStart, aStop: WideChar);
    procedure AddChars(const aUsage: TtsCharRangeUsage; aChars: PWideChar);
    procedure ClearRanges;

    function Execute(const aChar: TtsChar; const aImage: TtsImage): Boolean; virtual;

    constructor Create(const aContext: TtsContext);
    destructor Destroy; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsPostProcessorList = class(TtsPostProcessor)
  private
    fItems: TObjectList;

    function GetCount: Integer;
    function GetItem(const aIndex: Integer): TtsPostProcessor;
    function GetOwnsObjects: Boolean;
    procedure SetOwnsObjects(const aValue: Boolean);
  public
    property Count:       Integer read GetCount;
    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;

    property Items[const aIndex: Integer]: TtsPostProcessor read GetItem;

    procedure Add(const aPostProcessor: TtsPostProcessor);
    procedure Delete(const aIndex: Integer);
    procedure Clear;

    function Remove(const aPostProcessor: TtsPostProcessor): Integer;
    function IndexOf(const aPostProcessor: TtsPostProcessor): Integer;

    function Execute(const aChar: TtsChar; const aImage: TtsImage): Boolean; override;

    constructor Create(const aContext: TtsContext; const aOwnsObjects: Boolean);
    destructor Destroy; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsPostProcessorFillColor = class(TtsPostProcessor)
  private
    fColor: TtsColor4f;
    fModes: TtsImageModes;
    fChannels: TtsColorChannels;
  public
    property Color:     TtsColor4f       read fColor    write fColor;
    property Modes:     TtsImageModes    read fModes    write fModes;
    property Channels:  TtsColorChannels read fChannels write fChannels;

    function Execute(const aChar: TtsChar; const aImage: TtsImage): Boolean; override;

    constructor Create(
      const aContext: TtsContext;
      const aColor: TtsColor4f;
      const aModes: TtsImageModes;
      const aChannels: TtsColorChannels);
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsPostProcessorFillPattern = class(TtsPostProcessor)
  private
    fPattern: TtsImage;
    fOwnsPattern: Boolean;
    fPosition: TtsPosition;
    fModes: TtsImageModes;
    fChannels: TtsColorChannels;
    procedure SetPattern(aValue: TtsImage);
  public
    property Pattern:     TtsImage         read fPattern     write SetPattern;
    property OwnsPattern: Boolean          read fOwnsPattern write fOwnsPattern;
    property Position:    TtsPosition      read fPosition    write fPosition;
    property Modes:       TtsImageModes    read fModes       write fModes;
    property Channels:    TtsColorChannels read fChannels    write fChannels;

    function Execute(const aChar: TtsChar; const aImage: TtsImage): Boolean; override;

    constructor Create(
      const aContext: TtsContext;
      const aPattern: TtsImage;
      const aOwnsPattern: Boolean;
      const aPosition: TtsPosition;
      const aModes: TtsImageModes;
      const aChannels: TtsColorChannels);
    destructor Destroy; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsPostProcessorBorder = class(TtsPostProcessor)
  private
    fKernel: TtsKernel2D;
    fKeepSize: Boolean;
    fColor: TtsColor4f;
    fStrength: Single;
    fWidth: Single;
    procedure SetStrength(const aValue: Single);
    procedure SetWidth(const aValue: Single);
  public
    property Width:    Single     read fWidth    write SetWidth;
    property Strength: Single     read fStrength write SetStrength;
    property Color:    TtsColor4f read fColor    write fColor;
    property KeepSize: Boolean    read fKeepSize write fKeepSize;

    function Execute(const aChar: TtsChar; const aImage: TtsImage): Boolean; override;

    constructor Create(
      const aContext: TtsContext;
      const aWidth, aStrength: Single;
      const aColor: TtsColor4f;
      const aKeepSize: Boolean);
    destructor Destroy; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsPostProcessorShadow = class(TtsPostProcessor)
  private
    fKernel: TtsKernel1D;
    fColor: TtsColor4f;
    fOffset: TtsPosition;
    fRadius: Single;
    fStrength: Single;
    procedure SetRadius(const aValue: Single);
    procedure SetStrength(const aValue: Single);
  public
    property Radius:   Single      read fRadius   write SetRadius;
    property Strength: Single      read fStrength write SetStrength;
    property Offset:   TtsPosition read fOffset   write fOffset;
    property Color:    TtsColor4f  read fColor    write fColor;

    function Execute(const aChar: TtsChar; const aImage: TtsImage): Boolean; override;

    constructor Create(
      const aContext: TtsContext;
      const aRadius, aStrength: Single;
      const aOffset: TtsPosition;
      const aColor: TtsColor4f);
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  utsConstants;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsPostProcessor//////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsPostProcessor.ClearList(const aList: TList);
var
  i: Integer;
  p: PtsCharRange;
begin
  for i := 0 to aList.Count-1 do begin
    p := aList[i];
    Dispose(p);
  end;
  aList.Clear;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsPostProcessor.IsInRange(const aCharCode: WideChar): Boolean;
var
  i: Integer;
  p: PtsCharRange;
begin
  result := (fIncludeCharRanges.Count = 0);

  if not result then for i := 0 to fIncludeCharRanges.Count-1 do begin
    p := fIncludeCharRanges[i];
    if (aCharCode >= p^.Start) and (aCharCode <= p^.Stop) then begin
      result := true;
      break;
    end;
  end;

  if result then for i := 0 to fExcludeCharRanges.Count-1 do begin
    p := fExcludeCharRanges[i];
    if (aCharCode >= p^.Start) and (aCharCode <= p^.Stop) then begin
      result := false;
      break;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsPostProcessor.AddRange(const aUsage: TtsCharRangeUsage; const aStart, aStop: WideChar);
var
  p: PtsCharRange;
begin
  new(p);
  p^.Start := aStart;
  p^.Stop  := aStop;
  case aUsage of
    tsUsageInclude: fIncludeCharRanges.Add(p);
    tsUsageExclude: fExcludeCharRanges.Add(p);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsPostProcessor.AddChars(const aUsage: TtsCharRangeUsage; aChars: PWideChar);
begin
  if not Assigned(aChars) then
    exit;
  while (aChars^ <> #0) do begin
    AddRange(aUsage, aChars^, aChars^);
    inc(aChars);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsPostProcessor.ClearRanges;
begin
  ClearList(fIncludeCharRanges);
  ClearList(fExcludeCharRanges);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsPostProcessor.Execute(const aChar: TtsChar; const aImage: TtsImage): Boolean;
begin
  result := IsInRange(aChar.CharCode);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsPostProcessor.Create(const aContext: TtsContext);
begin
  inherited Create(aContext);
  fIncludeCharRanges := TList.Create;
  fExcludeCharRanges := TList.Create;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsPostProcessor.Destroy;
begin
  ClearRanges;
  FreeAndNil(fIncludeCharRanges);
  FreeAndNil(fExcludeCharRanges);
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsPostProcessorList//////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsPostProcessorList.GetCount: Integer;
begin
  result := fItems.Count;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsPostProcessorList.GetItem(const aIndex: Integer): TtsPostProcessor;
begin
  result := TtsPostProcessor(fItems[aIndex]);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsPostProcessorList.GetOwnsObjects: Boolean;
begin
  result := fItems.OwnsObjects;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsPostProcessorList.SetOwnsObjects(const aValue: Boolean);
begin
  fItems.OwnsObjects := aValue;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsPostProcessorList.Add(const aPostProcessor: TtsPostProcessor);
begin
  fItems.Add(aPostProcessor);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsPostProcessorList.Delete(const aIndex: Integer);
begin
  fItems.Delete(aIndex);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsPostProcessorList.Clear;
begin
  fItems.Clear;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsPostProcessorList.Remove(const aPostProcessor: TtsPostProcessor): Integer;
begin
  result := fItems.IndexOf(aPostProcessor);
  if (result >= 0) then
    fItems.Delete(result);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsPostProcessorList.IndexOf(const aPostProcessor: TtsPostProcessor): Integer;
begin
  result := fItems.IndexOf(aPostProcessor);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsPostProcessorList.Execute(const aChar: TtsChar; const aImage: TtsImage): Boolean;
var
  i: Integer;
begin
  result := inherited Execute(aChar, aImage);
  if result then
    for i := 0 to fItems.Count-1 do
      (fItems[i] as TtsPostProcessor).Execute(aChar, aImage);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsPostProcessorList.Create(const aContext: TtsContext; const aOwnsObjects: Boolean);
begin
  inherited Create(aContext);
  fItems := TObjectList.Create(aOwnsObjects);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsPostProcessorList.Destroy;
begin
  FreeAndNil(fItems);
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsPostProcessorFillColor/////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsPostProcessorFillColor.Execute(const aChar: TtsChar; const aImage: TtsImage): Boolean;
begin
  result := inherited Execute(aChar, aImage);
  if result and Assigned(aImage) then
    aImage.FillColor(fColor, fChannels, fModes);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsPostProcessorFillColor.Create(const aContext: TtsContext; const aColor: TtsColor4f;
  const aModes: TtsImageModes; const aChannels: TtsColorChannels);
begin
  inherited Create(aContext);
  fColor    := aColor;
  fModes    := aModes;
  fChannels := aChannels;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsPostProcessorFillPattern///////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsPostProcessorFillPattern.SetPattern(aValue: TtsImage);
begin
  if (fPattern = aValue) then
    exit;
  if fOwnsPattern then
    FreeAndNil(fPattern);
  fPattern := aValue;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsPostProcessorFillPattern.Execute(const aChar: TtsChar; const aImage: TtsImage): Boolean;
begin
  result := inherited Execute(aChar, aImage);
  if result and Assigned(aImage) then
    aImage.FillPattern(fPattern, fPosition.x, fPosition.y, fChannels, fModes);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsPostProcessorFillPattern.Create(const aContext: TtsContext; const aPattern: TtsImage;
  const aOwnsPattern: Boolean; const aPosition: TtsPosition; const aModes: TtsImageModes; const aChannels: TtsColorChannels);
begin
  inherited Create(aContext);
  fPattern     := aPattern;
  fOwnsPattern := aOwnsPattern;
  fPosition    := aPosition;
  fModes       := aModes;
  fChannels    := aChannels;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsPostProcessorFillPattern.Destroy;
begin
  if fOwnsPattern then
    FreeAndNil(fPattern);
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsPostProcessorBorder////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsPostProcessorBorder.SetStrength(const aValue: Single);
begin
  fStrength := aValue;
  FreeAndNil(fKernel);
  fKernel := TtsKernel2D.Create(fWidth, fStrength);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsPostProcessorBorder.SetWidth(const aValue: Single);
begin
  fWidth := aValue;
  FreeAndNil(fKernel);
  fKernel := TtsKernel2D.Create(fWidth, fStrength);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsPostProcessorBorder.Execute(const aChar: TtsChar; const aImage: TtsImage): Boolean;
var
  orig: TtsImage;
  x, y: Integer;
  dst: PByte;
  m: TtsGlyphMetric;

  function BorderLookup: TtsColor4f;
  var
    i: Integer;
    c: TtsColor4f;
    s: Single;
    chan: TtsColorChannel;
    mask: TtsColorChannels;
    tmpX, tmpY: Integer;
  begin
    mask   := TS_COLOR_CHANNELS_RGBA;
    result := tsColor4f(0, 0, 0, 0);
    for i := 0 to fKernel.ItemCount-1 do begin
      tmpX := x + fKernel.Items[i].OffsetX;
      tmpY := y + fKernel.Items[i].OffsetY;
      if (tmpX >= 0) and (tmpX < orig.Width) and
         (tmpY >= 0) and (tmpY < orig.Height) and
         orig.GetPixelAt(tmpX, tmpY, c) then
      begin
        {$IFDEF FPC}
        for chan in mask do begin
        {$ELSE}
        for chan := low(TtsColorChannel) to high(TtsColorChannel) do if (chan in mask) then begin
        {$ENDIF}
          s := c.arr[Integer(chan)] * fColor.arr[Integer(chan)] * fKernel.Items[i].Value;
          if (s > result.arr[Integer(chan)]) then begin
            result.arr[Integer(chan)] := s;
            if (s >= 1.0) then begin
              Exclude(mask, chan);
              if (mask = []) then
                exit;
            end;
          end;
        end;
      end;
    end;
  end;

begin
  result := inherited Execute(aChar, aImage);
  if not result or not Assigned(aImage) then
    exit;

  aImage.Resize(
      aImage.Width  + 2 * fKernel.SizeX,
      aImage.Height + 2 * fKernel.SizeY,
      fKernel.SizeX, fKernel.SizeY);
  orig := TtsImage.Create(fContext);
  try
    orig.Assign(aImage);
    aImage.FillColor(fColor, TS_COLOR_CHANNELS_RGBA, TS_IMAGE_MODES_REPLACE_ALL);
    for y := 0 to orig.Height-1 do begin
      dst := aImage.Scanline[y];
      for x := 0 to orig.Width-1 do
        tsFormatMap(aImage.Format, dst, BorderLookup);
    end;
    aImage.Blend(orig, 0, 0, @tsBlendColorAdditiveAlpha);
  finally
    FreeAndNil(orig);
  end;

  m := aChar.GlyphMetric;
  m.GlyphRect.Right  := m.GlyphRect.Right  + 2 * fKernel.SizeX;
  m.GlyphRect.Bottom := m.GlyphRect.Bottom + 2 * fKernel.SizeY;
  if fKeepSize then begin
    m.GlyphOrigin.x := m.GlyphOrigin.x - fKernel.SizeX;
    m.GlyphOrigin.y := m.GlyphOrigin.y + fKernel.SizeY;
  end else begin
    m.Advance       := m.Advance       + 2 * fKernel.SizeX;
    m.GlyphOrigin.y := m.GlyphOrigin.y +     fKernel.SizeY;
  end;
  aChar.GlyphMetric := m;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsPostProcessorBorder.Create(const aContext: TtsContext;
  const aWidth, aStrength: Single; const aColor: TtsColor4f; const aKeepSize: Boolean);
begin
  inherited Create(aContext);
  fWidth    := aWidth;
  fStrength := aStrength;
  fColor    := aColor;
  fKeepSize := aKeepSize;
  fKernel   := TtsKernel2D.Create(fWidth, fStrength);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsPostProcessorBorder.Destroy;
begin
  FreeAndNil(fKernel);
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsPostProcessorShadow////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsPostProcessorShadow.SetRadius(const aValue: Single);
begin
  fRadius := aValue;
  FreeAndNil(fKernel);
  fKernel := TtsKernel1D.Create(fRadius, fStrength);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsPostProcessorShadow.SetStrength(const aValue: Single);
begin
  fStrength := aValue;
  FreeAndNil(fKernel);
  fKernel := TtsKernel1D.Create(fRadius, fStrength);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsPostProcessorShadow.Execute(const aChar: TtsChar; const aImage: TtsImage): Boolean;
var
  orig: TtsImage;
  tmpX, tmpY: Integer;
  m: TtsGlyphMetric;
begin
  result := inherited Execute(aChar, aImage);
  if not result or not Assigned(aImage) then
    exit;
  orig := TtsImage.Create(fContext);
  try
    orig.Assign(aImage);
    aImage.Resize(
      aImage.Width  + 2 * fKernel.Size + abs(fOffset.x),
      aImage.Height + 2 * fKernel.Size + abs(fOffset.y),
      fKernel.Size + max(0, fOffset.x),
      fKernel.Size + max(0, fOffset.y));
    aImage.FillColor(fColor, TS_COLOR_CHANNELS_RGBA, TS_IMAGE_MODES_MODULATE_ALPHA);
    aImage.Blur(fKernel, fKernel, [tsChannelAlpha]);

    tmpX := fKernel.Size + max(0, -fOffset.x); //fKernel.Size - fOffset.x;
    tmpY := fKernel.Size + max(0, -fOffset.y); //fKernel.Size - fOffset.y;
    aImage.Blend(orig, tmpX, tmpY, @tsBlendColorAlpha);

    m := aChar.GlyphMetric;
    m.GlyphRect.Right  := m.GlyphRect.Right  + 2 * fKernel.Size + abs(fOffset.x);
    m.GlyphRect.Bottom := m.GlyphRect.Bottom + 2 * fKernel.Size + abs(fOffset.y);
    m.GlyphOrigin.x    := m.GlyphOrigin.x - tmpX;
    m.GlyphOrigin.y    := m.GlyphOrigin.y + tmpX;
    aChar.GlyphMetric  := m;
  finally
    FreeAndNil(orig);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsPostProcessorShadow.Create(const aContext: TtsContext; const aRadius, aStrength: Single;
  const aOffset: TtsPosition; const aColor: TtsColor4f);
begin
  inherited Create(aContext);
  fRadius   := aRadius;
  fStrength := aStrength;
  fOffset   := aOffset;
  fColor    := aColor;
  fKernel   := TtsKernel1D.Create(fRadius, fStrength);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsPostProcessorShadow.Destroy;
begin
  FreeAndNil(fKernel);
  inherited Destroy;
end;

end.

