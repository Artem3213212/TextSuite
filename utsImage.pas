unit utsImage;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  utsTypes, utsUtils;

type
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsImage = class;
  TtsImageFunc = procedure(const aImage: TtsImage; X, Y: Integer; var aPixel: TtsColor4f; aArgs: Pointer);
  TtsImage = class(TObject)
  private
    fWidth: Integer;
    fHeight: Integer;
    fDataSize: Integer;
    fLineSize: Integer;
    fFormat: TtsFormat;

    fData: Pointer;
    fHasScanlines: Boolean;
    fScanlines: array of Pointer;

    function GetScanline(const aIndex: Integer): Pointer;
    function GetIsEmpty: Boolean;
    procedure UpdateScanlines;
    procedure SetData(
      const aData: Pointer;
      const aFormat: TtsFormat = tsFormatEmpty;
      const aWidth: Integer = 0;
      const aHeight: Integer = 0;
      const aLineSize: Integer = 0;
      const aDataSize: Integer = 0);
  public
    property IsEmpty:  Boolean   read GetIsEmpty;
    property Width:    Integer   read fWidth;
    property Height:   Integer   read fHeight;
    property LineSize: Integer   read fLineSize;
    property DataSize: Integer   read fDataSize;
    property Format:   TtsFormat read fFormat;
    property Data:     Pointer   read fData;
    property Scanline[const aIndex: Integer]: Pointer read GetScanline;

    function GetPixelAt(const x, y: Integer; out aColor: TtsColor4f): Boolean;

    procedure Assign(const aImage: TtsImage);
    procedure CreateEmpty(const aFormat: TtsFormat; const aWidth, aHeight: Integer);
    procedure LoadFromFunc(const aFunc: TtsImageFunc; const aArgs: Pointer);

    procedure Resize(const aNewWidth, aNewHeight, X, Y: Integer);
    procedure FindMinMax(out aRect: TtsRect);

    procedure FillColor(const aColor: TtsColor4f; const aChannelMask: TtsColorChannels; const aModes: TtsImageModes);
    procedure FillPattern(const aPattern: TtsImage; X, Y: Integer; const aChannelMask: TtsColorChannels; const aModes: TtsImageModes);
    procedure Blend(const aImage: TtsImage; const X, Y: Integer; const aFunc: TtsBlendColorFunc);
    procedure Blur(const aHorzKernel, aVertKernel: TtsKernel1D; const aChannelMask: TtsColorChannels);

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  utsConstants;

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
procedure TtsImage.UpdateScanlines;
var
  i: Integer;
  tmp: PByte;
begin
  SetLength(fScanlines, fHeight);
  for i := 0 to fHeight-1 do begin
    tmp := fData;
    inc(tmp, i * fLineSize);
    fScanlines[i] := tmp;
  end;
  fHasScanlines := true;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsImage.SetData(const aData: Pointer; const aFormat: TtsFormat;
  const aWidth: Integer; const aHeight: Integer;
  const aLineSize: Integer; const aDataSize: Integer);
begin
  fHasScanlines := false;
  if Assigned(fData) then
    FreeMemory(fData);

  fData := aData;
  if Assigned(fData) then begin
    fWidth    := aWidth;
    fHeight   := aHeight;
    fFormat   := aFormat;
    fLineSize := aLineSize;
    fDataSize := aDataSize;
  end else begin
    fWidth    := 0;
    fHeight   := 0;
    fLineSize := 0;
    fDataSize := 0;
    fFormat   := tsFormatEmpty;
  end;
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
begin
  GetMem(ImgData, aImage.DataSize);
  if Assigned(ImgData) then
    Move(aImage.Data^, ImgData^, aImage.DataSize);
  SetData(ImgData, aImage.Format, aImage.Width, aImage.Height, aImage.LineSize, aImage.DataSize);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsImage.CreateEmpty(const aFormat: TtsFormat; const aWidth, aHeight: Integer);
var
  ImgData: PByte;
  lSize, dSize: Integer;
begin
  lSize   := aWidth * tsFormatSize(aFormat);
  lSize   := lSize + ((4 - (lSize mod 4)) mod 4);
  dSize   := aHeight * lSize;
  ImgData := AllocMem(dSize);
  FillChar(ImgData^, dSize, #0);
  SetData(ImgData, aFormat, aWidth, aHeight, lSize, dSize);
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
  pSize, lSize, dSize: Integer;

  src, dst: PByte;
  YStart, YEnd, YPos, XStart, XEnd: Integer;
begin
  if (aNewHeight = 0) or (aNewWidth = 0) then begin
    SetData(nil);
    exit;
  end;

  pSize     := tsFormatSize(Format);
  lSize     := pSize * aNewWidth;
  lSize     := lSize + ((4 - (lSize mod 4)) mod 4);
  dSize     := lSize * aNewHeight;

  GetMem(ImgData, dSize);
  try
    FillChar(ImgData^, dSize, 0);

    // positions
    YStart := Max(0, Y);
    YEnd   := Min(aNewHeight, Y + Height);
    XStart := Max(0, X);
    XEnd   := Min(aNewWidth, X + Width);

    // copy data
    for YPos := YStart to YEnd -1 do begin
      dst := ImgData;
      Inc(dst, lSize * YPos + pSize * XStart);

      src := fData;
      Inc(src, fLineSize * (YPos - Y) + pSize * (XStart - X));

      Move(src^, dst^, (XEnd - XStart) * pSize);
    end;

    // assign
    SetData(ImgData, Format, aNewWidth, aNewHeight, lSize, dSize);
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
procedure TtsImage.FillColor(const aColor: TtsColor4f; const aChannelMask: TtsColorChannels;
  const aModes: TtsImageModes);
var
  x, y: Integer;
  rp, wp: PByte;
  c: TtsColor4f;
  ch: TtsColorChannel;
  i: Integer;
begin
  for y := 0 to Height-1 do begin
    rp := Scanline[y];
    wp := rp;
    for x := 0 to Width-1 do begin
      tsFormatUnmap(Format, rp, c);
      for i := 0 to 3 do begin
        ch := TtsColorChannel(i);
        if (ch in aChannelMask) then
          c.arr[i] := TS_IMAGE_MODE_FUNCTIONS[aModes[ch]](aColor.arr[i], c.arr[i]);
      end;
      tsFormatMap(Format, wp, c);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsImage.FillPattern(const aPattern: TtsImage; X, Y: Integer;
  const aChannelMask: TtsColorChannels; const aModes: TtsImageModes);
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
      tsFormatUnmap(aPattern.Format, src, cSrc);
      tsFormatUnmap(Format, tmp, cDst);
      for i := 0 to 3 do begin
        ch := TtsColorChannel(i);
        if (ch in aChannelMask) then
          cDst.arr[i] := TS_IMAGE_MODE_FUNCTIONS[aModes[ch]](cSrc.arr[i], cDst.arr[i]);
      end;
      tsFormatMap(Format, dst, cDst);
      inc(posX);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsImage.Blend(const aImage: TtsImage; const X, Y: Integer; const aFunc: TtsBlendColorFunc);
var
  _x, _y, x1, x2, y1, y2: Integer;
  src, dst, tmp: PByte;
  srcColor, dstColor: TtsColor4f;
  srcPixelSize, dstPixelSize: Integer;
begin
  x1 := Max(X, 0);
  x2 := Min(X + aImage.Width , Width);
  y1 := Max(Y, 0);
  y2 := Min(Y + aImage.Height, Height);
  srcPixelSize := tsFormatSize(aImage.Format);
  dstPixelSize := tsFormatSize(Format);
  for _y := y1 to y2-1 do begin
    src := aImage.Scanline[_y - min(y1, y)];
    dst := Scanline[_y];
    inc(src, (x1 - x) * srcPixelSize);
    inc(dst,  x1      * dstPixelSize);
    tmp := dst;
    for _x := x1 to x2-1 do begin
      tsFormatUnmap(aImage.Format, src, srcColor);
      tsFormatUnmap(       Format, dst, dstColor);
      tsFormatMap(aImage.Format, tmp, aFunc(srcColor, dstColor));
    end;
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
        for i := 0 to 3 do
          if (TtsColorChannel(i) in aChannelMask) then
            c.arr[i] := 0;

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
        for i := 0 to 3 do
          if (TtsColorChannel(i) in aChannelMask) then
            c.arr[i] := c.arr[i] / v;
        tsFormatMap(aDst.Format, dst, c);
      end;
    end;
  end;

begin
  tmpImage := TtsImage.Create;
  try
    tmpImage.CreateEmpty(Format, Width, Height);
    tmpImage.FillColor(tsColor4f(1, 1, 1, 0), TS_COLOR_CHANNELS_RGBA, TS_IMAGE_MODES_REPLACE_ALL);

    DoBlur(self, tmpImage, aHorzKernel, 1, 0);
    DoBlur(tmpImage, self, aVertKernel, 0, 1);
  finally
    FreeAndNil(tmpImage);
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


end.

