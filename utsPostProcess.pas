unit utsPostProcess;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, utsTextSuite, utsTypes;

type
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsPostProcessFillColor = class(TtsPostProcessStep)
  private
    fColor: TtsColor4f;
    fModes: TtsImageModes;
    fChannels: TtsColorChannels;
  protected
    procedure Execute(const aChar: TtsChar; const aCharImage: TtsImage); override;
  public
    constructor Create(const aColor: TtsColor4f;
      const aModes: TtsImageModes; const aChannels: TtsColorChannels);
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsPostProcessFillPattern = class(TtsPostProcessStep)
  private
    fPattern: TtsImage;
    fOwnsPattern: Boolean;
    fX, fY: Integer;
    fModes: TtsImageModes;
    fChannels: TtsColorChannels;
  protected
    procedure Execute(const aChar: TtsChar; const aCharImage: TtsImage); override;
  public
    constructor Create(const aPattern: TtsImage; const aOwnsPattern: Boolean; const X, Y: Integer;
      const aModes: TtsImageModes; const aChannels: TtsColorChannels);
    destructor Destroy; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsPostProcessBorder = class(TtsPostProcessStep)
  private
    fKernel: TtsKernel2D;
    fColor: TtsColor4f;
    fKeepCharSize: Boolean;
  public
    procedure Execute(const aChar: TtsChar; const aCharImage: TtsImage); override;
  public
    constructor Create(const aWidth, aStrength: Single; const aColor: TtsColor4f;
      const aKeepCharSize: Boolean = false);
    destructor Destroy; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsPostProcessShadow = class(TtsPostProcessStep)
  private
    fKernel: TtsKernel1D;
    fColor: TtsColor4f;
    fX, fY: Integer;
  protected
    procedure Execute(const aChar: TtsChar; const aCharImage: TtsImage); override;
  public
    constructor Create(const aRadius, aStrength: Single; const X, Y: Integer; const aColor: TtsColor4f);
    destructor Destroy; override;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsPostProcessFillColor///////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsPostProcessFillColor.Execute(const aChar: TtsChar; const aCharImage: TtsImage);
begin
  if Assigned(aCharImage) then
    aCharImage.FillColor(fColor, fChannels, fModes);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsPostProcessFillColor.Create(const aColor: TtsColor4f; const aModes: TtsImageModes; const aChannels: TtsColorChannels);
begin
  inherited Create;
  fColor    := aColor;
  fModes    := aModes;
  fChannels := aChannels;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsPostProcessFillPattern/////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsPostProcessFillPattern.Execute(const aChar: TtsChar; const aCharImage: TtsImage);
begin
  if Assigned(aCharImage) then
    aCharImage.FillPattern(fPattern, fX, fY, fChannels, fModes);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsPostProcessFillPattern.Create(const aPattern: TtsImage; const aOwnsPattern: Boolean; const X,
  Y: Integer; const aModes: TtsImageModes; const aChannels: TtsColorChannels);
begin
  inherited Create;
  fPattern     := aPattern;
  fOwnsPattern := aOwnsPattern;
  fX           := X;
  fY           := Y;
  fModes       := aModes;
  fChannels    := aChannels;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsPostProcessFillPattern.Destroy;
begin
  if fOwnsPattern then
    FreeAndNil(fPattern);
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsPostProcessBorder//////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsPostProcessBorder.Execute(const aChar: TtsChar; const aCharImage: TtsImage);
var
  orig: TtsImage;
  x, y: Integer;
  dst: PByte;

  function BorderLookup: TtsColor4f;
  var
    i: Integer;
    c: TtsColor4f;
    s: Single;
    chan: TtsColorChannel;
    mask: TtsColorChannels;
    tmpX, tmpY: Integer;
  begin
    mask   := TS_CHANNELS_RGBA;
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
  if not Assigned(aCharImage) then
    exit;

  aCharImage.Resize(
    aCharImage.Width  + 2 * fKernel.SizeX,
    aCharImage.Height + 2 * fKernel.SizeY,
    fKernel.SizeX, fKernel.SizeY);

  orig := TtsImage.Create;
  try
    orig.Assign(aCharImage);
    aCharImage.FillColor(fColor, TS_CHANNELS_RGBA, TS_MODES_REPLACE_ALL);

    for y := 0 to orig.Height-1 do begin
      dst := aCharImage.Scanline[y];
      for x := 0 to orig.Width-1 do
        tsFormatMap(aCharImage.Format, dst, BorderLookup);
    end;

    aCharImage.Blend(orig, 0, 0, @tsBlendFundAdditiveAlpha);
  finally
    FreeAndNil(orig);
  end;

  aChar.GlyphRect := tsRect(
    aChar.GlyphRect.Left,
    aChar.GlyphRect.Top,
    aChar.GlyphRect.Right  + 2 * fKernel.SizeX,
    aChar.GlyphRect.Bottom + 2 * fKernel.SizeY);

  if fKeepCharSize then begin
    aChar.GlyphOrigin := tsPosition(
      aChar.GlyphOrigin.x - fKernel.SizeX,
      aChar.GlyphOrigin.y + fKernel.SizeY);
  end else begin
    aChar.Advance := aChar.Advance + 2 * fKernel.SizeX;
    aChar.GlyphOrigin := tsPosition(
      aChar.GlyphOrigin.x,
      aChar.GlyphOrigin.y + fKernel.SizeY);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsPostProcessBorder.Create(const aWidth, aStrength: Single; const aColor: TtsColor4f; const aKeepCharSize: Boolean);
begin
  inherited Create;
  fKernel       := TtsKernel2D.Create(aWidth, aStrength);
  fColor        := aColor;
  fKeepCharSize := aKeepCharSize;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsPostProcessBorder.Destroy;
begin
  FreeAndNil(fKernel);
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsPostProcessShadow//////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsPostProcessShadow.Execute(const aChar: TtsChar; const aCharImage: TtsImage);
var
  orig: TtsImage;
  tmpX, tmpY: Integer;
begin
  orig := TtsImage.Create;
  try
    orig.Assign(aCharImage);
    aCharImage.Resize(
      aCharImage.Width  + 2 * fKernel.Size,
      aCharImage.Height + 2 * fKernel.Size,
      fKernel.Size, fKernel.Size);
    aCharImage.FillColor(fColor, TS_CHANNELS_RGBA, TS_MODES_MODULATE_ALPHA);
    aCharImage.Blur(fKernel, fKernel, [tsChannelAlpha]);

    tmpX := fKernel.Size - fX;
    tmpY := fKernel.Size - fY;
    aCharImage.Blend(orig, tmpX, tmpY, @tsBlendFundAlpha);

    aChar.GlyphRect := tsRect(
      aChar.GlyphRect.Left,
      aChar.GlyphRect.Top,
      aChar.GlyphRect.Right  + 2 * fKernel.Size,
      aChar.GlyphRect.Bottom + 2 * fKernel.Size);
    aChar.GlyphOrigin := tsPosition(
      aChar.GlyphOrigin.x - tmpX,
      aChar.GlyphOrigin.y + tmpX);
  finally
    FreeAndNil(orig);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsPostProcessShadow.Create(const aRadius, aStrength: Single; const X, Y: Integer; const aColor: TtsColor4f);
begin
  inherited Create;
  fKernel := TtsKernel1D.Create(aRadius, aStrength);
  fX      := X;
  fY      := Y;
  fColor  := aColor;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsPostProcessShadow.Destroy;
begin
  FreeAndNil(fKernel);
  inherited Destroy;
end;

end.

