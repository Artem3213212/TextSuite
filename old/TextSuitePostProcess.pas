{
TextSuite (C) Steffen Xonna (aka Lossy eX)
http://www.opengl24.de/
-----------------------------------------------------------------------
For copyright informations see file copyright.txt.
}

{$I TextSuiteOptions.inc}

unit TextSuitePostProcess;

interface

uses
  TextSuite,
  TextSuiteClasses;


type
  // ** Post Processing FillColor **
  TtsPostFillColor = class(TtsPostProcessStep)
  protected
    fRed: Single;
    fGreen: Single;
    fBlue: Single;
    fAlpha: Single;
    fLuminance: Single;
    fChannelMask: tsBitmask;

    fModes: TtsImageModes;

    procedure PostProcess(const CharImage: TtsImage; const Char: TtsChar); override;
  public
    constructor Create(Red, Green, Blue, Alpha: Single; ChannelMask: tsBitmask; Modes: TtsImageModes);
  end;


  // ** Post Processing FillPattern **
  TtsPostFillPattern = class(TtsPostProcessStep)
  protected
    fPattern: TtsImage;
    fX: Integer;
    fY: Integer;
    fChannelMask: tsBitmask;
    fModes: TtsImageModes;

    procedure PostProcess(const CharImage: TtsImage; const Char: TtsChar); override;
  public
    constructor Create(Pattern: TtsImage; X, Y: Integer; ChannelMask: tsBitmask; Modes: TtsImageModes);
  end;


  // ** Post Processing Border **
  TtsPostBorderLookupFuncData = record
    Kernel: TtsKernel2D;
    XPos, YPos, XMax, YMax: Integer;

    pData: pByte;
  end;


  TtsPostBorder = class(TtsPostProcessStep)
  protected
    fKernel: TtsKernel2D;

    fRed: Single;
    fGreen: Single;
    fBlue: Single;
    fAlpha: Single;

    procedure PostProcess(const CharImage: TtsImage; const Char: TtsChar); override;
  public
    constructor Create(Width, Strength: Single; Red, Green, Blue, Alpha: Single);
    destructor Destroy; override;
  end;

  // ** Post Processing Kerning **
  TtsPostKerning = class(TtsPostProcessStep)
  protected
    procedure PostProcess(const CharImage: TtsImage; const Char: TtsChar); override;
  end;


  // ** Post Processing Shadow **
  TtsPostShadow = class(TtsPostProcessStep)
  protected
    fKernel: TtsKernel1D;

    fX: Integer;
    fY: Integer;

    fRed: Single;
    fGreen: Single;
    fBlue: Single;
    fAlpha: Single;

    procedure PostProcess(const CharImage: TtsImage; const Char: TtsChar); override;
  public
    constructor Create(Radius: Single; X, Y: Integer; Red, Green, Blue, Alpha: Single);
    destructor Destroy; override;
  end;


	// ** Post Processing Custom **
  TtsPostCustom = class(TtsPostProcessStep)
  protected
    fContext: TtsContext;
    fPostProcessProc: tsPostProcessProc;
    fData: Pointer;

    procedure PostProcess(const CharImage: TtsImage; const Char: TtsChar); override;
  public
    constructor Create(Context: TtsContext; PostProcessProc: tsPostProcessProc; Data: Pointer);
  end;


implementation

//uses
//  TextSuiteImageUtils;


{ TtsPostFillColor }

constructor TtsPostFillColor.Create(Red, Green, Blue, Alpha: Single; ChannelMask: tsBitmask; Modes: TtsImageModes);
begin
  inherited Create;

  fRed := Red;
  fGreen := Green;
  fBlue := Blue;
  fAlpha := Alpha;
  fChannelMask := ChannelMask;
  fModes := Modes;
end;


procedure TtsPostFillColor.PostProcess(const CharImage: TtsImage; const Char: TtsChar);
begin
  if CharImage <> nil then
    CharImage.FillColor(fRed, fGreen, fBlue, fAlpha, fChannelMask, fModes);
end;


{ TtsPostFillPattern }

constructor TtsPostFillPattern.Create(Pattern: TtsImage; X, Y: Integer; ChannelMask: tsBitmask; Modes: TtsImageModes);
begin
  inherited Create;

  fPattern := Pattern;
  fX := X;
  fY := Y;
  fChannelMask := ChannelMask;
  fModes := Modes;
end;


procedure TtsPostFillPattern.PostProcess(const CharImage: TtsImage; const Char: TtsChar);
begin
  if CharImage <> nil then
    CharImage.FillPattern(fPattern, fX, fY, fChannelMask, fModes);
end;



{ TtsPostBorder }

constructor TtsPostBorder.Create(Width, Strength, Red, Green, Blue, Alpha: Single);
begin
  inherited Create;

  fKernel := TtsKernel2D.Create(Width, Strength);

  fRed := Red;
  fGreen := Green;
  fBlue := Blue;
  fAlpha := Alpha;
end;


function BorderLookupMax(var Data: TtsPostBorderLookupFuncData): Byte;
var
  Idx: Integer;
  Temp, TempValue: Single; 
  pTempData: pByte;
begin
  TempValue := 0;

  with Data, Data.Kernel do begin
    for Idx := 0 to ItemCount - 1 do
      with Items[Idx] do
        if ((XPos + OffsetX >= 0) and (XPos + OffsetX < XMax) and
            (YPos + OffsetY >= 0) and (YPos + OffsetY < YMax)) then begin
          pTempData := pData;

          Inc(pTempData, DataOffset);

          // there is no value
          if pTempData^ = $00 then
            Continue;

          // calculate pixel
          Temp := pTempData^ * Value;
          if (Temp > TempValue) then
            TempValue := Temp;

          // there is nothing greater than this
          if pTempData^ = $FF then
            Break;
        end;
  end;

  Result := Round(TempValue);
end;


destructor TtsPostBorder.Destroy;
begin
  fKernel.Free;

  inherited;
end;


procedure TtsPostBorder.PostProcess(const CharImage: TtsImage; const Char: TtsChar);
var
  OriginalImage: TtsImage;

  X, Y: Integer;
  pSource, pDest: ptsColor;

  Data: TtsPostBorderLookupFuncData;
begin
  if CharImage <> nil then begin
    // Make image geater
    CharImage.Resize(CharImage.Width + fKernel.SizeX * 2, CharImage.Height + fKernel.SizeY * 2, fKernel.SizeX, fKernel.SizeY);

    // Create copy of Image
    OriginalImage := TtsImage.Create;
    try
      OriginalImage.AssignFrom(CharImage);
      CharImage.FillColor(fRed, fGreen, fBlue, fAlpha, TS_CHANNELS_RGBA, cModesReplace);

      fKernel.UpdateDataOffset(4, OriginalImage.Width * 4);

      Data.Kernel := fKernel;
      Data.XMax := OriginalImage.Width;
      Data.YMax := OriginalImage.Height;

      for Y := 0 to OriginalImage.Height - 1 do begin
        pSource := OriginalImage.ScanLine[Y];
        pDest := CharImage.ScanLine[Y];

        Data.pData := @(pSource^.Alpha);
        Data.YPos := Y;

        for X := 0 to OriginalImage.Width - 1 do begin
          Data.XPos := X;

          pDest^.Alpha := Round(fAlpha * BorderLookupMax(Data));

          Inc(Data.pData, 4);
          Inc(pDest);
        end;
      end;

      // Blend OriginalImage over CharImage (shadow)
      CharImage.BlendImage(OriginalImage, 0, 0);
    finally
      OriginalImage.Free;
    end;
  end;

  // Set Char Data
  Char.GlyphRect.Left   := Char.GlyphRect.Left + fKernel.SizeX - fKernel.MidSizeX;
  Char.GlyphRect.Right  := Char.GlyphRect.Right + fKernel.SizeX + fKernel.MidSizeX;

  Char.GlyphRect.Top    := Char.GlyphRect.Top + fKernel.SizeY - fKernel.MidSizeY;
  Char.GlyphRect.Bottom := Char.GlyphRect.Bottom + fKernel.SizeY + fKernel.MidSizeY;

  Char.GlyphOriginY := Char.GlyphOriginY + fKernel.MidSizeY;
  Char.Advance := Char.Advance + fKernel.MidSizeX;
end;


{ TtsPostKerning }

procedure TtsPostKerning.PostProcess(const CharImage: TtsImage; const Char: TtsChar);
begin
//  if CharImage <> nil then 
//    Char.CalculateKerningData(CharImage);
end;


{ TtsPostShadow }

constructor TtsPostShadow.Create(Radius: Single; X, Y: Integer; Red, Green, Blue, Alpha: Single);
begin
  inherited Create;

  fKernel := TtsKernel1D.Create(Radius, 0);

  fX := X;
  fY := Y;
  fRed := Red;
  fGreen := Green;
  fBlue := Blue;
  fAlpha := Alpha;
end;


destructor TtsPostShadow.Destroy;
begin
  fKernel.Free;

  inherited;
end;


procedure TtsPostShadow.PostProcess(const CharImage: TtsImage; const Char: TtsChar);
var
  OriginalImage: TtsImage;
  TempX, TempY: Integer;
begin
  if CharImage <> nil then begin
    OriginalImage := TtsImage.Create;
    try
      // backup to original 
      OriginalImage.AssignFrom(CharImage);

      // Resizing image
      CharImage.Resize(CharImage.Width + fKernel.Size * 2, CharImage.Height + fKernel.Size * 2, fKernel.Size, fKernel.Size);

      // fill char image with color
      CharImage.FillColor(fRed, fGreen, fBlue, fAlpha, TS_CHANNELS_RGBA, cModesNormal);

      // blur charimage
      CharImage.Blur(fKernel, fKernel, TS_CHANNEL_ALPHA);

      TempX := fKernel.Size - fX;
      TempY := fKernel.Size - fY;

      // Blend OriginalImage over CharImage (shadow)
      CharImage.BlendImage(OriginalImage, TempX, TempY);

      // Set Chardimension
      with Char.GlyphRect do begin
        if TempX > 0 then begin
          Left := Left + TempX;
          Right := Right + TempX;
        end;

        if TempY > 0 then begin
          Top := Top + TempY;
          Bottom := Bottom + TempY;
        end;
      end;
    finally
      OriginalImage.Free;
    end;
  end;
end;


{ TtsPostCustom }

constructor TtsPostCustom.Create(Context: TtsContext; PostProcessProc: tsPostProcessProc; Data: Pointer);
begin
  inherited Create;

  fContext := Context;
  fPostProcessProc := PostProcessProc;
  fData := Data;
end;


procedure TtsPostCustom.PostProcess(const CharImage: TtsImage; const Char: TtsChar);
var
  ImageID: tsImageID;
begin
  if CharImage <> nil then begin
    if fContext <> nil then begin
      // temporary Add Image
      ImageID := fContext.ImageAdd(CharImage);
      try
        fPostProcessProc(ImageID, Char.CharCode, fData);
      finally
        fContext.ImageDelete(ImageID);
      end;
    end;
  end 
    // call without an ImageID
    else fPostProcessProc(0, Char.CharCode, fData);
end;


end.
