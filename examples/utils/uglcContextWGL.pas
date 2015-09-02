unit uglcContextWGL;

{ Package:      OpenGLCore
  Prefix:       glc - OpenGL Core
  Beschreibung: diese Unit enthält eine Klassen-Kapselung für OpenGL Kontexte für Windows
  Hint:         diese Unit sollte niemals direkt genutzt werden (siehe uglcContext) }

interface

uses
  Classes, SysUtils, Forms, Windows, uglcContext, dglOpenGL, Controls;

type
  EWGLError = class(EGLError);

  { TglcContextWGL }

  TglcContextWGL = class(TglcContext)
  private
    FDC: HDC;
    FRC: HGLRC;
    fHandle: THandle;
    fPixelFormat: Integer;
    {%H-}constructor Create(const aControl: TWinControl); overload;
  protected
    procedure UpdatePixelFormat;
    procedure OpenContext; override;
    function FindPixelFormat: Integer;
    function FindPixelFormatNoAA: Integer;
    procedure OpenFromPF(PixelFormat: Integer);
  public
    constructor Create(const aControl: TWinControl; const aPixelFormatSettings: TglcContextPixelFormatSettings); overload; override;
    constructor Create(const aControl: TWinControl; const aPixelFormatSettings: TglcContextPixelFormatSettings; const aVersionSettings: TglcContextVersionSettings); overload; override;

    procedure CloseContext; override;
    procedure Activate; override;
    procedure Deactivate; override;
    function IsActive: boolean; override;
    procedure SwapBuffers; override;
    procedure SetSwapInterval(const aInterval: GLint); override;
    function GetSwapInterval: GLint; override;
    procedure Share(const aContext: TglcContext); override;

    class function ChangeDisplaySettings(const aWidth, aHeight, aBitPerPixel, aFreq: Integer;
      const aFlags: TglcDisplayFlags): Boolean; override;
    class function IsAnyContextActive: boolean; override;
  end;

implementation

{ TglcContextWGL }

constructor TglcContextWGL.Create(const aControl: TWinControl);
begin
  inherited Create(aControl, MakePF());
  fHandle := aControl.Handle;
end;

procedure TglcContextWGL.UpdatePixelFormat;
begin
  fPixelFormat := FindPixelFormat;
  if (fPixelFormat = 0) then begin
    // try without MS
    fPixelFormatSettings.MultiSampling := 1;
    fPixelFormat := FindPixelFormat;
  end;
end;

procedure TglcContextWGL.OpenContext;
begin
  inherited OpenContext;
  OpenFromPF(fPixelFormat);
end;

function TglcContextWGL.FindPixelFormat: Integer;
var
  OldRC: HGLRC; OldDC: HDC;
  tmpWnd: TForm;
  tmpContext: TglcContextWGL;
  pf, i, max: integer;
  Count: GLuint;
  PFList, SampleList: array[0..31] of GLint;

  procedure ChoosePF(pPFList, pSampleList: PGLint; MaxCount: integer);
  var
    //ARB_Erweiterung vorhanden
    //|          EXT_Erweiterung vorhanden
    MultiARBSup, MultiEXTSup: Boolean;
    //Liste der Integer Attribute
    IAttrib: array[0..22] of GLint;
    //Liste der Float Attribute (nur 0, da kein Wert)
    FAttrib: GLFloat;
    QueryAtrib, i: Integer;
    PPosiblePF, PSample: PglInt;
  begin
    //Pixelformate mit AA auslesen
    MultiARBSup := false;
    MultiEXTSup := false;
    if WGL_ARB_extensions_string and
       WGL_ARB_pixel_format and
       (WGL_ARB_MULTISAMPLE or GL_ARB_MULTISAMPLE) then
      multiARBSup := true;
    if WGL_EXT_extensions_string and
       WGL_EXT_pixel_format and
       (WGL_EXT_MULTISAMPLE or GL_EXT_MULTISAMPLE) then
      multiEXTSup := true;

    if multiARBSup then
      Read_WGL_ARB_pixel_format
    else if multiEXTSup then
      Read_WGL_EXT_pixel_format;

    if not (MultiARBSup or MultiEXTSup) then
      exit;

    IAttrib[00] := WGL_DRAW_TO_WINDOW_ARB;
    IAttrib[01] := 1;

    IAttrib[02] := WGL_SUPPORT_OPENGL_ARB;
    IAttrib[03] := 1;

    IAttrib[04] := WGL_DOUBLE_BUFFER_ARB;
    if (fPixelFormatSettings.DoubleBuffered) then
      IAttrib[05] := 1
    else
      IAttrib[05] := 0;

    IAttrib[06] := WGL_PIXEL_TYPE_ARB;
    IAttrib[07] := WGL_TYPE_RGBA_ARB;

    IAttrib[08] := WGL_COLOR_BITS_ARB;
    IAttrib[09] := fPixelFormatSettings.ColorBits;

    IAttrib[10] := WGL_ALPHA_BITS_ARB;
    IAttrib[11] := 0; //TODO: fPixelFormatSettings.AlphaBits;

    IAttrib[12] := WGL_DEPTH_BITS_ARB;
    IAttrib[13] := fPixelFormatSettings.DepthBits;

    IAttrib[14] := WGL_STENCIL_BITS_ARB;
    IAttrib[15] := fPixelFormatSettings.StencilBits;

    IAttrib[16] := WGL_ACCUM_BITS_ARB;
    IAttrib[17] := fPixelFormatSettings.AccumBits;

    IAttrib[18] := WGL_AUX_BUFFERS_ARB;
    IAttrib[19] := fPixelFormatSettings.AuxBuffers;

    IAttrib[20] := WGL_SAMPLE_BUFFERS_ARB;
    IAttrib[21] := 1;

    IAttrib[22] := 0;
    FAttrib     := 0;

    if multiARBSup then
      wglChoosePixelFormatARB(tmpContext.FDC, @IAttrib[0], @FAttrib, MaxCount, pPFList, @Count)
    else if multiEXTSup then
      wglChoosePixelFormatEXT(tmpContext.FDC, @IAttrib[0], @FAttrib, MaxCount, pPFList, @Count);

    if Integer(Count) > length(PFList) then
      Count := length(PFList);

    QueryAtrib := WGL_SAMPLES_ARB;
    PSample    := pSampleList;
    PPosiblePF := @PFList[0];
    for i := 0 to Count-1 do begin
      if multiARBSup then
        wglGetPixelFormatAttribivARB(tmpContext.FDC, PPosiblePF^, 0, 1, @QueryAtrib, PSample)
      else if multiEXTSup then
        wglGetPixelFormatAttribivEXT(tmpContext.FDC, PPosiblePF^, 0, 1, @QueryAtrib, PSample);
      inc(PSample);
      inc(PPosiblePF);
    end;
  end;
begin
  if (fPixelFormatSettings.MultiSampling = 1) then begin
    Result := FindPixelFormatNoAA;
    exit;
  end;
  Result := 0;
  OldDC  := wglGetCurrentDC();
  OldRC  := wglGetCurrentContext();
  try
    tmpWnd     := TForm.Create(nil);
    tmpContext := TglcContextWGL.Create(tmpWnd);
    try
      pf := tmpContext.FindPixelFormatNoAA;
      tmpContext.OpenFromPF(pf);
      tmpContext.Activate;

      FillChar({%H-}PFList[0], Length(PFList), 0);
      FillChar({%H-}SampleList[0], Length(SampleList), 0);
      ChoosePF(@PFList[0], @SampleList[0], length(SampleList));
      max := 0;
      for i := 0 to Count-1 do begin
        if (max < SampleList[i]) and (SampleList[i] <= fPixelFormatSettings.MultiSampling) and (PFList[i] <> 0) then begin
          max := SampleList[i];
          result := PFList[i];
          if (max = fPixelFormatSettings.MultiSampling) then
            break;
        end;
      end;
      tmpContext.Deactivate;
    finally
      FreeAndNil(tmpContext);
      FreeAndNil(tmpWnd);
    end;
  finally
    if (OldDC <> 0) and (OldRC <> 0) then
     ActivateRenderingContext(OldDC, OldRC);
  end;
end;

function TglcContextWGL.FindPixelFormatNoAA: Integer;
const
  MemoryDCs = [OBJ_MEMDC, OBJ_METADC, OBJ_ENHMETADC];
var
  //DeviceContext
  DC: HDC;
  //Objekttyp des DCs
  AType: DWord;
  //Beschreibung zum passenden Pixelformat
  PFDescriptor: TPixelFormatDescriptor;
begin
  result := 0;
  DC := GetDC(fHandle);
  if DC = 0 then begin
    exit;
  end;
  FillChar(PFDescriptor{%H-}, SizeOf(PFDescriptor), #0);
  with PFDescriptor do begin
    nSize    := SizeOf(PFDescriptor);
    nVersion := 1;
    dwFlags  := PFD_SUPPORT_OPENGL;
    AType    := GetObjectType(DC);
    if AType = 0 then begin
      exit;
    end;
    if fPixelFormatSettings.DoubleBuffered then
      dwFlags := dwFlags or PFD_DOUBLEBUFFER;
    if fPixelFormatSettings.Stereo then
      dwFlags := dwFlags or PFD_STEREO;
    if AType in MemoryDCs then
      dwFlags := dwFlags or PFD_DRAW_TO_BITMAP
    else
      dwFlags := dwFlags or PFD_DRAW_TO_WINDOW;

    iPixelType   := PFD_TYPE_RGBA;
    cColorBits   := fPixelFormatSettings.ColorBits;
//TODO:    cAlphaBits   := fPixelFormatSettings.AlphaBits;
    cDepthBits   := fPixelFormatSettings.DepthBits;
    cStencilBits := fPixelFormatSettings.StencilBits;
    cAccumBits   := fPixelFormatSettings.AccumBits;
    cAuxBuffers  := fPixelFormatSettings.AuxBuffers;

    if fPixelFormatSettings.Layer = 0 then
      iLayerType := PFD_MAIN_PLANE
    else if fPixelFormatSettings.Layer > 0 then
      iLayerType := PFD_OVERLAY_PLANE
    else
      iLayerType := Byte(PFD_UNDERLAY_PLANE);
  end;
  result := ChoosePixelFormat(DC, @PFDescriptor);
end;

procedure TglcContextWGL.OpenFromPF(PixelFormat: Integer);
var
  tmpRC: HGLRC;
  Attribs: array of GLint;
  CreateContextAttribs: TwglCreateContextAttribsARB;
begin
  if PixelFormat = 0 then begin
    raise EWGLError.Create('Invalid PixelFormat');
  end;

  FDC := GetDC(fHandle);
  if FDC = 0 then begin
    raise EWGLError.CreateFmt('Cannot create DC on %x',[fHandle]);
  end;

  if not SetPixelFormat(FDC, PixelFormat, nil) then begin
    ReleaseDC(fHandle, FDC);
    raise EWGLError.CreateFmt('Cannot set PF %d on Control %x DC %d',[PixelFormat, fHandle, FDC]);
  end;

  tmpRC := wglCreateContext(FDC);
  if tmpRC = 0 then begin
    ReleaseDC(fHandle, FDC);
    raise EWGLError.CreateFmt('Cannot create context on Control %x DC %d',[PixelFormat, fHandle, FDC]);
  end;

  if fUseVersion and
     (fVersionSettings.Major <> GLC_CONTEXT_VERSION_UNKNOWN) and
     (fVersionSettings.Minor <> GLC_CONTEXT_VERSION_UNKNOWN) then
  begin
    { Code from dglOpenGL.pas (modified) }
    wglMakeCurrent(FDC, tmpRC);

    // Set attributes to describe our requested context
    SetLength(Attribs, 5);
    Attribs[0] := WGL_CONTEXT_MAJOR_VERSION_ARB;
    Attribs[1] := fVersionSettings.Major;
    Attribs[2] := WGL_CONTEXT_MINOR_VERSION_ARB;
    Attribs[3] := fVersionSettings.Minor;

    // Add context flag for forward compatible context
    // Forward compatible means no more support for legacy functions like
    // immediate mode (glvertex, glrotate, gltranslate, etc.)
    if fVersionSettings.ForwardCompatible then begin
      SetLength(Attribs, Length(Attribs)+2);
      Attribs[4] := WGL_CONTEXT_FLAGS_ARB;
      Attribs[5] := WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB;
    end;

    // Attribute flags must be finalized with a zero
    Attribs[High(Attribs)] := 0;

    // Get function pointer for new context creation function
    CreateContextAttribs := TwglCreateContextAttribsARB(wglGetProcAddress('wglCreateContextAttribsARB'));
    if not Assigned(CreateContextAttribs) then begin
      wglMakeCurrent(0, 0);
      wglDeleteContext(tmpRC);
      ReleaseDC(fHandle, FDC);
      raise Exception.Create('Could not get function pointer adress for wglCreateContextAttribsARB - OpenGL 3.x and above not supported!');
    end;

    // Create context
    FRC := CreateContextAttribs(FDC, 0, @Attribs[0]);
    if (FRC = 0) then begin
      wglMakeCurrent(0, 0);
      wglDeleteContext(tmpRC);
      ReleaseDC(fHandle, FDC);
      raise Exception.Create('Could not create the desired OpenGL rendering context!');
    end;

    wglMakeCurrent(0, 0);
    wglDeleteContext(tmpRC);
  end else
    FRC := tmpRC;
end;

constructor TglcContextWGL.Create(const aControl: TWinControl; const aPixelFormatSettings: TglcContextPixelFormatSettings);
begin
  inherited Create(aControl, aPixelFormatSettings);
  fHandle := aControl.Handle;
  UpdatePixelFormat;
end;

constructor TglcContextWGL.Create(const aControl: TWinControl; const aPixelFormatSettings: TglcContextPixelFormatSettings; const aVersionSettings: TglcContextVersionSettings);
begin
  inherited Create(aControl, aPixelFormatSettings, aVersionSettings);
  fHandle := aControl.Handle;
  UpdatePixelFormat;
end;

procedure TglcContextWGL.CloseContext;
begin
  if (FRC <> 0) then begin
    Deactivate;
    DestroyRenderingContext(FRC);
    ReleaseDC(fHandle, FDC);
    FRC := 0;
    FDC := 0;
  end;
end;

procedure TglcContextWGL.Activate;
begin
  ActivateRenderingContext(FDC, FRC);
end;

procedure TglcContextWGL.Deactivate;
begin
  if wglGetCurrentContext()=FRC then
    DeactivateRenderingContext;
end;

function TglcContextWGL.IsActive: boolean;
begin
  Result:= (FRC <> 0) and
           (FRC = wglGetCurrentContext()) and
           (FDC = wglGetCurrentDC());
end;

procedure TglcContextWGL.SwapBuffers;
begin
  Windows.SwapBuffers(FDC);
end;

procedure TglcContextWGL.SetSwapInterval(const aInterval: GLint);
begin
  wglSwapIntervalEXT(aInterval);
end;

function TglcContextWGL.GetSwapInterval: GLint;
begin
  result := wglGetSwapIntervalEXT();
end;

procedure TglcContextWGL.Share(const aContext: TglcContext);
begin
  wglShareLists(FRC, (aContext as TglcContextWGL).FRC);
end;

class function TglcContextWGL.ChangeDisplaySettings(const aWidth, aHeight,
  aBitPerPixel, aFreq: Integer; const aFlags: TglcDisplayFlags): Boolean;
var
  dm: TDeviceMode;
  flags: Cardinal;
begin
  FillChar(dm{%H-}, SizeOf(dm), 0);
  with dm do begin
    dmSize             := SizeOf(dm);
    dmPelsWidth        := aWidth;
    dmPelsHeight       := aHeight;
    dmDisplayFrequency := aFreq;
    dmBitsPerPel       := aBitPerPixel;
    dmFields           := DM_PELSWIDTH or DM_PELSHEIGHT or DM_BITSPERPEL or DM_DISPLAYFREQUENCY;
  end;
  flags := 0; //CDS_TEST;
  if (dfFullscreen in aFlags) then
    flags := flags or CDS_FULLSCREEN;
  result := (Windows.ChangeDisplaySettings(dm, flags) = DISP_CHANGE_SUCCESSFUL);
end;

class function TglcContextWGL.IsAnyContextActive: boolean;
begin
  Result:= (wglGetCurrentContext()<>0) and (wglGetCurrentDC()<>0);
end;

end.

