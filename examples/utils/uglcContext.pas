unit uglcContext;

{ Package:      OpenGLCore
  Prefix:       glc - OpenGL Core
  Beschreibung: diese Unit enthält eine abstrakte Klassen-Kapselung für OpenGL Kontexte


Abstrakte Contextklasse zum Erstellen von Renderkontexten auf Windows & Linux(bzw X11/Gtk2)
Für aktuelle Plattform passende Klasse kann per GetPlatformClass gefunden werden.

Bsp.:
  //muss im GUI/Main-Thread aufgerufen werden:
  pf := TglcContext.GetPlatformClass().MakePF();
  fContext := TglcContext.GetPlatformClass().Create(MyTWinControl, PF);

  //_kann_ in Background Thread abgerufen werden:
  fContext.BuildContext();
  [Arbeit mit dem Context]
  fContext.CloseContext();

  //im MainThread
  FreeAndNil(fContext)


weitere Funktionen:
  MakePF()             erzeugt PixelFormatDescriptor mit Defaults
  BuildContext()       baut Kontext (kann in BackgrounThread aufgerufen werden)
  CloseContext()       gibt den Kontext frei (muss im selben Thread aufgerufen werden wie BuildContext;
                       wird der Kontext nur im MainThread genutzt, muss CloseContext nicht explizit aufgerufen
                       werden und wird beim zerstören des Kontext-Objekts ausgeführt)
  Activate/Deactiveate Kontext aktiv schalten oder nicht
  SwapBuffers          DoubleBuffering
  SetSwapInterval      VSync
  Share                ShareLists
  EnableDebugOutput    GL-Debug via ARB_debug_output oder AMD_debug_output de/aktivieren
}

interface

uses
  SysUtils, Controls, dglOpenGL;

const
  GLC_CONTEXT_VERSION_UNKNOWN = -1;

type
{$IFNDEF fpc}
  TThreadID = Cardinal;
{$ENDIF}

  TMultiSample = 1..high(byte);
  TglcContextPixelFormatSettings = packed record
    DoubleBuffered: boolean;
    Stereo: boolean;
    MultiSampling: TMultiSample;
    ColorBits: Integer;
    DepthBits: Integer;
    StencilBits: Integer;
    AccumBits: Integer;
    AuxBuffers: Integer;
    Layer: Integer;
  end;
  TglcContextVersionSettings = packed record
    Major: Integer;
    Minor: Integer;
    ForwardCompatible: Boolean;
  end;
  TSeverity = (svLow, svMedium, svHigh);
  TLogEvent = procedure(const aSender: TObject; const aSeverity: TSeverity; const aMsg: String) of Object;

  TglcDisplayFlag = (
    dfFullscreen);
  TglcDisplayFlags = set of TglcDisplayFlag;

  EGLError = class(Exception);

  { TglcContext }
  TglcContextClass = class of TglcContext;
  TglcContext = class
  private
    fControl: TWinControl;
    fThreadID: TThreadID;
    fEnableVsync: Boolean;
    fLogEvent: TLogEvent;

    function GetEnableVSync: Boolean;
    procedure SetEnableVSync(aValue: Boolean);
    procedure LogMsg(const aSeverity: TSeverity; const aMsg: String);
    procedure SetDebugMode(const aEnable: Boolean);
  protected
    fUseVersion: Boolean;
    fPixelFormatSettings: TglcContextPixelFormatSettings;
    fVersionSettings: TglcContextVersionSettings;
    procedure OpenContext; virtual;

  public
    property PixelFormatSettings: TglcContextPixelFormatSettings read fPixelFormatSettings;
    property VersionSettings:     TglcContextVersionSettings     read fVersionSettings;

    constructor Create(const aControl: TWinControl; const aPixelFormatSettings: TglcContextPixelFormatSettings); overload; virtual;
    constructor Create(const aControl: TWinControl; const aPixelFormatSettings: TglcContextPixelFormatSettings; const aVersionSettings: TglcContextVersionSettings); overload; virtual;
    destructor Destroy; override;

    property ThreadID:    TThreadID read fThreadID;
    property EnableVSync: Boolean   read GetEnableVSync write SetEnableVSync;

    procedure BuildContext;
    procedure EnableDebugOutput(const aLogEvent: TLogEvent);
    procedure DisableDebugOutput;
    procedure CloseContext; virtual;
    procedure Activate; virtual; abstract;
    procedure Deactivate; virtual; abstract;
    function IsActive: boolean; virtual; abstract;
    procedure SwapBuffers; virtual; abstract;
    procedure SetSwapInterval(const aInterval: GLint); virtual; abstract;
    function GetSwapInterval: GLint; virtual; abstract;
    procedure Share(const aContext: TglcContext); virtual; abstract;
{$IFDEF fpc}
  private class var
    fMainContextThreadID: TThreadID;
  public
    class property MainContextThreadID: TThreadID read fMainContextThreadID;
{$ENDIF}
  public    
    class function MakePF(DoubleBuffered: boolean = true;
                          Stereo: boolean=false;
                          MultiSampling: TMultiSample=1;
                          ColorBits: Integer=32;
                          DepthBits: Integer=24;
                          StencilBits: Integer=0;
                          AccumBits: Integer=0;
                          AuxBuffers: Integer=0;
                          Layer: Integer=0): TglcContextPixelFormatSettings;
    class function MakeVersion(const aMajor, aMinor: Integer; const aForwardCompatible: Boolean): TglcContextVersionSettings;
    class function GetPlatformClass: TglcContextClass;
    class function ChangeDisplaySettings(const aWidth, aHeight,
      aBitPerPixel, aFreq: Integer; const aFlags: TglcDisplayFlags): Boolean; virtual; abstract;
    class function IsAnyContextActive: boolean; virtual;
  end;

implementation

uses
  {$IFDEF WINDOWS}
    uglcContextWGL
  {$ELSE}{$IFDEF WIN32}
    uglcContextWGL{$IFNDEF fpc}, Windows{$ENDIF}
  {$ENDIF}{$ENDIF}

  {$IFDEF LINUX}
    uglcContextGtk2GLX
  {$ENDIF}
  ;

{$IFNDEF fpc}
var
  fMainContextThreadID: TThreadID;
{$ENDIF}

procedure GlDebugCallbackARB(source: GLenum; type_: GLenum; id: GLuint; severity: GLenum; {%H-}length: GLsizei; const message_: PGLchar; {%H-}userParam: PGLvoid); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
var
  src, typ: String;
  sv: TSeverity;
begin
  case source of
    GL_DEBUG_SOURCE_API_ARB            : src:= 'API';
    GL_DEBUG_SOURCE_WINDOW_SYSTEM_ARB  : src:= 'WINDOW';
    GL_DEBUG_SOURCE_SHADER_COMPILER_ARB: src:= 'SHADER';
    GL_DEBUG_SOURCE_THIRD_PARTY_ARB    : src:= '3RDPARTY';
    GL_DEBUG_SOURCE_APPLICATION_ARB    : src:= 'APPLICATION';
    GL_DEBUG_SOURCE_OTHER_ARB          : src:= 'OTHER';
  end;
  src:= 'GL_' + src;

  case type_ of
    GL_DEBUG_TYPE_ERROR_ARB               : typ:= 'ERROR';
    GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR_ARB : typ:= 'DEPRECATED';
    GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR_ARB  : typ:= 'UNDEF BEHAV';
    GL_DEBUG_TYPE_PORTABILITY_ARB         : typ:= 'PORTABILITY';
    GL_DEBUG_TYPE_PERFORMANCE_ARB         : typ:= 'PERFORMANCE';
    GL_DEBUG_TYPE_OTHER_ARB               : typ:= 'OTHER';
  end;

  case severity of
    GL_DEBUG_SEVERITY_LOW_ARB:    sv := svLow;
    GL_DEBUG_SEVERITY_MEDIUM_ARB: sv := svMedium;
    GL_DEBUG_SEVERITY_HIGH_ARB:   sv := svHigh;
  end;

  TglcContext(userParam).LogMsg(sv, format('%s [%d] %s',[typ, id, message_]));
end;

procedure GlDebugCallbackAMD(id: GLuint; category: GLenum; severity: GLenum; {%H-}length: GLsizei; const message_: PGLchar; {%H-}userParam: PGLvoid); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
var
  src: String;
  sv: TSeverity;
begin
  case category of
    GL_DEBUG_CATEGORY_API_ERROR_AMD            : src:= 'API';
    GL_DEBUG_CATEGORY_WINDOW_SYSTEM_AMD        : src:= 'WINDOW';
    GL_DEBUG_CATEGORY_DEPRECATION_AMD          : src:= 'SHADER';
    GL_DEBUG_CATEGORY_UNDEFINED_BEHAVIOR_AMD   : src:= 'UNDEF BEHAV';
    GL_DEBUG_CATEGORY_PERFORMANCE_AMD          : src:= 'PERFORMANCE';
    GL_DEBUG_CATEGORY_SHADER_COMPILER_AMD      : src:= 'SHADER';
    GL_DEBUG_CATEGORY_APPLICATION_AMD          : src:= 'APPLICATION';
    GL_DEBUG_CATEGORY_OTHER_AMD                : src:= 'OTHER';
  end;
  src:= 'GL_' + src;

  case severity of
    GL_DEBUG_SEVERITY_LOW_AMD:    sv := svLow;
    GL_DEBUG_SEVERITY_MEDIUM_AMD: sv := svMedium;
    GL_DEBUG_SEVERITY_HIGH_AMD:   sv := svHigh;
  end;

  TglcContext(userParam).LogMsg(sv, format('[%d] %s',[id, message_]));
end;

function TglcContext.GetEnableVSync: Boolean;
begin
  result := fEnableVsync;
end;

procedure TglcContext.SetEnableVSync(aValue: Boolean);
begin
  fEnableVsync := aValue;
  if IsActive then begin
    if fEnableVsync then
      SetSwapInterval(1)
    else
      SetSwapInterval(0);
  end;
end;

procedure TglcContext.LogMsg(const aSeverity: TSeverity; const aMsg: String);
begin
  if Assigned(fLogEvent) then
    fLogEvent(self, aSeverity, aMsg);
end;

procedure TglcContext.SetDebugMode(const aEnable: Boolean);
begin
  // ARB Debug Output
  if GL_ARB_debug_output then begin
    glDebugMessageCallbackARB(@GlDebugCallbackARB, self);
    glDebugMessageControlARB(GL_DONT_CARE, GL_DONT_CARE, GL_DONT_CARE, 0, nil, aEnable);
    if aEnable then begin
      glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS_ARB);
      glDebugMessageInsertARB(GL_DEBUG_SOURCE_APPLICATION_ARB, GL_DEBUG_TYPE_OTHER_ARB, 0, GL_DEBUG_SEVERITY_LOW_ARB, -1, PGLchar('Attached ARB_debug_output'));
    end;

  // AMD Debug Output
  end else if GL_AMD_debug_output then begin
    glDebugMessageCallbackAMD(@GlDebugCallbackAMD, self);
    glDebugMessageEnableAMD(GL_DONT_CARE, GL_DONT_CARE, 0, nil, aEnable);
    if aEnable then
      glDebugMessageInsertAMD(GL_DEBUG_CATEGORY_OTHER_AMD, GL_DEBUG_SEVERITY_LOW_ARB, 0, -1, PGLchar('Attached ARB_debug_output'));
  end;
end;

procedure TglcContext.OpenContext;
begin
  fThreadID := GetCurrentThreadId;
  if fMainContextThreadID = 0 then
    fMainContextThreadID := fThreadID;
end;

class function TglcContext.MakePF(DoubleBuffered: boolean; Stereo: boolean; MultiSampling: TMultiSample; ColorBits: Integer;
  DepthBits: Integer; StencilBits: Integer; AccumBits: Integer; AuxBuffers: Integer; Layer: Integer): TglcContextPixelFormatSettings;
begin
  Result.DoubleBuffered:= DoubleBuffered;
  Result.Stereo:= Stereo;
  Result.MultiSampling:= MultiSampling;
  Result.ColorBits:= ColorBits;
  Result.DepthBits:= DepthBits;
  Result.StencilBits:= StencilBits;
  Result.AccumBits:= AccumBits;
  Result.AuxBuffers:= AuxBuffers;
  Result.Layer:= Layer;
end;

class function TglcContext.MakeVersion(const aMajor, aMinor: Integer; const aForwardCompatible: Boolean): TglcContextVersionSettings;
begin
  result.Major := aMajor;
  result.Minor := aMinor;
  result.ForwardCompatible := aForwardCompatible;
end;

class function TglcContext.GetPlatformClass: TglcContextClass;
begin
  Result := nil;
  {$IFDEF WINDOWS}
  Result:= TglcContextWGL;
  {$ELSE}{$IFDEF WIN32}
  Result:= TglcContextWGL;
  {$ENDIF}{$ENDIF}
  {$IFDEF LINUX}
  Result:= TglcContextGtk2GLX;
  {$ENDIF}
  if not Assigned(result) then
    raise EGLError.Create('unable to find suitabe context class');
end;

class function TglcContext.IsAnyContextActive: boolean;
begin
  Result:= GetPlatformClass.IsAnyContextActive;
end;

constructor TglcContext.Create(const aControl: TWinControl; const aPixelFormatSettings: TglcContextPixelFormatSettings);
begin
  inherited Create;
  fPixelFormatSettings := aPixelFormatSettings;
  FControl             := aControl;
  fThreadID            := 0;
  fEnableVsync         := false;
  fUseVersion          := false;
  InitOpenGL();
end;

constructor TglcContext.Create(const aControl: TWinControl; const aPixelFormatSettings: TglcContextPixelFormatSettings; const aVersionSettings: TglcContextVersionSettings);
begin
  Create(aControl, aPixelFormatSettings);
  fVersionSettings := aVersionSettings;
  fUseVersion      := true;
end;

destructor TglcContext.Destroy;
begin
  if (GetCurrentThreadId = fMainContextThreadID) then
    fMainContextThreadID := 0;
  CloseContext;
  inherited Destroy;
end;

procedure TglcContext.BuildContext;
begin
  OpenContext;
  Activate;
  ReadImplementationProperties;
  ReadExtensions;
  SetEnableVSync(fEnableVsync);
end;

procedure TglcContext.EnableDebugOutput(const aLogEvent: TLogEvent);
begin
  fLogEvent := aLogEvent;
  SetDebugMode(true);
end;

procedure TglcContext.DisableDebugOutput;
begin
  SetDebugMode(false);
end;

procedure TglcContext.CloseContext;
begin
  if fMainContextThreadID = fThreadID then
    fMainContextThreadID := 0;
end;

initialization
  {$IFDEF fpc}TglcContext.{$ENDIF}fMainContextThreadID := 0;

end.

