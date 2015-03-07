unit utsRendererOpenGLES;

{$mode objfpc}{$H+}
{.$DEFINE DEBUG}

interface

uses
  Classes, SysUtils,
  utsTextSuite, utsTypes, utsOpenGLUtils, dglOpenGLES;

type
  TtsRendererOpenGLES = class(TtsBaseOpenGL)
  private
    fModelViewMatrix: TtsMatrix4f;
    fProjMatrix: TtsMatrix4f;
    fShaderProgram: GLuint;
    fVBO: GLuint;
    fShader: GLuint;

    fCharPosLocation: GLint;
    fCharTexPosLocation: GLint;
    fCharOffsetLocation: GLint;

    procedure SetModelViewMatrix(aValue: TtsMatrix4f);
    procedure SetProjectionMatrix(aValue: TtsMatrix4f);
    procedure SetShaderProgram(aValue: GLuint);

    function LoadShader: GLuint;

    procedure UpdateUniformProjMat;
    procedure UpdateUniformModelMat;
    procedure UpdateUniformCharOffset;
  protected
    function  CreateNewTexture: PtsFontTexture; override;
    procedure FreeTexture(var aTexture: PtsFontTexture); override;
    procedure UploadTexData(const aCharRef: TtsCharRenderRefOpenGL;
      const aCharImage: TtsImage; const X, Y: Integer); override;

    procedure BeginRender; override;

    procedure SetDrawPos(const X, Y: Integer); override;
    procedure MoveDrawPos(const X, Y: Integer); override;
    procedure SetColor(const aColor: TtsColor4f); override;
    procedure Render(const aCharRef: TtsCharRenderRef; const aForcedWidth: Integer); override;
  public
    property ShaderProgram:    GLuint      read fShaderProgram   write SetShaderProgram;
    property ProjectionMatrix: TtsMatrix4f read fProjMatrix      write SetProjectionMatrix;
    property ModelViewMatrix:  TtsMatrix4f read fModelViewMatrix write SetModelViewMatrix;

    constructor Create(const aContext: TtsContext; const aFormat: TtsFormat);
    destructor Destroy; override;
  end;

implementation

type
  TVertex = packed record
    pos: array[0..1] of GLfloat;
    tex: array[0..1] of GLfloat;
  end;

const
  ATTRIB_LOCATION_POSITION = 0;
  ATTRIB_LOCATION_TEXCOORD = 1;
  ATTRIB_NAME_POSITION = 'inPosition';
  ATTRIB_NAME_TEXCOORD = 'inTexCoord';
  UNIFORM_NAME_TEXTURE      = 'uTexture';
  UNIFORM_NAME_MODELVIEWMAT = 'uModelViewMat';
  UNIFORM_NAME_PROJMAT      = 'uProjMat';
  UNIFORM_NAME_CHARTEXPOS   = 'uCharTexPos';
  UNIFORM_NAME_CHARPOS      = 'uCharPos';
  UNIFORM_NAME_CHAROFFSET   = 'uCharOffset';
  VERTEX_SHADER =
    'attribute vec2  inPosition;' +
    'attribute vec2  inTexCoord;' +
    'varying   vec2  vTexCoord;' +
    'uniform   mat4  uModelViewMat;' +
    'uniform   mat4  uProjMat;' +
    'uniform   mat4  uCharTexPos;' +
    'uniform   mat4  uCharPos;' +
    'uniform   ivec2 uCharOffset;' +
    'void main() {' +
    '  vec4 pos     = uCharPos * vec4(inPosition, 0.0, 1.0);' +
    '       pos    += vec4(uCharOffset, 0.0, 0.0);' +
    '  gl_Position  = uProjMat * uModelViewMat * pos;' +
    '  vTexCoord    = (uCharTexPos * vec4(inTexCoord, 0.0, 1.0)).st;' +
    '}';
  FRAGMENT_SHADER =
    'uniform sampler2D uTexture;' +
    'varying vec2      vTexCoord;' +
    'void main() {' +
    '  gl_FragColor = texture2D(uTexture, vTexCoord);' +
    '}';

  FORMAT_TYPES: array[TtsFormat] of packed record
    InternalFormat: GLenum;
    Format: GLenum;
    DataFormat: GLenum;
  end = (
    ( //tsFormatEmpty
      InternalFormat: 0;
      Format:         0;
      DataFormat:     0),
    ( //tsFormatRGBA8
      InternalFormat: GL_RGBA;
      Format:         GL_RGBA;
      DataFormat:     GL_UNSIGNED_BYTE),
    ( //tsFormatLumAlpha8
      InternalFormat: GL_LUMINANCE_ALPHA;
      Format:         GL_LUMINANCE_ALPHA;
      DataFormat:     GL_UNSIGNED_BYTE),
    ( //tsFormatAlpha8
      InternalFormat: GL_ALPHA;
      Format:         GL_ALPHA;
      DataFormat:     GL_UNSIGNED_BYTE),
    ( //tsFormatAlpha8
      InternalFormat: GL_LUMINANCE;
      Format:         GL_LUMINANCE;
      DataFormat:     GL_UNSIGNED_BYTE)
  );

  VBO_DATA: array[0..3] of TVertex = (
    (pos: (0.0, 0.0); tex: (0.0, 0.0)),
    (pos: (0.0, 1.0); tex: (0.0, 1.0)),
    (pos: (1.0, 0.0); tex: (1.0, 0.0)),
    (pos: (1.0, 1.0); tex: (1.0, 1.0))
  );

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsRendererOpenGLES///////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRendererOpenGLES.SetModelViewMatrix(aValue: TtsMatrix4f);
begin
  fModelViewMatrix := aValue;
  UpdateUniformModelMat;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRendererOpenGLES.SetProjectionMatrix(aValue: TtsMatrix4f);
begin
  fProjMatrix := aValue;
  UpdateUniformProjMat;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRendererOpenGLES.SetShaderProgram(aValue: GLuint);
begin
  if (fShaderProgram = aValue) then
    exit;

  fShaderProgram      := aValue;
  fCharPosLocation    := glGetUniformLocation(fShaderProgram, UNIFORM_NAME_CHARPOS);
  fCharTexPosLocation := glGetUniformLocation(fShaderProgram, UNIFORM_NAME_CHARTEXPOS);
  fCharOffsetLocation := glGetUniformLocation(fShaderProgram, UNIFORM_NAME_CHAROFFSET);

  UpdateUniformProjMat;
  UpdateUniformModelMat;
  UpdateUniformCharOffset;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsRendererOpenGLES.LoadShader: GLuint;

{$IFDEF DEBUG}
  procedure PrintShaderInfo(const aObj: GLuint);
  var
    msg: PChar;
    bLen: GLint;
    sLen: GLsizei;
  begin
    bLen := 0;
    glGetShaderiv(aObj, GL_INFO_LOG_LENGTH, @bLen);
    if bLen > 1 then begin
      GetMem(msg, bLen * SizeOf(Char));
      try
        glGetShaderInfoLog(aObj, bLen, @sLen, msg);
        WriteLn(String(msg));
      finally
        FreeMem(msg);
      end;
    end;
  end;

  procedure PrintProgramInfo(const aObj: GLuint);
  var
    msg: PChar;
    bLen: GLint;
    sLen: GLsizei;
  begin
    bLen := 0;
    glGetProgramiv(aObj, GL_INFO_LOG_LENGTH, @bLen);
    if bLen > 1 then begin
      GetMem(msg, bLen * SizeOf(Char));
      try
        glGetProgramInfoLog(aObj, bLen, @sLen, msg);
        WriteLn(String(msg));
      finally
        FreeMem(msg);
      end;
    end;
  end;
{$ENDIF}

  function CreateObj(const aCode: String; const aType: GLenum): GLuint;
  var
    code: PAnsiChar;
    len: GLint;
  begin
    result := glCreateShader(aType);
    len    := Length(aCode);
    code   := PAnsiChar(aCode);
    glShaderSource(result, 1, @code, @len);
    glCompileShader(result);
    {$IFDEF DEBUG}PrintShaderInfo(result);{$ENDIF}
  end;

var
  fragObj, vertObj: GLuint;
  uLoc: Integer;
begin
  result := glCreateProgram();
  vertObj := CreateObj(VERTEX_SHADER,   GL_VERTEX_SHADER);
  fragObj := CreateObj(FRAGMENT_SHADER, GL_FRAGMENT_SHADER);
  glAttachShader(result, vertObj);
  glAttachShader(result, fragObj);
  glLinkProgram(result);
{$IFDEF DEBUG}PrintProgramInfo(result);{$ENDIF}
  glBindAttribLocation(result, ATTRIB_LOCATION_POSITION, ATTRIB_NAME_POSITION);
  glBindAttribLocation(result, ATTRIB_LOCATION_TEXCOORD, ATTRIB_NAME_TEXCOORD);
  uLoc := glGetUniformLocation(result, UNIFORM_NAME_TEXTURE);
  if (uLoc >= 0) then begin
    glUseProgram(result);
    glUniform1i(uLoc, 0);
    glUseProgram(0);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRendererOpenGLES.UpdateUniformProjMat;
var
  loc: Integer;
begin
  loc := glGetUniformLocation(fShaderProgram, UNIFORM_NAME_PROJMAT);
  if (loc >= 0) then begin
    glUseProgram(fShaderProgram);
    glUniformMatrix4fv(loc, 1, false, @fProjMatrix[0, 0]);
    glUseProgram(0);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRendererOpenGLES.UpdateUniformModelMat;
var
  loc: Integer;
begin
  loc := glGetUniformLocation(fShaderProgram, UNIFORM_NAME_MODELVIEWMAT);
  if (loc >= 0) then begin
    glUseProgram(fShaderProgram);
    glUniformMatrix4fv(loc, 1, false, @fModelViewMatrix[0, 0]);
    glUseProgram(0);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRendererOpenGLES.UpdateUniformCharOffset;
begin
  if (fCharOffsetLocation >= 0) then begin
    glUseProgram(fShaderProgram);
    glUniform2iv(fCharOffsetLocation, 1, @RenderPos.x);
    glUseProgram(0);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsRendererOpenGLES.CreateNewTexture: PtsFontTexture;
begin
  new(result);
  try
    FillByte(result^, SizeOf(result^), 0);
    new(result^.Usage);
    FillByte(result^.Usage^, SizeOf(result^.Usage^), 0);
    result^.Size := TextureSize;

    glGenTextures(1, @result^.ID);
    glBindTexture(GL_TEXTURE_2D, result^.ID);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexImage2D(
      GL_TEXTURE_2D,
      0,
      FORMAT_TYPES[Format].InternalFormat,
      result^.Size,
      result^.Size,
      0,
      FORMAT_TYPES[Format].Format,
      FORMAT_TYPES[Format].DataFormat,
      nil);

    PushTexture(result);
  except
    if Assigned(result^.Usage) then
      Dispose(result^.Usage);
    Dispose(result);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRendererOpenGLES.FreeTexture(var aTexture: PtsFontTexture);
begin
  if Assigned(aTexture) then
    glDeleteTextures(1, @aTexture^.ID);
  inherited FreeTexture(aTexture);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRendererOpenGLES.UploadTexData(const aCharRef: TtsCharRenderRefOpenGL; const aCharImage: TtsImage; const X, Y: Integer);
begin
  glBindTexture(GL_TEXTURE_2D, aCharRef.TextureID);
  glPixelStorei(GL_UNPACK_ALIGNMENT, 4);
  glTexSubImage2D(GL_TEXTURE_2D, 0,
    x, y, aCharImage.Width, aCharImage.Height,
    FORMAT_TYPES[aCharImage.Format].Format,
    FORMAT_TYPES[aCharImage.Format].DataFormat,
    aCharImage.Data);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRendererOpenGLES.BeginRender;
begin
  inherited BeginRender;
  glColor4f(Color.r, Color.g, Color.b, Color.a);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRendererOpenGLES.SetDrawPos(const X, Y: Integer);
begin
  inherited SetDrawPos(X, Y);
  UpdateUniformCharOffset;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRendererOpenGLES.MoveDrawPos(const X, Y: Integer);
begin
  inherited MoveDrawPos(X, Y);
  UpdateUniformCharOffset;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRendererOpenGLES.SetColor(const aColor: TtsColor4f);
begin
  inherited SetColor(aColor);
  glColor4f(Color.r, Color.g, Color.b, Color.a);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRendererOpenGLES.Render(const aCharRef: TtsCharRenderRef; const aForcedWidth: Integer);
var
  ref: TtsCharRenderRefOpenGL;
  m: TtsMatrix4f;
begin
  if Assigned(aCharRef) and (aCharRef is TtsCharRenderRefOpenGL) then begin
    ref := (aCharRef as TtsCharRenderRefOpenGL);

    glBindTexture(GL_TEXTURE_2D, ref.TextureID);
    glBindBuffer(GL_ARRAY_BUFFER, fVBO);
    glEnableVertexAttribArray(ATTRIB_LOCATION_POSITION);
    glVertexAttribPointer(ATTRIB_LOCATION_POSITION, 2, GL_FLOAT, false, SizeOf(TVertex), Pointer(0));
    glEnableVertexAttribArray(ATTRIB_LOCATION_TEXCOORD);
    glVertexAttribPointer(ATTRIB_LOCATION_TEXCOORD, 2, GL_FLOAT, false, SizeOf(TVertex), Pointer(8));
    glUseProgram(fShaderProgram);

    if (fCharPosLocation >= 0) then begin
      if (aForcedWidth > 0) then begin
        m := ref.VertMat;
        m[0] := tsVector4f(aForcedWidth, 0, 0, 0);
        glUniformMatrix4fv(fCharPosLocation, 1, false, @m[0, 0]);
      end else
        glUniformMatrix4fv(fCharPosLocation, 1, false, @ref.VertMat[0, 0]);
    end;
    if (fCharTexPosLocation >= 0) then
      glUniformMatrix4fv(fCharTexPosLocation, 1, false, @ref.TexMat);

    glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);

    glUseProgram(0);
    glDisableVertexAttribArray(ATTRIB_LOCATION_TEXCOORD);
    glDisableVertexAttribArray(ATTRIB_LOCATION_POSITION);
    glBindTexture(GL_TEXTURE_2D, 0);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsRendererOpenGLES.Create(const aContext: TtsContext; const aFormat: TtsFormat);
var
  viewport: array[0..3] of Integer;
begin
  inherited Create(aContext, aFormat);

  glGenBuffers(1, @fVBO);
  glBindBuffer(GL_ARRAY_BUFFER, fVBO);
  glBufferData(GL_ARRAY_BUFFER, SizeOf(TVertex) * Length(VBO_DATA), @VBO_DATA[0].pos[0], GL_STATIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER, 0);

  glGetIntegerv(GL_VIEWPORT, @viewport);
  fProjMatrix := tsMatrix4f(
    tsVector4f(2 / viewport[2],              0.0,  0.0,  0.0),
    tsVector4f(            0.0, -2 / viewport[3],  0.0,  0.0),
    tsVector4f(            0.0,              0.0, -0.1,  0.0),
    tsVector4f(           -1.0,              1.0,  0.0,  1.0));
  fModelViewMatrix := TS_MATRIX_IDENTITY;
  fShader       := LoadShader;
  ShaderProgram := fShader;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsRendererOpenGLES.Destroy;
begin
  glDeleteBuffers(1, @fVBO);
  glDeleteProgram(fShader);
  inherited Destroy;
end;

end.
