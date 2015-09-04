unit utsRendererOpenGL;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  utsTextSuite, utsTypes, utsOpenGLUtils, dglOpenGL;

type
  TtsRendererOpenGL = class(TtsBaseOpenGL)
  private
    fVBO: GLuint;
    fIsRendering: Boolean;
  protected
    function  CreateNewTexture: PtsFontTexture; override;
    procedure FreeTexture(var aTexture: PtsFontTexture); override;
    procedure UploadTexData(const aCharRef: TtsCharRenderRefOpenGL;
      const aCharImage: TtsImage; const X, Y: Integer); override;

    procedure BeginRender; override;
    procedure EndRender; override;

    procedure SetDrawPos(const X, Y: Integer); override;
    procedure MoveDrawPos(const X, Y: Integer); override;
    procedure SetColor(const aColor: TtsColor4f); override;
    procedure Render(const aCharRef: TtsCharRenderRef; const aForcedWidth: Integer); override;
  public
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
      InternalFormat: GL_RGBA8;
      Format:         GL_RGBA;
      DataFormat:     GL_UNSIGNED_BYTE),
    ( //tsFormatLumAlpha8
      InternalFormat: GL_LUMINANCE8_ALPHA8;
      Format:         GL_LUMINANCE_ALPHA;
      DataFormat:     GL_UNSIGNED_BYTE),
    ( //tsFormatAlpha8
      InternalFormat: GL_ALPHA8;
      Format:         GL_ALPHA;
      DataFormat:     GL_UNSIGNED_BYTE),
    ( //tsFormatAlpha8
      InternalFormat: GL_LUMINANCE8;
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
//TtsRendererOpenGL/////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsRendererOpenGL.CreateNewTexture: PtsFontTexture;
begin
  new(result);
  try
    FillChar(result^, SizeOf(result^), #0);
    new(result^.Usage);
    FillChar(result^.Usage^, SizeOf(result^.Usage^), #0);
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
procedure TtsRendererOpenGL.FreeTexture(var aTexture: PtsFontTexture);
begin
  if Assigned(aTexture) then
    glDeleteTextures(1, @aTexture^.ID);
  inherited FreeTexture(aTexture);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRendererOpenGL.UploadTexData(const aCharRef: TtsCharRenderRefOpenGL; const aCharImage: TtsImage; const X, Y: Integer);
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
procedure TtsRendererOpenGL.BeginRender;
begin
  inherited BeginRender;
  fIsRendering := true;
  glPushMatrix;
  glColor4fv(@Color.arr[0]);

  glBindBuffer(GL_ARRAY_BUFFER, fVBO);
  glEnableClientState(GL_VERTEX_ARRAY);
  glVertexPointer(2, GL_FLOAT, SizeOf(TVertex), Pointer(0));
  glEnableClientState(GL_TEXTURE_COORD_ARRAY);
  glTexCoordPointer(2, GL_FLOAT, SizeOf(TVertex), Pointer(8));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRendererOpenGL.EndRender;
begin
  if fIsRendering then begin
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    glDisableClientState(GL_VERTEX_ARRAY);
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    glPopMatrix;
    fIsRendering := false;
  end;
  inherited EndRender;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRendererOpenGL.SetDrawPos(const X, Y: Integer);
begin
  inherited SetDrawPos(X, Y);
  glPopMatrix;
  glPushMatrix;
  glTranslatef(X, Y, 0);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRendererOpenGL.MoveDrawPos(const X, Y: Integer);
begin
  inherited MoveDrawPos(X, Y);
  glTranslatef(X, Y, 0);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRendererOpenGL.SetColor(const aColor: TtsColor4f);
begin
  inherited SetColor(aColor);
  glColor4fv(@Color.arr[0]);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRendererOpenGL.Render(const aCharRef: TtsCharRenderRef; const aForcedWidth: Integer);
var
  ref: TtsCharRenderRefOpenGL;
  m: TtsMatrix4f;
begin
  if Assigned(aCharRef) and (aCharRef is TtsCharRenderRefOpenGL) then begin
    ref := (aCharRef as TtsCharRenderRefOpenGL);

    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, ref.TextureID);

    glMatrixMode(GL_TEXTURE);
    glPushMatrix;
    glLoadIdentity;
    glMultMatrixf(@ref.TexMat[0, 0]);

    glMatrixMode(GL_MODELVIEW);
    glPushMatrix;
    if (aForcedWidth > 0) then begin
      m := ref.VertMat;
      m[0] := tsVector4f(aForcedWidth, 0, 0, 0);
      glMultMatrixf(@m[0, 0]);
    end else
      glMultMatrixf(@ref.VertMat[0, 0]);

    glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);

    glMatrixMode(GL_TEXTURE);
    glPopMatrix;
    glMatrixMode(GL_MODELVIEW);
    glPopMatrix;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsRendererOpenGL.Create(const aContext: TtsContext; const aFormat: TtsFormat);
begin
  inherited Create(aContext, aFormat);
  fIsRendering  := false;
  glGenBuffers(1, @fVBO);
  glBindBuffer(GL_ARRAY_BUFFER, fVBO);
  glBufferData(GL_ARRAY_BUFFER, SizeOf(TVertex) * Length(VBO_DATA), @VBO_DATA[0].pos[0], GL_STATIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
end;

destructor TtsRendererOpenGL.Destroy;
begin
  glDeleteBuffers(1, @fVBO);
  inherited Destroy;
end;

end.

