unit utsRendererOpenGL;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs, dglOpenGL,
  utsTextSuite, utsTypes;

type
  TtsQuadPosF = array[0..3] of TtsPositionF;
  TtsCharRenderRefOpenGL = class(TtsCharRenderRef)
  private
    TextureID: GLint;           // ID of OpenGL texture where the char is stored in
    TexCoordSize: TtsPositionF; // size of the char in texture coords (0.0 - 1.0)
    TexCoordPos: TtsPositionF;  // position of the char in texture coords (0.0 - 1.0)
    VertexSize: TtsPositionF;   // size of the char in world coords
    VertexPos: TtsPositionF;    // size of the char in world coords
  public
    constructor Create;
  end;

  PtsTextureUsageItem = ^TtsTextureUsageItem;
  TtsTextureUsageItem = packed record
    children: array[0..3] of PtsTextureUsageItem;
  end;

  PtsTextureTreeItem = ^TtsTextureTreeItem;
  TtsTextureTreeItem = packed record
    value: SmallInt;
    children: array[0..1] of PtsTextureTreeItem;
    ref: TtsCharRenderRefOpenGL;
  end;

  PtsFontTexture = ^TtsFontTexture;
  TtsFontTexture = packed record
    ID: GLint;                      // OpenGL texture ID
    Usage: PtsTextureTreeItem ;     // tree of used texture space
    Next: PtsFontTexture;           // next texture in list
    Prev: PtsFontTexture;           // previouse texture in list
    Size: Integer;                  // size of this texture
    Count: Integer;                 // number of chars stored in this texture
  end;

  TtsRendererOpenGL = class(TtsRenderer)
  private
    fVBO: GLuint;
    fTextureSize: Integer;
    fColor: TtsColor4f;
    fRenderPos: TtsPosition;
    fIsRendering: Boolean;
    fFirstTexture: PtsFontTexture;
    fLastTexture: PtsFontTexture;

    function  CreateNewTexture: PtsFontTexture;
    procedure FreeTexture(var aTexture: PtsFontTexture);
    procedure FreeTextures(var aTexture: PtsFontTexture);
    procedure FreeTextureTreeItem(var aItem: PtsTextureTreeItem);
  protected
    function  CreateRenderRef(const aChar: TtsChar; const aCharImage: TtsImage): TtsCharRenderRef; override;
    procedure FreeRenderRef(const aCharRef: TtsCharRenderRef); override;

    procedure BeginRender; override;
    procedure EndRender; override;

    procedure SetDrawPos(const X, Y: Integer); override;
    function  GetDrawPos: TtsPosition; override;
    procedure MoveDrawPos(const X, Y: Integer); override;
    procedure SetColor(const aColor: TtsColor4f); override;
    procedure Render(const aCharRef: TtsCharRenderRef); override;
  public
    property TextureSize: Integer read fTextureSize write fTextureSize;

    constructor Create(const aContext: TtsContext; const aFormat: TtsFormat);
    destructor Destroy; override;
  end;

  EtsRendererOpenGL = class(EtsRenderer);

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
      DataFormat:     GL_UNSIGNED_BYTE)
  );

  VBO_DATA: array[0..3] of TVertex = (
    (pos: (0.0, 0.0); tex: (0.0, 0.0)),
    (pos: (0.0, 1.0); tex: (0.0, 1.0)),
    (pos: (1.0, 1.0); tex: (1.0, 1.0)),
    (pos: (1.0, 0.0); tex: (1.0, 0.0))
  );

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsCharRenderRefOpenGL////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsCharRenderRefOpenGL.Create;
begin
  inherited Create;
  TextureID := 0;
  FillByte(TexCoordPos,  SizeOf(TexCoordPos),  0);
  FillByte(TexCoordSize, SizeOf(TexCoordSize), 0);
  FillByte(VertexPos,    SizeOf(VertexPos),    0);
  FillByte(VertexSize,   SizeOf(VertexSize),   0);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsRendererOpenGL/////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsRendererOpenGL.CreateNewTexture: PtsFontTexture;
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

    result^.Prev := fLastTexture;
    if Assigned(fLastTexture) then
      fLastTexture^.Next := result
    else
      fFirstTexture := result;
    fLastTexture := result;
  except
    if Assigned(result^.Usage) then
      Dispose(result^.Usage);
    Dispose(result);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRendererOpenGL.FreeTexture(var aTexture: PtsFontTexture);
begin
  if not Assigned(aTexture) then
    exit;
  glDeleteTextures(1, @aTexture^.ID);
  FreeTextureTreeItem(aTexture^.Usage);
  if Assigned(aTexture^.Prev) then
    aTexture^.Prev^.Next := aTexture^.Next;
  if Assigned(aTexture^.Next) then
    aTexture^.Next^.Prev := aTexture^.Prev;
  Dispose(aTexture);
  aTexture := nil;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRendererOpenGL.FreeTextures(var aTexture: PtsFontTexture);
begin
  if not Assigned(aTexture) then
    exit;
  FreeTextures(aTexture^.Next);
  FreeTexture(aTexture);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRendererOpenGL.FreeTextureTreeItem(var aItem: PtsTextureTreeItem);
begin
  if not Assigned(aItem) then
    exit;
  FreeTextureTreeItem(aItem^.children[0]);
  FreeTextureTreeItem(aItem^.children[1]);
  Dispose(aItem);
  aItem := nil;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsRendererOpenGL.CreateRenderRef(const aChar: TtsChar; const aCharImage: TtsImage): TtsCharRenderRef;
var
  GlyphWidth, GlyphHeight: Integer;

  function InsertToTree(const aItem: PtsTextureTreeItem; const X1, Y1, X2, Y2: SmallInt; out X, Y: Integer): PtsTextureTreeItem;
  var
    w, h: Integer;
  begin
    result := nil;
    w := X2 - X1;
    h := Y2 - Y1;
    if not Assigned(aItem) or
       Assigned(aItem^.ref) or
       (w < GlyphWidth) or
       (h < GlyphHeight) then
          exit;

    if (aItem^.value > 0) then begin
      result := InsertToTree(aItem^.children[0], X1, Y1, X2, aItem^.value, X, Y);
      if not Assigned(result) then
        result := InsertToTree(aItem^.children[1], X1, aItem^.value, X2, Y2, X, Y);
    end else if (aItem^.value < 0) then begin
      result := InsertToTree(aItem^.children[0], X1, Y1, -aItem^.value, Y2, X, Y);
      if not Assigned(result) then
        result := InsertToTree(aItem^.children[1], -aItem^.value, Y1, X2, Y2, X, Y);
    end else if (w = GlyphWidth) and (h = GlyphHeight) then begin
      X      := X1;
      Y      := Y1;
      result := aItem;
    end else begin
      new(aItem^.children[0]);
      new(aItem^.children[1]);
      FillByte(aItem^.children[0]^, SizeOf(aItem^.children[0]^), 0);
      FillByte(aItem^.children[1]^, SizeOf(aItem^.children[1]^), 0);
      if (w - GlyphWidth) < (h - GlyphHeight) then begin
        aItem^.value := Y1 + GlyphHeight;
        result := InsertToTree(aItem^.children[0], X1, Y1, X2, aItem^.value, X, Y);
      end else begin
        aItem^.value := -(X1 + GlyphWidth);
        result := InsertToTree(aItem^.children[0], X1, Y1, -aItem^.value, Y2, X, Y)
      end;
    end;
  end;

  function AddToTexture(const aTexture: PtsFontTexture): TtsCharRenderRefOpenGL;
  var
    x, y: Integer;
    item: PtsTextureTreeItem;
  begin
    item := InsertToTree(aTexture^.Usage, 0, 0, aTexture^.Size, aTexture^.Size, x, y);
    if not Assigned(item) then
      raise EtsRendererOpenGL.Create('unable to add glyph to texture');
    item^.ref := TtsCharRenderRefOpenGL.Create;
    result    := item^.ref;

    // Text Coords
    result.TextureID      := aTexture^.ID;
    result.TexCoordPos.x  := x / aTexture^.Size;
    result.TexCoordPos.y  := y / aTexture^.Size;
    result.TexCoordSize.x := aCharImage.Width  / aTexture^.Size;
    result.TexCoordSize.y := aCharImage.Height / aTexture^.Size;

    // Vertex Coords
    result.VertexPos.x  := -aChar.GlyphRect.Left;
    result.VertexPos.y  := -aChar.GlyphRect.Top - aChar.GlyphOrigin.y;
    result.VertexSize.x := aCharImage.Width;
    result.VertexSize.y := aCharImage.Height;

    glBindTexture(GL_TEXTURE_2D, result.TextureID);
    glTexSubImage2D(GL_TEXTURE_2D, 0,
      x, y, aCharImage.Width, aCharImage.Height,
      FORMAT_TYPES[aCharImage.Format].Format,
      FORMAT_TYPES[aCharImage.Format].DataFormat,
      aCharImage.Data);
  end;

var
  tex: PtsFontTexture;
begin
  result := nil;
  if aCharImage.IsEmpty then
    exit;

  GlyphWidth  := aCharImage.Width  + 1;
  GlyphHeight := aCharImage.Height + 1;

  // try to add to existing texture
  tex := fFirstTexture;
  while Assigned(tex) and not Assigned(result) do begin
    result := AddToTexture(tex);
    tex    := tex^.Next;
  end;

  // create new texture
  if not Assigned(result) then begin
    if (aCharImage.Width > TextureSize) or (aCharImage.Height > TextureSize) then
      raise EtsRendererOpenGL.Create('char is to large to fit into a texture: ' + aChar.CharCode + ' (0x' + IntToHex(Ord(aChar.CharCode), 4) + ')');
    tex    := CreateNewTexture;
    result := AddToTexture(tex);
  end;

  if not Assigned(result) then
    raise EtsRendererOpenGL.Create('unable to creat render reference for char: ' + aChar.CharCode + ' (0x' + IntToHex(Ord(aChar.CharCode), 4) + ')');
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRendererOpenGL.FreeRenderRef(const aCharRef: TtsCharRenderRef);
var
  ref: TtsCharRenderRefOpenGL;
  tex: PtsFontTexture;

  function IsEmtpy(const aItem: PtsTextureTreeItem): Boolean;
  begin
    result :=
      Assigned(aItem) and
      not Assigned(aItem^.children[0]) and
      not Assigned(aItem^.children[1]) and
      not Assigned(aItem^.ref);
  end;

  function RemoveFromTree(const aItem: PtsTextureTreeItem; const X1, Y1, X2, Y2: Integer): Boolean;
  var
    w, h: Integer;
  begin
    w := X2 - X1;
    h := Y2 - Y1;
    if not Assigned(aItem) or
       (w < ref.VertexSize.x) or
       (h < ref.VertexSize.y) then
          exit;

    result := (aItem^.ref = ref);
    if not result then begin
      if (aItem^.value > 0) then begin
        result := result or RemoveFromTree(aItem^.children[0], X1, Y1, X2, aItem^.value);
        result := result or RemoveFromTree(aItem^.children[1], X1, aItem^.value, X2, Y2);
      end else if (aItem^.value < 0) then begin
        result := result or RemoveFromTree(aItem^.children[0], X1, Y1, -aItem^.value, Y2);
        result := result or RemoveFromTree(aItem^.children[1], -aItem^.value, Y1, X2, Y2);
      end;
    end else
      aItem^.ref := nil;

    if result and
       IsEmtpy(aItem^.children[0]) and
       IsEmtpy(aItem^.children[1]) then
    begin
      FreeTextureTreeItem(aItem^.children[0]);
      FreeTextureTreeItem(aItem^.children[1]);
      FillByte(aItem^, SizeOf(aItem^), 0);
    end;
  end;

begin
  try
    if not Assigned(aCharRef) or not (aCharRef is TtsCharRenderRefOpenGL) then
      exit;
    ref := (aCharRef as TtsCharRenderRefOpenGL);
    tex := fFirstTexture;
    while Assigned(tex) do begin
      if (tex^.ID = ref.TextureID) then begin
        if not RemoveFromTree(tex^.Usage, 0, 0, tex^.Size, tex^.Size) then
          raise EtsRendererOpenGL.Create('unable to remove render ref from texture');
        if IsEmtpy(tex^.Usage) then begin
          if (tex = fFirstTexture) then
            fFirstTexture := nil;
          FreeTexture(tex);
        end;
        tex := nil;
      end else
        tex := tex^.Next;
    end;
  finally
    if Assigned(aCharRef) then
      aCharRef.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRendererOpenGL.BeginRender;
begin
  inherited BeginRender;
  fIsRendering := true;
  fRenderPos.x := 0;
  fRenderPos.y := 0;
  glPushMatrix;
  glColor4fv(@fColor.arr[0]);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRendererOpenGL.EndRender;
begin
  if fIsRendering then begin
    glPopMatrix;
    fIsRendering := false;
  end;
  inherited EndRender;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRendererOpenGL.SetDrawPos(const X, Y: Integer);
begin
  fRenderPos.x := X;
  fRenderPos.y := Y;
  glPopMatrix;
  glPushMatrix;
  glTranslatef(X, Y, 0);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsRendererOpenGL.GetDrawPos: TtsPosition;
begin
  result := fRenderPos;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRendererOpenGL.MoveDrawPos(const X, Y: Integer);
begin
  fRenderPos.x := fRenderPos.x + X;
  fRenderPos.y := fRenderPos.y + Y;
  glTranslatef(X, Y, 0);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRendererOpenGL.SetColor(const aColor: TtsColor4f);
begin
  fColor := aColor;
  glColor4fv(@fColor.arr[0]);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRendererOpenGL.Render(const aCharRef: TtsCharRenderRef);
var
  ref: TtsCharRenderRefOpenGL;

  procedure RenderTreeItem(const aItem: PtsTextureTreeItem; const X1, Y1, X2, Y2: Integer);
  begin
    glBegin(GL_LINE_LOOP);
      glVertex2f(X1, Y1);
      glVertex2f(X2, Y1);
      glVertex2f(X2, Y2);
      glVertex2f(X1, Y2);
    glEnd;
    if (aItem^.value > 0) then begin
      RenderTreeItem(aItem^.children[0], X1, Y1, X2, aItem^.value);
      RenderTreeItem(aItem^.children[1], X1, aItem^.value, X2, Y2);
    end else if (aItem^.value < 0) then begin
      RenderTreeItem(aItem^.children[0], X1, Y1, -aItem^.value, Y2);
      RenderTreeItem(aItem^.children[1], -aItem^.value, Y1, X2, Y2);
    end;
  end;

begin
  if Assigned(aCharRef) and (aCharRef is TtsCharRenderRefOpenGL) then begin
    ref := (aCharRef as TtsCharRenderRefOpenGL);

    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, ref.TextureID);

    glMatrixMode(GL_TEXTURE);
    glPushMatrix;
    glLoadIdentity;
    glTranslatef(ref.TexCoordPos.x, ref.TexCoordPos.y, 0);
    glScalef(ref.TexCoordSize.x, ref.TexCoordSize.y, 1);

    glMatrixMode(GL_MODELVIEW);
    glPushMatrix;
    glTranslatef(ref.VertexPos.x, ref.VertexPos.y, 0);
    glScalef(ref.VertexSize.x, ref.VertexSize.y, 1);

    glBindBuffer(GL_ARRAY_BUFFER, fVBO);
    glEnableClientState(GL_VERTEX_ARRAY);
    glVertexPointer(2, GL_FLOAT, SizeOf(TVertex), Pointer(0));
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    glTexCoordPointer(2, GL_FLOAT, SizeOf(TVertex), Pointer(8));

    glDrawArrays(GL_QUADS, 0, 4);

    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    glDisableClientState(GL_VERTEX_ARRAY);

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
  fIsRendering := false;
  fFirstTexture    := nil;
  fLastTexture := nil;
  fTextureSize := 2048;
  fColor       := tsColor4f(1, 1, 1, 1);
  fRenderPos   := tsPosition(0, 0);

  glGenBuffers(1, @fVBO);
  glBindBuffer(GL_ARRAY_BUFFER, fVBO);
  glBufferData(GL_ARRAY_BUFFER, SizeOf(TVertex) * Length(VBO_DATA), @VBO_DATA[0].pos[0], GL_STATIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
end;

destructor TtsRendererOpenGL.Destroy;
begin
  glDeleteBuffers(1, @fVBO);
  FreeTextures(fFirstTexture);
  inherited Destroy;
end;

end.

