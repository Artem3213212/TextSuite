unit utsOpenGLUtils;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  utsTypes, utsRenderer, utsImage, utsContext, utsUtils, utsChar;

type
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsOpenGLRenderRef = class(TObject)
  public
    TextureID: Integer;
    Size: TtsPosition;
    TexMat: TtsMatrix4f;
    VertMat: TtsMatrix4f;
    constructor Create;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  PtsTextureUsageItem = ^TtsTextureUsageItem;
  TtsTextureUsageItem = packed record
    children: array[0..3] of PtsTextureUsageItem;
  end;

  PtsTextureTreeItem = ^TtsTextureTreeItem;
  TtsTextureTreeItem = packed record
    value: SmallInt;
    children: array[0..1] of PtsTextureTreeItem;
    ref: TtsOpenGLRenderRef;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  PtsFontTexture = ^TtsFontTexture;
  TtsFontTexture = packed record
    ID: Integer;                    // OpenGL texture ID
    Usage: PtsTextureTreeItem ;     // tree of used texture space
    Next: PtsFontTexture;           // next texture in list
    Prev: PtsFontTexture;           // previouse texture in list
    Size: Integer;                  // size of this texture
    Count: Integer;                 // number of chars stored in this texture
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsBaseOpenGL = class(TtsRenderer)
  private
    fTextureSize: Integer;
    fColor: TtsColor4f;
    fRenderPos: TtsPosition;
    fFirstTexture: PtsFontTexture;
    fLastTexture: PtsFontTexture;

    procedure FreeTextures(var aTexture: PtsFontTexture);
    procedure FreeTextureTreeItem(var aItem: PtsTextureTreeItem);
  protected
    property Color:     TtsColor4f  read fColor;
    property RenderPos: TtsPosition read fRenderPos;

    procedure PushTexture(const aTexture: PtsFontTexture);

    function  CreateNewTexture: PtsFontTexture; virtual;
    procedure FreeTexture(var aTexture: PtsFontTexture); virtual;

    procedure UploadTexData(const aCharRef: TtsOpenGLRenderRef; const aCharImage: TtsImage; const X, Y: Integer); virtual;
  public
    function CreateRenderRef(const aChar: TtsChar; const aImage: TtsImage): TtsRenderRef; override;
    procedure FreeRenderRef(const aRenderRef: TtsRenderRef); override;
  protected
    procedure BeginRender; override;
    procedure EndRender; override;
    procedure SetDrawPos(const aValue: TtsPosition); override;
    function GetDrawPos: TtsPosition; override;
    procedure MoveDrawPos(const aOffset: TtsPosition); override;
    procedure SetColor(const aValue: TtsColor4f); override;
  public
    property TextureSize: Integer read fTextureSize write fTextureSize;

    constructor Create(const aContext: TtsContext; const aFormat: TtsFormat);
    destructor Destroy; override;
  end;

  EtsRendererOpenGL = class(EtsRenderer);

implementation

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsOpenGLRenderRef////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsOpenGLRenderRef.Create;
begin
  inherited Create;
  TextureID := 0;
  FillChar(TexMat,  SizeOf(TexMat),  #0);
  FillChar(VertMat, SizeOf(VertMat), #0);
  FillChar(Size,    SizeOf(Size),    #0);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsBaseOpenGL/////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsBaseOpenGL.FreeTextures(var aTexture: PtsFontTexture);
begin
  if not Assigned(aTexture) then
    exit;
  FreeTextures(aTexture^.Next);
  FreeTexture(aTexture);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsBaseOpenGL.FreeTextureTreeItem(var aItem: PtsTextureTreeItem);
begin
  if not Assigned(aItem) then
    exit;
  FreeTextureTreeItem(aItem^.children[0]);
  FreeTextureTreeItem(aItem^.children[1]);
  Dispose(aItem);
  aItem := nil;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsBaseOpenGL.PushTexture(const aTexture: PtsFontTexture);
begin
  aTexture^.Prev := fLastTexture;
  if Assigned(fLastTexture) then
    fLastTexture^.Next := aTexture
  else
    fFirstTexture := aTexture;
  fLastTexture := aTexture;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsBaseOpenGL.CreateNewTexture: PtsFontTexture;
begin
  result := nil;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsBaseOpenGL.FreeTexture(var aTexture: PtsFontTexture);
begin
  if not Assigned(aTexture) then
    exit;
  FreeTextureTreeItem(aTexture^.Usage);
  if Assigned(aTexture^.Prev) then
    aTexture^.Prev^.Next := aTexture^.Next;
  if Assigned(aTexture^.Next) then
    aTexture^.Next^.Prev := aTexture^.Prev;
  Dispose(aTexture);
  if (fFirstTexture = aTexture) then begin
    fFirstTexture := nil;
    fLastTexture  := nil;
  end;
  aTexture := nil;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsBaseOpenGL.UploadTexData(const aCharRef: TtsOpenGLRenderRef; const aCharImage: TtsImage; const X,
  Y: Integer);
begin
  // DUMMY
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsBaseOpenGL.CreateRenderRef(const aChar: TtsChar; const aImage: TtsImage): TtsRenderRef;
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
      FillChar(aItem^.children[0]^, SizeOf(aItem^.children[0]^), #0);
      FillChar(aItem^.children[1]^, SizeOf(aItem^.children[1]^), #0);
      if (w - GlyphWidth) < (h - GlyphHeight) then begin
        aItem^.value := Y1 + GlyphHeight;
        result := InsertToTree(aItem^.children[0], X1, Y1, X2, aItem^.value, X, Y);
      end else begin
        aItem^.value := -(X1 + GlyphWidth);
        result := InsertToTree(aItem^.children[0], X1, Y1, -aItem^.value, Y2, X, Y)
      end;
    end;
  end;

  function AddToTexture(const aTexture: PtsFontTexture): TtsOpenGLRenderRef;
  var
    x, y, wChar, hChar, l, t: Integer;
    item: PtsTextureTreeItem;
  begin
    item := InsertToTree(aTexture^.Usage, 0, 0, aTexture^.Size, aTexture^.Size, x, y);
    if not Assigned(item) then
      raise EtsRendererOpenGL.Create('unable to add glyph to texture');

    item^.ref := TtsOpenGLRenderRef.Create;
    result    := item^.ref;

    wChar := aChar.GlyphMetric.GlyphRect.Right  - aChar.GlyphMetric.GlyphRect.Left;
    hChar := aChar.GlyphMetric.GlyphRect.Bottom - aChar.GlyphMetric.GlyphRect.Top;
    l     := aChar.GlyphMetric.GlyphRect.Left + x;
    t     := aChar.GlyphMetric.GlyphRect.Top  + y;
    result.TextureID := aTexture^.ID;
    result.Size      := tsPosition(aImage.Width, aImage.Height);
    result.TexMat := tsMatrix4f(
      tsVector4f(wChar  / aTexture^.Size,       0.0, 0.0, 0.0),
      tsVector4f(0.0,        hChar / aTexture^.Size, 0.0, 0.0),
      tsVector4f(0.0,                                0.0, 1.0, 0.0),
      tsVector4f(l / aTexture^.Size,  t / aTexture^.Size, 0.0, 1.0));
    result.VertMat := tsMatrix4f(
      tsVector4f(wChar, 0.0, 0.0, 0.0),
      tsVector4f(0.0, hChar, 0.0, 0.0),
      tsVector4f(0.0, 0.0, 1.0, 0.0),
      tsVector4f(aChar.GlyphMetric.GlyphOrigin.x, -aChar.GlyphMetric.GlyphOrigin.y, 0.0, 1.0));

    UploadTexData(result, aImage, x, y);
  end;

var
  tex: PtsFontTexture;
begin
  result := nil;
  if aImage.IsEmpty then
    exit;

  GlyphWidth  := aImage.Width  + 1;
  GlyphHeight := aImage.Height + 1;

  // try to add to existing texture
  tex := fFirstTexture;
  while Assigned(tex) and not Assigned(result) do begin
    result := AddToTexture(tex);
    tex    := tex^.Next;
  end;

  // create new texture
  if not Assigned(result) then begin
    if (aImage.Width > TextureSize) or (aImage.Height > TextureSize) then
      raise EtsRendererOpenGL.Create('char is to large to fit into a texture: (0x' + IntToHex(Ord(aChar.CharCode), 4) + ')');
    tex    := CreateNewTexture;
    result := AddToTexture(tex);
  end;

  if not Assigned(result) then
    raise EtsRendererOpenGL.Create('unable to creat render reference for char: (0x' + IntToHex(Ord(aChar.CharCode), 4) + ')');
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsBaseOpenGL.FreeRenderRef(const aRenderRef: TtsRenderRef);
var
  ref: TtsOpenGLRenderRef;
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
    result := false;
    w := X2 - X1;
    h := Y2 - Y1;
    if not Assigned(aItem) or
       (w < ref.Size.x) or
       (h < ref.Size.y) then
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
      FillChar(aItem^, SizeOf(aItem^), #0);
    end;
  end;

begin
  ref := nil;
  try
    if not Assigned(aRenderRef) then
      exit;
    ref := TtsOpenGLRenderRef(aRenderRef);
    tex := fFirstTexture;
    while Assigned(tex) do begin
      if (tex^.ID = ref.TextureID) then begin
        if not RemoveFromTree(tex^.Usage, 0, 0, tex^.Size, tex^.Size) then
          raise EtsRendererOpenGL.Create('unable to remove render ref from texture');
        if IsEmtpy(tex^.Usage) then
          FreeTexture(tex);
        tex := nil;
      end else
        tex := tex^.Next;
    end;
  finally
    FreeAndNil(ref);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsBaseOpenGL.BeginRender;
begin
  fRenderPos.x := 0;
  fRenderPos.y := 0;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsBaseOpenGL.EndRender;
begin
  // DUMMY
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsBaseOpenGL.SetDrawPos(const aValue: TtsPosition);
begin
  fRenderPos := aValue;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsBaseOpenGL.GetDrawPos: TtsPosition;
begin
  result := fRenderPos;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsBaseOpenGL.MoveDrawPos(const aOffset: TtsPosition);
begin
  fRenderPos.x := fRenderPos.x + aOffset.x;
  fRenderPos.y := fRenderPos.y + aOffset.y;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsBaseOpenGL.SetColor(const aValue: TtsColor4f);
begin
  fColor := aValue;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsBaseOpenGL.Create(const aContext: TtsContext; const aFormat: TtsFormat);
begin
  inherited Create(aContext, aFormat);
  fFirstTexture := nil;
  fLastTexture  := nil;
  fTextureSize  := 2048;
  fColor        := tsColor4f(1, 1, 1, 1);
  fRenderPos    := tsPosition(0, 0);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsBaseOpenGL.Destroy;
begin
  FreeTextures(fFirstTexture);
  inherited Destroy;
end;

end.

