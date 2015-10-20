unit utsTextBlock;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  utsUtils, utsTypes, utsFont, utsCharCache, utsContext;

type
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsLineItemType = (
    tsItemTypeUnknown,
    tsItemTypeFont,
    tsItemTypeColor,
    tsItemTypeText,
    tsItemTypeSpace,
    tsItemTypeLineBreak,
    tsItemTypeTab,
    tsItemTypeSpacing);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsLineFlag = (
    tsLastItemIsSpace,  // is set if the last item was a space item
    tsMetaValid,        // is set if the line meta data is valid
    tsAutoLineBreak     // is set if the linebreak was set automatically
  );
  TtsLineFlags = set of TtsLineFlag;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  PtsLineItem = ^TtsLineItem;
  TtsLineItem = packed record
    Next: PtsLineItem;
    Prev: PtsLineItem;
    ItemType: TtsLineItemType;
    case TtsLineItemType of
      tsItemTypeFont: (
        Font: TtsFont
      );
      tsItemTypeColor: (
        Color: TtsColor4f;
      );
      tsItemTypeText, tsItemTypeSpace: (
        Text: PWideChar;      // text of this item
        TextWidth: Integer;   // width of text (in pixel)
      );
      tsItemTypeSpacing: (
        Spacing: Integer;
      );
      tsItemTypeTab: (
        TabWidth: Integer; // with of tab (in pixel)
      );
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  PtsBlockLine = ^TtsBlockLine;
  TtsBlockLine = packed record
    Next: PtsBlockLine;
    First: PtsLineItem;
    Last: PtsLineItem;
    Flags: TtsLineFlags;

    meta: packed record
      Width: Integer;       // absolut width of this line
      Height: Integer;      // absolute height of this line
      Spacing: Integer;     // spacing between lines
      Ascent: Integer;      // text ascent
      SpaceCount: Integer;  // number of words in this line
    end;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsBlockRenderer = class(TtsRenderRefGenerator)
  private
    fCharCache: TtsCharCache;
  protected
    property CharCache: TtsCharCache read fCharCache;

    procedure BeginRender; virtual; abstract;
    procedure EndRender; virtual; abstract;

    function  GetDrawPos: TtsPosition; virtual; abstract;
    procedure SetDrawPos(const aValue: TtsPosition); virtual; abstract;
    procedure MoveDrawPos(const aOffset: TtsPosition); virtual; abstract;
    procedure SetColor(const aValue: TtsColor4f); virtual; abstract;
    procedure Render(const aRenderRef: TtsRenderRef; const aForcedWidth: Integer = 0); virtual; abstract;

  public
    constructor Create(const aContext: TtsContext; const aFormat: TtsFormat);
    destructor Destroy; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsTextBlock = class(TtsRefManager)
  private
    fRenderer: TtsBlockRenderer;

    fTop: Integer;
    fLeft: Integer;
    fWidth: Integer;
    fHeight: Integer;
    fFlags: TtsBlockFlags;
    fVertAlign: TtsVertAlignment;
    fHorzAlign: TtsHorzAlignment;
    fClipping: TtsClipping;

    fCurrentChars: TtsChars;
    fCurrentColor: TtsColor4f;
    fCurrentFont: TtsFont;
    fFirstLine: PtsBlockLine;
    fLastLine: PtsBlockLine;

    function GetRect: TtsRect;

    function PushLineItem(const aItem: PtsLineItem): Boolean;
    procedure PushSpacing(const aWidth: Integer);
    procedure PushNewLine;

    procedure FreeLineItem(var aItem: PtsLineItem);
    procedure FreeLineItems(var aItem: PtsLineItem);
    procedure FreeLines(var aItem: PtsBlockLine);

    function SplitText(aText: PWideChar): PtsLineItem;
    function SplitIntoLines(aItem: PtsLineItem): Boolean;
    procedure TrimSpaces(const aLine: PtsBlockLine);
    procedure UpdateLineMeta(const aLine: PtsBlockLine);
  public
    procedure ChangeFont(const aFont: TtsFont);
    procedure ChangeColor(const aColor: TtsColor4f);
  public
    property Rect:     TtsRect       read GetRect;
    property Width:    Integer       read fWidth;
    property Height:   Integer       read fHeight;
    property Flags:    TtsBlockFlags read fFlags;

    property Top:          Integer          read fTop          write fTop;
    property Left:         Integer          read fLeft         write fLeft;
    property VertAlign:    TtsVertAlignment read fVertAlign    write fVertAlign;
    property HorzAlign:    TtsHorzAlignment read fHorzAlign    write fHorzAlign;
    property Clipping:     TtsClipping      read fClipping     write fClipping;
    property CurrentColor: TtsColor4f       read fCurrentColor write ChangeColor;
    property CurrentFont:  TtsFont          read fCurrentFont  write ChangeFont;

    function GetActualBlockHeight: Integer;

    procedure TextOutA(const aText: PAnsiChar);
    procedure TextOutW(const aText: PWideChar);

    function GetTextWidthA(const aText: PAnsiChar): Integer;
    function GetTextWidthW(const aText: PWideChar): Integer;

    procedure Render;

    constructor Create(const aRenderer: TtsBlockRenderer; const aTop, aLeft, aWidth, aHeight: Integer; const aFlags: TtsBlockFlags);
    destructor Destroy; override;
  end;

implementation

uses
  math,
  utsChar;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsBlockRenderer//////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsBlockRenderer.Create(const aContext: TtsContext; const aFormat: TtsFormat);
begin
  inherited Create(aContext, aFormat);
  fCharCache := TtsCharCache.Create(self);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsBlockRenderer.Destroy;
begin
  FreeAndNil(fCharCache);
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsTextBlock//////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsTextBlock.GetRect: TtsRect;
begin
  result.Left   := fLeft;
  result.Top    := fTop;
  result.Right  := fLeft + fWidth;
  result.Bottom := fTop  + fHeight;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsTextBlock.PushLineItem(const aItem: PtsLineItem): Boolean;
begin
  result := false;
  if not Assigned(fLastLine) then
    PushNewLine;

  if not Assigned(fLastLine^.First) and
     (aItem^.ItemType in [tsItemTypeSpace, tsItemTypeSpacing]) then
         exit; // di not add line space or line spacing if line is empty

  if Assigned(fLastLine^.Last) then begin
    aItem^.Prev           := fLastLine^.Last;
    aItem^.Next           := nil;
    fLastLine^.Last^.Next := aItem;
    fLastLine^.Last       := aItem;
  end;

  if not Assigned(fLastLine^.First) then begin
    fLastLine^.First := aItem;
    fLastLine^.Last  := aItem;
  end;

  case aItem^.ItemType of
    tsItemTypeSpace, tsItemTypeText:
      fLastLine^.meta.Width := fLastLine^.meta.Width + aItem^.TextWidth;
    tsItemTypeSpacing:
      fLastLine^.meta.Width := fLastLine^.meta.Width + aItem^.Spacing;
    tsItemTypeTab:
      fLastLine^.meta.Width := fLastLine^.meta.Width + aItem^.TabWidth;
  end;
  result := true;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsTextBlock.PushSpacing(const aWidth: Integer);
var
  p: PtsLineItem;
begin
  if (aWidth <= 0) then
    exit;
  new(p);
  FillChar(p^, SizeOf(p^), #0);
  p^.ItemType := tsItemTypeSpacing;
  p^.Spacing  := aWidth;
  PushLineItem(p);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsTextBlock.PushNewLine;
var
  p: PtsBlockLine;
begin
  TrimSpaces(fLastLine);

  new(p);
  FillChar(p^, SizeOf(p^), #0);
  UpdateLineMeta(p);

  if Assigned(fLastLine) then begin
    fLastLine^.Next := p;
    fLastLine       := p;
  end;

  if not Assigned(fFirstLine) then begin
    fFirstLine := p;
    fLastLine  := p;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsTextBlock.FreeLineItem(var aItem: PtsLineItem);
begin
  if Assigned(aItem^.Prev) then
    aItem^.Prev^.Next := aItem^.Next;
  if Assigned(aItem^.Next) then
    aItem^.Next^.Prev := aItem^.Prev;
  case aItem^.ItemType of
    tsItemTypeText, tsItemTypeSpace:
      tsStrDispose(aItem^.Text);
  end;
  Dispose(aItem);
  aItem := nil;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsTextBlock.FreeLineItems(var aItem: PtsLineItem);
var
  p: PtsLineItem;
begin
  while Assigned(aItem) do begin
    p := aItem;
    aItem := aItem^.Next;
    FreeLineItem(p);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsTextBlock.FreeLines(var aItem: PtsBlockLine);
var
  p: PtsBlockLine;
begin
  while Assigned(aItem) do begin
    p := aItem;
    aItem := aItem^.Next;
    FreeLineItems(p^.First);
    p^.Last := nil;
    Dispose(p);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsTextBlock.SplitText(aText: PWideChar): PtsLineItem;
var
  TextBegin: PWideChar;
  TextLength: Integer;
  State: TtsLineItemType;
  LastItem: PtsLineItem;

  procedure AddItem(const aItem: PtsLineItem);
  begin
    if Assigned(result) then begin
      LastItem^.Next := aItem;
      aItem^.Prev    := LastItem;
      aItem^.Next    := nil;
      LastItem       := aItem;
    end;

    if not Assigned(result) then begin
      result   := aItem;
      LastItem := aItem;
    end;
  end;

  procedure ExtractWord;
  var
    p: PtsLineItem;
    Text: PWideChar;
  begin
    if (State = tsItemTypeUnknown) then
      exit;

    new(p);
    FillChar(p^, SizeOf(p^), #0);
    p^.ItemType := State;

    case State of
      tsItemTypeText, tsItemTypeSpace: begin
        p^.Text := tsStrAlloc(TextLength);
        Text    := p^.Text;
        while (TextBegin <> aText) do begin
          Text^ := TextBegin^;
          inc(Text,      1);
          inc(TextBegin, 1);
        end;
        AddItem(p);
      end;

      tsItemTypeLineBreak, tsItemTypeTab: begin
        AddItem(p);
      end;

    else
      Dispose(p);
    end;
    TextBegin  := aText;
    TextLength := 0;
  end;

begin
  result     := nil;
  LastItem   := nil;
  TextBegin  := aText;
  TextLength := 0;
  State      := tsItemTypeUnknown;

  if not Assigned(aText) then
    exit;

  while (aText^ <> #0) do begin
    case aText^ of

      // line breaks
      #$000D, #$000A: begin
        if (State <> tsItemTypeLineBreak) then begin
          ExtractWord;
          State := tsItemTypeLineBreak;
        end else if (TextBegin^ <> #13) or (aText^ <> #10) or (TextBegin + 1 < aText) then
          ExtractWord;
      end;

      // spaces
      #$0020: begin
        if (State <> tsItemTypeSpace) then
          ExtractWord;
        State := tsItemTypeSpace;
      end;

      // tabulator
      #$0009: begin
        ExtractWord;
        State := tsItemTypeTab;
      end;

    else
      if (State <> tsItemTypeText) then
        ExtractWord;
      State := tsItemTypeText;
    end;

    inc(aText,      1);
    inc(TextLength, 1);
  end;

  if (TextBegin <> aText) then
    ExtractWord;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsTextBlock.SplitIntoLines(aItem: PtsLineItem): Boolean;
var
  p: PtsLineItem;
  tab: Integer;
begin
  result := false;
  if not Assigned(fCurrentFont) then
    exit;

  result := true;
  while Assigned(aItem) do begin
    p := aItem;
    aItem := aItem^.Next;
    p^.Next := nil;
    p^.Prev := nil;

    if not Assigned(fLastLine) then
      PushNewLine;

    case p^.ItemType of
      tsItemTypeText, tsItemTypeSpace: begin
        // increment word counter
        if (p^.ItemType = tsItemTypeSpace) then begin
          if not (tsLastItemIsSpace in fLastLine^.Flags) then
            inc(fLastLine^.meta.SpaceCount, 1);
          Include(fLastLine^.Flags, tsLastItemIsSpace);
        end else
          Exclude(fLastLine^.Flags, tsLastItemIsSpace);

        // update and check line width
        p^.TextWidth := GetTextWidthW(p^.Text);
        if (tsBlockFlagWordWrap in fFlags) and
           (fLastLine^.meta.Width + p^.TextWidth > fWidth) then
        begin
          if (fLastLine^.meta.Width = 0) then begin
            if not PushLineItem(p) then // if is first word, than add anyway
              FreeLineItem(p);
            p := nil;
          end;
          include(fLastLine^.Flags, tsAutoLineBreak);
          PushNewLine;
        end;

        // add item
        if Assigned(p) then begin
          if not PushLineItem(p) then
            FreeLineItem(p);
          PushSpacing(fCurrentFont.CharSpacing);
        end;
      end;

      tsItemTypeLineBreak: begin
        if not PushLineItem(p) then
          FreeLineItem(p);
        PushNewLine;
      end;

      tsItemTypeTab: begin
        tab := fCurrentFont.TabWidth * fCurrentFont.Metric.Size;
        p^.TabWidth := (1 + fLastLine^.meta.Width div tab) * tab - fLastLine^.meta.Width;
        if not PushLineItem(p) then
          FreeLineItem(p);
      end;

    else
      raise EtsException.Create('unexpected line item');
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsTextBlock.TrimSpaces(const aLine: PtsBlockLine);

  procedure Trim(var aItem: PtsLineItem; const aMoveNext: Boolean);
  var
    tmp, p: PtsLineItem;
    IsFirst: Boolean;
  begin
    IsFirst := true;
    p       := aItem;
    while Assigned(p) do begin
      tmp := p;
      if aMoveNext then
        p := p^.Next
      else
        p := p^.Prev;

      case  tmp^.ItemType of
        tsItemTypeText: begin    //done
          break;
        end;

        tsItemTypeSpace,
        tsItemTypeSpacing: begin
          // update line meta
          if (tmp^.ItemType = tsItemTypeSpace) then begin
            aLine^.meta.Width := aLine^.meta.Width - tmp^.TextWidth;
            dec(aLine^.meta.SpaceCount, 1);
          end else
            aLine^.meta.Width := aLine^.meta.Width - tmp^.Spacing;

          FreeLineItem(tmp);
          if IsFirst then
            aItem := p;
        end;

      else
        IsFirst := false;
      end;
    end;
  end;

begin
  if not Assigned(aLine) then
    exit;
  Trim(aLine^.First, true);
  Trim(aLine^.Last,  false);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsTextBlock.UpdateLineMeta(const aLine: PtsBlockLine);
var
  metric: TtsTextMetric;
begin
  if not Assigned(fCurrentFont) or
     not Assigned(aLine) then
        exit;

  fCurrentFont.GetTextMetric(metric);
  if (tsMetaValid in aLine^.Flags) then begin
    aLine^.meta.Height := max(
      aLine^.meta.Height,
      metric.LineHeight);
    aLine^.meta.Spacing := max(
      aLine^.meta.Spacing,
      metric.LineSpacing);
    aLine^.meta.Ascent := max(
      aLine^.meta.Ascent,
      metric.Ascent);
  end else begin
    Include(aLine^.Flags, tsMetaValid);
    aLine^.meta.Height  := metric.LineHeight;
    aLine^.meta.Spacing := metric.LineSpacing;
    aLine^.meta.Ascent  := metric.Ascent;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsTextBlock.ChangeFont(const aFont: TtsFont);
var
  p: PtsLineItem;
begin
  if not Assigned(aFont) then
    exit;
  New(p);
  FillChar(p^, SizeOf(p^), #0);
  fCurrentFont  := aFont;
  fCurrentChars := fRenderer.fCharCache.Chars[fCurrentFont];
  p^.ItemType   := tsItemTypeFont;
  p^.Font       := fCurrentFont;
  PushLineItem(p);
  UpdateLineMeta(fLastLine);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsTextBlock.ChangeColor(const aColor: TtsColor4f);
var
  p: PtsLineItem;
begin
  New(p);
  FillChar(p^, SizeOf(p^), #0);
  fCurrentColor := aColor;
  p^.ItemType   := tsItemTypeColor;
  p^.Color      := fCurrentColor;
  PushLineItem(p);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsTextBlock.GetActualBlockHeight: Integer;
var
  line: PtsBlockLine;
begin
  result := 0;
  line   := fFirstLine;
  while Assigned(line) do begin
    result := result + line^.meta.Height;
    line := line^.Next;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsTextBlock.TextOutA(const aText: PAnsiChar);
var
  tmp: PWideChar;
begin
  tmp := fRenderer.Context.AnsiToWide(aText);
  try
    TextOutW(tmp);
  finally
    tsStrDispose(tmp);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsTextBlock.TextOutW(const aText: PWideChar);
var
  p: PtsLineItem;
begin
  p := SplitText(aText);
  if not SplitIntoLines(p) then
    FreeLineItems(p);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsTextBlock.GetTextWidthA(const aText: PAnsiChar): Integer;
begin
  result := 0;
  if Assigned(fCurrentChars) then
    result := fCurrentChars.GetTextWidthA(aText);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsTextBlock.GetTextWidthW(const aText: PWideChar): Integer;
begin
  result := 0;
  if Assigned(fCurrentChars) then
    result := fCurrentChars.GetTextWidthW(aText);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsTextBlock.Render;
var
  c: PWideChar;
  pos: TtsPosition;
  x, y, tmp, tab: Integer;
  ExtraSpaceTotal, ExtraSpaceActual: Single;
  r: TtsRect;
  line: PtsBlockLine;
  item: PtsLineItem;
  font: TtsFont;
  chars: TtsChars;
  char: TtsChar;
  metric: TtsTextMetric;
  draw: Boolean;

  procedure DrawItem;
  begin
    case item^.ItemType of
      tsItemTypeFont: begin
        font := item^.Font;
        font.GetTextMetric(metric);
        chars := fRenderer.fCharCache.Chars[font];
      end;

      tsItemTypeColor: begin
        fRenderer.SetColor(item^.Color);
      end;

      tsItemTypeText: begin
        if draw and Assigned(font) then begin
          c := item^.Text;
          while (c^ <> #0) do begin
            if Assigned(chars) then begin
              char := chars.AddChar(c^);
              if Assigned(char) then begin
                fRenderer.MoveDrawPos(tsPosition(0, -metric.BaseLineOffset));
                fRenderer.Render(char.RenderRef);
                fRenderer.MoveDrawPos(tsPosition(char.GlyphMetric.Advance + font.CharSpacing, metric.BaseLineOffset));
              end;
            end;
            inc(c);
          end;
        end;
      end;

      tsItemTypeSpace: begin
        if draw and Assigned(font) then begin
          ExtraSpaceActual := ExtraSpaceActual + ExtraSpaceTotal;
          c := item^.Text;
          while (c^ <> #0) do begin
            if Assigned(chars) then begin
              char := chars.AddChar(c^);
              if Assigned(char) then begin
                if (font.Metric.Style * [tsStyleUnderline, tsStyleStrikeout] <> []) then begin
                  fRenderer.MoveDrawPos(tsPosition(0, -metric.BaseLineOffset));
                  fRenderer.Render(char.RenderRef);
                  fRenderer.MoveDrawPos(tsPosition(char.GlyphMetric.Advance + font.CharSpacing, metric.BaseLineOffset));
                end else begin
                  fRenderer.MoveDrawPos(tsPosition(char.GlyphMetric.Advance + font.CharSpacing, 0));
                end;
              end;
            end;
            inc(c);
          end;

          tmp := Trunc(ExtraSpaceActual);
          ExtraSpaceActual := ExtraSpaceActual - tmp;
          if (font.Metric.Style * [tsStyleUnderline, tsStyleStrikeout] <> []) then begin
            if Assigned(chars) then begin
              char := chars.AddChar(#0);
              if Assigned(char) then
                fRenderer.Render(char.RenderRef, tmp);
              // TODO draw lines; maybe with a temporary created fake char or something like an empty char?
            end;
          end;
          fRenderer.MoveDrawPos(tsPosition(tmp, 0));
        end;
      end;

      tsItemTypeLineBreak: begin
        // because this should be the last item in a line, we have nothing to do here
      end;

      tsItemTypeTab: begin
        // get current x pos and round it to TabWidth
        pos := fRenderer.GetDrawPos;
        tab := font.TabWidth * font.Metric.Size;
        if (tab = 0) then
          tab := 1;
        pos.x := Left + (1 + (pos.x - Left) div tab) * tab;
        fRenderer.SetDrawPos(pos);
      end;

      tsItemTypeSpacing: begin
        fRenderer.MoveDrawPos(tsPosition(item^.Spacing, 0));
      end;
    end;
  end;

  procedure DrawLine;
  begin
    // check vertical clipping
    case Clipping of
      tsClipCharBorder, tsClipWordBorder:
        draw := (y + line^.meta.Height > r.Top) and (y < r.Bottom);
      tsClipCharComplete, tsClipWordComplete:
        draw := (y > r.Top) and (y + line^.meta.Height < r.Bottom);
    else
      draw := true;
    end;

    // check horizontal alignment
    x := r.Left;
    ExtraSpaceTotal  := 0;
    ExtraSpaceActual := 0;
    case HorzAlign of
      tsHorzAlignCenter:
        x := r.Left + (Width div 2) - (line^.meta.Width div 2);
      tsHorzAlignRight:
        x := r.Right - line^.meta.Width;
      tsHorzAlignJustify:
        if (tsAutoLineBreak in line^.Flags) and (line^.meta.SpaceCount > 0) then
          ExtraSpaceTotal := (Width - line^.meta.Width) / line^.meta.SpaceCount;
    end;

    if draw then
      fRenderer.SetDrawPos(tsPosition(x, y + line^.meta.Ascent));
    inc(y, line^.meta.Height + line^.meta.Spacing);
    item := line^.First;
    while Assigned(item) do begin
      DrawItem;
      item := item^.Next;
    end;
  end;

begin
  fRenderer.BeginRender;
  try
    // init variables
    y    := Top;
    r    := Rect;
    font := nil;
    line := fFirstLine;

    // check vertical alignment
    case VertAlign of
      tsVertAlignCenter:
        y := y + (Height div 2 - GetActualBlockHeight div 2);
      tsVertAlignBottom:
        y := y + (Height - GetActualBlockHeight);
    end;

    while Assigned(line) do begin
      DrawLine;
      line := line^.Next;
    end;
  finally
    fRenderer.EndRender;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsTextBlock.Create(const aRenderer: TtsBlockRenderer; const aTop, aLeft, aWidth, aHeight: Integer; const aFlags: TtsBlockFlags);
begin
  inherited Create(aRenderer);
  fRenderer := aRenderer;
  fTop      := aTop;
  fLeft     := aLeft;
  fWidth    := aWidth;
  fHeight   := aHeight;
  fFlags    := aFlags;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsTextBlock.Destroy;
begin
  FreeLines(fFirstLine);
  fLastLine := nil;
  inherited Destroy;
end;

end.

