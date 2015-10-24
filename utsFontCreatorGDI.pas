unit utsFontCreatorGDI;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  utsFont, utsFontCreator, utsTypes, utsGDI, utsImage, utsContext;

type
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsFontGDI = class(TtsFont)
  private
    fHandle: THandle;
    fMat2: TMat2;

    function GetGlyphIndex(const aCharCode: WideChar): Integer;
    procedure GetCharImageAANone(const aDC: HDC; const aCharCode: WideChar; const aImage: TtsImage; const aFormat: TtsFormat);
    procedure GetCharImageAANormal(const aDC: HDC; const aCharCode: WideChar; const aImage: TtsImage; const aFormat: TtsFormat);
  protected
    {%H-}constructor Create(const aHandle: THandle; const aCreator: TtsFontCreator; const aMetric: TtsFontMetric; const aNames: TtsFontNames);
  public
    procedure GetCharImage(const aCharCode: WideChar; const aCharImage: TtsImage; const aFormat: TtsFormat); override;
    function GetGlyphMetrics(const aCharCode: WideChar; out aGlyphOrigin, aGlyphSize: TtsPosition; out aAdvance: Integer): Boolean; override;

    destructor Destroy; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsFontRegistration = class(TObject)
  protected
    fIsRegistered: Boolean;
    fFontname: String;
    procedure UnregisterFont; virtual; abstract;
  public
    property IsRegistered: Boolean read fIsRegistered;
    property Fontname:     String  read fFontname;

    destructor Destroy; override;
  end;

  TtsFontRegistrationFile = class(TtsFontRegistration)
  private
    fFilename: String;
  protected
    procedure UnregisterFont; override;
  public
    constructor Create(const aFilename: String);
  end;

  TtsFontRegistrationStream = class(TtsFontRegistration)
  private
    fHandle: THandle;
  protected
    procedure UnregisterFont; override;
  public
    constructor Create(const aStream: TStream);
  end;

  TtsRegistredFontGDI = class(TtsFontGDI)
  private
    fRegistration: TtsFontRegistration;
  protected
    {%H-}constructor Create(const aRegistration: TtsFontRegistration; const aHandle: THandle;
      const aCreator: TtsFontCreator; const aMetric: TtsFontMetric; const aNames: TtsFontNames);
  public
    destructor Destroy; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsFontCreatorGDI = class(TtsFontCreator)
  private
    function CreateFont(const aFontname: String; const aSize: Integer; const aStyle: TtsFontStyles; const aAntiAliasing: TtsAntiAliasing; out aMetric: TtsFontMetric; out aNames: TtsFontNames): THandle;
  public
    function GetFontByName(const aFontname: String; const aSize: Integer; const aStyle: TtsFontStyles; const aAntiAliasing: TtsAntiAliasing): TtsFont; overload; override;
    function GetFontByFile(const aFilename: String; const aSize: Integer; const aStyle: TtsFontStyles; const aAntiAliasing: TtsAntiAliasing): TtsFont; overload; override;
    function GetFontByStream(const aStream: TStream; const aSize: Integer; const aStyle: TtsFontStyles; const aAntiAliasing: TtsAntiAliasing): TtsFont; overload; override;

    constructor Create(const aContext: TtsContext);
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  utsUtils;

type
  TT_OFFSET_TABLE = packed record
  	uMajorVersion: Word;
  	uMinorVersion: Word;
	  uNumOfTables: Word;
  	uSearchRange: Word;
  	uEntrySelector: Word;
	  uRangeShift: Word;
  end;


  TT_TABLE_DIRECTORY = packed record
  	TableName: Cardinal;     // table name
  	uCheckSum: Cardinal;  // Check sum
	  uOffset: Cardinal;    // Offset from beginning of file
	  uLength: Cardinal;    // length of the table in bytes
  end;


  TT_NAME_TABLE_HEADER = packed record
  	uFSelector: Word;     //format selector. Always 0
	  uNRCount: Word;       //Name Records count
	  uStorageOffset: Word; //Offset for strings storage, from start of the table
  end;

  TT_NAME_RECORD = packed record
  	uPlatformID: Word;
	  uEncodingID: Word;
	  uLanguageID: Word;
	  uNameID: Word;
	  uStringLength: Word;
	  uStringOffset: Word;  //from start of storage area
  end;

const
  NAME_ID_COPYRIGHT  = 0;
  NAME_ID_FACE_NAME  = 1;
  NAME_ID_STYLE_NAME = 2;
  NAME_ID_FULL_NAME  = 4;

  PLATFORM_ID_APPLE_UNICODE = 0;
  PLATFORM_ID_MACINTOSH     = 1;
  PLATFORM_ID_MICROSOFT     = 3;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TTF Utils/////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function SWAPWORD(x: Word): Word;
begin
  Result := x and $FF;
  Result := Result shl 8;
  Result := Result or (x shr 8);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function SWAPLONG(x: Cardinal): Cardinal;
begin
  Result := (x and $FF) shl 24;
  x := x shr 8;

  Result := Result or ((x and $FF) shl 16);
  x := x shr 8;

  Result := Result or ((x and $FF) shl 8);
  x := x shr 8;

  Result := Result or x;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function GetTTTableData(Stream: TStream; TableName: Cardinal; pBuff: Pointer; var Size: Integer): Boolean;
var
  Pos: Int64;
  OffsetTable: TT_OFFSET_TABLE;
  TableDir: TT_TABLE_DIRECTORY;
  Idx: Integer;
begin
  Result := False;

  Pos := Stream.Position;

  // Reading table header
  Stream.Read(OffsetTable{%H-}, sizeof(TT_OFFSET_TABLE));
  OffsetTable.uNumOfTables := SWAPWORD(OffsetTable.uNumOfTables);
  OffsetTable.uMajorVersion := SWAPWORD(OffsetTable.uMajorVersion);
  OffsetTable.uMinorVersion := SWAPWORD(OffsetTable.uMinorVersion);

  //check is this is a true type font and the version is 1.0
  if (OffsetTable.uMajorVersion <> 1) or (OffsetTable.uMinorVersion <> 0) then
    Exit;

  // seaching table with name
  for Idx := 0 to OffsetTable.uNumOfTables -1 do begin
    Stream.Read(TableDir{%H-}, sizeof(TT_TABLE_DIRECTORY));

    if (TableName = TableDir.TableName) then begin
      TableDir.uOffset := SWAPLONG(TableDir.uOffset);
      TableDir.uLength := SWAPLONG(TableDir.uLength);

      // copying tabledata
      if (pBuff <> nil) and (Size >= Integer(TableDir.uLength)) then begin
        Stream.Seek(TableDir.uOffset, soBeginning);
        Size := Stream.Read(pBuff^, TableDir.uLength);

        Result := (Size = Integer(TableDir.uLength));
      end else

      begin
        // restoring streamposition
        Stream.Position := Pos;

        Size := TableDir.uLength;
        Result := True;
      end;

      break;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function MakeTTTableName(const ch1, ch2, ch3, ch4: Char): Cardinal;
begin
  Result := ord(ch4) shl 24 or ord(ch3) shl 16 or ord(ch2) shl 8 or ord(ch1);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function GetTTString(pBuffer: Pointer; BufferSize: Integer; NameID, LanguageID: Cardinal; out Text: String): Boolean;
var
  pActBuffer: pByte;
  ttNTHeader: TT_NAME_TABLE_HEADER;
  ttRecord: TT_NAME_RECORD;
  Idx: Integer;
  Prio: Integer;

  procedure ExtractName;
  var
    pTempBuffer: pByte;
    pTemp: pWideChar;
    uStringLengthH2: Word;

    procedure SwapText(pText: pWideChar; Length: Word);
    begin
      while Length > 0 do begin
        pWord(pText)^ := SWAPWORD(pWord(pText)^);
        Inc(pText);
        Dec(Length);
      end;
    end;

  begin
    Result := True;

    ttRecord.uStringLength := SWAPWORD(ttRecord.uStringLength);
    ttRecord.uStringOffset := SWAPWORD(ttRecord.uStringOffset);

    uStringLengthH2 := ttRecord.uStringLength shr 1;

    pTempBuffer := pBuffer;
    Inc(pTempBuffer, ttNTHeader.uStorageOffset + ttRecord.uStringOffset);

    // Unicode
    if ((ttRecord.uPlatformID = PLATFORM_ID_MICROSOFT) and (ttRecord.uEncodingID in [0, 1])) or
       ((ttRecord.uPlatformID = PLATFORM_ID_APPLE_UNICODE) and (ttRecord.uEncodingID > 0)) then begin
      pTemp := tsStrAlloc(uStringLengthH2);
      try
        // uStringLengthH2 * 2 because possible buffer overrun
        Move(pTempBuffer^, pTemp^, uStringLengthH2 * 2);

        SwapText(pTemp, uStringLengthH2);

        WideCharLenToStrVar(pTemp, uStringLengthH2, Text);
      finally
        tsStrDispose(pTemp);
      end;
    end else

    // none unicode
    begin
      SetLength(Text, ttRecord.uStringLength);
      Move(pTempBuffer^, Text[1], ttRecord.uStringLength);
    end;
  end;

begin
  Result := False;

  pActBuffer := pBuffer;

  Move(pActBuffer^, ttNTHeader{%H-}, sizeof(TT_NAME_TABLE_HEADER));
  inc(pActBuffer, sizeof(TT_NAME_TABLE_HEADER));

  ttNTHeader.uNRCount := SWAPWORD(ttNTHeader.uNRCount);
  ttNTHeader.uStorageOffset := SWAPWORD(ttNTHeader.uStorageOffset);

  Prio := -1;

  for Idx := 0 to ttNTHeader.uNRCount -1 do begin
    Move(pActBuffer^, ttRecord, sizeof(TT_NAME_RECORD));
    Inc(pActBuffer, sizeof(TT_NAME_RECORD));

    ttRecord.uNameID := SWAPWORD(ttRecord.uNameID);

    if ttRecord.uNameID = NameID then begin
      ttRecord.uPlatformID := SWAPWORD(ttRecord.uPlatformID);
      ttRecord.uEncodingID := SWAPWORD(ttRecord.uEncodingID);
      ttRecord.uLanguageID := SWAPWORD(ttRecord.uLanguageID);

      // highest priority
      if (ttRecord.uPlatformID = PLATFORM_ID_MICROSOFT) then begin
        // system language
        if (ttRecord.uLanguageID = languageID) then begin
          if Prio <= 7 then begin
            ExtractName;

            Prio := 7;
          end;
        end else

        // english
        if (ttRecord.uLanguageID = 1033) then begin
          if Prio <= 6 then begin
            ExtractName;

            Prio := 6;
          end;
        end else

        // all else
        if Prio <= 5 then begin
          ExtractName;

          Prio := 5;
        end;
      end else

      // apple unicode
      if (ttRecord.uPlatformID = PLATFORM_ID_APPLE_UNICODE) then begin
        ExtractName;

        Prio := 4;
      end else

      // macintosh
      if (ttRecord.uPlatformID = PLATFORM_ID_MACINTOSH) then begin
        // english
        if (ttRecord.uLanguageID = 0) then begin
          if Prio <= 3 then begin
            ExtractName;

            Prio := 3;
          end;
        end else

        // all other
        begin
          ExtractName;

          Prio := 2;
        end;
      end else

      begin
        if Prio <= 1 then begin
          ExtractName;

          Prio := 1;
        end;
      end;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function GetTTFontFullNameFromStream(Stream: TStream; LanguageID: Cardinal): String;
var
  TableName: Cardinal;
  Buffer: Pointer;
  BufferSize: Integer;
begin
  TableName := MakeTTTableName('n', 'a', 'm', 'e');

  BufferSize := 0;
  if GetTTTableData(Stream, TableName, nil, BufferSize) then begin
    GetMem(Buffer, BufferSize);
    try
      if GetTTTableData(Stream, TableName, Buffer, BufferSize) then begin
        if not GetTTString(Buffer, BufferSize, NAME_ID_FULL_NAME, LanguageID, Result) then
          if not GetTTString(Buffer, BufferSize, NAME_ID_FACE_NAME, LanguageID, Result) then
            Result := '';
      end;
    finally
      FreeMem(Buffer);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function GetTTFontFullNameFromFile(const aFilename: String; const aLanguageID: Cardinal): String;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(aFilename, fmOpenRead or fmShareDenyWrite);
  try
    result := GetTTFontFullNameFromStream(fs, aLanguageID);
  finally
    fs.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsFontGDI////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFontGDI.GetGlyphIndex(const aCharCode: WideChar): Integer;
var
  DC: HDC;
  GCPRes: TGCPResultsW;
begin
  result := -1;
  DC := CreateCompatibleDC(0);
  try
    SelectObject(DC, fHandle);
    if Assigned(GetCharacterPlacementW) then begin
      FillChar(GCPRes{%H-}, SizeOf(GCPRes), #0);
      GetMem(GCPRes.lpGlyphs, SizeOf(Cardinal));
      try
        GCPRes.lStructSize := SizeOf(GCPRes);
        GCPRes.lpGlyphs^   := 0;
        GCPRes.nGlyphs     := 1;
        if (GetCharacterPlacementW(DC, @aCharCode, 1, GCP_MAXEXTENT, @GCPRes, 0) <> GDI_ERROR) and
           (GCPRes.nGlyphs = 1) and
           (GCPRes.lpGlyphs <> nil) then
        begin
          result := GCPRes.lpGlyphs^;
        end;
      finally
        FreeMem(GCPRes.lpGlyphs);
      end;
    end;
  finally
    DeleteDC(DC);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsFontGDI.GetCharImageAANone(const aDC: HDC; const aCharCode: WideChar; const aImage: TtsImage; const aFormat: TtsFormat);
var
  gm: TGlyphMetrics;
  GlyphIndex, srcW, srcX, w, h, x, y: Integer;
  Size, OutlineRes: Cardinal;
  Buffer, pSrc, pDst: PByte;

  procedure ExpandByte;
  var
    i, cnt, srcCnt: Integer;
    c: TtsColor4f;
  begin
    srcCnt := min(8, srcX);
    cnt    := min(8, x);
    for i := 1 to cnt do begin
      c := tsColor4f(1, 1, 1, 1);
      if ((pSrc^ and $80) > 0) then
        c.a := 1.0
      else
        c.a := 0.0;
      pSrc^ := (pSrc^ and not $80) shl 1;
      tsFormatMap(aFormat, pDst, c);
    end;
    dec(srcX, srcCnt);
    dec(x, cnt);
    inc(pSrc);
  end;

begin
  if (fMat2.eM11.value <> 1) then
    raise EtsException.Create('invalid value');
  FillChar(gm{%H-}, SizeOf(gm), #0);

  GlyphIndex := GetGlyphIndex(aCharCode);
  if (GlyphIndex < 0) then
    exit;

  Size := GetGlyphOutlineA(aDC, GlyphIndex, GGO_BITMAP or GGO_GLYPH_INDEX, @gm, 0, nil, @fMat2);
  if (Size = GDI_ERROR) or (Size = 0) then
    exit;

  GetMem(Buffer, Size);
  try
    OutlineRes := GetGlyphOutlineA(aDC, GlyphIndex, GGO_BITMAP or GGO_GLYPH_INDEX, @gm, Size, Buffer, @fMat2);
    if (OutlineRes = GDI_ERROR) then
      exit;
    w    := gm.gmBlackBoxX;
    h    := gm.gmBlackBoxY;
    srcW := (Integer(Size) div h) * 8;
    if (w <= 0) or (h <= 0) then
      exit;
    aImage.CreateEmpty(aFormat, w, h);
    pSrc := Buffer;
    for y := 0 to h-1 do begin
      pDst := aImage.Scanline[y];
      srcX := srcW;
      x    := w;
      while (srcX > 0) do
        ExpandByte;
    end;
  finally
    Freemem(Buffer);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsFontGDI.GetCharImageAANormal(const aDC: HDC; const aCharCode: WideChar; const aImage: TtsImage; const aFormat: TtsFormat);
var
  gm: TGlyphMetrics;
  OutlineRes: DWORD;
  GlyphIndex, tmp, Spacer, x, y, w, h: Integer;
  Size: Cardinal;
  Buffer, pSrc, pDst: PByte;

  procedure CopyPixel;
  var
    i: Integer;
    tmp, cnt: Cardinal;
    c: TtsColor4f;
  begin
    cnt := min(x, fMat2.eM11.value);
    tmp := 0;
    for i := 0 to cnt-1 do begin
      tmp := tmp + pSrc^;
      inc(pSrc, 1);
    end;
    dec(x, cnt);
    c := tsColor4f(1, 1, 1, tmp / $40);
    tsFormatMap(aFormat, pDst, c);
  end;

begin
  FillChar(gm{%H-}, SizeOf(gm), #0);

  GlyphIndex := GetGlyphIndex(aCharCode);
  if (GlyphIndex < 0) then
    exit;

  Size := GetGlyphOutlineA(aDC, GlyphIndex, GGO_GRAY8_BITMAP or GGO_GLYPH_INDEX, @gm, 0, nil, @fMat2);
  if (Size = GDI_ERROR) or (Size = 0) then
    exit;

  GetMem(Buffer, Size);
  try
    OutlineRes := GetGlyphOutlineA(aDC, GlyphIndex, GGO_GRAY8_BITMAP or GGO_GLYPH_INDEX, @gm, Size, Buffer, @fMat2);
    if (OutlineRes = GDI_ERROR) then
      exit;
    w   := Integer(gm.gmBlackBoxX) div fMat2.eM11.value;
    h   := gm.gmBlackBoxY;
    tmp := Integer(gm.gmBlackBoxX) mod fMat2.eM11.value;
    if (tmp <> 0) then
      w := w + fMat2.eM11.value - tmp;
    if (w <= 0) or (h <= 0) then
      exit;

    // spacer
    Spacer := gm.gmBlackBoxX mod 4;
    if (Spacer <> 0) then
      Spacer := 4 - Spacer;

    // copy image
    aImage.CreateEmpty(aFormat, w, h);
    pSrc := Buffer;
    for y := 0 to h-1 do begin
      pDst := aImage.Scanline[y];
      x    := gm.gmBlackBoxX;
      while (x > 0) do
        CopyPixel;
      inc(pSrc, Spacer);
    end;
  finally
    FreeMem(Buffer);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsFontGDI.Create(const aHandle: THandle; const aCreator: TtsFontCreator; const aMetric: TtsFontMetric; const aNames: TtsFontNames);
begin
  inherited Create(aCreator, aMetric, aNames);
  FillChar(fMat2, SizeOf(fMat2), #0);
  fMat2.eM11.value := 1;
  fMat2.eM22.value := 1;
  fHandle          := aHandle;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsFontGDI.GetCharImage(const aCharCode: WideChar; const aCharImage: TtsImage; const aFormat: TtsFormat);
var
  DC: HDC;
begin
  DC := CreateCompatibleDC(0);
  try
    SelectObject(DC, fHandle);
    case Metric.AntiAliasing of
      tsAANone:
        GetCharImageAANone(DC, aCharCode, aCharImage, aFormat);
      tsAANormal:
        GetCharImageAANormal(DC, aCharCode, aCharImage, aFormat);
    end;
  finally
    DeleteDC(DC);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFontGDI.GetGlyphMetrics(const aCharCode: WideChar; out aGlyphOrigin, aGlyphSize: TtsPosition; out aAdvance: Integer): Boolean;
var
  GlyphIndex: Integer;
  DC: HDC;
  gm: TGlyphMetrics;
  Size: Cardinal;
begin
  result := false;

  aGlyphOrigin.x := 0;
  aGlyphOrigin.x := 0;
  aGlyphSize.x   := 0;
  aGlyphSize.y   := 0;
  aAdvance       := 0;

  GlyphIndex := GetGlyphIndex(aCharCode);
  if (GlyphIndex < 0) then
    exit;

  DC := CreateCompatibleDC(0);
  try
    SelectObject(DC, fHandle);
    case Metric.AntiAliasing of
      tsAANone: begin
        Size := GetGlyphOutlineA(DC, GlyphIndex, GGO_BITMAP or GGO_GLYPH_INDEX, @gm, 0, nil, @fMat2);
      end;
      tsAANormal: begin
        Size := GetGlyphOutlineA(DC, GlyphIndex, GGO_GRAY8_BITMAP or GGO_GLYPH_INDEX, @gm, 0, nil, @fMat2);
      end;
    else
      Size := GDI_ERROR;
    end;

    if (Size = GDI_ERROR) then
      Size := GetGlyphOutlineA(DC, GlyphIndex, GGO_METRICS or GGO_GLYPH_INDEX, @gm, 0, nil, @fMat2);

    if (Size <> GDI_ERROR) then begin
      aGlyphOrigin.x := gm.gmptGlyphOrigin.x;
      aGlyphOrigin.y := gm.gmptGlyphOrigin.y;
      aGlyphSize.x   := gm.gmBlackBoxX;
      aGlyphSize.y   := gm.gmBlackBoxY;
      aAdvance       := gm.gmCellIncX;
      result         := true;
    end;
  finally
    DeleteDC(DC);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsFontGDI.Destroy;
begin
  DeleteObject(fHandle);
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsFontRegistration///////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsFontRegistration.Destroy;
begin
  if fIsRegistered then
    UnregisterFont;
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsFontRegistrationFile///////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsFontRegistrationFile.UnregisterFont;
begin
  if Assigned(RemoveFontResourceExA) then
    RemoveFontResourceExA(PAnsiChar(AnsiString(fFilename)), 0, nil)
  else if Assigned(RemoveFontResourceA) then
    RemoveFontResourceA(PAnsiChar(AnsiString(fFilename)));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsFontRegistrationFile.Create(const aFilename: String);
var
  lang: AnsiString;
begin
  inherited Create;
  fFilename := aFilename;

  // get Fontname
  SetLength(lang, 4);
  GetLocaleInfoA(LOCALE_USER_DEFAULT, LOCALE_ILANGUAGE, @lang[1], 4);
  fFontname := GetTTFontFullNameFromFile(aFilename, StrToInt('$' + String(lang)));

  // register font
  if Assigned(AddFontResourceExA) then
    fIsRegistered := (AddFontResourceExA(PAnsiChar(AnsiString(fFilename)), 0, nil) > 0)
  else if Assigned(AddFontResourceA) then
    fIsRegistered := (AddFontResourceA(PAnsiChar(AnsiString(fFilename))) > 0)
  else
    fIsRegistered := false;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsFontRegistrationStream/////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsFontRegistrationStream.UnregisterFont;
begin
  if Assigned(RemoveFontMemResourceEx) then
    RemoveFontMemResourceEx(fHandle);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsFontRegistrationStream.Create(const aStream: TStream);
var
  lang: AnsiString;
  ms: TMemoryStream;
  cnt: DWORD;
begin
  inherited Create;
  fHandle       := 0;
  fIsRegistered := false;

  // get Fontname
  SetLength(Lang, 4);
  GetLocaleInfoA(LOCALE_USER_DEFAULT, LOCALE_ILANGUAGE, @lang[1], 4);
  fFontname := GetTTFontFullNameFromStream(aStream, StrToInt('$' + String(Lang)));

  // register font
  ms := TMemoryStream.Create;
  try
    ms.CopyFrom(aStream, 0);
    if Assigned(AddFontMemResourceEx) then
      fHandle := AddFontMemResourceEx(ms.Memory, ms.Size, nil, @cnt);
    fIsRegistered := (fHandle > 0);
  finally
    FreeAndNil(ms);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsRegistredFontGDI///////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsRegistredFontGDI.Create(const aRegistration: TtsFontRegistration; const aHandle: THandle;
  const aCreator: TtsFontCreator; const aMetric: TtsFontMetric; const aNames: TtsFontNames);
begin
  inherited Create(aHandle, aCreator, aMetric, aNames);
  fRegistration := aRegistration;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsRegistredFontGDI.Destroy;
begin
  FreeAndNil(fRegistration);
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsFontCreatorGDI/////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFontCreatorGDI.CreateFont(const aFontname: String; const aSize: Integer; const aStyle: TtsFontStyles;
  const aAntiAliasing: TtsAntiAliasing; out aMetric: TtsFontMetric; out aNames: TtsFontNames): THandle;
var
  LogFont: TLogFontA;
  i: Integer;
  DC: HDC;
  TableName, BufSize: Cardinal;
  Buffer: PByte;
  Lang, tmpName: AnsiString;
  TextMetric: TTextMetricW;
  OutlineMetric: TOutlineTextmetricW;

  function _(e: Boolean; a, b: Integer): Integer;
  begin
    if e then
      result := a
    else
      result := b;
  end;

begin
  FillChar(aMetric{%H-}, SizeOf(aMetric), #0);
  aMetric.Size         := aSize;
  aMetric.Style        := aStyle;
  aMetric.AntiAliasing := aAntiAliasing;
  aNames.Fontname      := aFontname;

  // prepare font attribs
  FillChar(LogFont{%H-}, SizeOf(LogFont), #0);
  tmpName := AnsiString(aFontname);
  for i := 1 to min(Length(aFontname), Length(LogFont.lfFaceName)) do
    LogFont.lfFaceName[i-1] := tmpName[i];
  LogFont.lfCharSet   := DEFAULT_CHARSET;
  LogFont.lfHeight    := -aSize;
  LogFont.lfWeight    := _(tsStyleBold      in aStyle, FW_BOLD, FW_NORMAL);
  LogFont.lfItalic    := _(tsStyleItalic    in aStyle, 1, 0);
  LogFont.lfUnderline := _(tsStyleUnderline in aStyle, 1, 0);
  LogFont.lfQuality   := _(aAntiAliasing = tsAANormal, ANTIALIASED_QUALITY, NONANTIALIASED_QUALITY);

  result := CreateFontIndirectA(LogFont);
  DC     := CreateCompatibleDC(0);
  try try
    SelectObject(DC, result);
    TableName := MakeTTTableName('n', 'a', 'm', 'e');
    BufSize   := GetFontData(DC, TableName, 0, nil, 0);
    if (BufSize <> GDI_ERROR) then begin
      GetMem(Buffer, BufSize);
      try
        if (GetFontData(DC, TableName, 0, Buffer, BufSize) <> GDI_ERROR) then begin
          SetLength(Lang, 4);
          GetLocaleInfoA(LOCALE_USER_DEFAULT, LOCALE_ILANGUAGE, @Lang[1], 4);

          GetTTString(Buffer, BufSize, NAME_ID_COPYRIGHT,  StrToInt('$' + String(Lang)), aNames.Copyright);
          GetTTString(Buffer, BufSize, NAME_ID_FACE_NAME,  StrToInt('$' + String(Lang)), aNames.FaceName);
          GetTTString(Buffer, BufSize, NAME_ID_STYLE_NAME, StrToInt('$' + String(Lang)), aNames.StyleName);
          GetTTString(Buffer, BufSize, NAME_ID_FULL_NAME,  StrToInt('$' + String(Lang)), aNames.FullName);
        end;
      finally
        FreeMem(Buffer);
      end;
    end;

    if GetTextMetricsW(DC, TextMetric{%H-}) then begin
      aMetric.Ascent          := TextMetric.tmAscent;
      aMetric.Descent         := TextMetric.tmDescent;
      aMetric.ExternalLeading := TextMetric.tmExternalLeading;
      aMetric.DefaultChar     := TextMetric.tmDefaultChar;
    end;

    if (GetOutlineTextMetricsW(DC, SizeOf(OutlineMetric), OutlineMetric{%H-}) > 0) then begin
      aMetric.UnderlinePos  := OutlineMetric.otmsUnderscorePosition;
      aMetric.UnderlineSize := Min(1, OutlineMetric.otmsUnderscoreSize);
      aMetric.StrikeoutPos  := OutlineMetric.otmsStrikeoutPosition;
      aMetric.StrikeoutSize := Min(1, OutlineMetric.otmsStrikeoutSize);
    end;
  except
    DeleteObject(result);
    result := 0;
  end;
  finally
    DeleteDC(DC);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFontCreatorGDI.GetFontByName(const aFontname: String; const aSize: Integer; const aStyle: TtsFontStyles; const aAntiAliasing: TtsAntiAliasing): TtsFont;
var
  handle: THandle;
  metric: TtsFontMetric;
  names: TtsFontNames;
begin
  handle := CreateFont(aFontname, aSize, aStyle, aAntiAliasing, metric, names);
  if (handle = 0) then
    raise EtsException.Create('unable to create font from name: ' + aFontname);
  result := TtsFontGDI.Create(handle, self, metric, names);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFontCreatorGDI.GetFontByFile(const aFilename: String; const aSize: Integer; const aStyle: TtsFontStyles; const aAntiAliasing: TtsAntiAliasing): TtsFont;
var
  reg: TtsFontRegistrationFile;
  handle: THandle;
  metric: TtsFontMetric;
  names: TtsFontNames;
begin
  reg := TtsFontRegistrationFile.Create(aFilename);
  try
    if not reg.IsRegistered then
      raise EtsException.Create('unable to register font file: ' + aFilename);
    handle := CreateFont(reg.Fontname, aSize, aStyle, aAntiAliasing, metric, names);
    if (handle = 0) then
      raise EtsException.Create('unable to create font from file: ' + aFilename);
  except
    FreeAndNil(reg);
    raise;
  end;
  result := TtsRegistredFontGDI.Create(reg, handle, self, metric, names);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFontCreatorGDI.GetFontByStream(const aStream: TStream; const aSize: Integer; const aStyle: TtsFontStyles; const aAntiAliasing: TtsAntiAliasing): TtsFont;
var
  reg: TtsFontRegistrationStream;
  handle: THandle;
  metric: TtsFontMetric;
  names: TtsFontNames;
begin
  reg := TtsFontRegistrationStream.Create(aStream);
  if not reg.IsRegistered then
    raise EtsException.Create('unable to register font from stream');
  handle := CreateFont(reg.Fontname, aSize, aStyle, aAntiAliasing, metric, names);
  if (handle = 0) then
    raise EtsException.Create('unable to create font from stream: ' + reg.Fontname);
  result := TtsRegistredFontGDI.Create(reg, handle, self, metric, names);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsFontCreatorGDI.Create(const aContext: TtsContext);
begin
  inherited Create(aContext);
  InitGDI;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsFontCreatorGDI.Destroy;
begin
  inherited Destroy; // first free all fonts (managed by parent class)
  QuitGDI;
end;

end.

