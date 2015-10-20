unit utsUtils;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Contnrs,
  utsTypes;

type
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsRefManager = class(TObject)
  private
    fMasterRef: TtsRefManager;
    fSlaveRefs: TObjectList;
  protected
    procedure AddSlave(const aSlave: TtsRefManager); virtual;
    procedure DelSlave(const aSlave: TtsRefManager); virtual;
  public
    constructor Create(const aMaster: TtsRefManager);
    destructor Destroy; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsMultiMasterRefManager = class(TtsRefManager)
  private
    fMasterRefs: TObjectList;
  public
    procedure AddMaster(const aMaster: TtsRefManager); virtual;
    procedure DelMaster(const aMaster: TtsRefManager); virtual;

    constructor Create(const aMaster: TtsRefManager);
    destructor Destroy; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsKernel1DItem = packed record
    Offset: Integer;
    Value: Single;
  end;

  TtsKernel1D = class
  public
    Size: Integer;
    Items: array of TtsKernel1DItem;
    ItemCount: Integer;
    constructor Create(const aRadius, aStrength: Single);
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsKernel2DItem = packed record
    OffsetX: Integer;
    OffsetY: Integer;
    Value: Double;
    DataOffset: Integer;
  end;

  TtsKernel2D = class
  public
    SizeX: Integer;
    SizeY: Integer;

    MidSizeX: Integer;
    MidSizeY: Integer;

    ValueSum: Double;

    Items: array of TtsKernel2DItem;
    ItemCount: Integer;

    constructor Create(const aRadius, aStrength: Single);
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  EtsException = class(Exception);
  EtsRenderer = class(EtsException);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  function tsColor4f(r, g, b, a: Single): TtsColor4f;
  function tsPosition(const x, y: Integer): TtsPosition;
  function tsRect(const l, t, r, b: Integer): TtsRect; overload;
  function tsRect(const aTopLeft, aBottomRight: TtsPosition): TtsRect; overload;
  function tsVector4f(X, Y, Z, W: Single): TtsVector4f;
  function tsMatrix4f(X, Y, Z, P: TtsVector4f): TtsMatrix4f;

  function  tsFormatSize(const aFormat: TtsFormat): Integer;
  procedure tsFormatMap(const aFormat: TtsFormat; var aData: PByte; const aColor: TtsColor4f);
  procedure tsFormatUnmap(const aFormat: TtsFormat; var aData: PByte; out aColor: TtsColor4f);

  function tsBlendValueIgnore(const aSrc, aDst: Single): Single;
  function tsBlendValueReplace(const aSrc, aDst: Single): Single;
  function tsBlendValueModulate(const aSrc, aDst: Single): Single;

  function tsBlendColorAlpha(const aSrc, aDst: TtsColor4f): TtsColor4f;
  function tsBlendColorAdditive(const aSrc, aDst: TtsColor4f): TtsColor4f;
  function tsBlendColorAdditiveAlpha(const aSrc, aDst: TtsColor4f): TtsColor4f;

  function  tsStrAlloc(aSize: Cardinal): PWideChar;
  function  tsStrNew(const aText: PWideChar): PWideChar;
  procedure tsStrDispose(const aText: PWideChar);
  function  tsStrLength(aText: PWideChar): Cardinal;
  function  tsStrCopy(aDst, aSrc: PWideChar): PWideChar;

  function  tsAnsiToWide(aDst: PWideChar; const aSize: Integer; aSrc: PAnsiChar; const aCodePage: TtsCodePage; const aDefaultChar: WideChar): Integer;
  function  tsISO_8859_1ToWide(aDst: PWideChar; const aSize: Integer; aSrc: PAnsiChar): Integer;
  function  tsUTF8ToWide(aDst: PWideChar; const aSize: Integer; const aSrc: PAnsiChar; const aDefaultChar: WideChar): Integer;
  function  tsUTFBE16ToWide(aDst: PWideChar; const aDstSize: Integer; aSrc: PByte; aSrcSize: Integer; const aDefaultChar: WideChar): Integer;
  function  tsAnsiSBCDToWide(aDst: PWideChar; const aSize: Integer; aSrc: PAnsiChar; const aCodePage: TtsCodePage; const aDefaultChar: WideChar): Integer;

implementation

uses
  math,
  utsConstants;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsRefManager/////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRefManager.AddSlave(const aSlave: TtsRefManager);
begin
  if Assigned(fSlaveRefs) then
    fSlaveRefs.Add(aSlave);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsRefManager.DelSlave(const aSlave: TtsRefManager);
begin
  if Assigned(fSlaveRefs) then
    fSlaveRefs.Remove(aSlave);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsRefManager.Create(const aMaster: TtsRefManager);
begin
  inherited Create;
  fMasterRef := aMaster;
  fSlaveRefs := TObjectList.Create(false);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsRefManager.Destroy;
var
  m: TtsRefManager;
begin
  fSlaveRefs.OwnsObjects := true;
  FreeAndNil(fSlaveRefs);
  m := fMasterRef;
  fMasterRef := nil;
  if Assigned(m) then
    m.DelSlave(self);
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsMultiMasterRefManager//////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsMultiMasterRefManager.AddMaster(const aMaster: TtsRefManager);
begin
  if Assigned(fMasterRefs) then begin
    fMasterRefs.Add(aMaster);
    aMaster.AddSlave(self);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsMultiMasterRefManager.DelMaster(const aMaster: TtsRefManager);
begin
  if Assigned(fMasterRefs) then begin
    if (fMasterRefs.Remove(aMaster) >= 0) then
      aMaster.DelSlave(self);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsMultiMasterRefManager.Create(const aMaster: TtsRefManager);
begin
  inherited Create(aMaster);
  fMasterRefs := TObjectList.Create(false);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TtsMultiMasterRefManager.Destroy;
var
  i: Integer;
begin
  for i := fMasterRefs.Count-1 downto 0 do
    DelMaster(fMasterRefs[i] as TtsRefManager);
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Helper Methods////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsColor4f(r, g, b, a: Single): TtsColor4f;
begin
  result.r := r;
  result.g := g;
  result.b := b;
  result.a := a;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsPosition(const x, y: Integer): TtsPosition;
begin
  result.x := x;
  result.y := y;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsRect(const l, t, r, b: Integer): TtsRect;
begin
  result.Left   := l;
  result.Top    := t;
  result.Right  := r;
  result.Bottom := b;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsRect(const aTopLeft, aBottomRight: TtsPosition): TtsRect;
begin
  result.TopLeft     := aTopLeft;
  result.BottomRight := aBottomRight;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsVector4f(X, Y, Z, W: Single): TtsVector4f;
begin
  result[0] := X;
  result[1] := Y;
  result[2] := Z;
  result[3] := W;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsMatrix4f(X, Y, Z, P: TtsVector4f): TtsMatrix4f;
begin
  result[0] := X;
  result[1] := Y;
  result[2] := Z;
  result[3] := P;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsFormatSize(const aFormat: TtsFormat): Integer;
begin
  case aFormat of
    tsFormatRGBA8:      result := 4;
    tsFormatLumAlpha8:  result := 2;
    tsFormatAlpha8:     result := 1;
    tsFormatLum8:       result := 1;
  else
    result := 0;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure tsFormatMap(const aFormat: TtsFormat; var aData: PByte; const aColor: TtsColor4f);
var
  i: Integer;
  s: Single;
begin
  case aFormat of
    tsFormatRGBA8: begin
      for i := 0 to 3 do begin
        aData^ := Trunc($FF * min(aColor.arr[i], 1.0));
        inc(aData);
      end;
    end;

    tsFormatLumAlpha8: begin
      s := 0.30 * min(aColor.r, 1.0) +
           0.59 * min(aColor.g, 1.0) +
           0.11 * min(aColor.b, 1.0);
      aData^ := Trunc($FF * s);
      inc(aData);
      aData^ := Trunc($FF * min(aColor.a, 1.0));
      inc(aData);
    end;

    tsFormatAlpha8: begin
      aData^ := Trunc($FF * min(aColor.a, 1.0));
      inc(aData);
    end;

    tsFormatLum8: begin
      s := 0.30 * min(aColor.r, 1.0) +
           0.59 * min(aColor.g, 1.0) +
           0.11 * min(aColor.b, 1.0);
      aData^ := Trunc($FF * s);
      inc(aData);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure tsFormatUnmap(const aFormat: TtsFormat; var aData: PByte; out aColor: TtsColor4f);
var
  i: Integer;
begin
  case aFormat of
    tsFormatRGBA8: begin
      for i := 0 to 3 do begin
        aColor.arr[i] := aData^ / $FF;
        inc(aData);
      end;
    end;

    tsFormatLumAlpha8: begin
      aColor.r := aData^ / $FF;
      aColor.g := aData^ / $FF;
      aColor.b := aData^ / $FF;
      inc(aData);
      aColor.a := aData^ / $FF;
      inc(aData);
    end;

    tsFormatAlpha8: begin
      aColor.r := 1.0;
      aColor.g := 1.0;
      aColor.b := 1.0;
      aColor.a := aData^ / $FF;
      inc(aData);
    end;

    tsFormatLum8: begin
      aColor.r := aData^ / $FF;
      aColor.g := aData^ / $FF;
      aColor.b := aData^ / $FF;
      aColor.a := 1.0;
      inc(aData);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsBlendValueIgnore(const aSrc, aDst: Single): Single;
begin
  result := aDst;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsBlendValueReplace(const aSrc, aDst: Single): Single;
begin
  result := aSrc;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsBlendValueModulate(const aSrc, aDst: Single): Single;
begin
  result := aSrc * aDst;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsBlendColorAlpha(const aSrc, aDst: TtsColor4f): TtsColor4f;
var
  i: Integer;
begin
  for i := 0 to 2 do
    result.arr[i] := aSrc.arr[i] * aSrc.a + aDst.arr[i] * (1 - aSrc.a);
  result.a := aSrc.a + aDst.a * (1 - aSrc.a);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsBlendColorAdditive(const aSrc, aDst: TtsColor4f): TtsColor4f;
var
  i: Integer;
begin
  for i := 0 to 3 do
    result.arr[i] := aSrc.arr[i] + aDst.arr[i];
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsBlendColorAdditiveAlpha(const aSrc, aDst: TtsColor4f): TtsColor4f;
var
  i: Integer;
begin
  for i := 0 to 2 do
    result.arr[i] := aSrc.arr[i] * aSrc.a + aDst.arr[i];
  result.a := aDst.a;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsStrAlloc(aSize: Cardinal): PWideChar;
begin
  aSize := (aSize + 1) shl 1;
  GetMem(result, aSize);
  FillChar(result^, aSize, 0);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsStrNew(const aText: PWideChar): PWideChar;
begin
  result := tsStrAlloc(tsStrLength(aText));
  tsStrCopy(result, aText);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure tsStrDispose(const aText: PWideChar);
begin
  FreeMem(aText);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsStrLength(aText: PWideChar): Cardinal;
begin
  result := 0;
  if Assigned(aText) then
    while (ord(aText^) <> 0) do begin
      inc(result);
      inc(aText);
    end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsStrCopy(aDst, aSrc: PWideChar): PWideChar;
begin
  result := aDst;
  if Assigned(aDst) and Assigned(aSrc) then
    while ord(aSrc^) <> 0 do begin
      aDst^ := aSrc^;
      inc(aDst);
      inc(aSrc);
    end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsAnsiToWide(aDst: PWideChar; const aSize: Integer; aSrc: PAnsiChar;
  const aCodePage: TtsCodePage; const aDefaultChar: WideChar): Integer;
begin
  case aCodePage of
    tsUTF8:
      result := tsUTF8ToWide(aDst, aSize, aSrc, aDefaultChar);

    tsISO_8859_1:
      result := tsISO_8859_1ToWide(aDst, aSize, aSrc);
  else
    result := tsAnsiSBCDToWide(aDst, aSize, aSrc, aCodePage, aDefaultChar);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsISO_8859_1ToWide(aDst: PWideChar; const aSize: Integer; aSrc: PAnsiChar): Integer;
begin
  result := 0;
  if Assigned(aDst) and Assigned(aSrc) then
    while (ord(aSrc^) <> 0) and (result < aSize) do begin
      aDst^ := WideChar(aSrc^);
      inc(aDst);
      inc(aSrc);
      inc(result);
    end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsUTF8ToWide(aDst: PWideChar; const aSize: Integer; const aSrc: PAnsiChar; const aDefaultChar: WideChar): Integer;

  procedure AddToDest(aCharCode: UInt64);
  begin
    if (aCharCode > $FFFF) then
      aCharCode := ord(aDefaultChar);

    PWord(aDst)^ := aCharCode;
    inc(aDst);
    result := result + 1;
  end;

const
  STATE_STARTBYTE  = 0;
  STATE_FOLLOWBYTE = 1;
var
  cc: UInt64;
  len, state, c: Integer;
  p: PByte;
  tmp: Byte;
begin
  result := 0;
  if not Assigned(aDst) or not Assigned(aSrc) or (aSize <= 0) then
    exit;

  c     := 0;
  cc    := 0;
  p     := PByte(aSrc);
  len   := Length(aSrc);
  state := STATE_STARTBYTE;
  while (len > 0) do begin
    case state of
      STATE_STARTBYTE: begin
        if (p^ and $80 = 0) then begin
          AddToDest(p^);
        end else if (p^ and $40 > 0) then begin
          tmp := p^;
          c   := 0;
          while (tmp and $80) > 0 do begin
            inc(c);
            tmp := tmp shl 1;
          end;
          cc    := p^ and ((1 shl (7 - c)) - 1);
          state := STATE_FOLLOWBYTE;
          c := c - 1;
        end;
      end;

      STATE_FOLLOWBYTE: begin
        if ((p^ and $C0) = $80) then begin
          cc := (cc shl 6) or (p^ and $3F);
          c := c - 1;
          if (c = 0) then begin
            AddToDest(cc);
            state := STATE_STARTBYTE;
          end;
        end else
          state := STATE_STARTBYTE;
      end;
    end;

    if (result >= aSize) then
      exit;
    inc(p);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsUTFBE16ToWide(aDst: PWideChar; const aDstSize: Integer; aSrc: PByte; aSrcSize: Integer;
  const aDefaultChar: WideChar): Integer;
var
  tmp: Word;

  procedure AddToDest(aCharCode: Word);
  begin
    if ((aCharCode and $D800) = $D800) or
       ((aCharCode and $DC00) = $DC00) then
         aCharCode := Ord(aDefaultChar);

    aDst^ := WideChar(aCharCode);
    inc(aDst, 1);
    result := result + 1;
  end;

begin
  result := 0;
  while (aSrcSize > 1) and (aDstSize > 0) do begin
{$IFDEF FPC}
    tmp := (aSrc^ shl 8) or (aSrc + 1)^;
{$ELSE}
    tmp := (PByteArray(aSrc)[0] shl 8) or PByteArray(aSrc)[1];
{$ENDIF}
    inc(aSrc, 2);
    dec(aSrcSize, 2);
    AddToDest(tmp);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function tsAnsiSBCDToWide(aDst: PWideChar; const aSize: Integer; aSrc: PAnsiChar;
  const aCodePage: TtsCodePage; const aDefaultChar: WideChar): Integer;
var
  tmp: WideChar;
  cp: PtsCodePageValues;
begin
  result := 0;
  cp := TS_CODE_PAGE_LUT[aCodePage];
  if not Assigned(aDst) or
     not Assigned(aSrc) or
     not Assigned(cp) or
     (aSize < 0) then exit;

  while (Ord(aSrc^) <> 0) and (result < aSize) do begin
    tmp := WideChar(cp^[aSrc^]);
    if (ord(tmp) = 0) then begin
      if (ord(aDefaultChar) <> 0) then begin
        aDst^ := aDefaultChar;
        inc(aDst);
      end;
    end else begin
      aDst^ := tmp;
      inc(aDst);
    end;
    inc(aSrc);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsKernel1D///////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsKernel1D.Create(const aRadius, aStrength: Single);
var
  TempRadius, SQRRadius, TempStrength, TempValue: Double;
  Idx: Integer;

  function CalcValue(const aIndex: Integer): Single;
  var
    Temp: Double;
  begin
    Temp   := Max(0, Abs(aIndex) - TempStrength);
    Temp   := Sqr(Temp * TempRadius) / SQRRadius;
    result := Exp(-Temp);
  end;

begin
  inherited Create;

  // calculate new radius and strength
  TempStrength := Min(aRadius - 1, aRadius * aStrength);
  TempRadius   := aRadius - TempStrength;
  SQRRadius    := sqr(TempRadius) * sqr(TempRadius);

  // caluculating size of the kernel
  Size := Round(TempRadius);
  while CalcValue(Size) > 0.001 do
    Inc(Size);
  Size      := Size -1;
  ItemCount := Size * 2 +1;
  SetLength(Items, ItemCount);

  // calculate Value (yes thats right. there is no -1)
  for Idx := 0 to Size do begin
    TempValue := CalcValue(Idx);

    with Items[Size + Idx] do begin
      Offset := Idx;
      Value := TempValue;
    end;

    with Items[Size - Idx] do begin
      Offset := -Idx;
      Value := TempValue;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsKernel2D///////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsKernel2D.Create(const aRadius, aStrength: Single);
var
  tmpStrenght: Double;
  tmpRadius: Double;
  tmpValue: Double;
  sqrRadius: Double;
  x, y, w, h: Integer;

  function CalcValue(const aIndex: Double): Double;
  begin
    result := max(0, Abs(aIndex) - tmpStrenght);
    result := Sqr(result * tmpRadius) / sqrRadius;
    result := Exp(-result);
  end;

  procedure CalcSize(var aSize, aMidSize: Integer);
  begin
    aSize    := 0;
    aMidSize := 0;
    while CalcValue(aSize) > 0.5 do begin
      inc(aSize,    1);
      inc(aMidSize, 1);
    end;
    while CalcValue(aSize) > 0.001 do
      Inc(aSize, 1);
  end;

  procedure SetItem(const x, y: Integer);
  begin
    with Items[(SizeY + y) * w + (SizeX + x)] do begin
      OffsetX := x;
      OffsetY := y;
      Value   := tmpValue;
    end;
  end;

  procedure QuickSort(l, r: Integer);
  var
    _l, _r: Integer;
    p, t: TtsKernel2DItem;
  begin
    repeat
      _l := l;
      _r := r;
      p := Items[(l + r) shr 1];

      repeat
        while (Items[_l].Value > p.Value) do
          inc(_l, 1);

        while (Items[_r].Value < p.Value) do
          dec(_r, 1);

        if (_l <= _r) then begin
          t         := Items[_l];
          Items[_l] := Items[_r];
          Items[_r] := t;
          inc(_l, 1);
          dec(_r, 1);
        end;
      until (_l > _r);

      if (l < _r) then
        QuickSort(l, _r);

      l := _l;
    until (_l >= r);
  end;

begin
  inherited Create;

  tmpStrenght := Min(aRadius - 1.0, aRadius * aStrength);
  tmpRadius   := aRadius - tmpStrenght;
  sqrRadius   := sqr(tmpRadius) * sqr(tmpRadius);

  CalcSize(SizeX, MidSizeX);
  CalcSize(SizeY, MidSizeY);

  ValueSum  := 0.0;
  w         := 2 * SizeX + 1;
  h         := 2 * SizeY + 1;
  ItemCount := w * h;
  SetLength(Items, ItemCount);

  for y := 0 to SizeY do begin
    for x := 0 to SizeX do begin
      tmpValue := CalcValue(sqrt(Sqr(x) + Sqr(y)));

      SetItem( x,  y);
      SetItem( x, -y);
      SetItem(-x, -y);
      SetItem(-x,  y);

      ValueSum := ValueSum + tmpValue;
      if (x > 0) and (y > 0) then
        ValueSum := ValueSum + tmpValue;
    end;
  end;

  QuickSort(0, ItemCount-1);

  while (Items[ItemCount-1].Value < 0.001) do
    dec(ItemCount, 1);
  SetLength(Items, ItemCount);
end;

end.

