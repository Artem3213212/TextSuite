unit utsUtils;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, utsTypes;

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
  utsCodePages;

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
  cp := ANSI_TO_WIDE_CODE_PAGE_LUT[aCodePage];
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

end.

