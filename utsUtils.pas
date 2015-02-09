unit utsUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function  tsStrAlloc(aSize: Cardinal): PWideChar;
function  tsStrNew(const aText: PWideChar): PWideChar;
procedure tsStrDispose(const aText: PWideChar);
function  tsStrLength(aText: PWideChar): Cardinal;
function  tsStrCopy(aDst, aSrc: PWideChar): PWideChar;
function  tsISO_8859_1ToWide(aDst: PWideChar; const aSize: Integer; aSrc: PAnsiChar): Integer;
function  tsUTF8ToWide(aDst: PWideChar; const aSize: Integer; const aSrc: PAnsiChar; const aDefaultChar: WideChar): Integer;

implementation

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
function tsISO_8859_1ToWide(aDst: PWideChar; const aSize: Integer; aSrc: PAnsiChar): Integer;
begin
  result := 0;
  if Assigned(aDst) and Assigned(aSrc) then
    while (ord(aSrc^) <> 0) do begin
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
  cc: QWord;
  len, state, c: Integer;
  p: PByte;
  tmp: Byte;
begin
  result := 0;
  if not Assigned(aDst) or not Assigned(aSrc) or (aSize <= 0) then
    exit;

  p     := PByte(aSrc);
  len   := Length(aSrc);
  state := STATE_STARTBYTE;
  while (len > 0) do begin
    case state of
      STATE_STARTBYTE: begin
        if (p^ and %10000000 = 0) then begin
          AddToDest(p^);
        end else if (p^ and %01000000 > 0) then begin
          tmp := p^;
          c   := 0;
          while (tmp and %10000000) > 0 do begin
            inc(c);
            tmp := tmp shl 1;
          end;
          cc    := p^ and ((1 shl (7 - c)) - 1);
          state := STATE_FOLLOWBYTE;
          c := c - 1;
        end;
      end;

      STATE_FOLLOWBYTE: begin
        if ((p^ and %11000000) = %10000000) then begin
          cc := (cc shl 6) or (p^ and %00111111);
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

end.

