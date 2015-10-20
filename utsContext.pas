unit utsContext;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  utsUtils, utsTypes;

type
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsContext = class(TtsRefManager)
  private
    fCodePage: TtsCodePage;
    fDefaultChar: WideChar;
  public
    property CodePage:    TtsCodePage read fCodePage    write fCodePage;
    property DefaultChar: WideChar    read fDefaultChar write fDefaultChar;

    function AnsiToWide(const aText: PAnsiChar): PWideChar; overload;
    function AnsiToWide(const aText: PAnsiChar; const aLength: Integer): PWideChar; overload;

    constructor Create;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsContext////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsContext.AnsiToWide(const aText: PAnsiChar): PWideChar;
begin
  result := AnsiToWide(aText, Length(aText));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsContext.AnsiToWide(const aText: PAnsiChar; const aLength: Integer): PWideChar;
begin
  result := nil;
  if not Assigned(aText) then
    exit;
  result := tsStrAlloc(aLength);
  tsAnsiToWide(result, aLength, aText, fCodePage, fDefaultChar);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsContext.Create;
begin
  inherited Create(nil);
  fCodePage    := tsUTF8;
  fDefaultChar := '?';
end;

end.

