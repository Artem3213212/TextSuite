unit utsFontCreator;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  utsUtils, utsContext, utsTypes, utsFont;

type
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsFontCreator = class(TtsRefManager)
  private
    fContext: TtsContext;
  public
    property Context: TtsContext read fContext;

    function GetFontByName(const aFontname: String; const aSize: Integer; const aStyle: TtsFontStyles; const aAntiAliasing: TtsAntiAliasing): TtsFont; overload; virtual;
    function GetFontByFile(const aFilename: String; const aSize: Integer; const aStyle: TtsFontStyles; const aAntiAliasing: TtsAntiAliasing): TtsFont; overload; virtual;
    function GetFontByStream(const aStream: TStream; const aSize: Integer; const aStyle: TtsFontStyles; const aAntiAliasing: TtsAntiAliasing): TtsFont; overload; virtual;

    constructor Create(const aContext: TtsContext);
  end;

implementation

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsFontCreator////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFontCreator.GetFontByName(const aFontname: String; const aSize: Integer; const aStyle: TtsFontStyles; const aAntiAliasing: TtsAntiAliasing): TtsFont;
begin
  result := nil;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFontCreator.GetFontByFile(const aFilename: String; const aSize: Integer; const aStyle: TtsFontStyles; const aAntiAliasing: TtsAntiAliasing): TtsFont;
begin
  result := nil;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsFontCreator.GetFontByStream(const aStream: TStream; const aSize: Integer; const aStyle: TtsFontStyles; const aAntiAliasing: TtsAntiAliasing): TtsFont;
begin
  result := nil;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsFontCreator.Create(const aContext: TtsContext);
begin
  inherited Create(aContext);
  fContext := aContext;
end;

end.

