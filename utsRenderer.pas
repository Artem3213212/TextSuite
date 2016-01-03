unit utsRenderer;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  utsTypes, utsFont, utsCharCache, utsTextBlock;

type
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsRenderer = class(TtsBlockRenderer)
  public
    function GetTextWidthA(const aFont: TtsFont; const aText: PAnsiChar): Integer;
    function GetTextWidthW(const aFont: TtsFont; const aText: PWideChar): Integer;

    function BeginBlock(const aLeft, aTop, aWidth, aHeight: Integer; const aFlags: TtsBlockFlags): TtsTextBlock;
  public
    class procedure EndBlock(var aBlock: TtsTextBlock);
    class procedure AbortBlock(var aBlock: TtsTextBlock);
  end;

implementation

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsRenderer///////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsRenderer.GetTextWidthA(const aFont: TtsFont; const aText: PAnsiChar): Integer;
var
  c: TtsChars;
begin
  result := 0;
  c      := CharCache.Chars[aFont];
  if Assigned(c) then
    result := c.GetTextWidthA(aText);
end;

function TtsRenderer.GetTextWidthW(const aFont: TtsFont; const aText: PWideChar): Integer;
var
  c: TtsChars;
begin
  result := 0;
  c      := CharCache.Chars[aFont];
  if Assigned(c) then
    result := c.GetTextWidthW(aText);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TtsRenderer.BeginBlock(const aLeft, aTop, aWidth, aHeight: Integer; const aFlags: TtsBlockFlags): TtsTextBlock;
begin
  result := TtsTextBlock.Create(self, aLeft, aTop, aWidth, aHeight, aFlags);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class procedure TtsRenderer.EndBlock(var aBlock: TtsTextBlock);
begin
  try
    aBlock.Render;
  finally
    FreeAndNil(aBlock);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class procedure TtsRenderer.AbortBlock(var aBlock: TtsTextBlock);
begin
  FreeAndNil(aBlock);
end;

end.

