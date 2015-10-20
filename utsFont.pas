unit utsFont;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  utsUtils, utsFontCreator, utsTypes, utsPostProcessor, utsImage;

type
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TtsFont = class(TtsMultiMasterRefManager)
  private
    fCreator: TtsFontCreator;
    fMetric: TtsFontMetric;
    fPostProcessor: TtsPostProcessor;

    fTabWidth: Integer;
    fCharSpacing: Integer;
    fLineSpacing: Single;
  protected
    {%H-}constructor Create(const aCreator: TtsFontCreator; const aMetric: TtsFontMetric);
  public
    property Creator:       TtsFontCreator   read fCreator;
    property Metric:        TtsFontMetric    read fMetric;
    property PostProcessor: TtsPostProcessor read fPostProcessor write fPostProcessor;

    property TabWidth:    Integer read fTabWidth    write fTabWidth;
    property CharSpacing: Integer read fCharSpacing write fCharSpacing;
    property LineSpacing: Single  read fLineSpacing write fLineSpacing;

    procedure GetTextMetric(out aMetric: TtsTextMetric);
    procedure GetCharImage(const aCharCode: WideChar; const aCharImage: TtsImage; const aFormat: TtsFormat); virtual; abstract;
    function GetGlyphMetrics(const aCharCode: WideChar; out aGlyphOrigin, aGlyphSize: TtsPosition; out aAdvance: Integer): Boolean; virtual; abstract;
  end;

implementation

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TtsFont//////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TtsFont.Create(const aCreator: TtsFontCreator; const aMetric: TtsFontMetric);
begin
  inherited Create(aCreator);
  fCreator := aCreator;
  fMetric  := aMetric;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TtsFont.GetTextMetric(out aMetric: TtsTextMetric);
begin
  aMetric.Ascent            := fMetric.Ascent;
  aMetric.Descent           := fMetric.Descent;
  aMetric.ExternalLeading   := fMetric.ExternalLeading;
  aMetric.BaseLineOffset    := fMetric.BaseLineOffset;
  aMetric.CharSpacing       := CharSpacing;
  aMetric.LineHeight        := fMetric.Ascent + fMetric.Descent + fMetric.ExternalLeading;
  aMetric.LineSpacing       := Trunc(fMetric.Size * fLineSpacing);
end;

end.

