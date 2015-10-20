unit utsChar;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  utsTypes;

type
  TtsChar = class(TObject)
  private
    fCharCode: WideChar;
    fGlyphMetric: TtsGlyphMetric;
  protected
    fRenderRef: TtsRenderRef;
  public
    property CharCode:    WideChar       read fCharCode;
    property RenderRef:   TtsRenderRef   read fRenderRef;
    property GlyphMetric: TtsGlyphMetric read fGlyphMetric write fGlyphMetric;

    constructor Create(const aCharCode: WideChar);
  end;

implementation

constructor TtsChar.Create(const aCharCode: WideChar);
begin
  inherited Create;
  fCharCode := aCharCode;
end;

end.

