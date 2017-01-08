{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit TextSuite;

{$warn 5023 off : no warning about unused units}
interface

uses
  utsChar, utsCharCache, utsCodePages, utsConstants, utsContext, utsFont, utsFontCreator, utsFontCreatorFreeType, 
  utsFontCreatorGDI, utsFreeType, utsGDI, utsImage, utsOpenGLUtils, utsPostProcessor, utsRenderer, utsRendererOpenGL, 
  utsRendererOpenGLES, utsTextBlock, utsTextSuite, utsTypes, utsUtils, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('TextSuite', @Register);
end.
