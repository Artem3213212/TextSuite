unit utsConstants;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  utsTypes, utsUtils, utsCodePages;

const
  TS_IMAGE_MODE_FUNCTIONS: array[TtsImageMode] of TtsBlendValueFunc = (
    tsBlendValueIgnore,
    tsBlendValueReplace,
    tsBlendValueModulate);

  TS_IMAGE_MODES_REPLACE_ALL: TtsImageModes = (
    tsModeReplace,
    tsModeReplace,
    tsModeReplace,
    tsModeReplace);
  TS_IMAGE_MODES_MODULATE_ALPHA: TtsImageModes = (
    tsModeReplace,
    tsModeReplace,
    tsModeReplace,
    tsModeModulate);
  TS_IMAGE_MODES_MODULATE_ALL: TtsImageModes = (
    tsModeModulate,
    tsModeModulate,
    tsModeModulate,
    tsModeModulate);    

  TS_COLOR_CHANNELS_RGB: TtsColorChannels = [
    tsChannelRed,
    tsChannelGreen,
    tsChannelBlue];
  TS_COLOR_CHANNELS_RGBA: TtsColorChannels = [
    tsChannelRed,
    tsChannelGreen,
    tsChannelBlue,
    tsChannelAlpha];

  TS_CODE_PAGE_LUT: array[TtsCodePage] of PtsCodePageValues = (
      nil,          //tsUTF8
      nil,          //tsISO_8859_1
      @CP_8859_2,   //tsISO_8859_2
      @CP_8859_3,   //tsISO_8859_3
      @CP_8859_4,   //tsISO_8859_4
      @CP_8859_5,   //tsISO_8859_5
      @CP_8859_6,   //tsISO_8859_6
      @CP_8859_7,   //tsISO_8859_7
      @CP_8859_8,   //tsISO_8859_8
      @CP_8859_9,   //tsISO_8859_9
      @CP_8859_10,  //tsISO_8859_10
      @CP_8859_11,  //tsISO_8859_11
      @CP_8859_13,  //tsISO_8859_13
      @CP_8859_14,  //tsISO_8859_14
      @CP_8859_15,  //tsISO_8859_15
      @CP_8859_16,  //tsISO_8859_16
      @CP_037,      //tsISO_037
      @CP_437,      //tsISO_437
      @CP_500,      //tsISO_500
      @CP_737,      //tsISO_737
      @CP_775,      //tsISO_775
      @CP_850,      //tsISO_850
      @CP_852,      //tsISO_852
      @CP_855,      //tsISO_855
      @CP_857,      //tsISO_857
      @CP_860,      //tsISO_860
      @CP_861,      //tsISO_861
      @CP_862,      //tsISO_862
      @CP_863,      //tsISO_863
      @CP_864,      //tsISO_864
      @CP_865,      //tsISO_865
      @CP_866,      //tsISO_866
      @CP_869,      //tsISO_869
      @CP_874,      //tsISO_874
      @CP_875,      //tsISO_875
      @CP_1026,     //tsISO_1026
      @CP_1250,     //tsISO_1250
      @CP_1251,     //tsISO_1251
      @CP_1252,     //tsISO_1252
      @CP_1253,     //tsISO_1253
      @CP_1254,     //tsISO_1254
      @CP_1255,     //tsISO_1255
      @CP_1256,     //tsISO_1256
      @CP_1257,     //tsISO_1257
      @CP_1258      //tsISO_1258
    );

  TS_MATRIX_IDENTITY: TtsMatrix4f = ((1, 0, 0, 0), (0, 1, 0, 0), (0, 0, 1, 0), (0, 0, 0, 1));

implementation

end.

