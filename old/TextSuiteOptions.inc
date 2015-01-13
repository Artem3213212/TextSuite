
{ *** options *** }

{ to use the external library }
{.$define TS_EXTERN_STATIC}


{ to disable the assembler code and use pure pascal code instead.
  if you have problem with some older compiler or runtime errors.
  But. This can decrease the speed of some operations. }
{.$define TS_PURE_PASCAL}



{ *** important seetings. Don't touch it! *** }
{$IFDEF FPC}
  {$MODE Delphi}

  {$SMARTLINK ON}

  {$IFDEF CPUI386}
    {$DEFINE CPU386}
    {$ASMMODE INTEL}
  {$ELSE}
    {$define TS_PURE_PASCAL}
  {$ENDIF}

  {$IFNDEF WINDOWS}
    {$LINKLIB c}
  {$ENDIF}
{$ENDIF}


{$BOOLEVAL OFF}         // short boolean eval
{$LONGSTRINGS ON}       // huge strings
{$EXTENDEDSYNTAX ON}    // extended syntax
{$ALIGN ON}             // Alignment
{$TYPEDADDRESS OFF}     // Typed addresses with @

{$IFNDEF FPC}
  {$OPTIMIZATION ON}      // O+     Optimizations
  { $ASSERTIONS OFF}       // C-
  { $RANGECHECKS OFF}      // R-
  { $STACKFRAMES OFF}      // W-
  { $OVERFLOWCHECKS OFF}   // Q-
{$ENDIF}
