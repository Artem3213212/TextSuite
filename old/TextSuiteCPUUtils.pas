{
TextSuite (C) Steffen Xonna (aka Lossy eX)
http://www.opengl24.de/
-----------------------------------------------------------------------
For copyright informations see file copyright.txt.
}

{$I TextSuiteOptions.inc}

unit TextSuiteCPUUtils;

{$ifdef TS_PURE_PASCAL}
  {$message fatal 'This unit is''t compatible to the flag TS_PURE_PASCAL.'}
{$endif}

interface


var
  supportFPU,
  supportCMOV,
  supportMMX,
  supportMMX_EXT,
  supportSSE,
  supportSSE2,
  support3DNow,
  support3DNow_EXT,
  supportSSE3,
  supportSSSE3
    : ByteBool;


procedure ReadCPUFlags;

function GetSSESafeMem(Size: Cardinal): Pointer;
function GetSSESafeAddr(Addr: Pointer): Pointer;


implementation


const
  BIT_FPU         = 1 shl 0;
  BIT_CMOV        = 1 shl 15;
  BIT_MMX         = 1 shl 23;
  BIT_SSE         = 1 shl 25;
  BIT_SSE2        = 1 shl 26;
  BIT_3DNOW_EXT   = 1 shl 30;
  BIT_3DNOW       = 1 shl 31;

  BIT_SSE3        = 1 shl 0;
  BIT_SSSE3       = 1 shl 9;


procedure ReadCPUFlags;
asm
  pushfd
  pop   eax               // copy EEFlags to eax
  mov   edx, eax          // copy to edx

  xor   eax, $00200000    // clear bit 21
  push  eax
  popfd                   // restore to EEFlags

  pushfd
  pop   eax               // copy EEFlags to eax
  xor   eax, edx          // test if flags hav changed
  jnz @@supportCPUID

  ret

@@supportCPUID:

  push  ebx                 // save ebx

  mov   eax, 1              // function 1
  cpuid

  // test flags
  test  edx, BIT_FPU
  setnz [supportFPU]        // FPU supported

  test  edx, BIT_CMOV
  setnz [supportCMOV]       // CMOV supported

  test  edx, BIT_MMX
  setnz [supportMMX]        // MMX supported

  test  edx, BIT_SSE
  setnz [supportSSE]        // SSE supported

  test  edx, BIT_SSE2
  setnz [supportSSE2]       // SSE2 supported

  test  ecx, BIT_SSE3
  setnz [supportSSE3]       // SSE3 supported

  test  ecx, BIT_SSSE3
  setnz [supportSSSE3]      // SSSE3 supported

  // test extended functions
  mov   eax, $80000000
  cpuid
  cmp   eax, $80000000
  jbe @@no_ext_functions

  mov   eax, $80000001
  cpuid

  test  edx, BIT_3DNOW
  setnz [support3DNow]      // 3DNow supported

  test  edx, BIT_3DNOW_EXT
  setnz [support3DNow_EXT]  // 3DNowExt supported


@@no_ext_functions:

  pop   ebx                 // restore ebx

@@end:
end;


function GetSSESafeMem(Size: Cardinal): Pointer;
begin
  GetMem(Result, Size + $F);
end;


function GetSSESafeAddr(Addr: Pointer): Pointer;
asm
  test  eax, $F       // test if one of the last bits are set
  jz    @@end         // address is allways 16 Byte aligned
  
  or    eax, $F       // fill the last 4 bits
  inc   eax           // add 1
  
@@end:
end;


end.