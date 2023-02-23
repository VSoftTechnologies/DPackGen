unit DPackGen.Types;

interface

{$SCOPEDENUMS ON}

const
  CurrentSchemaVersion = 0;
  cUnset = '--unset--';
  cTokenRegex  = '\%(\w+)\%';

type
  TConstProc<T> = reference to procedure(const Arg1 : T);
  TConstProc<T1, T2> = reference to procedure(const Arg1 : T1; const Arg2 : T2);
  TConstProc<T1, T2, T3> = reference to procedure(const Arg1 : T1; const Arg2 : T2; const Arg3 : T3);
  TConstProc<T1, T2, T3, T4> = reference to procedure(const Arg1 : T1; const Arg2 : T2; const Arg3 : T3; const Arg4 : T4);

  //copied from DPM

  TCompilerVersion = (
    UnknownVersion,
    XE2,
    XE3,
    XE4,
    XE5,
    XE6,
    XE7,
    XE8,
    D10_0,
    D10_1,
    D10_2,
    D10_3,
    D10_4,
    D11
    );

  TCompilerVersions = set of TCompilerVersion;

  //covering all bases here.
  TPlatform = (
    UnknownPlatform,
    Win32,
    Win64,
    WinArm32, //reserved for future use
    WinArm64, //reserved for future use
    OSX32,
    OSX64,
    OSXARM64,
    AndroidArm32,
    AndroidArm64,
    AndroidIntel32, //reserved for future use
    AndroidIntel64, //reserved for future use
    iOS32,
    iOS64, //reserved for future use
    LinuxIntel32, //reserved for future use
    LinuxIntel64,
    LinuxArm32, //reserved for future use
    LinuxArm64 //reserved for future use
    );

    TPlatforms = set of TPlatform;

    TProjectType = (Invalid, Application, Package, DLL);

    TFrameworkType = (Invalid, None,VCL, FMX);

    TPackageType = (Invalid, Runtime, Designtime);

const
   projectFileExt : array[TProjectType] of string = ('','.dpr','.dpk','.dpr');
   projectStart : array[TProjectType] of string = ('','program','package','library');
   borlandProjectType : array[TProjectType] of string = ('','Application','Package','Application');
implementation

end.
