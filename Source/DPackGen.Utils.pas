unit DPackGen.Utils;

interface

uses
  System.TypInfo,
  System.SysUtils,
  DPackGen.Types,
  DPackGen.MSXML;

function StringToCompilerVersion(const value : string) : TCompilerVersion;
function IsValidCompilerString(const value : string) : boolean;
function CompilerToString(const value : TCompilerVersion) : string;
function CompilerToStringNoPoint(const value : TCompilerVersion) : string;
function CompilerToBDSVersion(const compiler : TCompilerVersion) : string;
function CompilerToCompilerVersionIntStr(const compiler : TCompilerVersion) : string;
function CompilerWithCodeName(const value : TCompilerVersion) : string;
function CompilerCodeName(const value : TCompilerVersion) : string;
function CompilerToLibSuffix(const compiler : TCompilerVersion) : string;

function CompilerToDPROJVersion(const compiler : TCompilerVersion) : string;

function StringToFrameworkType(const value : string) : TFrameworkType;
function FrameworkTypeToString(const value : TFrameworkType) : string;

function StringToProjectType(const value : string) : TProjectType;
function ProjectTypeToString(const value : TProjectType) : string;

function StringToPackageType(const value : string) : TPackageType;


function StringToPlatform(const value : string) : TPlatform;
function IsValidPlatformString(const value : string) : boolean;
function PlatformToString(const value : TPlatform) : string;
function PlatformToDisplayString(const value : TPlatform) : string;
function ValidatePlatform(const target : TCompilerVersion; const platform : TPlatform) : boolean;
function PlatformToDPROJPlatform(const value : TPlatform) : string;


procedure PrettyFormatXML(const node: IXMLDOMNode; const indentSize: integer); overload;


type
  TSystemUtils = class
  public
    class function GetVersionString: string;
    class procedure GetResourceVersionNumbers(out AMajor, AMinor, ARelease, ABuild: Integer);
  end;



implementation

uses
  WinApi.Windows,
  System.IOUtils;

function StringToFrameworkType(const value : string) : TFrameworkType;
var
  iValue : integer;
begin
  iValue := GetEnumValue(typeInfo(TFrameworkType), value);
  if iValue = -1 then
    result := TFrameworkType.Invalid
  else
    result := TFrameworkType(iValue);
end;

function FrameworkTypeToString(const value : TFrameworkType) : string;
begin
  result := GetEnumName(TypeInfo(TFrameworkType), ord(value));
end;

function StringToProjectType(const value : string) : TProjectType;
var
  iValue : integer;
begin
  iValue := GetEnumValue(typeInfo(TProjectType), value);
  if iValue = -1 then
    result := TProjectType.Invalid
  else
    result := TProjectType(iValue);
end;

function ProjectTypeToString(const value : TProjectType) : string;
begin
  result := GetEnumName(TypeInfo(TProjectType), ord(value));
end;


function StringToPackageType(const value : string) : TPackageType;
var
  iValue : integer;
begin
  iValue := GetEnumValue(typeInfo(TPackageType), value);
  if iValue = -1 then
    result := TPackageType.Invalid
  else
    result := TPackageType(iValue);
end;

function StringToCompilerVersion(const value : string) : TCompilerVersion;
var
  iValue : integer;
  sValue : string;
begin
  sValue := StringReplace(value, '.', '_', [rfReplaceAll]);

  iValue := GetEnumValue(typeInfo(TCompilerVersion), sValue);

  if iValue = -1 then
    result := TCompilerVersion.UnknownVersion
  else
    result := TCompilerVersion(iValue);
end;


function IsValidCompilerString(const value : string) : boolean;
begin
  result := StringToCompilerVersion(value) <> TCompilerVersion.UnknownVersion;
end;


function CompilerToString(const value : TCompilerVersion) : string;
begin
  result := GetEnumName(TypeInfo(TCompilerVersion), ord(value));
//  if result.StartsWith('D') then
//    Delete(result, 1, 1); // remove D
  result := StringReplace(result, '_', '.', [rfReplaceAll]);
end;

function CompilerToStringNoPoint(const value : TCompilerVersion) : string;
var
  i : integer;
begin
  result := CompilerToString(value);
  i := pos('.', result);
  if i > 0 then
    Delete(result, i, length(result));
end;


function CompilerToBDSVersion(const compiler : TCompilerVersion) : string;
begin
  case compiler of
    TCompilerVersion.XE2  : result := '9.0';
    TCompilerVersion.XE3  : result := '10.0';
    TCompilerVersion.XE4  : result := '11.0';
    TCompilerVersion.XE5  : result := '12.0';
    TCompilerVersion.XE6  : result := '14.0';
    TCompilerVersion.XE7  : result := '15.0';
    TCompilerVersion.XE8  : result := '16.0';
    TCompilerVersion.D10_0 : result := '17.0';
    TCompilerVersion.D10_1 : result := '18.0';
    TCompilerVersion.D10_2 : result := '19.0';
    TCompilerVersion.D10_3 : result := '20.0';
    TCompilerVersion.D10_4 : result := '21.0';
    TCompilerVersion.D11   : result := '22.0';
  else
    raise Exception.Create('BDSVersion is missing for : ' + CompilerToString(compiler));
  end;
end;


function CompilerToCompilerVersionIntStr(const compiler : TCompilerVersion) : string;
begin
  case compiler of
    TCompilerVersion.XE2  : result := '23';
    TCompilerVersion.XE3  : result := '24';
    TCompilerVersion.XE4  : result := '25';
    TCompilerVersion.XE5  : result := '26';
    TCompilerVersion.XE6  : result := '27';
    TCompilerVersion.XE7  : result := '28';
    TCompilerVersion.XE8  : result := '29';
    TCompilerVersion.D10_0 : result := '30';
    TCompilerVersion.D10_1 : result := '31';
    TCompilerVersion.D10_2 : result := '32';
    TCompilerVersion.D10_3 : result := '33';
    TCompilerVersion.D10_4 : result := '34';
    TCompilerVersion.D11   : result := '35';
  else
    raise Exception.Create('BDSVersion is missing for : ' + CompilerToString(compiler));
  end;

end;

function CompilerCodeName(const value : TCompilerVersion) : string;
begin
  case value of
    TCompilerVersion.D10_0 : result := 'Seattle';
    TCompilerVersion.D10_1 : result := 'Berlin';
    TCompilerVersion.D10_2 : result := 'Tokyo';
    TCompilerVersion.D10_3 : result := 'Rio';
    TCompilerVersion.D10_4 : result := 'Sydney';
    TCompilerVersion.D11   : result := 'Alexandria';
  else
    result := '';
  end;

end;


function CompilerWithCodeName(const value : TCompilerVersion) : string;
var
  codeName : string;
begin
  result := CompilerToString(value);
  codeName := CompilerCodeName(value);
  if codeName <> '' then
    result := result + ' ' + codeName;
end;


function CompilerToLibSuffix(const compiler : TCompilerVersion) : string;
begin
  case compiler of
    TCompilerVersion.XE2  : result := '160';
    TCompilerVersion.XE3  : result := '170';
    TCompilerVersion.XE4  : result := '180';
    TCompilerVersion.XE5  : result := '190';
    TCompilerVersion.XE6  : result := '200';
    TCompilerVersion.XE7  : result := '210';
    TCompilerVersion.XE8  : result := '220';
    TCompilerVersion.D10_0 : result := '230';
    TCompilerVersion.D10_1 : result := '240';
    TCompilerVersion.D10_2 : result := '250';
    TCompilerVersion.D10_3 : result := '260';
    TCompilerVersion.D10_4 : result := '270';
    TCompilerVersion.D11   : result := '280';
  else
    raise Exception.Create('LibSuffix is missing for : ' + CompilerToString(compiler));
  end;
end;

function CompilerToDPROJVersion(const compiler : TCompilerVersion) : string;
begin
  case compiler of
    TCompilerVersion.UnknownVersion: result := '';
    TCompilerVersion.XE2   : result := '13.0';
    TCompilerVersion.XE3   : result := '14.0';
    TCompilerVersion.XE4   : result := '14.4';
    TCompilerVersion.XE5   : result := '15.0';
    TCompilerVersion.XE6   : result := '15.4';
    TCompilerVersion.XE7   : result := '16.0';
    TCompilerVersion.XE8   : result := '17.0';
    TCompilerVersion.D10_0 : result := '18.0';
    TCompilerVersion.D10_1 : result := '18.2';
    TCompilerVersion.D10_2 : result := '18.3';
    TCompilerVersion.D10_3 : result := '18.5';
    TCompilerVersion.D10_4 : result := '19.0';
    TCompilerVersion.D11   : result := '19.3';
  else
    raise Exception.Create('DPROJ Version is missing for : ' + CompilerToString(compiler));
  end;
end;


function StringToPlatform(const value : string) : TPlatform;
var
  iValue : integer;
begin
  iValue := GetEnumValue(typeInfo(TPlatform), value);
  if iValue = -1 then
  begin
    if value = 'Android' then
      result := TPlatform.AndroidArm32
    else if value = 'Android64' then
      result := TPlatform.AndroidArm64
    else if value = 'Linux64' then
      result := TPlatform.LinuxIntel64
    else
      result := TPlatform.UnknownPlatform
  end
  else
    result := TPlatform(iValue);
end;


function IsValidPlatformString(const value : string) : boolean;
begin
  result := StringToPlatform(value) <> TPlatform.UnknownPlatform;
end;

function PlatformToString(const value : TPlatform) : string;
begin
  case value  of
    TPlatform.AndroidArm32: result := 'Android';
    TPlatform.AndroidArm64: result := 'Android64';
  else
    result := GetEnumName(TypeInfo(TPlatform), ord(value));
  end;
end;


function PlatformToDisplayString(const value : TPlatform) : string;
begin
  case value of
    TPlatform.UnknownPlatform: Result := 'Unknown' ;
    TPlatform.Win32: result := 'Windows 32-bit' ;
    TPlatform.Win64: result := 'Windows 64-bit';
    TPlatform.WinArm32: result := 'Windows 32-bit ARM';
    TPlatform.WinArm64: result := 'Windows 64-bit ARM';
    TPlatform.OSX32: result := 'macOS 32-bit';
    TPlatform.OSX64: result := 'macOS 64-bit';
    TPlatform.OSXARM64: result := 'macOS ARM 64-bit';

    TPlatform.AndroidArm32: result := 'Andriod 32-bit ARM';
    TPlatform.AndroidArm64: result := 'Andriod 64-bit ARM';
    TPlatform.AndroidIntel32: result := 'Andriod 32-bit Intel';
    TPlatform.AndroidIntel64: result := 'Andriod 64-bit Intel';
    TPlatform.iOS32: result := 'iOS 32-bit';
    TPlatform.iOS64: result := 'iOS 64-bit';
    TPlatform.LinuxIntel32: result := 'Linux 32-bit';
    TPlatform.LinuxIntel64: result := 'Linux 64-bit';
    TPlatform.LinuxArm32: result := 'Linux 32-bit ARM';
    TPlatform.LinuxArm64: result := 'Linux 64-bit ARM';
  end;

end;


function AllPlatforms(const compiler : TCompilerVersion) : TPlatforms;
begin
  result := [];
  case compiler of
    TCompilerVersion.XE2 : result := [TPlatform.Win32, TPlatform.Win64, TPlatform.OSX32];
    TCompilerVersion.XE3,
    TCompilerVersion.XE4,
    TCompilerVersion.XE5,
    TCompilerVersion.XE6,
    TCompilerVersion.XE7,
    TCompilerVersion.XE8 : result := [TPlatform.Win32, TPlatform.Win64, TPlatform.OSX32, TPlatform.iOS32, TPlatform.AndroidArm32];
    TCompilerVersion.D10_0,
    TCompilerVersion.D10_1 : result := [TPlatform.Win32, TPlatform.Win64, TPlatform.OSX32, TPlatform.iOS32, TPlatform.AndroidArm32];
    TCompilerVersion.D10_2 : result := [TPlatform.Win32, TPlatform.Win64, TPlatform.OSX32, TPlatform.iOS32, TPlatform.AndroidArm32, TPlatform.LinuxIntel64];
    TCompilerVersion.D10_3 : result := [TPlatform.Win32, TPlatform.Win64, TPlatform.OSX32, TPlatform.iOS32, TPlatform.AndroidArm32, TPlatform.LinuxIntel64,
        TPlatform.AndroidArm64, TPlatform.OSX64];
    TCompilerVersion.D10_4 : result := [TPlatform.Win32, TPlatform.Win64, TPlatform.OSX64, TPlatform.iOS32, TPlatform.iOS64, TPlatform.AndroidArm32,
        TPlatform.AndroidArm64, TPlatform.LinuxIntel64];
    TCompilerVersion.D11 : result := [TPlatform.Win32, TPlatform.Win64, TPlatform.OSXARM64, TPlatform.OSX64, TPlatform.iOS64, TPlatform.AndroidArm32,
        TPlatform.AndroidArm64, TPlatform.LinuxIntel64];
  else
    raise Exception.Create('AllPlatforms is missing for : ' + CompilerToString(compiler));
  end;
end;


function ValidatePlatform(const target : TCompilerVersion; const platform : TPlatform) : boolean;
begin
  result := platform in AllPlatforms(target);
end;

function PlatformToDPROJPlatform(const value : TPlatform) : string;
begin
  case value of
    TPlatform.AndroidArm32 : result := 'Android';
    TPlatform.AndroidArm64 : result := 'Android64';
  else
    result := GetEnumName(TypeInfo(TPlatform), ord(value));
  end;
end;


class procedure TSystemUtils.GetResourceVersionNumbers(out AMajor, AMinor, ARelease, ABuild: Integer);
var
  HResource: TResourceHandle;
  HResData: THandle;
  PRes: Pointer;
  InfoSize: DWORD;
  FileInfo: PVSFixedFileInfo;
  FileInfoSize: DWORD;
begin
  AMajor := 0;
  AMinor := 0;
  ARelease := 0;
  ABuild := 0;
  HResource := FindResource(HInstance, MakeIntResource(VS_VERSION_INFO), RT_VERSION);
  if HResource <> 0 then
  begin
    HResData:=LoadResource(HInstance, HResource);
    if HResData <> 0 then
    begin
      PRes:=LockResource(HResData);
      if Assigned(PRes) then
      begin
        InfoSize := SizeofResource(HInstance, HResource);
        if InfoSize = 0 then
          exit; //we do not want to raise errors
        if VerQueryValue(PRes, '\', Pointer(FileInfo), FileInfoSize) then
        begin
          AMajor := FileInfo.dwFileVersionMS shr 16;
          AMinor := FileInfo.dwFileVersionMS and $FFFF;
          ARelease := FileInfo.dwFileVersionLS shr 16;
          ABuild := FileInfo.dwFileVersionLS and $FFFF;
        end;
      end;
    end;
  end;
end;

class function TSystemUtils.GetVersionString: string;
var
  Major, Minor, Release, Build: Integer;
begin
  GetResourceVersionNumbers(Major,Minor,Release,Build);
  result := Format('%d.%d.%d.%d',[Major,Minor,Release,Build]);
end;



function SpaceString(stringLen : integer) : string;
begin
  result := StringOfChar(' ', stringLen);
end;



procedure DoPrettyFormatXML(const node: IXMLDOMNode; const indentSize: integer; var indentLen: integer);
const
  CRLF = #13#10;
  TAB = #9;
var
  i: Integer;
  newnode: IXMLDOMNode;
  childNode: IXMLDOMNode;
  siblingNode : IXMLDOMNode;
  stmp: string;
  sDebug: string;

  function hasChildElements(const anode : IXMLDOMNode) : boolean;
  var
    j : integer;
    cnode : IXMLDomNode;
  begin
    result := False;
    for j := 0 to anode.childNodes.length -1 do
    begin
      cNode := anode.childNodes.item[j];
      if (cNode.nodeType = NODE_ELEMENT) or (cNode.nodeType = NODE_CDATA_SECTION) then
      begin
        result := True;
        break;
      end;
    end;

  end;

  function GetNextElement(const aNode : IXMLDOMNode) : IXMLDOMNode;
  begin
    result := aNode.nextSibling;
    while result <> nil do
    begin
      if result.nodeType = NODE_ELEMENT then
        break;
      result := result.nextSibling;
    end;
  end;

begin
  if node.nodeType = NODE_TEXT then
    exit;
  if node.nodeType = NODE_TEXT then
    exit;
  if node.childNodes.length > 0 then
  begin
    sDebug := node.nodeName;
    if hasChildElements(node) then
    begin
      Inc(indentLen, indentSize);
      i := 0;
      while i < node.childNodes.length do
      begin
        childNode := node.childNodes.item[i];
        sDebug := childNode.nodeName;
        case childNode.nodeType of //
          NODE_ELEMENT, NODE_CDATA_SECTION:
            begin
              stmp := CRLF + SpaceString(indentLen);
              newnode := node.ownerDocument.createTextNode(stmp);
              node.insertBefore(newnode, childNode);

              if hasChildElements(childNode) then
                DoPrettyFormatXML(childNode, indentSize, indentLen);

              siblingNode := GetNextElement(childNode);
              stmp := CRLF + SpaceString(indentLen);
              newnode := node.ownerDocument.createTextNode(stmp);
              if siblingNode <> nil then
                node.insertBefore(newnode, siblingNode);
              Inc(i);
            end;
          NODE_TEXT:
            begin
              // remove any old formatting nodes.
              node.removeChild(childNode);
              continue;
            end
        end;
        Inc(i);
      end;
      Dec(indentLen, indentSize);
      stmp := CRLF + SpaceString(indentLen);
      newnode := node.ownerDocument.createTextNode(stmp);
      node.appendChild(newnode);
    end;
  end;
end;

procedure PrettyFormatXML(const node: IXMLDOMNode; const indentSize: integer);
var
 indentLen: integer;
begin
  indentLen := 0;
  DoPrettyFormatXML(node, indentSize, indentLen);
end;


end.
