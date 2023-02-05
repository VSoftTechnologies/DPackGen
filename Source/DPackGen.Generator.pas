unit DPackGen.Generator;

interface

uses
  DPackGen.Interfaces;

type
  TGenerator = class
  protected
    class function GenerateDPK(const definition : IProjectDefinition; const targetPlatform: ITargetPlatform; const outputFolder : string ): boolean;
    class function GenerateDPROJ(const definition : IProjectDefinition;const targetPlatform: ITargetPlatform; const outputFolder : string): boolean;
  public
    class function Generate(const definition : IProjectDefinition; const targetPlatform: ITargetPlatform; const baseFolder : string): boolean;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.RegularExpressions,
  WinApi.ActiveX,
  DPackGen.Types,
  DPackGen.Utils,
  DPackGen.MSXML;


var
  formRegex : TRegEx;
{ TDpkGenerator }

class function TGenerator.Generate(const definition : IProjectDefinition; const targetPlatform: ITargetPlatform; const baseFolder : string): boolean;
var
  outputFolder : string;
begin
  try
    outputFolder := TPath.Combine(baseFolder, targetPlatform.FolderNameTemplate);
    TDirectory.CreateDirectory(outputFolder);
    result := GenerateDPK(definition, targetPlatform,outputFolder);
    if not result then
      exit;

    result := GenerateDPROJ(definition, targetPlatform,outputFolder);

  except
    on e : Exception do
    begin
      WriteLn('ERROR : Error generating dpk : ' + e.Message);
      exit(false);
    end;

  end;




end;

class function TGenerator.GenerateDPK(const definition : IProjectDefinition; const targetPlatform: ITargetPlatform; const outputFolder : string): boolean;
var
  sList : TStringList;
  packageFileName : string;
  i : integer;
  require : string;
  fileEntry : string;
  formEntry : string;

begin
    packageFileName := TPath.Combine(outputFolder, definition.Name + '.dpk');
    WriteLn('Generating project file : ' + packageFileName);
    sList := TStringList.Create;
    try
      sList.Add(projectStart[definition.ProjectType] + ' ' + definition.Name + ';');
      sList.Add('');
      sList.AddStrings(targetPlatform.DPKOptions);
      sList.Add('');
      sList.Add('requires');
      for i := 0 to targetPlatform.Requires.Count -1 do
      begin
        require := targetPlatform.Requires.Strings[i];
        if i = targetPlatform.Requires.Count -1 then
          require := require + ';'
        else
          require := require + ',';
        sList.Add('  ' + require);

      end;
      sList.Add('');
      sList.Add('contains');
      for i := 0 to targetPlatform.Files.Count -1 do
      begin
        fileEntry := targetPlatform.Files.KeyNames[i];
        formEntry := targetPlatform.Files.ValueFromIndex[i];
        fileEntry := ChangeFileExt(ExtractFileName(fileEntry),'') + ' in ''' + fileEntry + '''';
        if formEntry <> '' then
          fileEntry := fileEntry + ' ' + formEntry;

        if i = targetPlatform.Files.Count -1 then
          fileEntry := fileEntry + ';'
        else
          fileEntry := fileEntry + ',';
        sList.Add('  ' + fileEntry);

      end;
      sList.Add('');

      //for exe and dll's the end. should be in the code.
      if definition.ProjectType = TProjectType.Bpl then
        sList.Add('end.');


      sList.SaveToFile(packageFileName);
      result := true;
    finally
      sList.Free;
    end;

end;

class function TGenerator.GenerateDPROJ(const definition : IProjectDefinition;const targetPlatform: ITargetPlatform; const outputFolder: string): boolean;
var
  xmlDoc : IXMLDomDocument;
  projectElement : IXMLDOMElement;
  propertyGroupElement : IXMLDOMElement;
  itemGroupElement : IXMLDOMElement;
  projectExtensionsElement : IXMLDOMElement;
  borlandProjectElement : IXMLDOMElement;
  delphiPersonalityElement : IXMLDOMElement;
  sourceElement : IXMLDOMElement;
  platformsElement : IXMLDOMElement;
  tmpElement : IXMLDOMElement;
  sPlatform : string;
  sValue : string;
  sMainSource : string;

  //Delphi needs tuples!
  sFormName : string;
  sDesignClass : string;
const
    xmlns = 'http://schemas.microsoft.com/developer/msbuild/2003';


  function CreateElement(const name : string) : IXMLDOMElement;overload;
  begin
    result := xmlDoc.createNode(NODE_ELEMENT, name, xmlns ) as IXMLDOMElement;
  end;
    function CreateElement(const name : string; const value : string) : IXMLDOMElement;overload;
  begin
    result := xmlDoc.createNode(NODE_ELEMENT, name, xmlns ) as IXMLDOMElement;
    result.text := value;
  end;

  function AddElement(const parent : IXMLDOMElement; const name : string) : IXMLDOMElement;overload;
  begin
    result := CreateElement(name);
    parent.appendChild(result);
  end;

  function AddElement(const parent : IXMLDOMElement; const name : string; const value : string) : IXMLDOMElement;overload;
  begin
    result := CreateElement(name, value);
    parent.appendChild(result);
  end;

  function CreatePropertyGroup : IXMLDOMElement;
  begin
    result := AddElement(projectElement, 'PropertyGroup');
  end;

  function CreateItemGroup : IXMLDOMElement;
  begin
    result := AddElement(projectElement, 'ItemGroup');
  end;

  function GetFormType : string;
  begin
    result := '';
    case definition.FrameworkType of
      TFrameworkType.VCL: result := 'dfm';
      TFrameworkType.FMX: result := 'fmx';
    end;
  end;


  procedure ParseFormInfo(const value : string);
  var
    match : TMatch;
  begin
    match := formRegex.Match(value);
    if match.Success then
    begin
      if match.Groups.Count > 3 then
      begin
        sFormName := match.Groups[2].Value;
        sDesignClass := match.Groups[3].Value;
      end
      else
        sFormName := match.Groups[1].Value
    end
    else
    begin
      sFormName := '';
      sDesignClass := '';
    end;
  end;



begin
  result := false;
  xmlDoc := CoDOMDocument60.Create;
  projectElement := CreateElement('Project');
  xmlDoc.documentElement := projectElement;
  sMainSource := definition.Name + projectFileExt[definition.ProjectType] ;

  propertyGroupElement := CreatePropertyGroup;
    AddElement(propertyGroupElement, 'ProjectGuid', definition.ProjectGUID);
    AddElement(propertyGroupElement, 'MainSource', sMainSource);
    AddElement(propertyGroupElement, 'ProjectVersion', CompilerToDPROJVersion(targetPlatform.CompilerVersion));
    AddElement(propertyGroupElement, 'FrameworkType', FrameworkTypeToString(definition.FrameworkType));
    AddElement(propertyGroupElement, 'Base', 'true');
    tmpElement := AddElement(propertyGroupElement, 'Config', 'Release');
    tmpElement.setAttribute('Condition', '''$(Config)''==''''');

    sPlatform := PlatformToDPROJPlatform(targetPlatform.Platforms[0]);
    //TODO : Check which version of delphi this was added, it's not in XE2
    tmpElement := AddElement(propertyGroupElement, 'Platform', sPlatform);
    tmpElement.setAttribute('Condition', '''$(Platform)''==''''');

    AddElement(propertyGroupElement, 'TargetedPlatforms', Length(targetPlatform.Platforms).ToString());
    AddElement(propertyGroupElement, 'AppType', 'Package');

  //setting up the config tree
  propertyGroupElement := CreatePropertyGroup;
  propertyGroupElement.setAttribute('Condition', '''$(Config)''==''Base'' or ''$(Base)''!=''''');
      AddElement(propertyGroupElement, 'Base', 'true');

  //Base platform Config
  for var platform in targetPlatform.Platforms  do
  begin
    sPlatform := PlatformToDPROJPlatform(platform);
    propertyGroupElement := CreatePropertyGroup;
    propertyGroupElement.setAttribute('Condition', '(''$(Platform)''==''' + sPlatform + ''' and ''$(Base)''==''true'') or ''$(Base_' + sPlatform + ')''!=''''' );
      AddElement(propertyGroupElement, 'Base_' + sPlatform, 'true');
      AddElement(propertyGroupElement, 'CfgParent', 'Base');
      AddElement(propertyGroupElement, 'Base', 'true');
  end;
  //Base Debug Config
  propertyGroupElement := CreatePropertyGroup;
  propertyGroupElement.setAttribute('Condition', '''$(Config)''==''Debug'' or ''$(Cfg_1)''!=''''');
      AddElement(propertyGroupElement, 'Cfg_1', 'true');
      AddElement(propertyGroupElement, 'CfgParent', 'Base');
      AddElement(propertyGroupElement, 'Base', 'true');

  //Base Release Config
  propertyGroupElement := CreatePropertyGroup;
  propertyGroupElement.setAttribute('Condition', '''$(Config)''==''Release'' or ''$(Cfg_2)''!=''''');
      AddElement(propertyGroupElement, 'Cfg_2', 'true');
      AddElement(propertyGroupElement, 'CfgParent', 'Base');
      AddElement(propertyGroupElement, 'Base', 'true');


  //Debug/Release Platform Configs
  for var platform in targetPlatform.Platforms  do
  begin
    sPlatform := PlatformToDPROJPlatform(platform);
    //Debug
    propertyGroupElement := CreatePropertyGroup;
    propertyGroupElement.setAttribute('Condition', '(''$(Platform)''==''' + sPlatform + ''' and ''$(Cfg_1)''==''true'') or ''$(Cfg_1_' + sPlatform + ')''!=''''' );
      AddElement(propertyGroupElement, 'Cfg_1_' + sPlatform, 'true');
      AddElement(propertyGroupElement, 'CfgParent', 'Cfg_1');
      AddElement(propertyGroupElement, 'Cfg_1', 'true');
      AddElement(propertyGroupElement, 'Base', 'true');

    //Release
    propertyGroupElement := CreatePropertyGroup;
    propertyGroupElement.setAttribute('Condition', '(''$(Platform)''==''' + sPlatform + ''' and ''$(Cfg_2)''==''true'') or ''$(Cfg_2_' + sPlatform + ')''!=''''' );
      AddElement(propertyGroupElement, 'Cfg_2_' + sPlatform, 'true');
      AddElement(propertyGroupElement, 'CfgParent', 'Cfg_2');
      AddElement(propertyGroupElement, 'Cfg_2', 'true');
      AddElement(propertyGroupElement, 'Base', 'true');
  end;

  //this is where the actual settings begin
  //Base Config
  propertyGroupElement := CreatePropertyGroup;
    propertyGroupElement.setAttribute('Condition', '''$(Base)''!=''''');
      AddElement(propertyGroupElement, 'SanitizedProjectName', StringReplace(definition.Name, '.','_',[rfReplaceAll]) );
      AddElement(propertyGroupElement, 'DllSuffix', targetPlatform.LibSuffix );

      case definition.ProjectType of
        TProjectType.Invalid: ;
        TProjectType.Exe:
        begin
          AddElement(propertyGroupElement, 'GenPackage', 'false');
          AddElement(propertyGroupElement, 'GenDll', 'false');
        end;
        TProjectType.Bpl:
        begin
          AddElement(propertyGroupElement, 'GenPackage', 'true');
          AddElement(propertyGroupElement, 'GenDll', 'true');
        end;

        TProjectType.Dll:
        begin
          AddElement(propertyGroupElement, 'GenPackage', 'false');
          AddElement(propertyGroupElement, 'GenDll', 'true');
        end;
      end;

      AddElement(propertyGroupElement, 'Description', targetPlatform.DescriptionTemplate );
      if definition.ProjectType = TProjectType.Bpl then
      begin
        if definition.PackageType = TPackageType.Runtime then
          AddElement(propertyGroupElement, 'RuntimeOnlyPackage', 'true' )
        else
          AddElement(propertyGroupElement, 'DesignOnlyPackage', 'true' )
      end;
      AddElement(propertyGroupElement, 'DCC_Namespace', targetPlatform.NameSpacePrefixes.Values['Base'] +';$(DCC_Namespace)' );
      AddElement(propertyGroupElement, 'DCC_DcuOutput', '.\$(Platform)\$(Config)' );
      AddElement(propertyGroupElement, 'DCC_ExeOutput', '.\$(Platform)\$(Config)' );

  //platform base settings
  for var platform in targetPlatform.Platforms  do
  begin
    sPlatform := PlatformToDPROJPlatform(platform);
    propertyGroupElement := CreatePropertyGroup;
      propertyGroupElement.setAttribute('Condition', '''$(Base_' + sPlatform + ')''!=''''');
      case platform of
        TPlatform.Win32,
        TPlatform.Win64: AddElement(propertyGroupElement, 'DCC_Namespace', targetPlatform.NameSpacePrefixes.Values[sPlatform] +';$(DCC_Namespace)' );
        TPlatform.OSX32: ;
        TPlatform.OSX64   :  AddElement(propertyGroupElement, 'BT_BuildType', 'Debug' );
        TPlatform.OSXARM64: AddElement(propertyGroupElement, 'BT_BuildType', 'Debug' );
        TPlatform.AndroidArm32: ;
        TPlatform.AndroidArm64: ;
        TPlatform.iOS32: ;
        TPlatform.iOS64: ;
        TPlatform.LinuxIntel64: ;
      end;
  end;

  //configuration settings
  propertyGroupElement := CreatePropertyGroup;
    //Debug
    propertyGroupElement.setAttribute('Condition', '''$(Cfg_1)''!=''''');
      AddElement(propertyGroupElement, 'DCC_Define', 'DEBUG;$(DCC_Define)' );
      AddElement(propertyGroupElement, 'DCC_Optimize', 'false' );
      AddElement(propertyGroupElement, 'DCC_GenerateStackFrames', 'true' );
      AddElement(propertyGroupElement, 'DCC_DebugInfoInExe', 'true' );
      AddElement(propertyGroupElement, 'DCC_RemoteDebug', 'true' );

  propertyGroupElement := CreatePropertyGroup;
    //Release
    propertyGroupElement.setAttribute('Condition', '''$(Cfg_2)''!=''''');
      AddElement(propertyGroupElement, 'DCC_LocalDebugSymbols', 'false' );
      AddElement(propertyGroupElement, 'DCC_Define', 'RELEASE;$(DCC_Define)' );
      AddElement(propertyGroupElement, 'DCC_SymbolReferenceInfo', '0' );
      AddElement(propertyGroupElement, 'DCC_DebugInformation', '0' );

//configuration settings
  propertyGroupElement := CreatePropertyGroup;
    //Debug-Win32
    propertyGroupElement.setAttribute('Condition', '''$(Cfg_1_Win32)''!=''''');
      AddElement(propertyGroupElement, 'DCC_RemoteDebug', 'false' );


  //files itemgroup
  itemGroupElement := CreateItemGroup;
    tmpElement := AddElement(itemGroupElement, 'DelphiCompile');
      tmpElement.setAttribute('Include','$(MainSource)');
    AddElement(tmpElement,'MainSource', 'MainSource');
    for var i := 0 to targetPlatform.Files.Count -1 do
    begin
      tmpElement := AddElement(itemGroupElement, 'DCCReference');
      tmpElement.setAttribute('Include', targetPlatform.Files.KeyNames[i]);
      sValue := targetPlatform.Files.ValueFromIndex[i];
      if sValue <> '' then
      begin
        ParseFormInfo(sValue);
        if sFormName <> '' then
        begin
          AddElement(tmpElement, 'Form', sFormName);
          AddElement(tmpElement, 'FormType', GetFormType);
          if sDesignClass <> '' then
            AddElement(tmpElement, 'DesignClass', sDesignClass);
        end;
      end;
    end;
    tmpElement := AddElement(itemGroupElement, 'BuildConfiguration');
      tmpElement.setAttribute('Include','Release');
      AddElement(tmpElement, 'Key', 'Cfg_2');
      AddElement(tmpElement, 'CfgParent', 'Base');

    tmpElement := AddElement(itemGroupElement, 'BuildConfiguration');
      tmpElement.setAttribute('Include','Base');
      AddElement(tmpElement, 'Key', 'Base');

    tmpElement := AddElement(itemGroupElement, 'BuildConfiguration');
      tmpElement.setAttribute('Include','Debug');
      AddElement(tmpElement, 'Key', 'Cfg_1');
      AddElement(tmpElement, 'CfgParent', 'Base');

  projectExtensionsElement := AddElement(projectElement, 'ProjectExtensions');
      AddElement(projectExtensionsElement, 'Borland.Personality', 'Borland.Personality.12'); //TODO : Lookup for personality version!
      AddElement(projectExtensionsElement, 'Borland.ProjectType', borlandProjectType[definition.ProjectType] );
      borlandProjectElement := AddElement(projectExtensionsElement, 'BorlandProject');
        delphiPersonalityElement := AddElement(borlandProjectElement, 'Delphi.Personality');
          sourceElement := AddElement(delphiPersonalityElement, 'Source');
            tmpElement := AddElement(sourceElement, 'Source', sMainSource );
            tmpElement.setAttribute('Name','MainSource');
        platformsElement := AddElement(borlandProjectElement, 'Platforms');
        for var platform in targetPlatform.Platforms do
        begin
          tmpElement := AddElement(platformsElement, 'Platform', 'True');
          tmpElement.setAttribute('value',PlatformToDPROJPlatform(platform));
        end;

//  <Import Condition="Exists('$(BDS)\Bin\CodeGear.Delphi.Targets')" Project="$(BDS)\Bin\CodeGear.Delphi.Targets"/>
//    <Import Condition="Exists('$(APPDATA)\Embarcadero\$(BDSAPPDATABASEDIR)\$(PRODUCTVERSION)\UserTools.proj')" Project="$(APPDATA)\Embarcadero\$(BDSAPPDATABASEDIR)\$(PRODUCTVERSION)\UserTools.proj"/>
//    <Import Project="$(MSBuildProjectName).deployproj" Condition="Exists('$(MSBuildProjectName).deployproj')"/>

  //TODO : This has changed many times over the versions, generate based on compiler version!
  tmpElement := AddElement(projectElement, 'Import' );
    tmpElement.setAttribute('Condition', 'Exists(''$(BDS)\Bin\CodeGear.Delphi.Targets'')');
    tmpElement.setAttribute('Project', '$(BDS)\Bin\CodeGear.Delphi.Targets');

  tmpElement := AddElement(projectElement, 'Import' );
    tmpElement.setAttribute('Condition', 'Exists(''$(APPDATA)\Embarcadero\$(BDSAPPDATABASEDIR)\$(PRODUCTVERSION)\UserTools.proj'')');
    tmpElement.setAttribute('Project', '$(APPDATA)\Embarcadero\$(BDSAPPDATABASEDIR)\$(PRODUCTVERSION)\UserTools.proj');

  tmpElement := AddElement(projectElement, 'Import' );
    tmpElement.setAttribute('Condition', 'Exists(''$(MSBuildProjectName).deployproj'')');
    tmpElement.setAttribute('Project', '$(MSBuildProjectName).deployproj');

    
  PrettyFormatXML(xmlDoc.documentElement,4);
  xmlDoc.save('c:\temp\test.dproj');


end;


initialization
  CoInitialize(nil);
  formRegex := TRegEx.Create('\{\s*(\w+)\s*\}|\{\s*(\w+)\s*:\s*(\w+)\s*\}',[roIgnoreCase])

end.
