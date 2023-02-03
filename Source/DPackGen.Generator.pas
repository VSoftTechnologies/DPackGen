unit DPackGen.Generator;

interface

uses
  DPackGen.Interfaces;

type
  TGenerator = class
  protected
    class function GenerateDPK(const definition : IPackageDefinition; const targetPlatform: ITargetPlatform; const outputFolder : string ): boolean;
    class function GenerateDPROJ(const definition : IPackageDefinition;const targetPlatform: ITargetPlatform; const outputFolder : string): boolean;

  public
    class function Generate(const definition : IPackageDefinition; const targetPlatform: ITargetPlatform; const baseFolder : string): boolean;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  WinApi.ActiveX,
  DPackGen.Utils,
  DPackGen.MSXML;

{ TDpkGenerator }

class function TGenerator.Generate(const definition : IPackageDefinition; const targetPlatform: ITargetPlatform; const baseFolder : string): boolean;
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

class function TGenerator.GenerateDPK(const definition : IPackageDefinition; const targetPlatform: ITargetPlatform; const outputFolder : string): boolean;
var
  sList : TStringList;
  packageFileName : string;
  i : integer;
  require : string;
  fileEntry : string;

begin
    packageFileName := TPath.Combine(outputFolder, definition.Name + '.dpk');
    WriteLn('Generating package : ' + packageFileName);
    sList := TStringList.Create;
    try
      sList.Add('package ' + definition.Name + ';');
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
        fileEntry := targetPlatform.Files.Strings[i];
        fileEntry := ChangeFileExt(ExtractFileName(fileEntry),'') + ' in ''' + fileEntry + '''';
        
        if i = targetPlatform.Files.Count -1 then
          fileEntry := fileEntry + ';'
        else
          fileEntry := fileEntry + ',';
        sList.Add('  ' + fileEntry);

      end;
      sList.Add('');
      sList.Add('end.');


      sList.SaveToFile(packageFileName);
      result := true;
    finally
      sList.Free;
    end;

end;

class function TGenerator.GenerateDPROJ(const definition : IPackageDefinition;const targetPlatform: ITargetPlatform; const outputFolder: string): boolean;
var
  xmlDoc : IXMLDomDocument;
  projectElement : IXMLDOMElement;
  propertyGroupElement : IXMLDOMElement;
  tmpElement : IXMLDOMElement;
  sPlatform : string;
  i : integer;
  
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

begin
  result := false;
  xmlDoc := CoDOMDocument60.Create;
  projectElement := CreateElement('Project');
  xmlDoc.documentElement := projectElement;

  propertyGroupElement := AddElement(projectElement, 'PropertyGroup');
    AddElement(propertyGroupElement, 'ProjectGuid', definition.ProjectGUID);
    AddElement(propertyGroupElement, 'MainSource', definition.Name + '.dpk');
    AddElement(propertyGroupElement, 'ProjectVersion', CompilerToDPROJVersion(targetPlatform.CompilerVersion));
    AddElement(propertyGroupElement, 'FrameworkType', definition.FrameworkType);
    AddElement(propertyGroupElement, 'Base', 'true');
    tmpElement := AddElement(propertyGroupElement, 'Config', 'Debug');
    tmpElement.setAttribute('Condition', '''$(Config)''==''''');

    sPlatform := PlatformToDPROJPlatform(targetPlatform.Platforms[0]);
    //TODO : Check which version of delphi this was added, it's not in XE2
    tmpElement := AddElement(propertyGroupElement, 'Platform', sPlatform);
    tmpElement.setAttribute('Condition', '''$(Platform)''==''''');

    AddElement(propertyGroupElement, 'TargetedPlatforms', Length(targetPlatform.Platforms).ToString());
    AddElement(propertyGroupElement, 'AppType', 'Package');


  propertyGroupElement := AddElement(projectElement, 'PropertyGroup');
  propertyGroupElement.setAttribute('Condition', '''$(Config)''==''Base'' or ''$(Base)''!=''''');
      AddElement(propertyGroupElement, 'Base', 'true');

  //Base platform Config
  for var platform in targetPlatform.Platforms  do
  begin
    sPlatform := PlatformToDPROJPlatform(platform);
    propertyGroupElement := AddElement(projectElement, 'PropertyGroup');    
    propertyGroupElement.setAttribute('Condition', '(''$(Platform)''==''' + sPlatform + ''' and ''$(Base)''==''true'') or ''$(Base_' + sPlatform + ')''!=''''' );
      AddElement(propertyGroupElement, 'Base_' + sPlatform, 'true');    
      AddElement(propertyGroupElement, 'CfgParent', 'Base');    
      AddElement(propertyGroupElement, 'Base', 'true');
  end;
  //Base Debug Config
  propertyGroupElement := AddElement(projectElement, 'PropertyGroup');    
  propertyGroupElement.setAttribute('Condition', '''$(Config)''==''Debug'' or ''$(Cfg_1)''!=''''');
      AddElement(propertyGroupElement, 'Cfg_1', 'true');
      AddElement(propertyGroupElement, 'CfgParent', 'Base');    
      AddElement(propertyGroupElement, 'Base', 'true');

  //Base Release Config
  propertyGroupElement := AddElement(projectElement, 'PropertyGroup');    
  propertyGroupElement.setAttribute('Condition', '''$(Config)''==''Release'' or ''$(Cfg_2)''!=''''');
      AddElement(propertyGroupElement, 'Cfg_2', 'true');
      AddElement(propertyGroupElement, 'CfgParent', 'Base');    
      AddElement(propertyGroupElement, 'Base', 'true');


  //Debug/Release Platform Configs
  for var platform in targetPlatform.Platforms  do
  begin
    sPlatform := PlatformToDPROJPlatform(platform);
    //Debug
    propertyGroupElement := AddElement(projectElement, 'PropertyGroup');    
    propertyGroupElement.setAttribute('Condition', '(''$(Platform)''==''' + sPlatform + ''' and ''$(Cfg_1)''==''true'') or ''$(Cfg_1_' + sPlatform + ')''!=''''' );
      AddElement(propertyGroupElement, 'Cfg_1_' + sPlatform, 'true');    
      AddElement(propertyGroupElement, 'CfgParent', 'Cfg_1');    
      AddElement(propertyGroupElement, 'Cfg_1', 'true');
      AddElement(propertyGroupElement, 'Base', 'true');

    //Release
    propertyGroupElement := AddElement(projectElement, 'PropertyGroup');    
    propertyGroupElement.setAttribute('Condition', '(''$(Platform)''==''' + sPlatform + ''' and ''$(Cfg_2)''==''true'') or ''$(Cfg_2_' + sPlatform + ')''!=''''' );
      AddElement(propertyGroupElement, 'Cfg_2_' + sPlatform, 'true');    
      AddElement(propertyGroupElement, 'CfgParent', 'Cfg_2');    
      AddElement(propertyGroupElement, 'Cfg_2', 'true');
      AddElement(propertyGroupElement, 'Base', 'true');
  end;

  //Base Config
  propertyGroupElement := AddElement(projectElement, 'PropertyGroup');    
  propertyGroupElement.setAttribute('Condition', '''$(Base)''!=''''');
      AddElement(propertyGroupElement, 'SanitizedProjectName', StringReplace(definition.Name, '.','_',[rfReplaceAll]) );
      AddElement(propertyGroupElement, 'DllSuffix', targetPlatform.LibSuffix );
      AddElement(propertyGroupElement, 'GenPackage', 'true');
      AddElement(propertyGroupElement, 'GenDll', 'true');
      AddElement(propertyGroupElement, 'Description', 'true');
  
  
  PrettyFormatXML(xmlDoc.documentElement,4);
  xmlDoc.save('c:\temp\test.dproj');


end;


initialization
  CoInitialize(nil);

end.
