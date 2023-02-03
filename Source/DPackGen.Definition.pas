unit DPackGen.Definition;

interface

uses
  System.Classes,
  System.RegularExpressions,
  JsonDataObjects,
  Spring.Collections,
  DPackGen.Types,
  DPackGen.Interfaces;

type
  TPackageDefinition = class(TInterfacedObject, IPackageDefinition)
  private
    FFileName : string;
    FSchemaVersion : integer;
    FName : string;
    FProjectGUID : string;
    FPackagesFolder : string;
    FFrameworkType : string;
    FPackageType : TPackageType;

    FTemplates : IList<ITemplate>;
    FTargetPlatforms : IList<ITargetPlatform>;

    FCurrentTokens : TStringList;
  protected
    function GetName: string;
    function GetProjectGUID : string;
    function GetFrameworkType : string;
    function GetPackageType : TPackageType;

    class function InternalReadDefinitionJson(const fileName : string; const jsonObject : TJsonObject) : IPackageDefinition;
    function LoadFromJson(const jsonObject : TJsonObject) : boolean;
    function LoadTemplatesFromJson(const templatesArray : TJsonArray) : boolean;
    function LoadTemplateFromJson(const templateObj : TJsonObject; const templateNo : integer) : boolean;
    function LoadTargetPlatformsFromJson(const targetPlatformsArray : TJsonArray) : boolean;


    function TokenMatchEvaluator(const match : TMatch) : string;
    procedure GetTokensForTargetPlatform(const targetPlatform : ITargetPlatform; const list : TStringList);
    function ReplaceTargetPlatformTokens : boolean;

    function FindTemplate(const name : string) : ITemplate;
    function ApplyTemplates : Boolean;


    function Generate : boolean;

    constructor Create(const fileName : string);

  public
    class function LoadFromFile(const fileName : string) : IPackageDefinition;

  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  DPackGen.Template,
  DPackGen.Utils,
  DPackGen.TargetPlatform,
  DPackGen.Generator;


{ TPackageDefinition }

function TPackageDefinition.FindTemplate(const name : string) : ITemplate;
begin
  result := FTemplates.FirstOrDefault(
    function(const item : ITemplate) : boolean
    begin
      result := SameText(name, item.Name);
    end);
end;


function TPackageDefinition.ApplyTemplates: Boolean;
var
  template : ITemplate;
  targetPlatform : ITargetPlatform;
begin
  result := true;
  WriteLn('Applying templates..');
  //if any targetPlatforms reference a template
  if not FTargetPlatforms.Any(
    function(const item : ITargetPlatform) : boolean
    begin
      result := item.TemplateName <> '';
    end) then  exit;

  //if we don't have templates then the spec is not valid.
  if not FTemplates.Any then
  begin
    WriteLn('No templates were found but targetPlatforms reference a template.');
    exit(false);
  end;

  for targetPlatform in FTargetPlatforms do
  begin
    if SameText(targetPlatform.TemplateName, cUnset) then
      continue;
    template := FindTemplate(targetPlatform.TemplateName);
    if template <> nil then
    begin
      //libsuffix can be set in targetplatform or template.
      if targetPlatform.LibSuffix = '' then
        targetPlatform.LibSuffix := template.LibSuffix;

      targetPlatform.FolderNameTemplate := template.FolderNameTemplate;
      targetPlatform.DescriptionTemplate := template.DescriptionTemplate;

      targetPlatform.Files.Assign(template.Files);
      targetPlatform.Requires.Assign(template.Requires);
      targetPlatform.DPKOptions.Assign(template.DPKOptions);
    end
    else
    begin
      WriteLn('A referenced template [' + targetPlatform.TemplateName + '] was not found.');
      exit(false);
    end;

  end;



end;

constructor TPackageDefinition.Create(const fileName: string);
begin
  FFileName := fileName;
  FTemplates := TCollections.CreateList<ITemplate>;
  FTargetPlatforms := TCollections.CreateList<ITargetPlatform>;
  FFrameworkType := 'None';
  FPackageType := TPackageType.Runtime;
end;

function TPackageDefinition.Generate: boolean;
var
  targetPlatform : ITargetPlatform;
  sPackagesFolder : string;

begin
  result :=  ApplyTemplates;
  if not result then
    exit;

  result := ReplaceTargetPlatformTokens;
  if not result then
    exit;

  sPackagesFolder := FPackagesFolder;

  if TPathUtils.IsRelativePath(sPackagesFolder) then
  begin
    if TPathUtils.IsRelativePath(FFileName) then
    begin
      FFileName := TPath.Combine(GetCurrentDir, FFileName);
    end;
    sPackagesFolder := TPath.Combine(TPath.GetDirectoryName(FFileName), sPackagesFolder);
    sPackagesFolder := ExpandFileName(sPackagesFolder);
  end;



  for targetPlatform in FTargetPlatforms do
  begin
    Writeln('Generating package for Compiler Version [' + CompilerToString(targetPlatform.CompilerVersion) + ']');
    result := result and TGenerator.Generate(self, targetPlatform, sPackagesFolder);
    if not result then
      exit;
  end;

  result := true;




end;

function TPackageDefinition.GetFrameworkType: string;
begin
  result := FFrameworkType;
end;

function TPackageDefinition.GetName: string;
begin
  result := FName;
end;

function TPackageDefinition.GetPackageType: TPackageType;
begin
  result := FPackageType;
end;

function TPackageDefinition.GetProjectGUID: string;
begin
  result := FProjectGUID;
end;

procedure TPackageDefinition.GetTokensForTargetPlatform(const targetPlatform: ITargetPlatform; const list: TStringList);
begin
  list.Clear;
  list.Add('compiler=' + CompilerToString(targetPlatform.CompilerVersion));
  list.Add('compilerNoPoint=' + CompilerToStringNoPoint(targetPlatform.CompilerVersion));
  list.Add('compilerCodeName=' + CompilerCodeName(targetPlatform.CompilerVersion));
  list.Add('compilerWithCodeName=' + CompilerWithCodeName(targetPlatform.CompilerVersion));
  list.Add('compilerVersion=' + CompilerToCompilerVersionIntStr(targetPlatform.CompilerVersion));
  list.Add('bdsVersion=' + CompilerToBDSVersion(targetPlatform.CompilerVersion));
  list.Add('libSuffix=' + CompilerToLibSuffix(targetPlatform.CompilerVersion));
end;

class function TPackageDefinition.InternalReadDefinitionJson(const fileName: string; const jsonObject: TJsonObject): IPackageDefinition;
begin
 result := nil;
  if not jsonObject.Contains('definitionSchemaVersion') then
  begin
    WriteLn('json document does not have a definitionSchemaVersion entry, this is probably not a pkdef file');
    exit;
  end;

  result := TPackageDefinition.Create(fileName);
  if not result.LoadFromJson(jsonObject) then
    result := nil;
end;

class function TPackageDefinition.LoadFromFile(const fileName: string): IPackageDefinition;
var
  jsonObj : TJsonObject;
begin
  result := nil;
  if not FileExists(fileName) then
  begin
    WriteLn('Definition file : [' + filename + '] does not exist');
    exit;
  end;
  try
    jsonObj := TJsonObject.ParseFromFile(fileName) as TJsonObject;
    try
      Result := InternalReadDefinitionJson(fileName, jsonObj);

    finally
      jsonObj.Free;
    end;
  except
    on e : Exception do
    begin
      WriteLn('Error parsing spec json : ' + e.Message);
      exit;
    end;
  end;

end;

function TPackageDefinition.LoadFromJson(const jsonObject: TJsonObject): boolean;
var
  templatesArray : TJsonArray;
  targetPlatformsArray : TJsonArray;
  sValue : string;
  framework : TFrameworkType;
begin
  result := true;
  FSchemaVersion := jsonObject.I['definitionSchemaVersion'];
  if FSchemaVersion > CurrentSchemaVersion then
  begin
    WriteLn('ERROR: Definition requires a newer version of this tool.');
    exit(false);
  end;
  FName := jsonObject.S['name'];
  if FName = '' then
  begin
    WriteLn('ERROR: Required property [name] is empty or missing');
    exit(false);
  end;
  FPackagesFolder := jsonObject.S['packagesFolder'];
  if FPackagesFolder = '' then
  begin
    WriteLn('ERROR: Required property [packagesFolder] is empty or missing');
    exit(false);
  end;

  FProjectGUID := jsonObject.S['projectGUID'];
  if FProjectGUID = '' then
  begin
    WriteLn('ERROR: Required property [projectGUID] is empty or missing');
    exit(false);
  end;

  sValue := jsonObject.S['frameworkType'];
  if sValue <> '' then
  begin
    framework := StringToFrameworkType(sValue);
    if framework <> TFrameworkType.Invalid then
      FFrameworkType := sValue;
  end;
  sValue := jsonObject.S['packageType'];
  if sValue = '' then
  begin
    WriteLn('ERROR: Required property [packageType] is empty or missing');
    exit(false);
  end;
  FPackageType := StringToPackageType(sValue);
  if FPackageType = TPackageType.Invalid then
  begin
    WriteLn('ERROR: packageType [' + sValue + '] is not valid (Runtime or Designtime)');
    exit(false);
  end;


  if jsonObject.Contains('templates') then
  begin
    //Logger.Debug('Reading spec templates');
    templatesArray := jsonObject.A['templates'];
    if templatesArray.Count > 0 then
      result := LoadTemplatesFromJson(templatesArray) and result
    else
      WriteLn('WARN: Empty [templates] array, did you forget to define them?');
  end;

  if not jsonObject.Contains('targetPlatforms') then
  begin
    WriteLn('ERROR: Required element [targetPlatforms] not found!');
    result := false;
  end
  else
  begin
    //Logger.Debug('Reading spec targetPlatforms');
    targetPlatformsArray := jsonObject.A['targetPlatforms'];
    if targetPlatformsArray.Count = 0 then
    begin
      WriteLn('ERROR: targetPlatforms array is empty');
      exit(false);
    end;
    result := LoadTargetPlatformsFromJson(targetPlatformsArray) and result;
  end;

end;

function TPackageDefinition.LoadTargetPlatformsFromJson(const targetPlatformsArray : TJsonArray): boolean;
var
  i : integer;
  targetPlatform : ITargetPlatform;
begin
  result := true;
  if targetPlatformsArray.Count = 0 then
  begin
    WriteLn('ERROR: No targetPlatforms found, at least 1 is required');
    exit(false);
  end;

  for i := 0 to targetPlatformsArray.Count - 1 do
  begin
    targetPlatform := TTargetPlatform.Create;
    FTargetPlatforms.Add(targetPlatform);
    result := targetPlatform.LoadFromJson(targetPlatformsArray.O[i]) and result;
  end;

end;

function TPackageDefinition.LoadTemplateFromJson(const templateObj: TJsonObject;  const templateNo: integer): boolean;
var
  template : ITemplate;
begin
  result := true;
  if not templateObj.Contains('name') then
  begin
    result := false;
    Writeln('ERROR: Template #' + IntToStr(templateNo) + ' is missing the required name field!');
  end;
  template := TTemplate.Create;
  result := result and template.LoadFromJson(templateObj);
  FTemplates.Add(template);
end;

function TPackageDefinition.LoadTemplatesFromJson(const templatesArray: TJsonArray): boolean;
var
  i : integer;
begin
  result := true;
  if templatesArray.Count > 0 then
  begin
    for i := 0 to templatesArray.Count - 1 do
      result := LoadTemplateFromJson(templatesArray.O[i], i + 1) and result;
  end;
end;

function TPackageDefinition.ReplaceTargetPlatformTokens: boolean;
var
  tokenList : TStringList;
  targetPlatform : ITargetPlatform;
  regEx : TRegEx;
  evaluator : TMatchEvaluator;
  i : integer;
begin
  result := true;
  WriteLn('Replacing tokens..');
  tokenList := TStringList.Create;
  FCurrentTokens := tokenList;
  try
    try
      regEx := TRegEx.Create(cTokenRegex, [TRegExOption.roIgnoreCase]);
      evaluator := TokenMatchEvaluator; //work around for compiler overload resolution issue.
      for targetPlatform in FTargetPlatforms do
      begin
        GetTokensForTargetPlatform(targetPlatform, tokenList);
        targetPlatform.LibSuffix := regEx.Replace(targetPlatform.LibSuffix, evaluator);
        targetPlatform.FolderNameTemplate := Trim(regEx.Replace(targetPlatform.FolderNameTemplate, evaluator));
        targetPlatform.DescriptionTemplate := Trim(regEx.Replace(targetPlatform.DescriptionTemplate, evaluator));
        //we need this for the dpk options
        tokenList.Add('description=' + targetPlatform.DescriptionTemplate);

        for i := 0 to targetPlatform.Files.Count -1 do
          targetPlatform.Files.Strings[i] := Trim(regex.Replace(targetPlatform.Files.Strings[i], evaluator));

        for i := 0 to targetPlatform.DPKOptions.Count -1 do
          targetPlatform.DPKOptions.Strings[i] := Trim(regex.Replace(targetPlatform.DPKOptions.Strings[i], evaluator));
      end;


    finally
      FCurrentTokens := nil;
      tokenList.Free;
    end;
  except
    on e : Exception do
    begin
      WriteLn('Error replacing tokens : ' + e.Message);
      result := false;
    end;
  end;

end;

function TPackageDefinition.TokenMatchEvaluator(const match : TMatch) : string;
begin
  if match.Success and (match.Groups.Count = 2) then
  begin
    if FCurrentTokens.IndexOfName(match.Groups.Item[1].Value) <> -1 then
    begin
    // WriteLn('Replacing [' + match.Groups.Item[1].Value + ']');
      result := FCurrentTokens.Values[match.Groups.Item[1].Value];
    //  WriteLn('  with [' + result + ']');
    end
    else
      raise Exception.Create('Unknown token [' + match.Groups.Item[1].Value + ']');
  end
  else
    result := match.Value;
end;


end.
