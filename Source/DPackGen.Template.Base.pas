unit DPackGen.Template.Base;

interface


uses
  System.Classes,
  JsonDataObjects,
  Spring.Collections,
  DPackGen.Types,
  DPackGen.Interfaces;

type
  TTemplateBase = class(TInterfacedObject, ITemplateBase)
  private
    //don't forget to update assign if you add new props
    FProjectType : TProjectType;
    FPreFiles   : TStringList;
    FPostFiles  : TStringList;
    FFiles      : TStringList;
    FRequires   : TStringList;
    FDPKOptions : TStringList;
    FCode       : TStringList;
    FNSPrefix   : TStringList;
    FLibSuffix  : string;
    FFolderNameTemplate : string;
    FDescriptionTemplate : string;
    FMainSourceTemplate : string;
    FDPMPackageReferences : IList<IDPMPackageReference>;
  protected
    procedure Assign(const source : ITemplateBase);virtual;
    function IsTemplate : boolean;virtual;
    function GetPreFiles : TStrings;
    procedure SetPreFiles(const value : TStrings);
    function GetPostFiles : TStrings;
    procedure SetPostFiles(const value : TStrings);

    function GetFiles : TStrings;
    procedure SetFiles(const value : TStrings);
    function GetRequires : TStrings;
    procedure SetRequires(const value : TStrings);

    function GetDPKOptions : TStrings;
    procedure SetDPKOptions(const value : TStrings);

    function GetCode : TStrings;
    procedure SetCode(const value : TStrings);
    function GetNameSpacePrefixes : TStrings;
    procedure SetNameSpacePrefixes(const value : TStrings);


    function GetLibSuffix : string;
    procedure SetLibSuffix(const value : string);

    function GetFolderNameTemplate : string;
    procedure SetFolderNameTemplate(const value : string);
    function GetDescriptionTemplate : string;
    procedure SetDescriptionTemplate(const value : string);
    function GetMainSourceTemplate : string;
    procedure SetMainSourceTemplate(const value : string);

    function GetDPMPackages : IList<IDPMPackageReference>;
    procedure SetDPMPackages(const value : IList<IDPMPackageReference>);

    procedure LoadListFromArray(const jsonArray : TJsonArray; const list : TStringList);

    function LoadFromJson(const jsonObject: TJsonObject): Boolean;virtual;
  public
    constructor Create(const projectType : TProjectType);virtual;

    destructor Destroy;override;

  end;

implementation


{ TTemplateBase }

uses DPackGen.PackageReference;

procedure TTemplateBase.Assign(const source: ITemplateBase);
begin
  FPreFiles.Assign(source.PreFiles);
  FPostFiles.Assign(source.PostFiles);
  FFiles.Assign(source.Files);
  FRequires.Assign(source.Requires);
  FDPKOptions.Assign(source.DPKOptions);
  FCode.Assign(source.Code);
  FNSPrefix.Assign(source.NameSpacePrefixes);
  FLibSuffix  := source.LibSuffix;
  FFolderNameTemplate := source.FolderNameTemplate;
  FDescriptionTemplate := source.DescriptionTemplate;
  FMainSourceTemplate := source.MainSourceTemplate;
  FDPMPackageReferences := source.DPMPackages;

end;

constructor TTemplateBase.Create(const projectType : TProjectType);
begin
  FProjectType := projectType;
  FPreFiles   := TStringList.Create;
  FPostFiles  := TStringList.Create;
  FFiles      := TStringList.Create;
  FCode       := TStringList.Create;
  FRequires   := TStringList.Create;
  FDPKOptions := TStringList.Create;
  FNSPrefix   := TStringList.Create;
  FDPMPackageReferences := TCollections.CreateList<IDPMPackageReference>;
  //Default NS Prefixes
  FNSPrefix.Values['Base'] := 'System;Xml;Data;Datasnap;Web;Soap';
  FNSPrefix.Values['Win32'] := 'Winapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Bde';
  FNSPrefix.Values['Win64'] := 'Winapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win';
  //TODO Add NS prefixes for other platforms;
end;

destructor TTemplateBase.Destroy;
begin
  FPreFiles.Free;
  FPostFiles.Free;
  FFiles.Free;
  FRequires.Free;
  FDPKOptions.Free;
  FCode.Free;
  FNSPrefix.Free;
  inherited;
end;

function TTemplateBase.GetCode: TStrings;
begin
  result := FCode;
end;

function TTemplateBase.GetDescriptionTemplate: string;
begin
  result := FDescriptionTemplate;
end;

function TTemplateBase.GetDPKOptions: TStrings;
begin
  result := FDPKOptions;
end;

function TTemplateBase.GetDPMPackages: IList<IDPMPackageReference>;
begin
  result := FDPMPackageReferences;
end;

function TTemplateBase.GetFiles: TStrings;
begin
  result := FFiles;
end;

function TTemplateBase.GetFolderNameTemplate: string;
begin
  result := FFolderNameTemplate;
end;

function TTemplateBase.GetLibSuffix: string;
begin
  result := FLibSuffix;
end;

function TTemplateBase.GetMainSourceTemplate: string;
begin
  result := FMainSourceTemplate;
end;

function TTemplateBase.GetNameSpacePrefixes: TStrings;
begin
  result := FNSPrefix;
end;

function TTemplateBase.GetPostFiles: TStrings;
begin
  result := FPostFiles;
end;

function TTemplateBase.GetPreFiles: TStrings;
begin
  result := FPreFiles;
end;

function TTemplateBase.GetRequires: TStrings;
begin
  result := FRequires;
end;

function TTemplateBase.IsTemplate: boolean;
begin
  result := false;
end;


function TTemplateBase.LoadFromJson(const jsonObject: TJsonObject): Boolean;
var
  hasChildren : boolean;
  sValue : string;
  collectionObj : TJsonArray;
  nsPrefixObj : TJsonObject;
  packageRef : IDPMPackageReference;
begin
  result := true;
  if IsTemplate then
  begin
    if jsonObject.Contains('template') then
    begin
      result := false;
      WriteLn('ERROR: template property not valid in a template!');
      exit;
    end;
    hasChildren := jsonObject.Count > 1; //name +
    if not hasChildren then
    begin
      if jsonObject.Contains('name') then
        sValue := jsonObject.S['name']
      else
        sValue := 'unamed';
      result := false;
      WriteLn('ERROR: Empty template [' + sValue + ']');
      exit;
    end;
  end
  else
  begin
    //if we point to a template and have props other than compiler + platforms + libsuffix then fail
    if jsonObject.Contains('template') then
    begin
      if jsonObject.Count > 4 then //compiler + platforms + libsuffix + template
      begin
        WriteLn('targetPlatform cannot specify a template and it''s own definition, pick one');
        result := false;
      end;
      exit;
    end;
  end;

  FFolderNameTemplate := jsonObject.S['folderNameTemplate'];
  if FFolderNameTemplate = '' then
  begin
    WriteLn('ERROR: Required property "folderNameTemplate" is empty or missing');
    exit(false);
  end;

  FDescriptionTemplate := jsonObject.S['descriptionTemplate'];
  if FDescriptionTemplate = '' then
  begin
    WriteLn('ERROR: Required property "descriptionTemplate" is empty or missing');
    exit(false);
  end;

  FMainSourceTemplate := jsonObject.S['mainSourceTemplate'];
  if FMainSourceTemplate = '' then
  begin
    WriteLn('ERROR: Required property "mainSourceTemplate" is empty or missing');
    exit(false);
  end;



  FLibSuffix := jsonObject.S['libSuffix'];
  if FLibSuffix = '' then
  begin
    result := false;
    Writeln('ERROR: Required property [libSuffix] is empty or missing');
  end;


  collectionObj := jsonObject.A['files'];
  if collectionObj.Count > 0 then
    LoadListFromArray(collectionObj, FFiles)
  else
  begin
    result := false;
    Writeln('ERROR: Required array [files] is empty or missing');
  end;

  collectionObj := jsonObject.A['requires'];
  if collectionObj.Count > 0 then
    LoadListFromArray(collectionObj, FRequires)
  else
  begin
    result := false;
    Writeln('ERROR: Required array [requires] is empty or missing');
  end;

  collectionObj := jsonObject.A['dpkOptions'];
  if collectionObj.Count > 0 then
    LoadListFromArray(collectionObj, FDPKOptions)
  else
  begin
    result := false;
    Writeln('ERROR: Required array [dpkOptions] is empty or missing');
  end;

  collectionObj := jsonObject.A['preFiles'];
  if collectionObj.Count > 0 then
    LoadListFromArray(collectionObj, FPreFiles);

  collectionObj := jsonObject.A['postFiles'];
  if collectionObj.Count > 0 then
    LoadListFromArray(collectionObj, FPostFiles);

  collectionObj := jsonObject.A['code'];
  if collectionObj.Count > 0 then
    LoadListFromArray(collectionObj, FCode);


  if jsonObject.Contains('namespacePrefix') then
  begin
    nsPrefixObj := jsonObject.O['namespacePrefix'];
    //we don't have the platforms here so cannot validate them, will do that in the targetplatform class
    for var i := 0 to nsPrefixObj.Count -1 do
      FNSPrefix.Values[nsPrefixObj.Names[i]] := nsPrefixObj.S[nsPrefixObj.Names[i]] ;
  end;

  //
  if jsonObject.Contains('dpm') then
  begin

    collectionObj := jsonObject.A['dpm'];
    for var i := 0 to collectionObj.Count -1 do
    begin
      packageRef := TPackageReference.Create;
      result := packageRef.LoadFromJson(collectionObj.O[i]) and result;
      FDPMPackageReferences.Add(packageRef);
    end;
  end;

end;

procedure TTemplateBase.LoadListFromArray(const jsonArray: TJsonArray; const list: TStringList);
var
  i : integer;
  sValue : string;
begin
  if jsonArray.Count = 0 then
    exit;
  for i := 0 to jsonArray.Count - 1 do
  begin
    sValue := jsonArray.S[i];
    list.Add(sValue);
  end;
end;


procedure TTemplateBase.SetCode(const value: TStrings);
begin
  FCode.Assign(value);
end;

procedure TTemplateBase.SetDescriptionTemplate(const value: string);
begin
  FDescriptionTemplate := value;
end;

procedure TTemplateBase.SetDPKOptions(const value: TStrings);
begin
  FDPKOptions.Assign(value);
end;

procedure TTemplateBase.SetDPMPackages(const value: IList<IDPMPackageReference>);
begin
  FDPMPackageReferences.Clear;
  FDPMPackageReferences.AddRange(value);
end;

procedure TTemplateBase.SetFiles(const value: TStrings);
begin
  FFiles.Assign(value);
end;

procedure TTemplateBase.SetFolderNameTemplate(const value: string);
begin
  FFolderNameTemplate := value;
end;

procedure TTemplateBase.SetLibSuffix(const value: string);
begin
  FLibSuffix := value;
end;

procedure TTemplateBase.SetMainSourceTemplate(const value: string);
begin
  FMainSourceTemplate := value;
end;

procedure TTemplateBase.SetNameSpacePrefixes(const value: TStrings);
begin
  FNSPrefix.Assign(value);
end;

procedure TTemplateBase.SetPostFiles(const value: TStrings);
begin
  FPostFiles.Assign(value);
end;

procedure TTemplateBase.SetPreFiles(const value: TStrings);
begin
  FPreFiles.Assign(value);
end;

procedure TTemplateBase.SetRequires(const value: TStrings);
begin
  FRequires.Assign(value);
end;

end.
