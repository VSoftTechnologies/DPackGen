unit DPackGen.Template.Base;

interface


uses
  System.Classes,
  JsonDataObjects,
  DPackGen.Interfaces;

type
  TTemplateBase = class(TInterfacedObject, ITemplateBase)
  private
    FFiles      : TStringList;
    FRequires   : TStringList;
    FDPKOptions : TStringList;
    FLibSuffix  : string;
    FFolderNameTemplate : string;
    FDescriptionTemplate : string;

  protected
    function IsTemplate : boolean;virtual;
    function GetFiles : TStrings;
    procedure SetFiles(const value : TStrings);
    function GetRequires : TStrings;
    procedure SetRequires(const value : TStrings);

    function GetDPKOptions : TStrings;
    procedure SetDPKOptions(const value : TStrings);

    function GetLibSuffix : string;
    procedure SetLibSuffix(const value : string);

    function GetFolderNameTemplate : string;
    procedure SetFolderNameTemplate(const value : string);
    function GetDescriptionTemplate : string;
    procedure SetDescriptionTemplate(const value : string);


    function LoadFromJson(const jsonObject: TJsonObject): Boolean;virtual;
    function LoadFilesFromJson(const filesArray : TJsonArray) : Boolean;
    function LoadRequiresFromJson(const requiresArray : TJsonArray) : Boolean;
    function LoadDPKOptionsFromJson(const dpkOptionsArray : TJsonArray) : Boolean;
  public
    constructor Create;virtual;

    destructor Destroy;override;

  end;

implementation

{ TTemplateBase }

constructor TTemplateBase.Create;
begin
  FFiles      := TStringList.Create;
  FRequires   := TStringList.Create;
  FDPKOptions := TStringList.Create;
end;

destructor TTemplateBase.Destroy;
begin
  FFiles.Free;
  FRequires.Free;
  FDPKOptions.Free;
  inherited;
end;

function TTemplateBase.GetDescriptionTemplate: string;
begin
  result := FDescriptionTemplate;
end;

function TTemplateBase.GetDPKOptions: TStrings;
begin
  result := FDPKOptions;
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

function TTemplateBase.GetRequires: TStrings;
begin
  result := FRequires;
end;

function TTemplateBase.IsTemplate: boolean;
begin
  result := false;
end;

function TTemplateBase.LoadDPKOptionsFromJson(const dpkOptionsArray: TJsonArray): Boolean;
var
  i : integer;
  sValue : string;
begin
  result := true;
  if dpkOptionsArray.Count = 0 then
    exit;
  for i := 0 to dpkOptionsArray.Count - 1 do
  begin
    sValue := dpkOptionsArray.S[i];
    FDPKOptions.Add(sValue);
  end;
end;

function TTemplateBase.LoadFilesFromJson(const filesArray: TJsonArray): Boolean;
var
  i : integer;
  sValue : string;
begin
  result := true;
  if filesArray.Count = 0 then
    exit;
  for i := 0 to filesArray.Count - 1 do
  begin
    sValue := filesArray.S[i];
    FFiles.Add(sValue);
  end;
end;

function TTemplateBase.LoadFromJson(const jsonObject: TJsonObject): Boolean;
var
  hasChildren : boolean;
  sValue : string;
  collectionObj : TJsonArray;
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


  FLibSuffix := jsonObject.S['libSuffix'];
  if FLibSuffix = '' then
  begin
    result := false;
    Writeln('ERROR: Required property [libSuffix] is empty or missing');
  end;


  collectionObj := jsonObject.A['files'];
  if collectionObj.Count > 0 then
    result := LoadFilesFromJson(collectionObj) and result
  else
  begin
    result := false;
    Writeln('ERROR: Required array [files] is empty or missing');
  end;

  collectionObj := jsonObject.A['requires'];
  if collectionObj.Count > 0 then
    result := LoadRequiresFromJson(collectionObj) and result
  else
  begin
    result := false;
    Writeln('ERROR: Required array [requires] is empty or missing');
  end;

  collectionObj := jsonObject.A['dpkOptions'];
  if collectionObj.Count > 0 then
    result := LoadDPKOptionsFromJson(collectionObj) and result
  else
  begin
    result := false;
    Writeln('ERROR: Required array [dpkOptions] is empty or missing');
  end;

end;

function TTemplateBase.LoadRequiresFromJson(const requiresArray: TJsonArray): Boolean;
var
  i : integer;
  sValue : string;
begin
  result := true;
  if requiresArray.Count = 0 then
    exit;
  for i := 0 to requiresArray.Count - 1 do
  begin
    sValue := requiresArray.S[i];
    FRequires.Add(sValue);
  end;
end;

procedure TTemplateBase.SetDescriptionTemplate(const value: string);
begin
  FDescriptionTemplate := value;
end;

procedure TTemplateBase.SetDPKOptions(const value: TStrings);
begin
  FDPKOptions.Assign(value);
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

procedure TTemplateBase.SetRequires(const value: TStrings);
begin
  FRequires.Assign(value);
end;

end.
