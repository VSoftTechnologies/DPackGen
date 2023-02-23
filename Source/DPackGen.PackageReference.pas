unit DPackGen.PackageReference;


interface

uses
  JsonDataObjects,
  Spring.Collections,
  DPackGen.Interfaces;


type
  TPackageReference = class(TInterfacedObject, IDPMPackageReference)
  private
    FId : string;
    FVersion : string;
    FPlatform : string;
    FDependencies : IList<IDPMPackageReference>;
  protected
    function GetId : string;
    procedure SetId(const value : string);
    function GetVersion : string;
    procedure SetVersion(const value : string);
    function GetPlatform : string;
    procedure SetPlatform(const value : string);
    function GetDependencies : IList<IDPMPackageReference>;
    procedure SetDependencies(const value : IList<IDPMPackageReference>);
    function LoadFromJson(const jsonObject : TJsonObject) : boolean;
  public
    constructor Create;
  end;

implementation

uses
  VSoft.SemanticVersion;

{ TPackageReference }

constructor TPackageReference.Create;
begin
  FDependencies := TCollections.CreateList<IDPMPackageReference>;
end;

function TPackageReference.GetDependencies: IList<IDPMPackageReference>;
begin
  result := FDependencies;
end;

function TPackageReference.GetId: string;
begin
  result := FId;
end;

function TPackageReference.GetPlatform: string;
begin
  result := FPlatform;
end;

function TPackageReference.GetVersion: string;
begin
  result := FVersion;
end;

function TPackageReference.LoadFromJson(const jsonObject: TJsonObject): boolean;
var
  theVersion : TSemanticVersion;
  error : string;
  deps : TJsonArray;
  dep : IDPMPackageReference;
  lresult : boolean;
begin
  result := true;
  FId := jsonObject.S['id'];
  if FId = '' then
  begin
    WriteLn('ERROR: Required package reference property "id" is empty or missing');
    exit(false);
  end;

  FVersion := jsonObject.S['version'];
  if FVersion = '' then
  begin
    WriteLn('ERROR: Required package reference property "version" is empty or missing');
    exit(false);
  end;
  if not TSemanticVersion.TryParseWithError(FVersion, theVersion, error) then
  begin
    WriteLn('ERROR: Invalid semantic version : ' + error);
    exit(false);
  end;
  FPlatform := jsonObject.S['platform'];
  if FVersion = '' then
  begin
    WriteLn('ERROR: Required package reference property "platform" is empty or missing');
    exit(false);
  end;

  deps := jsonObject.A['deps'];
  if deps.Count > 0 then
  begin
    for var i := 0 to deps.Count -1 do
    begin
      dep := TPackageReference.Create;
      lResult := dep.LoadFromJson(deps.O[i]);
      result := lResult and result;
      if lresult then
        FDependencies.Add(dep);
    end;
  end;
end;

procedure TPackageReference.SetDependencies( const value: IList<IDPMPackageReference>);
begin
  FDependencies.Clear;
  FDependencies.AddRange(value);
end;

procedure TPackageReference.SetId(const value: string);
begin
  FId := value;
end;

procedure TPackageReference.SetPlatform(const value: string);
begin
  FPlatform := value;
end;

procedure TPackageReference.SetVersion(const value: string);
begin
  FVersion := value;
end;

end.
