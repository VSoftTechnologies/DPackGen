unit DPackGen.TargetPlatform;

interface

uses
  System.Classes,
  JsonDataObjects,
  DPackGen.Types,
  DPackGen.Interfaces,
  DPackGen.Template.Base;


type
  TTargetPlatform = class(TTemplateBase, ITargetPlatform)
  private
    FCompilerVersion : TCompilerVersion;
    FPlatforms : TArray<TPlatform>;
    FTemplateName : string;
  protected
    function LoadFromJson(const jsonObject: TJsonObject): Boolean;override;
    function GetCompilerVersion : TCompilerVersion;
    function GetPlatforms : TArray<TPlatform>;
    function GetTemplateName : string;
  public
    constructor Create(const projectType : TProjectType);override;
  end;

implementation

uses
  System.SysUtils,
  Spring.Collections,
  DPackGen.Utils;

{ TTargetPlatform }

constructor TTargetPlatform.Create(const projectType : TProjectType);
begin
  inherited;
  FCompilerVersion := TCompilerVersion.UnknownVersion;
  FTemplateName := cUnset;
  FPlatforms := [];
end;

function TTargetPlatform.GetCompilerVersion: TCompilerVersion;
begin
  result := FCompilerVersion;
end;

function TTargetPlatform.GetPlatforms: TArray<TPlatform>;
begin
  result := FPlatforms;
end;

function TTargetPlatform.GetTemplateName: string;
begin
  result := FTemplateName;
end;

function TTargetPlatform.LoadFromJson(const jsonObject: TJsonObject): Boolean;
var
  sValue : string;
  platformStrings : TArray<string>;
  platform : TPlatform;
  platformList : IList<TPlatform>;
  sCompiler : string;
  sTemplate : string;
begin
  result := true;
  sCompiler := jsonObject.S['compiler'];
  if sCompiler = '' then
  begin
    WriteLn('ERROR: Required property [compiler] is missing)');
    result := false;
  end
  else
  begin
    FCompilerVersion := StringToCompilerVersion(sCompiler);
    if FCompilerVersion = TCompilerVersion.UnknownVersion then
    begin
      result := false;
      WriteLn('ERROR: Invalid compiler value [' + sCompiler + ']');
    end;
  end;

  sValue := jsonObject.S['platforms'];
  if sValue = '' then
  begin
    WriteLn('ERROR: Required property [platforms] is missing)');
    result := false;
  end
  else
  begin
    sValue := StringReplace(sValue, ' ', '', [rfReplaceAll]);
    //Logger.Debug('[targetPlatform] platforms : ' + sValue);
    platformStrings := sValue.Split([','], TStringSplitOptions.ExcludeEmpty);

    if Length(platformStrings) > 0 then
    begin
      platformList := TCollections.CreateList<TPlatform>;
      for sValue in platformStrings do
      begin
        platform := StringToPlatform(sValue);
        if platform <> TPlatform.UnknownPlatform then
        begin
          platformList.Add(platform);
          if not ValidatePlatform(FCompilerVersion, platform) then
          begin
            WriteLn('ERROR: Invalid platform value [' + sValue + '] for compiler version [' + sCompiler + ']');
            result := false;
          end;
        end
        else
        begin
          WriteLn('ERROR: Invalid platform value [' + sValue + ']');
          result := false;
        end;
      end;
      FPlatforms := platformList.ToArray();
    end
    else
    begin
      WriteLn('ERROR: At least 1 platform must be specified.');
      result := false;
    end;
  end;

  sTemplate := jsonObject.S['template'];
  if sTemplate <> '' then
    FTemplateName := sTemplate;

  result := inherited LoadFromJson(jsonObject) and result;
end;

end.
