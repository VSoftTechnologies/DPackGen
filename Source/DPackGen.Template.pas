unit DPackGen.Template;

interface

uses
  System.Classes,
  JsonDataObjects,
  DPackGen.Interfaces,
  DPackGen.Template.Base;

type
  TTemplate = class(TTemplateBase, ITemplate)
  private
    FName : string;
  protected
   function IsTemplate : boolean;override;
   function LoadFromJson(const jsonObject: TJsonObject): Boolean;override;
   function GetName: string;
  public
  end;

implementation

{ TTemplate }

function TTemplate.GetName: string;
begin
  result := FName;
end;

function TTemplate.IsTemplate: boolean;
begin
  result := true;
end;

function TTemplate.LoadFromJson(const jsonObject: TJsonObject): Boolean;
begin
  result := true;
  FName := jsonObject.S['name'];
  if FName = '' then
  begin
    Writeln('ERROR: Template name is missing or empty');
    result := false;
  end;

  result := result and inherited LoadFromJson(jsonObject);
end;

end.
