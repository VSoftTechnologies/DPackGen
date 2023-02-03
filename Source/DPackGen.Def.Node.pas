unit DPackGen.Def.Node;

interface

uses
  JsonDataObjects,
  DPackGen.Interfaces,
  DPackGen.Types;

type
  TDefNodeClass = class of TDefNode;

  TDefNode = class(TInterfacedObject, IDefNode)
  protected
    function LoadFromJson(const jsonObject : TJsonObject) : boolean;virtual;abstract;

  end;


implementation

{ TDefNode }

function TDefNode.LoadJsonCollection(const collection: TJSonArray;  const nodeClass: TDefNodeClass;  const action: TConstProc<IInterface>): boolean;
var
  item : IDefNode;
  i : Integer;
  obj : TJsonObject;
begin
  result := true;
  if collection.Count = 0 then
    exit;

  for i := 0 to collection.Count - 1 do
  begin
    item := nodeClass.Create as IDefNode;
    obj := collection.O[i];
    item.LoadFromJson(obj);
    action(item);
  end;
end;

end.
