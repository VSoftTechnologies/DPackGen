unit DPackGen.Utils.Path;

interface

type
  TPathUtils = class
  public
    class function IsPathRooted(const value : string) : boolean;
    class function IsRelativePath(const value : string) : boolean;
    class function CompressRelativePath(basePath : string; path : string) : string;overload;
    class function ContainsWildcard(const value : string) : boolean;
    class function StripBase(const base : string; const fileName : string) : string;
    class function CreateRelativePath(const base : string; const fileName : string) : string;
  end;

implementation

uses
  WinApi.ShLwApi,
  WinApi.Windows,
  System.SysUtils,
  System.IOUtils,
  System.RegularExpressions,
  Spring.Collections;

function IndexOfAny(const value : string; const AnyOf : array of Char; StartIndex, Count : Integer) : Integer;
var
  I : Integer;
  C : Char;
  Max : Integer;
begin
  if (StartIndex + Count) >= Length(value) then
    Max := Length(value)
  else
    Max := StartIndex + Count;

  I := StartIndex;
  while I < Max do
  begin
    for C in AnyOf do
      if value[I] = C then
        Exit(I);
    Inc(I);
  end;
  Result := -1;
end;

function AntSplit(const value : string; const Separator: array of Char; const Count: Integer): TArray<string>;
const
  DeltaGrow = 32;
var
  NextSeparator, LastIndex: Integer;
  Total: Integer;
  CurrentLength: Integer;
  S: string;
begin
  Total := 0;
  LastIndex := 1;
  CurrentLength := 0;
  NextSeparator := IndexOfAny(value, Separator, LastIndex, Length(value));
  while (NextSeparator >= 0) and (Total < Count) do
  begin
    S := Copy(value, LastIndex, NextSeparator - LastIndex);
    if (S <> '') then
    begin
      Inc(Total);
      if CurrentLength < Total then
      begin
        CurrentLength := Total + DeltaGrow;
        SetLength(Result, CurrentLength);
      end;
      Result[Total - 1] := S;
    end;
    LastIndex := NextSeparator + 1;
    NextSeparator := IndexOfAny(value, Separator, LastIndex, Length(value));
  end;

  if (LastIndex < Length(value)) and (Total < Count) then
  begin
    Inc(Total);
    SetLength(Result, Total);
    Result[Total - 1] := Copy(value, LastIndex, Length(value));
  end
  else
    SetLength(Result, Total);
end;


class function TPathUtils.CompressRelativePath(basePath, path: string): string;
var
  stack : IStack<string>;
  segments : TArray<string>;
  segment : string;
  baseSegments : TArray<string>;
  baseSegLength : integer;
  isUnc : boolean;
begin
  if path = '' then
    exit(path);

  isUnc := false;
  if basePath <> '' then
  begin
    if TPath.IsUNCPath(basePath) then
    begin
      isUnc := true;
      Delete(basePath,1,2);
      if path.StartsWith(PathDelim) then
        path := basePath + path
      else
        path := IncludeTrailingPathDelimiter(basePath) + path;
    end
    else
    begin
      if not TPathUtils.IsPathRooted(path) then  //TPath.IsPathRooted treats \ as rooted.. which is incorrect
        path := IncludeTrailingPathDelimiter(basePath) + path
       else if not path.StartsWith(basePath) then
         exit(path);
    end;

    baseSegments := AntSplit(basePath, [PathDelim], MaxInt);
    baseSegLength := Length(baseSegments);
  end
  else
    baseSegLength := 1;

  stack := TCollections.CreateStack<string>;
  if TPath.IsUNCPath(path) then
  begin
    Delete(path,1,2);
    isUnc := true;
  end;

  segments := AntSplit(path, [PathDelim], MaxInt);


  for segment in segments do
  begin
    if segment = '..' then
    begin
      if stack.Count > 1 then
        stack.Pop; //up one and don't add
    end
    else if segment <> '.' then
      stack.Push(segment);
  end;
  result := '';

  if stack.Count < baseSegLength then
    exit(path);


  while stack.Count > 0 do
  begin
    if result <> '' then
      result := stack.Pop + PathDelim + result
    else
      result := stack.Pop;
  end;
  if path.EndsWith(PathDelim) then
    result := IncludeTrailingPathDelimiter(result);
  if result.EndsWith(':') then
    result := result + PathDelim;

  if isUnc then
    result := '\\' +  result;

  if (basePath <> '') then
  begin
    if isUnc then
      basePath := '\\' + basePath;
    if not result.StartsWith(basePath) then
      exit(path);
  end;
end;

class function TPathUtils.IsRelativePath(const value : string) : boolean;
begin
  result := (not TPath.IsUNCPath(value) and value.StartsWith('.\')) or System.SysUtils.IsRelativePath(value);
end;

class function TPathUtils.ContainsWildcard(const value: string): boolean;
begin
  result := IndexOfAny(value, ['?', '*' ], 1, Length(value)) <> -1;
end;

class function TPathUtils.CreateRelativePath(const base, fileName: string): string;
var
  buffer : array[0..MAX_PATH] of Char;
begin
  SetLength(result, MAX_PATH);
  if not PathRelativePathTo(buffer, PWideChar(base), 0, PWideChar(fileName), 0) then
    result := fileName
  else
    result := string(buffer);

end;

class function TPathUtils.IsPathRooted(const value: string): boolean;
begin
  result := TRegEx.IsMatch(value, '^[a-zA-z]\:\\|\\\\');
end;

class function TPathUtils.StripBase(const base : string; const fileName : string) : string;
begin
  if fileName.StartsWith(base) then
    result := Copy(fileName, Length(base) + 1, Length(fileName))
  else
    //it's below or outside the base path,
    //there's no way to know what else to do
    //but just use the filename, so it will
    //end up in the root folder??
    result := ExtractFileName(fileName);
end;


end.
