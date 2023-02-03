unit DPackGen.Main;

interface

type
  TDpackGenApplication = class
  private
    class function Generate : integer;

  public
    class function Run : integer;
  end;

implementation

uses
  DPackGen.OptionsDef,
  DPackGen.Interfaces,
  DPackGen.Definition,
  DPackGen.Banner,
  VSoft.CommandLine.Options;

procedure PrintUsage(const command : string);
begin
  if command = 'help' then
  begin
    if TGlobalOptions.HelpCommand = '' then
      TGlobalOptions.HelpCommand := 'help';
  end
  else
    TGlobalOptions.HelpCommand := command;

  TOptionsRegistry.PrintUsage(TGlobalOptions.HelpCommand,
    procedure (const value : string)
    begin
      WriteLn(value);
    end);
end;


{ TDpackGenApplication }

class function TDpackGenApplication.Generate: integer;
var
  packageDef : IPackageDefinition;
begin
  WriteLn('loading definition file');

  packageDef := TPackageDefinition.LoadFromFile(TGenerateOptions.DefinitionFile);
  if packageDef = nil then
  begin
    WriteLn('Invalid Package Definition file.');
    Exit(1);
  end;

  WriteLn('Generating....');

  if not packageDef.Generate then
    exit(2);

  WriteLn('done.');
  result := 0;
end;

class function TDpackGenApplication.Run: integer;
var
  parseresult :  ICommandLineParseResult;
begin
  ShowBanner;
  TOptionsRegistry.DescriptionTab := 35;
  parseresult := TOptionsRegistry.Parse;
  if parseresult.HasErrors then
  begin
    Writeln;
    Writeln(parseresult.ErrorText);
    PrintUsage(parseresult.Command);
    result := 1;
  end
  else
  begin
    if (parseresult.Command = '') or (parseresult.Command = 'help') then
    begin
      PrintUsage(parseresult.Command);
      result := 1;

    end
    else
    begin
      //only have the 1 command for now.
      result := Generate;
    end;
  end;
end;

end.
