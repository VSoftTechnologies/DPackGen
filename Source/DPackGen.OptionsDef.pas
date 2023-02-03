unit DPackGen.OptionsDef;

interface

type
  TGlobalOptions = record
  class var
    Verbose : boolean;
    HelpCommand : string;
  end;

  TGenerateOptions = record
  class var
    DefinitionFile : string;
    DpkOnly        : boolean;
  end;


implementation

uses
  VSoft.CommandLine.Options;

procedure ConfigureOptions;
var
  cmd    : TCommandDefinition;
  option : IOptionDefinition;
begin
  option := TOptionsRegistry.RegisterOption<boolean>('verbose','v','verbose output',
    procedure(const value : boolean)
    begin
        TGlobalOptions.Verbose := value;
    end);
  option.HasValue := false;

  cmd := TOptionsRegistry.RegisterCommand('help','h','get some help','','help [command]');
  option := cmd.RegisterUnNamedOption<string>('The command you need help for, a long description so that it probably wraps on the console', 'command',
                  procedure(const value : string)
                  begin
                      TGlobalOptions.HelpCommand := value;
                  end);

  cmd := TOptionsRegistry.RegisterCommand('generate','','generate packages', '', 'generate [options]');
  option := cmd.RegisterUnNamedOption<string>('defintionFile','The path to the package definition file',
                  procedure(const value : string)
                  begin
                      TGenerateOptions.DefinitionFile := value;
                  end);
  option.Required := true;
  option := cmd.RegisterOption<boolean>('dpkonly','dpk','generate dpk''s only',
                  procedure(const value : boolean)
                  begin
                      TGenerateOptions.DpkOnly := value;
                  end);



end;

initialization
  ConfigureOptions;



end.
