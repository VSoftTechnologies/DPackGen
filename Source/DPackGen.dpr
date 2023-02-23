program DPackGen;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  VSoft.CommandLine.Parser,
  DPackGen.Main in 'DPackGen.Main.pas',
  DPackGen.OptionsDef in 'DPackGen.OptionsDef.pas',
  DPackGen.Definition in 'DPackGen.Definition.pas',
  DPackGen.Generator in 'DPackGen.Generator.pas',
  DPackGen.Interfaces in 'DPackGen.Interfaces.pas',
  DPackGen.Template in 'DPackGen.Template.pas',
  DPackGen.Template.Base in 'DPackGen.Template.Base.pas',
  DPackGen.TargetPlatform in 'DPackGen.TargetPlatform.pas',
  DPackGen.Utils in 'DPackGen.Utils.pas',
  DPackGen.Types in 'DPackGen.Types.pas',
  DPackGen.Banner in 'DPackGen.Banner.pas',
  DPackGen.MSXML in 'DPackGen.MSXML.pas',
  DPackGen.Utils.Path in 'Utils\DPackGen.Utils.Path.pas',
  DPackGen.PackageReference in 'DPackGen.PackageReference.pas';

begin
  try
    ExitCode := TDpackGenApplication.Run;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ExitCode := 911;
    end;
  end;
end.
