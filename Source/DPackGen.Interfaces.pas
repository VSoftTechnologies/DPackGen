unit DPackGen.Interfaces;

interface

uses
  System.Classes,
  JsonDataObjects,
  Spring.Collections,
  DPackGen.Types;

type
  IDefNode = interface
  ['{B2A7747F-78AB-4461-B0CF-379B2047E4B6}']
    function LoadFromJson(const jsonObject : TJsonObject) : boolean;
  end;

  IDPMPackageReference = interface;

  IDPMPackageReference = interface(IDefNode)
  ['{9487BF3A-56F6-43B9-A40A-6C01B1697D4A}']
    function GetId : string;
    procedure SetId(const value : string);
    function GetVersion : string;
    procedure SetVersion(const value : string);
    function GetPlatform : string;
    procedure SetPlatform(const value : string);
    function GetDependencies : IList<IDPMPackageReference>;
    procedure SetDependencies(const value : IList<IDPMPackageReference>);

    property Id : string read GetId write SetId;
    property Version : string read GetVersion write SetVersion;
    property Platform : string read GetPlatform write SetPlatform;
    property Dependencies : IList<IDPMPackageReference> read GetDependencies write SetDependencies;
  end;

  ITemplateBase = interface(IDefNode)
  ['{89B4E8DD-C39E-4B31-9CAC-6145E74F69EE}']

    function GetCode : TStrings;
    procedure SetCode(const value : TStrings);

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


    function GetNameSpacePrefixes : TStrings;
    procedure SetNameSpacePrefixes(const value : TStrings);

    function GetLibSuffix : string;
    procedure SetLibSuffix(const value : string);

    function GetFolderNameTemplate : string;
    procedure SetFolderNameTemplate(const value : string);

    function GetMainSourceTemplate : string;
    procedure SetMainSourceTemplate(const value : string);

    function GetDescriptionTemplate : string;
    procedure SetDescriptionTemplate(const value : string);

    function GetDPMPackages : IList<IDPMPackageReference>;
    procedure SetDPMPackages(const value : IList<IDPMPackageReference>);

    property Code : TStrings read GetCode write SetCode;
    property DPKOptions : TStrings read GetDPKOptions write SetDPKOptions;
    property FolderNameTemplate : string read GetFolderNameTemplate write SetFolderNameTemplate;
    property DescriptionTemplate : string read GetDescriptionTemplate write SetDescriptionTemplate;
    property MainSourceTemplate : string read GetMainSourceTemplate write SetMainSourceTemplate;
    property PreFiles : TStrings read GetPreFiles write SetPreFiles;
    property Files : TStrings read GetFiles write SetFiles;
    property PostFiles : TStrings read GetPostFiles write SetPostFiles;
    property LibSuffix : string read GetLibSuffix write SetLibSuffix;
    property Requires : TStrings read GetRequires write SetRequires;
    property NameSpacePrefixes : TStrings read GetNameSpacePrefixes write SetNameSpacePrefixes;

    property DPMPackages : IList<IDPMPackageReference> read GetDPMPackages write SetDPMPackages;
  end;

  ITemplate = interface(ITemplateBase)
  ['{22FAB79F-359D-4A27-8B87-AC9998157046}']
    function GetName : string;

    property Name : string read GetName;
  end;

  ITargetPlatform = interface(ITemplateBase)
  ['{CFC4E58F-E0D2-469F-9EE4-202EB0303FBA}']
    function GetCompilerVersion : TCompilerVersion;
    function GetPlatforms : TArray<TPlatform>;
    function GetTemplateName : string;
    procedure Assign(const source : ITemplateBase);

    property CompilerVersion: TCompilerVersion read GetCompilerVersion;
    property Platforms: TArray<TPlatform> read GetPlatforms;
    property TemplateName : string read GetTemplateName;
  end;


  IProjectDefinition = interface(IDefNode)
  ['{53C03E8C-9646-4B99-949B-A02A752A932F}']
    function GetName : string;
    function GetProjectGUID : string;
    function GetProjectType : TProjectType;
    function GetFrameworkType : TFrameworkType;
    function GetPackageType : TPackageType;
    function Generate : boolean;

    property Name : string read GetName;
    property ProjectGUID : string read GetProjectGUID;
    property ProjectType : TProjectType read GetProjectType;
    property FrameworkType : TFrameworkType read GetFrameworkType;
    property PackageType : TPackageType read GetPackageType;
  end;



implementation

end.
