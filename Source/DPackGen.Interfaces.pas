unit DPackGen.Interfaces;

interface

uses
  System.Classes,
  JsonDataObjects,
  DPackGen.Types;

type
  IDefNode = interface
  ['{B2A7747F-78AB-4461-B0CF-379B2047E4B6}']
    function LoadFromJson(const jsonObject : TJsonObject) : boolean;
  end;


//  IDefPackageNode = interface(IDefNode)
//  ['{C0A5D98D-4BF4-4038-A41E-87E26EB435AC}']
//    function GetName : string;
//    procedure SetName(const value : string);
//
//    property Name : string read GetName write SetName;
//  end;

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

    function GetDescriptionTemplate : string;
    procedure SetDescriptionTemplate(const value : string);

    property DPKOptions : TStrings read GetDPKOptions write SetDPKOptions;
    property FolderNameTemplate : string read GetFolderNameTemplate write SetFolderNameTemplate;
    property DescriptionTemplate : string read GetDescriptionTemplate write SetDescriptionTemplate;
    property PreFiles : TStrings read GetPreFiles write SetPreFiles;
    property Files : TStrings read GetFiles write SetFiles;
    property PostFiles : TStrings read GetPostFiles write SetPostFiles;
    property LibSuffix : string read GetLibSuffix write SetLibSuffix;
    property Requires : TStrings read GetRequires write SetRequires;
    property NameSpacePrefixes : TStrings read GetNameSpacePrefixes write SetNameSpacePrefixes;
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
