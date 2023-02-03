unit DPackGen.Banner;

interface

procedure ShowBanner();

implementation

uses
  System.SysUtils,
  DPackGen.Utils;

procedure ShowBanner();
begin
  WriteLn('');
  WriteLn('DPackGen - DPK/DPROJ Generator - Version : ' + TSystemUtils.GetVersionString);
  WriteLn('© 2023 Vincent Parrett and Contributors');
  WriteLn('License - http://www.apache.org/licenses/LICENSE-2.0');
  WriteLn('');
end;
end.
