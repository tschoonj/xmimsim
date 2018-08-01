;Copyright (C) 2010-2017 Tom Schoonjans and Laszlo Vincze

;This program is free software: you can redistribute it and/or modify
;it under the terms of the GNU General Public License as published by
;the Free Software Foundation, either version 3 of the License, or
;(at your option) any later version.

;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.

;You should have received a copy of the GNU General Public License
;along with this program.  If not, see <http://www.gnu.org/licenses/>.

#define MyAppId "XMI-MSIM"
#define srcdir abs_top_srcdir_win
#define builddir abs_top_builddir_win

#define MyAppName "XMI-MSIM 64-bit"
#define MY_MINGW "C:\msys64\mingw64\"
#define MY_HOME "C:\msys64\home\"+GetEnv("USER")+"\"

#define MyAppPublisher "Tom Schoonjans"
#define MyAppURL "http://github.com/tschoonj/xmimsim"
#define XRAYLIB_VERSION '3.3.0'
#define XRAYLIB_VERSION_MIN '3.3.0'

#define MyInstCreationDateTime GetDateTimeString ('yyyymmdd-hhnnss', '', '');

#define USER_AGENT 'InnoSetup XMI-MSIM installer'

[Setup]
AppName={#MyAppName}
AppId={#MyAppId}
AppVersion={#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={pf}\{#MyAppName}
DefaultGroupName={#MyAppName}
DisableProgramGroupPage=yes
LicenseFile={#srcdir}\License.rtf
OutputDir={#builddir}\windows
#if Len(GetEnv("DEPLOY")) == 0
OutputBaseFilename={#MyAppId}-{#MyAppVersion}-win64
#else
OutputBaseFilename={#MyAppId}-{#MyAppVersion}-{#MyInstCreationDateTime}-win64
#endif

ArchitecturesInstallIn64BitMode=x64
ArchitecturesAllowed=x64
Compression=lzma
ChangesEnvironment=yes
SetupLogging=yes
ChangesAssociations=yes
SetupIconFile="{#srcdir}\icons\Logo_xmi_msim_Win7.ico"
MinVersion=6.0
VersionInfoVersion={#MyAppVersion}
DisableWelcomePage=no

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Types]
Name: "minimal" ; Description: "Minimal installation"
Name: "full" ; Description: "Full installation"
Name: "custom" ; Description: "Custom installation" ; Flags: iscustom

[Components]
Name: "core" ; Description: "Core components" ; Flags: fixed ; Types: full minimal custom 
Name: "sdk" ; Description: "SDK: development headers, modules and libraries" ; Types: full
Name: "examples" ; Description: "Examples" ; Types: full
Name: "source" ; Description: "Source code" ; Types: full

[Files]
Source: "{#builddir}\windows\{#GTK_INSTALLER_EXE}" ; Flags: deleteafterinstall ; DestDir: "{tmp}" ; Components: core 
Source: "{#MY_MINGW}\bin\libgfortran-5.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_MINGW}\bin\libquadmath-0.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_MINGW}\bin\libgomp-1.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_HOME}\install\bin\libhdf5-8.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_HOME}\install\bin\libxrlf03-7.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_HOME}\install\bin\libcsirocsa.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_HOME}\install\bin\libeasyRNG-0.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_HOME}\install\bin\libgtkmm-plplot-2.0-2.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_HOME}\install\bin\libplplot.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_HOME}\install\bin\libplplotcxx.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_HOME}\install\bin\libqsastime.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_HOME}\install\share\plplot5.13.0\*.*" ; DestDir: "{app}\Share\plplot" ; Components: core
Source: "{#builddir}\src\.libs\libxmimsim-0.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#builddir}\bin\.libs\libxmimsim-gui-0.dll" ; DestDir: "{app}\Lib" ; Components: core

Source: "{#builddir}\bin\.libs\xmimsim.exe" ; DestDir: "{app}\Bin" ; Components: core
Source: "{#builddir}\bin\.libs\xmimsim-cli.exe" ; DestDir: "{app}\Bin" ; Components: core
Source: "{#builddir}\bin\.libs\xmimsim-db.exe" ; DestDir: "{app}\Bin" ; Components: core
Source: "{#builddir}\bin\.libs\xmimsim-conv.exe" ; DestDir: "{app}\Bin" ; Components: core
Source: "{#builddir}\bin\.libs\xmimsim-gui.exe" ; DestDir: "{app}\Bin" ; Components: core
Source: "{#builddir}\bin\.libs\xmimsim-pymca.exe" ; DestDir: "{app}\Bin" ; Components: core
Source: "{#builddir}\bin\.libs\xmso2xmsi.exe" ; DestDir: "{app}\Bin" ; Components: core
Source: "{#builddir}\bin\.libs\xmso2svg.exe" ; DestDir: "{app}\Bin" ; Components: core
Source: "{#builddir}\bin\.libs\xmso2spe.exe" ; DestDir: "{app}\Bin" ; Components: core
Source: "{#builddir}\bin\.libs\xmso2csv.exe" ; DestDir: "{app}\Bin" ; Components: core
Source: "{#builddir}\bin\.libs\xmso2htm.exe" ; DestDir: "{app}\Bin" ; Components: core
Source: "{#builddir}\bin\.libs\xmsa2xmso.exe" ; DestDir: "{app}\Bin" ; Components: core

Source: "{#srcdir}\xml\xmimsim-1.0.dtd" ; DestDir: "{app}\Share" ; Components: core

Source: "{#builddir}\src\.libs\xmimsim-cl.dll" ; DestDir: "{app}\Lib\OpenCL" ; Components: core
Source: "{#builddir}\bin\.libs\xmimsim-gui-source-radionuclide.dll" ; DestDir: "{app}\Lib\Sources" ; Components: core
Source: "{#builddir}\bin\.libs\xmimsim-gui-source-tube-ebel.dll" ; DestDir: "{app}\Lib\Sources" ; Components: core

Source: "{#builddir}\xmimsim-{#MyAppVersion}.tar.gz" ; DestDir: "{app}\Sources" ; Components: source

Source: "{#srcdir}\examples\srm1155.xmsi" ; DestDir: "{app}\Examples" ; Components: examples
Source: "{#srcdir}\examples\srm1132.xmsi" ; DestDir: "{app}\Examples" ; Components: examples
Source: "{#srcdir}\examples\srm1412.xmsi" ; DestDir: "{app}\Examples" ; Components: examples
Source: "{#srcdir}\examples\In.xmsi" ; DestDir: "{app}\Examples" ; Components: examples
Source: "{#srcdir}\examples\srm1155.xmso" ; DestDir: "{app}\Examples" ; Components: examples
Source: "{#srcdir}\examples\srm1132.xmso" ; DestDir: "{app}\Examples" ; Components: examples
Source: "{#srcdir}\examples\srm1412.xmso" ; DestDir: "{app}\Examples" ; Components: examples
Source: "{#srcdir}\examples\In.xmso" ; DestDir: "{app}\Examples" ; Components: examples

Source: "{#builddir}\windows\xmi*.h" ; DestDir: "{app}\SDK\Include" ; Components: sdk
Source: "{#builddir}\src\xmimsim*mod" ; DestDir: "{app}\SDK\Include" ; Components: sdk
Source: "{#builddir}\src\.libs\libxmimsim.dll.a" ; DestDir: "{app}\SDK\Lib" ; Components: sdk
Source: "{#builddir}\bin\.libs\libxmimsim-gui.dll.a" ; DestDir: "{app}\SDK\Lib" ; Components: sdk
Source: "{#builddir}\windows\libxmimsim-0.lib" ; DestDir: "{app}\SDK\Lib" ; Components: sdk
Source: "{#builddir}\windows\libxmimsim-gui-0.lib" ; DestDir: "{app}\SDK\Lib" ; Components: sdk
Source: "{tmp}\xraylib.exe" ; DestDir: "{tmp}" ; Components: core ; Flags: external ; Check: InstallXraylibCheck and DwinsHs_Check(ExpandConstant('{tmp}\xraylib.exe'), \
    'http://lvserver.ugent.be/xraylib/xraylib-{#XRAYLIB_VERSION}-win64.exe', '{#USER_AGENT}', 'get', 0, 0)

#if Len(GetEnv("DO_NOT_USE_DATA")) == 0
Source: "{#builddir}\bin\xmimsimdata.h5" ; DestDir: "{app}\Share" ; Components: core
#elif Len(GetEnv("DEPLOY")) > 0
Source: "{tmp}\xmimsimdata.7z" ; DestDir: "{tmp}" ; Components: core ; Flags: external ; Check: DwinsHs_Check(ExpandConstant('{tmp}\xmimsimdata.7z'), \
    'https://xmi-msim.tomschoonjans.eu/nightly/xmimsimdata.7z', '{#USER_AGENT}', 'get', 0, 0)
Source: "{#srcdir}\windows\7za.exe"; DestDir: "{tmp}" ; Components: core
#endif

[Icons]
Name: "{group}\{cm:LaunchProgram,{#MyAppName}}"; Filename: "{app}\Bin\xmimsim-gui.exe"
Name: "{userdesktop}\{#MyAppName}"; Filename: "{app}\Bin\xmimsim-gui.exe"; Components: core; Tasks: desktopicon 
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"

[Tasks]
Name: desktopicon; Description: "Create a desktop icon"; GroupDescription: "Additional icons:"; Components: core

[Run]
Filename: "{tmp}\{#GTK_INSTALLER_EXE}" ; Parameters: "/sideeffects=no /dllpath=root /translations=no /S /D={app}\GTK" ; StatusMsg: "Installing GTK runtime libraries..." ; Components: core
Filename: "{tmp}\xraylib.exe" ; Parameters: "/VERYSILENT /SP- /SUPPRESSMSGBOXES" ; Flags: skipifdoesntexist ; StatusMsg: "Installing xraylib..."
;Filename: "{app}\Bin\xmimsim-gui.exe"; Description: "Launch XMI-MSIM"; Flags: postinstall nowait skipifsilent 
#if Len(GetEnv("DEPLOY")) > 0
Filename: {tmp}\7za.exe; Parameters: "x ""{tmp}\xmimsimdata.7z"" -o""{app}\Share\"" * -aoa"; Flags: runhidden runascurrentuser ; StatusMsg: "Decompressing data"

#endif

[UninstallRun]
Filename: "{app}\GTK\gtk3_runtime_uninst.exe" ; Parameters: "/remove_config=yes /sideeffects=no /dllpath=root /translations=no /compatdlls=no /S" 

[UninstallDelete]
Type: filesandordirs ; Name: "{app}\GTK"
Type: files ; Name: "{app}\Bin\set_xmi_msim_path.bat"
#if Len(GetEnv("DEPLOY")) > 0
Type: files ; Name: "{app}\Share\xmimsimdata.h5"
#endif
Type: dirifempty ; Name: "{app}"

[Registry]
Root: HKLM; Subkey: "Software\XMI-MSIM" ; ValueType: string ; ValueName: "" ; ValueData: "{app}" ; Flags: uninsdeletekey 
Root: HKLM; Subkey: "Software\XMI-MSIM" ; ValueType: string ; ValueName: "InstallationDirectory" ; ValueData: "{app}"
Root: HKLM; Subkey: "Software\XMI-MSIM" ; ValueType: string ; ValueName: "Vendor" ; ValueData: "Tom Schoonjans"
Root: HKLM; Subkey: "Software\XMI-MSIM\data" ; ValueType: string ; ValueName: "" ; ValueData: "{app}\Share\xmimsimdata.h5"
Root: HKLM; Subkey: "Software\XMI-MSIM\share" ; ValueType: string ; ValueName: "" ; ValueData: "{app}\Share\"
Root: HKLM; Subkey: "Software\XMI-MSIM\opencllib" ; ValueType: string ; ValueName: "" ; ValueData: "{app}\Lib\OpenCL"
Root: HKLM; Subkey: "Software\XMI-MSIM\sources" ; ValueType: string ; ValueName: "" ; ValueData: "{app}\Lib\Sources"

Root: HKLM; Subkey: "Software\Microsoft\Windows\CurrentVersion\App Paths\xmimsim-gui.exe" ; ValueType: string ; ValueName: "" ; ValueData: "{app}\Bin\xmimsim-gui.exe" ; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\Microsoft\Windows\CurrentVersion\App Paths\xmimsim-gui.exe" ; ValueType: string ; ValueName: "Path" ; ValueData: "{app}\Bin;{app}\Lib;{app}\GTK"

Root: HKCR; Subkey: ".xmsi" ; ValueType: string ; ValueName: "" ; ValueData: "{#MyAppName} input-file" ; Flags: uninsdeletekey 
Root: HKCR; Subkey: "{#MyAppName} input-file" ; ValueType: string ; ValueName: "" ; ValueData: "{#MyAppName} input-file" ; Flags: uninsdeletekey 
Root: HKCR; Subkey: "{#MyAppName} input-file\shell" ; ValueType: string ; ValueName: "" ; ValueData: "open"
Root: HKCR; Subkey: "{#MyAppName} input-file\DefaultIcon" ; ValueType: string ; ValueName: "" ; ValueData: "{app}\Bin\xmimsim-gui.exe,1"
Root: HKCR; Subkey: "{#MyAppName} input-file\shell\open\command" ; ValueType: string ; ValueName: "" ; ValueData: """{app}\Bin\xmimsim-gui.exe"" ""%1"""
Root: HKCR; Subkey: "{#MyAppName} input-file\shell\edit" ; ValueType: string ; ValueName: "" ; ValueData: "Edit {#MyAppName} input-file"
Root: HKCR; Subkey: "{#MyAppName} input-file\shell\edit\command" ; ValueType: string ; ValueName: "" ; ValueData: """{app}\Bin\xmimsim-gui.exe"" ""%1"""
Root: HKCR; Subkey: ".xmso" ; ValueType: string ; ValueName: "" ; ValueData: "{#MyAppName} output-file" ; Flags: uninsdeletekey 
Root: HKCR; Subkey: "{#MyAppName} output-file" ; ValueType: string ; ValueName: "" ; ValueData: "{#MyAppName} output-file" ; Flags: uninsdeletekey 
Root: HKCR; Subkey: "{#MyAppName} output-file\shell" ; ValueType: string ; ValueName: "" ; ValueData: "open"
Root: HKCR; Subkey: "{#MyAppName} output-file\DefaultIcon" ; ValueType: string ; ValueName: "" ; ValueData: "{app}\Bin\xmimsim-gui.exe,2"
Root: HKCR; Subkey: "{#MyAppName} output-file\shell\open\command" ; ValueType: string ; ValueName: "" ; ValueData: """{app}\Bin\xmimsim-gui.exe"" ""%1"""
Root: HKCR; Subkey: "{#MyAppName} output-file\shell\edit" ; ValueType: string ; ValueName: "" ; ValueData: "Edit {#MyAppName} output-file"
Root: HKCR; Subkey: "{#MyAppName} output-file\shell\edit\command" ; ValueType: string ; ValueName: "" ; ValueData: """{app}\Bin\xmimsim-gui.exe"" ""%1"""
Root: HKCR; Subkey: ".xmsa" ; ValueType: string ; ValueName: "" ; ValueData: "{#MyAppName} archive" ; Flags: uninsdeletekey 
Root: HKCR; Subkey: "{#MyAppName} archive" ; ValueType: string ; ValueName: "" ; ValueData: "{#MyAppName} archive" ; Flags: uninsdeletekey 
Root: HKCR; Subkey: "{#MyAppName} archive\shell" ; ValueType: string ; ValueName: "" ; ValueData: "open"
Root: HKCR; Subkey: "{#MyAppName} archive\DefaultIcon" ; ValueType: string ; ValueName: "" ; ValueData: "{app}\Bin\xmimsim-gui.exe,3"
Root: HKCR; Subkey: "{#MyAppName} archive\shell\open\command" ; ValueType: string ; ValueName: "" ; ValueData: """{app}\Bin\xmimsim-gui.exe"" ""%1"""
Root: HKCR; Subkey: "{#MyAppName} archive\shell\edit" ; ValueType: string ; ValueName: "" ; ValueData: "Edit {#MyAppName} archive"
Root: HKCR; Subkey: "{#MyAppName} archive\shell\edit\command" ; ValueType: string ; ValueName: "" ; ValueData: """{app}\Bin\xmimsim-gui.exe"" ""%1"""




[Code]

#define DwinsHs_Use_Predefined_Downloading_WizardPage
#include "dwinshs.iss"

//taken from http://blog.lextudio.com/2007/08/inno-setup-script-sample-for-version-comparison-2/

function GetNumber(var temp: String): Integer;
var
  part: String;
  pos1: Integer;
begin
  if Length(temp) = 0 then
  begin
    Result := -1;
    Exit;
  end;
    pos1 := Pos('.', temp);
    if (pos1 = 0) then
    begin
      Result := StrToInt(temp);
    temp := '';
    end
    else
    begin
    part := Copy(temp, 1, pos1 - 1);
      temp := Copy(temp, pos1 + 1, Length(temp));
      Result := StrToInt(part);
    end;
end;
 
function CompareInner(var temp1, temp2: String): Integer;
var
  num1, num2: Integer;
begin
    num1 := GetNumber(temp1);
  num2 := GetNumber(temp2);
  if (num1 = -1) or (num2 = -1) then
  begin
    Result := 0;
    Exit;
  end;
      if (num1 > num2) then
      begin
        Result := 1;
      end
      else if (num1 < num2) then
      begin
        Result := -1;
      end
      else
      begin
        Result := CompareInner(temp1, temp2);
      end;
end;
 
function CompareVersion(str1, str2: String): Integer;
var
  temp1, temp2: String;
begin
    temp1 := str1;
    temp2 := str2;
    Result := CompareInner(temp1, temp2);
end;

function InstallXraylibCheck() : Boolean;
var subkeyName: String;
var value: String;
  
begin
  //do we need to install or update xraylib?
  Log('Checking for xraylib');
  value := '';
  subkeyName := ExpandConstant('Software\Microsoft\Windows\CurrentVersion\Uninstall\xraylib_64_is1');
  RegQueryStringValue(HKLM, subkeyName, 'DisplayVersion', value);
  Result := ((value <> '') and (CompareVersion(value, '{#XRAYLIB_VERSION_MIN}') < 0)) or (value = '')
end;

procedure InitializeWizard();
begin

  DwinsHs_InitializeWizard(wpPreparing);

end;

function GetUninstallString(const RootKey: Integer): String;
var
  sUnInstPath: String;
  sUnInstallString: String;
begin
  sUnInstallString := '';
  sUnInstPath := ExpandConstant('Software\Microsoft\Windows\CurrentVersion\Uninstall\XMI-MSIM_is1');
  if not RegQueryStringValue(RootKey, sUnInstPath, 'UninstallString', sUnInstallString) then
    begin
    sUnInstPath := ExpandConstant('Software\Microsoft\Windows\CurrentVersion\Uninstall\XMI-MSIM');
    RegQueryStringValue(RootKey, sUnInstPath, 'QuietUninstallString', sUnInstallString);
  end
  else
  begin
	//innosetups QuietUninstallString is not as silent as I would like...
	sUnInstallString := sUnInstallString + ' /VERYSILENT /SUPPRESSMSGBOXES';
  end;
  Log('QuietUninstallString: '+ sUnInstallString);
  Result := sUnInstallString;
end;


/////////////////////////////////////////////////////////////////////
function IsUpgrade(): Boolean;
begin
  Result := (GetUninstallString(HKLM) <> '');
end;


/////////////////////////////////////////////////////////////////////
function UnInstallOldVersion(const RootKey: Integer): Integer;
var
  sUnInstallString: String;
  iResultCode: Integer;
begin
// Return Values:
// 1 - uninstall string is empty
// 2 - error executing the UnInstallString
// 3 - successfully executed the UnInstallString

  // default return value
  Result := 0;

  // get the uninstall string of the old app
  sUnInstallString := GetUninstallString(RootKey);
  if sUnInstallString <> '' then begin
    //sUnInstallString := RemoveQuotes(sUnInstallString);
    if Exec('>',sUnInstallString,'', SW_HIDE, ewWaitUntilTerminated, iResultCode) then
      Result := 3
    else
      Result := 2;
  end else
    Result := 1;
end;

function InitializeSetup(): Boolean;

begin
  Result := True;

  //check if the 32-bit version is installed
      if (GetUninstallString(HKLM32) <> '') then
      begin
      if (WizardSilent()) then
      begin
        UnInstallOldVersion(HKLM32);
      end
      else
      begin
	//display msgbox
	if (MsgBox('A previously installed 32-bit version of XMI-MSIM was found on the system. It has to be uninstalled before the installation can proceed.', mbConfirmation, MB_OKCANCEL)) = IDOK then
	begin
        	UnInstallOldVersion(HKLM32);
	end
	else
	begin
  		Result := False;
		Exit;
	end;
      end;
      end;

  if (IsUpgrade()) then
  begin
    //launch dialog when not operating in silent mode
      if (WizardSilent()) then
      begin
        UnInstallOldVersion(HKLM);
      end
      else
      begin
	//display msgbox
	if (MsgBox('A previously installed version of XMI-MSIM was found on the system. It has to be uninstalled before the installation can proceed.', mbConfirmation, MB_OKCANCEL)) = IDOK then
	begin
        	UnInstallOldVersion(HKLM);
	end
	else
	begin
  		Result := False;
	end;
      end;
  end;
end;

procedure CurStepChanged(CurStep: TSetupStep);

begin
    if (CurStep=ssPostInstall) then
    begin
	SaveStringToFile(ExpandConstant('{app}\Bin\set_xmi_msim_path.bat'), ExpandConstant('set PATH=%PATH%;{app}\Bin;{app}\Lib;{app}\GTK'), False);
    end;
end;

function BeforeDownload(): Boolean;
begin
  DwinsHs_AppendMirrorFile(ExpandConstant('{tmp}\xraylib.exe'), 'https://xraylib.tomschoonjans.eu/xraylib-{#XRAYLIB_VERSION}-win64.exe', '{#USER_AGENT}', rmGet);
  Result := True;
end;

procedure CurPageChanged(CurPageID: Integer);
begin
  DwinsHs_CurPageChanged(CurPageID, @BeforeDownload, nil);
end;

function ShouldSkipPage(CurPageId: Integer): Boolean;
begin
  Result := False;
  DwinsHs_ShouldSkipPage(CurPageId, Result);
end;

function BackButtonClick(CurPageID: Integer): Boolean;
begin
  Result := True;
  DwinsHs_BackButtonClick(CurPageID);
end;

function NextButtonClick(CurPageID: Integer): Boolean;
begin
  Result := True;
  DwinsHs_NextButtonClick(CurPageID, Result);
end;

procedure CancelButtonClick(CurPageID: Integer; var Cancel, Confirm: Boolean);
begin
  DwinsHs_CancelButtonClick(CurPageID, Cancel, Confirm);
end;

