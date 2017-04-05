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

#include ReadReg(HKEY_LOCAL_MACHINE,'Software\Sherlock Software\InnoTools\Downloader','ScriptPath','')

#define MyAppId "XMI-MSIM"
#define srcdir abs_top_srcdir_win
#define builddir abs_top_builddir_win

#ifdef XMI_MSIM64
  #define MyAppName "XMI-MSIM 64-bit"
  #define GTK_INSTALLER_EXE "gtk3-runtime-3.22.10-2017-04-05-ts-win64.exe"
  #define MY_MINGW "C:\msys64\mingw64\"
  #define MY_HOME "C:\msys64\home\"+GetEnv("USER")+"\"
#else
  #define MyAppName "XMI-MSIM 32-bit"
  #define GTK_INSTALLER_EXE "gtk2-runtime-2.24.8-2011-12-03-ash.exe"
  #define MY_MINGW "C:\MinGW32\"
  #define MY_HOME "C:\msys\1.0\home\schoon\"
#endif

#define MyAppPublisher "Tom Schoonjans"
#define MyAppURL "http://github.com/tschoonj/xmimsim"
#define XRAYLIB_VERSION '3.2.0'
#define XRAYLIB_VERSION_MIN '3.2.0'


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
#ifdef XMI_MSIM64
OutputBaseFilename={#MyAppId}-{#MyAppVersion}-win64
ArchitecturesInstallIn64BitMode=x64
ArchitecturesAllowed=x64
#else
OutputBaseFilename={#MyAppId}-{#MyAppVersion}-win32
#endif
Compression=lzma
ChangesEnvironment=yes
SetupLogging=yes
ChangesAssociations=yes
SetupIconFile="{#MY_HOME}github\xmimsim\icons\Logo_xmi_msim_Win7.ico"
#ifdef XMI_MSIM64
MinVersion=6.0
#else
MinVersion=5.1
#endif
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
Source: "{#srcdir}\windows\{#GTK_INSTALLER_EXE}" ; Flags: deleteafterinstall ; DestDir: "{tmp}" ; Components: core 
#ifdef XMI_MSIM64
Source: "{#MY_MINGW}\bin\libgfortran-3.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_MINGW}\bin\libquadmath-0.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_MINGW}\bin\libgomp-1.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_MINGW}\bin\libcurl-4.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_MINGW}\bin\libeay32.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_MINGW}\bin\ssleay32.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_MINGW}\bin\libidn-11.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_MINGW}\bin\libnghttp2-14.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_MINGW}\bin\librtmp-1.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_MINGW}\bin\libssh2-1.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_MINGW}\bin\libgmp-10.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_MINGW}\bin\libgnutls-30.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_MINGW}\bin\libhogweed-4.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_MINGW}\bin\libnettle-6.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_MINGW}\bin\libp11-kit-0.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_MINGW}\bin\libtasn1-6.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_MINGW}\bin\libunistring-2.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_HOME}\install\bin\libhdf5-8.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_HOME}\install\bin\libxrlf03-7.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_HOME}\install\bin\libcsirocsa.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_HOME}\install\bin\libeasyRNG-0.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_HOME}\install\bin\libgtkmm-plplot-2.0-1.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_HOME}\install\bin\libplplot.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_HOME}\install\bin\libplplotcxx.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_HOME}\install\bin\libqsastime.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_HOME}\install\share\plplot5.11.1\*.*" ; DestDir: "{app}\Share\plplot" ; Components: core
Source: "{#MY_HOME}\install\bin\libxmimsim-0.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_HOME}\install\bin\libxmimsim-gui-0.dll" ; DestDir: "{app}\Lib" ; Components: core
#else
Source: "{#MY_MINGW}\bin\libgfortran-3.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_MINGW}\bin\libquadmath-0.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_MINGW}\bin\libgcc_s_sjlj-1.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_MINGW}\bin\libgomp-1.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_HOME}\bin\libxml2-2.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_HOME}\bin\libeay32.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_HOME}\bin\ssleay32.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_HOME}\bin\libjson-glib-1.0-0.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_HOME}\bin\libxslt-1.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_HOME}\bin\libgtkextra-win32-3.0-8.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_HOME}\bin\libcurl-4.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_HOME}\bin\libgsl-0.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_HOME}\bin\libgslcblas-0.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_HOME}\bin\libfgsl-0.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_HOME}\bin\libhdf5-8.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_HOME}\bin\libxrlf03-7.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_HOME}\bin\libxmimsim-0.dll" ; DestDir: "{app}\Lib" ; Components: core
Source: "{#MY_HOME}\bin\libxmimsim-gui-0.dll" ; DestDir: "{app}\Lib" ; Components: core
#endif

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

#if Len(GetEnv("DO_NOT_USE_DATA")) == 0
Source: "{#MY_HOME}\github\xmimsim\build\bin\xmimsimdata.h5" ; DestDir: "{app}\Share" ; Components: core
#endif

Source: "{#MY_HOME}\github\xmimsim\xml\xmimsim-1.0.dtd" ; DestDir: "{app}\Share" ; Components: core
Source: "{#MY_HOME}\github\xmimsim\xml\xmso2xmsi.xml" ; DestDir: "{app}\Share" ; Components: core
Source: "{#MY_HOME}\github\xmimsim\xml\xmso2spe.xml" ; DestDir: "{app}\Share" ; Components: core
Source: "{#MY_HOME}\github\xmimsim\xml\xmso2csv.xml" ; DestDir: "{app}\Share" ; Components: core
Source: "{#MY_HOME}\github\xmimsim\xml\xmso2svg.xml" ; DestDir: "{app}\Share" ; Components: core
Source: "{#MY_HOME}\github\xmimsim\xml\xmso2htm.xml" ; DestDir: "{app}\Share" ; Components: core
Source: "{#MY_HOME}\github\xmimsim\xml\xmsa2xmso.xml" ; DestDir: "{app}\Share" ; Components: core

Source: "{#MY_HOME}\github\xmimsim\src\array.h" ; DestDir: "{app}\Share" ; Components: core
Source: "{#MY_HOME}\github\xmimsim\src\compilerfeatures.h" ; DestDir: "{app}\Share" ; Components: core
Source: "{#MY_HOME}\github\xmimsim\src\openclfeatures.h" ; DestDir: "{app}\Share" ; Components: core
Source: "{#MY_HOME}\github\xmimsim\src\sse.h" ; DestDir: "{app}\Share" ; Components: core
Source: "{#MY_HOME}\github\xmimsim\src\threefry.h" ; DestDir: "{app}\Share" ; Components: core
Source: "{#MY_HOME}\github\xmimsim\src\xmi_kernels.cl" ; DestDir: "{app}\Share" ; Components: core
Source: "{#builddir}\src\.libs\xmimsim-cl.dll" ; DestDir: "{app}\Lib\OpenCL" ; Components: core
Source: "{#builddir}\bin\.libs\xmimsim-gui-source-radionuclide.dll" ; DestDir: "{app}\Lib\Sources" ; Components: core
Source: "{#builddir}\bin\.libs\xmimsim-gui-source-tube-ebel.dll" ; DestDir: "{app}\Lib\Sources" ; Components: core

Source: "{#MY_HOME}\github\xmimsim\icons\256x256\Logo_xmi_msim.png" ; DestDir: "{app}\Share\Icons" ; Components: core
Source: "{#MY_HOME}\github\xmimsim\icons\256x256\Logo_xmi_msim_red.png" ; DestDir: "{app}\Share\Icons" ; Components: core
Source: "{#MY_HOME}\github\xmimsim\icons\256x256\Logo_xmi_msim_archive.png" ; DestDir: "{app}\Share\Icons" ; Components: core
Source: "{#MY_HOME}\github\xmimsim\icons\256x256\Radiation_warning_symbol.png" ; DestDir: "{app}\Share\Icons" ; Components: core
Source: "{#MY_HOME}\github\xmimsim\bin\coordinate_system.png" ; DestDir: "{app}\Share" ; Components: core

Source: "{#MY_HOME}\github\xmimsim\build\xmimsim-{#MyAppVersion}.tar.gz" ; DestDir: "{app}\Sources" ; Components: source

Source: "{#MY_HOME}\github\xmimsim\examples\srm1155.xmsi" ; DestDir: "{app}\Examples" ; Components: examples
Source: "{#MY_HOME}\github\xmimsim\examples\srm1132.xmsi" ; DestDir: "{app}\Examples" ; Components: examples
Source: "{#MY_HOME}\github\xmimsim\examples\srm1412.xmsi" ; DestDir: "{app}\Examples" ; Components: examples
Source: "{#MY_HOME}\github\xmimsim\examples\In.xmsi" ; DestDir: "{app}\Examples" ; Components: examples
Source: "{#MY_HOME}\github\xmimsim\examples\srm1155.xmso" ; DestDir: "{app}\Examples" ; Components: examples
Source: "{#MY_HOME}\github\xmimsim\examples\srm1132.xmso" ; DestDir: "{app}\Examples" ; Components: examples
Source: "{#MY_HOME}\github\xmimsim\examples\srm1412.xmso" ; DestDir: "{app}\Examples" ; Components: examples
Source: "{#MY_HOME}\github\xmimsim\examples\In.xmso" ; DestDir: "{app}\Examples" ; Components: examples

Source: "{#builddir}\windows\xmi*.h" ; DestDir: "{app}\SDK\Include" ; Components: sdk
Source: "{#builddir}\src\xmimsim*mod" ; DestDir: "{app}\SDK\Include" ; Components: sdk
Source: "{#builddir}\src\.libs\libxmimsim.dll.a" ; DestDir: "{app}\SDK\Lib" ; Components: sdk
Source: "{#builddir}\bin\.libs\libxmimsim-gui.dll.a" ; DestDir: "{app}\SDK\Lib" ; Components: sdk
Source: "{#builddir}\windows\libxmimsim-0.lib" ; DestDir: "{app}\SDK\Lib" ; Components: sdk
Source: "{#builddir}\windows\libxmimsim-gui-0.lib" ; DestDir: "{app}\SDK\Lib" ; Components: sdk

[Icons]
Name: "{group}\{cm:LaunchProgram,{#MyAppName}}"; Filename: "{app}\Bin\xmimsim-gui.exe"
Name: "{userdesktop}\{#MyAppName}"; Filename: "{app}\Bin\xmimsim-gui.exe"; Components: core; Tasks: desktopicon 
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"

[Tasks]
Name: desktopicon; Description: "Create a desktop icon"; GroupDescription: "Additional icons:"; Components: core

[Run]
Filename: "{tmp}\{#GTK_INSTALLER_EXE}" ; Parameters: "/sideeffects=no /dllpath=root /translations=no /S /D={app}\GTK" ; StatusMsg: "Installing GTK runtime libraries..." ; Components: core
Filename: "{tmp}\xraylib-{#XRAYLIB_VERSION}.exe" ; Parameters: "/VERYSILENT /SP- /SUPPRESSMSGBOXES" ; Flags: skipifdoesntexist ; StatusMsg: "Installing xraylib..."
;Filename: "{app}\Bin\xmimsim-gui.exe"; Description: "Launch XMI-MSIM"; Flags: postinstall nowait skipifsilent 

[UninstallRun]
#ifdef XMI_MSIM64
Filename: "{app}\GTK\gtk3_runtime_uninst.exe" ; Parameters: "/remove_config=yes /sideeffects=no /dllpath=root /translations=no /compatdlls=no /S" 
#else
Filename: "{app}\GTK\gtk2_runtime_uninst.exe" ; Parameters: "/remove_config=yes /sideeffects=no /dllpath=root /translations=no /compatdlls=no /S" 
#endif

[UninstallDelete]
Type: filesandordirs ; Name: "{app}\GTK"
Type: files ; Name: "{app}\Bin\set_xmi_msim_path.bat"
Type: dirifempty ; Name: "{app}"

[Registry]
Root: HKLM; Subkey: "Software\XMI-MSIM" ; ValueType: string ; ValueName: "" ; ValueData: "{app}" ; Flags: uninsdeletekey 
Root: HKLM; Subkey: "Software\XMI-MSIM" ; ValueType: string ; ValueName: "InstallationDirectory" ; ValueData: "{app}"
Root: HKLM; Subkey: "Software\XMI-MSIM" ; ValueType: string ; ValueName: "Vendor" ; ValueData: "Tom Schoonjans"
Root: HKLM; Subkey: "Software\XMI-MSIM\data" ; ValueType: string ; ValueName: "" ; ValueData: "{app}\Share\xmimsimdata.h5"
Root: HKLM; Subkey: "Software\XMI-MSIM\share" ; ValueType: string ; ValueName: "" ; ValueData: "{app}\Share\"
Root: HKLM; Subkey: "Software\XMI-MSIM\xmso2svg" ; ValueType: string ; ValueName: "" ; ValueData: "{app}\Share\xmso2svg.xml"
Root: HKLM; Subkey: "Software\XMI-MSIM\xmso2spe" ; ValueType: string ; ValueName: "" ; ValueData: "{app}\Share\xmso2spe.xml"
Root: HKLM; Subkey: "Software\XMI-MSIM\xmso2csv" ; ValueType: string ; ValueName: "" ; ValueData: "{app}\Share\xmso2csv.xml"
Root: HKLM; Subkey: "Software\XMI-MSIM\xmso2xmsi" ; ValueType: string ; ValueName: "" ; ValueData: "{app}\Share\xmso2xmsi.xml"
Root: HKLM; Subkey: "Software\XMI-MSIM\xmso2htm" ; ValueType: string ; ValueName: "" ; ValueData: "{app}\Share\xmso2htm.xml"
Root: HKLM; Subkey: "Software\XMI-MSIM\xmsa2xmso" ; ValueType: string ; ValueName: "" ; ValueData: "{app}\Share\xmsa2xmso.xml"
Root: HKLM; Subkey: "Software\XMI-MSIM\icon" ; ValueType: string ; ValueName: "" ; ValueData: "{app}\Share\Logo_xmi_msim.png"
Root: HKLM; Subkey: "Software\XMI-MSIM\coordinate-system" ; ValueType: string ; ValueName: "" ; ValueData: "{app}\Share\coordinate_system.png"
Root: HKLM; Subkey: "Software\XMI-MSIM\icons-dir" ; ValueType: string ; ValueName: "" ; ValueData: "{app}\Share\Icons\"
Root: HKLM; Subkey: "Software\XMI-MSIM\openclcode" ; ValueType: string ; ValueName: "" ; ValueData: "{app}\Share"
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
procedure InitializeWizard();
var subkeyName: String;
var value: String;
  
begin
  ITD_Init;

  //do we need to install or update xraylib?
  Log('Checking for xraylib');
  value := '';
#ifdef XMI_MSIM64
  subkeyName := ExpandConstant('Software\Microsoft\Windows\CurrentVersion\Uninstall\xraylib_64_is1');
  RegQueryStringValue(HKLM, subkeyName, 'DisplayVersion', value);

#else
  subkeyName := ExpandConstant('Software\Microsoft\Windows\CurrentVersion\Uninstall\xraylib_is1');
  if not RegQueryStringValue(HKLM, subkeyName, 'DisplayVersion', value) then
    begin
    subkeyName := ExpandConstant('Software\Microsoft\Windows\CurrentVersion\Uninstall\xraylib');
    RegQueryStringValue(HKLM, subkeyName, 'DisplayVersion', value);
  end;
#endif

  if (((value <> '') and (CompareVersion(value, '{#XRAYLIB_VERSION_MIN}') < 0)) or (value = '')) then
  begin
     //xraylib was not found or too old
#ifdef XMI_MSIM64
     ITD_AddFile(ExpandConstant('http://lvserver.ugent.be/xraylib/xraylib-{#XRAYLIB_VERSION}-win64.exe'), ExpandConstant('{tmp}\xraylib-{#XRAYLIB_VERSION}.exe'));
     ITD_AddMirror(ExpandConstant('https://xraylib.tomschoonjans.eu/xraylib-{#XRAYLIB_VERSION}-win64.exe'), ExpandConstant('{tmp}\xraylib-{#XRAYLIB_VERSION}.exe'));
     ITD_AddMirror(ExpandConstant('http://xraylib.s3.amazonaws.com/xraylib-{#XRAYLIB_VERSION}-win64.exe'), ExpandConstant('{tmp}\xraylib-{#XRAYLIB_VERSION}.exe'));
     ITD_AddMirror(ExpandConstant('http://10.0.2.2/~schoon/xraylib-{#XRAYLIB_VERSION}-win64.exe'), ExpandConstant('{tmp}\xraylib-{#XRAYLIB_VERSION}.exe'));
#else
     ITD_AddFile(ExpandConstant('http://lvserver.ugent.be/xraylib/xraylib-{#XRAYLIB_VERSION}-win32.exe'), ExpandConstant('{tmp}\xraylib-{#XRAYLIB_VERSION}.exe'));
     ITD_AddMirror(ExpandConstant('https://xraylib.tomschoonjans.eu/xraylib-{#XRAYLIB_VERSION}-win32.exe'), ExpandConstant('{tmp}\xraylib-{#XRAYLIB_VERSION}.exe'));
     ITD_AddMirror(ExpandConstant('http://xraylib.s3.amazonaws.com/xraylib-{#XRAYLIB_VERSION}-win32.exe'), ExpandConstant('{tmp}\xraylib-{#XRAYLIB_VERSION}.exe'));
     ITD_AddMirror(ExpandConstant('http://10.0.2.2/~schoon/xraylib-{#XRAYLIB_VERSION}-win32.exe'), ExpandConstant('{tmp}\xraylib-{#XRAYLIB_VERSION}.exe'));
#endif
     ITD_DownloadAfter(wpReady);
  end;

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

#ifdef XMI_MSIM64
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
#else
  //check if the 64-bit version is installed
      if (IsWin64 and (GetUninstallString(HKLM64) <> '')) then
      begin
      if (WizardSilent()) then
      begin
        UnInstallOldVersion(HKLM64);
      end
      else
      begin
	//display msgbox
	if (MsgBox('A previously installed 64-bit version of XMI-MSIM was found on the system. It has to be uninstalled before the installation can proceed.', mbConfirmation, MB_OKCANCEL)) = IDOK then
	begin
        	UnInstallOldVersion(HKLM64);
	end
	else
	begin
  		Result := False;
		Exit;
	end;
      end;
      end;
#endif
  

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
//  if (CurStep=ssInstall) then
//  begin
//	RegWriteStringValue(HKLM, 'Software\Microsoft\Windows\CurrentVersion\App Paths\xmimsim-gui.exe', '', ExpandConstant('{app}\Bin\xmimsim-gui.exe'));	
//	RegWriteStringValue(HKLM, 'Software\Microsoft\Windows\CurrentVersion\App Paths\xmimsim-gui.exe', 'Path', ExpandConstant('{app}\Bin;{app}\Lib;{app}\GTK'));	
//  end;
    if (CurStep=ssPostInstall) then
    begin
	SaveStringToFile(ExpandConstant('{app}\Bin\set_xmi_msim_path.bat'), ExpandConstant('set PATH=%PATH%;{app}\Bin;{app}\Lib;{app}\GTK'), False);
    end;
end;

//procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
//begin
//  if (CurUninstallStep = usUninstall) then
//  begin
//	RegDeleteKeyIncludingSubkeys(HKLM, 'Software\Microsoft\Windows\CurrentVersion\App Paths\xmimsim-gui.exe');
//  end;
//end;
