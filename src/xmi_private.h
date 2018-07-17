/*
Copyright (C) 2010-2017 Tom Schoonjans and Laszlo Vincze

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/


#ifndef XMI_PRIVATE_H
#define XMI_PRIVATE_H

#include <config.h>
#include <glib.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifdef G_OS_WIN32
  #include <windows.h>
  #define XMI_ARGC_ORIG argc_orig
  #define XMI_ARGV_ORIG argv_orig
  #define XMI_ARGC argc
  #define XMI_ARGV argv
  #define XMI_MAIN int main(int argc_orig, char *argv_orig[]) {\
	int argc;\
	char **argv;\
	int argc_counter;\
	LPWSTR WinCommandLine = GetCommandLineW();\
	LPWSTR *WinArgv = CommandLineToArgvW(WinCommandLine,&argc);\
	argv = (char **) g_malloc(sizeof(char *)*argc);\
	for (argc_counter = 0 ; argc_counter < argc ; argc_counter++) {\
		int size_needed = WideCharToMultiByte(CP_UTF8, 0, WinArgv[argc_counter], -1, NULL, 0, NULL, NULL); \
		char *argvSingle = (char *) g_malloc(sizeof(char) * size_needed); \
		WideCharToMultiByte(CP_UTF8, 0 , WinArgv[argc_counter], -1, argvSingle, size_needed, NULL, NULL); \
		argv[argc_counter] = argvSingle; \
	} \
	LocalFree(WinArgv);

#else
  #define XMI_ARGC_ORIG argc
  #define XMI_ARGV_ORIG argv
  #define XMI_ARGC argc
  #define XMI_ARGV argv
  #define XMI_MAIN int main(int argc, char *argv[]) {

#endif

extern int64_t XMI_H5T_NATIVE_DOUBLE;
extern int64_t XMI_H5T_NATIVE_INT;


#include <libxml/xmlversion.h>
#if LIBXML_VERSION < 20901
#include <libxml/xpath.h>
#include <libxml/xpathInternals.h>
int xmlXPathSetContextNode(xmlNodePtr node, xmlXPathContextPtr ctx);
xmlXPathObjectPtr xmlXPathNodeEval(xmlNodePtr node, const xmlChar *str, xmlXPathContextPtr ctx);

#endif
#ifdef __cplusplus
}
#endif

#endif
