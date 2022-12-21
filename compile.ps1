Param(
	[string] $choice = "both"
)

if($choice -match "compiler|both") {

if(-not (haxe build.hxml)) {
	exit
}

$mainFile = "bin/output/main.c"
$mainData = get-content $mainFile
if(-not $mainData.contains("#undef ERROR")) {
	$mainData = $mainData.Replace("#include <hlc_main.c>", "#include <hlc_main.c>`n#undef ERROR")
	$mainData | out-file -encoding ASCII $mainFile
}

$nativesFile = "bin/output/hl/natives.h"
$nativesData = get-content $nativesFile
if(-not $nativesData.contains("#ifndef HL_H")) {
	$nativesData = $nativesData.replace(
		"typedef struct _hl_buffer hl_buffer;",
		"#ifndef HL_H
typedef struct _hl_buffer hl_buffer;
#endif"
	)

	$nativesData = $nativesData.replace(
		"HL_API void hl_buffer_char(hl_buffer*,int);
HL_API vbyte* hl_buffer_content(hl_buffer*,int*);
HL_API int hl_buffer_length(hl_buffer*);
HL_API void hl_buffer_str(hl_buffer*,vbyte*);",
		"#ifndef HL_H
HL_API void hl_buffer_char(hl_buffer*,int);
HL_API vbyte* hl_buffer_content(hl_buffer*,int*);
HL_API int hl_buffer_length(hl_buffer*);
HL_API void hl_buffer_str(hl_buffer*,vbyte*);
#endif"
	)

	$nativesData = $nativesData.replace(
		"HL_API void hl_buffer_char(hl_buffer*,int);",
		"#ifndef HL_H`nHL_API void hl_buffer_char(hl_buffer*,int);"
	);

	$nativesData = $nativesData.replace(
		"HL_API void hl_buffer_str(hl_buffer*,vbyte*);",
		"HL_API void hl_buffer_str(hl_buffer*,vbyte*);`n#endif"
	)

	$nativesData | out-file -encoding ASCII $nativesFile
}

# My VS setup is really fucked for some reason, so this is what I use to make it work

$path = (ls "C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Tools\MSVC")[-1].FullName
$winIncPath = (ls "C:\Program Files (x86)\Windows Kits\10\Include")[-1].FullName

$objs = (ls $env:HASHLINKPATH\x64\Release\*.obj | ForEach-Object {$_.FullName})

& "$path\bin\Hostx64\x64\cl.exe" `
	/TC `
	/I "$env:HASHLINKPATH\src" /I "bin/output" `
	/I "$path\include" `
	/I "$winIncPath\um" `
	/I "$winIncPath\shared" `
	/I "$winIncPath\ucrt" `
	/Fe"bin/starc.exe" `
	/Fo"bin/starc.obj" `
	bin/output/main.c `
	/link `
		/LIBPATH:"$env:HASHLINKPATH\Release" `
		("/LIBPATH:" + (ls "C:\Program Files (x86)\Windows Kits\10\Lib")[-1].FullName + "\um\x64") `
		("/LIBPATH:" + (ls "C:\Program Files (x86)\Windows Kits\10\Lib")[-1].FullName + "\ucrt\x64") `
		/LIBPATH:"$path\lib\x64" `
		@objs `
		/NODEFAULTLIB:LIBCMT /FORCE

}

if($choice -match "vm|both") {

echo (nim --out:bin/starvm.exe c vm/main.nim)

}