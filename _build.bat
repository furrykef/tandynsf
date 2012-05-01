@echo off
call dmc-env
"C:\Program Files\NASM\nasm" playnsf.asm -fobj
if errorlevel 1 goto end
"C:\Program Files\NASM\nasm" 6502.asm -fobj
if errorlevel 1 goto end
link /dosseg playnsf.obj+6502.obj,tandynsf.exe
:end
pause
