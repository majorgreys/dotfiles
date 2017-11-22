REM Requires Developer Mode

REM Creating symbolic links

REM VIM
del %HOMEPATH%\.vimrc
mklink %HOMEPATH%\.vimrc %cd%\.vimrc

rmdir  %HOMEPATH%\.config\nvim
mklink /D %HOMEPATH%\.config\nvim %cd%\.config\nvim
