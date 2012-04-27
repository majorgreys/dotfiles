source ~/.bash/aliases
source ~/.bash/completions
source ~/.bash/paths
source ~/.bash/config

export ORACLE_HOME=~/Oracle/instantclient_10_2
export DYLD_LIBRARY_PATH="$ORACLE_HOME"
export VERSIONER_PYTHON_PREFER_32_BIT="yes"

# use .localrc for settings specific to one system
if [ -f ~/.localrc ]; then
  source ~/.localrc
fi
