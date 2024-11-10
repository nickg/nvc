#!bash completion for nvc(1)      -*- shell-script -*-

_nvc () {
  local cur prev words cword split
  _init_completion -s -n : || return

  local have_cmd
  for i in "${COMP_WORDS[@]}"; do
    case "$i" in
      -a|-e|-r|--do|--cover-export|--cover-report|--cover-merge)
        have_cmd=$i
        ;;
    esac
  done

  if [ "$cur" == "$have_cmd" ]; then
    COMPREPLY=( $(compgen -W "$have_cmd" -- $cur ) )
  fi

  case "$prev" in
    --work|-L)
      _filedir -d
      return
      ;;
    --load)
      _filedir '@(so|dll)'
      return
      ;;
    --std)
      COMPREPLY+=( $( compgen -W '1993 2000 2002 2008 2019' -- $cur ) )
      return
      ;;
    --messages)
      COMPREPLY+=( $( compgen -W 'full compact' -- $cur ) )
      return
      ;;
    -f|--files)
      _filedir
      return
      ;;
  esac

  local global_opts='-L -h --help --messages= --std= -v --version --init --list
                     --install --work= -a -e -r -i --dump --print-deps --load=
                     --map= --do --cover-report --cover-export --cover-merge'
  local analyse_opts='-D --define= --error-limit= --relaxed --psl --error-limit=
                      -f --files --no-save'
  local elab_opts='--cover --disable-opt --dump-llvm --dump-vcode --jit --no-save
                   --native -V --verbose'
  local run_opts='--trace --stop-time= --ieee-warnings= --stats= --stop-delta=
                  -w --wave --format='
  local export_opts='--format= -o --output='
  local merge_opts='-o --output='
  local report_opts='-o --output= --exclude-file= --dont-print= --item-limit='

  case "$have_cmd" in
    -a)
      _filedir '@(vhd|vhdl|v)'
      COMPREPLY+=( $( compgen -W "$analyse_opts" -- $cur ) )
      ;;
    -e)
      local toplevel=$(nvc --list | awk '/(Entity|Configuration)/ {
                                            split($1, a, ".");
                                            print tolower(a[2]);
                                         }')
      COMPREPLY+=( $( compgen -W "$toplevel" -- $cur )
                   $( compgen -W "$elab_opts" -- $cur ) )
      ;;
    -r)
      local toplevel=$(nvc --list | awk '/Elaborated/ {
                                            split($1, a, ".");
                                            print tolower(a[2]);
                                         }')
      COMPREPLY+=( $( compgen -W "$toplevel" -- $cur )
                   $( compgen -W "$run_opts" -- $cur ) )
      ;;
    --do)
      _filedir '@(pro|do|tcl)'
      ;;
    --cover-export)
      _filedir '@(covdb|ncdb)'
      COMPREPLY+=( $( compgen -W "$export_opts" -- $cur ) )
      ;;
    --cover-report)
      _filedir '@(covdb|ncdb)'
      COMPREPLY+=( $( compgen -W "$report_opts" -- $cur ) )
      ;;
    --cover-merge)
      _filedir '@(covdb|ncdb)'
      COMPREPLY+=( $( compgen -W "$merge_opts" -- $cur ) )
      ;;
    *)
      COMPREPLY+=( $( compgen -W "$global_opts" -- $cur ) )
      ;;
  esac

  [[ $COMPREPLY == *= ]] && compopt -o nospace

  return 0
} &&
complete -F _nvc nvc
