# -----------------------------------------------------------------------------
# Fishshell Samples
#  |- Theme / bobthefish
#  |- Function / funced
#  |- Configuration / config.fish
# -----------------------------------------------------------------------------

# name: bobthefish
#
# bobthefish is a Powerline-style, Git-aware fish theme optimized for awesome.
#
# You will probably need a Powerline-patched font for this to work:
#
#     https://powerline.readthedocs.org/en/latest/fontpatching.html
#
# I recommend picking one of these:
#
#     https://github.com/Lokaltog/powerline-fonts
#
# You can override some default options in your config.fish:
#
#     set -g theme_display_user yes
#     set -g default_user your_normal_user

set -g __bobthefish_current_bg NONE

# Powerline glyphs
set __bobthefish_branch_glyph            \uE0A0
set __bobthefish_ln_glyph                \uE0A1
set __bobthefish_padlock_glyph           \uE0A2
set __bobthefish_right_black_arrow_glyph \uE0B0
set __bobthefish_right_arrow_glyph       \uE0B1
set __bobthefish_left_black_arrow_glyph  \uE0B2
set __bobthefish_left_arrow_glyph        \uE0B3

# Additional glyphs
set __bobthefish_detached_glyph          \u27A6
set __bobthefish_nonzero_exit_glyph      '! '
set __bobthefish_superuser_glyph         '$ '
set __bobthefish_bg_job_glyph            '% '
set __bobthefish_hg_glyph                \u263F

# Python glyphs
set __bobthefish_superscript_glyph       \u00B9 \u00B2 \u00B3
set __bobthefish_virtualenv_glyph        \u25F0
set __bobthefish_pypy_glyph              \u1D56

# Colors
set __bobthefish_lt_green   addc10
set __bobthefish_med_green  189303
set __bobthefish_dk_green   0c4801

set __bobthefish_lt_red     C99
set __bobthefish_med_red    ce000f
set __bobthefish_dk_red     600

set __bobthefish_slate_blue 255e87

set __bobthefish_lt_orange  f6b117
set __bobthefish_dk_orange  3a2a03

set __bobthefish_dk_grey    333
set __bobthefish_med_grey   999
set __bobthefish_lt_grey    ccc

set __bobthefish_dk_brown   4d2600
set __bobthefish_med_brown  803F00
set __bobthefish_lt_brown   BF5E00

set __bobthefish_dk_blue    1E2933
set __bobthefish_med_blue   275379
set __bobthefish_lt_blue    326D9E

# ===========================
# Helper methods
# ===========================

function __bobthefish_in_git -d 'Check whether pwd is inside a git repo'
  command which git > /dev/null 2>&1; and command git rev-parse --is-inside-work-tree >/dev/null 2>&1
end

function __bobthefish_in_hg -d 'Check whether pwd is inside a hg repo'
  command which hg > /dev/null 2>&1; and command hg stat > /dev/null 2>&1
end

function __bobthefish_git_branch -d 'Get the current git branch (or commitish)'
  set -l ref (command git symbolic-ref HEAD 2> /dev/null)
  if [ $status -gt 0 ]
    set -l branch (command git show-ref --head -s --abbrev |head -n1 2> /dev/null)
    set ref "$__bobthefish_detached_glyph $branch"
  end
  echo $ref | sed  "s-refs/heads/-$__bobthefish_branch_glyph -"
end

function __bobthefish_hg_branch -d 'Get the current hg branch'
  set -l branch (hg branch ^/dev/null)
  set -l book " @ "(hg book | grep \* | cut -d\  -f3)
  echo "$__bobthefish_branch_glyph $branch$book"
end

function __bobthefish_pretty_parent -d 'Print a parent directory, shortened to fit the prompt'
  echo -n (dirname $argv[1]) | sed -e 's|/private||' -e "s|^$HOME|~|" -e 's-/\(\.\{0,1\}[^/]\)\([^/]*\)-/\1-g' -e 's|/$||'
end

function __bobthefish_git_project_dir -d 'Print the current git project base directory'
  command git rev-parse --show-toplevel 2>/dev/null
end

function __bobthefish_hg_project_dir -d 'Print the current hg project base directory'
  command hg root 2>/dev/null
end

function __bobthefish_project_pwd -d 'Print the working directory relative to project root'
  echo "$PWD" | sed -e "s*$argv[1]**g" -e 's*^/**'
end


# ===========================
# Segment functions
# ===========================

function __bobthefish_start_segment -d 'Start a prompt segment'
  set_color -b $argv[1]
  set_color $argv[2]
  if [ "$__bobthefish_current_bg" = 'NONE' ]
    # If there's no background, just start one
    echo -n ' '
  else
    # If there's already a background...
    if [ "$argv[1]" = "$__bobthefish_current_bg" ]
      # and it's the same color, draw a separator
      echo -n "$__bobthefish_right_arrow_glyph "
    else
      # otherwise, draw the end of the previous segment and the start of the next
      set_color $__bobthefish_current_bg
      echo -n "$__bobthefish_right_black_arrow_glyph "
      set_color $argv[2]
    end
  end
  set __bobthefish_current_bg $argv[1]
end

function __bobthefish_path_segment -d 'Display a shortened form of a directory'
  if test -w "$argv[1]"
    __bobthefish_start_segment $__bobthefish_dk_grey $__bobthefish_med_grey
  else
    __bobthefish_start_segment $__bobthefish_dk_red $__bobthefish_lt_red
  end

  set -l directory
  set -l parent

  switch "$argv[1]"
    case /
      set directory '/'
    case "$HOME"
      set directory '~'
    case '*'
      set parent    (__bobthefish_pretty_parent "$argv[1]")
      set parent    "$parent/"
      set directory (basename "$argv[1]")
  end

  test "$parent"; and echo -n -s "$parent"
  set_color fff --bold
  echo -n "$directory "
  set_color normal
end

function __bobthefish_finish_segments -d 'Close open prompt segments'
  if [ -n $__bobthefish_current_bg -a $__bobthefish_current_bg != 'NONE' ]
    set_color -b normal
    set_color $__bobthefish_current_bg
    echo -n "$__bobthefish_right_black_arrow_glyph "
    set_color normal
  end
  set -g __bobthefish_current_bg NONE
end


# ===========================
# Theme components
# ===========================

function __bobthefish_prompt_status -d 'Display symbols for a non zero exit status, root and background jobs'
  set -l nonzero
  set -l superuser
  set -l bg_jobs

  # Last exit was nonzero
  if [ $status -ne 0 ]
    set nonzero $__bobthefish_nonzero_exit_glyph
  end

  # if superuser (uid == 0)
  set -l uid (id -u $USER)
  if [ $uid -eq 0 ]
    set superuser $__bobthefish_superuser_glyph
  end

  # Jobs display
  if [ (jobs -l | wc -l) -gt 0 ]
    set bg_jobs $__bobthefish_bg_job_glyph
  end

  set -l status_flags "$nonzero$superuser$bg_jobs"

  if test "$nonzero" -o "$superuser" -o "$bg_jobs"
    __bobthefish_start_segment fff 000
    if [ "$nonzero" ]
      set_color $__bobthefish_med_red --bold
      echo -n $__bobthefish_nonzero_exit_glyph
    end

    if [ "$superuser" ]
      set_color $__bobthefish_med_green --bold
      echo -n $__bobthefish_superuser_glyph
    end

    if [ "$bg_jobs" ]
      set_color $__bobthefish_slate_blue --bold
      echo -n $__bobthefish_bg_job_glyph
    end

    set_color normal
  end
end

function __bobthefish_prompt_user -d 'Display actual user if different from $default_user'
  if [ "$theme_display_user" = 'yes' ]
    if [ "$USER" != "$default_user" -o -n "$SSH_CLIENT" ]
      __bobthefish_start_segment $__bobthefish_lt_grey $__bobthefish_slate_blue
      echo -n -s (whoami) '@' (hostname | cut -d . -f 1) ' '
    end
  end
end

function __bobthefish_prompt_hg -d 'Display the actual hg state'
  set -l dirty   (command hg stat; or echo -n '*')

  set -l flags "$dirty"
  test "$flags"; and set flags ""

  set -l flag_bg $__bobthefish_lt_green
  set -l flag_fg $__bobthefish_dk_green
  if test "$dirty"
    set flag_bg $__bobthefish_med_red
    set flag_fg fff
  end

  __bobthefish_path_segment (__bobthefish_hg_project_dir)

  __bobthefish_start_segment $flag_bg $flag_fg
  echo -n -s $__bobthefish_hg_glyph ' '

  __bobthefish_start_segment $flag_bg $flag_fg
  set_color $flag_fg --bold
  echo -n -s (__bobthefish_hg_branch) $flags ' '
  set_color normal

  set -l project_pwd  (__bobthefish_project_pwd (__bobthefish_hg_project_dir))
  if test "$project_pwd"
    if test -w "$PWD"
      __bobthefish_start_segment 333 999
    else
      __bobthefish_start_segment $__bobthefish_med_red $__bobthefish_lt_red
    end

    echo -n -s $project_pwd ' '
  end
end

# TODO: clean up the fugly $ahead business
function __bobthefish_prompt_git -d 'Display the actual git state'
  set -l dirty   (command git diff --no-ext-diff --quiet --exit-code; or echo -n '*')
  set -l staged  (command git diff --cached --no-ext-diff --quiet --exit-code; or echo -n '~')
  set -l stashed (command git rev-parse --verify refs/stash > /dev/null 2>&1; and echo -n '$')
  set -l ahead   (command git branch -v 2> /dev/null | grep -Eo '^\* [^ ]* *[^ ]* *\[[^]]*\]' | grep -Eo '\[[^]]*\]$' | awk 'ORS="";/ahead/ {print "+"} /behind/ {print "-"}' | sed -e 's/+-/±/')

  set -l new (command git ls-files --other --exclude-standard);
  test "$new"; and set new '…'

  set -l flags   "$dirty$staged$stashed$ahead$new"
  test "$flags"; and set flags " $flags"

  set -l flag_bg $__bobthefish_lt_green
  set -l flag_fg $__bobthefish_dk_green
  if test "$dirty" -o "$staged"
    set flag_bg $__bobthefish_med_red
    set flag_fg fff
  else
    if test "$stashed"
      set flag_bg $__bobthefish_lt_orange
      set flag_fg $__bobthefish_dk_orange
    end
  end

  __bobthefish_path_segment (__bobthefish_git_project_dir)

  __bobthefish_start_segment $flag_bg $flag_fg
  set_color $flag_fg --bold
  echo -n -s (__bobthefish_git_branch) $flags ' '
  set_color normal

  set -l project_pwd  (__bobthefish_project_pwd (__bobthefish_git_project_dir))
  if test "$project_pwd"
    if test -w "$PWD"
      __bobthefish_start_segment 333 999
    else
      __bobthefish_start_segment $__bobthefish_med_red $__bobthefish_lt_red
    end

    echo -n -s $project_pwd ' '
  end
end

function __bobthefish_prompt_dir -d 'Display a shortened form of the current directory'
  __bobthefish_path_segment "$PWD"
end

function __bobthefish_in_virtualfish_virtualenv
  set -q VIRTUAL_ENV
end

function __bobthefish_virtualenv_python_version -d 'Get current python version'
  switch (readlink (which python))
    case python2
      echo $__bobthefish_superscript_glyph[2]
    case python3
      echo $__bobthefish_superscript_glyph[3]
    case pypy
      echo $__bobthefish_pypy_glyph
    end
end

function __bobthefish_virtualenv -d 'Get the current virtualenv'
  echo $__bobthefish_virtualenv_glyph(__bobthefish_virtualenv_python_version) (basename "$VIRTUAL_ENV")
end

function __bobthefish_prompt_virtualfish -d "Display activated virtual environment (only for virtualfish, virtualenv's activate.fish changes prompt by itself)"
  set flag_bg $__bobthefish_lt_blue
  set flag_fg $__bobthefish_dk_blue
  __bobthefish_start_segment $flag_bg $flag_fg
  set_color $flag_fg --bold
  echo -n -s (__bobthefish_virtualenv) $flags ' '
  set_color normal
end


# ===========================
# Apply theme
# ===========================

function fish_prompt -d 'bobthefish, a fish theme optimized for awesome'
  __bobthefish_prompt_status
  __bobthefish_prompt_user
  if __bobthefish_in_virtualfish_virtualenv
    __bobthefish_prompt_virtualfish
  end
  if __bobthefish_in_git       # TODO: do this right.
    __bobthefish_prompt_git    # if something is in both git and hg, check the length of
  else if __bobthefish_in_hg   # __bobthefish_git_project_dir vs __bobthefish_hg_project_dir
    __bobthefish_prompt_hg     # and pick the longer of the two.
  else
    __bobthefish_prompt_dir
  end
  __bobthefish_finish_segments
end

# -----------------------------------------------------------------------------
# funced - edit a function interactively
#
# Synopsis
#
#   funced [OPTIONS] NAME
#
# Description
#
#   funced provides an interface to edit the definition of the function NAME.
# -----------------------------------------------------------------------------

function funced --description 'Edit function definition'
    set -l editor $EDITOR
    set -l interactive
    set -l funcname
    while set -q argv[1]
        switch $argv[1]
            case -h --help
                __fish_print_help funced
                return 0

            case -e --editor
                set editor $argv[2]
                set -e argv[2]

            case -i --interactive
                set interactive 1

            case --
                set funcname $funcname $argv[2]
                set -e argv[2]

            case '-*'
                set_color red
                printf (_ "%s: Unknown option %s\n") funced $argv[1]
                set_color normal
                return 1

            case '*' '.*'
                set funcname $funcname $argv[1]
        end
        set -e argv[1]
    end

    if begin; set -q funcname[2]; or not test "$funcname[1]"; end
        set_color red
        _ "funced: You must specify one function name
"
        set_color normal
        return 1
    end

    set -l init
    switch $funcname
        case '-*'
        set init function -- $funcname\n\nend
        case '*'
        set init function $funcname\n\nend
    end

    # Break editor up to get its first command (i.e. discard flags)
    if test -n "$editor"
        set -l editor_cmd
        eval set editor_cmd $editor
        if not type -f "$editor_cmd[1]" >/dev/null
            _ "funced: The value for \$EDITOR '$editor' could not be used because the command '$editor_cmd[1]' could not be found
    "
            set editor fish
        end
    end

    # If no editor is specified, use fish
    if test -z "$editor"
        set editor fish
    end

    if begin; set -q interactive[1]; or test "$editor" = fish; end
        set -l IFS
        if functions -q -- $funcname
            # Shadow IFS here to avoid array splitting in command substitution
            set init (functions -- $funcname | fish_indent --no-indent)
        end

        set -l prompt 'printf "%s%s%s> " (set_color green) '$funcname' (set_color normal)'
        # Unshadow IFS since the fish_title breaks otherwise
        set -e IFS
        if read -p $prompt -c "$init" -s cmd
            # Shadow IFS _again_ to avoid array splitting in command substitution
            set -l IFS
            eval (echo -n $cmd | fish_indent)
        end
        return 0
    end

    set -q TMPDIR; or set -l TMPDIR /tmp
    set -l tmpname (printf "$TMPDIR/fish_funced_%d_%d.fish" %self (random))
    while test -f $tmpname
        set tmpname (printf "$TMPDIR/fish_funced_%d_%d.fish" %self (random))
    end

    if functions -q -- $funcname
        functions -- $funcname > $tmpname
    else
        echo $init > $tmpname
    end
    if eval $editor $tmpname
        . $tmpname
    end
    set -l stat $status
    rm -f $tmpname >/dev/null
    return $stat
end

# -----------------------------------------------------------------------------
# Main file for fish command completions. This file contains various
# common helper functions for the command completions. All actual
# completions are located in the completions subdirectory.
## -----------------------------------------------------------------------------

#
# Set default field separators
#

set -g IFS \n\ \t

#
# Set default search paths for completions and shellscript functions
# unless they already exist
#

set -l configdir ~/.config

if set -q XDG_CONFIG_HOME
  set configdir $XDG_CONFIG_HOME
end

# __fish_datadir, __fish_sysconfdir, __fish_help_dir, __fish_bin_dir
# are expected to have been set up by read_init from fish.cpp

# Set up function and completion paths. Make sure that the fish
# default functions/completions are included in the respective path.

if not set -q fish_function_path
  set fish_function_path $configdir/fish/functions    $__fish_sysconfdir/functions    $__fish_datadir/functions
end

if not contains $__fish_datadir/functions $fish_function_path
  set fish_function_path[-1] $__fish_datadir/functions
end

if not set -q fish_complete_path
  set fish_complete_path $configdir/fish/completions  $__fish_sysconfdir/completions  $__fish_datadir/completions
end

if not contains $__fish_datadir/completions $fish_complete_path
  set fish_complete_path[-1] $__fish_datadir/completions
end

#
# This is a Solaris-specific test to modify the PATH so that
# Posix-conformant tools are used by default. It is separate from the
# other PATH code because this directory needs to be prepended, not
# appended, since it contains POSIX-compliant replacements for various
# system utilities.
#

if test -d /usr/xpg4/bin
  if not contains /usr/xpg4/bin $PATH
    set PATH /usr/xpg4/bin $PATH
  end
end

#
# Add a few common directories to path, if they exists. Note that pure
# console programs like makedep sometimes live in /usr/X11R6/bin, so we
# want this even for text-only terminals.
#

set -l path_list /bin /usr/bin /usr/X11R6/bin /usr/local/bin $__fish_bin_dir

# Root should also have the sbin directories in the path
switch $USER
  case root
  set path_list $path_list /sbin /usr/sbin /usr/local/sbin
end

for i in $path_list
  if not contains $i $PATH
    if test -d $i
      set PATH $PATH $i
    end
  end
end

#
# Launch debugger on SIGTRAP
#
function fish_sigtrap_handler --on-signal TRAP --no-scope-shadowing --description "Signal handler for the TRAP signal. Lanches a debug prompt."
  breakpoint
end

#
# Whenever a prompt is displayed, make sure that interactive
# mode-specific initializations have been performed.
# This handler removes itself after it is first called.
#
function __fish_on_interactive --on-event fish_prompt
  __fish_config_interactive
  functions -e __fish_on_interactive
end
