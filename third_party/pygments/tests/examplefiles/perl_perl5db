
=head1 NAME 

perl5db.pl - the perl debugger

=head1 SYNOPSIS

    perl -d  your_Perl_script

=head1 DESCRIPTION

After this routine is over, we don't have user code executing in the debugger's
context, so we can use C<my> freely.

=cut

############################################## Begin lexical danger zone

# 'my' variables used here could leak into (that is, be visible in)
# the context that the code being evaluated is executing in. This means that
# the code could modify the debugger's variables.
#
# Fiddling with the debugger's context could be Bad. We insulate things as
# much as we can.

sub eval {

    # 'my' would make it visible from user code
    #    but so does local! --tchrist
    # Remember: this localizes @DB::res, not @main::res.
    local @res;
    {

        # Try to keep the user code from messing  with us. Save these so that
        # even if the eval'ed code changes them, we can put them back again.
        # Needed because the user could refer directly to the debugger's
        # package globals (and any 'my' variables in this containing scope)
        # inside the eval(), and we want to try to stay safe.
        local $otrace  = $trace;
        local $osingle = $single;
        local $od      = $^D;

        # Untaint the incoming eval() argument.
        { ($evalarg) = $evalarg =~ /(.*)/s; }

        # $usercontext built in DB::DB near the comment
        # "set up the context for DB::eval ..."
        # Evaluate and save any results.
        @res = eval "$usercontext $evalarg;\n";  # '\n' for nice recursive debug

        # Restore those old values.
        $trace  = $otrace;
        $single = $osingle;
        $^D     = $od;
    }

    # Save the current value of $@, and preserve it in the debugger's copy
    # of the saved precious globals.
    my $at = $@;

    # Since we're only saving $@, we only have to localize the array element
    # that it will be stored in.
    local $saved[0];    # Preserve the old value of $@
    eval { &DB::save };

    # Now see whether we need to report an error back to the user.
    if ($at) {
        local $\ = '';
        print $OUT $at;
    }

    # Display as required by the caller. $onetimeDump and $onetimedumpDepth
    # are package globals.
    elsif ($onetimeDump) {
        if ( $onetimeDump eq 'dump' ) {
            local $option{dumpDepth} = $onetimedumpDepth
              if defined $onetimedumpDepth;
            dumpit( $OUT, \@res );
        }
        elsif ( $onetimeDump eq 'methods' ) {
            methods( $res[0] );
        }
    } ## end elsif ($onetimeDump)
    @res;
} ## end sub eval

############################################## End lexical danger zone

# After this point it is safe to introduce lexicals.
# The code being debugged will be executing in its own context, and
# can't see the inside of the debugger.
#
# However, one should not overdo it: leave as much control from outside as
# possible. If you make something a lexical, it's not going to be addressable
# from outside the debugger even if you know its name.

# This file is automatically included if you do perl -d.
# It's probably not useful to include this yourself.
#
# Before venturing further into these twisty passages, it is
# wise to read the perldebguts man page or risk the ire of dragons.
#
# (It should be noted that perldebguts will tell you a lot about
# the underlying mechanics of how the debugger interfaces into the
# Perl interpreter, but not a lot about the debugger itself. The new
# comments in this code try to address this problem.)

# Note that no subroutine call is possible until &DB::sub is defined
# (for subroutines defined outside of the package DB). In fact the same is
# true if $deep is not defined.

# Enhanced by ilya@math.ohio-state.edu (Ilya Zakharevich)

# modified Perl debugger, to be run from Emacs in perldb-mode
# Ray Lischner (uunet!mntgfx!lisch) as of 5 Nov 1990
# Johan Vromans -- upgrade to 4.0 pl 10
# Ilya Zakharevich -- patches after 5.001 (and some before ;-)

# (We have made efforts to  clarify the comments in the change log
# in other places; some of them may seem somewhat obscure as they
# were originally written, and explaining them away from the code
# in question seems conterproductive.. -JM)

=head1 DEBUGGER INITIALIZATION

The debugger starts up in phases.

=head2 BASIC SETUP

First, it initializes the environment it wants to run in: turning off
warnings during its own compilation, defining variables which it will need
to avoid warnings later, setting itself up to not exit when the program
terminates, and defaulting to printing return values for the C<r> command.

=cut

# Needed for the statement after exec():
#
# This BEGIN block is simply used to switch off warnings during debugger
# compiliation. Probably it would be better practice to fix the warnings,
# but this is how it's done at the moment.

BEGIN {
    $ini_warn = $^W;
    $^W       = 0;
}    # Switch compilation warnings off until another BEGIN.

# test if assertions are supported and actived:
BEGIN {
    $ini_assertion = eval "sub asserting_test : assertion {1}; 1";

    # $ini_assertion = undef => assertions unsupported,
    #        "       = 1     => assertions supported
    # print "\$ini_assertion=$ini_assertion\n";
}

local ($^W) = 0;    # Switch run-time warnings off during init.

=head2 THREADS SUPPORT

If we are running under a threaded Perl, we require threads and threads::shared
if the environment variable C<PERL5DB_THREADED> is set, to enable proper
threaded debugger control.  C<-dt> can also be used to set this.

Each new thread will be announced and the debugger prompt will always inform
you of each new thread created.  It will also indicate the thread id in which
we are currently running within the prompt like this:

	[tid] DB<$i>

Where C<[tid]> is an integer thread id and C<$i> is the familiar debugger
command prompt.  The prompt will show: C<[0]> when running under threads, but
not actually in a thread.  C<[tid]> is consistent with C<gdb> usage.

While running under threads, when you set or delete a breakpoint (etc.), this
will apply to all threads, not just the currently running one.  When you are 
in a currently executing thread, you will stay there until it completes.  With
the current implementation it is not currently possible to hop from one thread
to another.

The C<e> and C<E> commands are currently fairly minimal - see C<h e> and C<h E>.

Note that threading support was built into the debugger as of Perl version
C<5.8.6> and debugger version C<1.2.8>.

=cut

BEGIN {
  # ensure we can share our non-threaded variables or no-op
  if ($ENV{PERL5DB_THREADED}) {
	require threads;
	require threads::shared;
	import threads::shared qw(share);
	$DBGR;
	share(\$DBGR);
	lock($DBGR);
	print "Threads support enabled\n";
  } else {
	*lock  = sub(*) {};
	*share = sub(*) {};
  }
}

# This would probably be better done with "use vars", but that wasn't around
# when this code was originally written. (Neither was "use strict".) And on
# the principle of not fiddling with something that was working, this was
# left alone.
warn(               # Do not ;-)
    # These variables control the execution of 'dumpvar.pl'.
    $dumpvar::hashDepth,
    $dumpvar::arrayDepth,
    $dumpvar::dumpDBFiles,
    $dumpvar::dumpPackages,
    $dumpvar::quoteHighBit,
    $dumpvar::printUndef,
    $dumpvar::globPrint,
    $dumpvar::usageOnly,

    # used to save @ARGV and extract any debugger-related flags.
    @ARGS,

    # used to control die() reporting in diesignal()
    $Carp::CarpLevel,

    # used to prevent multiple entries to diesignal()
    # (if for instance diesignal() itself dies)
    $panic,

    # used to prevent the debugger from running nonstop
    # after a restart
    $second_time,
  )
  if 0;

foreach my $k (keys (%INC)) {
	&share(\$main::{'_<'.$filename});
};

# Command-line + PERLLIB:
# Save the contents of @INC before they are modified elsewhere.
@ini_INC = @INC;

# This was an attempt to clear out the previous values of various
# trapped errors. Apparently it didn't help. XXX More info needed!
# $prevwarn = $prevdie = $prevbus = $prevsegv = ''; # Does not help?!

# We set these variables to safe values. We don't want to blindly turn
# off warnings, because other packages may still want them.
$trace = $signal = $single = 0;    # Uninitialized warning suppression
                                   # (local $^W cannot help - other packages!).

# Default to not exiting when program finishes; print the return
# value when the 'r' command is used to return from a subroutine.
$inhibit_exit = $option{PrintRet} = 1;

=head1 OPTION PROCESSING

The debugger's options are actually spread out over the debugger itself and 
C<dumpvar.pl>; some of these are variables to be set, while others are 
subs to be called with a value. To try to make this a little easier to
manage, the debugger uses a few data structures to define what options
are legal and how they are to be processed.

First, the C<@options> array defines the I<names> of all the options that
are to be accepted.

=cut

@options = qw(
  CommandSet
  hashDepth    arrayDepth    dumpDepth
  DumpDBFiles  DumpPackages  DumpReused
  compactDump  veryCompact   quote
  HighBit      undefPrint    globPrint
  PrintRet     UsageOnly     frame
  AutoTrace    TTY           noTTY
  ReadLine     NonStop       LineInfo
  maxTraceLen  recallCommand ShellBang
  pager        tkRunning     ornaments
  signalLevel  warnLevel     dieLevel
  inhibit_exit ImmediateStop bareStringify
  CreateTTY    RemotePort    windowSize
  DollarCaretP OnlyAssertions WarnAssertions
);

@RememberOnROptions = qw(DollarCaretP OnlyAssertions);

=pod

Second, C<optionVars> lists the variables that each option uses to save its
state.

=cut

%optionVars = (
    hashDepth     => \$dumpvar::hashDepth,
    arrayDepth    => \$dumpvar::arrayDepth,
    CommandSet    => \$CommandSet,
    DumpDBFiles   => \$dumpvar::dumpDBFiles,
    DumpPackages  => \$dumpvar::dumpPackages,
    DumpReused    => \$dumpvar::dumpReused,
    HighBit       => \$dumpvar::quoteHighBit,
    undefPrint    => \$dumpvar::printUndef,
    globPrint     => \$dumpvar::globPrint,
    UsageOnly     => \$dumpvar::usageOnly,
    CreateTTY     => \$CreateTTY,
    bareStringify => \$dumpvar::bareStringify,
    frame         => \$frame,
    AutoTrace     => \$trace,
    inhibit_exit  => \$inhibit_exit,
    maxTraceLen   => \$maxtrace,
    ImmediateStop => \$ImmediateStop,
    RemotePort    => \$remoteport,
    windowSize    => \$window,
    WarnAssertions => \$warnassertions,
);

=pod

Third, C<%optionAction> defines the subroutine to be called to process each
option.

=cut 

%optionAction = (
    compactDump   => \&dumpvar::compactDump,
    veryCompact   => \&dumpvar::veryCompact,
    quote         => \&dumpvar::quote,
    TTY           => \&TTY,
    noTTY         => \&noTTY,
    ReadLine      => \&ReadLine,
    NonStop       => \&NonStop,
    LineInfo      => \&LineInfo,
    recallCommand => \&recallCommand,
    ShellBang     => \&shellBang,
    pager         => \&pager,
    signalLevel   => \&signalLevel,
    warnLevel     => \&warnLevel,
    dieLevel      => \&dieLevel,
    tkRunning     => \&tkRunning,
    ornaments     => \&ornaments,
    RemotePort    => \&RemotePort,
    DollarCaretP  => \&DollarCaretP,
    OnlyAssertions=> \&OnlyAssertions,
);

=pod

Last, the C<%optionRequire> notes modules that must be C<require>d if an
option is used.

=cut

# Note that this list is not complete: several options not listed here
# actually require that dumpvar.pl be loaded for them to work, but are
# not in the table. A subsequent patch will correct this problem; for
# the moment, we're just recommenting, and we are NOT going to change
# function.
%optionRequire = (
    compactDump => 'dumpvar.pl',
    veryCompact => 'dumpvar.pl',
    quote       => 'dumpvar.pl',
);

=pod

There are a number of initialization-related variables which can be set
by putting code to set them in a BEGIN block in the C<PERL5DB> environment
variable. These are:

=over 4

=item C<$rl> - readline control XXX needs more explanation

=item C<$warnLevel> - whether or not debugger takes over warning handling

=item C<$dieLevel> - whether or not debugger takes over die handling

=item C<$signalLevel> - whether or not debugger takes over signal handling

=item C<$pre> - preprompt actions (array reference)

=item C<$post> - postprompt actions (array reference)

=item C<$pretype>

=item C<$CreateTTY> - whether or not to create a new TTY for this debugger

=item C<$CommandSet> - which command set to use (defaults to new, documented set)

=back

=cut

# These guys may be defined in $ENV{PERL5DB} :
$rl          = 1     unless defined $rl;
$warnLevel   = 1     unless defined $warnLevel;
$dieLevel    = 1     unless defined $dieLevel;
$signalLevel = 1     unless defined $signalLevel;
$pre         = []    unless defined $pre;
$post        = []    unless defined $post;
$pretype     = []    unless defined $pretype;
$CreateTTY   = 3     unless defined $CreateTTY;
$CommandSet  = '580' unless defined $CommandSet;

share($rl);
share($warnLevel);
share($dieLevel);
share($signalLevel);
share($pre);
share($post);
share($pretype);
share($rl);
share($CreateTTY);
share($CommandSet);

=pod

The default C<die>, C<warn>, and C<signal> handlers are set up.

=cut

warnLevel($warnLevel);
dieLevel($dieLevel);
signalLevel($signalLevel);

=pod

The pager to be used is needed next. We try to get it from the
environment first.  if it's not defined there, we try to find it in
the Perl C<Config.pm>.  If it's not there, we default to C<more>. We
then call the C<pager()> function to save the pager name.

=cut

# This routine makes sure $pager is set up so that '|' can use it.
pager(

    # If PAGER is defined in the environment, use it.
    defined $ENV{PAGER}
    ? $ENV{PAGER}

      # If not, see if Config.pm defines it.
    : eval { require Config }
      && defined $Config::Config{pager}
    ? $Config::Config{pager}

      # If not, fall back to 'more'.
    : 'more'
  )
  unless defined $pager;

=pod

We set up the command to be used to access the man pages, the command
recall character (C<!> unless otherwise defined) and the shell escape
character (C<!> unless otherwise defined). Yes, these do conflict, and
neither works in the debugger at the moment.

=cut

setman();

# Set up defaults for command recall and shell escape (note:
# these currently don't work in linemode debugging).
&recallCommand("!") unless defined $prc;
&shellBang("!")     unless defined $psh;

=pod

We then set up the gigantic string containing the debugger help.
We also set the limit on the number of arguments we'll display during a
trace.

=cut

sethelp();

# If we didn't get a default for the length of eval/stack trace args,
# set it here.
$maxtrace = 400 unless defined $maxtrace;

=head2 SETTING UP THE DEBUGGER GREETING

The debugger I<greeting> helps to inform the user how many debuggers are
running, and whether the current debugger is the primary or a child.

If we are the primary, we just hang onto our pid so we'll have it when
or if we start a child debugger. If we are a child, we'll set things up
so we'll have a unique greeting and so the parent will give us our own
TTY later.

We save the current contents of the C<PERLDB_PIDS> environment variable
because we mess around with it. We'll also need to hang onto it because
we'll need it if we restart.

Child debuggers make a label out of the current PID structure recorded in
PERLDB_PIDS plus the new PID. They also mark themselves as not having a TTY
yet so the parent will give them one later via C<resetterm()>.

=cut

# Save the current contents of the environment; we're about to
# much with it. We'll need this if we have to restart.
$ini_pids = $ENV{PERLDB_PIDS};

if ( defined $ENV{PERLDB_PIDS} ) {

    # We're a child. Make us a label out of the current PID structure
    # recorded in PERLDB_PIDS plus our (new) PID. Mark us as not having
    # a term yet so the parent will give us one later via resetterm().
    $pids = "[$ENV{PERLDB_PIDS}]";
    $ENV{PERLDB_PIDS} .= "->$$";
    $term_pid = -1;
} ## end if (defined $ENV{PERLDB_PIDS...
else {

    # We're the parent PID. Initialize PERLDB_PID in case we end up with a
    # child debugger, and mark us as the parent, so we'll know to set up
    # more TTY's is we have to.
    $ENV{PERLDB_PIDS} = "$$";
    $pids             = "{pid=$$}";
    $term_pid         = $$;
}

$pidprompt = '';

# Sets up $emacs as a synonym for $slave_editor.
*emacs = $slave_editor if $slave_editor;    # May be used in afterinit()...

=head2 READING THE RC FILE

The debugger will read a file of initialization options if supplied. If    
running interactively, this is C<.perldb>; if not, it's C<perldb.ini>.

=cut      

# As noted, this test really doesn't check accurately that the debugger
# is running at a terminal or not.

if ( -e "/dev/tty" ) {                      # this is the wrong metric!
    $rcfile = ".perldb";
}
else {
    $rcfile = "perldb.ini";
}

=pod

The debugger does a safety test of the file to be read. It must be owned
either by the current user or root, and must only be writable by the owner.

=cut

# This wraps a safety test around "do" to read and evaluate the init file.
#
# This isn't really safe, because there's a race
# between checking and opening.  The solution is to
# open and fstat the handle, but then you have to read and
# eval the contents.  But then the silly thing gets
# your lexical scope, which is unfortunate at best.
sub safe_do {
    my $file = shift;

    # Just exactly what part of the word "CORE::" don't you understand?
    local $SIG{__WARN__};
    local $SIG{__DIE__};

    unless ( is_safe_file($file) ) {
        CORE::warn <<EO_GRIPE;
perldb: Must not source insecure rcfile $file.
        You or the superuser must be the owner, and it must not 
        be writable by anyone but its owner.
EO_GRIPE
        return;
    } ## end unless (is_safe_file($file...

    do $file;
    CORE::warn("perldb: couldn't parse $file: $@") if $@;
} ## end sub safe_do

# This is the safety test itself.
#
# Verifies that owner is either real user or superuser and that no
# one but owner may write to it.  This function is of limited use
# when called on a path instead of upon a handle, because there are
# no guarantees that filename (by dirent) whose file (by ino) is
# eventually accessed is the same as the one tested.
# Assumes that the file's existence is not in doubt.
sub is_safe_file {
    my $path = shift;
    stat($path) || return;    # mysteriously vaporized
    my ( $dev, $ino, $mode, $nlink, $uid, $gid ) = stat(_);

    return 0 if $uid != 0 && $uid != $<;
    return 0 if $mode & 022;
    return 1;
} ## end sub is_safe_file

# If the rcfile (whichever one we decided was the right one to read)
# exists, we safely do it.
if ( -f $rcfile ) {
    safe_do("./$rcfile");
}

# If there isn't one here, try the user's home directory.
elsif ( defined $ENV{HOME} && -f "$ENV{HOME}/$rcfile" ) {
    safe_do("$ENV{HOME}/$rcfile");
}

# Else try the login directory.
elsif ( defined $ENV{LOGDIR} && -f "$ENV{LOGDIR}/$rcfile" ) {
    safe_do("$ENV{LOGDIR}/$rcfile");
}

# If the PERLDB_OPTS variable has options in it, parse those out next.
if ( defined $ENV{PERLDB_OPTS} ) {
    parse_options( $ENV{PERLDB_OPTS} );
}

=pod

The last thing we do during initialization is determine which subroutine is
to be used to obtain a new terminal when a new debugger is started. Right now,
the debugger only handles X Windows and OS/2.

=cut

# Set up the get_fork_TTY subroutine to be aliased to the proper routine.
# Works if you're running an xterm or xterm-like window, or you're on
# OS/2. This may need some expansion: for instance, this doesn't handle
# OS X Terminal windows.

if (
    not defined &get_fork_TTY    # no routine exists,
    and defined $ENV{TERM}       # and we know what kind
                                 # of terminal this is,
    and $ENV{TERM} eq 'xterm'    # and it's an xterm,
#   and defined $ENV{WINDOWID}   # and we know what window this is, <- wrong metric
    and defined $ENV{DISPLAY}    # and what display it's on,
  )
{
    *get_fork_TTY = \&xterm_get_fork_TTY;    # use the xterm version
} ## end if (not defined &get_fork_TTY...
elsif ( $^O eq 'os2' ) {                     # If this is OS/2,
    *get_fork_TTY = \&os2_get_fork_TTY;      # use the OS/2 version
}

# untaint $^O, which may have been tainted by the last statement.
# see bug [perl #24674]
$^O =~ m/^(.*)\z/;
$^O = $1;

# Here begin the unreadable code.  It needs fixing.

=head2 RESTART PROCESSING

This section handles the restart command. When the C<R> command is invoked, it
tries to capture all of the state it can into environment variables, and
then sets C<PERLDB_RESTART>. When we start executing again, we check to see
if C<PERLDB_RESTART> is there; if so, we reload all the information that
the R command stuffed into the environment variables.

  PERLDB_RESTART   - flag only, contains no restart data itself.       
  PERLDB_HIST      - command history, if it's available
  PERLDB_ON_LOAD   - breakpoints set by the rc file
  PERLDB_POSTPONE  - subs that have been loaded/not executed, and have actions
  PERLDB_VISITED   - files that had breakpoints
  PERLDB_FILE_...  - breakpoints for a file
  PERLDB_OPT       - active options
  PERLDB_INC       - the original @INC
  PERLDB_PRETYPE   - preprompt debugger actions
  PERLDB_PRE       - preprompt Perl code
  PERLDB_POST      - post-prompt Perl code
  PERLDB_TYPEAHEAD - typeahead captured by readline()

We chug through all these variables and plug the values saved in them
back into the appropriate spots in the debugger.

=cut

if ( exists $ENV{PERLDB_RESTART} ) {

    # We're restarting, so we don't need the flag that says to restart anymore.
    delete $ENV{PERLDB_RESTART};

    # $restart = 1;
    @hist          = get_list('PERLDB_HIST');
    %break_on_load = get_list("PERLDB_ON_LOAD");
    %postponed     = get_list("PERLDB_POSTPONE");

	share(@hist);
	share(@truehist);
	share(%break_on_load);
	share(%postponed);

    # restore breakpoints/actions
    my @had_breakpoints = get_list("PERLDB_VISITED");
    for ( 0 .. $#had_breakpoints ) {
        my %pf = get_list("PERLDB_FILE_$_");
        $postponed_file{ $had_breakpoints[$_] } = \%pf if %pf;
    }

    # restore options
    my %opt = get_list("PERLDB_OPT");
    my ( $opt, $val );
    while ( ( $opt, $val ) = each %opt ) {
        $val =~ s/[\\\']/\\$1/g;
        parse_options("$opt'$val'");
    }

    # restore original @INC
    @INC     = get_list("PERLDB_INC");
    @ini_INC = @INC;

    # return pre/postprompt actions and typeahead buffer
    $pretype   = [ get_list("PERLDB_PRETYPE") ];
    $pre       = [ get_list("PERLDB_PRE") ];
    $post      = [ get_list("PERLDB_POST") ];
    @typeahead = get_list( "PERLDB_TYPEAHEAD", @typeahead );
} ## end if (exists $ENV{PERLDB_RESTART...

=head2 SETTING UP THE TERMINAL

Now, we'll decide how the debugger is going to interact with the user.
If there's no TTY, we set the debugger to run non-stop; there's not going
to be anyone there to enter commands.

=cut

if ($notty) {
    $runnonstop = 1;
	share($runnonstop);
}

=pod

If there is a TTY, we have to determine who it belongs to before we can
proceed. If this is a slave editor or graphical debugger (denoted by
the first command-line switch being '-emacs'), we shift this off and
set C<$rl> to 0 (XXX ostensibly to do straight reads).

=cut

else {

    # Is Perl being run from a slave editor or graphical debugger?
    # If so, don't use readline, and set $slave_editor = 1.
    $slave_editor =
      ( ( defined $main::ARGV[0] ) and ( $main::ARGV[0] eq '-emacs' ) );
    $rl = 0, shift(@main::ARGV) if $slave_editor;

    #require Term::ReadLine;

=pod

We then determine what the console should be on various systems:

=over 4

=item * Cygwin - We use C<stdin> instead of a separate device.

=cut

    if ( $^O eq 'cygwin' ) {

        # /dev/tty is binary. use stdin for textmode
        undef $console;
    }

=item * Unix - use C</dev/tty>.

=cut

    elsif ( -e "/dev/tty" ) {
        $console = "/dev/tty";
    }

=item * Windows or MSDOS - use C<con>.

=cut

    elsif ( $^O eq 'dos' or -e "con" or $^O eq 'MSWin32' ) {
        $console = "con";
    }

=item * MacOS - use C<Dev:Console:Perl Debug> if this is the MPW version; C<Dev:
Console> if not.

Note that Mac OS X returns C<darwin>, not C<MacOS>. Also note that the debugger doesn't do anything special for C<darwin>. Maybe it should.

=cut

    elsif ( $^O eq 'MacOS' ) {
        if ( $MacPerl::Version !~ /MPW/ ) {
            $console =
              "Dev:Console:Perl Debug";    # Separate window for application
        }
        else {
            $console = "Dev:Console";
        }
    } ## end elsif ($^O eq 'MacOS')

=item * VMS - use C<sys$command>.

=cut

    else {

        # everything else is ...
        $console = "sys\$command";
    }

=pod

=back

Several other systems don't use a specific console. We C<undef $console>
for those (Windows using a slave editor/graphical debugger, NetWare, OS/2
with a slave editor, Epoc).

=cut

    if ( ( $^O eq 'MSWin32' ) and ( $slave_editor or defined $ENV{EMACS} ) ) {

        # /dev/tty is binary. use stdin for textmode
        $console = undef;
    }

    if ( $^O eq 'NetWare' ) {

        # /dev/tty is binary. use stdin for textmode
        $console = undef;
    }

    # In OS/2, we need to use STDIN to get textmode too, even though
    # it pretty much looks like Unix otherwise.
    if ( defined $ENV{OS2_SHELL} and ( $slave_editor or $ENV{WINDOWID} ) )
    {    # In OS/2
        $console = undef;
    }

    # EPOC also falls into the 'got to use STDIN' camp.
    if ( $^O eq 'epoc' ) {
        $console = undef;
    }

=pod

If there is a TTY hanging around from a parent, we use that as the console.

=cut

    $console = $tty if defined $tty;

=head2 SOCKET HANDLING   

The debugger is capable of opening a socket and carrying out a debugging
session over the socket.

If C<RemotePort> was defined in the options, the debugger assumes that it
should try to start a debugging session on that port. It builds the socket
and then tries to connect the input and output filehandles to it.

=cut

    # Handle socket stuff.

    if ( defined $remoteport ) {

        # If RemotePort was defined in the options, connect input and output
        # to the socket.
        require IO::Socket;
        $OUT = new IO::Socket::INET(
            Timeout  => '10',
            PeerAddr => $remoteport,
            Proto    => 'tcp',
        );
        if ( !$OUT ) { die "Unable to connect to remote host: $remoteport\n"; }
        $IN = $OUT;
    } ## end if (defined $remoteport)

=pod

If no C<RemotePort> was defined, and we want to create a TTY on startup,
this is probably a situation where multiple debuggers are running (for example,
a backticked command that starts up another debugger). We create a new IN and
OUT filehandle, and do the necessary mojo to create a new TTY if we know how
and if we can.

=cut

    # Non-socket.
    else {

        # Two debuggers running (probably a system or a backtick that invokes
        # the debugger itself under the running one). create a new IN and OUT
        # filehandle, and do the necessary mojo to create a new tty if we
        # know how, and we can.
        create_IN_OUT(4) if $CreateTTY & 4;
        if ($console) {

            # If we have a console, check to see if there are separate ins and
            # outs to open. (They are assumed identiical if not.)

            my ( $i, $o ) = split /,/, $console;
            $o = $i unless defined $o;

            # read/write on in, or just read, or read on STDIN.
            open( IN,      "+<$i" )
              || open( IN, "<$i" )
              || open( IN, "<&STDIN" );

            # read/write/create/clobber out, or write/create/clobber out,
            # or merge with STDERR, or merge with STDOUT.
                 open( OUT, "+>$o" )
              || open( OUT, ">$o" )
              || open( OUT, ">&STDERR" )
              || open( OUT, ">&STDOUT" );    # so we don't dongle stdout

        } ## end if ($console)
        elsif ( not defined $console ) {

            # No console. Open STDIN.
            open( IN, "<&STDIN" );

            # merge with STDERR, or with STDOUT.
            open( OUT,      ">&STDERR" )
              || open( OUT, ">&STDOUT" );    # so we don't dongle stdout
            $console = 'STDIN/OUT';
        } ## end elsif (not defined $console)

        # Keep copies of the filehandles so that when the pager runs, it
        # can close standard input without clobbering ours.
        $IN = \*IN, $OUT = \*OUT if $console or not defined $console;
    } ## end elsif (from if(defined $remoteport))

    # Unbuffer DB::OUT. We need to see responses right away.
    my $previous = select($OUT);
    $| = 1;                                  # for DB::OUT
    select($previous);

    # Line info goes to debugger output unless pointed elsewhere.
    # Pointing elsewhere makes it possible for slave editors to
    # keep track of file and position. We have both a filehandle
    # and a I/O description to keep track of.
    $LINEINFO = $OUT     unless defined $LINEINFO;
    $lineinfo = $console unless defined $lineinfo;
	# share($LINEINFO); # <- unable to share globs
	share($lineinfo);   # 

=pod

To finish initialization, we show the debugger greeting,
and then call the C<afterinit()> subroutine if there is one.

=cut

    # Show the debugger greeting.
    $header =~ s/.Header: ([^,]+),v(\s+\S+\s+\S+).*$/$1$2/;
    unless ($runnonstop) {
        local $\ = '';
        local $, = '';
        if ( $term_pid eq '-1' ) {
            print $OUT "\nDaughter DB session started...\n";
        }
        else {
            print $OUT "\nLoading DB routines from $header\n";
            print $OUT (
                "Editor support ",
                $slave_editor ? "enabled" : "available", ".\n"
            );
            print $OUT
"\nEnter h or `h h' for help, or `$doccmd perldebug' for more help.\n\n";
        } ## end else [ if ($term_pid eq '-1')
    } ## end unless ($runnonstop)
} ## end else [ if ($notty)

# XXX This looks like a bug to me.
# Why copy to @ARGS and then futz with @args?
@ARGS = @ARGV;
for (@args) {
    # Make sure backslashes before single quotes are stripped out, and
    # keep args unless they are numeric (XXX why?)
    # s/\'/\\\'/g;                      # removed while not justified understandably
    # s/(.*)/'$1'/ unless /^-?[\d.]+$/; # ditto
}

# If there was an afterinit() sub defined, call it. It will get
# executed in our scope, so it can fiddle with debugger globals.
if ( defined &afterinit ) {    # May be defined in $rcfile
    &afterinit();
}

# Inform us about "Stack dump during die enabled ..." in dieLevel().
$I_m_init = 1;


