implement Ninewin;
include "sys.m";
	sys: Sys;
include "draw.m";
	draw: Draw;
	Image, Display, Pointer: import draw;
include "arg.m";
include "keyboard.m";
include "tk.m";
include "wmclient.m";
	wmclient: Wmclient;
	Window: import wmclient;
include "sh.m";
	sh: Sh;

# run a p9 graphics program (default rio) under inferno wm,
# making available to it:
# /dev/winname - naming the current inferno window (changing on resize)
# /dev/mouse - pointer file + resize events; write to change position
# /dev/cursor - change appearance of cursor.
# /dev/draw - inferno draw device
# /dev/cons - read keyboard events, write to 9win stdout.

Ninewin: module {
	init: fn(ctxt: ref Draw->Context, argv: list of string);
};
winname: string;

init(ctxt: ref Draw->Context, argv: list of string)
{
	size := Draw->Point(500, 500);
	sys = load Sys Sys->PATH;
	draw = load Draw Draw->PATH;
	wmclient = load Wmclient Wmclient->PATH;
	wmclient->init();
	sh = load Sh Sh->PATH;

	buts := Wmclient->Resize;
	if(ctxt == nil){
		ctxt = wmclient->makedrawcontext();
		buts = Wmclient->Plain;
	}
	arg := load Arg Arg->PATH;
	arg->init(argv);
	arg->setusage("9win [-s] [-x width] [-y height]");
	exportonly := 0;
	while(((opt := arg->opt())) != 0){
		case opt {
		's' =>
			exportonly = 1;
		'x' =>
			size.x = int arg->earg();
		'y' =>
			size.y = int arg->earg();
		* =>
			arg->usage();
		}
	}
	if(size.x < 1 || size.y < 1)
		arg->usage();
	argv = arg->argv();
	if(argv != nil && hd argv == "-s"){
		exportonly = 1;
		argv = tl argv;
	}
	if(argv == nil && !exportonly)
		argv = "rio" :: nil;
	if(argv != nil && exportonly){
		sys->fprint(sys->fildes(2), "9win: no command allowed with -s flag\n");
		raise "fail:usage";
	}
	title := "9win";
	if(!exportonly)
		title += " " + hd argv;
	w := wmclient->window(ctxt, title, buts);
	w.reshape(((0, 0), size));
	w.onscreen(nil);
	if(w.image == nil){
		sys->fprint(sys->fildes(2), "9win: cannot get image to draw on\n");
		raise "fail:no window";
	}

	sys->pctl(Sys->FORKNS|Sys->NEWPGRP, nil);
	ld := "/n/9win";
	if(sys->bind("#s", ld, Sys->MREPL) == -1 &&
			sys->bind("#s", ld = "/n/local", Sys->MREPL) == -1){
		sys->fprint(sys->fildes(2), "9win: cannot bind files: %r\n");
		raise "fail:error";
	}
	w.startinput("kbd" :: "ptr" :: nil);
	spawn ptrproc(rq := chan of Sys->Rread, ptr := chan[10] of ref Pointer, reshape := chan[1] of int);

		
	fwinname := sys->file2chan(ld, "winname");
	fconsctl := sys->file2chan(ld, "consctl");
	fcons := sys->file2chan(ld, "cons");
	fmouse := sys->file2chan(ld, "mouse");
	fcursor := sys->file2chan(ld, "cursor");
	if(!exportonly){
		spawn run(sync := chan of string, w.ctl, ld, argv);
		if((e := <-sync) != nil){
			sys->fprint(sys->fildes(2), "9win: %s", e);
			raise "fail:error";
		}
	}
	spawn serveproc(w, rq, fwinname, fconsctl, fcons, fmouse, fcursor);
	if(!exportonly){
		# handle events synchronously so that we don't get a "killed" message
		# from the shell.
		handleevents(w, ptr, reshape);
	}else{
		spawn handleevents(w, ptr, reshape);
		sys->bind(ld, "/dev", Sys->MBEFORE);
		export(sys->fildes(0), w.ctl);
	}
}

handleevents(w: ref Window, ptr: chan of ref Pointer, reshape: chan of int)
{
	for(;;)alt{
	c := <-w.ctxt.ctl or
	c = <-w.ctl =>
		e := w.wmctl(c);
		if(e != nil)
			sys->fprint(sys->fildes(2), "9win: ctl error: %s\n", e);
		if(e == nil && c != nil && c[0] == '!'){
			alt{
			reshape <-= 1 =>
				;
			* =>
				;
			}
			winname = nil;
		}
	p := <-w.ctxt.ptr =>
		if(w.pointer(*p) == 0){
			# XXX would block here if client isn't reading mouse... but we do want to
			# extert back-pressure, which conflicts.
			alt{
			ptr <-= p =>
				;
			* =>
				; # sys->fprint(sys->fildes(2), "9win: discarding mouse event\n");
			}
		}
	}
}

serveproc(w: ref Window, mouserq: chan of Sys->Rread, fwinname, fconsctl, fcons, fmouse, fcursor: ref Sys->FileIO)
{
	winid := 0;
	krc: list of Sys->Rread;
	ks: string;

	for(;;)alt {
	c := <-w.ctxt.kbd =>
		ks[len ks] = inf2p9key(c);
		if(krc != nil){
			hd krc <-= (array of byte ks, nil);
			ks = nil;
			krc = tl krc;
		}
	(nil, d, nil, wc) := <-fcons.write =>
		if(wc != nil){
			sys->write(sys->fildes(1), d, len d);
			wc <-= (len d, nil);
		}
	(nil, nil, nil, rc) := <-fcons.read =>
		if(rc != nil){
			if(ks != nil){
				rc <-= (array of byte ks, nil);
				ks = nil;
			}else
				krc = rc :: krc;
		}
	(offset, nil, nil, rc) := <-fwinname.read =>
		if(rc != nil){
			if(winname == nil){
				winname = sys->sprint("noborder.9win.%d", winid++);
				if(w.image.name(winname, 1) == -1){
					sys->fprint(sys->fildes(2), "9win: namewin %q failed: %r", winname);
					rc <-= (nil, "namewin failure");
					break;
				}
			}
			d := array of byte winname;
			if(offset < len d)
				d = d[offset:];
			else
				d = nil;
			rc <-= (d, nil);
		}
	(nil, nil, nil, wc) := <-fwinname.write =>
		if(wc != nil)
			wc <-= (-1, "permission denied");
	(nil, nil, nil, rc) := <-fconsctl.read =>
		if(rc != nil)
			rc <-= (nil, "permission denied");
	(nil, d, nil, wc) := <-fconsctl.write =>
		if(wc != nil){
			if(string d != "rawon")
				wc <-= (-1, "cannot change console mode");
			else
				wc <-= (len d, nil);
		}
	(nil, nil, nil, rc) := <-fmouse.read =>
		if(rc != nil)
			mouserq <-= rc;
	(nil, d, nil, wc) := <-fmouse.write =>
		if(wc != nil){
			e := cursorset(w, string d);
			if(e == nil)
				wc <-= (len d, nil);
			else
				wc <-= (-1, e);
		}
	(nil, nil, nil, rc) := <-fcursor.read =>
		if(rc != nil)
			rc <-= (nil, "permission denied");
	(nil, d, nil, wc) := <-fcursor.write =>
		if(wc != nil){
			e := cursorswitch(w, d);
			if(e == nil)
				wc <-= (len d, nil);
			else
				wc <-= (-1, e);
		}
	}
}

ptrproc(rq: chan of Sys->Rread, ptr: chan of ref Pointer, reshape: chan of int)
{
	rl: list of Sys->Rread;
	c := ref Pointer(0, (0, 0), 0);
	for(;;){
		ch: int;
		alt{
		p := <-ptr =>
			ch = 'm';
			c = p;
		<-reshape =>
			ch = 'r';
		rc := <-rq =>
			rl  = rc :: rl;
			continue;
		}
		if(rl == nil)
			rl = <-rq :: rl;
		hd rl <-= (sys->aprint("%c%11d %11d %11d %11d ", ch, c.xy.x, c.xy.y, c.buttons, c.msec), nil);
		rl = tl rl;
	}
}

cursorset(w: ref Window, m: string): string
{
	if(m == nil || m[0] != 'm')
		return "invalid mouse message";
	x := int m[1:];
	for(i := 1; i < len m; i++)
		if(m[i] == ' '){
			while(m[i] == ' ')
				i++;
			break;
		}
	if(i == len m)
		return "invalid mouse message";
	y := int m[i:];
	return w.wmctl(sys->sprint("ptr %d %d", x, y));
}

cursorswitch(w: ref Window, d: array of byte): string
{
	Hex: con "0123456789abcdef";
	if(len d != 2*4+64)
		return w.wmctl("cursor");
	hot := Draw->Point(bglong(d, 0*4), bglong(d, 1*4));
	s := sys->sprint("cursor %d %d 16 32 ", hot.x, hot.y);
	for(i := 2*4; i < len d; i++){
		c := int d[i];
		s[len s] = Hex[c >> 4];
		s[len s] = Hex[c & 16rf];
	}
	return w.wmctl(s);
}

run(sync, ctl: chan of string, ld: string, argv: list of string)
{
	Rcmeta: con "|<>&^*[]?();";
	sys->pctl(Sys->FORKNS, nil);
	if(sys->bind("#₪", "/srv", Sys->MCREATE) == -1){
		sync <-= sys->sprint("cannot bind srv device: %r");
		exit;
	}
	srvname := "/srv/9win."+string sys->pctl(0, nil);	# XXX do better.
	fd := sys->create(srvname, Sys->ORDWR, 8r600);
	if(fd == nil){
		sync <-= sys->sprint("cannot create %s: %r", srvname);
		exit;
	}
	sync <-= nil;
	spawn export(fd, ctl);
	sh->run(nil, "os" ::
		"rc" :: "-c" ::
			"mount "+srvname+" /mnt/term;"+
			"rm "+srvname+";"+
			"bind -b /mnt/term"+ld+" /dev;"+
			"bind /mnt/term/dev/draw /dev/draw ||"+
				"bind -a /mnt/term/dev /dev;"+
			quotedc("cd"::"/mnt/term"+cwd()::nil, Rcmeta)+";"+
			quotedc(argv, Rcmeta)+";"::
			nil
		);
}

export(fd: ref Sys->FD, ctl: chan of string)
{
	sys->export(fd, "/", Sys->EXPWAIT);
	ctl <-= "exit";
}

inf2p9key(c: int): int
{
	KF: import Keyboard;

	P9KF: con	16rF000;
	Spec: con	16rF800;
	Khome: con	P9KF|16r0D;
	Kup: con	P9KF|16r0E;
	Kpgup: con	P9KF|16r0F;
	Kprint: con	P9KF|16r10;
	Kleft: con	P9KF|16r11;
	Kright: con	P9KF|16r12;
	Kdown: con	Spec|16r00;
	Kview: con	Spec|16r00;
	Kpgdown: con	P9KF|16r13;
	Kins: con	P9KF|16r14;
	Kend: con	P9KF|16r18;
	Kalt: con		P9KF|16r15;
	Kshift: con	P9KF|16r16;
	Kctl: con		P9KF|16r17;

	case c {
	Keyboard->LShift =>
		return Kshift;
	Keyboard->LCtrl =>
		return Kctl;
	Keyboard->LAlt =>
		return Kalt;
	Keyboard->Home =>
		return Khome;
	Keyboard->End =>
		return Kend;
	Keyboard->Up =>
		return Kup;
	Keyboard->Down =>
		return Kdown;
	Keyboard->Left =>
		return Kleft;
	Keyboard->Right =>
		return Kright;
	Keyboard->Pgup =>
		return Kpgup;
	Keyboard->Pgdown =>
		return Kpgdown;
	Keyboard->Ins =>
		return Kins;

	# function keys
	KF|1 or
	KF|2 or
	KF|3 or
	KF|4 or
	KF|5 or
	KF|6 or
	KF|7 or
	KF|8 or
	KF|9 or
	KF|10 or
	KF|11 or
	KF|12 =>
		return (c - KF) + P9KF;
	}
	return c;
}

cwd(): string
{
	return sys->fd2path(sys->open(".", Sys->OREAD));
}

# from string.b, waiting for declaration to be uncommented.
quotedc(argv: list of string, cl: string): string
{
	s := "";
	while (argv != nil) {
		arg := hd argv;
		for (i := 0; i < len arg; i++) {
			c := arg[i];
			if (c == ' ' || c == '\t' || c == '\n' || c == '\'' || in(c, cl))
				break;
		}
		if (i < len arg || arg == nil) {
			s += "'" + arg[0:i];
			for (; i < len arg; i++) {
				if (arg[i] == '\'')
					s[len s] = '\'';
				s[len s] = arg[i];
			}
			s[len s] = '\'';
		} else
			s += arg;
		if (tl argv != nil)
			s[len s] = ' ';
		argv = tl argv;
	}
	return s;
}

in(c: int, s: string): int
{
	n := len s;
	if(n == 0)
		return 0;
	ans := 0;
	negate := 0;
	if(s[0] == '^') {
		negate = 1;
		s = s[1:];
		n--;
	}
	for(i := 0; i < n; i++) {
		if(s[i] == '-' && i > 0 && i < n-1)  {
			if(c >= s[i-1] && c <= s[i+1]) {
				ans = 1;
				break;
			}
			i++;
		}
		else
		if(c == s[i]) {
			ans = 1;
			break;
		}
	}
	if(negate)
		ans = !ans;

	# just to showcase labels
skip:
	return ans;
}

bglong(d: array of byte, i: int): int
{
	return int d[i] | (int d[i+1]<<8) | (int d[i+2]<<16) | (int d[i+3]<<24);
}
