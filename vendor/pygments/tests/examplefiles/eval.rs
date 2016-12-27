// -------------------------------------------------------------------------------------------------
// Rick, a Rust intercal compiler.  Save your souls!
//
// Copyright (c) 2015 Georg Brandl
//
// This program is free software; you can redistribute it and/or modify it under the terms of the
// GNU General Public License as published by the Free Software Foundation; either version 2 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
// even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with this program;
// if not, write to the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
// -------------------------------------------------------------------------------------------------

/// Interprets INTERCAL source.
///
/// The evaluator is used when rick is called with `-i`, or when the compiler generates
/// the output while compiling (in the constant-output case).

use std::fmt::{ Debug, Display };
use std::io::Write;
use std::u16;

use err::{ Res, IE123, IE129, IE252, IE275, IE555, IE633, IE774, IE994 };
use ast::{ self, Program, Stmt, StmtBody, ComeFrom, Expr, Var, VType };
use stdops::{ Bind, Array, write_number, read_number, check_chance, check_ovf, pop_jumps,
              get_random_seed, mingle, select, and_16, and_32, or_16, or_32, xor_16, xor_32 };


/// Represents a value (either 16-bit or 32-bit) at runtime.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Val {
    I16(u16),
    I32(u32),
}

impl Val {
    /// Cast as a 16-bit value; returns an error if 32-bit and too big.
    pub fn as_u16(&self) -> Res<u16> {
        match *self {
            Val::I16(v) => Ok(v),
            Val::I32(v) => {
                if v > (u16::MAX as u32) {
                    return IE275.err();
                }
                Ok(v as u16)
            }
        }
    }

    /// Cast as a 32-bit value; always succeeds.
    pub fn as_u32(&self) -> u32 {
        match *self {
            Val::I16(v) => v as u32,
            Val::I32(v) => v
        }
    }

    /// Cast as an usize value; always succeeds.
    pub fn as_usize(&self) -> usize {
        self.as_u32() as usize
    }

    /// Create from a 32-bit value; will select the smallest possible type.
    pub fn from_u32(v: u32) -> Val {
        if v & 0xFFFF == v {
            Val::I16(v as u16)
        } else {
            Val::I32(v)
        }
    }
}

/// The state of the interpreter's evaluator.
pub struct Eval<'a> {
    /// Program to execute.
    program: &'a Program,
    /// Stream to use for printing output.
    stdout: &'a mut Write,
    /// Whether to print debugging output during execution.
    debug: bool,
    /// Variable bindings for the four types of variables.
    spot: Vec<Bind<u16>>,
    twospot: Vec<Bind<u32>>,
    tail: Vec<Bind<Array<u16>>>,
    hybrid: Vec<Bind<Array<u32>>>,
    /// The infamous NEXT stack, capable of holding 80 elements.
    jumps: Vec<ast::LogLine>,
    /// Abstain counter for each statement.
    abstain: Vec<u32>,
    /// Binary I/O "tape" state.
    last_in: u8,
    last_out: u8,
    /// Random number generator state.
    rand_st: u32,
    /// Counts the number of executed statements.
    stmt_ctr: usize,
}

/// Represents the control flow effect of an executed statement.
enum StmtRes {
    /// normal execution, next statement
    Next,
    /// jump around, from DO ... NEXT
    Jump(usize),
    /// jump back, from RESUME
    Back(usize),
    /// start from the first statement, from TRY AGAIN
    FromTop,
    /// end the program, from GIVE UP
    End,
}

impl<'a> Eval<'a> {
    /// Construct a new evaluator.
    pub fn new(program: &'a Program, stdout: &'a mut Write, debug: bool,
               random: bool) -> Eval<'a> {
        let abs = program.stmts.iter().map(|stmt| stmt.props.disabled as u32).collect();
        let nvars = (program.var_info.0.len(),
                     program.var_info.1.len(),
                     program.var_info.2.len(),
                     program.var_info.3.len());
        Eval {
            program:  program,
            stdout:   stdout,
            debug:    debug,
            spot:     vec![Bind::new(0); nvars.0],
            twospot:  vec![Bind::new(0); nvars.1],
            tail:     vec![Bind::new(Array::empty()); nvars.2],
            hybrid:   vec![Bind::new(Array::empty()); nvars.3],
            jumps:    Vec::with_capacity(80),
            rand_st:  if random { get_random_seed() } else { 0 },
            abstain:  abs,
            last_in:  0,
            last_out: 0,
            stmt_ctr: 0,
        }
    }

    /// Interpret the program.  Returns either the number of executed statements,
    /// or an error (RtError).
    pub fn eval(&mut self) -> Res<usize> {
        let mut pctr = 0;  // index of current statement
        let program = self.program.clone();
        let nstmts = program.stmts.len();
        loop {
            // check for falling off the end
            if pctr >= nstmts {
                // if the last statement was a TRY AGAIN, falling off the end is fine
                if let StmtBody::TryAgain = program.stmts[program.stmts.len() - 1].body {
                    break;
                }
                return IE633.err();
            }
            self.stmt_ctr += 1;
            let stmt = &program.stmts[pctr];
            // execute statement if not abstained
            if self.abstain[pctr] == 0 {
                // check execution chance
                let (passed, rand_st) = check_chance(stmt.props.chance, self.rand_st);
                self.rand_st = rand_st;
                if passed {
                    // try to eval this statement
                    let res = match self.eval_stmt(stmt) {
                        // on error, set the correct line number and bubble up
                        Err(mut err) => {
                            err.set_line(stmt.props.onthewayto);
                            // special treatment for NEXT
                            if let StmtBody::DoNext(n) = stmt.body {
                                if let Some(i) = program.labels.get(&n) {
                                    err.set_line(program.stmts[*i as usize].props.srcline);
                                }
                            }
                            return Err(err);
                        }
                        Ok(res)  => res
                    };
                    // handle control flow effects
                    match res {
                        StmtRes::Next    => { }
                        StmtRes::Jump(n) => {
                            self.jumps.push(pctr as u16);  // push the line with the NEXT
                            pctr = n;
                            continue;  // do not increment or check for COME FROMs
                        }
                        StmtRes::Back(n) => {
                            pctr = n;  // will be incremented below after COME FROM check
                        }
                        StmtRes::FromTop => {
                            pctr = 0;  // start from the beginning, do not push any stack
                            continue;
                        }
                        StmtRes::End     => break,
                    }
                }
            }
            // if we are on the line with the compiler bug, error out
            if pctr == self.program.bugline as usize {
                return IE774.err_with(None, stmt.props.onthewayto);
            }
            // try to determine if we have to go to a COME FROM statement
            // (note: in general, program.stmts[pctr] != stmt)
            //
            // the static COME FROM is always a possibility
            let mut maybe_next = program.stmts[pctr].comefrom;
            // the complicated case: evaluate all computed-come-from expressions
            let my_label = program.stmts[pctr].props.label;
            if program.uses_complex_comefrom && my_label > 0 {
                for (i, stmt) in program.stmts.iter().enumerate() {
                    if let StmtBody::ComeFrom(ComeFrom::Expr(ref e)) = stmt.body {
                        let v = try!(try!(self.eval_expr(e)).as_u16());
                        if v == my_label {
                            // as soon as we have multiple candidates, we can bail out
                            if maybe_next.is_some() {
                                return IE555.err();
                            }
                            maybe_next = Some(i as u16);
                        }
                    }
                }
            }
            // check for COME FROMs from this line
            if let Some(next) = maybe_next {
                let next = next as usize;
                // check for abstained COME FROM
                if self.abstain[next] == 0 {
                    // the COME FROM can also have a % chance
                    let (passed, rand_st) = check_chance(program.stmts[next].props.chance,
                                                         self.rand_st);
                    self.rand_st = rand_st;
                    if passed {
                        pctr = next;
                        continue;
                    }
                }
            }
            // no COME FROM, normal execution
            pctr += 1;
        }
        Ok(self.stmt_ctr)
    }

    /// Interpret a single statement.
    fn eval_stmt(&mut self, stmt: &Stmt) -> Res<StmtRes> {
        if self.debug {
            println!("\nExecuting Stmt #{} (state before following)", self.stmt_ctr);
            self.dump_state();
            println!("{}", stmt);
        }
        match stmt.body {
            StmtBody::Calc(ref var, ref expr) => {
                let val = try!(self.eval_expr(expr));
                try!(self.assign(var, val));
                Ok(StmtRes::Next)
            }
            StmtBody::Dim(ref var, ref exprs) => {
                try!(self.array_dim(var, exprs));
                Ok(StmtRes::Next)
            }
            StmtBody::DoNext(n) => {
                match self.program.labels.get(&n) {
                    // too many jumps on stack already?
                    Some(_) if self.jumps.len() >= 80 => IE123.err(),
                    Some(i)                           => Ok(StmtRes::Jump(*i as usize)),
                    None                              => IE129.err(),
                }
            }
            StmtBody::ComeFrom(_) => {
                // nothing to do here at runtime
                Ok(StmtRes::Next)
            }
            StmtBody::Resume(ref expr) => {
                let n = try!(self.eval_expr(expr)).as_u32();
                // this expect() is safe: if the third arg is true, there will
                // be no Ok(None) returns
                let next = try!(pop_jumps(&mut self.jumps, n, true, 0))
                    .expect("https://xkcd.com/378/ ?!");
                Ok(StmtRes::Back(next as usize))
            }
            StmtBody::Forget(ref expr) => {
                let n = try!(self.eval_expr(expr)).as_u32();
                try!(pop_jumps(&mut self.jumps, n, false, 0));
                Ok(StmtRes::Next)
            }
            StmtBody::Ignore(ref vars) => {
                for var in vars {
                    self.set_rw(var, false);
                }
                Ok(StmtRes::Next)
            }
            StmtBody::Remember(ref vars) => {
                for var in vars {
                    self.set_rw(var, true);
                }
                Ok(StmtRes::Next)
            }
            StmtBody::Stash(ref vars) => {
                for var in vars {
                    self.stash(var);
                }
                Ok(StmtRes::Next)
            }
            StmtBody::Retrieve(ref vars) => {
                for var in vars {
                    try!(self.retrieve(var));
                }
                Ok(StmtRes::Next)
            }
            StmtBody::Abstain(ref expr, ref whats) => {
                let f: Box<Fn(u32) -> u32> = if let Some(ref e) = *expr {
                    let n = try!(self.eval_expr(e)).as_u32();
                    box move |v: u32| v.saturating_add(n)
                } else {
                    box |_| 1
                };
                for what in whats {
                    self.abstain(what, &*f);
                }
                Ok(StmtRes::Next)
            }
            StmtBody::Reinstate(ref whats) => {
                for what in whats {
                    self.abstain(what, &|v: u32| v.saturating_sub(1));
                }
                Ok(StmtRes::Next)
            }
            StmtBody::ReadOut(ref vars) => {
                for var in vars {
                    match *var {
                        // read out whole array
                        Expr::Var(ref var) if var.is_dim() => {
                            try!(self.array_readout(var));
                        }
                        // read out single var or array element
                        Expr::Var(ref var) => {
                            let varval = try!(self.lookup(var));
                            try!(write_number(self.stdout, varval.as_u32(), 0));
                        }
                        // read out constant
                        Expr::Num(_, v) => try!(write_number(self.stdout, v, 0)),
                        // others will not be generated
                        _ => return IE994.err(),
                    };
                }
                Ok(StmtRes::Next)
            }
            StmtBody::WriteIn(ref vars) => {
                for var in vars {
                    if var.is_dim() {
                        // write in whole array
                        try!(self.array_writein(var));
                    } else {
                        // write in single var or array element
                        let n = try!(read_number(0));
                        try!(self.assign(var, Val::from_u32(n)));
                    }
                }
                Ok(StmtRes::Next)
            }
            // this one is only generated by the constant-program optimizer
            StmtBody::Print(ref s) => {
                if let Err(_) = self.stdout.write(&s) {
                    return IE252.err();
                }
                Ok(StmtRes::Next)
            }
            StmtBody::TryAgain => Ok(StmtRes::FromTop),
            StmtBody::GiveUp => Ok(StmtRes::End),
            StmtBody::Error(ref e) => Err((*e).clone()),
        }
    }

    /// Evaluate an expression to a value.
    fn eval_expr(&self, expr: &Expr) -> Res<Val> {
        match *expr {
            Expr::Num(vtype, v) => match vtype {
                VType::I16 => Ok(Val::I16(v as u16)),
                VType::I32 => Ok(Val::I32(v)),
            },
            Expr::Var(ref var) => self.lookup(var),
            Expr::Mingle(ref vx, ref wx) => {
                let v = try!(self.eval_expr(vx)).as_u32();
                let w = try!(self.eval_expr(wx)).as_u32();
                let v = try!(check_ovf(v, 0));
                let w = try!(check_ovf(w, 0));
                Ok(Val::I32(mingle(v, w)))
            }
            Expr::Select(vtype, ref vx, ref wx) => {
                let v = try!(self.eval_expr(vx));
                let w = try!(self.eval_expr(wx));
                if vtype == VType::I16 {
                    Ok(Val::I16(select(v.as_u32(), try!(w.as_u16()) as u32) as u16))
                } else {
                    Ok(Val::I32(select(v.as_u32(), w.as_u32())))
                }
            }
            Expr::And(vtype, ref vx) => {
                let v = try!(self.eval_expr(vx));
                match vtype {
                    VType::I16 => Ok(Val::I16(and_16(try!(v.as_u16()) as u32) as u16)),
                    VType::I32 => Ok(Val::I32(and_32(v.as_u32()))),
                }
            }
            Expr::Or(vtype, ref vx) => {
                let v = try!(self.eval_expr(vx));
                match vtype {
                    VType::I16 => Ok(Val::I16(or_16(try!(v.as_u16()) as u32) as u16)),
                    VType::I32 => Ok(Val::I32(or_32(v.as_u32()))),
                }
            }
            Expr::Xor(vtype, ref vx) => {
                let v = try!(self.eval_expr(vx));
                match vtype {
                    VType::I16 => Ok(Val::I16(xor_16(try!(v.as_u16()) as u32) as u16)),
                    VType::I32 => Ok(Val::I32(xor_32(v.as_u32()))),
                }
            }
            Expr::RsNot(ref vx) => {
                let v = try!(self.eval_expr(vx));
                Ok(Val::I32(!v.as_u32()))
            }
            Expr::RsAnd(ref vx, ref wx) => {
                let v = try!(self.eval_expr(vx));
                let w = try!(self.eval_expr(wx));
                Ok(Val::I32(v.as_u32() & w.as_u32()))
            }
            Expr::RsOr(ref vx, ref wx) => {
                let v = try!(self.eval_expr(vx));
                let w = try!(self.eval_expr(wx));
                Ok(Val::I32(v.as_u32() | w.as_u32()))
            }
            Expr::RsXor(ref vx, ref wx) => {
                let v = try!(self.eval_expr(vx));
                let w = try!(self.eval_expr(wx));
                Ok(Val::I32(v.as_u32() ^ w.as_u32()))
            }
            Expr::RsRshift(ref vx, ref wx) => {
                let v = try!(self.eval_expr(vx));
                let w = try!(self.eval_expr(wx));
                Ok(Val::I32(v.as_u32() >> w.as_u32()))
            }
            Expr::RsLshift(ref vx, ref wx) => {
                let v = try!(self.eval_expr(vx));
                let w = try!(self.eval_expr(wx));
                Ok(Val::I32(v.as_u32() << w.as_u32()))
            }
            // Expr::RsEqual(ref vx, ref wx) => {
            //     let v = try!(self.eval_expr(vx));
            //     let w = try!(self.eval_expr(wx));
            //     Ok(Val::I32((v.as_u32() == w.as_u32()) as u32))
            // }
            Expr::RsNotEqual(ref vx, ref wx) => {
                let v = try!(self.eval_expr(vx));
                let w = try!(self.eval_expr(wx));
                Ok(Val::I32((v.as_u32() != w.as_u32()) as u32))
            }
            Expr::RsPlus(ref vx, ref wx) => {
                let v = try!(self.eval_expr(vx));
                let w = try!(self.eval_expr(wx));
                Ok(Val::I32(v.as_u32() + w.as_u32()))
            }
            Expr::RsMinus(ref vx, ref wx) => {
                let v = try!(self.eval_expr(vx));
                let w = try!(self.eval_expr(wx));
                Ok(Val::I32(v.as_u32() - w.as_u32()))
            }
        }
    }

    #[inline]
    fn eval_subs(&self, subs: &Vec<Expr>) -> Res<Vec<usize>> {
        subs.iter().map(|v| self.eval_expr(v).map(|w| w.as_usize())).collect()
    }

    /// Dimension an array.
    fn array_dim(&mut self, var: &Var, dims: &Vec<Expr>) -> Res<()> {
        let dims = try!(self.eval_subs(dims));
        match *var {
            Var::A16(n, _) => self.tail[n].dimension(dims, 0),
            Var::A32(n, _) => self.hybrid[n].dimension(dims, 0),
            _ => return IE994.err(),
        }
    }

    /// Assign to a variable.
    fn assign(&mut self, var: &Var, val: Val) -> Res<()> {
        match *var {
            Var::I16(n) => Ok(self.spot[n].assign(try!(val.as_u16()))),
            Var::I32(n) => Ok(self.twospot[n].assign(val.as_u32())),
            Var::A16(n, ref subs) => {
                let subs = try!(self.eval_subs(subs));
                self.tail[n].set_md(subs, try!(val.as_u16()), 0)
            }
            Var::A32(n, ref subs) => {
                let subs = try!(self.eval_subs(subs));
                self.hybrid[n].set_md(subs, val.as_u32(), 0)
            }
        }
    }

    /// Look up the value of a variable.
    fn lookup(&self, var: &Var) -> Res<Val> {
        match *var {
            Var::I16(n) => Ok(Val::I16(self.spot[n].val)),
            Var::I32(n) => Ok(Val::I32(self.twospot[n].val)),
            Var::A16(n, ref subs) => {
                let subs = try!(self.eval_subs(subs));
                self.tail[n].get_md(subs, 0).map(Val::I16)
            }
            Var::A32(n, ref subs) => {
                let subs = try!(self.eval_subs(subs));
                self.hybrid[n].get_md(subs, 0).map(Val::I32)
            }
        }
    }

    /// Process a STASH statement.
    fn stash(&mut self, var: &Var) {
        match *var {
            Var::I16(n) => self.spot[n].stash(),
            Var::I32(n) => self.twospot[n].stash(),
            Var::A16(n, _) => self.tail[n].stash(),
            Var::A32(n, _) => self.hybrid[n].stash(),
        }
    }

    /// Process a RETRIEVE statement.
    fn retrieve(&mut self, var: &Var) -> Res<()> {
        match *var {
            Var::I16(n) => self.spot[n].retrieve(0),
            Var::I32(n) => self.twospot[n].retrieve(0),
            Var::A16(n, _) => self.tail[n].retrieve(0),
            Var::A32(n, _) => self.hybrid[n].retrieve(0),
        }
    }

    /// Process an IGNORE or REMEMBER statement.  Cannot fail.
    fn set_rw(&mut self, var: &Var, rw: bool) {
        match *var {
            Var::I16(n) => self.spot[n].rw = rw,
            Var::I32(n) => self.twospot[n].rw = rw,
            Var::A16(n, _) => self.tail[n].rw = rw,
            Var::A32(n, _) => self.hybrid[n].rw = rw,
        }
    }

    /// P()rocess an ABSTAIN or REINSTATE statement.  Cannot fail.
    fn abstain(&mut self, what: &ast::Abstain, f: &Fn(u32) -> u32) {
        if let &ast::Abstain::Label(lbl) = what {
            let idx = self.program.labels[&lbl] as usize;
            if self.program.stmts[idx].body != StmtBody::GiveUp {
                self.abstain[idx] = f(self.abstain[idx]);
            }
        } else {
            for (i, stype) in self.program.stmt_types.iter().enumerate() {
                if stype == what {
                    self.abstain[i] = f(self.abstain[i]);
                }
            }
        }
    }

    /// Array readout helper.
    fn array_readout(&mut self, var: &Var) -> Res<()> {
        let state = &mut self.last_out;
        match *var {
            Var::A16(n, _) => self.tail[n].readout(self.stdout, state, 0),
            Var::A32(n, _) => self.hybrid[n].readout(self.stdout, state, 0),
            _ => return IE994.err(),
        }
    }

    /// Array writein helper.
    fn array_writein(&mut self, var: &Var) -> Res<()> {
        let state = &mut self.last_in;
        match *var {
            Var::A16(n, _) => self.tail[n].writein(state, 0),
            Var::A32(n, _) => self.hybrid[n].writein(state, 0),
            _ => return IE994.err(),
        }
    }

    /// Debug helpers.
    fn dump_state(&self) {
        self.dump_state_one(&self.spot, ".");
        self.dump_state_one(&self.twospot, ":");
        self.dump_state_one(&self.tail, ",");
        self.dump_state_one(&self.hybrid, ";");
        if self.jumps.len() > 0 {
            println!("Next stack: {:?}", self.jumps);
        }
        //println!("Abstained: {:?}", self.abstain);
    }

    fn dump_state_one<T: Debug + Display>(&self, vec: &Vec<Bind<T>>, sigil: &str) {
        if vec.len() > 0 {
            for (i, v) in vec.iter().enumerate() {
                print!("{}{} = {}, ", sigil, i, v);
            }
            println!("");
        }
    }
}
