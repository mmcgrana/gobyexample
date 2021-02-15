
% this is a single-line comment

/*
this is
a block
comment
*/


:- encoding(some_encoding).
:- op(Precedence, Associativity, Operator).


:- object(prototype,
	implements(protocol),
	imports(category),
	extends(parent)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2008/5/1,
		comment is 'Sample prototype for testing syntax coloring.']).
	:- threaded.
	:- synchronized.
	:- dynamic.
	:- initialization(some_goal(X, Y)).
	:- calls(some_other_protocol).
	:- uses(another_object).

	:- alias(set, member/2, set_member/2).
	:- alias(words, singular//0, peculiar//0).

	:- uses(list, [append/3, member/2]).
	:- uses(queues, [new/1::new_queue/1]).

	:- public(aaa/2).
	:- meta_predicate(aaa(::, *)).
	:- discontiguous(aaa/2).
	:- mode(aaa(+callable, ?integer), zero_or_one).
	:- info(position/2, [
		comment is 'Predicate brief description.',
		arguments is ['Arg1'-'Arg1 description', 'Arg2'-'Arg2 description']]).

	:- protected(bbb/2).
	:- synchronized(bbb/2).
	:- mode(bbb(+integer, -float), one).
	:- info(bbb/2, [
		comment is 'Predicate brief description.',
		argnames is ['Arg1', 'Arg2']]).

	:- private(ccc/2).
	:- dynamic(ccc/2).
	:- mode(ccc(@atom, ?atom), one_or_more).
	:- info(ccc/2, [
		comment is 'Predicate brief description.',
		argnames is ['Arg1', 'Arg2']]).

	enumerating_entities(Object, Protocol, Category) :-
		current_category(Category),
		current_object(Object),
		current_protocol(Protocol).

	enumerating_properties :-
		category_property(Category, Property),
		object_property(Object, Property),
		protocol_property(Protocol, Property).

	creating_entities(Object, Protocol, Category) :-
		create_category(Category, Relations, Directives, Clauses),
		create_object(Object, Relations, Directives, Clauses),
		create_protocol(Protocol, Relations, Directives).

	abolishing_entities(Object, Protocol, Category) :-
		abolish_category(Category),
		abolish_object(Object),
		abolish_protocol(Protocol).

	entity_relations :-
		extends_object(Prototype, Parent, Scope),
		extends_protocol(Protocol1, Protocol2, Scope),
		extends_category(Category1, Category2, Scope),
		implements_protocol(Object, Protocol, Scope),
		imports_category(Object, Category, Scope),
		instantiates_class(Instance, Class, Scope),
		specializes_class(Class, Superclass, Scope),
		complements_object(Category, Object).

	event_handling :-
		abolish_events(Event, Object, Message, Sender, Monitor),
		current_event(Event, Object, Message, Sender, Monitor),
		define_events(Event, Object, Message, Sender, Monitor).

	multi_threading :-
		threaded(Goals),
		threaded_call(Goal),
		threaded_once(Goal),
		threaded_ignore(Goal),
		threaded_exit(Goal),
		threaded_peek(Goal),
		threaded_wait(Goal),
		threaded_notify(Notification).

	compiling_and_loading :-
		logtalk_compile(File, Options),
		logtalk_load(File, Options),
		logtalk_library_path(Library, Path).

	flags :-
		current_logtalk_flag(Flag, Value),
		set_logtalk_flag(Flag, Value).

	execution_context_methods :-
		parameter(N, Parameter),
		self(Self),
		sender(Sender),
		this(This).

	reflection_methods :-
		current_predicate(Predicate),
		predicate_property(Predicate, Property).

	database_methods :-
		abolish(Functor/Arity),
		asserta(Clause),
		assertz(Clause),
		clause(Head, Body),
		retract(Clause),
		retractall(Head).

	meta_call_methods :-
		call(Goal).

	all_solutions_methods :-
		bagof(Term, Goal, List),
		findall(Term, Goal, List),
		forall(Generate, Test),
		setof(Term, Goal, List).

	event_handler_methods :-
		before(Object, Message, Sender),
		after(Object, Message, Sender).

	dcg_rules_parsing_methods :-
		phrase(NonTerminal, Input, Rest).

	term_expansion_methods :-
		expand_term(Term, Expanded),
		term_expansion(Term, Expanded),
		goal_expansion(Goal, Expanded).

	message_sending :-
		Object::Message,
		::Message,
		^^Message.

	calling_external_code :-
		{goal1, goal2, goal3}.

	context_switching_calls :-
		Object<<Goal.

	direct_calls_of_category_predicates :-
		:Goal.

	if_then_else :-
		(	If ->
			Then
		;	Else
		).

	numbers :-
		X is 13,
		Y is 13.13,
		Z is 13.13e-23,
		C1 is 0'A, C2 is 0'', C3 is 0'",
		B is 0b1011101,
		O is 0o1234560,
		H is 0x1234567890abcDEF.

	functions :-
		A is atan(3.14) + sin(0.77) - cos(123.23),
		B is sign(-12) * abs(35/78),
		C is truncate(3.14) + round(-7.8) - ceiling(111.88),
		D is exp(3.8) - log(123.98) / sqrt(33) * 23 ** 4,
		E is rem(3, 2) + mod(5, 3) * 2 rem 2 // 5 mod 3,
		F is float_fractional_part(3.14) + float_integer_part(3.14),
		G is float(33) + floor(99.99).

	bitwise :-
		A is 16 >> 2,
		B is 16 << 2,
		C is 10 /\ 12,
		D is 10 \/ 12,
		E is \ 10.

	term_unification :-
		Term1 = Term2,
		Term1 \= Term2,
		unify_with_occurs_check(Term1, Term2).

	term_testing :-
		atom(Atom),
		atomic(Atomic),
		integer(Integer),
		float(Float),
		compound(Term),
		nonvar(Term),
		var(Term),
		number(Number).

	term_comparison :-
		Term1 == Term2,
		Term1 \== Term2,
		Term1 @< Term2,
		Term1 @=< Term2,
		Term1 @>= Term2,
		Term1 @> Term2.

	term_creation_and_decomposition :-
		functor(Term, Functor, Arity),
		arg(N, Term, Arg),
		Term =.. [Functor| Args],
		copy_term(Term, Copy).

	arithemtic_evaluation :-
		X is Expression.

	arithemtic_comparison :-
		Exp1 =:= Exp2,
		Exp1 =\= Exp2,
		Exp1 < Exp2,
		Exp1 =< Exp2,
		Exp1 > Exp2,
		Exp1 >= Exp2.

	stream_selection_and_control :-
		current_input(Stream),
		current_output(Stream),
		set_input(Stream),
		set_output(Stream),
		open(Source, Mode, Stream, Options),
		close(Stream),
		flush_output(Stream),
		stream_property(Stream, Property),
		at_end_of_stream(Stream),
		set_stream_position(Stream, Position),
		flush_output,
		at_end_of_stream.

	character_input_output :-
		get_char(Char),
		get_code(Code),
		peek_char(Char),
		peek_code(Code),
		put_char(Char),
		put_code(Code),
		nl(Stream),
		nl.

	byte_input_output :-
		get_byte(Byte),
		peek_byte(Byte),
		put_byte(Byte).

	term_input_output :-
		read(Term),
		read_term(Term),
		write(Term),
		write(Term),
		write_canonical(Term),
		write_term(Stream, Term, Options),
		current_op(Precedence, Associativity, Operator),
		op(Precedence, Associativity, Operator),
		current_char_conversion(InChar, OutChar),
		char_conversion(InChar, OutChar).

	logic_and_control :-
		\+ Goal,
		once(Goal),
		repeat,
		!.

	atomic_term_processing :-
		atom_length(Atom, Length),
		atom_chars(Atom, Chars),
		atom_codes(Atom, Codes),
		atom_concat(Atom1, Atom2, Atom),
		sub_atom(Atom, Before, Length, After, SubAtom),
		char_code(Char, Code),
		number_chars(Number, Chars),
		number_codes(Number, Codes).

	implementation_defined_hooks :-
		current_prolog_flag(Flag, Value),
		set_prolog_flag(Flag, Value),
		halt(ExitCode),
		halt.

	number(C) --> "+", number(C).
	number(C) --> "-", number(X), {C is -X}.
	number(X) --> [C], {0'0 =< C, C =< 0'9, X is C - 0'0}.

:- end_object.



:- object(class,
	implements(protocol),
	imports(category),
	instantiates(metaclass),
	specializes(superclass)).


:- end_object.



:- object(parametric(_Par1, _Par2),
	implements(protocol),
	imports(category),
	extends(parent(_Par))).


:- end_object.



:- category(category,
	implements(protocol),
	extends(other_category)).


:- end_category.



:- protocol(extended,
	extends(minimal)).


:- end_protocol.
