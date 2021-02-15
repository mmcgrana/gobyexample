#!/usr/bin/lasso9

/*
	Builtins Generator for Lasso 9

	This is the shell script that was used to extract Lasso 9's built-in keywords
	and generate most of the _lasso_builtins.py file. When run, it creates a file
	containing the types, traits, methods, and members of the currently-installed
	version of Lasso 9.

	A list of tags in Lasso 8 can be generated with this code:

	<?LassoScript
		local('l8tags' = list,
					'l8libs' = array('Cache','ChartFX','Client','Database','File','HTTP',
						'iCal','Lasso','Link','List','PDF','Response','Stock','String',
						'Thread','Valid','WAP','XML'));
		iterate(#l8libs, local('library'));
			local('result' = namespace_load(#library));
		/iterate;
		iterate(tags_list, local('i'));
			#l8tags->insert(string_removeleading(#i, -pattern='_global_'));
		/iterate;
		#l8tags->sort;
		iterate(#l8tags, local('i'));
			string_lowercase(#i)+"<br>";
		/iterate;

*/

output("This output statement is required for a complete list of methods.")
local(f) = file("_lasso_builtins-9.py")
#f->doWithClose => {

#f->openTruncate
#f->writeString('# -*- coding: utf-8 -*-
"""
    pygments.lexers._lasso_builtins
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Built-in Lasso types, traits, methods, and members.

    :copyright: Copyright 2006-'+date->year+' by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

')

// Load and register contents of $LASSO9_MASTER_HOME/LassoModules/
database_initialize

// Load all of the libraries from builtins and lassoserver
// This forces all possible available types and methods to be registered
local(srcs =
	(:
		dir(sys_masterHomePath + '/LassoLibraries/builtins/')->eachFilePath,
		dir(sys_masterHomePath + '/LassoLibraries/lassoserver/')->eachFilePath
	)
)

with topLevelDir in delve(#srcs)
where not #topLevelDir->lastComponent->beginsWith('.')
do protect => {
	handle_error => {
		stdoutnl('Unable to load: ' + #topLevelDir + ' ' + error_msg)
	}
	library_thread_loader->loadLibrary(#topLevelDir)
	stdoutnl('Loaded: ' + #topLevelDir)
}

email_initialize
log_initialize
session_initialize

local(
	typesList = set(),
	traitsList = set(),
	unboundMethodsList = set(),
	memberMethodsList = set()
)

// types
with type in sys_listTypes
where not #type->asString->endsWith('$')		// skip threads
do {
	#typesList->insert(#type)
}

// traits
with trait in sys_listTraits
where not #trait->asString->beginsWith('$')		// skip combined traits
do {
	#traitsList->insert(#trait)
}

// member methods
with type in #typesList
do {
	with method in #type->getType->listMethods
	where #method->typeName == #type	 // skip inherited methods
	let name = #method->methodName
	where not #name->asString->endsWith('=')		// skip setter methods
	where #name->asString->isAlpha(1)		// skip unpublished methods
	do {
		#memberMethodsList->insert(#name)
	}
}
with trait in #traitsList
do {
	with method in #trait->getType->provides
	where #method->typeName == #trait		// skip inherited methods
	let name = #method->methodName
	where not #name->asString->endsWith('=')		// skip setter methods
	where #name->asString->isAlpha(1)		// skip unpublished methods
	do {
		#memberMethodsList->insert(#name)
	}
}

// unbound methods
with method in sys_listUnboundMethods
let name = #method->methodName
where not #name->asString->endsWith('=')		// skip setter methods
where #name->asString->isAlpha(1)		// skip unpublished methods
where #typesList !>> #name
where #traitsList !>> #name
do {
	#unboundMethodsList->insert(#name)
}

// write to file
with i in (:
	pair(#typesList, "BUILTINS = {
    'Types': (
"),
	pair(#traitsList, "    ),
    'Traits': (
"),
	pair(#unboundMethodsList, "    ),
    'Unbound Methods': (
"),
	pair(#memberMethodsList, "    )
}
MEMBERS = {
    'Member Methods': (
")
)
do {
	#f->writeString(#i->second)
	with t in (#i->first)
	let ts = #t->asString
	order by #ts
	do {
		#f->writeString("        '"+#ts->lowercase&asString+"',\n")
	}
}

#f->writeString("    )
}
")

}
