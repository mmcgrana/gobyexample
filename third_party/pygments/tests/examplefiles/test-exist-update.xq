xquery version "3.0";

declare function local:add-log-message($message as xs:string) as empty-sequence()?
{
	let $logfile-collection := "/db/apps/exist101/log"
	let $logfile-name := "exist101-log.xml"
	let $logfile-full := concat($logfile-collection, '/', $logfile-name)
	let $logfile-created :=
	if(doc-available($logfile-full))then
		$logfile-full
	else
		xmldb:store($logfile-collection, $logfile-name, <eXist101-Log/>)
	return
		update insert
			<LogEntry timestamp="{current-dateTime()}">{$message}</LogEntry>
		into doc($logfile-full)/*
};

declare function local:insert-attributes() {
	let $elm as element() := doc('/db/Path/To/Some/Document.xml')/*
	return (
		update insert <NEW/> into $elm,
		update insert attribute x { 'y' } into $elm/*[last()],
		update insert attribute a { 'b' } into $elm/*[last()]
	)
};

declare function local:insert-elem() {
	let $elm as element() := doc('/db/Path/To/Some/Document.xml')/*
	return
		update insert <NEW x="y" a="b"/> into $elm
};

declare function local:insert-elem2() {
	let $elm as element() := doc('/db/Path/To/Some/Document.xml')/*
	let $new-element as element() := <NEW x="y" a="b"/>
	return
		update insert $new-element into $elm	
};

declare function local:insert-single() {
	update insert <LogEntry>Something happened...</LogEntry> into doc('/db/logs/mainlog.xml')/*
};


declare function local:trim-insert() {
	let $document := doc('/db/logs/mainlog.xml')
	let $newentry := <LogEntry>Something happened...</LogEntry>
	return
		update delete $document/*/LogEntry[position() ge 10],
		if(exists($document/*/LogEntry[1]))then
			update insert $newentry preceding $document/*/LogEntry[1]
		else
			update insert $newentry into $document/*
};


declare function local:attempt-document-node-insert() {
	
	(: This is invalid: :)
	let $document as document-node() := <Root><a/></Root>
	return
		update insert <b/> into $document/*
};

declare function local:attempt-attr-update-with-node() {
	update replace doc('/db/test/test.xml')/*/@name with
		<a>aaa<b>bbb</b></a>
};


(# exist:batch-transaction #) {
	update delete $document/*/LogEntry[position() ge 10],
	update insert $newentry preceding $document/*/LogEntry[1]
}