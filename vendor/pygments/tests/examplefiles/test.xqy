(: made up functions, etc just to test xquery parsing (: even embedded comments
on multiple :)
lines
:)
xquery version "1.0";

module namespace xqueryexample "http://example.com/namespace";
import module namespace importedns = "http://example.com/ns/imported" at "no/such/file.xqy";

declare namespace sess = "com.example.session";

declare variable $amazing := "awesome";
declare variable $SESSIONS as element(sess:session)* := c:sessions();

declare option sess:clear "false";

define function whatsit($param as xs:string) as xs:string {
	let $var1 := 1
	let $var2 := 2
	return (1 + 2 div ($var1 + $var2))

	let $let := <x>"test"</x>
	return (: some whitespace :) element element {
			attribute attribute { 1 },
				element test { 'a' },
					attribute foo { "bar" },
						fn:doc()[ foo/@bar eq $let ],
                        //x/with/another/*/*:version/xpath/@attr }
};

let $bride := "Bride"
let $test := validate lax { <some>html</some> }
let $test := validate strict { <some>html</some> }
let $test := validate { <some>html</some> }
let $test := $var1/*:Article       (: comment here :) [fn:not()]
let $test := $var1/@*:name/fn:string()

let $noop := ordered { $test }
let $noop := unordered { $test }

let $noop :=
	for $version at $i in $versions/version
		let $row := if($i mod 2 eq 0) then "even" else "odd"
		order by $version descending
		return

return
<html xmlns="http://www.w3.org/1999/xhtml">
{
	<outer>
	<movie>
		<title>The Princess { fn:capitalize($bride) }</title>
	</movie>
	<form action="" method="post" id="session-form" call="callsomething()">
      <input type="hidden" name="{$d:DEBUG-FIELD}" value="{$d:DEBUG}"/>
      {
				(: placeholder for local sessions :)
				element div {
					attribute id { "sessions-local" },
					attribute class { "hidden" },
					element h1 { "Local Sessions" },
					element p {
						'These sessions use storage provided by your browser.',
						'You can also ',
						element a {
							attribute href { 'session-import-local.xqy' },
							'import' },
						' sessions from local XML files.'
					}
				}
			}
			{
				for $i in $sessions
				let $id := c:session-id($i)
				let $uri := c:session-uri($i)
				(: we only care about the lock that expires last :)
				let $conflicting := c:conflicting-locks($uri, 1)
				let $name as xs:string := ($i/sess:name, "(unnamed)")[1]
				return element tr {
					element td { $name },
					element td { string($i/sec:user) },
					element td { data($i/sess:created) },
					element td { data($i/sess:last-modified) },
					element td {
						if (empty($conflicting)) then () else
						text {
							"by", $conflicting/lock:owner,
							"until", adjust-dateTime-to-timezone(
								x:epoch-seconds-to-dateTime(
									$conflicting/lock:timestamp + $conflicting/lock:timeout
								)
							)
						},
						(: only show resume button if there are no conflicting locks :)
						element input {
							attribute type { "button" },
							attribute title {
								data($i/sess:query-buffers/sess:query[1]) },
							attribute onclick {
								concat("list.resumeSession('", $id, "')") },
							attribute value {
								"Resume", (' ', $id)[ $d:DEBUG ] }
						}[ not($conflicting) ],
						$x:NBSP,
						(: clone button :)
						element input {
							attribute type { "button" },
							attribute title { "clone this session" },
							attribute onclick {
								concat("list.cloneSession('", $id, "', this)") },
							attribute value { "Clone", (' ', $id)[ $d:DEBUG ] }
						},
						$x:NBSP,
						(: export button :)
						element input {
							attribute type { "button" },
							attribute title { "export this session" },
							attribute onclick {
								concat("list.exportServerSession('", $id, "', this)") },
							attribute value { "Export", (' ', $id)[ $d:DEBUG ] }
						},
						$x:NBSP,
						(: only show delete button if there are no conflicting locks :)
						element input {
							attribute type { "button" },
							attribute title { "permanently delete this session" },
							attribute onclick {
								concat("list.deleteSession('", $id, "', this)") },
							attribute value { "Delete", (' ', $id)[ $d:DEBUG ] }
						}[ not($conflicting) ]
					}
				}
			}
	</form>
	</outer>
}
  <tr><td><!-- some commented things-->&nbsp;</td></tr>
</html>
