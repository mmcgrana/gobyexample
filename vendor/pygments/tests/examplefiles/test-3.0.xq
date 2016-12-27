xquery version "3.0";

declare namespace other = "http://other";

declare variable $local:straight-var1 := 'one';

declare %private variable $local:private-var := 'secret';
declare %public variable $local:public-var := 'not-secret';
declare %other:annotation('param1', "param2") variable $local:some-var := 'anything';

declare variable $local:straight-var2 := 'two';


(: Simple Map Operator example :)
declare function local:word-count($elms as element()*) as xs:integer {
	sum($elms ! count(tokenize(., '\s+')))
};

declare function local:add($a, $b) {
	$a + $b
};

declare function local:dispatch($node as node()) as item()* {
    typeswitch($node)
        case text() return $node
        case comment() return $node
        case element(bill) return local:bill($node)
        case element(btitle) return local:btitle($node)
        case element(section-id) return local:section-id($node)
        case element(bill-text) return local:bill-text($node)
        case element(strike) return local:strike($node)
        default return local:passthru($node)
};

(: `switch` expression example :)
declare function local:noise($animal) {
	let $duck := "Duck",
	$quack := "Quack"
	return
		switch ($animal)
			case "Cow" return "Moo"
			case 'Cat' return 'Meow'
			case $duck return $quack
			default return "What's that odd noise?"
};

(: `group by` expression with binding example :)
declare function local:a-to-z() {
	let $data as element()* := (
		<item>Apples</item>,
		<item>Bananas</item>,
		<item>Apricots</item>,
		<item>Pears</item>,
		<item>Brambles</item>
	) return
		<GroupedItems>{
			for $item in $data
			group by $key := upper-case(substring($item, 1, 1))
			order by $key
			return
				<Group key="{$key}">{$item}</Group>
		}</GroupedItems>
};

(: `group by` expression example :)
declare function local:plays-by-character() {
	let $plays := (
		document {
			<play>
				<title>Hamlet</title>
				<characters>
					<character>Hamlet</character>       
					<character>Claudius</character>
					<character>Polonius</character>
					<character>Rosencrantz</character>
					<character>Guildenstern</character>
					<character>Francisco</character>
					<character>Reynaldo</character>     
				</characters>
			</play>
		},
		document {
			<play>
				<title>Rosenkrantz and Guildenstern are Dead</title>
				<characters>
					<character>Alfred</character>
					<character>Rosencrantz</character>
					<character>Guildenstern</character>
					<character>Hamlet</character>
					<character>Claudius</character>
				</characters>
			</play>
		}
	) return

		for $play in $plays/play
		let $title := $play/title
		for $character in $play/characters/character
		group by $character
		return
			<character name="{$character}">
			{
				$title ! <play>{ . }</play>
     		}
			</character>	
};

declare
	%other:a
	%private
	%other:b('1')
	%other:c("1", "2", "3", "4")
function local:very-annotated() {
	let $thing := "thing"
	return
		$thing
};

declare %public function local:slightly-annotated() {
	let $nothing := ()
	return
		$nothing
};

declare function local:ordered() {
	for $hit in doc("/db/doc-with-indexes.xml")//tei:p[other:query(., $search-expression)]
	let $score as xs:float := other:score($hit)
	order by $score descending
	return (
		<p>Score: {$score}:</p>,
		other:summarize($hit, <config width="40"/>)
	)
};

declare function local:concat-expr($postfix) {

	let $concatenated := other:uri() || "/" || $postfix
	return
		$concatenated
};

declare function local:human-units($bytes) {
	let $unit := if($bytes > math:pow(1024, 3)) then
		(math:pow(1024, 3), "GB")
	else if($bytes > math:pow(1024, 2)) then
		(math:pow(1024, 2), "MB")
	else
		(1024, "KB")
	return
		format-number($bytes div $unit[1], ".00") || " " || $unit[2]
};

declare function local:merge-simple($a as xs:string+, $b as xs:string+) as xs:string+ {
	($a, $b)	
};

(: higher order function example 1 :)
declare function local:apply($func, $value) {
	$func($value)
};

(: higher order function example 2 :)
declare function local:apply-all($func, $list) {
	$list ! $func(.)
};

(: higher order function example 3 :)
declare function local:apply-all-long($func as function(xs:string) as xs:string, $list) {
	$list ! $func(.)
};

(: higher order function example 4 :)
declare function local:merge($func as function(xs:string+, xs:string+) as xs:string+, $a as xs:string+, $b as xs:string+) as xs:string+ {
	$func($a, $b)
};

let $to-upper := upper-case#1
let $to-upper-long as function(xs:string) as xs:string := upper-case#1
return
    <case>
    {
        local:apply-all($to-upper, ("Hello", "world!")) ! <upper>{.}</upper>,
        local:apply-all-long(lower-case#1, ("Hello", "world!")) ! <lower>{.}</lower>
    }
    </case>
