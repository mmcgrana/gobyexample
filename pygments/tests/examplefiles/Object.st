!ProtoObject subclass: #Object
	instanceVariableNames: ''
	classVariableNames: 'DependentsFields'
	poolDictionaries: ''
	category: 'Kernel-Objects'!

!Object methodsFor: '*39Deprecated' stamp: 'gk 2/24/2004 08:49'!
beep
	"Deprecated."
	
	self deprecated: 'Use Beeper class>>beep instead.'.
	Beeper beep! !

!Object methodsFor: '*39Deprecated' stamp: 'gk 2/24/2004 08:50'!
beepPrimitive
	"Deprecated. Beep in the absence of sound support."
	
	self deprecated: 'Use Beeper class>>beep or Beeper class>>beepPrimitive instead.'.
	Beeper beepPrimitive! !

!Object methodsFor: '*39Deprecated' stamp: 'md 12/12/2003 17:02'!
beep: soundName
	"Make the given sound, unless the making of sound is disabled in Preferences."

	self deprecated: 'Use SampledSound>>playSoundNamed: instead.'.
	Preferences soundsEnabled
		ifTrue: [self playSoundNamed: soundName]
! !

!Object methodsFor: '*39Deprecated' stamp: 'sd 11/19/2004 16:57'!
contentsGetz: x
	self deprecated: 'there is no method named contents in object and in addition only one sender in a method not called'. 
	self contents: x! !

!Object methodsFor: '*39Deprecated' stamp: 'sd 11/13/2003 21:10'!
deprecatedExplanation: aString
     "This method is OBSOLETE.  Use #deprecated: instead."
	self deprecated: 'Use Object>>deprecated: instead of deprecatedExplanation:.'.

	Preferences showDeprecationWarnings ifTrue:
		[Deprecation signal: ('{1} has been deprecated. {2}' translated format: {thisContext sender printString. aString})]! !

!Object methodsFor: '*39Deprecated' stamp: 'sd 11/13/2003 21:11'!
deprecated: aBlock explanation: aString 
	 "This method is OBSOLETE.  Use #deprecated:block: instead."
	self deprecated: 'Use Object>>deprecated:block: instead of deprecated:explanation:.'.

	Preferences showDeprecationWarnings ifTrue:
		[Deprecation
			signal: ('{1} has been deprecated. {2}' translated format: {thisContext sender printString. aString})].
	^ aBlock value.
! !

!Object methodsFor: '*39Deprecated' stamp: 'md 12/12/2003 16:25'!
doIfNotNil: aBlock
	self deprecated: 'use ifNotNilDo:'.
	^ self ifNotNilDo: aBlock
! !

!Object methodsFor: '*39Deprecated' stamp: 'md 11/27/2004 12:20'!
ifKindOf: aClass thenDo: aBlock
	self deprecated: 'Deprecated. Just use #isKindOf:'.
	^ (self isKindOf: aClass) ifTrue: [aBlock value: self]! !

!Object methodsFor: '*39Deprecated' stamp: 'gk 2/23/2004 20:51'!
playSoundNamed: soundName
	"Deprecated.
	Play the sound with the given name."

	self deprecated: 'Use "SoundService default playSoundNamed: aName" instead.'.
	SoundService default playSoundNamed: soundName! !


!Object methodsFor: '*Aida' stamp: 'JM 4/22/2007 15:23'!
aidaCanBeLocked
	"can we get an exclusive lock on that object (not already locked)?"
	^false! !

!Object methodsFor: '*Aida' stamp: 'JM 4/22/2007 15:23'!
aidaDontCache
	"don't cache web content in a browser. Appropriate header is added to http response"
	^false! !

!Object methodsFor: '*Aida' stamp: 'JM 4/22/2007 15:24'!
aidaIsLocked
	"is object locked exclusively?"
	^false! !

!Object methodsFor: '*Aida' stamp: 'JM 4/22/2007 15:24'!
aidaLock
	"get an exclusive lock on that object. Until unlocked, noon else can get that lock. Return false if already locked, true if successfull"
	^false! !

!Object methodsFor: '*Aida' stamp: 'JM 4/22/2007 15:24'!
aidaUnlock
	"release an exclusive lock if any"
	^true! !

!Object methodsFor: '*Aida' stamp: 'JM 4/22/2007 15:24'!
app
	"fastest and most convinient way to find a web app for that object"
	^self webAppFor: self firstSessionFromStack! !

!Object methodsFor: '*Aida' stamp: 'JM 4/22/2007 15:25'!
contentType

	"Janko Mivsek, apr98"
	"return 'text/html' as content type for web pages"

	^'text/html'! !

!Object methodsFor: '*Aida' stamp: 'JM 4/22/2007 15:25'!
deepSearchOfClass: aClassName
	"finf all objects of that class down in object hierarchy"
	| objectDictionary class |
	objectDictionary := IdentityDictionary new.
	self deepCopyNotIn: objectDictionary.
	class := aClassName asSymbol.
	^objectDictionary keys select: [:each | each class name = class].! !

!Object methodsFor: '*Aida' stamp: 'JM 4/22/2007 15:26'!
deepSearchOfObsoleteClasses
	"find all objects of obsolete classes down in object hierarchy"
	| objectDictionary |
	objectDictionary := IdentityDictionary new.
	self deepCopyNotIn: objectDictionary.
	^objectDictionary keys select: [:each | each class isObsolete].! !

!Object methodsFor: '*Aida' stamp: 'JM 4/22/2007 15:26'!
expiresTimestamp
	"until when content of this object wont be changed"
	"used in http response, override if you like to be included"
	^self modifiedTimestamp  "to reload pages immediately"! !

!Object methodsFor: '*Aida' stamp: 'JM 4/22/2007 15:26'!
firstAppFromStack
	"try to find a first sender up in calling stack, who is  WebApplication"
	| context |
	context := thisContext.
	[context notNil] whileTrue: [
		(context receiver isKindOf: WebApplication) ifTrue: [^context receiver].
		context := context sender].
	^self firstSessionFromStack lastApp! !

!Object methodsFor: '*Aida' stamp: 'JM 4/25/2007 21:34'!
firstSessionFromStack
	"try to find a first sender up in calling stack, who is  AIDASite and get session if that call"
	| context |
	context := thisContext.
	[context notNil] whileTrue: [
		(context receiver isKindOf: AIDASite) ifTrue: 	[^(context at: 3) "always?"].
		context := context sender].
	^nil! !

!Object methodsFor: '*Aida' stamp: 'mivsek 1/10/2008 18:14'!
forLanguage: aLanguageCodeSymbol
	"for multilingual support: returns an apropriate instance of itself for that language. 
	Langage is defined by ISO 639 2-letter language code, see 
	http://en.wikipedia.org/wiki/List_of_ISO_639-1_codes"
	^false! !

!Object methodsFor: '*Aida' stamp: 'mivsek 1/10/2008 18:14'!
isMultilingual
	"for multilingual support: override this if your domain object responds 
	to #forLanguage: and returns an apropriate instance of itself for that language"
	^false! !

!Object methodsFor: '*Aida' stamp: 'JM 4/22/2007 15:27'!
isVersionedObject
	^false! !

!Object methodsFor: '*Aida' stamp: 'JM 4/22/2007 15:27'!
isWebApplication
	^false! !

!Object methodsFor: '*Aida' stamp: 'JM 4/22/2007 15:27'!
isWebStyle
	^false! !

!Object methodsFor: '*Aida' stamp: 'JM 4/22/2007 15:27'!
modifiedTimestamp
	"when this object was last modified"
	"used in http response, override if you like to be included"
	^nil! !

!Object methodsFor: '*Aida' stamp: 'JM 4/22/2007 15:28'!
preferedUrl
	"override with a suggestion for url of this method!! If not already used, 
	it will be considered by URLResolver during automatic url generation"
	^nil! !

!Object methodsFor: '*Aida' stamp: 'JM 4/22/2007 15:28'!
printWebAppNotFoundFor: aSession 
	| page |
	page := WebPage new.
	page addText: 'Cannot find aWebApplication for object a', self class name.
	^page! !

!Object methodsFor: '*Aida' stamp: 'JM 4/22/2007 15:28'!
printWebPageFor: aSession 
	"find appropriate web application to represent self as web page"

	| webApp |
	webApp := self webAppFor: aSession.
	^webApp notNil 
		ifTrue: [webApp printWebPage]
		ifFalse: [self printWebAppNotFoundFor: aSession]! !

!Object methodsFor: '*Aida' stamp: 'JM 4/22/2007 15:29'!
sendOver: aStream 
	"from Wiki rendering"
	self printOn: aStream! !

!Object methodsFor: '*Aida' stamp: 'JM 4/22/2007 15:29'!
webAppFor: aSession
	| webApp |
	aSession isNil ifTrue: [^nil].
	webApp := aSession webAppFor: self.
	webApp notNil ifTrue: [^webApp].
	webApp := WebApplication newFor: self on: aSession.
	webApp notNil ifTrue: [aSession addWebApp: webApp for: self].
	^webApp! !


!Object methodsFor: '*DynamicBindings' stamp: 'svp 3/6/2003 16:08'!
binding
	"Answer the DynamicBinding for the receiver (if any)"

	^Bindings bindingFor: self ifNotBound: [nil]! !

!Object methodsFor: '*DynamicBindings' stamp: 'svp 4/29/2003 00:35'!
binding: anObject
	"Set the dynamic binding for the receiver, if anObject is nil, then 
	remove the receiver's dynamic binding (if any)"

	^anObject 
		ifNil: [self removeBinding]
		ifNotNil: [Bindings bind: self to: anObject]! !

!Object methodsFor: '*DynamicBindings' stamp: 'svp 3/6/2003 16:09'!
hasBinding
	"Answer whether or not the receiver has a dynamic binding"

	^Bindings includesKey: self! !

!Object methodsFor: '*DynamicBindings' stamp: 'svp 3/6/2003 16:09'!
removeBinding
	"Remove the dynamic binding associated with the receiver"

	^Bindings removeKey: self ifAbsent: []! !


!Object methodsFor: '*KomHttpServer' stamp: 'SVP 8/23/1999 19:17'!
asHtmlDocumentForRequest: aNetworkRequest

	self error: 
		('The requested object (', 
		self asString, 
		'), could not be converted into HTML for your browser.')! !

!Object methodsFor: '*KomHttpServer' stamp: 'SVP 8/23/1999 19:23'!
asHttpResponseTo: anHttpRequest

	^(self asHtmlDocumentForRequest: anHttpRequest) 
		asHttpResponseTo: anHttpRequest
! !

!Object methodsFor: '*KomHttpServer' stamp: 'svp 5/16/2003 12:47'!
isComancheModule

	^false! !

!Object methodsFor: '*KomHttpServer' stamp: 'SVP 8/17/1999 17:51'!
mimeType

	^MIMEDocument defaultContentType! !


!Object methodsFor: '*Morphic-NewCurve-testing''' stamp: 'wiz 12/31/2005 21:31'!
isNonZero
"Overriden in Number. This returns the backstop answer for non-numbers"
^false.! !


!Object methodsFor: '*Pinesoft-EventEnhancements' stamp: 'gvc 10/25/2006 18:18'!
when: anEventSelector
send: aMessageSelector
to: anObject
exclusive: aValueHolder
 
	self
		when: anEventSelector
		evaluate: ((ExclusiveWeakMessageSend
					receiver: anObject
					selector: aMessageSelector)
						basicExecuting: aValueHolder)! !

!Object methodsFor: '*Pinesoft-EventEnhancements' stamp: 'gvc 10/25/2006 18:17'!
when: anEventSelector
send: aMessageSelector
to: anObject
with: anArg
exclusive: aValueHolder
 
    self
        when: anEventSelector
        evaluate: ((ExclusiveWeakMessageSend
 		receiver: anObject
		selector: aMessageSelector
		arguments: (Array with: anArg))
			basicExecuting: aValueHolder)! !

!Object methodsFor: '*Pinesoft-EventEnhancements' stamp: 'gvc 10/25/2006 18:17'!
when: anEventSelector
send: aMessageSelector
to: anObject
withArguments: anArgArray
exclusive: aValueHolder
 
    self
        when: anEventSelector
        evaluate: ((ExclusiveWeakMessageSend
		receiver: anObject
		selector: aMessageSelector
		arguments: anArgArray)
			basicExecuting: aValueHolder)! !

!Object methodsFor: '*Pinesoft-EventEnhancements' stamp: 'gvc 10/24/2006 11:50'!
when: anEventSelector
sendOnce: aMessageSelector
to: anObject
 
    self
        when: anEventSelector
        evaluate: (NonReentrantWeakMessageSend
            receiver: anObject
            selector: aMessageSelector)! !

!Object methodsFor: '*Pinesoft-EventEnhancements' stamp: 'gvc 10/24/2006 11:49'!
when: anEventSelector
sendOnce: aMessageSelector
to: anObject
with: anArg
 
    self
        when: anEventSelector
        evaluate: (NonReentrantWeakMessageSend
            receiver: anObject
            selector: aMessageSelector
		arguments: (Array with: anArg))! !

!Object methodsFor: '*Pinesoft-EventEnhancements' stamp: 'gvc 10/24/2006 11:49'!
when: anEventSelector
sendOnce: aMessageSelector
to: anObject
withArguments: anArgArray
 
    self
        when: anEventSelector
        evaluate: (NonReentrantWeakMessageSend
            receiver: anObject
            selector: aMessageSelector
		arguments: anArgArray)! !


!Object methodsFor: '*Pinesoft-Widgets' stamp: 'gvc 1/10/2007 11:41'!
okToClose
	"Sent to models when a window closing.
	Allows this check to be independent of okToChange."
	
	^true! !

!Object methodsFor: '*Pinesoft-Widgets' stamp: 'gvc 4/17/2007 17:41'!
taskbarIcon
	"Answer the icon for the receiver in a task bar
	or nil for the default."

	^self class taskbarIcon! !


!Object methodsFor: '*Pinesoft-Widgets-override' stamp: 'gvc 9/4/2007 12:32'!
windowActiveOnFirstClick
	"Return true if my window should be active on first click."

	^true! !


!Object methodsFor: '*SeasideAdaptersCompatibility' stamp: 'pmm 11/25/2007 14:17'!
toString
	^self! !


!Object methodsFor: '*Tools-Explorer' stamp: 'stephaneducasse 9/17/2005 21:52'!
exploreAndYourself
	"i.e. explore; yourself. Thisway i can peek w/o typing all the parentheses"
	self explore. 
     ^self! !

!Object methodsFor: '*Tools-Explorer' stamp: 'stephaneducasse 9/17/2005 21:48'!
exploreWithLabel: label

	^ ObjectExplorer new openExplorerFor: self withLabel:
label! !


!Object methodsFor: '*kernel-extensions-flagging' stamp: 'mtf 1/26/2008 23:34'!
deprecated
	"Warn that the sending method has been deprecated."

	Preferences showDeprecationWarnings ifTrue:
		[Deprecation signal: thisContext sender printString, ' has been deprecated.']! !


!Object methodsFor: '*kernel-extensions-flexibility' stamp: 'kph 1/27/2008 19:21'!
askFor: selector 

    "returns true or false"
	
	^ (self askFor: selector ifAbsent: nil) == true! !

!Object methodsFor: '*kernel-extensions-flexibility' stamp: 'kph 10/17/2007 14:01'!
askFor: selector ifAbsent: aBlock

   "enables a default value to be specified in order to be tolerant of potentially missing methods
	
	e.g.
	(myPoint askFor: #originOffset) ifAbsent: [ 0@0 ].
	"

	^ (self class canUnderstand: selector) ifFalse: [ aBlock value ] ifTrue: [self perform: selector]! !


!Object methodsFor: '*kernel-extensions-logging' stamp: 'mtf 1/26/2008 23:52'!
log
	"This method provides the univeral entry point fo all logging mechanisms"
	
	"Options:
	1. Null for null logging
	2. A LogRouter instance wih a FrameworkAdaptor.
	3. CurrentLog a process local variable supplying a LogRouter"
	
	^ (Smalltalk at: #CurrentLog ifAbsent: [ Null default ]) value
		sender: thisContext sender; beginEntry; yourself! !


!Object methodsFor: '*magritte-model-accessing' stamp: 'lr 3/9/2006 11:31'!
description
	"Return the description of the reciever. Subclasses might override this message to return instance-based descriptions."

	^ self class description! !

!Object methodsFor: '*magritte-model-accessing' stamp: 'lr 3/9/2006 11:31'!
mementoClass
	"Return a class to be used to remember or cache the receiver, namely a memento object."

	^ MACheckedMemento! !


!Object methodsFor: '*magritte-model-model' stamp: 'lr 3/9/2006 11:31'!
readUsing: aDescription
	"Dispatch the read-access to the receiver using the accessor of aDescription."

	^ aDescription accessor read: self! !

!Object methodsFor: '*magritte-model-model' stamp: 'lr 3/9/2006 11:31'!
write: anObject using: aDescription
	"Dispatch the write-access to the receiver of anObject using the accessor of aDescription."

	aDescription accessor write: anObject to: self! !


!Object methodsFor: '*magritte-model-testing' stamp: 'lr 3/9/2006 11:31'!
isDescription
	^ false! !


!Object methodsFor: '*magritte-morph-converting' stamp: 'lr 3/9/2006 11:33'!
asMorph
	^ self description asMorphOn: self! !


!Object methodsFor: '*magritte-seaside-converting' stamp: 'lr 3/9/2006 11:33'!
asComponent
	^ self description asComponentOn: self! !


!Object methodsFor: '*monticello' stamp: 'dvf 8/10/2004 23:25'!
isConflict
	^false! !


!Object methodsFor: '*null' stamp: 'kph 9/6/2007 23:31'!
ifNull: aBlock

	^ self! !

!Object methodsFor: '*null' stamp: 'kph 9/6/2007 23:33'!
isNull

	^ false! !

!Object methodsFor: '*null' stamp: 'kph 4/12/2007 08:27'!
orNull

	^ self! !


!Object methodsFor: '*ob-tools-inspector' stamp: 'lr 6/5/2008 11:07'!
basicInspectorNodes
	<inspector: #'1' priority: 600>
	
	| nodes |
	nodes := OrderedCollection new: self class instSize + self basicSize + 5.
	nodes add: self selfInspectorNode.
	self class allInstVarNames withIndexDo: [ :name :index |
		nodes add: (OTNamedVariableNode on: self index: index name: name) ].
	1 to: self basicSize do: [ :index |
		nodes add: (OTIndexedVariableNode on: self index: index) ].
	^ nodes! !

!Object methodsFor: '*ob-tools-inspector' stamp: 'lr 6/5/2008 11:07'!
protocolInspectorNodes
	<inspector: #'#' priority: 800>

	^ self class allSelectors asArray sort
		collect: [ :each | OTProtocolInspectorNode on: self selector: each ]! !

!Object methodsFor: '*ob-tools-inspector' stamp: 'lr 6/5/2008 09:58'!
selfInspectorNode
	^ OTDerivedInspectorNode on: self label: 'self' block: [ :obj | obj ]! !


!Object methodsFor: '*omnibrowser-converting' stamp: 'cwp 4/17/2006 12:16'!
asAnnouncement
	^ self! !


!Object methodsFor: '*pier-model' stamp: 'lr 3/9/2006 11:29'!
accept: aVisitor
	self subclassResponsibility! !

!Object methodsFor: '*pier-model' stamp: 'lr 3/9/2006 11:29'!
acceptDecorated: aVisitor
	self accept: aVisitor! !


!Object methodsFor: '*rio-kernel' stamp: 'kph 3/8/2007 21:25'!
isRio

	^ false! !


!Object methodsFor: '*scriptaculous' stamp: 'lr 1/4/2007 17:20'!
asFunction
	^ self asFunction: #()! !

!Object methodsFor: '*scriptaculous' stamp: 'lr 1/4/2007 17:21'!
asFunction: aCollection
	^ SUFunction new add: self; arguments: aCollection! !

!Object methodsFor: '*scriptaculous' stamp: 'lr 4/11/2006 19:49'!
asJavascript
	^ String streamContents: [ :stream | self javascriptOn: stream ]! !


!Object methodsFor: '*scriptaculous-printing' stamp: 'lr 4/20/2006 21:10'!
javascriptOn: aStream
	self printOn: aStream! !


!Object methodsFor: '*seaside2' stamp: 'lr 6/5/2007 21:35'!
deprecatedApi
	self deprecatedApi: thisContext sender displayString! !

!Object methodsFor: '*seaside2' stamp: 'lr 6/5/2007 21:35'!
deprecatedApi: aString
	WADeprecatedApi raiseSignal: aString! !

!Object methodsFor: '*seaside2' stamp: 'lr 5/9/2007 08:47'!
inspectorFields
	| members |
	members := Array new writeStream.
	self class allInstVarNames withIndexDo: [ :each :index |
		members nextPut: each -> (self instVarAt: index) ].
	self class isVariable ifTrue: [
		1 to: self size do: [ :index |
			members nextPut: index -> (self at: index) ] ].
	^ members contents! !

!Object methodsFor: '*seaside2' stamp: 'avi 3/14/2005 15:19'!
labelForSelector: aSymbol
	^ aSymbol asCapitalizedPhrase! !

!Object methodsFor: '*seaside2' stamp: 'pmm 4/7/2007 17:14'!
renderOn: aRenderer
	"Override this method to customize how objects (not components) are rendered when passed as an argument to #render:. The default is the return value of #displayString.
	Just remember that you can not use #callback:, #on:of:, or #call:"

	aRenderer text: self! !

!Object methodsFor: '*seaside2' stamp: 'lr 3/19/2007 23:13'!
restoreFromSnapshot: anObject
	self copyFrom: anObject! !

!Object methodsFor: '*seaside2' stamp: 'avi 9/1/2004 21:20'!
snapshotCopy
	^ self shallowCopy! !

!Object methodsFor: '*seaside2' stamp: 'lr 10/28/2007 14:42'!
validationError: message
	^WAValidationNotification raiseSignal: message! !


!Object methodsFor: '*seaside2-encoding' stamp: 'lr 3/26/2007 20:16'!
encodeOn: aDocument
	aDocument print: self displayString! !


!Object methodsFor: '*seaside2-squeak' stamp: 'pmm 5/22/2007 22:10'!
beMutable
	"for VW compatibility, a hack that allows to cache a value in a literal array"! !

!Object methodsFor: '*seaside2-squeak' stamp: 'lr 7/12/2005 17:01'!
displayString
	^ self asString! !


!Object methodsFor: '*services-base' stamp: 'rr 3/21/2006 11:54'!
requestor
	"returns the focused window's requestor"

	"SystemWindow focusedWindow ifNotNilDo: [:w | ^ w requestor]."

	"triggers an infinite loop"

	^ Requestor default! !


!Object methodsFor: '*system-support' stamp: 'dvf 8/23/2003 12:27'!
systemNavigation

	^ SystemNavigation default! !


!Object methodsFor: '*tools-browser' stamp: 'mu 3/6/2004 15:13'!
browse
	self systemNavigation browseClass: self class! !

!Object methodsFor: '*tools-browser' stamp: 'mu 3/11/2004 16:00'!
browseHierarchy
	self systemNavigation browseHierarchy: self class! !


!Object methodsFor: '*universes' stamp: 'ls 11/26/2006 12:33'!
isUPackage
	^false! !

!Object methodsFor: '*universes' stamp: 'ls 11/26/2006 12:33'!
isUPackageCategory
	^false! !


!Object methodsFor: 'accessing' stamp: 'sw 4/30/1998 12:18'!
addInstanceVarNamed: aName withValue: aValue
	"Add an instance variable named aName and give it value aValue"
	self class addInstVarName: aName asString.
	self instVarAt: self class instSize put: aValue! !

!Object methodsFor: 'accessing' stamp: 'yo 6/29/2004 11:39'!
at: index 
	"Primitive. Assumes receiver is indexable. Answer the value of an 
	indexable element in the receiver. Fail if the argument index is not an 
	Integer or is out of bounds. Essential. See Object documentation 
	whatIsAPrimitive."

	<primitive: 60>
	index isInteger ifTrue:
		[self class isVariable
			ifTrue: [self errorSubscriptBounds: index]
			ifFalse: [self errorNotIndexable]].
	index isNumber
		ifTrue: [^self at: index asInteger]
		ifFalse: [self errorNonIntegerIndex]! !

!Object methodsFor: 'accessing'!
at: index modify: aBlock
	"Replace the element of the collection with itself transformed by the block"
	^ self at: index put: (aBlock value: (self at: index))! !

!Object methodsFor: 'accessing' stamp: 'yo 6/29/2004 13:08'!
at: index put: value 
	"Primitive. Assumes receiver is indexable. Store the argument value in 
	the indexable element of the receiver indicated by index. Fail if the 
	index is not an Integer or is out of bounds. Or fail if the value is not of 
	the right type for this kind of collection. Answer the value that was 
	stored. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 61>
	index isInteger ifTrue:
		[self class isVariable
			ifTrue: [(index >= 1 and: [index <= self size])
					ifTrue: [self errorImproperStore]
					ifFalse: [self errorSubscriptBounds: index]]
			ifFalse: [self errorNotIndexable]].
	index isNumber
		ifTrue: [^self at: index asInteger put: value]
		ifFalse: [self errorNonIntegerIndex]! !

!Object methodsFor: 'accessing' stamp: 'yo 9/20/2004 10:22'!
basicAddInstanceVarNamed: aName withValue: aValue
	"Add an instance variable named aName and give it value aValue"
	self class addInstVarName: aName asString.
	self instVarAt: self class instSize put: aValue! !

!Object methodsFor: 'accessing'!
basicAt: index 
	"Primitive. Assumes receiver is indexable. Answer the value of an 
	indexable element in the receiver. Fail if the argument index is not an 
	Integer or is out of bounds. Essential. Do not override in a subclass. See 
	Object documentation whatIsAPrimitive."

	<primitive: 60>
	index isInteger ifTrue: [self errorSubscriptBounds: index].
	index isNumber
		ifTrue: [^self basicAt: index asInteger]
		ifFalse: [self errorNonIntegerIndex]! !

!Object methodsFor: 'accessing'!
basicAt: index put: value 
	"Primitive. Assumes receiver is indexable. Store the second argument 
	value in the indexable element of the receiver indicated by index. Fail 
	if the index is not an Integer or is out of bounds. Or fail if the value is 
	not of the right type for this kind of collection. Answer the value that 
	was stored. Essential. Do not override in a subclass. See Object 
	documentation whatIsAPrimitive."

	<primitive: 61>
	index isInteger
		ifTrue: [(index >= 1 and: [index <= self size])
					ifTrue: [self errorImproperStore]
					ifFalse: [self errorSubscriptBounds: index]].
	index isNumber
		ifTrue: [^self basicAt: index asInteger put: value]
		ifFalse: [self errorNonIntegerIndex]! !

!Object methodsFor: 'accessing'!
basicSize
	"Primitive. Answer the number of indexable variables in the receiver. 
	This value is the same as the largest legal subscript. Essential. Do not 
	override in any subclass. See Object documentation whatIsAPrimitive."

	<primitive: 62>
	"The number of indexable fields of fixed-length objects is 0"
	^0	! !

!Object methodsFor: 'accessing'!
bindWithTemp: aBlock
	^ aBlock value: self value: nil! !

!Object methodsFor: 'accessing' stamp: 'md 10/7/2004 15:43'!
ifNil: nilBlock ifNotNilDo: aBlock 
	"Evaluate aBlock with the receiver as its argument."

	^ aBlock value: self
! !

!Object methodsFor: 'accessing' stamp: 'di 11/8/2000 21:04'!
ifNotNilDo: aBlock
	"Evaluate the given block with the receiver as its argument."

	^ aBlock value: self
! !

!Object methodsFor: 'accessing' stamp: 'md 10/7/2004 15:43'!
ifNotNilDo: aBlock ifNil: nilBlock
	"Evaluate aBlock with the receiver as its argument."

	^ aBlock value: self
! !

!Object methodsFor: 'accessing' stamp: 'ajh 1/21/2003 12:59'!
in: aBlock
	"Evaluate the given block with the receiver as its argument."

	^ aBlock value: self
! !

!Object methodsFor: 'accessing' stamp: 'sw 10/17/2000 11:15'!
presenter
	"Answer the presenter object associated with the receiver.  For morphs, there is in effect a clear containment hierarchy of presenters (accessed via their association with PasteUpMorphs); for arbitrary objects the hook is simply via the current world, at least at present."

	^ self currentWorld presenter! !

!Object methodsFor: 'accessing'!
readFromString: aString
	"Create an object based on the contents of aString."

	^self readFrom: (ReadStream on: aString)! !

!Object methodsFor: 'accessing' stamp: 'di 3/29/1999 13:10'!
size
	"Primitive. Answer the number of indexable variables in the receiver. 
	This value is the same as the largest legal subscript. Essential. See Object 
	documentation whatIsAPrimitive."

	<primitive: 62>
	self class isVariable ifFalse: [self errorNotIndexable].
	^ 0! !

!Object methodsFor: 'accessing' stamp: 'md 5/16/2006 12:34'!
yourself
	"Answer self."
	^self! !


!Object methodsFor: 'associating' stamp: 'md 7/22/2005 16:03'!
-> anObject
	"Answer an Association between self and anObject"

	^Association basicNew key: self value: anObject! !


!Object methodsFor: 'binding'!
bindingOf: aString
	^nil! !


!Object methodsFor: 'breakpoint' stamp: 'bkv 7/1/2003 12:33'!
break
	"This is a simple message to use for inserting breakpoints during debugging.
	The debugger is opened by sending a signal. This gives a chance to restore
	invariants related to multiple processes."

	BreakPoint signal.

	"nil break."! !


!Object methodsFor: 'casing'!
caseOf: aBlockAssociationCollection
	"The elements of aBlockAssociationCollection are associations between blocks.
	 Answer the evaluated value of the first association in aBlockAssociationCollection
	 whose evaluated key equals the receiver.  If no match is found, report an error."

	^ self caseOf: aBlockAssociationCollection otherwise: [self caseError]

"| z | z _ {[#a]->[1+1]. ['b' asSymbol]->[2+2]. [#c]->[3+3]}. #b caseOf: z"
"| z | z _ {[#a]->[1+1]. ['d' asSymbol]->[2+2]. [#c]->[3+3]}. #b caseOf: z"
"The following are compiled in-line:"
"#b caseOf: {[#a]->[1+1]. ['b' asSymbol]->[2+2]. [#c]->[3+3]}"
"#b caseOf: {[#a]->[1+1]. ['d' asSymbol]->[2+2]. [#c]->[3+3]}"! !

!Object methodsFor: 'casing'!
caseOf: aBlockAssociationCollection otherwise: aBlock
	"The elements of aBlockAssociationCollection are associations between blocks.
	 Answer the evaluated value of the first association in aBlockAssociationCollection
	 whose evaluated key equals the receiver.  If no match is found, answer the result
	 of evaluating aBlock."

	aBlockAssociationCollection associationsDo:
		[:assoc | (assoc key value = self) ifTrue: [^assoc value value]].
	^ aBlock value

"| z | z _ {[#a]->[1+1]. ['b' asSymbol]->[2+2]. [#c]->[3+3]}. #b caseOf: z otherwise: [0]"
"| z | z _ {[#a]->[1+1]. ['d' asSymbol]->[2+2]. [#c]->[3+3]}. #b caseOf: z otherwise: [0]"
"The following are compiled in-line:"
"#b caseOf: {[#a]->[1+1]. ['b' asSymbol]->[2+2]. [#c]->[3+3]} otherwise: [0]"
"#b caseOf: {[#a]->[1+1]. ['d' asSymbol]->[2+2]. [#c]->[3+3]} otherwise: [0]"! !


!Object methodsFor: 'class membership'!
class
	"Primitive. Answer the object which is the receiver's class. Essential. See 
	Object documentation whatIsAPrimitive."

	<primitive: 111>
	self primitiveFailed! !

!Object methodsFor: 'class membership' stamp: 'sw 9/27/2001 15:51'!
inheritsFromAnyIn: aList
	"Answer whether the receiver inherits from any class represented by any element in the list.  The elements of the list can be classes, class name symbols, or strings representing possible class names.  This allows speculative membership tests to be made even when some of the classes may not be known to the current image, and even when their names are not interned symbols."

	| aClass |
	aList do:
		[:elem | Symbol hasInterned: elem asString ifTrue: 
			[:elemSymbol | (((aClass _ Smalltalk at: elemSymbol ifAbsent: [nil]) isKindOf: Class)
						and: [self isKindOf: aClass])
				ifTrue:
					[^ true]]].
	^ false


"
{3.  true. 'olive'} do:
	[:token |
		 {{#Number. #Boolean}. {Number.  Boolean }.  {'Number'. 'Boolean'}} do:
			[:list |
				Transcript cr; show: token asString, ' list element provided as a ', list first class name, ' - ', (token inheritsFromAnyIn: list) asString]]
"! !

!Object methodsFor: 'class membership'!
isKindOf: aClass 
	"Answer whether the class, aClass, is a superclass or class of the receiver."

	self class == aClass
		ifTrue: [^true]
		ifFalse: [^self class inheritsFrom: aClass]! !

!Object methodsFor: 'class membership' stamp: 'sw 2/16/98 02:08'!
isKindOf: aClass orOf: anotherClass
	"Answer whether either of the classes, aClass or anotherClass,, is a superclass or class of the receiver.  A convenience; could be somewhat optimized"
	^ (self isKindOf: aClass) or: [self isKindOf: anotherClass]! !

!Object methodsFor: 'class membership'!
isMemberOf: aClass 
	"Answer whether the receiver is an instance of the class, aClass."

	^self class == aClass! !

!Object methodsFor: 'class membership'!
respondsTo: aSymbol 
	"Answer whether the method dictionary of the receiver's class contains 
	aSymbol as a message selector."

	^self class canUnderstand: aSymbol! !

!Object methodsFor: 'class membership' stamp: 'tk 10/21/1998 12:38'!
xxxClass
	"For subclasses of nil, such as ObjectOut"
	^ self class! !


!Object methodsFor: 'comparing' stamp: 'tk 4/16/1999 18:26'!
closeTo: anObject
	"Answer whether the receiver and the argument represent the same
	object. If = is redefined in any subclass, consider also redefining the
	message hash."

	| ans |
	[ans _ self = anObject] ifError: [:aString :aReceiver | ^ false].
	^ ans! !

!Object methodsFor: 'comparing'!
hash
	"Answer a SmallInteger whose value is related to the receiver's identity.
	May be overridden, and should be overridden in any classes that define = "

	^ self identityHash! !

!Object methodsFor: 'comparing' stamp: 'pm 9/23/97 09:36'!
hashMappedBy: map
	"Answer what my hash would be if oops changed according to map."

	^map newHashFor: self! !

!Object methodsFor: 'comparing' stamp: 'di 9/27/97 20:23'!
identityHashMappedBy: map
	"Answer what my hash would be if oops changed according to map."

	^map newHashFor: self! !

!Object methodsFor: 'comparing' stamp: 'sw 8/20/1998 12:34'!
identityHashPrintString
	"'fred' identityHashPrintString"

	^ '(', self identityHash printString, ')'! !

!Object methodsFor: 'comparing' stamp: 'ajh 2/2/2002 15:02'!
literalEqual: other

	^ self class == other class and: [self = other]! !

!Object methodsFor: 'comparing'!
= anObject 
	"Answer whether the receiver and the argument represent the same 
	object. If = is redefined in any subclass, consider also redefining the 
	message hash."

	^self == anObject! !

!Object methodsFor: 'comparing'!
~= anObject 
	"Answer whether the receiver and the argument do not represent the 
	same object."

	^self = anObject == false! !


!Object methodsFor: 'converting' stamp: 'di 11/9/1998 12:15'!
adaptToFloat: rcvr andSend: selector
	"If no method has been provided for adapting an object to a Float,
	then it may be adequate to simply adapt it to a number."
	^ self adaptToNumber: rcvr andSend: selector! !

!Object methodsFor: 'converting' stamp: 'di 11/9/1998 12:14'!
adaptToFraction: rcvr andSend: selector
	"If no method has been provided for adapting an object to a Fraction,
	then it may be adequate to simply adapt it to a number."
	^ self adaptToNumber: rcvr andSend: selector! !

!Object methodsFor: 'converting' stamp: 'di 11/9/1998 12:15'!
adaptToInteger: rcvr andSend: selector
	"If no method has been provided for adapting an object to a Integer,
	then it may be adequate to simply adapt it to a number."
	^ self adaptToNumber: rcvr andSend: selector! !

!Object methodsFor: 'converting' stamp: 'rw 4/27/2002 07:48'!
asActionSequence

	^WeakActionSequence with: self! !

!Object methodsFor: 'converting' stamp: 'rw 7/20/2003 16:03'!
asActionSequenceTrappingErrors

	^WeakActionSequenceTrappingErrors with: self! !

!Object methodsFor: 'converting' stamp: 'svp 5/16/2000 18:14'!
asDraggableMorph
	^(StringMorph contents: self printString)
		color: Color white;
		yourself! !

!Object methodsFor: 'converting' stamp: 'sma 5/12/2000 17:39'!
asOrderedCollection
	"Answer an OrderedCollection with the receiver as its only element."

	^ OrderedCollection with: self! !

!Object methodsFor: 'converting'!
asString
	"Answer a string that represents the receiver."

	^ self printString ! !

!Object methodsFor: 'converting' stamp: 'ajh 3/11/2003 10:27'!
asStringOrText
	"Answer a string that represents the receiver."

	^ self printString ! !

!Object methodsFor: 'converting'!
as: aSimilarClass
	"Create an object of class aSimilarClass that has similar contents to the receiver."

	^ aSimilarClass newFrom: self! !

!Object methodsFor: 'converting' stamp: 'RAA 8/2/1999 12:41'!
complexContents

	^self! !

!Object methodsFor: 'converting' stamp: 'ajh 7/6/2003 20:37'!
mustBeBoolean
	"Catches attempts to test truth of non-Booleans.  This message is sent from the VM.  The sending context is rewound to just before the jump causing this exception."

	^ self mustBeBooleanIn: thisContext sender! !

!Object methodsFor: 'converting' stamp: 'ajh 7/6/2003 20:40'!
mustBeBooleanIn: context
	"context is the where the non-boolean error occurred. Rewind context to before jump then raise error."

	| proceedValue |
	context skipBackBeforeJump.
	proceedValue _ NonBooleanReceiver new
		object: self;
		signal: 'proceed for truth.'.
	^ proceedValue ~~ false! !

!Object methodsFor: 'converting' stamp: 'sw 3/26/2001 12:12'!
printDirectlyToDisplay
	"For debugging: write the receiver's printString directly to the display at (0, 100); senders of this are detected by the check-for-slips mechanism."

	self asString displayAt: 0@100

"StringMorph someInstance printDirectlyToDisplay"! !

!Object methodsFor: 'converting' stamp: 'RAA 3/31/1999 12:13'!
withoutListWrapper

	^self! !


!Object methodsFor: 'copying'!
clone

	<primitive: 148>
	self primitiveFailed! !

!Object methodsFor: 'copying' stamp: 'ajh 8/18/2001 21:25'!
copy
	"Answer another instance just like the receiver. Subclasses typically override postCopy; they typically do not override shallowCopy."

	^self shallowCopy postCopy! !

!Object methodsFor: 'copying' stamp: 'tk 8/20/1998 16:01'!
copyAddedStateFrom: anotherObject
	"Copy over the values of instance variables added by the receiver's class from anotherObject to the receiver.  These will be remapped in mapUniClasses, if needed."

	self class superclass instSize + 1 to: self class instSize do:
		[:index | self instVarAt: index put: (anotherObject instVarAt: index)]! !

!Object methodsFor: 'copying' stamp: 'tpr 2/14/2004 21:53'!
copyFrom: anotherObject
	"Copy to myself all instance variables I have in common with anotherObject.  This is dangerous because it ignores an object's control over its own inst vars.  "

	| mine his |
	<primitive: 168>
	mine _ self class allInstVarNames.
	his _ anotherObject class allInstVarNames.
	1 to: (mine size min: his size) do: [:ind |
		(mine at: ind) = (his at: ind) ifTrue: [
			self instVarAt: ind put: (anotherObject instVarAt: ind)]].
	self class isVariable & anotherObject class isVariable ifTrue: [
		1 to: (self basicSize min: anotherObject basicSize) do: [:ind |
			self basicAt: ind put: (anotherObject basicAt: ind)]].! !

!Object methodsFor: 'copying' stamp: 'ajh 5/23/2002 00:38'!
copySameFrom: otherObject
	"Copy to myself all instance variables named the same in otherObject.
	This ignores otherObject's control over its own inst vars."

	| myInstVars otherInstVars match |
	myInstVars _ self class allInstVarNames.
	otherInstVars _ otherObject class allInstVarNames.
	myInstVars doWithIndex: [:each :index |
		(match _ otherInstVars indexOf: each) > 0 ifTrue:
			[self instVarAt: index put: (otherObject instVarAt: match)]].
	1 to: (self basicSize min: otherObject basicSize) do: [:i |
		self basicAt: i put: (otherObject basicAt: i)].
! !

!Object methodsFor: 'copying' stamp: 'tk 4/20/1999 14:44'!
copyTwoLevel
	"one more level than a shallowCopy"

	| newObject class index |
	class _ self class.
	newObject _ self clone.
	newObject == self ifTrue: [^ self].
	class isVariable
		ifTrue: 
			[index _ self basicSize.
			[index > 0]
				whileTrue: 
					[newObject basicAt: index put: (self basicAt: index) shallowCopy.
					index _ index - 1]].
	index _ class instSize.
	[index > 0]
		whileTrue: 
			[newObject instVarAt: index put: (self instVarAt: index) shallowCopy.
			index _ index - 1].
	^newObject! !

!Object methodsFor: 'copying'!
deepCopy
	"Answer a copy of the receiver with its own copy of each instance 
	variable."

	| newObject class index |
	class _ self class.
	(class == Object) ifTrue: [^self].
	class isVariable
		ifTrue: 
			[index _ self basicSize.
			newObject _ class basicNew: index.
			[index > 0]
				whileTrue: 
					[newObject basicAt: index put: (self basicAt: index) deepCopy.
					index _ index - 1]]
		ifFalse: [newObject _ class basicNew].
	index _ class instSize.
	[index > 0]
		whileTrue: 
			[newObject instVarAt: index put: (self instVarAt: index) deepCopy.
			index _ index - 1].
	^newObject! !

!Object methodsFor: 'copying' stamp: 'hg 11/23/1999 13:43'!
initialDeepCopierSize
	"default value is 4096; other classes may override this, esp. for smaller (=faster) sizes"

	^4096! !

!Object methodsFor: 'copying' stamp: 'ajh 1/27/2003 18:45'!
postCopy
	"self is a shallow copy, subclasses should copy fields as necessary to complete the full copy"

	^ self! !

!Object methodsFor: 'copying' stamp: 'jm 11/14/97 11:08'!
shallowCopy
	"Answer a copy of the receiver which shares the receiver's instance variables."
	| class newObject index |
	<primitive: 148>
	class _ self class.
	class isVariable
		ifTrue: 
			[index _ self basicSize.
			newObject _ class basicNew: index.
			[index > 0]
				whileTrue: 
					[newObject basicAt: index put: (self basicAt: index).
					index _ index - 1]]
		ifFalse: [newObject _ class basicNew].
	index _ class instSize.
	[index > 0]
		whileTrue: 
			[newObject instVarAt: index put: (self instVarAt: index).
			index _ index - 1].
	^ newObject! !

!Object methodsFor: 'copying' stamp: 'tk 3/11/2003 13:58'!
veryDeepCopy
	"Do a complete tree copy using a dictionary.  An object in the tree twice is only copied once.  All references to the object in the copy of the tree will point to the new copy."

	| copier new |
	copier _ DeepCopier new initialize: self initialDeepCopierSize.
	new _ self veryDeepCopyWith: copier.
	copier mapUniClasses.
	copier references associationsDo: [:assoc | 
		assoc value veryDeepFixupWith: copier].
	copier fixDependents.
	^ new! !

!Object methodsFor: 'copying' stamp: 'tk 3/11/2003 13:58'!
veryDeepCopySibling
	"Do a complete tree copy using a dictionary.  Substitute a clone of oldPlayer for the root.  Normally, a Player or non systemDefined object would have a new class.  We do not want one this time.  An object in the tree twice, is only copied once.  All references to the object in the copy of the tree will point to the new copy."

	| copier new |
	copier _ DeepCopier new initialize: self initialDeepCopierSize.
	copier newUniClasses: false.
	new _ self veryDeepCopyWith: copier.
	copier mapUniClasses.
	copier references associationsDo: [:assoc | 
		assoc value veryDeepFixupWith: copier].
	copier fixDependents.
	^ new! !

!Object methodsFor: 'copying' stamp: 'tk 5/13/2003 19:39'!
veryDeepCopyUsing: copier
	"Do a complete tree copy using a dictionary.  An object in the tree twice is only copied once.  All references to the object in the copy of the tree will point to the new copy.
	Same as veryDeepCopy except copier (with dictionary) is supplied.
	** do not delete this method, even if it has no callers **"

	| new refs newDep newModel |
	new _ self veryDeepCopyWith: copier.
	copier mapUniClasses.
	copier references associationsDo: [:assoc | 
		assoc value veryDeepFixupWith: copier].
	"Fix dependents"
	refs _ copier references.
	DependentsFields associationsDo: [:pair |
		pair value do: [:dep | 
			(newDep _ refs at: dep ifAbsent: [nil]) ifNotNil: [
				newModel _ refs at: pair key ifAbsent: [pair key].
				newModel addDependent: newDep]]].
	^ new! !

!Object methodsFor: 'copying' stamp: 'tk 3/11/2003 14:12'!
veryDeepCopyWith: deepCopier
	"Copy me and the entire tree of objects I point to.  An object in the tree twice is copied once, and both references point to him.  deepCopier holds a dictionary of objects we have seen.  Some classes refuse to be copied.  Some classes are picky about which fields get deep copied."
	| class index sub subAss new uc sup has mine |
	deepCopier references at: self ifPresent: [:newer | ^ newer]. 	"already did him"
	class _ self class.
	class isMeta ifTrue: [^ self].		"a class"
	new _ self clone.
	(class isSystemDefined not and: [deepCopier newUniClasses "allowed"]) ifTrue: [
		uc _ deepCopier uniClasses at: class ifAbsent: [nil].
		uc ifNil: [
			deepCopier uniClasses at: class put: (uc _ self copyUniClassWith: deepCopier).
			deepCopier references at: class put: uc].	"remember"
		new _ uc new.
		new copyFrom: self].	"copy inst vars in case any are weak"
	deepCopier references at: self put: new.	"remember"
	(class isVariable and: [class isPointers]) ifTrue: 
		[index _ self basicSize.
		[index > 0] whileTrue: 
			[sub _ self basicAt: index.
			(subAss _ deepCopier references associationAt: sub ifAbsent: [nil])
				ifNil: [new basicAt: index put: (sub veryDeepCopyWith: deepCopier)]
				ifNotNil: [new basicAt: index put: subAss value].
			index _ index - 1]].
	"Ask each superclass if it wants to share (weak copy) any inst vars"
	new veryDeepInner: deepCopier.		"does super a lot"

	"other superclasses want all inst vars deep copied"
	sup _ class.  index _ class instSize.
	[has _ sup compiledMethodAt: #veryDeepInner: ifAbsent: [nil].
	has _ has ifNil: [class isSystemDefined not "is a uniClass"] ifNotNil: [true].
	mine _ sup instVarNames.
	has ifTrue: [index _ index - mine size]	"skip inst vars"
		ifFalse: [1 to: mine size do: [:xx |
				sub _ self instVarAt: index.
				(subAss _ deepCopier references associationAt: sub ifAbsent: [nil])
						"use association, not value, so nil is an exceptional value"
					ifNil: [new instVarAt: index put: 
								(sub veryDeepCopyWith: deepCopier)]
					ifNotNil: [new instVarAt: index put: subAss value].
				index _ index - 1]].
	(sup _ sup superclass) == nil] whileFalse.
	new rehash.	"force Sets and Dictionaries to rehash"
	^ new
! !

!Object methodsFor: 'copying' stamp: 'tk 1/6/1999 17:39'!
veryDeepFixupWith: deepCopier
	"I have no fields and no superclass.  Catch the super call."
! !

!Object methodsFor: 'copying' stamp: 'tk 9/4/2001 10:30'!
veryDeepInner: deepCopier
	"No special treatment for inst vars of my superclasses.  Override when some need to be weakly copied.  Object>>veryDeepCopyWith: will veryDeepCopy any inst var whose class does not actually define veryDeepInner:"
! !


!Object methodsFor: 'creation' stamp: 'nk 2/26/2004 13:35'!
asStringMorph
	"Open a StringMorph, as best one can, on the receiver"

	^ self asStringOrText asStringMorph
! !

!Object methodsFor: 'creation' stamp: 'nk 2/26/2004 13:35'!
asTextMorph
	"Open a TextMorph, as best one can, on the receiver"

	^ TextMorph new contentsAsIs: self asStringOrText
! !

!Object methodsFor: 'creation' stamp: 'sw 1/29/2002 21:45'!
openAsMorph
	"Open a morph, as best one can, on the receiver"

	^ self asMorph openInHand

"
234 openAsMorph
(ScriptingSystem formAtKey: #TinyMenu) openAsMorph
'fred' openAsMorph
"! !


!Object methodsFor: 'debugging' stamp: 'md 11/24/2004 11:45'!
haltIf: condition
	"This is the typical message to use for inserting breakpoints during 
	debugging.  Param can be a block or expression, halt if true.
	If the Block has one arg, the receiver is bound to that.
 	If the condition is a selector, we look up in the callchain. Halt if
      any method's selector equals selector."
	| cntxt |

	condition isSymbol ifTrue:[
		"only halt if a method with selector symbol is in callchain"
		cntxt := thisContext.
		[cntxt sender isNil] whileFalse: [
			cntxt := cntxt sender. 
			(cntxt selector = condition) ifTrue: [Halt signal].
			].
		^self.
	].
	(condition isBlock 
			ifTrue: [condition valueWithPossibleArgument: self] 
			ifFalse: [condition] 
	) ifTrue: [
		Halt signal
	].! !

!Object methodsFor: 'debugging'!
needsWork! !


!Object methodsFor: 'debugging-haltOnce' stamp: 'sbw 6/2/2004 08:26'!
checkHaltCountExpired
	| counter |
	counter _ Smalltalk at: #HaltCount ifAbsent: [0].
	^counter = 0! !

!Object methodsFor: 'debugging-haltOnce' stamp: 'sbw 5/19/2004 19:04'!
clearHaltOnce
	"Turn on the halt once flag."
	Smalltalk at: #HaltOnce put: false! !

!Object methodsFor: 'debugging-haltOnce' stamp: 'sbw 6/2/2004 08:30'!
decrementAndCheckHaltCount
	self decrementHaltCount.
	^self checkHaltCountExpired! !

!Object methodsFor: 'debugging-haltOnce' stamp: 'sbw 6/2/2004 08:28'!
decrementHaltCount
	| counter |
	counter := Smalltalk
				at: #HaltCount
				ifAbsent: [0].
	counter > 0 ifTrue: [
		counter _ counter - 1.
		self setHaltCountTo: counter]! !

!Object methodsFor: 'debugging-haltOnce' stamp: 'sbw 6/2/2004 08:42'!
doExpiredHaltCount
	self clearHaltOnce.
	self removeHaltCount.
	self halt! !

!Object methodsFor: 'debugging-haltOnce' stamp: 'sbw 6/2/2004 08:44'!
doExpiredHaltCount: aString
	self clearHaltOnce.
	self removeHaltCount.
	self halt: aString! !

!Object methodsFor: 'debugging-haltOnce' stamp: 'sbw 6/2/2004 08:45'!
doExpiredInspectCount
	self clearHaltOnce.
	self removeHaltCount.
	self inspect! !

!Object methodsFor: 'debugging-haltOnce' stamp: 'sbw 6/2/2004 08:43'!
haltOnCount: int 
	self haltOnceEnabled
		ifTrue: [self hasHaltCount
				ifTrue: [self decrementAndCheckHaltCount
						ifTrue: [self doExpiredHaltCount]]
				ifFalse: [int = 1
						ifTrue: [self doExpiredHaltCount]
						ifFalse: [self setHaltCountTo: int - 1]]]! !

!Object methodsFor: 'debugging-haltOnce' stamp: 'sbw 5/19/2004 19:05'!
haltOnce
	"Halt unless we have already done it once."
	self haltOnceEnabled
		ifTrue: [self clearHaltOnce.
			^ self halt]! !

!Object methodsFor: 'debugging-haltOnce' stamp: 'sbw 5/19/2004 19:04'!
haltOnceEnabled
	^ Smalltalk
		at: #HaltOnce
		ifAbsent: [false]! !

!Object methodsFor: 'debugging-haltOnce' stamp: 'sbw 5/19/2004 19:05'!
haltOnce: aString 
	"Halt unless we have already done it once."
	self haltOnceEnabled
		ifTrue: [self clearHaltOnce.
			^ self halt: aString]! !

!Object methodsFor: 'debugging-haltOnce' stamp: 'sbw 6/2/2004 08:45'!
halt: aString onCount: int 
	self haltOnceEnabled
		ifTrue: [self hasHaltCount
				ifTrue: [self decrementAndCheckHaltCount
						ifTrue: [self doExpiredHaltCount: aString]]
				ifFalse: [int = 1
						ifTrue: [self doExpiredHaltCount: aString]
						ifFalse: [self setHaltCountTo: int - 1]]]! !

!Object methodsFor: 'debugging-haltOnce' stamp: 'sbw 6/2/2004 08:36'!
hasHaltCount
	^Smalltalk
				includesKey: #HaltCount! !

!Object methodsFor: 'debugging-haltOnce' stamp: 'sbw 6/2/2004 08:46'!
inspectOnCount: int 
	self haltOnceEnabled
		ifTrue: [self hasHaltCount
				ifTrue: [self decrementAndCheckHaltCount
						ifTrue: [self doExpiredInspectCount]]
				ifFalse: [int = 1
						ifTrue: [self doExpiredInspectCount]
						ifFalse: [self setHaltCountTo: int - 1]]]! !

!Object methodsFor: 'debugging-haltOnce' stamp: 'sbw 5/19/2004 19:05'!
inspectOnce
	"Inspect unless we have already done it once."
	self haltOnceEnabled
		ifTrue: [self clearHaltOnce.
			^ self inspect]! !

!Object methodsFor: 'debugging-haltOnce' stamp: 'sbw 6/2/2004 13:20'!
inspectUntilCount: int 
	self haltOnceEnabled
		ifTrue: [self hasHaltCount
				ifTrue: [self decrementAndCheckHaltCount
						ifTrue: [self doExpiredInspectCount]
						ifFalse: [self inspect]]
				ifFalse: [int = 1
						ifTrue: [self doExpiredInspectCount]
						ifFalse: [self setHaltCountTo: int - 1]]]! !

!Object methodsFor: 'debugging-haltOnce' stamp: 'sbw 6/2/2004 08:49'!
removeHaltCount
	(Smalltalk includesKey: #HaltCount) ifTrue: [
		Smalltalk removeKey: #HaltCount]! !

!Object methodsFor: 'debugging-haltOnce' stamp: 'sbw 6/2/2004 08:25'!
setHaltCountTo: int
	Smalltalk at: #HaltCount put: int! !

!Object methodsFor: 'debugging-haltOnce' stamp: 'sbw 5/19/2004 19:04'!
setHaltOnce
	"Turn on the halt once flag."
	Smalltalk at: #HaltOnce put: true! !

!Object methodsFor: 'debugging-haltOnce' stamp: 'sbw 5/19/2004 19:04'!
toggleHaltOnce
	self haltOnceEnabled
		ifTrue: [self clearHaltOnce]
		ifFalse: [self setHaltOnce]! !


!Object methodsFor: 'dependents access' stamp: 'ar 2/11/2001 01:55'!
addDependent: anObject
	"Make the given object one of the receiver's dependents."

	| dependents |
	dependents _ self dependents.
	(dependents includes: anObject) ifFalse:
		[self myDependents: (dependents copyWithDependent: anObject)].
	^ anObject! !

!Object methodsFor: 'dependents access' stamp: 'sma 2/29/2000 19:53'!
breakDependents
	"Remove all of the receiver's dependents."

	self myDependents: nil! !

!Object methodsFor: 'dependents access' stamp: 'sma 2/29/2000 19:26'!
canDiscardEdits
	"Answer true if none of the views on this model has unaccepted edits that matter."

	self dependents
		do: [:each | each canDiscardEdits ifFalse: [^ false]]
		without: self.
	^ true! !

!Object methodsFor: 'dependents access' stamp: 'sma 2/29/2000 19:58'!
dependents
	"Answer a collection of objects that are 'dependent' on the receiver;
	 that is, all objects that should be notified if the receiver changes."

	^ self myDependents ifNil: [#()]! !

!Object methodsFor: 'dependents access'!
evaluate: actionBlock wheneverChangeIn: aspectBlock
	| viewerThenObject objectThenViewer |
	objectThenViewer _ self.
	viewerThenObject _ ObjectViewer on: objectThenViewer.
	objectThenViewer become: viewerThenObject.
	"--- Then ---"
	objectThenViewer xxxViewedObject: viewerThenObject
			evaluate: actionBlock
			wheneverChangeIn: aspectBlock! !

!Object methodsFor: 'dependents access' stamp: 'sma 2/29/2000 19:59'!
hasUnacceptedEdits
	"Answer true if any of the views on this object has unaccepted edits."

	self dependents
		do: [:each | each hasUnacceptedEdits ifTrue: [^ true]]
		without: self.
	^ false! !

!Object methodsFor: 'dependents access' stamp: 'sma 2/29/2000 19:55'!
myDependents
	"Private. Answer a list of all the receiver's dependents."

	^ DependentsFields at: self ifAbsent: []! !

!Object methodsFor: 'dependents access' stamp: 'sma 2/29/2000 19:52'!
myDependents: aCollectionOrNil
	"Private. Set (or remove) the receiver's dependents list."

	aCollectionOrNil
		ifNil: [DependentsFields removeKey: self ifAbsent: []]
		ifNotNil: [DependentsFields at: self put: aCollectionOrNil]! !

!Object methodsFor: 'dependents access' stamp: 'reThink 2/18/2001 17:06'!
release
	"Remove references to objects that may refer to the receiver. This message 
	should be overridden by subclasses with any cycles, in which case the 
	subclass should also include the expression super release."

	self releaseActionMap! !

!Object methodsFor: 'dependents access' stamp: 'sma 2/29/2000 20:23'!
removeDependent: anObject
	"Remove the given object as one of the receiver's dependents."

	| dependents |
	dependents _ self dependents reject: [:each | each == anObject].
	self myDependents: (dependents isEmpty ifFalse: [dependents]).
	^ anObject! !


!Object methodsFor: 'drag and drop' stamp: 'bh 9/16/2001 18:10'!
acceptDroppingMorph: transferMorph event: evt inMorph: dstListMorph 
	
	^false.! !

!Object methodsFor: 'drag and drop' stamp: 'mir 5/16/2000 11:35'!
dragAnimationFor: item transferMorph: transferMorph 
	"Default do nothing"! !

!Object methodsFor: 'drag and drop' stamp: 'panda 4/28/2000 16:20'!
dragPassengerFor: item inMorph: dragSource 
	^item! !

!Object methodsFor: 'drag and drop' stamp: 'panda 4/28/2000 16:11'!
dragTransferType
	^nil! !

!Object methodsFor: 'drag and drop' stamp: 'panda 4/28/2000 16:05'!
dragTransferTypeForMorph: dragSource 
	^nil! !

!Object methodsFor: 'drag and drop' stamp: 'mir 5/8/2000 17:19'!
wantsDroppedMorph: aMorph event: anEvent inMorph: destinationLM 
	^false! !


!Object methodsFor: 'error handling' stamp: 'sma 5/6/2000 19:35'!
assert: aBlock
	"Throw an assertion error if aBlock does not evaluates to true."

	aBlock value ifFalse: [AssertionFailure signal: 'Assertion failed']! !

!Object methodsFor: 'error handling' stamp: 'nk 1/15/2004 10:54'!
assert: aBlock descriptionBlock: descriptionBlock
	"Throw an assertion error if aBlock does not evaluate to true."

	aBlock value ifFalse: [AssertionFailure signal: descriptionBlock value asString ]! !

!Object methodsFor: 'error handling' stamp: 'nk 10/25/2003 16:47'!
assert: aBlock description: aString
	"Throw an assertion error if aBlock does not evaluates to true."

	aBlock value ifFalse: [AssertionFailure signal: aString ]! !

!Object methodsFor: 'error handling' stamp: 'md 10/13/2004 15:59'!
backwardCompatibilityOnly: anExplanationString
	"Warn that the sending method has been deprecated. Methods that are tagt with #backwardCompatibility:
	 are kept for compatibility."

	Preferences showDeprecationWarnings ifTrue:
		[Deprecation signal: thisContext sender printString, ' has been deprecated (but will be kept for compatibility). ', anExplanationString]! !

!Object methodsFor: 'error handling'!
caseError
	"Report an error from an in-line or explicit case statement."

	self error: 'Case not found, and no otherwise clause'! !

!Object methodsFor: 'error handling' stamp: 'rbb 3/1/2005 09:26'!
confirm: queryString
	"Put up a yes/no menu with caption queryString. Answer true if the 
	response is yes, false if no. This is a modal question--the user must 
	respond yes or no."

	"nil confirm: 'Are you hungry?'"

	^ UIManager default confirm: queryString! !

!Object methodsFor: 'error handling' stamp: 'rbb 3/1/2005 09:27'!
confirm: aString orCancel: cancelBlock
	"Put up a yes/no/cancel menu with caption aString. Answer true if  
	the response is yes, false if no. If cancel is chosen, evaluate  
	cancelBlock. This is a modal question--the user must respond yes or no."

	^ UIManager default confirm: aString orCancel: cancelBlock! !

!Object methodsFor: 'error handling' stamp: 'dew 10/6/2003 18:20'!
deprecated: anExplanationString
	"Warn that the sending method has been deprecated."

	Preferences showDeprecationWarnings ifTrue:
		[Deprecation signal: thisContext sender printString, ' has been deprecated. ', anExplanationString]! !

!Object methodsFor: 'error handling' stamp: 'dew 10/7/2003 00:26'!
deprecated: anExplanationString block: aBlock 
	 "Warn that the sender has been deprecated.  Answer the value of aBlock on resumption.  (Note that #deprecated: is usually the preferred method.)"

	Preferences showDeprecationWarnings ifTrue:
		[Deprecation
			signal: thisContext sender printString, ' has been deprecated. ', anExplanationString].
	^ aBlock value.
! !

!Object methodsFor: 'error handling' stamp: 'md 2/22/2006 21:21'!
doesNotUnderstand: aMessage 
	 "Handle the fact that there was an attempt to send the given message to the receiver but the receiver does not understand this message (typically sent from the machine when a message is sent to the receiver and no method is defined for that selector)."
	"Testing: (3 activeProcess)"

	MessageNotUnderstood new 
		message: aMessage;
		receiver: self;
		signal.
	^ aMessage sentTo: self.
! !

!Object methodsFor: 'error handling' stamp: 'TRee 11/4/2003 16:47'!
dpsTrace: reportObject  
	Transcript myDependents isNil ifTrue: [^self].
	self dpsTrace: reportObject levels: 1 withContext: thisContext
		
" nil dpsTrace: 'sludder'. "! !

!Object methodsFor: 'error handling' stamp: 'TRee 11/4/2003 16:49'!
dpsTrace: reportObject levels: anInt
	self dpsTrace: reportObject levels: anInt withContext: thisContext

"(1 to: 3) do: [:int | nil dpsTrace: int levels: 5.]"! !

!Object methodsFor: 'error handling' stamp: 'TRee 11/4/2003 17:02'!
dpsTrace: reportObject levels: anInt withContext: currentContext
	| reportString context displayCount |
	reportString := (reportObject respondsTo: #asString) 
			ifTrue: [reportObject asString] ifFalse: [reportObject printString].
	(Smalltalk at: #Decompiler ifAbsent: [nil]) 
	ifNil: 
		[Transcript cr; show: reportString]
	ifNotNil:
		[context := currentContext.
		displayCount := anInt > 1.
		1 to: anInt do:
			[:count |
			Transcript cr.
			displayCount
				ifTrue: [Transcript show: count printString, ': '].
			
			reportString notNil
			ifTrue:
				[Transcript show: context home class name 
			, '/' , context sender selector,  ' (' , reportString , ')'.
				context := context sender.
				reportString := nil]
			ifFalse:
				[(context notNil and: [(context := context sender) notNil])
				ifTrue: [Transcript show: context receiver class name , '/' , context selector]]].
		"Transcript cr"].! !

!Object methodsFor: 'error handling' stamp: 'md 8/2/2005 22:17'!
error
	"Throw a generic Error exception."

	^self error: 'Error!!'.! !

!Object methodsFor: 'error handling' stamp: 'tfei 4/12/1999 12:55'!
error: aString 
	"Throw a generic Error exception."

	^Error new signal: aString! !

!Object methodsFor: 'error handling' stamp: 'al 9/16/2005 14:12'!
explicitRequirement
	self error: 'Explicitly required method'! !

!Object methodsFor: 'error handling' stamp: 'al 2/13/2006 22:20'!
halt
	"This is the typical message to use for inserting breakpoints during 
	debugging. It behaves like halt:, but does not call on halt: in order to 
	avoid putting this message on the stack. Halt is especially useful when 
	the breakpoint message is an arbitrary one."

	Halt signal! !

!Object methodsFor: 'error handling' stamp: 'tfei 4/12/1999 12:59'!
halt: aString 
	"This is the typical message to use for inserting breakpoints during 
	debugging. It creates and schedules a Notifier with the argument, 
	aString, as the label."
	
	Halt new signal: aString! !

!Object methodsFor: 'error handling' stamp: 'md 1/20/2006 16:24'!
handles: exception
	"This method exists in case a non exception class is the first arg in an on:do: (for instance using a exception class that is not loaded). We prefer this to raising an error during error handling itself. Also, semantically it makes sense that the exception handler is not active if its exception class is not loaded"

	^ false! !

!Object methodsFor: 'error handling' stamp: 'ar 9/27/2005 20:24'!
notifyWithLabel: aString 
	"Create and schedule a Notifier with aString as the window label as well as the contents of the window, in  order to request confirmation before a process can proceed."

	ToolSet
		debugContext: thisContext
		label: aString
		contents: aString

	"nil notifyWithLabel: 'let us see if this works'"! !

!Object methodsFor: 'error handling' stamp: 'hg 10/2/2001 20:49'!
notify: aString 
	"Create and schedule a Notifier with the argument as the message in 
	order to request confirmation before a process can proceed."

	Warning signal: aString

	"nil notify: 'confirmation message'"! !

!Object methodsFor: 'error handling'!
notify: aString at: location
	"Create and schedule a Notifier with the argument as the message in 
	order to request confirmation before a process can proceed. Subclasses can
	override this and insert an error message at location within aString."

	self notify: aString

	"nil notify: 'confirmation message' at: 12"! !

!Object methodsFor: 'error handling'!
primitiveFailed
	"Announce that a primitive has failed and there is no appropriate 
	Smalltalk code to run."

	self error: 'a primitive has failed'! !

!Object methodsFor: 'error handling' stamp: 'al 9/16/2005 14:12'!
requirement
	self error: 'Implicitly required method'! !

!Object methodsFor: 'error handling' stamp: 'AFi 2/8/2003 22:52'!
shouldBeImplemented
	"Announce that this message should be implemented"

	self error: 'This message should be implemented'! !

!Object methodsFor: 'error handling'!
shouldNotImplement
	"Announce that, although the receiver inherits this message, it should 
	not implement it."

	self error: 'This message is not appropriate for this object'! !

!Object methodsFor: 'error handling' stamp: 'md 2/17/2006 12:02'!
subclassResponsibility
	"This message sets up a framework for the behavior of the class' subclasses.
	Announce that the subclass should have implemented this message."

	self error: 'My subclass should have overridden ', thisContext sender selector printString! !

!Object methodsFor: 'error handling' stamp: 'al 12/16/2003 16:16'!
traitConflict
	self error: 'A class or trait does not properly resolve a conflict between multiple traits it uses.'! !


!Object methodsFor: 'evaluating' stamp: 'reThink 3/12/2001 18:14'!
value

	^self! !

!Object methodsFor: 'evaluating' stamp: 'reThink 2/18/2001 15:23'!
valueWithArguments: aSequenceOfArguments

	^self! !


!Object methodsFor: 'events' stamp: 'nk 8/27/2003 16:23'!
actionsWithReceiver: anObject forEvent: anEventSelector

	^(self actionSequenceForEvent: anEventSelector)
                select: [:anAction | anAction receiver == anObject ]! !

!Object methodsFor: 'events' stamp: 'nk 8/27/2003 17:45'!
renameActionsWithReceiver: anObject forEvent: anEventSelector toEvent: newEvent

	| oldActions newActions |
	oldActions _ Set new.
	newActions _ Set new.
	(self actionSequenceForEvent: anEventSelector) do: [ :action |
		action receiver == anObject
			ifTrue: [ oldActions add: anObject ]
			ifFalse: [ newActions add: anObject ]].
	self setActionSequence: (ActionSequence withAll: newActions) forEvent: anEventSelector.
	oldActions do: [ :act | self when: newEvent evaluate: act ].! !


!Object methodsFor: 'events-accessing' stamp: 'nk 12/20/2002 17:48'!
actionForEvent: anEventSelector
    "Answer the action to be evaluated when <anEventSelector> has been triggered."

	| actions |
	actions := self actionMap
		at: anEventSelector asSymbol
		ifAbsent: [nil].
	actions ifNil: [^nil].
	^ actions asMinimalRepresentation! !

!Object methodsFor: 'events-accessing' stamp: 'nk 12/20/2002 17:48'!
actionForEvent: anEventSelector
ifAbsent: anExceptionBlock
    "Answer the action to be evaluated when <anEventSelector> has been triggered."

	| actions |
	actions := self actionMap
		at: anEventSelector asSymbol
		ifAbsent: [nil].
	actions ifNil: [^anExceptionBlock value].
	^ actions asMinimalRepresentation! !

!Object methodsFor: 'events-accessing' stamp: 'reThink 2/18/2001 14:43'!
actionMap

	^EventManager actionMapFor: self! !

!Object methodsFor: 'events-accessing' stamp: 'rw 4/27/2002 08:35'!
actionSequenceForEvent: anEventSelector

    ^(self actionMap
        at: anEventSelector asSymbol
        ifAbsent: [^WeakActionSequence new])
            asActionSequence! !

!Object methodsFor: 'events-accessing' stamp: 'SqR 6/28/2001 13:19'!
actionsDo: aBlock

	self actionMap do: aBlock! !

!Object methodsFor: 'events-accessing' stamp: 'rw 2/10/2002 13:05'!
createActionMap

	^IdentityDictionary new! !

!Object methodsFor: 'events-accessing' stamp: 'SqR 2/19/2001 14:04'!
hasActionForEvent: anEventSelector
    "Answer true if there is an action associated with anEventSelector"

    ^(self actionForEvent: anEventSelector) notNil! !

!Object methodsFor: 'events-accessing' stamp: 'reThink 2/18/2001 15:29'!
setActionSequence: actionSequence
forEvent: anEventSelector

    | action |
    action := actionSequence asMinimalRepresentation.
    action == nil
        ifTrue:
            [self removeActionsForEvent: anEventSelector]
        ifFalse:
            [self updateableActionMap
                at: anEventSelector asSymbol
                put: action]! !

!Object methodsFor: 'events-accessing' stamp: 'reThink 2/25/2001 08:50'!
updateableActionMap

	^EventManager updateableActionMapFor: self! !


!Object methodsFor: 'events-registering' stamp: 'reThink 2/18/2001 15:04'!
when: anEventSelector evaluate: anAction 

	| actions |
	actions := self actionSequenceForEvent: anEventSelector.
	(actions includes: anAction)
		ifTrue: [^ self].
	self 
		setActionSequence: (actions copyWith: anAction)
		forEvent: anEventSelector! !

!Object methodsFor: 'events-registering' stamp: 'rww 12/30/2002 10:37'!
when: anEventSelector
send: aMessageSelector
to: anObject
 
    self
        when: anEventSelector
        evaluate: (WeakMessageSend
            receiver: anObject
            selector: aMessageSelector)! !

!Object methodsFor: 'events-registering' stamp: 'rww 12/30/2002 10:37'!
when: anEventSelector
send: aMessageSelector
to: anObject
withArguments: anArgArray
 
    self
        when: anEventSelector
        evaluate: (WeakMessageSend
            receiver: anObject
            selector: aMessageSelector
		arguments: anArgArray)! !

!Object methodsFor: 'events-registering' stamp: 'rww 12/30/2002 10:37'!
when: anEventSelector
send: aMessageSelector
to: anObject
with: anArg
 
    self
        when: anEventSelector
        evaluate: (WeakMessageSend
            receiver: anObject
            selector: aMessageSelector
		arguments: (Array with: anArg))! !


!Object methodsFor: 'events-removing' stamp: 'reThink 2/18/2001 15:33'!
releaseActionMap

	EventManager releaseActionMapFor: self! !

!Object methodsFor: 'events-removing' stamp: 'reThink 2/18/2001 15:33'!
removeActionsForEvent: anEventSelector

    | map |
    map := self actionMap.
    map removeKey: anEventSelector asSymbol ifAbsent: [].
    map isEmpty
        ifTrue: [self releaseActionMap]! !

!Object methodsFor: 'events-removing' stamp: 'nk 8/25/2003 21:46'!
removeActionsSatisfying: aBlock

	self actionMap keys do:
		[:eachEventSelector |
			self
   				removeActionsSatisfying: aBlock
				forEvent: eachEventSelector
		]! !

!Object methodsFor: 'events-removing' stamp: 'reThink 2/18/2001 15:31'!
removeActionsSatisfying: aOneArgBlock 
forEvent: anEventSelector

    self
        setActionSequence:
            ((self actionSequenceForEvent: anEventSelector)
                reject: [:anAction | aOneArgBlock value: anAction])
        forEvent: anEventSelector! !

!Object methodsFor: 'events-removing' stamp: 'rw 7/29/2003 17:18'!
removeActionsWithReceiver: anObject

	self actionMap copy keysDo:
		[:eachEventSelector |
			self
   				removeActionsSatisfying: [:anAction | anAction receiver == anObject]
				forEvent: eachEventSelector
		]! !

!Object methodsFor: 'events-removing' stamp: 'reThink 2/18/2001 15:36'!
removeActionsWithReceiver: anObject
forEvent: anEventSelector

    self
        removeActionsSatisfying:
            [:anAction |
            anAction receiver == anObject]
        forEvent: anEventSelector! !

!Object methodsFor: 'events-removing' stamp: 'reThink 2/18/2001 15:31'!
removeAction: anAction
forEvent: anEventSelector

    self
        removeActionsSatisfying: [:action | action = anAction]
        forEvent: anEventSelector! !


!Object methodsFor: 'events-triggering' stamp: 'reThink 2/18/2001 15:22'!
triggerEvent: anEventSelector
	"Evaluate all actions registered for <anEventSelector>. Return the value of the last registered action."

    ^(self actionForEvent: anEventSelector) value! !

!Object methodsFor: 'events-triggering' stamp: 'reThink 2/18/2001 17:09'!
triggerEvent: anEventSelector
ifNotHandled: anExceptionBlock
	"Evaluate all actions registered for <anEventSelector>. Return the value of the last registered action."

    ^(self 
		actionForEvent: anEventSelector
		ifAbsent: [^anExceptionBlock value]) value
! !

!Object methodsFor: 'events-triggering' stamp: 'reThink 2/18/2001 15:21'!
triggerEvent: anEventSelector
withArguments: anArgumentList

    ^(self actionForEvent: anEventSelector)
        valueWithArguments: anArgumentList! !

!Object methodsFor: 'events-triggering' stamp: 'reThink 2/18/2001 15:21'!
triggerEvent: anEventSelector
withArguments: anArgumentList
ifNotHandled: anExceptionBlock

    ^(self 
		actionForEvent: anEventSelector
		ifAbsent: [^anExceptionBlock value])
        valueWithArguments: anArgumentList! !

!Object methodsFor: 'events-triggering' stamp: 'reThink 2/18/2001 14:59'!
triggerEvent: anEventSelector
with: anObject

    ^self 
		triggerEvent: anEventSelector
		withArguments: (Array with: anObject)! !

!Object methodsFor: 'events-triggering' stamp: 'reThink 2/18/2001 14:59'!
triggerEvent: anEventSelector
with: anObject
ifNotHandled: anExceptionBlock

    ^self 
		triggerEvent: anEventSelector
		withArguments: (Array with: anObject)
		ifNotHandled: anExceptionBlock! !


!Object methodsFor: 'filter streaming' stamp: 'MPW 1/1/1901 00:42'!
byteEncode:aStream
	self flattenOnStream:aStream.
! !

!Object methodsFor: 'filter streaming'!
drawOnCanvas:aStream
	self flattenOnStream:aStream.
! !

!Object methodsFor: 'filter streaming' stamp: 'MPW 1/1/1901 01:31'!
elementSeparator
	^nil.! !

!Object methodsFor: 'filter streaming'!
encodePostscriptOn:aStream
	self byteEncode:aStream.
! !

!Object methodsFor: 'filter streaming' stamp: 'MPW 1/1/1901 00:07'!
flattenOnStream:aStream
	self writeOnFilterStream:aStream.
! !

!Object methodsFor: 'filter streaming' stamp: 'mpw 6/22/1930 22:56'!
fullDrawPostscriptOn:aStream
	^aStream fullDraw:self.
! !

!Object methodsFor: 'filter streaming' stamp: 'MPW 1/1/1901 01:51'!
printOnStream:aStream
	self byteEncode:aStream.
! !

!Object methodsFor: 'filter streaming' stamp: 'MPW 1/1/1901 00:49'!
putOn:aStream
	^aStream nextPut:self.
! !

!Object methodsFor: 'filter streaming' stamp: 'MPW 1/1/1901 01:53'!
storeOnStream:aStream
	self printOnStream:aStream.
! !

!Object methodsFor: 'filter streaming' stamp: 'MPW 1/1/1901 00:06'!
writeOnFilterStream:aStream
	aStream writeObject:self.
! !


!Object methodsFor: 'finalization' stamp: 'ar 3/21/98 16:26'!
actAsExecutor
	"Prepare the receiver to act as executor for any resources associated with it"
	self breakDependents! !

!Object methodsFor: 'finalization' stamp: 'ar 3/20/98 22:19'!
executor
	"Return an object which can act as executor for finalization of the receiver"
	^self shallowCopy actAsExecutor! !

!Object methodsFor: 'finalization' stamp: 'ar 5/19/2003 20:10'!
finalizationRegistry
	"Answer the finalization registry associated with the receiver."
	^WeakRegistry default! !

!Object methodsFor: 'finalization' stamp: 'ar 3/21/98 16:27'!
finalize
	"Finalize the resource associated with the receiver. This message should only be sent during the finalization process. There is NO garantuee that the resource associated with the receiver hasn't been free'd before so take care that you don't run into trouble - this all may happen with interrupt priority."! !

!Object methodsFor: 'finalization' stamp: 'ar 3/21/98 18:38'!
retryWithGC: execBlock until: testBlock
	"Retry execBlock as long as testBlock returns false. Do an incremental GC after the first try, a full GC after the second try."
	| blockValue |
	blockValue := execBlock value.
	(testBlock value: blockValue) ifTrue:[^blockValue].
	Smalltalk garbageCollectMost.
	blockValue := execBlock value.
	(testBlock value: blockValue) ifTrue:[^blockValue].
	Smalltalk garbageCollect.
	^execBlock value.! !

!Object methodsFor: 'finalization' stamp: 'ar 5/19/2003 20:14'!
toFinalizeSend: aSelector to: aFinalizer with: aResourceHandle
	"When I am finalized (e.g., garbage collected) close the associated resource handle by sending aSelector to the appropriate finalizer (the guy who knows how to get rid of the resource).
	WARNING: Neither the finalizer nor the resource handle are allowed to reference me. If they do, then I will NEVER be garbage collected. Since this cannot be validated here, it is up to the client to make sure this invariant is not broken."
	self == aFinalizer ifTrue:[self error: 'I cannot finalize myself'].
	self == aResourceHandle ifTrue:[self error: 'I cannot finalize myself'].
	^self finalizationRegistry add: self executor:
		(ObjectFinalizer new
			receiver: aFinalizer
			selector: aSelector
			argument: aResourceHandle)! !


!Object methodsFor: 'flagging' stamp: 'sw 8/4/97 16:49'!
isThisEverCalled
	^ self isThisEverCalled: thisContext sender printString! !

!Object methodsFor: 'flagging'!
isThisEverCalled: msg
	"Send this message, with some useful printable argument, from methods or branches of methods which you believe are never reached.  2/5/96 sw"

	self halt: 'This is indeed called: ', msg printString! !

!Object methodsFor: 'flagging' stamp: 'jm 3/18/98 17:23'!
logEntry

	Transcript show: 'Entered ', thisContext sender printString; cr.
! !

!Object methodsFor: 'flagging' stamp: 'jm 3/18/98 17:23'!
logExecution

	Transcript show: 'Executing ', thisContext sender printString; cr.
! !

!Object methodsFor: 'flagging' stamp: 'jm 3/18/98 17:22'!
logExit

	Transcript show:  'Exited ', thisContext sender printString; cr.
! !


!Object methodsFor: 'graph model' stamp: 'dgd 9/18/2004 15:07'!
addModelYellowButtonMenuItemsTo: aCustomMenu forMorph: aMorph hand: aHandMorph 
	"The receiver serves as the model for aMorph; a menu is being constructed for the morph, and here the receiver is able to add its own items"
	Preferences cmdGesturesEnabled ifTrue: [ "build mode"
		aCustomMenu add: 'inspect model' translated target: self action: #inspect.
	].

	^aCustomMenu
! !

!Object methodsFor: 'graph model' stamp: 'nk 1/23/2004 14:35'!
hasModelYellowButtonMenuItems
	^Preferences cmdGesturesEnabled! !


!Object methodsFor: 'inspecting' stamp: 'ar 9/27/2005 18:31'!
basicInspect
	"Create and schedule an Inspector in which the user can examine the 
	receiver's variables. This method should not be overriden."
	^ToolSet basicInspect: self! !

!Object methodsFor: 'inspecting' stamp: 'md 1/18/2006 19:09'!
inspect
	"Create and schedule an Inspector in which the user can examine the receiver's variables."
	ToolSet inspect: self! !

!Object methodsFor: 'inspecting' stamp: 'apb 7/14/2004 12:19'!
inspectorClass
	"Answer the class of the inspector to be used on the receiver.  Called by inspect; 
	use basicInspect to get a normal (less useful) type of inspector."

	^ Inspector! !


!Object methodsFor: 'locales' stamp: 'tak 8/4/2005 14:55'!
localeChanged
	self shouldBeImplemented! !


!Object methodsFor: 'macpal' stamp: 'sw 5/7/1998 23:00'!
codeStrippedOut: messageString
	"When a method is stripped out for external release, it is replaced by a method that calls this"

	self halt: 'Code stripped out -- ', messageString, '-- do not proceed.'! !

!Object methodsFor: 'macpal' stamp: 'sw 1/28/1999 17:31'!
contentsChanged
	self changed: #contents! !

!Object methodsFor: 'macpal' stamp: 'ar 3/18/2001 00:03'!
currentEvent
	"Answer the current Morphic event.  This method never returns nil."
	^ActiveEvent ifNil:[self currentHand lastEvent]! !

!Object methodsFor: 'macpal' stamp: 'nk 9/1/2004 10:41'!
currentHand
	"Return a usable HandMorph -- the one associated with the object's current environment.  This method will always return a hand, even if it has to conjure one up as a last resort.  If a particular hand is actually handling events at the moment (such as a remote hand or a ghost hand), it will be returned."

	^ActiveHand ifNil: [ self currentWorld primaryHand ]! !

!Object methodsFor: 'macpal' stamp: 'sw 5/17/2001 12:08'!
currentVocabulary
	"Answer the currently-prevailing default vocabulary."

	^ Smalltalk isMorphic ifTrue:
			[ActiveWorld currentVocabulary]
		ifFalse:
			[Vocabulary fullVocabulary]! !

!Object methodsFor: 'macpal' stamp: 'ar 3/18/2001 00:08'!
currentWorld
	"Answer a morphic world that is the current UI focus.
		If in an embedded world, it's that world.
		If in a morphic project, it's that project's world.  
		If in an mvc project, it is the topmost morphic-mvc-window's worldMorph. 
		If in an mvc project that has no morphic-mvc-windows, then it's just some existing worldmorph instance.
		If in an mvc project in a Squeak that has NO WorldMorph instances, one is created.

	This method will never return nil, it will always return its best effort at returning a relevant world morph, but if need be -- if there are no worlds anywhere, it will create a new one."

	| aView aSubview |
	ActiveWorld ifNotNil:[^ActiveWorld].
	World ifNotNil:[^World].
	aView _ ScheduledControllers controllerSatisfying:
		[:ctrl | (aSubview _ ctrl view firstSubView) notNil and:
			[aSubview model isMorph and: [aSubview model isWorldMorph]]].
	^aView
		ifNotNil:
			[aSubview model]
		ifNil:
			[MVCWiWPasteUpMorph newWorldForProject: nil].! !

!Object methodsFor: 'macpal' stamp: 'jm 5/6/1998 22:35'!
flash
	"Do nothing."
! !

!Object methodsFor: 'macpal' stamp: 'sw 6/16/1998 15:07'!
instanceVariableValues
	"Answer a collection whose elements are the values of those instance variables of the receiver which were added by the receiver's class"
	| c |
	c _ OrderedCollection new.
	self class superclass instSize + 1 to: self class instSize do:
		[:i | c add: (self instVarAt: i)].
	^ c! !

!Object methodsFor: 'macpal' stamp: 'sw 3/20/2001 13:29'!
isUniversalTiles
	"Return true if I (my world) uses universal tiles.  This message can be called in places where the current World is not known, such as when writing out a project.  For more information about the project-writing subtlety addressed by this protocol, kindly contact Ted Kaehler."

	^ Preferences universalTiles! !

!Object methodsFor: 'macpal' stamp: 'sw 10/24/2000 07:04'!
objectRepresented
	"most objects represent themselves; this provides a hook for aliases to grab on to"

	^ self! !

!Object methodsFor: 'macpal' stamp: 'sw 5/22/2001 18:31'!
refusesToAcceptCode
	"Answer whether the receiver is a code-bearing instrument which at the moment refuses to allow its contents to be submitted"

	^ false
	! !

!Object methodsFor: 'macpal' stamp: 'jm 2/24/1999 12:40'!
scriptPerformer

	^ self
! !

!Object methodsFor: 'macpal' stamp: 'sw 3/20/2001 13:40'!
slotInfo
	"Answer a list of slot-information objects.  Initally only provides useful info for players"

	^ Dictionary new! !


!Object methodsFor: 'message handling' stamp: 'md 1/20/2006 16:28'!
executeMethod: compiledMethod
	"Execute compiledMethod against the receiver with no args"

	"<primitive: 189>" "uncomment once prim 189 is in VM"
	^ self withArgs: #() executeMethod: compiledMethod! !

!Object methodsFor: 'message handling' stamp: 'di 3/26/1999 07:52'!
perform: aSymbol 
	"Send the unary selector, aSymbol, to the receiver.
	Fail if the number of arguments expected by the selector is not zero.
	Primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 83>
	^ self perform: aSymbol withArguments: (Array new: 0)! !

!Object methodsFor: 'message handling' stamp: 'st 11/5/2004 16:19'!
perform: selector orSendTo: otherTarget
	"If I wish to intercept and handle selector myself, do it; else send it to otherTarget"
	^ (self respondsTo: selector) ifTrue: [self perform: selector] ifFalse: [otherTarget perform: selector]! !

!Object methodsFor: 'message handling' stamp: 'di 3/26/1999 07:55'!
perform: selector withArguments: argArray 
	"Send the selector, aSymbol, to the receiver with arguments in argArray.
	Fail if the number of arguments expected by the selector 
	does not match the size of argArray.
	Primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 84>
	^ self perform: selector withArguments: argArray inSuperclass: self class! !

!Object methodsFor: 'message handling' stamp: 'ar 4/25/2005 13:35'!
perform: selector withArguments: argArray inSuperclass: lookupClass
	"NOTE:  This is just like perform:withArguments:, except that
	the message lookup process begins, not with the receivers's class,
	but with the supplied superclass instead.  It will fail if lookupClass
	cannot be found among the receiver's superclasses.
	Primitive. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 100>
	(selector isSymbol)
		ifFalse: [^ self error: 'selector argument must be a Symbol'].
	(selector numArgs = argArray size)
		ifFalse: [^ self error: 'incorrect number of arguments'].
	(self class == lookupClass or: [self class inheritsFrom: lookupClass])
		ifFalse: [^ self error: 'lookupClass is not in my inheritance chain'].
	self primitiveFailed! !

!Object methodsFor: 'message handling' stamp: 'nk 4/11/2002 14:13'!
perform: selector withEnoughArguments: anArray
	"Send the selector, aSymbol, to the receiver with arguments in argArray.
	Only use enough arguments for the arity of the selector; supply nils for missing ones."
	| numArgs args |
	numArgs _ selector numArgs.
	anArray size == numArgs
		ifTrue: [ ^self perform: selector withArguments: anArray asArray ].

	args _ Array new: numArgs.
	args replaceFrom: 1
		to: (anArray size min: args size)
		with: anArray
		startingAt: 1.

	^ self perform: selector withArguments: args! !

!Object methodsFor: 'message handling' stamp: 'di 3/26/1999 07:52'!
perform: aSymbol with: anObject 
	"Send the selector, aSymbol, to the receiver with anObject as its argument.
	Fail if the number of arguments expected by the selector is not one.
	Primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 83>
	^ self perform: aSymbol withArguments: (Array with: anObject)! !

!Object methodsFor: 'message handling' stamp: 'di 3/26/1999 07:52'!
perform: aSymbol with: firstObject with: secondObject 
	"Send the selector, aSymbol, to the receiver with the given arguments.
	Fail if the number of arguments expected by the selector is not two.
	Primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 83>
	^ self perform: aSymbol withArguments: (Array with: firstObject with: secondObject)! !

!Object methodsFor: 'message handling' stamp: 'di 3/26/1999 07:51'!
perform: aSymbol with: firstObject with: secondObject with: thirdObject 
	"Send the selector, aSymbol, to the receiver with the given arguments.
	Fail if the number of arguments expected by the selector is not three.
	Primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 83>
	^ self perform: aSymbol
		withArguments: (Array with: firstObject with: secondObject with: thirdObject)! !

!Object methodsFor: 'message handling' stamp: 'NS 1/28/2004 11:19'!
withArgs: argArray executeMethod: compiledMethod
	"Execute compiledMethod against the receiver and args in argArray"

	| selector |
	<primitive: 188>
	selector _ Symbol new.
	self class addSelectorSilently: selector withMethod: compiledMethod.
	^ [self perform: selector withArguments: argArray]
		ensure: [self class basicRemoveSelector: selector]! !

!Object methodsFor: 'message handling' stamp: 'md 1/20/2006 16:28'!
with: arg1 executeMethod: compiledMethod
	"Execute compiledMethod against the receiver and arg1"

	"<primitive: 189>" "uncomment once prim 189 is in VM"
	^ self withArgs: {arg1} executeMethod: compiledMethod! !

!Object methodsFor: 'message handling' stamp: 'md 1/20/2006 16:28'!
with: arg1 with: arg2 executeMethod: compiledMethod
	"Execute compiledMethod against the receiver and arg1 & arg2"

	"<primitive: 189>" "uncomment once prim 189 is in VM"
	^ self withArgs: {arg1. arg2} executeMethod: compiledMethod! !

!Object methodsFor: 'message handling' stamp: 'md 1/20/2006 16:28'!
with: arg1 with: arg2 with: arg3 executeMethod: compiledMethod
	"Execute compiledMethod against the receiver and arg1, arg2, & arg3"

	"<primitive: 189>" "uncomment once prim 189 is in VM"
	^ self withArgs: {arg1. arg2. arg3} executeMethod: compiledMethod! !

!Object methodsFor: 'message handling' stamp: 'md 1/20/2006 16:28'!
with: arg1 with: arg2 with: arg3 with: arg4 executeMethod: compiledMethod
	"Execute compiledMethod against the receiver and arg1, arg2, arg3, & arg4"

	"<primitive: 189>" "uncomment once prim 189 is in VM"
	^ self withArgs: {arg1. arg2. arg3. arg4} executeMethod: compiledMethod! !


!Object methodsFor: 'objects from disk' stamp: 'tk 4/8/1999 12:46'!
comeFullyUpOnReload: smartRefStream
	"Normally this read-in object is exactly what we want to store. 7/26/96 tk"

	^ self! !

!Object methodsFor: 'objects from disk' stamp: 'RAA 12/20/2000 16:51'!
convertToCurrentVersion: varDict refStream: smartRefStrm

	"subclasses should implement if they wish to convert old instances to modern ones"! !

!Object methodsFor: 'objects from disk' stamp: 'tk 11/29/2004 15:04'!
fixUponLoad: aProject seg: anImageSegment
	"change the object due to conventions that have changed on
the project level.  (sent to all objects in the incoming project).
Specific classes should reimplement this."! !

!Object methodsFor: 'objects from disk' stamp: 'RAA 1/10/2001 14:02'!
indexIfCompact

	^0		"helps avoid a #respondsTo: in publishing"! !

!Object methodsFor: 'objects from disk' stamp: 'tk 2/24/1999 11:08'!
objectForDataStream: refStrm
    "Return an object to store on an external data stream."

    ^ self! !

!Object methodsFor: 'objects from disk' stamp: 'tk 4/8/1999 12:05'!
readDataFrom: aDataStream size: varsOnDisk
	"Fill in the fields of self based on the contents of aDataStream.  Return self.
	 Read in the instance-variables written by Object>>storeDataOn:.
	 NOTE: This method must send beginReference: before reading any objects from aDataStream that might reference it.
	 Allow aDataStream to have fewer inst vars.  See SmartRefStream."
	| cntInstVars cntIndexedVars |

	cntInstVars _ self class instSize.
	self class isVariable
		ifTrue: [cntIndexedVars _ varsOnDisk - cntInstVars.
				cntIndexedVars < 0 ifTrue: [
					self error: 'Class has changed too much.  Define a convertxxx method']]
		ifFalse: [cntIndexedVars _ 0.
				cntInstVars _ varsOnDisk]. 	"OK if fewer than now"

	aDataStream beginReference: self.
	1 to: cntInstVars do:
		[:i | self instVarAt: i put: aDataStream next].
	1 to: cntIndexedVars do:
		[:i | self basicAt: i put: aDataStream next].
	"Total number read MUST be equal to varsOnDisk!!"
	^ self	"If we ever return something other than self, fix calls 
			on (super readDataFrom: aDataStream size: anInteger)"! !

!Object methodsFor: 'objects from disk' stamp: 'CdG 10/17/2005 20:32'!
saveOnFile
	"Ask the user for a filename and save myself on a SmartReferenceStream file.  Writes out the version and class structure.  The file is fileIn-able.  Does not file out the class of the object.  tk 6/26/97 13:48"

	| aFileName fileStream |
	aFileName := self class name asFileName.	"do better?"
	aFileName := UIManager default 
				request: 'File name?' translated initialAnswer: aFileName.
	aFileName size == 0 ifTrue: [^ Beeper beep].

	fileStream := FileStream newFileNamed: aFileName asFileName.
	fileStream fileOutClass: nil andObject: self.! !

!Object methodsFor: 'objects from disk' stamp: 'tk 8/9/2001 15:40'!
storeDataOn: aDataStream
	"Store myself on a DataStream.  Answer self.  This is a low-level DataStream/ReferenceStream method. See also objectToStoreOnDataStream.  NOTE: This method must send 'aDataStream beginInstance:size:' and then (nextPut:/nextPutWeak:) its subobjects.  readDataFrom:size: reads back what we write here."
	| cntInstVars cntIndexedVars |

	cntInstVars _ self class instSize.
	cntIndexedVars _ self basicSize.
	aDataStream
		beginInstance: self class
		size: cntInstVars + cntIndexedVars.
	1 to: cntInstVars do:
		[:i | aDataStream nextPut: (self instVarAt: i)].

	"Write fields of a variable length object.  When writing to a dummy 
		stream, don't bother to write the bytes"
	((aDataStream byteStream class == DummyStream) and: [self class isBits]) ifFalse: [
		1 to: cntIndexedVars do:
			[:i | aDataStream nextPut: (self basicAt: i)]].
! !


!Object methodsFor: 'parts bin' stamp: 'sw 10/24/2001 16:34'!
descriptionForPartsBin
	"If the receiver is a member of a class that would like to be represented in a parts bin, answer the name by which it should be known, and a documentation string to be provided, for example, as balloon help.  When the 'nativitySelector' is sent to the 'globalReceiver', it is expected that some kind of Morph will result.  The parameters used in the implementation below are for documentation purposes only!!"

	^ DescriptionForPartsBin
		formalName: 'PutFormalNameHere'
		categoryList: #(PutACategoryHere MaybePutAnotherCategoryHere)
		documentation: 'Put the balloon help here'
		globalReceiverSymbol: #PutAGlobalHere
		nativitySelector: #PutASelectorHere! !


!Object methodsFor: 'printing' stamp: 'di 6/20/97 08:57'!
fullPrintString
	"Answer a String whose characters are a description of the receiver."

	^ String streamContents: [:s | self printOn: s]! !

!Object methodsFor: 'printing'!
isLiteral
	"Answer whether the receiver has a literal text form recognized by the 
	compiler."

	^false! !

!Object methodsFor: 'printing' stamp: 'sma 6/1/2000 09:28'!
longPrintOn: aStream
	"Append to the argument, aStream, the names and values of all 
	of the receiver's instance variables."

	self class allInstVarNames doWithIndex:
		[:title :index |
		aStream nextPutAll: title;
		 nextPut: $:;
		 space;
		 tab;
		 print: (self instVarAt: index);
		 cr]! !

!Object methodsFor: 'printing' stamp: 'tk 10/19/2001 11:18'!
longPrintOn: aStream limitedTo: sizeLimit indent: indent
	"Append to the argument, aStream, the names and values of all of the receiver's instance variables.  Limit is the length limit for each inst var."

	self class allInstVarNames doWithIndex:
		[:title :index |
		indent timesRepeat: [aStream tab].
		aStream nextPutAll: title;
		 nextPut: $:;
		 space;
		 tab;
		 nextPutAll: 
			((self instVarAt: index) printStringLimitedTo: (sizeLimit -3 -title size max: 1));
		 cr]! !

!Object methodsFor: 'printing' stamp: 'tk 10/16/2001 19:41'!
longPrintString
	"Answer a String whose characters are a description of the receiver."
	
	| str |
	str _ String streamContents: [:aStream | self longPrintOn: aStream].
	"Objects without inst vars should return something"
	^ str isEmpty ifTrue: [self printString, String cr] ifFalse: [str]! !

!Object methodsFor: 'printing' stamp: 'BG 11/7/2004 13:39'!
longPrintStringLimitedTo: aLimitValue
	"Answer a String whose characters are a description of the receiver."
	
	| str |
	str _ String streamContents: [:aStream | self longPrintOn: aStream limitedTo: aLimitValue indent: 0].
	"Objects without inst vars should return something"
	^ str isEmpty ifTrue: [self printString, String cr] ifFalse: [str]! !

!Object methodsFor: 'printing' stamp: 'sw 3/7/2001 13:14'!
nominallyUnsent: aSelectorSymbol
	"From within the body of a method which is not formally sent within the system, but which you intend to have remain in the system (for potential manual invocation, or for documentation, or perhaps because it's sent by commented-out-code that you anticipate uncommenting out someday, send this message, with the selector itself as the argument.

This will serve two purposes:

	(1)  The method will not be returned by searches for unsent selectors (because it, in a manner of speaking, sends itself).
	(2)	You can locate all such methods by browsing senders of #nominallyUnsent:"

	false ifTrue: [self flag: #nominallyUnsent:]    "So that this method itself will appear to be sent"
! !

!Object methodsFor: 'printing' stamp: 'sma 6/1/2000 09:31'!
printOn: aStream
	"Append to the argument, aStream, a sequence of characters that  
	identifies the receiver."

	| title |
	title _ self class name.
	aStream
		nextPutAll: (title first isVowel ifTrue: ['an '] ifFalse: ['a ']);
		nextPutAll: title! !

!Object methodsFor: 'printing' stamp: 'sma 6/1/2000 09:22'!
printString
	"Answer a String whose characters are a description of the receiver. 
	If you want to print without a character limit, use fullPrintString."

	^ self printStringLimitedTo: 50000! !

!Object methodsFor: 'printing' stamp: 'tk 5/7/1999 16:20'!
printStringLimitedTo: limit
	"Answer a String whose characters are a description of the receiver.
	If you want to print without a character limit, use fullPrintString."
	| limitedString |
	limitedString _ String streamContents: [:s | self printOn: s] limitedTo: limit.
	limitedString size < limit ifTrue: [^ limitedString].
	^ limitedString , '...etc...'! !

!Object methodsFor: 'printing' stamp: 'MPW 1/1/1901 00:30'!
propertyList
	"Answer a String whose characters are a property-list description of the receiver."

	^ PropertyListEncoder process:self.
! !

!Object methodsFor: 'printing' stamp: 'sw 10/17/2000 11:16'!
reportableSize
	"Answer a string that reports the size of the receiver -- useful for showing in a list view, for example"

	^ (self basicSize + self class instSize) printString! !

!Object methodsFor: 'printing'!
storeOn: aStream 
	"Append to the argument aStream a sequence of characters that is an 
	expression whose evaluation creates an object similar to the receiver."

	aStream nextPut: $(.
	self class isVariable
		ifTrue: [aStream nextPutAll: '(', self class name, ' basicNew: ';
					store: self basicSize;
					nextPutAll: ') ']
		ifFalse: [aStream nextPutAll: self class name, ' basicNew'].
	1 to: self class instSize do:
		[:i |
		aStream nextPutAll: ' instVarAt: ';
			store: i;
			nextPutAll: ' put: ';
			store: (self instVarAt: i);
			nextPut: $;].
	1 to: self basicSize do:
		[:i |
		aStream nextPutAll: ' basicAt: ';
			store: i;
			nextPutAll: ' put: ';
			store: (self basicAt: i);
			nextPut: $;].
	aStream nextPutAll: ' yourself)'
! !

!Object methodsFor: 'printing' stamp: 'di 6/20/97 09:12'!
storeString
	"Answer a String representation of the receiver from which the receiver 
	can be reconstructed."

	^ String streamContents: [:s | self storeOn: s]! !

!Object methodsFor: 'printing' stamp: 'sw 5/2/1998 13:55'!
stringForReadout
	^ self stringRepresentation! !

!Object methodsFor: 'printing'!
stringRepresentation
	"Answer a string that represents the receiver.  For most objects this is simply its printString, but for strings themselves, it's themselves.  6/12/96 sw"

	^ self printString ! !


!Object methodsFor: 'scripting' stamp: 'ar 3/17/2001 20:11'!
adaptedToWorld: aWorld
	"If I refer to a world or a hand, return the corresponding items in the new world."
	^self! !

!Object methodsFor: 'scripting' stamp: 'sw 3/10/2000 13:57'!
defaultFloatPrecisionFor: aGetSelector
	"Answer a number indicating the default float precision to be used in a numeric readout for which the receiver is the model."

	^ 1! !

!Object methodsFor: 'scripting' stamp: 'RAA 3/9/2001 17:08'!
evaluateUnloggedForSelf: aCodeString

	^Compiler evaluate:
		aCodeString
		for: self
		logged: false! !

!Object methodsFor: 'scripting' stamp: 'yo 12/25/2003 16:43'!
methodInterfacesForCategory: aCategorySymbol inVocabulary: aVocabulary limitClass: aLimitClass
	"Return a list of methodInterfaces for the receiver in the given category, given a vocabulary.  aCategorySymbol is the inherent category symbol, not necessarily the wording as expressed in the vocabulary."

	| categorySymbol |
	categorySymbol _ aCategorySymbol asSymbol.

	(categorySymbol == ScriptingSystem nameForInstanceVariablesCategory) ifTrue: [
		"user-defined instance variables"
		^ self methodInterfacesForInstanceVariablesCategoryIn: aVocabulary].
	(categorySymbol == ScriptingSystem nameForScriptsCategory) ifTrue: [
		"user-defined scripts"
		^ self methodInterfacesForScriptsCategoryIn: aVocabulary].
	"all others"
	^ self usableMethodInterfacesIn: (aVocabulary methodInterfacesInCategory: categorySymbol
		forInstance: self
		ofClass: self class
		limitClass: aLimitClass)
! !

!Object methodsFor: 'scripting' stamp: 'sw 8/3/2001 13:54'!
methodInterfacesForInstanceVariablesCategoryIn: aVocabulary
	"Return a collection of methodInterfaces for the instance-variables category.  The vocabulary parameter, at present anyway, is not used.  And for non-players, the method is at present vacuous in any case"

	^  OrderedCollection new! !

!Object methodsFor: 'scripting' stamp: 'sw 8/3/2001 13:53'!
methodInterfacesForScriptsCategoryIn: aVocabulary
	"Answer a list of method interfaces for the category #scripts, as seen in a viewer or other tool.  The vocabulary argument is not presently used.  Also, at present, only Players really do anyting interesting here."

	^ OrderedCollection new! !

!Object methodsFor: 'scripting' stamp: 'RAA 2/16/2001 19:37'!
selfWrittenAsIll

	^self! !

!Object methodsFor: 'scripting' stamp: 'RAA 2/16/2001 19:38'!
selfWrittenAsIm

	^self! !

!Object methodsFor: 'scripting' stamp: 'RAA 2/16/2001 19:37'!
selfWrittenAsMe

	^self! !

!Object methodsFor: 'scripting' stamp: 'RAA 2/16/2001 19:37'!
selfWrittenAsMy

	^self! !

!Object methodsFor: 'scripting' stamp: 'RAA 2/16/2001 19:38'!
selfWrittenAsThis

	^self! !


!Object methodsFor: 'scripts-kernel' stamp: 'nk 10/14/2004 10:55'!
universalTilesForGetterOf: aMethodInterface
	"Return universal tiles for a getter on the given method interface."

	| ms argTile argArray itsSelector |
	itsSelector _ aMethodInterface selector.
	argArray _ #().

	"Four gratuituous special cases..."

	(itsSelector == #color:sees:) ifTrue:
		[argTile _ ScriptingSystem tileForArgType: #Color.
		argArray _ Array with: argTile colorSwatch color with: argTile colorSwatch color copy].

	itsSelector == #seesColor: ifTrue:
		[argTile _ ScriptingSystem tileForArgType: #Color.
		argArray _  Array with: argTile colorSwatch color].

	(#(touchesA: overlaps: overlapsAny:) includes: itsSelector) ifTrue:
		[argTile _ ScriptingSystem tileForArgType: #Player.
		argArray _ Array with: argTile actualObject].

	ms _ MessageSend receiver: self selector: itsSelector arguments: argArray.
	^ ms asTilesIn: self class globalNames: (self class officialClass ~~ CardPlayer)
			"For CardPlayers, use 'self'.  For others, name it, and use its name."! !

!Object methodsFor: 'scripts-kernel' stamp: 'tk 9/28/2001 13:30'!
universalTilesForInterface: aMethodInterface
	"Return universal tiles for the given method interface.  Record who self is."

	| ms argTile itsSelector aType argList |
	itsSelector _ aMethodInterface selector.
	argList _ OrderedCollection new.
	aMethodInterface argumentVariables doWithIndex:
		[:anArgumentVariable :anIndex | 
			argTile _ ScriptingSystem tileForArgType: (aType _ aMethodInterface typeForArgumentNumber: anIndex).
			argList add: (aType == #Player 
				ifTrue: [argTile actualObject]
				ifFalse: [argTile literal]).	"default value for each type"].

	ms _ MessageSend receiver: self selector: itsSelector arguments: argList asArray.
	^ ms asTilesIn: self class globalNames: (self class officialClass ~~ CardPlayer)
			"For CardPlayers, use 'self'.  For others, name it, and use its name."! !


!Object methodsFor: 'self evaluating' stamp: 'sd 7/31/2005 21:47'!
isSelfEvaluating
	^ self isLiteral! !


!Object methodsFor: 'system primitives'!
asOop
	"Primitive. Answer a SmallInteger whose value is half of the receiver's 
	object pointer (interpreting object pointers as 16-bit signed quantities). 
	Fail if the receiver is a SmallInteger. Essential. See Object documentation 
	whatIsAPrimitive."

	<primitive: 75>
	self primitiveFailed! !

!Object methodsFor: 'system primitives' stamp: 'di 1/9/1999 15:19'!
becomeForward: otherObject 
	"Primitive. All variables in the entire system that used to point
	to the receiver now point to the argument.
	Fails if either argument is a SmallInteger."

	(Array with: self)
		elementsForwardIdentityTo:
			(Array with: otherObject)! !

!Object methodsFor: 'system primitives' stamp: 'zz 3/3/2004 23:53'!
becomeForward: otherObject copyHash: copyHash
	"Primitive. All variables in the entire system that used to point to the receiver now point to the argument.
	If copyHash is true, the argument's identity hash bits will be set to those of the receiver.
	Fails if either argument is a SmallInteger."

	(Array with: self)
		elementsForwardIdentityTo:
			(Array with: otherObject)
				copyHash: copyHash! !

!Object methodsFor: 'system primitives' stamp: 'sw 10/16/2000 10:59'!
className
	"Answer a string characterizing the receiver's class, for use in list views for example"

	^ self class name asString! !

!Object methodsFor: 'system primitives' stamp: 'sw 10/16/2000 11:04'!
creationStamp
	"Answer a string which reports the creation particulars of the receiver.  Intended perhaps for list views, but this is presently a feature not easily accessible"

	^ '<no creation stamp>'! !

!Object methodsFor: 'system primitives'!
instVarAt: index 
	"Primitive. Answer a fixed variable in an object. The numbering of the 
	variables corresponds to the named instance variables. Fail if the index 
	is not an Integer or is not the index of a fixed variable. Essential. See 
	Object documentation whatIsAPrimitive."

	<primitive: 73>
	"Access beyond fixed variables."
	^self basicAt: index - self class instSize		! !

!Object methodsFor: 'system primitives'!
instVarAt: anInteger put: anObject 
	"Primitive. Store a value into a fixed variable in the receiver. The 
	numbering of the variables corresponds to the named instance variables. 
	Fail if the index is not an Integer or is not the index of a fixed variable. 
	Answer the value stored as the result. Using this message violates the 
	principle that each object has sovereign control over the storing of 
	values into its instance variables. Essential. See Object documentation 
	whatIsAPrimitive."

	<primitive: 74>
	"Access beyond fixed fields"
	^self basicAt: anInteger - self class instSize put: anObject! !

!Object methodsFor: 'system primitives' stamp: 'sw 10/16/2000 11:09'!
instVarNamed: aString
	"Return the value of the instance variable in me with that name.  Slow and unclean, but very useful. "

	^ self instVarAt: (self class allInstVarNames indexOf: aString asString)


! !

!Object methodsFor: 'system primitives' stamp: 'sw 10/16/2000 11:10'!
instVarNamed: aString put: aValue
	"Store into the value of the instance variable in me of that name.  Slow and unclean, but very useful. "

	^ self instVarAt: (self class allInstVarNames indexOf: aString asString) put: aValue
! !

!Object methodsFor: 'system primitives' stamp: 'sw 10/17/2000 11:12'!
oopString
	"Answer a string that represents the oop of the receiver"

	^ self asOop printString! !

!Object methodsFor: 'system primitives' stamp: 'ar 3/2/2001 01:34'!
primitiveChangeClassTo: anObject
	"Primitive. Change the class of the receiver into the class of the argument given that the format of the receiver matches the format of the argument's class. Fail if receiver or argument are SmallIntegers, or the receiver is an instance of a compact class and the argument isn't, or when the argument's class is compact and the receiver isn't, or when the format of the receiver is different from the format of the argument's class, or when the arguments class is fixed and the receiver's size differs from the size that an instance of the argument's class should have.
	Note: The primitive will fail in most cases that you think might work. This is mostly because of a) the difference between compact and non-compact classes, and b) because of differences in the format. As an example, '(Array new: 3) primitiveChangeClassTo: Morph basicNew' would fail for three of the reasons mentioned above. Array is compact, Morph is not (failure #1). Array is variable and Morph is fixed (different format - failure #2). Morph is a fixed-field-only object and the array is too short (failure #3).
	The facility is really provided for certain, very specific applications (mostly related to classes changing shape) and not for casual use."

	<primitive: 115>
	self primitiveFailed! !

!Object methodsFor: 'system primitives' stamp: 'di 3/27/1999 12:21'!
rootStubInImageSegment: imageSegment

	^ ImageSegmentRootStub new
		xxSuperclass: nil
		format: nil
		segment: imageSegment! !

!Object methodsFor: 'system primitives'!
someObject
	"Primitive. Answer the first object in the enumeration of all
	 objects."

	<primitive: 138>
	self primitiveFailed.! !


!Object methodsFor: 'testing' stamp: 'sw 9/26/2001 11:58'!
basicType
	"Answer a symbol representing the inherent type of the receiver"

	^ #Object! !

!Object methodsFor: 'testing' stamp: 'sw 5/3/2001 16:19'!
beViewed
	"Open up a viewer on the receiver.  The Presenter is invited to decide just how to present this viewer"

	self uniqueNameForReference.  "So the viewer will have something nice to refer to"
	self presenter viewObject: self! !

!Object methodsFor: 'testing' stamp: 'sw 10/16/2000 11:01'!
costumes
	"Answer a list of costumes associated with the receiver.  The appearance of this method in class Object serves only as a backstop, probably only transitionally"

	^ nil! !

!Object methodsFor: 'testing' stamp: 'sw 1/12/98 18:09'!
haltIfNil! !

!Object methodsFor: 'testing' stamp: 'md 1/20/2006 17:09'!
hasLiteralSuchThat: testBlock
	"This is the end of the imbedded structure path so return false."

	^ false! !

!Object methodsFor: 'testing' stamp: 'md 1/20/2006 17:10'!
hasLiteralThorough: literal
	"Answer true if literal is identical to any literal in this array, even if imbedded in further structures.  This is the end of the imbedded structure path so return false."

	^ false! !

!Object methodsFor: 'testing' stamp: 'sw 1/30/2001 22:24'!
haveFullProtocolBrowsed
	"Open up a Lexicon on the receiver"

	^ self haveFullProtocolBrowsedShowingSelector: nil

	"(2@3) haveFullProtocolBrowsed"
! !

!Object methodsFor: 'testing' stamp: 'ar 9/27/2005 21:04'!
haveFullProtocolBrowsedShowingSelector: aSelector
	"Open up a Lexicon on the receiver, having it open up showing aSelector, which may be nil"

	| aBrowser |
	aBrowser := (Smalltalk at: #InstanceBrowser ifAbsent:[^nil]) new useVocabulary: Vocabulary fullVocabulary.
	aBrowser openOnObject: self inWorld: ActiveWorld showingSelector: aSelector

	"(2@3) haveFullProtocolBrowsed"! !

!Object methodsFor: 'testing' stamp: 'md 7/30/2005 21:21'!
isArray
	^false! !

!Object methodsFor: 'testing' stamp: 'ar 7/9/1999 18:18'!
isBehavior
	"Return true if the receiver is a behavior.
	Note: Do not override in any class except behavior."
	^false! !

!Object methodsFor: 'testing' stamp: 'ajh 1/21/2003 13:15'!
isBlock

	^ false! !

!Object methodsFor: 'testing' stamp: 'md 11/21/2003 12:14'!
isBlockClosure

	^ false! !

!Object methodsFor: 'testing' stamp: 'yo 8/28/2002 13:41'!
isCharacter

	^ false.
! !

!Object methodsFor: 'testing' stamp: 'ar 8/17/1999 19:43'!
isCollection
	"Return true if the receiver is some sort of Collection and responds to basic collection messages such as #size and #do:"
	^false! !

!Object methodsFor: 'testing'!
isColor
	"Answer true if receiver is a Color. False by default."

	^ false
! !

!Object methodsFor: 'testing' stamp: 'nk 4/17/2004 19:43'!
isColorForm
	^false! !

!Object methodsFor: 'testing' stamp: 'md 11/21/2003 12:14'!
isCompiledMethod

	^ false! !

!Object methodsFor: 'testing' stamp: 'mk 10/27/2003 17:33'!
isComplex
	"Answer true if receiver is a Complex number. False by default."

	^ false
! !

!Object methodsFor: 'testing' stamp: 'md 8/11/2005 16:45'!
isDictionary
	^false! !

!Object methodsFor: 'testing' stamp: 'di 11/9/1998 09:38'!
isFloat
	"Overridden to return true in Float, natch"
	^ false! !

!Object methodsFor: 'testing' stamp: 'ar 10/30/2000 23:22'!
isForm
	^false! !

!Object methodsFor: 'testing' stamp: 'len 1/13/98 21:18'!
isFraction
	"Answer true if the receiver is a Fraction."

	^ false! !

!Object methodsFor: 'testing' stamp: 'rhi 8/14/2003 08:51'!
isHeap

	^ false! !

!Object methodsFor: 'testing'!
isInteger
	"Overridden to return true in Integer."

	^ false! !

!Object methodsFor: 'testing' stamp: 'rhi 8/12/2003 09:52'!
isInterval

	^ false! !

!Object methodsFor: 'testing' stamp: 'nk 4/25/2002 08:04'!
isMessageSend
	^false
! !

!Object methodsFor: 'testing' stamp: 'md 2/19/2006 11:24'!
isMethodProperties
	^false! !

!Object methodsFor: 'testing'!
isMorph

	^ false! !

!Object methodsFor: 'testing' stamp: 'ar 9/13/2000 15:37'!
isMorphicEvent
	^false! !

!Object methodsFor: 'testing' stamp: 'gm 2/22/2003 12:56'!
isMorphicModel
	"Return true if the receiver is a morphic model"
	^false
! !

!Object methodsFor: 'testing'!
isNumber
	"Overridden to return true in Number, natch"
	^ false! !

!Object methodsFor: 'testing' stamp: 'di 11/6/1998 08:04'!
isPoint
	"Overridden to return true in Point."

	^ false! !

!Object methodsFor: 'testing' stamp: 'ikp 9/26/97 14:45'!
isPseudoContext
	^false! !

!Object methodsFor: 'testing' stamp: 'md 10/2/2005 21:52'!
isRectangle
	^false! !

!Object methodsFor: 'testing' stamp: 'nk 6/14/2004 16:49'!
isSketchMorph
	^false! !

!Object methodsFor: 'testing' stamp: 'ar 12/23/1999 15:43'!
isStream
	"Return true if the receiver responds to the stream protocol"
	^false
! !

!Object methodsFor: 'testing' stamp: 'sma 6/15/2000 15:48'!
isString
	"Overridden to return true in String, natch"
	^ false! !

!Object methodsFor: 'testing' stamp: 'md 4/30/2003 15:30'!
isSymbol
	^ false ! !

!Object methodsFor: 'testing' stamp: 'jam 3/9/2003 15:10'!
isSystemWindow
"answer whatever the receiver is a SystemWindow"
	^ false! !

!Object methodsFor: 'testing'!
isText
	^ false! !

!Object methodsFor: 'testing' stamp: 'pmm 7/6/2006 20:46'!
isTrait
	"Return true if the receiver is a trait.
	Note: Do not override in any class except TraitBehavior."
	^false! !

!Object methodsFor: 'testing' stamp: 'tk 10/21/97 12:45'!
isTransparent
	^ false! !

!Object methodsFor: 'testing' stamp: 'ar 8/14/2001 23:19'!
isVariableBinding
	"Return true if I represent a literal variable binding"
	^false
	! !

!Object methodsFor: 'testing' stamp: 'ls 7/14/1998 21:45'!
isWebBrowser
	"whether this object is a web browser.  See class: Scamper"
	^false! !

!Object methodsFor: 'testing' stamp: 'sw 10/27/2000 06:58'!
knownName
	"If a formal name has been handed out for this object, answer it, else nil"
	
	^ Preferences capitalizedReferences
		ifTrue:
			[References keyAtValue: self ifAbsent: [nil]]
		ifFalse:
			[nil]! !

!Object methodsFor: 'testing' stamp: 'sw 9/27/96'!
name
	"Answer a name for the receiver.  This is used generically in the title of certain inspectors, such as the referred-to inspector, and specificially by various subsystems.  By default, we let the object just print itself out..  "

	^ self printString! !

!Object methodsFor: 'testing' stamp: 'sw 11/19/2001 13:28'!
nameForViewer
	"Answer a name to be shown in a Viewer that is viewing the receiver"

	| aName |
	(aName _ self uniqueNameForReferenceOrNil) ifNotNil: [^ aName].
	(aName _ self knownName) ifNotNil: [^ aName].

	^ [(self asString copyWithout: Character cr) truncateTo:  27] ifError:
		[:msg :rcvr | ^ self class name printString]! !

!Object methodsFor: 'testing'!
notNil
	"Coerces nil to false and everything else to true."

	^true! !

!Object methodsFor: 'testing' stamp: 'tk 9/6/2001 19:15'!
openInstanceBrowserWithTiles
	"Open up an instance browser on me with tiles as the code type, and with the search level as desired."

	| aBrowser |
	aBrowser _ InstanceBrowser new.
	aBrowser useVocabulary: Vocabulary fullVocabulary.
	aBrowser limitClass: self class.
	aBrowser contentsSymbol: #tiles.		"preset it to make extra buttons (tile menus)"
	aBrowser openOnObject: self inWorld: ActiveWorld showingSelector: nil.
	aBrowser contentsSymbol: #source.
	aBrowser toggleShowingTiles.

	"
(2@3) openInstanceBrowserWithTiles.
WatchMorph new openInstanceBrowserWithTiles
"! !

!Object methodsFor: 'testing' stamp: 'tk 7/28/2005 04:50'!
renameInternal: newName 
	"Change the internal name (because of a conflict) but leave the external name unchanged.  Change Player class name, but do not change the names that appear in tiles.  Any object that might be pointed to in the References dictionary might get this message sent to it upon reload"

	^ nil	"caller will renameTo:.  new name may be different"! !

!Object methodsFor: 'testing' stamp: 'sw 2/27/2002 14:55'!
renameTo: newName
	"If the receiver has an inherent idea about its own name, it should take action here.  Any object that might be pointed to in the References dictionary might get this message sent to it upon reload"! !

!Object methodsFor: 'testing' stamp: 'sw 1/18/2001 13:43'!
showDiffs
	"Answer whether the receiver, serving as the model of a text-bearing entity, is 'showing differences' -- if it is, the editor may wish to show special feedback"

	^ false! !

!Object methodsFor: 'testing' stamp: 'sw 10/20/1999 14:52'!
stepAt: millisecondClockValue in: aWindow

	^ self stepIn: aWindow! !

!Object methodsFor: 'testing' stamp: 'sw 10/19/1999 08:16'!
stepIn: aWindow

	^ self step! !

!Object methodsFor: 'testing' stamp: 'sw 10/19/1999 08:21'!
stepTime
	
	^ 1000 "milliseconds -- default backstop for objects serving as models of system windows"! !

!Object methodsFor: 'testing' stamp: 'sw 10/19/1999 08:22'!
stepTimeIn: aSystemWindow
	
	^ 1000 "milliseconds -- default backstop for objects serving as models of system windows"! !

!Object methodsFor: 'testing' stamp: 'sw 5/3/2001 18:22'!
vocabularyDemanded
	"Answer a vocabulary that the receiver insists be used when it is looked at in a Viewer.  This allows specific classes to insist on specific custom vocabularies"

	^ nil! !

!Object methodsFor: 'testing' stamp: 'sw 11/13/2001 07:26'!
wantsDiffFeedback
	"Answer whether the receiver, serving as the model of a text-bearing entity, would like for 'diffs' green pane-border feedback to be shown"

	^ false! !

!Object methodsFor: 'testing' stamp: 'di 1/8/1999 15:04'!
wantsSteps
	"Overridden by morphic classes whose instances want to be stepped,
	or by model classes who want their morphic views to be stepped."

	^ false! !

!Object methodsFor: 'testing' stamp: 'sw 10/19/1999 08:26'!
wantsStepsIn: aSystemWindow
	
	^ self wantsSteps! !


!Object methodsFor: 'thumbnail' stamp: 'dgd 9/25/2004 23:17'!
iconOrThumbnailOfSize: aNumberOrPoint 
	"Answer an appropiate form to represent the receiver"
	^ nil! !


!Object methodsFor: 'translation support'!
inline: inlineFlag
	"For translation only; noop when running in Smalltalk."! !

!Object methodsFor: 'translation support'!
var: varSymbol declareC: declString
	"For translation only; noop when running in Smalltalk."! !


!Object methodsFor: 'undo' stamp: 'di 9/11/2000 20:32'!
capturedState
	"May be overridden in subclasses."

	^ self shallowCopy
! !

!Object methodsFor: 'undo' stamp: 'di 9/11/2000 20:29'!
commandHistory
	"Return the command history for the receiver"
	| w |
	(w _ self currentWorld) ifNotNil: [^ w commandHistory].
	^ CommandHistory new. "won't really record anything but prevent breaking things"! !

!Object methodsFor: 'undo' stamp: 'di 12/12/2000 15:01'!
purgeAllCommands
	"Purge all commands for this object"
	Preferences useUndo ifFalse: [^ self]. "get out quickly"
	self commandHistory purgeAllCommandsSuchThat: [:cmd | cmd undoTarget == self].
! !

!Object methodsFor: 'undo' stamp: 'di 9/12/2000 08:15'!
redoFromCapturedState: st 
	"May be overridden in subclasses.  See also capturedState"

	self undoFromCapturedState: st  "Simple cases are symmetric"
! !

!Object methodsFor: 'undo' stamp: 'sw 11/16/2000 14:42'!
refineRedoTarget: target selector: aSymbol arguments: arguments in: refineBlock 
	"Any object can override this method to refine its redo specification"

	^ refineBlock
		value: target
		value: aSymbol
		value: arguments! !

!Object methodsFor: 'undo' stamp: 'sw 11/16/2000 14:42'!
refineUndoTarget: target selector: aSymbol arguments: arguments in: refineBlock 
	"Any object can override this method to refine its undo specification"

	^ refineBlock
		value: target
		value: aSymbol
		value: arguments! !

!Object methodsFor: 'undo' stamp: 'di 9/11/2000 20:30'!
rememberCommand: aCommand
	"Remember the given command for undo"
	Preferences useUndo ifFalse: [^ self]. "get out quickly"
	^ self commandHistory rememberCommand: aCommand! !

!Object methodsFor: 'undo' stamp: 'di 9/11/2000 20:30'!
rememberUndoableAction: actionBlock named: caption
	| cmd result |
	cmd _ Command new cmdWording: caption.
	cmd undoTarget: self selector: #undoFromCapturedState: argument: self capturedState.
	result _ actionBlock value.
	cmd redoTarget: self selector: #redoFromCapturedState: argument: self capturedState.
	self rememberCommand: cmd.
	^ result! !

!Object methodsFor: 'undo' stamp: 'di 9/11/2000 20:32'!
undoFromCapturedState: st 
	"May be overridden in subclasses.  See also capturedState"

	self copyFrom: st
! !


!Object methodsFor: 'updating'!
changed
	"Receiver changed in a general way; inform all the dependents by 
	sending each dependent an update: message."

	self changed: self! !

!Object methodsFor: 'updating'!
changed: aParameter 
	"Receiver changed. The change is denoted by the argument aParameter. 
	Usually the argument is a Symbol that is part of the dependent's change 
	protocol. Inform all of the dependents."

	self dependents do: [:aDependent | aDependent update: aParameter]! !

!Object methodsFor: 'updating' stamp: 'nk 2/17/2004 11:12'!
changed: anAspect with: anObject
	"Receiver changed. The change is denoted by the argument anAspect. 
	Usually the argument is a Symbol that is part of the dependent's change 
	protocol. Inform all of the dependents. Also pass anObject for additional information."

	self dependents do: [:aDependent | aDependent update: anAspect with: anObject]! !

!Object methodsFor: 'updating' stamp: 'sw 10/12/1999 18:15'!
handledListVerification
	"When a self-updating PluggableListMorph lazily checks to see the state of affairs, it first gives its model an opportunity to handle the list verification itself (this is appropriate for some models, such as VersionsBrowser); if a list's model has indeed handled things itself, it returns true here"

	^ false! !

!Object methodsFor: 'updating' stamp: 'sw 10/31/1999 00:15'!
noteSelectionIndex: anInteger for: aSymbol
	"backstop"! !

!Object methodsFor: 'updating'!
okToChange
	"Allows a controller to ask this of any model"
	^ true! !

!Object methodsFor: 'updating' stamp: 'sw 10/19/1999 14:39'!
updateListsAndCodeIn: aWindow
	self canDiscardEdits ifFalse: [^ self].
	aWindow updatablePanes do: [:aPane | aPane verifyContents]! !

!Object methodsFor: 'updating' stamp: 'sma 2/29/2000 20:05'!
update: aParameter 
	"Receive a change notice from an object of whom the receiver is a 
	dependent. The default behavior is to do nothing; a subclass might want 
	to change itself in some way."

	^ self! !

!Object methodsFor: 'updating' stamp: 'nk 2/17/2004 11:13'!
update: anAspect with: anObject
	"Receive a change notice from an object of whom the receiver is a 
	dependent. The default behavior is to call update:,
	which by default does nothing; a subclass might want 
	to change itself in some way."

	^ self update: anAspect! !

!Object methodsFor: 'updating' stamp: 'jm 8/20/1998 18:26'!
windowIsClosing
	"This message is used to inform a models that its window is closing. Most models do nothing, but some, such as the Debugger, must do some cleanup. Note that this mechanism must be used with care by models that support multiple views, since one view may be closed while others left open."
! !


!Object methodsFor: 'user interface' stamp: 'sw 10/4/1999 08:13'!
addModelItemsToWindowMenu: aMenu
	"aMenu is being constructed to be presented to the user in response to the user's pressing on the menu widget in the title bar of a morphic window.  Here, the model is given the opportunity to add any model-specific items to the menu, whose default target is the SystemWindow itself."! !

!Object methodsFor: 'user interface' stamp: 'sw 10/5/1998 14:39'!
addModelMenuItemsTo: aCustomMenu forMorph: aMorph hand: aHandMorph 
	"The receiver serves as the model for aMorph; a menu is being constructed for the morph, and here the receiver is able to add its own items"
! !

!Object methodsFor: 'user interface' stamp: 'sma 11/12/2000 11:43'!
asExplorerString
	^ self printString! !

!Object methodsFor: 'user interface' stamp: 'sw 7/13/1999 15:53'!
defaultBackgroundColor
	"Answer the color to be used as the base window color for a window whose model is an object of the receiver's class"
	
	^ Preferences windowColorFor: self class name! !

!Object methodsFor: 'user interface'!
defaultLabelForInspector
	"Answer the default label to be used for an Inspector window on the receiver."

	^ self class name! !

!Object methodsFor: 'user interface' stamp: 'RAA 7/10/2000 08:11'!
eToyStreamedRepresentationNotifying: aWidget

	| outData |
	[ outData _ SmartRefStream streamedRepresentationOf: self ] 
		on: ProgressInitiationException
		do: [ :ex | 
			ex sendNotificationsTo: [ :min :max :curr |
				aWidget ifNotNil: [aWidget flashIndicator: #working].
			].
		].
	^outData
! !

!Object methodsFor: 'user interface' stamp: 'ar 9/27/2005 20:29'!
explore
	^ToolSet explore: self! !

!Object methodsFor: 'user interface' stamp: 'sw 8/15/97 17:25'!
fullScreenSize
	"Answer the size to which a window displaying the receiver should be set"
	| adj |
	adj _ (3 * Preferences scrollBarWidth) @ 0.
	^ Rectangle origin: adj extent: (DisplayScreen actualScreenSize - adj)! !

!Object methodsFor: 'user interface' stamp: 'RAA 6/21/1999 11:27'!
hasContentsInExplorer

	^self basicSize > 0 or: [self class allInstVarNames isEmpty not]
! !

!Object methodsFor: 'user interface' stamp: 'rbb 3/1/2005 09:28'!
inform: aString
	"Display a message for the user to read and then dismiss. 6/9/96 sw"

	aString isEmptyOrNil ifFalse: [UIManager default inform: aString]! !

!Object methodsFor: 'user interface'!
initialExtent
	"Answer the desired extent for the receiver when a view on it is first opened on the screen. 
	5/22/96 sw: in the absence of any override, obtain from RealEstateAgent"

	^ RealEstateAgent standardWindowExtent! !

!Object methodsFor: 'user interface' stamp: 'ar 9/27/2005 20:30'!
inspectWithLabel: aLabel
	"Create and schedule an Inspector in which the user can examine the receiver's variables."
	^ToolSet inspect: self label: aLabel! !

!Object methodsFor: 'user interface' stamp: 'sw 6/12/2001 11:09'!
launchPartVia: aSelector
	"Obtain a morph by sending aSelector to self, and attach it to the morphic hand.  This provides a general protocol for parts bins"

	| aMorph |
	aMorph _ self perform: aSelector.
	aMorph setProperty: #beFullyVisibleAfterDrop toValue: true.
	aMorph openInHand! !

!Object methodsFor: 'user interface' stamp: 'sw 6/17/2004 01:47'!
launchPartVia: aSelector label: aString
	"Obtain a morph by sending aSelector to self, and attach it to the morphic hand.  This provides a general protocol for parts bins"

	| aMorph |
	aMorph _ self perform: aSelector.
	aMorph setNameTo: (ActiveWorld unusedMorphNameLike: aString).
	aMorph setProperty: #beFullyVisibleAfterDrop toValue: true.
	aMorph openInHand! !

!Object methodsFor: 'user interface' stamp: 'sw 10/16/2000 11:11'!
launchTileToRefer
	"Create a tile to reference the receiver, and attach it to the hand"

	self currentHand attachMorph: self tileToRefer! !

!Object methodsFor: 'user interface' stamp: 'di 5/11/1999 22:26'!
modelSleep
	"A window with me as model is being exited or collapsed or closed.
	Default response is no-op" ! !

!Object methodsFor: 'user interface' stamp: 'di 5/11/1999 22:01'!
modelWakeUp
	"A window with me as model is being entered or expanded.  Default response is no-op" ! !

!Object methodsFor: 'user interface' stamp: 'sw 10/16/1999 22:45'!
modelWakeUpIn: aWindow
	"A window with me as model is being entered or expanded.  Default response is no-op" 
	self modelWakeUp! !

!Object methodsFor: 'user interface' stamp: 'sw 3/8/1999 15:27'!
mouseUpBalk: evt
	"A button I own got a mouseDown, but the user moved out before letting up.  Certain kinds of objects (so-called 'radio buttons', for example, and other structures that must always have some selection, e.g. PaintBoxMorph) wish to take special action in this case; this default does nothing."
! !

!Object methodsFor: 'user interface' stamp: 'sw 8/22/97 13:14'!
newTileMorphRepresentative
	^ TileMorph new setLiteral: self! !

!Object methodsFor: 'user interface' stamp: 'jcg 11/1/2001 13:13'!
notYetImplemented
	self inform: 'Not yet implemented (', thisContext sender printString, ')'! !

!Object methodsFor: 'user interface' stamp: 'di 6/10/1998 15:06'!
windowReqNewLabel: labelString
	"My window's title has been edited.
	Return true if this is OK, and override for further behavior."

	^ true! !


!Object methodsFor: 'viewer' stamp: 'sw 10/16/2000 10:35'!
assureUniClass
	"If the receiver is not yet an instance of a uniclass, create a uniclass for it and make the receiver become an instance of that class."

	| anInstance |
	self belongsToUniClass ifTrue: [^ self].
	anInstance _ self class instanceOfUniqueClass.
	self become: (self as: anInstance class).
	^ anInstance! !

!Object methodsFor: 'viewer' stamp: 'sw 10/16/2000 10:41'!
belongsToUniClass
	"Answer whether the receiver belongs to a uniclass.  For the moment (this is not entirely satisfactory) this is precisely equated with the classname ending in a digit"

	^ self class name endsWithDigit! !

!Object methodsFor: 'viewer' stamp: 'sw 12/11/2000 15:37'!
browseOwnClassSubProtocol
	"Open up a ProtocolBrowser on the subprotocol of the receiver"

	ProtocolBrowser openSubProtocolForClass: self class
! !

!Object methodsFor: 'viewer' stamp: 'sw 8/4/2001 00:51'!
categoriesForViewer: aViewer
	"Answer a list of categories to offer in the given viewer"

	^ aViewer currentVocabulary categoryListForInstance: self ofClass: self class limitClass: aViewer limitClass! !

!Object methodsFor: 'viewer' stamp: 'sw 8/3/2001 22:08'!
categoriesForVocabulary: aVocabulary limitClass: aLimitClass
	"Answer a list of categories of methods for the receiver when using the given vocabulary, given that one considers only methods that are implemented not further away than aLimitClass"

	^ aVocabulary categoryListForInstance: self ofClass: self class limitClass: aLimitClass! !

!Object methodsFor: 'viewer' stamp: 'sw 10/25/2000 07:20'!
chooseNewNameForReference
	"Offer an opportunity for the receiver, presumed already to be known in the References registry, to be renamed"

	|  nameSym current newName |
	current _ References keyAtValue: self ifAbsent: [^ self error: 'not found in References'].

	newName _ FillInTheBlank request: 'Please enter new name' initialAnswer: current.
	"Want to user some better way of determining the validity of the chosen identifier, and also want to give more precise diagnostic if the string the user types in is not acceptable.  Work to be done here."

	newName isEmpty ifTrue: [^ nil].
	((Scanner isLiteralSymbol: newName) and: [(newName includes: $:) not])
		ifTrue:
			[nameSym _ newName capitalized asSymbol.
			(((References includesKey:  nameSym) not and:
				[(Smalltalk includesKey: nameSym) not]) and:
						[(ScriptingSystem allKnownClassVariableNames includes: nameSym) not])
					ifTrue:
						[(References associationAt: current) key: nameSym.
						References rehash.
						^ nameSym]].
	self inform: 'Sorry, that name is not available.'.
	^ nil! !

!Object methodsFor: 'viewer' stamp: 'sw 8/3/2001 21:22'!
defaultLimitClassForVocabulary: aVocabulary
	"Answer the class to use, by default, as the limit class on a protocol browser or viewer opened up on the receiver, within the purview of the Vocabulary provided"

	^ (aVocabulary isKindOf: FullVocabulary)
		ifTrue:
			 [self class superclass == Object
				ifTrue:
					[self class]
				ifFalse:
					[self class superclass]]
		ifFalse:
			[ProtoObject]! !

!Object methodsFor: 'viewer' stamp: 'sw 2/14/2000 14:24'!
defaultNameStemForInstances
	"Answer a basis for names of default instances of the receiver.  The default is to let the class specify, but certain instances will want to override.  (PasteUpMorphs serving as Worlds come to mind"

	^ self class defaultNameStemForInstances! !

!Object methodsFor: 'viewer' stamp: 'sw 5/22/2001 16:53'!
elementTypeFor: aStringOrSymbol vocabulary: aVocabulary
	"Answer a symbol characterizing what kind of element aStringOrSymbol represents.  Realistically, at present, this always just returns #systemScript; a prototyped but not-incorporated architecture supported use of a leading colon to characterize an inst var of a system class, and for the moment we still see its remnant here."

	self flag: #deferred.  "a loose end in the non-player case"
	^ #systemScript! !

!Object methodsFor: 'viewer' stamp: 'sw 5/4/2001 07:04'!
externalName
	"Answer an external name by which the receiver is known.  Generic implementation here is a transitional backstop. probably"

	^ self nameForViewer! !

!Object methodsFor: 'viewer' stamp: 'sw 5/4/2001 07:06'!
graphicForViewerTab
	"When a Viewer is open on the receiver, its tab needs some graphic to show to the user.  Answer a form or a morph to serve that purpose.  A generic image is used for arbitrary objects, but note my reimplementors"
	
	^ ScriptingSystem formAtKey: 'Image'! !

!Object methodsFor: 'viewer' stamp: 'sw 5/4/2001 07:08'!
hasUserDefinedSlots
	"Answer whether the receiver has any user-defined slots, in the omniuser sense of the term.  This is needed to allow Viewers to look at any object, not just at Players."

	^ false! !

!Object methodsFor: 'viewer' stamp: 'sw 8/22/2002 14:07'!
infoFor: anElement inViewer: aViewer
	"The user made a gesture asking for info/menu relating to me.  Some of the messages dispatched here are not yet available in this image"

	| aMenu elementType |
	elementType _ self elementTypeFor: anElement vocabulary: aViewer currentVocabulary.
	((elementType = #systemSlot) | (elementType == #userSlot))
		ifTrue:	[^ self slotInfoButtonHitFor: anElement inViewer: aViewer].
	self flag: #deferred.  "Use a traditional MenuMorph, and reinstate the pacify thing"
	aMenu _ MenuMorph new defaultTarget: aViewer.
	#(	('implementors'			browseImplementorsOf:)
		('senders'				browseSendersOf:)
		('versions'				browseVersionsOf:)
		-
		('browse full'			browseMethodFull:)
		('inheritance'			browseMethodInheritance:)
		-
		('about this method'		aboutMethod:)) do:

			[:pair |
				pair = '-'
					ifTrue:
						[aMenu addLine]
					ifFalse:
						[aMenu add: pair first target: aViewer selector: pair second argument: anElement]].
	aMenu addLine.
	aMenu defaultTarget: self.
	#(	('destroy script'		removeScript:)
		('rename script'		renameScript:)
		('pacify script'		pacifyScript:)) do:
			[:pair |
				aMenu add: pair first target: self selector: pair second argument: anElement].

	aMenu addLine.
	aMenu  add: 'show categories....' target: aViewer selector: #showCategoriesFor: argument: anElement.
	aMenu items size == 0 ifTrue:  "won't happen at the moment a/c the above"
		[aMenu add: 'ok' action: nil].  "in case it was a slot -- weird, transitional"

	aMenu addTitle: anElement asString, ' (', elementType, ')'.

	aMenu popUpInWorld: self currentWorld.
 ! !

!Object methodsFor: 'viewer' stamp: 'sw 9/26/2001 11:58'!
initialTypeForSlotNamed: aName
	"Answer the initial type to be ascribed to the given instance variable"

	^ #Object! !

!Object methodsFor: 'viewer' stamp: 'ar 5/26/2001 16:13'!
isPlayerLike
	"Return true if the receiver is a player-like object"
	^false! !

!Object methodsFor: 'viewer' stamp: 'nk 9/11/2004 16:53'!
methodInterfacesInPresentationOrderFrom: interfaceList forCategory: aCategory 
	"Answer the interface list sorted in desired presentation order, using a 
	static master-ordering list, q.v. The category parameter allows an 
	escape in case one wants to apply different order strategies in different 
	categories, but for now a single master-priority-ordering is used -- see 
	the comment in method EToyVocabulary.masterOrderingOfPhraseSymbols"

	| masterOrder ordered unordered index |
	masterOrder := Vocabulary eToyVocabulary masterOrderingOfPhraseSymbols.
	ordered := SortedCollection sortBlock: [:a :b | a key < b key].
	unordered := SortedCollection sortBlock: [:a :b | a wording < b wording].

	interfaceList do: [:interface | 
		index := masterOrder indexOf: interface elementSymbol.
		index isZero
			ifTrue: [unordered add: interface]
			ifFalse: [ordered add: index -> interface]].

	^ Array
		streamContents: [:stream | 
			ordered do: [:assoc | stream nextPut: assoc value].
			stream nextPutAll: unordered]! !

!Object methodsFor: 'viewer' stamp: 'sw 10/24/2000 11:36'!
newScriptorAround: aPhraseTileMorph
	"Sprout a scriptor around aPhraseTileMorph, thus making a new script.  This is where generalized scriptors will be threaded in"

	^ nil! !

!Object methodsFor: 'viewer' stamp: 'sw 10/25/2000 17:42'!
offerViewerMenuForEvt: anEvent morph: aMorph
	"Offer the viewer's primary menu to the user.  aMorph is some morph within the viewer itself, the one within which a mousedown triggered the need for this menu, and it is used only to retrieve the Viewer itself"

	self offerViewerMenuFor: (aMorph ownerThatIsA: StandardViewer) event: anEvent! !

!Object methodsFor: 'viewer' stamp: 'sw 8/11/2002 02:03'!
offerViewerMenuFor: aViewer event: evt
	"Offer the primary Viewer menu to the user.  Copied up from Player code, but most of the functions suggested here don't work for non-Player objects, many aren't even defined, some relate to exploratory sw work not yet reflected in the current corpus.  We are early in the life cycle of this method..."

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addStayUpItem.
	aMenu title: '**CAUTION -- UNDER CONSTRUCTION!!**
Many things may not work!!
', self nameForViewer.
	(aViewer affordsUniclass and: [self belongsToUniClass not]) ifTrue:
		[aMenu add: 'give me a Uniclass' action: #assureUniClass.
		aMenu addLine].
	aMenu add: 'choose vocabulary...' target: aViewer action: #chooseVocabulary.
	aMenu add: 'choose limit class...' target: aViewer action: #chooseLimitClass.
	aMenu add: 'add search pane' target: aViewer action: #addSearchPane.
	aMenu balloonTextForLastItem: 'Specify which class should be the most generic one to have its methods shown in this Viewer'.
	aMenu addLine.

	self belongsToUniClass ifTrue:
		[aMenu add: 'add a new instance variable' target: self selector: #addInstanceVariableIn: argument: aViewer.
		aMenu add: 'add a new script' target: aViewer selector: #newPermanentScriptIn: argument: aViewer.
		aMenu addLine.
		aMenu add: 'make my class be first-class' target: self selector: #makeFirstClassClassIn: argument: aViewer.
		aMenu add: 'move my changes up to my superclass' target: self action: #promoteChangesToSuperclass.
		aMenu addLine].

	aMenu add: 'tear off a tile' target: self selector: #launchTileToRefer.
	aMenu addLine.

	aMenu add: 'inspect me' target: self selector: #inspect.
	aMenu add: 'inspect my class' target: self class action: #inspect.
	aMenu addLine.

	aMenu add: 'browse vocabulary' action: #haveFullProtocolBrowsed.
	aMenu add: 'inspect this Viewer' target: aViewer action: #inspect.

	aMenu popUpEvent: evt in: aViewer currentWorld

"
	aMenu add: 'references to me' target: aViewer action: #browseReferencesToObject.
	aMenu add: 'toggle scratch pane' target: aViewer selector: #toggleScratchPane.
	aMenu add: 'make a nascent script for me' target: aViewer selector: #makeNascentScript.
	aMenu add: 'rename me' target: aViewer selector: #chooseNewNameForReference.
	aMenu add: 'browse full' action: #browseOwnClassFull.
	aMenu add: 'browse hierarchy' action: #browseOwnClassHierarchy.
	aMenu add: 'set user level...' target: aViewer action: #setUserLevel.
	aMenu add: 'browse sub-protocol' action: #browseOwnClassSubProtocol.
	aMenu addLine.

"! !

!Object methodsFor: 'viewer' stamp: 'sw 1/22/2001 15:20'!
renameScript: oldSelector
	"prompt the user for a new selector and apply it.  Presently only works for players"

	self notYetImplemented! !

!Object methodsFor: 'viewer' stamp: 'sw 8/10/2004 11:53'!
tilePhrasesForCategory: aCategorySymbol inViewer: aViewer
	"Return a collection of phrases for the category."

	| interfaces |
	interfaces _ self methodInterfacesForCategory: aCategorySymbol inVocabulary: aViewer currentVocabulary limitClass: aViewer limitClass.
	interfaces _ self methodInterfacesInPresentationOrderFrom: interfaces forCategory: aCategorySymbol.
	^ self tilePhrasesForMethodInterfaces: interfaces inViewer: aViewer! !

!Object methodsFor: 'viewer' stamp: 'sw 8/10/2004 11:53'!
tilePhrasesForMethodInterfaces: methodInterfaceList inViewer: aViewer
	"Return a collection of ViewerLine objects corresponding to the method-interface list provided.   The resulting list will be in the same order as the incoming list, but may be smaller if the viewer's vocbulary suppresses some of the methods, or if, in classic tiles mode, the selector requires more arguments than can be handled."

	| toSuppress interfaces resultType itsSelector |
	toSuppress _ aViewer currentVocabulary phraseSymbolsToSuppress.
	interfaces _ methodInterfaceList reject: [:int | toSuppress includes: int selector].
	Preferences universalTiles ifFalse:  "Classic tiles have their limitations..."
		[interfaces _ interfaces select:
			[:int |
				itsSelector _ int selector.
				itsSelector numArgs < 2 or:
					"The lone two-arg loophole in classic tiles"
					[#(color:sees:) includes: itsSelector]]].

	^ interfaces collect:
		[:aMethodInterface |
			((resultType _ aMethodInterface resultType) notNil and: [resultType ~~ #unknown]) 
				ifTrue:
					[aViewer phraseForVariableFrom: aMethodInterface]
				ifFalse:
					[aViewer phraseForCommandFrom: aMethodInterface]]! !

!Object methodsFor: 'viewer' stamp: 'sw 8/10/2004 12:23'!
tilePhrasesForSelectorList: aList inViewer: aViewer
	"Particular to the search facility in viewers.  Answer a list, in appropriate order, of ViewerLine objects to put into the viewer."

	| interfaces aVocab |
	aVocab _ aViewer currentVocabulary.
	interfaces _ self
		methodInterfacesInPresentationOrderFrom:
			(aList collect: [:aSel | aVocab methodInterfaceForSelector: aSel class: self class])
		forCategory: #search.
	^ self tilePhrasesForMethodInterfaces: interfaces inViewer: aViewer! !

!Object methodsFor: 'viewer' stamp: 'sw 5/4/2001 04:51'!
tileToRefer
	"Answer a reference tile that comprises an alias to me"

	^ TileMorph new setToReferTo: self! !

!Object methodsFor: 'viewer' stamp: 'sd 3/30/2005 22:04'!
uniqueInstanceVariableNameLike: aString excluding: takenNames
	"Answer a nice instance-variable name to be added to the receiver which resembles aString, making sure it does not coincide with any element in takenNames"

	| okBase uniqueName usedNames |
	usedNames _ self class allInstVarNamesEverywhere.
	usedNames removeAllFoundIn: self class instVarNames.
	usedNames addAll: takenNames.
	okBase _ Scanner wellFormedInstanceVariableNameFrom: aString.

	uniqueName _ Utilities keyLike: okBase satisfying: 
		[:aKey | (usedNames includes: aKey) not].

	^ uniqueName! !

!Object methodsFor: 'viewer' stamp: 'sw 11/21/2001 15:16'!
uniqueNameForReference
	"Answer a nice name by which the receiver can be referred to by other objects.  At present this uses a global References dictionary to hold the database of references, but in due course this will need to acquire some locality"

	| aName nameSym stem knownClassVars |
	(aName _ self uniqueNameForReferenceOrNil) ifNotNil: [^ aName].
	(stem _ self knownName) ifNil:
		[stem _ self defaultNameStemForInstances asString].
	stem _ stem select: [:ch | ch isLetter or: [ch isDigit]].
	stem size == 0 ifTrue: [stem _ 'A'].
	stem first isLetter ifFalse:
		[stem _ 'A', stem].
	stem _ stem capitalized.
	knownClassVars _ ScriptingSystem allKnownClassVariableNames.
	aName _ Utilities keyLike:  stem satisfying:
		[:jinaLake |
			nameSym _ jinaLake asSymbol.
			 ((References includesKey:  nameSym) not and:
				[(Smalltalk includesKey: nameSym) not]) and:
						[(knownClassVars includes: nameSym) not]].

	References at: (aName _ aName asSymbol) put: self.
	^ aName! !

!Object methodsFor: 'viewer' stamp: 'md 1/17/2006 17:58'!
uniqueNameForReferenceFrom: proposedName
	"Answer a satisfactory symbol, similar to the proposedName but obeying the rules, to represent the receiver"

	| aName nameSym stem okay |
	proposedName = self uniqueNameForReferenceOrNil 
		ifTrue: [^ proposedName].  "No change"

	stem _ proposedName select: [:ch | ch isLetter or: [ch isDigit]].
	stem size == 0 ifTrue: [stem _ 'A'].
	stem first isLetter ifFalse:
		[stem _ 'A', stem].
	stem _ stem capitalized.
	aName _ Utilities keyLike: stem satisfying:
		[:jinaLake |
			nameSym _ jinaLake asSymbol.
			okay _ true.
			(self class bindingOf: nameSym) ifNotNil: [okay _ false "don't use it"].
			okay].
	^ aName asSymbol! !

!Object methodsFor: 'viewer' stamp: 'sw 3/15/2004 23:01'!
uniqueNameForReferenceOrNil
	"If the receiver has a unique name for reference, return it here, else return nil"

	^ References keyAtValue: self ifAbsent: [nil]! !

!Object methodsFor: 'viewer' stamp: 'ar 5/16/2001 01:40'!
updateThresholdForGraphicInViewerTab
	"When a Viewer is open on the receiver, its tab needs some graphic to show to the user. Computing this graphic can take quite some time so we want to make the update frequency depending on how long it takes to compute the thumbnail. The threshold returned by this method defines that the viewer will update at most every 'threshold * timeItTakesToDraw' milliseconds. Thus, if the time for computing the receiver's thumbnail is 200 msecs and the the threshold is 10, the viewer will update at most every two seconds."
	^20 "seems to be a pretty good general choice"! !

!Object methodsFor: 'viewer' stamp: 'sw 3/9/2001 13:48'!
usableMethodInterfacesIn: aListOfMethodInterfaces
	"Filter aList, returning a subset list of apt phrases"

	^ aListOfMethodInterfaces
! !


!Object methodsFor: 'world hacking' stamp: 'ar 3/17/2001 23:45'!
couldOpenInMorphic

        "is there an obvious morphic world in which to open a new morph?"

        ^World notNil or: [ActiveWorld notNil]! !


!Object methodsFor: 'private'!
errorImproperStore
	"Create an error notification that an improper store was attempted."

	self error: 'Improper store into indexable object'! !

!Object methodsFor: 'private'!
errorNonIntegerIndex
	"Create an error notification that an improper object was used as an index."

	self error: 'only integers should be used as indices'! !

!Object methodsFor: 'private' stamp: 'yo 6/29/2004 11:37'!
errorNotIndexable
	"Create an error notification that the receiver is not indexable."

	self error: ('Instances of {1} are not indexable' translated format: {self class name})! !

!Object methodsFor: 'private'!
errorSubscriptBounds: index 
	"Create an error notification that an improper integer was used as an index."

	self error: 'subscript is out of bounds: ' , index printString! !

!Object methodsFor: 'private' stamp: 'ar 2/6/2004 14:47'!
primitiveError: aString 
	"This method is called when the error handling results in a recursion in 
	calling on error: or halt or halt:."

	| context |
	(String
		streamContents: 
			[:s |
			s nextPutAll: '***System error handling failed***'.
			s cr; nextPutAll: aString.
			context _ thisContext sender sender.
			20 timesRepeat: [context == nil ifFalse: [s cr; print: (context _ context sender)]].
			s cr; nextPutAll: '-------------------------------'.
			s cr; nextPutAll: 'Type CR to enter an emergency evaluator.'.
			s cr; nextPutAll: 'Type any other character to restart.'])
		displayAt: 0 @ 0.
	[Sensor keyboardPressed] whileFalse.
	Sensor keyboard = Character cr ifTrue: [Transcripter emergencyEvaluator].
	Smalltalk isMorphic
		ifTrue: [World install "init hands and redisplay"]
		ifFalse: [ScheduledControllers searchForActiveController]! !

!Object methodsFor: 'private'!
species
	"Answer the preferred class for reconstructing the receiver.  For example, 
	collections create new collections whenever enumeration messages such as 
	collect: or select: are invoked.  The new kind of collection is determined by 
	the species of the original collection.  Species and class are not always the 
	same.  For example, the species of Interval is Array."

	^self class! !

!Object methodsFor: 'private'!
storeAt: offset inTempFrame: aContext
	"This message had to get sent to an expression already on the stack
	as a Block argument being accessed by the debugger.
	Just re-route it to the temp frame."
	^ aContext tempAt: offset put: self! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Object class
	instanceVariableNames: ''!

!Object class methodsFor: '*Pinesoft-Widgets' stamp: 'gvc 4/17/2007 17:40'!
taskbarIcon
	"Answer the icon for an instance of the receiver in a task bar
	or nil for the default."

	^nil! !


!Object class methodsFor: '*magritte-model-accessing' stamp: 'lr 3/27/2006 15:47'!
description
	^ MADescriptionBuilder for: self! !


!Object class methodsFor: 'class initialization' stamp: 'ar 2/11/2001 02:00'!
flushDependents
	DependentsFields keysAndValuesDo:[:key :dep|
		key ifNotNil:[key removeDependent: nil].
	].
	DependentsFields finalizeValues.! !

!Object class methodsFor: 'class initialization' stamp: 'rw 2/10/2002 13:09'!
flushEvents
	"Object flushEvents"

	EventManager flushEvents. ! !

!Object class methodsFor: 'class initialization' stamp: 'rww 10/2/2001 07:35'!
initialize
	"Object initialize"
	DependentsFields ifNil:[self initializeDependentsFields].! !

!Object class methodsFor: 'class initialization' stamp: 'ar 2/11/2001 01:41'!
initializeDependentsFields
	"Object initialize"
	DependentsFields _ WeakIdentityKeyDictionary new.
! !

!Object class methodsFor: 'class initialization' stamp: 'ar 2/11/2001 01:45'!
reInitializeDependentsFields
	"Object reInitializeDependentsFields"
	| oldFields |
	oldFields _ DependentsFields.
	DependentsFields _ WeakIdentityKeyDictionary new.
	oldFields keysAndValuesDo:[:obj :deps|
		deps do:[:d| obj addDependent: d]].
! !


!Object class methodsFor: 'documentation'!
howToModifyPrimitives
	"You are allowed to write methods which specify primitives, but please use 
	caution.  If you make a subclass of a class which contains a primitive method, 
	the subclass inherits the primitive.  The message which is implemented 
	primitively may be overridden in the subclass (E.g., see at:put: in String's 
	subclass Symbol).  The primitive behavior can be invoked using super (see 
	Symbol string:). 
	 
	A class which attempts to mimic the behavior of another class without being 
	its subclass may or may not be able to use the primitives of the original class.  
	In general, if the instance variables read or written by a primitive have the 
	same meanings and are in the same fields in both classes, the primitive will 
	work.  

	For certain frequently used 'special selectors', the compiler emits a 
	send-special-selector bytecode instead of a send-message bytecode.  
	Special selectors were created because they offer two advantages.  Code 
	which sends special selectors compiles into fewer bytes than normal.  For 
	some pairs of receiver classes and special selectors, the interpreter jumps 
	directly to a primitive routine without looking up the method in the class.  
	This is much faster than a normal message lookup. 
	 
	A selector which is a special selector solely in order to save space has a 
	normal behavior.  Methods whose selectors are special in order to 
	gain speed contain the comment, 'No Lookup'.  When the interpreter 
	encounters a send-special-selector bytecode, it checks the class of the 
	receiver and the selector.  If the class-selector pair is a no-lookup pair, 
	then the interpreter swiftly jumps to the routine which implements the 
	corresponding primitive.  (A special selector whose receiver is not of the 
	right class to make a no-lookup pair, is looked up normally).  The pairs are 
	listed below.  No-lookup methods contain a primitive number specification, 
	<primitive: xx>, which is redundant.  Since the method is not normally looked 
	up, deleting the primitive number specification cannot prevent this 
	primitive from running.  If a no-lookup primitive fails, the method is looked 
	up normally, and the expressions in it are executed. 
	 
	No Lookup pairs of (class, selector) 
	 
	SmallInteger with any of		+ - * /  \\  bitOr: bitShift: bitAnd:  // 
	SmallInteger with any of		=  ~=  >  <  >=  <= 
	Any class with					== 
	Any class with 					@ 
	Point with either of				x y 
	ContextPart with					blockCopy: 
	BlockContext with either of 		value value:
	"

	self error: 'comment only'! !

!Object class methodsFor: 'documentation'!
whatIsAPrimitive
	"Some messages in the system are responded to primitively. A primitive   
	response is performed directly by the interpreter rather than by evaluating   
	expressions in a method. The methods for these messages indicate the   
	presence of a primitive response by including <primitive: xx> before the   
	first expression in the method.   
	  
	Primitives exist for several reasons. Certain basic or 'primitive' 
	operations cannot be performed in any other way. Smalltalk without 
	primitives can move values from one variable to another, but cannot add two 
	SmallIntegers together. Many methods for arithmetic and comparison 
	between numbers are primitives. Some primitives allow Smalltalk to 
	communicate with I/O devices such as the disk, the display, and the keyboard. 
	Some primitives exist only to make the system run faster; each does the same 
	thing as a certain Smalltalk method, and its implementation as a primitive is 
	optional.  
	  
	When the Smalltalk interpreter begins to execute a method which specifies a 
	primitive response, it tries to perform the primitive action and to return a 
	result. If the routine in the interpreter for this primitive is successful, 
	it will return a value and the expressions in the method will not be evaluated. 
	If the primitive routine is not successful, the primitive 'fails', and the 
	Smalltalk expressions in the method are executed instead. These 
	expressions are evaluated as though the primitive routine had not been 
	called.  
	  
	The Smalltalk code that is evaluated when a primitive fails usually 
	anticipates why that primitive might fail. If the primitive is optional, the 
	expressions in the method do exactly what the primitive would have done (See 
	Number @). If the primitive only works on certain classes of arguments, the 
	Smalltalk code tries to coerce the argument or appeals to a superclass to find 
	a more general way of doing the operation (see SmallInteger +). If the 
	primitive is never supposed to fail, the expressions signal an error (see 
	SmallInteger asFloat).  
	  
	Each method that specifies a primitive has a comment in it. If the primitive is 
	optional, the comment will say 'Optional'. An optional primitive that is not 
	implemented always fails, and the Smalltalk expressions do the work 
	instead.  
	 
	If a primitive is not optional, the comment will say, 'Essential'. Some 
	methods will have the comment, 'No Lookup'. See Object 
	howToModifyPrimitives for an explanation of special selectors which are 
	not looked up.  
	  
	For the primitives for +, -, *, and bitShift: in SmallInteger, and truncated 
	in Float, the primitive constructs and returns a 16-bit 
	LargePositiveInteger when the result warrants it. Returning 16-bit 
	LargePositiveIntegers from these primitives instead of failing is 
	optional in the same sense that the LargePositiveInteger arithmetic 
	primitives are optional. The comments in the SmallInteger primitives say, 
	'Fails if result is not a SmallInteger', even though the implementor has the 
	option to construct a LargePositiveInteger. For further information on 
	primitives, see the 'Primitive Methods' part of the chapter on the formal 
	specification of the interpreter in the Smalltalk book."

	self error: 'comment only'! !


!Object class methodsFor: 'file list services' stamp: 'nk 6/12/2004 11:41'!
fileReaderServicesForDirectory: aFileDirectory
	"Backstop"
	^#()! !

!Object class methodsFor: 'file list services' stamp: 'nk 6/12/2004 11:30'!
fileReaderServicesForFile: fullName suffix: suffix
	"Backstop"
	^#()! !

!Object class methodsFor: 'file list services' stamp: 'md 2/15/2006 17:20'!
services
	"Backstop"
	^#()! !


!Object class methodsFor: 'instance creation' stamp: 'sw 1/23/2003 09:45'!
categoryForUniclasses
	"Answer the default system category into which to place unique-class instances"

	^ 'UserObjects'! !

!Object class methodsFor: 'instance creation' stamp: 'sw 7/28/97 15:56'!
chooseUniqueClassName
	| i className |
	i _ 1.
	[className _ (self name , i printString) asSymbol.
	 Smalltalk includesKey: className]
		whileTrue: [i _ i + 1].
	^ className! !

!Object class methodsFor: 'instance creation' stamp: 'tk 8/22/1998 08:22'!
initialInstance
	"Answer the first instance of the receiver, generate an error if there is one already"
	"self instanceCount > 0 ifTrue: [self error: 'instance(s) already exist.']."
		"Debugging test that is very slow"
	^ self new! !

!Object class methodsFor: 'instance creation' stamp: 'sw 5/5/2000 09:30'!
initializedInstance
	^ self new! !

!Object class methodsFor: 'instance creation' stamp: 'sw 10/16/2000 10:58'!
instanceOfUniqueClass
	"Answer an instance of a unique subclass of the receiver"

	^ self instanceOfUniqueClassWithInstVarString: '' andClassInstVarString: ''! !

!Object class methodsFor: 'instance creation' stamp: 'tk 8/22/1998 08:27'!
instanceOfUniqueClassWithInstVarString: instVarString andClassInstVarString: classInstVarString
	"Create a unique class for the receiver, and answer an instance of it"

	^ (self newUniqueClassInstVars: instVarString 
		classInstVars: classInstVarString) initialInstance! !

!Object class methodsFor: 'instance creation' stamp: 'sw 10/23/1999 22:51'!
isUniClass
	^ false! !

!Object class methodsFor: 'instance creation' stamp: 'ajh 5/23/2002 00:35'!
newFrom: aSimilarObject
	"Create an object that has similar contents to aSimilarObject.
	If the classes have any instance varaibles with the same names, copy them across.
	If this is bad for a class, override this method."

	^ (self isVariable
		ifTrue: [self basicNew: aSimilarObject basicSize]
		ifFalse: [self basicNew]
	  ) copySameFrom: aSimilarObject! !

!Object class methodsFor: 'instance creation' stamp: 'tk 6/29/1998 12:11'!
newUniqueClassInstVars: instVarString classInstVars: classInstVarString
	"Create a unique class for the receiver"

	| aName aClass |
	self isSystemDefined ifFalse:
		[^ superclass newUniqueClassInstVars: instVarString classInstVars: classInstVarString].
	aName _ self chooseUniqueClassName.
	aClass _ self subclass: aName instanceVariableNames: instVarString 
		classVariableNames: '' poolDictionaries: '' category: self categoryForUniclasses.
	classInstVarString size > 0 ifTrue:
		[aClass class instanceVariableNames: classInstVarString].
	^ aClass! !

!Object class methodsFor: 'instance creation' stamp: 'sw 7/28/97 15:56'!
newUserInstance
	"Answer an instance of an appropriate class to serve as a user object in the containment hierarchy"

	^ self instanceOfUniqueClass! !

!Object class methodsFor: 'instance creation' stamp: 'nk 8/30/2004 07:57'!
readCarefullyFrom: textStringOrStream
	"Create an object based on the contents of textStringOrStream.  Return an error instead of putting up a SyntaxError window."

	| object |
	(Compiler couldEvaluate: textStringOrStream)
		ifFalse: [^ self error: 'expected String, Stream, or Text'].
	object _ Compiler evaluate: textStringOrStream for: nil 
				notifying: #error: "signal we want errors" logged: false.
	(object isKindOf: self) ifFalse: [self error: self name, ' expected'].
	^object! !

!Object class methodsFor: 'instance creation' stamp: 'nk 8/30/2004 07:57'!
readFrom: textStringOrStream
	"Create an object based on the contents of textStringOrStream."

	| object |
	(Compiler couldEvaluate: textStringOrStream)
		ifFalse: [^ self error: 'expected String, Stream, or Text'].
	object _ Compiler evaluate: textStringOrStream.
	(object isKindOf: self) ifFalse: [self error: self name, ' expected'].
	^object! !


!Object class methodsFor: 'objects from disk' stamp: 'tk 1/8/97'!
createFrom: aSmartRefStream size: varsOnDisk version: instVarList
	"Create an instance of me so objects on the disk can be read in.  Tricky part is computing the size if variable.  Inst vars will be filled in later.  "

	^ self isVariable
		ifFalse: [self basicNew]
		ifTrue: ["instVarList is names of old class's inst vars plus a version number" 
				self basicNew: (varsOnDisk - (instVarList size - 1))]
! !


!Object class methodsFor: 'window color' stamp: 'nk 6/10/2004 08:10'!
windowColorSpecification
	"Answer a WindowColorSpec object that declares my preference.
	This is a backstop for classes that don't otherwise define a preference."

	^ WindowColorSpec classSymbol: self name
		wording: 'Default' brightColor: #white
		pastelColor: #white
		helpMessage: 'Other windows without color preferences.'! !


!Object class methodsFor: 'private' stamp: 'mir 8/22/2001 15:20'!
releaseExternalSettings
	"Do nothing as a default"! !


Object initialize!
