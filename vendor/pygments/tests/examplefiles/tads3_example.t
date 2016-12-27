#charset "utf-8"

#include <adv3.h>
#include <en_us.h>

extern function extern_function;
extern method extern_method;
extern function extern_function(a, b=a, c='<<a>>', d:, e:=1, f?, ...);
extern method extern_method(a, b=a, c='<<a>>', d:, e:=1, f?, [g]);;
extern class extern_class;
extern object extern_object;
intrinsic 't3vm' { };
#ifndef PropDefAny
intrinsic class Object 'root-object/030004' { };
#endif
object /**//**/ // /* \\
#define Room Unthing
    template [lst];

/*
 *   Quotations from "Le Roman de la Rose" are transcribed from MS. Douce 195,
 *   owned by Bodleian Library, University of Oxford
 *   (http://image.ox.ac.uk/show?collection=bodleian&manuscript=msdouce195).
 */

versionInfo: GameID
    IFID = '17d8efc3-07da-4dde-a837-ff7c4e386a77'
    name = 'Pygmentalion'
    byline = 'by David Corbett'
    htmlByline = 'by <a href="mailto:corbett.dav\100husky.neu.edu">David
        Corbett</a>'
    version = '1'
    authorEmail = 'David Corbett\040<corbett.dav\x40husky.neu.edu>'
    desc = 'You have fallen in love with a statue\x2e'
    htmlDesc = 'You have fallen in love with a statue\x2E'
;

/*
 *   Pymalion fu ẽtailleꝛꝛes.
 *   Poᷣtrayãs en fus ⁊ en peꝛꝛeˢ
 *   En metaulx en os ⁊ en cyꝛes
 *   Et en touteˢ aultres matires.
 *   Quon peult a tel oeuure trouuer.
 *   Poᷣ ſon grant engin eſpꝛouuer.
 *   Car maiſtre en fu bien dire loz.
 *   Ainſi com poᷣ acquerre loz
 *   Se voult a poᷣtraire deduyꝛe
 *   Si fiſt vng ymage diuuyꝛe
 *   Et miſt au faire tel entente
 *   Quel fu ſi plaiſãt et ſi gente
 *   Quel ſembloit eſtre auſſi viue.
 *   Com la plus belle riens q̇ viue
 *      (MS. Douce 195, fol. 149r)
 */

modify _init()
{
    ({: local r, r = randomize, r})();
    replaced();
}

gameMain: GameMainDef
    initialPlayerChar: Actor {
        desc = "You look the same as usual, but you feel unusually
            sentimental. "
        location = entrance
    }
    showIntro
    {
        "The statue is undeniably a masterpiece: the most skillful carving you
        have ever done, and the most beautiful woman you have ever seen.
        Unfortunately, she is also an inanimate block, and now you can neither
        work nor rest for unrequitable love.\b
        Once again you stumble into your studio, hoping and praying to find
        your statue brought to life.\b
        <b><<versionInfo.name>></b>\r\n
        <<versionInfo.byline>>\b";
    }
;

enum token token, tokOp, token;

modify cmdTokenizer
    rules_ = static
    [
        ['whitespace', new RexPattern('%s+'), nil, &tokCvtSkip, nil],
        ['punctuation', new RexPattern('[.,;:?!]'), tokPunct, nil, nil],
        ['spelled number',
         new RexPattern('<NoCase>(twenty|thirty|forty|fifty|sixty|'
                        + 'seventy|eighty|ninety)-'
                        + '(one|two|three|four|five|six|seven|eight|nine)'
                        + '(?!<AlphaNum>)'),
         tokWord, &tokCvtSpelledNumber, nil],
        ['spelled operator', new RexPattern(
            '<NoCase>(plus|positive|minus|negat(iv)?e|not|inverse(%s+of)?|'
            + 'times|over|divided%s+by|mod(ulo)?|and|xor|or|[al]?sh[lr])'
            + '(?!<AlphaNum>)'),
         tokOp, &tokCvtSpelledOperator, nil],
        ['operator', R'[-!~+*/%&^|]|<<|>>>?', tokOp, nil, nil],
        ['word', new RexPattern('<Alpha|-|&><AlphaNum|-|&|squote>*'),
         tokWord, nil, nil],
        ['string ascii-quote', R"""<min>([`\'"])(.*)%1(?!<AlphaNum>)""",
         tokString, nil, nil],
        ['string back-quote', R"<min>`(.*)'(?!%w)", tokString, nil, nil],
        ['string curly single-quote', new RexPattern('<min>\u2018(.*)\u2019'),
         tokString, nil, nil],
        ['string curly double-quote', new RexPattern('<min>\u201C(.*)\u201D'),
         tokString, nil, nil],
        ['string unterminated', R'''([`\'"\u2018\u201C](.*)''', tokString,
         nil, nil],
        ['integer', new RexPattern('[0-9]+'), tokInt, nil, nil]
    ]
    replace tokCvtSpelledOperator(txt, typ, toks)
    {
        toks.append([rexReplace(R'%s+', txt.toLower(), '\\'), typ, txt]);
    }
;

/* Tokens */

/*
 *   Puiˢ li reueſt en maĩteˢ guiſes.
 *   Robeˢ faicteˢ ꝑ grãˢ maiſtriſeˢ.
 *   De biaulx dꝛaps de ſoye ⁊ de laĩe.
 *   Deſcarlate de tiretaine
 *   De vert de pers ⁊ de bꝛunecte
 *   De couleᷣ freſche fine ⁊ necte
 *   Ou moult a riches paneˢ miſes.
 *   Herminees vaires et griſes
 *   Puis les li roſte puis reſſaye.
 *   Cõmant li ſiet robbe de ſaye
 *   Sendaulx meloguins galebꝛunˢ.
 *   Indes vermeilz iaunes ⁊ bꝛunˢ.
 *   [...]
 *   Aultre foiz luy repꝛẽd courage.
 *   De tout oſter ⁊ mectre guindeˢ.
 *   Iaunes vermeilles vers ⁊ indeˢ.
 *      (MS. Douce 195, fol. 150r)
 */

class Token: Achievement
{
    points = 1;
    desc = "<<before_>><<desc_>><<after_>>";
    before = before = '', before_
    after = (after = '', after_)
}

Token template inherited 'before_' 'after_' 'desc_';

#define DefineToken(name, before, after) name##Token: Token before after #@name

DefineToken(builtin, '<font color=g&#x72;een>', '</font>');
DefineToken(comment, '<i><font color=#408080>', '</font></i>');
DefineToken(decorator, '<font color=#aa22ff>', '</font>');
DefineToken(error, '<U><FONT COLOR=RED>', '</FONT></U>');
DefineToken(escape, '<b><font color=#bb6622>', '</font></b>');
DefineToken(float, '<u><font color=gray>', '</font></u>');
DefineToken(keyword, '<b><font face=TADS-Sans color=green>', '</font></b>');
DefineToken(label, '<font color=#A0A000>', '</font>');
DefineToken(long, '<i><font color=gray>', '</font></i>');
DefineToken(name, '<u>', '</u>');
DefineToken(operator, '<b><font color=\"#AA22F&#x46;\">', '</font></b>');
DefineToken(string, '<font color=\'#BA212&#49;\'>', '</font>');
DefineToken(whitespace, '<font color="bgcolor"bgcolor=\'text\'>', '</font>');

function highlightToken(tokenString)
{
    local token = [
        'built in' -> builtinToken,
        'comment' -> commentToken,
        'decorator' -> decoratorToken,
        'error' -> errorToken,
        'escape' -> escapeToken,
        'float' -> floatToken,
        'keyword' -> keywordToken,
        'label' -> labelToken,
        'long' -> longToken,
        'name' -> nameToken,
        'operator' -> operatorToken,
        'string' -> stringToken,
        'white space' -> whitespaceToken,
        * -> nil
    ][tokenString.toLower()];
    if (!token)
        return tokenString;
    token.awardPointsOnce();
    return '<<token.before>><<tokenString>><<token.after>>';
}

string /**//**/ // /* \\
#define Room Unthing
    template <<highlight *>> highlightToken;

/* Grammar for materials */

dictionary property material;
grammar adjWord(material): <material material>->adj_ : AdjPhraseWithVocab
    getVocabMatchList(resolver, results, extraFlags)
    {
        return getWordMatches(adj_, &material, resolver, extraFlags,
                              VocabTruncated);
    }
    getAdjustedTokens()
    {
        return [adj_, &material];
    }
;

/* Rooms and objects */

+ property location;

entrance: Room 'Entrance'
    "You are in the entrance to your studio. This is where you carve great
    works of art, not that you have felt like making any lately. A door leads
    outside, and the studio itself is to the north and the east. "
    north = workbenchRoom
    northeast = sinkRoom
    east = altarRoom
    south = door
    out asExit(south)
;

+ door: LockableWithKey, Door 'door' 'door'
    "It is a simple wooden door. "
    material = 'wood' 'wooden'
    keyList = [key]
    cannotOpenLockedMsg = '{The dobj/He} {is} locked. You cannot
        <<highlight 'escape'>>! '
;

key: PresentLater, Key 'key' 'key' @altar
    "It is a <<unless clean>>grimy<<end>> bronze key. <<if clean>>On it is \
    etched the word <q><<keyword>></q>. "
    material = 'bronze'
    clean = nil
    keyword = (keyword = randomGreekWord(), targetprop)
    dobjFor(Clean) { verify { } action { askForIobj(CleanWith); } }
    dobjFor(CleanWith)
    {
        verify
        {
            if (clean)
                illogicalAlready('{The dobj/He} {is} already clean. ');
        }
        action
        {
            gDobj.clean = true;
            "{You/He} clean{s} {the dobj/him}, revealing an inscription. ";
        }
    }
    dobjFor(Read) { verify { nonObvious; } }
;

workbenchRoom: Room 'At the Workbench'
    "This workbench, in the northwest part of the studio, was where you would
    create works of art. Now you just come here to contemplate your
    creation&rsquo;s beauty and lament your hopeless situation.\b
    The statue stands on a plinth beside the workbench. "
    east = sinkRoom
    southeast = altarRoom
    south = entrance
    getDestName(actor, origin) { return 'the workbench'; }
;

+ workbench: Fixture, Surface
    'workbench/bench/material/materials/tool/tools' 'workbench'
    "Normally, the workbench would be scattered with half-finished projects,
    but now your tools and materials lie abandoned. "
;

+ plinth: Fixture, Thing 'marble plinth/pedestal' 'plinth'
    "It&rsquo;s a smoothed block of marble about a cubit high. "
;

replace grammar predicate(Screw): ' ': object;
replace grammar predicate(ScrewWith): ' ': object;
+ + statue: Fixture, Surface
    '"creation\'s" beauty/carving/creation/galatea/statue/woman' 'statue'
    "This is a<<if nameToken.scoreCount>>n untitled<<end>> statue of a woman
    carved from <<if errorToken.scoreCount>>flawless <<end>>
    <<if whitespaceToken.scoreCount>>milk-white <<end>>ivory.
    <<if escapeToken.scoreCount || longToken.scoreCount>>Her
    <<if longToken.scoreCount>>long <<end>>hair is done up in a
    chignon<<if escapeToken.scoreCount>>, with a few strands falling down her
    neck<<end>><<if floatToken.scoreCount>>, and \v<<else>>.<<end>><<end>>
    <<if floatToken.scoreCount>>She radiates an aura of contrapposto grace.
    <<end>><<if keywordToken.scoreCount>>\bYou wonder what she
    <<if labelToken.scoreCount>>is going to<<else>>will<<end>> be like as a
    woman.
    <<if decoratorToken.scoreCount>>Maybe she&rsquo;ll be a painter and expand
    your business.<<end>>
    <<if operatorToken.scoreCount>>Maybe she&rsquo;ll have a head for figures
    and will put the accounts in order.<<end>>
    <<if builtinToken.scoreCount>>She&rsquo;ll love you, obviously, but beyond
    that you don&rsquo;t know.<<end>><<end>>
    <<if commentToken.scoreCount>>If only Aphrodite would bring her to life
    without this silly puzzle about tokens and mirrors!<<end>> "
    material = 'ivory'
    propertyset 'is*'
    {
        propertyset 'H*'
        {
            im = nil\
            er = true;
        }
        It = true
    }
    iobjFor(PutOn)
    {
        check
        {
            if (gDobj not /**//**/ // /* \\
#define Room Unthing
                in (necklace, __objref(necklace, warn)))
                "How rude! You don&rsquo;t know what you were thinking. ";
        }
    }
    iobjFor(GiveTo) remapTo(PutOn, DirectObject, IndirectObject)
;

+++ necklace: Wearable
    'pearl necklace/string pearls' '<<highlight 'string'>> of pearls'
    "This is a masterfully crafted pearl necklace. You hope the statue
    won&rsquo;t mind if you hold onto it for a while. "
    initDesc = "You gave the statue this pearl necklace yesterday. "
    isPlural = true
;

altarRoom: Room 'At the Altar'
    "Light from the window illuminates a crude altar. Until recently, this
    corner was your bedroom. The rest of the studio lies north and west. "
    north = sinkRoom
    northwest = workbenchRoom
    west = entrance
    getDestName(actor, origin) { return 'the altar'; }
;

+ window: Fixture 'window' 'window'
    "It&rsquo;s just a window above the altar. <<one of>>The space under the
    window is blank; as an interior <<highlight 'decorator'>>, you can&rsquo;t
    help but think the wall would benefit from a bas-relief, but &ndash;
    <i>sigh</i> &endash you are too lovelorn to wield the chisel. <<||>>The
    wall right below it is a boring <<highlight 'white space'>>. <<stopping>>"
;

+ altar: Fixture, Surface 'crude rough altar/banker/slab' 'altar'
    "A rough marble slab lies on a wooden banker. In your rush to construct an
    altar, you neglected the usual surface finish and friezes, but you pray at
    it anyway. You are sure the gods will understand. "
    material = 'marble' 'wood' 'wooden'
    bulkCapacity = 1
    dobjFor(PrayAt)
    {
        verify { }
        action()
        {
            /*
             *   Biaulx dieux diſt il tout ce poez.
             *   Sil voꝰ plaiſt ma requeſte oez
             *   [...]
             *   Et la belle q̇ mon cueᷣ emble
             *   Qui ſi bien yuoyꝛe reſſemble.
             *   Deuiengne ma loyal amye
             *   De fẽme ait coꝛps ame et vie
             *      (MS. Douce 195, fol. 151r)
             */
            local offering;
            foreach (offering in contents);
            if (!keywordToken.scoreCount)
                "<<one of>><q>O Aphrodite,</q> you say, <q>comforter of
                hopeless lovers, hear my prayer! May she to whom I have given
                my heart be given body, soul, and life. And a colorful
                personality. And&mdash</q>\b
                You are interrupted by a shimmering about the altar. As you
                watch, it takes the form of a callipygian goddess.\b
                <q>Mortal, I have heard your heart-felt and oft-repeated plea,
                and I will take pity on you,</q> says Aphrodite. <q>If you give
                me a token of your love as an offering, I will give you the
                <<highlight 'keyword'>> of life. Speak this word in the
                presence of a mirror, and I will grant your request.</q>\b
                She fades away, adding, <q>As for her colorful personality,
                just look around you.</q> <<or>><<stopping>>";
            else if (key.location)
                "<q>O Aphrodite,</q> you say, <q>what am I supposed to do
                again?</q>\bThe goddess reappears and reminds you to speak the
                keyword of life at a mirror. <<one of>><q>What&rsquo;s the
                keyword, then?</q> <q>Gods help those who help themselves.
                Figure it out yourself.</q><<or>><q>Why a mirror?</q> <q>I like
                mirrors.</q><<purely at random>> ";
            else if (offering == necklace)
            {
                "Aphrodite reappears. <q>A necklace! Perfect!</q> The necklace
                disappears in a bright flash. When your eyes readjust, you see
                a key lying in its place. ";
                necklace.moveInto(nil);
                key.makePresent();
            }
            else if (+offering)
                "Aphrodite reappears. She eyes <<offering.theNameObj>>
                skeptically. <q><<one of>>No <<highlight 'comment'>>.<<or>>You
                call <i>that</i> a token of love?<<or>>\^<<offering.aNameObj>>?
                Really?<<or>>Come on, mortal, it&rsquo;s not that
                difficult!<<then at random>></q> ";
            else
                "<q>I heard you the first time,</q> says Aphrodite. <q>Prove
                your devotion by offering a token of your love at the altar,
                or the deal&rsquo;s off.</q> ";
        }
    }
    iobjFor(GiveTo) remapTo(PutOn, DirectObject, IndirectObject)
;

aphrodite: Unthing
    '(love) aphrodite/cytherea/god/goddess/venus love' 'Aphrodite'
    '<<if gActor.canSee(altar)>>You can only pray to a god.
    <<else>>You need an altar to interact with a god. '
    location = (gPlayerChar)
    isProperName = true
    isHer = true
    iobjFor(GiveTo)
    {
        verify
        {
            illogical('She isn&rsquo;t here. You&rsquo;ll have to leave {the
                dobj/him} somewhere she can find it. ');
        }
    }
    dobjFor(PrayAt) maybeRemapTo(gActor.canSee(altar), PrayAt, altar)
;

sinkRoom: Room 'Washroom'
    "Sculpting marble is a dusty business. You use this sink to clean off after
    a hard day&rsquo;s work. Beside the sink is a small end table, and on the
    wall is a calculator. The rest of the studio is south and west. "
    south = altarRoom
    southwest = entrance
    west = workbenchRoom
;

property level, overflowing;
export overflowing;
export level 'waterLevel';
+ sink: Fixture '(auto) (mop) auto-sink/autosink/bowl/drain/faucet/sink' 'sink'
    "This is a state-of-the-art mop sink with anti-miasmic coating and bronze
    backsplash. It is so modern, there are no handles or other obvious ways to
    turn it on.\b
    <<if overflowing>>It is overflowing.
    <<else unless level < 19500>>It is full to the brim with water.
    <<otherwise if level >= 15000>>It is full of water.
    <<otherwise unless level < 10000>>It is half full of water.
    <<else if level >= 2000>>There is some water in the sink.
    <<else if level > 0>>A small puddle has formed at the bottom of the sink.
    <<otherwise>>It is empty.
    <<if level <= -1.0e+2>>It looks like it hasn&rsquo;t been used in a
    <<highlight 'long'>> time. "
    level = not in ([lst]) { return argcount; }
    not = in()
    overflowing = nil
    current = self
    setLevel(level:)
    {
        targetobj.current.overflowing = level == nil;
        targetobj.current.level = min(level ?? 0, 20000);
        if (sink.overflowing || sink.level > 0e+1)
            sinkWater.makePresent();
        if (basin.overflowing || basin.level > 0e-1)
            basinWater.makePresent();
    }
    iobjFor(CleanWith) remapTo(CleanWith, DirectObject, sinkWater)
;

++ sinkWater: PresentLater, Fixture
    '(sink) water sink water' 'water' "<<sink.desc>>"
    disambigName = 'water in the sink'
    dobjFor(Drink)
    {
        verify { illogical('''{You're} not thirsty. '''); }
    }
    iobjFor(CleanWith)
    {
        preCond = []
        verify {
            if (!location)
                illogicalNow('There is no water in the sink. ');
            if (!sink.overflowing && sink.level < 1e2)
                illogicalNow('There is not enough water in the sink. ');
        }
    }
;

+ table: Fixture, Surface 'small end bracket/table' 'table'
    "<<first time>>Upon closer inspection, you see that \v<<only>>The table is
    bracketed to the wall. "
;

++ Readable '"operator\'s" manual' 'manual'
    "<center ><<highlight 'Operator'>>&rsquo;s Manual<\center>\b
    <bq>To control the auto-sink, use the calculator add-on to enter the
    desired volume of water. For example,\n
    \t\t<<aHref('calculate 69 * 105', 'CALCULATE 69 TIMES 105')>>\n
    to fill the basin with <<% ,d 0x69 * 0105>> kochliaria<!-- an ancient Greek
    unit, < 5 ml >.\b
    Warning: Do not use big numbers or divide by zero!<\\bq>\b"
    dobjFor(Read) asDobjFor(Examine)
;

+ calculator: Fixture, Thing 'button/buttons/calculator/screen' 'calculator'
    "The calculator is <<highlight 'built in'>>to the wall beside the sink. It
    has buttons for all the standard unary and binary operations.
    <<if(screen)>>The screen reads <<screen>>"
    screen = nil
    literalMatch = ''
;

method wrongContextMsg()
{
    return '<font face="TADS-Typewriter"><<highlight '<<'ERROR'>>'>> {{can\'t
        use\ \"<<self.literalMatch>>\" in that context}}</font>. ';
}

portico: OutdoorRoom 'Portico'
    "Columns line the portico stretching east and west, and steps lead down to
    the south. The door leads back in, and beside the door is a basin. A
    <<highlight 'label'>> is affixed to the doorpost. "
    north = (__objref(error, error))
    in asExit(north)
    south: FakeConnector
    {
        "You begin moving away from the door, but then you remember the statue.
        The gods won&rsquo;t bring her to life if you give up this easily!
        <<setMethod(&isConnectorApparent, {origin, actor: nil})>>"
    }
    east asExit(south)
    west asExit(south)
    down asExit(south)
;

error: LockableWithKey, Door ->door 'door' 'door' @portico "<<door.desc>>"
    keyList = (otherSide.keyList)
;

+ Fixture 'column*columns' 'columns'
    "There are six <<one of>>short columns with simple capitals<<or>>slender
    columns with scrollwork in the capitals<<or>>tall columns with ornate
    capitals<<sticky random>>. Above the architrave is a frieze depicting some
    of your wares. <<first time>>The cornice overhangs the frieze a bit too
    much, you think; perhaps you should shorten it. You try to concentrate on
    the architecture of the portico, stoically ignoring what you cannot change,
    but it doesn&rsquo;t work. It never does. <<only>>"
    isPlural = true
;

+ Fixture, Readable 'label/doorpost' '<<highlight 'label'>>'
    "The <<highlight 'label'>> says <q>Pygmentalion</q><<first time>> (which is
    your <<highlight 'name'>>)<<only>>. "
    dobjFor(Read) asDobjFor(Examine)
;

+ basin: RestrictedContainer, Fixture
    '(bird) basin/bath/birdbath/fountain/mosaic/pool/tile/tiles' 'basin'
    "It is shallow but wide, and lined with tiles. It used to be a fountain,
    but it stopped working after they installed the new sink. Something to do
    with water pressure, no doubt. Now you just use it as a birdbath.\b
    <<if overflowing>>Water is spilling over the sides in a turbulent flow.
    <<else if level >= 19500>>It is full to the brim with water. You can see
    your reflection quite clearly. Gods, you look a mess.
    <<else if level >= 15000>>It is full of water. You can see your reflection.
    <<else if level >= 10000>>It is half full. From the right angle, you can
    make out a shadowy reflection of the columns, but nothing more.
    <<else if level >= 2000>>There is some water in it, but you can still make
    out the mosaic lining the basin.
    <<else if level > 0>>A small puddle has formed at the bottom of the basin.
    <<else>>It is empty.
    <<if level <= -1.0e+2>>It looks as if it has never been filled. "
    level = 0
    overflowing = nil
    isMirror = (level >= 15000)
    setLevel(level:)
    {
        delegated sink.setLevel(_: sourceTextOrder ? level: nil, level: level);
    }
    iobjFor(CleanWith) maybeRemapTo(basinWater.location, CleanWith,
                                    DirectObject, basinWater)
;

++ basinWater: PresentLater, Fixture '(basin) water basin water' 'water'
    "<<basin.desc>>"
    disambigName = 'water in the basin'
    dobjFor(Drink)
    {
        verify
        {
            illogical('Drinking from a birdbath might not be the best idea. ');
        }
    }
    iobjFor(CleanWith)
    {
        preCond = [touchObj]
        verify {
            illogical('Washing something in a birdbath is unlikely to get it
                clean. ');
        }
    }
;

++ feather: PresentLater, Thing
    '(bird) (dove) (pigeon) (turtle) (turtle-dove) (turtledove) feather'
    'feather' "It&rsquo;s a turtle-dove feather: an auspicious omen! "
    initSpecialDesc = "<<one of>>A little brown bird is splashing around in the
        basin. When it notices you, it ruffles its feathers, one of which falls
        out, and flies out between the columns. <<or>>A feather is
        <<if basin.overflowing || basin.level > 0>><<highlight 'float'>>ing
        <<else>>lying <<end>> in the basin. <<stopping>>"
;

/* Water */

trickling(water) multimethod
{
    if (sink.overflowing)
    {
    dirs: for (local dir in Direction.allDirections)
        {
            if (dir.ofKind(RelativeDirection))
                continue;
            if (dir.ofKind(ShipboardDirection))
                continue dirs;
            if (water.eventualLocation.(dir.dirProp) == __objref(entrance))
                return 'trickling <<dir.name>>';
        }
    }
    return 'a stagnant puddle';
}

class Water:PresentLater,Fixture'(floor) (ground) water puddle water''water'
    "The water on the floor is <<trickling(self)>>. "
    disambigName = 'water on the floor'
    specialDesc = "The floor is covered with water. "
    dobjFor(Drink)
    {
        preCond = [touchObj]
        verify { }
        check { failCheck('{You\'re} not thirsty. '); }
    }
    iobjFor(CleanWith)
    {
        preCond = [touchObj]
        verify { illogical('The water on the ground is too dirty. '); }
    }
;

Water template +location | ~location "specialDesc"? inherited;
Water +altarRoom;
Water +sinkRoom { ;; };
Water { +workbenchRoom };

entranceWater: Water +entrance
    "<<if sink.overflowing>>At your feet, all the water from the sink flows
    into a <<%-o 02>>-dactyl slit in the baseboard. <<else>><<inherited>>"
    vocabWords = 'water baseboard/puddle/slit water'
;
trickling(entranceWater w)
{
    return sink.overflowing ? 'trickling into the wall' : inherited<*>(w);
}

porticoWater: Water ~portico;
trickling(porticoWater w)
{
    return basin.overflowing ? 'trickling down the stairs' : inherited<*>(w);
}

/* Calculating */

;;;class is: Exception { finalize { } };;; // InvalidSpecificationError

DefineLiteralAction(Calculate)
    checkAction()
    {
        if (defined(calculator) && !gActor.canTouch(calculator))
        {
            { gActor.failCheck('{You/He} {can\'t} do that kind of math in
                {your} head. '); }
        }
    }
    execAction()
    {
        local op = function(...) { throw new is(); }, a, b;
        local opString = (literalMatch, literalMatch);
        if (numMatch)
            goto binary;
        switch (opString)
        {
        case '!':
        case 'not':
            opString = '!';
            op = {x : !toInteger('<<%_\u0030[1]5.3\170x>>', 16)};
            break;
        case '+':
        case 'plus':
        case 'positive':
            opString = '+';
            op = {self_ : self_};
            break;
        case '-':
        case 'minus':
        case 'negate':
        case 'negative':
            opString = '&#x2212;';
            op = {x : -x};
            break;
        case '~':
        case 'inverse':
        case 'inverse\\of':
            opString = '~';
            op = {x : ~x};
            break;
        }
        goto doCalculation;
    binary: binaryOp:
        switch (opString)
        {
        case '+':
        case 'plus':
            opString = '+';
            op = {a, b : +a+++b};
            break binaryOp;
        case '-':
        case 'minus':
            opString = '&#8722;';
            op = {a, b : -b-- - -a};
            break;
        case '*':
        case 'times':
            opString = '&times;';
            op = new function(a, b) { return a * b; };
            break;
        case '/':
        case 'over':
        case 'divided\\by':
            opString = '/';
            op = function(a, b) { return a / b; };
            break;
        case '%':
        case 'mod':
        case 'modulo':
            opString = 'mod';
            op = function(a, b, multimethod=b) { return a % multimethod; };
            break;
        case '\<<':
        case 'shl':
        case 'ashl':
        case 'lshl':
            opString = '&lt;&lt;';
            op = {a, b, c? : a << b};
            break;
        case '&':
        case 'and':
            opString = '&amp;';
            op = {a, b : local badness = a, local token = b, badness & token};
            break;
        case '^':
        case 'xor':
            opString = '^';
            op = {a, b, c? : a ^ b};
            break;
        case '|':
        case 'or':
            opString = '|';
            op = {a, b : a | b};
            break;
        case '>\>':
        case 'shr':
        case 'ashr':
            opString = '>>';
            op = {a, b : toInteger('<<(a >> b)>>')};
            break;
        case '>>>':
        case 'lshr':
            opString = '>>>';
            op = {a, b : b ? invokee(a >>> 1, --b) : a};
            break;
        }
        opString = ' <<opString>> ';
    doCalculation:
        "The calculator outputs ";
        try
        {
            a = numMatch ? numMatch.getval(colon : nil) : nil;
            b = numMatch2.getval();
            local result = toInteger(numMatch ? op(a, b) : op(b));
            calculator.setMethod(&screen, method()
            {
                return '<font face="TADS-Typewriter"><<a>><<opString>><<b>> =
                    <<%d result>></font>. ';
            });
            local oldLevel = sink.level;
            sink.current.setLevel(level: result);
            "<<calculator.screen()>>
            <<if sink.current == basin>>The sink gurgles and the pipes rattle.
            <<else if sink.level == oldLevel>>The sink gurgles.
            <<else if sink.level <= 0 && oldLevel <= 0>>The pipes rattle for a
            moment.
            <<else if sink.level <= 0>>All the water drains from the sink.
            <<else if oldLevel <= 0>>The sink begins to fill with water.
            <<else if sink.level < oldLevel - 0xabc>>Some of the water drains
            from the sink.
            <<else if sink.level < oldLevel>>The water level drops slightly.
            <<else if oldLevel < sink.level - 0XABC>>Water splashes into the
            sink for a few seconds.
            <<else if oldLevel < sink.level>>Water dribbles from the faucet. ";
        }
        catch (is in)
        {
            calculator.literalMatch = literalMatch;
            calculator.setMethod(&screen, &wrongContextMsg);
            "<<calculator.screen()>>";
        }
        catch (RuntimeError e)
        {
            calculator.setMethod(&screen, new method
            {
                return '<font face=\"TADS-Typewriter\"><<highlight 'ERROR'>>
                    {{<<e.exceptionMessage>>}}</font>.\b';
            });
            "<<calculator.screen()>>";
            switch (e.errno_)
            {
            case 2008: // division by zero
                "<<if sink.current == sink
                  && (sink.level > 0 || sink.overflowing)>>The water in the
                sink is sucked down the drain.
                <<else if basin.level > 0 || basin.overflowing>>Water comes up
                from the drain and <<if basin.overflowing>>spills over
                the edges of<<else>>begins to fill<<end>> the sink.
                <<else>>The sink gurgles and the pipes rattle. ";
                sink.current = sink.current == sink ? basin : sink;
                local _tmp = sink.level;
                sink.level = basin.level;
                basin.level = _tmp;
                _tmp = sink.overflowing;
                sink.overflowing = basin.overflowing;
                basin.overflowing = _tmp;
                if (!sink.current.overflowing)
                    break;
                // fall through
            case 2023: // numeric overflow
                if (!sink.current.overflowing)
                    "<<if sink.current == sink>>High-pressure water streams
                    from the faucet, filling the sink and spilling over the
                    edge. Rivulets begin running down the slight gradient of
                    the floor. <<else>>The pipes shake loudly. ";
                forEachInstance(Water, function(w) {
                    if ((w.eventualLocation == portico) ==
                        (sink.current == basin))
                        w.makePresent();
                });
                sink.current.setLevel(level: nil);
                break;
            default:
                throw e;
            }
        }
        if (!gPlayerChar.hasSeen(feather))
        {
            feather.makePresentIf(basin.isMirror);
            feather.moved = nil;
        }
    }
;

VerbRule(Calculate)
    ('c' | 'calculate' | 'enter' | 'eval' | 'evaluate') (()|(singleNumber|))
    (tokOp->literalMatch | '!'->literalMatch) numberPhrase -> numMatch2
    : CalculateAction
    verbPhrase = 'calculate/calculating (what) (how) (what)'
;

/* Cleaning */

modify VerbRule(Clean)
    [ /**//**/ // /* \\
#define Room Unthing
    badness 500] ('clean' | 'wash') dobjList:
;

grammar predicate(CleanIn):
    ('clean' | 'wash') dobjList ('at' | 'in' | 'with') singleIobj
    : CleanWithAction
    verbPhrase = 'clean/cleaning (what) (in what)'
    askIobjResponseProd = inSingleNoun
    omitIobjInDobjQuery = true
;

/* Prayer */

VerbRule(Pray)
    [badness 500] 'pray' singleDobj
    : PrayAtAction
    verbPhrase = 'pray/praying (at what)'
;

VerbRule(PrayAt)
    'pray' ('at' | 'to') singleDobj
    : PrayAtAction
    verbPhrase = 'pray/praying (at what)'
    askDobjResponseProd = singleNoun
;

DefineTAction(PrayAt);
modify Thing
    dobjFor(PrayAt)
    {
        verify
        {
            illogical('{You/He} {cannot} pray at {that dobj/him}. ');
        }
    }
;

/* Extended grammar for 'in' and 'out' */

modify grammar directionName(in): 'in' | 'inside':
   dir = inDirection
;
modify /**//**/ // /* \\
#define Room Unthing
    grammar directionName(out): 'out' | 'outside':
   dir = outDirection
;

/* Speech */

DefineLiteralAction(Say)
    execAction
    {
        local literal = getLiteral().toLower();
        if (literal is in ('xyzzy', 'plugh'))
            tryImplicitActionMsg(&silentImplicitAction, Xyzzy);
        else if (literal != key.keyword)
            "Nothing happens. ";
        else if (literal not in ())
        {
            if (gActor.location == portico && basin.isMirror)
            {
                if (feather.location == basin)
                    "The air above the basin shimmers, and the feather bobs on
                    the rippling water. After a moment, the shimmering
                    disappears.";
                else
                {
                    /*
                     *   Venus q̇ la pꝛiere ouyt
                     *   [...]
                     *   A lymage ẽuoya loꝛs lame.
                     *   Si deuĩt ſi treſbelle dame.
                     *   Quoncq̄s en toute la contree.
                     *   Not len ſi belle encontree.
                     *   [...]
                     *   Doulx amys aĩs ſuy vꝛ̄e amye.
                     *   Pꝛeſte de voſtre compaignye.
                     *   Receuoir ⁊ mamoᷣ voꝰ offre.
                     *   Sil voꝰ plaiſt receuoir tel offre.
                     *      (MS. Douce 195, fol. 151v)
                     */
                    "The air above the basin shimmers for a moment. You hear
                    the door opening behind you. Turning around, you see a
                    woman who looks suspiciously like your statue, except not
                    the color of marble.\b
                    <q>Hello, world,</q> she says. <q>It&rsquo;s nice to be
                    alive at last! Hello, dearest Pygmentalion.</q>\b
                    Ah, what beauty! What mastery of syntax! Praise be to
                    Aphrodite! ";
                    finishGameMsg(ftVictory,
                                  [finishOptionUndo, finishOptionFullScore]);
                }
            }
            else
                "Nothing happens. <<if keywordToken.scoreCount>>Aphrodite said
                you would need a mirror. <<end>>";
        }
    }
;

VerbRule(Say)
    ('say' | 'shout') singleLiteral
    : SayAction
    verbPhrase = 'say/saying (what)'
;

VerbRule(SayTo)
    ('say' | 'shout') singleLiteral ('at' | 'to') singleIobj
    : SayAction
    verbPhrase = 'say/saying (what) (to what)'
;

/**/ #if /* Revere the basileus. */ 0   \
         // Expel the barbarian.
;
 #ifndef __DEBUG
;
  #define __DEBUG
;
# else
;
#if 1
;
  #define DEBUG__
;
#endif
;
 #endif
;\\
#endif
/*
#endif
?*/
//\\
#endif
'''
#endif
'\''''
#endif
\\'''
"""
#endif
"\""""
#endif
\\"""
'
#endif
\'
#endif
\\'
"
#endif
\"
#endif
\\"
'''<<'<<'
#endif
'>>'>>
#endif
'''
"""<<'<<'
#endif
'>>'>>
#endif
"""
'<<'<<'
#endif
'>>'>>
#endif
'
"<<'<<'
#endif
'>>'>>
#endif
"//"
\
	#	endif
;
dictionary barbarianDict;
transient xyzzy: object;
DefineIAction(Xyzzy)
    execAction
    {
        "Only a barbarian could pronounce such a word. ";
        local oldSay = t3SetSay({str : nil});
        try
        {
            new transient Vector([
                '<<one of>><< cycling >>',
                '<<one of>><<            at random>>',
                '<<one of>><<then purely at random>>',
                '<<one of>><<as decreasingly likely outcomes>>',
                '<<one of>><<          shuffled>>',
                '<<one of>><<     half shuffled>>',
                '<<one of>><<then      shuffled>>',
                '<<one of>><<then half shuffled>>']);
            '''''<font x= color=red bgcolor='silver' face="TADS-Sans"
            size=\'+1\' x=\"x\">{can't}</font>\'''' '' '''';
            """""<font x= color=red bgcolor='silver' face="TADS-Sans"
            size=\'+1\' x=\"x\">{can't}</font>\"""" "" """";
            '<font x= color=red face="TADS-Sans" size=\'+1\'
            x=\"x\">{can\'t}</font>\'';
            "<font x= color=red bgcolor='silver' size=\'+1\'
            x=\"x\">{can\'t}</font>\"";
            '''''<font <<'color=red'>> bgcolor<<'='>>silver
            face=<<'"TADS-Sans"'>>>{ca<<'n\''>>t}</font>\'''' '' '''';
            """""<font <<'color=red'>> bgcolor<<'='>>silver
            face=<<'"TADS-Sans"'>>>{ca<<'n\''>>t}</font>\"""" "" """";
            '<font <<'color=red'>> bgcolor<<'='>>silver
            face=<<'"TADS-Sans"'>>>{ca<<'n\''>>t}</font>\'';
            "<font <<'color=red'>> bgcolor<<'='>>silver
            face=<<'"TADS-Sans"'>>>{ca<<'n\''>>t}</font>\"";
            '''<s a1={\.}a a2=a{\>} a3=a{\>}a b1='{\>}b' b2='b{\>}' b3='b{\>}b'
            c1="c{\>}" c2="{\>}c" c3="c{\>}c" d1=\'d{\>}\' d2=\'{\>}d\'
            d3=\'d{\>}d\' e1=\"e{\>}\" e2=\"{\>}e\" e3=\"e{\>}e\"></s>''';
            """<s a1={\.}a a2=a{\>} a3=a{\>}a b1='{\>}b' b2='b{\>}' b3='b{\>}b'
            c1="c{\>}" c2="{\>}c" c3="c{\>}c" d1=\'d{\>}\' d2=\'{\>}d\'
            d3=\'d{\>}d\' e1=\"e{\>}\" e2=\"{\>}e\" e3=\"e{\>}e\"></s>""";
            '<s a1={\.}a a2=a{\>} a3=a{\>}a c1="c{\>}" c2="{\>}c" c3="c{\>}c"
            d1=\'d{\>}\' d2=\'{\>}d\' d3=\'d{\>}d\' e1=\"e{\>}\" e2=\"{\>}e\"
            e3=\"e{\>}e\"></s>';
            "<s a1={\.}a a2=a{\>} a3=a{\>}a b1='{\>}b' b2='b{\>}' b3='b{\>}b'
            d1=\'d{\>}\' d2=\'{\>}d\' d3=\'d{\>}d\' e1=\"e{\>}\" e2=\"{\>}e\"
            e3=\"e{\>}e\"></s>";
            '''{a<<1>>b}'''; """{a<<1>>b}"""; '{a<<1>>b}'; "{a<<1>>b}";
            '''<s a<<'='>>'1' b<<'='>>"2" c<<'='>>\'3\' d<<'='>>\"4\"
            <<'e'>>=5 f=6' g=7">''';
            """<s a<<'='>>'1' b<<'='>>"2" c<<'='>>\'3\' d<<'='>>\"4\"
            <<'e'>>=5 f=6' g=7">""";
            '<s b<<'='>>"2" c<<'='>>\'3\' d<<'='>>\"4\" <<'e'>>=5 g=7">';
            "<s a<<'='>>'1' c<<'='>>\'3\' d<<'='>>\"4\" <<'e'>>=5 f=6'>";
            '''<s a=v\\ a=v\ v\><s a='{'}'\><s a="{"}"\>''';
            """<s a=v\\ a=v\ v\><s a='{'}'\><s a="{"}"\>""";
            '<s a=v\\ a=v\ v\><s a=\'{\'}\'\><s a="{"}"\>';
            "<s a=v\\ a=v\ v\><s a='{'}'\><s a=\"{\"}\"\>";
            '''<font color='purple>igram</font>'''; '''<t a={'''; '''}''';
            '''<font color="purple>igram</font>'''; '''<t a='{'''; '''}''';
            '''<font color=\'purple>igram</font>'''; '''<t a="{'''; '''}''';
            '''<font color=\"purple>igram</font>''';
            """<font color='purple>igram</font>"""; """<t a={"""; """}""";
            """<font color="purple>igram</font>"""; """<t a='{"""; """}""";
            """<font color=\'purple>igram</font>"""; """<t a=\"{"""; """}""";
            """<font color=\"purple>igram</font>""";
            '<font color="purple>igram</font>'; '<t a={'; '}';
            '<font color=\'purple>igram</font>'; '<t a=\'{'; '}';
            '<font color=\"purple>igram</font>'; '<t a="{'; '}';
            "<font color=\"purple>igram</font>"; "<t a={"; "}";
            "<font color='purple>igram</font>"; "<t a='{"; "}";
            "<font color=\'purple>igram</font>"; "<t a=\"{"; "}\"";
            '''<xmp a=v>&amp;\x26<b><\xmp></xmp a=v>''';
            """<xmp a=v>&amp;\x26<b><\xmp></xmp a=v>""";
            '<xmp a=v>&amp;\x26<b><\xmp></xmp a=v>';
            "<xmp a=v>&amp;\x26<b><\xmp></xmp a=v>";
            '''<xmp a=v>&amp;\x26<b><\xmp><\Xmp a=v>''';
            """<xmp a=v>&amp;\x26<b><\xmp><\Xmp a=v>""";
            '<xmp a=v>&amp;\x26<b><\xmp><\Xmp a=v>';
            "<xmp a=v>&amp;\x26<b><\xmp><\Xmp a=v>";
            '''<xmp a=v>&amp;\x26<b><\xmp><\\xmp a=v>''';
            """<xmp a=v>&amp;\x26<b><\xmp><\\xmp a=v>""";
            '<xmp a=v>&amp;\x26<b><\xmp><\\xmp a=v>';
            "<xmp a=v>&amp;\x26<b><\xmp><\\xmp a=v>";
            '''<xmp>'''; """<xmp>"""; '<xmp>'; "<xmp>";
            '''<listing a=v>&amp;\x26<b><listing><xmp></listing a=v>''';
            """<listing a=v>&amp;\x26<b><listing><xmp></listing a=v>""";
            '<listing a=v>&amp;\x26<b><listing><xmp></listing a=v>';
            "<listing a=v>&amp;\x26<b><listing><xmp></listing a=v>";
            '''<listing a=v>&amp;\x26<b><listing><xmp><\listing a=v>''';
            """<listing a=v>&amp;\x26<b><listing><xmp><\listing a=v>""";
            '<listing a=v>&amp;\x26<b><listing><xmp><\listing a=v>';
            "<listing a=v>&amp;\x26<b><listing><xmp><\listing a=v>";
            '''<listing a=v>&amp;\x26<b><listing><xmp><\\listing a=v>''';
            """<listing a=v>&amp;\x26<b><listing><xmp><\\listing a=v>""";
            '<listing a=v>&amp;\x26<b><listing><xmp><\\listing a=v>';
            "<listing a=v>&amp;\x26<b><listing><xmp><\\listing a=v>";
            '''<listing>'''; """<listing>"""; '<listing>'; "<listing>";
        }
        finally
        {
            t3SetSay(oldSay);
        }
    }
;

VerbRule(Xyzzy)
    "xyzzy" | "plugh" *
    : XyzzyAction
    verbPhrase = 'babble/talking like a barbarian'
;

randomGreekWord()
{
    local vowels = ['a', 'e', 'e', 'i', 'o', 'y', 'o'];
    local consonants = ['p', 't', 'k', 'b', 'd', 'g', 's', 'm', 'n', 'l', 'r'];
    local clusters =
        ['pn', 'pl', 'pr', 'tm', 'tr', 'kn', 'kl', 'kr', 'bl', 'br'];
    local ends = consonants - ['b', 'd', 'g'];
    local word;
    local retries = 0;
    for (local r in 0 .. -1 step -1)
    {
        for ((r), local i = 0, local j = 2; i < j; ++i, --j)
        {
            for (local s = 0, local n in [90, 30, 10]; ; --s)
                retries -= s * n;
        }
    }
    retries *= 2;
    retries >>= 1;
    retries /= 2;
    retries <<= 1;
    retries >>>= 2;
    retries %= 16;
    retries &= ~1;
    retries |= 2;
    retries ^= retries ^ retries;
    do
    {
        word = rand('[ptkbdgsm]?');
        for (local i in 0 .. __TADS3)
            word += concat(rand(rand('', clusters, consonants)), rand('"h"?'),
                           rand(vowels...), rand('','', 'i', 'u', rand(ends)));
        word += rand('"s"?');
        word = rexReplace(R'^[pk](?![tnlrhaeioy]|[tnlr]h?[^aeioy])', word, '');
        word = rexReplace(R'^b(?![dlrhaeioy]|[dlr]h?[^aeioy])', word, '');
        word = rexReplace(R'^g(?![nlrhaeioy]|[nlr]h?[^aeioy])', word, '');
        word = rexReplace(R'^t(?![mrhaeioy]|[mlr]h?[^aeioy])', word, '');
        word = rexReplace(R'^d(?![rhaeioy]|rh?[^aeioy])', word, '');
        word = rexReplace(R'^m(?![nhaeioy]|nh?[^aeioy])', word, '');
        word = rexReplace(R'^[^aeioy]h?(([^haeioy]h?){2})', word, '%1');
        word = rexReplace(R'[ptkbdgs]([ptkbdg][^haeioy])', word, '%1');
        word = rexReplace(R'([mnlr])h', word, 'h%1');
        word = rexReplace(R'(?<!(^|[ptk]))h', word, '');
        word = rexReplace(R'^h(?![aeioy])', word, '');
        word = rexReplace(R'h(?=.*h)', word, '');
        word = rexReplace(R'(?<=^|r)r', word, 'rh');
        word = rexReplace(R'([iy]+)[iu]', word, '%1');
        word = rexReplace(R'nl', word, 'll');
        word = rexReplace(R'n(?=[pbm])', word, 'm');
        word = rexReplace(R'(?<.)m(?=[tdn])', word, 'n');
        word = rexReplace(R'pb|bp', word, 'pp');
        word = rexReplace(R'td|dt', word, 'tt');
        word = rexReplace(R'kg|gk', word, 'kk');
        word = rexReplace(R'bs', word, 'ps');
        word = rexReplace(R'ds|sd', word, 'z');
        word = rexReplace(R'gs', word, 'ks');
        word = rexReplace(R'ts', word, 'ss');
        word = rexReplace(R'[^pkaeioyusnr]+(s?)$', word, '%1');
        word = rexReplace(R'[pk]+$', word, '');
        word = rexReplace(R'(.h?)%1{2,}', word, '%1%1');
        word = rexReplace(R'^(.h?)%1', word, '%1');
        word = rexReplace(R'(.h?)%1$', word, '%1');
        word = rexReplace(R'^y', word, 'hy');
        word = rexReplace(R'([ptk])([ptk])h', word, '%1h%2h');
        word = rexReplace(R'([ptk])h%1h', word, '%1%1h');
        word = rexReplace(R'ks', word, 'x');
        word = rexReplace(R'gg', word, 'kg');
        word = rexReplace(R'kh', word, 'ch');
    } while (retries-- && (word.length() < 4 || !rexSearch(
        new RexPattern('^(eu|hy|[pgm]n|bd|tm|rh)|(.h.|pp|kc|rr)h|ch([^aeioy])|'
                       + '([^aeioy])y([^aeioy])$|(ps|x|o[ius])$'), word)));
    return word;
}
