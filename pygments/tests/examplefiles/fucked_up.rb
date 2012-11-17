# vim:ft=ruby

events = Hash.new { |h, k| h[k] = [] }
DATA.read.split(/\n\n\n\s*/).each do |event|
	name = event[/^.*/].sub(/http:.*/, '')
	event[/\n.*/m].scan(/^([A-Z]{2}\S*)\s*(\S*)\s*(\S*)(\s*\S*)/) do |kind, day, daytime, comment|
		events[ [day, daytime] ] << [kind, name + comment]
	end
end

conflicts = 0
events.to_a.sort_by do |(day, daytime),|
	[%w(Mo Di Mi Do Fr).index(day) || 0, daytime]
end.each do |(day, daytime), names|
	if names.size > 1
		conflicts += 1
		print '!!! '
	end
	print "#{day} #{daytime}: "
	names.each { |kind, name| puts "  #{kind}  #{name}" }
	puts
end

puts '%d conflicts' % conflicts
puts '%d SWS' % (events.inject(0) { |sum, ((day, daytime),)| sum + (daytime[/\d+$/].to_i - daytime[/^\d+/].to_i) })

string = % foo     # strange. huh?
print "Escape here: \n"
print 'Dont escape here: \n'

__END__
Informatik und Informationsgesellschaft I: Digitale Medien (32 214)
Computer lassen ihre eigentliche Bestimmung durch Multimedia und Vernetzung erkennen: Es sind digitale Medien, die alle bisherigen Massen- und Kommunikationsmedien simulieren, kopieren oder ersetzen können. Die kurze Geschichte elektronischer Medien vom Telegramm bis zum Fernsehen wird so zur Vorgeschichte des Computers als Medium. Der Prozess der Mediatisierung der Rechnernetze soll in Technik, Theorie und Praxis untersucht werden. Das PR soll die Techniken der ortsverteilten und zeitversetzten Lehre an Hand praktischer Übungen vorführen und untersuchen.
VL 	Di	15-17	wöch.	RUD 25, 3.101	J. Koubek
VL	Do	15-17	wöch.	RUD 25, 3.101
UE/PR	Do	17-19	wöch.	RUD 25, 3.101	J.-M. Loebel


Methoden und Modelle des Systementwurfs (32 223)
Gute Methoden zum Entwurf und zur Verifikation von Systemen sind ein Schlüssel für gute Software. Dieses Seminar betrachtet moderne Entwurfsmethoden.
 VL	Di	09-11	wöch.	RUD 26, 0313	W. Reisig
 VL	Do	09-11	wöch.	RUD 26, 0313	
 UE	Di	11-13	wöch.	RUD 26, 0313	
 PR	Di	13-15	wöch.	RUD 26, 0313	D. Weinberg


Komplexitätstheorie (32 229)
In dieser Vorlesung untersuchen wir eine Reihe von wichtigen algorithmischen Problemstellungen aus verschiedenen Bereichen der Informatik. Unser besonderes Interesse gilt dabei der Abschätzung der Rechenressourcen, die zu ihrer Lösung aufzubringen sind.  Die Vorlesung bildet eine wichtige Grundlage für weiterführende Veranstaltungen in den Bereichen Algorithmen, Kryptologie, Algorithmisches Lernen und Algorithmisches Beweisen.
 VL 	Di	09-11	wöch.	RUD 26, 1303	J. Köbler
 VL	Do	09-11	wöch.	RUD 26, 1305	
 UE	Do	11-13	wöch.	RUD 26, 1305	


Zuverlässige Systeme (32 234)
Mit zunehmender Verbreitung der Computertechnologie in immer mehr Bereichen des menschlichen Lebens wird die Zuverlässigkeit solcher Systeme zu einer immer zentraleren Frage.
Der Halbkurs "Zuverlässige Systeme" konzentriert sich auf folgende Schwerpunkte: Zuverlässigkeit, Fehlertoleranz, Responsivität, Messungen, Anwendungen, Systemmodelle und Techniken, Ausfallverhalten, Fehlermodelle, Schedulingtechniken, Software/Hardware - responsives Systemdesign, Analyse und Synthese, Bewertung, Fallstudien in Forschung und Industrie.
Der Halbkurs kann mit dem Halbkurs "Eigenschaften mobiler und eingebetteter Systeme" zu einem Projektkurs kombiniert werden. Ein gemeinsames Projekt begleitet beide Halbkurse.
VL 	Di	09-11	wöch.	RUD 26, 1308	M. Malek
VL	Do	09-11	wöch.	RUD 26, 1308
PR	n.V.


Stochastik für InformatikerInnen (32 239)
Grundlagen der Wahrscheinlichkeitsrechnung, Diskrete und stetige Wahrscheinlichkeitsmodelle in der Informatik, Grenzwertsätze, Simulationsverfahren, Zufallszahlen, Statistische Schätz- und Testverfahren, Markoffsche Ketten, Simulated Annealing, Probabilistische Analyse von Algorithmen.
VL	Mo	09-11	wöch.	RUD 25, 3.101	W. Kössler
VL	Mi	09-11	wöch.	RUD 25, 3.101
UE	Mo	11-13	wöch.	RUD 25, 3.101
 UE	Mi	11-13	wöch.	RUD 25. 3.101


Geschichte der Informatik  Ausgewählte Kapitel (32 243)
VL	Mi	13-15	wöch.	RUD 25, 3.113	W. Coy


Aktuelle Themen der Theoretischen Informatik (32 260)
In diesem Seminar sollen wichtige aktuelle Veröffentlichungen aus der theoretischen Informatik gemeinsam erarbeitet werden. Genaueres wird erst kurz vor dem Seminar entschieden. Bei Interesse wenden Sie sich bitte möglichst frühzeitig an den Veranstalter.
 SE	Fr	09-11	wöch.	RUD 26, 1307	M. Grohe 
