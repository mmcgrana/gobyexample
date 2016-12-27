--
-- Example of a fragment of an openEHR Archetype, written in the Object Data Instance Notation (ODIN)
-- Definition available here: https://github.com/openEHR/odin
-- Author: Thomas Beale
--

    original_author = <
        ["name"] = <"Dr J Joyce">
        ["organisation"] = <"NT Health Service">
        ["date"] = <2003-08-03>
    >
    term_bindings = <
        ["umls"] = <
            ["id1"] = <http://umls.nlm.edu/id/C124305> -- apgar result
            ["id2"] = <http://umls.nlm.edu/id/0000000> -- 1-minute event 
        >
    >
    lifecycle_state =  <"initial">
    resource_package_uri =  <"http://www.aihw.org.au/data_sets/diabetic_archetypes.html">

    details = <
        ["en"] = <
            language = <[iso_639-1::en]>
            purpose =  <"archetype for diabetic patient review">
            use = <"used for all hospital or clinic-based diabetic reviews, 
                including first time. Optional sections are removed according to the particular review"
            >
            misuse = <"not appropriate for pre-diagnosis use">
            original_resource_uri = <"http://www.healthdata.org.au/data_sets/diabetic_review_data_set_1.html">
        >
        ["de"] = <
            language = <[iso_639-1::de]>
            purpose =  <"Archetyp für die Untersuchung von Patienten mit Diabetes">
            use = <"wird benutzt für alle Diabetes-Untersuchungen im
                    Krankenhaus, inklusive der ersten Vorstellung. Optionale
                    Abschnitte werden in Abhängigkeit von der speziellen
                    Vorstellung entfernt."
            >
            misuse = <"nicht geeignet für Benutzung vor Diagnosestellung">
            original_resource_uri = <"http://www.healthdata.org.au/data_sets/diabetic_review_data_set_1.html">
        >
    >
	
