# ==============================================================================
# load_lumen_data.R
#
# Funzione che carica LUMEN_DATA.csv, applica label e factor/levels.
# Restituisce il dataframe pronto per le analisi.
#
# Uso:
#   source("load_lumen_data.R")
#   data <- load_lumen_data()                     # cerca LUMEN_DATA.csv nella wd
#   data <- load_lumen_data("/path/to/file.csv")  # path esplicito
# ==============================================================================

library(Hmisc)

load_lumen_data <- function(file = 'LUMEN_DATA.csv') {
  data = read.csv(file)
#Setting Labels

  label(data$record_id)="Record ID"
  label(data$redcap_survey_identifier)="Survey Identifier"
  label(data$questionario_lumen_timestamp)="Survey Timestamp"
  label(data$consenso)="Dopo aver preso visione ed aver compreso linformativa che precede, relativa al progetto:"
  label(data$eta)="Quanti anni hai?"
  label(data$sesso_nascita)="Quale sesso ti è stato assegnato alla nascita?"
  label(data$trans_cis)="Ti identifichi come persona trans*?"
  label(data$identita_genere)="Qual è la tua identità di genere?"
  label(data$identita_genere_altro)="Per favore, specifica la tua identità di genere:"
  label(data$orientamento_sex)="Qual è il tuo orientamento sessuale?"
  label(data$orientamento_sex_altro)="Per favore, specifica il tuo orientamento sessuale:"
  label(data$regione)="In quale regione vivi?"
  label(data$centro_abitato)="In che tipo di centro abitato vivi?" 
  label(data$titolo_studio)="Qual è il titolo di studio più avanzato che hai conseguito?"
  label(data$titolo_studio_genitore1)="Qual è il titolo di studio più avanzato conseguito dal tuo genitore 1?"
  label(data$titolo_studio_genitore2)="Qual è il titolo di studio più avanzato conseguito dal tuo genitore 2?"
  label(data$lavoro_genitore1)="Qual è stata loccupazione principale del tuo genitore 1?"
  label(data$lavoro_genitore2)="Qual è stata loccupazione principale del tuo genitore 2?"
  label(data$luogo_nascita)="Qual è il tuo luogo di nascita?"
  label(data$luogo_nascita_altro)="Per favore, specifica il tuo luogo di nascita:"
  label(data$luogo_nascita_gen1)="Qual è il luogo di nascita del tuo genitore 1?"
  label(data$luogo_nascita_gen1_altro)="Per favore, specifica il luogo di nascita del tuo genitore 1:"
  label(data$luogo_nascita_gen2)="Qual è il luogo di nascita dal tuo genitore 2?"
  label(data$luogo_nascita_gen2_altro)="Per favore, specifica il luogo di nascita del tuo genitore 2?"
  label(data$figli)="Hai figli?"
  label(data$quanti_figli)="Quanti figli hai?"
  label(data$reddito)="Quanto è difficile per il tuo nucleo familiare arrivare a fine mese?"
  label(data$servizi_salute_mentale)="Hai mai usufruito di un servizio di salute mentale (per la tua persona)? Per servizio di salute mentale sintendono CPS, consultori, psicologi e psicoterapeuti, psichiatri, ..."
  label(data$diagnosi_si_no)="Hai mai avuto una diagnosi ufficiale in ambito di salute mentale?"
  label(data$aut)="Autismo"
  label(data$adhd)="ADHD"
  label(data$pers)="Disturbi di personalità"
  label(data$ansia)="Disturbi dansia"
  label(data$umore)="Disturbi dellumore"
  label(data$comp_al)="Disturbi del comportamento alimentare"
  label(data$appr)="Disturbi dellapprendimento"
  label(data$psicosi)="Psicosi / Schizofrenia"
  label(data$ocd)="Disturbo ossessivo-compulsivo"
  label(data$ptsd)="PTSD e disturbi dissociativi"
  label(data$dipendenza)="Dipendenza da sostanze e da gioco"
  label(data$altro)="Altro"
  label(data$altro_diagnosi)="Per favore, specifica la diagnosi:"
  label(data$livello_aut)="Quale livello di supporto ti è stato diagnosticato relativamente allautismo?"
  label(data$eta_aut)="A che età hai ricevuto la diagnosi di autismo? (Per favore, esprimi la tua età in numeri)"
  label(data$invio_aut)="Autismo"
  label(data$invio_adhd)="ADHD"
  label(data$invio_person)="Disturbi di personalità"
  label(data$invio_ansia)="Disturbi dansia"
  label(data$invio_umore)="Disturbi dellumore"
  label(data$invio_comp_al)="Disturbi del comportamento alimentare"
  label(data$invio_appr)="Disturbi dellapprendimento"
  label(data$invio_psicosi)="Psicosi / Schizofrenia"
  label(data$invio_ocd)="Disturbo ossessivo-compulsivo"
  label(data$invio_ptsd)="PTSD e disturbi dissociativi"
  label(data$invio_dipendenza)="Dipendenza da sostanze e da gioco"
  label(data$invio_altro)="[altro_diagnosi]"
  label(data$prima_visita_aut)="Autismo"
  label(data$prima_visita_adhd)="ADHD"
  label(data$prima_visita_pers)="Disturbi di personalità"
  label(data$prima_visita_ansia)="Disturbi dansia"
  label(data$prima_visita_umore)="Disturbi dellumore"
  label(data$prima_visita_comp_al)="Disturbi del comportamento alimentare"
  label(data$prima_visita_appr)="Disturbi dellapprendimento"
  label(data$prima_visita_psicosi)="Psicosi / Schizofrenia"
  label(data$prima_visita_ocd)="Disturbo ossessivo-compulsivo"
  label(data$prima_visita_ptsd)="PTSD e disturbi dissociativi"
  label(data$prima_visita_dipendenza)="Dipendenza da sostanze e da gioco"
  label(data$prima_visita_altro)="[altro_diagnosi]"
  label(data$att_diagnosi_aut)="Autismo"
  label(data$att_diagnosi_adhd)="ADHD"
  label(data$att_diagnosi_pers)="Disturbi di personalità"
  label(data$att_diagnosi_ansia)="Disturbi Dansia"
  label(data$att_diagnosi_umore)="Disturbi dellumore"
  label(data$att_diagnosi_comp_al)="Disturbi del comportamento alimentare"
  label(data$att_diagnosi_appr)="Disturbi dellapprendimento"
  label(data$att_diagnosi_psicosi)="Psicosi / Schizofrenia"
  label(data$att_diagnosi_ocd)="Disturbo ossessivo-compulsivo"
  label(data$att_diagnosi_ptsd)="PTSD e disturbi dissociativi"
  label(data$att_diagnosi_dipendenza)="Dipendenza da sostanze e da gioco"
  label(data$att_diagnosi_altro)="[altro_diagnosi]"
  label(data$sbagliate_aut)="Autismo"
  label(data$sbagliate_adhd)="ADHD"
  label(data$sbagliate_pers)="Disturbi di persoonalità"
  label(data$sbagliate_ansia)="Disturbi dansia"
  label(data$sbagliate_umore)="Disturbi dellumore"
  label(data$sbagliate_comp_al)="Disturbi del comportamento alimentare"
  label(data$sbagliate_appr)="Disturbi dellìapprendimento"
  label(data$sbagliate_psicosi)="Psicosi / Schizofrenia"
  label(data$sbagliate_ocd)="Disturbo ossessivo-compulsivo"
  label(data$sbagliate_ptsd)="PTSD e disturbi dissociativi"
  label(data$sbagliate_dipendenza)="Dipendenza da sostanze e da gioco"
  label(data$sbagliate_altro)="[altro_diagnosi]"
  label(data$sbagliate_quali)="Quali diagnosi sbagliate hai ricevuto? "
  label(data$costo_aut)="Autismo"
  label(data$costo_adhd)="ADHD"
  label(data$costo_pers)="Disturbi di personalità"
  label(data$costo_ansia)="Disturbi dansia"
  label(data$costo_umore)="Disturbi dellumore"
  label(data$costo_comp_al)="Disturbi del comportamento alimentare"
  label(data$costo_appr)="Disturbi dellapprendimento"
  label(data$costo_psicosi)="Psicosi / Schizofrenia"
  label(data$costo_ocd)="Disturbo ossessivo-compulsivo"
  label(data$costo_ptsd)="PTSD e disturbi dissociativi"
  label(data$costo_dipendenza)="Dipendenza da sostanze e da gioco"
  label(data$costo_altro)="[altro_diagnosi]"
  label(data$dove_aut)="Autismo"
  label(data$dove_adhd)="ADHD"
  label(data$dove_pers)="Disturbi di personalità"
  label(data$dove_ansia)="Disturbi dansia"
  label(data$dove_umore)="Disturbi dellumore"
  label(data$dove_comp_al)="Disturbi del comportamento alimentare"
  label(data$dove_appr)="Disturbi dellapprendimento"
  label(data$dove_psicosi)="Psicosi/Schizofrenia"
  label(data$dove_ocd)="Disturbo ossessivo-compulsivo"
  label(data$dove_ptsd)="PTSD e disturbi dissociativi"
  label(data$dove_dipendenza)="Dipendenza da sostanze e da gioco"
  label(data$dove_altro)="[altro_diagnosi]"
  label(data$n_prof_aut)="Autismo"
  label(data$n_prof_adhd)="ADHD"
  label(data$n_prof_pers)="Disturbi di personalità"
  label(data$n_prof_ansia)="Disturbi dansia"
  label(data$n_prof_umore)="Disturbi dellumore"
  label(data$n_prof_comp_al)="Disturbi del comportamento alimentare"
  label(data$n_prof_appr)="Disturbi dellapprendimento"
  label(data$n_prof_psicosi)="Psicosi / Schizofrenia"
  label(data$n_prof_ocd)="Disturbo ossessivo-compulsivo"
  label(data$n_prof_ptsd)="PTSD e disturbi dissociativi"
  label(data$n_prof_dipendenza)="Dipendenza da sostanze e da gioco"
  label(data$n_prof_altro)="[altro_diagnosi]"
  label(data$presa_carico_aut)="Relativamente alla diagnosi di autismo, dopo la diagnosi il centro o il professionista da cui lhai ottenuta:"
  label(data$percorsi_aut___1)="Relativamente alla diagnosi di autismo, cosa ti ha proposto il centro o il professionista? (choice=Percorso psicoeducativo (incontri informativi/educativi sulla diagnosi o problematiche complesse))"
  label(data$percorsi_aut___2)="Relativamente alla diagnosi di autismo, cosa ti ha proposto il centro o il professionista? (choice=Percorso psicologico o psicoterapico di gruppo o individuale)"
  label(data$percorsi_aut___3)="Relativamente alla diagnosi di autismo, cosa ti ha proposto il centro o il professionista? (choice=Monitoraggio farmacologico)"
  label(data$percorsi_aut___4)="Relativamente alla diagnosi di autismo, cosa ti ha proposto il centro o il professionista? (choice=Presa in carico da assistenti sociali)"
  label(data$percorsi_aut___5)="Relativamente alla diagnosi di autismo, cosa ti ha proposto il centro o il professionista? (choice=Gruppo di auto mutuo aiuto)"
  label(data$percorsi_aut___6)="Relativamente alla diagnosi di autismo, cosa ti ha proposto il centro o il professionista? (choice=Ricovero)"
  label(data$percorsi_aut___7)="Relativamente alla diagnosi di autismo, cosa ti ha proposto il centro o il professionista? (choice=Centro diurno)"
  label(data$percorsi_aut___8)="Relativamente alla diagnosi di autismo, cosa ti ha proposto il centro o il professionista? (choice=Non mi ha proposto niente)"
  label(data$durata_percorsi)="Se hai iniziato uno di questi percorsi, quanto è durata la presa in carico?"
  label(data$sei_in_carico)="Sei attualmente in carico?"
  label(data$interruzione_percorso)="La decisione di interrompere il percorso è stata tua o del curante?"
  label(data$inter_percorso_fattori)="Per favore, specifica quali fattori hanno causato linterruzione del percorso: "
  label(data$prior_ric_cause)="Cause della condizione (genetiche, biochimiche, ambientali...)"
  label(data$prior_ric_diagnosi)="Diagnosi (test, criteri diagnostici, bias di genere...)"
  label(data$prior_ric_prevenzione)="Prevenzione (interventi precoci, identificazione precoce)"
  label(data$prior_ric_psicosoc)="Interventi psicosociali (psicologici, educativi, sensoriali, supporto alla famiglia...)"
  label(data$prior_ric_farmaci)="Interventi farmacologici (efficacia, effetti collaterali...)"
  label(data$prior_ric_prognosi)="Prognosi (traiettorie di sviluppo, evoluzione del quadro...)"
  label(data$prior_ric_accesso)="Accesso ai servizi di salute"
  label(data$prior_ric_relazioni)="Aspetti relazionali (relazioni significative e affettive, interazioni quotidiane, sessualità, genitorialità...)"
  label(data$prior_ric_indipendenza)="Vita indipendente (inserimento lavorativo, abilità della vita quotidiana, indipendenza abitativa,...)"
  label(data$prior_aut_sal_ment)="Interventi per migliorare la salute mentale o ridurre i problemi di salute mentale nelle persone autistiche"
  label(data$prior_aut_comunicaz)="Interventi per lo sviluppo di abilità comunicative o linguistiche nellautismo"
  label(data$prior_aut_sociale)="Supporto o ottenimento di assistenza sociale per adulti autistici"
  label(data$prior_aut_ansia)="Interventi per la riduzione dellansia nelle persone autistiche"
  label(data$prior_aut_formaz)="Identificazione di contesti o supporti più appropriati per ottenere i migliori esiti di formazione/vita/abilità sociali nelle persone autistiche"
  label(data$prior_aut_famiglia)="Supporto o formazione per genitori e familiari rispetto al prendersi cura e a comprendere meglio un parente autistico"
  label(data$prior_aut_diagnosi)="Implementazione di criteri diagnostici per lautismo per renderli più rilevanti per la popolazione adulta"
  label(data$prior_aut_lavoro)="Interventi sui datori di lavoro per favorire il supporto alla persona autistica con la finalità di massimizzare il suo potenziale e la sua performance sul posto di lavoro"
  label(data$prior_aut_sensorialita)="Comprensione dellelaborazione sensoriale nellautismo"
  label(data$prior_aut_servizi)="Miglioramento e adattamento della fruibilità dei servizi per le persone autistiche e per i loro bisogni"
  label(data$prior_salute_mentale)="Salute mentale"
  label(data$prior_salute_igiene)="Igiene della persona"
  label(data$prior_salute_dentale)="Igiene orale e salute dentale"
  label(data$prior_salute_sessuale)="Salute riproduttiva e sessuale"
  label(data$prior_salute_sonno)="Medicina del sonno"
  label(data$prior_salute_nutrizione)="Nutrizione e alimentazione"
  label(data$prior_salute_oncologia)="Screening oncologici"
  label(data$prior_salute_dolore)="Controllo del dolore"
  label(data$prior_salute_gastro)="Disturbi gastrointestinali"
  label(data$prior_salute_autoimmune)="Patologie autoimmuni, disturbi immunologici e allergie"
  label(data$prior_salute_dermato)="Controlli dermatologici"
  label(data$prior_salute_endo)="Disturbi endocrinologici e del metabolismo"
  label(data$prior_salute_mobilita)="Promozione di mobilità sana e attività fisica"
  label(data$prior_salute_neuro)="Disturbi neurologici (cefalea, epilessia...)"
  label(data$prior_salute_respir)="Patologie respiratorie"
  label(data$prior_salute_cardio)="Patologie cardiovascolari"
  label(data$prior_salute_emato)="Patologie ematologiche"
  label(data$prior_salute_vista_udito)="Vista e udito"
  label(data$prior_salute_urinari)="Disturbi urinari"
  label(data$prior_salute_infettive)="Malattie infettive e trasmissibili"
  label(data$prior_salute_invec)="Invecchiamento"
  label(data$prior_salute_urgenza)="Servizi durgenza (pronto soccorso, guardia medica...)"
  label(data$prior_salute_fisio)="Fisioterapia"
  label(data$email)="Questo progetto comprende anche una seconda fase, che prevede unintervista, dal vivo oppure online. Se vuoi partecipare, puoi lasciare la tua email. Grazie mille!"
  label(data$questionario_lumen_complete)="Complete?"
#Setting Units


#Setting Factors(will create new variable for factors)
  data$consenso.factor = factor(data$consenso,levels=c("1","2"))
  data$eta.factor = factor(data$eta,levels=c("1","2","3","4","6","7"))
  data$sesso_nascita.factor = factor(data$sesso_nascita,levels=c("1","2","3"))
  data$trans_cis.factor = factor(data$trans_cis,levels=c("1","2"))
  data$identita_genere.factor = factor(data$identita_genere,levels=c("1","2","3","4","5","6","7","8","9"))
  data$orientamento_sex.factor = factor(data$orientamento_sex,levels=c("1","2","3","4","5","6","7","8","9","10","11","12"))
  data$regione.factor = factor(data$regione,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"))
  data$centro_abitato.factor = factor(data$centro_abitato,levels=c("1","2","3"))
  data$titolo_studio.factor = factor(data$titolo_studio,levels=c("1","2","3","4","5","6"))
  data$titolo_studio_genitore1.factor = factor(data$titolo_studio_genitore1,levels=c("1","2","3","4","5","6"))
  data$titolo_studio_genitore2.factor = factor(data$titolo_studio_genitore2,levels=c("1","2","3","4","5","6"))
  data$luogo_nascita.factor = factor(data$luogo_nascita,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187","188","189","190","191","192","193","194","195","196","197"))
  data$luogo_nascita_gen1.factor = factor(data$luogo_nascita_gen1,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187","188","189","190","191","192","193","194","195","196","197"))
  data$luogo_nascita_gen2.factor = factor(data$luogo_nascita_gen2,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187","188","189","190","191","192","193","194","195","196","197"))
  data$figli.factor = factor(data$figli,levels=c("1","0"))
  data$quanti_figli.factor = factor(data$quanti_figli,levels=c("1","2","3","4","5"))
  data$reddito.factor = factor(data$reddito,levels=c("1","2","3","4","5","6"))
  data$servizi_salute_mentale.factor = factor(data$servizi_salute_mentale,levels=c("1","2","3","4"))
  data$diagnosi_si_no.factor = factor(data$diagnosi_si_no,levels=c("1","2"))
  data$aut.factor = factor(data$aut,levels=c("1","2","3","4"))
  data$adhd.factor = factor(data$adhd,levels=c("1","2","3","4"))
  data$pers.factor = factor(data$pers,levels=c("1","2","3","4"))
  data$ansia.factor = factor(data$ansia,levels=c("1","2","3","4"))
  data$umore.factor = factor(data$umore,levels=c("1","2","3","4"))
  data$comp_al.factor = factor(data$comp_al,levels=c("1","2","3","4"))
  data$appr.factor = factor(data$appr,levels=c("1","2","3","4"))
  data$psicosi.factor = factor(data$psicosi,levels=c("1","2","3","4"))
  data$ocd.factor = factor(data$ocd,levels=c("1","2","3","4"))
  data$ptsd.factor = factor(data$ptsd,levels=c("1","2","3","4"))
  data$dipendenza.factor = factor(data$dipendenza,levels=c("1","2","3","4"))
  data$altro.factor = factor(data$altro,levels=c("1","2","3","4"))
  data$livello_aut.factor = factor(data$livello_aut,levels=c("1","2","3","4"))
  data$invio_aut.factor = factor(data$invio_aut,levels=c("1","2","3","4","5","6","7","8"))
  data$invio_adhd.factor = factor(data$invio_adhd,levels=c("1","2","3","4","5","6","7","8"))
  data$invio_person.factor = factor(data$invio_person,levels=c("1","2","3","4","5","6","7","8"))
  data$invio_ansia.factor = factor(data$invio_ansia,levels=c("1","2","3","4","5","6","7","8"))
  data$invio_umore.factor = factor(data$invio_umore,levels=c("1","2","3","4","5","6","7","8"))
  data$invio_comp_al.factor = factor(data$invio_comp_al,levels=c("1","2","3","4","5","6","7","8"))
  data$invio_appr.factor = factor(data$invio_appr,levels=c("1","2","3","4","5","6","7","8"))
  data$invio_psicosi.factor = factor(data$invio_psicosi,levels=c("1","2","3","4","5","6","7","8"))
  data$invio_ocd.factor = factor(data$invio_ocd,levels=c("1","2","3","4","5","6","7","8"))
  data$invio_ptsd.factor = factor(data$invio_ptsd,levels=c("1","2","3","4","5","6","7","8"))
  data$invio_dipendenza.factor = factor(data$invio_dipendenza,levels=c("1","2","3","4","5","6","7","8"))
  data$invio_altro.factor = factor(data$invio_altro,levels=c("1","2","3","4","5","6","7","8"))
  data$prima_visita_aut.factor = factor(data$prima_visita_aut,levels=c("1","2","3","4","5","6"))
  data$prima_visita_adhd.factor = factor(data$prima_visita_adhd,levels=c("1","2","3","4","5","6"))
  data$prima_visita_pers.factor = factor(data$prima_visita_pers,levels=c("1","2","3","4","5","6"))
  data$prima_visita_ansia.factor = factor(data$prima_visita_ansia,levels=c("1","2","3","4","5","6"))
  data$prima_visita_umore.factor = factor(data$prima_visita_umore,levels=c("1","2","3","4","5","6"))
  data$prima_visita_comp_al.factor = factor(data$prima_visita_comp_al,levels=c("1","2","3","4","5","6"))
  data$prima_visita_appr.factor = factor(data$prima_visita_appr,levels=c("1","2","3","4","5","6"))
  data$prima_visita_psicosi.factor = factor(data$prima_visita_psicosi,levels=c("1","2","3","4","5","6"))
  data$prima_visita_ocd.factor = factor(data$prima_visita_ocd,levels=c("1","2","3","4","5","6"))
  data$prima_visita_ptsd.factor = factor(data$prima_visita_ptsd,levels=c("1","2","3","4","5","6"))
  data$prima_visita_dipendenza.factor = factor(data$prima_visita_dipendenza,levels=c("1","2","3","4","5","6"))
  data$prima_visita_altro.factor = factor(data$prima_visita_altro,levels=c("1","2","3","4","5","6"))
  data$att_diagnosi_aut.factor = factor(data$att_diagnosi_aut,levels=c("1","2","3","4","5","6"))
  data$att_diagnosi_adhd.factor = factor(data$att_diagnosi_adhd,levels=c("1","2","3","4","5","6"))
  data$att_diagnosi_pers.factor = factor(data$att_diagnosi_pers,levels=c("1","2","3","4","5","6"))
  data$att_diagnosi_ansia.factor = factor(data$att_diagnosi_ansia,levels=c("1","2","3","4","5","6"))
  data$att_diagnosi_umore.factor = factor(data$att_diagnosi_umore,levels=c("1","2","3","4","5","6"))
  data$att_diagnosi_comp_al.factor = factor(data$att_diagnosi_comp_al,levels=c("1","2","3","4","5","6"))
  data$att_diagnosi_appr.factor = factor(data$att_diagnosi_appr,levels=c("1","2","3","4","5","6"))
  data$att_diagnosi_psicosi.factor = factor(data$att_diagnosi_psicosi,levels=c("1","2","3","4","5","6"))
  data$att_diagnosi_ocd.factor = factor(data$att_diagnosi_ocd,levels=c("1","2","3","4","5","6"))
  data$att_diagnosi_ptsd.factor = factor(data$att_diagnosi_ptsd,levels=c("1","2","3","4","5","6"))
  data$att_diagnosi_dipendenza.factor = factor(data$att_diagnosi_dipendenza,levels=c("1","2","3","4","5","6"))
  data$att_diagnosi_altro.factor = factor(data$att_diagnosi_altro,levels=c("1","2","3","4","5","6"))
  data$sbagliate_aut.factor = factor(data$sbagliate_aut,levels=c("1","0"))
  data$sbagliate_adhd.factor = factor(data$sbagliate_adhd,levels=c("1","0"))
  data$sbagliate_pers.factor = factor(data$sbagliate_pers,levels=c("1","0"))
  data$sbagliate_ansia.factor = factor(data$sbagliate_ansia,levels=c("1","0"))
  data$sbagliate_umore.factor = factor(data$sbagliate_umore,levels=c("1","0"))
  data$sbagliate_comp_al.factor = factor(data$sbagliate_comp_al,levels=c("1","0"))
  data$sbagliate_appr.factor = factor(data$sbagliate_appr,levels=c("1","0"))
  data$sbagliate_psicosi.factor = factor(data$sbagliate_psicosi,levels=c("1","0"))
  data$sbagliate_ocd.factor = factor(data$sbagliate_ocd,levels=c("1","0"))
  data$sbagliate_ptsd.factor = factor(data$sbagliate_ptsd,levels=c("1","0"))
  data$sbagliate_dipendenza.factor = factor(data$sbagliate_dipendenza,levels=c("1","0"))
  data$sbagliate_altro.factor = factor(data$sbagliate_altro,levels=c("1","0"))
  data$costo_aut.factor = factor(data$costo_aut,levels=c("1","2","3","4","5"))
  data$costo_adhd.factor = factor(data$costo_adhd,levels=c("1","2","3","4","5"))
  data$costo_pers.factor = factor(data$costo_pers,levels=c("1","2","3","4","5"))
  data$costo_ansia.factor = factor(data$costo_ansia,levels=c("1","2","3","4","5"))
  data$costo_umore.factor = factor(data$costo_umore,levels=c("1","2","3","4","5"))
  data$costo_comp_al.factor = factor(data$costo_comp_al,levels=c("1","2","3","4","5"))
  data$costo_appr.factor = factor(data$costo_appr,levels=c("1","2","3","4","5"))
  data$costo_psicosi.factor = factor(data$costo_psicosi,levels=c("1","2","3","4","5"))
  data$costo_ocd.factor = factor(data$costo_ocd,levels=c("1","2","3","4","5"))
  data$costo_ptsd.factor = factor(data$costo_ptsd,levels=c("1","2","3","4","5"))
  data$costo_dipendenza.factor = factor(data$costo_dipendenza,levels=c("1","2","3","4","5"))
  data$costo_altro.factor = factor(data$costo_altro,levels=c("1","2","3","4","5"))
  data$dove_aut.factor = factor(data$dove_aut,levels=c("1","2","3","4","5"))
  data$dove_adhd.factor = factor(data$dove_adhd,levels=c("1","2","3","4","5"))
  data$dove_pers.factor = factor(data$dove_pers,levels=c("1","2","3","4","5"))
  data$dove_ansia.factor = factor(data$dove_ansia,levels=c("1","2","3","4","5"))
  data$dove_umore.factor = factor(data$dove_umore,levels=c("1","2","3","4","5"))
  data$dove_comp_al.factor = factor(data$dove_comp_al,levels=c("1","2","3","4","5"))
  data$dove_appr.factor = factor(data$dove_appr,levels=c("1","2","3","4","5"))
  data$dove_psicosi.factor = factor(data$dove_psicosi,levels=c("1","2","3","4","5"))
  data$dove_ocd.factor = factor(data$dove_ocd,levels=c("1","2","3","4","5"))
  data$dove_ptsd.factor = factor(data$dove_ptsd,levels=c("1","2","3","4","5"))
  data$dove_dipendenza.factor = factor(data$dove_dipendenza,levels=c("1","2","3","4","5"))
  data$dove_altro.factor = factor(data$dove_altro,levels=c("1","2","3","4","5"))
  data$n_prof_aut.factor = factor(data$n_prof_aut,levels=c("1","2","3","4","5","6","7"))
  data$n_prof_adhd.factor = factor(data$n_prof_adhd,levels=c("1","2","3","4","5","6","7"))
  data$n_prof_pers.factor = factor(data$n_prof_pers,levels=c("1","2","3","4","5","6","7"))
  data$n_prof_ansia.factor = factor(data$n_prof_ansia,levels=c("1","2","3","4","5","6","7"))
  data$n_prof_umore.factor = factor(data$n_prof_umore,levels=c("1","2","3","4","5","6","7"))
  data$n_prof_comp_al.factor = factor(data$n_prof_comp_al,levels=c("1","2","3","4","5","6","7"))
  data$n_prof_appr.factor = factor(data$n_prof_appr,levels=c("1","2","3","4","5","6","7"))
  data$n_prof_psicosi.factor = factor(data$n_prof_psicosi,levels=c("1","2","3","4","5","6","7"))
  data$n_prof_ocd.factor = factor(data$n_prof_ocd,levels=c("1","2","3","4","5","6","7"))
  data$n_prof_ptsd.factor = factor(data$n_prof_ptsd,levels=c("1","2","3","4","5","6","7"))
  data$n_prof_dipendenza.factor = factor(data$n_prof_dipendenza,levels=c("1","2","3","4","5","6","7"))
  data$n_prof_altro.factor = factor(data$n_prof_altro,levels=c("1","2","3","4","5","6","7"))
  data$presa_carico_aut.factor = factor(data$presa_carico_aut,levels=c("1","2","3","4"))
  data$percorsi_aut___1.factor = factor(data$percorsi_aut___1,levels=c("0","1"))
  data$percorsi_aut___2.factor = factor(data$percorsi_aut___2,levels=c("0","1"))
  data$percorsi_aut___3.factor = factor(data$percorsi_aut___3,levels=c("0","1"))
  data$percorsi_aut___4.factor = factor(data$percorsi_aut___4,levels=c("0","1"))
  data$percorsi_aut___5.factor = factor(data$percorsi_aut___5,levels=c("0","1"))
  data$percorsi_aut___6.factor = factor(data$percorsi_aut___6,levels=c("0","1"))
  data$percorsi_aut___7.factor = factor(data$percorsi_aut___7,levels=c("0","1"))
  data$percorsi_aut___8.factor = factor(data$percorsi_aut___8,levels=c("0","1"))
  data$durata_percorsi.factor = factor(data$durata_percorsi,levels=c("0","1","2","3","4","5","6"))
  data$sei_in_carico.factor = factor(data$sei_in_carico,levels=c("1","2"))
  data$interruzione_percorso.factor = factor(data$interruzione_percorso,levels=c("1","2","3"))
  data$questionario_lumen_complete.factor = factor(data$questionario_lumen_complete,levels=c("0","1","2"))

  levels(data$consenso.factor)=c("ACCONSENTO alla compilazione del questionario ed al trattamento dei dati personali","NON ACCONSENTO alla compilazione del questionario ed al trattamento dei dati personali")
  levels(data$eta.factor)=c("Meno di 18 anni","Tra 18 e 25 anni","Tra 26 e 35 anni","Tra 36 e 45 anni","Tra 46 e 50 anni","Più di 50 anni")
  levels(data$sesso_nascita.factor)=c("Maschile","Femminile","Intersex")
  levels(data$trans_cis.factor)=c("Sì","No")
  levels(data$identita_genere.factor)=c("Uomo","Donna","Nonbinary","Bigender","Agender","Genderqueer","Genderfluid","Questioning","Altro")
  levels(data$orientamento_sex.factor)=c("Eterosessuale","Gay","Lesbica","Fluido","Bisessuale","Pansessuale","Asessuale","Queer","Questioning / non sicurə","Demisessuale","Greysessuale","Altro")
  levels(data$regione.factor)=c("Abruzzo","Basilicata","Calabria","Campania","Emilia Romagna","Friuli Venezia Giulia","Lazio","Liguria","Lombardia","Marche","Molise","Piemonte","Puglia","Sardegna","Sicilia","Toscana","Trentino Alto Adige","Umbria","Val dAosta","Veneto")
  levels(data$centro_abitato.factor)=c("Città (oltre 50.000 abitanti)","Paese (tra 5.000 e 50.000 abitanti)","Area rurale (meno di 5.000 abitanti)")
  levels(data$titolo_studio.factor)=c("Licenza elementare","Licenza media","Diploma di scuola secondaria di secondo grado","Laurea triennale","Laurea magistrale","Dottorato di ricerca")
  levels(data$titolo_studio_genitore1.factor)=c("Licenza elementare","Licenza media","Diploma di scuola secondaria di secondo grado","Laurea triennale","Laurea magistrale","Dottorato di ricerca")
  levels(data$titolo_studio_genitore2.factor)=c("Licenza elementare","Licenza media","Diploma di scuola secondaria di secondo grado","Laurea triennale","Laurea magistrale","Dottorato di ricerca")
  levels(data$luogo_nascita.factor)=c("Afghanistan","Albania","Algeria","Andorra","Angola","Antigua e Barbuda","Arabia Saudita","Argentina","Armenia","Australia","Austria","Azerbaigian","Bahamas","Bahrein","Bangladesh","Barbados","Belgio","Belize","Benin","Bhutan","Bielorussia","Birmania","Bolivia","Bosnia ed Erzegovina","Botswana","Brasile","Brunei","Bulgaria","Burkina Faso","Burundi","Cambogia","Camerun","Canada","Capo Verde","Ciad","Cile","Cina","Cipro","Colombia","Comore","Corea del Nord","Corea del Sud","Costa dAvorio","Costa Rica","Croazia","Cuba","Danimarca","Dominica","Ecuador","Egitto","El Salvador","Emirati Arabi Uniti","Eritrea","Estonia","Etiopia","Figi","Filippine","Finlandia","Francia","Gabon","Gambia","Georgia","Germania","Ghana","Giamaica","Giappone","Gibuti","Giordania","Grecia","Grenada","Guatemala","Guinea","Guinea-Bissau","Guinea Equatoriale","Guyana","Haiti","Honduras","India","Indonesia","Iran","Iraq","Irlanda","Islanda","Isole Cook","Isole Marshall","Isole Salomone","Italia","Kazakistan","Kenya","Kirghizistan","Kiribati","Kuwait","Laos","Lesotho","Lettonia","Libano","Liberia","Libia","Liechtenstein","Lituania","Lussemburgo","Macedonia del Nord","Madagascar","Malawi","Malaysia","Maldive","Mali","Malta","Marocco","Mauritania","Mauritius","Messico","Micronesia","Moldavia","Monaco","Mongolia","Montenegro","Mozambico","Namibia","Nauru","Nepal","Nicaragua","Niger","Nigeria","Niue","Norvegia","Nuova Zelanda","Oman","Paesi Bassi","Pakistan","Palau","Palestina","Panama","Papua Nuova Guinea","Paraguay","Perù","Polonia","Portogallo","Qatar","Regno Unito","Repubblica Ceca","Repubblica Centrafricana","Repubblica del Congo","Repubblica Democratica del Congo","Repubblica Dominicana","Romania","Ruanda","Russia","Saint Kitts e Nevis","Saint Lucia","Saint Vincent e Grenadine","Samoa","San Marino","São Tomé e Príncipe","Senegal","Serbia","Seychelles","Sierra Leone","Singapore","Siria","Slovacchia","Slovenia","Somalia","Spagna","Sri Lanka","Stati Uniti","Sudafrica","Sudan","Sudan del Sud","Suriname","Svezia","Svizzera","eSwatini","Tagikistan","Taiwan","Tanzania","Thailandia","Timor Est","Togo","Tonga","Trinidad e Tobago","Tunisia","Turchia","Turkmenistan","Tuvalu","Ucraina","Uganda","Ungheria","Uruguay","Uzbekistan","Vanuatu","Venezuela","Vietnam","Yemen","Zambia","Zimbabwe","Altro luogo")
  levels(data$luogo_nascita_gen1.factor)=c("Afghanistan","Albania","Algeria","Andorra","Angola","Antigua e Barbuda","Arabia Saudita","Argentina","Armenia","Australia","Austria","Azerbaigian","Bahamas","Bahrein","Bangladesh","Barbados","Belgio","Belize","Benin","Bhutan","Bielorussia","Birmania","Bolivia","Bosnia ed Erzegovina","Botswana","Brasile","Brunei","Bulgaria","Burkina Faso","Burundi","Cambogia","Camerun","Canada","Capo Verde","Ciad","Cile","Cina","Cipro","Colombia","Comore","Corea del Nord","Corea del Sud","Costa dAvorio","Costa Rica","Croazia","Cuba","Danimarca","Dominica","Ecuador","Egitto","El Salvador","Emirati Arabi Uniti","Eritrea","Estonia","Etiopia","Figi","Filippine","Finlandia","Francia","Gabon","Gambia","Georgia","Germania","Ghana","Giamaica","Giappone","Gibuti","Giordania","Grecia","Grenada","Guatemala","Guinea","Guinea-Bissau","Guinea Equatoriale","Guyana","Haiti","Honduras","India","Indonesia","Iran","Iraq","Irlanda","Islanda","Isole Cook","Isole Marshall","Isole Salomone","Italia","Kazakistan","Kenya","Kirghizistan","Kiribati","Kuwait","Laos","Lesotho","Lettonia","Libano","Liberia","Libia","Liechtenstein","Lituania","Lussemburgo","Macedonia del Nord","Madagascar","Malawi","Malaysia","Maldive","Mali","Malta","Marocco","Mauritania","Mauritius","Messico","Micronesia","Moldavia","Monaco","Mongolia","Montenegro","Mozambico","Namibia","Nauru","Nepal","Nicaragua","Niger","Nigeria","Niue","Norvegia","Nuova Zelanda","Oman","Paesi Bassi","Pakistan","Palau","Palestina","Panama","Papua Nuova Guinea","Paraguay","Perù","Polonia","Portogallo","Qatar","Regno Unito","Repubblica Ceca","Repubblica Centrafricana","Repubblica del Congo","Repubblica Democratica del Congo","Repubblica Dominicana","Romania","Ruanda","Russia","Saint Kitts e Nevis","Saint Lucia","Saint Vincent e Grenadine","Samoa","San Marino","São Tomé e Príncipe","Senegal","Serbia","Seychelles","Sierra Leone","Singapore","Siria","Slovacchia","Slovenia","Somalia","Spagna","Sri Lanka","Stati Uniti","Sudafrica","Sudan","Sudan del Sud","Suriname","Svezia","Svizzera","eSwatini","Tagikistan","Taiwan","Tanzania","Thailandia","Timor Est","Togo","Tonga","Trinidad e Tobago","Tunisia","Turchia","Turkmenistan","Tuvalu","Ucraina","Uganda","Ungheria","Uruguay","Uzbekistan","Vanuatu","Venezuela","Vietnam","Yemen","Zambia","Zimbabwe","Altro luogo")
  levels(data$luogo_nascita_gen2.factor)=c("Afghanistan","Albania","Algeria","Andorra","Angola","Antigua e Barbuda","Arabia Saudita","Argentina","Armenia","Australia","Austria","Azerbaigian","Bahamas","Bahrein","Bangladesh","Barbados","Belgio","Belize","Benin","Bhutan","Bielorussia","Birmania","Bolivia","Bosnia ed Erzegovina","Botswana","Brasile","Brunei","Bulgaria","Burkina Faso","Burundi","Cambogia","Camerun","Canada","Capo Verde","Ciad","Cile","Cina","Cipro","Colombia","Comore","Corea del Nord","Corea del Sud","Costa dAvorio","Costa Rica","Croazia","Cuba","Danimarca","Dominica","Ecuador","Egitto","El Salvador","Emirati Arabi Uniti","Eritrea","Estonia","Etiopia","Figi","Filippine","Finlandia","Francia","Gabon","Gambia","Georgia","Germania","Ghana","Giamaica","Giappone","Gibuti","Giordania","Grecia","Grenada","Guatemala","Guinea","Guinea-Bissau","Guinea Equatoriale","Guyana","Haiti","Honduras","India","Indonesia","Iran","Iraq","Irlanda","Islanda","Isole Cook","Isole Marshall","Isole Salomone","Italia","Kazakistan","Kenya","Kirghizistan","Kiribati","Kuwait","Laos","Lesotho","Lettonia","Libano","Liberia","Libia","Liechtenstein","Lituania","Lussemburgo","Macedonia del Nord","Madagascar","Malawi","Malaysia","Maldive","Mali","Malta","Marocco","Mauritania","Mauritius","Messico","Micronesia","Moldavia","Monaco","Mongolia","Montenegro","Mozambico","Namibia","Nauru","Nepal","Nicaragua","Niger","Nigeria","Niue","Norvegia","Nuova Zelanda","Oman","Paesi Bassi","Pakistan","Palau","Palestina","Panama","Papua Nuova Guinea","Paraguay","Perù","Polonia","Portogallo","Qatar","Regno Unito","Repubblica Ceca","Repubblica Centrafricana","Repubblica del Congo","Repubblica Democratica del Congo","Repubblica Dominicana","Romania","Ruanda","Russia","Saint Kitts e Nevis","Saint Lucia","Saint Vincent e Grenadine","Samoa","San Marino","São Tomé e Príncipe","Senegal","Serbia","Seychelles","Sierra Leone","Singapore","Siria","Slovacchia","Slovenia","Somalia","Spagna","Sri Lanka","Stati Uniti","Sudafrica","Sudan","Sudan del Sud","Suriname","Svezia","Svizzera","eSwatini","Tagikistan","Taiwan","Tanzania","Thailandia","Timor Est","Togo","Tonga","Trinidad e Tobago","Tunisia","Turchia","Turkmenistan","Tuvalu","Ucraina","Uganda","Ungheria","Uruguay","Uzbekistan","Vanuatu","Venezuela","Vietnam","Yemen","Zambia","Zimbabwe","Altro luogo")
  levels(data$figli.factor)=c("Si","No")
  levels(data$quanti_figli.factor)=c("Uno","Due","Tre","Quattro","Cinque o più")
  levels(data$reddito.factor)=c("Molto difficile","Difficile","Abbastanza difficile","Abbastanza facile","Facile","Molto facile")
  levels(data$servizi_salute_mentale.factor)=c("Sì, nel pubblico","Sì, nel privato","Sì, sia nel pubblico che nel privato","No")
  levels(data$diagnosi_si_no.factor)=c("Sì","No")
  levels(data$aut.factor)=c("Nel pubblico","Nel privato","Sia nel pubblico che nel privato","No")
  levels(data$adhd.factor)=c("Nel pubblico","Nel privato","Sia nel pubblico che nel privato","No")
  levels(data$pers.factor)=c("Nel pubblico","Nel privato","Sia nel pubblico che nel privato","No")
  levels(data$ansia.factor)=c("Nel pubblico","Nel privato","Sia nel pubblico che nel privato","No")
  levels(data$umore.factor)=c("Nel pubblico","Nel privato","Sia nel pubblico che nel privato","No")
  levels(data$comp_al.factor)=c("Nel pubblico","Nel privato","Sia nel pubblico che nel privato","No")
  levels(data$appr.factor)=c("Nel pubblico","Nel privato","Sia nel pubblico che nel privato","No")
  levels(data$psicosi.factor)=c("Nel pubblico","Nel privato","Sia nel pubblico che nel privato","No")
  levels(data$ocd.factor)=c("Nel pubblico","Nel privato","Sia nel pubblico che nel privato","No")
  levels(data$ptsd.factor)=c("Nel pubblico","Nel privato","Sia nel pubblico che nel privato","No")
  levels(data$dipendenza.factor)=c("Nel pubblico","Nel privato","Sia nel pubblico che nel privato","No")
  levels(data$altro.factor)=c("Nel pubblico","Nel privato","Sia nel pubblico che nel privato","No")
  levels(data$livello_aut.factor)=c("Livello 1 (o sindrome di Asperger)","Livello 2","Livello 3","Non specificato nella diagnosi")
  levels(data$invio_aut.factor)=c("Autoinvio","Genitore / caregiver","Amici / parenti / conoscenti","Istituzioni scolastiche","Medico curante","Psicologo","Psichiatra","Altro")
  levels(data$invio_adhd.factor)=c("Autoinvio","Genitore / caregiver","Amici / parenti / conoscenti","Istituzioni scolastiche","Medico curante","Psicologo","Psichiatra","Altro")
  levels(data$invio_person.factor)=c("Autoinvio","Genitore / caregiver","Amici / parenti / conoscenti","Istituzioni scolastiche","Medico curante","Psicologo","Psichiatra","Altro")
  levels(data$invio_ansia.factor)=c("Autoinvio","Genitore / caregiver","Amici / parenti / conoscenti","Istituzioni scolastiche","Medico curante","Psicologo","Psichiatra","Altro")
  levels(data$invio_umore.factor)=c("Autoinvio","Genitore / caregiver","Amici / parenti / conoscenti","Istituzioni scolastiche","Medico curante","Psicologo","Psichiatra","Altro")
  levels(data$invio_comp_al.factor)=c("Autoinvio","Genitore / caregiver","Amici / parenti / conoscenti","Istituzioni scolastiche","Medico curante","Psicologo","Psichiatra","Altro")
  levels(data$invio_appr.factor)=c("Autoinvio","Genitore / caregiver","Amici / parenti / conoscenti","Istituzioni scolastiche","Medico curante","Psicologo","Psichiatra","Altro")
  levels(data$invio_psicosi.factor)=c("Autoinvio","Genitore / caregiver","Amici / parenti / conoscenti","Istituzioni scolastiche","Medico curante","Psicologo","Psichiatra","Altro")
  levels(data$invio_ocd.factor)=c("Autoinvio","Genitore / caregiver","Amici / parenti / conoscenti","Istituzioni scolastiche","Medico curante","Psicologo","Psichiatra","Altro")
  levels(data$invio_ptsd.factor)=c("Autoinvio","Genitore / caregiver","Amici / parenti / conoscenti","Istituzioni scolastiche","Medico curante","Psicologo","Psichiatra","Altro")
  levels(data$invio_dipendenza.factor)=c("Autoinvio","Genitore / caregiver","Amici / parenti / conoscenti","Istituzioni scolastiche","Medico curante","Psicologo","Psichiatra","Altro")
  levels(data$invio_altro.factor)=c("Autoinvio","Genitore / caregiver","Amici / parenti / conoscenti","Istituzioni scolastiche","Medico curante","Psicologo","Psichiatra","Altro")
  levels(data$prima_visita_aut.factor)=c("Meno di 3 mesi","Tra 3 e 6 mesi","Tra 6 mesi e 1 anno","Tra 1 e 2 anni","Tra 2 e 3 anni","Più di 3 anni")
  levels(data$prima_visita_adhd.factor)=c("Meno di 3 mesi","Tra 3 e 6 mesi","Tra 6 mesi e 1 anno","Tra 1 e 2 anni","Tra 2 e 3 anni","Più di 3 anni")
  levels(data$prima_visita_pers.factor)=c("Meno di 3 mesi","Tra 3 e 6 mesi","Tra 6 mesi e 1 anno","Tra 1 e 2 anni","Tra 2 e 3 anni","Più di 3 anni")
  levels(data$prima_visita_ansia.factor)=c("Meno di 3 mesi","Tra 3 e 6 mesi","Tra 6 mesi e 1 anno","Tra 1 e 2 anni","Tra 2 e 3 anni","Più di 3 anni")
  levels(data$prima_visita_umore.factor)=c("Meno di 3 mesi","Tra 3 e 6 mesi","Tra 6 mesi e 1 anno","Tra 1 e 2 anni","Tra 2 e 3 anni","Più di 3 anni")
  levels(data$prima_visita_comp_al.factor)=c("Meno di 3 mesi","Tra 3 e 6 mesi","Tra 6 mesi e 1 anno","Tra 1 e 2 anni","Tra 2 e 3 anni","Più di 3 anni")
  levels(data$prima_visita_appr.factor)=c("Meno di 3 mesi","Tra 3 e 6 mesi","Tra 6 mesi e 1 anno","Tra 1 e 2 anni","Tra 2 e 3 anni","Più di 3 anni")
  levels(data$prima_visita_psicosi.factor)=c("Meno di 3 mesi","Tra 3 e 6 mesi","Tra 6 mesi e 1 anno","Tra 1 e 2 anni","Tra 2 e 3 anni","Più di 3 anni")
  levels(data$prima_visita_ocd.factor)=c("Meno di 3 mesi","Tra 3 e 6 mesi","Tra 6 mesi e 1 anno","Tra 1 e 2 anni","Tra 2 e 3 anni","Più di 3 anni")
  levels(data$prima_visita_ptsd.factor)=c("Meno di 3 mesi","Tra 3 e 6 mesi","Tra 6 mesi e 1 anno","Tra 1 e 2 anni","Tra 2 e 3 anni","Più di 3 anni")
  levels(data$prima_visita_dipendenza.factor)=c("Meno di 3 mesi","Tra 3 e 6 mesi","Tra 6 mesi e 1 anno","Tra 1 e 2 anni","Tra 2 e 3 anni","Più di 3 anni")
  levels(data$prima_visita_altro.factor)=c("Meno di 3 mesi","Tra 3 e 6 mesi","Tra 6 mesi e 1 anno","Tra 1 e 2 anni","Tra 2 e 3 anni","Più di 3 anni")
  levels(data$att_diagnosi_aut.factor)=c("Meno di 3 mesi","Tra 3 e 6 mesi","tra 6 mesi ed 1 anno","Tra 1 e 2 anni","tra 2 e 3 anni","Più di 3 anni")
  levels(data$att_diagnosi_adhd.factor)=c("Meno di 3 mesi","Tra 3 e 6 mesi","tra 6 mesi ed 1 anno","Tra 1 e 2 anni","tra 2 e 3 anni","Più di 3 anni")
  levels(data$att_diagnosi_pers.factor)=c("Meno di 3 mesi","Tra 3 e 6 mesi","tra 6 mesi ed 1 anno","Tra 1 e 2 anni","tra 2 e 3 anni","Più di 3 anni")
  levels(data$att_diagnosi_ansia.factor)=c("Meno di 3 mesi","Tra 3 e 6 mesi","tra 6 mesi ed 1 anno","Tra 1 e 2 anni","tra 2 e 3 anni","Più di 3 anni")
  levels(data$att_diagnosi_umore.factor)=c("Meno di 3 mesi","Tra 3 e 6 mesi","tra 6 mesi ed 1 anno","Tra 1 e 2 anni","tra 2 e 3 anni","Più di 3 anni")
  levels(data$att_diagnosi_comp_al.factor)=c("Meno di 3 mesi","Tra 3 e 6 mesi","tra 6 mesi ed 1 anno","Tra 1 e 2 anni","tra 2 e 3 anni","Più di 3 anni")
  levels(data$att_diagnosi_appr.factor)=c("Meno di 3 mesi","Tra 3 e 6 mesi","tra 6 mesi ed 1 anno","Tra 1 e 2 anni","tra 2 e 3 anni","Più di 3 anni")
  levels(data$att_diagnosi_psicosi.factor)=c("Meno di 3 mesi","Tra 3 e 6 mesi","tra 6 mesi ed 1 anno","Tra 1 e 2 anni","tra 2 e 3 anni","Più di 3 anni")
  levels(data$att_diagnosi_ocd.factor)=c("Meno di 3 mesi","Tra 3 e 6 mesi","tra 6 mesi ed 1 anno","Tra 1 e 2 anni","tra 2 e 3 anni","Più di 3 anni")
  levels(data$att_diagnosi_ptsd.factor)=c("Meno di 3 mesi","Tra 3 e 6 mesi","tra 6 mesi ed 1 anno","Tra 1 e 2 anni","tra 2 e 3 anni","Più di 3 anni")
  levels(data$att_diagnosi_dipendenza.factor)=c("Meno di 3 mesi","Tra 3 e 6 mesi","tra 6 mesi ed 1 anno","Tra 1 e 2 anni","tra 2 e 3 anni","Più di 3 anni")
  levels(data$att_diagnosi_altro.factor)=c("Meno di 3 mesi","Tra 3 e 6 mesi","tra 6 mesi ed 1 anno","Tra 1 e 2 anni","tra 2 e 3 anni","Più di 3 anni")
  levels(data$sbagliate_aut.factor)=c("Si","No")
  levels(data$sbagliate_adhd.factor)=c("Si","No")
  levels(data$sbagliate_pers.factor)=c("Si","No")
  levels(data$sbagliate_ansia.factor)=c("Si","No")
  levels(data$sbagliate_umore.factor)=c("Si","No")
  levels(data$sbagliate_comp_al.factor)=c("Si","No")
  levels(data$sbagliate_appr.factor)=c("Si","No")
  levels(data$sbagliate_psicosi.factor)=c("Si","No")
  levels(data$sbagliate_ocd.factor)=c("Si","No")
  levels(data$sbagliate_ptsd.factor)=c("Si","No")
  levels(data$sbagliate_dipendenza.factor)=c("Si","No")
  levels(data$sbagliate_altro.factor)=c("Si","No")
  levels(data$costo_aut.factor)=c("È stata gratuita","Meno di 150 euro","Tra 150 e 300 euro","Tra 301 e 500 euro","Più di 500 euro")
  levels(data$costo_adhd.factor)=c("È stata gratuita","Meno di 150 euro","Tra 150 e 300 euro","Tra 301 e 500 euro","Più di 500 euro")
  levels(data$costo_pers.factor)=c("È stata gratuita","Meno di 150 euro","Tra 150 e 300 euro","Tra 301 e 500 euro","Più di 500 euro")
  levels(data$costo_ansia.factor)=c("È stata gratuita","Meno di 150 euro","Tra 150 e 300 euro","Tra 301 e 500 euro","Più di 500 euro")
  levels(data$costo_umore.factor)=c("È stata gratuita","Meno di 150 euro","Tra 150 e 300 euro","Tra 301 e 500 euro","Più di 500 euro")
  levels(data$costo_comp_al.factor)=c("È stata gratuita","Meno di 150 euro","Tra 150 e 300 euro","Tra 301 e 500 euro","Più di 500 euro")
  levels(data$costo_appr.factor)=c("È stata gratuita","Meno di 150 euro","Tra 150 e 300 euro","Tra 301 e 500 euro","Più di 500 euro")
  levels(data$costo_psicosi.factor)=c("È stata gratuita","Meno di 150 euro","Tra 150 e 300 euro","Tra 301 e 500 euro","Più di 500 euro")
  levels(data$costo_ocd.factor)=c("È stata gratuita","Meno di 150 euro","Tra 150 e 300 euro","Tra 301 e 500 euro","Più di 500 euro")
  levels(data$costo_ptsd.factor)=c("È stata gratuita","Meno di 150 euro","Tra 150 e 300 euro","Tra 301 e 500 euro","Più di 500 euro")
  levels(data$costo_dipendenza.factor)=c("È stata gratuita","Meno di 150 euro","Tra 150 e 300 euro","Tra 301 e 500 euro","Più di 500 euro")
  levels(data$costo_altro.factor)=c("È stata gratuita","Meno di 150 euro","Tra 150 e 300 euro","Tra 301 e 500 euro","Più di 500 euro")
  levels(data$dove_aut.factor)=c("Online","Nella tua città","Nella tua provincia","Nella tua regione","Fuori dalla tua regione")
  levels(data$dove_adhd.factor)=c("Online","Nella tua città","Nella tua provincia","Nella tua regione","Fuori dalla tua regione")
  levels(data$dove_pers.factor)=c("Online","Nella tua città","Nella tua provincia","Nella tua regione","Fuori dalla tua regione")
  levels(data$dove_ansia.factor)=c("Online","Nella tua città","Nella tua provincia","Nella tua regione","Fuori dalla tua regione")
  levels(data$dove_umore.factor)=c("Online","Nella tua città","Nella tua provincia","Nella tua regione","Fuori dalla tua regione")
  levels(data$dove_comp_al.factor)=c("Online","Nella tua città","Nella tua provincia","Nella tua regione","Fuori dalla tua regione")
  levels(data$dove_appr.factor)=c("Online","Nella tua città","Nella tua provincia","Nella tua regione","Fuori dalla tua regione")
  levels(data$dove_psicosi.factor)=c("Online","Nella tua città","Nella tua provincia","Nella tua regione","Fuori dalla tua regione")
  levels(data$dove_ocd.factor)=c("Online","Nella tua città","Nella tua provincia","Nella tua regione","Fuori dalla tua regione")
  levels(data$dove_ptsd.factor)=c("Online","Nella tua città","Nella tua provincia","Nella tua regione","Fuori dalla tua regione")
  levels(data$dove_dipendenza.factor)=c("Online","Nella tua città","Nella tua provincia","Nella tua regione","Fuori dalla tua regione")
  levels(data$dove_altro.factor)=c("Online","Nella tua città","Nella tua provincia","Nella tua regione","Fuori dalla tua regione")
  levels(data$n_prof_aut.factor)=c("1","2","3","4","5","6","7 o più")
  levels(data$n_prof_adhd.factor)=c("1","2","3","4","5","6","7 o più")
  levels(data$n_prof_pers.factor)=c("1","2","3","4","5","6","7 o più")
  levels(data$n_prof_ansia.factor)=c("1","2","3","4","5","6","7 o più")
  levels(data$n_prof_umore.factor)=c("1","2","3","4","5","6","7 o più")
  levels(data$n_prof_comp_al.factor)=c("1","2","3","4","5","6","7 o più")
  levels(data$n_prof_appr.factor)=c("1","2","3","4","5","6","7 o più")
  levels(data$n_prof_psicosi.factor)=c("1","2","3","4","5","6","7 o più")
  levels(data$n_prof_ocd.factor)=c("1","2","3","4","5","6","7 o più")
  levels(data$n_prof_ptsd.factor)=c("1","2","3","4","5","6","7 o più")
  levels(data$n_prof_dipendenza.factor)=c("1","2","3","4","5","6","7 o più")
  levels(data$n_prof_altro.factor)=c("1","2","3","4","5","6","7 o più")
  levels(data$presa_carico_aut.factor)=c("Ti ha proposto una presa in carico","Ti ha proposto un altro servizio o professionista per la presa in carico","Non ti ha dato indicazioni per la presa in carico","Hai cercato tu un altro percorso rispetto a quanto proposto")
  levels(data$percorsi_aut___1.factor)=c("Unchecked","Checked")
  levels(data$percorsi_aut___2.factor)=c("Unchecked","Checked")
  levels(data$percorsi_aut___3.factor)=c("Unchecked","Checked")
  levels(data$percorsi_aut___4.factor)=c("Unchecked","Checked")
  levels(data$percorsi_aut___5.factor)=c("Unchecked","Checked")
  levels(data$percorsi_aut___6.factor)=c("Unchecked","Checked")
  levels(data$percorsi_aut___7.factor)=c("Unchecked","Checked")
  levels(data$percorsi_aut___8.factor)=c("Unchecked","Checked")
  levels(data$durata_percorsi.factor)=c("Mai iniziato / fatto nulla","Meno di 3 mesi","Tra 3 e 6 mesi","Tra 6 mesi e 1 anno","Tra 1 e 2 anni","Tra 2 e 3 anni","Più di 3 anni")
  levels(data$sei_in_carico.factor)=c("Sì","No")
  levels(data$interruzione_percorso.factor)=c("Mia","Del curante","Ci sono stati altri fattori")
  levels(data$questionario_lumen_complete.factor)=c("Incomplete","Unverified","Complete")

  # Verifica consenso
  data <- data[data$consenso == 1 & !is.na(data$consenso), ]
  
  
  return(data)
}
