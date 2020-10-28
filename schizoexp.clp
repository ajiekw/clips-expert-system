(clear)

(deffunction ask-question (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)

(deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
       then yes 
       else no))

;;;***************
;;;* SYMPTOMS RULES *
;;;***************

(defrule waham-kendali ""
   (not (waham-kendali ?))
   =>
   (assert (waham-kendali-positif (yes-or-no-p "Apakah pasien mengalami waham kendali (yes/no)? "))))
   
(defrule durasi ""
   (waham-kendali-positif yes)
   (not (positif ?))
   =>
   (assert (durasi-waham-kendali (yes-or-no-p "Durasi gejala lebih dari 3 bulan (yes/no)? "))))
   
(defrule waham-persistent""
   (waham-kendali-positif no)
   (not (positif ?))
   =>
   (assert (waham-persistent-positif (yes-or-no-p "Pasien mengalami waham persisten (yes/no)? "))))

(defrule waham-persistent-positif ""
   (waham-persistent-positif yes)
   (not (positif ?))
   =>
   (assert (durasi-waham-persistent (yes-or-no-p "Durasi gejala lebih dari 3 bulan (yes/no)? "))))

(defrule halusinasi ""
	(or (durasi-waham-kendali yes)
	(durasi-waham-persistent yes))
   (not (positif ?))
   =>
   (assert (gejala-halusinasi (yes-or-no-p "pasien mengalami halusinasi (yes/no)? "))))

(defrule halusinasi-positif ""
   (gejala-halusinasi yes)
   (not (positif ?))   
   =>
   (assert (durasi-halusinasi (yes-or-no-p "Durasi gejala lebih dari 3 bulan (yes/no)? "))))
 
(defrule durasi-gejala-negatif ""
   (split-symptoms gn)
   (not (positif ?))   
   =>
   (assert (durasi-negatif (yes-or-no-p "Durasi gejala lebih dari 3 bulan (yes/no)? "))))

(defrule gangguan-primorbod ""
   (or (and (durasi-waham-persistent yes)
   (durasi-halusinasi yes)))
   (not (positif ?))   
   =>
   (assert (gangguan-primorbid-positif (yes-or-no-p "Pasien mengalami gangguan primorbid? (yes/no)? "))))

(defrule gangguan-primorbod-positif ""
   (gangguan-primorbid-positif yes)
   (not (positif ?))   
   =>
   (assert (durasi-primorbid (yes-or-no-p "Durasi lebih dari 3 bulan? (yes/no)? "))))
   
(defrule split-symptom""
   (or (and (durasi-waham-kendali yes)
   (durasi-halusinasi yes)))
   (not (positif ?))   
   =>
   (assert (split-symptoms (ask-question "Dilanjutkan oleh gejala negatif atau stupor? (gn/st)? " gn st))))
   
(defrule stupor-positif ""
   (split-symptoms st)
   (not (positif ?))   
   =>
   (assert (durasi-stupor (yes-or-no-p "Durasi lebih dari 3 bulan? (yes/no)? "))))
;;;****************
;;;* SCHIZO RULES *
;;;****************

(defrule pasien-normal ""
   (and (waham-kendali-positif no)
   	(waham-persistent-positif no))
   (not (positif ?))
   =>
   (assert (positif "Tidak ada gangguan")))

(defrule skizofrenia-paranoid ""
   (durasi-negatif yes)
   (not (positif ?))
   =>
   (assert (positif "Pasien menderita skizofrenia paranoid")))
   
(defrule skizofrenia-hebefrenik ""
   (durasi-primorbid yes)
   (not (positif ?))
   =>
   (assert (positif "Pasien menderita skizofrenia hebefrenik")))
   
(defrule skizofrenia-katatonik ""
   (durasi-stupor yes)
   (not (positif ?))
   =>
   (assert (positif "Pasien menderita skizofrenia katatonik")))

(defrule gangguan-lainnya ""
  (declare (salience -10))
  (not (positif ?))
  =>
  (assert (positif "Pasien kemungkinan mengalami gangguan lainnya.")))

;;;********************************
;;;* STARTUP AND CONCLUSION RULES *
;;;********************************

(defrule system-banner ""
  (declare (salience 10))
  =>
  (printout t crlf crlf)
  (printout t "Schizophrenia Expert System")
  (printout t crlf crlf))

(defrule print-repair ""
  (declare (salience 10))
  (positif ?item)
  =>
  (printout t crlf crlf)
  (printout t "Kemungkinan Diagnosis:")
  (printout t crlf crlf)
  (format t " %s%n%n%n" ?item))