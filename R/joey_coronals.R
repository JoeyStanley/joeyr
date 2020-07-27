#' Joey's pre- and post-coronal formants
#' 
#' A dataset containing 14,424 formant measurements from 1,049 vowel 
#' tokens. These came from generated nonce words. Formants are extracted at 
#' 21 points along the duration of the vowel. This dataset is relatively clean
#' and can be used for vowel formant example data.
#' 
#' I generated 333 nonce words of the form (C)CVC(C) with the following sounds:
#' \itemize{
#'    \item Onsets: /t/, /d/, /s/, /z/, n/, /h/, / /, /st/, /sn/
#'    \item Vowels: /i/, /ɪ/, /eɪ/, /ɛ/, /æ/, /a/, /ɔ/, /oʊ/, /ʊ/, /u/, /aɪ/, /aʊ/, /ɔɪ/
#'    \item Codas: /d/, /z/, /dz/.
#' }
#' Generally, only coronals were used. Clusters were okay. Codas were only 
#' obstruents. All American English vowels were used. Not the most scientific
#' dataset, but sufficient for my purposes.
#' 
#' Every combination of these levels was generated and repeated three times. The
#' resulting list was sorted randomly. I read them in a quiet environment, manually aligned
#' them, extracted formants using a Praat script (4 formants at 4500 Hz), and 
#' filtered out the bad measurements. 
#' 
#' The result is a pretty clean dataset showing my vowel formant trajectories, 
#' in the environment of a coronal consonant.
#' 
#' The intended purpose of this data is so that I can quickly have a nice sample
#' at my disposal when illustrating R functions. However, you may use this 
#' dataset however you please.
#' 
#' Metadata about me:
#' White male, born in 1989 in suburban St. Louis where I lived  until I was 
#' 18. Parents are from upstate New York and Minnesota. Lived in Utah, Brazil, 
#' and Georgia as an adult. Data was recorded July 2020 (age 31).
#' 
#' @format A data frame with 14,424 rows and 13 variables:
#' \describe{
#'   \item{vowel_id}{a unique identifier for each vowel token}
#'   \item{start}{the start time for that vowel}
#'   \item{end}{the end time for that vowel}
#'   \item{t}{the time where formants were extracted}
#'   \item{percent}{how far into the vowel's duration (in terms of percent of the duration) the formants were extracted. 0 = onset, 50 = midpoint, 100 = offset}
#'   \item{F1}{the F1 measurement}
#'   \item{F2}{the F2 measurement}
#'   \item{F3}{the F3 measurement}
#'   \item{F4}{the F4 measurement}
#'   \item{word}{the generated nonce word I read}
#'   \item{pre}{the consonant(s) before the vowel (if any)}
#'   \item{vowel}{the vowel class, in Wells' Lexical Sets}
#'   \item{fol}{the consonant(s) after the vowel}
#' }
#' 
"joey_coronals"