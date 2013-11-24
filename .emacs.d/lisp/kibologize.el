;;; kibologize.el --- generate ravings about kibology, in the style of kibo

;; Copyright (C) 1990 James "Kibo" Parry

;; Authors: James Parry <kibo@world.std.com>
;;          Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: friedman@prep.ai.mit.edu
;; Created: 1990-01-08

;; $Id: kibologize.el,v 1.7 1997/03/14 10:29:59 friedman Exp $

;;; Commentary:

;; Originally written by James Parry based on "flame.el" by Ian G. Batten &
;; others.  Completely rewritten 1997-03-14 by Noah Friedman.

;; Load this file, then do M-x kibologize.
;; If you are bored, try M-x psychoanalyze-kibologist.

;;; Code:

(random t)

;; top level -- framework of sentences to be made
(defconst kibo-sentence
  '(("!adj" "*things" "are" "*adj.")
    ("!dir" "with" "*thing-things!")
    ("!fund-force" "is like" "*a-symbol.")
    ("!judgment")
    ("!person-be" "*adj.")
    ("!person-be" "*what-it-is," "because" "*reason.")
    ("!person-y" "could" "*t-verb" "*person.")
    ("!query")
    ("!quote")
    ("!ratio" "of the" "*adj" "*thing-s" "*adj.")
    ("!sense" "Kibology, and" "*good-verb!")
    ("!sentence" "*agree.")
    ("!thing" "is not" "*what-it-is," "I" "*sense!")
    ("!thing" "is" "*what-it-is.")
    ("!thing" "makes sense.")
    ("!thing" "seems" "*adj" "to me!")
    ("!thing-s" "not" "*thing-things.")
    ("!thing-s" "the" "*something-of" "of" "*thing-things.")
    ("!thing-things" "can teach us how to" "*i-verb.")
    ("!you-we-they" "*ability" "*t-i-verb.")
    ("Can you" "*sense" "*fund-force?")
    ("Do you want to" "*t-verb" "*thing-things?" "You're allowed!")
    ("Enjoy" "*thing-things!")
    ("I think" "*person-be" "*adj" "*ratio" "of the time.")
    ("If you" "*ability" "*t-verb-sense" "the" "*something-of"
     "of" "*thing," "*result.")
    ("If" "*thing-things" "could" "*i-verb," "would the"
     "*Universe" "be" "*adj?")
    ("Let's" "*t-i-verb!")
    ("Reach out with your" "*soul" "and" "*sense" "the" "*fund-force.")
    ("There's one reason for" "*thing," "and it's" "*fund-force.")
    ;; FIXME
    ;("When" "*person" "says" "\"*sentence\"," it means "\"*sentence\".")
    ("Why not" "*good-verb?")
    ("You can" "*sense" "when you try to" "*sense.")))

;; "Variety is the ____ of life."
(defconst kibo-something-of
  '(("Oracle") ("answer to the question") ("bane") ("beginning") ("best")
    ("birth") ("book") ("business") ("code of honor") ("color") ("core")
    ("cost") ("creation") ("dance") ("death") ("definition") ("destiny")
    ("end") ("enemy") ("energy") ("evil twin") ("explosion") ("foolishness")
    ("form") ("friend") ("full range") ("genesis") ("goal") ("god")
    ("greatness") ("guru") ("hiding place") ("incredible truth") ("joy")
    ("kind") ("luxury") ("meaning") ("meaning") ("mission") ("opiate")
    ("opposite") ("point") ("power") ("price") ("priviledge") ("purpose")
    ("refuge") ("renaissance") ("reward") ("reward") ("sameness") ("shape")
    ("silliness") ("slave") ("spice") ("strategy") ("stupidity")
    ("symbol") ("use") ("whole mess")))

;; "The ____ of life are silly."
(defconst kibo-somethings-of
  '(("*adj" "parts") ("*something-of" "and" "*something-of")
    ("answers to the questions") ("capacities") ("chapters of the book")
    ("creators") ("definitions") ("deities") ("destinies") ("differences")
    ("enemies") ("energies") ("feelings") ("flavors") ("forms") ("freedoms")
    ("friends") ("highlights and lowlights") ("isms") ("kinds") ("last gasps")
    ("miracles") ("options") ("powers") ("rebirths") ("rewards")
    ("shapelessnesses") ("sounds") ("uses") ("uses")))

;; "Everything is made of ____."  "____ is power!"
(defconst kibo-fund-force
  '(("*a-symbol") ("*poss" "*thing") ("*poss" "ability to" "*sense")
    ("*poss" "destiny") ("*poss" "fate") ("*poss" "life") ("atoms")
    ("brainwaves") ("deadly radiation that comes out of TV sets")
    ("deities") ("energy") ("everything") ("goodness") ("gravity")
    ("life") ("light") ("light") ("memory") ("mind control rays")
    ("mindwaves") ("new ideas") ("odyle") ("odyleforce") ("odylematter")
    ("odylethought") ("our souls") ("people like you and me")
    ("reality") ("sound") ("space") ("television") ("the Earth") ("the Void")
    ("the ecology") ("the excitement") ("the sum of all forms of odyle")
    ("the total of all forms of vril") ("thought") ("ubiquitons")
    ("vril and odyle") ("vril") ("vrilforce") ("vrilmatter") ("vrilthought")
    ("*Universe") ("the shape of" "*fund-force")))

;; some mystical body part, maybe non-physical
(defconst kibo-soul
  '(("aura") ("body") ("brain") ("brain") ("creative \"insanity\"")
    ("doubts") ("essence") ("eyes") ("faith") ("feelings") ("fingers")
    ("inner mind") ("intellect") ("intuition") ("life-force") ("meat")
    ("mind") ("outer mind") ("powers, if any,") ("senses") ("sixth sense")
    ("soul") ("thought-loop") ("understanding of peoples' minds")
    ("vibrations")))

;; "____ the excitement."  "I can ____!"
(defconst kibo-sense
  '(("KNOW") ("be consumed by") ("believe") ("catch") ("consume") ("contact")
    ("doubt") ("engulf") ("experience") ("fall in love with") ("feel")
    ("feel") ("get in touch with") ("hear") ("ignore") ("join") ("know")
    ("listen to") ("open a mental door") ("reach") ("remember")
    ("see with your inner eye") ("see") ("sense") ("smell") ("think about")
    ("touch") ("trust") ("understand") ("walk into") ("watch") ("witness")))

;; "Universe" is always followed by "U for ____"....
(defconst kibo-Universe
  '(("Universe, U for" "*U-word,")))

;; "Universe" is always followed by "U for ____"....
(defconst kibo-U-word
  '(("U and Me") ("Ubiquitous") ("Ugly") ("Ullage") ("Ulterior") ("Ultimate")
    ("Umanity") ("Umbilical") ("Un") ("Unaccompanied") ("Unbalanced")
    ("Unbiased") ("Unbounded") ("Uncanny") ("Uncensored") ("Uncertain")
    ("Uncle") ("Uncreated") ("Underhanded") ("Underwhelming") ("Undeserved")
    ("Undying") ("Unessential") ("Uneven") ("Unfair") ("Uniform") ("Unique")
    ("Unisex") ("Universe") ("Universe") ("Unmentionable") ("Unnatural")
    ("Up and Up and Up") ("Upsetting") ("Uranium") ("Urban") ("Urgent")
    ("Useful") ("Useless") ("Utopia") ("Utter") ("You") ("\"Ugh!\"")))

;; Any singular noun.  "____ is my middle name." "A red ____ is in my sock."
(defconst kibo-thing
  '(("*poss" "body") ("*thing" "in a box") ("Chinese food") ("Kibo") ("Kibo")
    ("Kibology") ("Kibology") ("Kibology") ("Nature") ("a game") ("a guess")
    ("allowedness") ("allowedness") ("an egg") ("another dimension")
    ("being allowed") ("birth") ("clothing") ("communication") ("death")
    ("everything") ("existence") ("fiction") ("fire") ("food") ("humanity")
    ("language") ("life") ("love") ("nothing") ("one solitary atom")
    ("plastic") ("radiation") ("reality") ("reality") ("relaxation")
    ("religion") ("sex") ("spark of life") ("television") ("the Bible")
    ("the Kibo Bible") ("the Void") ("the color blue") ("the future")
    ("the reason why") ("the sacred symbol \"298R\"")
    ("the script we're following in THIS life") ("the truth")
    ("the word \"Noom\"") ("vril") ("what they call insanity") ("zumfing")
    ("zumfing") ("conflict between" "*thing-things" "and" "*thing")
    ("something like" "*a-symbol") ("*Universe")
    ("the fact that" "*thing-s" "*adj") ("the truth about" "*fund-force")))

;; Some intransitive infinitive verb that's really neat-o
(defconst kibo-good-verb
  '(("allow people to be people") ("be allowed") ("be allowed") ("be someone")
    ("conserve energy") ("eat Chinese food") ("eat well") ("grow")
    ("help Harry") ("live to the fullest") ("live") ("love")
    ("move into a new dimension") ("own a home") ("profit") ("stay sane")
    ("understand everything") ("zumf") ("learn about" "*thing-things")))

;; "X is ____."
(defconst kibo-what-it-is
  '(("*adj") ("*adj") ("ALLOWED") ("Kibology") ("a bozo") ("a bozo")
    ("a sham") ("allowed") ("allowed") ("complex") ("everything")
    ("everywhere") ("false") ("here") ("incomprehensible") ("life itself")
    ("like a spider's web") ("nothing") ("perverse") ("real") ("revered")
    ("serious") ("silly") ("simple") ("something") ("surreal") ("the Truth")
    ("unreal") ("what it appears") ("worth" "*poss" "time")))

;; Name of a person... "____ is a bozo."
(defconst kibo-person
  '(("Albert Einstein") ("Aristotle") ("Confucius") ("Einstein") ("Freud")
    ("God") ("Harry, who was helped by Kibology,") ("Jesus") ("Kibo")
    ("Kibo") ("Kibo") ("Kibo") ("Kibo") ("Kibo") ("Kibo") ("Kibo")
    ("Kibo's best friend") ("Kibo, the great teacher") ("Xibo") ("Xibo")
    ("Xibo") ("a Kibologist") ("a false god") ("anyone who is allowed")
    ("some bozo") ("some weenie") ("someone") ("the Pope") ("the President")
    ("the evil Xibo") ("your pal")))

;; Like kibo-person but includes "you", "I"
(defconst kibo-person-y
  '(("*person") ("*person") ("*person") ("*person")
    ("I") ("I") ("I") ("everyone") ("you") ("you") ("you")))

;; Person plus "to be"... "____ a bozo."
(defconst kibo-person-be
  '(("*person" "is not") ("*person" "is") ("*person" "was")
    ("*person" "will never be") ("*you-we-they" "all are")
    ("*you-we-they" "are") ("Einstein would be, if he were alive,")
    ("Harry, before he was helped by Kibology, was")
    ("I am") ("I can be, when I want to,") ("I have been") ("I shall be")
    ("I used to be") ("I was") ("I'm NOT") ("Kibo says you're allowed to be")
    ("Xibo says you're not allowed to be")
    ("everyone was, is, and always will be") ("humanity is") ("nobody is")
    ("people are") ("they're") ("we should be") ("you are allowed to be")
    ("you are not") ("you are") ("you aren't") ("you once were")
    ("you will always be") ("you're")))

;; "You", "We", or "They", so that "are" can follow
(defconst kibo-you-we-they
  '(("*adj" "people") ("people") ("they") ("they") ("we") ("we")
    ("you") ("you") ("you") ("you")))

;; A reason... "Kibo likes plastic because ____."
(defconst kibo-reason
  '(("*fund-force" "is like" "*a-symbol") ("everything's allowed")
    ("it is allowed") ("it makes sense") ("it's more fun that way")
    ("it's less painful than death") ("it's natural")
    ("it's the natural way of things") ("life cheats") ("life is fair")
    ("life is serious") ("reality is somewhat flexible")
    ("sometimes life's dice come up with your number")
    ("that seems to be the best way") ("that's the way it HAS to be")
    ("there really is no other way to do it") ("we can change the future")
    ("that's how the" "*Universe" "works")))

;; Plural or collective noun... "____ are blue."  "Kibo eats red _____."
(defconst kibo-things
  '(("*poss" "abilities") ("*poss" "thoughts") ("*poss" "toys")
    ("Chinese restaurants") ("all nations everywhere")
    ("all the" "*things" "in the" "*Universe") ("atoms") ("books")
    ("bozos") ("bunches of" "*things") ("colors") ("computers")
    ("destinies") ("different meanings") ("emotions") ("feelings")
    ("flowers") ("holes in" "*poss" "knowledge") ("languages")
    ("lies") ("many worlds") ("many" "*things") ("multiple realities")
    ("nodes") ("past, present, and future") ("people") ("pets")
    ("sane ideas for today's lifestyle") ("sharp knives")
    ("things that matter to us all") ("unspoiled places") ("victories")
    ("vril and odyle") ("what I say and what you hear") ("words")
    ("works of art") ("writings of Kibo")))

;; Singular OR plural noun...
(defconst kibo-thing-things
  '(("*thing") ("*thing") ("*thing") ("*thing") ("*thing")
    ("*thing-things" "and" "*thing-things")
    ("*things") ("*things") ("*things")))

;; Subject plus "to be"... "_______ my middle name."
(defconst kibo-thing-s
  '(("*thing" "and" "*thing" "are")
    ("*thing" "is") ("*thing" "is") ("*thing" "is")
    ("*things" "are") ("*things" "are")))

;; Ordinary intrans. verbal infinitive... "I like to ____."
(defconst kibo-i-verb
  '(("*good-verb") ("*sense") ("be me") ("be") ("be") ("breathe") ("create")
    ("die") ("disappear") ("eat") ("exist") ("explode in fireworks")
    ("go") ("grow") ("have an experience") ("keep going to" "*poss" "limit")
    ("live") ("radiate") ("shine") ("stay in our world") ("stay")
    ("think") ("try to" "*t-i-verb") ("warm up to" "*t-i-verb")
    ("work") ("zumf")))

;; Ordinary transitive verbal infinitive... "I like to ____ rats."
(defconst kibo-t-verb
  '(("*sense") ("accept") ("allow all") ("avoid") ("be inspired by") ("be")
    ("challenge") ("change") ("create") ("destroy") ("fiddle with")
    ("fix up") ("kill") ("learn from") ("love") ("love") ("preserve")
    ("resemble") ("see") ("seem related to") ("share")
    ("support the ideas of") ("talk to") ("tamper with") ("teach Kibology to")
    ("think about") ("wrap up")))

;; Possessive adjective... "This is ____ life!"
(defconst kibo-poss
  '(("Einstein's") ("George's") ("God's") ("Harry's") ("Kibo's") ("Kibo's")
    ("Kibo's") ("Kibo's") ("Kibo's") ("Kibo's") ("Xibo's")
    ("a human being's") ("a") ("anybody's") ("everybody's") ("everyone's")
    ("my") ("my") ("my") ("my") ("my") ("nobody's") ("our") ("our") ("our")
    ("our") ("some") ("somebody's") ("your life's") ("your") ("your")
    ("your") ("your") ("your") ("your") ("your")))

;; An adjective... "The ____ fox jumps..."
(defconst kibo-adj
  '(("*adj" "*adj") ("*ratio" "*adj") ("Kibological") ("Kibotic")
    ("Xibotic") ("anti-" "*adj") ("artistic") ("baby") ("bad")
    ("beautiful") ("bogus") ("boring") ("bright") ("confusing")
    ("contaminated") ("dark") ("darned") ("eternal") ("evil") ("fair")
    ("false") ("futuristic") ("galactic") ("good") ("good") ("great")
    ("great") ("halcyon") ("half-baked") ("happy") ("hefty") ("incredible")
    ("infinite") ("intense") ("interestingly" "*adj") ("mock") ("modern")
    ("new") ("newly created") ("nice") ("nonexistant") ("obvious") ("old")
    ("pure") ("respected") ("right") ("sacred") ("sad") ("sane")
    ("sensitive") ("serious") ("silly") ("silly") ("spurious") ("standard")
    ("stuck-in-a-rut") ("thing-loving") ("true") ("truthful") ("ubiquitous")
    ("un-" "*adj") ("unfair") ("untainted") ("very very" "*adj")
    ("very" "*adj") ("warped") ("well-worn") ("wimpy") ("wrong") ("young")))

;; A complete sentence that shows up rarely
(defconst kibo-quote
  '(("*fund-force" "abounds!") ("*sentence" "WHY?")
    ("All" "*things" "are arbitrary!")
    ("Always, always, please remember..." "*sentence")
    ("Be allowed!") ("Be" "*adj!") ("Do everything!")
    ("George was helped by Kibology.  Allow yourself to be helped too!")
    ("Hmm...") ("Kibology helped Harry!") ("Kibology is deadly serious.")
    ("Kibology is no joke.") ("La!") ("La, la, la...") ("La...")
    ("Whee!") ("Xibology is no fun.") ("Yes!") ("Yes..." "*sentence")
    ("You are ALLOWED.")
    ("You're allowed to not be a Kibologist if you want.")
    ("You're allowed!") ("You're allowed...") ("Zumf and be perfect!")))

;; Direction for "____ with nuclear weapons!"
(defconst kibo-dir
  '(("down") ("down") ("good luck") ("in") ("life is a game") ("live")
    ("make life fun") ("out") ("stick") ("try it") ("up") ("up")))

;; Some transitive verb or sense
(defconst kibo-t-verb-sense
  '(("*t-verb") ("*sense")))

;; Can, can't, etc... "You ____ burn water."
;; This should go with "You" or "We" or "They" since it may use "are"
(defconst kibo-ability
  '(("always") ("are afraid to") ("are allowed not to") ("are allowed to")
    ("are not afraid to") ("can not") ("can teach others to") ("can too")
    ("can try to") ("can") ("can") ("can") ("can't") ("cannot")
    ("have a reason to") ("have to") ("might") ("need no excuse to")
    ("never") ("often can") ("should not") ("should") ("try to") ("want to")
    ("wish everyone could")))

;; "If you can read this, __________."
(defconst kibo-result
  '(("*person" "wins") ("DO IT") ("allow it to happen") ("allow yourself")
    ("don't put it off") ("don't tell anyone") ("enjoy" "*thing-things")
    ("go ahead") ("great") ("it's a sign") ("it's because" "*reason")
    ("it's nice to get it over with") ("keep trying") ("oh no")
    ("please tell everyone") ("relax") ("say what you feel")
    ("sorry about that") ("ssh! Be quiet") ("that's allowed")
    ("whee") ("you don't need help") ("you get the idea") ("you have won")
    ("you lose") ("you win") ("you" "*ability" "*good-verb")
    ("you're allowed") ("you've got it made")))

;; "I'm silly ____ of the time." "____ of the cat is black."
(defconst kibo-ratio
  '(("ABSOLUTELY" "*ratio") ("all") ("all") ("all") ("almost all")
    ("every bit") ("exactly half") ("half of all") ("half") ("little")
    ("most") ("much") ("none") ("none") ("none") ("one hundred percent")
    ("only a small fraction") ("part") ("some") ("the best part")
    ("three-quarters") ("very little")))

;; "Your heart is ____." optional "a/an" plus something symbolic.
(defconst kibo-a-symbol
  '(("*Universe") ("Einstein's brain")
    ("a Chinese Restaurant with bilingual menus")
    ("a bathtub full of telephones") ("a beacon")
    ("a big box full of" "*things")
    ("a big city with no people but many" "*things")
    ("a bowl of lime Jello") ("a bunch of bozos who know that they are bozos")
    ("a dark place where nobody has sung")
    ("a dry, dead cactus") ("a forty-five degree angle to reality")
    ("a fresh warm bag of newly-roasted peanuts at the circus")
    ("a galaxy") ("a heavy brick") ("a hot refrigerator") ("a lightning bolt")
    ("a locked restroom in a Chinese restaurant") ("a new life-form")
    ("a private tomb with a view") ("a short skyscraper")
    ("a sock which is comfortable even though it has a BIG hole in the toe")
    ("a spiderweb")
    ("a stack of blocks that people kept adding to until"
     "*person-y" "knocked it down")
    ("a suit of armor") ("a summit on a high mountain") ("a vortex")
    ("a wild animal") ("an apple with a worm in it") ("an apple")
    ("an ocean") ("last week's news") ("sex") ("skywriting")
    ("the color cobalt blue") ("the color sky blue")
    ("the color ultramarine blue") ("the dying words of Einstein")
    ("the failure of Gravity itself to hold things down")
    ("the inside of a microwave oven") ("the letter K")
    ("the songs that food sings when it cooks")
    ("the speed of Light") ("the symbol \"298R\"")))

;; Any complete questioning sentence.
(defconst kibo-query
  '(("Can" "*person-y" "*t-i-verb?")
    ("Does" "*thing" "*t-i-verb?" "*agree")
    ("How do" "*things" "*t-i-verb?")
    ("I'd like to know the" "*something-of" "of"
     "*fund-force," "because" "*reason.")
    ("What are the" "*somethings-of" "of" "*fund-force?")
    ("What's the" "*something-of" "of" "*fund-force?")
    ("Who is" "*person?")
    ("Why does" "*thing" "*t-i-verb?")))

;; A simple cry of "Yes!" "No!" etc. to be added after some sentences
(defconst kibo-agree
  '(("*ratio" "true!") ("Believe it or not!") ("Could be.") ("FALSE!")
    ("Maybe...") ("NO!") ("No, but" "*sentence") ("No.") ("Not true.")
    ("TRUE!") ("True or False?  You be the judge!") ("YES!!!") ("YES!!!")
    ("YES!") ("Yes!") ("Yes, and also," "*sentence") ("Yes...")))

;; A statement of opinion about something.
(defconst kibo-judgment
  '(("*judgment" "Believe it or not!")
    ("*thing" "abounds!") ("*thing" "is evil and should be destroyed!")
    ("*things" "abound!") ("*things" "are evil and should be destroyed!")))

;; Any predicate.  "I like to ____".  Biased towards intransitives.
(defconst kibo-t-i-verb
  '(("*i-verb") ("*i-verb") ("*i-verb") ("*i-verb") ("*i-verb")
    ("*t-verb" "*adj" "*thing-things") ("*t-verb" "*thing-things")
    ("*t-verb" "*thing-things")))


;;;###autoload
(defun kibologize ()
  (interactive)
  (let ((s (kibo-iterate-list
            (kibo-random-member kibo-sentence) "kibo-")))
    (and (interactive-p)
         (let ((temp-buffer-show-function temp-buffer-show-function)
               (temp-buffer-show-hook
                (function (lambda ()
                            (fill-paragraph nil)))))

           ;; Play nice with JBW's winning temp-buffer window height
           ;; minimization hacks
           (and (eq temp-buffer-show-function 'show-temp-buffer)
                (setq temp-buffer-show-function
                      (function (lambda (buf)
                                  (save-excursion
                                    (set-buffer buf)
                                    (fill-paragraph nil))
                                  (show-temp-buffer buf)))))
           (with-output-to-temp-buffer "*Kibo Says*"
             (princ s))))
    s))

;;;###autoload
(defun psychoanalyze-kibologist ()
  "Kibo goes to the analyst."
  (interactive)
  (doctor)
  (message "")
  (switch-to-buffer "*doctor*")
  (sit-for 0)
  (while (not (input-pending-p))
    (insert (kibologize))
    (insert "\n")
    (sit-for 0)
    (doctor-ret-or-read 1)))

;; Return a random member of list A.
(defun kibo-random-member (a)
  (and a
       (listp a)
       (nth (random (length a)) a)))

;; Process entries from list LISTNAME and from RESTOFLIST, handling periods
;; and commas at the end of LISTNAME as needed.
(defun kibo-getlist (listname restoflist prefix)
  (let* ((lastchar (aref listname (1- (length listname))))
         (punct-char-p (memq lastchar '(?. ?, ?? ?!)))
         (period-p (memq lastchar '(?. ?? ?!)))
         (suffix (if punct-char-p
                     (substring listname 0 -1)
                   listname)))
    (concat (kibo-iterate-list
             (kibo-random-member
              (symbol-value (intern (concat prefix suffix)))) prefix)
            (cond (period-p
                   (concat (char-to-string lastchar) " "))
                  (punct-char-p
                   (char-to-string lastchar))
                  (t ""))
            (if restoflist " " "")
            (kibo-iterate-list restoflist prefix))))

;; Iterate over list A, replacing all strings beginning with a '*' or '!'
;; with a random selection from the appropriate list.
(defun kibo-iterate-list (a prefix)
  (cond ((null a) a)
        ((= (aref (car a) 0) ?*)
         (kibo-getlist (substring (car a) 1) (cdr a) prefix))
        ((= (aref (car a) 0) ?!)
         (kibo-capitalize
          (kibo-getlist (substring (car a) 1) (cdr a) prefix)))
        (t
         (concat (car a)
                 (if (cdr a) " " "")
                 (kibo-iterate-list (cdr a) prefix)))))

(defun kibo-capitalize (a)
  (let ((new (copy-sequence a)))
    (aset new 0 (upcase (aref new 0)))
    new))

(provide 'kibologize)

;; kibologize.el ends here
