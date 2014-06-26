-- Nikolai Kallhovde	nkallhov@ucsc.edu	03/21/14
import System.Random
import System.IO
import Data.List
import Data.Char

main = do
	handle <- openFile "en.in" ReadMode
	rawLex <- hGetContents handle
	gen <- getStdGen
	(putStrLn . formatSentence . unwords . genTP gen . buildLexicon emptyLex . lines) rawLex
	where formatSentence :: String -> String
	      formatSentence (w:ws) = toUpper w:ws ++ "." 
	      emptyLex = Lexicon [] [] [] [] []

-- Takes an empty lexicon and a array of lines from the imput file and recursively builds the
-- lexicon from the file using createLexicalEntry.
buildLexicon :: Lexicon -> [String] -> Lexicon
buildLexicon l [] = error "Nothing in lexicon to process."
buildLexicon l [x] = createLexicalEntry l x
buildLexicon l (x:xs) = createLexicalEntry (buildLexicon l xs) x

-- Uses the LEntry class function setEntry to format the string into the correct  LEntry form
-- and then the other class function insertEntry to put it into the given lexicon.
createLexicalEntry :: Lexicon -> String -> Lexicon
createLexicalEntry l s
	| w == "N" = insertEntry l (setEntry ws :: LNoun)
	| w == "V" = insertEntry l (setEntry ws :: LVerb)
	| w == "D" = insertEntry l (setEntry ws :: LDeter)
	| w == "M" = insertEntry l (setEntry ws :: LModal)
	| w == "A" = insertEntry l (setEntry ws :: LAdj)
	| otherwise = error "Invalid word type in lexicon file"
	where (w:ws) = words s

-- This is the top-level sentence generation function.  It first picks some random elements -- a
-- noun phrase, a verb phrase, another noun phrase (object position), an adjective, and an extra
-- verb. The noun phrase and verb phrase are mandatory, but the third element is based on the type
-- of verb and which arguments it takes.  Here are the possibilities:
-- 	'N': Noun phrase.  Standard transitive verb. e.g. shoot, compliment, kiss
-- 	'A': Adjective.  Usually verbs related to being or becoming. e.g. become, turn, appear
-- 	'L': Locative goal ("to" + NP).  Often positional movement related. e.g. go, travel
-- 	'C': Embedded clause ("that" + recursive TP). Thought-related. e.g. know, think, hope
--	'0': No argument. Intransitive, involving action of just one agent. e.g. die, laugh
genTP :: (RandomGen r) => r -> Lexicon -> [String]
genTP r l = let (np, isP, r')   = genNP r l
		(vp, vArgs, r'') = genVP r' isP l
		(np2, _, r3)  = genNP r'' l
		(a, r4) = (randWord r3 . adjs) l
      	    in np ++ vp ++ case vArgs of
	    			'N' -> np2
				'A' -> [adj a]
				'L' -> "to":np2
	    			'C' -> "that":genTP r4 l
				_   -> []

-- Generates a single noun with an associated determiner and possibly an adjective.  It
-- randomly decides whether the noun is plural and whether it has an adjective, and returns a
-- list of words based on those factors.  Also returns the plurality of the noun for use in
-- present tense verb forms (e.g. the cats bark vs. the cat barks)
genNP :: (RandomGen r) => r -> Lexicon -> ([String], Bool, r)
genNP r l = let	(isP, r') = randBool r
		(hasA, r'') = randBool r'
		(randNoun, r3) = (randWord r'' . nouns) l
	        (randDet, r4) = genT r3 isP l
		(randAdj, r5) = (randWord r4 . adjs) l
	    in case (hasA, isP) of
	    		(True, True)   -> (randDet:adj randAdj:plur randNoun:[], isP, r5)
			(True, False)  -> (randDet:adj randAdj:sing randNoun:[], isP, r5)
			(False, True)  -> (randDet:plur randNoun:[], isP, r5)
			(False, False) -> (randDet:sing randNoun:[], isP, r5)

-- Determines randomly whether the verb is in present or past tense and whether it has a modal
-- like "will".  If the modal is there, verb form can be ignored and the verb appears in base
-- form (the cat will bark).  If the verb is in past tense, only one past form applies (the cat
-- barked).  Otherwise, the verb is in sform or base form depending on the plurality of the
-- noun.  Along with this list of words (and the RandomGen), the function also returns the
-- argument type that the verb takes (see genTP comment for details).
genVP :: (RandomGen r) => r -> Bool -> Lexicon -> ([String], Char, r)
genVP r isP l = let (isPresent, r') = randBool r
		    (hasModal, r'') = randBool r'
		    (randMod, r3) = (randWord r'' . modals) l
		    (randVerb, r4) = (randWord r3 . verbs) l
		in case (hasModal, isPresent, isP) of
			(True, _, _)  -> (modal randMod:base randVerb:[], arg randVerb, r4)
			(False, True, True)  -> (base randVerb:[], arg randVerb, r4)
			(False, True, False) -> (sform randVerb:[], arg randVerb, r4)
			otherwise     -> (past randVerb:[], arg randVerb, r4)

-- Generates a random determiner taking either plural or nonplural nouns, based on passed-in
-- Bool.  This could be done with multiple randomization passes until the proper determiner type
-- is found, but "partition" handily breaks this determiner list into the two relevant
-- categories for random selection from each individually.
genT :: (RandomGen r) => r -> Bool -> Lexicon -> (String, r)
genT r isP l
	| isP == True  = (deter rpd, r')
	| otherwise    = (deter rsd, r'')
	where (plurs, sings) = partition (isPlur) (deters l)
	      (rpd, r')  = randWord r plurs
	      (rsd, r'') = randWord r sings

-- Takes an array of some word type and returns the word at a random index in the array
randWord :: (RandomGen r, LEntry a) => r -> [a] -> (a,r)
randWord rand ws = let (index, rand') = randomR (0, length ws - 1) rand
			in  ((ws !! index), rand')

-- Simply returns a random Bool value, True or False
randBool :: (RandomGen r) => r -> (Bool, r)
randBool rand = randomR (False, True) rand


-- Classes and data constructors

-- Class for all lexical entry types (LNoun, LVerb, etc.)  Contains the following:
-- 	setEntry: Turns unformatted string of words from file into an LEntry
-- 	insertEntry: Takes in a Lexicon and a LEntry and inserts the former into the
-- 		     latter, returning a Lexicon with the new word added.
class LEntry a where
	setEntry :: [String] -> a
	insertEntry :: Lexicon -> a -> Lexicon

-- Each of the following types hold some information about the words themselves, either
-- the 
data LNoun = LNoun { sing :: String
		    , plur :: String
		    }
instance LEntry LNoun where
	setEntry [w,w1] = LNoun w w1
	insertEntry l n = Lexicon (n:nouns l) (verbs l) (deters l) (modals l) (adjs l)

-- "arg" stores what second element the verb accepts -- an adjective, noun,
-- locative goal (to + NP), or even another clause.
-- Note that enform and ingform are not currently used in this program, just stored.
-- They would be used as the verb form coming after "have" and "be" respectively.
data LVerb = LVerb { arg :: Char
		    , base :: String
	    	    , sform :: String
		    , past :: String
		    , enform :: String
		    , ingform :: String
		    }
instance LEntry LVerb where
	setEntry [a,w,w1,w2,w3,w4] = LVerb (a !! 0) w w1 w2 w3 w4
	insertEntry l v = Lexicon (nouns l) (v:verbs l) (deters l) (modals l) (adjs l)

-- isPlur is used to distinguish between determiners that take plural nouns, like
-- "those" and "many", and those that take singular nouns, like "that" and "a".
--  Determiners like "the" which take both single and plural nouns have duplicate
--  entries in the lexicon.
data LDeter = LDeter { deter :: String
		     , isPlur :: Bool
		     }
instance LEntry LDeter where
	setEntry [b, w]
		| b == "sing" = LDeter w False
		| b == "plur" = LDeter w True
		| otherwise   = error "Determiner with unclear plurality"
	insertEntry l d = Lexicon (nouns l) (verbs l) (d:deters l) (modals l) (adjs l)

-- Modals act as qualifiers to verbs, for example "will", "should", and "can"
data LModal = LModal { modal :: String
		     }
instance LEntry LModal where
	setEntry [m] = LModal m
	insertEntry l m = Lexicon (nouns l) (verbs l) (deters l) (m:modals l) (adjs l)

-- Adjectives describe nouns
data LAdj = LAdj { adj :: String
		 }
instance LEntry LAdj where
	setEntry [a] = LAdj a
	insertEntry l a = Lexicon (nouns l) (verbs l) (deters l) (modals l) (a:adjs l)

-- This type is simply a convenient way to pass around a list of all the words in our
-- lexicon.  It contains within it lists of all nouns, verbs, etc. that have been read.
data Lexicon = Lexicon { nouns :: [LNoun]
		       , verbs :: [LVerb]
		       , deters :: [LDeter]
		       , modals :: [LModal]
		       , adjs :: [LAdj]
		       }
