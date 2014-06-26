//Nikolai Kallhovde		nkallhov@ucsc.edu		03/21/14

import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;
import java.util.Random;
import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;


public class BokmaalGen {
	// Used for grammatical gender
	public static Boolean MASC = true;
	public static Boolean NEUT = false;

	public static int MAXWORDS = 10000;

	public static void main(String[] args) {
		List<String> sentence = new ArrayList<String>();

		Lexicon lex = buildLexicon("no.in");
		Random rand = new Random();

		sentence = genNP(sentence, lex, rand);
		sentence = genVP(sentence, lex, rand);
		System.out.println(formatSentence(sentence));
	}
	
	//Turns list into string capitalized, punctuated, and separated by spaces
	public static String formatSentence(List<String> sentence){
		String finalSent = new String();
		ListIterator<String> si = sentence.listIterator();
		while (si.hasNext()){
			String word = si.next();
			finalSent = finalSent + word + " ";
		}
		finalSent = finalSent.substring(0,1).toUpperCase() + finalSent.substring(1);
		return (finalSent.substring(0, finalSent.length()-1) + ".");
	}
	
	
	//Read from file and add entries to the lexicon
	public static Lexicon buildLexicon(String fn){
		Lexicon lex = new Lexicon();
		try {
			BufferedReader reader = new BufferedReader(new FileReader(fn));
			String lexEntry = reader.readLine();
			while (lexEntry != null){
				lex = addLexEntry(lexEntry, lex);
				lexEntry = reader.readLine();
			}
			reader.close();
		} catch (FileNotFoundException e) {
			System.out.println("ERROR: File not found.");
			e.printStackTrace();
		} catch (IOException e) {
			System.out.println("ERROR: IO Error.");
			e.printStackTrace();
		}
		return lex;
	}

	//Add an individual entry to the lexicon
	public static Lexicon addLexEntry (String lexEntry, Lexicon lex){
		String[] words = lexEntry.split("\\s+");
		if (words[0].equals("N")){ //Add a noun to lexicon
			Boolean gender = true;
			if (words[1].equals("neut")){
				gender = NEUT;
			}
			Noun newWord = new Noun(gender, words[2], words[3], words[4], words[5]);
			lex.insertWord('N', newWord);
		} else if (words[0].equals("D")){ //Add a determiner to lexicon
			Determiner newWord;	
			if (words[1].equals("neut")){ //Determiners for neuter singular nouns
				newWord = new Determiner(words[2], false, true, false);
			} else if (words[1].equals("masc")){ //For masculine singular nouns
				newWord = new Determiner(words[2], true, false, false);
			} else if (words[1].equals("plur")){ //For plural nouns of either gender
				newWord = new Determiner(words[2], false, false, true);
			} else {
				System.out.println("ERROR: Improper determiner form" + words[1]);
				newWord = new Determiner("dummy", false, false, false);
			}
			lex.insertWord('D', newWord);
		} else if (words[0].equals("V")) { //Add a verb to lexicon
			char takes = words[1].charAt(0);
			Verb newWord = new Verb (words[2], words[3], words[4], words[5], takes);
			lex.insertWord('V', newWord);
		} else if (words[0].equals("M")) {  //Add a modal to lexicon
			lex.insertModal(words[1]);
		} else {
			System.out.println("ERROR: Incorrect word type " + words[0] + " in lexicon file.");
		}
		return lex;
	}
	
	//Generate a noun and a determiner that matches it
	public static List<String> genNP (List<String> sentence, Lexicon lex, Random rand){
		Noun noun = (Noun) lex.getRandomWord('N', rand);
		//Chooses noun form at random: base, definite, plural, or plural definite
		int nounForm = rand.nextInt(4);
		String nounInForm;
		
		switch (nounForm){
			case 0: //If noun is in base, find a determiner that matches gender
				    nounInForm = noun.base;
					sentence.add(genD(lex, rand, noun.gender, false));
					sentence.add(nounInForm);
				    break;
			case 1: //If noun is in definite, no determiner needed
				    sentence.add(noun.definite);
				    break;
			case 2: //Noun is plural, find a matching determiner
				    nounInForm = noun.plural;
					sentence.add(genD(lex, rand, noun.gender, true));
					sentence.add(nounInForm);
				    break;
			case 3: //Plural definite, also no determiner needed
					sentence.add(noun.pluralDef);
				    break;
			default: System.out.println("Randomization Error in noun forms");
					 break;
		}
		return sentence;
	}
	
	//Returns a determiner that is of the right form for the noun that will succeed it.
	public static String genD (Lexicon lex, Random rand, Boolean gend, Boolean plur){
		Determiner testDet = (Determiner) lex.getRandomWord('D', rand);
		if (plur == true){
			//Gender doesn't matter for plurals
			if (testDet.takesPlural == true){
				return testDet.base;
			}
		} else {
			//Male singular match male determiner
			if (testDet.takesMascBase == true && gend == MASC){
				return testDet.base;
			}
			//Neuter singular match neuter determiner
			if (testDet.takesNeutBase == true && gend == NEUT){
				return testDet.base;
			}
		}
		//Fail case, recursively try again with new random word
		return (genD(lex, rand, gend, plur));
	}
	
	//Adds to the sentence a verb along with any complements it may take
	public static List<String> genVP(List<String> sentence, Lexicon lex, Random rand){
		Verb verb = (Verb) lex.getRandomWord('V', rand);
		int verbForm = rand.nextInt(4);
		String verbInForm;
		
		//Insert the verb or series of verbs
		switch (verbForm){
			case 0: //If verb is in base, precede it w/ a modal like "will"
				    verbInForm = verb.base;
				    sentence.add(lex.getRandomModal(rand));
				    sentence.add(verbInForm);
				    break;
			case 1: //Present tense, fine as is.  Could be negated.
				    verbInForm = verb.present;
				    sentence.add(verbInForm);
				    //Chance to be negated
				    if (rand.nextBoolean()){
				    	sentence.add("ikke");
				    }
				    break;
			case 2: //Past tense, also works
					verbInForm = verb.past;
					sentence.add(verbInForm);
					//Chance to be negated
				    if (rand.nextBoolean()){
				    	sentence.add("ikke");
				    }
				    break;
			case 3: //Past perfect verb, precede with "have" or "had"
					verbInForm = verb.pastPerfect;
					int modalStatus = rand.nextInt();
					if (modalStatus == 0){
						sentence.add("har");
					} else if (modalStatus == 1) {
						sentence.add("hadde");
					} else {
						sentence.add(lex.getRandomModal(rand));
						sentence.add("ha");
					}
					sentence.add(verbInForm);
				    break;
			default: System.out.println("Randomization Error in verb forms");
					 break;
		}
		
		//Insert the verb's complement locative goal, noun, or embedded clause.
		switch (verb.takes){
			case 'L': //Locative goal, e.g. "Joe went to the park". Falls through
				sentence.add("til");
			case 'N': //Noun, e.g. "He shot Joe"
				sentence = genNP(sentence, lex, rand);
				break;
			case 'C': //Embedded clause, e.g. "I know that Joe died"
				sentence.add("at");
				sentence = genNP(sentence, lex, rand);
				sentence = genVP(sentence, lex, rand);
				break;
			default:
				break;
		}
		return sentence;
	}
	
	
	
	public static class Word{
		String base;
	}
	
	public static class Lexicon {
		List<Word> determiners;
		List<Word> nouns;
		List<Word> verbs;
		
		List<String> modals;
		
		public Lexicon (){
			determiners = new ArrayList<Word>();
			nouns = new ArrayList<Word>();
			verbs = new ArrayList<Word>();
			modals = new ArrayList<String>();
		}
		
		public void insertWord (char wordType, Word newWord){
			switch (wordType){
				case 'D': determiners.add(newWord);
						  break;
				case 'N': nouns.add(newWord);
						  break;
				case 'V': verbs.add(newWord);
						  break;
				default : System.out.println("Invalid word type.\n");
						  break;
			}
		}
		
		public void insertModal (String modal){
			modals.add(modal);
		}
		
		public Word getRandomWord (char wordType, Random rand){
			switch (wordType){
				case 'D': return (determiners.get(rand.nextInt(determiners.size())));
				case 'N': return (nouns.get(rand.nextInt(nouns.size())));
				case 'V': return (verbs.get(rand.nextInt(verbs.size())));
				default : System.out.println("Invalid word type.\n");
						  return null;
			}
		}
		
		public String getRandomModal (Random rand){
			return (modals.get(rand.nextInt(modals.size())));
		}
	}
	
	public static class Determiner extends Word {
		Boolean takesMascBase;
		Boolean takesNeutBase;
		Boolean takesPlural;
		
		// Constructor
		public Determiner (String word, Boolean tmb, Boolean tnb,
				           Boolean tp){
			base = word;
			takesMascBase = tmb;
			takesNeutBase = tnb;
			takesPlural = tp;
		}
	}

	public static class Noun extends Word {
		// Gender determines whether the article is "et" or "en" as well as
		// changing the form of an adjective that reflects it.  Certain dialects
		// have a feminine gender "ei" -- this is not represented here.
		Boolean gender;
		
		// The following strings are usually the base word with a special ending.
		// However, this is not true in all cases so separate strings are needed.
		String definite;  // Base word  with -et or -en depending on gender
		String plural;    // Base word with -er
		String pluralDef; // Base word with -ene
		
		//Constructor
		public Noun (Boolean g, String b, String def, String pl, String pldef){
			gender = g;
			base = b;
			definite = def;
			plural = pl;
			pluralDef = pldef;
		}
	}
	
	public static class Verb extends Word {
		String present;     // Verb with -r or -er added
		String past;        // Sometimes -t added vowel changed, or other
		String pastPerfect; // Sometimes same form as past. (Eng. eaten vs. ate)
		
		//This shows what argument a given verb can take.  0 is nothing, N is NP,
		//L is a locative goal (til + NP), and C is an embedded clause (at + TP).
		char takes;
		
		//Constructor
		public Verb (String bs, String pres, String pas, String pp, char t){
			base = bs;
			present = pres;
			past = pas;
			pastPerfect = pp;
			takes = t;
		}
	}
}