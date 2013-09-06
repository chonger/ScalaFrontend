package multitool

import edu.berkeley.nlp.PCFGLA.{ParserData,CoarseToFineMaxRuleParser,TreeAnnotations,CoarseToFineNBestParser}
import edu.berkeley.nlp.io.PTBLineLexer
import edu.berkeley.nlp.util.Numberer

import java.io._

class BerkHack(val substates : Boolean) {

  def this() = {
    this(false)
  }

  val gramFile = "src/main/resources/eng_sm6.gr"

  val pData = ParserData.Load(gramFile)
  if (pData==null) 
    throw new Exception("Failed to load grammar from this file : "+ gramFile)
  
  val grammar = pData.getGrammar()
  val lexicon = pData.getLexicon()
  Numberer.setNumberers(pData.getNumbs())
  
  val parser = new CoarseToFineMaxRuleParser(grammar, lexicon,1.0,-1,substates,substates,false,false, false, true, true)
  //parser.binarization = pData.getBinarization()

  val tokenizer = new PTBLineLexer()

  def parse(line : String) : String = {
    //println("WTP : " + line)
    /**
    //berkeley parser doesnt deal well with (%d*)
    val rx = "\\((\\d*)\\)".r    
    val lineClean = rx.replaceAllIn(line,x => {
      "( " + x.group(1) + " )"
    })
*/
    val lineClean = line.replaceAll("\\("," ( ").replaceAll("\\)"," ) ")
    val sentence = tokenizer.tokenizeLine(lineClean)
    var parsedTree = parser.getBestConstrainedParse(sentence,null,null)
    //parsedTree = TreeAnnotations.unAnnotateTree(parsedTree)
    if(parsedTree.getChildren().isEmpty())
      null
    else
    "(ROOT "+parsedTree.getChildren().get(0)+" )"
  }

  

  def parseFile(infile : File, outfile : File) = {
    val lines = io.Source.fromFile(infile).getLines.toList.map(_.replaceAll("\\n",""))
    println("Parsing " + lines.length + " lines")
    val bw = new BufferedWriter(new FileWriter(outfile))
    var ind = 0
    lines.foreach(l => {
      if(ind % 100 == 0) {
        println()
        print(ind + "-")
      }
      ind += 1

      val p = parse(l)
      if(p != null) {
        print(".")
        bw.write(parse(l) + "\n")
      } else {
        print("!")
      }
      
    })
    println()
    bw.close()
  }
}


/**
 *  Returns K best parses using the berkeley parser
 * 
 */ 
class BerkHackK(val k : Int) {

  val gramFile = "src/main/resources/eng_sm6.gr"

  val pData = ParserData.Load(gramFile)
  if (pData==null) 
    throw new Exception("Failed to load grammar from this file : "+ gramFile)
  
  val grammar = pData.getGrammar()
  val lexicon = pData.getLexicon()
  Numberer.setNumberers(pData.getNumbs())

  val substates = false
  val parser = new CoarseToFineNBestParser(grammar, lexicon,k,1.0,-1,substates,substates,false,false, false, true, true)
  //parser.binarization = pData.getBinarization()

  val tokenizer = new PTBLineLexer()

  def parse(line : String) : List[String] = {
    //println("WTP : " + line)
    /**
    //berkeley parser doesnt deal well with (%d*)
    val rx = "\\((\\d*)\\)".r    
    val lineClean = rx.replaceAllIn(line,x => {
      "( " + x.group(1) + " )"
    })
*/
    val lineClean = line.replaceAll("\\("," ( ").replaceAll("\\)"," ) ")
    val sentence = tokenizer.tokenizeLine(lineClean)

    var parsedTrees = scala.collection.JavaConversions.asScalaBuffer(parser.getKBestConstrainedParses(sentence,null,k)).toList
    //parsedTree = TreeAnnotations.unAnnotateTree(parsedTree)
    parsedTrees.map(p => {
      if(p.getChildren().isEmpty())
        null
      else
        "(ROOT "+p.getChildren().get(0)+" )"
    })
  }

  def parseFile(infile : File, outfile : File) = {
    val lines = io.Source.fromFile(infile).getLines.toList.map(_.replaceAll("\\n",""))
    println("Parsing " + lines.length + " lines")
    val bw = new BufferedWriter(new FileWriter(outfile))
    var ind = 0
    lines.foreach(l => {
//      if(ind % 100 == 0) {
        println()
        print(ind + "\t")
  //    }
      ind += 1

      val p = parse(l)
      if(p != null) {
        print(".")
        bw.write(parse(l).mkString("\n") + "\n\n")
      } else {
        print("!")
      }
      
    })
    println()
    bw.close()
  }
}

