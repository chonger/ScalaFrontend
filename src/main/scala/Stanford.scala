package multitool

import edu.stanford.nlp.parser.lexparser.LexicalizedParser
import edu.stanford.nlp.ling.Sentence
import edu.stanford.nlp.trees.{PennTreebankLanguagePack,TreePrint,Tree}
import edu.stanford.nlp.parser.lexparser.{TreebankAnnotator,Options}
import scala.collection.JavaConverters._

import java.io._

object Stanford {

  def main(args : Array[String]) = {
    annotate("/home/chonger/data/demo.txt","/home/chonger/data/demoout.txt")
    //unannotate("/home/chonger/data/PTB/stanout.txt","/home/chonger/data/PTB/stanout2.txt")
  }

  def unannotate(path : String, out : String) {
    val op = new Options()
    val trees = TreebankAnnotator.getTrees(path,200,219,0,1000)
    val strees = trees.asScala.toList
    println(strees.length + " trees")
    val deannotatedTrees : List[Tree] = TreebankAnnotator.removeDependencyRoots(new TreebankAnnotator(op, path).deannotateTrees(trees)).asScala.toList

    val bw = new BufferedWriter(new FileWriter(out))

    deannotatedTrees.foreach(t => {
      bw.write(t.pennString().trim.replaceAll("\\s+"," ") + "\n")
    })
    
    bw.close()
  }

/**

  def unannotate(path : String, out : String) {
    val st = new CFGSymbolTable() 

    def expandU(n : NonTerminalNode) : NonTerminalNode = {
      n match {
        case n : ProtoNode => {
          val sym = st.syms(n.symbol)
          if(sym.indexOf("&") == 0) {
            val parts = sym.split("&").drop(1).map(x => {
              val iii = x.indexOf("-^") 
              if(iii >= 0) {
                x.slice(0,iii)
              }
              else x
            }).reverse
            val start = new ProtoNode(st.syms.add(parts(0)),n.children.map(nn => expandU(nn)))
            (start /: parts.drop(1))((a,b) => {
              new ProtoNode(st.syms.add(b),List(a))
            })
          } else 
            new ProtoNode(n.symbol,n.children.map(nn => expandU(nn)))
        }
        case n : PreTerminalNode => {
          n
        }
      }
    }
    
    val trees = st.read(path).map(x => new ParseTree(expandU(x.root)))

    0.until(st.syms.size).foreach(i => {
      var s = st.syms(i)
      if(s.indexOf("&") != 0) { // not a unary chain
        val index = s.indexOf('^')
        if(index >= 0)
          s = s.slice(0,index)
        st.syms.strings(i) = s
      }
    })

    import scala.collection.mutable.HashSet

    val hs = new HashSet[String]()
    hs ++= st.syms.strings

    println(hs.size)
    hs.iterator.foreach(s => { println(s)})

    st.write(out,trees)

  }
  *
  */

  def annotate(path : String, out : String) {
    val op = new Options()
    val trees = TreebankAnnotator.getTrees(path,200,219,0,1000)
    val strees = trees.asScala.toList
    println(strees.length + " trees")
    val annotatedTrees : List[Tree] = TreebankAnnotator.removeDependencyRoots(new TreebankAnnotator(op, path).annotateTrees(trees)).asScala.toList

    val bw = new BufferedWriter(new FileWriter(out))

    (strees zip annotatedTrees).foreach({
      case (t1,t2) => {
        println(t1.pennString().trim.replaceAll("\\s+"," "))
        var s = t2.pennString().trim.replaceAll("\\s+"," ").toCharArray()
        println(t2.pennString().trim.replaceAll("\\s+"," "))
/**
        var os = s
        s = ""
        while(os != s) {
          val tmp = s
          s = os.replaceAll("(\\|[^\\s]*)\\s[^\\(]","$1@")
          println(s)
          os = tmp
        }
        * */
        var rep = false
        0.until(s.length-1).foreach(ind => {
          if(rep) {
            if(s(ind) == ' ' && s(ind+1) != '(')
              s(ind) = '@'
            if(s(ind+1) == '(')
              rep = false
          } else {
            if(s(ind) == '|')
              rep = true
          }
        })
        val ss = s.mkString("")
        //println(ss)
        //readLine()
        bw.write(ss + "\n")
      }
    })

    bw.close()
  }


  lazy val tlp = new PennTreebankLanguagePack()
  lazy val lp = LexicalizedParser.loadModel("edu/stanford/nlp/models/lexparser/englishPCFG.ser.gz")

  lazy val st = new CFGSymbolTable()

  def conv[A](x: Array[A]) : java.util.List[A] = java.util.Arrays.asList(x : _*)

/**
  def main(args : Array[String]) : Unit = {

    val tags = Array(null,"IN",null,null,null,null)
    val toks = Array("I","like","big","butts",".")

    val tree = parse(toks)

    println(tree.fString(st))
  }
*/
  def parse(toks : Array[String]) : ParseTree = {
    parse(toks,toks.map(x => {null}))
  }

  def parse(toks : Array[String], tags : Array[String]) : ParseTree = {
    val parse = lp.apply(Sentence.toTaggedList(conv(toks),conv(tags)))
    val printer = new TreePrint("typedDependencies,wordsAndTags","includePunctuationDependencies,basicDependencies",new PennTreebankLanguagePack())
    val sw = new StringWriter()
    val pw = new PrintWriter(sw)
    printer.printTree(parse,pw)
    
    //println(sw.toString)

    val lines = sw.toString.split("\n")

    val wtags = lines(0)

    val otags = wtags.split("\\s").map(x => {
      val WT = "^.*/([^/]+)$".r
      val WT(t) = x
      t
    })

    //println(otags.mkString(" "))

    val rels = lines.drop(2).map(x => {

      val Rel = "^([^\\(]+)\\([^\\s]+-([\\d']*), [^\\s]+-([\\d']*)\\)".r
      val Rel(relType,from,to) = x

      (relType,from,to)

    })

    //println(rels.mkString("\n"))

    val deps = rels.groupBy(_._2)

//    new ParseTree(deps)

    val GetS = "^(\\d+)'*$".r

    def toN(s : String) = {
      val GetS(r) = s
      r.toInt
    }

    def makeNode(rT : String, f : String, t : String) : NonTerminalNode = {
      
      //println(rT + " " + f + " " + t)

      val tInd = toN(t)

      val mySym = otags(tInd-1)
      val myW = toks(tInd-1)

      def makeR(k : List[NonTerminalNode]) : ProtoNode = {
        if(k.length == 1) 
          new ProtoNode(st.syms.add(mySym + "-R"),k)
        else
          new ProtoNode(st.syms.add(mySym + "-R"),List(makeR(k.slice(0,k.length-1)),k(k.length-1)))
      }

      def makeL(k : List[NonTerminalNode]) : ProtoNode = {
        if(k.length == 1) 
          new ProtoNode(st.syms.add(mySym + "-L"),k)
        else
          new ProtoNode(st.syms.add(mySym + "-L"),List(k(0),makeR(k.drop(1))))
      }

      val outD : List[(String,String,String)] = deps.getOrElse(t,Array()).toList

      val lefts = outD.filter(x => toN(x._3) < tInd).map(x => makeNode(x._1,x._2,x._3)).toList
      val rights = outD.filter(x => toN(x._3) >= tInd).map(x => makeNode(x._1,x._2,x._3)).toList

      var kids = List[NonTerminalNode]()

      if(rights.length > 0) {
        //kids ::= new ProtoNode(st.syms.add(mySym + "-R"),rights)
        kids ::= makeR(rights)
      }

      kids ::= new PreTerminalNode(st.syms.add(mySym),new TerminalNode(st.terms.add(myW)))

      if(lefts.length > 0) {
        //kids ::= new ProtoNode(st.syms.add(mySym + "-L"),lefts)
        kids ::= makeL(lefts)
      }
      val rStr = if(t.indexOf("'") > 0) {rT + "_prime"} else rT

      new ProtoNode(st.syms.add(rStr),kids)

    }

    val rdep = deps("0")(0)

    new ParseTree(new ProtoNode(st.syms.add("R"),List((makeNode _).tupled(rdep))))
  }

}
